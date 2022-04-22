pub mod lex;
pub mod parser;
pub mod util;

use std::fmt;
use std::collections::HashMap;
use std::error::Error;

use util::PrintTable;
use parser::{Ast, Parser, Expression, ParseError};

use crate::parser::{
    Statement,
    Statement::{CreateStatement, InsertStatement, SelectStatement},
    ColumnDefinition,
    Expression::{Literal, Binary},
};

use crate::lex::{
    KeywordType::{Int, Text, True, False, And, Or},
    SymbolType::{Equals, NotEquals, Concatenate, Plus, Asterisk},
    Token,
    Token::{Integer, PGString, Identifier, Keyword, Symbol},
    IdentifierType::Symbol as SymbolIdentifier,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ColumnType {
    TextType,
    IntType,
    BoolType,
}

impl Into<ColumnType> for Token {
    fn into(self) -> ColumnType {
        match self {
            PGString(_) => ColumnType::TextType,
            Integer(_) => ColumnType::IntType,
            Keyword(True) | Keyword(False) => ColumnType::BoolType,
            _ => unimplemented!("Not a column type")
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ColumnName {
    DefaultName,
    Name(String),
}

impl Into<String> for ColumnName {
    fn into(self) -> String {
        match self {
            ColumnName::DefaultName => "?column?".to_string(),
            ColumnName::Name(s) => s,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Column {
    pub column_type: ColumnType,
    pub column_name: ColumnName,
}

#[derive(Debug)]
pub enum BackendError {
    ErrTableDoesNotExist,
    ErrColumnDoesNotExist,
    ErrInvalidSelectItem,
    ErrInvalidDatatype,
    ErrMissingValues,
    ErrInvalidOperands,
    ErrParsingError(ParseError),
}

impl fmt::Display for BackendError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let err_msg = match self {
            BackendError::ErrTableDoesNotExist  => "Table does not exist".to_string(),
            BackendError::ErrColumnDoesNotExist => "Column does not exist".to_string(),
            BackendError::ErrInvalidSelectItem => "Select item is not valid".to_string(),
            BackendError::ErrInvalidDatatype => "Invalid datatype".to_string(),
            BackendError::ErrMissingValues => "Missing values".to_string(),
            BackendError::ErrInvalidOperands => "Invalid operands".to_string(),
            BackendError::ErrParsingError(e) => format!("Parse Error: {}", e),
        };
        write!(f, "{}", err_msg)
    }
}

impl Error for BackendError {}

impl Into<BackendError> for ParseError {
    fn into(self) -> BackendError {
        BackendError::ErrParsingError(self)
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct MemoryCell(Vec<u8>);

impl From<MemoryCell> for String {
    fn from(MemoryCell(mc): MemoryCell) -> String {
        String::from_utf8_lossy(&mc).to_string()
    }
}

impl Into<MemoryCell> for String {
    fn into(self) -> MemoryCell {
        MemoryCell(self.as_bytes().to_vec())
    }
}

impl From<MemoryCell> for i32 {
    fn from(MemoryCell(mc): MemoryCell) -> Self {
        i32::from_le_bytes(
            mc.try_into().unwrap_or_else(|v: Vec<u8>| {
                panic!("Expected Vec of length 4 but found length {}", v.len())
            })
        )
    }
}

impl Into<MemoryCell> for i32 {
    fn into(self) -> MemoryCell {
        MemoryCell(self.to_le_bytes().to_vec())
    }
}

impl From<MemoryCell> for bool {
    fn from(MemoryCell(mc): MemoryCell) -> Self {
        mc.len() == 1
    }
}

impl Into<MemoryCell> for bool {
    fn into(self) -> MemoryCell {
        MemoryCell(if self { vec![1] } else { vec![] })
    }
}

impl From<Token> for MemoryCell {
    fn from(token: Token) -> MemoryCell {
        match token {
            Integer(i) => i.clone().into(),
            PGString(s) => s.clone().into(),
            Keyword(k) => match k {
                True => MemoryCell(vec![1]),
                False => MemoryCell(vec![]),
                _ => unimplemented!("Not a memory cell type"),
            }
            _ => unimplemented!("Not a memory cell type"),
        }
    }
}

#[derive(Debug, Default)]
pub struct Table {
    columns: Vec<String>,
    column_types: Vec<ColumnType>,
    rows: Vec<Vec<MemoryCell>>,
}

impl Table {
    pub fn evaluate(
        &self,
        row_index: usize,
        exp: &Expression
    ) -> Result<(MemoryCell, ColumnName, ColumnType), BackendError> {
        match exp {
            Literal(e) => {
                if let Identifier(SymbolIdentifier, id) = e.clone() {
                    let (c, _) = self.columns.iter()
                                             .enumerate()
                                             .find(|(_, col)| **col == id)
                                             .ok_or(BackendError::ErrColumnDoesNotExist)?;
                    Ok((self.rows[row_index][c].clone(), ColumnName::Name(id), self.column_types[c].clone()))
                } else {
                    Ok((e.clone().into(), ColumnName::DefaultName, e.clone().into()))
                }
            },
            Binary { l, r, op } => {
                let (l_cell, _, l_type) = self.evaluate(row_index, &l.clone())?;
                let (r_cell, _, r_type) = self.evaluate(row_index, &r.clone())?;
                let true_cell: MemoryCell = Keyword(True).into();
                let false_cell: MemoryCell = Keyword(False).into();

                match op {
                    Symbol(s) => match s {
                        Equals => {
                            let eq = l_cell == r_cell;
                            Ok((
                                match (l_type, r_type) {
                                    (ColumnType::TextType, ColumnType::TextType)
                                        | (ColumnType::IntType, ColumnType::IntType)
                                        | (ColumnType::BoolType, ColumnType::BoolType)
                                        if eq => true_cell,
                                    _ => false_cell
                                },
                                ColumnName::DefaultName,
                                ColumnType::BoolType
                            ))
                        },
                        NotEquals => {
                            Ok((
                                if l_type != r_type || l != r {
                                    true_cell
                                } else {
                                    false_cell
                                },
                                ColumnName::DefaultName,
                                ColumnType::BoolType
                            ))
                        },
                        Concatenate => {
                            if l_type != ColumnType::TextType || r_type != ColumnType::TextType {
                                return Err(BackendError::ErrInvalidOperands);
                            }

                            let mut l_string: String = l_cell.into();
                            let r_string: String = r_cell.into();
                            l_string.push_str(r_string.as_str());

                            Ok((l_string.into(), ColumnName::DefaultName, ColumnType::TextType))
                        },
                        Plus => {
                            if l_type != ColumnType::IntType || r_type != ColumnType::IntType {
                                return Err(BackendError::ErrInvalidOperands);
                            }

                            let l_int: i32 = l_cell.into();
                            let r_int: i32 = r_cell.into();

                            Ok(((l_int + r_int).into(), ColumnName::DefaultName, ColumnType::IntType))
                        },
                        _ => todo!(),
                    },
                    Keyword(k) => {
                        match k {
                            And => {
                                if l_type != ColumnType::BoolType || r_type != ColumnType::BoolType {
                                    return Err(BackendError::ErrInvalidOperands);
                                }

                                let l_b: bool = l_cell.into();
                                let r_b: bool = r_cell.into();

                                Ok((
                                    if l_b && r_b { true_cell } else { false_cell },
                                    ColumnName::DefaultName,
                                    ColumnType::BoolType
                                ))
                            },
                            Or => {
                                if l_type != ColumnType::BoolType || r_type != ColumnType::BoolType {
                                    return Err(BackendError::ErrInvalidOperands);
                                }

                                let l_b: bool = l_cell.into();
                                let r_b: bool = r_cell.into();

                                Ok((
                                    if l_b || r_b { true_cell } else { false_cell },
                                    ColumnName::DefaultName,
                                    ColumnType::BoolType
                                ))
                            },
                            _ => todo!()
                        }
                    },
                    _ => todo!()
                }
            }
        }
    }
}

trait Backend {
    fn execute(&mut self, stmt: Statement) -> Result<(), BackendError>;
}

#[derive(Debug, Default)]
pub struct InMemoryBackend {
    tables: HashMap<String, Table>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Execute {
    Empty,
    Results {
        columns: Vec<Column>,
        rows: Vec<Vec<MemoryCell>>
    }
}

impl InMemoryBackend {
    pub fn new() -> Self {
        InMemoryBackend { tables: HashMap::new() }
    }

    pub fn run(&mut self, line: &String) -> Result<(), BackendError> {
        let Ast { statements } = Parser::new(&line).parse().or_else(|e| Err(e.into()))?;
        for stmt in statements {
            match stmt {
                CreateStatement { .. } => {
                    self.execute(&stmt)?;
                    println!("CREATE TABLE");
                },
                InsertStatement { .. } => {
                    self.execute(&stmt)?;
                    println!("INSERT");
                },
                SelectStatement { .. } => {
                    if let Execute::Results {rows, columns} = self.execute(&stmt)? {
                        let mut tbl = PrintTable::new();

                        tbl.header(
                            &columns.iter()
                                    .map(|c| c.column_name.clone().into())
                                    .collect()
                        );

                        let results = rows.len();
                        for row in rows {
                            tbl.add(
                                &row.iter().enumerate().map(|(i, cell)| {
                                    match &columns[i].column_type {
                                        ColumnType::IntType => {
                                            let r: i32 = cell.clone().into();
                                            r.to_string()
                                        },
                                        ColumnType::TextType => cell.clone().into(),
                                        ColumnType::BoolType => {
                                            let r: bool = cell.clone().into();
                                            if r { "t" } else { "f" }.to_string()
                                        }
                                    }
                                }).collect()
                            );
                        }

                        println!("");
                        tbl.print();
                        println!("({} result{})", results, if results > 1 { "s" } else { "" });
                    }
                },
            }
        }

        Ok(())
    }

    fn execute(&mut self, stmt: &Statement) -> Result<Execute, BackendError> {
        match stmt {
            CreateStatement { name, cols } => self.create_table(name.to_string(), cols.to_vec()),
            InsertStatement { table, values } => self.insert(table.to_string(), values.to_vec()),
            SelectStatement { item, from, where_cond } =>
                self.select(from.to_owned(), item.to_vec(), where_cond.to_owned()),
        }
    }

    fn create_table(
        &mut self,
        table_name: String,
        cols: Vec<ColumnDefinition>,
    ) -> Result<Execute, BackendError> {
        let mut table = Table::default();

        for ColumnDefinition {name, data_type} in cols {
            table.columns.push(name);
            table.column_types.push(
                match data_type {
                    Int => ColumnType::IntType,
                    Text => ColumnType::TextType,
                    _ => return Err(BackendError::ErrInvalidDatatype)
                }
            )
        }

        self.tables.insert(table_name, table);
        Ok(Execute::Empty)
    }

    fn insert(
        &mut self,
        table_name: String,
        values: Vec<Vec<Expression>>,
    ) -> Result<Execute, BackendError> {
        let table = match self.tables.get_mut(&table_name) {
            Some(t) => t,
            None => return Err(BackendError::ErrTableDoesNotExist),
        };

        if values.is_empty() { return Ok(Execute::Empty); }

        for value in values {
            let mut row = Vec::new();

            if value.len() != table.columns.len() {
                return Err(BackendError::ErrMissingValues);
            }

            for val in value {
                match val {
                    Literal(_) => {
                        let (v, _, _) = Table::default().evaluate(0, &val)?;
                        row.push(v);
                    },
                    _ => {
                        eprintln!("Skipping non-literal");
                        continue;
                    }
                }
            }

            table.rows.push(row);
        }

        Ok(Execute::Empty)
    }

    fn select(
        &mut self,
        table_name: Option<String>,
        item: Vec<Expression>,
        where_cond: Option<Expression>,
    ) -> Result<Execute, BackendError> {
        if item.is_empty() {
            return Ok(Execute::Results {
                columns: Vec::new(),
                rows: Vec::new()
            });
        }

        let d = Table::default();
        let table = match table_name {
            Some(n) => self.tables.get(&n).ok_or(BackendError::ErrTableDoesNotExist)?,
            None => &d
        };

        let mut results = Vec::new();
        let mut cols = Vec::new();

        for i in 0..table.rows.len() {
            if let Some(ref expr) = where_cond {
                let (val, _, _) = table.evaluate(i, &expr)?;
                let b: bool = val.into();
                if !b { continue }
            }

            let mut result = Vec::new();
            for itm in &item {
                if let Literal(Symbol(Asterisk)) = itm {
                    todo!();
                }

                let (value, column_name, column_type) = table.evaluate(i, &itm)?;

                if results.is_empty() { cols.push(Column { column_name, column_type }); }
                result.push(value);
            }
            results.push(result);
        }

        Ok(Execute::Results{rows: results, columns: cols})
    }

}

#[cfg(test)]
mod test {
    use super::*;
    use super::ColumnType::{IntType, TextType, BoolType};
    use super::ColumnName::{Name, DefaultName};
    use crate::parser::Expression::{Binary, Literal};
    use crate::lex::{
        Token::{Integer, Symbol},
        SymbolType::Plus
    };

    #[test]
    fn test_backend() {
        let mut b = InMemoryBackend::new();

        let r = run_stmt(&mut b, "CREATE TABLE users (name TEXT, age INT);");
        assert_eq!(r, Execute::Empty);

        let r = run_stmt(&mut b, "INSERT INTO users VALUES ('Stephen', 16);");
        assert_eq!(r, Execute::Empty);

        let r = run_stmt(&mut b, "SELECT name, age FROM users;");
        assert_eq!(
            r,
            Execute::Results {
                columns: vec![
                    Column {
                        column_type: TextType,
                        column_name: Name("name".to_string())
                    },
                    Column {
                        column_type: IntType,
                        column_name: Name("age".to_string())
                    }
                ],
                rows: vec![
                    vec![
                        "Stephen".to_string().into(),
                        16i32.into()
                    ]
                ]
            }
        );

        let r = run_stmt(&mut b, "INSERT INTO users VALUES ('Adrienne', 23);");
        assert_eq!(r, Execute::Empty);

        let r = run_stmt(&mut b, "SELECT age + 2, name FROM users WHERE age = 23;");
        assert_eq!(
            r,
            Execute::Results {
                columns: vec![
                    Column {
                        column_type: IntType,
                        column_name: DefaultName
                    },
                    Column {
                        column_type: TextType,
                        column_name: Name("name".to_string())
                    },
                ],
                rows: vec![
                    vec![
                        25i32.into(),
                        "Adrienne".to_string().into(),
                    ]
                ]
            }
        );

        let r = run_stmt(&mut b, "SELECT name FROM users;");
        assert_eq!(
            r,
            Execute::Results {
                columns: vec![
                    Column {
                        column_type: TextType,
                        column_name: Name("name".to_string())
                    }
                ],
                rows: vec![
                    vec!["Stephen".to_string().into()],
                    vec!["Adrienne".to_string().into()]
                ]
            }
        );

        fn run_stmt(b: &mut InMemoryBackend, s: &str) -> Execute {
            b.execute(
                &Parser::new(&s.to_string()).parse().unwrap().statements[0]
            ).unwrap()
        }
    }

    #[test]
    fn test_table_evaluation() {
        let r = eval_table_expr(Binary {
            l: Box::new(Literal(Integer(1))),
            r: Box::new(Literal(Integer(1))),
            op: Symbol(Plus),
        });

        assert_eq!(r, (2i32.into(), ColumnName::DefaultName, IntType));

        let r = eval_table_expr(Binary {
            l: Box::new(Literal(PGString("one".to_string()))),
            r: Box::new(Literal(PGString("two".to_string()))),
            op: Symbol(Concatenate),
        });

        assert_eq!(r, ("onetwo".to_string().into(), ColumnName::DefaultName, TextType));

        let r = eval_table_expr(Binary {
            l: Box::new(Literal(Keyword(True))),
            r: Box::new(Literal(Keyword(False))),
            op: Keyword(Or),
        });

        assert_eq!(r, (true.into(), ColumnName::DefaultName, BoolType));

        let r = eval_table_expr(Binary {
            l: Box::new(Literal(Keyword(False))),
            r: Box::new(Literal(Keyword(True))),
            op: Keyword(And),
        });

        assert_eq!(r, (false.into(), ColumnName::DefaultName, BoolType));
    }

    #[test]
    fn test_row_evaluation() {
        let e = Binary {
            l: Box::new(Literal(Identifier(SymbolIdentifier, "column1".to_string()))),
            r: Box::new(Literal(Keyword(True))),
            op: Symbol(Equals),
        };

        let r = Table {
            columns: vec!["column1".to_string()],
            column_types: vec![BoolType],
            rows: vec![
                vec![true.into()]
            ],
        }.evaluate(0, &e).unwrap();

        assert_eq!(r, (true.into(), ColumnName::DefaultName, BoolType));

        let e = Binary {
            l: Box::new(Literal(Identifier(SymbolIdentifier, "column1".to_string()))),
            r: Box::new(
                Binary {
                    l: Box::new(Literal(Identifier(SymbolIdentifier, "column2".to_string()))),
                    r: Box::new(Literal(Integer(3))),
                    op: Symbol(Plus),
                }
            ),
            op: Symbol(Equals),
        };

        let r = Table {
            columns: vec!["column1".to_string(), "column2".to_string()],
            column_types: vec![IntType, IntType],
            rows: vec![
                vec![5.into(), 2.into()]
            ],
        }.evaluate(0, &e).unwrap();

        assert_eq!(r, (true.into(), ColumnName::DefaultName, BoolType));
    }

    fn eval_table_expr(e: Expression) -> (MemoryCell, ColumnName, ColumnType) {
        Table::default().evaluate(0, &e).unwrap()
    }
}
