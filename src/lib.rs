pub mod lex;
pub mod parser;
pub mod util;
pub mod table;
pub mod error;

use std::collections::HashMap;

use util::PrintTable;
use parser::{Ast, Parser, Expression};
use table::{Table, Column, ColumnType, MemoryCell};
use error::BackendError;

use crate::parser::{
    Statement,
    Statement::{CreateStatement, InsertStatement, SelectStatement},
    ColumnDefinition,
    Expression::Literal,
};

use crate::lex::{
    KeywordType::{Int, Text},
    SymbolType::Asterisk,
    Token::{Identifier, Symbol},
    IdentifierType::Symbol as SymbolIdentifier,
};

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

            let expanded_items: Vec<Expression> = item.iter().flat_map(|i| {
                if let Literal(Symbol(Asterisk)) = i {
                    table.columns.iter().map(|c| {
                        Literal(Identifier(SymbolIdentifier, c.to_string()))
                    }).collect()
                } else {
                    vec![i.clone()]
                }
            }).collect();

            let mut result = Vec::new();
            for itm in expanded_items {
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

    use crate::table::{
        ColumnType::{TextType, IntType},
        ColumnName::{DefaultName, Name},
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

        let r = run_stmt(&mut b, "SELECT *, name FROM users;");
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
                    },
                    Column {
                        column_type: TextType,
                        column_name: Name("name".to_string())
                    },
                ],
                rows: vec![
                    vec![
                        "Stephen".to_string().into(),
                        16i32.into(),
                        "Stephen".to_string().into(),
                    ],
                    vec![
                        "Adrienne".to_string().into(),
                        23i32.into(),
                        "Adrienne".to_string().into(),
                    ]
                ]
            }
        );

        fn run_stmt(b: &mut InMemoryBackend, s: &str) -> Execute {
            b.execute(
                &Parser::new(&s.to_string()).parse().unwrap().statements[0]
            ).unwrap()
        }
    }
}
