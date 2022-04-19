pub mod lex;
pub mod parser;

use std::collections::HashMap;

use parser::Expression;

use crate::parser::{
    Statement,
    Statement::{CreateStatement, InsertStatement, SelectStatement},
    ColumnDefinition,
    Expression::Literal,
};

use crate::lex::{
    KeywordType::{Int, Text},
    Token,
    Token::{Integer, PGString, Identifier},
    IdentifierType::Symbol,
};

#[derive(Debug, Clone)]
pub enum ColumnType {
    TextType,
    IntType,
}

#[derive(Debug)]
pub struct Column {
    column_type: ColumnType,
    name: String,
}

#[derive(Debug)]
pub enum BackendError {
    ErrTableDoesNotExist,
    ErrColumnDoesNotExist,
    ErrInvalidSelectItem,
    ErrInvalidDatatype,
    ErrMissingValues,
}

impl<'a> Into<&'a str> for BackendError {
    fn into(self) -> &'a str {
        match self {
            BackendError::ErrTableDoesNotExist  => "Table does not exist",
            BackendError::ErrColumnDoesNotExist => "Column does not exist",
            BackendError::ErrInvalidSelectItem => "Select item is not valid",
            BackendError::ErrInvalidDatatype => "Invalid datatype",
            BackendError::ErrMissingValues => "Missing values",
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct MemoryCell(Vec<u8>);

impl Into<String> for MemoryCell {
    fn into(self) -> String {
        String::from_utf8_lossy(&self.0).to_string()
    }
}

impl From<String> for MemoryCell {
    fn from(str: String) -> Self {
        MemoryCell(str.as_bytes().to_vec())
    }
}

impl Into<i32> for MemoryCell {
    fn into(self) -> i32 {
        i32::from_le_bytes(
            self.0.try_into().unwrap_or_else(|v: Vec<u8>| {
                panic!("Expected Vec of length 4 but found length {}", v.len())
            })
        )
    }
}

impl From<i32> for MemoryCell {
    fn from(i: i32) -> Self {
        MemoryCell(i.to_le_bytes().to_vec())
    }
}

impl From<Token> for MemoryCell {
    fn from(token: Token) -> MemoryCell {
        match token {
            Integer(i) => i.clone().into(),
            PGString(s) => s.clone().into(),
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

trait Backend {
    fn execute(&mut self, stmt: Statement) -> Result<(), BackendError>;
}

#[derive(Debug, Default)]
pub struct InMemoryBackend {
    tables: HashMap<String, Table>,
}

#[derive(Debug)]
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

    pub fn execute(&mut self, stmt: &Statement) -> Result<Execute, BackendError> {
        match stmt {
            CreateStatement { name, cols } => self.create_table(name.to_string(), cols.to_vec()),
            InsertStatement { table, values } => self.insert(table.to_string(), values.to_vec()),
            SelectStatement { item, from } => self.select(from.to_owned(), item.to_vec()),
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
        values: Vec<Expression>,
    ) -> Result<Execute, BackendError> {
        let table = match self.tables.get_mut(&table_name) {
            Some(t) => t,
            None => return Err(BackendError::ErrTableDoesNotExist),
        };

        if values.is_empty() { return Ok(Execute::Empty); }
        if values.len() != table.columns.len() {
            return Err(BackendError::ErrMissingValues);
        }

        let mut row = Vec::new();
        for val in values {
            match val {
                Literal(e) => {
                    row.push(e.clone().into());
                },
                _ => {
                    eprintln!("Skipping non-literal");
                    continue;
                }
            }
        }

        table.rows.push(row);

        Ok(Execute::Empty)
    }

    fn select(
        &mut self,
        table_name: Option<String>,
        item: Vec<Expression>
    ) -> Result<Execute, BackendError> {
        let table = match table_name.and_then(|n| self.tables.get_mut(&n)) {
            Some(t) => t,
            None => return Err(BackendError::ErrTableDoesNotExist),
        };

        let mut results = Vec::new();
        let mut columns = Vec::new();

        for (i, row) in table.rows.iter().enumerate() {
            let is_first_row = i == 0;

            let mut result = Vec::new();
            for itm in &item {
                match itm {
                    Literal(Identifier(Symbol, column_name)) => {
                        let (c, _) = match table.columns.iter().enumerate().find(|(_, col)| *col == column_name) {
                            Some(c) => c,
                            None => return Err(BackendError::ErrColumnDoesNotExist),
                        };

                        if is_first_row {
                            columns.push(Column {
                                name: column_name.clone(),
                                column_type: table.column_types[c].clone(),
                            });
                        }

                        result.push(row[c].clone())
                    },
                    _ => {
                        eprintln!("Skipping non-literal");
                        continue;
                    }
                }
            }
            results.push(result);
        }

        Ok(Execute::Results{rows: results, columns})
    }
}
