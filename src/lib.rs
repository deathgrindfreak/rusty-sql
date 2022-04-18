pub mod lex;
pub mod parser;

use std::collections::HashMap;

use parser::Expression;

use crate::parser::{
    Statement,
    Statement::{CreateStatement, InsertStatement, SelectStatement},
    ColumnDefinition,
};

use crate::lex::{
    KeywordType::{Int, Text},
};

#[derive(Debug)]
enum ColumnType {
    TextType,
    IntType,
}

#[derive(Debug)]
struct Column {
    column_type: ColumnType,
    name: String,
}

#[derive(Debug, Default)]
struct Cell();

#[derive(Debug, Default)]
struct Results {
    columns: Vec<Column>,
    rows: Vec<Vec<Cell>>
}

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

#[derive(Debug, Default)]
pub struct Table {
    columns: Vec<String>,
    column_types: Vec<ColumnType>,
    rows: Vec<Vec<u8>>,
}

trait Backend {
    fn execute(&mut self, stmt: Statement) -> Result<(), BackendError>;
}

pub struct InMemoryBackend {
    // TODO Remove pub
    pub tables: HashMap<String, Table>,
}

impl InMemoryBackend {
    pub fn new() -> Self {
        InMemoryBackend { tables: HashMap::new() }
    }

    pub fn execute(&mut self, stmt: &Statement) -> Result<(), BackendError> {
        match stmt {
            CreateStatement { name, cols } => self.create_table(name.to_string(), cols.to_vec()),
            InsertStatement { table, values } => self.insert(table.to_string(), values.to_vec()),
            // SelectStatement { item, from } => self.select(from, item.to_vec()),
            _ => unimplemented!("Unimplemented statement"),
        }
    }

    fn create_table(
        &mut self,
        table_name: String,
        cols: Vec<ColumnDefinition>,
    ) -> Result<(), BackendError> {
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
        Ok(())
    }

    fn insert(&self, table: String, values: Vec<Expression>) -> Result<(), BackendError> {
        Ok(())
    }

    fn select(
        &self,
        table: Option<String>,
        item: Vec<Expression>
    ) -> Result<(), BackendError> {
        // Ok(Results::default())
        Ok(())
    }
}

// impl Backend for InMemoryBackend {
// }
