extern crate sql_db;

use crate::sql_db::{
    InMemoryBackend,
    parser::{Parser, Ast}
};

fn main() {
    let Ast { statements } = Parser::new(
        "CREATE TABLE test (column1 INT, column2 TEXT);"
    ).parse().unwrap();

    let mut backend = InMemoryBackend::new();
    backend.execute(statements.first().unwrap().clone());
    println!("{:?}", backend.tables);
}
