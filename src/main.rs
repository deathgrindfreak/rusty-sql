extern crate sql_db;

use crate::sql_db::{
    InMemoryBackend,
    parser::{Parser, Ast}
};

fn main() {
    let Ast { statements } = Parser::new("
CREATE TABLE test (column1 INT, column2 TEXT);

INSERT INTO test VALUES (123, 'a string');
").parse().unwrap();

    let mut backend = InMemoryBackend::new();
    for stmt in statements {
        backend.execute(&stmt);
    }
    println!("{:?}", backend);
}
