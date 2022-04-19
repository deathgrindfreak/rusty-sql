extern crate sql_db;

use crate::sql_db::{
    InMemoryBackend,
    Execute::Results,
    parser::{Parser, Ast}
};

fn main() {
    let Ast { statements } = Parser::new("
CREATE TABLE test (column1 INT, column2 TEXT);

INSERT INTO test VALUES (123, 'a string');
INSERT INTO test VALUES (456, 'Here''s another string');

SELECT column1, column2 FROM test;
").parse().unwrap();

    let mut backend = InMemoryBackend::new();
    for stmt in statements {
        if let Results {rows, columns} = backend.execute(&stmt).expect("Oh no!") {
            println!("{:?} {:?}", columns, rows);
        }
    }
    println!("{:?}", backend);
}
