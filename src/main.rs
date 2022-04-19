extern crate rusty_sql;

use rustyline::error::ReadlineError;
use rustyline::{Editor, Helper};
use crate::rusty_sql::util::PrintTable;
use crate::rusty_sql::{
    InMemoryBackend,
    Execute::Results,
    Column,
    ColumnType::{IntType, TextType},
};
use crate::rusty_sql::parser::{
    Parser,
    Ast,
    Statement::{
        CreateStatement,
        SelectStatement,
        InsertStatement,
    },
};

fn main() {
    let mut backend = InMemoryBackend::new();
    let mut rl = Editor::<()>::new();
    println!("Welcome to rusty-sql.");

    loop {
        match rl.readline("# ") {
            Ok(l) => {
                rl.add_history_entry(l.as_str());
                if let Err(err) = run_sql(&l, &mut backend) {
                    eprintln!("{:?}", err);
                }
            },
            Err(ReadlineError::Eof) => {
                std::process::exit(0);
            },
            Err(ReadlineError::Interrupted) => {
                // TODO Will probably clear a buffer full of input here
            },
            Err(err) => eprintln!("{:?}", err),
        };
    }
}

fn run_sql<'a>(
    line: &'a String,
    backend: &mut InMemoryBackend,
) -> Result<(), Box<dyn std::error::Error + 'a>> {
    let Ast { statements } = Parser::new(&line).parse()?;
    for stmt in statements {
        match stmt {
            CreateStatement { .. } => {
                backend.execute(&stmt)?;
                println!("CREATE TABLE");
            },
            InsertStatement { .. } => {
                backend.execute(&stmt)?;
                println!("INSERT 0 1");
            },
            SelectStatement { .. } => {
                if let Results {rows, columns} = backend.execute(&stmt)? {
                    let mut tbl = PrintTable::new();

                    let header: Vec<String> = columns.iter().map(|Column { name, .. }| name.clone()).collect();
                    tbl.header(&header);

                    for row in rows {
                        tbl.add(
                            &row.iter().enumerate().map(|(i, cell)| {
                                match columns[i].column_type {
                                    IntType => {
                                        let r: i32 = cell.clone().into();
                                        r.to_string()
                                    },
                                    TextType => cell.clone().into()
                                }
                            }).collect()
                        );
                    }

                    println!("");
                    tbl.print();
                    println!("");
                }
            },
        }
    }

    Ok(())
}
