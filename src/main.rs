extern crate rusty_sql;

use rustyline::error::ReadlineError;
use rustyline::Editor;
use crate::rusty_sql::InMemoryBackend ;

fn main() {
    let mut backend = InMemoryBackend::new();
    let mut rl = Editor::<()>::new();

    println!("Welcome to rusty-sql.");

    loop {
        match rl.readline("# ") {
            Ok(l) => {
                rl.add_history_entry(l.as_str());
                if let Err(err) = backend.run(&l) {
                    eprintln!("{}", err);
                }
            },
            Err(ReadlineError::Eof) => {
                std::process::exit(0);
            },
            Err(ReadlineError::Interrupted) => {
                // TODO Will probably clear a buffer full of input here (multi-line input mode)
            },
            Err(err) => eprintln!("{:?}", err),
        };
    }
}
