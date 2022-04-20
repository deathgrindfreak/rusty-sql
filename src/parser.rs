use std::fmt;
use crate::lex::{
    lex, Token, SymbolType, KeywordType,
    Token::{Symbol, Identifier, Keyword, Integer, Float, PGString},
    SymbolType::{Comma, RightParen, LeftParen, SemiColon},
    IdentifierType::Symbol as SymbolIdentifier,
    KeywordType::{Int, Text, Create, Table, Select, From, As, Or, True, False, Insert, Into, Values},
};

use nom::error::VerboseError;

#[derive(Debug, PartialEq)]
pub struct Ast {
    pub statements: Vec<Statement>
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Literal(Token),
    Binary(Box<BinaryExpr>),
    Expr(Box<Expression>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpr {
    l: Expression,
    r: Expression,
    op: Token,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ColumnDefinition {
    pub name: String,
    pub data_type: KeywordType,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    SelectStatement {
        item: Vec<Expression>,
        from: Option<String>,
    },
    CreateStatement {
        name: String,
        cols: Vec<ColumnDefinition>,
    },
    InsertStatement {
        table: String,
        values: Vec<Expression>,
    },
}

#[derive(Debug)]
pub enum ParseError<'a> {
    LexError(nom::Err<VerboseError<&'a str>>),
    NoMoreTokensError,
    ExpectedSymbolError(SymbolType),
    ExpectedKeywordError(KeywordType),
    ExpectedIdentifierError,
    ParsingError(&'a str),
}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let err_msg = match self {
            ParseError::LexError(_err) => "Lex error.".to_string(),
            ParseError::NoMoreTokensError => "Expected more tokens.".to_string(),
            ParseError::ExpectedIdentifierError => "Expected identifier.".to_string(),
            ParseError::ExpectedSymbolError(s) => format!("Expected '{}' symbol", s),
            ParseError::ExpectedKeywordError(k) => format!("Expected '{}' keyword", k),
            ParseError::ParsingError(s) => s.to_string(),
        };
        write!(f, "{}", err_msg)
    }
}

impl<'a> std::error::Error for ParseError<'a> {}

pub struct Parser<'a> {
    cursor: usize,
    source: &'a str,
    tokens: Vec<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Parser { cursor: 0, source, tokens: Vec::new()}
    }

    pub fn parse(&mut self) -> Result<Ast, ParseError<'a>> {
        self.tokens = match lex(self.source) {
            Ok((_, t)) => t,
            Err(r) => return Err(ParseError::LexError(r)),
        };

        let mut statements = Vec::new();
        while self.cursor < self.tokens.len() {
            statements.push(self.parse_statement()?);

            let mut at_least_one_semicolon = false;
            while self.cursor < self.tokens.len() && self.parse_symbol(SemiColon)? {
                at_least_one_semicolon = true;
            }

            if !at_least_one_semicolon {
                return Err(ParseError::ExpectedSymbolError(SemiColon));
            }
        }

        Ok(Ast { statements })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError<'a>> {
        self.parse_select_statement()
            .or_else(|err| {
                match err {
                    ParseError::ExpectedKeywordError(KeywordType::Select) => {
                        self.parse_insert_statement()
                    },
                    _ => Err(err)
                }
            })
            .or_else(|err| {
                match err {
                    ParseError::ExpectedKeywordError(KeywordType::Insert) => {
                        self.parse_create_table_statement()
                    },
                    _ => Err(err)
                }
            })
    }

    fn parse_select_statement(&mut self) -> Result<Statement, ParseError<'a>> {
        self.expect_keyword(Select)?;
        let item = self.parse_expressions(vec![Keyword(From), Symbol(SemiColon)])?;
        let from = if self.parse_keyword(From)? {
            Some(self.expect_identifier()?)
        } else {
            None
        };
        Ok(Statement::SelectStatement { item, from })
    }

    fn parse_insert_statement(&mut self) -> Result<Statement, ParseError<'a>> {
        self.expect_keyword(Insert)?;
        self.expect_keyword(Into)?;
        let table = self.expect_identifier()?;
        self.expect_keyword(Values)?;
        self.expect_symbol(LeftParen)?;
        let values = self.parse_expressions(vec![Symbol(RightParen)])?;
        self.expect_symbol(RightParen)?;
        Ok(Statement::InsertStatement { table, values })
    }

    fn parse_create_table_statement(&mut self) -> Result<Statement, ParseError<'a>> {
        self.expect_keyword(Create)?;
        self.expect_keyword(Table)?;
        let table_name = self.expect_identifier()?;
        self.expect_symbol(LeftParen)?;

        let cols = self.parse_column_definitions(Symbol(RightParen))?;
        self.cursor += 1;

        Ok(Statement::CreateStatement {
            name: table_name.to_string(),
            cols,
        })
    }

    fn parse_column_definitions(
        &mut self,
        delimiter: Token
    ) -> Result<Vec<ColumnDefinition>, ParseError<'a>> {
        let mut column_definitions = Vec::new();
        loop {
            if self.peek_next_token()? == &delimiter { break }

            if column_definitions.len() > 0 {
                self.expect_symbol(Comma)?;
            }

            let column_name = self.expect_identifier()?;
            let data_type = self.parse_token(|t| match t {
                Keyword(Int) => Some(Int),
                Keyword(Text) => Some(Text),
                _ => None,
            })?.ok_or(ParseError::ParsingError("Expected data type"))?;

            column_definitions.push(
                ColumnDefinition { name: column_name.to_string(), data_type }
            );
        }
        Ok(column_definitions)
    }

    fn parse_expressions(
        &mut self,
        delimiters: Vec<Token>
    ) -> Result<Vec<Expression>, ParseError<'a>> {
        let mut exps = Vec::new();
        loop {
            let token = self.peek_next_token()?;

            if delimiters.iter().any(|d| d == token) { break }

            if exps.len() > 0 {
                self.expect_symbol(Comma)?;
            }

            let expression = self.parse_expression()?;
            exps.push(expression);
        }
        Ok(exps)
    }

    fn parse_expression(
        &mut self,
    ) -> Result<Expression, ParseError<'a>> {
        let expr = self.parse_token(|t| match t {
            Identifier(_, _)
                | PGString(_)
                | Integer(_)
                | Float(_)
                | Symbol(_)
                | Keyword(_)
                => Some(t.clone()),
            _ => None
        })?;

        match expr {
            Some(token) => Ok(Expression::Literal(token)),
            None => Err(ParseError::ParsingError("Expected expression")),
        }
    }

    fn expect_keyword(&mut self, expected: KeywordType) -> Result<(), ParseError<'a>> {
        self.parse_token(|t| match t {
            Keyword(s) if *s == expected => Some(()),
            _ => None
        })?.ok_or(ParseError::ExpectedKeywordError(expected))
    }

    fn parse_keyword(&mut self, expected: KeywordType) -> Result<bool, ParseError<'a>> {
        self.parse_token(|t| match t {
            Keyword(s) => Some(*s == expected),
            _ => None
        }).map(|o| o.unwrap_or(false))
    }

    fn expect_symbol(&mut self, expected: SymbolType) -> Result<(), ParseError<'a>> {
        self.parse_token(|t| match t {
            Symbol(s) if *s == expected => Some(()),
            _ => None
        })?.ok_or(ParseError::ExpectedSymbolError(expected))
    }

    fn parse_symbol(&mut self, expected: SymbolType) -> Result<bool, ParseError<'a>> {
        self.parse_token(|t| match t {
            Symbol(s) => Some(*s == expected),
            _ => None
        }).map(|o| o.unwrap_or(false))
    }

    fn expect_identifier(&mut self) -> Result<String, ParseError<'a>> {
        self.parse_identifier()?.ok_or(ParseError::ExpectedIdentifierError)
    }

    fn parse_identifier(&mut self) -> Result<Option<String>, ParseError<'a>> {
        self.parse_token(|t| match t {
            Identifier(SymbolIdentifier, name) => Some(name.clone()),
            _ => None,
        })
    }

    fn parse_token<F, T>(
        &mut self,
        matcher: F
    ) -> Result<Option<T>, ParseError<'a>>
    where F: Fn(&Token) -> Option<T> {
        let token = self.peek_next_token()?;
        Ok(matcher(&token).map(|t| {
            self.cursor += 1;
            t
        }))
    }

    fn peek_next_token(&self) -> Result<&Token, ParseError<'a>> {
        self.tokens.get(self.cursor).ok_or(ParseError::NoMoreTokensError)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::lex::{
        Token::{Symbol, Identifier, Keyword, Integer, Float, PGString},
        SymbolType::Asterisk,
        IdentifierType::{Symbol as SymbolIdentifier, DoubleQuote},
        KeywordType::{Int, Text, As},
    };

    #[test]
    fn test_create_table() {
        let def = Parser::new("CREATE TABLE a_table_name (column1 INT);").parse().unwrap();
        assert_eq!(
            def,
            Ast {
                statements: vec![
                    Statement::CreateStatement {
                        name: "a_table_name".to_string(),
                        cols: vec![
                            ColumnDefinition {
                                name: "column1".to_string(),
                                data_type: Int
                            },
                        ]
                    }
                ]
            }
        );

        let def = Parser::new(
            "CREATE TABLE table_name (
                column1 INT,
                column2 TEXT,
                column3 INT
            );").parse().unwrap();
        assert_eq!(
            def,
            Ast {
                statements: vec![
                    Statement::CreateStatement {
                        name: "table_name".to_string(),
                        cols: vec![
                            ColumnDefinition {name: "column1".to_string(), data_type: Int},
                            ColumnDefinition {name: "column2".to_string(), data_type: Text},
                            ColumnDefinition {name: "column3".to_string(), data_type: Int},
                        ]
                    }
                ]
            }
        );
    }

    #[test]
    fn test_basic_select() {
        let def = Parser::new("SELECT 1;").parse().unwrap();
        assert_eq!(
            def,
            Ast {
                statements: vec![
                    Statement::SelectStatement {
                        item: vec![Expression::Literal(Integer(1))],
                        from: None,
                    }
                ],
            }
        );

        let def = Parser::new("SELECT * FROM table_name;").parse().unwrap();
        assert_eq!(
            def,
            Ast {
                statements: vec![
                    Statement::SelectStatement {
                        item: vec![
                            Expression::Literal(Symbol(Asterisk))
                        ],
                        from: Some("table_name".to_string()),
                    }
                ],
            }
        );

        let def = Parser::new("SELECT column1, column2, column3 FROM table_name;").parse().unwrap();
        assert_eq!(
            def,
            Ast {
                statements: vec![
                    Statement::SelectStatement {
                        item: vec![
                            Expression::Literal(Identifier(SymbolIdentifier, "column1".to_string())),
                            Expression::Literal(Identifier(SymbolIdentifier, "column2".to_string())),
                            Expression::Literal(Identifier(SymbolIdentifier, "column3".to_string())),
                        ],
                        from: Some("table_name".to_string()),
                    }
                ],
            }
        );
    }

    #[test]
    fn test_insert() {
        let def = Parser::new(
            "INSERT INTO table_name VALUES ('a string', 123, 2.3e+12);"
        ).parse().unwrap();
        assert_eq!(
            def,
            Ast {
                statements: vec![
                    Statement::InsertStatement {
                        table: "table_name".to_string(),
                        values: vec![
                            Expression::Literal(PGString("a string".to_string())),
                            Expression::Literal(Integer(123)),
                            Expression::Literal(Float(2.3e+12)),
                        ]
                    }
                ],
            }
        );
    }

    #[test]
    fn test_multiple_statements() {
        let def = Parser::new("
            CREATE TABLE table_name (
                column1 INT,
                column2 TEXT,
                column3 INT
            );

            INSERT INTO table_name
            VALUES (123, 'a string', 2.3e+12);

            SELECT
                column1,
                column2,
                column3
            FROM table_name;
        ").parse().unwrap();

        assert_eq!(
            def,
            Ast {
                statements: vec![
                    Statement::CreateStatement {
                        name: "table_name".to_string(),
                        cols: vec![
                            ColumnDefinition {
                                name: "column1".to_string(),
                                data_type: Int
                            },
                            ColumnDefinition {
                                name: "column2".to_string(),
                                data_type: Text
                            },
                            ColumnDefinition {
                                name: "column3".to_string(),
                                data_type: Int
                            },
                        ]
                    },

                    Statement::InsertStatement {
                        table: "table_name".to_string(),
                        values: vec![
                            Expression::Literal(Integer(123)),
                            Expression::Literal(PGString("a string".to_string())),
                            Expression::Literal(Float(2.3e+12)),
                        ]
                    },

                    Statement::SelectStatement {
                        item: vec![
                            Expression::Literal(Identifier(SymbolIdentifier, "column1".to_string())),
                            Expression::Literal(Identifier(SymbolIdentifier, "column2".to_string())),
                            Expression::Literal(Identifier(SymbolIdentifier, "column3".to_string())),
                        ],
                        from: Some("table_name".to_string()),
                    }
                ],
            }
        );
    }

    #[ignore]
    fn test_select_with_alias() {
        let def = Parser::new("SELECT column1 as \"column_one\" FROM table_name;").parse().unwrap();
        assert_eq!(
            def,
            Ast {
                statements: vec![
                    Statement::SelectStatement {
                        item: vec![
                            Expression::Literal(Identifier(SymbolIdentifier, "column1".to_string())),
                            Expression::Literal(Keyword(As)),
                            Expression::Literal(Identifier(DoubleQuote, "column_one".to_string())),
                        ],
                        from: Some("table_name".to_string()),
                    }
                ],
            }
        );

        let def = Parser::new("
            SELECT
                column1 as \"column_one\",
                column2 as \"column_two\",
                column3 as \"column_three\"
            FROM table_name;
        ").parse().unwrap();
        assert_eq!(
            def,
            Ast {
                statements: vec![
                    Statement::SelectStatement {
                        item: vec![
                            Expression::Literal(Identifier(SymbolIdentifier, "column1".to_string())),
                            Expression::Literal(Keyword(As)),
                            Expression::Literal(Identifier(DoubleQuote, "column_one".to_string())),
                            Expression::Literal(Identifier(SymbolIdentifier, "column2".to_string())),
                            Expression::Literal(Keyword(As)),
                            Expression::Literal(Identifier(DoubleQuote, "column_two".to_string())),
                            Expression::Literal(Identifier(SymbolIdentifier, "column3".to_string())),
                            Expression::Literal(Keyword(As)),
                            Expression::Literal(Identifier(DoubleQuote, "column_three".to_string())),
                        ],
                        from: Some("table_name".to_string()),
                    }
                ],
            }
        );
    }
}
