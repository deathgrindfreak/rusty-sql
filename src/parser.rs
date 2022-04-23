use std::fmt;
use crate::lex::{
    lex, Token, SymbolType, KeywordType,
    Token::{Symbol, Identifier, Keyword, Integer, Float, PGString},
    SymbolType::{
        Comma, RightParen, LeftParen, SemiColon,
        Concatenate, Plus, Equals, NotEquals, Asterisk,
    },
    IdentifierType::Symbol as SymbolIdentifier,
    KeywordType::{
        Int, Text, Create, Table, Select, From, Where,
        And, Or, True, False, Insert, Into, Values, As
    },
};

#[derive(Debug, PartialEq)]
pub struct Ast {
    pub statements: Vec<Statement>
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Literal(Token),
    Binary {
        l: Box<Expression>,
        r: Box<Expression>,
        op: Token,
    },
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
        where_cond: Option<Expression>,
    },
    CreateStatement {
        name: String,
        cols: Vec<ColumnDefinition>,
    },
    InsertStatement {
        table: String,
        values: Vec<Vec<Expression>>,
    },
}

#[derive(Debug)]
pub enum ParseError {
    LexError,
    NoMoreTokensError,
    ExpectedSymbolError(SymbolType),
    ExpectedKeywordError(KeywordType),
    ExpectedIdentifierError,
    ExpectedBinaryOperator,
    ParsingError(String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::LexError => write!(f, "Lex error."),
            ParseError::NoMoreTokensError => write!(f, "Expected more tokens."),
            ParseError::ExpectedIdentifierError => write!(f, "Expected identifier."),
            ParseError::ExpectedSymbolError(s) => write!(f, "Expected '{}' symbol", s),
            ParseError::ExpectedKeywordError(k) => write!(f, "Expected '{}' keyword", k),
            ParseError::ExpectedBinaryOperator => write!(f, "Expected binary operator"),
            ParseError::ParsingError(s) => write!(f, "{}", s),
        }
    }
}

impl std::error::Error for ParseError {}

impl Token {
    fn binding_power(&self) -> i32 {
        match self {
            Keyword(k) => match k {
                And | Or => 1,
                _ => 0,
            },
            Symbol(s) => match s {
                Equals | NotEquals | Concatenate | Plus => 3,
                _ => 0,
            },
            _ => 0
        }
    }
}

pub struct Parser<'a> {
    cursor: usize,
    source: &'a str,
    tokens: Vec<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Parser { cursor: 0, source, tokens: Vec::new()}
    }

    pub fn parse(&mut self) -> Result<Ast, ParseError> {
        self.tokens = match lex(self.source) {
            Ok((_, t)) => t,
            Err(_) => return Err(ParseError::LexError),
        };

        let mut statements = Vec::new();
        while self.cursor < self.tokens.len() {
            statements.push(self.parse_statement()?);

            let mut at_least_one_semicolon = false;
            while self.cursor < self.tokens.len() && self.parse_symbol(&SemiColon)? {
                at_least_one_semicolon = true;
            }

            if !at_least_one_semicolon {
                return Err(ParseError::ExpectedSymbolError(SemiColon));
            }
        }

        Ok(Ast { statements })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
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

    fn parse_select_statement(&mut self) -> Result<Statement, ParseError> {
        self.expect_keyword(Select)?;

        let delimiters = vec![Keyword(From), Symbol(SemiColon)];

        let item = self.parse_expressions(delimiters.to_vec())?;
        let from = if self.parse_keyword(&From)? {
            Some(self.expect_identifier()?)
        } else {
            None
        };

        let where_cond = if self.parse_keyword(&Where)? {
            Some(self.parse_expression(delimiters, 0)?)
        } else {
            None
        };
        Ok(Statement::SelectStatement { item, from, where_cond })
    }

    fn parse_insert_statement(&mut self) -> Result<Statement, ParseError> {
        self.expect_keyword(Insert)?;
        self.expect_keyword(Into)?;
        let table = self.expect_identifier()?;
        self.expect_keyword(Values)?;

        let mut values = Vec::new();
        loop {
            self.expect_symbol(LeftParen)?;
            let value = self.parse_expressions(vec![Symbol(RightParen)])?;
            self.expect_symbol(RightParen)?;

            values.push(value);

            if !self.parse_symbol(&Comma)? { break }
        }

        Ok(Statement::InsertStatement { table, values })
    }

    fn parse_create_table_statement(&mut self) -> Result<Statement, ParseError> {
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
    ) -> Result<Vec<ColumnDefinition>, ParseError> {
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
            })?.ok_or(ParseError::ParsingError("Expected data type".to_string()))?;

            column_definitions.push(
                ColumnDefinition { name: column_name.to_string(), data_type }
            );
        }
        Ok(column_definitions)
    }

    fn parse_expressions(
        &mut self,
        delimiters: Vec<Token>
    ) -> Result<Vec<Expression>, ParseError> {
        let mut exps = Vec::new();
        loop {
            let token = self.peek_next_token()?;
            if delimiters.iter().any(|d| d == token) { break }

            if exps.len() > 0 {
                self.expect_symbol(Comma)?;
            }

            let delims = delimiters.to_vec()
                                   .iter()
                                   .chain(&vec![Symbol(Comma), Symbol(RightParen)])
                                   .map(|x| x.clone())
                                   .collect();

            let expression = self.parse_expression(delims, 0)?;
            exps.push(expression);
        }
        Ok(exps)
    }

    fn parse_expression(
        &mut self,
        delimiters: Vec<Token>,
        min_bp: i32,
    ) -> Result<Expression, ParseError> {
        let mut expr = if self.parse_symbol(&LeftParen)? {
            let delims = delimiters.to_vec()
                                   .iter()
                                   .chain(&vec![Symbol(RightParen)])
                                   .map(|x| x.clone())
                                   .collect();

            let expr = self.parse_expression(delims, min_bp)?;
            self.expect_symbol(RightParen)?;
            expr
        } else {
            self.parse_literal_expression()?
        };

        while self.cursor < self.tokens.len() {
            let token = self.peek_next_token()?;
            if delimiters.iter().any(|d| d == token) { break }

            let op = self.parse_token(|t| match t {
                Keyword(k) => match k {
                    And | Or | As => Some(t.clone()),
                    _ => None
                },
                Symbol(s) => match s {
                    Equals | NotEquals | Concatenate | Plus => Some(t.clone()),
                    _ => None
                },
                _ => None
            })?.ok_or(ParseError::ExpectedBinaryOperator)?;

            let bp = op.binding_power();
            if bp < min_bp {
                self.cursor -= 1;
                break
            }

            expr = Expression::Binary {
                l: Box::new(expr),
                r: Box::new(self.parse_expression(delimiters.to_vec(), bp)?),
                op
            };
        }

        Ok(expr)
    }

    fn parse_literal_expression(&mut self) -> Result<Expression, ParseError> {
        let expr = self.parse_token(|t| match t {
            Keyword(k) => match k {
                Int | Text | True | False => Some(t.clone()),
                _ => None
            },
            Symbol(s) => match s {
                Asterisk => Some(t.clone()),
                _ => None
            },
            Identifier(_, _) | PGString(_) | Integer(_) | Float(_) => Some(t.clone()),
        })?;

        match expr {
            Some(token) => Ok(Expression::Literal(token)),
            None => Err(ParseError::ParsingError("Expected expression".to_string())),
        }
    }

    fn expect_keyword(&mut self, expected: KeywordType) -> Result<(), ParseError> {
        self.parse_keyword(&expected).and_then(|matched| {
            if matched { Ok(()) } else { Err(ParseError::ExpectedKeywordError(expected)) }
        })
    }

    fn expect_symbol(&mut self, expected: SymbolType) -> Result<(), ParseError> {
        self.parse_symbol(&expected).and_then(|matched| {
            if matched { Ok(()) } else { Err(ParseError::ExpectedSymbolError(expected)) }
        })
    }

    fn parse_keyword(&mut self, expected: &KeywordType) -> Result<bool, ParseError> {
        self.parse_token(|t| match t {
            Keyword(s) if s == expected => Some(true),
            _ => None
        }).map(|o| o.unwrap_or(false))
    }

    fn parse_symbol(&mut self, expected: &SymbolType) -> Result<bool, ParseError> {
        self.parse_token(|t| match t {
            Symbol(s) if s == expected => Some(true),
            _ => None
        }).map(|o| o.unwrap_or(false))
    }

    fn expect_identifier(&mut self) -> Result<String, ParseError> {
        self.parse_identifier()?.ok_or(ParseError::ExpectedIdentifierError)
    }

    fn parse_identifier(&mut self) -> Result<Option<String>, ParseError> {
        self.parse_token(|t| match t {
            Identifier(SymbolIdentifier, name) => Some(name.clone()),
            _ => None,
        })
    }

    fn parse_token<F, T>(&mut self, matcher: F) -> Result<Option<T>, ParseError>
    where F: Fn(&Token) -> Option<T> {
        let token = self.peek_next_token()?;
        Ok(matcher(&token).map(|t| {
            self.cursor += 1;
            t
        }))
    }

    fn peek_next_token(&self) -> Result<&Token, ParseError> {
        self.tokens.get(self.cursor).ok_or(ParseError::NoMoreTokensError)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::lex::{
        Token::{Symbol, Identifier, Keyword, Integer, Float, PGString},
        SymbolType::{Asterisk, Equals},
        IdentifierType::{Symbol as SymbolIdentifier, DoubleQuote},
        KeywordType::{Int, Text, As, And},
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
        assert_eq!(
            Parser::new("SELECT 1;").parse().unwrap(),
            Ast {
                statements: vec![
                    Statement::SelectStatement {
                        item: vec![Expression::Literal(Integer(1))],
                        from: None,
                        where_cond: None,
                    }
                ],
            }
        );

        assert_eq!(
            Parser::new("SELECT 'a string' || ' ' || 'another string';").parse().unwrap(),
            Ast {
                statements: vec![
                    Statement::SelectStatement {
                        item: vec![
                            Expression::Binary {
                                l: Box::new(
                                    Expression::Literal(PGString("a string".to_string()))
                                ),
                                r: Box::new(Expression::Binary {
                                    l: Box::new(
                                        Expression::Literal(PGString(" ".to_string()))
                                    ),
                                    r: Box::new(
                                        Expression::Literal(PGString("another string".to_string()))
                                    ),
                                    op: Symbol(Concatenate)
                                }),
                                op: Symbol(Concatenate)
                            }
                        ],
                        from: None,
                        where_cond: None,
                    }
                ],
            }
        );

        assert_eq!(
            Parser::new("SELECT * FROM table_name;").parse().unwrap(),
            Ast {
                statements: vec![
                    Statement::SelectStatement {
                        item: vec![
                            Expression::Literal(Symbol(Asterisk))
                        ],
                        from: Some("table_name".to_string()),
                        where_cond: None,
                    }
                ],
            }
        );

        assert_eq!(
            Parser::new("SELECT column1, column2, column3 FROM table_name;").parse().unwrap(),
            Ast {
                statements: vec![
                    Statement::SelectStatement {
                        item: vec![
                            Expression::Literal(Identifier(SymbolIdentifier, "column1".to_string())),
                            Expression::Literal(Identifier(SymbolIdentifier, "column2".to_string())),
                            Expression::Literal(Identifier(SymbolIdentifier, "column3".to_string())),
                        ],
                        from: Some("table_name".to_string()),
                        where_cond: None,
                    }
                ],
            }
        );
    }

    #[test]
    fn test_select_with_where() {
        assert_eq!(
            Parser::new("SELECT * FROM table_name WHERE column1 = 123;").parse().unwrap(),
            Ast {
                statements: vec![
                    Statement::SelectStatement {
                        item: vec![
                            Expression::Literal(Symbol(Asterisk))
                        ],
                        from: Some("table_name".to_string()),
                        where_cond: Some(
                            Expression::Binary {
                                l: Box::new(Expression::Literal(Identifier(SymbolIdentifier, "column1".to_string()))),
                                r: Box::new(Expression::Literal(Integer(123))),
                                op: Symbol(Equals)
                            }
                        ),
                    }
                ],
            }
        );

        assert_eq!(
            Parser::new("
                SELECT * FROM table_name
                WHERE column1 = 123
                AND column2 = 'foo' || column3;
            ").parse().unwrap(),
            Ast {
                statements: vec![
                    Statement::SelectStatement {
                        item: vec![
                            Expression::Literal(Symbol(Asterisk))
                        ],
                        from: Some("table_name".to_string()),
                        where_cond: Some(
                            Expression::Binary {
                                l: Box::new(Expression::Binary {
                                    l: Box::new(Expression::Literal(Identifier(SymbolIdentifier, "column1".to_string()))),
                                    r: Box::new(Expression::Literal(Integer(123))),
                                    op: Symbol(Equals)
                                }),
                                r: Box::new(Expression::Binary {
                                    l: Box::new(Expression::Literal(Identifier(SymbolIdentifier, "column2".to_string()))),
                                    r: Box::new(Expression::Binary {
                                        l: Box::new(Expression::Literal(PGString("foo".to_string()))),
                                        r: Box::new(Expression::Literal(Identifier(SymbolIdentifier, "column3".to_string()))),
                                        op: Symbol(Concatenate)
                                    }),
                                    op: Symbol(Equals)
                                }),
                                op: Keyword(And),
                            }
                        ),
                    }
                ],
            }
        );

        assert_eq!(
            Parser::new("
                SELECT * FROM table_name
                WHERE column1 = 123
                AND column2 = 'foo' || column3
                OR column4 = true;
            ").parse().unwrap(),
            Ast {
                statements: vec![
                    Statement::SelectStatement {
                        item: vec![
                            Expression::Literal(Symbol(Asterisk))
                        ],
                        from: Some("table_name".to_string()),
                        where_cond: Some(
                            Expression::Binary {
                                l: Box::new(Expression::Binary {
                                    l: Box::new(Expression::Literal(Identifier(SymbolIdentifier, "column1".to_string()))),
                                    r: Box::new(Expression::Literal(Integer(123))),
                                    op: Symbol(Equals)
                                }),
                                r: Box::new(Expression::Binary {
                                    l: Box::new(Expression::Binary {
                                        l: Box::new(Expression::Literal(Identifier(SymbolIdentifier, "column2".to_string()))),
                                        r: Box::new(Expression::Binary {
                                            l: Box::new(Expression::Literal(PGString("foo".to_string()))),
                                            r: Box::new(Expression::Literal(Identifier(SymbolIdentifier, "column3".to_string()))),
                                            op: Symbol(Concatenate)
                                        }),
                                        op: Symbol(Equals)
                                    }),
                                    r: Box::new(Expression::Binary {
                                        l: Box::new(Expression::Literal(Identifier(SymbolIdentifier, "column4".to_string()))),
                                        r: Box::new(Expression::Literal(Keyword(True))),
                                        op: Symbol(Equals)
                                    }),
                                    op: Keyword(Or),
                                }),
                                op: Keyword(And),
                            },
                        ),
                    }
                ],
            }
        );
    }

    #[test]
    fn test_insert() {
        assert_eq!(
            Parser::new("INSERT INTO table_name VALUES ('a string', 123, 2.3e+12);").parse().unwrap(),
            Ast {
                statements: vec![
                    Statement::InsertStatement {
                        table: "table_name".to_string(),
                        values: vec![
                            vec![
                                Expression::Literal(PGString("a string".to_string())),
                                Expression::Literal(Integer(123)),
                                Expression::Literal(Float(2.3e+12)),
                            ]
                        ]
                    }
                ],
            }
        );

        assert_eq!(
            Parser::new("INSERT INTO table_name VALUES ('some string' || 's', 123 + 2.3e+12);").parse().unwrap(),
            Ast {
                statements: vec![
                    Statement::InsertStatement {
                        table: "table_name".to_string(),
                        values: vec![
                            vec![
                                Expression::Binary {
                                    l: Box::new(Expression::Literal(PGString("some string".to_string()))),
                                    r: Box::new(Expression::Literal(PGString("s".to_string()))),
                                    op: Symbol(Concatenate)
                                },
                                Expression::Binary {
                                    l: Box::new(Expression::Literal(Integer(123))),
                                    r: Box::new(Expression::Literal(Float(2.3e12))),
                                    op: Symbol(Plus)
                                },
                            ]
                        ]
                    }
                ],
            }
        );

        assert_eq!(
            Parser::new("
                INSERT INTO table_name
                VALUES ('a string', 123), ('another string', 234), ('a different string', 42);
            ").parse().unwrap(),
            Ast {
                statements: vec![
                    Statement::InsertStatement {
                        table: "table_name".to_string(),
                        values: vec![
                            vec![
                                Expression::Literal(PGString("a string".to_string())),
                                Expression::Literal(Integer(123)),
                            ],
                            vec![
                                Expression::Literal(PGString("another string".to_string())),
                                Expression::Literal(Integer(234)),
                            ],
                            vec![
                                Expression::Literal(PGString("a different string".to_string())),
                                Expression::Literal(Integer(42)),
                            ],
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
                            vec![
                                Expression::Literal(Integer(123)),
                                Expression::Literal(PGString("a string".to_string())),
                                Expression::Literal(Float(2.3e+12)),
                            ]
                        ]
                    },

                    Statement::SelectStatement {
                        item: vec![
                            Expression::Literal(Identifier(SymbolIdentifier, "column1".to_string())),
                            Expression::Literal(Identifier(SymbolIdentifier, "column2".to_string())),
                            Expression::Literal(Identifier(SymbolIdentifier, "column3".to_string())),
                        ],
                        from: Some("table_name".to_string()),
                        where_cond: None,
                    }
                ],
            }
        );
    }

    #[test]
    fn test_binary_expressions() {
        assert_eq!(
            run_binary_expression("('a string' || ' ' || 'another string')").unwrap(),
            Expression::Binary {
                l: Box::new(Expression::Literal(PGString("a string".to_string()))),
                r: Box::new(Expression::Binary {
                    l: Box::new(Expression::Literal(PGString(" ".to_string()))),
                    r: Box::new(Expression::Literal(PGString("another string".to_string()))),
                    op: Symbol(Concatenate)
                }),
                op: Symbol(Concatenate)
            }
        );

        assert_eq!(
            run_binary_expression("(1 + 2 + 3)").unwrap(),
            Expression::Binary {
                l: Box::new(Expression::Literal(Integer(1))),
                r: Box::new(Expression::Binary {
                    l: Box::new(Expression::Literal(Integer(2))),
                    r: Box::new(Expression::Literal(Integer(3))),
                    op: Symbol(Plus)
                }),
                op: Symbol(Plus)
            }
        );
    }

    fn run_binary_expression<'a>(e: &'a str) -> Result<Expression, ParseError> {
        let mut def = Parser::new(e);

        def.tokens = match lex(def.source) {
            Ok((_, t)) => t,
            Err(r) => panic!("{}", r),
        };

        def.parse_expression(vec![], 0)
    }

    #[test]
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
                        where_cond: None,
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
                        where_cond: None,
                    }
                ],
            }
        );
    }
}
