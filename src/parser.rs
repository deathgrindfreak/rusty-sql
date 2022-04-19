use crate::lex::{
    lex, Token, SymbolType, KeywordType,
    Token::{Symbol, Identifier, Keyword, Integer, Float, PGString},
    SymbolType::{Comma, RightParen, LeftParen, SemiColon},
    IdentifierType::Symbol as SymbolIdentifier,
    KeywordType::{Int, Text, Create, Table, Select, From, As, Insert, Into, Values},
};

use nom::error::VerboseError;

#[derive(Debug, PartialEq)]
pub struct Ast {
    pub statements: Vec<Statement>
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Literal(Token),
    Expr(Box<Expression>),
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
    ParsingError(&'a str),
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

    pub fn parse(&mut self) -> Result<Ast, ParseError<'a>> {
        self.tokens = match lex(self.source) {
            Ok((_, t)) => t,
            Err(r) => return Err(ParseError::LexError(r)),
        };

        let mut statements = Vec::new();
        while self.cursor < self.tokens.len() {
            let statement = self.parse_statement()?;
            statements.push(statement);

            let mut at_least_one_semicolon = false;
            while self.cursor < self.tokens.len() && self.expect_symbol(SemiColon)? {
                self.cursor += 1;
                at_least_one_semicolon = true;
            }

            if !at_least_one_semicolon {
                return Err(ParseError::ParsingError("Expected semicolon"));
            }
        }

        Ok(Ast { statements })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError<'a>> {
        if let Ok(s) = self.parse_select_statement() {
            Ok(s)
        } else if let Ok(s) = self.parse_insert_statement() {
            Ok(s)
        } else if let Ok(s) = self.parse_create_table_statement() {
            Ok(s)
        } else {
            Err(ParseError::ParsingError("Expected statement"))
        }
    }

    fn parse_select_statement(&mut self) -> Result<Statement, ParseError<'a>> {
        if self.expect_keyword(Select)? {
            self.cursor += 1;
        } else {
            return Err(ParseError::ParsingError("Expected select keyword"))
        }

        Ok(Statement::SelectStatement {
            item: self.parse_expressions(vec![Keyword(From), Symbol(SemiColon)])?,
            from: if self.expect_keyword(From)? {
                self.cursor += 1;

                let from = self.parse_token(|t| match t {
                    Identifier(SymbolIdentifier, name) => Some(name.clone()),
                    _ => None
                })?;

                if from.is_none() {
                    return Err(ParseError::ParsingError("Expected FROM token"))
                }

                from
            } else {
                None
            }
        })
    }

    fn parse_insert_statement(&mut self) -> Result<Statement, ParseError<'a>> {
        if self.expect_keyword(Insert)? {
            self.cursor += 1;
        } else {
            return Err(ParseError::ParsingError("Expected insert keyword"));
        }

        if self.expect_keyword(Into)? {
            self.cursor += 1;
        } else {
            return Err(ParseError::ParsingError("Expected into keyword"));
        }

        let table_name = self.parse_token(|t| match t {
            Identifier(SymbolIdentifier, name) => Some(name.clone()),
            _ => None
        })?;

        if table_name.is_none() {
            return Err(ParseError::ParsingError("Expected table name"));
        }

        if self.expect_keyword(Values)? {
            self.cursor += 1;
        } else {
            return Err(ParseError::ParsingError("Expected values keyword"));
        }

        if self.expect_symbol(LeftParen)? {
            self.cursor += 1;
        } else {
            return Err(ParseError::ParsingError("Expected left paren"));
        }

        let values = self.parse_expressions(vec![Symbol(RightParen)])?;

        if self.expect_symbol(RightParen)? {
            self.cursor += 1;
        } else {
            return Err(ParseError::ParsingError("Expected right paren"));
        }

        Ok(Statement::InsertStatement { table: table_name.unwrap(), values })
    }

    fn parse_expressions(
        &mut self,
        delimiters: Vec<Token>
    ) -> Result<Vec<Expression>, ParseError<'a>> {
        let mut exps = Vec::new();
        loop {
            let token = self.peek_next_token()?;

            if delimiters.iter().any(|d| d == token) {
                break;
            }

            if exps.len() > 0 {
                let was_comma = self.expect_symbol(Comma)?;
                if was_comma {
                    self.cursor += 1;
                } else {
                    return Err(ParseError::ParsingError("Expected comma"));
                }
            }

            let expression = self.parse_expression()?;
            exps.push(expression);
        }
        Ok(exps)
    }

    fn parse_expression(&mut self) -> Result<Expression, ParseError<'a>> {
        let expr = self.parse_token(|t| {
            match t {
                Identifier(_, _) | PGString(_) | Integer(_) | Float(_) | Symbol(_) | Keyword(As) => Some(t.clone()),
                _ => None
            }
        })?;

        match expr {
            Some(token) => Ok(Expression::Literal(token)),
            None => Err(ParseError::ParsingError("Expected expression")),
        }
    }

    fn parse_create_table_statement(&mut self) -> Result<Statement, ParseError<'a>> {
        if self.expect_keyword(Create)? {
            self.cursor += 1;
        } else {
            return Err(ParseError::ParsingError("Expected create keyword"))
        }

        if self.expect_keyword(Table)? {
            self.cursor += 1;
        } else {
            return Err(ParseError::ParsingError("Expected table keyword"))
        }

        let table_name = self.parse_token(|t| match t {
            Identifier(SymbolIdentifier, name) => Some(name.clone()),
            _ => None
        })?;

        if self.expect_symbol(LeftParen)? {
            self.cursor += 1;
        } else {
            return Err(ParseError::ParsingError("Expected left paren"))
        }

        let cols = self.parse_column_definitions(Symbol(RightParen))?;
        self.cursor += 1;

        match table_name {
            Some(name) => Ok(
                Statement::CreateStatement {
                    name: name.to_string(),
                    cols,
                }
            ),
            _ => Err(ParseError::ParsingError("Expected table name"))
        }
    }

    fn parse_column_definitions(
        &mut self,
        delimiter: Token
    ) -> Result<Vec<ColumnDefinition>, ParseError<'a>> {
        let mut column_definitions = Vec::new();
        loop {
            if self.peek_next_token()? == &delimiter { break; }

            if column_definitions.len() > 0 {
                let was_comma = self.expect_symbol(Comma)?;
                if was_comma {
                    self.cursor += 1;
                } else {
                    return Err(ParseError::ParsingError("Expected comma"));
                }
            }

            let column_name = self.parse_token(|t| match t {
                Identifier(SymbolIdentifier, name) => Some(name.clone()),
                _ => None,
            })?;

            let column_type = self.parse_token(|t| match t {
                Keyword(Int) => Some(Int),
                Keyword(Text) => Some(Text),
                _ => None,
            })?;

            match (column_name, column_type) {
                (Some(name), Some(data_type)) =>
                    column_definitions.push(ColumnDefinition {name: name.to_string(), data_type}),
                (Some(_), None) => return Err(ParseError::ParsingError("Expected data type")),
                _ => return Err(ParseError::ParsingError("Expecting column definition")),
            }
        }
        Ok(column_definitions)
    }

    fn expect_keyword(&mut self, expected: KeywordType) -> Result<bool, ParseError<'a>> {
        self.expect_token(|t| match t {
            Keyword(s) => *s == expected,
            _ => false
        })
    }

    fn expect_symbol(&mut self, expected: SymbolType) -> Result<bool, ParseError<'a>> {
        self.expect_token(|t| match t {
            Symbol(s) => *s == expected,
            _ => false
        })
    }

    fn expect_token<F>(
        &self,
        expected: F
    ) -> Result<bool, ParseError<'a>>
    where F: Fn(&Token) -> bool{
        let token = self.peek_next_token()?;
        Ok(expected(token))
    }

    fn parse_token<F, T>(
        &mut self,
        matcher: F
    ) -> Result<Option<T>, ParseError<'a>>
    where F: Fn(&Token) -> Option<T> {
        let token = self.peek_next_token()?;
        Ok(
            if let Some(t) = matcher(&token) {
                self.cursor += 1;
                Some(t)
            } else {
                None
            }
        )
    }

    fn peek_next_token(&self) -> Result<&Token, ParseError<'a>> {
        match self.tokens.get(self.cursor) {
            Some(token) => Ok(token),
            None => Err(ParseError::NoMoreTokensError),
        }
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

        let def = Parser::new("SELECT
column1 as \"column_one\",
column2 as \"column_two\",
column3 as \"column_three\"
FROM table_name;").parse().unwrap();
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
