use crate::lex::{lex, Token, SymbolType, KeywordType};

use crate::lex::{
    Token::{Symbol, Identifier, Keyword, Number, PGString},
    SymbolType::{Comma, RightParen, LeftParen, SemiColon, Asterisk},
    NumberType::{Integer},
    IdentifierType::{Symbol as SymbolIdentifier, DoubleQuote},
    KeywordType::{Int, Text, Create, Table, Select, From, As},
};

use nom::error::VerboseError;

#[derive(Debug, PartialEq, Eq)]
pub struct Ast {
    statements: Vec<Statement>
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Literal(Token),
    Expr(Box<Expression>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct ColumnDefinition {
    name: Token,
    data_type: Token,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    SelectStatement {
        item: Vec<Expression>,
        from: Option<Token>,
    },
    CreateStatement {
        name: Token,
        cols: Vec<ColumnDefinition>,
    },
    InsertStatement {
        table: Token,
        values: Expression,
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

        let item = self.parse_expressions(vec![Keyword(From), Symbol(SemiColon)])?;

        let mut from = None;
        if self.expect_keyword(From)? {
            self.cursor += 1;
            from = self.parse_token(|t| match t {
                Identifier(SymbolIdentifier, _) => true,
                _ => false
            })?;

            if from.is_none() {
                return Err(ParseError::ParsingError("Expected FROM token"))
            }
        }

        Ok(Statement::SelectStatement { item, from })
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
                Identifier(_, _) | PGString(_) | Number(_, _) | Symbol(_) | Keyword(As) => true,
                _ => false
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
            Identifier(SymbolIdentifier, _) => true,
            _ => false
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
                    name,
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
                Identifier(SymbolIdentifier, _) => true,
                _ => false,
            })?;

            let column_type = self.parse_token(|t| match t {
                Keyword(Int) | Keyword(Text) => true,
                _ => false,
            })?;

            match (column_name, column_type) {
                (Some(name), Some(data_type)) =>
                    column_definitions.push(ColumnDefinition {name, data_type}),
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

    fn parse_token<F>(
        &mut self,
        matcher: F
    ) -> Result<Option<Token>, ParseError<'a>>
    where F: Fn(&Token) -> bool {
        let token = self.peek_next_token()?;
        Ok(
            if matcher(&token) {
                let t = token.clone();
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

    #[test]
    fn test_create_table() {
        let def = Parser::new("CREATE TABLE a_table_name (column1 INT);").parse().unwrap();
        assert_eq!(
            def,
            Ast {
                statements: vec![
                    Statement::CreateStatement {
                        name: Identifier(SymbolIdentifier, "a_table_name".to_string()),
                        cols: vec![
                            ColumnDefinition {
                                name: Identifier(SymbolIdentifier, "column1".to_string()),
                                data_type: Keyword(Int)
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
                        name: Identifier(SymbolIdentifier, "table_name".to_string()),
                        cols: vec![
                            ColumnDefinition {
                                name: Identifier(SymbolIdentifier, "column1".to_string()),
                                data_type: Keyword(Int)
                            },
                            ColumnDefinition {
                                name: Identifier(SymbolIdentifier, "column2".to_string()),
                                data_type: Keyword(Text)
                            },
                            ColumnDefinition {
                                name: Identifier(SymbolIdentifier, "column3".to_string()),
                                data_type: Keyword(Int)
                            },
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
                        item: vec![
                            Expression::Literal(Number(Integer, "1".to_string()))
                        ],
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
                        from: Some(Identifier(SymbolIdentifier, "table_name".to_string())),
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
                        from: Some(Identifier(SymbolIdentifier, "table_name".to_string())),
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
                        from: Some(Identifier(SymbolIdentifier, "table_name".to_string())),
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
                        from: Some(Identifier(SymbolIdentifier, "table_name".to_string())),
                    }
                ],
            }
        );
    }
}
