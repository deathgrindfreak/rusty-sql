use std::fmt;
use nom::{
    IResult,
    branch::alt,
    bytes::complete::{tag, is_not},
    error::{VerboseError, context},
    sequence::{tuple, preceded},
    character::complete::{digit1, char, one_of, multispace0, alpha1, alphanumeric0},
    combinator::{opt, eof},
    multi::{many1, separated_list1},
};

pub type Res<T, U> = IResult<T, U, VerboseError<T>>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum KeywordType {
    Select,
    From,
    As,
    Table,
    Create,
    Insert,
    Into,
    Values,
    Or,
    True,
    False,

    // Data types
    Int,
    Text
}

impl fmt::Display for KeywordType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let err_msg = match self {
            KeywordType::Select => "SELECT",
            KeywordType::From => "FROM",
            KeywordType::As => "AS",
            KeywordType::Table => "TABLE",
            KeywordType::Create => "CREATE",
            KeywordType::Insert => "INSERT",
            KeywordType::Into => "INTO",
            KeywordType::Values => "VALUES",
            KeywordType::Or => "OR",
            KeywordType::True => "TRUE",
            KeywordType::False => "FALSE",
            KeywordType::Int => "INT",
            KeywordType::Text => "TEXT",
        };
        write!(f, "{}", err_msg)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SymbolType {
    SemiColon,
    Asterisk,
    Comma,
    LeftParen,
    RightParen,
    Equals,
    NotEquals,
    Concatenate,
    Plus,
}

impl fmt::Display for SymbolType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let err_msg = match self {
            SymbolType::SemiColon => ";",
            SymbolType::Asterisk => "*",
            SymbolType::Comma => ",",
            SymbolType::LeftParen => "(",
            SymbolType::RightParen => ")",
            SymbolType::Equals => "=",
            SymbolType::NotEquals => "<>",
            SymbolType::Concatenate => "||",
            SymbolType::Plus => "+",
        };
        write!(f, "{}", err_msg)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum IdentifierType {
    DoubleQuote,
    Symbol,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    PGString(String),
    Integer(i32),
    Float(f64),
    Identifier(IdentifierType, String),
    Symbol(SymbolType),
    Keyword(KeywordType),
}

pub fn lex(input: &str) -> Res<&str, Vec<Token>> {
    context(
        "sql",
        tuple((
            many1(
                preceded(
                    multispace0,
                    alt((symbol, identifier, string, number))
                )
            ),
            preceded(multispace0, eof)
        ))
    )(input).map(|(next_input, res)| (next_input, res.0))
}

fn single_char_symbol(input: &str) -> Res<&str, Token> {
    context("single_char_symbol", one_of(";*,()=+"))
        (input).map(|(next_input, res)| {
            (
                next_input,
                Token::Symbol(match res {
                    ';' => SymbolType::SemiColon,
                    '*' => SymbolType::Asterisk,
                    ',' => SymbolType::Comma,
                    '(' => SymbolType::LeftParen,
                    ')' => SymbolType::RightParen,
                    '=' => SymbolType::Equals,
                    '+' => SymbolType::Plus,
                    _ => unimplemented!("Bad symbol")
                })
            )
        })
}

fn multi_char_symbol(input: &str) -> Res<&str, Token> {
    context("multi_char_symbol", alt((tag("<>"), tag("||")))
    )(input).map(|(next_input, res)| {
            (
                next_input,
                Token::Symbol(match res {
                    "<>" => SymbolType::NotEquals,
                    "||" => SymbolType::Concatenate,
                    _ => unimplemented!("Bad symbol")
                })
            )
        })
}

fn symbol(input: &str) -> Res<&str, Token> {
    context("symbol", alt((single_char_symbol, multi_char_symbol)))(input)
}

fn symbol_identifier(input: &str) -> Res<&str, Token> {
    context(
        "symbol_identifier",
        tuple((
            alpha1,
            opt(separated_list1(char('_'), alphanumeric0))
        ))
    )(input).map(|(next_input, (c, res))| {
        let mut r = c.to_string();
        if let Some(v) = res {
            r.push_str(v.join("_").as_str());
        }

        (
            next_input,
            match r.to_lowercase().as_str() {
                "select" => Token::Keyword(KeywordType::Select),
                "from" => Token::Keyword(KeywordType::From),
                "as" => Token::Keyword(KeywordType::As),
                "table" => Token::Keyword(KeywordType::Table),
                "create" => Token::Keyword(KeywordType::Create),
                "insert" => Token::Keyword(KeywordType::Insert),
                "into" => Token::Keyword(KeywordType::Into),
                "values" => Token::Keyword(KeywordType::Values),
                "int" => Token::Keyword(KeywordType::Int),
                "text" => Token::Keyword(KeywordType::Text),
                "or" => Token::Keyword(KeywordType::Or),
                "true" => Token::Keyword(KeywordType::True),
                "false" => Token::Keyword(KeywordType::False),
                _ => Token::Identifier(IdentifierType::Symbol, r)
            }
        )
    })
}

fn string_identifier(input: &str) -> Res<&str, Token> {
    context(
        "string_identifier",
        tuple((
            tag("\""),
            many1(
                alt((
                    preceded(tag("\""), tag("\"")),
                    is_not("\"")
                )),
            ),
            tag("\"")
        ))
    )(input).map(|(next_input, res)| {
        (
            next_input,
            Token::Identifier(IdentifierType::DoubleQuote, res.1.join(""))
        )
    })
}

fn identifier(input: &str) -> Res<&str, Token> {
    context("identifier", alt((symbol_identifier, string_identifier)))(input)
}

fn float_optional_int(input: &str) -> Res<&str, Token> {
    context(
        "float_optional_int",
        tuple((
            opt(digit1),
            tag("."),
            digit1,
            opt(
                tuple((
                    tag("e"),
                    opt(alt((tag("-"), tag("+")))),
                    digit1
                ))
            ),
        ))
    )(input).map(|(next_input, res)| {
        (
            next_input,
            Token::Float(
                match res {
                    (int, _, frac, Some((_, sign, e))) => {
                        vec![int.unwrap_or("0"), ".", frac, "e", sign.unwrap_or("+"), e]
                    },
                    (int, _, frac, None) => vec![int.unwrap_or("0"), ".", frac],
                }.join("").parse().unwrap()
            )
        )
    })
}

fn float_optional_frac(input: &str) -> Res<&str, Token> {
    context(
        "float_optional_frac",
        tuple((
            digit1,
            tag("."),
            opt(digit1),
            opt(
                tuple((
                    tag("e"),
                    opt(alt((tag("-"), tag("+")))),
                    digit1
                ))
            ),
        ))
    )(input).map(|(next_input, res)| {
        (
            next_input,
            Token::Float(
                match res {
                    (int, _, frac, Some((_, sign, e))) => {
                        vec![int, ".", frac.unwrap_or("0"), "e", sign.unwrap_or("+"), e]
                    },
                    (int, _, frac, None) => vec![int, ".", frac.unwrap_or("0")],
                }.join("").parse().unwrap()
            )
        )
    })
}

fn scientific(input: &str) -> Res<&str, Token> {
    context(
        "scientific",
        tuple((
            digit1,
            tag("e"),
            opt(alt((tag("-"), tag("+")))),
            digit1
        ))
    )(input).map(|(next_input, res)| {
        let (int, _, sign, e) = res;
        let s = sign.unwrap_or("+");
        let n = vec![int, "e", s, e].join("");
        eprintln!("{:?}", n);
        (
            next_input,
            if s == "+" {
                // Rust won't parse as integers, so we have to cast
                Token::Integer(n.parse::<f64>().unwrap() as i32)
            } else {
                Token::Float(n.parse().unwrap())
            }
        )
    })
}

fn integer(input: &str) -> Res<&str, Token> {
    context("integer", digit1)(input)
        .map(|(next_input, res)| (next_input,  Token::Integer(res.parse().unwrap())))
}

fn number(input: &str) -> Res<&str, Token> {
    context(
        "number",
        alt((
            float_optional_frac,
            float_optional_int,
            scientific,
            integer,
        ))
    )(input)
}

fn string(input: &str) -> Res<&str, Token> {
    context(
        "string",
        tuple((
            tag("'"),
            many1(
                alt((
                    preceded(tag("'"), tag("'")),
                    is_not("'")
                )),
            ),
            tag("'")
        ))
    )(input).map(|(next_input, res)| {
        let (_, s, _) = res;
        (next_input,  Token::PGString(s.join("")))
    })
}

#[cfg(test)]
mod test {
    use nom::{
        error::{VerboseErrorKind, ErrorKind},
        Err as NomErr,
    };

    use super::{
        Token::{PGString, Integer, Float, Identifier, Symbol, Keyword},
        SymbolType::{SemiColon, Asterisk, Comma, LeftParen, RightParen, Equals, Plus, NotEquals, Concatenate},
        IdentifierType::{DoubleQuote, Symbol as IdentSymbol},
        KeywordType::{Select, From, As, Table, Create, Insert, Into, Values, Int, Text, Or, True, False}
    };

    use super::*;

    #[test]
    fn test_token() {
        // Should read entire input
        assert_eq!(
            lex("SELECT !"),
            Err(NomErr::Error(VerboseError {
                errors: vec![
                    ("!", VerboseErrorKind::Nom(ErrorKind::Eof)),
                    ("SELECT !", VerboseErrorKind::Context("sql"))
                ]
            }))
        );

        assert_eq!(lex("SELECT     "), Ok(("", vec![Keyword(Select)])));
        assert_eq!(lex("SELECT"), Ok(("", vec![Keyword(Select)])));

        assert_eq!(
            lex("SELECT;"),
            Ok(("", vec![Keyword(Select), Symbol(SemiColon)]))
        );

        assert_eq!(
            lex("SELECT * FROM table_name;"),
            Ok((
                "",
                vec![
                    Keyword(Select),
                    Symbol(Asterisk),
                    Keyword(From),
                    Identifier(IdentSymbol, "table_name".to_string()),
                    Symbol(SemiColon)
                ]
            ))
        );

        assert_eq!(
            lex("SELECT
    column1 AS \"column_numero_uno\",
    column2 AS \"column_deux\"
FROM test_table_name;"),
            Ok((
                "",
                vec![
                    Keyword(Select),
                    Identifier(IdentSymbol, "column1".to_string()),
                    Keyword(As),
                    Identifier(DoubleQuote, "column_numero_uno".to_string()),
                    Symbol(Comma),
                    Identifier(IdentSymbol, "column2".to_string()),
                    Keyword(As),
                    Identifier(DoubleQuote, "column_deux".to_string()),
                    Keyword(From),
                    Identifier(IdentSymbol, "test_table_name".to_string()),
                    Symbol(SemiColon)
                ]
            ))
        );

        assert_eq!(
            lex("insert into a_long_name values('a string!!!', 123.456e+10);"),
            Ok((
                "",
                vec![
                    Keyword(Insert),
                    Keyword(Into),
                    Identifier(IdentSymbol, "a_long_name".to_string()),
                    Keyword(Values),
                    Symbol(LeftParen),
                    PGString("a string!!!".to_string()),
                    Symbol(Comma),
                    Float("123.456e+10".parse().unwrap()),
                    Symbol(RightParen),
                    Symbol(SemiColon)
                ]
            ))
        );

        assert_eq!(
            lex("SELECT * FROM table_name;"),
            Ok((
                "",
                vec![
                    Keyword(Select),
                    Symbol(Asterisk),
                    Keyword(From),
                    Identifier(IdentSymbol, "table_name".to_string()),
                    Symbol(SemiColon)
                ]
            ))
        )
    }

    #[test]
    fn test_keyword_identifier() {
        assert_eq!(identifier("SELECT FROM"), Ok((" FROM", Keyword(Select))));
        assert_eq!(identifier("TaBlE"), Ok(("", Keyword(Table))));
        assert_eq!(identifier("From"), Ok(("", Keyword(From))));
        assert_eq!(identifier("aS"), Ok(("", Keyword(As))));
        assert_eq!(identifier("Create"), Ok(("", Keyword(Create))));
        assert_eq!(identifier("INSERT"), Ok(("", Keyword(Insert))));
        assert_eq!(identifier("Into"), Ok(("", Keyword(Into))));
        assert_eq!(identifier("Values"), Ok(("", Keyword(Values))));
        assert_eq!(identifier("INT"), Ok(("", Keyword(Int))));
        assert_eq!(identifier("Text"), Ok(("", Keyword(Text))));
        assert_eq!(identifier("Or"), Ok(("", Keyword(Or))));
        assert_eq!(identifier("true"), Ok(("", Keyword(True))));
        assert_eq!(identifier("false"), Ok(("", Keyword(False))));


        // Symbol-like identifier
        assert_eq!(
            identifier("table_name"),
            Ok(("", Identifier(IdentSymbol, "table_name".to_string())))
        );
        assert_eq!(
            identifier("select_from_this_cool_table"),
            Ok(("", Identifier(IdentSymbol, "select_from_this_cool_table".to_string())))
        );
    }

    #[test]
    fn test_symbol() {
        assert_eq!(symbol(";"), Ok(("", Symbol(SemiColon))));
        assert_eq!(symbol("*"), Ok(("", Symbol(Asterisk))));
        assert_eq!(symbol(","), Ok(("", Symbol(Comma))));
        assert_eq!(symbol("("), Ok(("", Symbol(LeftParen))));
        assert_eq!(symbol(")"), Ok(("", Symbol(RightParen))));
        assert_eq!(symbol("="), Ok(("", Symbol(Equals))));
        assert_eq!(symbol("+"), Ok(("", Symbol(Plus))));
        assert_eq!(symbol("<>"), Ok(("", Symbol(NotEquals))));
        assert_eq!(symbol("||"), Ok(("", Symbol(Concatenate))));
        assert_eq!(
            symbol("!"),
            Err(NomErr::Error(VerboseError {
                errors: vec![
                    ("!", VerboseErrorKind::Nom(ErrorKind::Tag)),
                    ("!", VerboseErrorKind::Nom(ErrorKind::Alt)),
                    ("!", VerboseErrorKind::Context("multi_char_symbol")),
                    ("!", VerboseErrorKind::Nom(ErrorKind::Alt)),
                    ("!", VerboseErrorKind::Context("symbol"))
                ]
            })));
    }

    #[test]
    fn test_identifier() {
        assert_eq!(identifier("a"), Ok(("", Identifier(IdentSymbol, "a".to_string()))));
        assert_eq!(identifier("one"), Ok(("", Identifier(IdentSymbol, "one".to_string()))));
        assert_eq!(identifier("a_123_bcd"), Ok(("", Identifier(IdentSymbol, "a_123_bcd".to_string()))));
        assert_eq!(identifier("a123"), Ok(("", Identifier(IdentSymbol, "a123".to_string()))));
        assert_eq!(identifier("a123_bcd"), Ok(("", Identifier(IdentSymbol, "a123_bcd".to_string()))));
        assert_eq!(
            identifier("here_is_an_identifier"),
            Ok(("", Identifier(IdentSymbol, "here_is_an_identifier".to_string())))
        );

        // String identifiers
        assert_eq!(
            identifier("\"Here's a quoted identifier 1337!\""),
            Ok(("", Identifier(DoubleQuote, "Here's a quoted identifier 1337!".to_string())))
        );
        assert_eq!(
            identifier("\"Here's some extra quotes \"\"quoted\"\"\""),
            Ok(("", Identifier(DoubleQuote, "Here's some extra quotes \"quoted\"".to_string())))
        );

        // Can't start with number
        assert_eq!(
            identifier("123"),
            Err(NomErr::Error(VerboseError {
                errors: vec![
                    ("123", VerboseErrorKind::Nom(ErrorKind::Tag)),
                    ("123", VerboseErrorKind::Context("string_identifier")),
                    ("123", VerboseErrorKind::Nom(ErrorKind::Alt)),
                    ("123", VerboseErrorKind::Context("identifier")),
                ]
            }))
        );
    }

    #[test]
    fn test_number() {
        assert_eq!(number("0"), Ok(("", Integer(0))));
        assert_eq!(number("123"), Ok(("", Integer(123))));

        // Optional fractional part
        assert_eq!(number("123.123"), Ok(("", Float(123.123))));
        assert_eq!(number("24.12e-2"), Ok(("", Float(24.12e-2))));
        assert_eq!(number("5.34e+2"), Ok(("", Float(5.34e+2))));
        assert_eq!(number("5.e+2"), Ok(("", Float(5.0e+2))));
        assert_eq!(number("5."), Ok(("", Float(5.0))));
        assert_eq!(number("5.e1"), Ok(("", Float(5.0e+1))));

        // Optional integer part
        assert_eq!(number(".123"), Ok(("", Float(0.123))));
        assert_eq!(number(".12e-2"), Ok(("", Float(0.12e-2))));
        assert_eq!(number(".34e+2"), Ok(("", Float(0.34e+2))));
        assert_eq!(number(".1e2"), Ok(("", Float(0.1e+2))));

        // Scientific notation
        assert_eq!(number("1e2"), Ok(("", Integer(100))));
        assert_eq!(number("1e-2"), Ok(("", Float(1e-2))));
        assert_eq!(number("34e+2"), Ok(("", Integer(3400))));

        // Not a number
        assert_eq!(
            number("aabd"),
            Err(NomErr::Error(VerboseError {
                errors: vec![
                    ("aabd", VerboseErrorKind::Nom(ErrorKind::Digit)),
                    ("aabd", VerboseErrorKind::Context("integer")),
                    ("aabd", VerboseErrorKind::Nom(ErrorKind::Alt)),
                    ("aabd", VerboseErrorKind::Context("number")),
                ]
            }))
        );
    }

    #[test]
    fn test_string() {
        assert_eq!(string("'123'"), Ok(("", PGString("123".to_string()))));
        assert_eq!(
            string("'Here''s a string with a quote'"),
            Ok(("", PGString("Here's a string with a quote".to_string())))
        );
        assert_eq!(string("'A string!'"), Ok(("", PGString("A string!".to_string()))));
    }
}
