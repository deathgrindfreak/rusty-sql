extern crate nom;

use nom::{
    IResult,
    branch::alt,
    bytes::complete::{tag_no_case, tag, is_not},
    error::{VerboseError, context},
    sequence::{tuple, preceded},
    character::complete::{digit1, alphanumeric1, char, one_of, multispace0},
    combinator::opt,
    multi::{many1, separated_list1},
};

type Res<T, U> = IResult<T, U, VerboseError<T>>;

#[derive(Debug, PartialEq, Eq)]
pub enum NumberType {
    Integer,
    Float,
}

#[derive(Debug, PartialEq, Eq)]
pub enum KeywordType {
    Select,
    From,
    As,
    Table,
    Create,
    Insert,
    Into,
    Values,
    Int,
    Text
}

#[derive(Debug, PartialEq, Eq)]
pub enum SymbolType {
    SemiColon,
    Asterisk,
    Comma,
    LeftParen,
    RightParen,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    PGString(String),
    Number(NumberType, String),
    Identifier(String),
    Symbol(SymbolType),
    Keyword(KeywordType),
}

pub fn lex_sql(input: &str) -> Res<&str, Vec<Token>> {
    context(
        "sql",
        separated_list1(
            multispace0,
            alt((
                keyword,
                symbol,
                identifier,
                string,
                number
            ))
        )
    )(input)
}

impl From<&str> for KeywordType {
    fn from(i: &str) -> Self {
        match i.to_lowercase().as_str() {
            "select" => KeywordType::Select,
            "from" => KeywordType::From,
            "as" => KeywordType::As,
            "table" => KeywordType::Table,
            "create" => KeywordType::Create,
            "insert" => KeywordType::Insert,
            "into" => KeywordType::Into,
            "values" => KeywordType::Values,
            "int" => KeywordType::Int,
            "text" => KeywordType::Text,
            _ => unimplemented!("Not a keyword!")
        }
    }
}

fn keyword(input: &str) -> Res<&str, Token>{
    context(
        "keyword",
        alt((
            tag_no_case("select"),
            tag_no_case("from"),
            tag_no_case("as"),
            tag_no_case("table"),
            tag_no_case("create"),
            tag_no_case("insert"),
            tag_no_case("into"),
            tag_no_case("values"),
            tag_no_case("int"),
            tag_no_case("text")
        ))
    )(input).map(|(next_input, res)| (next_input, Token::Keyword(res.into())))
}

fn symbol(input: &str) -> Res<&str, Token> {
    context("symbol", one_of(";*,()"))(input)
        .map(|(next_input, res)| {
            (
                next_input,
                Token::Symbol(match res {
                    ';' => SymbolType::SemiColon,
                    '*' => SymbolType::Asterisk,
                    ',' => SymbolType::Comma,
                    '(' => SymbolType::LeftParen,
                    ')' => SymbolType::RightParen,
                    _ => unimplemented!("Bad symbol")
                })
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
        eprintln!("string_identifier: {:?}", res);
        (
            next_input,
            Token::Identifier(res.1.join(""))
        )
    })
}

fn symbol_identifier(input: &str) -> Res<&str, Token> {
    context(
        "symbol_identifier",
        separated_list1(char('_'), alphanumeric1)
    )(input).map(|(next_input, res)| {
        (
            next_input,
            Token::Identifier(res.join("_"))
        )
    })
}

fn identifier(input: &str) -> Res<&str, Token> {
    context("identifier", alt((string_identifier, symbol_identifier)))(input)
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
            Token::Number(
                NumberType::Float,
                match res {
                    (int, _, frac, Some((_, sign, e))) => {
                        vec![int.unwrap_or("0"), ".", frac, "e", sign.unwrap_or("+"), e]
                    },
                    (int, _, frac, None) => vec![int.unwrap_or("0"), ".", frac],
                }.join("")
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
            Token::Number(
                NumberType::Float,
                match res {
                    (int, _, frac, Some((_, sign, e))) => {
                        vec![int, ".", frac.unwrap_or("0"), "e", sign.unwrap_or("+"), e]
                    },
                    (int, _, frac, None) => vec![int, ".", frac.unwrap_or("0")],
                }.join("")
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
        (
            next_input,
            Token::Number(
                if s == "+" {
                    NumberType::Integer
                } else {
                    NumberType::Float
                },
                vec![int, "e", s, e].join("")
            )
        )
    })
}

fn integer(input: &str) -> Res<&str, Token> {
    context("integer", digit1)(input)
        .map(|(next_input, res)| (next_input,  Token::Number(NumberType::Integer, res.to_string())))
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
        Token::{PGString, Number, Identifier, Symbol, Keyword},
        SymbolType::{SemiColon, Asterisk, Comma, LeftParen, RightParen},
        NumberType::{Integer, Float},
        KeywordType::{Select, From, As, Table, Create, Insert, Into, Values, Int, Text}
    };

    use super::*;

    #[test]
    fn test_token() {
        assert_eq!(
            lex_sql("SELECT * FROM test_name;"),
            Ok((
                "",
                vec![
                    Keyword(Select),
                    Symbol(Asterisk),
                    Keyword(From),
                    Identifier("test_name".to_string()),
                    Symbol(SemiColon)
                ]
            ))
        );

        assert_eq!(
            lex_sql("insert into a_long_name values(\"a string!!!\", 123.456e+10);"),
            Ok((
                "",
                vec![
                    Keyword(Insert),
                    Keyword(Into),
                    Identifier("a_long_name".to_string()),
                    Keyword(Values),
                    Symbol(LeftParen),
                    PGString("a string!!!".to_string()),
                    Symbol(Comma),
                    Number(Float, "123.456e+10".to_string()),
                    Symbol(SemiColon)
                ]
            ))
        );

        assert_eq!(
            lex_sql("SELECT * FROM table_name;"),
            Ok((
                "",
                vec![
                    Keyword(Select),
                    Symbol(Asterisk),
                    Keyword(From),
                    Identifier("table_name".to_string()),
                    Symbol(SemiColon)
                ]
            ))
        )
    }

    #[test]
    fn test_keyword() {
        assert_eq!(keyword("SELECT FROM"), Ok((" FROM", Keyword(Select))));
        assert_eq!(keyword("TaBlE"), Ok(("", Keyword(Table))));
        assert_eq!(keyword("From"), Ok(("", Keyword(From))));
        assert_eq!(keyword("aS"), Ok(("", Keyword(As))));
        assert_eq!(keyword("Create"), Ok(("", Keyword(Create))));
        assert_eq!(keyword("INSERT"), Ok(("", Keyword(Insert))));
        assert_eq!(keyword("Into"), Ok(("", Keyword(Into))));
        assert_eq!(keyword("Values"), Ok(("", Keyword(Values))));
        assert_eq!(keyword("INT"), Ok(("", Keyword(Int))));
        assert_eq!(keyword("Text"), Ok(("", Keyword(Text))));


        // Keyword-like identifier
        assert_eq!(
            keyword("table_name"),
            Ok(("", Identifier("table_name".to_string())))
        );
        assert_eq!(
            keyword("select_from_this_cool_table"),
            Ok(("", Identifier("select_from_this_cool_table".to_string())))
        );
        assert_eq!(
            keyword("Not_a_keyword"),
            Err(NomErr::Error(VerboseError {
                errors: vec![
                    ("Not_a_keyword", VerboseErrorKind::Nom(ErrorKind::Tag)),
                    ("Not_a_keyword", VerboseErrorKind::Nom(ErrorKind::Alt)),
                    ("Not_a_keyword", VerboseErrorKind::Context("keyword")),
                ]
            }))
        );
    }

    #[test]
    fn test_symbol() {
        assert_eq!(symbol(";"), Ok(("", Symbol(SemiColon))));
        assert_eq!(symbol("*"), Ok(("", Symbol(Asterisk))));
        assert_eq!(symbol(","), Ok(("", Symbol(Comma))));
        assert_eq!(symbol("("), Ok(("", Symbol(LeftParen))));
        assert_eq!(symbol(")"), Ok(("", Symbol(RightParen))));
    }

    #[test]
    fn test_identifier() {
        assert_eq!(identifier("one"), Ok(("", Identifier("one".to_string()))));
        assert_eq!(
            identifier("here_is_an_identifier"),
            Ok(("", Identifier("here_is_an_identifier".to_string())))
        );
        assert_eq!(
            identifier("\"Here's a quoted identifier 1337!\""),
            Ok(("", Identifier("Here's a quoted identifier 1337!".to_string())))
        );
        assert_eq!(
            identifier("\"Here's some extra quotes \"\"quoted\"\"\""),
            Ok(("", Identifier("Here's some extra quotes \"quoted\"".to_string())))
        );
    }

    #[test]
    fn test_number() {
        assert_eq!(number("0"), Ok(("", Number(Integer, "0".to_string()))));
        assert_eq!(number("123"), Ok(("", Number(Integer, "123".to_string()))));

        // Optional fractional part
        assert_eq!(number("123.123"), Ok(("", Number(Float, "123.123".to_string()))));
        assert_eq!(number("24.12e-2"), Ok(("", Number(Float, "24.12e-2".to_string()))));
        assert_eq!(number("5.34e+2"), Ok(("", Number(Float, "5.34e+2".to_string()))));
        assert_eq!(number("5.e+2"), Ok(("", Number(Float, "5.0e+2".to_string()))));
        assert_eq!(number("5."), Ok(("", Number(Float, "5.0".to_string()))));
        assert_eq!(number("5.e1"), Ok(("", Number(Float, "5.0e+1".to_string()))));

        // Optional integer part
        assert_eq!(number(".123"), Ok(("", Number(Float, "0.123".to_string()))));
        assert_eq!(number(".12e-2"), Ok(("", Number(Float, "0.12e-2".to_string()))));
        assert_eq!(number(".34e+2"), Ok(("", Number(Float, "0.34e+2".to_string()))));
        assert_eq!(number(".1e2"), Ok(("", Number(Float, "0.1e+2".to_string()))));

        // Scientific notation
        assert_eq!(number("1e2"), Ok(("", Number(Integer, "1e+2".to_string()))));
        assert_eq!(number("1e-2"), Ok(("", Number(Float, "1e-2".to_string()))));
        assert_eq!(number("34e+2"), Ok(("", Number(Integer, "34e+2".to_string()))));

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
