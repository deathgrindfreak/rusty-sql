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
pub enum NumberType {
    Integer,
    Float,
}

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

    // Data types
    Int,
    Text
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SymbolType {
    SemiColon,
    Asterisk,
    Comma,
    LeftParen,
    RightParen,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum IdentifierType {
    DoubleQuote,
    Symbol,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    PGString(String),
    Number(NumberType, String),
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
        IdentifierType::{DoubleQuote, Symbol as IdentSymbol},
        KeywordType::{Select, From, As, Table, Create, Insert, Into, Values, Int, Text}
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
                    Number(Float, "123.456e+10".to_string()),
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
        assert_eq!(
            symbol("!"),
            Err(NomErr::Error(VerboseError {
                errors: vec![
                    ("!", VerboseErrorKind::Nom(ErrorKind::OneOf)),
                    ("!", VerboseErrorKind::Context("symbol")),
                ]
            }))
        );
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
