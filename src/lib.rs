extern crate nom;

use nom::{
    IResult,
    branch::alt,
    bytes::complete::{tag_no_case, tag},
    error::{VerboseError, context},
    sequence::tuple,
    character::complete::{digit1, digit0},
    combinator::opt,
};

type Res<T, U> = IResult<T, U, VerboseError<T>>;

#[derive(Debug, PartialEq, Eq)]
enum Keyword {
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

impl From<&str> for Keyword {
    fn from(i: &str) -> Self {
        match i.to_lowercase().as_str() {
            "select" => Keyword::Select,
            "from" => Keyword::From,
            "as" => Keyword::As,
            "table" => Keyword::Table,
            "create" => Keyword::Create,
            "insert" => Keyword::Insert,
            "into" => Keyword::Into,
            "values" => Keyword::Values,
            "int" => Keyword::Int,
            "text" => Keyword::Text,
            _ => unimplemented!("Not a keyword!")
        }
    }
}

fn keyword(input: &str) -> Res<&str, Keyword>{
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
    )(input)
        .map(|(next_input, res)| (next_input, res.into()))
}

#[derive(Debug, PartialEq, Eq)]
enum Numeric {
    Int(String),
    Scientific(String),
    OptInt(String),
    OptFrac(String),
}

fn float_optional_int(input: &str) -> Res<&str, Numeric> {
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
            Numeric::OptInt(
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

fn float_optional_frac(input: &str) -> Res<&str, Numeric> {
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
            Numeric::OptFrac(
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

fn scientific(input: &str) -> Res<&str, Numeric> {
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
        (
            next_input,
            Numeric::Scientific(vec![int, "e", sign.unwrap_or("+"), e].join(""))
        )
    })
}

fn integer(input: &str) -> Res<&str, Numeric> {
    context("integer", digit1)(input)
        .map(|(next_input, res)| (next_input,  Numeric::Int(res.to_string())))
}

fn number(input: &str) -> Res<&str, Numeric> {
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

#[cfg(test)]
mod test {
    use nom::{
        error::{VerboseErrorKind, ErrorKind},
        Err as NomErr,
    };
    use super::Numeric::{
        OptFrac,
        OptInt,
        Int,
        Scientific,
    };

    use super::*;

    #[test]
    fn test_keyword() {
        assert_eq!(keyword("SELECT FROM"), Ok((" FROM", Keyword::Select)));
        assert_eq!(keyword("TaBlE"), Ok(("", Keyword::Table)));
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
    fn test_number() {
        assert_eq!(number("0"), Ok(("", Int("0".to_string()))));
        assert_eq!(number("123"), Ok(("", Int("123".to_string()))));

        // Optional fractional part
        assert_eq!(number("123.123"), Ok(("", OptFrac("123.123".to_string()))));
        assert_eq!(number("24.12e-2"), Ok(("", OptFrac("24.12e-2".to_string()))));
        assert_eq!(number("5.34e+2"), Ok(("", OptFrac("5.34e+2".to_string()))));
        assert_eq!(number("5.e+2"), Ok(("", OptFrac("5.0e+2".to_string()))));
        assert_eq!(number("5."), Ok(("", OptFrac("5.0".to_string()))));
        assert_eq!(number("5.e1"), Ok(("", OptFrac("5.0e+1".to_string()))));

        // Optional integer part
        assert_eq!(number(".123"), Ok(("", OptInt("0.123".to_string()))));
        assert_eq!(number(".12e-2"), Ok(("", OptInt("0.12e-2".to_string()))));
        assert_eq!(number(".34e+2"), Ok(("", OptInt("0.34e+2".to_string()))));
        assert_eq!(number(".1e2"), Ok(("", OptInt("0.1e+2".to_string()))));

        // Scientific notation
        assert_eq!(number("1e2"), Ok(("", Scientific("1e+2".to_string()))));
        assert_eq!(number("1e-2"), Ok(("", Scientific("1e-2".to_string()))));
        assert_eq!(number("34e+2"), Ok(("", Scientific("34e+2".to_string()))));
    }
}
