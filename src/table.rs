use crate::error::BackendError;

use crate::parser::{
    Expression,
    Expression::{Literal, Binary},
};

use crate::lex::{
    KeywordType::{True, False, And, Or, As},
    SymbolType::{Equals, NotEquals, Concatenate, Plus},
    Token,
    Token::{Integer, PGString, Identifier, Keyword, Symbol},
    IdentifierType::{Symbol as SymbolIdentifier, DoubleQuote},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ColumnType {
    TextType,
    IntType,
    BoolType,
}

impl Into<ColumnType> for Token {
    fn into(self) -> ColumnType {
        match self {
            PGString(_) => ColumnType::TextType,
            Integer(_) => ColumnType::IntType,
            Keyword(True) | Keyword(False) => ColumnType::BoolType,
            _ => unimplemented!("Not a column type")
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Column {
    pub column_type: ColumnType,
    pub column_name: String,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct MemoryCell(Vec<u8>);

impl From<MemoryCell> for String {
    fn from(MemoryCell(mc): MemoryCell) -> String {
        String::from_utf8_lossy(&mc).to_string()
    }
}

impl Into<MemoryCell> for String {
    fn into(self) -> MemoryCell {
        MemoryCell(self.as_bytes().to_vec())
    }
}

impl From<MemoryCell> for i32 {
    fn from(MemoryCell(mc): MemoryCell) -> Self {
        i32::from_le_bytes(
            mc.try_into().unwrap_or_else(|v: Vec<u8>| {
                panic!("Expected Vec of length 4 but found length {}", v.len())
            })
        )
    }
}

impl Into<MemoryCell> for i32 {
    fn into(self) -> MemoryCell {
        MemoryCell(self.to_le_bytes().to_vec())
    }
}

impl From<MemoryCell> for bool {
    fn from(MemoryCell(mc): MemoryCell) -> Self {
        mc.len() == 1
    }
}

impl Into<MemoryCell> for bool {
    fn into(self) -> MemoryCell {
        MemoryCell(if self { vec![1] } else { vec![] })
    }
}

impl From<Token> for MemoryCell {
    fn from(token: Token) -> MemoryCell {
        match token {
            Integer(i) => i.clone().into(),
            PGString(s) => s.clone().into(),
            Keyword(k) => match k {
                True => MemoryCell(vec![1]),
                False => MemoryCell(vec![]),
                _ => unimplemented!("Not a memory cell type"),
            }
            _ => unimplemented!("Not a memory cell type"),
        }
    }
}

#[derive(Debug, Default)]
pub struct Table {
    pub columns: Vec<String>,
    pub column_types: Vec<ColumnType>,
    pub rows: Vec<Vec<MemoryCell>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Cell {
    TableCell(MemoryCell, String, ColumnType),
    ExprCell(MemoryCell, ColumnType),
    Alias(String)
}

impl Table {
    pub fn evaluate(&self, row_index: usize, exp: &Expression) -> Result<Cell, BackendError> {
        match exp {
            Literal(e) => {
                match e {
                    Identifier(DoubleQuote, id) => Ok(Cell::Alias(id.to_string())),
                    Identifier(SymbolIdentifier, id) => {
                        Ok(
                            match self.columns.iter().enumerate().find(|(_, col)| *col == id) {
                                Some((c, _)) => Cell::TableCell(
                                    self.rows[row_index][c].clone(),
                                    id.to_string(),
                                    self.column_types[c].clone()
                                ),
                                None => Cell::Alias(id.to_string())
                            }
                        )
                    },
                    _ => Ok(Cell::ExprCell(e.clone().into(), e.clone().into()))
                }
            },
            Binary { l, r, op } => {
                let l_c = self.evaluate(row_index, &l.clone())?;
                let r_c = self.evaluate(row_index, &r.clone())?;

                let true_cell: MemoryCell = Keyword(True).into();
                let false_cell: MemoryCell = Keyword(False).into();

                // Alias type should never be in the left operand
                let (l_cell, l_type) = match l_c {
                    Cell::TableCell(ref mc, _, ref mt) => (mc.clone(), mt.clone()),
                    Cell::ExprCell(ref mc, ref mt) => (mc.clone(), mt.clone()),
                    Cell::Alias(_) => return Err(BackendError::ErrInvalidOperands),
                };

                let (r_cell, r_type) = match r_c {
                    Cell::TableCell(mc, _, mt) => (mc, mt),
                    Cell::ExprCell(mc, mt) => (mc, mt),
                    Cell::Alias(alias_name) => return if let Keyword(As) = op {
                        Ok(Cell::TableCell(l_cell.clone().into(), alias_name, l_type.clone()))
                    } else {
                        Err(BackendError::ErrInvalidOperands)
                    }
                };

                match op {
                    Symbol(s) => match s {
                        Equals => {
                            let eq = l_cell == r_cell;
                            Ok(Cell::ExprCell(
                                match (l_type, r_type) {
                                    (ColumnType::TextType, ColumnType::TextType)
                                        | (ColumnType::IntType, ColumnType::IntType)
                                        | (ColumnType::BoolType, ColumnType::BoolType)
                                        if eq => true_cell,
                                    _ => false_cell
                                },
                                ColumnType::BoolType
                            ))
                        },
                        NotEquals => {
                            Ok(Cell::ExprCell(
                                if l_type != r_type || l != r {
                                    true_cell
                                } else {
                                    false_cell
                                },
                                ColumnType::BoolType
                            ))
                        },
                        Concatenate => {
                            if l_type != ColumnType::TextType || r_type != ColumnType::TextType {
                                return Err(BackendError::ErrInvalidOperands);
                            }

                            let mut l_string: String = l_cell.into();
                            let r_string: String = r_cell.into();
                            l_string.push_str(r_string.as_str());

                            Ok(Cell::ExprCell(l_string.into(), ColumnType::TextType))
                        },
                        Plus => {
                            if l_type != ColumnType::IntType || r_type != ColumnType::IntType {
                                return Err(BackendError::ErrInvalidOperands);
                            }

                            let l_int: i32 = l_cell.into();
                            let r_int: i32 = r_cell.into();

                            Ok(Cell::ExprCell((l_int + r_int).into(), ColumnType::IntType))
                        },
                        _ => todo!(),
                    },
                    Keyword(k) => {
                        match k {
                            And => {
                                if l_type != ColumnType::BoolType || r_type != ColumnType::BoolType {
                                    return Err(BackendError::ErrInvalidOperands);
                                }

                                let l_b: bool = l_cell.into();
                                let r_b: bool = r_cell.into();

                                Ok(Cell::ExprCell(
                                    if l_b && r_b { true_cell } else { false_cell },
                                    ColumnType::BoolType
                                ))
                            },
                            Or => {
                                if l_type != ColumnType::BoolType || r_type != ColumnType::BoolType {
                                    return Err(BackendError::ErrInvalidOperands);
                                }

                                let l_b: bool = l_cell.into();
                                let r_b: bool = r_cell.into();

                                Ok(Cell::ExprCell(
                                    if l_b || r_b { true_cell } else { false_cell },
                                    ColumnType::BoolType
                                ))
                            },
                            _ => todo!()
                        }
                    },
                    _ => todo!()
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use super::ColumnType::{IntType, TextType, BoolType};

    use crate::parser::Expression::{Binary, Literal};
    use crate::lex::{
        Token::{Integer, Symbol},
        SymbolType::Plus
    };

    #[test]
    fn test_table_evaluation() {
        let r = eval_table_expr(Binary {
            l: Box::new(Literal(Integer(1))),
            r: Box::new(Literal(Integer(1))),
            op: Symbol(Plus),
        });

        assert_eq!(r, Cell::ExprCell(2i32.into(), IntType));

        let r = eval_table_expr(Binary {
            l: Box::new(Literal(PGString("one".to_string()))),
            r: Box::new(Literal(PGString("two".to_string()))),
            op: Symbol(Concatenate),
        });

        assert_eq!(r, Cell::ExprCell("onetwo".to_string().into(), TextType));

        let r = eval_table_expr(Binary {
            l: Box::new(Literal(Keyword(True))),
            r: Box::new(Literal(Keyword(False))),
            op: Keyword(Or),
        });

        assert_eq!(r, Cell::ExprCell(true.into(), BoolType));

        let r = eval_table_expr(Binary {
            l: Box::new(Literal(Keyword(False))),
            r: Box::new(Literal(Keyword(True))),
            op: Keyword(And),
        });

        assert_eq!(r, Cell::ExprCell(false.into(), BoolType));
    }

    #[test]
    fn test_row_evaluation() {
        let e = Binary {
            l: Box::new(Literal(Identifier(SymbolIdentifier, "column1".to_string()))),
            r: Box::new(Literal(Keyword(True))),
            op: Symbol(Equals),
        };

        let r = Table {
            columns: vec!["column1".to_string()],
            column_types: vec![BoolType],
            rows: vec![
                vec![true.into()]
            ],
        }.evaluate(0, &e).unwrap();

        assert_eq!(r, Cell::ExprCell(true.into(), BoolType));

        let e = Binary {
            l: Box::new(Literal(Identifier(SymbolIdentifier, "column1".to_string()))),
            r: Box::new(
                Binary {
                    l: Box::new(Literal(Identifier(SymbolIdentifier, "column2".to_string()))),
                    r: Box::new(Literal(Integer(3))),
                    op: Symbol(Plus),
                }
            ),
            op: Symbol(Equals),
        };

        let r = Table {
            columns: vec!["column1".to_string(), "column2".to_string()],
            column_types: vec![IntType, IntType],
            rows: vec![
                vec![5.into(), 2.into()]
            ],
        }.evaluate(0, &e).unwrap();

        assert_eq!(r, Cell::ExprCell(true.into(), BoolType));
    }

    fn eval_table_expr(e: Expression) -> Cell {
        Table::default().evaluate(0, &e).unwrap()
    }
}
