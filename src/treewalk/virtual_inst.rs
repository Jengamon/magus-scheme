use core::fmt;

use gc_arena::{Collect, Gc, Mutation, RefLock};
use lasso::Rodeo;
use rowan::TextRange;

use crate::{
    bytecode::ChunkPtr,
    value::Bytevector,
    value::Vector,
    value::{ConsCell, Symbol, Value, ValuePtr},
    ContainsDatum, Datum, DatumVisitor, ExactReal, GAstNode, GAstToken, SchemeNumber,
};

use super::StackValue;

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub enum VirtualInstructionDatum<'gc> {
    Number(i64),
    Bool(bool),
    String(String),
    Character(char),
    Bytevector(Vec<u8>),
    Symbol(Symbol),
    EmptyList,
    List {
        head: Box<VirtualInstruction<'gc>>,
        body: Vec<VirtualInstruction<'gc>>,
        // If the list has a dot, this is the element after the dot
        dot: Option<Box<VirtualInstruction<'gc>>>,
    },
    Labeled {
        label: usize,
        instruction: Box<VirtualInstruction<'gc>>,
        // we can cache if an instruction is circular
        //
        // this is so that we don't have to determine whether it is over and over again
        // and can be a quick check to prevent the execution of circular lists
        // (we don't want to stop the ~meh~ but technically correct #0=(... not even gonna use the label ....), but
        // we *also* want to fully preserve circular structures b/c `quote` is a thing)
        is_circular: bool,
    },
}

/// This is the code chunk size used by the Treewalk.
pub type CodeChunkPtr<'gc> = ChunkPtr<'gc, 256, 256, 256>;
#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub enum VirtualInstructionPayload<'gc> {
    Datum {
        datum: VirtualInstructionDatum<'gc>,
        // If we tried to JIT this already, but it failed, this is
        // false, so don't try again.
        can_jit: Gc<'gc, RefLock<bool>>,
    },
    // Eventually, virtual instructions can be JITed, while the value would
    // be preserved for macros who use it as data
    Jit {
        chunk: CodeChunkPtr<'gc>,
        datum: VirtualInstructionDatum<'gc>,
        // reset if converted to data
        pc: usize,
    },
}

impl<'gc> VirtualInstructionPayload<'gc> {
    pub fn display<'a>(&'a self, interner: &'a Rodeo) -> VIPDisplay<'a, 'gc> {
        VIPDisplay {
            instruction: self,
            interner,
        }
    }

    #[inline]
    pub fn datum(&self) -> &VirtualInstructionDatum<'gc> {
        match self {
            Self::Datum { datum, .. } => datum,
            Self::Jit { datum, .. } => datum,
        }
    }

    pub fn as_symbol(&self) -> Option<Symbol> {
        match self {
            Self::Datum {
                datum: VirtualInstructionDatum::Symbol(sym),
                ..
            } => Some(*sym),
            Self::Jit {
                datum: VirtualInstructionDatum::Symbol(sym),
                ..
            } => Some(*sym),
            _ => None,
        }
    }
}

pub struct VIPDisplay<'a, 'gc> {
    interner: &'a Rodeo,
    instruction: &'a VirtualInstructionPayload<'gc>,
}

impl<'a, 'gc> fmt::Display for VIPDisplay<'a, 'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut datum_writer = |datum: &VirtualInstructionDatum<'gc>| match datum {
            VirtualInstructionDatum::Number(num) => write!(f, "{num}"),
            VirtualInstructionDatum::Bool(b) => write!(f, "{b}"),
            VirtualInstructionDatum::String(s) => write!(f, "{s}"),
            VirtualInstructionDatum::Character(c) => write!(f, "{c}"),
            VirtualInstructionDatum::Bytevector(bv) => write!(
                f,
                "#u8({})",
                bv.iter()
                    .map(|b| format!("#x{b:02x}"))
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            VirtualInstructionDatum::Symbol(s) => write!(f, "{}", self.interner.resolve(&s.0)),
            VirtualInstructionDatum::EmptyList => write!(f, "()"),
            VirtualInstructionDatum::List { head, body, dot } => {
                write!(f, "({}", head.payload.display(self.interner))?;
                for bitem in body {
                    write!(f, " {}", bitem.payload.display(self.interner))?;
                }

                if let Some(dot) = dot {
                    write!(f, " . {})", dot.payload.display(self.interner))
                } else {
                    write!(f, ")")
                }
            }
            VirtualInstructionDatum::Labeled { .. } => todo!(),
        };

        datum_writer(self.instruction.datum())
    }
}

// TODO This will replace Instruction
#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub struct VirtualInstruction<'gc> {
    pub payload: VirtualInstructionPayload<'gc>,
    // if this is directly from datum, store it.
    // this can also be synthesized (generally copied) by macros
    // TODO should we start storing this kind of information in values too?
    #[collect(require_static)]
    pub range: Option<TextRange>,

    /// when using this, we can track how many times this particular expression
    /// has been executed (this is kept through even as a stack value)
    pub touch_count: Gc<'gc, RefLock<usize>>,

    /// Optional source id designated when this instruction was created
    pub source_id: Option<usize>,
}

pub trait StackValueVisitor<'gc> {
    fn visit_value(&mut self, value: StackValue<'gc>) {
        match *value.borrow() {
            Value::Undefined => self.visit_undefined(value),
            Value::Void => self.visit_void(value),
            Value::Vector(vec) => self.visit_vector(vec, value),
            Value::Bytevector(vec) => self.visit_bytevector(vec, value),
            Value::Cons(cons) => self.visit_cons(cons, value),
            Value::Number(int) => self.visit_number(int, value),
            Value::Inexact(iex) => self.visit_inexact(iex, value),
            Value::String(str) => self.visit_string(str.as_ref().borrow().as_str(), value),
            Value::Symbol(sym) => self.visit_symbol(sym, value),
            Value::Bool(bool) => self.visit_bool(bool, value),
            Value::Char(char) => self.visit_char(char, value),
            _ => self.visit_no_external(value),
        }
    }

    fn visit_no_external(&mut self, value: StackValue<'gc>) {
        let _ = value;
    }

    fn visit_undefined(&mut self, value: StackValue<'gc>) {
        let _ = value;
    }

    fn visit_void(&mut self, value: StackValue<'gc>) {
        let _ = value;
    }

    fn visit_number(&mut self, integer: i64, value: StackValue<'gc>) {
        let _ = value;
        _ = integer;
    }

    fn visit_inexact(&mut self, integer: f64, value: StackValue<'gc>) {
        let _ = value;
        _ = integer;
    }

    fn visit_string(&mut self, string: &str, value: StackValue<'gc>) {
        let _ = value;
        _ = string;
    }

    fn visit_symbol(&mut self, symbol: Symbol, value: StackValue<'gc>) {
        let _ = value;
        _ = symbol;
    }

    fn visit_bool(&mut self, bool: bool, value: StackValue<'gc>) {
        let _ = value;
        _ = bool;
    }

    fn visit_char(&mut self, char: char, value: StackValue<'gc>) {
        let _ = value;
        _ = char;
    }

    fn visit_cons(&mut self, cons: ConsCell<'gc>, value: StackValue<'gc>) {
        let _ = value;
        _ = cons;
    }

    fn visit_vector(&mut self, vec: Vector<'gc>, value: StackValue<'gc>) {
        let _ = value;
        _ = vec;
    }

    fn visit_bytevector(&mut self, vec: Bytevector<'gc>, value: StackValue<'gc>) {
        let _ = value;
        _ = vec;
    }
}

struct ValueToVirtualInstructionDatum<'a, 'gc> {
    instruction: Option<VirtualInstructionDatum<'gc>>,
    found: Vec<StackValue<'gc>>,
    range: Option<TextRange>,
    chunk: Result<CodeChunkPtr<'gc>, Gc<'gc, RefLock<bool>>>,
    circular_list: bool,
    source_id: Option<usize>,
    mc: &'a Mutation<'gc>,
}

impl<'a, 'gc> ValueToVirtualInstructionDatum<'a, 'gc> {
    fn synthesize(&self, value: ValuePtr<'gc>) -> StackValue<'gc> {
        StackValue {
            value,
            range: self.range,
            touch_count: Gc::new(self.mc, RefLock::new(0)),
            source_id: self.source_id,
            chunk: self.chunk,
        }
    }
}

impl<'a, 'gc> StackValueVisitor<'gc> for ValueToVirtualInstructionDatum<'a, 'gc> {
    fn visit_number(&mut self, integer: i64, _value: StackValue<'gc>) {
        self.instruction = Some(VirtualInstructionDatum::Number(integer));
    }

    fn visit_symbol(&mut self, symbol: Symbol, _value: StackValue<'gc>) {
        self.instruction = Some(VirtualInstructionDatum::Symbol(symbol));
    }

    fn visit_cons(&mut self, cons: ConsCell<'gc>, value: StackValue<'gc>) {
        if self.found.iter().any(|ptr| Gc::ptr_eq(**ptr, *value))
            && self
                .instruction
                .as_ref()
                .is_some_and(|i| !matches!(i, VirtualInstructionDatum::EmptyList))
        {
            self.circular_list = true;
            return;
        }
        self.found.push(value);

        let car = if let Some(car) = cons.car.as_ref() {
            let fake_car = self.synthesize(*car);
            self.visit_value(fake_car);
            if self.circular_list {
                return;
            }
            let Some(inst) = self.instruction.take() else {
                return;
            };
            inst
        } else {
            VirtualInstructionDatum::EmptyList
        };

        let cdr = if let Some(cdr) = cons.cdr.as_ref() {
            let fake_cdr = self.synthesize(*cdr);
            self.visit_value(fake_cdr);
            if self.circular_list {
                return;
            }
            let Some(
                inst @ (VirtualInstructionDatum::List { dot: None, .. }
                | VirtualInstructionDatum::EmptyList),
            ) = self.instruction.take()
            else {
                return;
            };
            inst
        } else {
            VirtualInstructionDatum::EmptyList
        };

        if let (VirtualInstructionDatum::EmptyList, VirtualInstructionDatum::EmptyList) =
            (&car, &cdr)
        {
            self.instruction = Some(VirtualInstructionDatum::EmptyList);
        } else if let (car, VirtualInstructionDatum::EmptyList) = (&car, &cdr) {
            self.instruction = Some(VirtualInstructionDatum::List {
                head: Box::new(VirtualInstruction {
                    payload: VirtualInstructionPayload::Datum {
                        datum: car.clone(),
                        can_jit: Gc::new(self.mc, RefLock::new(true)),
                    },
                    range: self.range,
                    touch_count: Gc::new(self.mc, RefLock::new(0)),
                    source_id: self.source_id,
                }),
                body: Vec::new(),
                dot: None,
            });
        } else {
            // cdr has to be a list with no dot, so
            let VirtualInstructionDatum::List { head, body, .. } = cdr else {
                unreachable!("{cdr:?}")
            };

            self.instruction = Some(VirtualInstructionDatum::List {
                head: Box::new(VirtualInstruction {
                    payload: VirtualInstructionPayload::Datum {
                        datum: car,
                        can_jit: Gc::new(self.mc, RefLock::new(true)),
                    },
                    range: self.range,
                    touch_count: Gc::new(self.mc, RefLock::new(0)),
                    source_id: self.source_id,
                }),
                body: std::iter::once(*head).chain(body).collect(),
                dot: None,
            });
        }
    }
}

impl<'gc> TryFrom<(&'gc Mutation<'gc>, StackValue<'gc>)> for VirtualInstruction<'gc> {
    type Error = ();
    fn try_from((mc, value): (&'gc Mutation<'gc>, StackValue<'gc>)) -> Result<Self, Self::Error> {
        let mut visitor = ValueToVirtualInstructionDatum {
            source_id: value.source_id,
            range: value.range,
            instruction: None,
            found: Vec::new(),
            circular_list: false,
            chunk: value.chunk,
            mc,
        };
        visitor.visit_value(value);
        let datum = visitor.instruction.ok_or(())?;
        Ok(Self {
            payload: match value.chunk {
                Ok(chunk) => VirtualInstructionPayload::Jit {
                    chunk,
                    datum,
                    pc: 0,
                },
                Err(can_jit) => VirtualInstructionPayload::Datum { datum, can_jit },
            },
            range: value.range,
            touch_count: value.touch_count,
            source_id: value.source_id,
        })
    }
}

fn convert_vidatum_to_value<'gc>(
    mc: &gc_arena::Mutation<'gc>,
    null: ValuePtr<'gc>,
    datum: VirtualInstructionDatum,
) -> ValuePtr<'gc> {
    match datum {
        VirtualInstructionDatum::Number(num) => Value::Number(num).into_ptr(mc),
        VirtualInstructionDatum::Bool(b) => Value::Bool(b).into_ptr(mc),
        VirtualInstructionDatum::String(s) => {
            Value::String(Gc::new(mc, RefLock::new(s))).into_ptr(mc)
        }
        VirtualInstructionDatum::Character(c) => Value::Char(c).into_ptr(mc),
        VirtualInstructionDatum::Bytevector(bv) => {
            Value::Bytevector(Gc::new(mc, RefLock::new(bv)).into()).into_ptr(mc)
        }
        VirtualInstructionDatum::Symbol(s) => Value::Symbol(s).into_ptr(mc),
        VirtualInstructionDatum::EmptyList => null,
        VirtualInstructionDatum::List { head, body, dot } => {
            let cons = ConsCell::from_iter(
                mc,
                null,
                std::iter::once(convert_vidatum_to_value(
                    mc,
                    null,
                    head.payload.datum().clone(),
                ))
                .chain(
                    body.iter()
                        .map(|b| convert_vidatum_to_value(mc, null, b.payload.datum().clone())),
                )
                .chain(
                    dot.iter()
                        .map(|b| convert_vidatum_to_value(mc, null, b.payload.datum().clone())),
                ),
            );

            cons
        }
        VirtualInstructionDatum::Labeled { .. } => todo!(),
    }
}

impl<'gc> VirtualInstruction<'gc> {
    /// Convert a virtual instruction into data (stack value)
    pub fn into_value(self, mc: &gc_arena::Mutation<'gc>, null: ValuePtr<'gc>) -> StackValue<'gc> {
        match self.payload {
            VirtualInstructionPayload::Jit { chunk, datum, .. } => StackValue {
                value: convert_vidatum_to_value(mc, null, datum),
                range: self.range,
                source_id: self.source_id,
                touch_count: self.touch_count,
                chunk: Ok(chunk),
            },
            VirtualInstructionPayload::Datum { datum, can_jit } => StackValue {
                value: convert_vidatum_to_value(mc, null, datum),
                range: self.range,
                source_id: self.source_id,
                touch_count: self.touch_count,
                chunk: Err(can_jit),
            },
        }
    }
}

// TODO use DatumVisitor to convert from Datum to this

// Convert Datum into a VirtualInstruction
struct DatumToVirtualInstruction<'a, 'gc> {
    interner: &'a mut Rodeo,
    mc: &'a Mutation<'gc>,
    source_id: Option<usize>,
    instruction: Option<VirtualInstruction<'gc>>,
}

impl<'a, 'gc> DatumToVirtualInstruction<'a, 'gc> {
    fn set_instuction<D: GAstNode>(&mut self, node: &D, datum: VirtualInstructionDatum<'gc>) {
        self.instruction = Some(VirtualInstruction {
            payload: VirtualInstructionPayload::Datum {
                datum,
                can_jit: Gc::new(self.mc, RefLock::new(true)),
            },
            range: Some(node.syntax().text_range()),
            touch_count: Gc::new(self.mc, RefLock::new(0)),
            source_id: self.source_id,
        });
    }

    fn set_instuction_token<D: GAstToken>(
        &mut self,
        token: &D,
        datum: VirtualInstructionDatum<'gc>,
    ) {
        self.instruction = Some(VirtualInstruction {
            payload: VirtualInstructionPayload::Datum {
                datum,
                can_jit: Gc::new(self.mc, RefLock::new(true)),
            },
            range: Some(token.syntax().text_range()),
            touch_count: Gc::new(self.mc, RefLock::new(0)),
            source_id: self.source_id,
        });
    }
}

impl<'a, 'gc> DatumVisitor for DatumToVirtualInstruction<'a, 'gc> {
    fn visit_bool(&mut self, bool: &crate::Boolean) {
        if let Some(vbool) = bool.bool() {
            self.set_instuction_token(bool, VirtualInstructionDatum::Bool(vbool));
        }
    }

    fn visit_number(&mut self, number: &crate::Number) {
        if let Some(SchemeNumber::Exact(ExactReal::Integer { value, is_neg })) = number.number() {
            let Ok(num) = (if is_neg {
                value.try_into().map(|i: i64| -i)
            } else {
                value.try_into()
            }) else {
                return;
            };

            self.set_instuction_token(number, VirtualInstructionDatum::Number(num));
        }
    }

    fn visit_symbol(&mut self, symbol: &crate::Symbol) {
        if let Some(sym) = symbol.identifier(false) {
            let sym = self.interner.get_or_intern(sym);
            self.set_instuction_token(symbol, VirtualInstructionDatum::Symbol(Symbol(sym)));
        }
    }

    fn visit_list(&mut self, list: &crate::List) {
        let mut datum = list.datum();
        let Some(head) = datum.next() else {
            self.set_instuction(list, VirtualInstructionDatum::EmptyList);
            return;
        };
        self.visit_datum(&head);
        let Some(head_inst) = self.instruction.take() else {
            return;
        };

        let body = datum.collect::<Vec<_>>();
        let mut body_inst = Vec::with_capacity(body.len());
        for bdata in body {
            self.visit_datum(&bdata);
            let Some(body_elem) = self.instruction.take() else {
                return;
            };
            body_inst.push(body_elem);
        }

        if list.has_dot() {
            let Some(dot) = body_inst.pop() else {
                return;
            };

            self.set_instuction(
                list,
                VirtualInstructionDatum::List {
                    head: Box::new(head_inst),
                    body: body_inst,
                    dot: Some(Box::new(dot)),
                },
            );
        } else {
            self.set_instuction(
                list,
                VirtualInstructionDatum::List {
                    head: Box::new(head_inst),
                    body: body_inst,
                    dot: None,
                },
            );
        }
    }
}

// we don't need the Mutation because this *never* allocates Gc pointers
// (this is not responsible for producing JIT'ed chunks)
pub fn convert_to_virtual<'gc>(
    datum: Datum,
    interner: &mut Rodeo,
    mc: &Mutation<'gc>,
    source_id: Option<usize>,
) -> VirtualInstruction<'gc> {
    let mut converter = DatumToVirtualInstruction {
        interner,
        mc,
        source_id,
        instruction: None,
    };
    converter.visit_datum(&datum);
    converter.instruction.unwrap_or_else(|| {
        panic!(
            "failed to convert datum `{}` into instruction",
            datum.syntax().text()
        )
    })
}
