use core::fmt;

use gc_arena::{Collect, Gc, RefLock};
use lasso::Rodeo;
use rowan::TextRange;

use crate::{
    bytecode::ChunkPtr,
    environment::EnvironmentPtr,
    runtime::EnsureNullVisitor,
    value::{ConsCell, Symbol, Value, ValuePtr, ValueVisitor},
    ContainsDatum, Datum, DatumVisitor, ExactReal, GAstNode, GAstToken, SchemeNumber,
};

use super::StackValue;

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
#[expect(dead_code)]
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
pub type CodeChunkPtr<'gc> = ChunkPtr<'gc, 256, 256>;
#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub enum VirtualInstructionPayload<'gc> {
    Datum {
        datum: VirtualInstructionDatum<'gc>,
        // If we tried to JIT this already, but it failed, this is
        // false, so don't try again.
        can_jit: bool,
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
    pub touch_count: usize,

    // A macro may request code be run in a certain environment, this
    // is the requested environment.
    pub environment: Option<EnvironmentPtr<'gc>>,
}

struct ValueToVirtualInstructionDatum<'gc> {
    instruction: Option<VirtualInstructionDatum<'gc>>,
}

impl<'gc> ValueVisitor<'gc> for ValueToVirtualInstructionDatum<'gc> {}

impl<'gc> TryFrom<StackValue<'gc>> for VirtualInstruction<'gc> {
    type Error = ();
    fn try_from(value: StackValue<'gc>) -> Result<Self, Self::Error> {
        let mut visitor = ValueToVirtualInstructionDatum { instruction: None };
        visitor.visit_value(*value);
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
            // ASK check if the results from this match up with expectactions,
            // we can also wipe the environment if necessary
            environment: value.environment,
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
            let cons = Value::Cons(ConsCell::from_iter(
                mc,
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
            ))
            .into_ptr(mc);

            let mut ensure_null = EnsureNullVisitor {
                mutation: mc,
                null: &null.borrow(),
            };
            ensure_null.visit_value(cons);
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
                touch_count: self.touch_count,
                chunk: Ok(chunk),
                environment: self.environment,
            },
            VirtualInstructionPayload::Datum { datum, can_jit } => StackValue {
                value: convert_vidatum_to_value(mc, null, datum),
                range: self.range,
                touch_count: self.touch_count,
                chunk: Err(can_jit),
                environment: self.environment,
            },
        }
    }
}

// TODO use DatumVisitor to convert from Datum to this

// Convert Datum into a VirtualInstruction
struct DatumToVirtualInstruction<'a, 'gc> {
    interner: &'a mut Rodeo,
    instruction: Option<VirtualInstruction<'gc>>,
}

impl<'a, 'gc> DatumToVirtualInstruction<'a, 'gc> {
    fn set_instuction<D: GAstNode>(&mut self, node: &D, datum: VirtualInstructionDatum<'gc>) {
        self.instruction = Some(VirtualInstruction {
            payload: VirtualInstructionPayload::Datum {
                datum,
                can_jit: true,
            },
            range: Some(node.syntax().text_range()),
            touch_count: 0,
            environment: None,
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
                can_jit: true,
            },
            range: Some(token.syntax().text_range()),
            touch_count: 0,
            environment: None,
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

            self.instruction = Some(VirtualInstruction {
                payload: VirtualInstructionPayload::Datum {
                    datum: VirtualInstructionDatum::Number(num),
                    can_jit: true,
                },
                range: Some(number.syntax().text_range()),
                touch_count: 0,
                environment: None,
            });
        }
    }

    fn visit_symbol(&mut self, symbol: &crate::Symbol) {
        if let Some(sym) = symbol.identifier(false) {
            let sym = self.interner.get_or_intern(sym);
            self.instruction = Some(VirtualInstruction {
                payload: VirtualInstructionPayload::Datum {
                    datum: VirtualInstructionDatum::Symbol(Symbol(sym)),
                    can_jit: true,
                },
                range: Some(symbol.syntax().text_range()),
                touch_count: 0,
                environment: None,
            });
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
pub fn convert_to_virtual<'gc>(datum: Datum, interner: &mut Rodeo) -> VirtualInstruction<'gc> {
    let mut converter = DatumToVirtualInstruction {
        interner,
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
