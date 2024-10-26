use gc_arena::Mutation;
use value::{ConsCell, Value, ValuePtr, ValueVisitor, Vector};

pub mod any;
pub mod convert;
pub mod error;
pub mod fuel;
pub mod lambda;
pub mod port;
pub mod userstruct;
pub mod value;

pub struct FuelCosts;
impl FuelCosts {
    pub const CALL_COST: i32 = 10;
    // cost of loading a piece of data
    pub const LOAD_COST: i32 = 4;
    // cost of loading a var from environment
    pub const ENV_COST: i32 = 4;
}

/// Used to ensure that all "null cons" refer to the same pointer
/// so that `eq?` works between them. Any list that is stored as a value must
/// be processed through this.
pub struct EnsureNullVisitor<'a, 'gc> {
    pub mutation: &'a Mutation<'gc>,
    pub null: &'a Value<'gc>,
}

impl<'a, 'gc> ValueVisitor<'gc> for EnsureNullVisitor<'a, 'gc> {
    fn visit_cons(&mut self, cons: ConsCell<'gc>, value: ValuePtr<'gc>) {
        if cons.car.is_none() && cons.cdr.is_none() {
            *value.unlock(self.mutation).borrow_mut() = *self.null;
            return;
        }

        if let Some(car) = cons.car {
            self.visit_value(car);
        }

        if let Some(cdr) = cons.cdr {
            self.visit_value(cdr);
        }
    }

    fn visit_vector(&mut self, vec: Vector<'gc>, _value: ValuePtr<'gc>) {
        for elem in vec.vec.borrow().iter() {
            self.visit_value(*elem)
        }
    }
}
