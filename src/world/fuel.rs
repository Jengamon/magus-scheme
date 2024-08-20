//! Fuel for the machine

#[derive(Debug, Clone)]
pub struct Fuel {
    fuel: i32,
    interrupted: bool,
}

// Lots of references to `Executor` cuz this is straight lifted from piccolo
impl Fuel {
    pub fn empty() -> Self {
        Self {
            fuel: 0,
            interrupted: false,
        }
    }

    pub fn with(fuel: i32) -> Self {
        Self {
            fuel,
            interrupted: false,
        }
    }

    /// Refills fuel up to a given maximum and also clears the fuel interrupt flag.
    ///
    /// This is a convenience method that is intended to be called outside of a call to
    /// `Executor::step` in some loop. It does the operations that all loops that would *re-use* a
    /// fuel container from one tick to another would need to do.
    ///
    /// It credits the running thread with fuel while also preventing available fuel from growing to
    /// an unbounded level, and also clears the interrupt flag since we are now on the "outside" of
    /// whatever lua code is being run and this is where we would want to interrupt to.
    pub fn refill(&mut self, fuel: i32, max_fuel: i32) {
        self.fuel = self.fuel.saturating_add(fuel).min(max_fuel);
        self.interrupted = false;
    }

    /// Add to or subtract from the current remaining fuel.
    pub fn adjust(&mut self, fuel: i32) {
        self.fuel = self.fuel.saturating_add(fuel);
    }

    /// Subtract from the current remaining fuel.
    ///
    /// This is a convenience method that is equivalent to `self.adjust_fuel(-fuel)`.
    pub fn consume(&mut self, fuel: i32) {
        self.adjust(fuel.saturating_neg());
    }

    pub fn remaining(&self) -> i32 {
        self.fuel
    }

    pub fn set_remaining(&mut self, fuel: i32) {
        self.fuel = fuel;
    }

    /// Marks that the calling `Executor` should immediately stop executing, without actually
    /// consuming any fuel.
    ///
    /// This is useful in situations where you need to interrupt a running `Executor` because it
    /// may be waiting on an external event that has not yet occurred, but still want to record the
    /// correct fuel usage.
    pub fn interrupt(&mut self) {
        self.interrupted = true;
    }

    pub fn is_interrupted(&self) -> bool {
        self.interrupted
    }

    pub fn clear_interrupt(&mut self) {
        self.interrupted = false;
    }

    /// Returns true if we have positive fuel remaining *and* we have not been interrupted.
    pub fn should_continue(&self) -> bool {
        self.fuel > 0 && !self.interrupted
    }
}

pub(crate) fn count_fuel(per_item: i32, count: usize) -> i32 {
    i32::try_from(count)
        .unwrap_or(i32::MAX)
        .saturating_mul(per_item)
}
