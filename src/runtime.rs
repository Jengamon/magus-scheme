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
    // cost of defining/set!ing a value in environment
    pub const ENV_SET_COST: i32 = 6;
}
