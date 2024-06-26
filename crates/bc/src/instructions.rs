//! Module defining instructions

use crate::{
    instructions, BuiltinByte, FunctionId, LongOffset, LongReg, Offset, Primitive, Reg, StringId,
    UpvalueId,
};

instructions! {
    /// Load the i8 value in imm into dst
    Loadi8{ dst: Reg, imm: i8 },
    /// Load the i16 value in imm into dst
    Loadi16{ dst: Reg, imm: i16 },
    /// Load the i32 value in imm into dst
    Loadi32{ dst: Reg, imm: i32 },
    /// Load the f32 value in imm into dst
    Loadf32{ dst: Reg, imm: f32 },
    /// Load the f64 value in imm into dst
    Loadf64{ dst: Reg, imm: f64 },
    /// Loads a small primitive value in imm into dst
    /// - 0: Empty
    /// - 2: Null
    /// - 5: Deleted
    /// - 6: True
    /// - 7: False
    /// - 10: Undefined
    LoadPrim{ dst: Reg, imm: Primitive },
    /// Load the string with id `const` into dst
    LoadString{ dst: Reg, cons: StringId },
    /// Load a bytecode function into dst
    LoadFunction{ dst: Reg, cons: FunctionId },
    /// Load the global object into register dst
    LoadGlobal{ dst: Reg},

    /// Create a new object and load it into register reg
    NewObject{ dst: Reg},
    /// Create a new array like object and load it into register reg
    NewArray{ dst: Reg},

    /// `dst = obj[key]`, or index into obj with key and store into dst
    IndexLoad{ dst: Reg, obj: Reg, key: Reg },
    /// `obj[key] = src` or set the value of obj at key to the value in src
    IndexStore{ obj: Reg, key: Reg, src: Reg },

    /// Copy the value from src into dst.
    Move{ dst: Reg , src: Reg},
    /// Copy the value from src into dst allowing reaching far furter then regular move
    MoveLong{ dst: LongReg, src: LongReg},

    /// Load an upvalue into a specified registers dst
    LoadUpvalue{ dst: Reg, upvalue: UpvalueId },
    /// Store the value or register src into an upvalue.
    StoreUpvalue{ upvalue: UpvalueId, src: Reg},

    /// Add left to right and store the result into dst.
    Add{ dst: Reg, left: Reg, right: Reg},
    /// Subtract left form right and store the result into dst.
    Sub{ dst: Reg, left: Reg, right: Reg},
    /// Multiply left by right and store the result into dst.
    Mul{ dst: Reg, left: Reg, right: Reg},
    /// Divide left by right and store the result into dst.
    Div{ dst: Reg, left: Reg, right: Reg},
    /// Modulo left by right and store the result into dst.
    Mod{ dst: Reg, left: Reg, right: Reg},
    /// Raise left to the power of right store the result into dst.
    Pow{ dst: Reg, left: Reg, right: Reg},

    /// Shift left left by right and store the result into dst
    ShiftL{ dst: Reg, left: Reg, right: Reg},
    /// Shift right left by right and store the result into dst
    ShiftR{ dst: Reg, left: Reg, right: Reg},
    /// Shift right unsigned left by right and store the result into dst
    ShiftRU{ dst: Reg, left: Reg, right: Reg},

    /// Calculate if left and right are equal and store the result in dst.
    Equal{ dst: Reg, left: Reg, right: Reg},
    /// Calculate if left and right are strictly equal and store the result in dst.
    SEqual{ dst: Reg, left: Reg, right: Reg},
    /// Calculate if left and right are not equal and store the result in dst.
    NotEqual{ dst: Reg, left: Reg, right: Reg},
    /// Calculate if left and right are strictly not equal and store the result in dst.
    SNotEqual{ dst: Reg, left: Reg, right: Reg},

    /// Calculate if left is greater then right and store the result in dst.
    Greater{ dst: Reg, left: Reg, right: Reg},
    /// Calculate if left is greater then  or equal to right and store the result in dst.
    GreaterEq{ dst: Reg, left: Reg, right: Reg},
    /// Calculate if left is less then right and store the result in dst.
    Less{ dst: Reg, left: Reg, right: Reg},
    /// Calculate if left is less then or equal to right and store the result in dst.
    LessEq{ dst: Reg, left: Reg, right: Reg},

    /// Calculate bitwise and of left and right and store the result in dst.
    BitAnd{ dst: Reg, left: Reg, right: Reg},
    /// Calculate bitwise or of left and right and store the result in dst.
    BitOr{ dst: Reg, left: Reg, right: Reg},
    /// Calculate bitwise exclusive or of left and right and store the result in dst.
    BitXor{ dst: Reg, left: Reg, right: Reg},
    /// Calculate bitwise not of src and store the result in dst.
    BitNot{ dst: Reg, src: Reg},

    /// Calculate negative of src and store the result in dst.
    Neg{ dst: Reg, src: Reg},
    /// Coerce src into a number and store the result in dst.
    ToNum{ dst: Reg, src: Reg},
    /// Calculate if src is falsish and store the result in dst.
    Not{ dst: Reg, src: Reg},

    /// Jump to instruction with offset dst.
    Jump{ dst: Offset },
    /// Jump to instruction with offset dst if cond is trueish.
    JumpTrue{ cond: Reg, dst: Offset},
    /// Jump to instruction with offset dst if cond is falsish.
    JumpFalse{ cond: Reg, dst: Offset},
    /// Jump to a offset in table with index table based on the idx register.
    /// Used for handling control flow after a finally statement.
    JumpTable{ idx: Reg, table: u32 },

    /// Jump to instruction with offset dst.
    LongJump{ dst: LongOffset},
    /// Jump to instruction with offset dst if cond is trueish.
    LongJumpTrue{ cond: Reg, dst: LongOffset},
    /// Jump to instruction with offset dst if cond is falsish.
    LongJumpFalse{ cond: Reg, dst: LongOffset},

    /// Throw the error value in src.
    Throw{ src: Reg },
    /// Push a error handler onto the stack which jumps to dst if an error was thrown.
    Try{ dst: Offset },
    /// Same a try but able to jump further.
    TryLong{ dst: LongOffset },
    /// Remove the error handler from the stack.
    Untry{},
    ///  Retrieve the thrown error value and store it in dst.
    Catch{ dst: Reg },

    /// Return from the current function with a undefined value.
    RetUndefined{},
    /// Return from the current function with a the value in the src register.
    Ret{ src: Reg },

    CallBuiltin{reg: Reg, func: BuiltinByte},
    /// Call the function in func and store its result in ret.
    Call{dst: Reg, argc: u16 },
    /// Call the function with a specific number of arguments store its result in ret.
    Call1{dst: Reg, func: Reg, this: Reg},
    /// Call the function with a specific number of arguments store its result in ret.
    Call2{dst: Reg, func: Reg, this: Reg, arg1: Reg},
    /// Call the function with a specific number of arguments store its result in ret.
    Call3{dst: Reg, func: Reg, this: Reg, arg1: Reg, arg2: Reg},
}
