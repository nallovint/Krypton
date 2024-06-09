extern crate core;
#[macro_export]
macro_rules! debug {
    ($($arg:tt)*) => {
        if cfg!(debug_assertions) {
            println!($($arg)*);
        }
    }
}

pub mod bytecode;

#[cfg(feature = "compiler")]
pub mod compiler;

#[cfg(feature = "vm")]
pub mod vm;
