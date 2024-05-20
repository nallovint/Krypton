#[macro_export]
macro_rules! debug {
    ($($arg:tt)*) => {
        if cfg!(debug_assertions) {
            println!($($arg)*);
        }
    }
}

pub mod bytecode;
pub mod compiler;
pub mod vm;
