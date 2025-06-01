# Changelog

## [Unreleased]

### krypton-lang/src/vm.rs
- Refactored the stack to use `MaybeUninit<Value>` for safe memory management, replacing the previous fixed-size array of `Value`.
- Updated the `push` method to use `MaybeUninit::write` to initialize stack slots.
- Updated the `pop` method to use `MaybeUninit::assume_init_read` to safely extract values and de-initialize slots.
- Implemented a custom `Drop` for the `VM` struct to ensure only initialized stack elements are dropped, preventing double free and invalid free errors.
- Updated the `debug_stack_trace` method to safely display only initialized stack values using `assume_init_ref`.

### krypton-lang/src/bytecode.rs
- No changes made in this round, but previously ensured that `Value` is `Clone` and safe for stack operations.

### CHANGES.md
- Created this changelog to document recent changes, fixes, and improvements.

### Fixed
- Resolved double free and memory errors that occurred when running programs involving heap-allocated values (e.g., strings).
- The VM now runs example programs (such as `examples/variables.kr`) to completion without memory errors, and prints all expected results.

---

For more details, see the changes in `krypton-lang/src/vm.rs` and the test run outputs in the assistant's session. 