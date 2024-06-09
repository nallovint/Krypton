# Klass Bytecode Specification

u16 - [Magic Number](#magic-number)
u16 - [Version](#version)
u8 - [Name Length](#name)
u8[length] - [Name](#name)

## Magic Number

The magic number is a two-byte sequence that identifies it as a Klass bytecode file.
The first byte is `K` and the second byte is `r` in ASCII.

## Version

The version is a two-byte sequence that identifies the version of the Klass bytecode file.
The first byte is `V` and the second byte is a version number between 1 and 255.
Zero is considered an invalid version number.

## Name

The name is a string that identifies the Klass bytecode file.
It is encoded as an u8 byte for the length of the name and then the name itself.
Zero is a valid name length and is assumed as an "anonymous" Klass bytecode file.
1–255 is the range of lengths for a non-anonymous Klass bytecode file name.
Names may contain only the ASCII characters `a`–`z`, `A`–`Z`, `0`–`9`, and `_`.



