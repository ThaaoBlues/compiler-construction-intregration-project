# Programming Paradigms Integrating Project

**Date:** 04-07-2025

## Overview

This project implements a custom programming language with a focus on basic types, expressions, variable typing, and concurrency features. The language is designed to be strongly typed and supports multi-threading with locks for synchronization.

## Installing the compiler
- get all the files
- navigate into the root directory of the compiler

## Running the compiler

### Compile a given file and start program on Sprockells

```bash 
stack run file_name.whatever_extension
```
It is possible to use whatever file you want to write code in it as long as it contains brut text.
Number of threads (and by extension, required Sprockells) should be automatically determined by the compiler.

### Rinning the automated tests
```bash 
stack test
```

## Language Features

### Comments
- Supports both single-line (`//`) and multi-line (`/* */`) comments.

### Basic Types
- Integers and booleans are the primary variable types.
- Supports arrays of integers and booleans (Sadly, code generation for this feature was not finished).

### Expressions
- **Integers**: Addition, subtraction, multiplication, and various equality/inequality comparisons.
- **Booleans**: Conjunction (AND), disjunction (OR), and negation (NOT).

### Variables
- Variables must be declared with a type, which remains fixed throughout their scope.
- Supports nested scopes and variable shadowing.

### Control Structures
- **If-Else Statements**: Uses `si` and `sino` for conditional execution.
- **While Loops**: Uses `durante` for loop constructs.

### Concurrency
- Implements a fork-join model for thread creation.
- Supports locks for thread synchronization.

### Input/Output
- Supports print statements for integers and booleans using `imprimir ¡expression! :)`.

### Scoping and Blocks
- Variables can be scoped within blocks, and nested blocks can shadow outer block variables.

## Example Programs

### Simple Assignment
```plaintext
entero x :)
x = 5 :)
imprimir ¡x! :)
```

### If-Else Statement
```plaintext
booleana condition :)
condition = verdad :)
si condition { imprimir ¡1! :) } sino { imprimir ¡0! :) }
```

### While Loop
```plaintext
entero x :)
x = 0 :)
durante x < 5 { x = x + 1 :) }
imprimir ¡x! :)
```

### Thread Creation
```plaintext
entero x :)
hilo { entero y :) y = 10 :) imprimir ¡y! :) }
esperamos :)
```

### Lock Usage
```plaintext
esclusa lock :)
obtener lock :)
imprimir ¡1! :)
liberar lock :)
```

### Global Variable
```plaintext
global entero sharedVar :)
sharedVar = 20 :)
imprimir ¡sharedVar! :)
```

## Software Description

### MyParser.hs
- **Symbol Tables**: Used for type checking. Symbol tables map variable names to their types. A stack of symbol tables (`STStack`) is used to manage nested scopes.
- **Grammar Rules and Parsing**: Uses the Parsec library to define parsers for different language constructs.
- **Inherited and Synthesized Attributes**: Used implicitly during parsing and type checking. The type of an expression is synthesized from its sub-expressions, while the symbol table is an inherited attribute passed down to child nodes.

### MyCodeGen.hs
- **Symbol Tables**: Used for code generation and memory management. Global and local symbol tables map variable names to memory addresses.
- **Code Generation**: Divided into two passes:
  1. **First Pass**: Fills the global symbol table with locks and global variables, allocating memory addresses.
  2. **Second Pass**: Generates the actual code, handling threads, locks, and other constructs.

### Spec.hs
- **Testing Framework**: Uses the Hspec library to define tests for the parser, type checker, and code generator. Tests ensure that each component functions correctly and interacts properly with the others.

## Test Plan and Results

### Parser Tests
- Objective: Ensure the parser correctly parses valid constructs and rejects invalid ones.
- Scope: Tests for variable declarations, assignments, conditionals, loops, threads, etc.

### Type Checker Tests
- Objective: Ensure the type checker correctly identifies type errors and accepts valid type assignments.
- Scope: Tests for valid and invalid type assignments, undeclared variables, etc.

### Code Generation Tests
- Objective: Ensure the code generator produces correct Sprockell code for various language constructs.
- Scope: Tests for generating code for basic print statements, if-else statements, thread creation, etc.

### Runtime Tests
- Objective: Ensure test programs behave as expected.
- Scope: Simple programs including loops, prints, if/else, and calculations.

## Running the Tests

To run the tests, use the following command:

```bash
stack test
```

The tests are automated and designed to be self-documenting with descriptive names and comments explaining the expected behavior.

## Problems Encountered

2. **Lack of Sprockell Documentation**: Made it challenging to implement certain features.
3. **Negating Booleans**: Implemented using an if-else construct due to the lack of a native NOT operator.
4. **Generating If/Else Statements**: Required a workaround due to the behavior of the branch instruction.
5. **Generating Multithreading Jumps/Initialization**: Required detailed planning and storage of thread information.
6. **Joining Threads**: Implemented using a shared counter and lock for synchronization.
7. **Implementing Locks**: Used the SPRIL "TestAndSet" atomic instruction for lock implementation.

