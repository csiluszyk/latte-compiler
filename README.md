# latte compiler v.1.0.0 - Cezary Siłuszyk, 321191

## Building the project
### Requirements
* llvm-as
* llvm-link
* `ghc` v8.0.2 (if not present `stack` will install it during building)
* Haskell libraries:
    * `base`,
    * `array`,
    * `mtl`,
    * `containers`,
    * `extra`,
    * `filepath`,
    * `process`,
    * `HTF` (required only to run the test suite)

### Building
Project uses `stack` as a build tool (NOTE: currently there is no system-wide
`stack` installation on students so it uses one installed in
`/home/students/inf/PUBLIC/MRJP/Stack`, if you prefer to use your local `stack`
installation just change the `STACK` variable in the `Makefile`). You can
build whole project by calling `make`. After that `llvm_latc` executable should
be present in the project root directory.

### Running the compiler
Running the compiler follows the rules described in the assignment. To output
the `*.bc` bytecode it will use `llvm-as` and `llvm-link`. If some aspect of
the Latte language were ambiguous I tried to follow Java compiler behaviour
(e.g. only `==` and `!=` relations are allowed for strings).

### Running test suite
To run the tests use `make test`. It will use `HTF` library to run black
box tests from `test/tests` directory. Which test should be run and with what
effect is determined by `BBTArgs` files in test directories. To find more about
running tests check the `HTF` docs:
<http://hackage.haskell.org/package/HTF#readme>.

## Project structure
* `src/` - compiler source code structure:
    * `Main.hs` - project main,
    * `TypeCheckLatte.hs` - semantic analysis with type checking,
    * `GenLatte.hs` - LLVM IR generator,
    * `LlvmLatte.hs` - LLVM IR representation,
    * `SsaLatte.hs` - module for transforming LLVM IR to SSA form,
    * `OptLatte.hs` - implemented optimizations (described below),
    * `UtilsLatte.hs` - common utils,
    * `AbsLatte.hs`, `ErrM.hs`, `LexLatte.hs`, `ParLatte.hs` - parser files
    generated from provided grammar `Latte.cf` by `bcfc` (with changes from
    `source-position` branch),
* `test/` - test suite:
    * `Main.hs` - test runner,
    * `tests/good`, `tests/bad`, `tests/extensions` - tests provided in the
    assignment,
    * `tests/mrjp-tests` - tests from github,
    * `tests/misc` - additional tests.
* `lib/` - runtime library (with its `runtime.ll` source code)
* `latte.cabal` - Cabal configuration,
* `stack.yaml` - Stack configuration,
* `README.md`

## Optimizations
* copy propagation (during SSA phase),
* constants folding and constants propagation (`propagateConstsInsts`),
* string constants folding (i.e. don't call `_concat` and `_streq` for string
literals),
* peephole optimization for the following expressions (during constants
propagation):
    * addition: `x + 0`, `0 + x`,
    * substitution: `x - 0`, `x - x`,
    * multiplication: `x * 1`, `1 * x`,
    * division: `x / 1`, `x / x`,
    * modulo: `x % 1`, `x % x`,
    * comparision: `x op x`,
* dead code elimination:
    * eliminating dead stores (except function calls - they can have side
    effects like IO or infinite loop inside),
    * eliminating unreachable code,
* flow-of-control optimization:
    * removing jumps to jumps, including blocks with phi instructions only
    (except case when subsequent blocks don't have phi instructions at all -
    in such case further SSA phase run should be run),
    * unifying direct paths (i.e. without branches) in CFG.

## Extensions
In current version there is no implemented extensions.
