# latte compiler

## Optimizations

* copy propagation (during SSA phase),
* constant folding and constant propagation (`propagateConstsInsts`),
* string constants folding (i.e. don't call `_concat` for string literals)
* peephole optimization for the following expressions
(during constant propagation):
    * addition: `x + 0`, `0 + x`
    * substitution: `x - 0`, `x - x`
    * multiplication: `x * 1`, `1 * x`
    * division: `x / 1`, `x / x`
    * modulo: `x % 1`, `x % x`
    * comparision: `x op x`

### todo:
* dead code elimination
    * eliminating dead stores
        * if function call can then leave (function can have side effects like printing or infinite loop inside)
    * eliminating unreachable code
* same exprs locally (without function calls with side effects) (including getelementptr)
* same exprs globally (without function calls with side effects) (including getelementptr)
* flow-of-control optimization - removing jumps to jumps, including blocks with
phi instructions only (except the case when subsequent blocks don't have phi
instructions at all - in such case further SSA phase run should be run)
