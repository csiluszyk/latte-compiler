# latte

## Optimizations

* copy propagation (during SSA phase),
* constant folding and constant propagation (`propagateConstsInsts`),
* string constants folding (i.e. don't call `_concat` for string literals)
* peephole optimization for the following expressions
(during constant propagation):
    * additon: `x + 0`, `0 + x`
    * substitution: `x - 0`, `x - x`
    * multiplication: `x * 1`, `1 * x`
    * division: `x / 1`, `x / x`
    * modulo: `x % 1`, `x % x`
    * comparision: `x op x`

### todo:
* dead code elimination
    * unused variables
        * if function call can then leave (function can have side effects like printing or infinite loop inside)
    * icmp 1, icmp 0
    * ret : insts
    * check used labels (also in phis) -> delete unused
* same exprs locally (without function calls with side effects) (including getelementptr)
* same exprs globally (without function calls with side effects) (including getelementptr)
* direct jumps
    * [phi, goto] - can delete
