# latte

## Optimization

### done:
* copy propagation (during SSA phase),
* constant folding and constant propagation (`propagateConstsInsts`),
* string constants folding (i.e. don't call `_concat` for string literals)

### todo:
* dead code elimination
    * if call function can have side effects then leave
    * check used labels (also in phis) -> delete unused
    * ret : insts
* blocks only with jumps
    * [phi, goto] - can delete
* same exprs locally (without function calls with side effects) (including getelementptr)
* same exprs globally (without function calls with side effects) (including getelementptr)
