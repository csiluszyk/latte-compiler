# latte

## Optimization

### done:
* copy propagation (during SSA phase),
* constant folding and constant propagation (`propagateConstsInsts`),

### todo:
* dead code elimination
    * if call function can have side effects then leave
    * check used labels (also in phis) -> delete unused
* blocks only with jumps
    * [phi, goto] - can delete
* same exprs locally (without function calls with side effects)
* same exprs globally (without function calls with side effects)
* also for string (_concat)
