# latte

## Optimization

### done:
* copy propagation (during SSA phase)

### todo:
* dead code elimination
    * if call function can have side effects then leave
    * check used labels (also in phis) -> delete unused
* blocks only with jumps
    * [phi, goto] - can delete
* same exprs (without function calls with side effects)
* const propagation && computing value of expr
* also for string (_concat)
