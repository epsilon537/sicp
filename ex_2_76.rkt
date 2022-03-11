#lang racket

;Exercise 2.76: As a large system with generic operations evolves, new types of data objects or new operations may be needed. For each of the three strategies—generic operations with ;explicit dispatch, data-directed style, and message-passing-style—describe the changes that must be made to a system in order to add new types or new operations. 

;A:
;I got this one wrong. I answered by listing the mechanical steps needed to add an operation or type in each of the three cases, but that just obfuscates things.
;It's better to look at it in terms of impact on existing code if a type or operation is added in each of the three cases. That gives a clear result:
;
;Adding new types:
;- explicit dispatch:
;  Existing code: The dispatching logic in each generic function has to be extended to accommodate the new type.
;  => Not good.
;
;- data-directed:
;  Existing code: The generic functions and existing packages are not impacted.
;  New code: The new type's code has to be wrapped into a package and plugged into the dispatching table. 
;  => Good.
;
;- message-passing:
;  Existing code: Generic code and existing type-specific code is not impacted.
;  New code: The new type's constructor has to be written message-passing-style, i.e. containing an operation dispatching function. 
;  => Good.
;
;Adding new operations:
;- explicit dispatch:
;  Existing code: Neither existing generic or existing type specific code are impacted.
;  New code: The implementation of the new operation needs to be created. 
;  => Good.
;
;- data-directed:
;  Existing code: Neither existing generic or existing type specific code are impacted.
;  New code: The implementation of the new operation needs to be created. Note that new code can be added in its own package.
;  => Good.
;
;- message passing:
;  Existing code: Each type's constructor dispatching function needs to be extended with the new operator.
;  => Not good.

;Which organization would be most appropriate for a system in which new types must often be added?
;A: Data directed or Message Passing.

;Which would be most appropriate for a system in which new operations must often be added?
;A: Data directed or Explicit Dispatch.
