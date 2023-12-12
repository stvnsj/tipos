
#lang play

(require "p3.rkt")

(define-syntax run-test
  (syntax-rules ()
    ((_ bool program expected)
     (if bool (test (run program) expected) (displayln ">> Test Omitted\n")))))



(run-test #f '{5} (numV 5))


;; Lexical Scope, evaluation is 11 (in dynamic scope should be zero)
(run-test #f '{
               
               {define {f {x : Num}} : Num {with {{y 1}} {+ x y}}}
               
               {with {{y : Num 0}} {f 1}}
               
               } (numV 2))

;; Xor
(run-test #f '{

               {define {xor {p : Bool} {q : Bool}} : Bool
                 { || { && p {! q}} {&& {! p} q}}}

               {xor #t #f}
               
               } (boolV #t))


;; Typed recursive function
(run-test #t '{

               {define {fib {n : Num}} : Num
                 {if {= n 0} 0
                     {if {= n 1} 1
                         {+ {fib {- n 1}} {fib {- n 2}}}}}}

               {fib 8}
               
               } (numV 21))

