
#lang play
;(require "env.rkt")
(require "deftype.rkt")
;(require "utils.rkt")
(require "parse.rkt")
(require "typecheck.rkt")



(define (run sp)
  (def (prog funs main) (parse sp))
  (begin
    (typecheck (prog funs main))))



(run '{
       {define { f {x : Num} {y : Num} } : Num {+ x y}}
       {f 3 2}})
