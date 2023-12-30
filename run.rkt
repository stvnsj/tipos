#lang play
(require "env.rkt")
(require "deftype.rkt")
(require "parse.rkt")
(require "typecheck.rkt")
(require "interp.rkt")



(define (run sp)
  (def (prog funs main) (parse sp))
  (begin
    (typecheck (prog funs main))
    (interp main empty-env funs)))
