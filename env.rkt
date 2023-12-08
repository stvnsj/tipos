
#lang play

#|-----------------------------
Environment abstract data type

empty-env  :: Env
extend-env :: Sym Val Env -> Env
env-lookup :: Sym Env -> Val

representation BNF:
<env> ::= (mtEnv)
| (aEnv <id> <val> <env>)
|#
(deftype Env
  (mtEnv)
  (aEnv id val env))

(def empty-env  (mtEnv))

(def extend-env aEnv)

(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest)
     (if (symbol=? id x)
         val
         (env-lookup x rest))]))


;; build-fun-env :: build an environment for the interpretation of a
;; function body, where the formal parameters of the function
;; reference the actual arguments passed.
(define (build-fun-env params args env)
  (match params
    [(cons x xs)
     (def arg (car args))
     (def extended-env (extend-env x arg env))
     (build-fun-env xs (cdr args) extended-env)]
    [(list) env]))
