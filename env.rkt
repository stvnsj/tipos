
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


;; env-lookup :: Symbol Env -> Value
(define (env-lookup x env)
  (begin
    (match env
      [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
      [(aEnv id val rest)
       (if (symbol=? id x)
           val
           (env-lookup x rest))])))


;; env-contains :: Symbol Env -> Bool
;; Returns #t if env contains the identifier 'x'.
(define (env-contains x env)
  (match env
    [(mtEnv) #f]
    [(aEnv id val rest)
     (if (symbol=? id x)
         #t
         (env-contains x rest))]))

