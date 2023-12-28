
#lang play

(require "deftype.rkt")
(require "env.rkt")

;; build-fun-env :: (ListOf TypedId) (ListOf Value) Env -> Env
;; Returns an environment for the interpretation of a function body,
;; where the formal parameters of the function reference the actual
;; arguments passed.
(define (build-fun-env params args env)
  (match params
    [(cons x xs)
     (def id  (typedId-id x))
     (def arg (car args))
     (def extended-env (extend-env id arg env))
     (build-fun-env xs (cdr args) extended-env)]
    [(list) env]))

;; build-typed-env :: (ListOf TypedId) Env -> Env
;; Takes a list of typed ids and an environment,
;; and returns an extended environment with the
;; id-type pairs in the list.
(define (build-type-env typed-params env)
  (match typed-params
    [(cons (typedId id type) rest)
     (def extended-env (extend-env id type env))
     (build-type-env rest extended-env)]
    [(list) env]))


;; lookup-fundef :: Symbol (ListOf Fundef) -> Fundef
;; looks up the requested function by its
;; identifier among the defined functions
(define (lookup-fundef f funs)
  (match funs
    [(list ) (error 'lookup-fundef "function not found: ~a" f)]
    [(cons (and fd (fundef fn _ _ _ _)) rest)    ;; untyped function 
     (if (symbol=? fn f)
         fd
         (lookup-fundef f rest))]))



;; sym-to-type :: Symbol -> Type
;; Translates a type annotation to a Type
(define (sym-to-type s)
  (match s
    ['Num (numT)]
    ['Bool (boolT)]
    [(list 'Pair ta1 ta2)
     (def t1 (sym-to-type ta1))
     (def t2 (sym-to-type ta2))
     (pairT t1 t2)]
    [_ (error (format "Error: There is no ~a type!" s))]))


;; type-to-sym :: Type -> Symbol
;; Translates a type to its corresponding type
;; annotation.
(define (type-to-sym t)
  (match t
    [(numT)  'Num]
    [(boolT) 'Bool]
    [(pairT t1 t2) (list 'Pair (type-to-sym t1) (type-to-sym t2))]
    [_ (error "Type not recognized")]))

;; val-to-str :: Value -> String
(define (val-to-str v)
  (match v
    [(numV n)  (format "~a" n)]
    [(boolV b) (format "~a" b)]
    [(pairV l r)
     (def s1 (val-to-str l))
     (def s2 (val-to-str r))
     (format "(~a,~a)" s1 s2)]))



;; wrong-type-error :: Symbol Type Type -> err
;; Raises an exception for a given operation when
;; operand type is not right.
(define (operand-type-error op-sym exp-type actual-type)
  (def exp-sym
    (type-to-sym exp-type))
  (def actual-sym
    (type-to-sym actual-type))
  (def err-message
    (format "Static type error: operator ~a expected ~a found ~a" op-sym exp-sym actual-sym)) 
  (error err-message))


;; arity-error :: Symbol Number Number -> err
;; Raises error for a function with wrong number
;; of arguments.
(define (arity-error fun arity actual-arg-num)
  (def err-message
    (format "Arity mismatch: ~a expected ~a found ~a" fun arity actual-arg-num))
  (error err-message))



;; arg-type-error :: Type Type -> err
;; Raises an exception for a function argument
;; with wrong type.
(define (arg-type-error param-type arg-type)
  (def t1
    (type-to-sym param-type))
  (def t2
    (type-to-sym arg-type))
  (def err-message
    (format "Static type error: expected ~a found ~a" t1 t2))
  (error err-message))




;; ret-type-err :: Symbol Type Type -> err
;; Raises an exception for functions which return
;; a type other than the declared one.
(define (ret-type-err f declared-type actual-type)
  (def t1 (type-to-sym declared-type))
  (def t2 (type-to-sym actual-type))
  (def err-message (format "Static type error: function ~a expected return type ~a found ~a" f t1 t2))
  (error err-message))



;; contract-err :: Value Symbol -> err
;; Called in runtime when an argument value does not fulfill its
;; contract.
(define (contract-err value predicate)
  (def v (val-to-str value))
  (def err-message (format "Runtime contract error: ~a does not satisfy ~a" v predicate))
  (error err-message))

;; predicate-type-err :: Symbol -> err
;; called when predicate of contract has a wrong type.
(define (predicate-type-err predicate)
  (def err-message (format "Static contract error: invalid type for ~a" predicate))
  (error err-message))


;; ret-type-test :: Symbol Type Type -> Number / err
;; If actual type matches the declared returned
;; type, succeeds with 1. Otherwise, raises an
;; exception.
(define (ret-type-test f declared-type actual-type)
  (cond
    [(equal? declared-type (anyT)) 1]
    [ else
      (if (equal? declared-type actual-type) 1
          (ret-type-err f declared-type actual-type))]))




;; operand-type-test :: Symbol Type Type -> Bool
;; Returns #t if the actual type matches the
;; expected type.
(define (operand-type-test op-sym exp-type actual-type)
  (if (equal? exp-type actual-type)
      #t (operand-type-error op-sym exp-type actual-type)))



