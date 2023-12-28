
#lang play
(require "env.rkt")



;; (debuggin function)
(define (pp name object)
  (display (format "===================================================\n~a :: ~a \n" name object))
  1) 





;; ====================
;;   Data Structures
;; ====================

;; Expression
(deftype Expr
  (Num n)
  (Id x)
  (Bool b)
  (Cons f s)
  (Fst p)
  (Snd p)
  (Add1 n)
  (Add l r)
  (Sub l r)
  (Mul l r)
  (Div l r)
  (Lt l r)
  (Eq l r)
  (Not b)
  (And l r)
  (Or l r)
  (If c t f)
  (With x e b)
  (App f e))

;; Value
(deftype Val
  (numV n)
  (boolV b)
  (pairV lV rV))

;; Type
(deftype Type
  (anyT)
  (numT)
  (boolT)
  (pairT lT rT))

;; Program
(deftype Prog
  (prog funs main))

;; Function Definition
(deftype Fundef
  (fundef name type arg body contracts))

;; Formal Parameter
(deftype TypedId
  (typedId id type))

;; Contract (of a function argument)
(deftype ArgContract
  (argContract id type predicate))

;; ==============================
;;     AUXILIARY FUNCTIONS
;; ==============================


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

;; ret-type-err :: Symbol Type Type -> err
;; Raises an exception for functions which return
;; a type other than the declared one.
(define (ret-type-err f declared-type actual-type)
  (def t1 (type-to-sym declared-type))
  (def t2 (type-to-sym actual-type))
  (def err-message (format "Static type error: function ~a expected return type ~a found ~a" f t1 t2))
  (error err-message))


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
;; Translates a type to its corresponding type annotation.
(define (type-to-sym t)
  (match t
    [(numT)  'Num]
    [(boolT) 'Bool]
    [(pairT t1 t2) (list 'Pair (type-to-sym t1) (type-to-sym t2))]
    [_ (error "Type not recognized")]))



;; operand-type-test :: Symbol Type Type -> Bool
;; Returns #t if the actual type matches the expected type.
(define (operand-type-test op-sym exp-type actual-type)
  (if (equal? exp-type actual-type)
      #t (operand-type-error op-sym exp-type actual-type)))



;; =====================================
;;          PARSING FUNCTIONS
;; =====================================


;; parse :: [Symbolic Program] -> Program
(define (parse sp)
  (match sp
    [(list ds ... e)
     (prog (map parse-fundef ds) (parse-expr e))]))


;; parse-expr :: [Symbolic Expression] -> Expression
(define (parse-expr se)
  (match se
    [(? number?)              (Num  se)]
    [(? symbol?)              (Id   se)]
    [(? boolean?)             (Bool se)]
    [(list 'cons se1 se2)     (Cons (parse-expr se1) (parse-expr se2))]
    [(list 'add1 se1)         (Add1 (parse-expr se1))]
    [(list '+ se1 se2)        (Add  (parse-expr se1) (parse-expr se2))]
    [(list '- se1 se2)        (Sub  (parse-expr se1) (parse-expr se2))]
    [(list '* se1 se2)        (Mul  (parse-expr se1) (parse-expr se2))]
    [(list '/ se1 se2)        (Div  (parse-expr se1) (parse-expr se2))]
    [(list '< se1 se2)        (Lt   (parse-expr se1) (parse-expr se2))]
    [(list '= se1 se2)        (Eq   (parse-expr se1) (parse-expr se2))]
    [(list '! se1)            (Not  (parse-expr se1))]
    [(list '&& se1 se2)       (And  (parse-expr se1) (parse-expr se2))]
    [(list '|| se1 se2)       (Or   (parse-expr se1) (parse-expr se2))]
    [(list 'fst se)           (Fst  (parse-expr se))]
    [(list 'snd se)           (Snd  (parse-expr se))]
    [(list 'if se1 se2 se3)   (If   (parse-expr se1) (parse-expr se2) (parse-expr se3))]
    [(list 'with lst e)
     (match lst
       
       [(cons (list x b) bs)
        (def named-expr
          (parse-expr b))
        (def body-expr
          (parse-expr (list 'with bs e))) 
        (With (typedId x (anyT)) named-expr body-expr )]
       
       [(cons (list x ': t  b) bs)
        (def named-expr
          (parse-expr b))
        (def body-expr
          (parse-expr (list 'with bs e)))
        (def type
          (sym-to-type t))
        (With (typedId x type) named-expr body-expr )]
       
       [(list)
        (parse-expr e)])]
    
    [(cons f ses) (App f (map parse-expr ses))]
    
    [_ (error "not yet implemented")]))

;; parse-fundef-params :: 
(define (parse-fundef-params se)
  (match se
    [(or (cons (list id ': type-annotation) rest)
         (cons (list id ': type-annotation '@ _) rest))
     (def type  (sym-to-type type-annotation))
     (def param (typedId id type))
     (cons param (parse-fundef-params rest))]
    [(list) (list)]
    [_ (error "Bad function parameter")]))

;; parse-fundef-contracts :: 
(define (parse-fundef-contracts se)
  (match se
    ;; parameter has contract
    [(cons (list id ': type-annotation '@ predicate) rest)
     (def type  (sym-to-type type-annotation))
     (def cont  (argContract id type predicate))
     (cons cont (parse-fundef-contracts rest))]
    ;; parameter with no contract.
    [(cons (list _ ': _) rest) (parse-fundef-contracts rest)]
    [(list) (list)]
    [_ (error "Bad function parameter")]))


;; parse-fundef :: [Symbolic Function Declaration] -> Fundef
(define (parse-fundef sf)
  
  (match sf
    
    ;; untyped function definition
    [(list 'define (cons fun params) body)
     (def typed-params (parse-fundef-params params))
     (def contracts    (parse-fundef-contracts params))
     (def body-expr    (parse-expr body))
     (fundef fun (anyT) typed-params body-expr contracts)]

    ;; typed function definition
    [(list 'define (cons fun params) ': type-annotation body)
     (def fun-type     (sym-to-type type-annotation))
     (def typed-params (parse-fundef-params params))
     (def contracts    (parse-fundef-contracts params))
     (def body-expr    (parse-expr body))
     (fundef fun fun-type typed-params body-expr contracts)]))



;; ==============================================
;;              TYPECHECK FUNCTIONS
;; ==============================================

;; Builds an environment whose pairs have function name as id and
;; function type as value.
(define (build-funtype-env fenv funs)
  (match funs
    [(list) fenv]
    [(cons f fs)
     (def f-name
       (fundef-name f))
     (cond
       [(env-contains f-name fenv)
        (build-funtype-env fenv fs)]
       [else
        (def (list _ fenv1)
          (typecheck-fundef f fenv funs))
        (build-funtype-env fenv1 fs)])]))

;; typecheck-fundef :: Fundef Env (ListOf Fundef) -> Type
(define (typecheck-fundef f fenv funs)
  (def (fundef f-name f-type f-params f-body _) f)
  (def env
    (build-type-env f-params empty-env))
  (def (list body-type fenv0)
    (typecheck-expr f-body env fenv funs))
  
  (def fenv1
    (extend-env f-name body-type fenv0))

  (begin
    (ret-type-test f-name f-type body-type)
    (list body-type fenv1)))



(define (typecheck-expr-list e-list t-list env fenv funs)
  (match e-list
    [(cons e es)
     (def (list t fenv1)  (typecheck-expr e env fenv funs))
     (typecheck-expr-list es (cons t t-list) env fenv1 funs)]
    [(list) (list (reverse t-list) fenv)]))





;; typecheck-expr :: Expr Env Env (ListOf Fundef) -> (Type Env Env)
(define (typecheck-expr e venv fenv funs)
  (match e

    [(Num _)  (list (numT)  fenv)]
    
    [(Bool _) (list (boolT)  fenv)]
    
    [(Id x)
     (def type (env-lookup x venv))
     (list type fenv)]
    
    [(Cons e1 e2)
     (def (list t1  fenv1) (typecheck-expr e1 venv fenv  funs))
     (def (list t2  fenv2) (typecheck-expr e2 venv fenv1 funs))
     (list (pairT t1 t2)  fenv2)]

    
    [(Fst e1)
     (def (list type1  fenv1) (typecheck-expr e1 venv fenv funs))
     (match type1
       [(pairT t _) (list t  fenv1)]
       [ _ (error "Expression is not a pair")])]

    [(Snd e1)
     (def (list type1 fenv1) (typecheck-expr e1 venv fenv funs))
     (match type1
       [(pairT _ t) (list t  fenv1)]
       [ _ (error "Expression is not a pair")])]

    [(Add1 e1)
     (def (list t1  fenv1) (typecheck-expr e1 venv fenv funs))
     (begin
       (operand-type-test 'add1 (numT) t1)
       (list (numT)  fenv1))]

    [(Add e1 e2)
     (def (list t1  fenv1) (typecheck-expr e1 venv fenv funs))
     (def (list t2  fenv2) (typecheck-expr e2 venv fenv1 funs))
     (begin
       (operand-type-test '+ (numT) t1)
       (operand-type-test '+ (numT) t2)
       (list (numT)  fenv2))]
    
    [(Sub e1 e2)
     (def (list t1 fenv1) (typecheck-expr e1 venv fenv funs))
     (def (list t2 fenv2) (typecheck-expr e2 venv fenv1 funs))
     (begin
       (operand-type-test '- (numT) t1)
       (operand-type-test '- (numT) t2)
       (list (numT) fenv2))]

    [(Mul e1 e2)
     (def (list t1 fenv1) (typecheck-expr e1 venv fenv funs))
     (def (list t2 fenv2) (typecheck-expr e2 venv fenv1 funs))
     (begin
       (operand-type-test '* (numT) t1)
       (operand-type-test '* (numT) t2)
       (list (numT) fenv2))]

    [(Div e1 e2)
     (def (list t1 fenv1) (typecheck-expr e1 venv fenv funs))
     (def (list t2 fenv2) (typecheck-expr e2 venv fenv1 funs))
     (begin
       (operand-type-test '/ (numT) t1)
       (operand-type-test '/ (numT) t2)
       (list (numT) fenv2))]

    [(Eq e1 e2)
     (def (list t1  fenv1) (typecheck-expr e1 venv fenv funs))
     (def (list t2  fenv2) (typecheck-expr e2 venv fenv1 funs))
     (begin
       (operand-type-test '= (numT) t1)
       (operand-type-test '= (numT) t2)
       (list (boolT) fenv2))]

    [(Lt e1 e2)
     (def (list t1  fenv1) (typecheck-expr e1 venv fenv funs))
     (def (list t2  fenv2) (typecheck-expr e2 venv fenv1 funs))
     (begin
       (operand-type-test '< (numT) t1)
       (operand-type-test '< (numT) t2)
       (list (boolT) fenv2))]
       
    [(Not e1)
     (def (list t1 fenv1) (typecheck-expr e1 venv fenv funs))
     (begin
       (operand-type-test '! (boolT) t1)
       (list (boolT) fenv1))]



    ;; ───────────────────────────────────────────
    [(And e1 e2)
     (def (list t1  fenv1) (typecheck-expr e1 venv fenv funs))
     (def (list t2  fenv2) (typecheck-expr e2 venv fenv1 funs))
     (begin
       (operand-type-test '&& (boolT) t1)
       (operand-type-test '&& (boolT) t2)
       (list (boolT) fenv2))]

    ;; ───────────────────────────────────────────
    [(Or e1 e2)
     (def (list t1 fenv1) (typecheck-expr e1 venv fenv funs))
     (def (list t2 fenv2) (typecheck-expr e2 venv fenv1 funs))
     (begin
       (operand-type-test '|| (boolT) t1)
       (operand-type-test '|| (boolT) t2)
       (list (boolT) fenv2))]

    ;; ───────────────────────────────────────────
    [(If e1 e2 e3)
     (def (list t1  fenv1) (typecheck-expr e1 venv fenv funs))
     (def (list t2  fenv2) (typecheck-expr e2 venv fenv1 funs))
     (def (list t3  fenv3) (typecheck-expr e3 venv fenv2 funs))
     (cond
       [(not (boolT? t1)) (arg-type-error (boolT) t1)]
       [(not (equal? t2 t3)) (error "Branches must have same type")]
       [else (list t3 fenv3)])]


    [(With (typedId id id-type) e1 e2)
     (def (list type fenv1)  (typecheck-expr e1 venv fenv funs))
     (cond
       
       ;; named expression and identifier have the same
       ;; type or identifier can be any type.
       [(or (equal? id-type (anyT)) (equal? id-type type))
        (def extended-env (extend-env id type venv))
        (def (list t2 fenv2) (typecheck-expr e2 extended-env fenv1 funs))
        (list t2 fenv2)]
       
       ;; wrong type!
       [else (arg-type-error id-type type)])]




    [(App f arguments)
     (def (list t-list fenv1) (typecheck-expr-list arguments '() venv fenv funs))
     (def fun (lookup-fundef f funs))
     (def (fundef _ _ params _ _) fun)
     (def arity    (length params))
     (def args-num (length arguments))
     (def test-arg-type (λ (param arg-type)
                          (def (typedId _ id-type) param)
                          (if (equal? id-type arg-type)
                              #t
                              (arg-type-error id-type arg-type))))
     (def test-arity    (λ ()
                          (if (equal? arity args-num)
                              #t (arity-error f arity args-num))))
     (begin
       (test-arity)
       (map test-arg-type params t-list)
       (cond
         [(env-contains f fenv1) (list (env-lookup f fenv1) fenv1)]
         [else (typecheck-fundef fun fenv1 funs)]))]
    
    [_ (error "not yet implemented")]))



(define (typecheck p)
  (def (prog funs main) p)
  (def fenv (build-funtype-env empty-env funs))
  (def (list type _) (typecheck-expr main empty-env fenv funs))
  type)




(define (run sp)
  (def (prog funs main) (parse sp))
  (begin
    (typecheck (prog funs main))))
