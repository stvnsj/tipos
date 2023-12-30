#lang play
(require "env.rkt")

#|
----------------------------------------------------------------------------

* Éric Tanter: 1- lo que se hace en estos casos es usar una función
auxiliar (que incluso puede estar anidada):

(define (typecheck-expr e)
  (define (typecheck e tenv)
     ... funcion recursiva ...)
  (typecheck e (empty-tenv))

asi la funcion top-level simplemente usar la funcion interna con un
ambiente de tipos vacío.
 

* Éric Tanter: 2- la manera de evitar typecheckear varias veces el
cuerpo de una función es simplemente extender el AST con la
información de tipo, que al inicio es no-inicializada (eg. #f) y luego
de typechecking, atachas la información de tipo.  La forma
no-imperativa (ie. funcional pura) de hacer esto es que el typing sea
una fase que toma un AST sin info de tipo (o con información parcial)
y produce un nuevo AST con toda la info asociada

Éric Tanter: 3- lo que hiciste es una buena opción (basicamente lo que
hacen en Python: si no hay info, entonces es Any)
----------------------------------------------------------------------------
|#





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

;; operand-type-test :: Symbol Type Type -> Bool
;; Returns #t if the actual type matches the expected type.
(define (operand-type-test op-sym exp-type actual-type)
  (if (equal? exp-type actual-type)
      #t (operand-type-error op-sym exp-type actual-type)))

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

;; wrong-type-error :: Symbol Type Type
;; Raises an exception for a given operation when type 
(define (operand-type-error op-sym exp-type actual-type)
  (define exp-sym    (type-to-sym exp-type))
  (define actual-sym (type-to-sym actual-type))
  (error (format "Static type error: operator ~a expected ~a found ~a" op-sym exp-sym actual-sym)))


;; wrong-arg-error :: Type Type -> err
;; Raises an exception for an argument with the wrong type in a
;; function
(define (arg-type-error param-type arg-type)
  (define t1 (type-to-sym param-type))
  (define t2 (type-to-sym arg-type))
  (error (format "Static type error: expected ~a found ~a" t1 t2)))

;; arity-error :: Symbol Number Number -> err
(define (arity-error fun arity actual-arg-num)
  (def err-message (format "Arity mismatch: ~a expected ~a found ~a" fun arity actual-arg-num))
  (error err-message))

;; ret-type-err :: Symbol Type Type -> err
(define (ret-type-err f declared-type actual-type)
  (def s1 (type-to-sym declared-type))
  (def s2 (type-to-sym actual-type))
  (def err-message (format "Static type error: declared return type of ~a was ~a returned ~a" f s1 s2))
  (error err-message))



;; build-typed-env :: (ListOf TypedId) Env -> Env
;; Takes a list of typed ids and an environment, and returns an
;; extended environment with the id-type pairs in the list. (Used in
;; typechecking of expressions)
(define (build-type-env typed-params env)
  (match typed-params
    [(cons (typedId id type) rest)
     (def extended-env (extend-env id type env))
     (build-type-env rest extended-env)]
    [(list) env]))


;; lookup-fundef :: Symbol (ListOf Fundef) -> Fundef
;; looks up the requested function by its identifier among the defined
;; functions
(define (lookup-fundef f funs)
  (match funs
    ['() (error 'lookup-fundef "function not found: ~a" f)]
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

;; val-to-str :: Value -> String
(define (val-to-str v)
  (match v
    [(numV n)  (format "~a" n)]
    [(boolV b) (format "~a" b)]
    [(pairV l r)
     (def s1 (val-to-str l))
     (def s2 (val-to-str r))
     (format "(~a,~a)" s1 s2)]))


;; parse :: Symbolic Program -> Program
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
        (def named-expr (parse-expr b))
        (def body-expr (parse-expr (list 'with bs e))) 
        (With x named-expr body-expr )]
       [(cons (list x ': t  b) bs)
        (def named-expr (parse-expr b))
        (def body-expr (parse-expr (list 'with bs e)))
        (def type (sym-to-type t))
        (With (typedId x type) named-expr body-expr )]
       [(list) (parse-expr e)])]
    
    [(cons f ses) (App f (map parse-expr ses))]
    
    [_
     (displayln se)
     (error "not yet implemented")]))







;; parse-fundef-params :: 
;; params : list of parameters in function definition.
(define (parse-fundef-params se)
  
  (match se

    [(or (cons (list id ': type-annotation) rest)
         (cons (list id ': type-annotation '@ _) rest))
     (def type  (sym-to-type type-annotation))
     (def param (typedId id type))
     (cons param (parse-fundef-params rest))]

    [(list) (list)]

    [_ (error "Bad function parameter")]))



;; parse-fundef-contracts :: (ListOf se) -> (ListOf Contract)
;; params : list of parameters in function definition.
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


;; parse-fundef :: [Symbolic Function Definition] -> Fundef
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




;; typecheck-fundef-list :: (ListOf Fundef) (ListOf Fundef) -> Bool
;; Returns #t if a type is successfully resolved for each checked
;; function. Otherwise, raises an exception.
(define (typecheck-fundef-list unchecked-funs funs)
  (match unchecked-funs
    [(cons f fs)
     (begin
       (typecheck-fundef f funs)
       (typecheck-fundef-list fs funs))]
    [(list) #t]))



;; typecheck-contract :: ArgContract (ListOf Fundef) -> Bool/err
(define (typecheck-contract con funs)

  (def (argContract _ arg-type predicate) con) ; unpack contract 
  (def con-def (lookup-fundef predicate funs)) ; predicate fundef
  (def (fundef _ type args _ _) con-def) ; unpack predicate fundef
  (def arity (length args)) ; predicate arity

  ;; ==== STATIC CONTRACT ERRORS ====
  (begin
    ;; If arity is not 1, then error.
    (if (equal? 1 arity) #t
        (predicate-type-err predicate))
    ;; If parameter type of predicate does not match the function
    ;; parameter type, then error
    (if (equal? arg-type (typedId-type (car args))) #t
        (predicate-type-err predicate))
    ;; If predicate does not return boolen, then error
    (if (equal? type (boolT)) #t ;; Declared type
        (if (equal? (typecheck-fundef con-def funs) (boolT))
            #t
            (predicate-type-err predicate)))))

      


;; typecheck-fundef :: Fundef (ListOf Fundef) -> Type
;; Returns the type of a function definition.
(define (typecheck-fundef f funs)
  (def (fundef f-id f-type f-params f-body f-contracts) f)
  (def env (build-type-env f-params empty-env))
  (def ret-type (typecheck-expr f-body env funs))
  (begin

    ;; Type-related tests on each contract are run.
    (map (λ (c) (typecheck-contract c funs)) f-contracts)

    (cond
      ;; If no return type is declared, the return type is that of the
      ;; body.
      [(equal? f-type (anyT))  ret-type]
      [else
       ;; If return type is declared, the type of the body must be
       ;; equal to it.
       (if (equal? f-type ret-type)
           f-type
           (ret-type-err f-id f-type ret-type))])))
  




;; typecheck-expr :: Expression Env (ListOf Fundef) -> Type/err
(define (typecheck-expr e env funs)

  (match e

    ;; Num 
    [(Num _) (numT)]
    
    ;; Id
    [(Id x)  (env-lookup x env)]

    ;; Bool
    [(Bool _) (boolT)]
    
    ;; Cons 
    [(Cons e1 e2)
     (def t1 (typecheck-expr e1 env funs))
     (def t2 (typecheck-expr e2 env funs))
     (pairT t1 t2)]

    ;; Fst
    [(Fst e1)
     (match (typecheck-expr e1 env funs)
       [(pairT t _) t]
       [ _ (error "Expression is not a pair")])]

    ;; Snd
    [(Snd e1) 
     (match (typecheck-expr e1 env funs)
       [(pairT _ t) t]
       [ _ (error "Expression is not a pair")])]

    ;; Add1
    [(Add1 e1)
     (def t1 (typecheck-expr e1 env funs))
     (begin
       (operand-type-test 'add1 (numT) t1)
       (numT))]

    ;; Add
    [(Add e1 e2)
     (def t1 (typecheck-expr e1 env funs))
     (def t2 (typecheck-expr e2 env funs))
     (begin
       (operand-type-test '+ (numT) t1)
       (operand-type-test '+ (numT) t2)
       (numT))]

    ;; Sub
    [(Sub e1 e2)
     (def t1 (typecheck-expr e1 env funs))
     (def t2 (typecheck-expr e2 env funs))
     (begin
       (operand-type-test '- (numT) t1)
       (operand-type-test '- (numT) t2)
       (numT))]

    ;; Mul
    [(Mul e1 e2)
     (def t1 (typecheck-expr e1 env funs))
     (def t2 (typecheck-expr e2 env funs))
     (begin
       (operand-type-test '* (numT) t1)
       (operand-type-test '* (numT) t2)
       (numT))]

    ;; Div
    [(Div e1 e2)
     (def t1 (typecheck-expr e1 env funs))
     (def t2 (typecheck-expr e2 env funs))
     (begin
       (operand-type-test '/ (numT) t1)
       (operand-type-test '/ (numT) t2)
       (numT))]

    ;; Eq
    [(Eq e1 e2)
     (def t1 (typecheck-expr e1 env funs))
     (def t2 (typecheck-expr e2 env funs))
     (begin
       (operand-type-test '= (numT) t1)
       (operand-type-test '= (numT) t2)
       (boolT))]

    ;; Lt
    [(Lt e1 e2)
     (def t1 (typecheck-expr e1 env funs))
     (def t2 (typecheck-expr e2 env funs))
     (begin
       (operand-type-test '< (numT) t1)
       (operand-type-test '< (numT) t2)
       (boolT))]
       
    ;; Not
    [(Not e1)
     (def t1 (typecheck-expr e1 env funs))
     (begin
       (operand-type-test '! (boolT) t1)
       (boolT))]

    ;; And
    [(And e1 e2)
     (def t1 (typecheck-expr e1 env funs))
     (def t2 (typecheck-expr e2 env funs))
     (begin
       (operand-type-test '&& (boolT) t1)
       (operand-type-test '&& (boolT) t2)
       (boolT))]

    ;; Or
    [(Or e1 e2)
     (def t1 (typecheck-expr e1 env funs))
     (def t2 (typecheck-expr e2 env funs))
     (begin
       (operand-type-test '|| (boolT) t1)
       (operand-type-test '|| (boolT) t2)
       (boolT))]

    ;; If
    [(If e1 e2 e3)
     (def t1 (typecheck-expr e1 env funs))
     (def t2 (typecheck-expr e2 env funs))
     (def t3 (typecheck-expr e3 env funs))
     (cond
       [(not (boolT? t1)) (arg-type-error (boolT) t1)]
       [(not (equal? t2 t3)) (error "Branches must have same type")]
       [else t2])]

    ;; With (typed)
    [(With (typedId id id-type) e1 e2) 
     (def type  (typecheck-expr e1 env funs))
     (cond
       [(equal? type id-type) 
        (def extended-env (extend-env id type env))
        (def t2 (typecheck-expr e2 extended-env funs))
        t2]
       [else (arg-type-error id-type type)])]

    ;; With (untyped)
    [(With x e1 e2)
     (def t1 (typecheck-expr e1 env funs))
     (def extended-env (extend-env x t1 env))
     (def t2 (typecheck-expr e2 extended-env funs))
     t2]

    ;; App
    [(App f es)
     ;; number of arguments passed to the function
     (def args-num (length es))
     (def fun (lookup-fundef f funs))
     ;; Compares the types of argumens with those of the formal
     ;; parameters
     (def typecheck-arg (λ (p)
                          (def (list param arg) p)
                          (def param-type (typedId-type param))
                          (def arg-type (typecheck-expr arg env funs))
                          (if (equal? arg-type param-type)
                              true
                              (arg-type-error param-type arg-type))))
     
     ;; Verifies the number of arguments equals the function arity.
     (def test-arity (λ (arity)
                       (if (equal? arity args-num)
                           #t (arity-error f arity args-num))))

     
     (def (fundef _ type params _ _) fun)
     
     (def param-arg-list (map list params es))
     
     (begin
       (map typecheck-arg param-arg-list)
       (test-arity (length params))
       (if (equal? type (anyT))
           (typecheck-fundef fun funs)
           type))]
       
    [_ (error "not yet implemented")]
    ))





;; typecheck :: Program -> Type
;; Returns the type of a program. 
(define (typecheck p)
  (def (prog funs main) p)
  (begin
    (typecheck-fundef-list funs funs)
    (typecheck-expr main empty-env funs)))


;; interp :: Expression Env (ListOf Fundef) -> Value
(define (interp e env funs)
  (match e
    
    [(Num n) (numV n)]
    
    [(Id x) (env-lookup x env)]
    
    [(Bool b) (boolV b)]
    
    [(Cons e1 e2)
     (def left-val  (interp e1 env funs))
     (def right-val (interp e2 env funs))       
     (pairV left-val right-val)]
    
    [(Fst e1)
     (def (pairV v1 _) (interp e1 env funs))
     v1]
    
    [(Snd e1)
     (def (pairV _ v1) (interp e1 env funs))
     v1]
    
    [(Add1 e1)
     (def (numV n) (interp e1 env funs))
     (numV (+ 1 n))]
    
    [(Add e1 e2)
     (def (numV n1) (interp e1 env funs))
     (def (numV n2) (interp e2 env funs))
     (numV (+ n1 n2))]

    [(Sub e1 e2)
     (def (numV n1) (interp e1 env funs))
     (def (numV n2) (interp e2 env funs))
     (numV (- n1 n2))]

    [(Mul e1 e2)
     (def (numV n1) (interp e1 env funs))
     (def (numV n2) (interp e2 env funs))
     (numV (* n1 n2))]

    [(Div e1 e2)
     (def (numV n1) (interp e1 env funs))
     (def (numV n2) (interp e2 env funs))
     (numV (/ n1 n2))]
    
    [(Lt e1 e2)
     (def (numV n1) (interp e1 env funs))
     (def (numV n2) (interp e2 env funs))
     (boolV (< n1 n2))]
    
    [(Eq e1 e2)
     (def (numV n1) (interp e1 env funs))
     (def (numV n2) (interp e2 env funs))
     (boolV (= n1 n2))]
    
    [(Not e1)
     (def (boolV b) (interp e1 env funs))
     (boolV (not b))]
    
    [(And e1 e2)
     (def (boolV b1) (interp e1 env funs))
     (def (boolV b2) (interp e2 env funs))
     (boolV (and b1 b2))]
    
    [(Or e1 e2)
     (def (boolV b1) (interp e1 env funs))
     (def (boolV b2) (interp e2 env funs))
     (boolV (or b1 b2))]
    
    [(If e1 e2 e3)
     (def (boolV b) (interp e1 env funs))
     (if b (interp e2 env funs)
         (interp e3 env funs))]

    ;; With
    [(With x e b)
     (def named-expr-val (interp e env funs))
     (match x
       [(or (typedId id _) id)
        (def extended-env (extend-env id named-expr-val env))
        (def body-val (interp b extended-env funs))
        body-val])]

    ;; App 
    [(App f expr-list)

     
     (def (fundef _ _ params body contracts) (lookup-fundef f funs))
     
     (def interp-expr-list (λ (arg-expr) (interp arg-expr env funs)))
     
     (def arg-val-list (map interp-expr-list expr-list))
     
     (def fun-env (build-fun-env params arg-val-list empty-env))
     
     (def interp-contract
       (λ (con)
         (def (argContract con-id con-type con-pred) con)
         (def (fundef _ _ _ pred-body _) (lookup-fundef con-pred funs))
         (def v (interp pred-body fun-env funs) )
         (def arg-val (interp (Id con-id) fun-env funs))
         (if (equal? v (boolV #t))
             #t
             (contract-err arg-val  con-pred))))

     (begin
       (map interp-contract contracts)
       (interp body fun-env funs))]
    
    [_ (error "not yet implemented")]))



;; run :: Program -> Value
;; Evaluates a program
(define (run sp)
  (def (prog funs main) (parse sp))
  (begin
    (typecheck (prog funs main))
    (interp main empty-env funs)))
