#lang play
(require "env.rkt")

;;;;
#|
<fundef> ::= {define {<id> {arg}*} [: <type>] <expr>}
<arg>    ::= {<id> : <type>}
<expr>   ::= ... | {with { {<id> [: <type>] <expr>}* } <expr>}
<type>   ::= Num | Bool | {Pair <type> <type>}
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
  (Lt l r)
  (Eq l r)
  (Not b)
  (And l r)
  (Or l r)
  (If c t f)
  (With x e b)
  (App f e)
  )

;; Type
(deftype Type
  (numT)
  (boolT)
  (pairT lT rT))

;; Program
(deftype Prog
  (prog funs main))

;; Function Definition
(deftype Fundef
  (fundef  name arg body)
  (typedFundef name type arg body)) ;; function with type declaration

;; Formal Parameter
(deftype TypedId
  (typedId id type))


;; operand-type-test
(define (operand-type-test op-sym exp-type actual-type)
  (if (equal? exp-type actual-type)
      1 (operand-type-error op-sym exp-type actual-type)))

;; wrong-arg-error ::
(define (arg-type-error param-type arg-type)
  (define t1 (type-to-sym param-type))
  (define t2 (type-to-sym arg-type))
  (error (format "Static type error: expected ~a found ~a" t1 t2)))


;; wrong-type-error ::
(define (operand-type-error op-sym exp-type actual-type)
  (define exp-sym    (type-to-sym exp-type))
  (define actual-sym (type-to-sym actual-type))
  (error (format "Static type error: operator ~a expected ~a found ~a" op-sym exp-sym actual-sym)))



;; build-typed-env :: (List TypedId) Env -> Env
;; Takes a list of typed ids and an environment, and returns an
;; extended environment with the id-type pairs in the list.
(define (build-type-env typed-params  env)
  (match typed-params
    [(cons (typedId id type) rest)
     (def extended-env (extend-env id type env))
     (build-type-env rest extended-env)]
    [(list) env]))


;; lookup-fundef :: 
;; looks up the requested function by its identifier among the defined
;; functions
(define (lookup-fundef f funs)
  (match funs
    ['() (error 'lookup-fundef "function not found: ~a" f)]
    [(cons (and fd (typedFundef fn _ _ _)) rest)
     (if (symbol=? fn f)
         fd
         (lookup-fundef f rest))]
    [(cons (and fd (fundef fn _ _)) rest)    ;; untyped function 
     (if (symbol=? fn f)
         fd
         (lookup-fundef f rest))]))


;; sym-to-type :: Translate type annotation into actual type.
(define (sym-to-type s)
  (match s
    ['Num (numT)]
    ['Bool (boolT)]
    [(list 'Pair ta1 ta2)
     (def t1 (sym-to-type ta1))
     (def t2 (sym-to-type ta2))
     (pairT t1 t2)]
    [_ (error (format "Error: There is no ~a type!" s))]))


(define (type-to-sym t)
  (match t
    [(numT)  'Num]
    [(boolT) 'Bool]
    [(pairT t1 t2) (list 'Pair (type-to-sym t1) (type-to-sym t2))]
    [_ (error "More types to be supported")]))





;; parse :: ...
(define (parse sp)
  (match sp
    [(list ds ... e) (prog (map parse-fundef ds) (parse-expr e))]
    ))

;; parse-expr :: ...
(define (parse-expr se)
  (match se
    [(? number?)              (Num se)]
    [(? symbol?)              (Id se)]
    [(? boolean?)             (Bool se)]
    [(list 'cons se1 se2)     (Cons (parse-expr se1) (parse-expr se2))]
    [(list 'add1 se1)         (Add1 (parse-expr se1))]
    [(list '+ se1 se2)        (Add (parse-expr se1) (parse-expr se2))]
    [(list '< se1 se2)        (Lt (parse-expr se1) (parse-expr se2))]
    [(list '= se1 se2)        (Eq (parse-expr se1) (parse-expr se2))]
    [(list '! se1)            (Not (parse-expr se1))]
    [(list '&& se1 se2)       (And (parse-expr se1) (parse-expr se2))]
    [(list '|| se1 se2)       (Or  (parse-expr se1) (parse-expr se2))]
    [(list 'fst se)           (Fst (parse-expr se))]
    [(list 'snd se)           (Snd (parse-expr se))]
    [(list 'if se1 se2 se3)   (If (parse-expr se1) (parse-expr se2) (parse-expr se3))]
    
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
    
    [_ (error "not yet implemented")]))



;; parse-fundef-params ::
;; params : list of parameters in function definition.
(define (parse-fundef-params params)
  (match params

    [(cons (list id ': type-annotation) rest)
     (def type  (sym-to-type type-annotation))
     (def param (typedId id type))
     (cons param (parse-fundef-params rest))]

    [(list) (list)]

    [_ (error "Bad function parameter")]))


;; parse-fundef :: ...
(define (parse-fundef sf)
  (match sf
    
    [(list 'define (cons fun params) body)
     (def typed-params (parse-fundef-params params))
     (def body-expr    (parse-expr body))
     (fundef fun typed-params body-expr)]
    
    [(list 'define (cons fun params) ': type-annotation body)
     (def fun-type     (sym-to-type type-annotation))
     (def typed-params (parse-fundef-params params))
     (def body-expr    (parse-expr body))
     (typedFundef fun fun-type typed-params body-expr)]))


;; typecheck-fundef-list :: ... -> 
;; fenv        : environment storing the (fun,type) pairs
;; funs        : list of funs left to be typechecked
;; fundef-list : complete list of function definitions
;;
;; Returns a pair with the updated fenv and the remaining functions to
;; be typechecked.
(define (typecheck-fundef-list funs fundef-list)
  (match funs
    [(cons f fs)
     (begin
       (typecheck-fundef f fundef-list)
       (typecheck-fundef-list fs fundef-list))]
    [(list) #t]))
                   


;; typecheck-fundef :: 
;; f    : function to be typechecked
;; funs : list of function definitions
(define (typecheck-fundef f funs)
  (match f
    [(fundef _ params body) ;; non-typed function
     (def env      (build-type-env params empty-env))
     (def fun-type (typecheck-expr body env funs))
     fun-type]
    [(typedFundef _ type params body)
     (def env      (build-type-env params empty-env))
     (def fun-type (typecheck-expr body env funs))
     (if (equal? type fun-type)
         fun-type
         (error "Wrong function type"))]))

  

;; typecheck-expr :: 
;; 
       
(define (typecheck-expr e env funs)
  (match e

    ;; Num 
    [(Num _) (numT)]
    
    ;; IMPORTANT: If [x] identifier is not found, this error is
    ;; statically catched during the type checking step.
    [(Id x)  (env-lookup x env)]

    ;; Bool
    [(Bool _) (boolT)]
    
    ;; Cons 
    [(Cons e1 e2) ;; ============================
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
    [(With (typedId id idt) e1 e2) 
     (def t1  (typecheck-expr e1 env funs))
     (cond
       [(equal? t1 idt) 
        (def extended-env (extend-env id t1 env))
        (def t2 (typecheck-expr e2 extended-env funs))
        t2]
       [else (arg-type-error idt t1)])]

    ;; With (untyped)
    [(With x e1 e2)
     (def t1 (typecheck-expr e1 env funs))
     (def extended-env (extend-env x t1 env))
     (def t2 (typecheck-expr e2 extended-env funs))
     t2]

    ;; App
    ;; IMPORTANT: If function identifier is not found, this error is
    ;; statically catched in the type checking step.
    [(App f es)

     (def fun (lookup-fundef f funs))
     (def typecheck-arg (λ (p)
                          (def (list param arg) p)
                          (def param-type (typedId-type param))
                          (def arg-type (typecheck-expr arg env funs))
                          (if (equal? arg-type param-type)
                              true
                              (arg-type-error param-type arg-type))))

     (match fun
       [(fundef _ params _) 
        (def param-arg-list (map list params es))
        (begin
          (map typecheck-arg param-arg-list)
          (typecheck-fundef fun funs))]
       
        [(typedFundef _ type params _)
         (def param-arg-list (map list params es))
         (begin
           (map typecheck-arg param-arg-list)
           type)])]
           

       

    [_ (error "not yet implemented")]
    ))




;; typecheck :: returns the type of a program
(define (typecheck p)
  (def (prog funs main) p)
  (begin
    (typecheck-fundef-list funs funs)
    (typecheck-expr main empty-env funs)))
