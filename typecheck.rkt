#lang play

(require "deftype.rkt")
(require "utils.rkt")
(require "env.rkt")





;; ==============================================
;;              TYPECHECK FUNCTIONS
;; ==============================================



;; Builds an environment whose pairs have function name as id and
;; function type as value.
(define (typecheck-fundef-list fenv funs)
  (match funs
    [(list) fenv]
    [(cons f fs)
     (def f-name (fundef-name f))
     (cond
       [(env-contains f-name fenv) (typecheck-fundef-list fenv fs)]
       [else
        (def (list _ fenv1) (typecheck-fundef f fenv funs))
        (typecheck-fundef-list fenv1 fs)])]))




;; typecheck-fundef :: Fundef Env (ListOf Fundef) -> Type
(define (typecheck-fundef f fenv funs)
  (def (fundef f-name f-type f-params f-body _) f)
  (def env (build-type-env f-params empty-env))
  (def hhh (if (not (equal? f-type (anyT))) (extend-env f-name f-type fenv) fenv))
  (def (list body-type fenv0) (typecheck-expr f-body env hhh funs))
  (def fenv1 (extend-env f-name body-type fenv0))
  (begin
    (ret-type-test f-name f-type body-type)
    (list body-type fenv1)))



(define (typecheck-expr-list e-list t-list env fenv funs)
  (match e-list
    [(cons e es)
     (def (list t fenv1) (typecheck-expr e env fenv funs))
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

    [(And e1 e2)
     (def (list t1  fenv1) (typecheck-expr e1 venv fenv funs))
     (def (list t2  fenv2) (typecheck-expr e2 venv fenv1 funs))
     (begin
       (operand-type-test '&& (boolT) t1)
       (operand-type-test '&& (boolT) t2)
       (list (boolT) fenv2))]

    [(Or e1 e2)
     (def (list t1 fenv1) (typecheck-expr e1 venv fenv funs))
     (def (list t2 fenv2) (typecheck-expr e2 venv fenv1 funs))
     (begin
       (operand-type-test '|| (boolT) t1)
       (operand-type-test '|| (boolT) t2)
       (list (boolT) fenv2))]

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
       
       ;; Named expression can be any type or has
       ;; correct type.
       [(or (equal? id-type (anyT)) (equal? id-type type))
        (def extended-env (extend-env id type venv))
        (def (list t2 fenv2) (typecheck-expr e2 extended-env fenv1 funs))
        (list t2 fenv2)]
       
       ;; wrong type!
       [else (arg-type-error id-type type)])]




    [(App f arguments)
     (def fun (lookup-fundef f funs))
     (def (fundef _ _ params _ _) fun)
     (def arity    (length params))
     (def args-num (length arguments))
     (def (list t-list fenv1) (typecheck-expr-list arguments '() venv fenv funs))

     (def test-arg-type (λ (param arg-type)
                          (def (typedId _ id-type) param)
                          (if (equal? id-type arg-type) #t
                              (arg-type-error id-type arg-type))))
     (def test-arity    (λ ()
                          (if (equal? arity args-num) #t
                              (arity-error f arity args-num))))
     (begin
       (test-arity)
       (map test-arg-type params t-list)
       (cond
         [(env-contains f fenv1) (list (env-lookup f fenv1) fenv1)]
         [else (typecheck-fundef fun fenv1 funs)]))]
    
    [_ (error "not yet implemented")]))




;; typecheck-contract :: ArgContract (ListOf Fundef) -> Bool/err
(define (typecheck-contract con fenv funs)

  (def (argContract _ arg-type predicate) con) ; unpack contract 
  (def con-def (lookup-fundef predicate funs)) ; predicate fundef
  (def (fundef _ _ args _ _) con-def) ; unpack predicate fundef
  (def arity (length args)) ; predicate arity
  (def type  (env-lookup (argContract-predicate con) fenv)) ; return type of predicate

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

(define (typecheck-contract-list fun fenv funs)
  (def contract-list (fundef-contracts fun))
  (map (λ (con) (typecheck-contract con fenv funs)) contract-list))
         



(define (typecheck p)
  (def (prog funs main) p)
  (def fenv (typecheck-fundef-list empty-env funs))  
  (def (list type _) (typecheck-expr main empty-env fenv funs))
  (begin 
    (map (λ (fun) (typecheck-contract-list fun fenv funs)) funs)
    type))
