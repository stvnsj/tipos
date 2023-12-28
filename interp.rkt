#lang play
(require "utils.rkt")
(require "deftype.rkt")
(require "env.rkt")

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
         (def pred-val (interp pred-body fun-env funs) )
         (def arg-val (interp (Id con-id) fun-env funs))
         (if (equal? pred-val (boolV #t))
             #t
             (contract-err arg-val con-pred))))

     (begin
       (map interp-contract contracts)
       (interp body fun-env funs))]
    
    [_ (error "not yet implemented")]))

