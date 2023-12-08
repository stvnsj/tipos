#lang play

(require "env.rkt")

;; ======================
;;    Utility functions
;; ======================



;; arity-error :: Raises and exception, for different arity and number
;; of arguments in function application.
(define (arity-error f params args)
  (error
   (format "Arity error: function ~a expected ~a arguments but got ~a" f params args)))


;; Parte 1

#|
<prog>   ::= {<fundef>* <expr>}

<fundef> ::= {define {<id> <id>*} <expr>}

<expr>   ::= <num>
           | <id>
           | <bool>
           | {cons <expr> <expr>}
           | {add1 <expr>}
           | {+ <expr> <expr>}
           | {< <expr> <expr>}
           | {= <expr> <expr>}
           | {! <expr> <expr>}
           | {&& <expr> <expr>}
           | {|| <expr> <expr>}
           | {fst <expr>}
           | {snd <expr>}
           | {if <expr> <expr> <expr>}
           | {with {{<id> <expr>}*} <expr>}
           | {<id> <expr>*}
|#






(deftype Prog
  (prog fundefs main))

(deftype Fundef
  (fundef name arg body))

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

(define (lookup-fundef f funs)
  (match funs
    ['() (error 'lookup-fundef "function not found: ~a" f)]
    [(cons (and fd (fundef fn _ _)) rest)
     (if (symbol=? fn f)
         fd
         (lookup-fundef f rest))]))

;; tipo inductivo para los valores del lenguaje
(deftype Val
  (numV n)
  (boolV b)
  (pairV lV rV))

;; parse :: ...
(define (parse sp)
  (match sp
    [(list ds ... e) (prog (map parse-fundef ds) (parse-expr e))] ;; ds es la lista de definiciones, e es la expresion principal
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
       [(cons (list x b) bs) ;; non-empty list
        (def named-expr (parse-expr b))
        (def body-expr (parse-expr (list 'with bs e))) 
        (With x named-expr body-expr )]       
       [(list) (parse-expr e)])] ;; empty list
    
    [(cons f ses)
     (App f (map parse-expr ses))]
    
    [_ (error "not yet implemented")]
    ))


(define (parse-fundef sf)
  (match sf
    [(list 'define (cons f args) body)
     (fundef f args (parse-expr body))]
    [_ (error "not yet implemented")]))


;; interp :: ...
(define (interp e env funs)
  (match e
    
    [(Num n) (numV n)]
    [(Id x) (env-lookup x env)]
    [(Bool b) (boolV b)]
    [(Cons e1 e2) ;; =============================   
     (def left-val  (interp e1 env funs))
     (def right-val (interp e2 env funs))       
     (pairV left-val right-val)]
    [(Fst e1) ;; =================================
     (def (pairV v1 _) (interp e1 env funs))
     v1]
    [(Snd e1) ;; =================================
     (def (pairV _ v1) (interp e1 env funs))
     v1]
    [(Add1 e1) ;; ================================
     (def (numV n) (interp e1 env funs))
     (numV (+ 1 n))]
    [(Add e1 e2) ;; ==============================
     (def (numV n1) (interp e1 env funs))
     (def (numV n2) (interp e2 env funs))
     (numV (+ n1 n2))]
    [(Lt e1 e2) ;; ===============================
     (def (numV n1) (interp e1 env funs))
     (def (numV n2) (interp e2 env funs))
     (boolV (< n1 n2))]
    [(Eq e1 e2) ;; ===============================
     (def (numV n1) (interp e1 env funs))
     (def (numV n2) (interp e2 env funs))
     (boolV (= n1 n2))]
    [(Not e1) ;; =================================
     (def (boolV b) (interp e1 env funs))
     (boolV (not b))]
    [(And e1 e2) ;; ==============================
     (def (boolV b1) (interp e1 env funs))
     (def (boolV b2) (interp e2 env funs))
     (boolV (and b1 b2))]
    [(Or e1 e2) ;; ===============================
     (def (boolV b1) (interp e1 env funs))
     (def (boolV b2) (interp e2 env funs))
     (boolV (or b1 b2))]
    [(If e1 e2 e3) ;; ============================
     (def (boolV b) (interp e1 env funs))
     (def true-val  (interp e2 env funs))
     (def false-val (interp e3 env funs))
     (if b true-val false-val)]
    [(With x e b) ;; =============================
     (def named-expr-val (interp e env funs))
     (def extended-env (extend-env x named-expr-val env))
     (def body-val (interp b extended-env funs))
     body-val]
    [(App f expr-list) ;; ========================
     (def (fundef fun params body) (lookup-fundef f funs))
     (def arity (length params))
     (def arg-num (length expr-list))
     (cond 
       [(= arity arg-num)
        (def interp-expr-list (λ (arg-expr) (interp arg-expr env funs)))
        (def arg-val-list (map interp-expr-list expr-list))
        (def fun-env (build-fun-env params arg-val-list empty-env))
        (interp body fun-env funs)]
       [else (arity-error fun arity arg-num)])]
    
    [_ (error "not yet implemented")]
    ))


(define (run sp)
  (def (prog funs main) (parse sp))
  (interp main empty-env funs))
