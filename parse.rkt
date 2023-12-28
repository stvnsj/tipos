
#lang play
(require "utils.rkt")
(require "deftype.rkt")


(define (pp name obj)
  (displayln (format "-------------------------------------\n~a :: ~a\n" name obj)))


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
