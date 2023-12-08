#lang play

;;;;
#|
<fundef> ::= {define {<id> {arg}*} [: <type>] <expr>}

<arg>    ::= {<id> : <type>}

<expr>   ::= ... | {with { {<id> [: <type>] <expr>}* } <expr>}  ; los otros casos no cambian

<type>   ::= Num | Bool | {Pair <type> <type>}
|#





(define (parse p)
  (error "replace parse with your own implementation"))


(deftype Type
  (numT)
  (boolT)
  (pairT lT rT))



;; typecheck-expr :: ...
(define (typecheck-expr e)
  (match e
    [(num n) (numT)]
    [(bool b) (boolT)]
    ; ...
    [_ (error "not yet implemented")]
    ))

;; typecheck-fundef :: ...
(define (typecheck-fundef f)
  ; ...
  (error "not yet implemented"))

;; typecheck :: ...
(define (typecheck p)
  (def (prog funs main) p)
  (begin
    (map typecheck-fundef funs)
    (typecheck-expr main)))

