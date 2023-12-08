
#lang play

;;;;
#|  COPIE Y PEGUE SU CODIGO DE LA PREGUNTA DOS   |#
#| LUEGO MODIFIQUELO SIGUIENDO LAS INSTRUCCIONES |#
;;;;

;;;; luego de copiar su codigo elimine la definiciones de run y val
(define (run p) (error "not yet implemented"))
(deftype Val
  (numV n)
  (boolV b)
  (pairV lV rV))

#|
<fundef> ::= {define {<id> {arg}*} [: <type>] <expr>} ; como antes

<arg>    ::= {<id> : <type>} ; como antes
           | {<id> : <type> @ <contract>}  ; lo único nuevo

<expr>   ::= ... | {with { {<id> [: <type>] <expr>}* } <expr>}  ; los otros casos no cambian

<type>   ::= Num | Bool | {Pair <type> <type>}
|#


