

#|

Testing Scheme
==============


─────|
P2   |
─────|

- Las declaraciones de función incluyen
anotaciones de tipos en cada uno de sus argumentos
y también para el tipo de retorno.

- Las expresiones with incluyen anotaciones de
tipos en cada identificador introducido.

- Para una definición de función, se valida que la
expresión del cuerpo tenga el mismo tipo que el
tipo de retorno declarado, suponiendo que cada
argumento tiene el tipo declarado. Si el tipo de
retorno no se especifica, entonces se usa el tipo
del cuerpo.

- Debe definir los tipos de los operadores
primitivos de manera exacta, p.ej. < es una
operación que toma dos Num y retorna Bool.

- Considere que la igualdad (=) solo puede
comparar números.

- Para una expresión if la condición debe tener
tipo Bool y ambas ramas deben tener el mismo tipo
t. El tipo resultante de la expresión if es t.

- Para with se verifica que todos los argumentos
cumplan con el tipo declarado y el tipo resultante
será el del cuerpo de la expresión. Si los
identificadores no tienen tipo explícito, entonces
se les asigna el de la expresión asociada.

- En la aplicación de función se valida que el
número de argumentos coincide, y que el tipo de
los argumentos coincide con los tipos esperados de
la función aplicada. El tipo resultante de una
aplicación es el tipo de retorno de la función
aplicada.

|#




#lang play
(require "run.rkt")
(require "deftype.rkt")


(define-syntax test-macro
  (syntax-rules ()
    ((test-macro bool program expected)
     (if bool (test (run program) expected) (displayln ">> Test Omitted")))

    ;; If exc argument is given, an error test is run
    ((test-macro bool exc program expected)
     (if bool (test/exn (run program) expected) (displayln ">> Test Omitted")))))
    


    




(test-macro #f '{
               {define {f {x : Num}} : Num {with {{y 1}} {+ x y}}}
               {with {{y : Num 0}} {f 1}}
               }
          (numV 2))

(test-macro #f '{{define {xor {p : Bool} {q : Bool}} : Bool
                 { || { && p {! q}} {&& {! p} q}}}
               {xor #t #f}
               }
          (boolV #t))

(test-macro #f '{{define {fib {n : Num}} : Num
                 {if {= n 0} 0
                     {if {= n 1} 1
                         {+ {fib {- n 1}} {fib {- n 2}}}}}}
               {fib 8}}
          (numV 21))




;; Different branch types.
(test-macro #t 1
            '{{define {and3 {b1 : Bool} {b2 : Bool} {b3 : Bool}} : Bool {&& b1 {&& b2 b3}}}
              {define {add3 {n1 : Num} {n2 : Num} {n3 : Num}} {+ n1 {+ n2 n3}}}
              {if #t {and3 #t #t #f} {add3 4 1 -5}}}
            "Static type error: branches must have same type")





;; =========== Contract Tests ====================

;; All arguments satisfy contract.
(test-macro #f
          '{{define {positive {n : Num}} : Bool {< 0 n}}
            {define {pitagorean-triple {a : Num @ positive}
                                       {b : Num @ positive}
                                       {c : Num @ positive}} : Bool
              {= {+ {* a a} {* b b}} {* c c}}}
            {pitagorean-triple 3 4 5}}
          (boolV #t))

;; Contract is not satisfied by -5.
(test-macro #f 1
          '{{define {positive {n : Num}} : Bool {< 0 n}}
            {define {pitagorean-triple {a : Num @ positive}
                                       {b : Num @ positive}
                                       {c : Num @ positive}} : Bool
              {= {+ {* a a} {* b b}} {* c c}}}
            {pitagorean-triple 3 4 -5}}
          "Runtime contract error: -5 does not satisfy positive")

;; Contract has bad type; it takes more than one argument.
(test-macro #f 1
            '{{define {pred1 {x : Num} {y : Num}} {< {+ x y} 0}}
              {define {fun1  {n : Num @ pred1}} n}
              {fun1 2}}
            "Static contract error: invalid type for pred1")


;; Contract has bad type; it returns pair of Number Boolean.
(test-macro #f 1
            '{{define {positive {n : Num}} : Bool {< 0 n}}
              {define {pred1 {x : Num}} : {Pair Num Bool} {cons x {positive x}}}
              {define {fun1  {b : Bool} {n : Num @ pred1}} {if b n {* -1 n}}}
              {fun1 #t 1}}
            "Static contract error: invalid type for pred1")








