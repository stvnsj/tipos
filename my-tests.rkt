

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

Para los errores:

- Los errores de identificadores libres (o
funciones no definidas). Este error debe
detectarse estáticamente.

- Los mensajes de error de tipo detectados
estáticamente tienen que seguir el siguiente
patrón:

-----|
P3   |
-----|

- En el intérprete, cuando se efectúa una
aplicación de función, se debe que verificar que
los argumentos cumplan con los contratos
declarados (en caso de que existan).  

- Cuando el contrato no se cumpla, se debe lanzar
un error ...

- Una función usada como contrato debe aceptar un
solo argumento de cualquier tipo válido y debe
retornar un valor de tipo Bool. En caso de no
cumplir esta condición se debe lanzar un error ...

|#




#lang play
(require "run.rkt")
(require "deftype.rkt")


(define-syntax run-test
  (syntax-rules ()
    ((run-test bool program expected)
     (if bool (test (run program) expected) (displayln ">> Test Omitted\n")))
    ((run-test bool description program expected)
     (if bool (begin
                (displayln (format "Description: ~a" description))
                (test (run program) expected))
         (displayln ">> Test Omitted\n")))))



(run-test #f "This is some description" '{
               {define {f {x : Num}} : Num {with {{y 1}} {+ x y}}}
               {with {{y : Num 0}} {f 1}}
               }
          (numV 2))

(run-test #f '{{define {xor {p : Bool} {q : Bool}} : Bool
                 { || { && p {! q}} {&& {! p} q}}}
               {xor #t #f}
               }
          (boolV #t))

(run-test #f '{{define {fib {n : Num}} : Num
                 {if {= n 0} 0
                     {if {= n 1} 1
                         {+ {fib {- n 1}} {fib {- n 2}}}}}}
               {fib 8}}
          (numV 21))



;; =========== Contract Tests ====================

(run-test #t '{

               {define {positive {n : Num}} : Bool {< 0 n}}
               {define {pitagorean-triple {a : Num @ positive}
                                          {b : Num }
                                          {c : Num }} : Bool
                                          {= {+ {* a a} {* b b}} {* c c}}}



               {pitagorean-triple 3 4 5}
               
               }
          (boolV #t))




