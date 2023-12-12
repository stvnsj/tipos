
#lang play

;; Interpreter compatible with p{2,3}-test.rkt 
(require "p3.rkt")

;; ============ P1 Tests ================

(test (run '{5}) (numV 5))
(test (run '{#t}) (boolV #t))


(test (run '{ ;; Programa de Ejemplo 1
             {define {sum {x : Num} {y : Num} {z : Num}} : Num {+ x {+ y z}}}
             {define {cadr {x : {Pair Num {Pair Num Num}}}} : Num  {fst {snd x}}}
             {with {{x 9} {y {cons 1 {cons 3 4}}}}
                   {sum x {fst y} {cadr y}} }
             })
      (numV 13))



(test (run '{ ;; Programa de Ejemplo 2
             {with {{x 5} {y 23} {z : {Pair Num Num} {cons 11 -3}}}
                   z}
             })
      (pairV (numV 11) (numV -3)))



(test (run '{ ;; Programa de Ejemplo 3
             {define {triple {x : Num}} : Num {+ x {+ x x}}}
             {define {add2 {x : Num}} {+ 2 x}}
             {add2 {triple 2}}
             })
      (numV 8))



(test (run '{ ;; Programa de Ejemplo 4
             {with {{x 3} {y {+ 1 2}}}
                   {if {= x y} x y}}
             })
      (numV 3))



;; ============ P2 Tests ================

(test (typecheck (prog '() (Num 5))) (numT))
(test (typecheck (prog '() (Bool #t))) (boolT))

(test (typecheck (parse '{ ; Programa de ejemplo 1
                          {with {{x : Num 5} {y : Num 10}}
                                {+ x y}}
                          }))
      (numT))

(test (typecheck (parse '{ ; Programa de ejemplo 2
                          {with {{x 5}}
                                {with {{y : Num {+ x 1}}}
                                      {+ x y}}
                                }}))
      (numT))

(test (typecheck (parse '{ ; Programa de ejemplo 3
                          {define {add-pair {p : {Pair Num Num}} {x : Num}} : {Pair Num Num}
                            {cons {+ {fst p} x} {+ {snd p} x}}}
                          {add-pair {cons 1 1} 1}
                          }))
      (pairT (numT) (numT)))

(test (typecheck (parse '{ ; Programa de ejemplo 4
                          {define {id {x : Num}} x}
                          {id 5}
                          }))
      (numT))

(test (typecheck (parse '{ ; Programa de ejemplo 5
                          {define {sum {x : Num} {y : Num} {z : Num}}
                            {+ x {+ y z}}}
                          {define {cadr {x : {Pair Num {Pair Num Num}}}} : Num
                            {fst {snd x}}}
                          {with {{x 9} {y {cons 1 {cons 3 4}}}}
                                {sum x {fst y} {cadr y}} }
                          }))
      (numT))

(test (typecheck (parse '{3})) (numT))

(test  (typecheck (parse '{{define {f {p : Bool}} {if p 23 42}}
                     {f {< 3 4}}}))
        (numT))

(test/exn (typecheck (parse '{{define {one {x : Num}} 1}
                       {one #t}}))
          "Static type error: expected Num found Bool")

(test/exn (typecheck (parse '{{< 10 #t}}))
          "Static type error: operator < expected Num found Bool")

(test/exn (typecheck (parse '{{if 73 #t #t}}))
          "Static type error: expected Bool found Num")

(test/exn (typecheck (parse '{{with {{x : Num 5} {y : Num #t} {z : Num 42}}
                             z}}))
          "Static type error: expected Num found Bool")




;; ============ P3 Tests =================

(test (run '{{define {positive {x : Num}} {< 0 x}}
             {define {sub {x : Num @ positive} {y : Num}} : Num {- x y}}
             {sub 5 3}})
      (numV 2))

(test (run '{{define {positive {x : Num}} {< 0 x}}
             {define {negate {x : Num @ positive}} {- 0 x}}
             {negate 23}})
      (numV -23))

(test/exn (run '{{define {pair-non-zero? {p : {Pair Num Num}}} {!{= 0 {snd p}}}}
                 {define {pair-div {p : {Pair Num Num} @ pair-non-zero?}} {/ {fst p} {snd p}}}
                 {+ {pair-div {cons 30 5}} {pair-div {cons 60 0}}}
                 })
          "Runtime contract error: (60,0) does not satisfy pair-non-zero?")

(test/exn (run '{{define {add {x : Num} {y : Num}} {+ x y}}
                 {define {oh-no {x : Num @ add}} x}
                 {oh-no 21 21}})
          "Static contract error: invalid type for add")


