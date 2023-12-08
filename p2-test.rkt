#lang play

(require "p2.rkt")

(test (typecheck (prog '() (num 5))) (numT))
(test (typecheck (prog '() (bool #t))) (boolT))



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


