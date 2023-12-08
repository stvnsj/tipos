
#lang play

(require "p3.rkt")
(require "p2-test.rkt")

(test (run '{{define {positive {x : Num}} {< 0 x}}
             {define {sub {x : Num @ positive} {y : Num}} : Num
               {- y x}}
             {sub 5 3}})
      (numV 2))

(test (run '{{define {positive {x : Num}} {< 0 x}}
             {define {negate {x : Num @ positive}} {- 0 x}}
             {negate 23}})
      (numV -23))

(test/exn (run '{{define {pair-non-zero? {p : {Pair Num Num}}} {and {} {}}}
                 {define {pair-div {p : {Pair Num Num} @ pair-non-zero?}} {/ {fst x} {snd x}}}
                 {+ {pair-div {cons 30 5}} {pair-div {cons 60 0}}}
                 })
          "Runtime contract error: (60,0) does not satisfy pair-non-zero?")

(test/exn (run '{{define {add {x : Num} {y : Num}} {+ x y}}
                 {define {oh-no {x : Num @ add}} x}
                 {oh-no 21 21}})
          "Static contract error: invalid type for add")

