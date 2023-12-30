
#lang play

;; ====================
;;   Data Structures
;; ====================

;; Expression
(deftype Expr
  (Num n)
  (Id x)
  (Bool b)
  (Cons f s)
  (Fst p)
  (Snd p)
  (Add1 n)
  (Add l r)
  (Sub l r)
  (Mul l r)
  (Div l r)
  (Lt l r)
  (Eq l r)
  (Not b)
  (And l r)
  (Or l r)
  (If c t f)
  (With x e b)
  (App f e))

;; Program
(deftype Prog
  (prog funs main))

;; Function Definition
(deftype Fundef
  (fundef name type arg body contracts))

;; Formal Parameter
(deftype TypedId
  (typedId id type))

;; Contract (of a function argument)
(deftype ArgContract
  (argContract id type predicate))

;; Type
(deftype Type
  (anyT)
  (numT)
  (boolT)
  (pairT lT rT))

;; Value
(deftype Val
  (numV n)
  (boolV b)
  (pairV lV rV))
