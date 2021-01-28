#lang plai-typed

(define-type MisspelledAnimal
  [caml (humps : number)]
  [yacc (height : number)])

(caml 2)

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define (interp [a : ArithC]) : number
    (type-case ArithC a
      [numC (n) n]
      [plusC (l r) (+ (interp l) (interp r))]
      [multC (l r) (* (interp l) (interp r))]))

(test (interp (plusC (numC 2)
                     (numC 3))) 5)

(test (interp (multC
               (plusC (numC 2) (numC 3))
               (multC (numC 1) (numC 2)))) 10)
