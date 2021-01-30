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

(define-type ArithS
  [numS (n : number)]
  [uminusS (e : ArithS)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)])

(define (desugar [as : ArithS]) : ArithC
  (type-case ArithS as
             [numS (n) (numC n)]
             [uminusS (n) (multC (numC -1) (desugar n))]
             [plusS (l r) (plusC (desugar l)
                                 (desugar r))]
             [multS (l r) (multC (desugar l)
                                 (desugar r))]
             [bminusS (l r) (plusC (desugar l)
                                   (multC (numC -1) (desugar r)))]))

(test (interp (desugar (bminusS (numS 2) (numS 1)))) 1)
(test (interp (desugar (uminusS (numS 2)))) -2)

(define-type BooleanC
  [boolC (b : boolean)])

(define (condition [b : BooleanC]) : boolean
  (type-case BooleanC b
             [boolC (b) b]))

(test (condition (boolC #t)) #t)
(test (condition (boolC #f)) #f)
