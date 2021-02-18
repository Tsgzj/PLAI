#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [lamC (arg : symbol) (body : ExprC)]
  [boxC (arg : ExprC)]
  [unboxC (arg : ExprC)]
  [setboxC (b : ExprC) (v : ExprC)]
  [seqC (b1 : ExprC) (b2 : ExprC)])

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [boxV (l : Location)])

(define-type-alias Location number)

(define-type Binding
  [bind (name : symbol) (val : Location)])

(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

(define-type Storage
  [cell (location : Location) (val : Value)])

(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)

(define (lookup [sym : symbol] [env : Env]) : Location
   (cond
    [(empty? env) (error 'lookup "failed to find symbol")]
    [else (cond
            [(equal? sym (bind-name (first env))) (bind-val (first env))]
            [else (lookup sym (rest env))])]))

(define (fetch [loc : Location] [sto : Store]) : Value
   (cond
    [(empty? sto) (error 'fetch "failed to find location")]
    [else (cond
            [(equal? loc (cell-location (first sto))) (sell-val (first sto))]
            [else (fetch loc (rest sto))])]))

(define (interp [expr : ExprC] [env : Env]) : Value
  (type-case ExprC expr
             [numC (n) (numV n)]
             [idC (n) (lookup n env)]
             [appC (f a) (local ([define f-value (interp f env)])
                           (interp (closV-body f-value)
                                   (extend-env (bind (closV-arg f-value)
                                                     (interp a env))
                                               (closV-env f-value))))]
             [plusC (l r) (num+ (interp l env) (interp r env))]
             [multC (l r) (num* (interp l env) (interp r env))]
             [lamC (a b) (closV a b env)]
             [boxC (a) (boxV (interp a env))]
             [unboxC (a) (boxV-V (interp a env))]
             <setboxC-case>
             <seqC-case>))
