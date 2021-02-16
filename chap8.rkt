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
  [boxV (v : Value)])

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
