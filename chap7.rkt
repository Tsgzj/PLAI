#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))

(define-type Value
  [numV (n : number)]
  [funV (name : symbol) (arg : symbol) (body : ExprC)])

(define mt-env empty)

(define extend-env cons)

(define (lookup [s : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "failed to find symbol")]
    [else (cond
            [(equal? s (bind-name (first env))) (bind-val (first env))]
            [else (lookup s (rest env))])]))

(define (num-op [l : Value] [r : Value] [op : (number number -> number)]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (op (numV-n l) (numV-n r)))]
    [else
     (error 'num-op "one argument was not a number")]))

(define (num+ [l : Value] [r : Value])
  (num-op l r +))

(define (num* [l : Value] [r : Value])
  (num-op l r *))

(define (interp [e : ExprC] [env : Env]) : Value
  (type-case ExprC e
             [numC (n) (numV n)]
             [idC (n) (lookup n env)]
             [fdC (n a b) (funV n a b)]
             [appC (f a) (local ([define fd (interp f env)])
                                 (interp (funV-body fd)
                                         (extend-env (bind (funV-arg fd)
                                                           (interp a env))
                                                     mt-env)))]
             [plusC (l r) (num+ (interp l env) (interp r env))]
             [multC (l r) (num* (interp l env) (interp r env))]))

(test (interp (plusC (numC 10) (appC (fdC 'const5 '_ (numC 5)) (numC 10)))
              mt-env)
      (numV 15))

(test/exn (interp (appC (fdC 'f1 'x (appC (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))
                                          (numC 4)))
                        (numC 3))
                  mt-env)
          "name not found")
