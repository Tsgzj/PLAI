#lang plai-typed

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

(define (subst [what : number] [for : symbol] [in : ExprC]) : ExprC
  (subst-helper (numC what) for in))

(define (subst-helper [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
             [numC (n) in]
             [idC (s) (cond
                        [(symbol=? s for) what]
                        [else in])]
             [appC (f a) (appC f (subst-helper what for a))]
             [plusC (l r) (plusC (subst-helper what for l)
                                 (subst-helper what for r))]
             [multC (l r) (multC (subst-helper what for l)
                                 (subst-helper what for r))]))

(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
             [numC (n) n]
             [idC (_) (error 'interp "shouldn't get here")]
             [appC (f a) (local ([define fd (get-fundef f fds)])
                           (interp (subst (interp a fds)
                                          (fdC-arg fd)
                                          (fdC-body fd))
                                   fds))]
             [plusC (l r) (+ (interp l fds) (interp r fds))]
             [multC (l r) (* (interp l fds) (interp r fds))]))


(test (interp (appC 'quadruple (numC 5))
              (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))
                    (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))))
      20)
