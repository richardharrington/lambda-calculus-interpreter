#lang racket

(module interpreter racket
  (require racket/match)
  (provide evaluate)
  
  (define evaluate
    (lambda (expr env)
      (match expr
        [ (list 'var name) (lookup name env)]
        [ (list 'num n) n ]
        [ (list 'plus left right) (+ (evaluate left env) (evaluate right env)) ]
        [ (list 'minus left right) (- (evaluate left env) (evaluate right env)) ]
        [ (list 'func __ __) (list 'closure expr env) ]
        [ (list 'call (list 'closure (list 'func var-name body) cenv) 
                      val)
            (evaluate body (cons env (cons (cons var-name val) cenv))) ]
        ;[ (list 'call func val) (evaluate func val) ]
        )))
  
  (define lookup
    (lambda (var env)
      (cond
        [(null? env) (error "unbound variable")]
        [(eq? (first (car env)) var) (second (car env))]
        [else (lookup var (cdr env))])))
          
  )


(module test racket
  (require rackunit)
  (require (submod ".." interpreter))
  
  (test-equal? "functions"
               (evaluate (list 'call (list 'func "x" (list 'plus (list 'var "x") (list 'num 1)))
                                     (list 'num 2)) '())
               3)
  (test-equal? "evaluate to 7"
               (evaluate (list 'num 7) '()) 
               7)
  
  (test-equal? "simple addition"
               (evaluate (list 'plus (list 'num 5) (list 'num 3)) '())
               8)
  
  (test-equal? "nested addition"
               (evaluate (list 'plus (list 'num 5)
                               (list 'plus (list 'num 2)
                                     (list 'num 10))) '())
               17)
  
  (test-equal? "addition and subtraction"
               (evaluate (list 'plus (list 'num 5)
                               (list 'minus (list 'num 2)
                                     (list 'num 1))) '())
               6)

  (test-equal? "evaluate a variable"
               (evaluate (list 'var "a") '(("a" 5)))
               5)
  
  (test-equal? "addition and subtraction with variable"
               (evaluate (list 'plus (list 'var "a")
                               (list 'minus (list 'num 2)
                                     (list 'num 1))) '(("a" 5)))
               6)
  
  )

