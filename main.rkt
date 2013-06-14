#lang racket

(module interpreter racket
  (require racket/match)
  (provide eval-with-env evaluate)
  
  (define evaluate
    (lambda (expr)
      (eval-with-env expr '())))
  
  (define eval-with-env
    (lambda (expr env)
      (match expr
        [ (list 'var name) (lookup name env)]
        [ (list 'num n) n ]
        [ (list 'plus left right) (+ (eval-with-env left env) (eval-with-env right env)) ]
        [ (list 'minus left right) (- (eval-with-env left env) (eval-with-env right env)) ]
        [ (list 'func var-name body) (list 'closure var-name body env) ]
        [ (list 'call c val) (match (eval-with-env c env)
                             [ (list 'closure var-name body cenv) 
                               (eval-with-env body (cons (list var-name (eval-with-env val env)) cenv)) ]
                             [ _ (raise-arguments-error 'Eval: "Cannot eval-with-env Call" "expr: " c) ])]
        ;[ (list 'let-direct (list ')))
  
  )))
  
  (define lookup
    (lambda (var env)
      (cond
        [(null? env) (error "unbound variable")]
        [(eq? (first (car env)) var) (second (car env))]
        [else (lookup var (cdr env))]))))


(module test racket
  (require rackunit)
  (require (submod ".." interpreter))
  
  (test-equal? "evaluate to 7"
               (evaluate '(num 7)) 
               7)
  
  (test-equal? "simple addition"
               (evaluate '(plus (num 5) (num 3)))
               8)
  
  (test-equal? "nested addition"
               (evaluate '(plus (num 5)
                                (plus (num 2)
                                      (num 10))))
               17)
  
  (test-equal? "addition and subtraction"
               (evaluate '(plus (num 5)
                                (minus (num 2)
                                       (num 1))))
               6)

  (test-equal? "eval-with-env a variable"
               (eval-with-env '(var "a") '(("a" 5)))
               5)
  
  (test-equal? "addition and subtraction with variable"
               (eval-with-env '(plus (var "a")
                                (minus (num 2)
                                       (num 1))) '(("a" 5)))
               6)

  (test-equal? "functions"
               (evaluate '(call (func "x" 
                                      (plus (var "x") 
                                            (num 1)))
                                (num 2)))
               3)
  
  (test-equal? "passing functions to functions"
               (evaluate '(call (func "f"
                                      (plus (num 2)
                                            (call (var "f")
                                                  (num 8))))
                                (func "x"
                                      (plus (num 5)
                                            (var "x")))))
               15)
  
  )
               


 

