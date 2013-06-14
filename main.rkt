#lang racket

(module interpreter racket
  (require racket/match)
  (provide eval-with-env evaluate)
  
  (define evaluate
    (lambda (expr)
      (eval-with-env expr '())))
  
  (define eval-with-env
    (lambda (expression env)
      (match expression
        [ (list 'var name)  
          (lookup name env)]
        [ (list 'num n) 
          n ]
        [ (list 'bool 'false)
          #f ]
        [ (list 'bool 'true)
          #t ]
        [ (list 'plus left right) 
          (+ (eval-with-env left env) (eval-with-env right env)) ]
        [ (list 'minus left right) 
          (- (eval-with-env left env) (eval-with-env right env)) ]
        [ (list 'func var-name body) 
          (list 'closure var-name body env) ]
    
        
        [ (list 'call c param) 
          (match (eval-with-env c env)
            [ (list 'closure var-name body cenv) 
              (eval-with-env body (cons (list var-name (eval-with-env param env)) cenv)) ]
            [ _ (raise-arguments-error 'Eval: "Cannot eval-with-env Call" "expr: " c) ])]
        
        [ (list 'let-direct (list var-name expr) body)
          (eval-with-env body (cons (list var-name (eval-with-env expr env)) env)) ]
        
        [ (list 'let-by-func (list var-name expr) body)
          (eval-with-env (list 'call (list 'func var-name body) expr) env) ]
        
        [ (list 'if test do-if-true do-if-false)
          (if (eval-with-env test env)
              (eval-with-env do-if-true env)
              (eval-with-env do-if-false env)) ])))
  
  
  
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
  
  (test-equal? "let-direct"
               (evaluate '(let-direct ("x" (num 28))
                                      (var "x")))
               28)
  
  (test-equal? "let-by-func"
               (evaluate '(let-by-func ("x" (num 18))
                                       (var "x")))
               18)
  
  (test-equal? "if true"
               (evaluate '(let-by-func ("b" (bool true))
                                       (if (var "b") (num 6) (num 7))))
               6)
  
  (test-equal? "if false"
               (evaluate '(let-by-func ("b" (bool false))
                                       (if (var "b") (num 6) (num 7))))
               7)
  
  )
               


 

