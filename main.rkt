#lang racket
(require racket/match)

(module interpreter racket
  (provide evaluate)
  (define evaluate
    (lambda (expr)
      (match expr
        [ (list 'num n) n ]
        [ (list 'plus left right) (+ (evaluate left) (evaluate right)) ]
        [ (list 'minus left right) (- (evaluate left) (evaluate right)) ]))))

(module test racket
  (require rackunit)
  (test-equal? "evaluate to 7"
               (evaluate (list 'num 7)) 
               7)
  
  (test-equal? "simple addition"
               (evaluate (list 'plus (list 'num 5) (list 'num 3)))
               8)
  
  (test-equal? "nested addition" 
               (evaluate (list 'plus (list 'num 5)
                               (list 'plus (list 'num 2)
                                     (list 'num 10))))
               17)
  
  (test-equal? "addition and subtraction"
               (evaluate (list 'plus (list 'num 5)
                               (list 'minus (list 'num 2)
                                     (list 'num 1))))
               6))

