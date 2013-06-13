#lang racket
(require racket/match)

(define evaluate
  (lambda (expr)
    (match expr
      [ (list 'num n) n ]
      [ (list 'plus left right) (+ (evaluate left) (evaluate right)) ]
      [ (list 'minus left right) (- (evaluate left) (evaluate right)) ])))

(evaluate (list 'num 7))
(evaluate (list 'plus (list 'num 5) (list 'num 3)))
(evaluate (list 'plus (list 'num 5)
                      (list 'plus (list 'num 2)
                                  (list 'num 10))))

(evaluate (list 'plus (list 'num 5)
                      (list 'minus (list 'num 2)
                                   (list 'num 1))))


