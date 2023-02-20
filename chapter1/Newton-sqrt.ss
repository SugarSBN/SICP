(define  (sqrt guess x) 
  (if (good-enough? guess x) 
      guess 
      (sqrt (improve guess x) x)))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
    (average guess (/ x guess)))

(define (average x y)
    (/ (+ x y) 2))
    
(sqrt 1.0 9)