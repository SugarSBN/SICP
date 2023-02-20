  
(define (iterative-improve good-enough improve) 
    (define (iter x)
        (if (good-enough x)
            x
            (iter (improve x))))
    iter)

(define (fixed-point f) 
    (define (good-enough x) (< (abs (- x (f x))) 0.00000001))
    (define (improve x) (f x))
    (iterative-improve good-enough improve))

((fixed-point (lambda (x) (+ (- (* x x) x) 1))) 0.9)

(define (average x y) (/ (+ x y) 2.0))
(define (sqrt x)
  (define (good-enough guess)
    (< (abs (- (* guess guess) x)) 0.0001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough improve) 1.0))


(sqrt 2.0)