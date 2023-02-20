(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (inc x) (+ x 1))

(sum cube 1 inc 10)