(define (prod term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* (term a) result))))
    (iter a 1))

(define (inc x) (+ x 1))

(define (identity x) x)

; factorial
(define (fact n)
    (prod identity 1 inc n))

(fact 6)

; pi
(define (square x) (* (/ x (- x 1)) (/ x (- x 1))))
(define (inc2 x) (+ x 2))
(define n 50000)
(* (/ 8.0 (+ n 1)) (prod square 4 inc2 n))

