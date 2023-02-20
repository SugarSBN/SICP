
(define (filter pred combiner null-value term a next b)
    (define (iter x result)
        (if (> x b)
            result
            (iter (next x) (if (pred x)
                               (combiner result (term x))
                               result))))
    (iter a null-value))

(define (prime a)
    (define (iter x) (
        if (> (* x x) a)
            #t
            (if (zero? (remainder a x))
                #f
                (iter (+ x 1)))))
    (if (= a 1)
        #f
        (iter 2)))

(prime 7)
(prime 8)

(define (id x) x)
(define (inc x) (+ x 1))
(define (sum-prime a b) (filter prime + 0 id a inc b))

(sum-prime 1 10)

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (phi n)
    (define (prime? x) (= (gcd x n) 1))
    (filter prime? * 1 id 1 inc n))

(phi 5)