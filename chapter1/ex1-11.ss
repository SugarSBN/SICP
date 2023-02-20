(define (f x)
    (define (iter a b c n)
        (if (< n 3)
            a
            (iter (+ a (* 2 b) (* 3 c)) a b (- n 1))))
    (iter 2 1 0 x))

(f 3)
(f 10)