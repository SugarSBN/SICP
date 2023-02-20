; recursion version
(define (recursion-fact n)
    (if (= n 1)
      1
      (* n (recursion-fact (- n 1)))))

; iteration version
(define (iteration-fact n)
    (define (iter product counter)
      (if (> counter n)
        product
        (iter (* counter product) (+ counter 1))))
    (iter 1 1))