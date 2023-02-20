
(define (term-d n) 
    (cond ((= n 0) 1)
          ((= n 1) 2)
          (else (cond ((= 0 (remainder (- n 2) 3)) 1)
                      ((= 1 (remainder (- n 2) 3)) 1)
                      (else (* 2 (+ 1 (quotient (- n 1) 3))))))))

(define n 1000)

(define (iter i result)
    (if (< i 0)
        result
        (iter (- i 1) (/ 1.0 (+ result (term-d i))))))

; e
(+ 2 (iter n 0))
