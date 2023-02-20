;recursion version
(define (recursion-fib n)
  (if (< n 2)
      n
      (+ (recursion-fib (- n 1)) (recursion-fib (- n 2)))))

;iterative version
(define (iterative-fib n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  (iter 1 0 n))

(recursion-fib 10)
(iterative-fib 10)