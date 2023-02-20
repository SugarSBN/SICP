; If the interpreter is eagerly evaluating, then the program will be dead loop

(define (p x) (p x)) ; always loop

(if (= 0 0)
    0
    (p 0))