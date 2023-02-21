
(define random-init 10)

(define rand
    (let ((x random-init))
        (lambda ()
            (set! x (rand-update x))
            x)))

(rand)

(rand)

(rand)