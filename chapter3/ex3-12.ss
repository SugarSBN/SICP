(define (append x y)
    (if (null? x)
        y
        (cons (car x) (append (cdr x) y))))

(define (append! x y)
    (define (last x)
        (if (null? (cdr x))
            x
            (last (cdr x))))
    (set-cdr! (last x) y)
    x)

(define x '(a b))
(define y '(c d))

(define z (append x y))
z
(cdr x)

(define w (append! x y))
w
(cdr x)