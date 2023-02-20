(define x (list (list (list 1 2) 3) 4 5))

(define (atom? l) (not (list? l)))

(define (reverse l)
    (if (null? l)
        ()
        (append (reverse (cdr l)) (list (car l)))))

(reverse x)

(define (deep-reverse l)
    (cond ((null? l) ())
          ((atom? (car l)) (append (deep-reverse (cdr l)) (list (car l))))
          (else (append (deep-reverse (cdr l)) (list (deep-reverse (car l)))))))

(deep-reverse x)