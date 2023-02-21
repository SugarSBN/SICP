(define (attach-type type contents) (cons type contents))

(define (type datum)
    (if (list? datum)
        (car datum)
        (error "Bad typed datum -- TYPE" datum)))

(define (contents datum)
    (if (list? datum)
        (cdr datum)
        (error "Bad typed datum -- CONTENTS" datum)))

