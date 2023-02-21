(define (fringe l) 
    (cond ((null? l) ()) 
          ((list? (car l)) (append (fringe (car l)) (fringe (cdr l)))) 
          (else (cons (car l) (fringe (cdr l))))))

(fringe (list (list 1 2) 3 4))
(fringe (list (list (list 1 2) 3 4) (list (list 1 2) 3 4)))
