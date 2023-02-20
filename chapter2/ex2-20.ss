(define 1-through-4 (list 1 2 3 4))

(define (mapcar func lis)
    (if (null? lis)
        ()
        (cons (func (car lis)) (mapcar func (cdr lis)))))

(mapcar (lambda (x) (* x x)) 1-through-4)

(mapcar (lambda (x) (+ x 1)) 1-through-4)