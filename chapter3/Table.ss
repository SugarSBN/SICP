(define (make-table t1)
    (let ((t t1)) 
        (define (getType s) (car s))
        (define (getFuncName s) (cadr s))
        (define (getFunc s) (caddr s))
        
        (define (put typeName funcName func)
            (begin (set! t (cons (list typeName funcName func) t)) t))

        (define (get typeName funcName)
            (define (lookUp s) 
                (cond ((null? s) ())
                      ((and (equal? typeName (getType (car s)))
                            (equal? funcName (getFuncName (car s)))) (getFunc (car s)))
                      (else (lookUp (cdr s)))
                ))
            (lookUp t))
        (lambda (m) (cond ((eq? m 'put) put)
                          ((eq? m 'get) get)
                          (else (error "Unknown operation -- TABLE")))
        )
    )
)

;-------------------------------------------------------------------------------

(define (attach-type type contents) (list type contents))

(define (type datum)
    (if (list? datum)
        (car datum)
        (error "Bad typed datum -- TYPE" datum)))

(define (contents datum)
    (if (list? datum)
        (cadr datum)
        (error "Bad typed datum -- CONTENTS" datum)))

;--------------------------------------------------------------------------------


(define (make-rectangular real imag) (list real imag))
(define (real-part z) (car z))
(define (imag-part z) (cadr z))

(define (make-polar r a) (list (* r (cos a)) (* r (sin a))))
(define (magnitude z) (sqrt (+ (square (car z)) (square (cadr z)))))
(define (angle z) (atan (cadr z) (car z)))

(define (+c z1 z2)
    (make-rectangular (+ (real-part z1) (real-part z2))
                      (+ (imag-part z1) (imag-part z2))))
(define (-c z1 z2)
    (make-rectangular (- (real-part z1) (real-part z2))
                      (- (imag-part z1) (imag-part z2))))

(define (*c z1 z2)
    (make-polar (* (magnitude z1) (magnitude z2))
                (+ (angle z1) (angle z2))))

(define (/c z1 z2)
    (make-polar (/ (magnitude z1) (magnitude z2))
                (- (angle z1) (angle z2))))

;--------------------------------------------------------------------------------


(define (make-complex z) (attach-type 'complex z))
(define (make-integer n) (attach-type 'integer n))

;--------------------------------------------------------------------------------

(define coercion-tab (make-table ()))
((coercion-tab 'put) '(integer complex) '(integer complex) (lambda (x) (make-complex (make-rectangular (contents x) 0))))

(define operator-tab (make-table ()))
((operator-tab 'put) 'complex '+ +c)
((operator-tab 'put) 'complex '- -c)
((operator-tab 'put) 'complex '* *c)
((operator-tab 'put) 'complex '/ /c)
((operator-tab 'put) 'integer '+ +)
((operator-tab 'put) 'integer '- -)
((operator-tab 'put) 'integer '* *)
((operator-tab 'put) 'integer '/ /)

(define make-tab (make-table ()))
((make-tab 'put) 'complex 'make make-complex)
((make-tab 'put) 'integer 'make make-integer)

;---------------




(define (operate-2 op obj1 obj2)
    (let ((t1 (type obj1))
          (t2 (type obj2)))
        (if (eq? t1 t2)
            (let ((proc ((operator-tab 'get) t1 op)))
                (if (null? proc)
                    (error "Operator undefined on this type -- OPERATE-2" (list op obj1 obj2))
                    (((make-tab 'get) t1 'make) (proc (contents obj1) (contents obj2)))))
            (let ((t1->t2 ((coercion-tab 'get) (list t1 t2) (list t1 t2)))
                  (t2->t1 ((coercion-tab 'get) (list t2 t1) (list t2 t1))))
                (cond ((not (null? t1->t2)) (operate-2 op (t1->t2 obj1) obj2))
                      ((not (null? t2->t1)) (operate-2 op obj1 (t2->t1 obj2)))
                      (else (error "Operands not of same type -- OPERATE-2" (list op obj1 obj2))))))))

(define z1 (make-complex (make-rectangular 1 2)))
(define z2 (make-integer 3))

(define integer->complex ((coercion-tab 'get) '(integer complex) '(integer complex)))

(define (!+! a b) (operate-2 '+ a b))
(define (!-! a b) (operate-2 '- a b))
(define (!*! a b) (operate-2 '* a b))
(define (!/! a b) (operate-2 '/ a b))

(!+! z1 z2)
(!-! z1 z2)
(!*! z1 z2)
(!/! z1 z2)
(!+! z2 z1)
(!-! z2 z1)
(!*! z2 z1)
(!/! z2 z1)
