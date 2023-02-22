(define (count-pairs x)
    (if (or (not (list? x)) (equal? x '()))
        0
        (+ (count-pairs (car x))
           (count-pairs (cdr x))
           1)))

; output=3
(define x (cons 'a (cons 'b (cons 'c '()))))
(count-pairs x)
; x -> ( . ) -> ( . ) -> ( . ) -> null
;       |        |        | 
;       v        v        v
;      'a       'b       'c

; output=4
(define a (cons 'a ()))
(define y (cons (cons a a) ()))
(count-pairs y)
; y -> ( . ) -> null 
;       | 
;       v 
;      ( . ) 
;       | | 
;       v v 
;      ( . ) -> 'null 
;       | 
;       v 
;      'a

; output=7
(define b (cons 'a ()))
(define c (cons b b))
(define z (cons c c))
(count-pairs z) 
; z -> ( . ) 
;       | |
;       v v
;      ( . ) 
;       | | 
;       v v 
;      ( . ) -> 'null 
;       | 
;       v 
;      'a


;-------------------------------------
(define (count-pairs-correct x)
    (if (or (not (list? x)) (equal? x '()))
        0
        (let ((first (car x))
              (rest (cdr x)))
            (if (eq? first rest)
                (+ (count-pairs-correct first)
                   1)
                (+ (count-pairs-correct first)
                   (count-pairs-correct rest)
                   1)
                ))))

(count-pairs-correct x)
(count-pairs-correct y)
(count-pairs-correct z)