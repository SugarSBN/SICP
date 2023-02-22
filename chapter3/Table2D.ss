; table -> ( . ) ---------> ( . ) ---------> ( . ) ---------> ( . ) -> NULL
;           |                |                |                |
;           v                v                v                v 
;     'tableTitle      1 <- ( . )       2 <- ( . )       3 <- ( . )
;                              |                |                | 
;                              v                v                v
;                  ( . ) <- ( . )   ( . ) <- ( . )    ( . ) <- ( . )
;                   | |        |     | |        |      | |        |
;                   v v        |     v v        |      v v        |
;                   1 a        |     1 d        |      1 h        |
;                              v                v                 v
;                  ( . ) <- ( . )   ( . ) <- ( . )    ( . ) <- ( . )
;                   | |        |     | |        |      | |        |
;                   v v        |     v v        |      v v        |
;                   2 b        |     2 e        |      2 i        |
;                              v                v                 v
;                  ( . ) <- ( . )   ( . ) <- ( . )    ( . ) <- ( . )
;                   | |        |     | |        |      | |        |
;                   v v        |     v v        |      v v        |
;                   3 c        |     3 f        |      3 j        |
;                              v                v                 v
;                            NULL             NULL              NULL

; (1, 1) = a
; (1, 2) = b
; (1, 3) = c
; (2, 1) = d
; (2, 2) = e
; (2, 3) = f
; (3, 1) = h
; (3, 2) = i
; (3, 3) = j
; a, b, c, d, e, f, h, i, j can be lists!

(define (make-table tableTitle) (cons tableTitle '()))

(define (table-title table) (car table))

(define (table-query table key1 key2)
    (define (lookUp table key)
        (cond ((null? table) '())
              ((eq? (caar table) key) (cdar table))
              (else (lookUp (cdr table) key))))
    (lookUp (lookUp (cdr table) key1) key2))



(define (table-insert table key1 key2 value)

    (define (lookUp table key)
        (cond ((null? table) '())
                ((eq? (caar table) key) table)
                (else (lookUp (cdr table) key))))

    (define (last x)
        (cond ((null? (cdr x)) x)
                (else (last (cdr x)))))

    (let ((t (lookUp (cdr table) key1)))
         (cond ((null? t) (begin (set-cdr! (last table) (cons (cons key1 '()) '()))
                                 (table-insert table key1 key2 value)))
               (else (let ((t1 (lookUp (cdar t) key2)))
                       (cond ((null? t1) (begin (set-cdr! (last (car t)) (cons (cons key2 value) '()))
                                                table))
                             (else (begin (set-cdr! (car t1) value)
                                          table))))))))

(define t (make-table 'testTable))

(table-insert t '1 '1 'a)
(table-insert t '1 '2 'b)
(table-insert t '1 '3 'c)
(table-insert t '2 '1 'd)
(table-insert t '2 '2 'e)
(table-insert t '2 '3 'f)
(table-insert t '3 '1 'h)
(table-insert t '3 '2 'i)
(table-insert t '3 '3 'j)

(table-query t '1 '1)
(table-query t '1 '2)
(table-query t '1 '3)
(table-query t '2 '1)
(table-query t '2 '2)
(table-query t '2 '3)
(table-query t '3 '1)
(table-query t '3 '2)
(table-query t '3 '3)


;--------------------------------------------------------------------------------------------
(define (new-table title)
    (let ((t (make-table title)))
        (lambda (m) 
            (cond ((eq? m 'title) (table-title t))
                  ((eq? m 'insert) (lambda (key1 key2 value) (table-insert t key1 key2 value)))
                  ((eq? m 'query) (lambda (key1 key2) (table-query t key1 key2)))
                  (else (error "Unknown message: TABLE" m))))))


(define T (new-table 'testTable))
(T 'title)
((T 'insert) '1 '1 'a)
((T 'query) '1 '1)