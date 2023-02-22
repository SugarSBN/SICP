; q -> ( . ) --------------.
;       |                  |
;       v                  v
;      ( . ) -> ( . ) -> ( . ) -> NULL
;       |        |        |
;       v        v        v 
;      'a       'b       'c
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue front) (set-car! queue front))
(define (set-rear-ptr! queue rear) (set-cdr! queue rear))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front queue)
    (if (empty-queue? queue)
        (error "FRONT called with an empty queue" queue)
        (car (front-ptr queue))))

(define (insert-queue! queue item)
    (let ((new-pair (cons item ())))
        (cond ((empty-queue? queue) 
                    (set-front-ptr! queue new-pair)
                    (set-rear-ptr! queue new-pair)
                    queue)
              (else
                    (set-cdr! (rear-ptr queue) new-pair)
                    (set-rear-ptr! queue new-pair)
                    queue))))

(define (delete-queue! queue)
    (cond ((empty-queue? queue)
                (error "DELETE-QUEUE! called with an empty queue" queue))
          (else
                (set-front-ptr! queue (cdr (front-ptr queue)))
                queue)))

(define q (make-queue))
(insert-queue! q 'a)
(insert-queue! q 'b)
(insert-queue! q 'c)
(delete-queue! q)