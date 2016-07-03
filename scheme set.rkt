(define (basic-set)
  (let ((s '()))
    (define (empty?)
      (null? s))
    (define (element? n)
      (if (null? s)
          #f
          (if (= (car s) n)
              #t
              (element? (cdr s) n))))
    (define (insert n)
      (if (element? n)
          s
          (begin
            (set! s
                  (cons n s))
            s)))
    (define (delete n)
      (begin
        (set! s
              (if (element? s n)
                  (if (= (car s) n)
                      (cdr s)
                      (cons (car s) (delete (cdr s) n)))
                  s))))
    (lambda (method)
      (cond ((eq? method 'empty?) empty)
            ((eq? method 'insert) insert)
            ((eq? method 'delete) delete)
            ((eq? method 'element?) element?)))))