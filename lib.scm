(define-syntax defn
  (syntax-rules ()
    ((defn name (var ...) expr ...)
     (define name
       (lambda (var ...) expr ...)))))

