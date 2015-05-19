(define-syntax defn
  (syntax-rules ()
    ((defn name (var ...) expr ...)
     (define name
       (lambda (var ...) expr ...)))))

(defn read-lines (filename)
  (defn get-em (accum)
    (let ((line (read-line)))
    (if (eof-object? line)
      accum
      (get-em (cons line accum)))))
  (let ((lines (with-input-from-file filename (lambda () (get-em '())))))
    (reverse lines)))

; split a line on `char` until you run out of `char`s in line; return the
; elements found this way
(defn split-line (line char)
  (let ((space-index (string-find-next-char line char)))
    (if space-index
      (let ((head (string-head line space-index)) (tail (string-tail line (+ 1 space-index))))
        (cons head (split-line tail char)))
      (list line))))

; map `fn` a->b to `iter` [[a]]
; :: a->b [[a]] -> [[b]]
(defn deep-map (fn iter)
  (map (lambda (child) (map fn child)) iter))

; takes a list of parameter lists, and applies `fn` to each
; in python this would be like [fn(*args) for args in param-lists]
(defn map-apply (fn param-lists)
   (map (lambda (param-list) (apply fn param-list)) param-lists))
