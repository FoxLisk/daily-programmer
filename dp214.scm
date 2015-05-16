(define get-lines
  (lambda ()
    (define read-n-lines
      (lambda (n accum)
        (let ((line (read-line)))
          (if (= 1 n)
            (cons line accum)
            (read-n-lines (- n 1) (cons line accum)))
          )))
    (read-n-lines (string->number (read-line)) '())))

(define parse-line
  (lambda (line)
    (let ((space-index (string-find-next-char line #\ )))
      ;(write-line
      ;  (string-append
      ;    "parsing " line " split at " (number->string space-index)
      ;    " into " (string-head line space-index) " and " (string-tail line space-index)))
        (cons
          (string->number (string-head line space-index))
          (string->number (string-tail line (+ 1 space-index)))))))

(define parse-lines
  (lambda (lines)
    (define parse-accum
      (lambda (ac-lines accum)
        (let* ((line (car ac-lines)) (rest (cdr ac-lines)) (vals (parse-line line)))
          (if (null? rest)
            (cons vals accum)
            (parse-accum rest (cons vals accum))
            ))))
    (parse-accum lines '())
    ))

(define distance
  (lambda (pt1 pt2)
    (let ((x1 (car pt1)) (y1 (cdr pt1))
          (x2 (car pt2)) (y2 (cdr pt2)))
      (sqrt (+ (square (- x1 x2)) (square (- y1 y2)))))))

(define split-at
  (lambda (to-split i)
    (if (null? to-split)
      '()
        (if
          (= i 0) (split-at (cdr to-split) (- i 1))
          (cons (car to-split) (split-at (cdr to-split) (- i 1)))))))

(define distance-nearest-and-pop
  (lambda (start-pos treats)
    (define closest-and-index
      (lambda (data i)
        (let* ((cur (car data))
               (rest (cdr data))
               (point (cadr cur))
               (dist (car cur)))
          (if (null? rest)
            (list point dist i)
            (let* ((next-call (closest-and-index rest (+ i 1)))
                   (next-dist (cadr next-call)))
              (cond ((< dist next-dist) (list point dist i))
                    (else next-call)))
            ))))
    (let* ((with-distance (map (lambda (t) (list (distance start-pos t) t)) treats))
           (closest (closest-and-index with-distance 0))
           (best-point (car closest))
           (best-dist (cadr closest))
           (split-idx (caddr closest)))
      (list best-dist best-point (split-at treats split-idx)))))

(define get-total-distance
  (lambda (start-pos treats)
    (define get-accum
      (lambda (cur-pos remaining sum)
        (if
          (null? remaining)
          sum
          (let* ((res (distance-nearest-and-pop cur-pos remaining))
                 (dist (car res))
                 (next-pos (cadr res))
                 (next-rem (caddr res)))
            (get-accum next-pos next-rem (+ sum dist))))))
    (get-accum start-pos treats 0)))

(define lines (with-input-from-file "input.dat" get-lines))
(define treats (parse-lines lines))
;(distance-nearest-and-pop (cons .5 .5) treats)
(get-total-distance (cons .5 .5) treats)
