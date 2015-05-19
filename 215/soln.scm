(define-syntax defn
  (syntax-rules ()
    ((defn name (var ...) expr ...)
     (define name
       (lambda (var ...) expr ...)))))

(defn get-input (filename)
  (defn read-input ()
    (let* ((b (read-line)) (n (read-line)))
      (map string->number (list b n))))
  (with-input-from-file filename read-input))

(defn split-num (num)
  (let ((digit (modulo num 10)) (remainder (quotient num 10)))
    (if (> remainder 0)
      (cons  digit(split-num remainder))
      `(,digit))))

(split-num 12345)

(defn calc (b n)
  (let* ((split (split-num n)) (mapped (map (lambda (i) (expt i b)) split)))
  (apply + mapped)))

(defn sad-cycle (b n)
  (defn sad-accum (num so-far)
    (let* ((pow-sum (calc b num)) (found (memv pow-sum so-far)))
      (if found
        (memv pow-sum (reverse (cons num so-far)))
        (sad-accum pow-sum (cons num so-far)))))
  (sad-accum n '()))

(define input (get-input "input.dat"))
(apply sad-cycle input)

(sad-cycle 5 117649)
(sad-cycle 6 2)
(sad-cycle 7 7)
(sad-cycle 3 14)
(sad-cycle 11 2)
;(sad-cycle 104 223114111230211)
