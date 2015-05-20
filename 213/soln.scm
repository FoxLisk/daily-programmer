(load "../lib")

(define box (make-record-type "box" '(color x y width height)))
(defn make-box (color x y width height)
  (let ((box-construct (record-constructor box '(color x y width height)))
        (is-int (map integer? (list color x y width height))))
    (if (apply boolean/and is-int)
      (box-construct color x y width height)
      (error "cannot construct a box from"
             color x y width height
             "(all parameters must be integers)"))))

(define box-get-color (record-accessor box 'color))
(define box-get-x (record-accessor box 'x))
(define box-get-y (record-accessor box 'y))
(define box-get-width (record-accessor box 'width))
(define box-get-height (record-accessor box 'height))

(defn in-box (my-box px py)
  (let* (
        (bx (box-get-x my-box))
        (by (box-get-y my-box))
        (bw (box-get-width my-box))
        (bh (box-get-height my-box))
        (x-ok (and (>= px bx) (< px (+ bx bw))))
        (y-ok (and (>= py by) (< py (+ by bh)))))
      (and x-ok y-ok)))

(defn build-model (params)
  (let* ((board-params (car params)) (boxes-params (cdr params))
         (board (make-box 0 0 0 (car board-params) (cadr board-params))))
    (cons (list (box-get-width board) (box-get-height board))
    (reverse (cons board (map-apply make-box boxes-params))))))

(defn color-at (boxes x y)
  (box-get-color (find (lambda (box) (in-box box x y)) boxes)))

(defn build-results (model)
  (let ((sizes (car model))
        (boxes (cdr model))
        (results (make-strong-eqv-hash-table)))
    (do
      ((i 0 (+ i 1)))
      ((= i (car sizes)) results)
        (do
          ((j 0 (+ j 1)))
          ((= j (cadr sizes)))
          (let ((color (color-at boxes i j)))
            (hash-table/put! results color (+ 1 (hash-table/get results color 0))))))))


(define lines (map (lambda (line) (split-line line #\ )) (read-lines "input.dat")))
(define params (map-each string->number lines))
(define model (build-model params))
(hash-table->alist (build-results model))
