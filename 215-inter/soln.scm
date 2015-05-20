(load "../lib")

(define network (make-record-type "network" '(num-wires comps)))
(define make-network (record-constructor network '(num-wires comps)))
(define network-num-wires (record-accessor network 'num-wires))
(define network-comps (record-accessor network 'comps))

(defn parse-input (input)
  (let ((num-wires (caar input)) (comps (cdr input)))
    (make-network num-wires comps)))

(defn get-input (filename)
  (let ((lines (read-lines filename)))
    (parse-input
      (map-each string->number
        (split-lines lines #\ )))))

(defn swap (vec i j)
  (let ((i-val (vector-ref vec i)) (j-val (vector-ref vec j)))
    (vector-set! vec i j-val)
    (vector-set! vec j i-val)))

(defn is-sorted (vec)
  (defn inner (i)
     (if (= i (- (vector-length vec) 1))
       #t
       (if (> (vector-ref vec i) (vector-ref vec (+ i 1)))
         #f
         (inner (+ i 1)))))
  (inner 0))

(defn run-network (network nums)
  (if (not (vector? nums))
    (error "Must be called with a vector of numbers"))
  (if (not (= (network-num-wires network) (vector-length nums)))
    (error "Must be called with the same number of input numbers"
           "as wires in the network"))
  (defn run-comp (comp)
    (let ((top (car comp)) (bottom (cadr comp)))
      (if (> (vector-ref nums top) (vector-ref nums bottom))
        (swap nums top bottom))))
  (defn run-all (inner-comps)
    (if (null? inner-comps)
      nums
      (begin
        (run-comp (car inner-comps))
        (run-all (cdr inner-comps)))))
  (run-all (network-comps network)))

(defn all-binary-seqs (len)
  (defn build-lists (build-len)
      (if (zero? build-len)
        '(())
        (append (map (lambda (l) (cons 0 l)) (build-lists (- build-len 1)))
                (map (lambda (l) (cons 1 l)) (build-lists (- build-len 1))))))
  (map list->vector (build-lists len)))

(defn network-sorts-input (network nums)
   (is-sorted (run-network network nums)))

(defn network-sorts-inputs (network inputs)
  (defn sort-tail (inner-inputs)
    (if (null? inner-inputs)
      #t
      (if
        (not (network-sorts-input network (car inner-inputs)))
        #f
        (sort-tail (cdr inner-inputs)))))
  (sort-tail inputs))

(defn test-network (network)
  (let ((size (network-num-wires network)))
    (network-sorts-inputs network (all-binary-seqs size))))

(define network (get-input "input.dat"))
(begin
  (newline)
    (if
      (test-network network)
      (write-string "Valid network\n")
      (write-string "Invalid network\n"))
    (newline))
