(define-syntax define-auxiliary-keyword
  (syntax-rules ()
    ((_ id)
     (define-syntax id
       (lambda (stx)
         (syntax-violation #f "misplaced auxiliary keyword" stx))))))

(define-auxiliary-keyword __)
(define-auxiliary-keyword ___)
(define-auxiliary-keyword __1)
(define-auxiliary-keyword __=)
(define-auxiliary-keyword __*)
(define-auxiliary-keyword ***)
(define-auxiliary-keyword ?)
(define-auxiliary-keyword $)
(define-auxiliary-keyword struct)
(define-auxiliary-keyword _@)
(define-auxiliary-keyword object)
(define-auxiliary-keyword get!)

(define-syntax is-a?
  (syntax-rules ()
    ((_ rec rec-name)
     (let ((rtd (record-type-descriptor rec-name)))
       (and (record-type-descriptor? rtd)
            ((record-predicate rtd) rec))))))

(define-syntax slot-ref
  (syntax-rules ()
    ((_ rec-name rec n)
     (let ((rtd (record-type-descriptor rec-name)))
       (if (integer? n)
           ((record-accessor rtd n) rec)
           ((record-accessor rtd (name->idx rtd n)) rec))))))

(define-syntax slot-set!
  (syntax-rules ()
    ((_ rec-name rec n)
     (let ((rtd (record-type-descriptor rec-name)))
       (if (integer? n)
           ((record-mutator rtd n) rec)
           ((record-mutator rtd (name->idx rtd n)) rec))))))

(define-syntax name->idx
  (syntax-rules ()
    ((_ rtd n)
     (let* ((names (record-type-field-names rtd))
            (len (vector-length names)))
       (let lp ((i 0))
         (cond
          ((> i len) (error "name not in record" n))
          ((eq? n (vector-ref names i)) i)
          (else (lp (+ i 1)))))))))
