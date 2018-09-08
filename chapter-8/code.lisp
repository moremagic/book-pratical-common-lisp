(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))

(do ((p (next-prime 0) (next-prime (1+ p))))
  ((> p 19))
  (format t "~d " p))

(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
    ((> ,var ,end))
    ,@body))

(do-primes (p 0 19)
  (format t "~d " p))


(defmacro do-primes ((var start end) &body body)
  (let ((ending-value (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
          (,ending-value ,end)
         ((> ,var ,ending-value))
      ,@body)))

;; (macroexpand-1 '(do-primes (p 0 19) (format t "~d " p)))


;; (macroexpand-1 '(do-primes (p 0 (random 100)) (format t "~d " p)))

