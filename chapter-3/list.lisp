(defvar *db* nil)

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd)
  (push cd *db*))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a:" prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0) 
    (y-or-n-p "Repped [y/n]")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]:")) (return))))

(add-cds)
(add-record
  (make-cd "hoge1" "fuga1" 1 t))
(add-record
  (make-cd "hoge2" "fuga2" 2 t))
(add-record
  (make-cd "hoge4" "fuga4" 4 t))
(add-record
  (make-cd "hoge5" "fuga5" 5 t))

(dump-db)
