(defun test-+ ()
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3) `(= (+ 1 2) 3))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6) `(= (+ 1 2 3) 6))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ -1 -3) -4) `(= (+ -1 -3) -4)))

(defun report-result (result form)
    (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defun test2-+ ()
  (report-result (= (+ 1 2) 3) `(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 6) `(= (+ 1 2 3) 6))
  (report-result (= (+ -1 -3) -4) `(= (+ -1 -3) -4)))

(defmacro check (form)
    `(report-result ,form ',form))

(defun test3-+ ()
  (check (= (+ 1 2) 3))
  (check (= (+ 1 2 3) 6))
  (check (= (+ -1 -3) -4)))

(defmacro check2 (&body forms)
    `(progn
        ,@(loop for f in forms collect `(report-result ,f ',f))))

(defun test4-+ ()
    (check2
        (= (+ 1 2) 3)
        (= (+ 1 2 3) 6)
        (= (+ -1 -3) -4)))
