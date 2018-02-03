(defun reading (var)
	(format *query-io* "~a: " var)
	(force-output *query-io*)
	(read-line *query-io*))
(defun plot (fn min max step)
  (loop for i from min to max by step do
        (loop repeat (funcall fn i) do (format t "*"))
        (format t "~%")))
(defun read-int (var)
	(or (parse-integer (reading var) :junk-allowed t) 0)
)

(plot #'exp (read-int "min") (read-int "max") 1/2)

(quit)
