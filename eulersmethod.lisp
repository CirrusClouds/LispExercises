
;; Just a simple program designed to make euler's method recursive rather than by utilising loops


(defun acceleration (x)
  ;; Non-constant acceleration dependent on x
  (+ (* -1 (expt x 3)) (* 2 (expt x 2)) (* 5 x) 3))
  

(defun eulermethod (x u accel tmax &optional (tacc 0) (tstep 1))
  (cond ((= tmax tacc)
	 nil)
	(t
	 (cons x (eulermethod (+ x (* u tstep)) (+ u (* (funcall accel x) tstep)) accel tmax (+ tacc tstep) tstep)))))




