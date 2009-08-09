(in-package :gsl-point)

(defclass gsl-point ()
  ((x
     :initform 0
     :initarg :x
     :accessor gsl-point-x)
   (y 
     :initform 0
     :initarg :y
     :accessor gsl-point-y)
   (z
     :initform 0
     :initarg :z
     :accessor gsl-point-z)))

(export-all gsl-point-x
	    gsl-point-y
	    gsl-point-z)

(defmacro gsl-point-new (&rest rest);;{{{
  "Create a new point instance"
  `(make-instance 'gsl-point ,@rest));;}}}

(defun gsl-point-list (point);;{{{
  "Return the <x,y,z> coordinates of a point in a list"
  (list (gsl-point-x point) (gsl-point-y point) (gsl-point-z point)));;}}}
