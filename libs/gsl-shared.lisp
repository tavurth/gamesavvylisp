;;;		Copyright (c) William Whitty 2009
;;;
;;;	This file is part of GSL. 
;;;
;;;	GSL is free software: you can redistribute it and/or modify
;;;     it under the terms of the GNU General Public License as published by
;;;     the Free Software Foundation, either version 3 of the License, or
;;;     (at your option) any later version.
;;;
;;;     GSL is distributed in the hope that it will be useful,
;;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;     GNU General Public License for more details.
;;;
;;;     You should have received a copy of the GNU General Public License
;;;     along with GSL.  If not, see <http://www.gnu.org/licenses/>.

(in-package :gsl-shared)

(defmacro const (symbol val);;{{{
  "Exports symbol and then defines it as a constant"
  (export symbol)
  `(defconstant ,symbol ,val));;}}}

(defmacro defparam (symbol val);;{{{
  "Exports symbol and then defines it as a parameter"
  (export symbol)
  `(defparameter ,symbol ,val));;}}}

(defmacro print-macro (statements);;{{{
  `(format t "~a~%" (macroexpand-1 ',statements)));;}}}

(defmacro new-c-func (symbol c-name arguments &rest args);;{{{
  `(FFI:DEF-CALL-OUT ,symbol (:name ,c-name) (:arguments ,@arguments) ,@args));;}}}

(defmacro i->float (num);;{{{
  (if (not (typep num 'float)) `(setf ,num (coerce ,num 'single-float))));;}}}

(defmacro pop->float (target-list);;{{{
  "Pop from a list and convert to a float. Note - Does not check with numberp"
  `(progn 
     (setf num (pop ,target-list))
     (i->float num)));;}}}

(defmacro mirror (a);;{{{
  `(- 0 ,a));;}}}

(defmacro do-array (element array &rest rest)
  `(let ((len (- (array-total-size ,array) 1)))
    (do ((,element 0 (incf ,element)))
      ((>= ,element len))
      (progn
	,@rest))))
