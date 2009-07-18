;;;		Copyright (c) William Whitty 2009
;;;
;;;	This file is part of GSL. 
;;;
;;;	GSL is free software: you can redistribute it and/or modify
;;;     it under the terms of the GNU Lesser General Public License as published by
;;;     the Free Software Foundation, either version 3 of the License, or
;;;     (at your option) any later version.
;;;
;;;     GSL is distributed in the hope that it will be useful,
;;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;     GNU Lesser General Public License for more details.
;;;
;;;     You should have received a copy of the GNU Lesser General Public License
;;;     along with GSL.  If not, see <http://www.gnu.org/licenses/>.

(defpackage :gsl-init
  (:use :common-lisp)
  (:export :gsl-relative
	   :gsl-clib-relative
	   :gsl-lisp-relative
	   :*gsl-dir*
	   :*clib-dir*
	   :*libs-dir*
	   :+LINUX+
	   :+WINDOWS+
	   :*clib-type*))
(in-package :gsl-init)

(defparameter *gsl-dir*   "./")	        ;;Directory where gsl is stored.
(defparameter *clib-dir*  "clibs/")     	;;This should be ok to leave. Directory of C libraries.
(defparameter *libs-dir*  "libs/")	        ;;Leave this too. Directory of lisp libraries
(defparameter *clib-type* ".so")

;;	Automatic set up	;;{{{

(defun gsl-relative (loc);;{{{
  "Return a relative path to the GSL/ directory"
  (concatenate 'string *gsl-dir* loc));;}}}

(defun gsl-clib-relative (loc);;{{{
  "Return a relative path to the clibs/ directory"
  (gsl-relative (concatenate 'string *clib-dir* (concatenate 'string loc *clib-type*))));;}}}

(defun gsl-lisp-relative (loc);;{{{
  "Return a relative path to the libs/ directory"
  (gsl-relative (concatenate 'string *libs-dir* loc)));;}}}

;;If gsl has not already been loaded (from a saved image) make sure we load all the required libraries.
(when (not (find-package :gsl))
  (load (gsl-lisp-relative "gsl-packages.lisp"))
  (load (gsl-lisp-relative "gsl-libs.lisp")))

(in-package :cl-user)
(gsl:use-all)
;;}}}
