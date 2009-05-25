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

;;Some quick initialisation;;{{{
(defpackage :gsl-shared
  (:use :common-lisp)
  (:export :const 
	   :print-macro
	   :gsl-load-all
	   :defparam
	   :gsl-relative
	   :gsl-clib-relative
	   :gsl-lisp-relative
	   :default-foreign-lib
	   :do-array
	   :new-c-func 
	   :if-not 
	   :i->float 
	   :pop->float 
	   :mirror))

(in-package :gsl-shared)

(defun gsl-relative (loc)
  (concatenate 'string *gsl-dir* loc))

(defun gsl-clib-relative (loc)
  (gsl-relative (concatenate 'string *clib-dir* (concatenate 'string loc *clib-type*))))

(defun gsl-lisp-relative (loc)
  (gsl-relative (concatenate 'string *lisplibs* loc)))
;;}}}

(defparameter *gsl-dir* 	"../../")		;;Directory where gsl is stored.
(defparameter *clib-type*	".so")					;;If you are running windows this should be ".dll" Linux: ".so".
(defparameter *clib-dir*	"clibs/")				;;This should be ok to leave. Directory of C libraries.
(defparameter *lisplibs*	"libs/")				;;Leave this too. Directory of lisp libraries

;; Initialisation of GSL.
(load (gsl-lisp-relative "gsl-packages.lisp"))

;; Making sure that cl-user can see all functions exported by GSL;;{{{
(in-package :cl-user)
(gsl:use-all)
;;}}}
