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

(in-package :gsl-event-funcs)

;;	GUI EVENT FUNCTIONS
;;	You should push a function to these with (push #'my-function *GSL-MOUSE-MOTION-FUNCS*)
;;	and it will be called when the appropriate event occurs.

(defparam *GSL-LAST-EVENT-FUNC-ID* 0)

(defclass event-func ();;{{{
  ((function
     :initform nil
     :initarg :func
     :accessor gsl-event-func)
   (id
     :initform (incf *GSL-LAST-EVENT-FUNC-ID*)
     :accessor gsl-event-func-id)))

(export-all gsl-event-func
	    gsl-event-func-id);;}}}

;;	Function lists;;{{{

(defparam *GSL-MOUSE-MOTION-FUNCS*	nil)
(defparam *GSL-MOUSE-EVENT-FUNCS*	nil)
(defparam *GSL-KEY-EVENT-FUNCS*		nil);;}}}

(defun gsl-make-event-func (func);;{{{
  "Creates a new event-func and returns it"
  (make-instance 'event-func :func func));;}}}

(defmacro gsl-add-event-func (func func-list);;{{{
  "Adds a new event-func to <func-list>"
  `(push (gsl-make-event-func ,func) ,func-list));;}}}

(defun gsl-add-mouse-motion-func (func);;{{{
  "Add a mouse motion function"
  (gsl-add-event-func func *GSL-MOUSE-MOTION-FUNCS*));;}}}

(defun gsl-add-mouse-event-func (func);;{{{
  "Add a mouse event function"
  (gsl-add-event-func func *GSL-MOUSE-EVENT-FUNCS*));;}}}

(defun gsl-add-key-event-func (func);;{{{
  "Add a key event func"
  (gsl-add-event-func func *GSL-KEY-EVENT-FUNCS*));;}}}

(defmacro gsl-delete-event (func func-list);;{{{
  "Delete <func> from <func-list>"
  `(setf ,func-list (delete-if #'(lambda (event-func) (equalp (gsl-event-func event-func) ,func)) ,func-list)));;}}}

(defun gsl-delete-mouse-motion-func (func);;{{{
  "Deletes event-func <func> which is a compiled function from *GSL-MOUSE-MOTION-FUNCS*"
  (gsl-delete-event func *GSL-MOUSE-MOTION-FUNCS*));;}}}

(defun gsl-delete-mouse-event-func (func);;{{{
  "Deletes event-func <func> which is a compiled function from *GSL-MOUSE-EVENT-FUNCS*"
  (gsl-delete-event func *GSL-MOUSE-EVENT-FUNCS*));;}}}

(defun gsl-delete-key-event-func (func);;{{{
  "Deletes event-func <func> which is a compiled function from *GSL-KEY-EVENT-FUNCS*"
  (gsl-delete-event func *GSL-KEY-EVENT-FUNCS*));;}}}
