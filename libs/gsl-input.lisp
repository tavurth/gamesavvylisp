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

(in-package :gsl-input)

;;	Setting up our callback events for C
(defcallback gsl-mouse-event-func (:int button :int type)
  (when *GSL-MOUSE-EVENT-FUNCS*
    (dolist (event-func *GSL-MOUSE-EVENT-FUNCS*)
      (funcall (gsl-event-func event-func) button type))))
(gsl_set_mouse_event_func gsl-mouse-event-func)

(defcallback gsl-mouse-move-func (:int motionx :int motiony)
  (when *GSL-MOUSE-MOTION-FUNCS*
    (dolist (event-func *GSL-MOUSE-MOTION-FUNCS*)
      (funcall (gsl-event-func event-func) motionx motiony))))
(gsl_set_mouse_motion_func gsl-mouse-move-func)

(defcallback gsl-key-event-func (:int key :int type)
  (when *GSL-KEY-EVENT-FUNCS*
    (dolist (key-event *GSL-KEY-EVENT-FUNCS*)
      (funcall (gsl-event-func key-event) key type))))
(gsl_set_key_event_func gsl-key-event-func)

(defun gsl-mouse-motion (&optional (type 0 type-supplied))
  (if type-supplied
    (gsl_mouse_motion type)
    (cons (gsl_mouse_motion +y+) (gsl_mouse_motion +x+))))

(defun gsl-get-key (key)
  (if (= (gsl_get_key key) 0) nil t))

(defun gsl-skip-events (time)
  (gsl_skip_events (truncate time)))

(defmacro gsl-keys (&rest keys)
  "Use instead of writing multiple calls to (gsl-get-key)
   Example:
     (gsl-keys
       (+LEFT-KEY+  (incf (gsl-point-x *camera*)))
       (+RIGHT-KEY+ (decf (gsl-point-x *camera*))))"
  (let ((list nil))
    (dolist (key keys)
      (push `((gsl-get-key ,(pop key)) ,@key) list))
    (setf list (reverse list))
    `(cond ,@list)))
