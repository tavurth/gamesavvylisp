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
  (when *GSL-MOUSE-EVENT-FUNC*
    (funcall *GSL-MOUSE-EVENT-FUNC* button type))
  (when *GSL-GUI-MOUSE-EVENT-FUNC*
    (funcall *GSL-GUI-MOUSE-EVENT-FUNC* button type)))
(gsl_set_mouse_event_func gsl-mouse-event-func)

(defcallback gsl-mouse-move-func (:int motionx :int motiony)
  (when *GSL-MOUSE-MOVE-FUNC* 
    (funcall *GSL-MOUSE-MOVE-FUNC* motionx motiony))
  (when *GSL-GUI-MOVE-FUNC*
    (funcall *GSL-GUI-MOVE-FUNC* motionx motiony)))
(gsl_set_mouse_move_func gsl-mouse-move-func)

(defcallback gsl-key-event-func (:int key :int type)
  (when *GSL-KEY-EVENT-FUNC* (funcall *GSL-KEY-EVENT-FUNC* key type)))
(gsl_set_key_event_func gsl-key-event-func)

(defun gsl-mouse-motion (&optional (type 0 type-supplied))
  (if type-supplied
    (gsl_mouse_motion type)
    (cons (gsl_mouse_motion +y+) (gsl_mouse_motion +x+))))

(defun gsl-get-key (key)
  (if (= (gsl_get_key key) 0) nil t))

(defun gsl-get-charkey ()
  (%get-cstring (gsl_get_charkey)))

(defun gsl-skip-events (time)
  (gsl_skip_events (truncate time)))
