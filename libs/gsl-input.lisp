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

(in-package :gsl-input)

(defmacro gsl-mouse-motion (&optional (type 0 type-supplied))
  (if type-supplied
    `(gsl_mouse_motion ,type)
    `(cons (gsl_mouse_motion +y+) (gsl_mouse_motion +x+))))

(defmacro gsl-get-key (key)
  `(if (= (gsl_get_key ,key) 0) nil t))

(defmacro gsl-get-charkey ()
  `(%get-cstring (gsl_get_charkey)))

(defmacro gsl-pump-events ()
  `(gsl_pump_events))

(defmacro gsl-skip-events (time)
  `(gsl_skip_events ,time))

(defmacro gsl-get-mods ()
  `(gsl_get_mods))
