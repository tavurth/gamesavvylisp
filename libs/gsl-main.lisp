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

(in-package gsl)

;;Macros;;{{{

(defmacro use-all ();;{{{
  "Use all gsl dependancies"
  `(progn
     (use-package :gsl)
     (use-package :gsl-draw)
     (use-package :gsl-shared)
     (use-package :gsl-globals)
     (use-package :gsl-input)
     (use-package :gsl-gl)
     (use-package :gsl-sdl)
     (use-package :gsl-console)
     (use-package :gsl-classes)
     (use-package :gsl-with)
     (use-package :gsl-shader)
     (use-package :gsl-updates)
     (in-package  :cl-user)));;}}}

(defmacro gsl-init-video (&optional (width 1024) (height 512) (bpp 32) (flags 0 flags_passed));;{{{
  (when (not flags_passed) (setf flags +GSL-DEFAULT-VIDEO-FLAGS+))
  (defparam *width* width)
  (defparam *height* height)
  (defparam *aspect* (/ *width* *height*))
  `(progn
     (gsl_init_video ,width ,height ,bpp ,flags)))
     ;(gsl-new-font "font")))	;;Setting up *GSL-DEFAULT-FONT*
;;}}}

(defmacro gsl-init (&key (flags +SDL-INIT-VIDEO+) (options +GSL-GET-MOUSE+));;{{{
  (if (logand flags +GSL-DEFAULT-VIDEO+)
    `(progn
       (gsl_init ,flags ,options)
       (gsl-init-video))
    `(gsl_init ,flags ,options)));;}}}

(defun gsl-quit ()
  (progn
    (gsl-delete-all-textures)
    (gsl-delete-all-fbos)
    (gsl_quit)))

(defmacro gsl-new-font (loc)
  `(setf *GSL-DEFAULT-FONT* (gsl_new_font (gsl-relative (concatenate 'string (concatenate 'string "fonts/" ,loc) ".tga")))))

;;}}}
