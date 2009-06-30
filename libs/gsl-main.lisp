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
  `(use-packages
     :gsl
     :gsl-draw
     :gsl-shared
     :gsl-globals
     :gsl-input
     :gsl-gl-bits
     :gsl-gl
     :gsl-sdl
     :gsl-console
     :gsl-classes
     :gsl-with
     :gsl-gui
     :gsl-animation
     :gsl-updates))
;;}}}

(defmacro use-input ();;{{{
  "Use all packages required for input"
  `(use-packages
    :gsl-shared
    :gsl
    :gsl-input
    :gsl-sdl));;}}}

(defmacro use-video ();;{{{
  "Use all packages required for video"
  `(use-packages
    :gsl-shared
    :gsl
    :gsl-draw
    :gsl-globals
    :gsl-classes
    :gsl-sdl
    :gsl-gl
    :gsl-gl-bits
    :gsl-gui
    :gsl-with));;}}}

(defmacro gsl-new-font (loc);;{{{
  `(setf *GSL-DEFAULT-FONT* (gsl_new_font (gsl-relative (concatenate 'string (concatenate 'string "fonts/" ,loc) ".tga")))));;}}}

(defun gsl-init-video (&key (width 1024) (height 512) (bpp 32) (flags 0 flags_passed) (options 0) (fov 90) (near_clip 0.1) (far_clip 10000));;{{{
  (when (not flags_passed) (setf flags +GSL-DEFAULT-VIDEO-FLAGS+))
  (setf flags (logior flags +SDL-OPENGL+))	;Make sure OpenGL is always enabled

  (defparam *width* width)
  (defparam *height* height)

  (defparam *aspect-x* (/ *width* *height*))
  (defparam *aspect-y* (/ 1 *aspect-x*))
  (progn
     (gsl_init_video width height bpp flags options (truncate fov) (float near_clip) (truncate far_clip))
     (gsl-new-font "font")
     (setf *GSL-VIDEO-DONE* t)))
;;}}}

(defun gsl-init (&key (flags +SDL-INIT-VIDEO+) (options +GSL-GET-MOUSE+) (width 1024) (height 512));;{{{
  (if (logand options +GSL-DEFAULT-VIDEO+)
    (progn
       (gsl_init flags options)
       (gsl-init-video :flags flags :options options :width width :height height)
       (setf *GSL-INIT-DONE* t))))
;;}}}

(defun gsl-quit ();;{{{
  (progn
    (gsl-delete-all-textures)
    (gsl-delete-all-fbos)
    (gsl_quit)));;}}}

;;}}}
