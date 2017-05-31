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

(in-package :gsl)

;;Macros

(defmacro use-all ()
  "Use all gsl dependancies"
  `(use-packages
     :gsl
     :gsl-draw
     :gsl-shared
     :gsl-init
     :gsl-globals
     :gsl-input
     :gsl-gl-bits
     :gsl-gl
     :gsl-sdl
     :gsl-console
     :gsl-classes
     :gsl-with
     :gsl-gui
     :gsl-point
     :gsl-animation
     :gsl-updates
     :gsl-event-funcs))


(defmacro use-input ()
  "Use all packages required for input"
  `(use-packages
    :gsl-shared
    :gsl-init
    :gsl
    :gsl-input
    :gsl-sdl
    :gsl-event-funcs))

(defmacro use-video ()
  "Use all packages required for video"
  `(use-packages
    :gsl-shared
    :gsl-init
    :gsl
    :gsl-draw
    :gsl-globals
    :gsl-classes
    :gsl-sdl
    :gsl-gl
    :gsl-gl-bits
    :gsl-gui
    :gsl-with))

(defun gsl-new-font (loc)
  (setf *GSL-DEFAULT-FONT* (gsl_new_font (gsl-relative (concatenate 'string (concatenate 'string "fonts/" loc) ".tga")))))

(defun gsl-finish-video ()
  (setf gsl-shared:*GSL-VIDEO-DONE* t)
  (gsl-new-font "font")
  (gsl-gui-set :border-tex (gsl-relative "themes/border.tga") 
	       :corner-tex (gsl-relative "themes/corner.tga") 
	       :border-size 20 :corner-size 20)
  (progn *GSL-EXEC-AFTER-VIDEO*))

(defun gsl-init-video (&key (width 1024) (height 512) (bpp 32) 
			    (flags 0 flags-passed) (options 0)
			    (fov 90) (near-clip 0.01) (far-clip 10000))
  ;;If we pass no flags, initialise OpenGL with +GSL-DEFAULT-VIDEO-FLAGS+
  (when (not flags-passed) (setf flags +GSL-DEFAULT-VIDEO-FLAGS+))

  ;;Saving the width and height for a later date
  (defparam *width* width)
  (defparam *height* height)

  ;;Setting up & saving our aspect ratio
  (defparam *aspect-x* (/ *width* *height*))
  (defparam *aspect-y* (/ 1 *aspect-x*))

  (gsl_init_video width height bpp flags options (truncate fov) (float near-clip) (truncate far-clip))
  (gsl-finish-video))

(defun gsl-finish-init (options width height)
  (setf *GSL-INIT-DONE* t)
  (when (logand options +GSL-DEFAULT-VIDEO+)
    (gsl-init-video :options options :width width :height height))
  (progn 
    *GSL-EXEC-AFTER-INIT*))

(defun gsl-init (&key (flags +SDL-INIT-VIDEO+) (options +GSL-GET-MOUSE+) (width 1024) (height 512))
  (gsl-shared::reload-binary-libs) ;Make sure our foreign function definitions are up to date
  (gsl_init flags options)
  (gsl-finish-init options width height))


(defun gsl-quit ()
  (progn
    (gsl-delete-all-textures)
    (gsl-delete-all-fbos)
    (gsl_quit)))
