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

(load "../gsl.lisp")

(gsl-init :options (logior +GSL-DEFAULT-VIDEO+ +GSL-GET-MOUSE+))

;;Initialising shader
(defparameter *shader* (gsl-shader-new :frag "b_w.frag"))
(gsl-shader-source *shader* :frag "b_w.frag")
(gsl-shader-set *shader* "name" 1)

;;Loading texture
(defparameter *tex* (gsl-load-tex "skyline.tga"))

(defun input ()
  (gsl-pump-events)
  (when (gsl-get-key +SDLK-ESCAPE+) (gsl-quit)))

(defun draw ()
  (gl-clear (logior +GL-COLOR-BUFFER-BIT+ +GL-DEPTH-BUFFER-BIT+))
  (gl-load-identity)
  (gl-translate :z -50)

  ;;Draw the following with this shader
  (gsl-with-shader *shader*
    (gsl-draw-rect :fill-screen t :with-tex *tex*))

  (gl-swap-buffers))

(loop
  (input)
  (draw))
