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

(load "gsl.lisp")	;You have to keep gsl.lisp in the same directory as your program as it contains the location of the GSL source on your HD-Drive

(gsl-init :options (logior +GSL-DEFAULT-VIDEO+ +GSL-GET-MOUSE+))
(setf *shader* (gsl-shader-new :frag "b_w.frag"))
(setf *shader2* (gsl-shader-new :frag "texture.frag"))
(setf *texture* (gsl-load-tex "skyline.tga"))

(defun input ()
  (gsl-load-updates)
  (when *GSL-IS-IN-CONSOLE*
    (gsl-console-input)
    (return-from input))
  (gsl-pump-events)
  (when (gsl-get-key +SDLK-ESCAPE+) (quit))

  (when (gsl-get-key +SDLK-BACKQUOTE+) (gsl-enter-console)))

(defun draw ()
  (gl-clear (logior +GL-COLOR-BUFFER-BIT+))	;Clearing OpenGL's color buffer only
  (gl-load-identity)	
  (gl-translate :z -25)				;You can use :x :y :z and :pos for translating. Pos is a list of 3 numbers (x y z).
  (gsl-with-shader *shader* 
	(gsl-draw-rect -20 -10 :w 40 :h 20 :with-tex *texture*))

  (when *GSL-IS-IN-CONSOLE*
    (gsl-with-shader *shader2*
	(gsl-draw-console)))				;Drawing the gsl console if required.
  (gl-swap-buffers))

;; Main loop;
(loop
  (input)
  (draw)
  (sdl-delay 50))
