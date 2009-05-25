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

;;Initialising GSL
(gsl-init :options (logior +GSL-DEFAULT-VIDEO+ +GSL-GET-MOUSE+))

:;Creating a single fbo
(setf *fbo* (gsl-fbo-new *width* *height*))
;;Adding a color buffer to thi fbo
(gsl-fbo-add-color *fbo* 0)

;;Setting the clear color to black
(gl-clear-color 0 0 0)

(defparameter *camera* (list 0 0 -50))

(defun input ()
  (gsl-pump-events)
  (when (gsl-get-key +SDLK-ESCAPE+) (gsl-quit))

  (incf (first *camera*) (/ (gsl-mouse-motion +x+) 10.0))
  (incf (second *camera*) (/ (gsl-mouse-motion +y+) 10.0)))

(defun draw ()
  ;;Draw to the fbo
  (gsl-with-fbo *fbo*
     ;;Clear the fbo's color buffer to black (0 0 0 1)
     (gl-clear (logior +GL-COLOR-BUFFER-BIT+))
     (gl-load-identity)
     (gl-translate :pos *camera*) 
     ;;Draw the box
     (gsl-with-color (1 1 0)
	(gsl-draw-rect :x -10 :y -10 :w 20 :h 20)))

  ;;Draw the fbo's color buffre to the normal screen with blending enabled
  (gsl-with-blendfunc (+GL-SRC-ALPHA+ +GL-ONE-MINUS-SRC-ALPHA+)
      (gsl-with-color (1 1 1 0.33)
	(gsl-draw-rect :fill-screen t :with-fbo *fbo* :fbo-color-pos 0)))

  (gl-swap-buffers))

;; Main loop;
(loop
  (input)
  (draw)
  (sdl-delay 50))
