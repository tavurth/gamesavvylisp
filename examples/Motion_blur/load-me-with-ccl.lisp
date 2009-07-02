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

(load "../gsl.lisp")
(gsl-init :options (logior +GSL-DEFAULT-VIDEO+ +GSL-GET-MOUSE+))

;;Creating a single fbo
(defparameter *fbo* (gsl-fbo-new *width* *height*))
;;Adding a color buffer to the fbo
(gsl-fbo-add-color *fbo* 0)
;;Adding a depth buffer to the fbo (for depth testing)
(gsl-fbo-add-depth *fbo*)

;;rotation of the cuboid
(defparameter *rotation* 0)

;;So we overlay the entire screen with black each frame, try commenting this line out
(gl-clear-color 0 0 0)

(defun input()
  (gsl-pump-events)
  (when (gsl-get-key +SDLK-ESCAPE+) (gsl-quit)))

(defun draw ()

  ;;Draw to the fbo *fbo*
  (gsl-with-fbo *fbo*
		(gl-clear (logior +GL-COLOR-BUFFER-BIT+ +GL-DEPTH-BUFFER-BIT+))
		(gl-load-identity)	  ;;Setting up the camera position
		(gl-rotate :x 25)
		(gl-translate :y -15 :z -25)
		(gl-rotate :x 45 :y 90)
		
		;;Positioning our light statically
		(gl-light +GL-LIGHT0+ +GL-POSITION+ (15 15 15))
		
		
		;;Rotate the cube
		(gl-rotate :x (incf *rotation*) :y (incf *rotation*) :z (incf *rotation*))
		;;Drawing the cube
		(gsl-with-depthtest
		  (gsl-draw-cube :sizex 20)))
  ;;Lighting is disabled again here

  ;;Make sure that blending will take into account our alpha
  (gsl-with-blendfunc (+GL-SRC-ALPHA+ +GL-ONE-MINUS-SRC-ALPHA+)
	(gsl-with-textures
		(gsl-with-color (:list '(1 1 0 0.33))
			(gsl-draw-rect :fill-screen t :with-fbo *fbo*))))
  
  (gl-swap-buffers))

(loop
  (input)
  (draw)
  (sdl-delay 50))
