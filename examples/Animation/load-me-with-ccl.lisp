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
(gsl-init :options +GSL-DEFAULT-VIDEO+)

;;	Creating our animation (list of all the frames with a couple of parameters)
(defparam *animation01* (gsl-animation-new))
(gsl-animation-add-frame *animation01* "sign-working.tga")
(defparam *broken-frame* (gsl-animation-add-frame *animation01* "sign-broken.tga" :speed 500))

;;	Creating an anim (holds timing information and current frame)
(defparam *sign* (gsl-anim-new *animation01*))

(defun input ()
  (gsl-pump-events)
  (when (gsl-get-key +SDLK-ESCAPE+) (gsl-quit)))

(defun update-anims ()
  ;;	Setting the time that the broken frame should be displayed to something random
  (gsl-frame-set-speed *broken-frame* (random 1250))
  ;;	Update the anim (check to see if we should change to the next frame or not)
  (gsl-anim-update *sign*))

(defun draw ()
  (gl-clear +GL-COLOR-BUFFER-BIT+)
  (gl-load-identity)
  (gl-translate :z -50)
  
  ;;Drawing the current animation frame with blending enabled
  (gsl-with-blendfunc (+GL-SRC-ALPHA+ +GL-ONE-MINUS-SRC-ALPHA+)
		      (gsl-draw-rect :x -40 :y -10 :w 80 :h 20 :with-anim *sign*))
  (gl-swap-buffers))

(loop
  (input)
  
  ;;We update once a frame here, but you don't have to
  (update-anims)
  (draw)
  (sdl-delay 50))
