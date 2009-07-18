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
;;Initialising GSL
;;+GSL_DEFAULT_VIDEO+ Sets up GSL with the optimal video settings
;;+GSL_GET_MOUSE+     Captures all mouse output as well as hiding the mouse
(gsl-init :options (logior +GSL-DEFAULT-VIDEO+ +GSL-GET-MOUSE+))

(defun input ()
  ;;Load any pending gsl source code updates. (see manual) 
  (gsl-load-updates)
  (when *GSL-IS-IN-CONSOLE*
    (gsl-console-input)
    (return-from input))
  ;;gsl-pump-events pumps all event information and updates the mouse structures etc accordingly
  ;;You can retrieve the mouse movement with (gsl-mouse-motion)
  (gsl-pump-events)
  (when (gsl-get-key +SDLK-ESCAPE+) (quit))

  ;;This line is neccesary if you want to use the gcl-console in your app
  (when (gsl-get-key +SDLK-BACKQUOTE+) (gsl-enter-console)))

(defun draw ()
  (gl-clear (logior +GL-COLOR-BUFFER-BIT+))	;Clearing OpenGL's color buffer only
  (gl-load-identity)	
  (gl-translate :z -50)				;You can use :x :y :z and :pos for translating. Pos is a list of 3 numbers (x y z).
  (gsl-draw-rect :x -10 :y -10 :w 20 :h 20)	;You can specify :tex to apply a texture. 
 						;However doing this requires that you have a (gsl-with-textures) call above it.
  						;eg.
						;
						;(gsl-with-textures
						;	(gsl-draw-rect -10 -10 :w 20 :h 20 :tex *sometexture*))

  (when *GSL-IS-IN-CONSOLE*
    (gsl-draw-console))				;Drawing the gsl console if required.
  (gl-swap-buffers))

;; Main loop;
(loop
  (input)
  (draw)
  (sdl-delay 50))
