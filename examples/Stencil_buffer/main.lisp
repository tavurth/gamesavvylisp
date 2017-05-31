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

;;	GLOBAL-VARS
;;Setting up our camera;					(deylen - 14/5/2009)
(defvar *camera* (list 0 0 -150))

;;Loading textures.
(defconstant *grass01*  (gsl-load-tex "grass.tga"))
(defconstant *house01*  (gsl-load-tex "house.tga"))

;;Creating our house rectangles.
(defconstant *rect1* (gsl-make-rect 50 0 50 50))
(defconstant *rect2* (gsl-make-rect -50 -50 50 100))


;;Changed the below to show the new event callbacks
(defun mouse-movement (motionx motiony)
  (decf (first *camera*) motionx)
  (decf (second *camera*) motiony))

(defun key-event (key type)
  (when (equalp type +SDL-KEYDOWN+)
    (cond
      ((equalp key +SDLK-BACKQUOTE+) (gsl-enter-console))
      ((equalp key +SDLK-ESCAPE+) (gsl-quit)))))

(gsl-add-key-event-func #'key-event)
(gsl-add-mouse-motion-func #'mouse-movement)

;;These functions are called each frame

(defun draw ()

  ;;General Drawing
  (gl-clear (logior +GL-COLOR-BUFFER-BIT+ +GL-DEPTH-BUFFER-BIT+ +GL-STENCIL-BUFFER-BIT+))
  (gl-load-identity)
  (gl-translate :list *camera*)
  
  ;;We want to draw nearly all of the scene using textures
  ;;Withought this line (gsl-with-textures) textures will show as white.
  ;;You can also call (gl-enable +GL-TEXTURE-2D+) and then (gl-disable +GL-TEXTURE-2D+)
  ;;to enable and disable them manually if you wish, or call :with-tex when drawing a rect instead of :tex

  (gsl-with-stenciltest
    (gsl-with-textures
       ;;Drawing the terrain for the first time
       (gsl-with-color (:list '(0.5 0.5 0.5))	;;drawing the terrain with low brightness
		       (gsl-draw-rect :x -125 :y -125 :w 250 :h 250 :tex *grass01*))

       ;;Drawing the shadows from the rectangles, using the camera as the light
       (gsl-with-colormask (0 0 0 0)
           (gsl-with-stencilop (+GL-KEEP+ +GL-KEEP+ +GL-REPLACE+)
           	(gsl-with-stencilfunc (+GL-ALWAYS+ 1 1)
           		(gsl-draw-shadow *rect1* (mirror (first *camera*)) (mirror (second *camera*)))
           		(gsl-draw-shadow *rect2* (mirror (first *camera*)) (mirror (second *camera*))))))

       ;;Drawing the ground for a second time (full color this time)
       (gsl-with-stencilfunc (+GL-NOTEQUAL+ 1 1)
			     (gsl-draw-tex *grass01* -125 -125 0 250 250))
       
       ;;Drawing the rectangles themselves
       (gsl-with-color (:list '(0.5 0.5 0.5))
		      (gsl-draw *rect1* :tex *house01*)
		      (gsl-draw *rect2* :tex *house01*))))

  ;;After this point textures are disabled
  ;;
  ;;However if we want to draw a single texture withought the use of (gsl-with-textures) or using (gl-enable +GL-TEXTURE-2D+)
  ;;We can use the line below:
  ;;
  ;;(gsl-draw-rect -125 -125 :w 250 :h 250 :with-tex *grass01*)
  ;;
  ;;Using :with-tex instead of :tex automatically enables textures for you and then disables them once the single texture has been drawn.
  ;;This is slower than using gsl-with-textures however (it enables and disables textures for each image) so use it sparingly.

  ;;Drawing the target point
  (gsl-draw-points
    ;;Mirror inverts the arg that is sent to it (- 0 (first *camera*))
    (mirror (first *camera*)) (mirror (second *camera*)))

  (when *GSL-IS-IN-CONSOLE*
    (gsl-draw-console))
  (gl-swap-buffers))

(defun input ()
  (if *GSL-IS-IN-CONSOLE*
    (gsl-console-input)
    (gsl-pump-events)))

(loop
  (input)
  (draw)
  (sdl-delay 50))
