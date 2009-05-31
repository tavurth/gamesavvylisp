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

(load "gsl.lisp")

;;			GLOBAL-VARS;;{{{
(defvar *camera* (list 0 0 -150))	;Setting up our camera;					(deylen - 14/5/2009)
(gsl-init :options (logior +GSL-DEFAULT-VIDEO+ +GSL-GET-MOUSE+))
(gl-enable +GL-STENCIL-TEST+)

;;Using +GSL-DEFAULT-VIDEO+ means GSL will set up our perspective options for us;		(deylen - 14/5/2009)
;;Using +GET-MOUSE+	means GSL will capture all mouse events for us;				(deylen - 14/5/2009)

(defconstant *grass01*  (gsl-load-tex "grass.tga"))
(defconstant *sand01*   (gsl-load-tex "sand.tga"))
(defconstant *house01*  (gsl-load-tex "house.tga"))

(defconstant *rect1* (gsl-make-rect 50 0 50 50))
(defconstant *rect2* (gsl-make-rect -50 -50 50 100))
(defconstant *font1* (gsl-new-font "font"))

(const +LEFT-KEY+  +SDLK-a+)
(const +RIGHT-KEY+ +SDLK-s+)
(const +UP-KEY+    +SDLK-w+)
(const +DOWN-KEY+  +SDLK-r+);;}}}

(defun main-loop ();;{{{
  (loop
    (input)
    (draw)
    (sdl-delay 50)));;}}}

(defun draw ();;{{{

  ;;General Drawing
  (gl-clear (logior +GL-COLOR-BUFFER-BIT+ +GL-DEPTH-BUFFER-BIT+ +GL-STENCIL-BUFFER-BIT+))
  (gl-load-identity)
  (gl-translate :pos *camera*)
  
  ;;We want to draw nearly all of the scene using textures
  ;;Withought this line (gsl-with-textures) textures will show as white. You can also call (gl-enable +GL-TEXTURE-2D+) and then (gl-disable +GL-TEXTURE-2D+)
  ;;to enable and disable them manually if you wish.
  ;;
  (gsl-with-textures

       ;;Drawing the terrain for the first time
       (gsl-with-color (0.5 0.5 0.5)
           (gsl-draw-rect :x -125 :y -125 :w 250 :h 250 :tex *grass01*))

       ;;Drawing the shadows from the rectangles here
       (gsl-with-colormask (0 0 0 0)
           (gsl-with-stencilop (+GL-KEEP+ +GL-KEEP+ +GL-REPLACE+)
           	(gsl-with-stencilfunc (+GL-ALWAYS+ 1 1)
           		(gsl-draw-shadow *rect1* (mirror (first *camera*)) (mirror (second *camera*)))
           		(gsl-draw-shadow *rect2* (mirror (first *camera*)) (mirror (second *camera*))))))

       ;;Drawing the ground for a second time
       (gsl-with-stencilfunc (+GL-NOTEQUAL+ 1 1)
			     (gsl-draw-tex *grass01* -125 -125 0 250 250))
       
       ;;Drawing the rectangles themselves
       (gsl-with-color (0.5 0.5 0.5)
		      (gsl-draw *rect1* :tex *house01*)
		      (gsl-draw *rect2* :tex *house01*)))

  ;;After this point textures are disabled
  ;;
  ;;However if we want to draw a single texture withought the use of (gsl-with-textures) or using (gl-enable +GL-TEXTURE-2D+)
  ;;We can use the line below:
  ;;
  ;;(gsl-draw-rect -125 -125 :w 250 :h 250 :with-tex *grass01*)
  ;;
  ;;Using :with-tex instead of :tex automatically enables textures for you and then disables them once the single texture has been drawn.
  ;;This is slower than using with-textures however so use sparingly.

  ;;Drawing the target point
  (gsl-draw-points
    (mirror (first *camera*)) (mirror (second *camera*)))

  (when *GSL-IS-IN-CONSOLE*
    (gsl-draw-console))
  (gl-swap-buffers));;}}}

(defun input ();;{{{
  "All keyboard input is captured here"

  (gsl-load-updates)
  ;;If the user is currently in console mode, go to a different event handling system	(deylen - 14/5/2009)
  (when *GSL-IS-IN-CONSOLE*
    (gsl-console-input)
    (return-from input))

  ;;Pumping all waiting events so we don't have to process them one at a time;		(deylen - 14/5/2009)
  (gsl-pump-events)
  
  ;;Move the camera in the X and Y coords in relation to mouse movement;		(deylen - 14/5/2009)
  (decf (first *camera*) (gsl-mouse-motion +x+))
  (decf (second *camera*) (gsl-mouse-motion +y+))

  ;;Console key;
  (when (gsl-get-key +SDLK-BACKQUOTE+) 
    (gsl-enter-console))
  ;;Exit key;
  (when (gsl-get-key +SDLK-ESCAPE+) (gsl-quit))
  ;;Reload this source file :D
  ;(when (gsl-get-key +SDLK-SPACE+) (load "test-sdl.lisp"))

  ;;Left and right movement;
  (cond ((gsl-get-key +LEFT-KEY+) (incf (first *camera*) 5))
	((gsl-get-key +RIGHT-KEY+) (decf (first *camera*) 5)))

  ;;Up down movement;
  (cond ((gsl-get-key +UP-KEY+) (decf (second *camera*) 5))
	((gsl-get-key +DOWN-KEY+) (incf (second *camera*) 5))));;}}}

(main-loop)
