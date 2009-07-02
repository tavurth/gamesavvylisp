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

;;	Setting up the gui textures and sizes (not required)
;(gsl-gui-set :border-tex (gsl-relative "themes/border.tga") :corner-tex (gsl-relative "themes/corner.tga") :border-size 20 :corner-size 20)

;;	First gui
(defparam *test* (gsl-gui-new -800 -800 1600 1600))
;;	Second gui
(defparam *test2* (gsl-gui-new -1000 -1000 500 500))
(defparam *cursor* '(0 0))

;;Called when a mouse event occurs
(defun mouse-event (button type)
  (when (equalp type +SDL-MOUSEBUTTONDOWN+)
    (when (equalp button 1)
      ;;When the left mouse button is pushed down, call the gui input function with the <x,y> coords of the mouse
      (gsl-gui-mouse-input (first *cursor*) (second *cursor*)))))

;;Called when a keyboard event occurs
(defun input-event (key type)
  (when (equalp type +SDL-KEYDOWN+)
    (when (equalp key +SDLK-ESCAPE+) (gsl-quit))))

;;Setting up the callback functions
(setf *GSL-KEY-EVENT-FUNC*	#'input-event)
(setf *GSL-MOUSE-EVENT-FUNC*	#'mouse-event)

(defun input ()
  (gsl-pump-events)
  ;;Moving our cursor
  (incf (first *cursor*) (gsl-mouse-motion +x+))
  (incf (second *cursor*) (gsl-mouse-motion +y+)))

(defun draw ()
  (gl-clear +GL-COLOR-BUFFER-BIT+)
  (gl-load-identity)
  (gl-translate :z -1024)

  ;;Drawing the gui
  (gsl-gui-draw-all)

  ;;Drawing the cursor
  (gsl-draw-rect :x (first *cursor*) :y (second *cursor*) :w 5 :h 5)

  (gl-swap-buffers))

(loop
  (input)
  (draw)
  (sdl-delay 50))
