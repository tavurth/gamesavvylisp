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

;;Handling for all mouse button events
;;Your button-event functions need to take two args (the button and the event type)
(defun button-event (button type)
  (cond
    ((equalp type +SDL-MOUSEBUTTONDOWN+) (print "A button was pressed"))
    ((equalp type +SDL-MOUSEBUTTONUP+)   (print "A button was released")))
  (format t "~%Button number: ~a~%Event type: ~a~2%" button type))

;;Handling for all key events
;;Your key-event functions need to take two args (the key and the event type)
(defun key-event (key type)
  (cond
    ((equalp type +SDL-KEYDOWN+) (print "A key was pressed"))
    ((equalp type +SDL-KEYUP+)   (print "A key was released")))
  (format t "~%Key number: ~a~%Event type: ~a~2%" key type)

  ;;	Quit key
  (when (equalp key +SDLK-ESCAPE+) (gsl-quit)))

(defun move-event (motion-x motion-y)
  (format t "Motion-x: ~a~%Motion-y: ~a~2%" motion-x motion-y))

;;	Setting up the callback functions
(gsl-add-mouse-motion-func #'move-event)
(gsl-add-mouse-event-func  #'button-event)
(gsl-add-key-event-func    #'key-event)

(loop
  (gsl-pump-events)
  (sdl-delay 50))
