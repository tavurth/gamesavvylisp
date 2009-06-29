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
(gsl-init :options +GSL-GET-MOUSE+)
(gsl-init-video :width 1024 :height 512 :options +GSL-DEFAULT-VIDEO+)

(defun input ()
  (gsl-pump-events)
  ;;To get the name of the next key pressed use (gsl-get-charkey)
  ;;To get the relative motion of the mouse use (gsl-mouse-motion <dir>)
  ;;Where <dir> can be either +x+ +y+ or nothing (returns a list of both +x+ and +y+ movement)
  ;;To get the status of a key use (gsl-get-key <KEYNAME>) returns t or nil.

  ;;Quit key
  (when (gsl-get-key +SDLK-ESCAPE+) (gsl-quit)))

(loop
  (input)
  (sdl-delay 50))
