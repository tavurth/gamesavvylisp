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

(defun input ()
  (gsl-console-input)
  (when (gsl-get-key +SDLK-ESCAPE+) (gsl-quit)))

(defun draw ()
  (gl-clear +GL-COLOR-BUFFER-BIT+)
  (gsl-draw-console)
  (gl-swap-buffers))

(loop
  (input)
  (draw)
  (sdl-delay 50))
