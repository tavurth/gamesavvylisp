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

(in-package :gsl-sdl)
(load (gsl-lisp-relative "gsl-sdl-keys.lisp"))

(open-library (gsl-clib-relative "gsl-sdl-bindings"))

;;Globals for intialising SDL;
(const +SDL-INIT-TIMER+ 	#x00000001)
(const +SDL-INIT-AUDIO+ 	#x00000010)
(const +SDL-INIT-VIDEO+ 	#x00000020)
(const +SDL-INIT-CDROM+ 	#x00000100)
(const +SDL-INIT-JOYSTICK+ 	#x00000200)
(const +SDL-INIT-NOPARACHUTE+	#x00100000)
(const +SDL-INIT-EVENTTHREA+	#x01000000)
(const +SDL-INIT-EVERYTHING+	#x0000FFFF)

;;Globals for initialising SDL-VIDEO;
(const +SW-SURFACE+   	#x00000000)
(const +HW-SURFACE+   	#x00000001)
(const +ANY-FORMAT+   	#x10000000)
(const +DOUBLEBUFF+   	#x40000000)
(const +FULLSCREEN+   	#x80000000)
(const +SDL-OPENGL+   	#x00000002)
(const +RESIZEABLE+   	#x00000010)
(const +NOFRAME+      	#x00000020)
(const +HWACCEL+      	#x00000100)
(const +RLEACCEL+	#x00004000)

;;C function declarations;
(new-c-func sdl-pump-events	"sdl_pump_events"	nil)
(new-c-func sdl-quit		"sdl_quit"		nil)
(new-c-func sdl-delay		"sdl_delay" 		((delay int)))
(new-c-func sdl-get-ticks       "sdl_get_ticks"		nil :int)
(new-c-func sdl_init		"sdl_init" 		((flags int)))
(new-c-func sdl_init_video	"sdl_init_video" 	((width int) (height int) (bpp int) (flags int)))
(new-c-func sdl_key_name	"sdl_key_name"	((key int)) :address)

;;Lisp wrappers;
(defmacro sdl-init-video (&rest args)
  `(sdl_init_video ,@args))

(defmacro sdl-get-key-name (key)
  `(%get-cstring (sdl_key_name ,key)))
