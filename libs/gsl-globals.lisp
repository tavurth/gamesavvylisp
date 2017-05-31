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

(in-package :gsl-globals)

;;GLOBAL VARIABLES

(defparam *width*	0)
(defparam *height*      0)
(defparam *aspect-x*    0.0)
(defparam *aspect-y*    0.0)

(const +X+	#x0000001)
(const +Y+	#x0000002)
(defparam *GSL-CURRENT-SHADER* nil)

(defparam *GSL-EXEC-AFTER-INIT*  nil)
(defparam *GSL-EXEC-AFTER-VIDEO* nil)

;;	INITIALISATION FLAGS

(const +GSL-DEFAULT-VIDEO-FLAGS+ (logior +SDL-OPENGL+))

(const +GSL-CATCH-MOUSE+	#x000000001)
(const +GSL-HIDE-MOUSE+		#x000000002)
(const +GSL-GET-MOUSE+		#x000000004)
(const +GSL-DEFAULT-VIDEO+	#x000000008)
(const +GSL-GET-UPDATES+	#x000000010)	;Should we get updates from the source file GSL_UPDATE_FILE while running.

(defparam *GSL-UPDATE-FILE*     "gsl-updates.lisp")	;;Setting the update-code file should we desire to use it
(when (probe-file *GSL-UPDATE-FILE*) 
	(delete-file *GSL-UPDATE-FILE*))			;;Making sure there is no code left from last time.


;;	GLOBAL EVENT FUNCTIONS
(defparam *GSL-MOUSE-EVENT-FUNC* nil)
(defparam *GSL-MOUSE-MOTION-FUNC*  nil)
(defparam *GSL-KEY-EVENT-FUNC*   nil)


;;	CONSOLE

(defparam *console-curr* "")
(defparam *console-prev* nil)
(defparam *console*	 nil) 

(defparam *GSL-DEFAULT-FONT*	nil)
(defparam *GSL-IS-IN-CONSOLE*	nil)
(defparam *GSL-USER-NAME*	"GSL-USER")

(defparam *GSL-MAX-CONSOLE* 	 10)
(defparam *GSL-MAX-CONSOLE-PREV* 10) 
(defparam *GSL-CURSOR-Y*	 0)
(defparam *GSL-CURSOR-X*	 0)

