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

(in-package :gsl-updates)

(defmacro gsl-should-load-updates()
  `(when (> (gsl_should_load_updates) 0) t))

(defun gsl-load-updates ()
  (when 
    (gsl-should-load-updates)
    (progn
      (print "Loading file")
      (load *GSL-UPDATE-FILE*)
      (delete-file *GSL-UPDATE-FILE*))))

