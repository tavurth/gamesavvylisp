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

(in-package :gsl-string)

(defun new-adjustable-string (&optional size);;{{{
  (make-array size
	      :element-type 'character
	      :adjustable t
	      :fill-pointer 0));;}}}

(defmacro push-string (str1 str2);;{{{
  "Push str1 onto the end of str2"
  `(setf ,str2 (concatenate 'string ,str2 ,str1)));;}}}

(defmacro push-char (char string);;{{{
  "Push char onto the end of string"
  `(push-string (string ',char) ,string));;}}}

(defmacro del-from-string (str index);;{{{
  "Deletes char index from string"
  `(del-from-array ,str ,index));;}}}

(defmacro backspace (str);;{{{
  "Removes the last character from the string"
  `(let ((len (length ,str)))
     (if (> len 0)		;So we don't end up with a -1 length array
       (setf ,str (subseq ,str 0 (- len 1)))
       (setf ,str (subseq ,str 0)))))		;use the whole string if (length < 1);;}}}
