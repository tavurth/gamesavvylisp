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

(in-package gsl-shader)

(const +GSL-SHADER-VERT+ 0)
(const +GSL-SHADER-FRAG+ 1)

(defmacro gsl-shader-new (&key (vert nil) (frag nil))
  `(gsl_new_shader ,vert ,frag))

(defmacro gsl-shader-source (shader &key (vert "" vert_supplied) (frag "" frag_supplied))
  (let ((list nil))
    (when vert_supplied
      (push `(gsl_shader_source ,shader ,+GSL-SHADER-VERT+ ,vert) list))
    (when frag_supplied
      (push `(gsl_shader_source ,shader ,+GSL-SHADER-FRAG+ ,frag) list))
    (push 'progn list)

    (return-from gsl-shader-source list)))

(defmacro gsl-shader-use (shader)
  `(gsl_use_shader ,shader))
