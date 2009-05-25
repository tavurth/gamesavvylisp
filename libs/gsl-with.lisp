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

(in-package :gsl-with)

(defmacro gsl-with-color (color &rest rest);;{{{
  `(progn
     (gl-push-attrib +GL-CURRENT-BIT+)
     (gl-color ,@color)
     ,@rest
     (gl-pop-attrib)));;}}}

(defmacro gsl-with-textures (&rest rest);;{{{
  `(progn
     (gl-enable +GL-TEXTURE-2D+)
     ,@rest
     (gl-disable +GL-TEXTURE-2D+)));;}}}

(defmacro gsl-with-blendfunc (func &rest rest);;{{{
  `(progn
     (gl-push-attrib +GL-COLOR-BUFFER-BIT+)
     (gl-blend-func ,@func)
     ,@rest
     (gl-pop-attrib)));;}}}

(defmacro gsl-with-stencilfunc (args &rest rest);;{{{
  `(progn
     (gl-push-attrib +GL-STENCIL-BUFFER-BIT+)
     (gl-stencil-func ,@args)
     ,@rest
     (gl-pop-attrib)));;}}}

(defmacro gsl-with-stencilop (args &rest rest);;{{{
  `(progn
     (gl-push-attrib +GL-STENCIL-BUFFER-BIT+)
     (gl-stencil-op ,@args)
     ,@rest
     (gl-pop-attrib)));;}}}

(defmacro gsl-with-colormask (color &rest rest);;{{{
  `(progn
     (gl-push-attrib +GL-COLOR-BUFFER-BIT+)
     (gl-color-mask ,@color)
     ,@rest
     (gl-pop-attrib)));;}}}

(defmacro gsl-with-font (font &rest rest);;{{{
  (let ((*GSL-DEFAULT-FONT* font))
    `(progn ,@rest)));;}}}

(defmacro gsl-with-pushmatrix (&rest rest);;{{{
  `(progn
     (gl-push-matrix)
     ,@rest
     (gl-pop-matrix)));;}}}

(defmacro gsl-with-shader (shader &rest rest);;{{{
  `(progn
     (gsl-shader-use ,shader)
     ,@rest
     (gsl-shader-use ,*GSL-CURRENT-SHADER*)));;}}}

(defmacro gsl-with-fbo (fbo &rest rest)
  (if (numberp fbo)
    nil
    `(progn
       (gl-push-attrib +GL-VIEWPORT-BIT+)
       (gl-viewport 0 0 (gsl-fbo-width ,fbo) (gsl-fbo-height ,fbo))
       (gsl-fbo-use ,fbo)
       ,@rest
       (gsl-fbo-use 0)
       (gl-pop-attrib))))
