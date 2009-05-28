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

(in-package :gsl)
(require (gsl-lisp-relative "gsl-gl-bits.lisp"))

(use-package :ffi)
(default-foreign-language :stdc)
(default-foreign-library (gsl-clib-relative "gsl-gl-bindings"))

;;	C OpenGL functions;;{{{

;;Buffer functions;;{{{
(new-c-func gl-swap-buffers 	"swap_buffers"		nil)
;;}}}

;;Draw functions;;{{{
(new-c-func gl-end 		"gl_end" 		nil)
(new-c-func gl-begin 		"gl_begin" 		((flags int)))
(new-c-func gl-clear 		"gl_clear" 		((flags int)))
(new-c-func gl_vertex_3f 	"gl_vertex_3f" 		((x single-float) (y single-float) (z single-float)));;}}}

;;Position and perspective;;{{{
(new-c-func gl-load-identity 	"gl_load_identity" 	nil)
(new-c-func gl_translate 	"gl_translate" 		((x single-float) (y single-float) (z single-float)))
(new-c-func gl_rotate		"gl_rotate"		((amount int) (x single-float) (y single-float) (z single-float)))
(new-c-func glu_perspective 	"glu_perspective"	((fov int) (aspect single-float) (near_clip single-float) (far_clip single-float)));;}}}

;;Enable / disable;;{{{
(new-c-func gl-enable		"gl_enable"		((flags int)))
(new-c-func gl-disable		"gl_disable"		((flags int)));;}}}

;;Matrix functions;;{{{
(new-c-func gl-push-matrix	"gl_push_matrix"	nil)
(new-c-func gl-pop-matrix	"gl_pop_matrix"		nil)
(new-c-func gl-matrix-mode	"gl_matrix_mode"	((flags int)));;}}}

;;Attrib functions;;{{{
(new-c-func gl-pop-attrib	"gl_pop_attrib"		nil)
(new-c-func gl-push-attrib	"gl_push_attrib"	((mask int)));;}}}

;;Color;;{{{
(new-c-func gl-color-mask	"gl_color_mask"		((r short) (g short) (b short) (a short)))
(new-c-func gl_color_4f 	"gl_color_4f" 		((r single-float) (g single-float) (b single-float) (a single-float)))
(new-c-func gl_clear_color 	"gl_clear_color"	((r single-float) (g single-float) (b single-float) (a single-float)));;}}}

;;Blending;;{{{
(new-c-func gl-bind-texture	"gl_bind_texture"	((target int) (tex int)))
(new-c-func gl-blend-func	"gl_blend_func"		((sfactor int) (dfactor int)));;}}}

(new-c-func	gl-viewport		"gl_viewport"	     ((x int) (y int) (width int) (height int)))
(new-c-func	gl-delete-texture	"gl_delete_texture"  ((id int)))

;;Stencil;;{{{
(new-c-func gl-stencil-func	"gl_stencil_func"	((func int) (ref int) (mask int)))
(new-c-func gl-stencil-op	"gl_stencil_op"		((sfail int) (dfail int) (sdpass int)));;}}};;}}}

;;	Lisp wrappers to C functions;;{{{

(defmacro glu-perspective (&key (fov 75) (aspect (/ (* *width* 1.0) *height*)) (near_clip 0.1) (far_clip 10000.0))
  `(glu_perspective ,fov ,aspect ,near_clip ,far_clip))

(defun gl-translate (&key (x 0.0) (y 0.0) (z 0.0) (pos nil p_supplied))
  (if p_supplied
    (gl_translate (i->float (first pos)) (i->float (second pos)) (i->float (third pos)))
    (gl_translate (i->float x) (i->float y) (i->float z))))	;Else just use x y z					(deylen - 14/5/2009)

(defmacro gl-rotate (&key (x 0) (y 0) (z 0))
  (let ((list nil))
    ;;Push the different rotation operations to a list (all at once are allowed)
    (when x (push `(gl_rotate ,x 1.0 0.0 0.0) list))
    (when y (push `(gl_rotate ,y 0.0 1.0 0.0) list))
    (when z (push `(gl_rotate ,z 0.0 0.0 1.0) list))
    (return-from gl-rotate `(progn ,@list))))

(defun gl-vertex (&optional (x 0.0) (y 0.0) (z 0.0))
  (gl_vertex_3f (i->float x) (i->float y) (i->float z)))

(defmacro gl-color (&optional (r 0.0) (g 0.0) (b 0.0) (a 1.0))
  `(gl_color_4f ,(i->float r) ,(i->float g) ,(i->float b) ,(i->float a)))

(defmacro gl-clear-color (&optional (r 0.0) (g 0.0) (b 0.0) (a 1.0))
  `(gl_clear_color ,(i->float r) ,(i->float g) ,(i->float b) ,(i->float a)));;}}}
