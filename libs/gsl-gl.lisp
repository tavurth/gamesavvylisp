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

(in-package :gsl-gl)
(load (gsl-lisp-relative "gsl-gl-bits.lisp"))

(open-shared-library (gsl-clib-relative "gsl-gl-bindings"))

;;	C OpenGL functions;;{{{

;;Buffer functions;;{{{
(new-c-func gl-swap-buffers 	"swap_buffers"		nil)
(new-c-func gl-bind-renderbuffer "gl_bind_renderbuffer" ((id int)))
;;}}}

;;Draw functions;;{{{
(new-c-func gl-end 		"gl_end" 		nil)
(new-c-func gl-begin 		"gl_begin" 		((flags int)))
(new-c-func gl-clear 		"gl_clear" 		((flags int)))
(new-c-func gl_vertex_3f 	"gl_vertex_3f" 		((x single-float) (y single-float) (z single-float)))
(new-c-func gl_tex_coord_2d	"gl_tex_coord_2d"	((x single-float) (y single-float)));;}}}

;;Position and perspective;;{{{
(new-c-func gl-load-identity 	"gl_load_identity" 	nil)
(new-c-func gl_translate 	"gl_translate" 		((x single-float) (y single-float) (z single-float)))
(new-c-func gl_rotate		"gl_rotate"		((amount single-float) (x single-float) (y single-float) (z single-float)))
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

(new-c-func	gl_light		"gl_light"	     ((light int) (pname int) (param single-float)))
(new-c-func	gl_light_fv		"gl_light_fv"	     ((light int) (pname int) (paramlist address)))
(new-c-func     gl_material		"gl_material"	     ((face int) (pname int) (param single-float)))
(new-c-func	gl_material_fv		"gl_material_fv"     ((face int) (pname int) (params address)))
(new-c-func	gl-viewport		"gl_viewport"	     ((x int) (y int) (width int) (height int)))
(new-c-func	gl-delete-texture	"gl_delete_texture"  ((id int)))

;;Stencil;;{{{
(new-c-func gl-stencil-func	"gl_stencil_func"	((func int) (ref int) (mask int)))
(new-c-func gl-stencil-op	"gl_stencil_op"		((sfail int) (dfail int) (sdpass int)));;}}};;}}}

;;	Lisp wrappers to C functions;;{{{

(defmacro glu-perspective (&key (fov 90) (aspect (/ (* *width* 1.0) *height*)) (near_clip 0.1) (far_clip 10000.0));;{{{
  `(glu_perspective ,fov ,aspect ,near_clip ,far_clip));;}}}

(defmacro gl-translate (&key (x 0.0) (y 0.0) (z 0.0) (list nil p_supplied));;{{{
  (if p_supplied
    `(gl_translate (float (or (first ,list) 0.0)) (float (or (second ,list) 0.0)) (float (or (third ,list) 0.0)))
    `(gl_translate (float ,x) (float ,y) (float ,z))))	;Else just use x y z					(deylen - 14/5/2009);;}}}

(defmacro gl-rotate (&key (x nil) (y nil) (z nil));;{{{
    ;;Push the different rotation operations to a list (all at once are allowed)
    (let ((list nil))
      (when x (push `(gl_rotate (float ,x) 1.0 0.0 0.0) list))
      (when y (push `(gl_rotate (float ,y) 0.0 1.0 0.0) list))
      (when z (push `(gl_rotate (float ,z) 0.0 0.0 1.0) list))
      `(progn ,@list)))
;;}}}

(defmacro gl-vertex (&key (x 0.0) (y 0.0) (z 0.0) (list nil list-passed));;{{{
  (if list-passed
    `(gl_vertex_3f (float (or (first ,list) ,x) 0.0) (float (or (second ,list) ,y) 0.0) (float (or (third ,list) ,z) 0.0))
    `(gl_vertex_3f (float ,x 0.0) (float ,y 0.0) (float ,z 0.0))));;}}}

(defun gl-tex-coord (x y);;{{{
  (gl_tex_coord_2d (float x 0.0) (float y 0.0)));;}}}

(defmacro gl-color (&key (r 0.0) (g 0.0) (b 0.0) (a 1.0) (list nil list-passed));;{{{
  (if list-passed
    `(gl_color_4f (float (or (first ,list) ,r)) (float (or (second ,list) ,g)) (float (or (third ,list) ,b)) (float (or (fourth ,list) ,a)))
    `(gl_color_4f (float ,r) (float ,g) (float ,b) (float ,a))))
;;}}}

(defmacro gl-light (light pname param);;{{{
  "Compile-time calculation of correct glLight function to call"
  (if (consp param)
    `(let-float-array (array ,param :length 4)
	(gl_light_fv ,light ,pname array))
    `(gl_light ,light ,pname ,(float param))))  ;;If param was a single number just call gl_light;;}}}

(defmacro gl-material (material pname param);;{{{
  "Compile-time calculation of correct glLight function to call"
  (if (consp param)
    `(let-float-array (array ,param :length 4) ;create a temp float array and copy param into it
	(gl_material_fv ,material ,pname array))
    `(gl_material ,material ,pname ,(float param))))  ;;If param was a single number just call gl_material;;}}}

(defmacro gl-clear-color (&optional (r 0.0) (g 0.0) (b 0.0) (a 1.0));;{{{
  `(gl_clear_color ,(float r) ,(float g) ,(float b) ,(float a)));;}}};;}}}
