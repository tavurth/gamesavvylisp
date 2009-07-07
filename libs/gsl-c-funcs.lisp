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

(in-package :gsl-c-funcs)

(open-shared-library (gsl-clib-relative "gsl-bindings"))

;;Drawing;;{{{

(new-c-func _gsl_draw_tex		"gsl_draw_tex"		((id int) (x float) (y float) (z float) (w float) (h float)
									  (repeatX float) (repeatY float)))
(new-c-func gsl_draw_rect_shadow	"gsl_draw_rect_shadow"	((x int) (y int) (x1 int) (y1 int) (x2 int) (y2 int)))
(new-c-func gsl_draw_cube		"gsl_draw_cube"		((x int) (y int) (z int) (sizex int) (sizey int) (sizez int)
									 (anglex single-float) (angley single-float) (anglez single-float)
									 (centered int)
									 (tex_left int) (tex_right int) (tex_front int) (tex_back int)
									 (tex_top int) (tex_bot int)))
;;}}}

;;Initialisation;;{{{
(new-c-func gsl_quit			"gsl_quit"		nil)
(new-c-func _gsl_load_texture		"gsl_load_tex"		((loc address) (filter_min int) (filter_mag int) (wrap_s int) (wrap_t int)) :address)
(new-c-func gsl_init			"gsl_init"		((flags int) (options int)))
(new-c-func gsl_init_video		"gsl_init_video"	((width int) (height int) (bpp int) (flags int) (options int) (fov int)
									     (near_clip single-float) (far-clip int)))
;;}}}

;;Fonts;;{{{
(new-c-func _gsl_new_font		"gsl_new_font"		((loc address)) :int)
(new-c-func gsl_draw_char		"gsl_draw_char"		((font int) (character char) (x int) (y int) (z int) (w int) (h int)))
;;}}}

;;	Events;;{{{
(new-c-func gsl_get_key			"gsl_get_key"			((key int))  	:int)
(new-c-func gsl-pump-events		"gsl_pump_events"		nil)
(new-c-func gsl_mouse_motion		"gsl_mouse_motion"		((type short))  :int)
(new-c-func gsl-get-mods		"gsl_get_mods"			nil  		:int)
(new-c-func gsl_skip_events		"gsl_skip_events"		((time :int)))
(new-c-func gsl_set_mouse_event_func	"gsl_set_mouse_event_func"	((func address)))
(new-c-func gsl_set_mouse_move_func	"gsl_set_mouse_move_func" 	((func address)))
(new-c-func gsl_set_key_event_func	"gsl_set_key_event_func"	((func address)));;}}}

;;	Fbo;;{{{
(new-c-func	gsl_new_framebuffer	"gsl_new_framebuffer"	((width int) (height int)) :int)
(new-c-func	gsl_fbo_use		"gsl_use_framebuffer"	((id int)))
(new-c-func	gsl_fbo_add_color	"gsl_fbo_add_color"	((id int) (pos int)) :int)
(new-c-func	gsl_fbo_add_depth	"gsl_fbo_add_depth"	((id int)) :int)
(new-c-func	gsl_fbo_del		"gsl_fbo_del"		((id int)));;}}}

;;	Shader;;{{{
(new-c-func	gsl_new_shader		"gsl_new_shader"	((vert address) (frag address)) :int)
(new-c-func	_gsl_shader_source	"gsl_shader_source"   	((shader int) (vert address) (frag address)))
(new-c-func	gsl_use_shader		"gsl_use_shader"	((shader int)))
(new-c-func	_gsl_new_shader_var	"gsl_new_shader_var"    ((program int) (name address)) :int)
(new-c-func     gsl_set_shader_var	"gsl_set_shader_var_f"	((program int) (var int) (value float)));;}}}

;;	Utils;;{{{
(new-c-func	gsl_get_angle		"gsl_get_angle"		((x int) (y int) (x2 int) (y2 int)) :float)
(new-c-func	gsl_get_dist		"gsl_get_dist"		((x int) (y int) (x2 int) (y2 int)) :float)
(new-c-func	gsl_to_degrees		"gsl_to_degrees"	((angle_rad float)) :float)
(new-c-func	gsl_to_radians		"gsl_to_radians"	((angle_deg float)) :float)
(new-c-func	gsl_x_in_rect		"gsl_x_in_rect"		((x int) (y int) (x2 int) (y2 int) (w int) (h int)) :int)
;;}}}

(new-c-func	gsl_should_load_updates "gsl_should_load_updates"nil :int)
(new-c-func	gsl_set_update_file	"gsl_set_update_file"   ((loc address)))

(defun gsl_load_texture (loc filter-min filter-mag wrap-s wrap-t)
  "The C binding for loading a texture"
  (with-cstrs ((cloc loc))
	 (_gsl_load_texture cloc filter-min filter-mag wrap-s wrap-t)))

(defmacro gsl-set-update-file (str)
  "Sets the update file for loading code dynamically at runtime"
  `(with-cstrs ((var ,str))
     (gsl_set_update_file var)))

(defmacro gsl_new_font (loc)
  "C binding for creating a new font"
  `(with-cstrs ((var ,loc))
	      (_gsl_new_font var)))

(defmacro gsl_shader_source (shader vert frag)
  "Change shader source code"
  `(with-cstrs ((vert_cstring ,vert) (frag_cstring ,frag))
	       (_gsl_shader_source ,shader vert_cstring frag_cstring)))

(defmacro gsl_new_shader_var (program name)
  `(with-cstrs ((c-name ,name))
	       (_gsl_new_shader_var ,program c-name)))

(defun gsl_draw_tex (id x y z w h &optional (repeatX 0.0) (repeatY 0.0))
  "Call _gsl_draw_tex in a safer manner"
  (_gsl_draw_tex id x y z w h (float repeatX) (float repeatY)))

(defmacro gsl-get-angle (x1 y1 x2 y2)
  "Gets the angle from <x1,y1> to <x2,y2>"
  `(gsl_get_angle (truncate ,x1) (truncate ,y1) (truncate ,x2) (truncate ,y2)))

(defmacro gsl-get-dist (x1 y1 x2 y2)
  "Gets the 2D distance from <x1,y1> to <x2,y2>"
  `(gsl_get_dist (truncate ,x1) (truncate ,y1) (truncate ,x2) (truncate ,y2)))

(defmacro gsl-to-degrees (angle)
  `(gsl_to_degrees (float ,angle)))

(defmacro gsl-to-radians (angle)
  `(gsl_to_radians (float ,angle)))

(defmacro gsl-x-in-rect (x y x2 y2 w h)
  `(when (equalp (gsl_x_in_rect (truncate ,x) (truncate ,y) (truncate ,x2) (truncate ,y2) (truncate ,w) (truncate ,h)) 1) t))
