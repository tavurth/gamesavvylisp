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

(in-package :gsl-c-funcs)

(default-foreign-language :stdc)
(default-foreign-library (gsl-clib-relative "gsl-bindings")) 

;;Drawing;;{{{
(new-c-func gsl_draw_tex		"gsl_draw_tex"		((id int) (x int) (y int) (z int) (w int) (h int)))
(new-c-func gsl_draw_rect_shadow	"gsl_draw_rect_shadow"	((x int) (y int) (x1 int) (y1 int) (x2 int) (y2 int)))
;;}}}

;;Initialisation;;{{{
(new-c-func gsl_quit			"gsl_quit"		nil)
(new-c-func gsl_load_texture		"gsl_load_tex"		((loc c-string)) (:return-type (c-ptr (c-array uint32 3)) :malloc-free))
(new-c-func gsl_init			"gsl_init"		((flags int) (options int)))
(new-c-func gsl_init_video		"gsl_init_video"	((width int) (height int) (bpp int) (flags uint32)))
;;}}}

;;Fonts;;{{{
(new-c-func gsl_new_font		"gsl_new_font"		((loc c-string)) (:return-type int))
(new-c-func gsl_draw_char		"gsl_draw_char"		((font int) (character char) (x int) (y int) (z int) (w int) (h int)))
;;}}}

;;	Events;;{{{
(new-c-func gsl_get_key			"gsl_get_key"		((key int)) (:return-type int))
(new-c-func gsl_pump_events		"gsl_pump_events"	nil)
(new-c-func gsl_mouse_motion		"gsl_mouse_motion"	((type short)) (:return-type int))
(new-c-func gsl_get_charkey		"gsl_get_charkey"	nil (:return-type c-string))
(new-c-func gsl_get_mods		"gsl_get_mods"		nil (:return-type int))
(new-c-func gsl_skip_events		"gsl_skip_events"	((time int)));;}}}

(new-c-func	gsl_new_shader		"gsl_new_shader"	((vert c-string) (frag c-string)) (:return-type int))
(new-c-func	gsl_shader_source	"gsl_shader_source"   	((shader Uint32) (type int) (loc c-string)))
(new-c-func	gsl_use_shader		"gsl_use_shader"	((shader Uint32)))
(new-c-func	gsl_new_framebuffer	"gsl_new_framebuffer"	((width int) (height int)) (:return-type Uint32))
(new-c-func	gsl_fbo_add		"gsl_fbo_add"		((Uint32 int) (type int) (pos Uint32)) (:return-type Uint32))
(new-c-func	gsl_fbo_use		"gsl_use_framebuffer"	((id int)))
(new-c-func	gsl_fbo_del		"gsl_fbo_del"		((id int)))
(new-c-func	gsl_should_load_updates "gsl_should_load_updates"nil (:return-type int))
(new-c-func	gsl-set-update-file	"gsl_set_update_file"   ((loc c-string)))
