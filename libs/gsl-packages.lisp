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

(defpackage :gsl-c-funcs;;{{{
  (:use :common-lisp :gsl-shared :ffi)
  (:export :gsl_init
	   :gsl_init_video
	   :gsl_new_font
	   :gsl_draw_tex
	   :gsl_pump_events
	   :gsl_load_texture
	   :gsl_load_texture2
	   :gsl_skip_events
	   :gsl_get_mods
	   :gsl_quit
	   :gsl_mouse_motion
	   :gsl_get_key
	   :gsl_get_charkey
	   :gsl_draw_char
	   :gsl_draw_rect_shadow
	   :gsl_new_shader
	   :gsl_shader_source
	   :gsl_use_shader
	   :gsl_new_framebuffer
	   :gsl_fbo_add
	   :gsl_fbo_use
	   :gsl_fbo_del
	   :gsl_should_load_updates
	   :gsl-set-update-file))
;;}}}

(defpackage :gsl-gl;;{{{
  (:use :common-lisp :gsl-shared :gsl-c-funcs)
  (:export :glu-perspective
	   :gl-clear
	   :gl-clear-color
	   :gl-begin
	   :gl-end
	   :gl-vertex
	   :gl-load-identity
	   :gl-translate
	   :gl-rotate
	   :gl-viewport
	   :gl-enable
	   :gl-disable
	   :gl-push-matrix
	   :gl-pop-attrib
	   :gl-push-attrib
	   :gl-pop-matrix
	   :gl-delete-texture
	   :gl-color-mask
	   :gl-color
	   :gl-bind-texture
	   :gl-blend-func
	   :gl-stencil-op
	   :gl-stencil-func
	   :gl-swap-buffers))
;;}}}

(defpackage :gsl-classes;;{{{
  (:use :common-lisp :gsl-shared :gsl-c-funcs :gsl-gl)
  (:export ;;Rect class
           :gsl-make-rect
	   :gsl-draw-shadow
	   :gsl-draw
	  
	   ;;Tex class
	   :tex
	   :gsl-load-tex
	   :gsl-delete-tex
	   :gsl-delete-all-textures
	   :gsl-tex-id
	   :gsl-tex-loc
	   :gsl-tex-nused
	   :gsl-tex-width
	   :gsl-tex-height
	   :gsl-tex-bpp
	   
	   :gsl-fbo-new
	   :gsl-fbo-add-color
	   :gsl-fbo-del-color
	   :gsl-fbo-use
	   :gsl-fbo-del
	   :gsl-fbo-id
	   :gsl-fbo-width
	   :gsl-fbo-height
	   :gsl-fbo-color-pos
	   :gsl-delete-all-fbos
	   :gsl-fbo-color-attachments
	   :gsl-fbo-depth-attachments))
;;}}}

(defpackage :gsl-string;;{{{
  (:use :common-lisp :gsl-shared)
  (:export :new-adjustable-string
	   :push-string
	   :push-char
	   :backspace
	   :del-from-string))
;;}}}

(defpackage :gsl-sdl;;{{{
  (:use :common-lisp :gsl-shared :ffi)
  (:export :sdl-delay))
;;}}}

(defpackage :gsl-globals;;{{{
  (:use :common-lisp :gsl-shared :gsl-sdl :gsl-classes :gsl-c-funcs))
;;}}}

(defpackage :gsl-shader;;{{{
  (:use :common-lisp :gsl-shared :gsl-c-funcs)
  (:export :gsl-shader-new
	   :gsl-shader-source
	   :gsl-shader-use));;}}}

(defpackage :gsl-updates;;{{{
  (:use :common-lisp :gsl-shared :gsl-c-funcs :gsl-globals)
  (:export :gsl-should-load-updates
	   :gsl-load-updates));;}}}

(defpackage :gsl-with;;{{{
  (:use :common-lisp :gsl-gl :gsl-shader :gsl-globals :gsl-classes)
  (:export :gsl-with-color
	   :gsl-with-colormask
	   :gsl-with-textures
	   :gsl-with-stencilfunc
	   :gsl-with-blendfunc
	   :gsl-with-pushmatrix
	   :gsl-with-font
	   :gsl-with-stencilop
	   :gsl-with-shader
	   :gsl-with-fbo))
;;}}}

(defpackage :gsl-input;;{{{
  (:use :common-lisp :gsl-shared :ffi :gsl-sdl :gsl-globals :gsl-c-funcs)
  (:export :gsl-mouse-motion
	   :gsl-get-key
	   :gsl-get-mods
	   :gsl-pump-events
	   :gsl-skip-events
	   :gsl-get-charkey))
;;}}}

(defpackage :gsl-draw;;{{{
  (:use :common-lisp :gsl-shared :gsl-globals :gsl-classes :gsl-gl :gsl-c-funcs :gsl-with)
  (:export :gsl-draw
	   :gsl-draw-points
	   :gsl-draw-tex
	   :gsl-draw-char
	   :gsl-draw-string
	   :gsl-draw-rect
	   :gsl-with-draw))
;;}}}

(defpackage :gsl-console;;{{{
  (:use :common-lisp :gsl-shared :gsl-input :gsl-sdl :gsl-string :gsl-globals :gsl-draw :gsl-gl :gsl-with)
  (:export :gsl-console-input
	   :gsl-draw-console
	   :gsl-enter-console
	   :gsl-print-console
	   :gsl-clear-console))
;;}}}

(defpackage :gsl;;{{{
  (:use :common-lisp :gsl-shared :gsl-gl :gsl-input :gsl-sdl :gsl-globals :gsl-draw :gsl-classes :gsl-c-funcs :gsl-string)
  (:export :gsl-init
	   :gsl-quit
	   :gsl-init-video
	   :gsl-new-font
	   :gsl-draw-char
	   :gsl-draw-string
	   :gsl-draw-tex
	   :gsl-draw-points
	   :gsl-with-draw
	   :gsl-draw-rect
	   :gsl-draw
	   :use-all))
;;}}}

;;Loading libs;;{{{
(load (gsl-lisp-relative "gsl-shared.lisp"))
(load (gsl-lisp-relative "gsl-classes.lisp"))
(load (gsl-lisp-relative "gsl-sdl.lisp"))
(load (gsl-lisp-relative "gsl-c-funcs.lisp"))
(load (gsl-lisp-relative "gsl-globals.lisp"))
(load (gsl-lisp-relative "gsl-gl.lisp"))
(load (gsl-lisp-relative "gsl-updates.lisp"))
(load (gsl-lisp-relative "gsl-shaders.lisp"))
(load (gsl-lisp-relative "gsl-with.lisp"))
(load (gsl-lisp-relative "gsl-draw.lisp"))
(load (gsl-lisp-relative "gsl-input.lisp"))
(load (gsl-lisp-relative "gsl-string.lisp"))
(load (gsl-lisp-relative "gsl-console.lisp"))
(load (gsl-lisp-relative "gsl-main.lisp"))
;;}}}
