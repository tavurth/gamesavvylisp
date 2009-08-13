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

(defpackage :gsl-shared
  (:use :ccl :common-lisp :gsl-init)
  (:export :const 
	   :print-macro
	   :gsl-load-all
	   :gsl-exec-after-init
	   :gsl-exec-after-video
	   :defparam
	   :default-foreign-lib
	   :export-all
	   :defmacro-export
	   :defun-export
	   :gsl-get-angle
	   :gsl-get-dist
	   :gsl-draw-gui-line
	   :gsl-to-degrees
	   :gsl-to-radians
	   :gsl-x-in-rect
	   :use-packages
	   :let-float-array
	   :do-array
	   :while
	   :del-from-array
	   :new-c-func 
	   :setf-all
	   :last-val
	   :mirror))

(defpackage :gsl-event-funcs
  (:use :ccl :common-lisp :gsl-shared :gsl-init)
  (:export :gsl-add-event-func
	   :gsl-add-mouse-motion-func
	   :gsl-add-mouse-event-func
	   :gsl-add-key-event-func
	   :gsl-make-event-func
	   :gsl-delete-event
	   :gsl-delete-mouse-motion-func
	   :gsl-delete-mouse-event-func
	   :gsl-add-key-event-func ))

(defpackage :gsl-c-funcs
  (:use :ccl :common-lisp :gsl-shared :gsl-init)
  (:export :gsl_init
	   :gsl_init_video
	   :gsl_new_font
	   :gsl_draw_tex
	   :gsl_draw_cube
	   :gsl-pump-events
	   :gsl_load_texture
	   :gsl_load_texture2
	   :gsl_skip_events
	   :gsl-get-mods
	   :gsl_quit
	   :gsl_mouse_motion
	   :gsl_get_key
	   :gsl_get_charkey
	   :gsl_draw_char
	   :gsl_draw_rect_shadow
	   :gsl_new_shader
	   :gsl_shader_source
	   :gsl_set_mouse_event_func
	   :gsl_set_mouse_motion_func
	   :gsl_set_key_event_func
	   :gsl_use_shader
	   :gsl_new_shader_var
	   :gsl_set_shader_var
	   :gsl_new_framebuffer
	   :gsl_fbo_add_color
	   :gsl_fbo_add_depth
	   :gsl_tex_glid
	   :gsl_fbo_use
	   :gsl_fbo_del
	   :gsl_should_load_updates
	   :gsl-set-update-file))


(defpackage :gsl-gl-bits
  (:use :ccl :common-lisp :gsl-shared :gsl-init))

(defpackage :gsl-gl
  (:use :ccl :common-lisp :gsl-shared :gsl-init :gsl-c-funcs :gsl-gl-bits)
  (:export :glu-perspective
	   :gl-clear
	   :gl-clear-color
	   :gl-begin
	   :gl-end
	   :gl-vertex
	   :gl-tex-coord
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
	   :gl-material
	   :gl-light
	   :gl-bind-renderbuffer
	   :gl-bind-texture
	   :gl-blend-func
	   :gl-stencil-op
	   :gl-stencil-func
	   :gl-swap-buffers))


(defpackage :gsl-classes
  (:use :ccl :common-lisp :gsl-shared :gsl-init :gsl-c-funcs :gsl-gl :gsl-gl-bits)
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
	   
	   :gsl-fbo-new
	   :gsl-fbo-add-color
	   :gsl-fbo-del-color
	   :gsl-fbo-add-depth
	   :gsl-fbo-use
	   :gsl-fbo-del
	   :gsl-fbo-id
	   :gsl-fbo-width
	   :gsl-fbo-height
	   :gsl-fbo-color-pos
	   :gsl-delete-all-fbos
	   :gsl-fbo-color-attachments
	   :gsl-fbo-depth
	   
	   :gsl-shader-new
	   :gsl-shader-set
	   :gsl-shader-get
	   :gsl-shader-vars
	   :gsl-shader-use
	   :gsl-shader-id
	   :gsl-shader-delete-vars
	   :gsl-shader-source))


(defpackage :gsl-string
  (:use :ccl :common-lisp :gsl-shared :gsl-init)
  (:export :push-string
	   :push-char
	   :backspace
	   :del-from-string))


(defpackage :gsl-sdl
  (:use :ccl :common-lisp :gsl-shared :gsl-init)
  (:export :sdl-delay
	   :sdl-get-key-name
	   :sdl-get-ticks))


(defpackage :gsl-globals
  (:use :ccl :common-lisp :gsl-shared :gsl-init :gsl-gl :gsl-sdl :gsl-classes :gsl-c-funcs))


(defpackage :gsl-updates
  (:use :ccl :common-lisp :gsl-shared :gsl-init :gsl-c-funcs :gsl-globals)
  (:export :gsl-should-load-updates
	   :gsl-load-updates))

(defpackage :gsl-with
  (:use :ccl :common-lisp :gsl-gl :gsl-globals :gsl-classes :gsl-gl-bits)
  (:export :gsl-with-color
	   :gsl-with-colormask
	   :gsl-with-draw
	   :gsl-with-textures
	   :gsl-with-alpha
	   :gsl-with-stencilfunc
	   :gsl-with-blendfunc
	   :gsl-with-pushmatrix
	   :gsl-with-translate
	   :gsl-with-load-identity
	   :gsl-with-rotate
	   :gsl-with-font
	   :gsl-with-depthtest
	   :gsl-with-stenciltest
	   :gsl-with-stencilop
	   :gsl-with-lights
	   :gsl-with-shader
	   :gsl-with-fbo))


(defpackage :gsl-input
  (:use :ccl :common-lisp :gsl-shared :gsl-init :gsl-sdl :gsl-globals :gsl-c-funcs :gsl-string :gsl-event-funcs)
  (:export :gsl-mouse-motion
	   :gsl-get-key
	   :gsl-get-mods
	   :gsl-pump-events
	   :gsl-get-mods
	   :gsl-skip-events
	   :gsl-read-input-to-string
	   :gsl-get-charkey
	   :gsl-keys))


(defpackage :gsl-animation
  (:use :ccl :common-lisp :gsl-shared :gsl-init :gsl-classes :gsl-sdl :gsl-c-funcs)
  (:export :gsl-animation-new
	   :gsl-animation-add-frame
	   :gsl-animation-frame
	   :gsl-animation-frames

	   :gsl-anim-new
	   :gsl-anim-update
	   :gsl-anim-current-tex
	   :gsl-anim-update-time
	   :gsl-anim-next-frame

	   :gsl-frame-set-speed
	   :gsl-animation-frame-speed))

(defpackage :gsl-draw
  (:use :ccl :common-lisp :gsl-shared :gsl-init :gsl-globals :gsl-classes :gsl-gl :gsl-c-funcs :gsl-with :gsl-gl-bits :gsl-animation)
  (:export :gsl-draw
	   :gsl-draw-points
	   :gsl-draw-tex
	   :gsl-draw-char
	   :gsl-draw-string
	   :gsl-draw-rect
	   :gsl-draw-cube
	   :gsl-draw-tri
	   :gsl-draw-quad))


(defpackage :gsl-gui
  (:use :ccl :common-lisp :gsl-globals :gsl-shared :gsl-init :gsl-classes :gsl-gl :gsl-gl-bits
	:gsl-with :gsl-draw :gsl-sdl :gsl-input :gsl-c-funcs :gsl-event-funcs)
  (:export :gsl-gui-draw
	   :gsl-gui-new
	   :gsl-gui-set-height
	   :gsl-gui-set-width
	   :gsl-gui-draw-border
	   :gsl-gui-draw-corner
	   :gsl-gui-draw-borders
	   :gsl-gui-draw-corners
	   :gsl-gui-draw-box
	   :gsl-gui-draw-all
	   :gsl-gui-mouse-motion
	   :gsl-gui-set
	   :gsl-gui-set-bg-color
	   :gsl-mouse-is-on-gui
	   :gsl-gui-mouse-input))

(defpackage :gsl-console
  (:use :ccl :common-lisp :gsl-shared :gsl-init :gsl-input :gsl-sdl :gsl-string :gsl-globals :gsl-draw :gsl-gl :gsl-gl-bits :gsl-with :gsl-event-funcs)
  (:export :gsl-console-input
	   :gsl-draw-console
	   :gsl-enter-console
	   :gsl-print-console
	   :gsl-clear-console))

(defpackage :gsl
  (:use :ccl :common-lisp :gsl-shared :gsl-init :gsl-gl :gsl-input :gsl-sdl :gsl-globals :gsl-draw
	:gsl-classes :gsl-c-funcs :gsl-string :gsl-gui)
  (:export :gsl-init
	   :gsl-init-from-image
	   :gsl-quit
	   :gsl-init-video
	   :gsl-new-font
	   :gsl-draw-char
	   :gsl-draw-string
	   :gsl-draw-tex
	   :gsl-draw-points
	   :gsl-draw-rect
	   :gsl-draw
	   :gsl-use-input
	   :gsl-use-video
	   :use-all))

(defpackage :gsl-point
  (:use :ccl :common-lisp :gsl-shared)
  (:export :gsl-point-new
	   :gsl-point-list))
