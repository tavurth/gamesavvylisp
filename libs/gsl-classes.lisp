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

(in-package :gsl-classes)

(defclass tex ();;{{{
   ((loc
     :initform ""
     :initarg :loc
     :accessor gsl-tex-loc)
    (id
      :initform 0
      :initarg :id
      :accessor gsl-tex-id)    
    (width
      :initform 0
      :initarg :width
      :accessor gsl-tex-width)
    (height
      :initform 0
      :initarg :height
      :accessor gsl-tex-height)
    (nUsed
      :initform 0
      :accessor gsl-tex-nused)))

(defparam *GSL-TEXTURE-HASH* (make-hash-table))
(defparam *GSL-TEXTURE-DEFERRED-LOAD-LIST* nil)

(defun add-tex-to-hash (tex loc);;{{{
  "Adds the passed texture to the *GSL-TEXTURE-HASH*"
  (setf (gethash (read-from-string loc) *GSL-TEXTURE-HASH*) tex))
;;}}}

(defun make-tex (loc &optional (id 0) (width 0) (height 0));;{{{
  "Creates a new texture object"
  (let ((tex (make-instance 'tex :loc loc :id id :width width :height height)))
    (add-tex-to-hash tex (string loc))))
;;}}}

(defmacro gsl-get-tex-from-hash (loc);;{{{
  "Returns the texture at hash loaction <loc>"
  `(gethash (read-from-string ,loc) *GSL-TEXTURE-HASH*));;}}}

(defun copy-extern-to-tex (tex loc filter-min filter-mag wrap-t wrap-s);;{{{
  "Copys an external texture <loc>'s properties into <tex>"
  (let ((extern (gsl_load_texture loc filter-min filter-mag wrap-t wrap-s)))	;;Using lisp C bindings to load the texture
    (setf (gsl-tex-loc tex)	loc)
    (setf (gsl-tex-id tex)	(%get-signed-word extern 0))
    (setf (gsl-tex-width tex) 	(%get-signed-word extern 1))
    (setf (gsl-tex-height tex) 	(%get-signed-word extern 2))
    (free extern)));;}}}

(defun gsl-load-tex-deferred (loc filter-min filter-mag wrap-t wrap-s);;{{{
  "Loads a texture at run-time instead of compile time. Only called when a texture is loaded before *GSL-VIDEO-DONE*"
  (let ((tex (gsl-get-tex-from-hash 'loc)))
    (copy-extern-to-tex tex loc filter-min filter-mag wrap-t wrap-s)));;}}}

(defun gsl-texture-add-new-deferred (loc filter-min filter-mag wrap-t wrap-s);;{{{
  "Adds a new texture that will be loaded when GSL has finished initialising"
  (push `(gsl-load-tex-deferred ,loc ,filter-min ,filter-mag ,wrap-t ,wrap-s) *GSL-TEXTURE-DEFERRED-LOAD-LIST*)
  (make-tex loc))
;;}}}

(defun gsl-load-tex (loc &key (filter-min +GL-LINEAR-MIPMAP-NEAREST+) (filter-mag +GL-NEAREST+) (wrap-t +GL-REPEAT+) (wrap-s +GL-REPEAT+));;{{{
  "Loads and returns a texture, returns the texture and does not load it again if it already exists in *GSL-TEXTURE-HASH*"
  ;;If the texture location is already in the hash table, return the tex.
  (let ((tex (gsl-get-tex-from-hash loc)))
    (when tex (progn
		(incf (gsl-tex-nused tex))	;;Increment the ref count
		(return-from gsl-load-tex tex))))

  (when (not *GSL-VIDEO-DONE*) (return-from gsl-load-tex (gsl-texture-add-new-deferred loc filter-min filter-mag wrap-t wrap-s)))

  ;;Else load the texture and store it in the hash table.
    (let ((tex (make-tex loc)))
      (copy-extern-to-tex tex loc filter-min filter-mag wrap-t wrap-s)
      tex))
;;}}}

(defun gsl-delete-tex (tex);;{{{
  "Deletes a single texture from *GSL-TEXTURE-HASH* and cleans all memory associated with it"
  (when (<= (decf (gsl-tex-nused tex)) 0)	;;If the refcount is > 0 do nothing
    (progn
      (gl-delete-texture (gsl-tex-id tex))
      (remhash (gsl-tex-loc tex) *GSL-TEXTURE-HASH*)
      (setf tex nil))));;}}}

(defun gsl-delete-all-textures ();;{{{
  "Deletes all textures in *GSL-TEXTURE-HASH*"
  (maphash #'(lambda (key val) (when key (gsl-delete-tex val))) *GSL-TEXTURE-HASH*));;}}};;}}}

(defclass rect ();;{{{
  ((x
    :accessor rect-x
    :initarg :x
    :initform 0)
  (y
    :accessor rect-y
    :initarg :y
    :initform 0)
  (z
    :accessor rect-z
    :initarg :z
    :initform 0)
  (w
    :accessor rect-w
    :initarg :w
    :initform 0)
  (h
    :accessor rect-h
    :initarg :h
    :initform 0)
  (x2
    :accessor rect-x2
    :initarg :x2)
  (y2
    :accessor rect-y2
    :initarg :y2)))

;;Methods

(defmethod gsl-make-rect (x y w h)
  "Creates a new rect object"
  (make-instance 'rect :x x :y y :w w :h h :x2 (+ x w) :y2 (+ y h)))

(defmethod gsl-draw-shadow ((rect rect) x y)
  "Draws the shadow of <rect> see gsl_draw_rect_shadow in GSL/clibs/gsl-bindings.c"
  (gsl_draw_rect_shadow x y (rect-x rect) (rect-y rect) (rect-x2 rect) (rect-y2 rect)))

(defmethod gsl-draw ((rect rect) &key (tex nil))
  "Method for drawing a rect based on a rect object"
  (if tex
    (gsl_draw_tex (gsl-tex-id tex) (float (rect-x rect))
		  (float (rect-y rect)) (float (rect-z rect))
		  (float (rect-w rect)) (float (rect-h rect)))

    (gsl_draw_tex 0 (rect-x rect) 
		  (float (rect-y rect)) (float (rect-z rect)) 
		  (float (rect-w rect)) (float (rect-h rect)))))
;;}}}

(defclass framebuffer ();;{{{
  ((id
     :initarg  :id
     :reader gsl-fbo-id
     :initform 0)
   (w
     :initarg  :w
     :reader gsl-fbo-width
     :initform 512)
   (h
     :initarg  :h
     :reader gsl-fbo-height
     :initform 512)
   (color-attachments
     :initform (make-array 10 :element-type 'tex :fill-pointer t :initial-element nil)
     :accessor gsl-fbo-color-attachments)
   (depth-buffer
     :initform 0 
     :accessor gsl-fbo-depth)))

(const +GSL-COLOR-BUFFER+ #x00)
(const +GSL-DEPTH-BUFFER+ #x01)
(defparam *GSL-FBO-LIST* nil)

(defmethod gsl-fbo-new (w h);;{{{
  "Create a new framebuffer object and initialise it"
  (let ((id (gsl_new_framebuffer w h)))
    (let ((fbo (make-instance 'framebuffer :id id :w w :h h)))
      (push fbo *GSL-FBO-LIST*)
      (return-from gsl-fbo-new fbo))))
;;}}}

(defmethod gsl-fbo-add-color ((fbo framebuffer) pos);;{{{
  "Adds a color buffer to a gsl-fbo object"
  (let ((tex-id (gsl_fbo_add_color (gsl-fbo-id fbo) pos)))
    (let ((tex (make-tex (gensym) tex-id (gsl-fbo-width fbo) (gsl-fbo-height fbo))))
      (setf (aref (gsl-fbo-color-attachments fbo) pos) tex))));;}}}

(defmethod gsl-fbo-add-depth ((fbo framebuffer));;{{{
  "Adds a depth buffer to <fbo>"
  (let ((depth (gsl-fbo-depth fbo)))
    (when (> depth 0) (gl-delete-texture depth))
    (gsl_fbo_add_depth (gsl-fbo-id fbo))))
;;}}}

(defmethod gsl-fbo-color-pos ((fbo framebuffer) pos);;{{{
  "Gets the color buffer at position pos"
  (aref (gsl-fbo-color-attachments fbo) pos));;}}}

(defmethod gsl-fbo-del-color ((fbo framebuffer) pos);;{{{
  "Deletes a color buffer from a gsl-fbo object"
  (let ((array (gsl-fbo-color-attachments fbo)))
    (when (aref array pos)
      (gl-delete-texture (gsl-tex-id (aref array pos)))
      (setf (aref array pos) nil))))
;;}}}

(defmethod gsl-fbo-del-depth ((fbo framebuffer));;{{{
  "Deletes a depth buffer from a gsl-fbo object"
  (let ((depth (gsl-fbo-depth fbo)))
    (when (> depth 0) (gl-delete-texture depth)))) ;;}}}

(defmethod gsl-fbo-del ((fbo framebuffer));;{{{
  "Deletes an entire gsl-fbo object"
  (do-array pos (gsl-fbo-color-attachments fbo)
	    (gsl-fbo-del-color fbo pos))
  (gsl-fbo-del-depth fbo)
  (setf fbo nil))
;;}}}

(defmethod gsl-delete-all-fbos ();;{{{
  (dolist (x *GSL-FBO-LIST*)
    (progn
      (pop *GSL-FBO-LIST*)
      (gsl-fbo-del x))));;}}}

(defmethod gsl-fbo-use ((fbo framebuffer));;{{{
  "glBindFramebuffer"
  (gsl_fbo_use (gsl-fbo-id fbo)));;}}}

(defmethod gsl-fbo-use ((fbo number));;{{{
  "For use with (gsl-fbo-use 0) (basically glBindFramebuffer(GL_FRAMEBUFFER_EXT, 0);)"
  (gsl_fbo_use -1));;}}}
;;}}}

(defclass shader ();;{{{
  ((id
     :initform 0
     :initarg :id
     :accessor gsl-shader-id)
   (vars
     :initform (make-hash-table) 
     :accessor gsl-shader-vars)))

(defclass shader-var ()
  ((address
     :initform 0
     :initarg :address
     :accessor gsl-shader-var-address)
   (value
     :initform 0
     :accessor gsl-shader-var-value)
   (program 
     :initform 0
     :initarg :program
     :accessor gsl-shader-var-program)))

(const +GSL-SHADER-VERT+ 0)
(const +GSL-SHADER-FRAG+ 1)

(defmacro create-shader (&key (vert "") (frag "") &allow-other-keys);;{{{
  "Create a new OpenGL shader and return its ID"
  `(with-cstrs ((vert_var ,vert) (frag_var ,frag))
	       (gsl_new_shader vert_var frag_var)));;}}}

(defmacro gsl-shader-new (&rest rest);;{{{
  "Creates and returns a new gsl-shader object"
  `(block nil 
	  (let ((shader-id (create-shader ,@rest)))
	    (return (make-instance 'shader :id shader-id)))))
;;}}}

(defmacro gsl-shader-delete-vars (shader);;{{{
  "Resets all shader vars"
  `(maphash #'(lambda (key val) (when key (setf (gsl-shader-var-value val) 0))) (gsl-shader-vars ,shader)));;}}}

(defmacro gsl-shader-source (shader &key (vert "" vert_supplied) (frag "" frag_supplied));;{{{
  "Change <shader>'s source code"
  `(progn (gsl_shader_source (gsl-shader-id ,shader) ,vert ,frag)
	  (gsl-shader-delete-vars ,shader)));;}}}

(defmacro gsl-shader-use (shader);;{{{
  "Use <shader>"
  `(if ,shader 
     (gsl_use_shader (gsl-shader-id ,shader))
     (gsl_use_shader 0)));;}}}

(defmacro make-shader-var (&rest rest);;{{{
  "Initialises a shader-var Object"
  `(make-instance 'shader-var ,@rest));;}}}

(defmacro gsl-shader-var-get (shader name);;{{{
  "Gets a shader-var from <shader>'s var table"
  `(gethash (intern ,name) (gsl-shader-vars ,shader)));;}}}

(defmacro gsl-shader-var-set (var val);;{{{
  "Sets <var> to <val>"
  `(let ((address (gsl-shader-var-address ,var)) (program (gsl-shader-var-program ,var)))
     (gsl_set_shader_var program address (float ,val))));;}}}

(defmacro gsl-shader-var-new (shader name);;{{{
  "Creates a new shader-var and adds it to <shader>'s var hash"
  `(let ((program (gsl-shader-id ,shader)))
     (let ((address (gsl_new_shader_var program ,name)))
       (let ((shader (make-shader-var :address address :program program)))
	 (setf (gsl-shader-var-get ,shader ,name) shader)))));;}}}

(defmacro gsl-shader-set (shader name val);;{{{
  "Sets var <name> in <shader> to <val>"
  `(let ((var (gsl-shader-var-get ,shader ,name)))
     (when (not var) (setf var (gsl-shader-var-new ,shader ,name)))
     (gsl-shader-var-set var ,val)));;}}}

(defmacro gsl-shader-get (shader name);;{{{
  "Gets var <name> from <shader>"
  `(let ((var (gsl-shader-var-get ,shader ,name)))
     (when var (gsl-shader-var-value var))));;}}};;}}}
