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

(defun add-tex-to-hash (tex loc);;{{{
  (setf (gethash (read-from-string loc) *GSL-TEXTURE-HASH*) tex));;}}}

(defun make-tex (loc id width height);;{{{
  (make-instance 'tex :loc loc :id id :width width :height height));;}}}

(defun gsl-load-tex (loc);;{{{
  "Loads and returns a texture, only returns the texture if it already exists"
  ;;If the texture location is already in the hash table, return the tex.

  (let ((tex (gethash (read-from-string loc) *GSL-TEXTURE-HASH*)))
    (when tex (progn
		(incf (gsl-tex-nused tex))	;;Increment the ref count
		(return-from gsl-load-tex tex))))

  ;;Else load the texture and store it in the hash table.
  (let ((extern (gsl_load_texture loc)))	;;Using lisp C bindings to load the texture
    (let ((tex (make-tex loc (aref extern 0) (aref extern 1) (aref extern 2))))
      (add-tex-to-hash tex loc)
      (return-from gsl-load-tex tex))));;}}}

(defun gsl-delete-tex (tex);;{{{
  (when (<= (decf (gsl-tex-nused tex)) 0)	;;If the refcount is > 0 do nothing
    (progn
      (gl-delete-texture (gsl-tex-id tex))
      (remhash (gsl-tex-loc tex) *GSL-TEXTURE-HASH*)
      (setf tex nil))));;}}}

(defun gsl-delete-all-textures ();;{{{
  (maphash #'(lambda (key val) (gsl-delete-tex val)) *GSL-TEXTURE-HASH*));;}}}
;;}}}

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
  (make-instance 'rect :x x :y y :w w :h h :x2 (+ x w) :y2 (+ y h)))

(defmethod gsl-draw-shadow ((rect rect) x y)
  (gsl_draw_rect_shadow x y (rect-x rect) (rect-y rect) (rect-x2 rect) (rect-y2 rect)))

(defmethod gsl-draw ((rect rect) &key (tex nil))
  (if tex
    (gsl_draw_tex (gsl-tex-id tex) (rect-x rect) (rect-y rect) (rect-z rect) (rect-w rect) (rect-h rect))
    (gsl_draw_tex 0 (rect-x rect) (rect-y rect) (rect-z rect) (rect-w rect) (rect-h rect))))
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
   (depth-attachments
     :initform (make-array 10 :element-type 'tex :fill-pointer t :initial-element nil)
     :accessor gsl-fbo-depth-attachments)))

(const +GSL-COLOR-BUFFER+ #x00)
(const +GSL-DEPTH-BUFFER+ #x01)
(defparam *GSL-FBO-LIST* nil)

(defmethod gsl-fbo-new (w h);;{{{
  (let ((id (gsl_new_framebuffer w h)))
    (let ((fbo (make-instance 'framebuffer :id id :w w :h h)))
      (push fbo *GSL-FBO-LIST*)
      (return-from gsl-fbo-new fbo))))
;;}}}

(defmethod gsl-fbo-add-color ((fbo framebuffer) pos);;{{{
  "Adds a color buffer to a gsl-fbo object"
  (let ((tex-id (gsl_fbo_add (gsl-fbo-id fbo) +GSL-COLOR-BUFFER+ pos)))
    (let ((tex (make-tex (gensym) tex-id (gsl-fbo-width fbo) (gsl-fbo-height fbo))))
      (setf (aref (gsl-fbo-color-attachments fbo) pos) tex))));;}}}

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

(defmethod gsl-fbo-del-depth ((fbo framebuffer) pos);;{{{
  "Deletes a depth buffer from a gsl-fbo object"
  (let ((array (gsl-fbo-depth-attachments fbo)))
    (when (equalp (aref array pos) nil) (return-from gsl-fbo-del-depth))
    (gsl-delete-texture (gsl-tex-id (aref array pos)))
    (setf (aref array pos) 0)));;}}}

(defmethod gsl-fbo-del ((fbo framebuffer));;{{{
  "Deletes an entire gsl-fbo object"
  (do-array pos (gsl-fbo-color-attachments fbo)
	    (gsl-fbo-del-color fbo pos))
  (do-array pos (gsl-fbo-depth-attachments fbo)
	    (gsl-fbo-del-depth fbo pos))
  (setf fbo nil))
;;}}}

(defmethod gsl-delete-all-fbos ();;{{{
  (dolist (x *GSL-FBO-LIST*)
    (progn
      (pop *GSL-FBO-LIST*)
      (gsl-fbo-del x))))

(defmethod gsl-fbo-use ((fbo framebuffer));;{{{
  "glBindFramebuffer"
  (gsl_fbo_use (gsl-fbo-id fbo)));;}}}

(defmethod gsl-fbo-use ((fbo number));;{{{
  "For use with (gsl-fbo-use 0) (basically glBindFramebuffer(GL_FRAMEBUFFER_EXT, 0);)"
  (gsl_fbo_use -1));;}}}
;;}}}
