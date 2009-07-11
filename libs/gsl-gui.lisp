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

(in-package :gsl-gui)

;;	GSL-GUI-VARIABLES;;{{{
(defparam *GSL-GUI-CORNER-COLOR* '(1 1 1))
(defparam *GSL-GUI-BORDER-COLOR* '(1 1 1))
(defparam *GSL-GUI-BORDER-SIZE* 1)
(defparam *GSL-GUI-CORNER-SIZE* 1)
(defparam *GSL-GUI-BACKGROUND-COLOR* '(0.2 0.2 0.2 0.9))
(defparam *GSL-GUI-BORDER-TEX*	  nil) 
(defparam *GSL-GUI-CORNER-TEX*	  nil)
(defparam *GSL-GUI-TOP-FOCUS*     0)
(defparam *GSL-GUI-NEXT-ID*	  0)
(defparam *GSL-GUI-LAST-ACTIVE*	  nil)
(defparam *GSL-GUI-LAST-ACTION*	  nil)
(defparam *GSL-GUI-OFFSET*	  nil)
(defparam *GSL-GUI-MASTER*	  nil)
;;}}}

(const	+GSL-GUI-DRAGGING+	1)
(const  +GSL-GUI-RESIZING+	2)

;;	GUI CREATION;;{{{

(defclass gui ();;{{{
  ((x
     :initform 0
     :initarg :x
     :accessor gsl-gui-x)
   (y
     :initform 0
     :initarg :y
     :accessor gsl-gui-y)
   (width
     :initform 50
     :initarg :width
     :accessor gsl-gui-width)
   (height
     :initform 50
     :initarg :height
     :accessor gsl-gui-height)
   (parent
     :initform nil
     :initarg :parent
     :accessor gsl-gui-parent)
   (focus-level
     :initform (incf *GSL-GUI-TOP-FOCUS*)
     :initarg :focus-level
     :accessor gsl-gui-focus-level)
   (id
     :initform (incf *GSL-GUI-NEXT-ID*)
     :accessor gsl-gui-id)
   (gui-list
     :initform nil
     :initarg :gui-list
     :accessor gsl-gui-list)))
;;}}}

(export-all gsl-gui-listgsl-gui-x;;{{{
	    gsl-gui-y
	    gsl-gui-width
	    gsl-gui-height);;}}}

(defmacro make-gui (x y w h);;{{{
  "Creates and returns a new gui object"
  `(make-instance 'gui :x ,x :y ,y :width ,w :height ,h));;}}}

(defun gsl-gui-new (x y w h &key parent &allow-other-keys);;{{{
  "Creates a new gui adds it to the gui list of parent and returns it"
  (let ((gui (make-gui x y w h)))
    (cond
      (parent (push gui (gsl-gui-list parent)))	;;If a parent was passed push this gui onto the parents gui list
      (*GSL-GUI-MASTER*	(push gui (gsl-gui-list *GSL-GUI-MASTER*))) ;;Else if there is a gui master, push to its list
      (t (setf *GSL-GUI-MASTER* gui)))	;;Else assign this as the gui master
    (return-from gsl-gui-new gui)))
;;}}};;}}}

;;	Drawing;;{{{

(defun gsl-gui-draw-corner (rotation x y);;{{{
  "Draw a single gui corner at <xy> rotated to <rotation>"
  (gsl-with-translate (:x x :y y)
      (gsl-with-rotate (:z rotation)
	 (gsl-draw-rect :tex *GSL-GUI-CORNER-TEX*
			:x (/ (mirror *GSL-GUI-CORNER-SIZE*) 1.5)
			:y (/ (mirror *GSL-GUI-CORNER-SIZE*) 1.5)
			:w (* *GSL-GUI-CORNER-SIZE* 2) 
			:h (* *GSL-GUI-CORNER-SIZE* 2)))))
;;}}}

(defun gsl-gui-draw-border (x y x2 y2);;{{{
  "Draws a single border with gui textures from <xy> to <x2y2>"
  (let ((angle (gsl-to-degrees (gsl-get-angle x y x2 y2))) (width (gsl-get-dist x y x2 y2)))
     (gsl-with-translate (:x x :y y)
	(gsl-with-rotate (:z (- 90 angle))
	   (gsl-draw-rect :y (mirror (/ *GSL-GUI-BORDER-SIZE* 2.0)) 
			  :h *GSL-GUI-BORDER-SIZE* 
			  :w width 
			  :tex *GSL-GUI-BORDER-TEX* 
			  :repeat-sizex *GSL-GUI-BORDER-SIZE*)))))
;;}}}

(defun gsl-gui-draw-borders (x y w h);;{{{
  "Draw the border outlines of the rect <xywh>"
  (gsl-with-color (:list *GSL-GUI-BORDER-COLOR*)
      (let ((x2 (+ x w)) (y2 (+ y h)))
	(gsl-gui-draw-border x y x2 y)	;Bottom
	(gsl-gui-draw-border x y2 x2 y2) 	;Top
	(gsl-gui-draw-border x y x y2)	;Left
	(gsl-gui-draw-border x2 y x2 y2))))	;Right
;;}}}

(defun gsl-gui-draw-background (x y w h);;{{{
  "Draws the background for a gui"
  (gsl-with-color (:list *GSL-GUI-BACKGROUND-COLOR*)
      (gsl-draw-rect :x x :y y :w w :h h)));;}}}

(defun gsl-gui-draw-corners (x y w h);;{{{
  "Draw the corners of gui rect <x,y,w,h>"
  (gsl-with-color (:list *GSL-GUI-CORNER-COLOR*)
      (let ((x2 (+ x w)) (y2 (+ y h)))
	(gsl-gui-draw-corner 0   x y)		;Bottom-left
	(gsl-gui-draw-corner 90  x2 y)		;Bottom-right
	(gsl-gui-draw-corner 180 x2 y2)		;Top-right
	(gsl-gui-draw-corner 270 x y2))))	;Top-left
;;}}}

(defun gsl-gui-draw-box (x y w h);;{{{
  "Draws a gui border and background"
  (gsl-with-textures
     (gsl-with-blendfunc (+GL-SRC-ALPHA+ +GL-ONE-MINUS-SRC-ALPHA+)
	(gsl-gui-draw-background x y w h)
	(gsl-gui-draw-borders x y w h)
	(gsl-gui-draw-corners x y w h))));;}}}

(defun gsl-gui-draw (this-gui);;{{{
  "Draws <gui>"
  (gsl-gui-draw-box (gsl-gui-x this-gui) (gsl-gui-y this-gui) (gsl-gui-width this-gui) (gsl-gui-height this-gui))
  (dolist (gui (gsl-gui-list this-gui))
    (gsl-gui-draw gui)))
;;}}}

(defun gsl-gui-draw-all (&optional (parent *GSL-GUI-MASTER*));;{{{
  "Draws all guis from parent"
  (let ((list (gsl-gui-list parent)))
    (dolist (gui (reverse list)) 
      (gsl-gui-draw gui))))
;;}}};;}}}

;;	Parameters;;{{{

(defmacro gsl-gui-set-height (gui newheight);;{{{
  "Sets <gui's> height to <newheight>"
  `(setf (gsl-gui-height ,gui) ,newheight));;}}}

(defmacro gsl-gui-set-width (gui newwidth);;{{{
  "Sets <gui>'s width to <newwidth>"
  `(setf (gsl-gui-width ,gui) ,newwidth));;}}}

(defmacro gsl-gui-set-tex (type loc);;{{{
  "Sets <type> to (gsl-load-tex <loc>)"
  `(if (string ,loc)
     (setf ,type (gsl-load-tex ,loc))
     (setf ,type ,loc)));;}}}

(defmacro gsl-gui-set (&key (border-size nil) (corner-size nil) (border-tex nil) (corner-tex nil));;{{{
  "Sets gui parameters"
  `(progn
     (when ,border-size (setf *GSL-GUI-BORDER-SIZE* ,border-size))
     (when ,corner-size (setf *GSL-GUI-CORNER-SIZE* ,corner-size))
     (when ,border-tex (gsl-gui-set-tex *GSL-GUI-BORDER-TEX* ,border-tex))
     (when ,corner-tex (gsl-gui-set-tex *GSL-GUI-CORNER-TEX* ,corner-tex))));;}}};;}}}

(defmacro gsl-gui-focus (gui &key set);;{{{
  "Returns or sets <gui>'s focus level, sets only if <focus> was passed"
  (if set 
    `(setf (gsl-gui-focus-level ,gui) ,set)
    `(gsl-gui-focus-level ,gui)));;}}}

(defun gsl-gui-move (gui x y)
  (incf (gsl-gui-x gui) x)
  (incf (gsl-gui-y gui) y))

;;	Input;;{{{

(defmacro pos-in-gui (gui x y);;{{{
  "Returns t if <x,y> is inside <gui>"
  `(gsl-x-in-rect ,x ,y (gsl-gui-x ,gui) (gsl-gui-y ,gui) (gsl-gui-width ,gui) (gsl-gui-height ,gui)));;}}}

(defun gsl-mouse-is-on-gui (x y &optional (parent *GSL-GUI-MASTER*));;{{{
  "Returns a list of all gui objects that collide with position <x,y>"
  (let ((list nil))
    (dolist (gui (gsl-gui-list parent))
      (when (pos-in-gui gui x y) (push gui list)))
    list))
;;}}}

(defun highest-focus-gui (gui-list &optional (highest (car gui-list)));;{{{
  "Returns the gui with the highest focus from <gui-list>"
  (when (not gui-list) (return-from highest-focus-gui highest))
  (let ((first (car gui-list)))
    (when (> (gsl-gui-focus first) (gsl-gui-focus highest))
      (setf highest first)))
  (return-from highest-focus-gui (highest-focus-gui (cdr gui-list) highest)));;}}}

(defun delete-gui (gui);;{{{
  "Removes <gui> from "
  (let ((parent (gsl-gui-parent gui)))
    (let ((id (gsl-gui-id gui)))
      (setf (gsl-gui-list parent) (delete-if #'(lambda (temp-gui) (equal (gsl-gui-id temp-gui) id)) (gsl-gui-list parent))))))
;;}}}

(defun raise-gui (gui);;{{{
  "Raises <gui> to be the top-level gui"
  (when gui
    (when (< (gsl-gui-focus gui) *GSL-GUI-TOP-FOCUS*)
      (gsl-gui-focus gui :set (incf *GSL-GUI-TOP-FOCUS*))
      (delete-gui gui)
      (push gui (gsl-gui-list *GSL-GUI-MASTER*))
      (setf *GSL-GUI-LAST-ACTIVE* gui))
    gui));;}}}

(defun gsl-gui-drag (motionx motiony);;{{{
  "Drag the active gui by <motionx, motiony>"
  (move *GSL-GUI-LAST-ACTIVE* motionx motiony));;}}}

(defun gsl-gui-begin-drag (gui);;{{{
  "Start dragging <gui>"
  (setf *GSL-GUI-LAST-ACTIVE* gui)
  (setf *GSL-GUI-LAST-ACTION* +GSL-GUI-DRAGGING+));;}}}

(defun gsl-gui-action (gui x y);;{{{
  "This function checks all the gui objects in <gui> to locate the one that is active"
  (and x y)	;;Ignore X and Y for now
  (gsl-gui-begin-drag gui));;}}}

(defun gsl-gui-clear-last-action ();;{{{
  "Clear the current action"
  (setf *GSL-GUI-LAST-ACTION*	nil));;}}}

(defun gsl-gui-mouse-motion (motionx motiony);;{{{
  "Called when the mouse is moved"
  (when *GSL-GUI-LAST-ACTIVE*
    (cond
      ((equalp *GSL-GUI-LAST-ACTION* +GSL-GUI-DRAGGING+) (gsl-gui-move *GSL-GUI-LAST-ACTIVE* motionx motiony)))))
(setf *GSL-GUI-MOVE-FUNC*		 #'gsl-gui-mouse-motion);;}}}

(defun gsl-gui-mouse-event (button type);;{{{
  "Called when a mouse event occurs"
  (when button
    (when (equalp type +SDL-MOUSEBUTTONUP+)
      (gsl-gui-clear-last-action))))
(setf *GSL-GUI-MOUSE-EVENT-FUNC*	#'gsl-gui-mouse-event);;}}}

(defun gsl-gui-mouse-input (x y);;{{{
  (let ((current-gui (highest-focus-gui (gsl-mouse-is-on-gui x y))))
    (raise-gui current-gui)
    (gsl-gui-action current-gui x y)))
;;}}};;}}}
