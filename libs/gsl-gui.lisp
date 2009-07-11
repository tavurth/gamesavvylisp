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
(defparam *GSL-GUI-CURSOR-X*	  0)
(defparam *GSL-GUI-CURSOR-Y*	  0)
;;}}}

;;	GSL-GUI-CONSTANTS;;{{{
(const	+GSL-GUI-DRAGGING+	1)
(const  +GSL-GUI-RESIZING+	2)
;;}}}

;;	Creation ;;{{{

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
    (gl-load-identity)
    (gl-translate :z -1024)
    (dolist (gui (reverse list))
      (gsl-gui-draw gui))
    ;;Drawing the cursor
    (gsl-draw-rect :x *GSL-GUI-CURSOR-X* :y *GSL-GUI-CURSOR-Y* :w 5 :h 5)))

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
     (when ,corner-tex (gsl-gui-set-tex *GSL-GUI-CORNER-TEX* ,corner-tex))));;}}}

(defun gsl-gui-set-cursor (&key x y);;{{{
  "Set the gui cursor to a specific location"
  (when x (setf *GSL-GUI-CURSOR-X* x))
  (when y (setf *GSL-GUI-CURSOR-Y* y)));;}}};;}}}

;;	Input;;{{{

(defun gui-is-under-pos (gui x y);;{{{
  "Find out if <gui> is under position <x,y>"
  (gsl-x-in-rect x y (gsl-gui-x gui) (gsl-gui-y gui) (gsl-gui-width gui) (gsl-gui-height gui)));;}}}

(defun get-guis-under-cursor (&optional (parent *GSL-GUI-MASTER*));;{{{
  "Return a list of all guis under the *GSL-GUI-CURSOR-X/Y* position"
  (let ((list-to-return nil))
    (dolist (gui (gsl-gui-list parent))
      (when (gui-is-under-pos gui *GSL-GUI-CURSOR-X* *GSL-GUI-CURSOR-Y*) (push gui list-to-return)))
    list-to-return));;}}}

(defun get-highest-focus (gui-list &optional highest);;{{{
  "Return the gui with the highest focus level from <gui-list>"
  (when (not gui-list) (return-from get-highest-focus highest))
  (let ((first (car gui-list)))
    (when (not highest) (setf highest first))
    (when (>= (gsl-gui-focus first) (gsl-gui-focus highest)) (setf highest first))
    (get-highest-focus (cdr gui-list) highest)));;}}};;}}}

;;	Actions;;{{{

(defun gsl-gui-move (gui &key (x 0) (y 0));;{{{
  "Move a gui by <x,y>"
  (incf (gsl-gui-x gui) x)
  (incf (gsl-gui-y gui) y));;}}}

(defun gsl-gui-resize (gui &key (x 0) (y 0));;{{{
  "Resize <gui> by <x,y>"
  (let ((gsl-gui-min 50))
    (when (< (incf (gsl-gui-width gui)  x) gsl-gui-min) (setf (gsl-gui-width gui)  gsl-gui-min))
    (when (< (incf (gsl-gui-height gui) y) gsl-gui-min) (setf (gsl-gui-height gui) gsl-gui-min))))
;;}}}

(defun gsl-gui-move-cursor (&key (x 0) (y 0));;{{{
  "Move the gui cursor by <x,y>"
  (incf *GSL-GUI-CURSOR-X* x)
  (incf *GSL-GUI-CURSOR-Y* y));;}}}

(defun begin-action (gui action-type);;{{{
  "Call this function to begin an action on <gui>"
  (when (not gui) (return-from begin-action))
  (setf *GSL-GUI-LAST-ACTIVE* gui)
  (setf *GSL-GUI-LAST-ACTION* action-type));;}}}

(defun begin-dragging (gui);;{{{
  "Call this function to begin dragging <gui>"
  (begin-action gui +GSL-GUI-DRAGGING+))
;;}}}

(defun begin-resizing (gui);;{{{
  "Call this function to begin resizing <gui>"
  (begin-action gui +GSL-GUI-RESIZING+));;}}}

(defun end-actions ();;{{{
  "Ends all gui actions"
  (setf *GSL-GUI-LAST-ACTIVE* nil)
  (setf *GSL-GUI-LAST-ACTION* nil));;}}}

(defun move-action (motionx motiony);;{{{
  "Called when a mouse motion event occurs and a gui action is also occuring"
  (cond
    ((equalp *GSL-GUI-LAST-ACTION* +GSL-GUI-DRAGGING+) (gsl-gui-move *GSL-GUI-LAST-ACTIVE* :x motionx :y motiony))
    ((equalp *GSL-GUI-LAST-ACTION* +GSL-GUI-RESIZING+) (gsl-gui-resize *GSL-GUI-LAST-ACTIVE* :x motionx :y motiony))));;}}}

(defun delete-gui (gui);;{{{
  "Delete <gui> from its parent's gui-list"
  (let ((parent-gui (gsl-gui-parent gui)))
    (when (not parent-gui) (setf parent-gui *GSL-GUI-MASTER*))
    (setf (gsl-gui-list parent-gui)
	  (delete-if #'(lambda (temp-gui) (equalp (gsl-gui-id temp-gui) (gsl-gui-id gui))) (gsl-gui-list parent-gui)))));;}}}

(defun add-gui-to-parent (gui parent);;{{{
  "Add <gui> to <parent>'s gui list"
  (push gui (gsl-gui-list parent)));;}}}

(defun raise-gui (gui);;{{{
  "Raise <gui> to be the top level gui in its parent"
  (let ((parent-gui (gsl-gui-parent gui)))
    (when (not parent-gui) (setf parent-gui *GSL-GUI-MASTER*))
    (delete-gui gui)
    (add-gui-to-parent gui parent-gui)));;}}};;}}}

;;	Events;;{{{

;;	Left Mouse Event ;;{{{
(defun left-mouse-down ()
  (let ((current-gui (first (reverse (get-guis-under-cursor)))))
    (when (not current-gui) (return-from left-mouse-down))
    (raise-gui current-gui)
    ;(begin-resizing current-gui)))
    (begin-dragging current-gui)))

(defun left-mouse-up ()
  (end-actions))
;;}}}

;;	Mouse Event;;{{{

(defun mouse-down (button);;{{{
  "Called when a mouse button is pressed"
  (cond
    ((equalp button 1) (left-mouse-down))));;}}}

(defun mouse-up (button);;{{{
  "Called when a mouse button is released"
  (cond
    ((equalp button 1) (left-mouse-up))))

;;}}};;}}}

;;	Event types;;{{{

(defun gsl-gui-mouse-event (button type);;{{{
  "Called when a mouse event occurs"
  (cond
    ((equalp type +SDL-MOUSEBUTTONDOWN+) (mouse-down button))
    ((equalp type +SDL-MOUSEBUTTONUP+)   (mouse-up   button))))
(setf *GSL-GUI-MOUSE-EVENT-FUNC* #'gsl-gui-mouse-event)
;;}}}

(defun gsl-gui-mouse-motion (motionx motiony);;{{{
  "Called when a mouse motion event occurs"
  (gsl-gui-move-cursor :x motionx :y motiony)
  (when *GSL-GUi-LAST-ACTIVE* (move-action motionx motiony)))
(setf *GSL-GUI-MOVE-FUNC* #'gsl-gui-mouse-motion);;}}};;}}};;}}}

(defparam *GSL-GUI-MASTER* (gsl-gui-new -1000 -1000 1024 512))
