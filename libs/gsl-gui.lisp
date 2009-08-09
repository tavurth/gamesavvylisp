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
(defparam *GSL-GUI-BG-COLOR* '(0.2 0.2 0.2 0.9))
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
(const	+GSL-GUI-MOVING+	1)
(const  +GSL-GUI-RESIZING+	2)
;;}}}

;;	Creation ;;{{{

(const +GSL-GUI-ALLOW-RESIZE+ 1)
(const +GSL-GUI-ALLOW-MOVE+   2)
(const +GSL-GUI-DRAW-BORDER+  4)

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
   (background
     :initform nil
     :initarg :background
     :accessor gsl-gui-background)
   (bg-color
     :initform nil
     :initarg :background-color
     :accessor gsl-gui-bg-color)
   (parent
     :initform nil
     :initarg :parent
     :accessor gsl-gui-parent)
   (flags
     :initform 0
     :initarg :flags
     :accessor gsl-gui-flags)
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

(defmacro check-set-gui-flag (name val);;{{{
  "Create a function to check and set the value of (gui-flags) with <val> as reference"
  `(defun ,name (gui &optional (new-val 0 new-val-passed))
    (let ((gui-flags (gsl-gui-flags gui)))
      (if new-val-passed
	(if new-val
	  (setf (gsl-gui-flags gui) (logior gui-flags ,val))
	  (setf (gsl-gui-flags gui) (logxor gui-flags ,val)))
	(when (> (logand (gsl-gui-flags gui) ,val) 0) t)))));;}}}

(check-set-gui-flag allow-gui-resize    +GSL-GUI-ALLOW-RESIZE+)
(check-set-gui-flag allow-gui-move      +GSL-GUI-ALLOW-MOVE+)
(check-set-gui-flag should-draw-borders +GSL-GUI-DRAW-BORDER+)

(defmacro add-gui (this gui)
  `(push ,gui (gsl-gui-list ,this)))

(defun gsl-gui-set-bg-color (gui &key r g b a)
  (setf (gsl-gui-bg-color gui) `(:r ,r :g ,g :b ,b :a ,a)))

(defmacro gsl-gui-new (x y w h &rest rest);;{{{
  "Creates a new gui adds it to the gui list of parent and returns it"
  (let ((parent (getf rest :parent)) (background (getf rest :background)))
    `(let ((gui (make-instance 'gui :x ,x :y ,y :width ,w :height ,h ,@rest)))
	   (cond
	     (,parent (add-gui ,parent gui)) ;;If a parent was passed push this gui onto the parents gui list
	     (*GSL-GUI-MASTER* (progn
				 (add-gui *GSL-GUI-MASTER* gui)
				 (should-draw-borders gui t))) ;;Else if there is a gui master, push to its list
	     (t (setf *GSL-GUI-MASTER* gui)))	;;Else assign this as the gui master
	   (if ,background
	     (gsl-gui-set-bg-color gui :r 1 :g 1 :b 1)
	     (gsl-gui-set-bg-color gui))
	   gui)))
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

(defun gsl-gui-draw-borders (gui);;{{{
  "Draw the border outlines of the rect <xywh>"
  (gsl-with-color (:list *GSL-GUI-BORDER-COLOR*)
      (let ((x (gsl-gui-x gui)) (y (gsl-gui-y gui)) (w (gsl-gui-width gui)) (h (gsl-gui-height gui)))
	(let ((x2 (+ x w)) (y2 (+ y h)))
	  (gsl-gui-draw-border x y x2 y)	;Bottom
	  (gsl-gui-draw-border x y2 x2 y2) 	;Top
	  (gsl-gui-draw-border x y x y2)	;Left
	  (gsl-gui-draw-border x2 y x2 y2)))))	;Right
;;}}}

(defun make-gui-color (gui);;{{{
  "Create the color of <gui> to be used by combining <(gsl-gui-bg-color gui)> and <*GSL-GUI-BG-COLOR*>"
  (let ((list nil) (gui-color (gsl-gui-bg-color gui)) (temp-var nil))
    (if (setf temp-var (getf gui-color :a)) (push temp-var list) (push (fourth *GSL-GUI-BG-COLOR*) list))
    (if (setf temp-var (getf gui-color :b)) (push temp-var list) (push (third  *GSL-GUI-BG-COLOR*) list))
    (if (setf temp-var (getf gui-color :g)) (push temp-var list) (push (second *GSL-GUI-BG-COLOR*) list))
    (if (setf temp-var (getf gui-color :r)) (push temp-var list) (push (first  *GSL-GUI-BG-COLOR*) list))
    list));;}}}

(defun gsl-gui-draw-background (gui);;{{{
  "Draws the background for a gui"
  (gsl-with-color (:list (make-gui-color gui))
      (gsl-draw-rect :tex (gsl-gui-background gui) :x (gsl-gui-x gui) :y (gsl-gui-y gui) :w (gsl-gui-width gui) :h (gsl-gui-height gui))));;}}}

(defun gsl-gui-draw-corners (gui);;{{{
  "Draw the corners of gui rect <x,y,w,h>"
  (gsl-with-color (:list *GSL-GUI-CORNER-COLOR*)
      (let ((x (gsl-gui-x gui)) (y (gsl-gui-y gui)) (w (gsl-gui-width gui)) (h (gsl-gui-height gui)))
	(let ((x2 (+ x w)) (y2 (+ y h)))
	  (gsl-gui-draw-corner 0   x y)		;Bottom-left
	  (gsl-gui-draw-corner 90  x2 y)	;Bottom-right
	  (gsl-gui-draw-corner 180 x2 y2)	;Top-right
	  (gsl-gui-draw-corner 270 x y2)))))	;Top-left
;;}}}

(defun gsl-gui-draw-box (gui depth);;{{{
  "Draws a gui border and background"
  
  (gsl-with-stencilop (+GL-KEEP+ +GL-KEEP+ +GL-INCR+)
    (gsl-with-stencilfunc (+GL-EQUAL+ depth depth)
      (gsl-with-blendfunc (+GL-SRC-ALPHA+ +GL-ONE-MINUS-SRC-ALPHA+)
        (gsl-gui-draw-background gui))))

  (when (should-draw-borders gui)
    (gsl-gui-draw-borders    gui)
    (gsl-gui-draw-corners    gui)));;}}}

(defun gsl-gui-draw (this &optional (depth 0));;{{{
  "Draws <gui>"
  (gsl-with-textures
    (gsl-gui-draw-box this depth))

  (gsl-with-translate (:x (gsl-gui-x this) :y (gsl-gui-y this))
    (gsl-with-stencilfunc (+GL-NOTEQUAL+ (incf depth) depth)
      (dolist (gui (gsl-gui-list this))
	(gsl-gui-draw gui depth)))))
;;}}}

(defun gsl-gui-draw-all (&optional (parent *GSL-GUI-MASTER*));;{{{
  "Draws all guis from parent"
  (gsl-with-stenciltest
    (gl-clear +GL-STENCIL-BUFFER-BIT+)
    (let ((list (gsl-gui-list parent)))
      (when (not list) (return-from gsl-gui-draw-all))

      (gl-load-identity)
      (gl-translate :z -1024)
      (dolist (gui (reverse list))
	(gsl-gui-draw gui))))

  ;;Drawing the cursor
  (gsl-draw-rect :x *GSL-GUI-CURSOR-X* :y *GSL-GUI-CURSOR-Y* :w 5 :h 5))

;;}}};;}}}

;;	Parameters;;{{{

(defmacro gsl-gui-set-height (gui newheight);;{{{
  "Sets <gui's> height to <newheight>"
  `(setf (gsl-gui-height ,gui) ,newheight));;}}}

(defmacro gsl-gui-set-width (gui newwidth);;{{{
  "Sets <gui>'s width to <newwidth>"
  `(setf (gsl-gui-width ,gui) ,newwidth));;}}}

(defmacro gsl-gui-set-tex (type loc)
  "Sets <type> to (gsl-load-tex <loc>)"
  `(setf ,type (gsl-load-tex ,loc)))

(defmacro gsl-gui-set (&key (border-size nil) (corner-size nil) (border-tex nil) (corner-tex nil) (bg-color nil col-pass));;{{{
  "Sets gui parameters"
  `(progn
     (when ,border-size (setf *GSL-GUI-BORDER-SIZE* ,border-size))
     (when ,corner-size (setf *GSL-GUI-CORNER-SIZE* ,corner-size))
     (when ,border-tex  (gsl-gui-set-tex *GSL-GUI-BORDER-TEX* ,border-tex))
     (when ,corner-tex  (gsl-gui-set-tex *GSL-GUI-CORNER-TEX* ,corner-tex))
     (when ,col-pass    (setf *GSL-GUI-BG-COLOR* (list ,@bg-color)))));;}}}

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

(defun begin-moving (gui);;{{{
  "Call this function to begin moving <gui>"
  (when (allow-gui-move gui)
    (begin-action gui +GSL-GUI-MOVING+)))
;;}}}

(defun begin-resizing (gui);;{{{
  "Call this function to begin resizing <gui>"
  (when (allow-gui-resize gui)
    (begin-action gui +GSL-GUI-RESIZING+)));;}}}

(defun end-actions ();;{{{
  "Ends all gui actions"
  (setf *GSL-GUI-LAST-ACTIVE* nil)
  (setf *GSL-GUI-LAST-ACTION* nil));;}}}

(defun move-action (motionx motiony);;{{{
  "Called when a mouse motion event occurs and a gui action is also occuring"
  (cond
    ((equalp *GSL-GUI-LAST-ACTION* +GSL-GUI-MOVING+) (gsl-gui-move *GSL-GUI-LAST-ACTIVE* :x motionx :y motiony))
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
    (add-gui-to-parent gui parent-gui)));;}}}

(defun get-current-gui ();;{{{
  (first (reverse (get-guis-under-cursor))));;}}};;}}}

;;	Events;;{{{

;;	Left Mouse Event ;;{{{
(defun left-mouse-down ()
  (let ((current-gui (get-current-gui)))
    (when (not current-gui) (return-from left-mouse-down))
    (raise-gui current-gui)
    (begin-moving current-gui)))

(defun left-mouse-up ()
  (end-actions));;}}}

(defun right-mouse-down ();;{{{
  (let ((current-gui (get-current-gui)))
    (when (not current-gui) (return-from right-mouse-down))
    (raise-gui current-gui)
    (begin-resizing current-gui)))

(defun right-mouse-up ()
  (end-actions));;}}}

;;	Mouse Event;;{{{

(defun mouse-down (button);;{{{
  "Called when a mouse button is pressed"
  (cond
    ((equalp button 1) (left-mouse-down))
    ((equalp button 3) (right-mouse-down))));;}}}

(defun mouse-up (button);;{{{
  "Called when a mouse button is released"
  (cond
    ((equalp button 1) (left-mouse-up))
    ((equalp button 3) (right-mouse-up))))

;;}}};;}}}

;;	Event types;;{{{

(defun gsl-gui-mouse-event (button type);;{{{
  "Called when a mouse event occurs"
  (cond
    ((equalp type +SDL-MOUSEBUTTONDOWN+) (mouse-down button))
    ((equalp type +SDL-MOUSEBUTTONUP+)   (mouse-up   button))))
(gsl-add-mouse-event-func #'gsl-gui-mouse-event)
;;}}}

(defun gsl-gui-mouse-motion (motionx motiony);;{{{
  "Called when a mouse motion event occurs"
  (gsl-gui-move-cursor :x motionx :y motiony)
  (when *GSL-GUI-LAST-ACTIVE* (move-action motionx motiony)));;}}}
(gsl-add-mouse-motion-func #'gsl-gui-mouse-motion);;}}};;}}};;}}}

(defparam *GSL-GUI-MASTER* (gsl-gui-new -1000 -1000 1024 512))
