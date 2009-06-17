(in-package :gsl-gui)

(defparam *GSL-GUI-CORNER-COLOR* '(1 1 1))
(defparam *GSL-GUI-BORDER-COLOR* '(1 1 1))
(defparam *GSL-GUI-BORDER-SIZE* 1)
(defparam *GSL-GUI-CORNER-SIZE* 1)
(defparam *GSL-GUI-BACKGROUND-COLOR* '(0.2 0.2 0.2 0.5))
(defparam *GSL-GUI-BORDER-TEX*	  nil) 
(defparam *GSL-GUI-CORNER-TEX*	  nil)
(defparam *GSL-GUI-LIST*	  nil)

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
     :accessor gsl-gui-height)));;}}}

(export-all gsl-gui-x;;{{{
	    gsl-gui-y
	    gsl-gui-width
	    gsl-gui-height);;}}}

(defmacro make-gui (x y w h);;{{{
  "Creates and returns a new gui object"
  `(make-instance 'gui :x ,x :y ,y :width ,w :height ,h));;}}}

(defun gsl-gui-new (x y w h);;{{{
  "Creates a new gui adds it to *GSL-GUI-LIST* and returns it"
  (let ((gui (make-gui x y w h)))
    (push gui *GSL-GUI-LIST*)
    (return-from gsl-gui-new gui)))
;;}}}

(defmacro gsl-gui-set-height (gui newheight);;{{{
  "Sets <gui's> height to <newheight>"
  `(setf (gsl-gui-height ,gui) ,newheight));;}}}

(defmacro gsl-gui-set-width (gui newwidth);;{{{
  "Sets <gui>'s width to <newwidth>"
  `(setf (gsl-gui-width ,gui) ,newwidth));;}}}

(defmacro gsl-gui-draw-corner (rotation x y);;{{{
  "Draw a single gui corner at <x,y> rotated to <rotation>"
  `(gsl-with-translate (:x ,x :y ,y)
      (gsl-with-rotate (:z ,rotation)
	 (gsl-draw-rect :tex *GSL-GUI-CORNER-TEX*
			:x (/ (mirror *GSL-GUI-CORNER-SIZE*) 1.5)
			:y (/ (mirror *GSL-GUI-CORNER-SIZE*) 1.5)
			:w (* *GSL-GUI-CORNER-SIZE* 2) 
			:h (* *GSL-GUI-CORNER-SIZE* 2)))))
;;}}}

(defmacro gsl-gui-draw-border (x y x2 y2);;{{{
  "Draws a single border with gui textures from <x,y> to <x2,y2>"
  `(let ((angle (gsl-to-degrees (gsl-get-angle ,x ,y ,x2 ,y2))) (width (gsl-get-dist ,x ,y ,x2 ,y2)))
     (gsl-with-translate (:x ,x :y ,y)
	(gsl-with-rotate (:z (- 90 angle))
	   (gsl-draw-rect :y (mirror (/ *GSL-GUI-BORDER-SIZE* 2.0)) 
			  :h *GSL-GUI-BORDER-SIZE* 
			  :w width 
			  :tex *GSL-GUI-BORDER-TEX* 
			  :repeat-sizex *GSL-GUI-BORDER-SIZE*)))))
;;}}}

(defmacro gsl-gui-draw-borders (x y w h);;{{{
  "Draw the border outlines of the rect <x,y,w,h>"
  `(gsl-with-color (:list *GSL-GUI-BORDER-COLOR*)
      (let ((x2 (+ ,x ,w)) (y2 (+ ,y ,h)))
	(gsl-gui-draw-border ,x ,y x2 ,y)	;Bottom
	(gsl-gui-draw-border ,x y2 x2 y2) 	;Top
	(gsl-gui-draw-border ,x ,y ,x y2)	;Left
	(gsl-gui-draw-border x2 ,y x2 y2))))	;Right
;;}}}

(defmacro gsl-gui-draw-background (x y w h);;{{{
  "Draws the background for a gui"
  `(gsl-with-color (:list *GSL-GUI-BACKGROUND-COLOR*)
      (gsl-draw-rect :x ,x :y ,y :w ,w :h ,h)));;}}}

(defmacro gsl-gui-draw-corners (x y w h);;{{{
  "Draw the corners of gui rect <x,y,w,h>"
  `(gsl-with-color (:list *GSL-GUI-CORNER-COLOR*)
      (let ((x2 (+ ,x ,w)) (y2 (+ ,y ,h)))
	(gsl-gui-draw-corner 0   ,x ,y)		;Bottom-left
	(gsl-gui-draw-corner 90  x2 ,y)		;Bottom-right
	(gsl-gui-draw-corner 180 x2 y2)		;Top-right
	(gsl-gui-draw-corner 270 ,x y2))))	;Top-left
;;}}}

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

(defmacro gsl-gui-draw-box (x y w h);;{{{
  "Draws a gui border and background"
  `(gsl-with-textures
     (gsl-with-blendfunc (+GL-SRC-ALPHA+ +GL-ONE-MINUS-SRC-ALPHA+)
	(gsl-gui-draw-background ,x ,y ,w ,h)
	(gsl-gui-draw-borders ,x ,y ,w ,h)
	(gsl-gui-draw-corners ,x ,y ,w ,h))));;}}}

(defmacro gsl-gui-draw (gui);;{{{
  "Draws <gui>"
  `(gsl-gui-draw-box (gsl-gui-x ,gui) (gsl-gui-y ,gui) (gsl-gui-width ,gui) (gsl-gui-height ,gui)));;}}}

(defmacro pos-in-gui (gui x y);;{{{
  "Returns t if <x,y> is inside <gui>"
  `(gsl-x-in-rect ,x ,y (gsl-gui-x ,gui) (gsl-gui-y ,gui) (gsl-gui-width ,gui) (gsl-gui-height ,gui)));;}}}

(defun gsl-mouse-is-on-gui (x y);;{{{
  "Returns t if <x,y> is on any gui in *GSL-GUI-LIST*"
  (dolist (gui *GSL-GUI-LIST*)
    (when (pos-in-gui gui x y) (return-from gsl-mouse-is-on-gui t))));;}}}
