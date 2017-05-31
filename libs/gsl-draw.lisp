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

(in-package :gsl-draw)

(defun gsl-draw-points (&rest rest)
  "Draws a list of coordinates as points on the screen"
  (gl-begin +GL-POINTS+)
  (loop
    (if (not rest) (return))
    (eval `(gl-vertex :x ,(pop rest) :y ,(pop rest))))
  (gl-end))


(defun gsl-draw-char (char x y z w h)
  "Draws a character at the specified coordinates"
  (gsl-with-blendfunc (+GL-SRC-ALPHA+ +GL-ONE-MINUS-SRC-ALPHA+)
      (gsl-with-textures
	(eval `(gsl_draw_char *GSL-DEFAULT-FONT* ,(char-int char) ,x ,y ,z ,w ,h)))))


(defun gsl-draw-string (text x y z w h)
  "Draws the string at the specified coordinates"
  (let ((len (length text)))
    (dotimes (a len)
      (eval `(gsl-draw-char ,(aref text a) ,x ,y ,z ,w ,h))
      (incf x))))


(defmacro gsl-draw-tex (tex x y z w h &optional (repeatX 0.0) (repeatY 0.0))
  "Draw tex to the OpenGL screen"
  (if (equalp tex 0)
    `(gsl_draw_tex 0 (float ,x) (float ,y) (float ,z) (float ,w) (float ,h) (float ,repeatX) (float ,repeatY))
    `(gsl_draw_tex (gsl-tex-id ,tex) (float ,x) (float ,y) (float ,z) (float ,w) (float ,h) (float ,repeatX) (float ,repeatY))))

(defmacro gsl-draw-rect (&key (x 0) (y 0) (z 0) (pos nil) (w 1) (h 1) (fill-screen nil)
			     (tex 0) (with-tex nil) (repeat-sizex 0.0) (repeat-sizey 0.0)
			     (with-fbo nil) (fbo-color-pos 0)
			     (anim nil) (with-anim nil))
  "Draw a new rect on the screen"
  (let ((form nil))

    (when pos (setf x `(first ,pos) y `(second ,pos) z `(third ,pos)))
    ;;Setting up what we are sending to be drawn. We use the tex variable to contain whatever needs to be drawn.
    (when with-tex  (setf tex with-tex)) 
    (when with-fbo  (setf tex `(gsl-fbo-color-pos ,with-fbo ,fbo-color-pos) with-tex t))
    ;;Animations
    (when with-anim (setf tex `(gsl-anim-current-tex ,with-anim)))
    (when anim      (setf tex `(gsl-anim-current-tex ,anim)))
    (when (or with-anim with-fbo) (setf with-tex t))
    
    ;;In this next section we setf form to be one of two options. This form is then returned or used with (gsl-with-textures) and then returned
    ;;Depending on the users options.

    (if fill-screen	;;If we want the texture to completely fill the screen. Useful for fbo and multiple pass drawing sequences.
	(setf form `(progn
		      (gsl-with-pushmatrix
			(gl-load-identity)
			(let ((x (mirror *width*)))
			  (gl-translate :z (- x 10))
			  (setf x (* x 2))
			  ;;		     X              Y         Z      W                H
			  (gsl-draw-tex ,tex x (* x *aspect-y*) 0 (* *width* 4) (* (* *width* 4) *aspect-y*))))))
 	(setf form `(gsl-draw-tex ,tex ,x ,y ,z ,w ,h ,repeat-sizex ,repeat-sizey)))

    (if with-tex
      (return-from gsl-draw-rect `(gsl-with-textures ,form))	;Enable textures only for this texture
      (return-from gsl-draw-rect form))))			;Just return the drawing commands



(defmacro gsl-draw-cube (&key (x 0) (y 0) (z 0) (size nil size_passed) (sizex 5) (sizey 5) (sizez 5)
			      (anglex 0.0 anglex_passed) (angley 0.0 angley_passed) (anglez 0.0 anglez_passed)
			      (centered 0) (tex-left 0) (tex-right 0) (tex-front 0) (tex-back 0) (tex-top 0) (tex-bottom 0) (tex-all nil))
  "Draw a cuboid on the screen"

  ;;COMPILE TIME:
  
  ;;If a global size value is passed set all sizes to it
  (when size_passed (setf-all sizex sizey sizez size))

  (when centered (setf centered 1))

  ;;Making sure that we pass floats to the function instead of anything ints etc
  (when anglex_passed (setf anglex `(float ,anglex)))
  (when angley_passed (setf angley `(float ,angley)))
  (when anglez_passed (setf anglez `(float ,anglez)))

  ;;Converting our textures to their OpenGL texture ID's
  (when (not (numberp tex-left))   (setf tex-left   `(gsl-tex-id ,tex-left)))
  (when (not (numberp tex-right))  (setf tex-right  `(gsl-tex-id ,tex-right)))
  (when (not (numberp tex-back))   (setf tex-back   `(gsl-tex-id ,tex-back)))
  (when (not (numberp tex-front))  (setf tex-front  `(gsl-tex-id ,tex-front)))
  (when (not (numberp tex-top))    (setf tex-top    `(gsl-tex-id ,tex-top)))
  (when (not (numberp tex-bottom)) (setf tex-bottom `(gsl-tex-id ,tex-bottom)))
  
  ;;Creating our texture list
  (let ((textures (list tex-left tex-right tex-back tex-front tex-top tex-bottom)))
    
    ;;If a global texture list has been sent (:tex-all <texture>)  change all textures that have not been explicately specifed to the global.
    (when tex-all
      (setf textures (mapcar #'(lambda (tex) (if (numberp tex) `(gsl-tex-id ,tex-all) tex)) textures)))
    
    ;;RUN TIME:
    
    (return-from gsl-draw-cube `(gsl_draw_cube ,x ,y ,z ,sizex ,sizey ,sizez ,anglex ,angley ,anglez ,centered ,@textures)))) 

(defun gsl-draw-quad (&key tex (x1 0.0) (y1 0.0) (x2 0.0) (y2 0.0) (x3 0.0) (y3 0.0) (x4 0.0) (y4 0.0))
  (gsl-with-textures
    (gsl-with-draw +GL-QUADS+
	(when tex (gl-bind-texture +GL-TEXTURE-2D+ (gsl-tex-id tex)))
	(gl-tex-coord x1 y1) (gl-vertex :x x1 :y y1)
	(gl-tex-coord x2 y2) (gl-vertex :x x2 :y y2)
	(gl-tex-coord x3 y3) (gl-vertex :x x3 :y y3)
	(gl-tex-coord x4 y4) (gl-vertex :x x4 :y y4))))

(defun gsl-draw-tri (&key tex
			  (x1 0.0) (y1 0.0) (z1 0.0)
			  (x2 0.0) (y2 0.0) (z2 0.0)
			  (x3 0.0) (y3 0.0) (z3 0.0))
  (gsl-with-textures
    (gsl-with-draw +GL-TRIANGLES+
	(when tex (gl-bind-texture +GL-TEXTURE-2D+ (gsl-tex-id tex)))
	(gl-tex-coord x1 y1) (gl-vertex :x x1 :y y1 :z z1)
	(gl-tex-coord x2 y2) (gl-vertex :x x2 :y y2 :z z2)
	(gl-tex-coord x3 y3) (gl-vertex :x x3 :y y3 :z z3))))

