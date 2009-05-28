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

(in-package :gsl-draw)

(defun gsl-draw-points (&rest rest);;{{{
  "Draws a list of coordinates as points on the screen"
  (gl-begin +GL-POINTS+)
  (loop
    (if (not rest) (return))
    (eval `(gl-vertex ,(pop rest) ,(pop rest))))
  (gl-end))
;;}}}

(defun gsl-draw-char (char x y z w h);;{{{
  "Draws a character at the specified coordinates"
  (gsl-with-blendfunc (+GL-SRC-ALPHA+ +GL-ONE-MINUS-SRC-ALPHA+)
      (gsl-with-textures
	(eval `(gsl_draw_char *GSL-DEFAULT-FONT* ,(char-int char) ,x ,y ,z ,w ,h)))))
;;}}}

(defun gsl-draw-string (text x y z w h);;{{{
  "Draws the string at the specified coordinates"
  (let ((len (length text)))
    (dotimes (a len)
      (eval `(gsl-draw-char ,(aref text a) ,x ,y ,z ,w ,h))
      (incf x w))))
;;}}}

(defmacro gsl-draw-tex (tex x y z w h);;{{{
  (if (equalp tex 0)
    `(gsl_draw_tex 0 ,x ,y ,z ,w ,h)
    `(gsl_draw_tex (gsl-tex-id ,tex) ,x ,y ,z ,w ,h)));;}}}

(defmacro gsl-draw-rect (&key (x 0) (y 0) (z 0) (w 512) (h 512) (tex 0) (fill-screen nil) (with-tex nil) (with-fbo nil) (fbo-color-pos 0));;{{{
  (let ((form nil))
    (when with-tex (setf tex with-tex)) 
    (when with-fbo (setf tex `(gsl-fbo-color-pos ,with-fbo ,fbo-color-pos) with-tex t))
    
    ;;In this next section we setf form to be one of two options. This form is then returned or used with (gsl-with-textures) and then returned
    ;;Depending on the users options.

    (if fill-screen	;;If we want the texture to completely fill the screen. Useful for fbo and multiple pass sequences.
	(setf form `(progn
		      (gsl-with-pushmatrix
			(gl-load-identity)
			(gl-translate :z -1)
			(gsl-draw-tex ,tex (mirror *aspect*) -1 0 (* *aspect* 2) 2))))
	(setf form `(gsl-draw-tex ,tex ,x ,y ,z ,w ,h)))

    (if with-tex
      (return-from gsl-draw-rect `(gsl-with-textures ,form))	;Enable textures only for this texture
      (return-from gsl-draw-rect form))))			;Just return the drawing commands

;;}}}

(defun gsl-draw-cube (&key (x 0) (y 0) (z 0) (size 5) (tex-l 0) (tex-r 0) (tex-f 0) (tex-b 0) (tex-t 0) (tex-b 0) (tex-a nil))
  (when tex-a (setf tex-f tex-a tex-b tex-a tex-l tex-a tex-r tex-a tex-t tex-a tex-b tex-a))
  (gsl_draw_cube x y z size size size tex-l tex-r tex-f tex-b tex-t tex-b))
