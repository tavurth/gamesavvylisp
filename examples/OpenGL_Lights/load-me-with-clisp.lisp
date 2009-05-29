(load "gsl.lisp")
(gsl-init :options (logior +GSL-DEFAULT-VIDEO+ +GSL-GET-MOUSE+))

;;Setting up the fbo and box texture
(setf *tex* (gsl-load-tex "box.tga"))
(setf *fbo* (gsl-fbo-new *width* *height*))
(gsl-fbo-add-color *fbo* 0)
(gsl-fbo-add-depth *fbo*)

;;Setting up the camera
(gl-clear-color 0 0 0)
(setf *camera* '(0 25))
(setf *cube-rotation*     0)

;;Setting up our light
(gl-enable +GL-LIGHT0+)
(gl-light +GL-LIGHT0+ +GL-AMBIENT+ (1 1 1 1))
(gl-light +GL-LIGHT0+ +GL-DIFFUSE+ (1 1 1 1)) 

(defun draw ()

  ;;Enabling lighting and then drawing to the fbo (lighting is automatically disabled again at closing brace)
  (gsl-with-lights
    (gsl-with-fbo *fbo*
 	(gl-clear (logior +GL-COLOR-BUFFER-BIT+ +GL-DEPTH-BUFFER-BIT+))
	(gl-load-identity)
	
	;;Setting up the camera
	(gl-translate :y -5 :z -50)
	(gl-rotate :x (second *camera*) :y (first *camera*))
	(gl-translate :x -20)
	
	;;Setting the light position
	(gl-light +GL-LIGHT0+ +GL-POSITION+ (-5 5 0 1.0))
	
	;;Drawing the cubes
	(gsl-with-textures
	  (gsl-with-depthtest
	    (dotimes (x 5)
	      (gsl-draw-cube :sizey 20 :x (* x 10) :centered t :anglex *cube-rotation* :tex-all *tex*))))))
    ;;Lighting has been disabled again here

  ;;Drawing the fbo to the screen with blending to mimic motion blur
  (gsl-with-blendfunc (+GL-SRC-ALPHA+ +GL-ONE-MINUS-SRC-ALPHA+)
	(gsl-with-textures
	  (gsl-with-color (1 1 1 0.2)
		(gsl-draw-rect :fill-screen t :with-fbo *fbo*))))
  
  (gl-swap-buffers))

(defun input ()
  (gsl-pump-events)
  (when (gsl-get-key +SDLK-ESCAPE+) (gsl-quit))
  
  ;;Incrementing the cubes rotating
  (incf *cube-rotation* 5)

  (decf (first *camera*) (/ (gsl-mouse-motion +x+) 5.0))
  (incf (second *camera*) (/ (gsl-mouse-motion +y+) 5.0)))

(loop
  (input)
  (draw))
