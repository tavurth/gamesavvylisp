(load "gsl.lisp")

(gsl-init :options (logior +GSL-DEFAULT-VIDEO+ +GSL-GET-MOUSE+))

;;Initialising shader
(defparameter *shader* (gsl-shader-new :frag "b_w.frag"))
;;Loading texture
(defparameter *tex*    (gsl-load-tex "skyline.tga"))

(defun input ()
  (gsl-pump-events)
  (when (gsl-get-key +SDLK-ESCAPE+) (gsl-quit)))

(defun draw ()
  (gl-clear (logior +GL-COLOR-BUFFER-BIT+ +GL-DEPTH-BUFFER-BIT+))
  (gl-load-identity)
  (gl-translate :z -50)

  ;;Draw the following with this shader
  (gsl-with-shader *shader*
    (gsl-draw-rect :fill-screen t :with-tex *tex*))

  (gl-swap-buffers))

(loop
  (input)
  (draw))
