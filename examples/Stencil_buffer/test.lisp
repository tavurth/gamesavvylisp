(load "gsl.lisp")

(gsl-init :options (logior +GSL-DEFAULT-VIDEO+))

(defun draw ()
  (gl-clear +GL-COLOR-BUFFER-BIT+)
  (gl-translate :z -10)

  (gsl-draw-rect -10 -10 :w 20 :h 20)
  (gl-swap-buffers))

(defun input() 
  (gsl-pump-events)
  (when (gsl-get-key +SDLK-ESCAPE+) (gsl-quit)))

(defun main ()
  (draw)
  (input)
  (sdl-delay 50)
  (gsl-load-updates))

(loop (main))
