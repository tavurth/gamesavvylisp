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

(in-package :gsl-animation)

(defclass frame ();;{{{
  ((texture
     :initform nil
     :initarg :tex
     :accessor gsl-animation-frame-texture)
   (speed
     :initform 200
     :initarg :speed
     :accessor gsl-animation-frame-speed)));;}}}

(defclass animation ();;{{{
  ((frames
     :initform (make-array 1 :element-type 'frame :fill-pointer 0 :adjustable t)
     :accessor gsl-animation-frames)
   (speed
     :initform 200
     :initarg :speed
     :accessor gsl-animation-speed)));;}}}

(defclass anim ();;{{{
  ((animation
     :initform nil
     :initarg :animation
     :accessor gsl-anim-animation)
   (current-frame-n
     :initform 0
     :initarg :current-frame
     :accessor gsl-anim-current-frame)
   (current-frame
     :initform nil
     :accessor gsl-anim-current)
   (next-frame-time
     :initform 0
     :initarg :next-frame-time
     :accessor gsl-anim-next-frame-time)));;}}}

(defun gsl-animation-new (&key (speed 0.2));;{{{
  "Creates and returns a new animation instance"
  (make-instance 'animation :speed speed));;}}}

(defun gsl-anim-new (animation);;{{{
  "Creates and returns a new anim instance"
  (make-instance 'anim :animation animation));;}}}

(defun gsl-animation-frame-new (loc &key (speed 0.2));;{{{
  "Creates and returns a new frame instance"
  (make-instance 'frame :tex (gsl-load-tex loc) :speed speed));;}}}

(defmethod gsl-animation-frame ((anim animation) n);;{{{
  "Returns frame <n> from <anim>"
  (let ((frames (gsl-animation-frames anim)))
    (let ((len (length frames)))
      (if (or (> n len) (< n 0))
	(aref frames len)
	(aref frames n)))));;}}}

(defmethod gsl-animation-add-frame ((anim animation) loc &key (speed 0.2));;{{{
  "Adds an frame to <anim>"
  (let ((frame (gsl-animation-frame-new loc :speed speed)))
    (vector-push-extend frame (gsl-animation-frames anim))
    (return-from gsl-animation-add-frame frame)));;}}}

(defmethod gsl-anim-update-time ((anim anim) &optional (time (sdl-get-ticks)));;{{{
  "Updates the time of the next frame of <anim>. Pass in a time if you have already got one
  from SDL to save CPU cycles"
  (let ((animation (gsl-anim-animation anim)) (current-frame-time (gsl-animation-frame-speed (gsl-anim-current anim))))
    (setf (gsl-anim-next-frame-time anim) (+ current-frame-time (gsl-animation-speed animation) time))));;}}}

(defmethod gsl-anim-next-frame ((anim anim));;{{{
  "Increments the current frame. If the current frame is > than the length of the
  array of frames, the current frame is reset to zero"
  (let ((animation (gsl-anim-animation anim)))
    (let ((frames (gsl-animation-frames animation)))
      (let ((len (length frames)) (current-frame (incf (gsl-anim-current-frame anim))))
	(when (>= current-frame len) (setf (gsl-anim-current-frame anim) 0))
	(setf (gsl-anim-current anim) (gsl-animation-frame animation (gsl-anim-current-frame anim)))))));;}}}

(defmacro gsl-frame-set-speed (frame speed);;{{{
  "Sets the speed of <frame> to <speed> ms"
  `(setf (gsl-animation-frame-speed ,frame) ,speed));;}}}

(defmethod gsl-anim-update ((anim anim));;{{{
  "Updates <anim> only if it is time for the next frame"
  (let ((next-frame-time (gsl-anim-next-frame-time anim)) (current-time (sdl-get-ticks)))
    (when (> current-time next-frame-time)
      (gsl-anim-next-frame anim)
      (gsl-anim-update-time anim current-time))));;}}}

(defmethod gsl-anim-set-animation ((anim anim) animation);;{{{
  "Sets <anim>'s animation to <animation>"
  (setf (gsl-anim-animation anim) animation));;}}}

(defmethod gsl-anim-current-tex ((anim anim));;{{{
  "Returns the current texture frame in use by anim" 
  (gsl-animation-frame-texture (gsl-anim-current anim)));;}}}
