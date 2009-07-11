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

(in-package :gsl-console)

(defparameter *temp-str* nil)

;;	GET INPUT > STRING;;{{{

(defmacro when-mod (mod rest);;{{{
  "When GSL keymods & mod do <rest>"
  `(when (logand ,mod (gsl-get-mods))
     ,rest));;}}}

(defmacro if-mods (&optional true false);;{{{
  "If there are any mods active"
  `(if (> (gsl-get-mods) 0) ,true ,false));;}}}

(defmacro when-mods (&rest rest);;{{{
  "When there are any mods active do <rest>"
  `(when (> (gsl-get-mods) 0)
     ,@rest));;}}}

(defun char-is-num (char);;{{{
  "If the character passed is a number"
  (when (typep char 'string) (setf char (char char 0)))
  (let ((c-int (char-int char)))
    (when (and (> c-int 47) (< c-int 58))
      c-int)));;}}}

(defun shift-case-key (key);;{{{
  "Converts a key into its shifted character case"
  (let ((num (char-is-num key)))
    (when num
      (decf num 48)
      (cond
	((when (= num 0) (setf key ")")))
	((when (= num 1) (setf key "!")))
	((when (= num 2) (setf key "@")))
	((when (= num 3) (setf key "#")))
	((when (= num 4) (setf key "$")))
	((when (= num 5) (setf key "%")))
	((when (= num 6) (setf key "^")))
	((when (= num 7) (setf key "&")))
	((when (= num 8) (setf key "*")))
	((when (= num 9) (setf key "(")))))
    (cond
      ((string= key "'") (setf key "\""))
      ((string= key "`") (setf key "~"))
      ((string= key "=") (setf key "+"))
      ((string= key "-") (setf key "_"))
      ((string= key "\\")(setf key "|")))

    (setf key (string-capitalize key)))
  (return-from shift-case-key key));;}}}

(defun test-key-mods (key);;{{{
  "Checks weither any mods are held when key is pressed"
  (when-mods
    (when-mod +KMOD-SHIFT+ (setf key (shift-case-key key))))
  (return-from test-key-mods key));;}}}

(defun key-tests (key);;{{{
  "Test for special keys"
  (cond
    ((when (string= key "backspace") 	(setf *temp-str* (backspace *temp-str*))))
    ((when (string= key "space")	(push-char #\Space *temp-str*)))))
;;}}}

(defun key-event-callback (key type);;{{{
  "Called when a key is pressed"
  (when (equalp type +SDL-KEYDOWN+)
    (let ((this-key (sdl-get-key-name key)))
      (let ((len (length this-key)))		;Evaluate the length
	(cond 
	  ((> len 1)	(key-tests this-key))	;if the key is a string (backspace, space, return etc.);
	  ((> len 0)	(push-string (test-key-mods this-key) *temp-str*)))))))	;else if the key is a single char;;;}}}

(defun gsl-read-input-to-string (&optional string);;{{{
  "Get input in the form of a string"
  (setf *temp-str* string)
  (let ((*GSL-KEY-EVENT-FUNC* #'key-event-callback))
    (gsl-pump-events)
    *temp-str*));;}}};;}}}

;;	CONSOLE-UTILS;;{{{

(defun eval-string (str);;{{{
  "Lisp evals string that is passed to it"
  (when (> (length str) 0)
    (eval (read-from-string str))));;}}}

(defun gsl-enter-console ();;{{{
  "Enter the console"
  (setf *console-curr* "")
  (gsl-skip-events 250)
  (setf *GSL-IS-IN-CONSOLE* t));;}}}

(defun exit-console ();;{{{
  "Quit the console"
  (setf *console-curr* ""
  	*GSL-CURSOR-Y* 0
	*GSL-CURSOR-X* 0
  	*GSL-IS-IN-CONSOLE* nil))
;;}}}

(defmacro remove-oldest (list);;{{{
  "Remove the oldest string from the console history"
  `(setf ,list (reverse (cdr (reverse ,list)))));;}}}

(defun add-to-console-prev (line);;{{{
  "Adds to the consoles command history"
  (when (> (length *console-prev*) *GSL-MAX-CONSOLE-PREV*) (remove-oldest *console-prev*))
  (push line *console-prev*));;}}};;}}}

;;	CONSOLE-TEXT;;{{{
(defun add-to-console (line);;{{{
  "Adds <line> to the console"
  (when (> (length *console*) *GSL-MAX-CONSOLE*) (remove-oldest *console*))
  (push line *console*));;}}}

(defun gsl-clear-console ();;{{{
  "Clears the console and the console history"
  (setf *console* nil)
  (setf *console-prev* nil));;}}}

(defun set-console-curr (line);;{{{
  "Sets the consoles current line to <line>"
  (when (not line) (setf line ""))
  (setf *console-curr* line));;}}}

(defun format-console (string args);;{{{
  "Format but for the GSL-console"
  (let ((list nil))
    (dolist (line args)
      (push (format nil "~s" line) list))
    (setf list (reverse list))
  (add-to-console (eval `(format nil ,string ,@list)))));;}}}

(defun gsl-print-console (line);;{{{
  "Use for simple commands instead of format-console"
  (add-to-console line));;}}};;}}}

;;	DRAW-CONSOLE;;{{{
(defun draw-console (x y sizex sizey);;{{{
  "Draws the console history"
  (dolist (line *console*)
    (gsl-draw-string line x (incf y (+ sizey 1)) 0 sizex sizey)))
;;}}}

(defun gsl-draw-console ();;{{{
  "Call this function to draw the console"
  (gl-load-identity)
  (gl-translate :z -25)

  ;;Console black background
  (gsl-with-blendfunc (+GL-SRC-ALPHA+ +GL-ONE-MINUS-SRC-ALPHA+)
      (gsl-with-color (:a 0.8)
          (gsl-draw-rect :x -42 :y -22 :w *width* :h 28)))

  ;;Console white edges
  (gsl-with-color (:r 1 :g 1 :b 1 :a 1)
	 (gsl-with-draw +GL-LINES+
		(gl-vertex :x -42 :y -18)
		(gl-vertex :x 42  :y -18)
		(gl-vertex :x -42 :y 6)
		(gl-vertex :x 42  :y 6)))

  (gsl-with-font 0
	(gsl-draw-string *console-curr* -41 -20 0 1 1)
	(draw-console -41 -18 1 1)))
;;}}};;}}}

;;	CONSOLE-INPUT;;{{{
(defun console-parse-input (line);;{{{
  "Parses the consoles input (when you hit enter)"
  (when (> (length line) 1)
    (progn
      (add-to-console line)
      (add-to-console-prev line)
      (handler-case
	(eval-string line)
	(simple-condition (err) (format-console (simple-condition-format-control err) (simple-condition-format-arguments err)))
	(error () (gsl-print-console "An error has occurred")))))
  (exit-console))
;;}}}

(defun console-cursor-up ();;{{{
  "Increments the cursor pointer (up one command history)"
  (let ((len (list-length *console-prev*)))
    (when (> len 0)	;When the list is not empty
      (progn
	(when (> *GSL-CURSOR-Y* (- len 1)) (setf *GSL-CURSOR-Y* (- len 1)))	;(- len 1) Makes sure we never hit the last square 
	(set-console-curr (nth *GSL-CURSOR-Y* *console-prev*))
	(incf *GSL-CURSOR-Y*)))))						;For next operation;;}}}

(defun console-cursor-down ();;{{{
  "De-increments the console cursor position"
  (let ((len (list-length *console-prev*)))
    (when (> len 0)
      (progn
	(decf *GSL-CURSOR-Y*)
	(cond ((<= *GSL-CURSOR-Y* 0) (progn (setf *GSL-CURSOR-Y* 0) (setf *console-curr* "")))	;If the cursor is at 0 or below clear curr and set cursor to 0;
	      (t (set-console-curr (nth *GSL-CURSOR-Y* *console-prev*))))))))			;Else just set the cursor to the nth;;;}}}

(defun test-console-keys ();;{{{
  "Test keys to see if a special console action is required"
  (cond 
    ((gsl-get-key +SDLK-ESCAPE+) 	(exit-console))
    ((gsl-get-key +SDLK-RETURN+)	(console-parse-input *console-curr*))
    ((gsl-get-key +SDLK-UP+)		(console-cursor-up))
    ((gsl-get-key +SDLK-DOWN+)		(console-cursor-down))
    ((gsl-get-key +SDLK-PAGEDOWN+)      (setf *GSL-CURSOR-Y* 0))
    ((gsl-get-key +SDLK-PAGEUP+)	(setf *GSL-CURSOR-Y* (length *console-prev*)))));;}}}

(defun gsl-console-input ();;{{{
  "Get input in text form for the console"
  (let ((new-str (gsl-read-input-to-string *console-curr*)))
    (when new-str (setf *console-curr* new-str))
    (test-console-keys)))
;;}}}
