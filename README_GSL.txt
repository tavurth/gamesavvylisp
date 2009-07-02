;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;										;;;
;;;		     ((	GAME SAVVY LISP (GSL) ))				;;;
;;;										;;;
;;;		(( Copyright (c) William Whitty 2009 ))				;;;
;;;			        V_0.55_Beta		     			;;;
;;;										;;;
;;;			This file is part of GSL. 				;;;
;;;										;;;
;;;	GSL is free software: you can redistribute it and/or modify		;;;
;;;     it under the terms of the GNU Lesser General Public License as published by	;;;
;;;     the Free Software Foundation, either version 3 of the License, or	;;;
;;;     (at your option) any later version.					;;;
;;;										;;;
;;;     GSL is distributed in the hope that it will be useful,			;;;
;;;     but WITHOUT ANY WARRANTY; without even the implied warranty of		;;;
;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the		;;;
;;;     GNU Lesser General Public License for more details.				;;;
;;;										;;;
;;;     You should have received a copy of the GNU Lesser General Public License	;;;
;;;     along with GSL.  If not, see <http://www.gnu.org/licenses/>.		;;;
;;;										;;;
;;;	I would really like to hear any thoughts you have about this lib.	;;;
;;;		You can contact me at deylen@gmail.com :)			;;;
;;;										;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;										;;;
;;;										;;;
;;;	GSL USES THE FOLLOWING LIBRARIES:					;;;
;;;			      \ \						;;;
;;;			       \ `---===>> SDL 	(http://www.libsdl.org/)	;;;
;;;				`---===>> GLee	(http://elf-stone.com/)		;;;
;;;										;;;
;;;	You can find the readme and licenses for both these files		;;;
;;;	in the GSL directory.							;;;
;;;										;;;
;;;										;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	Git access	;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

	Git access for GSL is available: 
		git://gamesavvylisp.git.sourceforge.net/gitroot/gamesavvylisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	Getting GSL up and running:	;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	To use GSL you will need to have the following:
		
		- Clozure CL: http://trac.clozure.com/openmcl/wiki/WikiStart#GettingClozureCL

	A basic gsl structure is given in the folder 'Basic' in
	the examples directory. Feel free to use this in whatever 
	projects you like.

	--------------------------------------------------------------------------------------------------
	1) Copy the gsl.lisp file into your project directory.

	2) Open the gsl.lisp file and change the *gsl-dir* to point to the location of the GSL
	   base directory. (relative paths are allowed)

	3) Change the *clib-type* variable to account for your operating system (windows: .dll linux: .so)
	---------------------------------------------------------------------------------------------------

	Now when using GSL all you need do is to include "gsl.lisp" into
	your program. GSL will then automatically initialise all of the
	required libraries for you and import the gsl symbols into your
	ccl-user package.

	If you are using multiple source files you can import all gsl functionality by using (gsl::use-all)
	You can also import certain parts of gsl by using the macros 
								(gsl::use-input)
								(gsl::use-video)

	Or individually import gsl's files:
					(use-package :gsl-input)
					(use-package :gsl-shared)
					. . .

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	FELLOW VIM USERS!      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	Howdy :D
	
	There are a lot of folds in my source files. If you want vim
	to recognise them put this at the end of your ~/.vimrc file: 
	
	set foldmethod=marker
	au BufNewFile,BufRead *.lisp set commentstring=;;%s

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	Using the GSL runtime code modification.	;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	This allows you to actually edit your code while your program is running, and see the effects immediately in your OpenGL window.
	
	Editors currently supported:
		- Vim

	If an editor you use is not on the list, it should be fairly simple to add support for it.
	A file called update-code.pl in the 'GSL/utils' folder takes care of everything relating to finding your current function and updating
	the GSL executable.
	 All you need to do is provide it with the full directory path of the current source file and the line number
	your cursor is currently on. It will take care of the rest :)

	Vim:
	----
		Linux:
		------
			Open the file 'GSL/update-gsl.vim' with any text editor and change the line
			let gslHome=""
			To point to the location where you are going to keep the GSL utils directory.
			Eg. let gslHome="/home/yourname/code/GSL/utils"
			
			Copy the 'GSL/update-gsl.vim' into your '/home/yourname/.vim/plugin/' folder. If the vim folder does not exist
			Create it.

			Add the following to the end of your '/home/yourname/.vimrc'
				source /home/yourname/.vim/plugin/update-gsl.vim
				nnoremap <C-p> :call UpdateCode()<CR>
			
			When you press Control and P your code will now be loaded into your running program!!

			If you have any trouble with this, send me an e-mail and I'll try to help out :)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	FUNCTIONS:	;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   GSL Functions Available:   ;;
;;   ------------------------   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;;;;;;;;;;;;;;;;;;;
	   gsl-main.lisp
	;;;;;;;;;;;;;;;;;;;
		gsl-init
		--------
			:options
				+GSL-CATCH-MOUSE+	
					Catch the mouse in the center of the created window and get all mouse move events;
				+GSL-HIDE-MOUSE+
				+GSL-GET-MOUSE+		
					Bitwise OR of +GSL-CATCH-MOUSE+ and +GSL-HIDE-MOUSE+
				+GSL-DEFAULT-VIDEO+
					Use the GSL default video settings (Very much reccomended as this calls glu-perspective
					etc for you. Take a look in the clibs/gsl-bindings.c file to see exactly what options
					are set up.
			:flags
				These are the flags that are used in the SDL_Init(flags) function.
				+SDL-INIT-VIDEO+ is passed by default.	

		gsl-quit
		--------
			Use this to quit your GSL game or application. (Cleans up all memory allocated)	

		gsl-init-video
		--------------
			Use this function if you did not specify +GSL-DEFAULT-VIDEO+ in gsl-init.
				ARGS (&optional (width 1024) (height 512) (bpp 32) (flags 0 flags_passed))
				flags is used when initialising OpenGL. (Defaults to +GSL-DEFAULT-VIDEO+ to set up OpenGL for you)

		gsl-new-font
		------------
			This is automatically called for you in gsl.lisp to initialise a default console font.

		gsl-draw-char
		-------------
			Draw a single character to the screen. 
			ARGS: (gsl-draw-char (char x y z w h))
		gsl-draw-string
		---------------
			(gsl-draw-string (text x y z w h))

		gsl-draw-tex
		------------
			Draws a texture at the specified coordinates;
			(gsl-draw-tex (tex x y w h))

		gsl-draw-points
		---------------
			Draws a list of points to the screen.
			Example:
				(gsl-draw-points
					0 0
					1 0
					150 0)
		gsl-with-draw
		-------------
			Calls (gl-begin <TYPE>) at the opening bracket and then (gl-end) at the close.
			Example:
				(gsl-with-draw +GL-QUADS+
					(gl-vertex 0 0)
					(gl-vertex 1 0)
					(gl-vertex 1 1)
					(gl-vertex 0 1))
		gsl-draw-rect
		-------------
			Draws a rectangular surface at the given coordinates.
			(gsl-draw-rect (&key (x 0) (y 0) (z 0) (w 512) (h 512) (tex 0) (fill-screen nil) (with-tex nil))
			:tex 		specify a texture to be put onto the rect.
			:with-tex	same as :tex but enable textures at the same time. 
			:fill-screen	fill the entire screen with the texture. (Useful for FBO multiple-pass rendering)
			:with-fbo	Use the following fbo to draw with.
			:fbo-color-pos  Use the color buffer n (defaults to zero)
			:anim		Specify an anim from which the current frame will be used as a texture.
			:with-anim	Same as :anim but enable textures at the same time.

		gsl-draw-cube
		-------------
			Draws a cube at the given coordinates.
			(gsl-draw-cube (&key (x 0) (y 0) (z 0) (size nil) (sizex 5) (sizey 5) (sizez 5) 
						(anglex 0) (angley 0) (anglez 0) (centered 0)
						(tex-left nil) (tex-right nil) (tex-front nil) (tex-back nil) (tex-top nil) (tex-bottom nil)
						(tex-all nil)))

			
			:size		Sets the global size for all the size parameters (sizex sizey sizez)
			:anglex-z	Sets the angle that the cube should be rotated to before drawing.
			:centered	If t this cube will be drawn with its center at (x y z) instead of its corner.
			:tex		Pass in texture objects and they will be applied to the sides of the cube.
			:tex-all	Set the texture of all the sides exept those specified with :tex-<side>

		gsl-draw
		--------
			Draws a primitive shape (Only rects supported at the moment)
			(gsl-draw ((rect rect) &key (tex nil))
			
		use-all
		-------
			This macro makes your source file automatically use the required GSL dependancies.	(Called for you at init)

	;;;;;;;;;;;;;;;;;;;;;;
	   gsl-updates.lisp  
	;;;;;;;;;;;;;;;;;;;;;;

		gsl-load-updates
		----------------
			If any updates are present load them (Should be called once per frame, GSL takes care of timing to avoid slowdown.)

	;;;;;;;;;;;;;;;;;;;;;;
	   gsl-classes.lisp:
	;;;;;;;;;;;;;;;;;;;;;;
	   
	   Textures
	   --------

		gsl-load-tex
		------------
			(gsl-load-tex (loc &key (filter-min +GL-LINEAR-MIPMAP-NEAREST+) (filter-mag +GL-NEAREST+) (wrap-s +GL-REPEAT+)
						(wrap-t +GL-REPEAT+)))
			Use this function to load a texture. (Currently only TGA is supported.)
				Ex:
				       (gsl-load-tex "test.tga" :filter-min +GL-NEAREST+ :wrap-s +GL-CLAMP+)

		gsl-delete-tex
		--------------
			Use this function to delete a single texture Object that has been loaded by GSL

		gsl-delete-all-textures
		-----------------------
			Cleanup - use this to delete all textures loaded by GSL (Called automatically from (gsl-quit))
		
		Texture attributes:
		-------------------

			gsl-tex-nused
			-------------
				Number of times that the texture with this location-id has been used

			gsl-tex-id
			----------
				The OpenGL texture ID for this texture

			gsl-tex-loc
			-----------
				The location of this texture on the HDD

			gsl-tex-width
			-------------
				Width of texture

			gsl-tex-height
			--------------
				Height of texture
	
		gsl-draw
		--------
			Draw a given shape primitve (Only supports rects at the moment)

		gsl-make-rect
		-------------
			Make a gsl-rect instance (x y w h x2 y2)

		gsl-draw-shadow		Draw the shadows given by light (x y) (Rect x y)
		---------------
			Draw the shadows given by light (x y) for rect (Rect) (Rect x y)
	  
	
	   Framebuffers
           ------------

		gsl-fbo-new
		-----------
			Create a new Framebuffer object (w h)

		gsl-fbo-add-color
		-----------------
			Add a color buffer to the FBO (gsl-fbo-add-color <fbo> <color-position>) 

		gsl-fbo-del-color
		-----------------
			Delete a color buffer from the fbo (gsl-fbo-del-color <fbo> <pos>)

		gsl-fbo-use
		-----------
			Use the fbo for all operations following this one (gsl-fbo-use <fbo>)

		gsl-fbo-del
		-----------
			Totally delete a fbo object and all its color and depth buffers (gsl-fbo-del <fbo>)

		gsl-fbo-id
		----------
			Get the OpenGL fbo reference number (gsl-fbo-id <fbo>)

		gsl-fbo-width
		-------------
			Get the width of all the buffers attached to this framebuffer (They must all be the same) (gsl-fbo-width <fbo>)

		gsl-fbo-height
		--------------
			Get the height of all the buffers attached to this framebuffer (gsl-fbo-height <fbo>)

		gsl-fbo-color-pos
		-----------------
			Get the texture object at the nth position in the fbo (gsl-fbo-color-pos <fbo> <nth>)

		gsl-delete-all-fbos
		-------------------
			Delete all fbos in the GSL memory
		
		gsl-fbo-color-attachments
		-------------------------
			Get an array of all the color attachments in the fbo

		gsl-fbo-depth-attachments
		-------------------------
			Get an array of all the depth attachments in the fbo


   	Shaders
   	-------
		gsl-shader-set
		--------------
			(gsl-shader-set <shader> <"var-name"> <value>)

			Gets the address of <"var-name"> in <shader> and sets its value to <value>. Also stores
			the name for quicker access later.

		gsl-shader-get
		--------------
			(gsl-shader-get <shader> <"var-name">)

			Gets the current value of <"var-name"> in <shader>

		gsl-shader-vars
		---------------
			(gsl-shader-vars <shader>)
			
			Lists all the currently stored variables (that gsl knows about) in <shader>
		
		gsl-shader-id
		-------------
			(gsl-shader-id <shader>)
		
			Returns the OpenGL ID of <shader>

		gsl-shader-source
		-----------------
			(gsl-shader-source <shader> :frag "test.frag" :vert "test.vert")

			Changes the source of either :frag :vert or both in <shader>.

	;;;;;;;;;;;;;;;;;;;;
	   gsl-input.lisp:
	;;;;;;;;;;;;;;;;;;;;

		gsl-mouse-motion
		----------------
			Get the motion of the mouse since last frame, if called with no args returns both X and Y motion in a list (x y)
			(gsl-mouse-motion +x+) (gsl-mouse-motion +y+) (gsl-mouse-motion)

		gsl-get-key
		-----------
			Get the current state of the key (gsl-get-key +SDLK-ESCAPE+) full keysym list can be found in libs/gsl-sdl-keys.lisp

		gsl-get-mods
		------------
			Get an integer that has the current state of all keyboard modifiers

		gsl-pump-events
		---------------
			Pump all GSL events, you need to call this before other things such as (gsl-mouse-motion) for example. (gsl-pump-events)

		gsl-skip-events
		---------------
			Skips all events for n seconds. (gsl-skip-events <n>) Useful for when you are starting a menu and you want a pause
			before allowing user input.

		gsl-get-charkey
		---------------
			Return the next key pressed as a string. (gsl-get-charkey) might return "w" or "backspace" or "escape" etc.
			Useful for text input.
	
	;;;;;;;;;;;;;;;;;;;;;;
	   gsl-console.lisp:
	;;;;;;;;;;;;;;;;;;;;;;

		gsl-console-input
		-----------------
			Go to console input mode. (gsl-console-input)

		gsl-draw-console
		----------------
			Draw the console (gsl-draw-console)

		gsl-enter-console
		-----------------
			Enter console mode (gsl-enter-console)

		gsl-print-console
		-----------------
			Print text to the console (gsl-print-console <text>)

		gsl-clear-console
		-----------------
			Clear the console and all console histories (gsl-clear-console)


	;;;;;;;;;;;;;;;;;;;;;;;;
	   gsl-animation.lisp
	;;;;;;;;;;;;;;;;;;;;;;;;

		To create a 2D animation you will need two things:
			1: A animation parent. This stores things like the image list and timings of each image.
			2: A anim child. This stores things like individual timing sequences and current frame.
			
			Every frame you should update the animation with (gsl-anim-update <anim-name-here>)
			You can then call (gsl-draw-rect :anim <anim-name-here>) to draw the animation as you would draw a texture.
			Call (gsl-draw-rect :with-anim <anim-name-here>) if you want textures to be automatically enabled and then
			disabled again for you.

		gsl-animation-new
		-----------------
			(gsl-animation-new :speed 0.2)

			Creates a new parent animation for many anim childeren objects.

		gsl-animation-add
		-----------------
			(gsl-animation-add-frame <animation> "test-image.tga" &key :speed 0.5) 
		
			Adds a new frame to <animation> with <speed>

		gsl-animation-frame
		-------------------
			(gsl-animation-frame <animation> <n>)
			
			Will return frame <n> from <animation>

		gsl-animation-frames
		--------------------
			(gsl-animation-frames <animation>)

			Will return a list of all frames in <animation>

		gsl-anim-new
		------------
			(gsl-anim-new <animation>)

			Creates a new anim object. (used for timing etc) Uses <animation> for its parent animation.

		gsl-anim-update
		---------------
			(gsl-anim-update <anim>)
		
			This should really be called every frame, updates the anims current frame
			when the current frames run-time is up.

		gsl-anim-current
		----------------
			(gsl-anim-current-tex <anim>)

			Returns the current texture frame of <anim>

		gsl-anim-next-frame
		-------------------
			(gsl-anim-next-frame <anim>)

			Skip immediately to the next frame of <anim>

	;;;;;;;;;;;;;;;;;;;
	   gsl-with.lisp   
	;;;;;;;;;;;;;;;;;;;
		gsl-with-color
		--------------
			Use the color (r g b a) for the next commands.
				Ex:  (gsl-with-color (0.5 0.5 0.5 1.0)
					(gsl-draw-rect))
				Would draw a grey rectangle.
				Colors can be ommitted so the above could also be written as:
				     (gsl-with-color (0.5 0.5 0.5)
					(gsl-draw-rect))

		gsl-with-colormask
		------------------
			Use the color mask (r g b) for the next commands
				Ex:  (gsl-with-colormask (0 0 1)
					(gsl-with-color (1 1 1)
					   (gsl-draw-rect)))
				Would draw a blue rectangle.

		gsl-with-textures
		-----------------
			Calls (glEnable +GL-TEXTURES-2D+) at the start and (gl-disable +GL-TEXTURES-2D+) at the end.
				Ex:   (gsl-with-textures
					(gsl-draw-rect :tex <texture>))
			Withought this call textures cannot normally be accessed unless you call (gl-enable +GL-TEXTURE-2D+) or use the
			:with-texture option in gsl-draw-rects
			
		gsl-with-stencilfunc
		--------------------
			Draws the following commands with the stencil function (func ref mask)
				Ex:   (gsl-with-stencilfunc (+GL-ALWAYS+ #x01 #x01)
					(gsl-draw-rect))

		gsl-with-stencilop
		------------------
			Draws the following commands with the stencil operation (depth-fail stencil-fail all-pass)
				Ex:   (gsl-with-stencilop (+GL-KEEP+ +GL-KEEP+ +GL-REPLACE+)
					(gsl-draw-rect))

		gsl-with-blendfunc
		------------------
			Draws the following commands with the blend function (sfactor dfactor)
				Ex:   (gsl-with-blendfunc (+GL-SRC-ALPHA+ +GL-ONE-MINUS-SRC-ALPHA+)
					(gsl-draw-rect))

		gsl-with-pushmatrix
		-------------------
			Draws the following commands with a call to push and pop matrix.
				Ex:   (gsl-with-pushmatrix
					(gl-translate :x 50))

		gsl-with-translate
		------------------
			Same as gsl-with-pushmatrix exept it also translates for you
				Ex:   (gsl-with-translate (0 0 0)
					<other function calls here>)

		gsl-with-font
		-------------
			Select a non default font for use in drawing text.
				Ex:   (gsl-with-font <other-font>
					(gsl-draw-string <text> <x> <y> <w> <h>))

		gsl-with-depthtest
		------------------
			Draw the following commands with the depth test enabled.
				Ex:   (gsl-with-depthtest
					(gsl-draw-rect))

		gsl-with-lights
		---------------
			Calls (gl-enable +GL-LIGHTING+) at opening brace and (gl-disable +GL-LIGHTING+) at the closing brace
				Ex:   (gsl-with-lights
					(gl-light +GL-LIGHT0+ +GL-POSITION+ (0 0 0)))

		gsl-with-shader
		---------------
			Use the shader <shader> for the following commands,
				Ex:   (gsl-with-shader <shader>
					(gsl-draw-rect))

		gsl-with-fbo
		------------
			Draw the following commands to <fbo>,
				Ex:   (gsl-with-fbo <fbo>
					(gsl-draw-rect))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   OpenGL Functions Available:   ;;
;;   ---------------------------   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		gl-translate
		------------
			(gl-translate (&key (x 0) (y 0) (z 0) (pos nil)))
				Pos should be a list of 3 elements.
				Ex:
				      (gl-translate :pos '(0 5 15))
				
		glu-perspective
		---------------
			(glu-perspective (&key (fov 90) (aspect (width / height)) (near_clip 0.1) (far_clip 10000)))

		gl-clear
		--------
			To do multiple clears use (logior)
				Ex:
				      (gl-clear (logior +GL-COLOR-BUFFER-BIT+ +GL-DEPTH-BUFFER-BIT+))
		gl-light
		--------
			(gl-light <LIGHT> <PNAME> <PARAM>)
				Ex:
				      (gl-light +GL-LIGHT0+ +GL-AMBIENT+ (0 0 0))

		gl-material
		-----------
			(gl-material <FACE> <PNAME> <PARAM>)
				Ex:
				      (gl-material +GL-FRONT+ +GL-DIFFUES+ (1 1 1))

		gl-clear-color
		gl-begin
		gl-end
		gl-vertex
		gl-load-identity
		gl-rotate
		gl-enable
		gl-disable
		gl-push-matrix
		gl-pop-attrib
		gl-push-attrib
		gl-delete-texture
		gl-pop-matrix
		gl-color-mask
		gl-viewport
		gl-color
		gl-bind-texture
		gl-blend-func
		gl-bind-framebuffer
		gl-stencil-op
		gl-stencil-func
		gl-swap-buffers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   SDL Functions Available:   ;;
;;   ------------------------   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		sdl-delay
		sdl-get-ticks
		(most of these have been replaced by gsl functions)
