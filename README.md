# gamesavvylisp
Game Development Library for Clozure Common Lisp

* This repo has been outdated for several years now.
* This project was based on [GLEE](http://elf-stone.com/downloads.php), the OpenGL toolkit.

A lisp interface to GLEE and OpenGL was automatically generated and then tweaked by hand.
This project showcased several demos, including
  # Animation
  # GUI Console
  # Motion blur
  # Stencil buffer
  # Fragment shaders

I moved away from Lisp as a game development tool, due to it's high memory and CPU requirements.
However, due to advances in technology, this may now be a feasible choice. It all depends on
what kind of game you're hoping to create.

A great advantage of writing code in Lisp was that I was able to precompile C and C++ code,
which could be then linked into the running lisp program.

This isn't a very lisp way of doing things, but when interfacing directly with the graphics memory
an interface like this was the best decision at that time.

Take a look at [gamesavvycplusplus](https://github.com/tavurth/gamesavvycplusplus), another library built at around the same time as this.
