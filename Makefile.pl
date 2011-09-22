#! /usr/bin/perl

#		Copyright (c) William Whitty 2009
#
#        This file is part of GSL. 
#    	 GSL is free software: you can redistribute it and/or modify
#	 it under the terms of the GNU Lesser General Public License as published by
#	 the Free Software Foundation, either version 3 of the License, or
#	 (at your option) any later version.
#	 
#	 GSL is distributed in the hope that it will be useful,
#	 but WITHOUT ANY WARRANTY; without even the implied warranty of
#	 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#	 GNU Lesser General Public License for more details.
#	 
#	 You should have received a copy of the GNU Lesser General Public License
#	 along with GSL.  If not, see <http://www.gnu.org/licenses/>.

$LIBS = "-shared -lSDL -lGL -lGLU -O3 -Wall";
$LOCATION = "clibs";

#if we cannot find libSDL we quit
if (!`locate SDL`) { print "Cannot continue making without SDL libraries, please install SDL. (www.libSDL.org)"; exit(1); }

sub compileFile  {
# Compiling our C libraries
	my ($loc, $compiled, $libs) = @_;
	if (!$libs) { $libs = $LIBS; }

	#If the compiled file does not exist, or the .c file is newer, compile:
	if (!(-e "$LOCATION/$compiled") or (-M "$LOCATION/$loc" <= -M "$LOCATION/$compiled")) {
		print "gcc $LOCATION/$loc $libs -o $LOCATION/$compiled\n";
		`gcc $LOCATION/$loc $libs -o $LOCATION/$compiled`;
	}

	else { print "$loc is up to date\n"; }
}

#Running through our list of binary files to be compiled
compileFile("GLee.c", "GLee.o", "-c -O3");
compileFile("gsl-bindings.c", "gsl-bindings.so", "$LOCATION/GLee.o " . "$LIBS");
compileFile("gsl-gl-bindings.c", "gsl-gl-bindings.so", "$LOCATION/GLee.o " . "$LIBS");
compileFile("gsl-sdl-bindings.c", "gsl-sdl-bindings.so", "$LOCATION/GLee.o " . "$LIBS");
