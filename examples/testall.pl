#! /usr/bin/perl
###		Copyright (c) William Whitty 2009
###
###	This file is part of GSL. 
###
###	GSL is free software: you can redistribute it and/or modify
###     it under the terms of the GNU General Public License as published by
###     the Free Software Foundation, either version 3 of the License, or
###     (at your option) any later version.
###
###     GSL is distributed in the hope that it will be useful,
###     but WITHOUT ANY WARRANTY; without even the implied warranty of
###     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
###     GNU General Public License for more details.
###
###     You should have received a copy of the GNU General Public License
###     along with GSL.  If not, see <http://www.gnu.org/licenses/>.

sub test_example {
	my $dir = shift;

	chdir($dir);
	return 1 if system("ccl -l load-me-with-ccl.lisp");
	chdir("..");
}

sub test_examples {
	my @list = @_;

	foreach (@list) {
		print "Testing --==>> $_\n";
		return 1 if not test_example($_);
	}
}

test_examples("Basic", "Basic_input", "Event_callbacks", "Animation", "Motion_blur", "OpenGL_Lights", "Stencil_buffer", "Shaders");
