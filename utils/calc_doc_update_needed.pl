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

#! /usr/bin/perl

#	This script is for calculating the functions that still need to be documented.
#	It reads from gsl-packages.lisp and identifies any functions that need to be
#	documented.
#	It then stores the output in needs_documenting.txt
#
#	Usage ./calc_doc_update_needed.pl <location of gsl-packages.lisp>

die "Usage: $0 <gsl-packages.lisp>\n" if not @ARGV;

open(IN_FILE, shift @ARGV) or die "$!";
my @lines = <IN_FILE>;
close IN_FILE;
my @functionList;

foreach (@lines) {
	if (/(\:)(\w+-\w+-\w+)/) {
		next if (/\:use/);
		next if (/defpackage/);
		push(@functionList, $2);
	}	
}

open(DOC_FILE, "README_GSL.txt") or die "Could not open README_GSL.txt for documentation";
my @DOC_lines = <DOC_FILE>;
close DOC_FILE;

sub recursiveFind ($) {
	my $line = shift;

	for (my $a=0; $a < @functionList; $a++) {
		if ($line =~ @functionList[$a]) {
			splice(@functionList, $a, 1);
			recursiveFind();
		}
	}
}

recursiveFind($_) foreach (@DOC_lines);
open(DOC_OUT, ">needs_documenting.txt") or die "Could not open needs_documenting.txt\n";
foreach (@functionList) {
	print DOC_OUT "$_\n";
}
