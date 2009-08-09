###		Copyright (c) William Whitty 2009
###
###	This file is part of GSL. 
###
###	GSL is free software: you can redistribute it and/or modify
###     it under the terms of the GNU Lesser General Public License as published by
###     the Free Software Foundation, either version 3 of the License, or
###     (at your option) any later version.
###
###     GSL is distributed in the hope that it will be useful,
###     but WITHOUT ANY WARRANTY; without even the implied warranty of
###     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
###     GNU Lesser General Public License for more details.
###
###     You should have received a copy of the GNU Lesser General Public License
###     along with GSL.  If not, see <http://www.gnu.org/licenses/>.

#! /usr/bin/perl

use strict;
use warnings;

my $updatedFile = shift(@ARGV);
my $lineNumber  = shift(@ARGV)-1;
my $updatefile  = shift(@ARGV);

open(FILE, $updatedFile) or die "Could not open file $!";
my @lines = <FILE>;

sub findStart ($) {
	my $searchStart = shift;

	for (my $a=$searchStart; $a > 0; $a--) {
		return $a if ($lines[$a] =~ m/defmacro/ || $lines[$a] =~ m/defun/);
	}

	for (my $a=$searchStart; $a < @lines; $a++) {
		return $a if ($lines[$a] =~ m/defmacro/ || $lines[$a] =~ m/defun/);
	}
}

sub findEnd ($) {
	my $funcStart = shift;
	my $bracketCount = 0;

	for (my $a=$funcStart; $a < @lines; $a++) {
		my $line = $lines[$a];

		for (my $b=0; $b < length($line); $b++) {
			my $char = substr($line, $b, 1);

			if ($char eq '(') 	{ $bracketCount++; }
			elsif ($char eq ')')	{ $bracketCount--; }
		}
		return $a if (!$bracketCount);
	}
}

sub buildFunc ($$) {
	my ($start, $end) = @_;
	my $func = "";

	for (my $a=$start; $a <= $end; $a++) {
		$func .= $lines[$a]; 
	}
	return $func;
}

my $funcStart = findStart($lineNumber);
my $funcEnd   = findEnd($funcStart);
my $func      = buildFunc($funcStart, $funcEnd);

open(FILE, ">".$updatefile) or die "Could not open $! for write";
print FILE "$func\n";
