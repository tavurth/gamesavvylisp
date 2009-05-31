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

# 	This script is used to parse through #define variables turning them into (const ++ variables;
# 	Usage:
#		Place your variables to be converted into a file (eg. convert.txt)
#		Run this script on the file
#		Fix any small indenting errors

die "Usage: $0 <file-name>\n" if (!@ARGV);

open (FILE, $ARGV[0]);
my @lines = <FILE>;
my $outfile = ">$ARGV[0]";
open (FILE, $outfile);

for $line (@lines) {
	$line =~ s/#define/(const/;
	$line =~ s/(\w+\s+)(\w+)(.+)/$1\+$2\+$3/ unless ($line =~ m/\+/);
	$line =~ s/(0x)(.+)/\#x$2)/;
	$line =~ s/_/-/;
	print "Here: $2";
	print FILE "$line";
	print $line;
}
