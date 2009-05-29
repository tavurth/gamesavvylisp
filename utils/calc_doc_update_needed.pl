#! /usr/bin/perl

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

open(DOC_FILE, "README_GSL") or die "Could not open README_GSL.txt for documentation";
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
