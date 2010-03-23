#!/usr/bin/perl -w

sub chase {-l $_[0]?chase(readlink($_[0])):$_[0];}

$x=chase("/usr/lib/libpetsc.so");

$x=~/libpetsc.so.2.([23]).(\d+)$/ or die "Unknown Petsc Version!\n";

$ARGV[0] eq "version" and print "2.${1}.${2}";
$ARGV[0] eq "define" and print "PETSC2${1}0";
