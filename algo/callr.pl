# What: Execute rscript rcmd from perl.
# GPL(C) moshahmed@gmail.com

use strict;
use warnings;
use Data::Dumper;
my $pwd = `pwd`; $pwd =~ s,/cygdrive/(\w)/,$1:/,;
my $cwd = `cygpath -wam .`;
# print $pwd; # c:/Manipal/.exams
# print $cwd; # C:/mosh3/courses/2016-Manipal/9-Exams
my $rcmd = shift || 'print(\"Rscript: need R command!\")';

my $output = `c:/math/r323/bin/Rscript.exe -e '$rcmd'`;
print $? if $?;

my @output = split( /\n/, $output);
# print @output;
my $printing; # bistable
for (@output){ s,\r,\n,g;
  chomp;
  # header/footer is printed by ~/.RProfile
  $printing=0 if m/Goodbye from R/;
  print "$_" if $printing;
  $printing=1 if m/cwd=/;
}

