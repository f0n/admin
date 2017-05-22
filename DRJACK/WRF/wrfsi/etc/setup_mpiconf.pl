#!/usr/bin/perl

use Getopt::Std;
getopts ("f:");

$h = $ENV{"HOME"};

if (defined($opt_f)) {
    $file = $opt_f;
} else {
    $file = "$h/.gmpi/conf";
}

open (OUT, ">$file") || die "can't open $file";
@portlist = ("2","4","5","6","7");

%nodelist=();
# First build the list

while (<>) {
    chomp;
    next if (/^#/);
    if (!defined($nodelist{$_}))  {
      $nodelist{$_}=0;
    } else {
      $nodelist{$_}++;
    }
    $p=$portlist[$nodelist{$_}];
         
    push (@m, "$_ $p");

}

print OUT $#m+1, "\n";
while ($mach = shift (@m)) {
    print OUT "$mach\n";
}

close (OUT) || die "can't close $file";