#!/usr/bin/perl

# wrfsi.pl
#
# This script runs two other scripts which, together, read
# the input data required for a WRF model run, reformat it,
# interpolate it to the WRF grid, and write it out in WRF
# input format.
# 
# In a real-time application, in which there may be several
# WRF model runs all of which use the Eta (or AVN) grids for
# init/lbc, one would run the grib_prep processing separately
# from the interp processing, to avoid running grib_prep 
# redundantly for all those windows.

require 5;
use strict;
umask 002;
use vars qw($opt_c $opt_d $opt_e $opt_h $opt_g $opt_i $opt_q $opt_u);
use Getopt::Std;

getopts('c:d:e:hgi:q:u:');

# Print help message if -h, then exti

if ($opt_h) {
  print
"  Usage:  wrfsi.pl [options] YYYYMMDDHH FF SRC
   
      YYYYMMDDHH = Year/month/day/hour GMT of forecast start time
      FF = Length of forecast run in hours
      SRC = source to use for lateral boundaries and initialization

      OPTIONS:
 
        -c NODETYPE
           Specifies node type to use when submitting jobs using PBS/SGE
           If not set, assumed type=comp

        -d MOAD_DATAROOT
           Used to set or override MOAD_DATAROOT environment variable.
        
        -e EXT_DATAROOT
           Used when wrfsi.pl is allowed to run grib_prep

        -g 
           If set, grib_prep is not run. (Assumes you are running
           it separately and processed grib data is already available
           in EXT_DATAROOT/extprd for your source model)
 
        -h 
           Displays this help messag

        -i INSTALLROOT
	   Used to set a specific installroot to use for scripts/code
       
        -q HH:MM:SS
           Submit job using PBS/SGE qsub command with a max run time
           of hours:minutes:seconds

        -u ACCOUNT
           Used to specify project account to queuing system (e.g., at FSL)
         \n";

  exit;
}
      
     
# Get user specification of $INSTALLROOT and $MOAD_DATAROOT.
# Command line specification supersedes environment variable.

my ($INSTALLROOT, $DATAROOT);

if(defined $opt_i){
  $INSTALLROOT = $opt_i;
  $ENV{INSTALLROOT} = $INSTALLROOT;
}else{
  if ($ENV{INSTALLROOT}){
    $INSTALLROOT = $ENV{INSTALLROOT};
  }else{
    my $curdir = `pwd`; chomp $curdir;
    my $script = $0;
    if ($script =~ /^(\S{1,})\/wrfsi.pl/){
      chdir "$1/..";
    }else{
      chdir "..";
    }
    $INSTALLROOT = `pwd`; chomp $INSTALLROOT;
    $ENV{INSTALLROOT}=$INSTALLROOT;
    chdir $curdir;
  }
}

if(defined $opt_d){
  $DATAROOT = $opt_d; 
  $ENV{MOAD_DATAROOT} = $DATAROOT;
# system("setenv MOAD_DATAR00T $DATAROOT");
}else{
  if ($ENV{MOAD_DATAROOT}){
    $DATAROOT = $ENV{MOAD_DATAROOT};
  }else{
    $DATAROOT="$INSTALLROOT/data";
    $ENV{MOAD_DATAROOT} = $DATAROOT;
    print "ASSUMING dataroot = $DATAROOT\n";
  }
}

if (!defined $INSTALLROOT || !defined $DATAROOT) {
  print "\nMust have \$INSTALLROOT and \$MOAD\_DATAROOT defined.  Two options:
            1) Set environment variables
            2) Use command line switches -i and -d (before other args)
  Note that command line specifications override environment variables.\n\n";
  exit;
  }

require "$INSTALLROOT/etc/wrfsi_utils.pm";

# Get copies of the static and setup data from $INSTALLROOT, if
# that hasn't already been done.

if (-e $DATAROOT && -d _ && -w _) {
  if (!-e "$DATAROOT/cdl")    {system "cp -r $INSTALLROOT/data/cdl $DATAROOT";}
  if (!-e "$DATAROOT/static") {system "cp -r $INSTALLROOT/data/static $DATAROOT";}
  }
else {die "\$MOAD_DATAROOT $DATAROOT does not exist, is not a directory, 
           or is not writeable.\n";}

# Get command line arguments.

my ($starttime, $fcstlen, $model) = @ARGV;

# We could do a lot more checking here ... maybe later.

if (!defined $model) {die "
Need three command line arguments, for example:\n
      wrfsi.pl yyyymmddhh fcstlen src_model \n
where yyyymmddhh is the model start time,
      fcstlen is the forecast length in hours, and
      src_model one of those supported by Vtables.
      Type wrfsi.pl -h for more help.
\n"}

$model =~ tr/[a-z]/[A-Z]/;       #convert to upper case

# Set up log file
my $logfile = "$DATAROOT/log/$starttime.wrfsi";
open(LF, ">$logfile");
print LF "Running using $model\n";

# Read in the namelist info.

open(NL,"$DATAROOT/static/wrfsi.nl") 
             or die "Can't open $DATAROOT/static/wrfsi.nl\n";
my @lines = <NL>;
close(NL);
my %namehash = &wrfsi_utils::get_namelist_hash(@lines);

# Now that we are using wrfprep, we just need
# to edit the source model to use.

open(NL,">$DATAROOT/static/wrfsi.nl") or die 
         "Can't write new version of $DATAROOT/static/wrfsi.nl\n";
my $line;
foreach $line (@lines){
  if ($line =~  /^\s*(init_root)\s*=/i){
    $line = " $1 = \'$model\',\n";}
  if ($line =~  /^\s*(lbc_root)\s*=/i) {
    $line = " $1 = \'$model\',\n";}
#  if ($line =~  /^\s*(lsm_root)\s*=/i){
#    $line = " $1 = \'$model\',\n";}
  print NL "$line";
}
close(NL);

# Run grib reader/reformatter if opt_g not set.

if (! $opt_g) {
  print LF "Grib_prep run is requested (-g not set)\n";
  # Determine EXT_DATAROOT
  
  if ($opt_e) {
    $ENV{EXT_DATAROOT} = $opt_e;
  }else{
    my $extdataroot = ${namehash{LBCPATH}}[0];
    print "LBCPATH = $extdataroot\n";
    if ($extdataroot =~ /(\S{1,})\/extprd/) {
      $ENV{EXT_DATAROOT}=$1;
    }else{
      $ENV{EXT_DATAROOT}="$ENV{INSTALLROOT}/extdata";
    }
  }
  print LF "EXT_DATAROOT = $ENV{EXT_DATAROOT}\n";
  my $gpnl =  "$ENV{EXT_DATAROOT}/static/grib_prep.nl";
  if (! -f "$gpnl"){
       print LF "$gpnl not found!\n";
       close(LF);
       die;
  }

  # Set starttime and forecast length.  Since we sometimes
  # use an older cycle of the background model, forecast
  # length for the LBC needs to be longer than fcstlen
  my $lbclen = $fcstlen+12;

  # Set up grib_prep command
  my $command = "$INSTALLROOT/etc/grib_prep.pl";
  $command = "$command -s $starttime -l $lbclen";
  $command = "$command $model";

  if ($opt_q) {
    $command = "$command -q $opt_q";
    if ($opt_c){ $command = "$command -c $opt_c" }
    if ($opt_u){ $command = "$command -u $opt_u" }
    print LF "grib_prep job to be submitted using qsub:\n";
  }
  my $date = `date`; chomp $date;
  print LF "Starting grib_prep.pl at $date\n";
  print LF "Log for grib_prep in $ENV{EXT_DATAROOT}/log\n";
  print LF "Command line:  $command";
  system ("$command");
}
# Run interpolator/initializer.

my $command = "$INSTALLROOT/etc/wrfprep.pl";
$command = "$command -f $fcstlen -s $starttime";
if ($opt_q) {
  $command = "$command -q $opt_q";
  if ($opt_c){ $command = "$command -c $opt_c" }
  if ($opt_u){ $command = "$command -u $opt_u" }
  print LF "wrfprep job to be submitted using qsub:\n";
}

my $date = `date`; chomp $date;
print LF "Starting wrfprep.pl at $date\n";
print LF "Command line:  $command\n";
system ("$command");

# Finished.

my $date = `date`; chomp $date;
print LF "Exiting wrfsi.pl at $date\n";
close (LF);
exit;
