#!/usr/bin/perl
umask 000;
#dis   
#dis    Open Source License/Disclaimer, Forecast Systems Laboratory
#dis    NOAA/OAR/FSL, 325 Broadway Boulder, CO 80305
#dis    
#dis    This software is distributed under the Open Source Definition,
#dis    which may be found at http://www.opensource.org/osd.html.
#dis    
#dis    In particular, redistribution and use in source and binary forms,
#dis    with or without modification, are permitted provided that the
#dis    following conditions are met:
#dis    
#dis    - Redistributions of source code must retain this notice, this
#dis    list of conditions and the following disclaimer.
#dis    
#dis    - Redistributions in binary form must provide access to this
#dis    notice, this list of conditions and the following disclaimer, and
#dis    the underlying source code.
#dis    
#dis    - All modifications to this software must be clearly documented,
#dis    and are solely the responsibility of the agent making the
#dis    modifications.
#dis    
#dis    - If significant modifications or enhancements are made to this
#dis    software, the FSL Software Policy Manager
#dis    (softwaremgr@fsl.noaa.gov) should be notified.
#dis    
#dis    THIS SOFTWARE AND ITS DOCUMENTATION ARE IN THE PUBLIC DOMAIN
#dis    AND ARE FURNISHED "AS IS."  THE AUTHORS, THE UNITED STATES
#dis    GOVERNMENT, ITS INSTRUMENTALITIES, OFFICERS, EMPLOYEES, AND
#dis    AGENTS MAKE NO WARRANTY, EXPRESS OR IMPLIED, AS TO THE USEFULNESS
#dis    OF THE SOFTWARE AND DOCUMENTATION FOR ANY PURPOSE.  THEY ASSUME
#dis    NO RESPONSIBILITY (1) FOR THE USE OF THE SOFTWARE AND
#dis    DOCUMENTATION; OR (2) TO PROVIDE TECHNICAL SUPPORT TO USERS.
#dis   
#dis 

# Script Name:  grib_prep218.pl
#
# Purpose:  FTPs and processes the Eta 218 GRIB tiles.   
#
# Usage:
#
#      grib_prep218.pl [-i $INSTALLROOT ] -F [-d $EXT_DATAROOT] 
#                      [ -l fcstlen ]
#
#        INSTALLROOT = location of compiled wrfsi binaries and scripts
#        EXT_DATAROOT = top level directory of grib_prep output and 
#                        configuration data.
#
#        fcstlen = Number of output hours from start time to produce
#                  output
#        interval = interval between output files in hours
###############################################################################

require 5;
use strict;
use vars qw($opt_c $opt_h $opt_i $opt_d $opt_F $opt_l 
            $opt_q $opt_u);
use Getopt::Std;

print "Routine: grib_prep218.pl\n";
my $mydir = `pwd`; chomp $mydir;

getopts('c:hi:d:Fl:q:u:');

if ($opt_h){
    print "grib_prep218.pl Usage
          =================
          grib_prep218.pl [options] 

          Valid options
          -------------

          -c nodetype
             Type of compute node to use when using PBS
 
          -d EXT_DATAROOT
             Used to set/override the EXT_DATAROOT environment var.

          -F
             If set, then the script FTPs the data from NCEP based
             on EXT_DATAROOT/static/tilelist.txt.  Otherwise, it
             assumes the GRIB data is already present in
             EXT_DATAROOT/GRIB

          -h
             Prints this help menu

          -i INSTALLROOT
             Used to set/override the WRFSI INSTALLROOT environment var.

          -l Forecast length in hours
             Set the number of forecast hours the data should span.  If
             not set, this will default to 36 hours.

          -q hh:mm:ss 
             Submit job using PBS qsub routine with this maximum wall time

          -u Set a user ID for PBS (qsub) use \n";
exit;
}

          
# Get user specified installroot and dataroot.  If they are not
# present, then use environment variables.

my ($installroot, $dataroot);
if (! defined $opt_i){
  if (! $ENV{INSTALLROOT}){
    # Get installroot from script name
    my $scriptname = $0;
    my $curdir = `pwd`; chomp $curdir;
    if ($scriptname =~ /^(\S{1,})\/grib_prep218.pl/){
      chdir "$1/../";
    }else{
      chdir "..";
    }
     $installroot = `pwd`; chomp $installroot;
     chdir "$curdir";
     $ENV{INSTALLROOT} = $installroot;
  }else{
    $installroot = $ENV{INSTALLROOT};
  }
}else{
  $installroot = $opt_i;
}
if (! defined $opt_d){
  if (! $ENV{EXT_DATAROOT}){
    $dataroot = "$installroot/extdata";
    $ENV{EXT_DATAROOT}="$dataroot";
  }else{
    $dataroot = $ENV{EXT_DATAROOT};
  }
}else{
  $dataroot = $opt_d;
  $ENV{EXT_DATAROOT} = $dataroot;
}

require "$installroot/etc/wrfsi_utils.pm";

my $source = "ETA218";
my $sourcel = "ETAL218";
my  $workdir = "$dataroot/work/$source";
if (! -d "$workdir" ) { system("mkdir -p $workdir") }
chdir "$workdir";
# Set up some variables to hold directory names, namelist file names,
# etc. for convenience
my $runtime = `date -u +%H%M`; chomp $runtime;
my $nlfilename = "$dataroot/static/tilelist.txt";
my $workdir = "$dataroot/work/$source";
my $gribprepexe = "$installroot/bin/grib_prep_etatiles.exe";
my $logfile = "$dataroot/log/gp_$source.$runtime.log";
my $logfileexe = "$logfile.exe";
print "WORKDIR = $workdir \n";
# Make sure executable is present.

if (! -f "$gribprepexe"){
  die "$gribprepexe does not exist.  Make sure your INSTALLROOT is set
correctly and you have compiled everything.\n";
}

# Make sure dataroot exists.

if (! -d "$dataroot"){
  die "Your specified EXT_DATAROOT does not exist! \n";
}


# Is the namelist present?

if (! -f "$nlfilename"){
  die "The namelist: $nlfilename is not found! \n";
}

# OK, if we made it this far we are ready to start.  

# Determine starting time string. It is either defined on the command
# line (opt_s) or we use the real-time system clock along with some
# of the namelist entries.
my $interval = 10800;
my $interval_hr = 3;
my $timenow = `date -u +%Y%m%d%H`;
my $startdate = $timenow;
my $waittime = 2; 
my $freq = 6;
my ($startyear, $startmonth, $startday, $starthour,
    $enddate, $endyear, $endmonth, $endday, $endhour);
# If we used the system clock, we need to adjust by
# the cycle delay for this model source.  Otherwise,
# we may or may not need to adjust.

my $latest_avail = &wrfsi_utils::compute_time($timenow,"-$waittime");
if ($startdate gt $latest_avail) {
    $startdate = $latest_avail;
}
  

# Parse out hour and change to nearest cycle time
if ($startdate =~ /^(\d\d\d\d)(\d\d)(\d\d)(\d\d)$/){
  $startyear = $1;
  $startmonth = $2;
  $startday = $3;
  $starthour = $4;
}else{
    print "Unrecognized date format. Should be YYYYMMDDHH. \n";
    exit;
}
$starthour = int($starthour/$freq)*$freq;
$starthour = "0".$starthour while(length($starthour)<2);
$startdate = $startyear.$startmonth.$startday.$starthour;

# Get ending time.

my $fcstlen;
if (defined $opt_l){
  $fcstlen = $opt_l;
}else{
  $fcstlen = 36;
}
$enddate = &wrfsi_utils::compute_time($startdate, $fcstlen);
if ($enddate =~ /^(\d\d\d\d)(\d\d)(\d\d)(\d\d)$/){
  $endyear = $1;
  $endmonth = $2;
  $endday = $3;
  $endhour = $4;
}else{
    print "Unrecognized date format. Should be YYYYMMDDHH. \n";
    exit;
}

open (LOG, ">$logfile");

# If requested, FTP the data from NCEP
my $command;
my $gribdir = "$dataroot/GRIB";
if ($opt_F) {
  if (! -d "$gribdir") { mkdir "$gribdir", 0777 }

  # Clean gribdir
  system("rm -f $gribdir/eta.t??z.awip*.* $gribdir/tile.list.*");
  print LOG "Obtaining data from NCEP\n";
  $command = "$installroot/etc/ftp_eta_tiles.csh";
  $command = "$command $starthour $fcstlen $interval_hr tile218";
  $command = "$command $dataroot";
  print LOG "Running $command\n";
  close LOG;
  system("$command >> $logfile 2>&1");
  open(LOG,">>$logfile");
  print LOG "Completed script to ftp data.\n";
}

# Run the executable
my ($command, $maxruntime, $qsubsec, $script);
$script = "$installroot/etc/process_eta218_tiles.csh $starthour $fcstlen $interval_hr tile218 $dataroot";
if (! $opt_q){
  $command = "$script";
}else{
  my $nodetype;
  if ($opt_c){
    $nodetype=$opt_c;
  }else{
    $nodetype="comp";
  }
  # Compute the amount of run time in seconds by parsing string
  $qsubsec = &wrfsi_utils::qsub_hms2sec($opt_q);
  print LOG "Max job time in seconds = $qsubsec\n";
  open(QS,">$workdir/qsub_grib_prep218.ksh");
  print QS "#!/bin/ksh\n";
  print QS "#PBS -l walltime=$opt_q,nodes=1:$nodetype\n";
# 
# SGE syntax for ijet/jet at FSL
  print QS "#\$ -S /bin/ksh\n";
  print QS "#\$ -pe $nodetype 1\n";
  print QS "#\$ -l h_rt=$opt_q\n";

  print QS "cd $workdir\n";
  print QS "$script > $logfileexe 2>&1\n";
  print QS "exit\n";
  close(QS);
  chmod 0777, "qsub_grib_prep218.ksh";
}

my $started = `date -u`;
print LOG "Starting $command at $started \n";
my $status;
if ($opt_q){
  my ($jobid,$jobserver);
  my $qcommand;
  if ($opt_u){
    $qcommand = "/bin/qsub -A $opt_u -V -N wrfsi_gp qsub_grib_prep218.ksh";
  }else{
    $qcommand = "/bin/qsub -V -N wrfsi_gp qsub_grib_prep218.ksh";
  }
  my $status = system "$qcommand > jobfile.txt";
  open(JF,"jobfile.txt");
  my @joblines = <JF>;
  close(JF);
  #unlink "jobfile.txt";
  foreach (@joblines){
    if (/(\d{1,})/) {
      $jobid = $1;
      #$jobserver=$2;
    }
  }
  print LOG "Job ID # = $jobid.$jobserver\n";
  my $stdout = "$workdir/wrfsi_gp.o$jobid";
  print LOG "Std output will be in $stdout\n";
  if (-f "/bin/wait_job"){
    my $qsubwait = $qsubsec + 120;  # Allows for 2 minutes in queue
    print LOG "Using wait_job $jobid $qsubwait -v\n";
    system("/bin/wait_job $jobid $qsubwait -v >> $logfile");
  }else{
    my $jobcheck = `/bin/qstat | grep $jobid`;
    while ( ($jobcheck) and (! -f "$stdout" ) ){
       sleep 5;
       $jobcheck = `/bin/qstat | grep $jobid`;
    }
  }
  unlink "qsub_grib_prep218.ksh";
}else{
 close (LOG);
 $status = system("$command >> $logfile 2>&1");
 open(LOG, ">>$logfile");
}
my $finished = `date -u`;
print LOG "Termination of grib_prep218 at $finished.\n";

# Clean up work space
opendir(WORK,$workdir);
foreach (readdir WORK) {
  if (/wrfsi_gp\.\w\d*/) {
    print LOG "\n";
    print LOG "============================================\n";
    print LOG "$_\n";
    print LOG "============================================\n";
    my $line;
    open(LF,"$_");
    foreach $line (readline *LF) {
      print LOG "$line";
    }
    close LF;
    print LOG "\n";
    unlink "$workdir/$_";}
  }
closedir(WORK);  

if ( -f "$logfileexe" ) {
  print LOG "\n";
  print LOG "============================================\n";
  print LOG "Log from program execution\n";
  print LOG "============================================\n"; 
  open (LF, "$logfileexe");
  foreach (readline *LF) {
    print LOG "$_";
  }
  close LF;
  unlink "$logfileexe";
  chmod 0666, "$logfile";
}
close LOG;
chmod 0666, "$logfile";

# Clean up old files (this should be done in a purger script eventually).

my $wrfsi_start = $startyear."-".$startmonth."-".$startday."_".$starthour;
my $wrfsi_end = $endyear."-".$endmonth."-".$endday."_".$endhour;
opendir (DATACOM, "$dataroot/extprd");
foreach (readdir DATACOM) {
  if ((/^$source\D(\d\d\d\d\D\d\d\D\d\d\D\d\d)/i) or
      (/^$sourcel\D(\d\d\d\d\D\d\d\D\d\d\D\d\d)/i)) {
    if ( $1 lt $wrfsi_start ){
      unlink "$dataroot/extprd/$_";
      print "Purging $dataroot/extprd/$_\n";  
    }
  }
}
closedir(DATACOM);

exit;


