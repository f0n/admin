#!/usr/bin/perl
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

# Script Name:  run_wrf.pl
#
# Purpose:  This script runs the WRF model from a pregenerated
#           set of wrfinput/wrfbdy/CYCLE.* files in the siprd
#           directory (i.e., it assumes you have just run
#           wrfprep.pl for your domain.
#
# Usage:
#   
#   run_wrf.pl -h to see options
#
#        

require 5;
use strict;
use Time::Local;
use vars qw($opt_c $opt_d $opt_h $opt_i $opt_M $opt_p $opt_q $opt_u);
use Getopt::Std;
print "Routine: run_wrf.pl\n";
my $mydir = `pwd`; chomp $mydir;

# Get command line options
getopts('c:d:hi:M:p:q:u:');

# Did the user ask for help?
if ($opt_h){
  print "Usage:  run_wrf.pl [options]

          Valid Options:
          ===============================================================
          -c NODETYPE
             Sets the type of node to use when submitting jobs via the
             queuing system (-q set).  If not set, we assume type=comp

          -d MOAD_DATAROOT
             Sets or overrides the MOAD_DATAROOT environment variable.
             If no environment variable is set, this must be provided.

          -h 
             Prints this message

          -i INSTALLROOT 
             Sets/overrides the INSTALLROOT environment variable.

          -M mpioptions
             Used to set additional options to be passed to mpirun.

          -p NPROC
             Number of processors to use if mpi job.

          -q HH:MM:SS
             Use the PBS queuing system to run the job.  Requires
             that max run time in hh:mm:ss be set.
  
          -u ACCOUNT
             Provides the account name to PBS/SGE (e.g., at FSL)

           \n"; 
  exit;
}

# Set up run-time environment

my $runtime = time;
my ($installroot, $moad_dataroot);

# Determine the installroot.  Use the -i flag as first option,
# followed by INSTALLROOT environment variable, followed by
# current working directory with ../. appended.

if (! defined $opt_i){
  if (! $ENV{INSTALLROOT}){
    print "No INSTALLROOT environment variable set! \n";
    print "Attempting to use the current diretory to set installroot.\n";
    my $script = $0;
    my $curdir = `pwd`; chomp $curdir;
    if ($script =~ /^(\S{1.})\/run_wrf.pl$/){
      chdir "$1/..";
    }else{
      chdir "..";
    }
    $installroot = `pwd`; chomp $installroot;
    chdir "$curdir";
    if (! -e "$installroot/bin/hinterp.exe") {
      die "Cannot determine installroot\n";
    }else{
      $ENV{INSTALLROOT} = $installroot;
    }
  }else{
    $installroot = $ENV{INSTALLROOT};
  }
}else{
  $installroot = $opt_i;
  $ENV{INSTALLROOT}=$installroot;
}

# Look for the  critical executable and other files.
my $wrfparamdir = "$installroot/../run";
my $wrfexe = "$installroot/../run/wrf.exe";
if (! -e "$wrfexe") { die "$wrfexe not found.\n" }

print "INSTALLROOT = $installroot\n";
require "$installroot/etc/wrfsi_utils.pm";

# Process MOAD_DATAROOT.  Use -d argument first, followed by 
# environment variable.

if (! defined $opt_d){
  if (! $ENV{MOAD_DATAROOT}){
    die "No MOAD_DATAROOT environment variable set! \n";
  }else{
    $moad_dataroot = $ENV{MOAD_DATAROOT};
  }
}else{
  $moad_dataroot = $opt_d;
  $ENV{MOAD_DATAROOT} = $moad_dataroot;
}
# Check for a couple of critical files in moad_dataroot
if (! -e "$moad_dataroot/siprd/namelist.input"){
  die "$moad_dataroot/siprd/namelist.input not found.  Run wrfprep first.\n";
}
if (! -e "$moad_dataroot/siprd/wrfinput_d01"){
  die "$moad_dataroot/siprd/wrfinput_d01 not found. Run wrfprep.pl first.\n";
}

if (! -e "$moad_dataroot/siprd/wrfbdy_d01"){
  die "$moad_dataroot/siprd/wrfbdy_d01 not found. Run wrfprep.pl first.\n";
}

print "MOAD_DATAROOT = $moad_dataroot\n";

# Get the cycle file       
opendir (SID,"$moad_dataroot/siprd");
my ($cycle, $fcstlen);
foreach (readdir SID){
  if (/CYCLE.(\d\d\d\d\d\d\d\d\d)(\d\d\d\d)/){
    $cycle = $1;
    $fcstlen = $2;
  }
}
closedir (SID);
if (! $cycle){
  die "No CYCLE file found.\n";
}

# Start a log for this cycle
my $wrflog = "$moad_dataroot/log/$cycle.wrf";
open (LOG, ">$wrflog");
my $timenow = `date -u`; chomp $timenow;
print LOG "WRF Log for cycle $cycle opened on $timenow.\n";

my $workdir = "$moad_dataroot/wrfprd";
if (! -d "$workdir") { 
  mkdir "$workdir", 0777;
  mkdir "$workdir/d01", 0777;
  mkdir "$workdir/d01/awips", 0777;
  mkdir "$workdir/d01/fua", 0777;
  mkdir "$workdir/d01/fsf", 0777;
  mkdir "$workdir/d01/grib", 0777;
  mkdir "$workdir/d01/points", 0777;
  mkdir "$workdir/d01/v5d", 0777;
}else{
  # Clean the work directory
  opendir (WORK, "$workdir");
  foreach (readdir WORK){
    if (-e "$workdir/$_"){ unlink "$workdir/$_";}
  }
  if (! -d "$workdir/d01", 0777) { mkdir "$workdir/d01", 0777;}
  if (! -d  "$workdir/d01/awips") { mkdir "$workdir/d01/awips", 0777}
  if (! -d "$workdir/d01/fua") { mkdir "$workdir/d01/fua", 0777 }
  if (! -d "$workdir/d01/fsf") { mkdir "$workdir/d01/fsf", 0777}
  if (! -d "$workdir/d01/grib") { mkdir "$workdir/d01/grib", 0777}
  if (! -d "$workdir/d01/points") { mkdir "$workdir/d01/points", 0777}
  if (! -d "$workdir/d01/v5d") { mkdir "$workdir/d01/v5d", 0777}

  closedir (WORK);
}
chdir "$workdir";

# Copy/link files into the work directory
chdir "$workdir";
system ("cp $moad_dataroot/siprd/wrfinput_d?? .");
system ("cp $moad_dataroot/siprd/wrfbdy_d?? .");
system ("cp $moad_dataroot/siprd/namelist.input .");
system ("cp $moad_dataroot/siprd/CYCLE.????????????? .");

opendir(WPDIR, "$wrfparamdir");
foreach (readdir WPDIR){
  my $parmfile = "$wrfparamdir/$_";
  if (-f $parmfile){
    if ( ($parmfile !~ /.exe/) and ($parmfile !~ /namelist/)){
      symlink $parmfile, "$workdir/$_";
    }
  }
}
closedir(WPDIR);

# Set up executable command
my ($runcommand, $nproc);
if ($opt_p) {
  if ($opt_M){
    $runcommand = "/usr/bin/mpirun -np $opt_p $opt_M $wrfexe";
  }else{
    $runcommand = "/usr/bin/mpirun -np $opt_p $wrfexe";
  }
  $nproc = $opt_p;
  if (-f "$moad_dataroot/static/mpi_machines.conf"){
    $ENV{MACHINE_FILE} = "$moad_dataroot/static/mpi_machines.conf";
    $ENV{GMPICONF} =  $ENV{MACHINE_FILE};
  }else{
    if ((! $ENV{MACHINE_FILE})and(! $ENV{GMPICONF})and(! $opt_q)){
      print "You have requested a multi-processor MPI run with -p $opt_p\n";
      print "But...no machines file seems to be present.  I checked:\n";
      print "GMPICONF environment variable, MACHINE_FILE environment variable, \
n";
      print "and $moad_dataroot/static/mpi_machines.conf\n";
      print "So, if things go awry, this may be why!\n";
    }
  }

}else{
  $runcommand = "$wrfexe";
  $nproc = 1;
}

# Process PBS arguments if opt_q set
my ($walltime,$jobid,$pbsserver,$wrflogpbs);
my $wrflogpbs = "$wrflog.pbs";
if ($opt_q) {
  my $pbsscript = "$workdir/qsub_wrf.sh";

  # Get the user options for the PBS job submission

  my ($nodetype,$walltime);
  if ($opt_c){
    $nodetype = $opt_c;
  }else{
    $nodetype = "comp";
  }
  $walltime = $opt_q;
  my $pbscfg = "#PBS -lnodes=$nproc:$nodetype,walltime=$walltime";

  # Account for setup-mpi script on ijet
  my $setupmpi;
  if ( -e "/usr/local/bin/setup-mpi.sh"){
    $setupmpi = "/usr/local/bin/setup-mpi.sh";
  }else{
    $setupmpi = "$installroot/etc/setup-mpi.sh";
  }
  print LOG "Creating PBS script: $pbsscript\n"; 
  open (PBS, ">$pbsscript");
  print PBS "#!/bin/sh\n";
  print PBS "$pbscfg\n";

  # SGE directives on jet
  print PBS "#\$ -S /bin/ksh\n";
  print PBS "#\$ -pe $nodetype $nproc\n";
  print PBS "#\$ -l h_rt=$walltime\n";
  print PBS "#\n";
  if ($opt_p){
    print PBS ". $setupmpi\n";
  } 
  print PBS "cd $workdir\n";
  print PBS "echo \$PBS_JOBID > $wrflogpbs\n";
  print PBS "echo \$PBS_NODEFILE >> $wrflogpbs\n";
  print PBS "$runcommand >> $wrflogpbs 2>&1\n";
  if ($opt_p){
    print PBS "rm -f \$GMPICONF\n";
  }
  print PBS "exit\n";
  close (PBS);
  chmod 0777, "$pbsscript";
  my $command;
  if ($opt_u){
    $command = "/bin/qsub -V -Nwrf -A $opt_u $pbsscript"; 
  }else{
    $command = "/bin/qsub -V -Nwrf $pbsscript";
  }
  system ("$command > qsub.out");

  # Now, we need to wait until the job is complete before moving on.
  # On jet at FSL, we can use the wait_job script.  On other systems
  # with PBS, we will use qstat if wait job is not available.  In either
  # even, we need to get the job number.
  open (JF, "qsub.out");
  my @lines = <JF>;
  close (JF);
  foreach (@lines){
   if (/(\d{1,})/) {
     $jobid = $1;
     #$pbsserver = $2;
    }
  }
  if (! $jobid){
    print LOG "Problem with job submission...here is output:\n";
    print LOG "@lines\n";
    $timenow = `date -u`; chomp $timenow;
    print LOG "Died at $timenow\n";
    close (LOG);
    die;
  }
  if (-e "/bin/wait_job"){
    my $qsubwait = &wrfsi_utils::qsub_hms2sec($walltime) + 300;
    print LOG "Using wait_job $jobid $qsubwait\n";
    system("/bin/wait_job $jobid $qsubwait");
  }else{
    # Go into a loop using qstat and grep to
    # check if job is running
    my $stdout = "$workdir/wrf.o$jobid";
    my $jobcheck = `/bin/qstat | grep $jobid`;
    while ( ($jobcheck) or (! -e "$stdout") ){
       sleep 10;
       $jobcheck = `/bin/qstat | grep $jobid`;
    }
  }
  $timenow = `date -u`; chomp $timenow;
  print LOG "WRF complete at $timenow\n";
  
  # Check to see if we got output from all appropriate stages...

}else{
  # No PBS...run on this node

  $timenow = `date -u`; chomp $timenow;
  system("$runcommand> $wrflog");
  $timenow = `date -u`; chomp $timenow;
  print LOG "$timenow : WRF finished.\n";

}

$timenow = `date -u`; chomp $timenow;
print LOG "$timenow : wrfprep.pl ended normally.\n";
close (LOG);

exit;
