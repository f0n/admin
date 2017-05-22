#!/usr/bin/perl -w -T

### GET RASP SKEWT FOR SPECIFIED IMAGE LOCATION
### for current day only (since uses wrfout file for data)
### eg call ala http://www.drjack.info/cgi-bin/get_rasp_skewt.cgi?&region=PANOCHE&grid=d2&day=0&i=559&k=70&width=585&height=585

  #untaint - UNTAINT PATH
  $ENV{'PATH'} = '/bin:/usr/bin';

################################################################################

### MODIFIED FROM get_rasp_blipspot.cgi

use CGI::Carp qw(fatalsToBrowser);

  my $PROGRAMNAME = 'get_rasp_skewt.cgi';

  ### SET DIRECTORIES
  ### DETERMINE SCRIPT DIRECTORY - this should be automatic but can over-ride here if necessary
  ### require latlon<->ij conversion scripts to be in current directory
  if( $0 =~ m|^/| ) { ( $SCRIPTDIR = "${0}" ) =~ s|/[^/]*$|| ; }
  else              { ( $SCRIPTDIR = "$ENV{'PWD'}/${0}" ) =~ s|[\./]*/[^/]*$|| ; }
  #unused ( $BASEDIR = $SCRIPTDIR ) =~ s|/[R]*ASP/UTIL[/]*||i ;
  #untaint
  if ( defined $SCRIPTDIR && $SCRIPTDIR =~ m|^([A-Za-z0-9/][A-Za-z0-9_.:/-]*)$| ) { $SCRIPTDIR = $1 ; }  # filename chars only
  ### SET SITE DEPENDENT VARIABLES (use separate file to avoid tar update overwrite)
  #untaint - untainting put into routines in this file
  require "$SCRIPTDIR/sub.rasp_site_parameters.PL" ;
  ### SET SITE DEPENDENT DIRECTORIES
  ### apache popup skewt run on SM => $ENV{PWD}=blank $0=/var/www/cgi-bin/get_rasp_skewt.cgi
  ### apache popup skewt run on SM => before &site_directories, $ENV{HOME}=(undefined)  $BASEDIR=(undefined)        $SCRIPTDIR=/var/www/cgi-bin
  ### apache popup skewt run on SM => after  &site_directories, $ENV{HOME}=/home/admin  $BASEDIR=/home/admin/DRJACK $SCRIPTDIR=/var/www/cgi-bin 
  #4testprint: `echo "PWD= $ENV{'PWD'} & 0= $0 <<" >> /var/www/cgi-bin/LOG/get_rasp_skewt.test_prints`;
  #untaint - untainting put into this routine
  &site_directories ;

  ### SET EXTERNAL SCRIPT WHICH PLOTS WRF OUTPUT FILE DATA
  $PLOTSCRIPT = "$BASEDIR/RASP/UTIL/plot.wrfoutfile.PL";

  ### SET PARAMETER FOR XI TESTS
  $LTEST = 0;
  #4XItest: $LTEST = 1;

   if ( $LTEST == 1 )
   {
    ###### INITIALIZATION FOR XI TESTS
    ###### PANOCHE CASE
      $REGION = 'PANOCHE';
      ### ARTIFICIAL INPUT
      $GRID = 'd2' ;
      ### SET DAY=-1 TO GET SOUNDINGS FROM 24HR BEFORE
      $DAY = 0;
      $TIME = 1300 ;
      ### TEST IMAGE
      $iimage = 501. ;
      $kimage =  301. ;
      $imagewidth = $imageheight = 1001;
  }
  else
  {
    ### PARSE CGI INPUT
    use CGI qw(:standard);
    $query = new CGI;
    $REGION = $query->param('region');
    $GRID = $query->param('grid');
    $DAY = $query->param('day');
    $TIME = $query->param('time');
    $iimage = $query->param('i');
    $kimage = $query->param('k');
    $imagewidth = $query->param('width');
    $imageheight = $query->param('height');
    #untaint - untaint input arguments
    if ( defined $REGION && $REGION =~ m|^([A-Za-z0-9][A-Za-z0-9_.-]*)$| ) { $REGION = $1 ; } # alphanumeric+
    if ( defined $GRID && $GRID =~ m|^([A-Za-z0-9][A-Za-z0-9_.-]*)$| ) { $GRID = $1 ; } # filename chars only
    if ( defined $DAY && $DAY =~ m|^([0-9+-][0-9]*)$| ) { $DAY = $1 ; } # integer only
    if ( defined $TIME && $TIME =~ m|^([A-Za-z0-9][A-Za-z0-9_.-]*)$| ) { $TIME = $1 ; } # alphanumeric+
    if ( defined $iimage && $iimage =~ m|^([0-9+-][0-9]*)$| ) { $iimage = $1 ; } # integer only
    if ( defined $kimage && $kimage =~ m|^([0-9+-][0-9]*)$| ) { $kimage = $1 ; } # integer only
    if ( defined $imagewidth && $imagewidth =~ m|^([0-9+-][0-9]*)$| ) { $imagewidth = $1 ; } # integer only
    if ( defined $imageheight && $imageheight =~ m|^([0-9+-][0-9]*)$| ) { $imageheight = $1 ; } # integer only
  }

  #4testprint: `echo "ARGS: $REGION & $GRID & $DAY & $iimage & $kimage & $imagewidth & $imageheight " >> /var/www/cgi-bin/LOG/get_rasp_skewt.test_prints`;

  ### TEST FOR CURRENT DAY
  if ( ! defined $DAY || ( $DAY != 0 && $DAY != -1 ) ) { die "$PROGRAMNAME ERROR EXIT: DAY value missing or non-zero"; }

  ### TEST FOR MISSING ARGUMENTS
  if ( ! defined $GRID || $GRID eq '' ) { die "$PROGRAMNAME ERROR EXIT: missing GRID argument"; }
  if ( ! defined $iimage || $iimage eq '' ) { die "$PROGRAMNAME ERROR EXIT: missing iimage argument"; }
  if ( ! defined $kimage || $kimage eq '' ) { die "$PROGRAMNAME ERROR EXIT: missing kimage argument"; }
  if ( ! defined $imagewidth || $imagewidth eq '' ) { die "$PROGRAMNAME ERROR EXIT: missing imagewidth argument"; }
  if ( ! defined $imageheight || $imageheight eq '' ) { die "$PROGRAMNAME ERROR EXIT: missing imageheight argument"; }

########################################

  ### GET REGION AND GRID DEPENDENT GRID AND IMAGE PARAMETERS
  ( $grid_imin,$grid_imax, $grid_jmin,$grid_jmax ) = &grid_params( $REGION,$GRID ) ;
  ( $image_mapwidth,$image_mapheight, $image_maporiginx,$image_maporiginy ) = &image_params( $REGION,$GRID ) ;

  ### INITIALIZATION
  ### ensure REGION is capitalized
  $REGION =~ tr/a-z/A-Z/;

  ### CONVERT IMAGE i,k INTO LAT,LON
  ### find regional image values and grid corner values

  ### ADJUST FOR RE-SIZED MAP - convert re-sized image coord to original size coord
  $ximage = $iimage / $imagewidth ;
  $yimage = 1. - ( $kimage / $imageheight ) ;
 
  ### convert into i,j (nonstaggered)
  $aigrid = $grid_imin + ($grid_imax-$grid_imin)*(($ximage-$image_maporiginx)/$image_mapwidth) ;
  ### note that viewport y origin is on _top_edge of viewport
  $ajgrid = $grid_jmin + ($grid_jmax-$grid_jmin)*(($yimage-$image_maporiginy+$image_mapheight)/$image_mapheight) ;

  ### get nearest integer values
  $igrid = nint( $aigrid );
  $jgrid = nint( $ajgrid );

  ### FIND LAT,LON needed for input
  $result = `$BASEDIR/RASP/UTIL/ij2latlon.PL $REGION $GRID $igrid $jgrid` ;
  #old $result = `$SCRIPTDIR/ij2latlon.PL $REGION $GRID $IGRID $JGRID` ;
  my $tail ;
  ( $alat,$alon, $tail ) = split /\s+/, $result ;
  $alat = sprintf "%.3f", $alat ;
  $alon = sprintf "%.3f", $alon ;

  ### SET WRF OUT FILE
  if( $GRID eq 'd1' || $GRID eq '1' )
  { 
    $grid = 'd01' ;
    $region = $REGION ;
  }
  elsif( $GRID eq 'd2' || $GRID eq '2' )
  { 
    $grid = 'd02' ;
    $region = $REGION ;
  }
  elsif( $GRID eq 'w1' )
  { 
    $grid = 'd01' ;
    $region = $REGION . "-WINDOW" ;
  }
  elsif( $GRID eq 'w2' || $GRID eq '3' )
  { 
    $grid = 'd02' ;
    $region = $REGION . "-WINDOW" ;
  }
  else
  { die "$PROGRAMNAME ERROR EXIT: bad GRID value = $GRID"; }
  my ( $localtime_id, $localtime_adj ) =  region_params( $REGION ) ;
  ### strip any non-number from time, then parse into zulu hr+min
  ( $time = $TIME ) =~ s|[a-zA-Z]||g ;
  $zhr = int( $time / 100 ) ;
  $mm = sprintf "%02d", ( $time - 100*$zhr ) ;
  $hh = sprintf "%02d", ( $zhr - int( $localtime_adj ) ) ;
  if( $hh < 0 ) { $hh += 24 ; }
  elsif( $hh > 24 ) { $hh -= 24 ; }
  ### search wrf outpaut directory for needed file
  if( $DAY == 0 )
  {
    $wrfoutfiletest = "$BASEDIR/WRF/WRFV2/RASP/${region}/wrfout_${grid}_*_${hh}:${mm}:00" ;
    #untaint - even though $BASEDIR,$region,$grid,$hh,$mm tested to be untainted !?
    if ( defined $wrfoutfiletest && $wrfoutfiletest =~ m|^([A-Za-z0-9/][A-Za-z0-9_.:/-]*)$| ) { $wrfoutfiletest = $1 ; } # filename chars only
    $wrfoutfile = `ls $wrfoutfiletest`;
    chomp( $wrfoutfile );
    #old ### SET PARAMETER NAME includes lat,lon
    #old $param = "sounding0_${REGION}_${alat}_${alon}" ;
    #old #ok    $param = "sounding0_${REGION}-current_${alat}_${alon}" ;
    #old #bad    $param = "sounding0_${REGION}(Current)_${alat}_${alon}" ;
    #old #bad    $param = "sounding0_${REGION}:Current_${alat}_${alon}" ;
  }
  ### use previous file if run currently in progress so no "current" file
  if( $DAY == -1 || $wrfoutfile =~ m|^\s*$| )
  {
    $previouswrfoutfiletest = "$BASEDIR/WRF/WRFV2/RASP/${region}/previous.wrfout_${grid}_*_${hh}:${mm}:00" ;
    #untaint - even though $BASEDIR,$region,$grid,$hh,$mm tested to be untainted !?
    if ( defined $previouswrfoutfiletest && $previouswrfoutfiletest =~ m|^([A-Za-z0-9/][A-Za-z0-9_.:/-]*)$| ) { $previouswrfoutfiletest = $1 ; } # filename chars only
    $wrfoutfile = `ls $previouswrfoutfiletest`;
    chomp( $wrfoutfile );
    #old ### SET PARAMETER NAME includes lat,lon
    #old $param = "sounding0_${REGION}_${alat}_${alon}" ;
  }
  if( $wrfoutfile =~ m|^\s*$| )
  { die "$PROGRAMNAME ERROR EXIT: no current or previous WRF output file ala $wrfoutfiletest"; }

  ### SET PARAMETER NAME includes lat,lon
  $param = "sounding0_${REGION}_${alat}_${alon}" ;

  #4testprint: if ( defined $ENV{HOME} && $ENV{HOME} eq '/home/glendeni' )  {  print "x,yIMAGE= $iimage $kimage x,ySIZE= $image_xsize $image_ysize x,yORIGIN= $image_maporiginx $image_maporiginy i,jGRID= $aigrid $ajgrid \n";  }
  #4testprint: `echo "wrfoutfiletest= $wrfoutfiletest " >> /var/www/cgi-bin/LOG/get_rasp_skewt.test_prints`;
  #4testprint: `echo "${PLOTSCRIPT} -H -d /tmp $wrfoutfile $param" >> /var/www/cgi-bin/LOG/get_rasp_skewt.test_prints`;

  #untaint 
  if ( defined $wrfoutfile && $wrfoutfile =~ m|^([A-Za-z0-9/][A-Za-z0-9_.:/-]*)$| ) { $wrfoutfile = $1 ; }  # filename chars only
  #unused if( is_tainted($wrfoutfile) ) { `echo "TAINTED wrfoutfile " >> /var/www/cgi-bin/LOG/test.get_rasp_skewt.test_prints`; }

  ### PLOT USING EXTERNAL SCRIPT
  exec "${PLOTSCRIPT} -H -d /tmp $wrfoutfile $param";
  #4testprint:   print "${PLOTSCRIPT} ARGS= -H -d /tmp/ $wrfoutfile $param \n" ;

###########################################################################################
### FIND NEAREST INTEGER
sub nint { int($_[0] + ($_[0] >=0 ? 0.5 : -0.5)); }
###########################################################################################
### TAINT TEST
#unused sub is_tainted { not eval { join("",@_), kill 0; 1; }; }
###########################################################################################
