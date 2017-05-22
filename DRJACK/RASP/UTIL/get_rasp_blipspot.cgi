#!/usr/bin/perl -w -T

### GET RASP BLIPSPOT FOR SPECIFIED IMAGE LOCATION
### eg: http://www.drjack.info/cgi-bin/get_rasp_blipspot.cgi?region=PANOCHE&grid=d2&i=461&k=615&width=800&height=800&day=0

################################################################################

### MODIFIED FROM BLIP's get_image_minispot.cgi


use CGI::Carp qw(fatalsToBrowser);

  my $PROGRAMNAME = 'get_rasp_blipspot.cgi';

  #untaint - UNTAINT PATH
  $ENV{'PATH'} = '/bin:/usr/bin';
  ### following avoids warning statement
  local $BASEDIR ;

  ### DETERMINE SCRIPT DIRECTORY - this should be automatic but can over-ride here if necessary
  ### require latlon<->ij conversion scripts to be in current directory
  if( $0 =~ m|^/| ) { ( $SCRIPTDIR = "${0}" ) =~ s|/[^/]*$|| ; }
  else              { ( $SCRIPTDIR = "$ENV{'PWD'}/${0}" ) =~ s|[\./]*/[^/]*$|| ; }
  #untaint
  if ( defined $SCRIPTDIR && $SCRIPTDIR =~ m|^([A-Za-z0-9/][A-Za-z0-9_.:/-]*)$| ) { $SCRIPTDIR = $1 ; }  # filename chars only

  ### IMPORT SITE LOCAL PARAMETERS
  #untaint - untainting put into routines in this file
  require "$SCRIPTDIR/sub.rasp_site_parameters.PL" ;

  ### SET SITE DEPENDENT DIRECTORIES
  #untaint - untainting put into this routine
  &site_directories ;

  ### SET EXTERNAL SCRIPT WHICH EXTRACTS BLIPSPOT DATA INTO PRINTABLE FORMAT
  $EXTRACTSCRIPT = "$BASEDIR/RASP/UTIL/extract.blipspot.PL";

  ### SET PARAMETER FOR XI TESTS
  $LTEST = 0;
  #4XItest: $LTEST = 1;

   if ( $LTEST == 1 )
   {
    ###### INITIALIZATION FOR XI TESTS
    #4test: $EXTRACTSCRIPT = "$ENV{HOME}/DRJACK/RASP/UTIL/test.extract.blipspot.PL";
    ###### PANOCHE CASE
      $REGION = 'PANOCHE';
      ### ARTIFICIAL INPUT
      $GRID = 'd2' ;
      ### SET DAY=-1 TO GET SOUNDINGS FROM 24HR BEFORE
      $DAY = 0;
      $DAY = 20070901;
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

  #4testprint: `echo "ARGS: $REGION & $GRID & $DAY & $iimage & $kimage & $imagewidth & $imageheight " >> /var/www/cgi-bin/LOG/get_rasp_blipspot.test_prints`;

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

  #4testprint: if ( defined $ENV{HOME} && $ENV{HOME} eq '/home/glendeni' )  {  print "x,yIMAGE= $iimage $kimage x,ySIZE= $image_xsize $image_ysize x,yORIGIN= $image_maporiginx $image_maporiginy i,jGRID= $aigrid $ajgrid \n";  }

  ### GET BLIPSPOT INFO FROM EXTERNAL SCRIPT
  @spotlines = `${EXTRACTSCRIPT} $REGION $GRID $DAY $igrid $jgrid 1`;
  #4testprint: `echo "${EXTRACTSCRIPT} ARGS: $REGION & $GRID & $DAY & $igrid & $jgrid " >> /var/www/cgi-bin/LOG/get_rasp_blipspot.test_prints`;
    
  ### PRINT HTML TEXT HEADER+array
  print "Content-type: text/plain\n\n@{spotlines}\n";

  #4test:  print "\nCOOKIE== $cookieinfo \n";

###########################################################################################
### FIND NEAREST INTEGER
sub nint { int($_[0] + ($_[0] >=0 ? 0.5 : -0.5)); }
###########################################################################################
### TAINT TEST
#unused sub is_tainted { not eval { join("",@_), kill 0; 1; }; }
###########################################################################################
