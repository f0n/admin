;;;  Plot Topography info generated in WRFSI/domains/REGIONXYZ/static
;;;       also create terrain, latitude, longitude datafiles
;;;       (created by modifing rasp.ncl)    

;;;;;;;  TO USE THIS PLOTTING SCRIPT:  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  (1) Copy file rasp.site_load.topo.ncl.TEMPLATE to create new file  rasp.site_load.topo.ncl
;;;  (2) Add data from your site's region(s) to file rasp.site_load.topo.ncl as follows:
;;;  (3) Add your REGION name following line "ADD NEW REGION NAME HERE" below (mimic example above it)
;;;      *NB* The region will be plotted is the _last_ REGION name listed
;;;  (4) Add your list of gui-produced filenames (one for each grid to be plotted) following line "ADD NEW REGION FILENAMES HERE" below (mimic example above it)
;;;  (5) Optional:  Add your grid corner point locations following line "ADD NEW REGION FRAME CORNERS HERE" below (see instructions following that line)
;;;  (6) Optional:  Force specified terrain contour spacing by uncommenting and altering line following "SET NEW REGION CONTOUR SPACING HERE" below (see instructions following that line)
;;;  (7) If not already set, set environmental variable  NCARG_ROOT to "$BASEDIR/UTIL/NCARG"
;;;  (8) Run NCL plot program to create ncgm metafile for each grid: "$NCARG_ROOT/bin/ncl topo.ncl"
;;;      (this will also create datafiles of height, lat, lon ala terrain.regionxyz.g?.data where ? is grid number)
;;;      alternative: "ncl topo.ncl LOUT=0" will plot to screen instead of to metafile
;;;  (9) Create PNG toporgraphy image from metafiles ala "$BASEDIR/UTIL/ncgm2png -800x800 topo.wrf.?.ncgm color_topo.g?.png" where ? is grid number
;;; (10) Note: terrain, latitude, and longitude data files are created for use with other programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;  TO-DO ?
;;; make selection of grid corner points automatic by reading from file wrfsi.nl


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROGRAMMING NOTES  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  NOTE THAT SYNTAX ERROR IN FUNCTION WILL CAUSE BOMB WITH MESSAGE
;          fatal:Undefined identifier: (ROUTINE_NAME) is undefined, can't continue
;;; "float" number must contain decimal pt to avoid "coercing" warning message
;;; rather slow, exp. for lcl calc
;;; COMMON ERRORS: 
;;;    using "endif" instead of "end if"
;;;    using ' instead of "
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; ncar routines
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

  ;;; wrf ncl routines 
  ;  "./" _not_ required prior to filename !
  ;;; ! no longer use  "load "gsn_code.ncl"  !!! 
  load "wrf_plot.ncl"           ; modified by jack
  load "wrf_user_mass.ncl"      ; modified by jack
  load "skewt_func.ncl"         ; modified by jack

  ;;; my fortran routines - load before my ncl routines since so they can use fortran functions
  ;  "./" _IS_ required prior to filename !
  external NCL_JACK_FORTRAN "./ncl_jack_fortran.so"
  ;4test:  external NCL_JACK_FORTRAN_TEST "./ncl_jack_fortran_test.so"
  ; gives syntax error!   external NCL_JACK_FORTRAN nclrundir+"ncl_jack_fortran.so"

  ;;; my ncl routines - must be loaded _after_ external NCL_JACK_FORTRAN
  load "blipmap_jack.ncl"      

;;; ***********************************************************************************
;;;                    START OF REQUIRED INPUT
;;; ***********************************************************************************

;;;;- ********** SELECT PLOT OUTPUT TYPE **********
;;; LOUT= 0=X11 2=individual_ncgm_files(topo.wrf.#.ncgm) -1=postscript(topo.wrf.ps)
if ( .not. isvar("LOUT") ) then 
  LOUT = 0
  LOUT = 2
end if

;;;;;;;;;;;;;;;;;;; START OF REGION DEPENDENT CHOICES ;;;;;;;;;;;;;;;;;;;;

;;; ********** SELECT DESIRED REGION ********* (selects file and frame)
REGION = "RUC-TEST"
REGION = "GREATBRITAIN"
REGION = "CANV"
REGION = "SIERRA"
REGION = "GOVCUP"    ; LOCAL NOT SAME AS ON WEBSITE
REGION = "MOJAVE"
REGION = "MINDEN"
REGION = "GREATBRITAIN-5to1g3"
REGION = "SW_SOUTHAFRICA"
REGION = "GENERIC_SFBAY"
REGION = "BLACKFOREST_CO"
REGION = "WILLIAMS"
REGION = "S_NEWZEALAND"
REGION = "PANOCHE"
;;; **********  ADD NEW REGION NAME HERE **********

;;; ********** SET REGION-SPECIFIC MODEL OUTPUT FILE  **********
; not that last item in list must NOT have a comma after it !
if( REGION .eq. "PANOCHE" ) then
   UNITS="american"
   filenames = (/ \
    "/home/admin/DRJACK/WRF/WRFSI/domains/PANOCHE/static/wrfstatic_d01", \ 
    "/home/admin/DRJACK/WRF/WRFSI/domains/PANOCHE/static/wrfstatic_d02", \ 
    "/home/admin/DRJACK/WRF/WRFSI/domains/PANOCHE/static/wrfstatic_d03", \ 
   "" /)
end if
if( REGION .eq. "WILLIAMS" ) then
   UNITS="american"
   filenames = (/ \
    "/home/admin/DRJACK/WRF/WRFSI/domains/WILLIAMS/static/wrfstatic_d01", \ 
    "/home/admin/DRJACK/WRF/WRFSI/domains/WILLIAMS/static/wrfstatic_d02", \ 
    "/home/admin/DRJACK/WRF/WRFSI/domains/WILLIAMS/static/wrfstatic_d03", \ 
   "" /)
end if
if( REGION .eq. "CANV" ) then
   UNITS="american"
   filenames = (/ \
    "/home/admin/DRJACK/WRF/WRFSI/domains/CANV/static/wrfstatic_d01", \ 
    "/home/admin/DRJACK/WRF/WRFSI/domains/CANV/static/wrfstatic_d02", \ 
    "/home/admin/DRJACK/WRF/WRFSI/domains/CANV/static/wrfstatic_d03", \ 
   "" /)
end if
if( REGION .eq. "RUC-TEST" ) then
   UNITS="american"
   filenames = (/ \
    "/home/admin/DRJACK/WRF/WRFSI/domains/RUC-TEST/static/wrfstatic_d01", \ 
    "/home/admin/DRJACK/WRF/WRFSI/domains/RUC-TEST/static/wrfstatic_d02", \ 
    ; "/home/admin/DRJACK/WRF/WRFSI/domains/RUC-TEST/static/wrfstatic_d03", \ 
   "" /)
end if
if( REGION .eq. "MINDEN" ) then
   UNITS="american"
   filenames = (/ \
    "/home/admin/DRJACK/WRF/WRFSI/domains/MINDEN/static/wrfstatic_d01", \ 
    "/home/admin/DRJACK/WRF/WRFSI/domains/MINDEN/static/wrfstatic_d02", \ 
    "/home/admin/DRJACK/WRF/WRFSI/domains/MINDEN/static/wrfstatic_d03", \ 
   "" /)
end if
if( REGION .eq. "SIERRA" ) then
   UNITS="american"
   filenames = (/ \
    "/home/admin/DRJACK/WRF/WRFSI/domains/SIERRA/static/wrfstatic_d01", \ 
    "/home/admin/DRJACK/WRF/WRFSI/domains/SIERRA/static/wrfstatic_d02", \ 
    ; "/home/admin/DRJACK/WRF/WRFSI/domains/SIERRA/static/wrfstatic_d03", \ 
   "" /)
end if
if( REGION .eq. "SW_SOUTHAFRICA" ) then
    UNITS="celsius"
   filenames = (/ \
    "/home/admin/DRJACK/WRF/WRFSI/domains/SW_SOUTHAFRICA/static/wrfstatic_d01", \ 
    "/home/admin/DRJACK/WRF/WRFSI/domains/SW_SOUTHAFRICA/static/wrfstatic_d02", \ 
    "/home/admin/DRJACK/WRF/WRFSI/domains/SW_SOUTHAFRICA/static/wrfstatic_d03", \ 
   "" /)
end if
if( REGION .eq. "S_NEWZEALAND" ) then
    UNITS="celsius"
   filenames = (/ \
    "/home/admin/DRJACK/WRF/WRFSI/domains/S_NEWZEALAND/static/wrfstatic_d01", \ 
    "/home/admin/DRJACK/WRF/WRFSI/domains/S_NEWZEALAND/static/wrfstatic_d02", \ 
    "/home/admin/DRJACK/WRF/WRFSI/domains/S_NEWZEALAND/static/wrfstatic_d03", \ 
   "" /)
end if
if( REGION .eq. "GREATBRITAIN" ) then
    UNITS="celsius"
   filenames = (/ \
    "/home/admin/DRJACK/WRF/WRFSI/domains/GREATBRITAIN/static/wrfstatic_d01", \ 
    "/home/admin/DRJACK/WRF/WRFSI/domains/GREATBRITAIN/static/wrfstatic_d02", \ 
    "/home/admin/DRJACK/WRF/WRFSI/domains/GREATBRITAIN/static/wrfstatic_d03", \ 
;;old    "/home/admin/DRJACK/WRF/WRFSI/domains/GREATBRITAIN-FIRST/static/wrfstatic_d01", \ 
;;old    "/home/admin/DRJACK/WRF/WRFSI/domains/GREATBRITAIN-FIRST/static/wrfstatic_d02", \ 
;;old    "/home/admin/DRJACK/WRF/WRFSI/domains/GREATBRITAIN-FIRST/static/wrfstatic_d03", \ 
   "" /)
end if
if( REGION .eq. "GREATBRITAIN-5to1g3" ) then
    UNITS="celsius"
   filenames = (/ \
    "/home/admin/DRJACK/WRF/WRFSI/domains/GREATBRITAIN-5to1g3/static/wrfstatic_d01", \ 
    "/home/admin/DRJACK/WRF/WRFSI/domains/GREATBRITAIN-5to1g3/static/wrfstatic_d02", \ 
    "/home/admin/DRJACK/WRF/WRFSI/domains/GREATBRITAIN-5to1g3/static/wrfstatic_d03", \ 
;;old    "/home/admin/DRJACK/WRF/WRFSI/domains/GREATBRITAIN-FIRST/static/wrfstatic_d01", \ 
;;old    "/home/admin/DRJACK/WRF/WRFSI/domains/GREATBRITAIN-FIRST/static/wrfstatic_d02", \ 
;;old    "/home/admin/DRJACK/WRF/WRFSI/domains/GREATBRITAIN-FIRST/static/wrfstatic_d03", \ 
   "" /)
end if
if( REGION .eq. "GOVCUP" ) then
   UNITS="american"
   filenames = (/ \
;   "/home/admin/DRJACK/WRF/WRFSI/domains/GOVCUP/static/wrfstatic_d01", \ 
;   "/home/admin/DRJACK/WRF/WRFSI/domains/GOVCUP/static/wrfstatic_d02", \ 
;   "/home/admin/DRJACK/WRF/WRFSI/domains/GOVCUP/static/wrfstatic_d03", \ 
;    "/home/admin/DRJACK/WRF/WRFSI/domains/GOVCUP_NORTHCOURSE/static/wrfstatic_d01", \ 
;    "/home/admin/DRJACK/WRF/WRFSI/domains/GOVCUP_NORTHCOURSE/static/wrfstatic_d02", \ 
;    "/home/admin/DRJACK/WRF/WRFSI/domains/GOVCUP_NORTHCOURSE/static/wrfstatic_d03", \ 
    "/home/admin/DRJACK/WRF/WRFSI/domains/GOVCUP_SOUTHCOURSE/static/wrfstatic_d01", \ 
    "/home/admin/DRJACK/WRF/WRFSI/domains/GOVCUP_SOUTHCOURSE/static/wrfstatic_d02", \ 
;    "/home/admin/DRJACK/WRF/WRFSI/domains/GOVCUP_SOUTHCOURSE/static/wrfstatic_d03", \ 
   "" /)
end if
if( REGION .eq. "GENERIC_SFBAY" ) then
   UNITS="american"
   filenames = (/ \
   "/home/admin/DRJACK/WRF/WRFSI/domains/GENERIC_SFBAY/static/wrfstatic_d01", \ 
   "/home/admin/DRJACK/WRF/WRFSI/domains/GENERIC_SFBAY/static/wrfstatic_d02", \ 
   "/home/admin/DRJACK/WRF/WRFSI/domains/GENERIC_SFBAY/static/wrfstatic_d03", \ 
   "" /)
end if
if( REGION .eq. "BLACKFOREST_CO" ) then
   UNITS="american"
   filenames = (/ \
   "/home/admin/DRJACK/WRF/WRFSI/domains/BLACKFOREST_CO/static/wrfstatic_d01", \ 
   "/home/admin/DRJACK/WRF/WRFSI/domains/BLACKFOREST_CO/static/wrfstatic_d02", \ 
   "/home/admin/DRJACK/WRF/WRFSI/domains/BLACKFOREST_CO/static/wrfstatic_d03", \ 
   "" /)
end if
if( REGION .eq. "MOJAVE" ) then
   UNITS="american"
   filenames = (/ \
   "/home/admin/DRJACK/WRF/WRFSI/domains/MOJAVE/static/wrfstatic_d01", \ 
   "/home/admin/DRJACK/WRF/WRFSI/domains/MOJAVE/static/wrfstatic_d02", \ 
   "/home/admin/DRJACK/WRF/WRFSI/domains/MOJAVE/static/wrfstatic_d03", \ 
   "" /)
end if
if( REGION .eq. "SISKIYOU" ) then
   UNITS="american"
   filenames = (/ \
   "/home/admin/DRJACK/WRF/WRFSI/domains/SISKIYOU/static/wrfstatic_d01", \ 
   "/home/admin/DRJACK/WRF/WRFSI/domains/SISKIYOU/static/wrfstatic_d02", \ 
;   "/home/admin/DRJACK/WRF/WRFSI/domains/SISKIYOU/static/wrfstatic_d03", \ 
   "" /)
end if
if( REGION .eq. "regionxyz" ) then
   UNITS="american"
   filenames = (/ \
   "/home/admin/DRJACK/WRF/WRFSI/domains/REGIONXYZ/static/wrfstatic_d01", \ 
   "/home/admin/DRJACK/WRF/WRFSI/domains/REGIONXYZ/static/wrfstatic_d02", \ 
   "/home/admin/DRJACK/WRF/WRFSI/domains/REGIONXYZ/static/wrfstatic_d03", \ 
   "" /)
end if
;;; **********  ADD NEW REGION FILENAMES HERE **********


;;; ********** ADD A FRAME TO PLOT **********  (kframe depends on REGION (file) to display subgrids)
;;; SET FRAME TO BE LOCATION OF INER SUBGRID WITHIN COARSE GRID 
;;;    for coarsest grid:  kframe(0,:)=(/ DOMAIN_ORIGIN_LLI, DOMAIN_ORIGIN_LLJ, DOMAIN_ORIGIN_URI, DOMAIN_ORIGIN_URJ /)
;;;       where DOMAIN_ORIGIN_LLI,... are values in _second_ column in $BASEDIR/WRF/wrfsi/REGIONXYZ/static/wrfsi.nl
;;;    for next-coarsest grid:  kframe(1,:)=(/ DOMAIN_ORIGIN_LLI, DOMAIN_ORIGIN_LLJ, DOMAIN_ORIGIN_URI, DOMAIN_ORIGIN_URJ /)
;;;       where DOMAIN_ORIGIN_LLI,... are values in _third_ column in $BASEDIR/WRF/wrfsi/REGIONXYZ/static/wrfsi.nl
;;;    ldoframe:  0=none, ;=const.frame.width, -1=>use_array kframe 
;;;    kframe array: xl,yb, xr,yt USING NAMELIST FORTRAN INDEXS !!! (which are offset 1/2 from mass coords)
;;;    SO for ACTUAL grid indexs (based on 1,1), subtract 1 from last 2 numbers !!!
;;;    SO for ACTUAL grid indexs (based on 0,0), subtract 1,2 from first,last 2 numbers !!!
kframe = new( (/3, 4/), float)
;;; frame plotting - set default value = none - the override for specifically chosen cases
ldoframe = 0
if( REGION .eq. "PANOCHE" ) then
   ldoframe = -1
   ;;;    kframe array: xl,yb, xr,yt USING NAMELIST FORTRAN INDEXS !!! (which are offset 1/2 from mass coords)
   kframe(0,:) = (/ 12,16, 29,37 /)   ; northg3
   kframe(1,:) = (/ 16,31, 39,65 /)   ; northg3
   kframe(2,:) = (/ 0,0, 0,0 /)       ; northg3
   ;originalg3 kframe(0,:) = (/ 12,16, 29,37 /)   ; PANOCHE3 grid2(1-51x1-69) in grid1(1-42x1-48)
   ;originalg3 kframe(1,:) = (/ 16,31, 39,65 /)   ; PANOCHE3 grid3b(1-69x1-63) in grid2(1-51x1-69)
   ;originalg3 kframe(2,:) = (/ 0,0, 0,0 /)       ; no grid4 available
end if 
if( REGION .eq. "WILLIAMS" ) then
   ldoframe = -1
   ;;;    kframe array: xl,yb, xr,yt USING NAMELIST FORTRAN INDEXS !!! (which are offset 1/2 from mass coords)
   kframe(0,:) = (/  9,14, 21,36 /)   ; WILLIAMS3 grid2 in grid1
   kframe(1,:) = (/  9,20, 31,57 /)   ; WILLIAMS3 grid3b in grid2
;calc   kframe(1,:) = (/ 10,23, 25,60 /)   ; WILLIAMS3 grid3b in grid2
  ;WILLIAMS3a  kframe(1,:) = (/ 18,15, 41,50 /)   ; WILLIAMS3 grid3a in grid2
   kframe(2,:) = (/ 0,0, 0,0 /)       ; no grid3 available
  ;WILLIAMS wave box in grid2 - 26 nov 2007
   kframe(1,:) = (/  10,13, 26,29 /)   ;
end if 
if( REGION .eq. "SISKIYOU" ) then
   ;;; display blending region
   ldoframe = 5
end if
if( REGION .eq. "CANV" ) then
   ldoframe = -1
   ;;;    kframe array: xl,yb, xr,yt USING NAMELIST FORTRAN INDEXS !!! (which are offset 1/2 from mass coords)
   kframe(0,:) = (/ 17,17, 41,49 /)   ;
   kframe(1,:) = (/ 0,0, 0,0 /)       ; no subgrid
   kframe(2,:) = (/ 0,0, 0,0 /)       ; no subgrid
end if 
if( REGION .eq. "RUC-TEST" ) then
   ldoframe = -1
   ;;;    kframe array: xl,yb, xr,yt USING NAMELIST FORTRAN INDEXS !!! (which are offset 1/2 from mass coords)
   kframe(0,:) = (/ 0,0, 0,0 /)   ;
   kframe(1,:) = (/ 0,0, 0,0 /)       ; no subgrid
   kframe(2,:) = (/ 0,0, 0,0 /)       ; no subgrid
end if 
if( REGION .eq. "MINDEN" ) then
   ldoframe = -1
   ;;;    kframe array: xl,yb, xr,yt USING NAMELIST FORTRAN INDEXS !!! (which are offset 1/2 from mass coords)
   kframe(0,:) = (/ 14,20, 29,44 /)   ;
   kframe(1,:) = (/ 10,30, 37,58 /)       ; 
   kframe(2,:) = (/ 0,0, 0,0 /)       ; no subgrid
end if 
if( REGION .eq. "SIERRA" ) then
   ldoframe = -1
   ;;;    kframe array: xl,yb, xr,yt USING NAMELIST FORTRAN INDEXS !!! (which are offset 1/2 from mass coords)
   kframe(0,:) = (/ 14,14, 31,58 /)   ;
   kframe(1,:) = (/ 0,0, 0,0 /)       ; no subgrid
   kframe(2,:) = (/ 0,0, 0,0 /)       ; no subgrid
end if 
if( REGION .eq. "SW_SOUTHAFRICA" ) then
   ldoframe = -1
   ;;;    kframe array: xl,yb, xr,yt USING NAMELIST FORTRAN INDEXS !!! (which are offset 1/2 from mass coords)
   kframe(0,:) = (/ 17,16, 32,34 /)   ;
   kframe(1,:) = (/ 9,10, 35,38 /)    ;
   kframe(2,:) = (/ 0,0, 0,0 /)       ; no subgrid
end if 
if( REGION .eq. "S_NEWZEALAND" ) then
   ldoframe = -1
   ;;;    kframe array: xl,yb, xr,yt USING NAMELIST FORTRAN INDEXS !!! (which are offset 1/2 from mass coords)
   kframe(0,:) = (/ 14,13, 28,42 /)   ;
   kframe(1,:) = (/  10,22,  35,58 /)  ;
   kframe(2,:) = (/ 0,0, 0,0 /)       ; no subgrid
end if 
if( REGION .eq. "GREATBRITAIN" ) then
   ldoframe = -1
   ;;;    kframe array: xl,yb, xr,yt USING NAMELIST FORTRAN INDEXS !!! (which are offset 1/2 from mass coords)
   ;GREATBRITAIN-second
   kframe(0,:) = (/ 6,7, 23,36 /)   ;
   ;GREATBRITAIN-second
   kframe(1,:) = (/ 11,15, 34,77 /)       ; 
   ;GREATBRITAIN-second-strawman2   kframe(1,:) = (/ 8,16, 29,78 /)       ; no subgrid
   ;GREATBRITAIN-second-strawman1   kframe(1,:) = (/ 11,5, 42,77 /)       ; no subgrid
   ;GREATBRITAIN-first kframe(1,:) = (/ 5,5, 42,40 /)   ;
   ;GREATBRITAIN-first   kframe(0,:) = (/ 10,11, 25,38 /)   ;
   kframe(2,:) = (/ 0,0, 0,0 /)       ; no subgrid
end if 
if( REGION .eq. "GOVCUP" ) then
   ldoframe = -1
   ;;;    kframe array: xl,yb, xr,yt USING NAMELIST FORTRAN INDEXS !!! (which are offset 1/2 from mass coords)
;northcourse   kframe(0,:) = (/ 13,13, 31,41 /)   ; 
;southcourse+blairstown
   kframe(0,:) = (/ 13,13, 29,37 /)   ; 
;northcourse   kframe(1,:) = (/ 23,34, 44,72 /)   ; no grid2 available
;southcourse   kframe(1,:) = (/ 8,18, 31,44 /)   ; no grid2 available
;blairstown
   kframe(1,:) = (/ 15,28, 38,54 /)   ; no grid2 available
;northcourse    kframe(2,:) = (/ 6,6, 59,109 /)       ; no grid4 available
;southcourse+blairstown
    kframe(2,:) = (/ 6,6, 65,74 /)       ; no grid4 available
;   kframe(2,:) = (/ 0,0, 0,0 /)       ; no grid4 available
end if 
if( REGION .eq. "GENERIC_SFBAY" ) then
   ldoframe = -1
   kframe(0,:) = (/ 15,15, 35,35 /)   ; us generic grid
   kframe(1,:) = (/ 19,19, 43,43 /)   ; us generic grid
   kframe(2,:) = (/ 0,0, 0,0 /)       ; no grid4 available
end if 
if( REGION .eq. "MOJAVE" ) then
   ldoframe = -1
   kframe(0,:) = (/ 15,15, 35,35 /)   ; us generic grid
   kframe(1,:) = (/ 19,19, 43,43 /)   ; us generic grid
   kframe(2,:) = (/ 0,0, 0,0 /)       ; no grid4 available
end if 
if( REGION .eq. "regionxyz" ) then
   ldoframe = -1
   kframe(0,:) = (/ 15,15, 35,35 /)   ; coarest grid
   kframe(1,:) = (/ 19,19, 43,43 /)   ; grid for "non-windowed" forecasts
   kframe(2,:) = (/ 0,0, 0,0 /)       ; grid for "windowed" forecasts
end if 
;;; **********  ADD NEW REGION FRAME CORNERS HERE **********


;;; **********  NEW REGION INFO READ FROM SITE-SPECIFIC FILE HERE **********
;;; FOR SITE SUBSTITUTIONS  (jack: if alter filename also change BLIP/PLOT/filename)
load "rasp.site_load.topo.ncl" 


;;;;;;;;;;;;;;;;;;; END OF REGION DEPENDENT CHOICES ;;;;;;;;;;;;;;;;;;;;

;;;; ********** SPECIFY PARAMETERS FOR SINGLE PARAMETER SCALAR/VECTOR PLOTS **********
; use "" for NO plots  bplot_params = (/ "" /)   ; parameters to plot (""=>none)
;;; CAPS INDICATES VARIABLES INTERNAL TO WRF OUTPUT FILE
;;; lower case indicates variables calculated by this plotting program
;;; ter plots color-fill terrain, HGT plots black+white contours
;COLOR-TERRAIN:
  bplot_params = (/ "ter" /)  
;BLACK+WHITE-TERRAIN:   bplot_params = (/ "HGT" /)  
nbplots = dimsizes(bplot_params)  ; number of single-variable plots

;; ADDED FOR VIEWPORT ARGUMENTS 
;;; main... used for all wrf_new_... calls EXCEPT map... used for wrf_new_map
;;; dont know significant if any of different values for main vs map height/width
;;; main seems to affect titles, labelbar, text
;;; map seems to affect plot itself
;;; methinks best to set both main... and map... parameters to same value to avoid potential problems !
    mainvpwidth = 0.80   ; note that mainvpwidth or mainvpheight used (based on ratio), not both
    mainvpheight = 0.70  ; note that mainvpwidth or mainvpheight used (based on ratio), not both
    mainvpx = 0.5*( 1.-mainvpwidth )
    mainvpy = 0.87
    ;original mainvpwidth = 0.85   ; note that mainvpwidth or mainvpheight used (based on ratio), not both
    ;original mainvpheight = 0.65  ; note that mainvpwidth or mainvpheight used (based on ratio), not both
    ;original     mapvpx = 0.10
    ;original     mapvpy = 0.87
    ;original     mapvpwidth = 0.80   ; note that mapvpwidth or mapvpheight used (based on ratio), not both
    ;original     mapvpheight = 0.70  ; note that mapvpwidth or mapvpheight used (based on ratio), not both
    mapvpx = mainvpx
    mapvpy = mainvpy
    mapvpwidth = mainvpwidth   ; note that mapvpwidth or mapvpheight used (based on ratio), not both
    mapvpheight = mainvpheight ; note that mapvpwidth or mapvpheight used (based on ratio), not both

;;; ***********************************************************************************
;;;                    END OF REQUIRED INPUT
;;; ***********************************************************************************

;********************************************************************************************
;*************************  FUNCTIONS FOLLOW  ***********************************************
;********************************************************************************************
;;;---------------------------------------------------------------------------------
;********************************************************************************************
;;; FOLLOWING REPLACED BY FORTRAN ROUTINES TO IMPROVE EFFICIENCY  (for std.rasp.set, 280 sec vice 33 sec)
; FUNCTION TO CALCULATE WSTAR (in mks - convert to wstar outside routine)
; FUNCTION TO CALCULATE BL-AVG  (based on z avg, not mass avg)
; FUNCTION TO CALCULATE MAX IN BL at mass point
; FUNCTION TO CALCULATE W MAX/MIN IN BL (cm/sec) - linfo=1 returns height of Wmax/min (inefficient in ncl, 5 sec vice 1 sec)
; FUNCTION TO INTEGRATE MIXING RATIO WITHIN BL  (mass-based)
; FUNCTION TO INTEGRATE MIXING RATIO *ABOVE* BL  (mass-based)
; FUNCTION TO FIND LOWEST NON-ZERO CLOUD LEVEL (EXPLICIT CLOUD)  (badly inefficient in ncl, 113 sec vice 2 sec)
; FUNCTION TO CALCULATE HEIGHT OF SFC.LCL  (badly inefficient in ncl, 112 sec vice 1 sec)
; FUNCTION TO CALCULATE HEIGHT OF BL.LCL  (badly inefficient in ncl)
; FUNCTION TO CALCULATE "CRITICAL HEIGHT" WHERE EST. W<225fpm
; FUNCTION TO CALC MAX IN CHOSEN 3D BOX
; FUNCTION TO LIMIT MAX FOR ARRAY
;********************************************************************************************
;*************************  END OF FUNCTIONS  ***********************************************
;********************************************************************************************


;;; MISC PARAMS
 TMPDIR = "TMP"

;jack - SETUP OUTPUT
if ( LOUT .eq. 0 ) then
  wks = wrf_open_X11()                ; output to screen
end if
if ( LOUT .eq. 1 ) then
  wks = wrf_open_ncgm("topo.wrf")   ; output to ncgm
end if
;;; ALLOW PLOTTING TO INDIVIDUAL OUTPUT FILES
if ( LOUT .eq. 2 ) then
  iframe=1
  wks = wrf_open_ncgm("topo.wrf."+iframe)   ; output to ncgm
end if
if ( LOUT .eq. -1 ) then
  wks = wrf_open_PS("topo.wrf")     ; output to postscript
end if

print ( "REGION= "+REGION )

;;; START LOOP OVER ALL OF THE FILES TO BE PROCESSED
nfiles = dimsizes(filenames)  ; number of files
do ifile = 0,(nfiles-1)

;;; set filename
;       WRF MASS input file - do not remove ".nc" in following line
; allow blank filename to exit (allows easier alteration of array
if ( filenames(ifile) .eq. "" ) then
  exit
end if
expandedfilename = filenames(ifile)+".nc"
;;; EXTRACT DOMAIN NUMBER FROM FILENAME
filechars =  stringtochar( expandedfilename )
do kk=0,150
  ;; bombs here with "subscript out of range" if wrfstatic not found in filename
  if( filechars(kk:kk+8).eq."wrfstatic" ) then
  ;4wrfout_file:  ;; bombs here with "subscript out of range" if wrfout not found in filename
  ;4wrfout_file: if( filechars(kk:kk+5).eq."wrfout" ) then
   ;;; use character to get integer value
    kdomain = chartointeger( filechars(kk+12:kk+12) ) - 48
    ;4wrfout_file: kdomain = chartointeger( filechars(kk+9:kk+9) ) - 48
   break
 end if 
end do
print ( "FILE= "+expandedfilename )
a = addfile( expandedfilename, "r" )
;old a = addfile( filenames(ifile)+".nc", "r" )

;;; big loop over all of the time periods to be processed

;frame(wks)  ; allow for window resize before beginning plots

it = 0
time=0

      ;;; SPECIAL TOPO FILE PROCESSING - differs from blipmap.ncl since dont use wrfout file
      domainid = "g"+kdomain
      ;;; FIND GRID SPACING (m)
      dx = a@DX
      dy = a@DY
      gridspacing = sqrt(  0.5* ( dx^2 + dy^2 ) )
      gridspacingkm = sprintf ( "%.1f", (0.001*gridspacing) )
      ;;; FIND PROJECTION INFO
      if( a@map_projection .ne. "LAMBERT CONFORMAL" ) then
        print ( "*** ERROR EXIT: projection must be lambers, instead is "+a@map_projection )
         exit
      else
        projection = "lambert"
      end if
      projlat1 = a@TRUELAT1
      projlat2 = a@TRUELAT2
      projlon = a@STAND_LON
      centerlat = a@CEN_LAT
      centerlon = a@CEN_LON
      ;;; PRINT FILE INFO
      print ( "FILE= "+expandedfilename+"  DOMAIN="+domainid+" GRIDSPACINGkm="+gridspacingkm )

;;; TEST PRINT OF MODEL OUTPUT DIMENSIONS 
;;; print modeloutput_variable dimensions
;3dmodel: modelvar = a->HFX(time,:,:,:)
;2dmodel: modelvar = a->V10(time,:,:)
;2dmodel: modelvar = a->HGT(time,:,:)
; dimmodelvar = dimsizes(modelvar)
; print ( "model dims= "+dimmodelvar )
; delete(modelvar)
; exit

;;; EXPECT TO ALMOST ALWAYS NEED FOLLOWING PARAMETERS SO SIMPLY GET THEM
;jack - TO PLOT TERRAIN HEIGHT
;; TERRAIN-HEIGHT:
        ter = wrf_user_getvar(a,"HGT",time) ; for terrain height
;; TERRAIN-HEIGHT:
        terft = 3.28084 * ter
;jack - TO PLOT LAND MASK
;; LANDMASK:        ter = wrf_user_getvar(a,"LANDMASK",time) ; for landmask
;; LANDMASK:        terft = ter 

        n2ddims = dimsizes(ter)
        print ( "2D x,y PTs= "+n2ddims(1)+","+n2ddims(0)+" => WIDTHs= "+(n2ddims(1)-1)+","+(n2ddims(0)-1) )

;;; PRINT CORNER LAT,LON
        alat  = wrf_user_getvar(a,"XLAT",time)
        alon  = wrf_user_getvar(a,"XLONG",time)
        print ( "CORNERS AT: SW="+alat(0,0)+","+alon(0,0)+" SE="+alat(0,(n2ddims(1)-1))+","+alon(0,(n2ddims(1)-1))+" NW="+alat((n2ddims(0)-1),0)+","+alon((n2ddims(0)-1),0)+" NE="+alat((n2ddims(0)-1),(n2ddims(1)-1))+","+alon((n2ddims(0)-1),(n2ddims(1)-1)) )

        paramdims = dimsizes(alat)
        jsize = paramdims(0)
        isize = paramdims(1)

        ;;; qgridinfo now set once for all files 
        qgridinfo = "Model= RASP Grid= "+REGION+" Subgrid= "+domainid+" Reskm= "+gridspacingkm+" Indexs="+" 1 "+sprinti("%i",isize)+" 1 "+sprinti("%i",jsize)+" Proj= lambert"+sprintf(" %.0f",dx)+sprintf(" %.0f",dy)+sprintf(" %.3f",projlat1)+sprintf(" %.3f",projlat2)+sprintf(" %.3f",projlon)+sprintf(" %.3f",centerlat)+sprintf(" %.3f",centerlon)
        ;old qgridinfo = "Model= RASP Grid= "+REGION+" Res= "+gridspacingkm+" Subgrid= "+domainid+" Indexs="+" 1 "+sprinti("%i",isize)+" 1 "+sprinti("%i",jsize)+" PROJ: lambert"+sprintf(" %.0f",dx)+sprintf(" %.0f",dy)+sprintf(" %.3f",projlat1)+sprintf(" %.3f",projlat2)+sprintf(" %.3f",projlon)+sprintf(" %.3f",centerlat)+sprintf(" %.3f",centerlon)
        print ( "GRIDINFO"+qgridinfo )

;;; WRITE LAT,LON DATA FILES
          aspid = "Latitude [Deg]"
          qdatafile = REGION+".latitude.g"+kdomain+".data"
          ;old qdatafile = "latitude.data"
          ;;; qgridinfo now set once for all files 
          ;old qgridinfo = "Model= RASP Grid= "+REGION+" Res= "+gridspacingkm+" Subgrid= "+domainid+" Indexs="+" 1 "+sprinti("%i",isize)+" 1 "+sprinti("%i",jsize)
          ;older qgridinfo = "WRF_grid: "+REGION+".g"+kdomain+" ="+" 1 "+sprinti("%i",isize)+" 1 "+sprinti("%i",jsize)
          ;oldest qgridinfo = "WRF_grid: "+REGION+" ="+" 1 "+sprinti("%i",isize)+" 1 "+sprinti("%i",jsize)
          qparaminfo = "Param= latitude Unit= degE Mult= 1"
          NCL_JACK_FORTRAN ::  output_mapdatafile( qdatafile, aspid,qgridinfo,qparaminfo, alat, isize,jsize, 4 )
          ;old=3sigfigs NCL_JACK_FORTRAN ::  output_mapdatafile( qdatafile, aspid,qgridinfo,qparaminfo, alat, isize,jsize, 3 )

          aspid = "Longitude [Deg]"
          qdatafile = REGION+".longitude.g"+kdomain+".data"
          ;old qdatafile = "longitude.data"
          ;;; qgridinfo now set once for all files 
          ;old qgridinfo = "Model= RASP Grid= "+REGION+" Res= "+gridspacingkm+" Subgrid= "+domainid+" Indexs="+" 1 "+sprinti("%i",isize)+" 1 "+sprinti("%i",jsize)
          ;older qgridinfo = "WRF_grid: "+REGION+".g"+kdomain+" ="+" 1 "+sprinti("%i",isize)+" 1 "+sprinti("%i",jsize)
          qparaminfo = "Param= longitude Unit= degN Mult= 1"
          NCL_JACK_FORTRAN ::  output_mapdatafile( qdatafile, aspid,qgridinfo,qparaminfo, alon, isize,jsize, 4 )
          ;old=3sigfigs NCL_JACK_FORTRAN ::  output_mapdatafile( qdatafile, aspid,qgridinfo,qparaminfo, alon, isize,jsize, 3 )

        delete ( alat )
        delete ( alon )

;;; =======================================================================

;;; BELOW SET FOR ALL MAPS TO AVOID DUPLICATION (though may make adjustments in a section)

        ;        Plotting options for the terrain
        ;jack - put inside loop since sometimes add title via opts_ter
        opts_ter = True
        opts_ter@NoTitles = True
        ;original opts_ter@cnLineColor = "navy blue"
        opts_ter@cnLineColor = "black"
        opts_ter@cnInfoLabelOn = False
        opts_ter@cnHighLabelsOn = False
        opts_ter@cnLowLabelsOn = False
        opts_ter@cnLineLabelAngleF = -1.
        ;512x512:opts_ter@cnLineLabelFontHeightF = .015
        ;800x800
        opts_ter@cnLineLabelFontHeightF = .010
        opts_ter@cnLineLabelPerimOn = False
        ;original opts_ter@cnLineLabelPerimColor = "navy blue"
        opts_ter@cnLineLabelPerimColor = "black"
        ;original opts_ter@cnLineLabelFontColor = "navy blue"
        opts_ter@cnLineLabelFontColor = "black"
        opts_ter@cnHighLabelBackgroundColor = -1
        ;original opts_ter@cnHighLabelFontColor = "navy blue"
        opts_ter@cnHighLabelFontColor = "black"
        opts_ter@cnLowLabelBackgroundColor = -1
        ;original opts_ter@cnLowLabelFontColor = "navy blue"
        opts_ter@cnLowLabelFontColor = "black"
        ;old opts_ter@LineThickness = 1.
        ;jack - add control for label spacing distance
        opts_ter@cnLineLabelDensityF = 0.5
        ;jack - eliminate box around terrain contour labels
        opts_ter@cnLineLabelBackgroundColor = -1
        ;;; set line label font
        opts_ter@cnConstFLabelFont = "helvetica-bold"
        opts_ter@cnLineLabelFont = "helvetica-bold"
        ;;; controls font of label above colorbar
        opts_ter@lbTitleFont = "helvetica"
        opts_ter@lbLabelFont = "helvetica"
        opts_ter@lbTitleFontHeightF     = 0.015
        opts_ter@lbLabelFontHeightF     = 0.015
        opts_ter@lbTitleOffsetF     = -0.5  ; move colorbar and its title closer together
        opts_ter@lbTopMarginF     = -0.2  ; move colorbar and its title upward, closer to plot - but also expands height of colorbar !
        opts_ter@lbLabelOffsetF     = 0.05  ; move numbers close to colorbar

;;; TOPO ADDITION 
;;; TERRAIN ADDITION - plot contour lines of differing thickness
;;    (note that wrf_plot.ncl puts opts_ter@LineThickness into array cnLineThicknesses - but below overrides this)
;;; OVERRIDE LINE THICKNESS - make every 4th contour line thicker
      opts_ter@cnLineThicknesses = (/ 1., 1.,1.,1.,2., 1.,1.,1.,2., 1.,1.,1.,2., 1.,1.,1.,2., 1.,1.,1.,2., 1.,1.,1.,2., 1.,1.,1.,2. /)  ;; nb: wrf_plot.ncl sets required cnMonoLineThickness=False
      if ( bplot_params(0) .ne. "HGT" ) then
;;; do NOT label contour lines:
        opts_ter@cnLineLabelInterval = 0 ;
      else
;;; label every 4th contour line (should be the thicker contour lines):
        opts_ter@cnLineLabelInterval = 4 ;
;;; use larger label font size
        opts_ter@cnLineLabelFontHeightF = .015
      end if 
      ;;;  Contours spacing is normally set automatically - to instead force the contour spacing to your desired value, uncomment and alter the following line
      ;contourspacing: opts_ter@ContourParameters = (/ 0., 0., 500.0 /)

        ;;; for frame - set line options   
        opts_frame = True
        if ( bplot_params(0) .ne. "HGT" ) then
          opts_frame@gsLineColor = "white"
        else
          opts_frame@gsLineColor = "black"
        end if
        ;;; gsLineDashPattern:  0=> solid  1=> longdash  2=shortdash  11=middledash
        opts_frame@gsLineDashPattern = 2
        opts_frame@gsLineThicknessF = 1.0

;;; =======================================================================


;;;        SINGLE PARAMETER PLOTS

; jack - LOOP OVER ALL bplot_params
do ibparam=0,(nbplots-1)

        ;        Map options
;        opts_mp = False
        opts_mp = True
      if ( bplot_params(0) .ne. "HGT" ) then
        opts_mp@mpOutlineDrawOrder = "Draw" ; makes land/sea outline less obtrusive but state lines semi-hidden - so up thickness 1->2 if used
        opts_mp@mpGeophysicalLineThicknessF = 2.0
        ;without_mpOutlineDrawOrder=Draw: opts_mp@mpGeophysicalLineThicknessF = 1.0
        opts_mp@mpGeophysicalLineColor = "white"
        opts_mp@mpUSStateLineThicknessF = 2.0 ; 
        ;without_mpOutlineDrawOrder=Draw: opts_mp@mpUSStateLineThicknessF = 1.0 
        opts_mp@mpUSStateLineColor = "white" 
        opts_mp@mpUSStateLineDashPattern = 1 ;  1=long_dash
        opts_mp@mpNationalLineThicknessF = 2.0
        opts_mp@mpNationalLineDashPattern = 1 ; "DashLine" ; 1=long_dash
        ;without_mpOutlineDrawOrder=Draw: opts_mp@mpNationalLineThicknessF = 1.0
        opts_mp@mpNationalLineColor = "white"
        opts_mp@mpNationalLineDashPattern = 1 ; 1=long_dash
        ;unused opts_mp@mpNationalLineDashSegLenF = 
        opts_mp@mpGridLineThicknessF = 0.5 ; think gives same result as 1.0
        opts_mp@mpGridLineColor = "white"
        opts_mp@mpLimbLineThicknessF = 1.0
        opts_mp@mpLimbLineColor = "white"
        opts_mp@mpPerimOn = True
        opts_mp@mpPerimLineThicknessF = 1.0
        opts_mp@mpPerimLineColor = "black"
        ;;; set lat/lon spacing (degrees)
        opts_mp@mpGridSpacingF = 1
        ;jack - added since lat line labels currently stop at 50 for no apparent reason
        opts_mp@mpGridMaxLatF = 70
        ;dontuse opts_mp@mpGridMaskMode = 3  ; lat/lon lines disappear when this activated
      else
        opts_mp@mpGeophysicalLineThicknessF = 2.0
        opts_mp@mpGeophysicalLineColor = "black"
        opts_mp@mpUSStateLineThicknessF = 2.0
        opts_mp@mpUSStateLineColor = "black"
        opts_mp@mpNationalLineThicknessF = 2.0
        opts_mp@mpNationalLineColor = "black"
        opts_mp@mpGridLineThicknessF = 0.5 ; think gives same result as 1.0
        opts_mp@mpGridLineColor = "black"
        opts_mp@mpLimbLineThicknessF = 1.0
        opts_mp@mpLimbLineColor = "black"
        opts_mp@mpPerimOn = True
        opts_mp@mpPerimLineThicknessF = 1.0
        opts_mp@mpPerimLineColor = "black"
        ;;; set lat/lon spacing (degrees)
        opts_mp@mpGridSpacingF = 1
        ;jack - added since lat line labels currently stop at 50 for no apparent reason
        opts_mp@mpGridMaxLatF = 70
      end if

        ;        ... and yet more map options galore!
        opts_map = True
        opts_map@LabelFont = "helvetica-bold"
        opts_map@LabelFontHeight = .01
        ;jack - add lat/lon labels along all sides
        opts_map@LatSide = (/ "Left", "Right" /)
        opts_map@LonSide = (/ "Bottom", "Top" /)

        ; setup map plot
        map = wrf_new_map(wks,a,opts_mp, mainvpx,mainvpy,mainvpwidth,mainvpheight)
        wrf_maplabel(wks,map,opts_map)

      ;jack - PLOT PARAMETER
      parameterid = bplot_params(ibparam)

        ;;; TREAT SCALAR CASE HERE

        ;;; for parameter in file 
        if ( parameterid .ne. "HGT" ) then
          if ( parameterid .eq. "ter" ) then
            if( UNITS .ne. "metric" ) then
              bparam = terft
            else  
              bparam = ter
            end if
          else
              bparam = wrf_user_getvar(a,parameterid,time) 
          end if
          ;;; PRINT MIN,MAX
          print( "  PARAMETER: "+parameterid+"  MIN,MAX= "+min(bparam)+"  "+max(bparam) )
        end if

;JACK - SET INDIVIDUAL PARAMETER CONTOUR RANGE
        opts_bparam = True
        opts_bparam@MainTitle = "Default"
; SET DEFAULT CONTOURS LEVELS HERE - auto-scales if opts_bparam@ContourParameters _not_ set
        ;old opts_bparam@ContourParameters = (/ 0., 12000., 500. /)

        ;;; SET PARAMETER-SPECIFIC OPTIONS
        if( parameterid .eq. "ter" ) then
          delete(opts_bparam)
          opts_bparam = True
          opts_bparam@MainTitle = "Terrain Height"
          ;;; convert to pilot units (english)
          if( UNITS .ne. "metric" ) then
             opts_ter@MainTitle = "Terrain Height [ft]"
             opts_bparam@UnitLabel = "[ft]"
          else
             opts_ter@MainTitle = "Terrain Height [m]"
             opts_bparam@UnitLabel = "[m]"
          end if
;;; **********  SET NEW REGION CONTOUR SPACING HERE **********
        ;;; TO FIX TERRAIN CONTOUR SPACING (with auto min/max): opts_bparam@ContourParameters = (/ 0., 0., 500. /)
        ;;; TO FIX TERRAIN CONTOUR SPACING (with set min/max): opts_bparam@ContourParameters = (/ 1000., 12000., 500. /)
        end if
;        if( parameterid .eq. "SHDMAX" ) then
;          delete(opts_bparam)
;          opts_bparam = True
;          opts_bparam@MainTitle = "Max annual greenness"
;          opts_bparam@UnitLabel = ""
;          ;unused: opts_bparam@ContourParameters = (/ 0., 800., 50. /)
;        end if

        ;        Plotting options for bparam
          opts_bparam@TimeLabel = " "

        ;;; SET OPTIONS VALID FOR ALL PARAMETERS
        ;old opts_bparam@UnitLabel = "BL(m)"
        ;overwrites_long_time_label
        opts_bparam@LevelLabel = ""
        opts_bparam@LineThickness = 0.5
        opts_bparam@cnInfoLabelOn = False
        opts_bparam@cnLineLabelsOn = False
        opts_bparam@lbAutoManage = False
        opts_bparam@lbBottomMarginF = .4
        ;old-unneeded opts_bparam@lbLabelStrings = (/ 0., 200., 400., 600., 800. /)
        ;;; controls font of label above colorbar
        opts_bparam@lbTitleFont = "helvetica"
        opts_bparam@lbLabelFont = "helvetica"
        opts_bparam@lbTitleFontHeightF     = 0.015
        opts_bparam@lbLabelFontHeightF     = 0.015
        opts_bparam@lbLabelOffsetF     = 0.05  ; move numbers close to colorbar
        ;old-title_above: opts_bparam@lbTitleOffsetF     = -0.5  ; move title lower, closer to colorbar
        ;old-title_above: opts_bparam@lbTopMarginF     = -0.2  ; move colorbar and its title upward, closer to plot - but also expands height of colorbar !
        ;old-title_above: opts_bparam@lbBottomMarginF = 0.4
        ;for label title at bottom:
        opts_bparam@lbTopMarginF     = 0.0 ; move colorbar upward, closer to map
        opts_bparam@lbBottomMarginF = 0.0
        opts_bparam@lbTitleExtentF     = 0.6  ; extent of title region from bottom perimeter
        opts_bparam@lbTitleOffsetF     = 0.0  ; buffer between title and colorbar

        ;jack - PLOT TERRAIN HEIGHT PRIOR TO OTHER PLOTS
        ; place at end since sometimes title added
        ; must put contour_ter creation here, not outside loop
        ; put Terrain title on initial plots since if variable undefined
        if ( parameterid .ne. "HGT" .and. min(bparam) .eq. max(bparam) ) then
             delete(opts_ter@NoTitles)
           opts_ter@MainTitle = opts_bparam@MainTitle
           opts_ter@TimeLabel = ""
        end if

        if ( parameterid .eq. "HGT" ) then
          delete(opts_ter@NoTitles)
          if( UNITS .ne. "metric" ) then
             opts_ter@MainTitle = "Terrain Height [ft]"
          else
             opts_ter@MainTitle = "Terrain Height [m]"
          end if
          opts_ter@TimeLabel = ""
        end if

        if( UNITS .ne. "metric" ) then
           contour_ter = wrf_new_line_contour(wks,terft(:,:),opts_ter, mainvpx,mainvpy,mainvpwidth,mainvpheight)    ; in feet
           ;write contour spacing info on plot
           anno_ter_contour(wks,contour_ter,"ft")
        else  
           contour_ter = wrf_new_line_contour(wks,ter(:,:),opts_ter, mainvpx,mainvpy,mainvpwidth,mainvpheight)      ; in meters
           ;write contour spacing info on plot
           anno_ter_contour(wks,contour_ter,"m")
        end if
        wrf_mapoverlay(map,contour_ter)

        ;jack - PLOT PARAMETER IF NOT A CONSTANT OR TERRAIN
        if ( parameterid .ne. "HGT" .and. min(bparam) .ne. max(bparam) ) then
          contour_bparam = wrf_new_fill_contour(wks,bparam(:,:),opts_bparam, mainvpx,mainvpy,mainvpwidth,mainvpheight)
          wrf_mapoverlay(map,contour_bparam)
        end if
        delete(opts_bparam)

        ;        Put on the map last, and then close the metacode frame.
        draw(map)

   ;;; add a frame to indicate boundary region (must do here to get overlaid on top)
          if( ldoframe .gt. 0 ) then
            wrf_add_frame(wks,map, ter(:,:),opts_frame, ldoframe, mainvpwidth,mainvpheight )
            ;new-NOTworking primitivethingey = add_frame(wks, ter(:,:),opts_frame, ldoframe, (/isize,jsize/) )
          end if
          if( ldoframe .lt. 0 ) then
            ;;; make kframe dependent on file so can display subgrids - xl,yb, xr,yt
            wrf_add_frame(wks,map, ter(:,:),opts_frame, kframe(ifile,:), mainvpwidth,mainvpheight )
            ;new-NOTworking primitivethingey = add_frame(wks, ter(:,:),opts_frame, kframe(ifile,:), (/isize,jsize/) )
          end if

;4TOPO - plot optional line (must put here to plot on top)
if( REGION .eq. "GOVCUP" ) then
  print( ">> PLOTTING GOVCUP south POLYLINE" )
  opts_polyline = True
  ;;  GOVCUP NORTH course (Blairstown-Hackettstown-Middletown-Piolis-Wurtsboro-Blairstown)
  if ( isvar("polyline_x") ) then
    delete ( polyline_x )
  end if
  if ( isvar("polyline_y") ) then
    delete ( polyline_y )
  end if
  polyline_x = (/ -74.997,-74.855,-74.394,-74.333,-74.458,-74.997 /)
  polyline_y = (/  40.971, 40.820, 41.432, 41.865, 41.597, 40.971 /)
  opts_polyline@gsLineColor = "yellow"
  opts_polyline@gsLineDashPattern = 0    ; solid
  gsn_polyline( wks,map, polyline_x,polyline_y, opts_polyline )
  ;;  GOVCUP SOUTH course (Blairstown-Beltzville-Kutztown-Pottstown-PhiladelphiaGliderCouncil-VanSant-Blairstown)
  if ( isvar("polyline_x") ) then
    delete ( polyline_x )
  end if
  if ( isvar("polyline_y") ) then
    delete ( polyline_y )
  end if
  polyline_x = (/ -74.997,-75.635,-75.787,-75.671,-75.247,-75.100,-74.997 /)
  polyline_y = (/  40.971, 40.844, 40.503, 40.260, 40.332, 40.484, 40.971 /)
  opts_polyline@gsLineColor = "red"
  opts_polyline@gsLineDashPattern = 0    ; solid
  gsn_polyline( wks,map, polyline_x,polyline_y, opts_polyline )
  ;;  GOVCUP SOUTH - OPTIONAL MORGANTOWN course (Kutztown-Morgantown-PhiladelphiaGliderCouncil)
  if ( isvar("polyline_x") ) then
    delete ( polyline_x )
  end if
  if ( isvar("polyline_y") ) then
    delete ( polyline_y )
  end if
  polyline_x = (/ -75.787,-75.870,-75.247 /)
  polyline_y = (/  40.503, 40.157, 40.332 /)
  opts_polyline@gsLineColor = "red"
  opts_polyline@gsLineDashPattern = 1    ; solid
  gsn_polyline( wks,map, polyline_x,polyline_y, opts_polyline )
  ;;  GOVCUP SOUTH - OPTIONAL NEW GARDEN course (Kutztown-NewGarden-PhiladelphiaGliderCouncil)
  if ( isvar("polyline_x") ) then
    delete ( polyline_x )
  end if
  if ( isvar("polyline_y") ) then
    delete ( polyline_y )
  end if
  polyline_x = (/ -75.787,-75.770,-75.247 /)
  polyline_y = (/  40.503, 39.830, 40.332 /)
  opts_polyline@gsLineColor = "red"
  opts_polyline@gsLineDashPattern = 1    ; solid
  gsn_polyline( wks,map, polyline_x,polyline_y, opts_polyline )
  ;;  GOVCUP SOUTH - OPTIONAL SOMERSET course (PhiladelphiaGliderCouncil-Somerset-Blairstown)
  if ( isvar("polyline_x") ) then
    delete ( polyline_x )
  end if
  if ( isvar("polyline_y") ) then
    delete ( polyline_y )
  end if
  polyline_x = (/ -75.247,-74.670,-74.997 /)
  polyline_y = (/  40.332, 40.625, 40.971 /)
  opts_polyline@gsLineColor = "red"
  opts_polyline@gsLineDashPattern = 1    ; solid
  gsn_polyline( wks,map, polyline_x,polyline_y, opts_polyline )
end if

        frame(wks)

        ;;; ALLOW PLOTTING TO INDIVIDUAL OUTPUT FILES
        if ( LOUT .eq. 2 ) then
          iframe = iframe + 1
            wks = wrf_open_ncgm("topo.wrf."+iframe)   ; output to ncgm
        end if  

;;; WRITE TOPO DATA FILE
          paramdims = dimsizes(ter)
          jsize = paramdims(0)
          isize = paramdims(1)
          aspid = opts_ter@MainTitle
          ;alt aspid = "Terrain Height (Tpt)"
          qdatafile = REGION+".terrain.g"+kdomain+".data"
          ;old qdatafile = "topohgt."+REGION+".g"+kdomain+".data"
          ;;; qgridinfo now set once for all files 
          ;old qgridinfo = "Model= RASP Grid= "+REGION+" Res= "+gridspacingkm+" Subgrid= "+domainid+" Indexs="+" 1 "+sprinti("%i",isize)+" 1 "+sprinti("%i",jsize)
          ;old qgridinfo = "WRF_grid: "+REGION+".g"+kdomain+" ="+" 1 "+sprinti("%i",isize)+" 1 "+sprinti("%i",jsize)
          if( UNITS .ne. "metric" ) then
            qparaminfo = "Param= terrain Unit= ft Mult= 1"
            ;;; print as integer value
            NCL_JACK_FORTRAN ::  output_mapdatafile( qdatafile, aspid,qgridinfo,qparaminfo, terft, isize,jsize, 0 )
          else
            qparaminfo = "Param= terrain Unit= m Mult= 1"
            ;;; print as integer value
            NCL_JACK_FORTRAN ::  output_mapdatafile( qdatafile, aspid,qgridinfo,qparaminfo, ter, isize,jsize, 0 )
          end if

       ; blank any opts_ter title set above
       opts_ter@MainTitle = ""

end do
        
;;; =======================================================================

;jack-1timeonly- end do         ;        end of the time loop

   ;;; end of loop cleanup

   if ( isvar("bparam") ) then
     delete ( bparam )
   end if
   delete ( ter )
   delete ( terft )

end do ;  end of file loop
