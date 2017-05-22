C     
C     $Id: mp06f.f,v 1.2 2003/03/04 17:17:58 grubin Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1999                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
C      File:            mp06f.f
C
C      Author:          Mary Haley
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Mon Dec 13 16:19:54 MST 1999
C
C   Description:  Shows how to draw county lines in the US.
C
      external NhlFAppClass
      external NhlFXWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFMapPlotClass

      integer appid,wid,mapid
      integer rlist
      integer NCGM, X11, PS, PDF
C
C List of Florida counties.
C
      character*50 fl_counties(69)
      data fl_counties/'Florida . Alachua','Florida . Baker',
     +'Florida . Bay','Florida . Bradford','Florida . Brevard',
     +'Florida . Broward','Florida . Calhoun','Florida . Charlotte',
     +'Florida . Citrus','Florida . Clay','Florida . Collier',
     +'Florida . Columbia','Florida . De Soto','Florida . Dixie',
     +'Florida . Duval','Florida . Escambia','Florida . Flagler',
     +'Florida . Franklin','Florida . Gadsden','Florida . Gilchrist',
     +'Florida . Glades','Florida . Gulf','Florida . Hamilton',
     +'Florida . Hardee','Florida . Hendry','Florida . Hernando',
     +'Florida . Highlands','Florida . Hillsborough',
     +'Florida . Holmes','Florida . Indian River',
     +'Florida . Jackson','Florida . Jefferson','Florida . Keys',
     +'Florida . Lafayette','Florida . Lake',
     +'Florida . Lee','Florida . Leon','Florida . Levy',
     +'Florida . Liberty','Florida . Madison','Florida . Manatee',
     +'Florida . Marion','Florida . Martin','Florida . Miami-Dade',
     +'Florida . Monroe','Florida . Nassau','Florida . Okaloosa',
     +'Florida . Okeechobee','Florida . Orange','Florida . Osceola',
     +'Florida . Palm Beach','Florida . Pasco','Florida . Pinellas',
     +'Florida . Polk','Florida . Putnam','Florida . Saint Johns',
     +'Florida . Saint Lucie','Florida . Saint Vincent Island',
     +'Florida . Santa Rosa','Florida . Sarasota',
     +'Florida . Seminole','Florida . Sumter','Florida . Suwannee',
     +'Florida . Taylor','Florida . Union','Florida . Volusia',
     +'Florida . Wakulla','Florida . Walton','Florida . Washington'/

C
C Default is to display output to an X workstation
C
      NCGM=0
      X11=1
      PS=0
      PDF=0
C
C Initialize the high level utility library
C
      call NhlFInitialize
C
C Create an application object.
C
      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'mp06',NhlFAppClass,0,rlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./mp06f.ncgm',ierr)
         call NhlFCreate(wid,'mp06Work',NhlFNcgmWorkstationClass,0,
     1        rlist,ierr)
      else  if (X11.eq.1) then
C
C Create an X workstation
C
         call NhlFRLClear(rlist)
         call NhlFRLSetinteger(rlist,'wkPause',1,ierr)
         call NhlFCreate(wid,'mp06Work',NhlFXWorkstationClass,0,
     1     rlist,ierr)
      else if (PS.eq.1) then
C
C Create a PS object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./mp06f.ps',ierr)
         call NhlFCreate(wid,'mp06Work',NhlFPSWorkstationClass,0,
     1        rlist,ierr)
      else if (PDF.eq.1) then
C
C Create a PDF object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPDFFileName','./mp06f.pdf',ierr)
         call NhlFCreate(wid,'mp06Work',NhlFPDFWorkstationClass,0,
     1        rlist,ierr)
      endif

C
C Create and draw a map with all mainland US counties outlined.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'vpWidthF',  0.90,ierr)
      call NhlFRLSetFloat(rlist,'vpHeightF', 0.90,ierr)
      call NhlFRLSetFloat(rlist,'vpXF',      0.05,ierr)
      call NhlFRLSetFloat(rlist,'vpYF',      0.95,ierr)

      call NhlFRLSetString(rlist,'mpOutlineBoundarySets', 
     +     'AllBoundaries',ierr)

      call NhlFRLSetFloat(rlist,'mpMinLatF',   25.,ierr)
      call NhlFRLSetFloat(rlist,'mpMaxLatF',   50.,ierr)
      call NhlFRLSetFloat(rlist,'mpMinLonF', -130.,ierr)
      call NhlFRLSetFloat(rlist,'mpMaxLonF',  -60. ,ierr)

      call NhlFRLSetString(rlist,'tiMainString',
     +     ':F22:US with all counties outlined',ierr)

      call NhlFCreate(mapid,'mp06',NhlFMapPlotClass,wid,rlist,ierr)

      call NhlFDraw(mapid,ierr)
      call NhlFFrame(wid,ierr)
C
C Draw all counties in the United States that have the name
C "Adams".
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'mpOutlineBoundarySets',
     +     'GeophysicalAndUSStates',ierr)
      call NhlFRLSetString(rlist,'mpOutlineSpecifiers','Adams',ierr)
      call NhlFRLSetString(rlist,'tiMainString',
     +     ':F22:US with Adams counties outlined',ierr)

      call NhlFSetValues(mapid,rlist,ierr)

      call NhlFDraw(mapid,ierr)
      call NhlFFrame(wid,ierr)
C
C By putting the string "Florida . " in front of each county name, only
C those counties in Florida will get drawn. Otherwise, if any of these
C counties existed in other states, those counties would get drawn as
C well.
C
      call NhlFRLClear(rlist)

      call NhlFRLSetStringArray(rlist,'mpOutlineSpecifiers',
     +     fl_counties,69,ierr)
      call NhlFRLSetFloat(rlist,'vpWidthF',   0.90,ierr)
      call NhlFRLSetFloat(rlist,'vpHeightF',  0.90,ierr)
      call NhlFRLSetFloat(rlist,'vpXF',       0.05,ierr)
      call NhlFRLSetFloat(rlist,'vpYF',       0.95,ierr)
      call NhlFRLSetFloat(rlist,'mpMinLatF',    25.,ierr)
      call NhlFRLSetFloat(rlist,'mpMaxLatF',    32.,ierr)
      call NhlFRLSetFloat(rlist,'mpMinLonF',   -90.,ierr)
      call NhlFRLSetFloat(rlist,'mpMaxLonF',   -80. ,ierr)
      call NhlFRLSetString(rlist,'tiMainString',
     +     ':F22:Florida and its counties outlined',ierr)

      call NhlFSetValues(mapid,rlist,ierr)

      call NhlFDraw(mapid,ierr)
      call NhlFFrame(wid,ierr)
C
C Destroy the objects created, close the HLU library and exit.
C
      call NhlFDestroy(wid,ierr)
      call NhlFClose
      stop
      end
