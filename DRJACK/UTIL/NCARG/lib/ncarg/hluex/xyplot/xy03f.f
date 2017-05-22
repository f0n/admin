C     
C      $Id: xy03f.f,v 1.15 2003/03/03 21:31:21 grubin Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       xy03f.f
C
C  Author:     Mary Haley
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Wed Feb  8 11:44:39 MST 1995
C
C  Description:    This program shows how to create an XyPlot object
C                  with some of the XyPlot line resources tweaked.  A
C                  resource file is used to changed the resources. 
C                  This program uses the same Y-axis dataset as the
C                  example "xy02", but this time values for the X
C                  axis are specified, changing the look of the plot.
C 
C                  The "CoordArrays" object is used to set up the data.
C
      external NhlFAppClass
      external NhlFXWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFXyPlotClass
      external NhlFCoordArraysClass
C
C Define the number of points in the curve.
C
      parameter(NPTS=500)
      parameter(PI100=.031415926535898)

      integer appid,xworkid,plotid,dataid
      integer rlist, i, len(2)
      real cmap(3,4)
      real xdra(NPTS), ydra(NPTS), theta
      integer NCGM, X11, PS, PDF
C
C Default is to an X workstation.
C
      NCGM=0
      X11=1
      PS=0
      PDF=0
C
C Initialize some data for the XyPlot object.
C
      do 10 i = 1,NPTS
         theta = PI100*real(i-1)
         xdra(i) = 500.+.9*real(i-1)*cos(theta)
         ydra(i) = 500.+.9*real(i-1)*sin(theta)
 10   continue
C
C Initialize the HLU library and set up resource template.
C
      call NhlFInitialize
      call NhlFRLCreate(rlist,'setrl')
C
C Modify the color map.  Color indices '1' and '2' are the background
C and foreground colors respectively.
C
      cmap(1,1) = 0.
      cmap(2,1) = 0.
      cmap(3,1) = 0.
      cmap(1,2) = 1.
      cmap(2,2) = 1.
      cmap(3,2) = 1.
      cmap(1,3) = 0.
      cmap(2,3) = .5
      cmap(3,3) = 1.
      cmap(1,4) = 0.
      cmap(2,4) = 1.
      cmap(3,4) = 0.
      len(1) = 3
      len(2) = 4
C
C Create Application object.  The Application object name is used to
C determine the name of the resource file, which is "xy03.res" in
C this case.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'xy03',NhlFAppClass,0,rlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkMetaName','./xy03f.ncgm',ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,len,ierr)
         call NhlFCreate(xworkid,'xy03Work',
     +        NhlFNcgmWorkstationClass,0,rlist,ierr)
      else if (X11.eq.1) then
C
C Create an xworkstation object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPause','True',ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,len,ierr)
         call NhlFCreate(xworkid,'xy03Work',NhlFXWorkstationClass,
     +        0,rlist,ierr)
      else if (PS.eq.1) then
C
C Create a PS workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPSFileName','./xy03f.ps',ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,len,ierr)
         call NhlFCreate(xworkid,'xy03Work',
     +        NhlFPSWorkstationClass,0,rlist,ierr)
      else if (PDF.eq.1) then
C
C Create a PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPDFFileName','./xy03f.pdf',ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,len,ierr)
         call NhlFCreate(xworkid,'xy03Work',
     +        NhlFPDFWorkstationClass,0,rlist,ierr)
      endif
C
C Define the data object.  The id for this object will later be used
C as the value for the XyPlot data resource, 'xyCoordData'.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetFloatArray(rlist,'caXArray',xdra,NPTS,ierr)
      call NhlFRLSetFloatArray(rlist,'caYArray',ydra,NPTS,ierr)
      call NhlFCreate(dataid,'xyData',NhlFCoordArraysClass,
     +                0,rlist,ierr)
C
C Create the XyPlot object which is created as a child of the
C Xworkstation object.  The resources that are being changed are done
C in the "xy03.res" file.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'xyCoordData',dataid,ierr)
      call NhlFCreate(plotid,'xyPlot',NhlFXyPlotClass,xworkid,
     +                rlist,ierr)
C
C Draw the plot.
C
      call NhlFDraw(plotid,ierr)
      call NhlFFrame(xworkid,ierr)
C
C NhlDestroy destroys the given id and all of its children
C so destroying "xworkid" will also destroy "plotid".
C
      call NhlFRLDestroy(rlist)
      call NhlFDestroy(xworkid,ierr)
      call NhlFDestroy(appid,ierr)
C
C Restores state.
C
      call NhlFClose
      stop
      end

