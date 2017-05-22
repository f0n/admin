C
C      $Id: xy07f.f,v 1.6 2003/03/03 21:31:21 grubin Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       xy07f.f
C
C  Author:     Mary Haley (converted from example "agex11")
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C  Date:       Mon Apr 24 11:11:02 MST 1995
C
C  Description:   This example is similar to the ncargex Autograph
C                 example "agex11".  It shows how to draw a "scattergram".
C                 It also shows one way on how to modify the color map
C                 so we can get a different background/foreground color.
C
      external NhlFAppClass
      external NhlFXWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFCoordArraysClass
      external NhlFXyPlotClass

      parameter(NPTS=250)
C
C Create data arrays for XyPlot object.
C
      real xdra(NPTS), ydra(NPTS)

      integer appid,xworkid,plotid,dataid(2)
      integer i, j, len(2)
      real cmap(3,4)
      character*10 datastr
      integer NCGM, X11, PS, PDF
C
C Default is to an X workstation.
C
      NCGM=0
      X11=1
      PS=0
      PDF=0
C
C Initialize the HLU library and set up resource template.
C
      call NhlFInitialize
      call NhlFRLCreate(rlist,'setrl')
C
C Change the color map so we can have a white background, a black
C foreground and two colors defined for our markers.  Color '1' is
C the background color and '2' is the foreground color.
C
      cmap(1,1) = 1.
      cmap(2,1) = 1.
      cmap(3,1) = 1.
      cmap(1,2) = 0.
      cmap(2,2) = 0.
      cmap(3,2) = 0.
      cmap(1,3) = 1.
      cmap(2,3) = 0.
      cmap(3,3) = 0.
      cmap(1,4) = 0.
      cmap(2,4) = 0.
      cmap(3,4) = 1.
      len(1) = 3
      len(2) = 4
C
C Create Application object.  The Application object name is used to
C determine the name of the resource file, which is "xy07.res" in
C this case.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'xy07',NhlFAppClass,0,rlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGMWorkstation object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkMetaName','./xy07f.ncgm',ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,len,ierr)
         call NhlFCreate(xworkid,'xy07Work',
     +        NhlFNcgmWorkstationClass,0,rlist,ierr)
      else if (X11.eq.1) then
C
C Create an XWorkstation object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPause','True',ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,len,ierr)
         call NhlFCreate(xworkid,'xy07Work',NhlFXWorkstationClass,
     +        0,rlist,ierr)
      else if (PS.eq.1) then
C
C Create a PSWorkstation object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPSFileName','./xy07f.ps',ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,len,ierr)
         call NhlFCreate(xworkid,'xy07Work',
     +        NhlFPSWorkstationClass,0,rlist,ierr)
      else if (PDF.eq.1) then
C
C Create a PDFWorkstation object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPDFFileName','./xy07f.pdf',ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,len,ierr)
         call NhlFCreate(xworkid,'xy07Work',
     +        NhlFPDFWorkstationClass,0,rlist,ierr)
      endif
C
C Since we have two sets of points that we want to color differently,
C we need to create two Data obects here.
C
      do 20 j = 1,2
C
C Initialize data.
C
         do 10 i = 1, NPTS
            xdra(i)=.5+(2.*(fran()-.5))**5
            ydra(i)=.5+(2.*(fran()-.5))**5
 10     continue
C
C Define a data object.  Note that we are naming each object differently
C so we can distinguish them in the resource file.
C
        call NhlFRLClear(rlist)
        call NhlFRLSetFloatArray(rlist,'caXArray',xdra,NPTS,ierr)
        call NhlFRLSetFloatArray(rlist,'caYArray',ydra,NPTS,ierr)
        write(datastr,30)j-1
        call NhlFCreate(dataid(j),datastr,NhlFcoordArraysClass,0,rlist,
     +                  ierr)
 20   continue
 30   format('xyData',i1)
C
C Create the XyPlot object.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetIntegerArray(rlist,'xyCoordData',dataid,2,ierr)
      call NhlFCreate(plotid,'xyPlot',NhlFxyPlotClass,xworkid,rlist,
     +     ierr)
C
C Draw the plot.
C
      call NhlFDraw(plotid,ierr)
      call NhlFFrame(xworkid,ierr)
C
C NhlDestroy destroys the given id and all of its children
C so destroying xworkwid will also destroy plotid.
C
      call NhlFDestroy(xworkid,ierr)
C
C Restores state.
C
      call NhlFDestroy(appid,ierr)
      call NhlFClose

      stop
      end
      
      function fran()
C
C Pseudo-random-number generator.
C
      double precision x
      save x
      data x / 2.718281828459045 /
      x=mod(9821.d0*x+.211327d0,1.d0)
      fran=real(x)
      return
      end
