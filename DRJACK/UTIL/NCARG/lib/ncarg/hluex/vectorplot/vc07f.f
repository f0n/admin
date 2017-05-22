C
C      $Id: vc07f.f,v 1.5 2003/03/03 20:20:54 grubin Exp $
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1996                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  File:       vc07f.f
C
C  Author:     David Brown (converted by Mary Haley)
C              National Center for Atmospheric Research
C              PO 3000, Boulder, Colorado
C
C  Date:       Wed Jul  3 9:25:32 MST 1996
C
C  Description: This example emulates the LLU example "fcover", overlaying
C               contours and vectors on a map plot.
C
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFXWorkstationClass
      external NhlFVectorFieldClass
      external NhlFVectorPlotClass
      external NhlFScalarFieldClass
      external NhlFContourPlotClass
      external NhlFMapPlotClass

      parameter(MSIZE=73,NSIZE=73,NROWS=11,NCOLORS=24)

      integer NCGM, X11, PS, PDF
      integer i, j
      integer appid, wid, cnid, vcid, mpid
      integer vfield, sfield
      integer grlist, rlist, len_dims(2)
      real U(NSIZE,MSIZE),V(NSIZE,MSIZE),P(NSIZE,MSIZE)
      integer ithin(NROWS)
      data ithin /90,15,5,5,4,4,3,3,2,2,2/
      character*256 filename
      integer flen
      real cmap(3,NCOLORS)
      data cmap/1.0,1.0,1.0,
     +          0.0,0.0,0.0,
     +          0.9,0.9,0.9,
     +          0.6,0.6,0.6,
     +          0.3,0.3,0.3,
     +          0.8,0.9,1.0,
     +          0.5,0.0,0.5,
     +          0.0,0.5,0.7,
     +          0.0,0.0,0.0,
     +          0.00000,1.00000,0.00000,
     +          0.14286,1.00000,0.00000,
     +          0.28571,1.00000,0.00000,
     +          0.42857,1.00000,0.00000,
     +          0.57143,1.00000,0.00000,
     +          0.71429,1.00000,0.00000,
     +          0.85714,1.00000,0.00000,
     +          1.00000,1.00000,0.00000,
     +          1.00000,0.85714,0.00000,
     +          1.00000,0.71429,0.00000,
     +          1.00000,0.57143,0.00000,
     +          1.00000,0.42857,0.00000,
     +          1.00000,0.28571,0.00000,
     +          1.00000,0.14286,0.00000,
     +          1.00000,0.00000,0.00000/

      len_dims(1) = 3
      len_dims(2) = NCOLORS

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
      call NhlFRLCreate(rlist,'setrl')
      call NhlFRLCreate(grlist,'getrl')

      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFRLSetString(rlist,'appDefaultParent','True',ierr)
      call NhlFCreate(appid,'vc07',NhlFappClass,0,rlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkMetaName','./vc07f.ncgm',ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,len_dims,
     +        ierr)
         call NhlFCreate(wid,'vc07Work',
     +        NhlFNcgmWorkstationClass,0,rlist,ierr)
      else if (X11.eq.1) then
C
C Create an xworkstation object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPause','True',ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,len_dims,
     +        ierr)
         call NhlFCreate(wid,'vc07Work',NhlFXWorkstationClass,
     +        0,rlist,ierr)
      else if (PS.eq.1) then
C
C Create a PostScript workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPSFileName','./vc07f.ps',ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,len_dims,
     +        ierr)
         call NhlFCreate(wid,'vc07Work',
     +        NhlFPSWorkstationClass,0,rlist,ierr)
      else if (PDF.eq.1) then
C
C Create a PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPDFFileName','./vc07f.pdf',ierr)
         call NhlFRLSetMDFloatArray(rlist,'wkColorMap',cmap,2,len_dims,
     +        ierr)
         call NhlFCreate(wid,'vc07Work',
     +        NhlFPDFWorkstationClass,0,rlist,ierr)
      endif
C
C Read the data file
C
      flen = 10
      call gngpat(filename,'examples',ierr)
      do 10 i=1,256
         if( filename(i:i).eq.char(0) ) then
            filename(i:i+flen) = '/fcover.dat'
            goto 15
         endif
 10   continue
 15   open(unit=10,file=filename,status='old')
      read (10,*) U
      read (10,*) V
      read (10,*) P
C
C Massage the data to eliminate surplus of vectors near the pole
C
      do 50 j=NSIZE,NSIZE-NROWS+1,-1
         do 49 i=1,MSIZE
            if (mod(i,ithin(NSIZE-j+1)).ne.0) then
               u(i,j) = -9999.0
            endif
 49      continue
 50   continue
C
C Create a MapPlot object.
C
      call NhlFCreate(mpid,'mapplot',NhlFmapPlotClass,wid,0,ierr)
C
C Create a ScalarField.
C
      len_dims(1) = NSIZE
      len_dims(2) = MSIZE
      call NhlFRLClear(rlist)
      call NhlFRLSetMDFloatArray(rlist,'sfDataArray',P,2,len_dims,ierr)
      call NhlFRLSetFloat(rlist,'sfXCStartV',-180.,ierr)
      call NhlFRLSetFloat(rlist,'sfYCStartV',-90.,ierr)
      call NhlFRLSetFloat(rlist,'sfXCEndV',180.,ierr)
      call NhlFRLSetFloat(rlist,'sfYCEndV',90.,ierr)
      call NhlFRLSetFloat(rlist,'sfMissingValueV', -9999.0,ierr)
      call NhlFCreate(sfield,'ScalarField',NhlFscalarFieldClass,appid,
     +     rlist,ierr)
C
C Create a VectorField.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetMDFloatArray(rlist,'vfUDataArray',U,2,len_dims,ierr)
      call NhlFRLSetMDFloatArray(rlist,'vfVDataArray',V,2,len_dims,ierr)
      call NhlFRLSetFloat(rlist,'vfXCStartV',-180.,ierr)
      call NhlFRLSetFloat(rlist,'vfYCStartV',-90.,ierr)
      call NhlFRLSetFloat(rlist,'vfXCEndV', 180.,ierr)
      call NhlFRLSetFloat(rlist,'vfYCEndV',90.,ierr)
      call NhlFRLSetFloat(rlist,'vfMissingUValueV',-9999.0,ierr)
      call NhlFRLSetFloat(rlist,'vfMissingVValueV',-9999.0,ierr)
      call NhlFCreate(vfield,'VectorField',NhlFvectorFieldClass,appid,
     +     rlist,ierr)
C
C Create a VectorPlot object.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'vcUseScalarArray','true',ierr)
      call NhlFRLSetInteger(rlist,'vcVectorFieldData',vfield,ierr)
      call NhlFRLSetInteger(rlist,'vcScalarFieldData',sfield,ierr)
      call NhlFCreate(vcid,'vectorplot',NhlFvectorPlotClass,wid,rlist,
     +     ierr)

      call NhlFRLClear(grlist)
      call NhlFRLGetFloat(grlist,'vcMinMagnitudeF',vmin,ierr)
      call NhlFRLGetFloat(grlist,'vcMaxMagnitudeF',vmax,ierr)
      call NhlFGetValues(vcid,grlist,ierr)

      call NhlFRLClear(rlist)
      call NhlFRLSetFloat(rlist,'vcMinMagnitudeF',vmin+0.1*(vmax-vmin),
     +     ierr)
      call NhlFSetValues(vcid,rlist,ierr)
C
C Create a ContourPlot object.
C
      call NhlFRLClear(rlist)
      call NhlFRLSetInteger(rlist,'cnScalarFieldData',sfield,ierr)
      call NhlFCreate(cnid,'contourplot',NhlFcontourPlotClass,wid,
     +     rlist,ierr)
C
C Overlay the vectors and the contours on the map and draw
C everything.
C
      call NhlFDraw(mpid,ierr)
      call NhlFFrame(wid,ierr)

      call NhlFDraw(cnid,ierr)
      call NhlFFrame(wid,ierr)

      call NhlFDraw(vcid,ierr)
      call NhlFFrame(wid,ierr)

      call NhlFAddOverlay(mpid,vcid,-1,ierr)
      call NhlFAddOverlay(mpid,cnid,-1,ierr)
      call NhlFDraw(mpid,ierr)
      call NhlFFrame(wid,ierr)

      call NhlFDestroy(wid,ierr)
      call NhlFClose
      stop
      end

