C
C     $Id: cn01f.f,v 1.9 2003/03/04 16:46:36 grubin Exp $
C
C***********************************************************************
C                                                                      *
C                            Copyright (C)  1995                       *
C                 University Corporation for Atmospheric Research      *
C                            All Rights Reserved                       *
C                                                                      *
C***********************************************************************
C
C      File:            cn01f.f
C
C      Author:          Dave Brown (converted to Fortran by Mary Haley)
C                       National Center for Atmospheric Research
C                       PO 3000, Boulder, Colorado
C
C      Date:            Tue Jan 24 09:34:23 MST 1995
C
C      Description:     Given a simple mathematically generated data 
C                       set, demonstrates ContourPlot with all resources 
C                       (other thancnScalarFieldData) set to their 
C                       default value.
C
C
C Extern declarations for Types of objects that will be used
C
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFXWorkstationClass
      external nhlfscalarfieldclass
      external nhlfcontourplotclass

      parameter(M=25,N=25)
      parameter(PI=3.14159)

      real  t(625)
      integer len_dims(2)
      integer appid,wid,dataid,cnid
      integer srlist
      real  x,y
      integer i,j
      integer NCGM, X11, PS, PDF
C
C Default is to display output to an X workstation
C
      NCGM=0
      X11=0
      PS=0
      PDF=1
C
C Create a simple bull's eye pattern test data set
C     
      ii = 1
      do 20 i = -N/2,N/2
         do 10 j = -M/2,M/2
            x = 8.0 * i
            y = 8.0 * j
            t(ii) = 100.0 - sqrt(x*x + y*y)
            ii = ii + 1
 10      continue
 20   continue
C
C Initialize the high level utility library
C
      call NhlFInitialize
C
C Create an application context. Set the app dir to the current dir
C so the application looks for a resource file in the working directory.
C In this example the resource file supplies the plot title only.
C
      call NhlFRLCreate(srlist,'SETRL')
      call NhlFRLClear(srlist)
      call NhlFRLSetstring(srlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'cn01',NhlFAppClass,0,srlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkMetaName','./cn01f.ncgm',ierr)
         call NhlFCreate(wid,'cn01Work',NhlFNcgmWorkstationClass,
     1     0,srlist,ierr)
      else if (X11.eq.1) then
C
C Create an X workstation.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'cn01Work',NhlFXWorkstationClass,
     1        0,srlist,ierr) 
      else if (PS.eq.1) then
C
C Create a PS object.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkPSFileName','./cn01f.ps',ierr)
         call NhlFCreate(wid,'cn01Work',NhlFPSWorkstationClass,
     1     0,srlist,ierr)
      else if (PDF.eq.1) then
C
C Create a PDF object.
C
         call NhlFRLClear(srlist)
         call NhlFRLSetstring(srlist,'wkPDFFileName','./cn01f.pdf',ierr)
         call NhlFCreate(wid,'cn01Work',NhlFPDFWorkstationClass,
     1     0,srlist,ierr)
      endif
C
C Create a ScalarField data object using the data set defined above.
C By default the array bounds will define the data boundaries
C (zero-based, as in C language conventions)
C
      call NhlFRLClear(srlist)
      len_dims(1) = N
      len_dims(2) = M
      call NhlFRLSetmdfloatarray(srlist,'sfDataArray',t,2,len_dims,ierr)
      call NhlFCreate(dataid,'bullseye',nhlfscalarfieldclass,
     1      appid, srlist,ierr)
C
C Create a ContourPlot object, supplying the ScalarField object as data
C
      call NhlFRLClear(srlist)
      call NhlFRLSetinteger(srlist,'cnScalarFieldData',dataid,ierr)
      call NhlFCreate(cnid,'ContourPlot1',nhlfcontourplotclass,
     1      wid,srlist,ierr)
C
C Draw a plot illustrating the basic default behavior of
C the ContourPlot object. The contours appear as solid lines with 
C unboxed labels in a linear coordinate system with the origin at 
C the lower left. Tickmarks with labels show the data coordinate 
C range, and an informational label at the lower right gives the 
C minimum and maximum data values and the contour interval spacing.
C
      call NhlFDraw(cnid,ierr)
      call NhlFFrame(wid,ierr)
C
C Destroy the objects created, close the HLU library and exit.
C
      call NhlFDestroy(dataid,ierr)
      call NhlFDestroy(cnid,ierr)
      call NhlFDestroy(wid,ierr)
      call NhlFDestroy(appid,ierr)

      call NhlFClose
      stop
      end
