CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                      C
C                Copyright (C)  1995                                   C
C        University Corporation for Atmospheric Research               C
C                All Rights Reserved                                   C
C                                                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      File:           tm03f.f
C
C      Author:         Bob Lackman (converted by Ed Stautler)
C          National Center for Atmospheric Research
C          PO 3000, Boulder, Colorado
C
C      Date:           Fri Jan 06 18:31:18 MDT 1995
C
C      Description:    Demonstrates the TickMark Object
C                      with reversed and log y axis.
C
      external NhlFAppClass
      external NhlFTickMarkClass
      external NhlFXWorkstationClass
      external NhlFNcgmWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass

      real level(10)
      data level / 1000, 850, 700, 500, 400, 300, 250, 200, 150, 100 / 
C
C Define label strings for EXPLICIT mode tick mark placement
C
      character*10 labels(7)
      data labels /'90:S:o:N:S', '60:S:o:N:S', '30:S:o:N:S', 'EQ', 
     $     '30:S:o:N:N', '60:S:o:N:N', '90:S:o:N:N' /
C
C Specify data locations for above labels
C
      real labellocs(7)
      data labellocs / -90.0, -60.0, -30.0, 0.0, 30.0, 60.0, 90.0 /
        
      integer appid, wid, pid
      integer rlist, ierr

      integer NCGM, X11, PS, PDF
C
C Default is to create an X workstation.
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
C Create an application context. Set the app dir to the current
C directory so the application looks for a resource file in the
C working directory. In this example the resource file supplies the
C plot title only.
C
      call NhlFRLCreate(rlist,'SETRL')
      call NhlFRLClear(rlist)
      call NhlFRLSetstring(rlist,'appUsrDir','./',ierr)
      call NhlFRLSetstring(rlist,'appDefaultParent','True',ierr)
      call NhlFCreate(appid,'tm03',NhlFAppClass,0,rlist,ierr)

      if (NCGM.eq.1) then
C
C Create an NCGM workstation object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkMetaName','./tm03f.ncgm',ierr)
         call NhlFCreate(wid,'tm03Work',NhlFNcgmWorkstationClass,0,
     $        rlist,ierr)
      else if (X11.eq.1) then
C
C Create an XWorkstation object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPause','True',ierr)
         call NhlFCreate(wid,'tm03Work',NhlFXWorkstationClass,0,
     $        rlist,ierr)
      else if (PS.eq.1) then
C
C Create a PS object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPSFileName','./tm03f.ps',ierr)
         call NhlFCreate(wid,'tm03Work',NhlFPSWorkstationClass,0,
     $        rlist,ierr)
      else if (PDF.eq.1) then
C
C Create a PDF object.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetstring(rlist,'wkPDFFileName','./tm03f.pdf',ierr)
         call NhlFCreate(wid,'tm03Work',NhlFPDFWorkstationClass,0,
     $        rlist,ierr)
      endif
C
C Specify the viewport extent of the object.
C
      call NhlFRLClear(rlist)

      call NhlFRLSetfloat(rlist,'vpYF',.8,ierr)
      call NhlFRLSetfloat(rlist,'vpWidthF',.6,ierr)
      call NhlFRLSetfloat(rlist,'vpHeightF',.6,ierr)
      call NhlFRLSetfloat(rlist,'tmYLDataTopF',100.0,ierr)
      call NhlFRLSetfloat(rlist,'tmYLDataBottomF',1000.0,ierr)
      call NhlFRLSetfloat(rlist,'tmXBDataRightF',90.0,ierr)
      call NhlFRLSetfloat(rlist,'tmXBDataLeftF',-90.0,ierr)
      call NhlFRLSetstring(rlist,'tmYLStyle','Irregular',ierr)
      call NhlFRLSetstring(rlist,'tmXBMode','Explicit',ierr)
      call NhlFRLSetstring(rlist,'tmXBMinorOn','False',ierr)
      call NhlFRLSetfloatarray(rlist,'tmXBValues',labellocs,
     $     7,ierr)

C
C Array 'level' contains original grid point data locations in Y
C direction. Providing the grid points to the TickMark object as the
C control points for the IRREGULAR style transformation, means that
C these points will be evenly spaced along the Y axis. Since this is
C how CONPACK thinks the points are spaced, the tick marks will
C correctly correspond with the  data coordinates. See the HLU User's
C Guide for a complete discussion of IRREGULAR style transformations.
C
      call NhlFRLSetstringarray(rlist,'tmXBLabels',labels,
     $     7,ierr)
      call NhlFRLSetfloatarray(rlist,'tmYLIrregularPoints',level,
     $     10,ierr)
      call NhlFCreate(pid,'TickMarks',NhlFTickMarkClass,wid,
     $     rlist,ierr)

      call NhlFDraw(pid,ierr)
      call NhlFFrame(wid,ierr)
      call NhlFDestroy(pid,ierr)
      call NhlFDestroy(wid,ierr)
      call NhlFDestroy(appid,ierr)
      call NhlFClose

      stop
      end

