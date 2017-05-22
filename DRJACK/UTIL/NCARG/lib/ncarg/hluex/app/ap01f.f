CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                     C
C                          Copyright (C)  1995                        C
C               University Corporation for Atmospheric Research       C
C                          All Rights Reserved                        C
C                                                                     C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     File:         ap01f.f
C
C     Author:       Jeff W. Boote 
C                   (translated to Fortran by David Younghans)
C
C     Date:         Tue September 26, 1995
C
C     Description:  This very simple Fortran program illustrates the
C                   steps involved in creating an HLU.
C
C

      program ap01f
      implicit none
      external NhlFAppClass
      external NhlFNcgmWorkstationClass
      external NhlFXWorkstationClass
      external NhlFPSWorkstationClass
      external NhlFPDFWorkstationClass
      external NhlFTextItemClass

      integer appid,workid,textid
      integer rlist,ierr
      integer NCGM, X11, PS, PDF

      NCGM=0
      X11=1
      PS= 0
      PDF=0

C
C Initialize the HLU library.
C
      call NhlFInitialize

C
C Create a ResList. The ResList is a list of the resources 
C and values being explicitly set during the "NhlFCreate" call.
C
      call NhlFRLCreate(rlist,'SETRL')

C
C Create an App object so we can have an application specific
C resource file for this example. Since the App object is the
C one that readsin the application specific resource files,
C these resources must be set prgrammatically. (They could
C be set in the $(NCARG_SYSRESFILE) or $(NCARG_USRRESFILE), but
C these thing are pretty specific to this example, so I am
C setting them programmatically.)
C
      call NhlFRLClear(rlist)
      call NhlFRLSetString(rlist,'appDefaultParent','True',ierr)
      call NhlFRLSetString(rlist,'appUsrDir','./',ierr)
      call NhlFCreate(appid,'ap01',NhlFAppClass,0,rlist,ierr)

C
C Create the Workstation to manage the output device.
C Since the appDefaultParent resource was set to True for
C 'ap01', we can use either 0 or appid as the Parent id.
C They mean the same thing.
C
      if (NCGM.eq.1) then
C
C Create a metafile workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkMetaName','./ap01f.ncgm',ierr)
         call NhlFCreate(workid,'x',
     &        NhlFNcgmWorkstationClass,0,rlist,ierr)

      elseif (X11.eq.1) then
C
C Create an X workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPause','True',ierr)
         call NhlFCreate(workid,'x',
     &        NhlFXWorkstationClass,0,rlist,ierr)

      elseif (PS.eq.1) then
C
C Create a PS workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPSFileName','./ap01f.ps',ierr)
         call NhlFCreate(workid,'x',
     &        NhlFPSWorkstationClass,0,rlist,ierr)
      elseif (PDF.eq.1) then
C
C Create a PDF workstation.
C
         call NhlFRLClear(rlist)
         call NhlFRLSetString(rlist,'wkPDFFileName','./ap01f.pdf',ierr)
         call NhlFCreate(workid,'x',
     &        NhlFPDFWorkstationClass,0,rlist,ierr)
      endif

C
C Destroy the ResList.
C Since I am not setting any more resources programmatically, I
C don't need the ResList anymore.
C
      call NhlFRLDestroy(rlist,ierr)

C
C Create a TextItem. I am not programmatically setting any of
C the TextItem resources, so the Resource Database made up from
C the resource file read-in by the 'ap01' App object is specifying
C all the attributes to the TextItem.
C
      call NhlFcreate(textid,'tx1',NhlFTextItemClass,workid,0,ierr)

C
C Call draw on the Workstation Object. This will cause all of the
C Workstation's children to Draw. In this case, this is only
C 'tx1', so draw could have been called on it just as easily.
C
      call NhlFDraw(workid,ierr)

C
C Call frame on the Workstation Object. This is functionally
C equivalent to calling NhlFUpdateWorkstation, and then
C NhlFClearWorkstation.
C
      call NhlFFrame(workid,ierr)

C
C Destroying an object also destroys its children, so by destroying
C 'ap01', all of the objects get destroyed.
C
      call NhlFDestroy(appid,ierr)

C
C Close the HLU library.
C
      call NhlFClose

      stop

      end

