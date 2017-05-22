/*
 *      $Id: vc03c.c,v 1.2 2003/03/03 20:20:54 grubin Exp $
 *      
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1996                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *  File:       vc03c.c
 *
 *  Author:     David Brown (converted to C by Lynn Hermanson)
 *              National Center for Atmospheric Research
 *              PO 3000, Boulder, Colorado
 *
 *  Date:       June 5, 1996
 *
 *
 * Description: Manipulates the FillArrow resources to demonstrate some
 *               of the possible stylistic variations on the appearance
 *               of the filled vector arrows.
 *               The data is extracted from an NMC forecast dataset for
 *               11/10/1994.
 */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/VectorPlot.h>


#define a 2
#define b 37
#define c 37

main(int argc, char *argv[])
{
    int NCGM=0, X11=1, PS=0, PDF=0;
    int appid,wid,vcid,vfid;
    int rlist,grlist;
    int len_dims[3];
    float reflen, ref;
    float x[a][b][c];
    FILE * fd;
    int i,j,k;
/*
 * Generate vector data array
 */
    char  filename[256];   
    const char *dir = _NGGetNCARGEnv("data");
    sprintf( filename, "%s/asc/uvdata0.asc", dir );

    fd = fopen(filename,"r");
       
    for (k = 0; k < a; k++) {
        for (j = 0; j < b; j++) {
            for (i = 0; i < c; i++) {       
                fscanf(fd,"%f", &x[k][j][i]);
            }
        }
    }

/*
 * Initialize the high level utility library
 */
    NhlInitialize();
/*
 * Create an application context. Set the app dir to the current
 * directory so the application looks for a resource file in the working
 * directory. 
 */
    rlist = NhlRLCreate(NhlSETRL);
    grlist = NhlRLCreate(NhlGETRL);
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"vc03",NhlappClass,NhlDEFAULT_APP,rlist);

    if (NCGM) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./vc03c.ncgm");
        NhlCreate(&wid,"vc03Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (X11) {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"vc03Work",NhlxWorkstationClass,appid,rlist);
    }

    else if (PS) {
/*
 * Create a PS workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"vc03c.ps");
        NhlCreate(&wid,"vc03Work",NhlpsWorkstationClass,appid,rlist);
    }
    else if (PDF) {
/*
 * Create a PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"vc03c.pdf");
        NhlCreate(&wid,"vc03Work",NhlpdfWorkstationClass,appid,rlist);
    }
/*
 * Create a VectorField data object using the data set defined above.
 * By default the array bounds will define the data boundaries (zero-based,
 * as in C language conventions)
 */

    len_dims[0] = a;
    len_dims[1] = b;
    len_dims[2] = c;
    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNvfDataArray,&x[0][0][0],3,len_dims);
    NhlRLSetFloat(rlist,NhlNvfXCStartV, -180.0);
    NhlRLSetFloat(rlist,NhlNvfXCEndV, 0.0);
    NhlRLSetFloat(rlist,NhlNvfYCStartV, 0.0);
    NhlRLSetFloat(rlist,NhlNvfYCEndV, 90.0);
    NhlRLSetFloat(rlist,NhlNvfYCStartSubsetV, 20.0);
    NhlRLSetFloat(rlist,NhlNvfYCEndSubsetV, 80.0);
   
    NhlCreate(&vfid,"vectorfield",NhlvectorFieldClass,appid,rlist);

/*
 * Create a VectorPlot object, supplying the VectorField object as data
 */

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString, "Filled Arrow VectorPlot");
    NhlRLSetFloat(rlist,NhlNvcRefMagnitudeF, 20.0);
    NhlRLSetString(rlist,NhlNvcFillArrowsOn, "True");
    NhlRLSetFloat(rlist,NhlNvcMinFracLengthF, 0.2);

    NhlRLSetInteger(rlist,NhlNvcVectorFieldData,vfid);

    NhlCreate(&vcid,"vectorplot",NhlvectorPlotClass,wid,rlist);


    NhlRLClear(grlist);
    NhlRLGetFloat(grlist,NhlNvcRefLengthF,&reflen);
    NhlGetValues(vcid,grlist);

    ref= 1.5 * reflen;    
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvcRefLengthF,ref);
    NhlSetValues(vcid,rlist);

    NhlDraw(vcid);
    NhlFrame(wid);

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,
           "Variation #1:: Constant Width");
    NhlRLSetFloat(rlist,NhlNvcFillArrowWidthF, 0.15);
    NhlRLSetFloat(rlist,NhlNvcFillArrowMinFracWidthF, 1.0);
    NhlSetValues(vcid,rlist);

    NhlDraw(vcid);
    NhlFrame(wid);


    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,
           "Variation #2");
    NhlRLSetFloat(rlist,NhlNvcFillArrowMinFracWidthF,0.25);
    NhlRLSetFloat(rlist,NhlNvcFillArrowHeadMinFracXF,0.0);
    NhlRLSetFloat(rlist,NhlNvcFillArrowWidthF,0.2);
    NhlRLSetFloat(rlist,NhlNvcFillArrowHeadXF,0.8);
    NhlRLSetFloat(rlist,NhlNvcFillArrowHeadInteriorXF,0.7);
    NhlRLSetFloat(rlist,NhlNvcFillArrowHeadYF,0.2);
    NhlSetValues(vcid,rlist);

    NhlDraw(vcid);
    NhlFrame(wid);


    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,"Variation #3");
    ref = 1.2 * reflen;    
    NhlRLSetFloat(rlist,NhlNvcRefLengthF,ref);
    NhlRLSetFloat(rlist,NhlNvcFillArrowWidthF,0.3);
    NhlRLSetFloat(rlist,NhlNvcFillArrowHeadXF,0.4);
    NhlRLSetFloat(rlist,NhlNvcFillArrowHeadInteriorXF,0.35);
    NhlRLSetFloat(rlist,NhlNvcFillArrowHeadYF,0.3);

    NhlSetValues(vcid,rlist);

    NhlDraw(vcid);
    NhlFrame(wid);

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,"Variation #4");
    ref = 1.2 * reflen;    
    NhlRLSetFloat(rlist,NhlNvcRefLengthF,ref);
    NhlRLSetFloat(rlist,NhlNvcFillArrowWidthF,0.2);
    NhlRLSetFloat(rlist,NhlNvcFillArrowHeadXF,1.0);
    NhlRLSetFloat(rlist,NhlNvcFillArrowHeadInteriorXF,1.0);
    NhlRLSetFloat(rlist,NhlNvcFillArrowHeadYF,0.2);

    NhlSetValues(vcid,rlist);

    NhlDraw(vcid);
    NhlFrame(wid);

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,"Variation #5");
    ref = 0.8 * reflen;    
    NhlRLSetFloat(rlist,NhlNvcRefLengthF,ref);
    NhlRLSetFloat(rlist,NhlNvcFillArrowWidthF,0.2);
    NhlRLSetFloat(rlist,NhlNvcFillArrowHeadXF,1.5);
    NhlRLSetFloat(rlist,NhlNvcFillArrowHeadInteriorXF,1.0);
    NhlRLSetFloat(rlist,NhlNvcFillArrowHeadYF,0.5);

    NhlSetValues(vcid,rlist);

    NhlDraw(vcid);
    NhlFrame(wid);
/*
 * Destroy the objects created, close the HLU library and exit.
 */
    NhlDestroy(appid);
    NhlClose();
    exit(0);
}

