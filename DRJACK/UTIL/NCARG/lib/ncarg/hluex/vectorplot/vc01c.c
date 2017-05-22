/*
 *      $Id: vc01c.c,v 1.3 2003/03/03 20:20:54 grubin Exp $
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1993                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *  File:       vc01c.c
 *
 *  Author:     David Brown
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *  Date:       Wed Apr  3 17:00:55 MST 1996
 *
 *  Description:  Basic VectorPlot example
 */

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


#define N 25
#define M 30
#define PI  3.14159    

main(int argc, char *argv[])
{

    int NCGM=0, X11=1, PS=0, PDF=0;
    int appid,wid,vcid,vfid;
    int rlist;
    int len_dims[2];
    float U[N][M],V[N][M];

/*
 * Generate vector data arrays
 */
    {
	    float igrid, jgrid;
	    int i,j;
	    igrid = 2.0 * PI / (float) M;
	    jgrid = 2.0 * PI / (float) N;
	    for (j = 0; j < N; j++) {
		    for (i = 0; i < M; i++) {
			    U[j][i] = 10.0 * cos(jgrid * (float) j);
			    V[j][i] = 10.0 * cos(igrid * (float) i);
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
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"vc01",NhlappClass,NhlDEFAULT_APP,rlist);

    if (NCGM) {
/*
 * Create a meta file workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./vc01c.ncgm");
        NhlCreate(&wid,"vc01Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (X11) {
/*
 * Create an X workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"vc01Work",NhlxWorkstationClass,appid,rlist);
    }

    else if (PS) {
/*
 * Create a PS workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"vc01c.ps");
        NhlCreate(&wid,"vc01Work",NhlpsWorkstationClass,appid,rlist);
    }
    else if (PDF) {
/*
 * Create a PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"vc01c.pdf");
        NhlCreate(&wid,"vc01Work",NhlpdfWorkstationClass,appid,rlist);
    }

/*
 * Create a VectorField object; then use its id as the value of
 * the 'vcVectorFieldData' resource when creating the VectorPlot object.
 */

    len_dims[0] = N;
    len_dims[1] = M;
    NhlRLClear(rlist);
    NhlRLSetMDFloatArray(rlist,NhlNvfUDataArray,&U[0][0],2,len_dims);
    NhlRLSetMDFloatArray(rlist,NhlNvfVDataArray,&V[0][0],2,len_dims);
    NhlCreate(&vfid,"vectorfield",NhlvectorFieldClass,appid,rlist);

    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNtiMainString,"Basic VectorPlot Example");
    NhlRLSetInteger(rlist,NhlNvcVectorFieldData,vfid);
    NhlCreate(&vcid,"vector",NhlvectorPlotClass,wid,rlist);

    NhlDraw(vcid);
    NhlFrame(wid);
/*
 * Destroy the objects created, close the HLU library and exit.
 */

    NhlDestroy(appid);
    NhlClose();
    exit(0);
}
