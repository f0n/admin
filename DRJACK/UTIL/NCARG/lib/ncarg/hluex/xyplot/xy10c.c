/*
**      $Id: xy10c.c,v 1.4 2003/03/03 21:31:21 grubin Exp $
*/
/***********************************************************************
*                                                                      *
*                Copyright (C)  1995                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
**  File:       xy10c.c
**
**  Author:     Mary Haley
**          National Center for Atmospheric Research
**          PO 3000, Boulder, Colorado
**
**  Date:       Thu May  4 15:24:48 MDT 1995
**
** Description:    This example shows how to overlay an XyPlot
**                 on a MapPlot.
*/

#include <stdio.h>
#include <math.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ResList.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/XyPlot.h>
#include <ncarg/hlu/MapPlot.h>
#include <ncarg/hlu/CoordArrays.h>
#include <netcdf.h>

/*
 * Define the day and the hour for which we are getting data values.
 * (March 18, 1995, hour 0).
 */
char *file = "95031800_sao.cdf";

main()
{
/*
 * Declare variables for the HLU routine calls.
 */
    int     appid, xworkid, xyid1, xyid2, mpid1, mpid2, dataid;
    int     rlist, i;
    float   *lat, *lon;
    float   special_value = -9999.;
/*
 * Declare variables for getting information from netCDF file.
 */
    int     ncid, latid, lonid, recid;
    int     ndims, nvars, ngatts;
    long    rec_len;
    long    start[1], count[1];
    char    filename[256], recname[50];
    const   char *dir = _NGGetNCARGEnv("data");

    int NCGM=0, X11=1, PS=0, PDF=0;
/*
 * Initialize the HLU library and set up resource template.
 */
    NhlInitialize();
    rlist = NhlRLCreate(NhlSETRL);
/*
 * Create Application object.  The Application object name is used to
 * determine the name of the resource file, which is "xy10.res" in
 * this case.
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNappDefaultParent,"True");
    NhlRLSetString(rlist,NhlNappUsrDir,"./");
    NhlCreate(&appid,"xy10",NhlappClass,NhlDEFAULT_APP,rlist);

    if (NCGM) {
/*
 * Create a meta file object.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./xy10c.ncgm");
        NhlCreate(&xworkid,"xy10Work",NhlncgmWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (X11) {
/*
 * Create an XWorkstation object.
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&xworkid,"xy10Work",NhlxWorkstationClass,
              NhlDEFAULT_APP,rlist);
    }
    else if (PS) {
/*
 * Create a PS workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./xy10c.ps");
        NhlCreate(&xworkid,"xy10Work",NhlpsWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
    else if (PDF) {
/*
 * Create a PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"./xy10c.pdf");
        NhlCreate(&xworkid,"xy10Work",NhlpdfWorkstationClass,
                  NhlDEFAULT_APP,rlist);
    }
/*
 * Open the netCDF file.
 */
    sprintf( filename, "%s/cdf/%s", dir, file );
/*
 * Open the netCDF file.
 */
    ncid = ncopen(filename,NC_NOWRITE);
/*
 * Get the record length and dimension name.
 */
    ncinquire(ncid, &ndims, &nvars, &ngatts, &recid);
    ncdiminq(ncid, recid, recname, &rec_len);
/*
 * Malloc space for lat/lon arrays.
 */
    lat = (float *)malloc(sizeof(float)*rec_len);
    lon = (float *)malloc(sizeof(float)*rec_len);
    if (lat == NULL || lon == NULL ) {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
                  "Unable to malloc space for lat/lon arrays");
        exit(1);
    }
/*
 * Get the ids of the lat/lon arrays.
 */
    latid = ncvarid(ncid,"lat");
    lonid = ncvarid(ncid,"lon");
/*
 * Get lat/lon data values.
 */
    start[0] = 0; count[0] = rec_len;
    ncvarget(ncid,latid,(long const *)start,(long const *)count,lat);
    ncvarget(ncid,lonid,(long const *)start,(long const *)count,lon);
/*
 * Close the netCDF file.
 */
    ncclose(ncid);
/*
 * Throw out values that may be incorrect.
 */ 
    for( i = 0; i < rec_len; i++ ) {
        if( lat[i] <  -90. || lat[i] >  90.) lat[i] = special_value;
        if( lon[i] < -180. || lon[i] > 180.) lon[i] = special_value;
    }
/*
 * Define the Data object.
 */
    NhlRLClear(rlist);
    NhlRLSetFloatArray(rlist,NhlNcaXArray,lon,rec_len);
    NhlRLSetFloatArray(rlist,NhlNcaYArray,lat,rec_len);
    NhlCreate(&dataid,"xyData",NhlcoordArraysClass,
                  NhlDEFAULT_APP,rlist);
/*
 * The id for this Data object is now the resource value for
 * "xyCoordData".  Tweak some XyPlot resources in the resource file
 * ("xy10.res").
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNxyCoordData,dataid);
    NhlCreate(&xyid1,"xyPlot1",NhlxyPlotClass,xworkid,rlist);
/*
 * Plot all of the station ids.
 */
    NhlDraw(xyid1);
    NhlFrame(xworkid);
/*
 * Create a second XyPlot object with a different name so we can
 * change the "tr" resources to limit the plot within the mainland
 * United States.
 */
    NhlRLClear(rlist);
    NhlRLSetInteger(rlist,NhlNxyCoordData,dataid);
    NhlCreate(&xyid2,"xyPlot2",NhlxyPlotClass,xworkid,rlist);
/*
 * Plot station ids over mainland United States only.  This is done
 * using resources in the "xy10.res" resource file.
 */
    NhlDraw(xyid2);
    NhlFrame(xworkid);
/*
 * Create two MapPlots, one of the world and one of the United States 
 * (projection parameters are set up in the "xy10.res" resource file).
 */
    NhlCreate(&mpid1,"mpPlot1",NhlmapPlotClass,xworkid,0);

    NhlCreate(&mpid2,"mpPlot2",NhlmapPlotClass,xworkid,0);
/*
 * Draw the two map plots. 
 */
    NhlDraw(mpid1);
    NhlFrame(xworkid);

    NhlDraw(mpid2);
    NhlFrame(xworkid);
/*
 * Overlay the first XyPlot object on the first MapPlot object.  This
 * will plot station ids over the world.
 */
    NhlAddOverlay(mpid1,xyid1,-1);
    NhlDraw(mpid1);
    NhlFrame(xworkid);
/*
 * Overlay the second XyPlot object on the second MapPlot object.  This
 * will plot station ids over mainland United States only.
 */
    NhlAddOverlay(mpid2,xyid2,-1);
    NhlDraw(mpid2);
    NhlFrame(xworkid);
/*
 * NhlDestroy destroys the given id and all of its children so
 * destroying "appid" will destroy "xworkid" which will also destroy
 * "xyid".
 */
    NhlRLDestroy(rlist);
    NhlDestroy(xworkid);
    NhlDestroy(appid);
/*
 * Restores state.
 */
    NhlClose();
    exit(0);
}
