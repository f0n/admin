/*
 *      $Id: mp07c.c,v 1.2 2003/03/04 17:17:57 grubin Exp $
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  2002                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *  File:       mp07c.c
 *
 *  Author:     Mary Haley
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *  Date:       Mon Mar  4 14:34:48 MST 2002
 *
 *  Description: Shows how to draw high-resolution coastlines using the
 *  RANGS (Regionally Accessible Nested Global Shorelines), developed
 *  by Rainer Feistel from Wessel and Smith's GSHHS (Global
 *  Self-consistent Hierarchical High-resolution Shoreline) database.
 *  To access the RANGS/GSHHS database, you must first download it from
 *  Rainer Feistel's web site at
 *  http://www.io-warnemuende.de/homepages/rfeistel/index.html.  Right
 *  before the section entitled "Some WWW Links", you should see a
 *  little table with ten *.zip files to download:
 *
 *    rangs(0).zip         gshhs(0).zip
 *    rangs(1).zip         gshhs(1).zip
 *    rangs(2).zip         gshhs(2).zip
 *    rangs(3).zip         gshhs(3).zip
 *    rangs(4).zip         gshhs(4).zip
 *
 *  You must download all ten of these files, unzip them, and either
 *  put them in the default directory
 *  "$NCARG_ROOT/lib/ncarg/database/rangs", or put them somewhere else
 *  and set the environment variable NCARG_RANGS to this directory. The
 *  files take up about 140 megabytes, unzipped. Once you have the
 *  files in the appropriate location, then set the mpDataBaseVersion
 *  resource to "RANGS_GSHHS" to create maps using this database. You
 *  should not use this database to plot maximal area plots, because 1)
 *  you will get horizontal lines through your plot, and 2) it takes a
 *   long time.
 *
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/gks.h>
#include <ncarg/ncargC.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/PDFWorkstation.h>
#include <ncarg/hlu/XWorkstation.h>
#include <ncarg/hlu/MapPlot.h>

main(int argc, char *argv[])
{
    int wks,appid,mapid;
    int i, rlist;
    int NCGM=0, X11=1, PS=0, PDF=0;
/*
 * Initialize the high level utility library
 */
    NhlInitialize();
    rlist = NhlRLCreate(NhlSETRL);
/*
 * Create an application object.
 */
    NhlRLClear(rlist);
    NhlCreate(&appid,"mp07",NhlappClass,NhlDEFAULT_APP,rlist);

    if (NCGM) {
/*
 * Create a meta file workstation
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./mp07c.ncgm");
        NhlCreate(&wks,"mp07Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (X11) {
/*
 * Create an X workstation
 */
        NhlRLClear(rlist);
        NhlCreate(&wks,"mp07Work",NhlxWorkstationClass,appid,rlist);
    }
    else if (PS) {
/*
 * Create a PS workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./mp07c.ps");
        NhlCreate(&wks,"mp07Work",
                  NhlpsWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (PDF) {
/*
 * Create a PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"./mp07c.pdf");
        NhlCreate(&wks,"mp07Work",
                  NhlpdfWorkstationClass,NhlDEFAULT_APP,rlist);
    }
/*
 * Create and draw a map with all mainland US counties outlined.
 */
    NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpWidthF,  0.80);
    NhlRLSetFloat(rlist,NhlNvpHeightF, 0.80);
    NhlRLSetFloat(rlist,NhlNvpXF,      0.15);
    NhlRLSetFloat(rlist,NhlNvpYF,      0.90);

    NhlRLSetString(rlist,NhlNmpProjection,"CylindricalEquidistant");
/*
 * "LowRes" is the default database, also sometimes known
 * as "Ncarg4_0".
 *
 */
    NhlRLSetString(rlist,NhlNmpDataBaseVersion, "LowRes");
/*
 * Zoom in on part of the map.
 */
    NhlRLSetString(rlist,NhlNmpLimitMode,"LatLon");
    NhlRLSetFloat(rlist,NhlNmpMinLatF,   30.);
    NhlRLSetFloat(rlist,NhlNmpMaxLatF,   60.);
    NhlRLSetFloat(rlist,NhlNmpMinLonF,  -15.);
    NhlRLSetFloat(rlist,NhlNmpMaxLonF,   15. );
/*
 * Turn on the labeling of lat/lon lines.
 */
    NhlRLSetString(rlist,NhlNpmTickMarkDisplayMode,"Always");
     
    NhlCreate(&mapid,"mp07",NhlmapPlotClass,wks,rlist);
/*
 * Draw map and advance the frame.
 */
    NhlDraw(mapid);
    NhlFrame(wks);
/*
 * Set the resource indicating you want to use the high-resolution
 * RANGS/GSHHS database.
 */
    NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNmpDataBaseVersion,"RANGS_GSHHS");
    NhlSetValues(mapid,rlist);

    NhlDraw(mapid);
    NhlFrame(wks);
/*
 * Destroy the objects created, close the HLU library and exit.
 */
    NhlDestroy(wks);
    NhlClose();
    exit(0);
}
