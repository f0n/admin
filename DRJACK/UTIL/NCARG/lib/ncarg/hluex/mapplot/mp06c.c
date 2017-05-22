/*
 *      $Id: mp06c.c,v 1.2 2003/03/04 17:17:57 grubin Exp $
 */
/***********************************************************************
*                                                                      *
*                Copyright (C)  1999                                   *
*        University Corporation for Atmospheric Research               *
*                All Rights Reserved                                   *
*                                                                      *
***********************************************************************/
/*
 *  File:       mp06c.c
 *
 *  Author:     Mary Haley
 *          National Center for Atmospheric Research
 *          PO 3000, Boulder, Colorado
 *
 *  Date:       Mon Dec 13 15:35:42 MST 1999
 *
 *   Description:  Shows how to draw county lines in the US.
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

/*
 * List of Florida counties.
 */
char *florida_counties[69] = {
     "Florida . Alachua", "Florida . Baker", "Florida . Bay", 
     "Florida . Bradford", "Florida . Brevard", "Florida . Broward",    
     "Florida . Calhoun", "Florida . Charlotte", "Florida . Citrus", 
     "Florida . Clay", "Florida . Collier", "Florida . Columbia", 
     "Florida . De Soto", "Florida . Dixie", "Florida . Duval", 
     "Florida . Escambia", "Florida . Flagler", "Florida . Franklin",
     "Florida . Gadsden", "Florida . Gilchrist", "Florida . Glades", 
     "Florida . Gulf", "Florida . Hamilton", "Florida . Hardee", 
     "Florida . Hendry", "Florida . Hernando", "Florida . Highlands", 
     "Florida . Hillsborough", "Florida . Holmes", 
     "Florida . Indian River", "Florida . Jackson", "Florida . Jefferson", 
     "Florida . Keys", "Florida . Lafayette", "Florida . Lake", 
     "Florida . Lee", "Florida . Leon", "Florida . Levy", 
     "Florida . Liberty", "Florida . Madison", "Florida . Manatee", 
     "Florida . Marion", "Florida . Martin", "Florida . Miami-Dade", 
     "Florida . Monroe", "Florida . Nassau", "Florida . Okaloosa", 
     "Florida . Okeechobee", "Florida . Orange", "Florida . Osceola", 
     "Florida . Palm Beach", "Florida . Pasco", "Florida . Pinellas", 
     "Florida . Polk", "Florida . Putnam", "Florida . Saint Johns", 
     "Florida . Saint Lucie", "Florida . Saint Vincent Island", 
     "Florida . Santa Rosa", "Florida . Sarasota", "Florida . Seminole", 
     "Florida . Sumter", "Florida . Suwannee", "Florida . Taylor", 
     "Florida . Union", "Florida . Volusia", "Florida . Wakulla", 
     "Florida . Walton", "Florida . Washington"}; 

main(int argc, char *argv[])
{
    int wid,appid,mapid;
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
    NhlCreate(&appid,"mp06",NhlappClass,NhlDEFAULT_APP,rlist);

    if (NCGM) {
/*
 * Create a meta file workstation
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkMetaName,"./mp06c.ncgm");
        NhlCreate(&wid,"mp06Work",
                  NhlncgmWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (X11) {
/*
 * Create an X workstation
 */
        NhlRLClear(rlist);
        NhlRLSetInteger(rlist,NhlNwkPause,True);
        NhlCreate(&wid,"mp06Work",NhlxWorkstationClass,appid,rlist);
    }
    else if (PS) {
/*
 * Create a PS workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPSFileName,"./mp06c.ps");
        NhlCreate(&wid,"mp06Work",
                  NhlpsWorkstationClass,NhlDEFAULT_APP,rlist);
    }
    else if (PDF) {
/*
 * Create a PDF workstation.
 */
        NhlRLClear(rlist);
        NhlRLSetString(rlist,NhlNwkPDFFileName,"./mp06c.pdf");
        NhlCreate(&wid,"mp06Work",
                  NhlpdfWorkstationClass,NhlDEFAULT_APP,rlist);
    }
/*
 * Create and draw a map with all mainland US counties outlined.
 */
	NhlRLClear(rlist);
    NhlRLSetFloat(rlist,NhlNvpWidthF,  0.90);
    NhlRLSetFloat(rlist,NhlNvpHeightF, 0.90);
    NhlRLSetFloat(rlist,NhlNvpXF,      0.05);
    NhlRLSetFloat(rlist,NhlNvpYF,      0.95);

    NhlRLSetString(rlist,NhlNmpOutlineBoundarySets, "AllBoundaries");

    NhlRLSetFloat(rlist,NhlNmpMinLatF,   25.);
    NhlRLSetFloat(rlist,NhlNmpMaxLatF,   50.);
    NhlRLSetFloat(rlist,NhlNmpMinLonF, -130.);
    NhlRLSetFloat(rlist,NhlNmpMaxLonF,  -60. );

	NhlRLSetString(rlist,NhlNtiMainString,":F22:US with all counties outlined");
	NhlCreate(&mapid,"mp06",NhlmapPlotClass,wid,rlist);

    NhlDraw(mapid);
    NhlFrame(wid);
/*
 * Draw all counties in the United States that have the name
 * "Adams".
 */
	NhlRLClear(rlist);
    NhlRLSetString(rlist,NhlNmpOutlineBoundarySets,"GeophysicalAndUSStates");
    NhlRLSetString(rlist,NhlNmpOutlineSpecifiers,"Adams");
	NhlRLSetString(rlist,NhlNtiMainString,":F22:US with Adams counties outlined");
	NhlSetValues(mapid,rlist);

	NhlDraw(mapid);
	NhlFrame(wid);
/*
 * By putting the string "Florida . " in front of each county name, only
 * those counties in Florida will get drawn. Otherwise, if any of these
 * counties existed in other states, those counties would get drawn as well.
 */
	NhlRLClear(rlist);

    NhlRLSetStringArray(rlist,NhlNmpOutlineSpecifiers,florida_counties,69);
    NhlRLSetFloat(rlist,NhlNvpWidthF,   0.90);
    NhlRLSetFloat(rlist,NhlNvpHeightF,  0.90);
    NhlRLSetFloat(rlist,NhlNvpXF,       0.05);
    NhlRLSetFloat(rlist,NhlNvpYF,       0.95);
    NhlRLSetFloat(rlist,NhlNmpMinLatF,    25.);
    NhlRLSetFloat(rlist,NhlNmpMaxLatF,    32.);
    NhlRLSetFloat(rlist,NhlNmpMinLonF,   -90.);
    NhlRLSetFloat(rlist,NhlNmpMaxLonF,   -80. );

	NhlRLSetString(rlist,NhlNtiMainString,":F22:Florida and its counties outlined");
	NhlSetValues(mapid,rlist);

	NhlDraw(mapid);
	NhlFrame(wid);

/*
 * Destroy the objects created, close the HLU library and exit.
 */
    NhlDestroy(wid);
    NhlClose();
    exit(0);
}
