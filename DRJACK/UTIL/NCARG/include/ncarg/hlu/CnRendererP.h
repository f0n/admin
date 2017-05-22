/*
 *      $Id: CnRendererP.h,v 1.1 2004/03/11 02:00:16 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		CnRendererP.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Sep 23 17:23:23 MDT 2003
 *
 *	Description:	CnRenderer plot object private header file
 */

#ifndef _NCNRENDERERP_h
#define _NCNRENDERERP_h

#include <ncarg/hlu/CnRenderer.h>
#include <ncarg/hlu/ContourPlotP.h>


typedef struct _NhlCnRendererLayerPart {

	int foo;

} NhlCnRendererLayerPart;


typedef struct _NhlCnRendererLayerRec {
	NhlObjLayerPart		base;
	NhlCnRendererLayerPart	cnrenderer;
} NhlCnRendererLayerRec;

typedef NhlErrorTypes (*NhlContourRender)(
#if     NhlNeedProto
        NhlLayer                instance,
        NhlContourPlotLayer     cnl,
	NhlDrawOrder            order,
	NhlString		entry_name
#endif
);

typedef struct NhlCnRendererClassPart{
	NhlContourRender	render;
} NhlCnRendererClassPart;


typedef struct _NhlCnRendererClassRec{
	NhlObjClassPart		base_class;
	NhlCnRendererClassPart	cnrenderer_class;
} NhlCnRendererClassRec;

typedef struct _NhlCnRendererClassRec	*NhlCnRendererClass;
typedef struct _NhlCnRendererLayerRec		*NhlCnRendererLayer;

extern NhlCnRendererClassRec	NhlcnRendererClassRec;

#endif  /* _NCNRENDERERP_h */
