/*
 *      $Id: CnTriMeshRendererP.h,v 1.2 2004/07/23 21:24:54 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		CnTriMeshRendererP.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 2 15:01:59 MDT 1992
 *
 *	Description:	CnTriMeshRenderer plot object private header file
 */

#ifndef _NCNTRIMESHRENDERERERP_h
#define _NCNTRIMESHRENDERERERP_h

#include <ncarg/hlu/CnTriMeshRenderer.h>
#include <ncarg/hlu/CnRendererP.h>

extern void _NhlSetCnl(
#if	NhlNeedProto
	NhlContourPlotLayer cnl
#endif
);

typedef struct _NhlCnTriMeshRendererLayerPart {
	float *rpnt;
	int *iedg;
	int *itri;
	int  npnt;
	int nedg;
	int ntri;
	NhlBoolean ezmap;
	int update_mode;
} NhlCnTriMeshRendererLayerPart;


typedef struct _NhlCnTriMeshRendererLayerRec {
	NhlObjLayerPart			base;
	NhlCnRendererLayerPart  	cnrenderer;
	NhlCnTriMeshRendererLayerPart	cntrimeshrenderer;
} NhlCnTriMeshRendererLayerRec;


typedef struct NhlCnTriMeshRendererClassPart{
	int foo;
} NhlCnTriMeshRendererClassPart;


typedef struct _NhlCnTriMeshRendererClassRec{
	NhlObjClassPart			base_class;
	NhlCnRendererClassPart		cnrenderer_class;
	NhlCnTriMeshRendererClassPart	cntrimeshrenderer_class;
} NhlCnTriMeshRendererClassRec;

typedef struct _NhlCnTriMeshRendererClassRec	*NhlCnTriMeshRendererClass;
typedef struct _NhlCnTriMeshRendererLayerRec	*NhlCnTriMeshRendererLayer;

extern NhlCnTriMeshRendererClassRec	NhlcnTriMeshRendererClassRec;

#endif  /* _NCNTRIMESHRENDERERP_h */
