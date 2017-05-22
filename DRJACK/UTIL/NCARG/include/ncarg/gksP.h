/*
 *      $Id: gksP.h,v 1.7 2004/03/20 00:06:54 dbrown Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software          *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		gksP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Mar 8 19:18:41 MST 1996
 *
 *	Description:	Private GKS library interface.  This interface
 *			allows "C" native data types to be passed into
 *			the c drivers using the gescape function.  This
 *			interface should not be used directly, but only
 *			by the gescape function in libncarg_gksC.
 *
 *			There are no "public" interfaces defined in this file.
 */
#ifndef	_NGKSP_
#define	_NGKSP_

#define	NGESC_CNATIVE	-1450

#define	NGC_XGETXPIX	1
#define	NGC_XFREECI	2
#define NGC_XALLOCCOLOR	3
#define NGC_XSIZECHG	4
#define NGC_XWINCONFIG	5
#define NGC_PIXCONFIG	6

typedef struct {
	int		type;
	int		work_id;
	unsigned long	gksci;
	unsigned long	xpixnum;	/* return */
} _NGCXGetXPix;

typedef struct {
	int		type;
	int		work_id;
	unsigned long	gksci;
} _NGCXFreeCi;

typedef void (*_NGCXAllocColorProc)(
	void	*cref,
	void	*color_def	/* really (XColor*) */
);

typedef void (*_NGCXFreeColorsProc)(
	void		*cref,
	unsigned long	*pixels,
	int		npixels
);

typedef struct {
	int			type;
	int			work_id;
	_NGCXAllocColorProc	xalloc_color;
	_NGCXFreeColorsProc	xfree_colors;
	void			*cref;
} _NGCXAllocColor;

typedef void (*_NGCXGetSizeProc)(
	void		*closure,
	unsigned long	size
);

typedef struct {
	int			type;
	int			work_id;
	_NGCXGetSizeProc	xget_size;
	void			*closure;
} _NGCXGetSizeChg;

typedef struct {
	int			type;
	int			work_id;
	int			x;
	int			y;
	int			width;
	int			height;
	char			*title;
	char			*icon_title;
} _NGCXWinConfig;

typedef struct {
	int			type;
	int			work_id;
	int			width;
	int			height;
	int                     format;
	char                    *filename;
} _NGCPixConfig;

typedef struct {
	int		type;
	int		work_id;
} _NGCAny;

typedef union _NGCescapeRec_ {
	int		type;
	_NGCAny		any;
	_NGCXGetXPix	xgetxpix;
	_NGCXFreeCi	xfreeci;
	_NGCXAllocColor	xalloccolor;
	_NGCXGetSizeChg	xgetsizechg;
	_NGCXWinConfig	xwinconfig;
	_NGCPixConfig   pixconfig;
} _NGCesc;

/*
 * This function should only be called from libncarg_gksC(s_gesc.c).
 */
int _NGCescape(
#ifdef NeedFuncProto
	int	func_id,
	_NGCesc	*cesc
#endif
);

/*
 * This function should only be called from individual output drivers.
 */
_NGCesc *
_NGGetCEscInit(
#ifdef	NeedFuncProto
	void
#endif
);

#endif	/* _NGKSP_ */
