;
;      $Id: gsn_code.ncl,v 1.1.1.1 2002/04/25 02:18:07 weiwang Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                      ;
;                Copyright (C)  1998                                   ;
;        University Corporation for Atmospheric Research               ;
;                All Rights Reserved                                   ;
;                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;  File:       gsn_code.ncl
;;
;;  Author:     Mary Haley
;;          National Center for Atmospheric Research
;;          PO 3000, Boulder, Colorado
;;
;;  Date:       Sat Apr 11 12:42:53 MST 1998
;;
;;  Description: This script defines all of the basic plotting and
;;               miscellaneous functions and procedures used in the 
;;               examples in the "Getting started using NCL" documention.
;;               The URL for this document is:
;;
;;                   http://ngwww.ucar.edu/ngdoc/ng/ug/ncl/gsun/
;;
;;               To use the functions and procedures in this script,
;;               you must have the line:
;;
;;                   load "gsn_code.ncl"
;; 
;;               at the top of your NCL script, before the begin statement.
;;

;***********************************************************************;
; For every function and procedure defined in this script, undefine it  ;
; with a call to "undef" so it doesn't clash with other functions and   ;
; procedures with the same name.                                        ;
;***********************************************************************;

undef("smooth92d")
undef("smooth93d")
undef("hsv2rgb")
undef("set_attr")
undef("get_res_eq")
undef("get_res_ne")
undef("get_res_value")
undef("spread_colors")
undef("check_for_irreg2loglin")
undef("gsnp_turn_off_tickmarks")
undef("gsnp_point_tickmarks_outward")
undef("gsnp_uniform_tickmark_labels")
undef("gsnp_shape_plot")
undef("gsnp_scale_plot")
undef("check_for_tickmarks_off")
undef("draw_and_frame")
undef("create_labelbar")
undef("gsn_open_ps")
undef("gsn_open_x11")
undef("gsn_open_ncgm")
undef("gsn_open_wks")
undef("gsn_define_colormap")
undef("gsn_retrieve_colormap")
undef("gsn_polygon")
undef("gsn_polygon_ndc")
undef("gsn_polyline")
undef("gsn_polyline_ndc")
undef("gsn_polymarker")
undef("gsn_polymarker_ndc")
undef("gsn_panel")
undef("gsn_contour")
undef("gsn_contour_map")
undef("gsn_labelbar_ndc")
undef("gsn_map")
undef("gsn_streamline")
undef("gsn_streamline_map")
undef("gsn_text")
undef("gsn_text_ndc")
undef("gsn_vector")
undef("gsn_vector_contour")
undef("gsn_vector_contour_map")
undef("gsn_vector_map")
undef("gsn_vector_scalar")
undef("gsn_vector_scalar_map")
undef("gsn_xy")


function smooth92d(var[*][*]:float,p[1]:float,q[1]:float)
local dims,output,coef,m,n,p4,q4,i
begin
	dims = dimsizes(var)
	
	output = new((/dims(0),dims(1)/),float)

	coef = 1 - p - q
	m = dims(0)
	n = dims(1)
	p4 = p/4.0
	q4 = q/4.0

	do i = 1, m -2
		output(i,1:n-2) = (p4)*(var( i-1, 1 : n-2 ) + var( i, 2 : n-1) + \
				  var( i+1, 1 : n-2) + var( i, 0 : n-3)) + \
				(q4)*(var(i-1, 0 : n-3 ) + var(i-1, 2 : n-1) + \
				  var( i+1, 2 : n-1) + var( i+1, 0 : n-3))
	end do

	output = output + (coef * var) 
	
	if(iscoord(var,var!0))  then
		output!0 = var!0
		output&$var!0$ = var&$var!0$
	end if

	if(iscoord(var,var!1))  then
		output!1 = var!1
		output&$var!1$ = var&$var!1$
	end if
	
	return(output)
end

function smooth93d(var[*][*][*]:float,p[1]:float,q[1]:float)
local dims,output,coef,m,n,p4,q4,i
begin
	dims = dimsizes(var)
	
	output = new((/dims(0),dims(1),dims(2)/),float)

	coef = 1 - p - q
	m = dims(1)
	n = dims(2)
	p4 = p/4.0
	q4 = q/4.0

	do i = 1, m -2
		output(:,i,1:n-2) = (p4)*(var( :,i-1, 1 : n-2 ) + var(:, i, 2 : n-1) + var( :,i+1, 1 : n-2) + var(:, i, 0 : n-3)) + \
				(q4)*(var( :,i-1, 0 : n-3 ) + var( :,i-1, 2 : n-1) + var( :,i+1, 2 : n-1) + var(:, i+1, 0 : n-3))
	end do

	output = output + (coef * var) 
	
	if(iscoord(var,var!0))  then
		output!0 = var!0
		output&$var!0$ = var&$var!0$
	end if

	if(iscoord(var,var!1))  then
		output!1 = var!1
		output&$var!1$ = var&$var!1$
	end if
	if(iscoord(var,var!2))  then
		output!2 = var!2
		output&$var!2$ = var&$var!2$
	end if
	
	return(output)
end

function hsv2rgb (h[*]:float,s[*]:float,v[*]:float)
begin
;jack - correction per http://www.ncl.ucar.edu/Support/talk_archives/2004/0114.html
; Make a backup copy of the HSV values.
  h = h_old
  s = s_old
  v = v_old 
        ; This function converts between HSV and RGB color space
        ; Input: h [0.0-360.0], s [0.0-1.0], v [0.0-1.0]
        ; Output: r [0.0-1.0], g [0.0-1.0], b [0.0-1.0]
	    r_g_b = new((/3,dimsizes(h)/),float)
	    r_g_b!0 = "rgb"
	    r_g_b!1 = "cmap_len"
 
            if (any((s .eq. 0.0).and.(h.eq.0.0.or.h.eq.360))) then
	        indexs = ind((h.eq.0.0.or.h.eq.360).and.s.eq.0.0)
	        r_g_b(:,indexs) = (/v(indexs),v(indexs),v(indexs)/)
		delete(indexs)
	    end if

            f = new(dimsizes(h),float)
            p = new(dimsizes(h),float)
            q = new(dimsizes(h),float)
            t = new(dimsizes(h),float)
            i = new(dimsizes(h),integer)
            if any(h.eq.360.0)  
		h(ind(h.eq.360.0)) = 0.0
            end if

            h = h/60.0
            i = floattoint(floor(h))
            f = h - i
            p = v*(1.0 - s)
            q = v*(1.0 - (s*f))
            t = v*(1.0 - (s*(1.0 - f)))
            if any(i.eq.0) then
		indexs = ind(i.eq.0)
		r_g_b(:,indexs) = (/v(indexs),t(indexs),p(indexs)/)
		delete(indexs)
	    end if
            if any(i.eq.1) then
		indexs = ind(i.eq.1)
		r_g_b(:,indexs) = (/q(indexs),v(indexs),p(indexs)/)
		delete(indexs)
	    end if
            if any(i.eq.2) then
		indexs = ind(i.eq.2)
		r_g_b(:,indexs) = (/p(indexs),v(indexs),t(indexs)/)
		delete(indexs)
	    end if
	    if any(i.eq.3) then
		indexs = ind(i.eq.3)
		r_g_b(:,indexs) = (/p(indexs),q(indexs),v(indexs)/)
		delete(indexs)
	    end if
	    if any(i.eq.4) then
		indexs = ind(i.eq.4)
		r_g_b(:,indexs) = (/t(indexs),p(indexs),v(indexs)/)
		delete(indexs)
	    end if
	    if any(i.eq.5) then
		indexs = ind(i.eq.5)
		r_g_b(:,indexs) = (/v(indexs),p(indexs),q(indexs)/)
		delete(indexs)
            end if
	    if(any(ismissing(r_g_b)))
		print("WARNING: Some invalid HSV values were passed to hsv2rgb")
	    end if
	    return(r_g_b(cmap_len|:,rgb|:))
end

;***********************************************************************;
; function : tofloat                                                    ;
;                x:numeric                                              ;
;***********************************************************************;
function tofloat(x:numeric)
local xf
begin
  if(typeof(x).eq."double")
    xf = doubletofloat(x)
  else
    if(isatt(x,"_FillValue"))
      xf = new(dimsizes(x),float,x@_FillValue)
    else
      xf = new(dimsizes(x),float)
      delete(xf@_FillValue)
    end if
    xf = x
  end if
  return(xf)
end

;***********************************************************************;
; Procedure : set_attr                                                  ;
;                res:logical                                            ;
;           att_name: string                                            ;
;          att_value                                                    ;
;                                                                       ;
; Add resource and its value to a resource list if it isn't already set.;
;***********************************************************************;

procedure set_attr(res:logical,att_name:string,att_value)
begin
  res = True
  if(.not.isatt(res,att_name))
    res@$att_name$  = att_value
  end if
  return
end

;***********************************************************************;
; Function : get_res_eq                                                 ;
;                res:logical                                            ;
;             prefix: string                                            ;
;                                                                       ;
; Get a list of resources that start with res_prefix.                   ;
;***********************************************************************;

function get_res_eq(res,res_prefix:string)
local i, j, ret_res, res2, attnames, res_index
begin
  ret_res = False

  if(res.and..not.any(ismissing(getvaratts(res))))
    attnames = getvaratts(res)
    res2 = stringtochar(attnames(ind(attnames.ne."_FillValue")))
;
; Only one resource set.
;
    if(dimsizes(dimsizes(res2)).eq.1)
      if(any(chartostring(res2(0:1)).eq.res_prefix))
        ret_res = True
        ret_res@$attnames$ = res@$attnames$
      end if
    else
;
; Multiple resources set. They have to be checked differently than if
; just one resource is set. 
;
      do j=0,dimsizes(res_prefix)-1
        res_index = ind(chartostring(res2(:,0:1)).eq.res_prefix(j))
        if(.not.all(ismissing(res_index)))
          ret_res = True
          do i = 0,dimsizes(res_index)-1
            ret_res@$attnames(res_index(i))$ = res@$attnames(res_index(i))$
          end do
        end if
        delete(res_index)
      end do
    end if
    delete(res2)
    delete(attnames)
  end if
  return(ret_res)
end

;***********************************************************************;
; Function : get_res_ne                                                 ;
;                res:logical                                            ;
;             prefix: string                                            ;
;                                                                       ;
; Get a list of resources that don't start with res_prefix.             ;
;***********************************************************************;

function get_res_ne(res,res_prefix:string)
local i, ret_res, res2, attnames, res_index
begin
  ret_res = False

  if(res.and..not.any(ismissing(getvaratts(res))))
    attnames = getvaratts(res)
    res2 = stringtochar(attnames(ind(attnames.ne."_FillValue")))

    if(dimsizes(dimsizes(res2)).eq.1)
      if(all(chartostring(res2(0:1)).ne.res_prefix))
        ret_res = True
        ret_res@$attnames$ = res@$attnames$
      end if
    else
      nres = dimsizes(res2(:,0))
      do i = 0,nres-1
        if(all(chartostring(res2(i,0:1)).ne.res_prefix))
          ret_res = True
          ret_res@$attnames(i)$ = res@$attnames(i)$
        end if
      end do
    end if
    delete(res2)
    delete(attnames)
  end if
  return(ret_res)
end

;***********************************************************************;
; Function : get_res_value                                              ;
;                res:logical                                            ;
;            resname:string                                             ;
;        default_val                                                    ;
;                                                                       ;
; This function checks to see if the given resource has been set, and if;
; so, it returns its value and removes it from the resource list.       ;
; Otherwise, it returns the default value which is the last argument    ;
; passed in.                                                            ;
;                                                                       ;
;***********************************************************************;
function get_res_value(res:logical,resname:string,default_val)
local return_val
begin
  if(res.and..not.any(ismissing(getvaratts(res)))) then
    if(isatt(res,resname)) then
      return_val = res@$resname$
      delete(res@$resname$)
    else
      return_val = default_val
    end if
  else
    return_val = default_val
  end if

  return(return_val)
end

;***********************************************************************;
; Function : spread_colors                                              ;
;                wks:graphic                                            ;
;               plot:graphic                                            ;
;          min_index:logical                                            ;
;          max_index:logical                                            ;
;                                                                       ;
; By default, all of the plotting routines use the first n colors from  ;
; a color map, where "n" is the number of contour or vector levels.     ;
; If "gsnSpreadColors" is set to  True, then the colors are spanned     ;
; across the whole color map. The min_index and max_index values are    ;
; used for the start and end colors.  If max_index is < 0, then this    ;
; indicates to use ncol-i, where "i" is equal to max_index.             ;
;***********************************************************************;
function spread_colors(wks:graphic,plot:graphic,min_index:integer,\
                        max_index:integer)
local ncols, lcount, fcols, icols, minix, maxix, nc, fmin, fmax, class,\
levelcountres
begin

  class = NhlClassName(plot)
  if(.not.any(class.eq.(/"contourPlotClass","logLinPlotClass",\
                         "vectorPlotClass"/)))
    print("spread_colors: invalid plot: defaulting")
    return(ispan(2,255,1))
  end if

  if (class.eq."contourPlotClass".or.class.eq."logLinPlotClass")
    levelcountres = "cnLevelCount"
  else
    levelcountres = "vcLevelCount"
  end if

  getvalues wks
    "wkColorMapLen" : ncols
  end getvalues

  if (class.eq."contourPlotClass".or.class.eq."vectorPlotClass")
    getvalues plot
      levelcountres : lcount
    end getvalues
  else
    getvalues plot@contour
      levelcountres : lcount
    end getvalues
  end if
;
; -1 indicates that max_index should be set equal to ncols - 1
; -2 indicates that max_index should be set equal to ncols - 2, etc.
;
  minix = min_index
  maxix = max_index

  if (maxix .lt. 0)
    maxix = ncols + maxix
  end if

  if (maxix .le. minix .or. minix .lt. 0)
    print("spread_colors: invalid parameters: defaulting")
    maxix = ncols - 1
    minix = 2
  end if

  minix = max((/0,minix/))
  maxix = min((/ncols - 1,maxix/))
  nc = maxix - minix + 1

  fmin = new(1,float)
  fmax = new(1,float)

  fmin = minix
  fmax = maxix
  fcols = fspan(fmin,fmax,lcount+1)
  icols = floattointeger(fcols + 0.5)

  return(icols)
end

;***********************************************************************;
; Procedure : check_for_irreg2loglin                                    ;
;                res:logical                                            ;
;            xlinear:logical                                            ;
;            ylinear:logical                                            ;
;               xlog:logical                                            ;
;               ylog:logical                                            ;
;                                                                       ;
; If any of the sf*Array or vf*Array resources are set, this puts the   ;
; plot into "irregular" mode. If you want to make any of your axes log  ;
; or linear then, you have to overlay it on a LogLin plot.              ;
;                                                                       ;
; By setting one of the resources gsn{X,Y}AxisIrregular2Linear or       ;
; gsnXAxisIrregular2Log to True, the overlay is done for you. This      ;
; procedure checks for these resources being set and sets some logical  ;
; variables accordingly.                                                ;
;***********************************************************************;

procedure check_for_irreg2loglin(res:logical,xlinear:logical,ylinear:logical,\
                                 xlog:logical,ylog:logical)
begin

  xlinear = get_res_value(res,"gsnXAxisIrregular2Linear",xlinear)
  ylinear = get_res_value(res,"gsnYAxisIrregular2Linear",ylinear)
  xlog    = get_res_value(res,"gsnXAxisIrregular2Log",xlog)
  ylog    = get_res_value(res,"gsnYAxisIrregular2Log",ylog)

  if(ylog.and.ylinear)
    print("Error: You cannot set both gsnYAxisIrregular2Log")
    print("and gsnYAxisIrregular2Linear to True.")
    exit
  end if

  if(xlog.and.xlinear)
     print("Error: You cannot set both gsnXAxisIrregular2Log")
     print("and gsnXAxisIrregular2Linear to True.")
     exit
  end if

  return
end

;***********************************************************************;
; Procedure : gsnp_turn_off_tickmarks                                   ;
;                res:logical                                            ;
;                                                                       ;
; By default, tickmarks are drawn on all plots that aren't overlaid on  ;
; a map. If gsnTickMarksOn is set to False, then this turns off the     ;
; drawing of tick marks.  This procedure just sets the resources        ;
; necessary in order to turn off tick marks.                            ;
;***********************************************************************;

procedure gsnp_turn_off_tickmarks(res:logical)
begin
  set_attr(res,"tmXBBorderOn",False )
  set_attr(res,"tmXBOn",      False)
  set_attr(res,"tmXTBorderOn",False)
  set_attr(res,"tmXTOn",      False)
  set_attr(res,"tmYLBorderOn",False)
  set_attr(res,"tmYLOn",      False)
  set_attr(res,"tmYRBorderOn",False)
  set_attr(res,"tmYROn",      False)
end

;***********************************************************************;
; Procedure : gsnp_point_tickmarks_outward                              ;
;              plot:object                                              ;
;               res:logical                                             ;
;              x_major_length:numeric                                   ;
;              y_major_length:numeric                                   ;
;              x_minor_length:numeric                                   ;
;              y_minor_length:numeric                                   ;
;              major_length:numeric                                     ;
;              minor_length:numeric                                     ;
;                                                                       ;
; By default, tickmarks are drawn pointing inwards.  This procedure     ;
; makes them point out. This procedure also sets the major and/or minor ;
; tickmarks on both axes to be the same length if the major and/or minor;
; tickmarks lengths are != 0.                                           ;
;***********************************************************************;

procedure gsnp_point_tickmarks_outward(plot:graphic,res:logical, \
                              x_major_length, y_major_length, \
                              x_minor_length, y_minor_length, \
                              major_length, minor_length)
local tmres
begin
  if(major_length.lt.0.)
    getvalues plot
      "tmXBMajorLengthF"   : x_major_length
      "tmYLMajorLengthF"   : y_major_length
    end getvalues
    major_length = min((/x_major_length,y_major_length/))
    if(x_major_length.gt.0..and.y_major_length.gt.0.)
      x_major_length = min((/x_major_length,y_major_length/))
      y_major_length = x_major_length
    end if
  else
    if(x_major_length.gt.0.)
      x_major_length = major_length
    end if
    if(y_major_length.gt.0.)
      y_major_length = major_length
    end if
  end if

  if(minor_length.lt.0.)
    getvalues plot
      "tmXBMinorLengthF"        : x_minor_length
      "tmYLMinorLengthF"        : y_minor_length
    end getvalues
    if(x_minor_length.gt.0..and.y_minor_length.gt.0.)
      x_minor_length = min((/x_minor_length,y_minor_length/))
      y_minor_length = x_minor_length
    end if
  else
    if(x_minor_length.gt.0.)
      x_minor_length = minor_length
    end if
    if(y_minor_length.gt.0.)
      y_minor_length = minor_length
    end if
  end if

  tmres = res
  tmres = True
  set_attr(tmres,"tmXBMajorLengthF"        , x_major_length)
  set_attr(tmres,"tmXBMajorOutwardLengthF" , x_major_length)
  set_attr(tmres,"tmXBMinorLengthF"        , x_minor_length)
  set_attr(tmres,"tmXBMinorOutwardLengthF" , x_minor_length)
  set_attr(tmres,"tmXTMajorLengthF"        , x_major_length)
  set_attr(tmres,"tmXTMajorOutwardLengthF" , x_major_length)
  set_attr(tmres,"tmXTMinorLengthF"        , x_minor_length)
  set_attr(tmres,"tmXTMinorOutwardLengthF" , x_minor_length)
  set_attr(tmres,"tmYLMajorLengthF"        , y_major_length)
  set_attr(tmres,"tmYLMajorOutwardLengthF" , y_major_length)
  set_attr(tmres,"tmYLMinorLengthF"        , y_minor_length)
  set_attr(tmres,"tmYLMinorOutwardLengthF" , y_minor_length)
  set_attr(tmres,"tmYRMajorLengthF"        , y_major_length)
  set_attr(tmres,"tmYRMajorOutwardLengthF" , y_major_length)
  set_attr(tmres,"tmYRMinorLengthF"        , y_minor_length)
  set_attr(tmres,"tmYRMinorOutwardLengthF" , y_minor_length)

  if(tmres.and..not.any(ismissing(getvaratts(tmres))))
    attsetvalues(plot,tmres)
  end if

  return
end

;***********************************************************************;
; Procedure : gsnp_uniform_tickmark_labels                              ;
;              plot:object                                              ;
;               res:logical                                             ;
;              font_height                                              ;
;                                                                       ;
; This procedure makes the tickmark labels the same font height on both ;
; axes. If font_height <= 0., then a uniform font height is calculated. ;
;***********************************************************************;

procedure gsnp_uniform_tickmark_labels(plot:graphic,res:logical, \
                                       font_height)
local xbfont, ylfont, tmres
begin

; Get tickmark labels sizes

  if(font_height.le.0)
    getvalues plot
      "tmXBLabelFontHeightF" : xbfont
      "tmYLLabelFontHeightF" : ylfont
    end getvalues
    font_height = min((/xbfont,ylfont/))
  end if

; Make tickmark label sizes the same.

  tmres = res
  tmres = True

  set_attr(tmres,"tmXBLabelFontHeightF" , font_height)
  set_attr(tmres,"tmYLLabelFontHeightF" , font_height)
  set_attr(tmres,"tmXTLabelFontHeightF" , font_height)
  set_attr(tmres,"tmYRLabelFontHeightF" , font_height)

  if(tmres.and..not.any(ismissing(getvaratts(tmres))))
    attsetvalues(plot,tmres)
  end if

  return
end

;***********************************************************************;
; Procedure : gsnp_shape_plot                                           ;
;              plot:graphic                                             ;
;                                                                       ;
; If gsnShape is set to True, then the plot is scaled such that the X   ;
; and Y axes are proportional to each other.                            ;
;***********************************************************************;

procedure gsnp_shape_plot(plot:graphic)
local xf, yf, width, height, trxmin, trxmax, trymin, trymax, xrange, yrange, \
new_xf, new_yf, new_width, new_height
begin
  getvalues plot
    "vpXF"      : xf
    "vpYF"      : yf
    "vpWidthF"  : width
    "vpHeightF" : height
    "trXMinF"   : trxmin
    "trXMaxF"   : trxmax
    "trYMinF"   : trymin
    "trYMaxF"   : trymax
  end getvalues

  xrange = trxmax - trxmin
  yrange = trymax - trymin

  if(xrange.lt.yrange)
    new_width  = width * (xrange/yrange)
    new_height = height
    new_xf     = xf + 0.5*(width-new_width)
    new_yf     = yf
  else
    new_height = height * (yrange/xrange)
    new_width  = width
    new_yf     = yf - 0.5*(height-new_height)
    new_xf     = xf
  end if

  setvalues plot
    "vpXF"      : new_xf
    "vpYF"      : new_yf
    "vpWidthF"  : new_width
    "vpHeightF" : new_height
  end setvalues

;;; JACK - print viewport, data values - THIS *NOT* USED BY WRF MAP,CONTOUR,LINE PLOTTING (vector,stream untested)
; print("gsn_code.ncl VIEWPORT: "+new_width+"x"+new_height+"+"+new_xf+"+"+new_yf+" " )
; print("gsn_code.ncl DATA RANGE: "+trxmin+"-"+trxmax+" "+trymin+"+"+trymax+" " )

  return
end

;***********************************************************************;
; Procedure : gsnp_scale_plot                                           ;
;              plot:graphic                                             ;
;                                                                       ;
; If gsnScale is set to True, then the plot is scaled such the tickmarks;
; and tickmark labels are the same size on both axes.                   ;
;***********************************************************************;

procedure gsnp_scale_plot(plot:graphic)
local xfont, yfont, xbfont, xlength, xmlength, ylfont, ylength, ymlength
begin
  getvalues plot
    "tiXAxisFontHeightF"   : xfont
    "tiYAxisFontHeightF"   : yfont
    "tmXBLabelFontHeightF" : xbfont
    "tmXBMajorLengthF"     : xlength
    "tmXBMinorLengthF"     : xmlength
    "tmYLLabelFontHeightF" : ylfont
    "tmYLMajorLengthF"     : ylength
    "tmYLMinorLengthF"     : ymlength
  end getvalues

  if(xlength.ne.0..and.ylength.ne.0.)
    major_length = (ylength+xlength)/2. 
    xlength = major_length
    ylength = major_length
  end if

  if(xmlength.ne.0..and.ymlength.ne.0.)
    minor_length = (ymlength+xmlength)/2. 
    xmlength = minor_length
    ymlength = minor_length
  end if

  setvalues plot
    "tiXAxisFontHeightF"   : (xfont+yfont)/2.
    "tiYAxisFontHeightF"   : (xfont+yfont)/2.
    "tmXBLabelFontHeightF" : (xbfont+ylfont)/2.
    "tmXBMajorLengthF"     : xlength
    "tmXBMinorLengthF"     : xmlength
    "tmYLLabelFontHeightF" : (xbfont+ylfont)/2.
    "tmYLMajorLengthF"     : ylength
    "tmYLMinorLengthF"     : ymlength
  end setvalues
end

;***********************************************************************;
; Procedure : check_for_tickmarks_off                                   ;
;                res:logical                                            ;
;                                                                       ;
; By default, tickmarks are drawn on all plots that aren't overlaid on  ;
; a map. If gsnTickMarksOn is set to False, then this turns off the     ;
; drawing of tick marks.  This procedure checks for the setting of this ;
; resource, and then calls the routine that turns off tickmarks.        ;
;***********************************************************************;

procedure check_for_tickmarks_off(res:logical)
local ticks_ons
begin

; Check if turning tickmarks off.

  ticks_on = get_res_value(res,"gsnTickMarksOn",True)
  if(.not.ticks_on)
    gsnp_turn_off_tickmarks(res)
  end if
end


;***********************************************************************;
; Procedure : draw_and_frame                                            ;
;                wks:graphic                                            ;
;               plot:graphic                                            ;
;           calldraw:logical                                            ;
;          callframe:logical                                            ;
;                                                                       ;
; By default, all of the plotting routines will draw the plot and       ;
; advance the frame, unless the special resources gsnDraw and/or        ;
; gsnFrame are set to False. This procedure checks if these resources   ;
; had been set, and calls draw and/or frame accordingly.                ;
;***********************************************************************;

procedure draw_and_frame(wks:graphic,plot:graphic,calldraw:logical, \
                         callframe:logical)
begin
  if(calldraw)
    draw(plot)
  end if

  if(callframe)
    frame(wks)           ; advance the frame
  end if
  return
end

;***********************************************************************;
; Function : gsn_open_ncgm                                              ;
;               name : name of output cgm file                          ;
;                                                                       ;
; This function opens an NCGM output file called "<name>.ncgm" and      ;
; returns the workstation id. If "name" is an empty string, then the    ;
; NCGM is given its default name "gmeta".                               ;
;***********************************************************************;

function gsn_open_ncgm(name[1]:string)
local ncgm, res_file
begin
    if(isatt(name,"res_file"))
      res_file = name@res_file
    else
      res_file = "gsnapp"
    end if

    if(isatt(name,"wkColorMap"))
      ncgm = create res_file ncgmWorkstationClass defaultapp 
        "wkMetaName" : name
        "wkColorMap" : name@wkColorMap
      end create
    else
      ncgm = create res_file ncgmWorkstationClass defaultapp 
        "wkMetaName" : name
      end create
    end if


    return(ncgm)
end

;***********************************************************************;
; Function : gsn_open_x11                                               ;
;               name : name of X11 window                               ;
;                                                                       ;
; This function opens an X11 output window and returns the workstation  ;
; id.                                                                   ;
;***********************************************************************;

function gsn_open_x11(name[1]:string)
local window
begin
    if(isatt(name,"wkColorMap"))
      window = create name + "_x11" xWorkstationClass defaultapp
        "wkPause" : True
        "wkColorMap" : name@wkColorMap
      end create
    else
      window = create name + "_x11" xWorkstationClass defaultapp
        "wkPause" : True
      end create
    end if
    return(window)
end

;***********************************************************************;
; Function : create_labelbar                                            ;
;                wks: graphic                                           ;
;               nbox: integer                                           ;
;             colors: array                                             ;
;             labels: array                                             ;
;              lbres: logical                                           ;
;                                                                       ;
; This function creates a labelbar given a workstation, the number of   ;
; boxes, the colors and labels to use, and an optional list of          ;
; labelbar resources. By default, lbAutoManage is set to False, the     ;
; perimeter is turned off, and the fill patterns are set to solid.      ;
;***********************************************************************;
 
function create_labelbar(wks:graphic, nbox:integer, colors, labels, \
lbres:logical)
;jack - added names to local statement
local perim_on, mono_fill_pat, label_align, labelbar_object, vpxf,vpyf,vpwidthf,vpheightf,font_height,orientation
begin
;
; Set some defaults
;
  perim_on      = get_res_value(lbres,"lbPerimOn",False)
  mono_fill_pat = get_res_value(lbres,"lbMonoFillPattern",True);
  label_align   = get_res_value(lbres,"lbLabelAlignment","InteriorEdges")
  font_height   = get_res_value(lbres,"lbLabelFontHeightF",0.1)
  orientation   = get_res_value(lbres,"lbOrientation","horizontal")
  vpxf          = get_res_value(lbres,"vpXF",0.1)
  vpyf          = get_res_value(lbres,"vpYF",0.1)
  vpwidthf      = get_res_value(lbres,"vpWidthF",0.8)
  vpheightf     = get_res_value(lbres,"vpHeightF",0.3)

  labelbar_object = create "labelbar" labelBarClass wks
    "vpXF"              : vpxf
    "vpYF"              : vpyf
    "vpWidthF"          : vpwidthf
    "vpHeightF"         : vpheightf
    "lbBoxCount"        : nbox
    "lbFillColors"      : colors
    "lbLabelStrings"    : labels
    "lbOrientation"     : orientation
    "lbPerimOn"         : perim_on
    "lbLabelAlignment"  : label_align
    "lbLabelFontHeightF": font_height
    "lbMonoFillPattern" : mono_fill_pat
    "lbAutoManage"      : False
  end create

  if(lbres.and..not.any(ismissing(getvaratts(lbres))))
    attsetvalues(labelbar_object,lbres)
  end if

  return(labelbar_object)
end

;***********************************************************************;
; Function : gsn_open_ps                                                ;
;               name : name of PostScript file                          ;
;                                                                       ;
; This function opens a PostScript file called "<name>.ps" and returns  ;
; the workstation id. If "name" is an empty string, then the PostScript ;
; file is called "gmeta.ps".                                            ;
;***********************************************************************;

function gsn_open_ps(type:string,name[1]:string)
local ps, res_file
begin
    lower_x  =  36
    lower_y  = 126
    upper_x  = 576
    upper_y  = 666
    cmap     = "default"
    orient   = "portrait"
    resltn   = 1800
    res_file = "gsnapp"
    
    if(isatt(name,"res_file"))
      res_file = name@res_file
    end if

    if(isatt(type,"wkPSResolution"))
      resltn = type@wkPSResolution
    end if

    if(isatt(type,"wkOrientation"))
      orient = type@wkOrientation
    end if

    if(isatt(type,"wkDeviceLowerX"))
      lower_x = type@wkDeviceLowerX
    end if

    if(isatt(type,"wkDeviceLowerY"))
      lower_y = type@wkDeviceLowerY
    end if

    if(isatt(type,"wkDeviceUpperX"))
      upper_x = type@wkDeviceUpperX
    end if

    if(isatt(type,"wkDeviceUpperY"))
      upper_y = type@wkDeviceUpperY
    end if

    if(isatt(type,"wkColorMap"))
      delete(cmap)
      cmap = type@wkColorMap
    end if

    ps = create res_file psWorkstationClass defaultapp
      "wkColorMap"     : cmap
      "wkOrientation"  : orient
      "wkPSResolution" : resltn
      "wkPSFileName"   : name
      "wkPSFormat"     : type
      "wkDeviceLowerX" : lower_x
      "wkDeviceLowerY" : lower_y
      "wkDeviceUpperX" : upper_x
      "wkDeviceUpperY" : upper_y
    end create

    return(ps)
end

;***********************************************************************;
; Function : gsn_open_wks                                               ;
;               type : type of workstation to open                      ;
;               name : name of workstation                              ;
;                                                                       ;
; This function opens either an X11 window, an NCGM file, or a          ;
; PostScript file depending on "type", which can be "x11", "ncgm", or   ;
; "ps". If "type" is a PS file or an NCGM, then it will be named        ;
; <name>.ps or <name>.ncgm respectively. This function also looks for a ;
; resource file called "name.res". If it exists, then it loads the      ;
; resources defined in that file. This function returns the workstation ;
; id.                                                                   ;
;***********************************************************************;

function gsn_open_wks(type[1]:string,name[1]:string)
local i, wks, appusrdir, name_char, not_found, res_file, res_dir
begin
    res_dir = "./"          ; Default resource directory.
    res_file = "gsnapp"     ; Default resource file name.
;
; Parse "name" to get the directory and the file prefix.
;
    if(name.ne."") then
      name_char = stringtochar(name)
      name_len  = dimsizes(name_char)-1
      i = name_len-1      ; Start checking if a directory pathname
      not_found = True    ; was specified for the resource file.
      do while(not_found.and.i.ge.0)
        if(name_char(i).eq."/")
          res_dir  = chartostring(name_char(0:i))
          not_found = False
        end if
        i = i - 1
      end do

      res_file = chartostring(name_char(i+1:name_len-1))

      if(isatt(name,"appUsrDir").and.not_found)
        res_dir = name@appUsrDir   ; No directory specified.
      end if
    end if

    if(isatt(type,"wkMetaName"))
        ncgm_file = type@wkMetaName
    else
        ncgm_file = res_file + ".ncgm"
    end if
    ncgm_file@res_file = res_file

    if(isatt(type,"wkPSFileName"))
        ps_file = type@wkPSFileName
    else
        ps_file = res_file + "." + type
    end if

    ps_file@res_file = res_file

    x_file = res_file

;
; Check if color map being set.  The PostScript color map will get 
; passed automatically through "type".
;
    if(isatt(type,"wkColorMap"))
      ncgm_file@wkColorMap = type@wkColorMap
      x_file@wkColorMap    = type@wkColorMap
    end if

    appid = create res_file appClass defaultapp
        "appDefaultParent" : True
        "appUsrDir"        : res_dir
    end create

    if(type.eq."x11".or.type.eq."X11") then
        wks = gsn_open_x11(x_file)
    else
        if(type.eq."ps".or.type.eq."eps".or.type.eq."epsi".or.\
           type.eq."PS".or.type.eq."EPS".or.type.eq."EPSI") then
            wks = gsn_open_ps(type,res_dir+ps_file)
        else
            if(type.eq."ncgm".or.type.eq."NCGM") then
                wks = gsn_open_ncgm(res_dir+ncgm_file)
            else
                 print("Error: gsn_open_wks: "+ type + " is an illegal workstation type.")
                 exit
            end if
        end if
    end if
    wks@name = res_file
    wks@app = appid 
    return(wks)
end


;***********************************************************************;
; Procedure : gsn_polygon                                               ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                     x: 1-dimensional array of x points                ;
;                     y: 1-dimensional array of y points                ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws a filled polygon on the workstation "wks" (the   ;
; variable returned from a previous call to "gsn_open_wks") in the same ;
; data space as the data in "plotid" (returned from a previous call to  ;
; one of the gsn_* plotting functions). "x" and "y" are the x and y     ;
; locations of each point in the polygon, and should be in the same data;
; space as the data from "plotid". "resources" is an optional list of   ;
; resources.                                                            ;
;***********************************************************************;

procedure gsn_polygon(wks:graphic,plotid:graphic,x[*]:numeric,\
                      y[*]:numeric,resources:logical)
local i, gsid, attnames, plot_object, res, gs_res_index, res2, \
xf, yf, x2, y2
begin

; Retrieve graphic style object.

    getvalues wks 
        "wkDefGraphicStyleId":  gsid
    end getvalues

    gsres = get_res_eq(resources,"gs")
    if(gsres.and..not.any(ismissing(getvaratts(gsres))))
      attsetvalues(gsid,gsres)
    end if

; Draw a polygon.

    xf = tofloat(x)
    yf = tofloat(y)

    if(.not.any(ismissing(xf)).and..not.any(ismissing(yf)))
      NhlDataPolygon(plotid,gsid,xf,yf)
    else
      x2 = xf(ind(.not.ismissing(xf).and..not.ismissing(yf)))
      y2 = yf(ind(.not.ismissing(xf).and..not.ismissing(yf)))
      NhlDataPolygon(plotid,gsid,x2,y2)
    end if
end


;***********************************************************************;
; Procedure : gsn_polygon_ndc                                           ;
;                   wks: workstation object                             ;
;                     x: 1-dimensional array of x points                ;
;                     y: 1-dimensional array of y points                ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws a filled polygon on the workstation "wks" (the   ;
; variable returned from a previous call to "gsn_open_wks") in NDC      ;
; space. "x" and "y" are the x and y locations of each point in the     ;
; polygon, and "resources" is an optional list of resources.            ;
;***********************************************************************;

procedure gsn_polygon_ndc(wks:graphic,x[*]:numeric,y[*]:numeric,\
                          resources:logical)
local i, gsid, attnames, plot_object, res, gs_res_index, xf, yf, x2, y2
begin

; Retrieve graphic style object.

    getvalues wks 
        "wkDefGraphicStyleId":  gsid
    end getvalues

;
; Create a LogLinPlot that covers the entire NDC space
; to use as a drawing canvas
;
    canvas = create "canvas" logLinPlotClass wks
      "vpXF"      : 0.0
      "vpYF"      : 1.0
      "vpWidthF"  : 1.0
      "vpHeightF" : 1.0
    end create

    gsres = get_res_eq(resources,"gs")
    if(gsres.and..not.any(ismissing(getvaratts(gsres))))
      attsetvalues(gsid,gsres)
    end if

; Draw a polygon.

    xf = tofloat(x)
    yf = tofloat(y)

    if(.not.any(ismissing(xf)).and..not.any(ismissing(yf)))
      NhlNDCPolygon(canvas,gsid,xf,yf)
    else
      x2 = xf(ind(.not.ismissing(xf).and..not.ismissing(yf)))
      y2 = yf(ind(.not.ismissing(xf).and..not.ismissing(yf)))
      NhlNDCPolygon(canvas,gsid,x2,y2)
    end if
end

;***********************************************************************;
; Procedure : gsn_polyline                                              ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                     x: 1-dimensional array of x points                ;
;                     y: 1-dimensional array of y points                ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws a polyline on the workstation "wks" (the variable;
; returned from a previous call to "gsn_open_wks") in the same data     ;
; space as the data in "plotid" (returned from a previous call to one of;
; the gsn_* plotting functions). "x" and "y" are the x and y locations  ;
; of each point in the line, and should be in the same data space as the;
; data from "plotid". "resources" is an optional list of resources.     ;
;***********************************************************************;

procedure gsn_polyline(wks:graphic,plotid:graphic,x[*]:numeric,\
                       y[*]:numeric,resources:logical)
local i, gsid, attnames, plot_object, res, gs_res_index
begin

; Retrieve graphic style object.

    getvalues wks 
        "wkDefGraphicStyleId":  gsid
    end getvalues

    gsres = get_res_eq(resources,"gs")
    if(gsres.and..not.any(ismissing(getvaratts(gsres))))
      attsetvalues(gsid,gsres)
    end if

;4test: print( "gsn_polyline: gsres="+gsres )

;
; Make sure data is converted to a float, since NhlDataPolyline
; only takes floats.
;
    xf = tofloat(x)
    yf = tofloat(y)

; Draw a polyline.

    if(.not.any(ismissing(xf)).and..not.any(ismissing(yf)))
      NhlDataPolyline(plotid,gsid,xf,yf)

    else
      x2 = xf
      y2 = xf
;
; Since the data contains missing values, leave gaps where the missing
; values are located.
;
      j = 0
      do i = 0,dimsizes(x2)-1
        if(ismissing(x2(i)).or.ismissing(y2(i)))
          if(j.eq.1)    ; only one value, so draw a marker
            NhlDataPolymarker(plotid,gsid,x2(0),y2(0))
          end if
          if(j.gt.1)    ; multiple values, so draw a line
            NhlDataPolyline(plotid,gsid,x2(0:j-1),y2(0:j-1))
          end if
          j = 0
        else
          x2(j) = xf(i)   ; Neither point is missing, so store in
          y2(j) = yf(i)   ; separate arrays to draw later.
          j = j + 1
        end if
      end do
      if(j.eq.1)         
        NhlDataPolymarker(plotid,gsid,x2(0),y2(0))
        j = 0
      end if
      if(j.gt.1)         
        NhlDataPolyline(plotid,gsid,x2(0:j-1),y2(0:j-1))
        j = 0
      end if
    end if
end

;***********************************************************************;
; Procedure : gsn_polyline_ndc                                          ;
;                   wks: workstation object                             ;
;                     x: 1-dimensional array of x ndc points            ;
;                     y: 1-dimensional array of y ndc points            ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws a polyline on the workstation "wks" (the variable;
; returned from a previous call to "gsn_open_wks") in NDC space.        ;
; "x" and "y" are the x and y locations of each point in the line.      ;
; "resources" is an optional list of resources.                         ;
;***********************************************************************;

procedure gsn_polyline_ndc(wks:graphic,x[*]:numeric,y[*]:numeric,\
                           resources:logical)
local i, gsid, attnames, plot_object, res, gs_res_index, xf, yf, x2, y2
begin

; Retrieve graphic style object.

    getvalues wks 
        "wkDefGraphicStyleId":  gsid
    end getvalues
;
; Create a LogLinPlot that covers the entire NDC space
; to use as a drawing canvas
;
    canvas = create "canvas" logLinPlotClass wks
      "vpXF"      : 0.0
      "vpYF"      : 1.0
      "vpWidthF"  : 1.0
      "vpHeightF" : 1.0
    end create

    gsres = get_res_eq(resources,"gs")
    if(gsres.and..not.any(ismissing(getvaratts(gsres))))
      attsetvalues(gsid,gsres)
    end if

; Convert to float.
    xf = tofloat(x)
    yf = tofloat(y)

; Draw a polyline.
    if(.not.any(ismissing(xf)).and..not.any(ismissing(yf)))
      NhlNDCPolyline(canvas,gsid,xf,yf)
    else
;
; Since the data contains missing values, leave gaps where the missing
; values are located.
;
      x2 = new(dimsizes(x),float)
      y2 = new(dimsizes(y),float)
      j = 0
      do i = 0,dimsizes(xf)-1
        if(ismissing(xf(i)).or.ismissing(yf(i)))
          if(j.eq.1)    ; only one value, so draw a marker
            NhlNDCPolymarker(canvas,gsid,x2(0),y2(0))
          end if
          if(j.gt.1)    ; multiple values, so draw a line
            NhlNDCPolyline(canvas,gsid,x2(0:j-1),y2(0:j-1))
          end if
          j = 0
        else
          x2(j) = xf(i)   ; Neither point is missing, so store in
          y2(j) = yf(i)   ; separate arrays to draw later.
          j = j + 1
        end if
      end do
      if(j.eq.1)         
        NhlNDCPolymarker(canvas,gsid,x2(0),y2(0))
        j = 0
      end if
      if(j.gt.1)         
        NhlNDCPolyline(canvas,gsid,x2(0:j-1),y2(0:j-1))
        j = 0
      end if
    end if
end

;***********************************************************************;
; Procedure : gsn_polymarker                                            ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                     x: 1-dimensional array of x points                ;
;                     y: 1-dimensional array of y points                ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws polymarkers on the workstation "wks" (the        ;
; variable returned from a previous call to "gsn_open_wks") in the same ;
; data space as the data in "plotid" (returned from a previous call to  ;
; one of the gsn_* plotting functions). "x" and "y" are the x and y     ;
; locations of each marker, and should be in the same data space as the ;
; data from "plotid". "resources" is an optional list of resources.     ;
;***********************************************************************;

procedure gsn_polymarker(wks:graphic,plotid:graphic,x[*]:numeric,\
                         y[*]:numeric,resources:logical)
local i, gsid, attnames, plot_object, res, gs_res_index, xf, yf, x2, y2
begin

; Retrieve graphic style object.

    getvalues wks 
        "wkDefGraphicStyleId":  gsid
    end getvalues

    gsres = get_res_eq(resources,"gs")
    if(gsres.and..not.any(ismissing(getvaratts(gsres))))
      attsetvalues(gsid,gsres)
    end if

; Make sure data is float.
    xf = tofloat(x)
    yf = tofloat(y)

; Draw some polymarkers.
    if(.not.any(ismissing(xf)).and..not.any(ismissing(yf)))
      NhlDataPolymarker(plotid,gsid,xf,yf)
    else
      x2 = xf(ind(.not.ismissing(xf).and..not.ismissing(yf)))
      y2 = yf(ind(.not.ismissing(xf).and..not.ismissing(yf)))
      NhlDataPolymarker(plotid,gsid,x2,y2)
    end if
end

;***********************************************************************;
; Procedure : gsn_polymarker_ndc                                        ;
;                   wks: workstation object                             ;
;                     x: 1-dimensional array of x points                ;
;                     y: 1-dimensional array of y points                ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws polymarkers on the workstation "wks" (the        ;
; variable returned from a previous call to "gsn_open_wks") in NDC      ;
; space. "x" and "y" are the x and y locations of each marker in NDC    ;
; coordinates. "resources" is an optional list of resources.            ;
;***********************************************************************;

procedure gsn_polymarker_ndc(wks:graphic,x[*]:numeric,y[*]:numeric,\
                         resources:logical)
local i, gsid, attnames, plot_object, res, gs_res_index, xf, yf, x2, y2
begin

; Retrieve graphic style object.

    getvalues wks 
        "wkDefGraphicStyleId":  gsid
    end getvalues

;
; Create a LogLinPlot that covers the entire NDC space
; to use as a drawing canvas
;
    canvas = create "canvas" logLinPlotClass wks
      "vpXF"      : 0.0
      "vpYF"      : 1.0
      "vpWidthF"  : 1.0
      "vpHeightF" : 1.0
    end create

    gsres = get_res_eq(resources,"gs")
    if(gsres.and..not.any(ismissing(getvaratts(gsres))))
      attsetvalues(gsid,gsres)
    end if

; Make sure data is float.
    xf = tofloat(x)
    yf = tofloat(y)

; Draw some polymarkers.
    if(.not.any(ismissing(xf)).and..not.any(ismissing(yf)))
      NhlNDCPolymarker(canvas,gsid,xf,yf)
    else
      x2 = xf(ind(.not.ismissing(xf).and..not.ismissing(yf)))
      y2 = yf(ind(.not.ismissing(xf).and..not.ismissing(yf)))
      NhlNDCPolymarker(canvas,gsid,x2,y2)
    end if
end

;***********************************************************************;
; Procedure : gsn_panel                                                 ;
;                 wks: workstation object                               ;
;               plot : array of plots to put on one page.               ;
;               dims : a 2-D array indicating number of rows and columns;
;             resources: optional resources                             ;
;                                                                       ;
; This procedure takes the array of plots and draws them all on one     ;
; workstation in the configuration specified by dims.                   ;
;                                                                       ;
; For example, if you have six plots and dims is (/2,3/), then the six  ;
; plots will be drawn in 2 rows and 3 columns.                          ;
;                                                                       ;
;***********************************************************************;
procedure gsn_panel(wks:graphic,plot[*]:graphic,dims[2]:float,\
                    resources:logical )
local rows, cols, xrows, xcols, top, bottom, left, right, \
vpx, vpy, vpwidth, vpheight, bb, plot_width, plot_height, \
xwsp_perc, ywsp_perc, xwsp, ywsp, dx, dy, total_width, total_height, \
col_scale, row_scale, scale, xpos, ypos, xsp, ysp, panel_center, \
new_plot_height, new_plot_width, new_total_height, new_total_width, \
i, j, old_vpx, old_vpy, old_vpwidth, old_vpheight, callframe
begin
    rows = floattointeger(dims(0))
    cols = floattointeger(dims(1))
    xrows = dims(0)
    xcols = dims(1)
    res = resources              ; Make copy of resources
;
; Check for special resources.
; 
    panel_center   = get_res_value(res,"gsnPanelCenter",True)
    panel_labelbar = get_res_value(res,"gsnPanelLabelBar",False)
    callframe      = get_res_value(res,"gsnFrame",True)
    xwsp_perc      = get_res_value(res,"gsnPanelXWhiteSpacePercent",1.)
    ywsp_perc      = get_res_value(res,"gsnPanelYWhiteSpacePercent",1.)
    draw_boxes     = get_res_value(res,"gsnPanelBoxes",False)
    x_lft          = get_res_value(res,"gsnPanelLeft",0.)
    x_rgt          = get_res_value(res,"gsnPanelRight",1.)
    y_bot          = get_res_value(res,"gsnPanelBottom",0.)
    y_top          = get_res_value(res,"gsnPanelTop",1.)

    if(xwsp_perc.lt.0.or.xwsp_perc.ge.100.)
      print("Warning: gsn_panel: attribute gsnPanelXWhiteSpacePercent must be >= 0 and < 100.")
      print("Defaulting to 1.")
      xwsp_perc = 1.
    end if

    if(ywsp_perc.lt.0.or.ywsp_perc.ge.100.)
      print("Warning: gsn_panel: attribute gsnPanelYWhiteSpacePercent must be >= 0 and < 100.")
      print("Defaulting to 1.")
      ywsp_perc = 1.
    end if

    if(x_lft.lt.0..or.x_lft.ge.1.)
      print("Warning: gsn_panel: attribute gsnPanelLeft must be >= 0.0 and < 1.0")
      print("Defaulting to 0.")
      x_lft = 0.0
    end if

    if(x_rgt.le.0..or.x_rgt.gt.1.)
      print("Warning: gsn_panel: attribute gsnPanelRight must be > 0.0 and <= 1.0")
      print("Defaulting to 1.")
      x_rgt = 1.0
    end if

    if(y_top.le.0..or.y_top.gt.1.)
      print("Warning: gsn_panel: attribute gsnPanelTop must be > 0.0 and <= 1.0")
      print("Defaulting to 1.")
      y_top = 1.0
    end if

    if(y_bot.lt.0..or.y_bot.ge.1.)
      print("Warning: gsn_panel: attribute gsnPanelBottom must be >= 0.0 and < 1.0")
      print("Defaulting to 0.")
      y_bot = 0.0
    end if

    if(x_rgt.le.x_lft)
      print("Error: gsn_panel: attribute gsnPanelRight ("+x_rgt+") must be greater")
      print("than gsnPanelLeft ("+x_lft+").")
      exit
    end if

    if(y_top.le.y_bot)
      print("Error: gsn_panel: attribute gsnPanelTop ("+y_top+") must be greater")
      print("than gsnPanelBottom ("+y_bot+").")
      exit
    end if

;
; Make sure we have enough panels to fit all of the plots.
;
    nplots  = dimsizes(plot)      ; Total number of plots.
    npanels = rows*cols           ; Total number of panels.
    if(nplots.gt.npanels)
      print("Warning: gsn_panel: you have more plots than you have panels.")
      print("Only " + npanels + " plots will be drawn.")
    end if
;
; We assume all plots are the same size, so if we get the size of
; the first one, this should be the size of all of them.
;
    bb = NhlGetBB(plot(0))   ; Get bounding box of plot with
    top    = bb(0)           ; all of its annotations.
    bottom = bb(1)
    left   = bb(2)
    right  = bb(3)
;
; plot_width  : total width of plot with all of its annotations
; plot_height : total height of plot with all of its annotations
; total_width : plot_width plus white space on both sides
; total_height: plot_height plus white space on top and bottom
;
    plot_width  = right - left     ; Calculate total width of plot.
    plot_height = top - bottom     ; Calculate total height of plot.

    xwsp = xwsp_perc/100. * plot_width  ; White space is a percentage of total
    ywsp = ywsp_perc/100. * plot_height ; width and height.

    total_width  = 2.*xwsp + plot_width   ; Calculate total width and height
    total_height = 2.*ywsp + plot_height  ; with white space added.
;
; If we are putting a global labelbar  at the bottom, make it 2/10
; the height of the plot.
;
    if(panel_labelbar) then
      labelbar_height = 0.20 * plot_height + 2.*ywsp
      if(nplots.gt.1.and.cols.gt.1) then
        labelbar_width  = (cols-1) * (2.*xwsp + plot_width)
      else
        labelbar_width  = plot_width
      end if
    else
      labelbar_height = 0.
      labelbar_width  = 0.
    end if
;
; We want:
;
;   ncols * scale * total_width  <= x_rgt - x_lft (the viewport width)
;   nrows * scale * total_height <= y_top - y_bot (the viewport height)
;   [or scale * (nrows * total_height + labelbar_height) if a labelbar
;    is being drawn]
;
; By taking the minimum of these two, we get the scale
; factor that we need to fit all plots on a page.
;
    xrange = x_rgt - x_lft
    yrange = y_top - y_bot

    col_scale = min((/xrange/(xcols*total_width), xrange/))
    row_scale = min((/yrange/(xrows*total_height+labelbar_height),yrange/))
    scale = min((/col_scale,row_scale/))
    yrange = yrange - scale * labelbar_height

    new_plot_width  = scale*plot_width    ; Calculate new width
    new_plot_height = scale*plot_height   ; and height.

    xwsp = xwsp_perc/100. * new_plot_width   ; Calculate new white space.
    ywsp = ywsp_perc/100. * new_plot_height

    new_total_width  = 2.*xwsp + new_plot_width  ; Calculate new total width
    new_total_height = 2.*ywsp + new_plot_height ; and height w/white space.

    xsp = xrange - new_total_width*cols   ; Calculate total amt of white space
    ysp = yrange - new_total_height*rows  ; left in both X and Y directions.

    getvalues plot(0)
      "vpXF" : vpx
      "vpYF" : vpy
    end getvalues

    dx = scale * (vpx - left) ; Calculate distance from plot's left position
                              ; to its leftmost annotation
    dy = scale * (top - vpy) ; Calculate distance from plot's top position
                             ; to its topmost annotation.

    xpos = x_lft + xwsp + dx +(xsp/2.+new_total_width*ispan(0,cols-1,1))
    ypos = y_top - ywsp - dy -(ysp/2.+new_total_height*ispan(0,rows-1,1))

;
; Loop through each plot and draw it in the new scaled-down size.
;
    do i = 0,rows-1
;
; Before we add plots to a row, check to see if we have enough plots
; left to fill that row.  If not, then center the remaining plots.
;
      if(panel_center)
        num_plots_left = dimsizes(plot) - i*cols

        if(num_plots_left.lt.cols)
;
; Not enough plots to fill row, so center remaining plots rather
; than left-justifying them.
; 
          num_miss_plots = cols - num_plots_left
          if(num_miss_plots.gt.0.and.num_miss_plots.lt.cols)
            delete(xpos)
            xsp = xrange - new_total_width*(cols-num_miss_plots)
            xpos = x_lft + xwsp + dx +(xsp/2.+new_total_width*\
                   ispan(0,cols-num_miss_plots-1,1))
          end if
        end if
      end if
      do j = 0,cols-1
        if(i*cols+j .lt. dimsizes(plot))
          if(.not.ismissing(plot(i*cols+j)))
            getvalues plot(i*cols+j)
              "vpXF"      : old_vpx
              "vpYF"      : old_vpy
              "vpWidthF"  : old_vpwidth
              "vpHeightF" : old_vpheight
            end getvalues

            setvalues plot(i*cols+j)
              "vpXF"      : xpos(j)
              "vpYF"      : ypos(i)
              "vpWidthF"  : scale*old_vpwidth 
              "vpHeightF" : scale*old_vpheight
            end setvalues

            draw(plot(i*cols+j))  ; Draw the scaled down plot.

            if(panel_labelbar.or.draw_boxes) then
              bb = NhlGetBB(plot(i*cols+j)) ; Get bounding box of plot
              bot = bb(1)
              if(draw_boxes)
                top = bb(0)
                lft = bb(2)
                rgt = bb(3)
                gsn_polyline_ndc(wks,(/lft,rgt,rgt,lft,lft/), \
                                     (/bot,bot,top,top,bot/),False)
              end if
            end if

            setvalues plot(i*cols+j)
              "vpXF"      : old_vpx
              "vpYF"      : old_vpy
              "vpWidthF"  : old_vpwidth
              "vpHeightF" : old_vpheight
            end setvalues
          end if
        end if
      end do  ; end of columns
    end do    ; end of rows
;
; Check if a labelbar is to be drawn at the bottom.
;
    if(panel_labelbar) then
      lbres = get_res_eq(res,(/"lb","vp"/))  ; Get labelbar resources.
;
; "plot" can be a map, in which case the vector or contour plot overlaid
; on it will be indicated by "plot@contour" or "plot@vector"
;
      plot_type = "unknown"
      class = NhlClassName(plot)
      if(class(0).ne."contourPlotClass".and.class(0).ne."vectorPlotClass") then
        if(isatt(plot,"contour")) then
          contour_plot = plot@contour
          plot_type = "contour"
        else
          if(isatt(plot,"vector")) then
            vector_plot = plot@vector
            plot_type = "vector"
          end if
        end if
      else
        if(class(0).eq."contourPlotClass") then
          contour_plot = plot(0)
          plot_type = "contour"
        else
          vector_plot = plot(0)
          plot_type = "vector"
        end if
      end if
;
; If plot type is unknown, then we can't get labelbar information.
;
      if(plot_type.ne."unknown") then
        if(plot_type.eq."contour") then
;
; Get information on how contour plot is filled, so we can recreate 
; labelbar.
;
          getvalues contour_plot
            "cnLevels"               : levels
            "cnFillColors"           : colors
            "cnFillPatterns"         : fill_patterns
            "cnFillScales"           : fill_scales
            "cnMonoFillPattern"      : mono_fill_pat
            "cnMonoFillScale"        : mono_fill_scl
            "cnMonoFillColor"        : mono_fill_col
            "cnInfoLabelFontHeightF" : labelbar_font_height
          end getvalues
        else
;
; There are no fill patterns in VectorPlot, only solids.
;
          mono_fill_pat = True
          mono_fill_scl = True
          getvalues vector_plot
            "vcLevels"                 : levels
            "vcLevelColors"            : colors
            "vcMonoFillArrowFillColor" : mono_fill_col
            "vcRefAnnoFontHeightF"    : labelbar_font_height
          end getvalues
        end if
;
; Set labelbar height, width, and font height.
;
        labelbar_height      = scale * labelbar_height
        labelbar_width       = scale * labelbar_width
        labelbar_font_height = scale * labelbar_font_height
;
; Set some labelbar resources.
;
        lbres = True

        set_attr(lbres,"vpYF",     max ((/ywsp+labelbar_height,bot-ywsp/)))
        set_attr(lbres,"vpWidthF", labelbar_width)
        set_attr(lbres,"vpHeightF",labelbar_height)
        set_attr(lbres,"lbOrientation","Horizontal")
        set_attr(lbres,"lbLabelFontHeightF",labelbar_font_height)
        if(cols.eq.1.and.lbres@vpWidthF.le.scale*old_vpwidth)
          set_attr(lbres,"vpXF",xpos(0)+(scale*old_vpwidth - lbres@vpWidthF)/2.)
        else
          set_attr(lbres,"vpXF",(1. - lbres@vpWidthF)/2.)
        end if
;
; Check if we want different fill patterns or fill scales.  If so, we
; have to pass these on to the labelbar.
;
        set_attr(lbres,"lbMonoFillColor",mono_fill_col)
        if(.not.mono_fill_pat)
          set_attr(lbres,"lbMonoFillPattern", False)
          set_attr(lbres,"lbFillPatterns",    fill_patterns)
        end if
        if(.not.mono_fill_scl)
          set_attr(lbres,"lbMonoFillScale", False)
          set_attr(lbres,"lbFillScales",    fill_scales)
        end if
;
; Create and draw the labelbar.
;
        labelbar = create_labelbar(wks,dimsizes(colors),colors,levels,lbres)
        draw(labelbar)
      else
        print("Warning: gsn_panel: unrecognized plot type, thus unable to extract information for creating a labelbar.")
      end if
    end if

    if(callframe)
      frame(wks)   ; Advance the frame.
    end if
end

;***********************************************************************;
; Procedure : gsn_define_colormap                                       ;
;                   wks: workstation object                             ;
;                  cmap: Colormap (n x 3 array)                         ;
;                                                                       ;
; This procedure defines a color map for workstation "wks" (the         ;
; variable returned from a previous call to "gsn_open_wks") using float ;
; RGB values.                                                           ;
;***********************************************************************;

procedure gsn_define_colormap(wks:graphic, cmap)
begin
  dim_cmap = dimsizes(cmap)
  if((typeof(cmap).eq."float".and.(dimsizes(dim_cmap).ne.2.or.dim_cmap(1).ne.3)).or.\
     (typeof(cmap).eq."string".and.dimsizes(dim_cmap).ne.1))
    print("Warning: gsn_define_colormap: cmap must either be a 2-dimensional float array")
    print("dimensioned n x 3 or a 1-dimensional string array.")
  else
    setvalues wks
        "wkColorMap" : cmap
    end setvalues
  end if
end

;***********************************************************************;
; Function : gsn_retrieve_colormap                                      ;
;                   wks: workstation object                             ;
;                                                                       ;
; This function retrieves the current color map in use for workstation  ;
; "wks". "wks is the workstation id returned from a call to             ;
; gsn_open_wks.  The return variable will be an n x 3 array, where n is ;
; the number of colors, and the 3 represents the R, G, and B values.    ;
;***********************************************************************;

function gsn_retrieve_colormap(wks:graphic)
begin
    getvalues wks
        "wkColorMap" : cmap
    end getvalues

    return(cmap)
end

;***********************************************************************;
; Function : gsn_contour                                                ;
;                   wks: workstation object                             ;
;                  data: 2-dimensional data                             ;
;             resources: optional resources                             ;
;                                                                       ;
; This function creates and draws a contour plot to the workstation     ;
; "wks" (the variable returned from a previous call to "gsn_open_wks"). ;
; "data" is the 2-dimensional data to be contoured, and "resources" is  ;
; an optional list of resources. The id of the contour plot is returned.;
;***********************************************************************;

function gsn_contour(wks:graphic, data[*][*]:numeric, resources:logical )
local i, attnames, data_object, plot_object, res, sf_res_index, \
datares, cnres, llres, cn_res_index, ll_res_index, calldraw, callframe, \
force_x_linear, force_y_linear, force_x_log, force_y_log, \
trxmin, trxmax, trymin, trymax, res2, scale, shape, sprdcols
begin
    cnres     = False
    llres     = False
    res2      = resources

    force_x_linear = False
    force_y_linear = False
    force_x_log    = False
    force_y_log    = False

;;;JACK - print to indicate this routine being used
; print("    gsn_code.ncl FUNCTION gsn_contour ENTERED")

; Create the data object.

    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if

    data_object = create wksname + "_data" scalarFieldClass noparent
        "sfDataArray" : data
    end create

; Check for a missing value.

    if(isatt(data,"_FillValue")) then
        setvalues data_object
            "sfMissingValueV" :data@_FillValue
        end setvalues
    end if

; Create plot object.

    plot_object = create wksname + "_contour" contourPlotClass wks
        "cnScalarFieldData" : data_object
    end create

; Check for existence of data@long_name and use it in a title it
; it exists.

    if(isatt(data,"long_name")) then
      set_attr(res2,"tiMainString",data@long_name)
    end if

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)
    sprdcols  = get_res_value(res2,"gsnSpreadColors",False)
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)

    check_for_irreg2loglin(res2,force_x_linear,force_y_linear,\
                                force_x_log,force_y_log)
    check_for_tickmarks_off(res2)

    sfres = get_res_eq(res2,"sf")
    cnres = get_res_ne(res2,"sf")

    if(sfres.and..not.any(ismissing(getvaratts(sfres))))
      attsetvalues(data_object,sfres)
    end if
    if(cnres.and..not.any(ismissing(getvaratts(cnres))))
      attsetvalues(plot_object,cnres)
    end if
    if(sprdcols)
      cnres2 = True
      set_attr(cnres2,"cnFillColors",\
               spread_colors(wks,plot_object,min_index,max_index))
      attsetvalues(plot_object,cnres2)
    end if

    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
        llres = get_res_eq(res2,(/"tr","vp"/))
    end if
;
; If gsnShape was set to True, then resize the X or Y axis so that
; the scales are proportionally correct.
; 
    if(shape)
      gsnp_shape_plot(plot_object)
    end if
;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(plot_object)
    end if

; Check if we need to force the X or Y axis to be linear or log.
; If so, then we have to overlay it on a LogLin Plot.

    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
      overlay_plot_object = plot_object
      delete(plot_object)

      getvalues overlay_plot_object
        "trXMinF"    : trxmin
        "trXMaxF"    : trxmax
        "trYMinF"    : trymin
        "trYMaxF"    : trymax
      end getvalues

      plot_object = create wksname + "_loglin" logLinPlotClass wks
        "trXLog"  : force_x_log
        "trYLog"  : force_y_log
        "trXMinF" : trxmin
        "trXMaxF" : trxmax
        "trYMinF" : trymin
        "trYMaxF" : trymax
      end create
      if(llres.and..not.any(ismissing(getvaratts(llres))))
        attsetvalues(plot_object,llres)
      end if

      overlay(plot_object,overlay_plot_object)
      plot_object@contour = overlay_plot_object
    end if

    draw_and_frame(wks,plot_object,calldraw,callframe)

; Return plot object and data object (as attribute of plot object).

    plot_object@data = data_object
    return(plot_object)
end

;***********************************************************************;
; Function : gsn_contour_map                                            ;
;                   wks: workstation object                             ;
;                  data: 2-dimensional data                             ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a contour plot over a map plot to the ;
; workstation "wks" (the variable returned from a previous call to      ;
; "gsn_open_wks").  "data" is the 2-dimensional data to be contoured,   ;
; and "resources" is an optional list of resources. The id of the map   ;
; plot is returned.                                                     ;
;***********************************************************************;

function gsn_contour_map(wks:graphic,data[*][*]:numeric,\
                         resources:logical)
local i, attnames, data_object, contour_object, res, sf_res_index, \
cn_res_index, mp_res_index, map_object, res2, scale, shape, sprdcols
begin
    res2      = resources

;;;JACK - print to indicate this routine being used
; print("    gsn_code.ncl FUNCTION gsn_contour_map ENTERED")

; Create the data object.

    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if

    data_object = create wksname + "_data" scalarFieldClass noparent
        "sfDataArray" : data
    end create

; Check for a missing value.

    if(isatt(data,"_FillValue")) then
        setvalues data_object
            "sfMissingValueV" :data@_FillValue
        end setvalues
    end if

; Create plot object.

    contour_object = create wksname + "_contour" contourPlotClass wks
        "cnScalarFieldData" : data_object
    end create

; Check for existence of data@long_name and use it in a title it
; it exists.

    if(isatt(data,"long_name")) then
      set_attr(res2,"tiMainString",data@long_name)
    end if

; Create map object.

    map_object = create wksname + "_map" mapPlotClass wks
    end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    scale     = get_res_value(res2,"gsnScale",False)
    shape     = get_res_value(res2,"gsnShape",scale)
    sprdcols  = get_res_value(res2,"gsnSpreadColors",False)
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)

    sfres = get_res_eq(res2,"sf")
    mpres = get_res_eq(res2,(/"mp","vp"/))
    cnres = get_res_ne(res2,(/"mp","sf","vp"/))

    if(sfres.and..not.any(ismissing(getvaratts(sfres))))
      attsetvalues(data_object,sfres)
    end if
    if(mpres.and..not.any(ismissing(getvaratts(mpres))))
      attsetvalues(map_object,mpres)
    end if
    if(cnres.and..not.any(ismissing(getvaratts(cnres))))
      attsetvalues(contour_object,cnres)
    end if
    if(sprdcols)
      cnres2 = True
      set_attr(cnres2,"cnFillColors",\
               spread_colors(wks,contour_object,min_index,max_index))
      attsetvalues(contour_object,cnres2)
    end if

    overlay(map_object,contour_object)

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(contour_object)
    end if

    draw_and_frame(wks,map_object,calldraw,callframe)

; Return plot object and data object (as attribute of plot object).

    map_object@data = data_object
    map_object@contour = contour_object
    return(map_object)
end

;***********************************************************************;
; Procedure : gsn_labelbar_ndc                                          ;
;                   wks: workstation object                             ;
;                  nbox: number of labelbar boxes                       ;
;                labels: labels for boxes                               ;
;                     x: X NDC position of labelbar                     ;
;                     y: Y NDC position of labelbar                     ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws a labelbar on the workstation "wks" (the         ;
; variable returned from a previous call to "gsn_open_wks").            ;
; "resources" is an optional list of resources.                         ;
;***********************************************************************;

procedure gsn_labelbar_ndc(wks:graphic, nbox:integer, labels:string, \
                           x,y,resources:logical )
local i, lbid, attnames, res, lb_res_index, lbres
begin

    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if

    lbid = create wksname + "_labelbar" labelBarClass wks
      "vpXF"           : x
      "vpYF"           : y
      "lbBoxCount"     : nbox
      "lbLabelStrings" : labels
    end create

    lbres = get_res_eq(resources,(/"lb","vp"/))
    if(lbres.and..not.any(ismissing(getvaratts(lbres))))

; A special test is needed for the resource lbLabelFontHeightF.
; If it is set, then we need to turn off lbAutoManage.

      if(isatt(lbres,"lbLabelFontHeightF"))
        setvalues lbid
          "lbAutoManage"       : False
          "lbLabelJust"        : "CenterCenter"
          "lbLabelFontHeightF" : lbres@lbLabelFontHeightF
        end setvalues
        delete(lbres@lbLabelFontHeightF)
      end if
      if(lbres.and..not.any(ismissing(getvaratts(lbres))))
        attsetvalues(lbid,lbres)
      end if
    end if

; Draw labelbar.

    draw(lbid)
    delete(lbid)
end

;***********************************************************************;
; Function : gsn_map                                                    ;
;                      wks: workstation object                          ;
;               projection: Map projection                              ;
;                  resources: optional resources                        ;
;                                                                       ;
; This function creates and draws a map plot to the workstation "wks"   ;
; (the variable returned from a previous call to "gsn_open_wks").       ;
; "projection" is one of the ten supported map projections, and         ;
; "resources" is an optional list of resources. The id of the map plot  ;
; is returned.                                                          ;
;***********************************************************************;

function gsn_map(wks:graphic, projection:string, resources:logical )
local i, attnames, plot_object, res2
begin
    res2      = resources

;;;JACK - print to indicate this routine being used
; print("    gsn_code.ncl FUNCTION gsn_map ENTERED")

; Create plot object.

    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if

    plot_object = create wksname + "_map" mapPlotClass wks
        "mpProjection" : projection
    end create

; Check to see if any resources were set.

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)

    if(res2.and..not.any(ismissing(getvaratts(res2))))
      attsetvalues(plot_object,res2)
    end if

    draw_and_frame(wks,plot_object,calldraw,callframe)

; Return plot object.

    return(plot_object)
end

;***********************************************************************;
; Function : gsn_streamline                                             ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional U array                          ;
;                     v: 2-dimensional V array                          ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a streamline plot to the workstation  ;
; "wks" (the variable returned from a previous call to "gsn_open_wks"). ;
; "u" and "v" are the 2-dimensional arrays to be streamlined, and       ;
; "resources" is an optional list of resources. The id of the streamline;
; plot is returned.                                                     ;
;***********************************************************************;

function gsn_streamline(wks:graphic,u[*][*]:numeric,v[*][*]:numeric,\
                        resources:logical)
local i,attnames,data_object,plot_object,res,vf_res_index,st_res_index, \
force_x_linear, force_y_linear, force_x_log, force_y_log, \
trxmin, trxmax, trymin, trymax, ll_res_index, llres, res2, scale, shape
begin
    llres     = False
    res2      = resources

    force_x_linear = False
    force_y_linear = False
    force_x_log    = False
    force_y_log    = False

; Create the data object.

    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if

    data_object = create wksname + "_data" vectorFieldClass noparent
        "vfUDataArray" : u
        "vfVDataArray" : v
    end create

; Check for missing values.

    if(isatt(u,"_FillValue")) then
        setvalues data_object
            "vfMissingUValueV" :u@_FillValue
        end setvalues
    end if
    if(isatt(v,"_FillValue")) then
        setvalues data_object
            "vfMissingVValueV" :v@_FillValue
        end setvalues
    end if

; Create plot object.

    plot_object = create wksname + "_stream" streamlinePlotClass wks
        "stVectorFieldData" : data_object
    end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)

    check_for_irreg2loglin(res2,force_x_linear,force_y_linear,\
                                force_x_log,force_y_log)
    check_for_tickmarks_off(res2)

    vfres = get_res_eq(res2,"vf")
    stres = get_res_ne(res2,"vf")
    if(vfres.and..not.any(ismissing(getvaratts(vfres))))
      attsetvalues(data_object,vfres)
    end if
    if(stres.and..not.any(ismissing(getvaratts(stres))))
      attsetvalues(plot_object,stres)
    end if

    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
        llres = get_res_eq(res2,(/"tr","vp"/))
    end if

;
; If gsnShape was set to True, then resize the X or Y axis so that
; the scales are proportionally correct.
; 
    if(shape)
      gsnp_shape_plot(plot_object)
    end if

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(plot_object)
    end if

; Check if we need to force the X or Y axis to be linear or log.
; If so, then we have to overlay it on a LogLin Plot.

    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
      overlay_plot_object = plot_object
      delete(plot_object)

      getvalues overlay_plot_object
        "trXMinF"    : trxmin
        "trXMaxF"    : trxmax
        "trYMinF"    : trymin
        "trYMaxF"    : trymax
      end getvalues

      plot_object = create wksname + "_loglin" logLinPlotClass wks
        "trXLog"  : force_x_log
        "trYLog"  : force_y_log
        "trXMinF" : trxmin
        "trXMaxF" : trxmax
        "trYMinF" : trymin
        "trYMaxF" : trymax
      end create
      if(llres.and..not.any(ismissing(getvaratts(llres))))
        attsetvalues(plot_object,llres)
      end if

      overlay(plot_object,overlay_plot_object)
      plot_object@contour = overlay_plot_object
    end if

    draw_and_frame(wks,plot_object,calldraw,callframe)

; Return plot object and data object (as attribute of plot object).

    plot_object@data = data_object
    return(plot_object)
end

;***********************************************************************;
; Function : gsn_streamline_map                                         ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional U data                           ;
;                     v: 2-dimensional V data                           ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a streamline plot over a map plot to  ;
; the workstation "wks" (the variable returned from a previous call to  ;
; "gsn_open_wks").  "u" and "v" are the 2-dimensional arrays to be      ;
; streamlined, and "resources" is an optional list of resources. The id ;
; of the map plot is returned.                                          ;
;***********************************************************************;

function gsn_streamline_map(wks:graphic,u[*][*]:numeric,\
                            v[*][*]:numeric,resources:logical)
local i, attnames, data_object, contour_object, res, vf_res_index, \
st_res_index, mp_res_index, map_object, res2
begin
    res2      = resources

; Create the data object.

    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if

    data_object = create wksname + "_data" vectorFieldClass noparent
        "vfUDataArray" : u
        "vfVDataArray" : v
    end create

; Check for missing values.

    if(isatt(u,"_FillValue")) then
        setvalues data_object
            "vfMissingUValueV" :u@_FillValue
        end setvalues
    end if
    if(isatt(v,"_FillValue")) then
        setvalues data_object
            "vfMissingVValueV" :v@_FillValue
        end setvalues
    end if

; Create plot object.

    stream_object = create wksname + "_stream" streamlinePlotClass wks
        "stVectorFieldData" : data_object
    end create

; Create map object.

    map_object = create wksname + "_map" mapPlotClass wks
    end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)

    vfres = get_res_eq(res2,"vf")
    mpres = get_res_ne(res2,(/"vf","mp","vp"/))
    stres = get_res_ne(res2,(/"vf","mp","vp"/))

    if(vfres.and..not.any(ismissing(getvaratts(vfres))))
      attsetvalues(data_object,vfres)
    end if
    if(stres.and..not.any(ismissing(getvaratts(stres))))
      attsetvalues(stream_object,stres)
    end if
    if(mpres.and..not.any(ismissing(getvaratts(mpres))))
      attsetvalues(map_object,mpres)
    end if

    overlay(map_object,stream_object)

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(stream_object)
    end if

    draw_and_frame(wks,map_object,calldraw,callframe)

; Return plot object and data object (as attribute of plot object).

    map_object@data = data_object
    map_object@streamline = stream_object
    return(map_object)
end

;***********************************************************************;
; Procedure : gsn_text                                                  ;
;                   wks: workstation object                             ;
;                plotid: plot object                                    ;
;                  text: array of text strings                          ;
;                     x: 1-dimensional array of x data positions        ;
;                     y: 1-dimensional array of y data positions        ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws text strings on the workstation "wks" (the       ;
; variable returned from a previous call to "gsn_open_wks").  "x" and   ;
; "y" are the x and y locations of each text string, and should be      ;
; specified in the same data space as the data space of "plotid".       ;
; "resources" is an optional list of resources.                         ;
;***********************************************************************;

procedure gsn_text(wks:graphic, plotid:graphic, text:string, x:numeric, \
                   y:numeric, resources:logical )
local i, txid, attnames, plot_object, res, tx_res_index, x2, y2, xf, yf, \
funccode
begin
;
; datatondc can't accept doubles, so have to demote doubles if they
; come in.
;
    xf = tofloat(x)
    yf = tofloat(y)

    x2 = new(dimsizes(x),float)
    y2 = new(dimsizes(y),float)

    datatondc(plotid,xf,yf,x2,y2)

    delete(xf)
    delete(yf)
;
; The "txFuncCode" can't be set during a setvalues  call. It must be
; set during the creation of the object.  
;
    if((resources).and.isatt(resources,"txFuncCode")) then
      funccode = resources@txFuncCode
    else
      funccode = ":"     ; The default.
    end if

    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if

    txid = create wksname + "_text" textItemClass wks
        "txString"   : text
        "txPosXF"    : x2
        "txPosYF"    : y2
        "txFuncCode" : funccode
    end create

    txres = get_res_eq(resources,"tx")  ; Get text resources.
    if(txres.and..not.any(ismissing(getvaratts(txres))))
      attsetvalues(txid,txres)            ; Set text resources.
    end if
    draw(txid)                          ; Draw text string.
    delete(txid)
end

;***********************************************************************;
; Procedure : gsn_text_ndc                                              ;
;                   wks: workstation object                             ;
;                  text: array of text strings                          ;
;                     x: 1-dimensional array of x ndc positions         ;
;                     y: 1-dimensional array of y ndc positions         ;
;               resources: optional resources                           ;
;                                                                       ;
; This procedure draws text strings on the workstation "wks" (the       ;
; variable returned from a previous call to "gsn_open_wks").  "x" and   ;
; "y" are the x and y locations of each text string, and should be      ;
; specified in NDC space. "resources" is an optional list of resources. ;
;***********************************************************************;

procedure gsn_text_ndc(wks:graphic, text:string, x:numeric, \
                   y:numeric, resources:logical )
local i, txid, attnames, plot_object, res, tx_res_index, x2, y2
begin

    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if

    if((resources).and.isatt(resources,"txFuncCode")) then
      txid = create wksname + "_text_ndc" textItemClass wks
        "txString" : text
        "txPosXF"  : x
        "txPosYF"  : y
        "txFuncCode" : resources@txFuncCode
      end create
    else
      txid = create wksname + "_text_ndc" textItemClass wks
        "txString" : text
        "txPosXF"  : x
        "txPosYF"  : y
      end create
    end if

    txres = get_res_eq(resources,"tx")  ; Get text resources.
    if(txres.and..not.any(ismissing(getvaratts(txres))))
      attsetvalues(txid,txres)            ; Set text resources.
    end if
    draw(txid)                          ; Draw text string.
    delete(txid)
end

;***********************************************************************;
; Function : gsn_vector                                                 ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional U array                          ;
;                     v: 2-dimensional V array                          ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a vector plot to the workstation "wks";
; (the variable returned from a previous call to "gsn_open_wks").  "u"  ;
; and "v" are the 2-dimensional arrays to be vectorized, and "resources";
; is an optional list of resources. The id of the vector plot is        ;
; returned.                                                             ;
;***********************************************************************;

function gsn_vector(wks:graphic, u[*][*]:numeric, v[*][*]:numeric, \
                    resources:logical )
local i,attnames,data_object,plot_object,res,vf_res_index,vc_res_index, \
force_x_linear, force_y_linear, force_x_log, force_y_log, sprdcols, \
trxmin, trxmax, trymin, trymax, ll_res_index, llres, res2
begin
    llres     = False
    res2      = resources

    force_x_linear = False
    force_y_linear = False
    force_x_log    = False
    force_y_log    = False

; Create the data object.

    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if

    data_object = create wksname + "_data" vectorFieldClass noparent
        "vfUDataArray" : u
        "vfVDataArray" : v
    end create

; Check for missing values.

    if(isatt(u,"_FillValue")) then
        setvalues data_object
            "vfMissingUValueV" :u@_FillValue
        end setvalues
    end if
    if(isatt(v,"_FillValue")) then
        setvalues data_object
            "vfMissingVValueV" :v@_FillValue
        end setvalues
    end if

; Create plot object.

    plot_object = create wksname + "_vector" vectorPlotClass wks
        "vcVectorFieldData" : data_object
    end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)
    sprdcols  = get_res_value(res2,"gsnSpreadColors",False)
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)

    check_for_irreg2loglin(res2,force_x_linear,force_y_linear,\
                                force_x_log,force_y_log)
    check_for_tickmarks_off(res2)

    vfres = get_res_eq(res2,"vf")
    vcres = get_res_ne(res2,"vf")
    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
      llres = get_res_eq(res2,(/"tr","vp"/))
    end if

    if(vfres.and..not.any(ismissing(getvaratts(vfres))))
      attsetvalues(data_object,vfres)
    end if
    if(vcres.and..not.any(ismissing(getvaratts(vcres))))
      attsetvalues(plot_object,vcres)
    end if
    if(sprdcols)
      vcres2 = True
      set_attr(vcres2,"vcLevelColors",\
               spread_colors(wks,plot_object,min_index,max_index))
      attsetvalues(plot_object,vcres2)
    end if
;
; If gsnShape was set to True, then resize the X or Y axis so that
; the scales are proportionally correct.
; 
    if(shape)
      gsnp_shape_plot(plot_object)
    end if

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(plot_object)
    end if

; Check if we need to force the X or Y axis to be linear or log.
; If so, then we have to overlay it on a LogLin Plot.

    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
      overlay_plot_object = plot_object
      delete(plot_object)

      getvalues overlay_plot_object
        "trXMinF"    : trxmin
        "trXMaxF"    : trxmax
        "trYMinF"    : trymin
        "trYMaxF"    : trymax
      end getvalues

      plot_object = create wksname + "_loglin" logLinPlotClass wks
        "trXLog"  : force_x_log
        "trYLog"  : force_y_log
        "trXMinF" : trxmin
        "trXMaxF" : trxmax
        "trYMinF" : trymin
        "trYMaxF" : trymax
      end create
      if(llres.and..not.any(ismissing(getvaratts(llres))))
        attsetvalues(plot_object,llres)
      end if

      overlay(plot_object,overlay_plot_object)
      plot_object@contour = overlay_plot_object
    end if

    draw_and_frame(wks,plot_object,calldraw,callframe)

; Return plot object and data object (as attribute of plot object).

    plot_object@data = data_object
    return(plot_object)
end

;***********************************************************************;
; Function : gsn_vector_contour                                         ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional U data                           ;
;                     v: 2-dimensional V data                           ;
;                  data: 2-dimensional scalar field                     ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws vectors and contours over a map plot  ;
; to the workstation "wks" (the variable returned from a previous call  ;
; to "gsn_open_wks").  "u" and "v" are the 2-dimensional arrays to be   ;
; vectorized, and "data" is the scalar field to be contoured.           ;
; "resources" is an optional list of resources. The id of the map plot  ;
; is returned.                                                          ;
;***********************************************************************;

function gsn_vector_contour(wks:graphic,u[*][*]:numeric,\
                            v[*][*]:numeric,data[*][*]:numeric,\
                            resources:logical)
local i, attnames, vfdata_object, sfdata_object, contour_object, res, \
vf_res_index, vc_res_index, sf_res_index, res2
begin
    res2      = resources

;
; Create the vector field object.
;
    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if

    vfdata_object = create wksname + "_vfdata" vectorFieldClass noparent
        "vfUDataArray" : u
        "vfVDataArray" : v
    end create

;
; Check for missing values.
;
    if(isatt(u,"_FillValue")) then
        setvalues vfdata_object
            "vfMissingUValueV" :u@_FillValue
        end setvalues
    end if
    if(isatt(v,"_FillValue")) then
        setvalues vfdata_object
            "vfMissingVValueV" :v@_FillValue
        end setvalues
    end if

; Create the scalar field object.

    sfdata_object = create wksname + "_sfdata" scalarFieldClass noparent
        "sfDataArray" : data
    end create

; Check for a missing value.

    if(isatt(data,"_FillValue")) then
        setvalues sfdata_object
            "sfMissingValueV" :data@_FillValue
        end setvalues
    end if

; Create vector plot object.

    vector_object = create wksname + "_vector" vectorPlotClass wks
        "vcVectorFieldData" : vfdata_object
    end create

; Create contour plot object.

    contour_object = create wksname + "_contour" contourPlotClass wks
        "cnScalarFieldData" : sfdata_object
    end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)
    sprdcols  = get_res_value(res2,"gsnSpreadColors",False)
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)

    vfres = get_res_eq(res2,"vf")
    sfres = get_res_eq(res2,"sf")
    cnres = get_res_eq(res2,(/"cn","vp"/))
    vcres = get_res_ne(res2,(/"vf","sf","cn","vp"/))

    if(vfres.and..not.any(ismissing(getvaratts(vfres))))
      attsetvalues(vfdata_object,vfres)
    end if
    if(sfres.and..not.any(ismissing(getvaratts(sfres))))
      attsetvalues(sfdata_object,sfres)
    end if
    if(cnres.and..not.any(ismissing(getvaratts(cnres))))
      attsetvalues(contour_object,cnres)
    end if
    if(vcres.and..not.any(ismissing(getvaratts(vcres))))
      attsetvalues(vector_object,vcres)
    end if

    if(sprdcols)
      cnres2 = True
      vcres2 = True

      set_attr(cnres2,"cnFillColors",\
               spread_colors(wks,contour_object,min_index,max_index))
      set_attr(vcres2,"vcLevelColors",\
               spread_colors(wks,vector_object,min_index,max_index))

      attsetvalues(contour_object,cnres2)
      attsetvalues(vector_object,vcres2)
    end if

    overlay(vector_object,contour_object)

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(vector_object)
    end if

    draw_and_frame(wks,vector_object,calldraw,callframe)

; Return plot object and data object (as attribute of plot object).

    vector_object@vfdata = vfdata_object
    vector_object@sfdata = sfdata_object
    vector_object@contour = contour_object
    return(vector_object)
end

;***********************************************************************;
; Function : gsn_vector_contour_map                                     ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional U data                           ;
;                     v: 2-dimensional V data                           ;
;                  data: 2-dimensional scalar field                     ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws vectors and contours over a map plot  ;
; to the workstation "wks" (the variable returned from a previous call  ;
; to "gsn_open_wks").  "u" and "v" are the 2-dimensional arrays to be   ;
; vectorized, and "data" is the scalar field to be contoured.           ;
; "resources" is an optional list of resources. The id of the map plot  ;
; is returned.                                                          ;
;***********************************************************************;

function gsn_vector_contour_map(wks:graphic,u[*][*]:numeric,\
                               v[*][*]:numeric,data[*][*]:numeric,\
                               resources:logical)
local i, attnames, vfdata_object, sfdata_object, contour_object, res, \
vf_res_index, vc_res_index, sf_res_index, mp_res_index, map_object, res2
begin
    res2      = resources

;
; Create the vector field object.
;
    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if

    vfdata_object = create wksname + "_vfdata" vectorFieldClass noparent
        "vfUDataArray" : u
        "vfVDataArray" : v
    end create

;
; Check for missing values.
;
    if(isatt(u,"_FillValue")) then
        setvalues vfdata_object
            "vfMissingUValueV" :u@_FillValue
        end setvalues
    end if
    if(isatt(v,"_FillValue")) then
        setvalues vfdata_object
            "vfMissingVValueV" :v@_FillValue
        end setvalues
    end if

; Create the scalar field object.

    sfdata_object = create wksname + "_sfdata" scalarFieldClass noparent
        "sfDataArray" : data
    end create

; Check for a missing value.

    if(isatt(data,"_FillValue")) then
        setvalues sfdata_object
            "sfMissingValueV" :data@_FillValue
        end setvalues
    end if

; Create vector plot object.

    vector_object = create wksname + "_vector" vectorPlotClass wks
        "vcVectorFieldData" : vfdata_object
    end create

; Create contour plot object.

    contour_object = create wksname + "_contour" contourPlotClass wks
        "cnScalarFieldData" : sfdata_object
    end create

; Create map object.

    map_object = create wksname + "_map" mapPlotClass wks end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)
    sprdcols  = get_res_value(res2,"gsnSpreadColors",False)
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)

    vfres = get_res_eq(res2,"vf")
    sfres = get_res_eq(res2,"sf")
    cnres = get_res_eq(res2,"cn")
    mpres = get_res_eq(res2,(/"mp","vp"/))
    vcres = get_res_ne(res2,(/"cn","mp","sf","vf","vp"/))

    if(vfres.and..not.any(ismissing(getvaratts(vfres))))
      attsetvalues(vfdata_object,vfres)
    end if
    if(sfres.and..not.any(ismissing(getvaratts(sfres))))
      attsetvalues(sfdata_object,sfres)
    end if
    if(mpres.and..not.any(ismissing(getvaratts(mpres))))
      attsetvalues(map_object,mpres)
    end if
    if(cnres.and..not.any(ismissing(getvaratts(cnres))))
      attsetvalues(contour_object,cnres)
    end if
    if(vcres.and..not.any(ismissing(getvaratts(vcres))))
      attsetvalues(vector_object,vcres)
    end if

    if(sprdcols)
      cnres2 = True
      vcres2 = True

      set_attr(cnres2,"cnFillColors",\
               spread_colors(wks,contour_object,min_index,max_index))
      set_attr(vcres2,"vcLevelColors",\
               spread_colors(wks,vector_object,min_index,max_index))

      attsetvalues(contour_object,cnres2)
      attsetvalues(vector_object,vcres2)
    end if

    overlay(map_object,vector_object)
    overlay(map_object,contour_object)

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(vector_object)
    end if

    draw_and_frame(wks,map_object,calldraw,callframe)

; Return plot object and data object (as attribute of plot object).

    map_object@vfdata = vfdata_object
    map_object@sfdata = sfdata_object
    map_object@vector = vector_object
    map_object@contour = contour_object
    return(map_object)
end

;***********************************************************************;
; Function : gsn_vector_map                                             ;
;                   wks: workstation object                             ;
;                     : 2-dimensional U data                            ;
;                     v: 2-dimensional V data                           ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a vector plot over a map plot to the  ;
; workstation "wks" (the variable returned from a previous call to      ;
; "gsn_open_wks").  "u" and "v" are the 2-dimensional arrays to be      ;
; vectorized, and "resources" is an optional list of resources. The id  ;
; of the map plot is returned.                                          ;
;***********************************************************************;

function gsn_vector_map(wks:graphic, u[*][*]:numeric, v[*][*]:numeric, \
                        resources:logical )
local i, attnames, data_object, contour_object, res, vf_res_index, \
vc_res_index, mp_res_index, map_object, res2, sprdcols
begin
    res2      = resources

; Create the data object.

    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if

    data_object = create wksname + "_data" vectorFieldClass noparent
        "vfUDataArray" : u
        "vfVDataArray" : v
    end create

; Check for missing values.

    if(isatt(u,"_FillValue")) then
        setvalues data_object
            "vfMissingUValueV" :u@_FillValue
        end setvalues
    end if
    if(isatt(v,"_FillValue")) then
        setvalues data_object
            "vfMissingVValueV" :v@_FillValue
        end setvalues
    end if

; Create plot object.

    vector_object = create wksname + "_vector" vectorPlotClass wks
        "vcVectorFieldData" : data_object
    end create

; Create map object.

    map_object = create wksname + "_map" mapPlotClass wks
    end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)
    sprdcols  = get_res_value(res2,"gsnSpreadColors",False)
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)

    vfres = get_res_eq(res2,"vf")
    mpres = get_res_eq(res2,(/"mp","vp"/))
    vcres = get_res_ne(res2,(/"mp","vf","vp"/))

    if(vfres.and..not.any(ismissing(getvaratts(vfres))))
      attsetvalues(data_object,vfres)
    end if
    if(mpres.and..not.any(ismissing(getvaratts(mpres))))
      attsetvalues(map_object,mpres)
    end if
    if(vcres.and..not.any(ismissing(getvaratts(vcres))))
      attsetvalues(vector_object,vcres)
    end if

    if(sprdcols)
      vcres2 = True
      set_attr(vcres2,"vcLevelColors",\
               spread_colors(wks,vector_object,min_index,max_index))
      attsetvalues(vector_object,vcres2)
    end if

    overlay(map_object,vector_object)
;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(vector_object)
    end if

    draw_and_frame(wks,map_object,calldraw,callframe)

; Return plot object and data object (as attribute of plot object).

    map_object@data = data_object
    map_object@vector = vector_object
    return(map_object)
end

;***********************************************************************;
; Function : gsn_vector_scalar                                          ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional U array                          ;
;                     v: 2-dimensional V array                          ;
;                  data: 2-dimensional scalar field                     ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a vector plot to the workstation "wks";
; (the variable returned from a previous call to "gsn_open_wks").  "u"  ;
; and "v" are the 2-dimensional arrays to be vectorized, and "data" is  ;
; the scalar field that the vectors are colored by. "resources" is an   ;
; optional list of resources. The id of the vector plot is returned.    ;
;***********************************************************************;

function gsn_vector_scalar(wks:graphic,u[*][*]:numeric,v[*][*]:numeric,\
                           data[*][*]:numeric, resources:logical )
local i, attnames, vfdata_object, sfdata_object, plot_object, res, \
force_x_linear, force_y_linear, force_x_log, force_y_log, \
trxmin, trxmax, trymin, trymax, ll_res_index, llres, vf_res_index, \
vc_res_index, sf_res_index, res2, sprdcols
begin
    llres     = False
    res2      = resources

    force_x_linear = False
    force_y_linear = False
    force_x_log    = False
    force_y_log    = False

; Create the vector field data object.

    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if

    vfdata_object = create wksname + "_vfdata" vectorFieldClass noparent
        "vfUDataArray" : u
        "vfVDataArray" : v
    end create

; Create the scalar field data object.

    sfdata_object = create wksname + "_sfdata" scalarFieldClass noparent
        "sfDataArray" : data
    end create

; Check for a missing value.

    if(isatt(data,"_FillValue")) then
        setvalues sfdata_object
            "sfMissingValueV" :data@_FillValue
        end setvalues
    end if

; Check for missing values.

    if(isatt(u,"_FillValue")) then
        setvalues vfdata_object
            "vfMissingUValueV" :u@_FillValue
        end setvalues
    end if
    if(isatt(v,"_FillValue")) then
        setvalues vfdata_object
            "vfMissingVValueV" :v@_FillValue
        end setvalues
    end if

; Create plot object.

    plot_object = create wksname + "_vector" vectorPlotClass wks
        "vcVectorFieldData"     : vfdata_object
        "vcScalarFieldData"     : sfdata_object
        "vcUseScalarArray"      : True
        "vcMonoLineArrowColor"  : False
    end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)
    sprdcols  = get_res_value(res2,"gsnSpreadColors",False)
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)

    check_for_irreg2loglin(res2,force_x_linear,force_y_linear,\
                                force_x_log,force_y_log)
    check_for_tickmarks_off(res2)

    vfres = get_res_eq(res2,"vf")
    sfres = get_res_eq(res2,"sf")
    vcres = get_res_ne(res2,(/"sf","vf"/))

    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
      llres = get_res_eq(res2,(/"tr","vp"/))
    end if

    if(vfres.and..not.any(ismissing(getvaratts(vfres))))
      attsetvalues(vfdata_object,vfres)
    end if
    if(sfres.and..not.any(ismissing(getvaratts(sfres))))
      attsetvalues(sfdata_object,sfres)
    end if
    if(vcres.and..not.any(ismissing(getvaratts(vcres))))
      attsetvalues(plot_object,vcres)
    end if
    if(sprdcols)
      vcres2 = True
      set_attr(vcres2,"vcLevelColors",\
               spread_colors(wks,plot_object,min_index,max_index))
      attsetvalues(plot_object,vcres2)
    end if
;
; If gsnShape was set to True, then resize the X or Y axis so that
; the scales are proportionally correct.
; 
    if(shape)
      gsnp_shape_plot(plot_object)
    end if

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(plot_object)
    end if

; Check if we need to force the X or Y axis to be linear or log.
; If so, then we have to overlay it on a LogLin Plot.

    if(force_x_linear.or.force_x_log.or.force_y_linear.or.force_y_log)
      overlay_plot_object = plot_object
      delete(plot_object)

      getvalues overlay_plot_object
        "trXMinF"    : trxmin
        "trXMaxF"    : trxmax
        "trYMinF"    : trymin
        "trYMaxF"    : trymax
      end getvalues

      plot_object = create wksname + "_loglin" logLinPlotClass wks
        "trXLog"  : force_x_log
        "trYLog"  : force_y_log
        "trXMinF" : trxmin
        "trXMaxF" : trxmax
        "trYMinF" : trymin
        "trYMaxF" : trymax
      end create
      if(llres.and..not.any(ismissing(getvaratts(llres))))
        attsetvalues(plot_object,llres)
      end if

      overlay(plot_object,overlay_plot_object)
      plot_object@contour = overlay_plot_object
    end if

    draw_and_frame(wks,plot_object,calldraw,callframe)

; Return plot object and data object (as attribute of plot object).

    plot_object@vfdata = vfdata_object
    plot_object@sfdata = sfdata_object
    return(plot_object)
end

;***********************************************************************;
; Function : gsn_vector_scalar_map                                      ;
;                   wks: workstation object                             ;
;                     u: 2-dimensional U data                           ;
;                     v: 2-dimensional V data                           ;
;                  data: 2-dimensional scalar field                     ;
;               resources: optional resources                           ;
;                                                                       ;
; This function creates and draws a vector plot over a map plot to the  ;
; workstation "wks" (the variable returned from a previous call to      ;
; "gsn_open_wks").  "u" and "v" are the 2-dimensional arrays to be      ;
; vectorized, and "data" is the scalar field that the vectors are       ;
; colored by. "resources" is an optional list of resources. The id of   ;
; the map plot is returned.                                             ;
;***********************************************************************;

function gsn_vector_scalar_map(wks:graphic,u[*][*]:numeric,\
                               v[*][*]:numeric,data[*][*]:numeric,\
                               resources:logical)
local i, attnames, vfdata_object, sfdata_object, contour_object, res, \
vf_res_index, vc_res_index, sf_res_index, mp_res_index, map_object, res2, \
sprdcols
begin
    res2      = resources
;
; Create the vector field object.
;
    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if

    vfdata_object = create wksname + "_vfdata" vectorFieldClass noparent
        "vfUDataArray" : u
        "vfVDataArray" : v
    end create

;
; Check for missing values.
;
    if(isatt(u,"_FillValue")) then
        setvalues vfdata_object
            "vfMissingUValueV" :u@_FillValue
        end setvalues
    end if
    if(isatt(v,"_FillValue")) then
        setvalues vfdata_object
            "vfMissingVValueV" :v@_FillValue
        end setvalues
    end if

; Create the scalar field object.

    sfdata_object = create wksname + "_sfdata" scalarFieldClass noparent
        "sfDataArray" : data
    end create

; Check for a missing value.

    if(isatt(data,"_FillValue")) then
        setvalues sfdata_object
            "sfMissingValueV" :data@_FillValue
        end setvalues
    end if

; Create plot object.

    vector_object = create wksname + "_vector" vectorPlotClass wks
        "vcVectorFieldData"     : vfdata_object
        "vcScalarFieldData"     : sfdata_object
        "vcUseScalarArray"      : True
        "vcMonoLineArrowColor"  : False
    end create

; Create map object.

    map_object = create wksname + "_map" mapPlotClass wks end create

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)
    sprdcols  = get_res_value(res2,"gsnSpreadColors",False)
    min_index = get_res_value(res2,"gsnSpreadColorStart",2)
    max_index = get_res_value(res2,"gsnSpreadColorEnd",-1)

    vfres = get_res_eq(res2,"vf")
    sfres = get_res_eq(res2,"sf")
    mpres = get_res_eq(res2,(/"mp","vp"/))
    vcres = get_res_ne(res2,(/"mp","sf","vf","vp"/))

    if(vfres.and..not.any(ismissing(getvaratts(vfres))))
      attsetvalues(vfdata_object,vfres)
    end if
    if(sfres.and..not.any(ismissing(getvaratts(sfres))))
      attsetvalues(sfdata_object,sfres)
    end if
    if(mpres.and..not.any(ismissing(getvaratts(mpres))))
      attsetvalues(map_object,mpres)
    end if
    if(vcres.and..not.any(ismissing(getvaratts(vcres))))
      attsetvalues(vector_object,vcres)
    end if

    if(sprdcols)
      vcres2 = True
      set_attr(vcres2,"vcLevelColors",\
               spread_colors(wks,vector_object,min_index,max_index))
      attsetvalues(vector_object,vcres2)
    end if

    overlay(map_object,vector_object)

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(vector_object)
    end if

    draw_and_frame(wks,map_object,calldraw,callframe)

; Return plot object and data object (as attribute of plot object).

    map_object@vfdata = vfdata_object
    map_object@sfdata = sfdata_object
    map_object@vector = vector_object
    return(map_object)
end

;***********************************************************************;
; Function : gsn_xy                                                     ;
;                   wks: workstation object                             ;
;                     x: n-dimensional array of X arrays                ;
;                     y: n-dimensional array of Y array                 ;
;             resources: optional resources                             ;
;                                                                       ;
; This function creates and draws an xy plot to the workstation "wks"   ;
; (the variable returned from a previous call to "gsn_open_wks").  "x"  ;
; and "y" are either 1 or 2-dimensional arrays containing the X and Y   ;
; data points and "resources" is an optional list of resources. The id  ;
; of the xy plot is returned.                                           ;
;***********************************************************************;

function gsn_xy(wks:graphic, x:numeric, y:numeric, resources:logical )
local i, attnames, data_object, plot_object, res, ca_res_index, \
xy_res_index, xydp_res_index, dspec, res2, set_dash
begin
    set_dash = True       ; Default is to set some dash patterns.
    res2     = resources

; Determine if we have multiple lines or just one line.

    nxdims = dimsizes(dimsizes(x))
    xdims = dimsizes(x)
    if(isatt(wks,"name"))
      wksname = wks@name
    else
      wksname = "gsnapp"
    end if

    data_object = create wksname + "_data" coordArraysClass noparent
        "caXArray" : x
        "caYArray" : y
    end create

; Check for missing values.

    if(isatt(x,"_FillValue")) then
        setvalues data_object
            "caXMissingV" :x@_FillValue
        end setvalues
    end if
    if(isatt(y,"_FillValue")) then
        setvalues data_object
            "caYMissingV" :y@_FillValue
        end setvalues
    end if

; Create plot object.

    plot_object = create wksname + "_xy" xyPlotClass wks
        "xyCoordData" : data_object
    end create

    getvalues plot_object
      "trXMinF" : trxmin
      "trXMaxF" : trxmax
      "trYMinF" : trymin
      "trYMaxF" : trymax
    end getvalues

    if(res2.and..not.any(ismissing(getvaratts(res2))))
      if(isatt(res2,"trXMinF")) then
        trxmin = res2@trXMinF
      end if 
      if(isatt(res2,"trXMaxF")) then
        trxmax = res2@trXMaxF
      end if 
      if(isatt(res2,"trYMinF")) then
        trymin = res2@trYMinF
      end if 
      if(isatt(res2,"trYMayF")) then
        trymax = res2@trYMaxF
      end if 
    end if
    plot_object = create wksname + "_xy" xyPlotClass wks
        "xyCoordData" : data_object
        "trXMinF"     : trxmin
        "trXMaxF"     : trxmax
        "trYMinF"     : trymin
        "trYMaxF"     : trymax
    end create

; Check for existence of x@long_name and y@long_name and use them
; to label X and Y axes.

    if(isatt(x,"long_name")) then
      set_attr(res2,"tiXAxisString",x@long_name)
    end if
    if(isatt(y,"long_name")) then
      set_attr(res2,"tiYAxisString",y@long_name)
    end if

; By default, only solid lines get drawn if there are multiple lines, so
; set some dash patterns to use instead.  Also set different marker styles.

    getvalues plot_object
        "xyCoordDataSpec" : dspec
    end getvalues

    if(res2.and..not.any(ismissing(getvaratts(res2))))
      if(isatt(res2,"xyDashPattern").or.isatt(res2,"xyDashPatterns"))
        set_dash = False
      end if
    end if

    if(set_dash)
      setvalues dspec
        "xyDashPatterns" : (/0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,\
                             0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,\
                             0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/)
      end setvalues
    end if

    calldraw  = get_res_value(res2,"gsnDraw", True)
    callframe = get_res_value(res2,"gsnFrame",True)
    shape     = get_res_value(res2,"gsnShape",False)
    scale     = get_res_value(res2,"gsnScale",shape)

    check_for_tickmarks_off(res2)

    cares = get_res_eq(res2,"ca")
    if(cares.and..not.any(ismissing(getvaratts(cares))))
      attsetvalues(data_object,cares)
    end if

    if(res2.and..not.any(ismissing(getvaratts(res2))))
; Get list of resources.

        attnames = getvaratts(res2)
        res = stringtochar(attnames(ind(attnames.ne."_FillValue")))

;***********************************************************************;
; Check to see if any xy plot resources were set.  There are two kinds  ;
; of xy plot resources, the regular kind, and the data spec kind.  If   ;
; the resource starts with an "xy", it could be either kind, so we need ;
; to have some tests to see which object it belongs to.  Any "xy"       ;
; resources that start with "xyCo", "xyX", or "xyY" are regular         ;
; resources (meaning, it belongs to the XyPlot object). The remaining   ;
; "xy" resources belong to the data spec object. Any resources that do  ;
; not start with "xy" or "ca" are assumed to also go with the XyPlot    ;
; object.                                                               ;
;***********************************************************************;

        if(dimsizes(dimsizes(res)).eq.1)
            if((chartostring(res(0:1)).ne."ca".and.\
                chartostring(res(0:1)).ne."xy").or.\
               (chartostring(res(0:1)).eq."xy".and.\
               (chartostring(res(0:3)).eq."xyCo".or.\
                chartostring(res(0:2)).eq."xyX".or.\
                chartostring(res(0:2)).eq."xyY")))
                setvalues plot_object
                    attnames : res2@$attnames$
                end setvalues
            end if
            if(chartostring(res(0:1)).eq."xy".and.\
              (chartostring(res(0:3)).ne."xyCo".and.\
               chartostring(res(0:2)).ne."xyX".and.\
               chartostring(res(0:2)).ne."xyY"))
                setvalues dspec
                    attnames : res2@$attnames$
                end setvalues
            end if
        else
            xy_res_index = ind((chartostring(res(:,0:1)).ne."ca".and.\
                                chartostring(res(:,0:1)).ne."xy").or.\
                               (chartostring(res(:,0:1)).eq."xy".and.\
                               (chartostring(res(:,0:3)).eq."xyCo".or.\
                                chartostring(res(:,0:2)).eq."xyX".or.\
                                chartostring(res(:,0:2)).eq."xyY")))
            xydp_res_index = ind(chartostring(res(:,0:1)).eq."xy".and.\
                                (chartostring(res(:,0:3)).ne."xyCo".and.\
                                 chartostring(res(:,0:2)).ne."xyX".and.\
                                 chartostring(res(:,0:2)).ne."xyY"))
            if(.not.all(ismissing(xy_res_index)))
                xyres = True
                do i = 0,dimsizes(xy_res_index)-1
                    xyres@$attnames(xy_res_index(i))$ = res2@$attnames(xy_res_index(i))$
                end do
                attsetvalues(plot_object,xyres)
            end if
            if(.not.all(ismissing(xydp_res_index)))
                getvalues plot_object
                    "xyCoordDataSpec" : dspec
                end getvalues
                xydpres = True
                do i = 0,dimsizes(xydp_res_index)-1
                    xydpres@$attnames(xydp_res_index(i))$ = res2@$attnames(xydp_res_index(i))$
                end do
                attsetvalues(dspec,xydpres)
            end if
        end if

    end if
;
; If gsnShape was set to True, then resize the X or Y axis so that
; the scales are proportionally correct.
; 
    if(shape)
      gsnp_shape_plot(plot_object)
    end if

;
; If gsnScale was set to True, then make sure the X and Y axis labels
; and tick marks are the same size.
; 
    if(scale)
      gsnp_scale_plot(plot_object)
    end if

    draw_and_frame(wks,plot_object,calldraw,callframe)

; Return plot object and data object (as attribute of plot object).

    plot_object@data     = data_object
    plot_object@dataspec = dspec
    return(plot_object)
end
