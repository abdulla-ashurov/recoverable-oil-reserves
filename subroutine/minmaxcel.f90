    subroutine minmaxcel(ni,nj,xcel,ycel,xmin,ymin,xmax,ymax,imin,jmin,imax,jmax)
    real*4 xcel(ni,nj),ycel(ni,nj)
    integer*4 ni,nj
    integer*2 imin,jmin,imax,jmax

    xmin=999999999.
    if(xcel(imin,jmin).lt.xmin)xmin=xcel(imin,jmin)
    if(xcel(imax,jmin).lt.xmin)xmin=xcel(imax,jmin)
    if(xcel(imin,jmax).lt.xmin)xmin=xcel(imin,jmax)
    if(xcel(imax,jmax).lt.xmin)xmin=xcel(imax,jmax)
    ymin=999999999.
    if(ycel(imin,jmin).lt.ymin)ymin=ycel(imin,jmin)
    if(ycel(imax,jmin).lt.ymin)ymin=ycel(imax,jmin)
    if(ycel(imin,jmax).lt.ymin)ymin=ycel(imin,jmax)
    if(ycel(imax,jmax).lt.ymin)ymin=ycel(imax,jmax)
    xmax=-999999999.
    if(xcel(imin,jmin).gt.xmax)xmax=xcel(imin,jmin)
    if(xcel(imax,jmin).gt.xmax)xmax=xcel(imax,jmin)
    if(xcel(imin,jmax).gt.xmax)xmax=xcel(imin,jmax)
    if(xcel(imax,jmax).gt.xmax)xmax=xcel(imax,jmax)
    ymax=-999999999.
    if(ycel(imin,jmin).gt.ymax)ymax=ycel(imin,jmin)
    if(ycel(imax,jmin).gt.ymax)ymax=ycel(imax,jmin)
    if(ycel(imin,jmax).gt.ymax)ymax=ycel(imin,jmax)
    if(ycel(imax,jmax).gt.ymax)ymax=ycel(imax,jmax)
    return
    end
