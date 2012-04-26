c  Part of R package KernSmooth
c  Copyright (C) 1995  M. P. Wand
c
c  Unlimited use and distribution (see LICENCE).

cccccccccc FORTRAN subroutine rlbin.f cccccccccc

c Obtains bin counts for univariate regression data
c via the linear binning strategy. If "trun=0" then
c weight from end observations is given to corresponding
c end grid points. If "trun=1" then end observations
c are truncated.

c Last changed: 26 MAR 2009

      subroutine rlbin(X,Y,n,a,b,M,trun,xcnts,ycnts)
      double precision X(*),Y(*),a,b,xcnts(*),ycnts(*),lxi,delta,rem
      integer n,M,i,li,trun

c     Initialize grid counts to zero

      do 10 i=1,M
         xcnts(i) = dble(0)
         ycnts(i) = dble(0)
10    continue

      delta = (b-a)/(M-1)
      do 20 i=1,n
         lxi = ((X(i)-a)/delta) + 1

c        Find integer part of "lxi"

         li = int(lxi) 
         rem = lxi - li
         if (li.ge.1.and.li.lt.M) then
            xcnts(li) = xcnts(li) + (1-rem)
            xcnts(li+1) = xcnts(li+1) + rem
            ycnts(li) = ycnts(li) + (1-rem)*y(i)
            ycnts(li+1) = ycnts(li+1) + rem*y(i)
         endif

         if (li.lt.1.and.trun.eq.0) then
            xcnts(1) = xcnts(1) + 1
            ycnts(1) = ycnts(1) + y(i)
         endif      
  
         if (li.ge.M.and.trun.eq.0) then 
               xcnts(M) = xcnts(M) + 1
               ycnts(M) = ycnts(M) + y(i)
         endif

20    continue

      return
      end

cccccccccc End of rlbin.f cccccccccc
