
c Dysta() :
c
c Compute Distances from X matrix {also for agnes() and diana()}:
c -----------------------------------------------------------
c
c was part of  pam.f --- now called both from Fortran & C
c "keep in sync" with  daisy.f {move both to C or replace by R's dist!}
c
      subroutine dysta(nn,p,x,dys,ndyst,jtmd,valmd,jhalt)

      integer nn, p, ndyst, jtmd(p), jhalt
      double precision x(nn,p), dys(1+nn*(nn-1)/2), valmd(p)
c ndyst = 1 : euclidean
c "else"    : manhattan

c VARs
      integer nlk,j,l,k, lsubt, npres
      double precision pp, clk, rpres

      nlk=1
      dys(1)=0.0
c     ---------- is used potentially for  d[i,i] == dys[1] == 0
      pp=p
      do 100 l=2,nn
         lsubt=l-1
         do 20 k=1,lsubt
            clk=0.0
            nlk=nlk+1
            npres=0
            do 30 j=1,p
               if(jtmd(j).lt.0) then
                  if(x(l,j).eq.valmd(j))goto 30
                  if(x(k,j).eq.valmd(j))goto 30
               endif
               npres=npres+1
               if(ndyst.eq.1) then
                  clk=clk+ (x(l,j)-x(k,j))*(x(l,j)-x(k,j))
               else
                  clk=clk+ dabs(x(l,j)-x(k,j))
               endif
 30         continue
            rpres=npres
            if(npres.eq.0) then
               jhalt=1
               dys(nlk)=-1.0
            else
               if(ndyst.eq.1) then
                  dys(nlk)= dsqrt(clk*(pp/rpres))
               else
                  dys(nlk)= clk*(pp/rpres)
               endif
            endif
 20      continue
 100  continue
      end
