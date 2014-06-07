
      subroutine cldaisy(nn,jpp,x,valmd,weights,
     +     jtmd,jdat,vtype,ndyst,mdata,disv)
c     c
c     c  Calculating dissimilarities between objects or variables
c     c

      integer nn, jpp
c     c          nn  = number of objects
c     c          jpp = number of variables used for the calculations

c     c  The following vectors and matrices must be dimensioned in the
c     c  main program :
      double precision x(nn,jpp), valmd(jpp), weights(jpp)
      double precision disv(1+nn*(nn-1)/2)
      integer jtmd(jpp), jdat, vtype(jpp), ndyst, mdata

c       vtype was character originally
c       vtype(j) is the type of variable j:
c              = 1 (A) for an Asymmetric binary variable
c              = 2 (S) for a  Symmetric  binary variable
c              = 3 (N) for a  Nominal  variable
c              = 4 (O) for an Ordinal  variable
c              = 5 (I) for an Interval variable (additive)
c              = 6 (T) for a  raTio    variable (log transformed)

c       vector jtmd is only read if there are missing values : if(mdata)
c       jtmd(j) =  0 if variable j is binary
c               = -1 if variable j is not binary and has missing values
c               = +1 if variable j is not binary and has no missing values
c VAR
      double precision clk,dlk, pp,ppa, rpres
      integer j,k,l,la, lsubt, nlk, nbad, npres
      logical hasNA

      hasNA = (mdata .ne. 0)

c         calculation of the dissimilarities
      nlk=0
      if(jdat .eq. 1) then
c Case I: `mixed' type variables
         nbad=0
         do 450 l=2,nn
            la=l-1
            do 440 k=1,la
               nlk=nlk+1
               ppa=0.
               dlk=0.
c               Dissimilarity between obs.  l & k
               do 420 j=1,jpp
                  if(vtype(j) .ge. 3) then
                     if (hasNA) then
                        if(jtmd(j).lt.0) then
                           if(x(l,j).eq.valmd(j)) goto 420
                           if(x(k,j).eq.valmd(j)) goto 420
                        endif
                     endif
                     ppa=ppa + weights(j)
                     if(vtype(j).eq.3) then
                        if(x(l,j).ne.x(k,j)) dlk=dlk+ weights(j)
                     else
                        dlk=dlk+ weights(j)*dabs(x(l,j)-x(k,j))
                     endif
                  else
c               binary variable x(*,j)
                     if(x(l,j).ne.0..and.x(l,j).ne.1.) goto 420
                     if(x(k,j).ne.0..and.x(k,j).ne.1.) goto 420
                     if(vtype(j).eq.2.or.x(l,j).ne.0.or.x(k,j).ne.0)
     *                    ppa=ppa+weights(j)
                     if(x(l,j).ne.x(k,j)) dlk=dlk+ weights(j)
                  endif
 420           continue
               if(ppa.le.0.5) then
                  nbad=nbad+1
                  disv(nlk)=-1
               else
                  disv(nlk)=dlk/ppa
               endif
 440        continue
 450     continue

      else
c Case II : jdat != 1:  all variables are interval scaled
c -------   ~~~~~~~~~ { basically === dysta() in ./dysta.f
c                       FIXME: common code! }
         pp=jpp
         do 600 l=2,nn
            lsubt=l-1
            do 520 k=1,lsubt
               clk=0.0
               nlk=nlk+1
               npres=0
               do 530 j=1,jpp
                  if (hasNA) then
                     if(jtmd(j).lt.0) then
                        if(x(l,j).eq.valmd(j)) goto 530
                        if(x(k,j).eq.valmd(j)) goto 530
                     endif
                  endif
                  npres=npres+1
                  if(ndyst.eq.1) then
                     clk=clk+ (x(l,j)-x(k,j))*(x(l,j)-x(k,j))
                  else
                     clk=clk+ dabs(x(l,j)-x(k,j))
                  endif
 530           continue
               rpres=npres
               if(npres.eq.0)then
                  disv(nlk)=-1.0
               else if(ndyst.eq.1) then
                  disv(nlk)=dsqrt(clk*(pp/rpres))
               else
                  disv(nlk)=clk*(pp/rpres)
               endif
 520        continue
 600     continue
      endif

      end
