c  Part of R package KernSmooth
c  Copyright (C) 1995  M. P. Wand
c  Copyright (C) 2007  B. D. Ripley
c
c  Unlimited use and distribution (see LICENCE).

cccccccc FORTRAN subroutine cp.f cccccccccc

c     For computing Mallow's C_p values for a
c     set of "Nmax" blocked q'th degree fits.

c     Last changed: 09/05/95

c     remove unused 'q' 2007-07-10
      subroutine cp(X,Y,n,qq,Nmax,RSS,Xj,Yj,coef,Xmat,wk,qraux,Cpvals)
      integer Nmax,n,qq,Nval,nj,i,j,k,idiv,ilow,iupp
      double precision RSS(Nmax),X(n),Y(n),Xj(n),Yj(n),coef(qq),wk(n),
     +                 Xmat(n,qq),qraux(qq),Cpvals(NMax),fiti,RSSj,
     +                 work(1)

c     It is assumed that the (X,Y) data are
c     sorted with respect to the X's.

c     Compute vector of RSS values
      do 10 i = 1,Nmax
         RSS(i) = dble(0)
10    continue

      do 20 Nval = 1,Nmax

c     For each number of partitions

         idiv = n/Nval
         do 30 j = 1,Nval
c           For each member of the partition

            ilow = (j-1)*idiv + 1
            iupp = j*idiv
            if (j.eq.Nval) iupp = n
            nj = iupp - ilow + 1
            do 40 k = 1,nj
               Xj(k) = X(ilow+k-1)
               Yj(k) = Y(ilow+k-1)
40          continue

c           Obtain a q'th degree fit over current
c           member of partition

c           Set up "X" matrix

            do 50 i = 1,nj
               Xmat(i,1) = 1.0d0
               do 60 k = 2,qq
                  Xmat(i,k) = Xj(i)**(k-1)
60             continue
50          continue

            call dqrdc(Xmat,n,nj,qq,qraux,0,work,0)
      info=0
      call dqrsl(Xmat,n,nj,qq,qraux,Yj,wk,wk,coef,wk,wk,00100,info)

            RSSj = dble(0)
            do 70 i = 1,nj
               fiti = coef(1)
               do 80 k = 2,qq
                  fiti = fiti + coef(k)*Xj(i)**(k-1)
80             continue
               RSSj = RSSj + (Yj(i)-fiti)**2
70          continue

         RSS(Nval) = RSS(Nval) + RSSj

30       continue
20    continue

c     Now compute array of Mallow's C_p values.

      do 90 i = 1,Nmax
         Cpvals(i) = ((n-qq*Nmax)*RSS(i)/RSS(Nmax)) + 2*qq*i - n
90    continue

      return
      end

cccccccccc End of cp.f cccccccccc
