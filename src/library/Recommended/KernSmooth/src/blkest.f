c  Part of R package KernSmooth
c  Copyright (C) 1995  M. P. Wand
c
c  Unlimited use and distribution (see LICENCE).

cccccccc FORTRAN subroutine blkest.f cccccccccc

c     For computing blocked polynomial estimates
c     required for the direct plug-in bandwidth
c     selector of Ruppert, Sheather and Wand.

c     Last changed: 26/04/95

      subroutine blkest(X,Y,n,q,qq,Nval,Xj,Yj,coef,Xmat,wk,qraux,
     +                  sigsqe,th22e,th24e)

      integer n,q,qq,Nval,nj,i,j,k,idiv,ilow,iupp,info
      double precision RSS,X(n),Y(n),Xj(n),Yj(n),coef(qq),wk(n),
     +                 Xmat(n,qq),qraux(qq),fiti,th22e,th24e,sigsqe,
     +                 ddm,ddddm,work(1)

c     It is assumed that the (X,Y) data are
c     sorted with respect to the X's.
      RSS = 0.0d0
      th22e = 0.0d0
      th24e = 0.0d0
      idiv = n/Nval
      do 10 j = 1,Nval

c        For each member of the partition

         ilow = (j-1)*idiv + 1
         iupp = j*idiv
         if (j.eq.Nval) iupp = n
         nj = iupp - ilow + 1
         do 20 k = 1,nj
            Xj(k) = X(ilow+k-1)
            Yj(k) = Y(ilow+k-1)
20       continue

c        Obtain a q'th degree fit over current
c        member of partition

c        Set up "X" matrix

         do 30 i = 1,nj
            Xmat(i,1) = 1.0d0
            do 40 k = 2,qq
               Xmat(i,k) = Xj(i)**(k-1)
40          continue
30          continue

            call dqrdc(Xmat,n,nj,qq,qraux,0,work,0)
            info=0
            call dqrsl(Xmat,n,nj,qq,qraux,Yj,wk,wk,coef,wk,wk,
     +                 00100,info)

            do 50 i = 1,nj
               fiti = coef(1)
               ddm = 2*coef(3)
               ddddm = 24*coef(5)
               do 60 k = 2,qq
                  fiti = fiti + coef(k)*Xj(i)**(k-1)
                  if (k.le.(q-1)) then
                     ddm = ddm + k*(k+1)*coef(k+2)*Xj(i)**(k-1)
                     if (k.le.(q-3)) then
                        ddddm = ddddm +
     +                  k*(k+1)*(k+2)*(k+3)*coef(k+4)*Xj(i)**(k-1)
                     endif
                  endif
60             continue
               th22e = th22e + ddm**2
               th24e = th24e + ddm*ddddm
               RSS = RSS + (Yj(i)-fiti)**2
50          continue
10       continue

      sigsqe = RSS/(n-qq*Nval)
      th22e = th22e/n
      th24e = th24e/n

      return
      end

cccccccccc End of blkest.f cccccccccc
