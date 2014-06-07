      subroutine clmona(nn,pp, x,jerr, nban,ner,kwan,lava, jlack)
cc
cc   MONothetic Analysis
cc
cc   Program for divisive hierarchical clustering of binary data,
cc   using association analysis.
cc
cc   list of functions and subroutines:
cc       function kab

c Args
      integer nn, pp, jerr
c       nn   = number of objects
c       pp   = number of variables
c       jerr : error return code in {1,2,3,4}
      integer x(nn,pp), jlack(pp), nban(nn),ner(nn),kwan(nn),lava(nn)
c       x[i,j]: binary (0/1/NA) data (obs. i, var.j)
c               where "NA", missing values, are all values > 1

c Function called:
      integer kab
c VARs
      logical syn
      integer j, ja, jb, jnat, jma, jtel, jtelz, jtel2, jres
      integer j0,j1, jptwe
      integer a, b, c, d, k, ka, kb, kal, kalf, km
      integer l, lbb, laa, lcc, ldd, lee, lack,lama, lams
      integer nel, nelbb, nzf, nhalf, npass, nclu, myst, mysca
c--begin{-Wall}
      a=0
      b=0
      c=0
      d=0
      jma=0
      jtel=0
      jtelz=0
      lcc=0
      nelbb=0
c-- end{-Wall}
      nhalf=(nn+1)/2
      jptwe=(pp+4)/5
      myst=0
      do 70 l=1,nn
         mysca=0
         do 60 j=1,pp
            if(x(l,j) .gt. 1) mysca=mysca+1
 60      continue
         if(mysca .eq. pp) then
c     all variables missing for this object
            jerr=1
            return
         endif
         myst=myst+mysca
 70   continue
      if(myst.eq.0)go to 290

      lack=0
      do 100 j=1,pp
         j0=0
         j1=0
         do 80 l=1,nn
            if(x(l,j).eq.0) j0=j0+1
            if(x(l,j).eq.1) j1=j1+1
 80      continue
         jlack(j)=nn-j0-j1
         if(jlack(j).ne.0) lack=lack+1
         if(jlack(j).ge.nhalf) then
c     at least 50% of the objects have missing values for this variable
            jerr=2
            return
         endif
         if(j0.eq.0 .or. j1.eq.0) then
c           all non missing values are identical for this variable
            jerr=3
            return
         endif
 100  continue

      if(lack .eq. pp) then
c     all variables have missing values
         jerr=4
         return
      endif
cc
cc   filling in missing values
cc
      do 260 j=1,pp
         if(jlack(j) .ne. 0) then
            lama=-1
            syn=.true.
            do 240 ja=1,pp
               if(jlack(ja) .eq. 0) then
c               no missing in x[, ja]
                  a=0
                  b=0
                  c=0
                  d=0
                  do 230 k=1,nn
                     if(x(k,j).eq.1)go to 220
                     if(x(k,ja).eq.0)a=a+1
                     if(x(k,ja).eq.1)b=b+1
                     go to 230
 220                 if(x(k,ja).eq.0)c=c+1
                     if(x(k,ja).eq.1)d=d+1
 230              continue
                  kal=a*d - b*c
                  kalf=kab(kal)
                  if(kalf.ge.lama)then
                     lama=kalf
                     jma=ja
                     if(kal.lt.0) syn=.false.
                  endif
               endif
 240        continue

            do 250 l=1,nn
               if(x(l,j).gt. 1) then
c               missing
                  if(syn) then
                     x(l,j)=x(l,jma)
                  else
                     if(x(l,jma).eq.1) x(l,j)=0
                     if(x(l,jma).eq.0) x(l,j)=1
                  endif
               endif
 250        continue
         endif
 260  continue
c---  end of treating missing values ----

cc
cc   initialization
cc
 290  do 300 k=1,nn
         kwan(k)=0
         ner(k)=k
         lava(k)=0
 300  continue
      npass=1
      kwan(1)=nn
cc
cc   algorithm
cc
      nclu=1
      ka=1
C --- Loop ---
 310  kb=ka+kwan(ka)-1
      lama=-1
      jnat=pp
      do 370 j=1,pp
         if(nclu.eq.1)go to 330
         j0=0
         j1=0
         do 325 l=ka,kb
            nel=ner(l)
            if(x(nel,j).eq.0) j0=j0+1
            if(x(nel,j).eq.1) j1=j1+1
 325     continue
         if(j1.eq.0)go to 370
         if(j0.eq.0)go to 370
 330     jnat=jnat-1
         lams=0
         do 360 jb=1,pp
            if(jb .ne. j) then
               a=0
               b=0
               c=0
               d=0
               do 350 l=ka,kb
                  nel=ner(l)
                  if(x(nel,j).eq. 0) then
                     if(x(nel,jb).eq.0) a=a+1
                     if(x(nel,jb).eq.1) b=b+1
                  else
                     if(x(nel,jb).eq.0) c=c+1
                     if(x(nel,jb).eq.1) d=d+1
                  endif
 350           continue
               lams=lams+kab(a*d - b*c)
            endif
 360     continue
         if(lama .lt. lams) then
            lama=lams
            jtel =c+d
            jtelz=a+b
            jma=j
         endif
 370  continue
      if(jnat.lt.pp)go to 375
      kwan(ka)=-kwan(ka)
      go to 400
cc
cc    splitting
cc
 375  nel=ner(ka)
      if(x(nel,jma).eq.1)then
         nzf=0
         jtel2=jtel
      else
         nzf=1
         jtel2=jtelz
      endif
      jres=kb-ka+1-jtel2
      km=ka+jtel2
      l=ka
c  -- inner loop --
 378  nel=ner(l)
      if(x(nel,jma).eq.nzf)go to 380
      l=l+1
      if(l.lt.km)go to 378
      go to 390

 380  do 381 lbb=l,kb
         nelbb=ner(lbb)
         if(x(nelbb,jma) .ne. nzf) then
            lcc=lbb-1
            go to 382
         endif
 381  continue
 382  do 383 laa=l,lcc
         ldd=lcc+l-laa
         lee=ldd+1
         ner(lee)=ner(ldd)
 383  continue
      ner(l)=nelbb
      go to 378

 390  nclu=nclu+1
      nban(km)=npass
      kwan(ka)=jtel2
      kwan(km)=jres
      lava(km)=jma
      ka=ka+kwan(ka)
 400  if(kb.eq.nn)go to 500
 410  ka=ka+kab(kwan(ka))
      if(ka.gt.nn)go to 500
      if(kwan(ka).lt.2)go to 410
      go to 310

 500  npass=npass+1
      do 510 ka=1,nn
         if(kwan(ka).ge.2) go to 310
 510  continue
      end

cc
cc kab(j) = |j|
cc
      integer function kab(j)
      integer j
      kab=j
      if(j.lt.0) kab=-j
      return
      end
