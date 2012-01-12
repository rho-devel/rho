c Utility, called from TWINS;
c	   { originally also from CLARA, PAM
c	     which now use C ind_2() in ./ind_2.h }
c original had MEET(), MEET2(), and MEET3()
c in the corresponding 3 source files

C meet(l,j) returns the *index* of dys() where diss. d(l,j) is stored:
C        d(l,j) == dys(meet(l,j))
c
      integer function meet(l,j)
      integer l,j

      if(l.gt.j) then
c     			l > j
         meet= (l-2)*(l-1)/2 + j+1
      else if(l.eq.j) then
         meet= 1
      else
c   			l < j
         meet= (j-2)*(j-1)/2 + l+1
      endif
      return
      end
