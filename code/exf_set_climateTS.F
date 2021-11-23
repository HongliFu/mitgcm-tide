C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_set_runoff.F,v 1.3 2009/09/01 19:33:55 jmc Exp $
C $Name: checkpoint62h $

#include "EXF_OPTIONS.h"

      subroutine exf_set_climateTS(
     &     genfileclimate,  genperiodclimate,
     &     genfldclimate,gen0climate,gen1climate, 
     &     mytime, myiter, mythid )

c     ==================================================================
c     SUBROUTINE exf_set_runoff
c     ==================================================================
c
c     o set external forcing runoff
c       this is copy of exf_set_gen, but with the spatial interpolation
c       capability removed, so that this routine always expects
c       the runoff file to contain pre-interpolated data
c
c     started: Martin.Losch@awi.de 12-Feb-2009

c     ==================================================================
c     SUBROUTINE exf_set_runoff
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

C#include "EXF_PARAM.h"
C#include "EXF_CONSTANTS.h"

c     == routine arguments ==

      _RL genfldclimate(1-olx:snx+olx,1-oly:sny+oly,Nr,nsx,nsy)
      _RL gen0climate (1-olx:snx+olx,1-oly:sny+oly,Nr,nsx,nsy)
      _RL gen1climate (1-olx:snx+olx,1-oly:sny+oly,Nr,nsx,nsy)

      character*(22) genfileclimate
      _RL     mytime
      integer myiter
      integer mythid
      integer genperiodclimate

c     == local variables ==

      logical first, changed
      integer count0, count1
      integer year0, year1
      integer bi, bj, i, j,k
      _RL     fac
      character*(22) genfile0, genfile1

c     == external ==


c     == end of interface ==

      if ( genfileclimate .NE. ' ' .and. genperiodclimate .ne. 0 ) then


         if ( genperiodclimate .eq. -12 ) then
c     genperiod=-12 means input file contains 12 monthly means
c     record numbers are assumed 1 to 12 corresponding to
c     Jan. through Dec.
            call cal_GetMonthsRec(
     O           fac, first, changed,
     O           count0, count1,
     I           mytime, myiter, mythid
     &           )

         endif

         if ( first ) then
            genfile0=genfileclimate
               CALL READ_REC_3D_RL( genfile0, 32, Nr,
     &                  gen1climate, count0, myIter, myThid )      
         endif

         if (( first ) .or. ( changed )) then
            
            do bj = mybylo(mythid),mybyhi(mythid)
            do bi = mybxlo(mythid),mybxhi(mythid)
             do k=1,nr
              do j = 1,sny
                do i = 1,snx
                gen0climate(i,j,k,bi,bj)=gen1climate(i,j,k,bi,bj)
                gen1climate(i,j,k,bi,bj)=0.0
                enddo
              enddo
             enddo
            enddo
            enddo
            genfile1=genfileclimate

           CALL READ_REC_3D_RL( genfile1, 32, Nr,
     &                  gen1climate, count1, myIter, myThid )
     
         endif

c     Loop over tiles.
         do bj = mybylo(mythid),mybyhi(mythid)
          do bi = mybxlo(mythid),mybxhi(mythid)
           do k=1,nr
           do j = 1,sny
            do i = 1,snx
c     Interpolate linearly onto the  time.          
               if (maskC(i,j,k,bi,bj)>0.0) then
                genfldclimate(i,j,k,bi,bj) =
     &               fac * gen0climate(i,j,k,bi,bj) +
     &              (1 - fac) * gen1climate(i,j,k,bi,bj)
               else
                genfldclimate(i,j,k,bi,bj)=0.0
               endif 
               enddo
             enddo
            enddo
          enddo
         enddo
       
       _EXCH_XYZ_RL(genfldclimate,myThid)

      endif

      RETURN
      END

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
