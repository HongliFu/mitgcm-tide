C $Header: /u/gcmpack/MITgcm/model/src/ini_theta.F,v 1.23 2005/11/08 23:01:10 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_THETA
C     !INTERFACE:
 
	SUBROUTINE cal_elmean(bi,bj,myiter,niter0my,myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_THETA                                      
C     | o Set model initial temperature field.                    
C     *==========================================================*
C     | There are several options for setting the initial         
C     | temperature file                                          
C     |  1. Inline code                                           
C     |  2. Vertical profile ( uniform T in X and Y )             
C     |  3. Three-dimensional data from a file. For example from  
C     |     Levitus or from a checkpoint file from a previous     
C     |     integration.                                          
C     | In addition to setting the temperature field we also      
C     | set the initial temperature tendency term here.           
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_THETA
      INTEGER myThid,myiter
C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      integer bi,bj
      integer i,j,k,nn,mm,iloopmy,niter0my
c----------------------------------------------
       iloopmy=myiter-niter0my
       DO J=1-Oly,sNy+Oly
       DO I=1-Olx,sNx+Olx
!          nn=iloopmy/18
!          mm=iloopmy-nn*18
           nn=iloopmy/nint(1800/deltaTClock)
           mm=iloopmy-nn*nint(1800/deltaTClock)
!=================================================================
! 冷启动,前900步（25小时）生成elday
!=================================================================
!       IF (iloopmy.GT.900.and.mm.EQ.0) THEN
!         elmeanfhl(i,j,bi,bj)=sum(elday(i,j,1:Nr,bi,bj))/real(nr)
!         do k=1,Nr-1
!         elday(I,J,k,bi,bj)=elday(i,j,k+1,bi,bj)
!         enddo
!         elday(i,j,Nr,bi,bj)=etan(i,j,bi,bj)
!       elseif(iloopmy.LE.900.and.mm.EQ.0)then
!         elday(i,j,nn,bi,bj)=etan(i,j,bi,bj)
!         elmeanfhl(i,j,bi,bj)=sum(elday(i,j,1:nn,bi,bj))/float(nn)
!        endif
!===================================================================
! 热启动，eldday的初始值从文件中读出
!==================================================================
          IF(mm.EQ.0) THEN
!          do k=1,Nr-1
          do k=1,51
          elday(I,J,k,bi,bj)=elday(i,j,k+1,bi,bj)
          enddo
!          elday(i,j,Nr,bi,bj)=etan(i,j,bi,bj)
         elday(i,j,52,bi,bj)=etan(i,j,bi,bj)
         elmeanfhl(i,j,bi,bj)=sum(elday(i,j,1:Nr,bi,bj))/real(nr) !for 25hr mean
 !        elmeanfhl(i,j,bi,bj)=sum(elday(i,j,3:Nr,bi,bj))/real(nr-2) for 24 mean
 !         elmeanfhl(i,j,bi,bj)=sum(elday(i,j,1:Nr+2,bi,bj))/real(nr+2) for 26 mean
          elmeanfhl(i,j,bi,bj)=sum(elday(i,j,1:52,bi,bj))/real(nr+2) !for 26 mean
          ENDIF
!===================================================================
        enddo
        enddo

c        _EXCH_XY_RL(elmeanfhl,myThid)
c        _EXCH_XYZ_RL(elday,myThid)

C        write(*,*)(elmeanfhl(1,1,1,1)),
C     &  iLoopmy,'11111'
   
c----------------------------------------------
      RETURN
      END
