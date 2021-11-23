C $Header: /u/gcmpack/MITgcm/model/src/ini_theta.F,v 1.23 2005/11/08 23:01:10 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_THETA
C     !INTERFACE:
 
	SUBROUTINE initial_climateTS(myThid)
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
      INTEGER myThid
C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER :: bi,bj,i,j,k
c----------------------------------------------
!==========================================================
       DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
       DO J=1-Oly,sNy+Oly
       DO I=1-Olx,sNx+Olx
!===========================================================
! 冷启动
!===========================================================
        do k=1,Nr
            tclimate_fhl(I,J,k,bi,bj)=0. _d 0
            sclimate_fhl(I,J,k,bi,bj)=0. _d 0
        enddo
!=============================================================
       enddo
       enddo
       enddo
       enddo
       
       call EXCH2_3D_RL(tclimate_fhl,Nr,myThid)
       call EXCH2_3D_RL(sclimate_fhl,Nr,myThid)
c----------------------------------------------
      RETURN
      END
