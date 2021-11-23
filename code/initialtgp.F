C $Header: /u/gcmpack/MITgcm/model/src/ini_theta.F,v 1.23 2005/11/08 23:01:10 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_THETA
C     !INTERFACE:

	SUBROUTINE initialtgp(myThid)
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
      INTEGER bi, bj
      INTEGER I, J, K,kk
c----------------------------------------------
	  _RL w0(2),w1(4),w2(4),amp0(2),amp1(4),amp2(4),lat
	  W0(1)=0.00950112D0
	  W0(2)=0.01916424D0
	  W1(1)=0.26251617D0
	  W1(2)=0.24335188D0
	  W1(3)=0.26108260D0
	  W1(4)=0.23385075D0
	  W2(1)=0.50586805D0
	  W2(2)=0.52359878D0
	  W2(3)=0.49636692D0
	  W2(4)=0.52503234D0
	  AMP0(1)=0.015D0
	  AMP0(2)=0.029D0
	  AMP1(1)=0.104D0     ! 0.104D0 Arbic_etal_DSR_2004 or 0.098D0 Ye Anle ?
	  AMP1(2)=0.070D0
	  AMP1(3)=0.033D0     ! 0.033D0 Arbic_etal_DSR_2004 or 0.032D0 Ye Anle ?
	  AMP1(4)=0.013D0
	  AMP2(1)=0.168D0
	  AMP2(2)=0.078D0
	  AMP2(3)=0.032D0
	  AMP2(4)=0.021D0
	DO bj = myByLo(myThid), myByHi(myThid)
      DO bi = myBxLo(myThid), myBxHi(myThid)
	  DO J=1-Oly,sNy+Oly
      DO I=1-Olx,sNx+Olx
	  lat = yC(I,J,bi,bj)
c	  rlat=alat(i,j)*pi/180.0d0
	  DO KK=1,2
		ELM0(I,J,KK,bi,bj)=AMP0(KK)*
     &(0.5D0-1.5D0*SIND(deg2rad*lat)*SIND(deg2rad*lat))
        ENDDO
	  do kk=1,4 !ntide1
	    elm1(i,j,kk,bi,bj)=amp1(kk)*sin(2.0d0*deg2rad*lat)
	  enddo
	  do kk=1,4 !ntide2
	    elm2(i,j,kk,bi,bj)=amp2(kk)*cos(deg2rad*lat)*cos(deg2rad*lat)
	  enddo
	enddo
	enddo
	enddo
	enddo
	_EXCH_XYZ_RL(elm0 , myThid )
	_EXCH_XYZ_RL(elm1 , myThid )
	_EXCH_XYZ_RL(elm2 , myThid )
c----------------------------------------------
      RETURN
      END
