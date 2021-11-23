C $Header: /u/gcmpack/MITgcm/model/src/ini_theta.F,v 1.23 2005/11/08 23:01:10 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
	SUBROUTINE tgp( bi, bj,myiter,mytime,myThid)
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
#include "cal.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_THETA
      INTEGER myiter,myThid
      _RL mytime
C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER bi, bj
      INTEGER I, J, K,kk,iyear_xfz
      _RL myDateSeconds
	  DOUBLE PRECISION ::   thour,ftime,year_xfz,rlon,tmpel
	  DOUBLE PRECISION ::   wv0(2),wv1(4),wv2(4),ihour
	  DOUBLE PRECISION ::   wf0(2),wu0(2)
	  DOUBLE PRECISION ::   wf1(4),wu1(4)
	  DOUBLE PRECISION ::   wf2(4),wu2(4)
	  DOUBLE PRECISION ::    w0(2),w1(4),w2(4)
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
c----------------------------------------------
      call fhl_get_thour(myiter,mytime,mythid,
     &myDateSeconds,iyear_xfz)
      thour=myDateSeconds/3600.0D0 !iday0*24.0+dte*(iinttmp*isplit+iext)/3600.0
      year_xfz=float(iyear_xfz)
      ftime=thour/24.0D0
      ihour=deltaT/3600.0d0
      call fuv0(wv0,iyear_xfz*1.0D0,0)
	  call fuv1(wv1,iyear_xfz*1.0D0,0)
      call fuv2(wv2,iyear_xfz*1.0D0,0)
	  call fu10_0(wf0,wu0,year_xfz,ftime)
	  call fu10_1(wf1,wu1,year_xfz,ftime)
      call fu10_2(wf2,wu2,year_xfz,ftime)

c      DO bj = myByLo(myThid), myByHi(myThid)
c      DO bi = myBxLo(myThid), myBxHi(myThid)
	   DO J=1-Oly,sNy+Oly
       DO I=1-Olx,sNx+Olx
          rlon=xC(I,J,bi,bj)*deg2rad
	      tmpel=0.0D0
	      do kk=1,0 !ntide1
	      tmpel=tmpel+elm0(i,j,kk,bi,bj)*wf0(kk)
     &         *dcos(w0(kk)*(thour)+wu0(kk)+wv0(kk))
	      enddo
	      do kk=1,1 !ntide1
	      tmpel=tmpel+elm1(i,j,kk,bi,bj)*wf1(kk)
     &         *dcos(w1(kk)*(thour)+rlon+wu1(kk)+wv1(kk))
!     &         *dcos(w1(kk)*(thour-8.0-dhour)+rlon+wu1(kk)+wv1(kk))
	      enddo
	      do kk=1,1 !ntide2
	      tmpel=tmpel+elm2(i,j,kk,bi,bj)*wf2(kk)
     &         *dcos(w2(kk)*(thour)+2.0d0*rlon+wu2(kk)+wv2(kk)) 
!     &         *dcos(w2(kk)*(thour-8.0-dhour)+2.0*rlon+wu2(kk)+wv2(kk)) 
	      enddo
	      elbar(i,j,bi,bj)=-tmpel
	enddo
	enddo
c	enddo
c	enddo
c----------------------------------------------
      RETURN
      END
c----------------------------------------------------------------------
	SUBROUTINE FUV0(UU,Y,ID)
	  IMPLICIT NONE
	  DOUBLE PRECISION :: UU(2)
	  DOUBLE PRECISION :: S0,H0,P0
	  DOUBLE PRECISION :: Y
	  INTEGER :: I,ID,IYC
	  IYC=INT((Y-1901.0D0)/4.0D0)
	  S0=277.025D0+129.3848D0*(Y-1900.D0)+13.17640D0*(ID+IYC)
	  H0=280.190D0-0.23872D0*(Y-1900.D0)+0.98565D0*(ID+IYC)
	  P0=334.385D0+40.66249D0*(Y-1900.D0)+0.11140D0*(ID+IYC)
	  UU(1)=S0-P0
	  UU(2)=2.0D0*S0
	  DO I=1,2
	    UU(I)=UU(I)*DATAN(1.0D0)/45.0D0
	  ENDDO
	  RETURN
	END SUBROUTINE FUV0

	SUBROUTINE FUV1(UU,Y,ID)
	  IMPLICIT NONE
	  DOUBLE PRECISION :: UU(4)
	  DOUBLE PRECISION :: S0,H0,P0
	  DOUBLE PRECISION :: Y
	  INTEGER :: I,ID,IYC
	  IYC=INT((Y-1901.0D0)/4.0D0)
	  S0=277.025D0+129.3848D0*(Y-1900.D0)+13.17640D0*(ID+IYC)
	  H0=280.190D0-0.23872D0*(Y-1900.D0)+0.98565D0*(ID+IYC)
	  P0=334.385D0+40.66249D0*(Y-1900.D0)+0.11140D0*(ID+IYC)
	  UU(1)=H0+90.0D0
	  UU(2)=-2.0D0*S0+H0+270.0D0
	  UU(3)=-H0+270.0D0
	  UU(4)=-3.0D0*S0+H0+P0+270.0D0
	  DO I=1,4
	    UU(I)=UU(I)*DATAN(1.0D0)/45.0D0
	  ENDDO
	  RETURN
	END SUBROUTINE FUV1

	SUBROUTINE FUV2(UU,Y,ID)
	  IMPLICIT NONE
	  DOUBLE PRECISION :: UU(4)
	  DOUBLE PRECISION :: S0,H0,P0  
	  DOUBLE PRECISION :: Y
	  INTEGER :: I,ID,IYC
	  IYC=INT((Y-1901.0D0)/4.0D0)
	  S0=277.025D0+129.3848D0*(Y-1900.D0)+13.17640D0*(ID+IYC)
	  H0=280.190D0-0.23872D0*(Y-1900.D0)+0.98565D0*(ID+IYC)
	  P0=334.385D0+40.66249D0*(Y-1900.D0)+0.11140D0*(ID+IYC)
	  UU(1)=-2.0D0*S0+2.0D0*H0
	  UU(2)=0.0D0
	  UU(3)=-3.0D0*S0+2.0D0*H0+P0
	  UU(4)=2.0D0*H0
	  DO I=1,4
	    UU(I)=UU(I)*DATAN(1.0D0)/45.0D0
	  ENDDO
	  RETURN
	END SUBROUTINE FUV2

	SUBROUTINE FU10_0(F2,U2,YY,DD)
	  IMPLICIT NONE
	  DOUBLE PRECISION :: YY,DD
	  DOUBLE PRECISION :: FF(11),UU(11)
	  DOUBLE PRECISION :: F2(2),U2(2)
	  CALL FU(FF,UU,YY,DD)
	  F2(1)=FF(1)
	  U2(1)=UU(1)
	  F2(2)=FF(2)
	  U2(2)=UU(2)
	  RETURN
	END SUBROUTINE FU10_0

	SUBROUTINE FU10_1(F4,U4,YY,DD)
	  IMPLICIT NONE
	  DOUBLE PRECISION :: YY,DD
	  DOUBLE PRECISION :: FF(11),UU(11)
	  DOUBLE PRECISION :: F4(4),U4(4)
	  CALL FU(FF,UU,YY,DD)
	  F4(1)=FF(5)
	  U4(1)=UU(5)
	  F4(2)=FF(3)
	  U4(2)=UU(3)
	  F4(3)=FF(4)
	  U4(3)=UU(4)
	  F4(4)=FF(3)
	  U4(4)=UU(3)
	  RETURN
	END SUBROUTINE FU10_1

	SUBROUTINE FU10_2(F4,U4,YY,DD)
	  IMPLICIT NONE
	  DOUBLE PRECISION :: YY,DD
	  DOUBLE PRECISION :: FF(11),UU(11)
	  DOUBLE PRECISION :: F4(4),U4(4)
	  CALL FU(FF,UU,YY,DD)
	  F4(1)=FF(8)
	  U4(1)=UU(8)
	  F4(2)=1.0D0
	  U4(2)=0.0D0
	  F4(3)=FF(8)
	  U4(3)=UU(8)
	  F4(4)=FF(10)
	  U4(4)=UU(10)
	  RETURN
	END SUBROUTINE FU10_2

	SUBROUTINE FU(FF,UU,YY,DD)
	  IMPLICIT NONE
	  DOUBLE PRECISION :: YY,DD
	  INTEGER :: I,J,K,IYC,IU45(2,13)
	  DOUBLE PRECISION :: COEFF(13,10)
	  DOUBLE PRECISION :: RAD,P0,PN0,FCOSU,FSINU
	  DOUBLE PRECISION :: FF(11),UU(11)
	  DATA IU45/-2,-1, 
     &		    -2, 0, 
     &		    -2, 1, 
     &		     0,-2, 
     &		     0,-1, 
     &		     0, 0, 
     &		     0, 1, 
     &		     0, 2, 
     &		     0, 3, 
     &		     2,-1, 
     &		     2, 0, 
     &		     2, 1, 
     &		     2, 2/
	  DATA COEFF/ 0.0000D0, 0.0000D0, 0.0000D0, 0.0008D0,-0.0657D0, 
     & 1.0000D0,-0.0649D0, 0.0000D0, 0.0000D0, 
     & 0.0000D0,-0.0534D0,-0.0218D0,-0.0059D0, 
     & -0.0023D0, 0.0432D0,-0.0028D0, 0.0000D0, 
     & 0.0000D0, 1.0000D0, 0.4143D0, 0.0387D0,
     & -0.0008D0, 0.0000D0, 0.0000D0, 0.0000D0, 0.0000D0, 
     & 0.0000D0, 0.0000D0, 0.0000D0,-0.0058D0, 0.1885D0, 
     & 1.0000D0, 0.0000D0, 0.0000D0, 0.0000D0, 
     & 0.0002D0,-0.0064D0,-0.0010D0, 0.0000D0, 
     & 0.0000D0, 0.0000D0, 0.0000D0, 0.0008D0,-0.0112D0, 
     & 1.0000D0, 0.0000D0, 0.0000D0, 0.0000D0, 
     & 0.0000D0,-0.0015D0,-0.0003D0, 0.0000D0, 
     & 0.0002D0, 0.0000D0, 0.0000D0, 0.0001D0,-0.0198D0, 
     & 1.0000D0, 0.1356D0,-0.0029D0, 0.0000D0, 
     & 0.0000D0, 0.0000D0, 0.0000D0, 0.0000D0, 
     & 0.0000D0, 0.0000D0, 0.0000D0, 0.0000D0,-0.0294D0, 
     & 1.0000D0, 0.1980D0,-0.0047D0, 0.0000D0, 0.0000D0,
     & -0.0152D0,-0.0098D0,-0.0057D0, 
     & -0.0037D0, 0.1496D0, 0.0296D0, 0.0000D0, 0.0000D0, 
     & 1.0000D0, 0.6398D0, 0.1342D0, 0.0086D0, 0.0000D0, 
     & 0.0000D0, 0.0000D0, 0.0000D0, 
     & 0.0000D0, 0.0000D0, 0.0000D0, 0.0005D0,-0.0373D0, 
     & 1.0000D0, 0.0000D0, 0.0000D0, 0.0000D0, 0.0000D0, 
     & 0.0006D0, 0.0002D0, 0.0000D0, 
     & 0.0000D0, 0.0000D0, 0.0000D0, 0.0000D0,-0.0366D0, 
     & 1.0000D0, 0.0000D0, 0.0000D0, 0.0000D0, 0.0047D0,
     & -0.2505D0,-0.1102D0,-0.0156D0, 
     & 0.0000D0, 0.0000D0, 0.0000D0, 0.0000D0,-0.0128D0, 
     & 1.0000D0, 0.2980D0, 0.0324D0, 0.0000D0, 0.0000D0, 
     & 0.0000D0, 0.0000D0, 0.0000D0/
	  RAD=DATAN(1.0D0)/45.0D0
	  IYC=(YY-1901.0D0)/4.0D0
	  P0=334.385D0+40.66249D0*(YY-1900)+0.11140D0*(DD+IYC)
	  PN0=100.84D0+19.3282D0*(YY-1900)+0.0530D0*(DD+IYC)
	  P0=P0*RAD
	  PN0=PN0*RAD
	  DO K=1,11
	    FCOSU=0.0D0
	    FSINU=0.0D0
	    IF(K.LE.10)THEN
		DO I=1,13
		  FCOSU=FCOSU+COEFF(I,K)*DCOS(IU45(1,I)*P0+IU45(2,I)*PN0)
		  FSINU=FSINU+COEFF(I,K)*DSIN(IU45(1,I)*P0+IU45(2,I)*PN0)
		ENDDO
	    ENDIF
	    IF(K.EQ.11)THEN
		FCOSU=-0.008D0*DCOS(-P0-2.0D0*PN0)+0.094D0*DCOS(-P0-PN0)+
     & 0.510D0*DCOS(-P0)-0.041D0*DCOS(P0-PN0)+1.418D0*DCOS(P0)+
     & 0.284D0*DCOS(P0+PN0)-0.008D0*DCOS(P0+2.0D0*PN0)
		FSINU=-0.008D0*DSIN(-P0-2.0D0*PN0)+0.094D0*DSIN(-P0-PN0)+
     & 0.510D0*DSIN(-P0)-0.041D0*DSIN(P0-PN0)+1.418D0*DSIN(P0)+
     & 0.284D0*DSIN(P0+PN0)-0.008D0*DSIN(P0+2.0D0*PN0)
	    ENDIF
	    FF(K)=DSQRT(FCOSU*FCOSU+FSINU*FSINU)
	    IF(FCOSU.EQ.0.0D0) THEN
		IF(FSINU.GT.0.0D0)THEN
		  UU(K)=90.0D0
		ELSE
		  UU(K)=270.0D0
		ENDIF
	    ELSE
		UU(K)=DATAN2(FSINU,FCOSU)/RAD
		IF(K.EQ.11.AND.UU(K).LT.0.0D0)THEN
		  UU(K)=UU(K)+360.0D0
		ENDIF
	    ENDIF
	  ENDDO
	  DO I=1,11
	    UU(I)=UU(I)*DATAN(1.0D0)/45.0D0
	  ENDDO
	  RETURN
	END SUBROUTINE FU

      subroutine fhl_get_thour(myiter,mytime,mythid,myDateSeconds,year0)

	implicit none
#include "cal.h"
      integer :: myiter,mythid
	  real :: mytime,myDateSeconds

      integer mydate(4)
      integer yearStartDate(4)
      integer difftime(4)
	  integer year0

       call cal_GetDate( myiter, mytime, mydate, mythid )

       year0            = int(mydate(1)/10000.)
       yearStartDate(1) = year0 * 10000 + 101
       yearStartDate(2) = 0
       yearStartDate(3) = mydate(3)
       yearStartDate(4) = mydate(4)

       CALL cal_TimePassed(yearStartDate,mydate,difftime,myThid)

       CALL cal_ToSeconds (difftime,myDateSeconds,myThid)

	return
	end 

