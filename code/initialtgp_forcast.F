C $Header: /u/gcmpack/MITgcm/model/src/ini_theta.F,v 1.23 2005/11/08 23:01:10 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_THETA
C     !INTERFACE:
 
	SUBROUTINE initialtgp_forecast(myThid)
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

c----------------------------------------------
      call read_fld_xyz_rl('H_cs1020_m2s2n2k2.dat','',H2,0,myThid)
	call read_fld_xyz_rl('H_cs1020_k1o1p1q1.dat','',H1,0,myThid)
      call read_fld_xyz_rl('H_cs1020_mmmf.dat','',H0,0,myThid)
      call read_fld_xyz_rl('g_cs1020_m2s2n2k2.dat','',G2,0,myThid)
!      g 的单位为弧度
	call read_fld_xyz_rl('g_cs1020_k1o1p1q1.dat','',G1,0,myThid) 
      call read_fld_xyz_rl('g_cs1020_mmmf.dat','',G0,0,myThid)

	call EXCH_XYZ_RL(H1 , myThid )
	call EXCH_XYZ_RL(H2 , myThid )
	call EXCH_XYZ_RL(H0 , myThid )
	call EXCH_XYZ_RL(G1 , myThid )
	call EXCH_XYZ_RL(G2 , myThid )
	call EXCH_XYZ_RL(G0 , myThid )
c----------------------------------------------
      RETURN
      END
