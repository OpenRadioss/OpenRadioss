Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
Chd|====================================================================
Chd|  FAIL_SYAZWAN_S                 source/materials/fail/biquad/fail_syazwan_s.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MMAIN8                        source/materials/mat_share/mmain8.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE FAIL_SYAZWAN_S(
     1     NEL     ,UPARAM   ,NUPARAM ,UVAR    ,NUVAR   ,
     2     TIME    ,NGL      ,IP      ,DPLA    ,PLA     ,
     3     SIGNXX  ,SIGNYY   ,SIGNZZ  ,SIGNXY  ,SIGNYZ  ,SIGNZX  ,
     4     DFMAX   ,NFUNC    ,IFUNC   ,ALDT    ,FOFF    ,
     5     NPF     ,TF      )
C!-----------------------------------------------
C!   I m p l i c i t   T y p e s
C!-----------------------------------------------
#include      "implicit_f.inc"
C!---------+---------+---+---+--------------------------------------------
C! VAR     | SIZE    |TYP| RW| DEFINITION
C!---------+---------+---+---+--------------------------------------------
C! NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C! NUPARAM |  1      | I | R | SIZE OF THE USER PARAMETER ARRAY
C! NUVAR   |  1      | I | R | NUMBER OF FAILURE ELEMENT VARIABLES
C!---------+---------+---+---+--------------------------------------------
C! MFUNC   |  1      | I | R | NUMBER FUNCTION USED FOR THIS USER LAW not used
C! KFUNC   | NFUNC   | I | R | FUNCTION INDEX not used
C! NPF     |  *      | I | R | FUNCTION ARRAY   
C! TF      |  *      | F | R | FUNCTION ARRAY 
C!---------+---------+---+---+--------------------------------------------
C! TIME    |  1      | F | R | CURRENT TIME
C! TIMESTEP|  1      | F | R | CURRENT TIME STEP
C! UPARAM  | NUPARAM | F | R | USER FAILURE PARAMETER ARRAY
C!---------+---------+---+---+--------------------------------------------
C! SIGNXX  | NEL     | F | W | NEW ELASTO PLASTIC STRESS XX
C! SIGNYY  | NEL     | F | W | NEW ELASTO PLASTIC STRESS YY
C! ...     |         |   |   |
C! ...     |         |   |   |
C!---------+---------+---+---+--------------------------------------------
C! UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C! FOFF     | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C!---------+---------+---+---+--------------------------------------------
#include "mvsiz_p.inc"
#include "scr17_c.inc"
#include "units_c.inc"
#include "comlock.inc"
#include "param_c.inc"
C!-----------------------------------------------
      INTEGER NEL, NUPARAM, NUVAR,NGL(NEL),IP,NFUNC, IFUNC(NFUNC)
      my_real TIME,UPARAM(NUPARAM),
     .   SIGNXX(NEL),SIGNYY(NEL),SIGNZZ(NEL),
     .   SIGNXY(NEL),SIGNYZ(NEL),SIGNZX(NEL),UVAR(NEL,NUVAR),
     .   DPLA(NEL),EPSP(NEL),FOFF(NEL),DFMAX(NEL),TDELE(NEL),
     .   PLA(NEL) ,ALDT(NEL)   
C-----------------------------------------------
C   VARIABLES FOR FUNCTION INTERPOLATION 
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NPF(*)
      my_real, INTENT(IN) :: TF(*)
      my_real
     .         FINTER
      EXTERNAL FINTER
C!-----------------------------------------------
C!   L o c a l   V a r i a b l e s
C!-----------------------------------------------
      INTEGER I,J,NINDX,INST,DINIT,IFORM
      INTEGER, DIMENSION(MVSIZ) :: INDX
      my_real
     .   C1     ,C2     ,C3     ,C4     ,C5     ,C6     ,
     .   N_VAL  ,SOFTEXP,REF_LEN,REG_SCALE,DAM_SF,MAX_DAM
      my_real
     .   LAMBDA ,DYDX   ,FAC(NEL),DC(NEL),P      ,SVM    ,
     .   TRIAX  ,COS3THETA,LODEP ,EPSFAIL,
     .   DET    ,XI  ,SXX     ,SYY    ,SZZ       ,EPFMIN

      !=======================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=======================================================================
      ! Recovering failure criterion parameters
      C1        = UPARAM(1) 
      C2        = UPARAM(2) 
      C3        = UPARAM(3)
      C4        = UPARAM(4) 
      C5        = UPARAM(5)
      C6        = UPARAM(6)
      IFORM     = NINT(UPARAM(7))
      DINIT     = NINT(UPARAM(8))
      DAM_SF    = UPARAM(9)
      MAX_DAM   = UPARAM(10)
      INST      = NINT(UPARAM(11))
      N_VAL     = UPARAM(12) 
      SOFTEXP   = UPARAM(13) 
      REF_LEN   = UPARAM(14) 
      REG_SCALE = UPARAM(15) 
      EPFMIN    = UPARAM(16)
c
      ! At first timestep, initialization of the critical damage and 
      ! the element size scaling factor
      IF (UVAR(1,1) == ZERO) THEN
        IF (IFUNC(1) > 0) THEN 
          DO I=1,NEL
            LAMBDA    = ALDT(I)/REF_LEN
            UVAR(I,1) = FINTER(IFUNC(1),LAMBDA,NPF,TF,DYDX)
            UVAR(I,1) = UVAR(I,1)*REG_SCALE
          ENDDO
        ELSE 
          UVAR(1:NEL,1) = ONE
        ENDIF
      ENDIF
c
      DO I=1,NEL
        ! Recover element size scaling
        FAC(I)   = UVAR(I,1)
      ENDDO
c
C-----------------------------------------------
c
      !====================================================================
      ! - COMPUTATION OF THE DAMAGE VARIABLE EVOLUTION
      !==================================================================== 
      ! Initialization of element failure index
      NINDX = 0  
      INDX(1:NEL) = 0
c
      ! Loop over the elements 
      DO I=1,NEL
c
        IF (FOFF(I) /= 0 .AND. DPLA(I) > ZERO) THEN
c
          ! Computation of hydrostatic stress, Von Mises stress, and stress triaxiality
          P   = THIRD*(SIGNXX(I) + SIGNYY(I) + SIGNZZ(I))     
          SXX = SIGNXX(I) - P                                 
          SYY = SIGNYY(I) - P                                 
          SZZ = SIGNZZ(I) - P                                 
          SVM = HALF*(SXX**2 + SYY**2 + SZZ**2)             
     .         + SIGNXY(I)**2+ SIGNZX(I)**2 + SIGNYZ(I)**2    
          SVM = SQRT(THREE*SVM) 
          TRIAX = P/MAX(EM20,SVM)
c
          ! Computation of Lode parameter
          DET = SXX*SYY*SZZ + TWO*SIGNXY(I)*SIGNZX(I)*SIGNYZ(I)- 
     .          SXX*SIGNYZ(I)**2-SZZ*SIGNXY(I)**2-SYY*SIGNZX(I)**2
          COS3THETA = ONE/MAX(EM20,SVM**3)
          COS3THETA = HALF*TWENTY7*DET*XI         
          IF(COS3THETA < -ONE) COS3THETA = -ONE
          IF(COS3THETA > ONE) COS3THETA = ONE
          LODEP = ONE - TWO*ACOS(XI)/PI
c
          ! Computation of the plastic strain at failure
          EPSFAIL = C1 + C2*TRIAX + C3*LODEP + C4*(TRIAX**2) + 
     .              C5*(LODEP**2) + C6*TRIAX*LODEP
          EPSFAIL = EPSFAIL*FAC(I)
          EPSFAIL = MAX(EPFMIN,EPSFAIL)
c
          ! Computation of the damage variable update 
          DFMAX(I) = DFMAX(I) + DPLA(I)/MAX(EPSFAIL,EM20)
          DFMAX(I) = MIN(DFMAX(I),ONE) 
c              
          ! Check element failure    
          IF (DFMAX(I) >= ONE) THEN
            FOFF(I)     = 0
            NINDX       = NINDX + 1
            INDX(NINDX) = I
          ENDIF
        ENDIF
      ENDDO
c     
c------------------------
      
       IF(NINDX>0)THEN
         DO J=1,NINDX
          I = INDX(J)     
#include "lockon.inc"
          WRITE(IOUT, 1000) NGL(I),TIME
          WRITE(ISTDO,1100) NGL(I),TIME
#include "lockoff.inc"
         END DO
       END IF         
C------------------
 1000 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER (SYAZWAN) el#',I10,
     .          ' AT TIME :',1PE12.4)     
 1100 FORMAT(1X,'DELETE SOLID ELEMENT NUMBER (SYAZWAN) el#',I10,
     .          ' AT TIME :',1PE12.4)     
      RETURN
      END
