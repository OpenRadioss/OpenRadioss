Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    fail_connect   ../engine/source/materials/fail/connect/fail_connect.F
!||--- called by ------------------------------------------------------
!||    suser43        ../engine/source/elements/solid/sconnect/suser43.F
!||--- calls      -----------------------------------------------------
!||    finter         ../engine/source/tools/curve/finter.F
!||====================================================================
      SUBROUTINE FAIL_CONNECT(FAIL,NEL     ,
     1           NUVAR   ,NVARTMP ,UVAR    ,VARTMP  ,NGL     ,
     2           TIME    ,TIMESTEP,EPS1    ,EPS2    ,EPS3    ,
     4           EPSP    ,OFFG    ,OFFL    ,IPG     ,
     5           SIGNZZ  ,SIGNYZ  ,SIGNZX  ,DEIN    ,DEIT    ,
     6           DFMAX   ,LF_DAMMX,TDELE   ,AREA    ,SOFT    )
c-----------------------------------------------
!     Modules
!-----------------------------------------------
      use fail_param_mod
      use table_mod
      use table_mat_vinterp_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUVAR   |  1      | I | R | NUMBER OF FAILURE ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C TIMESTEP|  1      | F | R | CURRENT TIME STEP
C---------+---------+---+---+--------------------------------------------
C EPS1    | NEL     | F | W | LOCAL STRAIN1
C EPS2    | NEL     | F | W | LOCAL STRAIN2
C EPS3    | NEL     | F | W | LOCAL STRAIN3
C EPSP    | NEL     | F | W | STRAIN RATE
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFFG    | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "units_c.inc"
#include "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NUVAR,NVARTMP,IPG
      INTEGER, DIMENSION(NEL), INTENT(IN) :: NGL
      INTEGER, DIMENSION(NEL,NVARTMP), INTENT(INOUT) :: VARTMP
      my_real, DIMENSION(NEL,NUVAR)  , INTENT(INOUT) :: UVAR
      INTEGER, INTENT(IN) :: LF_DAMMX
      my_real :: TIME,TIMESTEP
      my_real, DIMENSION(NEL) :: OFFG,OFFL,EPS1,EPS2,EPS3,EPST,EPSP,
     .   SIGNZZ,SIGNYZ,SIGNZX,DEIN,DEIT,TDELE,AREA,SOFT
      my_real ,DIMENSION(NEL,LF_DAMMX),INTENT(INOUT) :: DFMAX
      type(fail_param_)  ,intent(in) :: fail       !< failure model data structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IDEL,NINDX,NINDXA,NINDXE,NINDXS,FUNN,FUNT,IFAILS,IFAILE,ISOLID,ISYM
      INTEGER, DIMENSION(NEL) :: INDX,INDXA,INDXE,INDXS
      my_real C1,C2,C3,C4,CM,DAMT,DEFO,DTIME,DSOFT,AREASCALE,
     .   MAXDN,MAXDT,MAXEI,MAXEN,MAXET,FAC1,FAC2,EXPN,EXPT,NN,NT
      my_real, DIMENSION(NEL) :: EI,EN,ET,DAM1,DAM2,DAM3,DAM4
      my_real ,dimension(nel) :: facn,fact,dydx
C=======================================================================
      IFAILS = FAIL%IPARAM(1)    
      IFAILE = FAIL%IPARAM(2)
      ISOLID = FAIL%IPARAM(3)  
      ISYM   = FAIL%IPARAM(4)   !
!
      MAXDN  = FAIL%UPARAM(1)         
      MAXDT  = FAIL%UPARAM(2)         
      EXPN   = FAIL%UPARAM(3)         
      EXPT   = FAIL%UPARAM(4)         
      FAC1   = FAIL%UPARAM(5)         
      FAC2   = FAIL%UPARAM(6)            
      MAXEI  = FAIL%UPARAM(7) 
      MAXEN  = FAIL%UPARAM(8) 
      MAXET  = FAIL%UPARAM(9) 
      NN     = FAIL%UPARAM(10)
      NT     = FAIL%UPARAM(11)
      DTIME  = FAIL%UPARAM(12)
      DSOFT  = FAIL%UPARAM(13)  
      AREASCALE = FAIL%UPARAM(14)        
C-----------------------------------------------
C     USER VARIABLES INITIALIZATION
C-----------------------------------------------
      IF (ISIGI == ZERO) THEN
        IF ((UVAR(1,1) == ZERO).AND.(OFFL(1) == ONE)) THEN 
          DO I=1,NEL
            UVAR(I,1)= ONE  
          ENDDO
        ENDIF
        IF (UVAR(1,8) == ZERO) THEN
          DO I=1,NEL         
            UVAR(I,8)= AREA(I)  
          ENDDO  
        ENDIF 
      ENDIF 
C-----------------------------------------------
      DO I=1,NEL
        DAM1(I) = UVAR(I,2)                   
        DAM2(I) = UVAR(I,3)                   
        DAM3(I) = UVAR(I,4)                   
        DAM4(I) = UVAR(I,5)                   
        EN(I)   = UVAR(I,6)                   
        ET(I)   = UVAR(I,7)                   
      ENDDO
      facn(1:nel) = one
      fact(1:nel) = one                                
C-----------------------------------------------
      if (ifails > 0) then
        if (fail%table4d(1)%notable > 0) then
          call table_mat_vinterp(fail%table4d(1),nel,nel,vartmp(1:nel,1:1),epsp,facn,dydx)
        endif
        if (fail%table4d(2)%notable > 0) then
          call table_mat_vinterp(fail%table4d(2),nel,nel,vartmp(1:nel,2:2),epsp,fact,dydx)
        endif
      end if
C-----------------------------------------------
      NINDX  = 0
      NINDXE = 0
      NINDXS = 0  
      NINDXA = 0  
c
      DO I=1,NEL
        IDEL = 0
        ET(I) = ET(I) + DEIT(I)
        IF (ISYM == 1 .AND. SIGNZZ(I) < ZERO) THEN
          FAC1  = ZERO
        ELSE
          EN(I) = EN(I) + DEIN(I)
        ENDIF
        EI(I) = EN(I) + ET(I)
        C1 = ZERO
        C2 = ZERO
        C3 = ZERO
        C4 = ZERO
        CM = ZERO
c-----------------------------
c-----  Energy criterion
c-----------------------------
        IF (IFAILE == 1) THEN   !  global energy criterion
          C1 = EI(I)/MAXEI
          DFMAX(I,7) = MAX(DFMAX(I,7),C1)
          DFMAX(I,7) = MIN(DFMAX(I,7),ONE)
          IF (C1 > ONE) THEN
            IF (DAM1(I) == ZERO) THEN
              NINDXE = NINDXE+1
              INDXE(NINDXE) = I
            ENDIF
            DAM1(I) = DAM1(I) + C1 * TIMESTEP
          ENDIF
c         
        ELSEIF (IFAILE == 2) THEN  !  orthotropic energy rupture
          C2 = (ET(I)/MAXET)**NT+ (EN(I)/MAXEN)**NN     
          DFMAX(I,5) = MAX(DFMAX(I,5),EN(I)/MAXEN)
          DFMAX(I,5) = MIN(DFMAX(I,5),ONE)
          DFMAX(I,6) = MAX(DFMAX(I,6),ET(I)/MAXET)
          DFMAX(I,6) = MIN(DFMAX(I,6),ONE)    
          IF (C2 > ONE) THEN
            IF (DAM1(I) == ZERO) THEN
              NINDXE = NINDXE+1
              INDXE(NINDXE) = I
            ENDIF
            DAM1(I) = DAM1(I) + C2*TIMESTEP
          ENDIF
        ELSEIF (IFAILE == 3) THEN  ! total + component
          C1 =  EI(I)/MAXEI
          DFMAX(I,7) = MAX(DFMAX(I,7),C1)
          DFMAX(I,7) = MIN(DFMAX(I,7),ONE)
          C2 = (ET(I)/MAXET)**NT+ (EN(I)/MAXEN)**NN
          DFMAX(I,5) = MAX(DFMAX(I,5),EN(I)/MAXEN)
          DFMAX(I,5) = MIN(DFMAX(I,5),ONE)
          DFMAX(I,6) = MAX(DFMAX(I,6),ET(I)/MAXET)
          DFMAX(I,6) = MIN(DFMAX(I,6),ONE)            
          C1 = MAX(C1,C2)
          DFMAX(I,8) = MAX(DFMAX(I,8),C1)
          DFMAX(I,8) = MIN(DFMAX(I,8),ONE)
          IF (C1 > ONE) THEN
            IF (DAM1(I) == ZERO) THEN
              NINDXE = NINDXE+1
              INDXE(NINDXE) = I
            ENDIF
            DAM1(I) = DAM1(I) + C1*TIMESTEP
          ENDIF
        ENDIF
c-----------------------------
c------ max strain criterion
c-----------------------------
        IF (IFAILS > 0) THEN
          EPST(I) = SQRT(EPS2(I)**2 + EPS3(I)**2)   ! EPS2 = EPSZX and EPS3 Epsxy                                  
          C3 = FAC1 * FACN(I) * EPS1(I) / MAXDN  
          C4 = FAC2 * FACT(I) * EPST(I) / MAXDT
          DFMAX(I,2) = MAX(DFMAX(I,2),C3)
          DFMAX(I,2) = MIN(DFMAX(I,2),ONE)
          DFMAX(I,3) = MAX(DFMAX(I,3),C4)
          DFMAX(I,3) = MIN(DFMAX(I,3),ONE)
          IF (IFAILS == 1) THEN
c           unidirectional rupture in strain
            C3 = MAX(C3,C4)
            DFMAX(I,4) = MAX(DFMAX(I,4),C3)
            DFMAX(I,4) = MIN(DFMAX(I,4),ONE)
            IF (C3 > ONE) THEN
              IF (DAM3(I) == ZERO) THEN
                NINDXS = NINDXS+1
                INDXS(NINDXS) = I
              ENDIF
              DAM3(I) = DAM3(I) + C3*TIMESTEP
            ENDIF
          ELSEIF (IFAILS == 2) THEN
c           multidirectional rupture in strain
            CM = ABS(C3)**EXPN + ABS(C4)**EXPT
            DFMAX(I,4) = MAX(DFMAX(I,4),CM)
            DFMAX(I,4) = MIN(DFMAX(I,4),ONE)
            IF (CM > ONE) THEN
              IF (DAM3(I) == ZERO) THEN
                NINDXS = NINDXS+1
                INDXS(NINDXS) = I
              ENDIF
              DAM3(I) = DAM3(I) + CM* TIMESTEP
            ENDIF
          ENDIF
        ENDIF
c-----------------------------
        DAMT = MAX(DAM1(I),DAM3(I))
        IF (DAMT > DTIME) THEN
          IDEL =1
          SOFT(I) = ZERO
        ELSEIF (DSOFT == ONE) THEN
          SOFT(I) = ONE - DAMT/MAX(DTIME,EM20)
        ELSEIF (DSOFT > ZERO) THEN
          SOFT(I) = (ONE - DAMT/MAX(DTIME,EM20))**DSOFT
        ENDIF  
c-----------------------------
        IF (IDEL == 1 .AND. OFFL(I) == ONE) THEN
          OFFL(I) = ZERO       ! local integ point rupture    
          NINDX = NINDX+1
          INDX(NINDX) = I
          TDELE(I) = TIME  
        ENDIF 
c-----------------------------
c---    deformed elements check
        IF (AREASCALE > ZERO .AND. OFFG(I) == ONE ) THEN
          DEFO = UVAR(I,8) * AREASCALE
          IF (AREA(I) > DEFO) THEN
            OFFL(I) = ZERO
            NINDXA = NINDXA+1
            INDXA(NINDXA) = I
            TDELE(I) = TIME  
            ISOLID   = 1
          ENDIF
        ENDIF
c-------------------------------
        UVAR(I,1) = SOFT(I)              
        UVAR(I,2) = DAM1(I)
        UVAR(I,3) = DAM2(I)
        UVAR(I,4) = DAM3(I)
        UVAR(I,5) = DAM4(I)
        UVAR(I,6) = EN(I)            
        UVAR(I,7) = ET(I)   
c--     Maximum Damage storing for output : 0 < DFMAX < 1
        DFMAX(I,1) = MIN(ONE,MAX(DFMAX(I,1),DAMT/ MAX(DTIME,EM20))) 
c
      ENDDO     ! I=1,NEL                        
C-----------------------------------------------
      IF (NINDXE > 0) THEN
        DO J=1,NINDXE
          I = INDXE(J)
#include "lockon.inc"
          WRITE(IOUT ,1000) NGL(I),IPG,EI(I)
          WRITE(ISTDO,1100) NGL(I),IPG,EI(I),TIME
#include "lockoff.inc"
        END DO
      ELSEIF (NINDXS > 0) THEN
        DO J=1,NINDXS
          I = INDXS(J)
#include "lockon.inc"
          WRITE(IOUT ,1600) NGL(I),IPG,EPS1(I),EPST(I)
          WRITE(ISTDO,1700) NGL(I),IPG,EPS1(I),EPST(I),TIME
#include "lockoff.inc"
        END DO
      ELSEIF (NINDX > 0) THEN
        DO J=1,NINDX
          I = INDX(J)
#include "lockon.inc"
          WRITE(IOUT ,1200) NGL(I),IPG
          WRITE(ISTDO,1300) NGL(I),IPG,TIME
#include "lockoff.inc"
        END DO
      ELSEIF (NINDXA > 0) THEN
        DO J=1,NINDXA
          I = INDXA(J)
#include "lockon.inc"
          WRITE(IOUT ,1400) NGL(I),AREA(I)
          WRITE(ISTDO,1500) NGL(I),AREA(I),TIME
#include "lockoff.inc"
        END DO
      ENDIF         
C-----------------------------------------------
 1000 FORMAT(5X,'START DAMAGE CONNECTION ELEMENT ',I10,
     .          ' INTEGRATION POINT',I2,', ENERGY=',1PE16.9)
 1100 FORMAT(5X,'START DAMAGE CONNECTION ELEMENT ',I10,
     .          ' INTEGRATION POINT',I2,', ENERGY=',1PE16.9,
     .          ' AT TIME ',1PE16.9)
 1200 FORMAT(5X,'FAILURE CONNECTION SOLID ELEMENT ',I10,
     .          ' INTEGRATION POINT',I2)
 1300 FORMAT(5X,'FAILURE CONNECTION SOLID ELEMENT ',I10,
     .          ' INTEGRATION POINT',I2,' AT TIME ',1PE16.9)
 1400 FORMAT(5X,'FAILURE CONNECTION SOLID ELEMENT ',I10,
     .         ',   AREA(LIMIT REACHED) =',1PE16.9)
 1500 FORMAT(5X,'FAILURE CONNECTION SOLID ELEMENT ',I10,
     .          ',  AREA(LIMIT REACHED) :',1PE16.9,' AT TIME ',1PE16.9)
 1600 FORMAT(5X,'START DAMAGE CONNECTION ELEMENT ',I10,
     .          ' INTEGRATION POINT',I2,', EPSN=',1PE16.9,', EPST=',1PE16.9)
 1700 FORMAT(5X,'START DAMAGE CONNECTION ELEMENT ',I10,
     .          ' INTEGRATION POINT',I2,', EPSN=',1PE16.9,', EPST=',1PE16.9,
     .          ' AT TIME ',1PE16.9)
C-----------------------------------------------
      RETURN
      END
