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
!||    fail_snconnect   ../engine/source/materials/fail/snconnect/fail_snconnect.F
!||--- called by ------------------------------------------------------
!||    suser43          ../engine/source/elements/solid/sconnect/suser43.F
!||--- calls      -----------------------------------------------------
!||    finter           ../engine/source/tools/curve/finter.F
!||====================================================================
      SUBROUTINE FAIL_SNCONNECT (FAIL      ,
     1           NEL     ,NUVAR   ,NVARTMP ,UVAR    ,VARTMP  ,
     2           TIME    ,NGL     ,IPG     ,NPG     ,NDAMF   ,
     4           EPSD    ,PLA     ,OFFG    ,OFFL    ,
     5           SIGNZZ  ,SIGNYZ  ,SIGNZX  ,SYM     ,AREA    ,
     6           DMG     ,DAMT    ,DFMAX   ,TDELE   )
C-----------------------------------------------
c  SNCONNECT failure model for solid spotwelds
c-----------------------------------------------
!     Modules
!-----------------------------------------------
      use fail_param_mod
      use table_mod
      use table_mat_vinterp_mod
C-----------------------------------------------
#include      "implicit_f.inc"
C---------+---------+---+---+--------------------------------------------
C VAR     | SIZE    |TYP| RW| DEFINITION
C---------+---------+---+---+--------------------------------------------
C NEL     |  1      | I | R | SIZE OF THE ELEMENT GROUP NEL 
C NUVAR   |  1      | I | R | NUMBER OF FAILURE ELEMENT VARIABLES
C---------+---------+---+---+--------------------------------------------
C TIME    |  1      | F | R | CURRENT TIME
C EPSP    | NEL     | F | W | STRAIN RATE
C---------+---------+---+---+--------------------------------------------
C UVAR    |NEL*NUVAR| F |R/W| USER ELEMENT VARIABLE ARRAY
C OFFG    | NEL     | F |R/W| DELETED ELEMENT FLAG (=1. ON, =0. OFF)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "units_c.inc"
#include "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NUVAR,NVARTMP,IPG,NPG,NDAMF
      INTEGER ,INTENT(IN) :: NGL(NEL)
      INTEGER ,DIMENSION(NEL,NVARTMP), INTENT(INOUT) :: VARTMP
      my_real, DIMENSION(NEL,NUVAR)  , INTENT(INOUT) :: UVAR
      my_real :: TIME
      my_real :: DAMT(NEL,NDAMF)
      my_real , DIMENSION(NEL) :: OFFG,OFFL,EPSD,PLA,
     .   SIGNZZ,SIGNYZ,SIGNZX,SYM,DMG,DFMAX,TDELE,AREA
      type(fail_param_)  ,intent(in) :: fail       !< failure model data structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      integer i,j,idel,nindx,nindxd,nindxa,isolid,isym
      integer indx(nel),indxd(nel),indxa(nel)
      my_real a2,b2,a3,b3,xscale2,xscale3,svmn,svmt,t1,t2,ttn,tts,fct,
     .  dama,ssym,cphi,sphi,areascale,defo
      my_real ,dimension(nel) :: fun2n,fun2t,fun3n,fun3t,phi,pla1,pla2
      my_real ,dimension(nel) :: dydx
C=======================================================================
c Internal VARIABLES :
c-----------------
c UVAR1  initial plastic elongation 
c UVAR2  final plastic elongation (at rupture)
c-----------------
c Damage output VARIABLES :
c-----------------
c DAMT1  damage factor d,   where (sig = sig *(1-d))
c DAMT2  damage function at damage start
c DAMT3  damage function at rupture 
C=======================================================================
      ISYM      = FAIL%IPARAM(1)  
      ISOLID    = FAIL%IPARAM(2)  

      A2        = FAIL%UPARAM(1)          
      B2        = FAIL%UPARAM(2)          
      A3        = FAIL%UPARAM(3)          
      B3        = FAIL%UPARAM(4)          
      AREASCALE = FAIL%UPARAM(5)        
C-----------------------------------------------
      IF (UVAR(1,3) == ZERO) THEN
        UVAR(:,3)= AREA(:)    ! initial area at time = 0   
      ENDIF
      PLA1(:) = UVAR(:,1)
      PLA2(:) = UVAR(:,2)                             
C-----------------------------------------------
C    RATE FUNCTIONS INERPOLATION
C-----------------------------------------------
      if (fail%table4d(1)%notable > 0) then
        call table_mat_vinterp(fail%table4d(1),nel,nel,vartmp(1:nel,1:1),epsd,fun2n,dydx)
      else
        fun2n(1:nel) = one
      endif
      if (fail%table4d(2)%notable > 0) then
        call table_mat_vinterp(fail%table4d(2),nel,nel,vartmp(1:nel,2:2),epsd,fun2t,dydx)
      else
        fun2t(1:nel) = one
      endif
      if (fail%table4d(3)%notable > 0) then
        call table_mat_vinterp(fail%table4d(3),nel,nel,vartmp(1:nel,3:3),epsd,fun3n,dydx)
      else
        fun3n(1:nel) = one
      endif
      if (fail%table4d(4)%notable > 0) then
        call table_mat_vinterp(fail%table4d(4),nel,nel,vartmp(1:nel,4:4),epsd,fun3t,dydx)
      else
        fun3t(1:nel) = one
      endif
C-----------------------------------------------
      NINDX  = 0  
      NINDXD = 0  
      NINDXA = 0  
c
      DO I=1,NEL
        IDEL  = 0
        DAMA  = ZERO
        SSYM  = SIN(SYM(I))
        SVMN  = ABS(SIGNZZ(I))                                                   
        SVMT  = SQRT(SIGNYZ(I)**2 + SIGNZX(I)**2)
        PHI(I)= ATAN(SVMN/MAX(EM20,SVMT))
        SPHI  = SIN(PHI(I))
        CPHI  = COS(PHI(I))
c
        IF (PLA1(I) == ZERO) THEN   ! No damage yet
          IF (ISYM == 1 .AND. SIGNZZ(I)<= ZERO) THEN
            T1 = ZERO
          ELSE
            T1  = SPHI/(ONE-A2*SSYM)/FUN2N(I)
          ENDIF
          T2  = CPHI/FUN2T(I)
          TTN = T1*PLA(I)
          TTS = T2*PLA(I)
          FCT = (TTN**B2 + TTS**B2)**(ONE/B2)
          DAMT(I,2) = MIN(FCT, ONE)
          IF (FCT > ONE ) THEN 
            PLA1(I) = (T1**B2 + T2**B2)**(-ONE/B2)
            IF (ISYM == 1 .AND. SIGNZZ(I) <= ZERO)THEN
              T1 = ZERO
            ELSE
              T1  = SPHI/(ONE-A3*SSYM)/FUN3N(I)
            ENDIF
            T2  = CPHI/FUN3T(I)
            TTN = T1*PLA(I)
            TTS = T2*PLA(I)
            FCT = (TTN**B3 + TTS**B3) **(ONE / B3)
            PLA2(I)   = (T1**B3 + T2**B3)**(-ONE/B3)
            DAMT(I,1) = (PLA(I)-PLA1(I))/MAX(EM20,(PLA2(I) - PLA1(I)))
            DAMT(I,1) = MIN(DAMT(I,1), ONE)
            DAMT(I,3) = MIN(FCT, ONE)
            DAMA = DAMT(I,1)
            NINDXD = NINDXD+1
            INDXD(NINDXD) = I
          ENDIF
c          
        ELSE     ! (PLA1(I) > ZERO) 
          IF (ISYM == 1 .AND. SIGNZZ(I) <= ZERO) THEN
            T1 = ZERO
          ELSE
            T1  = SPHI/(ONE-A3*SSYM)/FUN3N(I)
          ENDIF
          T2  = CPHI/FUN3T(I)
          TTN = T1*PLA(I)                        
          TTS = T2*PLA(I)      
          FCT = (TTN**B3 + TTS**B3) **(ONE / B3)  
          PLA2(I)   = (T1**B3 + T2**B3)**(-ONE/B3)
          DAMT(I,1) = (PLA(I)-PLA1(I))/MAX(EM20,(PLA2(I) - PLA1(I)))
          DAMT(I,1) = MIN(DAMT(I,1), ONE)
          DAMT(I,3) = MIN(FCT, ONE)
          DAMA = DAMT(I,1)
c-----    check if rupture ...
          IF (FCT > ONE) THEN
            IDEL =1
            DAMA = ONE
          ENDIF
        ENDIF
        DMG(I) = MAX(DMG(I),DAMA)
        DMG(I) = MIN(DMG(I),ONE)   
c-----------------------------
        IF (IDEL == 1 .AND. OFFL(I) == ONE) THEN
          OFFL(I) = ZERO       ! local integ point rupture    
          NINDX = NINDX+1
          INDX(NINDX) = I
          TDELE(I) = TIME  
        ENDIF 
C           
        UVAR(I,1) = PLA1(I)
        UVAR(I,2) = PLA2(I)
C-------------  Maximum Damage storing for output : 0 < DFMAX < 1 
        DFMAX(I) = MIN(ONE,MAX(DFMAX(I),DAMT(I,1)))
c        
c---    deformed elements check
        IF (AREASCALE > ZERO ) THEN
          DEFO = UVAR(I,3) * AREASCALE
          IF (AREA(I) > DEFO .AND. OFFG(I) == ONE) THEN
            OFFL(I) = ZERO
            NINDXA = NINDXA+1
            INDXA(NINDXA) = I
            TDELE(I) = TIME  
            ISOLID   = 1
          ENDIF
        ENDIF
c-------------------------------        
      ENDDO     ! I=1,NEL                           
C-----------------------------------------------
      IF (NINDXD > 0) THEN
        DO J=1,NINDXD
          I = INDXD(J)
#include "lockon.inc"
          WRITE(IOUT ,1000) NGL(I),IPG,PLA1(I)
          WRITE(ISTDO,1100) NGL(I),IPG,PLA1(I),TIME
#include "lockoff.inc"
        END DO
      ELSEIF (NINDX > 0) THEN
        DO J=1,NINDX
          I = INDX(J)
#include "lockon.inc"
          WRITE(IOUT ,1200) NGL(I),IPG,PLA2(I)
          WRITE(ISTDO,1300) NGL(I),IPG,PLA2(I),TIME
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
     .          ' INTEGRATION POINT',I2,', PLASTIC ELONGATION =',1PE16.9)
 1100 FORMAT(5X,'START DAMAGE CONNECTION ELEMENT ',I10,
     .          ' INTEGRATION POINT',I2,', PLASTIC ELONGATION =',1PE16.9,
     .          ' AT TIME ',1PE16.9)
 1200 FORMAT(5X,'FAILURE CONNECTION SOLID ELEMENT ',I10,
     .          ' INTEGRATION POINT',I2,',  PLASTIC ELONGATION=',1PE16.9)
 1300 FORMAT(5X,'FAILURE CONNECTION SOLID ELEMENT ',I10,
     .          ' INTEGRATION POINT',I2,',  PLASTIC ELONGATION :',1PE16.9,
     .          ' AT TIME ',1PE16.9)
 1400 FORMAT(5X,'FAILURE CONNECTION SOLID ELEMENT ',I10,
     .         ',   AREA (LIMIT REACHED) :',1PE16.9)
 1500 FORMAT(5X,'FAILURE CONNECTION SOLID ELEMENT ',I10,
     .          ',  AREA (LIMIT REACHED) :',1PE16.9,' AT TIME ',1PE16.9)
C-----------------------------------------------
      RETURN
      END
