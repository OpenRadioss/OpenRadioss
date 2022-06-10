Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  CUPDT3_CRK                    source/elements/xfem/xfemfsky.F
Chd|-- called by -----------
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE CUPDT3_CRK(
     .           JFT     ,JLT     ,NFT   ,IXC    ,OFF     ,IADC    ,
     .           F11     ,F21     ,F31   ,F12    ,F22     ,F32     ,
     .           F13     ,F23     ,F33   ,F14    ,F24     ,F34     ,
     .           M11     ,M21     ,M31   ,M12    ,M22     ,M32     ,
     .           M13     ,M23     ,M33   ,M14    ,M24     ,M34     ,
     .           STI     ,STIR    ,FSKY  ,ELCUTC ,IADC_CRK,IEL_CRK ,
     .           ILEV    ,INOD_CRK,OFFG  ,EINT   ,PARTSAV ,IPARTC  ,
     .           ILAY    ,CRKSKY  )
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "com01_c.inc"
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IADC(4,*),IADC_CRK(4,*),IXC(NIXC,*),IEL_CRK(*),
     .  ELCUTC(2,*),INOD_CRK(*),IPARTC(*)
      INTEGER JFT,JLT,NFT,IXFEM,ILEV,ILAY
      my_real
     .  FSKY(8,LSKY),OFF(*),OFFG(*),
     .  F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .  F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .  M11(*),M21(*),M31(*),M12(*),M22(*),M32(*),
     .  M13(*),M23(*),M33(*),M14(*),M24(*),M34(*),
     .  STI(*),STIR(*),EINT(JLT,2),PARTSAV(NPSAV,*)
      TYPE(XFEM_SKY_)  , DIMENSION(*) :: CRKSKY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,KK,ELCRK,ELCUT,ENR,IOFF
      my_real OFF_L,AREAP
C=======================================================================
      IOFF=0
      DO I=JFT,JLT
        IF (OFF(I) == ZERO .AND. OFFG(I) > ZERO) IOFF=1
      ENDDO
      IF (IOFF == 1) THEN
        NUMELCRK = NUMELCRK + 1
      ENDIF
      OFF_L = ZERO
      DO I=JFT,JLT
        IF (OFF(I) < ONE) OFFG(I) = OFF(I)
        OFF_L = MIN(OFF_L,OFFG(I))
      ENDDO
c----------------------
      IF (OFF_L <= ZERO) THEN
        DO I=JFT,JLT
          IF (OFF(I) <= ZERO) THEN
            F11(I) = ZERO
            F21(I) = ZERO
            F31(I) = ZERO
            M11(I) = ZERO
            M21(I) = ZERO
            M31(I) = ZERO
            F12(I) = ZERO
            F22(I) = ZERO
            F32(I) = ZERO
            M12(I) = ZERO
            M22(I) = ZERO
            M32(I) = ZERO
            F13(I) = ZERO
            F23(I) = ZERO
            F33(I) = ZERO
            M13(I) = ZERO
            M23(I) = ZERO
            M33(I) = ZERO
            F14(I) = ZERO
            F24(I) = ZERO
            F34(I) = ZERO
            M14(I) = ZERO
            M24(I) = ZERO
            M34(I) = ZERO
            STI(I) = ZERO
            STIR(I)= ZERO
          ENDIF
        ENDDO
      ENDIF
c----------------------
      DO I=JFT,JLT
        ELCRK = IEL_CRK(I+NFT)
        ELCUT = XFEM_PHANTOM(ILAY)%ELCUT(ELCRK)
        IF (ELCUT /= 0) THEN
          AREAP = CRKLVSET(ILEV)%AREA(ELCRK)
c
          KK = IADC_CRK(1,ELCRK)
          CRKSKY(ILEV)%FSKY(1,KK) = -F11(I)*AREAP
          CRKSKY(ILEV)%FSKY(2,KK) = -F21(I)*AREAP
          CRKSKY(ILEV)%FSKY(3,KK) = -F31(I)*AREAP
          CRKSKY(ILEV)%FSKY(4,KK) = -M11(I)*AREAP
          CRKSKY(ILEV)%FSKY(5,KK) = -M21(I)*AREAP
          CRKSKY(ILEV)%FSKY(6,KK) = -M31(I)*AREAP
          CRKSKY(ILEV)%FSKY(7,KK) =  STI(I)
          CRKSKY(ILEV)%FSKY(8,KK) =  STIR(I)
C
          KK = IADC_CRK(2,ELCRK)
          CRKSKY(ILEV)%FSKY(1,KK) = -F12(I)*AREAP
          CRKSKY(ILEV)%FSKY(2,KK) = -F22(I)*AREAP
          CRKSKY(ILEV)%FSKY(3,KK) = -F32(I)*AREAP
          CRKSKY(ILEV)%FSKY(4,KK) = -M12(I)*AREAP
          CRKSKY(ILEV)%FSKY(5,KK) = -M22(I)*AREAP
          CRKSKY(ILEV)%FSKY(6,KK) = -M32(I)*AREAP
          CRKSKY(ILEV)%FSKY(7,KK) =  STI(I)
          CRKSKY(ILEV)%FSKY(8,KK) =  STIR(I)
C
          KK = IADC_CRK(3,ELCRK)
          CRKSKY(ILEV)%FSKY(1,KK) = -F13(I)*AREAP
          CRKSKY(ILEV)%FSKY(2,KK) = -F23(I)*AREAP
          CRKSKY(ILEV)%FSKY(3,KK) = -F33(I)*AREAP
          CRKSKY(ILEV)%FSKY(4,KK) = -M13(I)*AREAP
          CRKSKY(ILEV)%FSKY(5,KK) = -M23(I)*AREAP
          CRKSKY(ILEV)%FSKY(6,KK) = -M33(I)*AREAP
          CRKSKY(ILEV)%FSKY(7,KK) =  STI(I)
          CRKSKY(ILEV)%FSKY(8,KK) =  STIR(I)
C
          KK = IADC_CRK(4,ELCRK)
          CRKSKY(ILEV)%FSKY(1,KK) = -F14(I)*AREAP
          CRKSKY(ILEV)%FSKY(2,KK) = -F24(I)*AREAP
          CRKSKY(ILEV)%FSKY(3,KK) = -F34(I)*AREAP
          CRKSKY(ILEV)%FSKY(4,KK) = -M14(I)*AREAP
          CRKSKY(ILEV)%FSKY(5,KK) = -M24(I)*AREAP
          CRKSKY(ILEV)%FSKY(6,KK) = -M34(I)*AREAP
          CRKSKY(ILEV)%FSKY(7,KK) =  STI(I)
          CRKSKY(ILEV)%FSKY(8,KK) =  STIR(I)
        END IF
      END DO
c--------------------------------------------------
      DO I=JFT,JLT
        ELCRK = IEL_CRK(I+NFT)
        ELCUT = XFEM_PHANTOM(ILAY)%ELCUT(ELCRK)
        IF (ELCUT == 0) CYCLE
C---
c       NODE 1
C---
        K   = IADC(1,I)
        KK  = IADC_CRK(1,ELCRK)
        ENR = CRKLVSET(ILEV)%ENR0(2,KK)
C
        IF (ENR <= 0) THEN
          FSKY(1,K) = FSKY(1,K) + CRKSKY(ILEV)%FSKY(1,KK)
          FSKY(2,K) = FSKY(2,K) + CRKSKY(ILEV)%FSKY(2,KK)
          FSKY(3,K) = FSKY(3,K) + CRKSKY(ILEV)%FSKY(3,KK)
          FSKY(4,K) = FSKY(4,K) + CRKSKY(ILEV)%FSKY(4,KK)
          FSKY(5,K) = FSKY(5,K) + CRKSKY(ILEV)%FSKY(5,KK)
          FSKY(6,K) = FSKY(6,K) + CRKSKY(ILEV)%FSKY(6,KK)
C
          CRKSKY(ILEV)%FSKY(1,KK) = ZERO
          CRKSKY(ILEV)%FSKY(2,KK) = ZERO
          CRKSKY(ILEV)%FSKY(3,KK) = ZERO
          CRKSKY(ILEV)%FSKY(4,KK) = ZERO
          CRKSKY(ILEV)%FSKY(5,KK) = ZERO
          CRKSKY(ILEV)%FSKY(6,KK) = ZERO
        END IF
C---
c       NODE 2
C---
        K   = IADC(2,I)
        KK  = IADC_CRK(2,ELCRK)
        ENR = CRKLVSET(ILEV)%ENR0(2,KK)
C
        IF (ENR <= 0) THEN
          FSKY(1,K) = FSKY(1,K) + CRKSKY(ILEV)%FSKY(1,KK)
          FSKY(2,K) = FSKY(2,K) + CRKSKY(ILEV)%FSKY(2,KK)
          FSKY(3,K) = FSKY(3,K) + CRKSKY(ILEV)%FSKY(3,KK)
          FSKY(4,K) = FSKY(4,K) + CRKSKY(ILEV)%FSKY(4,KK)
          FSKY(5,K) = FSKY(5,K) + CRKSKY(ILEV)%FSKY(5,KK)
          FSKY(6,K) = FSKY(6,K) + CRKSKY(ILEV)%FSKY(6,KK)
C
          CRKSKY(ILEV)%FSKY(1,KK) = ZERO
          CRKSKY(ILEV)%FSKY(2,KK) = ZERO
          CRKSKY(ILEV)%FSKY(3,KK) = ZERO
          CRKSKY(ILEV)%FSKY(4,KK) = ZERO
          CRKSKY(ILEV)%FSKY(5,KK) = ZERO
          CRKSKY(ILEV)%FSKY(6,KK) = ZERO
        END IF
C---
c       NODE 3
C---
        K   = IADC(3,I)
        KK  = IADC_CRK(3,ELCRK)
        ENR = CRKLVSET(ILEV)%ENR0(2,KK)
C
        IF (ENR <= 0) THEN
          FSKY(1,K) = FSKY(1,K) + CRKSKY(ILEV)%FSKY(1,KK)
          FSKY(2,K) = FSKY(2,K) + CRKSKY(ILEV)%FSKY(2,KK)
          FSKY(3,K) = FSKY(3,K) + CRKSKY(ILEV)%FSKY(3,KK)
          FSKY(4,K) = FSKY(4,K) + CRKSKY(ILEV)%FSKY(4,KK)
          FSKY(5,K) = FSKY(5,K) + CRKSKY(ILEV)%FSKY(5,KK)
          FSKY(6,K) = FSKY(6,K) + CRKSKY(ILEV)%FSKY(6,KK)
C
          CRKSKY(ILEV)%FSKY(1,KK) = ZERO
          CRKSKY(ILEV)%FSKY(2,KK) = ZERO
          CRKSKY(ILEV)%FSKY(3,KK) = ZERO
          CRKSKY(ILEV)%FSKY(4,KK) = ZERO
          CRKSKY(ILEV)%FSKY(5,KK) = ZERO
          CRKSKY(ILEV)%FSKY(6,KK) = ZERO
        END IF
C---
c       NODE 4
C---
        K   = IADC(4,I)
        KK  = IADC_CRK(4,ELCRK)
        ENR = CRKLVSET(ILEV)%ENR0(2,KK)
C
        IF (ENR <= 0) THEN
          FSKY(1,K) = FSKY(1,K) + CRKSKY(ILEV)%FSKY(1,KK)
          FSKY(2,K) = FSKY(2,K) + CRKSKY(ILEV)%FSKY(2,KK)
          FSKY(3,K) = FSKY(3,K) + CRKSKY(ILEV)%FSKY(3,KK)
          FSKY(4,K) = FSKY(4,K) + CRKSKY(ILEV)%FSKY(4,KK)
          FSKY(5,K) = FSKY(5,K) + CRKSKY(ILEV)%FSKY(5,KK)
          FSKY(6,K) = FSKY(6,K) + CRKSKY(ILEV)%FSKY(6,KK)
C
          CRKSKY(ILEV)%FSKY(1,KK) = ZERO
          CRKSKY(ILEV)%FSKY(2,KK) = ZERO
          CRKSKY(ILEV)%FSKY(3,KK) = ZERO
          CRKSKY(ILEV)%FSKY(4,KK) = ZERO
          CRKSKY(ILEV)%FSKY(5,KK) = ZERO
          CRKSKY(ILEV)%FSKY(6,KK) = ZERO
        END IF
C---
      ENDDO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  CUPDTN3_CRK                   source/elements/xfem/xfemfsky.F
Chd|-- called by -----------
Chd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE CUPDTN3_CRK(
     .          JFT   ,JLT  ,NFT  ,IXC   ,OFF     ,IADC   ,
     .          F11   ,F21  ,F31  ,F12   ,F22     ,F32    ,
     .          F13   ,F23  ,F33  ,F14   ,F24     ,F34    ,
     .          M11   ,M21  ,M31  ,M12   ,M22     ,M32    ,
     .          M13   ,M23  ,M33  ,M14   ,M24     ,M34    ,
     .          STI   ,STIR ,FSKY ,ELCUTC,IADC_CRK,IEL_CRK,
     .          ILEV  ,INOD_CRK,FAC,OFFG ,EINT    ,PARTSAV,
     .          IPARTC,ILAY    ,CRKSKY   )
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "com01_c.inc"
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NFT,IADC(4,*),IADC_CRK(4,*),IXC(NIXC,*),
     . IEL_CRK(*),ILEV,ELCUTC(2,*),INOD_CRK(*),IPARTC(*),
     . IXFEM,ILAY
      my_real
     .  FSKY(8,LSKY),OFF(*),
     .  F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .  F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .  M11(*),M21(*),M31(*),M12(*),M22(*),M32(*),
     .  M13(*),M23(*),M33(*),M14(*),M24(*),M34(*),
     .  STI(*),STIR(*),FAC(2,*),OFFG(*),EINT(JLT,2),PARTSAV(NPSAV,*)
      TYPE(XFEM_SKY_)  , DIMENSION(*) :: CRKSKY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,KK,ELCRK,ELCUT,ENR,IOFF
      my_real OFF_L,AREAP
C=======================================================================
      IOFF = 0
      DO I=JFT,JLT
        IF (OFF(I) == ZERO .AND. OFFG(I) > ZERO) IOFF=1
      ENDDO
      IF (IOFF == 1) THEN    ! debug anim only
        NUMELCRK = NUMELCRK + 1
      ENDIF
      OFF_L = ZERO
      DO I=JFT,JLT
        IF (OFF(I) < ONE) OFFG(I) = OFF(I)
        OFF_L = MIN(OFF_L,OFFG(I))
      ENDDO
C----------------------
      IF (OFF_L <= ZERO) THEN
        DO I=JFT,JLT
         IF (OFF(I) <= ZERO) THEN
            F11(I) = ZERO
            F21(I) = ZERO
            F31(I) = ZERO
            M11(I) = ZERO
            M21(I) = ZERO
            M31(I) = ZERO
            F12(I) = ZERO
            F22(I) = ZERO
            F32(I) = ZERO
            M12(I) = ZERO
            M22(I) = ZERO
            M32(I) = ZERO
            F13(I) = ZERO
            F23(I) = ZERO
            F33(I) = ZERO
            M13(I) = ZERO
            M23(I) = ZERO
            M33(I) = ZERO
            F14(I) = ZERO
            F24(I) = ZERO
            F34(I) = ZERO
            M14(I) = ZERO
            M24(I) = ZERO
            M34(I) = ZERO
            STI(I) = ZERO
            STIR(I)= ZERO
          ENDIF
        ENDDO
      ENDIF
C
      DO I=JFT,JLT
        ELCRK = IEL_CRK(I+NFT)
        ELCUT = XFEM_PHANTOM(ILAY)%ELCUT(ELCRK)
        IF (ELCUT /= 0) THEN
          AREAP = CRKLVSET(ILEV)%AREA(ELCRK)
c
          KK = IADC_CRK(1,ELCRK)
          CRKSKY(ILEV)%FSKY(1,KK) = -F11(I)*AREAP
          CRKSKY(ILEV)%FSKY(2,KK) = -F21(I)*AREAP
          CRKSKY(ILEV)%FSKY(3,KK) = -F31(I)*AREAP
          CRKSKY(ILEV)%FSKY(4,KK) = -M11(I)*AREAP
          CRKSKY(ILEV)%FSKY(5,KK) = -M21(I)*AREAP
          CRKSKY(ILEV)%FSKY(6,KK) = -M31(I)*AREAP
          CRKSKY(ILEV)%FSKY(7,KK) =  STI(I) *FAC(1,I)
          CRKSKY(ILEV)%FSKY(8,KK) =  STIR(I)*FAC(1,I)
C
          KK = IADC_CRK(2,ELCRK)
          CRKSKY(ILEV)%FSKY(1,KK) = -F12(I)*AREAP
          CRKSKY(ILEV)%FSKY(2,KK) = -F22(I)*AREAP
          CRKSKY(ILEV)%FSKY(3,KK) = -F32(I)*AREAP
          CRKSKY(ILEV)%FSKY(4,KK) = -M12(I)*AREAP
          CRKSKY(ILEV)%FSKY(5,KK) = -M22(I)*AREAP
          CRKSKY(ILEV)%FSKY(6,KK) = -M32(I)*AREAP
          CRKSKY(ILEV)%FSKY(7,KK) =  STI(I) *FAC(2,I)
          CRKSKY(ILEV)%FSKY(8,KK) =  STIR(I)*FAC(2,I)
C
          KK = IADC_CRK(3,ELCRK)
          CRKSKY(ILEV)%FSKY(1,KK) = -F13(I)*AREAP
          CRKSKY(ILEV)%FSKY(2,KK) = -F23(I)*AREAP
          CRKSKY(ILEV)%FSKY(3,KK) = -F33(I)*AREAP
          CRKSKY(ILEV)%FSKY(4,KK) = -M13(I)*AREAP
          CRKSKY(ILEV)%FSKY(5,KK) = -M23(I)*AREAP
          CRKSKY(ILEV)%FSKY(6,KK) = -M33(I)*AREAP
          CRKSKY(ILEV)%FSKY(7,KK) =  STI(I) *FAC(1,I)
          CRKSKY(ILEV)%FSKY(8,KK) =  STIR(I)*FAC(1,I)
C
          KK = IADC_CRK(4,ELCRK)
          CRKSKY(ILEV)%FSKY(1,KK) = -F14(I)*AREAP
          CRKSKY(ILEV)%FSKY(2,KK) = -F24(I)*AREAP
          CRKSKY(ILEV)%FSKY(3,KK) = -F34(I)*AREAP
          CRKSKY(ILEV)%FSKY(4,KK) = -M14(I)*AREAP
          CRKSKY(ILEV)%FSKY(5,KK) = -M24(I)*AREAP
          CRKSKY(ILEV)%FSKY(6,KK) = -M34(I)*AREAP
          CRKSKY(ILEV)%FSKY(7,KK) =  STI(I) *FAC(2,I)
          CRKSKY(ILEV)%FSKY(8,KK) =  STIR(I)*FAC(2,I)
        END IF
      END DO
C-----------------------------------------------
      DO I=JFT,JLT
        ELCRK = IEL_CRK(I+NFT)
        ELCUT = XFEM_PHANTOM(ILAY)%ELCUT(ELCRK)
        IF (ELCUT == 0) CYCLE
C---
c       NODE 1
C---
        K   = IADC(1,I)
        KK  = IADC_CRK(1,ELCRK)
        ENR = CRKLVSET(ILEV)%ENR0(2,KK)
c
        IF (ENR <= 0) THEN
          FSKY(1,K) = FSKY(1,K) + CRKSKY(ILEV)%FSKY(1,KK)
          FSKY(2,K) = FSKY(2,K) + CRKSKY(ILEV)%FSKY(2,KK)
          FSKY(3,K) = FSKY(3,K) + CRKSKY(ILEV)%FSKY(3,KK)
          FSKY(4,K) = FSKY(4,K) + CRKSKY(ILEV)%FSKY(4,KK)
          FSKY(5,K) = FSKY(5,K) + CRKSKY(ILEV)%FSKY(5,KK)
          FSKY(6,K) = FSKY(6,K) + CRKSKY(ILEV)%FSKY(6,KK)
C
          CRKSKY(ILEV)%FSKY(1,KK) = ZERO
          CRKSKY(ILEV)%FSKY(2,KK) = ZERO
          CRKSKY(ILEV)%FSKY(3,KK) = ZERO
          CRKSKY(ILEV)%FSKY(4,KK) = ZERO
          CRKSKY(ILEV)%FSKY(5,KK) = ZERO
          CRKSKY(ILEV)%FSKY(6,KK) = ZERO
        END IF
C---
c       NODE 2
C---
        K   = IADC(2,I)
        KK  = IADC_CRK(2,ELCRK)
        ENR = CRKLVSET(ILEV)%ENR0(2,KK)
c
        IF (ENR <= 0) THEN
          FSKY(1,K) = FSKY(1,K) + CRKSKY(ILEV)%FSKY(1,KK)
          FSKY(2,K) = FSKY(2,K) + CRKSKY(ILEV)%FSKY(2,KK)
          FSKY(3,K) = FSKY(3,K) + CRKSKY(ILEV)%FSKY(3,KK)
          FSKY(4,K) = FSKY(4,K) + CRKSKY(ILEV)%FSKY(4,KK)
          FSKY(5,K) = FSKY(5,K) + CRKSKY(ILEV)%FSKY(5,KK)
          FSKY(6,K) = FSKY(6,K) + CRKSKY(ILEV)%FSKY(6,KK)
C
          CRKSKY(ILEV)%FSKY(1,KK) = ZERO
          CRKSKY(ILEV)%FSKY(2,KK) = ZERO
          CRKSKY(ILEV)%FSKY(3,KK) = ZERO
          CRKSKY(ILEV)%FSKY(4,KK) = ZERO
          CRKSKY(ILEV)%FSKY(5,KK) = ZERO
          CRKSKY(ILEV)%FSKY(6,KK) = ZERO
        END IF
C---
c       NODE 3
C---
        K   = IADC(3,I)
        KK  = IADC_CRK(3,ELCRK)
        ENR = CRKLVSET(ILEV)%ENR0(2,KK)
c
        IF (ENR <= 0) THEN
          FSKY(1,K) = FSKY(1,K) + CRKSKY(ILEV)%FSKY(1,KK)
          FSKY(2,K) = FSKY(2,K) + CRKSKY(ILEV)%FSKY(2,KK)
          FSKY(3,K) = FSKY(3,K) + CRKSKY(ILEV)%FSKY(3,KK)
          FSKY(4,K) = FSKY(4,K) + CRKSKY(ILEV)%FSKY(4,KK)
          FSKY(5,K) = FSKY(5,K) + CRKSKY(ILEV)%FSKY(5,KK)
          FSKY(6,K) = FSKY(6,K) + CRKSKY(ILEV)%FSKY(6,KK)
C
          CRKSKY(ILEV)%FSKY(1,KK) = ZERO
          CRKSKY(ILEV)%FSKY(2,KK) = ZERO
          CRKSKY(ILEV)%FSKY(3,KK) = ZERO
          CRKSKY(ILEV)%FSKY(4,KK) = ZERO
          CRKSKY(ILEV)%FSKY(5,KK) = ZERO
          CRKSKY(ILEV)%FSKY(6,KK) = ZERO
        END IF
C---
c       NODE 4
C---
        K   = IADC(4,I)
        KK  = IADC_CRK(4,ELCRK)
        ENR = CRKLVSET(ILEV)%ENR0(2,KK)
c
        IF (ENR <= 0) THEN
          FSKY(1,K) = FSKY(1,K) + CRKSKY(ILEV)%FSKY(1,KK)
          FSKY(2,K) = FSKY(2,K) + CRKSKY(ILEV)%FSKY(2,KK)
          FSKY(3,K) = FSKY(3,K) + CRKSKY(ILEV)%FSKY(3,KK)
          FSKY(4,K) = FSKY(4,K) + CRKSKY(ILEV)%FSKY(4,KK)
          FSKY(5,K) = FSKY(5,K) + CRKSKY(ILEV)%FSKY(5,KK)
          FSKY(6,K) = FSKY(6,K) + CRKSKY(ILEV)%FSKY(6,KK)
C
          CRKSKY(ILEV)%FSKY(1,KK) = ZERO
          CRKSKY(ILEV)%FSKY(2,KK) = ZERO
          CRKSKY(ILEV)%FSKY(3,KK) = ZERO
          CRKSKY(ILEV)%FSKY(4,KK) = ZERO
          CRKSKY(ILEV)%FSKY(5,KK) = ZERO
          CRKSKY(ILEV)%FSKY(6,KK) = ZERO
        END IF
C---
      ENDDO
C-------------
      RETURN
      END
Chd|====================================================================
Chd|  C3UPDT3_CRK                   source/elements/xfem/xfemfsky.F
Chd|-- called by -----------
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE C3UPDT3_CRK(
     .          JFT  ,JLT  ,NFT  ,IXTG  ,OFF     ,IADC   ,
     .          F11  ,F21  ,F31  ,F12   ,F22     ,F32    ,
     .          F13  ,F23  ,F33  ,
     .          M11  ,M21  ,M31  ,M12   ,M22     ,M32    ,
     .          M13  ,M23  ,M33  ,
     .          STI  ,STIR ,FSKY ,ELCUTC,IAD_CRKTG,IEL_CRKTG,
     .          ILEV ,ILAY ,OFFG ,CRKSKY)
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "com01_c.inc"
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NFT,IADC(3,*),IAD_CRKTG(3,*),IXTG(NIXTG,*),
     . IEL_CRKTG(*),ILEV,ELCUTC(2,*),IXFEM,ILAY
C     REAL
      my_real
     .  FSKY(8,LSKY),OFF(*),OFFG(*),
     .  F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .  F13(*),F23(*),F33(*),
     .  M11(*),M21(*),M31(*),M12(*),M22(*),M32(*),
     .  M13(*),M23(*),M33(*),
     .  STI(*),STIR(*)
      TYPE(XFEM_SKY_)  , DIMENSION(*) :: CRKSKY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,KK,ELCUT,ELCRK,ELCRKTG,ENR,IOFF
      INTEGER IAD(3),IADXFE(3),UNENR0(3,JLT)
      my_real OFF_L,AREAP
C=======================================================================
      IOFF=0
      DO I=JFT,JLT
        IF (OFF(I) == ZERO .AND. OFFG(I) > ZERO) IOFF=1
      ENDDO
      IF (IOFF == 1) NUMELCRK = NUMELCRK + 1
C
      OFF_L = ZERO
      DO I=JFT,JLT
        IF (OFF(I) < ONE) OFFG(I) = OFF(I)
        OFF_L = MIN(OFF_L,OFFG(I))
      ENDDO
C----------------------
      IF (OFF_L <= ZERO) THEN
        DO I=JFT,JLT
         IF (OFF(I) <= ZERO) THEN
           F11(I) = ZERO
           F21(I) = ZERO
           F31(I) = ZERO
           M11(I) = ZERO
           M21(I) = ZERO
           M31(I) = ZERO
           F12(I) = ZERO
           F22(I) = ZERO
           F32(I) = ZERO
           M12(I) = ZERO
           M22(I) = ZERO
           M32(I) = ZERO
           F13(I) = ZERO
           F23(I) = ZERO
           F33(I) = ZERO
           M13(I) = ZERO
           M23(I) = ZERO
           M33(I) = ZERO
           STI(I) = ZERO
           STIR(I)= ZERO
         ENDIF
        ENDDO
      ENDIF
C
      DO I=JFT,JLT
        ELCRKTG = IEL_CRKTG(I+NFT)
        ELCRK = ELCRKTG + ECRKXFEC
        ELCUT = XFEM_PHANTOM(ILAY)%ELCUT(ELCRK)
        IF (ELCUT /= 0) THEN
          AREAP = CRKLVSET(ILEV)%AREA(ELCRK)
c
          KK = IAD_CRKTG(1,ELCRKTG)
          CRKSKY(ILEV)%FSKY(1,KK) = -F11(I)*AREAP
          CRKSKY(ILEV)%FSKY(2,KK) = -F21(I)*AREAP
          CRKSKY(ILEV)%FSKY(3,KK) = -F31(I)*AREAP
          CRKSKY(ILEV)%FSKY(4,KK) = -M11(I)*AREAP
          CRKSKY(ILEV)%FSKY(5,KK) = -M21(I)*AREAP
          CRKSKY(ILEV)%FSKY(6,KK) = -M31(I)*AREAP
          CRKSKY(ILEV)%FSKY(7,KK) =  STI(I)
          CRKSKY(ILEV)%FSKY(8,KK) =  STIR(I)
C
          KK = IAD_CRKTG(2,ELCRKTG)
          CRKSKY(ILEV)%FSKY(1,KK) = -F12(I)*AREAP
          CRKSKY(ILEV)%FSKY(2,KK) = -F22(I)*AREAP
          CRKSKY(ILEV)%FSKY(3,KK) = -F32(I)*AREAP
          CRKSKY(ILEV)%FSKY(4,KK) = -M12(I)*AREAP
          CRKSKY(ILEV)%FSKY(5,KK) = -M22(I)*AREAP
          CRKSKY(ILEV)%FSKY(6,KK) = -M32(I)*AREAP
          CRKSKY(ILEV)%FSKY(7,KK) =  STI(I)
          CRKSKY(ILEV)%FSKY(8,KK) =  STIR(I)
C
          KK = IAD_CRKTG(3,ELCRKTG)
          CRKSKY(ILEV)%FSKY(1,KK) = -F13(I)*AREAP
          CRKSKY(ILEV)%FSKY(2,KK) = -F23(I)*AREAP
          CRKSKY(ILEV)%FSKY(3,KK) = -F33(I)*AREAP
          CRKSKY(ILEV)%FSKY(4,KK) = -M13(I)*AREAP
          CRKSKY(ILEV)%FSKY(5,KK) = -M23(I)*AREAP
          CRKSKY(ILEV)%FSKY(6,KK) = -M33(I)*AREAP
          CRKSKY(ILEV)%FSKY(7,KK) =  STI(I)
          CRKSKY(ILEV)%FSKY(8,KK) =  STIR(I)
        END IF
      END DO
C-----------------------------------------------
      DO I=JFT,JLT
        ELCRKTG = IEL_CRKTG(I+NFT)
        ELCRK = ELCRKTG + ECRKXFEC
        ELCUT = XFEM_PHANTOM(ILAY)%ELCUT(ELCRK)
        IF (ELCUT == 0) CYCLE
C---
c       NODE 1
C---
        K   = IADC(1,I)
        KK  = IAD_CRKTG(1,ELCRKTG)
        ENR = CRKLVSET(ILEV)%ENR0(2,KK)
C
        IF (ENR <= 0) THEN
          FSKY(1,K) = FSKY(1,K) + CRKSKY(ILEV)%FSKY(1,KK)
          FSKY(2,K) = FSKY(2,K) + CRKSKY(ILEV)%FSKY(2,KK)
          FSKY(3,K) = FSKY(3,K) + CRKSKY(ILEV)%FSKY(3,KK)
          FSKY(4,K) = FSKY(4,K) + CRKSKY(ILEV)%FSKY(4,KK)
          FSKY(5,K) = FSKY(5,K) + CRKSKY(ILEV)%FSKY(5,KK)
          FSKY(6,K) = FSKY(6,K) + CRKSKY(ILEV)%FSKY(6,KK)
C
          CRKSKY(ILEV)%FSKY(1,KK) = ZERO
          CRKSKY(ILEV)%FSKY(2,KK) = ZERO
          CRKSKY(ILEV)%FSKY(3,KK) = ZERO
          CRKSKY(ILEV)%FSKY(4,KK) = ZERO
          CRKSKY(ILEV)%FSKY(5,KK) = ZERO
          CRKSKY(ILEV)%FSKY(6,KK) = ZERO
        END IF
C---
c       NODE 2
C---
        K   = IADC(2,I)
        KK  = IAD_CRKTG(2,ELCRKTG)
        ENR = CRKLVSET(ILEV)%ENR0(2,KK)
C
        IF (ENR <= 0) THEN
          FSKY(1,K) = FSKY(1,K) + CRKSKY(ILEV)%FSKY(1,KK)
          FSKY(2,K) = FSKY(2,K) + CRKSKY(ILEV)%FSKY(2,KK)
          FSKY(3,K) = FSKY(3,K) + CRKSKY(ILEV)%FSKY(3,KK)
          FSKY(4,K) = FSKY(4,K) + CRKSKY(ILEV)%FSKY(4,KK)
          FSKY(5,K) = FSKY(5,K) + CRKSKY(ILEV)%FSKY(5,KK)
          FSKY(6,K) = FSKY(6,K) + CRKSKY(ILEV)%FSKY(6,KK)
C
          CRKSKY(ILEV)%FSKY(1,KK) = ZERO
          CRKSKY(ILEV)%FSKY(2,KK) = ZERO
          CRKSKY(ILEV)%FSKY(3,KK) = ZERO
          CRKSKY(ILEV)%FSKY(4,KK) = ZERO
          CRKSKY(ILEV)%FSKY(5,KK) = ZERO
          CRKSKY(ILEV)%FSKY(6,KK) = ZERO
        END IF
C---
c       NODE 3
C---
        K   = IADC(3,I)
        KK  = IAD_CRKTG(3,ELCRKTG)
        ENR = CRKLVSET(ILEV)%ENR0(2,KK)
C
        IF (ENR <= 0) THEN
          FSKY(1,K) = FSKY(1,K) + CRKSKY(ILEV)%FSKY(1,KK)
          FSKY(2,K) = FSKY(2,K) + CRKSKY(ILEV)%FSKY(2,KK)
          FSKY(3,K) = FSKY(3,K) + CRKSKY(ILEV)%FSKY(3,KK)
          FSKY(4,K) = FSKY(4,K) + CRKSKY(ILEV)%FSKY(4,KK)
          FSKY(5,K) = FSKY(5,K) + CRKSKY(ILEV)%FSKY(5,KK)
          FSKY(6,K) = FSKY(6,K) + CRKSKY(ILEV)%FSKY(6,KK)
C
          CRKSKY(ILEV)%FSKY(1,KK) = ZERO
          CRKSKY(ILEV)%FSKY(2,KK) = ZERO
          CRKSKY(ILEV)%FSKY(3,KK) = ZERO
          CRKSKY(ILEV)%FSKY(4,KK) = ZERO
          CRKSKY(ILEV)%FSKY(5,KK) = ZERO
          CRKSKY(ILEV)%FSKY(6,KK) = ZERO
        END IF
C---
      ENDDO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_CRK_ADV                  source/elements/xfem/xfemfsky.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        SPMD_EXCH_NODENR              source/mpi/elements/spmd_xfem.F
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE SPMD_CRK_ADV(IAD_ELEM ,FR_ELEM, INOD_CRK ,ENRTAG)
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,NSPMD+1),FR_ELEM(*),INOD_CRK(*),
     .        ENRTAG(NUMNOD,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER SIZE,LENR,FLAG
C-----------------------------------------------
      SIZE = IENRNOD
      LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
      FLAG = 1
      CALL SPMD_EXCH_NODENR(IAD_ELEM,FR_ELEM,SIZE,LENR,INOD_CRK,
     .                      ENRTAG,FLAG)
C-------------
      RETURN
      END
