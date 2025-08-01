Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    r1def3             ../engine/source/elements/spring/r1def3.F
!||--- called by ------------------------------------------------------
!||    rforc3             ../engine/source/elements/spring/rforc3.F
!||--- calls      -----------------------------------------------------
!||    redef3             ../engine/source/elements/spring/redef3.F90
!||--- uses       -----------------------------------------------------
!||    python_funct_mod   ../common_source/modules/python_mod.F90
!||    redef3_mod         ../engine/source/elements/spring/redef3.F90
!||====================================================================
      SUBROUTINE R1DEF3(PYTHON,
     1   GEO,     F,       AL0,     E,
     2   DL,      NPF,     TF,      OFF,
     3   DPL,     FEP,     DPL2,    ANIM,
     4   IPOS,    IGEO,    AL0_ERR,
     5   X1DP,    X2DP,    V,       YIELD,
     6   NGL,     MGN,     EX,      EY,
     7   EZ,      XK,      XM,      XC,
     8   AK,      NC1,     NC2,     NUVAR,
     9   UVAR,    DL0,     NEL,     NFT, STF,
     A   SANIN,   IRESP,   SNPC)
C-----------------------------------------------  
        USE python_funct_mod
        USE REDEF3_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "scr14_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "com01_c.inc"
#include      "impl1_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      type(python_) :: PYTHON
      INTEGER, INTENT(IN) :: STF !< Size of TF
      INTEGER, INTENT(IN) :: SANIN !< Size of ANIM
      INTEGER, INTENT(IN) :: NFT
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: IRESP  !< Single precision flag
      INTEGER, INTENT(IN) :: SNPC   !< Size of NPF
      INTEGER NPF(*),IGEO(NPROPGI,*),NGL(*),MGN(*),
     .        NC1(*),NC2(*),NUVAR
C     REAL
      my_real
     .   GEO(NPROPG,*), F(*), AL0(*), E(*), DL(*), TF(STF), OFF(*),
     .   DPL(*), DPL2(*), FEP(*),ANIM(SANIN),IPOS(*),V(3,*),
     .   AL0_ERR(MVSIZ),YIELD(*),EX(MVSIZ),EY(MVSIZ),EZ(MVSIZ),
     .   XK(MVSIZ),XM(MVSIZ),XC(MVSIZ),AK(MVSIZ),UVAR(NUVAR,*),DL0(*)
      DOUBLE PRECISION X1DP(3,*),X2DP(3,*)
      TARGET :: UVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IECROU(MVSIZ), 
     .  IFUNC(MVSIZ), IFV(MVSIZ), IFUNC2(MVSIZ), I, J, ILENG,
     .  NINDX, INDX(MVSIZ), IFUNC3(MVSIZ)
C     REAL
      my_real
     .   DLOLD(MVSIZ),
     .   B(MVSIZ),  D(MVSIZ),  DMN(MVSIZ),DMX(MVSIZ),
     .   XL0(MVSIZ),DV(MVSIZ),FF(MVSIZ),LSCALE(MVSIZ),EE(MVSIZ),
     .   GF3(MVSIZ),EPLA(MVSIZ)
      my_real
     .   SUM ,VX21,VY21,VZ21,VL21,NOT_USED,NOT_USED2(2)
      my_real :: MAX_SLOPE(MVSIZ)
      DOUBLE PRECISION EXDP(MVSIZ),EYDP(MVSIZ),EZDP(MVSIZ),
     .                 AL0DP(MVSIZ),ALDP(MVSIZ)
      my_real ,DIMENSION(:), POINTER :: XX_OLD
      TARGET :: NOT_USED2
C-----------------------------------------------
C
      NOT_USED = ZERO
      NOT_USED2 = ZERO
C
      DO I=1,NEL
        EPLA(I)=ZERO
        XM(I)=GEO(1,MGN(I))
        XK(I)=GEO(2,MGN(I))
        XC(I)=GEO(3,MGN(I)) 
c        XC(I)=GEO(3,MGN(I)) + GEO(141,MGN(I)) ! +max slope of h
        IECROU(I)=NINT(GEO(7,MGN(I)))
        AK(I)  =GEO(10,MGN(I))
        B(I)   =GEO(11,MGN(I))
        D(I)   =GEO(13,MGN(I))
        EE(I)  =GEO(40 ,MGN(I))
        GF3(I) =GEO(132,MGN(I))
        FF(I)  =GEO(18 ,MGN(I))
        DMN(I) =GEO(15,MGN(I))
        DMX(I) =GEO(16,MGN(I))
        LSCALE(I)= GEO(39 ,MGN(I))
        IFUNC(I) =IGEO(101,MGN(I))
        IFV(I)   =IGEO(102,MGN(I))
        IFUNC2(I)=IGEO(103,MGN(I))
        IFUNC3(I)=IGEO(119,MGN(I))
        MAX_SLOPE(I) = GEO(141,MGN(I))
      ENDDO
C
      DO I=1,NEL
        EXDP(I)=X2DP(1,I)-X1DP(1,I)
        EYDP(I)=X2DP(2,I)-X1DP(2,I)
        EZDP(I)=X2DP(3,I)-X1DP(3,I)
        DLOLD(I)=DL(I)
        ALDP(I)=SQRT(EXDP(I)*EXDP(I)+EYDP(I)*EYDP(I)+EZDP(I)*EZDP(I))
      ENDDO
!
      IF (INISPRI /= 0 .and. TT == ZERO) THEN
        DO I=1,NEL
          DLOLD(I)=DL0(I)
        ENDDO
      ENDIF
!
      IF (INISPRI /= 0 .and. TT == ZERO) THEN
        DO I=1,NEL
          XL0(I)= AL0(I)
! if not initialized length
          IF (XL0(I) == ZERO) XL0(I) = ALDP(I)
        ENDDO
      ENDIF
!
      IF (TT == ZERO) THEN
        DO I=1,NEL
          AL0(I)=ALDP(I)                   ! cast double vers My_real
        ENDDO
      ENDIF
C
      IF (SCODVER >= 101) THEN
        IF (TT == ZERO) THEN
          DO I=1,NEL
            AL0_ERR(I)=ALDP(I)-AL0(I)        ! difference entre double et My_real
          ENDDO
        ENDIF
      ENDIF
!
      IF ( INISPRI /= 0 .and. TT == ZERO) THEN
        DO I=1,NEL
          AL0(I)= XL0(I)
        ENDDO
      ENDIF
!
      DO I=1,NEL
        AL0DP(I) = AL0(I)                  ! cast My_real en double
      ENDDO
!
      IF (SCODVER >= 101) THEN
        DO I=1,NEL
          AL0DP(I) = AL0DP(I) + AL0_ERR(I)   ! AL_DP doit   tre recalcul   ainsi afin de garantir la coh  rence absolue entre AL0_DP et AL_DP
        ENDDO
      ENDIF
C
      DO I=1,NEL
        SUM  = MAX(ALDP(I),EM15)
        EXDP(I)= EXDP(I)/SUM
        EYDP(I)= EYDP(I)/SUM
        EZDP(I)= EZDP(I)/SUM
        EX(I)=EXDP(I)
        EY(I)=EYDP(I)
        EZ(I)=EZDP(I)
      ENDDO
C
      IF (ISMDISP > 0) THEN
        DO I=1,NEL
          VX21 = V(1,NC2(I)) - V(1,NC1(I)) 
          VY21 = V(2,NC2(I)) - V(2,NC1(I)) 
          VZ21 = V(3,NC2(I)) - V(3,NC1(I)) 
          VL21 = VX21*EX(I)+VY21*EY(I)+VZ21*EZ(I)
          DL(I)= DLOLD(I)+VL21*DT1
        ENDDO
      ELSE
        DO I=1,NEL
          DL(I)= (ALDP(I)-AL0DP(I)) 
        ENDDO
      ENDIF !(ISMDISP>0) THEN
C
      DO I=1,NEL
        ILENG=NINT(GEO(93,MGN(I)))
        IF (ILENG /= 0) THEN
          XL0(I)=AL0DP(I)
        ELSE
          XL0(I)=ONE
        ENDIF
      ENDDO
C
      IF (NUVAR > 0) THEN 
         XX_OLD => UVAR(1,1:NEL) 
      ELSE 
         XX_OLD => NOT_USED2
      ENDIF 
      CALL REDEF3(PYTHON,
     1   F,          XK,         DL,         FEP,
     2   DLOLD,      DPL,        TF,         NPF,
     3   XC,         OFF,        E,          DPL2,
     4   ANIM,       ANIM_FE(11),IPOS,
     5   XL0,        DMN,        DMX,        DV,
     6   FF,         LSCALE,     EE,         GF3,
     7   IFUNC3,     YIELD,      ALDP,       AK,
     8   B,          D,          IECROU,     IFUNC,
     9   IFV,        IFUNC2,     EPLA,       XX_OLD,
     A   NEL,        NFT,        STF,        SANIN, 
     B   DT1,        IRESP,      IMPL_S,     IDYNA,
     C   SNPC,       MAX_SLOPE=MAX_SLOPE)
      NINDX = 0
      DO I=1,NEL
        IF (OFF(I) == ONE .AND. DMX(I) /= ZERO .AND. DMN(I) /= ZERO) THEN
          IF (DL(I) > DMX(I) .OR. DL(I) < DMN(I)) THEN
            OFF(I)=ZERO
            NINDX = NINDX + 1
            INDX(NINDX) = I
            IDEL7NOK = 1
          ENDIF
        ENDIF
      ENDDO
      DO J=1,NINDX
        I = INDX(J)
#include "lockon.inc"
        WRITE(IOUT, 1000) NGL(I)
        WRITE(ISTDO,1100) NGL(I),TT
#include "lockoff.inc"
      ENDDO
      DO I=1,NEL
        XM(I)=XM(I)*XL0(I)
        XK(I)=XK(I)/XL0(I)
C--- for time step compute adding +max slope of h
C       XC(I)=(XC(I)+GEO(141,MGN(I)))/XL0(I)
          XC(I)=(XC(I)+MAX_SLOPE(I))/XL0(I)


! sinon derivée au point courrant 
      ENDDO
C
 1000 FORMAT(1X,'-- RUPTURE OF SPRING ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'-- RUPTURE OF SPRING ELEMENT :',I10,' AT TIME :',G11.4)
C
      RETURN
      END
