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
Chd|  R27DEF3                       source/elements/spring/r27def3.F
Chd|-- called by -----------
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|====================================================================
      SUBROUTINE R27DEF3(F       ,E       ,DL      ,AL0     ,
     2                   IPOS    ,GEO     ,IGEO    ,
     3                   NPF     ,TF      ,V       ,OFF     ,ANIM    ,
     4                   AL0_ERR ,X1DP    ,X2DP    ,NGL     ,
     5                   MGN     ,EX      ,EY      ,EZ      ,XK      ,
     6                   XM      ,XC      ,FSCALE  )
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
#include      "com08_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPF(*),IGEO(NPROPGI,*),NGL(*),MGN(*)
C     REAL
      my_real
     .   GEO(NPROPG,*),F(*),AL0(*),E(*),DL(*),TF(*),OFF(*),
     .   ANIM(*),IPOS(*),V(3,*),
     .   AL0_ERR(*),EX(MVSIZ),EY(MVSIZ),EZ(MVSIZ),XK(MVSIZ),
     .   XM(MVSIZ),XC(MVSIZ),FSCALE(MVSIZ)
      DOUBLE PRECISION X1DP(3,*),X2DP(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,NINDX,PID
      INTEGER NC1(MVSIZ), NC2(MVSIZ),INDX(MVSIZ),IFUNC(MVSIZ),
     .        IFUNC2(MVSIZ),IFAIL(MVSIZ),ILENG(MVSIZ),ITENS(MVSIZ),
     .        FSMOOTH(MVSIZ)
C     REAL
      my_real
     .   DLOLD(MVSIZ),DMN(MVSIZ),DMX(MVSIZ),XL0(MVSIZ),DF1,DF2,
     .   DVL(MVSIZ),FK(MVSIZ),FD(MVSIZ),DDL(MVSIZ),GAP(MVSIZ),
     .   FSCALE2(MVSIZ),ASCALE(MVSIZ),ASCALE2(MVSIZ),FOLD(MVSIZ),
     .   NEXP(MVSIZ),FCUT(MVSIZ),ALPHA(MVSIZ),FKOLD(MVSIZ)
      my_real
     .    BID,SUM,DT11,DAMP,DAMM
      DOUBLE PRECISION EXDP(MVSIZ),EYDP(MVSIZ),EZDP(MVSIZ),ALDP(MVSIZ),
     .                 AL0DP(MVSIZ)
C-----------------------------------------------
C   E x t e r n a l
C-----------------------------------------------
      my_real
     .    FINTER
      EXTERNAL FINTER
C-----------------------------------------------
c      
C     ==================================================================
C     RECOVERING SPRING PARAMETERS
C     ==================================================================
      DO I=LFT,LLT
        PID        = MGN(I) 
        XM(I)      = GEO(1,PID)          ! Spring mass
        XK(I)      = GEO(2,PID)          ! Spring linear stiffness
        XC(I)      = GEO(3,PID)          ! Spring linear damping
        FSCALE(I)  = GEO(10,PID)         ! Stiffness tabulated function scale factor
        DMN(I)     = GEO(15,PID)         ! Negative limit for failure
        DMX(I)     = GEO(16,PID)         ! Positive limit for failure
        ASCALE2(I) = GEO(18,PID)         ! Velocity scale factor for damping tabulated function
        GAP(I)     = GEO(19,PID)         ! Compression gap for spring activation
        ASCALE(I)  = GEO(39,PID)         ! Displacement scale factor for stiffness tabulated function
        IFAIL(I)   = NINT(GEO(43,PID))   ! Flag for failure criterion
        ILENG(I)   = NINT(GEO(93,PID))   ! Flag for unit length values
        FSCALE2(I) = GEO(132,PID)        ! Damping tabulated function scale factor
        ITENS(I)   = NINT(GEO(133,PID))  ! Tensile behavior flag
        NEXP(I)    = GEO(134,PID)        ! Non-linear exponent
        FSMOOTH(I) = NINT(GEO(135,PID))  ! Spring velocity filtering flag
        FCUT(I)    = GEO(136,PID)        ! Cutoff frequency
        IFUNC(I)   = IGEO(101,PID)       ! Function ID for stiffness tabulated force
        IFUNC2(I)  = IGEO(102,PID)       ! Function ID for damping tabulated force
        IF (IFUNC2(I) /= 0) XC(I) = GEO(141,PID)
      ENDDO
C     ==================================================================
C
C     ==================================================================
C     COMPUTATION OF THE NEW SPRING LENGTH
C     ==================================================================
      DO I=LFT,LLT
        EXDP(I)  = X2DP(1,I)-X1DP(1,I)
        EYDP(I)  = X2DP(2,I)-X1DP(2,I)
        EZDP(I)  = X2DP(3,I)-X1DP(3,I)
        DLOLD(I) = DL(I)
        ALDP(I)  = SQRT(EXDP(I)*EXDP(I)+EYDP(I)*EYDP(I)+EZDP(I)*EZDP(I))
      ENDDO
C
      ! Conversion DOUBLE -> MY_REAL if TT = ZERO
      IF (TT == ZERO) THEN
        DO I=LFT,LLT
          AL0(I)     = ALDP(I)             ! cast double to My_real
          AL0_ERR(I) = ALDP(I)-AL0(I)      ! difference between double and My_real
        ENDDO
      ENDIF
C
      DO I=LFT,LLT
        AL0DP(I) = AL0(I)                  ! cast My_real to double
        AL0DP(I) = AL0DP(I) + AL0_ERR(I)   ! AL_DP doit etre recalcule ainsi afin de garantir la coherence absolue entre AL0_DP et AL_DP   
      ENDDO
C
      DO I=LFT,LLT
        SUM     = MAX(ALDP(I),EM15)
        EXDP(I) = EXDP(I)/SUM
        EYDP(I) = EYDP(I)/SUM
        EZDP(I) = EZDP(I)/SUM
        EX(I)   = EXDP(I)
        EY(I)   = EYDP(I)
        EZ(I)   = EZDP(I)
      ENDDO
C
      ! Spring total elongation
      DO I=LFT,LLT
        DL(I) = ALDP(I) - AL0DP(I)
      ENDDO
C
      ! ILENG flag 
      DO I=LFT,LLT
        ! Value per length unit
        IF (ILENG(I) /= 0) THEN
          XL0(I) = AL0DP(I)
        ! Classical units
        ELSE
          XL0(I) = ONE
        ENDIF
      ENDDO
C     ==================================================================
C
C     ==================================================================
C     COMPUTATION OF THE SPRING FORCE
C     ==================================================================
      ! Timestep
      DT11 = DT1
      IF (DT11 == ZERO) DT11 = EP30
C
      ! Recovering spring variables + Unit conversion if needed (ILENG)
      DO I = LFT,LLT
        ! Current total elongation
        DL(I)    = DL(I)/XL0(I)
        ! Old elongation
        DLOLD(I) = DLOLD(I)/XL0(I)
        ! Elongation increment
        DDL(I)   = (DL(I)-DLOLD(I))
        ! Elongation velocity
        DVL(I)   = DDL(I)/DT11
        ! Spring energy
        E(I)     = E(I)/XL0(I)
        ! Save old force value
        FOLD(I)  = F(I)
      ENDDO
C
C     ------------------------------------------------------------------
C     LOOP OVER THE ELEMENTS
C     ------------------------------------------------------------------
      DO I=LFT,LLT
c
        ! Computation if the element is not broken only
        IF ((OFF(I) == ONE).AND.(DL(I) < GAP(I) .OR. ITENS(I) > 0)) THEN 
          ! Stiffness part
          !   Computation of the tabulated stiffness force
          IF (IFUNC(I) > 0) THEN
            FK(I) = FSCALE(I)*FINTER(IFUNC(I),(DL(I)-GAP(I))/ASCALE(I),NPF,TF,DF1)
          !   Computation of linear elastic stiffness force (non-linear if NEXP > 1)
          ELSE
            IF (ABS(DL(I)-GAP(I)) > ZERO) THEN 
              FK(I) = SIGN(ONE,(DL(I)-GAP(I)))*XK(I)*EXP(NEXP(I)*LOG(ABS(DL(I)-GAP(I))))
            ELSE
              FK(I) = ZERO
            ENDIF
            ! If non-linear, the stiffness must be re-evaluated for the critical timestep
            IF (NEXP(I) > ONE) THEN
              ! Old stiffness force
              IF (ABS(DLOLD(I)-GAP(I)) > ZERO) THEN
                FKOLD(I) = SIGN(ONE,(DLOLD(I)-GAP(I)))*XK(I)*EXP(NEXP(I)*LOG(ABS(DLOLD(I)-GAP(I))))
              ELSE
                FKOLD(I) = ZERO
              ENDIF
              ! Non-linear stiffness slope
              XK(I) = MAX(ABS((FK(I)-FKOLD(I))/SIGN(MAX(ABS(DDL(I)),EM20),DDL(I))),XK(I))
            ENDIF
          ENDIF
c
          ! Damping part
          !   Computation of the tabulated damping force
          IF (IFUNC2(I) > 0) THEN
            FD(I) = FSCALE2(I)*FINTER(IFUNC2(I),DVL(I)/ASCALE2(I),NPF,TF,DF2)
          !   Computation of the linear damping
          ELSE
            FD(I) = XC(I)*DVL(I)
          ENDIF
c
          ! Assembling forces
          IF (ABS(FK(I)) > ABS(FD(I))) THEN
            F(I)  = FK(I) + FD(I)
          ELSE
            F(I)  = TWO*FK(I)
            XK(I) = TWO*XK(I)
            XC(I) = ZERO 
          ENDIF
        ELSE
          F(I)  = ZERO
          XC(I) = ZERO
        ENDIF
c
        ! Spring force filtering
        IF (FSMOOTH(I) > 0) THEN
          ALPHA(I) = (TWO*PI*DT11*FCUT(I))/(TWO*PI*DT11*FCUT(I) + ONE)
          F(I)     = ALPHA(I)*F(I) + (ONE - ALPHA(I))*FOLD(I)
        ENDIF
c
      ENDDO
C
      ! Computation of damage variable for animations
      IF (ANIM_FE(11) /= 0) THEN
        DO I=LFT,LLT
          J = I+NFT
          IF (IFAIL(I) == 1) THEN 
            IF (ITENS(I) > 0) THEN 
              DAMP = DL(I)/MAX(DMX(I),EM15)
            ELSE
              DAMP = ZERO
            ENDIF
            DAMM = DL(I)/MIN(DMN(I),-EM15)
          ELSEIF (IFAIL(I) == 2) THEN 
            IF (ITENS(I) > 0) THEN
              DAMP = F(I)/MAX(DMX(I),EM15)
            ELSE
              DAMP = ZERO
            ENDIF
            DAMM = F(I)/MIN(DMN(I),-EM15)
          ELSE
            DAMP = ZERO
            DAMM = ZERO
          ENDIF
          ANIM(J) = MAX(ANIM(J),DAMP,DAMM)
          ANIM(J) = MIN(ANIM(J),ONE)
        ENDDO
      ENDIF
c
      ! Computation of the Spring energy + Unit conversion if needed (ILENG)
      DO I=LFT,LLT
        E(I)     = E(I) + (DL(I)-DLOLD(I))*(F(I)+FOLD(I)) * HALF
        E(I)     = E(I)*XL0(I)
        DL(I)    = DL(I)*XL0(I)
        DLOLD(I) = DLOLD(I)*XL0(I)
        XM(I)    = XM(I)*XL0(I)
        XK(I)    = XK(I)/XL0(I)
        XC(I)    = XC(I)/XL0(I)
      ENDDO    
C     ==================================================================
C      
C     ==================================================================
C     CHECKING BROKEN SPRINGS
C     ==================================================================
      NINDX = 0
      DO I=LFT,LLT
        IF (OFF(I) == ONE .AND. IFAIL(I) /= 0) THEN
          IF (IFAIL(I) == 1) THEN
            IF (ITENS(I) > 0) THEN
              IF (DL(I) > DMX(I)*XL0(I) .OR. DL(I) < DMN(I)*XL0(I)) THEN
                OFF(I) = ZERO
                NINDX  = NINDX + 1
                INDX(NINDX) = I
                IDEL7NOK = 1
              ENDIF
            ELSE
              IF (DL(I) < DMN(I)*XL0(I)) THEN
                OFF(I) = ZERO
                NINDX  = NINDX + 1
                INDX(NINDX) = I
                IDEL7NOK = 1
              ENDIF
            ENDIF
          ELSEIF (IFAIL(I) == 2) THEN
            IF (ITENS(I) > 0) THEN 
              IF (F(I)  > DMX(I) .OR. F(I)  < DMN(I)) THEN
                OFF(I) = ZERO
                NINDX  = NINDX + 1
                INDX(NINDX) = I
                IDEL7NOK = 1
              ENDIF
            ELSE
              IF (F(I)  < DMN(I)) THEN
                OFF(I) = ZERO
                NINDX  = NINDX + 1
                INDX(NINDX) = I
                IDEL7NOK = 1
              ENDIF
            ENDIF
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
C-----------
 1000 FORMAT(1X,'-- RUPTURE OF SPRING ELEMENT NUMBER ',I10)
 1100 FORMAT(1X,'-- RUPTURE OF SPRING ELEMENT :',I10,' AT TIME :',G11.4)
C-----------
      END SUBROUTINE R27DEF3
