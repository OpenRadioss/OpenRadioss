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
Chd|  LAW119_MEMBRANE               source/materials/mat/mat119/law119_membrane.F
Chd|-- called by -----------
Chd|        SIGEPS119C                    source/materials/mat/mat119/sigeps119c.F
Chd|-- calls ---------------
Chd|        TABLE_VINTERP                 source/tools/curve/table_tools.F
Chd|        INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules/table_mod.F     
Chd|====================================================================
      SUBROUTINE LAW119_MEMBRANE(
     .            NEL    ,NUPARAM,NUVAR  ,UPARAM ,UVAR   ,
     .            GS     ,ET     ,DEPSXX ,DEPSYY ,DEPSXY    ,
     .            EPSXX  ,EPSYY  ,EPSXY  ,EPSYZ  ,EPSZX     ,
     .            SIGOXX ,SIGOYY ,SIGOXY ,SIGOYZ ,SIGOZX    ,
     .            SIGNXX ,SIGNYY ,SIGNXY ,SIGNYZ ,SIGNZX    ,
     .            NUMTABL,ITABLE ,TABLE  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE MESSAGE_MOD
      USE INTERFACE_TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NUPARAM,NUVAR,NUMTABL
      INTEGER ,DIMENSION(NUMTABL),INTENT(IN)   :: ITABLE
      my_real ,INTENT(IN) ::  GS
      my_real ,DIMENSION(NUPARAM) ,INTENT(IN)  :: UPARAM
      my_real ,DIMENSION(NEL) ,INTENT(IN) ::  DEPSXX,DEPSYY,DEPSXY,
     .   EPSXX,EPSYY,EPSXY,EPSYZ,EPSZX,SIGOXX,SIGOYY,SIGOXY,SIGOYZ,SIGOZX
      my_real ,DIMENSION(NEL) ,INTENT(OUT) ::
     .   SIGNXX,SIGNYY,SIGNXY,SIGNYZ,SIGNZX,ET
      my_real ,DIMENSION(NEL,NUVAR) ,INTENT(INOUT) :: UVAR
      TYPE(TTABLE), DIMENSION(NTABLE) ::  TABLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,II,NINDXU,NINDXL,NINDXR,NINDXT,NINDXC,FUNC,FUND,FUNCR,
     .           NDIM,IRELOAD
      my_real :: NU12,NU21,G12,A11,A12,A22,RCOMP,
     .           FSCALE,FSCALE1,FSCALE2,FSCALET,DW,HT,HF,
     .           DET,S,D,T,P1,P2,R,DEZZ,SVM2,XINT,YINT
      INTEGER ,DIMENSION(NEL)   :: INDXU,INDXL,INDXR,INDXT,INDXC
      my_real ,DIMENSION(NEL)   :: EPSQ,SVM,ETX,ETL,ETU,
     .  SIGF,EMAX,SMAX,EMINRL,SMINRL,EMAXRL,SMAXRL,BETA
      INTEGER ,DIMENSION(NEL,1) :: IPOS1
      my_real ,DIMENSION(NEL,1) :: XX1
C-----------------------------------------------
C     UVAR(1) = Emax
C     UVAR(2) = Smax
C     UVAR(3) = EminRL
C     UVAR(4) = EmaxRL
C     UVAR(5) = SminRL
C     UVAR(6) = SmaxRL
C=======================================================================
      NU12    = UPARAM(3)
      NU21    = UPARAM(4)
      G12     = UPARAM(6)
      A11     = UPARAM(7)
      A22     = UPARAM(8)
      A12     = UPARAM(9)
      RCOMP   = UPARAM(10)
      FSCALE1 = UPARAM(11)
      FSCALE2 = UPARAM(12)
      FSCALET = UPARAM(13)
      XINT    = UPARAM(18)
      YINT    = UPARAM(19)
      IRELOAD = NINT(UPARAM(21))
      DET     = ONE / (ONE - NU12*NU21)
c
      FUNC = ITABLE(1)
      FUND = ITABLE(2)
      NINDXT = 0
      NINDXC = 0
      NINDXU = 0
      NINDXL = 0
      NINDXR = 0
c-----------------------------------------
c     test principal tension / compression
c-----------------------------------------
      DO I=1,NEL
        S  = HALF*(EPSXX(I) + EPSYY(I))
        D  = HALF*(EPSXX(I) - EPSYY(I))
        R  = SQRT(EPSXY(I)**2 + D*D)
        P1 = S + R
        P2 = S - R
        IF (P2 >= -P1) THEN    ! max principal deformation in tension
          NINDXT = NINDXT + 1
          INDXT(NINDXT) = I
          BETA(I) = ONE
        ELSE
          NINDXC = NINDXC + 1
          INDXC(NINDXC) = I
          BETA(I) = RCOMP
        END IF
      END DO
c-----------------------------------------
      DO I=1,NEL
        SIGNXY(I) = G12*EPSXY(I)*BETA(I)
        SIGNYZ(I) = GS *EPSYZ(I)*BETA(I)
        SIGNZX(I) = GS *EPSZX(I)*BETA(I)
        ET(I) = BETA(I)
      END DO
c-----------------------------------------
      IF (FUNC == 0) THEN
        DO I=1,NEL
          SIGNXX(I) = (A11*EPSXX(I) + A12*EPSYY(I))*BETA(I)
          SIGNYY(I) = (A12*EPSXX(I) + A22*EPSYY(I))*BETA(I)
        END DO
      END IF
c-----------------------------------------
c
      IF (FUNC > 0) THEN
        NDIM = TABLE(FUNC)%NDIM
        DO I=1,NEL
          EPSQ(I) = SQRT((EPSXX(I)**2 + EPSYY(I)**2) / (ONE + NU21**2))
        ENDDO
        XX1(1:NEL,1) = EPSQ(1:NEL)
        IPOS1(1:NEL,1) = 1
c
        CALL TABLE_VINTERP(TABLE(FUNC),NEL,IPOS1,XX1,SIGF,ETL)
c
        DO I=1,NEL
          A11 = ETL(I) * DET * FSCALE1
          A22 = A11 * FSCALET
          A12 = A11 * NU21
          SIGNXX(I) = SIGOXX(I) + (A11*DEPSXX(I) + A12*DEPSYY(I))*BETA(I)
          SIGNYY(I) = SIGOYY(I) + (A12*DEPSXX(I) + A22*DEPSYY(I))*BETA(I)
        ENDDO
      END IF
c-----------------------------------------
c
      IF (FUND > 0) THEN ! nonlinear loading and unloading with hysteresis
c
        EMAX(1:NEL)   = UVAR(1:NEL,1)
        SMAX(1:NEL)   = UVAR(1:NEL,2)
        EMINRL(1:NEL) = UVAR(1:NEL,3)
        EMAXRL(1:NEL) = UVAR(1:NEL,4)
        SMINRL(1:NEL) = UVAR(1:NEL,5)
        SMAXRL(1:NEL) = UVAR(1:NEL,6)
c
        DO I=1,NEL
          SVM (I) = SQRT(SIGNXX(I)**2 + SIGNYY(I)**2 - SIGNXX(I)*SIGNYY(I))
        ENDDO
c
        DO II=1,NINDXT
          I = INDXT(II)
          DW = SIGOXX(I)*DEPSXX(I) + SIGOYY(I)*DEPSYY(I)
          IF (DW < ZERO) THEN
            NINDXU = NINDXU + 1
            INDXU(NINDXU) = I
          ELSE IF (SVM(I) >= SMAX(I)) THEN ! loading
            NINDXL = NINDXL + 1
            INDXL(NINDXL) = I
          ELSE                              ! reloading
            NINDXR = NINDXR + 1
            INDXR(NINDXR) = I
          END IF
        END DO
c
        IF (NINDXL > 0) THEN
c
          DO II=1,NINDXL
            I = INDXL(II)
            A11 = ETL(I) * DET * FSCALE1
            A22 = A11 * FSCALET
            A12 = A11 * NU21
            SIGNXX(I) = SIGOXX(I) + A11*DEPSXX(I) + A12*DEPSYY(I)
            SIGNYY(I) = SIGOYY(I) + A12*DEPSXX(I) + A22*DEPSYY(I)
            SVM2 = SIGNXX(I)**2 + SIGNYY(I)**2 - SIGNXX(I)*SIGNYY(I)
            SVM(I) = SQRT(SVM2)
            EMAX(I)   = EPSQ(I)
            EMINRL(I) = EPSQ(I)
            EMAXRL(I) = EPSQ(I)
            SMAX(I)   = SVM(I)
            SMINRL(I) = SVM(I)
            SMAXRL(I) = SVM(I)
          ENDDO
        END IF
c
        IF (NINDXR > 0) THEN
          IF (IRELOAD == 0) THEN
            FUNCR = FUND! reloading follows unloading curve
            FSCALE = FSCALE2
            DO II=1,NINDXR
              I = INDXR(II)
              XX1(II,1) = EPSQ(I) * XINT / EMAXRL(I)
            END DO
          ELSE
            FUNCR = FUNC! reloading follows loading curve
            FSCALE = FSCALE1
            DO II=1,NINDXR
              I = INDXR(II)
              XX1(II,1) = EMAX(I) * (EPSQ(I) - EMINRL(I)) / (EMAX(I) - EMINRL(I))
            END DO
          END IF
          IPOS1(1:NINDXR,1) = 1
          CALL TABLE_VINTERP(TABLE(FUNCR),NINDXR,IPOS1,XX1,SIGF,ETX)
c
          IF (IRELOAD == 1) THEN! reloading follows loading curve
            DO II=1,NINDXR
              I = INDXR(II)
              HT = (SMAX(I)-SMINRL(I)) / (EMAX(I)-EMINRL(I))
              HF = SMAX(I) / EMAX(I)
              ETX(I) = FSCALE * ETX(I) * HT / HF
c
              A11 = ETX(I) * DET
              A22 = A11 * FSCALET
              A12 = A11 * NU21
              SIGNXX(I) = SIGOXX(I) + A11*DEPSXX(I) + A12*DEPSYY(I)
              SIGNYY(I) = SIGOYY(I) + A12*DEPSXX(I) + A22*DEPSYY(I)
              SVM2 = SIGNXX(I)**2 + SIGNYY(I)**2 - SIGNXX(I)*SIGNYY(I)
              SVM(I) = SQRT(SVM2)
              EMAXRL(I) = EPSQ(I)
              SMAXRL(I) = SVM(I)
            END DO
          ELSE  ! reloading follows unloading curve
            DO II=1,NINDXR
              I = INDXR(II)
              HT = SMAX(I) / EMAX(I)
              HF  = YINT / XINT
              ETX(I) = FSCALE * ETX(I) * HT / HF
c
              A11 = ETX(I) * DET
              A22 = A11 * FSCALET
              A12 = A11 * NU21
              SIGNXX(I) = SIGOXX(I) + A11*DEPSXX(I) + A12*DEPSYY(I)
              SIGNYY(I) = SIGOYY(I) + A12*DEPSXX(I) + A22*DEPSYY(I)
              SVM2 = SIGNXX(I)**2 + SIGNYY(I)**2 - SIGNXX(I)*SIGNYY(I)
              SVM(I) = SQRT(SVM2)
              EMAXRL(I) = EMAX(I)
              SMAXRL(I) = SMAX(I)
            END DO
          END IF
c
        END IF
c
        IF (NINDXU > 0) THEN
c
          DO II=1,NINDXU
            I = INDXU(II)
            XX1(II,1) = EPSQ(I) * XINT / EMAXRL(I)
            IPOS1(II,1) = 1
          END DO
          CALL TABLE_VINTERP(TABLE(FUND),NINDXU,IPOS1,XX1,SIGF,ETU)
c
          DO II=1,NINDXU
            I = INDXU(II)
            ETX(I) = ETU(I) * (SMAXRL(I) / EMAXRL(I)) * (XINT  / YINT)
            IF (EPSQ(I) > ZERO) THEN
              ETX(I) = MAX(ETX(I), SVM(I) / EPSQ(I))
            END IF
          END DO
c
          DO II=1,NINDXU
            I = INDXU(II)
            A11 = ETX(I) * DET * FSCALE2
            A22 = A11 * FSCALET
            A12 = A11 * NU21
            SIGNXX(I) = SIGOXX(I) + A11*DEPSXX(I) + A12*DEPSYY(I)
            SIGNYY(I) = SIGOYY(I) + A12*DEPSXX(I) + A22*DEPSYY(I)
            SVM2 = SIGNXX(I)**2 + SIGNYY(I)**2 - SIGNXX(I)*SIGNYY(I)
            SVM(I) = SQRT(SVM2)
            EMINRL(I) = EPSQ(I)
            SMINRL(I) = SVM(I)
          ENDDO
        END IF
c-------------------------------------------------------------------------
c       compression
c-------------------------------------------------------------------------
        IF (NINDXC > 0) THEN
          DO II=1,NINDXC
            I = INDXC(II)
            A11 = ETL(I) * DET * FSCALE1
            A22 = A11 * FSCALET
            A12 = A11 * NU21
            SIGNXX(I) = SIGOXX(I) + (A11*DEPSXX(I) + A12*DEPSYY(I))*BETA(I)
            SIGNYY(I) = SIGOYY(I) + (A12*DEPSXX(I) + A22*DEPSYY(I))*BETA(I)
          ENDDO
        END IF
c
        UVAR(1:NEL,1) = EMAX(1:NEL)
        UVAR(1:NEL,2) = SMAX(1:NEL)
        UVAR(1:NEL,3) = EMINRL(1:NEL)
        UVAR(1:NEL,4) = EMAXRL(1:NEL)
        UVAR(1:NEL,5) = SMINRL(1:NEL)
        UVAR(1:NEL,6) = SMAXRL(1:NEL)

      END IF  ! FUND > 0
c--------------------------------------------------------
      RETURN
      END
