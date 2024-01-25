Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  FIXVEL                        source/constraints/general/impvel/fixvel.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        VINTERDP                      source/tools/curve/vinterdp.F 
Chd|        VINTER_SMOOTH                 source/tools/curve/vinter_smooth.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE FIXVEL(IBFV  ,A    ,V     ,NPC    ,TF   ,
     2                  VEL   ,MS   ,X     ,SKEW   ,AR   ,
     3                  VR    ,IN   ,NSENSOR,SENSOR_TAB,
     4                  WEIGHT,DEPLA    ,RBY   ,IFRAME,
     5                  XFRAME,DR   ,NODNX_SMS,
     6                  TT_DOUBLE,DEPLA_DOUBLE, PYTHON)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE PYTHON_FUNCT_MOD
      USE SENSOR_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER NPC(*)
      INTEGER IBFV(NIFV,*),WEIGHT(*),IFRAME(LISKN,*),NODNX_SMS(*)
      my_real, DIMENSION(3,*), TARGET :: DEPLA
      REAL(KIND=8), DIMENSION(3,*), TARGET :: DEPLA_DOUBLE
      REAL(KIND=8) :: TT_DOUBLE
      my_real
     .   A(3,*), V(3,*), TF(*), VEL(LFXVELR,*), MS(*), X(3,*), 
     .   SKEW(LSKEW,*), AR(3,*), VR(3,*), IN(*),
     .   RBY(NRBY,*),XFRAME(NXFRAME,*), DR(3,*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
      TYPE(PYTHON_), INTENT(INOUT) :: PYTHON
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, ISK, J, L, K1, K2, K3, ISENS,K,
     .        II, IC, NN, IDEB, NR, NSK, NFK, IFM, N0,
     .        ILENC(MVSIZ), IPOSC(MVSIZ), IADC(MVSIZ),
     .        LC(MVSIZ), INDEX(MVSIZ), ICOOR, ISMOOTH,JJ
      my_real :: 
     .   AX, AXI, AOLD, VV, A0, AA, STARTT, STOPT, 
     .   DYDX,DW,DW2,DD,RX,RY,RZ,VF,VFX,VFY,VFZ,AFX,AFY,AFZ,RW_SMS(MVSIZ),
     .   R, CTH, STH, SKDIR(3), FMDIR(3), HX, HY, HZ, COEF,
     .   ER(3),ET(3),ALPHA, NOR1, NOR2, NOR3, YIMP, SKEWGLOB,
     .   AOLD0(6,MVSIZ)

      REAL(KIND=8) :: TSTMP,FAC,FACX,YC0(MVSIZ),YC(MVSIZ),TSC0(MVSIZ),TSC(MVSIZ),DYDXC(MVSIZ)
      INTEGER, DIMENSION(MVSIZ) :: ILENC_SMOOTH,IPOSC_SMOOTH,IADC_SMOOTH
      my_real, DIMENSION(MVSIZ) :: YC0_SMOOTH,YC_SMOOTH,TSC0_SMOOTH,TSC_SMOOTH,DYDXC_SMOOTH
      INTEGER :: NINDEX_SMOOTH,NINDEX_SMOOTH_WSENS
      INTEGER, DIMENSION(MVSIZ) ::INDEX_SMOOTH

      INTEGER, DIMENSION(MVSIZ) :: ILENC_WO_SMOOTH,IPOSC_WO_SMOOTH,IADC_WO_SMOOTH
      REAL(KIND=8), DIMENSION(MVSIZ) :: YC0_WO_SMOOTH,YC_WO_SMOOTH,TSC0_WO_SMOOTH,TSC_WO_SMOOTH,DYDXC_WO_SMOOTH
      INTEGER :: NINDEX_WO_SMOOTH,NINDEX_WO_SMOOTH_WSENS
      INTEGER, DIMENSION(MVSIZ) ::INDEX_WO_SMOOTH

      REAL(KIND=8), DIMENSION(:,:), POINTER :: D
      REAL(KIND=8) :: DT2_DOUBLE

      INTEGER, DIMENSION(MVSIZ) :: NSENS
      REAL(KIND=8), DIMENSION(MVSIZ) :: TS
C=======================================================================
#if MYREAL4
!   Simple precision
        D => DEPLA_DOUBLE(1:3,1:NUMNOD)
#elif 1
!   Double precision
        D => DEPLA(1:3,1:NUMNOD)
#endif
      DT2_DOUBLE = DT2
      IF(N2D == 1)THEN
       AX=ONE
      ELSE
       AX=ZERO
      ENDIF
C
      NSENS(1:MVSIZ) = 0
      TS(1:MVSIZ) = ZERO           
C
      IDEB = 0
      ISMOOTH = 0
      DO NN=1,NFXVEL,NVSIZ
        IF (IBFV(8,NN) == 1) CYCLE
        IC = 0
        IF (NSENSOR > 0) THEN
          DO II = 1, MIN(NFXVEL-IDEB,NVSIZ)
            N = II+IDEB
            IF (IBFV(13,N) > 0 ) CYCLE
            STARTT = VEL(2,N)
            STOPT  = VEL(3,N)
            FACX   = VEL(5,N)
            IF (TT_DOUBLE < STARTT) CYCLE
            IF (TT_DOUBLE > STOPT)  CYCLE
            I = IABS(IBFV(1,N))
            ISENS = IBFV(4,N)
            IF (ISENS > 0) THEN
              TSTMP = TT_DOUBLE - SENSOR_TAB(ISENS)%TSTART
              IF (TSTMP < ZERO) CYCLE
            ELSE
              TSTMP = TT_DOUBLE
            ENDIF
            IC = IC + 1
            INDEX(IC) = N
            NSENS(IC) = ISENS
            TS(IC) = TSTMP
            IF (IBFV(7,N) == 1) THEN
              TSC(IC) = (TS(IC)+HALF*DT2_DOUBLE)*FACX
            ELSEIF (IBFV(7,N) == 2) THEN
              TSC0(IC) = TS(IC)*FACX
              TSC(IC)  = (TS(IC)+DT2_DOUBLE)*FACX
            ELSE
              TSC(IC)  = (TS(IC)+DT2_DOUBLE)*FACX
            ENDIF
            IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
              RW_SMS(IC)=ONE
            ELSE
              ISK   = IBFV(2,N)/10
              IFM   = IBFV(9,N)
              ICOOR = IBFV(10,N)
              J     = IBFV(2,N)
              IF (IFM<=1) J=J-10*ISK
              IF(NODNX_SMS(I)==0)THEN
                RW_SMS(IC)=ONE
              ELSE
                RW_SMS(IC)=ZERO
              END IF
            END IF
          ENDDO
        ELSE
C         sans sensor
          TS = TT_DOUBLE
          DO II = 1, MIN(NFXVEL-IDEB,NVSIZ)
            N = II+IDEB
            IF(IBFV(13,N) > 0 ) CYCLE
            STARTT = VEL(2,N)
            STOPT  = VEL(3,N)
            FACX   = VEL(5,N)
            IF (TT_DOUBLE < STARTT) CYCLE
            IF (TT_DOUBLE > STOPT)  CYCLE
            I=IABS(IBFV(1,N))
            IC = IC + 1
            INDEX(IC) = N
            IF(IBFV(7,N) == 1) THEN
              TSC(IC) = (TT_DOUBLE + HALF*DT2_DOUBLE)*FACX
            ELSE
              TSC(IC) = (TT_DOUBLE+DT2_DOUBLE)*FACX
            ENDIF
            IF(IDTMINS==0.AND.IDTMINS_INT==0)THEN
              RW_SMS(IC)=ONE
            ELSE
              ISK=IBFV(2,N)/10
              IFM = IBFV(9,N)
              ICOOR = IBFV(10,N)
              J=IBFV(2,N)
              IF (IFM<=1) J=J-10*ISK
              IF(NODNX_SMS(I)==0)THEN
                RW_SMS(IC)=ONE
              ELSE
                RW_SMS(IC)=ZERO
              END IF
            END IF
          ENDDO
        ENDIF
C
        IDEB = IDEB + MIN(NFXVEL-IDEB,NVSIZ)
C-------
        NINDEX_SMOOTH = 0
        NINDEX_WO_SMOOTH = 0
C------- First pass for impdisp with sensor - in the top of the list
        DO II=1,IC
            N = INDEX(II)
            L = IBFV(3,N)
            ISMOOTH = 0
            IF (L > 0) ISMOOTH = NPC(2*NFUNCT+L+1)
            IF(NSENS(II) > 0 .and. TS(II) >= ZERO .and. IBFV(7,N) == 2) THEN
              IF(ISMOOTH>0) THEN
                NINDEX_SMOOTH = NINDEX_SMOOTH + 1
                INDEX_SMOOTH(NINDEX_SMOOTH) = II
                TSC_SMOOTH(NINDEX_SMOOTH) = TSC(II)  
                TSC0_SMOOTH(NINDEX_SMOOTH) = TSC0(II)  
              ELSE IF(ISMOOTH==0) THEN
                NINDEX_WO_SMOOTH = NINDEX_WO_SMOOTH + 1
                INDEX_WO_SMOOTH(NINDEX_WO_SMOOTH) = II          
                TSC_WO_SMOOTH(NINDEX_WO_SMOOTH) = TSC(II)  
                TSC0_WO_SMOOTH(NINDEX_WO_SMOOTH) = TSC0(II)  
              ELSE IF(ISMOOTH<0) THEN
                L = -ISMOOTH
                CALL PYTHON_CALL_FUNCT1D(PYTHON,L,TSC(II),YC(II))
                CALL PYTHON_CALL_FUNCT1D(PYTHON,L,TSC0(II),YC0(II))
              ENDIF 
            ENDIF       
        ENDDO
        NINDEX_SMOOTH_WSENS = NINDEX_SMOOTH
        NINDEX_WO_SMOOTH_WSENS = NINDEX_WO_SMOOTH
C------- Second pass
        DO II=1,IC
            N = INDEX(II)
            L = IBFV(3,N)
            ISMOOTH = 0
            IF (L > 0) ISMOOTH = NPC(2*NFUNCT+L+1)
            IF(NSENS(II) == 0 .or. TS(II) < ZERO .or. IBFV(7,N) /= 2) THEN   
              IF(ISMOOTH>0) THEN
                NINDEX_SMOOTH = NINDEX_SMOOTH + 1
                INDEX_SMOOTH(NINDEX_SMOOTH) = II
                TSC_SMOOTH(NINDEX_SMOOTH) = TSC(II) 
              ELSE IF(ISMOOTH==0) THEN
                NINDEX_WO_SMOOTH = NINDEX_WO_SMOOTH + 1
                INDEX_WO_SMOOTH(NINDEX_WO_SMOOTH) = II          
                TSC_WO_SMOOTH(NINDEX_WO_SMOOTH) = TSC(II)  
              ELSE IF(ISMOOTH<0) THEN
                L = -ISMOOTH
                CALL PYTHON_CALL_FUNCT1D(PYTHON,L,TSC(II),YC(II))
              ENDIF
            ENDIF        
        ENDDO

        DO II=1,NINDEX_WO_SMOOTH
            JJ = INDEX_WO_SMOOTH(II)
            N = INDEX(JJ)
            L = IBFV(3,N)
            IF (L > 0) ISMOOTH = NPC(2*NFUNCT+L+1)
            IF(NCYCLE == 0)THEN 
                IPOSC_WO_SMOOTH(II) = 0
            ELSE
                IPOSC_WO_SMOOTH(II) = IBFV(5,N)
            ENDIF
            IADC_WO_SMOOTH(II) = NPC(L) / 2 + 1
            ILENC_WO_SMOOTH(II) = NPC(L+1) / 2 - IADC_WO_SMOOTH(II) - IPOSC_WO_SMOOTH(II)
        ENDDO

        DO II=1,NINDEX_SMOOTH
            JJ = INDEX_SMOOTH(II)
            N = INDEX(JJ)
            L = IBFV(3,N)
            IF (L > 0) ISMOOTH = NPC(2*NFUNCT+L+1)
            IF(NCYCLE == 0)THEN 
                IPOSC_SMOOTH(II) = 0
            ELSE
                IPOSC_SMOOTH(II) = IBFV(5,N)
            ENDIF
            IADC_SMOOTH(II) = NPC(L) / 2 + 1
            ILENC_SMOOTH(II) = NPC(L+1) / 2 - IADC_SMOOTH(II) - IPOSC_SMOOTH(II)
        ENDDO

        IF (NINDEX_WO_SMOOTH > 0) THEN
          IF (NINDEX_WO_SMOOTH_WSENS > 0) 
     .      CALL VINTERDP(TF,IADC_WO_SMOOTH,IPOSC_WO_SMOOTH,ILENC_WO_SMOOTH,NINDEX_WO_SMOOTH_WSENS,
     1                    TSC0_WO_SMOOTH,DYDXC_WO_SMOOTH,YC0_WO_SMOOTH)
          CALL VINTERDP(TF,IADC_WO_SMOOTH,IPOSC_WO_SMOOTH,ILENC_WO_SMOOTH,NINDEX_WO_SMOOTH,
     1                    TSC_WO_SMOOTH,DYDXC_WO_SMOOTH,YC_WO_SMOOTH)

          DO II=1,NINDEX_WO_SMOOTH
            JJ = INDEX_WO_SMOOTH(II)
            YC(JJ) = YC_WO_SMOOTH(II)
            YC0(JJ) = YC0_WO_SMOOTH(II)
            IPOSC(JJ) = IPOSC_WO_SMOOTH(II)
          ENDDO
        ENDIF
        IF (NINDEX_SMOOTH > 0) THEN
          IF (NINDEX_SMOOTH_WSENS > 0) 
     .      CALL VINTER_SMOOTH(TF,IADC_SMOOTH,IPOSC_SMOOTH,ILENC_SMOOTH,NINDEX_SMOOTH_WSENS,
     1                          TSC0_SMOOTH,DYDXC_SMOOTH,YC0_SMOOTH)
          CALL VINTER_SMOOTH(TF,IADC_SMOOTH,IPOSC_SMOOTH,ILENC_SMOOTH,NINDEX_SMOOTH,
     1                          TSC_SMOOTH,DYDXC_SMOOTH,YC_SMOOTH)
          DO II=1,NINDEX_SMOOTH
            JJ = INDEX_SMOOTH(II)
            YC(JJ) = YC_SMOOTH(II)
            YC0(JJ) = YC0_SMOOTH(II)
            IPOSC(JJ) = IPOSC_SMOOTH(II)
          ENDDO
        ENDIF



        IF (IVECTOR == 0) THEN
          DO II=1,IC
            N = INDEX(II)
            IBFV(5,N) = IPOSC(II)
            FAC  = VEL(1,N)
            YIMP   = VEL(6,N)
            VEL(6,N) = YC(II)
C IF sms on the degree of freedom,
C dw was already counted in TFEXT, in sms_fixvel...
            DW       = VEL(4,N)
            TFEXT    = TFEXT + RW_SMS(II)*DW
            VEL(4,N) = (ONE-RW_SMS(II))*VEL(4,N)
            IF (NSENS(II) > 0 .and. TS(II) > ZERO .and. IBFV(7,N) == 2) YC0(II)   = YC0(II) * FAC
            YC(II)   = YC(II) * FAC
            YIMP     = YIMP * FAC
            I        = IABS(IBFV(1,N))
            ISK      = IBFV(2,N)/10
            SKEWGLOB = 0
            IF(ISK/=0)THEN
              IF (SKEW(1,ISK) == ONE   .AND. SKEW(2,ISK) == ZERO .AND.
     .            SKEW(3,ISK) == ZERO .AND. SKEW(4,ISK) == ZERO .AND.
     .            SKEW(5,ISK) == ONE   .AND. SKEW(6,ISK) == ZERO .AND.
     .            SKEW(7,ISK) == ZERO .AND. SKEW(8,ISK) == ZERO .AND.
     .            SKEW(9,ISK) == ONE )  SKEWGLOB = 1
            ENDIF
            IFM   = IBFV(9,N)
            J     = IBFV(2,N)
            ICOOR = IBFV(10,N)
            SKDIR = ZERO
            FMDIR = ZERO
            R     = ONE
            IF (IFM<=1) J=J-10*ISK
           IF (J<=3) THEN
            AXI=ONE-AX+AX*X(2,I)
            IF (ISK<=1.AND.IFM<=1.AND.ICOOR == 0) THEN
              IF (IBFV(7,N) == 2) THEN
                IF (NSENS(II) > 0 .and. TS(II) > ZERO) THEN
                  YC(II) = (YC(II)-YC0(II))/DT2_DOUBLE
                ELSE
                  YC(II)=(YC(II)-D(J,I))/DT2_DOUBLE
                ENDIF
              ENDIF
              IF(IBFV(7,N)>=1) YC(II)=(YC(II)-V(J,I))/DT12
              AOLD=A(J,I)
              A(J,I)=YC(II)
              DW = FOURTH*MS(I)*AXI*WEIGHT(I)
     .           * (A(J,I)*DT12 + TWO*V(J,I))*(A(J,I)-AOLD)

            ELSEIF(ISK > 1.AND.IFM<=1.AND.ICOOR == 0.AND.
     .             SKEWGLOB == 1)THEN
              IF (IBFV(7,N) == 2) THEN
                IF (NSENS(II) > 0 .and. TS(II) > ZERO) THEN
                  YC(II) = (YC(II)-YC0(II))/DT2_DOUBLE
                ELSE
                  YC(II)=(YC(II)-D(J,I))/DT2_DOUBLE
                ENDIF
              ENDIF
              IF (IBFV(7,N)>=1) YC(II)=(YC(II)-V(J,I))/DT12
              AOLD=A(J,I)
              A(J,I)=YC(II)
              DW = FOURTH*MS(I)*AXI*WEIGHT(I)
     .           * (A(J,I)*DT12 + TWO*V(J,I))*(A(J,I)-AOLD)
            ELSEIF (ISK > 1 .AND. ICOOR == 0) THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              IF (IBFV(7,N) == 2) THEN
                DD = SKEW(K1,ISK)*D(1,I) +  
     .               SKEW(K2,ISK)*D(2,I) +  
     .               SKEW(K3,ISK)*D(3,I)  
              ENDIF
              VV = SKEW(K1,ISK)*V(1,I) +
     .             SKEW(K2,ISK)*V(2,I) +
     .             SKEW(K3,ISK)*V(3,I)
              A0 = SKEW(K1,ISK)*A(1,I) +
     .             SKEW(K2,ISK)*A(2,I) +
     .             SKEW(K3,ISK)*A(3,I)

              IF (IBFV(7,N) == 2) THEN
                IF (NSENS(II) > 0 .and. TS(II) > ZERO) THEN
                  YC(II) = (YC(II)-YC0(II))/DT2_DOUBLE
                ELSE
                  YC(II) = (YC(II)-DD)/DT2_DOUBLE
                ENDIF
              ENDIF
              IF (IBFV(7,N) >= 1) YC(II) = (YC(II)-VV)/DT12
              AA = YC(II) - A0
              A(1,I)=A(1,I)+SKEW(K1,ISK)*AA
              A(2,I)=A(2,I)+SKEW(K2,ISK)*AA
              A(3,I)=A(3,I)+SKEW(K3,ISK)*AA
              DW = FOURTH*MS(I)*(YC(II)*DT12+TWO*VV)*AA*AXI*WEIGHT(I)
            ELSEIF ((ISK<=1.AND.IFM<=1.AND.ICOOR == 1) .OR.
     &               (ISK > 1 .AND. ICOOR == 1)) THEN
              COEF = (SKEW(10,ISK)-X(1,I))*SKEW(7,ISK) +
     &                (SKEW(11,ISK)-X(2,I))*SKEW(8,ISK) +
     &                (SKEW(12,ISK)-X(3,I))*SKEW(9,ISK)
              HX = COEF*SKEW(7,ISK)+X(1,I)-SKEW(10,ISK)
              HY = COEF*SKEW(8,ISK)+X(2,I)-SKEW(11,ISK)
              HZ = COEF*SKEW(9,ISK)+X(3,I)-SKEW(12,ISK)
              R  = SQRT(HX*HX+HY*HY+HZ*HZ)
              IF(R<=EM20) THEN
                CTH = ZERO
                STH = ZERO
              ELSE
                CTH = (SKEW(1,ISK)*HX+SKEW(2,ISK)*HY+SKEW(3,ISK)*HZ)/R
                STH = (SKEW(4,ISK)*HX+SKEW(5,ISK)*HY+SKEW(6,ISK)*HZ)/R
              ENDIF
              IF (J == 1) THEN
                SKDIR(1) =CTH*SKEW(1,ISK)+ STH*SKEW(4,ISK)
                SKDIR(2) =CTH*SKEW(2,ISK)+ STH*SKEW(5,ISK)
                SKDIR(3) =CTH*SKEW(3,ISK)+ STH*SKEW(6,ISK)
              ELSEIF(J == 2) THEN
                ER(1) =CTH*SKEW(1,ISK)+ STH*SKEW(4,ISK)
                ER(2) =CTH*SKEW(2,ISK)+ STH*SKEW(5,ISK)
                ER(3) =CTH*SKEW(3,ISK)+ STH*SKEW(6,ISK)
                NOR1=SQRT(ER(1)*ER(1)+ER(2)*ER(2)+ER(3)*ER(3))
                IF (NOR1 > EM20) ER=ER/NOR1
                ER=ER/MAX(NOR1,EM20)
                ET(1) =CTH*SKEW(4,ISK)- STH*SKEW(1,ISK)
                ET(2) =CTH*SKEW(5,ISK)- STH*SKEW(2,ISK)
                ET(3) =CTH*SKEW(6,ISK)- STH*SKEW(3,ISK)
                NOR2=SQRT(ET(1)*ET(1)+ET(2)*ET(2)+ET(3)*ET(3))
                ET=ET/MAX(NOR2,EM20)
c-----------------vitesse angulaire imposee---------
                ALPHA=-YC(II)*DT2_DOUBLE/TWO
c-----------------sinon-----------------------------
c                ALPHA=-YC(II)*DT2_DOUBLE/(TWO*R)
                SKDIR=ALPHA*ER+ET
              ELSEIF(J == 3) THEN
                SKDIR(1)=SKEW(7,ISK)
                SKDIR(2)=SKEW(8,ISK)
                SKDIR(3)=SKEW(9,ISK)
                IF(IBFV(7,N) == 2) YIMP= SKDIR(1)*D(1,I) +
     &                                   SKDIR(2)*D(2,I) +
     &                                   SKDIR(3)*D(3,I)
              ENDIF
              NOR3 = SQRT(SKDIR(1)*SKDIR(1)
     &                   +SKDIR(2)*SKDIR(2)
     &                   +SKDIR(3)*SKDIR(3))
              SKDIR=SKDIR/MAX(NOR3,EM20)
              VV = V(1,I)*SKDIR(1) + V(2,I)*SKDIR(2) + V(3,I)*SKDIR(3)
              A0 = A(1,I)*SKDIR(1) + A(2,I)*SKDIR(2) + A(3,I)*SKDIR(3)
              IF(IBFV(7,N) == 2) YC(II)=(YC(II)-YIMP)/DT2_DOUBLE
              IF(IBFV(7,N)>=1) THEN
                IF (J == 2) THEN
c                 YC(II)=(YC(II)-VV)/DT12
c-----------------vitesse angulaire imposee--------
                   YC(II)=(R*YC(II)-VV)/DT12
c----------------------------------------------------
                ELSE
                  YC(II)=(YC(II)-VV)/DT12
                ENDIF
              ENDIF
              AA = YC(II) - A0
              IF(R<=EM20) AA=ZERO
              A(1,I)=A(1,I)+SKDIR(1)*AA
              A(2,I)=A(2,I)+SKDIR(2)*AA
              A(3,I)=A(3,I)+SKDIR(3)*AA
              DW = FOURTH*MS(I)*(YC(II)*DT12+TWO*VV)*AA*AXI*WEIGHT(I)
            ELSEIF (IFM > 1.AND.ICOOR == 0) THEN
C             Fixed velocity in moving frame (transl)
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              RX  = X(1,I) - XFRAME(10,IFM)
              RY  = X(2,I) - XFRAME(11,IFM)
              RZ  = X(3,I) - XFRAME(12,IFM)
              VFX = XFRAME(31,IFM)+XFRAME(14,IFM)*RZ-XFRAME(15,IFM)*RY
              VFY = XFRAME(32,IFM)+XFRAME(15,IFM)*RX-XFRAME(13,IFM)*RZ
              VFZ = XFRAME(33,IFM)+XFRAME(13,IFM)*RY-XFRAME(14,IFM)*RX
              VV = XFRAME(K1,IFM)*V(1,I)
     .           + XFRAME(K2,IFM)*V(2,I)
     .           + XFRAME(K3,IFM)*V(3,I)
              VF = XFRAME(K1,IFM)*VFX
     .           + XFRAME(K2,IFM)*VFY
     .           + XFRAME(K3,IFM)*VFZ
              A0 = XFRAME(K1,IFM)*A(1,I)
     .           + XFRAME(K2,IFM)*A(2,I)
     .           + XFRAME(K3,IFM)*A(3,I)
              YC(II)=(YC(II)-VV+VF)/DT12
              AA = YC(II) - A0
              A(1,I)=A(1,I)+XFRAME(K1,IFM)*AA
              A(2,I)=A(2,I)+XFRAME(K2,IFM)*AA
              A(3,I)=A(3,I)+XFRAME(K3,IFM)*AA
              DW = FOURTH*MS(I)*(YC(II)*DT12+TWO*VV)*AA*AXI*WEIGHT(I)
            ELSEIF (IFM > 1.AND.ICOOR == 1) THEN
              COEF = (XFRAME(10,IFM)-X(1,I))*XFRAME(7,IFM) +
     &               (XFRAME(11,IFM)-X(2,I))*XFRAME(8,IFM) +
     &               (XFRAME(12,IFM)-X(3,I))*XFRAME(9,IFM)
              HX = COEF*XFRAME(7,IFM)+X(1,I)-XFRAME(10,IFM)
              HY = COEF*XFRAME(8,IFM)+X(2,I)-XFRAME(11,IFM)
              HZ = COEF*XFRAME(9,IFM)+X(3,I)-XFRAME(12,IFM)
              R  = SQRT(HX*HX+HY*HY+HZ*HZ)
              IF(R<=EM20) THEN
                CTH = ZERO
                STH = ZERO
              ELSE
                CTH = (XFRAME(1,IFM)*HX +
     &                 XFRAME(2,IFM)*HY +
     &                 XFRAME(3,IFM)*HZ)/R
                STH = (XFRAME(4,IFM)*HX +
     &                 XFRAME(5,IFM)*HY +
     &                 XFRAME(6,IFM)*HZ)/R
              ENDIF
              IF (J == 1) THEN
                FMDIR(1) =CTH*XFRAME(1,IFM)+ STH*XFRAME(4,IFM)
                FMDIR(2) =CTH*XFRAME(2,IFM)+ STH*XFRAME(5,IFM)
                FMDIR(3) =CTH*XFRAME(3,IFM)+ STH*XFRAME(6,IFM)
              ELSEIF (J == 2) THEN
                ER(1) =CTH*XFRAME(1,IFM)+ STH*XFRAME(4,IFM)
                ER(2) =CTH*XFRAME(2,IFM)+ STH*XFRAME(5,IFM)
                ER(3) =CTH*XFRAME(3,IFM)+ STH*XFRAME(6,IFM)
                NOR1=SQRT(ER(1)*ER(1)+ER(2)*ER(2)+ER(3)*ER(3))
                ER=ER/MAX(NOR1,EM20)
                ET(1) =CTH*XFRAME(4,IFM)- STH*XFRAME(1,IFM)
                ET(2) =CTH*XFRAME(5,IFM)- STH*XFRAME(2,IFM)
                ET(3) =CTH*XFRAME(6,IFM)- STH*XFRAME(3,IFM)
                NOR2=SQRT(ET(1)*ET(1)+ET(2)*ET(2)+ET(3)*ET(3))
                ET=ET/MAX(NOR2,EM20)
c---------------vitesse angulaire imposee--------
                ALPHA=-YC(II)*DT2_DOUBLE/TWO
c-----------------------------------------------
c                ALPHA=-YC(II)*DT2_DOUBLE/(TWO*R)
                FMDIR=ALPHA*ER+ET
              ELSEIF (J == 3) THEN
                FMDIR(1)=XFRAME(7,IFM)
                FMDIR(2)=XFRAME(8,IFM)
                FMDIR(3)=XFRAME(9,IFM)
              ENDIF
              NOR3 = SQRT(FMDIR(1)*FMDIR(1)
     &                   +FMDIR(2)*FMDIR(2)
     &                   +FMDIR(3)*FMDIR(3))
              FMDIR=FMDIR/MAX(NOR1,EM20)
              RX  = X(1,I) - XFRAME(10,IFM)
              RY  = X(2,I) - XFRAME(11,IFM)
              RZ  = X(3,I) - XFRAME(12,IFM)
              VFX = XFRAME(31,IFM)+XFRAME(14,IFM)*RZ-XFRAME(15,IFM)*RY
              VFY = XFRAME(32,IFM)+XFRAME(15,IFM)*RX-XFRAME(13,IFM)*RZ
              VFZ = XFRAME(33,IFM)+XFRAME(13,IFM)*RY-XFRAME(14,IFM)*RX
              VV = FMDIR(1)*V(1,I) + FMDIR(2)*V(2,I) + FMDIR(3)*V(3,I)
              VF = FMDIR(1)*VFX + FMDIR(2)*VFY + FMDIR(3)*VFZ
              A0 = FMDIR(1)*A(1,I) + FMDIR(2)*A(2,I) + FMDIR(3)*A(3,I)
              IF (J == 2) THEN
c               YC(II)=(YC(II)-VV+VF)/DT12
c---------------vitesse angulaire imposee--------
                YC(II)=(R*YC(II)-VV+VF)/DT12
c-------------------------------------------------
              ELSE
                YC(II)=(YC(II)-VV+VF)/DT12
              ENDIF
              AA = YC(II) - A0
              IF(R<=EM20) AA=ZERO
              A(1,I)=A(1,I)+FMDIR(1)*AA
              A(2,I)=A(2,I)+FMDIR(2)*AA
              A(3,I)=A(3,I)+FMDIR(3)*AA
              DW = FOURTH*MS(I)*(YC(II)*DT12+TWO*VV)*AA*AXI*WEIGHT(I)
            ENDIF
          ELSEIF(J<=6)THEN
            J = J - 3
            IF(ISK<=1.AND.IFM<=1.AND.ICOOR == 0)THEN
              IF (IBFV(7,N) == 2) THEN
                IF (NSENS(II) > 0 .and. TS(II) > ZERO) THEN
                  YC(II) = (YC(II)-YC0(II))/DT2_DOUBLE
                ELSE
                  YC(II)=(YC(II)-DR(J,I))/DT2_DOUBLE
                ENDIF
              ENDIF
              IF (IBFV(7,N)>=1) YC(II)=(YC(II)-VR(J,I))/DT12
              AOLD   = AR(J,I)
              AR(J,I)= YC(II)
              IF(IBFV(6,N) == 0)THEN
                DW=FOURTH *
     .           (AR(J,I)*DT12 + TWO*VR(J,I))*IN(I)*(AR(J,I)-AOLD) *
     .           WEIGHT(I)
              ELSE
                NR = IBFV(6,N)
                DW= FOURTH *
     .          (AR(J,I)*DT12 + TWO*VR(J,I))*WEIGHT(I)*(AR(J,I)-AOLD)*
     .          (RBY(16+J,NR) + RBY(19+J,NR) + RBY(22+J,NR))
              ENDIF
            ELSEIF (ISK > 1.AND.ICOOR == 0) THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              IF(IBFV(7,N) == 2) THEN
                DD = SKEW(K1,ISK)*DR(1,I) +
     .               SKEW(K2,ISK)*DR(2,I) +
     .               SKEW(K3,ISK)*DR(3,I)
              END IF
              VV = SKEW(K1,ISK)*VR(1,I) +
     .             SKEW(K2,ISK)*VR(2,I) +
     .             SKEW(K3,ISK)*VR(3,I)
              A0 = SKEW(K1,ISK)*AR(1,I) +
     .             SKEW(K2,ISK)*AR(2,I) +
     .             SKEW(K3,ISK)*AR(3,I)
              IF (IBFV(7,N) == 2) YC(II)=(YC(II)-DD)/DT2_DOUBLE
              IF (IBFV(7,N)>=1) YC(II)=(YC(II)-VV)/DT12
              AA = YC(II) - A0
              AR(1,I)=AR(1,I)+SKEW(K1,ISK)*AA
              AR(2,I)=AR(2,I)+SKEW(K2,ISK)*AA
              AR(3,I)=AR(3,I)+SKEW(K3,ISK)*AA
              IF(IBFV(6,N) == 0)THEN
                DW= FOURTH*(YC(II)*DT12+TWO*VV)*IN(I)*AA*WEIGHT(I)
              ELSE
                NR = IBFV(6,N)
                DW= FOURTH*(YC(II)*DT12+TWO*VV)*WEIGHT(I)*AA*
     .             ((RBY(17,NR)*SKEW(K1,ISK)
     .              +RBY(18,NR)*SKEW(K2,ISK)
     .              +RBY(19,NR)*SKEW(K3,ISK))*SKEW(K1,ISK) +
     .              (RBY(20,NR)*SKEW(K1,ISK)
     .              +RBY(21,NR)*SKEW(K2,ISK)
     .              +RBY(22,NR)*SKEW(K3,ISK))*SKEW(K2,ISK) +
     .              (RBY(23,NR)*SKEW(K1,ISK)
     .              +RBY(24,NR)*SKEW(K2,ISK)
     .              +RBY(25,NR)*SKEW(K3,ISK))*SKEW(K3,ISK))
              ENDIF
            ELSEIF ((ISK<=1.AND.IFM<=1.AND.ICOOR == 1) .OR.
     &               (ISK > 1.AND.ICOOR == 1)) THEN
              COEF = (SKEW(10,ISK)-X(1,I))*SKEW(7,ISK) +
     &               (SKEW(11,ISK)-X(2,I))*SKEW(8,ISK) +
     &               (SKEW(12,ISK)-X(3,I))*SKEW(9,ISK)
              HX = COEF*SKEW(7,ISK)+X(1,I)-SKEW(10,ISK)
              HY = COEF*SKEW(8,ISK)+X(2,I)-SKEW(11,ISK)
              HZ = COEF*SKEW(9,ISK)+X(3,I)-SKEW(12,ISK)
              R  = SQRT(HX*HX+HY*HY+HZ*HZ)
              IF(R<=EM20) THEN
                CTH = ZERO
                STH = ZERO
              ELSE
                CTH = (SKEW(1,ISK)*HX+SKEW(2,ISK)*HY+SKEW(3,ISK)*HZ)/R
                STH = (SKEW(4,ISK)*HX+SKEW(5,ISK)*HY+SKEW(6,ISK)*HZ)/R
              ENDIF
              IF (J == 1) THEN
                SKDIR(1) =CTH*SKEW(1,ISK)+ STH*SKEW(4,ISK)
                SKDIR(2) =CTH*SKEW(2,ISK)+ STH*SKEW(5,ISK)
                SKDIR(3) =CTH*SKEW(3,ISK)+ STH*SKEW(6,ISK)
              ELSEIF(J == 2) THEN
                SKDIR(1) =CTH*SKEW(4,ISK)- STH*SKEW(1,ISK)
                SKDIR(2) =CTH*SKEW(5,ISK)- STH*SKEW(2,ISK)
                SKDIR(3) =CTH*SKEW(6,ISK)- STH*SKEW(3,ISK)
              ELSEIF(J == 3) THEN
                SKDIR(1)=SKEW(7,ISK)
                SKDIR(2)=SKEW(8,ISK)
                SKDIR(3)=SKEW(9,ISK)
                IF(IBFV(7,N) == 2) YIMP= SKDIR(1)*DR(1,I) +
     &                                   SKDIR(2)*DR(2,I) +
     &                                   SKDIR(3)*DR(3,I)
              ENDIF
              NOR3 = SQRT(SKDIR(1)*SKDIR(1)
     &                   +SKDIR(2)*SKDIR(2)
     &                   +SKDIR(3)*SKDIR(3))
              SKDIR=SKDIR/MAX(NOR3,EM20)
              IF(IBFV(7,N) == 2) YC(II)=(YC(II)-YIMP)/DT2_DOUBLE
              VV =VR(1,I)*SKDIR(1)+ VR(2,I)*SKDIR(2)+ VR(3,I)*SKDIR(3)
              A0 =AR(1,I)*SKDIR(1)+ AR(2,I)*SKDIR(2)+ AR(3,I)*SKDIR(3)
              IF(IBFV(7,N)>=1) YC(II)=(YC(II)-VV)/DT12
              AA = YC(II) - A0
              IF(R<=EM20) AA=ZERO
              AR(1,I)=AR(1,I)+SKDIR(1)*AA
              AR(2,I)=AR(2,I)+SKDIR(2)*AA
              AR(3,I)=AR(3,I)+SKDIR(3)*AA
              IF(IBFV(6,N) == 0)THEN
                DW= FOURTH*(YC(II)*DT12+TWO*VV)*IN(I)*AA*WEIGHT(I)
              ELSE
                NR = IBFV(6,N)
                DW= FOURTH*(YC(II)*DT12+TWO*VV)*WEIGHT(I)*AA*
     &              ((RBY(17,NR)*SKDIR(1)
     &               +RBY(18,NR)*SKDIR(2)
     &               +RBY(19,NR)*SKDIR(3))*SKDIR(1) +
     &               (RBY(20,NR)*SKDIR(1)
     &               +RBY(21,NR)*SKDIR(2)
     &               +RBY(22,NR)*SKDIR(3))*SKDIR(2) +
     &               (RBY(23,NR)*SKDIR(1)
     &               +RBY(24,NR)*SKDIR(2)
     &               +RBY(25,NR)*SKDIR(3))*SKDIR(3))
              ENDIF
            ELSEIF (IFM > 1.AND.ICOOR == 0) THEN
C             Fixed velocity in moving frame (rota)
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              VV = XFRAME(K1,IFM)*VR(1,I)
     .           + XFRAME(K2,IFM)*VR(2,I)
     .           + XFRAME(K3,IFM)*VR(3,I)
              VF = XFRAME(K1,IFM)*XFRAME(13,IFM)
     .           + XFRAME(K2,IFM)*XFRAME(14,IFM)
     .           + XFRAME(K3,IFM)*XFRAME(15,IFM)
              A0 = XFRAME(K1,IFM)*AR(1,I)
     .           + XFRAME(K2,IFM)*AR(2,I)
     .           + XFRAME(K3,IFM)*AR(3,I)
              AA = (YC(II)-VV+VF)/DT12 - A0
              AR(1,I)=AR(1,I)+XFRAME(K1,IFM)*AA
              AR(2,I)=AR(2,I)+XFRAME(K2,IFM)*AA
              AR(3,I)=AR(3,I)+XFRAME(K3,IFM)*AA
              IF(IBFV(6,N) == 0)THEN
                DW= FOURTH*(YC(II)+VV)*IN(I)*AA*WEIGHT(I)
              ELSE
                NR = IBFV(6,N)
                DW= FOURTH*(YC(II)+VV)*WEIGHT(I)*AA*
     .             ((RBY(17,NR)*XFRAME(K1,IFM)
     .              +RBY(18,NR)*XFRAME(K2,IFM)
     .              +RBY(19,NR)*XFRAME(K3,IFM))*XFRAME(K1,IFM) +
     .              (RBY(20,NR)*XFRAME(K1,IFM)
     .              +RBY(21,NR)*XFRAME(K2,IFM)
     .              +RBY(22,NR)*XFRAME(K3,IFM))*XFRAME(K2,IFM) +
     .              (RBY(23,NR)*XFRAME(K1,IFM)
     .              +RBY(24,NR)*XFRAME(K2,IFM)
     .              +RBY(25,NR)*XFRAME(K3,IFM))*XFRAME(K3,IFM))
              ENDIF
            ELSEIF (IFM > 1.AND.ICOOR == 1) THEN
              COEF = (XFRAME(10,IFM)-X(1,I))*XFRAME(7,IFM) +
     &               (XFRAME(11,IFM)-X(2,I))*XFRAME(8,IFM) +
     &               (XFRAME(12,IFM)-X(3,I))*XFRAME(9,IFM)
              HX = COEF*XFRAME(7,IFM)+X(1,I)-XFRAME(10,IFM)
              HY = COEF*XFRAME(8,IFM)+X(2,I)-XFRAME(11,IFM)
              HZ = COEF*XFRAME(9,IFM)+X(3,I)-XFRAME(12,IFM)
              R  = SQRT(HX*HX+HY*HY+HZ*HZ)
              IF(R<=EM20) THEN
                CTH = ZERO
                STH = ZERO
              ELSE
                CTH = (XFRAME(1,IFM)*HX +
     &                 XFRAME(2,IFM)*HY +
     &                 XFRAME(3,IFM)*HZ)/R
                STH = (XFRAME(4,IFM)*HX +
     &                 XFRAME(5,IFM)*HY +
     &                 XFRAME(6,IFM)*HZ)/R
              ENDIF
              IF (J == 1) THEN
                FMDIR(1) =CTH*XFRAME(1,IFM)+ STH*XFRAME(4,IFM)
                FMDIR(2) =CTH*XFRAME(2,IFM)+ STH*XFRAME(5,IFM)
                FMDIR(3) =CTH*XFRAME(3,IFM)+ STH*XFRAME(6,IFM)
              ELSEIF (J == 2) THEN
                FMDIR(1) =CTH*XFRAME(4,IFM)- STH*XFRAME(1,IFM)
                FMDIR(2) =CTH*XFRAME(5,IFM)- STH*XFRAME(2,IFM)
                FMDIR(3) =CTH*XFRAME(6,IFM)- STH*XFRAME(3,IFM)
              ELSEIF (J == 3) THEN
                FMDIR(1)=XFRAME(7,IFM)
                FMDIR(2)=XFRAME(8,IFM)
                FMDIR(3)=XFRAME(9,IFM)
              ENDIF
              NOR3 = SQRT(FMDIR(1)*FMDIR(1)
     &                   +FMDIR(2)*FMDIR(2)
     &                   +FMDIR(3)*FMDIR(3))
              FMDIR=FMDIR/MAX(NOR3,EM20)
              VV = FMDIR(1)*VR(1,I) +
     &             FMDIR(2)*VR(2,I) +
     &             FMDIR(3)*VR(3,I)
              VF = FMDIR(1)*XFRAME(13,IFM) +
     &             FMDIR(2)*XFRAME(14,IFM) +
     &             FMDIR(3)*XFRAME(15,IFM)
              A0 = FMDIR(1)*AR(1,I) +
     &             FMDIR(2)*AR(2,I) +
     &             FMDIR(3)*AR(3,I)
              AA = (YC(II)-VV+VF)/DT12 - A0
              IF(R<=EM20) AA=ZERO
              AR(1,I)=AR(1,I)+FMDIR(1)*AA
              AR(2,I)=AR(2,I)+FMDIR(2)*AA
              AR(3,I)=AR(3,I)+FMDIR(3)*AA
              IF(IBFV(6,N) == 0)THEN
                DW= FOURTH*(YC(II)+VV)*IN(I)*AA*WEIGHT(I)
              ELSE
                NR = IBFV(6,N)
                DW= FOURTH*(YC(II)+VV)*WEIGHT(I)*AA*
     &              ((RBY(17,NR)*FMDIR(1)
     &               +RBY(18,NR)*FMDIR(2)
     &               +RBY(19,NR)*FMDIR(3))*FMDIR(1) +
     &               (RBY(20,NR)*FMDIR(1)
     &               +RBY(21,NR)*FMDIR(2)
     &               +RBY(22,NR)*FMDIR(3))*FMDIR(2) +
     &               (RBY(23,NR)*FMDIR(1)
     &               +RBY(24,NR)*FMDIR(2)
     &               +RBY(25,NR)*FMDIR(3))*FMDIR(3))
              ENDIF
            ENDIF
          ENDIF
          TFEXT=TFEXT + DT1*DW
C DT2_DOUBLE*dw into memory ; if sms on the degree of freedom,
C part of DT2_DOUBLE*dw was already computed and stored in sms_fixvel...
          VEL(4,N) = VEL(4,N)+DT2_DOUBLE*DW
         ENDDO
c----------------------------------------------------------------------------
        ELSE    !    partie vectorielle
c----------------------------------------------------------------------------
        NSK=0
        NFK=0
        DO II=1,IC
          N = INDEX(II)
          IFM = IBFV(9,N)
          ISK = IBFV(2,N)/10
          ICOOR=IBFV(10,N)
          IF (IFM > 1) THEN
            NFK = 1
          ELSEIF (ISK > 1) THEN
            NSK=1
          ENDIF
         ENDDO

         IF (NSK == 0.AND.NFK == 0.AND.ICOOR == 0)THEN
#include "vectorize.inc"
           DO II=1,IC
             N = INDEX(II)
             IBFV(5,N) = IPOSC(II)
             FAC  = VEL(1,N)
C if sms on the degree of freedom,
C dw was already counted in TFEXT, in sms_fixvel...
             DW      = VEL(4,N)
             TFEXT   = TFEXT + RW_SMS(II)*DW
             VEL(4,N)= (ONE-RW_SMS(II))*VEL(4,N)
             YC(II) = YC(II) * FAC
             I=IABS(IBFV(1,N))
             ISK=IBFV(2,N)/10
             J=IBFV(2,N)-10*ISK
             IF(J<=3)THEN
               AXI=ONE-AX+AX*X(2,I)
               IF (IBFV(7,N) == 2) THEN
                 IF (NSENS(II) > 0 .and. TS(II) > ZERO) THEN
                   YC(II) = (YC(II)-YC0(II))/DT2_DOUBLE
                 ELSE
                   YC(II) = (YC(II)-D(J,I))/DT2_DOUBLE
                 ENDIF
               ENDIF
               IF(IBFV(7,N)>=1) YC(II)=(YC(II)-V(J,I))/DT12
               AOLD=A(J,I)
               A(J,I)=YC(II)
               DW = FOURTH*MS(I)*AXI*WEIGHT(I)
     .           * (A(J,I)*DT12+TWO*V(J,I))*(A(J,I)-AOLD)
             ELSEIF(J<=6)THEN
               J = J - 3
               IF (IBFV(7,N) == 2) THEN
                 IF (NSENS(II) > 0 .and. TS(II) > ZERO) THEN
                   YC(II) = (YC(II)-YC0(II))/DT2_DOUBLE
                 ELSE
                   YC(II)=(YC(II)-DR(J,I))/DT2_DOUBLE
                 ENDIF
               ENDIF
               IF (IBFV(7,N)>=1) YC(II)=(YC(II)-VR(J,I))/DT12
               AOLD=AR(J,I)
               AR(J,I)= YC(II)
               IF(IBFV(6,N) == 0)THEN
                 DW=FOURTH *
     .           (AR(J,I)*DT12+TWO*VR(J,I))*IN(I)*(AR(J,I)-AOLD) *
     .            WEIGHT(I)
               ELSE
                 NR = IBFV(6,N)
                 DW= FOURTH *
     .            (AR(J,I)*DT12+TWO*VR(J,I))*WEIGHT(I)*(AR(J,I)-AOLD)*
     .            (RBY(16+J,NR) + RBY(19+J,NR) + RBY(22+J,NR))
               ENDIF
             ENDIF
C DT2_DOUBLE*dw into memory ; if sms on the degree of freedom,
C part of DT2_DOUBLE*dw was already computed and stored in sms_fixvel...
             TFEXT=TFEXT + DT1*DW
             VEL(4,N) = VEL(4,N)+DT2_DOUBLE*DW
           ENDDO
         ELSEIF (NSK == 1.AND.NFK == 0.AND.ICOOR == 0) THEN
           DO K=1,3
#include "vectorize.inc"
             DO II=1,IC
               N = INDEX(II)
               ISK=IBFV(2,N)/10
               J=IBFV(2,N)-10*ISK
               IF(J == K)THEN
                 IBFV(5,N) = IPOSC(II)
                 FAC  = VEL(1,N)
C if sms on the degree of freedom,
C dw was already counted in TFEXT, in sms_fixvel...
                 DW      = VEL(4,N)
                 TFEXT   = TFEXT + RW_SMS(II)*DW
                 VEL(4,N)= (ONE-RW_SMS(II))*VEL(4,N)
                 YC(II) = YC(II) * FAC
                 I=IABS(IBFV(1,N))
                 AXI=ONE-AX+AX*X(2,I)
                 K1=3*J-2
                 K2=3*J-1
                 K3=3*J
                 DD = SKEW(K1,ISK)*D(1,I) +
     .                SKEW(K2,ISK)*D(2,I) +
     .                SKEW(K3,ISK)*D(3,I)
                 VV = SKEW(K1,ISK)*V(1,I) +
     .                SKEW(K2,ISK)*V(2,I) +
     .                SKEW(K3,ISK)*V(3,I)
                 A0 = SKEW(K1,ISK)*A(1,I) +
     .                SKEW(K2,ISK)*A(2,I) +
     .                SKEW(K3,ISK)*A(3,I)
                 IF (IBFV(7,N) == 2) YC(II)=(YC(II)-DD)/DT2_DOUBLE
                 IF (IBFV(7,N)>=1) YC(II)=(YC(II)-VV)/DT12
                 AA = YC(II) - A0
                 A(1,I)=A(1,I)+SKEW(K1,ISK)*AA
                 A(2,I)=A(2,I)+SKEW(K2,ISK)*AA
                 A(3,I)=A(3,I)+SKEW(K3,ISK)*AA
                 DW = FOURTH*MS(I)*(YC(II)*DT12+TWO*VV)*AA*AXI*WEIGHT(I)
C DT2_DOUBLE*dw into memory ; if sms on the degree of freedom,
C part of DT2_DOUBLE*dw was already computed and stored in sms_fixvel...
                 TFEXT=TFEXT + DT1*DW
                 VEL(4,N) = VEL(4,N)+DT2_DOUBLE*DW
               ENDIF
             ENDDO
           ENDDO
           IF (IRODDL>=0)THEN
             DO K=4,3+3*IRODDL
#include "vectorize.inc"
               DO II=1,IC
                 N = INDEX(II)
                 ISK=IBFV(2,N)/10
                 J=IBFV(2,N)-10*ISK
                 IF (J == K) THEN
                   IBFV(5,N) = IPOSC(II)
                   FAC  = VEL(1,N)
                   DW   = VEL(4,N)
                   TFEXT=TFEXT + DW
                   YC(II) = YC(II) * FAC
                   I=IABS(IBFV(1,N))
                   J = J - 3
                   K1=3*J-2
                   K2=3*J-1
                   K3=3*J
                   IF (IBFV(7,N) == 2) THEN
                     DD = SKEW(K1,ISK)*DR(1,I) +
     .                    SKEW(K2,ISK)*DR(2,I) +
     .                    SKEW(K3,ISK)*DR(3,I)
                   END IF
                   VV = SKEW(K1,ISK)*VR(1,I) +
     .                  SKEW(K2,ISK)*VR(2,I) +
     .                  SKEW(K3,ISK)*VR(3,I)
                   A0 = SKEW(K1,ISK)*AR(1,I) +
     .                  SKEW(K2,ISK)*AR(2,I) +
     .                  SKEW(K3,ISK)*AR(3,I)
                   IF (IBFV(7,N) == 2) YC(II) = (YC(II)-DD)/DT2_DOUBLE
                   IF (IBFV(7,N)>=1) YC(II)=(YC(II)-VV)/DT12
                   AA = YC(II) - A0
                   AR(1,I)=AR(1,I)+SKEW(K1,ISK)*AA
                   AR(2,I)=AR(2,I)+SKEW(K2,ISK)*AA
                   AR(3,I)=AR(3,I)+SKEW(K3,ISK)*AA
                   IF(IBFV(6,N) == 0)THEN
                     DW= FOURTH*(YC(II)*DT12+TWO*VV)*IN(I)*AA*WEIGHT(I)
                   ELSE
                     NR = IBFV(6,N)
                     DW= FOURTH*(YC(II)*DT12+TWO*VV)*WEIGHT(I)*AA*
     .                   ((RBY(17,NR)*SKEW(K1,ISK)
     .                    +RBY(18,NR)*SKEW(K2,ISK)
     .                    +RBY(19,NR)*SKEW(K3,ISK))*SKEW(K1,ISK) +
     .                    (RBY(20,NR)*SKEW(K1,ISK)
     .                    +RBY(21,NR)*SKEW(K2,ISK)
     .                     +RBY(22,NR)*SKEW(K3,ISK))*SKEW(K2,ISK) +
     .                     (RBY(23,NR)*SKEW(K1,ISK)
     .                     +RBY(24,NR)*SKEW(K2,ISK)
     .                     +RBY(25,NR)*SKEW(K3,ISK))*SKEW(K3,ISK))
                   ENDIF
                   TFEXT=TFEXT + DT1*DW
                   VEL(4,N) = DT2_DOUBLE*DW
                 ENDIF
               ENDDO
             ENDDO
           ENDIF
         ELSEIF ((NSK == 0.AND.NFK == 0.AND.ICOOR == 1).OR.
     &     (NSK == 1.AND.NFK == 0.AND.ICOOR == 1)) THEN
           DO K=1,3
#include "vectorize.inc"
             DO II=1,IC
               N = INDEX(II)
               ISK=IBFV(2,N)/10
               J=IBFV(2,N)-10*ISK
               SKDIR=ZERO
               R=ONE
               IF(J == K)THEN
                 IBFV(5,N) = IPOSC(II)
                 FAC  = VEL(1,N)
                 YIMP   = VEL(6,N)
                 VEL(6,N) = YC(II)
C if sms on the degree of freedom,
C dw was already counted in TFEXT, in sms_fixvel...
                 DW      = VEL(4,N)
                 TFEXT   = TFEXT + RW_SMS(II)*DW
                 VEL(4,N)= (ONE-RW_SMS(II))*VEL(4,N)
                 YC(II) = YC(II) * FAC
                 YIMP = YIMP * FAC
                 I=IABS(IBFV(1,N))
                 AXI=ONE-AX+AX*X(2,I)
                 COEF = (SKEW(10,ISK)-X(1,I))*SKEW(7,ISK) +
     &                  (SKEW(11,ISK)-X(2,I))*SKEW(8,ISK) +
     &                  (SKEW(12,ISK)-X(3,I))*SKEW(9,ISK)
                 HX = COEF*SKEW(7,ISK)+X(1,I)-SKEW(10,ISK)
                 HY = COEF*SKEW(8,ISK)+X(2,I)-SKEW(11,ISK)
                 HZ = COEF*SKEW(9,ISK)+X(3,I)-SKEW(12,ISK)
                 R  = SQRT(HX*HX+HY*HY+HZ*HZ)
                 IF(R<=EM20) THEN
                   CTH = ZERO
                   STH = ZERO
                 ELSE
                   CTH = (SKEW(1,ISK)*HX +
     &                    SKEW(2,ISK)*HY +
     &                    SKEW(3,ISK)*HZ)/R
                   STH = (SKEW(4,ISK)*HX +
     &                    SKEW(5,ISK)*HY +
     &                    SKEW(6,ISK)*HZ)/R
                 ENDIF
                 IF (J == 1) THEN
                   SKDIR(1) =CTH*SKEW(1,ISK)+ STH*SKEW(4,ISK)
                   SKDIR(2) =CTH*SKEW(2,ISK)+ STH*SKEW(5,ISK)
                   SKDIR(3) =CTH*SKEW(3,ISK)+ STH*SKEW(6,ISK)
                 ELSEIF(J == 2) THEN
                   ER(1) =CTH*SKEW(1,ISK)+ STH*SKEW(4,ISK)
                   ER(2) =CTH*SKEW(2,ISK)+ STH*SKEW(5,ISK)
                   ER(3) =CTH*SKEW(3,ISK)+ STH*SKEW(6,ISK)
                   NOR1=SQRT(ER(1)*ER(1)+ER(2)*ER(2)+ER(3)*ER(3))
                   ER=ER/MAX(NOR1,EM20)
                   ET(1) =CTH*SKEW(4,ISK)- STH*SKEW(1,ISK)
                   ET(2) =CTH*SKEW(5,ISK)- STH*SKEW(2,ISK)
                   ET(3) =CTH*SKEW(6,ISK)- STH*SKEW(3,ISK)
                   NOR2=SQRT(ET(1)*ET(1)+ET(2)*ET(2)+ET(3)*ET(3))
                   IF (NOR2 > EM20) ET=ET/NOR2
c-----------------vitesse angulaire imposee---------
                   ALPHA=-YC(II)*DT2_DOUBLE/TWO
c-----------------sinon-----------------------------
c                  ALPHA=-YC(II)*DT2_DOUBLE/(TWO*R)
                   SKDIR=ALPHA*ER+ET
                 ELSEIF(J == 3) THEN
                   SKDIR(1)=SKEW(7,ISK)
                   SKDIR(2)=SKEW(8,ISK)
                   SKDIR(3)=SKEW(9,ISK)
                   IF (IBFV(7,N) == 2) THEN
                     YIMP = SKDIR(1)*D(1,I) +
     &                      SKDIR(2)*D(2,I) +
     &                      SKDIR(3)*D(3,I)
                   ENDIF
                 ENDIF
                 NOR3 = SQRT(SKDIR(1)*SKDIR(1)
     &                 +SKDIR(2)*SKDIR(2)
     &                 +SKDIR(3)*SKDIR(3))
                 IF (NOR3 > EM20) SKDIR=SKDIR/NOR3
                 VV = V(1,I)*SKDIR(1) +
     &                V(2,I)*SKDIR(2) +
     &                V(3,I)*SKDIR(3)
                 A0 = A(1,I)*SKDIR(1) +
     &                A(2,I)*SKDIR(2) +
     &                A(3,I)*SKDIR(3)
                 IF(IBFV(7,N) == 2) YC(II)=(YC(II)-YIMP)/DT2_DOUBLE
                 IF(IBFV(7,N)>=1) THEN
                   IF (J == 2) THEN
c                  YC(II)=(YC(II)-VV)/DT12
c------------------vitesse angulaire imposee--------
                     YC(II)=(R*YC(II)-VV)/DT12
c----------------------------------------------------
                   ELSE
                     YC(II)=(YC(II)-VV)/DT12
                   ENDIF
                 ENDIF
                 AA = YC(II) - A0
                 IF(R<=EM20) AA=ZERO
                 A(1,I)=A(1,I)+SKDIR(1)*AA
                 A(2,I)=A(2,I)+SKDIR(2)*AA
                 A(3,I)=A(3,I)+SKDIR(3)*AA
                 DW = FOURTH*MS(I)*(YC(II)*DT12+TWO*VV)*AA*AXI*WEIGHT(I)
C DT2_DOUBLE*dw into memory ; if sms on the degree of freedom,
C part of DT2_DOUBLE*dw was already computed and stored in sms_fixvel...
                 TFEXT=TFEXT + DT1*DW
                 VEL(4,N) = VEL(4,N)+DT2_DOUBLE*DW
               ENDIF
             ENDDO
           ENDDO
           IF (IRODDL>=0)THEN
             DO K=4,3+3*IRODDL
#include "vectorize.inc"
               DO II=1,IC
                 N = INDEX(II)
                 ISK=IBFV(2,N)/10
                 J=IBFV(2,N)-10*ISK
                 SKDIR=ZERO
                 R=ONE
                 IF(J == K)THEN
                   IBFV(5,N) = IPOSC(II)
                   FAC  = VEL(1,N)
                   YIMP   = VEL(6,N)
                   VEL(6,N) = YC(II)
                   DW   = VEL(4,N)
                   TFEXT=TFEXT + DW
                   YC(II) = YC(II) * FAC
                   YIMP = YIMP * FAC
                   I=IABS(IBFV(1,N))
                   J = J - 3
                   COEF = (SKEW(10,ISK)-X(1,I))*SKEW(7,ISK) +
     &                    (SKEW(11,ISK)-X(2,I))*SKEW(8,ISK) +
     &                    (SKEW(12,ISK)-X(3,I))*SKEW(9,ISK)
                   HX = COEF*SKEW(7,ISK)+X(1,I)-SKEW(10,ISK)
                   HY = COEF*SKEW(8,ISK)+X(2,I)-SKEW(11,ISK)
                   HZ = COEF*SKEW(9,ISK)+X(3,I)-SKEW(12,ISK)
                   R  = SQRT(HX*HX+HY*HY+HZ*HZ)
                   IF(R<=EM20) THEN
                     CTH = ZERO
                     STH = ZERO
                   ELSE
                     CTH = (SKEW(1,ISK)*HX +
     &                      SKEW(2,ISK)*HY +
     &                      SKEW(3,ISK)*HZ)/R
                     STH = (SKEW(4,ISK)*HX +
     &                      SKEW(5,ISK)*HY +
     &                      SKEW(6,ISK)*HZ)/R
                   ENDIF
                   IF (J == 1) THEN
                     SKDIR(1) =CTH*SKEW(1,ISK)+ STH*SKEW(4,ISK)
                     SKDIR(2) =CTH*SKEW(2,ISK)+ STH*SKEW(5,ISK)
                     SKDIR(3) =CTH*SKEW(3,ISK)+ STH*SKEW(6,ISK)
                   ELSEIF(J == 2) THEN
                     SKDIR(1) =CTH*SKEW(4,ISK)- STH*SKEW(1,ISK)
                     SKDIR(2) =CTH*SKEW(5,ISK)- STH*SKEW(2,ISK)
                     SKDIR(3) =CTH*SKEW(6,ISK)- STH*SKEW(3,ISK)
                   ELSEIF(J == 3) THEN
                     SKDIR(1)=SKEW(7,ISK)
                     SKDIR(2)=SKEW(8,ISK)
                     SKDIR(3)=SKEW(9,ISK)
                     IF(IBFV(7,N) == 2) YIMP= SKDIR(1)*DR(1,I) +
     &                                        SKDIR(2)*DR(2,I) +
     &                                        SKDIR(3)*DR(3,I)
                    ENDIF
                    NOR3 = SQRT(SKDIR(1)*SKDIR(1)
     &                         +SKDIR(2)*SKDIR(2)
     &                        +SKDIR(3)*SKDIR(3))
                    SKDIR=SKDIR/MAX(NOR3,EM20)
                    IF(IBFV(7,N) == 2) YC(II)=(YC(II)-YIMP)/DT2_DOUBLE
                    VV = VR(1,I)*SKDIR(1) +
     &                   VR(2,I)*SKDIR(2) +
     &                   VR(3,I)*SKDIR(3)
                    A0 = AR(1,I)*SKDIR(1) +
     &                   AR(2,I)*SKDIR(2) +
     &                   AR(3,I)*SKDIR(3)
                    IF(IBFV(7,N)>=1) YC(II)=(YC(II)-VV)/DT12
                    AA = YC(II) - A0
                    IF(R<=EM20) AA=ZERO
                    AR(1,I)=AR(1,I)+SKDIR(1)*AA
                    AR(2,I)=AR(2,I)+SKDIR(2)*AA
                    AR(3,I)=AR(3,I)+SKDIR(3)*AA
                    IF(IBFV(6,N) == 0)THEN
                      DW= FOURTH*(YC(II)*DT12+TWO*VV)*IN(I)*AA*WEIGHT(I)
                    ELSE
                      NR = IBFV(6,N)
                      DW= FOURTH*(YC(II)*DT12+TWO*VV)*WEIGHT(I)*AA*
     &                    ((RBY(17,NR)*SKDIR(1)
     &                    +RBY(18,NR)*SKDIR(2)
     &                    +RBY(19,NR)*SKDIR(3))*SKDIR(1) +
     &                    (RBY(20,NR)*SKDIR(1)
     &                    +RBY(21,NR)*SKDIR(2)
     &                    +RBY(22,NR)*SKDIR(3))*SKDIR(2) +
     &                    (RBY(23,NR)*SKDIR(1)
     &                    +RBY(24,NR)*SKDIR(2)
     &                    +RBY(25,NR)*SKDIR(3))*SKDIR(3))
                    ENDIF
                    TFEXT=TFEXT + DT1*DW
                    VEL(4,N) = DT2_DOUBLE*DW
                  ENDIF
                ENDDO
              ENDDO
            ENDIF
         ELSEIF (NFK == 1.AND.ICOOR == 0) THEN
           DO K=1,3
#include "vectorize.inc"
             DO II=1,IC
               N = INDEX(II)
               IFM = IBFV(9,N)
               IF (IFM > 1) THEN
                 J = IBFV(2,N)
                 IF(J == K)THEN
                   IBFV(5,N) = IPOSC(II)
                   FAC  = VEL(1,N)
C if sms on the degree of freedom,
C dw was already counted in TFEXT, in sms_fixvel...
                   DW      = VEL(4,N)
                   TFEXT   = TFEXT + RW_SMS(II)*DW
                   VEL(4,N)= (ONE-RW_SMS(II))*VEL(4,N)
                   YC(II) = YC(II) * FAC
                   I=IABS(IBFV(1,N))
                   AXI=1.-AX+AX*X(2,I)
                   K1=3*J-2
                   K2=3*J-1
                   K3=3*J
                   RX = X(1,I) - XFRAME(10,IFM)
                   RY = X(2,I) - XFRAME(11,IFM)
                   RZ = X(3,I) - XFRAME(12,IFM)
                   VFX = VFX + XFRAME(31,IFM)
                   VFY = VFY + XFRAME(32,IFM)
                   VFZ = VFZ + XFRAME(33,IFM)
                   VFX = XFRAME(14,IFM)*RZ-XFRAME(15,IFM)*RY
                   VFY = XFRAME(15,IFM)*RX-XFRAME(13,IFM)*RZ
                   VFZ = XFRAME(13,IFM)*RY-XFRAME(14,IFM)*RX
                   VV = XFRAME(K1,IFM)*V(1,I)
     .                + XFRAME(K2,IFM)*V(2,I)
     .                + XFRAME(K3,IFM)*V(3,I)
                   VF = XFRAME(K1,IFM)*VFX
     .                + XFRAME(K2,IFM)*VFY
     .                + XFRAME(K3,IFM)*VFZ
                   A0 = XFRAME(K1,IFM)*A(1,I)
     .                + XFRAME(K2,IFM)*A(2,I)
     .                + XFRAME(K3,IFM)*A(3,I)
                   YC(II)=(YC(II)-VV+VF)/DT12
                   AA = YC(II) - A0
                   A(1,I)=A(1,I)+XFRAME(K1,IFM)*AA
                   A(2,I)=A(2,I)+XFRAME(K2,IFM)*AA
                   A(3,I)=A(3,I)+XFRAME(K3,IFM)*AA
                   DW=FOURTH*MS(I)*(YC(II)*DT12+TWO*VV)*AA*AXI*WEIGHT(I)
C DT2_DOUBLE*dw into memory ; if sms on the degree of freedom,
C part of DT2_DOUBLE*dw was already computed and stored in sms_fixvel...
                   TFEXT=TFEXT + DT1*DW
                   VEL(4,N) = VEL(4,N)+DT2_DOUBLE*DW
                 ENDIF
               ELSE
                 ISK=IBFV(2,N)/10
                 J=IBFV(2,N)-10*ISK
                 IF(J == K)THEN
                   IBFV(5,N) = IPOSC(II)
                   FAC  = VEL(1,N)
C if sms on the degree of freedom,
C dw was already counted in TFEXT, in sms_fixvel...
                   DW      = VEL(4,N)
                   TFEXT   = TFEXT + RW_SMS(II)*DW
                   VEL(4,N)= (ONE-RW_SMS(II))*VEL(4,N)
                   YC(II) = YC(II) * FAC
                   I=IABS(IBFV(1,N))
                   AXI=1.-AX+AX*X(2,I)
                   K1=3*J-2
                   K2=3*J-1
                   K3=3*J
                   DD = SKEW(K1,ISK)*D(1,I) +
     .                  SKEW(K2,ISK)*D(2,I) +
     .                  SKEW(K3,ISK)*D(3,I)
                   VV = SKEW(K1,ISK)*V(1,I) +
     .                  SKEW(K2,ISK)*V(2,I) +
     .                  SKEW(K3,ISK)*V(3,I)
                   A0 = SKEW(K1,ISK)*A(1,I) +
     .                  SKEW(K2,ISK)*A(2,I) +
     .                  SKEW(K3,ISK)*A(3,I)
                   IF(IBFV(7,N) == 2) YC(II)=(YC(II)-DD)/DT2_DOUBLE
                   IF(IBFV(7,N)>=1) YC(II)=(YC(II)-VV)/DT12
                   AA = YC(II) - A0
                   A(1,I)=A(1,I)+SKEW(K1,ISK)*AA
                   A(2,I)=A(2,I)+SKEW(K2,ISK)*AA
                   A(3,I)=A(3,I)+SKEW(K3,ISK)*AA
                   DW=FOURTH*MS(I)*(YC(II)*DT12+TWO*VV)*AA*AXI*WEIGHT(I)
C DT2_DOUBLE*dw into memory ; if sms on the degree of freedom,
C part of DT2_DOUBLE*dw was already computed and stored in sms_fixvel...
                   TFEXT=TFEXT + DT1*DW
                   VEL(4,N) = VEL(4,N)+DT2_DOUBLE*DW
                 ENDIF
               ENDIF
             ENDDO
           ENDDO
           IF (IRODDL>=0)THEN
             DO K=4,3+3*IRODDL
#include "vectorize.inc"
               DO II=1,IC
                 N = INDEX(II)
                 IFM = IBFV(9,N)
                 IF (IFM > 1) THEN
                   J=IBFV(2,N)
                   IF(J == K)THEN
                     IBFV(5,N) = IPOSC(II)
                     FAC  = VEL(1,N)
                     DW   = VEL(4,N)
                     TFEXT   = TFEXT + RW_SMS(II)*DW
                     VEL(4,N)= (ONE-RW_SMS(II))*VEL(4,N)
                     YC(II) = YC(II) * FAC
                     I=IABS(IBFV(1,N))
                     J = J - 3
                     K1=3*J-2
                     K2=3*J-1
                     K3=3*J
                     VV = XFRAME(K1,IFM)*VR(1,I)
     .                  + XFRAME(K2,IFM)*VR(2,I)
     .                  + XFRAME(K3,IFM)*VR(3,I)
                     VF = XFRAME(K1,IFM)*XFRAME(13,IFM)
     .                  + XFRAME(K2,IFM)*XFRAME(14,IFM)
     .                  + XFRAME(K3,IFM)*XFRAME(15,IFM)
                     A0 = XFRAME(K1,IFM)*AR(1,I)
     .                  + XFRAME(K2,IFM)*AR(2,I)
     .                  + XFRAME(K3,IFM)*AR(3,I)
                     AA = (YC(II)-VV+VF)/DT12 - A0
                     AR(1,I)=AR(1,I)+XFRAME(K1,IFM)*AA
                     AR(2,I)=AR(2,I)+XFRAME(K2,IFM)*AA
                     AR(3,I)=AR(3,I)+XFRAME(K3,IFM)*AA
                     IF(IBFV(6,N) == 0)THEN
                       DW= FOURTH*(YC(II)+VV)*IN(I)*AA*WEIGHT(I)
                     ELSE
                       NR = IBFV(6,N)
                       DW= FOURTH*(YC(II)+VV)*WEIGHT(I)*AA*
     .                    ((RBY(17,NR)*XFRAME(K1,IFM)
     .                     +RBY(18,NR)*XFRAME(K2,IFM)
     .                     +RBY(19,NR)*XFRAME(K3,IFM))*XFRAME(K1,IFM) +
     .                     (RBY(20,NR)*XFRAME(K1,IFM)
     .                     +RBY(21,NR)*XFRAME(K2,IFM)
     .                     +RBY(22,NR)*XFRAME(K3,IFM))*XFRAME(K2,IFM) +
     .                     (RBY(23,NR)*XFRAME(K1,IFM)
     .                     +RBY(24,NR)*XFRAME(K2,IFM)
     .                     +RBY(25,NR)*XFRAME(K3,IFM))*XFRAME(K3,IFM))
                     ENDIF
                     TFEXT=TFEXT + DT1*DW
                     VEL(4,N) = DT2_DOUBLE*DW
                   ENDIF
                 ELSE
                   ISK=IBFV(2,N)/10
                   J=IBFV(2,N)-10*ISK
                   IF(J == K)THEN
                     IBFV(5,N) = IPOSC(II)
                     FAC  = VEL(1,N)
                     DW   = VEL(4,N)
                     TFEXT   = TFEXT + RW_SMS(II)*DW
                     VEL(4,N)= (ONE-RW_SMS(II))*VEL(4,N)
                     YC(II) = YC(II) * FAC
                     I=IABS(IBFV(1,N))
                     J = J - 3
                     K1=3*J-2
                     K2=3*J-1
                     K3=3*J
                     IF(IBFV(7,N) == 2) THEN
                       DD = SKEW(K1,ISK)*DR(1,I) +
     .                      SKEW(K2,ISK)*DR(2,I) +
     .                      SKEW(K3,ISK)*DR(3,I)
                     END IF
                     VV = SKEW(K1,ISK)*VR(1,I) +
     .                    SKEW(K2,ISK)*VR(2,I) +
     .                    SKEW(K3,ISK)*VR(3,I)
                     A0 = SKEW(K1,ISK)*AR(1,I) +
     .                    SKEW(K2,ISK)*AR(2,I) +
     .                    SKEW(K3,ISK)*AR(3,I)
                     IF(IBFV(7,N) == 2) YC(II)=(YC(II)-DD)/DT2_DOUBLE
                     IF(IBFV(7,N)>=1) YC(II)=(YC(II)-VV)/DT12
                     AA = YC(II) - A0
                     AR(1,I)=AR(1,I)+SKEW(K1,ISK)*AA
                     AR(2,I)=AR(2,I)+SKEW(K2,ISK)*AA
                     AR(3,I)=AR(3,I)+SKEW(K3,ISK)*AA
                     IF(IBFV(6,N) == 0)THEN
                       DW=FOURTH*(YC(II)*DT12+TWO*VV)*IN(I)*AA*WEIGHT(I)
                     ELSE
                       NR = IBFV(6,N)
                       DW= FOURTH*(YC(II)*DT12+TWO*VV)*WEIGHT(I)*AA*
     .                    ((RBY(17,NR)*SKEW(K1,ISK)
     .                     +RBY(18,NR)*SKEW(K2,ISK)
     .                     +RBY(19,NR)*SKEW(K3,ISK))*SKEW(K1,ISK) +
     .                     (RBY(20,NR)*SKEW(K1,ISK)
     .                     +RBY(21,NR)*SKEW(K2,ISK)
     .                     +RBY(22,NR)*SKEW(K3,ISK))*SKEW(K2,ISK) +
     .                     (RBY(23,NR)*SKEW(K1,ISK)
     .                     +RBY(24,NR)*SKEW(K2,ISK)
     .                     +RBY(25,NR)*SKEW(K3,ISK))*SKEW(K3,ISK))
                     ENDIF
                     TFEXT=TFEXT + DT1*DW
                     VEL(4,N) = DT2_DOUBLE*DW
                   ENDIF
                 ENDIF
               ENDDO
             ENDDO
           ENDIF
         ELSEIF (NFK == 1.AND.ICOOR == 1) THEN
           DO K=1,3
#include "vectorize.inc"
             DO II=1,IC
               N = INDEX(II)
               IFM = IBFV(9,N)
               IF (IFM > 1) THEN
                 J = IBFV(2,N)
                 FMDIR=ZERO
                 R=ONE
                 IF(J == K)THEN
                   IBFV(5,N) = IPOSC(II)
                   FAC  = VEL(1,N)
C if sms on the degree of freedom,
C dw was already counted in TFEXT, in sms_fixvel...
                   DW      = VEL(4,N)
                   TFEXT   = TFEXT + RW_SMS(II)*DW
                   VEL(4,N)= (ONE-RW_SMS(II))*VEL(4,N)
                   YC(II) = YC(II) * FAC
                   I=IABS(IBFV(1,N))
                   AXI=1.-AX+AX*X(2,I)
                   COEF = (XFRAME(10,IFM)-X(1,I))*XFRAME(7,IFM) +
     &                    (XFRAME(11,IFM)-X(2,I))*XFRAME(8,IFM) +
     &                    (XFRAME(12,IFM)-X(3,I))*XFRAME(9,IFM)
                   HX = COEF*XFRAME(7,IFM)+X(1,I)-XFRAME(10,IFM)
                   HY = COEF*XFRAME(8,IFM)+X(2,I)-XFRAME(11,IFM)
                   HZ = COEF*XFRAME(9,IFM)+X(3,I)-XFRAME(12,IFM)
                   R  = SQRT(HX*HX+HY*HY+HZ*HZ)
                   IF(R<=EM20) THEN
                     CTH = ZERO
                     STH = ZERO
                   ELSE
                     CTH = (XFRAME(1,IFM)*HX +
     &                      XFRAME(2,IFM)*HY +
     &                      XFRAME(3,IFM)*HZ)/R
                     STH = (XFRAME(4,IFM)*HX +
     &                      XFRAME(5,IFM)*HY +
     &                      XFRAME(6,IFM)*HZ)/R
                   ENDIF
                   IF (J == 1) THEN
                     FMDIR(1) =CTH*XFRAME(1,IFM)+ STH*XFRAME(4,IFM)
                     FMDIR(2) =CTH*XFRAME(2,IFM)+ STH*XFRAME(5,IFM)
                     FMDIR(3) =CTH*XFRAME(3,IFM)+ STH*XFRAME(6,IFM)
                   ELSEIF (J == 2) THEN
                     ER(1) =CTH*XFRAME(1,IFM)+ STH*XFRAME(4,IFM)
                     ER(2) =CTH*XFRAME(2,IFM)+ STH*XFRAME(5,IFM)
                     ER(3) =CTH*XFRAME(3,IFM)+ STH*XFRAME(6,IFM)
                     NOR1=SQRT(ER(1)*ER(1)+ER(2)*ER(2)+ER(3)*ER(3))
                     ER=ER/MAX(NOR1,EM20)
                     ET(1) =CTH*XFRAME(4,IFM)- STH*XFRAME(1,IFM)
                     ET(2) =CTH*XFRAME(5,IFM)- STH*XFRAME(2,IFM)
                     ET(3) =CTH*XFRAME(6,IFM)- STH*XFRAME(3,IFM)
                     NOR2=SQRT(ET(1)*ET(1)+ET(2)*ET(2)+ET(3)*ET(3))
                     ET=ET/MAX(NOR2,EM20)
c--------------------vitesse angulaire imposee--------
                     ALPHA=-YC(II)*DT2_DOUBLE/TWO
c-----------------------------------------------
c                    ALPHA=-YC(II)*DT2_DOUBLE/(TWO*R)
                     FMDIR=ALPHA*ER+ET
                   ELSEIF (J == 3) THEN
                     FMDIR(1)=XFRAME(7,IFM)
                     FMDIR(2)=XFRAME(8,IFM)
                     FMDIR(3)=XFRAME(9,IFM)
                   ENDIF
                   NOR3 = SQRT(FMDIR(1)*FMDIR(1)
     &                   +FMDIR(2)*FMDIR(2)
     &                   +FMDIR(3)*FMDIR(3))
                   FMDIR=FMDIR/MAX(NOR3,EM20)
                   RX  = X(1,I) - XFRAME(10,IFM)
                   RY  = X(2,I) - XFRAME(11,IFM)
                   RZ  = X(3,I) - XFRAME(12,IFM)
                   VFX = XFRAME(31,IFM) +
     &                   XFRAME(14,IFM)*RZ - XFRAME(15,IFM)*RY
                   VFY = XFRAME(32,IFM) +
     &                   XFRAME(15,IFM)*RX - XFRAME(13,IFM)*RZ
                   VFZ = XFRAME(33,IFM) +
     &                   XFRAME(13,IFM)*RY-XFRAME(14,IFM)*RX
                   VV = FMDIR(1)*V(1,I) +
     &                  FMDIR(2)*V(2,I) +
     &                  FMDIR(3)*V(3,I)
                   VF = FMDIR(1)*VFX + FMDIR(2)*VFY + FMDIR(3)*VFZ
                   A0 = FMDIR(1)*A(1,I) +
     &                  FMDIR(2)*A(2,I) +
     &                  FMDIR(3)*A(3,I)
                   IF (J == 2) THEN
c                  YC(II)=(YC(II)-VV+VF)/DT12
c------------------vitesse angulaire imposee--------
                     YC(II)=(R*YC(II)-VV+VF)/DT12
c-------------------------------------------------
                   ELSE
                     YC(II)=(YC(II)-VV+VF)/DT12
                   ENDIF
                   AA = YC(II) - A0
                   IF(R<=EM20) AA=ZERO
                   A(1,I)=A(1,I)+FMDIR(1)*AA
                   A(2,I)=A(2,I)+FMDIR(2)*AA
                   A(3,I)=A(3,I)+FMDIR(3)*AA
                   DW=FOURTH*MS(I)*(YC(II)*DT12+TWO*VV)*AA*AXI*WEIGHT(I)
C DT2_DOUBLE*dw into memory ; if sms on the degree of freedom,
C part of DT2_DOUBLE*dw was already computed and stored in sms_fixvel...
                   TFEXT=TFEXT + DT1*DW
                   VEL(4,N) = VEL(4,N)+DT2_DOUBLE*DW
                 ENDIF
               ELSE
                 ISK=IBFV(2,N)/10
                 J=IBFV(2,N)-10*ISK
                 SKDIR=ZERO
                 R=ONE
                 IF(J == K)THEN
                   IBFV(5,N) = IPOSC(II)
                   FAC  = VEL(1,N)
                   YIMP   = VEL(6,N)
                   VEL(6,N) = YC(II)
C if sms on the degree of freedom,
C dw was already counted in TFEXT, in sms_fixvel...
                   DW      = VEL(4,N)
                   TFEXT   = TFEXT + RW_SMS(II)*DW
                   VEL(4,N)= (ONE-RW_SMS(II))*VEL(4,N)
                   YC(II) = YC(II) * FAC
                   YIMP = YIMP * FAC
                   I=IABS(IBFV(1,N))
                   AXI=1.-AX+AX*X(2,I)
                   COEF = (SKEW(10,ISK)-X(1,I))*SKEW(7,ISK) +
     &                    (SKEW(11,ISK)-X(2,I))*SKEW(8,ISK) +
     &                    (SKEW(12,ISK)-X(3,I))*SKEW(9,ISK)
                   HX = COEF*SKEW(7,ISK)+X(1,I)-SKEW(10,ISK)
                   HY = COEF*SKEW(8,ISK)+X(2,I)-SKEW(11,ISK)
                   HZ = COEF*SKEW(9,ISK)+X(3,I)-SKEW(12,ISK)
                   R  = SQRT(HX*HX+HY*HY+HZ*HZ)
                   IF(R<=EM20) THEN
                     CTH = ZERO
                     STH = ZERO
                   ELSE
                     CTH = (SKEW(1,ISK)*HX +
     &                      SKEW(2,ISK)*HY +
     &                      SKEW(3,ISK)*HZ)/R
                     STH = (SKEW(4,ISK)*HX +
     &                      SKEW(5,ISK)*HY +
     &                      SKEW(6,ISK)*HZ)/R
                   ENDIF
                   IF (J == 1) THEN
                     SKDIR(1) =CTH*SKEW(1,ISK)+ STH*SKEW(4,ISK)
                     SKDIR(2) =CTH*SKEW(2,ISK)+ STH*SKEW(5,ISK)
                     SKDIR(3) =CTH*SKEW(3,ISK)+ STH*SKEW(6,ISK)
                   ELSEIF(J == 2) THEN
                     ER(1) =CTH*SKEW(1,ISK)+ STH*SKEW(4,ISK)
                     ER(2) =CTH*SKEW(2,ISK)+ STH*SKEW(5,ISK)
                     ER(3) =CTH*SKEW(3,ISK)+ STH*SKEW(6,ISK)
                     NOR1=SQRT(ER(1)*ER(1)+ER(2)*ER(2)+ER(3)*ER(3))
                     ER=ER/MAX(NOR1,EM20)
                     ET(1) =CTH*SKEW(4,ISK)- STH*SKEW(1,ISK)
                     ET(2) =CTH*SKEW(5,ISK)- STH*SKEW(2,ISK)
                     ET(3) =CTH*SKEW(6,ISK)- STH*SKEW(3,ISK)
                     NOR2=SQRT(ET(1)*ET(1)+ET(2)*ET(2)+ET(3)*ET(3))
                     ET=ET/MAX(NOR2,EM20)
c-------------------vitesse angulaire imposee---------
                     ALPHA=-YC(II)*DT2_DOUBLE/TWO
c-------------------sinon-----------------------------
c                    ALPHA=-YC(II)*DT2_DOUBLE/(TWO*R)
                     SKDIR=ALPHA*ER+ET
                   ELSEIF(J == 3) THEN
                     SKDIR(1)=SKEW(7,ISK)
                     SKDIR(2)=SKEW(8,ISK)
                     SKDIR(3)=SKEW(9,ISK)
                     IF(IBFV(7,N) == 2) YIMP= SKDIR(1)*D(1,I) +
     &                                        SKDIR(2)*D(2,I) +
     &                                        SKDIR(3)*D(3,I)
                   ENDIF
                   NOR3 = SQRT(SKDIR(1)*SKDIR(1)
     &                   +SKDIR(2)*SKDIR(2)
     &                   +SKDIR(3)*SKDIR(3))
                   SKDIR=SKDIR/MAX(NOR3,EM20)
                   VV = V(1,I)*SKDIR(1) +
     &                  V(2,I)*SKDIR(2) +
     &                  V(3,I)*SKDIR(3)
                   A0 = A(1,I)*SKDIR(1) +
     &                  A(2,I)*SKDIR(2) +
     &                  A(3,I)*SKDIR(3)
                   IF(IBFV(7,N) == 2) YC(II)=(YC(II)-YIMP)/DT2_DOUBLE
                   IF(IBFV(7,N)>=1) THEN
                     IF (J == 2) THEN
c                    YC(II)=(YC(II)-VV)/DT12
c--------------------vitesse angulaire imposee--------
                       YC(II)=(R*YC(II)-VV)/DT12
c------------------------------------------------------
                     ELSE
                       YC(II)=(YC(II)-VV)/DT12
                     ENDIF
                   ENDIF
                   AA = YC(II) - A0
                   IF(R<=EM20) AA=ZERO
                   A(1,I)=A(1,I)+SKDIR(1)*AA
                   A(2,I)=A(2,I)+SKDIR(2)*AA
                   A(3,I)=A(3,I)+SKDIR(3)*AA
                   DW = FOURTH*MS(I)*(YC(II)*DT12+TWO*VV)
     &                  *AA*AXI*WEIGHT(I)
C DT2_DOUBLE*dw into memory ; if sms on the degree of freedom,
C part of DT2_DOUBLE*dw was already computed and stored in sms_fixvel...
                   TFEXT=TFEXT + DT1*DW
                   VEL(4,N) = VEL(4,N)+DT2_DOUBLE*DW
                 ENDIF
               ENDIF
             ENDDO
           ENDDO
           IF (IRODDL>=0)THEN
             DO K=4,3+3*IRODDL
#include "vectorize.inc"
               DO II=1,IC
                 N = INDEX(II)
                 IFM = IBFV(9,N)
                 IF (IFM > 1) THEN
                   J=IBFV(2,N)
                   FMDIR=ZERO
                   R=ONE
                   IF(J == K)THEN
                     IBFV(5,N) = IPOSC(II)
                     FAC  = VEL(1,N)
                     DW   = VEL(4,N)
                     TFEXT   = TFEXT + RW_SMS(II)*DW
                     VEL(4,N)= (ONE-RW_SMS(II))*VEL(4,N)
                     YC(II) = YC(II) * FAC
                     I=IABS(IBFV(1,N))
                     J = J - 3
                     COEF = (XFRAME(10,IFM)-X(1,I))*XFRAME(7,IFM) +
     &                      (XFRAME(11,IFM)-X(2,I))*XFRAME(8,IFM) +
     &                      (XFRAME(12,IFM)-X(3,I))*XFRAME(9,IFM)
                     HX = COEF*XFRAME(7,IFM)+X(1,I)-XFRAME(10,IFM)
                     HY = COEF*XFRAME(8,IFM)+X(2,I)-XFRAME(11,IFM)
                     HZ = COEF*XFRAME(9,IFM)+X(3,I)-XFRAME(12,IFM)
                     R  = SQRT(HX*HX+HY*HY+HZ*HZ)
                     IF(R<=EM20) THEN
                       CTH = ZERO
                       STH = ZERO
                     ELSE
                       CTH = (XFRAME(1,IFM)*HX +
     &                        XFRAME(2,IFM)*HY +
     &                        XFRAME(3,IFM)*HZ)/R
                       STH = (XFRAME(4,IFM)*HX +
     &                        XFRAME(5,IFM)*HY +
     &                        XFRAME(6,IFM)*HZ)/R
                     ENDIF
                     IF (J == 1) THEN
                       FMDIR(1) =CTH*XFRAME(1,IFM)+ STH*XFRAME(4,IFM)
                       FMDIR(2) =CTH*XFRAME(2,IFM)+ STH*XFRAME(5,IFM)
                       FMDIR(3) =CTH*XFRAME(3,IFM)+ STH*XFRAME(6,IFM)
                     ELSEIF (J == 2) THEN
                       FMDIR(1) =CTH*XFRAME(4,IFM)- STH*XFRAME(1,IFM)
                       FMDIR(2) =CTH*XFRAME(5,IFM)- STH*XFRAME(2,IFM)
                       FMDIR(3) =CTH*XFRAME(6,IFM)- STH*XFRAME(3,IFM)
                     ELSEIF (J == 3) THEN
                       FMDIR(1)=XFRAME(7,IFM)
                       FMDIR(2)=XFRAME(8,IFM)
                       FMDIR(3)=XFRAME(9,IFM)
                     ENDIF
                     NOR3 = SQRT(FMDIR(1)*FMDIR(1)
     &                          +FMDIR(2)*FMDIR(2)
     &                          +FMDIR(3)*FMDIR(3))
                     FMDIR=FMDIR/MAX(NOR3,EM20)
                     VV = FMDIR(1)*VR(1,I) +
     &                    FMDIR(2)*VR(2,I) +
     &                    FMDIR(3)*VR(3,I)
                     VF = FMDIR(1)*XFRAME(13,IFM) +
     &                    FMDIR(2)*XFRAME(14,IFM) +
     &                    FMDIR(3)*XFRAME(15,IFM)
                     A0 = FMDIR(1)*AR(1,I) +
     &                    FMDIR(2)*AR(2,I) +
     &                    FMDIR(3)*AR(3,I)
                     AA = (YC(II)-VV+VF)/DT12 - A0
                     IF(R<=EM20) AA=ZERO
                     AR(1,I)=AR(1,I)+FMDIR(1)*AA
                     AR(2,I)=AR(2,I)+FMDIR(2)*AA
                     AR(3,I)=AR(3,I)+FMDIR(3)*AA
                     IF(IBFV(6,N) == 0)THEN
                       DW= FOURTH*(YC(II)+VV)*IN(I)*AA*WEIGHT(I)
                     ELSE
                       NR = IBFV(6,N)
                       DW= FOURTH*(YC(II)+VV)*WEIGHT(I)*AA*
     &                     ((RBY(17,NR)*FMDIR(1)
     &                     +RBY(18,NR)*FMDIR(2)
     &                     +RBY(19,NR)*FMDIR(3))*FMDIR(1) +
     &                     (RBY(20,NR)*FMDIR(1)
     &                     +RBY(21,NR)*FMDIR(2)
     &                     +RBY(22,NR)*FMDIR(3))*FMDIR(2) +
     &                     (RBY(23,NR)*FMDIR(1)
     &                     +RBY(24,NR)*FMDIR(2)
     &                     +RBY(25,NR)*FMDIR(3))*FMDIR(3))
                     ENDIF
                     TFEXT=TFEXT + DT1*DW
                     VEL(4,N) = DT2_DOUBLE*DW
                   ENDIF
                 ELSE
                   ISK=IBFV(2,N)/10
                   J=IBFV(2,N)-10*ISK
                   SKDIR=ZERO
                   R=ONE
                   IF(J == K)THEN
                     IBFV(5,N) = IPOSC(II)
                     FAC  = VEL(1,N)
                     YIMP   = VEL(6,N)
                     VEL(6,N) = YC(II)
                     DW   = VEL(4,N)
                     TFEXT   = TFEXT + RW_SMS(II)*DW
                     VEL(4,N)= (ONE-RW_SMS(II))*VEL(4,N)
                     YC(II) = YC(II) * FAC
                     YIMP = YIMP * FAC
                     I=IABS(IBFV(1,N))
                     J = J - 3
                     COEF = (SKEW(10,ISK)-X(1,I))*SKEW(7,ISK) +
     &                      (SKEW(11,ISK)-X(2,I))*SKEW(8,ISK) +
     &                      (SKEW(12,ISK)-X(3,I))*SKEW(9,ISK)
                     HX = COEF*SKEW(7,ISK)+X(1,I)-SKEW(10,ISK)
                     HY = COEF*SKEW(8,ISK)+X(2,I)-SKEW(11,ISK)
                     HZ = COEF*SKEW(9,ISK)+X(3,I)-SKEW(12,ISK)
                     R  = SQRT(HX*HX+HY*HY+HZ*HZ)
                     IF(R<=EM20) THEN
                       CTH = ZERO
                       STH = ZERO
                     ELSE
                       CTH = (SKEW(1,ISK)*HX +
     &                        SKEW(2,ISK)*HY +
     &                        SKEW(3,ISK)*HZ)/R
                       STH = (SKEW(4,ISK)*HX +
     &                        SKEW(5,ISK)*HY +
     &                        SKEW(6,ISK)*HZ)/R
                     ENDIF
                     IF (J == 1) THEN
                       SKDIR(1) =CTH*SKEW(1,ISK)+ STH*SKEW(4,ISK)
                       SKDIR(2) =CTH*SKEW(2,ISK)+ STH*SKEW(5,ISK)
                       SKDIR(3) =CTH*SKEW(3,ISK)+ STH*SKEW(6,ISK)
                     ELSEIF(J == 2) THEN
                       SKDIR(1) =CTH*SKEW(4,ISK)- STH*SKEW(1,ISK)
                       SKDIR(2) =CTH*SKEW(5,ISK)- STH*SKEW(2,ISK)
                       SKDIR(3) =CTH*SKEW(6,ISK)- STH*SKEW(3,ISK)
                     ELSEIF(J == 3) THEN
                       SKDIR(1)=SKEW(7,ISK)
                       SKDIR(2)=SKEW(8,ISK)
                       SKDIR(3)=SKEW(9,ISK)
                       IF(IBFV(7,N) == 2) YIMP= SKDIR(1)*DR(1,I) +
     &                                          SKDIR(2)*DR(2,I) +
     &                                          SKDIR(3)*DR(3,I)
                      ENDIF
                      NOR3 = SQRT(SKDIR(1)*SKDIR(1)
     &                           +SKDIR(2)*SKDIR(2)
     &                           +SKDIR(3)*SKDIR(3))
                      SKDIR=SKDIR/MAX(NOR3,EM20)

                      IF(IBFV(7,N) == 2) YC(II)=(YC(II)-YIMP)/DT2_DOUBLE
                      VV = VR(1,I)*SKDIR(1) +
     &                     VR(2,I)*SKDIR(2) +
     &                     VR(3,I)*SKDIR(3)
                      A0 = AR(1,I)*SKDIR(1) +
     &                     AR(2,I)*SKDIR(2) +
     &                     AR(3,I)*SKDIR(3)
                      IF(IBFV(7,N)>=1) YC(II)=(YC(II)-VV)/DT12
                      AA = YC(II) - A0
                      IF(R<=EM20) AA=ZERO
                      AR(1,I)=AR(1,I)+SKDIR(1)*AA
                      AR(2,I)=AR(2,I)+SKDIR(2)*AA
                      AR(3,I)=AR(3,I)+SKDIR(3)*AA
                      IF(IBFV(6,N) == 0)THEN
                        DW= FOURTH*(YC(II)*DT12+TWO*VV)*IN(I)*
     &                      AA*WEIGHT(I)
                      ELSE
                        NR = IBFV(6,N)
                        DW= FOURTH*(YC(II)*DT12+TWO*VV)*WEIGHT(I)*AA*
     &                      ((RBY(17,NR)*SKDIR(1)
     &                      +RBY(18,NR)*SKDIR(2)
     &                      +RBY(19,NR)*SKDIR(3))*SKDIR(1) +
     &                      (RBY(20,NR)*SKDIR(1)
     &                      +RBY(21,NR)*SKDIR(2)
     &                      +RBY(22,NR)*SKDIR(3))*SKDIR(2) +
     &                      (RBY(23,NR)*SKDIR(1)
     &                      +RBY(24,NR)*SKDIR(2)
     &                      +RBY(25,NR)*SKDIR(3))*SKDIR(3))
                      ENDIF
                      TFEXT=TFEXT + DT1*DW
                      VEL(4,N) = DT2_DOUBLE*DW
                    ENDIF
                  ENDIF
                ENDDO
              ENDDO
            ENDIF
          ENDIF
        ENDIF
      ENDDO  !  NN=1,NFXVEL
c-----------
      RETURN
      END
