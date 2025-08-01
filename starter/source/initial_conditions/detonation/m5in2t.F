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
!||    m5in2t              ../starter/source/initial_conditions/detonation/m5in2t.F
!||--- called by ------------------------------------------------------
!||    multifluid_init2t   ../starter/source/multifluid/multifluid_init2t.F
!||--- calls      -----------------------------------------------------
!||    detcord             ../starter/source/initial_conditions/detonation/detcord.F
!||    ecran1              ../starter/source/initial_conditions/detonation/ecran1.F
!||    ecran2              ../starter/source/initial_conditions/detonation/ecran2.F
!||--- uses       -----------------------------------------------------
!||    detonators_mod      ../starter/share/modules1/detonators_mod.F
!||    message_mod         ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE M5IN2T(PM,MAT,M151_ID,DETONATORS,TB,X,IX,NIX)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE DETONATORS_MOD
      USE CONSTANT_MOD , ONLY : EP21
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
#include      "units_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "scr03_c.inc"
#include      "scr11_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(*),NIX,IX(NIX,*)
      INTEGER,INTENT(IN) :: M151_ID
      my_real PM(NPROPM,NUMMAT), TB(*),X(3,*),VDET2
      TYPE(DETONATORS_STRUCT_)::DETONATORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, MTL, MT,IOPT,NPE
      INTEGER NDETPS, NDETSG, NECRAN, NDETPL, NDETCORD,NDETPS_NO_SHADOW,NDETPS_SHADOW
      INTEGER :: I_SHADOW_FLAG
      LOGICAL HAS_DETONATOR(MVSIZ)
      my_real Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ),
     .        Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ),
     .        XC(MVSIZ), YC(MVSIZ), ZC(MVSIZ), BT(MVSIZ),
     .        DL(MVSIZ), ALT, XLP, YLP, ZLP, XLP1,
     .        YLP1,  ZLP1, XLP2, YLP2, ZLP2, XL0, YL0, ZL0, XL1, YL1, ZL1,
     .        XL2,   YL2,  ZL2, PS1, PS2, DL1, DL2, S1, S2, S3,
     .        NX,    NY,   NZ , NN, VDET
      INTEGER :: NODE1, NODE2, NODE3, II, GRNOD_ID, INOD, NNOD, NODE_ID
C-----------------------------------------------
C=======================================================================
C
      NDETPS = DETONATORS%N_DET_POINT
      NDETSG = DETONATORS%N_DET_LINE
      NECRAN = DETONATORS%N_DET_WAVE_SHAPER
      NDETPL = DETONATORS%N_DET_PLANE
      NDETCORD = DETONATORS%N_DET_CORD

      ! numbering /DFS/DETPOINT without shadowing option (then arrival time deduced from radial distance)
      NDETPS_NO_SHADOW = 0
      NDETPS_SHADOW = 0
      DO I = 1, DETONATORS%N_DET_POINT
        I_SHADOW_FLAG = DETONATORS%POINT(I)%SHADOW
        IF(I_SHADOW_FLAG == 0)THEN
          NDETPS_NO_SHADOW = NDETPS_NO_SHADOW + 1
        ELSE
          NDETPS_SHADOW = NDETPS_SHADOW + 1
        ENDIF
      ENDDO

!------------------------------------!
      !treat only detonators without shadowing option
      IF(DETONATORS%N_DET - NDETPS_SHADOW > 0) THEN

         DO I = LFT, LLT
            II = I + NFT
            NODE1 = IX(2, II)
            NODE2 = IX(3, II)
            NODE3 = IX(4, II)
            Y1(I) = X(2, NODE1)
            Y2(I) = X(2, NODE2)
            Y3(I) = X(2, NODE3)
            Z1(I) = X(3, NODE1)
            Z2(I) = X(3, NODE2)
            Z3(I) = X(3, NODE3)
         ENDDO

        !--------------------------------------------------!
        ! INITIALIZATION BEFORE LIGHTING TIME COMPUTATION  !
        !--------------------------------------------------!
         DO I=LFT,LLT
          TB(I)=EP21
          HAS_DETONATOR(I)=.FALSE. ! will be removed -1 for each detonator associated to this elem
          XC(I)=ZERO
          YC(I)=THIRD*(Y1(I)+Y2(I)+Y3(I))
          ZC(I)=THIRD*(Z1(I)+Z2(I)+Z3(I))
         END DO

        !---------------------------------!
        ! LIGHTING TIME FOR /DFS/DETPOINT !
        !---------------------------------!
        IF(NDETPS > 0) THEN
          DO I=LFT,LLT
            DO N=1,NDETPS
              I_SHADOW_FLAG = DETONATORS%POINT(N)%SHADOW
              IF(I_SHADOW_FLAG /= 0)CYCLE ! solve Eikonal equation instead
              MTL=DETONATORS%POINT(N)%MAT
              GRNOD_ID=DETONATORS%POINT(N)%GRNOD_ID
              IF(MTL == 0 .OR. MTL == MAT(I) .OR. MTL == M151_ID) THEN
                !-- SINGLE NODE ---!
                IF(GRNOD_ID == 0)THEN
                  ALT=DETONATORS%POINT(N)%TDET
                  XLP=DETONATORS%POINT(N)%XDET
                  YLP=DETONATORS%POINT(N)%YDET
                  ZLP=DETONATORS%POINT(N)%ZDET
                  DL(I) =(XC(I)-XLP)**2+(YC(I)-YLP)**2+(ZC(I)-ZLP)**2
                  DL(I)=SQRT(DL(I))
                  BT(I) =ALT+DL(I)/PM(38,MAT(I))
                  IF(BT(I) < ABS(TB(I))) TB(I)=-BT(I)
                  HAS_DETONATOR(I)= .TRUE.
                !--- GROUP OF NODE ---!
                ELSE
                  NNOD = DETONATORS%POINT(N)%NNOD
                  ALT=DETONATORS%POINT(N)%TDET
                  HAS_DETONATOR(I)=.TRUE.
                  DO INOD=1,NNOD
                    NODE_ID=DETONATORS%POINT(N)%NODLIST(INOD)
                    XLP=X(1,NODE_ID)
                    YLP=X(2,NODE_ID)
                    ZLP=X(3,NODE_ID)
                    DL(I) =(XC(I)-XLP)**2+(YC(I)-YLP)**2+(ZC(I)-ZLP)**2
                    DL(I)=SQRT(DL(I))
                    BT(I) =ALT+DL(I)/PM(38,MAT(I))
                    IF(BT(I) < ABS(TB(I))) TB(I)=-BT(I)
                  ENDDO !next INOD
                ENDIF
              END IF !(MTL == 0 .OR. MTL == MAT(I) .OR. MTL == M151_ID)
            END DO
          END DO
         ENDIF

        !---------------------------------!
        ! LIGHTING TIME FOR /DFS/DETLINE  !
        !---------------------------------!
        IF(NDETSG > 0) THEN
          DO N=1,NDETSG
          ALT=DETONATORS%LINE(N)%TDET
          MTL=DETONATORS%LINE(N)%MAT
          XLP1=DETONATORS%LINE(N)%XDET_1
          YLP1=DETONATORS%LINE(N)%YDET_1
          ZLP1=DETONATORS%LINE(N)%ZDET_1
          XLP2=DETONATORS%LINE(N)%XDET_2
          YLP2=DETONATORS%LINE(N)%YDET_2
          ZLP2=DETONATORS%LINE(N)%ZDET_2
          DO I=LFT,LLT
            IF(MTL == 0 .OR. MTL == MAT(I) .OR. MTL == M151_ID) THEN
              XL0 =(XLP1-XLP2)
              YL0 =(YLP1-YLP2)
              ZL0 =(ZLP1-ZLP2)
              XL1 =(XC(I)-XLP1)
              YL1 =(YC(I)-YLP1)
              ZL1 =(ZC(I)-ZLP1)
              XL2 =(XC(I)-XLP2)
              YL2 =(YC(I)-YLP2)
              ZL2 =(ZC(I)-ZLP2)
              PS1 =XL1*XL0+YL1*YL0+ZL1*ZL0
              PS2 =XL2*XL0+YL2*YL0+ZL2*ZL0
              IF(PS1*PS2 > ZERO)THEN
                DL1 =SQRT(XL1**2+YL1**2+ZL1**2)
                DL2 =SQRT(XL2**2+YL2**2+ZL2**2)
                DL(I)=MIN(DL1,DL2)
              ELSE
                S1 =YL1*ZL0 - ZL1*YL0
                S2 =ZL1*XL0 - XL1*ZL0
                S3 =XL1*YL0 - YL1*XL0
                DL(I)=SQRT((S1**2+S2**2+S3**2)/(XL0**2+YL0**2+ZL0**2))
              ENDIF
              BT(I) =ALT+DL(I)/PM(38,MAT(I))
              IF(BT(I) < ABS(TB(I))) TB(I)=-BT(I)
                HAS_DETONATOR(I)=.TRUE.
              END IF
           END DO
          END DO
        ENDIF

        !---------------------------------!
        ! LIGHTING TIME FOR /DFS/WAV_SHA  !
        !---------------------------------!
        IF(NECRAN > 0) THEN
          DO N=1,NECRAN
            ALT=DETONATORS%WAVE_SHAPER(N)%TDET
            MTL=DETONATORS%WAVE_SHAPER(N)%MAT
            VDET =DETONATORS%WAVE_SHAPER(N)%VDET
            YD =DETONATORS%WAVE_SHAPER(N)%YDET
            ZD =DETONATORS%WAVE_SHAPER(N)%ZDET
            NPE=DETONATORS%WAVE_SHAPER(N)%NUMNOD
            DTO0=ALT
            VDTO=PM(38,MAT(1))
            IF(VDET == ZERO)VDET=PM(38,MAT(1)) !optional detonation velocity
            !---detonation time for screen points
            CALL ECRAN1(DETONATORS%WAVE_SHAPER(N),X,VDET)
            !---detonation times for elements
            DO I=LFT,LLT
            IF(MTL /= MAT(I) .AND. MTL /= 0 .AND. MTL /= M151_ID) CYCLE
              YD =DETONATORS%WAVE_SHAPER(N)%YDET
              ZD =DETONATORS%WAVE_SHAPER(N)%ZDET
              DTO0=ALT
              YL=YC(I)
              ZL=ZC(I)
              CALL ECRAN2(DETONATORS%WAVE_SHAPER(N),X,VDET)
              BT(I) =DTO
              IF(BT(I) < ABS(TB(I))) TB(I)=-BT(I)
              HAS_DETONATOR(I)= .TRUE.
            END DO
          END DO
        ENDIF

        !---------------------------------!
        ! LIGHTING TIME FOR /DFS/DETPLAN  !
        !---------------------------------!
        IF(NDETPL > 0) THEN
          DO N=1,NDETPL
            ALT=DETONATORS%PLANE(N)%TDET
            MTL=DETONATORS%PLANE(N)%MAT
            XLP=DETONATORS%PLANE(N)%XDET
            YLP=DETONATORS%PLANE(N)%YDET
            ZLP=DETONATORS%PLANE(N)%ZDET
            NX=DETONATORS%PLANE(N)%NX
            NY=DETONATORS%PLANE(N)%NY
            NZ=DETONATORS%PLANE(N)%NZ
            NN=SQRT(NX**2+NY**2+NZ**2)
            NN=MAX(NN,EM20)
            DL1=XLP*NX + YLP*NY + ZLP*NZ
            DL1 = DL1/NN
            DO I=LFT,LLT
              IF(MTL == 0 .OR. MTL == MAT(I) .OR. MTL == M151_ID) THEN
                ! DL = (OC.ON) / ||ON||
               ! C: centroid
               ! P: detonation base
               ! N: detonation vector
                DL(I) = (XC(I)-XLP)*NX + (YC(I)-YLP)*NY + (ZC(I)-ZLP)*NZ
                DL(I) = ABS(DL(I))
                DL(I) = DL(I)/NN
                BT(I) =ALT+DL(I)/PM(38,MAT(I))
                IF(BT(I) < ABS(TB(I))) TB(I)=-BT(I)
                HAS_DETONATOR(I)= .TRUE.
                END IF
            END DO
          END DO
        ENDIF

        !---------------------------------!
        ! LIGHTING TIME FOR /DFS/DETCORD  !
        !---------------------------------!
        IF(NDETCORD > 0) THEN
          DO N=1,NDETCORD
            ALT     = DETONATORS%CORD(N)%TDET
            MTL     = DETONATORS%CORD(N)%MAT
            VDET2   = DETONATORS%CORD(N)%VDET
            IOPT    = DETONATORS%CORD(N)%IOPT
            DTO0    = ALT
            MT      = MTL
            IF(MT == 0)MT=MAT(1)
            VDTO    = PM(38,MT)
            IF(MTL /= MAT(1) .AND. MTL /= 0 .AND. MTL /= M151_ID) CYCLE
            DTO0    = ALT
            CALL DETCORD(DETONATORS%CORD(N),X,XC,YC,ZC,VDTO,VDET2,ALT,BT,TB,HAS_DETONATOR,IOPT)
          END DO! next N
        ENDIF

             !--------------------------------------!
      END IF !-> LIGHTING TIME COMPUTATION COMPLETE !
             !--------------------------------------!

      RETURN
      END
