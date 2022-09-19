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
Chd|  M5IN3                         source/initial_conditions/detonation/m5in3.F
Chd|-- called by -----------
Chd|        M105INIT                      source/materials/mat/mat105/m105init.F
Chd|        M51INIT                       source/materials/mat/mat051/m51init.F
Chd|        M97INIT                       source/materials/mat/mat097/m97init.F
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|        MULTIFLUID_INIT3              source/multifluid/multifluid_init3.F
Chd|        MULTIFLUID_INIT3T             source/multifluid/multifluid_init3t.F
Chd|-- calls ---------------
Chd|        DETCORD                       source/initial_conditions/detonation/detcord.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE M5IN3(PM,MAT,M151_ID,DETONATORS,TB,NGL,IPARG,X,IX,NIX)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE DETONATORS_MOD
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
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
#include      "com04_c.inc"
#include      "scr11_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NGL(*),MAT(*),IPARG(NPARG),NIX,IX(NIX,*)
      INTEGER,INTENT(IN) :: M151_ID
      my_real :: PM(NPROPM,*),  TB(*), X(3, *)
      TYPE(DETONATOR_STRUCT_)::DETONATORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, MTL, MPR, NEL,NDET3D, JJ(MVSIZ),
     .        N2, NP2, MT, IOPT, NPE2,
     .        NDETPS, NDETSG, NECRAN, NDETPL, NDETCORD
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   X5(MVSIZ), X6(MVSIZ), X7(MVSIZ), X8(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Y5(MVSIZ), Y6(MVSIZ), Y7(MVSIZ), Y8(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   Z5(MVSIZ), Z6(MVSIZ), Z7(MVSIZ), Z8(MVSIZ),
     .   XC(MVSIZ), YC(MVSIZ), ZC(MVSIZ),
     .   BT(MVSIZ), DL(MVSIZ),
     .   ALT , XLP,  YLP, ZLP, XLP1, YLP1, ZLP1,
     .   XLP2,YLP2, ZLP2, XL0,  YL0,  ZL0,  XL1,
     .   YL1 , ZL1,  XL2, YL2,  ZL2,  PS1,  PS2,
     .   DL1, DL2, S1, S2, S3, DETO,
     .   NX,    NY,    NZ,    NN, VDET,VDET2
      INTEGER :: NODE1, NODE2, NODE3, NODE4, NODE5, NODE6, NODE7, NODE8, II
C=======================================================================
C
      NDET3D=4 !Types of detonator for 3D analysis (point/line/plan/cord)

      NDETPS = DETONATORS%NDETPS
      NDETSG = DETONATORS%NDETSG
      NECRAN = DETONATORS%NECRAN
      NDETPL = DETONATORS%NDETPL
      NDETCORD = DETONATORS%NDETCORD

                                        !------------------------------------!
      IF(DETONATORS%NDET == 0) THEN     !-> TDET=0 IF NO DETONATOR DEFINED   !
                                        !------------------------------------!
         DO I=LFT,LLT
           TB(I)=ZERO
         END DO

           !-----------------------------------------------!
      ELSE !-> LIGHTING TIME CALCULATION                   !
           !-----------------------------------------------!

         IF (JSPH == 1) THEN
C     SPH -> only X1, Y1, Z1, have to be used
            DO I = LFT, LLT
C     Getting rid of the equivalences
               II = I + NFT
               NODE1 = IX(3, II)
               X1(I) = X(1, NODE1)
               X2(I) = ZERO
               X3(I) = ZERO
               X4(I) = ZERO
               X5(I) = ZERO
               X6(I) = ZERO
               X7(I) = ZERO
               X8(I) = ZERO
               Y1(I) = X(2, NODE1)
               Y2(I) =  ZERO
               Y3(I) = ZERO
               Y4(I) = ZERO
               Y5(I) = ZERO
               Y6(I) = ZERO
               Y7(I) = ZERO
               Y8(I) =  ZERO
               Z1(I) = X(3, NODE1)
               Z2(I) = ZERO
               Z3(I) = ZERO
               Z4(I) = ZERO
               Z5(I) = ZERO
               Z6(I) = ZERO
               Z7(I) = ZERO
               Z8(I) = ZERO
            ENDDO
         ELSE
            DO I = LFT, LLT
C     Getting rid of the equivalences
               II = I + NFT
               NODE1 = IX(2, II)
               NODE2 = IX(3, II)
               NODE3 = IX(4, II)
               NODE4 = IX(5, II)
               NODE5 = IX(6, II)
               NODE6 = IX(7, II)
               NODE7 = IX(8, II)
               NODE8 = IX(9, II)
               X1(I) = X(1, NODE1)
               X2(I) = X(1, NODE2)
               X3(I) = X(1, NODE3)
               X4(I) = X(1, NODE4)
               X5(I) = X(1, NODE5)
               X6(I) = X(1, NODE6)
               X7(I) = X(1, NODE7)
               X8(I) = X(1, NODE8)
               Y1(I) = X(2, NODE1)
               Y2(I) = X(2, NODE2)
               Y3(I) = X(2, NODE3)
               Y4(I) = X(2, NODE4)
               Y5(I) = X(2, NODE5)
               Y6(I) = X(2, NODE6)
               Y7(I) = X(2, NODE7)
               Y8(I) = X(2, NODE8)
               Z1(I) = X(3, NODE1)
               Z2(I) = X(3, NODE2)
               Z3(I) = X(3, NODE3)
               Z4(I) = X(3, NODE4)
               Z5(I) = X(3, NODE5)
               Z6(I) = X(3, NODE6)
               Z7(I) = X(3, NODE7)
               Z8(I) = X(3, NODE8)
            ENDDO
         ENDIF

        !--------------------------------------------------!
        ! INITIALIZATION BEFORE LIGHTING TIME COMPUTATION  !
        !--------------------------------------------------!
        DO I=LFT,LLT
              TB(I)=EP37
              JJ(I)=NDET3D ! will be removed -1 for each detonator associated to this elem
          IF(JSPH == 0)THEN
            IF(IPARG(28) == 4) THEN
              XC(I)=FOURTH*(X1(I)+X2(I)+X3(I)+X4(I))
              YC(I)=FOURTH*(Y1(I)+Y2(I)+Y3(I)+Y4(I))
              ZC(I)=FOURTH*(Z1(I)+Z2(I)+Z3(I)+Z4(I))
            ELSE
              XC(I)=ONE_OVER_8*(X1(I)+X2(I)+X3(I)+X4(I)+X5(I)+X6(I)+X7(I)+X8(I))
              YC(I)=ONE_OVER_8*(Y1(I)+Y2(I)+Y3(I)+Y4(I)+Y5(I)+Y6(I)+Y7(I)+Y8(I))
              ZC(I)=ONE_OVER_8*(Z1(I)+Z2(I)+Z3(I)+Z4(I)+Z5(I)+Z6(I)+Z7(I)+Z8(I))
            ENDIF
          ELSE
            XC(I)=X1(I)
            YC(I)=Y1(I)
            ZC(I)=Z1(I)
          ENDIF
        END DO

        !---------------------------------!
        ! LIGHTING TIME FOR /DFS/DETPOIN  !
        !---------------------------------!
        IF(NDETPS /= 0) THEN
          DO I=LFT,LLT
              DO N=1,NDETPS
                MTL=DETONATORS%BURN(2,N)
                IF(MTL == 0 .OR. MTL == MAT(I) .OR. MTL == M151_ID) THEN
                  ALT=DETONATORS%BURN(1,N)
                  XLP=DETONATORS%BURN(3,N)
                  YLP=DETONATORS%BURN(4,N)
                  ZLP=DETONATORS%BURN(5,N)
                  DL(I) =(XC(I)-XLP)**2+(YC(I)-YLP)**2+(ZC(I)-ZLP)**2
                  DL(I)=SQRT(DL(I))
                  BT(I) =ALT+DL(I)/PM(38,MAT(I))
                  IF(BT(I) < ABS(TB(I))) TB(I)=-BT(I)
                    JJ(I)=JJ(I)-1
                  END IF
                END DO
              END DO
        ENDIF

        !---------------------------------!
        ! LIGHTING TIME FOR /DFS/DETLINE  !
        !---------------------------------!
        IF(NDETSG /= 0) THEN
          DO N=NDETPS+1,NDETPS+NDETSG
          ALT=DETONATORS%BURN(1,N)
          MTL=DETONATORS%BURN(2,N)
          XLP1=DETONATORS%BURN(3,N)
          YLP1=DETONATORS%BURN(4,N)
          ZLP1=DETONATORS%BURN(5,N)
          XLP2=DETONATORS%BURN(6,N)
          YLP2=DETONATORS%BURN(7,N)
          ZLP2=DETONATORS%BURN(8,N)
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
                 JJ(I)=JJ(I)-1
              END IF
           END DO
          END DO
        ENDIF

        !---------------------------------!
        ! LIGHTING TIME FOR /DFS/DETPLAN  !
        !---------------------------------!
        IF(NDETPL /= 0) THEN
          DO N=NDETPS+NDETSG+1,NDETPS+NDETSG+NDETPL
          ALT=DETONATORS%BURN(1,N)
          MTL=NINT(DETONATORS%BURN(2,N))
          XLP=DETONATORS%BURN(3,N)
          YLP=DETONATORS%BURN(4,N)
          ZLP=DETONATORS%BURN(5,N)
          NX=DETONATORS%BURN(6,N)
          NY=DETONATORS%BURN(7,N)
          NZ=DETONATORS%BURN(8,N)
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
            DL(I) = (XC(I)-XLP)*NX + (YC(I)-YLP)*NY +
     .                (ZC(I)-ZLP)*NZ
              DL(I) = ABS(DL(I))
              DL(I) = DL(I)/NN
              BT(I) =ALT+DL(I)/PM(38,MAT(I))
              IF(BT(I) < ABS(TB(I))) TB(I)=-BT(I)
                JJ(I)=JJ(I)-1
              END IF
              END DO
          END DO
        ENDIF

        !---------------------------------!
        ! LIGHTING TIME FOR /DFS/DETCORD  !
        !---------------------------------!
        VDET = ZERO
        IF(NDETCORD /= 0) THEN
          N2        = 1
          DO N=NDETPS+NDETSG+NECRAN+NDETPL+1,NDETPS+NDETSG+NECRAN+NDETPL+NDETCORD
            ALT     = DETONATORS%BURN(1,N)
            MTL     = NINT(DETONATORS%BURN(2,N))
            VDET2   = DETONATORS%BURN(3,N)
            IOPT    = NINT(DETONATORS%BURN(4,N))
            NPE2    = NINT(DETONATORS%BURN(6,N))
            NP2     = NINT(DETONATORS%BURN(7,N))
            DTO0    = ALT
            VDTO=PM(38,MAT(1))
            IF(VDET == ZERO)VDET=PM(38,MAT(1)) !optional detonation velocity
            IF(MTL /= MAT(1) .AND. MTL /= 0 .AND. MTL /= M151_ID) CYCLE
            DTO0    = ALT
            CALL DETCORD(DETONATORS,N2,NPE2,X, MTL, XC,YC,ZC,VDTO,VDET2,ALT,BT,TB,JJ,IOPT,IX,NIX)
            N2      = N2+NPE2
          END DO! next N
        ENDIF

             !--------------------------------------!
      END IF !-> LIGHTING TIME COMPUTATION COMPLETE !
             !--------------------------------------!

      !---------------------------------!
      !    ELEM WITHOUT DETONATOR       !
      !---------------------------------!
      IF(DETONATORS%NDET > 0)THEN
        DO I=LFT,LLT
          IF (JJ(I)==NDET3D) TB(I)=ZERO
        END DO
      ENDIF

      !---------------------------------!
      !            PRINTOUT             !
      !---------------------------------!
      IF(IPRI >= 3)THEN
        MPR =0
        DO I=LFT,LLT
          MPR =MPR+1
          NEL =NGL(I)
          IF(MPR == 1) WRITE(IOUT,500)
          DETO=-(TB(I))
          WRITE(IOUT,510) NEL,DETO
          IF(MPR == 50) MPR=0
        END DO
      ENDIF
C-----------------------------------------------
 500  FORMAT(//,
     & 5X,40HDETONATION TIMES FOR JWL ELEMENTS       /,
     & 5X,40H---------------------------------       //,
     & 5X,40HELEMENT  DETONATION TIME                /)
  510 FORMAT(5X,I10,E15.5)
C-----------------------------------------------
      RETURN
      END

