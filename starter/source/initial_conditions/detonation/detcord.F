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
Chd|  DETCORD                       source/initial_conditions/detonation/detcord.F
Chd|-- called by -----------
Chd|        M5IN2                         source/initial_conditions/detonation/m5in2.F
Chd|        M5IN2T                        source/initial_conditions/detonation/m5in2t.F
Chd|        M5IN3                         source/initial_conditions/detonation/m5in3.F
Chd|-- calls ---------------
Chd|        CR_SPLINE_KNOTS               ../common_source/tools/interpolation/cr_spline_knots.F
Chd|        CR_SPLINE_LENGTH              ../common_source/tools/interpolation/cr_spline_length.F
Chd|        CR_SPLINE_POINT_PROJ          ../common_source/tools/interpolation/cr_spline_point_proj.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|====================================================================
      SUBROUTINE DETCORD(DETONATOR_CORD,X,XC,YC,ZC,VDET,VDET2,ALT,BT,TB,HAS_DETONATOR,IOPT)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
      !IOPT=0  : def=3
      !IOPT=1  : piecewise linear  - multiple segments (experimental / osbslete)
      !IOPT=2  : instantaneous - multiple segments (experimental / obsolete)
      !IOPT=3  : Centripetal-Catmull-Rom SPLINE interpolation + projection along neutral fiber
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DETONATORS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "vect01_c.inc"
#include      "scr11_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IOPT
      my_real :: X(3,*),XC(MVSIZ),YC(MVSIZ),ZC(MVSIZ),BT(MVSIZ),VDET,VDET2,ALT,TB(*)
      TYPE(DETONATOR_CORD_STRUCT_)::DETONATOR_CORD
      LOGICAL HAS_DETONATOR(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: J, I1,I2, II,K, iTdet,  i, NPTS, NSEG,IDIST(MVSIZ),NDETCORD,NNOD
      my_real :: DDMX, DTOS, XLP2, YLP2, XLP1     , ZLP2 ,YLP1,
     .           ZLP1, XL0 , YL0 , ZL0 , XL1      , YL1  ,ZL1 , XL2 ,YL2 ,ZL2,
     .           PS1 , PS2 , DL1 , DL2 , DL(MVSIZ), S1   ,S2  , S3,  TdetC, TP1, TP2, TH, TH1,TH2,
     .           TC1,TC2,TC0,TC,p1p2,dh,ALPHA,KNOTS(4),ZP(3),
     .           ZH1(3),DIST1,T1,ZH2(3),DIST2,T2,DIST(MVSIZ),XX,YY,ZZ,D,LOCAL_PT(4,3),C(3),T,DD,
     .           LEN,LEN1,LEN2

      type SPLINE_PATH
         INTEGER :: NUM_POINTS
         my_real,ALLOCATABLE,DIMENSION(:,:) :: control_point
         my_real,ALLOCATABLE,DIMENSION(:)   :: length
         my_real,ALLOCATABLE,DIMENSION(:)   :: cumulative_length
         my_real,ALLOCATABLE,DIMENSION(:,:) :: knots
      end type SPLINE_PATH

      type(SPLINE_PATH),TARGET :: USER_SPLINE_PATH

      my_real, POINTER, DIMENSION(:,:) :: ptr

C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      NNOD = DETONATOR_CORD%NUMNOD
      DTOS = EP30

      IF(IOPT == 2)THEN
      !INSTANTANEOUS IGNITION OF THE WHOLE CORD
        DO J=1,NNOD-1
          !first node
          II   = DETONATOR_CORD%NODES(J)
          XLP1 = X(1,II)
          YLP1 = X(2,II)
          ZLP1 = X(3,II)
          !second node
          II   = DETONATOR_CORD%NODES(J+1)
          XLP2 = X(1,II)
          YLP2 = X(2,II)
          ZLP2 = X(3,II)
          !
          XL0 = (XLP1-XLP2)
          YL0 = (YLP1-YLP2)
          ZL0 = (ZLP1-ZLP2)
          !loop on elems to compute burning time from line [P1,P2]
          DO I=LFT,LLT
            XL1 = (XC(I)-XLP1)
            YL1 = (YC(I)-YLP1)
            ZL1 = (ZC(I)-ZLP1)
            XL2 = (XC(I)-XLP2)
            YL2 = (YC(I)-YLP2)
            ZL2 = (ZC(I)-ZLP2)
            PS1 = XL1*XL0+YL1*YL0+ZL1*ZL0
            PS2 = XL2*XL0+YL2*YL0+ZL2*ZL0
            IF(PS1*PS2 > ZERO)THEN
              !projection point not on the segment [P1,P2]
              DL1 = SQRT(XL1**2+YL1**2+ZL1**2)
              DL2 = SQRT(XL2**2+YL2**2+ZL2**2)
              DL(I)=MIN(DL1,DL2)
            ELSE
              !projection along the segment [P1,P2]
              S1 = YL1*ZL0 - ZL1*YL0
              S2 = ZL1*XL0 - XL1*ZL0
              S3 = XL1*YL0 - YL1*XL0
              DL(I)=SQRT((S1**2+S2**2+S3**2)/(XL0**2+YL0**2+ZL0**2))
            ENDIF
            BT(I) =ALT+DL(I)/VDET
            IF(BT(I) < ABS(TB(I))) TB(I)=-BT(I)
          END DO !next I
        END DO !J=1,NNOD

      ELSEIF(IOPT==1)THEN
      ! DETONATING CORD HAS ITS OWN DETONATION VELOCITY ALONG THE PATH
        DO J=1,NNOD-1
          !first node
          II   = DETONATOR_CORD%NODES(J)
          XLP1 = X(1,II)
          YLP1 = X(2,II)
          ZLP1 = X(3,II)
          TP1  = DETONATOR_CORD%TDET_PATH(J)
          !second node
          II   = DETONATOR_CORD%NODES(J+1)
          XLP2 = X(1,II)
          YLP2 = X(2,II)
          ZLP2 = X(3,II)
          TP2  = DETONATOR_CORD%TDET_PATH(J+1)
          !loop on elems to compute burning time from line [P1,P2]
          DO I=LFT,LLT
            TdetC = ZERO
            XL0 = (XLP1-XLP2)
            YL0 = (YLP1-YLP2)
            ZL0 = (ZLP1-ZLP2)
            XL1 = (XC(I)-XLP1)
            YL1 = (YC(I)-YLP1)
            ZL1 = (ZC(I)-ZLP1)
            XL2 = (XC(I)-XLP2)
            YL2 = (YC(I)-YLP2)
            ZL2 = (ZC(I)-ZLP2)
            PS1 = XL1*XL0+YL1*YL0+ZL1*ZL0
            PS2 = XL2*XL0+YL2*YL0+ZL2*ZL0
            DL1 = SQRT(XL1**2+YL1**2+ZL1**2)
            DL2 = SQRT(XL2**2+YL2**2+ZL2**2)
            TC1 = TP1 + DL1  /VDET
            TC2 = TP2 + DL2  /VDET
            TC  = MIN(TC1,TC2)
            IF(PS1*PS2 <= ZERO)THEN
              S1    = YL1*ZL0 - ZL1*YL0
              S2    = ZL1*XL0 - XL1*ZL0
              S3    = XL1*YL0 - YL1*XL0
              !
              P1P2  = (XL0**2+YL0**2+ZL0**2)
              DL2   = (S1**2+S2**2+S3**2)/P1P2
              DL(I) = SQRT(DL2)
              P1P2  = SQRT(P1P2)
              DH    = SQRT((XL1**2+YL1**2+ZL1**2)-DL2)  !P1H
              !
              TH1   = TP1 + DH/VDET2
              TH2   = TP2 + (P1P2-DH)/VDET2
              TH    = MIN(TH1,TH2)
              TC0   = TH  + DL(I)/VDET
              TC    = MIN(TC,TC0)
            ENDIF
            BT(I)     = TC
            IF(BT(I) < ABS(TB(I))) TB(I)=-BT(I)
          END DO !next I
        END DO !J=1,NNOD

      ELSEIF(IOPT==3)THEN

        !CATMULL-ROM PARAMETER
        ! ALPHA = 0.0 : uniform
        ! ALPHA = 0.5 : centripetal
        ! ALPHA = 1.0 : chordal
        ALPHA = HALF

        IF(VDET2 == ZERO)VDET2=VDET

        !initialize burning times by following the neutral fiber.
        NPTS = NNOD    !number of point composing the cord path
        NSEG = NNOD-1  !number of segment composing the cord path
        ALLOCATE(USER_SPLINE_PATH%control_point(0:NPTS+1,3))
        ALLOCATE(USER_SPLINE_PATH%length(NPTS-1))
        ALLOCATE(USER_SPLINE_PATH%cumulative_length(NPTS-1))
        ALLOCATE(USER_SPLINE_PATH%knots(NPTS-1,4) )
        !PT(0    ,1:3)= will be defined by energy minimum
        !PT(NPTS+1,1:3)= will be defined by energy minimum

        !===CONTROL POINTS===!
        DO J=1,NNOD
          II   = DETONATOR_CORD%NODES(J)
          XLP1 = X(1,II)
          YLP1 = X(2,II)
          ZLP1 = X(3,II)
          USER_SPLINE_PATH%control_point(J,1:3)=(/XLP1, YLP1, ZLP1/)
        ENDDO

        !===END POINTS===!
        ptr=>USER_SPLINE_PATH%control_point(0:NPTS+1,1:3)
        !ADDING FIRST END POINTS - BENDING ENERGY MINIMUM
        ptr(1+0,1)=0.5*(5.*ptr(1+1,1)-4.*ptr(1+2,1)+ptr(1+3,1))
        ptr(1+0,2)=0.5*(5.*ptr(1+1,2)-4.*ptr(1+2,2)+ptr(1+3,2))
        ptr(1+0,3)=0.5*(5.*ptr(1+1,3)-4.*ptr(1+2,3)+ptr(1+3,3))
        !ADDING LAST END POINTS - BENDING ENERGY MINIMUM
        ptr(1+NPTS+1,1)=0.5*(1.*ptr(1+NPTS-2,1)-4.*ptr(1+NPTS-1,1)+5.*ptr(1+NPTS,1))
        ptr(1+NPTS+1,2)=0.5*(1.*ptr(1+NPTS-2,2)-4.*ptr(1+NPTS-1,2)+5.*ptr(1+NPTS,2))
        ptr(1+NPTS+1,3)=0.5*(1.*ptr(1+NPTS-2,3)-4.*ptr(1+NPTS-1,3)+5.*ptr(1+NPTS,3))

        !DEFINE KNOTS
        DO J=1,NSEG
          CALL CR_SPLINE_KNOTS(PTR(1+J,1:3), KNOTS, ALPHA)
          USER_SPLINE_PATH%knots(J,1:4)=KNOTS(1:4)
        ENDDO

       !COMPUTE DISTANCE FROM CENTROID Z TO CONTROL POINT ptr(1+:)
       !  idist(J) is the shortest point in PT from Z(J)
       DO J=LFT,LLT
         DIST(J)  = 1E30
         IDIST(J) = 0
         DO I=1,NPTS
           XX = XC(J)-PTR(1+I,1)
           YY = YC(J)-PTR(1+I,2)
           ZZ = ZC(J)-PTR(1+I,3)
           DD = SQRT(XX*XX+YY*YY+ZZ*ZZ)
           IF(DD < DIST(J))THEN
             DIST(J)  = DD
             IDIST(J) = I
           ENDIF
         ENDDO
       ENDDO

       !CALCUL DES LONGUEUR DE SPLINE
       DO I=1,NSEG
         LOCAL_PT(1,1:3)=PTR(1+I-1,1:3)
         LOCAL_PT(2,1:3)=PTR(1+I+0,1:3)
         LOCAL_PT(3,1:3)=PTR(1+I+1,1:3)
         LOCAL_PT(4,1:3)=PTR(1+I+2,1:3)
         T=1.0
         CALL CR_SPLINE_LENGTH(LOCAL_PT,ALPHA,T,LEN)
         USER_SPLINE_PATH%length(I) = LEN
         IF(I>1)THEN
           USER_SPLINE_PATH%cumulative_length(I) = USER_SPLINE_PATH%cumulative_length(I-1)  + LEN
         ELSE
           USER_SPLINE_PATH%cumulative_length(1) =  LEN
         ENDIF
         !print *, "spline-i=", I
         !print *, "len =", len
       ENDDO

       !PROJECTION DES POINTS TESTS
       DO J=LFT,LLT
         !point le plus proche : IDIST(J)
         I=IDIST(J)
         I=MIN(I,NSEG)
         K=I
         T=0.5
         LOCAL_PT(1,1:3)=PTR(1+I-1,1:3)
         LOCAL_PT(2,1:3)=PTR(1+I+0,1:3)
         LOCAL_PT(3,1:3)=PTR(1+I+1,1:3)
         LOCAL_PT(4,1:3)=PTR(1+I+2,1:3)
         ZP(1:3) =(/XC(J),YC(J),ZC(J)/)
         CALL CR_SPLINE_POINT_PROJ(LOCAL_PT,ZP(1:3),ALPHA,ZH1,DIST1,T1)
         CALL CR_SPLINE_LENGTH(LOCAL_PT,ALPHA,T1,LEN1)
         C(1:3)=ZH1(1:3)
         T=T1 ! position [0,1]
         LEN=LEN1
         !two adjacent spline on given node IDIST(J), unless for first spline I==1
         IF(I >= 2)THEN
           I=I-1
           LOCAL_PT(1,1:3)=PTR(1+I-1,1:3)
           LOCAL_PT(2,1:3)=PTR(1+I+0,1:3)
           LOCAL_PT(3,1:3)=PTR(1+I+1,1:3)
           LOCAL_PT(4,1:3)=PTR(1+I+2,1:3)
           CALL CR_SPLINE_POINT_PROJ(LOCAL_PT,ZP(1:3),ALPHA,ZH2,DIST2,T2)
           CALL CR_SPLINE_LENGTH(LOCAL_PT,ALPHA,T2,LEN2)
           IF(DIST2<DIST1)THEN
             C(1:3)=ZH2(1:3)
             T=T2
             K=I
             LEN = LEN2
           ENDIF
         ENDIF
         !print *, "point=", ZP(1:3)
         !print *, "    ->", C(1:3)
         !!CALL CR_SPLINE_LENGTH(LOCAL_PT,ALPHA,T,LEN)
         IF(K>1)LEN=LEN+USER_SPLINE_PATH%cumulative_length(K-1)
         BT(J) = DETONATOR_CORD%TDET_PATH(1) + LEN/VDET2
         IF(BT(J) < ABS(TB(J))) TB(J)=-BT(J)
       ENDDO

      ENDIF

      DO I=LFT,LLT
        HAS_DETONATOR(I) = .TRUE.
      ENDDO

      DTO=DTOS

C-----------------------------------------------
      RETURN
      END SUBROUTINE DETCORD
