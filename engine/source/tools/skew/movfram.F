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
Chd|  MOVFRA1                       source/tools/skew/movfram.F   
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|====================================================================
      SUBROUTINE MOVFRA1(XFRAME,IFRAME ,X, V ,A ,AR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, intent(IN) :: IFRAME(LISKN,NUMFRAM+1)
      my_real, intent(INOUT) :: XFRAME(NXFRAME,NUMFRAM+1)
      my_real, intent(IN) :: X(3,NUMNOD), V(3,NUMNOD), A(3,NUMNOD), AR(3,NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, N1, N2, N3, K, IMOV, IDIR
      my_real O(3), P(9), PP1, PP3, PP2, POLD(9),
     .        DR11, DR12, DR13,
     .        DR21, DR22, DR23,
     .        DR31, DR32, DR33,
     .        XPI, CS, R, SN, RS2, DRX, DRY, DRZ, VRX, VRY, VRZ, DTINV
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF(ISPMD == 0) THEN
        XPI=PI
        DO N=1,NUMFRAM
          N1=IFRAME(1,N+1)
          N2=IFRAME(2,N+1)
          N3=IFRAME(3,N+1)
          IMOV=IFRAME(5,N+1)
          IF(N1+N2+N3 == 0) THEN
            ! Fixed Frame
          ELSEIF (N2+N3 == 0) THEN
            ! Node defined frame (moving)
            XFRAME(16,N+1)=AR(1,N1)
            XFRAME(17,N+1)=AR(2,N1)
            XFRAME(18,N+1)=AR(3,N1)
            IF(NXFRAME >= 36)THEN
              XFRAME(28,N+1)=A(1,N1)
              XFRAME(29,N+1)=A(2,N1)
              XFRAME(30,N+1)=A(3,N1)
            ENDIF
          ELSE
            ! Moving frame frame
            ! extract frame orientation wrt global system at time t
            POLD(1)=XFRAME(1,N+1)
            POLD(2)=XFRAME(2,N+1)
            POLD(3)=XFRAME(3,N+1)
            POLD(4)=XFRAME(4,N+1)
            POLD(5)=XFRAME(5,N+1)
            POLD(6)=XFRAME(6,N+1)
            POLD(7)=XFRAME(7,N+1)
            POLD(8)=XFRAME(8,N+1)
            POLD(9)=XFRAME(9,N+1)
            ! position & orientation wrt global system at time t+1
            O(1)=X(1,N1)+DT2*(V(1,N1)+DT12*A(1,N1))
            O(2)=X(2,N1)+DT2*(V(2,N1)+DT12*A(2,N1))
            O(3)=X(3,N1)+DT2*(V(3,N1)+DT12*A(3,N1))
            IF (IMOV == 1) THEN
              IDIR = IFRAME(6,N+1)
              ! calculation of X'(IDIR=1) Y'(IDIR=2) Z'(IDIR=3)
              IF (IDIR == 1) THEN
                P(1)=X(1,N2)+DT2*(V(1,N2)+DT12*A(1,N2))-O(1)
                P(2)=X(2,N2)+DT2*(V(2,N2)+DT12*A(2,N2))-O(2)
                P(3)=X(3,N2)+DT2*(V(3,N2)+DT12*A(3,N2))-O(3)
              ELSEIF (IDIR == 2) THEN
                P(4)=X(1,N2)+DT2*(V(1,N2)+DT12*A(1,N2))-O(1)
                P(5)=X(2,N2)+DT2*(V(2,N2)+DT12*A(2,N2))-O(2)
                P(6)=X(3,N2)+DT2*(V(3,N2)+DT12*A(3,N2))-O(3)
              ELSEIF (IDIR == 3) THEN
                P(7)=X(1,N2)+DT2*(V(1,N2)+DT12*A(1,N2))-O(1)
                P(8)=X(2,N2)+DT2*(V(2,N2)+DT12*A(2,N2))-O(2)
                P(9)=X(3,N2)+DT2*(V(3,N2)+DT12*A(3,N2))-O(3)
              ENDIF
              !calculation of Y0'(IDIR=1) Z0'(IDIR=2) X0'(IDIR=3)
              IF (IDIR == 1) THEN
                P(4)=X(1,N3)+DT2*(V(1,N3)+DT12*A(1,N3))-O(1)
                P(5)=X(2,N3)+DT2*(V(2,N3)+DT12*A(2,N3))-O(2)
                P(6)=X(3,N3)+DT2*(V(3,N3)+DT12*A(3,N3))-O(3)
              ELSEIF (IDIR == 2) THEN
                P(7)=X(1,N3)+DT2*(V(1,N3)+DT12*A(1,N3))-O(1)
                P(8)=X(2,N3)+DT2*(V(2,N3)+DT12*A(2,N3))-O(2)
                P(9)=X(3,N3)+DT2*(V(3,N3)+DT12*A(3,N3))-O(3)
              ELSEIF (IDIR == 3) THEN
                P(1)=X(1,N3)+DT2*(V(1,N3)+DT12*A(1,N3))-O(1)
                P(2)=X(2,N3)+DT2*(V(2,N3)+DT12*A(2,N3))-O(2)
                P(3)=X(3,N3)+DT2*(V(3,N3)+DT12*A(3,N3))-O(3)
              ENDIF
              ! if X'=0 then X'=X (IDIR=1)
              ! if Y'=0 then Y'=Y (IDIR=2)
              ! if Z'=0 then Z'=Z (IDIR=3)
              IF (IDIR == 1) THEN
                PP1=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
                IF(PP1 == ZERO)THEN
                  P(1)=ONE
                  P(2)=ZERO
                  P(3)=ZERO
                  PP1 =ONE
                ENDIF
              ELSEIF (IDIR == 2) THEN
                PP1=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
                IF(PP1 == ZERO)THEN
                  P(4)=ONE
                  P(5)=ZERO
                  P(6)=ZERO
                  PP1 =ONE
                ENDIF
              ELSEIF (IDIR == 3) THEN
                PP1=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
                IF(PP1==ZERO)THEN
                  P(7)=ONE
                  P(8)=ZERO
                  P(9)=ZERO
                  PP1 =ONE
                ENDIF
              ENDIF
              ! calculation of Z'(IDIR=1) X'(IDIR=2) Y'(IDIR=3)
              IF (IDIR == 1) THEN
                P(7)=P(2)*P(6)-P(3)*P(5)
                P(8)=P(3)*P(4)-P(1)*P(6)
                P(9)=P(1)*P(5)-P(2)*P(4)
                PP3=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
              ELSEIF (IDIR == 2) THEN
                P(1)=P(5)*P(9)-P(6)*P(8)
                P(2)=P(6)*P(7)-P(4)*P(9)
                P(3)=P(4)*P(8)-P(5)*P(7)
                PP3=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
              ELSEIF (IDIR == 3) THEN
                P(4)=P(8)*P(3)-P(9)*P(2)
                P(5)=P(9)*P(1)-P(7)*P(3)
                P(6)=P(7)*P(2)-P(8)*P(1)
                PP3=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
              ENDIF
              ! if Z'=0 (Y0'=X') then Y0' != X' (IDIR=1)
              ! if X'=0 (Z0'=Y') then Z0' != Y' (IDIR=2)
              ! if Y'=0 (X0'=Z') then X0' != Z' (IDIR=3)
              IF (IDIR == 1) THEN
                IF(PP3 == ZERO)THEN
                  IF(P(1) == ZERO)THEN
                    P(4)=PP1
                    P(5)=P(2)
                  ELSE
                    P(4)=P(1)
                    P(5)=ABS(P(2))+PP1
                  ENDIF
                  P(6)=P(3)
                  P(7)=P(2)*P(6)-P(3)*P(5)
                  P(8)=P(3)*P(4)-P(1)*P(6)
                  P(9)=P(1)*P(5)-P(2)*P(4)
                  PP3=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
                ENDIF
              ELSEIF (IDIR == 2) THEN
                IF(PP3 == ZERO)THEN
                  IF(P(4) == ZERO)THEN
                    P(7)=PP1
                    P(8)=P(5)
                  ELSE
                    P(7)=P(4)
                    P(8)=ABS(P(5))+PP1
                  ENDIF
                  P(9)=P(6)
                  P(1)=P(5)*P(9)-P(6)*P(8)
                  P(2)=P(6)*P(7)-P(4)*P(9)
                  P(3)=P(4)*P(8)-P(5)*P(7)
                  PP3=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
                ENDIF
              ELSEIF (IDIR == 3) THEN
                IF(PP3 == ZERO)THEN
                  IF(P(7) == ZERO)THEN
                    P(1)=PP1
                    P(2)=P(8)
                  ELSE
                    P(1)=P(7)
                    P(2)=ABS(P(8))+PP1
                  ENDIF
                  P(3)=P(9)
                  P(4)=P(8)*P(3)-P(9)*P(2)
                  P(5)=P(9)*P(1)-P(7)*P(3)
                  P(6)=P(7)*P(2)-P(8)*P(1)
                  PP3=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
                ENDIF
              ENDIF
              ! calculation of Y'(IDIR=1) Z'(IDIR=2) X'(IDIR=3)
              IF (IDIR == 1) THEN
                P(4)=P(8)*P(3)-P(9)*P(2)
                P(5)=P(9)*P(1)-P(7)*P(3)
                P(6)=P(7)*P(2)-P(8)*P(1)
                PP2=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
              ELSEIF (IDIR == 2) THEN
                P(7)=P(2)*P(6)-P(3)*P(5)
                P(8)=P(3)*P(4)-P(1)*P(6)
                P(9)=P(1)*P(5)-P(2)*P(4)
                PP2=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
              ELSEIF (IDIR == 3) THEN
                P(1)=P(5)*P(9)-P(6)*P(8)
                P(2)=P(6)*P(7)-P(4)*P(9)
                P(3)=P(4)*P(8)-P(5)*P(7)
                PP2=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
              ENDIF
C
            ELSEIF (IMOV == 2) THEN
              ! calculation of X0'
              P(1)=X(1,N3)+DT2*(V(1,N3)+DT12*A(1,N3))-O(1)
              P(2)=X(2,N3)+DT2*(V(2,N3)+DT12*A(2,N3))-O(2)
              P(3)=X(3,N3)+DT2*(V(3,N3)+DT12*A(3,N3))-O(3)
              ! calculation of Z'
              P(7)=X(1,N2)+DT2*(V(1,N2)+DT12*A(1,N2))-O(1)
              P(8)=X(2,N2)+DT2*(V(2,N2)+DT12*A(2,N2))-O(2)
              P(9)=X(3,N2)+DT2*(V(3,N2)+DT12*A(3,N2))-O(3)
              ! if Z'=0 then Z'=Z
              PP3=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
              IF(PP3 == ZERO)THEN
                P(7)=ZERO
                P(8)=ZERO
                P(9)=ONE
                PP3 =ONE
              ENDIF
              ! calculation of Y = Z x X'
              P(4)=P(8)*P(3)-P(9)*P(2)
              P(5)=P(9)*P(1)-P(7)*P(3)
              P(6)=P(7)*P(2)-P(8)*P(1)
              PP2=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
             ! if Y'=0 (X0'=Z') then X0' != Y'
              IF(PP2 == ZERO)THEN
                IF(P(7) == ZERO)THEN
                  P(1)=PP3
                  P(2)=P(8)
                ELSE
                  P(1)=P(7)
                  P(2)=ABS(P(8))+PP3
                ENDIF
                P(3)=P(9)
                P(4)=P(8)*P(3)-P(9)*P(2)
                P(5)=P(9)*P(1)-P(7)*P(3)
                P(6)=P(7)*P(2)-P(8)*P(1)
                PP2=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
              ENDIF
              ! calculation of X' = Y' x Z'
              P(1)=P(5)*P(9)-P(6)*P(8)
              P(2)=P(6)*P(7)-P(4)*P(9)
              P(3)=P(4)*P(8)-P(5)*P(7)
              PP1=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
            ENDIF
            ! norm
            P(1)=P(1)/PP1
            P(2)=P(2)/PP1
            P(3)=P(3)/PP1
            P(4)=P(4)/PP2
            P(5)=P(5)/PP2
            P(6)=P(6)/PP2
            P(7)=P(7)/PP3
            P(8)=P(8)/PP3
            P(9)=P(9)/PP3
            ! ANGULAR VELOCITY at TIME T+1/2
            ! rotation increment DR=Transpose( P Transpose(POLD) )
            DR11=P(1)*POLD(1)+P(4)*POLD(4)+P(7)*POLD(7)
            DR21=P(1)*POLD(2)+P(4)*POLD(5)+P(7)*POLD(8)
            DR31=P(1)*POLD(3)+P(4)*POLD(6)+P(7)*POLD(9)
            DR12=P(2)*POLD(1)+P(5)*POLD(4)+P(8)*POLD(7)
            DR22=P(2)*POLD(2)+P(5)*POLD(5)+P(8)*POLD(8)
            DR32=P(2)*POLD(3)+P(5)*POLD(6)+P(8)*POLD(9)
            DR13=P(3)*POLD(1)+P(6)*POLD(4)+P(9)*POLD(7)
            DR23=P(3)*POLD(2)+P(6)*POLD(5)+P(9)*POLD(8)
            DR33=P(3)*POLD(3)+P(6)*POLD(6)+P(9)*POLD(9)
            ! rotation increment vector.
            CS=(DR11+DR22+DR33-ONE)*HALF
            IF (CS >= ONE) THEN
              DRX=(DR23-DR32)*HALF
              DRY=(DR31-DR13)*HALF
              DRZ=(DR12-DR21)*HALF
            ELSEIF (CS <= -ONE) THEN
              DRX=XPI*SQRT((DR11+ONE)*HALF)
              DRY=XPI*SQRT((DR22+ONE)*HALF)
              DRZ=XPI*SQRT((DR33+ONE)*HALF)
              IF (DR12 < ZERO .AND. DR23 < ZERO) THEN
                DRY=-DRY
              ELSEIF (DR23 < ZERO .AND. DR31 < ZERO) THEN
                DRZ=-DRZ
              ELSEIF (DR31 < ZERO .AND. DR12 < ZERO) THEN
                DRX=-DRX
              ELSEIF (DR12 < ZERO) THEN
                DRZ=-DRZ
              ELSEIF (DR23 < ZERO) THEN
                DRX=-DRX
              ELSEIF (DR31 < ZERO) THEN
                DRY=-DRY
            ENDIF
          ELSE
            R=ACOS(CS)
            SN=SIN(R)
            RS2=R/(TWO*SN)
            DRX=(DR23-DR32)*RS2
            DRY=(DR31-DR13)*RS2
            DRZ=(DR12-DR21)*RS2
          ENDIF
          VRX=DRX/MAX(EM20,DT2)
          VRY=DRY/MAX(EM20,DT2)
          VRZ=DRZ/MAX(EM20,DT2)
          ! RETRIEVE ANGULAR ACCELERATION AT TIME T
          DTINV=ONE/DT12
          XFRAME(16,N+1)=(VRX-XFRAME(13,N+1))*DTINV
          XFRAME(17,N+1)=(VRY-XFRAME(14,N+1))*DTINV
          XFRAME(18,N+1)=(VRZ-XFRAME(15,N+1))*DTINV
          IF(NXFRAME >= 36)THEN
            XFRAME(28,N+1)=A(1,N1)
            XFRAME(29,N+1)=A(2,N1)
            XFRAME(30,N+1)=A(3,N1)
          ENDIF
        ENDIF
C
        ENDDO !next N=1,NUMFRAM
      ENDIF

      ! SPMD : domain P0 sends data to other domains
      IF(NSPMD > 1) CALL SPMD_RBCAST(XFRAME,XFRAME,NXFRAME,NUMFRAM+1,0,2)

      RETURN
      END SUBROUTINE MOVFRA1


Chd|====================================================================
Chd|  MOVFRA2                       source/tools/skew/movfram.F   
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ROTBMR                        source/tools/skew/rotbmr.F    
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|====================================================================
      SUBROUTINE MOVFRA2(XFRAME ,IFRAME ,X ,V ,VR , D)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,intent(IN) :: IFRAME(LISKN,NUMFRAM+1)
      my_real, intent(INOUT) :: XFRAME(NXFRAME,NUMFRAM+1)
      my_real, intent(IN) :: X(3,NUMNOD), V(3,NUMNOD), VR(3,NUMNOD), D(3,NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, N1, N2, N3, K, IMOV, IDIR
      my_real O(3), P(9), PP1, PP3, PP2, POLD(9),PT(9),VRN(3),
     .        DR11, DR12, DR13,
     .        DR21, DR22, DR23,
     .        DR31, DR32, DR33,
     .        XPI, CS, R, SN, RS2, DRX, DRY, DRZ, VRX, VRY, VRZ
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF(ISPMD==0) THEN
       XPI=PI
       DO N=1,NUMFRAM
         N1=IFRAME(1,N+1)
         N2=IFRAME(2,N+1)
         N3=IFRAME(3,N+1)
         IMOV=IFRAME(5,N+1)
         ! reference frame mobile
         IF(N1+N2+N3==0)CYCLE
         ! extract frame orientation wrt global system at time t-1.
         POLD(1)=XFRAME(1,N+1)
         POLD(2)=XFRAME(2,N+1)
         POLD(3)=XFRAME(3,N+1)
         POLD(4)=XFRAME(4,N+1)
         POLD(5)=XFRAME(5,N+1)
         POLD(6)=XFRAME(6,N+1)
         POLD(7)=XFRAME(7,N+1)
         POLD(8)=XFRAME(8,N+1)
         POLD(9)=XFRAME(9,N+1)
         IF (N2+N3==0) THEN
           ! Node defined frame
           VRN(1)=POLD(1)*VR(1,N1)+POLD(2)*VR(2,N1)+POLD(3)*VR(3,N1)
           VRN(2)=POLD(4)*VR(1,N1)+POLD(5)*VR(2,N1)+POLD(6)*VR(3,N1)
           VRN(3)=POLD(7)*VR(1,N1)+POLD(8)*VR(2,N1)+POLD(9)*VR(3,N1)
           CALL ROTBMR (VRN ,POLD ,DT2)
           DO K=1,9
             XFRAME(K,N+1)=POLD(K)
           ENDDO
           XFRAME(10,N+1)=X(1,N1)
           XFRAME(11,N+1)=X(2,N1)
           XFRAME(12,N+1)=X(3,N1)
           XFRAME(13,N+1)=VR(1,N1)
           XFRAME(14,N+1)=VR(2,N1)
           XFRAME(15,N+1)=VR(3,N1)
           IF(NXFRAME>=36)THEN
             XFRAME(31,N+1)=V(1,N1)
             XFRAME(32,N+1)=V(2,N1)
             XFRAME(33,N+1)=V(3,N1)
             XFRAME(34,N+1)=D(1,N1)
             XFRAME(35,N+1)=D(2,N1)
             XFRAME(36,N+1)=D(3,N1)
           ENDIF
         ELSE
           ! MOVING FRAME
           !  retrieve position & orientation wrt global system at time t
           O(1)=X(1,N1)
           O(2)=X(2,N1)
           O(3)=X(3,N1)
           IF (IMOV == 1) THEN
             IDIR = IFRAME(6,N+1)
             ! calculation of X'(IDIR=1) Y'(IDIR=2) Z'(IDIR=3)
             IF (IDIR == 1) THEN
               P(1)=X(1,N2)-X(1,N1)
               P(2)=X(2,N2)-X(2,N1)
               P(3)=X(3,N2)-X(3,N1)
             ELSEIF (IDIR == 2) THEN
               P(4)=X(1,N2)-X(1,N1)
               P(5)=X(2,N2)-X(2,N1)
               P(6)=X(3,N2)-X(3,N1)
             ELSEIF (IDIR == 3) THEN
               P(7)=X(1,N2)-X(1,N1)
               P(8)=X(2,N2)-X(2,N1)
               P(9)=X(3,N2)-X(3,N1)
             ENDIF
             ! calculation of Y0'(IDIR=1) Z0'(IDIR=2) X0'(IDIR=3)
             IF (IDIR == 1) THEN
               P(4)=X(1,N3)-X(1,N1)
               P(5)=X(2,N3)-X(2,N1)
               P(6)=X(3,N3)-X(3,N1)
             ELSEIF (IDIR == 2) THEN
               P(7)=X(1,N3)-X(1,N1)
               P(8)=X(2,N3)-X(2,N1)
               P(9)=X(3,N3)-X(3,N1)
             ELSEIF (IDIR == 3) THEN
               P(1)=X(1,N3)-X(1,N1)
               P(2)=X(2,N3)-X(2,N1)
               P(3)=X(3,N3)-X(3,N1)
             ENDIF
             ! if X'=0 then X'=X(IDIR=1)
             ! if Y'=0 then Y'=Y(IDIR=2)
             ! if Z'=0 then Z'=Z(IDIR=3)
             IF (IDIR == 1) THEN
               PP1=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
               IF(PP1 == ZERO)THEN
                P(1)=ONE
                P(2)=ZERO
                P(3)=ZERO
                PP1 =ONE
               ENDIF
             ELSEIF (IDIR == 2) THEN
               PP1=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
               IF(PP1 == ZERO)THEN
                P(4)=ONE
                P(5)=ZERO
                P(6)=ZERO
                PP1 =ONE
               ENDIF
             ELSEIF (IDIR == 3) THEN
               PP1=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
               IF(PP1 == ZERO)THEN
                P(7)=ONE
                P(8)=ZERO
                P(9)=ZERO
                PP1 =ONE
               ENDIF
             ENDIF
             ! calculation of Z'(IDIR=1) DE X'(IDIR=2) DE Y'(IDIR=3)
             IF (IDIR == 1) THEN
               P(7)=P(2)*P(6)-P(3)*P(5)
               P(8)=P(3)*P(4)-P(1)*P(6)
               P(9)=P(1)*P(5)-P(2)*P(4)
               PP3=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
             ELSEIF (IDIR == 2) THEN
               P(1)=P(5)*P(9)-P(6)*P(8)
               P(2)=P(6)*P(7)-P(4)*P(9)
               P(3)=P(4)*P(8)-P(5)*P(7)
               PP3=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
             ELSEIF (IDIR == 3) THEN
               P(4)=P(8)*P(3)-P(9)*P(2)
               P(5)=P(9)*P(1)-P(7)*P(3)
               P(6)=P(7)*P(2)-P(8)*P(1)
               PP3=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
             ENDIF
             ! if Z'=0 (Y0'=X') then Y0' != X'(IDIR=1)
             ! if X'=0 (Z0'=Y') then Z0' != Y'(IDIR=2)
             ! if Y'=0 (X0'=Z') then X0' != Z'(IDIR=3)
             IF (IDIR == 1) THEN
               IF(PP3 == ZERO)THEN
                IF(P(1) == ZERO)THEN
                 P(4)=PP1
                 P(5)=P(2)
                ELSE
                 P(4)=P(1)
                 P(5)=ABS(P(2))+PP1
                ENDIF
                P(6)=P(3)
                P(7)=P(2)*P(6)-P(3)*P(5)
                P(8)=P(3)*P(4)-P(1)*P(6)
                P(9)=P(1)*P(5)-P(2)*P(4)
                PP3=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
               ENDIF
             ELSEIF (IDIR == 2) THEN
               IF(PP3 == ZERO)THEN
                IF(P(4) == ZERO)THEN
                 P(7)=PP1
                 P(8)=P(5)
                ELSE
                 P(7)=P(1)
                 P(8)=ABS(P(5))+PP1
                ENDIF
                P(9)=P(6)
                P(1)=P(5)*P(9)-P(6)*P(8)
                P(2)=P(6)*P(7)-P(4)*P(9)
                P(3)=P(4)*P(8)-P(5)*P(7)
                PP3=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
               ENDIF
             ELSEIF (IDIR == 3) THEN
               IF(PP3 == ZERO)THEN
                IF(P(7) == ZERO)THEN
                 P(1)=PP1
                 P(2)=P(8)
                ELSE
                 P(1)=P(4)
                 P(2)=ABS(P(8))+PP1
                ENDIF
                P(3)=P(9)
                P(4)=P(8)*P(3)-P(9)*P(2)
                P(5)=P(9)*P(1)-P(7)*P(3)
                P(6)=P(7)*P(2)-P(8)*P(1)
                PP3=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
               ENDIF
             ENDIF
             ! calculation of Y'(IDIR=1) Z'(IDIR=2) X'(IDIR=3)
             IF (IDIR == 1) THEN
               P(4)=P(8)*P(3)-P(9)*P(2)
               P(5)=P(9)*P(1)-P(7)*P(3)
               P(6)=P(7)*P(2)-P(8)*P(1)
               PP2=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
             ELSEIF (IDIR == 2) THEN
               P(7)=P(2)*P(6)-P(3)*P(5)
               P(8)=P(3)*P(4)-P(1)*P(6)
               P(9)=P(1)*P(5)-P(2)*P(4)
               PP2=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
             ELSEIF (IDIR == 3) THEN
               P(1)=P(5)*P(9)-P(6)*P(8)
               P(2)=P(6)*P(7)-P(4)*P(9)
               P(3)=P(4)*P(8)-P(5)*P(7)
               PP2=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
             ENDIF

           ELSEIF (IMOV == 2) THEN
             ! calculation of X0'
             P(1)=X(1,N3)-X(1,N1)
             P(2)=X(2,N3)-X(2,N1)
             P(3)=X(3,N3)-X(3,N1)
             ! calculation of Z'
             P(7)=X(1,N2)-X(1,N1)
             P(8)=X(2,N2)-X(2,N1)
             P(9)=X(3,N2)-X(3,N1)
             ! if Z'=0 then Z'=Z
             PP3=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
             IF(PP3==ZERO)THEN
               P(7)=ZERO
               P(8)=ZERO
               P(9)=ONE
               PP3 =ONE
             ENDIF
             ! calculation of Y' = Z' x X0'
             P(4)=P(8)*P(3)-P(9)*P(2)
             P(5)=P(9)*P(1)-P(7)*P(3)
             P(6)=P(7)*P(2)-P(8)*P(1)
             PP2=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
             ! if Y'=0 (X0'=Z') then X0' != Y'
             IF(PP2 == ZERO)THEN
               IF(P(7) == ZERO)THEN
                 P(1)=PP3
                 P(2)=P(8)
               ELSE
                 P(1)=P(7)
                 P(2)=ABS(P(8))+PP3
               ENDIF
               P(3)=P(9)
               P(4)=P(8)*P(3)-P(9)*P(2)
               P(5)=P(9)*P(1)-P(7)*P(3)
               P(6)=P(7)*P(2)-P(8)*P(1)
               PP2=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
             ENDIF
             ! calculation of X' = Y' x Z'
             P(1)=P(5)*P(9)-P(6)*P(8)
             P(2)=P(6)*P(7)-P(4)*P(9)
             P(3)=P(4)*P(8)-P(5)*P(7)
             PP1=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
           ENDIF
           ! NORM
           P(1)=P(1)/PP1
           P(2)=P(2)/PP1
           P(3)=P(3)/PP1
           P(4)=P(4)/PP2
           P(5)=P(5)/PP2
           P(6)=P(6)/PP2
           P(7)=P(7)/PP3
           P(8)=P(8)/PP3
           P(9)=P(9)/PP3
           DO K=1,9
             XFRAME(K,N+1)=P(K)
           ENDDO
           XFRAME(10,N+1)=O(1)
           XFRAME(11,N+1)=O(2)
           XFRAME(12,N+1)=O(3)
           IF(NXFRAME >= 36)THEN
             XFRAME(31,N+1)=V(1,N1)
             XFRAME(32,N+1)=V(2,N1)
             XFRAME(33,N+1)=V(3,N1)
             XFRAME(34,N+1)=D(1,N1)
             XFRAME(35,N+1)=D(2,N1)
             XFRAME(36,N+1)=D(3,N1)
           ENDIF
           ! retrieve ANGULAR VELOCITY at TIME T-1/2
           VRX=XFRAME(13,N+1)+DT12*XFRAME(16,N+1)
           VRY=XFRAME(14,N+1)+DT12*XFRAME(17,N+1)
           VRZ=XFRAME(15,N+1)+DT12*XFRAME(18,N+1)
           XFRAME(13,N+1)=VRX
           XFRAME(14,N+1)=VRY
           XFRAME(15,N+1)=VRZ
          ENDIF
        ENDDO ! next N=1,NUMFRAM
      ENDIF

      ! SPMD : domain P0 sends data to other domains
      IF(NSPMD > 1) CALL SPMD_RBCAST(XFRAME,XFRAME,NXFRAME,NUMFRAM+1,0,2)

      RETURN
      END SUBROUTINE MOVFRA2

Chd|====================================================================
Chd|  MOVFRA_IMP                    source/tools/skew/movfram.F   
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ROTBMR                        source/tools/skew/rotbmr.F    
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|====================================================================
      SUBROUTINE MOVFRA_IMP(XFRAME,IFRAME ,X, V ,A ,VR ,AR ,D )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, intent(in) :: IFRAME(LISKN,NUMFRAM+1)
      my_real, intent(INOUT) :: XFRAME(NXFRAME,NUMFRAM+1)
      my_real, intent(IN) :: X(3,NUMNOD), V(3,NUMNOD), A(3,NUMNOD), VR(3,NUMNOD), AR(3,NUMNOD), D(3,NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, N1, N2, N3, K, IMOV
      my_real O(3), P(9), PP1, PP3, PP2, POLD(9),
     .        DR11, DR12, DR13,
     .        DR21, DR22, DR23,
     .        DR31, DR32, DR33,
     .        XPI, CS, R, SN, RS2, DRX, DRY, DRZ, VRX, VRY, VRZ, DTINV, VRN(3)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF(ISPMD == 0) THEN
        XPI=PI
        DO N=1,NUMFRAM
          N1=IFRAME(1,N+1)
          N2=IFRAME(2,N+1)
          N3=IFRAME(3,N+1)
          IMOV=IFRAME(5,N+1)
          IF(N1+N2+N3 == 0) THEN
          ! Fixed Frame
          ELSEIF (N2+N3 == 0) THEN
          ! Node defined frame
            IF (INCONV == 1) THEN
              POLD(1)=XFRAME(1,N+1)
              POLD(2)=XFRAME(2,N+1)
              POLD(3)=XFRAME(3,N+1)
              POLD(4)=XFRAME(4,N+1)
              POLD(5)=XFRAME(5,N+1)
              POLD(6)=XFRAME(6,N+1)
              POLD(7)=XFRAME(7,N+1)
              POLD(8)=XFRAME(8,N+1)
              POLD(9)=XFRAME(9,N+1)
              VRN(1)=POLD(1)*VR(1,N1)+POLD(2)*VR(2,N1)+POLD(3)*VR(3,N1)
              VRN(2)=POLD(4)*VR(1,N1)+POLD(5)*VR(2,N1)+POLD(6)*VR(3,N1)
              VRN(3)=POLD(7)*VR(1,N1)+POLD(8)*VR(2,N1)+POLD(9)*VR(3,N1)
              CALL ROTBMR (VRN ,POLD ,DT2)
              DO K=1,9
                XFRAME(K,N+1)=POLD(K)
              ENDDO
            END IF !(INCONV==1) THEN
            XFRAME(10,N+1)=X(1,N1)
            XFRAME(11,N+1)=X(2,N1)
            XFRAME(12,N+1)=X(3,N1)
            XFRAME(13,N+1)=VR(1,N1)
            XFRAME(14,N+1)=VR(2,N1)
            XFRAME(15,N+1)=VR(3,N1)
            XFRAME(16,N+1)=AR(1,N1)
            XFRAME(17,N+1)=AR(2,N1)
            XFRAME(18,N+1)=AR(3,N1)
            IF(NXFRAME >= 36)THEN
             XFRAME(28,N+1)=A(1,N1)
             XFRAME(29,N+1)=A(2,N1)
             XFRAME(30,N+1)=A(3,N1)
             XFRAME(31,N+1)=V(1,N1)
             XFRAME(32,N+1)=V(2,N1)
             XFRAME(33,N+1)=V(3,N1)
             XFRAME(34,N+1)=D(1,N1)
             XFRAME(35,N+1)=D(2,N1)
             XFRAME(36,N+1)=D(3,N1)
            ENDIF
          ELSE
          ! Moving frame frame
            ! extract frame orientation wrt global system at time t
            POLD(1)=XFRAME(1,N+1)
            POLD(2)=XFRAME(2,N+1)
            POLD(3)=XFRAME(3,N+1)
            POLD(4)=XFRAME(4,N+1)
            POLD(5)=XFRAME(5,N+1)
            POLD(6)=XFRAME(6,N+1)
            POLD(7)=XFRAME(7,N+1)
            POLD(8)=XFRAME(8,N+1)
            POLD(9)=XFRAME(9,N+1)
           ! position & orientation wrt global system at time t+1
            O(1)=X(1,N1)
            O(2)=X(2,N1)
            O(3)=X(3,N1)
            IF (IMOV == 1) THEN
              ! calculation of X'
              P(1)=X(1,N2)-O(1)
              P(2)=X(2,N2)-O(2)
              P(3)=X(3,N2)-O(3)
              ! calculation of Y0'
              P(4)=X(1,N3)-O(1)
              P(5)=X(2,N3)-O(2)
              P(6)=X(3,N3)-O(3)
              ! if X'=0 then X'=X
              PP1=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
              IF(PP1 == ZERO)THEN
                P(1)=ONE
                P(2)=ZERO
                P(3)=ZERO
                PP1 =ONE
              ENDIF
              ! calculation of Z'
              P(7)=P(2)*P(6)-P(3)*P(5)
              P(8)=P(3)*P(4)-P(1)*P(6)
              P(9)=P(1)*P(5)-P(2)*P(4)
              PP3=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
              ! if Z'=0 (Y0'=X') then Y0' != X'
              IF(PP3 == ZERO)THEN
                IF(P(1) == ZERO)THEN
                  P(4)=PP1
                  P(5)=P(2)
                ELSE
                  P(4)=P(1)
                  P(5)=ABS(P(2))+PP1
                ENDIF
                P(6)=P(3)
                P(7)=P(2)*P(6)-P(3)*P(5)
                P(8)=P(3)*P(4)-P(1)*P(6)
                P(9)=P(1)*P(5)-P(2)*P(4)
                PP3=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
              ENDIF
              ! calculation of Y'
              P(4)=P(8)*P(3)-P(9)*P(2)
              P(5)=P(9)*P(1)-P(7)*P(3)
              P(6)=P(7)*P(2)-P(8)*P(1)
              PP2=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))

            ELSEIF (IMOV == 2) THEN
              ! calculation of X0'
              P(1)=X(1,N3)-O(1)
              P(2)=X(2,N3)-O(2)
              P(3)=X(3,N3)-O(3)
              ! calculation of Z'
              P(7)=X(1,N2)-O(1)
              P(8)=X(2,N2)-O(2)
              P(9)=X(3,N2)-O(3)
              ! if Z'=0 then Z'=Z
              PP3=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
              IF(PP3 == ZERO)THEN
                P(7)=ZERO
                P(8)=ZERO
                P(9)=ONE
                PP3 =ONE
              ENDIF
              ! calculation of Y=ZxX'
              P(4)=P(8)*P(3)-P(9)*P(2)
              P(5)=P(9)*P(1)-P(7)*P(3)
              P(6)=P(7)*P(2)-P(8)*P(1)
              PP2=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
              ! if Y'=0 (X0'=Z') then X0' != Y'
              IF(PP2 == ZERO)THEN
                IF(P(7)==ZERO)THEN
                  P(1)=PP3
                  P(2)=P(8)
                ELSE
                  P(1)=P(7)
                  P(2)=ABS(P(8))+PP3
                ENDIF
                P(3)=P(9)
                P(4)=P(8)*P(3)-P(9)*P(2)
                P(5)=P(9)*P(1)-P(7)*P(3)
                P(6)=P(7)*P(2)-P(8)*P(1)
                PP2=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
              ENDIF
              ! calculation of X' = Y' x Z'
              P(1)=P(5)*P(9)-P(6)*P(8)
              P(2)=P(6)*P(7)-P(4)*P(9)
              P(3)=P(4)*P(8)-P(5)*P(7)
              PP1=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
            ENDIF
            ! norm
            P(1)=P(1)/PP1
            P(2)=P(2)/PP1
            P(3)=P(3)/PP1
            P(4)=P(4)/PP2
            P(5)=P(5)/PP2
            P(6)=P(6)/PP2
            P(7)=P(7)/PP3
            P(8)=P(8)/PP3
            P(9)=P(9)/PP3
            IF (INCONV == 1) THEN
              XFRAME(10,N+1)=X(1,N1)
              XFRAME(11,N+1)=X(2,N1)
              XFRAME(12,N+1)=X(3,N1)
              DO K=1,9
                XFRAME(K,N+1)=P(K)
              ENDDO
            END IF !(INCONV==1) THEN
            !  angular velocity at time t+1/2
            !  rotation increment DR=Transpose( P Transpose(POLD) )
            DR11=P(1)*POLD(1)+P(4)*POLD(4)+P(7)*POLD(7)
            DR21=P(1)*POLD(2)+P(4)*POLD(5)+P(7)*POLD(8)
            DR31=P(1)*POLD(3)+P(4)*POLD(6)+P(7)*POLD(9)
            DR12=P(2)*POLD(1)+P(5)*POLD(4)+P(8)*POLD(7)
            DR22=P(2)*POLD(2)+P(5)*POLD(5)+P(8)*POLD(8)
            DR32=P(2)*POLD(3)+P(5)*POLD(6)+P(8)*POLD(9)
            DR13=P(3)*POLD(1)+P(6)*POLD(4)+P(9)*POLD(7)
            DR23=P(3)*POLD(2)+P(6)*POLD(5)+P(9)*POLD(8)
            DR33=P(3)*POLD(3)+P(6)*POLD(6)+P(9)*POLD(9)
            ! rotation increment vector.
            CS=(DR11+DR22+DR33-ONE)*HALF
            IF (CS >= ONE) THEN
              DRX=(DR23-DR32)*HALF
              DRY=(DR31-DR13)*HALF
              DRZ=(DR12-DR21)*HALF
            ELSEIF (CS <= -ONE) THEN
              DRX=XPI*SQRT((DR11+ONE)*HALF)
              DRY=XPI*SQRT((DR22+ONE)*HALF)
              DRZ=XPI*SQRT((DR33+ONE)*HALF)
              IF (DR12 < ZERO .AND. DR23 < ZERO) THEN
                DRY=-DRY
              ELSEIF (DR23<ZERO.AND.DR31 < ZERO) THEN
                DRZ=-DRZ
              ELSEIF (DR31 < ZERO .AND. DR12 < ZERO) THEN
                DRX=-DRX
              ELSEIF (DR12 < ZERO) THEN
                DRZ=-DRZ
              ELSEIF (DR23 < ZERO) THEN
                DRX=-DRX
              ELSEIF (DR31 < ZERO) THEN
                DRY=-DRY
            ENDIF
          ELSE
            R=ACOS(CS)
            SN=SIN(R)
            RS2=R/(TWO*SN)
            DRX=(DR23-DR32)*RS2
            DRY=(DR31-DR13)*RS2
            DRZ=(DR12-DR21)*RS2
          ENDIF

          VRX=DRX/MAX(EM20,DT2)
          VRY=DRY/MAX(EM20,DT2)
          VRZ=DRZ/MAX(EM20,DT2)

          XFRAME(13,N+1)=VRX
          XFRAME(14,N+1)=VRY
          XFRAME(15,N+1)=VRZ

          ! retrieve angular acceleration at time t
          DTINV=ONE/DT12
          XFRAME(16,N+1)=(VRX-XFRAME(13,N+1))*DTINV
          XFRAME(17,N+1)=(VRY-XFRAME(14,N+1))*DTINV
          XFRAME(18,N+1)=(VRZ-XFRAME(15,N+1))*DTINV
          IF(NXFRAME >= 36)THEN
           XFRAME(28,N+1)=A(1,N1)
           XFRAME(29,N+1)=A(2,N1)
           XFRAME(30,N+1)=A(3,N1)
           XFRAME(31,N+1)=V(1,N1)
           XFRAME(32,N+1)=V(2,N1)
           XFRAME(33,N+1)=V(3,N1)
           XFRAME(34,N+1)=D(1,N1)
           XFRAME(35,N+1)=D(2,N1)
           XFRAME(36,N+1)=D(3,N1)
          ENDIF
        ENDIF !(N1+N2+N3==0)

        ENDDO !next N=1,NUMFRAM
      ENDIF

      ! SPMD : domain P0 sends data to other domains
      IF(NSPMD > 1) CALL SPMD_RBCAST(XFRAME,XFRAME,NXFRAME,NUMFRAM+1,0,2)

      RETURN
      END SUBROUTINE MOVFRA_IMP

