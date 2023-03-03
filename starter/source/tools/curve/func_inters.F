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
Chd|  FUNC_INTERS                   source/tools/curve/func_inters.F
Chd|-- called by -----------
Chd|        LAW36_UPD                     source/materials/mat/mat036/law36_upd.F
Chd|        LAW58_UPD                     source/materials/mat/mat058/law58_upd.F
Chd|        LAW88_UPD                     source/materials/mat/mat088/law88_upd.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FUNC_INTERS(TITR,MAT_ID,FUNC1,FUNC2,FAC1,FAC2,NPC,PLD,XINT,YINT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*nchartitle :: TITR
      INTEGER  :: MAT_ID,FUNC1,FUNC2
      my_real  :: XINT,YINT,FAC1,FAC2
      INTEGER  ,DIMENSION(SNPC) :: NPC 
      my_real  ,DIMENSION(STF)  :: PLD
C-----------------------------------------------
      INTENT(IN)    :: TITR,FUNC1,FUNC2,MAT_ID,NPC,PLD,FAC1,FAC2
      INTENT(INOUT) :: XINT,YINT 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,ID,NP1,NP2,J1,K,K1,FOUND
      my_real :: S1,S2,T1,T2,X1,X2,Y1,Y2,AX,BX,AY,BY,CX,CY,DM,ALPHA,BETA
C=======================================================================
c     Check common points between 2 curves => intersection
      XINT  = ZERO
      YINT  = ZERO
      FOUND = 0
      NP1  = (NPC(FUNC1+1)-NPC(FUNC1))*HALF  ! nb of points of the 1st curve
      NP2  = (NPC(FUNC2+1)-NPC(FUNC2))*HALF  ! nb of points of the 2nd curve 
      DO J=2,NP1+1
        J1=2*(J-2)
        S1=PLD(NPC(FUNC1)+J1)
        T1=PLD(NPC(FUNC1)+J1+1)*FAC1
        DO K=2,NP2+1
          K1=2*(K-2)
          X1=PLD(NPC(FUNC2)+K1)
          Y1=PLD(NPC(FUNC2)+K1+1)*FAC2
          IF (X1 == S1 .AND. Y1 == T1 .AND. X1> ZERO) THEN
            XINT  = X1
            YINT  = Y1
            FOUND = 1
            EXIT
          ENDIF
        ENDDO
        IF (FOUND == 1) EXIT
      ENDDO
c     Check intersection of curve segments
      IF (FOUND == 0) THEN
        DO J=2,NP1
          J1=2*(J-2)
          S1=PLD(NPC(FUNC1)+J1)
          S2=PLD(NPC(FUNC1)+J1+2)
          T1=PLD(NPC(FUNC1)+J1+1)*FAC1
          T2=PLD(NPC(FUNC1)+J1+3)*FAC1
          DO K=2,NP2
            K1=2*(K-2)
            X1=PLD(NPC(FUNC2)+K1)
            X2=PLD(NPC(FUNC2)+K1+2)
            Y1=PLD(NPC(FUNC2)+K1+1)*FAC2
            Y2=PLD(NPC(FUNC2)+K1+3)*FAC2
            IF (X2 < S1 .or. S2 < X1) CYCLE
            AX = X2 - X1
            AY = Y2 - Y1
            BX = S1 - S2
            BY = T1 - T2
            DM = AY*BX - AX*BY
            IF (DM /= ZERO) THEN  ! check if segments are not parallel
              CX = S1 - X1
              CY = T1 - Y1
              ALPHA = (BX * CY - BY * CX) / DM
              BETA  = (AX * CY - AY * CX) / DM
              IF (ALPHA > ZERO .and. ALPHA < ONE .and.
     .            BETA  < ZERO .and. BETA  >-ONE) THEN
                XINT = X1 + ALPHA * AX
                YINT = Y1 + ALPHA * AY
                FOUND = 1
                EXIT
              ENDIF
            ENDIF
          ENDDO
          IF (FOUND == 1) EXIT
        ENDDO
      END IF
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  FUNC_INTERS_SHEAR             source/tools/curve/func_inters.F
Chd|-- called by -----------
Chd|        LAW58_UPD                     source/materials/mat/mat058/law58_upd.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE FUNC_INTERS_SHEAR(TITR,MAT_ID  ,FUNC   ,FUND,  FAC1  ,FAC2,
     .                            NPC,PLD,XINT1 ,YINT1  ,XINT2 ,YINT2  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*nchartitle :: TITR
      !INTEGER  , DIMENSION(NFUNC)      :: IFUNC,FUNC_ID
      INTEGER     FUNC,FUND,NPC(*) 
      INTEGER  :: MAT_ID
      my_real  
     .            XINT1 ,YINT1  ,XINT2 ,YINT2,FAC1,FAC2,PLD(*)
!      TYPE(TTABLE) TABLE(*)
C-----------------------------------------------
      INTENT(IN)    :: TITR,FUNC,FUND,MAT_ID,NPC,PLD,FAC1,FAC2
      INTENT(INOUT) :: XINT1 ,YINT1  ,XINT2 ,YINT2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,NP1,NP2,J1,K,K1
      my_real 
     .      S1,S2,T1,T2,X1,X2,Y1,Y2,SX,TY,DYDX,DTDS
C=======================================================================
          !direction SHEAR, kfunc(3) kfunc(6)
          XINT1 = ZERO
          YINT1 = ZERO
          XINT2 = ZERO
          YINT2 = ZERO
          SX = ZERO
          TY = ZERO
          NP1  = (NPC(FUNC+1)-NPC(FUNC))*HALF  
          NP2  = (NPC(FUND+1)-NPC(FUND))*HALF 
          DO J=2,NP1+1
            J1=2*(J-2)
            S1=PLD(NPC(FUNC)+J1)
            T1=PLD(NPC(FUNC)+J1+1)*FAC1
            DO K=2,NP2+1
              K1=2*(K-2)
              X1=PLD(NPC(FUND)+K1)
              Y1=PLD(NPC(FUND)+K1+1)*FAC2
              IF(X1 == S1 .AND. Y1 == T1 .AND.X1> ZERO)THEN
                    XINT1 = X1
                    YINT1 = Y1
                    GOTO 361
              ENDIF
             ENDDO
          ENDDO
          DO J=2,NP1
            J1=2*(J-2)
            S1=PLD(NPC(FUNC)+J1)
            S2=PLD(NPC(FUNC)+J1+2)
            T1=PLD(NPC(FUNC)+J1+1)*FAC1
            T2=PLD(NPC(FUNC)+J1+3)*FAC1
            DO K=2,NP2
              K1=2*(K-2)
              X1=PLD(NPC(FUND)+K1)
              X2=PLD(NPC(FUND)+K1+2)
              Y1=PLD(NPC(FUND)+K1+1)*FAC2
              Y2=PLD(NPC(FUND)+K1+3)*FAC2
              IF(X1>ZERO.AND.X2>ZERO.AND.S1>ZERO.AND.S2>ZERO)THEN
              IF (Y2>=T1 .AND. Y1<=T2 .AND. X2>=S1 .AND. X1<=S2) THEN
                DYDX = (Y2-Y1) / (X2-X1)
                DTDS = (T2-T1) / (S2-S1)
                IF (DYDX > DTDS) THEN
                  !SX = (DTDS*S2-DYDX*X2-T2+Y2) / (DTDS-DYDX)
                  !TY =  T2 + DTDS*(SX - S2)
                  SX = (T1-Y1-DTDS*S1+DYDX*X1) / (DYDX-DTDS)
                  TY =  T1 + DTDS*(SX - S1)
                  IF (TY>=Y1 .AND. TY<=Y2 .AND. SX>=X1 .AND. SX<=X2.AND.SX/=ZERO)THEN           
                    XINT1 = SX
                    YINT1 = TY
                    GOTO 361
                  ENDIF
                ENDIF
              ENDIF
             ENDIF
            ENDDO
          ENDDO
 361      CONTINUE
          DO J=2,NP1+1
            J1=2*(J-2)
            S1=PLD(NPC(FUNC)+J1)
            T1=PLD(NPC(FUNC)+J1+1)*FAC1
            DO K=2,NP2+1
              K1=2*(K-2)
              X1=PLD(NPC(FUND)+K1)
              Y1=PLD(NPC(FUND)+K1+1)*FAC2
              IF(X1 == S1 .AND. Y1 == T1 .AND.X1 < ZERO)THEN
                    XINT2 = X1
                    YINT2 = Y1
                    GOTO 362
              ENDIF
             ENDDO
          ENDDO
          DO J=2,NP1
            J1=2*(J-2)
            S1=PLD(NPC(FUNC)+J1)
            S2=PLD(NPC(FUNC)+J1+2)
            T1=PLD(NPC(FUNC)+J1+1)*FAC1
            T2=PLD(NPC(FUNC)+J1+3)*FAC1
            DO K=2,NP2
              K1=2*(K-2)
              X1=PLD(NPC(FUND)+K1)
              X2=PLD(NPC(FUND)+K1+2)
              Y1=PLD(NPC(FUND)+K1+1)*FAC2
              Y2=PLD(NPC(FUND)+K1+3)*FAC2
              IF(X1<ZERO.AND.X2<ZERO.AND.S1<ZERO.AND.S2<ZERO)THEN
              IF (Y2>=T1 .AND. Y1<=T2 .AND. X2>=S1 .AND. X1<=S2) THEN
                DYDX = (Y2-Y1) / (X2-X1)
                DTDS = (T2-T1) / (S2-S1)
                IF (DYDX > DTDS) THEN
                  !SX = (DTDS*S2-DYDX*X2-T2+Y2) / (DTDS-DYDX)
                  !TY =  T2 + DTDS*(SX - S2)
                  SX = (T1-Y1-DTDS*S1+DYDX*X1) / (DYDX-DTDS)
                  TY =  T1 + DTDS*(SX - S1)
                  IF (TY>=Y1 .AND. TY<=Y2 .AND. SX>=X1 .AND. SX<=X2.AND.SX/=ZERO)THEN 
                    XINT2 = SX
                    YINT2 = TY
                    GOTO 362
                  ENDIF
                ENDIF
              ENDIF
             ENDIF
            ENDDO
          ENDDO
 362  CONTINUE
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  FUNC_INTERS_C                 source/tools/curve/func_inters.F
Chd|-- called by -----------
Chd|        LAW88_UPD                     source/materials/mat/mat088/law88_upd.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE FUNC_INTERS_C(TITR,MAT_ID  ,FUNC,FUND,FAC1,FAC2,NPC,PLD,XINC,YINC  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*nchartitle :: TITR
      !INTEGER  , DIMENSION(NFUNC)      :: IFUNC,FUNC_ID
      INTEGER     FUNC,FUND,NPC(*) 
      INTEGER  :: MAT_ID
      my_real  
     .            XINC,YINC,FAC1,FAC2,PLD(*)
!      TYPE(TTABLE) TABLE(*)
C-----------------------------------------------
      INTENT(IN)    :: TITR,FUNC,FUND,MAT_ID,NPC,PLD,FAC1,FAC2
      INTENT(INOUT) :: XINC,YINC 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,NP1,NP2,J1,K,K1
      my_real 
     .      S1,S2,T1,T2,X1,X2,Y1,Y2,SX,TY,DYDX,DTDS,DET,B1,B2,X,Y
C=======================================================================
        !NFUNC =  IPM(10,IMAT)+IPM(6,IMAT)in updmat.f
        !IFUNC => IPM(10+1:10+NFUNC,IMAT) in updmat.f
! Calculation intersection
          XINC = ZERO
          YINC = ZERO
          NP1  = (NPC(FUNC+1)-NPC(FUNC))*HALF  
          NP2  = (NPC(FUND+1)-NPC(FUND))*HALF 
          DO J=2,NP1+1
            J1=2*(J-2)
            S1=PLD(NPC(FUNC)+J1)
            T1=PLD(NPC(FUNC)+J1+1)*FAC1
            DO K=2,NP2+1
              K1=2*(K-2)
              X1=PLD(NPC(FUND)+K1)
              Y1=PLD(NPC(FUND)+K1+1)*FAC2
              IF(X1 == S1 .AND. Y1 == T1 .AND.X1 < ZERO)THEN
                    XINC = X1
                    YINC = Y1
                    GOTO 350
              ENDIF
             ENDDO
          ENDDO
          DO J=2,NP1
            J1=2*(J-2)
            S2=PLD(NPC(FUNC)+J1)
            S1=PLD(NPC(FUNC)+J1+2)
            T2=PLD(NPC(FUNC)+J1+1)*FAC1
            T1=PLD(NPC(FUNC)+J1+3)*FAC1
            IF(S1 < ZERO .OR. S2 < ZERO)  THEN
               DO K=2,NP2
                 K1=2*(K-2)
                 X2=PLD(NPC(FUND)+K1)
                 X1=PLD(NPC(FUND)+K1+2)
                 Y2=PLD(NPC(FUND)+K1+1)*FAC2
                 Y1=PLD(NPC(FUND)+K1+3)*FAC2
                 IF(X1 < ZERO .OR. X2 < ZERO) THEN
                   DYDX = (Y2-Y1) / (X2-X1)
                   DTDS = (T2-T1) / (S2-S1)
                   DET = DTDS - DYDX
                    IF(DET /= ZERO ) THEN
                         B1  = Y1 - DYDX*X1
                         B2  = T1 - DTDS*S1
                         X = (B1 - B2) / DET
                         Y = (-DYDX*B2 + B1*DTDS)/DET
                         IF(X <= X1 .AND. X >= X2 .AND. X <= S1 .AND. X  >= S2 .AND.
     .                      Y <= Y1 .AND. Y >= Y2 .AND. Y <= T1 .AND. Y  >= T2 ) THEN
                            XINC = X
                            YINC = Y
                            GOTO 350
                         ENDIF
                    ENDIF 
                 ENDIF  
               ENDDO ! K
             ENDIF ! S1, S2    
          ENDDO
 350      CONTINUE
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  TABLE_INTERS                  source/tools/curve/func_inters.F
Chd|-- called by -----------
Chd|        LAW119_UPD                    source/materials/mat/mat119/law119_upd.F
Chd|-- calls ---------------
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE TABLE_INTERS(TABLE,FUNC1,FUNC2,FAC1,FAC2,XINT,YINT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  :: FUNC1,FUNC2
      my_real  :: XINT,YINT,FAC1,FAC2
      TYPE(TTABLE), DIMENSION(NTABLE) ::  TABLE
C-----------------------------------------------
      INTENT(IN)    :: FUNC1,FUNC2,FAC1,FAC2
      INTENT(INOUT) :: XINT,YINT 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: J,K,NP1,NP2,NDIM,FOUND
      my_real :: S1,S2,T1,T2,X1,X2,Y1,Y2,AX,BX,AY,BY,CX,CY,DM,ALPHA,BETA
C=======================================================================
c     Check common points between 2 curves => intersection
      NDIM   = TABLE(FUNC1)%NDIM
      NP1    = SIZE(TABLE(FUNC1)%X(1)%VALUES)
      NP2    = SIZE(TABLE(FUNC2)%X(1)%VALUES)
      XINT  = ZERO
      YINT  = ZERO
      FOUND = 0
      DO J=2,NP1
        S1 = TABLE(FUNC1)%X(1)%VALUES(J)
        T1 = TABLE(FUNC1)%Y%VALUES(J)*FAC1
        DO K=2,NP2
          X1 = TABLE(FUNC2)%X(1)%VALUES(K)
          Y1 = TABLE(FUNC2)%Y%VALUES(K)*FAC2
          IF (S1 > ZERO .and. X1 == S1 .and. Y1 == T1) THEN
            XINT  = X1
            YINT  = Y1
            FOUND = 1
            EXIT
          ENDIF
        ENDDO
        IF (FOUND == 1) EXIT
      ENDDO
c     Check intersection of curve segments
      IF (FOUND == 0) THEN
        DO J=2,NP1
          S1 = TABLE(FUNC1)%X(1)%VALUES(J-1)
          S2 = TABLE(FUNC1)%X(1)%VALUES(J)
          T1 = TABLE(FUNC1)%Y%VALUES(J-1)*FAC1
          T2 = TABLE(FUNC1)%Y%VALUES(J)*FAC1
          DO K=2,NP2
            X1 = TABLE(FUNC2)%X(1)%VALUES(K-1)
            X2 = TABLE(FUNC2)%X(1)%VALUES(K)
            Y1 = TABLE(FUNC2)%Y%VALUES(K-1)*FAC2
            Y2 = TABLE(FUNC2)%Y%VALUES(K)*FAC2
            IF (X2 < S1 .or. S2 < X1) CYCLE
            AX = X2 - X1
            AY = Y2 - Y1
            BX = S1 - S2
            BY = T1 - T2
            DM = AY*BX - AX*BY
            IF (DM /= ZERO) THEN  ! check if segments are not parallel
              CX = S1 - X1
              CY = T1 - Y1
              ALPHA = (BX * CY - BY * CX) / DM
              BETA  = (AX * CY - AY * CX) / DM
              IF (ALPHA >= ZERO .and. ALPHA < ONE .and.
     .            BETA  <= ZERO .and. BETA  >-ONE .and.
     .            S1 > ZERO) THEN
                XINT = X1 + ALPHA * AX
                YINT = Y1 + ALPHA * AY
                FOUND = 1
                EXIT
              ENDIF
            ENDIF
          ENDDO
          IF (FOUND == 1) EXIT
        ENDDO
      END IF
c-----------
      RETURN
      END
