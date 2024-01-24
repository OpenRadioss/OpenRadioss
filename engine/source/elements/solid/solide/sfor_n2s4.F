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
Chd|  SFOR_N2S4                     source/elements/solid/solide/sfor_n2s4.F
Chd|-- called by -----------
Chd|        S8FOR_DISTOR                  source/elements/solid/solide/s8for_distor.F
Chd|-- calls ---------------
Chd|        SSORT_N4                      source/elements/solid/solide/ssort_n4.F
Chd|====================================================================
      SUBROUTINE SFOR_N2S4( XI,      YI,     ZI,   STIF,     
     .                      X1,      X2,     X3,     X4,
     .                      Y1,      Y2,     Y3,     Y4,
     .                      Z1,      Z2,     Z3,     Z4,
     .                   FOR_T1, FOR_T2, FOR_T3, FOR_T4,
     .                   FORC_N,    LL ,  IFCTL,  IFC1 ,
     .                   PENMIN, PENREF,  MARGE,  FQMAX,
     .                   STIF0,   NEL  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT (IN)  :: NEL
      INTEGER, INTENT (OUT) :: IFCTL
      INTEGER, DIMENSION(MVSIZ), INTENT (IN) :: IFC1
      my_real,                   INTENT (IN) :: FQMAX
      my_real, DIMENSION(MVSIZ), INTENT (IN) :: 
     .                 X1,      X2,     X3,     X4,
     .                 Y1,      Y2,     Y3,     Y4,
     .                 Z1,      Z2,     Z3,     Z4,
     .                 XI,      YI,     ZI, PENMIN,
     .             PENREF,      LL,   STIF0, MARGE
      my_real, DIMENSION(MVSIZ), INTENT (INOUT) :: STIF
      my_real, DIMENSION(MVSIZ,3), INTENT (INOUT) :: FORC_N,
     .                     FOR_T1, FOR_T2, FOR_T3, FOR_T4
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IFCTL1,ITG(MVSIZ),IFC2(MVSIZ),ie
C                                                                     12
      my_real
     .   NX(MVSIZ),NY(MVSIZ),NZ(MVSIZ),
     .   XSC(MVSIZ),YSC(MVSIZ),ZSC(MVSIZ),PENE(MVSIZ),
     .   XA(MVSIZ),YA(MVSIZ),ZA(MVSIZ),HJ(MVSIZ,4),
     .   XB(MVSIZ),YB(MVSIZ),ZB(MVSIZ),FN(MVSIZ),
     .   XC(MVSIZ),YC(MVSIZ),ZC(MVSIZ),AREA(MVSIZ),
     .   LA(MVSIZ),LB(MVSIZ),LC(MVSIZ),FKT,
     .   RX, RY, RZ, SX, SY, SZ,VXC,VYC,VZC,
     .   X42,Y42, Z42, X31, Y31, Z31,FX,FY,FZ,
     .   SAX,SAY,SAZ,SBX,SBY,SBZ,SCX,SCY,SCZ,
     .   TRX,TRY,TRZ,TSX,TSY,TSZ,TTX,TTY,TTZ,
     .   TR2,TS2,TT2,AAA,BBB,VR,VS,VT,NNX,NNY,NNZ,
     .   XAB,XBC,XCA,YAB,YBC,YCA,ZAB,ZBC,ZCA,
     .   XIA,  XIB,  XIC, YIA,  YIB,  YIC,
     .   ZIA,  ZIB,  ZIC, H0,NORM,S2,FAC,
     .   F_Q,F_C,KTS,ZEROM,TX,TY,TZ,PENE3,PENE4,PENDR,LJ
C----------------------------
         IFC2(1:NEL) = IFC1(1:NEL)
         CALL SSORT_N4(XI,      YI,     ZI ,   MARGE,    
     .                 X1,     X2,      X3,      X4,
     .                 Y1,     Y2,      Y3,      Y4,
     .                 Z1,     Z2,      Z3,      Z4,
     .               IFC2,  STIF0,     NEL)
        IFCTL = 0
        IFCTL1 = 0
C-------diff in velocity as 1er sorting
         DO I=1,NEL
           IF (IFC2(I)==0) CYCLE
           XSC(I) = FOURTH*(X1(I)+X2(I)+X3(I)+X4(I))
           YSC(I) = FOURTH*(Y1(I)+Y2(I)+Y3(I)+Y4(I))
           ZSC(I) = FOURTH*(Z1(I)+Z2(I)+Z3(I)+Z4(I))
           IFCTL1=1
         END DO
C
       IF (IFCTL1==1) THEN
         ZEROM = -TWO*EM03
         ITG(1:NEL) = 1
         PENE(1:NEL) = ZERO
         DO I=1,NEL
           IF (IFC2(I)==0) CYCLE
           RX =X2(I)+X3(I)-X1(I)-X4(I)
           RY =Y2(I)+Y3(I)-Y1(I)-Y4(I)
           RZ =Z2(I)+Z3(I)-Z1(I)-Z4(I)
           SX =X3(I)+X4(I)-X1(I)-X2(I)
           SY =Y3(I)+Y4(I)-Y1(I)-Y2(I)
           SZ =Z3(I)+Z4(I)-Z1(I)-Z2(I)
           NX(I)=RY*SZ - RZ*SY
           NY(I)=RZ*SX - RX*SZ
           NZ(I)=RX*SY - RY*SX
           AREA(I) = NX(I)*NX(I)+NY(I)*NY(I)+NZ(I)*NZ(I)
           NORM=ONE/MAX(EM20,SQRT(AREA(I)))
           NX(I)=NX(I)*NORM
           NY(I)=NY(I)*NORM
           NZ(I)=NZ(I)*NORM
           BBB =(XSC(I)-XI(I))*NX(I) +
     .          (YSC(I)-YI(I))*NY(I) +
     .          (ZSC(I)-ZI(I))*NZ(I) -PENMIN(I)
           PENE(I) = MAX(ZERO,-BBB)            
           IF (FOUR*AREA(I)<PENMIN(I)*LL(I)) 
     .         PENE(I)=MIN(PENE(I),EM01*PENMIN(I))
           IF (AREA(I)<EM20) PENE(I) =ZERO
         ENDDO
! 4-------3
! |\     /|
! | \ 3 / |
! |  \ /2 |
! |   5   |
! | 4/ \  |
! | / 1 \ |
! |/     \|
! 1-------2
         DO I=1,NEL
           IF(PENE(I) == ZERO ) CYCLE
           IF(IFC2(I)>=3) THEN ! degenerated quad
             SELECT CASE (IFC2(I))
               CASE (3)
                 XB(I) = X1(I)   
                 YB(I) = Y1(I)    
                 ZB(I) = Z1(I)   
                 XC(I) = X2(I)   
                 YC(I) = Y2(I)    
                 ZC(I) = Z2(I)   
                 XA(I) = X3(I)   
                 YA(I) = Y3(I)    
                 ZA(I) = Z3(I) 
               CASE (4)
                 XB(I) = X2(I)   
                 YB(I) = Y2(I)    
                 ZB(I) = Z2(I)   
                 XC(I) = X3(I)   
                 YC(I) = Y3(I)    
                 ZC(I) = Z3(I)   
                 XA(I) = X4(I)   
                 YA(I) = Y4(I)    
                 ZA(I) = Z4(I) 
               CASE (5)
                 XB(I) = X4(I)   
                 YB(I) = Y4(I)    
                 ZB(I) = Z4(I)   
                 XC(I) = X1(I)   
                 YC(I) = Y1(I)    
                 ZC(I) = Z1(I)   
                 XA(I) = X2(I)   
                 YA(I) = Y2(I)    
                 ZA(I) = Z2(I) 
               CASE (6)
                 XB(I) = X3(I)   
                 YB(I) = Y3(I)    
                 ZB(I) = Z3(I)   
                 XC(I) = X4(I)   
                 YC(I) = Y4(I)    
                 ZC(I) = Z4(I)   
                 XA(I) = X1(I)   
                 YA(I) = Y1(I)    
                 ZA(I) = Z1(I) 
             END SELECT
!             PENE(I) = EM01*PENE(I)
           ELSE
            XA(I) = XSC(I)   
            YA(I) = YSC(I)    
            ZA(I) = ZSC(I) 
C-------- if other sub-tria recompute normal      
            BBB =(X3(I)-XI(I))*NX(I) +
     .           (Y3(I)-YI(I))*NY(I) +
     .           (Z3(I)-ZI(I))*NZ(I) -PENMIN(I)
            PENE3 = MAX(ZERO,-BBB)            
            BBB =(X4(I)-XI(I))*NX(I) +
     .           (Y4(I)-YI(I))*NY(I) +
     .           (Z4(I)-ZI(I))*NZ(I) -PENMIN(I)
            PENE4 = MAX(ZERO,-BBB)            
            IF (PENE3>PENE(I).AND.PENE4>PENE(I)) THEN
              ITG(I) = 3   
              XB(I) = X3(I)   
              YB(I) = Y3(I)    
              ZB(I) = Z3(I)   
              XC(I) = X4(I)   
              YC(I) = Y4(I)    
              ZC(I) = Z4(I)   
            ELSEIF (PENE3>PENE(I)) THEN
              ITG(I) = 2
              XB(I) = X2(I)   
              YB(I) = Y2(I)    
              ZB(I) = Z2(I)   
              XC(I) = X3(I)   
              YC(I) = Y3(I)    
              ZC(I) = Z3(I)   
            ELSEIF (PENE4>PENE(I)) THEN
              ITG(I) = 4
              XB(I) = X4(I)   
              YB(I) = Y4(I)    
              ZB(I) = Z4(I)   
              XC(I) = X1(I)   
              YC(I) = Y1(I)    
              ZC(I) = Z1(I)   
            ELSE 
             XB(I) = X1(I)   
             YB(I) = Y1(I)    
             ZB(I) = Z1(I)   
             XC(I) = X2(I)   
             YC(I) = Y2(I)    
             ZC(I) = Z2(I)   
            END IF
           END IF ! IFC2(I)>=3
         ENDDO
         DO I=1,NEL
           IF(PENE(I) == ZERO.OR.ITG(I)==1) CYCLE
           RX =XB(I)-XA(I)
           RY =YB(I)-YA(I)
           RZ =ZB(I)-ZA(I)
           SX =XC(I)-XA(I)
           SY =YC(I)-YA(I)
           SZ =ZC(I)-ZA(I)
           NX(I)=RY*SZ - RZ*SY
           NY(I)=RZ*SX - RX*SZ
           NZ(I)=RX*SY - RY*SX
           AREA(I) = NX(I)*NX(I)+NY(I)*NY(I)+NZ(I)*NZ(I)
           NORM=ONE/MAX(EM20,SQRT(AREA(I)))
           NX(I)=NX(I)*NORM
           NY(I)=NY(I)*NORM
           NZ(I)=NZ(I)*NORM
           BBB = (X3(I)-XI(I))*NX(I) +
     .           (Y3(I)-YI(I))*NY(I) +
     .           (Z3(I)-ZI(I))*NZ(I) -PENMIN(I)
           PENE(I) = MAX(ZERO,-BBB)  
           IF (FOUR*AREA(I)<PENMIN(I)*LL(I)) 
     .         PENE(I)=MIN(PENE(I),EM01*PENMIN(I))
         ENDDO
         DO I=1,NEL
           IF(PENE(I) == ZERO) CYCLE
           XAB = XB(I)-XA(I)
           YAB = YB(I)-YA(I)
           ZAB = ZB(I)-ZA(I)
           XBC = XC(I)-XB(I)
           YBC = YC(I)-YB(I)
           ZBC = ZC(I)-ZB(I)
           XCA = XA(I)-XC(I)
           YCA = YA(I)-YC(I)
           ZCA = ZA(I)-ZC(I)
           
           XIA = XA(I)-XI(I)
           YIA = YA(I)-YI(I)
           ZIA = ZA(I)-ZI(I)
           XIB = XB(I)-XI(I)
           YIB = YB(I)-YI(I)
           ZIB = ZB(I)-ZI(I)
           XIC = XC(I)-XI(I)
           YIC = YC(I)-YI(I)
           ZIC = ZC(I)-ZI(I)
           SX = - YAB*ZCA + ZAB*YCA
           SY = - ZAB*XCA + XAB*ZCA
           SZ = - XAB*YCA + YAB*XCA
           S2 = SX*SX+SY*SY+SZ*SZ
           SAX = YIB*ZIC - ZIB*YIC
           SAY = ZIB*XIC - XIB*ZIC
           SAZ = XIB*YIC - YIB*XIC
           LA(I) = (SX*SAX+SY*SAY+SZ*SAZ)/S2
           SBX = YIC*ZIA - ZIC*YIA
           SBY = ZIC*XIA - XIC*ZIA
           SBZ = XIC*YIA - YIC*XIA
           LB(I) = (SX*SBX+SY*SBY+SZ*SBZ)/S2
           LC(I) = ONE - LA(I) - LB(I)
           LJ = MIN(LA(I),LB(I),LC(I))
           IF (LJ<ZEROM) PENE(I)=MIN(PENE(I),PENMIN(I))
           IF(LA(I)<ZERO)THEN
             IF(LB(I)<ZERO)THEN
               LA(I) = ZERO
               LB(I) = ZERO
               LC(I) = ONE
             ELSEIF(LC(I)<ZERO)THEN
               LC(I) = ZERO
               LA(I) = ZERO
               LB(I) = ONE
             ELSE
               LA(I) = ZERO
               AAA = LB(I) + LC(I)
               LB(I) = LB(I)/AAA
               LC(I) = LC(I)/AAA
             ENDIF
           ELSEIF(LB(I)<ZERO)THEN
             IF(LC(I)<ZERO)THEN
               LB(I) = ZERO
               LC(I) = ZERO
               LA(I) = ONE
             ELSE
               LB(I) = ZERO
               AAA = LC(I) + LA(I)
               LC(I) = LC(I)/AAA
               LA(I) = LA(I)/AAA
             ENDIF
           ELSEIF(LC(I)<ZERO)THEN
               LC(I) = ZERO
               AAA = LA(I) + LB(I)
               LA(I) = LA(I)/AAA
               LB(I) = LB(I)/AAA
           ENDIF
         ENDDO
         DO I=1,NEL
           IF(PENE(I) == ZERO) CYCLE
           IF(IFC2(I)>=3) THEN ! degenerated quad
             SELECT CASE (IFC2(I))
               CASE (3)
                 HJ(I,1) = LB(I)
                 HJ(I,2) = LC(I) 
                 HJ(I,3) = LA(I)
                 HJ(I,4) = ZERO 
               CASE (4)
                 HJ(I,2) = LB(I)
                 HJ(I,3) = LC(I) 
                 HJ(I,4) = LA(I)
                 HJ(I,1) = ZERO 
               CASE (5)
                 HJ(I,4) = LB(I)
                 HJ(I,1) = LC(I) 
                 HJ(I,2) = LA(I)
                 HJ(I,3) = ZERO 
               CASE (6)
                 HJ(I,3) = LB(I)
                 HJ(I,4) = LC(I) 
                 HJ(I,1) = LA(I)
                 HJ(I,2) = ZERO 
             END SELECT
           ELSE
             H0    = FOURTH * LA(I)
             SELECT CASE (ITG(I))
               CASE (1)
                  HJ(I,1) = H0 + LB(I)
                  HJ(I,2) = H0 + LC(I) 
                  HJ(I,3) = H0 
                  HJ(I,4) = H0  
               CASE (2)
                  HJ(I,2) = H0 + LB(I)
                  HJ(I,3) = H0 + LC(I) 
                  HJ(I,4) = H0 
                  HJ(I,1) = H0  
               CASE (3)
                  HJ(I,3) = H0 + LB(I)
                  HJ(I,4) = H0 + LC(I) 
                  HJ(I,1) = H0 
                  HJ(I,2) = H0  
               CASE (4)
                  HJ(I,4) = H0 + LB(I)
                  HJ(I,1) = H0 + LC(I) 
                  HJ(I,2) = H0 
                  HJ(I,3) = H0  
             END SELECT
           END IF
         ENDDO
         F_Q = EP02
         DO I=1,NEL
           IF(PENE(I) == ZERO) CYCLE
           PENDR  = (PENE(I)/PENREF(I))**2
           FAC = MIN(FQMAX,F_Q*PENDR)
           FN(I) = (FAC+ONE)*STIF0(I)*PENE(I)
           FKT = ONE+THREE*FAC
           STIF(I) =MAX(STIF(I),FKT*STIF0(I))
         ENDDO
        DO I=1,NEL
          IF (PENE(I) ==ZERO) CYCLE
          FX = NX(I)*FN(I)
          FY = NY(I)*FN(I)
          FZ = NZ(I)*FN(I)
          FORC_N(I,1) = FORC_N(I,1) - FX
          FORC_N(I,2) = FORC_N(I,2) - FY
          FORC_N(I,3) = FORC_N(I,3) - FZ
          FOR_T1(I,1) = FOR_T1(I,1) + FX*HJ(I,1)
          FOR_T1(I,2) = FOR_T1(I,2) + FY*HJ(I,1)
          FOR_T1(I,3) = FOR_T1(I,3) + FZ*HJ(I,1)
          FOR_T2(I,1) = FOR_T2(I,1) + FX*HJ(I,2)
          FOR_T2(I,2) = FOR_T2(I,2) + FY*HJ(I,2)
          FOR_T2(I,3) = FOR_T2(I,3) + FZ*HJ(I,2)
          FOR_T3(I,1) = FOR_T3(I,1) + FX*HJ(I,3)
          FOR_T3(I,2) = FOR_T3(I,2) + FY*HJ(I,3)
          FOR_T3(I,3) = FOR_T3(I,3) + FZ*HJ(I,3)
          FOR_T4(I,1) = FOR_T4(I,1) + FX*HJ(I,4)
          FOR_T4(I,2) = FOR_T4(I,2) + FY*HJ(I,4)
          FOR_T4(I,3) = FOR_T4(I,3) + FZ*HJ(I,4)
        ENDDO
       END IF ! (IFCTL1==1) THEN
c-----------
      RETURN
      END
 