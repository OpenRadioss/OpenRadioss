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
Chd|  SFOR_N2S3                     source/elements/solid/solide4/sfor_n2s3.F
Chd|-- called by -----------
Chd|        SFOR_N2STRIA                  source/elements/solid/solide4/sfor_n2stria.F
Chd|        SFOR_N2STRIA2                 source/elements/solid/solide10/sfor_n2stria2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SFOR_N2S3( XI,     YI,     ZI, FORC_N,     
     .                      X1,     Y1,     Z1, FOR_T1, 
     .                      X2,     Y2,     Z2, FOR_T2,
     .                      X3,     Y3,     Z3, FOR_T3,
     .                    STIF,   IFC1,  IFCTL, PENMIN,
     .                  PENREF, FKTMAX,    LL , FQMAX ,
     .                     NEL)
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
      my_real, DIMENSION(MVSIZ), INTENT (IN) ::  X1, X2, X3,
     .                                           Y1, Y2, Y3,
     .                                           Z1, Z2, Z3,
     .                                           XI, YI, ZI
      my_real, INTENT (IN) :: FQMAX
      my_real, DIMENSION(MVSIZ), INTENT (INOUT) :: FKTMAX
      my_real, DIMENSION(MVSIZ), INTENT (IN) :: STIF,LL,PENMIN, PENREF
      my_real, DIMENSION(MVSIZ,3), INTENT (INOUT) :: FORC_N, FOR_T1, FOR_T2, FOR_T3
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IFCTL1,ie
C                                                                     12
      my_real NX(MVSIZ),NY(MVSIZ),NZ(MVSIZ),FN(MVSIZ),
     .        XSC(MVSIZ),YSC(MVSIZ),ZSC(MVSIZ),PENE(MVSIZ),
     .        XA(MVSIZ),YA(MVSIZ),ZA(MVSIZ),AREA(MVSIZ),
     .        XB(MVSIZ),YB(MVSIZ),ZB(MVSIZ),
     .        XC(MVSIZ),YC(MVSIZ),ZC(MVSIZ),
     .        LA(MVSIZ),LB(MVSIZ),LC(MVSIZ),STIFKT(MVSIZ),
     .        RX, RY, RZ, SX, SY, SZ,VXC,VYC,VZC,
     .        X42,Y42, Z42, X31, Y31, Z31,FX,FY,FZ,
     .        SAX,SAY,SAZ,SBX,SBY,SBZ,SCX,SCY,SCZ,
     .        TRX,TRY,TRZ,TSX,TSY,TSZ,TTX,TTY,TTZ,
     .        TR2,TS2,TT2,AAA,BBB,VR,VS,VT,NNX,NNY,NNZ,
     .        XAB,XBC,XCA,YAB,YBC,YCA,ZAB,ZBC,ZCA,
     .        XIA,  XIB,  XIC, YIA,  YIB,  YIC,
     .        ZIA,  ZIB,  ZIC, H0,NORM,S2,FAC,S2MIN,LJ,
     .        F_Q,F_C,KTS,ZEROM,TX,TY,TZ,PENDR
C----------------------------
        IFCTL = 0
        IFCTL1 = 0
C-------diff in velocity as 1er sorting
         DO I=1,NEL
           IF (IFC1(I)==0) CYCLE
           IFCTL1=1
         END DO
C
       IF (IFCTL1==1) THEN
         PENE(1:NEL) =ZERO
         ZEROM = -TWO*EM03
         DO I=1,NEL
           IF (IFC1(I)==0) CYCLE
           RX =X2(I)-X1(I)
           RY =Y2(I)-Y1(I)
           RZ =Z2(I)-Z1(I)
           SX =X3(I)-X1(I)
           SY =Y3(I)-Y1(I)
           SZ =Z3(I)-Z1(I)
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
           IF (AREA(I)<PENMIN(I)*LL(I)) PENE(I)=MIN(PENE(I),EM01*PENMIN(I))
         ENDDO
!     3  A 
!    / \
!   /   \
!  /     \
! 1-------2 C
         DO I=1,NEL
           IF(PENE(I) == ZERO) CYCLE
C-------- if other sub-tria ? still using elementary nodal normal      
            XA(I) = X3(I)   
            YA(I) = Y3(I)    
            ZA(I) = Z3(I) 
            XB(I) = X1(I)   
            YB(I) = Y1(I)    
            ZB(I) = Z1(I)   
            XC(I) = X2(I)   
            YC(I) = Y2(I)    
            ZC(I) = Z2(I)   
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
         F_Q = EP02
         DO I=1,NEL
           IF(PENE(I) == ZERO) CYCLE
           PENDR  = (PENE(I)/PENREF(I))**2
           FAC = MIN(FQMAX,F_Q*PENDR)
           FKTMAX(I) =MAX(FKTMAX(I),(ONE+THREE*FAC))
           FN(I) = (FAC+ONE)*STIF(I)*PENE(I)
         ENDDO
        DO I=1,NEL
          IF (PENE(I) ==ZERO) CYCLE
          FX = NX(I)*FN(I)
          FY = NY(I)*FN(I)
          FZ = NZ(I)*FN(I)
          FORC_N(I,1) = FORC_N(I,1) - FX
          FORC_N(I,2) = FORC_N(I,2) - FY
          FORC_N(I,3) = FORC_N(I,3) - FZ
          FOR_T1(I,1) = FOR_T1(I,1) + FX*LB(I)
          FOR_T1(I,2) = FOR_T1(I,2) + FY*LB(I)
          FOR_T1(I,3) = FOR_T1(I,3) + FZ*LB(I)
          FOR_T2(I,1) = FOR_T2(I,1) + FX*LC(I)
          FOR_T2(I,2) = FOR_T2(I,2) + FY*LC(I)
          FOR_T2(I,3) = FOR_T2(I,3) + FZ*LC(I)
          FOR_T3(I,1) = FOR_T3(I,1) + FX*LA(I)
          FOR_T3(I,2) = FOR_T3(I,2) + FY*LA(I)
          FOR_T3(I,3) = FOR_T3(I,3) + FZ*LA(I)
          IFCTL = 1
        ENDDO
       END IF ! (IFCTL1==1) THEN
c-----------
      RETURN
      END
