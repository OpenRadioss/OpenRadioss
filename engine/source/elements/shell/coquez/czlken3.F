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
Chd|  CZLKEN3                       source/elements/shell/coquez/czlken3.F
Chd|-- called by -----------
Chd|        CZKE3                         source/elements/shell/coquez/czke3.F
Chd|        CZKEL3                        source/elements/shell/coquez/czkel3.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE CZLKEN3(JFT ,JLT  ,VOL  ,THK0 ,THK2 ,
     2                    HM  ,HZ   ,A_I  ,PX1  ,PX2  ,
     3                   PY1  ,PY2  ,HXX  ,HYY  ,HXY  ,
     4                   PH1  ,PH2  ,Z1   ,NPLAT,IPLAT,DHZ  ,
     5           K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     6           M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     7           MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33, 
     8           MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34,
     9           IDRIL ) 
C--------------------------------------------------------------------------------------------------
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
      INTEGER JFT,JLT,NPLAT,IPLAT(*),IDRIL
      MY_REAL 
     .    VOL(*),THK0(*),THK2(*),A_I(*),Z1(*),HM(MVSIZ,4),HZ(*),
     .    PX1(*)  ,PX2(*)   ,PY1(*)  ,PY2(*)  ,PH1(*)  ,PH2(*)  ,
     .    HXX(*),HYY(*),HXY(*),
     .    K11(3,3,*),K12(3,3,*),K13(3,3,*),K14(3,3,*),
     .    K22(3,3,*),K23(3,3,*),K24(3,3,*),K33(3,3,*),
     .    M11(3,3,*),M12(3,3,*),M13(3,3,*),M14(3,3,*),
     .    M22(3,3,*),M23(3,3,*),M24(3,3,*),M33(3,3,*),
     .    MF11(3,3,*),MF12(3,3,*),MF13(3,3,*),MF14(3,3,*),
     .    MF22(3,3,*),MF23(3,3,*),MF24(3,3,*),MF33(3,3,*),
     .    FM12(3,3,*),FM13(3,3,*),FM14(3,3,*),
     .    FM23(3,3,*),FM24(3,3,*),FM34(3,3,*),
     .    K34(3,3,*),K44(3,3,*),M34(3,3,*),M44(3,3,*),
     .    MF34(3,3,*),MF44(3,3,*),DHZ(*)
C---------------|[KIJ][MFIJ]|----
C-----KE(6x6)=  |           |
C---------------|[FMIJ]{MIJ]|----
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER EP,I,J,NF,M
      MY_REAL 
     .    DM(2,2,MVSIZ),C1,C2,GM(MVSIZ),
     .    G11(MVSIZ),G12(MVSIZ),G13(MVSIZ),
     .    G14(MVSIZ),G22(MVSIZ),G23(MVSIZ),
     .    G24(MVSIZ),G33(MVSIZ),G34(MVSIZ),G44(MVSIZ),
     .    CHM(2,2,MVSIZ),CHF(2,2,MVSIZ),FACF(MVSIZ),FAC,ZR(MVSIZ),
     .    GAMA(4,MVSIZ),CXX,CYY,CXY,C11,C22,C12,CX1,CX2,CY1,CY2
C-----------gama(I)=hI/4-PH(I), PH:anti-sym comme bxI------
       NF=NPLAT+1
C-----------Attention Matrice sym Kii ne calcul que la moitie---------72
#include "vectorize.inc"
       DO M=JFT,JLT 
        EP=IPLAT(M)
        C2=VOL(EP)
        C1=THK2(EP)*C2
        DM(1,1,M)=HM(EP,1)*C2
        DM(2,2,M)=HM(EP,2)*C2
        DM(1,2,M)=HM(EP,3)*C2
        DM(2,1,M)=DM(1,2,M)
        GM(M) =HM(EP,4)*C2
        FACF(M)=ONE_OVER_12*THK2(EP)
C        DHZ(M)= HZ(EP)*C1
        ZR(M) = Z1(EP)
       ENDDO
C------------------shear est constante----
       DO EP=JFT,JLT
        CHM(1,1,EP)=DM(1,1,EP)*HXX(EP)
C+GM(EP)*HYY(EP)
        CHM(2,2,EP)=DM(2,2,EP)*HYY(EP)
C+GM(EP)*HXX(EP)
        CHM(1,2,EP)=DM(1,2,EP)*HXY(EP)
C+GM(EP)*HXY(EP)
        CHM(2,1,EP)=CHM(1,2,EP)
C------------------pour etre coherant avec QEPH explicite---
        CHF(1,1,EP)=FACF(EP)*CHM(2,2,EP)
C+GM(EP)*HXX(EP))
        CHF(2,2,EP)=FACF(EP)*CHM(1,1,EP)
C+GM(EP)*HYY(EP))
        CHF(1,2,EP)=-FACF(EP)*CHM(1,2,EP)
C+GM(EP)*HXY(EP))
c        FAC = FACF(EP)*GM(EP)
c        CHF(1,1,EP)=CHF(1,1,EP)+FAC*HXX(EP)
c        CHF(2,2,EP)=CHF(2,2,EP)+FAC*HYY(EP)
c        CHF(1,2,EP)=CHF(1,2,EP)-FAC*HXY(EP)
C
        CHF(2,1,EP)=CHF(1,2,EP)
       ENDDO
C 
      IF (IDRIL.GT.0) THEN
       DO EP=JFT,JLT
        CHM(1,1,EP)=CHM(1,1,EP)+GM(EP)*HYY(EP)
        CHM(2,2,EP)=CHM(2,2,EP)+GM(EP)*HXX(EP)
        CHM(1,2,EP)=CHM(1,2,EP)+GM(EP)*HXY(EP)
        CHM(2,1,EP)=CHM(1,2,EP)
C----------------------------
        FAC = FACF(EP)*GM(EP)
        CHF(1,1,EP)=CHF(1,1,EP)+FAC*HXX(EP)
        CHF(2,2,EP)=CHF(2,2,EP)+FAC*HYY(EP)
        CHF(1,2,EP)=CHF(1,2,EP)-FAC*HXY(EP)
        CHF(2,1,EP)=CHF(1,2,EP)
       ENDDO
      END IF !(IDRIL.GT.0) THEN
C--------------gamaI--------
       DO EP=JFT,JLT
        GAMA(1,EP)=FOURTH-PH1(EP)
        GAMA(2,EP)=-FOURTH-PH2(EP)
        GAMA(3,EP)=FOURTH+PH1(EP)
        GAMA(4,EP)=-FOURTH+PH2(EP)
        G11(EP) =GAMA(1,EP)*GAMA(1,EP) 
        G12(EP) =GAMA(1,EP)*GAMA(2,EP) 
        G13(EP) =GAMA(1,EP)*GAMA(3,EP) 
        G14(EP) =GAMA(1,EP)*GAMA(4,EP) 
        G22(EP) =GAMA(2,EP)*GAMA(2,EP) 
        G23(EP) =GAMA(2,EP)*GAMA(3,EP) 
        G24(EP) =GAMA(2,EP)*GAMA(4,EP) 
        G33(EP) =GAMA(3,EP)*GAMA(3,EP) 
        G34(EP) =GAMA(3,EP)*GAMA(4,EP) 
        G44(EP) =GAMA(4,EP)*GAMA(4,EP) 
       ENDDO
C-------
       DO I=1,2
       DO J=I,2
        DO EP=JFT,JLT
         K11(I,J,EP)=K11(I,J,EP)+CHM(I,J,EP)*G11(EP)
         K22(I,J,EP)=K22(I,J,EP)+CHM(I,J,EP)*G22(EP)
         K33(I,J,EP)=K33(I,J,EP)+CHM(I,J,EP)*G33(EP)
         K44(I,J,EP)=K44(I,J,EP)+CHM(I,J,EP)*G44(EP)
         M11(I,J,EP)=M11(I,J,EP)+CHF(I,J,EP)*G11(EP)
         M22(I,J,EP)=M22(I,J,EP)+CHF(I,J,EP)*G22(EP)
         M33(I,J,EP)=M33(I,J,EP)+CHF(I,J,EP)*G33(EP)
         M44(I,J,EP)=M44(I,J,EP)+CHF(I,J,EP)*G44(EP)
        ENDDO
       ENDDO
       ENDDO
       DO I=1,2
       DO J=1,2
        DO EP=JFT,JLT
         K12(I,J,EP)=K12(I,J,EP)+CHM(I,J,EP)*G12(EP)
         K13(I,J,EP)=K13(I,J,EP)+CHM(I,J,EP)*G13(EP)
         K14(I,J,EP)=K14(I,J,EP)+CHM(I,J,EP)*G14(EP)
         K23(I,J,EP)=K23(I,J,EP)+CHM(I,J,EP)*G23(EP)
         K24(I,J,EP)=K24(I,J,EP)+CHM(I,J,EP)*G24(EP)
         K34(I,J,EP)=K34(I,J,EP)+CHM(I,J,EP)*G34(EP)
         M12(I,J,EP)=M12(I,J,EP)+CHF(I,J,EP)*G12(EP)
         M13(I,J,EP)=M13(I,J,EP)+CHF(I,J,EP)*G13(EP)
         M14(I,J,EP)=M14(I,J,EP)+CHF(I,J,EP)*G14(EP)
         M23(I,J,EP)=M23(I,J,EP)+CHF(I,J,EP)*G23(EP)
         M24(I,J,EP)=M24(I,J,EP)+CHF(I,J,EP)*G24(EP)
         M34(I,J,EP)=M34(I,J,EP)+CHF(I,J,EP)*G34(EP)
        ENDDO
       ENDDO
       ENDDO
C---+---------+---------+warped elements-------------
       DO EP=NF,JLT
         CXX =ZR(EP)*CHM(1,1,EP) 
         CYY =ZR(EP)*CHM(2,2,EP) 
         CXY =ZR(EP)*CHM(1,2,EP)
         CX1 = CXX*PX1(EP)+CXY*PY1(EP)
         CX2 = CXX*PX2(EP)+CXY*PY2(EP)
         CY1 = CXY*PX1(EP)+CYY*PY1(EP)
         CY2 = CXY*PX2(EP)+CYY*PY2(EP)
         K11(1,3,EP) = CX1*GAMA(1,EP)
         K11(2,3,EP) = CY1*GAMA(1,EP)
         K11(3,1,EP) = K11(1,3,EP)
         K11(3,2,EP) = K11(2,3,EP)
         K22(1,3,EP) = CX2*GAMA(2,EP)
         K22(2,3,EP) = CY2*GAMA(2,EP)
         K22(3,1,EP) = K22(1,3,EP)
         K22(3,2,EP) = K22(2,3,EP)
         K33(1,3,EP) = -CX1*GAMA(3,EP)
         K33(2,3,EP) = -CY1*GAMA(3,EP)
         K33(3,1,EP) = K33(1,3,EP)
         K33(3,2,EP) = K33(2,3,EP)
         K44(1,3,EP) = -CX2*GAMA(4,EP)
         K44(2,3,EP) = -CY2*GAMA(4,EP)
         K44(3,1,EP) = K44(1,3,EP)
         K44(3,2,EP) = K44(2,3,EP)
         K12(1,3,EP) = CX2*GAMA(1,EP)
         K12(2,3,EP) = CY2*GAMA(1,EP)
         K12(3,1,EP) = CX1*GAMA(2,EP)
         K12(3,2,EP) = CY1*GAMA(2,EP)
         K13(1,3,EP) = -K11(1,3,EP)
         K13(2,3,EP) = -K11(2,3,EP)
         K13(3,1,EP) = -K33(1,3,EP)
         K13(3,2,EP) = -K33(2,3,EP)
         K14(1,3,EP) = -K12(1,3,EP)
         K14(2,3,EP) = -K12(2,3,EP)
         K14(3,1,EP) = CX1*GAMA(4,EP)
         K14(3,2,EP) = CY1*GAMA(4,EP)
         K23(1,3,EP) = -K12(3,1,EP)
         K23(2,3,EP) = -K12(3,2,EP)
         K23(3,1,EP) = CX2*GAMA(3,EP)
         K23(3,2,EP) = CY2*GAMA(3,EP)
         K24(1,3,EP) = -K22(1,3,EP)
         K24(2,3,EP) = -K22(2,3,EP)
         K24(3,1,EP) = -K44(1,3,EP)
         K24(3,2,EP) = -K44(2,3,EP)
         K34(1,3,EP) = -K23(3,1,EP)
         K34(2,3,EP) = -K23(3,2,EP)
         K34(3,1,EP) = -K14(3,1,EP)
         K34(3,2,EP) = -K14(3,2,EP)
       ENDDO
       DO EP=NF,JLT
         FAC = ZR(EP)*ZR(EP)
         CXX =FAC*CHM(1,1,EP) 
         CYY =FAC*CHM(2,2,EP) 
         CXY =FAC*CHM(1,2,EP)
         C11 = CXX*PX1(EP)*PX1(EP)+CYY*PY1(EP)*PY1(EP)+
     1         CXY*(PX1(EP)*PY1(EP)+PY1(EP)*PX1(EP))
         C12 = CXX*PX1(EP)*PX2(EP)+CYY*PY1(EP)*PY2(EP)+
     1         CXY*(PX1(EP)*PY2(EP)+PY1(EP)*PX2(EP))
         C22 = CXX*PX2(EP)*PX2(EP)+CYY*PY2(EP)*PY2(EP)+
     1         CXY*(PX2(EP)*PY2(EP)+PY2(EP)*PX2(EP))
         K11(3,3,EP) = K11(3,3,EP)+C11
         K12(3,3,EP) = K12(3,3,EP)+C12
         K13(3,3,EP) = K13(3,3,EP)-C11
         K14(3,3,EP) = K14(3,3,EP)-C12
         K22(3,3,EP) = K22(3,3,EP)+C22
         K23(3,3,EP) = K23(3,3,EP)-C12
         K24(3,3,EP) = K24(3,3,EP)-C22
         K33(3,3,EP) = K33(3,3,EP)+C11
         K34(3,3,EP) = K34(3,3,EP)+C12
         K44(3,3,EP) = K44(3,3,EP)+C22
       ENDDO
C------ Mzz  ----------
       IF (IDRIL == 0) THEN
       DO EP=JFT,JLT
        FAC =DHZ(EP)*(HXX(EP)+HYY(EP))
        M11(3,3,EP)=M11(3,3,EP)+FAC*G11(EP)
        M12(3,3,EP)=M12(3,3,EP)+FAC*G12(EP)
        M13(3,3,EP)=M13(3,3,EP)+FAC*G13(EP)
        M14(3,3,EP)=M14(3,3,EP)+FAC*G14(EP)
        M22(3,3,EP)=M22(3,3,EP)+FAC*G22(EP)
        M23(3,3,EP)=M23(3,3,EP)+FAC*G23(EP)
        M24(3,3,EP)=M24(3,3,EP)+FAC*G24(EP)
        M33(3,3,EP)=M33(3,3,EP)+FAC*G33(EP)
        M34(3,3,EP)=M34(3,3,EP)+FAC*G34(EP)
        M44(3,3,EP)=M44(3,3,EP)+FAC*G44(EP)
       ENDDO
       END IF !(IDRIL == 0) THEN
C
       RETURN
       END
Chd|====================================================================
Chd|  CZLKENR3                      source/elements/shell/coquez/czlken3.F
Chd|-- called by -----------
Chd|        CZKE3                         source/elements/shell/coquez/czke3.F
Chd|        CZKEL3                        source/elements/shell/coquez/czkel3.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE CZLKENR3(JFT ,JLT  ,VOL  ,THK0 ,THK2 ,
     2                    HM  ,HZ   ,A_I  ,PX1  ,PX2  ,
     3                    PY1  ,PY2  ,HXX  ,HYY  ,HXY  ,
     4                    PH1  ,PH2  ,Z1   ,NPLAT,IPLAT,DHZ  ,
     5           K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     6           M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     7           MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33, 
     8           MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34,
     9           PHKRX,PHKRY,PHKRXY,PHERX,PHERY,PHERXY,
     A           PHKRZ,PHERZ,PHKX ,PHKY ,PHEX ,PHEY  ) 
C--------------------------------------------------------------------------------------------------
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
      INTEGER JFT,JLT,NPLAT,IPLAT(*)
      MY_REAL 
     .    VOL(*),THK0(*),THK2(*),A_I(*),Z1(*),HM(MVSIZ,4),HZ(*),
     .    PX1(*)  ,PX2(*)   ,PY1(*)  ,PY2(*)  ,PH1(*)  ,PH2(*)  ,
     .    HXX(*),HYY(*),HXY(*),
     .    K11(3,3,*),K12(3,3,*),K13(3,3,*),K14(3,3,*),
     .    K22(3,3,*),K23(3,3,*),K24(3,3,*),K33(3,3,*),
     .    M11(3,3,*),M12(3,3,*),M13(3,3,*),M14(3,3,*),
     .    M22(3,3,*),M23(3,3,*),M24(3,3,*),M33(3,3,*),
     .    MF11(3,3,*),MF12(3,3,*),MF13(3,3,*),MF14(3,3,*),
     .    MF22(3,3,*),MF23(3,3,*),MF24(3,3,*),MF33(3,3,*),
     .    FM12(3,3,*),FM13(3,3,*),FM14(3,3,*),
     .    FM23(3,3,*),FM24(3,3,*),FM34(3,3,*),
     .    K34(3,3,*),K44(3,3,*),M34(3,3,*),M44(3,3,*),
     .    MF34(3,3,*),MF44(3,3,*),DHZ(*)
      my_real 
     .   PHKRX(4,*),PHKRY(4,*),
     .   PHKRXY(4,*),PHERX(4,*),PHERY(4,*),PHERXY(4,*),
     .   PHKRZ(4,*),PHERZ(4,*),PHKX(*) ,PHKY(*) ,PHEX(*) ,PHEY(*) 
C---------------|[KIJ][MFIJ]|----
C-----KE(6x6)=  |           |
C---------------|[FMIJ]{MIJ]|----
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER EP,I,J,NF,M
      MY_REAL 
     .    DM(3,3,MVSIZ),C1,C2,GM(MVSIZ),
     .    G11(MVSIZ),G12(MVSIZ),G13(MVSIZ),
     .    G14(MVSIZ),G22(MVSIZ),G23(MVSIZ),
     .    G24(MVSIZ),G33(MVSIZ),G34(MVSIZ),G44(MVSIZ),
     .    CHM(2,2,MVSIZ),CHF(2,2,MVSIZ),FACF(MVSIZ),FAC,
     .    GAMA(4,MVSIZ),CXX,CYY,CXY,C11,C22,C12,RZ_3,
     .    CBKRX(4,MVSIZ),CBKRY(4,MVSIZ),CBKRZ(4,MVSIZ),
     .    CBERX(4,MVSIZ),CBERY(4,MVSIZ),CBERZ(4,MVSIZ),
     .    GRZ3(4,MVSIZ),ZR(MVSIZ),C3,CX1,CX2,CY1,CY2,ZR2
C-----------gama(I)=hI/4-PH(I), PH:anti-sym comme bxI------
C-----------Attention Matrice sym Kii ne calcul que la moitie---------72
#include "vectorize.inc"
       DO M=JFT,JLT 
        EP=IPLAT(M)
        C2=VOL(EP)
        C1= THIRD*C2
        DM(1,1,M)=HM(EP,1)*C1
        DM(2,2,M)=HM(EP,2)*C1
        DM(1,2,M)=HM(EP,3)*C1
c        GM(M) =HM(EP,4)*C2
        DM(1,3,M)=ZERO
        DM(2,3,M)=ZERO
        DM(3,3,M)=HM(EP,4)*C1
        ZR(M) = Z1(EP)
c        zr(m)=zero
       ENDDO
       DO M=JFT,JLT 
        DM(2,1,M)=DM(1,2,M)
        DM(3,1,M)=DM(1,3,M)
        DM(3,2,M)=DM(2,3,M)
       ENDDO
C------------------shear est non constante----
C-------0.5*[-By Bx 0 0 0 BRZ]^tKG[-By Bx 0 0 0 BRZ]*0.5
C-------DHZ=0.25*Kg*V---------
       DO EP=JFT,JLT
        CHM(1,1,EP)=DHZ(EP)*HYY(EP)
        CHM(2,2,EP)=DHZ(EP)*HXX(EP)
        CHM(1,2,EP)=-DHZ(EP)*HXY(EP)
        CHM(2,1,EP)=CHM(1,2,EP)
       ENDDO
C--------------gamaI--------
       DO EP=JFT,JLT
        GAMA(1,EP)=FOURTH-PH1(EP)
        GAMA(2,EP)=-FOURTH-PH2(EP)
        GAMA(3,EP)=FOURTH+PH1(EP)
        GAMA(4,EP)=-FOURTH+PH2(EP)
        G11(EP) =GAMA(1,EP)*GAMA(1,EP) 
        G12(EP) =GAMA(1,EP)*GAMA(2,EP) 
        G13(EP) =GAMA(1,EP)*GAMA(3,EP) 
        G14(EP) =GAMA(1,EP)*GAMA(4,EP) 
        G22(EP) =GAMA(2,EP)*GAMA(2,EP) 
        G23(EP) =GAMA(2,EP)*GAMA(3,EP) 
        G24(EP) =GAMA(2,EP)*GAMA(4,EP) 
        G33(EP) =GAMA(3,EP)*GAMA(3,EP) 
        G34(EP) =GAMA(3,EP)*GAMA(4,EP) 
        G44(EP) =GAMA(4,EP)*GAMA(4,EP) 
       ENDDO
C-------
       DO I=1,2
       DO J=I,2
        DO EP=JFT,JLT
         K11(I,J,EP)=K11(I,J,EP)+CHM(I,J,EP)*G11(EP)
         K22(I,J,EP)=K22(I,J,EP)+CHM(I,J,EP)*G22(EP)
         K33(I,J,EP)=K33(I,J,EP)+CHM(I,J,EP)*G33(EP)
         K44(I,J,EP)=K44(I,J,EP)+CHM(I,J,EP)*G44(EP)
        ENDDO
       ENDDO
       ENDDO
       DO I=1,2
       DO J=1,2
        DO EP=JFT,JLT
         K12(I,J,EP)=K12(I,J,EP)+CHM(I,J,EP)*G12(EP)
         K13(I,J,EP)=K13(I,J,EP)+CHM(I,J,EP)*G13(EP)
         K14(I,J,EP)=K14(I,J,EP)+CHM(I,J,EP)*G14(EP)
         K23(I,J,EP)=K23(I,J,EP)+CHM(I,J,EP)*G23(EP)
         K24(I,J,EP)=K24(I,J,EP)+CHM(I,J,EP)*G24(EP)
         K34(I,J,EP)=K34(I,J,EP)+CHM(I,J,EP)*G34(EP)
        ENDDO
       ENDDO
       ENDDO
C
       DO J=1,4
       DO M=JFT,JLT
        RZ_3= DHZ(M)*THIRD
        GRZ3(J,M)=GAMA(J,M)*RZ_3
       ENDDO
       ENDDO
C       
       DO M=JFT,JLT
        I=1
        J=1
        C1 = -(PHKY(M)*PHKRZ(J,M)+PHEY(M)*PHERZ(J,M))
        C2 =  (PHKX(M)*PHKRZ(J,M)+PHEX(M)*PHERZ(J,M))
        MF11(1,3,M)=MF11(1,3,M)+GRZ3(I,M)*C1
        MF11(2,3,M)=MF11(2,3,M)+GRZ3(I,M)*C2
        I=2
        FM12(3,1,M)=FM12(3,1,M)+ GRZ3(I,M)*C1
        FM12(3,2,M)=FM12(3,2,M)+ GRZ3(I,M)*C2
        I=3
        FM13(3,1,M)=FM13(3,1,M)+GRZ3(I,M)*C1
        FM13(3,2,M)=FM13(3,2,M)+GRZ3(I,M)*C2
        I=4
        FM14(3,1,M)=FM14(3,1,M)+ GRZ3(I,M)*C1
        FM14(3,2,M)=FM14(3,2,M)+ GRZ3(I,M)*C2
        J=2
        C1 = -(PHKY(M)*PHKRZ(J,M)+PHEY(M)*PHERZ(J,M))
        C2 =  (PHKX(M)*PHKRZ(J,M)+PHEX(M)*PHERZ(J,M))
        I=1
        MF12(1,3,M)=MF12(1,3,M)+GRZ3(I,M)*C1
        MF12(2,3,M)=MF12(2,3,M)+GRZ3(I,M)*C2
        I=2
        MF22(1,3,M)=MF22(1,3,M)+GRZ3(I,M)*C1
        MF22(2,3,M)=MF22(2,3,M)+GRZ3(I,M)*C2
        I=3
        FM23(3,1,M)=FM23(3,1,M)+GRZ3(I,M)*C1
        FM23(3,2,M)=FM23(3,2,M)+GRZ3(I,M)*C2
        I=4
        FM24(3,1,M)=FM24(3,1,M)+ GRZ3(I,M)*C1
        FM24(3,2,M)=FM24(3,2,M)+ GRZ3(I,M)*C2
        J=3
        C1 = -(PHKY(M)*PHKRZ(J,M)+PHEY(M)*PHERZ(J,M))
        C2 =  (PHKX(M)*PHKRZ(J,M)+PHEX(M)*PHERZ(J,M))
        I=1
        MF13(1,3,M)=MF13(1,3,M)+ GRZ3(I,M)*C1
        MF13(2,3,M)=MF13(2,3,M)+ GRZ3(I,M)*C2
        I=2
        MF23(1,3,M)=MF23(1,3,M)+GRZ3(I,M)*C1
        MF23(2,3,M)=MF23(2,3,M)+GRZ3(I,M)*C2
        I=3
        MF33(1,3,M)=MF33(1,3,M)+ GRZ3(I,M)*C1
        MF33(2,3,M)=MF33(2,3,M)+ GRZ3(I,M)*C2
        I=4
        FM34(3,1,M)=FM34(3,1,M)+ GRZ3(I,M)*C1
        FM34(3,2,M)=FM34(3,2,M)+ GRZ3(I,M)*C2
        J=4
        C1 = -(PHKY(M)*PHKRZ(J,M)+PHEY(M)*PHERZ(J,M))
        C2 =  (PHKX(M)*PHKRZ(J,M)+PHEX(M)*PHERZ(J,M))
        I=1
        MF14(1,3,M)=MF14(1,3,M)+ GRZ3(I,M)*C1
        MF14(2,3,M)=MF14(2,3,M)+ GRZ3(I,M)*C2
        I=2
        MF24(1,3,M)=MF24(1,3,M)+ GRZ3(I,M)*C1
        MF24(2,3,M)=MF24(2,3,M)+ GRZ3(I,M)*C2
        I=3
        MF34(1,3,M)=MF34(1,3,M)+ GRZ3(I,M)*C1
        MF34(2,3,M)=MF34(2,3,M)+ GRZ3(I,M)*C2
        I=4
        MF44(1,3,M)=MF44(1,3,M)+ GRZ3(I,M)*C1
        MF44(2,3,M)=MF44(2,3,M)+ GRZ3(I,M)*C2
       ENDDO
C------ Mzz  ----------
       DO EP=JFT,JLT
        RZ_3= DHZ(EP)*THIRD
        I=1
        J=1
        M11(3,3,EP)=M11(3,3,EP)+
     .    RZ_3*(PHKRZ(I,EP)*PHKRZ(J,EP)+PHERZ(I,EP)*PHERZ(J,EP))
        J=2
        M12(3,3,EP)=M12(3,3,EP)+
     .    RZ_3*(PHKRZ(I,EP)*PHKRZ(J,EP)+PHERZ(I,EP)*PHERZ(J,EP))
        J=3
        M13(3,3,EP)=M13(3,3,EP)+
     .    RZ_3*(PHKRZ(I,EP)*PHKRZ(J,EP)+PHERZ(I,EP)*PHERZ(J,EP))
        J=4
        M14(3,3,EP)=M14(3,3,EP)+
     .    RZ_3*(PHKRZ(I,EP)*PHKRZ(J,EP)+PHERZ(I,EP)*PHERZ(J,EP))
        I=2
        J=2
        M22(3,3,EP)=M22(3,3,EP)+
     .    RZ_3*(PHKRZ(I,EP)*PHKRZ(J,EP)+PHERZ(I,EP)*PHERZ(J,EP))
        J=3
        M23(3,3,EP)=M23(3,3,EP)+
     .    RZ_3*(PHKRZ(I,EP)*PHKRZ(J,EP)+PHERZ(I,EP)*PHERZ(J,EP))
        J=4
        M24(3,3,EP)=M24(3,3,EP)+
     .    RZ_3*(PHKRZ(I,EP)*PHKRZ(J,EP)+PHERZ(I,EP)*PHERZ(J,EP))
        I=3
        J=3
        M33(3,3,EP)=M33(3,3,EP)+
     .    RZ_3*(PHKRZ(I,EP)*PHKRZ(J,EP)+PHERZ(I,EP)*PHERZ(J,EP))
        J=4
        M34(3,3,EP)=M34(3,3,EP)+
     .    RZ_3*(PHKRZ(I,EP)*PHKRZ(J,EP)+PHERZ(I,EP)*PHERZ(J,EP))
        I=4
        M44(3,3,EP)=M44(3,3,EP)+
     .    RZ_3*(PHKRZ(I,EP)*PHKRZ(J,EP)+PHERZ(I,EP)*PHERZ(J,EP))
       ENDDO
C-------[MFIJ]=[Bm]^t[C][BRm]; [MIJ]=[BRm]^t[C][BRm];----
C---------------|0 0 BRX       |----
C-----BR(3x6)=  |0 0 BRY       |
C---------------|0 0 BRXY+BRYX |----
C---------------Ksi first---
       DO J=1,4 
       DO M=JFT,JLT 
        CBKRX(J,M) =DM(1,1,M)*PHKRX(J,M)+DM(1,2,M)*PHKRY(J,M)+
     .              DM(1,3,M)*PHKRXY(J,M)
        CBKRY(J,M) =DM(2,1,M)*PHKRX(J,M)+DM(2,2,M)*PHKRY(J,M)+
     .              DM(2,3,M)*PHKRXY(J,M)
        CBKRZ(J,M) =DM(3,1,M)*PHKRX(J,M)+DM(3,2,M)*PHKRY(J,M)+
     .              DM(3,3,M)*PHKRXY(J,M)
        CBERX(J,M) =DM(1,1,M)*PHERX(J,M)+DM(1,2,M)*PHERY(J,M)+
     .              DM(1,3,M)*PHERXY(J,M)
        CBERY(J,M) =DM(2,1,M)*PHERX(J,M)+DM(2,2,M)*PHERY(J,M)+
     .              DM(2,3,M)*PHERXY(J,M)
        CBERZ(J,M) =DM(3,1,M)*PHERX(J,M)+DM(3,2,M)*PHERY(J,M)+
     .              DM(3,3,M)*PHERXY(J,M)
       ENDDO
       ENDDO
C
       DO M=JFT,JLT
        I=1
        J=1
        C1 = (PHKX(M)*CBKRX(J,M)+PHEX(M)*CBERX(J,M)+
     .        PHKY(M)*CBKRZ(J,M)+PHEY(M)*CBERZ(J,M))
        C2 = (PHKY(M)*CBKRY(J,M)+PHEY(M)*CBERY(J,M)+
     .        PHKX(M)*CBKRZ(J,M)+PHEX(M)*CBERZ(J,M))
        MF11(1,3,M)=MF11(1,3,M)+GAMA(I,M)*C1
        MF11(2,3,M)=MF11(2,3,M)+GAMA(I,M)*C2
        I=2
        FM12(3,1,M)=FM12(3,1,M)+ GAMA(I,M)*C1
        FM12(3,2,M)=FM12(3,2,M)+ GAMA(I,M)*C2
        I=3
        FM13(3,1,M)=FM13(3,1,M)+GAMA(I,M)*C1
        FM13(3,2,M)=FM13(3,2,M)+GAMA(I,M)*C2
        I=4
        FM14(3,1,M)=FM14(3,1,M)+ GAMA(I,M)*C1
        FM14(3,2,M)=FM14(3,2,M)+ GAMA(I,M)*C2
        J=2
        C1 = (PHKX(M)*CBKRX(J,M)+PHEX(M)*CBERX(J,M)+
     .        PHKY(M)*CBKRZ(J,M)+PHEY(M)*CBERZ(J,M))
        C2 = (PHKY(M)*CBKRY(J,M)+PHEY(M)*CBERY(J,M)+
     .        PHKX(M)*CBKRZ(J,M)+PHEX(M)*CBERZ(J,M))
        I=1
        MF12(1,3,M)=MF12(1,3,M)+GAMA(I,M)*C1
        MF12(2,3,M)=MF12(2,3,M)+GAMA(I,M)*C2
        I=2
        MF22(1,3,M)=MF22(1,3,M)+GAMA(I,M)*C1
        MF22(2,3,M)=MF22(2,3,M)+GAMA(I,M)*C2
        I=3
        FM23(3,1,M)=FM23(3,1,M)+ GAMA(I,M)*C1
        FM23(3,2,M)=FM23(3,2,M)+ GAMA(I,M)*C2
        I=4
        FM24(3,1,M)=FM24(3,1,M)+ GAMA(I,M)*C1
        FM24(3,2,M)=FM24(3,2,M)+ GAMA(I,M)*C2
        J=3
        C1 = (PHKX(M)*CBKRX(J,M)+PHEX(M)*CBERX(J,M)+
     .        PHKY(M)*CBKRZ(J,M)+PHEY(M)*CBERZ(J,M))
        C2 = (PHKY(M)*CBKRY(J,M)+PHEY(M)*CBERY(J,M)+
     .        PHKX(M)*CBKRZ(J,M)+PHEX(M)*CBERZ(J,M))
        I=1
        MF13(1,3,M)=MF13(1,3,M)+ GAMA(I,M)*C1
        MF13(2,3,M)=MF13(2,3,M)+ GAMA(I,M)*C2
        I=2
        MF23(1,3,M)=MF23(1,3,M)+GAMA(I,M)*C1
        MF23(2,3,M)=MF23(2,3,M)+GAMA(I,M)*C2
        I=3
        MF33(1,3,M)=MF33(1,3,M)+ GAMA(I,M)*C1
        MF33(2,3,M)=MF33(2,3,M)+ GAMA(I,M)*C2
        I=4
        FM34(3,1,M)=FM34(3,1,M)+ GAMA(I,M)*C1
        FM34(3,2,M)=FM34(3,2,M)+ GAMA(I,M)*C2
        J=4
        C1 = (PHKX(M)*CBKRX(J,M)+PHEX(M)*CBERX(J,M)+
     .        PHKY(M)*CBKRZ(J,M)+PHEY(M)*CBERZ(J,M))
        C2 = (PHKY(M)*CBKRY(J,M)+PHEY(M)*CBERY(J,M)+
     .        PHKX(M)*CBKRZ(J,M)+PHEX(M)*CBERZ(J,M))
        I=1
        MF14(1,3,M)=MF14(1,3,M)+ GAMA(I,M)*C1
        MF14(2,3,M)=MF14(2,3,M)+ GAMA(I,M)*C2
        I=2
        MF24(1,3,M)=MF24(1,3,M)+ GAMA(I,M)*C1
        MF24(2,3,M)=MF24(2,3,M)+ GAMA(I,M)*C2
        I=3
        MF34(1,3,M)=MF34(1,3,M)+ GAMA(I,M)*C1
        MF34(2,3,M)=MF34(2,3,M)+ GAMA(I,M)*C2
        I=4
        MF44(1,3,M)=MF44(1,3,M)+ GAMA(I,M)*C1
        MF44(2,3,M)=MF44(2,3,M)+ GAMA(I,M)*C2
       ENDDO
C------ Mzz  ----------
       DO M=JFT,JLT
        I=1
        J=1
        C1 = PHKRX(I,M)*CBKRX(J,M)+PHERX(I,M)*CBERX(J,M)
     .      +PHKRY(I,M)*CBKRY(J,M)+PHERY(I,M)*CBERY(J,M)
     .      +PHKRXY(I,M)*CBKRZ(J,M)+PHERXY(I,M)*CBERZ(J,M)
        M11(3,3,M)=M11(3,3,M)+C1
        J=2
        C1 = PHKRX(I,M)*CBKRX(J,M)+PHERX(I,M)*CBERX(J,M)
     .      +PHKRY(I,M)*CBKRY(J,M)+PHERY(I,M)*CBERY(J,M)
     .      +PHKRXY(I,M)*CBKRZ(J,M)+PHERXY(I,M)*CBERZ(J,M)
        M12(3,3,M)=M12(3,3,M)+C1
        J=3
        C1 = PHKRX(I,M)*CBKRX(J,M)+PHERX(I,M)*CBERX(J,M)
     .      +PHKRY(I,M)*CBKRY(J,M)+PHERY(I,M)*CBERY(J,M)
     .      +PHKRXY(I,M)*CBKRZ(J,M)+PHERXY(I,M)*CBERZ(J,M)
        M13(3,3,M)=M13(3,3,M)+C1
        J=4
        C1 = PHKRX(I,M)*CBKRX(J,M)+PHERX(I,M)*CBERX(J,M)
     .      +PHKRY(I,M)*CBKRY(J,M)+PHERY(I,M)*CBERY(J,M)
     .      +PHKRXY(I,M)*CBKRZ(J,M)+PHERXY(I,M)*CBERZ(J,M)
        M14(3,3,M)=M14(3,3,M)+C1
        I=2
        J=2
        C1 = PHKRX(I,M)*CBKRX(J,M)+PHERX(I,M)*CBERX(J,M)
     .      +PHKRY(I,M)*CBKRY(J,M)+PHERY(I,M)*CBERY(J,M)
     .      +PHKRXY(I,M)*CBKRZ(J,M)+PHERXY(I,M)*CBERZ(J,M)
        M22(3,3,M)=M22(3,3,M)+C1
        J=3
        C1 = PHKRX(I,M)*CBKRX(J,M)+PHERX(I,M)*CBERX(J,M)
     .      +PHKRY(I,M)*CBKRY(J,M)+PHERY(I,M)*CBERY(J,M)
     .      +PHKRXY(I,M)*CBKRZ(J,M)+PHERXY(I,M)*CBERZ(J,M)
        M23(3,3,M)=M23(3,3,M)+C1
        J=4
        C1 = PHKRX(I,M)*CBKRX(J,M)+PHERX(I,M)*CBERX(J,M)
     .      +PHKRY(I,M)*CBKRY(J,M)+PHERY(I,M)*CBERY(J,M)
     .      +PHKRXY(I,M)*CBKRZ(J,M)+PHERXY(I,M)*CBERZ(J,M)
        M24(3,3,M)=M24(3,3,M)+C1
        I=3
        J=3
        C1 = PHKRX(I,M)*CBKRX(J,M)+PHERX(I,M)*CBERX(J,M)
     .      +PHKRY(I,M)*CBKRY(J,M)+PHERY(I,M)*CBERY(J,M)
     .      +PHKRXY(I,M)*CBKRZ(J,M)+PHERXY(I,M)*CBERZ(J,M)
        M33(3,3,M)=M33(3,3,M)+C1
        J=4
        C1 = PHKRX(I,M)*CBKRX(J,M)+PHERX(I,M)*CBERX(J,M)
     .      +PHKRY(I,M)*CBKRY(J,M)+PHERY(I,M)*CBERY(J,M)
     .      +PHKRXY(I,M)*CBKRZ(J,M)+PHERXY(I,M)*CBERZ(J,M)
        M34(3,3,M)=M34(3,3,M)+C1
        I=4
        C1 = PHKRX(I,M)*CBKRX(J,M)+PHERX(I,M)*CBERX(J,M)
     .      +PHKRY(I,M)*CBKRY(J,M)+PHERY(I,M)*CBERY(J,M)
     .      +PHKRXY(I,M)*CBKRZ(J,M)+PHERXY(I,M)*CBERZ(J,M)
        M44(3,3,M)=M44(3,3,M)+C1
       ENDDO
C---+---------+---------+warped elements-------------
       NF=NPLAT+1
       DO M=NF,JLT
        J=1
        C1 = (PHKX(M)*CBKRX(J,M)+PHEX(M)*CBERX(J,M)+
     .        PHKY(M)*CBKRZ(J,M)+PHEY(M)*CBERZ(J,M))
        C2 = (PHKY(M)*CBKRY(J,M)+PHEY(M)*CBERY(J,M)+
     .        PHKX(M)*CBKRZ(J,M)+PHEX(M)*CBERZ(J,M))
C        I=1,3
        C3 = ZR(M)*(PX1(M)*C1+PY1(M)*C2)
        MF11(3,3,M)=MF11(3,3,M)+ C3
        FM13(3,3,M)=FM13(3,3,M)- C3
C        I=2,4
        C3 = ZR(M)*(PX2(M)*C1+PY2(M)*C2)
        FM12(3,3,M)=FM12(3,3,M)+ C3
        FM14(3,3,M)=FM14(3,3,M)- C3
        J=2
        C1 = (PHKX(M)*CBKRX(J,M)+PHEX(M)*CBERX(J,M)+
     .        PHKY(M)*CBKRZ(J,M)+PHEY(M)*CBERZ(J,M))
        C2 = (PHKY(M)*CBKRY(J,M)+PHEY(M)*CBERY(J,M)+
     .        PHKX(M)*CBKRZ(J,M)+PHEX(M)*CBERZ(J,M))
C        I=1,3
        C3 = ZR(M)*(PX1(M)*C1+PY1(M)*C2)
        MF12(3,3,M)=MF12(3,3,M)+ C3
        FM23(3,3,M)=FM23(3,3,M)- C3
C        I=2,4
        C3 = ZR(M)*(PX2(M)*C1+PY2(M)*C2)
        MF22(3,3,M)=MF22(3,3,M)+ C3
        FM24(3,3,M)=FM24(3,3,M)- C3
        J=3
        C1 = (PHKX(M)*CBKRX(J,M)+PHEX(M)*CBERX(J,M)+
     .        PHKY(M)*CBKRZ(J,M)+PHEY(M)*CBERZ(J,M))
        C2 = (PHKY(M)*CBKRY(J,M)+PHEY(M)*CBERY(J,M)+
     .        PHKX(M)*CBKRZ(J,M)+PHEX(M)*CBERZ(J,M))
C        I=1,3
        C3 = ZR(M)*(PX1(M)*C1+PY1(M)*C2)
        MF13(3,3,M)=MF13(3,3,M)+ C3
        MF33(3,3,M)=MF33(3,3,M)- C3
C        I=2,4
        C3 = ZR(M)*(PX2(M)*C1+PY2(M)*C2)
        MF23(3,3,M)=MF23(3,3,M)+ C3
        FM34(3,3,M)=FM34(3,3,M)- C3
        J=4
        C1 = (PHKX(M)*CBKRX(J,M)+PHEX(M)*CBERX(J,M)+
     .        PHKY(M)*CBKRZ(J,M)+PHEY(M)*CBERZ(J,M))
        C2 = (PHKY(M)*CBKRY(J,M)+PHEY(M)*CBERY(J,M)+
     .        PHKX(M)*CBKRZ(J,M)+PHEX(M)*CBERZ(J,M))
C        I=1,3
        C3 = ZR(M)*(PX1(M)*C1+PY1(M)*C2)
        MF14(3,3,M)=MF14(3,3,M)+ C3
        MF34(3,3,M)=MF34(3,3,M)- C3
C        I=2,4
        C3 = ZR(M)*(PX2(M)*C1+PY2(M)*C2)
        MF24(3,3,M)=MF24(3,3,M)+ C3
        MF44(3,3,M)=MF44(3,3,M)- C3
       ENDDO
C------------------add coulpling terms w/ warped----
C-------0.5*[-By Bx Zr* 0 0 BRZ]^tKG[-By Bx Zr* 0 0 BRZ]*0.5
C-------DHZ=0.25*KG*V---------
C-----KIJ(3,1)=KJI(1,3)=DHZ*Zr*(HYY*BxI*GamaJ-HXY*ByI*GamaJ)----------
C-----KIJ(3,2)=KJI(2,3)=DHZ*Zr*(HXX*ByI*GamaJ-HXY*BxI*GamaJ)----------
       DO M=NF,JLT
C-------------------------        
        J=1
C-----------------------        
	C11 = ZR(M)*GAMA(J,M)
C I=1,3
	C1 = (CHM(1,1,M)*PX1(M)+CHM(1,2,M)*PY1(M))*C11
	C2 = (CHM(1,2,M)*PX1(M)+CHM(2,2,M)*PY1(M))*C11
        K11(1,3,M)=K11(1,3,M)+ C1
        K13(1,3,M)=K13(1,3,M)- C1
        K11(2,3,M)=K11(2,3,M)+ C2
        K13(2,3,M)=K13(2,3,M)- C2
C I=2,4
	C1 = (CHM(1,1,M)*PX2(M)+CHM(1,2,M)*PY2(M))*C11
	C2 = (CHM(1,2,M)*PX2(M)+CHM(2,2,M)*PY2(M))*C11
        K12(1,3,M)=K12(1,3,M)+ C1
        K14(1,3,M)=K14(1,3,M)- C1
        K12(2,3,M)=K12(2,3,M)+ C2
        K14(2,3,M)=K14(2,3,M)- C2
C-------------------------        
        J=2
C---------------------------        
	C11 = ZR(M)*GAMA(J,M)
C I=1,3
	C1 = (CHM(1,1,M)*PX1(M)+CHM(1,2,M)*PY1(M))*C11
	C2 = (CHM(1,2,M)*PX1(M)+CHM(2,2,M)*PY1(M))*C11
        K12(3,1,M)=K12(3,1,M)+ C1
        K23(1,3,M)=K23(1,3,M)- C1
        K12(3,2,M)=K12(3,2,M)+ C2
        K23(2,3,M)=K23(2,3,M)- C2
C I=2,4
	C1 = (CHM(1,1,M)*PX2(M)+CHM(1,2,M)*PY2(M))*C11
	C2 = (CHM(1,2,M)*PX2(M)+CHM(2,2,M)*PY2(M))*C11
        K22(1,3,M)=K22(1,3,M)+ C1
        K24(1,3,M)=K24(1,3,M)- C1
        K22(2,3,M)=K22(2,3,M)+ C2
        K24(2,3,M)=K24(2,3,M)- C2
C-------------------------        
        J=3
C-------------------------        
	C11 = ZR(M)*GAMA(J,M)
C I=1,3
	C1 = (CHM(1,1,M)*PX1(M)+CHM(1,2,M)*PY1(M))*C11
	C2 = (CHM(1,2,M)*PX1(M)+CHM(2,2,M)*PY1(M))*C11
        K13(3,1,M)=K13(3,1,M)+ C1
        K33(1,3,M)=K33(1,3,M)- C1
        K13(3,2,M)=K13(3,2,M)+ C2
        K33(2,3,M)=K33(2,3,M)- C2
C I=2,4
	C1 = (CHM(1,1,M)*PX2(M)+CHM(1,2,M)*PY2(M))*C11
	C2 = (CHM(1,2,M)*PX2(M)+CHM(2,2,M)*PY2(M))*C11
        K23(3,1,M)=K23(3,1,M)+ C1
        K34(1,3,M)=K34(1,3,M)- C1
        K23(3,2,M)=K23(3,2,M)+ C2
        K34(2,3,M)=K34(2,3,M)- C2
C-------------------------        
        J=4
C-------------------------        
	C11 = ZR(M)*GAMA(J,M)
C I=1,3
	C1 = (CHM(1,1,M)*PX1(M)+CHM(1,2,M)*PY1(M))*C11
	C2 = (CHM(1,2,M)*PX1(M)+CHM(2,2,M)*PY1(M))*C11
        K14(3,1,M)=K14(3,1,M)+ C1
        K34(3,1,M)=K34(3,1,M)- C1
        K14(3,2,M)=K14(3,2,M)+ C2
        K34(3,2,M)=K34(3,2,M)- C2
C I=2,4
	C1 = (CHM(1,1,M)*PX2(M)+CHM(1,2,M)*PY2(M))*C11
	C2 = (CHM(1,2,M)*PX2(M)+CHM(2,2,M)*PY2(M))*C11
        K24(3,1,M)=K24(3,1,M)+ C1
        K44(1,3,M)=K44(1,3,M)- C1
        K24(3,2,M)=K24(3,2,M)+ C2
        K44(2,3,M)=K44(2,3,M)- C2
       ENDDO
C-----KIJ(3,3)=DHZ*Zr*Zr*(HYY*BxI*BxJ+HXX*ByI*ByJ-HXY*BxI*ByJ-HXY*ByI*BxJ)----------
       DO M=NF,JLT
	ZR2= ZR(M)*ZR(M)
C-------------------------        
        J=1
C-----------------------        
        CX1=ZR2*(CHM(1,1,M)*PX1(M)+CHM(1,2,M)*PY1(M))
        CY1=ZR2*(CHM(1,2,M)*PX1(M)+CHM(2,2,M)*PY1(M))  
C I=1,3
        C3=PX1(M)*CX1+PY1(M)*CY1   
        K11(3,3,M)=K11(3,3,M)+ C3   
c J=3  
        K13(3,3,M)=K13(3,3,M)- C3  
        K33(3,3,M)=K33(3,3,M)+ C3   
C I=2,4
        C3=PX2(M)*CX1+PY2(M)*CY1   
c  J=3  
        K23(3,3,M)=K23(3,3,M)- C3   
C-------------------------        
        J=2
C---------------------------        
        CX1=ZR2*(CHM(1,1,M)*PX2(M)+CHM(1,2,M)*PY2(M))
        CY1=ZR2*(CHM(1,2,M)*PX2(M)+CHM(2,2,M)*PY2(M))  
C I=1,3
        C3=PX1(M)*CX1+PY1(M)*CY1   
        K12(3,3,M)=K12(3,3,M)+ C3   
c  J=4   
        K14(3,3,M)=K14(3,3,M)- C3   
        K34(3,3,M)=K34(3,3,M)+ C3   
C I=2,4
        C3=PX2(M)*CX1+PY2(M)*CY1   
        K22(3,3,M)=K22(3,3,M)+ C3   
c  J=4   
        K24(3,3,M)=K24(3,3,M)- C3   
        K44(3,3,M)=K44(3,3,M)+ C3   
       ENDDO
C-----MFIJ(3,3)=FMJI(3,3)=DHZ*Zr(-PHy*BxI*BRZJ+PHx*ByI*BRZJ)----------
       DO M=NF,JLT
        RZ_3= ZR(M)*DHZ(M)*THIRD
C-------------------------        
        J=1
C-----------------------        
        CX2 = PHKX(M)*PHKRZ(J,M)+PHEX(M)*PHERZ(J,M)
        CY2 = PHKY(M)*PHKRZ(J,M)+PHEY(M)*PHERZ(J,M)
C I=1,3
        C3 = RZ_3*(-PX1(M)*CY2+PY1(M)*CX2)
        MF11(3,3,M)=MF11(3,3,M)+ C3
        FM13(3,3,M)=FM13(3,3,M)- C3
C I=2,4
        C3 = RZ_3*(-PX2(M)*CY2+PY2(M)*CX2)
        FM12(3,3,M)=FM12(3,3,M)+ C3
        FM14(3,3,M)=FM14(3,3,M)- C3
C-------------------------        
        J=2
C---------------------------        
        CX2 = PHKX(M)*PHKRZ(J,M)+PHEX(M)*PHERZ(J,M)
        CY2 = PHKY(M)*PHKRZ(J,M)+PHEY(M)*PHERZ(J,M)
C I=1,3
        C3 = RZ_3*(-PX1(M)*CY2+PY1(M)*CX2)
        MF12(3,3,M)=MF12(3,3,M)+ C3
        FM23(3,3,M)=FM23(3,3,M)- C3
C I=2,4
        C3 = RZ_3*(-PX2(M)*CY2+PY2(M)*CX2)
        MF22(3,3,M)=MF22(3,3,M)+ C3
        FM24(3,3,M)=FM24(3,3,M)- C3
C-------------------------        
        J=3
C-------------------------        
        CX2 = PHKX(M)*PHKRZ(J,M)+PHEX(M)*PHERZ(J,M)
        CY2 = PHKY(M)*PHKRZ(J,M)+PHEY(M)*PHERZ(J,M)
C I=1,3
        C3 = RZ_3*(-PX1(M)*CY2+PY1(M)*CX2)
        MF13(3,3,M)=MF13(3,3,M)+ C3
        MF33(3,3,M)=MF33(3,3,M)- C3
C I=2,4
        C3 = RZ_3*(-PX2(M)*CY2+PY2(M)*CX2)
        MF23(3,3,M)=MF23(3,3,M)+ C3
        FM34(3,3,M)=FM34(3,3,M)- C3
C-------------------------        
        J=4
C-------------------------        
        CX2 = PHKX(M)*PHKRZ(J,M)+PHEX(M)*PHERZ(J,M)
        CY2 = PHKY(M)*PHKRZ(J,M)+PHEY(M)*PHERZ(J,M)
C I=1,3
        C3 = RZ_3*(-PX1(M)*CY2+PY1(M)*CX2)
        MF14(3,3,M)=MF14(3,3,M)+ C3
        MF34(3,3,M)=MF34(3,3,M)- C3
C I=2,4
        C3 = RZ_3*(-PX2(M)*CY2+PY2(M)*CX2)
        MF24(3,3,M)=MF24(3,3,M)+ C3
        MF44(3,3,M)=MF44(3,3,M)- C3
       ENDDO
C       
       RETURN
       END
