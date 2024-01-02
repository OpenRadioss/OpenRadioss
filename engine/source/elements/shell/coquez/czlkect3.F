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
Chd|  CZLKECT3                      source/elements/shell/coquez/czlkect3.F
Chd|-- called by -----------
Chd|        CZKE3                         source/elements/shell/coquez/czke3.F
Chd|        CZKEL3                        source/elements/shell/coquez/czkel3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CZLKECT3(JFT ,JLT  ,VOL  ,HC   ,RX   ,
     4                    RY   ,SX    ,SY   ,RX2  ,RY2  ,
     5                    SX2  ,SY2   ,RHX  ,RHY  ,SHX  ,
     6                    SHY  ,GS    ,NPLAT ,IPLAT,
     9                    K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     A                    M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     B                    MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33, 
     C                    MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34) 
C--------------------------------------------------------------------------------------------------
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
      INTEGER JFT,JLT,NPLAT,IPLAT(*) ,IKGEO
      MY_REAL 
     .   VOL(*),HC(MVSIZ,2),RX(*)   ,RY(*)   ,SX(*)    ,SY(*)   ,
     .    RX2(*)  ,RY2(*)  ,SX2(*)  ,SY2(*)   ,
     .    RHX(MVSIZ,4)  ,RHY(MVSIZ,4)  ,SHX(MVSIZ,4)  ,SHY(MVSIZ,4) ,
     .    K11(3,3,*),K12(3,3,*),K13(3,3,*),K14(3,3,*),
     .    K22(3,3,*),K23(3,3,*),K24(3,3,*),K33(3,3,*),
     .    M11(3,3,*),M12(3,3,*),M13(3,3,*),M14(3,3,*),
     .    M22(3,3,*),M23(3,3,*),M24(3,3,*),M33(3,3,*),
     .    MF11(3,3,*),MF12(3,3,*),MF13(3,3,*),MF14(3,3,*),
     .    MF22(3,3,*),MF23(3,3,*),MF24(3,3,*),MF33(3,3,*),
     .    FM12(3,3,*),FM13(3,3,*),FM14(3,3,*),
     .    FM23(3,3,*),FM24(3,3,*),FM34(3,3,*),
     .    K34(3,3,*),K44(3,3,*),M34(3,3,*),M44(3,3,*),
     .    MF34(3,3,*),MF44(3,3,*),GS(*)
C---------------|[KIJ][MFIJ]|----
C-----KE(6x6)=  |           |
C---------------|[FMIJ]{MIJ]|----
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER EP,I,J,NF,M
      MY_REAL 
     .    CS1(MVSIZ),CS2(MVSIZ),CS3(MVSIZ),DS1(MVSIZ),QS1(MVSIZ),
     .    C1,C2,DS2(MVSIZ),QS2(MVSIZ),M1C1(2,2,MVSIZ),
     .    M1C2(2,2,MVSIZ),M1C3(2,2,MVSIZ),M2C1(2,2,MVSIZ),
     .    M2C2(2,2,MVSIZ),M2C3(2,2,MVSIZ),
     .    M1C11(2,2,MVSIZ),M1C12(2,2,MVSIZ),M1C13(2,2,MVSIZ),
     .    M1C14(2,2,MVSIZ),M1C22(2,2,MVSIZ),M1C23(2,2,MVSIZ),
     .    M1C24(2,2,MVSIZ),M1C33(2,2,MVSIZ),M1C34(2,2,MVSIZ),
     .    M1C44(2,2,MVSIZ),
     .    M2C11(2,2,MVSIZ),M2C12(2,2,MVSIZ),M2C13(2,2,MVSIZ),
     .    M2C14(2,2,MVSIZ),M2C22(2,2,MVSIZ),M2C23(2,2,MVSIZ),
     .    M2C24(2,2,MVSIZ),M2C33(2,2,MVSIZ),M2C34(2,2,MVSIZ),
     .    M2C44(2,2,MVSIZ),
     .    M3C11(2,2,MVSIZ),M3C12(2,2,MVSIZ),M3C13(2,2,MVSIZ),
     .    M3C14(2,2,MVSIZ),M3C22(2,2,MVSIZ),M3C23(2,2,MVSIZ),
     .    M3C24(2,2,MVSIZ),M3C33(2,2,MVSIZ),M3C34(2,2,MVSIZ),
     .    M3C44(2,2,MVSIZ),DCX(MVSIZ),DCY(MVSIZ),C11,
     .    DH(MVSIZ),HS1(MVSIZ),HS2(MVSIZ)
C-----------Attention Matrice sym Kii ne calcul que la moitie---------72
       NF=NPLAT+1
#include "vectorize.inc"
C--------Constante parts---------------
       DO M=JFT,JLT 
        EP=IPLAT(M)
        C2=VOL(EP)
        DCX(M)=HC(EP,1)*C2
        DCY(M)=HC(EP,2)*C2
       ENDDO
C----------shear----R : -1 1 1 -1; S: -1 -1 1 1--
       DO EP=JFT,JLT
        CS1(EP) = DCX(EP)*SY2(EP)+DCY(EP)*SX2(EP)
        CS2(EP) = DCX(EP)*RY2(EP)+DCY(EP)*RX2(EP)
        CS3(EP) =-DCX(EP)*SY(EP)*RY(EP)-DCY(EP)*SX(EP)*RX(EP)
       ENDDO
C--------add non-constant part for the case of orthopic---------------
       DO M=JFT,JLT 
        EP=IPLAT(M)
        C2=VOL(EP)
        DH(M)=GS(EP)*C2
       ENDDO
C
       DO EP=JFT,JLT
        HS1(EP) = THIRD*DH(EP)*(SY2(EP)+SX2(EP))
        HS2(EP) = THIRD*DH(EP)*(RY2(EP)+RX2(EP))
       ENDDO
C------R : -1 1 1 -1; S: -1 -1 1 1 ------------------
       DO EP=JFT,JLT
C------------------r1r1=1,s1s1=1, r1s1=1 ,r1s1=1------------------
        K11(3,3,EP) = CS1(EP)+CS2(EP)+CS3(EP)+CS3(EP) 
C------------------r2r2=1,s2s2=1, r2s2=-1 ,r2s2=-1------------------
        K22(3,3,EP) = CS1(EP)+CS2(EP)-CS3(EP)-CS3(EP) 
        K33(3,3,EP) = K11(3,3,EP) 
        K44(3,3,EP) = K22(3,3,EP) 
C------------------r1r2=-1,s1s2=1, r1s2=1 ,r2s1=-1------------------
        K12(3,3,EP) =-CS1(EP)+CS2(EP)+CS3(EP)-CS3(EP) 
C------------------r1r3=-1,s1s3=-1, r1s3=-1 ,r3s1=-1------------------
        K13(3,3,EP) =-CS1(EP)-CS2(EP)-CS3(EP)-CS3(EP) 
C------------------r1r4=1,s1s4=-1, r1s4=-1 ,r4s1=1------------------
        K14(3,3,EP) = - K12(3,3,EP)
C------------------r2r3=1,s2s3=-1, r2s3=1 ,r3s2=-1------------------
        K23(3,3,EP) = CS1(EP)-CS2(EP)+CS3(EP)-CS3(EP) 
C------------------r2r4=-1,s2s4=-1, r2s4=1 ,r4s2=1------------------
        K24(3,3,EP) =-K22(3,3,EP)
C------------------r3r4=-1,s3s4=1, r3s4=1 ,r4s3=-1------------------
        K34(3,3,EP) =K12(3,3,EP)
       ENDDO
C--------non constant part---hIhJ-
       DO EP=JFT,JLT
        C11 =HS1(EP)+HS2(EP)
        K11(3,3,EP) = K11(3,3,EP)+C11
        K22(3,3,EP) = K22(3,3,EP)+C11 
        K33(3,3,EP) = K33(3,3,EP)+C11 
        K44(3,3,EP) = K44(3,3,EP)+C11 
        K12(3,3,EP) = K12(3,3,EP)-C11
        K13(3,3,EP) = K13(3,3,EP)+C11
        K14(3,3,EP) = K14(3,3,EP)-C11
        K23(3,3,EP) = K23(3,3,EP)-C11 
        K24(3,3,EP) = K24(3,3,EP)+C11
        K34(3,3,EP) = K34(3,3,EP)-C11
       ENDDO
C-------bending terms----------------
C------KC1 ------
       DO EP=JFT,JLT
C------M1C1: 11,12,22 ------
        M1C1(1,1,EP) = RHY(EP,1)*RHY(EP,1)
        M1C1(2,2,EP) = RHX(EP,1)*RHX(EP,1)
        M1C1(1,2,EP) = -RHY(EP,1)*RHX(EP,1)
        M1C1(2,1,EP) = M1C1(1,2,EP)
C------M1C2: 13,14,23,24 ------
        M1C2(1,1,EP) = RHY(EP,1)*RHY(EP,3)
        M1C2(2,2,EP) = RHX(EP,1)*RHX(EP,3)
        M1C2(1,2,EP) = -RHY(EP,1)*RHX(EP,3)
        M1C2(2,1,EP) = -RHY(EP,3)*RHX(EP,1)
C------M1C3: 33,34,44 ------
        M1C3(1,1,EP) = RHY(EP,3)*RHY(EP,3)
        M1C3(2,2,EP) = RHX(EP,3)*RHX(EP,3)
        M1C3(1,2,EP) = -RHY(EP,3)*RHX(EP,3)
        M1C3(2,1,EP) = M1C3(1,2,EP)
        DS1(EP) = CS1(EP)-HS1(EP)
        QS1(EP) = CS1(EP)+HS1(EP)
       ENDDO
C---------non constant part is added directly : KcH1=SISJ/3 KC1;
       DO I=1,2
       DO J=I,2
        DO EP=JFT,JLT
C------M1C1: 11,12,22 ---SISJ=1---
         M1C11(I,J,EP) = M1C1(I,J,EP)*QS1(EP)
         M1C12(I,J,EP) = M1C11(I,J,EP)
         M1C22(I,J,EP) = M1C11(I,J,EP)
C------M1C3: 33,34,44 ----SISJ=1--
         M1C33(I,J,EP) = M1C3(I,J,EP)*QS1(EP)
         M1C34(I,J,EP) = M1C33(I,J,EP)
         M1C44(I,J,EP) = M1C33(I,J,EP)
        ENDDO
       ENDDO
       ENDDO
       DO EP=JFT,JLT
        M1C12(2,1,EP)=M1C12(1,2,EP)
        M1C34(2,1,EP)=M1C34(1,2,EP)
       ENDDO
       DO I=1,2
       DO J=1,2
        DO EP=JFT,JLT
C------M1C2: 13,14,23,24 --SISJ=-1----
         M1C13(I,J,EP) = M1C2(I,J,EP)*DS1(EP)
         M1C14(I,J,EP) = M1C13(I,J,EP)
         M1C23(I,J,EP) = M1C13(I,J,EP)
         M1C24(I,J,EP) = M1C13(I,J,EP)
        ENDDO
       ENDDO
       ENDDO
C---------KC2;KcH2=RIRJ/3 KC2
       DO EP=JFT,JLT
C------M2C1: 11,14,44 ---3=2,4=1---
        M2C1(1,1,EP) = SHY(EP,1)*SHY(EP,1)
        M2C1(2,2,EP) = SHX(EP,1)*SHX(EP,1)
        M2C1(1,2,EP) = -SHY(EP,1)*SHX(EP,1)
        M2C1(2,1,EP) = M2C1(1,2,EP)
C------M2C2: 12,13,24,34 ------
        M2C2(1,1,EP) = SHY(EP,1)*SHY(EP,2)
        M2C2(2,2,EP) = SHX(EP,1)*SHX(EP,2)
C------exception (1,2)->24,34=(2,1)-(2,1)->24,34=(1,2)-
        M2C2(1,2,EP) = -SHY(EP,1)*SHX(EP,2)
        M2C2(2,1,EP) = -SHY(EP,2)*SHX(EP,1)
C------M2C3: 22,23,33 ------
        M2C3(1,1,EP) = SHY(EP,2)*SHY(EP,2)
        M2C3(2,2,EP) = SHX(EP,2)*SHX(EP,2)
        M2C3(1,2,EP) = -SHY(EP,2)*SHX(EP,2)
        M2C3(2,1,EP) = M2C3(1,2,EP)
        DS2(EP) = CS2(EP)-HS2(EP)
        QS2(EP) = CS2(EP)+HS2(EP)
       ENDDO
       DO I=1,2
       DO J=I,2
        DO EP=JFT,JLT
C------M2C1: 11,14,44 ---RIRJ=1---
         M2C11(I,J,EP) = M2C1(I,J,EP)*QS2(EP)
         M2C14(I,J,EP) = M2C11(I,J,EP)
         M2C44(I,J,EP) = M2C11(I,J,EP)
C------M2C3: 22,23,33 ---RIRJ=1---
         M2C22(I,J,EP) = M2C3(I,J,EP)*QS2(EP)
         M2C23(I,J,EP) = M2C22(I,J,EP)
         M2C33(I,J,EP) = M2C22(I,J,EP)
        ENDDO
       ENDDO
       ENDDO
       DO EP=JFT,JLT
        M2C14(2,1,EP)=M2C14(1,2,EP)
        M2C23(2,1,EP)=M2C23(1,2,EP)
       ENDDO
       DO I=1,2
       DO J=1,2
        DO EP=JFT,JLT
C------M2C2: 12,13,24,34 -exception 24,34--(1,2)<->(2,1)--RIRJ=-1-
         M2C12(I,J,EP) = M2C2(I,J,EP)*DS2(EP)
         M2C13(I,J,EP) = M2C12(I,J,EP)
        ENDDO
       ENDDO
       ENDDO
       DO I=1,2
       DO J=1,2
        DO EP=JFT,JLT
C------M2C2: 12,13,24,34 -exception 24,34--(1,2)<->(2,1)---
         M2C24(I,J,EP) = M2C12(J,I,EP)
         M2C34(I,J,EP) = M2C12(J,I,EP)
        ENDDO
       ENDDO
       ENDDO
C------M3C ->M3C+M4C -cette partie peut etre optimise(decomp en const + antisym-----
       DO EP=JFT,JLT
C-----------Attention Matrice sym Kii ne calcul que la moitie---------72
        M3C11(1,1,EP) =(RHY(EP,1)*SHY(EP,1)+RHY(EP,1)*SHY(EP,1))*CS3(EP)
        M3C12(1,1,EP) =(RHY(EP,1)*SHY(EP,2)+RHY(EP,2)*SHY(EP,1))*CS3(EP)
        M3C13(1,1,EP) =(RHY(EP,1)*SHY(EP,3)+RHY(EP,3)*SHY(EP,1))*CS3(EP)
        M3C14(1,1,EP) =(RHY(EP,1)*SHY(EP,4)+RHY(EP,4)*SHY(EP,1))*CS3(EP)
        M3C22(1,1,EP) =(RHY(EP,2)*SHY(EP,2)+RHY(EP,2)*SHY(EP,2))*CS3(EP)
        M3C23(1,1,EP) =(RHY(EP,2)*SHY(EP,3)+RHY(EP,3)*SHY(EP,2))*CS3(EP)
        M3C24(1,1,EP) =(RHY(EP,2)*SHY(EP,4)+RHY(EP,4)*SHY(EP,2))*CS3(EP)
        M3C33(1,1,EP) =(RHY(EP,3)*SHY(EP,3)+RHY(EP,3)*SHY(EP,3))*CS3(EP) 
        M3C34(1,1,EP) =(RHY(EP,3)*SHY(EP,4)+RHY(EP,4)*SHY(EP,3))*CS3(EP) 
        M3C44(1,1,EP) =(RHY(EP,4)*SHY(EP,4)+RHY(EP,4)*SHY(EP,4))*CS3(EP)
       ENDDO
       DO EP=JFT,JLT
        M3C11(2,2,EP) =(RHX(EP,1)*SHX(EP,1)+RHX(EP,1)*SHX(EP,1))*CS3(EP)
        M3C12(2,2,EP) =(RHX(EP,1)*SHX(EP,2)+RHX(EP,2)*SHX(EP,1))*CS3(EP)
        M3C13(2,2,EP) =(RHX(EP,1)*SHX(EP,3)+RHX(EP,3)*SHX(EP,1))*CS3(EP)
        M3C14(2,2,EP) =(RHX(EP,1)*SHX(EP,4)+RHX(EP,4)*SHX(EP,1))*CS3(EP)
        M3C22(2,2,EP) =(RHX(EP,2)*SHX(EP,2)+RHX(EP,2)*SHX(EP,2))*CS3(EP)
        M3C23(2,2,EP) =(RHX(EP,2)*SHX(EP,3)+RHX(EP,3)*SHX(EP,2))*CS3(EP)
        M3C24(2,2,EP) =(RHX(EP,2)*SHX(EP,4)+RHX(EP,4)*SHX(EP,2))*CS3(EP)
        M3C33(2,2,EP) =(RHX(EP,3)*SHX(EP,3)+RHX(EP,3)*SHX(EP,3))*CS3(EP)
        M3C34(2,2,EP) =(RHX(EP,3)*SHX(EP,4)+RHX(EP,4)*SHX(EP,3))*CS3(EP) 
        M3C44(2,2,EP) =(RHX(EP,4)*SHX(EP,4)+RHX(EP,4)*SHX(EP,4))*CS3(EP)
       ENDDO
       DO EP=JFT,JLT
        M3C11(1,2,EP)=(-RHY(EP,1)*SHX(EP,1)-RHX(EP,1)*SHY(EP,1))*CS3(EP)
        M3C12(1,2,EP)=(-RHY(EP,1)*SHX(EP,2)-RHX(EP,2)*SHY(EP,1))*CS3(EP)
        M3C13(1,2,EP)=(-RHY(EP,1)*SHX(EP,3)-RHX(EP,3)*SHY(EP,1))*CS3(EP)
        M3C14(1,2,EP)=(-RHY(EP,1)*SHX(EP,4)-RHX(EP,4)*SHY(EP,1))*CS3(EP)
        M3C22(1,2,EP)=(-RHY(EP,2)*SHX(EP,2)-RHX(EP,2)*SHY(EP,2))*CS3(EP)
        M3C23(1,2,EP)=(-RHY(EP,2)*SHX(EP,3)-RHX(EP,3)*SHY(EP,2))*CS3(EP)
        M3C24(1,2,EP)=(-RHY(EP,2)*SHX(EP,4)-RHX(EP,4)*SHY(EP,2))*CS3(EP)
        M3C33(1,2,EP)=(-RHY(EP,3)*SHX(EP,3)-RHX(EP,3)*SHY(EP,3))*CS3(EP) 
        M3C34(1,2,EP)=(-RHY(EP,3)*SHX(EP,4)-RHX(EP,4)*SHY(EP,3))*CS3(EP)
        M3C44(1,2,EP)=(-RHY(EP,4)*SHX(EP,4)-RHX(EP,4)*SHY(EP,4))*CS3(EP)
       ENDDO
       DO EP=JFT,JLT
        M3C11(2,1,EP) =  M3C11(1,2,EP)
        M3C12(2,1,EP)=(-RHX(EP,1)*SHY(EP,2)-RHY(EP,2)*SHX(EP,1))*CS3(EP)
        M3C13(2,1,EP)=(-RHX(EP,1)*SHY(EP,3)-RHY(EP,3)*SHX(EP,1))*CS3(EP)
        M3C14(2,1,EP)=(-RHX(EP,1)*SHY(EP,4)-RHY(EP,4)*SHX(EP,1))*CS3(EP)
        M3C22(2,1,EP) =  M3C22(1,2,EP)
        M3C23(2,1,EP)=(-RHX(EP,2)*SHY(EP,3)-RHY(EP,3)*SHX(EP,2))*CS3(EP)
        M3C24(2,1,EP)=(-RHX(EP,2)*SHY(EP,4)-RHY(EP,4)*SHX(EP,2))*CS3(EP)
        M3C33(2,1,EP) =  M3C33(1,2,EP)
        M3C34(2,1,EP)=(-RHX(EP,3)*SHY(EP,4)-RHY(EP,4)*SHX(EP,3))*CS3(EP) 
        M3C44(2,1,EP) =  M3C44(1,2,EP)
       ENDDO
C
       DO I=1,2
       DO J=I,2
       DO EP=JFT,JLT
        M11(I,J,EP)=M11(I,J,EP)+
     1                   M1C11(I,J,EP)+M2C11(I,J,EP)+M3C11(I,J,EP)
        M22(I,J,EP)=M22(I,J,EP)+
     1                   M1C22(I,J,EP)+M2C22(I,J,EP)+M3C22(I,J,EP)
        M33(I,J,EP)=M33(I,J,EP)+
     1                   M1C33(I,J,EP)+M2C33(I,J,EP)+M3C33(I,J,EP)
        M44(I,J,EP)=M44(I,J,EP)+
     1                   M1C44(I,J,EP)+M2C44(I,J,EP)+M3C44(I,J,EP)
       ENDDO
       ENDDO
       ENDDO
C
       DO I=1,2
       DO J=1,2
       DO EP=JFT,JLT
        M12(I,J,EP)=M12(I,J,EP)+
     1                   M1C12(I,J,EP)+M2C12(I,J,EP)+M3C12(I,J,EP)
        M13(I,J,EP)=M13(I,J,EP)+
     1                   M1C13(I,J,EP)+M2C13(I,J,EP)+M3C13(I,J,EP)
        M14(I,J,EP)=M14(I,J,EP)+
     1                   M1C14(I,J,EP)+M2C14(I,J,EP)+M3C14(I,J,EP)
        M23(I,J,EP)=M23(I,J,EP)+
     1                   M1C23(I,J,EP)+M2C23(I,J,EP)+M3C23(I,J,EP)
        M24(I,J,EP)=M24(I,J,EP)+
     1                   M1C24(I,J,EP)+M2C24(I,J,EP)+M3C24(I,J,EP)
        M34(I,J,EP)=M34(I,J,EP)+
     1                   M1C34(I,J,EP)+M2C34(I,J,EP)+M3C34(I,J,EP)
       ENDDO
       ENDDO
       ENDDO
C------R : -1 1 1 -1; S: -1 -1 1 1; H:1 -1 1 -1----------------
C------CS1 : -R CS2: -S CS3: -R,H ---QSI=SISJ*CS1--QS2=RIRJ*CS2-----------
       DO EP=JFT,JLT
        MF11(3,1,EP)= QS1(EP)*RHY(EP,1)+QS2(EP)*SHY(EP,1)
     1               +CS3(EP)*(SHY(EP,1)+RHY(EP,1))
        MF12(3,1,EP)= QS1(EP)*RHY(EP,2)+DS2(EP)*SHY(EP,2)
     1               +CS3(EP)*(SHY(EP,2)+RHY(EP,2))
        MF13(3,1,EP)= DS1(EP)*RHY(EP,3)+DS2(EP)*SHY(EP,3)
     1               +CS3(EP)*(SHY(EP,3)+RHY(EP,3))
        MF14(3,1,EP)= DS1(EP)*RHY(EP,4)+QS2(EP)*SHY(EP,4)
     1               +CS3(EP)*(SHY(EP,4)+RHY(EP,4))
        MF22(3,1,EP)=-QS1(EP)*RHY(EP,2)+QS2(EP)*SHY(EP,2)
     1               -CS3(EP)*(SHY(EP,2)-RHY(EP,2))
        MF23(3,1,EP)=-DS1(EP)*RHY(EP,3)+QS2(EP)*SHY(EP,3)
     1               -CS3(EP)*(SHY(EP,3)-RHY(EP,3))
        MF24(3,1,EP)=-DS1(EP)*RHY(EP,4)+DS2(EP)*SHY(EP,4)
     1               -CS3(EP)*(SHY(EP,4)-RHY(EP,4))
        MF33(3,1,EP)=-QS1(EP)*RHY(EP,3)-QS2(EP)*SHY(EP,3)
     1               -CS3(EP)*(SHY(EP,3)+RHY(EP,3))
        MF34(3,1,EP)=-QS1(EP)*RHY(EP,4)-DS2(EP)*SHY(EP,4)
     1               -CS3(EP)*(SHY(EP,4)+RHY(EP,4))
        MF44(3,1,EP)= QS1(EP)*RHY(EP,4)-QS2(EP)*SHY(EP,4)
     1               +CS3(EP)*(SHY(EP,4)-RHY(EP,4))
       ENDDO
C------CS1 : R CS2: S CS3: R,H ----------------
       DO EP=JFT,JLT
        MF11(3,2,EP)=-QS1(EP)*RHX(EP,1)-QS2(EP)*SHX(EP,1)
     1               -CS3(EP)*(SHX(EP,1)+RHX(EP,1))
        MF12(3,2,EP)=-QS1(EP)*RHX(EP,2)-DS2(EP)*SHX(EP,2)
     1               -CS3(EP)*(SHX(EP,2)+RHX(EP,2))
        MF13(3,2,EP)=-DS1(EP)*RHX(EP,3)-DS2(EP)*SHX(EP,3)
     1               -CS3(EP)*(SHX(EP,3)+RHX(EP,3))
        MF14(3,2,EP)=-DS1(EP)*RHX(EP,4)-QS2(EP)*SHX(EP,4)
     1               -CS3(EP)*(SHX(EP,4)+RHX(EP,4))
        MF22(3,2,EP)= QS1(EP)*RHX(EP,2)-QS2(EP)*SHX(EP,2)
     1               +CS3(EP)*(SHX(EP,2)-RHX(EP,2))
        MF23(3,2,EP)= DS1(EP)*RHX(EP,3)-QS2(EP)*SHX(EP,3)
     1               +CS3(EP)*(SHX(EP,3)-RHX(EP,3))
        MF24(3,2,EP)= DS1(EP)*RHX(EP,4)-DS2(EP)*SHX(EP,4)
     1               +CS3(EP)*(SHX(EP,4)-RHX(EP,4))
        MF33(3,2,EP)= QS1(EP)*RHX(EP,3)+QS2(EP)*SHX(EP,3)
     1               +CS3(EP)*(SHX(EP,3)+RHX(EP,3))
        MF34(3,2,EP)= QS1(EP)*RHX(EP,4)+DS2(EP)*SHX(EP,4)
     1               +CS3(EP)*(SHX(EP,4)+RHX(EP,4))
        MF44(3,2,EP)=-QS1(EP)*RHX(EP,4)+QS2(EP)*SHX(EP,4)
     1               -CS3(EP)*(SHX(EP,4)-RHX(EP,4))
       ENDDO
C------R : -1 1 1 -1; S: -1 -1 1 1; H:1 -1 1 -1----------------
C------CS1 : -R CS2: -S CS3: -S,H ----------------
       DO EP=JFT,JLT
        FM12(1,3,EP)=-QS1(EP)*RHY(EP,1)+DS2(EP)*SHY(EP,1)
     1               +CS3(EP)*(RHY(EP,1)-SHY(EP,1))
        FM13(1,3,EP)=-DS1(EP)*RHY(EP,1)-DS2(EP)*SHY(EP,1)
     1               -CS3(EP)*(RHY(EP,1)+SHY(EP,1))
        FM23(1,3,EP)=-DS1(EP)*RHY(EP,2)-QS2(EP)*SHY(EP,2)
     1               -CS3(EP)*(RHY(EP,2)+SHY(EP,2))
        FM14(1,3,EP)= DS1(EP)*RHY(EP,1)-QS2(EP)*SHY(EP,1)
     1               -CS3(EP)*(RHY(EP,1)-SHY(EP,1))
        FM24(1,3,EP)= DS1(EP)*RHY(EP,2)-DS2(EP)*SHY(EP,2)
     1               -CS3(EP)*(RHY(EP,2)-SHY(EP,2))
        FM34(1,3,EP)= QS1(EP)*RHY(EP,3)-DS2(EP)*SHY(EP,3)
     1               -CS3(EP)*(RHY(EP,3)-SHY(EP,3))
C------CS1 : R CS2: S CS3: S,H ----------------
        FM12(2,3,EP)= QS1(EP)*RHX(EP,1)-DS2(EP)*SHX(EP,1)
     1               -CS3(EP)*(RHX(EP,1)-SHX(EP,1))
        FM13(2,3,EP)= DS1(EP)*RHX(EP,1)+DS2(EP)*SHX(EP,1)
     1               +CS3(EP)*(RHX(EP,1)+SHX(EP,1))
        FM23(2,3,EP)= DS1(EP)*RHX(EP,2)+QS2(EP)*SHX(EP,2)
     1               +CS3(EP)*(RHX(EP,2)+SHX(EP,2))
        FM14(2,3,EP)=-DS1(EP)*RHX(EP,1)+QS2(EP)*SHX(EP,1)
     1               +CS3(EP)*(RHX(EP,1)-SHX(EP,1))
        FM24(2,3,EP)=-DS1(EP)*RHX(EP,2)+DS2(EP)*SHX(EP,2)
     1               +CS3(EP)*(RHX(EP,2)-SHX(EP,2))
        FM34(2,3,EP)=-QS1(EP)*RHX(EP,3)+DS2(EP)*SHX(EP,3)
     1               +CS3(EP)*(RHX(EP,3)-SHX(EP,3))
       ENDDO
C
       RETURN
       END
