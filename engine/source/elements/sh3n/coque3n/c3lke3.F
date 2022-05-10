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
Chd|  C3LKE3                        source/elements/sh3n/coque3n/c3lke3.F
Chd|-- called by -----------
Chd|        C3KE3                         source/elements/sh3n/coque3n/c3ke3.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE C3LKE3(JFT,JLT,AREA,THK0,THK2,HM,HF,HC,HZ,
     1                      PX1,PY1,PY2,VOL,
     2                      K11,K12,K13,K22,K23,K33,
     3                      M11,M12,M13,M22,M23,M33,
     4                      MF11,MF12,MF13,MF22,MF23,MF33,
     5                      FM12,FM13,FM23,IKGEO,FOR  ,MOM  ,
     6                      IORTH,HMOR,HFOR,HMFOR,IDRIL,
     7                      NEL) 
C--------------------------------------------------------------------------------------------------
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
      INTEGER JFT,JLT,IKGEO,IORTH,IDRIL,NEL
      MY_REAL 
     .    AREA(*),VOL(*), PX1(*),PY1(*),PY2(*),THK0(*),THK2(*),
     .    HM(MVSIZ,4),HF(MVSIZ,4),HC(MVSIZ,2),HZ(*),HMOR(MVSIZ,2),HFOR(MVSIZ,2),
     .    K11(3,3,*),K12(3,3,*),K13(3,3,*),
     .    K22(3,3,*),K23(3,3,*),K33(3,3,*),
     .    M11(3,3,*),M12(3,3,*),M13(3,3,*),
     .    M22(3,3,*),M23(3,3,*),M33(3,3,*),
     .    MF11(3,3,*),MF12(3,3,*),MF13(3,3,*),
     .    MF22(3,3,*),MF23(3,3,*),MF33(3,3,*),
     .    FM12(3,3,*),FM13(3,3,*),FM23(3,3,*),FOR(NEL,5),MOM(NEL,3),
     .    HMFOR(MVSIZ,6)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER EP,I,J,L,K,I1,J1,IN(2),NF
      MY_REAL 
     .    DM(2,2,MVSIZ),DF(2,2,MVSIZ),DCX(MVSIZ),DCY(MVSIZ),
     .    PX1PX1(MVSIZ),PX1PY1(MVSIZ),PX1PY2(MVSIZ),PX1PY3(MVSIZ),
     .    PY1PY1(MVSIZ),PY1PY2(MVSIZ),PY1PY3(MVSIZ),PY3(MVSIZ),
     .    PY2PY2(MVSIZ),PY2PY3(MVSIZ),PY3PY3(MVSIZ),
     .    C1,C2,DHZ(MVSIZ),GM(MVSIZ),GF(MVSIZ),C3,
     .    GPX1PX1,GPX1PY1,GPX1PY2,GPX1PY3,BCXX1,BCXX2,BCXY3,BCXY1,
     .    BCXY2,BCYY1,BCYY2,BCYY3,BCYX1,BCYX2,BCYX3,
     .    PX1DX,PY1DY,PY2DY,PY3DY,FAC,FXX,FYY,FXY,FXY2,H33(MVSIZ),
     .    H11(MVSIZ),H12(MVSIZ),H22(MVSIZ),H13(MVSIZ),H23(MVSIZ),
     .    DM5(MVSIZ),DM6(MVSIZ),DF5(MVSIZ),DF6(MVSIZ),DM5_2(MVSIZ),
     .    DM6_2(MVSIZ),DF5_2(MVSIZ),DF6_2(MVSIZ),
     .    DMF(3,3,MVSIZ),MFIJ(2,2,MVSIZ)
C-----------Attention Matrice sym Kii ne calcul que la moitie---------72
       DO EP=JFT,JLT 
        C2=VOL(EP)
        C1=THK2(EP)*C2
        DM(1,1,EP)=HM(EP,1)*C2
        DM(2,2,EP)=HM(EP,2)*C2
        DM(1,2,EP)=HM(EP,3)*C2
        DM(2,1,EP)=DM(1,2,EP)
        GM(EP) =HM(EP,4)*C2
        DF(1,1,EP)=HF(EP,1)*C1
        DF(2,2,EP)=HF(EP,2)*C1
        DF(1,2,EP)=HF(EP,3)*C1
        DF(2,1,EP)=DF(1,2,EP)
        GF(EP) =HF(EP,4)*C1
        DHZ(EP)=HZ(EP)*C1
        DCX(EP)=HC(EP,1)*C2
        DCY(EP)=HC(EP,2)*C2
       ENDDO
       DO EP=JFT,JLT
C-------PX2=-PX1 ,PX3=0---------------       
        PY3(EP)= -PY1(EP)-PY2(EP)
        PX1PX1(EP)=PX1(EP)*PX1(EP)
        PX1PY1(EP)=PX1(EP)*PY1(EP)
        PX1PY2(EP)=PX1(EP)*PY2(EP)
        PX1PY3(EP)=-PX1PY1(EP)-PX1PY2(EP)
        PY1PY1(EP)=PY1(EP)*PY1(EP)
        PY1PY2(EP)=PY1(EP)*PY2(EP)
        PY1PY3(EP)=-PY1PY1(EP)-PY1PY2(EP)
        PY2PY2(EP)=PY2(EP)*PY2(EP)
        PY2PY3(EP)=PY2(EP)*PY3(EP)
        PY3PY3(EP)=PY3(EP)*PY3(EP)
       ENDDO
C------Membrainaires-----------
       DO EP=JFT,JLT 
        K11(1,1,EP)=PX1PX1(EP)*DM(1,1,EP)
        K11(1,2,EP)=PX1PY1(EP)*DM(1,2,EP)
        K11(2,2,EP)=PY1PY1(EP)*DM(2,2,EP)
        K22(1,1,EP)=K11(1,1,EP)
        K22(1,2,EP)=-PX1PY2(EP)*DM(1,2,EP)
        K22(2,2,EP)=PY2PY2(EP)*DM(2,2,EP)
        K33(2,2,EP)=PY3PY3(EP)*DM(2,2,EP)
C
        K12(1,1,EP)=-K11(1,1,EP)
        K12(1,2,EP)=-K22(1,2,EP)
        K12(2,1,EP)=-K11(1,2,EP)
        K12(2,2,EP)=PY1PY2(EP)*DM(2,2,EP)
        K13(1,2,EP)=PX1PY3(EP)*DM(1,2,EP)
        K13(2,2,EP)=PY1PY3(EP)*DM(2,2,EP)
        K23(1,2,EP)=-K13(1,2,EP)
        K23(2,2,EP)=PY2PY3(EP)*DM(2,2,EP)
       ENDDO
C------Flexion-----------
       DO EP=JFT,JLT 
        M11(2,2,EP)=PX1PX1(EP)*DF(1,1,EP)
        M11(1,2,EP)=-PX1PY1(EP)*DF(1,2,EP)
        M11(1,1,EP)=PY1PY1(EP)*DF(2,2,EP)
        M22(2,2,EP)=M11(2,2,EP)
        M22(1,2,EP)=PX1PY2(EP)*DF(1,2,EP)
        M22(1,1,EP)=PY2PY2(EP)*DF(2,2,EP)
        M33(1,1,EP)=PY3PY3(EP)*DF(2,2,EP)
C
        M12(2,2,EP)=-M11(2,2,EP)
        M12(1,2,EP)=-M11(1,2,EP)
        M12(2,1,EP)=-M22(1,2,EP)
        M12(1,1,EP)=PY1PY2(EP)*DF(2,2,EP)
        M13(2,1,EP)=-PX1PY3(EP)*DF(1,2,EP)
        M13(1,1,EP)=PY1PY3(EP)*DF(2,2,EP)
        M23(2,1,EP)=-M13(2,1,EP)
        M23(1,1,EP)=PY2PY3(EP)*DF(2,2,EP)
       ENDDO
C------G terms-----------
       DO EP=JFT,JLT 
        GPX1PX1=PX1PX1(EP)*GM(EP)
        GPX1PY1=PX1PY1(EP)*GM(EP)
        GPX1PY2=PX1PY2(EP)*GM(EP)
        GPX1PY3=-GPX1PY1-GPX1PY2
        K11(1,1,EP)=K11(1,1,EP)+PY1PY1(EP)*GM(EP)
        K11(1,2,EP)=K11(1,2,EP)+GPX1PY1
        K11(2,2,EP)=K11(2,2,EP)+GPX1PX1
        K22(1,1,EP)=K22(1,1,EP)+PY2PY2(EP)*GM(EP)
        K22(1,2,EP)=K22(1,2,EP)-GPX1PY2
        K22(2,2,EP)=K22(2,2,EP)+GPX1PX1
        K33(1,1,EP)=K33(1,1,EP)+PY3PY3(EP)*GM(EP)
C
        K12(1,1,EP)=K12(1,1,EP)+PY1PY2(EP)*GM(EP)
        K12(1,2,EP)=K12(1,2,EP)-GPX1PY1
        K12(2,1,EP)=K12(2,1,EP)+GPX1PY2
        K12(2,2,EP)=K12(2,2,EP)-GPX1PX1
        K13(1,1,EP)=K13(1,1,EP)+PY1PY3(EP)*GM(EP)
        K13(2,1,EP)=K13(2,1,EP)+GPX1PY3
        K23(1,1,EP)=K23(1,1,EP)+PY2PY3(EP)*GM(EP)
        K23(2,1,EP)=K23(2,1,EP)-GPX1PY3
       ENDDO
C
       DO EP=JFT,JLT 
        GPX1PX1=PX1PX1(EP)*GF(EP)
        GPX1PY1=PX1PY1(EP)*GF(EP)
        GPX1PY2=PX1PY2(EP)*GF(EP)
        GPX1PY3=-GPX1PY1-GPX1PY2
        M11(2,2,EP)=M11(2,2,EP)+PY1PY1(EP)*GF(EP)
        M11(1,2,EP)=M11(1,2,EP)-GPX1PY1
        M11(1,1,EP)=M11(1,1,EP)+GPX1PX1
        M22(2,2,EP)=M22(2,2,EP)+PY2PY2(EP)*GF(EP)
        M22(1,2,EP)=M22(1,2,EP)+GPX1PY2
        M22(1,1,EP)=M22(1,1,EP)+GPX1PX1
        M33(2,2,EP)=M33(2,2,EP)+PY3PY3(EP)*GF(EP)
C
        M12(2,2,EP)=M12(2,2,EP)+PY1PY2(EP)*GF(EP)
        M12(1,2,EP)=M12(1,2,EP)-GPX1PY2
        M12(2,1,EP)=M12(2,1,EP)+GPX1PY1
        M12(1,1,EP)=M12(1,1,EP)-GPX1PX1
        M13(1,2,EP)=M13(1,2,EP)-GPX1PY3
        M13(2,2,EP)=M13(2,2,EP)+PY1PY3(EP)*GF(EP)
        M23(1,2,EP)=M23(1,2,EP)+GPX1PY3
        M23(2,2,EP)=M23(2,2,EP)+PY2PY3(EP)*GF(EP)
       ENDDO
       IF (IORTH.EQ.1) THEN
#include "vectorize.inc"
        DO EP=JFT,JLT 
         C2=VOL(EP)
         C1=THK2(EP)*C2
         DM5(EP)=HMOR(EP,1)*C2
         DM6(EP)=HMOR(EP,2)*C2
         DF5(EP)=HFOR(EP,1)*C1
         DF6(EP)=HFOR(EP,2)*C1
        ENDDO
        DO EP=JFT,JLT 
         DM5_2(EP)=DEUX*DM5(EP)
         DM6_2(EP)=DEUX*DM6(EP)
         DF5_2(EP)=DEUX*DF5(EP)
         DF6_2(EP)=DEUX*DF6(EP)
        ENDDO
C----Px2=-Px1,--Px3=0---
        DO EP=JFT,JLT
         K11(1,1,EP)=K11(1,1,EP)+PX1PY1(EP)*DM5_2(EP)
         K11(1,2,EP)=K11(1,2,EP)+
     1               PX1PX1(EP)*DM5(EP)+PY1PY1(EP)*DM6(EP)
         K11(2,2,EP)=K11(2,2,EP)+PX1PY1(EP)*DM6_2(EP)
         K12(1,1,EP)=K12(1,1,EP)+(PX1PY2(EP)-PX1PY1(EP))*DM5(EP)
         C1 = -PX1PX1(EP)*DM5(EP)+PY1PY2(EP)*DM6(EP)
         K12(1,2,EP)=K12(1,2,EP)+C1
         K12(2,1,EP)=K12(2,1,EP)+C1
         K12(2,2,EP)=K12(2,2,EP)+(PX1PY2(EP)-PX1PY1(EP))*DM6(EP)
         K13(1,1,EP)=K13(1,1,EP)+PX1PY3(EP)*DM5(EP)
         C1 = PY1PY3(EP)*DM6(EP)
         K13(1,2,EP)=K13(1,2,EP)+C1
         K13(2,1,EP)=K13(2,1,EP)+C1
         K13(2,2,EP)=K13(2,2,EP)+PX1PY3(EP)*DM6(EP)
C
         K22(1,1,EP)=K22(1,1,EP)-PX1PY2(EP)*DM5_2(EP)
         K22(1,2,EP)=K22(1,2,EP)+
     1               PX1PX1(EP)*DM5(EP)+PY2PY2(EP)*DM6(EP)
         K22(2,2,EP)=K22(2,2,EP)-PX1PY2(EP)*DM6_2(EP)
         K23(1,1,EP)=K23(1,1,EP)-PX1PY3(EP)*DM5(EP)
C
         C1 = PY2PY3(EP)*DM6(EP)
         K23(1,2,EP)=K23(1,2,EP)+C1
         K23(2,1,EP)=K23(2,1,EP)+C1
         K23(2,2,EP)=K23(2,2,EP)-PX1PY3(EP)*DM6(EP)
C
         K33(1,2,EP)=K33(1,2,EP)+PY3PY3(EP)*DM6(EP)
C
         M11(1,1,EP)=M11(1,1,EP)+PX1PY1(EP)*DF6_2(EP)
         M11(1,2,EP)=M11(1,2,EP)-
     1               PX1PX1(EP)*DF5(EP)-PY1PY1(EP)*DF6(EP)
         M11(2,2,EP)=M11(2,2,EP)+PX1PY1(EP)*DF5_2(EP)
         M12(1,1,EP)=M12(1,1,EP)+(PX1PY2(EP)-PX1PY1(EP))*DF6(EP)
         C2 = -PX1PX1(EP)*DF5(EP)+PY1PY2(EP)*DF6(EP)
         M12(1,2,EP)=M12(1,2,EP)-C2
         M12(2,1,EP)=M12(2,1,EP)-C2
         M12(2,2,EP)=M12(2,2,EP)+(PX1PY2(EP)-PX1PY1(EP))*DF5(EP)
         M13(1,1,EP)=M13(1,1,EP)+PX1PY3(EP)*DF6(EP)
         M13(1,2,EP)=M13(1,2,EP)-PY1PY3(EP)*DF6(EP)
         M13(2,1,EP)=M13(2,1,EP)-PY1PY3(EP)*DF6(EP)
         M13(2,2,EP)=M13(2,2,EP)+PX1PY3(EP)*DF5(EP)
         M22(1,1,EP)=M22(1,1,EP)-PX1PY2(EP)*DF6_2(EP)
         M22(1,2,EP)=M22(1,2,EP)-
     1               PX1PX1(EP)*DF5(EP)-PY2PY2(EP)*DF6(EP)
         M22(2,2,EP)=M22(2,2,EP)-PX1PY2(EP)*DF5_2(EP)
         M23(1,1,EP)=M23(1,1,EP)-PX1PY3(EP)*DF6(EP)
         M23(1,2,EP)=M23(1,2,EP)-PY2PY3(EP)*DF6(EP)
         M23(2,1,EP)=M23(2,1,EP)-PY2PY3(EP)*DF6(EP)
         M23(2,2,EP)=M23(2,2,EP)-PX1PY3(EP)*DF5(EP)
         M33(1,2,EP)=M33(1,2,EP)-PY3PY3(EP)*DF6(EP)
        ENDDO
        DO EP=JFT,JLT 
 	     C2=VOL(EP)*THK0(EP)
         DMF(1,1,EP)=HMFOR(EP,1)*C2
         DMF(2,2,EP)=HMFOR(EP,2)*C2
         DMF(1,2,EP)=HMFOR(EP,3)*C2
         DMF(1,3,EP)=HMFOR(EP,5)*C2
         DMF(2,3,EP)=HMFOR(EP,6)*C2
         DMF(2,1,EP)=DMF(1,2,EP)
         DMF(3,1,EP)=DMF(1,3,EP)
         DMF(3,2,EP)=DMF(2,3,EP)
         DMF(3,3,EP)=HMFOR(EP,4)*C2
        ENDDO
C ----add mem-bending coupling terms of orthotrope--coplanar first---
        DO EP=JFT,JLT 
         MF11(1,1,EP)=
     1               -DMF(1,2,EP)*PX1PY1(EP)-DMF(1,3,EP)*PX1PX1(EP)
     2               -DMF(2,3,EP)*PY1PY1(EP)-DMF(3,3,EP)*PX1PY1(EP)
         MF11(1,2,EP)=
     1                DMF(1,1,EP)*PX1PX1(EP)+DMF(1,3,EP)*PX1PY1(EP)
     2               +DMF(1,3,EP)*PX1PY1(EP)+DMF(3,3,EP)*PY1PY1(EP)
         MF11(2,1,EP)=
     1               -DMF(2,2,EP)*PY1PY1(EP)-DMF(2,3,EP)*PX1PY1(EP)
     2               -DMF(2,3,EP)*PX1PY1(EP)-DMF(3,3,EP)*PX1PX1(EP)
         MF11(2,2,EP)=
     1                DMF(1,2,EP)*PX1PY1(EP)+DMF(2,3,EP)*PY1PY1(EP)
     2               +DMF(1,3,EP)*PX1PX1(EP)+DMF(3,3,EP)*PX1PY1(EP)
        ENDDO
        DO EP=JFT,JLT 
         MF12(1,1,EP)=
     1               -DMF(1,2,EP)*PX1PY2(EP)+DMF(1,3,EP)*PX1PX1(EP)
C--------------------------------------------------------YIXJ-----     
     2               -DMF(2,3,EP)*PY1PY2(EP)+DMF(3,3,EP)*PX1PY1(EP)
         MF12(1,2,EP)=
     1               -DMF(1,1,EP)*PX1PX1(EP)+DMF(1,3,EP)*PX1PY2(EP)
C-------------------------YIXJ-----     
     2               -DMF(1,3,EP)*PX1PY1(EP)+DMF(3,3,EP)*PY1PY2(EP)
         MF12(2,1,EP)=
C---------------------------------------------------------YIXJ-----     
     1               -DMF(2,2,EP)*PY1PY2(EP)+DMF(2,3,EP)*PX1PY1(EP)
     2               -DMF(2,3,EP)*PX1PY2(EP)+DMF(3,3,EP)*PX1PX1(EP)
         MF12(2,2,EP)=
C---------------------------------YIXJ-----     
     1               -DMF(1,2,EP)*PX1PY1(EP)+DMF(2,3,EP)*PY1PY2(EP)
     2               -DMF(1,3,EP)*PX1PX1(EP)+DMF(3,3,EP)*PX1PY2(EP)
        ENDDO
        DO EP=JFT,JLT 
         MF22(1,1,EP)=
     1                DMF(1,2,EP)*PX1PY2(EP)-DMF(1,3,EP)*PX1PX1(EP)
C--------------------------------------------------------YIXJ-----     
     2               -DMF(2,3,EP)*PY2PY2(EP)+DMF(3,3,EP)*PX1PY2(EP)
         MF22(1,2,EP)=
     1                DMF(1,1,EP)*PX1PX1(EP)-DMF(1,3,EP)*PX1PY2(EP)
C-------------------------YIXJ-----     
     2               -DMF(1,3,EP)*PX1PY2(EP)+DMF(3,3,EP)*PY2PY2(EP)
         MF22(2,1,EP)=
C---------------------------------------------------------YIXJ-----     
     1               -DMF(2,2,EP)*PY2PY2(EP)+DMF(2,3,EP)*PX1PY2(EP)
     2               +DMF(2,3,EP)*PX1PY2(EP)-DMF(3,3,EP)*PX1PX1(EP)
         MF22(2,2,EP)=
C---------------------------------YIXJ-----     
     1               -DMF(1,2,EP)*PX1PY2(EP)+DMF(2,3,EP)*PY2PY2(EP)
     2               +DMF(1,3,EP)*PX1PX1(EP)-DMF(3,3,EP)*PX1PY2(EP)
        ENDDO
        DO EP=JFT,JLT 
         MF23(1,1,EP)=
     1                DMF(1,2,EP)*PX1PY3(EP)
C--------------------------------------------------------YIXJ-----     
     2               -DMF(2,3,EP)*PY2PY3(EP)
         MF23(1,2,EP)=
     1                                      -DMF(1,3,EP)*PX1PY3(EP)
C-------------------------YIXJ-----     
     2                                      +DMF(3,3,EP)*PY2PY3(EP)
         MF23(2,1,EP)=
C---------------------------------------------------------YIXJ-----     
     1               -DMF(2,2,EP)*PY2PY3(EP)
     2               +DMF(2,3,EP)*PX1PY3(EP)
         MF23(2,2,EP)=
C---------------------------------YIXJ-----     
     1                                       DMF(2,3,EP)*PY2PY3(EP)
     2                                      -DMF(3,3,EP)*PX1PY3(EP)
        ENDDO
        DO EP=JFT,JLT 
         MF13(1,1,EP)=
     1               -DMF(1,2,EP)*PX1PY3(EP)
C--------------------------------------------------------YIXJ-----     
     2               -DMF(2,3,EP)*PY1PY3(EP)
         MF13(1,2,EP)=
     1                                       DMF(1,3,EP)*PX1PY3(EP)
C-------------------------YIXJ-----     
     2                                      +DMF(3,3,EP)*PY1PY3(EP)
         MF13(2,1,EP)=
C---------------------------------------------------------YIXJ-----     
     1               -DMF(2,2,EP)*PY1PY3(EP)
     2               -DMF(2,3,EP)*PX1PY3(EP)
         MF13(2,2,EP)=
C---------------------------------YIXJ-----     
     1                                       DMF(2,3,EP)*PY1PY3(EP)
     2                                      +DMF(3,3,EP)*PX1PY3(EP)
        ENDDO
        DO EP=JFT,JLT 
         MF33(1,1,EP)=
     2               -DMF(2,3,EP)*PY3PY3(EP)
         MF33(1,2,EP)=
     2                                       DMF(3,3,EP)*PY3PY3(EP)
         MF33(2,1,EP)=
     1               -DMF(2,2,EP)*PY3PY3(EP)
         MF33(2,2,EP)=
     1                                       DMF(2,3,EP)*PY3PY3(EP)
        ENDDO
C---------FMIJ(i,j)=MFJI(j,i)-----------        
        DO EP=JFT,JLT 
         FM12(1,1,EP)=
     1                DMF(1,2,EP)*PX1PY1(EP)+DMF(1,3,EP)*PX1PX1(EP)
C--------------------------------------------------------YIXJ-----     
     2               -DMF(2,3,EP)*PY1PY2(EP)-DMF(3,3,EP)*PX1PY2(EP)
         FM12(2,1,EP)=
     1               -DMF(1,1,EP)*PX1PX1(EP)-DMF(1,3,EP)*PX1PY1(EP)
c     1               -DMF(1,1,EP)*PX1PX1(EP)+DMF(1,3,EP)*PX1PY2(EP)
C-------------------------YIXJ-----     
     2               +DMF(1,3,EP)*PX1PY2(EP)+DMF(3,3,EP)*PY1PY2(EP)
         FM12(1,2,EP)=
C---------------------------------------------------------YIXJ-----     
     1               -DMF(2,2,EP)*PY1PY2(EP)-DMF(2,3,EP)*PX1PY2(EP)
     2               +DMF(2,3,EP)*PX1PY1(EP)+DMF(3,3,EP)*PX1PX1(EP)
         FM12(2,2,EP)=
C---------------------------------YIXJ-----     
     1                DMF(1,2,EP)*PX1PY2(EP)+DMF(2,3,EP)*PY1PY2(EP)
     2               -DMF(1,3,EP)*PX1PX1(EP)-DMF(3,3,EP)*PX1PY1(EP)
        ENDDO
        DO EP=JFT,JLT 
         FM13(1,1,EP)=
c     1               -DMF(1,2,EP)*PX3PY1(EP)-DMF(1,3,EP)*PX3PX1(EP)
C--------------------------------------------------------YIXJ-----     
     2               -DMF(2,3,EP)*PY1PY3(EP)-DMF(3,3,EP)*PX1PY3(EP)
         FM13(2,1,EP)=
c     1               +DMF(1,1,EP)*PX3PX1(EP)+DMF(1,3,EP)*PX3PY1(EP)
C-------------------------YIXJ-----     
     2                DMF(1,3,EP)*PX1PY3(EP)+DMF(3,3,EP)*PY1PY3(EP)
         FM13(1,2,EP)=
C---------------------------------------------------------YIXJ-----     
     1               -DMF(2,2,EP)*PY1PY3(EP)-DMF(2,3,EP)*PX1PY3(EP)
c     2               -DMF(2,3,EP)*PX3PY1(EP)-DMF(3,3,EP)*PX3PX1(EP)
         FM13(2,2,EP)=
C---------------------------------YIXJ-----     
     1                DMF(1,2,EP)*PX1PY3(EP)+DMF(2,3,EP)*PY1PY3(EP)
c     2               +DMF(1,3,EP)*PX3PX1(EP)+DMF(3,3,EP)*PX3PY1(EP)
        ENDDO
        DO EP=JFT,JLT 
         FM23(1,1,EP)=
c     1               -DMF(1,2,EP)*PX3PY2(EP)-DMF(1,3,EP)*PX2PX3(EP)
C--------------------------------------------------------YIXJ-----     
     2               -DMF(2,3,EP)*PY2PY3(EP)+DMF(3,3,EP)*PX1PY3(EP)
         FM23(2,1,EP)=
c     1                DMF(1,1,EP)*PX2PX3(EP)+DMF(1,3,EP)*PX3PY2(EP)
C-------------------------YIXJ-----     
     2               -DMF(1,3,EP)*PX1PY3(EP)+DMF(3,3,EP)*PY2PY3(EP)
         FM23(1,2,EP)=
C---------------------------------------------------------YIXJ-----     
     1               -DMF(2,2,EP)*PY2PY3(EP)+DMF(2,3,EP)*PX1PY3(EP)
c     2               -DMF(2,3,EP)*PX3PY2(EP)-DMF(3,3,EP)*PX2PX3(EP)
         FM23(2,2,EP)=
C---------------------------------YIXJ-----     
     1               -DMF(1,2,EP)*PX1PY3(EP)+DMF(2,3,EP)*PY2PY3(EP)
c     2               +DMF(1,3,EP)*PX2PX3(EP)+DMF(3,3,EP)*PX3PY2(EP)
        ENDDO
       ENDIF 
C-------------C.T.---------------
       DO EP=JFT,JLT 
        GPX1PX1=PX1PX1(EP)*DCX(EP)
        K11(3,3,EP)=GPX1PX1+PY1PY1(EP)*DCY(EP)
        K22(3,3,EP)=GPX1PX1+PY2PY2(EP)*DCY(EP)
        K33(3,3,EP)=PY3PY3(EP)*DCY(EP)
        K12(3,3,EP)=-GPX1PX1+PY1PY2(EP)*DCY(EP)
        K13(3,3,EP)=PY1PY3(EP)*DCY(EP)
        K23(3,3,EP)=PY2PY3(EP)*DCY(EP)
       ENDDO
       DO EP=JFT,JLT 
        FAC = THIRD*AREA(EP)
        BCXX1=-FAC*PX1PX1(EP)
        BCXX2= -BCXX1
        BCXY3= FAC*(PX1PY1(EP)+PX1PY2(EP))
        BCXY1= BCXY3+BCXY3+FAC*PX1PY2(EP)
        BCXY2= BCXY3+BCXY3+FAC*PX1PY1(EP)
        BCYY1= FAC*(PY1PY1(EP)+PY1PY2(EP)+PY1PY2(EP))
        BCYY2= -FAC*(PY2PY2(EP)+PY1PY2(EP)+PY1PY2(EP))
        BCYY3= -BCYY1-BCYY2
        BCYX1= -BCXY3-FAC*PX1PY1(EP)
        BCYX2= -BCXY3-FAC*PX1PY2(EP)
        BCYX3= BCYX1+BCYX2
        M11(1,1,EP)=M11(1,1,EP)+BCXX1*BCXX1*DCX(EP)+BCYX1*BCYX1*DCY(EP)
        M11(1,2,EP)=M11(1,2,EP)+BCXX1*BCXY1*DCX(EP)+BCYX1*BCYY1*DCY(EP)
        M11(2,2,EP)=M11(2,2,EP)+BCXY1*BCXY1*DCX(EP)+BCYY1*BCYY1*DCY(EP)
        M22(1,1,EP)=M22(1,1,EP)+BCXX2*BCXX2*DCX(EP)+BCYX2*BCYX2*DCY(EP)
        M22(1,2,EP)=M22(1,2,EP)+BCXX2*BCXY2*DCX(EP)+BCYX2*BCYY2*DCY(EP)
        M22(2,2,EP)=M22(2,2,EP)+BCXY2*BCXY2*DCX(EP)+BCYY2*BCYY2*DCY(EP)
        M33(1,1,EP)=M33(1,1,EP)+BCYX3*BCYX3*DCY(EP)
        M33(1,2,EP)=M33(1,2,EP)+BCYX3*BCYY3*DCY(EP)
        M33(2,2,EP)=M33(2,2,EP)+BCXY3*BCXY3*DCX(EP)+BCYY3*BCYY3*DCY(EP)
        M12(1,1,EP)=M12(1,1,EP)+BCXX1*BCXX2*DCX(EP)+BCYX1*BCYX2*DCY(EP)
        M12(1,2,EP)=M12(1,2,EP)+BCXX1*BCXY2*DCX(EP)+BCYX1*BCYY2*DCY(EP)
        M12(2,1,EP)=M12(2,1,EP)+BCXX2*BCXY1*DCX(EP)+BCYX2*BCYY1*DCY(EP)
        M12(2,2,EP)=M12(2,2,EP)+BCXY1*BCXY2*DCX(EP)+BCYY1*BCYY2*DCY(EP)
        M13(1,1,EP)=M13(1,1,EP)+BCYX1*BCYX3*DCY(EP)
        M13(1,2,EP)=M13(1,2,EP)+BCXX1*BCXY3*DCX(EP)+BCYX1*BCYY3*DCY(EP)
        M13(2,1,EP)=M13(2,1,EP)+BCYX3*BCYY1*DCY(EP)
        M13(2,2,EP)=M13(2,2,EP)+BCXY1*BCXY3*DCX(EP)+BCYY1*BCYY3*DCY(EP)
        M23(1,1,EP)=M23(1,1,EP)+BCYX2*BCYX3*DCY(EP)
        M23(1,2,EP)=M23(1,2,EP)+BCXX2*BCXY3*DCX(EP)+BCYX2*BCYY3*DCY(EP)
        M23(2,1,EP)=M23(2,1,EP)+BCYX3*BCYY2*DCY(EP)
        M23(2,2,EP)=M23(2,2,EP)+BCXY2*BCXY3*DCX(EP)+BCYY2*BCYY3*DCY(EP)
C---
        PX1DX=PX1(EP)*DCX(EP)
        PY1DY=PY1(EP)*DCY(EP)
        PY2DY=PY2(EP)*DCY(EP)
        PY3DY=PY3(EP)*DCY(EP)
        MF11(3,1,EP)=BCXX1*PX1DX+BCYX1*PY1DY
        MF11(3,2,EP)=BCXY1*PX1DX+BCYY1*PY1DY
        MF22(3,1,EP)=-BCXX2*PX1DX+BCYX2*PY2DY
        MF22(3,2,EP)=-BCXY2*PX1DX+BCYY2*PY2DY
        MF33(3,1,EP)=BCYX3*PY3DY
        MF33(3,2,EP)=BCYY3*PY3DY
        MF12(3,1,EP)=BCXX2*PX1DX+BCYX2*PY1DY
        MF12(3,2,EP)=BCXY2*PX1DX+BCYY2*PY1DY
        MF13(3,1,EP)=BCYX3*PY1DY
        MF13(3,2,EP)=BCXY3*PX1DX+BCYY3*PY1DY
        MF23(3,1,EP)=BCYX3*PY2DY
        MF23(3,2,EP)=-BCXY3*PX1DX+BCYY3*PY2DY
        FM12(1,3,EP)=-BCXX1*PX1DX+BCYX1*PY2DY
        FM12(2,3,EP)=-BCXY1*PX1DX+BCYY1*PY2DY
        FM13(1,3,EP)=BCYX1*PY3DY
        FM13(2,3,EP)=BCYY1*PY3DY
        FM23(1,3,EP)=BCYX2*PY3DY
        FM23(2,3,EP)=BCYY2*PY3DY
       ENDDO
C
C------ Mzz  pour tous-------
        DO EP=JFT,JLT
         M11(3,3,EP)=M11(3,3,EP)+DHZ(EP)*(PX1PX1(EP)+PY1PY1(EP))
         M12(3,3,EP)=M12(3,3,EP)+DHZ(EP)*(PY1PY2(EP)-PX1PX1(EP))
         M13(3,3,EP)=M13(3,3,EP)+DHZ(EP)*PY1PY3(EP)
         M22(3,3,EP)=M22(3,3,EP)+DHZ(EP)*(PX1PX1(EP)+PY2PY2(EP))
         M23(3,3,EP)=M23(3,3,EP)+DHZ(EP)*PY2PY3(EP)
         M33(3,3,EP)=M33(3,3,EP)+DHZ(EP)*PY3PY3(EP)
       ENDDO
C------ renforce positive  ----------
      IF (NEIG.EQ.0) THEN
       DO EP=JFT,JLT
        C1 = EM8*M11(3,3,EP)
        C2 = EM8*M22(3,3,EP)
        C3 = EM8*M33(3,3,EP)
        M11(3,3,EP)=M11(3,3,EP)+C1
        M22(3,3,EP)=M22(3,3,EP)+C2
        M33(3,3,EP)=M33(3,3,EP)+C3
       ENDDO
      ENDIF 
C
        IF (IKGEO == 1 ) THEN
C---------membrane--only-----
        DO EP=JFT,JLT
         C2=VOL(EP)
         FXX=FOR(EP,1)*C2
         FYY=FOR(EP,2)*C2
         FXY=FOR(EP,3)*C2
         FXY2=DEUX*FXY
         H11(EP)=FXX*PX1PX1(EP)+FYY*PY1PY1(EP)+FXY2*PX1PY1(EP)
         H12(EP)=-FXX*PX1PX1(EP)+FYY*PY1PY2(EP)
     .           +FXY*(PX1PY2(EP)-PX1PY1(EP))
         H22(EP)=FXX*PX1PX1(EP)+FYY*PY2PY2(EP)-FXY2*PX1PY2(EP)
         H13(EP)=FYY*PY1PY3(EP)+FXY*PX1PY3(EP)
         H23(EP)=FYY*PY2PY3(EP)-FXY*PX1PY3(EP)
         H33(EP)=FYY*PY3PY3(EP)
        ENDDO
       DO I=1,3
        DO EP=JFT,JLT
         K11(I,I,EP) = K11(I,I,EP)+H11(EP)
         K12(I,I,EP) = K12(I,I,EP)+H12(EP)
         K22(I,I,EP) = K22(I,I,EP)+H22(EP)
         K13(I,I,EP) = K13(I,I,EP)+H13(EP)
         K23(I,I,EP) = K23(I,I,EP)+H23(EP) 
         K33(I,I,EP) = K33(I,I,EP)+H33(EP)
        ENDDO
       ENDDO
C------ renforce positive  ----------
        IF (NEIG.EQ.0.AND.IDRIL==0.AND.IORTH>0) THEN
         DO EP=JFT,JLT
          C1 = MIN(H11(EP),H22(EP),H33(EP))
          IF (C1 < ZERO) THEN
           C2 =MIN(M11(3,3,EP),M22(3,3,EP),M33(3,3,EP))
           C1 = MIN(-C1,DIX*C2)
           M11(3,3,EP)=M11(3,3,EP) + C1
           M22(3,3,EP)=M22(3,3,EP) + C1
           M33(3,3,EP)=M33(3,3,EP) + C1
          END IF
         ENDDO
        ENDIF
       END IF !IF (IKGEO ==1) 
C
       RETURN
       END
Chd|====================================================================
Chd|  C3LKERZ3                      source/elements/sh3n/coque3n/c3lke3.F
Chd|-- called by -----------
Chd|        C3KE3                         source/elements/sh3n/coque3n/c3ke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C3LKERZ3(JFT,JLT,HM,HZ,
     1                   PX1,PY1,PY2,VOL,AREA,
     2                   K11,K12,K13,K22,K23,K33,
     3                   M11,M12,M13,M22,M23,M33,
     4                   MF11,MF12,MF13,MF22,MF23,MF33,
     5                   FM12,FM13,FM23,IORTH,HMOR,
     6                   BM0RZ,B0RZ,BKRZ,BERZ,THK0,HMFOR )
C--------------------------------------------------------------------------------------------------
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
      INTEGER JFT,JLT,IORTH
      MY_REAL
     .    VOL(*), PX1(*),PY1(*),PY2(*),AREA(*),THK0(*),
     .    HM(MVSIZ,4),HZ(*),HMOR(MVSIZ,2),
     .    K11(3,3,*),K12(3,3,*),K13(3,3,*),
     .    K22(3,3,*),K23(3,3,*),K33(3,3,*),
     .    M11(3,3,*),M12(3,3,*),M13(3,3,*),
     .    M22(3,3,*),M23(3,3,*),M33(3,3,*),
     .    MF11(3,3,*),MF12(3,3,*),MF13(3,3,*),
     .    MF22(3,3,*),MF23(3,3,*),MF33(3,3,*),
     .    FM12(3,3,*),FM13(3,3,*),FM23(3,3,*),
     .    BM0RZ(MVSIZ,3,2),B0RZ(MVSIZ,3),BKRZ(MVSIZ,2),BERZ(MVSIZ,2),HMFOR(MVSIZ,6)
C-----------------------------------------------
c FUNCTION: elementary sub-stiffness matrix relative to the drilling dof for Tria
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   JFT,JLT           - element id limit
c  I   HM(4,NEL)         - membrane stiffness modulus (plane stress)
c  I   HZ(NEL)           -drilling dof modulus
C  I   PX1,PY1,PX2=-PX1,PY2(NEL): standard [B] of Tria
c  I   VOL,AREA          - element volume,AREA
c  IO  Kij,FMij,MFij,Mij(3,3,NEL)   sub-stiffness matrix
C---------------|[KIJ][MFIJ]|----
C-----KE(6x6)=  |           |
C---------------|[FMIJ]{MIJ]|----
c  I   IORTH             - flag for orthotropic mat
c  I   HMOR(2,NEL)       - supplementary membrane stiffness modulus for orth
c  O   BM0RZ(I,J,NEL)   - constant terms of derivations for membrane
C                       I=1:A*Nx,x;I=2:A*Ny,y;I=3:A*(Nx,y+Ny,x); J=1,2(node)
C                        only store J=1,2 as f(j=3)=-f(j=1)-f(j=2)
C  O   B0RZ(J,NEL)       A*(-Nx,y+Ny,x -2Ni) for asymmetric rotation
c  O   BKRZ(J,NEL)     - Ksi terms of derivation : A*(-Nx,y+Ny,x -2Ni)
c  O   BERZ(J,NEL)    - Eta terms of derivation : A*(-Nx,y+Ny,x -2Ni)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER EP,I,J,NG,NPG
      MY_REAL
     .    DM(3,3,MVSIZ),
     .    PX1PX1(MVSIZ),PX1PY1(MVSIZ),PX1PY2(MVSIZ),PX1PY3(MVSIZ),
     .    PY1PY1(MVSIZ),PY1PY2(MVSIZ),PY1PY3(MVSIZ),PY3(MVSIZ),
     .    PY2PY2(MVSIZ),PY2PY3(MVSIZ),PY3PY3(MVSIZ),
     .    C1,C2,DHZ(MVSIZ),C3,
     .    KZ11(2,2,MVSIZ),KZ12(2,2,MVSIZ),KZ22(2,2,MVSIZ),
     .    KZ13(2,2,MVSIZ),KZ23(2,2,MVSIZ),KZ33(2,2,MVSIZ)
       PARAMETER (NPG = 3)
        my_real
     .     DPRZ(3,MVSIZ),A_HAMMER(NPG,2),BN1RZ,BN2RZ,BN3RZ,DZ12(MVSIZ),
     .     PRZ(3,MVSIZ),PRX(3,MVSIZ),PRY(3,MVSIZ),PRXY(3,MVSIZ),
     .     CBRX(3,MVSIZ),CBRY(3,MVSIZ),CBRZ(3,MVSIZ),A_I(MVSIZ),
     .     DMF(3,3,MVSIZ)
      DATA A_HAMMER /
     1 0.166666666666667,0.666666666666667,0.166666666666667,
     2 0.166666666666667,0.166666666666667,0.666666666666667/
C---------------|[KIJ][MFIJ]|----
C-----KE(6x6)=  |           |
C---------------|[FMIJ]{MIJ]|----
C-----------Attention Matrice sym Kii ne calcul que la moitie---------72
C--------Bi: Bi*A--- DM: DM/A
       DO EP=JFT,JLT
        A_I(EP)=UN/MAX(AREA(EP),EM20)
       ENDDO
       DO EP=JFT,JLT
        C2=VOL(EP)
        DM(1,1,EP)=HM(EP,1)*C2
        DM(2,2,EP)=HM(EP,2)*C2
        DM(1,2,EP)=HM(EP,3)*C2
        DM(2,1,EP)=DM(1,2,EP)
        DM(3,3,EP)=HM(EP,4)*C2
        DHZ(EP)=HZ(EP)*QUART*C2
        DZ12(EP)=DHZ(EP)*THIRD
        DM(1,3,EP)=ZERO
        DM(2,3,EP)=ZERO
       ENDDO
       IF (IORTH.EQ.1) THEN
        DO EP=JFT,JLT
         C2=VOL(EP)
         DM(1,3,EP)=HMOR(EP,1)*C2
         DM(2,3,EP)=HMOR(EP,2)*C2
        ENDDO
       ENDIF
       DO EP=JFT,JLT
        DM(3,1,EP)=DM(1,3,EP)
        DM(3,2,EP)=DM(2,3,EP)
       ENDDO
C------ PX2=-PX1 , PX3=0-------------
       DO EP=JFT,JLT
        PY3(EP)= -PY1(EP)-PY2(EP)
        PX1PX1(EP)=PX1(EP)*PX1(EP)
        PX1PY1(EP)=PX1(EP)*PY1(EP)
        PX1PY2(EP)=PX1(EP)*PY2(EP)
        PX1PY3(EP)=-PX1PY1(EP)-PX1PY2(EP)
        PY1PY1(EP)=PY1(EP)*PY1(EP)
        PY1PY2(EP)=PY1(EP)*PY2(EP)
        PY1PY3(EP)=-PY1PY1(EP)-PY1PY2(EP)
        PY2PY2(EP)=PY2(EP)*PY2(EP)
        PY2PY3(EP)=PY2(EP)*PY3(EP)
        PY3PY3(EP)=PY3(EP)*PY3(EP)
       ENDDO
C-------0.5*[-By Bx 0 0 0 BRZ]^tKG[-By Bx 0 0 0 BRZ]*0.5
C------ BRZ = (-BRXY+BRYX)-0.5
       DO EP=JFT,JLT
         KZ11(1,1,EP)=PY1PY1(EP)*DHZ(EP)
         KZ11(1,2,EP)=-PX1PY1(EP)*DHZ(EP)
         KZ11(2,2,EP)=PX1PX1(EP)*DHZ(EP)
C
         KZ22(1,1,EP)=PY2PY2(EP)*DHZ(EP)
         KZ22(1,2,EP)=PX1PY2(EP)*DHZ(EP)
         KZ22(2,2,EP)=KZ11(2,2,EP)
C
         KZ33(1,1,EP)=PY3PY3(EP)*DHZ(EP)
         KZ33(1,2,EP)=ZERO
         KZ33(2,2,EP)=ZERO
C
         KZ12(1,1,EP)=PY1PY2(EP)*DHZ(EP)
C         KZ12(1,2,EP)=PX1PY1(EP)*DHZ(EP)
         KZ12(1,2,EP)=-KZ11(1,2,EP)
         KZ12(2,2,EP)=-KZ11(2,2,EP)
C         KZ12(2,1,EP)=-PX1PY2(EP)*DHZ(EP)
         KZ12(2,1,EP)=-KZ22(1,2,EP)
C
         KZ13(1,1,EP)=PY1PY3(EP)*DHZ(EP)
         KZ13(1,2,EP)=ZERO
         KZ13(2,2,EP)=ZERO
         KZ13(2,1,EP)=-PX1PY3(EP)*DHZ(EP)
C
         KZ23(1,1,EP)=PY2PY3(EP)*DHZ(EP)
         KZ23(1,2,EP)=ZERO
         KZ23(2,2,EP)=ZERO
C         KZ23(2,1,EP)=PX1PY3(EP)*DHZ(EP)
         KZ23(2,1,EP)=-KZ13(2,1,EP)
       ENDDO
C
       DO I=1,2
        DO J=I,2
         DO EP=JFT,JLT
          K11(I,J,EP)=K11(I,J,EP)+KZ11(I,J,EP)
          K12(I,J,EP)=K12(I,J,EP)+KZ12(I,J,EP)
          K22(I,J,EP)=K22(I,J,EP)+KZ22(I,J,EP)
         ENDDO
        ENDDO
       ENDDO
C
       DO EP=JFT,JLT
        K12(2,1,EP)=K12(2,1,EP)+KZ12(2,1,EP)
        K13(1,1,EP)=K13(1,1,EP)+KZ13(1,1,EP)
        K13(2,1,EP)=K13(2,1,EP)+KZ13(2,1,EP)
        K23(1,1,EP)=K23(1,1,EP)+KZ23(1,1,EP)
        K23(2,1,EP)=K23(2,1,EP)+KZ23(2,1,EP)
        K33(1,1,EP)=K33(1,1,EP)+KZ33(1,1,EP)
       ENDDO
C
C------ Mzz  reset zero----MF,FM are not initialized to zero------
       DO EP=JFT,JLT
        M11(3,3,EP)=ZERO
        M12(3,3,EP)=ZERO
        M13(3,3,EP)=ZERO
        M22(3,3,EP)=ZERO
        M23(3,3,EP)=ZERO
        M33(3,3,EP)=ZERO
C
        MF11(1,3,EP)=ZERO
        MF11(2,3,EP)=ZERO
        MF22(1,3,EP)=ZERO
        MF22(2,3,EP)=ZERO
        MF33(1,3,EP)=ZERO
        MF33(2,3,EP)=ZERO
        MF12(1,3,EP)=ZERO
        MF12(2,3,EP)=ZERO
        MF13(1,3,EP)=ZERO
        MF13(2,3,EP)=ZERO
        MF23(1,3,EP)=ZERO
        MF23(2,3,EP)=ZERO
C
        FM12(3,1,EP)=ZERO
        FM12(3,2,EP)=ZERO
        FM13(3,1,EP)=ZERO
        FM13(3,2,EP)=ZERO
        FM23(3,1,EP)=ZERO
        FM23(3,2,EP)=ZERO
       ENDDO
C
      DO NG =1,NPG
       DO EP=JFT,JLT
        BN1RZ=BKRZ(EP,1)*A_HAMMER(NG,1)+BERZ(EP,1)*A_HAMMER(NG,2)
        BN2RZ=BKRZ(EP,2)*A_HAMMER(NG,1)+BERZ(EP,2)*A_HAMMER(NG,2)
        BN3RZ=-BN1RZ-BN2RZ
        PRZ(1,EP)=(B0RZ(EP,1)+BN1RZ)*A_I(EP)
        PRZ(2,EP)=(B0RZ(EP,2)+BN2RZ)*A_I(EP)
        PRZ(3,EP)=(B0RZ(EP,3)+BN3RZ)*A_I(EP)
        DPRZ(1,EP)=PRZ(1,EP)*DZ12(EP)
        DPRZ(2,EP)=PRZ(2,EP)*DZ12(EP)
        DPRZ(3,EP)=PRZ(3,EP)*DZ12(EP)
       ENDDO
C
       DO EP=JFT,JLT
C------MFIJ(1,3)=-PYI*DPRZ(J); MFIJ(2,3)=PXI*DPRZ(J)
C------FMIJ(3,1)=-PYJ*DPRZ(I); FMIJ(3,2)=PXJ*DPRZ(I)
        MF11(1,3,EP)=MF11(1,3,EP)-PY1(EP)*DPRZ(1,EP)
        MF11(2,3,EP)=MF11(2,3,EP)+PX1(EP)*DPRZ(1,EP)
        MF22(1,3,EP)=MF22(1,3,EP)-PY2(EP)*DPRZ(2,EP)
        MF22(2,3,EP)=MF22(2,3,EP)-PX1(EP)*DPRZ(2,EP)
        MF33(1,3,EP)=MF33(1,3,EP)-PY3(EP)*DPRZ(3,EP)
C        MF33(2,3,EP)=MF33(2,3,EP)+ PX3(EP)*DPRZ(3,EP)
        MF12(1,3,EP)=MF12(1,3,EP)-PY1(EP)*DPRZ(2,EP)
        MF12(2,3,EP)=MF12(2,3,EP)+PX1(EP)*DPRZ(2,EP)
        MF13(1,3,EP)=MF13(1,3,EP)-PY1(EP)*DPRZ(3,EP)
        MF13(2,3,EP)=MF13(2,3,EP)+PX1(EP)*DPRZ(3,EP)
        MF23(1,3,EP)=MF23(1,3,EP)-PY2(EP)*DPRZ(3,EP)
        MF23(2,3,EP)=MF23(2,3,EP)-PX1(EP)*DPRZ(3,EP)
C
        FM12(3,1,EP)=FM12(3,1,EP)-PY2(EP)*DPRZ(1,EP)
        FM12(3,2,EP)=FM12(3,2,EP)-PX1(EP)*DPRZ(1,EP)
        FM13(3,1,EP)=FM13(3,1,EP)-PY3(EP)*DPRZ(1,EP)
C        FM13(3,2,EP)=FM13(3,2,EP)+PX3(EP)*DPRZ(1,EP)
        FM23(3,1,EP)=FM23(3,1,EP)-PY3(EP)*DPRZ(2,EP)
C        FM23(3,2,EP)=FM23(3,2,EP)+PX3(EP)*DPRZ(2,EP)
       ENDDO
C------ Mzz  ----------
       DO EP=JFT,JLT
        M11(3,3,EP)=M11(3,3,EP)+PRZ(1,EP)*DPRZ(1,EP)
        M12(3,3,EP)=M12(3,3,EP)+PRZ(1,EP)*DPRZ(2,EP)
        M13(3,3,EP)=M13(3,3,EP)+PRZ(1,EP)*DPRZ(3,EP)
        M22(3,3,EP)=M22(3,3,EP)+PRZ(2,EP)*DPRZ(2,EP)
        M23(3,3,EP)=M23(3,3,EP)+PRZ(2,EP)*DPRZ(3,EP)
        M33(3,3,EP)=M33(3,3,EP)+PRZ(3,EP)*DPRZ(3,EP)
       ENDDO
      END DO !NG =1,NPG
C-------[MFIJ]=[Bm]^t[C][BRm]; [MIJ]=[BRm]^t[C][BRm];----
C---------------|0 0 BRX       |----
C-----BR(3x3)=  |0 0 BRY       |
C---------------|0 0 BRXY+BRYX |----
       DO EP=JFT,JLT
        DO J=1,2
         PRX(J,EP) =BM0RZ(EP,1,J)*A_I(EP)
         PRY(J,EP) =BM0RZ(EP,2,J)*A_I(EP)
         PRXY(J,EP)=BM0RZ(EP,3,J)*A_I(EP)
        ENDDO
         PRX(3,EP) =-PRX(1,EP)-PRX(2,EP)
         PRY(3,EP) =-PRY(1,EP)-PRY(2,EP)
         PRXY(3,EP)=-PRXY(1,EP)-PRXY(2,EP)
       END DO
C
       DO EP=JFT,JLT
        CBRX(1,EP) =DM(1,1,EP)*PRX(1,EP)+DM(1,2,EP)*PRY(1,EP)+
     .              DM(1,3,EP)*PRXY(1,EP)
        CBRY(1,EP) =DM(2,1,EP)*PRX(1,EP)+DM(2,2,EP)*PRY(1,EP)+
     .              DM(2,3,EP)*PRXY(1,EP)
        CBRZ(1,EP) =DM(3,1,EP)*PRX(1,EP)+DM(3,2,EP)*PRY(1,EP)+
     .              DM(3,3,EP)*PRXY(1,EP)
        CBRX(2,EP) =DM(1,1,EP)*PRX(2,EP)+DM(1,2,EP)*PRY(2,EP)+
     .              DM(1,3,EP)*PRXY(2,EP)
        CBRY(2,EP) =DM(2,1,EP)*PRX(2,EP)+DM(2,2,EP)*PRY(2,EP)+
     .              DM(2,3,EP)*PRXY(2,EP)
        CBRZ(2,EP) =DM(3,1,EP)*PRX(2,EP)+DM(3,2,EP)*PRY(2,EP)+
     .              DM(3,3,EP)*PRXY(2,EP)
        CBRX(3,EP) =-CBRX(1,EP)-CBRX(2,EP)
        CBRY(3,EP) =-CBRY(1,EP)-CBRY(2,EP)
        CBRZ(3,EP) =-CBRZ(1,EP)-CBRZ(2,EP)
       ENDDO
C
C--  [MFIJ(,3)]=|Bxi 0  Byi|{CPRXj}  [FMIJ(3,)]={CPRXi CPRYi CPRZi}|Bxj 0  0|
C--             |0  Byi Bxi|{CPRYj}                                |0  Byj 0|
C--             |0   0   0 |{CPRZj}                                |ByjBxj 0|
       DO EP=JFT,JLT
        MF11(1,3,EP)=MF11(1,3,EP)+ PX1(EP)*CBRX(1,EP)+PY1(EP)*CBRZ(1,EP)
        MF11(2,3,EP)=MF11(2,3,EP)+ PY1(EP)*CBRY(1,EP)+PX1(EP)*CBRZ(1,EP)
        MF12(1,3,EP)=MF12(1,3,EP)+ PX1(EP)*CBRX(2,EP)+PY1(EP)*CBRZ(2,EP)
        MF12(2,3,EP)=MF12(2,3,EP)+ PY1(EP)*CBRY(2,EP)+PX1(EP)*CBRZ(2,EP)
        MF13(1,3,EP)=MF13(1,3,EP)+ PX1(EP)*CBRX(3,EP)+PY1(EP)*CBRZ(3,EP)
        MF13(2,3,EP)=MF13(2,3,EP)+ PY1(EP)*CBRY(3,EP)+PX1(EP)*CBRZ(3,EP)
        MF22(1,3,EP)=MF22(1,3,EP)- PX1(EP)*CBRX(2,EP)+PY2(EP)*CBRZ(2,EP)
        MF22(2,3,EP)=MF22(2,3,EP)+ PY2(EP)*CBRY(2,EP)-PX1(EP)*CBRZ(2,EP)
        MF23(1,3,EP)=MF23(1,3,EP)- PX1(EP)*CBRX(3,EP)+PY2(EP)*CBRZ(3,EP)
        MF23(2,3,EP)=MF23(2,3,EP)+ PY2(EP)*CBRY(3,EP)-PX1(EP)*CBRZ(3,EP)
        MF33(1,3,EP)=MF33(1,3,EP)                    +PY3(EP)*CBRZ(3,EP)
        MF33(2,3,EP)=MF33(2,3,EP)+ PY3(EP)*CBRY(3,EP)
C
        FM12(3,1,EP)=FM12(3,1,EP)- PX1(EP)*CBRX(1,EP)+PY2(EP)*CBRZ(1,EP)
        FM12(3,2,EP)=FM12(3,2,EP)+ PY2(EP)*CBRY(1,EP)-PX1(EP)*CBRZ(1,EP)
        FM13(3,1,EP)=FM13(3,1,EP)                    +PY3(EP)*CBRZ(1,EP)
        FM13(3,2,EP)=FM13(3,2,EP)+ PY3(EP)*CBRY(1,EP)
        FM23(3,1,EP)=FM23(3,1,EP)                    +PY3(EP)*CBRZ(2,EP)
        FM23(3,2,EP)=FM23(3,2,EP)+ PY3(EP)*CBRY(2,EP)
       ENDDO
C------ Mzz  ----------
       DO EP=JFT,JLT
        M11(3,3,EP)=M11(3,3,EP)+PRX(1,EP)*CBRX(1,EP)+
     .              PRY(1,EP)*CBRY(1,EP)+PRXY(1,EP)*CBRZ(1,EP)
        M12(3,3,EP)=M12(3,3,EP)+PRX(1,EP)*CBRX(2,EP)+
     .              PRY(1,EP)*CBRY(2,EP)+PRXY(1,EP)*CBRZ(2,EP)
        M13(3,3,EP)=M13(3,3,EP)+PRX(1,EP)*CBRX(3,EP)+
     .              PRY(1,EP)*CBRY(3,EP)+PRXY(1,EP)*CBRZ(3,EP)
        M22(3,3,EP)=M22(3,3,EP)+PRX(2,EP)*CBRX(2,EP)+
     .              PRY(2,EP)*CBRY(2,EP)+PRXY(2,EP)*CBRZ(2,EP)
        M23(3,3,EP)=M23(3,3,EP)+PRX(2,EP)*CBRX(3,EP)+
     .              PRY(2,EP)*CBRY(3,EP)+PRXY(2,EP)*CBRZ(3,EP)
        M33(3,3,EP)=M33(3,3,EP)+PRX(3,EP)*CBRX(3,EP)+
     .              PRY(3,EP)*CBRY(3,EP)+PRXY(3,EP)*CBRZ(3,EP)
       ENDDO
       IF (IORTH.GT.0) THEN
C ----add mem-bending coupling terms of orthotrope--coplanar first---
#include "vectorize.inc"
        DO EP=JFT,JLT 
 	 C2=VOL(EP)*THK0(EP)
         DMF(1,1,EP)=HMFOR(EP,1)*C2
         DMF(2,2,EP)=HMFOR(EP,2)*C2
         DMF(1,2,EP)=HMFOR(EP,3)*C2
         DMF(1,3,EP)=HMFOR(EP,5)*C2
         DMF(2,3,EP)=HMFOR(EP,6)*C2
         DMF(2,1,EP)=DMF(1,2,EP)
         DMF(3,1,EP)=DMF(1,3,EP)
         DMF(3,2,EP)=DMF(2,3,EP)
         DMF(3,3,EP)=HMFOR(EP,4)*C2
        ENDDO
C-------[C][BRm];----
        DO EP=JFT,JLT
         CBRX(1,EP) =DMF(1,1,EP)*PRX(1,EP)+DMF(1,2,EP)*PRY(1,EP)+
     .               DMF(1,3,EP)*PRXY(1,EP)
         CBRY(1,EP) =DMF(2,1,EP)*PRX(1,EP)+DMF(2,2,EP)*PRY(1,EP)+
     .               DMF(2,3,EP)*PRXY(1,EP)
         CBRZ(1,EP) =DMF(3,1,EP)*PRX(1,EP)+DMF(3,2,EP)*PRY(1,EP)+
     .               DMF(3,3,EP)*PRXY(1,EP)
         CBRX(2,EP) =DMF(1,1,EP)*PRX(2,EP)+DMF(1,2,EP)*PRY(2,EP)+
     .               DMF(1,3,EP)*PRXY(2,EP)
         CBRY(2,EP) =DMF(2,1,EP)*PRX(2,EP)+DMF(2,2,EP)*PRY(2,EP)+
     .               DMF(2,3,EP)*PRXY(2,EP)
         CBRZ(2,EP) =DMF(3,1,EP)*PRX(2,EP)+DMF(3,2,EP)*PRY(2,EP)+
     .               DMF(3,3,EP)*PRXY(2,EP)
         CBRX(3,EP) =-CBRX(1,EP)-CBRX(2,EP)
         CBRY(3,EP) =-CBRY(1,EP)-CBRY(2,EP)
         CBRZ(3,EP) =-CBRZ(1,EP)-CBRZ(2,EP)
        ENDDO
C ----add Rz-bending coupling terms of orthotrope--coplanar first---
C     MIJ(1,3)=-BY(I)*CBR2(J)-BX(I)*CBR3(J)
C     MIJ(2,3)=BX(I)*CBR1(J)+BY(I)*CBR3(J)
C     MIJ(3,1)=-BY(J)*CBR2(I)-BX(J)*CBR3(I)
C     MIJ(3,2)=BX(J)*CBR1(I)+BY(J)*CBR3(I)
C         
C------ PX2=-PX1 , PX3=0-------------
         DO EP=JFT,JLT
          M11(1,3,EP)=-PY1(EP)*CBRY(1,EP)-PX1(EP)*CBRZ(1,EP)
          M11(2,3,EP)= PX1(EP)*CBRX(1,EP)+PY1(EP)*CBRZ(1,EP)
          M12(1,3,EP)=-PY1(EP)*CBRY(2,EP)-PX1(EP)*CBRZ(2,EP)
          M12(2,3,EP)= PX1(EP)*CBRX(2,EP)+PY1(EP)*CBRZ(2,EP)
          M13(1,3,EP)=-PY1(EP)*CBRY(3,EP)-PX1(EP)*CBRZ(3,EP)
          M13(2,3,EP)= PX1(EP)*CBRX(3,EP)+PY1(EP)*CBRZ(3,EP)
          M22(1,3,EP)=-PY2(EP)*CBRY(2,EP)+PX1(EP)*CBRZ(2,EP)
          M22(2,3,EP)=-PX1(EP)*CBRX(2,EP)+PY2(EP)*CBRZ(2,EP)
          M23(1,3,EP)=-PY2(EP)*CBRY(3,EP)+PX1(EP)*CBRZ(3,EP)
          M23(2,3,EP)=-PX1(EP)*CBRX(3,EP)+PY2(EP)*CBRZ(3,EP)
          M33(1,3,EP)= PY3(EP)*CBRY(3,EP)
          M33(2,3,EP)=-PY3(EP)*CBRZ(3,EP)
C          
          M12(3,1,EP)=-PY2(EP)*CBRY(1,EP)+PX1(EP)*CBRZ(1,EP)
          M12(3,2,EP)=-PX1(EP)*CBRX(1,EP)+PY2(EP)*CBRZ(1,EP)
          M13(3,1,EP)= PY3(EP)*CBRY(1,EP)
          M13(3,2,EP)=                   -PY3(EP)*CBRZ(1,EP)
          M23(3,1,EP)=-PY3(EP)*CBRY(2,EP)
          M23(3,2,EP)=                   PY3(EP)*CBRZ(2,EP)
        ENDDO
       ENDIF !IF (IORTH.EQ.1)
C
       RETURN
       END

