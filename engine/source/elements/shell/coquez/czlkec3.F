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
Chd|  CZLKEC3                       source/elements/shell/coquez/czlkec3.F
Chd|-- called by -----------
Chd|        CZKE3                         source/elements/shell/coquez/czke3.F
Chd|        CZKEL3                        source/elements/shell/coquez/czkel3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CZLKEC3(JFT ,JLT   ,VOL  ,THK0 ,THK2 ,
     2                     HM  ,HF    ,HZ   ,A_I  ,Z1   ,
     3                    PX1  ,PX2  ,PY1   ,PY2  ,NPLAT,
     4                    IPLAT,DHZ  ,
     6                    K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     7                    M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     8                    MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33,
     9                    MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34,
     A                    IORTH,HMOR,HFOR,HMFOR)
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
      INTEGER JFT,JLT,NPLAT,IPLAT(*),IKGEO,IORTH
      MY_REAL
     .   VOL(*),THK0(*),THK2(*),A_I(*),Z1(*),
     .    HM(MVSIZ,4),HF(MVSIZ,4),HZ(*),
     .    PX1(*)  ,PX2(*)   ,PY1(*)  ,PY2(*)  ,
     .    K11(3,3,*),K12(3,3,*),K13(3,3,*),K14(3,3,*),
     .    K22(3,3,*),K23(3,3,*),K24(3,3,*),K33(3,3,*),
     .    M11(3,3,*),M12(3,3,*),M13(3,3,*),M14(3,3,*),
     .    M22(3,3,*),M23(3,3,*),M24(3,3,*),M33(3,3,*),
     .    MF11(3,3,*),MF12(3,3,*),MF13(3,3,*),MF14(3,3,*),
     .    MF22(3,3,*),MF23(3,3,*),MF24(3,3,*),MF33(3,3,*),
     .    FM12(3,3,*),FM13(3,3,*),FM14(3,3,*),
     .    FM23(3,3,*),FM24(3,3,*),FM34(3,3,*),
     .    K34(3,3,*),K44(3,3,*),M34(3,3,*),M44(3,3,*),
     .    MF34(3,3,*),MF44(3,3,*),DHZ(*),HMOR(MVSIZ,2),HFOR(MVSIZ,2),
     .    HMFOR(MVSIZ,6)
C---------------|[KIJ][MFIJ]|----
C-----KE(6x6)=  |           |
C---------------|[FMIJ]{MIJ]|----
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER EP,I,J,NF,M
      MY_REAL
     .    DM(2,2,MVSIZ),DF(2,2,MVSIZ),
     .    C1,C2,GM(MVSIZ),GF(MVSIZ),DG(MVSIZ),
     .    PX1PX1(MVSIZ),PX1PX2(MVSIZ) ,PX2PX2(MVSIZ),
     .    PX1PY1(MVSIZ),PX1PY2(MVSIZ) ,PX2PY1(MVSIZ),
     .    PX2PY2(MVSIZ),PY1PY1(MVSIZ),PY1PY2(MVSIZ),PY2PY2(MVSIZ),
     .    FACZ(MVSIZ),FACZ2(MVSIZ),DFZ,DM5(MVSIZ),DM6(MVSIZ),DF5(MVSIZ),
     .    DF6(MVSIZ),DM5_2(MVSIZ),DM6_2(MVSIZ),DF5_2(MVSIZ),
     .    DF6_2(MVSIZ),DMF(3,3,MVSIZ),
     .    BXIBXJ(2,2,MVSIZ),BXIBYJ(2,2,MVSIZ),BYIBXJ(2,2,MVSIZ),
     .    BYIBYJ(2,2,MVSIZ),FAC2Z
C-----------Attention Matrice sym Kii ne calcul que la moitie---------72
       NF=NPLAT+1
#include "vectorize.inc"
C--------Constante parts---------------
       DO M=JFT,JLT
        EP=IPLAT(M)
        C2=VOL(EP)
        C1=THK2(EP)*C2
        DM(1,1,M)=HM(EP,1)*C2
        DM(2,2,M)=HM(EP,2)*C2
        DM(1,2,M)=HM(EP,3)*C2
        DM(2,1,M)=DM(1,2,M)
        GM(M) =HM(EP,4)*C2
        DF(1,1,M)=HF(EP,1)*C1
        DF(2,2,M)=HF(EP,2)*C1
        DF(1,2,M)=HF(EP,3)*C1
        DF(2,1,M)=DF(1,2,M)
        GF(M) =HF(EP,4)*C1
        DHZ(M)=HZ(EP)*C1
       ENDDO
C---------px3=-px1,px4=-px2-------------
       DO EP=JFT,JLT
         PX1PX1(EP) = PX1(EP)*PX1(EP)
         PX1PX2(EP) = PX1(EP)*PX2(EP)
         PX2PX2(EP) = PX2(EP)*PX2(EP)
         PX1PY1(EP) = PX1(EP)*PY1(EP)
         PX1PY2(EP) = PX1(EP)*PY2(EP)
         PX2PY1(EP) = PX2(EP)*PY1(EP)
         PX2PY2(EP) = PX2(EP)*PY2(EP)
         PY1PY1(EP) = PY1(EP)*PY1(EP)
         PY1PY2(EP) = PY1(EP)*PY2(EP)
         PY2PY2(EP) = PY2(EP)*PY2(EP)
       ENDDO
C
       DO EP=JFT,JLT
        K11(1,1,EP)=DM(1,1,EP)*PX1PX1(EP)+GM(EP)*PY1PY1(EP)
        K12(1,1,EP)=DM(1,1,EP)*PX1PX2(EP)+GM(EP)*PY1PY2(EP)
        K22(1,1,EP)=DM(1,1,EP)*PX2PX2(EP)+GM(EP)*PY2PY2(EP)
C
        M11(2,2,EP)=DF(1,1,EP)*PX1PX1(EP)+GF(EP)*PY1PY1(EP)
        M12(2,2,EP)=DF(1,1,EP)*PX1PX2(EP)+GF(EP)*PY1PY2(EP)
        M22(2,2,EP)=DF(1,1,EP)*PX2PX2(EP)+GF(EP)*PY2PY2(EP)
       ENDDO
C
       DO EP=JFT,JLT
        K11(1,2,EP)=(DM(1,2,EP)+GM(EP))*PX1PY1(EP)
        K12(1,2,EP)=DM(1,2,EP)*PX1PY2(EP)+GM(EP)*PX2PY1(EP)
        K22(1,2,EP)=(DM(1,2,EP)+GM(EP))*PX2PY2(EP)
        K12(2,1,EP)=DM(1,2,EP)*PX2PY1(EP)+GM(EP)*PX1PY2(EP)
C
        M11(1,2,EP)=-(DF(1,2,EP)+GF(EP))*PX1PY1(EP)
        M12(1,2,EP)=-DF(1,2,EP)*PX2PY1(EP)-GF(EP)*PX1PY2(EP)
        M22(1,2,EP)=-DF(1,2,EP)*PX2PY2(EP)-GF(EP)*PX2PY2(EP)
        M12(2,1,EP)=-DF(1,2,EP)*PX1PY2(EP)-GF(EP)*PX2PY1(EP)
       ENDDO
C
       DO EP=JFT,JLT
        K11(2,2,EP)=DM(2,2,EP)*PY1PY1(EP)+GM(EP)*PX1PX1(EP)
        K12(2,2,EP)=DM(2,2,EP)*PY1PY2(EP)+GM(EP)*PX1PX2(EP)
        K22(2,2,EP)=DM(2,2,EP)*PY2PY2(EP)+GM(EP)*PX2PX2(EP)
C
        M11(1,1,EP)=DF(2,2,EP)*PY1PY1(EP)+GF(EP)*PX1PX1(EP)
        M12(1,1,EP)=DF(2,2,EP)*PY1PY2(EP)+GF(EP)*PX1PX2(EP)
        M22(1,1,EP)=DF(2,2,EP)*PY2PY2(EP)+GF(EP)*PX2PX2(EP)
       ENDDO
       IF (IORTH >0 ) THEN
#include "vectorize.inc"
        DO M=JFT,JLT
         EP=IPLAT(M)
 	 C2=VOL(EP)
         C1=THK2(EP)*C2
         DM5(M)=HMOR(EP,1)*C2
         DM6(M)=HMOR(EP,2)*C2
         DF5(M)=HFOR(EP,1)*C1
         DF6(M)=HFOR(EP,2)*C1
        ENDDO
        DO M=JFT,JLT
         DM5_2(M)=TWO*DM5(M)
         DM6_2(M)=TWO*DM6(M)
         DF5_2(M)=TWO*DF5(M)
         DF6_2(M)=TWO*DF6(M)
        ENDDO
        DO EP=JFT,JLT
         K11(1,1,EP)=K11(1,1,EP)+PX1PY1(EP)*DM5_2(EP)
         K11(1,2,EP)=K11(1,2,EP)+
     1               PX1PX1(EP)*DM5(EP)+PY1PY1(EP)*DM6(EP)
         K11(2,2,EP)=K11(2,2,EP)+PX1PY1(EP)*DM6_2(EP)
         K12(1,1,EP)=K12(1,1,EP)+(PX1PY2(EP)+PX2PY1(EP))*DM5(EP)
         C1 = PX1PX2(EP)*DM5(EP)+PY1PY2(EP)*DM6(EP)
         K12(1,2,EP)=K12(1,2,EP)+C1
         K12(2,1,EP)=K12(2,1,EP)+C1
         K12(2,2,EP)=K12(2,2,EP)+(PX1PY2(EP)+PX2PY1(EP))*DM6(EP)
         K22(1,1,EP)=K22(1,1,EP)+PX2PY2(EP)*DM5_2(EP)
         K22(1,2,EP)=K22(1,2,EP)+
     1               PX2PX2(EP)*DM5(EP)+PY2PY2(EP)*DM6(EP)
         K22(2,2,EP)=K22(2,2,EP)+PX2PY2(EP)*DM6_2(EP)
C
         M11(1,1,EP)=M11(1,1,EP)+PX1PY1(EP)*DF6_2(EP)
         M11(1,2,EP)=M11(1,2,EP)-
     1               PX1PX1(EP)*DF5(EP)-PY1PY1(EP)*DF6(EP)
         M11(2,2,EP)=M11(2,2,EP)+PX1PY1(EP)*DF5_2(EP)
         M12(1,1,EP)=M12(1,1,EP)+(PX1PY2(EP)+PX2PY1(EP))*DF6(EP)
         C2 = PX1PX2(EP)*DF5(EP)+PY1PY2(EP)*DF6(EP)
         M12(1,2,EP)=M12(1,2,EP)-C2
         M12(2,1,EP)=M12(2,1,EP)-C2
         M12(2,2,EP)=M12(2,2,EP)+(PX1PY2(EP)+PX2PY1(EP))*DF5(EP)
         M22(1,1,EP)=M22(1,1,EP)+PX2PY2(EP)*DF6_2(EP)
         M22(1,2,EP)=M22(1,2,EP)-
     1               PX2PX2(EP)*DF5(EP)-PY2PY2(EP)*DF6(EP)
         M22(2,2,EP)=M22(2,2,EP)+PX2PY2(EP)*DF5_2(EP)
        ENDDO
        DO M=JFT,JLT
         EP=IPLAT(M)
 	 C2=VOL(EP)*THK0(EP)
         DMF(1,1,M)=HMFOR(EP,1)*C2
         DMF(2,2,M)=HMFOR(EP,2)*C2
         DMF(1,2,M)=HMFOR(EP,3)*C2
         DMF(1,3,M)=HMFOR(EP,5)*C2
         DMF(2,3,M)=HMFOR(EP,6)*C2
         DMF(2,1,M)=DMF(1,2,M)
         DMF(3,1,M)=DMF(1,3,M)
         DMF(3,2,M)=DMF(2,3,M)
         DMF(3,3,M)=HMFOR(EP,4)*C2
        ENDDO
C ----add mem-bending coupling terms of orthotrope--coplanar first---
        DO EP=JFT,NPLAT
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
        DO EP=JFT,NPLAT
         MF12(1,1,EP)=
     1               -DMF(1,2,EP)*PX1PY2(EP)-DMF(1,3,EP)*PX1PX2(EP)
C--------------------------------------------------------YIXJ-----     
     2               -DMF(2,3,EP)*PY1PY2(EP)-DMF(3,3,EP)*PX2PY1(EP)
         MF12(1,2,EP)=
     1                DMF(1,1,EP)*PX1PX2(EP)+DMF(1,3,EP)*PX1PY2(EP)
C-------------------------YIXJ-----     
     2               +DMF(1,3,EP)*PX2PY1(EP)+DMF(3,3,EP)*PY1PY2(EP)
         MF12(2,1,EP)=
C---------------------------------------------------------YIXJ-----     
     1               -DMF(2,2,EP)*PY1PY2(EP)-DMF(2,3,EP)*PX2PY1(EP)
     2               -DMF(2,3,EP)*PX1PY2(EP)-DMF(3,3,EP)*PX1PX2(EP)
         MF12(2,2,EP)=
C---------------------------------YIXJ-----     
     1                DMF(1,2,EP)*PX2PY1(EP)+DMF(2,3,EP)*PY1PY2(EP)
     2               +DMF(1,3,EP)*PX1PX2(EP)+DMF(3,3,EP)*PX1PY2(EP)
        ENDDO
        DO EP=JFT,NPLAT
         MF22(1,1,EP)=
     1               -DMF(1,2,EP)*PX2PY2(EP)-DMF(1,3,EP)*PX2PX2(EP)
C--------------------------------------------------------YIXJ-----     
     2               -DMF(2,3,EP)*PY2PY2(EP)-DMF(3,3,EP)*PX2PY2(EP)
         MF22(1,2,EP)=
     1                DMF(1,1,EP)*PX2PX2(EP)+DMF(1,3,EP)*PX2PY2(EP)
C-------------------------YIXJ-----     
     2               +DMF(1,3,EP)*PX2PY2(EP)+DMF(3,3,EP)*PY2PY2(EP)
         MF22(2,1,EP)=
C---------------------------------------------------------YIXJ-----     
     1               -DMF(2,2,EP)*PY2PY2(EP)-DMF(2,3,EP)*PX2PY2(EP)
     2               -DMF(2,3,EP)*PX2PY2(EP)-DMF(3,3,EP)*PX2PX2(EP)
         MF22(2,2,EP)=
C---------------------------------YIXJ-----     
     1                DMF(1,2,EP)*PX2PY2(EP)+DMF(2,3,EP)*PY2PY2(EP)
     2               +DMF(1,3,EP)*PX2PX2(EP)+DMF(3,3,EP)*PX2PY2(EP)
        ENDDO
        DO EP=JFT,NPLAT
         MF23(1,1,EP)=
     1                DMF(1,2,EP)*PX2PY1(EP)+DMF(1,3,EP)*PX1PX2(EP)
C--------------------------------------------------------YIXJ-----     
     2               +DMF(2,3,EP)*PY1PY2(EP)+DMF(3,3,EP)*PX1PY2(EP)
         MF23(1,2,EP)=
     1               -DMF(1,1,EP)*PX1PX2(EP)-DMF(1,3,EP)*PX2PY1(EP)
C-------------------------YIXJ-----     
     2               -DMF(1,3,EP)*PX1PY2(EP)-DMF(3,3,EP)*PY1PY2(EP)
         MF23(2,1,EP)=
C---------------------------------------------------------YIXJ-----     
     1                DMF(2,2,EP)*PY1PY2(EP)+DMF(2,3,EP)*PX1PY2(EP)
     2               +DMF(2,3,EP)*PX2PY1(EP)+DMF(3,3,EP)*PX1PX2(EP)
         MF23(2,2,EP)=
C---------------------------------YIXJ-----     
     1               -DMF(1,2,EP)*PX1PY2(EP)-DMF(2,3,EP)*PY1PY2(EP)
     2               -DMF(1,3,EP)*PX1PX2(EP)-DMF(3,3,EP)*PX2PY1(EP)
        ENDDO
        DO I=1,2
        DO J=1,2
         DO EP=JFT,NPLAT
          MF13(I,J,EP)=-MF11(I,J,EP)
          MF14(I,J,EP)=-MF12(I,J,EP)
          MF24(I,J,EP)=-MF22(I,J,EP)
          MF33(I,J,EP)=MF11(I,J,EP)
          MF34(I,J,EP)=MF12(I,J,EP)
          MF44(I,J,EP)=MF22(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,2
        DO J=1,2
         DO EP=JFT,NPLAT
          FM12(I,J,EP)=-MF23(J,I,EP)
          FM13(I,J,EP)=-MF11(J,I,EP)
          FM14(I,J,EP)= MF23(J,I,EP)
          FM23(I,J,EP)=-MF12(J,I,EP)
          FM24(I,J,EP)=-MF22(J,I,EP)
          FM34(I,J,EP)=-MF23(J,I,EP)
         ENDDO
        ENDDO
        ENDDO
       ENDIF !(IORTH==1)
C---+----1----+----2----+warped elements----+----5----+----6----+----7----+----8
#include "vectorize.inc"
       DO M=NF,JLT
        EP=IPLAT(M)
        FACZ(M) = FOUR*Z1(EP)*A_I(EP)
        FACZ2(M) = FACZ(M)*FACZ(M)
        DG(M)=GF(M)*FACZ2(M)
       ENDDO
C---+----1----+----2----+warped elements----+----5----+----6----+----7----+----8
C--------px1'=py2;px2'=py1;py1'=-px2;py2'=-px1;-------------
       DO EP=NF,JLT
        DFZ =DF(1,1,EP)*FACZ2(EP)
        K11(1,1,EP)=K11(1,1,EP)+DFZ*PY2PY2(EP)+DG(EP)*PX2PX2(EP)
        K12(1,1,EP)=K12(1,1,EP)+DFZ*PY1PY2(EP)+DG(EP)*PX1PX2(EP)
        K22(1,1,EP)=K22(1,1,EP)+DFZ*PY1PY1(EP)+DG(EP)*PX1PX1(EP)
       ENDDO
C
       DO EP=NF,JLT
        DFZ =DF(1,2,EP)*FACZ2(EP)
        K11(1,2,EP)=K11(1,2,EP)-(DFZ+DG(EP))*PX2PY2(EP)
        K12(1,2,EP)=K12(1,2,EP)-DFZ*PX1PY2(EP)-DG(EP)*PX2PY1(EP)
        K22(1,2,EP)=K22(1,2,EP)-(DFZ+DG(EP))*PX1PY1(EP)
        K12(2,1,EP)=K12(2,1,EP)-DFZ*PX2PY1(EP)-DG(EP)*PX1PY2(EP)
       ENDDO
C
       DO EP=NF,JLT
        DFZ =DF(2,2,EP)*FACZ2(EP)
        K11(2,2,EP)=K11(2,2,EP)+DFZ*PX2PX2(EP)+DG(EP)*PY2PY2(EP)
        K12(2,2,EP)=K12(2,2,EP)+DFZ*PX1PX2(EP)+DG(EP)*PY1PY2(EP)
        K22(2,2,EP)=K22(2,2,EP)+DFZ*PX1PX1(EP)+DG(EP)*PY1PY1(EP)
       ENDDO
C--------(px1px1)'=py2py2;(px1px2)'=py1py2;(px2px2)'=py1py1;-------------
C--------(py1py1)'=px2px2;(py1py2)'=px1px2;(py2py2)'=px1px1;-------------
C--------(px1py1)'=-px2py2;(px2py1)'=-px2py1;(px2py2)'=-px1py1;(px1py2)'=-px1py2;-------------
       IF (IORTH > 0) THEN
        DO EP=NF,JLT
         K11(1,1,EP)=K11(1,1,EP)-PX2PY2(EP)*DF5_2(EP)*FACZ2(EP)
         K11(1,2,EP)=K11(1,2,EP)+FACZ2(EP)*(
     1               PY2PY2(EP)*DF5(EP)+PX2PX2(EP)*DF6(EP))
         K11(2,2,EP)=K11(2,2,EP)-PX2PY2(EP)*DF6_2(EP)*FACZ2(EP)
         K12(1,1,EP)=K12(1,1,EP)-FACZ2(EP)*(
     1               PX1PY2(EP)+PX2PY1(EP))*DF5(EP)
         C1 = (PY1PY2(EP)*DF5(EP)+PX1PX2(EP)*DF6(EP))*FACZ2(EP)
         K12(1,2,EP)=K12(1,2,EP)+C1
         K12(2,1,EP)=K12(2,1,EP)+C1
         K12(2,2,EP)=K12(2,2,EP)-FACZ2(EP)*(
     1               PX1PY2(EP)+PX2PY1(EP))*DF6(EP)
         K22(1,1,EP)=K22(1,1,EP)-PX1PY1(EP)*DF5_2(EP)*FACZ2(EP)
         K22(1,2,EP)=K22(1,2,EP)+FACZ2(EP)*(
     1               PY1PY1(EP)*DF5(EP)+PX1PX1(EP)*DF6(EP))
         K22(2,2,EP)=K22(2,2,EP)-PX1PY1(EP)*DF6_2(EP)*FACZ2(EP)
        ENDDO
C ----add mem-bending coupling terms of orthotrope--warped elements-suite---
C--------px1px1'=px1'px1=px1py2;px1px2'=px1py1;px1'px2=px2py2;px2px2'=px2'px2=px2py1;-------------
C--------py1py1'=py1'py1=-px2py1;py1py2'=-px1py1;py1'py2=-px2py2;py2py2'=py2'py2=-px1py2;-------------
C--------px1py1'=-px2px1;px1'py1=py1py2;px2py1'=-px2px2;px2'py1=py1py1;px2py2'=-px1px2;px2'py2=py1py2;px1py2'=-px1px1;px1'py2=py2py2;-------------
        DO EP=NF,JLT
         FAC2Z=TWO*FACZ(EP)
         BXIBXJ(1,1,EP)=FAC2Z*PX1PY2(EP)
         BXIBXJ(1,2,EP)=FACZ(EP)*(PX1PY1(EP)+PX2PY2(EP))
         BXIBXJ(2,1,EP)=BXIBXJ(1,2,EP)
         BXIBXJ(2,2,EP)=FAC2Z*PX2PY1(EP)
C
         BXIBYJ(1,1,EP)=FACZ(EP)*(-PX1PX2(EP)+PY1PY2(EP))
         BXIBYJ(1,2,EP)=FACZ(EP)*(-PX1PX1(EP)+PY2PY2(EP))
         BXIBYJ(2,1,EP)=FACZ(EP)*(-PX2PX2(EP)+PY1PY1(EP))
         BXIBYJ(2,2,EP)=BXIBYJ(1,1,EP)
C
         BYIBYJ(1,1,EP)=-FAC2Z*PX2PY1(EP)
         BYIBYJ(1,2,EP)=FACZ(EP)*(-PX1PY1(EP)-PX2PY2(EP))
         BYIBYJ(2,1,EP)=BYIBYJ(1,2,EP)
         BYIBYJ(2,2,EP)=-FAC2Z*PX1PY2(EP)
C
         BYIBXJ(1,1,EP)=FACZ(EP)*(PY1PY2(EP)-PX1PX2(EP))
         BYIBXJ(1,2,EP)=FACZ(EP)*(PY1PY1(EP)-PX2PX2(EP))
         BYIBXJ(2,1,EP)=FACZ(EP)*(PY2PY2(EP)-PX1PX1(EP))
         BYIBXJ(2,2,EP)=BYIBXJ(1,1,EP)
C
        ENDDO
        DO EP=NF,JLT
         K11(1,1,EP)=K11(1,1,EP)+DMF(1,1,EP)*BXIBXJ(1,1,EP)+
     1               DMF(1,3,EP)*(BXIBYJ(1,1,EP)+BYIBXJ(1,1,EP))+
     2               DMF(3,3,EP)*BYIBYJ(1,1,EP)
         K11(1,2,EP)=K11(1,2,EP)+DMF(1,2,EP)*BXIBYJ(1,1,EP)+
     1               DMF(1,3,EP)*BXIBXJ(1,1,EP)+
     2               DMF(2,3,EP)*BYIBYJ(1,1,EP)+
     3               DMF(3,3,EP)*BYIBXJ(1,1,EP)
         K11(2,2,EP)=K11(2,2,EP)+DMF(2,2,EP)*BYIBYJ(1,1,EP)+
     1               DMF(2,3,EP)*(BXIBYJ(1,1,EP)+BYIBXJ(1,1,EP))+
     2               DMF(3,3,EP)*BXIBXJ(1,1,EP)
C     
         K12(1,1,EP)=K12(1,1,EP)+DMF(1,1,EP)*BXIBXJ(1,2,EP)+
     1               DMF(1,3,EP)*(BXIBYJ(1,2,EP)+BYIBXJ(1,2,EP))+
     2               DMF(3,3,EP)*BYIBYJ(1,2,EP)
         K12(1,2,EP)=K12(1,2,EP)+DMF(1,2,EP)*BXIBYJ(1,2,EP)+
     1               DMF(1,3,EP)*BXIBXJ(1,2,EP)+
     2               DMF(2,3,EP)*BYIBYJ(1,2,EP)+
     3               DMF(3,3,EP)*BYIBXJ(1,2,EP)
         K12(2,1,EP)=K12(2,1,EP)+DMF(1,2,EP)*BYIBXJ(1,2,EP)+
     1               DMF(1,3,EP)*BXIBXJ(1,2,EP)+
     2               DMF(2,3,EP)*BYIBYJ(1,2,EP)+
     3               DMF(3,3,EP)*BXIBYJ(1,2,EP)
         K12(2,2,EP)=K12(2,2,EP)+DMF(2,2,EP)*BYIBYJ(1,2,EP)+
     1               DMF(2,3,EP)*(BXIBYJ(1,2,EP)+BYIBXJ(1,2,EP))+
     2               DMF(3,3,EP)*BXIBXJ(1,2,EP)
C     
         K22(1,1,EP)=K22(1,1,EP)+DMF(1,1,EP)*BXIBXJ(2,2,EP)+
     1               DMF(1,3,EP)*(BXIBYJ(2,2,EP)+BYIBXJ(2,2,EP))+
     2               DMF(3,3,EP)*BYIBYJ(2,2,EP)
         K22(1,2,EP)=K22(1,2,EP)+DMF(1,2,EP)*BXIBYJ(2,2,EP)+
     1               DMF(1,3,EP)*BXIBXJ(2,2,EP)+
     2               DMF(2,3,EP)*BYIBYJ(2,2,EP)+
     3               DMF(3,3,EP)*BYIBXJ(2,2,EP)
         K22(2,2,EP)=K22(2,2,EP)+DMF(2,2,EP)*BYIBYJ(2,2,EP)+
     1               DMF(2,3,EP)*(BXIBYJ(2,2,EP)+BYIBXJ(2,2,EP))+
     2               DMF(3,3,EP)*BXIBXJ(2,2,EP)
        ENDDO
C
       END IF !(IORTH==1)

C
       DO I=1,2
        DO J=I,2
         DO EP=JFT,JLT
          K13(I,J,EP)=-K11(I,J,EP)
          K14(I,J,EP)=-K12(I,J,EP)
          K23(I,J,EP)=-K12(J,I,EP)
          K24(I,J,EP)=-K22(I,J,EP)
          K33(I,J,EP)=K11(I,J,EP)
          K34(I,J,EP)=K12(I,J,EP)
          K44(I,J,EP)=K22(I,J,EP)
          M13(I,J,EP)=-M11(I,J,EP)
          M14(I,J,EP)=-M12(I,J,EP)
          M23(I,J,EP)=-M12(J,I,EP)
          M24(I,J,EP)=-M22(I,J,EP)
          M33(I,J,EP)=M11(I,J,EP)
          M34(I,J,EP)=M12(I,J,EP)
          M44(I,J,EP)=M22(I,J,EP)
         ENDDO
        ENDDO
       ENDDO
C
       DO EP=JFT,JLT
        K13(2,1,EP)=-K11(1,2,EP)
        K14(2,1,EP)=-K12(2,1,EP)
        K23(2,1,EP)=-K12(1,2,EP)
        K24(2,1,EP)=-K22(1,2,EP)
        K34(2,1,EP)=K12(2,1,EP)
        M13(2,1,EP)=-M11(1,2,EP)
        M14(2,1,EP)=-M12(2,1,EP)
        M23(2,1,EP)=-M12(1,2,EP)
        M24(2,1,EP)=-M22(1,2,EP)
        M34(2,1,EP)=M12(2,1,EP)
       ENDDO
C---+---------+---------+warped elements-suite------------
C--------px1'=py2;px2'=py1;py1'=-px2;py2'=-px1;-------------
C--------px1'px1=px1py2; px1'px2=px2py2; px2'px2=px2py1;-------------
C--------py1'py1=-px2py1; py1'py2=-px2py2; py2'py2=-px1py2;-------------
C--------px1'py1=py1py2;px2'py1=py1py1;px2'py2=py1py2;px1'py2=py2py2;-------------
       DO EP=NF,JLT
        DG(EP)=GF(EP)*FACZ(EP)
        DFZ =DF(1,1,EP)*FACZ(EP)
        MF11(1,2,EP)=DFZ*PX1PY2(EP)-DG(EP)*PX2PY1(EP)
        MF12(1,2,EP)=(DFZ-DG(EP))*PX2PY2(EP)
        MF22(1,2,EP)=DFZ*PX2PY1(EP)-DG(EP)*PX1PY2(EP)
        MF23(1,2,EP)=-(DFZ-DG(EP))*PX1PY1(EP)
       ENDDO
       DO EP=NF,JLT
        DFZ =DF(1,2,EP)*FACZ(EP)
        MF11(1,1,EP)=-DFZ*PY1PY2(EP)+DG(EP)*PX1PX2(EP)
        MF12(1,1,EP)=-DFZ*PY2PY2(EP)+DG(EP)*PX2PX2(EP)
        MF22(1,1,EP)=MF11(1,1,EP)
        MF23(1,1,EP)=DFZ*PY1PY1(EP)-DG(EP)*PX1PX1(EP)
        MF11(2,2,EP)=-DFZ*PX1PX2(EP)+DG(EP)*PY1PY2(EP)
        MF12(2,2,EP)=-DFZ*PX2PX2(EP)+DG(EP)*PY2PY2(EP)
        MF22(2,2,EP)= MF11(2,2,EP)
        MF23(2,2,EP)=DFZ*PX1PX1(EP)-DG(EP)*PY1PY1(EP)
       ENDDO
       DO EP=NF,JLT
        DFZ =DF(2,2,EP)*FACZ(EP)
        MF11(2,1,EP)=DFZ*PX2PY1(EP)-DG(EP)*PX1PY2(EP)
        MF12(2,1,EP)=DFZ*PX2PY2(EP)-DG(EP)*PX2PY2(EP)
        MF22(2,1,EP)=DFZ*PX1PY2(EP)-DG(EP)*PX2PY1(EP)
        MF23(2,1,EP)=-(DFZ-DG(EP))*PX1PY1(EP)
       ENDDO
       IF (IORTH>0) THEN
        DO M=NF,JLT
         DF5(M)=FACZ(M)*DF5(M)
         DF6(M)=FACZ(M)*DF6(M)
        ENDDO
C
        DO EP=NF,JLT
         DFZ=DF5(EP)*PX1PY2(EP)-DF6(EP)*PX2PY1(EP)
         MF11(1,1,EP)=MF11(1,1,EP)-DFZ
         MF11(1,2,EP)=MF11(1,2,EP)+DF5(EP)*(PY1PY2(EP)-PX1PX2(EP))
         MF11(2,1,EP)=MF11(2,1,EP)+DF6(EP)*(PX1PX2(EP)-PY1PY2(EP))
         MF11(2,2,EP)=MF11(2,2,EP)+DFZ
        ENDDO
        DO EP=NF,JLT
         DFZ=(DF5(EP)-DF6(EP))*PX2PY2(EP)
         MF12(1,1,EP)=MF12(1,1,EP)-DFZ
         MF12(1,2,EP)=MF12(1,2,EP)+DF5(EP)*(PY2PY2(EP)-PX2PX2(EP))
         MF12(2,1,EP)=MF12(2,1,EP)+DF6(EP)*(PX2PX2(EP)-PY2PY2(EP))
         MF12(2,2,EP)=MF12(2,2,EP)+DFZ
        ENDDO
        DO EP=NF,JLT
         DFZ=DF5(EP)*PX2PY1(EP)-DF6(EP)*PX1PY2(EP)
         MF22(1,1,EP)=MF22(1,1,EP)-DFZ
         MF22(1,2,EP)=MF22(1,2,EP)+DF5(EP)*(PY1PY2(EP)-PX1PX2(EP))
         MF22(2,1,EP)=MF22(2,1,EP)+DF6(EP)*(PX1PX2(EP)-PY1PY2(EP))
         MF22(2,2,EP)=MF22(2,2,EP)+DFZ
        ENDDO
        DO EP=NF,JLT
         DFZ=(DF6(EP)-DF5(EP))*PX1PY1(EP)
         MF23(1,1,EP)=MF23(1,1,EP)-DFZ
         MF23(1,2,EP)=MF23(1,2,EP)-DF5(EP)*(PY1PY1(EP)-PX1PX1(EP))
         MF23(2,1,EP)=MF23(2,1,EP)-DF6(EP)*(PX1PX1(EP)-PY1PY1(EP))
         MF23(2,2,EP)=MF23(2,2,EP)+DFZ
        ENDDO
C ----add mem-bending coupling terms of orthotrope--warped(same than coplanar---
        DO EP=NF,JLT
         MF11(1,1,EP)=MF11(1,1,EP)
     1               -DMF(1,2,EP)*PX1PY1(EP)-DMF(1,3,EP)*PX1PX1(EP)
     2               -DMF(2,3,EP)*PY1PY1(EP)-DMF(3,3,EP)*PX1PY1(EP)
         MF11(1,2,EP)=MF11(1,2,EP)
     1               +DMF(1,1,EP)*PX1PX1(EP)+DMF(1,3,EP)*PX1PY1(EP)
     2               +DMF(1,3,EP)*PX1PY1(EP)+DMF(3,3,EP)*PY1PY1(EP)
         MF11(2,1,EP)=MF11(2,1,EP)
     1               -DMF(2,2,EP)*PY1PY1(EP)-DMF(2,3,EP)*PX1PY1(EP)
     2               -DMF(2,3,EP)*PX1PY1(EP)-DMF(3,3,EP)*PX1PX1(EP)
         MF11(2,2,EP)=MF11(2,2,EP)
     1               +DMF(1,2,EP)*PX1PY1(EP)+DMF(2,3,EP)*PY1PY1(EP)
     2               +DMF(1,3,EP)*PX1PX1(EP)+DMF(3,3,EP)*PX1PY1(EP)
        ENDDO
        DO EP=NF,JLT
         MF12(1,1,EP)=MF12(1,1,EP)
     1               -DMF(1,2,EP)*PX1PY2(EP)-DMF(1,3,EP)*PX1PX2(EP)
     2               -DMF(2,3,EP)*PY1PY2(EP)-DMF(3,3,EP)*PX2PY1(EP)
         MF12(1,2,EP)=MF12(1,2,EP)
     1               +DMF(1,1,EP)*PX1PX2(EP)+DMF(1,3,EP)*PX1PY2(EP)
     2               +DMF(1,3,EP)*PX2PY1(EP)+DMF(3,3,EP)*PY1PY2(EP)
         MF12(2,1,EP)=MF12(2,1,EP)
     1               -DMF(2,2,EP)*PY1PY2(EP)-DMF(2,3,EP)*PX2PY1(EP)
     2               -DMF(2,3,EP)*PX1PY2(EP)-DMF(3,3,EP)*PX1PX2(EP)
         MF12(2,2,EP)=MF12(2,2,EP)
     1               +DMF(1,2,EP)*PX2PY1(EP)+DMF(2,3,EP)*PY1PY2(EP)
     2               +DMF(1,3,EP)*PX1PX2(EP)+DMF(3,3,EP)*PX1PY2(EP)
        ENDDO
        DO EP=NF,JLT
         MF22(1,1,EP)=MF22(1,1,EP)
     1               -DMF(1,2,EP)*PX2PY2(EP)-DMF(1,3,EP)*PX2PX2(EP)
     2               -DMF(2,3,EP)*PY2PY2(EP)-DMF(3,3,EP)*PX2PY2(EP)
         MF22(1,2,EP)=MF22(1,2,EP)
     1               +DMF(1,1,EP)*PX2PX2(EP)+DMF(1,3,EP)*PX2PY2(EP)
     2               +DMF(1,3,EP)*PX2PY2(EP)+DMF(3,3,EP)*PY2PY2(EP)
         MF22(2,1,EP)=MF22(2,1,EP)
     1               -DMF(2,2,EP)*PY2PY2(EP)-DMF(2,3,EP)*PX2PY2(EP)
     2               -DMF(2,3,EP)*PX2PY2(EP)-DMF(3,3,EP)*PX2PX2(EP)
         MF22(2,2,EP)=MF22(2,2,EP)
     1               +DMF(1,2,EP)*PX2PY2(EP)+DMF(2,3,EP)*PY2PY2(EP)
     2               +DMF(1,3,EP)*PX2PX2(EP)+DMF(3,3,EP)*PX2PY2(EP)
        ENDDO
        DO EP=NF,JLT
         MF23(1,1,EP)=MF23(1,1,EP)
     1               +DMF(1,2,EP)*PX2PY1(EP)+DMF(1,3,EP)*PX1PX2(EP)
     2               +DMF(2,3,EP)*PY1PY2(EP)+DMF(3,3,EP)*PX1PY2(EP)
         MF23(1,2,EP)=MF23(1,2,EP)
     1               -DMF(1,1,EP)*PX1PX2(EP)-DMF(1,3,EP)*PX2PY1(EP)
     2               -DMF(1,3,EP)*PX1PY2(EP)-DMF(3,3,EP)*PY1PY2(EP)
         MF23(2,1,EP)=MF23(2,1,EP)
     1               +DMF(2,2,EP)*PY1PY2(EP)+DMF(2,3,EP)*PX1PY2(EP)
     2               +DMF(2,3,EP)*PX2PY1(EP)+DMF(3,3,EP)*PX1PX2(EP)
         MF23(2,2,EP)=MF23(2,2,EP)
     1               -DMF(1,2,EP)*PX1PY2(EP)-DMF(2,3,EP)*PY1PY2(EP)
     2               -DMF(1,3,EP)*PX1PX2(EP)-DMF(3,3,EP)*PX2PY1(EP)
        ENDDO
       END IF !(IORTH==1)
C 
       DO I=1,2
        DO J=1,2
         DO EP=NF,JLT
          MF13(I,J,EP)=-MF11(I,J,EP)
          MF14(I,J,EP)=-MF12(I,J,EP)
          MF24(I,J,EP)=-MF22(I,J,EP)
          MF33(I,J,EP)=MF11(I,J,EP)
          MF34(I,J,EP)=MF12(I,J,EP)
          MF44(I,J,EP)=MF22(I,J,EP)
         ENDDO
        ENDDO
       ENDDO
C
       DO I=1,2
        DO J=1,2
         DO EP=NF,JLT
          FM12(I,J,EP)=-MF23(J,I,EP)
          FM13(I,J,EP)=-MF11(J,I,EP)
          FM14(I,J,EP)= MF23(J,I,EP)
          FM23(I,J,EP)=-MF12(J,I,EP)
          FM24(I,J,EP)=-MF22(J,I,EP)
          FM34(I,J,EP)=-MF23(J,I,EP)
         ENDDO
        ENDDO
       ENDDO
C------ Mzz  ----------
       DO EP=JFT,JLT
        M11(3,3,EP)=DHZ(EP)*(PX1PX1(EP)+PY1PY1(EP))
        M12(3,3,EP)=DHZ(EP)*(PX1PX2(EP)+PY1PY2(EP))
        M13(3,3,EP)=-M11(3,3,EP)
        M14(3,3,EP)=-M12(3,3,EP)
        M22(3,3,EP)=DHZ(EP)*(PX2PX2(EP)+PY2PY2(EP))
        M23(3,3,EP)=-M12(3,3,EP)
        M24(3,3,EP)=-M22(3,3,EP)
        M33(3,3,EP)=M11(3,3,EP)
        M34(3,3,EP)=M12(3,3,EP)
        M44(3,3,EP)=M22(3,3,EP)
       ENDDO
C------ renforce positive  ----------
      IF (NEIG==0) THEN
       DO EP=JFT,JLT
        C1 = EM8*M11(3,3,EP)
	C2 = EM8*M22(3,3,EP)
        M11(3,3,EP)=M11(3,3,EP)+C1
        M22(3,3,EP)=M22(3,3,EP)+C2
        M33(3,3,EP)=M11(3,3,EP)
        M44(3,3,EP)=M22(3,3,EP)
       ENDDO
      ENDIF
C
       RETURN
       END
Chd|====================================================================
Chd|  CZLKECR3                      source/elements/shell/coquez/czlkec3.F
Chd|-- called by -----------
Chd|        CZKE3                         source/elements/shell/coquez/czke3.F
Chd|        CZKEL3                        source/elements/shell/coquez/czkel3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CZLKECR3(JFT ,JLT   ,VOL  ,THK0 ,THK2 ,
     2                     HM  ,HF    ,HZ   ,A_I  ,Z1   ,
     3                     PX1  ,PX2  ,PY1   ,PY2  ,
     6                    K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     7                    M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     8                    MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33,
     9                    MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34,
     A                    IORTH,HMOR,HFOR ,IPLAT,DHZ  ,
     4                    PRX  ,PRY  ,PRXY ,PRZ ,HMFOR,NPLAT)
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
      INTEGER JFT,JLT,IPLAT(*),IORTH,NPLAT
      MY_REAL
     .   VOL(*),THK0(*),THK2(*),A_I(*),Z1(*),
     .    HM(MVSIZ,4),HF(MVSIZ,4),HZ(*),
     .    PX1(*)  ,PX2(*)   ,PY1(*)  ,PY2(*)  ,
     .    K11(3,3,*),K12(3,3,*),K13(3,3,*),K14(3,3,*),
     .    K22(3,3,*),K23(3,3,*),K24(3,3,*),K33(3,3,*),
     .    M11(3,3,*),M12(3,3,*),M13(3,3,*),M14(3,3,*),
     .    M22(3,3,*),M23(3,3,*),M24(3,3,*),M33(3,3,*),
     .    MF11(3,3,*),MF12(3,3,*),MF13(3,3,*),MF14(3,3,*),
     .    MF22(3,3,*),MF23(3,3,*),MF24(3,3,*),MF33(3,3,*),
     .    FM12(3,3,*),FM13(3,3,*),FM14(3,3,*),
     .    FM23(3,3,*),FM24(3,3,*),FM34(3,3,*),
     .    K34(3,3,*),K44(3,3,*),M34(3,3,*),M44(3,3,*),
     .    MF34(3,3,*),MF44(3,3,*),DHZ(*),HMOR(MVSIZ,2),HFOR(MVSIZ,2)
      my_real
     .   PRX(4,*),PRY(4,*),PRXY(4,*),PRZ(4,*) ,HMFOR(MVSIZ,6)
C---------------|[KIJ][MFIJ]|----
C-----KE(6x6)=  |           |
C---------------|[FMIJ]{MIJ]|----
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER EP,I,J,NF,M
      MY_REAL
     .    DM(3,3,MVSIZ),DPRZ(4,MVSIZ),
     .    C1,C2,CBRX(4,MVSIZ),CBRY(4,MVSIZ),CBRZ(4,MVSIZ),
     .    KZ11(2,2,MVSIZ),KZ12(2,2,MVSIZ),KZ22(2,2,MVSIZ),
     .    DMF(3,3,MVSIZ),PCX1(MVSIZ),PCX2(MVSIZ),PCY1(MVSIZ),
     .    PCY2(MVSIZ),FACZ(MVSIZ)
C-----------Attention Matrice sym Kii ne calcul que la moitie---------72
#include "vectorize.inc"
C--------Constante parts---------------
       DO M=JFT,JLT
        EP=IPLAT(M)
        C2=VOL(EP)
        DM(1,1,M)=HM(EP,1)*C2
        DM(2,2,M)=HM(EP,2)*C2
        DM(1,2,M)=HM(EP,3)*C2
        DM(3,3,M)=HM(EP,4)*C2
        DHZ(M)=HZ(EP)*FOURTH*C2
        DM(1,3,M)=ZERO
        DM(2,3,M)=ZERO
       ENDDO
       IF (IORTH==1) THEN
        DO M=JFT,JLT
         EP=IPLAT(M)
 	 C2=VOL(EP)
         DM(1,3,M)=HMOR(EP,1)*C2
         DM(2,3,M)=HMOR(EP,2)*C2
        ENDDO
       ENDIF
C       
       DO M=JFT,JLT
        DM(2,1,M)=DM(1,2,M)
        DM(3,1,M)=DM(1,3,M)
        DM(3,2,M)=DM(2,3,M)
       ENDDO
C-------0.5*[-By Bx 0 0 0 BRZ]^tKG[-By Bx 0 0 0 BRZ]*0.5
C------ BRZ = (-BRXY+BRYX)-0.5
       DO EP=JFT,JLT
         KZ11(1,1,EP)=PY1(EP)*DHZ(EP)*PY1(EP)
         KZ11(1,2,EP)=-PY1(EP)*DHZ(EP)*PX1(EP)
         KZ11(2,2,EP)=PX1(EP)*DHZ(EP)*PX1(EP)
         KZ12(1,1,EP)=PY1(EP)*DHZ(EP)*PY2(EP)
         KZ12(1,2,EP)=-PY1(EP)*DHZ(EP)*PX2(EP)
         KZ12(2,1,EP)=-PX1(EP)*DHZ(EP)*PY2(EP)
         KZ12(2,2,EP)=PX1(EP)*DHZ(EP)*PX2(EP)
         KZ22(1,1,EP)=PY2(EP)*DHZ(EP)*PY2(EP)
         KZ22(1,2,EP)=-PY2(EP)*DHZ(EP)*PX2(EP)
         KZ22(2,2,EP)=PX2(EP)*DHZ(EP)*PX2(EP)
       ENDDO
C
       DO I=1,2
        DO J=I,2
         DO EP=JFT,JLT
          K11(I,J,EP)=K11(I,J,EP)+KZ11(I,J,EP)
          K12(I,J,EP)=K12(I,J,EP)+KZ12(I,J,EP)
          K22(I,J,EP)=K22(I,J,EP)+KZ22(I,J,EP)
          K13(I,J,EP)=K13(I,J,EP)-KZ11(I,J,EP)
          K14(I,J,EP)=K14(I,J,EP)-KZ12(I,J,EP)
          K23(I,J,EP)=K23(I,J,EP)-KZ12(J,I,EP)
          K24(I,J,EP)=K24(I,J,EP)-KZ22(I,J,EP)
          K33(I,J,EP)=K33(I,J,EP)+KZ11(I,J,EP)
          K34(I,J,EP)=K34(I,J,EP)+KZ12(I,J,EP)
          K44(I,J,EP)=K44(I,J,EP)+KZ22(I,J,EP)
         ENDDO
        ENDDO
       ENDDO
C
       DO EP=JFT,JLT
        K12(2,1,EP)=K12(2,1,EP)+KZ12(2,1,EP)
        K13(2,1,EP)=K13(2,1,EP)-KZ11(1,2,EP)
        K14(2,1,EP)=K14(2,1,EP)-KZ12(2,1,EP)
        K23(2,1,EP)=K23(2,1,EP)-KZ12(1,2,EP)
        K24(2,1,EP)=K24(2,1,EP)-KZ22(1,2,EP)
        K34(2,1,EP)=K34(2,1,EP)+KZ12(2,1,EP)
       ENDDO
C
       DO J=1,4
       DO M=JFT,JLT
        DPRZ(J,M)=PRZ(J,M)*DHZ(M)
       ENDDO
       ENDDO
C
       DO M=JFT,JLT
        C1 = -PY2(M)*DPRZ(1,M)
        C2 = PX2(M)*DPRZ(1,M)
        FM12(3,1,M)=FM12(3,1,M)+ C1
        FM14(3,1,M)=FM14(3,1,M)- C1
        FM12(3,2,M)=FM12(3,2,M)+ C2
        FM14(3,2,M)=FM14(3,2,M)- C2
        C1 = -PY1(M)*DPRZ(1,M)
        C2 = PX1(M)*DPRZ(1,M)
        MF11(1,3,M)=MF11(1,3,M)+ C1
        FM13(3,1,M)=FM13(3,1,M)- C1
        MF11(2,3,M)=MF11(2,3,M)+ C2
        FM13(3,2,M)=FM13(3,2,M)- C2
        C1 = -PY1(M)*DPRZ(2,M)
        C2 = PX1(M)*DPRZ(2,M)
        MF12(1,3,M)=MF12(1,3,M)+ C1
        FM23(3,1,M)=FM23(3,1,M)- C1
        MF12(2,3,M)=MF12(2,3,M)+ C2
        FM23(3,2,M)=FM23(3,2,M)- C2
        C1 = -PY1(M)*DPRZ(3,M)
        C2 = PX1(M)*DPRZ(3,M)
        MF13(1,3,M)=MF13(1,3,M)+ C1
        MF33(1,3,M)=MF33(1,3,M)- C1
        MF13(2,3,M)=MF13(2,3,M)+ C2
        MF33(2,3,M)=MF33(2,3,M)- C2
        C1 = -PY1(M)*DPRZ(4,M)
        C2 = PX1(M)*DPRZ(4,M)
        MF14(1,3,M)=MF14(1,3,M)+ C1
        MF34(1,3,M)=MF34(1,3,M)- C1
        MF14(2,3,M)=MF14(2,3,M)+ C2
        MF34(2,3,M)=MF34(2,3,M)- C2
        C1 = -PY2(M)*DPRZ(2,M)
        C2 = PX2(M)*DPRZ(2,M)
        MF22(1,3,M)=MF22(1,3,M)+ C1
        FM24(3,1,M)=FM24(3,1,M)- C1
        MF22(2,3,M)=MF22(2,3,M)+ C2
        FM24(3,2,M)=FM24(3,2,M)- C2
        C1 = -PY2(M)*DPRZ(3,M)
        C2 = PX2(M)*DPRZ(3,M)
        MF23(1,3,M)=MF23(1,3,M)+ C1
        FM34(3,1,M)=FM34(3,1,M)- C1
        MF23(2,3,M)=MF23(2,3,M)+ C2
        FM34(3,2,M)=FM34(3,2,M)- C2
        C1 = -PY2(M)*DPRZ(4,M)
        C2 = PX2(M)*DPRZ(4,M)
        MF24(1,3,M)=MF24(1,3,M)+ C1
        MF44(1,3,M)=MF44(1,3,M)- C1
        MF24(2,3,M)=MF24(2,3,M)+ C2
        MF44(2,3,M)=MF44(2,3,M)- C2
       ENDDO
C------ Mzz  ----------
       DO EP=JFT,JLT
        M11(3,3,EP)=PRZ(1,EP)*DPRZ(1,EP)
        M12(3,3,EP)=PRZ(1,EP)*DPRZ(2,EP)
        M13(3,3,EP)=PRZ(1,EP)*DPRZ(3,EP)
        M14(3,3,EP)=PRZ(1,EP)*DPRZ(4,EP)
        M22(3,3,EP)=PRZ(2,EP)*DPRZ(2,EP)
        M23(3,3,EP)=PRZ(2,EP)*DPRZ(3,EP)
        M24(3,3,EP)=PRZ(2,EP)*DPRZ(4,EP)
        M33(3,3,EP)=PRZ(3,EP)*DPRZ(3,EP)
        M34(3,3,EP)=PRZ(3,EP)*DPRZ(4,EP)
        M44(3,3,EP)=PRZ(4,EP)*DPRZ(4,EP)
       ENDDO
C-------[MFIJ]=[Bm]^t[C][BRm]; [MIJ]=[BRm]^t[C][BRm];----
C---------------|0 0 BRX       |----
C-----BR(3x3)=  |0 0 BRY       |
C---------------|0 0 BRXY+BRYX |----
       DO J=1,4
       DO M=JFT,JLT
        CBRX(J,M) =DM(1,1,M)*PRX(J,M)+DM(1,2,M)*PRY(J,M)+
     .             DM(1,3,M)*PRXY(J,M)
        CBRY(J,M) =DM(2,1,M)*PRX(J,M)+DM(2,2,M)*PRY(J,M)+
     .             DM(2,3,M)*PRXY(J,M)
        CBRZ(J,M) =DM(3,1,M)*PRX(J,M)+DM(3,2,M)*PRY(J,M)+
     .             DM(3,3,M)*PRXY(J,M)
       ENDDO
       ENDDO
C
       DO M=JFT,JLT
        C1 = PX2(M)*CBRX(1,M)+PY2(M)*CBRZ(1,M)
        C2 = PY2(M)*CBRY(1,M)+PX2(M)*CBRZ(1,M)
        FM12(3,1,M)=FM12(3,1,M)+ C1
        FM14(3,1,M)=FM14(3,1,M)- C1
        FM12(3,2,M)=FM12(3,2,M)+ C2
        FM14(3,2,M)=FM14(3,2,M)- C2
C
        C1 = PX1(M)*CBRX(1,M)+PY1(M)*CBRZ(1,M)
        C2 = PY1(M)*CBRY(1,M)+PX1(M)*CBRZ(1,M)
        MF11(1,3,M)=MF11(1,3,M)+ C1
        FM13(3,1,M)=FM13(3,1,M)- C1
        MF11(2,3,M)=MF11(2,3,M)+ C2
        FM13(3,2,M)=FM13(3,2,M)- C2
        C1 = PX1(M)*CBRX(2,M)+PY1(M)*CBRZ(2,M)
        C2 = PY1(M)*CBRY(2,M)+PX1(M)*CBRZ(2,M)
        MF12(1,3,M)=MF12(1,3,M)+ C1
        FM23(3,1,M)=FM23(3,1,M)- C1
        MF12(2,3,M)=MF12(2,3,M)+ C2
        FM23(3,2,M)=FM23(3,2,M)- C2
        C1 = PX1(M)*CBRX(3,M)+PY1(M)*CBRZ(3,M)
        C2 = PY1(M)*CBRY(3,M)+PX1(M)*CBRZ(3,M)
        MF13(1,3,M)=MF13(1,3,M)+ C1
        MF33(1,3,M)=MF33(1,3,M)- C1
        MF13(2,3,M)=MF13(2,3,M)+ C2
        MF33(2,3,M)=MF33(2,3,M)- C2
        C1 = PX1(M)*CBRX(4,M)+PY1(M)*CBRZ(4,M)
        C2 = PY1(M)*CBRY(4,M)+PX1(M)*CBRZ(4,M)
        MF14(1,3,M)=MF14(1,3,M)+ C1
        MF34(1,3,M)=MF34(1,3,M)- C1
        MF14(2,3,M)=MF14(2,3,M)+ C2
        MF34(2,3,M)=MF34(2,3,M)- C2
        C1 = PX2(M)*CBRX(2,M)+PY2(M)*CBRZ(2,M)
        C2 = PY2(M)*CBRY(2,M)+PX2(M)*CBRZ(2,M)
        MF22(1,3,M)=MF22(1,3,M)+ C1
        FM24(3,1,M)=FM24(3,1,M)- C1
        MF22(2,3,M)=MF22(2,3,M)+ C2
        FM24(3,2,M)=FM24(3,2,M)- C2
        C1 = PX2(M)*CBRX(3,M)+PY2(M)*CBRZ(3,M)
        C2 = PY2(M)*CBRY(3,M)+PX2(M)*CBRZ(3,M)
        MF23(1,3,M)=MF23(1,3,M)+ C1
        FM34(3,1,M)=FM34(3,1,M)- C1
        MF23(2,3,M)=MF23(2,3,M)+ C2
        FM34(3,2,M)=FM34(3,2,M)- C2
        C1 = PX2(M)*CBRX(4,M)+PY2(M)*CBRZ(4,M)
        C2 = PY2(M)*CBRY(4,M)+PX2(M)*CBRZ(4,M)
        MF24(1,3,M)=MF24(1,3,M)+ C1
        MF44(1,3,M)=MF44(1,3,M)- C1
        MF24(2,3,M)=MF24(2,3,M)+ C2
        MF44(2,3,M)=MF44(2,3,M)- C2
      ENDDO
C------ Mzz  ----------
       DO M=JFT,JLT
        M11(3,3,M)=M11(3,3,M)+PRX(1,M)*CBRX(1,M)+
     .              PRY(1,M)*CBRY(1,M)+PRXY(1,M)*CBRZ(1,M)
        M12(3,3,M)=M12(3,3,M)+PRX(1,M)*CBRX(2,M)+
     .              PRY(1,M)*CBRY(2,M)+PRXY(1,M)*CBRZ(2,M)
        M13(3,3,M)=M13(3,3,M)+PRX(1,M)*CBRX(3,M)+
     .              PRY(1,M)*CBRY(3,M)+PRXY(1,M)*CBRZ(3,M)
        M14(3,3,M)=M14(3,3,M)+PRX(1,M)*CBRX(4,M)+
     .              PRY(1,M)*CBRY(4,M)+PRXY(1,M)*CBRZ(4,M)
        M22(3,3,M)=M22(3,3,M)+PRX(2,M)*CBRX(2,M)+
     .              PRY(2,M)*CBRY(2,M)+PRXY(2,M)*CBRZ(2,M)
        M23(3,3,M)=M23(3,3,M)+PRX(2,M)*CBRX(3,M)+
     .              PRY(2,M)*CBRY(3,M)+PRXY(2,M)*CBRZ(3,M)
        M24(3,3,M)=M24(3,3,M)+PRX(2,M)*CBRX(4,M)+
     .              PRY(2,M)*CBRY(4,M)+PRXY(2,M)*CBRZ(4,M)
        M33(3,3,M)=M33(3,3,M)+PRX(3,M)*CBRX(3,M)+
     .              PRY(3,M)*CBRY(3,M)+PRXY(3,M)*CBRZ(3,M)
        M34(3,3,M)=M34(3,3,M)+PRX(3,M)*CBRX(4,M)+
     .              PRY(3,M)*CBRY(4,M)+PRXY(3,M)*CBRZ(4,M)
        M44(3,3,M)=M44(3,3,M)+PRX(4,M)*CBRX(4,M)+
     .              PRY(4,M)*CBRY(4,M)+PRXY(4,M)*CBRZ(4,M)
       ENDDO
       IF (IORTH>0) THEN
C ----add mem-bending coupling terms of orthotrope--coplanar first---
        NF=NPLAT+1
#include "vectorize.inc"
        DO M=JFT,JLT
         EP=IPLAT(M)
 	 C2=VOL(EP)*THK0(EP)
         DMF(1,1,M)=HMFOR(EP,1)*C2
         DMF(2,2,M)=HMFOR(EP,2)*C2
         DMF(1,2,M)=HMFOR(EP,3)*C2
         DMF(1,3,M)=HMFOR(EP,5)*C2
         DMF(2,3,M)=HMFOR(EP,6)*C2
         DMF(2,1,M)=DMF(1,2,M)
         DMF(3,1,M)=DMF(1,3,M)
         DMF(3,2,M)=DMF(2,3,M)
         DMF(3,3,M)=HMFOR(EP,4)*C2
        ENDDO
C-------[C][BRm];----
        DO J=1,4
         DO EP=JFT,JLT
         CBRX(J,EP) =DMF(1,1,EP)*PRX(J,EP)+DMF(1,2,EP)*PRY(J,EP)+
     .               DMF(1,3,EP)*PRXY(J,EP)
         CBRY(J,EP) =DMF(2,1,EP)*PRX(J,EP)+DMF(2,2,EP)*PRY(J,EP)+
     .               DMF(2,3,EP)*PRXY(J,EP)
         CBRZ(J,EP) =DMF(3,1,EP)*PRX(J,EP)+DMF(3,2,EP)*PRY(J,EP)+
     .               DMF(3,3,EP)*PRXY(J,EP)
         ENDDO
        ENDDO
C        
        DO M=NF,JLT
         EP=IPLAT(M)
         FACZ(M) = FOUR*Z1(EP)*A_I(EP)
        ENDDO
C--------BMF : px1'=py2;px2'=py1;py1'=-px2;py2'=-px1;-------------
        DO EP=NF,JLT
         PCX1(EP)=FACZ(EP)*PY2(EP)
         PCX2(EP)=FACZ(EP)*PY1(EP)
         PCY1(EP)=-FACZ(EP)*PX2(EP)
         PCY2(EP)=-FACZ(EP)*PX1(EP)
        ENDDO
C ----add Rz-bending coupling terms of orthotrope-----[Bf]^t[C][BRm]+[Bmf]^t[C][BRm]
C     MIJ(1,3)=-BY(I)*CBR2(J)-BX(I)*CBR3(J)
C     MIJ(2,3)=BX(I)*CBR1(J)+BY(I)*CBR3(J)
C     MIJ(3,1)=-BY(J)*CBR2(I)-BX(J)*CBR3(I)
C     MIJ(3,2)=BX(J)*CBR1(I)+BY(J)*CBR3(I)
C         
         DO EP=JFT,JLT
          C1=-PY1(EP)*CBRY(1,EP)-PX1(EP)*CBRZ(1,EP)
          C2= PX1(EP)*CBRX(1,EP)+PY1(EP)*CBRZ(1,EP)
          M11(1,3,EP)=M11(1,3,EP)+C1
          M11(2,3,EP)=M11(2,3,EP)+C2
          M13(3,1,EP)=M13(3,1,EP)-C1
          M13(3,2,EP)=M13(3,2,EP)-C2
          C1=-PY1(EP)*CBRY(2,EP)-PX1(EP)*CBRZ(2,EP)
          C2= PX1(EP)*CBRX(2,EP)+PY1(EP)*CBRZ(2,EP)
          M12(1,3,EP)=M12(1,3,EP)+C1
          M12(2,3,EP)=M12(2,3,EP)+C2
          M23(3,1,EP)=M23(3,1,EP)-C1
          M23(3,2,EP)=M23(3,2,EP)-C2
          C1=-PY1(EP)*CBRY(4,EP)-PX1(EP)*CBRZ(4,EP)
          C2= PX1(EP)*CBRX(4,EP)+PY1(EP)*CBRZ(4,EP)
          M14(1,3,EP)=M14(1,3,EP)+C1
          M14(2,3,EP)=M14(2,3,EP)+C2
          M34(1,3,EP)=M34(1,3,EP)-C1
          M34(2,3,EP)=M34(2,3,EP)-C2
          C1=-PY2(EP)*CBRY(2,EP)-PX2(EP)*CBRZ(2,EP)
          C2= PX2(EP)*CBRX(2,EP)+PY2(EP)*CBRZ(2,EP)
          M22(1,3,EP)=M22(1,3,EP)+C1
          M22(2,3,EP)=M22(2,3,EP)+C2
          M24(3,1,EP)=M24(3,1,EP)-C1
          M24(3,2,EP)=M24(3,2,EP)-C2
          C1=-PY2(EP)*CBRY(3,EP)-PX2(EP)*CBRZ(3,EP)
          C2= PX2(EP)*CBRX(3,EP)+PY2(EP)*CBRZ(3,EP)
          M23(1,3,EP)=M23(1,3,EP)+C1
          M23(2,3,EP)=M23(2,3,EP)+C2
          M34(3,1,EP)=M34(3,1,EP)-C1
          M34(3,2,EP)=M34(3,2,EP)-C2
          C1= PY1(EP)*CBRY(3,EP)+PX1(EP)*CBRZ(3,EP)
          C2=-PX1(EP)*CBRX(3,EP)-PY1(EP)*CBRZ(3,EP)
          M33(1,3,EP)=M33(1,3,EP)+C1
          M33(2,3,EP)=M33(2,3,EP)+C2
          M13(1,3,EP)=M13(1,3,EP)-C1
          M13(2,3,EP)=M13(2,3,EP)-C2
          C1= PY2(EP)*CBRY(4,EP)+PX2(EP)*CBRZ(4,EP)
          C2=-PX2(EP)*CBRX(4,EP)-PY2(EP)*CBRZ(4,EP)
          M44(1,3,EP)=M44(1,3,EP)+C1
          M44(2,3,EP)=M44(2,3,EP)+C2
          M24(1,3,EP)=M24(1,3,EP)-C1
          M24(2,3,EP)=M24(2,3,EP)-C2
          C1=-PY2(EP)*CBRY(1,EP)-PX2(EP)*CBRZ(1,EP)
          C2= PX2(EP)*CBRX(1,EP)+PY2(EP)*CBRZ(1,EP)
          M12(3,1,EP)=M12(3,1,EP)+C1
          M12(3,2,EP)=M12(3,2,EP)+C2
          M14(3,1,EP)=M14(3,1,EP)-C1
          M14(3,2,EP)=M14(3,2,EP)-C2
        ENDDO
C---+----1----+----2----+warped elements----+----5----+----6----+----7----+----8
C     MFIJ(1,3)=BCX(I)*CBR1(J)+BCY(I)*CBR3(J)
C     MFIJ(2,3)=BCY(I)*CBR2(J)+BCX(I)*CBR3(J)
C     FMIJ(3,1)=BCX(J)*CBR1(I)+BCY(J)*CBR3(I)
C     FMIJ(3,2)=BCY(J)*CBR2(I)+BCX(J)*CBR3(I)
C         
         DO EP=NF,JLT
          C1= PCX1(EP)*CBRX(1,EP)+PCY1(EP)*CBRZ(1,EP)
          C2= PCY1(EP)*CBRY(1,EP)+PCX1(EP)*CBRZ(1,EP)
          MF11(1,3,EP)=MF11(1,3,EP)+C1
          MF11(2,3,EP)=MF11(2,3,EP)+C2
          FM13(3,1,EP)=FM13(3,1,EP)-C1
          FM13(3,2,EP)=FM13(3,2,EP)-C2
          C1= PCX1(EP)*CBRX(2,EP)+PCY1(EP)*CBRZ(2,EP)
          C2= PCY1(EP)*CBRY(2,EP)+PCX1(EP)*CBRZ(2,EP)
          MF12(1,3,EP)=MF12(1,3,EP)+C1
          MF12(2,3,EP)=MF12(2,3,EP)+C2
          FM23(3,1,EP)=FM23(3,1,EP)-C1
          FM23(3,2,EP)=FM23(3,2,EP)-C2
          C1= PCX1(EP)*CBRX(4,EP)+PCY1(EP)*CBRZ(4,EP)
          C2= PCY1(EP)*CBRY(4,EP)+PCX1(EP)*CBRZ(4,EP)
          MF14(1,3,EP)=MF14(1,3,EP)+C1
          MF14(2,3,EP)=MF14(2,3,EP)+C2
          MF34(1,3,EP)=MF34(1,3,EP)-C1
          MF34(2,3,EP)=MF34(2,3,EP)-C2
          C1= PCX2(EP)*CBRX(2,EP)+PCY2(EP)*CBRZ(2,EP)
          C2= PCY2(EP)*CBRY(2,EP)+PCX2(EP)*CBRZ(2,EP)
          MF22(1,3,EP)=MF22(1,3,EP)+C1
          MF22(2,3,EP)=MF22(2,3,EP)+C2
          FM24(3,1,EP)=FM24(3,1,EP)-C1
          FM24(3,2,EP)=FM24(3,2,EP)-C2
          C1= PCX2(EP)*CBRX(3,EP)+PCY2(EP)*CBRZ(3,EP)
          C2= PCY2(EP)*CBRY(3,EP)+PCX2(EP)*CBRZ(3,EP)
          MF23(1,3,EP)=MF23(1,3,EP)+C1
          MF23(2,3,EP)=MF23(2,3,EP)+C2
          FM34(3,1,EP)=FM34(3,1,EP)-C1
          FM34(3,2,EP)=FM34(3,2,EP)-C2
          C1=-PCX1(EP)*CBRX(3,EP)-PCY1(EP)*CBRZ(3,EP)
          C2=-PCY1(EP)*CBRY(3,EP)-PCX1(EP)*CBRZ(3,EP)
          MF33(1,3,EP)=MF33(1,3,EP)+C1
          MF33(2,3,EP)=MF33(2,3,EP)+C2
          MF13(1,3,EP)=MF13(1,3,EP)-C1
          MF13(2,3,EP)=MF13(2,3,EP)-C2
          C1=-PCX2(EP)*CBRX(4,EP)-PCY2(EP)*CBRZ(4,EP)
          C2=-PCY2(EP)*CBRY(4,EP)-PCX2(EP)*CBRZ(4,EP)
          MF44(1,3,EP)=MF44(1,3,EP)+C1
          MF44(2,3,EP)=MF44(2,3,EP)+C2
          MF24(1,3,EP)=MF24(1,3,EP)-C1
          MF24(2,3,EP)=MF24(2,3,EP)-C2
          C1= PCX2(EP)*CBRX(1,EP)+PCY2(EP)*CBRZ(1,EP)
          C2= PCY2(EP)*CBRY(1,EP)+PCX2(EP)*CBRZ(1,EP)
          FM12(3,1,EP)=FM12(3,1,EP)+C1
          FM12(3,2,EP)=FM12(3,2,EP)+C2
          FM14(3,1,EP)=FM14(3,1,EP)-C1
          FM14(3,2,EP)=FM14(3,2,EP)-C2
        ENDDO
       ENDIF !IF (IORTH==1)
C
       RETURN
       END
