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
Chd|  CBALKE3                       source/elements/shell/coqueba/cbalke3.F
Chd|-- called by -----------
Chd|        CBAKE3                        source/elements/shell/coqueba/cbake3.F
Chd|-- calls ---------------
Chd|        CBALKEORFM                    source/elements/shell/coqueba/cbalke3.F
Chd|        CBALKEORFM1                   source/elements/shell/coqueba/cbalke3.F
Chd|        CBALKEORMF                    source/elements/shell/coqueba/cbalke3.F
Chd|        CBALKEORMF1                   source/elements/shell/coqueba/cbalke3.F
Chd|====================================================================
        SUBROUTINE CBALKE3(JFT,JLT,CDET,THK0,THK2,HM,HF,HC,HZ,
     1                      BM,BMF,BF,BC,TC,BZZ,NPLAT,IPLAT,VOL,
     2                      IKGEO,FOR  ,MOM  ,
     3                      K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     4                      M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     5                      MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33, 
     6                      MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34,
     7                      IORTH,HMOR,HFOR ,IDRIL,HMFOR,
     8                      X13  ,X24  ,Y13  ,Y24,NEL)
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
      INTEGER JFT,JLT,NPLAT,IPLAT(*),IKGEO,IORTH,IDRIL,NEL
      MY_REAL 
     .   CDET(*),VOL(*), 
     .   BM(MVSIZ,3,3,4),BMF(MVSIZ,3,3,4),BF(MVSIZ,3,2,4),BC(MVSIZ,2,5,4),
     .   THK0(*),THK2(*),BZZ(MVSIZ,8),TC(MVSIZ,2,2),
     .    HM(MVSIZ,4),HF(MVSIZ,4),HC(MVSIZ,2),HZ(*),
     .    K11(3,3,*),K12(3,3,*),K13(3,3,*),K14(3,3,*),
     .    K22(3,3,*),K23(3,3,*),K24(3,3,*),K33(3,3,*),
     .    M11(3,3,*),M12(3,3,*),M13(3,3,*),M14(3,3,*),
     .    M22(3,3,*),M23(3,3,*),M24(3,3,*),M33(3,3,*),
     .    MF11(3,3,*),MF12(3,3,*),MF13(3,3,*),MF14(3,3,*),
     .    MF22(3,3,*),MF23(3,3,*),MF24(3,3,*),MF33(3,3,*),
     .    FM12(3,3,*),FM13(3,3,*),FM14(3,3,*),
     .    FM23(3,3,*),FM24(3,3,*),FM34(3,3,*),HMOR(MVSIZ,2),HFOR(MVSIZ,2),
     .    K34(3,3,*),K44(3,3,*),M34(3,3,*),M44(3,3,*),
     .    MF34(3,3,*),MF44(3,3,*),FOR(NEL,5),MOM(NEL,3),HMFOR(MVSIZ,6),
     .    X13(*),X24(*),Y13(*),Y24(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER EP,I,J,L,K,M,I1,J1,IN(2),NF
      MY_REAL 
     .    DM(2,2,MVSIZ),DF(2,2,MVSIZ),DC(2,2,MVSIZ),
     .    T11(2,2,MVSIZ),T12(2,2,MVSIZ),T13(2,2,MVSIZ),T14(2,2,MVSIZ),
     .    T22(2,2,MVSIZ),T23(2,2,MVSIZ),T24(2,2,MVSIZ),T33(2,2,MVSIZ),
     .    T44(2,2,MVSIZ),T34(2,2,MVSIZ),
     .    S11(2,2,MVSIZ),S12(2,2,MVSIZ),S13(2,2,MVSIZ),S14(2,2,MVSIZ),
     .    S22(2,2,MVSIZ),S23(2,2,MVSIZ),S24(2,2,MVSIZ),S33(2,2,MVSIZ),
     .    S44(2,2,MVSIZ),S34(2,2,MVSIZ),C1,C2,DHZ(MVSIZ),C3,C4,
     .    BB(2,4,MVSIZ),BBC(2,3,4,MVSIZ),GM(MVSIZ),GF(MVSIZ),
     .    FXX(MVSIZ),FYY(MVSIZ),DM5(MVSIZ),DM6(MVSIZ),DF5(MVSIZ),
     .    FXY(MVSIZ),FXY2(MVSIZ),KM12,KM21,
     .    DF6(MVSIZ),DM5_2(MVSIZ),DM6_2(MVSIZ),DF5_2(MVSIZ),
     .    DF6_2(MVSIZ),DMF(3,3,MVSIZ),FAC2Z,CBR1(4,MVSIZ),
     .    CBR2(4,MVSIZ),CBR3(4,MVSIZ),BB0(2,2,MVSIZ),T0IJ(2,2,MVSIZ),
     .    T1IJ(2,2,MVSIZ)
       DATA IN/2,1/
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
        DF(1,1,M)=HF(EP,1)*C1
        DF(2,2,M)=HF(EP,2)*C1
        DF(1,2,M)=HF(EP,3)*C1
        DF(2,1,M)=DF(1,2,M)
        GF(M) =HF(EP,4)*C1
        DHZ(M)=HZ(EP)*C1
       ENDDO
#include "vectorize.inc"
       DO M=JFT,NPLAT 
        EP=IPLAT(M)
        C2=VOL(EP)
        DC(1,1,M)=HC(EP,1)*C2
        DC(2,2,M)=HC(EP,2)*C2
       ENDDO
C
       NF =NPLAT+1
C
       DO EP=JFT,NPLAT 
         BB(1,1,EP)=BM(EP,1,1,1)
         BB(1,2,EP)=BM(EP,2,1,1)
         BB(1,3,EP)=BM(EP,3,1,1)
         BB(1,4,EP)=BM(EP,1,2,1)
         BB(2,1,EP)=BM(EP,2,2,1)
         BB(2,2,EP)=BM(EP,3,2,1)
         BB(2,3,EP)=BM(EP,1,3,1)
         BB(2,4,EP)=BM(EP,2,3,1)
       ENDDO
C
       DO I=1,2
       DO J=I,2
        DO EP=JFT,NPLAT 
         T11(I,J,EP)=BB(I,1,EP)*BB(J,1,EP)
         T11(J,I,EP)=T11(I,J,EP)
         T22(I,J,EP)=BB(I,2,EP)*BB(J,2,EP)
         T22(J,I,EP)=T22(I,J,EP)
         T33(I,J,EP)=BB(I,3,EP)*BB(J,3,EP)
         T33(J,I,EP)=T33(I,J,EP)
         T44(I,J,EP)=BB(I,4,EP)*BB(J,4,EP)
         T44(J,I,EP)=T44(I,J,EP)
         S11(IN(I),IN(J),EP)=T11(I,J,EP)
         S22(IN(I),IN(J),EP)=T22(I,J,EP)
         S33(IN(I),IN(J),EP)=T33(I,J,EP)
         S44(IN(I),IN(J),EP)=T44(I,J,EP)
       ENDDO
       ENDDO
       ENDDO
C
       DO I=1,2
       DO J=1,2
        DO EP=JFT,NPLAT 
         T12(I,J,EP)=BB(I,1,EP)*BB(J,2,EP)
         T13(I,J,EP)=BB(I,1,EP)*BB(J,3,EP)
         T14(I,J,EP)=BB(I,1,EP)*BB(J,4,EP)
         T23(I,J,EP)=BB(I,2,EP)*BB(J,3,EP)
         T24(I,J,EP)=BB(I,2,EP)*BB(J,4,EP)
         T34(I,J,EP)=BB(I,3,EP)*BB(J,4,EP)
         S12(IN(I),IN(J),EP)=T12(I,J,EP)
         S13(IN(I),IN(J),EP)=T13(I,J,EP)
         S14(IN(I),IN(J),EP)=T14(I,J,EP)
         S23(IN(I),IN(J),EP)=T23(I,J,EP)
         S24(IN(I),IN(J),EP)=T24(I,J,EP)
         S34(IN(I),IN(J),EP)=T34(I,J,EP)
        ENDDO
       ENDDO
       ENDDO
       DO EP=JFT,NPLAT 
         S11(2,1,EP)=-S11(2,1,EP)
         S11(1,2,EP)= S11(2,1,EP)
         S22(2,1,EP)=-S22(2,1,EP)
         S22(1,2,EP)= S22(2,1,EP)
         S33(2,1,EP)=-S33(2,1,EP)
         S33(1,2,EP)= S33(2,1,EP)
         S44(2,1,EP)=-S44(2,1,EP)
         S44(1,2,EP)= S44(2,1,EP)
         S12(1,2,EP)=-S12(1,2,EP)
         S12(2,1,EP)=-S12(2,1,EP)
         S13(1,2,EP)=-S13(1,2,EP)
         S13(2,1,EP)=-S13(2,1,EP)
         S14(1,2,EP)=-S14(1,2,EP)
         S14(2,1,EP)=-S14(2,1,EP)
         S23(1,2,EP)=-S23(1,2,EP)
         S23(2,1,EP)=-S23(2,1,EP)
         S24(1,2,EP)=-S24(1,2,EP)
         S24(2,1,EP)=-S24(2,1,EP)
         S34(1,2,EP)=-S34(1,2,EP)
         S34(2,1,EP)=-S34(2,1,EP)
       ENDDO
C
       DO I=1,2
       DO J=I,2
        DO EP=JFT,NPLAT 
         K11(I,J,EP)=K11(I,J,EP)+T11(I,J,EP)*DM(I,J,EP)
         K22(I,J,EP)=K22(I,J,EP)+T22(I,J,EP)*DM(I,J,EP)
         K33(I,J,EP)=K33(I,J,EP)+T33(I,J,EP)*DM(I,J,EP)
         K44(I,J,EP)=K44(I,J,EP)+T44(I,J,EP)*DM(I,J,EP)
        ENDDO
       ENDDO
       ENDDO
       DO I=1,2
        I1 = IN(I)
       DO J=I,2
        J1 = IN(J)
#include "vectorize.inc"
        DO EP=JFT,NPLAT 
         M11(I,J,EP)=M11(I,J,EP)+S11(I,J,EP)*DF(I1,J1,EP)+
     1               S11(I1,J1,EP)*GF(EP)
         M22(I,J,EP)=M22(I,J,EP)+S22(I,J,EP)*DF(I1,J1,EP)+
     1               S22(I1,J1,EP)*GF(EP)
         M33(I,J,EP)=M33(I,J,EP)+S33(I,J,EP)*DF(I1,J1,EP)+
     1               S33(I1,J1,EP)*GF(EP)
         M44(I,J,EP)=M44(I,J,EP)+S44(I,J,EP)*DF(I1,J1,EP)+
     1               S44(I1,J1,EP)*GF(EP)
        ENDDO
       ENDDO
       ENDDO
C
       DO I=1,2
       DO J=1,2
        DO EP=JFT,NPLAT 
         K12(I,J,EP)=K12(I,J,EP)+T12(I,J,EP)*DM(I,J,EP)
         K13(I,J,EP)=K13(I,J,EP)+T13(I,J,EP)*DM(I,J,EP)
         K14(I,J,EP)=K14(I,J,EP)+T14(I,J,EP)*DM(I,J,EP)
         K23(I,J,EP)=K23(I,J,EP)+T23(I,J,EP)*DM(I,J,EP)
         K24(I,J,EP)=K24(I,J,EP)+T24(I,J,EP)*DM(I,J,EP)
         K34(I,J,EP)=K34(I,J,EP)+T34(I,J,EP)*DM(I,J,EP)
        ENDDO
       ENDDO
       ENDDO
       DO I=1,2
        I1 = IN(I)
       DO J=1,2
        J1 = IN(J)
#include "vectorize.inc"
        DO EP=JFT,NPLAT 
         M12(I,J,EP)=M12(I,J,EP)+S12(I,J,EP)*DF(I1,J1,EP)+
     1               S12(I1,J1,EP)*GF(EP)
         M13(I,J,EP)=M13(I,J,EP)+S13(I,J,EP)*DF(I1,J1,EP)+
     1               S13(I1,J1,EP)*GF(EP)
         M14(I,J,EP)=M14(I,J,EP)+S14(I,J,EP)*DF(I1,J1,EP)+
     1               S14(I1,J1,EP)*GF(EP)
         M23(I,J,EP)=M23(I,J,EP)+S23(I,J,EP)*DF(I1,J1,EP)+
     1               S23(I1,J1,EP)*GF(EP)
         M24(I,J,EP)=M24(I,J,EP)+S24(I,J,EP)*DF(I1,J1,EP)+
     1               S24(I1,J1,EP)*GF(EP)
         M34(I,J,EP)=M34(I,J,EP)+S34(I,J,EP)*DF(I1,J1,EP)+
     1               S34(I1,J1,EP)*GF(EP)
        ENDDO
       ENDDO
       ENDDO
C ----add shear part with drilling dof-----
      IF (IDRIL>0) THEN
       DO I=1,2
        I1 = IN(I)
       DO J=I,2
        J1 = IN(J)
#include "vectorize.inc"
        DO EP=JFT,NPLAT
         K11(I,J,EP)=K11(I,J,EP)+T11(I1,J1,EP)*GM(EP)
         K22(I,J,EP)=K22(I,J,EP)+T22(I1,J1,EP)*GM(EP)
         K33(I,J,EP)=K33(I,J,EP)+T33(I1,J1,EP)*GM(EP)
         K44(I,J,EP)=K44(I,J,EP)+T44(I1,J1,EP)*GM(EP)
        ENDDO
       ENDDO
       ENDDO
C
       DO I=1,2
        I1 = IN(I)
       DO J=1,2
        J1 = IN(J)
#include "vectorize.inc"
        DO EP=JFT,NPLAT
         K12(I,J,EP)=K12(I,J,EP)+T12(I1,J1,EP)*GM(EP)
         K13(I,J,EP)=K13(I,J,EP)+T13(I1,J1,EP)*GM(EP)
         K14(I,J,EP)=K14(I,J,EP)+T14(I1,J1,EP)*GM(EP)
         K23(I,J,EP)=K23(I,J,EP)+T23(I1,J1,EP)*GM(EP)
         K24(I,J,EP)=K24(I,J,EP)+T24(I1,J1,EP)*GM(EP)
         K34(I,J,EP)=K34(I,J,EP)+T34(I1,J1,EP)*GM(EP)
        ENDDO
       ENDDO
       ENDDO
C---+----1----+----2----warped-+----5----+----6----+----7----+----8
       DO I=1,3
       DO J=I,3
        DO EP=NF,JLT
          K11(I,J,EP)=K11(I,J,EP)+BM(EP,3,I,1)*GM(EP)*BM(EP,3,J,1)
          K22(I,J,EP)=K22(I,J,EP)+BM(EP,3,I,2)*GM(EP)*BM(EP,3,J,2)
          K33(I,J,EP)=K33(I,J,EP)+BM(EP,3,I,3)*GM(EP)*BM(EP,3,J,3)
          K44(I,J,EP)=K44(I,J,EP)+BM(EP,3,I,4)*GM(EP)*BM(EP,3,J,4)
        ENDDO
       ENDDO
       ENDDO
C
       DO I=1,3
       DO J=1,3
        DO EP=NF,JLT
          K12(I,J,EP)=K12(I,J,EP)+BM(EP,3,I,1)*GM(EP)*BM(EP,3,J,2)
          K13(I,J,EP)=K13(I,J,EP)+BM(EP,3,I,1)*GM(EP)*BM(EP,3,J,3)
          K14(I,J,EP)=K14(I,J,EP)+BM(EP,3,I,1)*GM(EP)*BM(EP,3,J,4)
          K23(I,J,EP)=K23(I,J,EP)+BM(EP,3,I,2)*GM(EP)*BM(EP,3,J,3)
          K24(I,J,EP)=K24(I,J,EP)+BM(EP,3,I,2)*GM(EP)*BM(EP,3,J,4)
          K34(I,J,EP)=K34(I,J,EP)+BM(EP,3,I,3)*GM(EP)*BM(EP,3,J,4)
        ENDDO
       ENDDO
       ENDDO
      END IF
C ----ajoute termes sup. due au orthotrope-----
       IF (IORTH>0) THEN
C ----add mem-bending coupling terms of orthotrope--coplanar first---
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
C------add here before TIJ(1,2,*)... have been modified --TIJ(i,j)=B(i,I)*B(j,J)----i,j->1:x;2:y
       IF (IDRIL >0 ) THEN
        CALL CBALKEORMF(JFT    ,NPLAT   ,MF11   ,DMF   ,T11   )
        CALL CBALKEORMF(JFT    ,NPLAT   ,MF12   ,DMF   ,T12   )
        CALL CBALKEORMF(JFT    ,NPLAT   ,MF13   ,DMF   ,T13   )
        CALL CBALKEORMF(JFT    ,NPLAT   ,MF14   ,DMF   ,T14   )
        CALL CBALKEORMF(JFT    ,NPLAT   ,MF22   ,DMF   ,T22   )
        CALL CBALKEORMF(JFT    ,NPLAT   ,MF23   ,DMF   ,T23   )
        CALL CBALKEORMF(JFT    ,NPLAT   ,MF24   ,DMF   ,T24   )
        CALL CBALKEORMF(JFT    ,NPLAT   ,MF33   ,DMF   ,T33   )
        CALL CBALKEORMF(JFT    ,NPLAT   ,MF34   ,DMF   ,T34   )
        CALL CBALKEORMF(JFT    ,NPLAT   ,MF44   ,DMF   ,T44   )
C        
        CALL CBALKEORFM(JFT    ,NPLAT   ,FM12   ,DMF   ,T12   )
        CALL CBALKEORFM(JFT    ,NPLAT   ,FM13   ,DMF   ,T13   )
        CALL CBALKEORFM(JFT    ,NPLAT   ,FM14   ,DMF   ,T14   )
        CALL CBALKEORFM(JFT    ,NPLAT   ,FM23   ,DMF   ,T23   )
        CALL CBALKEORFM(JFT    ,NPLAT   ,FM24   ,DMF   ,T24   )
        CALL CBALKEORFM(JFT    ,NPLAT   ,FM34   ,DMF   ,T34   )
       ELSE
C-----------due to constant in-plane shear --------------      
#include "vectorize.inc"
        DO M=JFT,NPLAT 
         EP=IPLAT(M)
         BB0(1,1,M)=Y24(EP)
         BB0(1,2,M)=-Y13(EP)
         BB0(2,1,M)=-X24(EP)
         BB0(2,2,M)=X13(EP)
        ENDDO
C
        DO EP=JFT,NPLAT 
         T0IJ(1,1,EP)=BB0(1,1,EP)*BB(1,1,EP)
         T0IJ(1,2,EP)=BB0(1,1,EP)*BB(2,1,EP)
         T0IJ(2,1,EP)=BB0(2,1,EP)*BB(1,1,EP)
         T0IJ(2,2,EP)=BB0(2,1,EP)*BB(2,1,EP)
         T1IJ(1,1,EP)=-T0IJ(1,1,EP)
         T1IJ(1,2,EP)=-T0IJ(2,1,EP)
         T1IJ(2,1,EP)=-T0IJ(1,2,EP)
         T1IJ(2,2,EP)=-T0IJ(2,2,EP)
        ENDDO
        CALL CBALKEORMF1(JFT  ,NPLAT  ,MF11  ,DMF  ,T11  ,T0IJ )
        CALL CBALKEORFM1(JFT  ,NPLAT  ,FM13  ,DMF  ,T13  ,T1IJ )
        DO EP=JFT,NPLAT 
         T0IJ(1,1,EP)=BB0(1,1,EP)*BB(1,2,EP)
         T0IJ(1,2,EP)=BB0(1,1,EP)*BB(2,2,EP)
         T0IJ(2,1,EP)=BB0(2,1,EP)*BB(1,2,EP)
         T0IJ(2,2,EP)=BB0(2,1,EP)*BB(2,2,EP)
         T1IJ(1,1,EP)=-T0IJ(1,1,EP)
         T1IJ(1,2,EP)=-T0IJ(2,1,EP)
         T1IJ(2,1,EP)=-T0IJ(1,2,EP)
         T1IJ(2,2,EP)=-T0IJ(2,2,EP)
        ENDDO
        CALL CBALKEORMF1(JFT  ,NPLAT  ,MF12  ,DMF  ,T12  ,T0IJ )
        CALL CBALKEORFM1(JFT  ,NPLAT  ,FM23  ,DMF  ,T23  ,T1IJ )
        DO EP=JFT,NPLAT 
         T0IJ(1,1,EP)=BB0(1,1,EP)*BB(1,3,EP)
         T0IJ(1,2,EP)=BB0(1,1,EP)*BB(2,3,EP)
         T0IJ(2,1,EP)=BB0(2,1,EP)*BB(1,3,EP)
         T0IJ(2,2,EP)=BB0(2,1,EP)*BB(2,3,EP)
         T1IJ(1,1,EP)=-T0IJ(1,1,EP)
         T1IJ(1,2,EP)=-T0IJ(1,2,EP)
         T1IJ(2,1,EP)=-T0IJ(2,1,EP)
         T1IJ(2,2,EP)=-T0IJ(2,2,EP)
        ENDDO
        CALL CBALKEORMF1(JFT  ,NPLAT  ,MF13  ,DMF  ,T13  ,T0IJ )
        CALL CBALKEORMF1(JFT  ,NPLAT  ,MF33  ,DMF  ,T33  ,T1IJ )
        DO EP=JFT,NPLAT 
         T0IJ(1,1,EP)=BB0(1,1,EP)*BB(1,4,EP)
         T0IJ(1,2,EP)=BB0(1,1,EP)*BB(2,4,EP)
         T0IJ(2,1,EP)=BB0(2,1,EP)*BB(1,4,EP)
         T0IJ(2,2,EP)=BB0(2,1,EP)*BB(2,4,EP)
         T1IJ(1,1,EP)=-T0IJ(1,1,EP)
         T1IJ(1,2,EP)=-T0IJ(1,2,EP)
         T1IJ(2,1,EP)=-T0IJ(2,1,EP)
         T1IJ(2,2,EP)=-T0IJ(2,2,EP)
        ENDDO
        CALL CBALKEORMF1(JFT  ,NPLAT  ,MF14  ,DMF  ,T14  ,T0IJ )
        CALL CBALKEORMF1(JFT  ,NPLAT  ,MF34  ,DMF  ,T34  ,T1IJ )
        DO EP=JFT,NPLAT 
         T0IJ(1,1,EP)=BB0(1,2,EP)*BB(1,2,EP)
         T0IJ(1,2,EP)=BB0(1,2,EP)*BB(2,2,EP)
         T0IJ(2,1,EP)=BB0(2,2,EP)*BB(1,2,EP)
         T0IJ(2,2,EP)=BB0(2,2,EP)*BB(2,2,EP)
         T1IJ(1,1,EP)=-T0IJ(1,1,EP)
         T1IJ(1,2,EP)=-T0IJ(2,1,EP)
         T1IJ(2,1,EP)=-T0IJ(1,2,EP)
         T1IJ(2,2,EP)=-T0IJ(2,2,EP)
        ENDDO
        CALL CBALKEORMF1(JFT  ,NPLAT  ,MF22  ,DMF  ,T22  ,T0IJ )
        CALL CBALKEORFM1(JFT  ,NPLAT  ,FM24  ,DMF  ,T24  ,T1IJ )
        DO EP=JFT,NPLAT 
         T0IJ(1,1,EP)=BB0(1,2,EP)*BB(1,3,EP)
         T0IJ(1,2,EP)=BB0(1,2,EP)*BB(2,3,EP)
         T0IJ(2,1,EP)=BB0(2,2,EP)*BB(1,3,EP)
         T0IJ(2,2,EP)=BB0(2,2,EP)*BB(2,3,EP)
         T1IJ(1,1,EP)=-T0IJ(1,1,EP)
         T1IJ(1,2,EP)=-T0IJ(2,1,EP)
         T1IJ(2,1,EP)=-T0IJ(1,2,EP)
         T1IJ(2,2,EP)=-T0IJ(2,2,EP)
        ENDDO
        CALL CBALKEORMF1(JFT  ,NPLAT  ,MF23  ,DMF  ,T23  ,T0IJ )
        CALL CBALKEORFM1(JFT  ,NPLAT  ,FM34  ,DMF  ,T34  ,T1IJ )
        DO EP=JFT,NPLAT 
         T0IJ(1,1,EP)=BB0(1,2,EP)*BB(1,4,EP)
         T0IJ(1,2,EP)=BB0(1,2,EP)*BB(2,4,EP)
         T0IJ(2,1,EP)=BB0(2,2,EP)*BB(1,4,EP)
         T0IJ(2,2,EP)=BB0(2,2,EP)*BB(2,4,EP)
         T1IJ(1,1,EP)=-T0IJ(1,1,EP)
         T1IJ(1,2,EP)=-T0IJ(1,2,EP)
         T1IJ(2,1,EP)=-T0IJ(2,1,EP)
         T1IJ(2,2,EP)=-T0IJ(2,2,EP)
        ENDDO
        CALL CBALKEORMF1(JFT  ,NPLAT  ,MF24  ,DMF  ,T24  ,T0IJ )
        CALL CBALKEORMF1(JFT  ,NPLAT  ,MF44  ,DMF  ,T44  ,T1IJ )
C        
        DO EP=JFT,NPLAT 
         T0IJ(1,1,EP)=BB(1,1,EP)*BB0(1,2,EP)
         T0IJ(1,2,EP)=BB(1,1,EP)*BB0(2,2,EP)
         T0IJ(2,1,EP)=BB(2,1,EP)*BB0(1,2,EP)
         T0IJ(2,2,EP)=BB(2,1,EP)*BB0(2,2,EP)
         T1IJ(1,1,EP)=-T0IJ(1,1,EP)
         T1IJ(1,2,EP)=-T0IJ(1,2,EP)
         T1IJ(2,1,EP)=-T0IJ(2,1,EP)
         T1IJ(2,2,EP)=-T0IJ(2,2,EP)
        ENDDO
        CALL CBALKEORFM1(JFT  ,NPLAT  ,FM12  ,DMF  ,T12  ,T0IJ )
        CALL CBALKEORFM1(JFT  ,NPLAT  ,FM14  ,DMF  ,T14  ,T1IJ )
       END IF !(IDRIL >0 ) THEN
C        
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
        DO EP=JFT,NPLAT 
         T12(1,2,EP)=HALF*(T12(1,2,EP)+T12(2,1,EP))
         T13(1,2,EP)=HALF*(T13(1,2,EP)+T13(2,1,EP))
         T14(1,2,EP)=HALF*(T14(1,2,EP)+T14(2,1,EP))
         T23(1,2,EP)=HALF*(T23(1,2,EP)+T23(2,1,EP))
         T24(1,2,EP)=HALF*(T24(1,2,EP)+T24(2,1,EP))
         T34(1,2,EP)=HALF*(T34(1,2,EP)+T34(2,1,EP))
        ENDDO
       IF (IDRIL >0 ) THEN
        DO EP=JFT,NPLAT 
         K11(1,1,EP)=K11(1,1,EP)+T11(1,2,EP)*DM5_2(EP)
         KM12       =T11(1,1,EP)*DM5(EP)+T11(2,2,EP)*DM6(EP)
         K11(1,2,EP)=K11(1,2,EP)+KM12
         K11(2,2,EP)=K11(2,2,EP)+T11(1,2,EP)*DM6_2(EP)
         K12(1,1,EP)=K12(1,1,EP)+T12(1,2,EP)*DM5_2(EP)
         KM12       =T12(1,1,EP)*DM5(EP)+T12(2,2,EP)*DM6(EP)
         K12(1,2,EP)=K12(1,2,EP)+KM12
         K12(2,1,EP)=K12(2,1,EP)+KM12
         K12(2,2,EP)=K12(2,2,EP)+T12(1,2,EP)*DM6_2(EP)
         K13(1,1,EP)=K13(1,1,EP)+T13(1,2,EP)*DM5_2(EP)
         KM12       =T13(1,1,EP)*DM5(EP)+T13(2,2,EP)*DM6(EP)
         K13(1,2,EP)=K13(1,2,EP)+KM12
         K13(2,1,EP)=K13(2,1,EP)+KM12
         K13(2,2,EP)=K13(2,2,EP)+T13(1,2,EP)*DM6_2(EP)
         K14(1,1,EP)=K14(1,1,EP)+T14(1,2,EP)*DM5_2(EP)
         KM12       =T14(1,1,EP)*DM5(EP)+T14(2,2,EP)*DM6(EP)
         K14(1,2,EP)=K14(1,2,EP)+KM12
         K14(2,1,EP)=K14(2,1,EP)+KM12
         K14(2,2,EP)=K14(2,2,EP)+T14(1,2,EP)*DM6_2(EP)
         K22(1,1,EP)=K22(1,1,EP)+T22(1,2,EP)*DM5_2(EP)
         KM12       =T22(1,1,EP)*DM5(EP)+T22(2,2,EP)*DM6(EP)
         K22(1,2,EP)=K22(1,2,EP)+KM12
         K22(2,2,EP)=K22(2,2,EP)+T22(1,2,EP)*DM6_2(EP)
         K23(1,1,EP)=K23(1,1,EP)+T23(1,2,EP)*DM5_2(EP)
         KM12       =T23(1,1,EP)*DM5(EP)+T23(2,2,EP)*DM6(EP)
         K23(1,2,EP)=K23(1,2,EP)+KM12
         K23(2,1,EP)=K23(2,1,EP)+KM12
         K23(2,2,EP)=K23(2,2,EP)+T23(1,2,EP)*DM6_2(EP)
         K24(1,1,EP)=K24(1,1,EP)+T24(1,2,EP)*DM5_2(EP)
         KM12       =T24(1,1,EP)*DM5(EP)+T24(2,2,EP)*DM6(EP)
         K24(1,2,EP)=K24(1,2,EP)+KM12
         K24(2,1,EP)=K24(2,1,EP)+KM12
         K24(2,2,EP)=K24(2,2,EP)+T24(1,2,EP)*DM6_2(EP)
         K33(1,1,EP)=K33(1,1,EP)+T33(1,2,EP)*DM5_2(EP)
         KM12       =T33(1,1,EP)*DM5(EP)+T33(2,2,EP)*DM6(EP)
         K33(1,2,EP)=K33(1,2,EP)+KM12 
         K33(2,2,EP)=K33(2,2,EP)+T33(1,2,EP)*DM6_2(EP)
         K34(1,1,EP)=K34(1,1,EP)+T34(1,2,EP)*DM5_2(EP)
         KM12       =T34(1,1,EP)*DM5(EP)+T34(2,2,EP)*DM6(EP)
         K34(1,2,EP)=K34(1,2,EP)+KM12
         K34(2,1,EP)=K34(2,1,EP)+KM12
         K34(2,2,EP)=K34(2,2,EP)+T34(1,2,EP)*DM6_2(EP)
         K44(1,1,EP)=K44(1,1,EP)+T44(1,2,EP)*DM5_2(EP)
         KM12       =T44(1,1,EP)*DM5(EP)+T44(2,2,EP)*DM6(EP)
         K44(1,2,EP)=K44(1,2,EP)+KM12
         K44(2,2,EP)=K44(2,2,EP)+T44(1,2,EP)*DM6_2(EP)
        ENDDO
       ELSE
C K11 ,K13  ,K33  
        DO EP=JFT,NPLAT 
         T0IJ(1,1,EP)=BB(1,1,EP)*BB0(1,1,EP)
         T0IJ(1,2,EP)=BB(1,1,EP)*BB0(2,1,EP)
         T0IJ(2,1,EP)=BB(2,1,EP)*BB0(1,1,EP)
         T0IJ(2,2,EP)=BB(2,1,EP)*BB0(2,1,EP)
         T1IJ(1,1,EP)=T0IJ(1,1,EP)
         T1IJ(1,2,EP)=T0IJ(2,1,EP)
         T1IJ(2,1,EP)=T0IJ(1,2,EP)
         T1IJ(2,2,EP)=T0IJ(2,2,EP)
         K11(1,1,EP)=K11(1,1,EP)+(T0IJ(1,2,EP)+T1IJ(2,1,EP))*DM5(EP)
         KM12       =T0IJ(1,1,EP)*DM5(EP)+T1IJ(2,2,EP)*DM6(EP)
         K11(1,2,EP)=K11(1,2,EP)+KM12
         K11(2,2,EP)=K11(2,2,EP)+(T0IJ(2,1,EP)+T1IJ(1,2,EP))*DM6(EP)
C K13  T0IJ(13)=-T0IJ(11) 
         T1IJ(1,1,EP)=BB0(1,1,EP)*BB(1,3,EP)
         T1IJ(1,2,EP)=BB0(1,1,EP)*BB(2,3,EP)
         T1IJ(2,1,EP)=BB0(2,1,EP)*BB(1,3,EP)
         T1IJ(2,2,EP)=BB0(2,1,EP)*BB(2,3,EP)
         K13(1,1,EP)=K13(1,1,EP)-(T0IJ(1,2,EP)-T1IJ(2,1,EP))*DM5(EP)
         KM12       =-T0IJ(1,1,EP)*DM5(EP)+T1IJ(2,2,EP)*DM6(EP)
         K13(1,2,EP)=K13(1,2,EP)+KM12
         KM21       =T1IJ(1,1,EP)*DM5(EP)-T0IJ(2,2,EP)*DM6(EP)
         K13(2,1,EP)=K13(2,1,EP)+KM21
         K13(2,2,EP)=K13(2,2,EP)-(T0IJ(2,1,EP)-T1IJ(1,2,EP))*DM6(EP)
C-----K33, T1IJ(33)=-T1IJ(13),T0IJ(i,j)=T1IJ(33)(j,i)=-T0IJ(13)        
         T0IJ(1,1,EP)=T1IJ(1,1,EP)
         T0IJ(1,2,EP)=T1IJ(2,1,EP)
         T0IJ(2,1,EP)=T1IJ(1,2,EP)
         T0IJ(2,2,EP)=T1IJ(2,2,EP)
         K33(1,1,EP)=K33(1,1,EP)-(T0IJ(1,2,EP)+T1IJ(2,1,EP))*DM5(EP)
         KM12       =-T0IJ(1,1,EP)*DM5(EP)-T1IJ(2,2,EP)*DM6(EP)
         K33(1,2,EP)=K33(1,2,EP)+KM12
         K33(2,2,EP)=K33(2,2,EP)-(T0IJ(2,1,EP)+T1IJ(1,2,EP))*DM6(EP)
        ENDDO
C K12,K14,K34 ,K23   
        DO EP=JFT,NPLAT 
         T0IJ(1,1,EP)=BB(1,1,EP)*BB0(1,2,EP)
         T0IJ(1,2,EP)=BB(1,1,EP)*BB0(2,2,EP)
         T0IJ(2,1,EP)=BB(2,1,EP)*BB0(1,2,EP)
         T0IJ(2,2,EP)=BB(2,1,EP)*BB0(2,2,EP)
         T1IJ(1,1,EP)=BB0(1,1,EP)*BB(1,2,EP)
         T1IJ(1,2,EP)=BB0(1,1,EP)*BB(2,2,EP)
         T1IJ(2,1,EP)=BB0(2,1,EP)*BB(1,2,EP)
         T1IJ(2,2,EP)=BB0(2,1,EP)*BB(2,2,EP)
         K12(1,1,EP)=K12(1,1,EP)+(T0IJ(1,2,EP)+T1IJ(2,1,EP))*DM5(EP)
         KM12       =T0IJ(1,1,EP)*DM5(EP)+T1IJ(2,2,EP)*DM6(EP)
         K12(1,2,EP)=K12(1,2,EP)+KM12
         KM21       =T1IJ(1,1,EP)*DM5(EP)+T0IJ(2,2,EP)*DM6(EP)
         K12(2,1,EP)=K12(2,1,EP)+KM21
         K12(2,2,EP)=K12(2,2,EP)+(T0IJ(2,1,EP)+T1IJ(1,2,EP))*DM6(EP)
C-----K14, T0IJ(14)=-T0IJ(12),    
         T1IJ(1,1,EP)=BB0(1,1,EP)*BB(1,4,EP)
         T1IJ(1,2,EP)=BB0(1,1,EP)*BB(2,4,EP)
         T1IJ(2,1,EP)=BB0(2,1,EP)*BB(1,4,EP)
         T1IJ(2,2,EP)=BB0(2,1,EP)*BB(2,4,EP)
         K14(1,1,EP)=K14(1,1,EP)-(T0IJ(1,2,EP)-T1IJ(2,1,EP))*DM5(EP)
         KM12       =-T0IJ(1,1,EP)*DM5(EP)+T1IJ(2,2,EP)*DM6(EP)
         K14(1,2,EP)=K14(1,2,EP)+KM12
         KM21       =T1IJ(1,1,EP)*DM5(EP)-T0IJ(2,2,EP)*DM6(EP)
         K14(2,1,EP)=K14(2,1,EP)+KM21
         K14(2,2,EP)=K14(2,2,EP)-(T0IJ(2,1,EP)-T1IJ(1,2,EP))*DM6(EP)
C-----K34, T1IJ(34)=-T1IJ(14), T0IJ(34)=-T0IJ(32)   
         T0IJ(1,1,EP)=BB(1,3,EP)*BB0(1,2,EP)
         T0IJ(1,2,EP)=BB(1,3,EP)*BB0(2,2,EP)
         T0IJ(2,1,EP)=BB(2,3,EP)*BB0(1,2,EP)
         T0IJ(2,2,EP)=BB(2,3,EP)*BB0(2,2,EP)
         K34(1,1,EP)=K34(1,1,EP)-(T0IJ(1,2,EP)+T1IJ(2,1,EP))*DM5(EP)
         KM12       =-T0IJ(1,1,EP)*DM5(EP)-T1IJ(2,2,EP)*DM6(EP)
         K34(1,2,EP)=K34(1,2,EP)+KM12
         KM21       =-T1IJ(1,1,EP)*DM5(EP)-T0IJ(2,2,EP)*DM6(EP)
         K34(2,1,EP)=K34(2,1,EP)+KM21
         K34(2,2,EP)=K34(2,2,EP)-(T0IJ(2,1,EP)+T1IJ(1,2,EP))*DM6(EP)
C-----K23, T1IJ(i,j)=-T0IJ(34)(j,i)=T0IJ(32)(j,i),    
         T1IJ(1,1,EP)=T0IJ(1,1,EP)
         T1IJ(1,2,EP)=T0IJ(2,1,EP)
         T1IJ(2,1,EP)=T0IJ(1,2,EP)
         T1IJ(2,2,EP)=T0IJ(2,2,EP)
         T0IJ(1,1,EP)=-BB(1,2,EP)*BB0(1,1,EP)
         T0IJ(1,2,EP)=-BB(1,2,EP)*BB0(2,1,EP)
         T0IJ(2,1,EP)=-BB(2,2,EP)*BB0(1,1,EP)
         T0IJ(2,2,EP)=-BB(2,2,EP)*BB0(2,1,EP)
         K23(1,1,EP)=K23(1,1,EP)+(T0IJ(1,2,EP)+T1IJ(2,1,EP))*DM5(EP)
         KM12       =T0IJ(1,1,EP)*DM5(EP)+T1IJ(2,2,EP)*DM6(EP)
         K23(1,2,EP)=K23(1,2,EP)+KM12
         KM21       =T1IJ(1,1,EP)*DM5(EP)+T0IJ(2,2,EP)*DM6(EP)
         K23(2,1,EP)=K23(2,1,EP)+KM21
         K23(2,2,EP)=K23(2,2,EP)+(T0IJ(2,1,EP)+T1IJ(1,2,EP))*DM6(EP)
        ENDDO
C K22 ,K24 ,K44   
        DO EP=JFT,NPLAT 
         T1IJ(1,1,EP)=BB0(1,2,EP)*BB(1,2,EP)
         T1IJ(1,2,EP)=BB0(1,2,EP)*BB(2,2,EP)
         T1IJ(2,1,EP)=BB0(2,2,EP)*BB(1,2,EP)
         T1IJ(2,2,EP)=BB0(2,2,EP)*BB(2,2,EP)
         T0IJ(1,1,EP)=T1IJ(1,1,EP)
         T0IJ(1,2,EP)=T1IJ(2,1,EP)
         T0IJ(2,1,EP)=T1IJ(1,2,EP)
         T0IJ(2,2,EP)=T1IJ(2,2,EP)
         K22(1,1,EP)=K22(1,1,EP)+(T0IJ(1,2,EP)+T1IJ(2,1,EP))*DM5(EP)
         KM12       =T0IJ(1,1,EP)*DM5(EP)+T1IJ(2,2,EP)*DM6(EP)
         K22(1,2,EP)=K22(1,2,EP)+KM12
         K22(2,2,EP)=K22(2,2,EP)+(T0IJ(2,1,EP)+T1IJ(1,2,EP))*DM6(EP)
C---K24, T0IJ(24)=-T0IJ(22)         
         T1IJ(1,1,EP)=BB0(1,2,EP)*BB(1,4,EP)
         T1IJ(1,2,EP)=BB0(1,2,EP)*BB(2,4,EP)
         T1IJ(2,1,EP)=BB0(2,2,EP)*BB(1,4,EP)
         T1IJ(2,2,EP)=BB0(2,2,EP)*BB(2,4,EP)
         K24(1,1,EP)=K24(1,1,EP)-(T0IJ(1,2,EP)-T1IJ(2,1,EP))*DM5(EP)
         KM12       =-T0IJ(1,1,EP)*DM5(EP)+T1IJ(2,2,EP)*DM6(EP)
         K24(1,2,EP)=K24(1,2,EP)+KM12
         KM21       =T1IJ(1,1,EP)*DM5(EP)-T0IJ(2,2,EP)*DM6(EP)
         K24(2,1,EP)=K24(2,1,EP)+KM21
         K24(2,2,EP)=K24(2,2,EP)-(T0IJ(2,1,EP)-T1IJ(1,2,EP))*DM6(EP)
C---K44, T1IJ(44)=-T1IJ(24) ; T0IJ(44)=-T0IJ(42)     
         T0IJ(1,1,EP)=BB(1,4,EP)*BB0(1,2,EP)
         T0IJ(1,2,EP)=BB(1,4,EP)*BB0(2,2,EP)
         T0IJ(2,1,EP)=BB(2,4,EP)*BB0(1,2,EP)
         T0IJ(2,2,EP)=BB(2,4,EP)*BB0(2,2,EP)
         K44(1,1,EP)=K44(1,1,EP)-(T0IJ(1,2,EP)+T1IJ(2,1,EP))*DM5(EP)
         KM12       =-T0IJ(1,1,EP)*DM5(EP)-T1IJ(2,2,EP)*DM6(EP)
         K44(1,2,EP)=K44(1,2,EP)+KM12
         K44(2,2,EP)=K44(2,2,EP)-(T0IJ(2,1,EP)+T1IJ(1,2,EP))*DM6(EP)
        ENDDO
       END IF !(IDRIL >0 ) THEN
C        
        DO EP=JFT,NPLAT 
         M11(1,1,EP)=M11(1,1,EP)+T11(1,2,EP)*DF6_2(EP)
         KM12       =T11(1,1,EP)*DF5(EP)+T11(2,2,EP)*DF6(EP)
         M11(1,2,EP)=M11(1,2,EP)-KM12
         M11(2,2,EP)=M11(2,2,EP)+T11(1,2,EP)*DF5_2(EP)
	 
         M12(1,1,EP)=M12(1,1,EP)+T12(1,2,EP)*DF6_2(EP)	 
         KM12       =T12(1,1,EP)*DF5(EP)+T12(2,2,EP)*DF6(EP)
         M12(1,2,EP)=M12(1,2,EP)-KM12
         M12(2,1,EP)=M12(2,1,EP)-KM12
         M12(2,2,EP)=M12(2,2,EP)+T12(1,2,EP)*DF5_2(EP)
	 
         M13(1,1,EP)=M13(1,1,EP)+T13(1,2,EP)*DF6_2(EP)	 
         KM12       =T13(1,1,EP)*DF5(EP)+T13(2,2,EP)*DF6(EP)
         M13(1,2,EP)=M13(1,2,EP)-KM12
         M13(2,1,EP)=M13(2,1,EP)-KM12
         M13(2,2,EP)=M13(2,2,EP)+T13(1,2,EP)*DF5_2(EP)
	 
         M14(1,1,EP)=M14(1,1,EP)+T14(1,2,EP)*DF6_2(EP)
         KM12       =T14(1,1,EP)*DF5(EP)+T14(2,2,EP)*DF6(EP)
         M14(1,2,EP)=M14(1,2,EP)-KM12
         M14(2,1,EP)=M14(2,1,EP)-KM12
         M14(2,2,EP)=M14(2,2,EP)+T14(1,2,EP)*DF5_2(EP)
	 
         M22(1,1,EP)=M22(1,1,EP)+T22(1,2,EP)*DF6_2(EP)	 
         KM12       =T22(1,1,EP)*DF5(EP)+T22(2,2,EP)*DF6(EP)
         M22(1,2,EP)=M22(1,2,EP)-KM12
         M22(2,2,EP)=M22(2,2,EP)+T22(1,2,EP)*DF5_2(EP)
	 
         M23(1,1,EP)=M23(1,1,EP)+T23(1,2,EP)*DF6_2(EP)
         KM12       =T23(1,1,EP)*DF5(EP)+T23(2,2,EP)*DF6(EP)
         M23(1,2,EP)=M23(1,2,EP)-KM12
         M23(2,1,EP)=M23(2,1,EP)-KM12
         M23(2,2,EP)=M23(2,2,EP)+T23(1,2,EP)*DF5_2(EP)
	 
         M24(1,1,EP)=M24(1,1,EP)+T24(1,2,EP)*DF6_2(EP)
         KM12       =T24(1,1,EP)*DF5(EP)+T24(2,2,EP)*DF6(EP)
         M24(1,2,EP)=M24(1,2,EP)-KM12
         M24(2,1,EP)=M24(2,1,EP)-KM12
         M24(2,2,EP)=M24(2,2,EP)+T24(1,2,EP)*DF5_2(EP)
	 
         M33(1,1,EP)=M33(1,1,EP)+T33(1,2,EP)*DF6_2(EP)
         KM12       =T33(1,1,EP)*DF5(EP)+T33(2,2,EP)*DF6(EP)
         M33(1,2,EP)=M33(1,2,EP)-KM12
         M33(2,2,EP)=M33(2,2,EP)+T33(1,2,EP)*DF5_2(EP)
	 
         M34(1,1,EP)=M34(1,1,EP)+T34(1,2,EP)*DF6_2(EP)
         KM12       =T34(1,1,EP)*DF5(EP)+T34(2,2,EP)*DF6(EP)
         M34(1,2,EP)=M34(1,2,EP)-KM12
         M34(2,1,EP)=M34(2,1,EP)-KM12
         M34(2,2,EP)=M34(2,2,EP)+T34(1,2,EP)*DF5_2(EP)
	 
         M44(1,1,EP)=M44(1,1,EP)+T44(1,2,EP)*DF6_2(EP)
         KM12       =T44(1,1,EP)*DF5(EP)+T44(2,2,EP)*DF6(EP)
         M44(1,2,EP)=M44(1,2,EP)-KM12
         M44(2,2,EP)=M44(2,2,EP)+T44(1,2,EP)*DF5_2(EP)
        ENDDO
       ENDIF 
C
       DO EP=JFT,NPLAT 
         BBC(1,1,1,EP)=BC(EP,1,1,1)
         BBC(2,1,1,EP)=BC(EP,2,1,1)
         BBC(1,2,1,EP)=BC(EP,1,2,1)
         BBC(2,2,1,EP)=BC(EP,2,2,1)
         BBC(1,3,1,EP)=BC(EP,1,3,1)
         BBC(2,3,1,EP)=BC(EP,2,3,1)
         BBC(1,1,2,EP)=BC(EP,1,4,1)
         BBC(2,1,2,EP)=BC(EP,2,4,1)
         BBC(1,2,2,EP)=BC(EP,1,5,1)
         BBC(2,2,2,EP)=BC(EP,2,5,1)
         BBC(1,3,2,EP)=BC(EP,1,1,2)
         BBC(2,3,2,EP)=BC(EP,2,1,2)
         BBC(1,1,3,EP)=BC(EP,1,2,2)
         BBC(2,1,3,EP)=BC(EP,2,2,2)
         BBC(1,2,3,EP)=BC(EP,1,3,2)
         BBC(2,2,3,EP)=BC(EP,2,3,2)
         BBC(1,3,3,EP)=BC(EP,1,4,2)
         BBC(2,3,3,EP)=BC(EP,2,4,2)
         BBC(1,1,4,EP)=BC(EP,1,5,2)
         BBC(2,1,4,EP)=BC(EP,2,5,2)
         BBC(1,2,4,EP)=BC(EP,1,1,3)
         BBC(2,2,4,EP)=BC(EP,2,1,3)
         BBC(1,3,4,EP)=BC(EP,1,2,3)
         BBC(2,3,4,EP)=BC(EP,2,2,3)
        ENDDO
C
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
       DO I=1,2
        I1 =I+1
       DO J=I,2
        J1 =J+1
        DO EP=JFT,NPLAT
          M11(I,J,EP)=M11(I,J,EP)+
     1                BBC(1,I1,1,EP)*DC(1,1,EP)*BBC(1,J1,1,EP)+
     2                BBC(2,I1,1,EP)*DC(2,2,EP)*BBC(2,J1,1,EP)
          M22(I,J,EP)=M22(I,J,EP)+
     1                BBC(1,I1,2,EP)*DC(1,1,EP)*BBC(1,J1,2,EP)+
     2                BBC(2,I1,2,EP)*DC(2,2,EP)*BBC(2,J1,2,EP)
          M33(I,J,EP)=M33(I,J,EP)+
     1                BBC(1,I1,3,EP)*DC(1,1,EP)*BBC(1,J1,3,EP)+
     2                BBC(2,I1,3,EP)*DC(2,2,EP)*BBC(2,J1,3,EP)
          M44(I,J,EP)=M44(I,J,EP)+
     1                BBC(1,I1,4,EP)*DC(1,1,EP)*BBC(1,J1,4,EP)+
     2                BBC(2,I1,4,EP)*DC(2,2,EP)*BBC(2,J1,4,EP)
        ENDDO
       ENDDO
       ENDDO
C
       DO I=1,2
        I1 =I+1
       DO J=1,2
        J1 =J+1
         DO EP=JFT,NPLAT
          M12(I,J,EP)=M12(I,J,EP)+
     1                BBC(1,I1,1,EP)*DC(1,1,EP)*BBC(1,J1,2,EP)+
     2                BBC(2,I1,1,EP)*DC(2,2,EP)*BBC(2,J1,2,EP)
          M13(I,J,EP)=M13(I,J,EP)+
     1                BBC(1,I1,1,EP)*DC(1,1,EP)*BBC(1,J1,3,EP)+
     2                BBC(2,I1,1,EP)*DC(2,2,EP)*BBC(2,J1,3,EP)
          M14(I,J,EP)=M14(I,J,EP)+
     1                BBC(1,I1,1,EP)*DC(1,1,EP)*BBC(1,J1,4,EP)+
     2                BBC(2,I1,1,EP)*DC(2,2,EP)*BBC(2,J1,4,EP)
          M23(I,J,EP)=M23(I,J,EP)+
     1                BBC(1,I1,2,EP)*DC(1,1,EP)*BBC(1,J1,3,EP)+
     2                BBC(2,I1,2,EP)*DC(2,2,EP)*BBC(2,J1,3,EP)
          M24(I,J,EP)=M24(I,J,EP)+
     1                BBC(1,I1,2,EP)*DC(1,1,EP)*BBC(1,J1,4,EP)+
     2                BBC(2,I1,2,EP)*DC(2,2,EP)*BBC(2,J1,4,EP)
          M34(I,J,EP)=M34(I,J,EP)+
     1                BBC(1,I1,3,EP)*DC(1,1,EP)*BBC(1,J1,4,EP)+
     2                BBC(2,I1,3,EP)*DC(2,2,EP)*BBC(2,J1,4,EP)
        ENDDO
       ENDDO
       ENDDO
C
        DO EP=JFT,NPLAT
          K11(3,3,EP)=K11(3,3,EP)+
     1                BBC(1,1,1,EP)*DC(1,1,EP)*BBC(1,1,1,EP)+
     2                BBC(2,1,1,EP)*DC(2,2,EP)*BBC(2,1,1,EP)
          K12(3,3,EP)=K12(3,3,EP)+
     1                BBC(1,1,1,EP)*DC(1,1,EP)*BBC(1,1,2,EP)+
     2                BBC(2,1,1,EP)*DC(2,2,EP)*BBC(2,1,2,EP)
          K13(3,3,EP)=K13(3,3,EP)+
     1                BBC(1,1,1,EP)*DC(1,1,EP)*BBC(1,1,3,EP)+
     2                BBC(2,1,1,EP)*DC(2,2,EP)*BBC(2,1,3,EP)
          K14(3,3,EP)=K14(3,3,EP)+
     1                BBC(1,1,1,EP)*DC(1,1,EP)*BBC(1,1,4,EP)+
     2                BBC(2,1,1,EP)*DC(2,2,EP)*BBC(2,1,4,EP)
          K22(3,3,EP)=K22(3,3,EP)+
     1                BBC(1,1,2,EP)*DC(1,1,EP)*BBC(1,1,2,EP)+
     2                BBC(2,1,2,EP)*DC(2,2,EP)*BBC(2,1,2,EP)
          K23(3,3,EP)=K23(3,3,EP)+
     1                BBC(1,1,2,EP)*DC(1,1,EP)*BBC(1,1,3,EP)+
     2                BBC(2,1,2,EP)*DC(2,2,EP)*BBC(2,1,3,EP)
          K24(3,3,EP)=K24(3,3,EP)+
     1                BBC(1,1,2,EP)*DC(1,1,EP)*BBC(1,1,4,EP)+
     2                BBC(2,1,2,EP)*DC(2,2,EP)*BBC(2,1,4,EP)
          K33(3,3,EP)=K33(3,3,EP)+
     1                BBC(1,1,3,EP)*DC(1,1,EP)*BBC(1,1,3,EP)+
     2                BBC(2,1,3,EP)*DC(2,2,EP)*BBC(2,1,3,EP)
          K34(3,3,EP)=K34(3,3,EP)+
     1                BBC(1,1,3,EP)*DC(1,1,EP)*BBC(1,1,4,EP)+
     2                BBC(2,1,3,EP)*DC(2,2,EP)*BBC(2,1,4,EP)
          K44(3,3,EP)=K44(3,3,EP)+
     1                BBC(1,1,4,EP)*DC(1,1,EP)*BBC(1,1,4,EP)+
     2                BBC(2,1,4,EP)*DC(2,2,EP)*BBC(2,1,4,EP)
        ENDDO
       DO J=1,2
        J1 =J+1
        DO EP=JFT,NPLAT
          MF11(3,J,EP)=MF11(3,J,EP)+
     1                BBC(1,1,1,EP)*DC(1,1,EP)*BBC(1,J1,1,EP)+
     2                BBC(2,1,1,EP)*DC(2,2,EP)*BBC(2,J1,1,EP)
          MF22(3,J,EP)=MF22(3,J,EP)+
     1                BBC(1,1,2,EP)*DC(1,1,EP)*BBC(1,J1,2,EP)+
     2                BBC(2,1,2,EP)*DC(2,2,EP)*BBC(2,J1,2,EP)
          MF33(3,J,EP)=MF33(3,J,EP)+
     1                BBC(1,1,3,EP)*DC(1,1,EP)*BBC(1,J1,3,EP)+
     2                BBC(2,1,3,EP)*DC(2,2,EP)*BBC(2,J1,3,EP)
          MF44(3,J,EP)=MF44(3,J,EP)+
     1                BBC(1,1,4,EP)*DC(1,1,EP)*BBC(1,J1,4,EP)+
     2                BBC(2,1,4,EP)*DC(2,2,EP)*BBC(2,J1,4,EP)
          MF12(3,J,EP)=MF12(3,J,EP)+
     1                BBC(1,1,1,EP)*DC(1,1,EP)*BBC(1,J1,2,EP)+
     2                BBC(2,1,1,EP)*DC(2,2,EP)*BBC(2,J1,2,EP)
          MF13(3,J,EP)=MF13(3,J,EP)+
     1                BBC(1,1,1,EP)*DC(1,1,EP)*BBC(1,J1,3,EP)+
     2                BBC(2,1,1,EP)*DC(2,2,EP)*BBC(2,J1,3,EP)
          MF14(3,J,EP)=MF14(3,J,EP)+
     1                BBC(1,1,1,EP)*DC(1,1,EP)*BBC(1,J1,4,EP)+
     2                BBC(2,1,1,EP)*DC(2,2,EP)*BBC(2,J1,4,EP)
          MF23(3,J,EP)=MF23(3,J,EP)+
     1                BBC(1,1,2,EP)*DC(1,1,EP)*BBC(1,J1,3,EP)+
     2                BBC(2,1,2,EP)*DC(2,2,EP)*BBC(2,J1,3,EP)
          MF24(3,J,EP)=MF24(3,J,EP)+
     1                BBC(1,1,2,EP)*DC(1,1,EP)*BBC(1,J1,4,EP)+
     2                BBC(2,1,2,EP)*DC(2,2,EP)*BBC(2,J1,4,EP)
          MF34(3,J,EP)=MF34(3,J,EP)+
     1                BBC(1,1,3,EP)*DC(1,1,EP)*BBC(1,J1,4,EP)+
     2                BBC(2,1,3,EP)*DC(2,2,EP)*BBC(2,J1,4,EP)
          FM12(J,3,EP)=FM12(J,3,EP)+
     1                BBC(1,1,2,EP)*DC(1,1,EP)*BBC(1,J1,1,EP)+
     2                BBC(2,1,2,EP)*DC(2,2,EP)*BBC(2,J1,1,EP)
          FM13(J,3,EP)=FM13(J,3,EP)+
     1                BBC(1,1,3,EP)*DC(1,1,EP)*BBC(1,J1,1,EP)+
     2                BBC(2,1,3,EP)*DC(2,2,EP)*BBC(2,J1,1,EP)
          FM14(J,3,EP)=FM14(J,3,EP)+
     1                BBC(1,1,4,EP)*DC(1,1,EP)*BBC(1,J1,1,EP)+
     2                BBC(2,1,4,EP)*DC(2,2,EP)*BBC(2,J1,1,EP)
          FM23(J,3,EP)=FM23(J,3,EP)+
     1                BBC(1,1,3,EP)*DC(1,1,EP)*BBC(1,J1,2,EP)+
     2                BBC(2,1,3,EP)*DC(2,2,EP)*BBC(2,J1,2,EP)
          FM24(J,3,EP)=FM24(J,3,EP)+
     1                BBC(1,1,4,EP)*DC(1,1,EP)*BBC(1,J1,2,EP)+
     2                BBC(2,1,4,EP)*DC(2,2,EP)*BBC(2,J1,2,EP)
          FM34(J,3,EP)=FM34(J,3,EP)+
     1                BBC(1,1,4,EP)*DC(1,1,EP)*BBC(1,J1,3,EP)+
     2                BBC(2,1,4,EP)*DC(2,2,EP)*BBC(2,J1,3,EP)
        ENDDO
       ENDDO
C------ Mzz  pour tous----x:1,3,5,7;y:2,4,6,8---
      IF (IDRIL==0) THEN
        DO EP=JFT,JLT
          M11(3,3,EP)=M11(3,3,EP)+DHZ(EP)
     1                *(BZZ(EP,1)*BZZ(EP,1)+BZZ(EP,2)*BZZ(EP,2))
          M12(3,3,EP)=M12(3,3,EP)+DHZ(EP)
     1                *(BZZ(EP,1)*BZZ(EP,3)+BZZ(EP,2)*BZZ(EP,4))
          M13(3,3,EP)=M13(3,3,EP)+DHZ(EP)
     1                *(BZZ(EP,1)*BZZ(EP,5)+BZZ(EP,2)*BZZ(EP,6))
          M14(3,3,EP)=M14(3,3,EP)+DHZ(EP)
     1                *(BZZ(EP,1)*BZZ(EP,7)+BZZ(EP,2)*BZZ(EP,8))
          M22(3,3,EP)=M22(3,3,EP)+DHZ(EP)
     1                *(BZZ(EP,3)*BZZ(EP,3)+BZZ(EP,4)*BZZ(EP,4))
          M23(3,3,EP)=M23(3,3,EP)+DHZ(EP)
     1                *(BZZ(EP,3)*BZZ(EP,5)+BZZ(EP,4)*BZZ(EP,6))
          M24(3,3,EP)=M24(3,3,EP)+DHZ(EP)
     1                *(BZZ(EP,3)*BZZ(EP,7)+BZZ(EP,4)*BZZ(EP,8))
          M33(3,3,EP)=M33(3,3,EP)+DHZ(EP)
     1                *(BZZ(EP,5)*BZZ(EP,5)+BZZ(EP,6)*BZZ(EP,6))
          M34(3,3,EP)=M34(3,3,EP)+DHZ(EP)
     1                *(BZZ(EP,5)*BZZ(EP,7)+BZZ(EP,6)*BZZ(EP,8))
          M44(3,3,EP)=M44(3,3,EP)+DHZ(EP)
     1                *(BZZ(EP,7)*BZZ(EP,7)+BZZ(EP,8)*BZZ(EP,8))
       ENDDO
C------ renforce positive  ----------
       IF (NEIG==0) THEN
       DO EP=JFT,JLT
        C1 = EM8*M11(3,3,EP)
        C2 = EM8*M22(3,3,EP)
        C3 = EM8*M33(3,3,EP)
        C4 = EM8*M44(3,3,EP)
        M11(3,3,EP)=M11(3,3,EP)+C1
        M22(3,3,EP)=M22(3,3,EP)+C2
        M33(3,3,EP)=M33(3,3,EP)+C3
        M44(3,3,EP)=M44(3,3,EP)+C4
       ENDDO
       ENDIF 
      END IF !(IDRIL==0) THEN
C---+----1----+----2----+warped elements----+----5----+----6----+----7----+----8
#include "vectorize.inc"
       DO M=NF,JLT
        EP=IPLAT(M)
         C2=VOL(EP)
        C1=THK2(EP)*C2
        DC(1,1,M)=(HC(EP,1)*TC(EP,1,1)*TC(EP,1,1)+
     1             HC(EP,2)*TC(EP,1,2)*TC(EP,1,2))*C2
        DC(2,2,M)=(HC(EP,1)*TC(EP,2,1)*TC(EP,2,1)+
     1             HC(EP,2)*TC(EP,2,2)*TC(EP,2,2))*C2
        DC(1,2,M)=(HC(EP,1)*TC(EP,1,1)*TC(EP,2,1)+
     1             HC(EP,2)*TC(EP,2,2)*TC(EP,1,2))*C2
        DC(2,1,M)=DC(1,2,M)
       ENDDO
       DO I=1,3
       DO J=I,3
       DO L=1,2
       DO K=1,2
        DO EP=NF,JLT
          K11(I,J,EP)=K11(I,J,EP)+BM(EP,K,I,1)*DM(K,L,EP)*BM(EP,L,J,1)+
     1                BMF(EP,K,I,1)*DF(K,L,EP)*BMF(EP,L,J,1)+
     2                 BC(EP,K,I,1)*DC(K,L,EP)*BC(EP,L,J,1)
          K22(I,J,EP)=K22(I,J,EP)+BM(EP,K,I,2)*DM(K,L,EP)*BM(EP,L,J,2)+
     1                BMF(EP,K,I,2)*DF(K,L,EP)*BMF(EP,L,J,2)+
     2                 BC(EP,K,I,2)*DC(K,L,EP)*BC(EP,L,J,2)
          K33(I,J,EP)=K33(I,J,EP)+BM(EP,K,I,3)*DM(K,L,EP)*BM(EP,L,J,3)+
     1                BMF(EP,K,I,3)*DF(K,L,EP)*BMF(EP,L,J,3)+
     2                 BC(EP,K,I,3)*DC(K,L,EP)*BC(EP,L,J,3)
          K44(I,J,EP)=K44(I,J,EP)+BM(EP,K,I,4)*DM(K,L,EP)*BM(EP,L,J,4)+
     1                BMF(EP,K,I,4)*DF(K,L,EP)*BMF(EP,L,J,4)+
     2                 BC(EP,K,I,4)*DC(K,L,EP)*BC(EP,L,J,4)
        ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
C
       DO I=1,2
        I1 =I+3
       DO J=I,2
        J1 =J+3
       DO L=1,2
       DO K=1,2
        DO EP=NF,JLT
          M11(I,J,EP)=M11(I,J,EP)+BF(EP,K,I,1)*DF(K,L,EP)*BF(EP,L,J,1)+
     1                           BC(EP,K,I1,1)*DC(K,L,EP)*BC(EP,L,J1,1)
          M22(I,J,EP)=M22(I,J,EP)+BF(EP,K,I,2)*DF(K,L,EP)*BF(EP,L,J,2)+
     1                           BC(EP,K,I1,2)*DC(K,L,EP)*BC(EP,L,J1,2)
          M33(I,J,EP)=M33(I,J,EP)+BF(EP,K,I,3)*DF(K,L,EP)*BF(EP,L,J,3)+
     1                           BC(EP,K,I1,3)*DC(K,L,EP)*BC(EP,L,J1,3)
          M44(I,J,EP)=M44(I,J,EP)+BF(EP,K,I,4)*DF(K,L,EP)*BF(EP,L,J,4)+
     1                           BC(EP,K,I1,4)*DC(K,L,EP)*BC(EP,L,J1,4)
        ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
C---+----1----+----2----+shear contribution-+----5----+----6----+----7----+----8
       DO I=1,3
       DO J=I,3
        DO EP=NF,JLT
          K11(I,J,EP)=K11(I,J,EP)+BMF(EP,3,I,1)*GF(EP)*BMF(EP,3,J,1)
          K22(I,J,EP)=K22(I,J,EP)+BMF(EP,3,I,2)*GF(EP)*BMF(EP,3,J,2)
          K33(I,J,EP)=K33(I,J,EP)+BMF(EP,3,I,3)*GF(EP)*BMF(EP,3,J,3)
          K44(I,J,EP)=K44(I,J,EP)+BMF(EP,3,I,4)*GF(EP)*BMF(EP,3,J,4)
        ENDDO
       ENDDO
       ENDDO
       DO I=1,2
       DO J=I,2
        DO EP=NF,JLT
          M11(I,J,EP)=M11(I,J,EP)+BF(EP,3,I,1)*GF(EP)*BF(EP,3,J,1)
          M22(I,J,EP)=M22(I,J,EP)+BF(EP,3,I,2)*GF(EP)*BF(EP,3,J,2)
          M33(I,J,EP)=M33(I,J,EP)+BF(EP,3,I,3)*GF(EP)*BF(EP,3,J,3)
          M44(I,J,EP)=M44(I,J,EP)+BF(EP,3,I,4)*GF(EP)*BF(EP,3,J,4)
        ENDDO
       ENDDO
       ENDDO
C
       DO I=1,3
       DO J=1,3
       DO L=1,2
       DO K=1,2
        DO EP=NF,JLT
          K12(I,J,EP)=K12(I,J,EP)+BM(EP,K,I,1)*DM(K,L,EP)*BM(EP,L,J,2)+
     1                BMF(EP,K,I,1)*DF(K,L,EP)*BMF(EP,L,J,2)+
     1                 BC(EP,K,I,1)*DC(K,L,EP)*BC(EP,L,J,2)
          K13(I,J,EP)=K13(I,J,EP)+BM(EP,K,I,1)*DM(K,L,EP)*BM(EP,L,J,3)+
     1                BMF(EP,K,I,1)*DF(K,L,EP)*BMF(EP,L,J,3)+
     1                 BC(EP,K,I,1)*DC(K,L,EP)*BC(EP,L,J,3)
          K14(I,J,EP)=K14(I,J,EP)+BM(EP,K,I,1)*DM(K,L,EP)*BM(EP,L,J,4)+
     1                BMF(EP,K,I,1)*DF(K,L,EP)*BMF(EP,L,J,4)+
     1                 BC(EP,K,I,1)*DC(K,L,EP)*BC(EP,L,J,4)
          K23(I,J,EP)=K23(I,J,EP)+BM(EP,K,I,2)*DM(K,L,EP)*BM(EP,L,J,3)+
     1                BMF(EP,K,I,2)*DF(K,L,EP)*BMF(EP,L,J,3)+
     1                 BC(EP,K,I,2)*DC(K,L,EP)*BC(EP,L,J,3)
          K24(I,J,EP)=K24(I,J,EP)+BM(EP,K,I,2)*DM(K,L,EP)*BM(EP,L,J,4)+
     1                BMF(EP,K,I,2)*DF(K,L,EP)*BMF(EP,L,J,4)+
     1                 BC(EP,K,I,2)*DC(K,L,EP)*BC(EP,L,J,4)
          K34(I,J,EP)=K34(I,J,EP)+BM(EP,K,I,3)*DM(K,L,EP)*BM(EP,L,J,4)+
     1                BMF(EP,K,I,3)*DF(K,L,EP)*BMF(EP,L,J,4)+
     1                 BC(EP,K,I,3)*DC(K,L,EP)*BC(EP,L,J,4)
        ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
C
       DO I=1,2
        I1 =I+3
       DO J=1,2
        J1 =J+3
       DO L=1,2
       DO K=1,2
        DO EP=NF,JLT
          M12(I,J,EP)=M12(I,J,EP)+BF(EP,K,I,1)*DF(K,L,EP)*BF(EP,L,J,2)+
     1                           BC(EP,K,I1,1)*DC(K,L,EP)*BC(EP,L,J1,2)
          M13(I,J,EP)=M13(I,J,EP)+BF(EP,K,I,1)*DF(K,L,EP)*BF(EP,L,J,3)+
     1                           BC(EP,K,I1,1)*DC(K,L,EP)*BC(EP,L,J1,3)
          M14(I,J,EP)=M14(I,J,EP)+BF(EP,K,I,1)*DF(K,L,EP)*BF(EP,L,J,4)+
     1                           BC(EP,K,I1,1)*DC(K,L,EP)*BC(EP,L,J1,4)
          M23(I,J,EP)=M23(I,J,EP)+BF(EP,K,I,2)*DF(K,L,EP)*BF(EP,L,J,3)+
     1                           BC(EP,K,I1,2)*DC(K,L,EP)*BC(EP,L,J1,3)
          M24(I,J,EP)=M24(I,J,EP)+BF(EP,K,I,2)*DF(K,L,EP)*BF(EP,L,J,4)+
     1                           BC(EP,K,I1,2)*DC(K,L,EP)*BC(EP,L,J1,4)
          M34(I,J,EP)=M34(I,J,EP)+BF(EP,K,I,3)*DF(K,L,EP)*BF(EP,L,J,4)+
     1                           BC(EP,K,I1,3)*DC(K,L,EP)*BC(EP,L,J1,4)
        ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
C---+----1----+----2----+shear contribution-+----5----+----6----+----7----+----8
       DO I=1,3
       DO J=1,3
        DO EP=NF,JLT
          K12(I,J,EP)=K12(I,J,EP)+BMF(EP,3,I,1)*GF(EP)*BMF(EP,3,J,2)
          K13(I,J,EP)=K13(I,J,EP)+BMF(EP,3,I,1)*GF(EP)*BMF(EP,3,J,3)
          K14(I,J,EP)=K14(I,J,EP)+BMF(EP,3,I,1)*GF(EP)*BMF(EP,3,J,4)
          K23(I,J,EP)=K23(I,J,EP)+BMF(EP,3,I,2)*GF(EP)*BMF(EP,3,J,3)
          K24(I,J,EP)=K24(I,J,EP)+BMF(EP,3,I,2)*GF(EP)*BMF(EP,3,J,4)
          K34(I,J,EP)=K34(I,J,EP)+BMF(EP,3,I,3)*GF(EP)*BMF(EP,3,J,4)
        ENDDO
       ENDDO
       ENDDO
       DO I=1,2
       DO J=1,2
        DO EP=NF,JLT
          M12(I,J,EP)=M12(I,J,EP)+BF(EP,3,I,1)*GF(EP)*BF(EP,3,J,2)
          M13(I,J,EP)=M13(I,J,EP)+BF(EP,3,I,1)*GF(EP)*BF(EP,3,J,3)
          M14(I,J,EP)=M14(I,J,EP)+BF(EP,3,I,1)*GF(EP)*BF(EP,3,J,4)
          M23(I,J,EP)=M23(I,J,EP)+BF(EP,3,I,2)*GF(EP)*BF(EP,3,J,3)
          M24(I,J,EP)=M24(I,J,EP)+BF(EP,3,I,2)*GF(EP)*BF(EP,3,J,4)
          M34(I,J,EP)=M34(I,J,EP)+BF(EP,3,I,3)*GF(EP)*BF(EP,3,J,4)
        ENDDO
       ENDDO
       ENDDO
C
C---+----1----+----2----+couplage part--[MF](3x2)[FM](2x3)-6----+----7----+----8
       DO I=1,3
       DO J=1,2
        J1=J+3
       DO L=1,2
       DO K=1,2
        DO EP=NF,JLT
         MF11(I,J,EP)=MF11(I,J,EP)+BMF(EP,K,I,1)*DF(K,L,EP)*BF(EP,L,J,1)
     1                            +BC(EP,K,I,1)*DC(K,L,EP)*BC(EP,L,J1,1)
         MF12(I,J,EP)=MF12(I,J,EP)+BMF(EP,K,I,1)*DF(K,L,EP)*BF(EP,L,J,2)
     1                            +BC(EP,K,I,1)*DC(K,L,EP)*BC(EP,L,J1,2)
         MF13(I,J,EP)=MF13(I,J,EP)+BMF(EP,K,I,1)*DF(K,L,EP)*BF(EP,L,J,3)
     1                            +BC(EP,K,I,1)*DC(K,L,EP)*BC(EP,L,J1,3)
         MF14(I,J,EP)=MF14(I,J,EP)+BMF(EP,K,I,1)*DF(K,L,EP)*BF(EP,L,J,4)
     1                            +BC(EP,K,I,1)*DC(K,L,EP)*BC(EP,L,J1,4)
         MF22(I,J,EP)=MF22(I,J,EP)+BMF(EP,K,I,2)*DF(K,L,EP)*BF(EP,L,J,2)
     1                            +BC(EP,K,I,2)*DC(K,L,EP)*BC(EP,L,J1,2)
         MF23(I,J,EP)=MF23(I,J,EP)+BMF(EP,K,I,2)*DF(K,L,EP)*BF(EP,L,J,3)
     1                            +BC(EP,K,I,2)*DC(K,L,EP)*BC(EP,L,J1,3)
         MF24(I,J,EP)=MF24(I,J,EP)+BMF(EP,K,I,2)*DF(K,L,EP)*BF(EP,L,J,4)
     1                            +BC(EP,K,I,2)*DC(K,L,EP)*BC(EP,L,J1,4)
         MF33(I,J,EP)=MF33(I,J,EP)+BMF(EP,K,I,3)*DF(K,L,EP)*BF(EP,L,J,3)
     1                            +BC(EP,K,I,3)*DC(K,L,EP)*BC(EP,L,J1,3)
         MF34(I,J,EP)=MF34(I,J,EP)+BMF(EP,K,I,3)*DF(K,L,EP)*BF(EP,L,J,4)
     1                            +BC(EP,K,I,3)*DC(K,L,EP)*BC(EP,L,J1,4)
         MF44(I,J,EP)=MF44(I,J,EP)+BMF(EP,K,I,4)*DF(K,L,EP)*BF(EP,L,J,4)
     1                            +BC(EP,K,I,4)*DC(K,L,EP)*BC(EP,L,J1,4)
C
         FM12(J,I,EP)=FM12(J,I,EP)+BMF(EP,K,I,2)*DF(K,L,EP)*BF(EP,L,J,1)
     1                            +BC(EP,K,I,2)*DC(K,L,EP)*BC(EP,L,J1,1)
         FM13(J,I,EP)=FM13(J,I,EP)+BMF(EP,K,I,3)*DF(K,L,EP)*BF(EP,L,J,1)
     1                            +BC(EP,K,I,3)*DC(K,L,EP)*BC(EP,L,J1,1)
         FM14(J,I,EP)=FM14(J,I,EP)+BMF(EP,K,I,4)*DF(K,L,EP)*BF(EP,L,J,1)
     1                            +BC(EP,K,I,4)*DC(K,L,EP)*BC(EP,L,J1,1)
         FM23(J,I,EP)=FM23(J,I,EP)+BMF(EP,K,I,3)*DF(K,L,EP)*BF(EP,L,J,2)
     1                            +BC(EP,K,I,3)*DC(K,L,EP)*BC(EP,L,J1,2)
         FM24(J,I,EP)=FM24(J,I,EP)+BMF(EP,K,I,4)*DF(K,L,EP)*BF(EP,L,J,2)
     1                            +BC(EP,K,I,4)*DC(K,L,EP)*BC(EP,L,J1,2)
         FM34(J,I,EP)=FM34(J,I,EP)+BMF(EP,K,I,4)*DF(K,L,EP)*BF(EP,L,J,3)
     1                            +BC(EP,K,I,4)*DC(K,L,EP)*BC(EP,L,J1,3)
        ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
C
       DO I=1,3
       DO J=1,2
        DO EP=NF,JLT
         MF11(I,J,EP)=MF11(I,J,EP)+BMF(EP,3,I,1)*GF(EP)*BF(EP,3,J,1)
         MF12(I,J,EP)=MF12(I,J,EP)+BMF(EP,3,I,1)*GF(EP)*BF(EP,3,J,2)
         MF13(I,J,EP)=MF13(I,J,EP)+BMF(EP,3,I,1)*GF(EP)*BF(EP,3,J,3)
         MF14(I,J,EP)=MF14(I,J,EP)+BMF(EP,3,I,1)*GF(EP)*BF(EP,3,J,4)
         MF22(I,J,EP)=MF22(I,J,EP)+BMF(EP,3,I,2)*GF(EP)*BF(EP,3,J,2)
         MF23(I,J,EP)=MF23(I,J,EP)+BMF(EP,3,I,2)*GF(EP)*BF(EP,3,J,3)
         MF24(I,J,EP)=MF24(I,J,EP)+BMF(EP,3,I,2)*GF(EP)*BF(EP,3,J,4)
         MF33(I,J,EP)=MF33(I,J,EP)+BMF(EP,3,I,3)*GF(EP)*BF(EP,3,J,3)
         MF34(I,J,EP)=MF34(I,J,EP)+BMF(EP,3,I,3)*GF(EP)*BF(EP,3,J,4)
         MF44(I,J,EP)=MF44(I,J,EP)+BMF(EP,3,I,4)*GF(EP)*BF(EP,3,J,4)
C
         FM12(J,I,EP)=FM12(J,I,EP)+BMF(EP,3,I,2)*GF(EP)*BF(EP,3,J,1)
         FM13(J,I,EP)=FM13(J,I,EP)+BMF(EP,3,I,3)*GF(EP)*BF(EP,3,J,1)
         FM14(J,I,EP)=FM14(J,I,EP)+BMF(EP,3,I,4)*GF(EP)*BF(EP,3,J,1)
         FM23(J,I,EP)=FM23(J,I,EP)+BMF(EP,3,I,3)*GF(EP)*BF(EP,3,J,2)
         FM24(J,I,EP)=FM24(J,I,EP)+BMF(EP,3,I,4)*GF(EP)*BF(EP,3,J,2)
         FM34(J,I,EP)=FM34(J,I,EP)+BMF(EP,3,I,4)*GF(EP)*BF(EP,3,J,3)
        ENDDO
       ENDDO
       ENDDO
       IF (IORTH==1) THEN
        L=3
        K=1
        DO I=1,3
        DO J=I,3
         DO EP=NF,JLT
          K11(I,J,EP)=K11(I,J,EP)+BM(EP,K,I,1)*DM5(EP)*BM(EP,L,J,1)+
     1                            BM(EP,L,I,1)*DM5(EP)*BM(EP,K,J,1)+
     1                            BMF(EP,K,I,1)*DF5(EP)*BMF(EP,L,J,1)+
     1                            BMF(EP,L,I,1)*DF5(EP)*BMF(EP,K,J,1)
          K22(I,J,EP)=K22(I,J,EP)+BM(EP,K,I,2)*DM5(EP)*BM(EP,L,J,2)+
     1                            BM(EP,L,I,2)*DM5(EP)*BM(EP,K,J,2)+
     1                            BMF(EP,K,I,2)*DF5(EP)*BMF(EP,L,J,2)+
     1                            BMF(EP,L,I,2)*DF5(EP)*BMF(EP,K,J,2)
          K33(I,J,EP)=K33(I,J,EP)+BM(EP,K,I,3)*DM5(EP)*BM(EP,L,J,3)+
     1                            BM(EP,L,I,3)*DM5(EP)*BM(EP,K,J,3)+
     1                            BMF(EP,K,I,3)*DF5(EP)*BMF(EP,L,J,3)+
     1                            BMF(EP,L,I,3)*DF5(EP)*BMF(EP,K,J,3)
          K44(I,J,EP)=K44(I,J,EP)+BM(EP,K,I,4)*DM5(EP)*BM(EP,L,J,4)+
     1                            BM(EP,L,I,4)*DM5(EP)*BM(EP,K,J,4)+
     1                            BMF(EP,K,I,4)*DF5(EP)*BMF(EP,L,J,4)+
     1                            BMF(EP,L,I,4)*DF5(EP)*BMF(EP,K,J,4)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3
        DO J=1,3
         DO EP=NF,JLT
          K12(I,J,EP)=K12(I,J,EP)+BM(EP,K,I,1)*DM5(EP)*BM(EP,L,J,2)+
     1                            BM(EP,L,I,1)*DM5(EP)*BM(EP,K,J,2)+
     1                            BMF(EP,K,I,1)*DF5(EP)*BMF(EP,L,J,2)+
     1                            BMF(EP,L,I,1)*DF5(EP)*BMF(EP,K,J,2)
          K13(I,J,EP)=K13(I,J,EP)+BM(EP,K,I,1)*DM5(EP)*BM(EP,L,J,3)+
     1                            BM(EP,L,I,1)*DM5(EP)*BM(EP,K,J,3)+
     1                            BMF(EP,K,I,1)*DF5(EP)*BMF(EP,L,J,3)+
     1                            BMF(EP,L,I,1)*DF5(EP)*BMF(EP,K,J,3)
          K14(I,J,EP)=K14(I,J,EP)+BM(EP,K,I,1)*DM5(EP)*BM(EP,L,J,4)+
     1                            BM(EP,L,I,1)*DM5(EP)*BM(EP,K,J,4)+
     1                            BMF(EP,K,I,1)*DF5(EP)*BMF(EP,L,J,4)+
     1                            BMF(EP,L,I,1)*DF5(EP)*BMF(EP,K,J,4)
          K23(I,J,EP)=K23(I,J,EP)+BM(EP,K,I,2)*DM5(EP)*BM(EP,L,J,3)+
     1                            BM(EP,L,I,2)*DM5(EP)*BM(EP,K,J,3)+
     1                            BMF(EP,K,I,2)*DF5(EP)*BMF(EP,L,J,3)+
     1                            BMF(EP,L,I,2)*DF5(EP)*BMF(EP,K,J,3)
          K24(I,J,EP)=K24(I,J,EP)+BM(EP,K,I,2)*DM5(EP)*BM(EP,L,J,4)+
     1                            BM(EP,L,I,2)*DM5(EP)*BM(EP,K,J,4)+
     1                            BMF(EP,K,I,2)*DF5(EP)*BMF(EP,L,J,4)+
     1                            BMF(EP,L,I,2)*DF5(EP)*BMF(EP,K,J,4)
          K34(I,J,EP)=K34(I,J,EP)+BM(EP,K,I,3)*DM5(EP)*BM(EP,L,J,4)+
     1                            BM(EP,L,I,3)*DM5(EP)*BM(EP,K,J,4)+
     1                            BMF(EP,K,I,3)*DF5(EP)*BMF(EP,L,J,4)+
     1                            BMF(EP,L,I,3)*DF5(EP)*BMF(EP,K,J,4)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,2
        DO J=I,2
         DO EP=NF,JLT
          M11(I,J,EP)=M11(I,J,EP)+BF(EP,K,I,1)*DF5(EP)*BF(EP,L,J,1)
     1                           +BF(EP,L,I,1)*DF5(EP)*BF(EP,K,J,1)
          M22(I,J,EP)=M22(I,J,EP)+BF(EP,K,I,2)*DF5(EP)*BF(EP,L,J,2)
     1                           +BF(EP,L,I,2)*DF5(EP)*BF(EP,K,J,2)
          M33(I,J,EP)=M33(I,J,EP)+BF(EP,K,I,3)*DF5(EP)*BF(EP,L,J,3)
     1                           +BF(EP,L,I,3)*DF5(EP)*BF(EP,K,J,3)
          M44(I,J,EP)=M44(I,J,EP)+BF(EP,K,I,4)*DF5(EP)*BF(EP,L,J,4)
     1                           +BF(EP,L,I,4)*DF5(EP)*BF(EP,K,J,4)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,2
        DO J=1,2
         DO EP=NF,JLT
          M12(I,J,EP)=M12(I,J,EP)+BF(EP,K,I,1)*DF5(EP)*BF(EP,L,J,2)
     1                           +BF(EP,L,I,1)*DF5(EP)*BF(EP,K,J,2)
          M13(I,J,EP)=M13(I,J,EP)+BF(EP,K,I,1)*DF5(EP)*BF(EP,L,J,3)
     1                           +BF(EP,L,I,1)*DF5(EP)*BF(EP,K,J,3)
          M14(I,J,EP)=M14(I,J,EP)+BF(EP,K,I,1)*DF5(EP)*BF(EP,L,J,4)
     1                           +BF(EP,L,I,1)*DF5(EP)*BF(EP,K,J,4)
          M23(I,J,EP)=M23(I,J,EP)+BF(EP,K,I,2)*DF5(EP)*BF(EP,L,J,3)
     1                           +BF(EP,L,I,2)*DF5(EP)*BF(EP,K,J,3)
          M24(I,J,EP)=M24(I,J,EP)+BF(EP,K,I,2)*DF5(EP)*BF(EP,L,J,4)
     1                           +BF(EP,L,I,2)*DF5(EP)*BF(EP,K,J,4)
          M34(I,J,EP)=M34(I,J,EP)+BF(EP,K,I,3)*DF5(EP)*BF(EP,L,J,4)
     1                           +BF(EP,L,I,3)*DF5(EP)*BF(EP,K,J,4)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3
        DO J=1,2
         DO EP=NF,JLT
          MF11(I,J,EP)=MF11(I,J,EP)+BMF(EP,K,I,1)*DF5(EP)*BF(EP,L,J,1)
     1                             +BMF(EP,L,I,1)*DF5(EP)*BF(EP,K,J,1)
          MF12(I,J,EP)=MF12(I,J,EP)+BMF(EP,K,I,1)*DF5(EP)*BF(EP,L,J,2)
     1                             +BMF(EP,L,I,1)*DF5(EP)*BF(EP,K,J,2)
          MF13(I,J,EP)=MF13(I,J,EP)+BMF(EP,K,I,1)*DF5(EP)*BF(EP,L,J,3)
     1                             +BMF(EP,L,I,1)*DF5(EP)*BF(EP,K,J,3)
          MF14(I,J,EP)=MF14(I,J,EP)+BMF(EP,K,I,1)*DF5(EP)*BF(EP,L,J,4)
     1                             +BMF(EP,L,I,1)*DF5(EP)*BF(EP,K,J,4)
          MF22(I,J,EP)=MF22(I,J,EP)+BMF(EP,K,I,2)*DF5(EP)*BF(EP,L,J,2)
     1                             +BMF(EP,L,I,2)*DF5(EP)*BF(EP,K,J,2)
          MF23(I,J,EP)=MF23(I,J,EP)+BMF(EP,K,I,2)*DF5(EP)*BF(EP,L,J,3)
     1                             +BMF(EP,L,I,2)*DF5(EP)*BF(EP,K,J,3)
          MF24(I,J,EP)=MF24(I,J,EP)+BMF(EP,K,I,2)*DF5(EP)*BF(EP,L,J,4)
     1                             +BMF(EP,L,I,2)*DF5(EP)*BF(EP,K,J,4)
          MF33(I,J,EP)=MF33(I,J,EP)+BMF(EP,K,I,3)*DF5(EP)*BF(EP,L,J,3)
     1                             +BMF(EP,L,I,3)*DF5(EP)*BF(EP,K,J,3)
          MF34(I,J,EP)=MF34(I,J,EP)+BMF(EP,K,I,3)*DF5(EP)*BF(EP,L,J,4)
     1                             +BMF(EP,L,I,3)*DF5(EP)*BF(EP,K,J,4)
          MF44(I,J,EP)=MF44(I,J,EP)+BMF(EP,K,I,4)*DF5(EP)*BF(EP,L,J,4)
     1                             +BMF(EP,L,I,4)*DF5(EP)*BF(EP,K,J,4)
C
          FM12(J,I,EP)=FM12(J,I,EP)+BMF(EP,K,I,2)*DF5(EP)*BF(EP,L,J,1)
     1                             +BMF(EP,L,I,2)*DF5(EP)*BF(EP,K,J,1)
          FM13(J,I,EP)=FM13(J,I,EP)+BMF(EP,K,I,3)*DF5(EP)*BF(EP,L,J,1)
     1                             +BMF(EP,L,I,3)*DF5(EP)*BF(EP,K,J,1)
          FM14(J,I,EP)=FM14(J,I,EP)+BMF(EP,K,I,4)*DF5(EP)*BF(EP,L,J,1)
     1                             +BMF(EP,L,I,4)*DF5(EP)*BF(EP,K,J,1)
          FM23(J,I,EP)=FM23(J,I,EP)+BMF(EP,K,I,3)*DF5(EP)*BF(EP,L,J,2)
     1                             +BMF(EP,L,I,3)*DF5(EP)*BF(EP,K,J,2)
          FM24(J,I,EP)=FM24(J,I,EP)+BMF(EP,K,I,4)*DF5(EP)*BF(EP,L,J,2)
     1                             +BMF(EP,L,I,4)*DF5(EP)*BF(EP,K,J,2)
          FM34(J,I,EP)=FM34(J,I,EP)+BMF(EP,K,I,4)*DF5(EP)*BF(EP,L,J,3)
     1                             +BMF(EP,L,I,4)*DF5(EP)*BF(EP,K,J,3)
         ENDDO
        ENDDO
        ENDDO
C
        K=2
        DO I=1,3
        DO J=I,3
         DO EP=NF,JLT
          K11(I,J,EP)=K11(I,J,EP)+BM(EP,K,I,1)*DM6(EP)*BM(EP,L,J,1)+
     1                            BM(EP,L,I,1)*DM6(EP)*BM(EP,K,J,1)+
     1                            BMF(EP,K,I,1)*DF6(EP)*BMF(EP,L,J,1)+
     1                            BMF(EP,L,I,1)*DF6(EP)*BMF(EP,K,J,1)
          K22(I,J,EP)=K22(I,J,EP)+BM(EP,K,I,2)*DM6(EP)*BM(EP,L,J,2)+
     1                            BM(EP,L,I,2)*DM6(EP)*BM(EP,K,J,2)+
     1                            BMF(EP,K,I,2)*DF6(EP)*BMF(EP,L,J,2)+
     1                            BMF(EP,L,I,2)*DF6(EP)*BMF(EP,K,J,2)
          K33(I,J,EP)=K33(I,J,EP)+BM(EP,K,I,3)*DM6(EP)*BM(EP,L,J,3)+
     1                            BM(EP,L,I,3)*DM6(EP)*BM(EP,K,J,3)+
     1                            BMF(EP,K,I,3)*DF6(EP)*BMF(EP,L,J,3)+
     1                            BMF(EP,L,I,3)*DF6(EP)*BMF(EP,K,J,3)
          K44(I,J,EP)=K44(I,J,EP)+BM(EP,K,I,4)*DM6(EP)*BM(EP,L,J,4)+
     1                            BM(EP,L,I,4)*DM6(EP)*BM(EP,K,J,4)+
     1                            BMF(EP,K,I,4)*DF6(EP)*BMF(EP,L,J,4)+
     1                            BMF(EP,L,I,4)*DF6(EP)*BMF(EP,K,J,4)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3
        DO J=1,3
         DO EP=NF,JLT
          K12(I,J,EP)=K12(I,J,EP)+BM(EP,K,I,1)*DM6(EP)*BM(EP,L,J,2)+
     1                            BM(EP,L,I,1)*DM6(EP)*BM(EP,K,J,2)+
     1                            BMF(EP,K,I,1)*DF6(EP)*BMF(EP,L,J,2)+
     1                            BMF(EP,L,I,1)*DF6(EP)*BMF(EP,K,J,2)
          K13(I,J,EP)=K13(I,J,EP)+BM(EP,K,I,1)*DM6(EP)*BM(EP,L,J,3)+
     1                            BM(EP,L,I,1)*DM6(EP)*BM(EP,K,J,3)+
     1                            BMF(EP,K,I,1)*DF6(EP)*BMF(EP,L,J,3)+
     1                            BMF(EP,L,I,1)*DF6(EP)*BMF(EP,K,J,3)
          K14(I,J,EP)=K14(I,J,EP)+BM(EP,K,I,1)*DM6(EP)*BM(EP,L,J,4)+
     1                            BM(EP,L,I,1)*DM6(EP)*BM(EP,K,J,4)+
     1                            BMF(EP,K,I,1)*DF6(EP)*BMF(EP,L,J,4)+
     1                            BMF(EP,L,I,1)*DF6(EP)*BMF(EP,K,J,4)
          K23(I,J,EP)=K23(I,J,EP)+BM(EP,K,I,2)*DM6(EP)*BM(EP,L,J,3)+
     1                            BM(EP,L,I,2)*DM6(EP)*BM(EP,K,J,3)+
     1                            BMF(EP,K,I,2)*DF6(EP)*BMF(EP,L,J,3)+
     1                            BMF(EP,L,I,2)*DF6(EP)*BMF(EP,K,J,3)
          K24(I,J,EP)=K24(I,J,EP)+BM(EP,K,I,2)*DM6(EP)*BM(EP,L,J,4)+
     1                            BM(EP,L,I,2)*DM6(EP)*BM(EP,K,J,4)+
     1                            BMF(EP,K,I,2)*DF6(EP)*BMF(EP,L,J,4)+
     1                            BMF(EP,L,I,2)*DF6(EP)*BMF(EP,K,J,4)
          K34(I,J,EP)=K34(I,J,EP)+BM(EP,K,I,3)*DM6(EP)*BM(EP,L,J,4)+
     1                            BM(EP,L,I,3)*DM6(EP)*BM(EP,K,J,4)+
     1                            BMF(EP,K,I,3)*DF6(EP)*BMF(EP,L,J,4)+
     1                            BMF(EP,L,I,3)*DF6(EP)*BMF(EP,K,J,4)
         ENDDO
        ENDDO
        ENDDO
C	
        DO I=1,2
        DO J=I,2
         DO EP=NF,JLT
          M11(I,J,EP)=M11(I,J,EP)+BF(EP,K,I,1)*DF6(EP)*BF(EP,L,J,1)
     1                           +BF(EP,L,I,1)*DF6(EP)*BF(EP,K,J,1)
          M22(I,J,EP)=M22(I,J,EP)+BF(EP,K,I,2)*DF6(EP)*BF(EP,L,J,2)
     1                           +BF(EP,L,I,2)*DF6(EP)*BF(EP,K,J,2)
          M33(I,J,EP)=M33(I,J,EP)+BF(EP,K,I,3)*DF6(EP)*BF(EP,L,J,3)
     1                           +BF(EP,L,I,3)*DF6(EP)*BF(EP,K,J,3)
          M44(I,J,EP)=M44(I,J,EP)+BF(EP,K,I,4)*DF6(EP)*BF(EP,L,J,4)
     1                           +BF(EP,L,I,4)*DF6(EP)*BF(EP,K,J,4)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,2
        DO J=1,2
         DO EP=NF,JLT
          M12(I,J,EP)=M12(I,J,EP)+BF(EP,K,I,1)*DF6(EP)*BF(EP,L,J,2)
     1                           +BF(EP,L,I,1)*DF6(EP)*BF(EP,K,J,2)
          M13(I,J,EP)=M13(I,J,EP)+BF(EP,K,I,1)*DF6(EP)*BF(EP,L,J,3)
     1                           +BF(EP,L,I,1)*DF6(EP)*BF(EP,K,J,3)
          M14(I,J,EP)=M14(I,J,EP)+BF(EP,K,I,1)*DF6(EP)*BF(EP,L,J,4)
     1                           +BF(EP,L,I,1)*DF6(EP)*BF(EP,K,J,4)
          M23(I,J,EP)=M23(I,J,EP)+BF(EP,K,I,2)*DF6(EP)*BF(EP,L,J,3)
     1                           +BF(EP,L,I,2)*DF6(EP)*BF(EP,K,J,3)
          M24(I,J,EP)=M24(I,J,EP)+BF(EP,K,I,2)*DF6(EP)*BF(EP,L,J,4)
     1                           +BF(EP,L,I,2)*DF6(EP)*BF(EP,K,J,4)
          M34(I,J,EP)=M34(I,J,EP)+BF(EP,K,I,3)*DF6(EP)*BF(EP,L,J,4)
     1                           +BF(EP,L,I,3)*DF6(EP)*BF(EP,K,J,4)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3
        DO J=1,2
         DO EP=NF,JLT
          MF11(I,J,EP)=MF11(I,J,EP)+BMF(EP,K,I,1)*DF6(EP)*BF(EP,L,J,1)
     1                             +BMF(EP,L,I,1)*DF6(EP)*BF(EP,K,J,1)
          MF12(I,J,EP)=MF12(I,J,EP)+BMF(EP,K,I,1)*DF6(EP)*BF(EP,L,J,2)
     1                             +BMF(EP,L,I,1)*DF6(EP)*BF(EP,K,J,2)
          MF13(I,J,EP)=MF13(I,J,EP)+BMF(EP,K,I,1)*DF6(EP)*BF(EP,L,J,3)
     1                             +BMF(EP,L,I,1)*DF6(EP)*BF(EP,K,J,3)
          MF14(I,J,EP)=MF14(I,J,EP)+BMF(EP,K,I,1)*DF6(EP)*BF(EP,L,J,4)
     1                             +BMF(EP,L,I,1)*DF6(EP)*BF(EP,K,J,4)
          MF22(I,J,EP)=MF22(I,J,EP)+BMF(EP,K,I,2)*DF6(EP)*BF(EP,L,J,2)
     1                             +BMF(EP,L,I,2)*DF6(EP)*BF(EP,K,J,2)
          MF23(I,J,EP)=MF23(I,J,EP)+BMF(EP,K,I,2)*DF6(EP)*BF(EP,L,J,3)
     1                             +BMF(EP,L,I,2)*DF6(EP)*BF(EP,K,J,3)
          MF24(I,J,EP)=MF24(I,J,EP)+BMF(EP,K,I,2)*DF6(EP)*BF(EP,L,J,4)
     1                             +BMF(EP,L,I,2)*DF6(EP)*BF(EP,K,J,4)
          MF33(I,J,EP)=MF33(I,J,EP)+BMF(EP,K,I,3)*DF6(EP)*BF(EP,L,J,3)
     1                             +BMF(EP,L,I,3)*DF6(EP)*BF(EP,K,J,3)
          MF34(I,J,EP)=MF34(I,J,EP)+BMF(EP,K,I,3)*DF6(EP)*BF(EP,L,J,4)
     1                             +BMF(EP,L,I,3)*DF6(EP)*BF(EP,K,J,4)
          MF44(I,J,EP)=MF44(I,J,EP)+BMF(EP,K,I,4)*DF6(EP)*BF(EP,L,J,4)
     1                             +BMF(EP,L,I,4)*DF6(EP)*BF(EP,K,J,4)
C
          FM12(J,I,EP)=FM12(J,I,EP)+BMF(EP,K,I,2)*DF6(EP)*BF(EP,L,J,1)
     1                             +BMF(EP,L,I,2)*DF6(EP)*BF(EP,K,J,1)
          FM13(J,I,EP)=FM13(J,I,EP)+BMF(EP,K,I,3)*DF6(EP)*BF(EP,L,J,1)
     1                             +BMF(EP,L,I,3)*DF6(EP)*BF(EP,K,J,1)
          FM14(J,I,EP)=FM14(J,I,EP)+BMF(EP,K,I,4)*DF6(EP)*BF(EP,L,J,1)
     1                             +BMF(EP,L,I,4)*DF6(EP)*BF(EP,K,J,1)
          FM23(J,I,EP)=FM23(J,I,EP)+BMF(EP,K,I,3)*DF6(EP)*BF(EP,L,J,2)
     1                             +BMF(EP,L,I,3)*DF6(EP)*BF(EP,K,J,2)
          FM24(J,I,EP)=FM24(J,I,EP)+BMF(EP,K,I,4)*DF6(EP)*BF(EP,L,J,2)
     1                             +BMF(EP,L,I,4)*DF6(EP)*BF(EP,K,J,2)
          FM34(J,I,EP)=FM34(J,I,EP)+BMF(EP,K,I,4)*DF6(EP)*BF(EP,L,J,3)
     1                             +BMF(EP,L,I,4)*DF6(EP)*BF(EP,K,J,3)
         ENDDO
        ENDDO
        ENDDO
C--add mem-bending coupling terms of orthotrope--warped elements
       DO I=1,3
       DO J=I,3
       DO L=1,3
       DO K=1,3
        DO EP=NF,JLT
          K11(I,J,EP)=K11(I,J,EP)+
     1                BM(EP,K,I,1)*DMF(K,L,EP)*BMF(EP,L,J,1)+
     2                BMF(EP,K,I,1)*DMF(K,L,EP)*BM(EP,L,J,1)
          K22(I,J,EP)=K22(I,J,EP)+
     1                BM(EP,K,I,2)*DMF(K,L,EP)*BMF(EP,L,J,2)+
     2                BMF(EP,K,I,2)*DMF(K,L,EP)*BM(EP,L,J,2)
          K33(I,J,EP)=K33(I,J,EP)+
     1                BM(EP,K,I,3)*DMF(K,L,EP)*BMF(EP,L,J,3)+
     2                BMF(EP,K,I,3)*DMF(K,L,EP)*BM(EP,L,J,3)
          K44(I,J,EP)=K44(I,J,EP)+
     1                BM(EP,K,I,4)*DMF(K,L,EP)*BMF(EP,L,J,4)+
     2                BMF(EP,K,I,4)*DMF(K,L,EP)*BM(EP,L,J,4)
        ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
C
       DO I=1,3
       DO J=1,3
       DO L=1,3
       DO K=1,3
        DO EP=NF,JLT
          K12(I,J,EP)=K12(I,J,EP)+
     1                BM(EP,K,I,1)*DMF(K,L,EP)*BMF(EP,L,J,2)+
     2                BMF(EP,K,I,1)*DMF(K,L,EP)*BM(EP,L,J,2)
          K13(I,J,EP)=K13(I,J,EP)+
     1                BM(EP,K,I,1)*DMF(K,L,EP)*BMF(EP,L,J,3)+
     2                BMF(EP,K,I,1)*DMF(K,L,EP)*BM(EP,L,J,3)
          K14(I,J,EP)=K14(I,J,EP)+
     1                BM(EP,K,I,1)*DMF(K,L,EP)*BMF(EP,L,J,4)+
     2                BMF(EP,K,I,1)*DMF(K,L,EP)*BM(EP,L,J,4)
          K23(I,J,EP)=K23(I,J,EP)+
     1                BM(EP,K,I,2)*DMF(K,L,EP)*BMF(EP,L,J,3)+
     2                BMF(EP,K,I,2)*DMF(K,L,EP)*BM(EP,L,J,3)
          K24(I,J,EP)=K24(I,J,EP)+
     1                BM(EP,K,I,2)*DMF(K,L,EP)*BMF(EP,L,J,4)+
     2                BMF(EP,K,I,2)*DMF(K,L,EP)*BM(EP,L,J,4)
          K34(I,J,EP)=K34(I,J,EP)+
     1                BM(EP,K,I,3)*DMF(K,L,EP)*BMF(EP,L,J,4)+
     2                BMF(EP,K,I,3)*DMF(K,L,EP)*BM(EP,L,J,4)
        ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
C--------------------------
       DO I=1,3
       DO J=1,2
       DO L=1,3
       DO K=1,3
        DO EP=NF,JLT
          MF11(I,J,EP)=MF11(I,J,EP)+
     1                 BM(EP,K,I,1)*DMF(K,L,EP)*BF(EP,L,J,1)
          MF12(I,J,EP)=MF12(I,J,EP)+
     1                 BM(EP,K,I,1)*DMF(K,L,EP)*BF(EP,L,J,2)
          MF13(I,J,EP)=MF13(I,J,EP)+
     1                 BM(EP,K,I,1)*DMF(K,L,EP)*BF(EP,L,J,3)
          MF14(I,J,EP)=MF14(I,J,EP)+
     1                 BM(EP,K,I,1)*DMF(K,L,EP)*BF(EP,L,J,4)
          MF22(I,J,EP)=MF22(I,J,EP)+
     1                 BM(EP,K,I,2)*DMF(K,L,EP)*BF(EP,L,J,2)
          MF23(I,J,EP)=MF23(I,J,EP)+
     1                 BM(EP,K,I,2)*DMF(K,L,EP)*BF(EP,L,J,3)
          MF24(I,J,EP)=MF24(I,J,EP)+
     1                 BM(EP,K,I,2)*DMF(K,L,EP)*BF(EP,L,J,4)
          MF33(I,J,EP)=MF33(I,J,EP)+
     1                 BM(EP,K,I,3)*DMF(K,L,EP)*BF(EP,L,J,3)
          MF34(I,J,EP)=MF34(I,J,EP)+
     1                 BM(EP,K,I,3)*DMF(K,L,EP)*BF(EP,L,J,4)
          MF44(I,J,EP)=MF44(I,J,EP)+
     1                 BM(EP,K,I,4)*DMF(K,L,EP)*BF(EP,L,J,4)
        ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
C       
       DO I=1,2
       DO J=1,3
       DO L=1,3
       DO K=1,3
        DO EP=NF,JLT
          FM12(I,J,EP)=FM12(I,J,EP)+
     1                 BF(EP,K,I,1)*DMF(K,L,EP)*BM(EP,L,J,2)
          FM13(I,J,EP)=FM13(I,J,EP)+
     1                 BF(EP,K,I,1)*DMF(K,L,EP)*BM(EP,L,J,3)
          FM14(I,J,EP)=FM14(I,J,EP)+
     1                 BF(EP,K,I,1)*DMF(K,L,EP)*BM(EP,L,J,4)
          FM23(I,J,EP)=FM23(I,J,EP)+
     1                 BF(EP,K,I,2)*DMF(K,L,EP)*BM(EP,L,J,3)
          FM24(I,J,EP)=FM24(I,J,EP)+
     1                 BF(EP,K,I,2)*DMF(K,L,EP)*BM(EP,L,J,4)
          FM34(I,J,EP)=FM34(I,J,EP)+
     1                 BF(EP,K,I,3)*DMF(K,L,EP)*BM(EP,L,J,4)
        ENDDO
       ENDDO
       ENDDO
       ENDDO
       ENDDO
C       
       ENDIF !(IORTH==1)
C
       IF (IKGEO==1) THEN
#include "vectorize.inc"
        DO M=JFT,JLT 
         EP=IPLAT(M)
         C2=VOL(EP)
         FXX(M)=FOR(EP,1)*C2
         FYY(M)=FOR(EP,2)*C2
        ENDDO
C
        DO I=1,2
         DO EP=JFT,NPLAT
          T11(I,I,EP)=BB(I,1,EP)*BB(I,1,EP)
          T22(I,I,EP)=BB(I,2,EP)*BB(I,2,EP)
          T33(I,I,EP)=BB(I,3,EP)*BB(I,3,EP)
          T44(I,I,EP)=BB(I,4,EP)*BB(I,4,EP)
          T12(I,I,EP)=BB(I,1,EP)*BB(I,2,EP)
          T13(I,I,EP)=BB(I,1,EP)*BB(I,3,EP)
          T14(I,I,EP)=BB(I,1,EP)*BB(I,4,EP)
          T23(I,I,EP)=BB(I,2,EP)*BB(I,3,EP)
          T24(I,I,EP)=BB(I,2,EP)*BB(I,4,EP)
          T34(I,I,EP)=BB(I,3,EP)*BB(I,4,EP)
         ENDDO
        ENDDO
C
        DO I=1,2
         DO EP=NF,JLT 
          T11(I,I,EP)=BM(EP,I,I,1)*BM(EP,I,I,1)
          T22(I,I,EP)=BM(EP,I,I,2)*BM(EP,I,I,2)
          T33(I,I,EP)=BM(EP,I,I,3)*BM(EP,I,I,3)
          T44(I,I,EP)=BM(EP,I,I,4)*BM(EP,I,I,4)
          T12(I,I,EP)=BM(EP,I,I,1)*BM(EP,I,I,2)
          T13(I,I,EP)=BM(EP,I,I,1)*BM(EP,I,I,3)
          T14(I,I,EP)=BM(EP,I,I,1)*BM(EP,I,I,4)
          T23(I,I,EP)=BM(EP,I,I,2)*BM(EP,I,I,3)
          T24(I,I,EP)=BM(EP,I,I,2)*BM(EP,I,I,4)
          T34(I,I,EP)=BM(EP,I,I,3)*BM(EP,I,I,4)
         ENDDO
        ENDDO
        DO EP=JFT,JLT 
          T11(1,2,EP)=FXX(EP)*T11(1,1,EP)+FYY(EP)*T11(2,2,EP)
          T22(1,2,EP)=FXX(EP)*T22(1,1,EP)+FYY(EP)*T22(2,2,EP)
          T33(1,2,EP)=FXX(EP)*T33(1,1,EP)+FYY(EP)*T33(2,2,EP)
          T44(1,2,EP)=FXX(EP)*T44(1,1,EP)+FYY(EP)*T44(2,2,EP)
          T12(1,2,EP)=FXX(EP)*T12(1,1,EP)+FYY(EP)*T12(2,2,EP)
          T13(1,2,EP)=FXX(EP)*T13(1,1,EP)+FYY(EP)*T13(2,2,EP)
          T14(1,2,EP)=FXX(EP)*T14(1,1,EP)+FYY(EP)*T14(2,2,EP)
          T23(1,2,EP)=FXX(EP)*T23(1,1,EP)+FYY(EP)*T23(2,2,EP)
          T24(1,2,EP)=FXX(EP)*T24(1,1,EP)+FYY(EP)*T24(2,2,EP)
          T34(1,2,EP)=FXX(EP)*T34(1,1,EP)+FYY(EP)*T34(2,2,EP)
        ENDDO
C
        IF (IDRIL>0) THEN
#include "vectorize.inc"
        DO M=JFT,JLT
         EP=IPLAT(M)
         FXY(M)=FOR(EP,3)*VOL(EP)
         FXY2(M)=TWO*FXY(M)
        ENDDO
C---------distinque plat and warped
        DO EP=JFT,NPLAT
         T11(1,2,EP)=T11(1,2,EP)+FXY2(EP)*BB(1,1,EP)*BB(2,1,EP)
         T22(1,2,EP)=T22(1,2,EP)+FXY2(EP)*BB(1,2,EP)*BB(2,2,EP)
         T33(1,2,EP)=T33(1,2,EP)+FXY2(EP)*BB(1,3,EP)*BB(2,3,EP)
         T44(1,2,EP)=T44(1,2,EP)+FXY2(EP)*BB(1,4,EP)*BB(2,4,EP)
         T12(1,2,EP)=T12(1,2,EP)+FXY(EP)*(BB(1,1,EP)*BB(2,2,EP)+
     .                                    BB(2,1,EP)*BB(1,2,EP))
         T13(1,2,EP)=T13(1,2,EP)+FXY(EP)*(BB(1,1,EP)*BB(2,3,EP)+
     .                                    BB(2,1,EP)*BB(1,3,EP))
         T14(1,2,EP)=T14(1,2,EP)+FXY(EP)*(BB(1,1,EP)*BB(2,4,EP)+
     .                                    BB(2,1,EP)*BB(1,4,EP))
         T23(1,2,EP)=T23(1,2,EP)+FXY(EP)*(BB(1,2,EP)*BB(2,3,EP)+
     .                                    BB(2,2,EP)*BB(1,3,EP))
         T24(1,2,EP)=T24(1,2,EP)+FXY(EP)*(BB(1,2,EP)*BB(2,4,EP)+
     .                                    BB(2,2,EP)*BB(1,4,EP))
         T34(1,2,EP)=T34(1,2,EP)+FXY(EP)*(BB(1,3,EP)*BB(2,4,EP)+
     .                                    BB(2,3,EP)*BB(1,4,EP))
        ENDDO
        DO EP=NF,JLT
         T11(1,2,EP)=T11(1,2,EP)+FXY2(EP)*BM(EP,3,1,1)*BM(EP,3,2,1)
         T22(1,2,EP)=T22(1,2,EP)+FXY2(EP)*BM(EP,3,1,2)*BM(EP,3,2,2)
         T33(1,2,EP)=T33(1,2,EP)+FXY2(EP)*BM(EP,3,1,3)*BM(EP,3,2,3)
         T44(1,2,EP)=T44(1,2,EP)+FXY2(EP)*BM(EP,3,1,4)*BM(EP,3,2,4)
         T12(1,2,EP)=T12(1,2,EP)+FXY(EP)*(BM(EP,3,1,1)*BM(EP,3,2,2)+
     .                                    BM(EP,3,2,1)*BM(EP,3,1,2))
         T13(1,2,EP)=T13(1,2,EP)+FXY(EP)*(BM(EP,3,1,1)*BM(EP,3,2,3)+
     .                                    BM(EP,3,2,1)*BM(EP,3,1,3))
         T14(1,2,EP)=T14(1,2,EP)+FXY(EP)*(BM(EP,3,1,1)*BM(EP,3,2,4)+
     .                                    BM(EP,3,2,1)*BM(EP,3,1,4))
         T23(1,2,EP)=T23(1,2,EP)+FXY(EP)*(BM(EP,3,1,2)*BM(EP,3,2,3)+
     .                                    BM(EP,3,2,2)*BM(EP,3,1,3))
         T24(1,2,EP)=T24(1,2,EP)+FXY(EP)*(BM(EP,3,1,2)*BM(EP,3,2,4)+
     .                                    BM(EP,3,2,2)*BM(EP,3,1,4))
         T34(1,2,EP)=T34(1,2,EP)+FXY(EP)*(BM(EP,3,1,3)*BM(EP,3,2,4)+
     .                                    BM(EP,3,2,3)*BM(EP,3,1,4))
        ENDDO
C
        ENDIF
C
        DO I=1,3
         DO EP=JFT,JLT 
          K11(I,I,EP)=K11(I,I,EP)+T11(1,2,EP)
          K22(I,I,EP)=K22(I,I,EP)+T22(1,2,EP)
          K33(I,I,EP)=K33(I,I,EP)+T33(1,2,EP)
          K44(I,I,EP)=K44(I,I,EP)+T44(1,2,EP) 
          K12(I,I,EP)=K12(I,I,EP)+T12(1,2,EP) 
          K13(I,I,EP)=K13(I,I,EP)+T13(1,2,EP) 
          K14(I,I,EP)=K14(I,I,EP)+T14(1,2,EP) 
          K23(I,I,EP)=K23(I,I,EP)+T23(1,2,EP) 
          K24(I,I,EP)=K24(I,I,EP)+T24(1,2,EP) 
          K34(I,I,EP)=K34(I,I,EP)+T34(1,2,EP) 
         ENDDO
        ENDDO
C------ renforce positive  ----------
        IF (NEIG==0.AND.IDRIL==0.AND. IORTH >0 ) THEN
         DO EP=JFT,JLT
          C1 = MIN(T11(1,2,EP),T22(1,2,EP),T33(1,2,EP),T44(1,2,EP))
          IF (C1 < ZERO) THEN
           C2 =MIN(M11(3,3,EP),M22(3,3,EP),M33(3,3,EP),M44(3,3,EP))
           C1 = MIN(-C1,FIVE*C2)
           M11(3,3,EP)=M11(3,3,EP) + C1
           M22(3,3,EP)=M22(3,3,EP) + C1
           M33(3,3,EP)=M33(3,3,EP) + C1
           M44(3,3,EP)=M44(3,3,EP) + C1
          END IF
         ENDDO
        ENDIF
C        
       END IF !IF (IKGEO==1)
C    
      RETURN
      END
Chd|====================================================================
Chd|  CBALKEC3                      source/elements/shell/coqueba/cbalke3.F
Chd|-- called by -----------
Chd|        CBAKE3                        source/elements/shell/coqueba/cbake3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CBALKEC3(JFT,JLT, VOL  ,X13  ,X24  ,Y13  ,Y24,HM,
     1                      K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     2                      NPLAT,IPLAT,IKGEO,FOR ,M11,M22,M33,M44,
     3                      IORTH,NEL)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C        CALCUL 'membrane shear traitement' use only PARTIE CONSTANTE
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
       INTEGER JFT ,JLT,NPLAT,IPLAT(*),IKGEO,IORTH,NEL
       my_real 
     .     VOL(*), X13(*)   ,X24(*)    ,Y13(*) ,Y24(*)    ,
     .    K11(3,3,*),K12(3,3,*),K13(3,3,*),K14(3,3,*),
     .    K22(3,3,*),K23(3,3,*),K24(3,3,*),K33(3,3,*),
     .    K34(3,3,*),K44(3,3,*),HM(MVSIZ,4),FOR(NEL,5),
     .    M11(3,3,*),M22(3,3,*),M33(3,3,*),M44(3,3,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
        INTEGER I ,J,EP,M
       my_real 
     .     BB(2,2,MVSIZ),GVOL(MVSIZ),FXY(MVSIZ),FXY2(MVSIZ),
     .     T11(2,2,MVSIZ),T22(2,2,MVSIZ),T12(2,2,MVSIZ),C1,C2
C
#include "vectorize.inc"
       DO M=JFT,JLT 
         EP=IPLAT(M)
         GVOL(M)=HM(EP,4)*VOL(EP)
         BB(2,1,M)=Y24(EP)
         BB(2,2,M)=-Y13(EP)
         BB(1,1,M)=-X24(EP)
         BB(1,2,M)=X13(EP)
       ENDDO
C
       DO I=1,2
       DO J=1,2
        DO EP=JFT,JLT 
         T11(I,J,EP)=GVOL(EP)*BB(I,1,EP)*BB(J,1,EP)
         T22(I,J,EP)=GVOL(EP)*BB(I,2,EP)*BB(J,2,EP)
         T12(I,J,EP)=GVOL(EP)*BB(I,1,EP)*BB(J,2,EP)
        ENDDO
       ENDDO
       ENDDO
C
       DO I=1,2
       DO J=I,2
        DO EP=JFT,JLT 
         K11(I,J,EP)=K11(I,J,EP)+T11(I,J,EP)
         K22(I,J,EP)=K22(I,J,EP)+T22(I,J,EP)
         K33(I,J,EP)=K33(I,J,EP)+T11(I,J,EP)
         K44(I,J,EP)=K44(I,J,EP)+T22(I,J,EP)
        ENDDO
       ENDDO
       ENDDO
       DO I=1,2
       DO J=1,2
        DO EP=JFT,JLT 
         K12(I,J,EP)=K12(I,J,EP)+T12(I,J,EP)
         K13(I,J,EP)=K13(I,J,EP)-T11(I,J,EP)
         K14(I,J,EP)=K14(I,J,EP)-T12(I,J,EP)
         K23(I,J,EP)=K23(I,J,EP)-T12(J,I,EP)
         K24(I,J,EP)=K24(I,J,EP)-T22(I,J,EP)
         K34(I,J,EP)=K34(I,J,EP)+T12(I,J,EP)
        ENDDO
       ENDDO
       ENDDO
       IF (IKGEO==1) THEN
#include "vectorize.inc"
        DO M=JFT,JLT 
         EP=IPLAT(M)
         FXY(M)=FOR(EP,3)*VOL(EP)
         FXY2(M)=TWO*FXY(M)
        ENDDO
C
        DO EP=JFT,JLT 
         T11(1,2,EP)=FXY2(EP)*BB(1,1,EP)*BB(2,1,EP)
         T22(1,2,EP)=FXY2(EP)*BB(1,2,EP)*BB(2,2,EP)
         T12(1,2,EP)=FXY(EP)*(BB(1,1,EP)*BB(2,2,EP)+
     .                        BB(2,1,EP)*BB(1,2,EP))
        ENDDO
        DO I=1,3
         DO EP=JFT,JLT
          K11(I,I,EP)=K11(I,I,EP)+T11(1,2,EP)
          K22(I,I,EP)=K22(I,I,EP)+T22(1,2,EP)
          K33(I,I,EP)=K33(I,I,EP)+T11(1,2,EP)
          K44(I,I,EP)=K44(I,I,EP)+T22(1,2,EP)
          K12(I,I,EP)=K12(I,I,EP)+T12(1,2,EP)
          K13(I,I,EP)=K13(I,I,EP)-T11(1,2,EP)
          K14(I,I,EP)=K14(I,I,EP)-T12(1,2,EP)
          K23(I,I,EP)=K23(I,I,EP)-T12(1,2,EP)
          K24(I,I,EP)=K24(I,I,EP)-T22(1,2,EP)
          K34(I,I,EP)=K34(I,I,EP)+T12(1,2,EP)
         ENDDO
        ENDDO
C------ renforce positive  ----------
        IF (NEIG == 0.AND. IORTH >0 ) THEN
         DO EP=JFT,JLT
          C1 = MIN(T11(1,2,EP),T22(1,2,EP))
          IF (C1 < ZERO) THEN
           C2 =MIN(M11(3,3,EP),M22(3,3,EP),M33(3,3,EP),M44(3,3,EP))
           C1 = MIN(-C1,TWO*C2)
           M11(3,3,EP)=M11(3,3,EP)+C1
           M22(3,3,EP)=M22(3,3,EP)+C1
           M33(3,3,EP)=M33(3,3,EP)+C1
           M44(3,3,EP)=M44(3,3,EP)+C1
          END IF
         ENDDO
        ENDIF
       ENDIF 
C
       RETURN
       END
Chd|====================================================================
Chd|  CBALKERZ                      source/elements/shell/coqueba/cbalke3.F
Chd|-- called by -----------
Chd|        CBAKE3                        source/elements/shell/coqueba/cbake3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CBALKERZ(JFT ,JLT   ,VOL  ,THK0 ,
     2                     HM   ,HZ   ,BM   ,
     6                    K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     7                    M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     8                    MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33,
     9                    MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34,
     A                    IORTH,HMOR,HFOR ,IPLAT,NPLAT,
     B                    PMRZ,BRZ  ,FRZ  ,IKGEO,NG   ,HMFOR,BF ,
     C                    BMF ,NEL)
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
      INTEGER JFT,JLT,IPLAT(*),IORTH,NPLAT,NG,IKGEO,NEL
      MY_REAL
     .    VOL(*),THK0(*),HM(MVSIZ,4),HZ(*),BM(MVSIZ,3,3,4),
     .    K11(3,3,*),K12(3,3,*),K13(3,3,*),K14(3,3,*),
     .    K22(3,3,*),K23(3,3,*),K24(3,3,*),K33(3,3,*),
     .    M11(3,3,*),M12(3,3,*),M13(3,3,*),M14(3,3,*),
     .    M22(3,3,*),M23(3,3,*),M24(3,3,*),M33(3,3,*),
     .    MF11(3,3,*),MF12(3,3,*),MF13(3,3,*),MF14(3,3,*),
     .    MF22(3,3,*),MF23(3,3,*),MF24(3,3,*),MF33(3,3,*),
     .    FM12(3,3,*),FM13(3,3,*),FM14(3,3,*),
     .    FM23(3,3,*),FM24(3,3,*),FM34(3,3,*),
     .    K34(3,3,*),K44(3,3,*),M34(3,3,*),M44(3,3,*),
     .    MF34(3,3,*),MF44(3,3,*),HMOR(MVSIZ,2),HFOR(MVSIZ,2),
     .    PMRZ(MVSIZ,3,4),BRZ(MVSIZ,4,4),FRZ(NEL,5),
     .    BF(MVSIZ,3,2,4),HMFOR(MVSIZ,6),BMF(MVSIZ,3,3,4)
C---------------|[KIJ][MFIJ]|----
C-----KE(6x6)=  |           |
C---------------|[FMIJ]{MIJ]|----
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER EP,I,J,NF,M
      MY_REAL
     .    T11(3,3,MVSIZ),T12(3,3,MVSIZ),T13(3,3,MVSIZ),T14(3,3,MVSIZ),
     .    T22(3,3,MVSIZ),T23(3,3,MVSIZ),T24(3,3,MVSIZ),T33(3,3,MVSIZ),
     .    T44(3,3,MVSIZ),T34(3,3,MVSIZ),
     .    BB(2,4,MVSIZ),GM(MVSIZ),BMRZ1(MVSIZ,4,4),BMRZ2(MVSIZ,4,4),
     .    BMRZ3(MVSIZ,4,4),DHZ(MVSIZ),
     .    DM(3,3,MVSIZ),DPRZ(4,MVSIZ),PX(4,MVSIZ),PY(4,MVSIZ),
     .    C1,C2,CBRX(4,MVSIZ),CBRY(4,MVSIZ),CBRZ(4,MVSIZ),FRZV(MVSIZ)
      my_real
     .   PRX(4,MVSIZ),PRY(4,MVSIZ),PRXY(4,MVSIZ),PRZ(4,MVSIZ),
     .   DMF(3,3,MVSIZ),CBR1(4,MVSIZ),CBR2(4,MVSIZ),CBR3(4,MVSIZ)
C-----------Attention Matrice sym Kii ne calcul que la moitie---------72
        NF =NPLAT+1
#include "vectorize.inc"
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
       IF (IORTH>0) THEN
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
C
       DO J=1,4
       DO M=JFT,JLT
        PRZ(J,M)=BRZ(M,4,J)
        DPRZ(J,M)=PRZ(J,M)*DHZ(M)
       ENDDO
       ENDDO
C-------0.5*[-By Bx 0 0 0 BRZ]^tKG[-By Bx 0 0 0 BRZ]*0.5
C---warped----0.5*[Pmrz1 Pmrz2 Pmrz3 0 0 BRZ]^tKG[Pmrz1 Pmrz2 Pmrz3  0 0 BRZ]*0.5
C------ BRZ(1-3,J,EP):Pmrz1 Pmrz2 Pmrz3  BRZ(4,J,EP)= (-BRXY+BRYX)-2NI :BB(1,J,I):-ByJ; BB(2,J,I):BxJ;
       DO EP=JFT,NPLAT
         BB(2,1,EP)=BM(EP,1,1,1)
         BB(2,2,EP)=BM(EP,2,1,1)
         BB(2,3,EP)=BM(EP,3,1,1)
         BB(2,4,EP)=BM(EP,1,2,1)
         BB(1,1,EP)=-BM(EP,2,2,1)
         BB(1,2,EP)=-BM(EP,3,2,1)
         BB(1,3,EP)=-BM(EP,1,3,1)
         BB(1,4,EP)=-BM(EP,2,3,1)
       ENDDO
C       
       DO J=1,4
        DO EP=JFT,NPLAT
         PX(J,EP) = BB(2,J,EP)
         PY(J,EP) =-BB(1,J,EP)
        ENDDO
       ENDDO
C
       DO I=1,2
       DO J=I,2
        DO EP=JFT,NPLAT
         T11(I,J,EP)=BB(I,1,EP)*BB(J,1,EP)
         T11(J,I,EP)=T11(I,J,EP)
         T22(I,J,EP)=BB(I,2,EP)*BB(J,2,EP)
         T22(J,I,EP)=T22(I,J,EP)
         T33(I,J,EP)=BB(I,3,EP)*BB(J,3,EP)
         T33(J,I,EP)=T33(I,J,EP)
         T44(I,J,EP)=BB(I,4,EP)*BB(J,4,EP)
         T44(J,I,EP)=T44(I,J,EP)
       ENDDO
       ENDDO
       ENDDO
C
       DO I=1,2
       DO J=1,2
        DO EP=JFT,NPLAT
         T12(I,J,EP)=BB(I,1,EP)*BB(J,2,EP)
         T13(I,J,EP)=BB(I,1,EP)*BB(J,3,EP)
         T14(I,J,EP)=BB(I,1,EP)*BB(J,4,EP)
         T23(I,J,EP)=BB(I,2,EP)*BB(J,3,EP)
         T24(I,J,EP)=BB(I,2,EP)*BB(J,4,EP)
         T34(I,J,EP)=BB(I,3,EP)*BB(J,4,EP)
        ENDDO
       ENDDO
       ENDDO
C
       DO I=1,2
       DO J=I,2
        DO EP=JFT,NPLAT
         K11(I,J,EP)=K11(I,J,EP)+T11(I,J,EP)*DHZ(EP)
         K22(I,J,EP)=K22(I,J,EP)+T22(I,J,EP)*DHZ(EP)
         K33(I,J,EP)=K33(I,J,EP)+T33(I,J,EP)*DHZ(EP)
         K44(I,J,EP)=K44(I,J,EP)+T44(I,J,EP)*DHZ(EP)
        ENDDO
       ENDDO
       ENDDO
C
       DO I=1,2
       DO J=1,2
        DO EP=JFT,NPLAT
         K12(I,J,EP)=K12(I,J,EP)+T12(I,J,EP)*DHZ(EP)
         K13(I,J,EP)=K13(I,J,EP)+T13(I,J,EP)*DHZ(EP)
         K14(I,J,EP)=K14(I,J,EP)+T14(I,J,EP)*DHZ(EP)
         K23(I,J,EP)=K23(I,J,EP)+T23(I,J,EP)*DHZ(EP)
         K24(I,J,EP)=K24(I,J,EP)+T24(I,J,EP)*DHZ(EP)
         K34(I,J,EP)=K34(I,J,EP)+T34(I,J,EP)*DHZ(EP)
        ENDDO
       ENDDO
       ENDDO
C
       DO I=1,4
       DO J=1,4
        DO EP=JFT,NPLAT
         BMRZ1(EP,I,J)=BB(1,I,EP)*DPRZ(J,EP)
         BMRZ2(EP,I,J)=BB(2,I,EP)*DPRZ(J,EP)
        ENDDO
       ENDDO
       ENDDO
C
       DO EP=JFT,NPLAT
C------MFIJ(1,3)=BB(1,I)*BRZ(J)=BMRZ1(I,J); MFIJ(2,3)=BB(2,I)*BRZ(J)=BMRZ2(I,J)
C------FMIJ(3,1)=BB(1,J)*BRZ(I)=BMRZ1(J,I); FMIJ(3,2)=BB(2,J)*BRZ(I)=BMRZ2(J,I);
        MF11(1,3,EP)=MF11(1,3,EP)+ BMRZ1(EP,1,1)
        MF11(2,3,EP)=MF11(2,3,EP)+ BMRZ2(EP,1,1)
        MF22(1,3,EP)=MF22(1,3,EP)+ BMRZ1(EP,2,2)
        MF22(2,3,EP)=MF22(2,3,EP)+ BMRZ2(EP,2,2)
        MF33(1,3,EP)=MF33(1,3,EP)+ BMRZ1(EP,3,3)
        MF33(2,3,EP)=MF33(2,3,EP)+ BMRZ2(EP,3,3)
        MF44(1,3,EP)=MF44(1,3,EP)+ BMRZ1(EP,4,4)
        MF44(2,3,EP)=MF44(2,3,EP)+ BMRZ2(EP,4,4)
C
        MF12(1,3,EP)=MF12(1,3,EP)+ BMRZ1(EP,1,2)
        MF12(2,3,EP)=MF12(2,3,EP)+ BMRZ2(EP,1,2)
        FM12(3,1,EP)=FM12(3,1,EP)+ BMRZ1(EP,2,1)
        FM12(3,2,EP)=FM12(3,2,EP)+ BMRZ2(EP,2,1)
        MF13(1,3,EP)=MF13(1,3,EP)+ BMRZ1(EP,1,3)
        MF13(2,3,EP)=MF13(2,3,EP)+ BMRZ2(EP,1,3)
        FM13(3,1,EP)=FM13(3,1,EP)+ BMRZ1(EP,3,1)
        FM13(3,2,EP)=FM13(3,2,EP)+ BMRZ2(EP,3,1)
        MF14(1,3,EP)=MF14(1,3,EP)+ BMRZ1(EP,1,4)
        MF14(2,3,EP)=MF14(2,3,EP)+ BMRZ2(EP,1,4)
        FM14(3,1,EP)=FM14(3,1,EP)+ BMRZ1(EP,4,1)
        FM14(3,2,EP)=FM14(3,2,EP)+ BMRZ2(EP,4,1)
        MF23(1,3,EP)=MF23(1,3,EP)+ BMRZ1(EP,2,3)
        MF23(2,3,EP)=MF23(2,3,EP)+ BMRZ2(EP,2,3)
        FM23(3,1,EP)=FM23(3,1,EP)+ BMRZ1(EP,3,2)
        FM23(3,2,EP)=FM23(3,2,EP)+ BMRZ2(EP,3,2)
        MF24(1,3,EP)=MF24(1,3,EP)+ BMRZ1(EP,2,4)
        MF24(2,3,EP)=MF24(2,3,EP)+ BMRZ2(EP,2,4)
        FM24(3,1,EP)=FM24(3,1,EP)+ BMRZ1(EP,4,2)
        FM24(3,2,EP)=FM24(3,2,EP)+ BMRZ2(EP,4,2)
        MF34(1,3,EP)=MF34(1,3,EP)+ BMRZ1(EP,3,4)
        MF34(2,3,EP)=MF34(2,3,EP)+ BMRZ2(EP,3,4)
        FM34(3,1,EP)=FM34(3,1,EP)+ BMRZ1(EP,4,3)
        FM34(3,2,EP)=FM34(3,2,EP)+ BMRZ2(EP,4,3)
       ENDDO
       IF (IORTH>0) THEN
C ----add mem-bending coupling terms of orthotrope--coplanar first---
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
         CBR1(J,EP) =DMF(1,1,EP)*PMRZ(EP,1,J)+DMF(1,2,EP)*PMRZ(EP,2,J)+
     .               DMF(1,3,EP)*PMRZ(EP,3,J)
         CBR2(J,EP) =DMF(2,1,EP)*PMRZ(EP,1,J)+DMF(2,2,EP)*PMRZ(EP,2,J)+
     .               DMF(2,3,EP)*PMRZ(EP,3,J)
         CBR3(J,EP) =DMF(3,1,EP)*PMRZ(EP,1,J)+DMF(3,2,EP)*PMRZ(EP,2,J)+
     .               DMF(3,3,EP)*PMRZ(EP,3,J)
         ENDDO
        ENDDO
C ----add Rz-bending coupling terms of orthotrope--coplanar first---
C     MIJ(1,3)=-BY(I)*CBR2(J)-BX(I)*CBR3(J)
C     MIJ(2,3)=BX(I)*CBR1(J)+BY(I)*CBR3(J)
C     MIJ(3,1)=-BY(J)*CBR2(I)-BX(J)*CBR3(I)
C     MIJ(3,2)=BX(J)*CBR1(I)+BY(J)*CBR3(I)
C         
         DO EP=JFT,NPLAT
          M11(1,3,EP)=M11(1,3,EP)
     .                -PY(1,EP)*CBR2(1,EP)-PX(1,EP)*CBR3(1,EP)
          M11(2,3,EP)=M11(2,3,EP)
     .                +PX(1,EP)*CBR1(1,EP)+PY(1,EP)*CBR3(1,EP)
          M22(1,3,EP)=M22(1,3,EP)
     .                -PY(2,EP)*CBR2(2,EP)-PX(2,EP)*CBR3(2,EP)
          M22(2,3,EP)=M22(2,3,EP)
     .                +PX(2,EP)*CBR1(2,EP)+PY(2,EP)*CBR3(2,EP)
          M33(1,3,EP)=M33(1,3,EP)
     .                -PY(3,EP)*CBR2(3,EP)-PX(3,EP)*CBR3(3,EP)
          M33(2,3,EP)=M33(2,3,EP)
     .                +PX(3,EP)*CBR1(3,EP)+PY(3,EP)*CBR3(3,EP)
          M44(1,3,EP)=M44(1,3,EP)
     .                -PY(4,EP)*CBR2(4,EP)-PX(4,EP)*CBR3(4,EP)
          M44(2,3,EP)=M44(2,3,EP)
     .                +PX(4,EP)*CBR1(4,EP)+PY(4,EP)*CBR3(4,EP)
         ENDDO
         DO EP=JFT,NPLAT 
          M12(1,3,EP)=M12(1,3,EP)
     .                -PY(1,EP)*CBR2(2,EP)-PX(1,EP)*CBR3(2,EP)
          M12(2,3,EP)=M12(2,3,EP)
     .                +PX(1,EP)*CBR1(2,EP)+PY(1,EP)*CBR3(2,EP)
          M13(1,3,EP)=M13(1,3,EP)
     .                -PY(1,EP)*CBR2(3,EP)-PX(1,EP)*CBR3(3,EP)
          M13(2,3,EP)=M13(2,3,EP)
     .                +PX(1,EP)*CBR1(3,EP)+PY(1,EP)*CBR3(3,EP)
          M14(1,3,EP)=M14(1,3,EP)
     .                -PY(1,EP)*CBR2(4,EP)-PX(1,EP)*CBR3(4,EP)
          M14(2,3,EP)=M14(2,3,EP)
     .                +PX(1,EP)*CBR1(4,EP)+PY(1,EP)*CBR3(4,EP)
          M23(1,3,EP)=M23(1,3,EP)
     .                -PY(2,EP)*CBR2(3,EP)-PX(2,EP)*CBR3(3,EP)
          M23(2,3,EP)=M23(2,3,EP)
     .                +PX(2,EP)*CBR1(3,EP)+PY(2,EP)*CBR3(3,EP)
          M24(1,3,EP)=M24(1,3,EP)
     .                -PY(2,EP)*CBR2(4,EP)-PX(2,EP)*CBR3(4,EP)
          M24(2,3,EP)=M24(2,3,EP)
     .                +PX(2,EP)*CBR1(4,EP)+PY(2,EP)*CBR3(4,EP)
          M34(1,3,EP)=M34(1,3,EP)
     .                -PY(3,EP)*CBR2(4,EP)-PX(3,EP)*CBR3(4,EP)
          M34(2,3,EP)=M34(2,3,EP)
     .                +PX(3,EP)*CBR1(4,EP)+PY(3,EP)*CBR3(4,EP)
         ENDDO
         DO EP=JFT,NPLAT 
          M12(3,1,EP)=M12(3,1,EP)
     .                -PY(2,EP)*CBR2(1,EP)-PX(2,EP)*CBR3(1,EP)
          M12(3,2,EP)=M12(3,2,EP)
     .                +PX(2,EP)*CBR1(1,EP)+PY(2,EP)*CBR3(1,EP)
          M13(3,1,EP)=M13(3,1,EP)
     .                -PY(3,EP)*CBR2(1,EP)-PX(3,EP)*CBR3(1,EP)
          M13(3,2,EP)=M13(3,2,EP)
     .                +PX(3,EP)*CBR1(1,EP)+PY(3,EP)*CBR3(1,EP)
          M14(3,1,EP)=M14(3,1,EP)
     .                -PY(4,EP)*CBR2(1,EP)-PX(4,EP)*CBR3(1,EP)
          M14(3,2,EP)=M14(3,2,EP)
     .                +PX(4,EP)*CBR1(1,EP)+PY(4,EP)*CBR3(1,EP)
          M23(3,1,EP)=M23(3,1,EP)
     .                -PY(3,EP)*CBR2(2,EP)-PX(3,EP)*CBR3(2,EP)
          M23(3,2,EP)=M23(3,2,EP)
     .                +PX(3,EP)*CBR1(2,EP)+PY(3,EP)*CBR3(2,EP)
          M24(3,1,EP)=M24(3,1,EP)
     .                -PY(4,EP)*CBR2(2,EP)-PX(4,EP)*CBR3(2,EP)
          M24(3,2,EP)=M24(3,2,EP)
     .                +PX(4,EP)*CBR1(2,EP)+PY(4,EP)*CBR3(2,EP)
          M34(3,1,EP)=M34(3,1,EP)
     .                -PY(4,EP)*CBR2(3,EP)-PX(4,EP)*CBR3(3,EP)
          M34(3,2,EP)=M34(3,2,EP)
     .                +PX(4,EP)*CBR1(3,EP)+PY(4,EP)*CBR3(3,EP)
         ENDDO
       ENDIF !IF (IORTH==1)
C--------warped elements--------
       DO I=1,3
       DO J=I,3
        DO EP=NF,JLT
         T11(I,J,EP)=BRZ(EP,I,1)*BRZ(EP,J,1)
         T11(J,I,EP)=T11(I,J,EP)
         T22(I,J,EP)=BRZ(EP,I,2)*BRZ(EP,J,2)
         T22(J,I,EP)=T22(I,J,EP)
         T33(I,J,EP)=BRZ(EP,I,3)*BRZ(EP,J,3)
         T33(J,I,EP)=T33(I,J,EP)
         T44(I,J,EP)=BRZ(EP,I,4)*BRZ(EP,J,4)
         T44(J,I,EP)=T44(I,J,EP)
       ENDDO
       ENDDO
       ENDDO
C
       DO I=1,3
       DO J=1,3
        DO EP=NF,JLT
         T12(I,J,EP)=BRZ(EP,I,1)*BRZ(EP,J,2)
         T13(I,J,EP)=BRZ(EP,I,1)*BRZ(EP,J,3)
         T14(I,J,EP)=BRZ(EP,I,1)*BRZ(EP,J,4)
         T23(I,J,EP)=BRZ(EP,I,2)*BRZ(EP,J,3)
         T24(I,J,EP)=BRZ(EP,I,2)*BRZ(EP,J,4)
         T34(I,J,EP)=BRZ(EP,I,3)*BRZ(EP,J,4)
        ENDDO
       ENDDO
       ENDDO
C
       DO I=1,3
       DO J=I,3
        DO EP=NF,JLT
         K11(I,J,EP)=K11(I,J,EP)+T11(I,J,EP)*DHZ(EP)
         K22(I,J,EP)=K22(I,J,EP)+T22(I,J,EP)*DHZ(EP)
         K33(I,J,EP)=K33(I,J,EP)+T33(I,J,EP)*DHZ(EP)
         K44(I,J,EP)=K44(I,J,EP)+T44(I,J,EP)*DHZ(EP)
        ENDDO
       ENDDO
       ENDDO
C
       DO I=1,3
       DO J=1,3
        DO EP=NF,JLT
         K12(I,J,EP)=K12(I,J,EP)+T12(I,J,EP)*DHZ(EP)
         K13(I,J,EP)=K13(I,J,EP)+T13(I,J,EP)*DHZ(EP)
         K14(I,J,EP)=K14(I,J,EP)+T14(I,J,EP)*DHZ(EP)
         K23(I,J,EP)=K23(I,J,EP)+T23(I,J,EP)*DHZ(EP)
         K24(I,J,EP)=K24(I,J,EP)+T24(I,J,EP)*DHZ(EP)
         K34(I,J,EP)=K34(I,J,EP)+T34(I,J,EP)*DHZ(EP)
        ENDDO
       ENDDO
       ENDDO
C
       DO I=1,4
       DO J=1,4
        DO EP=NF,JLT
         BMRZ1(EP,I,J)=BRZ(EP,1,I)*DPRZ(J,EP)
         BMRZ2(EP,I,J)=BRZ(EP,2,I)*DPRZ(J,EP)
         BMRZ3(EP,I,J)=BRZ(EP,3,I)*DPRZ(J,EP)
        ENDDO
       ENDDO
       ENDDO
C
       DO EP=NF,JLT
C------MFIJ(1,3)=BB(1,I)*BRZ(J)=BMRZ1(I,J); MFIJ(2,3)=BB(2,I)*BRZ(J)=BMRZ2(I,J)
C------FMIJ(3,1)=BB(1,J)*BRZ(I)=BMRZ1(J,I); FMIJ(3,2)=BB(2,J)*BRZ(I)=BMRZ2(J,I);
        MF11(1,3,EP)=MF11(1,3,EP)+ BMRZ1(EP,1,1)
        MF11(2,3,EP)=MF11(2,3,EP)+ BMRZ2(EP,1,1)
        MF11(3,3,EP)=MF11(3,3,EP)+ BMRZ3(EP,1,1)
        
        MF22(1,3,EP)=MF22(1,3,EP)+ BMRZ1(EP,2,2)
        MF22(2,3,EP)=MF22(2,3,EP)+ BMRZ2(EP,2,2)
        MF22(3,3,EP)=MF22(3,3,EP)+ BMRZ3(EP,2,2)
        
        MF33(1,3,EP)=MF33(1,3,EP)+ BMRZ1(EP,3,3)
        MF33(2,3,EP)=MF33(2,3,EP)+ BMRZ2(EP,3,3)
        MF33(3,3,EP)=MF33(3,3,EP)+ BMRZ3(EP,3,3)
        
        MF44(1,3,EP)=MF44(1,3,EP)+ BMRZ1(EP,4,4)
        MF44(2,3,EP)=MF44(2,3,EP)+ BMRZ2(EP,4,4)
        MF44(3,3,EP)=MF44(3,3,EP)+ BMRZ3(EP,4,4)
C
        MF12(1,3,EP)=MF12(1,3,EP)+ BMRZ1(EP,1,2)
        MF12(2,3,EP)=MF12(2,3,EP)+ BMRZ2(EP,1,2)
        MF12(3,3,EP)=MF12(3,3,EP)+ BMRZ3(EP,1,2)
        
        FM12(3,1,EP)=FM12(3,1,EP)+ BMRZ1(EP,2,1)
        FM12(3,2,EP)=FM12(3,2,EP)+ BMRZ2(EP,2,1)
        FM12(3,3,EP)=FM12(3,3,EP)+ BMRZ3(EP,2,1)
        
        MF13(1,3,EP)=MF13(1,3,EP)+ BMRZ1(EP,1,3)
        MF13(2,3,EP)=MF13(2,3,EP)+ BMRZ2(EP,1,3)
        MF13(3,3,EP)=MF13(3,3,EP)+ BMRZ3(EP,1,3)
        
        FM13(3,1,EP)=FM13(3,1,EP)+ BMRZ1(EP,3,1)
        FM13(3,2,EP)=FM13(3,2,EP)+ BMRZ2(EP,3,1)
        FM13(3,3,EP)=FM13(3,3,EP)+ BMRZ3(EP,3,1)
        
        MF14(1,3,EP)=MF14(1,3,EP)+ BMRZ1(EP,1,4)
        MF14(2,3,EP)=MF14(2,3,EP)+ BMRZ2(EP,1,4)
        MF14(3,3,EP)=MF14(3,3,EP)+ BMRZ3(EP,1,4)
        
        FM14(3,1,EP)=FM14(3,1,EP)+ BMRZ1(EP,4,1)
        FM14(3,2,EP)=FM14(3,2,EP)+ BMRZ2(EP,4,1)
        FM14(3,3,EP)=FM14(3,3,EP)+ BMRZ3(EP,4,1)
        
        MF23(1,3,EP)=MF23(1,3,EP)+ BMRZ1(EP,2,3)
        MF23(2,3,EP)=MF23(2,3,EP)+ BMRZ2(EP,2,3)
        MF23(3,3,EP)=MF23(3,3,EP)+ BMRZ3(EP,2,3)
        
        FM23(3,1,EP)=FM23(3,1,EP)+ BMRZ1(EP,3,2)
        FM23(3,2,EP)=FM23(3,2,EP)+ BMRZ2(EP,3,2)
        FM23(3,3,EP)=FM23(3,3,EP)+ BMRZ3(EP,3,2)
        
        MF24(1,3,EP)=MF24(1,3,EP)+ BMRZ1(EP,2,4)
        MF24(2,3,EP)=MF24(2,3,EP)+ BMRZ2(EP,2,4)
        MF24(3,3,EP)=MF24(3,3,EP)+ BMRZ3(EP,2,4)
        
        FM24(3,1,EP)=FM24(3,1,EP)+ BMRZ1(EP,4,2)
        FM24(3,2,EP)=FM24(3,2,EP)+ BMRZ2(EP,4,2)
        FM24(3,3,EP)=FM24(3,3,EP)+ BMRZ3(EP,4,2)
        
        MF34(1,3,EP)=MF34(1,3,EP)+ BMRZ1(EP,3,4)
        MF34(2,3,EP)=MF34(2,3,EP)+ BMRZ2(EP,3,4)
        MF34(3,3,EP)=MF34(3,3,EP)+ BMRZ3(EP,3,4)
        
        FM34(3,1,EP)=FM34(3,1,EP)+ BMRZ1(EP,4,3)
        FM34(3,2,EP)=FM34(3,2,EP)+ BMRZ2(EP,4,3)
        FM34(3,3,EP)=FM34(3,3,EP)+ BMRZ3(EP,4,3)
       ENDDO
C------ Mzz  -------
       DO M=JFT,JLT
        M11(3,3,M)=M11(3,3,M)+PRZ(1,M)*DPRZ(1,M)
        M12(3,3,M)=M12(3,3,M)+PRZ(1,M)*DPRZ(2,M)
        M13(3,3,M)=M13(3,3,M)+PRZ(1,M)*DPRZ(3,M)
        M14(3,3,M)=M14(3,3,M)+PRZ(1,M)*DPRZ(4,M)
        M22(3,3,M)=M22(3,3,M)+PRZ(2,M)*DPRZ(2,M)
        M23(3,3,M)=M23(3,3,M)+PRZ(2,M)*DPRZ(3,M)
        M24(3,3,M)=M24(3,3,M)+PRZ(2,M)*DPRZ(4,M)
        M33(3,3,M)=M33(3,3,M)+PRZ(3,M)*DPRZ(3,M)
        M34(3,3,M)=M34(3,3,M)+PRZ(3,M)*DPRZ(4,M)
        M44(3,3,M)=M44(3,3,M)+PRZ(4,M)*DPRZ(4,M)
       ENDDO
C
C-------[MFIJ]=[Bm]^t[C][BRm]; [MIJ]=[BRm]^t[C][BRm];----
C---------------|0 0 BRX       |----
C-----BR(3x3)=  |0 0 BRY       |
C---------------|0 0 BRXY+BRYX |----
C--  [MFIJ]=|Bxi 0  Byi|{CPRXj}  [FMIJ]={CPRXi CPRYi CPRZi}|Bxj 0  0|
C--         |0  Byi Bxi|{CPRYj}                            |0  Byj 0|
C--         |0   0   0 |{CPRZj}                            |ByjBxj 0|
C-- warped [MFIJ]=[BMi]^t{CPRXj}  [FMIJ]={CPRXi CPRYi CPRZi}[Bmj](3,3)
C
       DO M=JFT,JLT
       DO J=1,4
        PRX(J,M)=PMRZ(M,1,J)
        PRY(J,M)=PMRZ(M,2,J)
        PRXY(J,M)=PMRZ(M,3,J)
       ENDDO
       ENDDO
C-------[C][BRm];----
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
       DO M=JFT,NPLAT
        MF11(1,3,M)=MF11(1,3,M)+ PX(1,M)*CBRX(1,M)+PY(1,M)*CBRZ(1,M)
        MF11(2,3,M)=MF11(2,3,M)+ PY(1,M)*CBRY(1,M)+PX(1,M)*CBRZ(1,M)
        MF12(1,3,M)=MF12(1,3,M)+ PX(1,M)*CBRX(2,M)+PY(1,M)*CBRZ(2,M)
        MF12(2,3,M)=MF12(2,3,M)+ PY(1,M)*CBRY(2,M)+PX(1,M)*CBRZ(2,M)
        MF13(1,3,M)=MF13(1,3,M)+ PX(1,M)*CBRX(3,M)+PY(1,M)*CBRZ(3,M)
        MF13(2,3,M)=MF13(2,3,M)+ PY(1,M)*CBRY(3,M)+PX(1,M)*CBRZ(3,M)
        MF14(1,3,M)=MF14(1,3,M)+ PX(1,M)*CBRX(4,M)+PY(1,M)*CBRZ(4,M)
        MF14(2,3,M)=MF14(2,3,M)+ PY(1,M)*CBRY(4,M)+PX(1,M)*CBRZ(4,M)
        MF22(1,3,M)=MF22(1,3,M)+ PX(2,M)*CBRX(2,M)+PY(2,M)*CBRZ(2,M)
        MF22(2,3,M)=MF22(2,3,M)+ PY(2,M)*CBRY(2,M)+PX(2,M)*CBRZ(2,M)
        MF23(1,3,M)=MF23(1,3,M)+ PX(2,M)*CBRX(3,M)+PY(2,M)*CBRZ(3,M)
        MF23(2,3,M)=MF23(2,3,M)+ PY(2,M)*CBRY(3,M)+PX(2,M)*CBRZ(3,M)
        MF24(1,3,M)=MF24(1,3,M)+ PX(2,M)*CBRX(4,M)+PY(2,M)*CBRZ(4,M)
        MF24(2,3,M)=MF24(2,3,M)+ PY(2,M)*CBRY(4,M)+PX(2,M)*CBRZ(4,M)
        MF33(1,3,M)=MF33(1,3,M)+ PX(3,M)*CBRX(3,M)+PY(3,M)*CBRZ(3,M)
        MF33(2,3,M)=MF33(2,3,M)+ PY(3,M)*CBRY(3,M)+PX(3,M)*CBRZ(3,M)
        MF34(1,3,M)=MF34(1,3,M)+ PX(3,M)*CBRX(4,M)+PY(3,M)*CBRZ(4,M)
        MF34(2,3,M)=MF34(2,3,M)+ PY(3,M)*CBRY(4,M)+PX(3,M)*CBRZ(4,M)
        MF44(1,3,M)=MF44(1,3,M)+ PX(4,M)*CBRX(4,M)+PY(4,M)*CBRZ(4,M)
        MF44(2,3,M)=MF44(2,3,M)+ PY(4,M)*CBRY(4,M)+PX(4,M)*CBRZ(4,M)
C        
        FM12(3,1,M)=FM12(3,1,M)+ PX(2,M)*CBRX(1,M)+PY(2,M)*CBRZ(1,M)
        FM12(3,2,M)=FM12(3,2,M)+ PY(2,M)*CBRY(1,M)+PX(2,M)*CBRZ(1,M)
        FM13(3,1,M)=FM13(3,1,M)+ PX(3,M)*CBRX(1,M)+PY(3,M)*CBRZ(1,M)
        FM13(3,2,M)=FM13(3,2,M)+ PY(3,M)*CBRY(1,M)+PX(3,M)*CBRZ(1,M)
        FM14(3,1,M)=FM14(3,1,M)+ PX(4,M)*CBRX(1,M)+PY(4,M)*CBRZ(1,M)
        FM14(3,2,M)=FM14(3,2,M)+ PY(4,M)*CBRY(1,M)+PX(4,M)*CBRZ(1,M)
        FM23(3,1,M)=FM23(3,1,M)+ PX(3,M)*CBRX(2,M)+PY(3,M)*CBRZ(2,M)
        FM23(3,2,M)=FM23(3,2,M)+ PY(3,M)*CBRY(2,M)+PX(3,M)*CBRZ(2,M)
        FM24(3,1,M)=FM24(3,1,M)+ PX(4,M)*CBRX(2,M)+PY(4,M)*CBRZ(2,M)
        FM24(3,2,M)=FM24(3,2,M)+ PY(4,M)*CBRY(2,M)+PX(4,M)*CBRZ(2,M)
        FM34(3,1,M)=FM34(3,1,M)+ PX(4,M)*CBRX(3,M)+PY(4,M)*CBRZ(3,M)
        FM34(3,2,M)=FM34(3,2,M)+ PY(4,M)*CBRY(3,M)+PX(4,M)*CBRZ(3,M)
       ENDDO
       DO EP=NF,JLT
        DO I=1,3
        MF11(I,3,EP)=MF11(I,3,EP)+ BM(EP,1,I,1)*CBRX(1,EP)+
     .              BM(EP,2,I,1)*CBRY(1,EP)+BM(EP,3,I,1)*CBRZ(1,EP)
        MF12(I,3,EP)=MF12(I,3,EP)+ BM(EP,1,I,1)*CBRX(2,EP)+
     .              BM(EP,2,I,1)*CBRY(2,EP)+BM(EP,3,I,1)*CBRZ(2,EP)
        MF13(I,3,EP)=MF13(I,3,EP)+ BM(EP,1,I,1)*CBRX(3,EP)+
     .              BM(EP,2,I,1)*CBRY(3,EP)+BM(EP,3,I,1)*CBRZ(3,EP)
        MF14(I,3,EP)=MF14(I,3,EP)+ BM(EP,1,I,1)*CBRX(4,EP)+
     .              BM(EP,2,I,1)*CBRY(4,EP)+BM(EP,3,I,1)*CBRZ(4,EP)
        MF22(I,3,EP)=MF22(I,3,EP)+ BM(EP,1,I,2)*CBRX(2,EP)+
     .              BM(EP,2,I,2)*CBRY(2,EP)+BM(EP,3,I,2)*CBRZ(2,EP)
        MF23(I,3,EP)=MF23(I,3,EP)+ BM(EP,1,I,2)*CBRX(3,EP)+
     .              BM(EP,2,I,2)*CBRY(3,EP)+BM(EP,3,I,2)*CBRZ(3,EP)
        MF24(I,3,EP)=MF24(I,3,EP)+ BM(EP,1,I,2)*CBRX(4,EP)+
     .              BM(EP,2,I,2)*CBRY(4,EP)+BM(EP,3,I,2)*CBRZ(4,EP)
        MF33(I,3,EP)=MF33(I,3,EP)+ BM(EP,1,I,3)*CBRX(3,EP)+
     .              BM(EP,2,I,3)*CBRY(3,EP)+BM(EP,3,I,3)*CBRZ(3,EP)
        MF34(I,3,EP)=MF34(I,3,EP)+ BM(EP,1,I,3)*CBRX(4,EP)+
     .              BM(EP,2,I,3)*CBRY(4,EP)+BM(EP,3,I,3)*CBRZ(4,EP)
        MF44(I,3,EP)=MF44(I,3,EP)+ BM(EP,1,I,4)*CBRX(4,EP)+
     .              BM(EP,2,I,4)*CBRY(4,EP)+BM(EP,3,I,4)*CBRZ(4,EP)
C        
        FM12(3,I,EP)=FM12(3,I,EP)+ BM(EP,1,I,2)*CBRX(1,EP)+
     .               BM(EP,2,I,2)*CBRY(1,EP)+BM(EP,3,I,2)*CBRZ(1,EP)
        FM13(3,I,EP)=FM13(3,I,EP)+ BM(EP,1,I,3)*CBRX(1,EP)+
     .               BM(EP,2,I,3)*CBRY(1,EP)+BM(EP,3,I,3)*CBRZ(1,EP)
        FM14(3,I,EP)=FM14(3,I,EP)+ BM(EP,1,I,4)*CBRX(1,EP)+
     .               BM(EP,2,I,4)*CBRY(1,EP)+BM(EP,3,I,4)*CBRZ(1,EP)
        FM23(3,I,EP)=FM23(3,I,EP)+ BM(EP,1,I,3)*CBRX(2,EP)+
     .               BM(EP,2,I,3)*CBRY(2,EP)+BM(EP,3,I,3)*CBRZ(2,EP)
        FM24(3,I,EP)=FM24(3,I,EP)+ BM(EP,1,I,4)*CBRX(2,EP)+
     .               BM(EP,2,I,4)*CBRY(2,EP)+BM(EP,3,I,4)*CBRZ(2,EP)
        FM34(3,I,EP)=FM34(3,I,EP)+ BM(EP,1,I,4)*CBRX(3,EP)+
     .               BM(EP,2,I,4)*CBRY(3,EP)+BM(EP,3,I,4)*CBRZ(3,EP)
        END DO !I=1,3
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
C       
       IF (IORTH>0) THEN
C ----add Rz-bending coupling terms of orthotrope--warped element---
C     MIJ(i,3)=BF(I)^t*CBRi(J) 
C     MIJ(3,i)=BF(J)^t*CBRi(I)
C     FMIJ(i,3)=BMF(I)^t*CBRi(J)
C     MFIJ(3,i)=BMF(J)^t*CBRi(I)
C         
         DO I=1,2
         DO EP=NF,JLT
          M11(I,3,EP)=M11(I,3,EP)
     .                +BF(EP,1,I,1)*CBR1(1,EP)+BF(EP,2,I,1)*CBR2(1,EP)
     .                +BF(EP,3,I,1)*CBR3(1,EP)
          M22(I,3,EP)=M22(I,3,EP)
     .                +BF(EP,1,I,2)*CBR1(2,EP)+BF(EP,2,I,2)*CBR2(2,EP)
     .                +BF(EP,3,I,2)*CBR3(2,EP)
          M33(I,3,EP)=M33(I,3,EP)
     .                +BF(EP,1,I,3)*CBR1(3,EP)+BF(EP,2,I,3)*CBR2(3,EP)
     .                +BF(EP,3,I,3)*CBR3(3,EP)
          M44(I,3,EP)=M44(I,3,EP)
     .                +BF(EP,1,I,4)*CBR1(4,EP)+BF(EP,2,I,4)*CBR2(4,EP)
     .                +BF(EP,3,I,4)*CBR3(4,EP)
         ENDDO
         ENDDO
C         
         DO I=1,2
         DO EP=NF,JLT
          M12(I,3,EP)=M12(I,3,EP)
     .                +BF(EP,1,I,1)*CBR1(2,EP)+BF(EP,2,I,1)*CBR2(2,EP)
     .                +BF(EP,3,I,1)*CBR3(2,EP)
          M13(I,3,EP)=M13(I,3,EP)
     .                +BF(EP,1,I,1)*CBR1(3,EP)+BF(EP,2,I,1)*CBR2(3,EP)
     .                +BF(EP,3,I,1)*CBR3(3,EP)
          M14(I,3,EP)=M14(I,3,EP)
     .                +BF(EP,1,I,1)*CBR1(4,EP)+BF(EP,2,I,1)*CBR2(4,EP)
     .                +BF(EP,3,I,1)*CBR3(4,EP)
          M23(I,3,EP)=M23(I,3,EP)
     .                +BF(EP,1,I,2)*CBR1(3,EP)+BF(EP,2,I,2)*CBR2(3,EP)
     .                +BF(EP,3,I,2)*CBR3(3,EP)
          M24(I,3,EP)=M24(I,3,EP)
     .                +BF(EP,1,I,2)*CBR1(4,EP)+BF(EP,2,I,2)*CBR2(4,EP)
     .                +BF(EP,3,I,2)*CBR3(4,EP)
          M34(I,3,EP)=M34(I,3,EP)
     .                +BF(EP,1,I,3)*CBR1(4,EP)+BF(EP,2,I,3)*CBR2(4,EP)
     .                +BF(EP,3,I,3)*CBR3(4,EP)
         ENDDO
         ENDDO
         DO I=1,2
         DO EP=NF,JLT
          M12(3,I,EP)=M12(3,I,EP)
     .                +BF(EP,1,I,2)*CBR1(1,EP)+BF(EP,2,I,2)*CBR2(1,EP)
     .                +BF(EP,3,I,2)*CBR3(1,EP)
          M13(3,I,EP)=M13(3,I,EP)
     .                +BF(EP,1,I,3)*CBR1(1,EP)+BF(EP,2,I,3)*CBR2(1,EP)
     .                +BF(EP,3,I,3)*CBR3(1,EP)
          M14(3,I,EP)=M14(3,I,EP)
     .                +BF(EP,1,I,4)*CBR1(1,EP)+BF(EP,2,I,4)*CBR2(1,EP)
     .                +BF(EP,3,I,4)*CBR3(1,EP)
          M23(3,I,EP)=M23(3,I,EP)
     .                +BF(EP,1,I,3)*CBR1(2,EP)+BF(EP,2,I,3)*CBR2(2,EP)
     .                +BF(EP,3,I,3)*CBR3(2,EP)
          M24(3,I,EP)=M24(3,I,EP)
     .                +BF(EP,1,I,4)*CBR1(2,EP)+BF(EP,2,I,4)*CBR2(2,EP)
     .                +BF(EP,3,I,4)*CBR3(2,EP)
          M34(3,I,EP)=M34(3,I,EP)
     .                +BF(EP,1,I,4)*CBR1(3,EP)+BF(EP,2,I,4)*CBR2(3,EP)
     .                +BF(EP,3,I,4)*CBR3(3,EP)
         ENDDO
         ENDDO
        DO I=1,3
         DO EP=NF,JLT
          MF11(I,3,EP)=MF11(I,3,EP) 
     .               +BMF(EP,1,I,1)*CBR1(1,EP)+BMF(EP,2,I,1)*CBR2(1,EP)
     .               +BMF(EP,3,I,1)*CBR3(1,EP)
          MF12(I,3,EP)=MF12(I,3,EP)
     .               +BMF(EP,1,I,1)*CBR1(2,EP)+BMF(EP,2,I,1)*CBR2(2,EP)
     .               +BMF(EP,3,I,1)*CBR3(2,EP)
          MF13(I,3,EP)=MF13(I,3,EP) 
     .               +BMF(EP,1,I,1)*CBR1(3,EP)+BMF(EP,2,I,1)*CBR2(3,EP)
     .               +BMF(EP,3,I,1)*CBR3(3,EP)
          MF14(I,3,EP)=MF14(I,3,EP) 
     .               +BMF(EP,1,I,1)*CBR1(4,EP)+BMF(EP,2,I,1)*CBR2(4,EP)
     .               +BMF(EP,3,I,1)*CBR3(4,EP)
          MF22(I,3,EP)=MF22(I,3,EP) 
     .               +BMF(EP,1,I,2)*CBR1(2,EP)+BMF(EP,2,I,2)*CBR2(2,EP)
     .               +BMF(EP,3,I,2)*CBR3(2,EP)
          MF23(I,3,EP)=MF23(I,3,EP) 
     .               +BMF(EP,1,I,2)*CBR1(3,EP)+BMF(EP,2,I,2)*CBR2(3,EP)
     .               +BMF(EP,3,I,2)*CBR3(3,EP)
          MF24(I,3,EP)=MF24(I,3,EP) 
     .               +BMF(EP,1,I,2)*CBR1(4,EP)+BMF(EP,2,I,2)*CBR2(4,EP)
     .               +BMF(EP,3,I,2)*CBR3(4,EP)
          MF33(I,3,EP)=MF33(I,3,EP) 
     .               +BMF(EP,1,I,3)*CBR1(3,EP)+BMF(EP,2,I,3)*CBR2(3,EP)
     .               +BMF(EP,3,I,3)*CBR3(3,EP)
          MF34(I,3,EP)=MF34(I,3,EP) 
     .               +BMF(EP,1,I,3)*CBR1(4,EP)+BMF(EP,2,I,3)*CBR2(4,EP)
     .               +BMF(EP,3,I,3)*CBR3(4,EP)
          MF44(I,3,EP)=MF44(I,3,EP) 
     .               +BMF(EP,1,I,4)*CBR1(4,EP)+BMF(EP,2,I,4)*CBR2(4,EP)
     .               +BMF(EP,3,I,4)*CBR3(4,EP)
C        
          FM12(3,I,EP)=FM12(3,I,EP) 
     .               +BMF(EP,1,I,2)*CBR1(1,EP)+BMF(EP,2,I,2)*CBR2(1,EP)
     .               +BMF(EP,3,I,2)*CBR3(1,EP)
          FM13(3,I,EP)=FM13(3,I,EP) 
     .               +BMF(EP,1,I,3)*CBR1(1,EP)+BMF(EP,2,I,3)*CBR2(1,EP)
     .               +BMF(EP,3,I,3)*CBR3(1,EP)
          FM14(3,I,EP)=FM14(3,I,EP) 
     .               +BMF(EP,1,I,4)*CBR1(1,EP)+BMF(EP,2,I,4)*CBR2(1,EP)
     .               +BMF(EP,3,I,4)*CBR3(1,EP)
          FM23(3,I,EP)=FM23(3,I,EP) 
     .               +BMF(EP,1,I,3)*CBR1(2,EP)+BMF(EP,2,I,3)*CBR2(2,EP)
     .               +BMF(EP,3,I,3)*CBR3(2,EP)
          FM24(3,I,EP)=FM24(3,I,EP) 
     .               +BMF(EP,1,I,4)*CBR1(2,EP)+BMF(EP,2,I,4)*CBR2(2,EP)
     .               +BMF(EP,3,I,4)*CBR3(2,EP)
          FM34(3,I,EP)=FM34(3,I,EP) 
     .               +BMF(EP,1,I,4)*CBR1(3,EP)+BMF(EP,2,I,4)*CBR2(3,EP)
     .               +BMF(EP,3,I,4)*CBR3(3,EP)
         ENDDO
        END DO !I=1,3
       END IF !(IORTH==1) THEN
C------ Geometrical stiffness associated w/ Rz  ----------
       IF (IKGEO==1) THEN
#include "vectorize.inc"
        DO M=JFT,JLT 
         EP=IPLAT(M)
         FRZV(M)=FRZ(EP,NG)*VOL(EP)
        ENDDO
        DO EP=JFT,JLT 
          T11(1,2,EP)=FRZV(EP)*(PRX(1,EP)*PRX(1,EP)+PRY(1,EP)*PRY(1,EP)+
     .                          PRXY(1,EP)*PRXY(1,EP))
          T22(1,2,EP)=FRZV(EP)*(PRX(2,EP)*PRX(2,EP)+PRY(2,EP)*PRY(2,EP)+
     .                          PRXY(2,EP)*PRXY(2,EP))
          T33(1,2,EP)=FRZV(EP)*(PRX(3,EP)*PRX(3,EP)+PRY(3,EP)*PRY(3,EP)+
     .                          PRXY(3,EP)*PRXY(3,EP))
          T44(1,2,EP)=FRZV(EP)*(PRX(4,EP)*PRX(4,EP)+PRY(4,EP)*PRY(4,EP)+
     .                          PRXY(4,EP)*PRXY(4,EP))
          T12(1,2,EP)=FRZV(EP)*(PRX(1,EP)*PRX(2,EP)+PRY(1,EP)*PRY(2,EP)+
     .                          PRXY(1,EP)*PRXY(2,EP))
          T13(1,2,EP)=FRZV(EP)*(PRX(1,EP)*PRX(3,EP)+PRY(1,EP)*PRY(3,EP)+
     .                          PRXY(1,EP)*PRXY(3,EP))
          T14(1,2,EP)=FRZV(EP)*(PRX(1,EP)*PRX(4,EP)+PRY(1,EP)*PRY(4,EP)+
     .                          PRXY(1,EP)*PRXY(4,EP))
          T23(1,2,EP)=FRZV(EP)*(PRX(2,EP)*PRX(3,EP)+PRY(2,EP)*PRY(3,EP)+
     .                          PRXY(2,EP)*PRXY(3,EP))
          T24(1,2,EP)=FRZV(EP)*(PRX(2,EP)*PRX(4,EP)+PRY(2,EP)*PRY(4,EP)+
     .                          PRXY(2,EP)*PRXY(4,EP))
          T34(1,2,EP)=FRZV(EP)*(PRX(3,EP)*PRX(4,EP)+PRY(3,EP)*PRY(4,EP)+
     .                          PRXY(3,EP)*PRXY(4,EP))
        ENDDO
C
        DO I=1,3
         DO EP=JFT,JLT 
          K11(I,I,EP)=K11(I,I,EP)+T11(1,2,EP)
          K22(I,I,EP)=K22(I,I,EP)+T22(1,2,EP)
          K33(I,I,EP)=K33(I,I,EP)+T33(1,2,EP)
          K44(I,I,EP)=K44(I,I,EP)+T44(1,2,EP) 
          K12(I,I,EP)=K12(I,I,EP)+T12(1,2,EP) 
          K13(I,I,EP)=K13(I,I,EP)+T13(1,2,EP) 
          K14(I,I,EP)=K14(I,I,EP)+T14(1,2,EP) 
          K23(I,I,EP)=K23(I,I,EP)+T23(1,2,EP) 
          K24(I,I,EP)=K24(I,I,EP)+T24(1,2,EP) 
          K34(I,I,EP)=K34(I,I,EP)+T34(1,2,EP) 
         ENDDO
        ENDDO
       ENDIF 
C
       RETURN
       END
Chd|====================================================================
Chd|  CBALKEORMF                    source/elements/shell/coqueba/cbalke3.F
Chd|-- called by -----------
Chd|        CBALKE3                       source/elements/shell/coqueba/cbalke3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CBALKEORMF(JFT    ,JLT   ,KMFIJ    ,DMF   ,TIJ   )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
      INTEGER JFT,JLT
      my_real
     .    KMFIJ(3,3,*),TIJ(2,2,*),DMF(3,3,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER EP,I,J
      my_real
     .    MFIJ(2,2,MVSIZ)
C-------MFIJ=[BmI]^t[DMF][BbJ] ;TIJ(i,j)=B(i,I)*B(j,J)----i,j->1:x;2:y    
        DO EP=JFT,JLT 
         MFIJ(1,1,EP)=
     1               -DMF(1,2,EP)*TIJ(1,2,EP)-DMF(1,3,EP)*TIJ(1,1,EP)
     2               -DMF(3,2,EP)*TIJ(2,2,EP)-DMF(3,3,EP)*TIJ(2,1,EP)
         MFIJ(1,2,EP)=
     1               +DMF(1,1,EP)*TIJ(1,1,EP)+DMF(1,3,EP)*TIJ(1,2,EP)
     2               +DMF(3,1,EP)*TIJ(2,1,EP)+DMF(3,3,EP)*TIJ(2,2,EP)
         MFIJ(2,1,EP)=
     1               -DMF(2,2,EP)*TIJ(2,2,EP)-DMF(2,3,EP)*TIJ(2,1,EP)
     2               -DMF(3,2,EP)*TIJ(1,2,EP)-DMF(3,3,EP)*TIJ(1,1,EP)
         MFIJ(2,2,EP)=
     1               +DMF(2,1,EP)*TIJ(2,1,EP)+DMF(2,3,EP)*TIJ(2,2,EP)
     2               +DMF(3,1,EP)*TIJ(1,1,EP)+DMF(3,3,EP)*TIJ(1,2,EP)
        ENDDO
C        
       DO I=1,2
       DO J=1,2
        DO EP=JFT,JLT
         KMFIJ(I,J,EP)=KMFIJ(I,J,EP)+MFIJ(I,J,EP)
        ENDDO
       ENDDO
       ENDDO
C
       RETURN
       END
Chd|====================================================================
Chd|  CBALKEORFM                    source/elements/shell/coqueba/cbalke3.F
Chd|-- called by -----------
Chd|        CBALKE3                       source/elements/shell/coqueba/cbalke3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CBALKEORFM(JFT    ,JLT   ,KFMIJ    ,DMF   ,TIJ   )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
      INTEGER JFT,JLT
      my_real
     .    KFMIJ(3,3,*),TIJ(2,2,*),DMF(3,3,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER EP,I,J
      my_real
     .    FMIJ(2,2,MVSIZ)
C-------FMIJ=[BbI]^t[DMF][BmJ]=MFJI^t;TIJ(i,j)=B(i,I)*B(j,J)--TJI(i,j)=B(i,J)*B(j,I) 
C-------I<J->TJI(i,j)=TIJ(j,i)
        DO EP=JFT,JLT 
         FMIJ(1,1,EP)=
     1               -DMF(1,2,EP)*TIJ(2,1,EP)-DMF(1,3,EP)*TIJ(1,1,EP)
     2               -DMF(3,2,EP)*TIJ(2,2,EP)-DMF(3,3,EP)*TIJ(1,2,EP)
         FMIJ(2,1,EP)=
     1               +DMF(1,1,EP)*TIJ(1,1,EP)+DMF(1,3,EP)*TIJ(2,1,EP)
     2               +DMF(3,1,EP)*TIJ(1,2,EP)+DMF(3,3,EP)*TIJ(2,2,EP)
         FMIJ(1,2,EP)=
     1               -DMF(2,2,EP)*TIJ(2,2,EP)-DMF(2,3,EP)*TIJ(1,2,EP)
     2               -DMF(3,2,EP)*TIJ(2,1,EP)-DMF(3,3,EP)*TIJ(1,1,EP)
         FMIJ(2,2,EP)=
     1               +DMF(2,1,EP)*TIJ(1,2,EP)+DMF(2,3,EP)*TIJ(2,2,EP)
     2               +DMF(3,1,EP)*TIJ(1,1,EP)+DMF(3,3,EP)*TIJ(2,1,EP)
        ENDDO
C        
       DO I=1,2
       DO J=1,2
        DO EP=JFT,JLT
         KFMIJ(I,J,EP)=KFMIJ(I,J,EP)+FMIJ(I,J,EP)
        ENDDO
       ENDDO
       ENDDO
C
       RETURN
       END
Chd|====================================================================
Chd|  CBALKEORMF1                   source/elements/shell/coqueba/cbalke3.F
Chd|-- called by -----------
Chd|        CBALKE3                       source/elements/shell/coqueba/cbalke3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CBALKEORMF1(JFT  ,JLT  ,KMFIJ  ,DMF   ,TIJ  ,T0IJ )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
      INTEGER JFT,JLT
      my_real
     .    KMFIJ(3,3,*),TIJ(2,2,*),DMF(3,3,*),T0IJ(2,2,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER EP,I,J
      my_real
     .    MFIJ(2,2,MVSIZ)
C----cas Idril=0:MFIJ=[BmI]^t[DMF][BbJ] ;TIJ(i,j)=B(i,I)*B(j,J)----i,j->1:x;2:y    
        DO EP=JFT,JLT 
         MFIJ(1,1,EP)=
     1               -DMF(1,2,EP)*TIJ(1,2,EP)-DMF(1,3,EP)*TIJ(1,1,EP)
     2               -DMF(3,2,EP)*T0IJ(2,2,EP)-DMF(3,3,EP)*T0IJ(2,1,EP)
         MFIJ(1,2,EP)=
     1               +DMF(1,1,EP)*TIJ(1,1,EP)+DMF(1,3,EP)*TIJ(1,2,EP)
     2               +DMF(3,1,EP)*T0IJ(2,1,EP)+DMF(3,3,EP)*T0IJ(2,2,EP)
         MFIJ(2,1,EP)=
     1               -DMF(2,2,EP)*TIJ(2,2,EP)-DMF(2,3,EP)*TIJ(2,1,EP)
     2               -DMF(3,2,EP)*T0IJ(1,2,EP)-DMF(3,3,EP)*T0IJ(1,1,EP)
         MFIJ(2,2,EP)=
     1               +DMF(2,1,EP)*TIJ(2,1,EP)+DMF(2,3,EP)*TIJ(2,2,EP)
     2               +DMF(3,1,EP)*T0IJ(1,1,EP)+DMF(3,3,EP)*T0IJ(1,2,EP)
        ENDDO
C        
       DO I=1,2
       DO J=1,2
        DO EP=JFT,JLT
         KMFIJ(I,J,EP)=KMFIJ(I,J,EP)+MFIJ(I,J,EP)
        ENDDO
       ENDDO
       ENDDO
C
       RETURN
       END
Chd|====================================================================
Chd|  CBALKEORFM1                   source/elements/shell/coqueba/cbalke3.F
Chd|-- called by -----------
Chd|        CBALKE3                       source/elements/shell/coqueba/cbalke3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CBALKEORFM1(JFT  ,JLT  ,KFMIJ  ,DMF  ,TIJ  ,T0IJ )
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
      INTEGER JFT,JLT
      my_real
     .    KFMIJ(3,3,*),TIJ(2,2,*),DMF(3,3,*),T0IJ(2,2,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER EP,I,J
      my_real
     .    FMIJ(2,2,MVSIZ)
C-------FMIJ=[BbI]^t[DMF][BmJ]=MFJI^t;TIJ(i,j)=B(i,I)*B(j,J)--TJI(i,j)=B(i,J)*B(j,I) 
C-------I<J->TJI(i,j)=TIJ(j,i)
        DO EP=JFT,JLT 
         FMIJ(1,1,EP)=
     1               -DMF(1,2,EP)*TIJ(2,1,EP)-DMF(1,3,EP)*TIJ(1,1,EP)
     2               -DMF(3,2,EP)*T0IJ(2,2,EP)-DMF(3,3,EP)*T0IJ(1,2,EP)
         FMIJ(2,1,EP)=
     1               +DMF(1,1,EP)*TIJ(1,1,EP)+DMF(1,3,EP)*TIJ(2,1,EP)
     2               +DMF(3,1,EP)*T0IJ(1,2,EP)+DMF(3,3,EP)*T0IJ(2,2,EP)
         FMIJ(1,2,EP)=
     1               -DMF(2,2,EP)*TIJ(2,2,EP)-DMF(2,3,EP)*TIJ(1,2,EP)
     2               -DMF(3,2,EP)*T0IJ(2,1,EP)-DMF(3,3,EP)*T0IJ(1,1,EP)
         FMIJ(2,2,EP)=
     1               +DMF(2,1,EP)*TIJ(1,2,EP)+DMF(2,3,EP)*TIJ(2,2,EP)
     2               +DMF(3,1,EP)*T0IJ(1,1,EP)+DMF(3,3,EP)*T0IJ(2,1,EP)
        ENDDO
C        
       DO I=1,2
       DO J=1,2
        DO EP=JFT,JLT
         KFMIJ(I,J,EP)=KFMIJ(I,J,EP)+FMIJ(I,J,EP)
        ENDDO
       ENDDO
       ENDDO
C
       RETURN
       END
       
