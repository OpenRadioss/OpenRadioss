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
Chd|  CZLKECG3                      source/elements/shell/coquez/czlkecg3.F
Chd|-- called by -----------
Chd|        CZKE3                         source/elements/shell/coquez/czke3.F
Chd|        CZKEL3                        source/elements/shell/coquez/czkel3.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE CZLKECG3(JFT ,JLT   ,VOL  ,THK0 ,THK2 ,
     1                      PX1  ,PX2  ,PY1   ,PY2  ,RX   ,
     2                      RY   ,SX    ,SY   ,RX2  ,RY2  ,
     3                      SX2  ,SY2   ,RHX  ,RHY  ,SHX  ,
     4                      SHY  ,NPLAT ,IPLAT,FOR  ,MOM  ,
     5                      K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     6                      M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     7                      MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33,
     8                      MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34,
     9                      IDRIL,IORTH,NEL)
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
      INTEGER JFT,JLT,NPLAT,IPLAT(*),IDRIL,IORTH,NEL
      MY_REAL
     .   VOL(*),THK0(*),THK2(*),
     .    RX(*)   ,RY(*)   ,SX(*)    ,SY(*)   ,
     .    RX2(*)  ,RY2(*)  ,SX2(*)  ,SY2(*) ,FOR(NEL,5),MOM(NEL,3),
     .    RHX(MVSIZ,4)  ,RHY(MVSIZ,4)  ,SHX(MVSIZ,4)  ,SHY(MVSIZ,4) ,
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
     .    MF34(3,3,*),MF44(3,3,*)
C---------------|[KIJ][MFIJ]|----
C-----KE(6x6)=  |           |
C---------------|[FMIJ]{MIJ]|----
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER EP,I,J,NF,M
      MY_REAL
     .    C1,C2,
     .    PX1PX1(MVSIZ),PX1PX2(MVSIZ) ,PX2PX2(MVSIZ),
     .    PX1PY1(MVSIZ),PX1PY2(MVSIZ) ,PX2PY1(MVSIZ),
     .    PX2PY2(MVSIZ),PY1PY1(MVSIZ),PY1PY2(MVSIZ),PY2PY2(MVSIZ),
     .    H11(MVSIZ),H12(MVSIZ),H22(MVSIZ),H13(MVSIZ),H23(MVSIZ),
     .    FXX(MVSIZ),FYY(MVSIZ),FXY(MVSIZ),MXX(MVSIZ),MYY(MVSIZ),
     .    MXY(MVSIZ),FXY2(MVSIZ),MXY2(MVSIZ),SCX(MVSIZ),SCY(MVSIZ),
     .    CS1(MVSIZ),CS2(MVSIZ),CS3(MVSIZ)
C-----------Attention Matrice sym Kii ne calcul que la moitie---------72
       NF=NPLAT+1
#include "vectorize.inc"
C--------Constante parts---------------
       DO M=JFT,JLT
        EP=IPLAT(M)
        C2=VOL(EP)
        C1=THK2(EP)*C2
        FXX(M)=FOR(EP,1)*C2
        FYY(M)=FOR(EP,2)*C2
        FXY(M)=FOR(EP,3)*C2
        FXY2(M)=TWO*FXY(M)
        MXX(M)=MOM(EP,1)*C1
        MYY(M)=MOM(EP,2)*C1
        MXY(M)=MOM(EP,3)*C1
        MXY2(M)=TWO*MXY(M)
        SCX(M)=FOR(EP,5)*C2
        SCY(M)=FOR(EP,4)*C2
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
         CS1(EP) = SCX(EP)*SY2(EP)+SCY(EP)*SX2(EP)
         CS2(EP) = SCX(EP)*RY2(EP)+SCY(EP)*RX2(EP)
         CS3(EP) =-SCX(EP)*SY(EP)*RY(EP)-SCY(EP)*SX(EP)*RX(EP)
       ENDDO
C---------membrane-------
        DO EP=JFT,JLT
         H11(EP)=FXX(EP)*PX1PX1(EP)+FYY(EP)*PY1PY1(EP)
     .                        +FXY2(EP)*PX1PY1(EP)
         H12(EP)=FXX(EP)*PX1PX2(EP)+FYY(EP)*PY1PY2(EP)
     .        +FXY(EP)*(PX1PY2(EP)+PX2PY1(EP))
         H22(EP)=FXX(EP)*PX2PX2(EP)+FYY(EP)*PY2PY2(EP)
     .                        +FXY2(EP)*PX2PY2(EP)
        ENDDO
       DO I=1,3
        DO EP=JFT,JLT
         K11(I,I,EP) = K11(I,I,EP)+H11(EP)
         K12(I,I,EP) = K12(I,I,EP)+H12(EP)
         K22(I,I,EP) = K22(I,I,EP)+H22(EP)
         K13(I,I,EP) = K13(I,I,EP)-H11(EP)
         K14(I,I,EP) = K14(I,I,EP)-H12(EP)
         K23(I,I,EP) = K23(I,I,EP)-H12(EP)
         K24(I,I,EP) = K24(I,I,EP)-H22(EP)
         K33(I,I,EP) = K33(I,I,EP)+H11(EP)
         K34(I,I,EP) = K34(I,I,EP)+H12(EP)
         K44(I,I,EP) = K44(I,I,EP)+H22(EP)
        ENDDO
       ENDDO
C------ renforce positive  ----------
        IF (NEIG==0.AND.IDRIL==0.AND.IORTH >0) THEN
         DO EP=JFT,JLT
          C1 = MIN(H11(EP),H22(EP),-H11(EP),-H22(EP))
          IF (C1 < ZERO) THEN
           C2 =MIN(M11(3,3,EP),M22(3,3,EP),M33(3,3,EP),M44(3,3,EP))
           C1 = MIN(-C1,TEN*C2)
           M11(3,3,EP)=M11(3,3,EP) + C1
           M22(3,3,EP)=M22(3,3,EP) + C1
           M33(3,3,EP)=M33(3,3,EP) + C1
           M44(3,3,EP)=M44(3,3,EP) + C1
          END IF
         ENDDO
        ENDIF
C  tmp+1
       RETURN
C---------flexion---------
        DO EP=JFT,JLT
         H11(EP)=MXX(EP)*PY1PY1(EP)+MYY(EP)*PX1PX1(EP)
     .                         -MXY2(EP)*PX1PY1(EP)
         H12(EP)=MXX(EP)*PY1PY2(EP)+MYY(EP)*PX1PX2(EP)
     .        -MXY(EP)*(PX2PY1(EP)+PX1PY2(EP))
         H22(EP)=MXX(EP)*PY2PY2(EP)+MYY(EP)*PX2PX2(EP)
     .                        -MXY2(EP)*PX2PY2(EP)
        ENDDO
       DO I=1,2
        DO EP=JFT,JLT
         M11(I,I,EP) = M11(I,I,EP)+H11(EP)
         M12(I,I,EP) = M12(I,I,EP)+H12(EP)
         M22(I,I,EP) = M22(I,I,EP)+H22(EP)
         M13(I,I,EP) = M13(I,I,EP)-H11(EP)
         M14(I,I,EP) = M14(I,I,EP)-H12(EP)
         M23(I,I,EP) = M23(I,I,EP)-H12(EP)
         M24(I,I,EP) = M24(I,I,EP)-H22(EP)
         M33(I,I,EP) = M33(I,I,EP)+H11(EP)
         M34(I,I,EP) = M34(I,I,EP)+H12(EP)
         M44(I,I,EP) = M44(I,I,EP)+H22(EP)
        ENDDO
       ENDDO
C---------c.t---------
        DO EP=JFT,JLT
         H11(EP)=CS1(EP)+CS2(EP)+CS3(EP)+CS3(EP)
         H22(EP)=CS1(EP)+CS2(EP)-CS3(EP)-CS3(EP)
         H12(EP)=-CS1(EP)+CS2(EP)+CS3(EP)-CS3(EP)
         H13(EP)=-CS1(EP)-CS2(EP)-CS3(EP)-CS3(EP)
         H23(EP)= CS1(EP)-CS2(EP)+CS3(EP)-CS3(EP)
        ENDDO
       DO I=1,3
        DO EP=JFT,JLT
         K11(I,I,EP) = K11(I,I,EP)+H11(EP)
         K12(I,I,EP) = K12(I,I,EP)+H12(EP)
         K22(I,I,EP) = K22(I,I,EP)+H22(EP)
         K13(I,I,EP) = K13(I,I,EP)+H13(EP)
         K14(I,I,EP) = K14(I,I,EP)-H12(EP)
         K23(I,I,EP) = K23(I,I,EP)+H23(EP)
         K24(I,I,EP) = K24(I,I,EP)-H22(EP)
         K33(I,I,EP) = K33(I,I,EP)+H11(EP)
         K34(I,I,EP) = K34(I,I,EP)+H12(EP)
         K44(I,I,EP) = K44(I,I,EP)+H22(EP)
        ENDDO
       ENDDO
C
C---------------KC1-----------
        DO EP=JFT,JLT
C--------11: 11,12,22 ------
         H11(EP)=(RHY(EP,1)*RHY(EP,1)+RHX(EP,1)*RHX(EP,1))*CS1(EP)
C------12: 13,14,23,24 ------
         H12(EP)=(RHY(EP,1)*RHY(EP,3)+RHX(EP,1)*RHX(EP,3))*CS1(EP)
C------13: 33,34,44 ------
         H13(EP)=(RHY(EP,3)*RHY(EP,3)+RHX(EP,3)*RHX(EP,3))*CS1(EP)
        ENDDO
       DO I=1,2
        DO EP=JFT,JLT
         M11(I,I,EP) = M11(I,I,EP)+H11(EP)
         M12(I,I,EP) = M12(I,I,EP)+H11(EP)
         M22(I,I,EP) = M22(I,I,EP)+H11(EP)
         M13(I,I,EP) = M13(I,I,EP)+H12(EP)
         M14(I,I,EP) = M14(I,I,EP)+H12(EP)
         M23(I,I,EP) = M23(I,I,EP)+H12(EP)
         M24(I,I,EP) = M24(I,I,EP)+H12(EP)
         M33(I,I,EP) = M33(I,I,EP)+H13(EP)
         M34(I,I,EP) = M34(I,I,EP)+H13(EP)
         M44(I,I,EP) = M44(I,I,EP)+H13(EP)
        ENDDO
       ENDDO
        DO EP=JFT,JLT
C---------------KC2-----------
C--------11: 11,14,44 ------
         H11(EP)=(SHY(EP,1)*SHY(EP,1)+SHX(EP,1)*SHX(EP,1))*CS2(EP)
C------12: 12,13,24,34 ------
         H12(EP)=(SHY(EP,1)*SHY(EP,2)+SHX(EP,1)*SHX(EP,2))*CS2(EP)
C------22: 22,23,33 ------
         H22(EP)=(SHY(EP,2)*SHY(EP,2)+SHX(EP,2)*SHX(EP,2))*CS2(EP)
        ENDDO
       DO I=1,2
        DO EP=JFT,JLT
         M11(I,I,EP) = M11(I,I,EP)+H11(EP)
         M12(I,I,EP) = M12(I,I,EP)+H12(EP)
         M22(I,I,EP) = M22(I,I,EP)+H22(EP)
         M13(I,I,EP) = M13(I,I,EP)+H12(EP)
         M14(I,I,EP) = M14(I,I,EP)+H11(EP)
         M23(I,I,EP) = M23(I,I,EP)+H22(EP)
         M24(I,I,EP) = M24(I,I,EP)+H12(EP)
         M33(I,I,EP) = M33(I,I,EP)+H22(EP)
         M34(I,I,EP) = M34(I,I,EP)+H12(EP)
         M44(I,I,EP) = M44(I,I,EP)+H11(EP)
        ENDDO
       ENDDO
C---------------KC2-----------
        DO EP=JFT,JLT
         H11(EP)=(RHY(EP,1)*SHY(EP,1)+RHY(EP,1)*SHY(EP,1)+
     .        RHX(EP,1)*SHX(EP,1)+RHX(EP,1)*SHX(EP,1))*CS3(EP)
         H12(EP)=(RHY(EP,1)*SHY(EP,2)+RHY(EP,2)*SHY(EP,1)+
     .        RHX(EP,1)*SHX(EP,2)+RHX(EP,2)*SHX(EP,1))*CS3(EP)
         H22(EP)=(RHY(EP,2)*SHY(EP,2)+RHY(EP,2)*SHY(EP,2)+
     .        RHX(EP,2)*SHX(EP,2)+RHX(EP,2)*SHX(EP,2))*CS3(EP)
         H13(EP)=(RHY(EP,1)*SHY(EP,3)+RHY(EP,3)*SHY(EP,1)+
     .        RHX(EP,1)*SHX(EP,3)+RHX(EP,3)*SHX(EP,1))*CS3(EP)
         H23(EP)=(RHY(EP,2)*SHY(EP,3)+RHY(EP,3)*SHY(EP,2)+
     .        RHX(EP,2)*SHX(EP,3)+RHX(EP,3)*SHX(EP,2))*CS3(EP)
        ENDDO
       DO I=1,2
        DO EP=JFT,JLT
         M11(I,I,EP) = M11(I,I,EP)+H11(EP)
         M12(I,I,EP) = M12(I,I,EP)+H12(EP)
         M22(I,I,EP) = M22(I,I,EP)+H22(EP)
         M13(I,I,EP) = M13(I,I,EP)+H12(EP)
         M23(I,I,EP) = M23(I,I,EP)+H22(EP)
        ENDDO
       ENDDO
C---------11:14 12:24 22:33 13:34 23:44----------
        DO EP=JFT,JLT
         H11(EP)=(RHY(EP,1)*SHY(EP,4)+RHY(EP,4)*SHY(EP,1)+
     .        RHX(EP,1)*SHX(EP,4)+RHX(EP,4)*SHX(EP,1))*CS3(EP)
         H12(EP)=(RHY(EP,2)*SHY(EP,4)+RHY(EP,4)*SHY(EP,2)+
     .        RHX(EP,2)*SHX(EP,4)+RHX(EP,4)*SHX(EP,2))*CS3(EP)
         H22(EP)=(RHY(EP,3)*SHY(EP,3)+RHY(EP,3)*SHY(EP,3)+
     .        RHX(EP,3)*SHX(EP,3)+RHX(EP,3)*SHX(EP,3))*CS3(EP)
         H13(EP)=(RHY(EP,3)*SHY(EP,4)+RHY(EP,4)*SHY(EP,3)+
     .        RHX(EP,3)*SHX(EP,4)+RHX(EP,4)*SHX(EP,3))*CS3(EP)
         H23(EP)=(RHY(EP,4)*SHY(EP,4)+RHY(EP,4)*SHY(EP,4)+
     .        RHX(EP,4)*SHX(EP,4)+RHX(EP,4)*SHX(EP,4))*CS3(EP)
        ENDDO
       DO I=1,2
        DO EP=JFT,JLT
         M14(I,I,EP) = M14(I,I,EP)+H11(EP)
         M24(I,I,EP) = M24(I,I,EP)+H12(EP)
         M33(I,I,EP) = M33(I,I,EP)+H22(EP)
         M34(I,I,EP) = M34(I,I,EP)+H12(EP)
         M44(I,I,EP) = M44(I,I,EP)+H22(EP)
        ENDDO
       ENDDO
C
       RETURN
       END
