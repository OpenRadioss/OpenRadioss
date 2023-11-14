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
Chd|  CBASUMG3                      source/elements/shell/coqueba/cbasumg3.F
Chd|-- called by -----------
Chd|        CBAKE3                        source/elements/shell/coqueba/cbake3.F
Chd|-- calls ---------------
Chd|        CBAPROJK                      source/elements/shell/coqueba/cbasumg3.F
Chd|        CBATRAN2                      source/elements/shell/coqueba/cbasumg3.F
Chd|        CBATRAN223                    source/elements/shell/coqueba/cbasumg3.F
Chd|        CBATRAN232                    source/elements/shell/coqueba/cbasumg3.F
Chd|        CBATRAN233                    source/elements/shell/coqueba/cbasumg3.F
Chd|        CBATRAN3                      source/elements/shell/coqueba/cbasumg3.F
Chd|====================================================================
      SUBROUTINE CBASUMG3(
     1            JFT    ,JLT    ,VQN    ,VQ     ,NPLAT   ,IPLAT   ,
     2                     K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     3                     M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     4                     MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33, 
     5                     MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34, 
     6                     KE11,KE22,KE33,KE44,KE12,KE13,KE14,KE23,
     7                     KE24,KE34,VCORE,IDRIL,IORTH)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,NPLAT ,IPLAT(*),IDRIL,IORTH
      my_real 
     .   VQN(MVSIZ,3,3,4),VQ(MVSIZ,3,3)
      my_real
     .    K11(3,3,*),K12(3,3,*),K13(3,3,*),K14(3,3,*),
     .    K22(3,3,*),K23(3,3,*),K24(3,3,*),K33(3,3,*),
     .    M11(3,3,*),M12(3,3,*),M13(3,3,*),M14(3,3,*),
     .    M22(3,3,*),M23(3,3,*),M24(3,3,*),M33(3,3,*),
     .    MF11(3,3,*),MF12(3,3,*),MF13(3,3,*),MF14(3,3,*),
     .    MF22(3,3,*),MF23(3,3,*),MF24(3,3,*),MF33(3,3,*),
     .    FM12(3,3,*),FM13(3,3,*),FM14(3,3,*),
     .    FM23(3,3,*),FM24(3,3,*),FM34(3,3,*),
     .    K34(3,3,*),K44(3,3,*),M34(3,3,*),M44(3,3,*),
     .    MF34(3,3,*),MF44(3,3,*),
     .    KE11(6,6,*),KE22(6,6,*),KE33(6,6,*),KE44(6,6,*),
     .    KE12(6,6,*),KE13(6,6,*),KE14(6,6,*),KE23(6,6,*),
     .    KE24(6,6,*),KE34(6,6,*),VCORE(MVSIZ,3,4)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I, J, K,EP,IS,IAS,NF,MI,MJ,M
      my_real 
     .     Q(3,3,MVSIZ),QN(3,3,4,MVSIZ),CORELV(MVSIZ,2,4),Z1(MVSIZ)
      DATA IS/1/,IAS/0/ 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C---------------------------------------
C   TRANS LOCAL-->GLOBAL ET 5DDL-->6DDL w/  rigid mode projection
C---------------------------------------
        DO I=1,3 
        DO J=1,3 
#include "vectorize.inc"
        DO M=JFT,NPLAT
         EP=IPLAT(M)
         Q(J,I,M)=VQ(EP,I,J)
        ENDDO
       ENDDO
       ENDDO
        CALL CBATRAN2(JFT,NPLAT,Q,K11,Q,IS)
        CALL CBATRAN2(JFT,NPLAT,Q,K22,Q,IS)
        CALL CBATRAN2(JFT,NPLAT,Q,K33,Q,IS)
        CALL CBATRAN2(JFT,NPLAT,Q,K44,Q,IS)
        CALL CBATRAN2(JFT,NPLAT,Q,K12,Q,IAS)
        CALL CBATRAN2(JFT,NPLAT,Q,K13,Q,IAS)
        CALL CBATRAN2(JFT,NPLAT,Q,K14,Q,IAS)
        CALL CBATRAN2(JFT,NPLAT,Q,K23,Q,IAS)
        CALL CBATRAN2(JFT,NPLAT,Q,K24,Q,IAS)
        CALL CBATRAN2(JFT,NPLAT,Q,K34,Q,IAS)
       IF (IORTH >0 .AND.IDRIL>0) THEN
        CALL CBATRAN3(JFT,NPLAT,Q,M11,Q,IS)
        CALL CBATRAN3(JFT,NPLAT,Q,M22,Q,IS)
        CALL CBATRAN3(JFT,NPLAT,Q,M33,Q,IS)
        CALL CBATRAN3(JFT,NPLAT,Q,M44,Q,IS)
        CALL CBATRAN3(JFT,NPLAT,Q,M12,Q,IAS)
        CALL CBATRAN3(JFT,NPLAT,Q,M13,Q,IAS)
        CALL CBATRAN3(JFT,NPLAT,Q,M14,Q,IAS)
        CALL CBATRAN3(JFT,NPLAT,Q,M23,Q,IAS)
        CALL CBATRAN3(JFT,NPLAT,Q,M24,Q,IAS)
        CALL CBATRAN3(JFT,NPLAT,Q,M34,Q,IAS)
       ELSE
        CALL CBATRAN2(JFT,NPLAT,Q,M11,Q,IS)
        CALL CBATRAN2(JFT,NPLAT,Q,M22,Q,IS)
        CALL CBATRAN2(JFT,NPLAT,Q,M33,Q,IS)
        CALL CBATRAN2(JFT,NPLAT,Q,M44,Q,IS)
        CALL CBATRAN2(JFT,NPLAT,Q,M12,Q,IAS)
        CALL CBATRAN2(JFT,NPLAT,Q,M13,Q,IAS)
        CALL CBATRAN2(JFT,NPLAT,Q,M14,Q,IAS)
        CALL CBATRAN2(JFT,NPLAT,Q,M23,Q,IAS)
        CALL CBATRAN2(JFT,NPLAT,Q,M24,Q,IAS)
        CALL CBATRAN2(JFT,NPLAT,Q,M34,Q,IAS)
       END IF !(IORTH >0.AND.IDRIL>0)
C        
       IF (IORTH >0) THEN
        CALL CBATRAN3(JFT,NPLAT,Q,MF11,Q,IAS)
        CALL CBATRAN3(JFT,NPLAT,Q,MF12,Q,IAS)
        CALL CBATRAN3(JFT,NPLAT,Q,MF13,Q,IAS)
        CALL CBATRAN3(JFT,NPLAT,Q,MF14,Q,IAS)
        CALL CBATRAN3(JFT,NPLAT,Q,MF22,Q,IAS)
        CALL CBATRAN3(JFT,NPLAT,Q,MF23,Q,IAS)
        CALL CBATRAN3(JFT,NPLAT,Q,MF24,Q,IAS)
        CALL CBATRAN3(JFT,NPLAT,Q,MF33,Q,IAS)
        CALL CBATRAN3(JFT,NPLAT,Q,MF34,Q,IAS)
        CALL CBATRAN3(JFT,NPLAT,Q,MF44,Q,IAS)
        CALL CBATRAN3(JFT,NPLAT,Q,FM12,Q,IAS)
        CALL CBATRAN3(JFT,NPLAT,Q,FM13,Q,IAS)
        CALL CBATRAN3(JFT,NPLAT,Q,FM14,Q,IAS)
        CALL CBATRAN3(JFT,NPLAT,Q,FM23,Q,IAS)
        CALL CBATRAN3(JFT,NPLAT,Q,FM24,Q,IAS)
        CALL CBATRAN3(JFT,NPLAT,Q,FM34,Q,IAS)
       ELSEIF (IDRIL>0) THEN
        CALL CBATRAN233(JFT,NPLAT,Q,MF11,Q)
        CALL CBATRAN233(JFT,NPLAT,Q,MF12,Q)
        CALL CBATRAN233(JFT,NPLAT,Q,MF13,Q)
        CALL CBATRAN233(JFT,NPLAT,Q,MF14,Q)
        CALL CBATRAN233(JFT,NPLAT,Q,MF22,Q)
        CALL CBATRAN233(JFT,NPLAT,Q,MF23,Q)
        CALL CBATRAN233(JFT,NPLAT,Q,MF24,Q)
        CALL CBATRAN233(JFT,NPLAT,Q,MF33,Q)
        CALL CBATRAN233(JFT,NPLAT,Q,MF34,Q)
        CALL CBATRAN233(JFT,NPLAT,Q,MF44,Q)
        CALL CBATRAN233(JFT,NPLAT,Q,FM12,Q)
        CALL CBATRAN233(JFT,NPLAT,Q,FM13,Q)
        CALL CBATRAN233(JFT,NPLAT,Q,FM14,Q)
        CALL CBATRAN233(JFT,NPLAT,Q,FM23,Q)
        CALL CBATRAN233(JFT,NPLAT,Q,FM24,Q)
        CALL CBATRAN233(JFT,NPLAT,Q,FM34,Q)
       ELSE
        CALL CBATRAN232(JFT,NPLAT,Q,MF11,Q)
        CALL CBATRAN232(JFT,NPLAT,Q,MF12,Q)
        CALL CBATRAN232(JFT,NPLAT,Q,MF13,Q)
        CALL CBATRAN232(JFT,NPLAT,Q,MF14,Q)
        CALL CBATRAN232(JFT,NPLAT,Q,MF22,Q)
        CALL CBATRAN232(JFT,NPLAT,Q,MF23,Q)
        CALL CBATRAN232(JFT,NPLAT,Q,MF24,Q)
        CALL CBATRAN232(JFT,NPLAT,Q,MF33,Q)
        CALL CBATRAN232(JFT,NPLAT,Q,MF34,Q)
        CALL CBATRAN232(JFT,NPLAT,Q,MF44,Q)
        CALL CBATRAN223(JFT,NPLAT,Q,FM12,Q)
        CALL CBATRAN223(JFT,NPLAT,Q,FM13,Q)
        CALL CBATRAN223(JFT,NPLAT,Q,FM14,Q)
        CALL CBATRAN223(JFT,NPLAT,Q,FM23,Q)
        CALL CBATRAN223(JFT,NPLAT,Q,FM24,Q)
        CALL CBATRAN223(JFT,NPLAT,Q,FM34,Q)
       END IF
C---------------------------------------
C   ASSEMBLAGE
C---------------------------------------
C---------KII -------- 
       DO I=1,3 
        MI=I+3
        DO J=I,3 
         MJ=J+3
#include "vectorize.inc"
         DO M=JFT,NPLAT
          EP=IPLAT(M)
          KE11(I,J,EP)=K11(I,J,M) 
          KE11(MI,MJ,EP)=M11(I,J,M)
          KE22(I,J,EP)=K22(I,J,M)
          KE22(MI,MJ,EP)=M22(I,J,M)
          KE33(I,J,EP)=K33(I,J,M)
          KE33(MI,MJ,EP)=M33(I,J,M)
          KE44(I,J,EP)=K44(I,J,M)
          KE44(MI,MJ,EP)=M44(I,J,M)
         ENDDO
        ENDDO
       ENDDO
C
       DO I=1,3 
        DO J=1,3 
         MJ=J+3
#include "vectorize.inc"
         DO M=JFT,NPLAT
          EP=IPLAT(M)
          KE11(I,MJ,EP)=MF11(I,J,M)
          KE22(I,MJ,EP)=MF22(I,J,M)
          KE33(I,MJ,EP)=MF33(I,J,M)
          KE44(I,MJ,EP)=MF44(I,J,M)
         ENDDO
        ENDDO
       ENDDO
C
C---------KIJ -------- 
       DO I=1,3 
        MI=I+3
        DO J=1,3 
         MJ=J+3
#include "vectorize.inc"
         DO M=JFT,NPLAT
          EP=IPLAT(M)
          KE12(I,J,EP)=K12(I,J,M)
          KE12(I,MJ,EP)=MF12(I,J,M)
          KE12(MI,J,EP)=FM12(I,J,M)
          KE12(MI,MJ,EP)=M12(I,J,M)
          KE13(I,J,EP)=K13(I,J,M)
          KE13(I,MJ,EP)=MF13(I,J,M)
          KE13(MI,J,EP)=FM13(I,J,M)
          KE13(MI,MJ,EP)=M13(I,J,M)
          KE14(I,J,EP)=K14(I,J,M)
          KE14(I,MJ,EP)=MF14(I,J,M)
          KE14(MI,J,EP)=FM14(I,J,M)
          KE14(MI,MJ,EP)=M14(I,J,M)
          KE23(I,J,EP)=K23(I,J,M)
          KE23(I,MJ,EP)=MF23(I,J,M)
          KE23(MI,J,EP)=FM23(I,J,M)
          KE23(MI,MJ,EP)=M23(I,J,M)
          KE24(I,J,EP)=K24(I,J,M)
          KE24(I,MJ,EP)=MF24(I,J,M)
          KE24(MI,J,EP)=FM24(I,J,M)
          KE24(MI,MJ,EP)=M24(I,J,M)
          KE34(I,J,EP)=K34(I,J,M)
          KE34(I,MJ,EP)=MF34(I,J,M)
          KE34(MI,J,EP)=FM34(I,J,M)
          KE34(MI,MJ,EP)=M34(I,J,M)
         ENDDO
        ENDDO
       ENDDO
C----------------warped elements--------------
       NF=NPLAT+1
       IF (NF > JLT) RETURN
C       
       DO I=1,3 
       DO J=1,3 
#include "vectorize.inc"
        DO M=NF,JLT 
         EP=IPLAT(M)
         Q(J,I,M)=VQ(EP,I,J)
        ENDDO
       ENDDO
       ENDDO
C--------projection free Rigid rotation---- 
       DO J=1,4 
         DO I=1,2 
#include "vectorize.inc"
          DO M=NF,JLT 
           EP=IPLAT(M)
           CORELV(M,I,J)=VCORE(EP,I,J)
          ENDDO
         ENDDO
#include "vectorize.inc"
         DO M=NF,JLT 
           EP=IPLAT(M)
           Z1(M)=VCORE(EP,3,1)
         ENDDO
         DO I=1,3 
         DO K=1,3 
#include "vectorize.inc"
          DO M=NF,JLT 
           EP=IPLAT(M)
           QN(I,K,J,M)=VQN(EP,I,K,J)
          ENDDO
         ENDDO
         ENDDO
       END DO
        CALL CBAPROJK(
     1               NF    ,JLT    ,QN    ,Q     ,
     3               K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     4               M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     5               MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33, 
     6               MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34, 
     7               CORELV,Z1   )
C---------------------------------------
C   ASSEMBLAGE
C---------------------------------------
C---------KII -------- 
       DO I=1,3 
        MI=I+3
        DO J=I,3 
         MJ=J+3
#include "vectorize.inc"
         DO M=NF,JLT
          EP=IPLAT(M)
          KE11(I,J,EP)=K11(I,J,M)
          KE11(MI,MJ,EP)=M11(I,J,M)
          KE22(I,J,EP)=K22(I,J,M)
          KE22(MI,MJ,EP)=M22(I,J,M)
          KE33(I,J,EP)=K33(I,J,M)
          KE33(MI,MJ,EP)=M33(I,J,M)
          KE44(I,J,EP)=K44(I,J,M)
          KE44(MI,MJ,EP)=M44(I,J,M)
         ENDDO
        ENDDO
       ENDDO
C
       DO I=1,3 
        DO J=1,3 
         MJ=J+3
#include "vectorize.inc"
         DO M=NF,JLT
          EP=IPLAT(M)
          KE11(I,MJ,EP)=MF11(I,J,M)
          KE22(I,MJ,EP)=MF22(I,J,M)
          KE33(I,MJ,EP)=MF33(I,J,M)
          KE44(I,MJ,EP)=MF44(I,J,M)
         ENDDO
        ENDDO
       ENDDO
C---------KIJ -------- 
       DO I=1,3 
        MI=I+3
        DO J=1,3 
         MJ=J+3
#include "vectorize.inc"
         DO M=NF,JLT
          EP=IPLAT(M)
          KE12(I,J,EP)=K12(I,J,M)
          KE13(I,J,EP)=K13(I,J,M)
          KE14(I,J,EP)=K14(I,J,M)
          KE23(I,J,EP)=K23(I,J,M)
          KE24(I,J,EP)=K24(I,J,M)
          KE34(I,J,EP)=K34(I,J,M)
          KE12(I,MJ,EP)=MF12(I,J,M)
          KE13(I,MJ,EP)=MF13(I,J,M)
          KE14(I,MJ,EP)=MF14(I,J,M)
          KE23(I,MJ,EP)=MF23(I,J,M)
          KE24(I,MJ,EP)=MF24(I,J,M)
          KE34(I,MJ,EP)=MF34(I,J,M)
          KE12(MI,J,EP)=FM12(I,J,M)
          KE13(MI,J,EP)=FM13(I,J,M)
          KE14(MI,J,EP)=FM14(I,J,M)
          KE23(MI,J,EP)=FM23(I,J,M)
          KE24(MI,J,EP)=FM24(I,J,M)
          KE34(MI,J,EP)=FM34(I,J,M)
          KE12(MI,MJ,EP)=M12(I,J,M)
          KE13(MI,MJ,EP)=M13(I,J,M)
          KE14(MI,MJ,EP)=M14(I,J,M)
          KE23(MI,MJ,EP)=M23(I,J,M)
          KE24(MI,MJ,EP)=M24(I,J,M)
          KE34(MI,MJ,EP)=M34(I,J,M)
         ENDDO
        ENDDO
       ENDDO
C
       DO I=1,6 
        DO J=I+1,6 
         DO M=JFT,JLT
          KE11(J,I,M)=KE11(I,J,M)
          KE22(J,I,M)=KE22(I,J,M)
          KE33(J,I,M)=KE33(I,J,M)
          KE44(J,I,M)=KE44(I,J,M)
         ENDDO
        ENDDO
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  CBATRAN3                      source/elements/shell/coqueba/cbasumg3.F
Chd|-- called by -----------
Chd|        C3SUMG3                       source/elements/sh3n/coque3n/c3sumg3.F
Chd|        CBASUMG3                      source/elements/shell/coqueba/cbasumg3.F
Chd|        CZSUMG3                       source/elements/shell/coquez/czsumg3.F
Chd|        MSTIFORTH                     source/elements/solid/solide8z/mstiforth.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBATRAN3(JFT   ,JLT    ,VQI   ,KK,VQJ,ISYM) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,ISYM
      my_real 
     .   VQI(3,3,*), VQJ(3,3,*),KK(3,3,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP
      my_real
     .    K(3,3,MVSIZ)
C-----------------------------------------------
       IF (ISYM==1) THEN
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          K(I,J,EP)=VQI(1,I,EP)*(KK(1,1,EP)*VQJ(1,J,EP)+
     1                  KK(1,2,EP)*VQJ(2,J,EP)+KK(1,3,EP)*VQJ(3,J,EP))+
     2              VQI(2,I,EP)*(KK(1,2,EP)*VQJ(1,J,EP)+
     3                  KK(2,2,EP)*VQJ(2,J,EP)+KK(2,3,EP)*VQJ(3,J,EP))+
     4              VQI(3,I,EP)*(KK(1,3,EP)*VQJ(1,J,EP)+
     5                  KK(2,3,EP)*VQJ(2,J,EP)+KK(3,3,EP)*VQJ(3,J,EP))
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          KK(I,J,EP)= K(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
       ELSE
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K(I,J,EP)=VQI(1,I,EP)*(KK(1,1,EP)*VQJ(1,J,EP)+
     1                  KK(1,2,EP)*VQJ(2,J,EP)+KK(1,3,EP)*VQJ(3,J,EP))+
     2              VQI(2,I,EP)*(KK(2,1,EP)*VQJ(1,J,EP)+
     3                  KK(2,2,EP)*VQJ(2,J,EP)+KK(2,3,EP)*VQJ(3,J,EP))+
     4              VQI(3,I,EP)*(KK(3,1,EP)*VQJ(1,J,EP)+
     5                  KK(3,2,EP)*VQJ(2,J,EP)+KK(3,3,EP)*VQJ(3,J,EP))
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          KK(I,J,EP)= K(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
       ENDIF
      RETURN
      END
Chd|====================================================================
Chd|  CBATRAN2                      source/elements/shell/coqueba/cbasumg3.F
Chd|-- called by -----------
Chd|        C3SUMG3                       source/elements/sh3n/coque3n/c3sumg3.F
Chd|        CBASUMG3                      source/elements/shell/coqueba/cbasumg3.F
Chd|        CZSUMG3                       source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBATRAN2(JFT   ,JLT    ,VQI   ,KK,VQJ,ISYM) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real 
     .   VQI(3,3,*),VQJ(3,3,*),KK(3,3,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP,ISYM
      my_real
     .    K(3,3,MVSIZ)
C-----------------------------------------------
       IF (ISYM==1) THEN
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          K(I,J,EP)=VQI(1,I,EP)*(
     1                  KK(1,1,EP)*VQJ(1,J,EP)+KK(1,2,EP)*VQJ(2,J,EP))+
     2              VQI(2,I,EP)*(
     3                  KK(1,2,EP)*VQJ(1,J,EP)+KK(2,2,EP)*VQJ(2,J,EP))+
     4              VQI(3,I,EP)*KK(3,3,EP)*VQJ(3,J,EP)
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          KK(I,J,EP)= K(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
       ELSE
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K(I,J,EP)=VQI(1,I,EP)*(
     1                  KK(1,1,EP)*VQJ(1,J,EP)+KK(1,2,EP)*VQJ(2,J,EP))+
     2              VQI(2,I,EP)*(
     3                  KK(2,1,EP)*VQJ(1,J,EP)+KK(2,2,EP)*VQJ(2,J,EP))+
     4              VQI(3,I,EP)*KK(3,3,EP)*VQJ(3,J,EP)
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          KK(I,J,EP)= K(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
       ENDIF
      RETURN
      END
Chd|====================================================================
Chd|  CBATRAN32                     source/elements/shell/coqueba/cbasumg3.F
Chd|-- called by -----------
Chd|        CZSUMG3                       source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBATRAN32(JFT   ,JLT    ,VQI   ,KK,VQJ) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real 
     .   VQI(3,3,*),VQJ(3,3,*),KK(3,3,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP
      my_real
     .    K(3,3,MVSIZ)
C-----------------------------------------------
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K(I,J,EP)=VQI(1,I,EP)*(KK(1,1,EP)*VQJ(1,J,EP)+
     1                           KK(1,2,EP)*VQJ(2,J,EP))+
     2              VQI(2,I,EP)*(KK(2,1,EP)*VQJ(1,J,EP)+
     3                           KK(2,2,EP)*VQJ(2,J,EP))+
     4              VQI(3,I,EP)*(KK(3,1,EP)*VQJ(1,J,EP)+
     5                           KK(3,2,EP)*VQJ(2,J,EP))
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          KK(I,J,EP)= K(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
      RETURN
      END
Chd|====================================================================
Chd|  CBATRAN23                     source/elements/shell/coqueba/cbasumg3.F
Chd|-- called by -----------
Chd|        CZSUMG3                       source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBATRAN23(JFT   ,JLT    ,VQI   ,KK,VQJ) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real 
     .   VQI(3,3,*),VQJ(3,3,*),KK(3,3,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP
      my_real
     .    K(3,3,MVSIZ)
C-----------------------------------------------
C
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K(I,J,EP)=VQI(1,I,EP)*(KK(1,1,EP)*VQJ(1,J,EP)+
     1                  KK(1,2,EP)*VQJ(2,J,EP)+KK(1,3,EP)*VQJ(3,J,EP))+
     2              VQI(2,I,EP)*(KK(2,1,EP)*VQJ(1,J,EP)+
     3                  KK(2,2,EP)*VQJ(2,J,EP)+KK(2,3,EP)*VQJ(3,J,EP))
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          KK(I,J,EP)= K(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
      RETURN
      END
Chd|====================================================================
Chd|  CBATRAN232                    source/elements/shell/coqueba/cbasumg3.F
Chd|-- called by -----------
Chd|        C3SUMG3                       source/elements/sh3n/coque3n/c3sumg3.F
Chd|        CBASUMG3                      source/elements/shell/coqueba/cbasumg3.F
Chd|        CZSUMG3                       source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBATRAN232(JFT   ,JLT    ,VQI   ,KK,VQJ) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real 
     .   VQI(3,3,*),VQJ(3,3,*),KK(3,3,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP
      my_real
     .    K(3,3,MVSIZ)
C-----------------------------------------------
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K(I,J,EP)=VQI(3,I,EP)*(KK(3,1,EP)*VQJ(1,J,EP)+
     1                           KK(3,2,EP)*VQJ(2,J,EP))
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          KK(I,J,EP)= K(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
      RETURN
      END
Chd|====================================================================
Chd|  CBATRAN223                    source/elements/shell/coqueba/cbasumg3.F
Chd|-- called by -----------
Chd|        C3SUMG3                       source/elements/sh3n/coque3n/c3sumg3.F
Chd|        CBASUMG3                      source/elements/shell/coqueba/cbasumg3.F
Chd|        CZSUMG3                       source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBATRAN223(JFT   ,JLT    ,VQI   ,KK,VQJ) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real 
     .   VQI(3,3,*),VQJ(3,3,*),KK(3,3,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP
      my_real
     .    K(3,3,MVSIZ)
C-----------------------------------------------
C
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K(I,J,EP)=(VQI(1,I,EP)*KK(1,3,EP)+
     1               VQI(2,I,EP)*KK(2,3,EP))*VQJ(3,J,EP)
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          KK(I,J,EP)= K(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
      RETURN
      END
Chd|====================================================================
Chd|  CBATRAN233                    source/elements/shell/coqueba/cbasumg3.F
Chd|-- called by -----------
Chd|        C3SUMG3                       source/elements/sh3n/coque3n/c3sumg3.F
Chd|        CBASUMG3                      source/elements/shell/coqueba/cbasumg3.F
Chd|        CZSUMG3                       source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBATRAN233(JFT   ,JLT    ,VQI   ,KK,VQJ) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real 
     .   VQI(3,3,*),VQJ(3,3,*),KK(3,3,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP
      my_real
     .    K(3,3,MVSIZ)
C-----------------------------------------------
C----- QI^tKQJ K(1,3)>0,K(2,3)>0,K(3,1)>0,K(3,2)>0-------
C
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K(I,J,EP)=(VQI(1,I,EP)*KK(1,3,EP)+
     1               VQI(2,I,EP)*KK(2,3,EP))*VQJ(3,J,EP)+
     3               VQI(3,I,EP)*(KK(3,1,EP)*VQJ(1,J,EP)+
     4                            KK(3,2,EP)*VQJ(2,J,EP))
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          KK(I,J,EP)= K(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
      RETURN
      END
Chd|====================================================================
Chd|  CBAPROJK                      source/elements/shell/coqueba/cbasumg3.F
Chd|-- called by -----------
Chd|        CBASUMG3                      source/elements/shell/coqueba/cbasumg3.F
Chd|-- calls ---------------
Chd|        SETPROJKBA                    source/elements/shell/coqueba/cbasumg3.F
Chd|        SET_RSJ1                      source/elements/shell/coqueba/cbasumg3.F
Chd|====================================================================
      SUBROUTINE CBAPROJK(
     1                    JFT    ,JLT    ,VQN    ,VQ     ,
     3                     K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     4                     M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     5                     MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33,
     6                     MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34,
     7                     CORELV,Z1   )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real
     .    K11(3,3,*),K12(3,3,*),K13(3,3,*),K14(3,3,*),
     .    K22(3,3,*),K23(3,3,*),K24(3,3,*),K33(3,3,*),
     .    M11(3,3,*),M12(3,3,*),M13(3,3,*),M14(3,3,*),
     .    M22(3,3,*),M23(3,3,*),M24(3,3,*),M33(3,3,*),
     .    MF11(3,3,*),MF12(3,3,*),MF13(3,3,*),MF14(3,3,*),
     .    MF22(3,3,*),MF23(3,3,*),MF24(3,3,*),MF33(3,3,*),
     .    FM12(3,3,*),FM13(3,3,*),FM14(3,3,*),
     .    FM23(3,3,*),FM24(3,3,*),FM34(3,3,*),
     .    K34(3,3,*),K44(3,3,*),M34(3,3,*),M44(3,3,*),
     .    MF34(3,3,*),MF44(3,3,*),
     .    CORELV(MVSIZ,2,4),Z1(*),VQN(9,4,*),VQ(9,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I, J, K,L,EP,IS,IAS,NF,MI,MJ,M,ND
      my_real
     .    DR(3,3,MVSIZ),
     .    R1(3,3,MVSIZ),R2(3,3,MVSIZ),R3(3,3,MVSIZ),R4(3,3,MVSIZ),
     .    DI(6),Z2,DETA,D(6),
     .    XX,YY,ZZ,XY,XZ,YZ,ABC,XXYZ2,YYXZ2,ZZXY2
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C-------transport Mij to element local system first---------
        DO I=JFT,JLT
         Z2 = Z1(I)*Z1(I)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
         XX = CORELV(I,1,1)*CORELV(I,1,1)+CORELV(I,1,2)*CORELV(I,1,2)
     1       +CORELV(I,1,3)*CORELV(I,1,3)+CORELV(I,1,4)*CORELV(I,1,4)
         YY = CORELV(I,2,1)*CORELV(I,2,1)+CORELV(I,2,2)*CORELV(I,2,2)
     1       +CORELV(I,2,3)*CORELV(I,2,3)+CORELV(I,2,4)*CORELV(I,2,4)
         XY = CORELV(I,1,1)*CORELV(I,2,1)+CORELV(I,1,2)*CORELV(I,2,2)
     1       +CORELV(I,1,3)*CORELV(I,2,3)+CORELV(I,1,4)*CORELV(I,2,4)
         XZ =(CORELV(I,1,1)-CORELV(I,1,2)+CORELV(I,1,3)-CORELV(I,1,4))
     .           *Z1(I)
         YZ =(CORELV(I,2,1)-CORELV(I,2,2)+CORELV(I,2,3)-CORELV(I,2,4))
     .           *Z1(I)
         ZZ = FOUR*Z2
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
         D(1)= YY+ZZ+FOUR
         D(2)= XX+ZZ+FOUR
         D(3)= XX+YY+FOUR
         D(4)= -XY
         D(5)= -XZ
         D(6)= -YZ
         ABC = D(1)*D(2)*D(3)
         XXYZ2 = D(1)*D(6)*D(6)
         YYXZ2 = D(2)*D(5)*D(5)
         ZZXY2 = D(3)*D(4)*D(4)
         DETA = ABS(ABC+TWO*D(4)*D(5)*D(6)-XXYZ2-YYXZ2-ZZXY2)
         DETA = ONE/MAX(DETA,EM20)
         DI(1) = (ABC-XXYZ2)*DETA/MAX(D(1),EM20)
         DI(2) = (ABC-YYXZ2)*DETA/MAX(D(2),EM20)
         DI(3) = (ABC-ZZXY2)*DETA/MAX(D(3),EM20)
         DI(4) = (D(5)*D(6)-D(4)*D(3))*DETA
         DI(5) = (D(6)*D(4)-D(5)*D(2))*DETA
         DI(6) = (D(4)*D(5)-D(6)*D(1))*DETA
C
         DR(1,1,I)=  DI(1)
         DR(2,2,I)=  DI(2)
         DR(3,3,I)=  DI(3)
         DR(1,2,I)=  DI(4)
         DR(1,3,I)=  DI(5)
         DR(2,3,I)=  DI(6)
         DR(2,1,I)=  DR(1,2,I)
         DR(3,1,I)=  DR(1,3,I)
         DR(3,2,I)=  DR(2,3,I)
        END DO
C
        CALL SET_RSJ1(R1     ,R2     ,R3     ,R4    ,Z1    ,
     .                JFT    ,JLT    ,CORELV)
C -----------PROJECTION---------
        CALL SETPROJKBA(DR    ,R1     ,R2     ,R3     ,R4    ,
     3                K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     4                M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     5                MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33,
     6                MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34,
     7                VQ  ,VQN ,JFT ,JLT  )
C
      RETURN
      END
Chd|====================================================================
Chd|  SET_RSJ1                      source/elements/shell/coqueba/cbasumg3.F
Chd|-- called by -----------
Chd|        CBAPROJK                      source/elements/shell/coqueba/cbasumg3.F
Chd|        CZPROJKR                      source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|        SET_RI33                      source/elements/shell/coqueba/cbasumg3.F
Chd|====================================================================
      SUBROUTINE SET_RSJ1(R1     ,R2     ,R3     ,R4    ,Z1    ,
     .                    JFT    ,JLT    ,CORELV)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real
     .    CORELV(MVSIZ,2,4),
     .    R1(3,3,*),R2(3,3,*),R3(3,3,*),R4(3,3,*),Z1(*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER J,M,L
      my_real
     .    S,XI(MVSIZ),YI(MVSIZ),ZI(MVSIZ)
C-------------------------------------------------------------
        DO M=JFT,JLT
         DO L=1,3
          DO J=1,3
           R1(L,J,M)=ZERO
           R2(L,J,M)=ZERO
           R3(L,J,M)=ZERO
           R4(L,J,M)=ZERO
          ENDDO
         ENDDO
        END DO
C
        DO M=JFT,JLT
         XI(M)=CORELV(M,1,1)
         YI(M)=CORELV(M,2,1)
         ZI(M)=Z1(M)
        END DO
        CALL SET_RI33(XI ,YI, ZI  ,R1  ,JFT,JLT)
        DO M=JFT,JLT
         XI(M)=CORELV(M,1,2)
         YI(M)=CORELV(M,2,2)
         ZI(M)=-Z1(M)
        END DO
        CALL SET_RI33(XI ,YI, ZI  ,R2  ,JFT,JLT)
        DO M=JFT,JLT
         XI(M)=CORELV(M,1,3)
         YI(M)=CORELV(M,2,3)
         ZI(M)=Z1(M)
        END DO
        CALL SET_RI33(XI ,YI, ZI  ,R3  ,JFT,JLT)
        DO M=JFT,JLT
         XI(M)=CORELV(M,1,4)
         YI(M)=CORELV(M,2,4)
         ZI(M)=-Z1(M)
        END DO
        CALL SET_RI33(XI ,YI, ZI  ,R4  ,JFT,JLT)
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  SET_RI33                      source/elements/shell/coqueba/cbasumg3.F
Chd|-- called by -----------
Chd|        S6CCUMG3                      source/elements/thickshell/solide6c/s6ccumg3.F
Chd|        SET_RSJ1                      source/elements/shell/coqueba/cbasumg3.F
Chd|        SET_RSJ2                      source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SET_RI33(XI ,YI, ZI  ,RI  ,JFT,JLT)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real
     .   XI(*) ,YI(*), ZI(*),RI(3,3,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,L
         DO I=JFT,JLT
          RI(1,2,I)=ZI(I)
          RI(1,3,I)=-YI(I)
          RI(2,1,I)=-RI(1,2,I)
          RI(2,3,I)=XI(I)
          RI(3,1,I)=-RI(1,3,I)
          RI(3,2,I)=-RI(2,3,I)
         ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SETPROJKBA                    source/elements/shell/coqueba/cbasumg3.F
Chd|-- called by -----------
Chd|        CBAPROJK                      source/elements/shell/coqueba/cbasumg3.F
Chd|-- calls ---------------
Chd|        TRANKL1                       source/elements/shell/coquez/czsumg3.F
Chd|        TRANKLQ                       source/elements/shell/coquez/czsumg3.F
Chd|        TRANQIKQJ                     source/elements/shell/coquez/czsumg3.F
Chd|        TRANQIKQJ33                   source/elements/shell/coqueba/cbasumg3.F
Chd|        TRANQNKQ                      source/elements/shell/coqueba/cbasumg3.F
Chd|====================================================================
      SUBROUTINE SETPROJKBA(DR    ,R1     ,R2     ,R3     ,R4    ,
     3                     K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     4                     M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     5                     MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33, 
     6                     MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34, 
     .                     VQ  ,VQN ,JFT  ,JLT   ) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real 
     .    DR(3,3,*),VQ(3,3,*),VQN(3,3,4,*),
     .    R1(3,3,*),R2(3,3,*),R3(3,3,*),R4(3,3,*),
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
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP,IS,IAS,IT,IAT
      my_real
     .    KL(6,6,MVSIZ),KQ(6,6,MVSIZ),VQ1(3,3,MVSIZ),VQ2(3,3,MVSIZ),
     .    VQ3(3,3,MVSIZ),VQ4(3,3,MVSIZ)
         DATA IS/1/,IAS/0/,IT/1/,IAT/0/
      my_real,
     .  DIMENSION(:,:,:), ALLOCATABLE:: P,KE
C-------------------------------------------------------------
        ALLOCATE(P(24,24,MVSIZ))
        ALLOCATE(KE(24,24,MVSIZ))
C
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          VQ1(I,J,EP)= VQN(J,I,1,EP)
          VQ2(I,J,EP)= VQN(J,I,2,EP)
          VQ3(I,J,EP)= VQN(J,I,3,EP)
          VQ4(I,J,EP)= VQN(J,I,4,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P11=Q1Pr11Q
       CALL TRANQIKQJ33(JFT   ,JLT    ,R1  ,DR , R1 ,KL, IS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IS ) 
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IT ) 
       CALL TRANQNKQ(JFT  ,JLT    ,VQ1  ,KQ   ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          KE(I,J,EP)= K11(I,J,EP)
          KE(I+3,J+3,EP)= M11(I,J,EP)
         ENDDO
        ENDDO
        DO J=1,3 
         DO EP=JFT,JLT 
          KE(I,J+3,EP)= MF11(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P22=Q2Pr22Q
       CALL TRANQIKQJ33(JFT   ,JLT    ,R2  ,DR , R2 ,KL, IS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IS ) 
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ  ,IT ) 
       CALL TRANQNKQ(JFT  ,JLT    ,VQ2  ,KQ   ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+6,J+6,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          KE(I+6,J+6,EP)= K22(I,J,EP)
          KE(I+9,J+9,EP)= M22(I,J,EP)
         ENDDO
        ENDDO
        DO J=1,3 
         DO EP=JFT,JLT 
          KE(I+6,J+9,EP)= MF22(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P33=Q3Pr33Q
       CALL TRANQIKQJ33(JFT   ,JLT    ,R3  ,DR , R3 ,KL, IS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IS ) 
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IT ) 
       CALL TRANQNKQ(JFT  ,JLT    ,VQ3  ,KQ   ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+12,J+12,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          KE(I+12,J+12,EP)= K33(I,J,EP)
          KE(I+15,J+15,EP)= M33(I,J,EP)
         ENDDO
        ENDDO
        DO J=1,3 
         DO EP=JFT,JLT 
          KE(I+12,J+15,EP)= MF33(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P44=Q4Pr44Q
       CALL TRANQIKQJ33(JFT   ,JLT    ,R4  ,DR , R4 ,KL, IS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IS ) 
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IT ) 
       CALL TRANQNKQ(JFT  ,JLT    ,VQ4  ,KQ   ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+18,J+18,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          KE(I+18,J+18,EP)= K44(I,J,EP)
          KE(I+21,J+21,EP)= M44(I,J,EP)
         ENDDO
        ENDDO
        DO J=1,3 
         DO EP=JFT,JLT 
          KE(I+18,J+21,EP)= MF44(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P12=Q1Pr12Q
       CALL TRANQIKQJ33(JFT   ,JLT    ,R1  ,DR , R2 ,KL, IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IT ) 
       CALL TRANQNKQ(JFT  ,JLT    ,VQ1  ,KQ   ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I,J+6,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          KE(I,J+6,EP)= K12(I,J,EP)
          KE(I+3,J+9,EP)= M12(I,J,EP)
          KE(I,J+9,EP)= MF12(I,J,EP)
          KE(I+3,J+6,EP)= FM12(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P21=Q2Pr21Q
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ  ,IAT ) 
       CALL TRANQNKQ(JFT  ,JLT    ,VQ2  ,KQ   ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+6,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P13=Q1Pr13Q
       CALL TRANQIKQJ33(JFT   ,JLT    ,R1  ,DR , R3 ,KL, IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ  ,IT ) 
       CALL TRANQNKQ(JFT  ,JLT    ,VQ1  ,KQ   ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I,J+12,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          KE(I,J+12,EP)= K13(I,J,EP)
          KE(I+3,J+15,EP)= M13(I,J,EP)
          KE(I,J+15,EP)= MF13(I,J,EP)
          KE(I+3,J+12,EP)= FM13(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P31=Q3Pr31Q
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IAT ) 
       CALL TRANQNKQ(JFT  ,JLT    ,VQ3  ,KQ   ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+12,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P14=Q1Pr14Q
       CALL TRANQIKQJ33(JFT   ,JLT    ,R1  ,DR , R4 ,KL, IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IT ) 
       CALL TRANQNKQ(JFT  ,JLT    ,VQ1  ,KQ   ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I,J+18,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          KE(I,J+18,EP)= K14(I,J,EP)
          KE(I+3,J+21,EP)= M14(I,J,EP)
          KE(I,J+21,EP)= MF14(I,J,EP)
          KE(I+3,J+18,EP)= FM14(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P41=Q4P41Q
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IAT ) 
       CALL TRANQNKQ(JFT  ,JLT    ,VQ4  ,KQ   ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+18,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P23=Q2Pr23Q
       CALL TRANQIKQJ33(JFT   ,JLT    ,R2  ,DR , R3 ,KL, IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IT ) 
       CALL TRANQNKQ(JFT  ,JLT    ,VQ2  ,KQ   ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+6,J+12,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          KE(I+6,J+12,EP)= K23(I,J,EP)
          KE(I+9,J+15,EP)= M23(I,J,EP)
          KE(I+6,J+15,EP)= MF23(I,J,EP)
          KE(I+9,J+12,EP)= FM23(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P32=Q3Pr32Q
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IAT ) 
       CALL TRANQNKQ(JFT  ,JLT    ,VQ3  ,KQ   ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+12,J+6,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P24=Q2Pr24Q
       CALL TRANQIKQJ33(JFT   ,JLT    ,R2  ,DR , R4 ,KL, IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IT ) 
       CALL TRANQNKQ(JFT  ,JLT    ,VQ2  ,KQ   ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+6,J+18,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          KE(I+6,J+18,EP)= K24(I,J,EP)
          KE(I+9,J+21,EP)= M24(I,J,EP)
          KE(I+6,J+21,EP)= MF24(I,J,EP)
          KE(I+9,J+18,EP)= FM24(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P42=Q4Pr42Q
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IAT ) 
       CALL TRANQNKQ(JFT  ,JLT    ,VQ4  ,KQ   ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+18,J+6,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P34=Q3Pr34Q
       CALL TRANQIKQJ33(JFT   ,JLT    ,R3  ,DR , R4 ,KL, IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IT ) 
       CALL TRANQNKQ(JFT  ,JLT    ,VQ3  ,KQ   ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+12,J+18,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          KE(I+12,J+18,EP)= K34(I,J,EP)
          KE(I+15,J+21,EP)= M34(I,J,EP)
          KE(I+12,J+21,EP)= MF34(I,J,EP)
          KE(I+15,J+18,EP)= FM34(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P43=Q4Pr43Q
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IAT ) 
       CALL TRANQNKQ(JFT  ,JLT    ,VQ4  ,KQ   ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+18,J+12,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------
        DO I=1,24 
        DO J=I+1,24 
         DO EP=JFT,JLT 
          KE(J,I,EP)= KE(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------
        CALL TRANQIKQJ(JFT   ,JLT    ,P  ,KE,P ,24 ,IS ) 
C-----------after projection----
C-----------K11
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          K11(I,J,EP) =KE(I,J,EP)
          M11(I,J,EP) =KE(I+3,J+3,EP)
         ENDDO
        ENDDO
        DO J=1,3 
         DO EP=JFT,JLT 
          MF11(I,J,EP) = KE(I,J+3,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K22
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          K22(I,J,EP) = KE(I+6,J+6,EP)
          M22(I,J,EP) = KE(I+9,J+9,EP)
         ENDDO
        ENDDO
        DO J=1,3 
         DO EP=JFT,JLT 
          MF22(I,J,EP) = KE(I+6,J+9,EP) 
         ENDDO
        ENDDO
        ENDDO
C-----------K33
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          K33(I,J,EP) = KE(I+12,J+12,EP)
          M33(I,J,EP) = KE(I+15,J+15,EP) 
         ENDDO
        ENDDO
        DO J=1,3 
         DO EP=JFT,JLT 
          MF33(I,J,EP) = KE(I+12,J+15,EP) 
         ENDDO
        ENDDO
        ENDDO
C-----------K44
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          K44(I,J,EP) = KE(I+18,J+18,EP)
          M44(I,J,EP) = KE(I+21,J+21,EP)
         ENDDO
        ENDDO
        DO J=1,3 
         DO EP=JFT,JLT 
          MF44(I,J,EP) = KE(I+18,J+21,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K12
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K12(I,J,EP) =KE(I,J+6,EP) 
          M12(I,J,EP) =KE(I+3,J+9,EP)
          MF12(I,J,EP)=KE(I,J+9,EP)
          FM12(I,J,EP)=KE(I+3,J+6,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K13
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K13(I,J,EP) = KE(I,J+12,EP)
          M13(I,J,EP) = KE(I+3,J+15,EP) 
          MF13(I,J,EP) = KE(I,J+15,EP)
          FM13(I,J,EP) = KE(I+3,J+12,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K14
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K14(I,J,EP) =KE(I,J+18,EP)
          M14(I,J,EP) =KE(I+3,J+21,EP)
          MF14(I,J,EP)=KE(I,J+21,EP)
          FM14(I,J,EP)=KE(I+3,J+18,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K23
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K23(I,J,EP) = KE(I+6,J+12,EP) 
          M23(I,J,EP) = KE(I+9,J+15,EP) 
          MF23(I,J,EP) =KE(I+6,J+15,EP) 
          FM23(I,J,EP) =KE(I+9,J+12,EP) 
         ENDDO
        ENDDO
        ENDDO
C-----------K24
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K24(I,J,EP) = KE(I+6,J+18,EP)
          M24(I,J,EP) = KE(I+9,J+21,EP)
          MF24(I,J,EP) =KE(I+6,J+21,EP)
          FM24(I,J,EP) =KE(I+9,J+18,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------K34
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K34(I,J,EP) = KE(I+12,J+18,EP) 
          M34(I,J,EP) = KE(I+15,J+21,EP) 
          MF34(I,J,EP) =KE(I+12,J+21,EP) 
          FM34(I,J,EP) =KE(I+15,J+18,EP) 
         ENDDO
        ENDDO
        ENDDO
C-----------
        DEALLOCATE(P)
        DEALLOCATE(KE)
      RETURN
      END
Chd|====================================================================
Chd|  TRANQIKQJ33                   source/elements/shell/coqueba/cbasumg3.F
Chd|-- called by -----------
Chd|        SETPROJKBA                    source/elements/shell/coqueba/cbasumg3.F
Chd|        SETPROJKZ1                    source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TRANQIKQJ33(JFT   ,JLT    ,RI  ,RD , RJ,KD ,ISYM)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,ISYM
      my_real
     .   RI(3,3,*), RJ(3,3,*),RD(3,3,*),KD(6,6,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP,L,I1,J1
      my_real
     .    K(3,3)
C--------------[KD]=|RI| |RD| |RJ|^t---------------------------------
C--------------     | I|      |I |---------------------------------
       IF (ISYM==1) THEN
        DO I=1,3
        DO J=I,3
         DO EP=JFT,JLT
          K(I,J)=ZERO
          DO L=1,3
           K(I,J)=K(I,J)+RI(I,1,EP)*RD(1,L,EP)*RJ(J,L,EP)+
     1                   RI(I,2,EP)*RD(2,L,EP)*RJ(J,L,EP)+
     1                   RI(I,3,EP)*RD(3,L,EP)*RJ(J,L,EP)
          ENDDO
          KD(I,J,EP)= K(I,J)
          KD(J,I,EP)= K(I,J)
         ENDDO
C
         I1=I+3
         J1=J+3
         DO EP=JFT,JLT
          KD(I1,J1,EP)= RD(I,J,EP)
          KD(J1,I1,EP)= RD(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,3
        DO J=1,3
         I1=I+3
         J1=J+3
         DO EP=JFT,JLT
          K(I,J)=ZERO
          DO L=1,3
           K(I,J)=K(I,J)+RI(I,L,EP)*RD(L,J,EP)
          ENDDO
          KD(I,J1,EP)= K(I,J)
          KD(J1,I,EP)= K(I,J)
         ENDDO
        ENDDO
        ENDDO
C
       ELSE
        DO I=1,3
        DO J=1,3
         DO EP=JFT,JLT
          K(I,J)=ZERO
          DO L=1,3
           K(I,J)=K(I,J)+RI(I,1,EP)*RD(1,L,EP)*RJ(J,L,EP)+
     1                   RI(I,2,EP)*RD(2,L,EP)*RJ(J,L,EP)+
     1                   RI(I,3,EP)*RD(3,L,EP)*RJ(J,L,EP)
          ENDDO
          KD(I,J,EP)= K(I,J)
         ENDDO
C
         I1=I+3
         J1=J+3
         DO EP=JFT,JLT
          KD(I1,J1,EP)= RD(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C
        DO J=1,3
         J1=J+3
        DO I=1,3
         DO EP=JFT,JLT
          K(I,J)=ZERO
          DO L=1,3
           K(I,J)=K(I,J)+RI(I,L,EP)*RD(L,J,EP)
          ENDDO
          KD(I,J1,EP)= K(I,J)
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,3
         I1=I+3
        DO J=1,3
         DO EP=JFT,JLT
          K(I,J)=ZERO
          DO L=1,3
           K(I,J)=K(I,J)+RD(I,L,EP)*RJ(J,L,EP)
          ENDDO
          KD(I1,J,EP)= K(I,J)
         ENDDO
        ENDDO
        ENDDO
C
       ENDIF
      RETURN
      END
Chd|====================================================================
Chd|  TRANQNKQ                      source/elements/shell/coqueba/cbasumg3.F
Chd|-- called by -----------
Chd|        SETPROJKBA                    source/elements/shell/coqueba/cbasumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TRANQNKQ(JFT   ,JLT    ,VQN   ,KQ   ) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,IT
      my_real 
     .   VQN(3,3,*), KQ(6,6,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP,K,L,J1,I1
      my_real 
     .   KD(3,3,MVSIZ),KK(3,3,MVSIZ)
C--------------KQ<-VQ*KQ----VQ=|I   0 |
C------------------------------|0 VQN|---------------------
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          KD(I,J,EP)= ZERO
          KK(I,J,EP)= ZERO
         ENDDO
        ENDDO
        ENDDO
C        
        DO J=1,3 
           J1=J+3
        DO I=1,3 
         DO EP=JFT,JLT 
          DO K=1,3 
            KD(I,J,EP)=KD(I,J,EP)+VQN(I,K,EP)*KQ(K+3,J,EP)
            KK(I,J,EP)=KK(I,J,EP)+VQN(I,K,EP)*KQ(K+3,J1,EP)
          ENDDO 
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,3 
           I1=I+3
        DO J=1,3 
           J1=J+3
         DO EP=JFT,JLT 
            KQ(I1,J,EP)=KD(I,J,EP)
            KQ(I1,J1,EP)=KK(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  CN4SUMG3                      source/elements/shell/coqueba/cbasumg3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CN4SUMG3(
     1                     JFT    ,JLT    ,IPLAT   ,
     2                     K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     3                     M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     4                     MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33, 
     5                     MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34, 
     6                     KE11,KE22,KE33,KE44,KE12,KE13,KE14,KE23,
     7                     KE24,KE34)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,IPLAT(*)
      my_real
     .    K11(3,3,*),K12(3,3,*),K13(3,3,*),K14(3,3,*),
     .    K22(3,3,*),K23(3,3,*),K24(3,3,*),K33(3,3,*),
     .    M11(3,3,*),M12(3,3,*),M13(3,3,*),M14(3,3,*),
     .    M22(3,3,*),M23(3,3,*),M24(3,3,*),M33(3,3,*),
     .    MF11(3,3,*),MF12(3,3,*),MF13(3,3,*),MF14(3,3,*),
     .    MF22(3,3,*),MF23(3,3,*),MF24(3,3,*),MF33(3,3,*),
     .    FM12(3,3,*),FM13(3,3,*),FM14(3,3,*),
     .    FM23(3,3,*),FM24(3,3,*),FM34(3,3,*),
     .    K34(3,3,*),K44(3,3,*),M34(3,3,*),M44(3,3,*),
     .    MF34(3,3,*),MF44(3,3,*),
     .    KE11(6,6,*),KE22(6,6,*),KE33(6,6,*),KE44(6,6,*),
     .    KE12(6,6,*),KE13(6,6,*),KE14(6,6,*),KE23(6,6,*),
     .    KE24(6,6,*),KE34(6,6,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I, J, K,EP,M,MI,MJ
C---------KII -------- 
       DO I=1,3 
        MI=I+3
        DO J=I,3 
         MJ=J+3
#include "vectorize.inc"
         DO M=JFT,JLT
          EP=IPLAT(M)
          KE11(I,J,EP)=K11(I,J,M)
          KE11(MI,MJ,EP)=M11(I,J,M)
          KE22(I,J,EP)=K22(I,J,M)
          KE22(MI,MJ,EP)=M22(I,J,M)
          KE33(I,J,EP)=K33(I,J,M)
          KE33(MI,MJ,EP)=M33(I,J,M)
          KE44(I,J,EP)=K44(I,J,M)
          KE44(MI,MJ,EP)=M44(I,J,M)
         ENDDO
        ENDDO
       ENDDO
C
       DO I=1,3 
        DO J=1,3 
         MJ=J+3
#include "vectorize.inc"
         DO M=JFT,JLT
          EP=IPLAT(M)
          KE11(I,MJ,EP)=MF11(I,J,M)
          KE22(I,MJ,EP)=MF22(I,J,M)
          KE33(I,MJ,EP)=MF33(I,J,M)
          KE44(I,MJ,EP)=MF44(I,J,M)
         ENDDO
        ENDDO
       ENDDO
C
C---------KIJ -------- 
       DO I=1,3 
        MI=I+3
        DO J=1,3 
         MJ=J+3
#include "vectorize.inc"
         DO M=JFT,JLT
          EP=IPLAT(M)
          KE12(I,J,EP)=K12(I,J,M)
          KE12(I,MJ,EP)=MF12(I,J,M)
          KE12(MI,J,EP)=FM12(I,J,M)
          KE12(MI,MJ,EP)=M12(I,J,M)
          KE13(I,J,EP)=K13(I,J,M)
          KE13(I,MJ,EP)=MF13(I,J,M)
          KE13(MI,J,EP)=FM13(I,J,M)
          KE13(MI,MJ,EP)=M13(I,J,M)
          KE14(I,J,EP)=K14(I,J,M)
          KE14(I,MJ,EP)=MF14(I,J,M)
          KE14(MI,J,EP)=FM14(I,J,M)
          KE14(MI,MJ,EP)=M14(I,J,M)
          KE23(I,J,EP)=K23(I,J,M)
          KE23(I,MJ,EP)=MF23(I,J,M)
          KE23(MI,J,EP)=FM23(I,J,M)
          KE23(MI,MJ,EP)=M23(I,J,M)
          KE24(I,J,EP)=K24(I,J,M)
          KE24(I,MJ,EP)=MF24(I,J,M)
          KE24(MI,J,EP)=FM24(I,J,M)
          KE24(MI,MJ,EP)=M24(I,J,M)
          KE34(I,J,EP)=K34(I,J,M)
          KE34(I,MJ,EP)=MF34(I,J,M)
          KE34(MI,J,EP)=FM34(I,J,M)
          KE34(MI,MJ,EP)=M34(I,J,M)
         ENDDO
        ENDDO
       ENDDO
C
       DO I=1,6 
        DO J=I+1,6 
         DO M=JFT,JLT
          KE11(J,I,M)=KE11(I,J,M)
          KE22(J,I,M)=KE22(I,J,M)
          KE33(J,I,M)=KE33(I,J,M)
          KE44(J,I,M)=KE44(I,J,M)
         ENDDO
        ENDDO
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  IMPKCTMP                      source/elements/shell/coqueba/cbasumg3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITEKC4                      source/elements/shell/coqueba/cbasumg3.F
Chd|====================================================================
      SUBROUTINE IMPKCTMP( JFT,JLT,IUGEO,
     1                    KE11   ,KE12   ,KE13   ,KE14   ,KE22 ,
     2                    KE23   ,KE24   ,KE33   ,KE34   ,KE44 )
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,IUGEO
C     REAL
      my_real    
     .   KE11(6,6,*),KE22(6,6,*),KE33(6,6,*),KE44(6,6,*),
     .   KE12(6,6,*),KE13(6,6,*),KE14(6,6,*),KE23(6,6,*),
     .   KE24(6,6,*),KE34(6,6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NT,IG(MVSIZ)
C=======================================================================
C     SHELL
C=======================================================================
      CALL WRITEKC4(IUGEO,JFT,JLT,'K11',KE11)
      CALL WRITEKC4(IUGEO,JFT,JLT,'K12',KE12)
      CALL WRITEKC4(IUGEO,JFT,JLT,'K13',KE13)
      CALL WRITEKC4(IUGEO,JFT,JLT,'K14',KE14)
      CALL WRITEKC4(IUGEO,JFT,JLT,'K22',KE22)
      CALL WRITEKC4(IUGEO,JFT,JLT,'K23',KE23)
      CALL WRITEKC4(IUGEO,JFT,JLT,'K24',KE24)
      CALL WRITEKC4(IUGEO,JFT,JLT,'K33',KE33)
      CALL WRITEKC4(IUGEO,JFT,JLT,'K34',KE34)
      CALL WRITEKC4(IUGEO,JFT,JLT,'K44',KE44)
C
      RETURN
      END
Chd|====================================================================
Chd|  WRITEKC4                      source/elements/shell/coqueba/cbasumg3.F
Chd|-- called by -----------
Chd|        IMPKCTMP                      source/elements/shell/coqueba/cbasumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE WRITEKC4( IN,JFT,JLT,CH,KIJ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,IN
      CHARACTER CH*3
C     REAL
      my_real    
     .   KIJ(6,6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NT
      CHARACTER KEY*10,KEY1*23
C-----------------------------------------------
       KEY='/SHELL_'//CH
       KEY1='#3d Shell Elements '//CH
       WRITE(IN,'(A)') KEY
       WRITE(IN,'(A)')KEY1
       WRITE(IN,'(A)')
     .     '#FORMAT: (6(/,1P6E12.5),/) '
       WRITE(IN,'(2A)')'#SYSSHEL USRSHEL  K(I,J)  I=1,6;J=1,6'
       DO N= JFT,JLT
        WRITE(IN,'(6(/,1P6E12.5),/)'
     .    )((KIJ(I,J,N),I=1,6),J=1,6)
       ENDDO 
      RETURN
      END
C
 
