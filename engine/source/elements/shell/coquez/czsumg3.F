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
Chd|  CZSUMG3                       source/elements/shell/coquez/czsumg3.F
Chd|-- called by -----------
Chd|        CZKE3                         source/elements/shell/coquez/czke3.F
Chd|        CZKEL3                        source/elements/shell/coquez/czkel3.F
Chd|-- calls ---------------
Chd|        CBATRAN2                      source/elements/shell/coqueba/cbasumg3.F
Chd|        CBATRAN223                    source/elements/shell/coqueba/cbasumg3.F
Chd|        CBATRAN23                     source/elements/shell/coqueba/cbasumg3.F
Chd|        CBATRAN232                    source/elements/shell/coqueba/cbasumg3.F
Chd|        CBATRAN233                    source/elements/shell/coqueba/cbasumg3.F
Chd|        CBATRAN3                      source/elements/shell/coqueba/cbasumg3.F
Chd|        CBATRAN32                     source/elements/shell/coqueba/cbasumg3.F
Chd|        CZPROJK                       source/elements/shell/coquez/czsumg3.F
Chd|        CZTRAN2                       source/elements/shell/coquez/czsumg3.F
Chd|        CZTRANDR                      source/elements/shell/coquez/czsumg3.F
Chd|====================================================================
      SUBROUTINE CZSUMG3(
     1                    JFT    ,JLT    ,VQN    ,VQ     ,NPLAT,   
     2                    IPLAT  ,
     3                     K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     4                     M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     5                     MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33, 
     6                     MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34, 
     7                     KE11,KE22,KE33,KE44,KE12,KE13,KE14,KE23,
     8                     KE24,KE34,CORELV,Z1  ,IDRIL ,IORTH)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,NPLAT ,IPLAT(*),IDRIL,IORTH
      my_real 
     .   VQN(MVSIZ,3,4),VQ(MVSIZ,3,3)
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
     .    KE24(6,6,*),KE34(6,6,*),CORELV(MVSIZ,2,4),Z1(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I, J, K,EP,IS,IAS,NF,MI,MJ,M
      my_real 
     .    MZ11(MVSIZ),MZ22(MVSIZ),MZ33(MVSIZ),MZ44(MVSIZ),MZ12(MVSIZ),
     .    MZ13(MVSIZ),MZ14(MVSIZ),MZ23(MVSIZ),MZ24(MVSIZ),MZ34(MVSIZ),
     .     Q(3,3,MVSIZ),Q1(3,3,MVSIZ),Q2(3,3,MVSIZ),
     .     Q3(3,3,MVSIZ),Q4(3,3,MVSIZ),PP(3,3,4,MVSIZ)
      DATA IS/1/,IAS/0/
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C---------------------------------------
C   TRANS LOCAL-->GLOBAL ET 5DDL-->6DDL
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
C
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
      ELSEIF (IDRIL >0) THEN
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
       IF (IKPROJ > 0) THEN
        CALL CZPROJK(
     1               NF    ,JLT    ,VQN    ,Q     ,IPLAT,   
     3               K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     4               M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     5               MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33, 
     6               MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34, 
     7               CORELV,Z1   ,IDRIL )
c        
       ELSE
C-------------projection-----drilling only------------------------
       CALL CBATRAN3(NF,JLT,Q,K11,Q,IS)
       CALL CBATRAN3(NF,JLT,Q,K22,Q,IS)
       CALL CBATRAN3(NF,JLT,Q,K33,Q,IS)
       CALL CBATRAN3(NF,JLT,Q,K44,Q,IS)
       CALL CBATRAN3(NF,JLT,Q,K12,Q,IAS)
       CALL CBATRAN3(NF,JLT,Q,K13,Q,IAS)
       CALL CBATRAN3(NF,JLT,Q,K14,Q,IAS)
       CALL CBATRAN3(NF,JLT,Q,K23,Q,IAS)
       CALL CBATRAN3(NF,JLT,Q,K24,Q,IAS)
       CALL CBATRAN3(NF,JLT,Q,K34,Q,IAS)
       DO J=1,4 
#include "vectorize.inc"
       DO M=NF,JLT 
        EP=IPLAT(M)
        PP(1,1,J,M)=ONE-VQN(EP,1,J)*VQN(EP,1,J)
        PP(2,2,J,M)=ONE-VQN(EP,2,J)*VQN(EP,2,J)
C        PP(3,3,J,M)=VQN(3,J,EP)
        PP(3,3,J,M)=ONE-VQN(EP,3,J)*VQN(EP,3,J)
        PP(1,2,J,M)=-VQN(EP,1,J)*VQN(EP,2,J)
        PP(1,3,J,M)=-VQN(EP,1,J)*VQN(EP,3,J)
        PP(2,3,J,M)=-VQN(EP,2,J)*VQN(EP,3,J)
       ENDDO
       ENDDO
C
       IF (IDRIL >0) THEN
       DO J=1,4 
       DO M=NF,JLT 
        EP=IPLAT(M)
        PP(2,1,J,M)=PP(1,2,J,M)
        PP(3,1,J,M)=VQN(EP,1,J)
        PP(3,2,J,M)=VQN(EP,2,J)
        PP(3,3,J,M)=VQN(EP,3,J)
       ENDDO
       ENDDO
       ELSE
       DO J=1,4 
       DO M=NF,JLT 
        PP(2,1,J,M)=PP(1,2,J,M)
        PP(3,1,J,M)=PP(1,3,J,M)
        PP(3,2,J,M)=PP(2,3,J,M)
       ENDDO
       ENDDO
       END IF !(IDRIL >0) THEN
C
C       DO J=1,4 
C#include "vectorize.inc"
C       DO M=NF,JLT 
C        EP=IPLAT(M)
C        PP(2,1,J,M)=PP(1,2,J,M)
C        PP(3,1,J,M)=VQN(1,J,EP)
C        PP(3,2,J,M)=VQN(2,J,EP)
C       ENDDO
C       ENDDO
C------------------QJ=PPJ*Q-------------------
       DO I=1,3 
       DO J=1,3 
        DO EP=NF,JLT 
         Q1(I,J,EP)=PP(I,1,1,EP)*Q(1,J,EP)+PP(I,2,1,EP)*Q(2,J,EP)+
     .             PP(I,3,1,EP)*Q(3,J,EP)
         Q2(I,J,EP)=PP(I,1,2,EP)*Q(1,J,EP)+PP(I,2,2,EP)*Q(2,J,EP)+
     .             PP(I,3,2,EP)*Q(3,J,EP)
         Q3(I,J,EP)=PP(I,1,3,EP)*Q(1,J,EP)+PP(I,2,3,EP)*Q(2,J,EP)+
     .             PP(I,3,3,EP)*Q(3,J,EP)
         Q4(I,J,EP)=PP(I,1,4,EP)*Q(1,J,EP)+PP(I,2,4,EP)*Q(2,J,EP)+
     .             PP(I,3,4,EP)*Q(3,J,EP)
        ENDDO
       ENDDO
       ENDDO
C
       IF (IDRIL==0.AND.IORTH==0) THEN
       CALL CZTRAN2(NF,JLT,Q1,M11,Q1,IS,Q)
       CALL CZTRAN2(NF,JLT,Q2,M22,Q2,IS,Q)
       CALL CZTRAN2(NF,JLT,Q3,M33,Q3,IS,Q)
       CALL CZTRAN2(NF,JLT,Q4,M44,Q4,IS,Q)
       CALL CZTRAN2(NF,JLT,Q1,M12,Q2,IAS,Q)
       CALL CZTRAN2(NF,JLT,Q1,M13,Q3,IAS,Q)
       CALL CZTRAN2(NF,JLT,Q1,M14,Q4,IAS,Q)
       CALL CZTRAN2(NF,JLT,Q2,M23,Q3,IAS,Q)
       CALL CZTRAN2(NF,JLT,Q2,M24,Q4,IAS,Q)
       CALL CZTRAN2(NF,JLT,Q3,M34,Q4,IAS,Q)
       CALL CBATRAN32(NF,JLT,Q,MF11,Q1)
       CALL CBATRAN32(NF,JLT,Q,MF22,Q2)
       CALL CBATRAN32(NF,JLT,Q,MF33,Q3)
       CALL CBATRAN32(NF,JLT,Q,MF44,Q4)
       CALL CBATRAN32(NF,JLT,Q,MF12,Q2)
       CALL CBATRAN32(NF,JLT,Q,MF13,Q3)
       CALL CBATRAN32(NF,JLT,Q,MF14,Q4)
       CALL CBATRAN32(NF,JLT,Q,MF23,Q3)
       CALL CBATRAN32(NF,JLT,Q,MF24,Q4)
       CALL CBATRAN32(NF,JLT,Q,MF34,Q4)
       CALL CBATRAN23(NF,JLT,Q1,FM12,Q)
       CALL CBATRAN23(NF,JLT,Q1,FM13,Q)
       CALL CBATRAN23(NF,JLT,Q1,FM14,Q)
       CALL CBATRAN23(NF,JLT,Q2,FM23,Q)
       CALL CBATRAN23(NF,JLT,Q2,FM24,Q)
       CALL CBATRAN23(NF,JLT,Q3,FM34,Q)
       ELSE
        IF (IDRIL>0.AND.IORTH>0) THEN
         CALL CBATRAN3(NF,JLT,Q1,M11,Q1,IS)
         CALL CBATRAN3(NF,JLT,Q2,M22,Q2,IS)
         CALL CBATRAN3(NF,JLT,Q3,M33,Q3,IS)
         CALL CBATRAN3(NF,JLT,Q4,M44,Q4,IS)
         CALL CBATRAN3(NF,JLT,Q1,M12,Q2,IAS)
         CALL CBATRAN3(NF,JLT,Q1,M13,Q3,IAS)
         CALL CBATRAN3(NF,JLT,Q1,M14,Q4,IAS)
         CALL CBATRAN3(NF,JLT,Q2,M23,Q3,IAS)
         CALL CBATRAN3(NF,JLT,Q2,M24,Q4,IAS)
         CALL CBATRAN3(NF,JLT,Q3,M34,Q4,IAS)
        ELSEIF (IDRIL==0 ) THEN
         CALL CZTRAN2(NF,JLT,Q1,M11,Q1,IS,Q)
         CALL CZTRAN2(NF,JLT,Q2,M22,Q2,IS,Q)
         CALL CZTRAN2(NF,JLT,Q3,M33,Q3,IS,Q)
         CALL CZTRAN2(NF,JLT,Q4,M44,Q4,IS,Q)
         CALL CZTRAN2(NF,JLT,Q1,M12,Q2,IAS,Q)
         CALL CZTRAN2(NF,JLT,Q1,M13,Q3,IAS,Q)
         CALL CZTRAN2(NF,JLT,Q1,M14,Q4,IAS,Q)
         CALL CZTRAN2(NF,JLT,Q2,M23,Q3,IAS,Q)
         CALL CZTRAN2(NF,JLT,Q2,M24,Q4,IAS,Q)
         CALL CZTRAN2(NF,JLT,Q3,M34,Q4,IAS,Q)
        ELSE
         CALL CZTRANDR(NF,JLT,Q1,M11,Q1,IS)
         CALL CZTRANDR(NF,JLT,Q2,M22,Q2,IS)
         CALL CZTRANDR(NF,JLT,Q3,M33,Q3,IS)
         CALL CZTRANDR(NF,JLT,Q4,M44,Q4,IS)
         CALL CZTRANDR(NF,JLT,Q1,M12,Q2,IAS)
         CALL CZTRANDR(NF,JLT,Q1,M13,Q3,IAS)
         CALL CZTRANDR(NF,JLT,Q1,M14,Q4,IAS)
         CALL CZTRANDR(NF,JLT,Q2,M23,Q3,IAS)
         CALL CZTRANDR(NF,JLT,Q2,M24,Q4,IAS)
         CALL CZTRANDR(NF,JLT,Q3,M34,Q4,IAS)
        END IF  !IF (IDRIL>0.AND.IORTH>0) THEN
        CALL CBATRAN3(NF,JLT,Q,MF11,Q1,IAS)
        CALL CBATRAN3(NF,JLT,Q,MF22,Q2,IAS)
        CALL CBATRAN3(NF,JLT,Q,MF33,Q3,IAS)
        CALL CBATRAN3(NF,JLT,Q,MF44,Q4,IAS)
        CALL CBATRAN3(NF,JLT,Q,MF12,Q2,IAS)
        CALL CBATRAN3(NF,JLT,Q,MF13,Q3,IAS)
        CALL CBATRAN3(NF,JLT,Q,MF14,Q4,IAS)
        CALL CBATRAN3(NF,JLT,Q,MF23,Q3,IAS)
        CALL CBATRAN3(NF,JLT,Q,MF24,Q4,IAS)
        CALL CBATRAN3(NF,JLT,Q,MF34,Q4,IAS)
        CALL CBATRAN3(NF,JLT,Q1,FM12,Q,IAS)
        CALL CBATRAN3(NF,JLT,Q1,FM13,Q,IAS)
        CALL CBATRAN3(NF,JLT,Q1,FM14,Q,IAS)
        CALL CBATRAN3(NF,JLT,Q2,FM23,Q,IAS)
        CALL CBATRAN3(NF,JLT,Q2,FM24,Q,IAS)
        CALL CBATRAN3(NF,JLT,Q3,FM34,Q,IAS)
       END IF  !IF (IDRIL==0.O.AND.IORTH==0) THEN
       END IF !(IPROJF==1) THEN
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
         DO EP=JFT,JLT
          KE11(J,I,EP)=KE11(I,J,EP)
          KE22(J,I,EP)=KE22(I,J,EP)
          KE33(J,I,EP)=KE33(I,J,EP)
          KE44(J,I,EP)=KE44(I,J,EP)
         ENDDO
        ENDDO
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  CZTRAN2                       source/elements/shell/coquez/czsumg3.F
Chd|-- called by -----------
Chd|        CZSUMG3                       source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CZTRAN2(JFT   ,JLT  ,VQI  ,KK,VQJ,ISYM,VQ) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real 
     .   VQI(3,3,*),VQJ(3,3,*),KK(3,3,*),VQ(3,3,*)
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
     4              VQ(3,I,EP)*KK(3,3,EP)*VQ(3,J,EP)
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
     4              VQ(3,I,EP)*KK(3,3,EP)*VQ(3,J,EP)
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
Chd|  CZTRANDR                      source/elements/shell/coquez/czsumg3.F
Chd|-- called by -----------
Chd|        CZSUMG3                       source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CZTRANDR(JFT   ,JLT  ,VQI  ,KK,VQJ,ISYM) 
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
Chd|  CZPROJK                       source/elements/shell/coquez/czsumg3.F
Chd|-- called by -----------
Chd|        CZSUMG3                       source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|        SETPROJK                      source/elements/shell/coquez/czsumg3.F
Chd|        SETPROJKZ                     source/elements/shell/coquez/czsumg3.F
Chd|        SET_RSJ                       source/elements/shell/coquez/czsumg3.F
Chd|        SET_RSJ2                      source/elements/shell/coquez/czsumg3.F
Chd|====================================================================
      SUBROUTINE CZPROJK(
     1                    JFT    ,JLT    ,VQN    ,Q     ,IPLAT,   
     3                     K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     4                     M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     5                     MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33, 
     6                     MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34, 
     7                     CORELV,Z1   ,IDRIL)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,IPLAT(*),IDRIL
      my_real 
     .   VQN(MVSIZ,3,4),Q(3,3,*)
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
     .    CORELV(MVSIZ,2,4),Z1(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I, J, K,L,EP,IS,IAS,NF,MI,MJ,M,ND
      my_real 
     .    DR(7,7,MVSIZ),DRZ(3,MVSIZ),
     .    R1(6,7,MVSIZ),R2(6,7,MVSIZ),R3(6,7,MVSIZ),R4(6,7,MVSIZ),
     .    RZ1(3,3,MVSIZ),RZ2(3,3,MVSIZ),RZ3(3,3,MVSIZ),RZ4(3,3,MVSIZ),
     .    DI(6),DB(3,4),BTDB(4,4),Z2,DETA,BTB(6),D(6),
     .    XX,YY,ZZ,XY,XZ,YZ,ABC,XXYZ2,YYXZ2,ZZXY2,
     .    QN1(3,MVSIZ),QN2(3,MVSIZ),QN3(3,MVSIZ),QN4(3,MVSIZ)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
        DO M=JFT,JLT 
         I=IPLAT(M)
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
        IF (IDRIL > 0) THEN
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
         DI(3) = (ABC-ZZXY2)*DETA/MAX(D(3),EM20)
         DI(5) = (D(6)*D(4)-D(5)*D(2))*DETA
         DI(6) = (D(4)*D(5)-D(6)*D(1))*DETA
C
         DRZ(1,M)=  DI(5)
         DRZ(2,M)=  DI(6)
         DRZ(3,M)=  DI(3)
        END IF !(IDRIL > 0) THEN
         BTB(1)= VQN(I,1,1)*VQN(I,1,1)+VQN(I,1,2)*VQN(I,1,2)
     1          +VQN(I,1,3)*VQN(I,1,3)+VQN(I,1,4)*VQN(I,1,4)
         BTB(2)= VQN(I,2,1)*VQN(I,2,1)+VQN(I,2,2)*VQN(I,2,2)
     1          +VQN(I,2,3)*VQN(I,2,3)+VQN(I,2,4)*VQN(I,2,4)
         BTB(3)= VQN(I,3,1)*VQN(I,3,1)+VQN(I,3,2)*VQN(I,3,2)
     1          +VQN(I,3,3)*VQN(I,3,3)+VQN(I,3,4)*VQN(I,3,4)
         BTB(4)= VQN(I,1,1)*VQN(I,2,1)+VQN(I,1,2)*VQN(I,2,2)
     1          +VQN(I,1,3)*VQN(I,2,3)+VQN(I,1,4)*VQN(I,2,4)
         BTB(5)= VQN(I,1,1)*VQN(I,3,1)+VQN(I,1,2)*VQN(I,3,2)
     1          +VQN(I,1,3)*VQN(I,3,3)+VQN(I,1,4)*VQN(I,3,4)
         BTB(6)= VQN(I,2,1)*VQN(I,3,1)+VQN(I,2,2)*VQN(I,3,2)
     1          +VQN(I,2,3)*VQN(I,3,3)+VQN(I,2,4)*VQN(I,3,4)
         D(1)= YY+ZZ+FOUR-BTB(1)
         D(2)= XX+ZZ+FOUR-BTB(2)
         D(3)= XX+YY+FOUR-BTB(3)
         D(4)= -XY-BTB(4)
         D(5)= -XZ-BTB(5)
         D(6)= -YZ-BTB(6)
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
         DO J=1,4
          DB(1,J)= DI(1)*VQN(I,1,J)+DI(4)*VQN(I,2,J)
     1              +DI(5)*VQN(I,3,J)
          DB(2,J)= DI(4)*VQN(I,1,J)+DI(2)*VQN(I,2,J)
     1              +DI(6)*VQN(I,3,J)
          DB(3,J)= DI(5)*VQN(I,1,J)+DI(6)*VQN(I,2,J)
     1              +DI(3)*VQN(I,3,J)
         ENDDO
         DO L=1,4
          DO J=L,4
           BTDB(L,J)= VQN(I,1,L)*DB(1,J)+VQN(I,2,L)*DB(2,J)
     1               +VQN(I,3,L)*DB(3,J)
          ENDDO
         ENDDO
C
         DR(1,1,M)=  DI(1)    
         DR(2,2,M)=  DI(2)    
         DR(3,3,M)=  DI(3)    
         DR(1,2,M)=  DI(4)    
         DR(1,3,M)=  DI(5)    
         DR(2,3,M)=  DI(6)    
         DO J=1,4
          DR(1,J+3,M)=  -DB(1,J)    
          DR(2,J+3,M)=  -DB(2,J)      
          DR(3,J+3,M)=  -DB(3,J)      
          DR(J+3,J+3,M)=  ONE+BTDB(J,J)     
          DO K=J+1,4
           DR(J+3,K+3,M)=  BTDB(J,K)     
          ENDDO
         ENDDO
        END DO !M= 
C
        DO M=JFT,JLT 
         DO L=1,7
          DO J=L+1,7
           DR(J,L,M)=DR(L,J,M)
          ENDDO
         ENDDO
        END DO 
C
        CALL SET_RSJ(R1     ,R2     ,R3     ,R4    ,Z1    ,
     .               JFT   ,JLT    ,IPLAT  ,VQN    ,CORELV) 
C -----------PROJECTION---------
       IF (IDRIL == 0) THEN
        CALL SETPROJK(DR    ,R1     ,R2     ,R3     ,R4    ,
     3                K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     4                M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     5                MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33, 
     6                MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34, 
     7                Q   ,JFT ,JLT  ) 
       ELSE
        DO M=JFT,JLT 
         I=IPLAT(M)
         DO J=1,3
          QN1(J,M)= VQN(I,J,1)
          QN2(J,M)= VQN(I,J,2)
          QN3(J,M)= VQN(I,J,3)
          QN4(J,M)= VQN(I,J,4)
         ENDDO
        END DO 
C
        CALL SET_RSJ2(RZ1     ,RZ2     ,RZ3     ,RZ4    ,Z1    ,
     .                JFT     ,JLT     ,CORELV  ,IPLAT  )
C
        CALL SETPROJKZ(DR    ,R1     ,R2     ,R3     ,R4    ,
     3                K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     4                M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     5                MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33, 
     6                MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34, 
     7                DRZ   ,RZ1     ,RZ2     ,RZ3     ,RZ4    ,
     8                Q   ,JFT ,JLT ,QN1 ,QN2 ,QN3 ,QN4 ) 
       END IF !(IDRIL == 0) THEN
C 
      RETURN
      END
Chd|====================================================================
Chd|  SET_RSJ                       source/elements/shell/coquez/czsumg3.F
Chd|-- called by -----------
Chd|        CZPROJK                       source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|        SET_RSJ33                     source/elements/shell/coquez/czsumg3.F
Chd|====================================================================
      SUBROUTINE SET_RSJ(R1     ,R2     ,R3     ,R4    ,Z1    ,
     .                   JFT   ,JLT    ,IPLAT  ,VQN    ,CORELV) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,IPLAT(*)
      my_real 
     .    VQN(MVSIZ,3,4),CORELV(MVSIZ,2,4),
     .    R1(6,7,*),R2(6,7,*),R3(6,7,*),R4(6,7,*),Z1(*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP,M,L
      my_real
     .    S,XI(MVSIZ),YI(MVSIZ),ZI(MVSIZ)
C-------------------------------------------------------------
        DO M=JFT,JLT 
         DO L=1,6
          DO J=1,7
           R1(L,J,M)=ZERO
           R2(L,J,M)=ZERO
           R3(L,J,M)=ZERO
           R4(L,J,M)=ZERO
          ENDDO
         ENDDO
        END DO 
C
        DO M=JFT,JLT 
         I=IPLAT(M)
         XI(M)=CORELV(I,1,1)
         YI(M)=CORELV(I,2,1)
         ZI(M)=Z1(I)
        END DO 
        CALL SET_RSJ33(XI ,YI, ZI  ,R1  ,JFT,JLT) 
        DO M=JFT,JLT 
         I=IPLAT(M)
         XI(M)=CORELV(I,1,2)
         YI(M)=CORELV(I,2,2)
         ZI(M)=-Z1(I)
        END DO 
        CALL SET_RSJ33(XI ,YI, ZI  ,R2  ,JFT,JLT) 
        DO M=JFT,JLT 
         I=IPLAT(M)
         XI(M)=CORELV(I,1,3)
         YI(M)=CORELV(I,2,3)
         ZI(M)=Z1(I)
        END DO 
        CALL SET_RSJ33(XI ,YI, ZI  ,R3  ,JFT,JLT) 
        DO M=JFT,JLT 
         I=IPLAT(M)
         XI(M)=CORELV(I,1,4)
         YI(M)=CORELV(I,2,4)
         ZI(M)=-Z1(I)
        END DO 
        CALL SET_RSJ33(XI ,YI, ZI  ,R4  ,JFT,JLT) 
        DO M=JFT,JLT 
         DO L=1,3
          R1(3+L,L,M) = ONE
          R2(3+L,L,M) = ONE
          R3(3+L,L,M) = ONE
          R4(3+L,L,M) = ONE
         END DO 
        END DO  
C
        DO M=JFT,JLT 
         I=IPLAT(M)
         DO J=1,3
          R1(3+J,4,M)=VQN(I,J,1)
          R2(3+J,5,M)=VQN(I,J,2)
          R3(3+J,6,M)=VQN(I,J,3)
          R4(3+J,7,M)=VQN(I,J,4)
         END DO 
        END DO 
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  SET_RSJ2                      source/elements/shell/coquez/czsumg3.F
Chd|-- called by -----------
Chd|        CZPROJK                       source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|        SET_RI33                      source/elements/shell/coqueba/cbasumg3.F
Chd|====================================================================
      SUBROUTINE SET_RSJ2(R1     ,R2     ,R3     ,R4    ,Z1    ,
     .                    JFT    ,JLT    ,CORELV ,IPLAT  ) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,IPLAT(*)
      my_real 
     .    CORELV(MVSIZ,2,4),
     .    R1(3,3,*),R2(3,3,*),R3(3,3,*),R4(3,3,*),Z1(*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP,M,L
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
         I=IPLAT(M)
         XI(M)=CORELV(I,1,1)
         YI(M)=CORELV(I,2,1)
         ZI(M)=Z1(I)
        END DO 
        CALL SET_RI33(XI ,YI, ZI  ,R1  ,JFT,JLT) 
        DO M=JFT,JLT 
         I=IPLAT(M)
         XI(M)=CORELV(I,1,2)
         YI(M)=CORELV(I,2,2)
         ZI(M)=-Z1(I)
        END DO 
        CALL SET_RI33(XI ,YI, ZI  ,R2  ,JFT,JLT) 
        DO M=JFT,JLT 
         I=IPLAT(M)
         XI(M)=CORELV(I,1,3)
         YI(M)=CORELV(I,2,3)
         ZI(M)=Z1(I)
        END DO 
        CALL SET_RI33(XI ,YI, ZI  ,R3  ,JFT,JLT) 
        DO M=JFT,JLT 
         I=IPLAT(M)
         XI(M)=CORELV(I,1,4)
         YI(M)=CORELV(I,2,4)
         ZI(M)=-Z1(I)
        END DO 
        CALL SET_RI33(XI ,YI, ZI  ,R4  ,JFT,JLT) 
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  SET_RSJ33                     source/elements/shell/coquez/czsumg3.F
Chd|-- called by -----------
Chd|        SET_RSJ                       source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SET_RSJ33(XI ,YI, ZI  ,RI  ,JFT,JLT) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real 
     .   XI(*) ,YI(*), ZI(*),RI(6,7,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,L
C--------------RI=Q*RSI-------------------------------
C         DO I=JFT,JLT 
C          RI(1,1,I)=-VQ(1,2,I)*ZI(I)+VQ(1,3,I)*YI(I)
C          RI(1,2,I)=VQ(1,1,I)*ZI(I)-VQ(1,3,I)*XI(I)
C          RI(1,3,I)=-VQ(1,1,I)*YI(I)+VQ(1,2,I)*XI(I)
C          RI(2,1,I)=-VQ(2,2,I)*ZI(I)+VQ(2,3,I)*YI(I)
C          RI(2,2,I)=VQ(2,1,I)*ZI(I)-VQ(2,3,I)*XI(I)
C          RI(2,3,I)=-VQ(2,1,I)*YI(I)+VQ(2,2,I)*XI(I)
C          RI(3,1,I)=-VQ(3,2,I)*ZI(I)+VQ(3,3,I)*YI(I)
C          RI(3,2,I)=VQ(3,1,I)*ZI(I)-VQ(3,3,I)*XI(I)
C          RI(3,3,I)=-VQ(3,1,I)*YI(I)+VQ(3,2,I)*XI(I)
C         ENDDO
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
Chd|  SETPROJK                      source/elements/shell/coquez/czsumg3.F
Chd|-- called by -----------
Chd|        CZPROJK                       source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|        CZTRANK33                     source/elements/shell/coquez/czsumg3.F
Chd|        TRANKL1                       source/elements/shell/coquez/czsumg3.F
Chd|        TRANKLQ                       source/elements/shell/coquez/czsumg3.F
Chd|        TRANQIKQJ                     source/elements/shell/coquez/czsumg3.F
Chd|        TRANQIKQJ67                   source/elements/shell/coquez/czsumg3.F
Chd|====================================================================
      SUBROUTINE SETPROJK(DR    ,R1     ,R2     ,R3     ,R4    ,
     3                     K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     4                     M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     5                     MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33, 
     6                     MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34, 
     .                     VQ  ,JFT  ,JLT  ) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real 
     .    DR(7,7,*),VQ(3,3,*),
     .    R1(6,7,*),R2(6,7,*),R3(6,7,*),R4(6,7,*),
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
     .    KL(6,6,MVSIZ),KQ(6,6,MVSIZ),
     .    MZ11(MVSIZ),MZ22(MVSIZ),MZ33(MVSIZ),MZ44(MVSIZ),MZ12(MVSIZ),
     .    MZ13(MVSIZ),MZ14(MVSIZ),MZ23(MVSIZ),MZ24(MVSIZ),MZ34(MVSIZ)
         DATA IS/1/,IAS/0/,IT/1/,IAT/0/
      my_real,
     .  DIMENSION(:,:,:), ALLOCATABLE:: P,KE
C-------------------------------------------------------------
        ALLOCATE(P(24,24,MVSIZ))
        ALLOCATE(KE(24,24,MVSIZ))

C---Save Mij(3,3) prevent singularity-----
        DO EP=JFT,JLT 
         MZ11(EP)= M11(3,3,EP)
         MZ22(EP)= M22(3,3,EP)
         MZ33(EP)= M33(3,3,EP)
         MZ44(EP)= M44(3,3,EP)
         MZ12(EP)= M12(3,3,EP)
         MZ13(EP)= M13(3,3,EP)
         MZ14(EP)= M14(3,3,EP)
         MZ23(EP)= M23(3,3,EP)
         MZ24(EP)= M24(3,3,EP)
         MZ34(EP)= M34(3,3,EP)
         M11(3,3,EP) =ZERO
         M22(3,3,EP) =ZERO
         M33(3,3,EP) =ZERO
         M44(3,3,EP) =ZERO
         M12(3,3,EP) =ZERO
         M13(3,3,EP) =ZERO
         M14(3,3,EP) =ZERO
         M23(3,3,EP) =ZERO
         M24(3,3,EP) =ZERO
         M34(3,3,EP) =ZERO
        ENDDO
       CALL TRANQIKQJ67(JFT   ,JLT    ,R1  ,DR , R1 ,KL, IS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IS ) 
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IT ) 
C-----------P11Q
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
       CALL TRANQIKQJ67(JFT   ,JLT    ,R2  ,DR , R2 ,KL, IS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IS ) 
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ  ,IT ) 
C-----------P22Q
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
       CALL TRANQIKQJ67(JFT   ,JLT    ,R3  ,DR , R3 ,KL, IS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IS ) 
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IT ) 
C-----------P33Q
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
       CALL TRANQIKQJ67(JFT   ,JLT    ,R4  ,DR , R4 ,KL, IS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IS ) 
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IT ) 
C-----------P44Q
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
       CALL TRANQIKQJ67(JFT   ,JLT    ,R1  ,DR , R2 ,KL, IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IT ) 
C-----------P12Q
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
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ  ,IAT ) 
C-----------P21Q
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+6,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
       CALL TRANQIKQJ67(JFT   ,JLT    ,R1  ,DR , R3 ,KL, IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ  ,IT ) 
C-----------P13Q
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
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IAT ) 
C-----------P31Q
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+12,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
       CALL TRANQIKQJ67(JFT   ,JLT    ,R1  ,DR , R4 ,KL, IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IT ) 
C-----------P14Q
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
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IAT ) 
C-----------P41Q
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+18,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
       CALL TRANQIKQJ67(JFT   ,JLT    ,R2  ,DR , R3 ,KL, IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IT ) 
C-----------P23Q
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
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IAT ) 
C-----------P32Q
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+12,J+6,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
       CALL TRANQIKQJ67(JFT   ,JLT    ,R2  ,DR , R4 ,KL, IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IT ) 
C-----------P24Q
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
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IAT ) 
C-----------P42Q
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+18,J+6,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
       CALL TRANQIKQJ67(JFT   ,JLT    ,R3  ,DR , R4 ,KL, IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IT ) 
C-----------P34Q
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
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KL ,KQ ,IAT ) 
C-----------P43Q
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
c          P(J,I,EP)= P(I,J,EP)
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
C---prevent singularity-----
        CALL CZTRANK33(JFT   ,JLT  ,VQ  ,MZ11,M11 ,IS) 
        CALL CZTRANK33(JFT   ,JLT  ,VQ  ,MZ22,M22 ,IS) 
        CALL CZTRANK33(JFT   ,JLT  ,VQ  ,MZ33,M33 ,IS) 
        CALL CZTRANK33(JFT   ,JLT  ,VQ  ,MZ44,M44 ,IS) 
        CALL CZTRANK33(JFT   ,JLT  ,VQ  ,MZ12,M12 ,IAS) 
        CALL CZTRANK33(JFT   ,JLT  ,VQ  ,MZ13,M13 ,IAS) 
        CALL CZTRANK33(JFT   ,JLT  ,VQ  ,MZ14,M14 ,IAS) 
        CALL CZTRANK33(JFT   ,JLT  ,VQ  ,MZ23,M23 ,IAS) 
        CALL CZTRANK33(JFT   ,JLT  ,VQ  ,MZ24,M24 ,IAS) 
        CALL CZTRANK33(JFT   ,JLT  ,VQ  ,MZ34,M34 ,IAS) 
C-----------
        DEALLOCATE(P)
        DEALLOCATE(KE)
      RETURN
      END
Chd|====================================================================
Chd|  TRANQIKQJ67                   source/elements/shell/coquez/czsumg3.F
Chd|-- called by -----------
Chd|        SETPROJK                      source/elements/shell/coquez/czsumg3.F
Chd|        SETPROJKZ                     source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TRANQIKQJ67(JFT   ,JLT    ,VQI  ,KK , VQJ,KD ,ISYM) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,ISYM
      my_real 
     .   VQI(6,7,*), VQJ(6,7,*),KK(7,7,*),KD(6,6,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP,L
      my_real
     .    K(6,6)
C--------------QI(6,7)KK(7,7)QJ^t(7,6)---------------------------------
       IF (ISYM==1) THEN
        DO I=1,6 
        DO J=I,6 
         DO EP=JFT,JLT 
          K(I,J)=ZERO
          DO L=1,7 
           K(I,J)=K(I,J)+VQI(I,1,EP)*KK(1,L,EP)*VQJ(J,L,EP)+
     1                   VQI(I,2,EP)*KK(2,L,EP)*VQJ(J,L,EP)+
     1                   VQI(I,3,EP)*KK(3,L,EP)*VQJ(J,L,EP)+
     1                   VQI(I,4,EP)*KK(4,L,EP)*VQJ(J,L,EP)+
     1                   VQI(I,5,EP)*KK(5,L,EP)*VQJ(J,L,EP)+
     1                   VQI(I,6,EP)*KK(6,L,EP)*VQJ(J,L,EP)+
     1                   VQI(I,7,EP)*KK(7,L,EP)*VQJ(J,L,EP)
          ENDDO  
          KD(I,J,EP)= K(I,J)
          KD(J,I,EP)= K(I,J)
         ENDDO
        ENDDO
        ENDDO
C
       ELSE
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          K(I,J)=ZERO
          DO L=1,7 
           K(I,J)=K(I,J)+VQI(I,1,EP)*KK(1,L,EP)*VQJ(J,L,EP)+
     1                   VQI(I,2,EP)*KK(2,L,EP)*VQJ(J,L,EP)+
     1                   VQI(I,3,EP)*KK(3,L,EP)*VQJ(J,L,EP)+
     1                   VQI(I,4,EP)*KK(4,L,EP)*VQJ(J,L,EP)+
     1                   VQI(I,5,EP)*KK(5,L,EP)*VQJ(J,L,EP)+
     1                   VQI(I,6,EP)*KK(6,L,EP)*VQJ(J,L,EP)+
     1                   VQI(I,7,EP)*KK(7,L,EP)*VQJ(J,L,EP)
          ENDDO  
          KD(I,J,EP)= K(I,J)
         ENDDO
        ENDDO
        ENDDO
C
       ENDIF
      RETURN
      END
Chd|====================================================================
Chd|  TRANQIKQJ                     source/elements/shell/coquez/czsumg3.F
Chd|-- called by -----------
Chd|        SETPROJK                      source/elements/shell/coquez/czsumg3.F
Chd|        SETPROJKBA                    source/elements/shell/coqueba/cbasumg3.F
Chd|        SETPROJKS6                    source/elements/thickshell/solide6c/setprojks6.F
Chd|        SETPROJKZ                     source/elements/shell/coquez/czsumg3.F
Chd|        SETPROJKZ1                    source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TRANQIKQJ(JFT   ,JLT    ,VQI   ,KK,VQJ ,ND ,ISYM) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,ISYM,ND
      my_real 
     .   VQI(ND,ND,*), VQJ(ND,ND,*),KK(ND,ND,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP,K,L
      my_real,
     .  DIMENSION(:,:,:), ALLOCATABLE:: KD
C--------------QI^tKKQJ---------------------------------
        ALLOCATE(KD(ND,ND,MVSIZ))
       IF (ISYM==1) THEN
        DO I=1,ND 
        DO J=I,ND 
         DO EP=JFT,JLT 
          KD(I,J,EP)=ZERO
          DO K=1,ND 
           DO L=1,ND 
            KD(I,J,EP)=KD(I,J,EP)+VQI(K,I,EP)*KK(K,L,EP)*VQJ(L,J,EP)
           ENDDO 
          ENDDO 
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,ND 
        DO J=I,ND 
         DO EP=JFT,JLT 
          KK(I,J,EP)= KD(I,J,EP)
          KK(J,I,EP)= KD(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
       ELSE
        DO I=1,ND 
        DO J=1,ND 
         DO EP=JFT,JLT 
          KD(I,J,EP)=ZERO
          DO K=1,ND 
           DO L=1,ND 
            KD(I,J,EP)=KD(I,J,EP)+VQI(K,I,EP)*KK(K,L,EP)*VQJ(L,J,EP)
           ENDDO 
          ENDDO 
         ENDDO
        ENDDO
        ENDDO
C        
        DO I=1,ND 
        DO J=1,ND 
         DO EP=JFT,JLT 
          KK(I,J,EP)= KD(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
       ENDIF
C       
        DEALLOCATE(KD)
      RETURN
      END
Chd|====================================================================
Chd|  TRANKL1                       source/elements/shell/coquez/czsumg3.F
Chd|-- called by -----------
Chd|        SETPROJK                      source/elements/shell/coquez/czsumg3.F
Chd|        SETPROJKBA                    source/elements/shell/coqueba/cbasumg3.F
Chd|        SETPROJKZ                     source/elements/shell/coquez/czsumg3.F
Chd|        SETPROJKZ1                    source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TRANKL1(JFT   ,JLT    ,KL  ,IS ) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,IS
      my_real 
     .   KL(6,6,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP,K,L
C--------------KL=1-KL--------------------------
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          KL(I,J,EP)= -KL(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
       IF (IS==1) THEN
        DO I=1,6 
         DO EP=JFT,JLT 
          KL(I,I,EP)= KL(I,I,EP) + ONE
         ENDDO
        ENDDO
       END IF 
C
      RETURN
      END
Chd|====================================================================
Chd|  TRANKLQ                       source/elements/shell/coquez/czsumg3.F
Chd|-- called by -----------
Chd|        SETPROJK                      source/elements/shell/coquez/czsumg3.F
Chd|        SETPROJKBA                    source/elements/shell/coqueba/cbasumg3.F
Chd|        SETPROJKZ                     source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TRANKLQ(JFT   ,JLT    ,VQ   ,KL  ,KD  ,IT) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,IT
      my_real 
     .   VQ(3,3,*), KL(6,6,*), KD(6,6,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP,K,L
C--------------KQ=KL*VQ-------IT=0-> KQ=KL^t*VQ--------------------------
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          KD(I,J,EP)= ZERO
         ENDDO
        ENDDO
        ENDDO
C        
       IF (IT==1) THEN
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          DO K=1,3 
            KD(I,J,EP)=KD(I,J,EP)+KL(I,K,EP)*VQ(K,J,EP)
            KD(I+3,J+3,EP)=KD(I+3,J+3,EP)+KL(I+3,K+3,EP)*VQ(K,J,EP)
            KD(I,J+3,EP)=KD(I,J+3,EP)+KL(I,K+3,EP)*VQ(K,J,EP)
            KD(I+3,J,EP)=KD(I+3,J,EP)+KL(I+3,K,EP)*VQ(K,J,EP)
          ENDDO 
         ENDDO
        ENDDO
        ENDDO
       ELSE
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          DO K=1,3 
            KD(I,J,EP)=KD(I,J,EP)+KL(K,I,EP)*VQ(K,J,EP)
            KD(I+3,J+3,EP)=KD(I+3,J+3,EP)+KL(K+3,I+3,EP)*VQ(K,J,EP)
            KD(I,J+3,EP)=KD(I,J+3,EP)+KL(K+3,I,EP)*VQ(K,J,EP)
            KD(I+3,J,EP)=KD(I+3,J,EP)+KL(K,I+3,EP)*VQ(K,J,EP)
          ENDDO 
         ENDDO
        ENDDO
        ENDDO
       END IF 
      RETURN
      END
Chd|====================================================================
Chd|  CZTRANK33                     source/elements/shell/coquez/czsumg3.F
Chd|-- called by -----------
Chd|        SETPROJK                      source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CZTRANK33(JFT   ,JLT  ,VQ  ,K33 ,KK ,ISYM) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real 
     .   K33(*),KK(3,3,*),VQ(3,3,*)
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
          K(I,J,EP)= VQ(3,I,EP)*K33(EP)*VQ(3,J,EP)
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,3 
        DO J=I,3 
         DO EP=JFT,JLT 
          KK(I,J,EP)= KK(I,J,EP)+K(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
       ELSE
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          K(I,J,EP)=VQ(3,I,EP)*K33(EP)*VQ(3,J,EP)
         ENDDO
        ENDDO
        ENDDO
C
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          KK(I,J,EP)= KK(I,J,EP)+K(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
       ENDIF
      RETURN
      END
Chd|====================================================================
Chd|  SETPROJKZ                     source/elements/shell/coquez/czsumg3.F
Chd|-- called by -----------
Chd|        CZPROJK                       source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|        TRANKL1                       source/elements/shell/coquez/czsumg3.F
Chd|        TRANKLQ                       source/elements/shell/coquez/czsumg3.F
Chd|        TRANQIKQJ                     source/elements/shell/coquez/czsumg3.F
Chd|        TRANQIKQJ67                   source/elements/shell/coquez/czsumg3.F
Chd|        TRANQIKQJRZ                   source/elements/shell/coquez/czsumg3.F
Chd|====================================================================
      SUBROUTINE SETPROJKZ(DR    ,R1     ,R2     ,R3     ,R4    ,
     3                     K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     4                     M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     5                     MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33, 
     6                     MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34, 
     7                     DRZ   ,RZ1     ,RZ2   ,RZ3     ,RZ4    ,
     .                     VQ  ,JFT  ,JLT  ,QN1 ,QN2 ,QN3 ,QN4 ) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real 
     .    DR(7,7,*),VQ(3,3,*),DRZ(3,*),
     .    R1(6,7,*),R2(6,7,*),R3(6,7,*),R4(6,7,*),
     .    RZ1(3,3,*),RZ2(3,3,*),RZ3(3,3,*),RZ4(3,3,*),
     .    K11(3,3,*),K12(3,3,*),K13(3,3,*),K14(3,3,*),
     .    K22(3,3,*),K23(3,3,*),K24(3,3,*),K33(3,3,*),
     .    M11(3,3,*),M12(3,3,*),M13(3,3,*),M14(3,3,*),
     .    M22(3,3,*),M23(3,3,*),M24(3,3,*),M33(3,3,*),
     .    MF11(3,3,*),MF12(3,3,*),MF13(3,3,*),MF14(3,3,*),
     .    MF22(3,3,*),MF23(3,3,*),MF24(3,3,*),MF33(3,3,*),
     .    FM12(3,3,*),FM13(3,3,*),FM14(3,3,*),
     .    FM23(3,3,*),FM24(3,3,*),FM34(3,3,*),
     .    K34(3,3,*),K44(3,3,*),M34(3,3,*),M44(3,3,*),
     .    MF34(3,3,*),MF44(3,3,*),QN1(3,*),QN2(3,*),QN3(3,*),QN4(3,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP,IS,IAS,IT,IAT
      my_real
     .    KL(6,6,MVSIZ),KQ(6,6,MVSIZ),KR(6,6,MVSIZ)
         DATA IS/1/,IAS/0/,IT/1/,IAT/0/
      my_real,
     .  DIMENSION(:,:,:), ALLOCATABLE:: P,KE
C-------------------------------------------------------------
        ALLOCATE(P(24,24,MVSIZ))
        ALLOCATE(KE(24,24,MVSIZ))
C-----------P11Q---------
       CALL TRANQIKQJ67(JFT   ,JLT    ,R1  ,DR , R1 ,KL, IS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IS ) 
       CALL TRANQIKQJRZ(JFT   ,JLT    ,RZ1 ,DRZ ,RZ1,KL ,KR ,IT ,IS )
       CALL TRANKLQ(JFT   ,JLT    ,VQ  ,KR  ,KQ ,IT ) 
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
C-----------P22Q
       CALL TRANQIKQJ67(JFT   ,JLT    ,R2  ,DR , R2 ,KL, IS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IS ) 
       CALL TRANQIKQJRZ(JFT   ,JLT    ,RZ2 ,DRZ ,RZ2,KL ,KR ,IT ,IS )
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KR ,KQ  ,IT ) 
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
C-----------P33Q
       CALL TRANQIKQJ67(JFT   ,JLT    ,R3  ,DR , R3 ,KL, IS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IS ) 
       CALL TRANQIKQJRZ(JFT   ,JLT    ,RZ3 ,DRZ ,RZ3,KL ,KR ,IT ,IS )
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KR ,KQ ,IT ) 
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
C-----------P44Q
       CALL TRANQIKQJ67(JFT   ,JLT    ,R4  ,DR , R4 ,KL, IS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IS ) 
       CALL TRANQIKQJRZ(JFT   ,JLT    ,RZ4 ,DRZ ,RZ4,KL ,KR ,IT ,IS )
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KR ,KQ ,IT ) 
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
C-----------P12Q
       CALL TRANQIKQJ67(JFT   ,JLT    ,R1  ,DR , R2 ,KL, IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANQIKQJRZ(JFT   ,JLT    ,RZ1 ,DRZ ,RZ2,KL ,KR ,IT ,IAS)
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KR ,KQ ,IT ) 
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
C-----------P21Q
       CALL TRANQIKQJRZ(JFT   ,JLT    ,RZ1 ,DRZ ,RZ2,KL ,KR ,IAT ,IAS)
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KR ,KQ  ,IAT ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+6,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P13Q
       CALL TRANQIKQJ67(JFT   ,JLT    ,R1  ,DR , R3 ,KL, IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANQIKQJRZ(JFT   ,JLT    ,RZ1 ,DRZ ,RZ3,KL ,KR ,IT ,IAS)
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KR ,KQ  ,IT ) 
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
C-----------P31Q
       CALL TRANQIKQJRZ(JFT   ,JLT    ,RZ1 ,DRZ ,RZ3,KL ,KR ,IAT ,IAS)
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KR ,KQ ,IAT ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+12,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P14Q
       CALL TRANQIKQJ67(JFT   ,JLT    ,R1  ,DR , R4 ,KL, IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANQIKQJRZ(JFT   ,JLT    ,RZ1 ,DRZ ,RZ4,KL ,KR ,IT ,IAS)
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KR ,KQ ,IT ) 
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
C-----------P41Q
       CALL TRANQIKQJRZ(JFT   ,JLT    ,RZ1 ,DRZ ,RZ4,KL ,KR ,IAT ,IAS)
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KR ,KQ ,IAT ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+18,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P23Q
       CALL TRANQIKQJ67(JFT   ,JLT    ,R2  ,DR , R3 ,KL, IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANQIKQJRZ(JFT   ,JLT    ,RZ2 ,DRZ ,RZ3,KL ,KR ,IT ,IAS)
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KR ,KQ ,IT ) 
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
C-----------P32Q
       CALL TRANQIKQJRZ(JFT   ,JLT    ,RZ2 ,DRZ ,RZ3,KL ,KR ,IAT ,IAS)
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KR ,KQ ,IAT ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+12,J+6,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P24Q
       CALL TRANQIKQJ67(JFT   ,JLT    ,R2  ,DR , R4 ,KL, IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANQIKQJRZ(JFT   ,JLT    ,RZ2 ,DRZ ,RZ4,KL ,KR ,IT ,IAS)
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KR ,KQ ,IT ) 
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
C-----------P42Q
       CALL TRANQIKQJRZ(JFT   ,JLT    ,RZ2 ,DRZ ,RZ4,KL ,KR ,IAT ,IAS)
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KR ,KQ ,IAT ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+18,J+6,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P34Q
       CALL TRANQIKQJ67(JFT   ,JLT    ,R3  ,DR , R4 ,KL ,IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANQIKQJRZ(JFT   ,JLT    ,RZ3 ,DRZ ,RZ4,KL ,KR ,IT ,IAS)
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KR ,KQ ,IT ) 
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
C-----------P43Q
       CALL TRANQIKQJRZ(JFT   ,JLT    ,RZ3 ,DRZ ,RZ4,KL ,KR ,IAT ,IAS)
       CALL TRANKLQ(JFT   ,JLT    ,VQ   ,KR ,KQ ,IAT ) 
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
Chd|  TRANKL2                       source/elements/shell/coquez/czsumg3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TRANKL2(JFT   ,JLT    ,KL  ,QNI  ,KR  ,IT) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,IT
      my_real 
     .   KL(6,6,*),QNI(3,*),KR(6,6,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,EP,J,K
C--------------update KL(6,j),j=4,6--by QNI------------------------
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          KR(I,J,EP)= KL(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C
       IF (IT == 1 )THEN
        DO J=4,6
           K=J-3 
         DO EP=JFT,JLT 
          KR(6,K,EP)= ZERO
          KR(6,J,EP)= QNI(K,EP)
         ENDDO
        ENDDO
       ELSE
        DO J=4,6 
           K=J-3 
         DO EP=JFT,JLT 
          KR(K,6,EP)= ZERO
          KR(J,6,EP)= QNI(K,EP)
         ENDDO
        ENDDO
       END IF !(IT == 1 )THEN
      RETURN
      END
Chd|====================================================================
Chd|  CZPROJKR                      source/elements/shell/coquez/czsumg3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SETPROJKZ1                    source/elements/shell/coquez/czsumg3.F
Chd|        SET_RSJ1                      source/elements/shell/coqueba/cbasumg3.F
Chd|====================================================================
      SUBROUTINE CZPROJKR(
     1                    JFT    ,JLT    ,VQN    ,VQ     ,IPLAT  ,
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
     .    CORELV(MVSIZ,2,4),Z1(*),VQN(3,4,*),VQ(3,3,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I, J, K,L,EP,IS,IAS,NF,MI,MJ,M,ND
      my_real
     .    DR(3,3,MVSIZ),PP(3,3,4,MVSIZ),
     .    R1(3,3,MVSIZ),R2(3,3,MVSIZ),R3(3,3,MVSIZ),R4(3,3,MVSIZ),
     .    DI(6),Z2,DETA,D(6),
     .    QN1(3,3,MVSIZ),QN2(3,3,MVSIZ),QN3(3,3,MVSIZ),QN4(3,3,MVSIZ),
     .    XX,YY,ZZ,XY,XZ,YZ,ABC,XXYZ2,YYXZ2,ZZXY2
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C-------transport Mij to element local system first---------
#include "vectorize.inc"
        DO M=JFT,JLT
         I=IPLAT(M)
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
         DR(1,1,M)=  DI(1)
         DR(2,2,M)=  DI(2)
         DR(3,3,M)=  DI(3)
         DR(1,2,M)=  DI(4)
         DR(1,3,M)=  DI(5)
         DR(2,3,M)=  DI(6)
         DR(2,1,M)=  DR(1,2,M)
         DR(3,1,M)=  DR(1,3,M)
         DR(3,2,M)=  DR(2,3,M)
        END DO
C
        CALL SET_RSJ1(R1     ,R2     ,R3     ,R4    ,Z1    ,
     .                JFT    ,JLT    ,CORELV)
       DO J=1,4 
#include "vectorize.inc"
       DO M=JFT,JLT 
        EP=IPLAT(M)
        PP(1,1,J,M)=ONE-VQN(1,J,EP)*VQN(1,J,EP)
        PP(2,2,J,M)=ONE-VQN(2,J,EP)*VQN(2,J,EP)
        PP(1,2,J,M)=-VQN(1,J,EP)*VQN(2,J,EP)
        PP(1,3,J,M)=-VQN(1,J,EP)*VQN(3,J,EP)
        PP(2,3,J,M)=-VQN(2,J,EP)*VQN(3,J,EP)
        PP(2,1,J,M)=PP(1,2,J,M)
        PP(3,1,J,M)=VQN(1,J,EP)
        PP(3,2,J,M)=VQN(2,J,EP)
        PP(3,3,J,M)=VQN(3,J,EP)
       ENDDO
       ENDDO
C
C------------------QJ=PPJ*Q-------------------
       DO I=1,3 
       DO J=1,3 
        DO EP=JFT,JLT 
         QN1(I,J,EP)=PP(I,1,1,EP)*VQ(1,J,EP)+PP(I,2,1,EP)*VQ(2,J,EP)+
     .               PP(I,3,1,EP)*VQ(3,J,EP)
         QN2(I,J,EP)=PP(I,1,2,EP)*VQ(1,J,EP)+PP(I,2,2,EP)*VQ(2,J,EP)+
     .               PP(I,3,2,EP)*VQ(3,J,EP)
         QN3(I,J,EP)=PP(I,1,3,EP)*VQ(1,J,EP)+PP(I,2,3,EP)*VQ(2,J,EP)+
     .               PP(I,3,3,EP)*VQ(3,J,EP)
         QN4(I,J,EP)=PP(I,1,4,EP)*VQ(1,J,EP)+PP(I,2,4,EP)*VQ(2,J,EP)+
     .               PP(I,3,4,EP)*VQ(3,J,EP)
        ENDDO
       ENDDO
       ENDDO
C -----------PROJECTION---------
        CALL SETPROJKZ1(DR    ,R1     ,R2     ,R3     ,R4    ,
     3                K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     4                M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     5                MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33,
     6                MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34,
     7                VQ  ,JFT ,JLT ,QN1 ,QN2 ,QN3 ,QN4  )
C
      RETURN
      END
Chd|====================================================================
Chd|  SETPROJKZ1                    source/elements/shell/coquez/czsumg3.F
Chd|-- called by -----------
Chd|        CZPROJKR                      source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|        TRANKL1                       source/elements/shell/coquez/czsumg3.F
Chd|        TRANKLQN                      source/elements/shell/coquez/czsumg3.F
Chd|        TRANQIKQJ                     source/elements/shell/coquez/czsumg3.F
Chd|        TRANQIKQJ33                   source/elements/shell/coqueba/cbasumg3.F
Chd|====================================================================
      SUBROUTINE SETPROJKZ1(DR    ,R1     ,R2     ,R3     ,R4    ,
     3                     K11,K12,K13,K14,K22,K23,K24,K33,K34,K44,
     4                     M11,M12,M13,M14,M22,M23,M24,M33,M34,M44,
     5                     MF11,MF12,MF13,MF14,MF22,MF23,MF24,MF33, 
     6                     MF34,MF44,FM12,FM13,FM14,FM23,FM24,FM34, 
     .                     VQ  ,JFT  ,JLT  ,QN1 ,QN2 ,QN3 ,QN4 ) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real 
     .    DR(3,3,*),VQ(3,3,*),
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
     .    MF34(3,3,*),MF44(3,3,*),
     .    QN1(3,3,*),QN2(3,3,*),QN3(3,3,*),QN4(3,3,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP,IS,IAS,IT,IAT
      my_real
     .    KL(6,6,MVSIZ),KQ(6,6,MVSIZ),KR(6,6,MVSIZ)
         DATA IS/1/,IAS/0/,IT/1/,IAT/0/
      my_real,
     .  DIMENSION(:,:,:), ALLOCATABLE:: P,KE
C-------------------------------------------------------------
        ALLOCATE(P(24,24,MVSIZ))
        ALLOCATE(KE(24,24,MVSIZ))

C-----------P11=Pr11Q(QN1)
       CALL TRANQIKQJ33(JFT   ,JLT    ,R1  ,DR , R1 ,KL, IS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IS ) 
       CALL TRANKLQN(JFT  ,JLT    ,VQ   ,QN1  ,KL   ,KQ   ,IT  ) 
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
C-----------P22=Pr22Q(QN2)
       CALL TRANQIKQJ33(JFT   ,JLT    ,R2  ,DR , R2 ,KL, IS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IS ) 
       CALL TRANKLQN(JFT  ,JLT    ,VQ   ,QN2  ,KL   ,KQ   ,IT  ) 
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
C-----------P33=Pr33Q(QN3)
       CALL TRANQIKQJ33(JFT   ,JLT    ,R3  ,DR , R3 ,KL, IS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IS ) 
       CALL TRANKLQN(JFT  ,JLT    ,VQ   ,QN3  ,KL   ,KQ   ,IT  ) 
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
C-----------P44=Pr44Q(QN4)
       CALL TRANQIKQJ33(JFT   ,JLT    ,R4  ,DR , R4 ,KL, IS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IS ) 
       CALL TRANKLQN(JFT  ,JLT    ,VQ   ,QN4  ,KL   ,KQ   ,IT  ) 
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
C-----------P12=Pr12Q(QN1)
       CALL TRANQIKQJ33(JFT   ,JLT    ,R1  ,DR , R2 ,KL, IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANKLQN(JFT  ,JLT    ,VQ   ,QN1  ,KL   ,KQ   ,IT  ) 
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
C-----------P21=Pr21Q(QN2)
       CALL TRANKLQN(JFT  ,JLT    ,VQ   ,QN2  ,KL   ,KQ   ,IAT  ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+6,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P13=Pr13Q(QN1)
       CALL TRANQIKQJ33(JFT   ,JLT    ,R1  ,DR , R3 ,KL, IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANKLQN(JFT  ,JLT    ,VQ   ,QN1  ,KL   ,KQ   ,IT  ) 
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
C-----------P31=Pr31Q(QN3)
       CALL TRANKLQN(JFT  ,JLT    ,VQ   ,QN3  ,KL   ,KQ   ,IAT  ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+12,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P14=Pr14Q(QN1)
       CALL TRANQIKQJ33(JFT   ,JLT    ,R1  ,DR , R4 ,KL, IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANKLQN(JFT  ,JLT    ,VQ   ,QN1  ,KL   ,KQ   ,IT  ) 
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
C-----------P41=P41Q(QN4)
       CALL TRANKLQN(JFT  ,JLT    ,VQ   ,QN4  ,KL   ,KQ   ,IAT  ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+18,J,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P23=Pr23Q(QN2)
       CALL TRANQIKQJ33(JFT   ,JLT    ,R2  ,DR , R3 ,KL, IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANKLQN(JFT  ,JLT    ,VQ   ,QN2  ,KL   ,KQ   ,IT  ) 
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
C-----------P32=Pr32Q(QN3)
       CALL TRANKLQN(JFT  ,JLT    ,VQ   ,QN3  ,KL   ,KQ   ,IAT  ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+12,J+6,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P24=Pr24Q(QN2)
       CALL TRANQIKQJ33(JFT   ,JLT    ,R2  ,DR , R4 ,KL, IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANKLQN(JFT  ,JLT    ,VQ   ,QN2  ,KL   ,KQ   ,IT  ) 
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
C-----------P42=Pr42Q(QN4)
       CALL TRANKLQN(JFT  ,JLT    ,VQ   ,QN4  ,KL   ,KQ   ,IAT  ) 
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          P(I+18,J+6,EP)= KQ(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C-----------P34=Pr34Q(QN3)
       CALL TRANQIKQJ33(JFT   ,JLT    ,R3  ,DR , R4 ,KL, IAS) 
       CALL TRANKL1(JFT   ,JLT    ,KL  ,IAS ) 
       CALL TRANKLQN(JFT  ,JLT    ,VQ   ,QN3  ,KL   ,KQ   ,IT  ) 
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
C-----------P43=Pr43Q(QN4)
       CALL TRANKLQN(JFT  ,JLT    ,VQ   ,QN4  ,KL   ,KQ   ,IAT  ) 
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
Chd|  TRANKLQN                      source/elements/shell/coquez/czsumg3.F
Chd|-- called by -----------
Chd|        SETPROJKZ1                    source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TRANKLQN(JFT   ,JLT    ,VQ   ,VQN  ,KL   ,KD   ,IT  ) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,IT
      my_real 
     .   VQ(3,3,*), VQN(3,3,*), KL(6,6,*), KD(6,6,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP,K,L,J1,I1
C--------------KQ=KL*Q-- --Q=|VQ   0 |--IT=0-> KQ=KL^t*Q
C----------------------------|0  VQN|---------------------
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          KD(I,J,EP)= ZERO
         ENDDO
        ENDDO
        ENDDO
C        
       IF (IT==1) THEN
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          DO K=1,3 
            KD(I,J,EP)=KD(I,J,EP)+KL(I,K,EP)*VQ(K,J,EP)
            KD(I+3,J+3,EP)=KD(I+3,J+3,EP)+KL(I+3,K+3,EP)*VQN(K,J,EP)
            KD(I,J+3,EP)=KD(I,J+3,EP)+KL(I,K+3,EP)*VQN(K,J,EP)
            KD(I+3,J,EP)=KD(I+3,J,EP)+KL(I+3,K,EP)*VQ(K,J,EP)
          ENDDO 
         ENDDO
        ENDDO
        ENDDO
       ELSE
        DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT 
          DO K=1,3 
            KD(I,J,EP)=KD(I,J,EP)+KL(K,I,EP)*VQ(K,J,EP)
            KD(I+3,J+3,EP)=KD(I+3,J+3,EP)+KL(K+3,I+3,EP)*VQN(K,J,EP)
            KD(I,J+3,EP)=KD(I,J+3,EP)+KL(K+3,I,EP)*VQN(K,J,EP)
            KD(I+3,J,EP)=KD(I+3,J,EP)+KL(K,I+3,EP)*VQ(K,J,EP)
          ENDDO 
         ENDDO
        ENDDO
        ENDDO
       END IF 
C
      RETURN
      END
Chd|====================================================================
Chd|  TRANKL0                       source/elements/shell/coquez/czsumg3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TRANKL0(JFT   ,JLT    ,KL  ,KR  ,IT) 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,IT
      my_real 
     .   KL(6,6,*),KR(6,6,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,EP,J,K
C--------------update KL(6,j)=0,j=4,6------------------------
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          KR(I,J,EP)= KL(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C
       IF (IT == 1 )THEN
        DO J=1,6
         DO EP=JFT,JLT 
          KR(6,J,EP)= ZERO
         ENDDO
        ENDDO
       ELSE
        DO J=1,6 
         DO EP=JFT,JLT 
           KR(J,6,EP)= ZERO
         ENDDO
        ENDDO
       END IF !(IT == 1 )THEN
      RETURN
      END
Chd|====================================================================
Chd|  TRANQIKQJRZ                   source/elements/shell/coquez/czsumg3.F
Chd|-- called by -----------
Chd|        SETPROJKZ                     source/elements/shell/coquez/czsumg3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TRANQIKQJRZ(JFT   ,JLT    ,RI  ,RD  , RJ  , 
     .                       KL    ,KR     ,IT  ,IS  )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,IT,IS
      my_real
     .   RI(3,3,*), RJ(3,3,*),RD(3,*),KL(6,6,*),KR(6,6,*)
C-----------------------------------------------
C   LOCAL  A R G U M E N T S
C-----------------------------------------------
      INTEGER I,J,EP,L,I1,J1
      my_real
     .    KIJ
C--------------[KR]=|RI| |RD| |RJ|^t---and [KR]=I-[KD]------
C--------------     | I|      |I |--only for KR(6,j),KR(j,6)-for IT=0----
        DO I=1,6 
        DO J=1,6 
         DO EP=JFT,JLT 
          KR(I,J,EP)= KL(I,J,EP)
         ENDDO
        ENDDO
        ENDDO
C
C 
       IF (IT == 1) THEN
        DO J=1,3
         DO EP=JFT,JLT
          KIJ=ZERO
          DO L=1,3
           KIJ=KIJ+RD(L,EP)*RJ(J,L,EP)
          ENDDO
          KR(6,J,EP)= -KIJ
         ENDDO
        ENDDO
C
        DO J=1,3
         DO EP=JFT,JLT
          KR(6,J+3,EP)= -RD(J,EP)
         ENDDO
        ENDDO
C
       ELSE
        DO J=1,3
         DO EP=JFT,JLT
          KIJ=ZERO
          DO L=1,3
           KIJ=KIJ+RI(J,L,EP)*RD(L,EP)
          ENDDO
          KR(J,6,EP)= -KIJ
         ENDDO
        ENDDO
C
        DO J=1,3
         DO EP=JFT,JLT
          KR(J+3,6,EP)= -RD(J,EP)
         ENDDO
        ENDDO
       END IF !(IT == 1) THEN
       
       IF (IS == 1) THEN
         DO EP=JFT,JLT
          KR(6,6,EP)= ONE+KR(6,6,EP)
         ENDDO
       END IF !(IS == 1) THEN
C
      RETURN
      END
 
 
