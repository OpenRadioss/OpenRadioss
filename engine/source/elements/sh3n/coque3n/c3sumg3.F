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
Chd|  C3SUMG3                       source/elements/sh3n/coque3n/c3sumg3.F
Chd|-- called by -----------
Chd|        C3KE3                         source/elements/sh3n/coque3n/c3ke3.F
Chd|-- calls ---------------
Chd|        CBATRAN2                      source/elements/shell/coqueba/cbasumg3.F
Chd|        CBATRAN223                    source/elements/shell/coqueba/cbasumg3.F
Chd|        CBATRAN232                    source/elements/shell/coqueba/cbasumg3.F
Chd|        CBATRAN233                    source/elements/shell/coqueba/cbasumg3.F
Chd|        CBATRAN3                      source/elements/shell/coqueba/cbasumg3.F
Chd|====================================================================
      SUBROUTINE C3SUMG3(JFT ,JLT ,
     1                     R11,R12,R13,R21,R22,R23,R31,R32,R33,
     2                     K11,K12,K13,K22,K23,K33,
     3                     M11,M12,M13,M22,M23,M33,
     4                     MF11,MF12,MF13,MF22,MF23,MF33, 
     5                     FM12,FM13,FM23, 
     6                     KE11,KE22,KE33,KE12,KE13,KE23,
     7                     IDRIL,IORTH)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT,IDRIL,IORTH
      my_real 
     .    R11(*),R12(*),R13(*),
     .    R21(*),R22(*),R23(*),
     .    R31(*),R32(*),R33(*)
       my_real
     .    K11(3,3,*),K12(3,3,*),K13(3,3,*),
     .    K22(3,3,*),K23(3,3,*),K33(3,3,*),
     .    M11(3,3,*),M12(3,3,*),M13(3,3,*),
     .    M22(3,3,*),M23(3,3,*),M33(3,3,*),
     .    MF11(3,3,*),MF12(3,3,*),MF13(3,3,*),
     .    MF22(3,3,*),MF23(3,3,*),MF33(3,3,*),
     .    FM12(3,3,*),FM13(3,3,*),FM23(3,3,*),
     .    KE11(6,6,*),KE22(6,6,*),KE33(6,6,*),
     .    KE12(6,6,*),KE13(6,6,*),KE23(6,6,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I, J, EP,IS,IAS,MI,MJ
      my_real 
     .     Q(3,3,MVSIZ)
      DATA IS/1/,IAS/0/
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C---------------------------------------
C   TRANS LOCAL-->GLOBAL ET 5DDL-->6DDL
C---------------------------------------
       DO I=JFT,JLT
        Q(1,1,I)=R11(I)
        Q(1,2,I)=R21(I)
        Q(1,3,I)=R31(I)
        Q(2,1,I)=R12(I)
        Q(2,2,I)=R22(I)
        Q(2,3,I)=R32(I)
        Q(3,1,I)=R13(I)
        Q(3,2,I)=R23(I)
        Q(3,3,I)=R33(I)
       ENDDO 
C-----      CBATRAN233     QI^tKQJ K(1,3)>0,K(2,3)>0,K(3,1)>0,K(3,2)>0-------
       CALL CBATRAN2(JFT,JLT,Q,K11,Q,IS)
       CALL CBATRAN2(JFT,JLT,Q,K22,Q,IS)
       CALL CBATRAN2(JFT,JLT,Q,K33,Q,IS)
       CALL CBATRAN2(JFT,JLT,Q,K12,Q,IAS)
       CALL CBATRAN2(JFT,JLT,Q,K13,Q,IAS)
       CALL CBATRAN2(JFT,JLT,Q,K23,Q,IAS)
       IF (IORTH >0 .AND.IDRIL>0) THEN
        CALL CBATRAN3(JFT,JLT,Q,M11,Q,IS)
        CALL CBATRAN3(JFT,JLT,Q,M22,Q,IS)
        CALL CBATRAN3(JFT,JLT,Q,M33,Q,IS)
        CALL CBATRAN3(JFT,JLT,Q,M12,Q,IAS)
        CALL CBATRAN3(JFT,JLT,Q,M13,Q,IAS)
        CALL CBATRAN3(JFT,JLT,Q,M23,Q,IAS)
       ELSE
        CALL CBATRAN2(JFT,JLT,Q,M11,Q,IS)
        CALL CBATRAN2(JFT,JLT,Q,M22,Q,IS)
        CALL CBATRAN2(JFT,JLT,Q,M33,Q,IS)
        CALL CBATRAN2(JFT,JLT,Q,M12,Q,IAS)
        CALL CBATRAN2(JFT,JLT,Q,M13,Q,IAS)
        CALL CBATRAN2(JFT,JLT,Q,M23,Q,IAS)
       END IF !(IORTH >0.AND.IDRIL>0)
       IF (IORTH >0) THEN
       CALL CBATRAN3(JFT,JLT,Q,MF11,Q,IAS)
       CALL CBATRAN3(JFT,JLT,Q,MF12,Q,IAS)
       CALL CBATRAN3(JFT,JLT,Q,MF13,Q,IAS)
       CALL CBATRAN3(JFT,JLT,Q,MF22,Q,IAS)
       CALL CBATRAN3(JFT,JLT,Q,MF23,Q,IAS)
       CALL CBATRAN3(JFT,JLT,Q,MF33,Q,IAS)
       CALL CBATRAN3(JFT,JLT,Q,FM12,Q,IAS)
       CALL CBATRAN3(JFT,JLT,Q,FM13,Q,IAS)
       CALL CBATRAN3(JFT,JLT,Q,FM23,Q,IAS)
       ELSEIF (IDRIL>0) THEN
        CALL CBATRAN233(JFT,JLT,Q,MF11,Q)
        CALL CBATRAN233(JFT,JLT,Q,MF12,Q)
        CALL CBATRAN233(JFT,JLT,Q,MF13,Q)
        CALL CBATRAN233(JFT,JLT,Q,MF22,Q)
        CALL CBATRAN233(JFT,JLT,Q,MF23,Q)
        CALL CBATRAN233(JFT,JLT,Q,MF33,Q)
        CALL CBATRAN233(JFT,JLT,Q,FM12,Q)
        CALL CBATRAN233(JFT,JLT,Q,FM13,Q)
        CALL CBATRAN233(JFT,JLT,Q,FM23,Q)
       ELSE
        CALL CBATRAN232(JFT,JLT,Q,MF11,Q)
        CALL CBATRAN232(JFT,JLT,Q,MF12,Q)
        CALL CBATRAN232(JFT,JLT,Q,MF13,Q)
        CALL CBATRAN232(JFT,JLT,Q,MF22,Q)
        CALL CBATRAN232(JFT,JLT,Q,MF23,Q)
        CALL CBATRAN232(JFT,JLT,Q,MF33,Q)
        CALL CBATRAN223(JFT,JLT,Q,FM12,Q)
        CALL CBATRAN223(JFT,JLT,Q,FM13,Q)
        CALL CBATRAN223(JFT,JLT,Q,FM23,Q)
       END IF
C---------------------------------------
C   ASSEMBLAGE
C---------------------------------------
C---------KII -------- 
       DO I=1,3 
        MI=I+3
        DO J=I,3 
         MJ=J+3
         DO EP=JFT,JLT
          KE11(I,J,EP)=K11(I,J,EP)
          KE11(MI,MJ,EP)=M11(I,J,EP)
          KE22(I,J,EP)=K22(I,J,EP)
          KE22(MI,MJ,EP)=M22(I,J,EP)
          KE33(I,J,EP)=K33(I,J,EP)
          KE33(MI,MJ,EP)=M33(I,J,EP)
         ENDDO
        ENDDO
       ENDDO
C
       DO I=1,3 
        DO J=1,3 
         MJ=J+3
         DO EP=JFT,JLT
          KE11(I,MJ,EP)=MF11(I,J,EP)
          KE22(I,MJ,EP)=MF22(I,J,EP)
          KE33(I,MJ,EP)=MF33(I,J,EP)
         ENDDO
        ENDDO
       ENDDO
C
       DO I=1,6 
        DO J=I,6 
         DO EP=JFT,JLT
          KE11(J,I,EP)=KE11(I,J,EP)
          KE22(J,I,EP)=KE22(I,J,EP)
          KE33(J,I,EP)=KE33(I,J,EP)
         ENDDO
        ENDDO
       ENDDO
C---------KIJ -------- 
       DO I=1,3 
        MI=I+3
        DO J=1,3 
         MJ=J+3
         DO EP=JFT,JLT
          KE12(I,J,EP)=K12(I,J,EP)
          KE12(I,MJ,EP)=MF12(I,J,EP)
          KE12(MI,J,EP)=FM12(I,J,EP)
          KE12(MI,MJ,EP)=M12(I,J,EP)
          KE13(I,J,EP)=K13(I,J,EP)
          KE13(I,MJ,EP)=MF13(I,J,EP)
          KE13(MI,J,EP)=FM13(I,J,EP)
          KE13(MI,MJ,EP)=M13(I,J,EP)
          KE23(I,J,EP)=K23(I,J,EP)
          KE23(I,MJ,EP)=MF23(I,J,EP)
          KE23(MI,J,EP)=FM23(I,J,EP)
          KE23(MI,MJ,EP)=M23(I,J,EP)
         ENDDO
        ENDDO
       ENDDO
C
      RETURN
      END
 
