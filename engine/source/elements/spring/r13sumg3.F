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
Chd|  R13SUMG3                      source/elements/spring/r13sumg3.F
Chd|-- called by -----------
Chd|        R13KE3                        source/elements/spring/r13ke3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE R13SUMG3(JFT    ,JLT    ,AL      ,KX      ,KY     ,
     2                    KZ     ,MX     ,MY      ,MZ      ,R11    ,
     3                    R12    ,R13    ,R21     ,R22     ,R23    ,
     4                    R31    ,R32    ,R33     ,KE11    ,KE12   ,
     5                    KE22   )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real 
     .    AL(*),KX(*),KY(*),KZ(*),
     .    MX(*),MY(*),MZ(*),
     .    R11(*),R12(*),R13(*),
     .    R21(*),R22(*),R23(*),
     .    R31(*),R32(*),R33(*)
       my_real
     .    KE11(6,6,*),KE22(6,6,*),KE12(6,6,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I, J, EP,IS,IAS,MI,MJ
      my_real 
     .     Q(3,3,MVSIZ),K11(3,MVSIZ),M11(3,MVSIZ),MF32(MVSIZ),
     .     M12(3,MVSIZ),MF23(MVSIZ),ALDEMI,Q1,Q2,Q3,MF11(3,3,MVSIZ)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
       DO I=JFT,JLT
        K11(1,I)=KX(I)
        K11(2,I)=KY(I)
        K11(3,I)=KZ(I)
        ALDEMI = UNDEMI*AL(I)
        MF23(I)= ALDEMI*KY(I)
        MF32(I)= ALDEMI*KZ(I)
        M11(1,I)=MX(I)
        M11(2,I)=MY(I)+ALDEMI*MF23(I)
        M11(3,I)=MZ(I)+ALDEMI*MF32(I)
        M12(1,I)=-M11(1,I)
        M12(2,I)=AL(I)*MF32(I)-M11(2,I)
        M12(3,I)=AL(I)*MF23(I)-M11(3,I)
        MF32(I)=-MF32(I)
       ENDDO 
C---------------------------------------
C   TRANS LOCAL-->GLOBAL 
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
C---------------------------------------
C   ASSEMBLAGE
C---------------------------------------
C---------KII ----Keij=QkiQkj*Kkk---M12 est diag- 
       DO I=1,3 
        MI=I+3
        DO J=I,3 
         MJ=J+3
         DO EP=JFT,JLT
          Q1 =Q(1,I,EP)*Q(1,J,EP)
          Q2 =Q(2,I,EP)*Q(2,J,EP)
          Q3 =Q(3,I,EP)*Q(3,J,EP)
          KE11(I,J,EP)=Q1*K11(1,EP)+Q2*K11(2,EP)+Q3*K11(3,EP)
          KE11(MI,MJ,EP)=Q1*M11(1,EP)+Q2*M11(2,EP)+Q3*M11(3,EP)
          KE12(MI,MJ,EP)=Q1*M12(1,EP)+Q2*M12(2,EP)+Q3*M12(3,EP)
          KE22(I,J,EP)=KE11(I,J,EP)
          KE22(MI,MJ,EP)=KE11(MI,MJ,EP)
         ENDDO
        ENDDO
       ENDDO
C---------K23>0,K32>0-------------------------
       DO I=1,3 
        DO J=1,3 
         DO EP=JFT,JLT
          MF11(I,J,EP)=Q(2,I,EP)*MF23(EP)*Q(3,J,EP)+
     .                 Q(3,I,EP)*MF32(EP)*Q(2,J,EP)
         ENDDO
        ENDDO
       ENDDO
C
       DO I=1,3 
        DO J=1,3 
         MJ=J+3
         DO EP=JFT,JLT
          KE11(I,MJ,EP)=MF11(I,J,EP)
          KE22(I,MJ,EP)=-MF11(I,J,EP)
         ENDDO
        ENDDO
       ENDDO
C
       DO I=1,6 
        DO J=I,6 
         DO EP=JFT,JLT
          KE11(J,I,EP)=KE11(I,J,EP)
          KE22(J,I,EP)=KE22(I,J,EP)
         ENDDO
        ENDDO
       ENDDO
C---------KIJ -------- 
       DO I=1,3 
        MI=I+3
        DO J=1,3 
         MJ=J+3
         DO EP=JFT,JLT
          KE12(I,J,EP)=-KE11(I,J,EP)
          KE12(I,MJ,EP)=MF11(I,J,EP)
          KE12(MI,J,EP)=-MF11(J,I,EP)
         ENDDO
        ENDDO
       ENDDO
       DO EP=JFT,JLT
          KE12(5,4,EP)=KE12(4,5,EP)
          KE12(6,4,EP)=KE12(4,6,EP)
          KE12(6,5,EP)=KE12(5,6,EP)
       ENDDO
C
      RETURN
      END
 
