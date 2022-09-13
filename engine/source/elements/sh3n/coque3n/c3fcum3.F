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
Chd|  C3FCUM3                       source/elements/sh3n/coque3n/c3fcum3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C3FCUM3(JFT,JLT,F,
     .             F11,F12,F13,F21,F22,
     .             F23,F31,F32,F33,FZERO,
     .             E1X      ,E1Y      ,E1Z     ,E2X      ,                 
     .             E2Y      ,E2Z      ,E3X     ,E3Y      ,E3Z      )       
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
C     REAL
      my_real F(3,*),FZERO(3,4,*),
     .     E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .     E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .     E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),
     .     F11(MVSIZ), F12(MVSIZ), F13(MVSIZ),
     .     F21(MVSIZ), F22(MVSIZ), F23(MVSIZ),
     .     F31(MVSIZ), F32(MVSIZ), F33(MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
      my_real G11, G12, G13, G21, G22, G23, G31, G32, G33,
     .     TEMP1,TEMP2,TEMP3
C-----------------------------------------------
C               POUR ISIGI = 5 OU -5
C     CHANGEMENT DE REPERE DE FZERO POUR TT=DT1
C          REPERE GLOBAL ---> REPERE LOCAL
C-----------------------------------------------
      IF(IABS(ISIGI)==5.AND.NCYCLE==1.AND.IRUN==1) THEN
        DO I=JFT,JLT
          DO J=1,3
            TEMP1=FZERO(1,J,I)
            TEMP2=FZERO(2,J,I)
            TEMP3=FZERO(3,J,I)
            FZERO(1,J,I)=TEMP1*E1X(I)+TEMP2*E1Y(I)+TEMP3*E1Z(I)
            FZERO(2,J,I)=TEMP1*E2X(I)+TEMP2*E2Y(I)+TEMP3*E2Z(I)
            FZERO(3,J,I)=TEMP1*E3X(I)+TEMP2*E3Y(I)+TEMP3*E3Z(I)
          ENDDO
        ENDDO
      ENDIF
C-----------------------------------------------
C               POUR ISIGI = 5 OU -5
C     PRISE EN COMPTE DES FORCES D EQUILIBRAGE
C      DANS L EVALUATION DES EFFORTS INTERNES
C-----------------------------------------------
      IF((IABS(ISIGI)==5.AND.NCYCLE>=1.AND.IRUN==1).OR.
     .   (IABS(ISIGI)==5.AND.IRUN>1)) THEN
        DO I=JFT,JLT
          G11 = F11(I)+FZERO(1,1,I)
          G21 = F21(I)+FZERO(2,1,I)
          G31 = F31(I)+FZERO(3,1,I)
          F11(I) = E1X(I)*G11 + E2X(I)*G21 + E3X(I)*G31
          F21(I) = E1Y(I)*G11 + E2Y(I)*G21 + E3Y(I)*G31
          F31(I) = E1Z(I)*G11 + E2Z(I)*G21 + E3Z(I)*G31
          G12 = F12(I)+FZERO(1,2,I)
          G22 = F22(I)+FZERO(2,2,I)
          G32 = F32(I)+FZERO(3,2,I)
          F12(I) = E1X(I)*G12 + E2X(I)*G22 + E3X(I)*G32
          F22(I) = E1Y(I)*G12 + E2Y(I)*G22 + E3Y(I)*G32
          F32(I) = E1Z(I)*G12 + E2Z(I)*G22 + E3Z(I)*G32
          G13 = F13(I)+FZERO(1,3,I)
          G23 = F23(I)+FZERO(2,3,I)
          G33 = F33(I)+FZERO(3,3,I)
          F13(I) = E1X(I)*G13 + E2X(I)*G23 + E3X(I)*G33
          F23(I) = E1Y(I)*G13 + E2Y(I)*G23 + E3Y(I)*G33
          F33(I) = E1Z(I)*G13 + E2Z(I)*G23 + E3Z(I)*G33
        ENDDO
      ENDIF
      IF(IABS(ISIGI)/=5.OR.TT==0) THEN
        DO I=JFT,JLT
    	  G11 = F11(I)
    	  G21 = F21(I)
    	  G31 = F31(I)
    	  F11(I) = E1X(I)*G11 + E2X(I)*G21 + E3X(I)*G31
    	  F21(I) = E1Y(I)*G11 + E2Y(I)*G21 + E3Y(I)*G31
    	  F31(I) = E1Z(I)*G11 + E2Z(I)*G21 + E3Z(I)*G31
    	  G12 = F12(I)
    	  G22 = F22(I)
    	  G32 = F32(I)
    	  F12(I) = E1X(I)*G12 + E2X(I)*G22 + E3X(I)*G32
    	  F22(I) = E1Y(I)*G12 + E2Y(I)*G22 + E3Y(I)*G32
    	  F32(I) = E1Z(I)*G12 + E2Z(I)*G22 + E3Z(I)*G32
    	  G13 = F13(I)
    	  G23 = F23(I)
    	  G33 = F33(I)
    	  F13(I) = E1X(I)*G13 + E2X(I)*G23 + E3X(I)*G33
    	  F23(I) = E1Y(I)*G13 + E2Y(I)*G23 + E3Y(I)*G33
    	  F33(I) = E1Z(I)*G13 + E2Z(I)*G23 + E3Z(I)*G33
        ENDDO
      ENDIF
C
      RETURN
      END
