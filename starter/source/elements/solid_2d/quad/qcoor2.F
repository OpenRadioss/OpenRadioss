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
Chd|  QCOOR2                        source/elements/solid_2d/quad/qcoor2.F
Chd|-- called by -----------
Chd|        BINIT2                        source/ale/bimat/binit2.F     
Chd|        INISOLDIST                    source/initial_conditions/inivol/inisoldist.F
Chd|        MULTIFLUID_INIT2              source/multifluid/multifluid_init2.F
Chd|        Q4INIT2                       source/elements/solid_2d/quad4/q4init2.F
Chd|        QINIT2                        source/elements/solid_2d/quad/qinit2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE QCOOR2(X,IXQ,NGL,MXT, 
     .     PID, IX1, IX2, IX3, IX4, 
     .     Y1, Y2, Y3, Y4, 
     .     Z1, Z2, Z3, Z4,
     .     SY, SZ, TY, TZ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXQ(NIXQ,*), NGL(*),MXT(*)
      INTEGER PID(*), IX1(*), IX2(*), IX3(*), IX4(*)
      my_real
     .   X(3,*), Y1(*), Y2(*), Y3(*), Y4(*), Z1(*), Z2(*), Z3(*), Z4(*)
      my_real
     .   SY(MVSIZ),SZ(MVSIZ),
     .   TY(MVSIZ),TZ(MVSIZ)
      my_real
     .   SUMA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C--------------------------------------------------
C     CONNECTIVITES ET MATERIEL
C--------------------------------------------------
      DO I=LFT,LLT
      MXT(I)=IXQ(1,I)
      IX1(I)=IXQ(2,I)
      IX2(I)=IXQ(3,I)
      IX3(I)=IXQ(4,I)
      IX4(I)=IXQ(5,I)
      PID(I)=IXQ(6,I)
        NGL(I)=IXQ(NIXQ,I)
      END DO

C----------------------------
C     COORDONNEES NOEUDS    |
C----------------------------
      DO I=LFT,LLT
      Y1(I)=X(2,IX1(I))
      Z1(I)=X(3,IX1(I))
      Y2(I)=X(2,IX2(I))
      Z2(I)=X(3,IX2(I))
      Y3(I)=X(2,IX3(I))
      Z3(I)=X(3,IX3(I))
      Y4(I)=X(2,IX4(I))
      Z4(I)=X(3,IX4(I))
      END DO
C---------------------------------------
C     REPERE LOCAL (ISOPARAMETRIQUE)
C---------------------------------------
      DO I=LFT,LLT
        SY(I)=HALF*(Y2(I)+Y3(I)-Y1(I)-Y4(I))
        SZ(I)=HALF*(Z2(I)+Z3(I)-Z1(I)-Z4(I))
        TY(I)=HALF*(Y3(I)+Y4(I)-Y1(I)-Y2(I))
        TZ(I)=HALF*(Z3(I)+Z4(I)-Z1(I)-Z2(I))
      END DO
c
      RETURN     
      END
