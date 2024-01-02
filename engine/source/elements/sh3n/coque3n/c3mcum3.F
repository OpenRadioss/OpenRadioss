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
Chd|  C3MCUM3                       source/elements/sh3n/coque3n/c3mcum3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C3MCUM3(JFT,JLT,M,
     .           M11,M12,M13,M21,M22,M23,M31,M32,M33,
     .           E1X      ,E1Y      ,E1Z     ,E2X      ,                 
     .           E2Y      ,E2Z      ,E3X     ,E3Y      ,E3Z      )       
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
      my_real M(3,*), 
     .     M11(MVSIZ), M12(MVSIZ), M13(MVSIZ),
     .     M21(MVSIZ), M22(MVSIZ), M23(MVSIZ),
     .     M31(MVSIZ), M32(MVSIZ), M33(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real G11T, G21T, G12T, G22T, G13T, G23T
      my_real E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .        E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .        E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ)
C=======================================================================
      DO I=JFT,JLT
        G11T  =M11(I)
        G21T  =M21(I)
        M11(I)=E1X(I)*G11T+E2X(I)*G21T
        M21(I)=E1Y(I)*G11T+E2Y(I)*G21T
        M31(I)=E1Z(I)*G11T+E2Z(I)*G21T
        G12T  =M12(I)
        G22T  =M22(I)
        M12(I)=E1X(I)*G12T+E2X(I)*G22T
        M22(I)=E1Y(I)*G12T+E2Y(I)*G22T
        M32(I)=E1Z(I)*G12T+E2Z(I)*G22T
        G13T  =M13(I)
        G23T  =M23(I)
        M13(I)=E1X(I)*G13T+E2X(I)*G23T
        M23(I)=E1Y(I)*G13T+E2Y(I)*G23T
        M33(I)=E1Z(I)*G13T+E2Z(I)*G23T
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  C3MZCUM3                      source/elements/sh3n/coque3n/c3mcum3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C3MZCUM3(
     1            JFT    ,JLT    ,MLZ    ,E3X    ,E3Y    ,
     2            E3Z    ,M11    ,M12    ,M13    ,M21    ,
     5            M22    ,M23    ,M31    ,M32    ,M33    )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
C        TRANSMET LES FORCES INTERNES LOCALES VF,VM ---> GLOBALES FIJ ,MIJ
C        ENTREES : 
C        SORTIES : FIJ,MIJ
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real 
     .   E3X(*),E3Y(*),E3Z(*),MLZ(MVSIZ,3),
     .   M11(*), M12(*), M13(*), 
     .   M21(*), M22(*), M23(*), 
     .   M31(*), M32(*), M33(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER  K
C=======================================================================
      DO K=JFT,JLT
         M11(K)= M11(K)+ E3X(K)*MLZ(K,1)
         M21(K)= M21(K)+ E3Y(K)*MLZ(K,1)
         M31(K)= M31(K)+ E3Z(K)*MLZ(K,1)
C
         M12(K)= M12(K)+ E3X(K)*MLZ(K,2)
         M22(K)= M22(K)+ E3Y(K)*MLZ(K,2)
         M32(K)= M32(K)+ E3Z(K)*MLZ(K,2)
C
         M13(K)= M13(K)+ E3X(K)*MLZ(K,3)
         M23(K)= M23(K)+ E3Y(K)*MLZ(K,3)
         M33(K)= M33(K)+ E3Z(K)*MLZ(K,3)
      ENDDO
c-----------
      RETURN
      END
