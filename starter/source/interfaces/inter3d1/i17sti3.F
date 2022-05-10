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
Chd|  I17STI3                       source/interfaces/inter3d1/i17sti3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I17STI3(
     1 IXS       ,PM        ,NME    ,NMES  ,NELM,NELS,
     2 KM        ,KS        )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),NME    ,NMES  ,NELM(NME),NELS(NMES)
C     REAL ou REAL*8
      my_real
     .   PM(NPROPM,*),KM(2,*),KS(2,*)
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IE,MAT
C     REAL ou REAL*8
      my_real
     .   YOUNG,NU,C1,RHO
C-----------------------------------------------
C  Previous version in inint3.F
C-----------------------------------------------
      DO I=1,NME
        IE  = NELM(I)
        MAT = IXS(1,IE)
        RHO   = PM(89,MAT)
        YOUNG = PM(20,MAT)
        C1    = PM(32,MAT)
        IF(YOUNG == ZERO) YOUNG = C1
        NU    = PM(21,MAT)
        KM(1,I) = (UN-NU*NU) / YOUNG
        KM(2,I) = RHO
      ENDDO

      DO I=1,NMES
        IE  = NELS(I)
        MAT = IXS(1,IE)
        RHO   = PM(89,MAT)
        YOUNG = PM(20,MAT)
        C1    = PM(32,MAT)
        IF(YOUNG == ZERO) YOUNG = C1
        NU    = PM(21,MAT)
        KS(1,I) = (UN-NU*NU) / YOUNG
        KS(2,I) = RHO
      ENDDO

      RETURN
      END

