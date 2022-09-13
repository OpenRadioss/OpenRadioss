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
Chd|  CSUBF3                        source/elements/shell/coque/csubf3.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CSUBF3(JFT,JLT,NDT,AREAS,AREA ,
     4            F11    ,F12  ,F13 ,F14 ,F21  ,
     5            F22    ,F23  ,F24 ,F31 ,F32  ,
     6            F33    ,F34  ,M11 ,M12 ,M13  ,
     7            M14    ,M21  ,M22 ,M23 ,M24  ,
     8            M31    ,M32  ,M33 ,M34 )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDT(*),
     . JFT, JLT
C     REAL
      my_real
     .   F11(*), F12(*), F13(*), F14(*),
     .   F21(*), F22(*), F23(*), F24(*),
     .   F31(*), F32(*), F33(*), F34(*),
     .   M11(*), M12(*), M13(*), M14(*),
     .   M21(*), M22(*), M23(*), M24(*),
     .   M31(*), M32(*), M33(*), M34(*),
     .   AREAS(*),AREA(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
C
      IF(ISUB==1)THEN
        IF(NDT(1)==1)THEN
          DO I=JFT,JLT
          AREAS(I) = AREA(I)
          ENDDO
        ELSE
          DO I=JFT,JLT
            F11(I) = F11(I) * NDT(1)
            F12(I) = F12(I) * NDT(1)
            F13(I) = F13(I) * NDT(1)
            F14(I) = F14(I) * NDT(1)
            F21(I) = F21(I) * NDT(1)
            F22(I) = F22(I) * NDT(1)
            F23(I) = F23(I) * NDT(1)
            F24(I) = F24(I) * NDT(1)
            F31(I) = F31(I) * NDT(1)
            F32(I) = F32(I) * NDT(1)
            F33(I) = F33(I) * NDT(1)
            F34(I) = F34(I) * NDT(1)
            M11(I) = M11(I) * NDT(1)
            M12(I) = M12(I) * NDT(1)
            M13(I) = M13(I) * NDT(1)
            M14(I) = M14(I) * NDT(1)
            M21(I) = M21(I) * NDT(1)
            M22(I) = M22(I) * NDT(1)
            M23(I) = M23(I) * NDT(1)
            M24(I) = M24(I) * NDT(1)
            M31(I) = M31(I) * NDT(1)
            M32(I) = M32(I) * NDT(1)
            M33(I) = M33(I) * NDT(1)
            M34(I) = M34(I) * NDT(1)
            AREAS(I) = AREA(I)
          ENDDO
        ENDIF
        RETURN
      ENDIF
C
      DO I=JFT,JLT
        F11(I) = F11(I) * NDT(I)
        F12(I) = F12(I) * NDT(I)
        F13(I) = F13(I) * NDT(I)
        F14(I) = F14(I) * NDT(I)
        F21(I) = F21(I) * NDT(I)
        F22(I) = F22(I) * NDT(I)
        F23(I) = F23(I) * NDT(I)
        F24(I) = F24(I) * NDT(I)
        F31(I) = F31(I) * NDT(I)
        F32(I) = F32(I) * NDT(I)
        F33(I) = F33(I) * NDT(I)
        F34(I) = F34(I) * NDT(I)
        M11(I) = M11(I) * NDT(I)
        M12(I) = M12(I) * NDT(I)
        M13(I) = M13(I) * NDT(I)
        M14(I) = M14(I) * NDT(I)
        M21(I) = M21(I) * NDT(I)
        M22(I) = M22(I) * NDT(I)
        M23(I) = M23(I) * NDT(I)
        M24(I) = M24(I) * NDT(I)
        M31(I) = M31(I) * NDT(I)
        M32(I) = M32(I) * NDT(I)
        M33(I) = M33(I) * NDT(I)
        M34(I) = M34(I) * NDT(I)
        AREAS(I) = AREA(I)
      ENDDO
C
      RETURN
      END
