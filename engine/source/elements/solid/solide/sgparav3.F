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
Chd|  SGPARAV3                      source/elements/solid/solide/sgparav3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SGPARAV3(
     1   NPE,     X,       IXS,     RX,
     2   RY,      RZ,      SX,      SY,
     3   SZ,      TX,      TY,      TZ,
     4   NEL)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER NPE
      INTEGER IXS(NIXS,*)
C     REAL
      my_real
     .  X(3,*),RX(*), RY(*), RZ(*), SX(*), SY(*), SZ(*),
     .  TX(*), TY(*), TZ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NPE1,N
      INTEGER NC(MVSIZ,NPE)
C     REAL
      my_real
     .  X0(MVSIZ,NPE), Y0(MVSIZ,NPE), Z0(MVSIZ,NPE)
C-----------------------------------------------
C
      NPE1=NPE-1
      IF (NPE==4.OR.NPE==10) THEN
       DO I=1,NEL
        NC(I,1)=IXS(2,I)
        NC(I,2)=IXS(4,I)
        NC(I,3)=IXS(7,I)
        NC(I,4)=IXS(6,I)
       ENDDO
      ELSEIF (NPE==6) THEN
       DO I=1,NEL
        NC(I,1)=IXS(2,I)
        NC(I,2)=IXS(3,I)
        NC(I,3)=IXS(4,I)
        NC(I,4)=IXS(6,I)
        NC(I,5)=IXS(7,I)
        NC(I,6)=IXS(8,I)
       ENDDO
      ELSE
       DO N=1,NPE
        DO I=1,NEL
         NC(I,N)=IXS(N+1,I)
        ENDDO
       ENDDO
      ENDIF 
C----------------------------
C     NODAL COORDINATES     /
C----------------------------
      DO N=1,NPE
       DO I=1,NEL
        X0(I,N)=X(1,NC(I,N))
        Y0(I,N)=X(2,NC(I,N))
        Z0(I,N)=X(3,NC(I,N))
       ENDDO
      ENDDO 
C----------------------------
C     R,S,T     |
C----------------------------
      SELECT CASE (NPE)
      CASE(4,10)                                             
       DO I=1,NEL
        RX(I) =  X0(I,1) - X0(I,4)
        RY(I) =  Y0(I,1) - Y0(I,4)
        RZ(I) =  Z0(I,1) - Z0(I,4)
C
        SX(I) =  X0(I,2) - X0(I,4)
        SY(I) =  Y0(I,2) - Y0(I,4)
        SZ(I) =  Z0(I,2) - Z0(I,4)
C
        TX(I) =  X0(I,3) - X0(I,4)
        TY(I) =  Y0(I,3) - Y0(I,4)
        TZ(I) =  Z0(I,3) - Z0(I,4)
       ENDDO
      CASE(6)                                          
       DO I=1,NEL
        RX(I) =  X0(I,3) + X0(I,6) - X0(I,1)- X0(I,4)
        RY(I) =  Y0(I,3) + Y0(I,6) - Y0(I,1)- Y0(I,4)
        RZ(I) =  Z0(I,3) + Z0(I,6) - Z0(I,1) - Z0(I,4)
C
        SX(I) = (X0(I,4)+X0(I,5)+X0(I,6)-X0(I,1)+X0(I,2)+X0(I,3))*THIRD
        SY(I) = (Y0(I,4)+Y0(I,5)+Y0(I,6)-Y0(I,1)+Y0(I,2)+Y0(I,3))*THIRD
        SZ(I) = (Z0(I,4)+Z0(I,5)+Z0(I,6)-Z0(I,1)+Z0(I,2)+Z0(I,3))*THIRD
C
        TX(I) = X0(I,2) + X0(I,5)- X0(I,1) - X0(I,4)
        TY(I) = Y0(I,2) + Y0(I,5)- Y0(I,1) - Y0(I,4)
        TZ(I) = Z0(I,2) + Z0(I,5)- Z0(I,1) - Z0(I,4)
       ENDDO
      CASE(8)                                          
       DO I=1,NEL
        RX(I) = X0(I,3)+X0(I,4)+X0(I,7)+X0(I,8)-
     .         (X0(I,1)+X0(I,2)+X0(I,5)+X0(I,6)) 
        RY(I) = Y0(I,3)+Y0(I,4)+Y0(I,7)+Y0(I,8)-
     .         (Y0(I,1)+Y0(I,2)+Y0(I,5)+Y0(I,6))
        RZ(I) = Z0(I,3)+Z0(I,4)+Z0(I,7)+Z0(I,8)-
     .         (Z0(I,1)+Z0(I,2)+Z0(I,5)+Z0(I,6))
C
        SX(I) = X0(I,5)+X0(I,6)+X0(I,7)+X0(I,8)-
     .         (X0(I,1)+X0(I,2)+X0(I,3)+X0(I,4)) 
        SY(I) = Y0(I,5)+Y0(I,6)+Y0(I,7)+Y0(I,8)-
     .         (Y0(I,1)+Y0(I,2)+Y0(I,3)+Y0(I,4))
        SZ(I) = Z0(I,5)+Z0(I,6)+Z0(I,7)+Z0(I,8)-
     .         (Z0(I,1)+Z0(I,2)+Z0(I,3)+Z0(I,4))
C
        TX(I) = X0(I,3)+X0(I,2)+X0(I,7)+X0(I,6)-
     .         (X0(I,1)+X0(I,4)+X0(I,5)+X0(I,8)) 
        TY(I) = Y0(I,3)+Y0(I,2)+Y0(I,7)+Y0(I,6)-
     .         (Y0(I,1)+Y0(I,4)+Y0(I,5)+Y0(I,8))
        TZ(I) = Z0(I,3)+Z0(I,2)+Z0(I,7)+Z0(I,6)-
     .         (Z0(I,1)+Z0(I,4)+Z0(I,5)+Z0(I,8))
C
       ENDDO
      END SELECT 
C
      RETURN
      END
