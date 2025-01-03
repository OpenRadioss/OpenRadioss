Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    i22tzinf      ../starter/source/interfaces/inter3d1/i22tzinf.F
      !||--- called by ------------------------------------------------------
      !||    inint3        ../starter/source/interfaces/inter3d1/inint3.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod   ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE I22TZINF(
     1   X        ,TZINF    ,BUFBRIC  ,NBRIC    ,IXS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
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
      INTEGER  IXS(NIXS,*)
      INTEGER MAXSIZ
      my_real X(3,*),TZINF,XX(8),YY(8), ZZ(8), CoorMIN(3), CoorMAX(3), DDmax
      INTEGER BUFBRIC(*), NBRIC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, L, N1, N2, N3, N4, I_AMAX
      INTEGER I_ADD, ADESTK, NB_NC, NB_EC, ADNSTK, IBID
      INTEGER IP1, IP2, IP21, IP22, IP31,J_STOK,I_BID,NB_N_B,NE
      my_real
     .   DX1,DY1,DZ1,
     .   DX3,DY3,DZ3,
     .   DX4,DY4,DZ4,
     .   DX6,DY6,DZ6,
     .   DD1,DD2,DD3,DD4,DD,DD0,XMIN,YMIN,ZMIN,
     .   XMAX,YMAX,ZMAX,TZINF0,GAPSMAX,
     .   BID,TZINF_ST,MARGE,MARGE_ST,GAPV(MVSIZ),
     .   DIAG(4)
C-----------------------------------------------
C   P r e c o n d i t i o n s
C-----------------------------------------------
      IF(NBRIC==0)RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      !1- INFLUCENCE AREA DIAMETER
      !DD DIMENSION OF CIRCUMSCRIBED BALL (NORM L1)

      L = 1
      NE = BUFBRIC(L)
      XX(1:8)    = X(1, IXS(2:9,NE) )
      YY(1:8)    = X(2, IXS(2:9,NE) )
      ZZ(1:8)    = X(3, IXS(2:9,NE) ) 

      DIAG(1)    = SQRT((XX(1)-XX(7))**2 + (YY(1)-YY(7))**2 + (ZZ(1)-ZZ(7))**2) 
      DIAG(2)    = SQRT((XX(3)-XX(5))**2 + (YY(3)-YY(5))**2 + (ZZ(3)-ZZ(5))**2)
      DIAG(3)    = SQRT((XX(2)-XX(8))**2 + (YY(2)-YY(8))**2 + (ZZ(2)-ZZ(8))**2)
      DIAG(4)    = SQRT((XX(4)-XX(6))**2 + (YY(4)-YY(6))**2 + (ZZ(4)-ZZ(6))**2)
      
      DDmax      = MAXVAL(DIAG(1:4))

      DO L=2,NBRIC
        NE = BUFBRIC(L)
        XX(1:8)    = X(1, IXS(2:9,NE) )
        YY(1:8)    = X(2, IXS(2:9,NE) )
        ZZ(1:8)    = X(3, IXS(2:9,NE) ) 
        DIAG(1)    = SQRT((XX(1)-XX(7))**2 + (YY(1)-YY(7))**2 + (ZZ(1)-ZZ(7))**2) 
        DIAG(2)    = SQRT((XX(3)-XX(5))**2 + (YY(3)-YY(5))**2 + (ZZ(3)-ZZ(5))**2)
        DIAG(3)    = SQRT((XX(2)-XX(8))**2 + (YY(2)-YY(8))**2 + (ZZ(2)-ZZ(8))**2)
        DIAG(4)    = SQRT((XX(4)-XX(6))**2 + (YY(4)-YY(6))**2 + (ZZ(4)-ZZ(6))**2)      
        DD         = MAXVAL(DIAG(1:4))      
        IF(DD>DDmax)THEN
          DDmax = DD
        ENDIF
      ENDDO
      
      TZINF = (ONE+FIVE/HUNDRED) * DDmax
           
      RETURN
      END
