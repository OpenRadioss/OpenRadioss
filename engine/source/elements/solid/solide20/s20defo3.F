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
Chd|  S20DEFO3                      source/elements/solid/solide20/s20defo3.F
Chd|-- called by -----------
Chd|        S16FORC3                      source/elements/thickshell/solide16/s16forc3.F
Chd|        S20FORC3                      source/elements/solid/solide20/s20forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S20DEFO3(
     .   NPE, PX, PY, PZ, VX, VY, VZ,
     .   DXX, DXY, DXZ, DYX, DYY, DYZ, DZX, DZY,
     .   DZZ, D4, D5, D6, WXX, WYY, WZZ,RHO,RHOO,VOLNP,VOLN)
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
      INTEGER NPE
      my_real
     .  VX(MVSIZ,*),VY(MVSIZ,*),VZ(MVSIZ,*),
     .  PX(MVSIZ,*),PY(MVSIZ,*),PZ(MVSIZ,*),
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*), D4(*), D5(*), D6(*),
     .   WXX(*), WYY(*), WZZ(*),RHO(*),
     .  RHOO(*),VOLNP(*),VOLN(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,N
C     REAL
      my_real
     .   DT1D2
C-----------------------------------------------
C                                                                     12
      DO I=LFT,LLT
        RHOO(I)=RHO(I)
        VOLN(I)=VOLNP(I)
      ENDDO
      N = 1
      DO I=LFT,LLT
        DXX(I)=PX(I,N)*VX(I,N)
        DYY(I)=PY(I,N)*VY(I,N)
        DZZ(I)=PZ(I,N)*VZ(I,N)
        DXY(I)=PY(I,N)*VX(I,N)
        DXZ(I)=PZ(I,N)*VX(I,N)
        DYX(I)=PX(I,N)*VY(I,N)
        DYZ(I)=PZ(I,N)*VY(I,N)
        DZX(I)=PX(I,N)*VZ(I,N)
        DZY(I)=PY(I,N)*VZ(I,N)
      ENDDO
!
      DO N=2,NPE
       DO I=LFT,LLT
        DXX(I)=DXX(I)+PX(I,N)*VX(I,N)
        DYY(I)=DYY(I)+PY(I,N)*VY(I,N)
        DZZ(I)=DZZ(I)+PZ(I,N)*VZ(I,N)
        DXY(I)=DXY(I)+PY(I,N)*VX(I,N)
        DXZ(I)=DXZ(I)+PZ(I,N)*VX(I,N)
        DYX(I)=DYX(I)+PX(I,N)*VY(I,N)
        DYZ(I)=DYZ(I)+PZ(I,N)*VY(I,N)
        DZX(I)=DZX(I)+PX(I,N)*VZ(I,N)
        DZY(I)=DZY(I)+PY(I,N)*VZ(I,N)
       ENDDO
      ENDDO
C
      DT1D2=HALF*DT1
C
      DO I=LFT,LLT
       D4(I)   =DXY(I)+DYX(I)
       D5(I)   =DYZ(I)+DZY(I)
       D6(I)   =DXZ(I)+DZX(I)
       WZZ(I)=DT1D2*(DYX(I)-DXY(I))
       WYY(I)=DT1D2*(DXZ(I)-DZX(I))
       WXX(I)=DT1D2*(DZY(I)-DYZ(I))
      ENDDO
C
      RETURN
C
      END
