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
C-------------------------------------------------------------------------
Chd|====================================================================
Chd|  M24INSPH                      source/elements/sph/m24insph.F
Chd|-- called by -----------
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M24INSPH(PM     ,ANG    ,SF     ,VK0    ,ROB    ,
     .                    IPART  ,IPARTSP,NEL    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPART(LIPART1,*),IPARTSP(*),NEL
C     REAL
      my_real
     .   PM(NPROPM,*), ANG(NEL,6), SF(NEL,3), VK0(*), ROB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IPRT, MX, IG, ISK
C     REAL
C-----------------------------------------------
      DO 20 I=LFT,LLT
      ANG(I,1)=ONE
      ANG(I,2)=ZERO
      ANG(I,3)=ZERO
      ANG(I,4)=ZERO
      ANG(I,5)=ONE
   20 ANG(I,6)=ZERO
C
      DO 30 I=LFT,LLT
      SF(I,1)=-ONE
      SF(I,2)=-ONE
   30 SF(I,3)=-ONE
C
      DO 40 I=LFT,LLT
      IPRT=IPARTSP(I+NFT)
      MX  =IPART(1,IPRT)
      ROB(I)=PM(30,MX)
   40 VK0(I)=PM(46,MX)
C
      RETURN
      END
