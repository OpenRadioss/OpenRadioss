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
Chd|  SPBACK3                       source/elements/sph/spback3.F 
Chd|-- called by -----------
Chd|        SPSTRES                       source/elements/sph/spstres.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPBACK3(SIG     ,QVIS    ,STI     ,SSP     ,WA      ,
     .                   SSP_EQ  ,DIE     ,NEL     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .   SIG(NEL,6),QVIS(*),STI(*),SSP(*),WA(KWASPH,*),
     .   SSP_EQ(*),DIE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N
C     REAL
C-----------------------------------------------
C     overloads D in WA.
      DO I=LFT,LLT
       N=NFT+I
       WA(1,N)=SIG(I,1)
       WA(2,N)=SIG(I,2)
       WA(3,N)=SIG(I,3)
       WA(4,N)=SIG(I,4)
       WA(5,N)=SIG(I,5)
       WA(6,N)=SIG(I,6)
       WA(7,N)=STI(I)
       WA(8,N)=SSP(I)
C      SSP_EQ comes back from MDTSPH and is the equivalent material sound speed
C      (including material viscosity)
       WA(9,N)=SSP_EQ(I)
C      Thermique
       WA(15,N)=WA(15,N)+DIE(I)  
      ENDDO
      RETURN
      END
