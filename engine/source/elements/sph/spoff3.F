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
Chd|  SPOFF3                        source/elements/sph/spoff3.F  
Chd|-- called by -----------
Chd|        SPSTRES                       source/elements/sph/spstres.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPOFF3(KXSP,VD2,VIS,OFFG,OFF,
     .   NC1,NGL,MXT,NGEO,IPART,
     .   IPARTSP)
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
#include      "sphcom.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .  VIS(*),VD2(*),OFFG(*),OFF(*)
      INTEGER NC1(*), MXT(*), NGL(*), NGEO(*), KXSP(NISP,*),
     .        IPART(LIPART1,*),IPARTSP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IPRT
C     REAL
C-----------------------------------------------
      DO I=LFT,LLT
       VIS(I)=ZERO
       IPRT   =IPARTSP(I)
       NGEO(I)=IPART(2,IPRT)
       NGL(I) =KXSP(NISP,I)
       MXT(I) =IPART(1,IPRT)
       NC1(I) =KXSP(3,I)
      ENDDO
      DO I=LFT,LLT
C      OFF(I) = MIN(ONE,OFFG(I))
       OFF(I) = MIN(ONE,ABS(OFFG(I)))
      ENDDO
C     LAGRANGIAN ONLY.
C     IF (JLAG/=0)THEN
      DO I=LFT,LLT
       VD2(I)=ZERO
      ENDDO
C-----------------------------------------------
      RETURN
      END
