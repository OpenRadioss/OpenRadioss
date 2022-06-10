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
Chd|  BULKIGE3                      source/elements/ige3d/bulkigeo3.F
Chd|-- called by -----------
Chd|        IG3DINIT3                     source/elements/ige3d/ig3dinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BULKIGE3(
     1          VOLU    ,NCTRL   ,MAT     ,PM      ,VNIGE   ,BNIGE     ,
     2          PX      ,PY      ,PZ      ,NCTRLMAX)
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
#include      "scr05_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NCTRL, NCTRLMAX, MAT(*), PX,PY,PZ
C     REAL
      my_real
     .   VNIGE(NCTRLMAX,*),BNIGE(NCTRLMAX,*),VOLU(*),
     .   PM(NPROPM,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N
C     REAL
      my_real
     .   AV1,CV1
C-----------------------------------------------------------------------
C
      IF(NCTRL.EQ.0) RETURN
C 
C     isogeometric elements :
      DO I=LFT,LLT
        AV1 = VOLU(I)/NCTRL
        CV1 = PM(32,MAT(I))*AV1
        DO N=1,NCTRL
          VNIGE(N,I)=AV1
          BNIGE(N,I)=CV1
        ENDDO
      ENDDO

CC NB : on peut repartir le volume differemment selon si les points sont dans les coins, au milieu
cC      ou sur une arrete, avec PX,PY,PZ on peut retrouver la place de chacun
Cc pour les elements 20 neouds, il y a 3/192 * vol aux coins et 14/192 * vol aux points milieux
C
      RETURN
      END
