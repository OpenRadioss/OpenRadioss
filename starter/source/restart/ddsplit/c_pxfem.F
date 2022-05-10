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
Chd|  C_PXFEM                       source/restart/ddsplit/c_pxfem.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C_PXFEM(CEP,P ,NODGLOB  , IELPXFEM  ,INODPXFEM,
     .                 IEL_L,INOD_L , NUMELC ,NUMNOD_L,NUMELC_L,
     .                 NELPXFE_L,NUMNODPXFE_L,INDEX)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  CEP(*), P,NELPXFE_L ,NUMNODPXFE_L,NODGLOB(*),
     .     NUMELC ,NUMNOD_L  ,IELPXFEM(*),INODPXFEM(*),
     .     IEL_L(*),INOD_L(*),INDEX(*),NUMELC_L
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NL_L, N,II,NEL_L,NEL1_L,JJ
C
ccc      CEL_PXFEM(1:EPLYXFE) = 0
C
      IEL_L(1:NUMELC_L) = 0
      INOD_L(1:NUMNOD_L) = 0
      NEL_L = 0
      NEL1_L = 1
      II = 0
C
      DO I = 1, NUMELC
        IF(CEP(I).EQ.P)THEN
cc           IEL_L(NEL1_L) = 0
           II  = IELPXFEM(I)
           IF(II  > 0) THEN
               NEL_L = NEL_L + 1
cc               CEL_PXFEM(II) = NEL_L
               IEL_L(NEL1_L) = NEL_L
           ENDIF
           NEL1_L = NEL1_L + 1
         ENDIF
      ENDDO
C
      NELPXFE_L = NEL_L
C
      NL_L  = 0
      INDEX(1:NPLYXFE) = 0
      DO I = 1, NUMNOD_L
        II = NODGLOB(I)
        JJ = INODPXFEM(II)
        IF(JJ > 0)THEN
          NL_L  = NL_L + 1
          INOD_L(I) = NL_L
          INDEX(NL_L) = JJ
        ENDIF
      END DO
C
      NUMNODPXFE_L   = NL_L
      RETURN
      END
