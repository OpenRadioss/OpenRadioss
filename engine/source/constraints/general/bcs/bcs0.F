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
Chd|  BCS0                          source/constraints/general/bcs/bcs0.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BCS0(ICODE,ICODT,ICODR,ICODE_PLY, ICODT_PLY,IBC_PLY)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICODE(*), ICODT(*), ICODR(*), ICODE_PLY(*), ICODT_PLY(*),
     .        IBC_PLY(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, M, II
C-----------------------------------------------
      DO 10 N=1,NUMNOD
       ICODT(N)=ICODE(N)/512
   10 CONTINUE
C       
      IF(IPLYBCS > 0) THEN
         DO  N=1,NUMNOD
             II = MOD(ICODE_PLY(N),512)
             ICODT_PLY(N) = (ICODE_PLY(N)- II) /512
             IF(II > 10 ) IBC_PLY(N) = 2
             IF(II == 10 ) IBC_PLY(N) = 1
             IF(II == 1 ) IBC_PLY(N) = -1
         ENDDO
      ENDIF 
      IF(IRODDL>0)THEN
       DO 20 N=1,NUMNOD
       ICODR(N)=(ICODE(N)-512*(ICODE(N)/512))/64
   20  CONTINUE
      END IF
C
      IF(IALE>0) THEN
      DO 30 N=1,NUMNOD
      M=N+NUMNOD
      ICODT(M)=(ICODE(N)-64*(ICODE(N)/64))/8
   30 CONTINUE
      DO 40 N=1,NUMNOD
      M=N+NUMNOD+NUMNOD
      ICODT(M)=(ICODE(N)-8*(ICODE(N)/8))
   40 CONTINUE
      END IF
C
      RETURN
      END
