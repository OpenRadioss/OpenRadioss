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
Chd|====================================================================
Chd|  NINTERP                       source/interfaces/int14/ninterp.F
Chd|-- called by -----------
Chd|        I14DMP                        source/interfaces/int14/i14dmp.F
Chd|        I14FRT                        source/interfaces/int14/i14frt.F
Chd|        I6DAMP                        source/interfaces/inter3d/i6damp.F
Chd|        I6FRI3                        source/interfaces/inter3d/i6fri3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE NINTERP(IFUNC,NPC,PLD,NPOINT,XV,YV)
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
      INTEGER IFUNC,NPC(*),NPOINT
C     REAL
      my_real
     .  PLD(*),XV(*),YV(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   PENTE, Y
      INTEGER RANG(MVSIZ)
      INTEGER NDEB, NREST, I,
     .        IPT, NPT, II
C-----------------------------------------------
      NDEB=0
      NREST=NPOINT
 100  CONTINUE
C---------------------------------
      DO I=1,MIN(MVSIZ,NREST)
        RANG(I)=0
      ENDDO
C---------------------------------
      NPT = (NPC(IFUNC+1)-NPC(IFUNC))/2
      DO IPT=1,NPT-1
        II=NPC(IFUNC)+2*(IPT-1)
        DO I=1,MIN(MVSIZ,NREST)
          IF (PLD(II)<=XV(NDEB+I)
     .    .AND.XV(NDEB+I)<=PLD(II+2)) RANG(I)=IPT
        ENDDO
      ENDDO
      II=NPC(IFUNC)+2*(NPT-1)
      DO I=1,MIN(MVSIZ,NREST)
          IF (XV(NDEB+I)>PLD(II)) RANG(I)=NPT
      ENDDO
C---------------------------------
      DO I=1,MIN(MVSIZ,NREST)
        IF (RANG(I)==0) THEN
          II=NPC(IFUNC)
          PENTE=(PLD(II+3)-PLD(II+1))/(PLD(II+2)-PLD(II))
          YV(NDEB+I) =PLD(II+1)+PENTE*(XV(NDEB+I)-PLD(II))
        ELSEIF (RANG(I)==NPT) THEN
          II=NPC(IFUNC)+2*(NPT-1)
          PENTE=(PLD(II+1)-PLD(II-1))/(PLD(II)-PLD(II-2))
          YV(NDEB+I) =PLD(II+1)
     .       +MAX(-PLD(II+1),PENTE*(XV(NDEB+I)-PLD(II)))
        ELSE
          II=NPC(IFUNC)+2*(RANG(I)-1)
          PENTE=(PLD(II+3)-PLD(II+1))/(PLD(II+2)-PLD(II))
          YV(NDEB+I) =PLD(II+1)
     .       +PENTE*(XV(NDEB+I)-PLD(II))
        ENDIF
      ENDDO
C---------------------------------
C     Groupe suivant.
C---------------------------------
      IF (NREST-MVSIZ>0) THEN
        NREST=NREST-MVSIZ
        NDEB =NDEB +MVSIZ
        GOTO 100
      END IF
C-----------------------------------------------
      RETURN
      END
