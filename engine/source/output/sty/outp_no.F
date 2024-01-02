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
Chd|  OUTP_NO                       source/output/sty/outp_no.F   
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|-- calls ---------------
Chd|        SPMD_DOUTP_GATH               source/mpi/interfaces/spmd_outp.F
Chd|====================================================================
      SUBROUTINE OUTP_NO(KEY ,TEXT  ,LEN       ,ANIN   ,NUMNOD,
     2                   ITAB,INNODA,ITABG,LENG,NODGLOB,WEIGHT)
C-----------------------------------------------
C NODAL VALUES IN OUTP FILES
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "task_c.inc"
#include      "scr16_c.inc"
#include      "spmd_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*10 KEY
      CHARACTER*40 TEXT
      INTEGER ITAB(*), LEN, NUMNOD, INNODA,ITABG(*),LENG,NODGLOB(*),WEIGHT(*)
      my_real ANIN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N
      my_real VGLOB(LENG)
C-----------------------------------------------

      IF (NSPMD > 1) THEN
         CALL SPMD_DOUTP_GATH(ANIN(1+INNODA),NODGLOB,WEIGHT,VGLOB)
         IF (ISPMD/=0) RETURN
      ENDIF

C
       WRITE(IUGEO,'(2A)')'/NODAL     /SCALAR    /',KEY
       WRITE(IUGEO,'(A)')TEXT(1:LEN)
       IF (IOUTP_FMT==2) THEN
         WRITE(IUGEO,'(A)')'#FORMAT: (I8,1P1E16.9) '
       ELSE
         WRITE(IUGEO,'(A)')'#FORMAT: (I10,1P1E20.13) '
       ENDIF
       WRITE(IUGEO,'(2A)')'# USRNOD     NODAL VALUE'
       IF (NSPMD == 1) THEN
         IF (OUTYY_FMT==2) THEN
           WRITE(IUGEO,'(I8,1P1E16.9)')
     .          (ITAB(J),ANIN(J+INNODA),J=1,NUMNOD)
         ELSE
           WRITE(IUGEO,'(I10,1P1E20.13)')
     .          (ITAB(J),ANIN(J+INNODA),J=1,NUMNOD)
         ENDIF
       ELSE
         IF (OUTYY_FMT==2) THEN
           WRITE(IUGEO,'(I8,1P1E16.9)')
     .          (ITABG(J),VGLOB(J),J=1,NUMNODG)
         ELSE
           WRITE(IUGEO,'(I10,1P1E20.13)')
     .          (ITABG(J),VGLOB(J),J=1,NUMNODG)
         ENDIF
       ENDIF
C
C INNODA : ADRESSE DE LA DERNIERE VARIABLE
C (INCREMENTEE DE NUMNOD A CHAQUE PASSAGE)
C 
      RETURN
      END
