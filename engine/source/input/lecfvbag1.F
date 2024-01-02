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
Chd|  LECFVBAG1                     source/input/lecfvbag1.F      
Chd|-- called by -----------
Chd|        LECTUR                        source/input/lectur.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LECFVBAG1(NFVMODI, MONVOL, VOLMON)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFVMODI, MONVOL(*)
      my_real
     .        VOLMON(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ID, K1, KK1, N, ITYP, IGMERG, ILVOUT, IFOUND
      my_real
     .        CGMERG, CNMERG
C
      IF(ISPMD==0) WRITE (IOUT,1000)
      DO I=1,NFVMODI
         READ(IIN,'(I10)') ID
         READ(IIN,'(I10,2F20.0,I10)') IGMERG, CGMERG, CNMERG,ILVOUT
C
         IFOUND=0
         K1=1
         KK1=1
         DO N=1,NVOLU
           IF (MONVOL(K1) == ID) THEN
              ITYP=MONVOL(K1-1+2)
              IFOUND=1
              IF (ITYP/=6.AND.ITYP/=8) THEN
                IF(ISPMD==0)THEN
                   WRITE(IOUT,'(A)')      ' '
                   WRITE(IOUT,'(A)')      ' /FVMBAG/MODIF : '
                   WRITE(IOUT,'(A,I10,A)')' MONITORED VOLUME ID ',ID,
     .                                    ' IS NOT OF TYPE FVMBAG OR'
                   WRITE(IOUT,'(A)')      ' FVMBAG1 INPUT IS IGNORED'
                   WRITE(IOUT,'(A)')      ' '
                ENDIF
              ELSE
                IF(IGMERG==1.OR.IGMERG==2) MONVOL(K1-1+60)=IGMERG
                IF(CGMERG > ZERO ) VOLMON(KK1-1+31)=CGMERG
                IF(CNMERG > ZERO ) VOLMON(KK1-1+34)=CNMERG
                MONVOL(K1-1+44)=ILVOUT
                IF(ISPMD==0)THEN
                   WRITE(IOUT,1100)ID,MONVOL(K1-1+60),VOLMON(KK1-1+31),
     .                             VOLMON(KK1-1+34),MONVOL(K1-1+44)
                ENDIF
              ENDIF
           GO TO 100
           ENDIF
           K1=K1+NIMV
           KK1=KK1+NRVOLU
         ENDDO
         IF(IFOUND == 0) THEN
                 IF(ISPMD==0)THEN
                     WRITE(IOUT,'(A)')      ' '
               WRITE(IOUT,'(A)')      ' /FVMBAG/MODIF : '                 
                 WRITE(IOUT,'(A,I10,A)')' MONITORED VOLUME ID ',ID,
     .                                ' DOES NOT EXIST '
               WRITE(IOUT,'(A)')      ' INPUT IS IGNORED '
               WRITE(IOUT,'(A)')      ' '
           ENDIF
        ENDIF
  100 ENDDO
C
 1000 FORMAT(/' NEW FVMBAG PARAMETERS ')
 1100 FORMAT(/' FVMBAG ID. . . . . . . . . . . . . . .=',I10,
     .       /'   GLOBAL MERGING TYPE. . . . . . . . .=',I10,
     .       /'   GLOBAL MERGING COEFFICIENT . . . . .=',1PG20.13,
     .       /'   NEIGHBORHOOD MERGING COEFFICIENT . .=',1PG20.13,
     .       /'   OUTPUT LEVEL . . . . . . . . . . . .=',I10/)

      RETURN
      END
      
