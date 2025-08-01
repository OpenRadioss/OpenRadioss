Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    printcpu               ../starter/source/system/printcpu.F
!||--- called by ------------------------------------------------------
!||    starter0               ../starter/source/starter/starter0.F
!||--- calls      -----------------------------------------------------
!||    cpuinfo                ../starter/source/system/mon_c.c
!||    my_flush               ../starter/source/system/machine.F
!||====================================================================
      SUBROUTINE PRINTCPU()
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include        "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITASK

      CHARACTER*256 HOSTNAME, CPUNAM
      INTEGER FREQUENCE,LENH,LENC,I,NPROC,MEM,SWAP
      !
      CHARACTER(len=129) :: STACKSIZE_INFO
      INTEGER            :: STACKSIZE_INFO_LEN

      CHARACTER(len=129) :: OMP_STACKSIZE_INFO
      INTEGER            :: OMP_STACKSIZE_INFO_LEN

        CALL CPUINFO(HOSTNAME,LENH,CPUNAM, LENC, FREQUENCE,MEM,SWAP)

        IF(MEM>-1)THEN
          WRITE(IOUT,*)' ' 
          WRITE(IOUT,7000) 
          WRITE(IOUT,*)' ' 

          WRITE(IOUT,7400) 

          WRITE(IOUT,'(A,A,A,A,A,I4,A,I6,A,I6,A)') ' ',
     .    HOSTNAME(1:LENH),'   ',CPUNAM(1:LENC),', ',FREQUENCE,
     .    ' MHz, ',MEM,' MB RAM, ',SWAP,' MB swap'

          WRITE(IOUT,*)' ' 

        ELSE
          WRITE(IOUT,*)' ' 
          WRITE(IOUT,7000) 
          WRITE(IOUT,*)' ' 

          WRITE(IOUT,7450) 

          WRITE(IOUT,'(A,A,A,A,A,I4,A)') ' ',
     .    HOSTNAME(1:LENH),'   ',CPUNAM(1:LENC),', ',FREQUENCE,
     .    ' MHz '

          WRITE(IOUT,*)' ' 
        ENDIF

        OMP_STACKSIZE_INFO_LEN=128
        STACKSIZE_INFO_LEN=128
        call GET_SOLVER_STACKSIZE(STACKSIZE_INFO,STACKSIZE_INFO_LEN,OMP_STACKSIZE_INFO,OMP_STACKSIZE_INFO_LEN)
        WRITE(IOUT,'(A,A)') ' STACKSIZE  . . . . . . . . . . . . . . . . . . . . ',STACKSIZE_INFO(1:STACKSIZE_INFO_LEN)
        WRITE(IOUT,'(A,A)') ' THREAD STACKSIZE . . . . . . . . . . . . . . . . . ',OMP_STACKSIZE_INFO(1:OMP_STACKSIZE_INFO_LEN)
        WRITE(IOUT,'(A)') ' '
        ! Force write to output file
        CALL MY_FLUSH(IOUT)   

 7000 FORMAT (' COMPUTATION HARDWARE DESCRIPTION')
 7400 FORMAT (' HOSTNAME     CPU TYPE, FREQUENCY AND MEMORY')
 7450 FORMAT (' HOSTNAME     CPU TYPE AND FREQUENCY')
 7500 FORMAT (' CPU#  HOSTNAME     CPU TYPE, FREQUENCY AND MEMORY')
 7550 FORMAT (' CPU#  HOSTNAME     CPU TYPE AND FREQUENCY')

      END
