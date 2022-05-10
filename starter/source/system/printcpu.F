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
Chd|  PRINTCPU                      source/system/printcpu.F      
Chd|-- called by -----------
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|        CPUINFO                       source/system/mon_c.c         
Chd|====================================================================
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

        CALL CPUINFO(HOSTNAME,LENH,CPUNAM, LENC, FREQUENCE,MEM,SWAP)

        IF(MEM.GT.-1)THEN
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

 7000 FORMAT (' COMPUTATION HARDWARE DESCRIPTION')
 7400 FORMAT (' HOSTNAME     CPU TYPE, FREQUENCY AND MEMORY')
 7450 FORMAT (' HOSTNAME     CPU TYPE AND FREQUENCY')
 7500 FORMAT (' CPU#  HOSTNAME     CPU TYPE, FREQUENCY AND MEMORY')
 7550 FORMAT (' CPU#  HOSTNAME     CPU TYPE AND FREQUENCY')

      END
