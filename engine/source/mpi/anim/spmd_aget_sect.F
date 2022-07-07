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

C            DXYZSECT                /anim/dxyzsect.F
C            DXWALL                  /anim/dxwall.F
Chd|====================================================================
Chd|  SPMD_AGET_SECT                source/mpi/anim/spmd_aget_sect.F
Chd|-- called by -----------
Chd|        DXYZSECT                      source/output/anim/generate/dxyzsect.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SPMD_AGET_SECT(NSTRF,X,XSEC,WEIGHT,ITAB)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include "r4r8_p.inc"
#ifdef MPI
#include "mpif.h"
#endif
#include "com04_c.inc"
#include "task_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       my_real 
     .    XSEC(3,3,NSECT),X(3,*)
       INTEGER NSTRF(*),WEIGHT(*),ITAB(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER IERROR,
     .        STATUS(MPI_STATUS_SIZE)

       my_real
     .    XSECTMP(3,3,NSECT)

       INTEGER I,K1,N1,N2,N3,LEN

         DO I=1,NSECT
            XSECTMP(1,1,I)=ZERO
            XSECTMP(1,2,I)=ZERO
            XSECTMP(1,3,I)=ZERO

            XSECTMP(2,1,I)=ZERO
            XSECTMP(2,2,I)=ZERO
            XSECTMP(2,3,I)=ZERO

            XSECTMP(3,1,I)=ZERO
            XSECTMP(3,2,I)=ZERO
            XSECTMP(3,3,I)=ZERO

            XSEC(1,1,I)=ZERO
            XSEC(1,2,I)=ZERO
            XSEC(1,3,I)=ZERO

            XSEC(2,1,I)=ZERO
            XSEC(2,2,I)=ZERO
            XSEC(2,3,I)=ZERO

            XSEC(3,1,I)=ZERO
            XSEC(3,2,I)=ZERO
            XSEC(3,3,I)=ZERO

         ENDDO

         K1 = 33

         DO I=1,NSECT
            N1 = NSTRF(K1+1)
            N2 = NSTRF(K1+2)
            N3 = NSTRF(K1+3)
            IF (N1.GT.0) THEN
              IF (WEIGHT(N1).EQ.1) THEN
                XSECTMP(1,1,I) = X(1,N1)
                XSECTMP(1,2,I) = X(2,N1)
                XSECTMP(1,3,I) = X(3,N1)
              ENDIF
            ENDIF

            IF (N2.GT.0) THEN
              IF (WEIGHT(N2).EQ.1) THEN
                XSECTMP(2,1,I) = X(1,N2)
                XSECTMP(2,2,I) = X(2,N2)
                XSECTMP(2,3,I) = X(3,N2)
              ENDIF
            ENDIF

            IF (N3.GT.0) THEN
              IF (WEIGHT(N3).EQ.1) THEN
                XSECTMP(3,1,I) = X(1,N3)
                XSECTMP(3,2,I) = X(2,N3)
                XSECTMP(3,3,I) = X(3,N3)
              ENDIF
            ENDIF

            K1= NSTRF(K1+22)+2
         ENDDO
         
           LEN = 9*NSECT

          
           CALL MPI_REDUCE(XSECTMP,XSEC,LEN,
     .                  REAL,MPI_SUM,IT_SPMD(1),
     .                  MPI_COMM_WORLD,IERROR)


C  
C           IF (ISPMD.EQ.0) THEN
C             DO I=1,NSECT
C               print*,I,'XSEC N1= ',XSEC(1,1,I),XSEC(1,2,I),XSEC(1,3,I)
C               print*,I,'XSEC N2= ',XSEC(2,1,I),XSEC(2,2,I),XSEC(2,3,I)
C               print*,I,'XSEC N2= ',XSEC(3,1,I),XSEC(3,2,I),XSEC(3,3,I)
C               print*,' ' 
C             ENDDO
C           ENDIF
#endif
       RETURN
       END
