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
Chd|  DSCALN                        source/implicit/dsolve/dscaln.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SPMD_DS_IRECV                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_DS_ISEND                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE DSCALN(GRAPHE, NDDL, N)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "scr05_c.inc"
#include "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL, N
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ITAG(NDDL), J, JJ, NDFMAX, IPARAM(NSPMD-1), II,
     .        IRQTAG, MSGOFF, NDF
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: PDDLS
C
      DATA MSGOFF  /200000/ 
C
      DO I=1,NDDL
         ITAG(I)=0
      ENDDO
      IF (IMACH/=3) THEN
         DO I=1,NSPROC
            DO J=1,GRAPHE(I)%DGRAPH(1)%NDDL_F
               JJ=GRAPHE(I)%DGRAPH(1)%DDLS(J)
               ITAG(JJ)=1
            ENDDO
         ENDDO
         N=0
         DO I=1,NDDL
            N=N+ITAG(I)
         ENDDO
      ELSE
         IF (ISPMD==0) THEN
            NDFMAX=0
            DO I=1,NSPMD-1
               IRQTAG=MSGOFF + I
               CALL SPMD_DS_IRECV(IPARAM(I), 1, IRQTAG, I+1)
               NDFMAX=MAX(NDFMAX,IPARAM(I))
            ENDDO
C
            ALLOCATE(PDDLS(NDFMAX,NSPMD-1))
            DO I=1,GRAPHE(1)%DGRAPH(1)%NDDL_F
               II=GRAPHE(1)%DGRAPH(1)%DDLS(I)
               II=GRAPHE(1)%LSDDL_GLOB(II)
               ITAG(II)=1
            ENDDO
C
            DO I=1,NSPMD-1
               IRQTAG=MSGOFF + (NSPMD-1) + I
               CALL SPMD_DS_IRECV(PDDLS(1,I), IPARAM(I), IRQTAG, I+1)
               NDF=IPARAM(I)
               DO J=1,NDF
                  JJ=PDDLS(J,I)
                  ITAG(JJ)=1
               ENDDO
            ENDDO
            N=0
            DO I=1,NDDL
               N=N+ITAG(I)
            ENDDO
            DEALLOCATE(PDDLS)
         ELSE
            IPARAM(1)=GRAPHE(1)%DGRAPH(1)%NDDL_F
            IRQTAG=MSGOFF + ISPMD
            CALL SPMD_DS_ISEND(IPARAM(1), 1, IRQTAG, 1)
            ALLOCATE(PDDLS(IPARAM(1),1))
            DO I=1,IPARAM(1)
               II=GRAPHE(1)%DGRAPH(1)%DDLS(I)
               II=GRAPHE(1)%LSDDL_GLOB(II)
               PDDLS(I,1)=II
            ENDDO
            IRQTAG=MSGOFF + (NSPMD-1) + ISPMD
            CALL SPMD_DS_ISEND(PDDLS, IPARAM(1), IRQTAG, 1)
            DEALLOCATE(PDDLS)
         ENDIF
C Broadcast du nombre de modes statiques total
         IF (NSPMD>1) 
     .      CALL SPMD_IBCAST(N, N, 1, 1, 0, 2)
      ENDIF
C
      RETURN
      END
      
