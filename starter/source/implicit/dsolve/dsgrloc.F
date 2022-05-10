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
Chd|  DSGRLOC                       source/implicit/dsolve/dsgrloc.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        DSGRAPH_MOD                   share/modules1/dsgraph_mod.F  
Chd|====================================================================
      SUBROUTINE DSGRLOC(PGRAPH, NODLOCAL, P, MEMORY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODLOCAL(*), P
      INTEGER(KIND=8) :: MEMORY(*)
      TYPE(PRGRAPH) :: PGRAPH
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NDDL, NDDL_NEW, INO, NDF, NDI, JJ, NSLEVEL, NSDEC,
     .        DSLEN, NSDMAX, FAC, FAC2, NBSD, OFFSD, ISD, NMIN, NMAX,
     .        K, KK
      INTEGER, ALLOCATABLE :: LSDDL_NEW(:,:), LSDDL_GLOB(:), 
     .                        LSDDL_LOC(:), IFAC2(:)
C
C Nouvelle liste de ddls (locale au processeur)
      NDDL=PGRAPH%NDDL
      PGRAPH%NDDL_GLOB=NDDL
      ALLOCATE(LSDDL_NEW(2,NDDL), LSDDL_GLOB(NDDL), LSDDL_LOC(NDDL))
      NDDL_NEW=0
      DO I=1,NDDL
         INO=PGRAPH%LSDDL(1,I)
         IF (NODLOCAL(INO).NE.0) THEN
            NDDL_NEW=NDDL_NEW+1
            LSDDL_NEW(1,NDDL_NEW)=NODLOCAL(INO)
            LSDDL_NEW(2,NDDL_NEW)=PGRAPH%LSDDL(2,I)
            LSDDL_GLOB(NDDL_NEW)=I
            LSDDL_LOC(I)=NDDL_NEW
         ENDIF
      ENDDO
      DEALLOCATE(PGRAPH%LSDDL, PGRAPH%LSDDL_GLOB)
      ALLOCATE(PGRAPH%LSDDL(2,NDDL_NEW), PGRAPH%LSDDL_GLOB(NDDL_NEW))
      PGRAPH%NDDL=NDDL_NEW
      DO I=1,NDDL_NEW
         PGRAPH%LSDDL(1,I)=LSDDL_NEW(1,I)
         PGRAPH%LSDDL(2,I)=LSDDL_NEW(2,I)
         PGRAPH%LSDDL_GLOB(I)=LSDDL_GLOB(I)
      ENDDO
C Mise a jour de la liste des ddls pour chaque superelements du processeur
      DO I=1,PGRAPH%NSUPEL
         NDF=PGRAPH%DGRAPH(I)%NDDL_F
         NDI=PGRAPH%DGRAPH(I)%NDDL_I
         DO J=1,NDF+NDI
            JJ=PGRAPH%DGRAPH(I)%DDLS(J)
            PGRAPH%DGRAPH(I)%DDLS(J)=LSDDL_LOC(JJ)
         ENDDO
      ENDDO
      DEALLOCATE(LSDDL_NEW, LSDDL_GLOB, LSDDL_LOC)
C Correction du tableau IFAC au niveau le plus bas pour chaque processeur
      NSLEVEL=PGRAPH%NSLEVEL
      NSDEC=PGRAPH%NSDEC
      DSLEN=PGRAPH%NSUPEL
      NBSD=NSDEC**(NSLEVEL-1)
      OFFSD=DSLEN-NBSD
      NMIN=(P-1)*DSLEN+OFFSD+1
      NMAX=NMIN+NBSD-1
      DO I=1,NBSD
         ISD=OFFSD+I
         NDF=PGRAPH%DGRAPH(ISD)%NDDL_F
         NSDMAX=PGRAPH%DGRAPH(ISD)%NSDMAX
         DO J=1,NDF
            ALLOCATE(IFAC2(NSDMAX))
            FAC=PGRAPH%DGRAPH(ISD)%IFAC(1,J)
            FAC2=0
            DO K=1,FAC
               KK=PGRAPH%DGRAPH(ISD)%IFAC(K+1,J)
               IF (KK.GE.NMIN.AND.KK.LE.NMAX) THEN
                  FAC2=FAC2+1
                  IFAC2(FAC2)=KK
               ENDIF
            ENDDO
            PGRAPH%DGRAPH(ISD)%IFAC(1,J)=FAC2
            DO K=1,FAC2
               PGRAPH%DGRAPH(ISD)%IFAC(K+1,J)=IFAC2(K)-(P-1)*DSLEN
            ENDDO
            DO K=FAC2+1,NSDMAX
               PGRAPH%DGRAPH(ISD)%IFAC(K+1,J)=0
            ENDDO
            DEALLOCATE(IFAC2)
         ENDDO
      ENDDO
C Memoire pour stockage des champs de deplacements
      IF (NEIG.GT.0) MEMORY(1)=NSVMAXT*NDDL_NEW
      MEMORY(1)=MEMORY(1)+2*NSBMAX*NDDL_NEW
C
      RETURN
      END
            
