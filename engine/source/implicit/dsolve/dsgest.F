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
Cf51e6 Nouvelle routine
Chd|====================================================================
Chd|  DSDEAL                        source/implicit/dsolve/dsgest.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE DSDEAL(GRAPHE)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J, DSLEN
C
      DO I=1,NSPROC
         DSLEN=GRAPHE(I)%NSUPEL
         DEALLOCATE(GRAPHE(I)%LSDDL)
         DEALLOCATE(GRAPHE(I)%LSDDL_INI)
         DEALLOCATE(GRAPHE(I)%LSDDL_GLOB)
         DO J=1,DSLEN
            DEALLOCATE(GRAPHE(I)%DGRAPH(J)%CHILD)
            DEALLOCATE(GRAPHE(I)%DGRAPH(J)%DDLS)
            DEALLOCATE(GRAPHE(I)%DGRAPH(J)%IFAC)
            DEALLOCATE(GRAPHE(I)%DGRAPH(J)%IFAC_INI)
            DEALLOCATE(GRAPHE(I)%DGRAPH(J)%IFACM)
            DEALLOCATE(GRAPHE(I)%DGRAPH(J)%IFACM_INI)
            DEALLOCATE(GRAPHE(I)%DGRAPH(J)%CMDYN)
            DEALLOCATE(GRAPHE(I)%DGRAPH(J)%B)
            DEALLOCATE(GRAPHE(I)%DGRAPH(J)%IFI)
            DEALLOCATE(GRAPHE(I)%DGRAPH(J)%D)
            DEALLOCATE(GRAPHE(I)%DGRAPH(J)%DM)
         ENDDO
         DEALLOCATE(GRAPHE(I)%DGRAPH)
         DEALLOCATE(GRAPHE(I)%IPROW)
         DEALLOCATE(GRAPHE(I)%IPCOL)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  DSALLOC                       source/implicit/dsolve/dsgest.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE DSALLOC(GRAPHE)
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
#include      "buckcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J, DSLEN, NDI, NDF, NMAX, NSDEC, NSVMAX, NMAX2
C      
      NMAX=MAX(NSBMAX,NSVMAXT)
      NMAX=MAX(NBUCK,NMAX)
      NMAX2=MAX(NBUCK,NSVMAXT)
C
      DO I=1,NSPROC
         NSDEC=GRAPHE(I)%NSDEC
         NSVMAX=GRAPHE(I)%NSVMAX
C
         DSLEN=GRAPHE(I)%NSUPEL
         DO J=1,DSLEN
            ALLOCATE(GRAPHE(I)%DGRAPH(J)%CMDYN(NSVMAX))
            NDI=GRAPHE(I)%DGRAPH(J)%NDDL_I
            NDF=GRAPHE(I)%DGRAPH(J)%NDDL_F
            ALLOCATE(GRAPHE(I)%DGRAPH(J)%B(NSBMAX,NDF))
            ALLOCATE(GRAPHE(I)%DGRAPH(J)%IFI(NSBMAX,NDI))
            ALLOCATE(GRAPHE(I)%DGRAPH(J)%D(NMAX,NDI+NDF+NSVMAX*NSDEC))
            ALLOCATE(GRAPHE(I)%DGRAPH(J)%DM(NMAX2,NSVMAX))
         ENDDO
         ALLOCATE(GRAPHE(I)%IPROW(0,0))
         ALLOCATE(GRAPHE(I)%IPCOL(0,0))
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  DSOPEN                        source/implicit/dsolve/dsgest.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE DSOPEN(GRAPHE)      
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
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER RCLEN, I, IFILE, J, K, L, DSLEN, NDI, NDF, NSVMAX
      my_real
     .        DSTAB(10)
*
      CHARACTER FILNAM*7
C Ouverture des fichiers a acces direct
      INQUIRE(IOLENGTH=RCLEN) DSTAB
      DO I=1,NSPROC
         IFILE=IFICDS+I-1
         OPEN(UNIT=IFILE,STATUS='SCRATCH',ACCESS='DIRECT',RECL=RCLEN)
      ENDDO
C Initialisation a zero des tableaux temporaires
      DO I=1,NSPROC
         DSLEN=GRAPHE(I)%NSUPEL
         NSVMAX=GRAPHE(I)%NSVMAX
         DO J=1,DSLEN
            GRAPHE(I)%DGRAPH(J)%NMDYN=0
            GRAPHE(I)%DGRAPH(J)%NDDL_M=0
            DO K=1,NSVMAX
               GRAPHE(I)%DGRAPH(J)%CMDYN(K)=0
            ENDDO
            NDI=GRAPHE(I)%DGRAPH(J)%NDDL_I
            NDF=GRAPHE(I)%DGRAPH(J)%NDDL_F
            DO K=1,NSBMAX
               DO L=1,NDF
                  GRAPHE(I)%DGRAPH(J)%B(K,L)=ZERO
               ENDDO
               DO L=1,NDI
                  GRAPHE(I)%DGRAPH(J)%IFI(K,L)=ZERO
               ENDDO
            ENDDO
         ENDDO
      ENDDO   
C
      RETURN
      END
Chd|====================================================================
Chd|  DSCLOSE                       source/implicit/dsolve/dsgest.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DSCLOSE      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IFILE
C
      DO I=1,NSPROC
         IFILE=IFICDS+I-1
         CLOSE(IFILE)
      ENDDO
C
      RETURN
      END
                     
