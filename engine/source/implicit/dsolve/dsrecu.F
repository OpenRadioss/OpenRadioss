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
Chd|  DSRECU                        source/implicit/dsolve/dsrecu.F
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_DSOLV                     source/implicit/imp_dsolv.F   
Chd|        IMP_DSOLV_ITER                source/implicit/imp_dsolv.F   
Chd|-- calls ---------------
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE DSRECU(GRAPHE, NV, VECT, NDDL, LDV,
     .                  ITRANS)
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
      INTEGER NV, NDDL, LDV, ITRANS
      my_real
     .        VECT(LDV,*)
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, KK, L, DSLEN, NBSD, OFFSD, NDI, NDF, ISD,
     .        NSDEC, NSLEVEL
C
      IF (ITRANS==0) THEN
         DO I=1,NSPROC
C
            DSLEN=GRAPHE(I)%NSUPEL
            NSLEVEL=GRAPHE(I)%NSLEVEL
            NSDEC=GRAPHE(I)%NSDEC
            NBSD=NSDEC**(NSLEVEL-1)
            OFFSD=DSLEN-NBSD
            DO J=1,NBSD
               ISD=OFFSD+J
               NDI=GRAPHE(I)%DGRAPH(ISD)%NDDL_I
               NDF=GRAPHE(I)%DGRAPH(ISD)%NDDL_F
               DO K=1,NDI+NDF
                  KK=GRAPHE(I)%DGRAPH(ISD)%DDLS(K)
                  DO L=1,NV
                     VECT(L,KK)=GRAPHE(I)%DGRAPH(ISD)%D(L,K)
                  ENDDO
               ENDDO
            ENDDO
C
         ENDDO
      ELSEIF (ITRANS==1) THEN
         DO I=1,NSPROC
C
            DSLEN=GRAPHE(I)%NSUPEL
            NSLEVEL=GRAPHE(I)%NSLEVEL
            NSDEC=GRAPHE(I)%NSDEC
            NBSD=NSDEC**(NSLEVEL-1)
            OFFSD=DSLEN-NBSD
            DO J=1,NBSD
               ISD=OFFSD+J
               NDI=GRAPHE(I)%DGRAPH(ISD)%NDDL_I
               NDF=GRAPHE(I)%DGRAPH(ISD)%NDDL_F
               DO K=1,NDI+NDF
                  KK=GRAPHE(I)%DGRAPH(ISD)%DDLS(K)
                  DO L=1,NV
                     VECT(KK,L)=GRAPHE(I)%DGRAPH(ISD)%D(L,K)
                  ENDDO
               ENDDO
            ENDDO
C
         ENDDO
      ENDIF
C
      RETURN
      END
