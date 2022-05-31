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
Chd|  DSDESC                        source/implicit/dsolve/dsdesc.F
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_DSOLV                     source/implicit/imp_dsolv.F   
Chd|        IMP_DSOLV_ITER                source/implicit/imp_dsolv.F   
Chd|-- calls ---------------
Chd|        PROGCONDP_C                   source/system/progcond_c.c    
Chd|        PROGCOND_C                    source/system/progcond_c.c    
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE DSDESC(GRAPHE, NSCM, NDDL, IOPT)
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
* Timer
#include      "units_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSCM, NDDL, IOPT
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NBSD, ISD, ILVL, ISD2, I, J, IFILE, OFFSD, NDI, NDF, NDM,
     .        NMDYN, IPD, K, NDPI, NDPF, KK, L, IREC, LEN, IAD, NR,
     .        M, ITAG(NDDL), NSLEVEL, NSDEC, ISUPEL, ISTEP, IPRIB 
      INTEGER, DIMENSION(:), ALLOCATABLE :: PARENT
      my_real
     .        TEMP
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: VS, VD, DB, DI
C
      DO I=1,NSPROC
C Tableau des parents
         ALLOCATE(PARENT(GRAPHE(I)%NSUPEL))
         NBSD=1
         ISD=0
         NSLEVEL=GRAPHE(I)%NSLEVEL
         NSDEC=GRAPHE(I)%NSDEC
         ISUPEL=0
         DO ILVL=1,NSLEVEL-1
            ISD2=ISD+NBSD
            DO J=1,NBSD
               DO K=1,NSDEC
                  PARENT(ISD2+(J-1)*NSDEC+K)=ISD+J
               ENDDO
            ENDDO
            ISD=ISD2
            NBSD=NBSD*NSDEC
         ENDDO
         IPRIB=GRAPHE(I)%IPRI
         IF (IPRIB.LT.0) THEN
            IPRIB=1
         ELSE
            IPRIB=0
         ENDIF
C
         IFILE=GRAPHE(I)%IFILE
         OFFSD=0
         DO ILVL=1,NSLEVEL
            NBSD=NSDEC**(ILVL-1)
            DO J=1,NBSD
               ISD=OFFSD+J
               NDI=GRAPHE(I)%DGRAPH(ISD)%NDDL_I
               NDF=GRAPHE(I)%DGRAPH(ISD)%NDDL_F
               NDM=GRAPHE(I)%DGRAPH(ISD)%NDDL_M
               NMDYN=GRAPHE(I)%DGRAPH(ISD)%NMDYN
               IF (ILVL.GT.1) THEN
C Remplissage des ddls d'interface a partir du parent
                  IPD=PARENT(ISD)
                  DO K=1,NDDL
                     ITAG(K)=0
                  ENDDO
                  NDPI=GRAPHE(I)%DGRAPH(IPD)%NDDL_I
                  NDPF=GRAPHE(I)%DGRAPH(IPD)%NDDL_F
                  DO K=1,NDPI+NDPF
                     KK=GRAPHE(I)%DGRAPH(IPD)%DDLS(K)
                     ITAG(KK)=K
                  ENDDO
                  DO K=1,NDF
                     KK=GRAPHE(I)%DGRAPH(ISD)%DDLS(K)
                     KK=ITAG(KK)
                     DO L=1,NSCM
                        GRAPHE(I)%DGRAPH(ISD)%D(L,K)=
     .                         GRAPHE(I)%DGRAPH(IPD)%D(L,KK)
                     ENDDO
                  ENDDO
C Remplissage des ddls modaux dynamiques si necessaire
                  DO K=1,NMDYN
                     KK=GRAPHE(I)%DGRAPH(ISD)%CMDYN(K)
                     DO L=1,NSCM
                        GRAPHE(I)%DGRAPH(ISD)%DM(L,K)=
     .                           GRAPHE(I)%DGRAPH(IPD)%D(L,KK)
                     ENDDO
                  ENDDO
               ENDIF
C
               DO K=1,NDI+NDM
                  DO L=1,NSCM
                     GRAPHE(I)%DGRAPH(ISD)%D(L,NDF+K)=ZERO
                  ENDDO
               ENDDO
C
               IF (IOPT.EQ.1) THEN
                  DO K=1,NDI
                     DO L=1,NSCM
                        GRAPHE(I)%DGRAPH(ISD)%D(L,NDF+K)=
     .                          GRAPHE(I)%DGRAPH(ISD)%IFI(L,K)
                     ENDDO
                  ENDDO
               ENDIF
C Chargement des modes statiques
               IREC=GRAPHE(I)%DGRAPH(ISD)%MDREC
               LEN=NDI+NDM
               ALLOCATE(VS(NDF,LEN))
               DO K=1,NDF
                  IAD=0
                  DO L=1,LEN/10
                     READ(IFILE,REC=IREC) (VS(K,IAD+M),M=1,10)
                     IREC=IREC+1
                     IAD=IAD+10
                  ENDDO
                  NR=LEN-(LEN/10)*10
                  IF (NR.GT.0) THEN
                     READ(IFILE,REC=IREC) (VS(K,IAD+M),M=1,NR)
                     IREC=IREC+1
                  ENDIF
               ENDDO
               ALLOCATE(DB(NSCM,NDF), DI(NSCM,NDI+NDM))
               DO L=1,NDI+NDM
                  DO K=1,NSCM
                     DI(K,L)=ZERO
                  ENDDO
               ENDDO
C
               DO L=1,NDF
                  DO K=1,NSCM
                     DB(K,L)=GRAPHE(I)%DGRAPH(ISD)%D(K,L)
                  ENDDO
               ENDDO
               IF (NDF.GT.0)
     .            CALL DGEMM('N',  'N', NSCM, NDI+NDM, NDF,
     .                       ONE,   DB,  NSCM, VS,      NDF,
     .                       ZERO, DI,  NSCM)
               DO L=1,NDI+NDM
                  DO K=1,NSCM
                     GRAPHE(I)%DGRAPH(ISD)%D(K,NDF+L)=
     .                   GRAPHE(I)%DGRAPH(ISD)%D(K,NDF+L)+DI(K,L)
                  ENDDO
               ENDDO
               DEALLOCATE(VS, DB, DI)
               IF (IOPT.EQ.2) THEN
C Chargement des modes dynamiques
                  ALLOCATE(VD(NMDYN,LEN))
                  DO K=1,NMDYN
                     IAD=0
                     DO L=1,LEN/10
                        READ(IFILE,REC=IREC) (VD(K,IAD+M),M=1,10)
                        IREC=IREC+1
                        IAD=IAD+10
                     ENDDO
                     NR=LEN-(LEN/10)*10
                     IF (NR.GT.0) THEN
                        READ(IFILE,REC=IREC) (VD(K,IAD+M),M=1,NR)
                        IREC=IREC+1
                     ENDIF
                  ENDDO
                  ALLOCATE(DB(NSCM,NMDYN),DI(NSCM,NDI+NDM))
                  DO L=1,NMDYN
                     DO K=1,NSCM
                        DB(K,L)=GRAPHE(I)%DGRAPH(ISD)%DM(K,L)
                     ENDDO
                  ENDDO
                  IF (NMDYN.GT.0)
     .               CALL DGEMM('N',  'N', NSCM, NDI+NDM, NMDYN,
     .                          ONE,   DB,  NSCM, VD,      NMDYN, 
     .                          ZERO, DI,  NSCM)
                  DO L=1,NDI+NDM
                     DO K=1,NSCM
                        GRAPHE(I)%DGRAPH(ISD)%D(K,NDF+L)=
     .                     GRAPHE(I)%DGRAPH(ISD)%D(K,NDF+L)+DI(K,L)
                     ENDDO
                  ENDDO
                  DEALLOCATE(VD, DB, DI)
               ENDIF
C
               ISUPEL=ISUPEL+1
               ISTEP=0
               IF (IMACH.NE.3.AND.IPRIB.EQ.1) THEN
                 CALL PROGCOND_C(ISUPEL, GRAPHE(I)%NSUPEL, I, ILVL, ISD,
     .                           ISTEP)
               ELSEIF (IPRIB.EQ.1) THEN
C On fait un affichage interactif de l'avancement des procs si NSPMD
C est inferieur a 16 
C On affiche rien sinon
                  IF (NSPMD.LE.16) 
     .          CALL PROGCONDP_C(ISUPEL, GRAPHE(I)%NSUPEL, ISPMD+1, ISD)
               ENDIF
            ENDDO
            OFFSD=OFFSD+NBSD
         ENDDO
         DEALLOCATE(PARENT)
C
      ENDDO
C
      RETURN
      END
      
