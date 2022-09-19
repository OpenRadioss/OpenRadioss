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
Chd|  DSCDKG                        source/implicit/dsolve/dscdkg.F
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|-- calls ---------------
Chd|        DSEXTR3                       source/implicit/dsolve/dsextr.F
Chd|        DSKGCOND                      source/implicit/dsolve/dscdkg.F
Chd|        DSXKG1                        source/implicit/dsolve/dsextr.F
Chd|        DSXKG2                        source/implicit/dsolve/dsextr.F
Chd|        PROGCONDP_C                   source/system/progcond_c.c    
Chd|        PROGCOND_C                    source/system/progcond_c.c    
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE DSCDKG(GRAPHE, K_DIAG, K_LT, IADK, JDIK,
     .                  NDDL  )
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
#include      "scr05_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IADK(*), JDIK(*), NDDL
      my_real
     .        K_DIAG(*), K_LT(*)
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IPRIM, IPRIB, IFILE, DSLEN, NSLEVEL, NSDEC, OFFSD,
     .        ILVL, NBSD, J, ISD, NDDL_I, NDDL_F, NSDMAX, ISUPEL, ISTEP,
     .        LEN1, NDDL_I2, IC, ISC, NDDL_M, NMDYN, IREC, II, IAD, JJ,
     .        NR, K
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: KL, KLB, KLC, KLI, VD, VS
C
      DO I=1,NSPROC
         IPRIM=GRAPHE(I)%IPRI
         IF (IPRIM<0) THEN
            IPRIM=ABS(IPRIM)-1
            IPRIB=1
         ELSE
            IPRIM=IPRIM-1
            IPRIB=0
         ENDIF
C
         IFILE=GRAPHE(I)%IFILE
         DSLEN=GRAPHE(I)%NSUPEL
         NSLEVEL=GRAPHE(I)%NSLEVEL
         NSDEC=GRAPHE(I)%NSDEC
         OFFSD=DSLEN
         ISUPEL=0
         DO ILVL=1,NSLEVEL
            NBSD=NSDEC**(NSLEVEL-ILVL)
            OFFSD=OFFSD-NBSD
            DO J=1,NBSD
               ISD=OFFSD+J
               NDDL_I=GRAPHE(I)%DGRAPH(ISD)%NDDL_I
               NDDL_F=GRAPHE(I)%DGRAPH(ISD)%NDDL_F
               NSDMAX=GRAPHE(I)%DGRAPH(ISD)%NSDMAX
C
               ISUPEL=ISUPEL+1
               ISTEP=1
               IF (IMACH/=3.AND.IPRIB==1) THEN
                  CALL PROGCOND_C(ISUPEL, DSLEN, I,
     .                            NSLEVEL-ILVL+1, ISD, ISTEP)
               ELSEIF (IPRIB==1) THEN
C On fait un affichage interactif de l'avancement des procs si NSPMD
C est inferieur a 16 
C On affiche rien sinon
                  IF (NSPMD<=16)
     .               CALL PROGCONDP_C(ISUPEL, DSLEN, ISPMD+1, ISD)
               ENDIF
               IF (ILVL==1) THEN
C Condensation au niveau le plus bas : extraction de la rigidite geometrique
                  LEN1=NDDL_I+NDDL_F
                  ALLOCATE(KL(LEN1,LEN1))
                  CALL DSXKG1(K_DIAG, K_LT,         IADK, JDIK, NDDL_I,
     .                        NDDL_F, NDDL,         KL,
     .                        GRAPHE(I)%DGRAPH(ISD)%DDLS,
     .                        GRAPHE(I)%DGRAPH(ISD)%IFAC,
     .                        NSDMAX, NSPROC*DSLEN)
                  NDDL_I2=NDDL_I
               ELSE
C Assemblage de la rigidite a partir des enfants 
C Nombre de ddls internes
                  NDDL_I2=NDDL_I
                  DO IC=1,NSDEC
                     ISC=GRAPHE(I)%DGRAPH(ISD)%CHILD(IC)
                     NDDL_I2=NDDL_I2+GRAPHE(I)%DGRAPH(ISC)%NMDYN
                  ENDDO
C
                  GRAPHE(I)%DGRAPH(ISD)%NDDL_M=NDDL_I2-NDDL_I
                  LEN1=NDDL_I2+NDDL_F
                  NDDL_M=NDDL_I2-NDDL_I
                  ALLOCATE(KL(LEN1,LEN1))
                  CALL DSXKG2(GRAPHE, I,      ISD,  KL, NDDL_F,
     .                        NDDL_I, NDDL_M, NDDL)
               ENDIF
C
               ALLOCATE(KLB(NDDL_F,NDDL_F), KLC(NDDL_F,NDDL_I2),
     .                  KLI(NDDL_I2, NDDL_I2))
               CALL DSEXTR3(KL,     KLB, KLC, KLI, NDDL_F,
     .                      NDDL_I2)
               DEALLOCATE(KL)
C
               ISTEP=3
               IF (IMACH/=3.AND.IPRIB==1)
     .            CALL PROGCOND_C(ISUPEL, DSLEN, I,
     .                            NSLEVEL-ILVL+1, ISD, ISTEP)
C Chargement des modes
               NMDYN=GRAPHE(I)%DGRAPH(ISD)%NMDYN
               ALLOCATE(VS(NDDL_I2,NDDL_F), VD(NDDL_I2,NMDYN))
               IREC=GRAPHE(I)%DGRAPH(ISD)%MDREC
               DO II=1,NDDL_F
                  IAD=0
                  DO JJ=1,NDDL_I2/10
                     READ(IFILE,REC=IREC) (VS(IAD+K,II),K=1,10)
                     IAD=IAD+10
                     IREC=IREC+1
                  ENDDO
                  NR=NDDL_I2-(NDDL_I2/10)*10
                  IF (NR>0) THEN
                     READ(IFILE,REC=IREC) (VS(IAD+K,II),K=1,NR)
                     IREC=IREC+1
                  ENDIF
               ENDDO
               DO II=1,NMDYN
                  IAD=0
                  DO JJ=1,NDDL_I2/10
                     READ(IFILE,REC=IREC) (VD(IAD+K,II),K=1,10)
                     IAD=IAD+10
                     IREC=IREC+1
                  ENDDO
                  NR=NDDL_I2-(NDDL_I2/10)*10
                  IF (NR>0) THEN
                     READ(IFILE,REC=IREC) (VD(IAD+K,II),K=1,NR)
                     IREC=IREC+1
                  ENDIF
               ENDDO
C Condensation de la matrice de rigidite geometrique et
C ecriture sur fichier acces direct a la place de la matrice de masse
               LEN1=(NDDL_F+NMDYN)*(NDDL_F+NMDYN+1)/2
               IREC=GRAPHE(I)%DGRAPH(ISD)%MREC
               CALL DSKGCOND(KLB,   KLC, KLI, NDDL_I2, NDDL_F,
     .                       NMDYN, VS,  VD,  IFILE,   IREC,
     .                       LEN1 )
C
               DEALLOCATE(KLB, KLC, KLI, VS, VD)
            ENDDO
         ENDDO
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  DSKGCOND                      source/implicit/dsolve/dscdkg.F
Chd|-- called by -----------
Chd|        DSCDKG                        source/implicit/dsolve/dscdkg.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DSKGCOND(KLB, KLC, KLI, NDI  , NDF ,
     .                    NVD, VS , VD , IFILE, IREC,
     .                    LEN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDI, NDF, NVD, IFILE, IREC, LEN
      my_real
     .        KLB(NDF,*), KLC(NDF,*), KLI(NDI,*), VS(NDI,*), VD(NDI,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NRT, NLT, I, J, II, IAD, NR
      my_real
     .        KPS(NDF,NDF), KPC(NDF,NVD), KPI(NVD,NVD), KP(LEN)
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: KTMP
C
      NRT=MAX(NDF,NDI)
      NLT=MAX(NDF,NVD)
      ALLOCATE(KTMP(NRT,NLT))
C
      DO I=1,NDF
         DO J=1,NDF
            KPS(I,J)=KLB(I,J)
         ENDDO
      ENDDO
C
      IF (NDI>0) THEN
         IF (NDF>0)
     .   CALL DGEMM('N',  'N',  NDI, NDF, NDI,
     .              ONE,   KLI,  NDI, VS,  NDI,
     .              ZERO, KTMP, NRT)
         IF (NDF>0)
     .   CALL DGEMM('T', 'N', NDF, NDF,  NDI,
     .              ONE,  VS,  NDI, KTMP, NRT,
     .              ONE,  KPS, NDF)
C
         IF (NDF>0)
     .   CALL DGEMM('N',  'N',  NDF, NDF, NDI,
     .              ONE,   KLC,  NDF, VS,  NDI,
     .              ZERO, KTMP, NRT)
         DO I=1,NDF
            DO J=1,NDF
               KPS(I,J)=KPS(I,J)+KTMP(I,J)+KTMP(J,I)
            ENDDO
         ENDDO
C
         IF (NDF>0)
     .   CALL DGEMM('N',   'N', NDF, NVD, NDI,
     .              ONE,    KLC, NDF, VD,  NDI,
     .              ZERO,  KPC, NDF)
C
         CALL DGEMM('N',  'N',  NDI, NVD, NDI,
     .              ONE,   KLI,  NDI, VD,  NDI,
     .              ZERO, KTMP, NRT)
         IF (NDF>0)
     .   CALL DGEMM('T', 'N', NDF, NVD,  NDI,
     .              ONE,  VS,  NDI, KTMP, NRT,
     .              ONE,  KPC, NDF)
         IF (NVD>0)
     .   CALL DGEMM('T',  'N', NVD, NVD,  NDI,
     .              ONE,   VD,  NDI, KTMP, NRT,
     .              ZERO, KPI, NVD)
      ENDIF
C
      II=0
      DO I=1,NDF
         DO J=1,I
            II=II+1
            KP(II)=KPS(J,I)
         ENDDO
      ENDDO
      DO I=1,NVD
         DO J=1,NDF
            II=II+1
            KP(II)=KPC(J,I)
         ENDDO
         DO J=1,I
            II=II+1
            KP(II)=KPI(J,I)
         ENDDO
      ENDDO
C
      IAD=0
      DO I=1,LEN/10
         WRITE(IFILE,REC=IREC) (KP(IAD+J),J=1,10)
         IAD=IAD+10
         IREC=IREC+1
      ENDDO
      NR=LEN-(LEN/10)*10
      IF (NR>0) THEN
         WRITE(IFILE,REC=IREC) (KP(IAD+J),J=1,NR)
         IREC=IREC+1
      ENDIF
C
      DEALLOCATE(KTMP)
C
      RETURN
      END
