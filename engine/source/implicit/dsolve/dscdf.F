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
Chd|  DSCDF                         source/implicit/dsolve/dscdf.F
Chd|-- called by -----------
Chd|        IMP_DSOLV_ITER                source/implicit/imp_dsolv.F   
Chd|-- calls ---------------
Chd|        DSIFI                         source/implicit/dsolve/dscdf.F
Chd|        DSRDKLI                       source/implicit/dsolve/dscdf.F
Chd|        PROGCONDP_C                   source/system/progcond_c.c    
Chd|        PROGCOND_C                    source/system/progcond_c.c    
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE DSCDF(GRAPHE, NSCM, B, NDDL)
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
#include      "scr05_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSCM, NDDL
      my_real
     .        B(NSCM,*)
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IFILE, DSLEN, OFFSD, ILVL, NBSD, J, ISD, NDI, NDF,
     .        K, L, LL, KK, ITAG(NDDL), IC, ISC, IREC, LEN, IAD, NR,
     .        M, NSLEVEL, NSDEC, IPRIM, IPRIB, ISUPEL, ISTEP
      INTEGER, DIMENSION(:), ALLOCATABLE :: KLIPIV
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: BL, VS, KLI
C
C Attention : cette routine demande que KLI ait ete ecrite sur le fichier
C acces direct. DSCOND doit avoir ete appelee avec IWKLI=1
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
               NDI=GRAPHE(I)%DGRAPH(ISD)%NDDL_I
               NDF=GRAPHE(I)%DGRAPH(ISD)%NDDL_F
C
               ISUPEL=ISUPEL+1
               ISTEP=3
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
C
               DO K=1,NSCM
                  DO L=1,NDF
                     GRAPHE(I)%DGRAPH(ISD)%B(K,L)=ZERO
                  ENDDO
               ENDDO
C 
               ALLOCATE(BL(NSCM,NDI))
C Vecteurs des forces exterieures sur les ddls internes
               DO K=1,NSCM
                  DO L=1,NDI
                     LL=GRAPHE(I)%DGRAPH(ISD)%DDLS(NDF+L)
                     BL(K,L)=B(K,LL)
                  ENDDO
               ENDDO
               IF (ILVL>1) THEN
                  DO K=1,NDDL
                     ITAG(K)=0
                  ENDDO
                  DO K=1,NDI+NDF
                     KK=GRAPHE(I)%DGRAPH(ISD)%DDLS(K)
                     ITAG(KK)=K
                  ENDDO
                  DO IC=1,NSDEC
                     ISC=GRAPHE(I)%DGRAPH(ISD)%CHILD(IC)
                     DO K=1,NSCM
                        DO L=1,GRAPHE(I)%DGRAPH(ISC)%NDDL_F
                           LL=GRAPHE(I)%DGRAPH(ISC)%DDLS(L)
                           LL=ITAG(LL)
                           IF (LL>NDF) THEN
                              BL(K,LL-NDF)=BL(K,LL-NDF)
     .                                    +GRAPHE(I)%DGRAPH(ISC)%B(K,L)
                           ELSE
                              GRAPHE(I)%DGRAPH(ISD)%B(K,LL)=
     .                                    GRAPHE(I)%DGRAPH(ISD)%B(K,LL)
     .                                   +GRAPHE(I)%DGRAPH(ISC)%B(K,L)
                           ENDIF
                        ENDDO
                     ENDDO
                  ENDDO
               ENDIF
C Equilibre des forces externes portant sur les ddls internes
               IREC=GRAPHE(I)%DGRAPH(ISD)%KIREC
               ALLOCATE(KLI(NDI,NDI), KLIPIV(NDI))
               CALL DSRDKLI(KLI, KLIPIV, NDI, IFILE, IREC)
C
               CALL DSIFI(
     .    KLI, KLIPIV, NDI, GRAPHE(I)%DGRAPH(ISD)%IFI, NSCM,
     .    BL )
C
               DEALLOCATE(KLI, KLIPIV)
C Chargement des modes statiques
               IREC=GRAPHE(I)%DGRAPH(ISD)%MDREC
               ALLOCATE(VS(NDI,NDF))
               DO K=1,NDF
                  IAD=0
                  DO L=1,NDI/10
                     READ(IFILE,REC=IREC) (VS(IAD+M,K),M=1,10)
                     IREC=IREC+1
                     IAD=IAD+10
                  ENDDO
                  NR=NDI-(NDI/10)*10
                  IF (NR>0) THEN
                     READ(IFILE,REC=IREC) (VS(IAD+M,K),M=1,NR)
                     IREC=IREC+1
                  ENDIF
               ENDDO
C Projection du vecteur des forces
               IF (NDI>0.AND.NSCM>0)
     .            CALL DGEMM(
     .           'N', 'N',                     NSCM,   NDF, NDI,
     .           ONE,  BL,                      NSCM,   VS,  NDI,
     .           ONE,  GRAPHE(I)%DGRAPH(ISD)%B, NSBMAX)
               DEALLOCATE(VS)
               DEALLOCATE(BL)
            ENDDO
         ENDDO
C
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  DSRDKLI                       source/implicit/dsolve/dscdf.F
Chd|-- called by -----------
Chd|        DSCDF                         source/implicit/dsolve/dscdf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DSRDKLI(KLI, IPIV, NDI, IFILE, IREC)
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
      INTEGER IPIV(*), NDI, IFILE, IREC
      my_real
     .        KLI(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, IAD, I, J, NR
      my_real
     .        RPIV(NDI)
C
      LEN=NDI*NDI
      IAD=0
      DO I=1,LEN/10
         READ(IFILE,REC=IREC) (KLI(IAD+J),J=1,10)
         IAD=IAD+10
         IREC=IREC+1
      ENDDO
      NR=LEN-(LEN/10)*10
      IF (NR>0) THEN
         READ(IFILE,REC=IREC) (KLI(IAD+J),J=1,NR)
         IREC=IREC+1
      ENDIF
C
      LEN=NDI
      IAD=0
      DO I=1,LEN/10
         READ(IFILE,REC=IREC) (RPIV(IAD+J),J=1,10)
         IAD=IAD+10
         IREC=IREC+1
      ENDDO
      NR=LEN-(LEN/10)*10
      IF (NR>0) THEN
         READ(IFILE,REC=IREC) (RPIV(IAD+J),J=1,NR)
         IREC=IREC+1
      ENDIF
      DO I=1,NDI
         IPIV(I)=NINT(RPIV(I))
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  DSIFI                         source/implicit/dsolve/dscdf.F
Chd|-- called by -----------
Chd|        DSCDF                         source/implicit/dsolve/dscdf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DSIFI(KLI, IPIV, NDI, IFI, NSCM,
     .                 S  )
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
      INTEGER IPIV(*), NDI, NSCM
      my_real
     .        KLI(NDI,*), IFI(NSBMAX,*), S(NSCM,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NRHS, INFRES
      my_real
     .        F(NDI,NSCM)
C
      DO I=1,NDI
         DO J=1,NSCM
            F(I,J)=S(J,I)
         ENDDO
      ENDDO
      NRHS=NSCM
      IF (NDI>0)
     .   CALL DSYTRS('U', NDI, NRHS, KLI, NDI, IPIV, F, NDI, INFRES)
      DO I=1,NDI
         DO J=1,NSCM
            IFI(J,I)=F(I,J)
         ENDDO
      ENDDO
C
      RETURN
      END

