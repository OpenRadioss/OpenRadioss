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
Chd|  DSEXTR1                       source/implicit/dsolve/dsextr.F
Chd|-- called by -----------
Chd|        DSCOND                        source/implicit/dsolve/dscond.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DSEXTR1(K_DIAG, K_LT , IADK   , JDIK , NDI   ,
     .                   NDF   , NDDL , KL     , MASS , ML    ,
     .                   DDLS  , IFAC , NSDM   , NBD  , NSCM  ,
     .                   BL    , B    , IOPT   , IFACM, NDDLI7,
     .                   IADI7 , JDII7, I7_DIAG, I7_LT, I7TOK )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
*
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IADK(*), JDIK(*), NDI, NDF, NDDL, NSDM, 
     .        DDLS(*), IFAC(NSDM+1,*), NBD, NSCM, IOPT, IFACM(*),
     .        NDDLI7, IADI7(*), JDII7(*), I7TOK(*)
      my_real
     .        K_DIAG(*), K_LT(*), KL(NDI+NDF,*), MASS(*), ML(*),  
     .        BL(NSCM,*), B(NSCM,*), I7_DIAG(*), I7_LT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ITAG(NDDL), II, J, JJ, FAC, ITAGD(NBD), ND,
     .        FAC2, K, KK, N, IAD
C
      N=NDI+NDF
C
      DO I=1,NDDL
         ITAG(I)=0
      ENDDO
      DO I=1,N
         II=DDLS(I)
         ITAG(II)=I
      ENDDO
C
      DO I=1,N
         DO J=1,N
            KL(I,J)=ZERO
         ENDDO
      ENDDO
C
      DO I=1,NDDL
         II=ITAG(I)
         IF (II.NE.0.AND.II.LE.NDF) THEN
            DO J=1,NBD
               ITAGD(J)=0
            ENDDO
            FAC=IFAC(1,II)
            KL(II,II)=K_DIAG(I)/FAC
            DO J=1,FAC
               JJ=IFAC(J+1,II)
               ITAGD(JJ)=1
            ENDDO
            DO J=IADK(I),IADK(I+1)-1
               JJ=ITAG(JDIK(J))
               IF (JJ.NE.0.AND.JJ.LE.NDF) THEN
                  ND=IFAC(1,JJ)
                  FAC2=0
                  DO K=1,ND
                     KK=IFAC(K+1,JJ)
                     FAC2=FAC2+ITAGD(KK)
                  ENDDO
                  KL(II,JJ)=K_LT(J)/FAC2
                  KL(JJ,II)=K_LT(J)/FAC2
               ELSEIF (JJ.NE.0) THEN
                  KL(II,JJ)=K_LT(J)
                  KL(JJ,II)=K_LT(J)
               ENDIF
            ENDDO
            IF (IOPT.EQ.2) ML(II)=MASS(I)/IFACM(II)
         ELSEIF (II.NE.0) THEN
            KL(II,II)=K_DIAG(I)
            DO J=IADK(I),IADK(I+1)-1
               JJ=ITAG(JDIK(J))
               IF (JJ.NE.0) THEN
                  KL(II,JJ)=K_LT(J)
                  KL(JJ,II)=K_LT(J)
               ENDIF
            ENDDO
            IF (IOPT.EQ.2) ML(II)=MASS(I)
         ENDIF
      ENDDO
C
      IF (IOPT.EQ.2) RETURN
C
      DO I=1,NSCM
         DO J=1,NDI
            JJ=DDLS(NDF+J)
            BL(I,J)=B(I,JJ)
         ENDDO
      ENDDO
      IF (NDDLI7.GT.0) THEN
        DO I=1,NDDLI7
            II=I7TOK(I)
            II=ITAG(II)
            IF (II.NE.0.AND.II.LE.NDF) THEN
               DO J=1,NBD
                  ITAGD(J)=0
               ENDDO
               FAC=IFAC(1,II)
               KL(II,II)=KL(II,II)+I7_DIAG(I)/FAC
               DO J=1,FAC
                  JJ=IFAC(J+1,II)
                  ITAGD(JJ)=1
               ENDDO
               DO J=IADI7(I),IADI7(I+1)-1
                  JJ=I7TOK(JDII7(J))
                  JJ=ITAG(JJ)
                  IF (JJ.NE.0.AND.JJ.LE.NDF) THEN
                     ND=IFAC(1,JJ)
                     FAC2=0
                     DO K=1,ND
                        KK=IFAC(K+1,JJ)
                        FAC2=FAC2+ITAGD(KK)
                     ENDDO
                     KL(II,JJ)=KL(II,JJ)+I7_LT(J)/FAC2
                     KL(JJ,II)=KL(JJ,II)+I7_LT(J)/FAC2
                  ELSEIF (JJ.NE.0) THEN
                     KL(II,JJ)=KL(II,JJ)+I7_LT(J)
                     KL(JJ,II)=KL(JJ,II)+I7_LT(J)
                  ENDIF
               ENDDO
            ELSEIF (II.NE.0) THEN
               KL(II,II)=KL(II,II)+I7_DIAG(I)
               DO J=IADI7(I),IADI7(I+1)-1
                  JJ=I7TOK(JDII7(J))
                  JJ=ITAG(JJ)
                  IF (JJ.NE.0) THEN
                     KL(II,JJ)=KL(II,JJ)+I7_LT(J)
                     KL(JJ,II)=KL(JJ,II)+I7_LT(J)
                  ENDIF
               ENDDO
           ENDIF
         ENDDO   
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  DSEXTR2                       source/implicit/dsolve/dsextr.F
Chd|-- called by -----------
Chd|        DSCOND                        source/implicit/dsolve/dscond.F
Chd|-- calls ---------------
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE DSEXTR2(GRAPHE, IPROC, IDOM, KL  , ML  ,
     .                   NDF   , NDI  , NDM , NDDL, NSCM,
     .                   BL    , B    , IOPT)
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
      TYPE(PRGRAPH) ::  GRAPHE(*)
      INTEGER IPROC, IDOM, NDF, NDI, NDM, NDDL, NSCM, IOPT
      my_real
     .        KL(NDI+NDF+NDM,*), ML(NDF+NDI+NDM,*),
     .        BL(NSCM,*), B(NSCM,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ITAG(NDDL), II, IFILE, NMI, IC, ISC, NDFC, NDMC, LEN,
     .        IREC, IKC, IR, IAD1, J, JJ, NMI0, IMC, NR, N, NN, NSDEC
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: KLC, MLC
C
      N=NDI+NDF+NDM
      NN=NDF+NDI
      DO I=1,N
         DO J=1,N
            KL(I,J)=ZERO
         ENDDO
      ENDDO
      IF (IOPT.EQ.2) THEN
         DO I=1,N
            DO J=1,N
               ML(I,J)=ZERO
            ENDDO
         ENDDO
      ENDIF
C           
      DO I=1,NDDL
         ITAG(I)=0
      ENDDO
      DO I=1,NDI+NDF
         II=GRAPHE(IPROC)%DGRAPH(IDOM)%DDLS(I)
         ITAG(II)=I
      ENDDO
      IF (IOPT.EQ.1) THEN
         DO I=1,NSCM
            DO J=1,NDI
               JJ=GRAPHE(IPROC)%DGRAPH(IDOM)%DDLS(NDF+J)
               BL(I,J)=B(I,JJ)
            ENDDO
         ENDDO
      ENDIF
C
      IFILE=GRAPHE(IPROC)%IFILE
      NSDEC=GRAPHE(IPROC)%NSDEC
      NMI=0
      DO IC=1,NSDEC
         ISC=GRAPHE(IPROC)%DGRAPH(IDOM)%CHILD(IC)
         NDFC=GRAPHE(IPROC)%DGRAPH(ISC)%NDDL_F
         NDMC=GRAPHE(IPROC)%DGRAPH(ISC)%NMDYN
         LEN=(NDFC+NDMC)*(NDFC+NDMC+1)/2
C Chargement de la matrice de rigidite 
         ALLOCATE(KLC(LEN))
         IREC=GRAPHE(IPROC)%DGRAPH(ISC)%KREC
         IKC=0
         DO IR=1,LEN/10
            READ(IFILE,REC=IREC) (KLC(IKC+J),J=1,10)
            IKC=IKC+10
            IREC=IREC+1
         ENDDO
         NR=LEN-(LEN/10)*10
         IF (NR.GT.0) READ(IFILE,REC=IREC) (KLC(IKC+J),J=1,NR)
         DO I=1,NDFC
            II=GRAPHE(IPROC)%DGRAPH(ISC)%DDLS(I)
            II=ITAG(II)
            IAD1=(I-1)*I/2
            DO J=1,I-1
               JJ=GRAPHE(IPROC)%DGRAPH(ISC)%DDLS(J)
               JJ=ITAG(JJ)
               KL(II,JJ)=KL(II,JJ)+KLC(IAD1+J)
               KL(JJ,II)=KL(JJ,II)+KLC(IAD1+J)
            ENDDO
            KL(II,II)=KL(II,II)+KLC(IAD1+I)
         ENDDO
         NMI0=NMI
         DO I=1,NDMC
            NMI=NMI+1
            GRAPHE(IPROC)%DGRAPH(ISC)%CMDYN(I)=NDF+NDI+NMI
            IAD1=(NDFC+I-1)*(NDFC+I)/2
            DO J=1,NDFC
               JJ=GRAPHE(IPROC)%DGRAPH(ISC)%DDLS(J)
               JJ=ITAG(JJ)
               KL(NN+NMI,JJ)=KL(NN+NMI,JJ)+KLC(IAD1+J)
               KL(JJ,NN+NMI)=KL(JJ,NN+NMI)+KLC(IAD1+J)
            ENDDO
            DO J=1,NMI-NMI0-1
               KL(NN+NMI,NN+NMI0+J)=KL(NN+NMI,NN+NMI0+J)
     .                             +KLC(IAD1+NDFC+J)
               KL(NN+NMI0+J,NN+NMI)=KL(NN+NMI0+J,NN+NMI)
     .                             +KLC(IAD1+NDFC+J)
            ENDDO
            KL(NN+NMI,NN+NMI)=KL(NN+NMI,NN+NMI)
     .                       +KLC(IAD1+NDFC+NMI-NMI0)
         ENDDO
         DEALLOCATE(KLC)
C
         IF (IOPT.EQ.1) THEN
C Seconds membres si resolution statique
            DO I=1,NSCM
               DO J=1,NDFC
                  JJ=GRAPHE(IPROC)%DGRAPH(ISC)%DDLS(J)
                  JJ=ITAG(JJ)
                  IF (JJ.GT.NDF) THEN
                     BL(I,JJ-NDF)=BL(I,JJ-NDF)
     .                           +GRAPHE(IPROC)%DGRAPH(ISC)%B(I,J)
                  ELSE
                     GRAPHE(IPROC)%DGRAPH(IDOM)%B(I,JJ)=
     .                          GRAPHE(IPROC)%DGRAPH(IDOM)%B(I,JJ)
     .                           +GRAPHE(IPROC)%DGRAPH(ISC)%B(I,J)
                  ENDIF
               ENDDO
            ENDDO
            CYCLE
         ENDIF     
C Chargement de la matrice de masse
         ALLOCATE(MLC(LEN)) 
         IREC=GRAPHE(IPROC)%DGRAPH(ISC)%MREC
         IMC=0
         DO IR=1,LEN/10
            READ(IFILE,REC=IREC) (MLC(IMC+J),J=1,10)
            IMC=IMC+10
            IREC=IREC+1
         ENDDO
         NR=LEN-(LEN/10)*10
         IF (NR.GT.0) READ(IFILE,REC=IREC) (MLC(IMC+J),J=1,NR)
         DO I=1,NDFC
            II=GRAPHE(IPROC)%DGRAPH(ISC)%DDLS(I)
            II=ITAG(II)
            IAD1=(I-1)*I/2
            DO J=1,I-1
               JJ=GRAPHE(IPROC)%DGRAPH(ISC)%DDLS(J)
               JJ=ITAG(JJ)
               ML(II,JJ)=ML(II,JJ)+MLC(IAD1+J)
               ML(JJ,II)=ML(JJ,II)+MLC(IAD1+J)
            ENDDO
            ML(II,II)=ML(II,II)+MLC(IAD1+I)
         ENDDO
         NMI=NMI0
         DO I=1,NDMC
            NMI=NMI+1
            IAD1=(NDFC+I-1)*(NDFC+I)/2
            DO J=1,NDFC
               JJ=GRAPHE(IPROC)%DGRAPH(ISC)%DDLS(J)
               JJ=ITAG(JJ)
               ML(NN+NMI,JJ)=ML(NN+NMI,JJ)+MLC(IAD1+J)
               ML(JJ,NN+NMI)=ML(JJ,NN+NMI)+MLC(IAD1+J)
            ENDDO
            DO J=1,NMI-NMI0-1
               ML(NN+NMI,NN+NMI0+J)=ML(NN+NMI,NN+NMI0+J)
     .                             +MLC(IAD1+NDFC+J)
               ML(NN+NMI0+J,NN+NMI)=ML(NN+NMI0+J,NN+NMI)
     .                             +MLC(IAD1+NDFC+J)
            ENDDO
            ML(NN+NMI,NN+NMI)=ML(NN+NMI,NN+NMI)
     .                       +MLC(IAD1+NDFC+NMI-NMI0)
         ENDDO
         DEALLOCATE(MLC)
      ENDDO  
C
      RETURN
      END
Chd|====================================================================
Chd|  DSEXTR3                       source/implicit/dsolve/dsextr.F
Chd|-- called by -----------
Chd|        DSCDKG                        source/implicit/dsolve/dscdkg.F
Chd|        DSCOND                        source/implicit/dsolve/dscond.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DSEXTR3(KL , KLB, KLC, KLI, NDF, 
     .                   NDI)
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
      INTEGER NDF, NDI
      my_real
     .        KL(NDF+NDI,*), KLB(NDF,*), KLC(NDF,*), KLI(NDI,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C
      DO I=1,NDF
         DO J=1,NDF
            KLB(I,J)=KL(I,J)
         ENDDO
         DO J=1,NDI
            KLC(I,J)=KL(I,NDF+J)
         ENDDO
      ENDDO
      DO I=1,NDI
         DO J=1,NDI
            KLI(I,J)=KL(NDF+I,NDF+J)
         ENDDO
      ENDDO
C
      RETURN
      END            
Chd|====================================================================
Chd|  DSXKG1                        source/implicit/dsolve/dsextr.F
Chd|-- called by -----------
Chd|        DSCDKG                        source/implicit/dsolve/dscdkg.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DSXKG1(K_DIAG, K_LT, IADK, JDIK, NDI , 
     .                  NDF   , NDDL, KL  , DDLS, IFAC, 
     .                  NSDM  , NBD )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IADK(*), JDIK(*), NDI, NDF, NDDL, DDLS(*), NSDM,
     .        IFAC(NSDM+1,*), NBD
      my_real
     .        K_DIAG(*), K_LT(*), KL(NDI+NDF,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ITAG(NDDL), II, J, JJ, FAC, ITAGD(NBD), ND,
     .        FAC2, K, KK, N, IAD
C
      N=NDI+NDF
C
      DO I=1,NDDL
         ITAG(I)=0
      ENDDO
      DO I=1,N
         II=DDLS(I)
         ITAG(II)=I
      ENDDO
C
      DO I=1,N
         DO J=1,N
            KL(I,J)=ZERO
         ENDDO
      ENDDO
C
      DO I=1,NDDL
         II=ITAG(I)
         IF (II.NE.0.AND.II.LE.NDF) THEN
            DO J=1,NBD
               ITAGD(J)=0
            ENDDO
            FAC=IFAC(1,II)
            KL(II,II)=K_DIAG(I)/FAC
            DO J=1,FAC
               JJ=IFAC(J+1,II)
               ITAGD(JJ)=1
            ENDDO
            DO J=IADK(I),IADK(I+1)-1
               JJ=ITAG(JDIK(J))
               IF (JJ.NE.0.AND.JJ.LE.NDF) THEN
                  ND=IFAC(1,JJ)
                  FAC2=0
                  DO K=1,ND
                     KK=IFAC(K+1,JJ)
                     FAC2=FAC2+ITAGD(KK)
                  ENDDO
                  KL(II,JJ)=K_LT(J)/FAC2
                  KL(JJ,II)=K_LT(J)/FAC2
               ELSEIF (JJ.NE.0) THEN
                  KL(II,JJ)=K_LT(J)
                  KL(JJ,II)=K_LT(J)
               ENDIF
            ENDDO
         ELSEIF (II.NE.0) THEN
            KL(II,II)=K_DIAG(I)
            DO J=IADK(I),IADK(I+1)-1
               JJ=ITAG(JDIK(J))
               IF (JJ.NE.0) THEN
                  KL(II,JJ)=K_LT(J)
                  KL(JJ,II)=K_LT(J)
               ENDIF
            ENDDO
         ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  DSXKG2                        source/implicit/dsolve/dsextr.F
Chd|-- called by -----------
Chd|        DSCDKG                        source/implicit/dsolve/dscdkg.F
Chd|-- calls ---------------
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE DSXKG2(GRAPHE, IPROC, IDOM, KL, NDF, 
     .                  NDI   , NDM  , NDDL)
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
      INTEGER IPROC, IDOM, NDF, NDI, NDM, NDDL
      my_real
     .        KL(NDF+NDI+NDM,*)
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ITAG(NDDL), II, IFILE, NMI, IC, ISC, NDFC, NDMC, LEN,
     .        IREC, IKC, IR, IAD1, J, JJ, NMI0, IMC, NR, N, NN, NSDEC
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: KLC
C
C La matrice de rigidite geometrique est ecrite sur le fichier temporaire
C a la place de la matrice de masse
C
      N=NDI+NDF+NDM
      NN=NDF+NDI
      DO I=1,N
         DO J=1,N
            KL(I,J)=ZERO
         ENDDO
      ENDDO
C           
      DO I=1,NDDL
         ITAG(I)=0
      ENDDO
      DO I=1,NDI+NDF
         II=GRAPHE(IPROC)%DGRAPH(IDOM)%DDLS(I)
         ITAG(II)=I
      ENDDO
C
      IFILE=GRAPHE(IPROC)%IFILE
      NSDEC=GRAPHE(IPROC)%NSDEC
      NMI=0
      DO IC=1,NSDEC
         ISC=GRAPHE(IPROC)%DGRAPH(IDOM)%CHILD(IC)
         NDFC=GRAPHE(IPROC)%DGRAPH(ISC)%NDDL_F
         NDMC=GRAPHE(IPROC)%DGRAPH(ISC)%NMDYN
         LEN=(NDFC+NDMC)*(NDFC+NDMC+1)/2
C Chargement de la matrice de rigidite 
         ALLOCATE(KLC(LEN))
         IREC=GRAPHE(IPROC)%DGRAPH(ISC)%MREC
         IKC=0
         DO IR=1,LEN/10
            READ(IFILE,REC=IREC) (KLC(IKC+J),J=1,10)
            IKC=IKC+10
            IREC=IREC+1
         ENDDO
         NR=LEN-(LEN/10)*10
         IF (NR.GT.0) READ(IFILE,REC=IREC) (KLC(IKC+J),J=1,NR)
         DO I=1,NDFC
            II=GRAPHE(IPROC)%DGRAPH(ISC)%DDLS(I)
            II=ITAG(II)
            IAD1=(I-1)*I/2
            DO J=1,I-1
               JJ=GRAPHE(IPROC)%DGRAPH(ISC)%DDLS(J)
               JJ=ITAG(JJ)
               KL(II,JJ)=KL(II,JJ)+KLC(IAD1+J)
               KL(JJ,II)=KL(JJ,II)+KLC(IAD1+J)
            ENDDO
            KL(II,II)=KL(II,II)+KLC(IAD1+I)
         ENDDO
         NMI0=NMI
         DO I=1,NDMC
            NMI=NMI+1
            GRAPHE(IPROC)%DGRAPH(ISC)%CMDYN(I)=NDF+NDI+NMI
            IAD1=(NDFC+I-1)*(NDFC+I)/2
            DO J=1,NDFC
               JJ=GRAPHE(IPROC)%DGRAPH(ISC)%DDLS(J)
               JJ=ITAG(JJ)
               KL(NN+NMI,JJ)=KL(NN+NMI,JJ)+KLC(IAD1+J)
               KL(JJ,NN+NMI)=KL(JJ,NN+NMI)+KLC(IAD1+J)
            ENDDO
            DO J=1,NMI-NMI0-1
               KL(NN+NMI,NN+NMI0+J)=KL(NN+NMI,NN+NMI0+J)
     .                             +KLC(IAD1+NDFC+J)
               KL(NN+NMI0+J,NN+NMI)=KL(NN+NMI0+J,NN+NMI)
     .                             +KLC(IAD1+NDFC+J)
            ENDDO
            KL(NN+NMI,NN+NMI)=KL(NN+NMI,NN+NMI)
     .                       +KLC(IAD1+NDFC+NMI-NMI0)
         ENDDO
         DEALLOCATE(KLC)
      ENDDO  
C
      RETURN
      END
Cv51e18 ---
