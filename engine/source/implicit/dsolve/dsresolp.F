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
Cf51e14 Nouvelle routine
Chd|====================================================================
Chd|  DSRESOLP                      source/implicit/dsolve/dsresolp.F
Chd|-- called by -----------
Chd|        IMP_DSOLV                     source/implicit/imp_dsolv.F   
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        DSEIGP                        source/implicit/dsolve/dseigp.F
Chd|        DSMSHIFTP                     source/implicit/dsolve/dsresolp.F
Chd|        SPMD_DS_IEXCH                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_DS_MEXCH                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_DS_VDESC                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_DS_VEXCH                 source/mpi/implicit/spmd_dsreso.F
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE DSRESOLP(PGRAPH, IOPT  , NSCM  , NDDLC , B     ,
     .                    NM    , MAXITR, TOL   , CUTFRQ, NBLF  ,
     .                    IPRI  , NVD   , FREQ2 , NVAL  , SHIFTI,
     .                    TITRE , LTITR1 , IPMESS)
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
      INTEGER IOPT, NSCM, NDDLC, NM, MAXITR, NBLF, IPRI, NVD, NVAL,
     .        LTITR1, IPMESS
      my_real
     .        B(NSCM,*), TOL, CUTFRQ, FREQ2(*), SHIFTI
      CHARACTER TITRE*(*)
      TYPE(PRGRAPH) :: PGRAPH
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, LENBUF(2,NSPMD), NDF, NDM, IR, IC, IPROC, IFILE, LEN,
     .        IREC, IKC, NR, LENM, LENV, LENMR, LENVR, IADM(NSPMD+1),
     .        IADV(NSPMD+1), IADMC(NSPMD), IADVC(NSPMD), 
     .        IADMR(NSPMD+1), IADVR(NSPMD+1), IAD1, J, NNRP, NNCP,
     .        JJ, IMC, ICTXT, DESC_K(9), DESC_S(9), INFFAC, NRHS,
     .        INFRES, NDEPL, MAXNCV, ISTOP, IMDIAG, NIB, DESC_M(9),
     .        NBLF0, NVD0, INOPRI, NEV, NCV, N, MAXN, MAXNEV, LDV, 
     .        ISHFTS, MODE, INFO, NUV, IDBL, IDF, LENBUFR(2,NSPMD),
     .        II
      my_real
     .        SHIFT, TSHIFT, TSHIFT0, PULS, FRQ, FR1, FR2
      CHARACTER WHICH*2
      INTEGER, DIMENSION(:), ALLOCATABLE :: IVEXCH
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IMEXCH
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: KL, KEXCH, ML, MEXCH, S
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: RIGI, FEXCH, F, MASS,
     .                                        VECT, V, RIGIS, FREQ, VV
C
      INTEGER MSGOFF, MSGOFF2, MSGOFF3, MSGOFF4, MSGOFF5
      DATA MSGOFF  /200000/ 
      DATA MSGOFF2 /300000/
      DATA MSGOFF3 /400000/
      DATA MSGOFF4 /500000/
      DATA MSGOFF5 /600000/

#ifdef MPI
C
      IF (NDDLC.EQ.0) RETURN
C
      DO I=1,NSPMD
         LENBUF(1,I)=0
         LENBUF(2,I)=0
      ENDDO
      NDF=PGRAPH%DGRAPH(1)%NDDL_F
      NDM=PGRAPH%DGRAPH(1)%NMDYN
      DO I=1,NDF+NDM
         IR=PGRAPH%IPROW(1,I)
         DO J=1,NDF+NDM
            IC=PGRAPH%IPCOL(1,J)
            IPROC=DSNCOL*(IR-1)+IC
            LENBUF(1,IPROC)=LENBUF(1,IPROC)+1
         ENDDO
         IPROC=DSNCOL*(IR-1)+1
         LENBUF(2,IPROC)=LENBUF(2,IPROC)+1
      ENDDO
C      
      CALL SPMD_DS_IEXCH(LENBUF, LENBUFR, 2, MSGOFF)
C
      IF (ISPMD.EQ.0.AND.IPMESS.NE.0) THEN
         WRITE(ISTDO,*)
         WRITE(ISTDO,'(A)') TITRE(1:LTITR1)
         WRITE(IOUT,'(A)') TITRE(1:LTITR1)
      ENDIF
C
      IFILE=PGRAPH%IFILE
      LEN=(NDF+NDM)*(NDF+NDM+1)/2
C Chargement de la matrice de rigidite
      ALLOCATE(KL(LEN))
      IREC=PGRAPH%DGRAPH(1)%KREC
      IKC=0
      DO IR=1,LEN/10
         READ(IFILE,REC=IREC) (KL(IKC+J),J=1,10)
         IKC=IKC+10
         IREC=IREC+1
      ENDDO
      NR=LEN-(LEN/10)*10
      IF (NR.GT.0) READ(IFILE,REC=IREC) (KL(IKC+J),J=1,NR)
C
      LENM=0
      LENV=0
      LENMR=0
      LENVR=0
      DO I=1,NSPMD
         IADM(I)=LENM+1
         LENM=LENM+LENBUF(1,I)
         IADV(I)=LENV+1
         LENV=LENV+LENBUF(2,I)
         IADMC(I)=IADM(I)
         IADVC(I)=IADV(I)
         IADMR(I)=LENMR+1
         LENMR=LENMR+LENBUFR(1,I)
         IADVR(I)=LENVR+1
         LENVR=LENVR+LENBUFR(2,I)
      ENDDO
      IADM(NSPMD+1)=LENM+1
      IADV(NSPMD+1)=LENV+1
      IADMR(NSPMD+1)=LENMR+1
      IADVR(NSPMD+1)=LENVR+1
      ALLOCATE(KEXCH(LENM), IMEXCH(LENM,2))
      DO I=1,NDF+NDM
         IAD1=(I-1)*I/2
         DO J=1,I-1
C Terme triangle inferieur
            IR=PGRAPH%IPROW(1,I)
            IC=PGRAPH%IPCOL(1,J)
            IPROC=(IR-1)*DSNCOL+IC
            IMEXCH(IADMC(IPROC),1)=PGRAPH%IPROW(2,I)
            IMEXCH(IADMC(IPROC),2)=PGRAPH%IPCOL(2,J)
            KEXCH(IADMC(IPROC))=KL(IAD1+J)
            IADMC(IPROC)=IADMC(IPROC)+1
C Terme symetrique triangle superieur
            IR=PGRAPH%IPROW(1,J)
            IC=PGRAPH%IPCOL(1,I)
            IPROC=(IR-1)*DSNCOL+IC
            IMEXCH(IADMC(IPROC),1)=PGRAPH%IPROW(2,J)
            IMEXCH(IADMC(IPROC),2)=PGRAPH%IPCOL(2,I)
            KEXCH(IADMC(IPROC))=KL(IAD1+J)
            IADMC(IPROC)=IADMC(IPROC)+1
         ENDDO
         IR=PGRAPH%IPROW(1,I)
         IC=PGRAPH%IPCOL(1,I)
         IPROC=(IR-1)*DSNCOL+IC
         IMEXCH(IADMC(IPROC),1)=PGRAPH%IPROW(2,I)
         IMEXCH(IADMC(IPROC),2)=PGRAPH%IPCOL(2,I)
         KEXCH(IADMC(IPROC))=KL(IAD1+I)
         IADMC(IPROC)=IADMC(IPROC)+1
      ENDDO
      DEALLOCATE(KL)
C
      NNRP=PGRAPH%NNRP
      NNCP=PGRAPH%NNCP
      ALLOCATE(RIGI(NNRP,NNCP))
      DO I=1,NNRP
         DO J=1,NNCP
            RIGI(I,J)=ZERO
         ENDDO
      ENDDO
      CALL SPMD_DS_MEXCH(RIGI, NNRP,  IMEXCH, KEXCH, MSGOFF2,
     .                   IADM, IADMR, LENM  ) 
      DEALLOCATE(KEXCH)      
C
      IF (IOPT.EQ.1) THEN
         DEALLOCATE(IMEXCH)
         ALLOCATE(FEXCH(LENV,NSCM), IVEXCH(LENV))
         DO I=1,NSCM
            DO J=1,NSPMD
               IADVC(J)=IADV(J)
            ENDDO
C
            DO J=1,NDF
               JJ=PGRAPH%DGRAPH(1)%DDLS(J)
               IR=PGRAPH%IPROW(1,J)
               IPROC=(IR-1)*DSNCOL+1
               IVEXCH(IADVC(IPROC))=PGRAPH%IPROW(2,J)
               FEXCH(IADVC(IPROC),I)=B(I,JJ)+PGRAPH%DGRAPH(1)%B(I,J)
               IADVC(IPROC)=IADVC(IPROC)+1
            ENDDO
         ENDDO
C
         ALLOCATE(F(NNRP,NSCM))
         DO I=1,NSCM
            DO J=1,NNRP
               F(J,I)=ZERO
            ENDDO
         ENDDO
         CALL SPMD_DS_VEXCH(F,    NNRP,  IVEXCH, FEXCH, MSGOFF3,
     .                      IADV, IADVR, LENV  , NSCM )
         DEALLOCATE(FEXCH, IVEXCH)
      ELSEIF (IOPT.EQ.2) THEN
C Chargement de la matrice de masse
         ALLOCATE(ML(LEN))
         IREC=PGRAPH%DGRAPH(1)%MREC
         IMC=0
         DO IR=1,LEN/10
            READ(IFILE,REC=IREC) (ML(IMC+J),J=1,10)
            IMC=IMC+10
            IREC=IREC+1
         ENDDO
         NR=LEN-(LEN/10)*10
         IF (NR.GT.0) READ(IFILE,REC=IREC) (ML(IMC+J),J=1,NR)
C
         ALLOCATE(MEXCH(LENM))
         DO I=1,NSPMD
            IADMC(I)=IADM(I)
         ENDDO
         DO I=1,NDF+NDM
            IAD1=(I-1)*I/2
            DO J=1,I-1
C Terme triangle inferieur
               IR=PGRAPH%IPROW(1,I)
               IC=PGRAPH%IPCOL(1,J)
               IPROC=(IR-1)*DSNCOL+IC
               MEXCH(IADMC(IPROC))=ML(IAD1+J)
               IADMC(IPROC)=IADMC(IPROC)+1
C Terme symetrique triangle superieur
               IR=PGRAPH%IPROW(1,J)
               IC=PGRAPH%IPCOL(1,I)
               IPROC=(IR-1)*DSNCOL+IC
               MEXCH(IADMC(IPROC))=ML(IAD1+J)
               IADMC(IPROC)=IADMC(IPROC)+1
            ENDDO
            IR=PGRAPH%IPROW(1,I)
            IC=PGRAPH%IPCOL(1,I)
            IPROC=(IR-1)*DSNCOL+IC
            MEXCH(IADMC(IPROC))=ML(IAD1+I)
            IADMC(IPROC)=IADMC(IPROC)+1
         ENDDO
         DEALLOCATE(ML)
C
         ALLOCATE(MASS(NNRP,NNCP))
         DO I=1,NNRP
            DO J=1,NNCP
               MASS(I,J)=ZERO
            ENDDO
         ENDDO
         CALL SPMD_DS_MEXCH(MASS, NNRP,  IMEXCH, MEXCH, MSGOFF4,
     .                      IADM, IADMR, LENM  ) 
         DEALLOCATE(MEXCH, IMEXCH)
      ENDIF
C Initialisation de la process grid
      CALL SL_INIT(ICTXT, DSNROW, DSNCOL)
C Descripteur pour RIGI
      DESC_K(1)=1
      DESC_K(2)=ICTXT
      DESC_K(3)=NDDLC
      DESC_K(4)=NDDLC
      DESC_K(5)=DSNBLOC
      DESC_K(6)=DSNBLOC
      DESC_K(7)=0
      DESC_K(8)=0
      DESC_K(9)=MAX(1,NNRP)
C
      IF (IOPT.EQ.1) THEN
C Resolution systeme lineaire pour NSCM seconds membres
         ALLOCATE(V(NNRP,NSCM), S(NNRP))
C Descripteur pour S
         DESC_S(1)=1
         DESC_S(2)=ICTXT
         DESC_S(3)=NDDLC
         DESC_S(4)=1
         DESC_S(5)=DSNBLOC
         DESC_S(6)=DSNBLOC
         DESC_S(7)=0
         DESC_S(8)=0
         DESC_S(9)=MAX(1,NNRP)
C Factorisation et resolution
         CALL PDPOTRF('U',    NDDLC,  RIGI, 1, 1,
     .                DESC_K, INFFAC)
C
         DO I=1,NSCM
            DO J=1,NNRP
               S(J)=F(J,I)
            ENDDO
            NRHS=1
            CALL PDPOTRS('U',    NDDLC,  NRHS, RIGI, 1,
     .                   1,      DESC_K, S,    1,    1,
     .                   DESC_S, INFRES)
            DO J=1,NNRP
               V(J,I)=S(J)
            ENDDO
         ENDDO
         DEALLOCATE(S)
         NDEPL=NSCM
      ELSEIF (IOPT.EQ.2) THEN
C Resolution probleme aux valeurs propres
C Descripteur pour MASS
         DESC_M(1)=1
         DESC_M(2)=ICTXT
         DESC_M(3)=NDDLC
         DESC_M(4)=NDDLC
         DESC_M(5)=DSNBLOC
         DESC_M(6)=DSNBLOC
         DESC_M(7)=0
         DESC_M(8)=0
         DESC_M(9)=MAX(1,NNRP)
C
         N=NDDLC
         MAXNCV=NM*NSVMAXT
         ALLOCATE(VECT(NNRP,MAXNCV), V(NNRP,NSVMAXT), RIGIS(NNRP,N),
     .            FREQ(MAXNCV,2))
         FREQ = 0
         VECT = 0
         SHIFT=SHIFTI
         TSHIFT=SHIFT
         IF (ISPMD.EQ.0)
     .      WRITE(IOUT,FMT='(A15,1PG11.4)') ' INITIAL SHIFT ',SHIFT
         ISTOP=0
         NVD=0
         IMDIAG=0
C
         NIB=0
         NBLF=MIN(NBLF,NSVMAXT)
         NBLF0=NBLF
         NBLF=MIN(NBLF0,N-2)
C
         DO WHILE (ISTOP.EQ.0)
            NVD0=NVD
            INOPRI=0
            TSHIFT0=TSHIFT
            CALL DSMSHIFTP(RIGIS, RIGI, MASS, TSHIFT, NNRP,
     .                     N    )
            NEV=MIN(NBLF+1,NVAL-NVD)
            NCV=MIN(NM*NEV,N)
            MAXN=N
            MAXNEV=NEV
            LDV=NNRP
            WHICH='LA'
            ISHFTS=1
            MODE=3
            INFO=0
C
#ifdef WITHOUT_LINALG
           WRITE(6,*) "Error: compiled with WITHOUT_LINALG"
           CALL FLUSH(6)
           CALL ARRET(5)
#else
            CALL DSEIGP(N,       NEV,    NCV,    WHICH,  INFO,
     .                  MAXN,    MAXNEV, MAXNCV, LDV,    ISHFTS,
     .                  MAXITR,  MODE,   TOL,    RIGIS,  MASS, 
     .                  FREQ,    VECT,   IPRI,   TSHIFT, DESC_K,
     .                  DESC_M,  NNRP,   ICTXT,  DSNROW, DSNCOL,
     .                  DSNBLOC)
#endif
C
            NUV=NEV-1
            IF (NEV.EQ.NVAL-NVD) NUV=NEV
C
            DO I=1,NUV
               PULS=MAX(ZERO,FREQ(I,1))
               FRQ=SQRT(PULS)/DEUX/PI
               IF (FRQ.LE.CUTFRQ.AND.NVD.LT.NSVMAXT.
     .             AND.NVD.LT.NVAL) THEN
                  NVD=NVD+1
                  FREQ2(NVD)=FRQ
                  DO II=1,NNRP
                     V(II,NVD)=VECT(II,I)
                  ENDDO
                  IF (NVD.EQ.NSVMAXT) THEN
                     ISTOP=1
                  ENDIF
               ELSE
                  ISTOP=1
               ENDIF
            ENDDO
C
            IF (NVD.EQ.NVAL) ISTOP=1
C
            IF (ISTOP.EQ.0) THEN
               FR1=FREQ(NEV,1)
               IDBL=0
               IDF=2
               DO WHILE (IDBL.EQ.0.AND.IDF.LE.NEV)
                  FR2=FREQ(NEV-IDF+1,1)
                  IF (ABS((FR2-FR1)/FR1).LE.EM01) THEN
                     IDF=IDF+1
                     NVD=NVD-1
                  ELSE
                     IDBL=1
                  ENDIF
               ENDDO
               IF (IDF.EQ.NEV+1) THEN
                  NBLF=2*NBLF
                  SHIFT=ZERO
                  INOPRI=1
               ELSE
                  NBLF=MIN(NBLF0,N-2)
                  FR1=FREQ(NEV-IDF+2,1)
                  SHIFT=FR2+(FR1-FR2)*ZEP9-TSHIFT
               ENDIF
               TSHIFT=TSHIFT+SHIFT
            ENDIF
C
            IF (INOPRI.EQ.0) THEN
               NIB=NIB+1
               IF (ISPMD.EQ.0) THEN
                  WRITE(IOUT,*)
                  WRITE(IOUT,FMT='(A7,I10,A7,1PG11.4,A23,I10)')
     .                     ' BLOCK ',NIB,' SHIFT ',TSHIFT0,
     .                     ' NUMBER OF FREQUENCIES ',(NVD-NVD0)
                  WRITE(IOUT,FMT='(A13)') ' FREQUENCIES:'
                  WRITE(IOUT,FMT='(A39)')
     . '         NUMBER  EIGENVALUE   FREQUENCY'
                  DO I=NVD0+1,NVD
                     WRITE(IOUT,FMT='(5X,I10,1X,1PG11.4,1X,1PG11.4)')
     .                        I,(DEUX*PI*FREQ2(I))**2,FREQ2(I)
                  ENDDO
               ENDIF
            ENDIF
C
         ENDDO
         DEALLOCATE(VECT, RIGIS, FREQ)
         NDEPL=NVD
         IF (ISPMD.EQ.0) WRITE(IOUT,*)
      ENDIF
C
C Redescente des deplacements
C
      ALLOCATE(VV(NDDLC,NDEPL))
      CALL SPMD_DS_VDESC(V,       NDEPL, VV, NDDLC, NNRP,
     .                   MSGOFF5) 
C
      DO I=1,NDF
         II=PGRAPH%IPROW(3,I)
         DO J=1,NDEPL
            PGRAPH%DGRAPH(1)%D(J,I)=VV(II,J)
         ENDDO
      ENDDO
      DO I=1,NDM
         II=PGRAPH%IPROW(3,NDF+I)
         DO J=1,NDEPL
            PGRAPH%DGRAPH(1)%DM(J,I)=VV(II,J)
         ENDDO
      ENDDO
C
      DEALLOCATE(V, VV)
      IF (IOPT.EQ.1) DEALLOCATE(F)
      IF (IOPT.EQ.2) DEALLOCATE(MASS)
      DEALLOCATE(RIGI)
      CALL BLACS_GRIDEXIT(ICTXT)
C
#endif
      RETURN
      END
Chd|====================================================================
Chd|  DSRESOLP_ITER                 source/implicit/dsolve/dsresolp.F
Chd|-- called by -----------
Chd|        IMP_DSOLV_ITER                source/implicit/imp_dsolv.F   
Chd|-- calls ---------------
Chd|        SPMD_DS_IEXCH                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_DS_MEXCH                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_DS_VDESC                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_DS_VEXCH                 source/mpi/implicit/spmd_dsreso.F
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE DSRESOLP_ITER(PGRAPH, IT  , NSCM,  NDDLC, B     ,
     .                         RIGI  , NNRP, TITRE, LTITR1, IPMESS,
     .                         IPIV  , IGEN)
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
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IT, NSCM, NDDLC, NNRP, LTITR1, IPMESS, IPIV(*), IGEN
      my_real
     .        B(NSCM,*), RIGI(NNRP,*)
      CHARACTER TITRE*(*)
      TYPE(PRGRAPH) :: PGRAPH
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, LENBUF(2,NSPMD), NDF, NDM, IR, IC, IPROC, IFILE, LEN,
     .        IREC, IKC, NR, LENM, LENV, LENMR, LENVR, IADM(NSPMD+1),
     .        IADV(NSPMD+1), IADMC(NSPMD), IADVC(NSPMD),
     .        IADMR(NSPMD+1), IADVR(NSPMD+1), IAD1, J, NNCP, JJ,
     .        ICTXT, LENBUFR(2,NSPMD), NDEPL, II, DESC_K(9),
     .        DESC_S(9), INFFAC, INFRES, NRHS
      my_real
     .        F(NNRP,NSCM), V(NNRP,NSCM), S(NNRP), VV(NDDLC, NSCM)
      INTEGER, DIMENSION(:), ALLOCATABLE :: IVEXCH
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IMEXCH
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: KL, KEXCH
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: FEXCH
C
      INTEGER MSGOFF, MSGOFF2, MSGOFF3, MSGOFF4, MSGOFF5
      DATA MSGOFF  /200000/ 
      DATA MSGOFF2 /300000/
      DATA MSGOFF3 /400000/
      DATA MSGOFF4 /500000/
      DATA MSGOFF5 /600000/

#ifdef MPI
C
      IF (NDDLC.EQ.0) RETURN
C
      DO I=1,NSPMD
         LENBUF(1,I)=0
         LENBUF(2,I)=0
      ENDDO
      NDF=PGRAPH%DGRAPH(1)%NDDL_F
      DO I=1,NDF
         IR=PGRAPH%IPROW(1,I)
         DO J=1,NDF
            IC=PGRAPH%IPCOL(1,J)
            IPROC=DSNCOL*(IR-1)+IC
            LENBUF(1,IPROC)=LENBUF(1,IPROC)+1
         ENDDO
         IPROC=DSNCOL*(IR-1)+1
         LENBUF(2,IPROC)=LENBUF(2,IPROC)+1
      ENDDO
      
      CALL SPMD_DS_IEXCH(LENBUF, LENBUFR, 2, MSGOFF)
C
      IF (ISPMD.EQ.0.AND.IPMESS.NE.0) THEN
         WRITE(IOUT,'(A)') TITRE(1:LTITR1)
         WRITE(ISTDO,*)
         WRITE(ISTDO,'(A)') TITRE(1:LTITR1)
      ENDIF
C
      LENM=0
      LENV=0
      LENMR=0
      LENVR=0
      DO I=1,NSPMD
         IADM(I)=LENM+1
         LENM=LENM+LENBUF(1,I)
         IADV(I)=LENV+1
         LENV=LENV+LENBUF(2,I)
         IADMC(I)=IADM(I)
         IADVC(I)=IADV(I)
         IADMR(I)=LENMR+1
         LENMR=LENMR+LENBUFR(1,I)
         IADVR(I)=LENVR+1
         LENVR=LENVR+LENBUFR(2,I)
      ENDDO
      IADM(NSPMD+1)=LENM+1
      IADV(NSPMD+1)=LENV+1
      IADMR(NSPMD+1)=LENMR+1
      IADVR(NSPMD+1)=LENVR+1
C
      IFILE=PGRAPH%IFILE
      LEN=NDF*(NDF+1)/2
      IF (IDSC.EQ.1) THEN
C Chargement de la matrice de rigidite
         ALLOCATE(KL(LEN))
         IREC=PGRAPH%DGRAPH(1)%KREC
         IKC=0
         DO IR=1,LEN/10
            READ(IFILE,REC=IREC) (KL(IKC+J),J=1,10)
            IKC=IKC+10
            IREC=IREC+1
         ENDDO
         NR=LEN-(LEN/10)*10
         IF (NR.GT.0) READ(IFILE,REC=IREC) (KL(IKC+J),J=1,NR)
C
         ALLOCATE(KEXCH(LENM), IMEXCH(LENM,2))
         DO I=1,NDF
            IAD1=(I-1)*I/2
            DO J=1,I-1
C Terme triangle inferieur
               IR=PGRAPH%IPROW(1,I)
               IC=PGRAPH%IPCOL(1,J)
               IPROC=(IR-1)*DSNCOL+IC
               IMEXCH(IADMC(IPROC),1)=PGRAPH%IPROW(2,I)
               IMEXCH(IADMC(IPROC),2)=PGRAPH%IPCOL(2,J)
               KEXCH(IADMC(IPROC))=KL(IAD1+J)
               IADMC(IPROC)=IADMC(IPROC)+1
C Terme symetrique triangle superieur
               IR=PGRAPH%IPROW(1,J)
               IC=PGRAPH%IPCOL(1,I)
               IPROC=(IR-1)*DSNCOL+IC
               IMEXCH(IADMC(IPROC),1)=PGRAPH%IPROW(2,J)
               IMEXCH(IADMC(IPROC),2)=PGRAPH%IPCOL(2,I)
               KEXCH(IADMC(IPROC))=KL(IAD1+J)
               IADMC(IPROC)=IADMC(IPROC)+1
            ENDDO
            IR=PGRAPH%IPROW(1,I)
            IC=PGRAPH%IPCOL(1,I)
            IPROC=(IR-1)*DSNCOL+IC
            IMEXCH(IADMC(IPROC),1)=PGRAPH%IPROW(2,I)
            IMEXCH(IADMC(IPROC),2)=PGRAPH%IPCOL(2,I)
            KEXCH(IADMC(IPROC))=KL(IAD1+I)
            IADMC(IPROC)=IADMC(IPROC)+1
         ENDDO
         DEALLOCATE(KL)
C
         NNCP=PGRAPH%NNCP
         DO I=1,NNRP
            DO J=1,NNCP
               RIGI(I,J)=ZERO
            ENDDO
         ENDDO
         CALL SPMD_DS_MEXCH(RIGI, NNRP,  IMEXCH, KEXCH, MSGOFF2,
     .                      IADM, IADMR, LENM  ) 
         DEALLOCATE(KEXCH, IMEXCH)
      ENDIF
C
      ALLOCATE(FEXCH(LENV,NSCM), IVEXCH(LENV))
      DO I=1,NSCM
         DO J=1,NSPMD
            IADVC(J)=IADV(J)
         ENDDO
C
         DO J=1,NDF
            JJ=PGRAPH%DGRAPH(1)%DDLS(J)
            IR=PGRAPH%IPROW(1,J)
            IPROC=(IR-1)*DSNCOL+1
            IVEXCH(IADVC(IPROC))=PGRAPH%IPROW(2,J)
            FEXCH(IADVC(IPROC),I)=B(I,JJ)+PGRAPH%DGRAPH(1)%B(I,J)
            IADVC(IPROC)=IADVC(IPROC)+1
         ENDDO
      ENDDO
C
      DO I=1,NSCM
         DO J=1,NNRP
            F(J,I)=ZERO
         ENDDO
      ENDDO
      CALL SPMD_DS_VEXCH(F,    NNRP,  IVEXCH, FEXCH, MSGOFF3,
     .                   IADV, IADVR, LENV  , NSCM )
      DEALLOCATE(FEXCH, IVEXCH)
C Initialisation de la process grid
      CALL SL_INIT(ICTXT, DSNROW, DSNCOL)
C
C Resolution systeme lineaire pour NSCM seconds membres
C Descripteur pour RIGI
      DESC_K(1)=1
      DESC_K(2)=ICTXT
      DESC_K(3)=NDDLC
      DESC_K(4)=NDDLC
      DESC_K(5)=DSNBLOC
      DESC_K(6)=DSNBLOC
      DESC_K(7)=0
      DESC_K(8)=0
      DESC_K(9)=MAX(1,NNRP)
C Descripteur pour S
      DESC_S(1)=1
      DESC_S(2)=ICTXT
      DESC_S(3)=NDDLC
      DESC_S(4)=1
      DESC_S(5)=DSNBLOC
      DESC_S(6)=DSNBLOC
      DESC_S(7)=0
      DESC_S(8)=0
      DESC_S(9)=MAX(1,NNRP)
C Factorisation et resolution
      IF (IDSC.EQ.1) THEN
         IF (IGEN.EQ.1) THEN
            CALL PDPOTRF('U',    NDDLC,  RIGI, 1, 1,
     .                   DESC_K, INFFAC)
         ELSEIF (IGEN.EQ.2) THEN
            CALL PDGETRF(NDDLC,   NDDLC, RIGI,   1, 1,
     .                   DESC_K,  IPIV,  INFFAC)
         ENDIF
      ENDIF
C
      DO I=1,NSCM
         DO J=1,NNRP
            S(J)=F(J,I)
         ENDDO
         NRHS=1
         IF (IGEN.EQ.1) THEN
            CALL PDPOTRS('U',    NDDLC,  NRHS, RIGI, 1,
     .                   1,      DESC_K, S,    1,    1,
     .                   DESC_S, INFRES)
         ELSEIF (IGEN.EQ.2) THEN
            CALL PDGETRS('N', NDDLC,   1,      RIGI, 1,
     .                   1,   DESC_K,  IPIV,   S,    1,
     .                   1,   DESC_S,  INFRES)
         ENDIF
         DO J=1,NNRP
            V(J,I)=S(J)
         ENDDO
      ENDDO
      NDEPL=NSCM
C
C Redescente des deplacements
C
      CALL SPMD_DS_VDESC(V,       NDEPL, VV, NDDLC, NNRP,
     .                   MSGOFF5) 
C
      DO I=1,NDF
         II=PGRAPH%IPROW(3,I)
         DO J=1,NDEPL
            PGRAPH%DGRAPH(1)%D(J,I)=VV(II,J)
         ENDDO
      ENDDO
C
      CALL BLACS_GRIDEXIT(ICTXT)
C
#endif
      RETURN
      END
Chd|====================================================================
Chd|  DSMSHIFTP                     source/implicit/dsolve/dsresolp.F
Chd|-- called by -----------
Chd|        DSRESOLP                      source/implicit/dsolve/dsresolp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DSMSHIFTP(KLS, KL , ML, SHIFT, NLOC,
     .                     N  )
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
      INTEGER NLOC, N
      my_real
     .        KLS(NLOC,*), KL(NLOC,*), ML(NLOC,*), SHIFT      
     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C
      DO I=1,N
         DO J=1,NLOC
            KLS(J,I)=KL(J,I)-SHIFT*ML(J,I)
         ENDDO
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  DSRESOLP_BUCK                 source/implicit/dsolve/dsresolp.F
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        DSBUCKP                       source/implicit/dsolve/dsbuckp.F
Chd|        SPMD_DS_IEXCH                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_DS_MEXCH                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_DS_VDESC                 source/mpi/implicit/spmd_dsreso.F
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE DSRESOLP_BUCK(PGRAPH, NDDLC, EIG, TITRE, LTITR1,
     .                         IPMESS)
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
#include      "com06_c.inc"
#include      "task_c.inc"
#include      "buckcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDLC, LTITR1, IPMESS
      my_real
     .        EIG(NBUCK,*)
      CHARACTER TITRE*(*)
      TYPE(PRGRAPH) :: PGRAPH
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, LENBUF(2,NSPMD), NDF, NDM, IR, J, IC, IPROC, 
     .        IFILE, LEN, IREC, IKC, NR, LENM, LENV, LENMR,LENVR,
     .        IADM(NSPMD+1), IADMC(NSPMD), IADMR(NSPMD+1),
     .        IAD1, NNRP, NNCP, IMC, DESC_K(9), DESC_KG(9), DESC_OP(9),
     .        N, NM, NMMAX, MAXITR, MAXNCV, NEV, NCV, MAXN, LDV,
     .        ISHFTS, MODE, INFO, IPRI, NDEPL, II, LENBUFR(2,NSPMD),
     .        ICTXT, MAXNEV
      my_real
     .        SHIFT, TOL
      CHARACTER WHICH*2
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IMEXCH
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: KL, KEXCH, KGL, MEXCH
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: RIGI, RIGIG, OP, V, VAL, 
     .                                        VV
C
      INTEGER MSGOFF, MSGOFF2, MSGOFF3, MSGOFF4, MSGOFF5
      DATA MSGOFF  /200000/ 
      DATA MSGOFF2 /300000/
      DATA MSGOFF3 /400000/
      DATA MSGOFF4 /500000/
      DATA MSGOFF5 /600000/
#ifdef MPI
C
      IF (NDDLC.EQ.0) RETURN
C
      DO I=1,NSPMD
         LENBUF(1,I)=0
         LENBUF(2,I)=0
      ENDDO
      NDF=PGRAPH%DGRAPH(1)%NDDL_F
      NDM=PGRAPH%DGRAPH(1)%NMDYN
      DO I=1,NDF+NDM
         IR=PGRAPH%IPROW(1,I)
         DO J=1,NDF+NDM
            IC=PGRAPH%IPCOL(1,J)
            IPROC=DSNCOL*(IR-1)+IC
            LENBUF(1,IPROC)=LENBUF(1,IPROC)+1
         ENDDO
         IPROC=DSNCOL*(IR-1)+1
         LENBUF(2,IPROC)=LENBUF(2,IPROC)+1
      ENDDO
C      
      CALL SPMD_DS_IEXCH(LENBUF, LENBUFR, 2, MSGOFF)
C
      IF (ISPMD.EQ.0.AND.IPMESS.NE.0) THEN
         WRITE(ISTDO,*)
         WRITE(ISTDO,'(A)') TITRE(1:LTITR1)
         WRITE(IOUT,'(A)') TITRE(1:LTITR1)
      ENDIF
C
      IFILE=PGRAPH%IFILE
      LEN=(NDF+NDM)*(NDF+NDM+1)/2
C Chargement de la matrice de rigidite
      ALLOCATE(KL(LEN))
      IREC=PGRAPH%DGRAPH(1)%KREC
      IKC=0
      DO IR=1,LEN/10
         READ(IFILE,REC=IREC) (KL(IKC+J),J=1,10)
         IKC=IKC+10
         IREC=IREC+1
      ENDDO
      NR=LEN-(LEN/10)*10
      IF (NR.GT.0) READ(IFILE,REC=IREC) (KL(IKC+J),J=1,NR)
C
      LENM=0
      LENMR=0
      DO I=1,NSPMD
         IADM(I)=LENM+1
         LENM=LENM+LENBUF(1,I)
         IADMC(I)=IADM(I)
         IADMR(I)=LENMR+1
         LENMR=LENMR+LENBUFR(1,I)
      ENDDO
      IADM(NSPMD+1)=LENM+1
      IADMR(NSPMD+1)=LENMR+1
      ALLOCATE(KEXCH(LENM), IMEXCH(LENM,2))
      DO I=1,NDF+NDM
         IAD1=(I-1)*I/2
         DO J=1,I-1
C Terme triangle inferieur
            IR=PGRAPH%IPROW(1,I)
            IC=PGRAPH%IPCOL(1,J)
            IPROC=(IR-1)*DSNCOL+IC
            IMEXCH(IADMC(IPROC),1)=PGRAPH%IPROW(2,I)
            IMEXCH(IADMC(IPROC),2)=PGRAPH%IPCOL(2,J)
            KEXCH(IADMC(IPROC))=KL(IAD1+J)
            IADMC(IPROC)=IADMC(IPROC)+1
C Terme symetrique triangle superieur
            IR=PGRAPH%IPROW(1,J)
            IC=PGRAPH%IPCOL(1,I)
            IPROC=(IR-1)*DSNCOL+IC
            IMEXCH(IADMC(IPROC),1)=PGRAPH%IPROW(2,J)
            IMEXCH(IADMC(IPROC),2)=PGRAPH%IPCOL(2,I)
            KEXCH(IADMC(IPROC))=KL(IAD1+J)
            IADMC(IPROC)=IADMC(IPROC)+1
         ENDDO
         IR=PGRAPH%IPROW(1,I)
         IC=PGRAPH%IPCOL(1,I)
         IPROC=(IR-1)*DSNCOL+IC
         IMEXCH(IADMC(IPROC),1)=PGRAPH%IPROW(2,I)
         IMEXCH(IADMC(IPROC),2)=PGRAPH%IPCOL(2,I)
         KEXCH(IADMC(IPROC))=KL(IAD1+I)
         IADMC(IPROC)=IADMC(IPROC)+1
      ENDDO
      DEALLOCATE(KL)
C
      NNRP=PGRAPH%NNRP
      NNCP=PGRAPH%NNCP
      ALLOCATE(RIGI(NNRP,NNCP))
      DO I=1,NNRP
         DO J=1,NNCP
            RIGI(I,J)=ZERO
         ENDDO
      ENDDO
      CALL SPMD_DS_MEXCH(RIGI, NNRP,  IMEXCH, KEXCH, MSGOFF2,
     .                   IADM, IADMR, LENM  ) 
      DEALLOCATE(KEXCH)      
C Chargement de la matrice de rigidite geometrique
      ALLOCATE(KGL(LEN))
      IREC=PGRAPH%DGRAPH(1)%MREC
      IMC=0
      DO IR=1,LEN/10
         READ(IFILE,REC=IREC) (KGL(IMC+J),J=1,10)
         IMC=IMC+10
         IREC=IREC+1
      ENDDO
      NR=LEN-(LEN/10)*10
      IF (NR.GT.0) READ(IFILE,REC=IREC) (KGL(IMC+J),J=1,NR)
C
      ALLOCATE(MEXCH(LENM))
      DO I=1,NSPMD
         IADMC(I)=IADM(I)
      ENDDO
      DO I=1,NDF+NDM
         IAD1=(I-1)*I/2
         DO J=1,I-1
C Terme triangle inferieur
            IR=PGRAPH%IPROW(1,I)
            IC=PGRAPH%IPCOL(1,J)
            IPROC=(IR-1)*DSNCOL+IC
            MEXCH(IADMC(IPROC))=KGL(IAD1+J)
            IADMC(IPROC)=IADMC(IPROC)+1
C Terme symetrique triangle superieur
            IR=PGRAPH%IPROW(1,J)
            IC=PGRAPH%IPCOL(1,I)
            IPROC=(IR-1)*DSNCOL+IC
            MEXCH(IADMC(IPROC))=KGL(IAD1+J)
            IADMC(IPROC)=IADMC(IPROC)+1
         ENDDO
         IR=PGRAPH%IPROW(1,I)
         IC=PGRAPH%IPCOL(1,I)
         IPROC=(IR-1)*DSNCOL+IC
         MEXCH(IADMC(IPROC))=KGL(IAD1+I)
         IADMC(IPROC)=IADMC(IPROC)+1
      ENDDO
      DEALLOCATE(KGL)
C
      ALLOCATE(RIGIG(NNRP,NNCP))
      DO I=1,NNRP
         DO J=1,NNCP
            RIGIG(I,J)=ZERO
         ENDDO
      ENDDO
      CALL SPMD_DS_MEXCH(RIGIG, NNRP,  IMEXCH, MEXCH, MSGOFF4,
     .                   IADM, IADMR, LENM  ) 
      DEALLOCATE(MEXCH, IMEXCH)
C Initialisation de la process grid
      CALL SL_INIT(ICTXT, DSNROW, DSNCOL)
C Descripteur pour RIGI
      DESC_K(1)=1
      DESC_K(2)=ICTXT
      DESC_K(3)=NDDLC
      DESC_K(4)=NDDLC
      DESC_K(5)=DSNBLOC
      DESC_K(6)=DSNBLOC
      DESC_K(7)=0
      DESC_K(8)=0
      DESC_K(9)=MAX(1,NNRP)
C Descripteur pour RIGIG
      DESC_KG(1)=1
      DESC_KG(2)=ICTXT
      DESC_KG(3)=NDDLC
      DESC_KG(4)=NDDLC
      DESC_KG(5)=DSNBLOC
      DESC_KG(6)=DSNBLOC
      DESC_KG(7)=0
      DESC_KG(8)=0
      DESC_KG(9)=MAX(1,NNRP)
C Descripteur pour OP
      DESC_OP(1)=1
      DESC_OP(2)=ICTXT
      DESC_OP(3)=NDDLC
      DESC_OP(4)=NDDLC
      DESC_OP(5)=DSNBLOC
      DESC_OP(6)=DSNBLOC
      DESC_OP(7)=0
      DESC_OP(8)=0
      DESC_OP(9)=MAX(1,NNRP)
C
      N=NDDLC
      NM=BINCV
      NMMAX=BMAXNCV
      MAXITR=BNITER
      MAXNCV=NMMAX*NBUCK
      SHIFT=SHFTBUCK
      NEV=NBUCK
      NCV=NM*NEV
      MAXN=N
      MAXNEV=NEV
      LDV=NNRP
      WHICH='LM'
      ISHFTS=1
      MODE=4
      INFO=0
      TOL=ZERO
      IPRI=BIPRI
      ALLOCATE(OP(NNRP,N), V(NNRP,MAXNCV), VAL(MAXNCV,2))
      VAL = 0
      DO I=1,N
         DO J=1,NNRP
            OP(J,I)=RIGI(J,I)-SHIFT*RIGIG(J,I)
         ENDDO
      ENDDO
#ifdef WITHOUT_LINALG
      WRITE(6,*) "Error: compiled with WITHOUT_LINALG"
      CALL FLUSH(6)
      CALL ARRET(5)
#else
      CALL DSBUCKP(N,       NEV,    NCV,     WHICH,  INFO,
     .             MAXN,    MAXNEV, MAXNCV,  LDV,    ISHFTS,
     .             MAXITR,  MODE,   TOL,     OP,     RIGI, 
     .             RIGIG,   VAL,    V,       IPRI,   SHIFT, 
     .             DESC_OP, DESC_K, DESC_KG, NNRP,   ICTXT, 
     .             DSNROW,  DSNCOL, DSNBLOC)
#endif
      DO I=1,NBUCK
         EIG(I,1)=VAL(I,1)
         EIG(I,2)=VAL(I,2)
      ENDDO   
      IF (ISPMD.EQ.0) THEN
         WRITE(IOUT,*)
         WRITE(IOUT,'(A6,1PG11.4,A35,I10)') 
     .    'SHIFT ',SHIFT,' NUMBER OF BUCKLING CRITICAL LOADS ',NBUCK
         WRITE(IOUT,'(A)') ' CRITICAL LOADS:'
         WRITE(IOUT,'(A)') '         NUMBER  CRITICAL LOAD'
         DO I=1,NBUCK
            WRITE(IOUT,'(5X,I10,4X,1PG11.4)') I,EIG(I,1)
         ENDDO
         WRITE(IOUT,*)
      ENDIF 
      NDEPL=NBUCK
C
C Redescente des deplacements
C
      ALLOCATE(VV(NDDLC,NDEPL))
      CALL SPMD_DS_VDESC(V,       NDEPL, VV, NDDLC, NNRP,
     .                   MSGOFF5) 
C
      DO I=1,NDF
         II=PGRAPH%IPROW(3,I)
         DO J=1,NDEPL
            PGRAPH%DGRAPH(1)%D(J,I)=VV(II,J)
         ENDDO
      ENDDO
      DO I=1,NDM
         II=PGRAPH%IPROW(3,NDF+I)
         DO J=1,NDEPL
            PGRAPH%DGRAPH(1)%DM(J,I)=VV(II,J)
         ENDDO
      ENDDO
C
      DEALLOCATE(V, VV)
      DEALLOCATE(RIGI, RIGIG, OP)
      CALL BLACS_GRIDEXIT(ICTXT)
C
#endif
      RETURN
      END
            
