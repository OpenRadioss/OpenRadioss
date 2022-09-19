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
Chd|  DSPGRID                       source/implicit/dsolve/dspgrid.F
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_DSOLV                     source/implicit/imp_dsolv.F   
Chd|        IMP_DSOLV_ITER                source/implicit/imp_dsolv.F   
Chd|-- calls ---------------
Chd|        SPMD_DS_IRECV                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_DS_ISEND                 source/mpi/implicit/spmd_dsreso.F
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE DSPGRID(PGRAPH, NDDL, NDDLC, ITYP)
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
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL, NDDLC, ITYP
      TYPE(PRGRAPH) :: PGRAPH
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LENBUF(NSPMD), IRQTAG, I, II, ITAG(2+NSPMD,NDDL),
     .        J, JJ, NMOD, NMDYN(NSPMD), N, NN, IADM, NRMAX,
     .        NR, NBLOCMAX, NDDLL, IPR, IPC, IRL, ICL, NDDLF, IP, NP,
     .        IR, IC
      INTEGER, DIMENSION(:), ALLOCATABLE :: IBUF, IADR, IADC
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IROW, ICOL
      my_real
     .        RNSPMD
      INTEGER MSGOFF
      DATA MSGOFF  /200000/
C
      IF (ISPMD/=0) THEN
         LENBUF(1)=PGRAPH%DGRAPH(1)%NDDL_F+2
         IRQTAG=MSGOFF + ISPMD
         CALL SPMD_DS_ISEND(LENBUF(1), 1, IRQTAG, 1)
C
         ALLOCATE(IBUF(LENBUF(1)))
         IBUF(1)=PGRAPH%DGRAPH(1)%NDDL_F
         IBUF(2)=PGRAPH%DGRAPH(1)%NMDYN
         DO I=1,PGRAPH%DGRAPH(1)%NDDL_F
            II=PGRAPH%DGRAPH(1)%DDLS(I)
            II=PGRAPH%LSDDL_GLOB(II)
            IBUF(2+I)=II
         ENDDO
         IRQTAG=MSGOFF + NSPMD-1 + ISPMD
         CALL SPMD_DS_ISEND(IBUF, LENBUF(1), IRQTAG, 1)
C
         IRQTAG=MSGOFF + 2*(NSPMD-1) + ISPMD
         CALL SPMD_DS_IRECV(IBUF, LENBUF(1), IRQTAG, 1)
      ELSE
         DO I=1,NDDL
            ITAG(1,I)=0
         ENDDO
C
         DO I=1,PGRAPH%DGRAPH(1)%NDDL_F
            II=PGRAPH%DGRAPH(1)%DDLS(I)
            II=PGRAPH%LSDDL_GLOB(II)
            ITAG(1,II)=1
            ITAG(3,II)=1
         ENDDO
         NMDYN(1)=PGRAPH%DGRAPH(1)%NMDYN
         NMOD=NMDYN(1)
C
         DO I=1,NSPMD-1
            IRQTAG=MSGOFF + I
            CALL SPMD_DS_IRECV(LENBUF(I+1), 1, IRQTAG, I+1)
C
            ALLOCATE(IBUF(LENBUF(I+1)))
            IRQTAG=MSGOFF + NSPMD-1 + I
            CALL SPMD_DS_IRECV(IBUF, LENBUF(I+1), IRQTAG, I+1)
            DO J=1,IBUF(1)
               JJ=IBUF(2+J)
               ITAG(1,JJ)=ITAG(1,JJ)+1
               IP=ITAG(1,JJ)
               ITAG(2+IP,JJ)=I+1
            ENDDO
            NMDYN(I+1)=IBUF(2)
            NMOD=NMOD+NMDYN(I+1)
            DEALLOCATE(IBUF)
         ENDDO
C
         N=0
         DO I=1,NDDL
            N=N+MIN(1,ITAG(1,I))
            IF (ITAG(1,I)/=0) ITAG(2,I)=N
         ENDDO
         NN=N
         N=N+NMOD
C
         IADM=NN+1+PGRAPH%DGRAPH(1)%NMDYN
         DO I=1,NSPMD-1
            ALLOCATE(IBUF(LENBUF(I+1)))
            IBUF(1)=IADM
            IBUF(2)=N
            JJ=0
            DO J=1,NDDL
               IF (ITAG(1,J)/=0) THEN
                  NP=ITAG(1,J)
                  DO IP=1,NP
                     IF (ITAG(2+IP,J)==I+1) THEN
                        JJ=JJ+1
                        IBUF(2+JJ)=ITAG(2,J)
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
            IRQTAG=MSGOFF + 2*(NSPMD-1) + I
            CALL SPMD_DS_ISEND(IBUF, LENBUF(I+1), IRQTAG, I+1)
            IADM=IADM+NMDYN(I+1)
            DEALLOCATE(IBUF)
         ENDDO
C
         LENBUF(1)=PGRAPH%DGRAPH(1)%NDDL_F+2
         ALLOCATE(IBUF(LENBUF(1)))
         IBUF(1)=NN+1
         IBUF(2)=N
         II=0
         DO I=1,NDDL
            IF (ITAG(1,I)/=0) THEN
               DO J=1,ITAG(1,I)
                  IF (ITAG(2+J,I)==1) THEN
                     II=II+1
                     IBUF(2+II)=ITAG(2,I)
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
      ENDIF
C
      NDDLC=IBUF(2)
      DSNBLOC=0
      DSNROW=0
      DSNCOL=0
C
      IF (ITYP==1) THEN
C Process grid unidimensionnelle pour PARPACK
         DSNROW=NSPMD
         DSNCOL=1
         DSNBLOC=NDDLC/NSPMD+1
      ELSEIF (ITYP==2) THEN
C Progress grid bi-dimensionnelle pour resolution statique
         RNSPMD=NSPMD
         NRMAX=INT(SQRT(RNSPMD))
         DO NR=1,NRMAX
            IF (MOD(NSPMD,NR)==0) THEN
               DSNROW=NR
               DSNCOL=NSPMD/NR
            ENDIF
         ENDDO
         IF (NDDLC<1000) THEN
            NBLOCMAX=32
         ELSE
            NBLOCMAX=64
         ENDIF
         DSNBLOC=MIN(NDDLC/DSNROW, NDDLC/DSNCOL)
         DSNBLOC=MIN(NBLOCMAX,DSNBLOC)
         DSNBLOC=MAX(1,DSNBLOC)
      ENDIF
C
      NDDLL=PGRAPH%DGRAPH(1)%NDDL_F+PGRAPH%DGRAPH(1)%NMDYN
      DEALLOCATE(PGRAPH%IPROW,PGRAPH%IPCOL)
      ALLOCATE(PGRAPH%IPROW(3,NDDLL), PGRAPH%IPCOL(3,NDDLL))
C
      ALLOCATE(IROW(2,NDDLC), ICOL(2,NDDLC), IADR(DSNROW), IADC(DSNCOL))
      IPR=1
      IPC=1
      IRL=0
      ICL=0
      DO I=1,DSNROW
         IADR(I)=0
      ENDDO
      DO I=1,DSNCOL
         IADC(I)=0
      ENDDO
      DO I=1,NDDLC
         IRL=IRL+1
         ICL=ICL+1
         IF (IRL>DSNBLOC) THEN
            IRL=1
            IPR=IPR+1
            IF (IPR>DSNROW) IPR=1
         ENDIF
         IROW(1,I)=IPR
         IADR(IPR)=IADR(IPR)+1
         IROW(2,I)=IADR(IPR)
         IF (ICL>DSNBLOC) THEN
            ICL=1
            IPC=IPC+1
            IF (IPC>DSNCOL) IPC=1
         ENDIF
         ICOL(1,I)=IPC
         IADC(IPC)=IADC(IPC)+1
         ICOL(2,I)=IADC(IPC)
      ENDDO
C
      NDDLF=PGRAPH%DGRAPH(1)%NDDL_F
      DO I=1,NDDLF
         II=IBUF(2+I)
         PGRAPH%IPROW(1,I)=IROW(1,II)
         PGRAPH%IPCOL(1,I)=ICOL(1,II)
         PGRAPH%IPROW(2,I)=IROW(2,II)
         PGRAPH%IPCOL(2,I)=ICOL(2,II)
         PGRAPH%IPROW(3,I)=II
         PGRAPH%IPCOL(3,I)=II
      ENDDO
      DO I=1,PGRAPH%DGRAPH(1)%NMDYN
         II=IBUF(1)+I-1
         PGRAPH%IPROW(1,NDDLF+I)=IROW(1,II)
         PGRAPH%IPCOL(1,NDDLF+I)=ICOL(1,II)
         PGRAPH%IPROW(2,NDDLF+I)=IROW(2,II)
         PGRAPH%IPCOL(2,NDDLF+I)=ICOL(2,II)
         PGRAPH%IPROW(3,NDDLF+I)=II
         PGRAPH%IPCOL(3,NDDLF+I)=II
      ENDDO
C
      IR=ISPMD/DSNCOL+1
      IC=ISPMD-(IR-1)*DSNCOL+1
      PGRAPH%NNRP=IADR(IR)
      PGRAPH%NNCP=IADC(IC)
C
      DEALLOCATE(IBUF, IROW, ICOL, IADR, IADC)
C
      RETURN
      END
Chd|====================================================================
Chd|  DSDIM                         source/implicit/dsolve/dspgrid.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DSDIM(NDDLC, NRP, NCP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDLC, NRP, NCP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NRMAX, NR, NROW, NCOL, NBLOC, NBLOCMAX, NNR(NSPMD),
     .        NNC(NSPMD), I, NN, IIR, IIC, NN2, IR, IC
      my_real
     .        RNSPMD
C
      RNSPMD=NSPMD
      NRMAX=INT(SQRT(RNSPMD))
      DO NR=1,NRMAX
         IF (MOD(NSPMD,NR)==0) THEN
            NROW=NR
            NCOL=NSPMD/NR
         ENDIF
      ENDDO
      IF (NDDLC<1000) THEN
         NBLOCMAX=32
      ELSE
         NBLOCMAX=64
      ENDIF
      NBLOC=MIN(NDDLC/NROW, NDDLC/NCOL)
      NBLOC=MIN(NBLOCMAX,NBLOC)
      NBLOC=MAX(1,NBLOC)
C
      DO I=1,NROW
         NNR(I)=0
      ENDDO
      DO I=1,NCOL
         NNC(I)=0
      ENDDO
      NN=0
      IIR=0
      IIC=0
      DO WHILE (NN<NDDLC)
         IIR=IIR+1
         IIC=IIC+1
         IF (IIR==NROW+1) IIR=1
         IF (IIC==NCOL+1) IIC=1
         NN2=MIN(NN+NBLOC,NDDLC)
         NNR(IIR)=NNR(IIR)+NN2-NN
         NNC(IIC)=NNC(IIC)+NN2-NN
         NN=NN2
      ENDDO
C
      IR=ISPMD/NCOL+1
      IC=ISPMD-(IR-1)*NCOL+1
      NRP=NNR(IR)
      NCP=NNC(IC)
C
      RETURN
      END

