Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
#ifdef MUMPS5
Chd|====================================================================
Chd|  IMP_MUMPS1                    source/implicit/imp_mumps.F   
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        MUMPS_SET2                    source/implicit/imp_mumps.F   
Chd|        PRINT_STIFF_MAT               source/implicit/imp_mumps.F   
Chd|        SPMD_CDDL                     source/mpi/implicit/imp_spmd.F
Chd|        SPMD_INF_G                    source/mpi/implicit/imp_spmd.F
Chd|        SPMD_MUMPS_COUNT              source/mpi/implicit/imp_spmd.F
Chd|        SPMD_MUMPS_DEAL               source/mpi/implicit/imp_spmd.F
Chd|        SPMD_MUMPS_GATH               source/mpi/implicit/imp_spmd.F
Chd|        SPMD_MUMPS_INI                source/mpi/implicit/imp_spmd.F
Chd|        TMPENVF                       source/system/tmpenv_c.c      
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        IMP_KBCS                      share/modules/impbufdef_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE IMP_MUMPS1(NDDL0  , NNZK0    , NDDL  , NNZK , NNMAX   ,
     .                      NODGLOB, IDDL     , NDOF  , INLOC, IKC     ,
     .                      IADK   , JDIK     , DIAG_K, LT_K , IAD_ELEM,
     .                      FR_ELEM, MUMPS_PAR, CDDLP , IADI , JDII    ,
     .                      ITOK   , DIAG_I   , LT_I  , NDDLI, NNZI    ,
     .                      IMPRINT, IT)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE IMP_INTM
      USE IMP_KBCS
      USE MESSAGE_MOD      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "dmumps_struc.h"
#include "impl1_c.inc"
#include "task_c.inc"
#include "units_c.inc"
#include "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL0, NNZK0, NDDL, NNZK, NNMAX, NODGLOB(*), IDDL(*),
     .        NDOF(*), INLOC(*), IKC(*), IADK(*), JDIK(*),
     .        IAD_ELEM(2,*), FR_ELEM(*), CDDLP(*), IADI(*), JDII(*),
     .        ITOK(*), NDDLI, NNZI,IMPRINT,TLEN, IT
      my_real
     .        DIAG_K(*), LT_K(*), DIAG_I(*), LT_I(*)
      TYPE(DMUMPS_STRUC) MUMPS_PAR
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J , N
      INTEGER NDDLG0, NNZKG0, NDDLG, NNZKG, NNMAXG,
     .        NDDL0P(NSPMD), NNZK0P(NSPMD), NDDLP(NSPMD),
     .        NNZKP(NSPMD), NNMAXP(NSPMD), NKLOC,
     .        NKFRONT, NKFLOC, NZLOC, NNZ, NZP(NSPMD-1), IACTI(NDDL),
     .        NNZT
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITK
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: RTK
C
      IF (NDDLI==0) NNZI=0
C Desallocations si necessaire

      CALL SPMD_MUMPS_DEAL(MUMPS_PAR)
C
      CALL SPMD_MUMPS_INI(MUMPS_PAR, 1)
C

      IF (NCYCLE==1.AND.INCONV==1) THEN
         MUMPS_PAR%ICNTL(3) = IOUT
      ELSE
         MUMPS_PAR%ICNTL(3) = -1    
      ENDIF
C--Level of info to be printed with user input /MUMPS/MSGLV/n
      IF(M_MSG > 0) THEN
        MUMPS_PAR%ICNTL(3) = IOUT ! standard output
        MUMPS_PAR%ICNTL(4) = M_MSG ! max lev of info
      ENDIF
C      
      IF (M_ORDER==0) THEN
       MUMPS_PAR%ICNTL(7) = 7 ! Automatic choice of ordering 
      ELSE
       MUMPS_PAR%ICNTL(7) =  M_ORDER
      END IF

      MUMPS_PAR%ICNTL(13) = 1 ! Disable scalapack for the root matrix
C uncomment to set out of core
C      MUMPS_PAR%ICNTL(22)=1

      IF (M_OCORE > 0) THEN
        CALL TMPENVF(MUMPS_PAR%OOC_TMPDIR,TLEN)
        MUMPS_PAR%ICNTL(22)=1
      ENDIF

      IF (NSPMD>1) THEN
C LMEMV is the memory on the host (i.e. node)
C        MUMPS_PAR%ICNTL(23)=LMEMV/NSPMD_PER_NODE
         IF (IMUMPSD==1) THEN
            MUMPS_PAR%ICNTL(18)=3
         ELSEIF (IMUMPSD==2) THEN
            MUMPS_PAR%ICNTL(18)=0
         ENDIF
         IF (IDTC==3) MUMPS_PAR%ICNTL(13)=1
C
         NDDLG0 = NDDL0
         NNZKG0 = NNZK0
         NDDLG = NDDL 
         NNZKG = NNZK
         NNMAXG = NNMAX
         CALL SPMD_INF_G(
     1    NDDLG0   ,NNZKG0  ,NDDLG    ,NNZKG    ,NNMAXG    ,
     2    NDDL0P   ,NNZK0P  ,NDDLP    ,NNZKP    ,NNMAXP    )
C
         CALL SPMD_CDDL(NDDL,  NODGLOB, IDDL,  NDOF,  CDDLP,
     .                  INLOC, IKC,     NDDLG, NDDLP)          
C
         NNZT = NDDL+NNZK+NNZI+NZ_SL+NZ_SI
         ALLOCATE(ITK(2,NNZT),RTK(NNZT))
C
         DO I=1,NDDL
            IACTI(I)=I
         ENDDO
C
         CALL MUMPS_SET2(
     .         IADK,  JDIK,     DIAG_K,  LT_K,   CDDLP,
     .         NKLOC, NKFRONT,  ITK,     RTK,    IDDL, 
     .         INLOC, IAD_ELEM, FR_ELEM, NDOF,   IKC,
     .         NDDL,  NNZK,     IACTI,   NDDLI,  NNZI,
     .         IADI,  JDII,     ITOK,    DIAG_I, LT_I )
C
c        CALL SPMD_MUMPS_FRONT(
c    .         ITK,   RTK,     NKFRONT, NKFLOC, NKLOC,
c    .         NDDLG, IMPRINT )
C
         NKFLOC = 0
         NZLOC=NKLOC+NKFLOC	 
         IF (IMUMPSD==1) THEN
            ALLOCATE(MUMPS_PAR%A_LOC(NZLOC),
     .               MUMPS_PAR%IRN_LOC(NZLOC),
     .               MUMPS_PAR%JCN_LOC(NZLOC))
            IF (ISPMD==0) THEN
               ALLOCATE(MUMPS_PAR%RHS(NDDLG))
            ELSE
               ALLOCATE(MUMPS_PAR%RHS(0))
            ENDIF
            MUMPS_PAR%N=NDDLG
            MUMPS_PAR%NZ_LOC=NZLOC
C
            DO I=1,NZLOC
               MUMPS_PAR%IRN_LOC(I)=ITK(1,I)
               MUMPS_PAR%JCN_LOC(I)=ITK(2,I)
               MUMPS_PAR%A_LOC(I)=RTK(I)
            ENDDO
         ELSEIF (IMUMPSD==2) THEN
            CALL SPMD_MUMPS_COUNT(NZLOC, NZP, NNZ)
C
            IF (ISPMD==0) THEN
               ALLOCATE(MUMPS_PAR%A(NNZ),
     .                  MUMPS_PAR%IRN(NNZ),
     .                  MUMPS_PAR%JCN(NNZ),
     .                  MUMPS_PAR%RHS(NDDLG))
               MUMPS_PAR%N=NDDLG
               MUMPS_PAR%NZ=NNZ 
            ELSE
               ALLOCATE(MUMPS_PAR%A(0),
     .                  MUMPS_PAR%IRN(0),
     .                  MUMPS_PAR%JCN(0),
     .                  MUMPS_PAR%RHS(0))
            ENDIF
C
            CALL SPMD_MUMPS_GATH(
     . ITK,           RTK, NZLOC, MUMPS_PAR%A, MUMPS_PAR%IRN,
     . MUMPS_PAR%JCN, NZP)
C
         ENDIF
        DEALLOCATE(ITK, RTK)
      ELSE
         MUMPS_PAR%ICNTL(18)=0
C
         DO I=1,NDDL
            CDDLP(I)=I
         ENDDO
         NNZT = NNZK 
         NNZK = NNZK + NDDLI + NNZI
C
         ALLOCATE(MUMPS_PAR%A(NDDL+NNZK), 
     .            MUMPS_PAR%IRN(NDDL+NNZK),
     .            MUMPS_PAR%JCN(NDDL+NNZK),
     .            MUMPS_PAR%RHS(NDDL))
C
         NNZ=0
         DO I=1,NDDLI
          J=ITOK(I)
            NNZ=NNZ+1
            MUMPS_PAR%IRN(NNZ)=J
            MUMPS_PAR%JCN(NNZ)=J
            MUMPS_PAR%A(NNZ)=DIAG_I(I)
          DO N=IADI(I),IADI(I+1)-1
               NNZ=NNZ+1
               MUMPS_PAR%IRN(NNZ)=J
               MUMPS_PAR%JCN(NNZ)=ITOK(JDII(N))
               MUMPS_PAR%A(NNZ)=LT_I(N)
          ENDDO
         ENDDO
         DO I=1,NDDL
            NNZ=NNZ+1
            MUMPS_PAR%IRN(NNZ)=I
            MUMPS_PAR%JCN(NNZ)=I
            MUMPS_PAR%A(NNZ)=DIAG_K(I)
            DO J=IADK(I),IADK(I+1)-1
               NNZ=NNZ+1
               MUMPS_PAR%IRN(NNZ)=I
               MUMPS_PAR%JCN(NNZ)=JDIK(J)
               MUMPS_PAR%A(NNZ)=LT_K(J)
            ENDDO
         ENDDO
C
         IF (IMPRINT/=0) THEN
            WRITE(ISTDO,*)
            WRITE(ISTDO,'(A21,I10,A8,I10)')
     .' MUMPS    DIM : NNZ =',NNZK+NDDL,' NNZFR =',0
         ENDIF
C
         MUMPS_PAR%N=NDDL
         MUMPS_PAR%NZ=NNZK+NDDL
         NNZK = NNZT 
      ENDIF
C   
c      WRITE(IOUT,*) "NCYCLE,IT=",NCYCLE,IT
      IF (PRSTIFMAT == 1 .AND. (ILINE==1 .OR. (PRSTIFMAT_NC == NCYCLE
     .                   .AND.  PRSTIFMAT_IT == IT))) THEN
        IF (ISPMD == 0) THEN
          WRITE(IOUT,1000)
          WRITE(ISTDO,1000)
          WRITE(IOUT,*)
          WRITE(ISTDO,*)
        ENDIF
        CALL PRINT_STIFF_MAT(MUMPS_PAR, NDDL, NODGLOB, IDDL, NDOF,  
     .                       CDDLP, INLOC, IKC, NDDLG, NDDLP)


      ENDIF
1000  FORMAT(5X,'--STIFFNESS MATRIX IS PRINTED--')
      RETURN
      END
Chd|====================================================================
Chd|  PRINT_STIFF_MAT               source/implicit/imp_mumps.F   
Chd|-- called by -----------
Chd|        IMP_MUMPS1                    source/implicit/imp_mumps.F   
Chd|-- calls ---------------
Chd|        SPMD_INT_ALLREDUCE_MAX        source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE PRINT_STIFF_MAT(MUMPS_PAR, NDDL, NODGLOB, IDDL, NDOF,  
     .                           CDDLP, INLOC, IKC, NDDLG, NDDLP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "dmumps_struc.h"
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "impl2_c.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(DMUMPS_STRUC) MUMPS_PAR
      INTEGER NDDL, NODGLOB(*), IDDL(*), NDOF(*), CDDLP(*), INLOC(*), IKC(*), NDDLG, NDDLP(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,JJ,NROWS,NCOLS,NENTRIES,IItmp,JJtmp,
     .        tmpIROWS,tmpICOLS,LENGTH,L,IND,COUNT_DUP,
     .        NKC,TDDL(2,MUMPS_PAR%N),N,ID,ND,IERROR,OFFST
      my_real
     .        tmpK,sumK,tmpPROC   
      INTEGER, DIMENSION(:), ALLOCATABLE :: IROWS,ICOLS,NENTRIEStmp
      my_real, DIMENSION(:), ALLOCATABLE :: K
      LOGICAL SWITCH
      CHARACTER FILNAME*100,FILNAME2*100,CSPMD
C-----------------------------------------------    
C Define offset for UNIT file
      OFFST = 100  
C Automatic write of stiffness coefficients in MatrixMarket format by MUMPS: MUMPS_PAR%WRITE_PROBLEM = 'string'
C      MUMPS_PAR%WRITE_PROBLEM ="./stiffness_matrix_MUMPS"
      WRITE(CSPMD,'(I1)') ISPMD
      FILNAME = 'local_stiffness_matrix_domain'//CSPMD   
      OPEN(UNIT=OFFST+ISPMD,FILE=FILNAME(1:30),ACCESS="SEQUENTIAL",
     .        ACTION="WRITE",STATUS="UNKNOWN")      
C Manual write of stiffness coefficients in MatrixMarket format 
C TDDL: local DOF (at MPI domain level) to global node and direction (DX,DY,DZ,RX,RY,RZ)            
      NKC = 0
      TDDL(1,:) = 0
      TDDL(2,:) = 0
      DO N=1,NUMNOD
         I=INLOC(N)
         DO J=1,NDOF(I)
            ND=IDDL(I)+J
            ID=ND-NKC
            IF (IKC(ND)<1) THEN
               TDDL(1,CDDLP(ID))=NODGLOB(I)
               TDDL(2,CDDLP(ID))=J
            ELSE
               NKC=NKC+1
            ENDIF
         ENDDO
      ENDDO        
C Communications between processes for TDDL(1:2,1:MUMPS_PAR%N) 
      IF (NSPMD > 1) THEN         
        CALL SPMD_INT_ALLREDUCE_MAX(TDDL(1,:),TDDL(1,:),
     .                          MUMPS_PAR%N)  
        CALL SPMD_INT_ALLREDUCE_MAX(TDDL(2,:),TDDL(2,:),
     .                          MUMPS_PAR%N)       
      END IF  
C All processes: write local stiffness coefficients (at MPI domain level) in MatrixMarket format    
      IF (NSPMD == 1) THEN
        WRITE(OFFST+ISPMD,1002) MUMPS_PAR%N,MUMPS_PAR%N,MUMPS_PAR%NZ
        DO I=1,MUMPS_PAR%NZ
          IItmp = MUMPS_PAR%IRN(I)
          JJtmp = MUMPS_PAR%JCN(I)
          II = 6*(TDDL(1,IItmp)-1)+TDDL(2,IItmp)
          JJ = 6*(TDDL(1,JJtmp)-1)+TDDL(2,JJtmp)
          IF (JJ > II) THEN
            IItmp = II
            II = JJ
            JJ = IItmp
          ENDIF
          WRITE(OFFST+ISPMD,1003) II,JJ,MUMPS_PAR%A(I)
        ENDDO 
      ELSE
        WRITE(OFFST+ISPMD,1002) MUMPS_PAR%N,MUMPS_PAR%N,MUMPS_PAR%NZ_LOC
        DO I=1,MUMPS_PAR%NZ_LOC
          IItmp = MUMPS_PAR%IRN_LOC(I)
          JJtmp = MUMPS_PAR%JCN_LOC(I)
          II = 6*(TDDL(1,IItmp)-1)+TDDL(2,IItmp)
          JJ = 6*(TDDL(1,JJtmp)-1)+TDDL(2,JJtmp)
          IF (JJ > II) THEN
            IItmp = II
            II = JJ
            JJ = IItmp
          ENDIF
          WRITE(OFFST+ISPMD,1003) II,JJ,MUMPS_PAR%A_LOC(I)
        ENDDO 
      ENDIF   
      CLOSE(UNIT=OFFST+ISPMD)           
#ifdef MPI
      CALL MPI_BARRIER(MPI_COMM_WORLD,IERROR)
#endif
C Process 0: read stiff. coeff. from all processes, sort (bubble) and write stiffness coefficients in MatrixMarket format         
      IF (ISPMD == 0) THEN
         ALLOCATE(NENTRIEStmp(NSPMD))
         WRITE(CSPMD,'(I1)') NSPMD
         NENTRIES = 0         
         FILNAME2 = 'stiffness_matrix_'//CSPMD//'_SPMD'
         OPEN(UNIT=OFFST+NSPMD,FILE=FILNAME2(1:23),ACCESS="SEQUENTIAL",
     .        ACTION="WRITE",STATUS="UNKNOWN") 
         DO L = 0,NSPMD-1
           WRITE(CSPMD,'(I1)') L
           FILNAME = 'local_stiffness_matrix_domain'//CSPMD         
           OPEN(UNIT=OFFST+L,FILE=FILNAME(1:30),ACCESS="SEQUENTIAL",
     .          ACTION="READ",STATUS="UNKNOWN")  
           READ(UNIT=OFFST+L,FMT=*) NROWS,NCOLS,NENTRIEStmp(L+1) 
           NENTRIES = NENTRIES + NENTRIEStmp(L+1)
         ENDDO
         ALLOCATE(IROWS(NENTRIES))
         ALLOCATE(ICOLS(NENTRIES))
         ALLOCATE(K(NENTRIES))
         IND = 0
         sumK = ZERO
         DO L = 0,NSPMD-1
             DO I = 1,NENTRIEStmp(L+1)
                IND = IND + 1
                READ(UNIT=OFFST+L,FMT=*) IROWS(IND),ICOLS(IND),K(IND)
             ENDDO
         ENDDO
C Bubble sort in ascending order of ICOLS and then IROWS         
         I = NENTRIES
         SWITCH = .TRUE.
         DO WHILE ((I>0) .AND. (SWITCH))
             SWITCH = .FALSE.
             DO J = 1,I-1
                 IF (ICOLS(J) > ICOLS(J+1) .OR. (ICOLS(J) == ICOLS(J+1) 
     .               .AND. IROWS(J) > IROWS(J+1))) THEN
                       tmpIROWS = IROWS(J)
                       IROWS(J) = IROWS(J+1)
                       IROWS(J+1) = tmpIROWS
                       tmpICOLS = ICOLS(J)
                       ICOLS(J) = ICOLS(J+1)
                       ICOLS(J+1) = tmpICOLS                     
                       tmpK = K(J)
                       K(J) = K(J+1)
                       K(J+1) = tmpK                           
                       SWITCH = .TRUE.
                 ENDIF
             ENDDO 
             I = I - 1
         ENDDO  
C Write stiff. coeff. in only one file (suppress duplications) 
         IND = 1 
         DO WHILE (IND <= NENTRIES)
           tmpK = K(IND)
           DO WHILE (IND <= NENTRIES .AND. IROWS(IND)==IROWS(IND+1) 
     .               .AND. ICOLS(IND)==ICOLS(IND+1))  
             IND = IND + 1
             tmpK = tmpK + K(IND)      
           ENDDO
           IF (ABS(tmpK)>=PRSTIFMAT_TOL) THEN
             WRITE(OFFST+NSPMD,1003) IROWS(IND),ICOLS(IND),tmpK
             sumK = sumK + ABS(tmpK)
           ENDIF
           IND = IND + 1
         ENDDO
         WRITE(OFFST+NSPMD,1001) sumK
          DO L = 0,NSPMD            
            CLOSE(UNIT=OFFST+L)
          ENDDO 
        DEALLOCATE(NENTRIEStmp)
        DEALLOCATE(IROWS)
        DEALLOCATE(ICOLS)         
        DEALLOCATE(K)
      ENDIF
1000  FORMAT(I10,I10,I10,I10,E10.2) 
1001  FORMAT('Sum ABS(K_ij) = ',E10.2)  
1002  FORMAT(I10,I10,I10)   
1003  FORMAT(I10,I10,E10.2)
C
      RETURN
      END                
Chd|====================================================================
Chd|  IMP_MUMPS2                    source/implicit/imp_mumps.F   
Chd|-- called by -----------
Chd|        LIN_SOLVP2                    source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        SPMD_MUMPS_EXEC               source/mpi/implicit/imp_spmd.F
Chd|        SPMD_MUMPS_RHS                source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE IMP_MUMPS2(MUMPS_PAR, CDDLP, F, X, NDDL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "dmumps_struc.h"
#include "impl1_c.inc"
#include "filescount_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER CDDLP(*), NDDL
      my_real F(*), X(*)
      TYPE(DMUMPS_STRUC) MUMPS_PAR
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NDDLG,LENT
C
      IF (MUMPS_PAR%N<=0) RETURN
      IF (IDSC==1) CALL SPMD_MUMPS_EXEC(MUMPS_PAR, 1)
C---------For licence
      IF (IDSC==1) ISOLV_D = 1              
C
      NDDLG=MUMPS_PAR%N
      CALL SPMD_MUMPS_RHS(F,     CDDLP, MUMPS_PAR%RHS, NDDL, 1,
     .                    NDDLG)
C
      CALL SPMD_MUMPS_EXEC(MUMPS_PAR, 2)
C
      CALL SPMD_MUMPS_RHS(X,     CDDLP, MUMPS_PAR%RHS, NDDL, 2,
     .                    NDDLG)
C----FLAG for MUMPS: IF (IMPL_S>0.AND.ISOLV==3.AND.IMACH==3)	
	LENT = 1024*MUMPS_PAR%INFO(16)                   
        MUMPSFILESIZE = MAX(MUMPSFILESIZE,LENT)       
C
      RETURN
      END  

Chd|====================================================================
Chd|  MUMPS_SET                     source/implicit/imp_mumps.F   
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SPMD_IFRI                     source/mpi/implicit/imp_spmd.F
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE MUMPS_SET(IADK , JDIK    , DIAG_K , LT_K  , CDDLP,
     .                     NKLOC, NKFRONT , ITK    , RTK   , IDDL ,
     .                     INLOC, IAD_ELEM, FR_ELEM, NDOF  , IKC  ,
     .                     NDDL , NNZK    , IACTI  , NDDLI , NNZI ,
     .                     IADI , JDII    , ITOK   , DIAG_I, LT_I )     
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "com04_c.inc"
#include "com08_c.inc"
#include "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IADK(*), JDIK(*), CDDLP(*), NKLOC, NKFRONT, ITK(2,*),
     .        IDDL(*), INLOC(*), IAD_ELEM(2,*), FR_ELEM(*), NDOF(*),
     .        IKC(*), NDDL, NNZK, IACTI(*), NDDLI, NNZI, IADI(*),
     .        JDII(*), ITOK(*)
      my_real
     .        DIAG_K(*), LT_K(*), RTK(*), DIAG_I(*), LT_I(*)
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IDDL_FRONT(NSPMD+1,NDDL), NKC, N, TNKC(NUMNOD),
     .        J, ND, NOD, INOD, KK, K, ID, NN, ILOC, JJ,
     .        ITAG(2,NSPMD), INDEX, II, IDIAG(NDDL), IADL(NDDL),
     .        IADLFRONT(NDDL), IFOUND, CDDLP_REM(NDDL_SI),
     .        IDDL_REM(NDDL_SI)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITKFRONT
      my_real, DIMENSION(:), ALLOCATABLE :: RTKFRONT
C
      ALLOCATE(ITKFRONT(2,NDDL+NNZK+NNZI+NZ_SI), 
     .         RTKFRONT(NDDL+NNZK+NNZI+NZ_SI))
C
      DO I=1,NDDL
         IDDL_FRONT(1,I)=1
         IDDL_FRONT(2,I)=ISPMD+1
      ENDDO
C
      NKC=0
      DO N=1,NUMNOD
         I=INLOC(N)
         TNKC(I)=NKC
         DO J=1,NDOF(I)
            ND=IDDL(I)+J
            ID=ND-NKC
            IF (IKC(ND)>=1) NKC=NKC+1
         ENDDO
      ENDDO
C
      DO I=1,NSPMD
C        IF (I==ISPMD+1) CYCLE
C
         DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            INOD=FR_ELEM(J)
            NKC=TNKC(INOD)
            DO K=1,NDOF(INOD)
               ND=IDDL(INOD)+K
               ID=ND-NKC
               IF (IKC(ND)<1) THEN
                  IDDL_FRONT(1,ID)=IDDL_FRONT(1,ID)+1
                  NN=IDDL_FRONT(1,ID)
                  IDDL_FRONT(NN+1,ID)=I
               ELSE
                  NKC=NKC+1
               ENDIF
            ENDDO
         ENDDO
      ENDDO
C
      NKLOC=0
      NKFRONT=0
      DO I=1,NDDL
         IF (IACTI(I)==0) CYCLE
         II=IACTI(I)
         IADL(II)=NKLOC+1
         IADLFRONT(II)=NKFRONT+1
         IF (IDDL_FRONT(1,II)==1) THEN
            NKLOC=NKLOC+1
            IDIAG(II)=NKLOC
            ITK(1,NKLOC)=CDDLP(II)
            ITK(2,NKLOC)=CDDLP(II)
            RTK(NKLOC)=DIAG_K(I)
         ELSE
            NKFRONT=NKFRONT+1
            IDIAG(II)=NKFRONT
            ITKFRONT(1,NKFRONT)=CDDLP(II)
            ITKFRONT(2,NKFRONT)=CDDLP(II)
            RTKFRONT(NKFRONT)=DIAG_K(I)
         ENDIF
C
         DO J=IADK(I),IADK(I+1)-1
            ILOC=1
            JJ=IACTI(JDIK(J))
            IF (JJ==0) CYCLE
            IF (IDDL_FRONT(1,II)==1.OR.IDDL_FRONT(1,JJ)==1) THEN
               ILOC=0
            ELSE
               DO K=1,NSPMD
                  ITAG(1,K)=0
                  ITAG(2,K)=0
               ENDDO
               DO K=1,IDDL_FRONT(1,II)
                  KK=IDDL_FRONT(1+K,II)
                  ITAG(1,KK)=1
               ENDDO
               DO K=1,IDDL_FRONT(1,JJ)
                  KK=IDDL_FRONT(1+K,JJ)
                  ITAG(2,KK)=1
               ENDDO
               INDEX=0
               DO K=1,NSPMD
                  INDEX=INDEX+ITAG(1,K)*ITAG(2,K)
               ENDDO
               IF (INDEX==1) ILOC=0
            ENDIF
C
            IF (ILOC==0) THEN
               NKLOC=NKLOC+1
               ITK(1,NKLOC)=CDDLP(II)
               ITK(2,NKLOC)=CDDLP(JJ)
               RTK(NKLOC)=LT_K(J)
            ELSEIF (ILOC==1) THEN
               NKFRONT=NKFRONT+1
               ITKFRONT(1,NKFRONT)=CDDLP(II)
               ITKFRONT(2,NKFRONT)=CDDLP(JJ)
               RTKFRONT(NKFRONT)=LT_K(J)
            ENDIF
         ENDDO
      ENDDO
      IF (NDDLI>0) THEN
C Matrice de rigidite d'interface
         DO N=1,NDDLI
            I=ITOK(N)
            IF (IACTI(I)==0) CYCLE
            II=IACTI(I)
            IF (IDDL_FRONT(1,II)==1) THEN
               J=IDIAG(II)
               RTK(J)=RTK(J)+DIAG_I(N)
            ELSE
               J=IDIAG(II)
               RTKFRONT(J)=RTKFRONT(J)+DIAG_I(N)
            ENDIF
C
            DO J=IADI(N),IADI(N+1)-1
               ILOC=1
               JJ=ITOK(JDII(J))
               JJ=IACTI(JJ)
               IF (JJ==0) CYCLE
               IF (IDDL_FRONT(1,II)==1.OR.IDDL_FRONT(1,JJ)==1) THEN
                  ILOC=0
               ELSE
                  DO K=1,NSPMD
                     ITAG(1,K)=0
                     ITAG(2,K)=0
                  ENDDO
                  DO K=1,IDDL_FRONT(1,II)
                     KK=IDDL_FRONT(1+K,II)
                     ITAG(1,KK)=1
                  ENDDO
                  DO K=1,IDDL_FRONT(1,JJ)
                     KK=IDDL_FRONT(1+K,JJ)
                     ITAG(2,KK)=1
                  ENDDO
                  INDEX=0
                  DO K=1,NSPMD
                     INDEX=INDEX+ITAG(1,K)*ITAG(2,K)
                  ENDDO
                  IF (INDEX==1) ILOC=0
               ENDIF
C
               IF (ILOC==0) THEN
                  IFOUND=0
                  K=IADL(II)
                  DO WHILE (IFOUND==0.AND.K<=IADL(II+1)-1)
                     IF (CDDLP(II)==ITK(1,K)
     .              .AND.CDDLP(JJ)==ITK(2,K)) IFOUND=K
                     K=K+1
                  ENDDO
                  IF (IFOUND/=0) THEN
                     RTK(IFOUND)=RTK(IFOUND)+LT_I(J)
                  ELSE
                     NKLOC=NKLOC+1
                     ITK(1,NKLOC)=CDDLP(II)
                     ITK(2,NKLOC)=CDDLP(JJ)
                     RTK(NKLOC)=LT_I(J)
                  ENDIF
               ELSEIF (ILOC==1) THEN
                  IFOUND=0
                  K=IADLFRONT(II)
                  DO WHILE (IFOUND==0.AND.K<=IADLFRONT(II+1)-1)
                     IF (CDDLP(II)==ITKFRONT(1,K)
     .              .AND.CDDLP(JJ)==ITKFRONT(2,K)) IFOUND=K
                     K=K+1
                  ENDDO
                  IF (IFOUND/=0) THEN
                     RTKFRONT(IFOUND)=RTKFRONT(IFOUND)+LT_I(J)
                  ELSE
                     NKFRONT=NKFRONT+1
                     ITKFRONT(1,NKFRONT)=CDDLP(II)
                     ITKFRONT(2,NKFRONT)=CDDLP(JJ)
                     RTKFRONT(NKFRONT)=LT_I(J)
                  ENDIF
               ENDIF
            ENDDO 
         ENDDO
      ENDIF
C Complement de la matrice de rigidite d'interface pour secnds remote     
      DO I=1,NDDL_SL
        II=IDDL_SL(I)
         DO J=IAD_SS(I),IAD_SS(I+1)-1
               ILOC=1
            JJ=JDI_SL(J)
               IF (IDDL_FRONT(1,II)==1.OR.IDDL_FRONT(1,JJ)==1) THEN
                  ILOC=0
               ELSE
                  DO K=1,NSPMD
                     ITAG(1,K)=0
                     ITAG(2,K)=0
                  ENDDO
                  DO K=1,IDDL_FRONT(1,II)
                     KK=IDDL_FRONT(1+K,II)
                     ITAG(1,KK)=1
                  ENDDO
                  DO K=1,IDDL_FRONT(1,JJ)
                     KK=IDDL_FRONT(1+K,JJ)
                     ITAG(2,KK)=1
                  ENDDO
                  INDEX=0
                  DO K=1,NSPMD
                     INDEX=INDEX+ITAG(1,K)*ITAG(2,K)
                  ENDDO
                  IF (INDEX==1) ILOC=0
               ENDIF
            IF (ILOC==0) THEN
               NKLOC=NKLOC+1
               ITK(1,NKLOC)=CDDLP(II)
               ITK(2,NKLOC)=CDDLP(JJ)
               RTK(NKLOC)=LT_SL(J)
            ELSEIF (ILOC==1) THEN
               NKFRONT=NKFRONT+1
               ITKFRONT(1,NKFRONT)=CDDLP(II)
               ITKFRONT(2,NKFRONT)=CDDLP(JJ)
               RTKFRONT(NKFRONT)=LT_SL(J)
            ENDIF
         ENDDO
      ENDDO
C----- il manque DIAG_SL--------
      DO N=1,NDDL_SL
       I=IDDL_SL(N)
       IF (IACTI(I)==0) CYCLE
       II=IACTI(I)
       J=IDIAG(II)
       IF (IDDL_FRONT(1,II)==1) THEN
         RTK(J)=RTK(J)+DIAG_SL(N)
       ELSE
         RTKFRONT(J)=RTKFRONT(J)+DIAG_SL(N)
        ENDIF
      ENDDO
      IF ((NDDL_SI+NDDL_SL)>0) THEN
       CALL SPMD_IFRI(CDDLP, CDDLP_REM)
       DO I=1,NDDL
         IADL(I) = IDDL_FRONT(1,I)
       ENDDO
       CALL SPMD_IFRI(IADL, IDDL_REM)
      ENDIF 
      DO I=1,NDDL_SI
         DO J=IAD_SI(I),IAD_SI(I+1)-1
           JJ=JDI_SI(J)
           IF (IDDL_FRONT(1,JJ)==1.OR.IDDL_REM(I)==1) THEN
            NKLOC=NKLOC+1
            ITK(1,NKLOC)=CDDLP_REM(I)
            ITK(2,NKLOC)=CDDLP(JJ)
            RTK(NKLOC)=LT_SI(J)
           ELSE
            NKFRONT=NKFRONT+1
            ITKFRONT(1,NKFRONT)=CDDLP_REM(I)
            ITKFRONT(2,NKFRONT)=CDDLP(JJ)
            RTKFRONT(NKFRONT)=LT_SI(J)
           ENDIF
         ENDDO
      ENDDO
C
      DO I=1,NKFRONT
         ITK(1,NKLOC+I)=ITKFRONT(1,I)
         ITK(2,NKLOC+I)=ITKFRONT(2,I)
         RTK(NKLOC+I)=RTKFRONT(I)
      ENDDO
C
      DEALLOCATE(ITKFRONT, RTKFRONT)
C
      RETURN
      END


Chd|====================================================================
Chd|  MUMPS_SET2                    source/implicit/imp_mumps.F   
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_MUMPS1                    source/implicit/imp_mumps.F   
Chd|-- calls ---------------
Chd|        SPMD_IFRI                     source/mpi/implicit/imp_spmd.F
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE MUMPS_SET2(IADK , JDIK    , DIAG_K , LT_K  , CDDLP,
     .                     NKLOC, NKFRONT , ITK    , RTK   , IDDL ,
     .                     INLOC, IAD_ELEM, FR_ELEM, NDOF  , IKC  ,
     .                     NDDL , NNZK    , IACTI  , NDDLI , NNZI ,
     .                     IADI , JDII    , ITOK   , DIAG_I, LT_I )     
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "com01_c.inc"
#include "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IADK(*), JDIK(*), CDDLP(*), NKLOC, NKFRONT, ITK(2,*),
     .        IDDL(*), INLOC(*), IAD_ELEM(2,*), FR_ELEM(*), NDOF(*),
     .        IKC(*), NDDL, NNZK, IACTI(*), NDDLI, NNZI, IADI(*),
     .        JDII(*), ITOK(*)
      my_real
     .        DIAG_K(*), LT_K(*), RTK(*), DIAG_I(*), LT_I(*)
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NKC, N, 
     .        J, ND, NOD, INOD, KK, K, ID, NN, ILOC, JJ,
     .        ITAG(2,NSPMD), INDEX, II, IDIAG(NDDL), IADL(NDDL+1),
     .        IFOUND, CDDLP_REM(NDDL_SI),
     .        IDDL_REM(NDDL_SI)
      NKC=0
      DO N=1,NUMNOD
         I=INLOC(N)
         DO J=1,NDOF(I)
            ND=IDDL(I)+J
            ID=ND-NKC
            IF (IKC(ND)>=1) NKC=NKC+1
         ENDDO
      ENDDO

      NKLOC=0
      DO I=1,NDDL
         IF (IACTI(I)==0) CYCLE
         II=IACTI(I)
         IADL(II)=NKLOC+1
         NKLOC=NKLOC+1
         IDIAG(II)=NKLOC
         ITK(1,NKLOC)=CDDLP(II)
         ITK(2,NKLOC)=CDDLP(II)
         RTK(NKLOC)=DIAG_K(I)
         DO J=IADK(I),IADK(I+1)-1
            ILOC=1
            JJ=IACTI(JDIK(J))
            IF (JJ==0) CYCLE
            NKLOC=NKLOC+1
            ITK(1,NKLOC)=CDDLP(II)
            ITK(2,NKLOC)=CDDLP(JJ)
            RTK(NKLOC)=LT_K(J)
         ENDDO
      ENDDO
      IADL(NDDL+1) = NKLOC+1
      IF (NDDLI>0) THEN
C Matrice de rigidite d'interface
         DO N=1,NDDLI
            I=ITOK(N)
            IF (IACTI(I)==0) CYCLE
            II=IACTI(I)
            J=IDIAG(II)
            RTK(J)=RTK(J)+DIAG_I(N)
            DO J=IADI(N),IADI(N+1)-1
               ILOC=1
               JJ=ITOK(JDII(J))
               JJ=IACTI(JJ)
               IF (JJ==0) CYCLE
               IFOUND=0
               K=IADL(II)
               DO WHILE (IFOUND==0.AND.K<=IADL(II+1)-1)
                  IF (CDDLP(II)==ITK(1,K)
     .           .AND.CDDLP(JJ)==ITK(2,K)) IFOUND=K
                  K=K+1
               ENDDO
               IF (IFOUND/=0) THEN
                  RTK(IFOUND)=RTK(IFOUND)+LT_I(J)
               ELSE
                  NKLOC=NKLOC+1
                  ITK(1,NKLOC)=CDDLP(II)
                  ITK(2,NKLOC)=CDDLP(JJ)
                  RTK(NKLOC)=LT_I(J)
               ENDIF
            ENDDO 
         ENDDO
      ENDIF
C Complement de la matrice de rigidite d'interface pour secnds remote     
      DO I=1,NDDL_SL
        II=IDDL_SL(I)
         DO J=IAD_SS(I),IAD_SS(I+1)-1
           ILOC=1
           JJ=JDI_SL(J)
           NKLOC=NKLOC+1
           ITK(1,NKLOC)=CDDLP(II)
           ITK(2,NKLOC)=CDDLP(JJ)
           RTK(NKLOC)=LT_SL(J)
         ENDDO
      ENDDO
C----- il manque DIAG_SL--------
      DO N=1,NDDL_SL
       I=IDDL_SL(N)
       IF (IACTI(I)==0) CYCLE
       II=IACTI(I)
       J=IDIAG(II)
       RTK(J)=RTK(J)+DIAG_SL(N)
      ENDDO
      IF ((NDDL_SI+NDDL_SL)>0) THEN
       CALL SPMD_IFRI(CDDLP, CDDLP_REM)
      ENDIF 
      DO I=1,NDDL_SI
         DO J=IAD_SI(I),IAD_SI(I+1)-1
           JJ=JDI_SI(J)
           NKLOC=NKLOC+1
           ITK(1,NKLOC)=CDDLP_REM(I)
           ITK(2,NKLOC)=CDDLP(JJ)
           RTK(NKLOC)=LT_SI(J)
         ENDDO
      ENDDO
C
      RETURN
      END
#endif
