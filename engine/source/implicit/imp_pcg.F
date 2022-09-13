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
C-------------------------
Chd|====================================================================
Chd|  CRIT_STOP                     source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PCG1                      source/implicit/imp_fsa_inv.F 
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|        IMP_PPCGH                     source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|====================================================================
      INTEGER FUNCTION CRIT_STOP(IT,R2,IT_MAX,TOL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IT,IT_MAX  
C     REAL
      my_real
     .  R2,TOL 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C-----------------------------
      IF (IT>=IT_MAX) THEN
       CRIT_STOP = 0
       RETURN
      ENDIF
      IF (R2<=TOL) THEN
       CRIT_STOP = 0
      ELSE
       CRIT_STOP = 1
      ENDIF
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  IMP_ISAVE                     source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IMP_ISAVE(
     1   NDDL,X,R,DIAG_K,DIAG_M,
     2   NNZ,LT_K,LT_K0,LT_M,LT_M0,
     3   IADK,JDIK,IADK0,JDIK0,IADM,
     4   JDIM,IADM0,JDIM0,NNZM,TOLS,
     5   NLIM,itol,EPS_M,IPREC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .        X(*), R(*), TOLS, EPS_M,
     .        DIAG_K(*), DIAG_M(*),
     .        LT_K(*), LT_K0(*), LT_M(*), LT_M0(*)
      INTEGER NDDL, NNZ, NNZM, NLIM, ITOL, IPREC,
     .        IADK(*), JDIK(*), IADK0(*), JDIK0(*),
     .        IADM(*), JDIM(*), IADM0(*), JDIM0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      OPEN(66,file="input_mat",form="unformatted")
      WRITE(66)NDDL,NNZ,NNZM,NLIM,ITOL,IPREC
      WRITE(66)X(1:NDDL)
      WRITE(66)R(1:NDDL)
      WRITE(66)DIAG_K(1:NDDL)
      WRITE(66)DIAG_M(1:NDDL)
      WRITE(66)LT_K(1:NNZ)
      WRITE(66)LT_K0(1:NNZ)
      WRITE(66)LT_M(1:NNZM)
      WRITE(66)LT_M0(1:NNZM)
      WRITE(66)IADK(1:NDDL+1)
      WRITE(66)JDIK(1:IADK(NDDL+1)-1)
      WRITE(66)IADK0(1:NDDL+1)
      WRITE(66)JDIK0(1:IADK0(NDDL+1)-1)
      WRITE(66)IADM(1:NDDL+1)
      WRITE(66)JDIM(1:IADM(NDDL+1)-1)
      WRITE(66)IADM0(1:NDDL+1)
      WRITE(66)JDIM0(1:IADM0(NDDL+1)-1)
      WRITE(66)EPS_M,TOLS
      RETURN
      END
      
Chd|====================================================================
Chd|  IMP_ISAVE2                    source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IMP_ISAVE2(
     1   NDDL,X,DIAG_K,NNZ,LT_K,
     3   IADK,JDIK,LT_K0,IADK0,JDIK0)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .        X(*), 
     .        DIAG_K(*), 
     .        LT_K(*),LT_K0(*)
      INTEGER NDDL, NNZ, 
     .        IADK(*), JDIK(*),IADK0(*), JDIK0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K
      
      OPEN(66,file="input_matA")
      WRITE(66,*)NDDL,NDDL,NNZ+NDDL
      DO I = 1, NDDL
        DO K = IADK(I), IADK(I+1)-1
          J=JDIK(K)
          IF(J < I) THEN
            WRITE(66,*)I,J,LT_K(K)
          else
            print*,'error1'
          END IF
        END DO
        WRITE(66,*)I,I,DIAG_K(I)
        DO K = IADK0(I), IADK0(I+1)-1
          J=JDIK0(K)
          IF(J > I) THEN
            WRITE(66,*)I,J,LT_K0(K)
          else
            print*,'error2'
          END IF
        END DO
      END DO
      OPEN(67,file="input_vecX")
      WRITE(67,*)NDDL
      DO I = 1, NDDL
        WRITE(67,*)X(I)
      END DO
      RETURN
      END
      
Chd|====================================================================
Chd|  IMP_RSAVE                     source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE IMP_RSAVE(
     1   NDDL,X,R)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .        X(*), R(*)
      INTEGER NDDL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
c      WRITE(66,*)X(1:NDDL)
c      WRITE(66,*)R(1:NDDL)
      WRITE(66)X(1:NDDL)
      WRITE(66)R(1:NDDL)
      CLOSE(66)
      RETURN
      END

#if defined knf

!dec$ attributes offload: mic :: MIC_DCOPY
Chd|====================================================================
Chd|  MIC_DCOPY                     source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MIC_DCOPY(n,x,y)   !Y=X
      INTEGER n
      REAL *8 x(*),y(*)
!$OMP do
CAD not vectorized
      do i=1,n
      y(i)=x(i)
      enddo
      RETURN
      END
!dec$ attributes offload: mic :: MIC_DSCAL
Chd|====================================================================
Chd|  MIC_DSCAL                     source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MIC_DSCAL(n,scal,x)  !X=SCAL*X
      INTEGER n
      REAL *8 scal,x(*)
!$OMP DO
      do i=1,n
      x(i)=x(i)*scal
      enddo
      RETURN
      END
!dec$ attributes offload: mic :: MIC_DDOT
Chd|====================================================================
Chd|  MIC_DDOT                      source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MIC_DDOT(n,x,y,scal)  ! SCAL=DDOT(X,Y,N)
      INTEGER n
      REAL *8 scal,x(*),y(*)
      scal=0.
!$OMP BARRIER
!$OMP DO REDUCTION(+:scal)
      do i=1,n
      scal=scal+x(i)*y(i)
      enddo
      RETURN
      END
!dec$ attributes offload: mic ::MIC_DAXPY
Chd|====================================================================
Chd|  MIC_DAXPY                     source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MIC_DAXPY(n,scal,x,y)    !Y=Y+A*X
      INTEGER n
      REAL *8 scal,x(*),y(*)
!$OMP DO
      do i=1,n
      y(i)=y(i)+scal*x(i)
      enddo
      RETURN
      END
!dec$ attributes offload: mic ::MIC_MV
Chd|====================================================================
Chd|  MIC_MV                        source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MIC_MV(n,x,diag,y)   !Y=DIAG*X
      INTEGER n
      REAL *8 scal,diag(*),x(*),y(*)
!$OMP DO
      do i=1,n
      y(i)=diag(i)*x(i)
      enddo
      RETURN
      END
!dec$ attributes offload: mic :: MIC_SPMV
cad!dec$ attributes offload: mic :: mkl_dcsrgemv
Chd|====================================================================
Chd|  MIC_SPMV                      source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MIC_SPMV(n,y,x,mat,indl,indc)   !Y=Y+MAT*X (MAT SPARSE)
      INTEGER n,indl(*),indc(*),ideb,ifin,npack,lpack
      REAL *8 scal,x(*),y(*),mat(*)

cad!$OMP SINGLE
cad      call mkl_dcsrgemv('N',n,mat,indl,indc,x,y)
cad!$OMP END SINGLE
cad      return

!$OMP DO schedule(dynamic,512)
!dir$ noprefetch
      do i=1,n
      ideb=indl(i)
      ifin=indl(i+1)-1
!dir$ noprefetch
!dir$ novector
      do j=ideb,ifin
      k=indc(j)
      y(i)=y(i)+ mat(j)*x(k)
      enddo
      enddo

      RETURN
      END

!dec$ attributes offload: mic ::MIC_GATHER
Chd|====================================================================
Chd|  MIC_GATHER                    source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MIC_GATHER(n,lcom,x,y,ind)
      INTEGER n, lcom, ind(*)
      REAL *8 x(*),y(*)
!$OMP DO
      do i=1,lcom
      y(i)=x(ind(i))
      enddo
      RETURN
      END

!dec$ attributes offload: mic ::MIC_SCATTER
Chd|====================================================================
Chd|  MIC_SCATTER                   source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MIC_SCATTER(n,lcom,x,y,ind,flag)
      INTEGER n, lcom, ind(*), flag(*)
      REAL *8 x(*),y(*)
!$OMP DO
      do i=1,lcom
        if(flag(i)==0) x(ind(i))=x(ind(i))+y(i)
      enddo
      RETURN
      END

Chd|====================================================================
Chd|  MIC_INIT                      source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MIC_INIT(NSPMD,LOC_PROC,L_SPMD,IDEVICE,MICID,IERR)
      USE MIC_LIB
      INTEGER NSPMD, LOC_PROC, L_SPMD, IDEVICE, MICID, IERR
      INTEGER NBDEVICE,DEV
      IF (IDEVICE == 0) THEN
        DEV=L_SPMD+1
      ELSE
        DEV=IDEVICE
      ENDIF
      NBDEVICE = OFFLOAD_NUMBER_OF_DEVICES()
      IF(NBDEVICE < DEV)THEN
        IF(NBDEVICE == 0)THEN
          print *,'Intel MIC device not found'
          IERR=-1
        ELSE
          IF(IDEVICE == 0) THEN
            print *,'Not enough devices for SPMD domain number',loc_proc
            IERR=-2
          ELSE
            print *,'Device number',DEV,'not found'
            IERR=-3
          END IF
        END IF
      ELSE
        IERR=0
        MICID=DEV-1
      END IF

      RETURN
      END

Chd|====================================================================
Chd|  MIC_PCG                       source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|====================================================================
      MODULE MIC_PCG
!dec$ attributes offload:mic :: W, VGAT, VSCA
!dec$ attributes offload:mic :: INDEX
      my_real, DIMENSION(:),ALLOCATABLE :: W, VGAT, VSCA
      INTEGER, DIMENSION(:),ALLOCATABLE :: INDEX
      END MODULE MIC_PCG
#else
C CMake issue : Module needs to buildable
Chd|====================================================================
Chd|  MIC_PCG                       source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|====================================================================
      MODULE MIC_PCG
      my_real, DIMENSION(:),ALLOCATABLE :: W, VGAT, VSCA
      INTEGER, DIMENSION(:),ALLOCATABLE :: INDEX
      END MODULE MIC_PCG
#endif

#ifdef MUMPS5
Chd|====================================================================
Chd|  IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        LIN_SOLVH0                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVH1                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVHM                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVIH2                   source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        IMP_ISAVE                     source/implicit/imp_pcg.F     
Chd|        IMP_ISAVE2                    source/implicit/imp_pcg.F     
Chd|        IMP_RSAVE                     source/implicit/imp_pcg.F     
Chd|        MAV_LTH                       source/implicit/produt_v.F    
Chd|        MIC_DAXPY                     source/implicit/imp_pcg.F     
Chd|        MIC_DCOPY                     source/implicit/imp_pcg.F     
Chd|        MIC_DDOT                      source/implicit/imp_pcg.F     
Chd|        MIC_DSCAL                     source/implicit/imp_pcg.F     
Chd|        MIC_GATHER                    source/implicit/imp_pcg.F     
Chd|        MIC_INIT                      source/implicit/imp_pcg.F     
Chd|        MIC_MV                        source/implicit/imp_pcg.F     
Chd|        MIC_SCATTER                   source/implicit/imp_pcg.F     
Chd|        MIC_SPMV                      source/implicit/imp_pcg.F     
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        NLIM_MIX                      source/implicit/imp_pcg.F     
Chd|        PREC_SOLVH                    source/implicit/prec_solv.F   
Chd|        PRODUT_H                      source/implicit/produt_v.F    
Chd|        PUPD                          source/implicit/imp_pcg.F     
Chd|        SPMD_SUMFC_V                  source/mpi/implicit/imp_spmd.F
Chd|        SPMD_SUM_S                    source/mpi/implicit/imp_spmd.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        CRIT_STOP                     source/implicit/imp_pcg.F     
Chd|        MIXEDSOL                      source/implicit/imp_pcg.F     
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        IMP_WORKH                     share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MIC_PCG                       source/implicit/imp_pcg.F     
Chd|====================================================================
      SUBROUTINE IMP_PCGH( IPREC ,
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,NDDLI ,ITOK  ,IADI  ,JDII   ,
     3                    LT_I  ,NNZM  ,IADM  ,JDIM  ,DIAG_M ,
     4                    LT_M  ,X     ,R     ,ITOL  ,TOL    ,
     5                    P     ,Z     ,Y     ,ITASK ,IPRINT ,
     6                    N_MAX ,EPS_M ,F_X   ,ISTOP ,W_DDL ,
     8                    A     ,AR    ,VE    ,MS    ,XE    ,
     9                    D     ,DR    ,NDOF  ,IPARI ,INTBUF_TAB,
     A                    NUM_IMP,NS_IMP,NE_IMP,NSREM ,
     B                    NSL   ,NMONV  ,IMONV ,MONVOL,IGRSURF ,
     C                    VOLMON,FR_MV ,IBFV  ,SKEW  ,
     D                    XFRAME ,GRAPHE,IAD_ELEM,FR_ELEM,ITAB , 
     E                    INSOLV ,ITN   ,FAC_K ,IPIV_K,NK    ,
     F                    MUMPS_PAR,CDDLP,ISOLV,IDSC  ,IDDL  ,
     G                    IKC    ,INLOC ,IND_IMP,XI_C ,R0    ,
     H                    NDDLI_G,INTP_C,IRBE3 ,LRBE3 ,IRBE2 ,
     I                    LRBE2  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
      USE IMP_WORKH
      USE IMP_FRK
      USE IMP_INTM
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
#if defined knf
      USE MIC_LIB
      USE OMP_LIB
      USE MIC_PCG
#endif
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "dmumps_struc.h"
#include      "timeri_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C----------resol [K]{X}={F}---------
      INTEGER  NDDL  ,NNZ   ,ITOL,
     .         NDDLI ,ITOK(*) ,IADI(*),JDII(*),N_MAX,
     .         NNZM  ,IPREC,ITASK,IPRINT,
     .         ISTOP,W_DDL(*),IBFV(*),INTP_C,IRBE3(*) ,LRBE3(*),
     .         IRBE2(*) ,LRBE2(*)
      INTEGER  NDOF(*),NE_IMP(*),NSREM ,NSL,
     .         IPARI(*) ,NUM_IMP(*),NS_IMP(*) ,IND_IMP(*)
      INTEGER NMONV,IMONV(*),MONVOL(*),FR_MV(*)
      INTEGER IAD_ELEM(2,*), FR_ELEM(*), ITAB(*),  
     .        INSOLV, ITN, IPIV_K(*), NK, CDDLP(*), ISOLV, IDSC,
     .        IDDL(*), IKC(*), INLOC(*),NDDLI_G, MICID
C     REAL
      my_real
     .  LT_I(*), TOL, EPS_M, F_X, XI_C(*)
#if defined knf
      INTEGER    , target, intent(inout)  ::
     .  IADK(NDDL+1)  ,JDIK(NNZ), IADM(NDDL+1), JDIM(NNZM)
      my_real    , target, intent(inout)  ::
     .  DIAG_K(NDDL), LT_K(NNZ) ,DIAG_M(NDDL),LT_M(NNZM) ,X(NDDL),
     .  P(NDDL) ,Z(NDDL)  ,R(NDDL)  ,Y(NDDL)
#elif 1
      INTEGER 
     .  IADK(*)  ,JDIK(*), IADM(*),JDIM(*)
      my_real
     .  DIAG_K(*), LT_K(*) ,DIAG_M(*),LT_M(*) ,X(*),
     .  P(*) ,Z(*)  ,R(*)  ,Y(*)
#endif
      my_real
     .  A(3,*),AR(3,*),VE(3,*),D(3,*),DR(3,*),XE(3,*),
     .  MS(*),VOLMON(*),SKEW(*) ,XFRAME(*),FAC_K(*),
     .  R0(*)
      TYPE(PRGRAPH) :: GRAPHE(*)
      TYPE(DMUMPS_STRUC) MUMPS_PAR

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL MIXEDSOL
      EXTERNAL MIXEDSOL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IT,IP,NLIM,ND,IERR,IPRI,IERROR,NNZI,LOC_PROC,
     .        ICK,ISAVE,N, TB
      PARAMETER (ND=4)
      CHARACTER*4       EXIT
      CHARACTER*11       WARR
      my_real
     .   S , R2, R02,ALPHA,BETA,G0,G1,RR,TOLS,TOLN,TOLS2
      INTEGER CRIT_STOP,IUPD,IUPD0,F_DDL,L_DDL,IFLG,GPUR4R8,
     .        ITP,LCOM,NDDL1,LCOMI,LCOMX,K,
     .        NTAG(NDDL),IGATHER(NDDL)
      EXTERNAL CRIT_STOP

      my_real
     .  ANORM2,XNORM2,L_A,L_B2,L_B,A_OLD,B_OLD,TMP,R2_OLD,RMAX
      my_real
     .  CS,DBAR, DELTA, DENOM, KCOND,SNPROD,QRNORM,
     .  GAMMA, GBAR, GMAX, GMIN, EPSLN,LQNORM,DIAG,CGNORM,
     .  OLDB, RHS1, RHS2,SN, ZBAR, ZL ,OLDB2,TNORM2,EPS(4)
      DOUBLE PRECISION
     .  TT, TF
C--------------GPU SPECIFIC--------------------------
      REAL*4, DIMENSION(:),ALLOCATABLE ::
     .  LT_K_SP, LT_K0_SP, LT_M_SP, LT_M0_SP, DIAG_K_SP, DIAG_M_SP,
     .  X_SP, R_SP,W_SP, LT_I0_SP
#if defined knf
      my_real, DIMENSION(:),ALLOCATABLE :: WR
      INTEGER, DIMENSION(:),ALLOCATABLE :: IFRTMP, TABLE, INDTMP
#elif 1
      my_real, DIMENSION(:),ALLOCATABLE :: W, WR, VGAT, VSCA, XIR, YIR
      INTEGER, DIMENSION(:),ALLOCATABLE :: IFRTMP, INDEX, TABLE, INDTMP
#endif

c#if defined knf
c!dec$ attributes offload: mic ::  
c     &         M_IADK, M_IADM, M_JDIM, M_JDIK
c      INTEGER, pointer, save, dimension(:)  :: 
c     .         M_IADK, M_IADM, M_JDIM, M_JDIK
c!dec$ attributes offload: mic ::  
c     &         M_LT_K, M_LT_M, M_DIAG_K, M_DIAG_M, M_P,
c     &         M_X, M_R, M_Y, M_Z
c      DOUBLE PRECISION, pointer, save, dimension(:)  :: 
c     .         M_LT_K, M_LT_M, M_DIAG_K, M_DIAG_M, M_P,
c     .         M_X, M_R, M_Y, M_Z 
c#endif
C--------------END GPU SPECIFIC----------------------
      DATA               EXIT  /'WITH'/
      DATA               WARR  /'**WARRING**'/
C--------------INITIALISATION--------------------------
      ISAVE=0
C      ISAVE=1  ! sauvegarde input/res
C      ISAVE=2  ! sauvegarde input simple
#if defined knf
      IF(NDDLI > 0)THEN
        NNZI = IADI0(NDDLI+1)-IADI0(1)
      ELSE
        NNZI = 0
      END IF
C
      LCOM=0
      LCOMI=0
      LCOMX=0
      IF(NSPMD > 1)THEN
       DO I = 1, NSPMD
         LCOM = LCOM + ND_FR(I)
       END DO
       IF(NDDLI_G > 0)THEN
        NTAG(1:NDDL)=0
        DO I = 1, NSPMD
          DO J = IAD_SL(I),IAD_SL(I+1)-1
            K=IDDL_SL(J)
            IF(NTAG(K)==0)THEN
              LCOMI=LCOMI+1
              NTAG(K)=LCOMI
              IGATHER(LCOMI)=K-1
            ENDIF
          ENDDO 
        END DO
        DO I = 1, NSPMD
          LCOMX=LCOMX+F_DDL+IAD_SREM(I+1)-IAD_SREM(I)+1 
        END DO
        DO I=1,NDDL_SI
          DO J =IAD_SI(I),IAD_SI(I+1)-1
            K =JDI_SI(J)
            IF(NTAG(K)==0)THEN
              LCOMI=LCOMI+1
              NTAG(K)=LCOMI
              IGATHER(LCOMI)=K-1
            ENDIF
          ENDDO
        ENDDO
C
        DO I=1,NDDL_SL
          K = IDDL_SL(I)
          IF(NTAG(K)==0)THEN
            LCOMI=LCOMI+1
            NTAG(K)=LCOMI
            IGATHER(LCOMI)=K-1
          ENDIF
C
          DO J =IAD_SS(I),IAD_SS(I+1)-1
            K =JDI_SL(J)
            IF(NTAG(K)==0)THEN
              LCOMI=LCOMI+1
              NTAG(K)=LCOMI
              IGATHER(LCOMI)=K-1
            ENDIF
          ENDDO
        ENDDO
       END IF

C adding stop test in case of SPMD and null domain => bad because need to communicate V
C       IF (NDDL==0 .AND. NNZ==0 .AND. LCOM==0 .AND. LCOMI==0
C     .             .AND. LCOMX==0) RETURN
      END IF
#endif
      LOC_PROC=ISPMD+1
C-----Istop=1 : NIT>=N_LIM+ RR>TOL
C------X(I)=ZERO----******N_MAX,EPS_M a calculer avant----
      F_DDL=1+ITASK*NDDL/NTHREAD
      L_DDL=(ITASK+1)*NDDL/NTHREAD
C
      ISTOP=0 
      NLIM=N_MAX
      CALL NLIM_MIX(N_MAX,NDDLI_G,NLIM)
      NLIM=MAX(NLIM,2)
      TOLS=MAX(TOL,EPS_M)
      IPRI = IPRINT
      IF (ISPMD/=0) IPRI = 0
C--------
C routine speciale pour sauvegarder les input
      IF(ISAVE==1)CALL IMP_ISAVE(
     1   NDDL,X,R,DIAG_K,DIAG_M,
     2   NNZ,LT_K,LT_K0,LT_M,LT_M0,
     3   IADK,JDIK,IADK0,JDIK0,IADM,
     4   JDIM,IADM0,JDIM0,NNZM,TOLS,
     5   NLIM,itol,EPS_M,IPREC)
      IF(ISAVE==2)CALL IMP_ISAVE2(
     1   NDDL,R,DIAG_K,NNZ,LT_K,
     3   IADK,JDIK,LT_K0,IADK0,JDIK0)   
C--------
      IF (ITASK == 0) THEN
       IF(IPRI/=0)THEN
        WRITE(IOUT,*)'     *** BEGIN CONJUGATE GRADIENT ITERATION ***'
        WRITE(IOUT,*)
       ENDIF
       IF(IPRI<0)THEN
        WRITE(ISTDO,*)'     *** BEGIN CONJUGATE GRADIENT ITERATION ***'
        WRITE(ISTDO,*)
       ENDIF
      END IF !(ITASK == 0) THEN
      IP = IABS(IPRI)
      IT=0
C
      IUPD = 0
      IUPD0 = 0
C----------------------
      CALL MY_BARRIER
C---------------------
C 1 : no gpu
C -1 : nvidia dp
C -3 : ocl dp
cc      NOGPU=-1
cc      IF(NOGPU==1)THEN
C variable to switch between double (GPUR4R8=2)or single (GPUR4R8=1)precision GPU
      GPUR4R8=2

#if  !defined knf

       CALL PREC_SOLVH(IPREC, ITASK   ,
     1                 GRAPHE,IAD_ELEM,FR_ELEM,DIAG_K,LT_K   , 
     2                 IADK  ,JDIK    ,ITAB   ,IPRI,INSOLV , 
     3                 ITN   ,FAC_K   , IPIV_K, NK   ,MUMPS_PAR,
     4                 CDDLP ,ISOLV   , IDSC  , IDDL ,IKC      , 
     5                 INLOC  ,NDOF   , NDDL  ,NNZM  ,IADM     , 
     6                 JDIM  ,DIAG_M  , LT_M  ,R    ,Z        ,
     7                 F_DDL ,L_DDL   )
C----------------------
       CALL MY_BARRIER
C---------------------
       DO I = F_DDL,L_DDL
        P(I) = Z(I)
       ENDDO
       CALL PRODUT_H(F_DDL  ,L_DDL ,R  ,Z  ,W_DDL , G0 ,ITASK )
C---------------------
       CALL MAV_LTH(
     1            NDDL  ,NDDLI ,IADK  ,JDIK  ,DIAG_K,   
     2            LT_K  ,IADI  ,JDII  ,ITOK  ,LT_I  ,
     3            P     ,Y     ,A     ,AR    ,
     5            VE    ,MS    ,XE    ,D     ,DR    ,
     6            NDOF  ,IPARI ,INTBUF_TAB   ,NUM_IMP,
     7            NS_IMP,NE_IMP,NSREM ,NSL   ,IBFV   ,
     8            SKEW  ,XFRAME,MONVOL,VOLMON,IGRSURF ,
     9            FR_MV,NMONV ,IMONV ,IND_IMP,
     A            XI_C  ,IUPD  ,IRBE3 ,LRBE3 ,IRBE2  ,
     B            LRBE2 ,F_DDL ,L_DDL ,ITASK )
C----------------------
       CALL MY_BARRIER
C---------------------
       CALL PRODUT_H(F_DDL  ,L_DDL ,P   ,Y  ,W_DDL , S ,ITASK )
C---------------------
       IF (G0==ZERO) THEN
        IF (ITOL>1) THEN
         ISTOP=-1
         GOTO 200
	ELSE
         ALPHA = ZERO
        ENDIF
       ELSE
        ALPHA = G0/S
       ENDIF
       TOLS2=TOLS*TOLS
       IF (ITOL==1) THEN
         CALL PRODUT_H( F_DDL  ,L_DDL ,R   ,R  ,W_DDL , R02 ,ITASK )
C---------------------
        R2 =R02
       ELSEIF (ITOL==3) THEN
C------ R2<TOL*TOL*ANORM2*XNORM2------
        R02=ABS(G0)
        R2 =R02
        L_A=ONE/ALPHA
C        L_B2=R02
        TNORM2=L_A*L_A
        ANORM2=ZERO
        XNORM2=ZERO
        A_OLD=L_A
        B_OLD=ZERO
        OLDB   = SQRT(R02)
       ELSEIF (ITOL==4) THEN
        R02=ALPHA*ALPHA*ABS(G0)
        EPS(1)=R02
        R2=R02
       ELSE
        R02=ABS(G0)
        R2 =R02
       ENDIF
       IF (R02==ZERO) THEN
        ISTOP=-1
        GOTO 200
       ENDIF
       TOLN=R02*TOLS2
       IF(IPRI/=0.AND.ITASK==0)THEN
        RR = SQRT(R2)
        IF(IPRI<0) WRITE(ISTDO,1000)IT,RR
        WRITE(IOUT,1000)IT,RR
       ENDIF
C-------pour etre coherent avec lanzos for linear
C----------------------
       CALL MY_BARRIER
C---------------------
       IT=1
       DO I=F_DDL,L_DDL
         X(I) = X(I) + ALPHA*P(I)
         R(I) = R(I) - ALPHA*Y(I)
       ENDDO 
C----------------------
       CALL MY_BARRIER
C---------------------
       CALL PREC_SOLVH(IPREC, ITASK ,
     1                 GRAPHE,IAD_ELEM,FR_ELEM,DIAG_K,LT_K   , 
     2                 IADK  ,JDIK    ,ITAB   ,IPRI,INSOLV , 
     3                 ITN   ,FAC_K   , IPIV_K, NK   ,MUMPS_PAR,
     4                 CDDLP ,ISOLV   , IDSC  , IDDL ,IKC      , 
     5                 INLOC  ,NDOF   , NDDL  ,NNZM  ,IADM    , 
     6                 JDIM  ,DIAG_M  , LT_M  ,R     ,Z       ,
     7                 F_DDL ,L_DDL   )
C----------------------
       CALL MY_BARRIER
C---------------------
       CALL PRODUT_H( F_DDL  ,L_DDL ,R   ,Z  ,W_DDL , G1 ,ITASK )
C---------------------
       BETA=G1/G0
       IF (ITOL==1) THEN
        CALL PRODUT_H( F_DDL  ,L_DDL ,R   ,R  ,W_DDL , R2 ,ITASK )
       ELSEIF (ITOL==3) THEN
C------ R2<TOL*TOL*ANORM2*XNORM2------
        R2=ABS(G1)
        L_B2=ABS(BETA)*A_OLD*A_OLD
        L_B=SQRT(L_B2)
        TNORM2=TNORM2+L_B2
        B_OLD=BETA
C*     INITIALIZE OTHER QUANTITIES.
        GBAR   = L_A
        DBAR   = L_B
        RHS1   = OLDB
        RHS2   = ZERO
        GMAX   = ABS( L_A ) + EPS_M
        GMIN   = GMAX
        OLDB2   = L_B2
        OLDB   = L_B
       ELSEIF (ITOL==4) THEN
        R2=R02
       ELSE
        R2=ABS(G1)
       ENDIF
       G0 = G1
       IF (ITOL==3) TOLN=TOLN*TNORM2
C----------------------
       CALL MY_BARRIER
C---------------------
#include "lockon.inc"
       ISTOP=CRIT_STOP(IT,R2,NLIM,TOLN)
#include "lockoff.inc"
       IF(NDDLI_G>0.AND.INTP_C==-1)IUPD0 = -INTP_C
       DO WHILE (ISTOP==1)
        DO I=F_DDL,L_DDL
         P(I) = Z(I) + BETA*P(I)
        ENDDO 
        IF(IUPD0>0.AND.IT>NDDLI_G/2) IUPD = IUPD0
C----------------------
        CALL MY_BARRIER
C---------------------
        CALL MAV_LTH(
     1            NDDL  ,NDDLI ,IADK  ,JDIK  ,DIAG_K,   
     2            LT_K  ,IADI  ,JDII  ,ITOK  ,LT_I  ,
     3            P     ,Y     ,A     ,AR    ,
     5            VE    ,MS    ,XE    ,D     ,DR    ,
     6            NDOF  ,IPARI ,INTBUF_TAB   ,NUM_IMP,
     7            NS_IMP,NE_IMP,NSREM ,NSL   ,IBFV   ,
     8            SKEW  ,XFRAME,MONVOL,VOLMON,IGRSURF ,
     9            FR_MV,NMONV ,IMONV ,IND_IMP,
     A            XI_C  ,IUPD  ,IRBE3 ,LRBE3 ,IRBE2  ,
     B            LRBE2 ,F_DDL ,L_DDL ,ITASK )
C----------------------
        CALL MY_BARRIER
C---------------------
        CALL PRODUT_H(F_DDL  ,L_DDL ,P   ,Y  ,W_DDL , S ,ITASK )
C---------------------
        ALPHA=G0/S
        DO I=F_DDL,L_DDL
         X(I) = X(I) + ALPHA*P(I)
         R(I) = R(I) - ALPHA*Y(I)
        ENDDO 
C----------------------
        CALL MY_BARRIER
C---------------------
        CALL PREC_SOLVH(IPREC, ITASK ,
     1                 GRAPHE,IAD_ELEM,FR_ELEM,DIAG_K,LT_K   , 
     2                 IADK  ,JDIK    ,ITAB   ,IPRI,INSOLV , 
     3                 ITN   ,FAC_K   , IPIV_K, NK   ,MUMPS_PAR,
     4                 CDDLP ,ISOLV   , IDSC  , IDDL ,IKC      , 
     5                 INLOC  ,NDOF   , NDDL  ,NNZM  ,IADM    , 
     6                 JDIM  ,DIAG_M  , LT_M  ,R     ,Z       ,
     7                 F_DDL ,L_DDL   )
C----------------------
        CALL MY_BARRIER
C---------------------
        CALL PRODUT_H(F_DDL  ,L_DDL ,R   ,Z  ,W_DDL , G1 ,ITASK )
C---------------------
        BETA=G1/G0
        G0 = G1
        IF (ITOL==1) THEN
         CALL PRODUT_H( F_DDL  ,L_DDL ,R   ,R  ,W_DDL , R2 ,ITASK )
        ELSEIF (ITOL==3) THEN
         R2 =ABS(G1)
         S=ONE/ALPHA
         L_A=S+B_OLD*A_OLD
C----------L_B2 : beta(j-1)^2-A_OLD :1/alpha(j-1)-------
         L_B2=ABS(BETA)*S*S
         L_B=SQRT(L_B2)
         A_OLD=S
         B_OLD=BETA
         ANORM2=TNORM2
         TNORM2=TNORM2+L_A*L_A+OLDB2+L_B2
         GAMMA  = SQRT( GBAR*GBAR + OLDB2 )
         CS     = GBAR / GAMMA
         SN     = OLDB / GAMMA
         DELTA  = CS * DBAR  +  SN * L_A
         GBAR   = SN * DBAR  -  CS * L_A
         EPSLN  = SN * L_B
         DBAR   =            -  CS * L_B
         ZL     = RHS1 / GAMMA
         XNORM2 = XNORM2+ZL*ZL
         GMAX   = MAX( GMAX, GAMMA )
         GMIN   = MIN( GMIN, GAMMA )
         RHS1   = RHS2  -  DELTA * ZL
         RHS2   =       -  EPSLN * ZL
         TOLN=TOLS2*ANORM2*XNORM2
         OLDB2   = L_B2
         OLDB   = L_B
        ELSEIF (ITOL==4) THEN
         TMP=ALPHA*ALPHA*ABS(G1)
         IF (IT>=ND) THEN
          DO J=1,ND-1
           EPS(J)=EPS(J+1)
          ENDDO
          EPS(ND)=TMP
          R2=ZERO
          DO J=1,ND
           R2=R2+EPS(J)
          ENDDO
         ELSE
          EPS(IT+1)=TMP
          R2=R2+TMP
         ENDIF
        ELSE
         R2 =ABS(G1)
        ENDIF
C        
        IF(IPRI/=0.AND.ITASK==0)THEN
         IF(MOD(IT,IP)==0)THEN
          RR = SQRT(R2/R02)
          WRITE(IOUT,1001)IT,RR
          IF(IPRI<0) WRITE(ISTDO,1001)IT,RR
         ENDIF
        ENDIF
C----------------------
        CALL MY_BARRIER
C---------------------
#include "lockon.inc"
        ISTOP=CRIT_STOP(IT,R2,NLIM,TOLN)
#include "lockoff.inc"

        IF((IUPD>0.AND.IT==NLIM).OR.
     .     (IUPD==0.AND.ISTOP/=1.AND.IUPD0>0))THEN
C------- re-iter with linear Kint------	
         IUPD0 = 0 
         IF(IUPD>0) IUPD = 0 
#include "lockon.inc"
         ISTOP = 1
#include "lockoff.inc"

         NLIM = NLIM + NLIM
         DO I=F_DDL,L_DDL
          X(I) = ZERO
         ENDDO 
C----------------------
         CALL MY_BARRIER
C---------------------
         CALL MAV_LTH(
     1            NDDL  ,NDDLI ,IADK  ,JDIK  ,DIAG_K,   
     2            LT_K  ,IADI  ,JDII  ,ITOK  ,LT_I  ,
     3            X     ,Z     ,A     ,AR    ,
     5            VE    ,MS    ,XE    ,D     ,DR    ,
     6            NDOF  ,IPARI ,INTBUF_TAB   ,NUM_IMP,
     7            NS_IMP,NE_IMP,NSREM ,NSL   ,IBFV   ,
     8            SKEW  ,XFRAME,MONVOL,VOLMON,IGRSURF ,
     9            FR_MV,NMONV ,IMONV ,IND_IMP,
     A            XI_C  ,IUPD  ,IRBE3 ,LRBE3 ,IRBE2  ,
     B            LRBE2 ,F_DDL ,L_DDL ,ITASK )
C----------------------
         CALL MY_BARRIER
C---------------------
         DO I=F_DDL,L_DDL
          R(I) = R0(I)-Z(I)
         ENDDO 
C----------------------
         CALL MY_BARRIER
C---------------------
         CALL PREC_SOLVH(IPREC, ITASK ,
     1                 GRAPHE,IAD_ELEM,FR_ELEM,DIAG_K,LT_K   , 
     2                 IADK  ,JDIK    ,ITAB   ,IPRI,INSOLV , 
     3                 ITN   ,FAC_K   , IPIV_K, NK   ,MUMPS_PAR,
     4                 CDDLP ,ISOLV   , IDSC  , IDDL ,IKC      , 
     5                 INLOC  ,NDOF   , NDDL  ,NNZM  ,IADM    , 
     6                 JDIM  ,DIAG_M  , LT_M  ,R     ,Z       ,
     7                 F_DDL ,L_DDL   )
C----------------------
         CALL MY_BARRIER
C---------------------
         CALL PRODUT_H( F_DDL  ,L_DDL ,R   ,Z  ,W_DDL , G0 ,ITASK )
         BETA = ZERO
C------ R2<TOL*TOL*ANORM2*XNORM2------
         IF (ITOL==3) THEN
           TNORM2=L_A*L_A
           ANORM2=ZERO
           XNORM2=ZERO
           A_OLD=ZERO
           B_OLD=ZERO
           L_B=ZERO
C*     INITIALIZE OTHER QUANTITIES.
           GBAR   = L_A
           DBAR   = ZERO
           RHS1   = SQRT(R02)
           RHS2   = ZERO
           GMAX   = ABS( L_A ) + EPS_M
           GMIN   = GMAX
           OLDB2   = ZERO
           OLDB   = L_B
         ELSEIF (ITOL==4) THEN
          DO J=1,ND
           EPS(J)=R02
          ENDDO
         ENDIF
        ENDIF
C----------------------
        CALL MY_BARRIER
C---------------------
        IT = IT +1
       ENDDO
 200   CONTINUE

#elif defined knf
      IF(GPUR4R8==1)THEN
      ELSEIF(GPUR4R8==2)THEN
       IF(ITASK==0)THEN
C Code MIC simple precision

C debut code specifique MIC double precision
CAD
CAD
CAD  DIRECTIVES POUR MIC
CAD
C init MIC
        CALL MIC_INIT(
     .    NSPMD,LOC_PROC,L_SPMD(LOC_PROC),IDEVICE,MICID,IERR)
        IF(IERR < 0) THEN
          IF(IERR==-1)THEN
            CALL ANCMSG(MSGID=223,ANMODE=ANINFO_BLIND)
          ELSEIF(IERR==-2)THEN
            CALL ANCMSG(MSGID=224,ANMODE=ANINFO_BLIND)
          ELSEIF(IERR==-3)THEN
            CALL ANCMSG(MSGID=225,ANMODE=ANINFO_BLIND)
          END IF
          CALL ARRET(2)
        END IF
        NDDL1=NDDL+1
        print *,' INIT MIC CARD NUMBER ',MICID
C dummy variables to be replaced by pointer later
c        M_LT_K => LT_K(1:NNZ)
c        M_JDIK => JDIK(1:NNZ)
c        M_LT_M => LT_M(1:NNZM)
c        M_JDIM => JDIM(1:NNZM)
c        M_DIAG_K => DIAG_K(1:NDDL)
c        M_DIAG_M => DIAG_M(1:NDDL)
c        M_IADK => IADK(1:NDDL1)
c        M_IADM => IADM(1:NDDL1)
c        M_P => P(1:NDDL)
c        M_X => X(1:NDDL)
c        M_R => R(1:NDDL)
c        M_Y => Y(1:NDDL)
c        M_Z => Z(1:NDDL)
cc        call omp_set_num_threads_target (TARGET_MIC, MICID,116)
        i= omp_get_num_threads()
        print *,'Number of threads (host):',i
cc        call kmp_set_blocktime_target (TARGET_MIC, MICID,INFINITE)
        i= kmp_get_blocktime_target (TARGET_MIC, MICID)
!dec$ attributes offload: mic :: ONE,ZERO
!dec$ attributes offload: mic :: MIC_DSCAL,MIC_GATHER,MIC_SCATTER
!dec$ attributes offload: mic :: MIC_DDOT,MIC_MV,MIC_SPMV,MIC_DAXPY
!dec$ attributes offload: mic :: MIC_DCOPY
        TF=OMP_GET_WTIME()
        TB=0
!dir$ omp offload target(mic:MICID)
     & in (LT_K:length(nnz), free_if(.false.), align(512))
!$OMP PARALLEL 
!$OMP END PARALLEL
        TB = TB+NNZ
        print *,' Variables1= ',nnz
!dir$ omp offload target(mic:MICID)
     & in (JDIK:length(nnz), free_if(.false.), align(512))
!$OMP PARALLEL 
!$OMP END PARALLEL
        TB = TB+NNZ
        print *,' Variables2= ',nnz
!dir$ omp offload target(mic:MICID)
     & in (LT_K0:length(nnz), free_if(.false.), align(512))
!$OMP PARALLEL 
!$OMP END PARALLEL
        TB = TB+NNZ
        print *,' Variables3= ',nnz
!dir$ omp offload target(mic:MICID)
     & in (JDIK0:length(nnz), free_if(.false.), align(512))
!$OMP PARALLEL 
!$OMP END PARALLEL
        TB = TB+NNZ
        print *,' Variables4= ',nnz
        IF(IPREC == 5) THEN
!dir$ omp offload target(mic:MICID)
& in (lt_m:length(nnzm), free_if(.false.), align(512))
!$OMP PARALLEL 
!$OMP END PARALLEL
          TB = TB+NNZM
          print *,' Variables5= ',nnzm
!dir$ omp offload target(mic:MICID)
     & in (JDIM:length(nnzm), free_if(.false.), align(512))
!$OMP PARALLEL 
!$OMP END PARALLEL
          TB = TB+NNZM
          print *,' Variables6= ',nnzm
!dir$ omp offload target(mic:MICID)
     & in (lt_m0:length(nnzm), free_if(.false.), align(512))
!$OMP PARALLEL 
!$OMP END PARALLEL
          TB = TB+NNZM
          print *,' Variables7= ',nnzm
!dir$ omp offload target(mic:MICID)
     & in (JDIM0:length(nnzm), free_if(.false.), align(512))
!$OMP PARALLEL 
!$OMP END PARALLEL
          TB = TB+NNZ
          print *,' Variables8= ',nnzm
        END IF
!dir$ omp offload target(mic:MICID)
     & in (DIAG_K,DIAG_M,x,y,z,r,p:length(nddl),
     &     free_if(.false.))
!$OMP PARALLEL 
!$OMP END PARALLEL
        TB = TB+NDDL*7
        print *,' Variables9= ',nddl*7
!dir$ omp offload target(mic:MICID)
     & in (IADK,IADK0,IADM,IADM0:length(NDDL1),
     &     free_if(.false.), align(512))
!$OMP PARALLEL 
!$OMP END PARALLEL
        TB = TB+nddl1*4
        print *,' Variables10= ',nddl1*4
        TF=OMP_GET_WTIME()-TF
        print *,'Transfer Time CPU => MIC (s)   =',TF
        print *,'Transfer Rate CPU => MIC (MB/s)=',
     .         ((TB/(1024*1024))*8)/TF
        IF(NSPMD > 1)THEN
           ALLOCATE(W(NDDL),STAT=IERROR)
           IERROR=IERROR+IERR
           ALLOCATE(VGAT(LCOM),STAT=IERR)
           IERROR=IERROR+IERR
           ALLOCATE(VSCA(LCOM),STAT=IERR)
           IERROR=IERROR+IERR
           ALLOCATE(INDEX(LCOM),STAT=IERR)
           IERROR=IERROR+IERR
           ALLOCATE(TABLE(NDDL),STAT=IERR)
           IERROR=IERROR+IERR
           IF(IERROR /= 0)THEN
C code erreur
           END IF

           DO I = 1, NDDL
C transfo entier vers my_real
             W(I) = W_DDL(I)
           END DO

           TABLE(1:NDDL)=0
           DO I = 1, LCOM
             N = IFR2K(I)
             J = TABLE(N)
             IF(J==0)THEN
               INDEX(I)=0
               TABLE(N)=I
             ELSE
               INDEX(I)=J
             END IF
           END DO
           DEALLOCATE(TABLE)
!dir$ omp offload target(mic:MICID)
     & in (W:length(NDDL), free_if(.false.), align(512))
!$OMP PARALLEL 
!$OMP END PARALLEL
          print *,' Variables11= ',i
!dir$ omp offload target(mic:MICID)
     & in (IFR2K,INDEX:length(LCOM), free_if(.false.))
c     &     free_if(.false.), align(512))
!$OMP PARALLEL 
!$OMP END PARALLEL
          print *,' Variables12= ',i
        END IF
        
      IF(NSPMD>1)THEN 
!dir$ omp offload target(mic:MICID)
     & nocopy(IADK,IADK0,IADM,IADM0,LT_K,LT_K0,
     &        lt_m,lt_m0,JDIK,JDIK0,JDIM,JDIM0,
     &        DIAG_K,DIAG_M,x,y,z,r,p,w)
     &        inout(tt)
!$OMP PARALLEL default(shared)


!$OMP single 
      tt=OMP_GET_WTIME()
      i= omp_get_num_threads()
      print *,'Number of threads (MIC):',i
!$OMP END single 

C-------------IT=0--------
C----------{Z}=[K]{X}--------
C----------{Z}=[Dk]{X}+[Lk]{X}+[Lk]^t{X}--------

C D_Z = D_X * DIAG_K
      CALL MIC_MV(NDDL,X,DIAG_K,Z)

C D_Z = D_Z + LT_K * D_X
      CALL MIC_SPMV(NDDL ,Z ,X ,LT_K ,IADK ,JDIK ) 
C D_Z = D_Z + LT_K0 * D_X
      CALL MIC_SPMV(NDDL ,Z ,X ,LT_K0,IADK0,JDIK0) 
         IF(NDDLI > 0) THEN
C D_Z = D_Z + LT_I0 * D_X
ctempo      CALL MIC_SPMV(NDDL ,Z ,X ,LT_I0,IADI0,JDII0) 
         END IF
C D_R = D_R - D_Z
      CALL MIC_DAXPY(NDDL, -ONE, Z, R)    

C---------------------
C----------{Z}=[M]{R}-([M]=[Lm][Dm][Lm]^t)-------
C----------->{V}={R}+[Lm]^t{R}-(the first term presents the [I]{R} with is not included in Lm)-------
C----------->{W}=[Dm]{V}-------(the first term ->[I]{W}-same as above)
C----------->{Z}={W}+[Lm]{W}--------
C       CALL PREC_SOLVGH
         IF(IPREC == 1)THEN
C D_Z = D_R
      CALL MIC_DCOPY(NDDL, R, Z)
         ELSEIF(IPREC == 2)THEN
C D_Z = D_R * DIAG_M
      CALL MIC_MV(NDDL,R,DIAG_M,Z)
         ELSEIF(IPREC == 5)THEN
C D_Y = D_R
      CALL MIC_DCOPY(NDDL, R, Y)
C D_Y = D_Y + LT_M * D_R
      CALL MIC_SPMV(NDDL ,Y ,R ,LT_M ,IADM ,JDIM ) 
C D_Z = D_Y * DIAG_M
      CALL MIC_MV(NDDL,Y,DIAG_M,Z)
C D_Y = D_Z
      CALL MIC_DCOPY(NDDL, Z, Y)
C D_Z = D_Z + LT_M0 * D_Y
      CALL MIC_SPMV(NDDL ,Z ,Y ,LT_M0,IADM0,JDIM0) 

         ELSE
         END IF
!$OMP END PARALLEL
C----------------------
         IF(NSPMD > 1 .AND. IPREC > 1)THEN
           IF(IMONM > 0) CALL STARTIME(68,1)
!dir$ omp offload target(mic:MICID)
     & nocopy(z,ifr2k)
     & out(vgat:length(lcom))
!$OMP PARALLEL default(shared)
           CALL MIC_GATHER(NDDL, LCOM, Z, VGAT, IFR2K)
!$OMP END PARALLEL
           IF(IMONM > 0) CALL STOPTIME(68,1)
           IF(IMONM > 0) CALL STARTIME(66,1)
           CALL SPMD_SUMFC_V(VGAT,VSCA,INDEX,LCOM)
           IF(IMONM > 0) CALL STOPTIME(66,1)
           IF(IMONM > 0) CALL STARTIME(69,1)
!dir$ omp offload target(mic:MICID)
     & nocopy(z,ifr2k,index)
     & in(vsca:length(lcom))
!$OMP PARALLEL default(shared)
           CALL MIC_SCATTER(NDDL, LCOM, Z, VSCA, IFR2K, INDEX)          
!$OMP END PARALLEL
           IF(IMONM > 0) CALL STOPTIME(69,1)
         END IF
C---------------------
!dir$ omp offload target(mic:MICID)
     & nocopy(z,p)
!$OMP PARALLEL default(shared)
C D_P = D_Z
         CALL MIC_DCOPY(NDDL, Z, P)
!$OMP END PARALLEL
C  
         IF(NSPMD == 1) THEN
!dir$ omp offload target(mic:MICID)
     & nocopy(r,z)
     & inout(G0)
!$OMP PARALLEL default(shared) shared(g0)
          CALL MIC_DDOT(NDDL, R, Z, G0)
!$OMP END PARALLEL
         ELSE
C si spmd penser a faire avant D_Y = D_Z*W pour les poids
C D_Y = D_Z * D_W
!dir$ omp offload target(mic:MICID)
     & nocopy(z,y,w,r)
     & inout(G0)
!$OMP PARALLEL default(shared) shared(g0)
           CALL MIC_MV(NDDL,Z,W,Y)
           CALL MIC_DDOT(NDDL, R, Y, G0)
!$OMP END PARALLEL
           IF(IMONM > 0) CALL STARTIME(67,1)
           CALL SPMD_SUM_S(G0)
           IF(IMONM > 0) CALL STOPTIME(67,1)
         ENDIF

!dir$ omp offload target(mic:MICID)
     & nocopy(iadk,iadk0,iadm,iadm0,lt_k,lt_k0,lt_m)
     & nocopy(lt_m0,jdik,jdik0,jdim,jdim0,diag_k,diag_m,y,p,x,r,z)
!$OMP PARALLEL default(shared)
C---------------------
C       CALL MAV_LTGH(
C     1            NDDL  ,IADK  ,JDIK  ,DIAG_K,LT_K  ,   
C     2            P     ,Y     ,F_DDL ,L_DDL ,ITASK )

C D_Y = D_P * DIAG_K
         CALL MIC_MV(NDDL,P,DIAG_K,Y)
C D_Y = D_Y + LT_K * D_P
         CALL MIC_SPMV(NDDL ,Y ,P ,LT_K ,IADK ,JDIK ) 
C D_Y = D_Y + LT_K0 * D_P
         CALL MIC_SPMV(NDDL ,Y ,P ,LT_K0,IADK0,JDIK0) 
         IF(NDDLI > 0) THEN
C D_Y = D_Y + LT_I0 * D_P
ctempo      CALL MIC_SPMV(NDDL ,Y ,P ,LT_I0,IADI0,JDII0) 
         END IF
!$OMP END PARALLEL
C----------------------
         IF(NSPMD > 1)THEN
           IF(IMONM > 0) CALL STARTIME(68,1)
!dir$ omp offload target(mic:MICID)
     & nocopy(y,ifr2k)
     & out(vgat:length(lcom))
!$OMP PARALLEL default(shared)
           CALL MIC_GATHER(NDDL, LCOM, Y, VGAT, IFR2K)
!$OMP END PARALLEL
           IF(IMONM > 0) CALL STOPTIME(68,1)
           IF(IMONM > 0) CALL STARTIME(66,1)
           CALL SPMD_SUMFC_V(VGAT,VSCA,INDEX,LCOM)
           IF(IMONM > 0) CALL STOPTIME(66,1)
           IF(IMONM > 0) CALL STARTIME(69,1)
!dir$ omp offload target(mic:MICID)
     & nocopy(y,ifr2k,index)
     & in(vsca:length(lcom))
!$OMP PARALLEL default(shared)
           CALL MIC_SCATTER(NDDL, LCOM, Y, VSCA, IFR2K, INDEX)          
!$OMP END PARALLEL
           IF(IMONM > 0) CALL STOPTIME(69,1)
         END IF
C---------------------
         IF(NSPMD == 1) THEN
!dir$ omp offload target(mic:MICID)
     & nocopy(p,y)
     & inout(S)
!$OMP PARALLEL default(shared) shared(s)
          CALL MIC_DDOT(NDDL, P, Y, S)
!$OMP END PARALLEL
         ELSE
C si spmd penser a faire avant D_Z = D_Y*D_W pour les poids
!dir$ omp offload target(mic:MICID)
     & nocopy(y,w,p,z)
     & inout(S)
!$OMP PARALLEL default(shared) shared(s)
           CALL MIC_MV(NDDL,Y,W,Z)
           CALL MIC_DDOT(NDDL, P, Z, S)
!$OMP END PARALLEL
           IF(IMONM > 0) CALL STARTIME(67,1)
           CALL SPMD_SUM_S(S)
           IF(IMONM > 0) CALL STOPTIME(67,1)
         END IF
C---------------------
C---------------------
         IF (G0==ZERO) THEN
           IF (ITOL>1) THEN
             ISTOP=-1
             GOTO 206
           ELSE
             ALPHA = ZERO
           ENDIF
         ELSE
           ALPHA = G0/S
         ENDIF
         TOLS2=TOLS*TOLS
         IF (ITOL==1) THEN
           IF(NSPMD == 1) THEN
!dir$ omp offload target(mic:MICID)
     & nocopy(r)
     & inout(r02)
!$OMP PARALLEL default(shared) shared(r02)
            CALL MIC_DDOT(NDDL, R, R, R02)
!$OMP END PARALLEL
           ELSE
C si spmd penser a faire avant D_Z = D_R*W pour les poids
!dir$ omp offload target(mic:MICID)
     & nocopy(r,w,z)
     & inout(r02)
!$OMP PARALLEL default(shared) shared(r02)
             CALL MIC_MV(NDDL,R,W,Z)
             CALL MIC_DDOT(NDDL, Z, Z, R02)
!$OMP END PARALLEL
             IF(IMONM > 0) CALL STARTIME(67,1)
             CALL SPMD_SUM_S(R02)
             IF(IMONM > 0) CALL STOPTIME(67,1)
           END IF
C---------------------
C---------------------
           R2 =R02
         ELSEIF (ITOL==3) THEN
C------ R2<TOL*TOL*ANORM2*XNORM2------
           R02=ABS(G0)
           R2 =R02
           L_A=ONE/ALPHA
C          L_B2=R02
           TNORM2=L_A*L_A
           ANORM2=ZERO
           XNORM2=ZERO
           A_OLD=L_A
           B_OLD=ZERO
           OLDB   = SQRT(R02)
         ELSEIF (ITOL==4) THEN
           R02=ALPHA*ALPHA*ABS(G0)
           EPS(1)=R02
           R2=R02
         ELSE
           R02=ABS(G0)
           R2 =R02
         ENDIF
         IF (R02==ZERO) THEN
           ISTOP=-1
           GOTO 206
         ENDIF
         TOLN=R02*TOLS2
         IF(IPRI/=0.AND.ITASK==0)THEN
           RR = SQRT(R2)
           IF(IPRI<0) WRITE(ISTDO,1000)IT,RR
           WRITE(IOUT,1000)IT,RR
         ENDIF
C-------pour etre coherent avec lanzos for linear
C----------------------
c       CALL MY_BARRIER
C---------------------
         IT=1
!dir$ omp offload target(mic:MICID)
     & nocopy(iadk,iadk0,iadm,iadm0,lt_k,lt_k0,lt_m)
     & nocopy(lt_m0,jdik,jdik0,jdim,jdim0,diag_k,diag_m,y,p,x,r,z)
!$OMP PARALLEL default(shared)
      CALL MIC_DAXPY(NDDL, ALPHA, P, X)
      CALL MIC_DAXPY(NDDL,-ALPHA, Y, R)

C----------------------
c       CALL MY_BARRIER
C---------------------
C       CALL PREC_SOLVGH(IPREC, ITASK   ,NDDL  ,IADM  , JDIM  ,
C     6                 DIAG_M  , LT_M  ,R     ,Z        ,
C     7                 F_DDL ,L_DDL   )
         IF(IPREC == 1)THEN
C D_Z = D_R
      CALL MIC_DCOPY(NDDL, R, Z)
         ELSEIF(IPREC == 2)THEN
C D_Z = D_R * DIAG_M
      CALL MIC_MV(NDDL,R,DIAG_M,Z)
         ELSEIF(IPREC == 5)THEN
C D_Y = D_R
      CALL MIC_DCOPY(NDDL, R, Y)
C D_Y = D_Y + LT_M * D_R
      CALL MIC_SPMV(NDDL ,Y ,R ,LT_M ,IADM ,JDIM ) 
C D_Z = D_Y * DIAG_M
      CALL MIC_MV(NDDL,Y,DIAG_M,Z)
      CALL MIC_DCOPY(NDDL, Z, Y)
C D_Z = D_Z + LT_M0 * D_Y
      CALL MIC_SPMV(NDDL ,Z ,Y ,LT_M0,IADM0,JDIM0) 
         ELSE
         END IF
!$OMP END PARALLEL
C----------------------
         IF(NSPMD > 1 .AND. IPREC > 1)THEN
           IF(IMONM > 0) CALL STARTIME(68,1)
!dir$ omp offload target(mic:MICID)
     & nocopy(z,ifr2k)
     & out(vgat:length(lcom))
!$OMP PARALLEL default(shared)
           CALL MIC_GATHER(NDDL, LCOM, Z, VGAT, IFR2K)
!$OMP END PARALLEL
           IF(IMONM > 0) CALL STOPTIME(68,1)
           IF(IMONM > 0) CALL STARTIME(66,1)
           CALL SPMD_SUMFC_V(VGAT,VSCA,INDEX,LCOM)
           IF(IMONM > 0) CALL STOPTIME(66,1)
           IF(IMONM > 0) CALL STARTIME(69,1)
!dir$ omp offload target(mic:MICID)
     & nocopy(z,ifr2k,index)
     & in(vsca:length(lcom))
!$OMP PARALLEL default(shared)
           CALL MIC_SCATTER(NDDL, LCOM, Z, VSCA, IFR2K, INDEX)          
!$OMP END PARALLEL
           IF(IMONM > 0) CALL STOPTIME(69,1)
         END IF
C---------------------
         IF(NSPMD == 1) THEN
!dir$ omp offload target(mic:MICID)
     & nocopy(r,z)
     & inout(G1)
!$OMP PARALLEL default(shared) shared(G1)
          CALL MIC_DDOT(NDDL, R, Z, G1)
!$OMP END PARALLEL
         ELSE
C si spmd penser a faire avant D_Y = D_Z*D_W pour les poids
!dir$ omp offload target(mic:MICID)
     & nocopy(z,w,r,y)
     & inout(G1)
!$OMP PARALLEL default(shared) shared(G1)
           CALL MIC_MV(NDDL,Z,W,Y)
           CALL MIC_DDOT(NDDL, R, Y, G1)
!$OMP END PARALLEL
           IF(IMONM > 0) CALL STARTIME(67,1)
           CALL SPMD_SUM_S(G1)
           IF(IMONM > 0) CALL STOPTIME(67,1)
         END IF
C---------------------
         BETA=G1/G0
         IF (ITOL==1) THEN
           IF(NSPMD == 1) THEN
!dir$ omp offload target(mic:MICID)
     & nocopy(r)
     & inout(R2)
!$OMP PARALLEL default(shared) shared(R2)
             CALL MIC_DDOT(NDDL, R, R, R2)
!$OMP END PARALLEL
           ELSE
C si spmd penser a faire avant D_Y = D_R*W pour les poids
!dir$ omp offload target(mic:MICID)
     & nocopy(r,w,y)
     & inout(R2)
!$OMP PARALLEL default(shared) shared(R2)
             CALL MIC_MV(NDDL,R,W,Y)
             CALL MIC_DDOT(NDDL, Y, Y, R2)
!$OMP END PARALLEL
             IF(IMONM > 0) CALL STARTIME(67,1)
             CALL SPMD_SUM_S(R2)
             IF(IMONM > 0) CALL STOPTIME(67,1)
           END IF
         ELSEIF (ITOL==3) THEN
C------ R2<TOL*TOL*ANORM2*XNORM2------
           R2=ABS(G1)
           L_B2=ABS(BETA)*A_OLD*A_OLD
           L_B=SQRT(L_B2)
           TNORM2=TNORM2+L_B2
           B_OLD=BETA
C     INITIALIZE OTHER QUANTITIES.
           GBAR   = L_A
           DBAR   = L_B
           RHS1   = OLDB
           RHS2   = ZERO
           GMAX   = ABS( L_A ) + EPS_M
           GMIN   = GMAX
           OLDB2   = L_B2
           OLDB   = L_B
         ELSEIF (ITOL==4) THEN
           R2=R02
         ELSE
           R2=ABS(G1)
         ENDIF
         G0 = G1
         IF (ITOL==3) TOLN=TOLN*TNORM2
C----------------------
c         CALL MY_BARRIER
C---------------------
         ISTOP=CRIT_STOP(IT,R2,NLIM,TOLN)

C----------------------
c         LOOP OVER ITERATIONS
C---------------------

         DO WHILE (ISTOP==1)

!dir$ omp offload target(mic:MICID)
     & nocopy(iadk,iadk0,iadm,iadm0,lt_k,lt_k0,lt_m)
     & nocopy(lt_m0,jdik,jdik0,jdim,jdim0,diag_k,diag_m,y,p,x,r,z)
!$OMP PARALLEL default(shared)

C D_P = D_Z + BETA*D_P in 2 step : D_P = BETA*D_P ; D_P = D_P + D_Z
      CALL MIC_DSCAL(NDDL, BETA, P)
      CALL MIC_DAXPY(NDDL, ONE, Z, P)
C----------------------
c        CALL MY_BARRIER
C---------------------
C        CALL MAV_LTGH(
C     1            NDDL  ,IADK  ,JDIK  ,DIAG_K,LT_K  ,   
C     2            P     ,Y     ,F_DDL ,L_DDL ,ITASK )
C D_Y = D_P * DIAG_K
          CALL MIC_MV(NDDL,P,DIAG_K,Y)
C D_Y = D_Y + LT_K * D_P
          CALL MIC_SPMV(NDDL ,Y ,P ,LT_K ,IADK ,JDIK ) 
C D_Y = D_Y + LT_K0 * D_P
          CALL MIC_SPMV(NDDL ,Y ,P ,LT_K0,IADK0,JDIK0) 
          IF(NDDLI > 0) THEN
C D_Y = D_Y + LT_I0 * D_P
ctempo      CALL MIC_SPMV(NDDL ,Y ,P ,LT_I0,IADI0,JDII0) 
          END IF
!$OMP END PARALLEL
          IF(NSPMD > 1)THEN
           IF(IMONM > 0) CALL STARTIME(68,1)
!dir$ omp offload target(mic:MICID)
     & nocopy(y,ifr2k)
     & out(vgat:length(lcom))
!$OMP PARALLEL default(shared)
           CALL MIC_GATHER(NDDL, LCOM, Y, VGAT, IFR2K)
!$OMP END PARALLEL
           IF(IMONM > 0) CALL STOPTIME(68,1)
           IF(IMONM > 0) CALL STARTIME(66,1)
           CALL SPMD_SUMFC_V(VGAT,VSCA,INDEX,LCOM)
           IF(IMONM > 0) CALL STOPTIME(66,1)
           IF(IMONM > 0) CALL STARTIME(69,1)
!dir$ omp offload target(mic:MICID)
     & nocopy(y,ifr2k,index)
     & in(vsca:length(lcom))
!$OMP PARALLEL default(shared)
           CALL MIC_SCATTER(NDDL, LCOM, Y, VSCA, IFR2K, INDEX)          
!$OMP END PARALLEL
           IF(IMONM > 0) CALL STOPTIME(69,1)
          END IF
C---------------------
          IF(NSPMD == 1) THEN
!dir$ omp offload target(mic:MICID)
     & nocopy(p,y)
     & inout(S)
!$OMP PARALLEL default(shared) shared(S)
            CALL MIC_DDOT(NDDL, P, Y, S)
!$OMP END PARALLEL
          ELSE
C si spmd penser a faire avant D_Y = D_Y*W pour les poids
!dir$ omp offload target(mic:MICID)
     & nocopy(y,w,p,z)
     & inout(S)
!$OMP PARALLEL default(shared) shared(S)
            CALL MIC_MV(NDDL,Y,W,Z)
            CALL MIC_DDOT(NDDL, P, Z, S)
!$OMP END PARALLEL
            IF(IMONM > 0) CALL STARTIME(67,1)
            CALL SPMD_SUM_S(S)
            IF(IMONM > 0) CALL STOPTIME(67,1)
          END IF
C---------------------
          ALPHA=G0/S
!dir$ omp offload target(mic:MICID)
     & nocopy(iadk,iadk0,iadm,iadm0,lt_k,lt_k0,lt_m)
     & nocopy(lt_m0,jdik,jdik0,jdim,jdim0,diag_k,diag_m,y,p,x,r,z)
!$OMP PARALLEL default(shared)
          CALL MIC_DAXPY(NDDL, ALPHA, P, X)
          CALL MIC_DAXPY(NDDL,-ALPHA, Y, R)
C----------------------
C        CALL PREC_SOLVGH(IPREC , ITASK ,NDDL  ,IADM  , JDIM  ,
C     6                  DIAG_M, LT_M  ,R     ,Z     ,
C     7                  F_DDL ,L_DDL   )
          IF(IPREC == 1)THEN
C D_Z = D_R
            CALL MIC_DCOPY(NDDL, R, Z)
          ELSEIF(IPREC == 2)THEN
C D_Z = D_R * DIAG_M
            CALL MIC_MV(NDDL,R,DIAG_M,Z)
          ELSEIF(IPREC == 5)THEN
C D_Y = D_R
            CALL MIC_DCOPY(NDDL, R, Y)
C D_Y = D_Y + LT_M * D_R
            CALL MIC_SPMV(NDDL ,Y ,R ,LT_M ,IADM ,JDIM ) 
C D_Z = D_Y * DIAG_M
            CALL MIC_MV(NDDL,Y,DIAG_M,Z)
            CALL MIC_DCOPY(NDDL, Z, Y)
C D_Z = D_Z + LT_M0 * D_Y
            CALL MIC_SPMV(NDDL ,Z ,Y ,LT_M0,IADM0,JDIM0) 
          ELSE
          END IF
!$OMP END PARALLEL
C----------------------
          IF(NSPMD > 1 .AND. IPREC > 1)THEN
           IF(IMONM > 0) CALL STARTIME(68,1)
!dir$ omp offload target(mic:MICID)
     & nocopy(z,ifr2k)
     & out(vgat:length(lcom))
!$OMP PARALLEL default(shared)
           CALL MIC_GATHER(NDDL, LCOM, Z, VGAT, IFR2K)
!$OMP END PARALLEL
           IF(IMONM > 0) CALL STOPTIME(68,1)
           IF(IMONM > 0) CALL STARTIME(66,1)
           CALL SPMD_SUMFC_V(VGAT,VSCA,INDEX,LCOM)
           IF(IMONM > 0) CALL STOPTIME(66,1)
           IF(IMONM > 0) CALL STARTIME(69,1)
!dir$ omp offload target(mic:MICID)
     & nocopy(z,ifr2k,index)
     & in(vsca:length(lcom))
!$OMP PARALLEL default(shared)
           CALL MIC_SCATTER(NDDL, LCOM, Z, VSCA, IFR2K, INDEX)          
!$OMP END PARALLEL
           IF(IMONM > 0) CALL STOPTIME(69,1)
          END IF
C---------------------
          IF(NSPMD == 1) THEN
!dir$ omp offload target(mic:MICID)
     & nocopy(r,z)
     & inout(G1)
!$OMP PARALLEL default(shared) shared(G1)
            CALL MIC_DDOT(NDDL, R, Z, G1)
!$OMP END PARALLEL
          ELSE
C si spmd penser a faire avant D_Y = D_Z*W pour les poids
!dir$ omp offload target(mic:MICID)
     & nocopy(z,w,r,y)
     & inout(G1)
!$OMP PARALLEL default(shared) shared(G1)
            CALL MIC_MV(NDDL,Z,W,Y)
            CALL MIC_DDOT(NDDL, R, Y, G1)
!$OMP END PARALLEL
            IF(IMONM > 0) CALL STARTIME(67,1)
            CALL SPMD_SUM_S(G1)
            IF(IMONM > 0) CALL STOPTIME(67,1)
          END IF
C---------------------
cc!$OMP single 
          BETA=G1/G0
          G0 = G1
cc!$OMP end single 
          IF (ITOL==1) THEN
            IF(NSPMD == 1) THEN
!dir$ omp offload target(mic:MICID)
     & nocopy(r)
     & inout(R2)
!$OMP PARALLEL default(shared) shared(R2)
              CALL MIC_DDOT(NDDL, R, R, R2)
!$OMP END PARALLEL
            ELSE
C si spmd penser a faire avant D_Y = D_R*W pour les poids
!dir$ omp offload target(mic:MICID)
     & nocopy(r,w,y)
     & inout(R2)
!$OMP PARALLEL default(shared) shared(R2)
              CALL MIC_MV(NDDL,R,W,Y)
              CALL MIC_DDOT(NDDL, Y, Y, R2)
!$OMP END PARALLEL
              IF(IMONM > 0) CALL STARTIME(67,1)
              CALL SPMD_SUM_S(R2)
              IF(IMONM > 0) CALL STOPTIME(67,1)
            END IF
          ELSEIF (ITOL==3) THEN
           R2 =ABS(G1)
           S=ONE/ALPHA
           L_A=S+B_OLD*A_OLD
C----------L_B2 : beta(j-1)^2-A_OLD :1/alpha(j-1)-------
           L_B2=ABS(BETA)*S*S
           L_B=SQRT(L_B2)
           A_OLD=S
           B_OLD=BETA
           ANORM2=TNORM2
           TNORM2=TNORM2+L_A*L_A+OLDB2+L_B2
           GAMMA  = SQRT( GBAR*GBAR + OLDB2 )
           CS     = GBAR / GAMMA
           SN     = OLDB / GAMMA
           DELTA  = CS * DBAR  +  SN * L_A
           GBAR   = SN * DBAR  -  CS * L_A
           EPSLN  = SN * L_B
           DBAR   =            -  CS * L_B
           ZL     = RHS1 / GAMMA
           XNORM2 = XNORM2+ZL*ZL
           GMAX   = MAX( GMAX, GAMMA )
           GMIN   = MIN( GMIN, GAMMA )
           RHS1   = RHS2  -  DELTA * ZL
           RHS2   =       -  EPSLN * ZL
           TOLN=TOLS2*ANORM2*XNORM2
           OLDB2   = L_B2
           OLDB   = L_B
          ELSEIF (ITOL==4) THEN
           TMP=ALPHA*ALPHA*ABS(G1)
           IF (IT>=ND) THEN
            DO J=1,ND-1
             EPS(J)=EPS(J+1)
            ENDDO
            EPS(ND)=TMP
            R2=ZERO
            DO J=1,ND
             R2=R2+EPS(J)
            ENDDO
           ELSE
            EPS(IT+1)=TMP
            R2=R2+TMP
           ENDIF
          ELSE
           R2 =ABS(G1)
          ENDIF
          IF(IPRI/=0.AND.ITASK==0)THEN
           IF(MOD(IT,IP)==0)THEN
            RR = SQRT(R2/R02)
            WRITE(IOUT,1001)IT,RR
            IF(IPRI<0) WRITE(ISTDO,1001)IT,RR
           ENDIF
          ENDIF
C----------------------
c          CALL MY_BARRIER
C---------------------
          ISTOP=CRIT_STOP(IT,R2,NLIM,TOLN)
C----------------------
c        CALL MY_BARRIER
C---------------------
          IT = IT +1
         ENDDO
 206     CONTINUE

         WRITE(IOUT,1001)IT-1,RR

C!dec$ omp offload target(mic:MICID) out(X,R:alloc_if(.false.))
!$OMP PARALLEL 
!$OMP single 
      tt=OMP_GET_WTIME()-tt
      i= omp_get_num_threads()
      print *,' '
      print *,' '
      print *,' Execution time on MIC with',i,' THREADS'
      print *,' ==> ',tt,' SECONDS'
      print *,' '
!$OMP END single 
!$OMP END PARALLEL
      DEALLOCATE(W)
      DEALLOCATE(VGAT)
      DEALLOCATE(VSCA)
      DEALLOCATE(INDEX) 
c+1 tempo
      ELSE  ! CODE NSPMD = 1
C debut code provisoire
!dec$ attributes offload: mic :: ONE,ZERO,ISTDO,IOUT

!dir$ omp offload target(mic:MICID)
     & nocopy(IADK,IADK0,IADM,IADM0,LT_K,LT_K0,
     &        lt_m,lt_m0,JDIK,JDIK0,JDIM,JDIM0,
     &        DIAG_K,DIAG_M,x,y,z,r,p,w)
     &        inout(tt)
     
!$OMP PARALLEL default(shared)

!$OMP single 
      tt=OMP_GET_WTIME()
      i= omp_get_num_threads()
      print *,'Number of threads (MIC):',i
!$OMP END single 

C-------------IT=0--------
C----------{Z}=[K]{X}--------
C----------{Z}=[Dk]{X}+[Lk]{X}+[Lk]^t{X}--------

C D_Z = D_X * DIAG_K
      CALL MIC_MV(NDDL,X,DIAG_K,Z)

C D_Z = D_Z + LT_K * D_X
      CALL MIC_SPMV(NDDL ,Z ,X ,LT_K ,IADK ,JDIK ) 
C D_Z = D_Z + LT_K0 * D_X
      CALL MIC_SPMV(NDDL ,Z ,X ,LT_K0,IADK0,JDIK0) 
         IF(NDDLI > 0) THEN
C D_Z = D_Z + LT_I0 * D_X
ctempo      CALL MIC_SPMV(NDDL ,Z ,X ,LT_I0,IADI0,JDII0) 
         END IF
C D_R = D_R - D_Z
      CALL MIC_DAXPY(NDDL, -ONE, Z, R)    
C---------------------
C----------{Z}=[M]{R}-([M]=[Lm][Dm][Lm]^t)-------
C----------->{V}={R}+[Lm]^t{R}-(the first term presents the [I]{R} with is not included in Lm)-------
C----------->{W}=[Dm]{V}-------(the first term ->[I]{W}-same as above)
C----------->{Z}={W}+[Lm]{W}--------
C       CALL PREC_SOLVGH
         IF(IPREC == 1)THEN
C D_Z = D_R
      CALL MIC_DCOPY(NDDL, R, Z)
         ELSEIF(IPREC == 2)THEN
C D_Z = D_R * DIAG_M
      CALL MIC_MV(NDDL,R,DIAG_M,Z)
         ELSEIF(IPREC == 5)THEN
C D_Y = D_R
      CALL MIC_DCOPY(NDDL, R, Y)
C D_Y = D_Y + LT_M * D_R
      CALL MIC_SPMV(NDDL ,Y ,R ,LT_M ,IADM ,JDIM ) 
C D_Z = D_Y * DIAG_M
      CALL MIC_MV(NDDL,Y,DIAG_M,Z)
C D_Y = D_Z
      CALL MIC_DCOPY(NDDL, Z, Y)
C D_Z = D_Z + LT_M0 * D_Y
      CALL MIC_SPMV(NDDL ,Z ,Y ,LT_M0,IADM0,JDIM0) 

         ELSE
         END IF
c!$OMP END PARALLEL
C----------------------
c         IF(NSPMD > 1 .AND. IPREC > 1)THEN
c           IF(IMONM > 0) CALL STARTIME(68,1)
c!dir$ omp offload target(mic:MICID)
c     & nocopy(z,ifr2k)
c     & out(vgat:length(lcom))
c!$OMP PARALLEL default(shared)
c           CALL MIC_GATHER(NDDL, LCOM, Z, VGAT, IFR2K)
c!$OMP END PARALLEL
c           IF(IMONM > 0) CALL STOPTIME(68,1)
c           IF(IMONM > 0) CALL STARTIME(66,1)
c           CALL SPMD_SUMFC_V(VGAT,VSCA,INDEX,LCOM)
c           IF(IMONM > 0) CALL STOPTIME(66,1)
c           IF(IMONM > 0) CALL STARTIME(69,1)
c!dir$ omp offload target(mic:MICID)
c     & nocopy(z,ifr2k,index)
c     & in(vsca:length(lcom))
c!$OMP PARALLEL default(shared)
c           CALL MIC_SCATTER(NDDL, LCOM, Z, VSCA, IFR2K, INDEX)          
c!$OMP END PARALLEL
c           IF(IMONM > 0) CALL STOPTIME(69,1)
c         END IF
C---------------------
c!dir$ omp offload target(mic:MICID)
c     & nocopy(z,p)
c!$OMP PARALLEL default(shared)
C D_P = D_Z
         CALL MIC_DCOPY(NDDL, Z, P)
c!$OMP END PARALLEL
C  
c         IF(NSPMD == 1) THEN
c!dir$ omp offload target(mic:MICID)
c     & nocopy(r,z)
c     & inout(G0)
c!$OMP PARALLEL default(shared) shared(g0)
          CALL MIC_DDOT(NDDL, R, Z, G0)
c!$OMP END PARALLEL
c         ELSE
C si spmd penser a faire avant D_Y = D_Z*W pour les poids
C D_Y = D_Z * D_W
c!dir$ omp offload target(mic:MICID)
c     & nocopy(z,y,w,r)
c     & inout(G0)
c!$OMP PARALLEL default(shared) shared(g0)
c           CALL MIC_MV(NDDL,Z,W,Y)
c           CALL MIC_DDOT(NDDL, R, Y, G0)
c!$OMP END PARALLEL
c           IF(IMONM > 0) CALL STARTIME(67,1)
c           CALL SPMD_SUM_S(G0)
c           IF(IMONM > 0) CALL STOPTIME(67,1)
c         ENDIF

c!dir$ omp offload target(mic:MICID)
c     & nocopy(iadk,iadk0,iadm,iadm0,lt_k,lt_k0,lt_m)
c     & nocopy(lt_m0,jdik,jdik0,jdim,jdim0,diag_k,diag_m,y,p,x,r,z)
c!$OMP PARALLEL default(shared)
C---------------------
C       CALL MAV_LTGH(
C     1            NDDL  ,IADK  ,JDIK  ,DIAG_K,LT_K  ,   
C     2            P     ,Y     ,F_DDL ,L_DDL ,ITASK )

C D_Y = D_P * DIAG_K
         CALL MIC_MV(NDDL,P,DIAG_K,Y)
C D_Y = D_Y + LT_K * D_P
         CALL MIC_SPMV(NDDL ,Y ,P ,LT_K ,IADK ,JDIK ) 
C D_Y = D_Y + LT_K0 * D_P
         CALL MIC_SPMV(NDDL ,Y ,P ,LT_K0,IADK0,JDIK0) 
         IF(NDDLI > 0) THEN
C D_Y = D_Y + LT_I0 * D_P
ctempo      CALL MIC_SPMV(NDDL ,Y ,P ,LT_I0,IADI0,JDII0) 
         END IF
c!$OMP END PARALLEL
C----------------------
c         IF(NSPMD > 1)THEN
c           IF(IMONM > 0) CALL STARTIME(68,1)
c!dir$ omp offload target(mic:MICID)
c     & nocopy(y,ifr2k)
c     & out(vgat:length(lcom))
c!$OMP PARALLEL default(shared)
c           CALL MIC_GATHER(NDDL, LCOM, Y, VGAT, IFR2K)
c!$OMP END PARALLEL
c           IF(IMONM > 0) CALL STOPTIME(68,1)
c           IF(IMONM > 0) CALL STARTIME(66,1)
c           CALL SPMD_SUMFC_V(VGAT,VSCA,INDEX,LCOM)
c           IF(IMONM > 0) CALL STOPTIME(66,1)
c           IF(IMONM > 0) CALL STARTIME(69,1)
c!dir$ omp offload target(mic:MICID)
c     & nocopy(y,ifr2k,index)
c     & in(vsca:length(lcom))
c!$OMP PARALLEL default(shared)
c           CALL MIC_SCATTER(NDDL, LCOM, Y, VSCA, IFR2K, INDEX)          
c!$OMP END PARALLEL
c           IF(IMONM > 0) CALL STOPTIME(69,1)
c         END IF
C---------------------
c         IF(NSPMD == 1) THEN
c!dir$ omp offload target(mic:MICID)
c     & nocopy(p,y)
c     & inout(S)
c!$OMP PARALLEL default(shared) shared(s)
          CALL MIC_DDOT(NDDL, P, Y, S)
c!$OMP END PARALLEL
c         ELSE
C si spmd penser a faire avant D_Z = D_Y*D_W pour les poids
c!dir$ omp offload target(mic:MICID)
c     & nocopy(y,w,p,z)
c     & inout(S)
c!$OMP PARALLEL default(shared) shared(s)
c           CALL MIC_MV(NDDL,Y,W,Z)
c           CALL MIC_DDOT(NDDL, P, Z, S)
c!$OMP END PARALLEL
c           IF(IMONM > 0) CALL STARTIME(67,1)
c           CALL SPMD_SUM_S(S)
c           IF(IMONM > 0) CALL STOPTIME(67,1)
c         END IF
C---------------------
C---------------------
         IF (G0==ZERO) THEN
           IF (ITOL>1) THEN
             ISTOP=-1
             GOTO 2060
           ELSE
             ALPHA = ZERO
           ENDIF
         ELSE
           ALPHA = G0/S
         ENDIF
         TOLS2=TOLS*TOLS
         IF (ITOL==1) THEN
c           IF(NSPMD == 1) THEN
c!dir$ omp offload target(mic:MICID)
c     & nocopy(r)
c     & inout(r02)
c!$OMP PARALLEL default(shared) shared(r02)
            CALL MIC_DDOT(NDDL, R, R, R02)
c!$OMP END PARALLEL
c           ELSE
C si spmd penser a faire avant D_Z = D_R*W pour les poids
c!dir$ omp offload target(mic:MICID)
c     & nocopy(r,w,z)
c     & inout(r02)
c!$OMP PARALLEL default(shared) shared(r02)
c             CALL MIC_MV(NDDL,R,W,Z)
c             CALL MIC_DDOT(NDDL, Z, Z, R02)
c!$OMP END PARALLEL
c             IF(IMONM > 0) CALL STARTIME(67,1)
c             CALL SPMD_SUM_S(R02)
c             IF(IMONM > 0) CALL STOPTIME(67,1)
c           END IF
C---------------------
C---------------------
           R2 =R02
         ELSEIF (ITOL==3) THEN
C------ R2<TOL*TOL*ANORM2*XNORM2------
c+1
!$OMP SINGLE
           R02=ABS(G0)
           R2 =R02
           L_A=ONE/ALPHA
C          L_B2=R02
           TNORM2=L_A*L_A
           ANORM2=ZERO
           XNORM2=ZERO
           A_OLD=L_A
           B_OLD=ZERO
           OLDB   = SQRT(R02)
c+1
!$OMP END SINGLE
         ELSEIF (ITOL==4) THEN
           R02=ALPHA*ALPHA*ABS(G0)
           EPS(1)=R02
           R2=R02
         ELSE
           R02=ABS(G0)
           R2 =R02
         ENDIF
         IF (R02==ZERO) THEN
           ISTOP=-1
           GOTO 2060
         ENDIF
c+1
!$OMP SINGLE
         TOLN=R02*TOLS2
         IF(IPRI/=0.AND.ITASK==0)THEN
           RR = SQRT(R2)
           IF(IPRI<0) WRITE(ISTDO,1000)IT,RR
           WRITE(IOUT,1000)IT,RR
         ENDIF
c+1
!$OMP END SINGLE
C-------pour etre coherent avec lanzos for linear
C----------------------
c       CALL MY_BARRIER
C---------------------
         IT=1
c!dir$ omp offload target(mic:MICID)
c     & nocopy(iadk,iadk0,iadm,iadm0,lt_k,lt_k0,lt_m)
c     & nocopy(lt_m0,jdik,jdik0,jdim,jdim0,diag_k,diag_m,y,p,x,r,z)
c!$OMP PARALLEL default(shared)
      CALL MIC_DAXPY(NDDL, ALPHA, P, X)
      CALL MIC_DAXPY(NDDL,-ALPHA, Y, R)

C----------------------
c       CALL MY_BARRIER
C---------------------
C       CALL PREC_SOLVGH(IPREC, ITASK   ,NDDL  ,IADM  , JDIM  ,
C     6                 DIAG_M  , LT_M  ,R     ,Z        ,
C     7                 F_DDL ,L_DDL   )
         IF(IPREC == 1)THEN
C D_Z = D_R
      CALL MIC_DCOPY(NDDL, R, Z)
         ELSEIF(IPREC == 2)THEN
C D_Z = D_R * DIAG_M
      CALL MIC_MV(NDDL,R,DIAG_M,Z)
         ELSEIF(IPREC == 5)THEN
C D_Y = D_R
      CALL MIC_DCOPY(NDDL, R, Y)
C D_Y = D_Y + LT_M * D_R
      CALL MIC_SPMV(NDDL ,Y ,R ,LT_M ,IADM ,JDIM ) 
C D_Z = D_Y * DIAG_M
      CALL MIC_MV(NDDL,Y,DIAG_M,Z)
      CALL MIC_DCOPY(NDDL, Z, Y)
C D_Z = D_Z + LT_M0 * D_Y
      CALL MIC_SPMV(NDDL ,Z ,Y ,LT_M0,IADM0,JDIM0) 
         ELSE
         END IF
c!$OMP END PARALLEL
C----------------------
c         IF(NSPMD > 1 .AND. IPREC > 1)THEN
c           IF(IMONM > 0) CALL STARTIME(68,1)
c!dir$ omp offload target(mic:MICID)
c     & nocopy(z,ifr2k)
c     & out(vgat:length(lcom))
c!$OMP PARALLEL default(shared)
c           CALL MIC_GATHER(NDDL, LCOM, Z, VGAT, IFR2K)
c!$OMP END PARALLEL
c           IF(IMONM > 0) CALL STOPTIME(68,1)
c           IF(IMONM > 0) CALL STARTIME(66,1)
c           CALL SPMD_SUMFC_V(VGAT,VSCA,INDEX,LCOM)
c           IF(IMONM > 0) CALL STOPTIME(66,1)
c           IF(IMONM > 0) CALL STARTIME(69,1)
c!dir$ omp offload target(mic:MICID)
c     & nocopy(z,ifr2k,index)
c     & in(vsca:length(lcom))
c!$OMP PARALLEL default(shared)
c           CALL MIC_SCATTER(NDDL, LCOM, Z, VSCA, IFR2K, INDEX)          
c!$OMP END PARALLEL
c           IF(IMONM > 0) CALL STOPTIME(69,1)
c         END IF
C---------------------
c         IF(NSPMD == 1) THEN
c!dir$ omp offload target(mic:MICID)
c     & nocopy(r,z)
c     & inout(G1)
c!$OMP PARALLEL default(shared) shared(G1)
          CALL MIC_DDOT(NDDL, R, Z, G1)
c!$OMP END PARALLEL
c         ELSE
C si spmd penser a faire avant D_Y = D_Z*D_W pour les poids
c!dir$ omp offload target(mic:MICID)
c     & nocopy(z,w,r,y)
c     & inout(G1)
c!$OMP PARALLEL default(shared) shared(G1)
c           CALL MIC_MV(NDDL,Z,W,Y)
c           CALL MIC_DDOT(NDDL, R, Y, G1)
c!$OMP END PARALLEL
c           IF(IMONM > 0) CALL STARTIME(67,1)
c           CALL SPMD_SUM_S(G1)
c           IF(IMONM > 0) CALL STOPTIME(67,1)
c         END IF
C---------------------
         BETA=G1/G0
         IF (ITOL==1) THEN
c           IF(NSPMD == 1) THEN
c!dir$ omp offload target(mic:MICID)
c     & nocopy(r)
c     & inout(R2)
c!$OMP PARALLEL default(shared) shared(R2)
             CALL MIC_DDOT(NDDL, R, R, R2)
c!$OMP END PARALLEL
c           ELSE
C si spmd penser a faire avant D_Y = D_R*W pour les poids
c!dir$ omp offload target(mic:MICID)
c     & nocopy(r,w,y)
c     & inout(R2)
c!$OMP PARALLEL default(shared) shared(R2)
c             CALL MIC_MV(NDDL,R,W,Y)
c             CALL MIC_DDOT(NDDL, Y, Y, R2)
c!$OMP END PARALLEL
c             IF(IMONM > 0) CALL STARTIME(67,1)
c             CALL SPMD_SUM_S(R2)
c             IF(IMONM > 0) CALL STOPTIME(67,1)
c           END IF
         ELSEIF (ITOL==3) THEN
C------ R2<TOL*TOL*ANORM2*XNORM2------
c+1
!$OMP SINGLE 
           R2=ABS(G1)
           L_B2=ABS(BETA)*A_OLD*A_OLD
           L_B=SQRT(L_B2)
           TNORM2=TNORM2+L_B2
           B_OLD=BETA
C     INITIALIZE OTHER QUANTITIES.
           GBAR   = L_A
           DBAR   = L_B
           RHS1   = OLDB
           RHS2   = ZERO
           GMAX   = ABS( L_A ) + EPS_M
           GMIN   = GMAX
           OLDB2   = L_B2
           OLDB   = L_B
c+1
!$OMP END SINGLE
         ELSEIF (ITOL==4) THEN
           R2=R02
         ELSE
           R2=ABS(G1)
         ENDIF
         G0 = G1
c+1
!$OMP SINGLE
         IF (ITOL==3) TOLN=TOLN*TNORM2


C----------------------
c         CALL MY_BARRIER
C---------------------
C         ISTOP=CRIT_STOP(IT,R2,NLIM,TOLN)
         IF (IT>=NLIM) THEN
           ISTOP = 0
         ELSE
           IF (R2<=TOLN) THEN
             ISTOP = 0
           ELSE
             ISTOP = 1
           ENDIF
         ENDIF
c+1
!$OMP END SINGLE
C----------------------
c         LOOP OVER ITERATIONS
C---------------------

         DO WHILE (ISTOP==1)

c!dir$ omp offload target(mic:MICID)
c     & nocopy(iadk,iadk0,iadm,iadm0,lt_k,lt_k0,lt_m)
c     & nocopy(lt_m0,jdik,jdik0,jdim,jdim0,diag_k,diag_m,y,p,x,r,z)
c!$OMP PARALLEL default(shared)

C D_P = D_Z + BETA*D_P in 2 step : D_P = BETA*D_P ; D_P = D_P + D_Z
          CALL MIC_DSCAL(NDDL, BETA, P)
          CALL MIC_DAXPY(NDDL, ONE, Z, P)
C----------------------
c        CALL MY_BARRIER
C---------------------
C        CALL MAV_LTGH(
C     1            NDDL  ,IADK  ,JDIK  ,DIAG_K,LT_K  ,   
C     2            P     ,Y     ,F_DDL ,L_DDL ,ITASK )
C D_Y = D_P * DIAG_K
          CALL MIC_MV(NDDL,P,DIAG_K,Y)
C D_Y = D_Y + LT_K * D_P
          CALL MIC_SPMV(NDDL ,Y ,P ,LT_K ,IADK ,JDIK ) 
C D_Y = D_Y + LT_K0 * D_P
          CALL MIC_SPMV(NDDL ,Y ,P ,LT_K0,IADK0,JDIK0) 
          IF(NDDLI > 0) THEN
C D_Y = D_Y + LT_I0 * D_P
ctempo      CALL MIC_SPMV(NDDL ,Y ,P ,LT_I0,IADI0,JDII0) 
          END IF
c!$OMP END PARALLEL
c          IF(NSPMD > 1)THEN
c           IF(IMONM > 0) CALL STARTIME(68,1)
c!dir$ omp offload target(mic:MICID)
c     & nocopy(y,ifr2k)
c     & out(vgat:length(lcom))
c!$OMP PARALLEL default(shared)
c           CALL MIC_GATHER(NDDL, LCOM, Y, VGAT, IFR2K)
c!$OMP END PARALLEL
c           IF(IMONM > 0) CALL STOPTIME(68,1)
c           IF(IMONM > 0) CALL STARTIME(66,1)
c           CALL SPMD_SUMFC_V(VGAT,VSCA,INDEX,LCOM)
c           IF(IMONM > 0) CALL STOPTIME(66,1)
c           IF(IMONM > 0) CALL STARTIME(69,1)
c!dir$ omp offload target(mic:MICID)
c     & nocopy(y,ifr2k,index)
c     & in(vsca:length(lcom))
c!$OMP PARALLEL default(shared)
c           CALL MIC_SCATTER(NDDL, LCOM, Y, VSCA, IFR2K, INDEX)          
c!$OMP END PARALLEL
c           IF(IMONM > 0) CALL STOPTIME(69,1)
c          END IF
C---------------------
c          IF(NSPMD == 1) THEN
c!dir$ omp offload target(mic:MICID)
c     & nocopy(p,y)
c     & inout(S)
c!$OMP PARALLEL default(shared) shared(S)
            CALL MIC_DDOT(NDDL, P, Y, S)
c!$OMP END PARALLEL
c          ELSE
C si spmd penser a faire avant D_Y = D_Y*W pour les poids
c!dir$ omp offload target(mic:MICID)
c     & nocopy(y,w,p,z)
c     & inout(S)
c!$OMP PARALLEL default(shared) shared(S)
c            CALL MIC_MV(NDDL,Y,W,Z)
c            CALL MIC_DDOT(NDDL, P, Z, S)
c!$OMP END PARALLEL
c            IF(IMONM > 0) CALL STARTIME(67,1)
c            CALL SPMD_SUM_S(S)
c            IF(IMONM > 0) CALL STOPTIME(67,1)
c          END IF
C---------------------
          ALPHA=G0/S
c!dir$ omp offload target(mic:MICID)
c     & nocopy(iadk,iadk0,iadm,iadm0,lt_k,lt_k0,lt_m)
c     & nocopy(lt_m0,jdik,jdik0,jdim,jdim0,diag_k,diag_m,y,p,x,r,z)
c!$OMP PARALLEL default(shared)
          CALL MIC_DAXPY(NDDL, ALPHA, P, X)
          CALL MIC_DAXPY(NDDL,-ALPHA, Y, R)
C----------------------
C        CALL PREC_SOLVGH(IPREC , ITASK ,NDDL  ,IADM  , JDIM  ,
C     6                  DIAG_M, LT_M  ,R     ,Z     ,
C     7                  F_DDL ,L_DDL   )
          IF(IPREC == 1)THEN
C D_Z = D_R
            CALL MIC_DCOPY(NDDL, R, Z)
          ELSEIF(IPREC == 2)THEN
C D_Z = D_R * DIAG_M
            CALL MIC_MV(NDDL,R,DIAG_M,Z)
          ELSEIF(IPREC == 5)THEN
C D_Y = D_R
            CALL MIC_DCOPY(NDDL, R, Y)
C D_Y = D_Y + LT_M * D_R
            CALL MIC_SPMV(NDDL ,Y ,R ,LT_M ,IADM ,JDIM ) 
C D_Z = D_Y * DIAG_M
            CALL MIC_MV(NDDL,Y,DIAG_M,Z)
            CALL MIC_DCOPY(NDDL, Z, Y)
C D_Z = D_Z + LT_M0 * D_Y
            CALL MIC_SPMV(NDDL ,Z ,Y ,LT_M0,IADM0,JDIM0) 
          ELSE
          END IF
c!$OMP END PARALLEL
C----------------------
c          IF(NSPMD > 1 .AND. IPREC > 1)THEN
c           IF(IMONM > 0) CALL STARTIME(68,1)
c!dir$ omp offload target(mic:MICID)
c     & nocopy(z,ifr2k)
c     & out(vgat:length(lcom))
c!$OMP PARALLEL default(shared)
c           CALL MIC_GATHER(NDDL, LCOM, Z, VGAT, IFR2K)
c!$OMP END PARALLEL
c           IF(IMONM > 0) CALL STOPTIME(68,1)
c           IF(IMONM > 0) CALL STARTIME(66,1)
c           CALL SPMD_SUMFC_V(VGAT,VSCA,INDEX,LCOM)
c           IF(IMONM > 0) CALL STOPTIME(66,1)
c           IF(IMONM > 0) CALL STARTIME(69,1)
c!dir$ omp offload target(mic:MICID)
c     & nocopy(z,ifr2k,index)
c     & in(vsca:length(lcom))
c!$OMP PARALLEL default(shared)
c           CALL MIC_SCATTER(NDDL, LCOM, Z, VSCA, IFR2K, INDEX)          
c!$OMP END PARALLEL
c           IF(IMONM > 0) CALL STOPTIME(69,1)
c          END IF
C---------------------
c          IF(NSPMD == 1) THEN
c!dir$ omp offload target(mic:MICID)
c     & nocopy(r,z)
c     & inout(G1)
c!$OMP PARALLEL default(shared) shared(G1)
            CALL MIC_DDOT(NDDL, R, Z, G1)
c!$OMP END PARALLEL
c          ELSE
C si spmd penser a faire avant D_Y = D_Z*W pour les poids
c!dir$ omp offload target(mic:MICID)
c     & nocopy(z,w,r,y)
c     & inout(G1)
c!$OMP PARALLEL default(shared) shared(G1)
c            CALL MIC_MV(NDDL,Z,W,Y)
c            CALL MIC_DDOT(NDDL, R, Y, G1)
c!$OMP END PARALLEL
c            IF(IMONM > 0) CALL STARTIME(67,1)
c            CALL SPMD_SUM_S(G1)
c            IF(IMONM > 0) CALL STOPTIME(67,1)
c          END IF
C---------------------
c+1
!$OMP single 
          BETA=G1/G0
          G0 = G1
c+1
!$OMP end single 
          IF (ITOL==1) THEN
c            IF(NSPMD == 1) THEN
c!dir$ omp offload target(mic:MICID)
c     & nocopy(r)
c     & inout(R2)
c!$OMP PARALLEL default(shared) shared(R2)
              CALL MIC_DDOT(NDDL, R, R, R2)
c!$OMP END PARALLEL
c            ELSE
C si spmd penser a faire avant D_Y = D_R*W pour les poids
c!dir$ omp offload target(mic:MICID)
c     & nocopy(r,w,y)
c     & inout(R2)
c!$OMP PARALLEL default(shared) shared(R2)
c              CALL MIC_MV(NDDL,R,W,Y)
c              CALL MIC_DDOT(NDDL, Y, Y, R2)
c!$OMP END PARALLEL
c              IF(IMONM > 0) CALL STARTIME(67,1)
c              CALL SPMD_SUM_S(R2)
c              IF(IMONM > 0) CALL STOPTIME(67,1)
c            END IF
          ELSEIF (ITOL==3) THEN
c+1
!$OMP single 
           R2 =ABS(G1)
           S=ONE/ALPHA
           L_A=S+B_OLD*A_OLD
C----------L_B2 : beta(j-1)^2-A_OLD :1/alpha(j-1)-------
           L_B2=ABS(BETA)*S*S
           L_B=SQRT(L_B2)
           A_OLD=S
           B_OLD=BETA
           ANORM2=TNORM2
           TNORM2=TNORM2+L_A*L_A+OLDB2+L_B2
           GAMMA  = SQRT( GBAR*GBAR + OLDB2 )
           CS     = GBAR / GAMMA
           SN     = OLDB / GAMMA
           DELTA  = CS * DBAR  +  SN * L_A
           GBAR   = SN * DBAR  -  CS * L_A
           EPSLN  = SN * L_B
           DBAR   =            -  CS * L_B
           ZL     = RHS1 / GAMMA
           XNORM2 = XNORM2+ZL*ZL
           GMAX   = MAX( GMAX, GAMMA )
           GMIN   = MIN( GMIN, GAMMA )
           RHS1   = RHS2  -  DELTA * ZL
           RHS2   =       -  EPSLN * ZL
           TOLN=TOLS2*ANORM2*XNORM2
           OLDB2   = L_B2
           OLDB   = L_B
c+1
!$OMP end single 
          ELSEIF (ITOL==4) THEN
           TMP=ALPHA*ALPHA*ABS(G1)
           IF (IT>=ND) THEN
            DO J=1,ND-1
             EPS(J)=EPS(J+1)
            ENDDO
            EPS(ND)=TMP
            R2=ZERO
            DO J=1,ND
             R2=R2+EPS(J)
            ENDDO
           ELSE
            EPS(IT+1)=TMP
            R2=R2+TMP
           ENDIF
          ELSE
           R2 =ABS(G1)
          ENDIF
c+1
!$OMP single 
          IF(IPRI/=0.AND.ITASK==0)THEN
           IF(MOD(IT,IP)==0)THEN
            RR = SQRT(R2/R02)
            WRITE(IOUT,1001)IT,RR
            IF(IPRI<0) WRITE(ISTDO,1001)IT,RR
           ENDIF
          ENDIF
C----------------------
c          CALL MY_BARRIER
C---------------------
C          ISTOP=CRIT_STOP(IT,R2,NLIM,TOLN)
          IF (IT>=NLIM) THEN
            ISTOP = 0
          ELSE
            IF (R2<=TOLN) THEN
              ISTOP = 0
            ELSE
              ISTOP = 1
            ENDIF
          ENDIF
C----------------------
c        CALL MY_BARRIER
C---------------------
          IT = IT +1
c+1
!$OMP end single
         ENDDO
 2060    CONTINUE
!$OMP single 
         tt=OMP_GET_WTIME()-tt
         i= omp_get_num_threads()
         print *,' '
         print *,' '
         print *,' Execution time on MIC with',i,' THREADS'
         print *,' ==> ',tt,' SECONDS'
         print *,' '
!$OMP END single 
c+1
!$OMP END PARALLEL
         WRITE(IOUT,1001)IT-1,RR
      END IF ! fin NSPMD ==1
      TF=OMP_GET_WTIME()
!dec$ omp offload target(mic:MICID) out(X,R:alloc_if(.false.))
!$OMP PARALLEL 
c!$OMP single 
c      tt=OMP_GET_WTIME()-tt
c      i= omp_get_num_threads()
c      print *,' '
c      print *,' '
c      print *,' Execution time on MIC with',i,' THREADS'
c      print *,' ==> ',tt,' SECONDS'
c      print *,' '
c!$OMP END single 
!$OMP END PARALLEL
      TF=OMP_GET_WTIME()-TF
      print *,'Transfer Time MIC=>CPU (s)   =',TF
      print *,'Transfer Rate MIC=>CPU (MB/s)=',
     .        ((2*NDDL/(1024*1024))*8)/TF
c      END IF ! fin NSPMD ==1
C fin code provisoire

c      IF(NSPMD > 1) THEN
c        DEALLOCATE(W)
c        DEALLOCATE(VGAT)
c        DEALLOCATE(VSCA)
c        DEALLOCATE(INDEX)
c      END IF

      END IF ! fin GPUR4R8==2 (MIC double precision)

      END IF   !only task0
C fin code specifique MIC double precision
#endif
C fin code specifique GPU
C routine speciale pour sauvegarder les resultats
      IF(ISAVE==1)CALL IMP_RSAVE(NDDL,X,R)
      
C
C------due to ISTOP local variable
C      IF(IT>=NLIM.AND.ISOLV==7) ISTOP = 3
      IF(ITASK == 0) THEN
#include "lockon.inc"
        ISTOP_H = ISTOP
#include "lockoff.inc"
       CALL PUPD(N_MAX,NDDLI_G,IT)
C       
       IF(IT>=NLIM)THEN
        IF (ISOLV==7) THEN
#include "lockon.inc"
         ISTOP_H = 3
#include "lockoff.inc"
        ELSE
#include "lockon.inc"
        ISTOP_H = 1
#include "lockoff.inc"

        IF (.NOT.MIXEDSOL()) THEN
        WRITE(IOUT,*)
        WRITE(IOUT,1003)NLIM
        WRITE(IOUT,*)
        WRITE(ISTDO,*)
        WRITE(ISTDO,1003)NLIM
        WRITE(ISTDO,*)
        IF(ITOL==3)THEN
          WRITE(IOUT,2000)  
     .     EXIT, SQRT(ANORM2), SQRT(XNORM2),GMAX/GMIN
          WRITE(ISTDO,2000)  
     .     EXIT, SQRT(ANORM2), SQRT(XNORM2),GMAX/GMIN
        ENDIF
C----add condition to remove wrong message with mix solver        
        IF (R2>HUNDRED*TOLN) THEN
         ISTOP_H = 2
         RR = SQRT(R2/R02)
         WRITE(IOUT,*)
         WRITE(IOUT,1004)RR
         WRITE(IOUT,*)
         WRITE(ISTDO,*)
         WRITE(ISTDO,1004)RR
         WRITE(ISTDO,*)
        ENDIF
        END IF !(.NOT.(MIXEDSOL)) THEN
         
        END IF !(ISOLV==7) THEN
       ENDIF
       IF(IPRI/=0)THEN
        RR = SQRT(R2/R02)
        WRITE(IOUT,*)
        WRITE(IOUT,1002)IT,RR
        IF(ITOL==3)WRITE(IOUT,2000)  
     .     EXIT, SQRT(ANORM2), SQRT(XNORM2),GMAX/GMIN
        WRITE(IOUT,*)
        IF(IPRI<0) THEN
         WRITE(ISTDO,*)
         WRITE(ISTDO,1002)IT,RR
         IF(ITOL==3)WRITE(ISTDO,2000)
     .     EXIT, SQRT(ANORM2), SQRT(XNORM2),GMAX/GMIN
         WRITE(ISTDO,*)
        ENDIF
       ENDIF
       IF(ITOL==3) THEN
        K_LAMDA0= GMIN
        K_LAMDA1= GMAX
       ELSE
        K_LAMDA0= ZERO
        K_LAMDA1= ZERO
       ENDIF
      END IF !(ITASK == 0) THEN
C----------------------
      CALL MY_BARRIER
C----------------------
#include "lockon.inc"
      ISTOP = ISTOP_H
#include "lockoff.inc"

      CALL MY_BARRIER
C--------------------------------------------
 1000 FORMAT(5X,'ITERATION=',I8,5X,'  INITIAL RESIDUAL NORM=',E11.4)
 1001 FORMAT(5X,'ITERATION=',I8,5X,' RELATIVE RESIDUAL NORM=',E11.4)
 1002 FORMAT(3X,'TOTAL C.G. ITERATION=',I8,5X,
     .          ' RELATIVE RESIDUAL NORM=',E11.4)
 1003 FORMAT(5X,
     . '---WARNING : THE ITERATION LIMIT NUMBER WAS REACHED',I8)
 1004 FORMAT(5X,'WARNING:C.G STOP WITH RELATIVE RESIDUAL NORM=',E11.4)
 2000 FORMAT(/ 5X, A, 2X, 'ANORM =', E12.4, 2X, 'XNORM =', E12.4,2X,
     .       'KCOND =', E12.4)
 2002 FORMAT(/ 5X, 'WITH', 2X, 'ALFA =', E12.4, 2X, 'BETA =', 
     .       E12.4,2X,'OLDB =', E12.4)
      RETURN
      END

#endif
Chd|====================================================================
Chd|  PUPD                          source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|        IMP_PPCGH                     source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PUPD(NDDL,NDDLI,ITN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL,NDDLI,ITN
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,NLIM
C-----------------------------
C      
      IT_PCG = IT_PCG + ITN
      IF (ISOLV==5.OR.ISOLV==6) THEN
       IF (IPUPD>0) THEN
        NLIM=IPUPD+NDDLI/10
       ELSE
        NLIM=MAX(5,NDDL/10000)+NDDLI/10
       ENDIF
       IF (ITN>NLIM) THEN
c         WRITE(IOUT,*) '****RE-FRACTO. DUE TO ITN=',ITN
         IT_PCG = -IT_PCG
       ENDIF
      END IF
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  NLIM_MIX                      source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|        IMP_PPCGH                     source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE NLIM_MIX(NDDL,NDDLI,NLIM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL,NDDLI,NLIM
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I
C-----------------------------
       IF (ISOLV/=5.AND.ISOLV/=6) RETURN
       IF (IPUPD>0) THEN
        NLIM=10*IPUPD+NDDLI
       ELSE
        NLIM=10+NDDL/100+NDDLI
       ENDIF
       NLIM=MIN(NLIM,NDDL)
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  MIXEDSOL                      source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|====================================================================
      LOGICAL FUNCTION MIXEDSOL()
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C----6---------------------------------------------------------------7---------8
       MIXEDSOL=.FALSE.
       IF ((ISOLV==5.OR.ISOLV==6).AND.IT_PCG<0) MIXEDSOL=.TRUE.
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_INIX                      source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PPCGH                     source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|        MAV_MN                        source/implicit/produt_v.F    
Chd|        MAV_NM                        source/implicit/produt_v.F    
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        IMP_PCG_PROJ                  share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE IMP_INIX(F_DDL,L_DDL,NDDL,X  ,R ,W_DDL,ITASK )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
         USE IMP_PCG_PROJ
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  F_DDL,L_DDL,NDDL,ITASK,W_DDL(*)
C     REAL
      my_real
     .  X(*), R(*) 
C-----------------------------------------------
c FUNCTION: initialization of X0=[S][LAMDA]^-1[S]^t{R}
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   NDDL              - equation dimension
c  I   NPV               - projection vector number
c  I   W_DDL(*)          - itag for each id of subdomains
c  I   F_ND,L_ND,ITASK   - id in each ITASK:thread id (//)
c  I   R(NDDL)           - right-hand vector
c  O   X(NDDL)           - X0
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER I,J,M,NPV
C----------------------
       IF (NPCGPV == 0) RETURN
        NPV = NPCGPV
C----------------------
C        {W}=[S]^t{R}
C---------------------
       CALL MAV_NM(F_DDL,L_DDL,NDDL ,NPV   ,PROJ_S   ,
     .             R    ,PROJ_W,W_DDL,ITASK )
C----------------------
      CALL MY_BARRIER
C---------------------
C        [LAMDA]^-1{W}
C---------------------
      IF (ITASK ==0) THEN
       DO I=1,NPV
        PROJ_W(I)=PROJ_W(I)*PROJ_LA_1(I)
       ENDDO
C---------------------
C       {X0}=[S]{W}
C---------------------
       CALL MAV_MN(NDDL ,NPV   ,PROJ_S    ,PROJ_W ,X      ,ITASK )
C----------------------
      END IF !(ITASK ==0) THEN
C----------------------
      RETURN
      END
Chd|====================================================================
Chd|  IMP_PRO_P                     source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PPCGH                     source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|        MAV_MN                        source/implicit/produt_v.F    
Chd|        MAV_NM                        source/implicit/produt_v.F    
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        IMP_PCG_PROJ                  share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE IMP_PRO_P(F_DDL,L_DDL,NDDL ,P  ,W_DDL,ITASK )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
         USE IMP_PCG_PROJ
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  F_DDL ,L_DDL ,NDDL,ITASK,W_DDL(*)
C     REAL
      my_real
     .  P(*) 
C-----------------------------------------------
c FUNCTION: Projection of {p}={p}-[S][LAMDA]^-1[T]^t{p}
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   NDDL              - equation dimension
c  I   NPV               - projection vector number
c  I   W_DDL(*)          - itag for each id of subdomains
c  I   F_ND,L_ND,ITASK   - id in each ITASK:thread id (//)
c  IO  P(NDDL)           - PCG p vector
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER I,J,NPV
       my_real
     .  P1(NDDL)
C----------------------
       IF (NPCGPV == 0) RETURN
        NPV = NPCGPV
C----------------------
C        {W}=[T]^t{p}
C---------------------
       CALL MAV_NM(F_DDL,L_DDL,NDDL ,NPV  ,PROJ_T ,
     .             P    ,PROJ_W ,W_DDL,ITASK )
C----------------------
      CALL MY_BARRIER
C---------------------
C        [LAMDA]^-1{W}
C---------------------
      IF (ITASK==0) THEN
        DO I=1,NPV
         PROJ_W(I)=PROJ_W(I)*PROJ_LA_1(I)
        ENDDO
C----------------------
C       {P1}=[S]{W}
C---------------------
       CALL MAV_MN(NDDL ,NPV   ,PROJ_S   ,PROJ_W  ,P1      ,ITASK )
        DO I=1,NDDL
         P(I)=P(I)-P1(I)
        ENDDO
      END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_INIS                      source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_INISI                     source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|        ALEAT                         source/system/aleat.F         
Chd|====================================================================
      SUBROUTINE IMP_INIS(ND ,F_ND ,L_ND,   NPF,   NPL,   S   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  ND,F_ND ,L_ND,NPF,   NPL
C     REAL
      my_real
     .  S(ND,*),ALEAT
      EXTERNAL ALEAT
C-----------------------------------------------
c FUNCTION: initialization of [S] 
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   F_ND ,L_ND        - equation dimension divised by Nthead
c  I   NPF,NPL           - projection vector number (first to last)
c  I   ND                - equation dimension
c  I   S(ND,NPV)         - S-Matrix
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER I,J,M,INORM
C---------------------
       DO J=NPF,NPL
       DO I=F_ND ,L_ND
        S(I,J)=ALEAT()
       ENDDO
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_UPDV2                     source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PPCGH                     source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        MMAV_LTH                      source/implicit/produt_v.F    
Chd|        PRODUT_H                      source/implicit/produt_v.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        IMP_PCG_PROJ                  share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
       SUBROUTINE IMP_UPDV2( 
     1                    NDDL  ,NDDLI ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,ITOK  ,IADI  ,JDII  ,LT_I   ,
     3                    ITASK ,W_DDL ,A     ,AR    ,VE     ,
     4                    D     ,DR    ,NDOF  ,IPARI ,INTBUF_TAB,
     5                    NUM_IMP,NS_IMP,NE_IMP,NSREM ,
     6                    NSL   ,NMONV  ,IMONV ,MONVOL,IGRSURF ,
     7                    VOLMON,FR_MV ,IBFV  ,SKEW  ,
     8                    XFRAME ,IND_IMP,XI_C ,MS    ,XE    ,
     9                    IRBE3 ,LRBE3 ,IRBE2 ,LRBE2  ,U_TMP ,
     A                    P     ,Y     ,F_DDL ,L_DDL  ,IT    ,
     B                    ICHECK,Z     ,IADM  ,JDIM  ,DIAG_M,LT_M)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
         USE IMP_PCG_PROJ
         USE INTBUFDEF_MOD
         USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  F_DDL,L_DDL,NDDL  ,IADK(*)  ,JDIK(*),
     .         NDDLI ,ITOK(*) ,IADI(*),JDII(*),
     .         ITASK,W_DDL(*),IBFV(*),IRBE3(*) ,LRBE3(*),
     .         IRBE2(*) ,LRBE2(*),IT,ICHECK,IADM(*),JDIM(*) 
      INTEGER  NDOF(*),NE_IMP(*),NSREM ,NSL,
     .         IPARI(*) ,NUM_IMP(*),NS_IMP(*) ,IND_IMP(*)
      INTEGER NMONV,IMONV(*),MONVOL(*),FR_MV(*)
C     REAL
        my_real
     .  DIAG_K(*), LT_K(*),LT_I(*) ,XI_C(*),U_TMP(*),P(*),Y(*)
        my_real
     .  A(3,*),AR(3,*),VE(3,*),D(3,*),DR(3,*),XE(3,*),
     .  MS(*),VOLMON(*),SKEW(*) ,XFRAME(*),
     .  Z(*),DIAG_M(*),LT_M(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
c FUNCTION: update S,T of Projection
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   NDDL,NNZ          - dimension of [K]  and number of non zero (strict triangular)
c  I   IADK,JDIK         - indice arrays for compressed row(col.) format of [K]
c  I   DIAG_K(NDDL)      - diagonal terms of [K]
c  I   LT_K(NNZ)         - strict triangular of [K]
c  O   Proj_S(NDDL,M_VS)   - [S] reduced small Eigenvectors
c  O   Proj_T(NDDL,M_VS)   - [T] =[K][S]
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#if defined(MYREAL8) && !defined(WITHOUT_LINALG)
       CHARACTER          JOBZ, UPLO
       INTEGER I,J,IP,NLIM,ND,IUPD,IPRI,IERROR,NNZI,M,
     .         F_DDLI,L_DDLI,INFO,LW,M_VS1,INORM,NP
       my_real
     .  WORK(3*M_VS+3),W(M_VS+1),P2,PY,S,VP,VY,P_K2(2,2),
     .  WORK_T(NDDL)
C--- COMPUTE another lowest eigenvecor V(U_TMP)
        IF (IPRO_S0/=4) RETURN
C	
        CALL PRODUT_H( F_DDL  ,L_DDL ,P   ,P  ,W_DDL , P2 ,ITASK )
        CALL PRODUT_H( F_DDL  ,L_DDL ,P   ,Y  ,W_DDL , PY ,ITASK )
       IF (IT==0 ) THEN
        M_TMP(1,1)=ONE
	S = ONE/SQRT(P2)
        K_TMP(1,1)=PY/P2
        DO I=F_DDL,L_DDL
         U_TMP(I)=S*P(I)
        ENDDO
       ELSE
        CALL PRODUT_H( F_DDL  ,L_DDL ,P   ,U_TMP,W_DDL , VP ,ITASK )
        CALL PRODUT_H( F_DDL  ,L_DDL ,Y   ,U_TMP,W_DDL , VY ,ITASK )
        M_TMP(1,2)=VP
        M_TMP(2,2)=P2
        K_TMP(1,2)=VY
        K_TMP(2,2)=PY
	ND =2
        JOBZ='V'
        UPLO='U'
        LW=3*ND
        CALL DSYGV( 1, JOBZ, UPLO, ND, K_TMP, ND, M_TMP, ND, W, WORK,
     $              LW, INFO )
        DO I=F_DDL,L_DDL
         U_TMP(I)=K_TMP(1,1)*U_TMP(I)+K_TMP(2,1)*P(I)
        ENDDO
        M_TMP(1,1)=ONE
        K_TMP(1,1)=W(1)
       ENDIF
C-------checking V(U_TMP)      
      if (icheck==1.OR.IMPDEB>0) then
        CALL PRODUT_H( F_DDL  ,L_DDL ,U_TMP,U_TMP,W_DDL , S ,ITASK )
       CALL MMAV_LTH(
     1            NDDL  ,NDDLI ,IADK  ,JDIK  ,DIAG_K,   
     2            LT_K  ,IADI  ,JDII  ,ITOK  ,LT_I  ,
     3            U_TMP ,WORK_T,A     ,AR    ,
     5            VE    ,MS    ,XE    ,D     ,DR    ,
     6            NDOF  ,IPARI ,INTBUF_TAB   ,NUM_IMP,
     7            NS_IMP,NE_IMP,NSREM ,NSL   ,IBFV   ,
     8            SKEW  ,XFRAME,MONVOL,VOLMON,IGRSURF ,
     9            FR_MV,NMONV ,IMONV ,IND_IMP,
     A            XI_C  ,IUPD  ,IRBE3 ,LRBE3 ,IRBE2  ,
     B            LRBE2 ,IADM  ,JDIM  ,DIAG_M,LT_M   ,
     C            F_DDL ,L_DDL ,ITASK ,Z     )
        CALL PRODUT_H( F_DDL  ,L_DDL ,U_TMP,WORK_T,W_DDL ,P2,ITASK)
        write(iout,*)'inf,|v|,vAv,lamda1=',info,s,p2,K_TMP(1,1)
      endif

#else
       CALL ARRET(5)
#endif
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_UPDV1                     source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PPCGH                     source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE IMP_UPDV1( 
     1                    R2  ,R2_OLD  ,RMAX  ,PROJ_U,P      ,   
     2                    IT   ,F_DDL,L_DDL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IT,F_DDL,L_DDL
C     REAL
        my_real
     .  R2, R2_OLD  ,RMAX  ,PROJ_U(*),P(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER I
       my_real
     .  RR
C-----------------------------------     
        IF (IPRO_S0==4) RETURN
C	
          IF (IT==1) THEN
           R2_OLD=R2
          ELSE
           RR=R2/R2_OLD
           R2_OLD=R2
           IF (RMAX<RR) THEN
            DO I=F_DDL,L_DDL
             PROJ_U(I) = P(I)
            ENDDO
            RMAX=RR
           ENDIF 
          END IF
C
      RETURN
      END
#ifdef MUMPS5
Chd|====================================================================
Chd|  IMP_PPCGH                     source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        LIN_SOLVH0                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVH1                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVIH2                   source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        IMP_INISI                     source/implicit/imp_pcg.F     
Chd|        IMP_INIST                     source/implicit/imp_pcg.F     
Chd|        IMP_INIX                      source/implicit/imp_pcg.F     
Chd|        IMP_PRO_P                     source/implicit/imp_pcg.F     
Chd|        IMP_UPDST                     source/implicit/imp_pcg.F     
Chd|        IMP_UPDV1                     source/implicit/imp_pcg.F     
Chd|        IMP_UPDV2                     source/implicit/imp_pcg.F     
Chd|        MMAV_LTH                      source/implicit/produt_v.F    
Chd|        MMV_LH                        source/implicit/produt_v.F    
Chd|        MMV_LTH                       source/implicit/produt_v.F    
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        NLIM_MIX                      source/implicit/imp_pcg.F     
Chd|        PRODUT_H                      source/implicit/produt_v.F    
Chd|        PUPD                          source/implicit/imp_pcg.F     
Chd|        CRIT_STOP                     source/implicit/imp_pcg.F     
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        IMP_PCG_PROJ                  share/modules/impbufdef_mod.F 
Chd|        IMP_WORKH                     share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IMP_PPCGH( IPREC ,
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,NDDLI ,ITOK  ,IADI  ,JDII   ,
     3                    LT_I  ,NNZM  ,IADM  ,JDIM  ,DIAG_M ,
     4                    LT_M  ,X     ,R     ,ITOL  ,TOL    ,
     5                    P     ,Z     ,Y     ,ITASK ,IPRINT ,
     6                    N_MAX ,EPS_M ,F_X   ,ISTOP ,W_DDL ,
     8                    A     ,AR    ,VE    ,MS    ,XE    ,
     9                    D     ,DR    ,NDOF  ,IPARI ,INTBUF_TAB,
     A                    NUM_IMP,NS_IMP,NE_IMP,NSREM ,
     B                    NSL   ,NMONV  ,IMONV ,MONVOL,IGRSURF ,
     C                    VOLMON,FR_MV ,IBFV  ,SKEW  ,
     D                    XFRAME ,GRAPHE,IAD_ELEM,FR_ELEM,ITAB , 
     E                    INSOLV ,ITN   ,FAC_K ,IPIV_K,NK    ,
     F                    MUMPS_PAR,CDDLP,ISOLV,IDSC  ,IDDL  ,
     G                    IKC    ,INLOC ,IND_IMP,XI_C ,R0    ,
     H                    NDDLI_G,INTP_C,IRBE3 ,LRBE3 ,IRBE2 ,
     I                    LRBE2  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
      USE IMP_WORKH
      USE IMP_PCG_PROJ
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "dmumps_struc.h"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C----------resol [K]{X}={F}---------
      INTEGER  NDDL  ,NNZ   ,IADK(*)  ,JDIK(*),ITOL,
     .         NDDLI ,ITOK(*) ,IADI(*),JDII(*),N_MAX,
     .         NNZM  ,IADM(*),JDIM(*),IPREC,ITASK,IPRINT,
     .         ISTOP,W_DDL(*),IBFV(*),INTP_C,IRBE3(*) ,LRBE3(*),
     .         IRBE2(*) ,LRBE2(*)
      INTEGER  NDOF(*),NE_IMP(*),NSREM ,NSL,
     .         IPARI(*) ,NUM_IMP(*),NS_IMP(*) ,IND_IMP(*)
      INTEGER NMONV,IMONV(*),MONVOL(*),FR_MV(*)
      INTEGER IAD_ELEM(2,*), FR_ELEM(*), ITAB(*),  
     .        INSOLV, ITN, IPIV_K(*), NK, CDDLP(*), ISOLV, IDSC,
     .        IDDL(*), IKC(*), INLOC(*),NDDLI_G
C     REAL
      my_real
     .  DIAG_K(*), LT_K(*) ,DIAG_M(*),LT_M(*) ,X(*), TOL,
     .  P(*) ,Z(*)  ,R(*)  ,Y(*),LT_I(*) ,EPS_M,F_X,XI_C(*)
      my_real
     .  A(3,*),AR(3,*),VE(3,*),D(3,*),DR(3,*),XE(3,*),
     .  MS(*),VOLMON(*),SKEW(*) ,XFRAME(*),FAC_K(*),
     .  R0(*)
      TYPE(PRGRAPH) :: GRAPHE(*)
      TYPE(DMUMPS_STRUC) MUMPS_PAR
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IT,IP,NLIM,ND,IERR,IPRI,IERROR,NNZI,LOC_PROC,ICK
      PARAMETER (ND=4)
      CHARACTER*4       EXIT
      CHARACTER*11       WARR
      my_real
     .   S , R2, R02,ALPHA,BETA,G0,G1,RR,TOLS,TOLN,TOLS2
      INTEGER CRIT_STOP,IUPD,IUPD0,F_DDL,L_DDL,IFLG,GPUR4R8,ITP
      EXTERNAL CRIT_STOP
      my_real
     .  ANORM2,XNORM2,L_A,L_B2,L_B,A_OLD,B_OLD,TMP,G00,R2_OLD,RMAX
      my_real
     .  CS,DBAR, DELTA, DENOM, KCOND,SNPROD,QRNORM,
     .  GAMMA, GBAR, GMAX, GMIN, EPSLN,LQNORM,DIAG,CGNORM,
     .  OLDB, RHS1, RHS2,SN, ZBAR, ZL ,OLDB2,TNORM2,EPS(4)
C--------------P-PCG Usage--------------------------
      my_real, DIMENSION(:),ALLOCATABLE :: SQ_DIAG_M
      SAVE SQ_DIAG_M
C--------------END GPU SPECIFIC----------------------
      DATA               EXIT  /'WITH'/
      DATA               WARR  /'**WARRING**'/
C      
      LOC_PROC=ISPMD+1
C--------------INITIALISATION--------------------------
C-----Istop=1 : NIT>=N_LIM+ RR>TOL
C------X(I)=ZERO----******N_MAX,EPS_M a calculer avant----
      F_DDL=1+ITASK*NDDL/NTHREAD
      L_DDL=(ITASK+1)*NDDL/NTHREAD
C
      ISTOP=0 
      NLIM=N_MAX
      CALL NLIM_MIX(N_MAX,NDDLI_G,NLIM)
      NLIM=MAX(NLIM,2)
      TOLS=MAX(TOL,EPS_M)
      IPRI = IPRINT
      IF (ISPMD/=0) IPRI = 0
C--------
      IF (ITASK == 0) THEN
       IF(IPRI/=0)THEN
        WRITE(IOUT,*)
     1         '*** BEGIN PROJECTED CONJUGATE GRADIENT ITERATION ***'
        WRITE(IOUT,*)
       ENDIF
       IF(IPRI<0)THEN
        WRITE(ISTDO,*)
     1         '*** BEGIN PROJECTED CONJUGATE GRADIENT ITERATION ***'
        WRITE(ISTDO,*)
       ENDIF
      END IF !(ITASK == 0) THEN
      IP = IABS(IPRI)
      IT=0
C-------------Vector Z is no more necessary-> work array for A'*v (MMAV_LTH)
      IUPD = 0
      IUPD0 = 0
      IF (IPREC>1) THEN
       IF (ITASK == 0) ALLOCATE (SQ_DIAG_M(NDDL))
C----------------------
      CALL MY_BARRIER
C--------For S ini only-------------
       DO I = F_DDL,L_DDL
        SQ_DIAG_M(I) = SQRT(DIAG_M(I))
       ENDDO
      END IF !(IPREC>1) THEN
C----------------------
      CALL MY_BARRIER
C--------For S ini only----X is in place of R to not change R---------
        RMAX=ZERO
        CALL IMP_INISI( 
     1                    NDDL  ,IADK  ,JDIK  ,DIAG_K ,LT_K  ,   
     2                    NDDLI ,ITOK  ,IADI  ,JDII   ,LT_I  ,
     3                    NNZM  ,IADM  ,JDIM  ,DIAG_M ,LT_M  ,
     4                    X     ,P     ,Y      ,
     8                    A     ,AR    ,VE    ,MS    ,XE    ,
     9                    D     ,DR    ,NDOF  ,IPARI ,INTBUF_TAB,
     A                    NUM_IMP,NS_IMP,NE_IMP,NSREM ,
     B                    NSL   ,NMONV  ,IMONV ,MONVOL,IGRSURF ,
     C                    VOLMON,FR_MV ,IBFV  ,SKEW  ,
     D                    XFRAME ,Z      ,IDDL ,IKC    ,INLOC ,
     G                    IND_IMP,XI_C ,IRBE3  ,LRBE3 ,IRBE2 ,
     H                    LRBE2  ,ITASK ,W_DDL ,F_DDL ,L_DDL ,
     I                    SQ_DIAG_M)
C    /---------------/
      CALL MY_BARRIER
C    /---------------/
         CALL IMP_INIST( 
     1                    NDDL  ,NDDLI ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,ITOK  ,IADI  ,JDII  ,LT_I   ,
     3                    ITASK ,W_DDL ,A     ,AR    ,VE     ,
     4                    D     ,DR    ,NDOF  ,IPARI ,INTBUF_TAB,
     5                    NUM_IMP,NS_IMP,NE_IMP,NSREM ,
     6                    NSL   ,NMONV  ,IMONV ,MONVOL,IGRSURF ,
     7                    VOLMON,FR_MV ,IBFV  ,SKEW  ,
     8                    XFRAME ,IND_IMP,XI_C ,MS    ,XE    ,
     9                    IRBE3 ,LRBE3 ,IRBE2 ,LRBE2  ,IADM  , 
     A                    JDIM  ,SQ_DIAG_M, LT_M ,F_DDL ,L_DDL,
     B                    Z     )
C    /---------------/
      CALL MY_BARRIER
C    /---------------/
C--------b'=Lm^t b--------------
       CALL MMV_LTH(NDDL  ,IADM  ,JDIM  ,SQ_DIAG_M,LT_M  ,   
     2              R     ,Z     ,F_DDL ,L_DDL  ,ITASK )
C----------------------
       CALL MY_BARRIER
C---------------------
       DO I = F_DDL,L_DDL
        R(I) = Z(I)
       ENDDO
       CALL PRODUT_H(F_DDL  ,L_DDL ,R  ,R  ,W_DDL , G0 ,ITASK )
C------x' ini
       CALL IMP_INIX(F_DDL,L_DDL,NDDL,X  ,R   ,W_DDL,ITASK )
C----------------------
      CALL MY_BARRIER
C---------------------
       CALL MMAV_LTH(
     1            NDDL  ,NDDLI ,IADK  ,JDIK  ,DIAG_K,   
     2            LT_K  ,IADI  ,JDII  ,ITOK  ,LT_I  ,
     3            X     ,Y     ,A     ,AR    ,
     5            VE    ,MS    ,XE    ,D     ,DR    ,
     6            NDOF  ,IPARI ,INTBUF_TAB   ,NUM_IMP,
     7            NS_IMP,NE_IMP,NSREM ,NSL   ,IBFV   ,
     8            SKEW  ,XFRAME,MONVOL,VOLMON,IGRSURF ,
     9            FR_MV,NMONV ,IMONV ,IND_IMP,
     A            XI_C  ,IUPD  ,IRBE3 ,LRBE3 ,IRBE2  ,
     B            LRBE2 ,IADM  ,JDIM  ,SQ_DIAG_M,LT_M   ,
     C            F_DDL ,L_DDL ,ITASK ,Z     )
C----------------------
       CALL MY_BARRIER
C---------------------
       IF (ITOL==1) THEN
         R02 = G0
       ELSEIF (ITOL==3) THEN
C----------initialization of XNORM2       
        CALL PRODUT_H( F_DDL  ,L_DDL ,X  ,X ,W_DDL , XNORM2 ,ITASK )
       END IF
C
       DO I=F_DDL,L_DDL
        R(I) = R(I)-Y(I)
       ENDDO 
C----------------------
       CALL MY_BARRIER
C---------------------
C       CALL PREC_SOLVH(IPREC, ITASK   ,
c     1                 GRAPHE,IAD_ELEM,FR_ELEM,DIAG_K,LT_K   , 
c     2                 IADK  ,JDIK    ,ITAB   ,IPRI,INSOLV , 
c     3                 ITN   ,FAC_K   , IPIV_K, NK   ,MUMPS_PAR,
c     4                 CDDLP ,ISOLV   , IDSC  , IDDL ,IKC      , 
c     5                 INLOC  ,NDOF   , NDDL  ,NNZM  ,IADM     , 
c     6                 JDIM  ,DIAG_M  , LT_M  ,R     ,Z        ,
c     7                 F_DDL ,L_DDL   )
       DO I = F_DDL,L_DDL
        P(I) = R(I)
       ENDDO
C----------------------
       CALL MY_BARRIER
C---------------------
       CALL IMP_PRO_P(F_DDL,L_DDL,NDDL ,P  ,W_DDL,ITASK )
C----------------------
       CALL MY_BARRIER
C---------------------
       CALL PRODUT_H(F_DDL  ,L_DDL ,R   ,R  ,W_DDL , G0 ,ITASK )
C---------------------
       CALL MMAV_LTH(
     1            NDDL  ,NDDLI ,IADK  ,JDIK  ,DIAG_K,   
     2            LT_K  ,IADI  ,JDII  ,ITOK  ,LT_I  ,
     3            P     ,Y     ,A     ,AR    ,
     5            VE    ,MS    ,XE    ,D     ,DR    ,
     6            NDOF  ,IPARI ,INTBUF_TAB   ,NUM_IMP,
     7            NS_IMP,NE_IMP,NSREM ,NSL   ,IBFV   ,
     8            SKEW  ,XFRAME,MONVOL,VOLMON,IGRSURF ,
     9            FR_MV,NMONV ,IMONV ,IND_IMP,
     A            XI_C  ,IUPD  ,IRBE3 ,LRBE3 ,IRBE2  ,
     B            LRBE2 ,IADM  ,JDIM  ,SQ_DIAG_M,LT_M   ,
     C            F_DDL ,L_DDL ,ITASK ,Z     )
C----------------------
       CALL MY_BARRIER
C---------------------
       CALL IMP_UPDV2( 
     1                    NDDL  ,NDDLI ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,ITOK  ,IADI  ,JDII  ,LT_I   ,
     3                    ITASK ,W_DDL ,A     ,AR    ,VE     ,
     4                    D     ,DR    ,NDOF  ,IPARI ,INTBUF_TAB,
     5                    NUM_IMP,NS_IMP,NE_IMP,NSREM ,
     6                    NSL   ,NMONV  ,IMONV ,MONVOL,IGRSURF ,
     7                    VOLMON,FR_MV ,IBFV  ,SKEW  ,
     8                    XFRAME ,IND_IMP,XI_C ,MS    ,XE    ,
     9                    IRBE3 ,LRBE3 ,IRBE2 ,LRBE2  ,PROJ_V,
     A                    P     ,Y     ,F_DDL ,L_DDL  ,0     ,
     B                    0     ,Z     ,IADM  ,JDIM  ,SQ_DIAG_M,
     C                    LT_M  )
       CALL PRODUT_H(F_DDL  ,L_DDL ,P   ,Y  ,W_DDL , S ,ITASK )
C---------------------
       IF (G0==ZERO) THEN
        IF (ITOL>1) THEN
         ISTOP=-1
         GOTO 200
	ELSE
         ALPHA = ZERO
        ENDIF
       ELSE
        ALPHA = G0/S
       ENDIF
       TOLS2=TOLS*TOLS
       IF (ITOL==1) THEN
C---------------------
        R2 =G0
       ELSEIF (ITOL==3) THEN
C------ R2<TOL*TOL*ANORM2*XNORM2------
        R02=ABS(G0)
        R2 =R02
        L_A=ONE/ALPHA
C        L_B2=R02
        TNORM2=L_A*L_A
        ANORM2=ZERO
        IF (M_VS == 0) XNORM2=ZERO
        A_OLD=L_A
        B_OLD=ZERO
        OLDB   = SQRT(R02)
       ELSEIF (ITOL==4) THEN
        R02=ALPHA*ALPHA*ABS(G0)
        EPS(1)=R02
        R2=R02
       ELSE
        R02=ABS(G0)
        R2 =R02
       ENDIF
       IF (R02==ZERO) THEN
        ISTOP=-1
        GOTO 200
       ENDIF
       TOLN=R02*TOLS2
       IF(IPRI/=0.AND.ITASK==0)THEN
        RR = SQRT(R2)
        IF(IPRI<0) WRITE(ISTDO,1000)IT,RR
        WRITE(IOUT,1000)IT,RR
       ENDIF
C-------pour etre coherent avec lanzos for linear
C----------------------
       CALL MY_BARRIER
C---------------------
       IT=1
       DO I=F_DDL,L_DDL
         X(I) = X(I) + ALPHA*P(I)
         R(I) = R(I) - ALPHA*Y(I)
       ENDDO 
C----------------------
       CALL MY_BARRIER
C---------------------
c       CALL PREC_SOLVH(IPREC, ITASK ,
c     1                 GRAPHE,IAD_ELEM,FR_ELEM,DIAG_K,LT_K   , 
c     2                 IADK  ,JDIK    ,ITAB   ,IPRI,INSOLV , 
c     3                 ITN   ,FAC_K   , IPIV_K, NK   ,MUMPS_PAR,
c     4                 CDDLP ,ISOLV   , IDSC  , IDDL ,IKC      , 
c     5                 INLOC  ,NDOF   , NDDL  ,NNZM  ,IADM    , 
c     6                 JDIM  ,DIAG_M  , LT_M  ,R     ,Z       ,
c     7                 F_DDL ,L_DDL   )
C---------------------
       CALL PRODUT_H( F_DDL  ,L_DDL ,R   ,R  ,W_DDL , G1 ,ITASK )
C---------------------
       BETA=G1/G0
       IF (ITOL==1) THEN
        R2 = G1
       ELSEIF (ITOL==3) THEN
C------ R2<TOL*TOL*ANORM2*XNORM2------
        R2=ABS(G1)
        L_B2=ABS(BETA)*A_OLD*A_OLD
        L_B=SQRT(L_B2)
        TNORM2=TNORM2+L_B2
        B_OLD=BETA
C*     INITIALIZE OTHER QUANTITIES.
        GBAR   = L_A
        DBAR   = L_B
        RHS1   = OLDB
        RHS2   = ZERO
        GMAX   = ABS( L_A ) + EPS_M
        GMIN   = GMAX
        OLDB2   = L_B2
        OLDB   = L_B
       ELSEIF (ITOL==4) THEN
        R2=R02
       ELSE
        R2=ABS(G1)
       ENDIF
       G0 = G1
       IF (ITOL==3) TOLN=TOLN*TNORM2
C----------------------
       CALL MY_BARRIER
C---------------------
       ISTOP=CRIT_STOP(IT,R2,NLIM,TOLN)
       IF(NDDLI_G>0.AND.INTP_C==-1)IUPD0 = -INTP_C
       DO WHILE (ISTOP==1)
        DO I=F_DDL,L_DDL
         P(I) = R(I) + BETA*P(I)
        ENDDO 
        IF(IUPD0>0.AND.IT>NDDLI_G/2) IUPD = IUPD0
C       
C------PCG(PROJECTION)----
        CALL IMP_PRO_P(F_DDL,L_DDL,NDDL ,P  ,W_DDL,ITASK )
C       
C----------------------
        CALL MY_BARRIER
C---------------------
        CALL MMAV_LTH(
     1            NDDL  ,NDDLI ,IADK  ,JDIK  ,DIAG_K,   
     2            LT_K  ,IADI  ,JDII  ,ITOK  ,LT_I  ,
     3            P     ,Y     ,A     ,AR    ,
     5            VE    ,MS    ,XE    ,D     ,DR    ,
     6            NDOF  ,IPARI ,INTBUF_TAB   ,NUM_IMP,
     7            NS_IMP,NE_IMP,NSREM ,NSL   ,IBFV   ,
     8            SKEW  ,XFRAME,MONVOL,VOLMON,IGRSURF ,
     9            FR_MV,NMONV ,IMONV ,IND_IMP,
     A            XI_C  ,IUPD  ,IRBE3 ,LRBE3 ,IRBE2  ,
     B            LRBE2 ,IADM  ,JDIM  ,SQ_DIAG_M,LT_M   ,
     C            F_DDL ,L_DDL ,ITASK ,Z     )
C----------------------
        CALL MY_BARRIER
C---------------------
        ICK=0
       CALL IMP_UPDV2( 
     1                    NDDL  ,NDDLI ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,ITOK  ,IADI  ,JDII  ,LT_I   ,
     3                    ITASK ,W_DDL ,A     ,AR    ,VE     ,
     4                    D     ,DR    ,NDOF  ,IPARI ,INTBUF_TAB,
     5                    NUM_IMP,NS_IMP,NE_IMP,NSREM ,
     6                    NSL   ,NMONV  ,IMONV ,MONVOL,IGRSURF ,
     7                    VOLMON,FR_MV ,IBFV  ,SKEW  ,
     8                    XFRAME ,IND_IMP,XI_C ,MS    ,XE    ,
     9                    IRBE3 ,LRBE3 ,IRBE2 ,LRBE2  ,PROJ_V,
     A                    P     ,Y     ,F_DDL ,L_DDL  ,IT    ,
     B                    ICK   ,Z     ,IADM  ,JDIM  ,SQ_DIAG_M,
     C                    LT_M  )
C----------------------
       CALL MY_BARRIER
C---------------------
        CALL PRODUT_H(F_DDL  ,L_DDL ,P   ,Y  ,W_DDL , S ,ITASK )
C---------------------
        ALPHA=G0/S
       IF(MOD(IT,10)<0)THEN
        DO I=F_DDL,L_DDL
         X(I) = X(I) + ALPHA*P(I)
        ENDDO 
        CALL MMAV_LTH(
     1            NDDL  ,NDDLI ,IADK  ,JDIK  ,DIAG_K,   
     2            LT_K  ,IADI  ,JDII  ,ITOK  ,LT_I  ,
     3            X     ,Y     ,A     ,AR    ,
     5            VE    ,MS    ,XE    ,D     ,DR    ,
     6            NDOF  ,IPARI ,INTBUF_TAB   ,NUM_IMP,
     7            NS_IMP,NE_IMP,NSREM ,NSL   ,IBFV   ,
     8            SKEW  ,XFRAME,MONVOL,VOLMON,IGRSURF ,
     9            FR_MV,NMONV ,IMONV ,IND_IMP,
     A            XI_C  ,IUPD  ,IRBE3 ,LRBE3 ,IRBE2  ,
     B            LRBE2 ,IADM  ,JDIM  ,SQ_DIAG_M,LT_M   ,
     C            F_DDL ,L_DDL ,ITASK ,Z     )
        DO I=F_DDL,L_DDL
         R(I) = R0(I) - Y(I)
        ENDDO 
C----------------------
        CALL MY_BARRIER
C---------------------
       ELSE
        DO I=F_DDL,L_DDL
         X(I) = X(I) + ALPHA*P(I)
         R(I) = R(I) - ALPHA*Y(I)
        ENDDO 
       END IF !  IF(MOD(IT,IP)==0)THEN
C----------------------
        CALL MY_BARRIER
C---------------------
c        CALL PREC_SOLVH(IPREC, ITASK ,
c     1                 GRAPHE,IAD_ELEM,FR_ELEM,DIAG_K,LT_K   , 
c     2                 IADK  ,JDIK    ,ITAB   ,IPRI,INSOLV , 
c     3                 ITN   ,FAC_K   , IPIV_K, NK   ,MUMPS_PAR,
c     4                 CDDLP ,ISOLV   , IDSC  , IDDL ,IKC      , 
c     5                 INLOC  ,NDOF   , NDDL  ,NNZM  ,IADM    , 
c     6                 JDIM  ,DIAG_M  , LT_M  ,R     ,Z       ,
c     7                 F_DDL ,L_DDL   )
C---------------------
        CALL PRODUT_H(F_DDL  ,L_DDL ,R   ,R  ,W_DDL , G1 ,ITASK )
C---------------------
        BETA=G1/G0
        G0 = G1
        IF (ITOL==1) THEN
         R2 = G1
        ELSEIF (ITOL==3) THEN
         R2 =ABS(G1)
         S=ONE/ALPHA
         L_A=S+B_OLD*A_OLD
C----------L_B2 : beta(j-1)^2-A_OLD :1/alpha(j-1)-------
         L_B2=ABS(BETA)*S*S
         L_B=SQRT(L_B2)
         A_OLD=S
         B_OLD=BETA
         ANORM2=TNORM2
         TNORM2=TNORM2+L_A*L_A+OLDB2+L_B2
         GAMMA  = SQRT( GBAR*GBAR + OLDB2 )
         CS     = GBAR / GAMMA
         SN     = OLDB / GAMMA
         DELTA  = CS * DBAR  +  SN * L_A
         GBAR   = SN * DBAR  -  CS * L_A
         EPSLN  = SN * L_B
         DBAR   =            -  CS * L_B
         ZL     = RHS1 / GAMMA
         XNORM2 = XNORM2+ZL*ZL
         GMAX   = MAX( GMAX, GAMMA )
         GMIN   = MIN( GMIN, GAMMA )
         RHS1   = RHS2  -  DELTA * ZL
         RHS2   =       -  EPSLN * ZL
         TOLN=TOLS2*ANORM2*XNORM2
         OLDB2   = L_B2
         OLDB   = L_B
        ELSEIF (ITOL==4) THEN
         TMP=ALPHA*ALPHA*ABS(G1)
         IF (IT>=ND) THEN
          DO J=1,ND-1
           EPS(J)=EPS(J+1)
          ENDDO
          EPS(ND)=TMP
          R2=ZERO
          DO J=1,ND
           R2=R2+EPS(J)
          ENDDO
         ELSE
          EPS(IT+1)=TMP
          R2=R2+TMP
         ENDIF
        ELSE
         R2 =ABS(G1)
        ENDIF
C        
          CALL IMP_UPDV1( 
     1                    R2  ,R2_OLD  ,RMAX  ,PROJ_V,P      ,   
     2                    IT  ,F_DDL,L_DDL )
C        
        IF(IPRI/=0.AND.ITASK==0)THEN
         IF(MOD(IT,IP)==0)THEN
          RR = SQRT(R2/R02)
          WRITE(IOUT,1001)IT,RR
          IF(IPRI<0) WRITE(ISTDO,1001)IT,RR
         ENDIF
        ENDIF
C----------------------
        CALL MY_BARRIER
C---------------------
        ISTOP=CRIT_STOP(IT,R2,NLIM,TOLN)
        IF((IUPD>0.AND.IT==NLIM).OR.
     .     (IUPD==0.AND.ISTOP/=1.AND.IUPD0>0))THEN
C------- re-iter with linear Kint------	
         IUPD0 = 0 
         IF(IUPD>0) IUPD = 0 
         ISTOP = 1
         NLIM = NLIM + NLIM
         DO I=F_DDL,L_DDL
          X(I) = ZERO
         ENDDO 
C----------------------
         CALL MY_BARRIER
C---------------------
         CALL MMAV_LTH(
     1            NDDL  ,NDDLI ,IADK  ,JDIK  ,DIAG_K,   
     2            LT_K  ,IADI  ,JDII  ,ITOK  ,LT_I  ,
     3            X     ,Y     ,A     ,AR    ,
     5            VE    ,MS    ,XE    ,D     ,DR    ,
     6            NDOF  ,IPARI ,INTBUF_TAB   ,NUM_IMP,
     7            NS_IMP,NE_IMP,NSREM ,NSL   ,IBFV   ,
     8            SKEW  ,XFRAME,MONVOL,VOLMON,IGRSURF ,
     9            FR_MV,NMONV ,IMONV ,IND_IMP,
     A            XI_C  ,IUPD  ,IRBE3 ,LRBE3 ,IRBE2  ,
     B            LRBE2 ,IADM  ,JDIM  ,SQ_DIAG_M,LT_M   ,
     C            F_DDL ,L_DDL ,ITASK ,Z     )
C----------------------
         CALL MY_BARRIER
C---------------------
         DO I=F_DDL,L_DDL
          R(I) = R0(I)-Y(I)
         ENDDO 
C----------------------
         CALL MY_BARRIER
c         CALL PREC_SOLVH(IPREC, ITASK ,
c     1                 GRAPHE,IAD_ELEM,FR_ELEM,DIAG_K,LT_K   , 
c     2                 IADK  ,JDIK    ,ITAB   ,IPRI,INSOLV , 
c     3                 ITN   ,FAC_K   , IPIV_K, NK   ,MUMPS_PAR,
c     4                 CDDLP ,ISOLV   , IDSC  , IDDL ,IKC      , 
c     5                 INLOC  ,NDOF   , NDDL  ,NNZM  ,IADM    , 
c     6                 JDIM  ,DIAG_M  , LT_M  ,R     ,Z       ,
c     7                 F_DDL ,L_DDL   )
C---------------------
         CALL PRODUT_H( F_DDL  ,L_DDL ,R   ,R  ,W_DDL , G0 ,ITASK )
         BETA = ZERO
C------ R2<TOL*TOL*ANORM2*XNORM2------
         IF (ITOL==3) THEN
           TNORM2=L_A*L_A
           ANORM2=ZERO
           XNORM2=ZERO
           A_OLD=ZERO
           B_OLD=ZERO
           L_B=ZERO
C*     INITIALIZE OTHER QUANTITIES.
           GBAR   = L_A
           DBAR   = ZERO
           RHS1   = SQRT(R02)
           RHS2   = ZERO
           GMAX   = ABS( L_A ) + EPS_M
           GMIN   = GMAX
           OLDB2   = ZERO
           OLDB   = L_B
         ELSEIF (ITOL==4) THEN
          DO J=1,ND
           EPS(J)=R02
          ENDDO
         ENDIF
        ENDIF
C----------------------
        CALL MY_BARRIER
C---------------------
        IT = IT +1
       ENDDO
 200   CONTINUE
C--------PCG (PROJECTION)      
      IF (ISTOP==0) THEN
       CALL IMP_UPDST( 
     1                NDDL  ,NDDLI ,IADK  ,JDIK  ,DIAG_K ,   
     2                LT_K  ,ITOK  ,IADI  ,JDII  ,LT_I   ,
     3                ITASK ,W_DDL ,A     ,AR    ,VE     ,
     4                D     ,DR    ,NDOF  ,IPARI ,INTBUF_TAB,
     5                NUM_IMP,NS_IMP,NE_IMP,NSREM ,
     6                NSL   ,NMONV  ,IMONV ,MONVOL,IGRSURF ,
     7                VOLMON,FR_MV ,IBFV  ,SKEW  ,
     8                XFRAME ,IND_IMP,XI_C ,MS    ,XE    ,
     9                IRBE3  ,LRBE3 ,IRBE2 ,LRBE2 ,X     ,
     A                P      ,Y     ,IADM  ,JDIM  ,SQ_DIAG_M  , 
     B                LT_M  ,F_DDL ,L_DDL )
C----------------------
      CALL MY_BARRIER
C----------------------
C--------x=Lm x'--------------
       CALL MMV_LH(NDDL   ,IADM0 ,JDIM0 ,SQ_DIAG_M,LT_M0 ,   
     2              X     ,Z     ,F_DDL ,L_DDL  ,ITASK )
       DO I=F_DDL,L_DDL
         X(I) =  Z(I)
       ENDDO
       IF (ITASK == 0) THEN
        NCG_RUN = NCG_RUN + 1
	IF (IPREC>1) DEALLOCATE(SQ_DIAG_M)
       END IF
C----------------------
      CALL MY_BARRIER
C---------------------
      END IF
C------due to ISTOP local variable
C      IF(IT>=NLIM.AND.ISOLV==7) ISTOP = 3
c         IF(ITOL==3)then
c          DO I=F_DDL,L_DDL
c           Z(I)=X(I)/DIAG_M(I)
c          ENDDO
C----------------------
c      CALL MY_BARRIER
C---------------------
c          CALL PRODUT_H( F_DDL  ,L_DDL ,X  ,Z ,W_DDL , XNORM2 ,ITASK )
c	 END IF 
      IF(ITASK == 0) THEN
        ISTOP_H = ISTOP
       CALL PUPD(N_MAX,NDDLI_G,IT)
C       
       IF(IT>=NLIM)THEN
        IF (ISOLV==7) THEN
         ISTOP_H = 3
        ELSE

        WRITE(IOUT,*)
        WRITE(IOUT,1003)NLIM
        WRITE(IOUT,*)
        WRITE(ISTDO,*)
        WRITE(ISTDO,1003)NLIM
        WRITE(ISTDO,*)
        IF(ITOL==3)THEN
          WRITE(IOUT,2000)  
     .     EXIT, SQRT(ANORM2), SQRT(XNORM2),GMAX/GMIN
          WRITE(ISTDO,2000)  
     .     EXIT, SQRT(ANORM2), SQRT(XNORM2),GMAX/GMIN
        ENDIF
        ISTOP_H = 1
        IF (R2>HUNDRED*TOLN) THEN
         ISTOP_H = 2
         RR = SQRT(R2/R02)
         WRITE(IOUT,*)
         WRITE(IOUT,1004)RR
         WRITE(IOUT,*)
         WRITE(ISTDO,*)
         WRITE(ISTDO,1004)RR
         WRITE(ISTDO,*)
        ENDIF
         
        END IF !(ISOLV==7) THEN
       ENDIF
       IF(IPRI/=0)THEN
        RR = SQRT(R2/R02)
        WRITE(IOUT,*)
        WRITE(IOUT,1002)IT,RR
        IF(ITOL==3)WRITE(IOUT,2000)  
     .     EXIT, SQRT(ANORM2), SQRT(XNORM2),GMAX/GMIN
C          WRITE(IOUT,*) 'TOLN,TOLS2=',TOLN,TOLS2 
        WRITE(IOUT,*)
        IF(IPRI<0) THEN
         WRITE(ISTDO,*)
         WRITE(ISTDO,1002)IT,RR
         IF(ITOL==3)WRITE(ISTDO,2000)
     .     EXIT, SQRT(ANORM2), SQRT(XNORM2),GMAX/GMIN
         WRITE(ISTDO,*)
        ENDIF
       ENDIF
       IF(ITOL==3) THEN
        K_LAMDA0= GMIN
        K_LAMDA1= GMAX
       ELSE
        K_LAMDA0= ZERO
        K_LAMDA1= ZERO
       ENDIF
      END IF !(ITASK == 0) THEN
C----------------------
      CALL MY_BARRIER
C----------------------
      ISTOP = ISTOP_H
C--------------------------------------------
 1000 FORMAT(5X,'ITERATION=',I8,5X,'  INITIAL RESIDUAL NORM=',E11.4)
 1001 FORMAT(5X,'ITERATION=',I8,5X,' RELATIVE RESIDUAL NORM=',E11.4)
 1002 FORMAT(3X,'TOTAL C.G. ITERATION=',I8,5X,
     .          ' RELATIVE RESIDUAL NORM=',E11.4)
 1003 FORMAT(5X,
     . '---WARNING : THE ITERATION LIMIT NUMBER WAS REACHED',I8)
 1004 FORMAT(5X,'WARNING:C.G STOP WITH RELATIVE RESIDUAL NORM=',E11.4)
 2000 FORMAT(/ 5X, A, 2X, 'ANORM =', E12.4, 2X, 'XNORM =', E12.4,2X,
     .       'KCOND =', E12.4)
 2002 FORMAT(/ 5X, 'WITH', 2X, 'ALFA =', E12.4, 2X, 'BETA =', 
     .       E12.4,2X,'OLDB =', E12.4)
      RETURN
      END
#endif
Chd|====================================================================
Chd|  IMP_INIST                     source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PPCGH                     source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        MAM_NM                        source/implicit/produt_v.F    
Chd|        MAV_MM                        source/implicit/produt_v.F    
Chd|        MMAV_LTH                      source/implicit/produt_v.F    
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        IMP_PCG_PROJ                  share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
       SUBROUTINE IMP_INIST( 
     1                    NDDL  ,NDDLI ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,ITOK  ,IADI  ,JDII  ,LT_I   ,
     3                    ITASK ,W_DDL ,A     ,AR    ,VE     ,
     4                    D     ,DR    ,NDOF  ,IPARI ,INTBUF_TAB ,
     5                    NUM_IMP,NS_IMP,NE_IMP,NSREM ,
     6                    NSL   ,NMONV  ,IMONV ,MONVOL,IGRSURF ,
     7                    VOLMON,FR_MV ,IBFV  ,SKEW  ,
     8                    XFRAME ,IND_IMP,XI_C ,MS    ,XE    ,
     9                    IRBE3 ,LRBE3 ,IRBE2 ,LRBE2  ,IADM  , 
     A                    JDIM  ,DIAG_M, LT_M ,F_DDL ,L_DDL  ,
     B                    V_W   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
         USE IMP_PCG_PROJ
         USE INTBUFDEF_MOD
         USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,IADK(*)  ,JDIK(*),
     .         NDDLI ,ITOK(*) ,IADI(*),JDII(*),
     .         ITASK,W_DDL(*),IBFV(*),IRBE3(*) ,LRBE3(*),
     .         IRBE2(*) ,LRBE2(*),F_DDL,L_DDL,IADM(*) ,JDIM(*)
      INTEGER  NDOF(*),NE_IMP(*),NSREM ,NSL,
     .         IPARI(*) ,NUM_IMP(*),NS_IMP(*) ,IND_IMP(*)
      INTEGER NMONV,IMONV(*),MONVOL(*),FR_MV(*)
C     REAL
        my_real
     .  DIAG_K(*), LT_K(*),LT_I(*) ,XI_C(*),DIAG_M(*), LT_M(*)
        my_real
     .  A(3,*),AR(3,*),VE(3,*),D(3,*),DR(3,*),XE(3,*),
     .  MS(*),VOLMON(*),SKEW(*) ,XFRAME(*),V_W(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
c FUNCTION: initialization of S,T of Projection taking into account to [M] preconditioner
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   NDDL,NNZ          - dimension of [K]  and number of non zero (strict triangular)
c  I   IADK,JDIK         - indice arrays for compressed row(col.) format of [K]
c  I   DIAG_K(NDDL)      - diagonal terms of [K]
c  I   LT_K(NNZ)         - strict triangular of [K]
c  O   Proj_S(NDDL,M_VS)   - [S] reduced small Eigenvectors
c  O   Proj_T(NDDL,M_VS)   - [T] =[K][S]
c  I   W1,W2             - work arrays
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#if defined(MYREAL8) && !defined(WITHOUT_LINALG)
       CHARACTER          JOBZ, UPLO
       INTEGER I,J,IT,IP,NLIM,ND,IUPD,IPRI,IERROR,NNZI,M,
     .         F_DDLI,L_DDLI,INFO,LW,INORM,M_VS1
       my_real
     .  WORK(3*M_VS+3),W(M_VS+1)
CC---------------------
C       IF (NPCGPV >=0) RETURN
C----------------------
C      CALL MY_BARRIER
C---------------------
       IF (NCG_RUN==0) THEN 
        M_VS1 =M_VS
       ELSE
        M_VS1 =M_VS+1
       END IF  
       IF (ITASK==0) NPCGPV = M_VS
C----------------------
      CALL MY_BARRIER
C---------------------
C---------------------
C        T=[K][S]
C---------------------
       IUPD = 0
       DO M = 1,M_VS1
        CALL MMAV_LTH(
     1            NDDL  ,NDDLI ,IADK  ,JDIK  ,DIAG_K,   
     2            LT_K  ,IADI  ,JDII  ,ITOK  ,LT_I  ,
     3            PROJ_S(1,M),PROJ_T(1,M),A     ,AR    ,
     5            VE    ,MS    ,XE    ,D     ,DR    ,
     6            NDOF  ,IPARI ,INTBUF_TAB   ,NUM_IMP,
     7            NS_IMP,NE_IMP,NSREM ,NSL   ,IBFV   ,
     8            SKEW  ,XFRAME,MONVOL,VOLMON,IGRSURF ,
     9            FR_MV,NMONV ,IMONV ,IND_IMP,
     A            XI_C  ,IUPD  ,IRBE3 ,LRBE3 ,IRBE2  ,
     B            LRBE2 ,IADM  ,JDIM  ,DIAG_M,LT_M   ,
     C            F_DDL ,L_DDL ,ITASK ,V_W   )
C----------------------
      CALL MY_BARRIER
C---------------------
       END DO !M = 1,M_VS1
C----------------------
C        [m0]=[S]^t[S]
C---------------------
C       CALL MAM_NM(F_DDL  ,L_DDL  ,NDDL ,NPCGPV  ,PROJ_S ,PROJ_S ,P_M0  ,WDDL , ITASK )
C---------------------- 
C        [k0]=[S]^t[T]
C---------------------
       CALL MAM_NM(F_DDL  ,L_DDL  ,NDDL  ,M_VS1  ,PROJ_S ,
     .             PROJ_T ,PROJ_K ,W_DDL ,ITASK   )
C----------------------
      CALL MY_BARRIER
C---------------------
C        ([k0]-lamda(i)[m0])*phi(i)=0; Rather ([m0]-lamda(i)[I])*phi(i)=0
C---------------------
       IF (ITASK==0) THEN
        JOBZ='V'
        UPLO='U'
        LW=3*M_VS1
        CALL DSYEV(JOBZ, UPLO, M_VS1,PROJ_K, M_VS1, W, WORK, LW, INFO)
C        [S]<-[S][phi]
C---------------------
        CALL MAV_MM(NDDL   ,M_VS1     ,PROJ_S     ,PROJ_K  ,ITASK )
C----------------------
C        [T]<-[T][phi]
C---------------------
        CALL MAV_MM(NDDL   ,M_VS1     ,PROJ_T     ,PROJ_K  ,ITASK )
C----------------------
C        [LAMDA]^-1<- 1.0/lamda(i)
C---------------------
        DO I=1,M_VS1
         PROJ_LA_1(I)= ONE/SIGN(MAX(EM20,ABS(W(I))),W(I))
        ENDDO
      if (iline==1.AND.IMPDEB>0) then
       IF (NCG_RUN==0) THEN 
       write(iout,*)'info,ini EIGENVALUE=',info,
     .       (ONE/PROJ_LA_1(I),I=1,M_VS1)
       ELSEIF (iline==1) THEN
       write(iout,*)'info,Upd EIGENVALUE=',info,
     .       (ONE/PROJ_LA_1(I),I=1,M_VS1)
       END IF
      endif
       END IF !(ITASK==0) THEN
#else
        CALL ARRET(5)
#endif
      RETURN
      END
Chd|====================================================================
Chd|  IMP_UPDST                     source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PPCGH                     source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|        MORTHO_GS                     source/implicit/produt_v.F    
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        IMP_PCG_PROJ                  share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
       SUBROUTINE IMP_UPDST( 
     1                    NDDL  ,NDDLI ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,ITOK  ,IADI  ,JDII  ,LT_I   ,
     3                    ITASK ,W_DDL ,A     ,AR    ,VE     ,
     4                    D     ,DR    ,NDOF  ,IPARI ,INTBUF_TAB,
     5                    NUM_IMP,NS_IMP,NE_IMP,NSREM ,
     6                    NSL   ,NMONV  ,IMONV ,MONVOL,IGRSURF ,
     7                    VOLMON,FR_MV ,IBFV  ,SKEW  ,
     8                    XFRAME ,IND_IMP,XI_C ,MS    ,XE    ,
     9                    IRBE3 ,LRBE3 ,IRBE2 ,LRBE2  ,U     ,
     A                    P     ,Y     ,IADM  ,JDIM  ,DIAG_M  , 
     B                    LT_M  ,F_DDL ,L_DDL )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
         USE IMP_PCG_PROJ
         USE INTBUFDEF_MOD
         USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  F_DDL,L_DDL,NDDL  ,IADK(*)  ,JDIK(*),
     .         NDDLI ,ITOK(*) ,IADI(*),JDII(*),
     .         ITASK,W_DDL(*),IBFV(*),IRBE3(*) ,LRBE3(*),
     .         IRBE2(*) ,LRBE2(*),IADM(*)  ,JDIM(*)
      INTEGER  NDOF(*),NE_IMP(*),NSREM ,NSL,
     .         IPARI(*) ,NUM_IMP(*),NS_IMP(*) ,IND_IMP(*)
      INTEGER NMONV,IMONV(*),MONVOL(*),FR_MV(*)
C     REAL
        my_real
     .  DIAG_K(*), LT_K(*),LT_I(*) ,XI_C(*),U(*),P(*),Y(*)
        my_real
     .  A(3,*),AR(3,*),VE(3,*),D(3,*),DR(3,*),XE(3,*),
     .  MS(*),VOLMON(*),SKEW(*) ,XFRAME(*),
     .  DIAG_M(*), LT_M(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
c FUNCTION: update S,T of Projection taking into account to preconditioner [M]
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   NDDL,NNZ          - dimension of [K]  and number of non zero (strict triangular)
c  I   IADK,JDIK         - indice arrays for compressed row(col.) format of [K]
c  I   DIAG_K(NDDL)      - diagonal terms of [K]
c  I   LT_K(NNZ)         - strict triangular of [K]
c  O   Proj_S(NDDL,M_VS)   - [S] reduced small Eigenvectors
c  O   Proj_T(NDDL,M_VS)   - [T] =[K][S]
c  I   W1,W2             - work arrays
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       CHARACTER          JOBZ, UPLO
       INTEGER I,J,IT,IP,NLIM,ND,IUPD,IPRI,IERROR,NNZI,M,
     .         INFO,LW,M_VS1,INORM,NP
       my_real
     .  WORK(3*M_VS+3),W(M_VS+1)
C------M_VS input one -NPCGPV: activated ---------------
       IF (NPCGPV == 0) RETURN
C----------------------
      CALL MY_BARRIER
C---------------------
        M_VS1 = NPCGPV+ 1
C------add previous solution U ; default : IPRO_S0=2
C---IPRO_S0=1 : alertory updated w/ 1 vector x
C---IPRO_S0=2 : ini S w/ first p vectors updated w/ 2 vectors p,x
C---IPRO_S0=3 : ini S w/ first p vectors updated w/ 2 vectors p,x (p<-RR_max)
C---IPRO_S0=4 : ini S w/ first p vectors updated w/ 2 vectors v,x (v update each ite)
c       IF (IPRO_S0 > 2.AND.IPRO_S0 < 4.AND.NCG_RUN==0) THEN    
c         CALL IMP_INIS(NDDL  ,F_DDL  ,L_DDL  ,   1,NPCGPV-1 ,PROJ_S  )
C----------------------
c      CALL MY_BARRIER
C---------------------
c         CALL MORTHO_GS(F_DDL  ,L_DDL  ,NDDL ,1    ,NPCGPV-1  ,
c     .                  PROJ_S ,W_DDL  ,ITASK  )
C----------------------
c      CALL MY_BARRIER
C---------------------
c       END IF !(IPRO_S0 > 1.AND.NCG_RUN==0) THEN 
C          
       IF (IPRO_S0==2) THEN    
        DO I=F_DDL,L_DDL
         PROJ_S(I,NPCGPV)=U(I)
         PROJ_S(I,M_VS1)=P(I)
        ENDDO
       ELSEIF (IPRO_S0==3.OR.IPRO_S0==4) THEN    
         DO I=F_DDL,L_DDL
          PROJ_S(I,NPCGPV)=U(I)
          PROJ_S(I,M_VS1)=PROJ_V(I)
         ENDDO
       ELSE
        DO I=F_DDL,L_DDL
         PROJ_S(I,M_VS1)=U(I)
        ENDDO
       END IF !(MOD(IPRO_S0,2)==0) THEN    
C----------------------
      CALL MY_BARRIER
C---------------------
        CALL MORTHO_GS(F_DDL  ,L_DDL  ,NDDL   ,NPCGPV ,M_VS1  ,
     .                 PROJ_S ,W_DDL  ,ITASK  )
C-------will do (T=[K]S ...) in IMP_INIST      
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_INISI                     source/implicit/imp_pcg.F     
Chd|-- called by -----------
Chd|        IMP_PPCGH                     source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|        IMP_INIS                      source/implicit/imp_pcg.F     
Chd|        MMAV_LTH                      source/implicit/produt_v.F    
Chd|        MORTHO_GS                     source/implicit/produt_v.F    
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        PRODUT_H                      source/implicit/produt_v.F    
Chd|        ALEAT                         source/system/aleat.F         
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        IMP_PCG_PROJ                  share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IMP_INISI( 
     1                    NDDL  ,IADK  ,JDIK  ,DIAG_K ,LT_K  ,   
     2                    NDDLI ,ITOK  ,IADI  ,JDII   ,LT_I  ,
     3                    NNZM  ,IADM  ,JDIM  ,DIAG_M ,LT_M  ,
     4                    R     ,P     ,Y      ,
     8                    A     ,AR    ,VE    ,MS    ,XE    ,
     9                    D     ,DR    ,NDOF  ,IPARI ,INTBUF_TAB ,
     A                    NUM_IMP,NS_IMP,NE_IMP,NSREM ,
     B                    NSL   ,NMONV  ,IMONV ,MONVOL,IGRSURF ,
     C                    VOLMON,FR_MV ,IBFV  ,SKEW  ,
     D                    XFRAME ,Z      ,IDDL ,IKC    ,INLOC ,
     G                    IND_IMP,XI_C ,IRBE3  ,LRBE3 ,IRBE2 ,
     H                    LRBE2  ,ITASK ,W_DDL ,F_DDL ,L_DDL ,
     I                    SQ_DIAG_M)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_PCG_PROJ
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,IADK(*)  ,JDIK(*),
     .         NDDLI ,ITOK(*) ,IADI(*),JDII(*),
     .         NNZM  ,IADM(*),JDIM(*),ITASK,
     .         W_DDL(*),IBFV(*),IRBE3(*) ,LRBE3(*),
     .         IRBE2(*) ,LRBE2(*),F_DDL ,L_DDL
      INTEGER  NDOF(*),NE_IMP(*),NSREM ,NSL,
     .         IPARI(*) ,NUM_IMP(*),NS_IMP(*) ,IND_IMP(*)
      INTEGER NMONV,IMONV(*),MONVOL(*),FR_MV(*)
      INTEGER IDDL(*), IKC(*), INLOC(*) 
C     REAL
      my_real
     .  DIAG_K(*), LT_K(*) ,DIAG_M(*),LT_M(*) , 
     .  P(*) ,R(*)  ,Y(*),LT_I(*) ,XI_C(*),Z(*),SQ_DIAG_M(*)
      my_real
     .  A(3,*),AR(3,*),VE(3,*),D(3,*),DR(3,*),XE(3,*),
     .  MS(*),VOLMON(*),SKEW(*) ,XFRAME(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
c FUNCTION: initialization of S,by M_VS PCG iterations
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   NDDL,NNZ          - dimension of [K]  and number of non zero (strict triangular)
c  I   IADK,JDIK         - indice arrays for compressed row(col.) format of [K]
c  I   DIAG_K(NDDL)      - diagonal terms of [K]
c  I   LT_K(NNZ)         - strict triangular of [K]
c  O   Proj_S(NDDL,M_VS)   - [S] reduced small Eigenvectors
c  IO   P,Y,X,R(NDDL)      -work vectors
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER I,J,IT,IP,NLIM,ND,IUPD,IPRI,IERROR,NNZI,M,
     .         F_DDLI,L_DDLI,INFO,LW,INORM,NPV,ITP
        my_real
     .     ALPHA,BETA, G0,G1,S
      my_real
     .  ALEAT
      EXTERNAL ALEAT
C---------------------
       IF (NPCGPV >=0.OR.NCG_RUN>0) RETURN
C      
       NPV=MIN(NDDL-1,M_VS)
C---IPRO_S0=1 : alertory updated w/ 1 vector x
C---IPRO_S0=2 : ini S w/ first p vectors updated w/ 2 vectors p,x
C---IPRO_S0=3 : alertory ini S vectors updated w/ 2 vectors p,x (p<-RR_max)
C---------------------
       IUPD = 0
      IF (IPRO_S0 > 1) THEN 
C------seems more stable-----      
       DO I=F_DDL,L_DDL
         R(I) = ALEAT()
       ENDDO 
C----------------------
       CALL MY_BARRIER
C---------------------
       DO I = F_DDL,L_DDL
        P(I) = R(I)
       ENDDO
       CALL PRODUT_H(F_DDL  ,L_DDL ,R  ,R  ,W_DDL , G0 ,ITASK )
c       
       DO ITP=1,NPV
        CALL MMAV_LTH(
     1            NDDL  ,NDDLI ,IADK  ,JDIK  ,DIAG_K,   
     2            LT_K  ,IADI  ,JDII  ,ITOK  ,LT_I  ,
     3            P     ,Y     ,A     ,AR    ,
     5            VE    ,MS    ,XE    ,D     ,DR    ,
     6            NDOF  ,IPARI ,INTBUF_TAB   ,NUM_IMP,
     7            NS_IMP,NE_IMP,NSREM ,NSL   ,IBFV   ,
     8            SKEW  ,XFRAME,MONVOL,VOLMON,IGRSURF ,
     9            FR_MV,NMONV ,IMONV ,IND_IMP,
     A            XI_C  ,IUPD  ,IRBE3 ,LRBE3 ,IRBE2  ,
     B            LRBE2 ,IADM  ,JDIM  ,SQ_DIAG_M,LT_M   ,
     C            F_DDL ,L_DDL ,ITASK ,Z     )
C----------------------
       CALL MY_BARRIER
C---------------------
C---------------------
       CALL PRODUT_H(F_DDL  ,L_DDL ,P   ,Y  ,W_DDL , S ,ITASK )
C---------------------
        ALPHA = G0/S
C----------------------
       CALL MY_BARRIER
C---------------------
       DO I=F_DDL,L_DDL
         R(I) = R(I) - ALPHA*Y(I)
       ENDDO 
C----------------------
       CALL MY_BARRIER
C---------------------
       CALL PRODUT_H( F_DDL  ,L_DDL ,R   ,R  ,W_DDL , G1 ,ITASK )
C---------------------
       BETA=G1/G0
       G0 = G1
C----------------------
       CALL MY_BARRIER
C---------------------
        DO I=F_DDL,L_DDL
         P(I) = R(I) + BETA*P(I)
        ENDDO 
C----------------------
        CALL MY_BARRIER
C---------------------
        DO I=F_DDL,L_DDL
         PROJ_S(I,ITP) = P(I)
        ENDDO 
C----------------------
        CALL MY_BARRIER
C---------------------
       END DO !ITP=1,NPC
      ELSE
         CALL IMP_INIS(NDDL  ,F_DDL  ,L_DDL  ,1  ,NPV ,PROJ_S  )
C----------------------
       CALL MY_BARRIER
C---------------------
      END IF !(IPRO_S0 > 1) THEN 
C--------matrix GS orthonalization and normalized
       CALL MORTHO_GS(F_DDL  ,L_DDL  ,NDDL   ,1      ,NPV  ,
     .                PROJ_S ,W_DDL  ,ITASK  )
C
      RETURN
      END
