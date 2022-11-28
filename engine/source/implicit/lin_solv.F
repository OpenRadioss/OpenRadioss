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
#ifdef MUMPS5
C  |---- version non //------------
Chd|====================================================================
Chd|  LIN_SOLV                      source/implicit/lin_solv.F    
Chd|-- called by -----------
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        NL_SOLV                       source/implicit/nl_solv.F     
Chd|-- calls ---------------
Chd|        BFGS_H1                       source/implicit/imp_bfgs.F    
Chd|        BFGS_H1P                      source/implicit/imp_bfgs.F    
Chd|        BFGS_H2                       source/implicit/imp_bfgs.F    
Chd|        BFGS_H2P                      source/implicit/imp_bfgs.F    
Chd|        DIAG_KIF                      source/implicit/imp_solv.F    
Chd|        IMP_SMPINI                    source/implicit/imp_solv.F    
Chd|        INI_KISC                      source/implicit/lin_solv.F    
Chd|        LIN_SOLV2                     source/implicit/lin_solv.F    
Chd|        LIN_SOLVH0                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVH1                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVHM                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVIH2                   source/implicit/lin_solv.F    
Chd|        LIN_SOLVP2                    source/implicit/lin_solv.F    
Chd|        MAV_LTH0                      source/implicit/produt_v.F    
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        PRODUT_HP                     source/implicit/produt_v.F    
Chd|        QSTAT_END                     source/implicit/imp_dyna.F    
Chd|        QSTAT_IT                      source/implicit/imp_dyna.F    
Chd|        RECUDIS                       source/implicit/recudis.F     
Chd|        RECU_KDIS                     source/airbag/monv_imp0.F     
Chd|        RECU_KDIS0                    source/implicit/lin_solv.F    
Chd|        SET_KSYM                      source/implicit/imp_solv.F    
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        IMP_WORKH                     share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE LIN_SOLV(NDDL  ,IDDL  ,NDOF  ,IKC   ,D     ,
     1                    DR    ,TOL   ,NNZ   ,IADK  ,JDIK  ,
     2                    DIAG_K,LT_K  ,NDDLI ,IADI  ,JDII  ,   
     3                    DIAG_I,LT_I  ,ITOK  ,IADM  ,JDIM  ,   
     4                    DIAG_M,LT_M  ,F     ,F_U   ,INLOC ,   
     5                    FR_ELEM,IAD_ELEM,W_DDL,ITASK ,ICPREC,
     6                    ISTOP ,A     ,AR    ,VE    ,
     7                    MS    ,XE    ,IPARI ,INTBUF_TAB   ,
     8                    NUM_IMP,NS_IMP,NE_IMP,NSREM ,NSL  ,
     9                    IT     ,GRAPHE,ITAB  ,FAC_K ,IPIV_K,
     A                    NK     ,NMONV ,IMONV ,MONVOL,IGRSURF ,
     B                    FR_MV ,VOLMON ,IBFV ,SKEW  ,
     C                    XFRAME ,MUMPS_PAR,CDDLP,IND_IMP,XI_C,
     D                    IRBE3  ,LRBE3 ,IRBE2  ,LRBE2)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
      USE IMP_WORKH
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
#include      "units_c.inc" 
#include      "impl1_c.inc" 
#include      "impl2_c.inc" 
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"     
#include      "timeri_c.inc"
#include      "dmumps_struc.h"
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C----------resol [K]{X}={F}------
      INTEGER  NDDL  ,NNZ ,IADK(*),JDIK(*),IADM(*),JDIM(*),ITASK,
     .         NDOF(*),IDDL(*),IKC(*),ICPREC,ISTOP,
     .         NDDLI  ,IADI(*),JDII(*),ITOK(*),INLOC(*),IBFV(*),
     .         FR_ELEM(*),IAD_ELEM(2,*),W_DDL(*),NSREM ,NSL,IT
      INTEGER  NE_IMP(*),IPARI(*) ,NUM_IMP(*),NS_IMP(*),
     .         ITAB(*), IPIV_K(*), NK,IND_IMP(*),IRBE3(*),LRBE3(*),
     .         IRBE2(*),LRBE2(*)
C
      INTEGER NMONV,IMONV(*),MONVOL(*),FR_MV(*)
C     REAL
      my_real
     .  DIAG_K(*),LT_K(*),DIAG_M(*),LT_M(*), F(*),TOL,
     .  DIAG_I(*),LT_I(*),D(3,*),DR(3,*),F_U, FAC_K(*)
      my_real
     .  A(3,*),AR(3,*),VE(3,*),XE(3,*),MS(*),
     .  SKEW(*)  ,XFRAME(*),VOLMON(*),XI_C(*)
C
      TYPE(PRGRAPH) :: GRAPHE(*)
C
      INTEGER CDDLP(*)
      TYPE(DMUMPS_STRUC) MUMPS_PAR
C      
      INTEGER OMP_GET_THREAD_NUM
      EXTERNAL OMP_GET_THREAD_NUM

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C-----------------------------------------------
C------------------------------------------
c     isolv=1 => P.G.C.  
C------------------------------------------
C-------------avec LIN_SOLV0-----------------------------
c     iprec=1 => [I] (sans precon)
C-------------avec LIN_SOLV1 (2-10)-----------------------------
c     iprec=2 => jacobien 
c     iprec=3 => I.C.(0)       supprimed
c     iprec=4 => I.C.(0)_Stab  supprimed
c     iprec=5 => fsai .r
C---------avec LIN_SOLV2 NPAT>1: 
c-----(iprec>10)variable dimension(D_TOL) pour LT_M : supprimed------
C------------------------------------------
C------------------------------------------
C     [K]:matrice de rigidite [M]:matrice de preconditionnement
C------------------------------------------
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C---------------------)--------------------------
      INTEGER I,J,IPRINT,ITQ,F_DDL,L_DDL,F_DDLI,L_DDLI,IERR,ITSK
      my_real
     .   A2,QTOL,R2,IN_NZ,IN_ND
C        U->L_U,DIAG_T->DIAG_T,F0->L_F0
C---------------------
c        F_DDL=1+ITASK*NDDL/NTHREAD
c        L_DDL=(ITASK+1)*NDDL/NTHREAD
c        F_DDLI=1+ITASK*NDDLI/NTHREAD
c        L_DDLI=(ITASK+1)*NDDLI/NTHREAD
C
      INEGA = IMP_CHK   
!$OMP PARALLEL PRIVATE(ITSK,F_DDL ,L_DDL,I)
      CALL IMP_SMPINI(ITSK   ,F_DDL ,L_DDL ,NDDL   )
      DO I=F_DDL,L_DDL
       L_U(I)=ZERO
       DIAG_T(I)=DIAG_K(I)
       L_F0(I)=F(I)
      ENDDO
!$OMP END PARALLEL
C      
C--add isolv=1,7 after  IF (ITASK == 0.AND.NDDLI > 0.AND.(ISOLV == 1.OR.ISOLV > 6)) THEN
      IF (NDDLI > 0.AND.ISOLV > 7) THEN
       CALL INI_KISC(NDDL,NDDLI,IADI)
C       CALL SET_KISC(NDDL,NDDLI,IADI,JDII,ITOK,LT_I,IADI0,JDII0,LT_I0)
       CALL SET_KSYM(NDDLI,IADI,JDII,LT_I,IADI0,JDII0,LT_I0)
      END IF
C----------------------
c      CALL MY_BARRIER
C---------------------
      IF (IQSTAT>1.AND.(ILINTF==0.OR.ILINTF==NCYCLE)) THEN
       ITQ = 1
       CALL PRODUT_HP(NDDL,L_F0,L_F0,W_DDL,R2)
C----------------------
c      CALL MY_BARRIER
C---------------------
       QTOL = EM04*R2
      ENDIF
 100  CONTINUE
      IF ((ISOLV<3.OR.ISOLV>6)
     .     .AND.(IPREC>1.OR.INTP_C>=0)) THEN
!$OMP PARALLEL PRIVATE(ITSK,F_DDLI ,L_DDLI,I,J)
         CALL IMP_SMPINI(ITSK   ,F_DDLI ,L_DDLI ,NDDLI   )
         DO I=F_DDLI,L_DDLI
          J=ITOK(I)
          DIAG_T(J)=DIAG_T(J)+DIAG_I(I)
         ENDDO
!$OMP END PARALLEL
       IF (ILINTF>0) CALL DIAG_KIF(DIAG_T)
      ENDIF
C
!$OMP PARALLEL PRIVATE(ITSK,F_DDL ,L_DDL)
      CALL IMP_SMPINI(ITSK   ,F_DDL ,L_DDL ,NDDL   )
      IF (INSOLV==2) CALL BFGS_H1(F_DDL,L_DDL,W_DDL,F,A2,IT,ITSK)
C
      IF (INSOLV==3) CALL BFGS_H1P(F_DDL,L_DDL,W_DDL,F,A2,IT,ITSK)
!$OMP END PARALLEL
C----------------------
c      CALL MY_BARRIER
C---------------------
       IF (ISPMD==0) THEN
        IPRINT=LPRINT
       ELSE
        IPRINT=0
       ENDIF
      IF (IMON>0) CALL STARTIME(33,1)
      IF (NSPMD==1.AND.(ISOLV==3.OR.ISOLV==4).AND.IMUMPSV==0)THEN 
C-----------BCS,DS
citask0       IF (ITASK==0) THEN
         IF (IDSC>0) IT_BCS = IT_BCS + 1
         CALL LIN_SOLV2(
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_T ,   
     2                    LT_K  ,NDDLI ,ITOK  ,IADI  ,JDII   ,
     3                    LT_I  ,L_U     ,F     ,ITASK ,LPRINT ,
     4                    ISOLV ,ISTOP ,GRAPHE,ITAB  ,INSOLV,
     5                    IT    ,FAC_K ,IPIV_K,NK    ,DIAG_I,
     6                    IDSC  )
citask0       END IF 
      ELSE
C     IF (NSPMD>1 .OR. iterative,Mix except Isolv=2 -> Mumps
       IF (ISOLV==3.OR.ISOLV==4) THEN
citask0        IF (ITASK == 0) THEN
        IF (ISPMD==0.AND.IDSC>0) IT_BCS = IT_BCS + 1
        CALL LIN_SOLVP2(GRAPHE, F,      NDDL,  IAD_ELEM,  FR_ELEM,
     1                  DIAG_K, LT_K,   IADK,  JDIK,      L_U, 
     2                  ITAB,   IPRINT, NDDLI, IADI,      JDII,
     3                  DIAG_I, LT_I,   ITOK,  INSOLV,    IT,
     4                  FAC_K,  IPIV_K, NK,    MUMPS_PAR, CDDLP,
     5                  ISOLV,  IDSC,   IDDL,  IKC,       INLOC,
     6                  NDOF  , ITASK )
citask0        END IF !(ITASK == 0) THEN
       ELSEIF (ISOLV==5.OR.ISOLV==6) THEN
         IF (IDSC>0.AND.ITASK==0) IT_BCS = IT_BCS + 1
           CALL LIN_SOLVHM(TOL  ,
     1                    NDDL  ,NNZ     ,IADK   ,JDIK  ,DIAG_T ,
     2                    LT_K  ,NDDLI   ,ITOK   ,IADI  ,JDII   ,
     3                    LT_I  ,L_U       ,F      ,ITASK ,IPRINT ,
     4                    F_U   ,ISOLV   ,IPREC  ,L_LIM ,ITOL   ,
     6                    W_DDL ,A       ,AR     ,VE    ,MS     ,
     9                    XE    ,D       ,DR     ,NDOF  ,IPARI  ,
     A                    INTBUF_TAB     ,NUM_IMP,NS_IMP,NE_IMP,
     B                    NSREM ,NSL     ,P_MACH ,ISTOP ,NMONV ,
     E                    IMONV ,MONVOL  ,IGRSURF ,FR_MV ,
     F                    VOLMON,IBFV  ,SKEW    ,XFRAME ,GRAPHE, 
     G                    IAD_ELEM,FR_ELEM,ITAB  ,INSOLV ,IT    ,
     H                    FAC_K ,IPIV_K ,NK     ,MUMPS_PAR,CDDLP,
     I                    IDSC  ,IDDL  ,IKC    ,INLOC   ,DIAG_I ,
     J                    ILINE ,ILINTF,IND_IMP,XI_C    ,L_F0   ,
     K                    NDDLI_G,INTP_C,IRBE3  ,LRBE3  ,IRBE2  ,
     L                    LRBE2  ,IT_PCG ,IMUMPSV)
       ELSEIF (N_PAT>1) THEN 
        CALL LIN_SOLVIH2( TOL   ,N_PAT ,MAXB1 ,
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_T ,   
     2                    LT_K  ,NDDLI ,ITOK  ,IADI  ,JDII   ,
     3                    LT_I  ,IADM  ,JDIM  ,DIAG_M ,LT_M  ,
     4                    L_U   ,F     ,MAX_L ,D_TOL  ,
     4                    ITASK ,ICPREC,IPRINT,F_U   ,ISOLV  ,
     5                    IPREC ,L_LIM ,ITOL  ,INEGA ,W_DDL  ,
     7                    A     ,AR    ,VE     ,MS    ,XE    ,
     8                    D     ,DR    ,NDOF  ,IPARI  ,INTBUF_TAB,
     9                    NUM_IMP,NS_IMP,NE_IMP,NSREM ,
     A                    NSL   ,P_MACH,MAXB   ,ISTOP ,NMONV ,
     B                    IMONV ,MONVOL,IGRSURF ,FR_MV ,
     C                    VOLMON,IBFV  ,SKEW  ,XFRAME ,IND_IMP,
     D                    DIAG_I,XI_C  ,L_F0   ,NDDLI_G,INTP_C,
     E                    IRBE3  ,LRBE3,IRBE2  ,LRBE2 )
       ELSEIF (IPREC==1) THEN 
        CALL LIN_SOLVH0(   TOL   ,
     1                    NDDL  ,NNZ     ,IADK   ,JDIK  ,DIAG_T ,   
     2                    LT_K  ,NDDLI   ,ITOK   ,IADI  ,JDII   ,
     3                    LT_I  ,L_U       ,F      ,ITASK ,IPRINT ,
     4                    F_U   ,ISOLV   ,IPREC  ,L_LIM ,ITOL   ,
     6                    W_DDL ,A       ,AR     ,VE     ,MS    ,
     9                    XE    ,D       ,DR     ,NDOF  ,IPARI  ,
     A                    INTBUF_TAB     ,NUM_IMP,NS_IMP,NE_IMP,
     B                    NSREM ,NSL     ,P_MACH ,ISTOP ,NMONV  ,
     E                    IMONV ,MONVOL  ,IGRSURF,FR_MV ,
     F                    VOLMON,IBFV    ,SKEW   ,XFRAME ,IND_IMP,
     G                    XI_C  ,L_F0    ,NDDLI_G,INTP_C ,IRBE3  ,
     H                    LRBE3 ,IRBE2   ,LRBE2  )
       ELSEIF (IPREC>1.AND.IPREC<10) THEN
        CALL LIN_SOLVH1( TOL   ,MAX_L  ,
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_T ,   
     2                    LT_K  ,NDDLI ,ITOK  ,IADI  ,JDII   ,
     3                    LT_I  ,DIAG_M,LT_M  ,L_U     ,F      ,
     4                    ITASK ,ICPREC,IPRINT,F_U   ,ISOLV  ,
     5                    IPREC ,L_LIM ,ITOL  ,INEGA ,W_DDL  ,
     6                    A     ,AR    ,VE     ,MS    ,XE    ,
     7                    D     ,DR    ,NDOF  ,IPARI  ,INTBUF_TAB,
     8                    NUM_IMP,NS_IMP,NE_IMP,NSREM ,
     9                    NSL   ,P_MACH,MAXB   ,ISTOP ,NMONV ,
     C                    IMONV ,MONVOL,IGRSURF ,FR_MV ,
     D                    VOLMON,IBFV  ,SKEW  ,XFRAME,IND_IMP,
     E                    DIAG_I,XI_C  ,L_F0  ,NDDLI_G,INTP_C,
     F                    IRBE3 ,LRBE3 ,IRBE2 ,LRBE2  )
       ENDIF
      ENDIF
C----------------------
c      CALL MY_BARRIER
C---------------------
      IF (IMON>0) CALL STOPTIME(33,1)
C
       IF (IMP_CHK>0.OR.(ISTOP>0.AND.IMPDEB==0)) GOTO 200
C---------------------------------
!$OMP PARALLEL PRIVATE(ITSK,F_DDL ,L_DDL)
       CALL IMP_SMPINI(ITSK   ,F_DDL ,L_DDL ,NDDL   )
       IF (INSOLV==2) THEN
        CALL BFGS_H2(F_DDL,L_DDL,W_DDL,L_U,L_F0,A2,IT,N_LIM,ITSK)
C
       ELSEIF (INSOLV==3) THEN
        CALL BFGS_H2P(F_DDL,L_DDL,W_DDL,L_U,L_F0,A2,IT,N_LIM,ITSK)
       ENDIF
!$OMP END PARALLEL 
C----------------------
c      CALL MY_BARRIER
C---------------------
      IF (IQSTAT>1.AND.(ILINTF==0.OR.ILINTF==NCYCLE))THEN
       CALL QSTAT_IT(NDDL   ,F      ,L_U      )
C----------------------
C      CALL MY_BARRIER
C---------------------
       CALL PRODUT_HP(NDDL,F,F,W_DDL,R2)
C---------------------
       IF (R2>QTOL.AND.ITQ<=IQSTAT) THEN
        ITQ = ITQ  + 1
            IDSC = 0
            ICPREC = 0
            GOTO 100
       ELSE
        CALL QSTAT_END(NDDL   ,L_U      )
        IF(LPRINT/=0)THEN
         R2 = EM02*SQRT(R2/QTOL)
         WRITE(IOUT,1002)ITQ,R2
         IF(LPRINT<0) THEN
          WRITE(ISTDO,1002)ITQ,R2
         ENDIF
        ENDIF
       ENDIF !IF (R2>QTOL.AND.ITQ<=IQSTAT)
      ENDIF !IF (IQSTAT>1.AND.(ILINTF.EQ
      IF (ILINE>0.AND.LPRINT/=0.AND.(ISOLV==3.OR.ISOLV==4))THEN
       IF (ILINTF==0.OR.ILINTF==NCYCLE)THEN
C--------BCS hase done already inside imp_r2bcs---------  
        IF (IMUMPSV>0 .AND. NDDLI>0)THEN
!$OMP PARALLEL PRIVATE(ITSK,F_DDL,L_DDL,I,J)
         CALL IMP_SMPINI(ITSK   ,F_DDLI ,L_DDLI ,NDDLI   )
         DO I=F_DDLI,L_DDLI
          J=ITOK(I)
          DIAG_T(J)=DIAG_T(J)+DIAG_I(I)
         ENDDO
!$OMP END PARALLEL 
        END IF !(IMUMPSV>0)THEN
        ITQ = 0
C----------------------
c      CALL MY_BARRIER
C---------------------
!$OMP PARALLEL PRIVATE(ITSK,F_DDL,L_DDL,I)
        CALL IMP_SMPINI(ITSK   ,F_DDL ,L_DDL ,NDDL   )
        CALL MAV_LTH0(
     1            NDDL  ,NDDLI ,IADK  ,JDIK  ,DIAG_T,   
     2            LT_K  ,IADI  ,JDII  ,ITOK  ,LT_I  ,
     3            L_U   ,F     ,A     ,AR    ,
     5            VE    ,MS    ,XE    ,D     ,DR    ,
     6            NDOF  ,IPARI ,INTBUF_TAB   ,NUM_IMP,
     7            NS_IMP,NE_IMP,NSREM ,NSL   ,IBFV   ,
     8            SKEW  ,XFRAME,MONVOL,VOLMON,IGRSURF ,
     9            FR_MV,NMONV ,IMONV ,IND_IMP,
     A            XI_C  ,ITQ   ,IRBE3 ,LRBE3 ,IRBE2  ,
     B            LRBE2 ,F_DDL ,L_DDL ,ITSK  )
C----------------------
      CALL MY_BARRIER
C---------------------      
       DO I=F_DDL,L_DDL
         F(I) = F(I)-L_F0(I)
        ENDDO
!$OMP END PARALLEL 
        CALL PRODUT_HP(NDDL,F,F,W_DDL,R2)
        CALL PRODUT_HP(NDDL,L_F0,L_F0,W_DDL,QTOL)
citask0        IF (ITASK == 0) THEN
         R2 = SQRT(R2/QTOL)
         IF(IPRINT/=0) WRITE(IOUT,1003)R2
         IF(IPRINT<0) WRITE(ISTDO,1003)R2
citask0        END IF !(ITASK == 0) THEN
       ENDIF
      ENDIF
C--------POUR F*X--------
       CALL PRODUT_HP(NDDL,L_U,L_F0,W_DDL,F_U)
citask0      IF (ITASK == 0) THEN
C---------
       IF (NMONV>0.AND.ILINE==1) THEN
        CALL RECU_KDIS(NDOF   ,D     )
       ENDIF 
       IF (INTP_C<0.AND.NDDLI>0) THEN
        CALL RECU_KDIS0(NDOF   ,D     )
       ENDIF 
       CALL RECUDIS(NDDL  ,IDDL  ,NDOF  ,IKC   ,L_U     ,
     1              D     ,DR    ,INLOC )
C---------
citask0      END IF !(ITASK == 0) THEN
 200  CONTINUE
!$OMP PARALLEL PRIVATE(ITSK,F_DDL,L_DDL,I)
        CALL IMP_SMPINI(ITSK   ,F_DDL ,L_DDL ,NDDL   )
        DO I=F_DDL,L_DDL
         F(I) = L_F0(I)
        ENDDO
!$OMP END PARALLEL 
C----------------------
c      CALL MY_BARRIER
C---------------------
 1001 FORMAT(3X,'L_SOLVER : ISOLV =',I8,2X,'ITOL =',I8,2X,
     .       'L_TOL =',E11.4)
 1002 FORMAT(/3X,'NUM.QUASI-STATIC ITERATIONS=',I8,5X,
     .          ' RELATIVE ||R||=',E11.4/)
 1003 FORMAT(/3X,'DIRECT SOLVER TERMINATED WITH RELATIVE ||R||=',E11.4/)
      RETURN
      END

C  |---- solveurs directs smp------------
Chd|====================================================================
Chd|  LIN_SOLV2                     source/implicit/lin_solv.F    
Chd|-- called by -----------
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|        LIN_SOLVHM                    source/implicit/lin_solv.F    
Chd|        PREC_SOLV                     source/implicit/prec_solv.F   
Chd|        PREC_SOLVH                    source/implicit/prec_solv.F   
Chd|-- calls ---------------
Chd|        IMP_DSOLV                     source/implicit/imp_dsolv.F   
Chd|        IMP_DSOLV_ITER                source/implicit/imp_dsolv.F   
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE LIN_SOLV2(
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,NDDLI ,ITOK  ,IADI  ,JDII   ,
     3                    LT_I  ,X     ,F     ,ITASK ,IPRINT ,
     4                    ISOLV ,ISTOP ,GRAPHE,ITAB  ,INSOLV,
     5                    IT    ,FAC_K ,IPIV_K,NK    ,DIAG_I,
     6                    IDSC  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "units_c.inc" 
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL  ,NNZ ,IADK(*),JDIK(*),ITASK,IPRINT,
     .        ISTOP,NDDLI,ITOK(*),IADI(*),JDII(*),
     .        ISOLV ,ITAB(*), INSOLV,IT, IPIV_K(*), NK, IDSC
C     REAL
      my_real
     .  DIAG_K(*),LT_K(*),LT_I(*),X(*) ,F(*),
     .  FAC_K(*), DIAG_I(*)
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NF
      my_real
     .        RBID
C------------------------------------------
C     [K]:matrice de rigidite [KI]:matrice de contact
C------------------------------------------
C       IF (IMON>0) CALL STARTIME(33,ITASK+1)
C      IF (ISOLV==3) THEN
C        WRITE(6,*) "BCS Solver not available" 
C        CALL FLUSH(6)
C        CALL ARRET(5)
C       IF (ISOLV==4) THEN
        NF=1
        IF (INSOLV==0) THEN
           CALL IMP_DSOLV(GRAPHE, DIAG_K, LT_K, IADK, JDIK,
     .                    NDDL,   NF,     F,    X,    ITAB,
     .                    IPRINT, NDDLI,  IADI, JDII, DIAG_I,
     .                    LT_I,   ITOK  )
        ELSE
           CALL IMP_DSOLV_ITER(GRAPHE, DIAG_K, LT_K,   IADK,   JDIK,
     .                         NDDL,   NF,     F,      X,      ITAB,
     .                         IT,     FAC_K,  IPIV_K, NK,     IPRINT,
     .                         NDDLI,  IADI,   JDII,   DIAG_I, LT_I,
     .                         ITOK  )
           IDSC=0
        ENDIF
C      ENDIF
C      IF (IMON>0) CALL STOPTIME(33,ITASK+1)
      RETURN
      END

Chd|====================================================================
Chd|  LIN_SOLVP2                    source/implicit/lin_solv.F    
Chd|-- called by -----------
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|        LIN_SOLVHM                    source/implicit/lin_solv.F    
Chd|        PREC_SOLVH                    source/implicit/prec_solv.F   
Chd|        PREC_SOLVP                    source/implicit/prec_solv.F   
Chd|-- calls ---------------
Chd|        IMP_DSFEXT                    source/implicit/imp_dsfext.F  
Chd|        IMP_DSOLV                     source/implicit/imp_dsolv.F   
Chd|        IMP_DSOLV_ITER                source/implicit/imp_dsolv.F   
Chd|        IMP_MUMPS2                    source/implicit/imp_mumps.F   
Chd|        MUMPSLB                       source/implicit/lin_solv.F    
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE LIN_SOLVP2(GRAPHE, F     , NDDL , IAD_ELEM , FR_ELEM,
     1                      DIAG_K, LT_K  , IADK , JDIK     , X      ,
     2                      ITAB  , IPRINT, NDDLI, IADI     , JDII   ,
     3                      DIAG_I, LT_I  , ITOK , INSOLV   , IT     ,
     4                      FAC_K , IPIV_K, NK   , MUMPS_PAR, CDDLP  ,
     5                      ISOLV , IDSC  , IDDL , IKC      , INLOC  ,
     6                      NDOF  , ITASK )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "dmumps_struc.h"
#include      "timeri_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL, IAD_ELEM(2,*), FR_ELEM(*), IADK(*), JDIK(*), 
     .        ITAB(*), IPRINT, NDDLI, IADI(*), JDII(*), ITOK(*),
     .        INSOLV, IT, IPIV_K(*), NK, CDDLP(*), ISOLV, IDSC,
     .        IDDL(*), IKC(*), INLOC(*), NDOF(*), ITASK
      my_real
     .        F(*), DIAG_K(*), LT_K(*), X(*), DIAG_I(*), LT_I(*),
     .        FAC_K(*)
      TYPE(PRGRAPH) :: GRAPHE(*)
      TYPE(DMUMPS_STRUC) MUMPS_PAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NF, I, ITAG(NUMNOD), J
      my_real
     .        F1(NDDL)
C---------------------------------------
C      IF (IMON>0) CALL STARTIME(33,ITASK+1)
      IF (ISOLV==3) THEN
        CALL MUMPSLB(F     ,F1    , NDDL , IAD_ELEM , FR_ELEM,
     1               IDDL  , IKC  , INLOC    ,NDOF    ,ITAG   )
C
        CALL IMP_MUMPS2(MUMPS_PAR, CDDLP, F1 , X, NDDL)
C
      ELSEIF (ISOLV==4) THEN
         NF=1
C Division des forces exterieures sur les ddls de la frontiere par le nombre 
C de processeurs auxquels ils appartiennent (elles sont reassemblees dans DSRESOL)
         CALL IMP_DSFEXT(GRAPHE , NF, F, NDDL, IAD_ELEM,
     .                   FR_ELEM)
C
         IF (INSOLV==0) THEN
            CALL IMP_DSOLV(GRAPHE, DIAG_K, LT_K, IADK, JDIK,
     .                     NDDL,   NF,     F,    X,    ITAB,
     .                     IPRINT, NDDLI,  IADI, JDII, DIAG_I,
     .                     LT_I,   ITOK  )
         ELSE
            CALL IMP_DSOLV_ITER(GRAPHE, DIAG_K, LT_K,   IADK, JDIK,
     .                          NDDL,   NF,     F,      X,    ITAB,
     .                          IT,     FAC_K,  IPIV_K, NK,   IPRINT,
     .                          NDDLI,  IADI,   JDII,   DIAG_I, LT_I,
     .                          ITOK  )
         ENDIF
      ENDIF
      IF (INSOLV/=0) IDSC=0
C      IF (IMON>0) CALL STOPTIME(33,ITASK+1)
C
      RETURN
      END
#endif
Chd|====================================================================
Chd|  ERR_MEM                       source/implicit/lin_solv.F    
Chd|-- called by -----------
Chd|        IMP_FAC_ICJ                   source/implicit/imp_fac_ic.F  
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ERR_MEM(MEM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
#include "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "units_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER
     .  MEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CALL ANCMSG(MSGID=81,ANMODE=ANINFO,
     .            I1=MEM)
      CALL ARRET(2)
      RETURN
      END
#ifdef MUMPS5
Chd|====================================================================
Chd|  RECU_KDIS0                    source/implicit/lin_solv.F    
Chd|-- called by -----------
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        IMP_KNON                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE RECU_KDIS0(NDOF   ,D     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_KNON
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDOF(*)
      my_real
     .       D(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,ND
C---------------------------------
       DO I = 1, NUMN_KN
        N = IN_KN(I)
        DO J = 1, 3
         ND = ID_KN(J,I)
         IF (ND<0) THEN
          D(J,N) = ZERO
         ENDIF 
        ENDDO
       ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  LIN_SOLVH0                    source/implicit/lin_solv.F    
Chd|-- called by -----------
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        IMP_LANZP                     source/implicit/imp_lanz.F    
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|        IMP_PPCGH                     source/implicit/imp_pcg.F     
Chd|        SET_KSYM                      source/implicit/imp_solv.F    
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        IMP_WORKH                     share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE LIN_SOLVH0(TOL  ,
     1                    NDDL  ,NNZ     ,IADK   ,JDIK  ,DIAG_K ,
     2                    LT_K  ,NDDLI   ,ITOK   ,IADI  ,JDII   ,
     3                    LT_I  ,X       ,F      ,ITASK ,IPRINT ,
     4                    F_U   ,ISOLV   ,IPREC  ,L_LIM ,ITOL   ,
     6                    W_DDL ,A       ,AR     ,VE    ,MS     ,
     9                    XE    ,D       ,DR     ,NDOF  ,IPARI  ,
     A                    INTBUF_TAB     ,NUM_IMP,NS_IMP,NE_IMP,
     B                    NSREM ,NSL     ,P_MACH ,ISTOP ,NMONV ,
     E                    IMONV ,MONVOL  ,IGRSURF  ,FR_MV ,
     F                    VOLMON,IBFV  ,SKEW    ,XFRAME ,IND_IMP,
     G                    XI_C  ,F0     ,NDDLI_G,INTP_C,IRBE3  ,
     H                    LRBE3 ,IRBE2  ,LRBE2  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
      USE IMP_WORKH
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
#include "com01_c.inc"
#include "com04_c.inc"
#include "units_c.inc" 
#include "timeri_c.inc"
#include "dmumps_struc.h"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C----------resol [K]{X}={F}------wothout precoditioner----
      INTEGER  NDDL  ,NNZ ,IADK(*),JDIK(*),ITASK,IPRINT,ISTOP,
     .         NDDLI ,ITOK(*) ,IADI(*),JDII(*),NDDLI_G,
     .         ISOLV ,IPREC ,L_LIM,ITOL,W_DDL(*),IBFV(*),INTP_C
      INTEGER  NDOF(*),NE_IMP(*),NSREM ,NSL,
     .         IPARI(*) ,NUM_IMP(*),NS_IMP(*),IND_IMP(*)
      INTEGER NMONV,IMONV(*),MONVOL(*),FR_MV(*),
     .        IRBE3(*)  ,LRBE3(*),IRBE2(*)  ,LRBE2(*)
C     REAL
      my_real
     .  DIAG_K(*),LT_K(*),LT_I(*),  X(*) ,F(*),TOL,F_U,P_MACH
      my_real
     .  A(3,*),AR(3,*),VE(3,*),D(3,*),DR(3,*),XE(3,*),
     .  MS(*),VOLMON(*),SKEW(*)  ,XFRAME(*),XI_C(*) ,F0(*) 

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NNZM,LENV,IBID,IERR,ITSK
      my_real
     .  SHIFT,KCOND,RBID
      TYPE(PRGRAPH) :: GBID
      TYPE(DMUMPS_STRUC) MBID
C      
      INTEGER OMP_GET_THREAD_NUM
      EXTERNAL OMP_GET_THREAD_NUM
C
C-----------------------------------------------
C------------------------------------------
c     isolv=1 => P.G.C.  
c     isolv=2 => P.LANCZOS 
c     isolv=4 => direct multi-condensation
C------------------------------------------
      NNZM = 0
C      IF (IMON>0) CALL STARTIME(33,1)
c      IF (ITASK == 0) THEN
       CALL SET_KSYM(NDDL,IADK,JDIK,LT_K,IADK0,JDIK0,LT_K0)
c      END IF !(ITASK == 0) THEN
C----------------------
c      CALL MY_BARRIER
C---------------------
!$OMP PARALLEL PRIVATE(ITSK)
       ITSK = OMP_GET_THREAD_NUM()
       IF (ISOLV==1.OR.ISOLV>6) THEN
        CALL IMP_PCGH( IPREC ,
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,NDDLI ,ITOK  ,IADI  ,JDII   ,
     3                    LT_I  ,NNZM  ,IADK  ,JDIK  ,DIAG_K ,
     4                    LT_K  ,X     ,F     ,ITOL  ,TOL    ,
     5                    PCG_W1,PCG_W2,PCG_W3,ITSK  ,IPRINT ,
     6                    L_LIM ,P_MACH,F_U   ,ISTOP ,
     8                    W_DDL ,A     ,AR    ,VE    ,MS    ,
     9                    XE    ,D     ,DR    ,NDOF  ,IPARI ,
     A                    INTBUF_TAB   ,NUM_IMP,NS_IMP,NE_IMP,
     B                    NSREM ,NSL   ,NMONV ,IMONV ,MONVOL,
     C                    IGRSURF,VOLMON,FR_MV,IBFV  ,
     D                    SKEW  ,XFRAME ,GBID ,IBID  ,IBID  , 
     E                    IBID  ,IBID  ,IBID   ,RBID ,IBID  ,
     F                    IBID  ,MBID  ,IBID  ,ISOLV ,IBID  ,
     G                    IBID  ,IBID  ,IBID  ,IND_IMP,XI_C ,
     H                    F0    ,NDDLI_G,INTP_C,IRBE3 ,LRBE3,
     I                    IRBE2 ,LRBE2 )
       ELSEIF (ISOLV == 9) THEN
        CALL IMP_PPCGH( IPREC ,
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,NDDLI ,ITOK  ,IADI  ,JDII   ,
     3                    LT_I  ,NNZM  ,IADK  ,JDIK  ,DIAG_K ,
     4                    LT_K  ,X     ,F     ,ITOL  ,TOL    ,
     5                    PCG_W1,PCG_W2,PCG_W3,ITSK  ,IPRINT ,
     6                    L_LIM ,P_MACH,F_U   ,ISTOP ,
     8                    W_DDL ,A     ,AR    ,VE    ,MS    ,
     9                    XE    ,D     ,DR    ,NDOF  ,IPARI ,
     A                    INTBUF_TAB   ,NUM_IMP,NS_IMP,NE_IMP,
     B                    NSREM ,NSL   ,NMONV ,IMONV ,MONVOL,
     C                    IGRSURF ,VOLMON,FR_MV,IBFV  ,
     D                    SKEW  ,XFRAME ,GBID ,IBID  ,IBID  , 
     E                    IBID  ,IBID  ,IBID   ,RBID ,IBID  ,
     F                    IBID  ,MBID  ,IBID  ,ISOLV ,IBID  ,
     G                    IBID  ,IBID  ,IBID  ,IND_IMP,XI_C ,
     H                    F0    ,NDDLI_G,INTP_C,IRBE3 ,LRBE3,
     I                    IRBE2 ,LRBE2  )
       ELSEIF (ISOLV==2.AND.ITSK==0) THEN
        SHIFT=ZERO
        CALL IMP_LANZP(IPREC ,
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,NDDLI ,ITOK  ,IADI  ,JDII   ,
     3                    LT_I  ,NNZM  ,IADK  ,JDIK  ,DIAG_K ,
     4                    LT_K  ,X     ,F     ,ITOL  ,TOL    ,
     5                    PCG_W1,PCG_W2,PCG_W3,ITSK  ,IPRINT ,
     6                    SHIFT ,KCOND ,L_LIM ,P_MACH,F_U    ,
     7                    ISTOP ,W_DDL ,A     ,AR     ,
     9                    VE    ,MS    ,XE    ,D     ,DR    ,
     A                    NDOF  ,IPARI ,INTBUF_TAB   ,NUM_IMP,
     B                    NS_IMP,NE_IMP,NSREM ,NSL   ,NMONV ,
     C                    IMONV ,MONVOL,IGRSURF,VOLMON,
     D                    FR_MV ,IBFV  ,SKEW  ,XFRAME,IND_IMP,
     H                    XI_C  ,F0    ,NDDLI_G,INTP_C,IRBE3  ,
     E                    LRBE3 ,IRBE2 ,LRBE2 )
       ENDIF
!$OMP END PARALLEL 
C
C       IF (IMON>0) CALL STOPTIME(33,1)
      RETURN
      END
C  |---- version Hybrid------------
Chd|====================================================================
Chd|  LIN_SOLVH1                    source/implicit/lin_solv.F    
Chd|-- called by -----------
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        FR_DLFT                       source/mpi/implicit/imp_fri.F 
Chd|        IMP_DIAGS                     source/mpi/implicit/imp_fri.F 
Chd|        IMP_DIAGSN                    source/mpi/implicit/imp_fri.F 
Chd|        IMP_FSA_INVHP                 source/implicit/imp_fsa_inv.F 
Chd|        IMP_LANZP                     source/implicit/imp_lanz.F    
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|        IMP_PPCGH                     source/implicit/imp_pcg.F     
Chd|        MONV_DIAG                     source/airbag/monv_imp0.F     
Chd|        SET_KSYM                      source/implicit/imp_solv.F    
Chd|        SPMD_MAX_I                    source/mpi/implicit/imp_spmd.F
Chd|        SPMD_SUMF_K                   source/mpi/implicit/imp_spmd.F
Chd|        SPMD_SUMF_V                   source/mpi/implicit/imp_spmd.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        IMP_WORKH                     share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE LIN_SOLVH1(TOL  ,MAX_L  ,
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,NDDLI ,ITOK  ,IADI  ,JDII   ,
     3                    LT_I  ,DIAG_M,LT_M  ,X     ,F      ,
     4                    ITASK ,ICPREC,IPRINT,F_U   ,ISOLV  ,
     5                    IPREC ,L_LIM ,ITOL  ,INEGA ,W_DDL  ,
     7                    A     ,AR    ,VE     ,MS    ,XE    ,
     8                    D     ,DR    ,NDOF  ,IPARI  ,INTBUF_TAB,
     9                    NUM_IMP,NS_IMP,NE_IMP,NSREM ,
     A                    NSL   ,P_MACH,MAXB   ,ISTOP ,NMONV ,
     B                    IMONV ,MONVOL,IGRSURF ,FR_MV ,
     C                    VOLMON,IBFV  ,SKEW  ,XFRAME ,IND_IMP,
     D                    DIAG_I,XI_C  ,F0    ,NDDLI_G,INTP_C,
     E                    IRBE3  ,LRBE3,IRBE2 ,LRBE2 )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
      USE IMP_WORKH
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
#include "com01_c.inc"
#include "com04_c.inc"
#include "units_c.inc" 
#include "timeri_c.inc"
#include "task_c.inc" 
#include "dmumps_struc.h"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C----------resol [K]{X}={F}------
      INTEGER  NDDL  ,NNZ ,IADK(*),JDIK(*),ITASK,IPRINT,ICPREC,
     .         ISTOP,NDDLI ,ITOK(*) ,IADI(*),JDII(*),
     .         ISOLV  ,IPREC ,L_LIM ,MAXB,ITOL,INEGA,
     .         W_DDL(*),IBFV(*),MAX_L,INTP_C,NDDLI_G
      INTEGER  NDOF(*),NE_IMP(*),NSREM ,NSL,IRBE3(*)  ,LRBE3(*), 
     .         IPARI(*) ,NUM_IMP(*),NS_IMP(*) ,IND_IMP(*),
     .         IRBE2(*),LRBE2(*)
      INTEGER NMONV,IMONV(*),MONVOL(*),FR_MV(*)
C     REAL
      my_real
     .  DIAG_K(*),LT_K(*),LT_I(*),DIAG_M(*),LT_M(*), 
     .  X(*) ,F(*),TOL,F_U,P_MACH,XI_C(*),F0(*)
      my_real
     .  A(3,*),AR(3,*),VE(3,*),D(3,*),DR(3,*),XE(3,*),
     .  MS(*),VOLMON(*),SKEW(*)  ,XFRAME(*),DIAG_I(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NNE,NNZM,MAXC,MAXA,LEN,LENV, IDLFT0,IDLFT1
      INTEGER  SIZI(NSPMD+1),MAXS0,MAXS1,IG,IBID,IERR,ITSK
      my_real
     .  SHIFT ,KCOND,RBID
      TYPE(PRGRAPH) :: GBID
      TYPE(DMUMPS_STRUC) MBID
C      
      INTEGER OMP_GET_THREAD_NUM
      EXTERNAL OMP_GET_THREAD_NUM
C
C-----------------------------------------------
C------------------------------------------
c     isolv=1 => P.G.C.  
C------------------------------------------
C------------------------------------------
c     iprec=2 => jacobien 
c     iprec=5 => fsai static same pattern of [K}
C------------------------------------------
C------------------------------------------
C     [K]:matrice de rigidite [M]:matrice de preconditionnement
       NNE = 0
       NNZM = NNZ
       ISTOP = INEGA
C
      IF (IPREC==2) NNZM=0 
      IF (IMON>0) CALL STARTIME(32,1)
      IF (ICPREC==1) THEN
       IF (IPREC==2) THEN
        DO I=1,NDDL
         DIAG_M(I) = DIAG_K(I)
        ENDDO 
        IF (NSL>0) 
     .   CALL IMP_DIAGS(DIAG_M ,NDOF,NSL,IPARI,INTBUF_TAB,IRBE3,LRBE3,IRBE2)
        IF (NMONV>0) 
     .    CALL MONV_DIAG(DIAG_M,NDOF,IPARI,INTBUF_TAB,IRBE3 ,LRBE3 ,IRBE2,0)
        IF (NSPMD>1) CALL SPMD_SUMF_V(DIAG_M)
C-------------to add //         
        DO I=1,NDDL
         IF (DIAG_M(I)<EM20) THEN
          NNE=NNE+1
          DIAG_M(I)=ABS(DIAG_M(I))
          DIAG_M(I)=MAX(EM20,DIAG_M(I))
         ENDIF
         DIAG_M(I) = W_DDL(I)/DIAG_M(I)
        ENDDO 
        IF (NNE>0.AND.ISOLV/=2) ISTOP=NNE
       ELSEIF (IPREC==3) THEN
       ELSEIF (IPREC==5) THEN
C     ----- approx. (by each colonne of L_T) inverse ---------
C     ----- utilise d'abord la place de [M] pour la matrice assemblee ---------
         IF (NSPMD==1) THEN
             IDLFT0=0
             IDLFT1=0
             ELSE
         CALL FR_DLFT(NDDL,IDLFT0,IDLFT1)
             ENDIF
citask0       IF (ITASK == 0) THEN
C---------
        IF (NSL>0) 
     .   CALL IMP_DIAGS(DIAG_K ,NDOF,NSL,IPARI,INTBUF_TAB,IRBE3,LRBE3,IRBE2)
        IF (NMONV>0) 
     .    CALL MONV_DIAG(DIAG_K,NDOF,IPARI,INTBUF_TAB,IRBE3 ,LRBE3 ,IRBE2,0)
        DO I=1,IDLFT0
         DIAG_M(I) = DIAG_K(I)
         DO J=IADK(I),IADK(I+1)-1
          LT_M(J)=LT_K(J)
         ENDDO 
        ENDDO 
        DO I=IDLFT1+1,NDDL
         DIAG_M(I) = DIAG_K(I)
         DO J=IADK(I),IADK(I+1)-1
          LT_M(J)=LT_K(J)
         ENDDO 
        ENDDO 
C
        IF (NSPMD>1) CALL SPMD_SUMF_K(DIAG_M   ,LT_M     )
C---------------------
citask0       END IF !(ITASK == 0) THEN
C----------------------
c      CALL MY_BARRIER
C---------------------
        MAXC=MAXB
        IF (MAXC>10000) then
          MAXA = MAX_L
        ELSE
          MAXA = 1+(MAXC*(MAXC-1))/2
        ENDIF
        MAXS1=IADK(NDDL+1)-IADK(IDLFT1+1)
C ----------to do // inside  IMP_FSA_INVH    
        CALL IMP_FSA_INVHP(
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,DIAG_M,LT_M  ,MAXC  ,MAXA   ,
     4                    INEGA ,IDLFT0,IDLFT1,MAXS1 )
C----------------------
c      CALL MY_BARRIER
C---------------------
citask0       IF (ITASK == 0) THEN
C---------------------
        DO I=1,IDLFT0
         DIAG_M(I) = ZERO
         DO J=IADK(I),IADK(I+1)-1
          LT_M(J)=ZERO
         ENDDO 
        ENDDO 
        IG = INEGA
        IF (NSPMD>1) CALL SPMD_MAX_I(INEGA)
        IF (INEGA>0.AND.ISOLV/=2.AND.NCYCLE>1)ISTOP=INEGA
        IF (NSL>0) 
     .   CALL IMP_DIAGSN(DIAG_K,NDOF,NSL,IPARI,INTBUF_TAB,IRBE3,LRBE3,IRBE2)
        INEGA = IG
        IF (NMONV>0) 
     .   CALL MONV_DIAG(DIAG_K,NDOF,IPARI,INTBUF_TAB,IRBE3,LRBE3,IRBE2,1)
        ELSE
C       WRITE()''      
        ENDIF
C---------------------
citask0       END IF !(ITASK == 0) THEN

      ENDIF
C
      IF(NNE > 0)THEN
       WRITE(IOUT,2001)NNE
       WRITE(ISTDO,2001)NNE
      ENDIF
      IF (IMON>0) CALL STOPTIME(32,1)
      IF (ISTOP>0) RETURN
C
citask0      IF (ITASK == 0) THEN
       IF (INTP_C<0) THEN
         DO I=1,NDDLI
          J=ITOK(I)
          DIAG_K(J)=DIAG_K(J)-DIAG_I(I)
         ENDDO
       ENDIF
C------
       CALL SET_KSYM(NDDL,IADK,JDIK,LT_K,IADK0,JDIK0,LT_K0)
       IF (IPREC == 5) 
     1    CALL SET_KSYM(NDDL,IADK,JDIK,LT_M,IADM0,JDIM0,LT_M0)
citask0      END IF !(ITASK == 0) THEN
C----------------------
c      CALL MY_BARRIER
C---------------------
C      IF (IMON>0) CALL STARTIME(33,1)
!$OMP PARALLEL PRIVATE(ITSK)
       ITSK = OMP_GET_THREAD_NUM()
       IF (ISOLV==1.OR.ISOLV==7.OR.ISOLV==8) THEN
        CALL IMP_PCGH( IPREC ,
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,NDDLI ,ITOK  ,IADI  ,JDII   ,
     3                    LT_I  ,NNZM  ,IADK  ,JDIK  ,DIAG_M ,
     4                    LT_M  ,X     ,F     ,ITOL  ,TOL    ,
     5                    PCG_W1,PCG_W2,PCG_W3,ITSK  ,IPRINT ,
     6                    L_LIM ,P_MACH,F_U   ,ISTOP ,
     8                    W_DDL ,A     ,AR    ,VE    ,MS    ,
     9                    XE    ,D     ,DR    ,NDOF  ,IPARI ,
     A                    INTBUF_TAB   ,NUM_IMP,NS_IMP,NE_IMP,
     B                    NSREM ,NSL   ,NMONV ,IMONV ,MONVOL,
     C                    IGRSURF,VOLMON,FR_MV,IBFV  ,
     D                    SKEW  ,XFRAME ,GBID ,IBID  ,IBID  , 
     E                    IBID  ,IBID  ,IBID   ,RBID ,IBID  ,
     F                    IBID  ,MBID  ,IBID  ,ISOLV ,IBID  ,
     G                    IBID  ,IBID  ,IBID  ,IND_IMP,XI_C ,
     H                    F0    ,NDDLI_G,INTP_C,IRBE3 ,LRBE3,
     I                    IRBE2 ,LRBE2  )
       ELSEIF (ISOLV == 9) THEN
        CALL IMP_PPCGH( IPREC ,
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,NDDLI ,ITOK  ,IADI  ,JDII   ,
     3                    LT_I  ,NNZM  ,IADK  ,JDIK  ,DIAG_M ,
     4                    LT_M  ,X     ,F     ,ITOL  ,TOL    ,
     5                    PCG_W1,PCG_W2,PCG_W3,ITSK  ,IPRINT ,
     6                    L_LIM ,P_MACH,F_U   ,ISTOP ,
     8                    W_DDL ,A     ,AR    ,VE    ,MS    ,
     9                    XE    ,D     ,DR    ,NDOF  ,IPARI ,
     A                    INTBUF_TAB   ,NUM_IMP,NS_IMP,NE_IMP,
     B                    NSREM ,NSL   ,NMONV ,IMONV ,MONVOL,
     C                    IGRSURF ,VOLMON,FR_MV,IBFV  ,
     D                    SKEW  ,XFRAME ,GBID ,IBID  ,IBID  , 
     E                    IBID  ,IBID  ,IBID   ,RBID ,IBID  ,
     F                    IBID  ,MBID  ,IBID  ,ISOLV ,IBID  ,
     G                    IBID  ,IBID  ,IBID  ,IND_IMP,XI_C ,
     H                    F0    ,NDDLI_G,INTP_C,IRBE3 ,LRBE3,
     I                    IRBE2 ,LRBE2  )
       ELSEIF (ISOLV==2.AND.ITSK==0) THEN
        SHIFT=ZERO
        CALL IMP_LANZP(IPREC ,
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,NDDLI ,ITOK  ,IADI  ,JDII   ,
     3                    LT_I  ,NNZM  ,IADK  ,JDIK  ,DIAG_M ,
     4                    LT_M  ,X     ,F     ,ITOL  ,TOL    ,
     5                    PCG_W1,PCG_W2,PCG_W3,ITSK  ,IPRINT ,
     6                    SHIFT ,KCOND ,L_LIM ,P_MACH,F_U    ,
     7                    ISTOP ,W_DDL ,A     ,AR     ,
     9                    VE    ,MS    ,XE    ,D     ,DR    ,
     A                    NDOF  ,IPARI ,INTBUF_TAB   ,NUM_IMP,
     B                    NS_IMP,NE_IMP,NSREM ,NSL   ,NMONV ,
     C                    IMONV ,MONVOL,IGRSURF,VOLMON,
     D                    FR_MV ,IBFV   ,SKEW ,XFRAME,IND_IMP,
     H                    XI_C  ,F0    ,NDDLI_G,INTP_C,IRBE3  ,
     E                    LRBE3 ,IRBE2 ,LRBE2  )
       ENDIF
!$OMP END PARALLEL 
C
C       IF (IMON>0) CALL STOPTIME(33,1)
C--------------------------------------------
 1002 FORMAT(3X,'PRECONDITION METHOD : JACOBIEN '/)
 1003 FORMAT(3X,'PRECONDITION METHOD : Ic0 ')
 1004 FORMAT(3X,'PRECONDITION METHOD : Ic0_stab ')
 1009 FORMAT(3X,'PRECONDITION METHOD : FSAI_R  ' )
 2001 FORMAT(3X,'---WARNING :',I8,3X,
     .          'TOO SMALL PIVOT IN FACTORIZATION'/)
      RETURN
      END
Chd|====================================================================
Chd|  LIN_SOLVIH2                   source/implicit/lin_solv.F    
Chd|-- called by -----------
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        FR_DLFT                       source/mpi/implicit/imp_fri.F 
Chd|        IMP_DIAGS                     source/mpi/implicit/imp_fri.F 
Chd|        IMP_DIAGSN                    source/mpi/implicit/imp_fri.F 
Chd|        IMP_FSA_INV2HP                source/implicit/imp_fsa_inv.F 
Chd|        IMP_LANZP                     source/implicit/imp_lanz.F    
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|        IMP_PPCGH                     source/implicit/imp_pcg.F     
Chd|        IND_SPAN                      source/implicit/ind_glob_k.F  
Chd|        MONV_DIAG                     source/airbag/monv_imp0.F     
Chd|        SET_KSYM                      source/implicit/imp_solv.F    
Chd|        SPMD_MAX_I                    source/mpi/implicit/imp_spmd.F
Chd|        SPMD_SUMF_K                   source/mpi/implicit/imp_spmd.F
Chd|        SPMD_SUM_S                    source/mpi/implicit/imp_spmd.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        IMP_WORKH                     share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE LIN_SOLVIH2(TOL  ,N_PAT,MAXB1  ,
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,NDDLI ,ITOK  ,IADI  ,JDII   ,
     3                    LT_I  ,IADM  ,JDIM  ,DIAG_M ,LT_M  ,
     4                    X     ,F     ,MAX_L ,D_TOL  ,
     4                    ITASK ,ICPREC,IPRINT,F_U   ,ISOLV  ,
     5                    IPREC ,L_LIM ,ITOL  ,INEGA ,W_DDL  ,
     7                    A     ,AR    ,VE     ,MS    ,XE    ,
     8                    D     ,DR    ,NDOF  ,IPARI  ,INTBUF_TAB,
     9                    NUM_IMP,NS_IMP,NE_IMP,NSREM ,
     A                    NSL   ,P_MACH,MAXB   ,ISTOP ,NMONV ,
     B                    IMONV ,MONVOL,IGRSURF ,FR_MV ,
     C                    VOLMON,IBFV  ,SKEW  ,XFRAME ,IND_IMP,
     D                    DIAG_I,XI_C  ,F0    ,NDDLI_G,INTP_C,
     E                    IRBE3  ,LRBE3,IRBE2 ,LRBE2   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
      USE IMP_WORKH
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
#include "com01_c.inc"
#include "com04_c.inc"
#include "units_c.inc" 
#include "timeri_c.inc"
#include "task_c.inc" 
#include "dmumps_struc.h"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C----------resol [K]{X}={F}------
      INTEGER  NDDL  ,NNZ ,IADK(*),JDIK(*),ITASK,IPRINT,ICPREC,
     .         ISTOP,NDDLI ,ITOK(*) ,IADI(*),JDII(*),
     .         ISOLV  ,IPREC ,L_LIM ,MAXB,ITOL,INEGA,INTP_C,
     .         W_DDL(*),IBFV(*) ,IADM(*),JDIM(*),MAX_L,N_PAT,MAXB1
      INTEGER  NDOF(*),NE_IMP(*),NSREM ,NSL,NDDLI_G,
     .         IPARI(*) ,NUM_IMP(*),NS_IMP(*),IND_IMP(*) ,
     .         IRBE3(*)  ,LRBE3(*),IRBE2(*)  ,LRBE2(*)
      INTEGER NMONV,IMONV(*),MONVOL(*),FR_MV(*)
C     REAL
      my_real
     .  DIAG_K(*),LT_K(*),LT_I(*),DIAG_M(*),LT_M(*), 
     .  X(*) ,F(*),TOL,F_U,P_MACH,D_TOL,XI_C(*),F0(*)
      my_real
     .  A(3,*),AR(3,*),VE(3,*),D(3,*),DR(3,*),XE(3,*),
     .  MS(*),VOLMON(*),SKEW(*)  ,XFRAME(*),DIAG_I(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NNE,NNZM,MAXC,MAXA,LEN,LENV, IDLFT0,IDLFT1
      INTEGER MAXS0,MAXS1,IG,IBID,IERR,ITSK
      my_real
     .  SHIFT ,KCOND,FAC,RBID
      TYPE(PRGRAPH) :: GBID
      TYPE(DMUMPS_STRUC) MBID
C      
      INTEGER OMP_GET_THREAD_NUM
      EXTERNAL OMP_GET_THREAD_NUM
C
C-----------------------------------------------
C------------------------------------------
c     isolv=1 => P.G.C.  
C------------------------------------------
C------------------------------------------
c     iprec=2 => jacobien 
c     iprec=5 => fsai static same pattern of [K}
C------------------------------------------
C------------------------------------------
C     [K]:matrice de rigidite [M]:matrice de preconditionnement
      ISTOP = INEGA
C
      IF (IMON>0.AND.ITASK==0) CALL STARTIME(32,1)
      IF (ICPREC==1) THEN
       IF (IPREC==5) THEN
C     ----- approx. (by each colonne of L_T) inverse ---------
C     ----- utilise d'abord la place de [M] pour la matrice assemblee ---------
        IF (NSPMD==1) THEN
             IDLFT0=0
             IDLFT1=0
        ELSE
         CALL FR_DLFT(NDDL,IDLFT0,IDLFT1)
        ENDIF
citask0       IF (ITASK == 0) THEN
C-------use [M] for [K] assemblage--------------
        IF (NSL>0) 
     .   CALL IMP_DIAGS(DIAG_K ,NDOF,NSL,IPARI,INTBUF_TAB,IRBE3,LRBE3,IRBE2)
        IF (NMONV>0) 
     .    CALL MONV_DIAG(DIAG_K,NDOF,IPARI,INTBUF_TAB,IRBE3 ,LRBE3 ,IRBE2,0)
        DO I=1,IDLFT0
         DIAG_M(I) = DIAG_K(I)
         DO J=IADK(I),IADK(I+1)-1
          LT_M(J)=LT_K(J)
         ENDDO 
        ENDDO 
        DO I=IDLFT1+1,NDDL
         DIAG_M(I) = DIAG_K(I)
         DO J=IADK(I),IADK(I+1)-1
          LT_M(J)=LT_K(J)
         ENDDO 
        ENDDO 
C
        IF (NSPMD>1) CALL SPMD_SUMF_K(DIAG_M   ,LT_M     )
C       
       CALL IND_SPAN(N_PAT,IDLFT0,NDDL,IADK,JDIK,IADM,JDIM,W_MAXL,MAXB1)
citask0       END IF !(ITASK == 0) THEN
C----------------------
c      CALL MY_BARRIER
C---------------------
        MAXC = W_MAXL
        IF (MAXC>10000) THEN
          MAXA = MAX_L
        ELSE
          MAXA = 1+(MAXC*(MAXC-1))/2
        ENDIF
        MAXS1=IADK(NDDL+1)-IADK(IDLFT1+1)
C---------------------
C----------to add // inside
        CALL IMP_FSA_INV2HP(
     1                    NDDL   ,IADK   ,JDIK   ,DIAG_K ,LT_K   ,   
     2                    IADM   ,JDIM   ,DIAG_M ,LT_M   ,MAXC   ,
     3                    MAXA   ,INEGA  ,IDLFT0 ,IDLFT1 ,MAXS1  ,
     4                    D_TOL  ,P_MACH )
C----------------------
c      CALL MY_BARRIER
C---------------------
citask0       IF (ITASK == 0) THEN
C-------
        DO I=1,IDLFT0
         DIAG_M(I) = ZERO
         DO J=IADM(I),IADM(I+1)-1
          LT_M(J)=ZERO
         ENDDO 
        ENDDO 
        IG = INEGA
        IF (NSPMD>1) CALL SPMD_MAX_I(INEGA)
        IF (INEGA>0.AND.ISOLV/=2.AND.NCYCLE>1)ISTOP=INEGA
        IF (NSL>0) 
     .   CALL IMP_DIAGSN(DIAG_K,NDOF,NSL,IPARI,INTBUF_TAB,IRBE3,LRBE3,IRBE2)
        INEGA = IG
        IF (NMONV>0) 
     .   CALL MONV_DIAG(DIAG_K,NDOF,IPARI,INTBUF_TAB,IRBE3,LRBE3,IRBE2,1)
        ELSE
C       WRITE()''      
        ENDIF
C---------------------
citask0       END IF !(ITASK == 0) THEN

      ENDIF
C
      IF (IMON>0) CALL STOPTIME(32,1)
      IF (ISTOP>0) RETURN
C
citask0      IF (ITASK == 0) THEN
C-------
       NNZM = IADM(NDDL+1)-IADM(1)
       FAC=ONE*NNZM/NNZ/NSPMD
       IF (NSPMD > 1) CALL SPMD_SUM_S(FAC)
       IF (ISPMD==0.AND.IPRINT/=0) THEN
        WRITE(IOUT,1002)FAC
        IF (IPRINT<0) WRITE(ISTDO,1002)FAC
       ENDIF
C
       IF (INTP_C<0) THEN
         DO I=1,NDDLI
          J=ITOK(I)
          DIAG_K(J)=DIAG_K(J)-DIAG_I(I)
         ENDDO
       ENDIF
C       
       CALL SET_KSYM(NDDL,IADK,JDIK,LT_K,IADK0,JDIK0,LT_K0)
       IF (IPREC == 5) 
     1    CALL SET_KSYM(NDDL,IADM,JDIM,LT_M,IADM0,JDIM0,LT_M0)
citask0      END IF !(ITASK == 0) THEN
C----------------------
c      CALL MY_BARRIER
C---------------------
!$OMP PARALLEL PRIVATE(ITSK)
      ITSK = OMP_GET_THREAD_NUM()
      IF (ISOLV==1.OR.ISOLV==7.OR.ISOLV==8) THEN
        CALL IMP_PCGH( IPREC ,
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,NDDLI ,ITOK  ,IADI  ,JDII   ,
     3                    LT_I  ,NNZM  ,IADM  ,JDIM  ,DIAG_M ,
     4                    LT_M  ,X     ,F     ,ITOL  ,TOL    ,
     5                    PCG_W1,PCG_W2,PCG_W3,ITSK  ,IPRINT ,
     6                    L_LIM ,P_MACH,F_U   ,ISTOP ,
     8                    W_DDL ,A     ,AR    ,VE    ,MS    ,
     9                    XE    ,D     ,DR    ,NDOF  ,IPARI ,
     A                    INTBUF_TAB   ,NUM_IMP,NS_IMP,NE_IMP,
     B                    NSREM ,NSL   ,NMONV ,IMONV ,MONVOL,
     C                    IGRSURF,VOLMON,FR_MV,IBFV  ,
     D                    SKEW  ,XFRAME ,GBID ,IBID  ,IBID  , 
     E                    IBID  ,IBID  ,IBID   ,RBID ,IBID  ,
     F                    IBID  ,MBID  ,IBID  ,ISOLV ,IBID  ,
     G                    IBID  ,IBID  ,IBID  ,IND_IMP,XI_C ,
     H                    F0    ,NDDLI_G,INTP_C,IRBE3 ,LRBE3,
     I                    IRBE2 ,LRBE2  )
       ELSEIF (ISOLV == 9) THEN
        CALL IMP_PPCGH( IPREC ,
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,NDDLI ,ITOK  ,IADI  ,JDII   ,
     3                    LT_I  ,NNZM  ,IADM  ,JDIM  ,DIAG_M ,
     4                    LT_M  ,X     ,F     ,ITOL  ,TOL    ,
     5                    PCG_W1,PCG_W2,PCG_W3,ITSK  ,IPRINT ,
     6                    L_LIM ,P_MACH,F_U   ,ISTOP ,
     8                    W_DDL ,A     ,AR    ,VE    ,MS    ,
     9                    XE    ,D     ,DR    ,NDOF  ,IPARI ,
     A                    INTBUF_TAB   ,NUM_IMP,NS_IMP,NE_IMP,
     B                    NSREM ,NSL   ,NMONV ,IMONV ,MONVOL,
     C                    IGRSURF ,VOLMON,FR_MV,IBFV  ,
     D                    SKEW  ,XFRAME ,GBID ,IBID  ,IBID  , 
     E                    IBID  ,IBID  ,IBID   ,RBID ,IBID  ,
     F                    IBID  ,MBID  ,IBID  ,ISOLV ,IBID  ,
     G                    IBID  ,IBID  ,IBID  ,IND_IMP,XI_C ,
     H                    F0    ,NDDLI_G,INTP_C,IRBE3 ,LRBE3,
     I                    IRBE2 ,LRBE2  )
      ELSEIF (ISOLV==2.AND.ITSK==0) THEN
        SHIFT=ZERO
        CALL IMP_LANZP(IPREC ,
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,NDDLI ,ITOK  ,IADI  ,JDII   ,
     3                    LT_I  ,NNZM  ,IADM  ,JDIM  ,DIAG_M ,
     4                    LT_M  ,X     ,F     ,ITOL  ,TOL    ,
     5                    PCG_W1,PCG_W2,PCG_W3,ITASK ,IPRINT ,
     6                    SHIFT ,KCOND ,L_LIM ,P_MACH,F_U    ,
     7                    ISTOP ,W_DDL ,A     ,AR     ,
     9                    VE    ,MS    ,XE    ,D     ,DR    ,
     A                    NDOF  ,IPARI ,INTBUF_TAB   ,NUM_IMP,
     B                    NS_IMP,NE_IMP,NSREM ,NSL   ,NMONV ,
     C                    IMONV ,MONVOL,IGRSURF ,VOLMON,
     D                    FR_MV ,IBFV   ,SKEW  ,XFRAME,IND_IMP,
     H                    XI_C  ,F0    ,NDDLI_G,INTP_C,IRBE3  ,
     E                    LRBE3 ,IRBE2 ,LRBE2  )
       ENDIF
!$OMP END PARALLEL
C
C       IF (IMON>0) CALL STOPTIME(33,1)
 1002 FORMAT(3X,'END PRECONDITION WITH RELATIVE DENSITY =',E11.4/)
C--------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  LIN_SOLVHM                    source/implicit/lin_solv.F    
Chd|-- called by -----------
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|        LIN_SOLV2                     source/implicit/lin_solv.F    
Chd|        LIN_SOLVP2                    source/implicit/lin_solv.F    
Chd|        SET_KSYM                      source/implicit/imp_solv.F    
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        IMP_WORKH                     share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE LIN_SOLVHM(TOL  ,
     1                    NDDL  ,NNZ     ,IADK   ,JDIK  ,DIAG_K ,
     2                    LT_K  ,NDDLI   ,ITOK   ,IADI  ,JDII   ,
     3                    LT_I  ,X       ,F      ,ITASK ,IPRINT ,
     4                    F_U   ,ISOLV   ,IPREC  ,L_LIM ,ITOL   ,
     6                    W_DDL ,A       ,AR     ,VE    ,MS     ,
     9                    XE    ,D       ,DR     ,NDOF  ,IPARI  ,
     A                    INTBUF_TAB     ,NUM_IMP,NS_IMP,NE_IMP,
     B                    NSREM ,NSL     ,P_MACH ,ISTOP ,NMONV ,
     E                    IMONV ,MONVOL  ,IGRSURF  ,FR_MV ,
     F                    VOLMON,IBFV  ,SKEW    ,XFRAME ,GRAPHE, 
     G                    IAD_ELEM,FR_ELEM,ITAB  ,INSOLV ,ITN   ,
     H                    FAC_K ,IPIV_K ,NK     ,MUMPS_PAR,CDDLP,
     I                    IDSC  ,IDDL  ,IKC    ,INLOC  ,DIAG_I  ,
     J                    ILINE ,ILINTF,IND_IMP,XI_C  ,F0     ,
     K                    NDDLI_G,INTP_C,IRBE3  ,LRBE3  ,IRBE2 ,
     L                    LRBE2  ,IT_PCG,IMUMPSV)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
      USE IMP_WORKH
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
#include "com01_c.inc"
#include "com02_c.inc"
#include "com04_c.inc"
#include "units_c.inc" 
#include "timeri_c.inc"
#include "dmumps_struc.h"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C----------resol [K]{X}={F}------wothout precoditioner----
      INTEGER  NDDL  ,NNZ ,IADK(*),JDIK(*),ITASK,IPRINT,ISTOP,
     .         NDDLI ,ITOK(*) ,IADI(*),JDII(*),
     .         ISOLV ,IPREC ,L_LIM,ITOL,W_DDL(*),IBFV(*)
      INTEGER  NDOF(*),NE_IMP(*),NSREM ,NSL,NDDLI_G,
     .         IPARI(*) ,NUM_IMP(*),NS_IMP(*),IND_IMP(*)
      INTEGER NMONV,IMONV(*),MONVOL(*),FR_MV(*)
      INTEGER IAD_ELEM(2,*), FR_ELEM(*), ITAB(*),  
     .        INSOLV, ITN, IPIV_K(*), NK, CDDLP(*),IDSC,
     .        IDDL(*), IKC(*), INLOC(*),ILINE ,ILINTF,INTP_C,
     .        IRBE3(*)  ,LRBE3(*) ,IRBE2(*)  ,LRBE2(*) ,IT_PCG,IMUMPSV
C     REAL
      my_real
     .  DIAG_K(*),LT_K(*),LT_I(*),  X(*) ,F(*),TOL,F_U,P_MACH,
     .  DIAG_I(*),XI_C(*),F0(*)
      my_real
     .  A(3,*),AR(3,*),VE(3,*),D(3,*),DR(3,*),XE(3,*),
     .  MS(*),VOLMON(*),SKEW(*)  ,XFRAME(*),FAC_K(*)
      TYPE(PRGRAPH) :: GRAPHE(*)
      TYPE(DMUMPS_STRUC) MUMPS_PAR
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NNZM,LENV,IBID,ISOL,IERR,IDS,ITSK
      my_real
     .  SHIFT,KCOND,RBID
      INTEGER OMP_GET_THREAD_NUM
      EXTERNAL OMP_GET_THREAD_NUM
C
C-----------------------------------------------
      NNZM = 0
      ISOL = ISOLV-2
C------to avoid the modif of IDSC with LIN_SOLV2,LIN_SOLVP2 :issue oof nt>1     
      IDS = IDSC
C      IF (IMON>0) CALL STARTIME(33,1)
      IF (IDSC==0) THEN
citask0       IF (ITASK==0) THEN
        IF (INTP_C>=0) THEN
         DO I=1,NDDLI
          J=ITOK(I)
          DIAG_K(J)=DIAG_K(J)+DIAG_I(I)
         ENDDO
        ENDIF 
        CALL SET_KSYM(NDDL,IADK,JDIK,LT_K,IADK0,JDIK0,LT_K0)
citask0       END IF !(ITASK==0) THEN
C----------------------
c      CALL MY_BARRIER
C---------------------
!$OMP PARALLEL PRIVATE(ITSK)
        ITSK = OMP_GET_THREAD_NUM()
        CALL IMP_PCGH( IPREC ,
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,NDDLI ,ITOK  ,IADI  ,JDII   ,
     3                    LT_I  ,NNZM  ,IADK  ,JDIK  ,DIAG_K ,
     4                    LT_K  ,X     ,F     ,ITOL  ,TOL    ,
     5                    PCG_W1,PCG_W2,PCG_W3,ITSK  ,IPRINT ,
     6                    L_LIM ,P_MACH,F_U   ,ISTOP ,
     8                    W_DDL ,A     ,AR    ,VE    ,MS    ,
     9                    XE    ,D     ,DR    ,NDOF  ,IPARI ,
     A                    INTBUF_TAB   ,NUM_IMP,NS_IMP,NE_IMP,
     B                    NSREM ,NSL   ,NMONV ,IMONV ,MONVOL,
     C                    IGRSURF,VOLMON,FR_MV,IBFV  ,
     D                    SKEW  ,XFRAME ,GRAPHE,IAD_ELEM,FR_ELEM, 
     E                    INSOLV ,ITN   ,FAC_K ,IPIV_K,NK    ,
     F                    ITAB   ,MUMPS_PAR,CDDLP,ISOL,IDSC  ,
     G                    IDDL  ,IKC    ,INLOC ,IND_IMP,XI_C ,
     H                    F0    ,NDDLI_G,INTP_C,IRBE3  ,LRBE3 ,
     I                    IRBE2 ,LRBE2 )
!$OMP END PARALLEL 
       ELSE
citask0        IF (ITASK == 0) THEN
         IF (IMON>0) CALL STARTIME(32,1)
         IF (NSPMD == 1.AND.IMUMPSV==0) THEN
         CALL LIN_SOLV2(
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,NDDLI ,ITOK  ,IADI  ,JDII   ,
     3                    LT_I  ,X     ,F     ,ITASK ,IPRINT ,
     4                    ISOL  ,ISTOP ,GRAPHE,ITAB  ,INSOLV,
     5                    ITN   ,FAC_K ,IPIV_K,NK    ,DIAG_I,
     6                    IDS   )
         ELSE
         CALL LIN_SOLVP2(GRAPHE, F,      NDDL,  IAD_ELEM,  FR_ELEM,
     1                  DIAG_K, LT_K,   IADK,  JDIK,      X, 
     2                  ITAB,   IPRINT, NDDLI, IADI,      JDII,
     3                  DIAG_I, LT_I,   ITOK,  INSOLV,    ITN,
     4                  FAC_K,  IPIV_K, NK,    MUMPS_PAR, CDDLP,
     5                  ISOL ,  IDS ,   IDDL,  IKC,       INLOC,
     6                  NDOF  , ITASK )
         END IF 
         IF (IMON>0) CALL STOPTIME(32,1)
citask0        END IF !(ITASK==0) THEN
Cas --imcompabilite avec MONVOL-----
        IF (NMONV>0)THEN
         IF (ILINE==0.OR.(ILINE>0.AND.ILINTF==0) )THEN
citask0         IF (ITASK==0) THEN
          IF (INTP_C>=0) THEN
           DO I=1,NDDLI
            J=ITOK(I)
            DIAG_K(J)=DIAG_K(J)+DIAG_I(I)
           ENDDO
          ENDIF 
citask0         END IF !(ITASK==0) THEN
C----------------------
c      CALL MY_BARRIER
C---------------------
!$OMP PARALLEL PRIVATE(ITSK)
          ITSK = OMP_GET_THREAD_NUM()
          CALL IMP_PCGH( IPREC ,
     1                    NDDL  ,NNZ   ,IADK  ,JDIK  ,DIAG_K ,   
     2                    LT_K  ,NDDLI ,ITOK  ,IADI  ,JDII   ,
     3                    LT_I  ,NNZM  ,IADK  ,JDIK  ,DIAG_K ,
     4                    LT_K  ,X     ,F     ,ITOL  ,TOL    ,
     5                    PCG_W1,PCG_W2,PCG_W3,ITSK  ,IPRINT ,
     6                    L_LIM ,P_MACH,F_U   ,ISTOP ,
     8                    W_DDL ,A     ,AR    ,VE    ,MS    ,
     9                    XE    ,D     ,DR    ,NDOF  ,IPARI ,
     A                    INTBUF_TAB   ,NUM_IMP,NS_IMP,NE_IMP,
     B                    NSREM ,NSL   ,NMONV ,IMONV ,MONVOL,
     C                    IGRSURF,VOLMON,FR_MV,IBFV  ,
     D                    SKEW  ,XFRAME ,GRAPHE,IAD_ELEM,FR_ELEM, 
     E                    INSOLV ,ITN   ,FAC_K ,IPIV_K,NK    ,
     F                    ITAB   ,MUMPS_PAR,CDDLP,ISOL,IDSC  ,
     G                    IDDL  ,IKC    ,INLOC ,IND_IMP,XI_C ,
     H                    F0    ,NDDLI_G,INTP_C,IRBE3  ,LRBE3,
     I                    IRBE2 ,LRBE2 )
!$OMP END PARALLEL 
         ENDIF 
        ENDIF 
       ENDIF 
C       
C----------------------
c      CALL MY_BARRIER
C---------------------
citask0      IF (ITASK == 0) THEN
       IF (IT_PCG<0) THEN
        IDSC = 1
        IT_PCG = -IT_PCG
       ELSE
        IDSC = 0
       ENDIF
citask0      END IF !(ITASK == 0) THEN
C----------------------
c      CALL MY_BARRIER
C---------------------
C      IF (IMON>0) CALL STOPTIME(33,1)
      RETURN
      END
Chd|====================================================================
Chd|  SET_KISC                      source/implicit/lin_solv.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SET_KISC(NDDL ,NDDLI,IADI ,JDII ,ITOK ,LT_I ,
     .                   IADK0,JDIK0,LT_K0)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL,NDDLI,IADI(*),JDII(*),IADK0(*),JDIK0(*),ITOK(*)
      my_real
     .  LT_I(*),LT_K0(*)
C---------[I] local sym. [K0] global complete
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J,K,JD,ICOL(NDDL),NRI,I0,KTOI(NDDL)
C----6------------------
      DO I = 1, NDDL
        ICOL(I) = 0
        KTOI(I) = 0
      ENDDO
      DO I = 1, NDDLI
        K = ITOK(I)
        ICOL(K) = IADI(I+1) - IADI(I)
        KTOI(K) = I
      ENDDO
      DO I = 1, NDDLI
        DO J = IADI(I),IADI(I+1)-1
         JD = ITOK(JDII(J))
         ICOL(JD) = ICOL(JD) + 1
        ENDDO
      ENDDO
      IADK0(1) = 1
      DO I = 1,NDDL
       IADK0(I+1) = IADK0(I)+ICOL(I)
       ICOL(I) = 0
      ENDDO
C-----------true with initial lower-triang  (default)   
      DO I0 = 1,NDDL
        I = KTOI(I0)
            IF (I==0) CYCLE
        DO J=IADI(I),IADI(I+1)-1
         JD = ITOK(JDII(J))
             K=IADK0(I0)+J-IADI(I)
         JDIK0(K) = JD
         LT_K0(K) = LT_I(J)
        ENDDO 
        ICOL(I0) =  IADI(I+1)-IADI(I)
        DO J=IADI(I),IADI(I+1)-1
         JD = ITOK(JDII(J))
         K = IADK0(JD) + ICOL(JD)
         JDIK0(K) = ITOK(I)
         LT_K0(K) = LT_I(J)
         ICOL(JD) = ICOL(JD) + 1
        ENDDO 
      ENDDO
C      
      RETURN
      END
Chd|====================================================================
Chd|  INI_KISC                      source/implicit/lin_solv.F    
Chd|-- called by -----------
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        IMP_WORKH                     share/modules/impbufdef_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INI_KISC(NDDL,NDDLI,IADI)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_WORKH
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL,NDDLI,IADI(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  IERR,LNZ
C----6------------------
        IF (NDDLI<=0) RETURN
C       
        IF(ALLOCATED(IADI0)) DEALLOCATE(IADI0)
        IF(ALLOCATED(JDII0)) DEALLOCATE(JDII0)
        IF(ALLOCATED(LT_I0)) DEALLOCATE(LT_I0)
        LNZ=IADI(NDDLI+1)-IADI(1)
C--- KI0 with global id is still not completely validated
C---    LNZ=2*(IADI(NDDLI+1)-IADI(1))
        ALLOCATE(IADI0(NDDLI+1),JDII0(LNZ),LT_I0(LNZ),STAT=IERR)
C       
        IF (IERR/=0) THEN
          CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .                C1='FOR IMPLICIT SOLVER W/ CONTACT')
          CALL ARRET(2)
        ENDIF
C      
      RETURN
      END
Chd|====================================================================
Chd|  MUMPSLB_HP                    source/implicit/lin_solv.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        IMP_SMPINI                    source/implicit/imp_solv.F    
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE MUMPSLB_HP(F     ,F1    , NDDL , IAD_ELEM , FR_ELEM,
     1                      IDDL  , IKC  , INLOC    ,NDOF    ,ITAG   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL, IAD_ELEM(2,*), FR_ELEM(*), 
     .        IDDL(*), IKC(*), INLOC(*), NDOF(*),ITAG(*)
      my_real
     .        F(*),F1(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NF, I, J, N, NKC, ND, ID
      INTEGER ITSK,F_NOD ,L_NOD,F_DDL ,L_DDL
C
!$OMP PARALLEL PRIVATE(ITSK,F_NOD ,L_NOD,F_DDL ,L_DDL,N,I,J,NKC,ND,ID)
        CALL IMP_SMPINI(ITSK   ,F_NOD ,L_NOD ,NUMNOD )
        CALL IMP_SMPINI(ITSK   ,F_DDL ,L_DDL ,NDDL   )
         DO I=F_DDL ,L_DDL
            F1(I)=F(I)
         ENDDO
        IF (NSPMD > 1 ) THEN
         DO N=F_NOD ,L_NOD
            I=INLOC(N)
            ITAG(I)=1
         ENDDO
C
         DO I=1,NSPMD
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
               N=FR_ELEM(J)
               IF ( ITAG(N) > 0 ) ITAG(N)=ITAG(N)+1
            ENDDO
         ENDDO
C----------------
         NKC=0
         DO N=F_NOD ,L_NOD
            I=INLOC(N)
            DO J=1,NDOF(I)
               ND=IDDL(I)+J
               ID=ND-NKC
               IF (IKC(ND)>0) THEN
                  NKC=NKC+1
               ELSE
                 IF ( ITAG(I) > 1 ) F1(ID)=F(ID)/ITAG(I)
               ENDIF
            ENDDO
         ENDDO
        END IF !(NSPMD > 1 ) THEN
!$OMP END PARALLEL 
C      
      RETURN
      END
Chd|====================================================================
Chd|  MUMPSLB                       source/implicit/lin_solv.F    
Chd|-- called by -----------
Chd|        LIN_SOLVP2                    source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE MUMPSLB(F     ,F1    , NDDL , IAD_ELEM , FR_ELEM,
     1                   IDDL  , IKC  , INLOC    ,NDOF    ,ITAG   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL, IAD_ELEM(2,*), FR_ELEM(*), 
     .        IDDL(*), IKC(*), INLOC(*), NDOF(*),ITAG(*)
      my_real
     .        F(*),F1(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NF, I, J, N, NKC, ND, ID
      INTEGER ITSK,F_NOD ,L_NOD,F_DDL ,L_DDL
C
         DO I=1 ,NDDL
            F1(I)=F(I)
         ENDDO
        IF (NSPMD > 1 ) THEN
         DO N=1,NUMNOD
c            I=INLOC(N)
            ITAG(N)=1
         ENDDO
C
         DO I=1,NSPMD
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
               N=FR_ELEM(J)
               ITAG(N)=ITAG(N)+1
            ENDDO
         ENDDO
C----------------
         NKC=0
         DO N=1,NUMNOD
            I=INLOC(N)
            DO J=1,NDOF(I)
               ND=IDDL(I)+J
               ID=ND-NKC
               IF (IKC(ND)>0) THEN
                  NKC=NKC+1
               ELSE
                 IF ( ITAG(I) > 1 ) F1(ID)=F(ID)/ITAG(I)
               ENDIF
            ENDDO
         ENDDO
        END IF !(NSPMD > 1 ) THEN
C      
      RETURN
      END
#endif
