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
Chd|  NL_SOLV                       source/implicit/nl_solv.F     
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        AL_CONSTRAINT1_HP             source/implicit/nl_solv.F     
Chd|        AL_CONSTRAINT2_HP             source/implicit/nl_solv.F     
Chd|        BFGS_0                        source/implicit/imp_bfgs.F    
Chd|        BFGS_LS                       source/implicit/imp_bfgs.F    
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|        CRIT_ITE                      source/implicit/nl_solv.F     
Chd|        FRAC_DD_HP                    source/implicit/integrator.F  
Chd|        FRAC_D_HP                     source/implicit/integrator.F  
Chd|        GET_MAX                       source/implicit/nl_solv.F     
Chd|        IMP_STOP                      source/implicit/imp_solv.F    
Chd|        INTEGRATOR2_HP                source/implicit/integrator.F  
Chd|        LINE_S                        source/implicit/nl_solv.F     
Chd|        LINE_S1                       source/implicit/nl_solv.F     
Chd|        LIN_SOLV                      source/implicit/lin_solv.F    
Chd|        PRODUT_HP                     source/implicit/produt_v.F    
Chd|        PRODUT_UHP                    source/implicit/produt_v.F    
Chd|        PRODUT_UHP2                   source/implicit/produt_v.F    
Chd|        PRODUT_VMHP                   source/implicit/produt_v.F    
Chd|        VAXPY_HP                      source/implicit/produt_v.F    
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE NL_SOLV(NDDL  ,IDDL  ,NDOF  ,IKC   ,D     ,
     1                   DR    ,NNZ   ,IADK  ,JDIK  ,DIAG_K,
     2                   LT_K  ,F     ,NDDLI ,IADI  ,JDII  ,
     3                   DIAG_I,LT_I  ,ITOK  ,IADM  ,JDIM  ,
     4                   DIAG_M,LT_M  ,R02   ,DD    ,DDR   ,
     5                   ITASK0,IT    ,ITC   ,RU0   ,ROLD  ,
     6                   IDIV ,INPRINT,ICPREC,ISTOP ,E02   ,
     7                   DE0  ,EIMP   ,INLOC ,NDDL0 ,LS    ,
     8                   U02  ,GAP    ,ITAB  ,FR_ELEM,IAD_ELEM,
     9                   W_DDL,A      ,AR    ,V     ,MS    ,
     A                   X    ,IPARI  ,INTBUF_TAB   ,NUM_IMP,
     B                   NS_IMP,NE_IMP,NSREM ,NSL   ,ICONT ,
     C                   GRAPHE,FAC_K, IPIV_K, NK   ,NMONV ,
     D                   IMONV ,MONVOL,IGRSURF,FR_MV ,
     E                   VOLMON,IBFV  ,SKEW  ,XFRAME,MUMPS_PAR,
     F                   CDDLP ,IND_IMP,NBINTC,INTLIST,NEWFRONT,
     G                   ISENDTO,IRECVFROM,IRBE3 ,LRBE3,NDIV ,
     H                   ICONT0 ,ISIGN ,FEXT  ,DG    ,DGR   ,
     I                   DG0   ,DGR0  ,RFEXT ,LS1   ,NODFT ,
     J                   NODLT ,IRBE2 ,LRBE2 ,IDIV0,
     K                   RELRES,anew_stif )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
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
#include      "impl1_c.inc"
#include      "impl2_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
#include      "dmumps_struc.h"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C----------resol [K]{X}={F}------ 
      INTEGER  NDDL  ,NNZ ,IADK(*),JDIK(*),IADM(*),JDIM(*),ITASK0,
     .         NDDLI  ,IADI(*),JDII(*),ITOK(*),ICPREC,INLOC(*),
     .         NDOF(*),IDDL(*),IKC(*),IDIV  ,INPRINT,ISTOP,
     .         IT,ITC,NDDL0,ITAB(*),FR_ELEM(*),IAD_ELEM(*),W_DDL(*)
      INTEGER  NE_IMP(*),NSREM ,NSL,ICONT,ISIGN,IMP1,
     .         IPARI(*) ,NUM_IMP(*),NS_IMP(*), IPIV_K(*), NK
      INTEGER NMONV,IMONV(*),MONVOL(*),FR_MV(*),
     .        IBFV(*),IND_IMP(*),IRBE3(*) ,LRBE3(*),NDIV ,ICONT0,
     .        IRBE2(*),LRBE2(*)
      INTEGER NEWFRONT(*),NBINTC,INTLIST(*),ISENDTO(*),IRECVFROM(*),
     .        NODFT ,NODLT,IDIV0
C     REAL
      my_real
     .  DIAG_K(*),LT_K(*),F(*),R02,DIAG_M(*),LT_M(*),
     .  DIAG_I(*),LT_I(*),D(3,*),DR(3,*),DD(3,*),DDR(3,*),
     .  RU0,ROLD,E02 ,EIMP,LSTOL,DE0,LS(*),U02,GAP,RBID
      my_real
     .  A(3,*),AR(3,*),V(3,*),X(3,*),MS(*),FAC_K(*),
     .  VOLMON(*),SKEW(*),XFRAME(*),FEXT(*),DG(3,*),DGR(3,*),
     .  DG0(3,*),DGR0(3,*),LS1(*)   ,RFEXT, RELRES(*)
C-----LS(1):R0,LS(2):S0,LS(3):S1 ; LS1(1): EN, LS1(2):SN;LS1(3):DE
      TYPE(PRGRAPH) :: GRAPHE(*)
      INTEGER CDDLP(*)
      TYPE(DMUMPS_STRUC) MUMPS_PAR
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
c
      character*1 anew_stif
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C----------resol [K]{X}={F}------
      INTEGER  I  ,J ,ND,IP,ITMP,ICOV,ICALU,IER,ISETK0,IRIKS,
     .         F_DDL ,L_DDL
C     REAL
      my_real
     .  R2,U2,DU2,TOL,RR,RU,TEMP,FR,RE,DE,R1,NL_TOL,R01,RR1,
     .  GAPN,LAMDA,FA,UC2,UM1,UM2,PMAX
C-----------------------------------------------
C------------------------------------------
c     insolv=1 =>  (elast) Newton modified
c     insolv=2,3 =>  BFGS
c     insolv=4,5 =>  elastoplastic
C------------------------------------------
C------------------------------------------
C------------------------------------------
C     [K]:matrice de rigidite [M]:matrice de preconditionnement
c        F_DDL=1+ITASK*NDDL/NTHREAD
c        L_DDL=(ITASK+1)*NDDL/NTHREAD
C
      TOL =L_TOL
      NL_TOL=N_TOL
C   
      LSTOL=LS_TOL
      IP=IABS(INPRINT)
      DE=0
      ND=3*NUMNOD
      RU=RU0
      ISETK0=ISETK+ICONT
      IF (NITOL==3.OR.NITOL==13.OR.NITOL==23
     .         .OR.NITOL==123) THEN
       ICALU=1
      ELSE
       ICALU=0
      ENDIF
      IRIKS=0
      IF (NCYCLE>1.AND.IDTC==3) THEN
       IF (ILAST==0) IRIKS=1
      ENDIF
C----------------------
c      CALL MY_BARRIER
C---------------------
      ISETK=0
      IF (ICONT>0) GAPN=GAP*ZEP9
      IF (IMCONV>=0) ICONT0=ICONT
C--------idtc=3 :modified Riks arc_length-----
      IF (IT==0) THEN
       R01=SQRT(R02)
C--------ROLD=R2(IT=0)---------------------------------------
Ctmp       IF (ISPRB==1) R01=MIN(R01,SQRT(ROLD)/R01)
       RE=ONE
C--------cas particulier---------------------------------------
       IF (R01*NL_TOL<=EM12.AND.NL_TOL>EM10.AND.IDIV/=10) THEN
       IF (IREFI/=5.OR.ICONT==0) THEN
        IF(INPRINT/=0)THEN
         WRITE(IOUT,*)
c         WRITE(IOUT,1008)IT,RE,R01,RE
         WRITE(IOUT,1108)IT,anew_stif,RE,R01,RE
         WRITE(IOUT,*)
         IF(INPRINT<0)THEN
          WRITE(ISTDO,*)
c          WRITE(ISTDO,1008)IT,RE,R01,RE
          WRITE(ISTDO,1108)IT,anew_stif,RE,R01,RE
          WRITE(ISTDO,*)
         ENDIF
        ENDIF
        RELRES(1) = RE
        RELRES(2) = R01
        RELRES(3) = RE
        IDIV = -2
        ISETK=1
        RETURN
       ELSE
        F(1)=MAX(EM20,F(1))
	    R01=MAX(R01,EM20)
       ENDIF
       ENDIF
       IF (INSOLV==4) ICOV = IMCONV
       IMCONV=0
       RR=ROLD
       NDIV=0
C---------
       IF (INSOLV==2.OR.INSOLV==3) CALL BFGS_0
C---------
       IF (NCYCLE==1) ISIGN = 1
       IF (IRIKS>0) THEN
          CALL LIN_SOLV(NDDL  ,IDDL  ,NDOF  ,IKC   ,DG    ,
     1               DGR   ,TOL   ,NNZ   ,IADK  ,JDIK  ,
     2               DIAG_K,LT_K  ,NDDLI ,IADI  ,JDII  ,
     3               DIAG_I,LT_I  ,ITOK  ,IADM  ,JDIM  ,
     4               DIAG_M,LT_M  ,FEXT  ,DE    ,INLOC ,
     5               FR_ELEM,IAD_ELEM,W_DDL,ITASK0,ICPREC,
     6               ISTOP ,A     ,AR     ,V    ,
     7               MS    ,X     ,IPARI ,INTBUF_TAB ,
     8               NUM_IMP,NS_IMP,NE_IMP,NSREM ,NSL  ,
     9               ITC    ,GRAPHE, ITAB, FAC_K, IPIV_K,
     A               NK     ,NMONV,IMONV  ,MONVOL,IGRSURF,
     B               FR_MV ,VOLMON,IBFV  ,SKEW  ,
     C               XFRAME ,MUMPS_PAR,CDDLP,IND_IMP,RBID,
     E               IRBE3 ,LRBE3 ,IRBE2  ,LRBE2 )

C---------
         CALL PRODUT_UHP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                  DG    ,DGR   ,U2    ,W_DDL  )
C-------distinquer converge or diverge---
citask0         IF (ITASK == 0) THEN
	      IF (IDIV/=-2) THEN
           CALL CP_REAL(ND,DD,DG0)
           IF (IRODDL/=0) CALL CP_REAL(ND,DDR,DGR0)
	      ELSE
           DLA_RIKS = ISIGN*DT2
	      ENDIF
citask0         END IF !(ITASK == 0) THEN
          CALL PRODUT_UHP2(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                    DG    ,DGR   ,DG0   ,DGR0  ,UC2   ,
     .                    W_DDL )
C----------------------
c      CALL MY_BARRIER
C---------------------
citask0         IF (ITASK == 0) THEN
          CALL GET_MAX(NUMNOD,DG0,TEMP,I)
	      UM1=TEMP
          CALL GET_MAX(NUMNOD,DGR0,TEMP,I)
	      UM1=MAX(UM1,TEMP,EM20)
          CALL GET_MAX(NUMNOD,DG,TEMP,I)
	      UM2=TEMP
          CALL GET_MAX(NUMNOD,DGR,TEMP,I)
	      UM2=MAX(UM2,TEMP,EM20)
	      UC2=UC2/UM1/UM2
C          LAMDA = ALEN/SQRT(U2)
	     IF ((UC2+DLA_RIKS/RFEXT)<ZERO) THEN
	      ISIGN = -1
	     ELSE
	      ISIGN = 1
	     ENDIF
c           print *,'UC2,DLA_RIKS,ISIGN=',UC2,DLA_RIKS,ISIGN
          IF (IMPDEB>0) THEN
           IF (NCYCLE>=NDEB0.AND.NCYCLE<=NDEB1)
     .      WRITE(IOUT,1025)UC2,DLA_RIKS,ISIGN
	      ENDIF
	     IF (ISIGN<0) THEN
	      DLA_RIKS = 2*ISIGN*DT2
	     ELSE
	      DLA_RIKS = ZERO
	     ENDIF
C---------
citask0         END IF !(ITASK == 0) THEN

C----------------------
c      CALL MY_BARRIER
C---------------------
         LAMDA = ISIGN*DT2
         IF (IDIV/=-2.AND.ICONT==0)THEN
          CALL PRODUT_VMHP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                     DD    ,DDR   ,F     ,DE    ,W_DDL )
          CALL INTEGRATOR2_HP(IDDL,NDOF,IKC,D,DR,DD,DDR)
         ELSE
          CALL FRAC_DD_HP(IDDL,NDOF,IKC,D,DR,DG,DGR,LAMDA)
          CALL PRODUT_VMHP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                     D     ,DR    ,F     ,DE    ,W_DDL )
         ENDIF
       ELSE

        IF ((N_LIM /= 1.OR.ISPRB /= 0).AND.
     .      (NCYCLE>1.AND.IDIV/=-2.AND.ICONT==0.AND.IKT==0
     .     .OR.(NCYCLE==1.AND.IDIV==10)))THEN
	     IF (IDTC==3.AND.ILAST==1) THEN
          LAMDA = DLA_RIKS
	      DLA_RIKS = ZERO
          CALL FRAC_D_HP(IDDL,NDOF,IKC,DD,DDR,LAMDA)
	     ENDIF
         CALL PRODUT_VMHP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                   DD    ,DDR   ,F     ,DE    ,W_DDL )
        ELSE
         IF (INSOLV==4.AND.ICOV>=0) IMCONV=-1

         CALL LIN_SOLV(NDDL  ,IDDL  ,NDOF  ,IKC   ,DD    ,
     1               DDR   ,TOL   ,NNZ   ,IADK  ,JDIK  ,
     2               DIAG_K,LT_K  ,NDDLI ,IADI  ,JDII  ,
     3               DIAG_I,LT_I  ,ITOK  ,IADM  ,JDIM  ,
     4               DIAG_M,LT_M  ,F     ,DE    ,INLOC ,
     5               FR_ELEM,IAD_ELEM,W_DDL,ITASK0,ICPREC,
     6               ISTOP ,A     ,AR     ,V    ,
     7               MS    ,X     ,IPARI ,INTBUF_TAB ,
     8               NUM_IMP,NS_IMP,NE_IMP,NSREM ,NSL  ,
     9               ITC    ,GRAPHE, ITAB, FAC_K, IPIV_K,
     A               NK     ,NMONV,IMONV  ,MONVOL,IGRSURF,
     B               FR_MV ,VOLMON,IBFV  ,SKEW  ,
     C               XFRAME ,MUMPS_PAR,CDDLP,IND_IMP,RBID,
     E               IRBE3 ,LRBE3 ,IRBE2  ,LRBE2 )
         IF (DE<ZERO.AND.IREFI>1.AND.IREFI<5) IMCONV=-2
         IF (ICONT>0.AND.IDSGAP>0) THEN
         CALL GET_MAX(NUMNOD,DD,PMAX,I)
C----------------------
c      CALL MY_BARRIER
C---------------------
          IF (PMAX>GAPN) THEN
           TEMP=GAPN/PMAX
           CALL FRAC_D_HP(IDDL,NDOF,IKC,DD,DDR,TEMP)
	       DE = DE * TEMP
          ENDIF
         ENDIF
        ENDIF
C	
        CALL INTEGRATOR2_HP(IDDL,NDOF,IKC,D,DR,DD,DDR)
       ENDIF !IF (IRIKS>0)
C----------------------
c      CALL MY_BARRIER
C---------------------
citask0       IF (ITASK == 0) THEN
       IF (IMCONV>=0) THEN
        CALL PRODUT_UHP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                  D    ,DR    ,U2    ,W_DDL )
        EIMP=EIMP+DE
        IF (DE>EP10.AND.ILINE==0.AND.IDYNA==0
     .     .AND.NCYCLE==1) THEN
         IF(ISPMD==0)THEN
          WRITE(IOUT,1030)EIMP
          WRITE(ISTDO,1030)EIMP
         ENDIF
	     CALL IMP_STOP(-1)
        ENDIF
C--------pour idtc>=2-----
        U02=U2
        IF(INPRINT/=0)THEN
         RU=SQRT(U2)
c         WRITE(IOUT,*)
c         WRITE(IOUT,1002)IT,RU,R01,EIMP
         WRITE(IOUT,1102)
         WRITE(IOUT,1003)IT,anew_stif,RU,R01,EIMP
          IF(INPRINT<0)THEN
c          WRITE(ISTDO,*)
c          WRITE(ISTDO,1002)IT,RU,R01,EIMP
          WRITE(ISTDO,1102)
          WRITE(ISTDO,1003)IT,anew_stif,RU,R01,EIMP
         ENDIF
        ENDIF

        RELRES(1) = RU
        RELRES(2) = R01
        RELRES(3) = EIMP
       ELSE
          WRITE(IOUT,*)
          WRITE(IOUT,1013) DE
          IF(INPRINT<0)THEN
           WRITE(ISTDO,*)
           WRITE(ISTDO,1013)DE
          ENDIF
C
       ENDIF !IF (IMCONV>=0)
citask0       END IF !(ITASK == 0) THEN
C---------IT>0---------
      ELSE
C--------R=Fext-Fint correction due to Fext---------
       IF (IRIKS>0.AND.DLA_RIKS/=ZERO) THEN
        CALL VAXPY_HP(NDDL, F   ,FEXT,DLA_RIKS)
       ENDIF
       CALL PRODUT_HP(NDDL,F,F,W_DDL,R2)
C----------------------
c      CALL MY_BARRIER
C---------------------
      IF (R2>=ZERO.AND.R2<EP30) THEN
       RR=SQRT(R2/R02)
       IF (IT==1.AND.ISPRB==1.AND.IDIV/=-2) THEN
        RR1=SQRT(R2/ROLD)
C
        IF (ICONT>0.AND.RR<ONE) RR1=RR
       ELSE
        RR1=RR
       ENDIF
       RE=DE0/EIMP
       IF (IT==1) THEN
        RU=ONE
       ELSEIF (ICALU>0) THEN
         CALL PRODUT_UHP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                   D     ,DR    ,U2    ,W_DDL )
         CALL PRODUT_UHP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                   DD    ,DDR   ,DU2   ,W_DDL )
C----------------------
c      CALL MY_BARRIER
C---------------------
         RU=LS(3)*SQRT(DU2/U2)
         IF (INSOLV==5) RU=SQRT(DU2/U2)
       ENDIF
       CALL CRIT_ITE(IT,RU,RR1,RE,NDIV,NL_TOL)
      ELSE
       IMCONV =-2
       RR1 =EP30
      ENDIF
C----------------------
c      CALL MY_BARRIER
C---------------------
       IF (IMCONV==-3) THEN
        IF (NCYCLE==1.OR.IDIV==-2) IMCONV=-2
        R02=R2
       ENDIF
C----------------------
c      CALL MY_BARRIER
C---------------------
       IF (IMCONV==0) THEN
         IDIV=0
C-------initie line-serach------------
        IF (ILINE_S/=1) THEN
         LS(1)=ONE
         LS(2)=ZERO
        ENDIF
         LS(3)=ONE
         IF (NDIVER>0) R02=R2
       ENDIF
       IF(IMCONV==1) THEN
        ISTOP=0
        IDIV=0
C------For time correction-with Riks----	 
         IF (IT>1.AND.ICALU==0) THEN
          CALL PRODUT_UHP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                    D     ,DR    ,U2    ,W_DDL )
          CALL PRODUT_UHP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                    DD    ,DDR   ,DU2   ,W_DDL )
C----------------------
c      CALL MY_BARRIER
C---------------------
          RU=LS(3)*SQRT(DU2/U2)
          U02=U2
         ELSEIF (IDTC>=2) THEN
          CALL PRODUT_UHP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                    D     ,DR    ,U02   ,W_DDL )
C----------------------
c      CALL MY_BARRIER
C---------------------
         ENDIF
C
        IF(INPRINT/=0)THEN
c         WRITE(IOUT,*)
c         WRITE(IOUT,1008)IT,RU,RR,RE
         WRITE(IOUT,1108)IT,anew_stif,RU,RR,RE
         WRITE(IOUT,*)
         IF(INPRINT<0)THEN
c          WRITE(ISTDO,*)
c          WRITE(ISTDO,1008)IT,RU,RR,RE
          WRITE(ISTDO,1108)IT,anew_stif,RU,RR,RE
          WRITE(ISTDO,*)
         ENDIF
        ENDIF
        RELRES(1) = RU
        RELRES(2) = RR
        RELRES(3) = RE
       ELSEIF(IMCONV<=-2) THEN
C--------diverge----
C        write(iout,*)'diverge',IMCONV,IT ,IDIV  
        IF(INPRINT/=0)THEN
         WRITE(IOUT,*)
         IF (RR1>ONE) THEN
          WRITE(IOUT,1011)RR1
         ELSEIF(IT>NL_DTN) THEN
          WRITE(IOUT,1010)
         ELSEIF(IDTC==3) THEN
          WRITE(IOUT,1020)
         ENDIF
         WRITE(IOUT,*)
         IF(INPRINT<0)THEN
          WRITE(ISTDO,*)
          IF (RR1>ONE) THEN
           WRITE(ISTDO,1011)RR1
          ELSEIF(IT>NL_DTN) THEN
           WRITE(ISTDO,1010)
          ELSEIF(IDTC==3) THEN
           WRITE(ISTDO,1020)
          ENDIF
          WRITE(ISTDO,*)
         ENDIF
         IF(IMCONV==-2) THEN
          WRITE(IOUT,1012)
          IF(INPRINT<0)WRITE(ISTDO,1012)
         ENDIF
        ENDIF
        RELRES(1) = RU
        RELRES(2) = RR1
        RELRES(3) = RE
       ELSE
C--------CAS IMCONV=0,-1---add line-search for constant dt-
c        IF (IDTC>0) THEN
        IF (ILINE_S>0.AND.ISIGN>=0.AND.IRWALL==0) THEN
         FR=LS(3)
         IF (NITOL/=2.AND.NITOL/=4.OR.ILINE_S==1) THEN
C----------DD,DDR are modified in some cases         
          IF (LS1(3)/=ZERO ) THEN
           DE = LS1(3)
          ELSE
          CALL PRODUT_VMHP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                     DD    ,DDR   ,F     ,DE    ,W_DDL  )
          END IF
C----------------------
c      CALL MY_BARRIER
C---------------------
          R1=DE/DE0
          IF (ILINE_S==3) THEN
           R1=ABS(DE/DE0)
           IF (RR>ONE) R1=ONE
           IF (RR>ONE.AND.IREFI>=2) R1=LS(1)
           TEMP=EP02
          ENDIF
         ELSE
          R1=RR/ROLD
          TEMP=EP03
         ENDIF
         ICOV = IMCONV
C------------
citask0        IF (ITASK == 0) THEN
C------------
         IF (ILINE_S==3) THEN
          CALL LINE_S(LS(1),LS(2),R1,FR,LSTOL,IDIV,ICONT,TEMP)
         ELSEIF (ILINE_S==1) THEN
          CALL LINE_S1(R1,FR,LS(1),LS(2),LS1(1),LS1(2),IDIV,LSTOL,
     .                 ICONT,ICONT0,IRIKS)
         ELSEIF (ILINE_S==2) THEN
          CALL LINE_S(LS(1),LS(2),R1,FR,LSTOL,IDIV,ICONT,TEMP)
         ENDIF
         IF (IMPDEB>0.AND.ISPMD==0) THEN
          IF (NCYCLE>=NDEB0.AND.NCYCLE<=NDEB1) then
           WRITE(IOUT,*)'R1,FR,IDIV=',R1,FR,IDIV
C           WRITE(IOUT,*)'DE,DE0=',DE,DE0
          end if
         ENDIF
C------------
citask0        END IF !(ITASK == 0) THEN
C----------------------
c       CALL MY_BARRIER
C---------------------
C
         IF ((INSOLV==2.OR.INSOLV==3).AND.FR>ONE.AND.IKT==0)
     .       FR = MIN(FR,TWO)
        ENDIF
C        IF (IABS(INSOLV)==5) THEN
C----------suprimed----------
C        ELSE  
        IF (IMCONV==0) THEN
C----------schema normale---------
         IF (IT>1) THEN
C------------
          CALL PRODUT_UHP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                    D    ,DR    ,U2    ,W_DDL  )
          CALL PRODUT_UHP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                    DD    ,DDR   ,DU2   ,W_DDL  )
C----------------------
c      CALL MY_BARRIER
C---------------------
          RU=LS(3)*SQRT(DU2/U2)
         ENDIF
C------------
citask0        IF (ITASK == 0) THEN
C------------
         IF (INSOLV==2.OR.INSOLV==3) CALL BFGS_LS(LS(3))
         IF (RR>=ROLD) THEN
          NDIV = NDIV +1
         ELSE
          NDIV = 0
         ENDIF
         IF (INPRINT/=0)THEN
          IF(MOD(IT,IP)==0)THEN
c           WRITE(IOUT,*)
           WRITE(IOUT,1003)IT,anew_stif,RU,RR,RE
           IF(INPRINT<0)THEN
c            WRITE(ISTDO,*)
            WRITE(ISTDO,1003)IT,anew_stif,RU,RR,RE
           ENDIF
          ENDIF
         ENDIF
         RELRES(1) = RU
         RELRES(2) = RR
         RELRES(3) = RE
         IF(ITC>=N_LIM.AND.ISMDISP==0) THEN
c          IF(INPRINT/=0)THEN
c            WRITE(IOUT,*)
c            WRITE(IOUT,1006)ITC
c            IF(INPRINT<0)THEN
c             WRITE(ISTDO,*)
c             WRITE(ISTDO,1006)ITC
c            ENDIF
c          ENDIF
          ITC=0
         ENDIF
C------------
citask0        END IF !(ITASK == 0) THEN
C----------------------
c      CALL MY_BARRIER
C---------------------
         IF (IRIKS>0) THEN
          IF (ISETK0==1) THEN
           CALL LIN_SOLV(NDDL  ,IDDL  ,NDOF  ,IKC   ,DG    ,
     1               DGR   ,TOL   ,NNZ   ,IADK  ,JDIK  ,
     2               DIAG_K,LT_K  ,NDDLI ,IADI  ,JDII  ,
     3               DIAG_I,LT_I  ,ITOK  ,IADM  ,JDIM  ,
     4               DIAG_M,LT_M  ,FEXT  ,DE    ,INLOC ,
     5               FR_ELEM,IAD_ELEM,W_DDL,ITASK0,ICPREC,
     6               ISTOP ,A     ,AR     ,V    ,
     7               MS    ,X     ,IPARI ,INTBUF_TAB ,
     8               NUM_IMP,NS_IMP,NE_IMP,NSREM ,NSL  ,
     9               ITC    ,GRAPHE, ITAB, FAC_K, IPIV_K,
     A               NK     ,NMONV,IMONV  ,MONVOL,IGRSURF,
     B               FR_MV ,VOLMON,IBFV  ,SKEW  ,
     C               XFRAME ,MUMPS_PAR,CDDLP,IND_IMP,RBID,
     E               IRBE3 ,LRBE3 ,IRBE2  ,LRBE2 )
	      ICPREC = 0
	      IDSC = 0
          END IF
          CALL PRODUT_VMHP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                     DG    ,DGR   ,F     ,UM1   ,W_DDL )
         END IF
C
        CALL LIN_SOLV(NDDL  ,IDDL  ,NDOF  ,IKC   ,DD    ,
     1                 DDR   ,TOL   ,NNZ   ,IADK  ,JDIK  ,
     2                 DIAG_K,LT_K  ,NDDLI ,IADI  ,JDII  ,
     3                 DIAG_I,LT_I  ,ITOK  ,IADM  ,JDIM  ,
     4                 DIAG_M,LT_M  ,F     ,DE    ,INLOC ,
     5                 FR_ELEM,IAD_ELEM,W_DDL,ITASK0,ICPREC,
     6                 ISTOP ,A     ,AR     ,V    ,
     7                 MS    ,X     ,IPARI ,INTBUF_TAB ,
     8                 NUM_IMP,NS_IMP,NE_IMP,NSREM ,NSL  ,
     9                 ITC    ,GRAPHE, ITAB, FAC_K, IPIV_K,
     A                 NK     ,NMONV,IMONV  ,MONVOL,IGRSURF,
     B                 FR_MV ,VOLMON,IBFV  ,SKEW  ,
     C                 XFRAME ,MUMPS_PAR,CDDLP,IND_IMP,RBID,
     D                 IRBE3 ,LRBE3 ,IRBE2  ,LRBE2 )
         IF (ICONT>0.AND.IDSGAP>0) THEN
          CALL GET_MAX(NUMNOD,DD,PMAX,I)
C----------------------
c      CALL MY_BARRIER
C---------------------
          IF (PMAX>GAPN) THEN
           TEMP=GAPN/PMAX
           CALL FRAC_D_HP(IDDL,NDOF,IKC,DD,DDR,TEMP)
	       DE = DE * TEMP
          ENDIF
         ENDIF
C----------------------
c      CALL MY_BARRIER
C---------------------
C---------Riks type arc length------	 
         IF (IRIKS>0) THEN
	      LAMDA = DLA_RIKS
	      IF (IAL_M==1) THEN
            CALL AL_CONSTRAINT1_HP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                          DD    ,DDR   ,DG    ,DGR   ,D     ,
     .                          DR    ,W_DDL ,ALEN  ,LAMDA ,SCAL_RIKS,
     .                          IER   )
C----------------------
c      CALL MY_BARRIER
C---------------------
          IF (IER==1) THEN
	       IMCONV=-2
            IF(INPRINT/=0)THEN
             WRITE(IOUT,*)
             WRITE(IOUT,1020)
             WRITE(IOUT,*)
             IF(INPRINT<0)THEN
              WRITE(ISTDO,*)
              WRITE(ISTDO,1020)
              WRITE(ISTDO,*)
             ENDIF
            ENDIF
           END IF !(IER==1) THEN
          ELSEIF (IAL_M==2) THEN
            CALL AL_CONSTRAINT2_HP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                          DD    ,DDR   ,DG    ,DGR   ,D     ,
     .                          DR    ,W_DDL ,ALEN  ,LAMDA ,SCAL_RIKS)
C----------------------
c      CALL MY_BARRIER
C---------------------
	  END IF
	  IF (IMCONV>=0) THEN
           CALL FRAC_DD_HP(IDDL,NDOF,IKC,D ,DR ,DG,DGR,LAMDA)
citask0           IF (ITASK == 0) THEN
	        DLA_RIKS = DLA_RIKS + LAMDA
            EIMP=EIMP+DLA_RIKS*UM1
citask0           END IF !(ITASK == 0) THEN
C----------------------
c      CALL MY_BARRIER
C---------------------
          ENDIF
         END IF !IF (IRIKS>0) THEN
C	 
         CALL INTEGRATOR2_HP(IDDL,NDOF,IKC,D,DR,DD,DDR)
         EIMP=EIMP+DE
c	 if (de<0) print *,'****negative de=',de
C
        ELSEIF(IMCONV==-1) THEN
C----------line-search---------
         TEMP=-LS(3)+FR
        CALL FRAC_DD_HP(IDDL,NDOF,IKC,D,DR,DD,DDR,TEMP)
C----------------------
c      CALL MY_BARRIER
C---------------------
        LS(3) = FR
        END IF !IF (IMCONV==0) THEN
C      
       END IF !IF(IMCONV==1) THEN
C-----fin if IT=0
      ENDIF
      IF (IMCONV>=0) THEN
       RU0=RU
       DE0=DE
       IF (ABS(DE0)<EM20) DE0=EM20
       ROLD=MAX(EM20,RR)
      ENDIF
C-----REFORMING [K]----      
      IF (ISMDISP>0) THEN
       IF (ITC>=N_LIM.AND.IMCONV>=0) THEN
         ITC = 0
         ISETK=1
       END IF
       IF (ITC==0.AND.IMCONV>=0) THEN
        IF (NCYCLE==1.OR.N_LIM==1) ISETK=1
        IF (ISOLV==5.OR.ISOLV==6) ISETK=1
       END IF
       IF (IRWALL >0 .AND.IMCONV == 1) ISETK=1
       IF ((ITC==0.OR.IMCONV==1).AND.IKT>0) ISETK=1
      ELSE
C----condition IMCONV==1 reforming [K] for nothing (no resolution)-       
       IF (ITC==0.OR.IMCONV==1) ISETK=1
      END IF
      IF ((IDTC==3.OR.IKT>0).AND.IMCONV<=-2) ISETK=1
C-----if still diverge, try w/o [K] reforming      
      IF (IMCONV<=-2.AND.RR1>EP20.AND.IDIV0>=0) ISETK=1
      IF ((ISTOP==1.OR.ISTOP==2).AND.(ISOLV==5.OR.ISOLV==6))THEN
        ISTOP = 0
        ISETK = 1
        IMCONV=-2
      ENDIF
      IF (IMCONV<=-2.AND.ISPRB>0.AND.RR1>EP04) ISETK=1
! sb
      IF(IMCONV<= -2.AND.N_LIM == 1 .AND. ISPRB == 0)  ISETK = 1

C----------------------
c      CALL MY_BARRIER
C---------------------
Ctmp      IF (ITC==0.OR.IMCONV==1.OR.IMCONV<=-2) ISETK=1
c 1002 FORMAT(5X,'NL_ITERATION=',I5,1X,'  INITIAL RESIDUAL NORM=',3E11.4)
 1102 FORMAT(3X,78('-')/
     .       12X,'Stif. Mat.'/
     .       6x,'Iter',2X, ' reformed ',5X,'|du|/|u|',3X,'|r|/|r0|',3X,'|dE|/|E|',1X,'Conv.stat.'/
     .       3X,78('-'))
c 1003 FORMAT(5X,'NL_ITERATION=',I5,1X,' RELATIVE RESIDUAL NORM=',3E11.4)
 1003 FORMAT(5X,I5,6X,A1,5X,3(1X,1pE10.3))
 1005 FORMAT(3X,'TOLERANCE FOR LINEAR ITERATIVE SOLVER :',2X,E11.4)
 1006 FORMAT(3X,'--STIFFNESS MATRIX IS RESET AFTER ',I4,
     .          ' ITERATIONS--')
 1007 FORMAT(3X,'--ITERATION DIVERGE with R=',E11.4,2X,
     .       'STIFFNESS MATRIX WILL BE RESET')
c 1008 FORMAT(3X,'* CONVERGED WITH ',I4,1X,'ITERATIONS,',
c     .          ' |du|/|u|,|r|/|r0|,|dE|/|E|=',3E11.4)
 1108 FORMAT(5X,I5,6X,A1,5X,3(1X,1pE10.3),5X, 'C')
 1009 FORMAT(3X,'--ITERATION DIVERGE with RELATIVE R=',E11.4/,
     .       3X,'RESET ITERATION WITH STABLIZATION BY SCALING= ',E11.4)
 1010 FORMAT(3X,'--ITERATION DIVERGE with MAX_ITER REACHED--')
 1011 FORMAT(3X,'ITERATION DIVERGE with RELATIVE R=',E11.4)
 1012 FORMAT(3X,'--RESET ITERATION WITH NEW TIMESTEP--')
 1013 FORMAT(3X,'ITERATION DIVERGE with NEGATIVE ENERGY DE=',E11.4)
 1020 FORMAT(3X,'--ITERATION DIVERGE with RIKS Method--')
 1025 FORMAT(3X,'UC2,DLA_RIKS,ISIGN=',2E11.4,I4)
 1030 FORMAT(3X,'CHECK CONSTRAINT CONDITIONS, TOO LARGE ENERGY VALUE=',
     .           2X,E11.4)
      RETURN
      END

#endif
Chd|====================================================================
Chd|  CRIT_ITE                      source/implicit/nl_solv.F     
Chd|-- called by -----------
Chd|        NL_SOLV                       source/implicit/nl_solv.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CRIT_ITE(IT,UR,RR,ER,NDIV,TOL)
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
#include      "impl2_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IT,NDIV
C     REAL
      my_real
     .  UR,RR,ER,TOL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .  ERR,TOLR,TOLD
C--------cas particulier---------------------------------------
C      TOLD=EP04
C      IF (ILINE_S==1) TOLD=EP03 
C      IF (ISMDISP==1) TOLD=EP10 
      TOLD = TOL_DIV
      TOLR = ONE+TOL
C--------1:E,2:r,3:E+U,4:r+U,5,r+E,6,r+E+U-----
C--new:  1:E,2:r,3:U,12:E+r,13,E+U,23,r+U,123,r+E+U
      IF (NITOL==1) THEN
       ERR=ABS(ER)/TOL
       IF (IT==1.OR.RR>=ONE) ERR=ERR+TOLR
      ELSEIF (NITOL==2) THEN
       ERR=RR/TOL
      ELSEIF (NITOL==3) THEN
       ERR=UR/TOL
C       ERR=MAX(SQRT(ABS(ER)),UR)
       IF (RR>=ONE) ERR=ERR+TOLR
      ELSEIF (NITOL==12) THEN
       ERR=MAX(RR/N_TOLF,ABS(ER)/N_TOLE)
c       ERR=MAX(RR,UR)
      ELSEIF (NITOL==13) THEN
       ERR=MAX(UR/N_TOLU,ABS(ER)/N_TOLE)
c       ERR=MAX(SQRT(ABS(ER)),RR)
      ELSEIF (NITOL==23) THEN
       ERR=MAX(UR/N_TOLU,RR/N_TOLF)
c       ERR=MAX(SQRT(ABS(ER)),RR,UR)
      ELSEIF (NITOL==123) THEN
       ERR=MAX(UR/N_TOLU,RR/N_TOLF,ABS(ER)/N_TOLE)
      ENDIF
C
       IF (ERR<=ONE) THEN
C       IF (ERR<=TOL) THEN
        IMCONV=1
!sb       ELSEIF (UR<MIN(EM05,TOL).AND.RR<EM01) THEN
       ELSEIF ((N_LIM /= 1.OR.ISPRB /= 0).AND.UR<MIN(EM05,TOL).AND.RR<EM01) THEN
       IMCONV=1
       ELSEIF (IMCONV<0) THEN
C--------line-serach-----
       ELSE
        IMCONV=0
C------divergence-----------
!sb
       IF(N_LIM /= 1 .OR. ISPRB /= 0) THEN
        IF (RR>TOLR) THEN
         IF (IT==1) THEN
          IMCONV=-3
          IF (NDIV<NDIVER) IMCONV=0
         ELSE
          IMCONV=-2
          IF ((NDIV<NDIVER.OR.ILAST==1).AND.RR<TOLD) IMCONV=0
         ENDIF
        ENDIF
       ENDIF

       IF (IDTC>0.AND.IT>NL_DTN) IMCONV=-2
       ENDIF
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  LINE_S                        source/implicit/nl_solv.F     
Chd|-- called by -----------
Chd|        NL_SOLV                       source/implicit/nl_solv.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LINE_S(R0,S0,R1,S,DTOL,IDIV,IINT,PREC)
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
      INTEGER IDIV,IINT
C     REAL
      my_real
     .  R0,R1,S0,S,DTOL,PREC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .  S1,DR,R
C-----------------------------------------------
C      R1=ABS(E1/E0)
C      IF (R1<=ONE) THEN
      DR=ABS(R0-R1)
      R=MIN(R1/PREC,DR)
c      R=MIN(R1,DR*PREC)
      IF ((R<DTOL.OR.IDIV<-NLS_LIM)
     .    .OR.(R1>ONE.AND.IINT>0.AND.IREFI<2)) THEN
       IF (IMCONV==-1)IMCONV=0
c       IDIV=0
       IDIV=IDIV-1
       RETURN
      ELSEIF (R1>R0.AND.IMCONV==-1) THEN
       IDIV=-NLS_LIM-2
       S=S0
       RETURN
      ENDIF
      IMCONV=-1
      S1=S
      S=ABS(S0*R1-S1*R0)/DR
      IF (R1>ONE) THEN
C----------diverge---------------------------
       IF (S>ONE) S=HALF*S1/R1
       S=MAX(EM01,S)
C       IDIV=IDIV+1
      ELSE
C------extrapolation-------
       IF (S>MAX(S1,S0).OR.S<MIN(S1,S0)) THEN
        S=MIN(THREE_HALF*S1,S)
        S=MIN(FIVE,S)
       ENDIF
      ENDIF
      S0=S1
      R0=R1
      IDIV=IDIV-1
C
      RETURN
      END
Chd|====================================================================
Chd|  GET_MAX                       source/implicit/nl_solv.F     
Chd|-- called by -----------
Chd|        NL_SOLV                       source/implicit/nl_solv.F     
Chd|-- calls ---------------
Chd|        SPMD_MAX_S                    source/mpi/implicit/imp_spmd.F
Chd|====================================================================
      SUBROUTINE GET_MAX(N,D,VMAX,I)
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
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N,I
C     REAL
      my_real
     .  D(3,*),VMAX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K
      my_real
     .  T
C-----------------------------------------------
       VMAX=ZERO
       I=1
       DO K=1,N
        T=MAX(ABS(D(1,K)),ABS(D(2,K)),ABS(D(3,K)))
        IF (T>VMAX) THEN
         VMAX = T
         I=K
        ENDIF
       ENDDO
       IF (IMACH==3.AND.NSPMD>1)
     . CALL SPMD_MAX_S(VMAX)
C
      RETURN
      END
Chd|====================================================================
Chd|  LINE_S1                       source/implicit/nl_solv.F     
Chd|-- called by -----------
Chd|        NL_SOLV                       source/implicit/nl_solv.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LINE_S1(E1,S,EP,SP,EN,SN,IDIV,DTOL,ICONT,ICONT0,IRIKS)
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
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDIV,ICONT,ICONT0,IRIKS
C     REAL
      my_real
     .  E1,S,EP,SP,EN,SN,DTOL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IM
      my_real
     .  SOLD,STMP,S2,TOL,EOLD
C-------min e=r.du-----------------E1-> already relative /E0-----------------------
C------find sl so that el<0 
       IMCONV =-1
       EOLD=ZERO
       IM = MAX(ICONT,ICONT0)
       IF (E1>ZERO) THEN
        IF (IDIV==0) THEN
         IDIV = IDIV + 1
         SP = ONE
         EP = E1
         S = S + ONE
         IF (IRIKS>0.OR.IM > 0) THEN
          S = ONE
          IMCONV =0
         END IF
         RETURN
        ELSEIF (IDIV>0) THEN
         SOLD = S
         IF (ABS(E1-EP)<DTOL.OR.E1>EP.OR.IDIV>NLS_LIM) THEN	
          IF (E1>EP) S = S - ONE
          IMCONV =0
         ELSE
          S = S + ONE
         ENDIF
         IDIV = IDIV + 1
         SP = SOLD
         EP = E1
         RETURN
        ENDIF
        SP = S
        EP = E1
       ELSE
        IF (IDIV==0) THEN	
         SP = ZERO
         EP = ONE
         S = HALF
         IDIV = IDIV - 1
         SN = ONE
         EN = E1
         RETURN
        ELSEIF (SP==ZERO) THEN
          IF (ABS(E1-EN)<DTOL.OR.IDIV<-NLS_LIM) THEN	
           IMCONV =0
          ELSEIF (IRIKS>0.OR.ISMDISP>0) THEN
           IMCONV =0
          ELSE
c          SN = S
c	  EN = E1
           S = S*HALF
          ENDIF
          IDIV = IDIV - 1
          RETURN
        ELSEIF (IDIV>0) THEN
         IDIV = -IDIV
        ENDIF
        EOLD=EN
        SN = S
        EN = E1
       ENDIF
       IDIV = IDIV - 1
c----------return to
       SOLD = S
       IM = MAX(ICONT,ICONT0)
       IF (IM>0) THEN
c        S2 = (EP-EN)/SN+SN
c        STMP = HALF*(S2-SQRT(S2*S2-FOUR*EP))
        STMP = HALF*(-SP+SN)
       ELSE
        STMP = EP*(SN-SP)/(EP-EN)
       ENDIF
       S = SP + STMP
       STMP =(SOLD-S)/SOLD
       STMP = MAX(ABS(STMP),ABS(E1-EOLD))
       IF (STMP<DTOL.OR.IDIV<-NLS_LIM) THEN
        IMCONV =0
       ELSEIF (IRIKS>0.AND.
     .         (S>TWO.OR.S<EM01)) THEN
        IMCONV =0
       ELSE
        IMCONV =-1
       ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  AL_CONSTRAINT1                source/implicit/nl_solv.F     
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        FRAC_DD                       source/implicit/integrator.F  
Chd|        INTEGRATOR2                   source/implicit/integrator.F  
Chd|        PRODUT_U                      source/implicit/produt_v.F    
Chd|        PRODUT_U2                     source/implicit/produt_v.F    
Chd|====================================================================
      SUBROUTINE AL_CONSTRAINT1(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                          DD    ,DDR   ,DG    ,DGR   ,DI    ,
     .                          DIR   ,W_DDL ,L_A   ,LAMDA ,SW2   ,
     .                          IER   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL,NDDL0,IDDL(*)  ,NDOF(*)  ,IKC(*),W_DDL(*) ,IER
C     REAL
      my_real
     .  LAMDA,DG(3,*),DGR(3,*),DI(3,*),DIR(3,*),DD(3,*),DDR(3,*),
     .  L_A,SW2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .  A,B,C,UI2,UIUD,UIUT,UT2,LAM1,LAM2,S,TH1,TH2,FAC
C-----------------------------------------------
         IER=0
         CALL PRODUT_U(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                 DG    ,DGR   ,UT2   ,W_DDL )
         A = UT2+SW2
C-------DD< DI+DD	 
         CALL INTEGRATOR2(1,NUMNOD,IDDL,NDOF,IKC,DD,DDR,DI,DIR)
         CALL PRODUT_U2(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                  DG    ,DGR   ,DD    ,DDR   ,UIUT  ,
     .                  W_DDL )
         B= TWO*(UIUT+SW2*LAMDA)
         CALL PRODUT_U(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                 DD    ,DDR   ,UI2   ,W_DDL )
         C = UI2-L_A*L_A+SW2*LAMDA
	 S= B*B-FOUR*A*C
 	IF (S>=0) THEN
	 S=SQRT(S)
	 LAM1=HALF*(-B+S)/A
	 LAM2=HALF*(-B-S)/A
         CALL PRODUT_U2(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                  DD    ,DDR   ,DI    ,DIR   ,UIUD  ,
     .                  W_DDL )
         CALL PRODUT_U2(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                  DG    ,DGR   ,DI    ,DIR   ,UIUT  ,
     .                  W_DDL )
         TH1= UIUD+LAM1*UIUT
         TH2= UIUD+LAM2*UIUT
	 IF (TH1>ZERO.AND.TH2<ZERO) THEN
	  LAMDA =LAM1
	 ELSEIF (TH1<ZERO.AND.TH2>ZERO) THEN
	  LAMDA =LAM2
	 ELSEIF (TH1>ZERO.AND.TH2>ZERO) THEN
	  LAMDA =MIN(LAM1,LAM2)
	 ELSE
	  IER=1
	 ENDIF
        ELSE
	 IER=1
	ENDIF
	 FAC =-ONE
        CALL FRAC_DD(1,NUMNOD,IDDL,NDOF,IKC,DD,DDR,DI,DIR,FAC)
C
      RETURN
      END
Chd|====================================================================
Chd|  AL_CONSTRAINT2                source/implicit/nl_solv.F     
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        PRODUT_U                      source/implicit/produt_v.F    
Chd|        PRODUT_U2                     source/implicit/produt_v.F    
Chd|====================================================================
      SUBROUTINE AL_CONSTRAINT2(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                          DD    ,DDR   ,DG    ,DGR   ,DI    ,
     .                          DIR   ,W_DDL ,L_A   ,LAMDA ,SW2   )
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
      INTEGER  NDDL,NDDL0,IDDL(*)  ,NDOF(*)  ,IKC(*),W_DDL(*)
C     REAL
      my_real
     .  LAMDA,DG(3,*),DGR(3,*),DI(3,*),DIR(3,*),DD(3,*),DDR(3,*),
     .  L_A,SW2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .  GI,UI2,UIUD,UIUT,S
C-----------------------------------------------
         CALL PRODUT_U(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                 DI    ,DIR   ,UI2   ,W_DDL )
         GI = SQRT(UI2)
         CALL PRODUT_U2(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                  DG    ,DGR   ,DI    ,DIR   ,UIUT  ,
     .                  W_DDL )
         CALL PRODUT_U2(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                  DD    ,DDR   ,DI    ,DIR   ,UIUD  ,
     .                  W_DDL )
         S = SW2*LAMDA	
         LAMDA = ((L_A-GI)*GI-UIUD)/(UIUT+S)
C
      RETURN
      END
Chd|====================================================================
Chd|  B_CORRECT                     source/implicit/nl_solv.F     
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE B_CORRECT(NDDL  ,F   ,FEXT  ,DLAMDA )
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
      INTEGER  NDDL
C     REAL
      my_real
     .  DLAMDA,F(*),FEXT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
         DO I =1,NDDL
	  F(I)=F(I)+DLAMDA*FEXT(I)
	 ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  B_CORRECT_H                   source/implicit/nl_solv.F     
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE B_CORRECT_H(F_DDL  ,L_DDL  ,F   ,FEXT  ,DLAMDA )
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
      INTEGER  F_DDL  ,L_DDL
C     REAL
      my_real
     .  DLAMDA,F(*),FEXT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
         DO I =F_DDL  ,L_DDL
	      F(I)=F(I)+DLAMDA*FEXT(I)
	     ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  AL_CONSTRAINTH1               source/implicit/nl_solv.F     
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        FRAC_DD                       source/implicit/integrator.F  
Chd|        INTEGRATOR2                   source/implicit/integrator.F  
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        PRODUT_UH                     source/implicit/produt_v.F    
Chd|        PRODUT_UH2                    source/implicit/produt_v.F    
Chd|====================================================================
      SUBROUTINE AL_CONSTRAINTH1(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                          DD    ,DDR   ,DG    ,DGR   ,DI    ,
     .                          DIR   ,W_DDL ,L_A   ,LAMDA ,SW2   ,
     .                          FT_N  ,LT_N  ,F_DDL ,L_DDL ,ITASK ,
     .                          IER   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL,NDDL0,IDDL(*)  ,NDOF(*)  ,IKC(*),W_DDL(*) ,IER,
     .         FT_N  ,LT_N  ,F_DDL ,L_DDL ,ITASK
C     REAL
      my_real
     .  LAMDA,DG(3,*),DGR(3,*),DI(3,*),DIR(3,*),DD(3,*),DDR(3,*),
     .  L_A,SW2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .  A,B,C,UI2,UIUD,UIUT,UT2,LAM1,LAM2,S,TH1,TH2,FAC
C-----------------------------------------------
         IER=0
         CALL PRODUT_UH(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                 DG    ,DGR   ,UT2   ,W_DDL  ,F_DDL ,
     .                 L_DDL ,ITASK )
C----------------------
      CALL MY_BARRIER
C---------------------
         A = UT2+SW2
C-------DD< DI+DD	 
         CALL INTEGRATOR2(FT_N,LT_N,IDDL,NDOF,IKC,DD,DDR,DI,DIR)
C----------------------
      CALL MY_BARRIER
C---------------------
         CALL PRODUT_UH2(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                  DG    ,DGR   ,DD    ,DDR   ,UIUT  ,
     .                  W_DDL ,F_DDL ,L_DDL ,ITASK )
         CALL PRODUT_UH(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                 DD    ,DDR   ,UI2   ,W_DDL  ,F_DDL ,
     .                 L_DDL ,ITASK )
C----------------------
      CALL MY_BARRIER
C---------------------
         B= TWO*(UIUT+SW2*LAMDA)
         C = UI2-L_A*L_A+SW2*LAMDA
	 S= B*B-FOUR*A*C
	IF (S>=0) THEN
	 S=SQRT(S)
	 LAM1=HALF*(-B+S)/A
	 LAM2=HALF*(-B-S)/A
         CALL PRODUT_UH2(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                  DD    ,DDR   ,DI    ,DIR   ,UIUD  ,
     .                  W_DDL ,F_DDL ,L_DDL ,ITASK )
         CALL PRODUT_UH2(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                  DG    ,DGR   ,DI    ,DIR   ,UIUT  ,
     .                  W_DDL ,F_DDL ,L_DDL ,ITASK )
C----------------------
      CALL MY_BARRIER
C---------------------
         TH1= UIUD+LAM1*UIUT
         TH2= UIUD+LAM2*UIUT
	 IF (TH1>ZERO.AND.TH2<ZERO) THEN
	  LAMDA =LAM1
	 ELSEIF (TH1<ZERO.AND.TH2>ZERO) THEN
	  LAMDA =LAM2
	 ELSEIF (TH1>ZERO.AND.TH2>ZERO) THEN
	  LAMDA =MIN(LAM1,LAM2)
	 ELSE
	  IER=1
	 ENDIF
        ELSE
	 IER=1
	ENDIF
	 FAC =-ONE
        CALL FRAC_DD(FT_N,LT_N,IDDL,NDOF,IKC,DD,DDR,DI,DIR,FAC)
C
      RETURN
      END
Chd|====================================================================
Chd|  AL_CONSTRAINTH2               source/implicit/nl_solv.F     
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        PRODUT_UH                     source/implicit/produt_v.F    
Chd|        PRODUT_UH2                    source/implicit/produt_v.F    
Chd|====================================================================
      SUBROUTINE AL_CONSTRAINTH2(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                          DD    ,DDR   ,DG    ,DGR   ,DI    ,
     .                          DIR   ,W_DDL ,L_A   ,LAMDA ,SW2   ,
     .                          FT_N  ,LT_N  ,F_DDL ,L_DDL ,ITASK )
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
      INTEGER  NDDL,NDDL0,IDDL(*)  ,NDOF(*)  ,IKC(*),W_DDL(*),
     .         FT_N  ,LT_N  ,F_DDL ,L_DDL ,ITASK
C     REAL
      my_real
     .  LAMDA,DG(3,*),DGR(3,*),DI(3,*),DIR(3,*),DD(3,*),DDR(3,*),
     .  L_A,SW2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .  GI,UI2,UIUD,UIUT,S
C-----------------------------------------------
         CALL PRODUT_UH(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                 DI    ,DIR   ,UI2   ,W_DDL  ,F_DDL ,
     .                 L_DDL ,ITASK )
         CALL PRODUT_UH2(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                  DG    ,DGR   ,DI    ,DIR   ,UIUT  ,
     .                  W_DDL ,F_DDL ,L_DDL ,ITASK )
         CALL PRODUT_UH2(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                  DD    ,DDR   ,DI    ,DIR   ,UIUD  ,
     .                  W_DDL ,F_DDL ,L_DDL ,ITASK )
C----------------------
      CALL MY_BARRIER
C---------------------
         GI = SQRT(UI2)
         S = SW2*LAMDA	
         LAMDA = ((L_A-GI)*GI-UIUD)/(UIUT+S)
C
      RETURN
      END
Chd|====================================================================
Chd|  B_CORRECT_HP                  source/implicit/nl_solv.F     
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        IMP_SMPINI                    source/implicit/imp_solv.F    
Chd|====================================================================
      SUBROUTINE B_CORRECT_HP(NDDL  ,F   ,FEXT  ,DLAMDA )
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
      INTEGER NDDL
C     REAL
      my_real
     .  DLAMDA,F(*),FEXT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  F_DDL  ,L_DDL ,ITSK
      INTEGER I
C-----------------------------------------------
!$OMP PARALLEL PRIVATE(ITSK,F_DDL  ,L_DDL,I)
      CALL IMP_SMPINI(ITSK   ,F_DDL  ,L_DDL ,NDDL )
         DO I =F_DDL  ,L_DDL
	       F(I)=F(I)+DLAMDA*FEXT(I)
	     ENDDO
!$OMP END PARALLEL
C
      RETURN
      END
Chd|====================================================================
Chd|  AL_CONSTRAINT1_HP             source/implicit/nl_solv.F     
Chd|-- called by -----------
Chd|        NL_SOLV                       source/implicit/nl_solv.F     
Chd|-- calls ---------------
Chd|        FRAC_DD_HP                    source/implicit/integrator.F  
Chd|        INTEGRATOR2_HP                source/implicit/integrator.F  
Chd|        PRODUT_UHP                    source/implicit/produt_v.F    
Chd|        PRODUT_UHP2                   source/implicit/produt_v.F    
Chd|====================================================================
      SUBROUTINE AL_CONSTRAINT1_HP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                           DD    ,DDR   ,DG    ,DGR   ,DI    ,
     .                           DIR   ,W_DDL ,L_A   ,LAMDA ,SW2   ,
     .                           IER   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NDDL,NDDL0,IDDL(*)  ,NDOF(*)  ,IKC(*),W_DDL(*) ,IER,
     .         FT_N  ,LT_N
C     REAL
      my_real
     .  LAMDA,DG(3,*),DGR(3,*),DI(3,*),DIR(3,*),DD(3,*),DDR(3,*),
     .  L_A,SW2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .  A,B,C,UI2,UIUD,UIUT,UT2,LAM1,LAM2,S,TH1,TH2,FAC
C-----------------------------------------------
         IER=0
         CALL PRODUT_UHP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                  DG    ,DGR   ,UT2   ,W_DDL  )
C---------------------
         A = UT2+SW2
C-------DD< DI+DD	 
         CALL INTEGRATOR2_HP(IDDL,NDOF,IKC,DD,DDR,DI,DIR)
C---------------------
         CALL PRODUT_UHP2(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                   DG    ,DGR   ,DD    ,DDR   ,UIUT  ,
     .                   W_DDL )
         CALL PRODUT_UHP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                   DD    ,DDR   ,UI2   ,W_DDL  )
C---------------------
         B= TWO*(UIUT+SW2*LAMDA)
         C = UI2-L_A*L_A+SW2*LAMDA
	     S= B*B-FOUR*A*C
	    IF (S>=0) THEN
	      S=SQRT(S)
	      LAM1=HALF*(-B+S)/A
	      LAM2=HALF*(-B-S)/A
         CALL PRODUT_UHP2(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                   DD    ,DDR   ,DI    ,DIR   ,UIUD  ,
     .                   W_DDL )
         CALL PRODUT_UHP2(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                   DG    ,DGR   ,DI    ,DIR   ,UIUT  ,
     .                   W_DDL )
C---------------------
         TH1= UIUD+LAM1*UIUT
         TH2= UIUD+LAM2*UIUT
	     IF (TH1>ZERO.AND.TH2<ZERO) THEN
	      LAMDA =LAM1
	     ELSEIF (TH1<ZERO.AND.TH2>ZERO) THEN
	      LAMDA =LAM2
	     ELSEIF (TH1>ZERO.AND.TH2>ZERO) THEN
	      LAMDA =MIN(LAM1,LAM2)
	     ELSE
	      IER=1
	     ENDIF
        ELSE
	     IER=1
	    ENDIF
	    FAC =-ONE
        CALL FRAC_DD_HP(IDDL,NDOF,IKC,DD,DDR,DI,DIR,FAC)
C
      RETURN
      END
Chd|====================================================================
Chd|  AL_CONSTRAINT2_HP             source/implicit/nl_solv.F     
Chd|-- called by -----------
Chd|        NL_SOLV                       source/implicit/nl_solv.F     
Chd|-- calls ---------------
Chd|        PRODUT_UHP                    source/implicit/produt_v.F    
Chd|        PRODUT_UHP2                   source/implicit/produt_v.F    
Chd|====================================================================
      SUBROUTINE AL_CONSTRAINT2_HP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                             DD    ,DDR   ,DG    ,DGR   ,DI    ,
     .                             DIR   ,W_DDL ,L_A   ,LAMDA ,SW2   )
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
      INTEGER  NDDL,NDDL0,IDDL(*)  ,NDOF(*)  ,IKC(*),W_DDL(*)
C     REAL
      my_real
     .  LAMDA,DG(3,*),DGR(3,*),DI(3,*),DIR(3,*),DD(3,*),DDR(3,*),
     .  L_A,SW2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .  GI,UI2,UIUD,UIUT,S
C-----------------------------------------------
         CALL PRODUT_UHP(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                   DI    ,DIR   ,UI2   ,W_DDL  )
         CALL PRODUT_UHP2(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                    DG    ,DGR   ,DI    ,DIR   ,UIUT  ,
     .                    W_DDL )
         CALL PRODUT_UHP2(NDDL0 ,NDDL  ,IDDL  ,NDOF  ,IKC   ,
     .                    DD    ,DDR   ,DI    ,DIR   ,UIUD  ,
     .                    W_DDL )
C---------------------
         GI = SQRT(UI2)
         S = SW2*LAMDA	
         LAMDA = ((L_A-GI)*GI-UIUD)/(UIUT+S)
C
      RETURN
      END

