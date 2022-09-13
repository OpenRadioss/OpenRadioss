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
Chd|  IMP_SOL_INIT                  source/implicit/imp_sol_init.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        BUF_DIM1                      source/implicit/produt_v.F    
Chd|        CP_DM                         source/implicit/produt_v.F    
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|        DIM_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|        DIM_INT7                      source/implicit/ind_glob_k.F  
Chd|        DIM_SPAN                      source/implicit/ind_glob_k.F  
Chd|        FIL_SPAN0                     source/implicit/ind_glob_k.F  
Chd|        IMP_FRII                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_TRANS0                    source/output/restart/wrrest.F
Chd|        IND_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|        SPMD_MSTOP                    source/mpi/init/spmd_mstop.F  
Chd|        ZERO1                         source/system/zero.F          
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        IMPBUFDEF_MOD                 share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE IMP_SOL_INIT(
     1    GEO       ,NPBY      ,LPBY      ,ITAB      ,SENSOR    ,
     2    IPARI     ,IXS       ,IXQ       ,IXC       ,IXT       ,
     4    IXP       ,IXR       ,IXTG      ,IXTG1     ,IXS10     ,
     5    IXS20     ,IXS16     ,IXUR      ,IPARG     ,     
     6    ELBUF     ,NINT7     ,NBINTC    ,X         ,DMCP      ,
     7    FR_ELEM   ,IAD_ELEM  ,FR_I2M    ,IAD_I2M   ,
     8    NPRW      ,NUM_IMP1  ,NUM_IMPL  ,MONVOL    ,IGRSURF   ,
     9    FR_MV     ,IPM       ,IGEO      ,IAD_RBY   ,
     A    FR_RBY    ,SH4TREE   ,SH3TREE   ,IRBE3     ,LRBE3     ,
     B    FR_RBE3M  ,IAD_RBE3M ,IRBE2     ,LRBE2     ,IBFV      ,
     C    VEL       ,ELBUF_TAB ,IFRAME    ,INTBUF_TAB,
     D    NDDL0     ,NNZK0     ,IMPBUF_TAB  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
      USE IMPBUFDEF_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "impl1_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc" 
#include      "buckcom.inc"
#include      "units_c.inc"  
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL0,NNZK0,IPARG(NPARG,*),FR_ELEM(*)  ,IAD_ELEM(2,*)
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*)
      INTEGER NINT7,NBINTC,IPARI(NPARI,*),
     .        FR_I2M(*),IAD_I2M(*),FR_RBY(*),IAD_RBY(*)
      INTEGER MONVOL(*),
     .        FR_MV(NSPMD+2,NVOLU),NPRW(*),FR_RBE3M(*),IAD_RBE3M(*)
      INTEGER IPM(NPROPMI,*),IGEO(NPROPGI,*),IFRAME(LISKN,*)
      INTEGER 
     .   IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*),
     .   IXR(NIXR,*), IXTG(NIXTG,*),IXUR(*),IXS10(6,*),IXS20(12,*),
     .   IXS16(8,*),IXTG1(4,*),IRBE3(*),LRBE3(*),
     .   SH4TREE(*), SH3TREE(*),
     .   IRBE2(*),LRBE2(*),IBFV(*),NUM_IMP1(*),NUM_IMPL(NINTER,NTHREAD)
C     REAL
      my_real
     .   GEO(NPROPG,*),SENSOR(NSENSR,*),ELBUF(*),VEL(*),X(*),DMCP(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE (IMPBUF_STRUCT_) ,TARGET :: IMPBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s 
C-----------------------------------------------
      INTEGER I,J,K,N,M,L,NDOFI,NDOFJ,NKINE,NMIJ2,IP,NPN,NPP,IER1,
     .        LI1,LI2,LI3,LI4,LI5,LI6,LI7,LI8,LI9,LI10,LI11,LI12,
     .        LIF,LI13,LI14,LI15,LI16,LI17,IER2
      INTEGER NTMP,L1,NNDLNKINE,NNMAX,NKMAX,NNDL
      INTEGER, POINTER     :: NDDL,NNZK,NRBYAC,NINT2,NMC,NMC2,NMONV
      INTEGER, DIMENSION(:) ,POINTER     :: IADK,JDIK,IDDL,NDOF,INLOC,LSIZE,I_IMP,
     .                                      IRBYAC,NSC,IINT2,NKUD,IMONV,IKINW
      my_real, DIMENSION(:) ,POINTER     :: DIAG_K,LT_K,DIAG_M,LT_M,LB,
     .                                      LB0,BKUD,D_IMP,ELBUF_C,BUFMAT_C,
     .                                      X_C,DD,DDR
C=======================================================================
          IF (N2D>0) THEN
           IF(ISPMD==0)THEN
             CALL ANCMSG(MSGID=161,ANMODE=ANINFO)
           ENDIF
C            CALL ARRET(2)
          ENDIF
          IF (IRESP==1) THEN
           IF(ISPMD==0)THEN
             CALL ANCMSG(MSGID=162,ANMODE=ANINFO)
           ENDIF
             CALL ARRET(2)
          ENDIF
#ifndef MPI
        IF (NEIG>0) THEN
           IF(ISPMD==0)THEN
             CALL ANCMSG(MSGID=294,ANMODE=ANINFO)
           ENDIF
           CALL ARRET(2)
	    END IF
        IF (NBUCK>0) THEN
           IF(ISPMD==0)THEN
             CALL ANCMSG(MSGID=295,ANMODE=ANINFO)
           ENDIF
           CALL ARRET(2)
	    END IF
#endif
c          
          ALLOCATE(IMPBUF_TAB%IDDL(NUMNOD))
          ALLOCATE(IMPBUF_TAB%NDOF(NUMNOD))
          ALLOCATE(IMPBUF_TAB%INLOC(NUMNOD))
          ALLOCATE(IMPBUF_TAB%IRBYAC(2*NRBYKIN))         
          ALLOCATE(IMPBUF_TAB%NSC(NRBYKIN))        
          ALLOCATE(IMPBUF_TAB%IINT2(NINTER))           
          ALLOCATE(IMPBUF_TAB%NKUD(NFXVEL))          
          ALLOCATE(IMPBUF_TAB%IMONV(NVOLU))            
          NDDL => IMPBUF_TAB%NDDL
          NNZK => IMPBUF_TAB%NNZK
          NRBYAC => IMPBUF_TAB%NRBYAC
          NINT2 => IMPBUF_TAB%NINT2
          NMC => IMPBUF_TAB%NMC
          NMC2 => IMPBUF_TAB%NMC2
          NMONV => IMPBUF_TAB%NMONV
          IDDL => IMPBUF_TAB%IDDL
          NDOF => IMPBUF_TAB%NDOF
          INLOC => IMPBUF_TAB%INLOC
          LSIZE => IMPBUF_TAB%LSIZE
          I_IMP => IMPBUF_TAB%I_IMP
          IRBYAC => IMPBUF_TAB%IRBYAC
          NSC => IMPBUF_TAB%NSC
		  NSC = 0
          IINT2 => IMPBUF_TAB%IINT2
          NKUD => IMPBUF_TAB%NKUD
          IMONV => IMPBUF_TAB%IMONV
          CALL DIM_GLOB_K(
     1    GEO       ,NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,
     2    SENSOR    ,IRBYAC    ,NINT2     ,IINT2     ,IPARI     ,
     3    IXS       ,IXQ       ,IXC       ,IXT       ,
     4    IXP       ,IXR       ,IXTG      ,IXTG1     ,IXS10     ,
     5    IXS20     ,IXS16     ,IXUR      ,IPARG     ,NDOF      ,
     6    IDDL      ,NDDL      ,NNZK      ,ELBUF     ,INLOC     ,
     7    LSIZE     ,FR_ELEM   ,IAD_ELEM  ,FR_I2M    ,IAD_I2M   ,
     8    NPRW      ,NMONV     ,IMONV     ,MONVOL    ,IGRSURF   ,
     9               FR_MV     ,IPM       ,IGEO      ,IAD_RBY   ,
     A    FR_RBY    ,SH4TREE   ,SH3TREE   ,IRBE3     ,LRBE3     ,
     B    FR_RBE3M  ,IAD_RBE3M ,IRBE2     ,LRBE2     ,IBFV      ,
     C    VEL       ,ELBUF_TAB ,IFRAME    ,INTBUF_TAB   )
	      S_IADK=NDDL+1
		  S_JDIK=NNZK
          NDDL0 = NDDL
          NNZK0 = NNZK
          ALLOCATE(IMPBUF_TAB%IADK(S_IADK))
          ALLOCATE(IMPBUF_TAB%JDIK(S_JDIK))
          IADK => IMPBUF_TAB%IADK
          JDIK => IMPBUF_TAB%JDIK
c		  S_ISIJ=IMPBUF_TAB%LSIZE(4)
c		  S_IMIJ=IMPBUF_TAB%LSIZE(5)
c		  S_NSS=IMPBUF_TAB%LSIZE(1)
c		  S_ISS=IMPBUF_TAB%LSIZE(3)
c		  S_ISIJ2=IMPBUF_TAB%LSIZE(7)
c		  S_NSS2=IMPBUF_TAB%LSIZE(2)
c		  S_ISS2=IMPBUF_TAB%LSIZE(6)
c		  S_NSC2=NINT2
c		  S_NROWK=IMPBUF_TAB%LSIZE(8)
c		  S_ICOK=(LSIZE(8)-LCOKM)*LSIZE(9)
c		  S_ICOKM=LCOKM*LSIZE(10)
c		  S_NMIJ2=4*LSIZE(11)
c		  S_NSS3=IMPBUF_TAB%LSIZE(14)
c		  S_ISB2=IMPBUF_TAB%LSIZE(15)
c		  S_NSRB2=IMPBUF_TAB%LSIZE(16)
C		  
          NKINE = LSIZE(8) 
          NNMAX = LSIZE(9) 
          NKMAX = LSIZE(10)
          NMIJ2 = LSIZE(11)
          NPN   = LSIZE(12)
          NPP   = LSIZE(13)
C		  
c          ALLOCATE(IMPBUF_TAB%ISIJ(S_ISIJ))
c          ALLOCATE(IMPBUF_TAB%IMIJ(S_IMIJ))
c          ALLOCATE(IMPBUF_TAB%NSS(S_NSS))
c          ALLOCATE(IMPBUF_TAB%ISIJ2(S_ISIJ2))
c          ALLOCATE(IMPBUF_TAB%NSS2(S_NSS2))
c          ALLOCATE(IMPBUF_TAB%ISS2(S_ISS2))
c          ALLOCATE(IMPBUF_TAB%NSC2(S_NSC2))
c          ALLOCATE(IMPBUF_TAB%ISS2(S_ISS2))
c          ALLOCATE(IMPBUF_TAB%NROWK(S_NROWK))
c          ALLOCATE(IMPBUF_TAB%ICOK(S_ICOK))
c          ALLOCATE(IMPBUF_TAB%ICOKM(S_ICOKM))
c          ALLOCATE(IMPBUF_TAB%NMIJ2(S_NMIJ2))
c          ALLOCATE(IMPBUF_TAB%NSS3(S_NSS3))
c          ALLOCATE(IMPBUF_TAB%ISB2(S_ISB2))
c          ALLOCATE(IMPBUF_TAB%NSRB2(S_NSRB2))
C----finally not to splite IKINW, otherwise too many changes		  
          LI1 =1
          LI2 = LI1+LSIZE(4)
          LI3 = LI2+LSIZE(5)
          LI4 = LI3+LSIZE(1)
          LI5 = LI4+LSIZE(3)
          LI6 = LI5+LSIZE(7)
          LI7 = LI6+LSIZE(2)
          LI8 = LI7+LSIZE(6)
          LI9 = LI8+NINT2
          LI10 = LI9+LSIZE(8)
          LI11 = LI10+(LSIZE(8)-LCOKM)*LSIZE(9)
          LI12 = LI11+LCOKM*LSIZE(10)
          LI13 = LI12+4*LSIZE(11)
          LI14 = LI13+LSIZE(14)
          LI15 = LI14+LSIZE(15)
          LIF = LI15+LSIZE(16)
		  S_IKINW = LIF
          ALLOCATE(IMPBUF_TAB%IKINW(S_IKINW))
          IKINW => IMPBUF_TAB%IKINW
          CALL IND_GLOB_K(NPBY  ,LPBY      ,
     1    ITAB      ,NRBYAC    ,IRBYAC    ,NSC       ,IKINW(LI1),
     2    NMC       ,IKINW(LI2),IKINW(LI3),IKINW(LI4),NINT2     ,
     3    IINT2     ,IPARI     ,INTBUF_TAB,IKINW(LI8),IKINW(LI5),
     4    IKINW(LI6),IKINW(LI7),IPARG     ,ELBUF     ,ELBUF_TAB ,
     5    IXS       ,IXQ       ,IXC       ,IXT       ,IXP       ,
     6    IXR       ,IXTG      ,IXTG1     ,IXS10     ,IXS20     ,
     7    IXS16     ,IXUR      ,IDDL      ,NDOF      ,IADK      ,
     8    JDIK      ,NDDL      ,NNZK      ,LSIZE(9)  ,LSIZE(8)  ,
     9    INLOC     ,LSIZE(10),IKINW(LI9),IKINW(LI10),IKINW(LI11),
     A    LSIZE(11) ,IKINW(LI12),LI1      ,LSIZE(12) ,LSIZE(13) ,
     B    FR_ELEM   ,IAD_ELEM  ,IPM       ,IGEO      ,IRBE3     ,
     C    LRBE3     ,IKINW(LI13),FR_I2M   ,IAD_I2M   ,FR_RBE3M  ,
     D    IAD_RBE3M ,IRBE2     ,LRBE2     ,IKINW(LI14),IKINW(LI15))
          NTMP=MAX(NKMAX,NNMAX)
          IF (IRODDL/=0) THEN
           MAXB = MIN(6*(NTMP+1),NDDL)
           MAXB1 = MIN(6*(NNMAX+1),NDDL)
          ELSE
           MAXB = MIN(3*(NTMP+1),NDDL)
           MAXB1 = MIN(3*(NNMAX+1),NDDL)
          ENDIF
          MAXB0 = MAXB
          NNDL=3*NUMNOD
c          IF (IMON>0) CALL STOPTIME(31,1)
          IF(ISPMD==0)THEN
           WRITE(ISTDO,*)
           WRITE(ISTDO,*)'          **************************'
           WRITE(ISTDO,*)'          ** IMPLICIT OPTION USED **'
           WRITE(ISTDO,*)'          **************************'
           WRITE(ISTDO,*)
          ENDIF
C-----integer : 1:IKC,2:IKUD,3:W_DDL,4:IADM,5:JDIM,6:NDOFI,7:IDDLI,8:INBUF_C
          IF(NFXVEL>0.AND.NEIG==0)THEN
           L1=NFXVEL*MAX(3,MAXB)
          ELSE
           L1=0
          ENDIF
	      S_IKC=NDDL+6
	      S_IKUD=L1
C  -----------W_DDL is reserved anyway------
	      S_W_DDL=NDDL
          MAX_L = 0
          IF (NMONV>0.AND.ISOLV>=3.AND.NEIG==0) THEN
           IF(ISPMD==0)THEN
             CALL ANCMSG(MSGID=163,ANMODE=ANINFO)
           ENDIF
             CALL ARRET(2)
          ENDIF
C     ----- ---------
         IF ((ISOLV==1.OR.ISOLV>4).AND.N_PAT>1) THEN
          CALL FIL_SPAN0(NRBYAC,IRBYAC,NPBY,IDDL,NDOF,NDDL)
          CALL DIM_SPAN(N_PAT,NDDL,IADK,JDIK,MAX_L,MAXB1)
	      S_IADM=NDDL+1
         ELSE
	      S_IADM=0
         ENDIF
	      S_JDIM=MAX_L
          ALLOCATE(IMPBUF_TAB%IKC(S_IKC))
          ALLOCATE(IMPBUF_TAB%IKUD(S_IKUD))
          ALLOCATE(IMPBUF_TAB%W_DDL(S_W_DDL))
          ALLOCATE(IMPBUF_TAB%IADM(S_IADM))
          ALLOCATE(IMPBUF_TAB%JDIM(S_JDIM))
		  S_NDOFI=NUMNOD
	      S_IDDLI=0
          NINT7 = 0
          S_CAND_N = 0
          S_CAND_E = 0
          S_INDSUBT= 0  
         IF (NINTER/=0.AND.NEIG==0) THEN
          CALL DIM_INT7(NINTER,IPARI ,INTBUF_TAB ,NINT7)
          IF (NINT7>0) THEN
           IF (ISOLV==4.AND.IMACH==3) THEN
              CALL ANCMSG(MSGID=214,ANMODE=ANINFO)
              CALL ARRET(2)
           ENDIF
           S_CAND_N = NINT7*NTHREAD !NS_IMP
           S_CAND_E = S_CAND_N      !NE_IMP
           S_INDSUBT= S_CAND_N      !IND_IMP  
	       S_IDDLI=NUMNOD
          ELSE
           IF (ILINTF>0.AND.IMACH/=3) ILINTF=0
          ENDIF
         ENDIF
         ALLOCATE(IMPBUF_TAB%CAND_N(S_CAND_N))
         ALLOCATE(IMPBUF_TAB%CAND_E(S_CAND_E))
         ALLOCATE(IMPBUF_TAB%INDSUBT(S_INDSUBT),STAT=IER1)
		  S_INBUF_C =0
          ALLOCATE(IMPBUF_TAB%NDOFI(S_NDOFI))
          ALLOCATE(IMPBUF_TAB%IDDLI(S_IDDLI))
          ALLOCATE(IMPBUF_TAB%INBUF_C(S_INBUF_C))
         IF (NSPMD>1.AND.NBINTC>0) CALL IMP_FRII(NINTER)
c         IIF=II9
C-----reel : 1,2,3,4:DIAG_K,LT_K,DIAG_M,LT_M,5,6:LB,DB,7:BKUD,8,9:D_IMP,DR_IMP
C----       10,11,12:ELBUF_C,BUFMAT_C,X_C,13,14:DD,DDR,15,16:X_actual,free
C-----------17-21:FEXT,DG,DGR,DI,DIR;(Riks arc-length);22:BUFIN_C,23,24:AC,ACR,
	      S_DIAG_K=NDDL
	      S_LT_K=NNZK
	      S_DIAG_M=NDDL
	      S_LT_M=MAX(NNZK,MAX_L)
	      S_LB=NDDL
          IF (ILINE/=1.AND.TT==ZERO.AND.ISPRB==0) THEN
           IF (NMONV>0) ISIGINI=1
          ELSE
           ISIGINI=0
          ENDIF
          IF ((ISPRB==1.OR.ISIGINI==1.OR.ILINTF>0)
     .        .AND.NEIG==0) THEN
	       S_LB0=NDDL
          ELSE
	       S_LB0=0
          ENDIF
	      S_BKUD=L1
	      S_D_IMP=NNDL
          IF (IRODDL/=0.AND.NEIG==0) THEN
	        S_DR_IMP=NNDL
          ELSE
	        S_DR_IMP=0
          ENDIF
          IF (ILINE/=1) THEN
           CALL BUF_DIM1(S_ELBUF_C,S_BUFMAT_C)
		   S_X_C=NNDL
		   S_DD=NNDL
           IF (IRODDL/=0) THEN
		    S_DDR=NNDL
           ELSE
		    S_DDR=0
           ENDIF
          ELSEIF (ILINTF>0) THEN
           CALL BUF_DIM1(S_ELBUF_C,S_BUFMAT_C)
		   S_X_C=NNDL
		    S_DD=0
		    S_DDR=0
          ELSE
		    S_ELBUF_C=0
			S_BUFMAT_C=0
		    S_X_C=0
		    S_DD=0
		    S_DDR=0
          ENDIF
		  S_X_A=0
          IF (NEIG==0) THEN
           IF (ISMDISP>0) S_X_A=NNDL
          ENDIF
C-----------17-21:FEXT,DG,DGR,DG0,DGR0;(Riks arc-length);22:BUFIN_C,23,24:AC,ACR,
		  S_FEXT=0
		  S_DG=0
		  S_DGR=0
		  S_DG0=0
		  S_DGR0=0
          IF (IDTC==3) THEN
		   S_FEXT=NDDL
           IF (IRODDL/=0) THEN
		    S_DG=NNDL
		    S_DGR=NNDL
		    S_DG0=NNDL
		    S_DGR0=NNDL
           ELSE
		    S_DG=NNDL
		    S_DG0=NNDL
           ENDIF
          ENDIF
		   S_BUFIN_C=0
		   S_AC=0
		   S_ACR=0
          IF (NEIG==0) THEN
		    S_AC=NNDL
            IF (IRODDL/=0) S_ACR=NNDL
          ENDIF
C		  
	      ALLOCATE(IMPBUF_TAB%DIAG_K(S_DIAG_K)) 
	      ALLOCATE(IMPBUF_TAB%LT_K(S_LT_K))   
	      ALLOCATE(IMPBUF_TAB%DIAG_M(S_DIAG_M)) 
	      ALLOCATE(IMPBUF_TAB%LT_M(S_LT_M))   
	      ALLOCATE(IMPBUF_TAB%LB(S_LB))     
	      ALLOCATE(IMPBUF_TAB%LB0(S_LB0))     
	      ALLOCATE(IMPBUF_TAB%BKUD(S_BKUD))   
	      ALLOCATE(IMPBUF_TAB%D_IMP(S_D_IMP))  
	      ALLOCATE(IMPBUF_TAB%DR_IMP(S_DR_IMP)) 
          ALLOCATE(IMPBUF_TAB%ELBUF_C(S_ELBUF_C))
		  ALLOCATE(IMPBUF_TAB%BUFMAT_C(S_BUFMAT_C))
		  ALLOCATE(IMPBUF_TAB%X_C(S_X_C))
		  ALLOCATE(IMPBUF_TAB%DD(S_DD))
		  ALLOCATE(IMPBUF_TAB%DDR(S_DDR))
		  ALLOCATE(IMPBUF_TAB%X_A(S_X_A))   
		  ALLOCATE(IMPBUF_TAB%FEXT(S_FEXT))  
		  ALLOCATE(IMPBUF_TAB%DG(S_DG))    
		  ALLOCATE(IMPBUF_TAB%DGR(S_DGR))   
		  ALLOCATE(IMPBUF_TAB%DG0(S_DG0))    
		  ALLOCATE(IMPBUF_TAB%DGR0(S_DGR0))   
		  ALLOCATE(IMPBUF_TAB%BUFIN_C(S_BUFIN_C))
		  ALLOCATE(IMPBUF_TAB%AC(S_AC))
		  ALLOCATE(IMPBUF_TAB%ACR(S_ACR),STAT=IER2)
c
         IF (IER1/=0.OR.IER2/=0) THEN
             CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='FOR IMPLICIT')
             CALL ARRET(2)
         ENDIF
C---------INITIALISATION------
          IMPBUF_TAB%D_IMP=ZERO
          IMPBUF_TAB%IKC=0
          NDDL0 = NDDL
          NNZK0 = NNZK
          ISETK=1
          IDSC=1
C---------IT(TOTAL),ITC,IWAIT,IDIV,R02,EIMP,E02-,Ichang-----
          I_IMP=0
          IT_BCS = 0
          IT_PCG = 0
          IMPBUF_TAB%R_IMP(1:25)=ZERO
C ----   X_actual,V_zero-----
         IF (NEIG==0) THEN
          IF (S_X_A>0) CALL CP_REAL(NNDL,X,IMPBUF_TAB%X_A)
          IF (IDYNA==0) CALL CP_DM(NUMGEO,GEO,IGEO,DMCP,1)
         END IF
C----i--initialisation for for travers nodes detection-----
          IF (NINTER/=0.AND.NEIG==0) THEN
           NT_IMP1=0
           DO I=1,NINTER
            NUM_IMP1(I)=0
           ENDDO
           DO J=1,NTHREAD
           DO I=1,NINTER
            NUM_IMPL(I,J)=0
           ENDDO
           ENDDO
          ENDIF
          CALL IMP_TRANS0(IMPBUF_TAB%R_IMP,IMPL_S0)
          IF (ILINE/=1) CALL ZERO1(IMPBUF_TAB%DD,NNDL)
c      IBUCK= IBUCKL
      IF (NBUCK>0.AND.BISOLV==2.AND.NDSOLV/=1) THEN
         WRITE(ISTDO,'(A)')
     .' !! BISOLV =2 REQUIRES OPTION /IMPL/GRAPH IN RADIOSS STARTER'
         WRITE(IOUT,'(A)')
     .' !! BISOLV =2 REQUIRES OPTION /IMPL/GRAPH IN RADIOSS STARTER'
            CALL SPMD_MSTOP()
            RETURN
      ENDIF
C	  
      RETURN
      END SUBROUTINE IMP_SOL_INIT
Chd|====================================================================
Chd|  IMP_RESTARCP                  source/implicit/imp_sol_init.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        CP_DM                         source/implicit/produt_v.F    
Chd|        CP_REAL                       source/implicit/produt_v.F    
Chd|        ZEROR                         source/system/zero.F          
Chd|        IMPBUFDEF_MOD                 share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE IMP_RESTARCP(X,V,VR,GEO,IGEO,DMCP,IMPBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMPBUFDEF_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .   IGEO(NPROPGI,*)
C     REAL
      my_real
     .   GEO(NPROPG,*),X(*),DMCP(*),V(*),VR(*)
      TYPE (IMPBUF_STRUCT_) :: IMPBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s 
C-----------------------------------------------
      INTEGER I,J,NNDL
C-----------------------------------------------
          IF (IDYNA==0) THEN
            CALL ZEROR(V,NUMNOD)
            IF (IRODDL/=0) CALL ZEROR(VR,NUMNOD)
            CALL CP_DM(NUMGEO,GEO,IGEO,DMCP,2)
          ENDIF
C ----------------for restart---
          IF (ISMDISP>0) THEN
            NNDL=3*NUMNOD
            CALL CP_REAL(NNDL,IMPBUF_TAB%X_A,X)
          END IF
C	  
      RETURN
      END SUBROUTINE IMP_RESTARCP
