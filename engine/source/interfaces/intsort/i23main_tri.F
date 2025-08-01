Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    i23main_tri                  ../engine/source/interfaces/intsort/i23main_tri.F
!||--- called by ------------------------------------------------------
!||    inttri                       ../engine/source/interfaces/intsort/inttri.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                       ../engine/source/output/message/message.F
!||    arret                        ../engine/source/system/arret.F
!||    check_sorting_criteria       ../engine/source/interfaces/intsort/check_sorting_criteria.F90
!||    i23buce                      ../engine/source/interfaces/intsort/i23buce.F
!||    i23trc                       ../engine/source/interfaces/intsort/i23trc.F
!||    i7xsave                      ../engine/source/interfaces/intsort/i7xsave.F
!||    imp_rnumcd                   ../engine/source/implicit/imp_int_k.F
!||    my_barrier                   ../engine/source/system/machine.F
!||    spmd_rnumcd                  ../engine/source/mpi/interfaces/spmd_i7tool.F
!||    spmd_tri23vox0               ../engine/source/mpi/interfaces/spmd_int.F
!||    spmd_tri7gat                 ../engine/source/mpi/interfaces/spmd_int.F
!||    spmd_tri7vox                 ../engine/source/mpi/interfaces/spmd_int.F
!||    startime                     ../engine/source/system/timer_mod.F90
!||    stoptime                     ../engine/source/system/timer_mod.F90
!||    upgrade_multimp              ../common_source/interf/upgrade_multimp.F
!||--- uses       -----------------------------------------------------
!||    check_sorting_criteria_mod   ../engine/source/interfaces/intsort/check_sorting_criteria.F90
!||    h3d_mod                      ../engine/share/modules/h3d_mod.F
!||    intbufdef_mod                ../common_source/modules/interfaces/intbufdef_mod.F90
!||    message_mod                  ../engine/share/message_module/message_mod.F
!||    multi_fvm_mod                ../common_source/modules/ale/multi_fvm_mod.F90
!||    timer_mod                    ../engine/source/system/timer_mod.F90
!||    tri7box                      ../engine/share/modules/tri7box.F
!||====================================================================
      SUBROUTINE I23MAIN_TRI(TIMERS,
     1                  IPARI   ,X       ,INTBUF_TAB,V     ,
     2                  MS      ,NIN     ,ITASK   ,MWAG    ,WEIGHT  ,
     3                  ISENDTO ,IRCVFROM,RETRI   ,IAD_ELEM,FR_ELEM ,
     4                  ITAB    ,KINET   ,NRTM_T  ,RENUM   ,
     5                  NSNFIOLD,ESHIFT  ,NUM_IMP ,IND_IMP ,NODNX_SMS,
     6                  H3D_DATA, MULTI_FVM,INTHEAT,IDT_THERM,NODADT_THERM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TIMER_MOD
      USE TRI7BOX
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
      USE H3D_MOD 
      USE MULTI_FVM_MOD
      use check_sorting_criteria_mod , only : check_sorting_criteria
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
C common pour variable globale en memoire partagee
      COMMON /I7MAINC/BMINMA,CURV_MAX_MAX,RESULT,NSNR,NSNROLD,I_MEMG,NMN_G
      INTEGER RESULT,NSNR,NSNROLD,I_MEMG,NMN_G
      my_real 
     .        BMINMA(6),CURV_MAX_MAX
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TIMER_), INTENT(INOUT) :: TIMERS
      INTEGER NIN ,ITASK, RETRI, NRTM_T,ESHIFT,
     .        NUM_IMP ,IND_IMP(*),
     .        ITAB(*), KINET(*),
     .        IPARI(NPARI,NINTER),  MWAG(*),
     .        ISENDTO(NINTER+1,*),IRCVFROM(NINTER+1,*),
     .        WEIGHT(*), IAD_ELEM(2,*) ,FR_ELEM(*),
     .        RENUM(*), NSNFIOLD(NSPMD), NODNX_SMS(*)
      INTEGER, INTENT(IN) :: INTHEAT
      INTEGER, INTENT(IN) :: IDT_THERM
      INTEGER, INTENT(IN) :: NODADT_THERM
C     REAL
      my_real 
     .   X(*), V(*), MS(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LOC_PROC, IFQ, INTTH, ITIED,
     .        I, IP0, IP1, IP2, IP21, I_SK_OLD, 
     .        ADD1, NB_N_B, NOINT, INACTI, MULTIMP, IGAP, I_STOK ,NMN_L
      INTEGER 
     .       ILD, NCONTACT,NCONT,INTFRIC,
     .       I_MEM,CAND_N_OLD,IDUM1(1),ILEV, IVIS2
C     REAL
      my_real
     .   GAP,MAXBOX,MINBOX,TZINF,
     .   XMAXL, YMAXL, ZMAXL, XMINL, YMINL, ZMINL, GAPMIN, GAPMAX,
     .   SX,SY,SZ,SX2,SY2,SZ2,
     .   C_MAXL,CURV_MAX(NRTM_T),RDUM1(1)
      INTEGER :: NMN,NSN,NTY
      logical :: need_computation
C-----------------------------------------------
      ! --------------
      ! check if the current interface needs to be sorted
      call check_sorting_criteria( need_computation,nin,npari,nspmd,  
     .                             itask,ipari(1,nin),tt,intbuf_tab )
      if( .not.need_computation ) return
      ! --------------
      I_MEM = 0
      I_MEMG = 0
      NMN_G = 0
      NMN_L = 0
!     INTFRIC is a local variable (local to each thread in OMP //)
      INTFRIC = 0
C
C     Specific TYPE7 :
      ITIED = 0 
C
      LOC_PROC=ISPMD+1
      NSN    =IPARI(5,NIN)
      NMN    =IPARI(6,NIN)
      NTY    =IPARI(7,NIN)
      IVIS2  =IPARI(14,NIN)
      NOINT  =IPARI(15,NIN)
      NCONT  =IPARI(18,NIN)
      ILEV  =IPARI(20,NIN)
      INACTI =IPARI(22,NIN)
      MULTIMP=IPARI(23,NIN)

      NCONTACT=MULTIMP*NCONT
C
      NSNROLD = IPARI(24,NIN)
C
      GAP =INTBUF_TAB%VARIABLES(2)
      GAPMIN=INTBUF_TAB%VARIABLES(13)
      GAPMAX=INTBUF_TAB%VARIABLES(16)
C
C
C -------------------------------------------------------------
C
      RETRI = 1
C
C -------------------------------------------------------------
C
      MAXBOX = INTBUF_TAB%VARIABLES(9)
      MINBOX = INTBUF_TAB%VARIABLES(12)
      TZINF  = INTBUF_TAB%VARIABLES(8)
      BMINMA(1)=-EP30
      BMINMA(2)=-EP30
      BMINMA(3)=-EP30
      BMINMA(4)=EP30
      BMINMA(5)=EP30
      BMINMA(6)=EP30
      CURV_MAX_MAX = ZERO
C
C -------------------------------------------------------------
C     STOCKAGE DES ANCIENS CANDIDATS 
C -------------------------------------------------------------
C
C Barriere dans tous les cas pour bminma [et cur_max_max]
C
      CALL MY_BARRIER
C
      IF(ITASK==0)THEN
        IP0 = 1
        IP1 = IP0 + NSN + NSNROLD + 3
C MWA = MWAG SUR TASK 0
        I_SK_OLD = INTBUF_TAB%I_STOK(1)
        CALL I23TRC(
     1    NSN+NSNROLD  ,I_SK_OLD     ,INTBUF_TAB%CAND_N,INTBUF_TAB%CAND_E,
     2    INTBUF_TAB%CAND_P,INTBUF_TAB%FTSAVX,INTBUF_TAB%FTSAVY,INTBUF_TAB%FTSAVZ,
     3    MWAG(IP0)    ,INTBUF_TAB%IFPEN)
C 
        INTBUF_TAB%I_STOK(1)=I_SK_OLD
      ENDIF
C -------------------------------------------------------------
C     CALCUL BORNE DOMAINE REMONTE DANS I7XSAVE
C -------------------------------------------------------------
C eshift : decalage sur cand_e
      CALL I7XSAVE(
     1       X            ,INTBUF_TAB%NSV,INTBUF_TAB%MSR,NSN ,NMN     ,
     2       ITASK        ,INTBUF_TAB%XSAV,XMINL        ,YMINL ,ZMINL   ,
     3       XMAXL        ,YMAXL        ,ZMAXL        ,C_MAXL,CURV_MAX,
     4       IPARI(39,NIN),INTBUF_TAB%IRECTM(1+4*ESHIFT) ,NRTM_T,SX    ,SY      ,
     5       SZ           ,SX2          ,SY2          ,SZ2            ,NMN_L)
#include "lockon.inc"
      BMINMA(1) = MAX(BMINMA(1),XMAXL)
      BMINMA(2) = MAX(BMINMA(2),YMAXL)
      BMINMA(3) = MAX(BMINMA(3),ZMAXL)
      BMINMA(4) = MIN(BMINMA(4),XMINL)
      BMINMA(5) = MIN(BMINMA(5),YMINL)
      BMINMA(6) = MIN(BMINMA(6),ZMINL)
      CURV_MAX_MAX = MAX(CURV_MAX_MAX,C_MAXL)
      NMN_G = NMN_G + NMN_L
#include "lockoff.inc"

      RESULT = 0
C BARRIER II_STOK et RESULT
       CALL MY_BARRIER
       IF(ITASK==0)THEN
         IF(ABS(BMINMA(6)-BMINMA(3))>2*EP30.OR.
     +      ABS(BMINMA(5)-BMINMA(2))>2*EP30.OR.
     +      ABS(BMINMA(4)-BMINMA(1))>2*EP30)THEN
           CALL ANCMSG(MSGID=87,ANMODE=ANINFO,
     .            I1=NOINT,C1='(I23BUCE)')
           CALL ARRET(2)
         END IF
C
         BMINMA(1)=BMINMA(1)+TZINF+CURV_MAX_MAX
         BMINMA(2)=BMINMA(2)+TZINF+CURV_MAX_MAX
         BMINMA(3)=BMINMA(3)+TZINF+CURV_MAX_MAX
         BMINMA(4)=BMINMA(4)-TZINF-CURV_MAX_MAX
         BMINMA(5)=BMINMA(5)-TZINF-CURV_MAX_MAX
         BMINMA(6)=BMINMA(6)-TZINF-CURV_MAX_MAX

        IF(NSPMD > LRVOXELP)THEN
          CALL ANCMSG(MSGID=36,ANMODE=ANINFO,
     .            C1='(I7MAINTRI)')
          CALL ARRET(2)
        END IF

        NSNR = 0

      END IF


      IF(NSPMD > 1) THEN

        IF(ITASK==0) CRVOXEL(0:LRVOXEL,0:LRVOXEL,LOC_PROC)=0
c        goto 150
        CALL MY_BARRIER

        IF (IMONM > 0 .AND. ITASK == 0) CALL STARTIME(TIMERS,26)
        CALL SPMD_TRI23VOX0(
     1      X           ,BMINMA  ,IPARI(21,NIN),NRTM_T,INTBUF_TAB%STFM(1+ESHIFT),
     2      TZINF       ,CURV_MAX,GAPMIN       ,GAPMAX,INTBUF_TAB%GAP_M(1+ESHIFT),
     3      INTBUF_TAB%IRECTM(1+4*ESHIFT),GAP  ,INTBUF_TAB%VARIABLES(7) ,INTBUF_TAB%MSR)
 
        CALL MY_BARRIER
        IF (IMONM > 0 .AND. ITASK == 0) CALL STOPTIME(TIMERS,26)
c 150   continue
        IF(ITASK==0)THEN
C
C recuperation des noeuds remote NSNR stockes dans XREM
C
          IFQ  =0
          INTTH=0
        IF (IMONM > 0 .AND. ITASK == 0) CALL STARTIME(TIMERS,25)

            CALL SPMD_TRI7VOX(
     1      INTBUF_TAB%NSV,NSN    ,X    ,V        ,MS     ,
     2      BMINMA   ,WEIGHT  ,INTBUF_TAB%STFNS,NIN        ,ISENDTO,
     3      IRCVFROM   ,IAD_ELEM,FR_ELEM  ,NSNR  ,IPARI(21,NIN),
     4      INTBUF_TAB%GAP_S,ITAB    ,KINET  ,IFQ        ,INACTI ,
     5      NSNFIOLD,INTTH,IDUM1,RDUM1,RDUM1  ,
     6      NUM_IMP  ,NODNX_SMS,RDUM1,NTY        ,IDUM1  ,
     7      RDUM1  ,RDUM1,RDUM1,RDUM1  ,IDUM1        ,ILEV,IDUM1,
     8      INTFRIC      ,IDUM1  ,ITIED, IVIS2, INTBUF_TAB%IF_ADH)
        IF (IMONM > 0 .AND. ITASK == 0) CALL STOPTIME(TIMERS,25)

c 300      continue
C
C renumerotation locale des anciens candidats
C
            CALL SPMD_RNUMCD(
     1     INTBUF_TAB%CAND_N,RENUM  ,INTBUF_TAB%I_STOK(1), NIN, NSN,
     2     NSNFIOLD    ,NSNROLD)
         END IF
       END IF
C      
       CAND_N_OLD = INTBUF_TAB%I_STOK(1)
 40    CONTINUE

       ILD = 0
       NB_N_B = 1
C
C Barrier comm spmd_tri7box + BMINMA + Retour I7BUCE
C      
 50    CALL MY_BARRIER

       IF (IMONM > 0 .AND. ITASK == 0) CALL STARTIME(TIMERS,30)
       CALL I23BUCE(
     1   X      ,INTBUF_TAB%IRECTM(1+4*ESHIFT),INTBUF_TAB%NSV  ,IPARI(22,NIN),
     2   NRTM_T ,NSN        ,INTBUF_TAB%CAND_E   ,INTBUF_TAB%CAND_N,GAP ,
     3   NOINT  ,INTBUF_TAB%I_STOK(1),TZINF       ,MAXBOX   ,MINBOX       ,
     4   NCONTACT  ,NB_N_B      ,ESHIFT      ,INTBUF_TAB%CAND_P,NCONT    ,
     6   ILD      ,WEIGHT   ,INTBUF_TAB%STFNS,NIN      ,
     7   INTBUF_TAB%STFM(1+ESHIFT) ,IPARI(21,NIN),INTBUF_TAB%GAP_S,GAPMIN,GAPMAX,
     8   IPARI(39,NIN),NUM_IMP ,ITASK,
     9   I_MEM  ,INTBUF_TAB%MSR,INTBUF_TAB%GAP_M(1+ESHIFT),NSNR   ,CURV_MAX     ,
     A   RENUM  ,NSNROLD         ,INTBUF_TAB%IFPEN,MWAG         ,BMINMA ,
     B   NMN    ,INTBUF_TAB%IRECTM,INTBUF_TAB%VARIABLES(7),
     C   INTHEAT,IDT_THERM,NODADT_THERM )

C Upgrade MultiMP
      IF (I_MEM == 2)THEN
#include "lockon.inc"
         I_MEMG = I_MEM
#include "lockoff.inc"
      ENDIF

C New barrier needed for Dynamic MultiMP
      CALL MY_BARRIER

      IF(I_MEMG /=0)THEN
C CARE : JINBUF & JBUFIN array are reallocated in
C        UPGRADE_MULTIMP routine !!!!



!$OMP SINGLE
        MULTIMP = IPARI(23,NIN) + 4
        CALL UPGRADE_MULTIMP(NIN,MULTIMP,INTBUF_TAB)
!$OMP END SINGLE
        I_MEM = 0
        I_MEMG = 0
        INTBUF_TAB%I_STOK(1)=CAND_N_OLD
        MULTIMP=IPARI(23,NIN)
        NCONTACT=MULTIMP*NCONT
C eshift : decalage sur cand_e
        GOTO 40
      ENDIF

C
      IF (IMONM > 0 .AND. ITASK == 0) CALL STOPTIME(TIMERS,30)
C
#include "lockon.inc"
         INTBUF_TAB%VARIABLES(9)  = MIN(MAXBOX,INTBUF_TAB%VARIABLES(9))
         INTBUF_TAB%VARIABLES(12) = MIN(MINBOX,INTBUF_TAB%VARIABLES(12))
         INTBUF_TAB%VARIABLES(8)  = MIN(TZINF,INTBUF_TAB%VARIABLES(8))
         INTBUF_TAB%VARIABLES(5)  = INTBUF_TAB%VARIABLES(8)-SQRT(THREE)*GAP
         RESULT        = RESULT + ILD
#include "lockoff.inc"
C--------------------------------------------------------------
C--------------------------------------------------------------
      CALL MY_BARRIER
      IF (RESULT/=0) THEN
        CALL MY_BARRIER
        IF (ITASK==0) THEN
C utile si on revient
          INTBUF_TAB%I_STOK(1) = I_SK_OLD
          RESULT = 0
        ENDIF
        CALL MY_BARRIER
        ILD  = 0
        MAXBOX = INTBUF_TAB%VARIABLES(9)
        MINBOX = INTBUF_TAB%VARIABLES(12)
        TZINF  = INTBUF_TAB%VARIABLES(8)
        GOTO 50
      ENDIF
C mise a - de dist temporairement pour reperage dans partie frontiere
      IF(NSPMD>1)THEN
C mono tache
!$OMP SINGLE
        IF (IMONM > 0) CALL STARTIME(TIMERS,26)
        INTBUF_TAB%VARIABLES(5) = -INTBUF_TAB%VARIABLES(5)
C
        CALL SPMD_TRI7GAT(
     1      RESULT       ,NSN ,INTBUF_TAB%CAND_N,INTBUF_TAB%I_STOK(1),NIN,
     2      IPARI(21,NIN),NSNR,MULTIMP      ,NTY,IPARI(47,NIN),
     3      ILEV   ,NSNFIOLD, IPARI, H3D_DATA,INTFRIC,
     4      MULTI_FVM,NODADT_THERM)
        IPARI(24,NIN) = NSNR
C
        IF (NUM_IMP>0) 
     .     CALL IMP_RNUMCD(INTBUF_TAB%CAND_N,NIN,NSN,NUM_IMP,IND_IMP )
C
        IF (IMONM > 0) CALL STOPTIME(TIMERS,26)
!$OMP END SINGLE
      END IF
C
      RETURN
      END
