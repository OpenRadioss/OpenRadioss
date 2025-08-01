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
C  |         I10XSAVE                        /intsort/i10xsave.F
!||====================================================================
!||    i10main_tri                  ../engine/source/interfaces/intsort/i10main_tri.F
!||--- called by ------------------------------------------------------
!||    imp_tripi                    ../engine/source/implicit/imp_int_k.F
!||    inttri                       ../engine/source/interfaces/intsort/inttri.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                       ../engine/source/output/message/message.F
!||    arret                        ../engine/source/system/arret.F
!||    check_sorting_criteria       ../engine/source/interfaces/intsort/check_sorting_criteria.F90
!||    i10buce                      ../engine/source/interfaces/intsort/i10buce.F
!||    i10trc                       ../engine/source/interfaces/intsort/i10trc.F
!||    i10xsave                     ../engine/source/interfaces/intsort/i10xsave.F
!||    imp_rnumcd                   ../engine/source/implicit/imp_int_k.F
!||    my_barrier                   ../engine/source/system/machine.F
!||    spmd_rnumcd10                ../engine/source/mpi/interfaces/spmd_i7tool.F
!||    spmd_tri10box                ../engine/source/mpi/interfaces/spmd_int.F
!||    spmd_tri10gat                ../engine/source/mpi/interfaces/spmd_int.F
!||    startime                     ../engine/source/system/timer_mod.F90
!||    stoptime                     ../engine/source/system/timer_mod.F90
!||    upgrade_multimp              ../common_source/interf/upgrade_multimp.F
!||--- uses       -----------------------------------------------------
!||    check_sorting_criteria_mod   ../engine/source/interfaces/intsort/check_sorting_criteria.F90
!||    glob_therm_mod               ../common_source/modules/mat_elem/glob_therm_mod.F90
!||    h3d_mod                      ../engine/share/modules/h3d_mod.F
!||    intbufdef_mod                ../common_source/modules/interfaces/intbufdef_mod.F90
!||    message_mod                  ../engine/share/message_module/message_mod.F
!||    timer_mod                    ../engine/source/system/timer_mod.F90
!||====================================================================
      SUBROUTINE I10MAIN_TRI(TIMERS,
     1                  npari   ,IPARI   ,X       ,V       ,
     2                  MS      ,NIN     ,ITASK   ,MWAG    ,WEIGHT  ,
     3                  ISENDTO ,IRCVFROM,RETRI   ,IAD_ELEM,FR_ELEM ,
     5                  NRTM_T  ,RENUM   ,NSNFIOLD,ESHIFT  ,NUM_IMP ,
     6                  IND_IMP ,NODNX_SMS,ITAB   ,INTBUF_TAB ,
     7                  H3D_DATA,GLOB_THERM)
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TIMER_MOD
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
      USE H3D_MOD
      use check_sorting_criteria_mod , only : check_sorting_criteria
      use glob_therm_mod
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
#include      "timeri_c.inc"
      COMMON /I10MAINC/BMINMA,RESULT,NSNR,NSNROLD,I_MEMG
      INTEGER RESULT,NSNR,NSNROLD,I_MEMG
      my_real 
     .        BMINMA(6)
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TIMER_), intent(inout) :: TIMERS
      integer, intent(in) :: npari !< dimension of ipari
      INTEGER  ITASK, NIN, RETRI, NRTM_T, ESHIFT,
     .         NUM_IMP ,IND_IMP(*),
     .         IPARI(npari), MWAG(*), ITAB(*),
     .         ISENDTO(NINTER+1,*),IRCVFROM(NINTER+1,*),
     .         WEIGHT(*), IAD_ELEM(2,*) ,FR_ELEM(*),
     .         RENUM(NUMNOD), NSNFIOLD(NSPMD), NODNX_SMS(*)
      my_real
     .   X(*), V(3,*), MS(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(glob_therm_),INTENT(IN)  :: GLOB_THERM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NB_N_B,
     .   I, K11_T, IP0, IP1, IP2, IP3, JLT , NFT, J17_T,
     .   I_SK_OLD, I_STOK1, ITIED,
     .   ADD1, ILD, NOINT, MULTIMP, ITYP, NCONT, NCONTACT,
     .   IBID,I_MEM,CAND_N_OLD
C     REAL
      my_real
     .    GAP, MAXBOX, MINBOX, TZINF,DIST1,
     .   XMAXL, YMAXL, ZMAXL, XMINL, YMINL, ZMINL, GAPMIN, GAPMAX
      INTEGER :: NMN, NSN,NRTM,NTY
      logical :: need_computation
C-----------------------------------------------

      ! --------------
      ! check if the current interface needs to be sorted
      call check_sorting_criteria( need_computation,nin,npari,nspmd,  
     .                             itask,ipari,tt,intbuf_tab )
      if( .not.need_computation ) return
      ! --------------

      I_MEM = 0
      I_MEMG = 0

      NRTM   =IPARI(4)
      NSN    =IPARI(5)
      NMN    =IPARI(6)
      NTY    =IPARI(7)
      NOINT  =IPARI(15)
      MULTIMP=IPARI(23)
      NCONT  =IPARI(18)
      ITIED = IPARI(85)
      NCONTACT=MULTIMP*NCONT
      NSNROLD = IPARI(24)

      GAP   =INTBUF_TAB%VARIABLES(2)
      GAPMIN=INTBUF_TAB%VARIABLES(13)
      GAPMAX=INTBUF_TAB%VARIABLES(16)
C
C -------------------------------------------------------------
C
      RETRI=1
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
C -------------------------------------------------------------
C     TRI SUR N DES ANCIENS CANDIDATS 
C -------------------------------------------------------------
C
      CALL MY_BARRIER
      IF(ITASK==0)THEN
        IP0 = 1
        IP1 = IP0 + NSN + NSNROLD + 3
        I_SK_OLD = INTBUF_TAB%I_STOK(1)
        CALL I10TRC(
     1     NSN+NSNROLD  ,I_SK_OLD ,INTBUF_TAB%CAND_N,INTBUF_TAB%CAND_E,
     2     INTBUF_TAB%CAND_F,MWAG(IP0),NUM_IMP      ,IND_IMP      )
        INTBUF_TAB%I_STOK(1)=I_SK_OLD
      ENDIF
C -------------------------------------------------------------
C     CALCUL BORNE DOMAINE REMONTE DANS I7XSAVE
C -------------------------------------------------------------
C sauvegarde de XSAV (tableau BUFIN(JD(19)))
      CALL I10XSAVE(
     1       X     ,INTBUF_TAB%NSV,INTBUF_TAB%MSR,NSN   ,NMN   ,
     2       ITASK ,INTBUF_TAB%XSAV,XMINL        ,YMINL ,ZMINL ,
     3       XMAXL ,YMAXL        ,ZMAXL        )
#include "lockon.inc"
      BMINMA(1) = MAX(BMINMA(1),XMAXL)
      BMINMA(2) = MAX(BMINMA(2),YMAXL)
      BMINMA(3) = MAX(BMINMA(3),ZMAXL)
      BMINMA(4) = MIN(BMINMA(4),XMINL)
      BMINMA(5) = MIN(BMINMA(5),YMINL)
      BMINMA(6) = MIN(BMINMA(6),ZMINL)
#include "lockoff.inc"
      RESULT = 0
C BARRIER II_STOK et RESULT
      CALL MY_BARRIER
C -------------------------------------------------------------
      IF(ITASK==0)THEN
        IF(ABS(BMINMA(6)-BMINMA(3))>2*EP30.OR.
     +     ABS(BMINMA(5)-BMINMA(2))>2*EP30.OR.
     +     ABS(BMINMA(4)-BMINMA(1))>2*EP30)THEN
          CALL ANCMSG(MSGID=87,ANMODE=ANINFO,
     .                I1=NOINT)
          CALL ARRET(2)
        END IF
C
        BMINMA(1)=BMINMA(1)+TZINF
        BMINMA(2)=BMINMA(2)+TZINF
        BMINMA(3)=BMINMA(3)+TZINF
        BMINMA(4)=BMINMA(4)-TZINF
        BMINMA(5)=BMINMA(5)-TZINF
        BMINMA(6)=BMINMA(6)-TZINF
C
C recuperation des noeuds remote NSNR stockes dans XREM
C
        NSNR = 0
        IF(NSPMD>1) THEN
          CALL SPMD_TRI10BOX(
     1      INTBUF_TAB%NSV,NSN     ,X            ,V      ,MS       ,
     2      BMINMA       ,WEIGHT  ,INTBUF_TAB%STFNS,NIN    ,ISENDTO  ,
     3      IRCVFROM     ,IAD_ELEM,FR_ELEM      ,NSNR   ,IPARI(21),
     4      INTBUF_TAB%GAP_S,NSNFIOLD,NODNX_SMS    ,ITAB   ,ITIED)
C
C renumerotation locale des anciens candidats
C

            CALL SPMD_RNUMCD10(
     1        INTBUF_TAB%CAND_N,RENUM  ,INTBUF_TAB%I_STOK(1), NIN, NSN,
     2        NSNFIOLD     ,NSNROLD)
        END IF
      END IF
C
      CAND_N_OLD = INTBUF_TAB%I_STOK(1)
 40   CONTINUE
C eshift : decalage sur cand_e
      ILD = 0
      NB_N_B = 1
C
C Barrier comm spmd_tri10box + BMINMA + Retour I10BUCE
C      
 50   CALL MY_BARRIER
C
      IF (IMONM > 0) CALL STARTIME(TIMERS,30)
C
      IF(NRTM_T/=0)
     .   CALL I10BUCE(
     1   X        ,INTBUF_TAB%IRECTM(1+4*ESHIFT),INTBUF_TAB%NSV,NMN    ,NRTM_T  ,
     2   NSN      ,NCONT       ,INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,GAP      ,
     3   NOINT    ,INTBUF_TAB%I_STOK(1),TZINF     ,MAXBOX   ,MINBOX       ,
     4   NB_N_B   ,ESHIFT      ,BMINMA       ,MWAG     ,ILD          ,
     7   NCONTACT ,NSNROLD     ,INTBUF_TAB%STFNS,NIN      ,IPARI(21)    ,
     8   INTBUF_TAB%GAP_S,NSNR    ,RENUM     ,INTBUF_TAB%STFM(1+ESHIFT),INTBUF_TAB%GAP_M,
     9   GAPMIN   ,GAPMAX ,I_MEM,GLOB_THERM%INTHEAT, GLOB_THERM%IDT_THERM, GLOB_THERM%NODADT_THERM)
C
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
        MULTIMP = IPARI(23) + 4
        CALL UPGRADE_MULTIMP(NIN,MULTIMP,INTBUF_TAB)
!$OMP END SINGLE
        I_MEM = 0
        I_MEMG = 0
        INTBUF_TAB%I_STOK(1)=CAND_N_OLD
        MULTIMP=IPARI(23)
        NCONTACT=MULTIMP*NCONT
        GOTO 40
      ENDIF
      IF (IMONM > 0) CALL STOPTIME(TIMERS,30)
C
#include "lockon.inc"
      INTBUF_TAB%VARIABLES(9) = MIN(MAXBOX,INTBUF_TAB%VARIABLES(9))
      INTBUF_TAB%VARIABLES(12)= MIN(MINBOX,INTBUF_TAB%VARIABLES(12))
      INTBUF_TAB%VARIABLES(8)  = MIN(TZINF,INTBUF_TAB%VARIABLES(8))
C     TZINF mis a jour a ce niveau
      INTBUF_TAB%VARIABLES(5)  = INTBUF_TAB%VARIABLES(8)-GAP
      RESULT        = RESULT + ILD           
#include "lockoff.inc"
C--------------------------------------------------------------
C--------------------------------------------------------------
      CALL MY_BARRIER
      IF (RESULT/=0) THEN
        CALL MY_BARRIER
        IF (ITASK==0) THEN
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
        IBID = 0
c        CALL SPMD_TRI10GAT(
c     1    RESULT   ,NSN ,INBUF(KD(15)),INBUF(KD(1)),NIN,
c     2    IPARI(21),NSNR,MULTIMP      ,NTY,IBID)
        CALL SPMD_TRI10GAT(
     1    RESULT   ,NSN ,INTBUF_TAB%CAND_N,INTBUF_TAB%I_STOK(1),NIN,
     2    IPARI(21),NSNR,MULTIMP      ,NTY,IBID,H3D_DATA)
C sauvegarde des candidats additionnels dans IPARI(24)
        IPARI(24) = NSNR
        IF (NUM_IMP>0) 
     .    CALL IMP_RNUMCD(INTBUF_TAB%CAND_N,NIN,NSN,NUM_IMP,IND_IMP )

        IF (IMONM > 0) CALL STOPTIME(TIMERS,26)

!$OMP END SINGLE

      ENDIF
C
      RETURN
      END
