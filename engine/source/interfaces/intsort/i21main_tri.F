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
!||    i21main_tri                  ../engine/source/interfaces/intsort/i21main_tri.F
!||--- called by ------------------------------------------------------
!||    inttri                       ../engine/source/interfaces/intsort/inttri.F
!||--- calls      -----------------------------------------------------
!||    check_sorting_criteria       ../engine/source/interfaces/intsort/check_sorting_criteria.F90
!||    i21buce                      ../engine/source/interfaces/intsort/i21buce.F
!||    i21fpen                      ../engine/source/interfaces/int21/i21fpen.F
!||    i21xsave                     ../engine/source/interfaces/int21/i21xsave.F
!||    my_barrier                   ../engine/source/system/machine.F
!||    startime                     ../engine/source/system/timer_mod.F90
!||    stoptime                     ../engine/source/system/timer_mod.F90
!||    update_struct_int21          ../engine/source/interfaces/int21/update_struct_int21.F
!||    upgrade_multimp              ../common_source/interf/upgrade_multimp.F
!||--- uses       -----------------------------------------------------
!||    check_sorting_criteria_mod   ../engine/source/interfaces/intsort/check_sorting_criteria.F90
!||    intbufdef_mod                ../common_source/modules/interfaces/intbufdef_mod.F90
!||    intstamp_mod                 ../engine/share/modules/intstamp_mod.F
!||    timer_mod                    ../engine/source/system/timer_mod.F90
!||====================================================================
      SUBROUTINE I21MAIN_TRI(TIMERS,
     1                  IPARI   ,X       ,NIN     ,
     2                  ITASK   ,WEIGHT  ,RETRI   ,NUM_IMP ,IND_IMP ,
     3                  INTSTAMP,MWAG    ,INTBUF_TAB, nspmd)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TIMER_MOD
      USE INTSTAMP_MOD
      USE INTBUFDEF_MOD 
      use check_sorting_criteria_mod , only : check_sorting_criteria
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
      COMMON /I21MAINC/RESULT21,I_MEMG
      INTEGER RESULT21,I_MEMG
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TIMER_) :: TIMERS
      integer, intent(in) :: nspmd !< number of processor
      INTEGER NIN ,ITASK, RETRI, NUM_IMP ,IND_IMP(*),
     .        IPARI(NPARI,NINTER),
     .        WEIGHT(*), MWAG(*)
      TYPE(INTSTAMP_DATA) INTSTAMP
C     REAL
      my_real 
     .    X(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J17_T,
     .        I, IP0, IP1, IP2, IP21, K11_T, I_SK_OLD, 
     .        ADD1, NB_N_B, INIT, NOINT, INACTI, MULTIMP, IGAP, I_STOK,
     .        INTTH , ADXLOC
      INTEGER 
     .       NRTM_T,ESHIFT,ILD, NCONTACT, NRTM_L,
     .       I_MEM,CAND_N_OLD, IROT
C     REAL
      my_real
     .   GAP,MAXBOX,MINBOX,TZINF,
     .   XMAX, YMAX, ZMAX, XMIN, YMIN, ZMIN, GAPMIN, GAPMAX,
     .   DD(3)
      INTEGER :: NRTM,NSN,NTY,NMN
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
C
      NRTM   =IPARI(4,NIN)
      NSN    =IPARI(5,NIN)
      NTY    =IPARI(7,NIN)
      NOINT  =IPARI(15,NIN)
      NMN    =IPARI(8,NIN)
      INTTH = IPARI(47,NIN)

      INACTI =IPARI(22,NIN)
      MULTIMP=IPARI(23,NIN)

      NCONTACT=MULTIMP*NSN
C
      GAP =INTBUF_TAB%VARIABLES(2)
      GAPMIN=INTBUF_TAB%VARIABLES(13)
      GAPMAX=INTBUF_TAB%VARIABLES(16)
C
      IF(NSN > 0) THEN
         ADXLOC = 1+3*(NSN+2)
      ELSE
         ADXLOC = 1
      ENDIF
C
C -------------------------------------------------------------
C
       RETRI=1
C
       DD(1)=INTSTAMP%D(1)
       DD(2)=INTSTAMP%D(2)
       DD(3)=INTSTAMP%D(3)
C sauvegarde de XSAV (tableau BUFIN(JD(19)))
       IROT=INTSTAMP%IROT
       CALL I21XSAVE(
     1       X     ,INTBUF_TAB%NSV,NSN ,ITASK ,INTBUF_TAB%XSAV,
     2       DD    ,IROT         ,INTSTAMP%ROT,INTSTAMP%BRACKET)
C -------------------------------------------------------------
C
       MAXBOX = INTBUF_TAB%VARIABLES(9)
       MINBOX = INTBUF_TAB%VARIABLES(12)
       TZINF  = INTBUF_TAB%VARIABLES(8)
C
C -------------------------------------------------------------
       I_SK_OLD=0
       INTBUF_TAB%I_STOK(1)=ZERO
C BARRIER II_STOK et RESULT
       RESULT21 = 0
C      
      CAND_N_OLD = INTBUF_TAB%I_STOK(1)
 40    CONTINUE
       CALL MY_BARRIER
C      
       INACTI = IPARI(22,NIN)
C -------------------------------------------------------------
       NRTM_T=NRTM/NTHREAD
C eshift : decalage sur cand_e
       ESHIFT = ITASK*NRTM_T
C il faut conserver K11 global

       K11_T = 1+4*ITASK*NRTM_T

       J17_T = 1+ITASK*NRTM_T
       IF(ITASK==NTHREAD-1)NRTM_T=NRTM-(NTHREAD-1)*(NRTM/NTHREAD)
         INIT = 1
         ILD = 0
 50      CONTINUE
C
         IF (IMONM > 0) CALL STARTIME(TIMERS,30)
         IF(NRTM_T/=0) THEN
            CALL I21BUCE(
     1   X      ,INTBUF_TAB%IRECTM(K11_T),INTBUF_TAB%NSV  ,IPARI(22,NIN),
     2   NRTM_T ,NSN        ,INTBUF_TAB%CAND_E    ,INTBUF_TAB%CAND_N ,GAP,
     3   NOINT  ,INTBUF_TAB%I_STOK(1) ,TZINF       ,MAXBOX   ,MINBOX       ,
     4   NCONTACT  ,XMIN       ,XMAX         ,YMIN     ,
     5   YMAX           ,ZMIN      ,ZMAX     ,NB_N_B       ,ESHIFT   ,
     6   ILD            ,INIT      ,WEIGHT   ,INTBUF_TAB%STFNS ,NIN      ,
     7   INTBUF_TAB%STF(J17_T) ,IPARI(21,NIN),INTBUF_TAB%GAP_S,GAPMIN,GAPMAX       ,
     8   IPARI(39,NIN),NUM_IMP ,INTBUF_TAB%XM0  ,INTBUF_TAB%NOD_NORMAL  ,
     9   INTBUF_TAB%VARIABLES(23)  ,INTBUF_TAB%VARIABLES(22)   ,INTBUF_TAB%VARIABLES(27) ,
     .                             INTBUF_TAB%VARIABLES(28)   ,INTBUF_TAB%VARIABLES(29)  ,
     A   NRTM_L       ,INTBUF_TAB%XSAV(ADXLOC),I_MEM,INTBUF_TAB%VARIABLES(32) ,
     .                                               NMN             ,
     B  INTTH         ,INTBUF_TAB%MNDD ,INTBUF_TAB%MSR_L ,ITASK,INTBUF_TAB%IRECTM,
     C  IPARI(48,NIN) ,INTBUF_TAB%VARIABLES(46))

        ENDIF

C Upgrade MultiMP
      IF (I_MEM == 2)THEN
#include "lockon.inc"
         I_MEMG = I_MEM
#include "lockoff.inc"
      ENDIF
C New barrier needed for Dynamic MultiMP
      CALL MY_BARRIER

      IF(I_MEMG /=0)THEN
!$OMP SINGLE
         MULTIMP = IPARI(23,NIN) + 4
         CALL UPGRADE_MULTIMP(NIN,MULTIMP,INTBUF_TAB)
!$OMP END SINGLE
        I_MEMG = 0
        I_MEM = 0
        INTBUF_TAB%I_STOK(1) =CAND_N_OLD
        MULTIMP=IPARI(23,NIN)
        NCONTACT=MULTIMP*NSN
        GOTO 40
      ENDIF

      CALL UPDATE_STRUCT_INT21(
     1            INTBUF_TAB%CAND_E, INTBUF_TAB%I_STOK(1), IPARI(48,NIN) , NIN   , NMN,
     2            INTTH, INTBUF_TAB%MNDD, INTBUF_TAB%MSR_L , INTBUF_TAB%IRECTM )

         IF (IMONM > 0) CALL STOPTIME(TIMERS,30)
C
#include "lockon.inc"
         INTBUF_TAB%VARIABLES(9)   = MIN(MAXBOX,INTBUF_TAB%VARIABLES(9))
         INTBUF_TAB%VARIABLES(12) = MIN(MINBOX,INTBUF_TAB%VARIABLES(12))
         INTBUF_TAB%VARIABLES(8)  = MIN(TZINF,INTBUF_TAB%VARIABLES(8))
c +1 en 21 :
         INTBUF_TAB%VARIABLES(5)  = EP30
         RESULT21        = RESULT21 + ILD
#include "lockoff.inc"
C--------------------------------------------------------------
C--------------------------------------------------------------
         CALL MY_BARRIER
         IF (RESULT21/=0) THEN
          CALL MY_BARRIER
          IF (ITASK==0) THEN
            INTBUF_TAB%I_STOK(1) = I_SK_OLD
            RESULT21 = 0
          ENDIF
          CALL MY_BARRIER
          ILD  = 0
          INIT = 0
          MAXBOX = INTBUF_TAB%VARIABLES(9)
          MINBOX = INTBUF_TAB%VARIABLES(12)
          TZINF  = INTBUF_TAB%VARIABLES(8)

          CALL MY_BARRIER
          GOTO 50
         ENDIF
C-----------------------------------------------------------
C     Remise a 0 de IFPEN pour les nds qui ne sont plus candidats
C--------------------------------------------------------------
         IF(ITASK==0)THEN
           IP0 = 1
           IP1 = IP0 + NSN + 3
C MWA = MWAG SUR TASK 0
           I_STOK = INTBUF_TAB%I_STOK(1)
           CALL I21FPEN(
     1       NSN          ,I_STOK       ,INTBUF_TAB%CAND_N,INTBUF_TAB%CAND_E,
     .                                                 INTBUF_TAB%PENIS,
     2       INTBUF_TAB%FTSAVX,INTBUF_TAB%FTSAVY,INTBUF_TAB%FTSAVZ,MWAG(IP0)    ,
     .                                                 INTBUF_TAB%IFPEN,
     3       INACTI)
         ENDIF
C--------------------------------------------------------------

C
      RETURN
      END
