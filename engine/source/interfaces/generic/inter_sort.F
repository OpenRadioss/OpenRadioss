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
!||    inter_sort              ../engine/source/interfaces/generic/inter_sort.F
!||--- called by ------------------------------------------------------
!||    inttri                  ../engine/source/interfaces/intsort/inttri.F
!||--- calls      -----------------------------------------------------
!||    int_startime            ../engine/source/system/timer_interf.F
!||    int_stoptime            ../engine/source/system/timer_interf.F
!||    inter_sort_07           ../engine/source/interfaces/int07/inter_sort_07.F
!||--- uses       -----------------------------------------------------
!||    command_line_args_mod   ../engine/share/modules/command_line_args.F
!||    glob_therm_mod          ../common_source/modules/mat_elem/glob_therm_mod.F90
!||    h3d_mod                 ../engine/share/modules/h3d_mod.F
!||    intbufdef_mod           ../common_source/modules/interfaces/intbufdef_mod.F90
!||    inter_sorting_mod       ../engine/share/modules/inter_sorting_mod.F
!||    inter_struct_mod        ../engine/share/modules/inter_struct_mod.F
!||    metric_mod              ../common_source/modules/interfaces/metric_mod.F
!||    multi_fvm_mod           ../common_source/modules/ale/multi_fvm_mod.F90
!||    sensor_mod              ../common_source/modules/sensor_mod.F90
!||    timer_mod               ../engine/source/system/timer_mod.F90
!||====================================================================
        SUBROUTINE INTER_SORT( TIMERS,ITASK,NB_INTER_SORTED,LIST_INTER_SORTED,RETRI,IPARI,
     1                         NSENSOR,ISENDTO,IRECVFROM,INTBUF_TAB,X,ITAB,
     2                         RENUM,NSNFIOLD,MULTI_FVM,H3D_DATA,SENSOR_TAB,
     3                         INTER_STRUCT,SORT_COMM,RENUM_SIZ,GLOB_THERM )
!$COMMENT
!       INTER_SORT description
!       sort computation
!       INTER_SORT organization :
!       loop over the interfaces and sort computation
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE COMMAND_LINE_ARGS_MOD , ONLY : GOT_PREVIEW
      USE MULTI_FVM_MOD
      USE H3D_MOD
      USE METRIC_MOD
      USE INTBUFDEF_MOD  
      USE INTER_STRUCT_MOD
      USE INTER_SORTING_MOD
      USE SENSOR_MOD
      USE glob_therm_mod
      USE TIMER_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(TIMER_), INTENT(inout) :: TIMERS
      INTEGER, INTENT(in) :: ITASK    !   omp thread ID
      INTEGER, INTENT(in) :: NB_INTER_SORTED        !   number of interfaces that need to be sorted
      INTEGER, INTENT(in) :: NSENSOR
      INTEGER, DIMENSION(NB_INTER_SORTED), INTENT(in) :: LIST_INTER_SORTED   !   list of interfaces that need to be sorted
      INTEGER, INTENT(inout) :: RETRI
      INTEGER, DIMENSION(NPARI,NINTER), INTENT(inout) :: IPARI    !   interface data
      INTEGER, DIMENSION(NINTER+1,NSPMD+1), INTENT(in) :: ISENDTO,IRECVFROM 
      TYPE(INTBUF_STRUCT_),DIMENSION(NINTER), INTENT(inout) :: INTBUF_TAB    ! interface data
      my_real, DIMENSION(3,NUMNOD), INTENT(in), TARGET :: X            !   position
      INTEGER, DIMENSION(NUMNOD), INTENT(in) :: ITAB
      INTEGER, INTENT(in) :: RENUM_SIZ            !   size of RENUM
      INTEGER, DIMENSION(RENUM_SIZ), INTENT(inout) :: RENUM
      INTEGER, DIMENSION(NSPMD), INTENT(inout) :: NSNFIOLD
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT), TARGET     :: MULTI_FVM
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(inter_struct_type), DIMENSION(NINTER), INTENT(inout) :: INTER_STRUCT   !   structure for interface
      TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
      type (glob_therm_) ,INTENT(IN) :: GLOB_THERM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: KK,N,ESHIFT
      INTEGER :: NTY,INACTI,ISENS
      INTEGER :: NRTM_T
      LOGICAL :: TYPE18
      my_real :: TS
!   ----------------------------------------

        DO KK=1,NB_INTER_SORTED
            N = LIST_INTER_SORTED(KK)
            NTY = IPARI(7,N)
            INACTI = IPARI(22,N)
            TYPE18=.FALSE.
            IF(NTY==7.AND.INACTI==7)TYPE18=.TRUE.

            ! ---------------------------
            ! timer of current interface N : start
            IF( IMONM > 0 .AND. ITASK ==0 ) THEN
                INTBUF_TAB(N)%METRIC%NOINT   = IPARI(15,N)
                INTBUF_TAB(N)%METRIC%NCONT   = IPARI(18,N)
                INTBUF_TAB(N)%METRIC%MULTIMP = IPARI(23,N)
                INTBUF_TAB(N)%METRIC%NSNR   = MAX(INTBUF_TAB(N)%METRIC%NSNR , IPARI(24,N))
                INTBUF_TAB(N)%METRIC%NSN    = IPARI(5,N) 
                CALL INT_STARTIME(INTBUF_TAB(N)%METRIC,I_MAIN_TRI)   
            ENDIF
            ! ---------------------------

            ISENS = 0
            IF(NTY == 7.OR.NTY == 11.OR.NTY == 24.OR.NTY == 25) ISENS = IPARI(64,N)  
            ! ---------------------------
            ! interface activated by a a sensor 
            IF (ISENS > 0) THEN    
                TS = SENSOR_TAB(ISENS)%TSTART
            ELSE
                TS = TT
            ENDIF

            ! -----------------------------------------------------
            ! type 7
            IF((NTY==7.AND.TT>=TS).AND.(.NOT.TYPE18))THEN
                IF(GOT_PREVIEW == 1) THEN
                   NRTM_T = IPARI(4,N)
                   ESHIFT = 0
                ELSE
                  NRTM_T = IPARI(4,N)/NTHREAD
                  ESHIFT = ITASK*NRTM_T
                  IF(ITASK==NTHREAD-1)NRTM_T=IPARI(4,N)-(NTHREAD-1)*NRTM_T
                ENDIF
                CALL INTER_SORT_07( TIMERS,IPARI,X,N,ITASK,ISENDTO,
     1                              IRECVFROM,RETRI,ITAB,NRTM_T,RENUM,RENUM_SIZ,
     2                              NSNFIOLD,ESHIFT,MULTI_FVM,INTBUF_TAB(N),H3D_DATA,
     3                              INTER_STRUCT,SORT_COMM,GLOB_THERM%INTHEAT, GLOB_THERM%IDT_THERM, GLOB_THERM%NODADT_THERM)
            ENDIF
            ! -----------------------------------------------------

            ! ---------------------------
            ! timer of current interface N : end
            IF(IMONM > 0) THEN
              IF(ITASK==0) CALL INT_STOPTIME(INTBUF_TAB(N)%METRIC,I_MAIN_TRI)   
            ENDIF
            ! ---------------------------
        ENDDO

        RETURN
        END SUBROUTINE INTER_SORT
