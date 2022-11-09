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
Chd|  INTER_SORT                    source/interfaces/generic/inter_sort.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        INTER_SORT_07                 source/interfaces/int07/inter_sort_07.F
Chd|        INT_STARTIME                  source/system/timer_interf.F  
Chd|        INT_STOPTIME                  source/system/timer_interf.F  
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTER_SORTING_MOD             share/modules/inter_sorting_mod.F
Chd|        INTER_STRUCT_MOD              share/modules/inter_struct_mod.F
Chd|        METRIC_MOD                    ../common_source/modules/metric_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
        SUBROUTINE INTER_SORT( ITASK,NB_INTER_SORTED,LIST_INTER_SORTED,RETRI,IPARI,
     1                         ISENDTO,IRECVFROM,INTBUF_TAB,X,ITAB,
     2                         RENUM,NSNFIOLD,MULTI_FVM,H3D_DATA,SENSOR_TAB,
     3                         INTER_STRUCT,SORT_COMM,RENUM_SIZ )
!$COMMENT
!       INTER_SORT description
!       sort computation
!       INTER_SORT organization :
!       loop over the interfaces and sort computation
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MULTI_FVM_MOD
      USE H3D_MOD
      USE METRIC_MOD
      USE INTBUFDEF_MOD  
      USE INTER_STRUCT_MOD
      USE INTER_SORTING_MOD
      USE SENSOR_MOD
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
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "warn_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(in) :: ITASK    !   omp thread ID
      INTEGER, INTENT(in) :: NB_INTER_SORTED        !   number of interfaces that need to be sorted
      INTEGER, DIMENSION(NB_INTER_SORTED), INTENT(in) :: LIST_INTER_SORTED   !   list of interfaces that need to be sorted
      INTEGER, INTENT(inout) :: RETRI
      INTEGER, DIMENSION(NPARI,NINTER), INTENT(in) :: IPARI    !   interface data
      INTEGER, DIMENSION(NINTER+1,NSPMD+1), INTENT(in) :: ISENDTO,IRECVFROM 
      TYPE(INTBUF_STRUCT_),DIMENSION(NINTER), INTENT(inout) :: INTBUF_TAB    ! interface data
      my_real, DIMENSION(3,NUMNOD), INTENT(in), TARGET :: X            !   position
      INTEGER, DIMENSION(NUMNOD), INTENT(in) :: ITAB
      INTEGER, INTENT(in) :: RENUM_SIZ            !   size of RENUM
      INTEGER, DIMENSION(RENUM_SIZ), INTENT(inout) :: RENUM
      INTEGER, DIMENSION(NSPMD), INTENT(inout) :: NSNFIOLD
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT), TARGET     :: MULTI_FVM
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(inter_struct_type), DIMENSION(NINTER), INTENT(inout) :: INTER_STRUCT   !   struture for interface
      TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
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
            ! ---------------------------

            ! -----------------------------------------------------
            ! type 7
            IF((NTY==7.AND.TT>=TS).AND.(.NOT.TYPE18))THEN
                NRTM_T = IPARI(4,N)/NTHREAD
                ESHIFT = ITASK*NRTM_T
                IF(ITASK==NTHREAD-1)NRTM_T=IPARI(4,N)-(NTHREAD-1)*NRTM_T
                CALL INTER_SORT_07( IPARI,X,N,ITASK,ISENDTO,
     1                              IRECVFROM,RETRI,ITAB,NRTM_T,RENUM,RENUM_SIZ,
     2                              NSNFIOLD,ESHIFT,MULTI_FVM,INTBUF_TAB(N),H3D_DATA,
     3                              INTER_STRUCT,SORT_COMM)
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
