Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  INTER_SORT_07                 source/interfaces/int07/inter_sort_07.F
Chd|-- called by -----------
Chd|        INTER_SORT                    source/interfaces/generic/inter_sort.F
Chd|-- calls ---------------
Chd|        I7BUCE                        source/interfaces/intsort/i7buce.F
Chd|        I7BUCE_VOX                    source/interfaces/intsort/i7buce.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_CELL_EXCHANGE            source/mpi/generic/spmd_cell_exchange.F
Chd|        SPMD_RNUMCD                   source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_TRI7GAT                  source/mpi/interfaces/spmd_int.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        UPGRADE_MULTIMP               ../common_source/interf/upgrade_multimp.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTER_SORTING_MOD             share/modules/inter_sorting_mod.F
Chd|        INTER_STRUCT_MOD              share/modules/inter_struct_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
        SUBROUTINE INTER_SORT_07( IPARI,X,NIN,ITASK,ISENDTO,
     1                            IRCVFROM,RETRI,ITAB,NRTM_T,RENUM,RENUM_SIZ,
     2                            NSNFIOLD,ESHIFT,MULTI_FVM,INTBUF_TAB,H3D_DATA,
     3                            INTER_STRUCT,SORT_COMM)
!$COMMENT
!       INTER_SORT_07 description
!       sort computation for interface TYP07
!       INTER_SORT_07 organization :
!       
!$ENDCOMMENT
C============================================================================
C   M o d u l e s
C-----------------------------------------------
        USE TRI7BOX
        USE MESSAGE_MOD
        USE INTBUFDEF_MOD
        USE H3D_MOD
        USE MULTI_FVM_MOD
        USE INTER_STRUCT_MOD
        USE INTER_SORTING_MOD
        USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#ifdef MPI
#include      "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
C common pour variable globale en memoire partagee
        COMMON /I7MAINC/CURV_MAX_MAX,RESULT,NSNR,NSNROLD,I_MEMG,NMN_G
        INTEGER RESULT,NSNR,NSNROLD,I_MEMG,NMN_G
        my_real :: CURV_MAX_MAX
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: RENUM_SIZ    ! size of RENUM
        INTEGER, INTENT(in) :: NIN ,ITASK,NRTM_T,ESHIFT
        INTEGER, INTENT(inout) :: RETRI
        INTEGER, DIMENSION(NUMNOD), INTENT(inout) :: ITAB
        INTEGER, DIMENSION(NPARI,NINTER),INTENT(inout) :: IPARI
        INTEGER, DIMENSION(NINTER+1,NSPMD+1),INTENT(inout) :: ISENDTO,IRCVFROM
        INTEGER, DIMENSION(RENUM_SIZ), INTENT(inout) :: RENUM
        INTEGER, DIMENSION(NSPMD), INTENT(inout) :: NSNFIOLD
        my_real, DIMENSION(3*NUMNOD), INTENT(in) :: X   ! position

        TYPE(INTBUF_STRUCT_) INTBUF_TAB
        TYPE(H3D_DATABASE) :: H3D_DATA
        TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
        TYPE(inter_struct_type), DIMENSION(NINTER), INTENT(inout) :: INTER_STRUCT   !   structure for interface
        TYPE(sorting_comm_type), DIMENSION(NINTER), INTENT(inout) :: SORT_COMM   ! structure for interface sorting comm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER 
     .        LOC_PROC,
     .        I, IP0, IP1, IP2, IP21, I_SK_OLD, I_STOK1, 
     .        ADD1, NB_N_B, NOINT, INACTI, MULTIMP, IGAP, IFQ, ITIED
        INTEGER 
     .        ILD, NCONT, NCONTACT, INACTII, INACIMP, INTTH,
     .        I_MEM,CAND_N_OLD,IDUM1(1),NMN_L, IVIS2,NUM_IMP
        my_real
     .   STARTT,GAP,MAXBOX,MINBOX,STOPT,DIST,TZINF,
     .   XMAXL, YMAXL, ZMAXL, XMINL, YMINL, ZMINL, GAPMIN, GAPMAX,
     .   C_MAXL,DRAD,MX,MY,MZ,DX,DY,DZ,SX,SY,SZ,SX2,SY2,SZ2,
     .   CURV_MAX(NRTM_T),RDUM1(1)
        my_real :: DGAPLOADP
        REAL T1  !elapsed time in smp
        LOGICAL TYPE18
        INTEGER :: FIRST, LAST,ijk
        INTEGER :: NSN,NMN,NTY,NRTM
C-----------------------------------------------
        NUM_IMP = 0

        I_MEM  = 0
        I_MEMG = 0
        NMN_G  = 0
        NMN_L  = 0
C
        NRTM    =IPARI(4,NIN)
        NSN     =IPARI(5,NIN)
        NMN     =IPARI(6,NIN)
        NTY     =IPARI(7,NIN)
        IVIS2   =IPARI(14,NIN)
        NOINT   =IPARI(15,NIN)
        NCONT   =IPARI(18,NIN)
        INACTI  =IPARI(22,NIN)
        MULTIMP =IPARI(23,NIN)
        IFQ     =IPARI(31,NIN)
        INTTH   =IPARI(47,NIN)
        ITIED   =IPARI(85,NIN)
      
        LOC_PROC=ISPMD+1      
        NCONTACT=MULTIMP*NCONT
      
        TYPE18=.FALSE.
        IF(NTY==7 .AND. INACTI==7)TYPE18=.TRUE.

        IF(INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.IFQ>0.OR.
     .     ITIED/=0)THEN
            NSNROLD = IPARI(24,NIN)
        ELSE
            NSNROLD = 0
        ENDIF

        STARTT=INTBUF_TAB%VARIABLES(3)
        STOPT =INTBUF_TAB%VARIABLES(11)

        IF(STARTT>TT) RETURN
        IF(TT>STOPT)  RETURN

        GAP    =INTBUF_TAB%VARIABLES(2)
        GAPMIN=INTBUF_TAB%VARIABLES(13)
        GAPMAX=INTBUF_TAB%VARIABLES(16)
        DRAD = ZERO
        IF(IPARI(7,NIN)==7) DRAD =INTBUF_TAB%VARIABLES(32)
        DGAPLOADP= INTBUF_TAB%VARIABLES(46)
C -------------------------------------------------------------
        DIST = INTBUF_TAB%VARIABLES(5)
        IF (DIST>ZERO) RETURN
        RETRI = 1
C -------------------------------------------------------------
        MAXBOX = INTBUF_TAB%VARIABLES(9)
        MINBOX = INTBUF_TAB%VARIABLES(12)
        TZINF  = INTBUF_TAB%VARIABLES(8)
        CURV_MAX_MAX = ZERO

        CALL MY_BARRIER

        I_SK_OLD = INTER_STRUCT(NIN)%I_SK_OLD
!$OMP SINGLE
        NSNR = 0
        CURV_MAX_MAX = INTER_STRUCT(NIN)%CURV_MAX_MAX
        RESULT = 0
        NMN_G = INTER_STRUCT(NIN)%NMN_G
!$OMP END SINGLE
        FIRST = 1 + ITASK*(NRTM/NTHREAD)
        LAST = FIRST + NRTM_T - 1
        IF(ITASK==NTHREAD-1) LAST=NRTM
        CURV_MAX(1:NRTM_T) = INTER_STRUCT(NIN)%CURV_MAX(FIRST:LAST)

        IF(NSPMD > 1) THEN
        ! ---------------------------
            IF(ITASK==0)THEN
            ! ---------------------------
            ! send/rcv the secondary node data
                CALL SPMD_CELL_EXCHANGE( NIN,ISENDTO,IRCVFROM,NSNR,IPARI(21,NIN),
     1                                   IFQ,INACTI,NSNFIOLD,IPARI(47,NIN),NTY,
     2                                   ITIED,NMN,INTER_STRUCT,SORT_COMM )
C
                IF(INACTI==5.OR.INACTI==6.OR.INACTI==7.OR.
     +               IFQ>0.OR.ITIED/=0)THEN
                    CALL SPMD_RNUMCD(
     1                  INTBUF_TAB%CAND_N,RENUM  ,INTBUF_TAB%I_STOK(1), NIN,NSN,
     2                NSNFIOLD     ,NSNROLD)
                END IF
            ! ---------------------------
            ENDIF
        END IF
        ! ---------------------------
        CAND_N_OLD = INTBUF_TAB%I_STOK(1)
 40     CONTINUE
C
        ILD = 0
        NB_N_B = 1

 50     CALL MY_BARRIER    
        IF(ITASK==0) THEN
            IF(ALLOCATED( LIST_REMOTE_S_NODE ) ) DEALLOCATE( LIST_REMOTE_S_NODE )
            ALLOCATE( LIST_REMOTE_S_NODE(NSNR) )
            REMOTE_S_NODE = 0
        ENDIF
        CALL MY_BARRIER                          

        IF(IPARI(63,NIN) ==2 ) INTBUF_TAB%METRIC%ALGO = ALGO_VOXEL 
 
#ifdef MPI
        IF(ITASK == 0) INTBUF_TAB%METRIC%TIC = MPI_WTIME()
#else
        IF(ITASK == 0) THEN
            CALL CPU_TIME(T1)
            INTBUF_TAB%METRIC%TIC = NINT(100.0 * T1) 
        ENDIF
#endif
        IF (IMONM > 0 .AND. ITASK == 0) CALL STARTIME(30,1)
C
        IF(INTBUF_TAB%METRIC%ALGO == ALGO_VOXEL .OR.  INTBUF_TAB%METRIC%ALGO == TRY_ALGO_VOXEL) THEN
            CALL I7BUCE_VOX(
     1      X      ,INTBUF_TAB%IRECTM(1+4*ESHIFT),INTBUF_TAB%NSV  ,INACTI    ,INTBUF_TAB%CAND_P,
     2      NMN_G  ,NRTM_T                       ,NSN     ,INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,
     3      GAP    ,NOINT       ,INTBUF_TAB%I_STOK(1) ,NCONTACT ,INTER_STRUCT(NIN)%BOX_LIMIT_MAIN ,
     4      TZINF  ,MAXBOX      ,MINBOX         ,INTER_STRUCT(NIN)%CAND_A         ,CURV_MAX  ,
     6      NB_N_B ,ESHIFT      ,ILD            ,IFQ    ,INTBUF_TAB%IFPEN,
     8      INTBUF_TAB%STFNS,NIN ,INTBUF_TAB%STFM(1+ESHIFT),IPARI(21,NIN),INTBUF_TAB%GAP_S,
     A      NSNR   ,NCONT  ,RENUM  ,NSNROLD  ,INTBUF_TAB%GAP_M(1+ESHIFT),
     B      GAPMIN ,GAPMAX      ,CURV_MAX_MAX   ,NUM_IMP   ,INTBUF_TAB%GAP_SL,
     C      INTBUF_TAB%GAP_ML(1+ESHIFT),INTTH ,ITASK  , INTBUF_TAB%VARIABLES(7),I_MEM     ,  
     D      INTBUF_TAB%KREMNODE(1+2*ESHIFT),INTBUF_TAB%REMNODE,ITAB , IPARI(63,NIN),DRAD         ,
     E      ITIED ,INTBUF_TAB%CAND_F,DGAPLOADP,REMOTE_S_NODE,LIST_REMOTE_S_NODE,
     F      NRTM)

        ELSE
            CALL I7BUCE(
     1 X      ,INTBUF_TAB%IRECTM(1+4*ESHIFT),INTBUF_TAB%NSV  ,INACTI    ,INTBUF_TAB%CAND_P,
     2 NMN_G  ,NRTM_T                       ,NSN     ,INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,
     3 GAP    ,NOINT       ,INTBUF_TAB%I_STOK(1) ,NCONTACT ,INTER_STRUCT(NIN)%BOX_LIMIT_MAIN ,
     4 TZINF  ,MAXBOX      ,MINBOX         ,INTER_STRUCT(NIN)%CAND_A         ,CURV_MAX  ,
     6 NB_N_B ,ESHIFT      ,ILD            ,IFQ    ,INTBUF_TAB%IFPEN,
     8 INTBUF_TAB%STFNS,NIN ,INTBUF_TAB%STFM(1+ESHIFT),IPARI(21,NIN),INTBUF_TAB%GAP_S,
     A NSNR   ,NCONT  ,RENUM  ,NSNROLD  ,INTBUF_TAB%GAP_M(1+ESHIFT),
     B GAPMIN ,GAPMAX      ,CURV_MAX_MAX   ,NUM_IMP   ,INTBUF_TAB%GAP_SL,
     C INTBUF_TAB%GAP_ML(1+ESHIFT),INTTH ,ITASK  , INTBUF_TAB%VARIABLES(7),I_MEM     ,  
     D INTBUF_TAB%KREMNODE(1+2*ESHIFT),INTBUF_TAB%REMNODE,ITAB , IPARI(63,NIN),DRAD         ,
     E ITIED ,INTBUF_TAB%CAND_F,DGAPLOADP)

        ENDIF
      
        IF (I_MEM >= 1 )THEN
#include "lockon.inc"
            I_MEMG = I_MEM
#include "lockoff.inc"
        ENDIF

C New barrier needed for Dynamic MultiMP
        CALL MY_BARRIER

#ifdef MPI
        IF(ITASK == 0 ) INTBUF_TAB%METRIC%TOC = MPI_WTIME()
#else
        IF(ITASK == 0) THEN
            CALL CPU_TIME(T1)
            INTBUF_TAB%METRIC%TOC = NINT(100.0 * T1) 
        ENDIF
#endif


        IF(I_MEMG /=0)THEN
            IF(I_MEMG == 3 .OR. I_MEMG == 1) INTBUF_TAB%METRIC%ALGO = ALGO_VOXEL
C CARE : JINBUF & JBUFIN array are reallocated in
C        UPGRADE_MULTIMP routine !!!!

!$OMP SINGLE
            MULTIMP = IPARI(23,NIN) + 4
            CALL UPGRADE_MULTIMP(NIN,MULTIMP,INTBUF_TAB)
!$OMP END SINGLE
            I_MEM = 0
            I_MEMG = 0
            INTBUF_TAB%I_STOK(1) = CAND_N_OLD
            MULTIMP=IPARI(23,NIN)
            NCONTACT=MULTIMP*NCONT
            GOTO 40
        ENDIF

C
        IF (IMONM > 0 .AND. ITASK == 0) CALL STOPTIME(30,1)
        IF( ITASK == 0) THEN
            IF( INTBUF_TAB%METRIC%ALGO == TRY_ALGO_VOXEL) THEN ! if test phase
                INTBUF_TAB%METRIC%ALGO =  TRY_ALGO_BUCKET
                INTBUF_TAB%METRIC%TOLD =  INTBUF_TAB%METRIC%TOC -  INTBUF_TAB%METRIC%TIC
            ELSEIF ( INTBUF_TAB%METRIC%ALGO == TRY_ALGO_BUCKET) THEN
                IF( 1.2D0 * (INTBUF_TAB%METRIC%TOC-INTBUF_TAB%METRIC%TIC) < INTBUF_TAB%METRIC%TOLD) THEN
                    INTBUF_TAB%METRIC%ALGO = ALGO_BUCKET
                    WRITE(IOUT,*) "INFO: DOMAIN",ISPMD,
     .                    "USES SORT2 FOR CONTACT INTERFACE",NOINT
                ELSE
                    INTBUF_TAB%METRIC%ALGO = ALGO_VOXEL
c           WRITE(IOUT,*) "INFO: DOMAIN",ISPMD,
c    .                    "USES SORT1 FOR CONTACT INTERFACE",NOINT
                ENDIF
            ENDIF
        ENDIF
C
#include "lockon.inc"
        INTBUF_TAB%VARIABLES(9)  = MIN(MAXBOX,INTBUF_TAB%VARIABLES(9))
        INTBUF_TAB%VARIABLES(12) = MIN(MINBOX,INTBUF_TAB%VARIABLES(12))
        INTBUF_TAB%VARIABLES(8)  = MIN(TZINF,INTBUF_TAB%VARIABLES(8))
        INTBUF_TAB%VARIABLES(5)  = INTBUF_TAB%VARIABLES(8)-GAP
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
            IF (IMONM > 0) CALL STARTIME(26,1)
            INTBUF_TAB%VARIABLES(5) = -INTBUF_TAB%VARIABLES(5)
C
            CALL SPMD_TRI7GAT(
     1          RESULT       ,NSN     ,INTBUF_TAB%CAND_N,INTBUF_TAB%I_STOK(1),NIN,
     2          IPARI(21,NIN),NSNR    ,MULTIMP        ,NTY         ,IPARI(47,NIN),  
     3          IDUM1        ,NSNFIOLD, IPARI        , H3D_DATA    ,IPARI(72,NIN),
     4          MULTI_FVM)
            IPARI(24,NIN) = NSNR

            IF (IMONM > 0) CALL STOPTIME(26,1)
!$OMP END SINGLE
        END IF

      IF(ITASK==0) THEN
        IF(ALLOCATED( LIST_REMOTE_S_NODE ) ) DEALLOCATE( LIST_REMOTE_S_NODE )
      ENDIF
      CALL MY_BARRIER

        RETURN
        END
