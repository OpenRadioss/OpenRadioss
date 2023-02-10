Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  INIT_TH                       source/output/th/init_th.F    
Chd|-- called by -----------
Chd|        INIT_TH0                      source/output/th/init_th0.F   
Chd|-- calls ---------------
Chd|        SPMD_GATHERV_INT              source/mpi/generic/spmd_gatherv_int.F
Chd|        SPMD_GATHER_INT               source/mpi/generic/spmd_gather_int.F
Chd|        THCOQ_COUNT                   source/output/th/thcoq_count.F
Chd|        THNOD_COUNT                   source/output/th/thnod_count.F
Chd|        THNST_COUNT                   source/output/th/thnst_count.F
Chd|        THPOUT_COUNT                  source/output/th/thpout_count.F
Chd|        THQUAD_COUNT                  source/output/th/thquad_count.F
Chd|        THRES_COUNT                   source/output/th/thres_count.F
Chd|        THSOL_COUNT                   source/output/th/thsol_count.F
Chd|        THSPH_COUNT                   source/output/th/thsph_count.F
Chd|        THTRUS_COUNT                  source/output/th/thtrus_count.F
Chd|        CLUSTER_MOD                   share/modules/cluster_mod.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|        TH_MOD                        share/modules/th_mod.F        
Chd|====================================================================
        SUBROUTINE INIT_TH(IPARG,ITHBUF,ELBUF_TAB,IGEO,IXR,
     .                     ITHGRP,NTHGRP2,ID,WEIGHT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD         
      USE CLUSTER_MOD        
      USE STACK_MOD
      USE GROUPDEF_MOD
      USE TH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "com10_c.inc"
#include      "sphcom.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "scr12_c.inc"
#include      "scr13_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
#include      "scrfs_c.inc"
#include      "task_c.inc"
#include      "warn_c.inc"
#include      "timeri_c.inc"
#include      "impl1_c.inc"
#include      "rad2r_c.inc"
#include      "thermal_c.inc"
#include      "tabsiz_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MBUFFER, NPARTL,NTHGRP2
      INTEGER IPARG(NPARG,*),IGEO(NPROPGI,*),
     .   ITHGRP(NITHGR,*),ITHBUF(SITHBUF),IXR(NIXR,*),WEIGHT(NUMNOD)
      INTEGER, INTENT(in) :: ID
C     REAL
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,M,N,II,JJ,IP,NP,NN,NG,ITY,NEL,NFT,N1,N2,NPT,NRWA,
     .   JALE,FSAVMAX,PROC,NVAR,IAD,ITYP,IADV,FIRST,KRBHOL,ISKN,NNOD,ELTYPE
      INTEGER :: MY_SIZE,TOTAL_INDEX_WA_SIZE,IJK
      INTEGER, DIMENSION(NTHGRP2+1) :: LOCAL_WA
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX_WA_ELTYPE_P0,INDEX_WA_SIZE_P0
      INTEGER, DIMENSION(:), ALLOCATABLE :: WA_INDEX_DIPLS
      TYPE(TH_COMM), DIMENSION(:), POINTER :: WA_COMM
      INTEGER, DIMENSION(:), POINTER :: WA_SIZE,TOTAL_WA_SIZE
      TYPE(TH_PROC_TYPE), DIMENSION(:), POINTER :: ELTYPE_STRUCT 
      TYPE(TH_WA_REAL), DIMENSION(:), POINTER :: WA_P0,WA
C=======================================================================

C-------------------------------------------------------
C    TH GROUP
C-------------------------------------------------------


!   -------------------------------------
!               SPRING ELEMENT
!   TH optimization for spring elements
!           local data on each proc:
!
!   N=                     1              2   
!                   ________________   ______ 
!                  /                \ /      \
!   proc=    1    |    a1  |   a2    |   b1   |

!   N=                          1        2       3 
!                   ________________   ______  ______ 
!                  /                \ /      \/      \
!   proc=    2    |    a3  |   a4    |  b3    |   c1  |
!
!   N=                 2       3 
!                   ______   ______ 
!                  /      \ /      \
!   proc=    3    |  b2    |   c2  |
!
!   local data are sent to PROC0 (WA_SPRING_P0):
!   N=               1    1   2      1     1   2    3      2    3
!                 | a1 | a2 | b1 ||| a3 | a4 | b3 | c1 ||| b2 | c2 |
!   local proc=           1      |||         2         |||    3
!
!   --> need to know where are the data in WA_SPRING_P0 and where the data must be 
!       written in the TH file
!       for this, the local index (INDEX_WA_ELTYPE) for each proc is sent to PROC0 (INDEX_WA_ELTYPE_P0)
!       in order to build the global index SPRING_STRUCT(I)%TH_ELM(:,:)
!       SPRING_STRUCT(I)%TH_ELM(:,1) = position in the buffer
!       SPRING_STRUCT(I)%TH_ELM(:,2) = N
!
!       the position of the first element in the TH file is added at the end of each chunk ax, bx, cx... 
!

!   proc=    1    1    2    2
!   N=1    | a1 | a2 | a3 | a4 |
!          |         |    |
!   pos= a1(NVAR+1)..|  a4(NVAR+1)
!                    a3(NVAR+1)
!
!   proc=    1    3    2 
!   N=2    | b1 | b2 | b3 |
!          |    |    |
!   pos= b1(NVAR+1)..|b3(NVAR+1)
!               b2(NVAR+1)
!
!   proc=     2    3   
!   N=3    | c1 | c2 |
!
!   -------------------------------------
        !   ----------------------------------
        
        !   allocation of local arrays
        ALLOCATE( INDEX_WA_ELTYPE(2*NTHGRP2+1) )
        ALLOCATE( INDEX_WA_SIZE_P0(NSPMD) )
        ALLOCATE( WA_INDEX_DIPLS(NSPMD) )

        DO ELTYPE=1,9
            !   ELTYPE = 1 --> spring
            !   ELTYPE = 2 --> node
            !   ELTYPE = 3 --> sol
            !   ELTYPE = 4 --> quad
            !   ELTYPE = 5 --> shell/shell3n
            !   ELTYPE = 6 --> truss
            !   ELTYPE = 7 --> beam
            !   ELTYPE = 8 --> sph
            !   ELTYPE = 9 --> nstrand
            IF(ELTYPE==1) THEN
                WA_COMM=>WA_SPRING_COMM
                WA_SIZE=>WA_SPRING_SIZE
                TOTAL_WA_SIZE=>TOTAL_WA_SPRING_SIZE           
                ELTYPE_STRUCT=>SPRING_STRUCT
                WA=>WA_SPRING
                WA_P0=>WA_SPRING_P0
            ELSEIF(ELTYPE==2) THEN
                WA_COMM=>WA_NOD_COMM
                WA_SIZE=>WA_NOD_SIZE
                TOTAL_WA_SIZE=>TOTAL_WA_NOD_SIZE           
                ELTYPE_STRUCT=>NOD_STRUCT
                WA=>WA_NOD
                WA_P0=>WA_NOD_P0
            ELSEIF(ELTYPE==3) THEN
                WA_COMM=>WA_SOL_COMM
                WA_SIZE=>WA_SOL_SIZE
                TOTAL_WA_SIZE=>TOTAL_WA_SOL_SIZE           
                ELTYPE_STRUCT=>SOL_STRUCT
                WA=>WA_SOL
                WA_P0=>WA_SOL_P0
            ELSEIF(ELTYPE==4) THEN
                WA_COMM=>WA_QUAD_COMM
                WA_SIZE=>WA_QUAD_SIZE
                TOTAL_WA_SIZE=>TOTAL_WA_QUAD_SIZE           
                ELTYPE_STRUCT=>QUAD_STRUCT
                WA=>WA_QUAD
                WA_P0=>WA_QUAD_P0
            ELSEIF(ELTYPE==5) THEN
                WA_COMM=>WA_COQ_COMM
                WA_SIZE=>WA_COQ_SIZE
                TOTAL_WA_SIZE=>TOTAL_WA_COQ_SIZE           
                ELTYPE_STRUCT=>COQ_STRUCT
                WA=>WA_COQ
                WA_P0=>WA_COQ_P0
            ELSEIF(ELTYPE==6) THEN
                WA_COMM=>WA_TRUS_COMM
                WA_SIZE=>WA_TRUS_SIZE
                TOTAL_WA_SIZE=>TOTAL_WA_TRUS_SIZE           
                ELTYPE_STRUCT=>TRUS_STRUCT
                WA=>WA_TRUS
                WA_P0=>WA_TRUS_P0
            ELSEIF(ELTYPE==7) THEN
                WA_COMM=>WA_POUT_COMM
                WA_SIZE=>WA_POUT_SIZE
                TOTAL_WA_SIZE=>TOTAL_WA_POUT_SIZE           
                ELTYPE_STRUCT=>POUT_STRUCT
                WA=>WA_POUT
                WA_P0=>WA_POUT_P0
            ELSEIF(ELTYPE==8) THEN
                WA_COMM=>WA_SPH_COMM
                WA_SIZE=>WA_SPH_SIZE
                TOTAL_WA_SIZE=>TOTAL_WA_SPH_SIZE           
                ELTYPE_STRUCT=>SPH_STRUCT
                WA=>WA_SPH
                WA_P0=>WA_SPH_P0
            ELSEIF(ELTYPE==9) THEN
                WA_COMM=>WA_NST_COMM
                WA_SIZE=>WA_NST_SIZE
                TOTAL_WA_SIZE=>TOTAL_WA_NST_SIZE           
                ELTYPE_STRUCT=>NST_STRUCT
                WA=>WA_NST
                WA_P0=>WA_NST_P0
            ENDIF

            ALLOCATE( WA_COMM(ID)%TH_SIZE(NSPMD) )
            ALLOCATE( WA_COMM(ID)%TH_DIPLS(NSPMD) )

            WA_COMM(ID)%TH_SIZE(1:NSPMD) = 0
            INDEX_WA_ELTYPE(1:2*NTHGRP2+1) = 0
            INDEX_WA_SIZE_P0(1:NSPMD) = 0
            WA_INDEX_DIPLS(1:NSPMD) = 0

            !   count the number of chunk and get the total size of WA_SPRING
            IF(ELTYPE==1) THEN
                CALL THRES_COUNT(IPARG,ITHBUF,ELBUF_TAB,IGEO,IXR,
     .                         ITHGRP,NTHGRP2,WA_SIZE(ID),INDEX_WA_ELTYPE)
            ELSEIF(ELTYPE==2) THEN
                CALL THNOD_COUNT(ITHGRP,NTHGRP2,WA_SIZE(ID),INDEX_WA_ELTYPE,ITHBUF,
     .                           WEIGHT)
            ELSEIF(ELTYPE==3) THEN
                CALL THSOL_COUNT(NTHGRP2 ,ITHGRP,WA_SIZE(ID),INDEX_WA_ELTYPE,
     .                           IPARG,ITHBUF  )
            ELSEIF(ELTYPE==4) THEN
                CALL THQUAD_COUNT(NTHGRP2 ,ITHGRP,WA_SIZE(ID),INDEX_WA_ELTYPE,
     .                           IPARG,ITHBUF  )
            ELSEIF(ELTYPE==5) THEN
                CALL THCOQ_COUNT(NTHGRP2 ,ITHGRP,WA_SIZE(ID),INDEX_WA_ELTYPE,
     .                           IPARG,ITHBUF  )
            ELSEIF(ELTYPE==6) THEN
                CALL THTRUS_COUNT(NTHGRP2,ITHGRP,WA_SIZE(ID),INDEX_WA_ELTYPE,
     .                            IPARG  ,ITHBUF)
            ELSEIF(ELTYPE==7) THEN
                CALL THPOUT_COUNT(NTHGRP2, ITHGRP, WA_SIZE(ID),INDEX_WA_ELTYPE,
     .                            IPARG  , ITHBUF )
            ELSEIF(ELTYPE==8) THEN
                CALL THSPH_COUNT(NTHGRP2, ITHGRP, WA_SIZE(ID),INDEX_WA_ELTYPE, 
     .                           IPARG, ITHBUF )
            ELSEIF(ELTYPE==9) THEN
                CALL THNST_COUNT(NTHGRP2, ITHGRP, WA_SIZE(ID),INDEX_WA_ELTYPE,
     .                           IPARG,ITHBUF)
            ENDIF

            !   send the local size of index to PROC0
            MY_SIZE = INDEX_WA_ELTYPE(2*NTHGRP2+1)
            IF(NSPMD>1) THEN
                CALL SPMD_GATHER_INT(MY_SIZE,INDEX_WA_SIZE_P0,0,1,NSPMD)
            ELSE
                INDEX_WA_SIZE_P0 = MY_SIZE
            ENDIF

    
            WA_COMM(ID)%TH_DIPLS(1:NSPMD) = 0
            TOTAL_WA_SIZE(ID) = 0
            TOTAL_INDEX_WA_SIZE = 0
            IF(ISPMD==0) THEN
                WA_INDEX_DIPLS(1) = 0
                DO I=1,NSPMD-1
                    WA_INDEX_DIPLS(I+1) = WA_INDEX_DIPLS(I) + INDEX_WA_SIZE_P0(I)
                    TOTAL_INDEX_WA_SIZE = TOTAL_INDEX_WA_SIZE + INDEX_WA_SIZE_P0(I)
                ENDDO
                TOTAL_INDEX_WA_SIZE = TOTAL_INDEX_WA_SIZE + INDEX_WA_SIZE_P0(NSPMD)
            ENDIF
            !   allocation of INDEX_WA_ELTYPE_P0
            ALLOCATE( INDEX_WA_ELTYPE_P0(TOTAL_INDEX_WA_SIZE) )
    
            IF(NSPMD>1) THEN
                !   send the local index to PROC0
                CALL SPMD_GATHERV_INT(INDEX_WA_ELTYPE,INDEX_WA_ELTYPE_P0,0,MY_SIZE,TOTAL_INDEX_WA_SIZE,
     .                          INDEX_WA_SIZE_P0,WA_INDEX_DIPLS)

                !   send the local size of WA_SPRING to PROC0
                CALL SPMD_GATHER_INT(WA_SIZE(ID),WA_COMM(ID)%TH_SIZE,0,1,NSPMD)
            ELSE
                INDEX_WA_ELTYPE_P0 = INDEX_WA_ELTYPE
                WA_COMM(ID)%TH_SIZE(1) = WA_SIZE(ID)
            ENDIF
    
            IF(ISPMD==0) THEN
                !   displacement for the gatherv comm and total size of WA_SPRING_P0
                WA_COMM(ID)%TH_DIPLS(1) = 0
                DO I=1,NSPMD-1
                    WA_COMM(ID)%TH_DIPLS(I+1) = WA_COMM(ID)%TH_DIPLS(I) + WA_COMM(ID)%TH_SIZE(I)
                    TOTAL_WA_SIZE(ID) = TOTAL_WA_SIZE(ID) + WA_COMM(ID)%TH_SIZE(I)
                ENDDO
                TOTAL_WA_SIZE(ID) = TOTAL_WA_SIZE(ID) + WA_COMM(ID)%TH_SIZE(NSPMD)
            ENDIF

            IF(ISPMD==0) THEN
                !   allocation of SPRING_STRUCT + initialization
                ALLOCATE( ELTYPE_STRUCT(ID)%TH_PROC(NSPMD) )
                DO I=1,NSPMD                
                    LOCAL_WA(1:NTHGRP2+1) = 0
                    J = WA_INDEX_DIPLS(I)
                    LOCAL_WA(1) = WA_COMM(ID)%TH_DIPLS(I)
                    !   index initialization
                    DO IJK=1,INDEX_WA_SIZE_P0(I)/2
                        N = INDEX_WA_ELTYPE_P0(2*IJK+J)
                        LOCAL_WA(N+1) = WA_COMM(ID)%TH_DIPLS(I) + INDEX_WA_ELTYPE_P0(2*IJK-1+J)
                    ENDDO

                    DO N=2,NTHGRP2+1
                        IF(LOCAL_WA(N)==0) THEN
                            LOCAL_WA(N)=LOCAL_WA(N-1)
                        ENDIF
                    ENDDO
    
                    !   if PROC I must send its data to PROC0
                    IF(INDEX_WA_SIZE_P0(I)/2>0) THEN
                        !   count the number of N in order to reduce the size 
                        IJK = 0
                        DO N=2,NTHGRP2+1
                            IF(LOCAL_WA(N)-LOCAL_WA(N-1)>0) THEN
                                IJK=IJK+1                  
                            ENDIF
                        ENDDO
                        IJK=IJK+1
                        !   allocation of %TH_ELM_SIZE 
                        ELTYPE_STRUCT(ID)%TH_PROC(I)%TH_ELM_SIZE = IJK
                        IJK = ELTYPE_STRUCT(ID)%TH_PROC(I)%TH_ELM_SIZE
                        ALLOCATE( ELTYPE_STRUCT(ID)%TH_PROC(I)%TH_ELM(IJK,2) )
                        !   initialization of %TH_ELM_SIZE 
                        IJK = 0
                        DO N=2,NTHGRP2+1
                            IF(LOCAL_WA(N)-LOCAL_WA(N-1)>0) THEN
                                IJK=IJK+1
                                ELTYPE_STRUCT(ID)%TH_PROC(I)%TH_ELM(IJK,1) = LOCAL_WA(N-1)
                                ELTYPE_STRUCT(ID)%TH_PROC(I)%TH_ELM(IJK,2) = N-1                        
                            ENDIF
                        ENDDO
                        IJK=IJK+1
                        ELTYPE_STRUCT(ID)%TH_PROC(I)%TH_ELM(IJK,1) = LOCAL_WA(NTHGRP2+1)
                        ELTYPE_STRUCT(ID)%TH_PROC(I)%TH_ELM(IJK,2) = NTHGRP2+1
                    ELSE
                    !   elseif PROC I do nothing : allocation to 0
                        ELTYPE_STRUCT(ID)%TH_PROC(I)%TH_ELM_SIZE = 0
                        ALLOCATE( ELTYPE_STRUCT(ID)%TH_PROC(I)%TH_ELM(0,0) )
                    ENDIF
                ENDDO
            ENDIF

            !   allocation of WA_SPRING and WA_SPRING_P0 (--> size=0 for every PROC except PROC0)
            ALLOCATE( WA(ID)%WA_REAL(WA_SIZE(ID)) )
            ALLOCATE( WA_P0(ID)%WA_REAL(TOTAL_WA_SIZE(ID)) )
            !   initialisation done : next element type
             DEALLOCATE( INDEX_WA_ELTYPE_P0 )
        ENDDO
        !   initialisation done
        DEALLOCATE( INDEX_WA_ELTYPE )
        DEALLOCATE( INDEX_WA_SIZE_P0 )
        DEALLOCATE( WA_INDEX_DIPLS )
        !   ----------------------------------    
        RETURN
        END SUBROUTINE INIT_TH
