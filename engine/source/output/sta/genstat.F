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
!||    genstat                   ../engine/source/output/sta/genstat.F
!||--- called by ------------------------------------------------------
!||    sortie_main               ../engine/source/output/sortie_main.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                    ../engine/source/output/message/message.F
!||    arret                     ../engine/source/system/arret.F
!||    close_c                   ../common_source/tools/input_output/write_routtines.c
!||    cur_fil_c                 ../common_source/tools/input_output/write_routtines.c
!||    open_c                    ../common_source/tools/input_output/write_routtines.c
!||    spmd_outpitab             ../engine/source/mpi/interfaces/spmd_outp.F
!||    stat_beam_mp              ../engine/source/output/sta/stat_beam_mp.F
!||    stat_beam_spmd            ../engine/source/output/sta/stat_beam_spmd.F
!||    stat_brick_mp             ../engine/source/output/sta/stat_brick_mp.F
!||    stat_brick_spmd           ../engine/source/output/sta/stat_brick_spmd.F
!||    stat_c_auxf               ../engine/source/output/sta/stat_c_auxf.F
!||    stat_c_epspf              ../engine/source/output/sta/stat_c_epspf.F
!||    stat_c_fail               ../engine/source/output/sta/stat_c_fail.F
!||    stat_c_off                ../engine/source/output/sta/stat_c_off.F
!||    stat_c_orth_loc           ../engine/source/output/sta/stat_c_orth_loc.F
!||    stat_c_straf              ../engine/source/output/sta/stat_c_straf.F
!||    stat_c_strafg             ../engine/source/output/sta/stat_c_strafg.F
!||    stat_c_strsf              ../engine/source/output/sta/stat_c_strsf.F
!||    stat_c_strsfg             ../engine/source/output/sta/stat_c_strsfg.F
!||    stat_c_thk                ../engine/source/output/sta/stat_c_thk.F
!||    stat_inimap1d_file_spmd   ../engine/source/output/sta/stat_inimap1d_file_spmd.F
!||    stat_inimap1d_spmd        ../engine/source/output/sta/stat_inimap1d_spmd.F
!||    stat_inimap2d_file_spmd   ../engine/source/output/sta/stat_inimap2d_file_spmd.F
!||    stat_inimap2d_spmd        ../engine/source/output/sta/stat_inimap2d_spmd.F
!||    stat_n_bcs                ../engine/source/output/sta/stat_n_bcs.F
!||    stat_n_temp               ../engine/source/output/sta/stat_n_temp.F
!||    stat_n_vel                ../engine/source/output/sta/state_n_vel.F
!||    stat_node                 ../engine/source/output/sta/stat_node.F
!||    stat_p_aux                ../engine/source/output/sta/stat_p_aux.F
!||    stat_p_full               ../engine/source/output/sta/stat_p_full.F
!||    stat_quad_mp              ../engine/source/output/sta/stat_quad_mp.F
!||    stat_quad_spmd            ../engine/source/output/sta/stat_quad_spmd.F
!||    stat_r_full               ../engine/source/output/sta/stat_r_full.F
!||    stat_s_auxf               ../engine/source/output/sta/stat_s_auxf.F
!||    stat_s_eref               ../engine/source/output/sta/stat_s_eref.F
!||    stat_s_fail               ../engine/source/output/sta/stat_s_fail.F
!||    stat_s_ortho              ../engine/source/output/sta/stat_s_ortho.F
!||    stat_s_straf              ../engine/source/output/sta/stat_s_straf.F
!||    stat_s_strsf              ../engine/source/output/sta/stat_s_strsf.F
!||    stat_shel_mp              ../engine/source/output/sta/stat_shel_mp.F
!||    stat_shel_spmd            ../engine/source/output/sta/stat_shel_spmd.F
!||    stat_size_c               ../engine/source/output/sta/stat_size.F
!||    stat_sphcel_full          ../engine/source/output/sta/stat_sphcel_full.F90
!||    stat_sphcel_mp            ../engine/source/output/sta/stat_sphcel_mp.F90
!||    stat_sphcel_spmd          ../engine/source/output/sta/stat_sphcel_spmd.F90
!||    stat_spring_mp            ../engine/source/output/sta/stat_spring_mp.F
!||    stat_spring_spmd          ../engine/source/output/sta/stat_spring_spmd.F
!||    stat_t_full               ../engine/source/output/sta/stat_t_full.F
!||    stat_truss_mp             ../engine/source/output/sta/stat_truss_mp.F
!||    stat_truss_spmd           ../engine/source/output/sta/stat_truss_spmd.F
!||    strs_txt50                ../engine/source/output/sta/sta_txt.F
!||--- uses       -----------------------------------------------------
!||    drape_mod                 ../engine/share/modules/drape_mod.F
!||    elbufdef_mod              ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    inoutfile_mod             ../common_source/modules/inoutfile_mod.F
!||    mat_elem_mod              ../common_source/modules/mat_elem/mat_elem_mod.F90
!||    message_mod               ../engine/share/message_module/message_mod.F
!||    multi_fvm_mod             ../common_source/modules/ale/multi_fvm_mod.F90
!||    my_alloc_mod              ../common_source/tools/memory/my_alloc.F90
!||    output_mod                ../common_source/modules/output/output_mod.F90
!||    stack_mod                 ../engine/share/modules/stack_mod.F
!||    stat_sphcel_full_mod      ../engine/source/output/sta/stat_sphcel_full.F90
!||    stat_sphcel_mp_mod        ../engine/source/output/sta/stat_sphcel_mp.F90
!||    stat_sphcel_spmd_mod      ../engine/source/output/sta/stat_sphcel_spmd.F90
!||    state_inimap_mod          ../engine/share/modules/state_inimap_mod.F
!||====================================================================
      SUBROUTINE GENSTAT(X       ,MS      ,ELBUF_TAB,BUFEL  ,SPBUF    ,
     2                  IXS      ,IXQ     ,IXC      ,IXT     ,IXP     ,
     3                  IXR      ,IXTG    ,KXSP     ,IPARG   ,IPM     ,
     4                  IGEO     ,ITAB    ,IPART    ,PM      ,GEO     ,
     5                  IPARTS   ,IPARTQ  ,IPARTC   ,IPARTT  ,IPARTP  ,
     6                  IPARTR   ,IPARTUR ,IPARTTG  ,IPARTX  ,IPARTSP ,
     7                  DD_IAD   ,WEIGHT  ,NODGLOB  ,LENG,IPART_STATE ,
     8                  LENGC    ,LENGTG  ,SH4TREE  ,SH3TREE ,LENGS   , 
     9                  SH4TRIM  ,SH3TRIM ,TEMP     ,IXS10   ,THKE    ,
     A                  IXS16    ,IXS20   ,ICODE    ,LENGR   ,LENGP   ,
     B                  LENGT    ,ISKEW   ,V        ,VR      ,LENGQ   ,
     C                  MULTI_FVM,BUFMAT  ,NPBY     ,LPBY    ,STACK   ,
     D                  DRAPE_SH4N ,DRAPE_SH3N,DR,DRAPEG,MAT_PARAM    ,
     E                  IPARTSPH , OUTPUT ,LENGSPH  ,NUMSPHG ,ITHERM_FE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MAT_ELEM_MOD
      USE ELBUFDEF_MOD
      USE MESSAGE_MOD    
      USE INOUTFILE_MOD 
      USE MULTI_FVM_MOD
      USE STATE_INIMAP_MOD
      USE STACK_MOD
      USE DRAPE_MOD
      USE OUTPUT_MOD
      USE stat_sphcel_mp_mod
      USE stat_sphcel_spmd_mod
      USE stat_sphcel_full_mod
      USE MY_ALLOC_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com09_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "chara_c.inc"
#include      "task_c.inc"
#include      "sphcom.inc"
#include      "state_c.inc"
#include      "sysunit.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(*),
     .   IXS(NIXS,NUMELS),IXQ(NIXQ,NUMELQ),IXC(NIXC,NUMELC),IXTG(NIXTG,NUMELTG),
     .   IXT(NIXT,NUMELT),IXP(NIXP,NUMELP),IXR(NIXR,NUMELR),KXSP(NISP,*),
     .   ITAB(*),IPART(LIPART1,*),IPM(*),IGEO(*),
     .   IPARTS(*),IPARTQ(*) ,IPARTC(*) ,IPARTT(*),IPARTP(*) ,
     .   IPARTR(*),IPARTUR(*),IPARTTG(*),IPARTX(*),IPARTSP(*),
     .   WEIGHT(*), DD_IAD(*), NODGLOB(*), IPART_STATE(*),
     .   SH4TREE(*), SH3TREE(*),
     .   SH4TRIM(*), SH3TRIM(*),IXS10(*),IXS16(*),IXS20(*),ICODE(*),
     .   ISKEW(*), NPBY(NNPBY,*), LPBY(*)
      INTEGER LENG,LENGC,LENGTG,LENGS,LENGR,LENGP,LENGT,LENGQ
      INTEGER ,intent(in) :: ITHERM_FE
      my_real
     .   X(*), MS(*), BUFEL(*), SPBUF(*),
     .   PM(NPROPM,*), GEO(NPROPG,*) ,TEMP(*),THKE(*),
     .   V(3,*),VR(3,*),DR(SDR)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      TYPE (STACK_PLY) :: STACK
      TYPE (DRAPE_)  :: DRAPE_SH4N(NUMELC_DRAPE), DRAPE_SH3N(NUMELTG_DRAPE)
      TYPE(DRAPEG_) :: DRAPEG
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(IN) :: MAT_PARAM
      INTEGER, DIMENSION(NUMSPH), INTENT(IN)  :: IPARTSPH
      my_real BUFMAT(*)
      TYPE(OUTPUT_),INTENT(INOUT) :: OUTPUT
      INTEGER, INTENT(IN)  :: LENGSPH
      INTEGER, INTENT(IN)  :: NUMSPHG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER CHSTAT*4,FILNAM*100,T10*10,MES*40
      INTEGER FILEN,I,INNODA,IERR,J,N
      INTEGER LENR,SIZLOC,SIZP0
      INTEGER I161,I16A,I16B,I16C,I16D,I16E,I16F,I16G,I16H,I16I
      INTEGER,DIMENSION(:),ALLOCATABLE :: ITABG
      INTEGER,DIMENSION(:),ALLOCATABLE :: NODTAG
      INTEGER,DIMENSION(:),ALLOCATABLE :: STAT_INDXC
      INTEGER,DIMENSION(:),ALLOCATABLE :: STAT_INDXTG
      INTEGER,DIMENSION(:),ALLOCATABLE :: STAT_INDXQ
      INTEGER,DIMENSION(:),ALLOCATABLE :: STAT_INDXS
      INTEGER,DIMENSION(:),ALLOCATABLE :: STAT_INDXR
      INTEGER,DIMENSION(:),ALLOCATABLE :: STAT_INDXP
      INTEGER,DIMENSION(:),ALLOCATABLE :: STAT_INDX
      INTEGER,DIMENSION(:),ALLOCATABLE :: STAT_INDXT
      INTEGER,DIMENSION(:),ALLOCATABLE :: STAT_INDXSPH
      
      INTEGER CTEXT(2149)
      double precision
     .      ,  DIMENSION(:),ALLOCATABLE :: WA,WAP0

      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2148) :: TMP_NAME
      LOGICAL IS_FILE_TO_BE_WRITTEN
      CHARACTER*100 LINE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      CALL MY_ALLOC(ITABG,LENG)
      CALL MY_ALLOC(NODTAG,NUMNOD)
      CALL MY_ALLOC(STAT_INDXC,2*LENGC)
      CALL MY_ALLOC(STAT_INDXTG,2*LENGTG)
      CALL MY_ALLOC(STAT_INDXQ,2*LENGQ)
      CALL MY_ALLOC(STAT_INDXS,2*LENGS)
      CALL MY_ALLOC(STAT_INDXR,2*LENGR)
      CALL MY_ALLOC(STAT_INDXP,2*LENGP)
      CALL MY_ALLOC(STAT_INDXT,2*LENGT)
      CALL MY_ALLOC(STAT_INDXSPH,2*LENGSPH)
C-----------------------------------------------
      IS_FILE_TO_BE_WRITTEN = ((ISPMD == 0) .AND. (.NOT.IS_STAT_INIMAP_SINGLE))  !/STATE/INIMAP* is using an independent state file
C===============================================|
C   OPEN FILE
C-----------------------------------------------
      IF(ISTATF>=10000)ISTATF=1
      WRITE(CHSTAT,'(I4.4)')ISTATF
      FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHSTAT//'.sta'
      FILEN = ROOTLEN + 9
      LEN_TMP_NAME = OUTFILE_NAME_LEN + FILEN
      TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:FILEN)

      IF(IS_FILE_TO_BE_WRITTEN) THEN
        OPEN(UNIT=IUGEO,FILE=TMP_NAME(1:LEN_TMP_NAME),ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='UNKNOWN')
        WRITE(IUGEO,'(2A)')'#RADIOSS STATE FILE ',FILNAM(1:FILEN)
        !   OPEN STRS FILE
        IF(IZIPSTRS/=0) THEN
          FILNAM=ROOTNAM(1:ROOTLEN)//'_'//CHSTAT//'.str'
          LEN_TMP_NAME = OUTFILE_NAME_LEN + FILEN
          TMP_NAME=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:FILEN)
          DO I=1,LEN_TMP_NAME
            CTEXT(I)=ICHAR(TMP_NAME(I:I))
            CALL CUR_FIL_C(0)
          ENDDO
        ENDIF
        IF(IZIPSTRS==1)THEN
          CALL OPEN_C(CTEXT,LEN_TMP_NAME,0)
        ELSEIF(IZIPSTRS==2)THEN
          CALL OPEN_C(CTEXT,LEN_TMP_NAME,6)
        ENDIF
      END IF
C-----------------------------------------------
C   /BEGIN + UNITS
C-----------------------------------------------
      IF(IS_FILE_TO_BE_WRITTEN) THEN
          WRITE(IUGEO,'(A)')'/BEGIN'
          WRITE(IUGEO,'(A)') ROOTNAM(1:ROOTLEN)
          WRITE(IUGEO,'(I10,I10)') ST_INVERS, 0
          WRITE(IUGEO,'(1P3E20.13)') FAC_MASS,FAC_LENGTH,FAC_TIME
          WRITE(IUGEO,'(1P3E20.13)') FAC_MASS,FAC_LENGTH,FAC_TIME
        IF(IZIPSTRS > 0) THEN
          WRITE(LINE,'(A)')'/BEGIN'
          CALL STRS_TXT50(LINE,100)
          WRITE(LINE,'(A)') ROOTNAM(1:ROOTLEN)
          CALL STRS_TXT50(LINE,100)
          WRITE(LINE,'(I10,I10)') ST_INVERS, 0
          CALL STRS_TXT50(LINE,100)
          WRITE(LINE,'(1P3E20.13)') FAC_MASS,FAC_LENGTH,FAC_TIME
          CALL STRS_TXT50(LINE,100)
          WRITE(LINE,'(1P3E20.13)') FAC_MASS,FAC_LENGTH,FAC_TIME
          CALL STRS_TXT50(LINE,100)
        ENDIF
      ENDIF
C-----------------------------------------------
C   CONNECTIVITIES + NODAL COORDINATES
C-----------------------------------------------
c     
      IF (NSPMD > 1)CALL SPMD_OUTPITAB(ITAB,WEIGHT,NODGLOB,ITABG)

      NODTAG=0

      STAT_NUMELC =0
      STAT_NUMELTG =0
      STAT_NUMELS =0
      STAT_NUMELQ =0
      STAT_NUMELR =0
      STAT_NUMELP =0
      STAT_NUMELT =0
      OUTPUT%STATE%STAT_NUMELSPH = 0

      IF(NSPMD == 1)THEN
        !  - shells -
        CALL STAT_SHEL_MP(
     .                         ITAB,ITABG,LENG,IPART,IGEO,                    
     .                         IXC,IXTG,IPARTC,IPARTTG,IPART_STATE,           
     .                         NODTAG,STAT_INDXC,STAT_INDXTG,SH4TREE,SH3TREE, 
     .                         IPARG ,SH4TRIM   ,SH3TRIM    ,ELBUF_TAB,THKE,  
     .                         STAT_C(9))                                     
        STAT_NUMELC_G =STAT_NUMELC 
        STAT_NUMELTG_G =STAT_NUMELTG 
        !  - quads - (only taging nodes, quads are currently not exported in state files : 3d only)
        CALL STAT_QUAD_MP(
     .                         ITAB,IPART,IGEO,IXQ,IPARTQ,
     .                         IPART_STATE,NODTAG,STAT_INDXQ,
     .                         IPARG ,ELBUF_TAB)
        STAT_NUMELQ_G =STAT_NUMELQ       
        !  - solids -
        CALL STAT_BRICK_MP(
     .                         ITAB,IPART,IGEO,IXS,IPARTS,
     .                         IPART_STATE,NODTAG,STAT_INDXS,
     .                         IPARG ,IXS10,IXS16,IXS20,ELBUF_TAB,
     .                         STAT_S(12))
        STAT_NUMELS_G =STAT_NUMELS
        !  - springs -
        CALL STAT_SPRING_MP(
     .                          ITAB   ,IPART     ,IXR   ,IPARTR   ,IPART_STATE,
     .                           NODTAG ,STAT_INDXR,IPARG ,ELBUF_TAB,STAT_R(2)  )
        STAT_NUMELR_G =STAT_NUMELR
        !  - beams -
        CALL STAT_BEAM_MP(
     .                        ITAB   ,IPART     ,IXP   ,IPARTP   ,IPART_STATE,
     .                        NODTAG ,STAT_INDXP,IPARG ,ELBUF_TAB,STAT_P(2)  )
        STAT_NUMELP_G =STAT_NUMELP
        !  - trusses -
        CALL STAT_TRUSS_MP(
     .                         ITAB   ,IPART     ,IXT   ,IPARTT   ,IPART_STATE,
     .                         NODTAG ,STAT_INDXT,IPARG ,ELBUF_TAB,STAT_T(2)  )
        STAT_NUMELT_G =STAT_NUMELT
        !  - sphcels -
        CALL STAT_SPHCEL_MP(   NUMNOD , NUMSPH    ,NISP  ,NPART    ,NGROUP , 
     .                         NPARG  , LIPART1   ,OUTPUT%STATE%STAT_NUMELSPH ,ITAB   ,IPART     ,
     .                         KXSP   , IPARTSPH  ,IPART_STATE,NODTAG ,STAT_INDXSPH,
     .                         IPARG  ,ELBUF_TAB  ,OUTPUT%STATE%STAT_SPH(2)  )
        
        OUTPUT%STATE%STAT_NUMELSPH_G = OUTPUT%STATE%STAT_NUMELSPH

      ELSE
        !  - shells -
        CALL STAT_SHEL_SPMD(
     .                         ITAB,ITABG,LENG,IPART,IGEO,                  
     .                         IXC,IXTG,IPARTC,IPARTTG,IPART_STATE,         
     .                         NODTAG,STAT_INDXC,STAT_INDXTG,LENGC,LENGTG,  
     .                         IPARG ,ELBUF_TAB,THKE,STAT_C(9))       
        !  - solids -
        CALL STAT_BRICK_SPMD(
     .                         ITAB,IPART,IGEO,IXS,IPARTS,
     .                         IPART_STATE,NODTAG,STAT_INDXS,
     .                         IPARG, LENGS,IXS10,IXS16,IXS20,
     .                         ELBUF_TAB, STAT_S(12))
        !  - quads - (only taging nodes, quads are currently not exported in state files : 3d only)
        CALL STAT_QUAD_SPMD(
     .                         ITAB,IPART,IGEO,IXQ,IPARTQ,
     .                         IPART_STATE,NODTAG,STAT_INDXQ,
     .                         IPARG, LENGQ, ELBUF_TAB)
     
        !  - springs -
        CALL STAT_SPRING_SPMD(
     .                          ITAB   ,IPART     ,IXR   ,IPARTR,IPART_STATE,
     .                          NODTAG ,STAT_INDXR,LENGR ,IPARG ,ELBUF_TAB  ,
     .                          STAT_R(2) )
        !  - beams -
        CALL STAT_BEAM_SPMD(
     .                        ITAB   ,IPART     ,IXP   ,IPARTP,IPART_STATE,
     .                        NODTAG ,STAT_INDXP,LENGP ,IPARG ,ELBUF_TAB  ,
     .                        STAT_P(2) )
        !  - trusses -
        CALL STAT_TRUSS_SPMD(
     .                         ITAB   ,IPART     ,IXT   ,IPARTT,IPART_STATE,
     .                         NODTAG ,STAT_INDXT,LENGT ,IPARG ,ELBUF_TAB  ,
     .                         STAT_T(2) )
        !  - sphcels -
        CALL STAT_SPHCEL_SPMD(   
     .                         NUMNOD   ,NUMSPH    ,NUMSPHG  ,NISP    ,NPART     , 
     .                         NGROUP   ,NPARG     ,LIPART1  ,OUTPUT%STATE%STAT_NUMELSPH ,OUTPUT%STATE%STAT_NUMELSPH_G ,
     .                         LENGSPH  ,NSPMD     ,ITAB     ,IPART   ,KXSP      ,
     .                         IPARTSPH ,IPART_STATE ,NODTAG ,STAT_INDXSPH,IPARG ,
     .                         ELBUF_TAB ,OUTPUT%STATE%STAT_SPH(2)  )
      END IF

      !  - inimap1d -  (generic subroutine : SMP or SPMD)
      IF(IS_STAT_INIMAP1D)THEN
        IF(IS_STAT_INIMAP_FILE)THEN
          CALL STAT_INIMAP1D_FILE_SPMD(
     .                          X        , V        , ITAB  , IPART_STATE, NODTAG   ,
     .                          IPART    , IPARTS   , IPARTQ, IPARTTG    , MAT_PARAM,
     .                          IGEO     , IPARG    , IXS   , IXQ        , IXTG     ,
     .                          ELBUF_TAB, MULTI_FVM, BUFMAT, IPM) 
        ELSE
          CALL STAT_INIMAP1D_SPMD(
     .                          X        , V        , ITAB  , IPART_STATE, NODTAG   ,
     .                          IPART    , IPARTS   , IPARTQ, IPARTTG    , MAT_PARAM,
     .                          IGEO     , IPARG    , IXS   , IXQ        , IXTG     ,
     .                          ELBUF_TAB, MULTI_FVM, BUFMAT, IPM) 
        ENDIF 
      !  - inimap2d -  (generic subroutine : SMP or SPMD)
      ELSEIF(IS_STAT_INIMAP2D)THEN
        IF(IS_STAT_INIMAP_FILE)THEN
          CALL STAT_INIMAP2D_FILE_SPMD(
     .                          X        , V        , ITAB  , IPART_STATE, NODTAG   ,
     .                          IPART    , IPARTS   , IPARTQ, IPARTTG    , MAT_PARAM,
     .                          IGEO     , IPARG    , IXS   , IXQ        , IXTG     ,
     .                          ELBUF_TAB, MULTI_FVM, BUFMAT, IPM) 
        ELSE
          CALL STAT_INIMAP2D_SPMD(
     .                          X        , V        , ITAB  , IPART_STATE, NODTAG   ,
     .                          IPART    , IPARTS   , IPARTQ, IPARTTG    , MAT_PARAM,
     .                          IGEO     , IPARG    , IXS   , IXQ        , IXTG     ,
     .                          ELBUF_TAB, MULTI_FVM, BUFMAT, IPM)        
        ENDIF 
      ENDIF                     

C-----------------------------------------------
C   IF OPTION IS STATE/NODE/ALL 
C-----------------------------------------------
      IF(NSTATALL /= 0) THEN
        DO I=1,NUMNOD
          NODTAG(I) = 1
        ENDDO
      ELSE
C-----------------------------------------------
C   RIGID BODY'S PRIMARY NODE IS OUTPUTTED IF ONE OF ITS SECONDARY NODES ARE
C-----------------------------------------------
        DO I=1,NRBODY
          DO J=1,NPBY(2,I)
            N=LPBY(NPBY(11,I)+J)
            IF (NODTAG(N)/=0) THEN
              NODTAG(NPBY(1,I)) = 1
              EXIT
            END IF 
          ENDDO
        ENDDO
      ENDIF
C-----------------------------------------------
      CALL STAT_NODE(X,NUMNOD,ITAB,ITABG,LENG,NODGLOB,WEIGHT,NODTAG)
      
      IF(IZIPSTRS /= 0 .AND. IS_FILE_TO_BE_WRITTEN)THEN
        WRITE(IUGEO,'(A)')'/STATE/STR_FILE'
        WRITE(IUGEO,'(A)')'# gzip (no:0: yes:1)'
        WRITE(IUGEO,'(I10)')IZIPSTRS-1
        WRITE(IUGEO,'(A)')'# file name'
        IF (IZIPSTRS == 1) WRITE(IUGEO,'(A)')FILNAM(1:FILEN)
        IF (IZIPSTRS == 2) WRITE(IUGEO,'(A)')FILNAM(1:FILEN)//'.gz'
      ENDIF
C-----------------------------------------------
C   NODE SCALAR
C-----------------------------------------------
      IF(STAT_N(1)==1) 
     .   CALL STAT_N_TEMP(X,NUMNOD,ITAB,ITABG,LENG,
     .   NODGLOB,WEIGHT,NODTAG,TEMP,ITHERM_FE)

      IF(STAT_N(2)==1) 
     .  CALL STAT_N_BCS(ICODE,NUMNOD,ITAB,ITABG,LENG,
     .   NODGLOB,ISKEW,NODTAG)
! translation/rotation velocity
      IF(STAT_N(3)==1)
     .   CALL STAT_N_VEL(NUMNOD ,ITAB   ,ITABG  ,LENG ,NODGLOB,
     .                   WEIGHT ,NODTAG ,V      ,VR   )
C-----------------------------------------------
      CALL STAT_SIZE_C(IPARG ,IXC    ,IXTG ,IGEO ,IPM     ,
     .                 SIZP0 ,SIZLOC ,IXS  ,GEO  ,ELBUF_TAB,
     .                 IXR   ,IXP    ,IXT  ,OUTPUT,LIPART1 ,
     .                 NPART ,IPART  ,NUMSPH,IPARTSP  )
C-----------------------------------------------
C   ALLOCATION OF TABLES
C-----------------------------------------------
      IERR = 0
      IF(SIZLOC >= 1) THEN
        ALLOCATE(WA(SIZLOC),STAT=IERR)
      ELSE
        ALLOCATE(WA(1))
      ENDIF
      IF(IERR/=0)THEN
         CALL ANCMSG(MSGID=252,ANMODE=ANINFO,
     .        I1=IERR)
              CALL ARRET(2)
       END IF 

      IERR = 0
      SIZP0 = MAX(1,SIZP0)
      ALLOCATE(WAP0(SIZP0),STAT=IERR)
      IF(IERR/=0)THEN
         CALL ANCMSG(MSGID=252,ANMODE=ANINFO,
     .        I1=IERR)
              CALL ARRET(2)
       END IF 
C-----------------------------------------------
C   SHELL SCALAR
C-----------------------------------------------
      IF(STAT_C(1)==1) CALL STAT_C_OFF(
     .   ELBUF_TAB,IPARG ,IPM ,IGEO ,IXC  ,
     .   IXTG ,WA,WAP0,IPARTC,IPARTTG,
     .   IPART_STATE,STAT_INDXC,STAT_INDXTG,SIZP0)

C     option non documentee (not a public option) ::
      IF(STAT_C(2)==1) CALL STAT_C_THK(
     .   ELBUF_TAB,IPARG ,IPM ,IGEO,IXC  ,
     .   IXTG ,WA,WAP0 ,IPARTC,IPARTTG,
     .   IPART_STATE,STAT_INDXC,STAT_INDXTG,
     .   THKE ,SIZP0)

      IF(STAT_C(3)==1) CALL STAT_C_EPSPF(
     .   ELBUF_TAB,IPARG ,IPM ,IGEO,IXC  ,
     .   IXTG ,WA,WAP0 ,IPARTC,IPARTTG,
     .   IPART_STATE,STAT_INDXC,STAT_INDXTG,
     .   THKE ,SIZP0)

      IF(STAT_C(4)==1) CALL STAT_C_STRSF(
     .   ELBUF_TAB,IPARG ,IPM ,IGEO,IXC  ,
     .   IXTG ,WA,WAP0 ,IPARTC,IPARTTG,
     .   IPART_STATE,STAT_INDXC,STAT_INDXTG,
     .   THKE ,SIZP0)

      IF(STAT_C(10)==1) CALL STAT_C_STRSFG(
     .   ELBUF_TAB,X,IPARG ,IPM ,IGEO,IXC  ,
     .   IXTG ,WA,WAP0 ,IPARTC,IPARTTG,
     .   IPART_STATE,STAT_INDXC,STAT_INDXTG,
     .   THKE ,SIZP0,GEO ,STACK,DRAPE_SH4N,DRAPE_SH3N,DRAPEG)

      IF(STAT_C(5)==1) CALL STAT_C_STRAF(
     .   ELBUF_TAB,IPARG ,IPM ,IGEO,IXC  ,
     .   IXTG ,WA,WAP0 ,IPARTC,IPARTTG,
     .   IPART_STATE,STAT_INDXC,STAT_INDXTG,
     .   THKE ,SIZP0)

      IF(STAT_C(11)==1) CALL STAT_C_STRAFG(
     .   ELBUF_TAB,X,IPARG ,IPM ,IGEO,IXC  ,
     .   IXTG ,WA,WAP0 ,IPARTC,IPARTTG,
     .   IPART_STATE,STAT_INDXC,STAT_INDXTG,
     .   THKE ,SIZP0,GEO ,STACK,DRAPE_SH4N,DRAPE_SH3N,DRAPEG)

      IF(STAT_C(6)==1) CALL STAT_C_AUXF(
     .   ELBUF_TAB,IPARG ,IPM ,IGEO,IXC  ,
     .   IXTG ,WA,WAP0 ,IPARTC,IPARTTG,
     .   IPART_STATE,STAT_INDXC,STAT_INDXTG,SIZP0)
     
C     
      IF(STAT_C(7)==1) CALL STAT_C_ORTH_LOC(
     .   ELBUF_TAB,IPARG ,IPM ,IGEO,IXC,
     .   IXTG ,WA,WAP0 ,IPARTC,IPARTTG,
     .   IPART_STATE,STAT_INDXC,STAT_INDXTG,X,STAT_C(9),SIZP0)
C     
      IF(STAT_C(8)==1) CALL STAT_C_FAIL(
     .   ELBUF_TAB,IPARG ,IPM ,IGEO,IXC  ,
     .   IXTG ,WA,WAP0 ,IPARTC,IPARTTG,
     .   IPART_STATE,STAT_INDXC,STAT_INDXTG,SIZP0,
     .   NUMMAT,MAT_PARAM)
C-----------------------------------------------
C   BRICK TENSOR
C-----------------------------------------------
      IF(STAT_S(4)==1) CALL STAT_S_STRSF(                     ! /INIBRI/STRESS/FULL
     1                        ELBUF_TAB ,IPARG ,IPM ,IGEO ,IXS ,
     2                        WA,WAP0 ,IPARTS, IPART_STATE,     
     3                        STAT_INDXS,X,0,IPART,SIZP0)
     
      IF(STAT_S(5)==1) CALL STAT_S_STRAF(                     ! /INIBRI/STRAIN/FULL
     1                        ELBUF_TAB ,IPARG ,IPM ,IGEO ,IXS ,
     2                        WA,WAP0 ,IPARTS, IPART_STATE,     
     3                        STAT_INDXS,X,0,IPART,SIZP0)
     
      IF(STAT_S(6)==1) CALL STAT_S_AUXF(                      ! /INIBRI/AUX
     1                        ELBUF_TAB ,IPARG ,IPM ,IGEO ,IXS ,
     2                        WA,WAP0 ,IPARTS, IPART_STATE,     
     3                        STAT_INDXS,IPART,SIZP0)
      IF(STAT_S(7)==1) CALL STAT_S_ORTHO(                     ! /INIBRI/ORTHO
     1                        ELBUF_TAB ,IPARG ,IPM ,IGEO ,IXS ,
     2                        WA,WAP0 ,IPARTS, IPART_STATE,     
     3                        STAT_INDXS,X,0,IPART,STAT_S(12),SIZP0)  
      IF(STAT_S(8)==1) CALL STAT_S_STRSF(                     ! /INIBRI/STRESS/GLOBF
     1                        ELBUF_TAB ,IPARG ,IPM ,IGEO ,IXS ,
     2                        WA,WAP0 ,IPARTS, IPART_STATE,     
     3                        STAT_INDXS,X,1,IPART,SIZP0)
     
      IF(STAT_S(9)==1) CALL STAT_S_STRAF(                     ! /INIBRI/STRAIN/GLOBF
     1                        ELBUF_TAB ,IPARG ,IPM ,IGEO ,IXS ,
     2                        WA,WAP0 ,IPARTS, IPART_STATE,     
     3                        STAT_INDXS,X,1,IPART,SIZP0)
      IF(STAT_S(10)==1) CALL STAT_S_ORTHO(                    ! /INIBRI/ORTHO/GLOBF
     1                        ELBUF_TAB ,IPARG ,IPM ,IGEO ,IXS ,
     2                        WA,WAP0 ,IPARTS, IPART_STATE,     
     3                        STAT_INDXS,X,1,IPART,STAT_S(12),SIZP0)  
      IF(STAT_S(11)==1) CALL STAT_S_FAIL(
     1                        ELBUF_TAB ,IPARG ,IPM ,IGEO ,IXS ,
     2                        WA,WAP0 ,IPARTS, IPART_STATE,
     3                        STAT_INDXS,IPART,SIZP0,NUMMAT,MAT_PARAM)
      IF(STAT_S(13)==1) CALL STAT_S_EREF(
     1                        ELBUF_TAB ,IPARG ,IPM ,IGEO ,
     2                        IXS ,IXS10,IXS16,IXS20,X   ,
     3                        DR  ,WA,WAP0 ,IPARTS, IPART_STATE,
     4                        STAT_INDXS,IPART,SIZP0)
C
C-----------------------------------------------
C  - 1D - spring elems (FULL)
C-----------------------------------------------
      IF(STAT_R(1) == 1) CALL STAT_R_FULL(
     1                        ELBUF_TAB ,IPARG ,GEO    ,IGEO        ,IXR       ,
     2                        WA        ,WAP0  ,IPARTR ,IPART_STATE ,STAT_INDXR,
     3                        SIZP0     )
C-----------------------------------------------
C  - 1D - beam elems (FULL + AUX)
C-----------------------------------------------
! - FULL -
      IF(STAT_P(1) == 1) CALL STAT_P_FULL(
     1                        ELBUF_TAB ,IPARG ,GEO    ,IGEO        ,IXP       ,
     2                        WA        ,WAP0  ,IPARTP ,IPART_STATE ,STAT_INDXP,
     3                        SIZP0     )
! - AUX -
      IF(STAT_P(3)==1) CALL STAT_P_AUX(
     1                        ELBUF_TAB  ,IPARG  ,IPM    ,IGEO       ,IXP       ,
     2                        WA         ,WAP0   ,IPARTP ,IPART_STATE,STAT_INDXP,
     3                        SIZP0      )
C-----------------------------------------------
C  - 1D - truss elems (FULL)
C-----------------------------------------------
! - FULL -
      IF(STAT_T(1) == 1) CALL STAT_T_FULL(
     1                        ELBUF_TAB   ,IPARG ,GEO    ,IGEO        ,IXT       ,
     2                        WA          ,WAP0  ,IPARTT ,IPART_STATE ,STAT_INDXT,
     3                        SIZP0       )
C-----------------------------------------------
C  - SPHCEL elems (FULL)
C-----------------------------------------------
! - FULL -
      IF(OUTPUT%STATE%STAT_SPH(3) == 1) CALL STAT_SPHCEL_FULL(
     1                        NUMSPH     ,NISP     ,NGROUP      ,NPARG         ,SIZLOC        ,
     2                        NPART      ,SIZP0    ,NSPMD       ,OUTPUT%STATE%STAT_NUMELSPH ,OUTPUT%STATE%STAT_NUMELSPH_G ,
     2                        NSPBUF     ,NUMNOD   ,NPROPMI     ,NUMMAT        ,LIPART1       ,
     4                        KXSP       ,IPARTSPH ,IPART_STATE ,STAT_INDXSPH  ,IPARG         ,
     5                        ELBUF_TAB  ,WA       ,WAP0        ,SPBUF         ,ITAB          , 
     6                        IPM        ,OUTPUT%STATE%STAT_SPH(2),IPART )
C-----------------------------------------------
      IF(SIZLOC >= 1) DEALLOCATE(WA)
      IF(SIZP0 >= 1) DEALLOCATE(WAP0)
C-----------------------------------------------
C   END
C-----------------------------------------------
      IF(IS_FILE_TO_BE_WRITTEN) THEN
        WRITE(IUGEO,'(A)')'#ENDDATA   '
        CLOSE(UNIT=IUGEO)
        IF(IZIPSTRS /= 0)THEN
          CALL STRS_TXT50('#ENDDATA   ',11)
          CALL CLOSE_C()
        ENDIF
        WRITE (IOUT,1000)  FILNAM(1:FILEN)
        WRITE (ISTDO,1000) FILNAM(1:FILEN)
      ENDIF
C-----------------------------------------------
      DEALLOCATE(ITABG)
      DEALLOCATE(NODTAG)
      DEALLOCATE(STAT_INDXC)
      DEALLOCATE(STAT_INDXTG)
      DEALLOCATE(STAT_INDXQ)
      DEALLOCATE(STAT_INDXS)
      DEALLOCATE(STAT_INDXR)
      DEALLOCATE(STAT_INDXP)
      DEALLOCATE(STAT_INDXT)
      DEALLOCATE(STAT_INDXSPH)
      
 1000 FORMAT (4X,' STATE FILE:',1X,A,' WRITTEN')
C
      RETURN
      END
