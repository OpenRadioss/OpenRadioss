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
Chd|  LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|-- called by -----------
Chd|        LECTUR                        source/input/lectur.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        CREATE_H3D_1D_SCALAR          source/output/h3d/h3d_build_fortran/create_h3d_1d_scalar.F
Chd|        CREATE_H3D_1D_TENSOR          source/output/h3d/h3d_build_fortran/create_h3d_1d_tensor.F
Chd|        CREATE_H3D_1D_TORSOR          source/output/h3d/h3d_build_fortran/create_h3d_1d_torsor.F
Chd|        CREATE_H3D_1D_VECTOR          source/output/h3d/h3d_build_fortran/create_h3d_1d_vector.F
Chd|        CREATE_H3D_ARG_KEYWORD        source/output/h3d/h3d_build_fortran/create_h3d_arg_keyword.F
Chd|        CREATE_H3D_NODAL_SCALAR       source/output/h3d/h3d_build_fortran/create_h3d_nodal_scalar.F
Chd|        CREATE_H3D_NODAL_TENSOR       source/output/h3d/h3d_build_fortran/create_h3d_nodal_tensor.F
Chd|        CREATE_H3D_NODAL_VECTOR       source/output/h3d/h3d_build_fortran/create_h3d_nodal_vector.F
Chd|        CREATE_H3D_PARTS              source/output/h3d/h3d_build_fortran/create_h3d_parts.F
Chd|        CREATE_H3D_PARTS_ALL          source/output/h3d/h3d_build_fortran/create_h3d_parts_all.F
Chd|        CREATE_H3D_QUAD_SCALAR        source/output/h3d/h3d_build_fortran/create_h3d_quad_scalar.F
Chd|        CREATE_H3D_QUAD_TENSOR        source/output/h3d/h3d_build_fortran/create_h3d_quad_tensor.F
Chd|        CREATE_H3D_QUAD_VECTOR        source/output/h3d/h3d_build_fortran/create_h3d_quad_vector.F
Chd|        CREATE_H3D_SHELL_SCALAR       source/output/h3d/h3d_build_fortran/create_h3d_shell_scalar.F
Chd|        CREATE_H3D_SHELL_TENSOR       source/output/h3d/h3d_build_fortran/create_h3d_shell_tensor.F
Chd|        CREATE_H3D_SHELL_VECTOR       source/output/h3d/h3d_build_fortran/create_h3d_shell_vector.F
Chd|        CREATE_H3D_SKIN_SCALAR        source/output/h3d/h3d_build_fortran/create_h3d_skin_scalar.F
Chd|        CREATE_H3D_SKIN_TENSOR        source/output/h3d/h3d_build_fortran/create_h3d_skin_tensor.F
Chd|        CREATE_H3D_SKIN_VECTOR        source/output/h3d/h3d_build_fortran/create_h3d_skin_vector.F
Chd|        CREATE_H3D_SOLID_SCALAR       source/output/h3d/h3d_build_fortran/create_h3d_solid_scalar.F
Chd|        CREATE_H3D_SOLID_TENSOR       source/output/h3d/h3d_build_fortran/create_h3d_solid_tensor.F
Chd|        CREATE_H3D_SOLID_VECTOR       source/output/h3d/h3d_build_fortran/create_h3d_solid_vector.F
Chd|        CREATE_H3D_SPH_SCALAR         source/output/h3d/h3d_build_fortran/create_h3d_sph_scalar.F
Chd|        CREATE_H3D_SPH_TENSOR         source/output/h3d/h3d_build_fortran/create_h3d_sph_tensor.F
Chd|        H3D_GENE_KEYWORD              source/output/h3d/input_list/h3d_gene_keyword.F
Chd|        H3D_LIST_1D_SCALAR            source/output/h3d/input_list/h3d_list_1d_scalar.F
Chd|        H3D_LIST_1D_TENSOR            source/output/h3d/input_list/h3d_list_1d_tensor.F
Chd|        H3D_LIST_1D_TORSOR            source/output/h3d/input_list/h3d_list_1d_torsor.F
Chd|        H3D_LIST_1D_VECTOR            source/output/h3d/input_list/h3d_list_1d_vector.F
Chd|        H3D_LIST_NODAL_SCALAR         source/output/h3d/input_list/h3d_list_noda_scalar.F
Chd|        H3D_LIST_NODAL_TENSOR         source/output/h3d/input_list/h3d_list_noda_tensor.F
Chd|        H3D_LIST_NODAL_VECTOR         source/output/h3d/input_list/h3d_list_noda_vector.F
Chd|        H3D_LIST_QUAD_SCALAR          source/output/h3d/input_list/h3d_list_quad_scalar.F
Chd|        H3D_LIST_QUAD_TENSOR          source/output/h3d/input_list/h3d_list_quad_tensor.F
Chd|        H3D_LIST_QUAD_VECTOR          source/output/h3d/input_list/h3d_list_quad_vector.F
Chd|        H3D_LIST_SHELL_SCALAR         source/output/h3d/input_list/h3d_list_shell_scalar.F
Chd|        H3D_LIST_SHELL_TENSOR         source/output/h3d/input_list/h3d_list_shell_tensor.F
Chd|        H3D_LIST_SHELL_VECTOR         source/output/h3d/input_list/h3d_list_shell_vector.F
Chd|        H3D_LIST_SOLID_SCALAR         source/output/h3d/input_list/h3d_list_solid_scalar.F
Chd|        H3D_LIST_SOLID_TENSOR         source/output/h3d/input_list/h3d_list_solid_tensor.F
Chd|        H3D_LIST_SOLID_VECTOR         source/output/h3d/input_list/h3d_list_solid_vector.F
Chd|        H3D_LIST_SPH_SCALAR           source/output/h3d/input_list/h3d_list_sph_scalar.F
Chd|        H3D_LIST_SPH_TENSOR           source/output/h3d/input_list/h3d_list_sph_tensor.F
Chd|        H3D_SKIN_DIM                  source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        H3D_INC_MOD                   share/modules/h3d_inc_mod.F   
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|        LOADS_MOD                     ../common_source/modules/loads/loads_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE LECH3D(GEO,IGEO,IPM,IPART,H3D_DATA,MULTI_FVM,IPARI,IPARG,TAG_SKINS6,
     .                  MDS_LABEL,MDS_OUTPUT_TABLE,MDS_NMAT,MAX_DEPVAR,MDS_NDEPSVAR,
     .                  ELBUF_STR,STACK,IBCL,ILOADP,LLOADP,LOADS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE MESSAGE_MOD
      USE STACK_MOD
      USE H3D_MOD
      USE MULTI_FVM_MOD
      USE ELBUFDEF_MOD     
      USE STACK_MOD    
      USE H3D_INC_MOD
      USE LOADS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real GEO(NPROPG,NUMGEO)
      INTEGER IGEO(NPROPGI,NUMGEO),IPART(LIPART1,*),IPM(NPROPMI,NUMMAT),IPARI(NPARI,*),
     .        IPARG(NPARG,NGROUP),TAG_SKINS6(*),MDS_OUTPUT_TABLE(MAX_DEPVAR,MDS_NMAT),MDS_NMAT,
     .        MAX_DEPVAR,MDS_NDEPSVAR(*),IBCL(*),ILOADP(*),LLOADP(*)
      TYPE (H3D_DATABASE) :: H3D_DATA
      TYPE (MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      CHARACTER*64 MDS_LABEL(1024,MDS_NMAT)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_STR
      TYPE (STACK_PLY) :: STACK
      TYPE (LOADS_), INTENT(IN)    :: LOADS
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "spmd_c.inc"
#include      "vect01_c.inc"
#include      "nchar_c.inc"  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,JMAX,K,L,M,O,P,Q,CPT,NIP_MAX,NLAY_MAX,NPLY_MAX,NUVAR_MAX,ID_INPUT,N1,N2,N3,N4,N5,N6,
     .        IS_INTEGER_N3,IS_INTEGER_N4,IS_INTEGER_N5,IS_INTEGER_N6,
     .        NB_INTEGER,N,IR_MAX,IS_MAX,IT_MAX,
     .        NB_ALL,IS_CHAR_KEY2,IS_CHAR_KEY3,IS_CHAR_KEY4,IS_CHAR_KEY5,IS_CHAR_KEY6,IS_CHAR_KEY7,IS_CHAR_KEY8,
     .        IS_EMPTY_KEY2,IS_EMPTY_KEY3,IS_EMPTY_KEY4,IS_EMPTY_KEY5,IS_EMPTY_KEY6,IS_EMPTY_KEY7,IS_EMPTY_KEY8,
     .        IS_PLY_KEY4,IS_PLY_KEY5,IS_PLY_KEY6,IS_PLY_KEY7,IS_PLY_KEY8,IBID,IS_ALL,IS_UPPER,IS_LOWER,IS_DEF,
     .        IS_LAYER_KEY4,IS_LAYER_KEY5,IS_LAYER_KEY6,IS_LAYER_KEY7,IS_LAYER_KEY8,IS_IPT_KEY4,IS_IPT_KEY5,IS_IPT_KEY6,
     .        IS_IPT_KEY7,IS_IPT_KEY8,
     .        IS_GAUSS_KEY4,IS_GAUSS_KEY5,IS_GAUSS_KEY6,IS_GAUSS_KEY7,IS_GAUSS_KEY8,IS_PLY,IS_LAYER,IS_IPT,IS_GAUSS,
     .        IS_PLY_ALL,IS_LAYER_ALL,IS_LAYER_LOWER,IS_LAYER_UPPER,IS_LAYER_MEMB,IS_LAYER_BEND,
     .        IS_IPT_ALL,IS_IPT_LOWER,IS_IPT_UPPER,IS_IPT_MEMB,IS_IPT_BEND,
     .        IS_GAUSS_ALL,PLY,LAYER,IPT,GAUSS,IOK_H3DKEY,CPT_H3D,IUVAR,IS_UVAR,
     .        IS_UVAR_ALL,IS_UVAR_KEY4,IS_UVAR_KEY5,IS_UVAR_KEY6,IS_UVAR_KEY7,IS_UVAR_KEY8,
     .        ID_PLY(NUMGEO+NUMPLY),
     .        IPT_PLY(NUMGEO+NUMPLY),IS_SCALAR,IS_VECTOR,IS_TENSOR,IS_TORSOR,
     .        IR,IS_IR,IS_IR_ALL,IS_IR_LOWER,IS_IR_UPPER , 
     .        IS,IS_IS,IS_IS_ALL,IS_IS_LOWER,IS_IS_UPPER , 
     .        IT,IS_IT,IS_IT_ALL,IS_IT_LOWER,IS_IT_UPPER,
     .        NB_KEY,CPT_KEY,NBMAX,IS_INTER,INTER,NINTER_MAX,IS_INTER_ALL,NINTERSKID,IS_SKING,
     .        ISHELL_NPT_CHECK,ICSTR,NPTR,NPTS,NPTT,IS_CORNER_DATA,ISH_NPT0,
     .        IS_MDSVAR_DEF,NMDSVAR_MAX,IS_MDSVAR,IS_MDSVAR_ALL,IMDSVAR,
     .        IS_MODEL_NPT,IS_MODEL_PLY,IS_MODEL_LAYER,ISKIND,IOUTER,IPEXT,
     .        IS_ID,ID,ID_MAX,IS_ID_ALL,NINEFRIC,N19,IFAIL,CPT_IRUP,CPT_IRUP2
      INTEGER MLW,NEL,NG,JTURB,NLAY,NUVAR,IPLY,IMAT,ISUBSTACK,ID_PLY_TMP
      CHARACTER  KEY2*ncharkey, KEY3*ncharkey, KEY4*ncharkey, KEY5*ncharkey, KEY6*ncharkey,
     .           KEY7*ncharkey,KEY8*ncharkey,KEY3_GLOB*ncharline,KEY3_READ*ncharline, 
     .           KEY2_READ*ncharkey

      CHARACTER*64 MDS_LABEL_TMP
      
      INTEGER, DIMENSION(:), ALLOCATABLE ::
     .      LAYER_INPUT,IPT_INPUT,PLY_INPUT,UVAR_INPUT,GAUSS_INPUT,
     .      IR_INPUT,IS_INPUT,IT_INPUT,IS_AVAILABLE_KEY,INTER_INPUT,
     .      MDSVAR_INPUT,MDSVAR_INPUT1,MDSVAR_INPUT_TMP,MDSVAR_INPUT1_TMP,
     .      MDSVAR_INPUT2,MDSVAR_INPUT2_TMP,ID_MAT_MDS,INDEX_MAT_MDS,IDS_INPUT,
     .      IRUP_ALL,IRUP_ID_ALL,IRUP_H3D
c
      TYPE (H3D_KEYWORD) H3D_KEYWORD_NODE,H3D_KEYWORD_SHELL,H3D_KEYWORD_SOLID,H3D_KEYWORD_QUAD,H3D_KEYWORD_ONED
c
      TYPE (H3D_KEYWORD), DIMENSION(:), ALLOCATABLE :: H3D_KEYWORD_NODAL_SCALAR
      TYPE (H3D_KEYWORD), DIMENSION(:), ALLOCATABLE :: H3D_KEYWORD_NODAL_VECTOR
      TYPE (H3D_KEYWORD), DIMENSION(:), ALLOCATABLE :: H3D_KEYWORD_NODAL_TENSOR
      TYPE (H3D_KEYWORD), DIMENSION(:), ALLOCATABLE :: H3D_KEYWORD_ONED_SCALAR
      TYPE (H3D_KEYWORD), DIMENSION(:), ALLOCATABLE :: H3D_KEYWORD_ONED_VECTOR
      TYPE (H3D_KEYWORD), DIMENSION(:), ALLOCATABLE :: H3D_KEYWORD_ONED_TENSOR
      TYPE (H3D_KEYWORD), DIMENSION(:), ALLOCATABLE :: H3D_KEYWORD_ONED_TORSOR
      TYPE (H3D_KEYWORD), DIMENSION(:), ALLOCATABLE :: H3D_KEYWORD_SHELL_SCALAR
      TYPE (H3D_KEYWORD), DIMENSION(:), ALLOCATABLE :: H3D_KEYWORD_SHELL_VECTOR
      TYPE (H3D_KEYWORD), DIMENSION(:), ALLOCATABLE :: H3D_KEYWORD_SHELL_TENSOR
      TYPE (H3D_KEYWORD), DIMENSION(:), ALLOCATABLE :: H3D_KEYWORD_SOLID_SCALAR
      TYPE (H3D_KEYWORD), DIMENSION(:), ALLOCATABLE :: H3D_KEYWORD_SOLID_VECTOR
      TYPE (H3D_KEYWORD), DIMENSION(:), ALLOCATABLE :: H3D_KEYWORD_SOLID_TENSOR
      TYPE (H3D_KEYWORD), DIMENSION(:), ALLOCATABLE :: H3D_KEYWORD_SPH_SCALAR
      TYPE (H3D_KEYWORD), DIMENSION(:), ALLOCATABLE :: H3D_KEYWORD_SPH_TENSOR
      TYPE (H3D_KEYWORD), DIMENSION(:), ALLOCATABLE :: H3D_KEYWORD_QUAD_SCALAR
      TYPE (H3D_KEYWORD), DIMENSION(:), ALLOCATABLE :: H3D_KEYWORD_QUAD_VECTOR
      TYPE (H3D_KEYWORD), DIMENSION(:), ALLOCATABLE :: H3D_KEYWORD_QUAD_TENSOR
      
      TYPE (H3D_NUMBER_OF_KEY) :: H3D_NUM_KEY
  
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY 
      TYPE(BUF_FAIL_) ,POINTER :: FBUF 
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IS_LAYER_MAT 
      LOGICAL :: FOUND
      CHARACTER*64 TEST_CHAIN
      CHARACTER*10 FAIL_NAME(50)
      DATA  FAIL_NAME/
     1 'JOHNSON   ','TBUTCHER  ','WILKINS   ','USER1     ','USER2     ',
     2 'USER3     ','FLD       ','SPALLING  ','WIERZBICKI','TENSSTRAIN',
     3 'ENERGY    ','          ','CHANG     ','HASHIN    ','          ',
     4 'PUCK      ','          ','LADEVEZE  ','          ','CONNECT   ',
     5 '          ','          ','TAB1      ','ORTHSTRAIN','NXT       ',
     6 'SNCONNECT ','EMC       ','ALTER     ','SAHRAEI   ','BIQUAD    ',
     7 'FABRIC    ','HC_DSSE   ','MULLINS   ','COCKCROFT ','GURSON    ',
     8 'VISUAL    ','TAB_OLD   ','ORTHBIQUAD','GENE1     ','RTCL      ',
     9 'TAB2      ','INIEVO    ','SYAZWAN   ','TSAIWU    ','TSAIHILL  ',
     A 'HOFFMAN   ','MAXSTRAIN ','          ','          ','          '/
C=========================================================================
      IF(H3D_DATA%TH3D0 /= ZERO)  H3D_DATA%TH3D = H3D_DATA%TH3D0
      IF(H3D_DATA%TH3D_STOP0 /= EP20)  H3D_DATA%TH3D_STOP = H3D_DATA%TH3D_STOP0      
      IF(H3D_DATA%DTH3D0 > ZERO) H3D_DATA%DTH3D= H3D_DATA%DTH3D0
      IF(H3D_DATA%DTH3D <= ZERO)  THEN 
        H3D_DATA%TH3D = EP30
        H3D_DATA%TH3D_STOP = ZERO
      ENDIF
      IF (H3D_DATA%TH3D < TT-DT2 .AND. H3D_DATA%DTH3D > ZERO) H3D_DATA%TH3D = H3D_DATA%TH3D
     .                                                                + INT((TT-DT2-H3D_DATA%TH3D)/H3D_DATA%DTH3D)*H3D_DATA%DTH3D
      IF (H3D_DATA%TH3D < TT-DT2)H3D_DATA%TH3D = H3D_DATA%TH3D+H3D_DATA%DTH3D
C=========================================================================
      IBID = 0
c
      H3D_DATA%IPART_SELECT = 0
      IS_SCALAR = 0
      IS_VECTOR = 0
      IS_TENSOR = 0
      NB_INTEGER = 0
      NB_ALL = 0
      IS_SKING = 0
      NLAY_MAX = 0
      ALLOCATE(IRUP_ALL(1))
      IRUP_ALL(1) = 0
      CPT_IRUP = 0

      ALLOCATE(H3D_DATA%N_SKID_INTER(NINTER))
      H3D_DATA%N_SKID_INTER(1:NINTER) = 0
      NINTERSKID = 0
 
      ALLOCATE(H3D_DATA%N_CSE_FRIC_INTER(NINTER))
      H3D_DATA%N_CSE_FRIC_INTER(1:NINTER) = 0
      NINEFRIC = 0
      MDS_LABEL_TMP=' '
C-------------------------------------------------- 
C ALLOCATE / INIT KEYWORD LISTS
C--------------------------------------------------
      NBMAX = 1000 
      ALLOCATE(H3D_KEYWORD_NODAL_SCALAR(NBMAX))
       H3D_KEYWORD_NODAL_SCALAR(1:NBMAX)%KEY3=''
       H3D_KEYWORD_NODAL_SCALAR(1:NBMAX)%KEY4=''
       H3D_KEYWORD_NODAL_SCALAR(1:NBMAX)%KEY5=''
       H3D_KEYWORD_NODAL_SCALAR(1:NBMAX)%KEY6=''
       H3D_KEYWORD_NODAL_SCALAR(1:NBMAX)%COMMENT='' 
      ALLOCATE(H3D_KEYWORD_NODAL_VECTOR(NBMAX))
       H3D_KEYWORD_NODAL_VECTOR(1:NBMAX)%KEY3=''
       H3D_KEYWORD_NODAL_VECTOR(1:NBMAX)%KEY4=''
       H3D_KEYWORD_NODAL_VECTOR(1:NBMAX)%KEY5=''
       H3D_KEYWORD_NODAL_VECTOR(1:NBMAX)%KEY6=''
       H3D_KEYWORD_NODAL_VECTOR(1:NBMAX)%COMMENT='' 
      ALLOCATE(H3D_KEYWORD_NODAL_TENSOR(NBMAX))
       H3D_KEYWORD_NODAL_TENSOR(1:NBMAX)%KEY3=''
       H3D_KEYWORD_NODAL_TENSOR(1:NBMAX)%KEY4=''
       H3D_KEYWORD_NODAL_TENSOR(1:NBMAX)%KEY5=''
       H3D_KEYWORD_NODAL_TENSOR(1:NBMAX)%KEY6=''
       H3D_KEYWORD_NODAL_TENSOR(1:NBMAX)%COMMENT=''       
      ALLOCATE(H3D_KEYWORD_ONED_SCALAR(NBMAX))
       H3D_KEYWORD_ONED_SCALAR(1:NBMAX)%KEY3=''
       H3D_KEYWORD_ONED_SCALAR(1:NBMAX)%KEY4=''
       H3D_KEYWORD_ONED_SCALAR(1:NBMAX)%KEY5=''
       H3D_KEYWORD_ONED_SCALAR(1:NBMAX)%KEY6=''
       H3D_KEYWORD_ONED_SCALAR(1:NBMAX)%COMMENT=''       
      ALLOCATE(H3D_KEYWORD_ONED_VECTOR(NBMAX))
       H3D_KEYWORD_ONED_VECTOR(1:NBMAX)%KEY3=''
       H3D_KEYWORD_ONED_VECTOR(1:NBMAX)%KEY4=''
       H3D_KEYWORD_ONED_VECTOR(1:NBMAX)%KEY5=''
       H3D_KEYWORD_ONED_VECTOR(1:NBMAX)%KEY6=''
       H3D_KEYWORD_ONED_VECTOR(1:NBMAX)%COMMENT=''       
      ALLOCATE(H3D_KEYWORD_ONED_TENSOR(NBMAX))
       H3D_KEYWORD_ONED_TENSOR(1:NBMAX)%KEY3=''
       H3D_KEYWORD_ONED_TENSOR(1:NBMAX)%KEY4=''
       H3D_KEYWORD_ONED_TENSOR(1:NBMAX)%KEY5=''
       H3D_KEYWORD_ONED_TENSOR(1:NBMAX)%KEY6=''
       H3D_KEYWORD_ONED_TENSOR(1:NBMAX)%COMMENT=''       
      ALLOCATE(H3D_KEYWORD_ONED_TORSOR(NBMAX))
       H3D_KEYWORD_ONED_TORSOR(1:NBMAX)%KEY3=''
       H3D_KEYWORD_ONED_TORSOR(1:NBMAX)%KEY4=''
       H3D_KEYWORD_ONED_TORSOR(1:NBMAX)%KEY5=''
       H3D_KEYWORD_ONED_TORSOR(1:NBMAX)%KEY6=''
       H3D_KEYWORD_ONED_TORSOR(1:NBMAX)%COMMENT=''       
      ALLOCATE(H3D_KEYWORD_SHELL_SCALAR(NBMAX))
       H3D_KEYWORD_SHELL_SCALAR(1:NBMAX)%KEY3=''
       H3D_KEYWORD_SHELL_SCALAR(1:NBMAX)%KEY4=''
       H3D_KEYWORD_SHELL_SCALAR(1:NBMAX)%KEY5=''
       H3D_KEYWORD_SHELL_SCALAR(1:NBMAX)%KEY6=''
       H3D_KEYWORD_SHELL_SCALAR(1:NBMAX)%COMMENT=''
      ALLOCATE(H3D_KEYWORD_SHELL_VECTOR(NBMAX))
       H3D_KEYWORD_SHELL_VECTOR(1:NBMAX)%KEY3=''
       H3D_KEYWORD_SHELL_VECTOR(1:NBMAX)%KEY4=''
       H3D_KEYWORD_SHELL_VECTOR(1:NBMAX)%KEY5=''
       H3D_KEYWORD_SHELL_VECTOR(1:NBMAX)%KEY6=''
       H3D_KEYWORD_SHELL_VECTOR(1:NBMAX)%COMMENT=''
      ALLOCATE(H3D_KEYWORD_SHELL_TENSOR(NBMAX))
       H3D_KEYWORD_SHELL_TENSOR(1:NBMAX)%KEY3=''
       H3D_KEYWORD_SHELL_TENSOR(1:NBMAX)%KEY4=''
       H3D_KEYWORD_SHELL_TENSOR(1:NBMAX)%KEY5=''
       H3D_KEYWORD_SHELL_TENSOR(1:NBMAX)%KEY6=''
       H3D_KEYWORD_SHELL_TENSOR(1:NBMAX)%COMMENT=''
      ALLOCATE(H3D_KEYWORD_SOLID_SCALAR(NBMAX))
       H3D_KEYWORD_SOLID_SCALAR(1:NBMAX)%KEY3=''
       H3D_KEYWORD_SOLID_SCALAR(1:NBMAX)%KEY4=''
       H3D_KEYWORD_SOLID_SCALAR(1:NBMAX)%KEY5=''
       H3D_KEYWORD_SOLID_SCALAR(1:NBMAX)%KEY6=''
       H3D_KEYWORD_SOLID_SCALAR(1:NBMAX)%COMMENT=''       
      ALLOCATE(H3D_KEYWORD_SOLID_VECTOR(NBMAX))
       H3D_KEYWORD_SOLID_VECTOR(1:NBMAX)%KEY3=''
       H3D_KEYWORD_SOLID_VECTOR(1:NBMAX)%KEY4=''
       H3D_KEYWORD_SOLID_VECTOR(1:NBMAX)%KEY5=''
       H3D_KEYWORD_SOLID_VECTOR(1:NBMAX)%KEY6=''
       H3D_KEYWORD_SOLID_VECTOR(1:NBMAX)%COMMENT=''       
      ALLOCATE(H3D_KEYWORD_SOLID_TENSOR(NBMAX))
       H3D_KEYWORD_SOLID_TENSOR(1:NBMAX)%KEY3=''
       H3D_KEYWORD_SOLID_TENSOR(1:NBMAX)%KEY4=''
       H3D_KEYWORD_SOLID_TENSOR(1:NBMAX)%KEY5=''
       H3D_KEYWORD_SOLID_TENSOR(1:NBMAX)%KEY6=''
       H3D_KEYWORD_SOLID_TENSOR(1:NBMAX)%COMMENT=''       
      ALLOCATE(H3D_KEYWORD_SPH_SCALAR(NBMAX))
       H3D_KEYWORD_SPH_SCALAR(1:NBMAX)%KEY3=''
       H3D_KEYWORD_SPH_SCALAR(1:NBMAX)%KEY4=''
       H3D_KEYWORD_SPH_SCALAR(1:NBMAX)%KEY5=''
       H3D_KEYWORD_SPH_SCALAR(1:NBMAX)%KEY6=''
       H3D_KEYWORD_SPH_SCALAR(1:NBMAX)%COMMENT=''       
      ALLOCATE(H3D_KEYWORD_SPH_TENSOR(NBMAX))
       H3D_KEYWORD_SPH_TENSOR(1:NBMAX)%KEY3=''
       H3D_KEYWORD_SPH_TENSOR(1:NBMAX)%KEY4=''
       H3D_KEYWORD_SPH_TENSOR(1:NBMAX)%KEY5=''
       H3D_KEYWORD_SPH_TENSOR(1:NBMAX)%KEY6=''
       H3D_KEYWORD_SPH_TENSOR(1:NBMAX)%COMMENT=''       
      ALLOCATE(H3D_KEYWORD_QUAD_SCALAR(NBMAX))
       H3D_KEYWORD_QUAD_SCALAR(1:NBMAX)%KEY3=''
       H3D_KEYWORD_QUAD_SCALAR(1:NBMAX)%KEY4=''
       H3D_KEYWORD_QUAD_SCALAR(1:NBMAX)%KEY5=''
       H3D_KEYWORD_QUAD_SCALAR(1:NBMAX)%KEY6=''
       H3D_KEYWORD_QUAD_SCALAR(1:NBMAX)%COMMENT=''       
      ALLOCATE(H3D_KEYWORD_QUAD_VECTOR(NBMAX))
       H3D_KEYWORD_QUAD_VECTOR(1:NBMAX)%KEY3=''
       H3D_KEYWORD_QUAD_VECTOR(1:NBMAX)%KEY4=''
       H3D_KEYWORD_QUAD_VECTOR(1:NBMAX)%KEY5=''
       H3D_KEYWORD_QUAD_VECTOR(1:NBMAX)%KEY6=''
       H3D_KEYWORD_QUAD_VECTOR(1:NBMAX)%COMMENT=''       
      ALLOCATE(H3D_KEYWORD_QUAD_TENSOR(NBMAX))
       H3D_KEYWORD_QUAD_TENSOR(1:NBMAX)%KEY3=''
       H3D_KEYWORD_QUAD_TENSOR(1:NBMAX)%KEY4=''
       H3D_KEYWORD_QUAD_TENSOR(1:NBMAX)%KEY5=''
       H3D_KEYWORD_QUAD_TENSOR(1:NBMAX)%KEY6=''
       H3D_KEYWORD_QUAD_TENSOR(1:NBMAX)%COMMENT=''       
c
      ALLOCATE(ID_MAT_MDS(MAX(1,MDS_NMAT)))
      ID_MAT_MDS(1:MAX(1,MDS_NMAT)) = 0
      ALLOCATE(INDEX_MAT_MDS(MAX(1,MDS_NMAT)))
      INDEX_MAT_MDS(1:MAX(1,MDS_NMAT)) = 0
C-------------------------------------------------- 
C BUILD KEYWORD LISTS
C-------------------------------------------------- 
      CALL H3D_LIST_NODAL_SCALAR(H3D_KEYWORD_NODAL_SCALAR, GEO, H3D_NUM_KEY%NODAL_SCALAR)
C
      CALL H3D_LIST_NODAL_VECTOR(H3D_KEYWORD_NODAL_VECTOR, GEO, H3D_NUM_KEY%NODAL_VECTOR)
C
      CALL H3D_LIST_NODAL_TENSOR(H3D_KEYWORD_NODAL_TENSOR, GEO ,H3D_NUM_KEY%NODAL_TENSOR)
C
      CALL H3D_LIST_1D_SCALAR(H3D_KEYWORD_ONED_SCALAR, GEO, H3D_NUM_KEY%ONED_SCALAR)
C
      CALL H3D_LIST_1D_VECTOR(H3D_KEYWORD_ONED_VECTOR, GEO, H3D_NUM_KEY%ONED_VECTOR)
C
      CALL H3D_LIST_1D_TENSOR(H3D_KEYWORD_ONED_TENSOR, GEO, H3D_NUM_KEY%ONED_TENSOR)
C
      CALL H3D_LIST_1D_TORSOR(H3D_KEYWORD_ONED_TORSOR, GEO, H3D_NUM_KEY%ONED_TORSOR)
C
      CALL H3D_LIST_SHELL_SCALAR(H3D_KEYWORD_SHELL_SCALAR, GEO, IGEO, MULTI_FVM, H3D_NUM_KEY%SHELL_SCALAR)      
C
      CALL H3D_LIST_SHELL_VECTOR(H3D_KEYWORD_SHELL_VECTOR, GEO, IGEO, H3D_NUM_KEY%SHELL_VECTOR)
C
      CALL H3D_LIST_SHELL_TENSOR(H3D_KEYWORD_SHELL_TENSOR, GEO, IGEO, H3D_NUM_KEY%SHELL_TENSOR)
C
      CALL H3D_LIST_SOLID_SCALAR(H3D_KEYWORD_SOLID_SCALAR, GEO, IGEO, MULTI_FVM, H3D_NUM_KEY%SOLID_SCALAR)
C
      CALL H3D_LIST_SOLID_VECTOR(H3D_KEYWORD_SOLID_VECTOR, GEO, IGEO, H3D_NUM_KEY%SOLID_VECTOR)
C
      CALL H3D_LIST_SOLID_TENSOR(H3D_KEYWORD_SOLID_TENSOR, GEO, IGEO, H3D_NUM_KEY%SOLID_TENSOR) 
C
      CALL H3D_LIST_SPH_SCALAR(H3D_KEYWORD_SPH_SCALAR, GEO, IGEO, H3D_NUM_KEY%SPH_SCALAR) 
C
      CALL H3D_LIST_SPH_TENSOR(H3D_KEYWORD_SPH_TENSOR, GEO, IGEO, H3D_NUM_KEY%SPH_TENSOR) 
C
      CALL H3D_LIST_QUAD_SCALAR(H3D_KEYWORD_QUAD_SCALAR, GEO, IGEO, MULTI_FVM, H3D_NUM_KEY%QUAD_SCALAR) 
C
      CALL H3D_LIST_QUAD_TENSOR(H3D_KEYWORD_QUAD_TENSOR, GEO, IGEO, H3D_NUM_KEY%QUAD_TENSOR)
C
      CALL H3D_LIST_QUAD_VECTOR(H3D_KEYWORD_QUAD_VECTOR, GEO, IGEO, H3D_NUM_KEY%QUAD_VECTOR) 
C-------------------------------------------------- 
C BUILD OUTPUTS
C-------------------------------------------------- 
      IOUTER=0
      IPEXT=0
      DO ID_INPUT=1,H3D_DATA%N_INPUT_H3D
c
         CPT_H3D = 0
         IOK_H3DKEY = 0
         KEY3_GLOB = ''
c
         KEY2 = H3D_DATA%INPUT_LIST(ID_INPUT)%KEY2
         KEY3 = H3D_DATA%INPUT_LIST(ID_INPUT)%KEY3
         KEY4 = H3D_DATA%INPUT_LIST(ID_INPUT)%KEY4
         KEY5 = H3D_DATA%INPUT_LIST(ID_INPUT)%KEY5
         KEY6 = H3D_DATA%INPUT_LIST(ID_INPUT)%KEY6
         KEY7 = H3D_DATA%INPUT_LIST(ID_INPUT)%KEY7
         KEY8 = H3D_DATA%INPUT_LIST(ID_INPUT)%KEY8
c
         IS_CHAR_KEY2 = 1
         IS_EMPTY_KEY2 = 1
         IS_CHAR_KEY3 = 1
         IS_EMPTY_KEY3 = 1
         IS_CHAR_KEY4 = 1
         IS_EMPTY_KEY4 = 1
         IS_CHAR_KEY5 = 1
         IS_EMPTY_KEY5 = 1
         IS_CHAR_KEY6 = 1
         IS_EMPTY_KEY6 = 1
         IS_CHAR_KEY7 = 1
         IS_EMPTY_KEY7 = 1
         IS_CHAR_KEY8 = 1
         IS_EMPTY_KEY8 = 1
         PLY  = 0
         IS_PLY  = 0
         IS_PLY_ALL = 0
         IS_PLY_KEY4 = 0
         IS_PLY_KEY5 = 0
         IS_PLY_KEY6 = 0
         IS_PLY_KEY7 = 0
         IS_PLY_KEY8 = 0
         LAYER  = 0
         IS_LAYER  = 0
         IS_LAYER_LOWER = 0
         IS_LAYER_UPPER = 0
         IS_LAYER_MEMB = 0
         IS_LAYER_BEND = 0
         IS_LAYER_ALL = 0
         IS_LAYER_KEY4 = 0
         IS_LAYER_KEY5 = 0
         IS_LAYER_KEY6 = 0
         IS_LAYER_KEY7 = 0
         IS_LAYER_KEY8 = 0
         IPT = 0
         IS_IPT = 0
         IS_IPT_LOWER = 0
         IS_IPT_UPPER = 0
         IS_IPT_MEMB = 0
         IS_IPT_BEND = 0
         IS_IPT_ALL = 0
         IS_IPT_KEY4 = 0
         IS_IPT_KEY5 = 0
         IS_IPT_KEY6 = 0
         IS_IPT_KEY7 = 0
         IS_IPT_KEY8 = 0
         GAUSS = 0
         IS_GAUSS = 0
         IS_GAUSS_ALL = 0
         IS_GAUSS_KEY4 = 0
         IS_GAUSS_KEY5 = 0
         IS_GAUSS_KEY6 = 0
         IS_GAUSS_KEY7 = 0
         IS_GAUSS_KEY8 = 0
         IUVAR = 0
         IS_UVAR = 0
         IS_UVAR_ALL = 0
         IS_UVAR_KEY4 = 0
         IS_UVAR_KEY5 = 0
         IS_UVAR_KEY6 = 0
         IS_UVAR_KEY7 = 0
         IS_UVAR_KEY8 = 0
         INTER  = 0
         IS_INTER  = 0
         IS_INTER_ALL  = 0
         IS_CORNER_DATA  = 0
         IMDSVAR = 0
         IS_MDSVAR = 0
         IS_MDSVAR_ALL = 0
         IS_MDSVAR_DEF  = 0
         IF ( KEY3=='PEXT'.OR.KEY4=='PEXT') IPEXT=1
         IF ( KEY4=='OUTER'.OR.KEY5=='OUTER') IOUTER=1
C-------------------------------------------------- 
C SEARCH '=' character
C-------------------------------------------------- 
         DO I=1,ncharkey
           IF ( KEY3(I:I) == '=' ) IS_CHAR_KEY3 = 0
           IF ( KEY3(I:I) /= ' ' ) IS_EMPTY_KEY3 = 0
           IF ( KEY4(I:I) == '=' ) IS_CHAR_KEY4 = 0
           IF ( KEY4(I:I) /= ' ' ) IS_EMPTY_KEY4 = 0
           IF ( KEY5(I:I) == '=' ) IS_CHAR_KEY5 = 0
           IF ( KEY5(I:I) /= ' ' ) IS_EMPTY_KEY5 = 0
           IF ( KEY6(I:I) == '=' ) IS_CHAR_KEY6 = 0
           IF ( KEY6(I:I) /= ' ' ) IS_EMPTY_KEY6 = 0
           IF ( KEY7(I:I) == '=' ) IS_CHAR_KEY7 = 0
           IF ( KEY7(I:I) /= ' ' ) IS_EMPTY_KEY7 = 0
           IF ( KEY8(I:I) == '=' ) IS_CHAR_KEY8 = 0
           IF ( KEY8(I:I) /= ' ' ) IS_EMPTY_KEY8 = 0
         ENDDO
C-------------------------------------------------- 
C Read PLY= I/ALL
C-------------------------------------------------- 
         IS_ALL = 1
         IS_LOWER = 0
         IS_UPPER = 0
         IS_DEF = 0
         CALL CREATE_H3D_ARG_KEYWORD(KEY2       ,KEY3        ,KEY4      ,KEY5     ,KEY6      ,
     .                               KEY7       ,KEY8        ,IS_ALL    ,IS_LOWER ,IS_UPPER  , 
     .                               'PLY'	,3	     ,IS_PLY	,PLY	  ,
     .                               IS_PLY_ALL ,IBID        ,IBID      ,IS_DEF   ,
     .                               IBID       )
C-------------------------------------------------- 
C Read LAYER= I/ALL/LOWER/UPPER
C-------------------------------------------------- 
         IS_ALL = 1
         IS_LOWER = 1
         IS_UPPER = 1
         IS_DEF = 0
         CALL CREATE_H3D_ARG_KEYWORD(KEY2       ,KEY3        ,KEY4      ,KEY5     ,KEY6      ,
     .                               KEY7       ,KEY8        ,IS_ALL     ,IS_LOWER    ,IS_UPPER  , 
     .                               'LAYER'    ,5           ,IS_LAYER    ,LAYER	  ,
     .                               IS_LAYER_ALL,IS_LAYER_LOWER,IS_LAYER_UPPER   ,IS_DEF ,
     .                               IBID       )
C-------------------------------------------------- 
C Read NPT= I/ALL/LOWER/UPPER
C-------------------------------------------------- 


         IS_ALL = 1
         IS_LOWER = 1
         IS_UPPER = 1
         IS_DEF = 0
         CALL CREATE_H3D_ARG_KEYWORD(KEY2       ,KEY3        ,KEY4      ,KEY5     ,KEY6      ,
     .                               KEY7       ,KEY8        ,IS_ALL     ,IS_LOWER    ,IS_UPPER  , 
     .                               'NPT'      ,3           ,IS_IPT    ,IPT	  ,
     .                               IS_IPT_ALL,IS_IPT_LOWER,IS_IPT_UPPER,IS_DEF  ,
     .                               IBID       )
C-------------------------------------------------- 
C Read UVAR= I/ALL/DEF
C-------------------------------------------------- 
         IS_ALL = 1
         IS_LOWER = 0
         IS_UPPER = 0
         IS_DEF = 0
         CALL CREATE_H3D_ARG_KEYWORD(KEY2       ,KEY3        ,KEY4      ,KEY5     ,KEY6      ,
     .                               KEY7       ,KEY8        ,IS_ALL     ,IS_LOWER    ,IS_UPPER  , 
     .                               'UVAR'     ,4           ,IS_UVAR    ,IUVAR	  ,
     .                               IS_UVAR_ALL,IBID        ,IBID       ,IS_DEF  ,
     .                               IBID       ) 
C-------------------------------------------------- 
C Read MDS_VAR= I/ALL/DEF
C-------------------------------------------------- 
         IS_ALL = 1
         IS_LOWER = 0
         IS_UPPER = 0
         IS_DEF = 1
         CALL CREATE_H3D_ARG_KEYWORD(KEY2       ,KEY3        ,KEY4      ,KEY5     ,KEY6      ,
     .                               KEY7       ,KEY8        ,IS_ALL     ,IS_LOWER    ,IS_UPPER  , 
     .                               'MDS_VAR'  ,7           ,IS_MDSVAR  ,IMDSVAR	  ,
     .                               IS_MDSVAR_ALL,IBID      ,IBID       ,IS_DEF  ,
     .                               IS_MDSVAR_DEF)    
C-------------------------------------------------- 
C Read IR= I/ALL/LOWER/UPPER
C--------------------------------------------------
         IS_ALL = 1
         IS_LOWER = 1
         IS_UPPER = 1
         IS_DEF = 0
         CALL CREATE_H3D_ARG_KEYWORD(KEY2       ,KEY3        ,KEY4      ,KEY5     ,KEY6      ,
     .                               KEY7       ,KEY8        ,IS_ALL     ,IS_LOWER    ,IS_UPPER  , 
     .                               'IR'	,2	     ,IS_IR	,IR	  ,
     .                               IS_IR_ALL  ,IS_IR_LOWER ,IS_IR_UPPER ,IS_DEF ,
     .                               IBID       )
C-------------------------------------------------- 
C Read IS= I/ALL/LOWER/UPPER
C--------------------------------------------------
         IS_ALL = 1
         IS_LOWER = 1
         IS_UPPER = 1
         IS_DEF = 0
         CALL CREATE_H3D_ARG_KEYWORD(KEY2       ,KEY3        ,KEY4      ,KEY5     ,KEY6      ,
     .                               KEY7       ,KEY8        ,IS_ALL     ,IS_LOWER    ,IS_UPPER  , 
     .                               'IS'	,2	     ,IS_IS	,IS	  ,
     .                               IS_IS_ALL  ,IS_IS_LOWER ,IS_IS_UPPER ,IS_DEF ,
     .                               IBID       )
C-------------------------------------------------- 
C Read IT= I/ALL/LOWER/UPPER
C--------------------------------------------------
         IS_ALL = 1
         IS_LOWER = 1
         IS_UPPER = 1
         IS_DEF = 0
         CALL CREATE_H3D_ARG_KEYWORD(KEY2       ,KEY3        ,KEY4      ,KEY5     ,KEY6      ,
     .                               KEY7       ,KEY8        ,IS_ALL     ,IS_LOWER    ,IS_UPPER  , 
     .                               'IT'	,2	     ,IS_IT	,IT	  ,
     .                               IS_IT_ALL  ,IS_IT_LOWER ,IS_IT_UPPER ,IS_DEF ,
     .                               IBID       )
C-------------------------------------------------- 
C Read INTER= I/ALL
C-------------------------------------------------- 
         IS_ALL = 1
         IS_LOWER = 0
         IS_UPPER = 0
         IS_DEF = 0
         CALL CREATE_H3D_ARG_KEYWORD(KEY2       ,KEY3        ,KEY4      ,KEY5     ,KEY6      ,
     .                               KEY7       ,KEY8        ,IS_ALL     ,IS_LOWER    ,IS_UPPER  , 
     .                               'INTER'    ,5           ,IS_INTER    ,INTER	  ,
     .                               IS_INTER_ALL,IBID       ,IBID       ,IS_DEF    ,
     .                               IBID       )
C-------------------------------------------------- 
C Read ID= I/ALL
C-------------------------------------------------- 
         IS_ALL = 1
         IS_LOWER = 0
         IS_UPPER = 0
         IS_DEF = 0
         CALL CREATE_H3D_ARG_KEYWORD(KEY2       ,KEY3        ,KEY4      ,KEY5     ,KEY6      ,
     .                               KEY7       ,KEY8        ,IS_ALL     ,IS_LOWER    ,IS_UPPER  , 
     .                               'ID'       ,2           ,IS_ID     ,ID         ,
     .                               IS_ID_ALL  ,IBID       ,IBID       ,IS_DEF    ,
     .                               IBID       )
C-------------------------------------------------- 
         CPT = 0
         IF ( IS_CHAR_KEY3 == 1 .AND. IS_EMPTY_KEY3 == 0) THEN
           DO I=1,ncharkey
             IF ( KEY3(I:I) /= ' ' ) THEN 
               CPT = CPT + 1
               KEY3_GLOB(CPT:CPT) = KEY3(I:I)
             ENDIF
           ENDDO
         ENDIF
         IF ( IS_CHAR_KEY4 == 1 .AND. IS_EMPTY_KEY4 == 0 ) THEN
           CPT = CPT + 1
           KEY3_GLOB(CPT:CPT) = '/'
           DO I=1,ncharkey
             IF ( KEY4(I:I) /= ' ' ) THEN 
               CPT = CPT + 1
               KEY3_GLOB(CPT:CPT) = KEY4(I:I)
             ENDIF
           ENDDO
         ENDIF
         IF ( IS_CHAR_KEY5 == 1 .AND. IS_EMPTY_KEY5 == 0 ) THEN
           CPT = CPT + 1
           KEY3_GLOB(CPT:CPT) = '/'
           DO I=1,ncharkey
             IF ( KEY5(I:I) /= ' ' ) THEN 
               CPT = CPT + 1
               KEY3_GLOB(CPT:CPT) = KEY5(I:I)
             ENDIF
           ENDDO
         ENDIF
         IF ( IS_CHAR_KEY6 == 1 .AND. IS_EMPTY_KEY6 == 0 ) THEN
           CPT = CPT + 1
           KEY3_GLOB(CPT:CPT) = '/'
           DO I=1,ncharkey
             IF ( KEY6(I:I) /= ' ' ) THEN 
               CPT = CPT + 1
               KEY3_GLOB(CPT:CPT) = KEY6(I:I)
             ENDIF
           ENDDO
         ENDIF
         IF ( IS_CHAR_KEY7 == 1 .AND. IS_EMPTY_KEY7 == 0 ) THEN
           CPT = CPT + 1
           KEY3_GLOB(CPT:CPT) = '/'
           DO I=1,ncharkey
             IF ( KEY7(I:I) /= ' ' ) THEN 
               CPT = CPT + 1
               KEY3_GLOB(CPT:CPT) = KEY7(I:I)
             ENDIF
           ENDDO
         ENDIF
         IF ( IS_CHAR_KEY8 == 1 .AND. IS_EMPTY_KEY8 == 0 ) THEN
           CPT = CPT + 1
           KEY3_GLOB(CPT:CPT) = '/'
           DO I=1,ncharkey
             IF ( KEY8(I:I) /= ' ' ) THEN 
               CPT = CPT + 1
               KEY3_GLOB(CPT:CPT) = KEY8(I:I)
             ENDIF
           ENDDO
         ENDIF
c
         IS_MODEL_NPT = 0
         IS_MODEL_LAYER = 0
         IS_MODEL_PLY = 0
	 DO K=1,NUMGEO
           IF(IGEO(11,K) == 9 .OR. IGEO(11,K) == 11 .OR. IGEO(11,K) == 16)
     .       IS_MODEL_NPT = 1
           IF(IGEO(11,K) == 10 .OR. IGEO(11,K) == 11 .OR. IGEO(11,K) == 16)
     .       IS_MODEL_LAYER = 1
            IF(IGEO(11,K) == 17 .OR. IGEO(11,K) == 51 .OR. IGEO(11,K) == 52)
     .       IS_MODEL_PLY = 1
	 ENDDO
C
         CPT = 0
	 DO K=1,NUMMAT
           IF (IPM(2,K) == 200) THEN
             CPT = CPT + 1
             ID_MAT_MDS(CPT) = IPM(1,K)
             INDEX_MAT_MDS(CPT) = K
           ENDIF
         ENDDO
C-------------------------------------------------- 
c  KEYWORD MANAGEMENT
C--------------------------------------------------
        NB_KEY = 1
        CPT_KEY = 1
        KEY3_READ = KEY3_GLOB
        KEY2_READ = KEY2
        ISHELL_NPT_CHECK = 0
        DO WHILE (CPT_KEY <= NB_KEY)
          CALL H3D_GENE_KEYWORD(KEY2_READ,KEY2,KEY3_READ,KEY3_GLOB,NB_KEY,CPT_KEY,MULTI_FVM,
     .                          IS_MODEL_NPT,IS_MODEL_LAYER,IS_MODEL_PLY,IS_MDSVAR,IS_MDSVAR_DEF,
     .                          IS_PLY_ALL,IS_LAYER_ALL,IS_IPT,IS_LAYER,IS_PLY,IS_ID) 
          ISHELL_NPT_CHECK = 0
C-------------------------------------------------- 
          IF(KEY2 == 'PART') THEN
C-------------------------------------------------- 
            CALL CREATE_H3D_PARTS(H3D_DATA,ID_INPUT,IPART)
            H3D_DATA%IPART_SELECT = 1
            IOK_H3DKEY = 1
          ENDIF
C-------------------------------------------------- 
          IF(KEY2 == 'NODA') THEN
c Input for each interface
            NINTER_MAX = 0
            IF (IS_INTER_ALL == 1) NINTER_MAX = NINTER
  	    ALLOCATE (INTER_INPUT(MAX(1,NINTER_MAX)))
  	    ALLOCATE (IS_AVAILABLE_KEY(MAX(1,NINTER_MAX)))
C-------------------------------------------------- 
            JMAX=MAX(H3D_NUM_KEY%NODAL_SCALAR, H3D_NUM_KEY%NODAL_VECTOR, H3D_NUM_KEY%NODAL_TENSOR)
	    DO J=1,JMAX
c Input for each interface
              CPT_H3D = 0
              IS_SCALAR = 0
              IS_VECTOR = 0
              IS_TENSOR = 0
	      IF ( KEY3_GLOB == H3D_KEYWORD_NODAL_SCALAR(J)%KEY3) IS_SCALAR = 1
              IF ( KEY3_GLOB == H3D_KEYWORD_NODAL_VECTOR(J)%KEY3) IS_VECTOR = 1
              IF ( KEY3_GLOB == H3D_KEYWORD_NODAL_TENSOR(J)%KEY3) IS_TENSOR = 1
C--------------------------
	      IF ( IS_SCALAR == 1 ) THEN
           	IF(H3D_KEYWORD_NODAL_SCALAR(J)%IS_INTER /= 0 ) THEN
           	  IF(IS_INTER_ALL == 0 )THEN
           	    CPT_H3D = CPT_H3D + 1
           	    INTER_INPUT(CPT_H3D) = 0
           	    IS_AVAILABLE_KEY(CPT_H3D) = 1 
           	    IF(H3D_KEYWORD_NODAL_SCALAR(J)%IS_INTER == 1) THEN
           	      IF(INTER /= 0) THEN 
                         DO K=1,NINTER
                           IF(IPARI(15,K) == INTER) THEN
c                             H3D_DATA%OUTPUT_LIST(I)%INTER = K 
                             INTER_INPUT(CPT_H3D) = K
                             IF(KEY3_GLOB(1:4) == 'SKID' ) THEN
                                NINTERSKID = NINTERSKID +1
                                H3D_DATA%N_SCAL_SKID = NINTERSKID
                                H3D_DATA%N_SKID_INTER(K) = NINTERSKID
                             ELSEIF(KEY3_GLOB(1:8) == 'CSE_FRIC' ) THEN
                                IF(NINEFRIC == 0) NINEFRIC = NINEFRIC +1
                                H3D_DATA%N_SCAL_CSE_FRICINT = NINEFRIC
                                H3D_DATA%N_CSE_FRIC_INTER(K) = NINEFRIC
                             ENDIF
                            ENDIF
                          ENDDO
                       ENDIF
           	    ENDIF
           	  ELSE
	 	   IF(NINTER_MAX /= 0)THEN
                     NINEFRIC = 0
	 	     DO K=1,NINTER_MAX
                        IF(KEY3_GLOB(1:4) == 'SKID' ) THEN 
	 	           CPT_H3D = CPT_H3D + 1
c
	 	  	   INTER_INPUT(CPT_H3D) = K
           		   IS_AVAILABLE_KEY(CPT_H3D) = 1
                           H3D_DATA%N_SKID_INTER(K) = K
                        ELSEIF(KEY3_GLOB(1:8) == 'CSE_FRIC' ) THEN
                           N19 = IPARI(71,K) 
                           IF(N19 <= 0) THEN
                              IF(H3D_DATA%N_CSE_FRIC_INTER(K) ==0) THEN
                                 NINEFRIC = NINEFRIC +1
	 	                 CPT_H3D = CPT_H3D + 1
c
	 	  	         INTER_INPUT(CPT_H3D) = K
           		         IS_AVAILABLE_KEY(CPT_H3D) = 1
                                 H3D_DATA%N_CSE_FRIC_INTER(K) = K
                              ENDIF
                           ELSEIF(N19 > 0 ) THEN
                             IF(H3D_DATA%N_CSE_FRIC_INTER(N19) ==0) THEN
                               NINEFRIC = NINEFRIC +1
	 	               CPT_H3D = CPT_H3D + 1
c
	 	  	       INTER_INPUT(CPT_H3D) = N19
           		       IS_AVAILABLE_KEY(CPT_H3D) = 1
                               H3D_DATA%N_CSE_FRIC_INTER(K) = N19
                             ELSE
                               H3D_DATA%N_CSE_FRIC_INTER(K) = N19
                             ENDIF
                           ENDIF
                        ENDIF
	 	     ENDDO
                     IF(KEY3_GLOB(1:4) == 'SKID' ) H3D_DATA%N_SCAL_SKID = NINTER
                     IF(KEY3_GLOB(1:8) == 'CSE_FRIC' ) H3D_DATA%N_SCAL_CSE_FRICINT = NINEFRIC
           	   ENDIF
           	  ENDIF
           	ELSE
           	  CPT_H3D = CPT_H3D + 1
           	  INTER_INPUT(CPT_H3D) = 0

           	  IS_AVAILABLE_KEY(CPT_H3D) = 1 
           	ENDIF
	      ENDIF
	      IF ( IS_SCALAR == 1 ) THEN
                DO K=1,CPT_H3D
           	   IF (IS_AVAILABLE_KEY(K) == 1 .AND. IS_SCALAR == 1) THEN
                       CALL CREATE_H3D_NODAL_SCALAR(H3D_DATA,H3D_KEYWORD_NODAL_SCALAR(J)%ID,ID_INPUT,
     .          			           TRIM(H3D_KEYWORD_NODAL_SCALAR(J)%TEXT1),LEN_TRIM(H3D_KEYWORD_NODAL_SCALAR(J)%TEXT1),
     .                                             H3D_KEYWORD_NODAL_SCALAR(J)%COMMENT,80,IPART,KEY3_GLOB,
     .                                             INTER_INPUT(K))
           	       IOK_H3DKEY = 1 
                   ENDIF
                 ENDDO 
              ELSE
	         IF ( KEY3_GLOB == H3D_KEYWORD_NODAL_VECTOR(J)%KEY3 ) THEN
                   CALL CREATE_H3D_NODAL_VECTOR(H3D_DATA,H3D_KEYWORD_NODAL_VECTOR(J)%ID,ID_INPUT,
     .                                     TRIM(H3D_KEYWORD_NODAL_VECTOR(J)%TEXT1),LEN_TRIM(H3D_KEYWORD_NODAL_VECTOR(J)%TEXT1),
     .                                     H3D_KEYWORD_NODAL_VECTOR(J)%COMMENT,80,IPART,KEY3_GLOB)
                   IOK_H3DKEY = 1
	         ENDIF
                 IF ( KEY3_GLOB == H3D_KEYWORD_NODAL_TENSOR(J)%KEY3 ) THEN
                   CALL CREATE_H3D_NODAL_TENSOR(H3D_DATA,H3D_KEYWORD_NODAL_TENSOR(J)%ID,ID_INPUT,
     .                                     TRIM(H3D_KEYWORD_NODAL_TENSOR(J)%TEXT1),LEN_TRIM(H3D_KEYWORD_NODAL_TENSOR(J)%TEXT1),
     .                                     H3D_KEYWORD_NODAL_TENSOR(J)%COMMENT,80,IPART,KEY3_GLOB)
                   IOK_H3DKEY = 1
	         ENDIF
              ENDIF
C-----------------------------
	    ENDDO

            DEALLOCATE (INTER_INPUT)
            DEALLOCATE (IS_AVAILABLE_KEY)
          ENDIF
C-------------------------------------------------- 
c          IF(KEY2 == 'VECT') THEN
C-------------------------------------------------- 
c	    DO J=1,100
c	      IF ( KEY3_GLOB == H3D_KEYWORD_NODAL_VECTOR(J)%KEY3 ) THEN
c                CALL CREATE_H3D_NODAL_VECTOR(H3D_DATA,H3D_KEYWORD_NODAL_VECTOR(J)%ID,ID_INPUT,
c     .                                     H3D_KEYWORD_NODAL_VECTOR(J)%TEXT1,50,IPART,KEY3_GLOB)
c                IOK_H3DKEY = 1
c	      ENDIF
c	    ENDDO
c          ENDIF
C-------------------------------------------------- 
          IF(KEY2 == 'SHELL' .OR. KEY2 =='ELEM'.OR. KEY2 =='SOLID' .OR.
     .       KEY2 == 'BRICK' .OR. KEY2 =='QUAD'.OR. KEY2 =='BEAM ' .OR.
     .       KEY2 == 'SPRING'.OR. KEY2 =='TRUSS'.OR. KEY2 =='SPH') THEN
C-------------------------------------------------- 
            NLAY_MAX = 0
            NIP_MAX = 0
            NPLY_MAX = 0
            NUVAR_MAX = 0
            NMDSVAR_MAX = 0
C
            ! LAYER=ALL 
            IF (IS_LAYER_ALL == 1) THEN
	            DO K=1,NUMGEO
           	    IF(IGEO(11,K) == 10 .OR. IGEO(11,K) == 11 .OR. IGEO(11,K) == 16 ) THEN
                  NLAY_MAX = MAX(NLAY_MAX,IGEO(4,K))
           	    ELSEIF(IGEO(11,K) == 20 .OR. IGEO(11,K) == 21 .OR. IGEO(11,K) == 22) THEN
                  NLAY_MAX = MAX(NLAY_MAX,MIN(1,IGEO(30,K)))
                ENDIF
	            ENDDO
              DO K=1,NS_STACK ! type17,51 & 52
                NLAY_MAX = MAX(NLAY_MAX,STACK%IGEO(1,K))
              ENDDO
            ENDIF
C
            ! NPT=ALL
            ISH_NPT0 = 0    
            IF (IS_IPT_ALL == 1) THEN
               DO K=1,NUMGEO
                 IF(IGEO(11,K) == 1 .OR. IGEO(11,K) == 9 ) THEN
                   NIP_MAX = MAX(NIP_MAX,IGEO(4,K))
                   IF (IGEO(4,K)==0) ISH_NPT0=1
                 ELSEIF(IGEO(11,K) == 19 )THEN  ! 
                   NIP_MAX = MAX(NIP_MAX,IGEO(44,K))
                 ELSEIF(IGEO(11,K) == 18)THEN
                   NIP_MAX = MAX(NIP_MAX,IGEO(3,K))
                 ENDIF
               ENDDO
               DO K=1,NUMPLY
                  NIP_MAX = MAX(NIP_MAX, PLY_INFO(2,K)) 
               ENDDO
            ENDIF 
            IF(IS_IPT_ALL == 1 .AND. NIP_MAX == 0) ISHELL_NPT_CHECK = 1
C
            ! PLY=ALL
            ID_PLY(1:NUMGEO) = 0
            IPT_PLY(1:NUMGEO) = 0    
            IF (IS_PLY_ALL == 1) THEN
	      DO K=1,NUMGEO
           	IF(IGEO(11,K) == 19) THEN
           	  NPLY_MAX = NPLY_MAX + 1
           	  ID_PLY(NPLY_MAX) = IGEO(1,K)
           	  IPT_PLY(NPLY_MAX) = IGEO(4,K)
                ENDIF
	      ENDDO
	      DO K=1,NUMPLY
           	  NPLY_MAX = NPLY_MAX + 1
           	  ID_PLY(NPLY_MAX) = PLY_INFO(1,K)
           	  IPT_PLY(NPLY_MAX) = PLY_INFO(2,K)
	      ENDDO              
            ENDIF
            IF (IS_UVAR_ALL == 1) THEN
	      DO K=1,NUMMAT
           	 NUVAR_MAX = MAX(NUVAR_MAX,IPM(8,K))
	      ENDDO
            ENDIF
            IF (IS_MDSVAR_ALL == 1) THEN
              IF(KEY3_GLOB(1:3) == 'MDS' ) THEN
	        DO K=1,MDS_NMAT
           	   NMDSVAR_MAX = NMDSVAR_MAX + MDS_NDEPSVAR(K)
	        ENDDO
              ENDIF
            ENDIF

            IF (IS_MDSVAR_DEF == 1) THEN
              IF(KEY3_GLOB(1:3) == 'MDS' ) THEN
	        DO K=1,MDS_NMAT
	          DO L=1,MDS_NDEPSVAR(K)
           	     NMDSVAR_MAX = NMDSVAR_MAX+MDS_OUTPUT_TABLE(L,K)
	          ENDDO
	        ENDDO
              ENDIF
            ENDIF

            IR_MAX = 0
            IF (IS_IR_ALL == 1) THEN
              DO K=1,NUMGEO
                IF(IGEO(11,K) == 6 .OR. IGEO(11,K) == 14 .OR. IGEO(11,K) == 20 .OR. 
     .            IGEO(11,K) == 21 .OR. IGEO(11,K) == 22) THEN
                  IF(IGEO(4,K) == 1 .OR. IGEO(10,K)==15) THEN
                    IR_MAX = 1
                  ELSEIF(IGEO(4,K) == 8) THEN
                    IR_MAX = 2
                  ELSE
                    NPTR=MAX(2,IGEO(4,K)/100)
                    IR_MAX = MAX(IR_MAX,NPTR)
                  ENDIF
                  IF(N2D/=0) THEN
                    IF(IGEO(4,K) == 4) THEN
                      IR_MAX = 2
                    ELSE
                      IR_MAX = 1
                    ENDIF
                  ENDIF
                ELSEIF(IGEO(11,K) == 43)THEN
                  IR_MAX = MAX(IR_MAX,4)
                ENDIF
              ENDDO
            ENDIF

            IS_MAX = 0
            IF (IS_IS_ALL == 1) THEN
              DO K=1,NUMGEO
               IF(IGEO(11,K) == 6 .OR. IGEO(11,K) == 14 .OR. IGEO(11,K) == 20 .OR. 
     .            IGEO(11,K) == 21 .OR. IGEO(11,K) == 22) THEN
C-------------    thick-shell Isolid=15     
                  IF(IGEO(10,K)==15) THEN
                    IS_MAX = MAX(IS_MAX,IGEO(4,K))
                  ELSEIF(IGEO(4,K) == 1) THEN
                    IS_MAX = 1
                  ELSEIF(IGEO(4,K) == 8) THEN
                    IS_MAX = 2
                  ELSE
                    NPTS=MAX(2,MOD(IGEO(4,K)/10,10))
                    IS_MAX = MAX(IS_MAX,NPTS)
                  ENDIF
                  IF(N2D/=0) THEN
                    IF(IGEO(4,K) == 4) THEN
                      IS_MAX = 2
                    ELSE
                      IS_MAX = 1
                    ENDIF
                  ENDIF
               ELSEIF(IGEO(11,K) == 43)THEN
                  IS_MAX = MAX(IS_MAX,1)
               ENDIF
              ENDDO
            ENDIF

            IT_MAX = 0
            IF (IS_IT_ALL == 1) THEN
              DO K=1,NUMGEO
               IF(IGEO(11,K) == 6 .OR. IGEO(11,K) == 14 .OR. IGEO(11,K) == 20 .OR. 
     .           IGEO(11,K) == 21 .OR. IGEO(11,K) == 22) THEN
                  IF(IGEO(4,K) == 1 .OR. IGEO(10,K)==15) THEN
                    IT_MAX = 1
                  ELSEIF(IGEO(4,K) == 8) THEN
                    IT_MAX = 2
                  ELSE
                    NPTT=MAX(2,MOD(IGEO(4,K),10))
                    IT_MAX = MAX(IT_MAX,NPTT)
                  ENDIF
               ELSEIF(IGEO(11,K) == 43)THEN
                  IT_MAX = MAX(IT_MAX,1)
               ENDIF
               IF(N2D/=0) THEN
                  IT_MAX = 1
               ENDIF
              ENDDO
            ENDIF
C----special case for thick-shell Isolid=14  :          
        IF ((IS_IR_ALL + IS_IS_ALL+IS_IT_ALL)>0) THEN          
          DO K=1,NUMGEO                                        
            IF(IGEO(10,K)==14.AND. (IGEO(11,K) == 20 .OR.      
     .         IGEO(11,K) == 21 .OR. IGEO(11,K) == 22)) THEN      	
              ICSTR= IGEO(14,K)                                
              NPT = IGEO(4,K)                                  
              NPTR=NPT/100                                     
              NPTS=MOD(NPT/10,10)                              
              NPTT=MOD(NPT,10)                                 
              SELECT CASE (ICSTR)                            
                  CASE(100)                                    
                    IF (NPTT==0) NPTT= IGEO(15,K)              
                  CASE(10)                                     
                    IF (NPTS==0) NPTS= IGEO(15,K)              
                  CASE(1)                                      
                    IF (NPTR==0) NPTR= IGEO(15,K)              
              END SELECT                                     
              IF (IS_IR_ALL == 1) IR_MAX = MAX(IR_MAX,NPTR)    
              IF (IS_IS_ALL == 1) IS_MAX = MAX(IS_MAX,NPTS)    
              IF (IS_IT_ALL == 1) IT_MAX = MAX(IT_MAX,NPTT)    
            ENDIF                                              
          ENDDO                                                
        END IF ! IF ((IS_IR_ALL + IS_IS_ALL+IS_IT_ALL)>0)      

c 
        ID_MAX = 0                          
        IF (IS_ID_ALL == 1) THEN            
          DO K=1,NUMMAT                     
           IF (IPM(2,K) == 36) THEN         
             ID_MAX = 1                     
           ELSEIF (IPM(2,K) == 78) THEN     
             ID_MAX = 3                     
           ELSEIF (IPM(2,K) == 87) THEN     
             ID_MAX = 4                     
           ENDIF                            
          ENDDO                             
        ENDIF 
c
        ! /H3D/ELEM/FAILURE definition
        IF (KEY3_GLOB == 'FAILURE') THEN
c
          ! All failure IDs are asked by user
          IF (IS_ID_ALL > 0) THEN 
c 
            ! First count of the failure model            
            CPT_IRUP = 0
            DO NG=1,NGROUP
              NLAY = ELBUF_STR(NG)%NLAY
              DO J = 1,NLAY
                DO K = 1, ELBUF_STR(NG)%BUFLY(J)%NPTT
                  DO L = 1,ELBUF_STR(NG)%NPTR
                    DO M = 1,ELBUF_STR(NG)%NPTS
                      FBUF => ELBUF_STR(NG)%BUFLY(J)%FAIL(L,M,K)
                      CPT_IRUP = CPT_IRUP + ELBUF_STR(NG)%BUFLY(J)%NFAIL
                    ENDDO
                  ENDDO
                ENDDO 
              ENDDO
            ENDDO
c
            ! Save all IDs (might be doubled)
            IF (ALLOCATED(IRUP_ALL)) DEALLOCATE(IRUP_ALL)
            ALLOCATE(IRUP_ALL(CPT_IRUP))
            IRUP_ALL(1:CPT_IRUP) = 0
            CPT_IRUP = 0
            DO NG=1,NGROUP
              NLAY = ELBUF_STR(NG)%NLAY
              DO J = 1,NLAY
                DO K = 1, ELBUF_STR(NG)%BUFLY(J)%NPTT
                  DO L = 1,ELBUF_STR(NG)%NPTR
                    DO M = 1,ELBUF_STR(NG)%NPTS
                      FBUF => ELBUF_STR(NG)%BUFLY(J)%FAIL(L,M,K)
                      DO IFAIL = 1,ELBUF_STR(NG)%BUFLY(J)%NFAIL
                        CPT_IRUP = CPT_IRUP + 1
                        IRUP_ALL(CPT_IRUP) = FBUF%FLOC(IFAIL)%IDFAIL
                      ENDDO
                    ENDDO
                  ENDDO
                ENDDO 
              ENDDO
            ENDDO
c
            ! Remove doubled failure IDs
            ALLOCATE(IRUP_ID_ALL(CPT_IRUP))
            IRUP_ID_ALL(1:CPT_IRUP) = 0            
            CPT_IRUP2 = 0
            DO J = 1,CPT_IRUP 
              FOUND = .FALSE.
              DO K = 1,CPT_IRUP2
                IF (IRUP_ID_ALL(K) == IRUP_ALL(J) .AND. IRUP_ALL(J) /= 0) FOUND = .TRUE.
              ENDDO
              IF (.NOT.FOUND) THEN 
                CPT_IRUP2 = CPT_IRUP2 + 1
                IRUP_ID_ALL(CPT_IRUP2) = IRUP_ALL(J)
              ENDIF
            ENDDO
c
            ! Save corresponding failure models
            IF (ALLOCATED(IRUP_ALL)) DEALLOCATE(IRUP_ALL)
            ALLOCATE(IRUP_ALL(CPT_IRUP2))
            IRUP_ALL(1:CPT_IRUP2) = 0
            DO O = 1,CPT_IRUP2
              DO NG=1,NGROUP
                NLAY = ELBUF_STR(NG)%NLAY
                DO J = 1,NLAY
                  DO K = 1, ELBUF_STR(NG)%BUFLY(J)%NPTT
                    DO L = 1,ELBUF_STR(NG)%NPTR
                      DO M = 1,ELBUF_STR(NG)%NPTS
                        FBUF => ELBUF_STR(NG)%BUFLY(J)%FAIL(L,M,K)
                        DO IFAIL = 1,ELBUF_STR(NG)%BUFLY(J)%NFAIL
                          IF (FBUF%FLOC(IFAIL)%IDFAIL == IRUP_ID_ALL(O)) THEN 
                            IRUP_ALL(O) = FBUF%FLOC(IFAIL)%ILAWF
                          ENDIF
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO 
                ENDDO
              ENDDO
            ENDDO
c 
            ! Maximal number of IDs
            ID_MAX = CPT_IRUP2
c            
          ! Only a specific failure ID is asked by the user
          ELSEIF (IS_ID > 0) THEN 
c
            ! Look if ID is found for the element group
            CPT_IRUP = 0
            DO NG=1,NGROUP
              NLAY = ELBUF_STR(NG)%NLAY
              DO J = 1,NLAY
                DO K = 1, ELBUF_STR(NG)%BUFLY(J)%NPTT
                  DO L = 1,ELBUF_STR(NG)%NPTR
                    DO M = 1,ELBUF_STR(NG)%NPTS
                      FBUF => ELBUF_STR(NG)%BUFLY(J)%FAIL(L,M,K)
                      DO IFAIL = 1,ELBUF_STR(NG)%BUFLY(J)%NFAIL
                        IF (FBUF%FLOC(IFAIL)%IDFAIL == ID) THEN 
                          IRUP_ALL(1) = FBUF%FLOC(IFAIL)%ILAWF
                          CPT_IRUP = 1
                        ENDIF
                      ENDDO
                    ENDDO
                  ENDDO
                ENDDO 
              ENDDO
            ENDDO
c
          ENDIF
        ENDIF
c
  	    ALLOCATE (LAYER_INPUT(MAX(1,NLAY_MAX)*MAX(1,NIP_MAX)*MAX(1,NPLY_MAX)*MAX(1,NUVAR_MAX)*
     .     	      MAX(1,IR_MAX)*MAX(1,IS_MAX)*MAX(1,IT_MAX)*MAX(1,NMDSVAR_MAX)*MAX(1,ID_MAX)))
  	    ALLOCATE (IPT_INPUT(MAX(1,NLAY_MAX)*MAX(1,NIP_MAX)*MAX(1,NPLY_MAX)*MAX(1,NUVAR_MAX)*
     .     	      MAX(1,IR_MAX)*MAX(1,IS_MAX)*MAX(1,IT_MAX)*MAX(1,NMDSVAR_MAX)*MAX(1,ID_MAX)))
  	    ALLOCATE (PLY_INPUT(MAX(1,NLAY_MAX)*MAX(1,NIP_MAX)*MAX(1,NPLY_MAX)*MAX(1,NUVAR_MAX)*
     .     	      MAX(1,IR_MAX)*MAX(1,IS_MAX)*MAX(1,IT_MAX)*MAX(1,NMDSVAR_MAX)*MAX(1,ID_MAX)))
  	    ALLOCATE (UVAR_INPUT(MAX(1,NLAY_MAX)*MAX(1,NIP_MAX)*MAX(1,NPLY_MAX)*MAX(1,NUVAR_MAX)*
     .     	      MAX(1,IR_MAX)*MAX(1,IS_MAX)*MAX(1,IT_MAX)*MAX(1,NMDSVAR_MAX)*MAX(1,ID_MAX)))
  	    ALLOCATE (GAUSS_INPUT(MAX(1,NLAY_MAX)*MAX(1,NIP_MAX)*MAX(1,NPLY_MAX)*MAX(1,NUVAR_MAX)*
     .     	      MAX(1,IR_MAX)*MAX(1,IS_MAX)*MAX(1,IT_MAX)*MAX(1,NMDSVAR_MAX)*MAX(1,ID_MAX)))
  	    ALLOCATE (IR_INPUT(MAX(1,NLAY_MAX)*MAX(1,NIP_MAX)*MAX(1,NPLY_MAX)*MAX(1,NUVAR_MAX)*
     .     	      MAX(1,IR_MAX)*MAX(1,IS_MAX)*MAX(1,IT_MAX)*MAX(1,NMDSVAR_MAX)*MAX(1,ID_MAX)))
  	    ALLOCATE (IS_INPUT(MAX(1,NLAY_MAX)*MAX(1,NIP_MAX)*MAX(1,NPLY_MAX)*MAX(1,NUVAR_MAX)*
     .     	      MAX(1,IR_MAX)*MAX(1,IS_MAX)*MAX(1,IT_MAX)*MAX(1,NMDSVAR_MAX)*MAX(1,ID_MAX)))
  	    ALLOCATE (IT_INPUT(MAX(1,NLAY_MAX)*MAX(1,NIP_MAX)*MAX(1,NPLY_MAX)*MAX(1,NUVAR_MAX)*
     .     	      MAX(1,IR_MAX)*MAX(1,IS_MAX)*MAX(1,IT_MAX)*MAX(1,NMDSVAR_MAX)*MAX(1,ID_MAX)))
  	    ALLOCATE (IS_AVAILABLE_KEY(MAX(1,NLAY_MAX)*MAX(1,NIP_MAX)*MAX(1,NPLY_MAX)*MAX(1,NUVAR_MAX)*
     .     	      MAX(1,IR_MAX)*MAX(1,IS_MAX)*MAX(1,IT_MAX)*MAX(1,NMDSVAR_MAX)*MAX(1,ID_MAX)))
  	    ALLOCATE (MDSVAR_INPUT(MAX(1,NLAY_MAX)*MAX(1,NIP_MAX)*MAX(1,NPLY_MAX)*MAX(1,NUVAR_MAX)*
     .     	      MAX(1,IR_MAX)*MAX(1,IS_MAX)*MAX(1,IT_MAX)*MAX(1,NMDSVAR_MAX)*MAX(1,ID_MAX)))
  	    ALLOCATE (MDSVAR_INPUT1(MAX(1,NLAY_MAX)*MAX(1,NIP_MAX)*MAX(1,NPLY_MAX)*MAX(1,NUVAR_MAX)*
     .     	      MAX(1,IR_MAX)*MAX(1,IS_MAX)*MAX(1,IT_MAX)*MAX(1,NMDSVAR_MAX)*MAX(1,ID_MAX)))
  	    ALLOCATE (MDSVAR_INPUT2(MAX(1,NLAY_MAX)*MAX(1,NIP_MAX)*MAX(1,NPLY_MAX)*MAX(1,NUVAR_MAX)*
     .     	      MAX(1,IR_MAX)*MAX(1,IS_MAX)*MAX(1,IT_MAX)*MAX(1,NMDSVAR_MAX)*MAX(1,ID_MAX)))
  	    ALLOCATE (IDS_INPUT(MAX(1,NLAY_MAX)*MAX(1,NIP_MAX)*MAX(1,NPLY_MAX)*MAX(1,NUVAR_MAX)*
     .     	      MAX(1,IR_MAX)*MAX(1,IS_MAX)*MAX(1,IT_MAX)*MAX(1,NMDSVAR_MAX)*MAX(1,ID_MAX)))
        ALLOCATE (IRUP_H3D(MAX(1,NLAY_MAX)*MAX(1,NIP_MAX)*MAX(1,NPLY_MAX)*MAX(1,NUVAR_MAX)*
     .     	      MAX(1,IR_MAX)*MAX(1,IS_MAX)*MAX(1,IT_MAX)*MAX(1,NMDSVAR_MAX)*MAX(1,ID_MAX)))
C--------------------------------------------------
            CPT = 0
  	    ALLOCATE (MDSVAR_INPUT_TMP(MAX(1,NMDSVAR_MAX)))
  	    ALLOCATE (MDSVAR_INPUT1_TMP(MAX(1,NMDSVAR_MAX)))
  	    ALLOCATE (MDSVAR_INPUT2_TMP(MAX(1,NMDSVAR_MAX)))
        
            LAYER_INPUT(:) = 0
            IPT_INPUT(:) = 0
            PLY_INPUT(:) = 0
            UVAR_INPUT(:) = 0
            GAUSS_INPUT(:) = 0
            IR_INPUT(:) = 0
            IS_INPUT(:) = 0
            IT_INPUT(:) = 0
            IS_AVAILABLE_KEY(:) = 0
            MDSVAR_INPUT(:) = 0
            MDSVAR_INPUT1(:) = 0
            MDSVAR_INPUT2(:) = 0
            IDS_INPUT(:) = 0
            IRUP_H3D(:) = 0
        
            IF (IS_MDSVAR_ALL == 1) THEN
              IF(KEY3_GLOB(1:3) == 'MDS' ) THEN
	        DO K=1,MDS_NMAT
	          DO L=1,MDS_NDEPSVAR(K)
           	    CPT = CPT + 1
                    MDSVAR_INPUT_TMP(CPT) = K
                    MDSVAR_INPUT1_TMP(CPT) = L
                    MDSVAR_INPUT2_TMP(CPT) = ID_MAT_MDS(K)
	          ENDDO
	        ENDDO
              ENDIF
            ENDIF
            IF (IS_MDSVAR_DEF == 1) THEN
              IF(KEY3_GLOB(1:3) == 'MDS' ) THEN
	        DO K=1,MDS_NMAT
	          DO L=1,MDS_NDEPSVAR(K)
           	    IF(MDS_OUTPUT_TABLE(L,K) /= 0) THEN
           	      CPT = CPT + 1
                      MDSVAR_INPUT_TMP(CPT) = K
                      MDSVAR_INPUT1_TMP(CPT) = L
                      MDSVAR_INPUT2_TMP(CPT) = ID_MAT_MDS(K)
                    ENDIF
	          ENDDO
	        ENDDO
              ENDIF
            ENDIF
            CPT = 0
C
          ENDIF
C
          IF(.NOT.ALLOCATED(IS_LAYER_MAT))ALLOCATE(IS_LAYER_MAT(MAX(1,NUMMAT),MAX(1,NLAY_MAX)))
          IS_LAYER_MAT(1:MAX(1,NUMMAT),MAX(1,NLAY_MAX)) = 0
C
          IF(IS_MDSVAR == 1) THEN

            DO NG=1,NGROUP

              CALL INITBUF(IPARG    ,NG     ,                    
     2             MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3             NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTURB   ,  
     4             JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5             NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6             IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7             ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )

              NLAY  = ELBUF_STR(NG)%NLAY
              ISUBSTACK = IPARG(71,NG)
              DO J=1,NLAY
                NUVAR = ELBUF_STR(NG)%BUFLY(J)%NVAR_MAT
                IF(IUVAR <= NUVAR) THEN
                  ID_PLY_TMP = 0
                  IF (IGTYP == 17 .OR. IGTYP == 51) THEN
                    ID_PLY_TMP = IGEO(1,STACK%IGEO(2+J,ISUBSTACK))
                  ELSEIF (IGTYP == 52) THEN
                    ID_PLY_TMP = PLY_INFO(1,STACK%IGEO(2+J,ISUBSTACK)-NUMSTACK) 
                  ENDIF
c
                  IMAT = ELBUF_STR(NG)%BUFLY(J)%IMAT
c
                  IS_LAYER_MAT(IMAT,J) = 1
c
                ENDIF
              ENDDO
            ENDDO
          ENDIF
C-------------------------------------------------- 
          IF(KEY2 == 'SHELL' .OR. KEY2 =='ELEM') THEN
C-------------------------------------------------- 

            JMAX=MAX(H3D_NUM_KEY%SHELL_SCALAR, H3D_NUM_KEY%SHELL_VECTOR, H3D_NUM_KEY%SHELL_TENSOR)
	    DO J=1,JMAX
              CPT_H3D = 0
              IS_SCALAR = 0
              IS_VECTOR = 0
              IS_TENSOR = 0
	      IF ( KEY3_GLOB == H3D_KEYWORD_SHELL_SCALAR(J)%KEY3) IS_SCALAR = 1
	      IF ( KEY3_GLOB == H3D_KEYWORD_SHELL_VECTOR(J)%KEY3) IS_VECTOR = 1
              IF ( KEY3_GLOB == H3D_KEYWORD_SHELL_TENSOR(J)%KEY3) IS_TENSOR = 1
	      IF ( IS_SCALAR == 1 .OR. IS_VECTOR == 1 .OR. IS_TENSOR == 1 ) THEN
           	IF(IS_SCALAR == 1) H3D_KEYWORD_SHELL = H3D_KEYWORD_SHELL_SCALAR(J)
           	IF(IS_VECTOR == 1) H3D_KEYWORD_SHELL = H3D_KEYWORD_SHELL_VECTOR(J)
           	IF(IS_TENSOR == 1) H3D_KEYWORD_SHELL = H3D_KEYWORD_SHELL_TENSOR(J)
           	IF(H3D_KEYWORD_SHELL%IS_LAYER /= 0  .OR. 
     .     	   H3D_KEYWORD_SHELL%IS_IPT /= 0 .OR.
     .     	   H3D_KEYWORD_SHELL%IS_PLY /= 0 .OR.
     .     	   H3D_KEYWORD_SHELL%IS_UVAR /= 0  .OR.
     .     	   H3D_KEYWORD_SHELL%IS_MDSVAR /= 0  .OR.
     .     	   H3D_KEYWORD_SHELL%IS_ID /= 0  ) THEN
C            
           	  IF(IS_LAYER_ALL == 0 .AND. IS_IPT_ALL == 0 .AND. IS_PLY_ALL == 0 .AND. IS_UVAR_ALL == 0 .AND. 
     .               IS_MDSVAR_ALL == 0 .AND. IS_MDSVAR_DEF ==0 .AND. IS_ID_ALL ==0)THEN

           	    CPT_H3D = CPT_H3D + 1
           	    LAYER_INPUT(CPT_H3D) = -1
           	    IPT_INPUT(CPT_H3D) = -1
                    IDS_INPUT(CPT_H3D) = -1
           	    PLY_INPUT(CPT_H3D) = -1
           	    UVAR_INPUT (CPT_H3D)= -1
           	    GAUSS_INPUT(CPT_H3D) = -1
           	    MDSVAR_INPUT(CPT_H3D)= -1
           	    MDSVAR_INPUT1(CPT_H3D)= -1
           	    MDSVAR_INPUT2(CPT_H3D)= -1
           	    IS_AVAILABLE_KEY(CPT_H3D) = 1 

           	    IF(H3D_KEYWORD_SHELL%IS_LAYER == 1) THEN
           	      IF(LAYER /= 0) LAYER_INPUT(CPT_H3D) = LAYER
           	      IF(IS_LAYER_LOWER == 1) LAYER_INPUT(CPT_H3D) = -2
           	      IF(IS_LAYER_UPPER == 1) LAYER_INPUT(CPT_H3D) = -3
           	    ENDIF

           	    IF(H3D_KEYWORD_SHELL%IS_IPT == 1) THEN
           	      IF(IPT /= 0) IPT_INPUT(CPT_H3D) = IPT
           	      IF(IS_IPT_LOWER == 1 )IPT_INPUT(CPT_H3D) = -2
           	      IF(IS_IPT_UPPER == 1 )IPT_INPUT(CPT_H3D) = -3
           	    ENDIF

           	    IF(H3D_KEYWORD_SHELL%IS_PLY == 1) THEN
           	      IF(PLY /= 0) PLY_INPUT(CPT_H3D) = PLY
           	    ENDIF

           	    IF(H3D_KEYWORD_SHELL%IS_UVAR == 1) THEN
           	      IF(IUVAR /= 0) UVAR_INPUT(CPT_H3D) = IUVAR
           	    ENDIF

           	    IF(H3D_KEYWORD_SHELL%IS_ID == 1) THEN
           	      IF(ID /= 0) IDS_INPUT(CPT_H3D) = ID
                  IF ((KEY3_GLOB(1:7) == 'FAILURE').AND.(CPT_IRUP > 0)) THEN 
                    IRUP_H3D(CPT_H3D) = IRUP_ALL(1)
                  ENDIF
           	    ENDIF

           	    IF(H3D_KEYWORD_SHELL%IS_MDSVAR == 1) THEN
           	      IF(IMDSVAR /= 0) THEN
                        MDSVAR_INPUT(CPT_H3D) = ((IMDSVAR-1) * MDS_NMAT /NMDSVAR_MAX)+1
           	        MDSVAR_INPUT1(CPT_H3D) = IMDSVAR-(MDSVAR_INPUT(CPT_H3D)-1)*NMDSVAR_MAX/MDS_NMAT
           	        MDSVAR_INPUT2(CPT_H3D) = ID_MAT_MDS(MDSVAR_INPUT(CPT_H3D))
                      ENDIF
           	    ENDIF
           	  ELSE
	 	   IF(NLAY_MAX + MAX(1,NIP_MAX) + NPLY_MAX + NUVAR_MAX + NMDSVAR_MAX + ID_MAX /= 0)THEN
	 	     DO K=1,MAX(1,IS_LAYER_ALL*MAX(NLAY_MAX,1))
	 	       DO L=1,MAX(1,IS_IPT_ALL*MAX(NIP_MAX,1))
	 	  	 DO M=1,MAX(1,IS_PLY_ALL*MAX(NPLY_MAX,1))
	 	  	   DO N=1,MAX(1,IS_UVAR_ALL*MAX(NUVAR_MAX,1))
	 	  	     DO O=1,MAX(1,MAX(IS_MDSVAR_ALL,IS_MDSVAR_DEF)*MAX(NMDSVAR_MAX,1))
	 	  	       DO P=1,MAX(1,IS_ID_ALL*MAX(ID_MAX,1))
	 	  	         CPT_H3D = CPT_H3D + 1
c                    
	 	  	         LAYER_INPUT(CPT_H3D) = -1
	 	  	         IF (NLAY_MAX /= 0 .AND. IS_LAYER_ALL == 1) LAYER_INPUT(CPT_H3D) = K
	 	  	         IF (IS_LAYER_ALL == 0 .AND. LAYER >= 1) LAYER_INPUT(CPT_H3D) = LAYER
c
	 	  	         IPT_INPUT(CPT_H3D) = -1
	 	  	         IF (IS_IPT_ALL == 1) IPT_INPUT(CPT_H3D) = L
	 	  	         IF (IS_IPT_ALL == 0 .AND. IPT >= 1) IPT_INPUT(CPT_H3D) = IPT
c
	 	  	         PLY_INPUT(CPT_H3D) = -1
	 	  	         IF (NPLY_MAX /= 0  .AND. IS_PLY_ALL == 1) PLY_INPUT(CPT_H3D) = ID_PLY(M)
	 	  	         IF (IS_PLY_ALL == 0  .AND. PLY >= 1) PLY_INPUT(CPT_H3D) = PLY
c
	 	  	         UVAR_INPUT(CPT_H3D) = -1
	 	  	         IF (NUVAR_MAX /= 0  .AND. IS_UVAR_ALL == 1) THEN
                                   UVAR_INPUT(CPT_H3D) = N
                                 ENDIF
	 	  	         IF (IS_UVAR_ALL == 0  .AND. IUVAR >= 1) UVAR_INPUT(CPT_H3D) = IUVAR

	 	  	         MDSVAR_INPUT(CPT_H3D) = -1
	 	  	         MDSVAR_INPUT1(CPT_H3D) = -1
	 	  	         IF (NMDSVAR_MAX /= 0  .AND. IS_MDSVAR_ALL == 1 .AND. 
     .                               KEY3_GLOB(1:3) == 'MDS' .AND. MDS_NMAT /= 0) THEN
	 	  	           MDSVAR_INPUT(CPT_H3D) = ((O-1) * MDS_NMAT /NMDSVAR_MAX)+1
                                   MDSVAR_INPUT1(CPT_H3D) = O-(MDSVAR_INPUT(CPT_H3D)-1)*NMDSVAR_MAX/MDS_NMAT
                                   MDSVAR_INPUT2(CPT_H3D) = ID_MAT_MDS(MDSVAR_INPUT(CPT_H3D))
                                 ENDIF

	 	  	         IF (IS_MDSVAR_ALL == 0 .AND. IS_MDSVAR_DEF == 0 .AND. IMDSVAR >= 1)THEN 
	 	  	           MDSVAR_INPUT(CPT_H3D) = MDSVAR_INPUT_TMP(O)
	 	  	           MDSVAR_INPUT1(CPT_H3D) = MDSVAR_INPUT1_TMP(O)
	 	  	           MDSVAR_INPUT2(CPT_H3D) = MDSVAR_INPUT2_TMP(O)
                                 ENDIF

	 	  	         IF (NMDSVAR_MAX /= 0  .AND. IS_MDSVAR_DEF == 1 .AND. 
     .                               KEY3_GLOB(1:3) == 'MDS' .AND. MDS_NMAT /= 0) THEN
	 	  	           MDSVAR_INPUT(CPT_H3D) = MDSVAR_INPUT_TMP(O)
	 	  	           MDSVAR_INPUT1(CPT_H3D) = MDSVAR_INPUT1_TMP(O)
	 	  	           MDSVAR_INPUT2(CPT_H3D) = MDSVAR_INPUT2_TMP(O)
                                 ENDIF
	 	  	         
	 	  	         IDS_INPUT(CPT_H3D) = -1
	 	  	         IF (IS_ID_ALL == 1) IDS_INPUT(CPT_H3D) = P
	 	  	         IF (IS_ID_ALL == 0 .AND. ID >= 1) IDS_INPUT(CPT_H3D) = ID
                 IF ((KEY3_GLOB(1:7) == 'FAILURE').AND.(CPT_IRUP > 0)) THEN 
                   IF (IS_ID_ALL == 1) THEN 
                     IDS_INPUT(CPT_H3D) = IRUP_ID_ALL(P)
                     IRUP_H3D(CPT_H3D)  = IRUP_ALL(P)
                   ELSE 
                     IRUP_H3D(CPT_H3D)  = IRUP_ALL(1)
                   ENDIF
                 ENDIF
c
	 	  	         GAUSS_INPUT(CPT_H3D) = -1
c
           		         IS_AVAILABLE_KEY(CPT_H3D) = 1                              
           		         IF( IS_PLY_ALL == 1) THEN
           		           IF (L > IPT_PLY(M) ) IS_AVAILABLE_KEY(CPT_H3D) = 0
           		         ENDIF
                                 IF(LAYER_INPUT(CPT_H3D) /= -1.OR.PLY_INPUT(CPT_H3D)/=-1) ISH_NPT0=0 

                                 IF (IS_LAYER_ALL == 1 .AND. K > 0 .AND. MDSVAR_INPUT(CPT_H3D) > 0) THEN
                                   IF(IS_LAYER_MAT(INDEX_MAT_MDS(MDSVAR_INPUT(CPT_H3D)),K) == 0) THEN
                                     LAYER_INPUT(CPT_H3D) = -1
                                     IPT_INPUT(CPT_H3D) = -1
                                     IDS_INPUT(CPT_H3D) = -1
                                     PLY_INPUT(CPT_H3D) = -1
                                     UVAR_INPUT (CPT_H3D)= -1
                                     GAUSS_INPUT(CPT_H3D) = -1
                                     MDSVAR_INPUT(CPT_H3D)= -1
                                     MDSVAR_INPUT1(CPT_H3D)= -1
                                     MDSVAR_INPUT2(CPT_H3D)= -1
                                     IS_AVAILABLE_KEY(CPT_H3D) = 1 
c
                                     CPT_H3D = CPT_H3D - 1
           		           ENDIF
           		         ENDIF
c
	 	  	       ENDDO
	 	  	     ENDDO
	 	  	   ENDDO
           		 ENDDO
           	       ENDDO
	 	     ENDDO
C-----------IPT=ALL  w/ NPT=0 add lower                   
                     IF(ISH_NPT0 == 1) THEN
                       IF (IPT_INPUT(CPT_H3D)==1) IPT_INPUT(CPT_H3D)=-2
                       CPT_H3D = CPT_H3D + 1
                       LAYER_INPUT(CPT_H3D) = -1
                       IPT_INPUT(CPT_H3D) = -3
                       IDS_INPUT(CPT_H3D) = -1
                       PLY_INPUT(CPT_H3D) = -1
                       UVAR_INPUT(CPT_H3D) = -1
                       GAUSS_INPUT(CPT_H3D) = -1
                       IS_AVAILABLE_KEY(CPT_H3D) = 1   
                       MDSVAR_INPUT(CPT_H3D) = -1     
                       MDSVAR_INPUT1(CPT_H3D) = -1     
                       MDSVAR_INPUT2(CPT_H3D) = -1                       
                     ENDIF
           	   ENDIF
C                   
           	  ENDIF

           	ELSEIF(H3D_KEYWORD_SHELL%IS_LAYER == 0 .AND. IS_LAYER == 0 .AND. 
     .     	       H3D_KEYWORD_SHELL%IS_IPT == 0 .AND. IS_IPT == 0 .AND. 
     .     	       H3D_KEYWORD_SHELL%IS_PLY == 0 .AND. IS_PLY == 0 .AND. 
     .     	       H3D_KEYWORD_SHELL%IS_UVAR == 0 .AND. IS_UVAR == 0.AND. 
     .     	       H3D_KEYWORD_SHELL%IS_MDSVAR == 0 .AND. IS_MDSVAR == 0 .AND. 
     .     	       H3D_KEYWORD_SHELL%IS_ID == 0 .AND. IS_ID == 0) THEN
           	  CPT_H3D = CPT_H3D + 1
           	  LAYER_INPUT(CPT_H3D) = -1
           	  IPT_INPUT(CPT_H3D) = -1
           	  PLY_INPUT(CPT_H3D) = -1
           	  IDS_INPUT(CPT_H3D) = -1
           	  UVAR_INPUT (CPT_H3D)= -1
           	  MDSVAR_INPUT(CPT_H3D)= -1
           	  MDSVAR_INPUT1(CPT_H3D)= -1
           	  MDSVAR_INPUT2(CPT_H3D)= -1
           	  GAUSS_INPUT(CPT_H3D) = -1
           	  IS_AVAILABLE_KEY(CPT_H3D) = 1   
           	ENDIF
	      ENDIF
c
              DO K=1,CPT_H3D
               
           	IF (IS_AVAILABLE_KEY(K) == 1 .AND. IS_SCALAR == 1) THEN
                  IF (IS_MDSVAR ==1) MDS_LABEL_TMP = MDS_LABEL(MDSVAR_INPUT1(K), MDSVAR_INPUT(K))

           	  IF (H3D_KEYWORD_SHELL_SCALAR(J)%IS_SKIN == 1 ) THEN
           	    CALL CREATE_H3D_SKIN_SCALAR(H3D_DATA,H3D_KEYWORD_SHELL_SCALAR(J)%ID,ID_INPUT,
     .     				       TRIM(H3D_KEYWORD_SHELL_SCALAR(J)%TEXT1),LEN_TRIM(H3D_KEYWORD_SHELL_SCALAR(J)%TEXT1),
     .                         H3D_KEYWORD_SHELL_SCALAR(J)%COMMENT,80,IPART,KEY3_GLOB,
     .     				       LAYER_INPUT(K),IR_INPUT(K),IS_INPUT(K),IT_INPUT(K),
     .     				       UVAR_INPUT(K))
                    IS_SKING = 1
		   ELSE
         
                   IF ((KEY3_GLOB(1:7) == 'FAILURE').AND.(CPT_IRUP > 0)) THEN
                     TEST_CHAIN = ''
                     WRITE(TEST_CHAIN,'(A,1X,A,1X,A)') 
     *                     TRIM(H3D_KEYWORD_SHELL_SCALAR(J)%TEXT1(1:7)),TRIM(FAIL_NAME(IRUP_H3D(K))),'Damage'
                     H3D_KEYWORD_SHELL_SCALAR(J)%TEXT1 = TEST_CHAIN
                   ENDIF
                    
           	   CALL CREATE_H3D_SHELL_SCALAR(H3D_DATA,H3D_KEYWORD_SHELL_SCALAR(J)%ID,N1,N2,ID_INPUT,
     .     					   TRIM(H3D_KEYWORD_SHELL_SCALAR(J)%TEXT1),LEN_TRIM(H3D_KEYWORD_SHELL_SCALAR(J)%TEXT1),
     .                                             H3D_KEYWORD_SHELL_SCALAR(J)%COMMENT,80,IPART,KEY3_GLOB,
     .     					   LAYER_INPUT(K),IPT_INPUT(K),PLY_INPUT(K),GAUSS_INPUT(K),UVAR_INPUT(K),
     .                                             MDSVAR_INPUT(K),MDSVAR_INPUT1(K),MDSVAR_INPUT2(K),
     .                                             MDS_LABEL_TMP,30,IDS_INPUT(K))
              END IF			   
           	  IOK_H3DKEY = 1
           	ELSEIF (IS_AVAILABLE_KEY(K) == 1 .AND. IS_VECTOR == 1) THEN
           	  IF (H3D_KEYWORD_SHELL_VECTOR(J)%IS_SKIN == 1 ) THEN
                  CALL CREATE_H3D_SKIN_VECTOR(H3D_DATA,H3D_KEYWORD_SHELL_VECTOR(J)%ID,ID_INPUT,
     .                                      TRIM(H3D_KEYWORD_SHELL_VECTOR(J)%TEXT1),LEN_TRIM(H3D_KEYWORD_SHELL_VECTOR(J)%TEXT1),
     .                                      H3D_KEYWORD_SHELL_VECTOR(J)%COMMENT,80,IPART,KEY3_GLOB,
     .                                      LAYER_INPUT(K),IR_INPUT(K),IS_INPUT(K),IT_INPUT(K),
     .     				                    UVAR_INPUT(K) )
                    IS_SKING = 1
              ELSE
           	    CALL CREATE_H3D_SHELL_VECTOR(H3D_DATA,H3D_KEYWORD_SHELL_VECTOR(J)%ID,N1,N2,ID_INPUT,
     .     					   TRIM(H3D_KEYWORD_SHELL_VECTOR(J)%TEXT1),LEN_TRIM(H3D_KEYWORD_SHELL_VECTOR(J)%TEXT1),
     .                                             H3D_KEYWORD_SHELL_VECTOR(J)%COMMENT,80,IPART,KEY3_GLOB,
     .     					   LAYER_INPUT(K),IPT_INPUT(K),PLY_INPUT(K),GAUSS_INPUT(K),UVAR_INPUT(K))
              END IF
           	  IOK_H3DKEY = 1 

           	ELSEIF (IS_AVAILABLE_KEY(K) == 1 .AND. IS_TENSOR == 1) THEN
           	  CALL CREATE_H3D_SHELL_TENSOR(H3D_DATA,ID_INPUT,TRIM(H3D_KEYWORD_SHELL_TENSOR(J)%TEXT1),
     .                                         LEN_TRIM(H3D_KEYWORD_SHELL_TENSOR(J)%TEXT1),
     .                                         H3D_KEYWORD_SHELL_TENSOR(J)%COMMENT,80,IPART,KEY3_GLOB,
     .                                         LAYER_INPUT(K),IPT_INPUT(K),PLY_INPUT(K),GAUSS_INPUT(K),
     .                                         UVAR_INPUT(K),IDS_INPUT(K))
           	  IOK_H3DKEY = 1 
           	ENDIF
              ENDDO

            ENDDO
          ENDIF
C-------------------------------------------------- 
          IF(KEY2 == 'SOLID' .OR. KEY2 == 'BRICK' .OR.KEY2 =='ELEM') THEN
C-------------------------------------------------- 
            JMAX=MAX(H3D_NUM_KEY%SOLID_SCALAR, H3D_NUM_KEY%SOLID_VECTOR, H3D_NUM_KEY%SOLID_TENSOR)
	          DO J=1,JMAX
              CPT_H3D = 0
              IS_SCALAR = 0
              IS_VECTOR = 0
              IS_TENSOR = 0
	            IF ( KEY3_GLOB == H3D_KEYWORD_SOLID_SCALAR(J)%KEY3) IS_SCALAR = 1
              IF ( KEY3_GLOB == H3D_KEYWORD_SOLID_VECTOR(J)%KEY3) IS_VECTOR = 1
              IF ( KEY3_GLOB == H3D_KEYWORD_SOLID_TENSOR(J)%KEY3) IS_TENSOR = 1

              IS_CORNER_DATA = H3D_KEYWORD_SOLID_TENSOR(J)%IS_CORNER_DATA

	            IF ( IS_SCALAR == 1 .OR. IS_VECTOR == 1 .OR. IS_TENSOR == 1 ) THEN

           	IF(IS_SCALAR == 1) H3D_KEYWORD_SOLID = H3D_KEYWORD_SOLID_SCALAR(J)
           	IF(IS_VECTOR == 1) H3D_KEYWORD_SOLID = H3D_KEYWORD_SOLID_VECTOR(J)
           	IF(IS_TENSOR == 1) H3D_KEYWORD_SOLID = H3D_KEYWORD_SOLID_TENSOR(J)

           	IF(H3D_KEYWORD_SOLID%IS_LAYER /= 0  .OR. 
     .     	   H3D_KEYWORD_SOLID%IS_IR /= 0 .OR.
     .     	   H3D_KEYWORD_SOLID%IS_IS /= 0 .OR.
     .     	   H3D_KEYWORD_SOLID%IS_IT /= 0 .OR. 
     .     	   H3D_KEYWORD_SOLID%IS_UVAR /= 0  .OR. 
     .     	   H3D_KEYWORD_SOLID%IS_MDSVAR /= 0  .OR. 
     .     	   H3D_KEYWORD_SOLID%IS_ID /= 0 ) THEN

           	  IF(IS_LAYER_ALL == 0 .AND. IS_IR_ALL == 0 .AND. IS_IS_ALL == 0 .AND.
     .     	     IS_IT_ALL == 0 .AND. IS_UVAR_ALL == 0 .AND. IS_MDSVAR_ALL == 0 .AND. 
     .               IS_MDSVAR_DEF ==0 .AND. IS_ID_ALL == 0)THEN

           	    CPT_H3D = CPT_H3D + 1
           	    LAYER_INPUT(CPT_H3D) = -1
           	    UVAR_INPUT (CPT_H3D)= -1
           	    MDSVAR_INPUT(CPT_H3D)= -1
           	    MDSVAR_INPUT1(CPT_H3D)= -1
           	    MDSVAR_INPUT2(CPT_H3D)= -1
           	    IR_INPUT(CPT_H3D) = -1
           	    IS_INPUT(CPT_H3D) = -1
           	    IT_INPUT(CPT_H3D) = -1
           	    IDS_INPUT(CPT_H3D) = -1
           	    GAUSS_INPUT(CPT_H3D) = -1
           	    IS_AVAILABLE_KEY(CPT_H3D) = 1 

           	    IF(H3D_KEYWORD_SOLID%IS_LAYER == 1) THEN
           	      IF(LAYER /= 0) LAYER_INPUT(CPT_H3D) = LAYER
           	      IF(IS_LAYER_LOWER == 1) LAYER_INPUT(CPT_H3D) = -2
           	      IF(IS_LAYER_UPPER == 1) LAYER_INPUT(CPT_H3D) = -3
           	    ENDIF

           	    IF(H3D_KEYWORD_SOLID%IS_IPT == 1) THEN
           	      IF(IPT /= 0) IPT_INPUT(CPT_H3D) = IPT
           	      IF(IS_IPT_LOWER == 1) IPT_INPUT(CPT_H3D) = -2
           	      IF(IS_IPT_UPPER == 1) IPT_INPUT(CPT_H3D) = -3
           	    ENDIF

           	    IF(H3D_KEYWORD_SOLID%IS_UVAR == 1) THEN
           	      IF(IUVAR /= 0) UVAR_INPUT(CPT_H3D) = IUVAR
           	    ENDIF

           	    IF(H3D_KEYWORD_SOLID%IS_MDSVAR == 1) THEN
           	      IF(IMDSVAR /= 0) THEN
                        MDSVAR_INPUT(CPT_H3D) = ((IMDSVAR-1) * MDS_NMAT /NMDSVAR_MAX)+1
           	        MDSVAR_INPUT1(CPT_H3D) = IMDSVAR-(MDSVAR_INPUT(CPT_H3D)-1)*NMDSVAR_MAX/MDS_NMAT
           	        MDSVAR_INPUT2(CPT_H3D) = ID_MAT_MDS(MDSVAR_INPUT(CPT_H3D))
                      ENDIF
           	    ENDIF

           	    IF(H3D_KEYWORD_SOLID%IS_IR == 1) THEN
           	      IF(IR /= 0) IR_INPUT(CPT_H3D) = IR
           	    ENDIF

           	    IF(H3D_KEYWORD_SOLID%IS_IS == 1) THEN
           	      IF(IS /= 0) IS_INPUT(CPT_H3D) = IS
           	    ENDIF

           	    IF(H3D_KEYWORD_SOLID%IS_IT == 1) THEN
           	      IF(IT /= 0) IT_INPUT(CPT_H3D) = IT
           	    ENDIF

           	    IF(H3D_KEYWORD_SOLID%IS_ID == 1) THEN
           	      IF(ID /= 0) IDS_INPUT(CPT_H3D) = ID
                  IF ((KEY3_GLOB(1:7) == 'FAILURE').AND.(CPT_IRUP > 0)) THEN 
                    IRUP_H3D(CPT_H3D) = IRUP_ALL(1)
                  ENDIF
           	    ENDIF

           	  ELSE
C------ LAYER is not available for /SOLID/TENS/ in manuel, might be removed in the loop            
	 	   IF(NLAY_MAX + IR_MAX + IS_MAX + IT_MAX + NUVAR_MAX + NMDSVAR_MAX + ID_MAX /= 0)THEN
	 	     DO K=1,MAX(1,IS_LAYER_ALL*MAX(NLAY_MAX,1))
	 	       DO L=1,MAX(1,IS_UVAR_ALL*MAX(NUVAR_MAX,1))
	 	  	 DO M=1,MAX(1,IS_IR_ALL*MAX(IR_MAX,1))
	 	  	   DO N=1,MAX(1,IS_IS_ALL*MAX(IS_MAX,1))
	 	  	     DO O=1,MAX(1,IS_IT_ALL*MAX(IT_MAX,1))
	 	  	       DO P=1,MAX(1,MAX(IS_MDSVAR_ALL,IS_MDSVAR_DEF)*MAX(NMDSVAR_MAX,1))
	 	  	         DO Q=1,MAX(1,IS_ID_ALL*MAX(ID_MAX,1))
	 	  	           CPT_H3D = CPT_H3D + 1
c
	 	  	           LAYER_INPUT(CPT_H3D) = -1
	 	  	           IF (NLAY_MAX /= 0 .AND. IS_LAYER_ALL == 1) LAYER_INPUT(CPT_H3D) = K
c
	 	  	           UVAR_INPUT(CPT_H3D) = -1
           	                   IF(H3D_KEYWORD_SOLID%IS_UVAR == 1 .AND. IUVAR > 0) UVAR_INPUT(CPT_H3D) = IUVAR
	 	  	           IF (NUVAR_MAX /= 0  .AND. IS_UVAR_ALL == 1) UVAR_INPUT(CPT_H3D) = L

	 	  	           MDSVAR_INPUT(CPT_H3D) = -1
	 	  	           MDSVAR_INPUT1(CPT_H3D) = -1
	 	  	           MDSVAR_INPUT2(CPT_H3D) = -1
	 	  	           IF (NMDSVAR_MAX /= 0  .AND. IS_MDSVAR_ALL == 1 .AND. 
     .                                 KEY3_GLOB(1:3) == 'MDS' .AND. MDS_NMAT /= 0) THEN
	 	  	             MDSVAR_INPUT(CPT_H3D) = MDSVAR_INPUT_TMP(P)
	 	  	             MDSVAR_INPUT1(CPT_H3D) = MDSVAR_INPUT1_TMP(P)
	 	  	             MDSVAR_INPUT2(CPT_H3D) = MDSVAR_INPUT2_TMP(P)
                                   ENDIF
c 
	 	  	           IF (IS_MDSVAR_ALL == 0  .AND. IMDSVAR >= 1) MDSVAR_INPUT(CPT_H3D) = IMDSVAR


	 	  	           IF (NMDSVAR_MAX /= 0  .AND. IS_MDSVAR_DEF == 1 .AND. 
     .                                 KEY3_GLOB(1:3) == 'MDS' .AND. MDS_NMAT /= 0) THEN
	 	  	             MDSVAR_INPUT(CPT_H3D) = MDSVAR_INPUT_TMP(P)
	 	  	             MDSVAR_INPUT1(CPT_H3D) = MDSVAR_INPUT1_TMP(P)
	 	  	             MDSVAR_INPUT2(CPT_H3D) = MDSVAR_INPUT2_TMP(P)
                                   ENDIF

	 	  	           IR_INPUT(CPT_H3D) = -1
           	                   IF(H3D_KEYWORD_SOLID%IS_IR == 1 .AND. IR > 0) IR_INPUT(CPT_H3D) = IR
	 	  	           IF (IR_MAX /= 0  .AND. IS_IR_ALL == 1) IR_INPUT(CPT_H3D) = M

	 	  	           IS_INPUT(CPT_H3D) = -1
           	                   IF(H3D_KEYWORD_SOLID%IS_IS == 1 .AND. IS > 0) IS_INPUT(CPT_H3D) = IS
	 	  	           IF (IS_MAX /= 0  .AND. IS_IS_ALL == 1) IS_INPUT(CPT_H3D) = N

	 	  	           IT_INPUT(CPT_H3D) = -1
           	                   IF(H3D_KEYWORD_SOLID%IS_IT == 1 .AND. IT > 0) IT_INPUT(CPT_H3D) = IT
	 	  	           IF (IT_MAX /= 0  .AND. IS_IT_ALL == 1) IT_INPUT(CPT_H3D) = O
c
	 	  	           IDS_INPUT(CPT_H3D) = -1
	 	  	           IF (IS_ID_ALL == 1) IDS_INPUT(CPT_H3D) = Q
	 	  	           IF (IS_ID_ALL == 0 .AND. ID >= 1) IDS_INPUT(CPT_H3D) = ID
                   IF ((KEY3_GLOB(1:7) == 'FAILURE').AND.(CPT_IRUP > 0)) THEN 
                     IF (IS_ID_ALL == 1) THEN 
                       IDS_INPUT(CPT_H3D) = IRUP_ID_ALL(Q)
                       IRUP_H3D(CPT_H3D)  = IRUP_ALL(Q)
                     ELSE 
                       IRUP_H3D(CPT_H3D)  = IRUP_ALL(1)
                     ENDIF
                   ENDIF
	 	  	           GAUSS_INPUT(CPT_H3D) = -1
c
           		           IS_AVAILABLE_KEY(CPT_H3D) = 1 
c
           		         ENDDO
           		       ENDDO
           		     ENDDO
           		   ENDDO
           		 ENDDO
           	       ENDDO
	 	     ENDDO
           	   ENDIF
           	  ENDIF

           	ELSEIF(H3D_KEYWORD_SOLID%IS_LAYER == 0 .AND. IS_LAYER == 0 .AND. 
     .     	       H3D_KEYWORD_SOLID%IS_IR == 0 .AND. IS_IR == 0 .AND. 
     .     	       H3D_KEYWORD_SOLID%IS_IS == 0 .AND. IS_IS == 0 .AND. 
     .     	       H3D_KEYWORD_SOLID%IS_IT == 0 .AND. IS_IT == 0 .AND. 
     .     	       H3D_KEYWORD_SOLID%IS_UVAR == 0 .AND. IS_UVAR == 0 .AND. 
     .     	       H3D_KEYWORD_SOLID%IS_MDSVAR == 0 .AND. IS_MDSVAR == 0 .AND. 
     .     	       H3D_KEYWORD_SOLID%IS_ID == 0 .AND. IS_ID == 0 ) THEN
           	  CPT_H3D = CPT_H3D + 1
           	  LAYER_INPUT(CPT_H3D) = -1
           	  UVAR_INPUT (CPT_H3D)= -1
           	  MDSVAR_INPUT(CPT_H3D)= -1
           	  MDSVAR_INPUT1(CPT_H3D)= -1
           	  MDSVAR_INPUT2(CPT_H3D)= -1
           	  GAUSS_INPUT(CPT_H3D) = -1
           	  IR_INPUT(CPT_H3D) = -1
           	  IS_INPUT(CPT_H3D) = -1
           	  IT_INPUT(CPT_H3D) = -1
                  IDS_INPUT(CPT_H3D) = -1
           	  IS_AVAILABLE_KEY(CPT_H3D) = 1 
           	ENDIF
	      ENDIF
c
              DO K=1,CPT_H3D
           	IF (IS_AVAILABLE_KEY(K) == 1 .AND. IS_SCALAR == 1) THEN
           	  IF (H3D_KEYWORD_SOLID_SCALAR(J)%IS_SKIN == 1 ) THEN
           	    CALL CREATE_H3D_SKIN_SCALAR(H3D_DATA,H3D_KEYWORD_SOLID_SCALAR(J)%ID,ID_INPUT,
     .     				       TRIM(H3D_KEYWORD_SOLID_SCALAR(J)%TEXT1),LEN_TRIM(H3D_KEYWORD_SOLID_SCALAR(J)%TEXT1),
     .                                         H3D_KEYWORD_SOLID_SCALAR(J)%COMMENT,80,IPART,KEY3_GLOB,
     .     				       LAYER_INPUT(K),IR_INPUT(K),IS_INPUT(K),IT_INPUT(K),
     .     				       UVAR_INPUT(K))
                    IS_SKING = 1
                 ELSE
                    IF (IS_MDSVAR ==1) MDS_LABEL_TMP = MDS_LABEL(MDSVAR_INPUT1(K), MDSVAR_INPUT(K))
 
                    IF ((KEY3_GLOB(1:7) == 'FAILURE').AND.(CPT_IRUP > 0)) THEN
                      TEST_CHAIN = ''
                      WRITE(TEST_CHAIN,'(A,1X,A,1X,A)') 
     *                      TRIM(H3D_KEYWORD_SOLID_SCALAR(J)%TEXT1(1:7)),TRIM(FAIL_NAME(IRUP_H3D(K))),'Damage'
                      H3D_KEYWORD_SOLID_SCALAR(J)%TEXT1 = TEST_CHAIN
                    ENDIF

           	    CALL CREATE_H3D_SOLID_SCALAR(H3D_DATA,H3D_KEYWORD_SOLID_SCALAR(J)%ID,ID_INPUT,
     .     				       TRIM(H3D_KEYWORD_SOLID_SCALAR(J)%TEXT1),LEN_TRIM(H3D_KEYWORD_SOLID_SCALAR(J)%TEXT1),
     .                                         H3D_KEYWORD_SOLID_SCALAR(J)%COMMENT,80,IPART,KEY3_GLOB,
     .     				       LAYER_INPUT(K),IR_INPUT(K),IS_INPUT(K),IT_INPUT(K),
     .     				       UVAR_INPUT(K),MDSVAR_INPUT(K),MDSVAR_INPUT1(K),
     .                                         MDSVAR_INPUT2(K),MDS_LABEL_TMP,30,IDS_INPUT(K))
                  END IF
           	  IOK_H3DKEY = 1
           	ELSEIF (IS_AVAILABLE_KEY(K) == 1 .AND. IS_VECTOR == 1) THEN
           	  IF (H3D_KEYWORD_SOLID_VECTOR(J)%IS_SKIN == 1 ) THEN
                  CALL CREATE_H3D_SKIN_VECTOR(H3D_DATA,H3D_KEYWORD_SOLID_VECTOR(J)%ID,ID_INPUT,
     .                                      TRIM(H3D_KEYWORD_SOLID_VECTOR(J)%TEXT1),LEN_TRIM(H3D_KEYWORD_SOLID_VECTOR(J)%TEXT1),
     .                                      H3D_KEYWORD_SOLID_VECTOR(J)%COMMENT,80,IPART,KEY3_GLOB,
     .                                      LAYER_INPUT(K),IR_INPUT(K),IS_INPUT(K),IT_INPUT(K),
     .     				                    UVAR_INPUT(K) )
                   IS_SKING = 1
              ELSE
           	    CALL CREATE_H3D_SOLID_VECTOR(H3D_DATA,H3D_KEYWORD_SOLID_VECTOR(J)%ID,ID_INPUT,
     .     				       TRIM(H3D_KEYWORD_SOLID_VECTOR(J)%TEXT1),LEN_TRIM(H3D_KEYWORD_SOLID_VECTOR(J)%TEXT1),
     .                                         H3D_KEYWORD_SOLID_VECTOR(J)%COMMENT,80,IPART,KEY3_GLOB,
     .     				       LAYER_INPUT(K),IR_INPUT(K),IS_INPUT(K),IT_INPUT(K),
     .     				       UVAR_INPUT(K))
              END IF
           	  IOK_H3DKEY = 1
           	ELSEIF (IS_AVAILABLE_KEY(K) == 1 .AND. IS_TENSOR == 1) THEN
           	  IF (H3D_KEYWORD_SOLID_TENSOR(J)%IS_SKIN == 1 ) THEN
           	    CALL CREATE_H3D_SKIN_TENSOR(H3D_DATA,H3D_KEYWORD_SOLID_TENSOR(J)%ID,ID_INPUT,
     .     	                             TRIM(H3D_KEYWORD_SOLID_TENSOR(J)%TEXT1),LEN_TRIM(H3D_KEYWORD_SOLID_TENSOR(J)%TEXT1),
     .                                       H3D_KEYWORD_SOLID_TENSOR(J)%COMMENT,80,IPART,KEY3_GLOB,
     .     	                             LAYER_INPUT(K),IR_INPUT(K),IS_INPUT(K),IT_INPUT(K),
     .     	                             UVAR_INPUT(K))
                    IS_SKING = 1
                  ELSE
           	    CALL CREATE_H3D_SOLID_TENSOR(H3D_DATA,H3D_KEYWORD_SOLID_TENSOR(J)%ID,ID_INPUT,
     .     				       TRIM(H3D_KEYWORD_SOLID_TENSOR(J)%TEXT1),LEN_TRIM(H3D_KEYWORD_SOLID_TENSOR(J)%TEXT1),
     .                                         H3D_KEYWORD_SOLID_TENSOR(J)%COMMENT,80,IPART,KEY3_GLOB,
     .     				       LAYER_INPUT(K),IR_INPUT(K),IS_INPUT(K),IT_INPUT(K),
     .     				       UVAR_INPUT(K) ,IS_CORNER_DATA,IDS_INPUT(K))
                  END IF
           	  IOK_H3DKEY = 1
C-----------------                 
           	ENDIF
              ENDDO
	    ENDDO
          ENDIF
C-------------------------------------------------- 
          IF(KEY2 == 'SPH' .OR. KEY2 =='ELEM') THEN
C--------------------------------------------------
            JMAX=MAX(H3D_NUM_KEY%SPH_SCALAR, H3D_NUM_KEY%SPH_TENSOR)
	    DO J=1,JMAX
        CPT_H3D = 0
        IF (H3D_KEYWORD_SPH_SCALAR(J)%IS_ID /= 0 ) THEN
          IF (IS_ID_ALL == 0) THEN
            CPT_H3D = CPT_H3D + 1
            IDS_INPUT(CPT_H3D) = -1
            IF (H3D_KEYWORD_SPH_SCALAR(J)%IS_ID == 1) THEN
              IF (ID /= 0) IDS_INPUT(CPT_H3D) = ID
              IF ((KEY3_GLOB(1:7) == 'FAILURE').AND.(CPT_IRUP > 0)) THEN 
                IRUP_H3D(CPT_H3D) = IRUP_ALL(1)
              ENDIF
            ENDIF
          ELSE
            IF (ID_MAX /= 0) THEN
              DO Q=1,MAX(1,IS_ID_ALL*MAX(ID_MAX,1))
                CPT_H3D = CPT_H3D + 1
                IDS_INPUT(CPT_H3D) = -1
                IF (IS_ID_ALL == 1) IDS_INPUT(CPT_H3D) = Q
                IF (IS_ID_ALL == 0 .AND. ID >= 1) IDS_INPUT(CPT_H3D) = ID
                IF ((KEY3_GLOB(1:7) == 'FAILURE').AND.(CPT_IRUP > 0)) THEN 
                  IF (IS_ID_ALL == 1) THEN 
                    IDS_INPUT(CPT_H3D) = IRUP_ID_ALL(Q)
                    IRUP_H3D(CPT_H3D)  = IRUP_ALL(Q)
                  ELSE 
                    IRUP_H3D(CPT_H3D)  = IRUP_ALL(1)
                  ENDIF
                ENDIF
              ENDDO 
            ENDIF
          ENDIF
        ELSEIF(H3D_KEYWORD_SHELL%IS_ID == 0 .AND. IS_ID == 0) THEN
          CPT_H3D = CPT_H3D + 1
          IDS_INPUT(CPT_H3D) = -1
        ENDIF

	      IF ( KEY3_GLOB == H3D_KEYWORD_SPH_SCALAR(J)%KEY3 )THEN

            IF((KEY3_GLOB(1:7) == 'FAILURE').AND.(CPT_IRUP > 0)) THEN
              DO K=1,CPT_H3D
                IF (CPT_IRUP > 0) THEN 
                  TEST_CHAIN = ''
                  WRITE(TEST_CHAIN,'(A,1X,A,1X,A)') 
     .                  TRIM(H3D_KEYWORD_SPH_SCALAR(J)%TEXT1(1:7)),TRIM(FAIL_NAME(IRUP_H3D(K))),'Damage'
                  H3D_KEYWORD_SPH_SCALAR(J)%TEXT1 = TEST_CHAIN
                ENDIF
                CALL CREATE_H3D_SPH_SCALAR(H3D_DATA,H3D_KEYWORD_SPH_SCALAR(J)%ID,ID_INPUT,
     .     				     TRIM(H3D_KEYWORD_SPH_SCALAR(J)%TEXT1),LEN_TRIM(H3D_KEYWORD_SPH_SCALAR(J)%TEXT1),
     .                                       H3D_KEYWORD_SPH_SCALAR(J)%COMMENT,80,IPART,KEY3_GLOB,
     .                                       IUVAR,IDS_INPUT(K))
              ENDDO
            ELSE
           	  CALL CREATE_H3D_SPH_SCALAR(H3D_DATA,H3D_KEYWORD_SPH_SCALAR(J)%ID,ID_INPUT,
     .     				     TRIM(H3D_KEYWORD_SPH_SCALAR(J)%TEXT1),LEN_TRIM(H3D_KEYWORD_SPH_SCALAR(J)%TEXT1),
     .                                       H3D_KEYWORD_SPH_SCALAR(J)%COMMENT,80,IPART,KEY3_GLOB,
     .                                       IUVAR,IDS_INPUT(1))
            ENDIF
           	IOK_H3DKEY = 1
	      ENDIF
c
	      IF ( KEY3_GLOB == H3D_KEYWORD_SPH_TENSOR(J)%KEY3 )THEN
               
           	CALL CREATE_H3D_SPH_TENSOR(H3D_DATA,H3D_KEYWORD_SPH_TENSOR(J)%ID,ID_INPUT,
     .     				  TRIM(H3D_KEYWORD_SPH_TENSOR(J)%TEXT1),LEN_TRIM(H3D_KEYWORD_SPH_TENSOR(J)%TEXT1),
     .                                    H3D_KEYWORD_SPH_TENSOR(J)%COMMENT,80,IPART,KEY3_GLOB)
           	IOK_H3DKEY = 1
	      ENDIF
	    ENDDO
          ENDIF
C-------------------------------------------------- 
          IF(KEY2 == 'BEAM' .OR. KEY2 == 'SPRING' .OR. KEY2 == 'TRUSS' .OR. KEY2 =='ELEM') THEN
C-------------------------------------------------- 
            JMAX=MAX(H3D_NUM_KEY%ONED_SCALAR, H3D_NUM_KEY%ONED_VECTOR, H3D_NUM_KEY%ONED_TENSOR, H3D_NUM_KEY%ONED_TORSOR)
	    DO J=1,JMAX
              CPT_H3D = 0
              IS_SCALAR = 0
              IS_VECTOR = 0
              IS_TENSOR = 0
              IS_TORSOR = 0
              IF ( KEY3_GLOB == H3D_KEYWORD_ONED_SCALAR(J)%KEY3 ) IS_SCALAR = 1
              IF ( KEY3_GLOB == H3D_KEYWORD_ONED_VECTOR(J)%KEY3 ) IS_VECTOR = 1
              IF ( KEY3_GLOB == H3D_KEYWORD_ONED_TENSOR(J)%KEY3 ) IS_TENSOR = 1
              IF ( KEY3_GLOB == H3D_KEYWORD_ONED_TORSOR(J)%KEY3 ) IS_TORSOR = 1
C
C
              IF ( IS_SCALAR == 1 .OR. IS_VECTOR == 1 .OR. IS_TENSOR == 1 .OR. IS_TORSOR == 1 ) THEN

                IF (IS_SCALAR == 1 ) H3D_KEYWORD_ONED = H3D_KEYWORD_ONED_SCALAR(J)
                IF (IS_VECTOR == 1 ) H3D_KEYWORD_ONED = H3D_KEYWORD_ONED_VECTOR(J)
                IF (IS_TENSOR == 1 ) H3D_KEYWORD_ONED = H3D_KEYWORD_ONED_TENSOR(J)
                IF (IS_TORSOR == 1 ) H3D_KEYWORD_ONED = H3D_KEYWORD_ONED_TORSOR(J)

                IF ( H3D_KEYWORD_ONED%IS_IPT /= 0 ) THEN
C            
                  IF ( IS_IPT_ALL == 0 ) THEN
                    CPT_H3D = CPT_H3D + 1
                    IPT_INPUT(CPT_H3D) = -1
                    IS_AVAILABLE_KEY(CPT_H3D) = 1 
                    IF ( H3D_KEYWORD_ONED%IS_IPT == 1 ) THEN
                      IF ( IPT /= 0 ) IPT_INPUT(CPT_H3D) = IPT
                    ENDIF
                  ELSE   !  IF ( IS_IPT_ALL == 0 )
                    IF ( NIP_MAX /= 0 ) THEN
                      DO L=1,MAX(1,IS_IPT_ALL*MAX(NIP_MAX,1))
                        CPT_H3D = CPT_H3D + 1
                        IPT_INPUT(CPT_H3D) = -1
                        IF ( NIP_MAX /= 0  .AND. IS_IPT_ALL == 1 ) IPT_INPUT(CPT_H3D) = L
                        IF (IS_IPT_ALL == 0 .AND. IPT >= 1) IPT_INPUT(CPT_H3D) = IPT
                        IS_AVAILABLE_KEY(CPT_H3D) = 1                              
                      ENDDO
                    ENDIF
                  ENDIF ! IF ( IS_IPT_ALL == 0 )
                ELSEIF ( H3D_KEYWORD_ONED%IS_IPT == 0   .AND. IS_IPT == 0 ) THEN
                  CPT_H3D = CPT_H3D + 1
                  IPT_INPUT(CPT_H3D) = -1
                  IS_AVAILABLE_KEY(CPT_H3D) = 1 
                ENDIF ! IF ( H3D_KEYWORD_ONED%IS_IPT /= 0 )
              ENDIF ! IF ( IS_SCALAR == 1 .OR. IS_VECTOR == 1 .OR. IS_TENSOR == 1  .OR. IS_TORSOR == 1 )
c
              DO K=1,CPT_H3D
                IF (IS_AVAILABLE_KEY(K) == 1 .AND. IS_SCALAR == 1) THEN
                  CALL CREATE_H3D_1D_SCALAR(H3D_DATA,H3D_KEYWORD_ONED_SCALAR(J)%ID,ID_INPUT,
     .                                      TRIM(H3D_KEYWORD_ONED_SCALAR(J)%TEXT1),LEN_TRIM(H3D_KEYWORD_ONED_SCALAR(J)%TEXT1),
     .                                      H3D_KEYWORD_ONED_SCALAR(J)%COMMENT,80,IPART,KEY3_GLOB,IPT_INPUT(K))
                  IOK_H3DKEY = 1 
                ELSEIF (IS_AVAILABLE_KEY(K) == 1 .AND. IS_VECTOR == 1) THEN
                  CALL CREATE_H3D_1D_VECTOR(H3D_DATA,H3D_KEYWORD_ONED_VECTOR(J)%ID,ID_INPUT,
     .                                      TRIM(H3D_KEYWORD_ONED_VECTOR(J)%TEXT1),LEN_TRIM(H3D_KEYWORD_ONED_VECTOR(J)%TEXT1),
     .                                      H3D_KEYWORD_ONED_VECTOR(J)%COMMENT,80,IPART,KEY3_GLOB)
                  IOK_H3DKEY = 1
                ELSEIF (IS_AVAILABLE_KEY(K) == 1 .AND. IS_TENSOR == 1) THEN
                  CALL CREATE_H3D_1D_TENSOR(H3D_DATA,H3D_KEYWORD_ONED_TENSOR(J)%ID,ID_INPUT,
     .                                      TRIM(H3D_KEYWORD_ONED_TENSOR(J)%TEXT1),LEN_TRIM(H3D_KEYWORD_ONED_TENSOR(J)%TEXT1),
     .                                      H3D_KEYWORD_ONED_TENSOR(J)%COMMENT,80,IPART,KEY3_GLOB,IPT_INPUT(K))
                  IOK_H3DKEY = 1
                ELSEIF (IS_AVAILABLE_KEY(K) == 1 .AND. IS_TORSOR == 1) THEN
          	  CALL CREATE_H3D_1D_TORSOR(H3D_DATA,H3D_KEYWORD_ONED_TORSOR(J)%ID,ID_INPUT,
     .    				    TRIM(H3D_KEYWORD_ONED_TORSOR(J)%TEXT1),LEN_TRIM(H3D_KEYWORD_ONED_TORSOR(J)%TEXT1),
     .                                      H3D_KEYWORD_ONED_TORSOR(J)%COMMENT,80,IPART,KEY3_GLOB)
          	  IOK_H3DKEY = 1
                ENDIF
              ENDDO ! DO K=1,CPT_H3D
c
	    ENDDO
          ENDIF
C-------------------------------------------------- 
          IF(KEY2 == 'QUAD' .OR. KEY2 =='ELEM') THEN
C--------------------------------------------------
            JMAX=MAX(H3D_NUM_KEY%QUAD_SCALAR, H3D_NUM_KEY%QUAD_VECTOR, H3D_NUM_KEY%QUAD_TENSOR)
	    DO J=1,JMAX
              CPT_H3D = 0
              IS_SCALAR = 0
              IS_VECTOR = 0
              IS_TENSOR = 0
	      IF ( KEY3_GLOB == H3D_KEYWORD_QUAD_SCALAR(J)%KEY3) IS_SCALAR = 1
	      IF ( KEY3_GLOB == H3D_KEYWORD_QUAD_VECTOR(J)%KEY3) IS_VECTOR = 1
              IF ( KEY3_GLOB == H3D_KEYWORD_QUAD_TENSOR(J)%KEY3) IS_TENSOR = 1
	      IF ( IS_SCALAR == 1 .OR. IS_VECTOR == 1 .OR. IS_TENSOR == 1 ) THEN
           	IF(IS_SCALAR == 1) H3D_KEYWORD_QUAD = H3D_KEYWORD_QUAD_SCALAR(J)
           	IF(IS_VECTOR == 1) H3D_KEYWORD_QUAD = H3D_KEYWORD_QUAD_VECTOR(J)
           	IF(IS_TENSOR == 1) H3D_KEYWORD_QUAD = H3D_KEYWORD_QUAD_TENSOR(J)
           	IF(H3D_KEYWORD_QUAD%IS_LAYER /= 0  .OR. 
     .     	   H3D_KEYWORD_QUAD%IS_IR /= 0 .OR.
     .     	   H3D_KEYWORD_QUAD%IS_IS /= 0 .OR.
     .     	   H3D_KEYWORD_QUAD%IS_IT /= 0 .OR. 
     .     	   H3D_KEYWORD_QUAD%IS_UVAR /= 0 .OR. 
     .         H3D_KEYWORD_QUAD%IS_ID /= 0  ) THEN
           	  IF(IS_LAYER_ALL == 0 .AND. IS_IR_ALL == 0 .AND. IS_IS_ALL == 0 .AND.
     .     	     IS_IT_ALL == 0 .AND. IS_UVAR_ALL == 0 .AND. IS_ID_ALL ==0)THEN

           	    CPT_H3D = CPT_H3D + 1
           	    LAYER_INPUT(CPT_H3D) = -1
           	    UVAR_INPUT (CPT_H3D)= -1
           	    MDSVAR_INPUT(CPT_H3D)= -1
           	    MDSVAR_INPUT1(CPT_H3D)= -1
           	    MDSVAR_INPUT2(CPT_H3D)= -1
           	    IR_INPUT(CPT_H3D) = -1
           	    IS_INPUT(CPT_H3D) = -1
           	    IT_INPUT(CPT_H3D) = -1
           	    GAUSS_INPUT(CPT_H3D) = -1
           	    IS_AVAILABLE_KEY(CPT_H3D) = 1 

           	    IF(H3D_KEYWORD_QUAD%IS_LAYER == 1) THEN
           	      IF(LAYER /= 0) LAYER_INPUT(CPT_H3D) = LAYER
           	      IF(IS_LAYER_LOWER == 1) LAYER_INPUT(CPT_H3D) = -2
           	      IF(IS_LAYER_UPPER == 1) LAYER_INPUT(CPT_H3D) = -3
           	    ENDIF

           	    IF(H3D_KEYWORD_QUAD%IS_IPT == 1) THEN
           	      IF(IPT /= 0) IPT_INPUT(CPT_H3D) = IPT
           	      IF(IS_IPT_LOWER == 1) IPT_INPUT(CPT_H3D) = -2
           	      IF(IS_IPT_UPPER == 1) IPT_INPUT(CPT_H3D) = -3
           	    ENDIF

           	    IF(H3D_KEYWORD_QUAD%IS_UVAR == 1) THEN
           	      IF(IUVAR /= 0) UVAR_INPUT(CPT_H3D) = IUVAR
           	    ENDIF

           	    IF(H3D_KEYWORD_QUAD%IS_IR == 1) THEN
           	      IF(IR /= 0) IR_INPUT(CPT_H3D) = IR
           	    ENDIF

           	    IF(H3D_KEYWORD_QUAD%IS_IS == 1) THEN
           	      IF(IS /= 0) IS_INPUT(CPT_H3D) = IS
           	    ENDIF

           	    IF(H3D_KEYWORD_QUAD%IS_IT == 1) THEN
           	      IF(IT /= 0) IT_INPUT(CPT_H3D) = IT
           	    ENDIF

           	    IF(H3D_KEYWORD_QUAD%IS_ID == 1) THEN
           	      IF(ID /= 0) IDS_INPUT(CPT_H3D) = ID
                  IF ((KEY3_GLOB(1:7) == 'FAILURE').AND.(CPT_IRUP > 0)) THEN 
                    IRUP_H3D(CPT_H3D) = IRUP_ALL(1)
                  ENDIF
           	    ENDIF
           	  ELSE
	 	   IF(NLAY_MAX + IR_MAX + IS_MAX + IT_MAX + NUVAR_MAX + ID_MAX /= 0)THEN
	 	     DO K=1,MAX(1,IS_LAYER_ALL*MAX(NLAY_MAX,1))
	 	       DO L=1,MAX(1,IS_UVAR_ALL*MAX(NUVAR_MAX,1))
	 	  	 DO M=1,MAX(1,IS_IR_ALL*MAX(IR_MAX,1))
	 	  	   DO N=1,MAX(1,IS_IS_ALL*MAX(IS_MAX,1))
	 	  	     DO O=1,MAX(1,IS_IT_ALL*MAX(IT_MAX,1))
              DO P=1,MAX(1,IS_ID_ALL*MAX(ID_MAX,1))
	 	  	       CPT_H3D = CPT_H3D + 1
c
	 	  	       LAYER_INPUT(CPT_H3D) = -1
	 	  	       IF (NLAY_MAX /= 0 .AND. IS_LAYER_ALL == 1) LAYER_INPUT(CPT_H3D) = K
c
	 	  	       UVAR_INPUT(CPT_H3D) = -1
	 	  	       IF (NUVAR_MAX /= 0  .AND. IS_UVAR_ALL == 1) UVAR_INPUT(CPT_H3D) = L

	 	  	       IR_INPUT(CPT_H3D) = -1
	 	  	       IF (IR_MAX /= 0  .AND. IS_IR_ALL == 1) IR_INPUT(CPT_H3D) = M

	 	  	       IS_INPUT(CPT_H3D) = -1
	 	  	       IF (IS_MAX /= 0  .AND. IS_IS_ALL == 1) IS_INPUT(CPT_H3D) = N

	 	  	       IT_INPUT(CPT_H3D) = -1
	 	  	       IF (IT_MAX /= 0  .AND. IS_IT_ALL == 1) IT_INPUT(CPT_H3D) = O

           	   IDS_INPUT(CPT_H3D) = -1
           	   IF(ID /= 0) IDS_INPUT(CPT_H3D) = ID
               IF ((KEY3_GLOB(1:7) == 'FAILURE').AND.(CPT_IRUP > 0)) THEN 
                 IF (IS_ID_ALL == 1) THEN 
                   IDS_INPUT(CPT_H3D) = IRUP_ID_ALL(P)
                   IRUP_H3D(CPT_H3D)  = IRUP_ALL(P)
                 ELSE 
                   IRUP_H3D(CPT_H3D)  = IRUP_ALL(1)
                 ENDIF
               ENDIF
c
	 	  	       GAUSS_INPUT(CPT_H3D) = -1
c
           		       IS_AVAILABLE_KEY(CPT_H3D) = 1 
c
              ENDDO
           		    ENDDO
           		   ENDDO
           		 ENDDO
           	       ENDDO
	 	     ENDDO
           	   ENDIF
           	  ENDIF

           	ELSEIF(H3D_KEYWORD_QUAD%IS_LAYER == 0 .AND. IS_LAYER == 0 .AND. 
     .     	       H3D_KEYWORD_QUAD%IS_IR == 0 .AND. IS_IR == 0 .AND. 
     .     	       H3D_KEYWORD_QUAD%IS_IS == 0 .AND. IS_IS == 0 .AND. 
     .     	       H3D_KEYWORD_QUAD%IS_IT == 0 .AND. IS_IT == 0 .AND. 
     .     	       H3D_KEYWORD_QUAD%IS_UVAR == 0 .AND. IS_UVAR == 0 .AND.
     .             H3D_KEYWORD_QUAD%IS_ID == 0 .AND. IS_ID == 0 ) THEN
           	  CPT_H3D = CPT_H3D + 1
           	  LAYER_INPUT(CPT_H3D) = -1
           	  UVAR_INPUT (CPT_H3D)= -1
           	  MDSVAR_INPUT(CPT_H3D)= -1
           	  MDSVAR_INPUT1(CPT_H3D)= -1
           	  MDSVAR_INPUT2(CPT_H3D)= -1
           	  GAUSS_INPUT(CPT_H3D) = -1
           	  IR_INPUT(CPT_H3D) = -1
           	  IS_INPUT(CPT_H3D) = -1
           	  IT_INPUT(CPT_H3D) = -1
              IDS_INPUT(CPT_H3D) = -1
           	  IS_AVAILABLE_KEY(CPT_H3D) = 1 
           	ENDIF
	      ENDIF

              DO K=1,CPT_H3D
	        IF (IS_AVAILABLE_KEY(K) == 1 .AND. IS_SCALAR == 1)THEN

                   IF ((KEY3_GLOB(1:7) == 'FAILURE').AND.(CPT_IRUP > 0)) THEN
                     TEST_CHAIN = ''
                     WRITE(TEST_CHAIN,'(A,1X,A,1X,A)') 
     *                     TRIM(H3D_KEYWORD_QUAD_SCALAR(J)%TEXT1(1:7)),TRIM(FAIL_NAME(IRUP_H3D(K))),'Damage'
                     H3D_KEYWORD_QUAD_SCALAR(J)%TEXT1 = TEST_CHAIN
                   ENDIF


                  CALL CREATE_H3D_QUAD_SCALAR(H3D_DATA,H3D_KEYWORD_QUAD,ID_INPUT,
     .                                      TRIM(H3D_KEYWORD_QUAD_SCALAR(J)%TEXT1),LEN_TRIM(H3D_KEYWORD_QUAD_SCALAR(J)%TEXT1),
     .                                      H3D_KEYWORD_QUAD_SCALAR(J)%COMMENT,80,IPART,KEY3_GLOB,
     .                                      LAYER_INPUT(K),IR_INPUT(K),IS_INPUT(K),IT_INPUT(K),
     .     				                          UVAR_INPUT(K),IDS_INPUT(K))
                  IOK_H3DKEY = 1
	        ENDIF

	        IF (IS_AVAILABLE_KEY(K) == 1 .AND. IS_VECTOR == 1)THEN
                 
                  CALL CREATE_H3D_QUAD_VECTOR(H3D_DATA,H3D_KEYWORD_QUAD,ID_INPUT,
     .                                      TRIM(H3D_KEYWORD_QUAD_VECTOR(J)%TEXT1),LEN_TRIM(H3D_KEYWORD_QUAD_VECTOR(J)%TEXT1),
     .                                      H3D_KEYWORD_QUAD_VECTOR(J)%COMMENT,80,IPART,KEY3_GLOB,
     .                                      LAYER_INPUT(K),IR_INPUT(K),IS_INPUT(K),IT_INPUT(K),
     .     				    UVAR_INPUT(K) )
                  IOK_H3DKEY = 1
	        ENDIF

	        IF (IS_AVAILABLE_KEY(K) == 1 .AND. IS_TENSOR == 1)THEN
              
                  CALL CREATE_H3D_QUAD_TENSOR(H3D_DATA,H3D_KEYWORD_QUAD_TENSOR(J)%ID,ID_INPUT,
     .                                   TRIM(H3D_KEYWORD_QUAD_TENSOR(J)%TEXT1),LEN_TRIM(H3D_KEYWORD_QUAD_TENSOR(J)%TEXT1),
     .                                   H3D_KEYWORD_QUAD_TENSOR(J)%COMMENT,80,IPART,KEY3_GLOB,
     .                                   LAYER_INPUT(K),IR_INPUT(K),IS_INPUT(K),IT_INPUT(K),
     .     				 UVAR_INPUT(K) )
                  IOK_H3DKEY = 1
	        ENDIF
	      ENDDO
	    ENDDO
          ENDIF

C-------------------------------------------------- 
          IF(KEY2 == 'SHELL' .OR. KEY2 =='ELEM'.OR. KEY2 =='SOLID' .OR.
     .       KEY2 == 'BRICK' .OR. KEY2 =='QUAD'.OR. KEY2 =='BEAM ' .OR.
     .       KEY2 == 'SPRING'.OR. KEY2 =='TRUSS'.OR.KEY2 =='SPH') THEN
C-------------------------------------------------- 
            DEALLOCATE (LAYER_INPUT)
            DEALLOCATE (IPT_INPUT)
            DEALLOCATE (PLY_INPUT)
            DEALLOCATE (UVAR_INPUT)
            DEALLOCATE (GAUSS_INPUT)
            DEALLOCATE (IR_INPUT)
            DEALLOCATE (IS_INPUT)
            DEALLOCATE (IT_INPUT)
            DEALLOCATE (IDS_INPUT)
            DEALLOCATE (IRUP_H3D)
            DEALLOCATE (IS_AVAILABLE_KEY)
            DEALLOCATE (MDSVAR_INPUT)
            DEALLOCATE (MDSVAR_INPUT1)
            DEALLOCATE (MDSVAR_INPUT2)
            DEALLOCATE (MDSVAR_INPUT_TMP)
            DEALLOCATE (MDSVAR_INPUT1_TMP)
            DEALLOCATE (MDSVAR_INPUT2_TMP)
          ENDIF
          IF (ALLOCATED(IS_LAYER_MAT)) DEALLOCATE (IS_LAYER_MAT)
C-------------------------------------------------- 
          IF (IOK_H3DKEY == 0 .AND. ISHELL_NPT_CHECK == 0) THEN
            IF (ISPMD == 0) CALL ANCMSG(MSGID=275,c1='/H3D/'//KEY2//'/'//KEY3//'/'//KEY4//'/'//KEY5//'/'//KEY6,ANMODE=ANINFO)
            IF (ISPMD == 0) CALL ARRET(2)
          ENDIF
        ENDDO
      ENDDO
c
      IF( H3D_DATA%IPART_SELECT == 0 )  CALL CREATE_H3D_PARTS_ALL(H3D_DATA,IPART)
C=========================================================================
      CPT = 0
      DO I=1,H3D_DATA%N_OUTP_H3D
        IF (H3D_DATA%OUTPUT_LIST(I)%OK == 1) THEN
          CPT = CPT + 1
          H3D_DATA%OUTPUT_LIST(I)%N_OUTP = CPT + 2
        ELSE
          H3D_DATA%OUTPUT_LIST(I)%N_OUTP = 0
        ENDIF
      ENDDO
      IF (IS_SKING > 0) THEN
       ISKIND=0
       IF (IOUTER==1) ISKIND=ISKIND+1
       IF (IPEXT==1) ISKIND=ISKIND+2
       CALL H3D_SKIN_DIM(IPARG   ,NUMSKIN, NUMSKING,TAG_SKINS6,
     .                   IBCL    ,ILOADP ,LLOADP   ,LOADS  ,
     .                   NUMSKINP0,NUMSKINP,ISKIND )
      END IF
c For debug purpose
c      DO I=1,H3D_DATA%N_OUTP_H3D
c        DO J=1,H3D_DATA%OUTPUT_LIST(I)%N_H3D_PART_LIST
c          print *,'liste des parts',H3D_DATA%OUTPUT_LIST(I)%H3D_PART_LIST(J)
c        ENDDO
c      ENDDO
C
      DEALLOCATE(H3D_KEYWORD_NODAL_SCALAR)
      DEALLOCATE(H3D_KEYWORD_NODAL_VECTOR)
      DEALLOCATE(H3D_KEYWORD_NODAL_TENSOR)
      DEALLOCATE(H3D_KEYWORD_ONED_SCALAR)
      DEALLOCATE(H3D_KEYWORD_ONED_VECTOR)
      DEALLOCATE(H3D_KEYWORD_ONED_TENSOR)
      DEALLOCATE(H3D_KEYWORD_ONED_TORSOR)
      DEALLOCATE(H3D_KEYWORD_SHELL_SCALAR)
      DEALLOCATE(H3D_KEYWORD_SHELL_VECTOR)
      DEALLOCATE(H3D_KEYWORD_SHELL_TENSOR)
      DEALLOCATE(H3D_KEYWORD_SOLID_SCALAR)
      DEALLOCATE(H3D_KEYWORD_SOLID_VECTOR)
      DEALLOCATE(H3D_KEYWORD_SOLID_TENSOR)
      DEALLOCATE(H3D_KEYWORD_SPH_SCALAR)
      DEALLOCATE(H3D_KEYWORD_SPH_TENSOR)
      DEALLOCATE(H3D_KEYWORD_QUAD_SCALAR)
      DEALLOCATE(H3D_KEYWORD_QUAD_VECTOR)
      DEALLOCATE(H3D_KEYWORD_QUAD_TENSOR)
      DEALLOCATE(ID_MAT_MDS)
      IF (ALLOCATED(IRUP_ALL))    DEALLOCATE (IRUP_ALL)
      IF (ALLOCATED(IRUP_ID_ALL)) DEALLOCATE (IRUP_ID_ALL)
C    
      RETURN
      END
