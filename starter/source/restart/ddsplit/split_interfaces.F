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
Chd|  SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        COPYR8                        source/restart/ddsplit/copyr8.F
Chd|        COPY_IVAL                     source/restart/ddsplit/inter_tools.F
Chd|        COPY_IVAL_IGEO                source/restart/ddsplit/inter_tools.F
Chd|        COPY_NODE_NODLOC              source/restart/ddsplit/inter_tools.F
Chd|        COPY_RVAL                     source/restart/ddsplit/inter_tools.F
Chd|        FILTER_NODE_NODLOC            source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_CAND            source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_CAND_I20_EDGE   source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_CAND_I21        source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_CAND_I25_EDGE   source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I11             source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I17             source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I2              source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I20             source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I21             source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I24             source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I25             source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I7              source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I8              source/restart/ddsplit/inter_tools.F
Chd|        PREPARE_SPLIT_I9              source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_ADSKYN_25               source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_I11                source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_I20                source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_I20_EDGE           source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_I24                source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_I25                source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_I25_EDGE           source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_I7                 source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_IVAL               source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_IVAL_I21           source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_CAND_RVAL               source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_ISEGPT_IVAL             source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_LBOUND_I25              source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_LEDGE_I25               source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_NISUB_I25               source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_NISUB_I7                source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_NODE_IVAL               source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_NODE_IVAL2              source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_NODE_IVAL_I24           source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_NODE_IVAL_I25           source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_NODE_NODLOC             source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_NODE_NODLOC_P0          source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_NODE_RVAL               source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_REMNODE_I11             source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_REMNODE_I24             source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_REMNODE_I25             source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_REMNODE_I25_EDGE        source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_REMNODE_I7              source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_SEGEDGE_NODLOC_I24      source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_SEG_EDGE                source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_SEG_IVAL2               source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_SEG_IVAL_I20            source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_SEG_IVAL_I20_2          source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_SEG_NODLOC              source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_SEG_NODLOC_I24          source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_SEG_RVAL_I20            source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_SEG_SEGLOC              source/restart/ddsplit/inter_tools.F
Chd|        SPLIT_2RY_CAND_IVAL_I21       source/restart/ddsplit/inter_tools.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        W_INTBUF_SIZE                 source/restart/ddsplit/inter_tools.F
Chd|        W_TYPE8                       source/restart/ddsplit/split_interfaces.F
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INT8_MOD                      ../common_source/modules/int8_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_INTERFACES(INTBUF_TAB, IPARI   , PROC    , INTBUF_TAB_L,
     .                            IPARI_L   , INTERCEP, NODLOCAL,
     .                            ITAB      , ITABI2M , NBDDI2M , NUMNOD_L    ,
     .                            CEP       , CEL     , IGRBRIC , T8, MULTI_FVM,
     .                            TAG_NM    ,NINDX_NM ,INDX_NM  ,TAG_SCRATCH,NINDX_SCRT,
     .                            INDX_SCRT ,FLAG_24_25,I24MAXNSNE,INTBUF_FRIC_TAB)  
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE FRONT_MOD
      USE INTBUFDEF_MOD 
      USE INT8_MOD
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
      USE INTBUF_FRIC_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "analyse_name.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "assert.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
!       INDX_XXX : size = NUMNOD
!                 index of non-zero TAG_XXX value 
!                 used for optimize the initialization
!                 of TAG_XXX array (XXX = NM or SCRT for SCRATCH)
!                 allocated array in lectur and threadprivate array
!       NINDX_XXX : number of non-zero TAG_XXX valuee
!       TAG_XXX : size = NUMNOD
!                array used to tag an element for 
!                a given interface ; allocated in lectur
!                allocated array in lectur and threadprivate array
!       FLAG_24_25 : logical, flag for interface 24 or 25
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
      INTEGER 
     .         PROC,IPARI(NPARI,*),IPARI_L(NPARI,NINTER),
     .         NODLOCAL(*),ITAB(*),ITABI2M(*),
     .         NBDDI2M,NUMNOD_L,CEP(*), CEL(*),I24MAXNSNE
      LOGICAL, INTENT(IN) :: FLAG_24_25
      INTEGER, INTENT(INOUT) :: NINDX_NM,NINDX_SCRT
      INTEGER, DIMENSION(*), INTENT(INOUT) :: INDX_NM,INDX_SCRT
      INTEGER, DIMENSION(*) :: TAG_NM,TAG_SCRATCH
c      my_real

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*),INTBUF_TAB_L(*)
      TYPE(INTERSURFP) :: INTERCEP(3,NINTER)
      TYPE(INT8_STRUCT_) :: T8(NSPMD,*)
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
C-----------------------------------------------
      TYPE(INTBUF_FRIC_STRUCT_) :: INTBUF_FRIC_TAB(*) 

C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: FLAG_INIT_24_25
      INTEGER, DIMENSION(:),ALLOCATABLE :: 
     .         TAG_NODE_2RY,TAG_SEGM,TAG_SEGS,
     .         TAG_NODE_MSR,TAG_SEGM2,TAG_II,TAGE_II,
     .         TAG_NODE_MSR2,TAG,TAG_IRTL,
     .         TAG_NLINS, TAG_NLINM,TAG_NLINS2, TAG_NLINM2,
     .         TAG_NLG,TAG_NLG2,IBUF8_L,TAG_NSNE,TAG_NMNE,
     .         TAG_NSVE,TAG_MSRE, TAG_IELES, TAG_IELEM,TAG_NODE_2RY2,
     .         TAG_LMSR,TAG_LMSR2,TAG_NSEG,TAG_NSEG2,
     .         NODLOCAL24,TAG_SEG2S,TAG_NSNE_L,TAG_SM,TAG_2RY_INV,
     .         KNOR2MSR,NOR2MSR,MSR_L_I21,MNDD_I21,TAG_SEGSS

      INTEGER 
     .         I,NI,ITYP,NRTS,NSN,NMN,IGAP,II_STOK,II_STOK_E,
     .         NRTM,INACTI,NOINT,IFQ,INTTH,
     .         NISUB,IFORM,NLINS,NLINM,NLINSA,NLINMA,
     .         NSNE,NMNE,NREMNODE_L,FLAGREMNODE,ILEV,IEDGE,L24ADD,
     .         NUVAR,NLN,NIR,K,N,NL,NMT,NST,ILAGM,NME,NMNG,
     .         MX_VM, MX_VN,INTKG,INTPLY,NRTSE,NRTSE_L,NMT_L,
     .         NRTS_L,NSN_L,NRTM_L,NMN_L,II_STOK_L,II_STOK_E_L,II_STOK_S,II_STOK_S_L,
     .         MULTIMP,NCONT,NCONTE,SIZ,MFROT,NISUBS_L,NISUBM_L,
     .         NLN_L,NLINS_L,NLINM_L,NSNE_L,NMNE_L,NME_L,NMNG_l,
     .         NSN0,NSN0_L,KE,NBT8,P2,NBS_INTERF,NBM_INTERF,NADMSR,
     .         NADMSR_L,IBID,IEDGE4,
     .         NRTM_IGE,NRTM_IGE_L,NRTS_IGE,NRTS_IGE_L,
     .         NRTM_FE, NRTM_FE_L, NRTS_FE, NRTS_FE_L,NMN_FE_L,
     .         NMN_IGE_L,NSN_FE_L,NSN_IGE_L,NMN_FE,
     .         NMN_IGE,NSN_FE,NSN_IGE,L,INTFRIC,NREMNOR,IVIS2,
     .         NEDGE,NEDGE_L,INTNITSCHE,II,NISUBE_L,ITHK,FLAGLOADP
       INTEGER :: NINDX_NDLOCAL24
       INTEGER :: MULTIMPE
c      my_real
c     .        RCONT
!   Local array (integer and my_real arrays)
       INTEGER, DIMENSION(:), ALLOCATABLE :: TABI_ZERO
       my_real, DIMENSION(:), ALLOCATABLE :: TABR_ZERO
       INTEGER, DIMENSION(:), ALLOCATABLE :: I710XSAV
       INTEGER, DIMENSION(:), ALLOCATABLE :: INDX_NDLOCAL24
       INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_EDGE,TAG_EDGE2
       INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_II_E2E
       INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_II_E2S

C ----------------------------------------
!      NODLOCAL24 is used for interface /TYPE24 or /TYPE25
!      value of NODLOCAL24 : 1-->NUMNOD NODLOCAL24 = NODLOCAL
!                          NUMNOD+1-->end NODLOCAL24 = interface dependant
!      NODLOCAL is local to each domain, so it is interface independant
!      for each NI interface, flush NODLOCAL24(NUMNOD+1-->end) to 0 is not
!      necessary but for a future development, I flushed it
!            
       FLAG_INIT_24_25 = .TRUE.
       IF(FLAG_24_25) THEN
                ALLOCATE( NODLOCAL24(NUMNOD+I24MAXNSNE) )
                ALLOCATE( INDX_NDLOCAL24(NUMNOD+I24MAXNSNE) )
       ENDIF
C Care TYPE24 + E2E
C Edge to Edge Nodes fictive Nodes are numbered over NUMNOD 
C Tag Arrays must be increased accordingly
!      I24MAXNSNE = 0
!      DO NI= 1, NINTER
!         ITYP = IPARI(7,NI)
!         IF(ITYP.EQ.24)THEN
!            NSNE   = IPARI(55,NI)
!            I24MAXNSNE = MAX(I24MAXNSNE,NSNE)
!         ENDIF
!      ENDDO

      !allocation outside of the loop for optimisation
      !WARNING!! : must be reflushed to zero after use 
      !(only part of tab that has been used)

!      ALLOCATE(TAG_SCRATCH(NUMNOD+I24MAXNSNE+NUMELS))
!      TAG_SCRATCH(1:NUMNOD+I24MAXNSNE+NUMELS) = 0
C
C     Init tableau de tag pour frontiere int2
C
      IF(NBDDI2M.GT.0) THEN
        DO I = 1, NUMNOD_L
          ITABI2M(I) = 0
        ENDDO
      ENDIF
      NBT8 = 0
      DO NI= 1, NINTER
        NINDX_NM = 0
        NINDX_SCRT = 0
        NINDX_NDLOCAL24 = 0

!        print*,'=============SPLIT INTERFACE PROC NI======',PROC,NI

        !=========================================
        !write all interfaces array sizes      
C       WRITE(6,*) __FILE__,"INTERFACE=",NI
        CALL W_INTBUF_SIZE(INTBUF_TAB_L(NI))
        !=========================================

        !global variables
        NRTS = IPARI(3,NI)
        NRTS_IGE = IPARI(75,NI)
        NRTS_FE  = IPARI(76,NI)
        NRTM = IPARI(4,NI)
        NRTM_IGE = IPARI(73,NI)
        NRTM_FE  = IPARI(74,NI)
        NME = IPARI(4,NI)
        NSN  = IPARI(5,NI)
        NMN  = IPARI(6,NI)
        NMN_FE = IPARI(80,NI)!type7
        NMN_IGE = IPARI(79,NI)!type7
        NSN_FE = IPARI(78,NI)!type7
        NSN_IGE = IPARI(77,NI)!type7
        ITYP = IPARI(7,NI)
        NST  = IPARI(8,NI)
        NMNG = IPARI(8,NI) !type21
        NMT  = IPARI(9,NI)
        IVIS2  = IPARI(14,NI)
        NOINT = IPARI(15,NI)
        ILEV = IPARI(20,NI)
        IGAP = IPARI(21,NI)
        INACTI = ABS(IPARI(22,NI))    ! cas INACTI "passant a 0"
        MFROT = IPARI(30,NI)
        IFQ = IPARI(31,NI)
        ILAGM = IPARI(33,NI) 
        IF(ITYP == 2) THEN        
          NUVAR = IPARI(35,NI)
        ELSEIF(ITYP == 20) THEN
          NLN = IPARI(35,NI)
        ENDIF
        NISUB  = IPARI(36,NI)
        INTTH  = IPARI(47,NI)
        IFORM  = IPARI(48,NI)
        NLINS  = IPARI(51,NI)
        NLINM  = IPARI(52,NI)
        NLINSA = IPARI(53,NI)
        NLINMA = IPARI(54,NI)
        NSNE   = IPARI(55,NI)
        NMNE   = IPARI(56,NI)
        IEDGE = IPARI(58,NI)
        L24ADD = IPARI(59,NI)
        FLAGREMNODE  = IPARI(63,NI)
        INTKG  = IPARI(65,NI)
        INTPLY = IPARI(66,NI)
        NADMSR = IPARI(67,NI)
        NEDGE  = IPARI(68,NI)
        INTFRIC = IPARI(72,NI)
        INTNITSCHE = IPARI(86,NI)
        ITHK = IPARI(91,NI)
        FLAGLOADP = IPARI(95,NI)

        IF (ITYP == 24) NRTSE  = IPARI(52,NI)
        IF (ITYP == 24) IEDGE4 = IPARI(59,NI)

        IF(ITYP==7.OR.ITYP==8.OR.ITYP==10.OR.ITYP==11.OR.ITYP==20.
     .      OR.ITYP==21.OR.ITYP==22.OR.ITYP==23.OR.ITYP==24.
     .      OR.ITYP.EQ.25)THEN
          II_STOK = INTBUF_TAB(NI)%I_STOK(1)
          II_STOK_L = 0
          IF(ITYP==20)THEN
            II_STOK_E = INTBUF_TAB(NI)%I_STOK_E(1)
            II_STOK_E_L = 0
          ENDIF
          IF(ITYP==25)THEN
            II_STOK_E = 0
            II_STOK_E_L = 0
            II_STOK_S = 0
            II_STOK_S_L = 0
            IF(INTBUF_TAB(NI)%S_I_STOK_E > 1) THEN
              II_STOK_E = INTBUF_TAB(NI)%I_STOK_E(1)
              II_STOK_S = INTBUF_TAB(NI)%I_STOK_E(2)
            ENDIF
          ENDIF
        ENDIF

        !local variables
        NRTS_L = IPARI_L(3,NI)
        NRTS_IGE_L = IPARI_L(75,NI)
        NRTS_FE_L = IPARI_L(76,NI)
        NRTM_L = IPARI_L(4,NI)
        NRTM_IGE_L = IPARI_L(73,NI)
        NRTM_FE_L  = IPARI_L(74,NI)
        NME_L = IPARI_L(4,NI)
        NSN_L = IPARI_L(5,NI)
        NMN_L = IPARI_L(6,NI)
        NMN_FE_L = IPARI_L(80,NI)!type7
        NMN_IGE_L = IPARI_L(79,NI)!type7
        NSN_FE_L = IPARI_L(78,NI)!type7
        NSN_IGE_L = IPARI_L(77,NI)!type7
        NMT_L = IPARI_L(9,NI)
        NMNG_L = IPARI_L(8,NI) !type21
        NCONT = IPARI_L(18,NI)
        MULTIMP = IPARI_L(23,NI)
        NISUBS_L = IPARI_L(37,NI)
        NISUBM_L = IPARI_L(38,NI)
        NLN_L    = IPARI_L(35,NI)
        NLINS_L  = IPARI_L(51,NI)
        NLINM _L = IPARI_L(52,NI)
        NSNE_L   = IPARI_L(55,NI)
        NCONTE   = NCONT
        NMNE_L   = IPARI_L(56,NI)
        NREMNODE_L  = IPARI_L(62,NI)
        NREMNOR  = IPARI_L(81,NI)
        IF (ITYP == 24) NRTSE_L = IPARI_L(52,NI)
        NADMSR_L = IPARI_L(67,NI)
        NEDGE_L  = IPARI_L(68,NI)
        NISUBE_L = IPARI_L(90,NI)


c=====================================================================
        IF(ITYP ==1 )THEN
c=====================================================================
          IF(PROC.EQ.0) THEN
            !split and write on disk integers arrays
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRECTS,NRTS,4) !KD(10) 
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRECTM,NRTM,4) !KD(11)
            CALL SPLIT_NODE_NODLOC_P0(INTBUF_TAB(NI)%NSV,NSN,1,NODLOCAL) !KD(12)
            CALL SPLIT_NODE_NODLOC_P0(INTBUF_TAB(NI)%MSR,NMN,1,NODLOCAL) !KD(13)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLM,NSN,1)   !KD(14)
            CALL COPY_IVAL(INTBUF_TAB(NI)%ILOCS,NSN,1)   !KD(16)
            CALL COPY_IVAL(INTBUF_TAB(NI)%NSEGM,1+NMN,1) !KD(21)
            CALL COPY_IVAL(INTBUF_TAB(NI)%NRT,NMT,1)     !KD(14)

            !split and write on disk floating arrays
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFAC,1,1)  
            CALL COPY_RVAL(INTBUF_TAB(NI)%VARIABLES,NPARIR,1) 
            CALL COPY_RVAL(INTBUF_TAB(NI)%CSTS,NSN,2) !JD(12)
            CALL COPY_RVAL(INTBUF_TAB(NI)%N,NSN,3)    !JD(20)  
          ENDIF  
c=====================================================================
        ELSEIF(ITYP == 2)THEN
c=====================================================================
          ALLOCATE(TAG_NODE_2RY(NSN_L),TAG_SEGM(NRTM_L),
     .             TAG_SEGM2(NRTM),TAG(NRTM),TAG_IRTL(NMN_L))
!       useless initialization
          TAG_NODE_2RY(1:NSN_L) = 0
          TAG_SEGM(1:NRTM_L) = 0
          TAG_SEGM2(1:NRTM) = 0
          TAG(1:NRTM) = 0
          TAG_IRTL(1:NMN_L) = 0

          IF (N2D.EQ.0) THEN
            NIR = 4
          ELSE
            NIR = 2
          ENDIF

          !prepare addition arrays used for split
          CALL PREPARE_SPLIT_I2(PROC, INTBUF_TAB(NI), NSN      , NMN      ,
     .                          NRTM, TAG_NODE_2RY  , TAG_SEGM , TAG_SEGM2,
     .                          TAG_IRTL, TAG       , ITABI2M  , NODLOCAL ,
     .                          NBDDI2M , NIR       ,NUMNOD_L)

c split and write on disk integers arrays 
          CALL COPY_IVAL(INTBUF_TAB(NI)%IRECTS,NRTS,4) !KD(10)

          CALL SPLIT_SEG_NODLOC(INTBUF_TAB(NI)%IRECTM,NRTM_L,4,
     .                          TAG_SEGM,NODLOCAL) !KD(11)
          CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%NSV,NSN_L,
     .                          TAG_NODE_2RY,NODLOCAL) !KD(12)
          CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%MSR,NMN_L,
     .                          TAG_IRTL,NODLOCAL) !KD(13)

          CALL SPLIT_NODE_IVAL2(INTBUF_TAB(NI)%IRTLM,NSN_L,1,
     .                          TAG_SEGM2,TAG_NODE_2RY) !KD(14)

          IF (ILEV==10 .OR. ILEV==11 .OR. ILEV==12 .OR.
     *        ILEV==20 .OR. ILEV==21 .OR. ILEV==22) THEN

            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IRUPT,NSN_L,1,
     .                            TAG_NODE_2RY) !KD(14)
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%INORM,NSN_L,1,
     .                            TAG_NODE_2RY) !KD(14)
          ELSEIF ((ILEV == 27).OR.(ILEV == 28)) THEN
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IRUPT,NSN_L,1,TAG_NODE_2RY)
            CALL SPLIT_SEG_IVAL2(INTBUF_TAB(NI)%MSEGTYP2,NRTM_L,TAG_SEGM,TAG_SEGM2)
          ENDIF

c split and write on disk floating arrays 

          CALL COPY_RVAL(INTBUF_TAB(NI)%STFAC,1,1)  
          CALL COPY_RVAL(INTBUF_TAB(NI)%VARIABLES,NPARIR,1)   

          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%CSTS,NSN_L,2,
     .                            TAG_NODE_2RY) !JD(12)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%DPARA,NSN_L,7,
     .                            TAG_NODE_2RY) !JD(21)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%NMAS(1),NMN_L,1,
     .                            TAG_IRTL) !JD(22)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%NMAS(1+NMN),NMN_L,1,
     .                            TAG_IRTL) !JD(22)


          IF (ILEV==10 .OR. ILEV==11 .OR. ILEV==12 .OR.ILEV==20 .OR. 
     .         ILEV==21 .OR. ILEV==22.OR.INTTH >0 ) THEN


            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%AREAS2,NSN_L,1,
     .                            TAG_NODE_2RY) !JD(31)
          ENDIF
          IF (ILEV==10 .OR. ILEV==11 .OR. ILEV==12) THEN

            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%SMAS,NSN_L,1,
     .                            TAG_NODE_2RY) !JD(23)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%SINER,NSN_L,1,
     .                            TAG_NODE_2RY) !JD(24)
c            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%AREAS2,NSN_L,1,
c     .                            TAG_NODE_2RY) !JD(31)

            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%UVAR,NSN_L,NUVAR,
     .                            TAG_NODE_2RY) !JD(32)

            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%XM0,NSN_L,3,
     .                            TAG_NODE_2RY) !JD(33)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%DSM,NSN_L,3,
     .                            TAG_NODE_2RY) !JD(34)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%FSM,NSN_L,3,
     .                            TAG_NODE_2RY) !JD(35)

          ELSEIF (ILEV==20 .OR. ILEV==21 .OR. ILEV==22) THEN

            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%SMAS,NSN_L,1,
     .                            TAG_NODE_2RY) !JD(23)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%SINER,NSN_L,1,
     .                            TAG_NODE_2RY) !JD(24)
c            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%AREAS2,NSN_L,1,
c     .                            TAG_NODE_2RY) !JD(31)

            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%UVAR,NSN_L,NUVAR,
     .                            TAG_NODE_2RY) !JD(32)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%XM0,NSN_L,3,
     .                            TAG_NODE_2RY) !JD(33)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%DSM,NSN_L,3,
     .                            TAG_NODE_2RY) !JD(34)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%FSM,NSN_L,3,
     .                            TAG_NODE_2RY) !JD(35)

            CALL COPY_RVAL(INTBUF_TAB(NI)%RUPT,6,1) !JD(36)

          ELSEIF (ILEV == 25) THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%SMAS,NSN_L,1,
     .                            TAG_NODE_2RY) !JD(23)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%SINER,NSN_L,1,
     .                            TAG_NODE_2RY) !JD(24)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%SPENALTY,NSN_L,1,
     .                            TAG_NODE_2RY) !JD(25)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFR_PENALTY,NSN_L,1,
     .                            TAG_NODE_2RY) !JD(26)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%SKEW,NSN_L,9,
     .                            TAG_NODE_2RY) !JD(33)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%DSM,NSN_L,3,
     .                            TAG_NODE_2RY) !JD(34)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%FSM,NSN_L,3,
     .                            TAG_NODE_2RY) !JD(35)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%FINI,NSN_L,3,
     .                            TAG_NODE_2RY) !JD(36)
          ELSEIF (ILEV == 26) THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%SMAS,NSN_L,1,
     .                            TAG_NODE_2RY) !JD(23)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%SINER,NSN_L,1,
     .                            TAG_NODE_2RY) !JD(24)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%SPENALTY,NSN_L,1,
     .                            TAG_NODE_2RY) !JD(25)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFR_PENALTY,NSN_L,1,
     .                            TAG_NODE_2RY) !JD(26)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%SKEW,NSN_L,9,
     .                            TAG_NODE_2RY) !JD(33)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%DSM,NSN_L,12,
     .                            TAG_NODE_2RY) !JD(34)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%FSM,NSN_L,12,
     .                            TAG_NODE_2RY) !JD(35)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%FINI,NSN_L,24,
     .                            TAG_NODE_2RY) !JD(36)
          ELSEIF (ILEV == 27) THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%SMAS,NSN_L,1,
     .                            TAG_NODE_2RY) !JD(23)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%SINER,NSN_L,1,
     .                            TAG_NODE_2RY) !JD(24)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%SPENALTY,NSN_L,1,
     .                            TAG_NODE_2RY) !JD(25)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFR_PENALTY,NSN_L,1,
     .                            TAG_NODE_2RY) !JD(26)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%SKEW,NSN_L,9,
     .                            TAG_NODE_2RY) !JD(33)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%DSM,NSN_L,3,
     .                            TAG_NODE_2RY) !JD(34)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%FSM,NSN_L,3,
     .                            TAG_NODE_2RY) !JD(35)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%FINI,NSN_L,6,
     .                            TAG_NODE_2RY) !JD(36)

          ELSEIF (ILEV == 28) THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%SMAS,NSN_L,1,
     .                            TAG_NODE_2RY) !JD(23)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%SINER,NSN_L,1,
     .                            TAG_NODE_2RY) !JD(24)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%SPENALTY,NSN_L,1,
     .                            TAG_NODE_2RY) !JD(25)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFR_PENALTY,NSN_L,1,
     .                            TAG_NODE_2RY) !JD(26)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%SKEW,NSN_L,9,
     .                            TAG_NODE_2RY) !JD(33)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%DSM,NSN_L,3,
     .                            TAG_NODE_2RY) !JD(34)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%FSM,NSN_L,3,
     .                            TAG_NODE_2RY) !JD(35)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%FINI,NSN_L,6,
     .                            TAG_NODE_2RY) !JD(36)

          ELSEIF(IPARI(17,NI).NE.0)THEN
C si Idel2 actif
C smas & siner (ZERO)   

            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%SMAS,NSN_L,1,
     .                            TAG_NODE_2RY) !JD(23)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%SINER,NSN_L,1,
     .                            TAG_NODE_2RY) !JD(24)

          ENDIF

          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%CSTS_BIS,NSN_L,2,TAG_NODE_2RY)

          DEALLOCATE(TAG_NODE_2RY,TAG_SEGM,TAG_SEGM2,TAG,TAG_IRTL)

c=====================================================================
        ELSEIF(ITYP == 3)THEN
c=====================================================================

          IF(PROC.EQ.0) THEN
            !split and write on disk integers arrays
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRECTS,NRTS,4) !KD(10) 
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRECTM,NRTM,4) !KD(11)
            CALL SPLIT_NODE_NODLOC_P0(INTBUF_TAB(NI)%NSV,NSN,1,NODLOCAL) !KD(12)
            CALL SPLIT_NODE_NODLOC_P0(INTBUF_TAB(NI)%MSR,NMN,1,NODLOCAL) !KD(13)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLM,NSN,1)   !KD(14)
            CALL COPY_IVAL(INTBUF_TAB(NI)%ILOCS,NSN,1)   !KD(16)
            CALL COPY_IVAL(INTBUF_TAB(NI)%NSEGM,1+NMN,1) !KD(21)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLS,NMN,1)   !KD(15)
            CALL COPY_IVAL(INTBUF_TAB(NI)%ILOCM,NMN,1)   !KD(17)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLOM,NSN,1)   !KD(18)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLOS,NMN,1)   !KD(19)
            CALL COPY_IVAL(INTBUF_TAB(NI)%NSEGS,1+NSN,1) !KD(20)
            CALL COPY_IVAL(INTBUF_TAB(NI)%LNSV,NST,1) !KD(22)
            CALL COPY_IVAL(INTBUF_TAB(NI)%LMSR,NMT,1) !KD(23)

            !split and write on disk floating arrays
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFAC,1,1)  
            CALL COPY_RVAL(INTBUF_TAB(NI)%VARIABLES,NPARIR,1) 
            CALL COPY_RVAL(INTBUF_TAB(NI)%CSTS,NSN,2) !JD(12)
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFNS,NSN,1) !JD(14)
            IF (MFROT.NE.0)THEN
              CALL COPY_RVAL(INTBUF_TAB(NI)%FRIC_P,10,1) !JD(25)
            ENDIF
            IF (IFQ.NE.0) THEN 
              CALL COPY_RVAL(INTBUF_TAB(NI)%S_XFILTR,1,1) !JD(26)
              CALL COPY_RVAL(INTBUF_TAB(NI)%S_FTSAV,NSN,3) !JD(27)
            ENDIF
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFM,NRTM,1) !JD(17)
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFS,NRTS,1) !JD(16)
            CALL COPY_RVAL(INTBUF_TAB(NI)%AREAS,NSN,1) 
            CALL COPY_RVAL(INTBUF_TAB(NI)%AREAM,NMN,1) 
            CALL COPY_RVAL(INTBUF_TAB(NI)%CSTM,NMN,2) !JD(13)
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFNM,NMN,1) !JD(15)
            CALL COPY_RVAL(INTBUF_TAB(NI)%FRICOS,NSN,3) !JD(18)
            CALL COPY_RVAL(INTBUF_TAB(NI)%FRICOM,NMN,3) !JD(19)
          ENDIF 

c=====================================================================
        ELSEIF(ITYP == 4)THEN
c=====================================================================
          IF(PROC.EQ.0) THEN
            !split and write on disk integers arrays
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRECTS,NRTS,4) !KD(10) 
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRECTM,NRTM,4) !KD(11)
            CALL SPLIT_NODE_NODLOC_P0(INTBUF_TAB(NI)%NSV,NSN,1,NODLOCAL) !KD(12)
            CALL SPLIT_NODE_NODLOC_P0(INTBUF_TAB(NI)%MSR,NMN,1,NODLOCAL) !KD(13)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLM,NSN,1)   !KD(14)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IELES,NRTS,1) !KD(24)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLS,NMN,1)   !KD(15)
            CALL COPY_IVAL(INTBUF_TAB(NI)%ILOCS,NSN,1)   !KD(16)
            CALL COPY_IVAL(INTBUF_TAB(NI)%ILOCM,NMN,1)   !KD(17)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLOM,NSN,1)   !KD(18)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLOS,NMN,1)   !KD(19)
            CALL COPY_IVAL(INTBUF_TAB(NI)%NSEGS,1+NSN,1) !KD(20)
            CALL COPY_IVAL(INTBUF_TAB(NI)%NSEGM,1+NMN,1) !KD(21)
            CALL COPY_IVAL(INTBUF_TAB(NI)%LNSV,NST,1) !KD(22)
            CALL COPY_IVAL(INTBUF_TAB(NI)%LMSR,NMT,1) !KD(23)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IELEM,NRTM,1) !KD(25)

            !split and write on disk floating arrays
            CALL COPY_RVAL(INTBUF_TAB(NI)%CSTS,NSN,2) !JD(12)
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFNS,NSN,1) !JD(14)
            CALL COPY_RVAL(INTBUF_TAB(NI)%CSTM,NMN,2) !JD(13)
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFNM,NMN,1) !JD(15)
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFS,NRTS,1) !JD(16)
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFM,NRTM,1) !JD(17)
            CALL COPY_RVAL(INTBUF_TAB(NI)%FRICOS,NSN,3) !JD(18)
            CALL COPY_RVAL(INTBUF_TAB(NI)%FRICOM,NMN,3) !JD(19)
            IF (MFROT.NE.0)THEN
              CALL COPY_RVAL(INTBUF_TAB(NI)%FRIC_P,10,1) !JD(25)
            ENDIF
            IF (IFQ.NE.0) THEN 
              CALL COPY_RVAL(INTBUF_TAB(NI)%S_XFILTR,1,1) !JD(26)
              CALL COPY_RVAL(INTBUF_TAB(NI)%S_FTSAV,NSN,3) !JD(27)
            ENDIF
          ENDIF 

c=====================================================================
        ELSEIF(ITYP == 5)THEN
c=====================================================================

          IF(PROC.EQ.0) THEN
            !split and write on disk integers arrays 
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRECTS,NRTS,4) !KD(10) 
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRECTM,NRTM,4) !KD(11)
            CALL SPLIT_NODE_NODLOC_P0(INTBUF_TAB(NI)%NSV,NSN,1,NODLOCAL) !KD(12)
            CALL SPLIT_NODE_NODLOC_P0(INTBUF_TAB(NI)%MSR,NMN,1,NODLOCAL) !KD(13)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLM,NSN,1)   !KD(14)
            CALL COPY_IVAL(INTBUF_TAB(NI)%ILOCS,NSN,1)   !KD(16)
            CALL COPY_IVAL(INTBUF_TAB(NI)%NSEGM,1+NMN,1) !KD(21)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLS,NMN,1)   !KD(15)
            CALL COPY_IVAL(INTBUF_TAB(NI)%ILOCM,NMN,1)   !KD(17)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLOM,NSN,1)  !KD(18)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLOS,NMN,1)  !KD(19)
            CALL COPY_IVAL(INTBUF_TAB(NI)%NSEGS,1+NSN,1) !KD(20)
            CALL COPY_IVAL(INTBUF_TAB(NI)%LNSV,NST,1) !KD(22)
            CALL COPY_IVAL(INTBUF_TAB(NI)%LMSR,NMT,1) !KD(23)

            !split and write on disk floating arrays
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFAC,1,1)  
            CALL COPY_RVAL(INTBUF_TAB(NI)%VARIABLES,NPARIR,1)
            CALL COPY_RVAL(INTBUF_TAB(NI)%CSTS,NSN,2) !JD(12)
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFNS,NSN,1) !JD(14)
            IF (MFROT.NE.0)THEN
              CALL COPY_RVAL(INTBUF_TAB(NI)%FRIC_P,10,1) !JD(25)
            ENDIF
            IF (IFQ.NE.0) THEN 
              CALL COPY_RVAL(INTBUF_TAB(NI)%S_XFILTR,1,1) !JD(26)
              CALL COPY_RVAL(INTBUF_TAB(NI)%S_FTSAV,NSN,3) !JD(27)
            ENDIF
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFM,NRTM,1) !JD(17)
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFS,NRTS,1) !JD(16)
            CALL COPY_RVAL(INTBUF_TAB(NI)%AREAS,NSN,1) 
            CALL COPY_RVAL(INTBUF_TAB(NI)%CSTM,NMN,2) !JD(13)
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFNM,NMN,1) !JD(15)
            CALL COPY_RVAL(INTBUF_TAB(NI)%FRICOS,NSN,3) !JD(18)
            CALL COPY_RVAL(INTBUF_TAB(NI)%FRICOM,NMN,3) !JD(19)
          ENDIF
 
c=====================================================================
        ELSEIF(ITYP == 6)THEN
c=====================================================================

          IF(PROC.EQ.0) THEN
            !split and write on disk integers arrays
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRECTS,NRTS,4) !KD(10) 
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRECTM,NRTM,4) !KD(11)
            CALL SPLIT_NODE_NODLOC_P0(INTBUF_TAB(NI)%NSV,NSN,1,NODLOCAL) !KD(12)
            CALL SPLIT_NODE_NODLOC_P0(INTBUF_TAB(NI)%MSR,NMN,1,NODLOCAL) !KD(13)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLM,NSN,1)   !KD(14)
            CALL COPY_IVAL(INTBUF_TAB(NI)%ILOCS,NSN,1)   !KD(16)
            CALL COPY_IVAL(INTBUF_TAB(NI)%NSEGM,1+NMN,1) !KD(21)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLS,NMN,1)   !KD(15)
            CALL COPY_IVAL(INTBUF_TAB(NI)%ILOCM,NMN,1)   !KD(17)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLOM,NSN,1)   !KD(18)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLOS,NMN,1)   !KD(19)
            CALL COPY_IVAL(INTBUF_TAB(NI)%NSEGS,1+NSN,1) !KD(20)
            CALL COPY_IVAL(INTBUF_TAB(NI)%LNSV,NST,1) !KD(22)
            CALL COPY_IVAL(INTBUF_TAB(NI)%LMSR,NMT,1) !KD(23)

            !split and write on disk floating arrays
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFAC,1,1)  
            CALL COPY_RVAL(INTBUF_TAB(NI)%VARIABLES,NPARIR,1) 
            CALL COPY_RVAL(INTBUF_TAB(NI)%CSTS,NSN,2) !JD(12)
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFNS,NSN,1) !JD(14)
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFM,NRTM,1) !JD(17)
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFS,NRTS,1) !JD(16)
            CALL COPY_RVAL(INTBUF_TAB(NI)%CSTM,NMN,2) !JD(13)
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFNM,NMN,1) !JD(15)
            CALL COPY_RVAL(INTBUF_TAB(NI)%FRICOS,NSN,3) !JD(18)
            CALL COPY_RVAL(INTBUF_TAB(NI)%FRICOM,NMN,3) !JD(19)
            CALL COPY_RVAL(INTBUF_TAB(NI)%FCONT,1,1) !JD(20)
            CALL COPY_RVAL(INTBUF_TAB(NI)%FS,NSN,3) !JD(21)
            CALL COPY_RVAL(INTBUF_TAB(NI)%FM,NMN,3) !JD(22)
            CALL COPY_RVAL(INTBUF_TAB(NI)%RMAS,2,1) !JD(23)
            CALL COPY_RVAL(INTBUF_TAB(NI)%S_ANSMX0,2,1) !JD(24)
          ENDIF 

c=====================================================================
        ELSEIF(ITYP == 7)THEN ! (ITYP == 18)
c=====================================================================
           ALLOCATE(TAG_NODE_2RY(NSN_FE_L),TAG_SEGM(NRTM_FE_L),
     .          TAG_NODE_MSR(NMN_FE_L),TAG_SEGM2(NRTM_FE),TAG_II(II_STOK))

           ALLOCATE(I710XSAV(NMN))
           I710XSAV(1:NMN)=ZERO
                         
           TAG_NODE_2RY(1:NSN_FE_L) = 0
           TAG_SEGM(1:NRTM_FE_L) = 0
           TAG_SEGM2(1:NRTM_FE) = 0
           TAG_NODE_MSR(1:NMN_FE_L) = 0
           TAG_II(1:II_STOK) = 0
           IF (MULTI_FVM%IS_USED .AND. INACTI.EQ.7) THEN
C     Interface type 18 for law151
!prepare addition arrays used for split
              CALL PREPARE_SPLIT_I7(PROC          ,INTBUF_TAB(NI),IPARI(1,NI) ,
     .             INTERCEP(1,NI),TAG_NODE_2RY,TAG_SEGM      ,
     .             TAG_SEGM2     ,TAG_NM        ,TAG_NODE_MSR,
     .             TAG_SCRATCH   ,NI, CEP, MULTI_FVM,I710XSAV,
     .             NINDX_NM      ,INDX_NM,NINDX_SCRT,INDX_SCRT,NODLOCAL,
     .             NUMNOD_L)
              IF(II_STOK.GT.0)THEN
                 CALL PREPARE_SPLIT_CAND(INTBUF_TAB(NI), TAG_SEGM2, II_STOK,
     .                TAG_II )
              ENDIF
c     split and write on disk integers arrays              
              CALL SPLIT_SEG_NODLOC(INTBUF_TAB(NI)%IRECTM,NRTM_FE_L,4,
     .             TAG_SEGM,NODLOCAL)
              CALL COPY_IVAL(INTBUF_TAB(NI)%IRECTM,NRTM_IGE,4)

              ! NSN -> brick ids
              CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%NSV,NSN_FE_L,
     .             TAG_NODE_2RY,CEL)

              CALL COPY_IVAL_IGEO(INTBUF_TAB(NI)%NSV,NSN_IGE,1,NSN_FE) 
              
              CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%MSR,NMN_FE_L,
     .             TAG_NODE_MSR,NODLOCAL)
              
              CALL COPY_IVAL_IGEO(INTBUF_TAB(NI)%MSR,NMN_IGE,1,NMN_FE) 
              
              IF(INTTH.GT.0)THEN
                 CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IELEC,NSN_FE_L,1,
     .                TAG_NODE_2RY) !KD(25)
                 CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IELES,NRTM_FE_L,1,
     .                TAG_SEGM) !KD(24)
              ENDIF
              
              IF(NISUB.GT.0)THEN
                 CALL COPY_IVAL(INTBUF_TAB(NI)%LISUB,NISUB,1) !KD(28)
                 CALL COPY_IVAL(INTBUF_TAB(NI)%TYPSUB,NISUB,1) 
                 CALL SPLIT_NISUB_I7(INTBUF_TAB(NI), NSN_FE_L, TAG_NODE_2RY, NRTM_FE_L,
     .                TAG_SEGM      , NISUBS_L, NISUBM_L)
              ENDIF
              
c     split candidates
!TAG_SCRATCH should have been reflush to zero
              CALL SPLIT_CAND_I7(PROC     , INTBUF_TAB(NI), NSN_FE   , NSN_FE_L    ,
     .             TAG_SEGM2, II_STOK       , MULTIMP  , NCONT       ,
     .             NOINT    , INACTI        , TAG_SCRATCH , 
     .             II_STOK_L, ITYP          ,NINDX_SCRT,INDX_SCRT,NODLOCAL,
     .             NUMNOD_L )
              
              CALL COPY_IVAL(II_STOK_L,1,1)
              
              IF (IFQ.GT.0)THEN
                 CALL SPLIT_CAND_IVAL(INTBUF_TAB(NI)%IFPEN,
     .                II_STOK_L,TAG_II,MULTIMP,NCONT) !KD(27)
              ENDIF
              
              IF(FLAGREMNODE == 2) THEN  
                 CALL SPLIT_REMNODE_I7(PROC     ,INTBUF_TAB(NI), NRTM_FE, NRTM_FE_L,
     .                TAG_SEGM2,NREMNODE_L      , NODLOCAL, ITAB,NUMNOD_L)
              ENDIF
              
              SIZ = INTBUF_TAB_L(NI)%S_NIGE
              CALL COPY_IVAL(INTBUF_TAB(NI)%NIGE,SIZ,1) 
              
c     split and write on disk floating arrays 
              CALL COPY_RVAL(INTBUF_TAB(NI)%STFAC,1,1) !JD(10)
              CALL COPY_RVAL(INTBUF_TAB(NI)%VARIABLES,NPARIR,1) !JD(11)   
              
              CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFNS,NSN_FE_L,1,TAG_NODE_2RY)
              CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFM,NRTM_FE_L,1,TAG_SEGM)
              
              IF(IGAP.NE.0)THEN
                 CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_M,NRTM_FE_L,1,TAG_SEGM) !JD(15)
                 CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_S,NSN_FE_L,1,TAG_NODE_2RY) !JD(16)
              ENDIF
              
              SIZ = INTBUF_TAB_L(NI)%S_XSAV
              INTBUF_TAB(NI)%XSAV(1:SIZ) = 0
              CALL COPY_RVAL(INTBUF_TAB(NI)%XSAV,SIZ,1)
              
              SIZ = INTBUF_TAB_L(NI)%S_CRIT
              CALL COPY_RVAL(INTBUF_TAB(NI)%CRIT,SIZ,1)
              
              IF (MFROT.NE.0) THEN
                 CALL COPY_RVAL(INTBUF_TAB(NI)%FRIC_P,10,1)
              ENDIF
              
              IF (IFQ.NE.0) THEN
                 CALL COPY_RVAL(INTBUF_TAB(NI)%XFILTR,1,1)
              ENDIF
              
              IF(INTTH.GT.0) THEN
                 CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%AREAS,NSN_FE_L,1,TAG_NODE_2RY) !JD(30)
              ENDIF
              
              IF(IGAP==3)THEN
                 CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_ML,NRTM_FE_L,1,TAG_SEGM) !JD(31)
                 CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_SL,NSN_FE_L,1,TAG_NODE_2RY) !JD(32)  
              ENDIF
              
c     
              IF(INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7)THEN
                 CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%CAND_P,
     .                II_STOK_L,TAG_II,MULTIMP,NCONT)
              ENDIF

              IF(IFQ.NE.0)THEN        
                 CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%FTSAVX,
     .                II_STOK_L,TAG_II,MULTIMP,NCONT)
                 CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%FTSAVY,
     .                II_STOK_L,TAG_II,MULTIMP,NCONT)
                 CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%FTSAVZ,
     .                II_STOK_L,TAG_II,MULTIMP,NCONT)
              ENDIF
              
              SIZ = INTBUF_TAB_L(NI)%S_RIGE
              CALL COPY_RVAL(INTBUF_TAB(NI)%RIGE,SIZ,1)  
              SIZ = INTBUF_TAB_L(NI)%S_XIGE
              CALL COPY_RVAL(INTBUF_TAB(NI)%XIGE,SIZ,1)  
              SIZ = INTBUF_TAB_L(NI)%S_VIGE
              CALL COPY_RVAL(INTBUF_TAB(NI)%VIGE,SIZ,1)
              SIZ = INTBUF_TAB_L(NI)%S_MASSIGE
              CALL COPY_RVAL(INTBUF_TAB(NI)%MASSIGE,SIZ,1)  
              
              SIZ = INTBUF_TAB_L(NI)%S_CAND_F
              CALL COPY_RVAL(INTBUF_TAB(NI)%CAND_F,SIZ,1)
           ELSE 
!prepare addition arrays used for split
              CALL PREPARE_SPLIT_I7(PROC          ,INTBUF_TAB(NI),IPARI(1,NI) ,
     .             INTERCEP(1,NI),TAG_NODE_2RY,TAG_SEGM      ,
     .             TAG_SEGM2     ,TAG_NM        ,TAG_NODE_MSR,
     .             TAG_SCRATCH   ,NI, CEP, MULTI_FVM,I710XSAV,
     .             NINDX_NM      ,INDX_NM,NINDX_SCRT,INDX_SCRT,NODLOCAL,
     .             NUMNOD_L)
              
              IF(II_STOK.GT.0)THEN
                 CALL PREPARE_SPLIT_CAND(INTBUF_TAB(NI), TAG_SEGM2, II_STOK,
     .                TAG_II )
              ENDIF
              
c     split and write on disk integers arrays
              
              CALL SPLIT_SEG_NODLOC(INTBUF_TAB(NI)%IRECTM,NRTM_FE_L,4,
     .             TAG_SEGM,NODLOCAL)
              CALL COPY_IVAL(INTBUF_TAB(NI)%IRECTM,NRTM_IGE,4)
              
              CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%NSV,NSN_FE_L,
     .             TAG_NODE_2RY,NODLOCAL)
              
              CALL COPY_IVAL_IGEO(INTBUF_TAB(NI)%NSV,NSN_IGE,1,NSN_FE) 
              
              CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%MSR,NMN_FE_L,
     .             TAG_NODE_MSR,NODLOCAL)
              
              CALL COPY_IVAL_IGEO(INTBUF_TAB(NI)%MSR,NMN_IGE,1,NMN_FE) 
              
              IF(INTTH.GT.0)THEN
                 CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IELEC,NSN_FE_L,1,
     .                TAG_NODE_2RY) !KD(25)
                 CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IELES,NRTM_FE_L,1,
     .                TAG_SEGM) !KD(24)
              ENDIF

              IF(NISUB.GT.0)THEN
                 CALL COPY_IVAL(INTBUF_TAB(NI)%LISUB,NISUB,1) !KD(28)
                 CALL COPY_IVAL(INTBUF_TAB(NI)%TYPSUB,NISUB,1) 
                 CALL SPLIT_NISUB_I7(INTBUF_TAB(NI), NSN_FE_L, TAG_NODE_2RY, NRTM_FE_L,
     .                TAG_SEGM      , NISUBS_L, NISUBM_L)
              ENDIF
              
c     split candidates
!TAG_SCRATCH should has been reflush to zero
              CALL SPLIT_CAND_I7(PROC     , INTBUF_TAB(NI), NSN_FE   , NSN_FE_L    ,
     .             TAG_SEGM2, II_STOK       , MULTIMP  , NCONT       ,
     .             NOINT    , INACTI        , TAG_SCRATCH , 
     .             II_STOK_L, ITYP          ,NINDX_SCRT,INDX_SCRT,NODLOCAL,
     .             NUMNOD_L )
              
              CALL COPY_IVAL(II_STOK_L,1,1)
              
              IF (IFQ.GT.0)THEN
                 CALL SPLIT_CAND_IVAL(INTBUF_TAB(NI)%IFPEN,
     .                II_STOK_L,TAG_II,MULTIMP,NCONT) !KD(27)
              ENDIF
    
              IF(INTFRIC.GT.0)THEN
                 CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IPARTFRICS,NSN_FE_L,1,
     .                TAG_NODE_2RY) 
                 CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IPARTFRICM,NRTM_FE_L,1,
     .                TAG_SEGM) 
                 IF(INTBUF_FRIC_TAB(INTFRIC)%IORTHFRIC > 0) THEN
                    CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IREP_FRICM,NRTM_FE_L,1,
     .                   TAG_SEGM)

                 ENDIF
              ENDIF
              
              IF(FLAGREMNODE == 2) THEN  
                 CALL SPLIT_REMNODE_I7(PROC     ,INTBUF_TAB(NI), NRTM_FE, NRTM_FE_L,
     .                TAG_SEGM2,NREMNODE_L      , NODLOCAL, ITAB,NUMNOD_L)
              ENDIF
              
              SIZ = INTBUF_TAB_L(NI)%S_NIGE
              CALL COPY_IVAL(INTBUF_TAB(NI)%NIGE,SIZ,1) 
              
c     split and write on disk floating arrays 
              CALL COPY_RVAL(INTBUF_TAB(NI)%STFAC,1,1) !JD(10)
              CALL COPY_RVAL(INTBUF_TAB(NI)%VARIABLES,NPARIR,1) !JD(11)   
              
              CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFNS,NSN_FE_L,1,TAG_NODE_2RY)
              CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFM,NRTM_FE_L,1,TAG_SEGM)
              
              IF(IGAP.NE.0)THEN
                 CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_M,NRTM_FE_L,1,TAG_SEGM) !JD(15)
                 CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_S,NSN_FE_L,1,TAG_NODE_2RY) !JD(16)
              ENDIF
              
              SIZ = INTBUF_TAB_L(NI)%S_XSAV
              INTBUF_TAB(NI)%XSAV(1:SIZ) = 0
              CALL COPY_RVAL(INTBUF_TAB(NI)%XSAV,SIZ,1)
              
              SIZ = INTBUF_TAB_L(NI)%S_CRIT
              CALL COPY_RVAL(INTBUF_TAB(NI)%CRIT,SIZ,1)
              
              IF (MFROT.NE.0) THEN
                 CALL COPY_RVAL(INTBUF_TAB(NI)%FRIC_P,10,1)
              ENDIF
              
              IF (IFQ.NE.0) THEN
                 CALL COPY_RVAL(INTBUF_TAB(NI)%XFILTR,1,1)
              ENDIF
              
              IF(INTTH.GT.0) THEN
                 CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%AREAS,NSN_FE_L,1,TAG_NODE_2RY) !JD(30)
              ENDIF
              
              IF(IGAP==3)THEN
                 CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_ML,NRTM_FE_L,1,TAG_SEGM) !JD(31)
                 CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_SL,NSN_FE_L,1,TAG_NODE_2RY) !JD(32)  
              ENDIF
              
c     
              IF(INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7)THEN
                 CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%CAND_P,
     .                II_STOK_L,TAG_II,MULTIMP,NCONT)
              ENDIF
              
              IF(IFQ.NE.0)THEN        
                 CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%FTSAVX,
     .                II_STOK_L,TAG_II,MULTIMP,NCONT)
                 CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%FTSAVY,
     .                II_STOK_L,TAG_II,MULTIMP,NCONT)
                 CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%FTSAVZ,
     .                II_STOK_L,TAG_II,MULTIMP,NCONT)
              ENDIF
              
              SIZ = INTBUF_TAB_L(NI)%S_RIGE
              CALL COPY_RVAL(INTBUF_TAB(NI)%RIGE,SIZ,1)  
              SIZ = INTBUF_TAB_L(NI)%S_XIGE
              CALL COPY_RVAL(INTBUF_TAB(NI)%XIGE,SIZ,1)  
              SIZ = INTBUF_TAB_L(NI)%S_VIGE
              CALL COPY_RVAL(INTBUF_TAB(NI)%VIGE,SIZ,1)
              SIZ = INTBUF_TAB_L(NI)%S_MASSIGE
              CALL COPY_RVAL(INTBUF_TAB(NI)%MASSIGE,SIZ,1)  
              
              SIZ = INTBUF_TAB_L(NI)%S_CAND_F
              CALL COPY_RVAL(INTBUF_TAB(NI)%CAND_F,SIZ,1)

              IF(INTFRIC.GT.0)THEN
                 IF(INTBUF_FRIC_TAB(INTFRIC)%IORTHFRIC > 0) THEN
                    CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%DIR_FRICM,NRTM_FE_L,2,
     .                   TAG_SEGM)
                 ENDIF
              ENDIF

           ENDIF                ! NOT LAW151
           DEALLOCATE(TAG_NODE_2RY,TAG_SEGM,TAG_SEGM2,
     .          TAG_NODE_MSR,TAG_II)
           DEALLOCATE(I710XSAV)
c=====================================================================
        ELSEIF(ITYP == 8)THEN
c=====================================================================
          NBT8 = NBT8 + 1
          ALLOCATE(TAG_NODE_2RY(NSN),TAG_SEGM(NRTM_L),TAG_NODE_MSR2(NMN),
     .             TAG_NODE_MSR(NMN_L),TAG_SEGM2(NRTM),TAG_II(II_STOK),
     .             TAG_LMSR(NMT_L), TAG_NSEG(NMN_L+1), 
     .             TAG_LMSR2(NMT), TAG_NSEG2(NMN+1))

          TAG_NODE_2RY(1:NSN) = 0
          TAG_SEGM(1:NRTM_L) = 0
          TAG_SEGM2(1:NRTM) = 0
          TAG_NODE_MSR(1:NMN_L) = 0
          TAG_NODE_MSR2(1:NMN) = 0
          TAG_LMSR(1:NMT_L) = 0
          TAG_LMSR2(1:NMT) = 0
          TAG_II(1:II_STOK) = 0
          TAG_NSEG2(1:NMN+1) = 0
          TAG_NSEG(1:NMN_L+1) = 0
 
          !prepare addition arrays used for split
          CALL PREPARE_SPLIT_I8(PROC          ,INTBUF_TAB(NI),IPARI(1,NI) ,
     .                          INTERCEP(1,NI),TAG_NODE_2RY,TAG_SEGM      ,
     .                          TAG_SEGM2     ,TAG_NM        ,TAG_NODE_MSR,
     .                  TAG_NODE_MSR2         ,TAG_LMSR      ,TAG_LMSR2, 
     .                  TAG_NSEG              ,TAG_NSEG2, 
     .                  NI, T8(PROC+1,NBT8),ITAB,NINDX_NM,INDX_NM)

!         IF(PROC.EQ.0) THEN
            !split and write on disk integers arrays
!           CALL COPY_IVAL(INTBUF_TAB(NI)%IRECTS,NRTS,4) !KD(10) 



!           CALL COPY_IVAL(INTBUF_TAB(NI)%IRECTM,NRTM,4) !KD(11)
            CALL SPLIT_SEG_NODLOC(INTBUF_TAB(NI)%IRECTM,NRTM_L,4,
     .                            TAG_SEGM,TAG_NODE_MSR2)

            CALL COPY_NODE_NODLOC(INTBUF_TAB(NI)%NSV,NSN,NODLOCAL)

            CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%MSR,NMN_L,
     .                        TAG_NODE_MSR,NODLOCAL) !KD(13)


C           CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLM,NSN,1)   !KD(14)
            CALL FILTER_NODE_NODLOC(INTBUF_TAB(NI)%IRTLM,NSN,TAG_NODE_2RY,
     .                              TAG_SEGM2) 

            CALL COPY_IVAL(II_STOK,1,1)
         

!           CALL COPY_IVAL(INTBUF_TAB(NI)%ILOCS,NSN,1)   !KD(16)
!           INTBUF_TAB(NI)%ILOCS = -1
            CALL FILTER_NODE_NODLOC(INTBUF_TAB(NI)%ILOCS,NSN,TAG_NODE_2RY,
     .                              TAG_NODE_MSR2) 



C           CALL COPY_IVAL(INTBUF_TAB(NI)%NSEGM,1+NMN,1) !KD(21)

c           CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%NSEGM,NMN_L,
c    .           TAG_NODE_MSR,TAG_LMSR2)
            CALL WRITE_I_C(TAG_NSEG,NMN_L+1)

            CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLOM,NSN,1) !KD(18)


c           CALL FILTER_NODE_NODLOC(INTBUF_TAB(NI)%IRTLOM,NSN,TAG_NODE_2RY,
c    .                              TAG_SEGM2) 

            CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%LMSR,NMT_L,
     .           TAG_LMSR,TAG_SEGM2)

            !split and write on disk floating arrays
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFAC,1,1)  

            CALL COPY_RVAL(INTBUF_TAB(NI)%VARIABLES,NPARIR,1) 
            CALL COPY_RVAL(INTBUF_TAB(NI)%CSTS,NSN,2) !JD(12)

            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFM,NRTM_L,1,TAG_SEGM)

!           CALL COPY_RVAL(INTBUF_TAB(NI)%STFS,NRTS,1) !JD(16)
            IF(IFORM==2)THEN
              CALL COPY_RVAL(INTBUF_TAB(NI)%FTSAVX,NSN,1) !JD(28)
              CALL COPY_RVAL(INTBUF_TAB(NI)%FTSAVX,NSN,1) !JD(29)
              CALL COPY_RVAL(INTBUF_TAB(NI)%FTSAVX,NSN,1) !JD(30)
            ENDIF
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFNM,NMN_L,1,TAG_NODE_MSR)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAPN,NRTM_L,1,TAG_SEGM)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STF8,NRTM_L,1,TAG_SEGM)


c           CALL COPY_RVAL(INTBUF_TAB(NI)%STF8,NSN,1) !JD(32)

            IF(NSPMD > 1) CALL W_TYPE8(PROC,T8(PROC+1,NBT8))

!         ENDIF 
          DEALLOCATE(TAG_NODE_2RY,TAG_SEGM,TAG_NODE_MSR2,
     .             TAG_NODE_MSR,TAG_SEGM2,TAG_II,
     .             TAG_NSEG2,TAG_NSEG, TAG_LMSR2,TAG_LMSR)

c=====================================================================
        ELSEIF(ITYP == 9)THEN
c=====================================================================
          ALLOCATE(TAG_NODE_2RY(NSN_L), TAG_NODE_MSR(NMN_L),
     .             TAG_IELES(NRTS_L)  , TAG_IELEM(NRTM_L) )

          TAG_NODE_2RY(1:NSN_L) = 0
          TAG_NODE_MSR(1:NMN_L) = 0
          TAG_IELES(1:NRTS_L) = 0
          TAG_IELEM(1:NRTM_L) = 0

          !prepare addition arrays used for split
          CALL PREPARE_SPLIT_I9(PROC          ,INTBUF_TAB(NI),IPARI(1,NI) ,
     .                          TAG_NODE_2RY  ,TAG_NODE_MSR  ,TAG_SCRATCH ,
     .                          TAG_IELES     ,TAG_IELEM     ,         
     .                          CEP           ,CEL           ,NINDX_SCRT  ,INDX_SCRT)
          !split and write on disk integers arrays
          CALL COPY_IVAL(INTBUF_TAB(NI)%IRECTS,NRTS,4) !KD(10) 
          CALL COPY_IVAL(INTBUF_TAB(NI)%IRECTM,NRTM,4) !KD(11)

          CALL COPY_NODE_NODLOC(INTBUF_TAB(NI)%NSV,NSN_L,NODLOCAL)

          CALL COPY_NODE_NODLOC(INTBUF_TAB(NI)%MSR,NMN_L,NODLOCAL)

          CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLM,NSN,1)   !KD(14)
 
cc          CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IELES,NRTS_L,1,
cc     .                          TAG_IELES) !KD(24)
          CALL COPY_IVAL(TAG_IELES,NRTS_L,1)   !KD(24)

          CALL COPY_IVAL(INTBUF_TAB(NI)%ILOCS,NSN,1)   !KD(16)
          CALL COPY_IVAL(INTBUF_TAB(NI)%NSEGM,1+NMN,1) !KD(21)
          CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLS,NMN,1)   !KD(15)
          CALL COPY_IVAL(INTBUF_TAB(NI)%ILOCM,NMN,1)   !KD(17)
          CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLOM,NSN,1)   !KD(18)
          CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLOS,NMN,1)   !KD(19)
          CALL COPY_IVAL(INTBUF_TAB(NI)%NSEGS,1+NSN,1) !KD(20)
          CALL COPY_IVAL(INTBUF_TAB(NI)%LNSV,NST,1) !KD(22)
          CALL COPY_IVAL(INTBUF_TAB(NI)%LMSR,NMT,1) !KD(23)
          CALL COPY_IVAL(TAG_IELEM,NRTM_L,1)   !KD(25)

          !split and write on disk floating arrays
          CALL COPY_RVAL(INTBUF_TAB(NI)%STFAC,1,1)  
          CALL COPY_RVAL(INTBUF_TAB(NI)%VARIABLES,NPARIR,1) 
          CALL COPY_RVAL(INTBUF_TAB(NI)%CSTS,NSN,2) !JD(12)

          CALL COPY_RVAL(INTBUF_TAB(NI)%STFNS,NSN,1) !JD(14)
          CALL COPY_RVAL(INTBUF_TAB(NI)%STFM,NRTM,2) !JD(17)
          CALL COPY_RVAL(INTBUF_TAB(NI)%STFS,NRTS,1) !JD(16)
          CALL COPY_RVAL(INTBUF_TAB(NI)%N,NSN,3)     !JD(20) 
          CALL COPY_RVAL(INTBUF_TAB(NI)%CSTM,NMN,2)  !JD(13)
          CALL COPY_RVAL(INTBUF_TAB(NI)%STFNM,NMN,1) !JD(15)
          CALL COPY_RVAL(INTBUF_TAB(NI)%FRICOS,NSN,3)!JD(18)
          CALL COPY_RVAL(INTBUF_TAB(NI)%FRICOM,NMN,3) !JD(19)
          IF (MFROT.NE.0)THEN
            CALL COPY_RVAL(INTBUF_TAB(NI)%FRIC_P,10,1) !JD(25)
          ENDIF
          IF (IFQ.NE.0) THEN 
            CALL COPY_RVAL(INTBUF_TAB(NI)%S_XFILTR,1,1) !JD(26)
            CALL COPY_RVAL(INTBUF_TAB(NI)%S_FTSAV,NSN,3) !JD(27)
          ENDIF

          DEALLOCATE(TAG_NODE_2RY, TAG_NODE_MSR, TAG_IELES, TAG_IELEM)

c=====================================================================
        ELSEIF(ITYP == 10)THEN
c=====================================================================

          ALLOCATE(TAG_NODE_2RY(NSN_L),TAG_SEGM(NRTM_L),
     .             TAG_NODE_MSR(NMN_L),TAG_SEGM2(NRTM),TAG_II(II_STOK))

          ALLOCATE(I710XSAV(NMN))

          TAG_NODE_2RY(1:NSN_L) = 0
          TAG_SEGM(1:NRTM_L) = 0
          TAG_SEGM2(1:NRTM) = 0
          TAG_NODE_MSR(1:NMN_L) = 0
          TAG_II(1:II_STOK) = 0

          !prepare addition arrays used for split
          CALL PREPARE_SPLIT_I7(PROC          ,INTBUF_TAB(NI),IPARI(1,NI) ,
     .                          INTERCEP(1,NI),TAG_NODE_2RY,TAG_SEGM      ,
     .                          TAG_SEGM2     ,TAG_NM        ,TAG_NODE_MSR,
     .                          TAG_SCRATCH   ,NI, CEP, MULTI_FVM,I710XSAV,
     .                          NINDX_NM      ,INDX_NM,NINDX_SCRT,INDX_SCRT,NODLOCAL,
     .                          NUMNOD_L)
  
          IF(II_STOK.GT.0)THEN
            CALL PREPARE_SPLIT_CAND(INTBUF_TAB(NI), TAG_SEGM2, II_STOK,
     .                               TAG_II)
          ENDIF

c split and write on disk integers arrays

          CALL SPLIT_SEG_NODLOC(INTBUF_TAB(NI)%IRECTM,NRTM_L,4,
     .                          TAG_SEGM,NODLOCAL)
          CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%NSV,NSN_L,
     .                          TAG_NODE_2RY,NODLOCAL)
          CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%MSR,NMN_L,
     .                          TAG_NODE_MSR,NODLOCAL)


          IF(NISUB.GT.0)THEN
            CALL COPY_IVAL(INTBUF_TAB(NI)%LISUB,NISUB,1) !KD(28)
            CALL SPLIT_NISUB_I7(INTBUF_TAB(NI), NSN_L   , TAG_NODE_2RY, NRTM_L,
     .                          TAG_SEGM      , NISUBS_L, NISUBM_L)
          ENDIF

c split candidates
          !TAG_SCRATCH should has been reflush to zero
          CALL SPLIT_CAND_I7(PROC     , INTBUF_TAB(NI), NSN      , NSN_L       ,
     .                       TAG_SEGM2, II_STOK       , MULTIMP  , NCONT       ,
     .                       NOINT    , INACTI        , TAG_SCRATCH , 
     .                       II_STOK_L, ITYP          ,NINDX_SCRT,INDX_SCRT,NODLOCAL,
     .                       NUMNOD_L )

          CALL COPY_IVAL(II_STOK_L,1,1)

c split and write on disk floating arrays 

          CALL COPY_RVAL(INTBUF_TAB(NI)%STFAC,1,1)  
          CALL COPY_RVAL(INTBUF_TAB(NI)%VARIABLES,NPARIR,1) 

          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFNS,NSN_L,1,TAG_NODE_2RY)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFM,NRTM_L,1,TAG_SEGM)

          IF(IGAP.NE.0)THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_M,NRTM_L,1,TAG_SEGM)!JD(15)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_S,NSN_L,1,TAG_NODE_2RY)!JD(16)
          ELSE
            SIZ = INTBUF_TAB_L(NI)%S_GAP_M
            CALL COPY_RVAL(INTBUF_TAB(NI)%GAP_M,SIZ,1)

            SIZ = INTBUF_TAB_L(NI)%S_GAP_S
            CALL COPY_RVAL(INTBUF_TAB(NI)%GAP_S,SIZ,1)
          ENDIF

          SIZ = INTBUF_TAB_L(NI)%S_XSAV
          INTBUF_TAB(NI)%XSAV(1:SIZ) = 0
          CALL COPY_RVAL(INTBUF_TAB(NI)%XSAV,SIZ,1)

          SIZ = INTBUF_TAB_L(NI)%S_CRIT
          CALL COPY_RVAL(INTBUF_TAB(NI)%CRIT,SIZ,1)

          IF(INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7)THEN
            CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%CAND_F,
     .                               II_STOK_L,TAG_II,MULTIMP,NCONT)
          ELSE
            SIZ = INTBUF_TAB_L(NI)%S_CAND_F
            CALL COPY_RVAL(INTBUF_TAB(NI)%CAND_F,SIZ,1)
          ENDIF

          DEALLOCATE(TAG_NODE_2RY,TAG_SEGM,TAG_SEGM2,
     .              TAG_NODE_MSR,TAG_II)
          DEALLOCATE(I710XSAV)
      
c=====================================================================
        ELSEIF(ITYP == 11)THEN
c=====================================================================
          ALLOCATE(TAG_NODE_2RY(NSN_L),TAG_SEGM(NRTM_L),
     .               TAG_SEGM2(NRTM),TAG_SEGS(NRTS_L),
     .               TAG_II(II_STOK),TAG_NODE_MSR(NMN_L),TAG_SEG2S(NRTS))

          TAG_NODE_2RY(1:NSN_L) = 0
          TAG_SEGM(1:NRTM_L) = 0
          TAG_SEGM2(1:NRTM) = 0
          TAG_SEGS(1:NRTS_L) = 0
          TAG_SEG2S(1:NRTS) = 0
          TAG_II(1:II_STOK) = 0
          TAG_NODE_MSR(1:NMN_L) = 0

          !prepare addition arrays used for split
          CALL PREPARE_SPLIT_I11(PROC        , INTBUF_TAB(NI), IPARI(1,NI) ,
     .                           TAG_NODE_2RY, TAG_SEGM      , TAG_SEGM2   ,
     .                           TAG_NM      , TAG_SEGS      , TAG_NODE_MSR,
     .                           TAG_SCRATCH , INTERCEP      , NI,NINDX_NM,INDX_NM,
     .                           NINDX_SCRT  ,INDX_SCRT      ,TAG_SEG2S)

          IF(II_STOK.GT.0)THEN
            CALL PREPARE_SPLIT_CAND(INTBUF_TAB(NI), TAG_SEGM2, II_STOK, 
     .                               TAG_II)
          ENDIF

c split and write on disk integers arrays 
          CALL SPLIT_SEG_NODLOC(INTBUF_TAB(NI)%IRECTS,NRTS_L,2,
     .                          TAG_SEGS,NODLOCAL) !KD(10)
          CALL SPLIT_SEG_NODLOC(INTBUF_TAB(NI)%IRECTM,NRTM_L,2,
     .                          TAG_SEGM,NODLOCAL) !KD(11)
          CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%NSV,NSN_L,
     .                          TAG_NODE_2RY,NODLOCAL) !KD(12)
          CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%MSR,NMN_L,
     .                          TAG_NODE_MSR,NODLOCAL) !KD(13)
c
         IF(INTTH.GT.0) THEN
          CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IELEC,NRTS_L,1,
     .                          TAG_SEGS) !KD(10)
          CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IELES,NRTM_L,1,
     .                          TAG_SEGM) !KD(10)
          ENDIF

C- subinterfaces
          IF(NISUB.GT.0)THEN
            CALL COPY_IVAL(INTBUF_TAB(NI)%LISUB,NISUB,1) !KD(28)
            CALL COPY_IVAL(INTBUF_TAB(NI)%TYPSUB,NISUB,1) 

            CALL SPLIT_NISUB_I7(INTBUF_TAB(NI), NRTS_L, TAG_SEGS, NRTM_L,
     .                          TAG_SEGM      , NISUBS_L, NISUBM_L)
          ENDIF

c split candidates
          !TAG_SCRATCH should has been reflush to zero
          CALL SPLIT_CAND_I11(PROC       , INTBUF_TAB(NI), NRTS   , NRTS_L ,
     .                        TAG_SEGM2  , TAG_SEGS      , II_STOK, MULTIMP,
     .                        NCONT      , NOINT         , INACTI ,
     .                        TAG_SCRATCH, INTERCEP      , NI     , IPARI_L,
     .                        II_STOK_L  ,NINDX_SCRT     ,INDX_SCRT)

          CALL COPY_IVAL(II_STOK_L,1,1)

          IF (MFROT == 2)THEN
            CALL SPLIT_CAND_IVAL(INTBUF_TAB(NI)%IFPEN, II_STOK_L, TAG_II,
     .                           MULTIMP, NCONT) !KD(27)
          ENDIF

          IF(INTFRIC.GT.0)THEN
             CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IPARTFRICS,NRTS_L,1,
     .                TAG_SEGS) 
              CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IPARTFRICM,NRTM_L,1,
     .                TAG_SEGM) 
         ENDIF
C
          IF(FLAGREMNODE == 2) THEN 
            CALL SPLIT_REMNODE_I11(PROC     ,INTBUF_TAB(NI), NRTM    , NRTM_L,
     .                            TAG_SEGM2, NODLOCAL, ITAB,NUMNOD_L,TAG_SEG2S,
     .                            NREMNODE_L)
          ENDIF

          !flush to 0 (working area for sorting INT11
          SIZ = INTBUF_TAB_L(NI)%S_ADCCM
          CALL COPY_IVAL(INTBUF_TAB(NI)%ADCCM,SIZ,1) !KD(16)

          !flush to 0 (working area for sorting INT11)
          SIZ = INTBUF_TAB_L(NI)%S_CHAIN
          CALL COPY_IVAL(INTBUF_TAB(NI)%CHAIN,SIZ,1) !KD(17)

c split and write on disk floating arrays 
          CALL COPY_RVAL(INTBUF_TAB(NI)%STFAC,1,1) !JD(10)
          CALL COPY_RVAL(INTBUF_TAB(NI)%VARIABLES,NPARIR,1) !JD(11)

          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFM,NRTM_L,1,TAG_SEGM) !JD(17)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFS,NRTS_L,1,TAG_SEGS) !JD(16)

          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%PENIM,NRTM_L,2,TAG_SEGM) !JD(21)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%PENIS,NRTS_L,2,TAG_SEGS) !JD(20)

          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_M,NRTM_L,1,TAG_SEGM) !JD(15)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_S,NRTS_L,1,TAG_SEGS) !JD(14)

          SIZ = INTBUF_TAB_L(NI)%S_XSAV
          INTBUF_TAB(NI)%XSAV(1:SIZ) = 0
          CALL COPY_RVAL(INTBUF_TAB(NI)%XSAV,SIZ,1)

          SIZ = INTBUF_TAB_L(NI)%S_CRIT
          CALL COPY_RVAL(INTBUF_TAB(NI)%CRIT,SIZ,1) !JD(24)

          IF(INTTH.GT.0) THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%AREAS,NRTS_L,1,
     .                          TAG_SEGS)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%AREAM,NRTM_L,1,
     .                          TAG_SEGM)
          ENDIF

          IF(IGAP == 3)THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_ML,NRTM_L,1,TAG_SEGM) !JD(31)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_SL,NRTS_L,1,TAG_SEGS) !JD(32)
          ENDIF

          IF(MFROT == 2)THEN !IPARI(30,NI)
            INTBUF_TAB(NI)%FTSAVX(1:MULTIMP*NCONT) = 0
            INTBUF_TAB(NI)%FTSAVY(1:MULTIMP*NCONT) = 0
            INTBUF_TAB(NI)%FTSAVZ(1:MULTIMP*NCONT) = 0

            CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%FTSAVX,
     .                           II_STOK_L,TAG_II,MULTIMP,NCONT)
            CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%FTSAVY,
     .                           II_STOK_L,TAG_II,MULTIMP,NCONT)
            CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%FTSAVZ,
     .                           II_STOK_L,TAG_II,MULTIMP,NCONT)
          ENDIF
C
          DEALLOCATE(TAG_NODE_2RY,TAG_SEGM,TAG_SEGM2,TAG_SEGS,
     .              TAG_NODE_MSR,TAG_II,TAG_SEG2S)

c=====================================================================
        ELSEIF(ITYP == 12)THEN
c=====================================================================

          IF(PROC.EQ.0) THEN
            !split and write on disk integers arrays
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRECTS,NRTS,4) !KD(10) 
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRECTM,NRTM,4) !KD(11)
            CALL SPLIT_NODE_NODLOC_P0(INTBUF_TAB(NI)%NSV,NSN,1,NODLOCAL) !KD(12)
            CALL SPLIT_NODE_NODLOC_P0(INTBUF_TAB(NI)%MSR,NMN,1,NODLOCAL) !KD(13)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLM,NSN,1)   !KD(14)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IELES,NRTS,1) !KD(24)
            CALL COPY_IVAL(INTBUF_TAB(NI)%ILOCS,NSN,1)   !KD(16)
            CALL COPY_IVAL(INTBUF_TAB(NI)%NSEGM,1+NMN,1) !KD(21)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRTLS,NMN,1)   !KD(15)
            CALL COPY_IVAL(INTBUF_TAB(NI)%ILOCM,NMN,1)   !KD(17)
            CALL COPY_IVAL(INTBUF_TAB(NI)%NSEGS,1+NSN,1) !KD(21)
            CALL COPY_IVAL(INTBUF_TAB(NI)%LNSV,NST,1) !KD(22)
            CALL COPY_IVAL(INTBUF_TAB(NI)%LMSR,NMT,1) !KD(23)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IELEM,NRTM,1) !KD(25)
            CALL COPY_IVAL(INTBUF_TAB(NI)%FCOUNT,NSN,1) !KD(26)

            !split and write on disk floating arrays
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFAC,1,1)  
            CALL COPY_RVAL(INTBUF_TAB(NI)%VARIABLES,NPARIR,1) 
            CALL COPY_RVAL(INTBUF_TAB(NI)%CSTS,NSN,2) !JD(12)
            CALL COPY_RVAL(INTBUF_TAB(NI)%NMAS,NMN,1) !JD(22)
            IF(ILEV.GT.0)CALL COPY_RVAL(INTBUF_TAB(NI)%STFNS,NSN,1) !JD(14)
            CALL COPY_RVAL(INTBUF_TAB(NI)%CSTM,NMN,2) !JD(13)
            IF(ILEV.GT.0)CALL COPY_RVAL(INTBUF_TAB(NI)%STFNM,NMN,1) !JD(15)
          ENDIF 

c=====================================================================
        ELSEIF(ITYP == 14)THEN
c=====================================================================

          IF(PROC.EQ.0) THEN
            !split and write on disk integers arrays
            CALL SPLIT_NODE_NODLOC_P0(INTBUF_TAB(NI)%NSV,NSN,1,NODLOCAL) !KD(12)
            CALL COPY_IVAL(INTBUF_TAB(NI)%KSURF,1,1) !KD(11)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IMPACT,NSN,1) !KD(13)
 
            !split and write on disk floating arrays
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFAC,1,1)  !JD(10)
            CALL COPY_RVAL(INTBUF_TAB(NI)%VARIABLES,NPARIR,1) !JD(11)
            CALL COPY_RVAL(INTBUF_TAB(NI)%CIMP,NSN,3) !JD(12)
            CALL COPY_RVAL(INTBUF_TAB(NI)%NIMP,NSN,3) !JD(13)
          ENDIF 

c=====================================================================
        ELSEIF(ITYP == 15)THEN
c=====================================================================

          IF(PROC.EQ.0) THEN
            !split and write on disk integers arrays
            CALL SPLIT_NODE_NODLOC_P0(INTBUF_TAB(NI)%IRECTS,NRTS,4,NODLOCAL) !KD(10)
            CALL SPLIT_NODE_NODLOC_P0(INTBUF_TAB(NI)%NSV,NSN,1,NODLOCAL) !KD(12)
            CALL COPY_IVAL(INTBUF_TAB(NI)%KSURF,1,1) !KD(11)
            CALL COPY_IVAL(INTBUF_TAB(NI)%IMPACT,NRTS,4) !KD(13)
 
            !split and write on disk floating arrays
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFAC,1,1)  !JD(10)
            CALL COPY_RVAL(INTBUF_TAB(NI)%VARIABLES,NPARIR,1) !JD(11)
C            CALL COPY_RVAL(INTBUF_TAB(NI)%CIMP,NSN,3) !JD(12)
            CALL COPY_RVAL(INTBUF_TAB(NI)%IOLD,NRTS,3*4) !JD(12)
            CALL COPY_RVAL(INTBUF_TAB(NI)%HOLD,NRTS,3*4) !JD(13)
            CALL COPY_RVAL(INTBUF_TAB(NI)%NOLD,NRTS,3*4) !JD(14)
            CALL COPY_RVAL(INTBUF_TAB(NI)%DOLD,NRTS,3*4) !JD(15)
          ENDIF 

c=====================================================================
        ELSEIF(ITYP == 16)THEN
c=====================================================================

          IF(PROC.EQ.0) THEN
            NCONT = NSN_L

            !split and write on disk integers arrays
            CALL COPY_IVAL(INTBUF_TAB(NI)%IRECTS,NRTS,4) !KD(10)
            CALL COPY_IVAL(INTBUF_TAB(NI)%CAND_E,MULTIMP*NCONT,1) !KD(14) 
            CALL COPY_IVAL(INTBUF_TAB(NI)%CAND_N,MULTIMP*NCONT,1) !KD(15)
 
            !split and write on disk floating arrays
            CALL COPY_RVAL(INTBUF_TAB(NI)%STFAC,1,1)  !JD(10)
            CALL COPY_RVAL(INTBUF_TAB(NI)%VARIABLES,NPARIR,1) !JD(11)

            SIZ = INTBUF_TAB_L(NI)%S_XSAV
            CALL COPY_RVAL(INTBUF_TAB(NI)%XSAV,SIZ,1)
          ENDIF 

c=====================================================================
        ELSEIF(ITYP == 17)THEN
c=====================================================================
          ALLOCATE(TAG_NODE_2RY(NSN_L), TAG_NODE_MSR(NME_L))

          NCONT = NSN_L
          TAG_NODE_2RY(1:NSN_L) = 0
          TAG_NODE_MSR(1:NME_L) = 0

          !prepare addition arrays used for split
          CALL PREPARE_SPLIT_I17(PROC          ,INTBUF_TAB(NI),IPARI(1,NI) ,
     .                          TAG_NODE_2RY   ,TAG_NODE_MSR  ,     
     .                          CEP            ,CEL           ,IGRBRIC,
     .                          NSN_L, NME_L     ) 


          !split and write on disk integers arrays
          CALL COPY_IVAL(INTBUF_TAB(NI)%IRECTS,NRTS,4) !KD(10)
          CALL COPY_IVAL(INTBUF_TAB(NI)%CAND_E,MULTIMP*NCONT,1) !KD(14)
          CALL COPY_IVAL(INTBUF_TAB(NI)%CAND_N,MULTIMP*NCONT,1) !KD(15)
 
          !split and write on disk floating arrays
          CALL COPY_RVAL(INTBUF_TAB(NI)%STFAC,1,1)  !JD(10)
          CALL COPY_RVAL(INTBUF_TAB(NI)%VARIABLES,NPARIR,1) !JD(11)

          SIZ = INTBUF_TAB_L(NI)%S_XSAV
          CALL COPY_RVAL(INTBUF_TAB(NI)%XSAV,SIZ,1)

          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%KS,NSN_L,2,TAG_NODE_2RY) !JD(16)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%KM,NME_L,2,TAG_NODE_MSR) !JD(17)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%FROTS,NSN_L,7,TAG_NODE_2RY) !JD(18)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%FROTM,NME_L,7,TAG_NODE_MSR) !JD(19)

          DEALLOCATE(TAG_NODE_2RY, TAG_NODE_MSR)

c=====================================================================
        ELSEIF(ITYP == 18)THEN
c=====================================================================

c         TYPE 7 + INACTI = 7

c=====================================================================
        ELSEIF(ITYP == 20)THEN
c=====================================================================

          ALLOCATE(TAG_NODE_2RY(NSN_L),TAG_SEGM(NRTM_L),TAG_NODE_MSR(NMN_L),
     .             TAG_SEGM2(NRTM),TAG_II(II_STOK), TAGE_II(II_STOK_E),
     .             TAG_NLINS(NLINS_L),TAG_NLINM(NLINM_L),
     .             TAG_NLINS2(NLINS),TAG_NLINM2(NLINM),
     .             TAG_NLG(NLN_L),TAG_NLG2(NUMNOD), TAG_NSNE(NSNE_L),
     .             TAG_NMNE(NMNE_L),TAG_NSVE(NSNE_L),TAG_MSRE(NMNE_L) )

          TAG_NODE_2RY(1:NSN_L) = 0
          TAG_SEGM(1:NRTM_L)    = 0
          TAG_SEGM2(1:NRTM)     = 0
          TAG_II(1:II_STOK)     = 0
          TAGE_II(1:II_STOK_E)  = 0
          TAG_NODE_MSR(1:NMN_L) = 0
          TAG_NLINS(1:NLINS_L)  = 0
          TAG_NLINM(1:NLINM_L)  = 0
          TAG_NLINS2(1:NLINS)   = 0
          TAG_NLINM2(1:NLINM)   = 0
          TAG_NLG(1:NLN_L)      = 0
          TAG_NLG2(1:NUMNOD)    = 0
          TAG_NSNE(1:NSNE_L)    = 0
          TAG_NMNE(1:NMNE_L)    = 0
          TAG_NSVE(1:NSNE_L)    = 0
          TAG_MSRE(1:NMNE_L)    = 0

          !prepare addition arrays used for split
          CALL PREPARE_SPLIT_I20(
     .            PROC       , INTBUF_TAB(NI),IPARI(1,NI) ,
     .            TAG_NODE_2RY, TAG_SEGM  , TAG_NODE_MSR   ,
     .            TAG_SEGM2   , TAG_NM    , TAG_NLINS, TAG_NLINM,
     .            TAG_NLINS2  , TAG_NLINM2, TAG_NLG  ,TAG_NLG2,
     .            TAG_SCRATCH , INTERCEP  , IPARI_L  , NI, TAG_NSNE,
     .            TAG_NMNE    , TAG_NSVE  , TAG_MSRE ,NINDX_NM,INDX_NM,
     .            NINDX_SCRT  ,INDX_SCRT)

c split and write on disk integers arrays 

          CALL SPLIT_SEG_IVAL_I20(INTBUF_TAB(NI)%IRECTM,INTBUF_TAB(NI)%NLG,
     .                            NRTM_L,4,TAG_SEGM,TAG_NLG2) !KD(11)

          CALL SPLIT_SEG_IVAL_I20(INTBUF_TAB(NI)%NSV,INTBUF_TAB(NI)%NLG,
     .                            NSN_L,1,TAG_NODE_2RY,TAG_NLG2) !KD(12)

          CALL SPLIT_SEG_IVAL_I20(INTBUF_TAB(NI)%MSR,INTBUF_TAB(NI)%NLG,
     .                            NMN_L,1,TAG_NODE_MSR,TAG_NLG2) !KD(13)

          IF(INTTH.GT.0)THEN
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IELEC,NSN_L,1,
     .                          TAG_NODE_2RY) !KD(25)
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IELES,NRTM_L,1,
     .                          TAG_SEGM) !KD(24)
          ENDIF

          IF(NISUB.GT.0)THEN
            CALL COPY_IVAL(INTBUF_TAB(NI)%LISUB,NISUB,1) !KD(28)
            CALL SPLIT_NISUB_I7(INTBUF_TAB(NI), NSN_L   , TAG_NODE_2RY, NRTM_L,
     .                          TAG_SEGM      , NISUBS_L, NISUBM_L)
          ENDIF

c split candidates
          IF(II_STOK.GT.0)THEN
            CALL PREPARE_SPLIT_CAND(INTBUF_TAB(NI), TAG_SEGM2, II_STOK,
     .                               TAG_II)
          ENDIF

          !TAG_SCRATCH should has been reflush to zero
          CALL SPLIT_CAND_I20(PROC     , INTBUF_TAB(NI), NSN      , NSN_L       ,
     .                       TAG_SEGM2, II_STOK       , MULTIMP  , NCONT       ,
     .                       NOINT    , INACTI        , TAG_SCRATCH , 
     .                       II_STOK_L,IPARI_L,NI     ,NINDX_SCRT,INDX_SCRT)
          CALL COPY_IVAL(II_STOK_L,1,1)

          IF(II_STOK_E.GT.0)THEN
            CALL PREPARE_SPLIT_CAND_I20_EDGE(INTBUF_TAB(NI), TAG_NLINS2, II_STOK, TAGE_II)
          ENDIF
          CALL SPLIT_CAND_I20_EDGE(PROC     , INTBUF_TAB(NI), NLINS  , NLINS_L       ,
     .                       TAG_NLINS2, II_STOK_E       , MULTIMP  , NCONTE       ,
     .                       NOINT    , INACTI        , TAG_SCRATCH , 
     .                       II_STOK_E_L,IPARI_L,NI   ,NINDX_SCRT,INDX_SCRT)
          CALL COPY_IVAL(II_STOK_E_L,1,1)

          IF (IFQ.GT.0)THEN 
            SIZ = INTBUF_TAB(NI)%S_IFPEN
            INTBUF_TAB(NI)%IFPEN(1:SIZ) = 0
            CALL COPY_IVAL(INTBUF_TAB(NI)%IFPEN,MULTIMP*NCONT,1) !KD(27)
          ENDIF

          CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%NBINFLG,NLN_L,1,TAG_NLG) !KD(33)
          CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%MBINFLG,NRTM_L,1,TAG_SEGM) !KD(34)
          CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%NLG,NLN_L,TAG_NLG,NODLOCAL) !KD(35)

          SIZ = INTBUF_TAB_L(NI)%S_DAANC6
          ALLOCATE(IBUF8_L(SIZ))
          IBUF8_L(1:SIZ)=0
          DO K=1,NLN
             N = INTBUF_TAB(NI)%NLG(K) ! N noeud global
             NL = TAG_NLG2(N)       ! NL noeud local d interface sur le proc
             IF(NL > 0)THEN
C 36 entiers 18 doubles
               CALL COPYR8(IBUF8_L(36*(NL-1)+1),
     .                     INTBUF_TAB(NI)%DAANC6(36*(K-1)+1)     ,18  )
             ENDIF
           ENDDO
          CALL WRITE_I_C(IBUF8_L,SIZ)     
          DEALLOCATE(IBUF8_L)      

          CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%ISLINS,NLINS_L,2,TAG_NLINS) !KD(36)
          CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%ISLINM,NLINM_L,2,TAG_NLINM) !KD(37)

          CALL SPLIT_SEG_IVAL_I20(INTBUF_TAB(NI)%IXLINS,INTBUF_TAB(NI)%NLG,
     .                            NLINS_L,2,TAG_NLINS,TAG_NLG2) !KD(40)

          CALL SPLIT_SEG_IVAL_I20(INTBUF_TAB(NI)%IXLINM,INTBUF_TAB(NI)%NLG,
     .                            NLINM_L,2,TAG_NLINS,TAG_NLG2) !KD(41)


          ! NSVL
          CALL SPLIT_SEG_IVAL_I20_2(TAG_NSNE,NSNE_L,TAG_NLG2) !KD(42)
          ! MSRL
          CALL SPLIT_SEG_IVAL_I20_2(TAG_NMNE,NMNE_L,TAG_NLG2) !KD(43)

          !flush to 0 
          SIZ = INTBUF_TAB_L(NI)%S_ADCCM20
          CALL COPY_IVAL(INTBUF_TAB(NI)%ADCCM20,SIZ,1) !KD(16)

          !flush to 0 
          SIZ = INTBUF_TAB_L(NI)%S_CHAIN20
          CALL COPY_IVAL(INTBUF_TAB(NI)%CHAIN20,SIZ,1) !KD(17)


c split and write on disk floating arrays 

          CALL COPY_RVAL(INTBUF_TAB(NI)%STFAC,1,1)  !JD(10)
          CALL COPY_RVAL(INTBUF_TAB(NI)%VARIABLES,NPARIR,1) !JD(11)

          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFM,NRTM_L,1,TAG_SEGM)!JD(17)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFS,NLINS_L,1,TAG_NLINS) !JD(36)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%PENIM,NRTM_L,2,TAG_SEGM)!JD(21)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%PENIS,NSN_L,2,TAG_NODE_2RY)!JD(20)

          IF(IGAP.NE.0)THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_M,NRTM_L,1,TAG_SEGM)!JD(15)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_S,NSN_L,1,TAG_NODE_2RY)!JD(16)
          ENDIF

          SIZ = INTBUF_TAB_L(NI)%S_XSAV
          CALL COPY_RVAL(INTBUF_TAB(NI)%XSAV,SIZ,1)

          SIZ = INTBUF_TAB(NI)%S_CRIT
          CALL COPY_RVAL(INTBUF_TAB(NI)%CRIT,SIZ,1) !JD(24)

          IF (MFROT.NE.0) THEN
            CALL COPY_RVAL(INTBUF_TAB(NI)%FRIC_P,10,1) !JD(25)
          ENDIF

          IF (IFQ.NE.0) THEN
            CALL COPY_RVAL(INTBUF_TAB(NI)%XFILTR,1,1) !JD(26)
          ENDIF

          IF(INTTH.GT.0) THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%AREAS,NSN_L,1,TAG_NODE_2RY)!JD(30)
          ENDIF

          IF(INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7)THEN
            CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%CAND_P,
     .                             II_STOK_L,TAG_II,MULTIMP,NCONT)
          ENDIF

          IF(IFQ.NE.0)THEN        
            CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%CAND_FX,
     .                           II_STOK_L,TAG_II,MULTIMP,NCONT)
            CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%CAND_FY,
     .                           II_STOK_L,TAG_II,MULTIMP,NCONT)
            CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%CAND_FZ,
     .                           II_STOK_L,TAG_II,MULTIMP,NCONT)
          ENDIF

          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%XA,NLN_L,3,TAG_NLG) !JD(12)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%VA,NLN_L,3,TAG_NLG) !JD(13)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFA,NLN_L,1,TAG_NLG) !JD(14)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%PENIA,NLN_L,5,TAG_NLG) !JD(42)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%ALPHAK,NLN_L,3,TAG_NLG) !JD(43)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_SH,NRTM_L,1,TAG_SEGM)!JD(23)

          SIZ = INTBUF_TAB_L(NI)%S_AVX_ANCR
          CALL COPY_RVAL(INTBUF_TAB(NI)%AVX_ANCR,SIZ,1) !JD(22)


          IF(INTBUF_TAB(NI)%S_CRITX > 0) THEN
            CALL SPLIT_SEG_RVAL_I20(INTBUF_TAB(NI)%CRITX,
     .                              NSNE_L,3,TAG_NSVE) !JD(39)

            CALL SPLIT_SEG_RVAL_I20(INTBUF_TAB(NI)%CRITX(NSNE_L*3+1),
     .                              NMNE_L,3,TAG_MSRE) !JD(39)
          ENDIF

          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_ME,NLINM_L,1,TAG_NLINM) !JD(35)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STF,NLINM_L,1,TAG_NLINM) !JD(37)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%PENIME,NLINM_L,2,TAG_NLINM) !JD(41)

          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_SE,NLINS_L,1,TAG_NLINS) !JD(34)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%PENISE,NLINS_L,2,TAG_NLINS) !JD(40)

          DEALLOCATE(TAG_NODE_2RY,TAG_SEGM,TAG_SEGM2,
     .               TAG_NODE_MSR,TAG_II,TAGE_II,TAG_NLINS,TAG_NLINM,
     .               TAG_NLINS2,TAG_NLINM2,TAG_NLG,TAG_NLG2,TAG_NSNE,TAG_NMNE,
     .               TAG_NSVE,TAG_MSRE )

c=====================================================================
        ELSEIF(ITYP == 21)THEN
c=====================================================================      
         NMNG = IPARI(8,NI)
          ALLOCATE(TAG_NODE_2RY(NSN_L),TAG_SEGM(NRTM_L),
     .               TAG_NODE_2RY2(NSN),TAG_SEGS(NRTS_L),
     .               TAG_NODE_MSR(NMN_L),TAG_II(II_STOK) ,
     .               MSR_L_I21(NMNG),MNDD_I21(NMNG))

          TAG_NODE_2RY(1:NSN_L) = 0
          TAG_SEGM(1:NRTM_L) = 0
          TAG_SEGS(1:NRTS_L) = 0
          TAG_NODE_2RY2(1:NSN) = 0
          TAG_NODE_MSR(1:NMN_L) = 0


          !prepare addition arrays used for split
          CALL PREPARE_SPLIT_I21(PROC        , INTBUF_TAB(NI), IPARI(1,NI) ,
     .                           TAG_NODE_2RY, TAG_SEGM      , TAG_NODE_2RY2,
     .                           TAG_SEGS      , TAG_NODE_MSR,
     .                           TAG_SCRATCH , INTERCEP      , NI, INTTH   ,
     .                           NODLOCAL    , MSR_L_I21,MNDD_I21,NINDX_SCRT  ,INDX_SCRT)
 
  
          IF(II_STOK.GT.0)THEN
            CALL PREPARE_SPLIT_CAND_I21(INTBUF_TAB(NI), TAG_NODE_2RY2, II_STOK, TAG_II,
     .                                  II_STOK_L,PROC)
          ENDIF

c split and write on disk integers arrays 

          CALL SPLIT_SEG_NODLOC(INTBUF_TAB(NI)%IRECTS,NRTS_L,4,
     .                         TAG_SEGS,NODLOCAL) !KD(10)

          CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IRECTM,NRTM_L,4,
     .                        TAG_SEGM) !KD(11)


          CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%NSV,NSN_L,
     .                          TAG_NODE_2RY,NODLOCAL) !KD(12)

          CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%MSR,NMN_L,
     .                          TAG_NODE_MSR,NODLOCAL) !KD(13)

          IF(INACTI.EQ.5.OR.INACTI.EQ.6)THEN
            CALL SPLIT_CAND_IVAL_I21(INTBUF_TAB(NI)%IRTLM,
     .            II_STOK_L,TAG_II,NSN_L,2) !KD(16)
          ELSE
            ALLOCATE(TABI_ZERO(2*NSN_L))
            TABI_ZERO(1:2*NSN_L)=0
            CALL COPY_IVAL(TABI_ZERO,NSN_L,2)
            DEALLOCATE(TABI_ZERO)
          ENDIF

          CALL COPY_IVAL(INTBUF_TAB(NI)%IELES,NRTS_L,1)
          
c split MAIN candidates
          IF(INACTI.EQ.5.OR.INACTI.EQ.6)THEN
            CALL SPLIT_CAND_IVAL_I21(INTBUF_TAB(NI)%CAND_E,
     .            II_STOK_L,TAG_II,MULTIMP*NCONT,1) !KD(14)
c split Secnd candidates
            CALL SPLIT_2RY_CAND_IVAL_I21(INTBUF_TAB(NI)%CAND_N,
     .            II_STOK_L,TAG_II,TAG_NODE_2RY2,MULTIMP*NCONT) !KD(15)
          ELSE
            ALLOCATE(TABI_ZERO(MULTIMP*NCONT))
            TABI_ZERO(1:MULTIMP*NCONT)=0
            CALL COPY_IVAL(TABI_ZERO,MULTIMP*NCONT,1)
            CALL COPY_IVAL(TABI_ZERO,MULTIMP*NCONT,1)
            DEALLOCATE(TABI_ZERO)
          ENDIF

          CALL COPY_IVAL(II_STOK_L,1,1)

          IF(INACTI.EQ.5.OR.INACTI.EQ.6)THEN
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IFPEN,NSN_L,1,
     .                          TAG_NODE_2RY2) !KD(25)
          ELSE
            ALLOCATE(TABI_ZERO(NSN_L))
            TABI_ZERO(1:NSN_L)=0            
            CALL COPY_IVAL(TABI_ZERO,NSN_L,1)
            DEALLOCATE(TABI_ZERO)
          ENDIF

c          DO I=1, NMNG_L
c            INTBUF_TAB(NI)%MSR21(I) = INTBUF_TAB(NI)%MSR(I)
c          ENDDO
          CALL COPY_IVAL(INTBUF_TAB(NI)%MSR,NMNG_L,1)   !KD(17)


C  PREPARE MNDD TAB FOR MAIN TEMPERATURE COMMUNICATION
          IF (INTTH == 2.OR.FLAGLOADP > 0) THEN
            ! preparation done in PREPARE_SPLIT
            CALL COPY_IVAL(MNDD_I21,NMNG_L,1)   !KD(28)
          ENDIF 

C  PREPARE MSRL TAB FOR LOCAL NODES NUMBERING==> MAIN TEMPERATURE COMMUNICATION
          IF (INTTH == 2.OR.FLAGLOADP > 0) THEN
            ! preparation done previously in PREPARE_SPLIT
            CALL COPY_IVAL(MSR_L_I21,NMNG_L,1)   !KD(29)
          ENDIF 

c split and write on disk floating arrays
          CALL COPY_RVAL(INTBUF_TAB(NI)%STFAC,1,1)  
          CALL COPY_RVAL(INTBUF_TAB(NI)%VARIABLES,NPARIR,1)
          CALL COPY_RVAL(INTBUF_TAB(NI)%CSTS,NSN_L,2) 
          CALL COPY_RVAL(INTBUF_TAB(NI)%XM0,NMNG_L,3)  
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFNS,NSN_L,1,TAG_NODE_2RY)


          IF(INACTI.EQ.5.OR.INACTI.EQ.6)THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%PENIS,NSN_L,1,
     .                         TAG_NODE_2RY) !JD(18)
          ELSE
            ALLOCATE(TABR_ZERO(NSN_L))
            TABR_ZERO(1:NSN_L)=0
            CALL COPY_RVAL(TABR_ZERO,NSN_L,1) 
            DEALLOCATE(TABR_ZERO)
          ENDIF

          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_S,NSN_L,1,TAG_NODE_2RY) ! Gap_S : JD(16)
          IF(IGAP == 2)THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_S(1+NSN),NSN_L,1,TAG_NODE_2RY) ! Gap_S0 : JD(16)
          ENDIF

          SIZ = INTBUF_TAB_L(NI)%S_XSAV
          CALL COPY_RVAL(INTBUF_TAB(NI)%XSAV,SIZ,1)

          CALL COPY_RVAL(INTBUF_TAB(NI)%CRIT,12,1)

          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%AREAS,NSN_L,1,TAG_NODE_2RY)!JD(30)

          IF(INACTI.EQ.5.OR.INACTI.EQ.6)THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%FTSAVX,NSN_L,1,
     .                         TAG_NODE_2RY) !JD(27)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%FTSAVY,NSN_L,1,
     .                         TAG_NODE_2RY) !JD(28)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%FTSAVZ,NSN_L,1,
     .                         TAG_NODE_2RY) !JD(29) 
          ELSE
             ALLOCATE(TABR_ZERO(NSN_L))
             TABR_ZERO(1:NSN_L)=0
             CALL COPY_RVAL(TABR_ZERO,NSN_L,1)
             CALL COPY_RVAL(TABR_ZERO,NSN_L,1)       
             CALL COPY_RVAL(TABR_ZERO,NSN_L,1)
             DEALLOCATE(TABR_ZERO)
          ENDIF

          CALL COPY_RVAL(INTBUF_TAB(NI)%STF,NRTM_L,1) 

          CALL COPY_RVAL(INTBUF_TAB(NI)%RCURV,NRTM_L,1)

          CALL COPY_RVAL(INTBUF_TAB(NI)%ANGLM,NRTM_L,1)

          IF (MFROT.NE.0) THEN
            CALL COPY_RVAL(INTBUF_TAB(NI)%FROT_P,10,1)
          ENDIF

          CALL COPY_RVAL(INTBUF_TAB(NI)%ALPHA0,1,1)

          IF(INTTH > 0) THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%AS,NSN_L,1,TAG_NODE_2RY)!JD(31)  
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%BS,NSN_L,1,TAG_NODE_2RY)!JD(32)
          ENDIF

          IF(IGAP.GE.1) THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%THKNOD0,NSN_L,1,TAG_NODE_2RY)!JD(33)  
          ENDIF

          SIZ = INTBUF_TAB_L(NI)%S_NOD_NORMAL
          CALL COPY_RVAL(INTBUF_TAB(NI)%NOD_NORMAL,SIZ,1)

          DEALLOCATE(TAG_NODE_2RY,TAG_SEGM,TAG_NODE_2RY2,TAG_SEGS,
     .             TAG_NODE_MSR,TAG_II,MSR_L_I21,MNDD_I21)

c=====================================================================
        ELSEIF(ITYP == 22)THEN
c=====================================================================
          ALLOCATE(TAG_NODE_2RY(NSN_L),TAG_SEGM(NRTM_L),
     .             TAG_NODE_MSR(NMN_L),TAG_SEGM2(NRTM),TAG_II(II_STOK))

          ALLOCATE(I710XSAV(NMN))

          TAG_NODE_2RY(1:NSN_L) = 0
          TAG_SEGM(1:NRTM_L)    = 0
          TAG_SEGM2(1:NRTM)     = 0
          TAG_NODE_MSR(1:NMN_L) = 0
          TAG_II(1:II_STOK)     = 0

          !prepare addition arrays used for split
          CALL PREPARE_SPLIT_I7(PROC          ,INTBUF_TAB(NI),IPARI(1,NI) ,
     .                          INTERCEP(1,NI),TAG_NODE_2RY,TAG_SEGM      ,
     .                          TAG_SEGM2     ,TAG_NM        ,TAG_NODE_MSR,
     .                          TAG_SCRATCH   ,NI, CEP, MULTI_FVM,I710XSAV,
     .                          NINDX_NM      ,INDX_NM,NINDX_SCRT,INDX_SCRT,NODLOCAL,
     .                          NUMNOD_L)
  
          IF(II_STOK.GT.0)THEN
            CALL PREPARE_SPLIT_CAND(INTBUF_TAB(NI), TAG_SEGM2, II_STOK,
     .                               TAG_II)
          ENDIF

c split and write on disk integers arrays

          CALL SPLIT_SEG_NODLOC(INTBUF_TAB(NI)%IRECTM,NRTM_L,4,
     .                          TAG_SEGM,NODLOCAL)
          CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%NSV,NSN_L,
     .                          TAG_NODE_2RY,NODLOCAL)
          CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%MSR,NMN_L,
     .                          TAG_NODE_MSR,NODLOCAL)

          IF(INTTH.GT.0)THEN
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IELEC,NSN_L,1,
     .                          TAG_NODE_2RY) !KD(25)
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IELES,NRTM_L,1,
     .                          TAG_SEGM) !KD(24)
          ENDIF

          IF(NISUB.GT.0)THEN
            CALL COPY_IVAL(INTBUF_TAB(NI)%LISUB,NISUB,1) !KD(28)
            CALL SPLIT_NISUB_I7(INTBUF_TAB(NI), NSN_L   , TAG_NODE_2RY, NRTM_L,
     .                          TAG_SEGM      , NISUBS_L, NISUBM_L)
          ENDIF

c split candidates :
          !TAG_SCRATCH should has been reflush to zero
          CALL SPLIT_CAND_I7(PROC     , INTBUF_TAB(NI), NSN      , NSN_L       ,
     .                       TAG_SEGM2, II_STOK       , MULTIMP  , NCONT       ,
     .                       NOINT    , INACTI        , TAG_SCRATCH , 
     .                       II_STOK_L, ITYP          ,NINDX_SCRT,INDX_SCRT,NODLOCAL,
     .                       NUMNOD_L )

          CALL COPY_IVAL(II_STOK_L,1,1)

          IF (IFQ.GT.0)THEN
            CALL SPLIT_CAND_IVAL(INTBUF_TAB(NI)%IFPEN,
     .            II_STOK_L,TAG_II,MULTIMP,NCONT) !KD(27)
          ENDIF

          IF(FLAGREMNODE == 2) THEN  
            CALL SPLIT_REMNODE_I7(PROC     ,INTBUF_TAB(NI), NRTM    , NRTM_L,
     .                            TAG_SEGM2,NREMNODE_L      , NODLOCAL, ITAB,NUMNOD_L)
          ENDIF
           
c split and write on disk floating arrays 
          CALL COPY_RVAL(INTBUF_TAB(NI)%STFAC,1,1)  
          CALL COPY_RVAL(INTBUF_TAB(NI)%VARIABLES,NPARIR,1)   

          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFNS,NSN_L,1,TAG_NODE_2RY)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFM,NRTM_L,1,TAG_SEGM)

          IF(IGAP.NE.0)THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_M,NRTM_L,1,TAG_SEGM)!JD(15)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_S,NSN_L,1,TAG_NODE_2RY)!JD(16)
          ENDIF

          SIZ = INTBUF_TAB_L(NI)%S_XSAV
          INTBUF_TAB(NI)%XSAV(1:SIZ) = 0
          CALL COPY_RVAL(INTBUF_TAB(NI)%XSAV,SIZ,1)

          SIZ = INTBUF_TAB_L(NI)%S_CRIT
          CALL COPY_RVAL(INTBUF_TAB(NI)%CRIT,SIZ,1)

          IF (MFROT.NE.0) THEN
            CALL COPY_RVAL(INTBUF_TAB(NI)%FRIC_P,10,1)
          ENDIF

          IF (IFQ.NE.0) THEN
            CALL COPY_RVAL(INTBUF_TAB(NI)%XFILTR,1,1)
          ENDIF

          IF(INTTH.GT.0) THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%AREAS,NSN_L,1,TAG_NODE_2RY)!JD(30)
          ENDIF

          IF(IGAP==3)THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_ML,NRTM_L,1,TAG_SEGM)!JD(31)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_SL,NSN_L,1,TAG_NODE_2RY)!JD(32)  
          ENDIF

c
          IF(INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7)THEN
            CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%CAND_P,
     .                             II_STOK_L,TAG_II,MULTIMP,NCONT)
          ENDIF

          IF(IFQ.NE.0)THEN        
            CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%FTSAVX,
     .                           II_STOK_L,TAG_II,MULTIMP,NCONT)
            CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%FTSAVY,
     .                           II_STOK_L,TAG_II,MULTIMP,NCONT)
            CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%FTSAVZ,
     .                           II_STOK_L,TAG_II,MULTIMP,NCONT)
          ENDIF

          DEALLOCATE(TAG_NODE_2RY,TAG_SEGM,TAG_SEGM2,
     .              TAG_NODE_MSR,TAG_II)
          DEALLOCATE(I710XSAV)
c=====================================================================
        ELSEIF(ITYP == 23)THEN
c=====================================================================
          ALLOCATE(TAG_NODE_2RY(NSN_L),TAG_SEGM(NRTM_L),
     .             TAG_NODE_MSR(NMN_L),TAG_SEGM2(NRTM),TAG_II(II_STOK))
           ALLOCATE(I710XSAV(NMN))

          TAG_NODE_2RY(1:NSN_L) = 0
          TAG_SEGM(1:NRTM_L) = 0
          TAG_SEGM2(1:NRTM) = 0
          TAG_NODE_MSR(1:NMN_L) = 0
          TAG_II(1:II_STOK) = 0

          !prepare addition arrays used for split
          CALL PREPARE_SPLIT_I7(PROC          ,INTBUF_TAB(NI),IPARI(1,NI) ,
     .                          INTERCEP(1,NI),TAG_NODE_2RY,TAG_SEGM      ,
     .                          TAG_SEGM2     ,TAG_NM        ,TAG_NODE_MSR,
     .                          TAG_SCRATCH   ,NI, CEP, MULTI_FVM,I710XSAV,
     .                          NINDX_NM      ,INDX_NM,NINDX_SCRT,INDX_SCRT,NODLOCAL,
     .                          NUMNOD_L)

          IF(II_STOK.GT.0)THEN
            CALL PREPARE_SPLIT_CAND(INTBUF_TAB(NI), TAG_SEGM2, II_STOK,
     .                                   TAG_II)
          ENDIF

c split and write on disk integers arrays

          CALL COPY_IVAL(INTBUF_TAB(NI)%IRECTS,NRTS_L,4)
          CALL SPLIT_SEG_NODLOC(INTBUF_TAB(NI)%IRECTM,NRTM_L,4,
     .                          TAG_SEGM,NODLOCAL)


          CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%NSV,NSN_L,
     .                          TAG_NODE_2RY,NODLOCAL)
          CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%MSR,NMN_L,
     .                          TAG_NODE_MSR,NODLOCAL)

c split candidates
          !TAG_SCRATCH should has been reflush to zero
          CALL SPLIT_CAND_I7(PROC     , INTBUF_TAB(NI), NSN      , NSN_L       ,
     .                       TAG_SEGM2, II_STOK       , MULTIMP  , NCONT       ,
     .                       NOINT    , INACTI        , TAG_SCRATCH , 
     .                       II_STOK_L, ITYP          ,NINDX_SCRT,INDX_SCRT   ,NODLOCAL,
     .                       NUMNOD_L )

          CALL COPY_IVAL(II_STOK_L,1,1)

          CALL SPLIT_CAND_IVAL(INTBUF_TAB(NI)%IFPEN,
     .          II_STOK_L,TAG_II,MULTIMP,NCONT) !KD(27)

c split and write on disk floating arrays 
          CALL COPY_RVAL(INTBUF_TAB(NI)%STFAC,1,1)  
          CALL COPY_RVAL(INTBUF_TAB(NI)%VARIABLES,NPARIR,1)   

          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFNS,NSN_L,1,TAG_NODE_2RY)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFM,NRTM_L,1,TAG_SEGM)

          IF(IGAP.NE.0)THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_M,NRTM_L,1,TAG_SEGM)!JD(15)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_S,NSN_L,1,TAG_NODE_2RY)!JD(16)
          ENDIF

          SIZ = INTBUF_TAB_L(NI)%S_XSAV
          CALL COPY_RVAL(INTBUF_TAB(NI)%XSAV,SIZ,1)

          SIZ = INTBUF_TAB_L(NI)%S_CRIT
          CALL COPY_RVAL(INTBUF_TAB(NI)%CRIT,SIZ,1)

          IF(MFROT.NE.0) CALL COPY_RVAL(INTBUF_TAB(NI)%FRIC_P,10,1)

          CALL COPY_RVAL(INTBUF_TAB(NI)%XFILTR,1,1)

          CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%CAND_P,
     .                             II_STOK_L,TAG_II,MULTIMP,NCONT)

          CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%FTSAVX,
     .                         II_STOK_L,TAG_II,MULTIMP,NCONT)
          CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%FTSAVY,
     .                         II_STOK_L,TAG_II,MULTIMP,NCONT)
          CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%FTSAVZ,
     .                         II_STOK_L,TAG_II,MULTIMP,NCONT)

          DEALLOCATE(TAG_NODE_2RY,TAG_SEGM,TAG_SEGM2,
     .              TAG_NODE_MSR,TAG_II )
          DEALLOCATE(I710XSAV)

c=====================================================================
        ELSEIF(ITYP == 24) THEN
c=====================================================================
          ALLOCATE(TAG_NODE_2RY(NSN_L),TAG_SEGM(NRTM_L),
     .             TAG_NODE_MSR(NMN_L),TAG_SEGM2(NRTM),TAG_II(II_STOK))
          ALLOCATE(TAG_SEGS(NRTSE_L),TAG_SEG2S(NRTSE),TAG_NSNE_L(NSNE_L))
          ALLOCATE(TAG_NODE_2RY2(NSN),TAG_IELEM(NRTS_L),TAG_SEGSS(NRTS_L))


          IF(IEDGE4 > 0) THEN
               ALLOCATE(TAG_2RY_INV(NSN))
          ELSE
               ALLOCATE(TAG_2RY_INV(1))
          ENDIF

          TAG_NODE_2RY(1:NSN_L) = 0
          TAG_SEGM(1:NRTM_L) = 0
          TAG_SEGM2(1:NRTM) = 0
          TAG_NODE_MSR(1:NMN_L) = 0
          TAG_II(1:II_STOK) = 0
          TAG_SEGS(1:NRTSE_L) = 0
          TAG_SEG2S(1:NRTSE) = 0
          TAG_NSNE_L(1:NSNE_L)=0
          TAG_NODE_2RY2(1:NSN) = 0
          TAG_IELEM(1:NRTS_L) = 0
          TAG_SEGSS(1:NRTS_L) = 0
          IF(IEDGE4 > 0) TAG_2RY_INV(1:NSN)=0
          IF(FLAG_INIT_24_25) THEN
                NODLOCAL24(1:NUMNOD) = NODLOCAL(1:NUMNOD)
                NODLOCAL24(NUMNOD+1:NUMNOD+I24MAXNSNE) = 0
                FLAG_INIT_24_25 = .FALSE.
          ENDIF

          CALL PREPARE_SPLIT_I24(PROC          ,INTBUF_TAB(NI),IPARI(1,NI) ,
     .                          INTERCEP(1,NI),TAG_NODE_2RY,TAG_SEGM      ,
     .                          TAG_SEGM2     ,TAG_NM      ,TAG_NODE_MSR,
     .                          TAG_SCRATCH  ,NODLOCAL24,NODLOCAL ,
     .                          INTERCEP(2,NI),NUMNOD_L,TAG_NSNE_L,
     .                          TAG_SEGS,TAG_SEG2S,NI,TAG_2RY_INV,IEDGE4,
     .                          TAG_NODE_2RY2,TAG_IELEM,CEP,CEL,TAG_SEGSS,
     .                          NINDX_NM,INDX_NM,NINDX_SCRT  ,INDX_SCRT,
     .                          NINDX_NDLOCAL24,INDX_NDLOCAL24,INTERCEP(3,NI))

          IF(II_STOK.GT.0)THEN
            CALL PREPARE_SPLIT_CAND(INTBUF_TAB(NI), TAG_SEGM2, II_STOK,
     .                             TAG_II )
          ENDIF
c split and write on disk integers arrays

          IF(INTNITSCHE  > 0) THEN
            CALL SPLIT_SEG_NODLOC(INTBUF_TAB(NI)%IRECTS,NRTS_L,4,
     .                            TAG_SEGSS,TAG_NODE_2RY2)
          ENDIF

          CALL SPLIT_SEG_NODLOC(INTBUF_TAB(NI)%IRECTM,NRTM_L,4,
     .                          TAG_SEGM,NODLOCAL)
          CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%NSV,NSN_L,
     .                          TAG_NODE_2RY,NODLOCAL24)
          CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%MSR,NMN_L,
     .                          TAG_NODE_MSR,NODLOCAL)
C IRTML a besoin de num globaux
          CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IRTLM,NSN_L,2,
     .                         TAG_NODE_2RY) !KD(18)

          IF(INTTH.GT.0)THEN
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IELEC,NSN_L,1,
     .                          TAG_NODE_2RY) !KD(25)
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IELES,NRTM_L,1,
     .                          TAG_SEGM) !KD(24)
          ENDIF

          IF(NISUB.GT.0)THEN
            CALL COPY_IVAL(INTBUF_TAB(NI)%LISUB,NISUB,1) !KD(28)
            CALL COPY_IVAL(INTBUF_TAB(NI)%TYPSUB,NISUB,1) 
            CALL SPLIT_NISUB_I7(INTBUF_TAB(NI), NSN_L   , TAG_NODE_2RY, NRTM_L,
     .                          TAG_SEGM      , NISUBS_L, NISUBM_L)
          ENDIF

c split candidates--> to modify when spmd for edge
          !TAG_SCRATCH should has been reflush to zero
         NSN0 = NSN - NSNE
         NSN0_L = NSN_L - NSNE_L
          CALL SPLIT_CAND_I24(PROC     , INTBUF_TAB(NI), NSN     , NSN_L       ,
     .                       TAG_SEGM2, II_STOK       , MULTIMP  , NCONT       ,
     .                       NOINT    , INACTI        ,TAG_SCRATCH ,II_STOK_L  ,
     .                       INTERCEP(2,NI),NINDX_SCRT,INDX_SCRT  ,NODLOCAL    ,
     .                       NUMNOD_L )
          CALL COPY_IVAL(II_STOK_L,1,1)

         IF(INTFRIC.GT.0)THEN
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IPARTFRICS,NSN_L,1,
     .                TAG_NODE_2RY) 
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IPARTFRICM,NRTM_L,1,
     .                TAG_SEGM) 
            IF(INTBUF_FRIC_TAB(INTFRIC)%IORTHFRIC > 0) THEN
               CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IREP_FRICM,NRTM_L,1,
     .              TAG_SEGM)
            ENDIF
          ENDIF

          IF(FLAGREMNODE == 2) THEN  
            CALL SPLIT_REMNODE_I24(PROC     ,INTBUF_TAB(NI), NRTM    , NRTM_L,
     .                            TAG_SEGM2,NREMNODE_L      , NODLOCAL, ITAB   ,
     .                            INTBUF_TAB(NI)%IS2ID,INTERCEP(2,NI),NSNE_L ,
     .                            NODLOCAL24)
          ENDIF

C-------ILEV=2 for remove S1/S1 S2/S2 in case of S1/S2 input
          IF(ILEV==2)THEN
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%NBINFLG,NSN_L,1,
     .                           TAG_NODE_2RY) !KD(33)
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%MBINFLG,NRTM_L,1,
     .                           TAG_SEGM) !KD(34) 
          ELSEIF(IEDGE.GT.0)THEN
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%MBINFLG,NRTM_L,1,
     .                           TAG_SEGM) !KD(34) 
          ENDIF
c---------------to see if CAND_T is useful
          IF (IEDGE.GT.0)THEN
            CALL SPLIT_CAND_IVAL(INTBUF_TAB(NI)%CAND_T,
     .            II_STOK_L,TAG_II,MULTIMP,NCONT) !KD(37)
          ENDIF
          MX_VM=4
          CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%MVOISIN,NRTM_L,MX_VM,
     .                        TAG_SEGM) !KD(17)
          MX_VN=8
          CALL SPLIT_SEG_NODLOC_I24(INTBUF_TAB(NI)%NVOISIN,NRTM_L,
     .                         MX_VN,TAG_SEGM,NODLOCAL) !KD(18)
C----------MSEGLO(KD(19)): global number (should not change to local)
          CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%MSEGLO,NRTM_L,1,
     .                        TAG_SEGM) !KD(19)
C----------MSEGTYP(KD(20)): seg type-------------------------
          CALL SPLIT_NODE_IVAL_I24(INTBUF_TAB(NI)%MSEGTYP24,NRTM_L,
     .                        TAG_SEGM, TAG_SEGM2 ) !KD(20)
          IF(IEDGE.GT.0)THEN
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%ISEADD,NSN_L,1,
     .                           TAG_NODE_2RY) !KD(35)
            CALL COPY_IVAL(INTBUF_TAB(NI)%ISEDGE,L24ADD,1) !KD(36)
          ENDIF
C          
          IF(INTPLY > 0)THEN 
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%ISEG_PXFEM,NRTM_L,1,
     .                           TAG_SEGM) !KD(40)
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%ISEG_PLY,NRTM_L,12,
     .                           TAG_SEGM) !KD(41)
          ENDIF
          CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%ICONT_I,NSN_L,1,
     .                         TAG_NODE_2RY) 


C------- NEW EDGE :IRTSE, IS2SE, IS2PT   -->corrige 5eme  
         IF (IEDGE4 >0) THEN

          CALL SPLIT_SEGEDGE_NODLOC_I24(INTBUF_TAB(NI)%IRTSE,NRTSE_L,
     .                                  TAG_SEGS,NODLOCAL,NI)
C          print*,PROC,',NSNE_L=',NSNE_L
          CALL SPLIT_SEG_EDGE(NSNE_L,INTBUF_TAB(NI)%IS2SE,TAG_NSNE_L,TAG_SEG2S,NI) 

          CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IS2PT,NSNE_L,1,
     .                         TAG_NSNE_L)
          CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%ISPT2,NSN_L,1,
     .                         TAG_NODE_2RY)

          CALL SPLIT_ISEGPT_IVAL(INTBUF_TAB(NI)%ISEGPT,NSN_L,1,
     .                          TAG_NODE_2RY,NI,TAG_2RY_INV ,PROC)

          CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IS2ID,NSNE_L,1,
     .                         TAG_NSNE_L)
         END IF !(NSNE.GT.0) THEN

C          
          IF(INTNITSCHE > 0)THEN 
            CALL COPY_IVAL(TAG_IELEM,NRTS_L,1) 
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%ADRECTS,NRTS_L,4,TAG_SEGSS)
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%FACNRTS,NRTS_L,1,TAG_SEGSS)
          ENDIF
         
c split and write on disk floating arrays 

          CALL COPY_RVAL(INTBUF_TAB(NI)%STFAC,1,1)  
          CALL COPY_RVAL(INTBUF_TAB(NI)%VARIABLES,NPARIR,1)   
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFNS,NSN_L,1,TAG_NODE_2RY)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFM,NRTM_L,1,TAG_SEGM)

          IF(IGAP.NE.0)THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_M,NRTM_L,1,TAG_SEGM)!JD(15)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_S,NSN_L,1,TAG_NODE_2RY)!JD(16)
          ENDIF

          SIZ = INTBUF_TAB_L(NI)%S_XSAV
          CALL COPY_RVAL(INTBUF_TAB(NI)%XSAV,SIZ,1)

          SIZ = INTBUF_TAB_L(NI)%S_CRIT
          CALL COPY_RVAL(INTBUF_TAB(NI)%CRIT,SIZ,1)

          IF (MFROT.NE.0) THEN
            CALL COPY_RVAL(INTBUF_TAB(NI)%FRIC_P,10,1)
          ENDIF

          IF (IFQ.NE.0) THEN
            CALL COPY_RVAL(INTBUF_TAB(NI)%XFILTR,1,1)
          ENDIF

          IF(INTTH.GT.0) THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%AREAS,NSN_L,1,TAG_NODE_2RY)!JD(30)
          ENDIF


          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAPN_M,NMN_L,1,
     .                            TAG_NODE_MSR) !JD(22)
          CALL COPY_RVAL(INTBUF_TAB(NI)%SECND_FR,NSN_L,6)!JD(27)

          !PEN_OLD,PEN_INI for Inacti=5 
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%PENE_OLD,NSN_L,5,
     .                            TAG_NODE_2RY) !JD(31)

          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STIF_OLD,NSN_L,2,
     .                            TAG_NODE_2RY) !JD(31)

          CALL COPY_RVAL(INTBUF_TAB(NI)%TIME_S,NSN_L,1)!JD(33)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_NM,NRTM_L,12,TAG_SEGM)!JD(34)
          IF (IEDGE.GT.0) THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%EDGE8L2,NSN_L,1,
     .                              TAG_NODE_2RY) !JD(35)
          ENDIF


          !-for implicit Kg-----
          IF (INTKG.GT.0) THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%NOD_2RY_LGTH,NSN_L,1,
     .                              TAG_NODE_2RY) !JD(42)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%NOD_MAS_LGTH,NMN_L,1,
     .                              TAG_NODE_MSR) !JD(43)
          ENDIF

          IF(INTPLY > 0)THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_N0,NRTM_L,12,TAG_SEGM)!JD(36)
            CALL COPY_RVAL(INTBUF_TAB(NI)%DGAP_NM,NRTM_L,4) !JD(37)
            CALL COPY_RVAL(INTBUF_TAB(NI)%DGAP_M,NRTM_L,1) !JD(38)
            CALL COPY_RVAL(INTBUF_TAB(NI)%DELTA_PMAX_DGAP,1,1) !JD(39)
          ENDIF


          IF (NSNE.GT.0) THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%XFIC,NSNE_L,3,
     .                           TAG_NSNE_L)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%VFIC,NSNE_L,3,
     .                           TAG_NSNE_L) 

            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%MSFIC,NSNE_L,1,
     .                           TAG_NSNE_L) 
          ENDIF

         IF(INTFRIC.GT.0)THEN
            IF(INTBUF_FRIC_TAB(INTFRIC)%IORTHFRIC > 0) THEN
               CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%DIR_FRICM,NRTM_L,2,
     .                TAG_SEGM)
            ENDIF
          ENDIF


          IF(NINDX_NDLOCAL24>0) THEN
                DO II=1,NINDX_NDLOCAL24
                        I = INDX_NDLOCAL24(II)
                        IF(I>NUMNOD) THEN
                                NODLOCAL24(I) = 0
                        ELSE
                                NODLOCAL24(I) = NODLOCAL(I)
                        ENDIF
                ENDDO
          ENDIF
          NINDX_NDLOCAL24 = 0

          DEALLOCATE(TAG_NODE_2RY,TAG_SEGM,TAG_SEGM2,
     .              TAG_NODE_MSR,TAG_II,TAG_SEGS)
          DEALLOCATE(TAG_SEG2S)
          DEALLOCATE(TAG_NSNE_L)
          DEALLOCATE(TAG_2RY_INV)
          DEALLOCATE(TAG_NODE_2RY2,TAG_IELEM,TAG_SEGSS)

c=====================================================================
        ELSEIF(ITYP == 25) THEN
c=====================================================================
          ALLOCATE(TAG_NODE_2RY(NSN_L),TAG_SEGM(NRTM_L),
     .             TAG_NODE_MSR(NMN_L),TAG_SEGM2(NRTM),TAG_II(II_STOK),
     .             TAG_SM(NADMSR),KNOR2MSR(NADMSR_L+1),NOR2MSR(4*NRTM_L),
     .             TAG_NODE_2RY2(NSN))
!          ALLOCATE(NODLOCAL24(NUMNOD))

          TAG_NODE_2RY(1:NSN_L) = 0
          TAG_SEGM(1:NRTM_L) = 0
          TAG_SEGM2(1:NRTM)  = 0
          TAG_NODE_MSR(1:NMN_L) = 0
          TAG_II(1:II_STOK) = 0
          TAG_NODE_2RY2(1:NSN) = 0

          TAG_SM(1:NADMSR)  = 0

          IF(FLAG_INIT_24_25) THEN
                NODLOCAL24(1:NUMNOD) = NODLOCAL(1:NUMNOD)
                FLAG_INIT_24_25 = .FALSE.
          ENDIF

!          NODLOCAL24(1:NUMNOD) = NODLOCAL(1:NUMNOD)
          
          KNOR2MSR(1:NADMSR_L+1)=0
          NOR2MSR(1:4*NRTM_L) =0
          CALL PREPARE_SPLIT_I25(PROC          ,INTBUF_TAB(NI),IPARI(1,NI) ,
     .                          INTERCEP(1,NI),TAG_NODE_2RY,TAG_SEGM      ,
     .                          TAG_SEGM2     ,TAG_NM      ,TAG_NODE_MSR,
     .                          TAG_SCRATCH   ,TAG_SM      ,KNOR2MSR    ,
     .                          NOR2MSR       ,TAG_NODE_2RY2,NINDX_NM,INDX_NM,
     .                          NINDX_SCRT    ,INDX_SCRT)

          IF(II_STOK.GT.0)THEN
            CALL PREPARE_SPLIT_CAND(INTBUF_TAB(NI), TAG_SEGM2, II_STOK,
     .                             TAG_II )
          ENDIF

          ALLOCATE(TAG_EDGE(NEDGE_L)) ! local to global
          ALLOCATE(TAG_EDGE2(NEDGE))  ! global to local
          II_STOK_S_L = 0
          II_STOK_E_L = 0
          IF(IEDGE/=0)THEN
            ALLOCATE(TAG_II_E2E(II_STOK_E))
            ALLOCATE(TAG_II_E2S(II_STOK_S))
            CALL PREPARE_SPLIT_CAND_I25_EDGE(INTBUF_TAB(NI),TAG_SEGM2,
     .                               TAG_EDGE,  NEDGE_L,  TAG_EDGE2, NEDGE,
     .                               II_STOK_E, II_STOK_E_L, TAG_II_E2E,
     .                               II_STOK_S, II_STOK_S_L, TAG_II_E2S,PROC)
          END IF

c split and write on disk integers arrays

          CALL SPLIT_SEG_NODLOC(INTBUF_TAB(NI)%IRECTM,NRTM_L,4,
     .                          TAG_SEGM,NODLOCAL)
          CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%NSV,NSN_L,
     .                          TAG_NODE_2RY,NODLOCAL24)
          CALL SPLIT_NODE_NODLOC(INTBUF_TAB(NI)%MSR,NMN_L,
     .                          TAG_NODE_MSR,NODLOCAL)
C IRTML a besoin de num globaux
          CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IRTLM,NSN_L,4,
     .                         TAG_NODE_2RY) !KD(18)

         IF(INTTH.GT.0)THEN
           CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IELES,NSN_L,1,
     .                         TAG_NODE_2RY) 
         ENDIF

          IF(NISUB.GT.0)THEN
            CALL COPY_IVAL(INTBUF_TAB(NI)%LISUB,NISUB,1) !KD(28)
            CALL COPY_IVAL(INTBUF_TAB(NI)%TYPSUB,NISUB,1) 
            CALL SPLIT_NISUB_I25(INTBUF_TAB(NI), NSN_L   , TAG_NODE_2RY, NRTM_L,
     1                          TAG_SEGM      , NISUBS_L, NISUBM_L, 
     2       IEDGE,
     3       NEDGE,
     4       NEDGE_L,
     5       TAG_EDGE,
     6       TAG_EDGE2,
     7       NISUBE_L,
     8       PROC)
C           IF(IEDGE/=0)THEN ! FAIRE SPMD
C             CALL COPY_IVAL(INTBUF_TAB(NI)%ADDSUBE,NEDGE+1,1)
C             CALL COPY_IVAL(INTBUF_TAB(NI)%LISUBE,NISUBE_L,1) 
C             CALL COPY_IVAL(INTBUF_TAB(NI)%INFLG_SUBE,NISUBE_L,1) 
C           END IF
          ENDIF

c split candidates--> to modify when spmd for edge
          !TAG_SCRATCH should has been reflush to zero
          CALL SPLIT_CAND_I25(PROC     , INTBUF_TAB(NI), NSN     , NSN_L       ,
     .                       TAG_SEGM2, II_STOK       , MULTIMP  , NCONT       ,
     .                       NOINT    , INACTI        ,TAG_SCRATCH ,II_STOK_L  ,
     .                       NINDX_SCRT,INDX_SCRT)
          CALL COPY_IVAL(II_STOK_L,1,1)
C         I_STOK(2:3) not needed into Restart file (null value can be written)
          IBID = 0
          CALL COPY_IVAL(IBID,1,1)
          IBID = 0
          CALL COPY_IVAL(IBID,1,1)



          IF(IEDGE/=0)THEN
            CALL COPY_IVAL(II_STOK_E_L,1,1) 
            CALL COPY_IVAL(II_STOK_S_L,1,1) 
          END IF

          DO I = 1,NEDGE_L
            ASSERT(TAG_EDGE(I) > 0)
          ENDDO
C
         IF(INTFRIC.GT.0)THEN
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IPARTFRICS,NSN_L,1,
     .                TAG_NODE_2RY) 
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IPARTFRICM,NRTM_L,1,
     .                TAG_SEGM) 
             IF(IEDGE/=0)THEN 
                CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IPARTFRIC_E,NEDGE_L,1,
     .                           TAG_EDGE)    
             ENDIF
            IF(INTBUF_FRIC_TAB(INTFRIC)%IORTHFRIC > 0) THEN
               CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IREP_FRICM,NRTM_L,1,
     .              TAG_SEGM)

             ENDIF
          ENDIF

          IF(FLAGREMNODE == 2) THEN  
            CALL SPLIT_REMNODE_I25(PROC     ,INTBUF_TAB(NI), NRTM    , NRTM_L,
     .                            TAG_SEGM2,NREMNODE_L      , NODLOCAL ,NREMNOR, 
     .                            NSN      , NSN_L , TAG_NODE_2RY2,ITAB,
     .                            NUMNOD_L )
           IF(IEDGE.GT.0) CALL SPLIT_REMNODE_I25_EDGE(PROC     , INTBUF_TAB, NEDGE    , NEDGE_L,
     .                            TAG_EDGE   ,TAG_EDGE2        , TAG_SEGM2 , ITAB     ,NUMNOD_L  ,  
     .                            IPARI_L(94,NI))
          ENDIF

C-------ILEV=2 for remove S1/S1 S2/S2 in case of S1/S2 input
          IF(ILEV==2)THEN
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%NBINFLG,NSN_L,1,
     .                           TAG_NODE_2RY) 
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%MBINFLG,NRTM_L,1,
     .                           TAG_SEGM) 
            IF(IEDGE.GT.0)THEN
             CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%EBINFLG,NEDGE_L,1,
     .                           TAG_EDGE) 

            END IF
          ENDIF

         IF(INTTH.GT.0)THEN
           CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IELEM,NRTM_L,1,
     .                         TAG_SEGM) 
         ENDIF

C---------MVOISIN
          CALL SPLIT_SEG_SEGLOC(INTBUF_TAB(NI)%MVOISIN,NRTM_L,4,
     .                          TAG_SEGM,TAG_SEGM2)
C----------MSEGLO : global number (should not change to local)
          CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%MSEGLO,NRTM_L,1,
     .                        TAG_SEGM) 
C----------MSEGTYP: seg type-------------------------
          CALL SPLIT_NODE_IVAL_I25(INTBUF_TAB(NI)%MSEGTYP24,NRTM,NRTM_L,
     .                        TAG_SEGM, TAG_SEGM2 ) 
C          
c         IF(INTPLY > 0)THEN 
c           CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%ISEG_PXFEM,NRTM_L,1,
c     .                          TAG_SEGM) !KD(40)
c           CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%ISEG_PLY,NRTM_L,12,
c     .                          TAG_SEGM) !KD(41)
c         ENDIF

          CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%ICONT_I,NSN_L,1,
     .                         TAG_NODE_2RY) 
C---------EVOISIN
          CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%EVOISIN,NRTM_L,4,
     .                        TAG_SEGM)
C----------ADMSR: 
          CALL SPLIT_SEG_NODLOC(INTBUF_TAB(NI)%ADMSR,NRTM_L,4,
     .                          TAG_SEGM,TAG_SM)

C----------LEDGE:
          CALL SPLIT_LEDGE_I25(NEDGE,
     .                        NEDGE_L,
     .                        INTBUF_TAB(NI)%IRECTM,
     .                        NRTM_L,
     .                        INTBUF_TAB(NI)%LEDGE,
     .                        INTBUF_TAB(NI)%MSEGLO,
     .                        INTBUF_TAB(NI)%ADMSR,
     .                        TAG_SEGM2,
     .                        TAG_SM,
     .                        NODLOCAL,
     .                        TAG_EDGE,
     .                        ITAB,
     .                        PROC)

C----------LBOUND:
          CALL SPLIT_LBOUND_I25(NADMSR,NADMSR_L,INTBUF_TAB(NI)%LBOUND,TAG_SM)

C----------No need to split ACTNOR (no need in Rfile) : Null values could be written.
          CALL COPY_IVAL(INTBUF_TAB(NI)%ACTNOR,NRTM_L,1)
C----------No need to split FARM (no need in Rfile) : Null values could be written.
c         CALL COPY_IVAL(INTBUF_TAB(NI)%FARM,4*MULTIMP*NCONT,1)
C----------ADSKYN, IADNOR: 
          CALL SPLIT_ADSKYN_25(
     .      INTBUF_TAB(NI)%ADSKYN,INTBUF_TAB(NI)%IADNOR,NADMSR,
     .                            INTBUF_TAB(NI)%ADMSR,NADMSR_L,
     .      NRTM_L,TAG_SEGM,TAG_SM,INTERCEP(1,NI))
C----------No need to split ISLIDE (no need in Rfile) : Null values could be written.
          CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%ISLIDE,NSN_L,4,
     .                         TAG_NODE_2RY) 
C----------write KNOR2MSR & NOR2MSR
          CALL COPY_IVAL(KNOR2MSR,NADMSR_L+1,1)
          CALL COPY_IVAL(NOR2MSR ,4*NRTM_L,1)
C----------No need to split CAND_OPT_N & CAND_OPT_E (no need in Rfile) : Null values could be written.
          CALL COPY_IVAL(INTBUF_TAB(NI)%CAND_OPT_N,MULTIMP*NCONT,1)
          CALL COPY_IVAL(INTBUF_TAB(NI)%CAND_OPT_E,MULTIMP*NCONT,1)
C---------Adhesion case
          IF(IVIS2.EQ.-1) THEN
            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IF_ADH,NSN_L,1,
     .                            TAG_NODE_2RY)
          ENDIF
C
          IF(IEDGE/=0)THEN

            CALL SPLIT_CAND_I25_EDGE(INTBUF_TAB(NI),TAG_SEGM2, PROC,NI,
     .                             TAG_EDGE,  NEDGE_L,  TAG_EDGE2, NEDGE,
     .                             II_STOK_E, II_STOK_E_L, TAG_II_E2E,
     .                             II_STOK_S, II_STOK_S_L, TAG_II_E2S)


          END IF

          IF(IEDGE/=0)THEN
            NCONTE = IPARI_L(88,NI)
            MULTIMPE = IPARI_L(87,NI)

            CALL SPLIT_CAND_IVAL(INTBUF_TAB(NI)%IFPEN_E,
     .                II_STOK_E_L,TAG_II_E2E,MULTIMPE,NCONTE)

c            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IFPEN_E,II_STOK_E_L,1,
c     .                            TAG_II_E2E) 

            CALL SPLIT_NODE_IVAL(INTBUF_TAB(NI)%IFPEN_E2S,II_STOK_S_L,1,
     .                            TAG_II_E2S) 
          ENDIF

c split and write on disk floating arrays 

          CALL COPY_RVAL(INTBUF_TAB(NI)%STFAC,1,1)  
          CALL COPY_RVAL(INTBUF_TAB(NI)%VARIABLES,NPARIR,1)   
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFNS,NSN_L,1,TAG_NODE_2RY)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFM,NRTM_L,1,TAG_SEGM)

          IF(IGAP.NE.0)THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_M,NRTM_L,1,TAG_SEGM)!JD(15)
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_S,NSN_L,1,TAG_NODE_2RY)!JD(16)
          ENDIF

          SIZ = INTBUF_TAB_L(NI)%S_XSAV
          CALL COPY_RVAL(INTBUF_TAB(NI)%XSAV,SIZ,1)

          SIZ = INTBUF_TAB_L(NI)%S_CRIT
          CALL COPY_RVAL(INTBUF_TAB(NI)%CRIT,SIZ,1)

          IF (MFROT.NE.0) THEN
            CALL COPY_RVAL(INTBUF_TAB(NI)%FRIC_P,10,1)
          ENDIF

          IF (IFQ.NE.0) THEN
            CALL COPY_RVAL(INTBUF_TAB(NI)%XFILTR,1,1)
          ENDIF

          IF(INTTH.GT.0.OR.IVIS2==-1) THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%AREAS,NSN_L,1,TAG_NODE_2RY)!JD(30)
          ENDIF

          IF(IGAP==3)THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_ML,NRTM_L,1,TAG_SEGM) 
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_SL,NSN_L,1,TAG_NODE_2RY) 
          ENDIF

C         IF(IEDGE/=0.AND.INACTI==5)THEN
          IF(IEDGE/=0)THEN
C           CALL COPY_RVAL(INTBUF_TAB(NI)%CAND_P ,INTBUF_TAB_L(NI)%S_CAND_P ,1) ! attention SPMD
C           CALL COPY_RVAL(INTBUF_TAB(NI)%CAND_PS,INTBUF_TAB_L(NI)%S_CAND_PS,1) ! attention SPMD
C           WRITE(6,*) __FILE__,__LINE__
            NCONTE = IPARI_L(88,NI)
            MULTIMPE = IPARI_L(87,NI)
C           WRITE(6,*) NI,"S_CAND_P=",MULTIMPE*NCONTE,INTBUF_TAB_L(NI)%S_CAND_P,INTBUF_TAB(NI)%S_CAND_P
 
            CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%CAND_P,
     .                II_STOK_E_L,TAG_II_E2E,MULTIMPE,NCONTE)

C           WRITE(6,*) NI,"S_CAND_PS=",II_STOK_S_L*4,INTBUF_TAB_L(NI)%S_CAND_PS
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%CAND_PS,II_STOK_S_L,4,
     .                            TAG_II_E2S) 

          END IF

          IF(IEDGE/=0)THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAPE,NEDGE_L,1,TAG_EDGE)
            IF(IGAP==3)THEN
              CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_E_L,NEDGE_L,1,TAG_EDGE)
            ENDIF
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STFE,NEDGE_L,1,TAG_EDGE)
          END IF

          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAPN_M,NMN_L,1,
     .                            TAG_NODE_MSR) !JD(22)


          CALL COPY_RVAL(INTBUF_TAB(NI)%SECND_FR,NSN_L,6)!JD(27)

          !PEN_OLD,PEN_INI for Inacti=5 
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%PENE_OLD,NSN_L,5,
     .                            TAG_NODE_2RY) !JD(31)

          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%STIF_OLD,NSN_L,2,
     .                            TAG_NODE_2RY) !JD(31)

          CALL COPY_RVAL(INTBUF_TAB(NI)%TIME_S,NSN_L,2)!JD(33)
          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAP_NM,NRTM_L,4,TAG_SEGM)!JD(34)

          IF(INTFRIC.GT.0)THEN
            IF(INTBUF_FRIC_TAB(INTFRIC)%IORTHFRIC > 0) THEN
               CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%DIR_FRICM,NRTM_L,2,
     .                TAG_SEGM)
            ENDIF
          ENDIF

          IF(ITHK == 1)THEN
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%GAPMSAV,NRTM_L,1,TAG_SEGM)
          ENDIF

          IF(IEDGE/=0)THEN
            NCONTE = IPARI_L(88,NI)
            MULTIMPE = IPARI_L(87,NI)

            CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%FTSAVX_E,
     .                II_STOK_E_L,TAG_II_E2E,MULTIMPE,NCONTE)
            CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%FTSAVY_E,
     .                II_STOK_E_L,TAG_II_E2E,MULTIMPE,NCONTE)
            CALL SPLIT_CAND_RVAL(INTBUF_TAB(NI)%FTSAVZ_E,
     .                II_STOK_E_L,TAG_II_E2E,MULTIMPE,NCONTE)

            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%FTSAVX_E2S,II_STOK_S_L,4,
     .                            TAG_II_E2S) 
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%FTSAVY_E2S,II_STOK_S_L,4,
     .                            TAG_II_E2S) 
            CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%FTSAVZ_E2S,II_STOK_S_L,4,
     .                            TAG_II_E2S) 


          END IF

c          CALL COPY_RVAL(INTBUF_TAB(NI)%PENM ,4*MULTIMP*NCONT,1)
c          CALL COPY_RVAL(INTBUF_TAB(NI)%DISTM,  MULTIMP*NCONT,1)
c          CALL COPY_RVAL(INTBUF_TAB(NI)%LBM  ,4*MULTIMP*NCONT,1)
c          CALL COPY_RVAL(INTBUF_TAB(NI)%LCM  ,4*MULTIMP*NCONT,1)
C----------No need for VTX_BISECTOR in restart file
C          CALL SPLIT_NODE_RVAL(INTBUF_TAB(NI)%VTX_BISECTOR,NADMSR_L,6,TAG_SM)
          DEALLOCATE(TAG_NODE_2RY,TAG_SEGM,TAG_SEGM2,
     .              TAG_NODE_MSR,TAG_II,TAG_SM,
     .              KNOR2MSR,NOR2MSR,TAG_NODE_2RY2)
          DEALLOCATE(TAG_EDGE)
          DEALLOCATE(TAG_EDGE2)
          IF(IEDGE /=0)  DEALLOCATE(TAG_II_E2S)
          IF(IEDGE /=0)  DEALLOCATE(TAG_II_E2E)
        ENDIF !ENDIF (ALL ITYP) 

!       ----------------------------------
!       Flush arrays to 0
        IF(NINDX_NM>0) TAG_NM(INDX_NM(1:NINDX_NM)) = 0
        NINDX_NM = 0
        IF(NINDX_SCRT>0) TAG_SCRATCH(INDX_SCRT(1:NINDX_SCRT)) = 0
        NINDX_SCRT = 0
        IF(FLAG_24_25.AND.(NINDX_NDLOCAL24>0)) NODLOCAL24( INDX_NDLOCAL24(1:NINDX_NDLOCAL24) ) = 0
        NINDX_NDLOCAL24 = 0
!       ----------------------------------

      ENDDO !END DO NI= 1, NINTER

      IF(FLAG_24_25) THEN
                DEALLOCATE( NODLOCAL24 )
                DEALLOCATE( INDX_NDLOCAL24 )
      ENDIF


      RETURN
      END
C
Chd|====================================================================
Chd|  W_TYPE8                       source/restart/ddsplit/split_interfaces.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        INT8_MOD                      ../common_source/modules/int8_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE W_TYPE8( PROC, T8) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE FRONT_MOD
      USE INTBUFDEF_MOD 
      USE INT8_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "analyse_name.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  :: PROC
      TYPE(INT8_STRUCT_) :: T8
C-----------------------------------------------

C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .         I,ITYP,NBS_INTERF,NBM_INTERF,NBT8,
     .         NI,K,P2

         CALL WRITE_I_C(T8%S_COMM,1)
C        WRITE(6,*) __FILE__,__LINE__,"SCOMM=",T8%S_COMM 

         DO K=1,T8%S_COMM 
          CALL WRITE_I_C(T8%SPMD_COMM_PATTERN(K)%NUMLOC,1)
          CALL WRITE_I_C(T8%SPMD_COMM_PATTERN(K)%UID,1)
                     
          CALL WRITE_I_C(T8%SPMD_COMM_PATTERN(K)%NBCOM,1)
          CALL WRITE_I_C(T8%SPMD_COMM_PATTERN(K)%PROCLIST,NSPMD)
          CALL WRITE_I_C(T8%SPMD_COMM_PATTERN(K)%BUF_INDEX,NSPMD)
         ENDDO
         DO P2=1,NSPMD
           IF(P2.NE.PROC + 1) THEN
             NBS_INTERF = T8%BUFFER(P2)%NBSECND_TOT
             NBM_INTERF = T8%BUFFER(P2)%NBMAIN
C            WRITE(6,*) __FILE__,__LINE__,P2,NBM_INTERF,NBS_INTERF 
          
            CALL WRITE_I_C(NBM_INTERF,1)
            CALL WRITE_I_C(NBS_INTERF,1)
           
            !nb secnds per MAIN
            IF(NBM_INTERF > 0) THEN
              CALL WRITE_I_C(T8%BUFFER(P2)%NBSECND,NBM_INTERF)
              CALL WRITE_I_C(T8%BUFFER(P2)%MAIN_ID,NBM_INTERF)
              CALL WRITE_I_C(T8%BUFFER(P2)%MAIN_UID,NBM_INTERF)
            ENDIF
           ELSE !IF P2 == PROC + 1 
            NBS_INTERF = 0                         
            NBM_INTERF = 0                      
            CALL WRITE_I_C(NBM_INTERF,1)
            CALL WRITE_I_C(NBS_INTERF,1)
           ENDIF
         ENDDO
       RETURN
       END SUBROUTINE
