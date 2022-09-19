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
C
Chd|====================================================================
Chd|  W_FRONT                       source/restart/ddsplit/w_front.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        PLIST_IFRONT                  source/spmd/node/ddtools.F    
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        FVBAG_MOD                     share/modules1/fvbag_mod.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        SPLIT_CFD_MOD                 share/modules1/split_cfd_mod.F
Chd|====================================================================
      SUBROUTINE W_FRONT(
     1   PROC    ,NBDDACC ,NBDDKIN ,NODLOCAL,
     2   NBDDPROC,NBDDBOUN,NODGLOB ,NUMNOD_L,NBDDNRB ,
     3   NPBY    ,LPBY    ,NPRW    ,LPRW    ,LEN_IA  ,
     4   DD_RBY2 ,ITABI2M ,NBDDI2M ,CEP     ,MONVOL  ,
     5   NNLINK  ,LLLINK  ,LJOINT  ,
     6   NBDDNCJ ,IBVEL   ,LBVEL   ,NBDDNRBM,DD_RBM2 ,
     7   NSTRF   ,NNODT_L ,NNODL_L ,IEXMAD  ,ISP0    ,
     8   NRCVVOIS,NSNDVOIS,NERVOIS ,NESVOIS ,
     9   NSEGFL_L,IPARG   ,
     A   NUMEL   ,ALE_CONNECTIVITY  ,NBCFD   ,IXS     ,IXQ     , IXTG,
     B   NUMELS_L,NUMELQ_L,NUMELTG_L,CEL     ,GEO     ,PORNOD  ,
     C   NUMPOR_L,NUMEL_L ,IPARI   ,INTBUF_TAB,NBI18_L ,
     D   IEXLNK  ,IGRNOD  ,DD_LAGF ,NLAGF_L ,IADLL   ,
     E   LLL     ,ISKWP   ,NSKWP   ,ISENSP  ,NSENSP  ,
     F   IACCP   ,NACCP   ,IRBE3   ,LRBE3   ,ITABRBE3M,
     G   NBDDRBE3M,IRBYM  ,LCRBYM  ,FRONT_RM ,DD_RBYM2,
     H   NBDDNRBYM,IRBE2  ,LRBE2   ,NBDDRBE2,ITABRBE2M,
     I   USERNODS,IEDGE_TMP,NODEDGE,EDGELOCAL,NBDDEDGE_L,
     J   IGAUP   ,NGAUP ,FRONTB_R2R,SDD_R2R_ELEM,ADDCSRECT,
     K   CSRECT,NBDDNORT ,NBDDNOR_MAX,NBCCFR25 ,NBCCNOR ,
     L   NUMNOR_L,NBDDEDGT,NBDDEDG_MAX,INTERCEP,NBDDCNDM,
     M   ITABCNDM,MULTI_FVM,IGRSURF,ISKWP_L,ALE_ELM,
     N   SIZE_ALE_ELM  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FVBAG_MOD
      USE FRONT_MOD
      USE INTBUFDEF_MOD
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
      USE SPLIT_CFD_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
#include      "lagmult.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
#include      "units_c.inc"
#include      "r2r_c.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER  PROC, NBDDACC, NBDDKIN, NBDDPROC, NBDDBOUN, NUMELS_L,
     .          NUMNOD_L, NBDDNRB, LEN_IA, NBDDI2M, NBDDNCJ,NBDDNRBM,
     .          NNODT_L, NNODL_L, ISP0, NRCVVOIS, NSNDVOIS, NERVOIS,
     .          NESVOIS, NSEGFL_L, NUMEL, NBCFD, NUMELQ_L,NUMELTG_L,
     .          NUMPOR_L, NUMEL_L, NBI18_L, INACTI, NLAGF_L,
     .          NBDDNORT, NBDDNOR_MAX, NBCCFR25, NBCCNOR, NUMNOR_L,
     .          NBDDEDGT,NBDDEDG_MAX,
     .          NODLOCAL(*), NODGLOB(*), NPBY(NNPBY,*),
     .          LPBY(*), NPRW(*), LPRW(*), DD_RBY2(3,NRBYKIN),
     .          ITABI2M(*), CEP(*), MONVOL(*),
     .          NNLINK(10,*), LLLINK(*), LJOINT(*),
     .          IBVEL(NBVELP,*), LBVEL(*), DD_RBM2(3,NIBVEL), NSTRF(*),
     .          IPARG(NPARG,*),
     .          IXS(NIXS,*), IXQ(NIXQ,*), IXTG(NIXTG, *),CEL(*), PORNOD(*),
     .          IPARI(NPARI,*), IEXLNK(NR2R,NR2RLNK),
     .          DD_LAGF(3,NSPMD+1), IADLL(*), LLL(*),
     .          ISKWP(*), NSKWP(*),IEXMAD(*),
     .          ISENSP(2,*), NSENSP(*), IACCP(*), NACCP(*),
     .          IRBE3(NRBE3L,*), LRBE3(*),ITABRBE3M(*),NBDDRBE3M,
     .          IRBYM(NIRBYM,*), DD_RBYM2(3,NRBYM), FRONT_RM(NRBYM,*),
     .          LCRBYM(*),NBDDNRBYM,IRBE2(NRBE2L,*), LRBE2(*),NBDDRBE2,
     .          ITABRBE2M(*),USERNODS(*),IEDGE_TMP(3,*),NODEDGE(2,*),
     .          EDGELOCAL(*),NBDDEDGE_L, IGAUP(*), NGAUP(*),
     .          FRONTB_R2R(SFRONTB_R2R,NSPMD),SDD_R2R_ELEM,
     .          ADDCSRECT(*), CSRECT(*),NBDDCNDM,ITABCNDM(*)
       my_real
     .          GEO(NPROPG,*)

       TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
       TYPE(INTERSURFP) :: INTERCEP(3,NINTER)
       TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
       TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
       TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
       INTEGER, DIMENSION(*), INTENT(IN) :: ISKWP_L
      INTEGER, INTENT(IN) :: SIZE_ALE_ELM
      TYPE(split_cfd_type), INTENT(IN) :: ALE_ELM
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, P, PP, IPACC, IPKIN, NBDDNOD, IS,ITY,
     .        MSR, NSL, PMAIN, ISLOCAL, IDEB, IFIN, OFFC, OFFTG, IAD,
     .        ISKIN, NBRB, J, K, KK, K0, K1, K2, K6, ISO, IMAX, ISBOUND,
     .        NBCJ, NSN, M, NN, N1, N2, N3, N4, NNOD, IFRAM, OFF, IP, IP0,
     .        NNOD_S, NSELS_S, NSELQ_S, NSELC_S, NSELT_S, NSELP_S,
     .        NSELR_S, NSELTG_S, NSINT_S, NNL_L, NNT_L, TYP, NN_L,NAD_L,
     .        ESHIFT, NMAD_L, NG, NFT, NEL, ILAW, JTUR, JTHE, ISOLNOD,
     .        IV, IE, PROC2, IE_LOC, NS_L, II, JJ, IG, NR_L, NF_L,
     .        IV_LOC, NW_L, ILW, NIMP, ILP, NBE, NAD, NRTS, NRTM,
     .        NNG, IADD, NOD,
     .        ISHIFT, LSHIFT, NADMSR, NADMSR_L, NI, NTY, NI25, NBDDNOR,
     .        LCSRECT_L, NBDDEDG, NRTM_L, IC, IK0, IKN, IK,IJK
      LOGICAL PSEARCH

      INTEGER SPLIST

      INTEGER, DIMENSION(:), ALLOCATABLE :: FR_USERWI,DD_R2R_ELEM
      INTEGER, DIMENSION(:), ALLOCATABLE :: PLIST
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_SM, TAG_MS
      INTEGER TAGP(NSPMD)
C
      INTEGER IAD_EDGE(NSPMD+1),LENR,TAG_EDGE,FRNODES,IED_GL,OK,IED_FR,
     . FR_EDGE_OLD,FR_EDGE0,FR_NBEDGE(NSPMD+1)
      INTEGER, DIMENSION(:), ALLOCATABLE :: FR_EDGE,TAG_IED_FR
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TAG_IED_FR0
!
      INTEGER, DIMENSION(:), ALLOCATABLE :: WEIGHT,TAGE,NEWFRONT,TAG,TAGER,
     .                    TAGES
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TAGE_L,TAG_L
      INTEGER :: IAD1, LGTH
C---------------------------------------------------------------------



      INTEGER ::  COMPTR
      INTEGER ::  COMPTS
      INTEGER ::  IERR
      INTEGER ::  IFV
      INTEGER ::  L
      INTEGER ::  LEN_USERW
      INTEGER ::  NB_FREDGE
      INTEGER ::  NL
      INTEGER ::  NUSERWI_L
      INTEGER ::  OFFSET
      INTEGER ::  SHIFT_EDG
      INTEGER ::  SOLV
      INTEGER ::  NS
      INTEGER, DIMENSION(:), ALLOCATABLE :: ACCKIN  !(NBDDACC+NBDDKIN)
      INTEGER, DIMENSION(:), ALLOCATABLE :: ADDCSRECT_L  !(NUMNOR_L+1)
      INTEGER, DIMENSION(:), ALLOCATABLE :: CPULOCALER  !(NERVOIS)
      INTEGER, DIMENSION(:), ALLOCATABLE :: CPULOCALES  !(NESVOIS)
      INTEGER, DIMENSION(:), ALLOCATABLE :: CPULOCALF  !(NSEGFL_L)
      INTEGER, DIMENSION(:), ALLOCATABLE :: CPULOCALR  !(NRCVVOIS)
      INTEGER, DIMENSION(:), ALLOCATABLE :: CPULOCALS  !(NSNDVOIS)
      INTEGER, DIMENSION(:), ALLOCATABLE :: DD_CJ  !(NBDDNCJ)
      INTEGER, DIMENSION(:), ALLOCATABLE :: DD_CNDM  !(NBDDCNDM)
      INTEGER, DIMENSION(:), ALLOCATABLE :: DD_CUT  !(NNODT_L)
      INTEGER, DIMENSION(:), ALLOCATABLE :: DD_ELEM  !(NBDDACC+NBDDKIN)
      INTEGER, DIMENSION(:), ALLOCATABLE :: DD_I2M  !(NBDDI2M)
      INTEGER, DIMENSION(:), ALLOCATABLE :: DD_P  !(NSPMD)
      INTEGER, DIMENSION(:), ALLOCATABLE :: DD_RBE2  !(NBDDRBE2)
      INTEGER, DIMENSION(:), ALLOCATABLE :: DD_RBE3M  !(NBDDRBE3M)
      INTEGER, DIMENSION(:), ALLOCATABLE :: DD_RBM  !(NBDDNRBM)
      INTEGER, DIMENSION(:), ALLOCATABLE :: DD_RBY  !(NBDDNRB)
      INTEGER, DIMENSION(:), ALLOCATABLE :: DD_RBYM  !(NBDDNRBYM)
      INTEGER, DIMENSION(:), ALLOCATABLE :: DP_RBE3M  !(NBDDRBE3M)
      INTEGER, DIMENSION(:), ALLOCATABLE :: D_RBY  !(NSPMD+1)
      INTEGER, DIMENSION(:), ALLOCATABLE :: FR_NOR  !(NBDDNORT)
      INTEGER, DIMENSION(:), ALLOCATABLE :: IADUSERW  !(NSPMD+1)
      INTEGER, DIMENSION(:), ALLOCATABLE :: IADUSERW2  !(NSPMD+1)
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAD_CNDM  !(NSPMD+1)
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAD_I2M  !(NSPMD+1)
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAD_RBE2  !(NSPMD+1)
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAD_RBE3  !(NSPMD+1)
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAD_RBE3M  !(NSPMD+1)
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAD_RBM  !(NSPMD+1)
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAD_RBY  !(NSPMD+1)
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAD_RBYM  !(NSPMD+1)
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX  !(2*(NBDDACC+NBDDKIN))
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX2  !(2*(NBDDNRB))
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX3  !(2*NBDDNCJ)
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX4  !(2*NBCFD)
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX5  !(2*(NBDDNRBYM))
      INTEGER, DIMENSION(:), ALLOCATABLE :: ISOM  !(NSPMD)
      INTEGER, DIMENSION(:), ALLOCATABLE :: ISOM_R2R_R  !(NSPMD)
      INTEGER, DIMENSION(:), ALLOCATABLE :: ISOM_R2R_S  !(NSPMD)
      INTEGER, DIMENSION(:), ALLOCATABLE :: LERCVOIS  !(NERVOIS)
      INTEGER, DIMENSION(:), ALLOCATABLE :: LESDVOIS  !(NESVOIS)
      INTEGER, DIMENSION(:), ALLOCATABLE :: LLAGF  !(NLAGF_L)
      INTEGER, DIMENSION(:), ALLOCATABLE :: LNODPOR  !(NUMPOR_L)
      INTEGER, DIMENSION(:), ALLOCATABLE :: LNRCVOIS  !(NRCVVOIS)
      INTEGER, DIMENSION(:), ALLOCATABLE :: LNSDVOIS  !(NSNDVOIS)
      INTEGER, DIMENSION(:), ALLOCATABLE :: LSEGCOM  !(NSEGFL_L)
      INTEGER, DIMENSION(:), ALLOCATABLE :: NBRCVOIS  !(NSPMD+1)
      INTEGER, DIMENSION(:), ALLOCATABLE :: NBSDVOIS  !(NSPMD+1)
      INTEGER, DIMENSION(:), ALLOCATABLE :: NERCVOIS  !(NSPMD+1)
      INTEGER, DIMENSION(:), ALLOCATABLE :: NESDVOIS  !(NSPMD+1)
      INTEGER, DIMENSION(:), ALLOCATABLE :: NPORGEO  !(NUMGEO)
      INTEGER, DIMENSION(:), ALLOCATABLE :: NPSEGCOM  !(NSPMD+1)
      INTEGER, DIMENSION(:), ALLOCATABLE :: PROCNOR  !(ADDCSRECT(NUMNOR+1))
      INTEGER, DIMENSION(:), ALLOCATABLE :: PROC_REM  !(NBDDACC+NBDDKIN)
      INTEGER, DIMENSION(:), ALLOCATABLE :: PROC_REM1  !(NBDDNRBYM)
      INTEGER, DIMENSION(:), ALLOCATABLE :: RG_CUT  !(NNODL_L)
      INTEGER, DIMENSION(:), ALLOCATABLE :: SECVU  !(NSPMD)
      INTEGER, DIMENSION(:), ALLOCATABLE :: WORK  !(70000)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: DD_I18  !(NSPMD+2,NBI18_L)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: DD_LL  !(NSPMD+2,NLINK)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: DD_MAD  !(5,NSPMD+1)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: DD_MV  !(NSPMD+2,NVOLU)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: DD_R2R  !(NSPMD+1,NL_DDR2R)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: DD_SEC  !(NSPMD+1,NSECT)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: DD_WALL  !(NSPMD+2,NRWALL)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: FR_EDG  !(2,NBDDEDGT)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: FR_SAV  !(2,NBDDEDG_MAX)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IAD_CJ  !(NSPMD+1,NJOINT)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IAD_CUT  !(NSPMD+2,NSECT*ISECUT*ISP0)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IAD_ELEM  !(2,NSPMD+1)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IAD_FREDG  !(NINTER25,NSPMD+1)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IAD_FRNOR  !(NINTER25,NSPMD+1)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IAD_RBM2  !(4,NSPMD+1)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IAD_RBY2  !(4,NSPMD+1)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IAD_RBYM2  !(4,NSPMD+1)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IAD_SEC  !(4,NSPMD+1)
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX25  !(2*MAX(NBDDNOR_MAXNBDDEDG_MAX))
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITRI  !(3,NBDDACC+NBDDKIN)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITRI2  !(2,NBDDNRB)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITRI25  !(3,MAX(NBDDNOR_MAX,NBDDEDG_MAX))
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITRI3  !(2,NBDDNCJ)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITRI4  !(2,NBCFD)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITRI5  !(2,NBDDNRBYM)
      INTEGER, DIMENSION(:), ALLOCATABLE :: PROC_REM25  !(MAX(NBDDNOR_MAX,NBDDEDG_MAX))
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TAGS  !(NSPMD,SEGINDX)
      INTEGER, DIMENSION(:), ALLOCATABLE :: PROC_REM_CYL_JOINT

      ALLOCATE(ACCKIN(NBDDACC+NBDDKIN))
      ALLOCATE(ADDCSRECT_L(NUMNOR_L+1))
      ALLOCATE(CPULOCALER(NERVOIS))
      ALLOCATE(CPULOCALES(NESVOIS))
      ALLOCATE(CPULOCALF(NSEGFL_L))
      ALLOCATE(CPULOCALR(NRCVVOIS))
      ALLOCATE(CPULOCALS(NSNDVOIS))
      ALLOCATE(DD_CJ(NBDDNCJ))
      ALLOCATE(DD_CNDM(NBDDCNDM))
      ALLOCATE(DD_CUT(NNODT_L))
      ALLOCATE(DD_ELEM(NBDDACC+NBDDKIN))
      ALLOCATE(DD_I2M(NBDDI2M))
      ALLOCATE(DD_P(NSPMD))
      ALLOCATE(DD_RBE2(NBDDRBE2))
      ALLOCATE(DD_RBE3M(NBDDRBE3M))
      ALLOCATE(DD_RBM(NBDDNRBM))
      ALLOCATE(DD_RBY(NBDDNRB))
      ALLOCATE(DD_RBYM(NBDDNRBYM))
      ALLOCATE(DP_RBE3M(NBDDRBE3M))
      ALLOCATE(D_RBY(NSPMD+1))
      ALLOCATE(FR_NOR(NBDDNORT))
      ALLOCATE(IADUSERW(NSPMD+1))
      ALLOCATE(IADUSERW2(NSPMD+1))
      ALLOCATE(IAD_CNDM(NSPMD+1))
      ALLOCATE(IAD_I2M(NSPMD+1))
      ALLOCATE(IAD_RBE2(NSPMD+1))
      ALLOCATE(IAD_RBE3(NSPMD+1))
      ALLOCATE(IAD_RBE3M(NSPMD+1))
      ALLOCATE(IAD_RBM(NSPMD+1))
      ALLOCATE(IAD_RBY(NSPMD+1))
      ALLOCATE(IAD_RBYM(NSPMD+1))
      ALLOCATE(INDEX(2*(NBDDACC+NBDDKIN)))
      ALLOCATE(INDEX2(2*(NBDDNRB)))
      ALLOCATE(INDEX3(2*NBDDNCJ))
      ALLOCATE(INDEX4(2*NBCFD))
      ALLOCATE(INDEX5(2*(NBDDNRBYM)))
      ALLOCATE(ISOM(NSPMD))
      ALLOCATE(ISOM_R2R_R(NSPMD))
      ALLOCATE(ISOM_R2R_S(NSPMD))
      ALLOCATE(LERCVOIS(NERVOIS))
      ALLOCATE(LESDVOIS(NESVOIS))
      ALLOCATE(LLAGF(NLAGF_L))
      ALLOCATE(LNODPOR(NUMPOR_L))
      ALLOCATE(LNRCVOIS(NRCVVOIS))
      ALLOCATE(LNSDVOIS(NSNDVOIS))
      ALLOCATE(LSEGCOM(NSEGFL_L))
      ALLOCATE(NBRCVOIS(NSPMD+1))
      ALLOCATE(NBSDVOIS(NSPMD+1))
      ALLOCATE(NERCVOIS(NSPMD+1))
      ALLOCATE(NESDVOIS(NSPMD+1))
      ALLOCATE(NPORGEO(NUMGEO))
      ALLOCATE(NPSEGCOM(NSPMD+1))
      ALLOCATE(PROCNOR(ADDCSRECT(NUMNOR+1)))
      ALLOCATE(PROC_REM(NBDDACC+NBDDKIN))
      ALLOCATE(PROC_REM1(NBDDNRBYM))
      ALLOCATE(RG_CUT(NNODL_L))
      ALLOCATE(SECVU(NSPMD))
      ALLOCATE(WORK(70000))
      ALLOCATE(DD_I18(NSPMD+2,NBI18_L))
      ALLOCATE(DD_LL(NSPMD+2,NLINK))
      ALLOCATE(DD_MAD(5,NSPMD+1))
      ALLOCATE(DD_MV(NSPMD+2,NVOLU))
      ALLOCATE(DD_R2R(NSPMD+1,NL_DDR2R))
      ALLOCATE(DD_SEC(NSPMD+1,NSECT))
      ALLOCATE(DD_WALL(NSPMD+2,NRWALL))
      ALLOCATE(FR_EDG(2,NBDDEDGT))
      ALLOCATE(FR_SAV(2,NBDDEDG_MAX))
      ALLOCATE(IAD_CJ(NSPMD+1,NJOINT))
      ALLOCATE(IAD_CUT(NSPMD+2,NSECT*ISECUT*ISP0))
      ALLOCATE(IAD_ELEM(2,NSPMD+1))
      ALLOCATE(IAD_FREDG(NINTER25,NSPMD+1))
      ALLOCATE(IAD_FRNOR(NINTER25,NSPMD+1))
      ALLOCATE(IAD_RBM2(4,NSPMD+1))
      ALLOCATE(IAD_RBY2(4,NSPMD+1))
      ALLOCATE(IAD_RBYM2(4,NSPMD+1))
      ALLOCATE(IAD_SEC(4,NSPMD+1))
      ALLOCATE(INDEX25(2*MAX(NBDDNOR_MAX,NBDDEDG_MAX)))
      ALLOCATE(ITRI(3,NBDDACC+NBDDKIN))
      ALLOCATE(ITRI2(2,NBDDNRB))
      ALLOCATE(ITRI25(3,MAX(NBDDNOR_MAX,NBDDEDG_MAX)))
      ALLOCATE(ITRI3(2,NBDDNCJ))
      ALLOCATE(ITRI4(2,NBCFD))
      ALLOCATE(ITRI5(2,NBDDNRBYM))
      ALLOCATE(PROC_REM25(MAX(NBDDNOR_MAX,NBDDEDG_MAX)))
      ALLOCATE(TAGS(NSPMD,SEGINDX))
      ALLOCATE( WEIGHT(NUMNOD_L),TAGE(NUMEL) )
      ALLOCATE( NEWFRONT(NINTER),TAG(NUMNOD) )
      ALLOCATE( TAGER(NERVOIS),TAGES(NESVOIS) )
      ALLOCATE( TAGE_L(NSPMD,NUMEL_L),TAG_L(NSPMD,NUMNOD_L) )
! -----------------------------------

C
C Domdec pure
C
      NBDDNOD = 0
      NBDDBOUN= 0
      ISBOUND = 0
      ALLOCATE(PLIST(NSPMD))
      PLIST(1:NSPMD) = -1

      DO II = 1, NUMNOD_L
        I = NODGLOB(II)
          ISBOUND = 0
          SPLIST=0
          CALL PLIST_IFRONT(PLIST,I,SPLIST)
         !returns in "PLIST" array list of SPMD domains on which node I is sticked
         !SPLITS is the number of SPMD domains on which node I is sticked
          DO J=1,SPLIST
            P = PLIST(J)
            IF(P/=PROC)THEN
              NBDDNOD = NBDDNOD + 1
              DD_ELEM(NBDDNOD) = I
              PROC_REM(NBDDNOD) = P
              !FLAGKIN array identies boundary nodes with kinematic constraints
              !(FLAGKIN(N)=1 <=> old FRONT TAG=10)
              !FLAGKIN(N) can be set to one only for first SPMD domain
              IF(FLAGKIN(I)==1.AND.(PROC==1.OR.P==1))THEN
                ACCKIN(NBDDNOD) = 1
              ELSE
                ACCKIN(NBDDNOD) = 0
              ENDIF
              ISBOUND = 1
            ENDIF
          ENDDO
          NBDDBOUN = NBDDBOUN + ISBOUND
      ENDDO

      DEALLOCATE(PLIST)
C
      DO I = 1, NBDDNOD
        ITRI(1,I) =  PROC_REM(I)
        ITRI(2,I) =  ACCKIN(I)
        ITRI(3,I) =  DD_ELEM(I)
        INDEX(I) = I
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI,INDEX,NBDDNOD,3)
      DO I = 1, NBDDNOD
        PROC_REM(I)= ITRI(1,INDEX(I))
        ACCKIN(I)  = ITRI(2,INDEX(I))
        DD_ELEM(I) = NODLOCAL(ITRI(3,INDEX(I)))
      ENDDO
C
      DO P = 1, NSPMD
        ISOM(P) = 0
      ENDDO
      DO I = 1, NBDDNOD
        P =  PROC_REM(I)
        ISOM(P) = ISOM(P) + 1
      ENDDO
      IAD_ELEM(1,1) = 1
      IAD_ELEM(2,1) = 1
      DO P = 1, NSPMD
        IAD_ELEM(1,P+1) = IAD_ELEM(1,P) + ISOM(P)
        IAD_ELEM(2,P+1) = IAD_ELEM(1,P+1)
      ENDDO
C
C-- Domdec Multidomaines hmpp      

      IF ((NSUBDOM>0).AND.(IDDOM==0)) THEN
        COMPTS = 0
        COMPTR = 0
        ISOM_R2R_S = 0
        ISOM_R2R_R = 0
        DO I = 1, NBDDNOD
          P =  PROC_REM(I)
          N = NODGLOB(DD_ELEM(I))
          IF (FRONTB_R2R(N,PROC)==-1) THEN
            IF (FRONTB_R2R(N,P)>0) THEN
             ISOM_R2R_S(P) = ISOM_R2R_S(P) + 1
             COMPTS = COMPTS + 1
            ENDIF
          ELSEIF (FRONTB_R2R(N,PROC)>0) THEN
            IF (FRONTB_R2R(N,P)==-1) THEN
              ISOM_R2R_R(P) = ISOM_R2R_R(P) + 1
              COMPTR = COMPTR + 1
            ENDIF
          ENDIF
        ENDDO
        DD_R2R(1,1) = 1
        DD_R2R(1,2) = 1
        DO P = 1, NSPMD
          DD_R2R(P+1,1) = DD_R2R(P,1) + ISOM_R2R_S(P)
          DD_R2R(P+1,2) = DD_R2R(P,2) + ISOM_R2R_R(P)
        ENDDO
        ALLOCATE (DD_R2R_ELEM(SDD_R2R_ELEM),STAT=IERR)
        IF (IERR /= 0) THEN
          WRITE(IOUT,*)' ** ERROR IN MEMORY ALLOCATION'
          WRITE(ISTDO,*)' ** ERROR IN MEMORY ALLOCATION'
          CALL ARRET(2)
        ENDIF
        OFFSET = COMPTS
        COMPTR = 0
        COMPTS = 0
        DO I = 1, NBDDNOD
          N = NODGLOB(DD_ELEM(I))
          P =  PROC_REM(I)
          IF (FRONTB_R2R(N,PROC)==-1) THEN
            IF (FRONTB_R2R(N,P)>0) THEN
              COMPTS = COMPTS+1
              DD_R2R_ELEM(COMPTS) = DD_ELEM(I)
            ENDIF
          ELSEIF (FRONTB_R2R(N,PROC)>0) THEN
            IF (FRONTB_R2R(N,P)==-1) THEN
              COMPTR = COMPTR+1
              DD_R2R_ELEM(OFFSET+COMPTR) = DD_ELEM(I)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
      IF (NSPMD>1.AND.(NBDDACC+NBDDKIN)>0) THEN
       DO P = 1, NSPMD
         I = IAD_ELEM(1,P)
         ISKIN = 0
         DO WHILE (ISKIN==0.AND.I<IAD_ELEM(1,P+1))
           ISKIN = ACCKIN(I)
           I = I + 1
         ENDDO
         IF(ISKIN==1) THEN
               IAD_ELEM(2,P) = I-1
         ELSE
               IAD_ELEM(2,P) = IAD_ELEM(1,P+1)
             END IF
       ENDDO
       IAD_ELEM(2,NSPMD+1) = IAD_ELEM(1,NSPMD+1)
      ENDIF
      NBDDPROC = 0
      DO P = 1, NSPMD
        IF(ISOM(P)>0)NBDDPROC=NBDDPROC+1
      ENDDO
C
      DO I = 1, NUMNOD_L
        WEIGHT(I) = 0
        N = NODGLOB(I)
        DO P = 1, PROC-1
          IF(NLOCAL(N,P)==1)GOTO 10
        ENDDO
        WEIGHT(I) = 1
 10     CONTINUE
      ENDDO
C
C RBY specifique (POFF)
C
      NBRB = 0
      DO N = 1, NRBYKIN
        M=NPBY(1,N)
        IF(NLOCAL(M,PROC)==1)THEN
          DO P = 1, NSPMD
            IF(P/=PROC) THEN
              IF(NLOCAL(M,P)==1) THEN
                NBRB = NBRB + 1
                DD_RBY(NBRB) = M
                PROC_REM(NBRB) = P
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C

      DO I = 1, NBRB
        ITRI2(1,I) =  PROC_REM(I)
        ITRI2(2,I) =  DD_RBY(I)
        INDEX2(I) = I
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI2,INDEX2,NBRB,2)
      DO I = 1, NBRB
        PROC_REM(I)= ITRI2(1,INDEX2(I))
        DD_RBY(I) = NODLOCAL(ITRI2(2,INDEX2(I)))
      ENDDO
C
      DO P = 1, NSPMD
        ISOM(P) = 0
      ENDDO
      DO I = 1, NBRB
        P =  PROC_REM(I)
        ISOM(P) = ISOM(P) + 1
      ENDDO
      IAD_RBY(1) = 1
      DO P = 1, NSPMD
        IAD_RBY(P+1) = IAD_RBY(P) + ISOM(P)
      ENDDO
C
C
C
C
C Rigid material  specifique (POFF)
C

      NBRB = 0
      DO N = 1, NRBYM
        M=IRBYM(1,N)
        IF(MOD(FRONT_RM(M,PROC),10)==1)THEN
          DO P = 1, NSPMD
            IF(P/=PROC) THEN
              IF(MOD(FRONT_RM(M,P),10)==1) THEN
                NBRB = NBRB + 1
                DD_RBYM(NBRB) = M
                PROC_REM1(NBRB) = P
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
      DO I = 1, NBRB
        ITRI5(1,I) =  PROC_REM1(I)
        ITRI5(2,I) =  DD_RBYM(I)
        INDEX5(I) = I
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI5,INDEX5,NBRB,2)
      DO I = 1, NBRB
        PROC_REM1(I)= ITRI5(1,INDEX5(I))
        DD_RBYM(I) =  ITRI5(2,INDEX5(I))
      ENDDO
C
      DO P = 1, NSPMD
        ISOM(P) = 0
      ENDDO
      DO I = 1, NBRB
        P =  PROC_REM1(I)
        ISOM(P) = ISOM(P) + 1
      ENDDO
      IAD_RBYM(1) = 1
      DO P = 1, NSPMD
        IAD_RBYM(P+1) = IAD_RBYM(P) + ISOM(P)
      ENDDO
C    
C RWALL specifique type sliding
C
      K = 0
      DO N = 1, NRWALL
        DO P = 1, NSPMD+2
          DD_WALL(P,N) = 0
        ENDDO
        N3 = 2*NRWALL+N
        NSL=NPRW(N)
        MSR = NPRW(N3)
        IF(MSR/=0) THEN
          ISO = 0
          IMAX = 0
          PMAIN = 1
          DO P = 1, NSPMD
            IF(NLOCAL(MSR,P)==1) THEN
              DO KK = 1, NSL
                NN = LPRW(K+KK)
                IF(NLOCAL(NN,P)==1)THEN
                  DD_WALL(P,N) = DD_WALL(P,N) + 1
                ENDIF
              ENDDO
              ISO = ISO + DD_WALL(P,N)
              IF(DD_WALL(P,N)>IMAX)THEN
                PMAIN = P
                IMAX = DD_WALL(P,N)
              ELSEIF(DD_WALL(P,N)==0)THEN
C cas noeud MSR sur proc sans noeud secnd
                DD_WALL(P,N) = -1
                if (NSL==0) PMAIN = P
              ENDIF
            ENDIF
          ENDDO
          DD_WALL(NSPMD+1,N) = NSL
          DD_WALL(NSPMD+2,N) = PMAIN
        ENDIF
        K = K + NSL
      ENDDO
C
C RBY Kin specifique rigid body (PON)
C
      DO P = 1, NSPMD+1
        IAD_RBY2(1,P) = 0
        IAD_RBY2(2,P) = 0
        IAD_RBY2(3,P) = 0
        IAD_RBY2(4,P) = 0
      ENDDO
      K = 0
      DO N = 1, NRBYKIN
        MSR = NPBY(1,N)
        NSL = NPBY(2,N)
C
        IMAX = 0
        PMAIN = 1
        DO P = 1, NSPMD
          DD_P(P) = 0
          IF(NLOCAL(MSR,P)==1) THEN
            DO KK = 1, NSL
              NN = LPBY(K+KK)
              IF(NLOCAL(NN,P)==1)THEN
                IF(IFRONT%P(1,IFRONT%IENTRY(NN)) >= P)  DD_P(P) = DD_P(P) + 1
              ENDIF
            ENDDO
            IF(DD_P(P)>IMAX)THEN
              PMAIN = P
              IMAX = DD_P(P)
            ENDIF
          ELSE
            DD_P(P) = -1
          ENDIF
        ENDDO
C
        IF(NLOCAL(MSR,PROC)==1) THEN
          DD_RBY2(1,N) = DD_P(PROC)
          DD_RBY2(2,N) = NSL
          DD_RBY2(3,N) = PMAIN
          IF(PMAIN/=PROC) THEN
            IAD_RBY2(1,PMAIN) = IAD_RBY2(1,PMAIN) + DD_P(PROC)
            IAD_RBY2(4,PMAIN) = IAD_RBY2(4,PMAIN) + 1
          ELSE
            DO P = 1, NSPMD
              IF(P/=PROC) THEN
                IF(DD_P(P)/=-1) THEN
                  IAD_RBY2(2,P) = IAD_RBY2(2,P) + DD_P(P)
                  IAD_RBY2(3,P) = IAD_RBY2(3,P) + 1
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ELSE
C cas aucun noeuds de rby
          DD_RBY2(1,N) = 0
          DD_RBY2(2,N) = 0
          DD_RBY2(3,N) = PMAIN
        ENDIF
C
        ISLOCAL = 0
        DO P = 1, NSPMD
          IF(P/=PMAIN) THEN
            IF(DD_P(P)/=-1) THEN
              ISLOCAL = 1
            ENDIF
          ENDIF
        ENDDO
Cel tag DD_RBY(3) a negatif pour reperage des rb sans echange.
        IF(ISLOCAL==0) DD_RBY2(3,N) = -PMAIN
C
        K = K + NSL
      ENDDO
C
C comptage des mains
C
      DO P = 1, NSPMD
        DD_P(P) = 0
      ENDDO
      DO N = 1, NRBYKIN
        PMAIN = DD_RBY2(3,N)
C si rby decoupe
        IF(PMAIN>0) DD_P(PMAIN) = DD_P(PMAIN)+1
      ENDDO
      DO P = 1, NSPMD
        IF(IAD_RBY2(3,P)/=0)THEN
          IAD_RBY2(3,P) = DD_P(PROC)
        ENDIF
        IF(IAD_RBY2(4,P)/=0)THEN
          IAD_RBY2(4,P) = DD_P(P)
        ENDIF
      ENDDO
C
      DO P = 1, NSPMD
        IAD_RBY2(1,NSPMD+1) = IAD_RBY2(1,NSPMD+1) + IAD_RBY2(1,P)
        IAD_RBY2(2,NSPMD+1) = IAD_RBY2(2,NSPMD+1) + IAD_RBY2(2,P)
        IAD_RBY2(3,NSPMD+1) = IAD_RBY2(3,NSPMD+1) + IAD_RBY2(3,P)
        IAD_RBY2(4,NSPMD+1) = IAD_RBY2(4,NSPMD+1) + IAD_RBY2(4,P)
      END DO
C
C Rigid material  specifique rigid body (PON)
C
      DO P = 1, NSPMD+1
        IAD_RBYM2(1,P) = 0
        IAD_RBYM2(2,P) = 0
        IAD_RBYM2(3,P) = 0
        IAD_RBYM2(4,P) = 0
      ENDDO
      K = 0
C
      DO N = 1, NRBYM
        MSR = IRBYM(1,N)
        NSL = IRBYM(2,N)
        IMAX = 0
        PMAIN = 1
        DO P = 1, NSPMD
          DD_P(P) = 0
          IF(MOD(FRONT_RM(MSR,P),10)==1) THEN
            DO KK = 1, NSL
              NN = LCRBYM(K+KK)
              IF(NLOCAL(NN,P)==1)THEN
                DO PP = 1, P-1
                  IF(NLOCAL(NN,PP)==1) THEN
                     GOTO 111
                  ENDIF
                ENDDO
                DD_P(P) = DD_P(P) + 1
 111            CONTINUE
              ENDIF
            ENDDO
            IF(DD_P(P)>IMAX)THEN
              PMAIN = P
              IMAX = DD_P(P)
            ENDIF
          ELSE
            DD_P(P) = -1
          ENDIF
        ENDDO
C
C
        IF(MOD(FRONT_RM(MSR,PROC),10)==1) THEN
          DD_RBYM2(1,N) = DD_P(PROC)
          DD_RBYM2(2,N) = NSL
          DD_RBYM2(3,N) = PMAIN

          IF(PMAIN/=PROC) THEN
            IAD_RBYM2(1,PMAIN) = IAD_RBYM2(1,PMAIN) + DD_P(PROC)
            IAD_RBYM2(4,PMAIN) = IAD_RBYM2(4,PMAIN) + 1
          ELSE
            DO P = 1, NSPMD
              IF(P/=PROC) THEN
                IF(DD_P(P)/=-1) THEN
                  IAD_RBYM2(2,P) = IAD_RBYM2(2,P) + DD_P(P)
                  IAD_RBYM2(3,P) = IAD_RBYM2(3,P) + 1
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ELSE
C cas aucun noeuds rigid material
          DD_RBYM2(1,N) = 0
          DD_RBYM2(2,N) = 0
          DD_RBYM2(3,N) = PMAIN
        ENDIF
C
        ISLOCAL = 0
        DO P = 1, NSPMD
          IF(P/=PMAIN) THEN
            IF(DD_P(P)/=-1) THEN
              ISLOCAL = 1
            ENDIF
          ENDIF
        ENDDO

Cel tag DD_RBY(3) a negatif pour reperage des r material sans echange.
        IF(ISLOCAL==0) DD_RBYM2(3,N) = -PMAIN
        K = K + NSL
      ENDDO
C
C comptage des mains
C
      DO P = 1, NSPMD
        DD_P(P) = 0
      ENDDO
      DO N = 1, NRBYM
        PMAIN = DD_RBYM2(3,N)
C si rigid material decoupe
        IF(PMAIN>0) DD_P(PMAIN) = DD_P(PMAIN)+1
      ENDDO
      DO P = 1, NSPMD
        IF(IAD_RBYM2(3,P)/=0)THEN
          IAD_RBYM2(3,P) = DD_P(PROC)
        ENDIF
        IF(IAD_RBYM2(4,P)/=0)THEN
          IAD_RBYM2(4,P) = DD_P(P)
        ENDIF
      ENDDO
C
      DO P = 1, NSPMD
        IAD_RBYM2(1,NSPMD+1) = IAD_RBYM2(1,NSPMD+1) + IAD_RBYM2(1,P)
        IAD_RBYM2(2,NSPMD+1) = IAD_RBYM2(2,NSPMD+1) + IAD_RBYM2(2,P)
        IAD_RBYM2(3,NSPMD+1) = IAD_RBYM2(3,NSPMD+1) + IAD_RBYM2(3,P)
        IAD_RBYM2(4,NSPMD+1) = IAD_RBYM2(4,NSPMD+1) + IAD_RBYM2(4,P)
      END DO
C
C Specifique interface type 2 : on utilise les frontieres  
      DO P = 1, NSPMD+1
        IAD_I2M(P) = 0
      ENDDO
      IF(NBDDI2M>0) THEN
        N = 1
        DO P = 1, NSPMD
          IDEB = IAD_ELEM(1,P)
          IFIN = IAD_ELEM(1,P+1)-1
          IAD_I2M(P) = N
          DO I = IDEB, IFIN
            IF(ITABI2M(DD_ELEM(I))==1) THEN
              DD_I2M(N) = DD_ELEM(I)
              N = N + 1
            ENDIF
          ENDDO
        ENDDO
        IAD_I2M(NSPMD+1) = N
        if(n-1/=NBDDI2M)then
            print*,'error decomp I2',n-1,NBDDI2M
        endif
      ENDIF
C
C MV specifique
C
      OFFC = NUMELS+NUMELQ
      OFFTG =NUMELS+NUMELQ+ NUMELC+NUMELT+NUMELP+NUMELR
      K1 = 1
      K6 = 0
      IFV= 0
      DO N = 1, NVOLU
        DO P = 1, NSPMD+2
          DD_MV(P,N) = 0
        ENDDO
C
        IS = MONVOL(K1+3)
        NN   = IGRSURF(IS)%NSEG
        DO J = 1, NN
          ITY = IGRSURF(IS)%ELTYP(J)
          I   = IGRSURF(IS)%ELEM(J)
          IF (ITY==3) THEN
            P = CEP(I+OFFC) + 1
            DD_MV(P,N) = DD_MV(P,N) + 1
          ELSEIF (ITY==7) THEN
            P = CEP(I+OFFTG) + 1
            DD_MV(P,N) = DD_MV(P,N) + 1
          ELSE
          END IF
        END DO
        IMAX = 0
        PMAIN = 1
        DO P = 1, NSPMD
          IF(DD_MV(P,N)>IMAX)THEN
            PMAIN = P
            IMAX = DD_MV(P,N)
          END IF
        END DO
        DD_MV(NSPMD+1,N) = NN
        DD_MV(NSPMD+2,N) = PMAIN
C
c specific case for FVMBAG AND FVMBAG1, get PMAIN directly
c from PMAIN computed in C_FVBAG
        ITY=MONVOL(K1-1+2)
        IF (ITY==6.OR.ITY==8) THEN
          IFV = IFV+1
          PMAIN = FVSPMD(IFV)%PMAIN
          DD_MV(NSPMD+2,N) = PMAIN
        ENDIF

        K1 = K1 + NIMV
        K6 = K6 + NN
      ENDDO
C
C RLink specifique
C
      K = 0
      DO I = 1, NLINK
        DO P = 1, NSPMD+2
          DD_LL(P,I) = 0
        ENDDO
C
        NN = NNLINK(1,I)
        DO P = 1, NSPMD
          DO J = 1, NN
            N = LLLINK(K+J)
            IF (NLOCAL(N,P)==1)THEN
              DD_LL(P,I) = DD_LL(P,I) + 1
            ENDIF
          END DO
        END DO
        K = K + NN
C
        IMAX = 0
        PMAIN = 1
        DO P = 1, NSPMD
          IF(DD_LL(P,I)>IMAX)THEN
            PMAIN = P
            IMAX = DD_LL(P,I)
          END IF
        END DO
        DD_LL(NSPMD+1,I) = NN
        DD_LL(NSPMD+2,I) = PMAIN
C
      ENDDO
C
C Cyl. JOINT specifique
C
      NBCJ = 0
      K = 1
      DO N = 1, NJOINT
        NSN=LJOINT(K)
        NBCJ = NBCJ + NSN
        K = K + NSN + 1
      ENDDO
      ALLOCATE( PROC_REM_CYL_JOINT(NBCJ) )
      K = 1
      NAD = 1
      DO N = 1, NJOINT
        NBCJ = 0
        NSN=LJOINT(K)
        DO J = 1, NSN
          M = LJOINT(K+J)
          IF(PROC/=1) THEN
C proc <> 0, frontiere si noeud sur le proc
            IF(NLOCAL(M,PROC)==1)THEN
              NBCJ = NBCJ + 1
              DD_CJ(NAD+NBCJ-1) = M
              PROC_REM_CYL_JOINT(NBCJ) = 1
            END IF
          ELSE
C proc = 0, recherche des autres procs ayant le noeud
            DO P = 2, NSPMD
              IF(NLOCAL(M,P)==1) THEN
                NBCJ = NBCJ + 1
                DD_CJ(NAD+NBCJ-1) = M
                PROC_REM_CYL_JOINT(NBCJ) = P
              ENDIF
            END DO
          END IF
        END DO
C
      DO I = 1, NBCJ
        ITRI3(1,I) =  PROC_REM_CYL_JOINT(I)
          ITRI3(2,I) =  DD_CJ(NAD+I-1)
        INDEX3(I) = I
      ENDDO
      CALL MY_ORDERS(0,WORK,ITRI3,INDEX3,NBCJ,2)
      DO I = 1, NBCJ
          PROC_REM_CYL_JOINT(I)= ITRI3(1,INDEX3(I))
          DD_CJ(NAD+I-1) = NODLOCAL(ITRI3(2,INDEX3(I)))
      ENDDO
C
        DO P = 1, NSPMD
        ISOM(P) = 0
      ENDDO
      DO I = 1, NBCJ
        P =  PROC_REM_CYL_JOINT(I)
        ISOM(P) = ISOM(P) + 1
      ENDDO
        IAD_CJ(1,N) = NAD
        DO P = 1, NSPMD
          IAD_CJ(P+1,N) = IAD_CJ(P,N) + ISOM(P)
        ENDDO
C
        NAD = NAD + IAD_CJ(NSPMD+1,N) - IAD_CJ(1,N)
        K = K + NSN + 1
      ENDDO
      DEALLOCATE( PROC_REM_CYL_JOINT )
C
C RBY MOU specifique (POFF)
C
      NBRB = 0
      IAD_RBM(1) = 1
      DO P = 1, NSPMD
        IF (P/=PROC) THEN
          DO N = 1, NIBVEL
            M=IBVEL(4,N)
             IF(NLOCAL(M,PROC)==1.AND.
     +         NLOCAL(M,P)==1) THEN
              NBRB = NBRB + 1
              DD_RBM(NBRB) = N
            END IF
          END DO
        END IF
        IAD_RBM(P+1) = NBRB+1
      END DO
C
C RBM specifique (PON)
C
      DO P = 1, NSPMD+1
        IAD_RBM2(1,P) = 0
        IAD_RBM2(2,P) = 0
        IAD_RBM2(3,P) = 0
        IAD_RBM2(4,P) = 0
      ENDDO
      K = 0
      DO N = 1, NIBVEL
        NSL = IBVEL(3,N)
        MSR = IBVEL(4,N)
C
        IMAX = 0
        PMAIN = 1
        DO P = 1, NSPMD
          DD_P(P) = 0
          IF(NLOCAL(MSR,P)==1) THEN
            DO KK = 1, NSL
              NN = LBVEL(K+KK)
              IF(NLOCAL(NN,P)==1)THEN
                DO PP = 1, P-1
                  IF(NLOCAL(NN,PP)==1) THEN
                     GOTO 1000
                  ENDIF
                ENDDO
                DD_P(P) = DD_P(P) + 1
 1000           CONTINUE
              ENDIF
            ENDDO
            IF(DD_P(P)>IMAX)THEN
              PMAIN = P
              IMAX = DD_P(P)
            ENDIF
          ELSE
            DD_P(P) = -1
          ENDIF
        ENDDO
C
        IF(NLOCAL(MSR,PROC)==1) THEN
          DD_RBM2(1,N) = DD_P(PROC)
          DD_RBM2(2,N) = NSL
          DD_RBM2(3,N) = PMAIN
          IF(PMAIN/=PROC) THEN
            IAD_RBM2(1,PMAIN) = IAD_RBM2(1,PMAIN) + DD_P(PROC)
            IAD_RBM2(4,PMAIN) = IAD_RBM2(4,PMAIN) + 1
          ELSE
            DO P = 1, NSPMD
              IF(P/=PROC) THEN
                IF(DD_P(P)/=-1) THEN
                  IAD_RBM2(2,P) = IAD_RBM2(2,P) + DD_P(P)
                  IAD_RBM2(3,P) = IAD_RBM2(3,P) + 1
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ELSE
C cas aucun noeuds de rby
          DD_RBM2(1,N) = 0
          DD_RBM2(2,N) = 0
          DD_RBM2(3,N) = PMAIN
        ENDIF
C
        ISLOCAL = 0
        DO P = 1, NSPMD
          IF(P/=PMAIN) THEN
            IF(DD_P(P)/=-1) THEN
              ISLOCAL = 1
            ENDIF
          ENDIF
        ENDDO
Cel tag DD_RBY(3) a negatif pour reperage des rb sans echange.
        IF(ISLOCAL==0) DD_RBM2(3,N) = -PMAIN
C
        K = K + NSL
      ENDDO
C
C comptage des mains
C
      DO P = 1, NSPMD
        DD_P(P) = 0
      ENDDO
      DO N = 1, NIBVEL
        PMAIN = DD_RBM2(3,N)
C si rbm decoupe
        IF(PMAIN>0) DD_P(PMAIN) = DD_P(PMAIN)+1
      ENDDO
      DO P = 1, NSPMD
        IF(IAD_RBM2(3,P)/=0)THEN
          IAD_RBM2(3,P) = DD_P(PROC)
        ENDIF
        IF(IAD_RBM2(4,P)/=0)THEN
          IAD_RBM2(4,P) = DD_P(P)
        ENDIF
      ENDDO
C
      DO P = 1, NSPMD
        IAD_RBM2(1,NSPMD+1) = IAD_RBM2(1,NSPMD+1) + IAD_RBM2(1,P)
        IAD_RBM2(2,NSPMD+1) = IAD_RBM2(2,NSPMD+1) + IAD_RBM2(2,P)
        IAD_RBM2(3,NSPMD+1) = IAD_RBM2(3,NSPMD+1) + IAD_RBM2(3,P)
        IAD_RBM2(4,NSPMD+1) = IAD_RBM2(4,NSPMD+1) + IAD_RBM2(4,P)
      ENDDO
C
C RBE3 same than int2
C    
      DO P = 1, NSPMD+1
        IAD_RBE3M(P) = 0
      ENDDO
      IF(NBDDRBE3M>0) THEN
        N = 1
        DO P = 1, NSPMD
          IDEB = IAD_ELEM(1,P)
          IFIN = IAD_ELEM(1,P+1)-1
          IAD_RBE3M(P) = N
          DO I = IDEB, IFIN
            IF(ITABRBE3M(DD_ELEM(I))>0) THEN
              DD_RBE3M(N) = DD_ELEM(I)
              DP_RBE3M(N) = ITABRBE3M(DD_ELEM(I))
              N = N + 1
            ENDIF
          ENDDO
        ENDDO
        IAD_RBE3M(NSPMD+1) = N
        if(n-1/=NBDDRBE3M)then
            print*,'error decomp RBE3',n-1,NBDDRBE3M
        endif
      ENDIF
C
C RBE2
C
      DO P = 1, NSPMD+1
        IAD_RBE2(P) = 1
      ENDDO
      IF(NBDDRBE2>0) THEN
        N = 1
        DO P = 1, NSPMD
          IDEB = IAD_ELEM(1,P)
          IFIN = IAD_ELEM(1,P+1)-1
          IAD_RBE2(P) = N
          DO I = IDEB, IFIN
            IF(ITABRBE2M(DD_ELEM(I))>0) THEN
              DD_RBE2(N) = ITABRBE2M(DD_ELEM(I))
              N = N + 1
            ENDIF
          ENDDO
        ENDDO
        IAD_RBE2(NSPMD+1) = N
        if(n-1/=NBDDRBE2)then
            print*,'error decomp RBE2',n-1,NBDDRBE2
        endif
      ENDIF
C
C Itet=2 of Tetra10 same than int2
C    
      DO P = 1, NSPMD+1
        IAD_CNDM(P) = 0
      ENDDO
      IF(NBDDCNDM>0) THEN
        N = 1
        DO P = 1, NSPMD
          IDEB = IAD_ELEM(1,P)
          IFIN = IAD_ELEM(1,P+1)-1
          IAD_CNDM(P) = N
          DO I = IDEB, IFIN
            IF(ITABCNDM(DD_ELEM(I))>0) THEN
               DD_CNDM(N) = DD_ELEM(I)
c              DD_CNDM(N) = ITABCNDM(DD_ELEM(I))
               N = N + 1
            ENDIF
          ENDDO
        ENDDO
        IAD_CNDM(NSPMD+1) = N
        if(n-1/=NBDDCNDM)then
            print*,'error decomp Itet2 of S10',n-1,NBDDCNDM
        endif
      ENDIF
C
C Section specifique
C
      NNL_L = 0
      NNT_L = 0
      IF (NSECT>0)  K0=NSTRF(25)
      DO I = 1, NSECT
        DO P = 1, NSPMD+1
          DD_SEC(P,I) = 0
        ENDDO
        IF(ISECUT*ISP0==1)THEN
          DO P = 1, NSPMD+2
            IAD_CUT(P,I) = 0
          ENDDO
        END IF
C
        TYP= NSTRF(K0)
        N1 = NSTRF(K0+3)
        N2 = NSTRF(K0+4)
        N3 = NSTRF(K0+5)
        NNOD = NSTRF(K0+6)
        IFRAM = NSTRF(K0+26)
        K2 = K0+30+NSTRF(K0+14)
        IF (IFRAM<=10.OR.N1/=0) THEN
          IF(N1>0) THEN
            DO P = 1, NSPMD
              IF (NLOCAL(N1,P)==1)THEN
                DD_SEC(P,I) = DD_SEC(P,I) + 1
                GOTO 2001
              END IF
            END DO
 2001       CONTINUE
          END IF
          IF(N2>0) THEN
            DO P = 1, NSPMD
              IF (NLOCAL(N2,P)==1)THEN
                DD_SEC(P,I) = DD_SEC(P,I) + 1
                GOTO 2002
              END IF
            END DO
 2002       CONTINUE
          END IF
          IF(N3>0) THEN
            DO P = 1, NSPMD
              IF (NLOCAL(N3,P)==1)THEN
                DD_SEC(P,I) = DD_SEC(P,I) + 1
                GOTO 2003
              END IF
            END DO
 2003       CONTINUE
          END IF
        END IF
        IF(MOD(IFRAM,10)==1.OR.MOD(IFRAM,10)==2) THEN
          DO P = 1, NSPMD
            SECVU(P) = 0
          END DO
          DO NN = 1, NNOD
            DO P = 1, NSPMD
              IF (NLOCAL(NSTRF(K2+NN-1),P)==1)THEN
                SECVU(P) = 1
                GOTO 2004
              END IF
            END DO
 2004       CONTINUE
          END DO
          DO P = 1, NSPMD
            DD_SEC(P,I) = DD_SEC(P,I) + SECVU(P)
          END DO
        END IF
C cut
        IF(ISECUT==1) THEN
         IF (TYP>=1)THEN
          DO NN = 1, NNOD
            IF (NLOCAL(NSTRF(K2+NN-1),PROC)==1)THEN
              NNL_L = NNL_L + 1
              RG_CUT(NNL_L) = NN
            END IF
          END DO
         ENDIF
          IF(PROC==1) THEN
            IF(TYP>=1) THEN
              DO P = 1, NSPMD
                NN_L = 0
                DO NN = 1, NNOD
                  K = NSTRF(K2+NN-1)
                  IF(NLOCAL(K,P)==1)THEN
                    NN_L = NN_L + 1
                    DD_CUT(NNT_L+NN_L) = NN
                  END IF
                END DO
                NNT_L = NNT_L + NN_L
                IAD_CUT(P,I) = NN_L
                IAD_CUT(NSPMD+1,I) =  IAD_CUT(NSPMD+1,I)+NN_L
              END DO
            END IF
            IAD_CUT(NSPMD+2,I) = NNOD
          END IF
        END IF
C
        K0=NSTRF(K0+24)
C
        IMAX = 0
        PMAIN = 1
        DO P = 1, NSPMD
          IF(DD_SEC(P,I)>IMAX)THEN
            PMAIN = P
            IMAX = DD_SEC(P,I)
          END IF
        END DO
        DD_SEC(NSPMD+1,I) = PMAIN
      END DO
C
      DO P = 1, NSPMD+1
        IAD_SEC(1,P) = 0
        IAD_SEC(2,P) = 0
        IAD_SEC(3,P) = 0
        IAD_SEC(4,P) = 0
      ENDDO
C noeuds N1, N2, N3 NNODE a echanger
      DO I = 1, NSECT
        PMAIN = DD_SEC(NSPMD+1,I)
        IF(PROC==PMAIN) THEN
          DO P = 1, NSPMD
            IF(P/=PMAIN) THEN
              IAD_SEC(2,P) = IAD_SEC(2,P) + DD_SEC(P,I)
            END IF
          END DO
        ELSE
          IAD_SEC(1,PMAIN) = IAD_SEC(1,PMAIN) + DD_SEC(PROC,I)
        END IF
      END DO
C renvoi des X  N1, N2, N3 NNODE aux procs possedant des elements de sections
      DO P = 1, NSPMD
        DD_P(P) = 0
      ENDDO
C
      IP = 30
      DO I = 1, NSECT
        N1 = NSTRF(IP+4)
        N2 = NSTRF(IP+5)
        N3 = NSTRF(IP+6)
        IFRAM = NSTRF(IP+27)
        NNOD_S  = NSTRF(IP+7)
        NSELS_S = NSTRF(IP+8)
        NSELQ_S = NSTRF(IP+9)
        NSELC_S = NSTRF(IP+10)
        NSELT_S = NSTRF(IP+11)
        NSELP_S = NSTRF(IP+12)
        NSELR_S = NSTRF(IP+13)
        NSELTG_S= NSTRF(IP+14)
        NSINT_S = NSTRF(IP+15)
        IP = IP + 30 + NSINT_S + NNOD_S
        IP0 = IP
        PMAIN = DD_SEC(NSPMD+1,I)
C determination du nombre de noeuds a echange
        DD_P(PMAIN) = DD_P(PMAIN) + 1
        IF(PROC==PMAIN) THEN
          DO P = 1, NSPMD
            IP = IP0
            IF(P/=PMAIN) THEN
              IMAX = 0
              OFF = 0
              DO J = 1, NSELS_S
                K = NSTRF(IP + J*2 - 1)
                IF(CEP(K+OFF)+1==P) IMAX = 1
              END DO
              IF(IMAX==1) GO TO 3000
              IP = IP + 2*NSELS_S
              OFF = OFF+NUMELS
              DO J = 1, NSELQ_S
                K = NSTRF(IP + J*2 - 1)
                IF(CEP(K+OFF)+1==P) IMAX = 1
              END DO
              IF(IMAX==1) GO TO 3000
              IP = IP + 2*NSELQ_S
              OFF = OFF+NUMELQ

              DO J = 1, NSELC_S
                K = NSTRF(IP + J*2 - 1)
                IF(CEP(K+OFF)+1==P) IMAX = 1
              END DO
              IF(IMAX==1) GO TO 3000
              IP = IP + 2*NSELC_S
              OFF = OFF + NUMELC
              DO J = 1, NSELT_S
                K = NSTRF(IP + J*2 - 1)
                IF(CEP(K+OFF)+1==P) IMAX = 1
              END DO
              IF(IMAX==1) GO TO 3000
              IP = IP + 2*NSELT_S
              OFF = OFF + NUMELT
              DO J = 1, NSELP_S
                K = NSTRF(IP + J*2 - 1)
                IF(CEP(K+OFF)+1==P) IMAX = 1
              END DO
              IF(IMAX==1) GO TO 3000
              IP = IP + 2*NSELP_S
              OFF = OFF + NUMELP
              DO J = 1, NSELR_S
                K = NSTRF(IP + J*2 - 1)
                IF(CEP(K+OFF)+1==P) IMAX = 1
              END DO
              IF(IMAX==1) GO TO 3000
              IP = IP + 2*NSELR_S
              OFF = OFF + NUMELR
              DO J = 1, NSELTG_S
                K = NSTRF(IP + J*2 - 1)
                IF(CEP(K+OFF)+1==P) IMAX = 1
              END DO
              IP = IP + 2*NSELTG_S
 3000         CONTINUE
C si element de la section present sur le proc
c              IF(IMAX==1) THEN
              IF(IMAX==1.OR.ISECUT==1) THEN
c                IAD_SEC(3,P) = IAD_SEC(3,P) + NN
                IAD_SEC(3,P) = IAD_SEC(3,P) + 1
              END IF
            END IF
          END DO
        ELSE
          IP = IP0
          IMAX = 0
          OFF = 0
          DO J = 1, NSELS_S
            K = NSTRF(IP + J*2 - 1)
            IF(CEP(K+OFF)+1==PROC) IMAX = 1
          END DO
          IF(IMAX==1) GO TO 4000
          IP = IP + 2*NSELS_S
          OFF = OFF + NUMELS
          DO J = 1, NSELQ_S
            K = NSTRF(IP + J*2 - 1)
            IF(CEP(K+OFF)+1==PROC) IMAX = 1
          END DO
          IF(IMAX==1) GO TO 4000
          IP = IP + 2*NSELQ_S
          OFF = OFF + NUMELQ
          DO J = 1, NSELC_S
            K = NSTRF(IP + J*2 - 1)
            IF(CEP(K+OFF)+1==PROC) IMAX = 1
          END DO
          IF(IMAX==1) GO TO 4000
          IP = IP + 2*NSELC_S
          OFF = OFF + NUMELC
          DO J = 1, NSELT_S
            K = NSTRF(IP + J*2 - 1)
            IF(CEP(K+OFF)+1==PROC) IMAX = 1
          END DO
          IF(IMAX==1) GO TO 4000
          IP = IP + 2*NSELT_S
          OFF = OFF + NUMELT
          DO J = 1, NSELP_S
            K = NSTRF(IP + J*2 - 1)
            IF(CEP(K+OFF)+1==PROC) IMAX = 1
          END DO
          IF(IMAX==1) GO TO 4000
          IP = IP + 2*NSELP_S
          OFF = OFF + NUMELP
          DO J = 1, NSELR_S
            K = NSTRF(IP + J*2 - 1)
            IF(CEP(K+OFF)+1==PROC) IMAX = 1
          END DO
          IF(IMAX==1) GO TO 4000
          IP = IP + 2*NSELR_S
          OFF = OFF + NUMELR
          DO J = 1, NSELTG_S
            K = NSTRF(IP + J*2 - 1)
            IF(CEP(K+OFF)+1==PROC) IMAX = 1
          END DO
          IP = IP + 2*NSELTG_S
 4000     CONTINUE
C si element de la section present sur le proc
          IF(IMAX==1.OR.ISECUT==1) THEN
            IAD_SEC(4,PMAIN) = IAD_SEC(4,PMAIN) + 1

          END IF
        END IF
        IP = IP0 + 2*(NSELS_S+NSELQ_S+NSELC_S+
     +                NSELT_S+NSELP_S+NSELR_S+NSELTG_S)
      END DO
C
      DO P = 1, NSPMD
        IF(IAD_SEC(3,P)/=0)THEN
          IAD_SEC(3,P) = DD_P(PROC)
        ENDIF
        IF(IAD_SEC(4,P)/=0)THEN
          IAD_SEC(4,P) = DD_P(P)
        ENDIF
      ENDDO
C
      DO P = 1, NSPMD
        IAD_SEC(1,NSPMD+1) = IAD_SEC(1,NSPMD+1) + IAD_SEC(1,P)
        IAD_SEC(2,NSPMD+1) = IAD_SEC(2,NSPMD+1) + IAD_SEC(2,P)
        IAD_SEC(3,NSPMD+1) = IAD_SEC(3,NSPMD+1) + IAD_SEC(3,P)
        IAD_SEC(4,NSPMD+1) = IAD_SEC(4,NSPMD+1) + IAD_SEC(4,P)
      ENDDO
C   
C Couplage Madymo
C
        DO I = 1, NSPMD+1
          DD_MAD(1,I) = 0
          DD_MAD(2,I) = 0
          DD_MAD(3,I) = 0
          DD_MAD(4,I) = 0
          DD_MAD(5,I) = 0
        END DO
C
        DD_MAD(5,1) = NCONX
        DD_MAD(5,NSPMD+1) = NCONX
C
        IF(NEXMAD/=0) THEN
C
C ELEM shell4
C
          DO P = 1, NSPMD
            IDEB = 1 + 7*NCONX + NMADPRT
            ESHIFT = NUMELS+NUMELQ
            NMAD_L = 0
            DO I = 1, NMADSH4
              K = IEXMAD(IDEB+I-1)
              IF(CEP(K+ESHIFT)==P-1) THEN
                NMAD_L = NMAD_L+1
              END IF
            END DO
            DD_MAD(1,P) = NMAD_L
C
C Elem shell3
C
            IDEB = IDEB + NMADSH4
            ESHIFT = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR
            NMAD_L = 0
            DO I = 1, NMADSH3
              K = IEXMAD(IDEB+I-1)
              IF(CEP(K+ESHIFT)==P-1) THEN
                NMAD_L = NMAD_L+1
              END IF
            END DO
            DD_MAD(2,P) = NMAD_L
C
C Elem solides
C
            IDEB = IDEB + NMADSH3
            ESHIFT = 0
            NMAD_L = 0
            DO I = 1, NMADSOL
              K = IEXMAD(IDEB+I-1)
              IF(CEP(K+ESHIFT)==P-1) THEN
                NMAD_L = NMAD_L+1
              END IF
            END DO
            DD_MAD(3,P) = NMAD_L
C
C Noeuds
C
            IDEB = IDEB + NMADSOL
            NMAD_L = 0
            DO I = 1, NMADNOD
              K = IEXMAD(IDEB+I-1)
              IF(NLOCAL(K,P)==1) THEN
                DO PP = 1, P-1
                  IF(NLOCAL(K,PP)==1) GOTO 888
                END DO
                NMAD_L = NMAD_L+1
              END IF
 888          CONTINUE
            END DO
            DD_MAD(4,P) = NMAD_L
          END DO
          DD_MAD(1,NSPMD+1) = NMADSH4
          DD_MAD(2,NSPMD+1) = NMADSH3
          DD_MAD(3,NSPMD+1) = NMADSOL
          DD_MAD(4,NSPMD+1) = NMADNOD
        END IF
C
C NEWFRONT
C
      DO I = 1, NINTER
        NEWFRONT(I) = 0
      END DO
C
C Frontieres CFD
C    
      IF(IALE+IEULER+ITHERM+IALELAG/=0)THEN
C
C Partie nodale LNRCVOIS
C
        NR_L = 0
        NS_L = 0
        NF_L = 0
        II = 0
        JJ = 0
        DO P = 1, NSPMD+1
          NBRCVOIS(P) = 0
          NPSEGCOM(P) = 0
          NBSDVOIS(P) = 0
          NERCVOIS(P) = 0
          NESDVOIS(P) = 0
        END DO
        DO I = 1, NUMNOD
          TAG(I) = 0
        END DO
        DO I = 1, NUMEL
          TAGE(I) = 0
        END DO
C
        DO P = 1, NSPMD
          IF(P/=PROC)THEN
            DO I = 1, NUMEL_L
              TAGE_L(P,I) = 0
            END DO
            DO I = 1, NUMNOD_L
              TAG_L(P,I) = 0
            END DO
          END IF
          DO I = 1, SEGINDX
            TAGS(P,I) = 0
          END DO
        END DO
C
        DO NG=1,NGROUP
          JTUR=IPARG(12,NG)
          JTHE=IPARG(13,NG)
Cel test agrad0
          IF(IPARG(32,NG)+1==PROC) THEN
            NEL = IPARG(2,NG)
            NFT = IPARG(3,NG)
            ITY = IPARG(5,NG)
Cel voir autres types solides
            ISOLNOD = IPARG(28,NG)
Cel 3D
            IF(ITY==1) THEN
              DO I = 1, NEL
                IE = I+NFT
                IE_LOC = CEL(IE)
Cel IVOIS
                IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
                LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(IE+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
                DO J = 1, LGTH
                  IV = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J - 1)
                  IF (IV>0) THEN
                    PROC2  = CEP(IV)+1
                    IF(PROC2/=PROC) THEN
                      IF(TAGE(IV)==0) THEN
                        II = II + 1
                        TAGER(II)=IV
                        CPULOCALER(II)=PROC2
C no elt frontiere > no elt interne
                        LERCVOIS(II) = NUMELS_L+II
                        TAGE(IV) = PROC2
                        NERCVOIS(PROC2) = NERCVOIS(PROC2)+1
                      END IF
                      IF(TAGE_L(PROC2,IE_LOC)==0)THEN
                        JJ = JJ+1
                        TAGES(JJ)=IE
                        CPULOCALES(JJ)=PROC2
                        LESDVOIS(JJ) = IE_LOC
                        TAGE_L(PROC2,IE_LOC)=PROC2
                        NESDVOIS(PROC2) = NESDVOIS(PROC2)+1
                      END IF
                    END IF
                  ELSEIF(PROC/=1.AND.IV<0)THEN
C IV < 0 : SEGMENT INT12
                    IF(TAGS(1,-IV)==0)THEN
                      NF_L = NF_L + 1
                      LSEGCOM(NF_L) = -IV
                      CPULOCALF(NF_L)=1
                      TAGS(1,-IV) = 1
                      NPSEGCOM(1) = NPSEGCOM(1)+1
                    END IF
                  END IF
                END DO
              END DO
Cel 2D
            ELSEIF(ITY==2) THEN
              DO I = 1, NEL
                IE = I+NFT
                IE_LOC = CEL(IE)
                IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
                LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(IE+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
Cel IVOIS
                DO J = 1, LGTH
                  IV = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J - 1)
                  IF (IV>0) THEN
                    PROC2  = CEP(IV)+1
                    IF(PROC2/=PROC) THEN
                      IF(TAGE(IV)==0) THEN
                        II = II + 1
                        TAGER(II)=IV
                        CPULOCALER(II)=PROC2
C no elt frontiere > no elt interne
                        LERCVOIS(II) = NUMELQ_L+II
                        TAGE(IV) = PROC2
                        NERCVOIS(PROC2) = NERCVOIS(PROC2)+1
                      END IF
                      IF(TAGE_L(PROC2,IE_LOC)==0)THEN
                        JJ = JJ+1
                        TAGES(JJ)=IE
                        CPULOCALES(JJ)=PROC2
                        LESDVOIS(JJ) = IE_LOC
                        TAGE_L(PROC2,IE_LOC)=PROC2
                        NESDVOIS(PROC2) = NESDVOIS(PROC2)+1
                      END IF
                    END IF
                  END IF
                END DO
              END DO
Cel 2D -> triangles
            ELSEIF(ITY==7 .AND. (N2D /= 0 .AND. MULTI_FVM%IS_USED)) THEN
              DO I = 1, NEL
                IE = I+NFT
                IE_LOC = CEL(IE)
                IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
                LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(IE+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
Cel IVOIS
                DO J = 1, LGTH
                  IV = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J - 1)
                  IF (IV>0) THEN
                    PROC2  = CEP(IV)+1
                    IF(PROC2/=PROC) THEN
                      IF(TAGE(IV)==0) THEN
                        II = II + 1
                        TAGER(II)=IV
                        CPULOCALER(II)=PROC2
C no elt frontiere > no elt interne
                        LERCVOIS(II) = NUMELTG_L+II
                        TAGE(IV) = PROC2
                        NERCVOIS(PROC2) = NERCVOIS(PROC2)+1
                      END IF
                      IF(TAGE_L(PROC2,IE_LOC)==0)THEN
                        JJ = JJ+1
                        TAGES(JJ)=IE
                        CPULOCALES(JJ)=PROC2
                        LESDVOIS(JJ) = IE_LOC
                        TAGE_L(PROC2,IE_LOC)=PROC2
                        NESDVOIS(PROC2) = NESDVOIS(PROC2)+1
                      END IF
                    END IF
                  END IF
                END DO
              END DO
            END IF
C
          ELSEIF(SEGINDX>0.AND.PROC==1)THEN      ! IPARG(32,NG)+1/=PROC
            NEL = IPARG(2,NG)
            NFT = IPARG(3,NG)
            ITY = IPARG(5,NG)
            P = IPARG(32,NG)+1
            IF(P/=PROC) THEN
Cel voir autres types solides
             ISOLNOD = IPARG(28,NG)
Cel 3D
             IF(ITY==1) THEN
              DO I = 1, NEL
                IE = I+NFT
Cel IVOIS
                IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
                LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(IE+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
                DO J = 1, LGTH
                  IV = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J - 1)
                  IF(IV<0)THEN
C IV < 0 : SEGMENT INT12
                    IF(TAGS(P,-IV)==0)THEN
                      NF_L = NF_L + 1
                      LSEGCOM(NF_L) = -IV
                      CPULOCALF(NF_L)=P
                      TAGS(P,-IV) = 1
                      NPSEGCOM(P) = NPSEGCOM(P)+1
                    END IF
                  END IF
                END DO
              END DO
             END IF
            END IF
          END IF
        END DO
C
C
C Traitement supplementaire Couplage Fluide/Structure
C
!       Don't use here PLIST_IFRONT instead of NLOCAL
!       because it increases cpu time
        DO I=1,NUMELS
          DO J=1,8
             NS = IXS(j+1,I)
             IF ( (NODLOCAL(NS)/=0).AND.(NODLOCAL(NS)<=NUMNOD_L) ) THEN
                IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(I)
                LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(I+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(I)
               DO K=1,LGTH
                  IE = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + K - 1)
                  IF (IE>0) THEN
                   DO L = 1,8
                     N = IXS(L+1,IE)
                     IF(TAG(N)==0) THEN
                      IF ( (NODLOCAL(N)==0).OR.(NODLOCAL(N)>NUMNOD_L) ) THEN
                       NR_L = NR_L + 1
                       LNRCVOIS(NR_L) = N
                       TAG(N) = 1

                       PSEARCH = .TRUE.
                       IAD = IFRONT%IENTRY(N)
                       IF(IAD <= 0) PSEARCH = .FALSE.
C                      loop over all processors that have node N
C                      Two iterations max because of GOTO 
                       DO WHILE(PSEARCH)
                        PROC2 = IFRONT%P(1,IAD)
                        IF(PROC2/=PROC)THEN
                          CPULOCALR(NR_L) = PROC2
                          NBRCVOIS(PROC2) = NBRCVOIS(PROC2)+1
                          PSEARCH = .FALSE.
                        ENDIF ! proc2/=proc
                        IF(IFRONT%P(2,IAD)==0) PSEARCH = .FALSE.
                        IAD = IFRONT%P(2,IAD)
                       END DO
                      ENDIF    ! nlocal(n,proc)=0
                    ENDIF      ! tag(n)=0
                   ENDDO
                  END IF
               ENDDO
             ENDIF
          ENDDO
        ENDDO

        DO IJK=1,SIZE_ALE_ELM
          I = ALE_ELM%SOL_ID(IJK)
          IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(I)
          LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(I+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(I)
          DO J=1,8
            NS = IXS(J+1,I)
            DO K=1,LGTH
              SOLV = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + K - 1)
              IF (SOLV>0) THEN
               DO L=1,8
                 N = IXS(L+1,SOLV)
                 IF ( (NODLOCAL(N)/=0).AND.(NODLOCAL(N)<=NUMNOD_L) ) THEN
                  IF( IFRONT%IENTRY(N) /=0) THEN
                    IF( IFRONT%P(1,IFRONT%IENTRY(N)) < PROC) GOTO 1111
                  ENDIF

                  PSEARCH = .TRUE.
                  IAD = IFRONT%IENTRY(NS)
                  IF(IAD <= 0) PSEARCH = .FALSE.
                  DO WHILE(PSEARCH)
                    P = IFRONT%P(1,IAD)
                   IF(P/=PROC) THEN
                    IF(TAG_L(P,NODLOCAL(N))==0) THEN
                      IF(NLOCAL(N,P)==0) THEN
                       NS_L = NS_L + 1
                       LNSDVOIS(NS_L) = N
                       CPULOCALS(NS_L) = P
                       NBSDVOIS(P) = NBSDVOIS(P)+1
                       TAG_L(P,NODLOCAL(N)) = 1
                      END IF ! nlocal(n,p) = 0
                    ENDIF   ! tag_l = 0
                   ENDIF   ! p/=proc
                    IF(IFRONT%P(2,IAD)==0) PSEARCH = .FALSE.
                    IAD = IFRONT%P(2,IAD)
                  END DO
 1111             CONTINUE
                 END IF
               ENDDO
              ENDIF
            ENDDO
          END DO
        ENDDO
        DO I=1,NUMELQ
           IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(I)
           LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(I+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(I)
          DO J=1,4
             NS = IXQ(j+1,I)
             IF ( (NODLOCAL(NS)/=0).AND.(NODLOCAL(NS)<=NUMNOD_L) ) THEN
               DO K=1,LGTH
                  IE = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + K - 1)
                  IF (IE>0) THEN
                   DO L = 1,4
                     N = IXQ(L+1,IE)
                     IF(TAG(N)==0) THEN
                      IF ( (NODLOCAL(N)==0).OR.(NODLOCAL(N)>NUMNOD_L) ) THEN
                       NR_L = NR_L + 1
                       LNRCVOIS(NR_L) = N
                       TAG(N) = 1
                       DO PROC2 = 1, NSPMD
                        IF(PROC2/=PROC) THEN
                         IF(NLOCAL(N,PROC2)==1) THEN
                          CPULOCALR(NR_L) = PROC2
                          NBRCVOIS(PROC2) = NBRCVOIS(PROC2)+1
                          GOTO 2112
                         ENDIF  ! nlocal(n,proc2)=1
                        END IF  ! proc2/=proc
                       END DO
 2112                  CONTINUE
                      ENDIF     ! nlocal(n,proc)=0
                    ENDIF       ! tag(n)=0
                   ENDDO
                  END IF
               ENDDO
             ENDIF
          ENDDO

          DO J=1,4
            NS = IXQ(J+1,I)
            DO K=1,LGTH
              SOLV = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + K - 1)
              IF (SOLV>0) THEN
               DO L=1,4
                 N = IXQ(L+1,SOLV)
                 IF ( (NODLOCAL(N)/=0).AND.(NODLOCAL(N)<=NUMNOD_L) ) THEN
                  DO PROC2 = 1, PROC-1
                    IF(NLOCAL(N,PROC2)==1)GOTO 1112
                  END DO
                  DO P = 1, NSPMD
                   IF(P/=PROC) THEN
                    IF(TAG_L(P,NODLOCAL(N))==0) THEN
                     IF(NLOCAL(NS,P)==1) THEN
                      IF(NLOCAL(N,P)==0) THEN
                       NS_L = NS_L + 1
                       LNSDVOIS(NS_L) = N
                       CPULOCALS(NS_L) = P
                       NBSDVOIS(P) = NBSDVOIS(P)+1
                       TAG_L(P,NODLOCAL(N)) = 1
                      ENDIF ! nlocal(n,p)=0
                     END IF ! nlocal(ns,p)=1
                    ENDIF   ! tag_l = 0
                   ENDIF    ! p/=proc
                  END DO
 1112             CONTINUE
                 END IF
               ENDDO
              ENDIF
            ENDDO
          END DO
        ENDDO
C 2D --> triangles
        IF (N2D /= 0 .AND. MULTI_FVM%IS_USED) THEN
        DO I=1,NUMELTG
           IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(I)
           LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(I+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(I)
          DO J=1,3
             NS = IXTG(j+1,I)
             IF ( (NODLOCAL(NS)/=0).AND.(NODLOCAL(NS)<=NUMNOD_L) ) THEN
               DO K=1,LGTH
                  IE = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + K - 1)
                  IF (IE>0) THEN
                   DO L = 1,3
                     N = IXTG(L+1,IE)
                     IF(TAG(N)==0) THEN
                      IF ( (NODLOCAL(N)==0).OR.(NODLOCAL(N)>NUMNOD_L) ) THEN
                       NR_L = NR_L + 1
                       LNRCVOIS(NR_L) = N
                       TAG(N) = 1
                       DO PROC2 = 1, NSPMD
                        IF(PROC2/=PROC) THEN
                         IF(NLOCAL(N,PROC2)==1) THEN
                          CPULOCALR(NR_L) = PROC2
                          NBRCVOIS(PROC2) = NBRCVOIS(PROC2)+1
                          GOTO 2113
                         ENDIF  ! nlocal(n,proc2)=1
                        END IF  ! proc2/=proc
                       END DO
 2113                  CONTINUE
                      ENDIF     ! nlocal(n,proc)=0
                    ENDIF       ! tag(n)=0
                   ENDDO
                  END IF
               ENDDO
             ENDIF
          ENDDO

          DO J=1,3
            NS = IXTG(J+1,I)
            DO K=1,LGTH
              SOLV = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + K - 1)
              IF (SOLV>0) THEN
               DO L=1,3
                 N = IXTG(L+1,SOLV)
                 IF ( (NODLOCAL(N)/=0).AND.(NODLOCAL(N)<=NUMNOD_L) ) THEN
                  DO PROC2 = 1, PROC-1
                    IF(NLOCAL(N,PROC2)==1)GOTO 1113
                  END DO
                  DO P = 1, NSPMD
                   IF(P/=PROC) THEN
                    IF(TAG_L(P,NODLOCAL(N))==0) THEN
                     IF(NLOCAL(NS,P)==1) THEN
                      IF(NLOCAL(N,P)==0) THEN
                       NS_L = NS_L + 1
                       LNSDVOIS(NS_L) = N
                       CPULOCALS(NS_L) = P
                       NBSDVOIS(P) = NBSDVOIS(P)+1
                       TAG_L(P,NODLOCAL(N)) = 1
                      ENDIF ! nlocal(n,p)=0
                     END IF ! nlocal(ns,p)=1
                    ENDIF   ! tag_l = 0
                   ENDIF    ! p/=proc
                  END DO
 1113             CONTINUE
                 END IF
               ENDDO
              ENDIF
            ENDDO
          END DO
        ENDDO
        ENDIF
C
C Tris LNRCVOIS + renumerotation locale
C
        if(nr_l/=NRCVVOIS)print*,'error cfd node boundaries:',nr_l,
     .  NRCVVOIS
        DO I = 1, NRCVVOIS
          ITRI4(1,I) = CPULOCALR(I)
          ITRI4(2,I) = LNRCVOIS(I)
        END DO
        CALL MY_ORDERS(0,WORK,ITRI4,INDEX4,NRCVVOIS,2)
C
        DO I = 1, NRCVVOIS
          LNRCVOIS(I) = NODLOCAL(ITRI4(2,INDEX4(I)))
        END DO
C
C Tris LNSDVOIS + renumerotation locale
C
        if(ns_l/=NSNDVOIS)print*,'error cfd node boundaries :',ns_l,
     .  NSNDVOIS
        DO I = 1, NSNDVOIS
          ITRI4(1,I) = CPULOCALS(I)
          ITRI4(2,I) = LNSDVOIS(I)
        END DO
        CALL MY_ORDERS(0,WORK,ITRI4,INDEX4,NSNDVOIS,2)
C
        DO I = 1, NSNDVOIS
          LNSDVOIS(I) = NODLOCAL(ITRI4(2,INDEX4(I)))
        END DO

C
C Tris LERCVOIS
C
        if(ii/=NERVOIS)print*,'error cfd elem boundaries:',ii,
     .  NERVOIS
        DO I = 1, NERVOIS
          ITRI4(1,I) = CPULOCALER(I)
          ITRI4(2,I) = TAGER(I)
        END DO
        CALL MY_ORDERS(0,WORK,ITRI4,INDEX4,NERVOIS,2)
        DO I = 1, NERVOIS
          ITRI4(2,I) = LERCVOIS(I)
        END DO
        DO I = 1, NERVOIS
          LERCVOIS(I) = ITRI4(2,INDEX4(I))
        END DO
C
C Tris LESDVOIS
C
        if(jj/=NESVOIS)print*,'error cfd elem boundaries :',jj,
     .    NESVOIS
        DO I = 1, NESVOIS
          ITRI4(1,I) = CPULOCALES(I)
          ITRI4(2,I) = TAGES(I)
        END DO
        CALL MY_ORDERS(0,WORK,ITRI4,INDEX4,NESVOIS,2)
        DO I = 1, NESVOIS
          ITRI4(2,I) = LESDVOIS(I)
        END DO
        DO I = 1, NESVOIS
          LESDVOIS(I) = ITRI4(2,INDEX4(I))
        END DO
C
C Tris LSEGCOM
C
        if(nf_l/=NSEGFL_L)print*,'error cfd segment boundaries:',nf_l,
     .    NSEGFL_L
        DO I = 1, NSEGFL_L
          ITRI4(1,I) = CPULOCALF(I)
          ITRI4(2,I) = LSEGCOM(I)
        END DO
        CALL MY_ORDERS(0,WORK,ITRI4,INDEX4,NSEGFL_L,2)
C
        DO I = 1, NSEGFL_L
         LSEGCOM(I) = ITRI4(2,INDEX4(I))
        END DO
C
        DO P = 1, NSPMD
          NPSEGCOM(NSPMD+1)=NPSEGCOM(NSPMD+1)+NPSEGCOM(P)
          NBRCVOIS(NSPMD+1)=NBRCVOIS(NSPMD+1)+NBRCVOIS(P)
          NBSDVOIS(NSPMD+1)=NBSDVOIS(NSPMD+1)+NBSDVOIS(P)
          NERCVOIS(NSPMD+1)=NERCVOIS(NSPMD+1)+NERCVOIS(P)
          NESDVOIS(NSPMD+1)=NESDVOIS(NSPMD+1)+NESDVOIS(P)
        END DO
C
C Porosite
C
        IF(NUMPOR>0)THEN
          NAD=0
          NAD_L=0
          DO IG = 1, NUMGEO
            NPORGEO(IG)=0
            IF(INT(GEO(12,IG))==15)THEN
              N=INT(GEO(31,IG))
              IF(N>0)THEN
                NN_L = 0
                DO J = NAD+1, NAD+N
                  NN = PORNOD(J)
                  IF ( NODLOCAL(NN)/=0.AND.NODLOCAL(NN)<=NUMNOD_L ) THEN
                    NN_L = NN_L + 1
                    LNODPOR(NAD_L+NN_L)=J-NAD
                  END IF
                END DO
                NPORGEO(IG)=NN_L
                NAD = NAD + N
                NAD_L=NAD_L+NN_L
              END IF
            END IF
          END DO
        END IF
      END IF
C
C Interface 18
C
      IF(NBI18_L>0)THEN
        NN = 0
        DO N=1,NINTER
          ITY = IPARI(7,N)
          INACTI = IPARI(22,N)
          IF((ITY==7.OR.ITY==22).AND.INACTI==7)THEN  ! interface 18
            NN = NN + 1
            DO P = 1, NSPMD
              DD_I18(P,NN) = 0
            END DO
            NRTS  = IPARI(3,N)
            NRTM  = IPARI(4,N)
            DO K=1,NRTM
Cel TAGE flag servant pour inacti
              N1 = INTBUF_TAB(N)%IRECTM(4*(K-1)+1)
              N2 = INTBUF_TAB(N)%IRECTM(4*(K-1)+2)
              N3 = INTBUF_TAB(N)%IRECTM(4*(K-1)+3)
              N4 = INTBUF_TAB(N)%IRECTM(4*(K-1)+4)
              DO P = 1, NSPMD
                IF(NLOCAL(N1,P)==1) THEN
                  IF(NLOCAL(N2,P)==1) THEN
                   IF(NLOCAL(N3,P)==1) THEN
                    IF(NLOCAL(N4,P)==1) THEN
                     DD_I18(P,NN) = DD_I18(P,NN) + 1
                     GOTO 1300
                    ENDIF ! N4
                   ENDIF  ! N3
                  ENDIF   ! N2
                END IF    ! N1
              END DO
 1300         CONTINUE
            END DO
C nb total de segments
            DD_I18(NSPMD+1,NN) = NRTM
C PMAIN
            PMAIN = 1
            DO P = 2, NSPMD
              IF(DD_I18(P,NN)>DD_I18(PMAIN,NN))THEN
                PMAIN = P
              END IF
            END DO
            DD_I18(NSPMD+2,NN) = PMAIN
          END IF
        END DO
      END IF
C
      IF ((NR2RLNK>0).AND.(NSUBDOM==0)) THEN
        DO N=1,NR2RLNK
          NN   = IEXLNK(1,N)
          NNG  = IGRNOD(NN)%NENTITY
          DO P = 1, NSPMD
            DD_R2R(P,N) = 0
          END DO
          DD_R2R(NSPMD+1,N) = NNG
          DO I = 1, NNG
            NOD = IGRNOD(NN)%ENTITY(I)
            DO P = 1, NSPMD
              IF(NLOCAL(NOD,P)==1)THEN
                DD_R2R(P,N) =  DD_R2R(P,N) + 1
                GO TO 1400
              END IF
            END DO
 1400       CONTINUE
          END DO
        END DO
      END IF
C
C Frontiere LAG MULT
C
      DO P = 1, NSPMD
       DD_LAGF(1,NSPMD+1) = DD_LAGF(1,NSPMD+1)+DD_LAGF(1,P)  ! deja calcule ds routines de split
       DD_LAGF(2,NSPMD+1) = DD_LAGF(2,NSPMD+1)+DD_LAGF(2,P)  ! deja calcule ds routines de split
      END DO
C
      DO P = 1, NSPMD+1
        DD_LAGF(3,P) = 0
      END DO
C
      IF(LAG_NCF>0) THEN
        DO N = 1, NUMNOD
          TAG(N) = 0
        END DO
c        NLAGF = 0
        DO IC = 1, LAG_NCF
          IK0 = IADLL(IC)
          IKN = IADLL(IC+1)-1
          DO IK = IK0,IKN
            N = LLL(IK)
            IF(TAG(N)==0) THEN
              TAG(N) = 1
              DO P = 1, NSPMD
                IF(NLOCAL(N,P)==1)THEN
                  DD_LAGF(3,P) = DD_LAGF(3,P) + 1
                  IF(P==PROC)THEN
                    LLAGF(DD_LAGF(3,P)) = NODLOCAL(N)
                  END IF
                  GOTO 5000
                END IF
              END DO
 5000         CONTINUE
            END IF
          END DO
        END DO
        IF(DD_LAGF(3,PROC)/=NLAGF_L)print*,
     +    'error : wrong lag mult decomposition !'
        DO P = 1, NSPMD
          DD_LAGF(3,NSPMD+1) = DD_LAGF(3,NSPMD+1)+DD_LAGF(3,P)
        END DO
      END IF
C ---------------------
C Fontiere UserWindow
C ---------------------
      IF(IUSERWI /= 0 ) THEN
        IADUSERW(1:NSPMD+1)=1
        IADUSERW2(1:NSPMD+1)=0
        IF(PROC/=1)THEN
C pour les procs autre que le premier on note les noeuds
C dont on va recevoir les accels.
          DO I=1,NUMNOD
            IF(USERNODS(I)==1 .AND. NLOCAL(I,PROC)==1)THEN
               IADUSERW(PROC+1)=IADUSERW(PROC+1)+1
            ENDIF
          ENDDO
          DO P=PROC+2,NSPMD+1
            IADUSERW(P)=IADUSERW(P-1)
          ENDDO
        ELSE
          DO I=1,NUMNOD
             IF(USERNODS(I)==1)THEN
               DO P=2,NSPMD
                 IF(NLOCAL(I,P)==1)THEN
                    IADUSERW2(P)=IADUSERW2(P)+1
                 ENDIF
               ENDDO
             ENDIF
          ENDDO
          IADUSERW(1)=1
          DO P=2,NSPMD+1
            IADUSERW(P)=IADUSERW(P-1)+IADUSERW2(P-1)
          ENDDO
        ENDIF
C On remplit le tableau FR_USERW
        LEN_USERW=IADUSERW(NSPMD+1)-IADUSERW(1)
        ALLOCATE(FR_USERWI(LEN_USERW))
        IF (PROC/=1)THEN
          IAD=1
          DO I=1,NUMNOD
            IF(USERNODS(I)==1 .AND.NLOCAL(I,PROC)==1)THEN
               FR_USERWI(IAD)=NODLOCAL(I)
               IAD=IAD+1
            ENDIF
          ENDDO
         ELSE
          IADUSERW2=IADUSERW
          DO I=1,NUMNOD
            IF(USERNODS(I)==1 )THEN
              DO P=2,NSPMD
                IF (NLOCAL(I,P)==1)THEN
                  IAD = IADUSERW2(P)
                  FR_USERWI(IAD)=NODLOCAL(I)
                  IADUSERW2(P)=IADUSERW2(P)+1
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ENDIF
        NUSERWI_L=IADUSERW(NSPMD+1)-IADUSERW(1)
      ENDIF
C-------------------------
C  xfem edge boudary treatment
      IF(ICRACK3D > 0)THEN
C
        DO P = 1, NSPMD+1
          IAD_EDGE(P) = 0
          FR_NBEDGE(P)  = 0
        ENDDO
C
C  tag front edges (local)
C
        IF(NBDDEDGE_L > 0)THEN
          ALLOCATE(TAG_IED_FR0(2,NBDDEDGE_L))
          TAG_IED_FR0 = 0
        ELSE
          ALLOCATE(TAG_IED_FR0(0,0))
        ENDIF
C
        IED_FR = 0
        DO P = 1,NSPMD
          DO IED_GL=1,NUMEDGES
cc            IF(EDGELOCAL(IED_GL) > 0.AND.IEDGE_TMP(1,IED_GL) > 1)THEN
            IF(IEDGE_TMP(3,IED_GL) < 0)THEN
              N1 = NODEDGE(1,IED_GL)
              N2 = NODEDGE(2,IED_GL)
              IF((NLOCAL(N1,PROC)==1).AND.
     .           (NLOCAL(N2,PROC)==1))THEN
                IF(P/=PROC)THEN
                  IF((NLOCAL(N1,P)==1).AND.
     .               (NLOCAL(N2,P)==1))THEN
C---
                    IED_FR = IED_FR + 1
                    TAG_IED_FR0(1,IED_FR) = IED_GL
                    TAG_IED_FR0(2,IED_FR) = P
                    FR_NBEDGE(P) = FR_NBEDGE(P) + 1
C---
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDDO  ! DO IED_GL=1,NUMEDGES
        ENDDO !  DO P = 1,NSPMD
C
        NB_FREDGE = IED_FR
cc        IF(NB_FREDGE == 0)GOTO 113
C
        IF(NB_FREDGE > 0)THEN
          ALLOCATE(FR_EDGE(NB_FREDGE))
          FR_EDGE = 0
        ELSE
          ALLOCATE(FR_EDGE(0))
        ENDIF
C
        IF(NB_FREDGE == 0)GOTO 113
        IED_FR = 0
        DO P = 1,NSPMD
          IF(P /= PROC)THEN
            DO I=1,NB_FREDGE
             IF(TAG_IED_FR0(2,I) == P)THEN
               IED_GL = TAG_IED_FR0(1,I)
               IED_FR = IED_FR + 1
c                      IED_FR = IEDGE_TMP(1,IED_GL)
               FR_EDGE(IED_FR) = EDGELOCAL(IED_GL)
             ENDIF
            ENDDO
          ENDIF
        ENDDO
cc     IAD_EDGE(1) = 1
cc      DO I=2,NSPMD
cc        IAD_EDGE(I)=IAD_EDGE(I-1)+FR_NBEDGE(P)
cc      ENDDO
C
 113  CONTINUE
C
        IAD_EDGE(1) = 1
        DO I=1,NSPMD
          IAD_EDGE(I+1)=IAD_EDGE(I)+FR_NBEDGE(I)
        ENDDO
C
ccccccc        IED_FR = 0
ccccccc        SHIFT_EDG = 0
ccccccc        OK = 0
ccccccc        IAD_EDGE(1) = 1
ccccccc        DO P = 1,NSPMD
ccccccc          IED_FR = 0
ccccccc          IF(P /= PROC)THEN
ccccccc            DO I=1,NB_FREDGE
ccccccc              IED_GL = TAG_IED_FR0(1,I)
ccccccc              IF(IEDGE_TMP(2,IED_GL) > 0)THEN
ccccccc                IF(TAG_IED_FR0(2,I) == P)THEN
ccccccc                   IED_FR = IEDGE_TMP(2,IED_GL)
ccccccc                   IED_FR = IED_FR + SHIFT_EDG
ccccccc                   FR_EDGE(IED_FR) = EDGELOCAL(IED_GL)
ccccccc                   OK = OK + 1
ccccccc                ENDIF
ccccccc              ELSEIF(IEDGE_TMP(2,IED_GL) == 0)THEN
ccccccc                IF(TAG_IED_FR0(2,I) == P)THEN
ccccccc                  IED_FR = IED_FR + 1
ccccccc
ccccccc                  IEDGE_TMP(2,IED_GL) = IED_FR
ccccccc                  FR_EDGE(IED_FR+SHIFT_EDG) = EDGELOCAL(IED_GL)
ccccccc                  OK = OK + 1
ccccccc                ENDIF
ccccccc             ENDIF ! IF(IEDGE_TMP(2,IED_GL) > 0)THEN
ccccccc            ENDDO ! DO I=1,NB_FREDGE
ccccccc            SHIFT_EDG = OK
ccccccc          ENDIF ! IF(P /= PROC)THEN
ccccccc          IAD_EDGE(P+1) = OK+1
ccccccc        ENDDO ! DO P = 1,NSPMD
C
        DO P = 1, NSPMD
          FR_NBEDGE(NSPMD+1) = FR_NBEDGE(NSPMD+1) +  FR_NBEDGE(P)
        ENDDO
C
cc 113  CONTINUE
C
        IF(ALLOCATED(TAG_IED_FR0))DEALLOCATE(TAG_IED_FR0)
      ENDIF  !  IF(ICRACK3D > 0)

C
C     Frontiers vs vertices
      LCSRECT_L = 0
      IF(NINTER25/=0)THEN

        NI25=0
        ISHIFT=0
        LSHIFT=0

        NL=1
        ADDCSRECT_L(NL)=1

        DO NI=1,NINTER
          NTY=IPARI(7,NI)
          IF(NTY/=25) CYCLE

          NBDDNOR = 0

          NI25=NI25+1
          NRTM  =IPARI(4,NI)
          NADMSR=IPARI(67,NI)
          ALLOCATE(TAG_SM(NADMSR),TAG_MS(NADMSR))
          TAG_SM(1:NADMSR)=0

          NADMSR_L=0
          DO K=1,NRTM
            IF(INTERCEP(1,NI)%P(K)==PROC)THEN
              N1 = INTBUF_TAB(NI)%ADMSR(4*(K-1)+1)
              N2 = INTBUF_TAB(NI)%ADMSR(4*(K-1)+2)
              N3 = INTBUF_TAB(NI)%ADMSR(4*(K-1)+3)
              N4 = INTBUF_TAB(NI)%ADMSR(4*(K-1)+4)
              IF(TAG_SM(N1)==0)THEN
                NADMSR_L=NADMSR_L+1
                TAG_SM(N1)=NADMSR_L
              END IF
              IF(TAG_SM(N2)==0)THEN
                NADMSR_L=NADMSR_L+1
                TAG_SM(N2)=NADMSR_L
              END IF
              IF(TAG_SM(N3)==0)THEN
                NADMSR_L=NADMSR_L+1
                TAG_SM(N3)=NADMSR_L
              END IF
              IF(TAG_SM(N4)==0)THEN
                NADMSR_L=NADMSR_L+1
                TAG_SM(N4)=NADMSR_L
              END IF
            ENDIF
          ENDDO


          DO I = 1, NADMSR
            K = TAG_SM(I)
            IF(K/=0)THEN
               TAG_MS(K)=I
            END IF
          END DO

          DO I = 1, NADMSR_L
            N = TAG_MS(I) + ISHIFT

            ADDCSRECT_L(NL+1)=ADDCSRECT_L(NL)+ADDCSRECT(N+1)-ADDCSRECT(N)
            NL = NL + 1

            TAGP(1:NSPMD)=0
            DO J = ADDCSRECT(N), ADDCSRECT(N+1)-1
               K = CSRECT(J)
               P = INTERCEP(1,NI)%P(K)
               IF(P /= PROC.AND.TAGP(P)==0) THEN
                 NBDDNOR = NBDDNOR + 1
                 FR_NOR(LSHIFT+NBDDNOR) = TAG_MS(I)
                 PROC_REM25(NBDDNOR) = P
                 TAGP(P)=1
               ENDIF
               LCSRECT_L = LCSRECT_L + 1
               PROCNOR(LCSRECT_L)=P
            ENDDO
          ENDDO
C
          DO I = 1, NBDDNOR
            ITRI25(1,I) =  PROC_REM25(I)
            ITRI25(2,I) =  FR_NOR(LSHIFT+I)
            ITRI25(3,I) =  0 ! unused
            INDEX25(I) = I
          ENDDO
          CALL MY_ORDERS(0,WORK,ITRI25,INDEX25,NBDDNOR,3)
          DO I = 1, NBDDNOR
            PROC_REM25(I)= ITRI25(1,INDEX25(I))
            FR_NOR(LSHIFT+I) = TAG_SM(ITRI25(2,INDEX25(I)))
          ENDDO
C
          DO P = 1, NSPMD
            ISOM(P) = 0
          ENDDO
          DO I = 1, NBDDNOR
            P =  PROC_REM25(I)
            ISOM(P) = ISOM(P) + 1
          ENDDO
          IAD_FRNOR(NI25,1) = LSHIFT + 1
          DO P = 1, NSPMD
            IAD_FRNOR(NI25,P+1) = IAD_FRNOR(NI25,P) + ISOM(P)
          ENDDO

          DEALLOCATE(TAG_SM,TAG_MS)

          ISHIFT=ISHIFT+NADMSR
          LSHIFT=LSHIFT+NBDDNOR
        END DO
      ELSE
       ADDCSRECT_L(1:NUMNOR_L+1)=0
      END IF ! NINTER25/=0
C
C     Frontiers vs edges
      IF(NINTER25/=0)THEN

        NI25=0
        LSHIFT=0

        DO NI=1,NINTER
          NTY=IPARI(7,NI)
          IF(NTY/=25) CYCLE

          NBDDEDG = 0

          NI25=NI25+1
          NRTM  =IPARI(4,NI)
          ALLOCATE(TAG_SM(NRTM),TAG_MS(NRTM))
          TAG_SM(1:NRTM)=0

          NRTM_L=0
          DO K=1,NRTM
            IF(INTERCEP(1,NI)%P(K)==PROC)THEN
              NRTM_L=NRTM_L+1
              TAG_SM(K)=NRTM_L
            ENDIF
          ENDDO

          DO I = 1, NRTM
            K = TAG_SM(I)
            IF(K/=0)THEN
               TAG_MS(K)=I
            END IF
          END DO

          DO I = 1, NRTM_L
            N = TAG_MS(I)
            DO J = 1,4
               K = INTBUF_TAB(NI)%MVOISIN(4*(N-1)+J)
               IF(K/=0)THEN
                 P = INTERCEP(1,NI)%P(K)
                 IF(P /= PROC) THEN
                   NBDDEDG = NBDDEDG + 1
                   FR_SAV(1,NBDDEDG) = I ! tri par no global croissant
                   FR_SAV(2,NBDDEDG) = J
                   PROC_REM25(NBDDEDG) = P
                   ! Tri des aretes frontieres vs S1, S2 <=> ordre unique
                   ITRI25(1,NBDDEDG) =  P
                   N1=INTBUF_TAB(NI)%ADMSR(4*(N-1)+J)
                   N2=INTBUF_TAB(NI)%ADMSR(4*(N-1)+MOD(J,4)+1)
                   ITRI25(2,NBDDEDG) =  MIN(N1,N2)
                   ITRI25(3,NBDDEDG) =  MAX(N1,N2)
                 ENDIF
              END IF
            ENDDO
          ENDDO
C
          DO I = 1, NBDDEDG
            INDEX25(I) = I
          ENDDO
          CALL MY_ORDERS(0,WORK,ITRI25,INDEX25,NBDDEDG,3)
          DO I = 1, NBDDEDG
            PROC_REM25(I)      = ITRI25(1,INDEX25(I))
            FR_EDG(1,LSHIFT+I) = FR_SAV(1,INDEX25(I))
            FR_EDG(2,LSHIFT+I) = FR_SAV(2,INDEX25(I))
          ENDDO
C
          DO P = 1, NSPMD
            ISOM(P) = 0
          ENDDO
          DO I = 1, NBDDEDG
            P =  PROC_REM25(I)
            ISOM(P) = ISOM(P) + 1
          ENDDO
          IAD_FREDG(NI25,1) = LSHIFT + 1
          DO P = 1, NSPMD
            IAD_FREDG(NI25,P+1) = IAD_FREDG(NI25,P) + ISOM(P)
          ENDDO

          DEALLOCATE(TAG_SM,TAG_MS)

          LSHIFT=LSHIFT+NBDDEDG
        END DO

      END IF ! NINTER25/=0
C
C-------------------------
C
C Ecriture frontiere
C     
      CALL WRITE_I_C(DD_MV,NVOLU*(NSPMD+2))
      LEN_IA = LEN_IA + NVOLU*(NSPMD+2)
      CALL WRITE_I_C(IAD_ELEM,2*(NSPMD+1))
      LEN_IA = LEN_IA + 2*(NSPMD+1)
      CALL WRITE_I_C(IAD_RBY,NSPMD+1)
      LEN_IA = LEN_IA + NSPMD+1
      CALL WRITE_I_C(IAD_RBY2,4*(NSPMD+1))
      LEN_IA = LEN_IA + 4*(NSPMD+1)
      CALL WRITE_I_C(IAD_I2M,NSPMD+1)
      LEN_IA = LEN_IA + NSPMD+1
      CALL WRITE_I_C(IAD_CJ,(NSPMD+1)*NJOINT)
      LEN_IA = LEN_IA + NSPMD+1
      CALL WRITE_I_C(IAD_RBM,NSPMD+1)
      LEN_IA = LEN_IA + NSPMD+1
      CALL WRITE_I_C(IAD_RBM2,4*(NSPMD+1))
      LEN_IA = LEN_IA + 4*(NSPMD+1)
C  
      CALL WRITE_I_C(IAD_RBE2,NSPMD+1)
      LEN_IA = LEN_IA + NSPMD+1
      CALL WRITE_I_C(IAD_RBE3M,NSPMD+1)
      LEN_IA = LEN_IA + NSPMD+1
      CALL WRITE_I_C(IAD_SEC,4*(NSPMD+1))
      LEN_IA = LEN_IA + 4*(NSPMD+1)
      CALL WRITE_I_C(IAD_CUT,ISECUT*ISP0*NSECT*(NSPMD+2))
      LEN_IA = LEN_IA + ISECUT*ISP0*NSECT*(NSPMD+2)
C
      CALL WRITE_I_C(IAD_RBYM,NSPMD+1)
      LEN_IA = LEN_IA + NSPMD+1
      CALL WRITE_I_C(IAD_RBYM2,4*(NSPMD+1))
      LEN_IA = LEN_IA + 4*(NSPMD+1)
c
      CALL WRITE_I_C(DD_ELEM,NBDDACC+NBDDKIN)
      LEN_IA = LEN_IA + NBDDACC+NBDDKIN
      CALL WRITE_I_C(DD_RBY,NBDDNRB)
      LEN_IA = LEN_IA + NBDDNRB
      CALL WRITE_I_C(DD_WALL,NRWALL*(NSPMD+2))
      LEN_IA = LEN_IA + NRWALL*(NSPMD+2)
      CALL WRITE_I_C(DD_RBY2,3*NRBYKIN)
      LEN_IA = LEN_IA + 3*NRBYKIN
      CALL WRITE_I_C(DD_I2M,NBDDI2M)
      LEN_IA = LEN_IA + NBDDI2M
      CALL WRITE_I_C(DD_LL,NLINK*(NSPMD+2))
      LEN_IA = LEN_IA + NLINK*(NSPMD+2)
      CALL WRITE_I_C(DD_CJ,NBDDNCJ)
      LEN_IA = LEN_IA + NBDDNCJ
      CALL WRITE_I_C(DD_RBM,NBDDNRBM)
      LEN_IA = LEN_IA + NBDDNRBM
      CALL WRITE_I_C(DD_RBM2,3*NIBVEL)
      LEN_IA = LEN_IA + 3*NIBVEL
C  
      CALL WRITE_I_C(DD_RBE2,NBDDRBE2)
      LEN_IA = LEN_IA + NBDDRBE2
      CALL WRITE_I_C(DD_RBE3M,NBDDRBE3M)
      LEN_IA = LEN_IA + NBDDRBE3M
      CALL WRITE_I_C(DP_RBE3M,NBDDRBE3M)
      LEN_IA = LEN_IA + NBDDRBE3M
      CALL WRITE_I_C(DD_SEC,(NSPMD+1)*NSECT)
      LEN_IA = LEN_IA + (NSPMD+1)*NSECT
      CALL WRITE_I_C(DD_CUT,NNODT_L)
      LEN_IA = LEN_IA + NNODT_L
      CALL WRITE_I_C(RG_CUT,NNODL_L)
      LEN_IA = LEN_IA + NNODL_L
      CALL WRITE_I_C(DD_MAD,5*(NSPMD+1))
      LEN_IA = LEN_IA + 5*(NSPMD+1)
      CALL WRITE_I_C(DD_I18,(NSPMD+2)*NBI18_L)
      LEN_IA = LEN_IA + (NSPMD+2)*NBI18_L
      CALL WRITE_I_C(DD_R2R,(NSPMD+1)*NL_DDR2R)
      LEN_IA = LEN_IA + (NSPMD+1)*NL_DDR2R
      IF(SDD_R2R_ELEM > 0) THEN
      CALL WRITE_I_C(DD_R2R_ELEM,SDD_R2R_ELEM)
      LEN_IA = LEN_IA + SDD_R2R_ELEM
      ENDIF
Csm
      CALL WRITE_I_C(DD_RBYM,NBDDNRBYM)
      LEN_IA = LEN_IA + NBDDNRBYM
      CALL WRITE_I_C(DD_RBYM2,3*NRBYM)
      LEN_IA = LEN_IA + 3*NRBYM
C
      CALL WRITE_I_C(ADDCSRECT_L,NUMNOR_L+1)
      LEN_IA = LEN_IA + NUMNOR_L+1
      CALL WRITE_I_C(FR_NOR,NBDDNORT)
      LEN_IA = LEN_IA + NBDDNORT
      CALL WRITE_I_C(IAD_FRNOR,(NSPMD+1)*NINTER25)
      LEN_IA = LEN_IA + (NSPMD+1)*NINTER25
      if(LCSRECT_L /= NBCCNOR) print *,'internal error'
      CALL WRITE_I_C(PROCNOR,LCSRECT_L)
      LEN_IA = LEN_IA + LCSRECT_L
      CALL WRITE_I_C(FR_EDG,2*NBDDEDGT)
      LEN_IA = LEN_IA + 2*NBDDEDGT
      CALL WRITE_I_C(IAD_FREDG,(NSPMD+1)*NINTER25)
      LEN_IA = LEN_IA + (NSPMD+1)*NINTER25
C
      IF(NUMSKW>0)THEN
        CALL WRITE_I_C(ISKWP,NUMSKW+1)
        CALL WRITE_I_C(NSKWP,NSPMD)
        CALL WRITE_I_C(ISKWP_L,NSKWP(PROC))
        LEN_IA = LEN_IA + NSPMD + NUMSKW+1 + NSKWP(PROC)
      END IF
      IF(NSENSOR>0)THEN
        CALL WRITE_I_C(ISENSP,2*NSENSOR)
        CALL WRITE_I_C(NSENSP,NSPMD)
        LEN_IA = LEN_IA + 2*NSENSOR + NSPMD
      END IF
      IF(NACCELM>0)THEN
        CALL WRITE_I_C(IACCP,NACCELM)
        CALL WRITE_I_C(NACCP,NSPMD)
        LEN_IA = LEN_IA +NACCELM +NSPMD
      END IF
      IF(NBGAUGE>0)THEN
        CALL WRITE_I_C(IGAUP,NBGAUGE)
        CALL WRITE_I_C(NGAUP,NSPMD)
        LEN_IA = LEN_IA +NBGAUGE +NSPMD
      END IF
C
      CALL WRITE_I_C(DD_LAGF,3*(NSPMD+1))
      LEN_IA = LEN_IA + 3*(NSPMD+1)
      CALL WRITE_I_C(NEWFRONT,NINTER)
      LEN_IA = LEN_IA + NINTER
C
C Ecriture frontiere ALE/CFD
C
      IF(IALE+IEULER+ITHERM+IALELAG/=0)THEN
        CALL WRITE_I_C(NBRCVOIS,NSPMD+1)
        LEN_IA = LEN_IA + NSPMD+1
        CALL WRITE_I_C(LNRCVOIS,NRCVVOIS)
        LEN_IA = LEN_IA + NRCVVOIS
        CALL WRITE_I_C(NBSDVOIS,NSPMD+1)
        LEN_IA = LEN_IA + NSPMD+1
        CALL WRITE_I_C(LNSDVOIS,NSNDVOIS)
        LEN_IA = LEN_IA + NSNDVOIS
        CALL WRITE_I_C(NERCVOIS,NSPMD+1)
        LEN_IA = LEN_IA + NSPMD+1
        CALL WRITE_I_C(LERCVOIS,NERVOIS)
        LEN_IA = LEN_IA + NERVOIS
        CALL WRITE_I_C(NESDVOIS,NSPMD+1)
        LEN_IA = LEN_IA + NSPMD+1
        CALL WRITE_I_C(LESDVOIS,NESVOIS)
        LEN_IA = LEN_IA + NESVOIS
        IF(SEGINDX>0) THEN
          CALL WRITE_I_C(NPSEGCOM,NSPMD+1)
          LEN_IA = LEN_IA + NSPMD+1
          CALL WRITE_I_C(LSEGCOM,NSEGFL_L)
          LEN_IA = LEN_IA + NSEGFL_L
        END IF
        IF(NUMPOR>0)THEN
          CALL WRITE_I_C(NPORGEO,NUMGEO)
          LEN_IA = LEN_IA + NUMGEO
          CALL WRITE_I_C(LNODPOR,NUMPOR_L)
          LEN_IA = LEN_IA + NUMPOR_L
        END IF
      END IF
C
      CALL WRITE_I_C(LLAGF,NLAGF_L)
      LEN_IA = LEN_IA + NLAGF_L
C
      IF ((NSUBDOM>0).AND.(IDDOM==0)) THEN
        DEALLOCATE(DD_R2R_ELEM)
      ENDIF
      IF (IUSERWI /=0)THEN
        CALL WRITE_I_C(IADUSERW,NSPMD+1)
        LEN_IA = LEN_IA + NSPMD+1
        CALL WRITE_I_C(FR_USERWI,NUSERWI_L)
        DEALLOCATE(FR_USERWI)
      ENDIF
C
      IF(ICRACK3D > 0)THEN
        CALL WRITE_I_C(IAD_EDGE,NSPMD+1)
        LEN_IA = LEN_IA + NSPMD+1
        CALL WRITE_I_C(FR_EDGE,NB_FREDGE)
        LEN_IA = LEN_IA + NB_FREDGE
        CALL WRITE_I_C(FR_NBEDGE,NSPMD+1)
        LEN_IA = LEN_IA + NSPMD+1
        IF(ALLOCATED(FR_EDGE))DEALLOCATE(FR_EDGE)
      ENDIF
C     
        CALL WRITE_I_C(IAD_CNDM,NSPMD+1)
        LEN_IA = LEN_IA + NSPMD+1
        CALL WRITE_I_C(DD_CNDM,NBDDCNDM)
        LEN_IA = LEN_IA + NBDDCNDM
C
        DEALLOCATE( ACCKIN  )!(NBDDACC+NBDDKIN)
        DEALLOCATE( ADDCSRECT_L  )!(NUMNOR_L+1)
        DEALLOCATE( CPULOCALER  )!(NERVOIS)
        DEALLOCATE( CPULOCALES  )!(NESVOIS)
        DEALLOCATE( CPULOCALF  )!(NSEGFL_L)
        DEALLOCATE( CPULOCALR  )!(NRCVVOIS)
        DEALLOCATE( CPULOCALS  )!(NSNDVOIS)
        DEALLOCATE( DD_CJ  )!(NBDDNCJ)
        DEALLOCATE( DD_CNDM  )!(NBDDCNDM)
        DEALLOCATE( DD_CUT  )!(NNODT_L)
        DEALLOCATE( DD_ELEM  )!(NBDDACC+NBDDKIN)
        DEALLOCATE( DD_I2M  )!(NBDDI2M)
        DEALLOCATE( DD_P  )!(NSPMD)
        DEALLOCATE( DD_RBE2  )!(NBDDRBE2)
        DEALLOCATE( DD_RBE3M  )!(NBDDRBE3M)
        DEALLOCATE( DD_RBM  )!(NBDDNRBM)
        DEALLOCATE( DD_RBY  )!(NBDDNRB)
        DEALLOCATE( DD_RBYM  )!(NBDDNRBYM)
        DEALLOCATE( DP_RBE3M  )!(NBDDRBE3M)
        DEALLOCATE( D_RBY  )!(NSPMD+1)
        DEALLOCATE( FR_NOR  )!(NBDDNORT)
        DEALLOCATE( IADUSERW  )!(NSPMD+1)
        DEALLOCATE( IADUSERW2  )!(NSPMD+1)
        DEALLOCATE( IAD_CNDM  )!(NSPMD+1)
        DEALLOCATE( IAD_I2M  )!(NSPMD+1)
        DEALLOCATE( IAD_RBE2  )!(NSPMD+1)
        DEALLOCATE( IAD_RBE3  )!(NSPMD+1)
        DEALLOCATE( IAD_RBE3M  )!(NSPMD+1)
        DEALLOCATE( IAD_RBM  )!(NSPMD+1)
        DEALLOCATE( IAD_RBY  )!(NSPMD+1)
        DEALLOCATE( IAD_RBYM  )!(NSPMD+1)
        DEALLOCATE( INDEX  )!(2*(NBDDACC+NBDDKIN))
        DEALLOCATE( INDEX2  )!(2*(NBDDNRB))
        DEALLOCATE( INDEX3  )!(2*NBDDNCJ)
        DEALLOCATE( INDEX4  )!(2*NBCFD)
        DEALLOCATE( INDEX5  )!(2*(NBDDNRBYM))
        DEALLOCATE( ISOM  )!(NSPMD)
        DEALLOCATE( ISOM_R2R_R  )!(NSPMD)
        DEALLOCATE( ISOM_R2R_S  )!(NSPMD)
        DEALLOCATE( LERCVOIS  )!(NERVOIS)
        DEALLOCATE( LESDVOIS  )!(NESVOIS)
        DEALLOCATE( LLAGF  )!(NLAGF_L)
        DEALLOCATE( LNODPOR  )!(NUMPOR_L)
        DEALLOCATE( LNRCVOIS  )!(NRCVVOIS)
        DEALLOCATE( LNSDVOIS  )!(NSNDVOIS)
        DEALLOCATE( LSEGCOM  )!(NSEGFL_L)
        DEALLOCATE( NBRCVOIS  )!(NSPMD+1)
        DEALLOCATE( NBSDVOIS  )!(NSPMD+1)
        DEALLOCATE( NERCVOIS  )!(NSPMD+1)
        DEALLOCATE( NESDVOIS  )!(NSPMD+1)
        DEALLOCATE( NPORGEO  )!(NUMGEO)
        DEALLOCATE( NPSEGCOM  )!(NSPMD+1)
        DEALLOCATE( PROCNOR  )!(ADDCSRECT(NUMNOR+1))
        DEALLOCATE( PROC_REM  )!(NBDDACC+NBDDKIN)
        DEALLOCATE( PROC_REM1  )!(NBDDNRBYM)
        DEALLOCATE( RG_CUT  )!(NNODL_L)
        DEALLOCATE( SECVU  )!(NSPMD)
        DEALLOCATE( WORK  )!(70000)
        DEALLOCATE( DD_I18  )!(NSPMD+2,NBI18_L)
        DEALLOCATE( DD_LL  )!(NSPMD+2,NLINK)
        DEALLOCATE( DD_MAD  )!(5,NSPMD+1)
        DEALLOCATE( DD_MV  )!(NSPMD+2,NVOLU)
        DEALLOCATE( DD_R2R  )!(NSPMD+1,NL_DDR2R)
        DEALLOCATE( DD_SEC  )!(NSPMD+1,NSECT)
        DEALLOCATE( DD_WALL  )!(NSPMD+2,NRWALL)
        DEALLOCATE( FR_EDG  )!(2,NBDDEDGT)
        DEALLOCATE( FR_SAV  )!(2,NBDDEDG_MAX)
        DEALLOCATE( IAD_CJ  )!(NSPMD+1,NJOINT)
        DEALLOCATE( IAD_CUT  )!(NSPMD+2,NSECT*ISECUT*ISP0)
        DEALLOCATE( IAD_ELEM  )!(2,NSPMD+1)
        DEALLOCATE( IAD_FREDG  )!(NINTER25,NSPMD+1)
        DEALLOCATE( IAD_FRNOR  )!(NINTER25,NSPMD+1)
        DEALLOCATE( IAD_RBM2  )!(4,NSPMD+1)
        DEALLOCATE( IAD_RBY2  )!(4,NSPMD+1)
        DEALLOCATE( IAD_RBYM2  )!(4,NSPMD+1)
        DEALLOCATE( IAD_SEC  )!(4,NSPMD+1)
        DEALLOCATE( INDEX25  )!(2*MAX(NBDDNOR_MAXNBDDEDG_MAX))
        DEALLOCATE( ITRI  )!(3,NBDDACC+NBDDKIN)
        DEALLOCATE( ITRI2  )!(2,NBDDNRB)
        DEALLOCATE( ITRI25  )!(3,MAX(NBDDNOR_MAX,NBDDEDG_MAX))
        DEALLOCATE( ITRI3  )!(2,NBDDNCJ)
        DEALLOCATE( ITRI4  )!(2,NBCFD)
        DEALLOCATE( ITRI5  )!(2,NBDDNRBYM)
        DEALLOCATE( PROC_REM25  )!(MAX(NBDDNOR_MAXNBDDEDG_MAX))

C
! -----------------------------------
!     deallocate 1d arrays
      DEALLOCATE( WEIGHT,TAGE )
      DEALLOCATE( NEWFRONT,TAG )
      DEALLOCATE( TAGER,TAGES )
!     deallocate 2d arrays
      DEALLOCATE( TAGE_L,TAG_L )
! -----------------------------------
      RETURN
      END
