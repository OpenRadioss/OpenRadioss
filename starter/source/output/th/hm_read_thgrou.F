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
Chd|  HM_READ_THGROU                source/output/th/hm_read_thgrou.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_READ_THGRKI                source/output/th/hm_read_thgrki.F
Chd|        HM_READ_THGRKI_RBODY          source/output/th/hm_read_thgrki_rbody.F
Chd|        HM_READ_THGRNE                source/output/th/hm_read_thgrne.F
Chd|        HM_READ_THGRNS                source/output/th/hm_read_thgrns.F
Chd|        HM_READ_THGRPA                source/output/th/hm_read_thgrpa.F
Chd|        HM_READ_THGRPA_SUB            source/output/th/hm_read_thgrpa.F
Chd|        HM_READ_THGRSURF              source/output/th/hm_read_thgrsurf.F
Chd|        HM_THGRKI_VENT                source/output/th/hm_thgrki_vent.F
Chd|        THPRIN                        source/output/th/thprin.F     
Chd|        THPRIN_SUB                    source/output/th/thprin.F     
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MAPPING_OPTION_MOD            share/modules1/dichotomy_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_THGROU(
     1      ITHGRP,ITHBUF,ITAB   ,ITABM1,IXUR  ,IXTG  ,
     2      IXS   ,IXQ   ,IXC    ,IXT   ,IXP   ,IXR   ,
     3      KXX   ,IXX   ,IPART  ,IFI   ,
     4      NTHWA ,KXSP  ,IXRI  ,ISKWN ,IFRAME ,
     5      NTHGRP2 ,PATHID ,SUTHID ,FXBIPM ,IPARTH ,NPARTH ,
     6      NVPARTH ,NVSUBTH,IMERGE ,ITHVAR ,
     7      FLAGABF,NVARABF ,NOM_OPT,PTR_NOPT_FXBY,PTR_NOPT_INTER,
     8      PTR_NOPT_RWALL,PTR_NOPT_SECT,PTR_NOPT_JOINT,
     9      PTR_NOPT_MONV,PTR_NOPT_ACC,PTR_NOPT_SKW,PTR_NOPT_GAU,
     A      PTR_NOPT_CLUS,PTR_NOPT_SPHIO,ISPHIO,RFI,T_MONVOL,
     B      IGRSURF ,SUBSET ,ITHFLAG,NPBY,LSUBMODEL,IPARG,
     C      IPARTS  ,IPARTQ ,IPARTC ,IPARTT  ,IPARTP  ,IPARTR ,
     D      IPARTG  ,IPARTX ,IPARTSP ,IPARTIG3D, LITHBUFMX,
     E      MAP_TABLES, IFLAG,PTR_NOPT_SLIPRING,PTR_NOPT_RETRACTOR)
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE MONVOL_STRUCT_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE MAPPING_OPTION_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr23_c.inc"
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "com10_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "rnur_c.inc"
#include      "sphcom.inc"
#include      "fxbcom.inc"
#include      "submod_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: LITHBUFMX
      INTEGER ITAB(*),ITABM1(*), IFLAG,
     .        IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*),IXT(NIXT,*),
     .        IXP(NIXP,*),IXR(NIXR,*),IXTG(NIXTG,*),KXX(NIXX,*),
     .        IXX(*),IPART(LIPART1,*),ITHGRP(NITHGR,*),ITHBUF(LITHBUFMX),
     .        IXUR(NIXUR,*),IMERGE(*),
     .        IFI,NTHWA,KXSP(NISP,*),IXRI(4,*),
     .        IFRAME(LISKN,*), NTHGRP2,PATHID(*),SUTHID(*),
     .        FXBIPM(NBIPM,*),ISKWN(LISKN,*),
     .        NPARTH,IPARTH(NPARTH,*),NVPARTH,ITYP,
     .        NVSUBTH,ITHVAR(*),FLAGABF,NVARABF,
     .        RFI,ISPHIO(NISPHIO,*),ITHFLAG,IPARG(NPARG,*)
      INTEGER NOM_OPT(LNOPT1,*),PTR_NOPT_FXBY,PTR_NOPT_INTER,
     .        PTR_NOPT_RWALL,PTR_NOPT_SECT,PTR_NOPT_JOINT,
     .        PTR_NOPT_MONV,PTR_NOPT_ACC,PTR_NOPT_SKW,PTR_NOPT_GAU,
     .        PTR_NOPT_CLUS,PTR_NOPT_SPHIO,PTR_NOPT_SURF,PTR_NOPT_RETRACTOR,
     .        PTR_NOPT_SLIPRING
      INTEGER NPBY(*)
      INTEGER, TARGET :: IPARTS(NUMELS),IPARTQ(NUMELQ),IPARTC(NUMELC),
     .        IPARTT(NUMELT),IPARTP(NUMELP),IPARTR(NUMELR),IPARTG(NUMELTG),
     .        IPARTX(NUMELX),IPARTSP(NUMSPH),IPARTIG3D(NUMELIG3D)
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(IN) :: T_MONVOL
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      TYPE(MAPPING_STRUCT_), INTENT(IN) :: MAP_TABLES
C-----------------------------------------------
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
      TYPE (SURF_)   , DIMENSION(NSURF) :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,JJ,II, I,ISU,ID,NOSYS,J10(10),NTOT,KK,IER,
     .        OK,IGS,IGRS,NSU,K,L,CONT,IAD0,IADV,NTRI,NL,
     .        IFITMP,IADFIN,NUMEL,NV,NVAR,M,N,IAD2,IAD,NSNE,NNE,ISK,
     .        NVALL,TAGP,TAGS,IFIX_TMP,IBID
      CHARACTER TITR*nchartitle,MESS*40,KEY*ncharline,COPT*ncharline,
     .        TITR1*nchartitle
C
      INTEGER NVARN,NVARS,NVARC,NVART,NVARP,NVARR,NVARUR
      INTEGER NVARNS,NVARSPH
      INTEGER NVARIN,NVARRW,NVARRB,NVARAC,NVARSE,NVARJO,NVARFX,NVARFXM
      INTEGER NVARAB,NVARMV4,NVARMV,NVARPA,NVARMVENT
      INTEGER NVARF1,NVARFR,NVARGAU,NVARCLUS,NVARCLUSG,NVARFLOW,NVARSURF
      INTEGER NVARRIV,NVARRIVG,NVARSLIP,NVARRET,NSELRT,INOUT
      INTEGER HM_NTHGRP,NTHACCEL,NTHINTER,NTHRWALL,NTHSECTIO,NTHCLUS
      INTEGER NTHBEAM,NTHTRUS,NTHBRIC,NTHNODE,NTHSHEL,NTHSH3N,NTHSPRING,NTHRBODY  
      INTEGER NTHMONVOL,HM_NTHPART ,NUMTHPART, HM_NTHSUBS, HM_NTHSURF
      INTEGER HM_NTHSPHCEL, HM_NTHQUAD, HM_NTHSPHFLOW
      INTEGER HM_NTHGAUGE, HM_NTHFXBODY, HM_NTHFRAME, HM_NTHCYLJO, HM_NTHNSTRAND   
      INTEGER HM_NTHTRIA,HM_NTHSLIPRING,HM_NTHRETRACTOR
      PARAMETER (NVARRIV = 10,NVARRIVG = 1)
      INTEGER IVARRIV(18,NVARRIVG)
      CHARACTER*10 VARRIV(NVARRIV),VARRIVG(NVARRIVG)
C-----------------------------------------------
      PARAMETER (NVARN = 637,NVARS =239554 ,NVARC = 37856,NVART = 6)
      PARAMETER (NVARP = 11,NVARR = 65,NVARUR = 12)
      PARAMETER (NVARNS = 4,NVARSPH = 41)
      PARAMETER (NVARIN = 28,NVARRW = 6,NVARRB =15,NVARFX =4)
      PARAMETER (NVARFXM = 3)
      PARAMETER (NVARAC = 6,NVARSE =39,NVARJO = 6)
      PARAMETER (NVARAB = 7,NVARMV4= 9,NVARMV = 19,NVARMVENT = 150)
      PARAMETER (NVARPA =28)
      PARAMETER (NVARF1 = 18,NVARFR = 24,NVARGAU = 8,NVARCLUS=11,NVARFLOW=1,
     .           NVARSURF = 3,NVARSLIP=6,NVARRET=3)
      CHARACTER*10 VARN(NVARN),VARS(NVARS),VARC(NVARC),VART(NVART)
      CHARACTER*10 VARP(NVARP),VARR(NVARR),VARUR(NVARUR)
      CHARACTER*10 VARS1(196),VARS2(567), VARS3(369), VARS4(492),
     .             VARS5(22),VARS6(118692),VARS7(118692),VARN1(19),
     .             VARN2(9),
     .             VARNPINCH(9),
     .             VARSG1(202),VARSG2(19782),VARSG3(19782),VARSG4(1),
     .             VARS8(516),VARS9(6),VARSNLOC(2)
      CHARACTER*10 VARNS(NVARNS),VARSPH(NVARSPH)
      CHARACTER*10 VARIN(NVARIN),VARRW(NVARRW),VARRB(NVARRB)
      CHARACTER*10 VARAC(NVARAC),VARSE(NVARSE),VARJO(NVARJO)
      CHARACTER*10 VARAB(NVARAB),VARMV4(NVARMV4),VARMV(NVARMV),
     .             VARMVENT(NVARMVENT)
      CHARACTER*10 VARPA(NVARPA),VARFX(NVARFX),VARFXM(NVARFXM)
      CHARACTER*10 VARF1(NVARF1),VARFR(NVARFR),VARGAU(NVARGAU),
     .             VARSLIP(NVARSLIP),VARRET(NVARRET)
      CHARACTER*10 VARCLUS(NVARCLUS),VARFLOW(NVARFLOW),VARSURF(NVARSURF)
      INTEGER NVARNG,NVARSG,NVARCG,NVARTG,NVARPG,NVARRG,NVARURG
      INTEGER NVARNSG,NVARSPG
      INTEGER NVARING,NVARRWG,NVARRBG,NVARACG,NVARSEG,NVARJOG
      INTEGER NVARABG,NVARMG4,NVARMVG,NVARPAG,NVARFXG,NVARFXMG
      INTEGER NVARF1G,NVARFRG,NVARGAUG,NVARFLOWG,NVARMVENTG,NVARSURFG
      INTEGER NVARSLIPG,NVARRETG
      PARAMETER (NVARNG = 8,NVARSG = 39767,NVARCG = 675,NVARTG = 1)
      PARAMETER (NVARPG = 1,NVARRG = 1,NVARURG = 1)
      PARAMETER (NVARNSG = 1,NVARSPG = 2)
      PARAMETER (NVARING = 6,NVARRWG = 3,NVARRBG = 6,NVARFXG = 1)
      PARAMETER (NVARFXMG =1)
      PARAMETER (NVARACG = 2,NVARSEG = 7,NVARJOG = 3)
      PARAMETER (NVARABG = 1,NVARMG4 = 1,NVARMVG = 62,NVARMVENTG=62)
      PARAMETER (NVARPAG = 1)
      PARAMETER (NVARF1G = 3,NVARFRG = 4,NVARGAUG = 1,NVARCLUSG=2,NVARFLOWG=1,
     .           NVARSURFG = 1,NVARSLIPG=1,NVARRETG = 1)
      CHARACTER*10 VARNG(NVARNG),VARSG(NVARSG)
      CHARACTER*10 VARCG(NVARCG),VARTG(NVARTG)
      CHARACTER*10 VARPG(NVARPG),VARRG(NVARRG),VARURG(NVARURG)
      CHARACTER*10 VARNSG(NVARNSG),VARSPG(NVARSPG)
      CHARACTER*10 VARING(NVARING),VARRWG(NVARRWG),VARRBG(NVARRBG)
      CHARACTER*10 VARACG(NVARACG),VARSEG(NVARSEG),VARJOG(NVARJOG)
      CHARACTER*10 VARABG(NVARABG),VARMVG4(NVARMG4),VARMVG(NVARMVG),
     .             VARMVENTG(NVARMVENTG)
      CHARACTER*10 VARPAG(NVARPAG),VARFXG(NVARFXG),VARFXMG(NVARFXMG)
      CHARACTER*10 VARF1G(NVARF1G),VARFRG(NVARFRG),VARGAUG(NVARGAUG)
      CHARACTER*10 VARCLUSG(NVARCLUSG),VARFLOWG(NVARFLOWG)
      CHARACTER*10 VARSURFG(NVARSURFG),VARSLIPG(NVARSLIPG),VARRETG(NVARRETG)
C-----------------------------------------------
      INTEGER IVARNG(18,NVARNG),IVARSG(18,NVARSG)
      INTEGER IVARCG(18,NVARCG),IVARTG(18,NVARTG)
      INTEGER IVARPG(18,NVARPG),IVARRG(18,NVARRG),IVARURG(18,NVARURG)
      INTEGER IVARNSG(18,NVARNSG),IVARSPG(18,NVARSPG),IVNS2R(18,NVARNSG)
      INTEGER IVARING(18,NVARING),IVARRWG(18,NVARRWG)
      INTEGER IVARRBG(18,NVARRBG),IVARACG(18,NVARACG)
      INTEGER IVARSEG(18,NVARSEG),IVARJOG(18,NVARJOG)
      INTEGER IVARABG(18,NVARABG),IVARMG4(18,NVARMG4),
     .        IVARMVG(18,NVARMVG),IVARMVENTG(18,NVARMVENTG)
      INTEGER IVARPAG(18,NVARPAG),IVARFXG(18,NVARFXG)
      INTEGER IVARFXMG(18,NVARFXMG)
      INTEGER IVARF1G(18,NVARF1G),IVARFRG(18,NVARFRG),
     .        IVARGAUG(18,NVARGAUG),IVARCLUSG(18,NVARCLUSG),
     .        IVARFLOWG(18,NVARFLOWG),IVARSURFG(18,NVARSURFG),
     .        IVARSLIPG(18,NVARSLIPG),IVARRETG(18,NVARRETG)
      INTEGER NSMA
      INTEGER IUN,INODACC,INODFRA
      INTEGER NNOD, NSHEL, NTRUS, NBEAM, NSPRG, NCHTG, NCHINT, NCHJNT,
     .     CHNOD, CHELT, CNT1, CNT2, IVSG, IREC_0, OUT_VENT, NG, NEL, 
     .     NFT, NUMPART, I15ATH, I15A, I15B, I15C, I15D, I15E, I15F, 
     .     I15G, I15H, I15I, I15J, I15K, I15L, ITY
      my_real
     .   BID
       INTEGER IVCG
       CHARACTER  CHJJ*3,CHKK*2,CHII*2,CHJKK*4
       CHARACTER  CHI*1,CHJ*3,CHK*1,CHJS*1
       CHARACTER*10 VARC2(574),VARC1(208),VARCG1(167),VARCG2(407), VAR ,
     .              VAR_TMP,VARC3(500),VARCG3(100),
     .              VARCGPINCH(1),VARCPINCH(7),VARCNLOC(2)
       CHARACTER*80 STR, MES
       LOGICAL :: IS_AVAILABLE
      INTEGER, DIMENSION(:), POINTER :: IPARTEL_P
      CHARACTER THFILE*4
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,ULIST2S,LISTCNT
C-----------------------------------------------
C   D a t a
C-----------------------------------------------
      DATA MESS/'TH GROUP DEFINITION                     '/
      DATA IUN/1/
C-----------------------------------------------
C   TH VARIABLES
C-----------------------------------------------
C   nodes
      DATA VARN1/'DX      ','DY      ','DZ      ',
     .           'VX      ','VY      ','VZ      ',
     .           'AX      ','AY      ','AZ      ',
     .           'VRX     ','VRY     ','VRZ     ',
     .           'ARX     ','ARY     ','ARZ     ',
     .           'X       ','Y       ','Z       ',
     .           'TEMP    '/
      DATA VARN2/'REACX   ','REACY   ','REACZ   ',
     .           'REACXX  ','REACYY  ','REACZZ  ',
     .           'DRX     ','DRY     ','DRZ     '/
      DATA VARNPINCH/
     .          'APINCHX ','APINCHY ','APINCHZ ',
     .          'VPINCHX ','VPINCHY ','VPINCHZ ',
     .          'DPINCHX ','DPINCHY ','DPINCHZ '/
C   solids
      DATA VARS1/
     .'OFF     ','SX      ','SY      ','SZ      ','SXY     ',
     .'SYZ     ','SXZ     ','IE      ','DENS    ','BULK    ',
     .'VOL     ','PLAS    ','TEMP    ','PLSR    ','DAM1    ',
     .'DAM2    ','DAM3    ','DAM4    ','DAMA    ','SA1     ',
     .'SA2     ','SA3     ','CR      ','CAP     ','K0      ',
     .'RK      ','TD      ','EFIB    ','ISTA    ','VPLA    ',
     .'BFRAC   ','WPLA    ','SFIB    ','DAM5    ','LSX     ',
     .'LSY     ','LSZ     ','LSXY    ','LSYZ    ','LSXZ    ',
     .'SX1     ','SX2     ','SX3     ','SX4     ','SX5     ','SX6     ',
     .'SX7     ','SX8     ','SY1     ','SY2     ','SY3     ','SY4     ',
     .'SY5     ','SY6     ','SY7     ','SY8     ','SZ1     ','SZ2     ',
     .'SZ3     ','SZ4     ','SZ5     ','SZ6     ','SZ7     ','SZ8     ',
     .'SXY1    ','SXY2    ','SXY3    ','SXY4    ','SXY5    ','SXY6    ',
     .'SXY7    ','SXY8    ','SYZ1    ','SYZ2    ','SYZ3    ','SYZ4    ',
     .'SYZ5    ','SYZ6    ','SYZ7    ','SYZ8    ','SXZ1    ','SXZ2    ',
     .'SXZ3    ','SXZ4    ','SXZ5    ','SXZ6    ','SXZ7    ','SXZ8    ',
     .'LSX1    ','LSX2    ','LSX3    ','LSX4    ','LSX5    ','LSX6    ',
     .'LSX7    ','LSX8    ','LSY1    ','LSY2    ','LSY3    ','LSY4    ',
     .'LSY5    ','LSY6    ','LSY7    ','LSY8    ','LSZ1    ','LSZ2    ',
     .'LSZ3    ','LSZ4    ','LSZ5    ','LSZ6    ','LSZ7    ','LSZ8    ',
     .'LSXY1   ','LSXY2   ','LSXY3   ','LSXY4   ','LSXY5   ','LSXY6   ',
     .'LSXY7   ','LSXY8   ','LSYZ1   ','LSYZ2   ','LSYZ3   ','LSYZ4   ',
     .'LSYZ5   ','LSYZ6   ','LSYZ7   ','LSYZ8   ','LSXZ1   ','LSXZ2   ',
     .'LSXZ3   ','LSXZ4   ','LSXZ5   ','LSXZ6   ','LSXZ7   ','LSXZ8   ',
     .'USR1    ','USR2    ','USR3    ','USR4    ','USR5    ','USR6    ',
     .'USR7    ','USR8    ','USR9    ','USR10   ','USR11   ','USR12   ',
     .'USR13   ','USR14   ','USR15   ','USR16   ','USR17   ','USR18   ',
     .'USR19   ','USR20   ','USR21   ','USR22   ','USR23   ','USR24   ',
     .'USR25   ','USR26   ','USR27   ','USR28   ','USR29   ','USR30   ',
     .'USR31   ','USR32   ','USR33   ','USR34   ','USR35   ','USR36   ',
     .'USR37   ','USR38   ','USR39   ','USR40   ','USR41   ','USR42   ',
     .'USR43   ','USR44   ','USR45   ','USR46   ','USR47   ','USR48   ',
     .'USR49   ','USR50   ','USR51   ','USR52   ','USR53   ','USR54   ',
     .'USR55   ','USR56   ','USR57   ','USR58   ','USR59   ','USR60   '/
C
      DATA VARS2/
     .'SX111   ','SY111   ','SZ111   ','SXY111  ','SYZ111  ','SXZ111  ',
     .'EP111   ','SX211   ','SY211   ','SZ211   ','SXY211  ','SYZ211  ',
     .'SXZ211  ','EP211   ','SX311   ','SY311   ','SZ311   ','SXY311  ',
     .'SYZ311  ','SXZ311  ','EP311   ','SX121   ','SY121   ','SZ121   ',
     .'SXY121  ','SYZ121  ','SXZ121  ','EP121   ','SX221   ','SY221   ',
     .'SZ221   ','SXY221  ','SYZ221  ','SXZ221  ','EP221   ','SX321   ',
     .'SY321   ','SZ321   ','SXY321  ','SYZ321  ','SXZ321  ','EP321   ',
     .'SX131   ','SY131   ','SZ131   ','SXY131  ','SYZ131  ','SXZ131  ',
     .'EP131   ','SX231   ','SY231   ','SZ231   ','SXY231  ','SYZ331  ',
     .'SXZ231  ','EP231   ','SX331   ','SY331   ','SZ331   ','SXY331  ',
     .'SYZ331  ','SXZ331  ','EP331   ','SX141   ','SY141   ','SZ141   ',
     .'SXY141  ','SYZ141  ','SXZ141  ','EP141   ','SX241   ','SY241   ',
     .'SZ241   ','SXY241  ','SYZ241  ','SXZ241  ','EP241   ','SX341   ',
     .'SY341   ','SZ341   ','SXY341  ','SYZ341  ','SXZ341  ','EP341   ',
     .'SX151   ','SY151   ','SZ151   ','SXY151  ','SYZ151  ','SXZ151  ',
     .'EP151   ','SX251   ','SY251   ','SZ251   ','SXY251  ','SYZ251  ',
     .'SXZ251  ','EP251   ','SX351   ','SY351   ','SZ351   ','SXY351  ',
     .'SYZ351  ','SXZ351  ','EP351   ','SX161   ','SY161   ','SZ161   ',
     .'SXY161  ','SYZ161  ','SXZ161  ','EP161   ','SX261   ','SY261   ',
     .'SZ261   ','SXY261  ','SYZ261  ','SXZ261  ','EP261   ','SX361   ',
     .'SY361   ','SZ361   ','SXY361  ','SYZ361  ','SXZ361  ','EP361   ',
     .'SX171   ','SY171   ','SZ171   ','SXY171  ','SYZ171  ','SXZ171  ',
     .'EP171   ','SX271   ','SY271   ','SZ271   ','SXY271  ','SYZ271  ',
     .'SXZ271  ','EP271   ','SX371   ','SY371   ','SZ371   ','SXY371  ',
     .'SYZ371  ','SXZ371  ','EP371   ','SX181   ','SY181   ','SZ181   ',
     .'SXY181  ','SYZ181  ','SXZ181  ','EP181   ','SX281   ','SY281   ',
     .'SZ281   ','SXY281  ','SYZ281  ','SXZ281  ','EP281   ','SX381   ',
     .'SY381   ','SZ381   ','SXY381  ','SYZ381  ','SXZ381  ','EP381   ',
     .'SX191   ','SY191   ','SZ191   ','SXY191  ','SYZ191  ','SXZ191  ',
     .'EP191   ','SX291   ','SY291   ','SZ291   ','SXY291  ','SYZ291  ',
     .'SXZ291  ','EP291   ','SX391   ','SY391   ','SZ391   ','SXY391  ',
     .'SYZ391  ','SXZ391  ','EP391   ','SX112   ','SY112   ','SZ112   ',
     .'SXY112  ','SYZ112  ','SXZ112  ','EP112   ','SX212   ','SY212   ',
     .'SZ212   ','SXY212  ','SYZ212  ','SXZ212  ','EP212   ','SX312   ',
     .'SY312   ','SZ112   ','SXY112  ','SYZ312  ','SXZ112  ','EP312   ',
     .'SX122   ','SY122   ','SZ122   ','SXY122  ','SYZ122  ','SXZ122  ',
     .'EP122   ','SX222   ','SY222   ','SZ222   ','SXY222  ','SYZ222  ',
     .'SXZ222  ','EP222   ','SX322   ','SY322   ','SZ322   ','SXY322  ',
     .'SYZ322  ','SXZ322  ','EP322   ','SX132   ','SY132   ','SZ132   ',
     .'SXY132  ','SYZ132  ','SXZ132  ','EP132   ','SX232   ','SY232   ',
     .'SZ232   ','SXY232  ','SYZ332  ','SXZ232  ','EP232   ','SX332   ',
     .'SY332   ','SZ332   ','SXY332  ','SYZ332  ','SXZ332  ','EP332   ',
     .'SX142   ','SY142   ','SZ142   ','SXY142  ','SYZ142  ','SXZ142  ',
     .'EP142   ','SX242   ','SY242   ','SZ242   ','SXY242  ','SYZ242  ',
     .'SXZ242  ','EP242   ','SX342   ','SY342   ','SZ342   ','SXY342  ',
     .'SYZ342  ','SXZ342  ','EP342   ','SX152   ','SY152   ','SZ152   ',
     .'SXY152  ','SYZ152  ','SXZ152  ','EP152   ','SX252   ','SY252   ',
     .'SZ252   ','SXY252  ','SYZ252  ','SXZ252  ','EP252   ','SX352   ',
     .'SY352   ','SZ352   ','SXY352  ','SYZ352  ','SXZ352  ','EP352   ',
     .'SX162   ','SY162   ','SZ162   ','SXY162  ','SYZ162  ','SXZ162  ',
     .'EP162   ','SX262   ','SY262   ','SZ262   ','SXY262  ','SYZ262  ',
     .'SXZ262  ','EP262   ','SX362   ','SY362   ','SZ362   ','SXY362  ',
     .'SYZ362  ','SXZ362  ','EP362   ','SX172   ','SY172   ','SZ172   ',
     .'SXY172  ','SYZ172  ','SXZ172  ','EP172   ','SX272   ','SY272   ',
     .'SZ272   ','SXY272  ','SYZ272  ','SXZ272  ','EP272   ','SX372   ',
     .'SY372   ','SZ372   ','SXY372  ','SYZ372  ','SXZ372  ','EP372   ',
     .'SX182   ','SY182   ','SZ182   ','SXY182  ','SYZ182  ','SXZ182  ',
     .'EP182   ','SX282   ','SY282   ','SZ282   ','SXY282  ','SYZ282  ',
     .'SXZ282  ','EP282   ','SX382   ','SY382   ','SZ382   ','SXY382  ',
     .'SYZ382  ','SXZ382  ','EP382   ','SX192   ','SY192   ','SZ192   ',
     .'SXY192  ','SYZ192  ','SXZ192  ','EP192   ','SX292   ','SY292   ',
     .'SZ292   ','SXY292  ','SYZ292  ','SXZ292  ','EP292   ','SX392   ',
     .'SY392   ','SZ392   ','SXY392  ','SYZ392  ','SXZ392  ','EP392   ',
     .'SX113   ','SY113   ','SZ113   ','SXY113  ','SYZ113  ','SXZ113  ',
     .'EP113   ','SX213   ','SY213   ','SZ213   ','SXY213  ','SYZ213  ',
     .'SXZ213  ','EP213   ','SX313   ','SY313   ','SZ113   ','SXY113  ',
     .'SYZ313  ','SXZ113  ','EP313   ','SX123   ','SY123   ','SZ123   ',
     .'SXY123  ','SYZ123  ','SXZ123  ','EP123   ','SX223   ','SY223   ',
     .'SZ223   ','SXY223  ','SYZ223  ','SXZ223  ','EP223   ','SX323   ',
     .'SY323   ','SZ323   ','SXY323  ','SYZ323  ','SXZ323  ','EP323   ',
     .'SX133   ','SY133   ','SZ133   ','SXY133  ','SYZ133  ','SXZ133  ',
     .'EP133   ','SX233   ','SY233   ','SZ233   ','SXY233  ','SYZ333  ',
     .'SXZ233  ','EP233   ','SX333   ','SY333   ','SZ333   ','SXY333  ',
     .'SYZ333  ','SXZ333  ','EP333   ','SX143   ','SY143   ','SZ143   ',
     .'SXY143  ','SYZ143  ','SXZ143  ','EP143   ','SX243   ','SY243   ',
     .'SZ243   ','SXY243  ','SYZ243  ','SXZ243  ','EP243   ','SX343   ',
     .'SY343   ','SZ343   ','SXY343  ','SYZ343  ','SXZ343  ','EP343   ',
     .'SX153   ','SY153   ','SZ153   ','SXY153  ','SYZ153  ','SXZ153  ',
     .'EP153   ','SX253   ','SY253   ','SZ253   ','SXY253  ','SYZ253  ',
     .'SXZ253  ','EP253   ','SX353   ','SY353   ','SZ353   ','SXY353  ',
     .'SYZ353  ','SXZ353  ','EP353   ','SX163   ','SY163   ','SZ163   ',
     .'SXY163  ','SYZ163  ','SXZ163  ','EP163   ','SX263   ','SY263   ',
     .'SZ263   ','SXY263  ','SYZ263  ','SXZ263  ','EP263   ','SX363   ',
     .'SY363   ','SZ363   ','SXY363  ','SYZ363  ','SXZ363  ','EP363   ',
     .'SX173   ','SY173   ','SZ173   ','SXY173  ','SYZ173  ','SXZ173  ',
     .'EP173   ','SX273   ','SY273   ','SZ273   ','SXY273  ','SYZ273  ',
     .'SXZ273  ','EP273   ','SX373   ','SY373   ','SZ373   ','SXY373  ',
     .'SYZ373  ','SXZ373  ','EP373   ','SX183   ','SY183   ','SZ183   ',
     .'SXY183  ','SYZ183  ','SXZ183  ','EP183   ','SX283   ','SY283   ',
     .'SZ283   ','SXY283  ','SYZ283  ','SXZ283  ','EP283   ','SX383   ',
     .'SY383   ','SZ383   ','SXY383  ','SYZ383  ','SXZ383  ','EP383   ',
     .'SX193   ','SY193   ','SZ193   ','SXY193  ','SYZ193  ','SXZ193  ',
     .'EP193   ','SX293   ','SY293   ','SZ293   ','SXY293  ','SYZ293  ',
     .'SXZ293  ','EP293   ','SX393   ','SY393   ','SZ393   ','SXY393  ',
     .'SYZ393  ','SXZ393  ','EP393   '/
      DATA VARS3/
     .'SX1U1   ','SY1U1   ','SZ1U1   ','SXY1U1  ','SYZ1U1  ','SXZ1U1  ',
     .'EP1U1   ','SX2U1   ','SY2U1   ','SZ2U1   ','SXY2U1  ','SYZ2U1  ',
     .'SXZ2U1  ','EP2U1   ','SX3U1   ','SY3U1   ','SZ3U1   ','SXY3U1  ',
     .'SYZ3U1  ','SXZ3U1  ','EP3U1   ','SX1U2   ','SY1U2   ','SZ1U2   ',
     .'SXY1U2  ','SYZ1U2  ','SXZ1U2  ','EP1U2   ','SX2U2   ','SY2U2   ',
     .'SZ2U2   ','SXY2U2  ','SYZ2U2  ','SXZ2U2  ','EP2U2   ','SX3U2   ',
     .'SY3U2   ','SZ3U2   ','SXY3U2  ','SYZ3U2  ','SXZ3U2  ','EP3U2   ',
     .'SX1U3   ','SY1U3   ','SZ1U3   ','SXY1U3  ','SYZ1U3  ','SXZ1U3  ',
     .'EP1U3   ','SX2U3   ','SY2U3   ','SZ2U3   ','SXY2U3  ','SYZ3U3  ',
     .'SXZ2U3  ','EP2U3   ','SX3U3   ','SY3U3   ','SZ3U3   ','SXY3U3  ',
     .'SYZ3U3  ','SXZ3U3  ','EP3U3   ','SX1D1   ','SY1D1   ','SZ1D1   ',
     .'SXY1D1  ','SYZ1D1  ','SXZ1D1  ','EP1D1   ','SX2D1   ','SY2U1   ',
     .'SZ2D1   ','SXY2D1  ','SYZ2D1  ','SXZ2D1  ','EP2D1   ','SX3D1   ',
     .'SY3D1   ','SZ3D1   ','SXY3D1  ','SYZ3D1  ','SXZ3D1  ','EP3D1   ',
     .'SX1D2   ','SY1D2   ','SZ1D2   ','SXY1D2  ','SYZ1D2  ','SXZ1D2  ',
     .'EP1D2   ','SX2D2   ','SY2D2   ','SZ2D2   ','SXY2D2  ','SYZ2D2  ',
     .'SXZ2D2  ','EP2D2   ','SX3D2   ','SY3D2   ','SZ3D2   ','SXY3D2  ',
     .'SYZ3D2  ','SXZ3D2  ','EP3D2   ','SX1D3   ','SY1D3   ','SZ1D3   ',
     .'SXY1D3  ','SYZ1D3  ','SXZ1D3  ','EP1D3   ','SX2D3   ','SY2D3   ',
     .'SZ2D3   ','SXY2D3  ','SYZ3D3  ','SXZ2D3  ','EP2D3   ','SX3D3   ',
     .'SY3D3   ','SZ3D3   ','SXY3D3  ','SYZ3D3  ','SXZ3D3  ','EP3D3   ',
     .'USR1_111','USR2_111','USR3_111','USR4_111','USR5_111','USR6_111',
     .'USR7_111','USR8_111','USR9_111','USR1_211','USR2_211','USR3_211',
     .'USR4_211','USR5_211','USR6_211','USR7_211','USR8_211','USR9_211',
     .'USR1_311','USR3_311','USR3_311','USR4_311','USR5_311','USR6_311',
     .'USR7_311','USR8_311','USR9_311','USR1_121','USR2_121','USR3_121',
     .'USR4_121','USR5_121','USR6_121','USR7_121','USR8_121','USR9_121',
     .'USR1_221','USR2_221','USR3_221','USR4_221','USR5_221','USR6_221',
     .'USR7_221','USR8_221','USR9_221','USR1_321','USR3_321','USR3_321',
     .'USR4_321','USR5_321','USR6_321','USR7_321','USR8_321','USR9_321',
     .'USR1_131','USR2_131','USR3_131','USR4_131','USR5_131','USR6_131',
     .'USR7_131','USR8_131','USR9_131','USR1_231','USR2_231','USR3_231',
     .'USR4_231','USR5_231','USR6_231','USR7_231','USR8_231','USR9_231',
     .'USR1_331','USR3_331','USR3_331','USR4_331','USR5_331','USR6_331',
     .'USR7_331','USR8_331','USR9_331','USR1_141','USR2_141','USR3_141',
     .'USR4_141','USR5_141','USR6_141','USR7_141','USR8_141','USR9_141',
     .'USR1_241','USR2_241','USR3_241','USR4_241','USR5_241','USR6_241',
     .'USR7_241','USR8_241','USR9_241','USR1_341','USR3_341','USR3_341',
     .'USR4_341','USR5_341','USR6_341','USR7_341','USR8_341','USR9_341',
     .'USR1_151','USR2_151','USR3_151','USR4_151','USR5_151','USR6_151',
     .'USR7_151','USR8_151','USR9_151','USR1_251','USR2_251','USR3_251',
     .'USR4_251','USR5_251','USR6_251','USR7_251','USR8_251','USR9_251',
     .'USR1_351','USR3_351','USR3_351','USR4_351','USR5_351','USR6_351',
     .'USR7_351','USR8_351','USR9_351','USR1_161','USR2_161','USR3_161',
     .'USR4_161','USR5_161','USR6_161','USR7_161','USR8_161','USR9_161',
     .'USR1_261','USR2_261','USR3_261','USR4_261','USR5_261','USR6_261',
     .'USR7_261','USR8_261','USR9_261','USR1_361','USR3_361','USR3_361',
     .'USR4_361','USR5_361','USR6_361','USR7_361','USR8_361','USR9_361',
     .'USR1_171','USR2_171','USR3_171','USR4_171','USR5_171','USR6_171',
     .'USR7_171','USR8_171','USR9_171','USR1_271','USR2_271','USR3_271',
     .'USR4_271','USR5_271','USR6_271','USR7_271','USR8_271','USR9_271',
     .'USR1_371','USR3_371','USR3_371','USR4_371','USR5_371','USR6_371',
     .'USR7_371','USR8_371','USR9_371','USR1_181','USR2_181','USR3_181',
     .'USR4_181','USR5_171','USR6_171','USR7_181','USR8_181','USR9_181',
     .'USR1_281','USR2_281','USR3_281','USR4_281','USR5_271','USR6_271',
     .'USR7_281','USR8_281','USR9_281','USR1_381','USR3_381','USR3_381',
     .'USR4_381','USR5_371','USR6_371','USR7_381','USR8_381','USR9_381',
     .'USR1_191','USR2_191','USR3_191','USR4_191','USR5_191','USR6_191',
     .'USR7_191','USR8_191','USR9_191','USR1_291','USR2_291','USR3_291',
     .'USR4_291','USR5_291','USR6_291','USR7_291','USR8_291','USR9_291',
     .'USR1_391','USR3_391','USR3_391','USR4_391','USR5_391','USR6_391',
     .'USR7_391','USR8_391','USR9_391'/
      DATA VARS4/
     .'USR1_112','USR2_112','USR3_112','USR4_112','USR5_112','USR6_112',
     .'USR7_112','USR8_112','USR9_112',
     .'USR1_212','USR2_212','USR3_212','USR4_212','USR5_212','USR6_212',
     .'USR7_212','USR8_212','USR9_212','USR1_312','USR3_312','USR3_312',
     .'USR4_312','USR5_312','USR6_312','USR7_312','USR8_312','USR9_312',
     .'USR1_122','USR2_122','USR3_122','USR4_122','USR5_122','USR6_122',
     .'USR7_122','USR8_122','USR9_122','USR1_222','USR2_222','USR3_222',
     .'USR4_222','USR5_222','USR6_222','USR7_222','USR8_222','USR9_222',
     .'USR1_322','USR3_322','USR3_322','USR4_322','USR5_322','USR6_322',
     .'USR7_322','USR8_322','USR9_322','USR1_132','USR2_132','USR3_132',
     .'USR4_132','USR5_132','USR6_132','USR7_132','USR8_132','USR9_132',
     .'USR1_232','USR2_232','USR3_232','USR4_232','USR5_232','USR6_232',
     .'USR7_232','USR8_232','USR9_232','USR1_332','USR3_332','USR3_332',
     .'USR4_332','USR5_332','USR6_332','USR7_332','USR8_332','USR9_332',
     .'USR1_142','USR2_142','USR3_142','USR4_142','USR5_142','USR6_142',
     .'USR7_142','USR8_142','USR9_142','USR1_242','USR2_242','USR3_242',
     .'USR4_242','USR5_242','USR6_242','USR7_242','USR8_242','USR9_242',
     .'USR1_342','USR3_342','USR3_342','USR4_342','USR5_342','USR6_342',
     .'USR7_342','USR8_342','USR9_342','USR1_152','USR2_152','USR3_152',
     .'USR4_152','USR5_152','USR6_152','USR7_152','USR8_152','USR9_152',
     .'USR1_252','USR2_252','USR3_252','USR4_252','USR5_252','USR6_252',
     .'USR7_252','USR8_252','USR9_252','USR1_352','USR3_352','USR3_352',
     .'USR4_352','USR5_352','USR6_352','USR7_352','USR8_352','USR9_352',
     .'USR1_162','USR2_162','USR3_162','USR4_162','USR5_162','USR6_162',
     .'USR7_162','USR8_162','USR9_162','USR1_262','USR2_262','USR3_262',
     .'USR4_262','USR5_262','USR6_262','USR7_262','USR8_262','USR9_262',
     .'USR1_362','USR3_362','USR3_362','USR4_362','USR5_362','USR6_362',
     .'USR7_362','USR8_362','USR9_362','USR1_172','USR2_172','USR3_172',
     .'USR4_172','USR5_172','USR6_172','USR7_172','USR8_172','USR9_172',
     .'USR1_272','USR2_272','USR3_272','USR4_272','USR5_272','USR6_272',
     .'USR7_272','USR8_272','USR9_272','USR1_372','USR3_372','USR3_372',
     .'USR4_372','USR5_372','USR6_372','USR7_372','USR8_372','USR9_372',
     .'USR1_182','USR2_182','USR3_182','USR4_182','USR5_182','USR6_182',
     .'USR7_182','USR8_182','USR9_182','USR1_282','USR2_282','USR3_282',
     .'USR4_282','USR5_282','USR6_282','USR7_282','USR8_282','USR9_282',
     .'USR1_382','USR3_382','USR3_382','USR4_382','USR5_382','USR6_382',
     .'USR7_382','USR8_382','USR9_382','USR1_192','USR2_192','USR3_192',
     .'USR4_192','USR5_192','USR6_192','USR7_192','USR8_192','USR9_192',
     .'USR1_292','USR2_292','USR3_292','USR4_292','USR5_292','USR6_292',
     .'USR7_292','USR8_292','USR9_292','USR1_392','USR3_392','USR3_392',
     .'USR4_392','USR5_392','USR6_392','USR7_392','USR8_392','USR9_392',
     .'USR1_113','USR2_113','USR3_113','USR4_113','USR5_113','USR6_113',
     .'USR7_113','USR8_113','USR9_113','USR1_213','USR2_213','USR3_213',
     .'USR4_213','USR5_213','USR6_213','USR7_213','USR8_213','USR9_213',
     .'USR1_313','USR3_313','USR3_313','USR4_313','USR5_313','USR6_313',
     .'USR7_313','USR8_313','USR9_313','USR1_123','USR2_123','USR3_123',
     .'USR4_123','USR5_123','USR6_123','USR7_123','USR8_123','USR9_123',
     .'USR1_223','USR2_223','USR3_223','USR4_223','USR5_223','USR6_223',
     .'USR7_223','USR8_223','USR9_223','USR1_323','USR3_323','USR3_323',
     .'USR4_323','USR5_323','USR6_323','USR7_323','USR8_323','USR9_323',
     .'USR1_133','USR2_133','USR3_133','USR4_133','USR5_133','USR6_133',
     .'USR7_133','USR8_133','USR9_133','USR1_233','USR2_233','USR3_233',
     .'USR4_233','USR5_233','USR6_233','USR7_233','USR8_233','USR9_233',
     .'USR1_333','USR3_333','USR3_333','USR4_333','USR5_333','USR6_333',
     .'USR7_333','USR8_333','USR9_333','USR1_143','USR2_143','USR3_143',
     .'USR4_143','USR5_143','USR6_143','USR7_143','USR8_143','USR9_143',
     .'USR1_243','USR2_243','USR3_243','USR4_243','USR5_243','USR6_243',
     .'USR7_243','USR8_243','USR9_243','USR1_343','USR3_343','USR3_343',
     .'USR4_343','USR5_343','USR6_343','USR7_343','USR8_343','USR9_343',
     .'USR1_153','USR2_153','USR3_153','USR4_153','USR5_153','USR6_153',
     .'USR7_153','USR8_153','USR9_153','USR1_253','USR2_253','USR3_253',
     .'USR4_253','USR5_253','USR6_253','USR7_253','USR8_253','USR9_253',
     .'USR1_353','USR3_353','USR3_353','USR4_353','USR5_353','USR6_353',
     .'USR7_353','USR8_353','USR9_353','USR1_163','USR2_163','USR3_163',
     .'USR4_163','USR5_163','USR6_163','USR7_163','USR8_163','USR9_163',
     .'USR1_263','USR2_263','USR3_263','USR4_263','USR5_263','USR6_263',
     .'USR7_263','USR8_263','USR9_263','USR1_363','USR3_363','USR3_363',
     .'USR4_363','USR5_363','USR6_363','USR7_363','USR8_363','USR9_363',
     .'USR1_173','USR2_173','USR3_173','USR4_173','USR5_173','USR6_173',
     .'USR7_173','USR8_173','USR9_173','USR1_273','USR2_273','USR3_273',
     .'USR4_273','USR5_273','USR6_273','USR7_273','USR8_273','USR9_273',
     .'USR1_373','USR3_373','USR3_373','USR4_373','USR5_373','USR6_373',
     .'USR7_373','USR8_373','USR9_373','USR1_183','USR2_183','USR3_183',
     .'USR4_183','USR5_183','USR6_183','USR7_183','USR8_183','USR9_183',
     .'USR1_283','USR2_283','USR3_283','USR4_283','USR5_283','USR6_283',
     .'USR7_283','USR8_283','USR9_283','USR1_383','USR3_383','USR3_383',
     .'USR4_383','USR5_383','USR6_383','USR7_383','USR8_383','USR9_383',
     .'USR1_193','USR2_193','USR3_193','USR4_193','USR5_193','USR6_193',
     .'USR7_193','USR8_193','USR9_193','USR1_293','USR2_293','USR3_293',
     .'USR4_293','USR5_293','USR6_293','USR7_293','USR8_293','USR9_293',
     .'USR1_393','USR3_393','USR3_393','USR4_393','USR5_393','USR6_393',
     .'USR7_393','USR8_393','USR9_393',
     .'EPSXX   ','EPSYY   ','EPSZZ   ','EPSXY   ','EPSZX   ','EPSYZ   '/
      DATA VARS5/
     .'SX_MAT1 ','SY_MAT1 ','SZ_MAT1 ','SXY_MAT1','SYZ_MAT1','SXZ_MAT1',
     .'IE_MAT1 ','DEN_MAT1','VOL_MAT1','PLA_MAT1','TEM_MAT1',
     .'SX_MAT2 ','SY_MAT2 ','SZ_MAT2 ','SXY_MAT2','SYZ_MAT2','SXZ_MAT2',
     .'IE_MAT2 ','DEN_MAT2','VOL_MAT2','PLA_MAT2','TEM_MAT2'/
C Definition of VARS6 : strain for /PROP/TYPE22
C=======================================================================
      DO J=1,200
        IF (J <= 9) THEN
          WRITE(CHJ,'(I1.1)')J
        ELSEIF (J <= 99) THEN
          WRITE(CHJ,'(I2.2)')J
        ELSE
          WRITE(CHJ,'(I3.3)')J
        ENDIF
        DO I=1,9
          WRITE(CHI,'(I1.1)')I
          DO K=1,9
            WRITE(CHK,'(I1.1)')K
            DO L=1,6
              IF (L==1) VAR_TMP(1:5)='E11_'//CHI
              IF (L==2) VAR_TMP(1:5)='E12_'//CHI
              IF (L==3) VAR_TMP(1:5)='E13_'//CHI
              IF (L==4) VAR_TMP(1:5)='E22_'//CHI
              IF (L==5) VAR_TMP(1:5)='E23_'//CHI
              IF (L==6) VAR_TMP(1:5)='E33_'//CHI
              VAR_TMP=VAR_TMP(1:5)//CHK//'_'//CHJ
              VARS6((J-1)*9*9*6+((I-1)*9+K-1)*6+L)=VAR_TMP
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C Definition of VARS7 : stress for /PROP/TYPE22
      DO J=1,200
        IF (J <= 9) THEN
          WRITE(CHJ,'(I1.1)')J
        ELSEIF (J <= 99) THEN
          WRITE(CHJ,'(I2.2)')J
        ELSE
          WRITE(CHJ,'(I3.3)')J
        ENDIF
        DO I=1,9
          WRITE(CHI,'(I1.1)')I
          DO K=1,9
            WRITE(CHK,'(I1.1)')K
            DO L=1,6
              IF (L==1) VAR_TMP(1:5)='S11_'//CHI
              IF (L==2) VAR_TMP(1:5)='S12_'//CHI
              IF (L==3) VAR_TMP(1:5)='S13_'//CHI
              IF (L==4) VAR_TMP(1:5)='S22_'//CHI
              IF (L==5) VAR_TMP(1:5)='S23_'//CHI
              IF (L==6) VAR_TMP(1:5)='S33_'//CHI
              VAR_TMP=VAR_TMP(1:5)//CHK//'_'//CHJ
              VARS7((J-1)*9*9*6+((I-1)*9+K-1)*6+L)=VAR_TMP
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C Definition of VARS8 : strain matrix output

      VARS8(1) = 'LEPSX  '
      VARS8(2) = 'LEPSY    '
      VARS8(3) = 'LEPSZ   '
      VARS8(4) = 'LEPSXY  '
      VARS8(5) = 'LEPSYZ  '
      VARS8(6) = 'LEPSZX  '
      VARS8(7) = 'EPSX1   '
      VARS8(8) = 'EPSX2   '
      VARS8(9) = 'EPSX3   '
      VARS8(10) = 'EPSX4   '
      VARS8(11) = 'EPSY1   '
      VARS8(12) = 'EPSY2   '
      VARS8(13) = 'EPSY3   '
      VARS8(14) = 'EPSY4   '
      VARS8(15) = 'EPSZ1   '
      VARS8(16) = 'EPSZ2   '
      VARS8(17) = 'EPSZ3   '
      VARS8(18) = 'EPSZ4   '
      VARS8(19) = 'EPSXY1  '
      VARS8(20) = 'EPSXY2  '
      VARS8(21) = 'EPSXY3  '
      VARS8(22) = 'EPSXY4  '
      VARS8(23) = 'EPSYZ1  '
      VARS8(24) = 'EPSYZ2  '
      VARS8(25) = 'EPSYZ3  '
      VARS8(26) = 'EPSYZ4  '
      VARS8(27) = 'EPSZX1  '
      VARS8(28) = 'EPSZX2  '
      VARS8(29) = 'EPSZX3  '
      VARS8(30) = 'EPSZX4  '

      DO K=1,3
        DO J=1,9
          DO I=1,3
             WRITE(CHI,'(I1.1)')I
             WRITE(CHJS,'(I1.1)')J
             WRITE(CHK,'(I1.1)')K
             VAR_TMP = 'EPSX'
             VARS8(30+(k-1)*9*6*3+(J-1)*3*6+(I-1)*6+1)=VAR_TMP(1:4)//CHI//CHJS//CHK
             VAR_TMP = 'EPSY'
             VARS8(30+(k-1)*9*6*3+(J-1)*3*6+(I-1)*6+2)=VAR_TMP(1:4)//CHI//CHJS//CHK
             VAR_TMP = 'EPSZ'
             VARS8(30+(k-1)*9*6*3+(J-1)*3*6+(I-1)*6+3)=VAR_TMP(1:4)//CHI//CHJS//CHK
             VAR_TMP = 'EPSXY'
             VARS8(30+(k-1)*9*6*3+(J-1)*3*6+(I-1)*6+4)=VAR_TMP(1:5)//CHI//CHJS//CHK
             VAR_TMP = 'EPSYZ'
             VARS8(30+(k-1)*9*6*3+(J-1)*3*6+(I-1)*6+5)=VAR_TMP(1:5)//CHI//CHJS//CHK
             VAR_TMP = 'EPSZX'
             VARS8(30+(k-1)*9*6*3+(J-1)*3*6+(I-1)*6+6)=VAR_TMP(1:5)//CHI//CHJS//CHK

          ENDDO
        ENDDO
      ENDDO

      DATA VARS9/
     . 'VX      ','VY      ','VZ      ','SSP     ','MACH    ',
     . 'VK      '/

      DATA VARSNLOC/
     . 'NL_PLAS ','NL_PLSR '/

C   shells
c  VARC = VARC1 + VARC2 + VARC3
      DATA VARC1/
     . 'F1      ','F2      ','F12     ','Q1      ','Q2      ',
     . 'M1      ','M2      ','M12     ','IEM     ','IEB     ',
     . 'OFF     ','THIC    ','EMIN    ','EMAX    ','E1      ',
     . 'E2      ','E12     ','SH1     ','SH2     ','K1      ',
     .'K2      ','K12     ','EPSD    ',
     .'USR1    ','USR2    ','USR3    ','USR4    ','USR5    ','USR6    ',
     .'USR7    ','USR8    ','USR9    ','USR10   ','USR11   ','USR12   ',
     .'USR13   ','USR14   ','USR15   ','USR16   ','USR17   ','USR18   ',
     .'USR19   ','USR20   ','USR21   ','USR22   ','USR23   ','USR24   ',
     .'USR25   ','USR26   ','USR27   ','USR28   ','USR29   ','USR30   ',
     .'USR31   ','USR32   ','USR33   ','USR34   ','USR35   ','USR36   ',
     .'USR37   ','USR38   ','USR39   ','USR40   ','USR41   ','USR42   ',
     .'USR43   ','USR44   ','USR45   ','USR46   ','USR47   ','USR48   ',
     .'USR49   ','USR50   ','USR51   ','USR52   ','USR53   ','USR54   ',
     .'USR55   ','USR56   ','USR57   ','USR58   ','USR59   ','USR60   ',
     .'USR1_1  ','USR1_2  ','USR1_3  ','USR1_4  ','USR1_5  ','USR2_1  ',
     .'USR2_2  ','USR2_3  ','USR2_4  ','USR2_5  ','USR3_1  ','USR3_2  ',
     .'USR3_3  ','USR3_4  ','USR3_5  ','USR4_1  ','USR4_2  ','USR4_3  ',
     .'USR4_4  ','USR4_5  ','USR5_1  ','USR5_2  ','USR5_3  ','USR5_4  ',
     .'USR5_5  ','USR6_1  ','USR6_2  ','USR6_3  ','USR6_4  ','USR6_5  ',
     .'USR7_1  ','USR7_2  ','USR7_3  ','USR7_4  ','USR7_5  ','USR8_1  ',
     .'USR8_2  ','USR8_3  ','USR8_4  ','USR8_5  ','USR9_1  ','USR9_2  ',
     .'USR9_3  ','USR9_4  ','USR9_5  ','USR10_1 ','USR10_2 ','USR10_3 ',
     .'USR10_4 ','USR10_5 ','USR11_1 ','USR11_2 ','USR11_3 ','USR11_4 ',
     .'USR11_5 ','USR12_1 ','USR12_2 ','USR12_3 ','USR12_4 ','USR12_5 ',
     .'USR13_1 ','USR13_2 ','USR13_3 ','USR13_4 ','USR13_5 ','USR14_1 ',
     .'USR14_2 ','USR14_3 ','USR14_4 ','USR14_5 ','USR15_1 ','USR15_2 ',
     .'USR15_3 ','USR15_4 ','USR15_5 ','USR16_1 ','USR16_2 ','USR16_3 ',
     .'USR16_4 ','USR16_5 ','USR17_1 ','USR17_2 ','USR17_3 ','USR17_4 ',
     .'USR17_5 ','USR18_1 ','USR18_2 ','USR18_3 ','USR18_4 ','USR18_5 ',
     .'USR19_1 ','USR19_2 ','USR19_3 ','USR19_4 ','USR19_5 ','USR20_1 ',
     .'USR20_2 ','USR20_3 ','USR20_4 ','USR20_5 ','SX_1    ','SY_1    ',
     .'SXY_1   ','SYZ_1   ','SZX_1   ','SX_2    ','SY_2    ','SXY_2   ',
     .'SYZ_2   ','SZX_2   ','SX_3    ','SY_3    ','SXY_3   ','SYZ_3   ',
     .'SZX_3   ','SX_4    ','SY_4    ','SXY_4   ','SYZ_4   ','SZX_4   ',
     .'SX_5    ','SY_5    ','SXY_5   ','SYZ_5   ','SZX_5   '/
      DATA VARC2/
     .'SX_6    ','SY_6    ','SXY_6   ','SYZ_6   ','SZX_6   ',
     .'SX_7    ','SY_7    ','SXY_7   ','SYZ_7   ','SZX_7   ','SX_8    ',
     .'SY_8    ','SXY_8   ','SYZ_8   ','SZX_8   ','SX_9    ','SY_9    ',
     .'SXY_9   ','SYZ_9   ','SZX_9   ','SX_10   ','SY_10   ','SXY_10  ',
     .'SYZ_10  ','SZX_10  ','SX_11   ','SY_11   ','SXY_11  ','SYZ_11  ',
     .'SZX_11  ','SX_12   ','SY_12   ','SXY_12  ','SYZ_12  ','SZX_12  ',
     .'SX_13   ','SY_13   ','SXY_13  ','SYZ_13  ','SZX_13  ','SX_14   ',
     .'SY_14   ','SXY_14  ','SYZ_14  ','SZX_14  ','SX_15   ','SY_15   ',
     .'SXY_15  ','SYZ_15  ','SZX_15  ','SX_16   ','SY_16   ','SXY_16  ',
     .'SYZ_16  ','SZX_16  ','SX_17   ','SY_17   ','SXY_17  ','SYZ_17  ',
     .'SZX_17  ','SX_18   ','SY_18   ','SXY_18  ','SYZ_18  ','SZX_18  ',
     .'SX_19   ','SY_19   ','SXY_19  ','SYZ_19  ','SZX_19  ','SX_20   ',
     .'SY_20   ','SXY_20  ','SYZ_20  ','SZX_20  ','SX_21   ','SY_21   ',
     .'SXY_21  ','SYZ_21  ','SZX_21  ','SX_22   ','SY_22   ','SXY_22  ',
     .'SYZ_22  ','SZX_22  ','SX_23   ','SY_23   ','SXY_23  ','SYZ_23  ',
     .'SZX_23  ','SX_24   ','SY_24   ','SXY_24  ','SYZ_24  ','SZX_24  ',
     .'SX_25   ','SY_25   ','SXY_25  ','SYZ_25  ','SZX_25  ','SX_26   ',
     .'SY_26   ','SXY_26  ','SYZ_26  ','SZX_26  ','SX_27   ','SY_27   ',
     .'SXY_27  ','SYZ_27  ','SZX_27  ','SX_28   ','SY_28   ','SXY_28  ',
     .'SYZ_28  ','SZX_28  ','SX_29   ','SY_29   ','SXY_29  ','SYZ_29  ',
     .'SZX_29  ','SX_30   ','SY_30   ','SXY_30  ','SYZ_30  ','SZX_30  ',
     .'SX_31   ','SY_31   ','SXY_31  ','SYZ_31  ','SZX_31  ','SX_32   ',
     .'SY_32   ','SXY_32  ','SYZ_32  ','SZX_32  ','SX_33   ','SY_33   ',
     .'SXY_33  ','SYZ_33  ','SZX_33  ','SX_34   ','SY_34   ','SXY_34  ',
     .'SYZ_34  ','SZX_34  ','SX_35   ','SY_35   ','SXY_35  ','SYZ_35  ',
     .'SZX_35  ','SX_36   ','SY_36   ','SXY_36  ','SYZ_36  ','SZX_36  ',
     .'SX_37   ','SY_37   ','SXY_37  ','SYZ_37  ','SZX_37  ','SX_38   ',
     .'SY_38   ','SXY_38  ','SYZ_38  ','SZX_38  ','SX_39   ','SY_39   ',
     .'SXY_39  ','SYZ_39  ','SZX_39  ','SX_40   ','SY_40   ','SXY_40  ',
     .'SYZ_40  ','SZX_40  ','SX_41   ','SY_41   ','SXY_41  ','SYZ_41  ',
     .'SZX_41  ','SX_42   ','SY_42   ','SXY_42  ','SYZ_42  ','SZX_42  ',
     .'SX_43   ','SY_43   ','SXY_43  ','SYZ_43  ','SZX_43  ','SX_44   ',
     .'SY_44   ','SXY_44  ','SYZ_44  ','SZX_44  ','SX_45   ','SY_45   ',
     .'SXY_45  ','SYZ_45  ','SZX_45  ','SX_46   ','SY_46   ','SXY_46  ',
     .'SYZ_46  ','SZX_46  ','SX_47   ','SY_47   ','SXY_47  ','SYZ_47  ',
     .'SZX_47  ','SX_48   ','SY_48   ','SXY_48  ','SYZ_48  ','SZX_48  ',
     .'SX_49   ','SY_49   ','SXY_49  ','SYZ_49  ','SZX_49  ','SX_50   ',
     .'SY_50   ','SXY_50  ','SYZ_50  ','SZX_50  ','SX_51   ','SY_51   ',
     .'SXY_51  ','SYZ_51  ','SZX_51  ','SX_52   ','SY_52   ','SXY_52  ',
     .'SYZ_52  ','SZX_52  ','SX_53   ','SY_53   ','SXY_53  ','SYZ_53  ',
     .'SZX_53  ','SX_54   ','SY_54   ','SXY_54  ','SYZ_54  ','SZX_54  ',
     .'SX_55   ','SY_55   ','SXY_55  ','SYZ_55  ','SZX_55  ','SX_56   ',
     .'SY_56   ','SXY_56  ','SYZ_56  ','SZX_56  ','SX_57   ','SY_57   ',
     .'SXY_57  ','SYZ_57  ','SZX_57  ','SX_58   ','SY_58   ','SXY_58  ',
     .'SYZ_58  ','SZX_58  ','SX_59   ','SY_59   ','SXY_59  ','SYZ_59  ',
     .'SZX_59  ','SX_60   ','SY_60   ','SXY_60  ','SYZ_60  ','SZX_60  ',
     .'SX_61   ','SY_61   ','SXY_61  ','SYZ_61  ','SZX_61  ','SX_62   ',
     .'SY_62   ','SXY_62  ','SYZ_62  ','SZX_62  ','SX_63   ','SY_63   ',
     .'SXY_63  ','SYZ_63  ','SZX_63  ','SX_64   ','SY_64   ','SXY_64  ',
     .'SYZ_64  ','SZX_64  ','SX_65   ','SY_65   ','SXY_65  ','SYZ_65  ',
     .'SZX_65  ','SX_66   ','SY_66   ','SXY_66  ','SYZ_66  ','SZX_66  ',
     .'SX_67   ','SY_67   ','SXY_67  ','SYZ_67  ','SZX_67  ','SX_68   ',
     .'SY_68   ','SXY_68  ','SYZ_68  ','SZX_68  ','SX_69   ','SY_69   ',
     .'SXY_69  ','SYZ_69  ','SZX_69  ','SX_70   ','SY_70   ','SXY_70  ',
     .'SYZ_70  ','SZX_70  ','SX_71   ','SY_71   ','SXY_71  ','SYZ_71  ',
     .'SZX_71  ','SX_72   ','SY_72   ','SXY_72  ','SYZ_72  ','SZX_72  ',
     .'SX_73   ','SY_73   ','SXY_73  ','SYZ_73  ','SZX_73  ','SX_74   ',
     .'SY_74   ','SXY_74  ','SYZ_74  ','SZX_74  ','SX_75   ','SY_75   ',
     .'SXY_75  ','SYZ_75  ','SZX_75  ','SX_76   ','SY_76   ','SXY_76  ',
     .'SYZ_76  ','SZX_76  ','SX_77   ','SY_77   ','SXY_77  ','SYZ_77  ',
     .'SZX_77  ','SX_78   ','SY_78   ','SXY_78  ','SYZ_78  ','SZX_78  ',
     .'SX_79   ','SY_79   ','SXY_79  ','SYZ_79  ','SZX_79  ','SX_80   ',
     .'SY_80   ','SXY_80  ','SYZ_80  ','SZX_80  ','SX_81   ','SY_81   ',
     .'SXY_81  ','SYZ_81  ','SZX_81  ','SX_82   ','SY_82   ','SXY_82  ',
     .'SYZ_82  ','SZX_82  ','SX_83   ','SY_83   ','SXY_83  ','SYZ_83  ',
     .'SZX_83  ','SX_84   ','SY_84   ','SXY_84  ','SYZ_84  ','SZX_84  ',
     .'SX_85   ','SY_85   ','SXY_85  ','SYZ_85  ','SZX_85  ','SX_86   ',
     .'SY_86   ','SXY_86  ','SYZ_86  ','SZX_86  ','SX_87   ','SY_87   ',
     .'SXY_87  ','SYZ_87  ','SZX_87  ','SX_88   ','SY_88   ','SXY_88  ',
     .'SYZ_88  ','SZX_88  ','SX_89   ','SY_89   ','SXY_89  ','SYZ_89  ',
     .'SZX_89  ','SX_90   ','SY_90   ','SXY_90  ','SYZ_90  ','SZX_90  ',
     .'SX_91   ','SY_91   ','SXY_91  ','SYZ_91  ','SZX_91  ','SX_92   ',
     .'SY_92   ','SXY_92  ','SYZ_92  ','SZX_92  ','SX_93   ','SY_93   ',
     .'SXY_93  ','SYZ_93  ','SZX_93  ','SX_94   ','SY_94   ','SXY_94  ',
     .'SYZ_94  ','SZX_94  ','SX_95   ','SY_95   ','SXY_95  ','SYZ_95  ',
     .'SZX_95  ','SX_96   ','SY_96   ','SXY_96  ','SYZ_96  ','SZX_96  ',
     .'SX_97   ','SY_97   ','SXY_97  ','SYZ_97  ','SZX_97  ','SX_98   ',
     .'SY_98   ','SXY_98  ','SYZ_98  ','SZX_98  ','SX_99   ','SY_99   ',
     .'SXY_99  ','SYZ_99  ','SZX_99  ','NFAIL   ','PFAIL   ','FAIL_D1 ',
     .'FAIL_D2 ','FAIL_EN ','WPLAY01 ','WPLAY02 ','WPLAY03 ','WPLAY04 ',
     .'WPLAY05 ','WPLAY06 ','WPLAY07 ','WPLAY08 ','WPLAY09 ','WPLAY10 ',
     .'WPLAY11 ','WPLAY12 ','WPLAY13 ','WPLAY14 ','WPLAY15 ','WPLAY16 ',
     .'WPLAY17 ','WPLAY18 ','WPLAY19 ','WPLAY20 ','WPLAY21 ','WPLAY22 ',
     .'WPLAY23 ','WPLAY24 ','WPLAY25 ','WPLAY26 ','WPLAY27 ','WPLAY28 ',
     .'WPLAY29 ','WPLAY30 ','WPLAY31 ','WPLAY32 ','WPLAY33 ','WPLAY34 ',
     .'WPLAY35 ','WPLAY36 ','WPLAY37 ','WPLAY38 ','WPLAY39 ','WPLAY40 ',
     .'WPLAY41 ','WPLAY42 ','WPLAY43 ','WPLAY44 ','WPLAY45 ','WPLAY46 ',
     .'WPLAY47 ','WPLAY48 ','WPLAY49 ','WPLAY50 ','WPLAY51 ','WPLAY52 ',
     .'WPLAY53 ','WPLAY54 ','WPLAY55 ','WPLAY56 ','WPLAY57 ','WPLAY58 ',
     .'WPLAY59 ','WPLAY60 ','WPLAY61 ','WPLAY62 ','WPLAY63 ','WPLAY64 ',
     .'WPLAY65 ','WPLAY66 ','WPLAY67 ','WPLAY68 ','WPLAY69 ','WPLAY70 ',
     .'WPLAY71 ','WPLAY72 ','WPLAY73 ','WPLAY74 ','WPLAY75 ','WPLAY76 ',
     .'WPLAY77 ','WPLAY78 ','WPLAY79 ','WPLAY80 ','WPLAY81 ','WPLAY82 ',
     .'WPLAY83 ','WPLAY84 ','WPLAY85 ','WPLAY86 ','WPLAY87 ','WPLAY88 ',
     .'WPLAY89 ','WPLAY90 ','WPLAY91 ','WPLAY92 ','WPLAY93 ','WPLAY94 ',
     .'WPLAY95 ','WPLAY96 ','WPLAY97 ','WPLAY98 ','WPLAY99 '/
      DATA VARC3/
     .'SVX_1   ','SVY_1   ','SVXY_1  ','SVYZ_1  ','SVZX_1  ','SVX_2   ',
     .'SVY_2   ','SVXY_2  ','SVYZ_2  ','SVZX_2  ','SVX_3   ','SVY_3   ',
     .'SVXY_3  ','SVYZ_3  ','SVZX_3  ','SVX_4   ','SVY_4   ','SVXY_4  ',
     .'SVYZ_4  ','SVZX_4  ','SVX_5   ','SVY_5   ','SVXY_5  ','SVYZ_5  ',
     .'SVZX_5  ','SVX_6   ','SVY_6   ','SVXY_6  ','SVYZ_6  ','SVZX_6  ',
     .'SVX_7   ','SVY_7   ','SVXY_7  ','SVYZ_7  ','SVZX_7  ','SVX_8   ',
     .'SVY_8   ','SVXY_8  ','SVYZ_8  ','SVZX_8  ','SVX_9   ','SVY_9   ',
     .'SVXY_9  ','SVYZ_9  ','SVZX_9  ','SVX_10  ','SVY_10  ','SVXY_10 ',
     .'SVYZ_10 ','SVZX_10 ','SVX_11  ','SVY_11  ','SVXY_11 ','SVYZ_11 ',
     .'SVZX_11 ','SVX_12  ','SVY_12  ','SVXY_12 ','SVYZ_12 ','SVZX_12 ',
     .'SVX_13  ','SVY_13  ','SVXY_13 ','SVYZ_13 ','SVZX_13 ','SVX_14  ',
     .'SVY_14  ','SVXY_14 ','SVYZ_14 ','SVZX_14 ','SVX_15  ','SVY_15  ',
     .'SVXY_15 ','SVYZ_15 ','SVZX_15 ','SVX_16  ','SVY_16  ','SVXY_16 ',
     .'SVYZ_16 ','SVZX_16 ','SVX_17  ','SVY_17  ','SVXY_17 ','SVYZ_17 ',
     .'SVZX_17 ','SVX_18  ','SVY_18  ','SVXY_18 ','SVYZ_18 ','SVZX_18 ',
     .'SVX_19  ','SVY_19  ','SVXY_19 ','SVYZ_19 ','SVZX_19 ','SVX_20  ',
     .'SVY_20  ','SVXY_20 ','SVYZ_20 ','SVZX_20 ','SVX_21  ','SVY_21  ',
     .'SVXY_21 ','SVYZ_21 ','SVZX_21 ','SVX_22  ','SVY_22  ','SVXY_22 ',
     .'SVYZ_22 ','SVZX_22 ','SVX_23  ','SVY_23  ','SVXY_23 ','SVYZ_23 ',
     .'SVZX_23 ','SVX_24  ','SVY_24  ','SVXY_24 ','SVYZ_24 ','SVZX_24 ',
     .'SVX_25  ','SVY_25  ','SVXY_25 ','SVYZ_25 ','SVZX_25 ','SVX_26  ',
     .'SVY_26  ','SVXY_26 ','SVYZ_26 ','SVZX_26 ','SVX_27  ','SVY_27  ',
     .'SVXY_27 ','SVYZ_27 ','SVZX_27 ','SVX_28  ','SVY_28  ','SVXY_28 ',
     .'SVYZ_28 ','SVZX_28 ','SVX_29  ','SVY_29  ','SVXY_29 ','SVYZ_29 ',
     .'SVZX_29 ','SVX_30  ','SVY_30  ','SVXY_30 ','SVYZ_30 ','SVZX_30 ',
     .'SVX_31  ','SVY_31  ','SVXY_31 ','SVYZ_31 ','SVZX_31 ','SVX_32  ',
     .'SVY_32  ','SVXY_32 ','SVYZ_32 ','SVZX_32 ','SVX_33  ','SVY_33  ',
     .'SVXY_33 ','SVYZ_33 ','SVZX_33 ','SVX_34  ','SVY_34  ','SVXY_34 ',
     .'SVYZ_34 ','SVZX_34 ','SVX_35  ','SVY_35  ','SVXY_35 ','SVYZ_35 ',
     .'SVZX_35 ','SVX_36  ','SVY_36  ','SVXY_36 ','SVYZ_36 ','SVZX_36 ',
     .'SVX_37  ','SVY_37  ','SVXY_37 ','SVYZ_37 ','SVZX_37 ','SVX_38  ',
     .'SVY_38  ','SVXY_38 ','SVYZ_38 ','SVZX_38 ','SVX_39  ','SVY_39  ',
     .'SVXY_39 ','SVYZ_39 ','SVZX_39 ','SVX_40  ','SVY_40  ','SVXY_40 ',
     .'SVYZ_40 ','SVZX_40 ','SVX_41  ','SVY_41  ','SVXY_41 ','SVYZ_41 ',
     .'SVZX_41 ','SVX_42  ','SVY_42  ','SVXY_42 ','SVYZ_42 ','SVZX_42 ',
     .'SVX_43  ','SVY_43  ','SVXY_43 ','SVYZ_43 ','SVZX_43 ','SVX_44  ',
     .'SVY_44  ','SVXY_44 ','SVYZ_44 ','SVZX_44 ','SVX_45  ','SVY_45  ',
     .'SVXY_45 ','SVYZ_45 ','SVZX_45 ','SVX_46  ','SVY_46  ','SVXY_46 ',
     .'SVYZ_46 ','SVZX_46 ','SVX_47  ','SVY_47  ','SVXY_47 ','SVYZ_47 ',
     .'SVZX_47 ','SVX_48  ','SVY_48  ','SVXY_48 ','SVYZ_48 ','SVZX_48 ',
     .'SVX_49  ','SVY_49  ','SVXY_49 ','SVYZ_49 ','SVZX_49 ','SVX_50  ',
     .'SVY_50  ','SVXY_50 ','SVYZ_50 ','SVZX_50 ','SVX_51  ','SVY_51  ',
     .'SVXY_51 ','SVYZ_51 ','SVZX_51 ','SVX_52  ','SVY_52  ','SVXY_52 ',
     .'SVYZ_52 ','SVZX_52 ','SVX_53  ','SVY_53  ','SVXY_53 ','SVYZ_53 ',
     .'SVZX_53 ','SVX_54  ','SVY_54  ','SVXY_54 ','SVYZ_54 ','SVZX_54 ',
     .'SVX_55  ','SVY_55  ','SVXY_55 ','SVYZ_55 ','SVZX_55 ','SVX_56  ',
     .'SVY_56  ','SVXY_56 ','SVYZ_56 ','SVZX_56 ','SVX_57  ','SVY_57  ',
     .'SVXY_57 ','SVYZ_57 ','SVZX_57 ','SVX_58  ','SVY_58  ','SVXY_58 ',
     .'SVYZ_58 ','SVZX_58 ','SVX_59  ','SVY_59  ','SVXY_59 ','SVYZ_59 ',
     .'SVZX_59 ','SVX_60  ','SVY_60  ','SVXY_60 ','SVYZ_60 ','SVZX_60 ',
     .'SVX_61  ','SVY_61  ','SVXY_61 ','SVYZ_61 ','SVZX_61 ','SVX_62  ',
     .'SVY_62  ','SVXY_62 ','SVYZ_62 ','SVZX_62 ','SVX_63  ','SVY_63  ',
     .'SVXY_63 ','SVYZ_63 ','SVZX_63 ','SVX_64  ','SVY_64  ','SVXY_64 ',
     .'SVYZ_64 ','SVZX_64 ','SVX_65  ','SVY_65  ','SVXY_65 ','SVYZ_65 ',
     .'SVZX_65 ','SVX_66  ','SVY_66  ','SVXY_66 ','SVYZ_66 ','SVZX_66 ',
     .'SVX_67  ','SVY_67  ','SVXY_67 ','SVYZ_67 ','SVZX_67 ','SVX_68  ',
     .'SVY_68  ','SVXY_68 ','SVYZ_68 ','SVZX_68 ','SVX_69  ','SVY_69  ',
     .'SVXY_69 ','SVYZ_69 ','SVZX_69 ','SVX_70  ','SVY_70  ','SVXY_70 ',
     .'SVYZ_70 ','SVZX_70 ','SVX_71  ','SVY_71  ','SVXY_71 ','SVYZ_71 ',
     .'SVZX_71 ','SVX_72  ','SVY_72  ','SVXY_72 ','SVYZ_72 ','SVZX_72 ',
     .'SVX_73  ','SVY_73  ','SVXY_73 ','SVYZ_73 ','SVZX_73 ','SVX_74  ',
     .'SVY_74  ','SVXY_74 ','SVYZ_74 ','SVZX_74 ','SVX_75  ','SVY_75  ',
     .'SVXY_75 ','SVYZ_75 ','SVZX_75 ','SVX_76  ','SVY_76  ','SVXY_76 ',
     .'SVYZ_76 ','SVZX_76 ','SVX_77  ','SVY_77  ','SVXY_77 ','SVYZ_77 ',
     .'SVZX_77 ','SVX_78  ','SVY_78  ','SVXY_78 ','SVYZ_78 ','SVZX_78 ',
     .'SVX_79  ','SVY_79  ','SVXY_79 ','SVYZ_79 ','SVZX_79 ','SVX_80  ',
     .'SVY_80  ','SVXY_80 ','SVYZ_80 ','SVZX_80 ','SVX_81  ','SVY_81  ',
     .'SVXY_81 ','SVYZ_81 ','SVZX_81 ','SVX_82  ','SVY_82  ','SVXY_82 ',
     .'SVYZ_82 ','SVZX_82 ','SVX_83  ','SVY_83  ','SVXY_83 ','SVYZ_83 ',
     .'SVZX_83 ','SVX_84  ','SVY_84  ','SVXY_84 ','SVYZ_84 ','SVZX_84 ',
     .'SVX_85  ','SVY_85  ','SVXY_85 ','SVYZ_85 ','SVZX_85 ','SVX_86  ',
     .'SVY_86  ','SVXY_86 ','SVYZ_86 ','SVZX_86 ','SVX_87  ','SVY_87  ',
     .'SVXY_87 ','SVYZ_87 ','SVZX_87 ','SVX_88  ','SVY_88  ','SVXY_88 ',
     .'SVYZ_88 ','SVZX_88 ','SVX_89  ','SVY_89  ','SVXY_89 ','SVYZ_89 ',
     .'SVZX_89 ','SVX_90  ','SVY_90  ','SVXY_90 ','SVYZ_90 ','SVZX_90 ',
     .'SVX_91  ','SVY_91  ','SVXY_91 ','SVYZ_91 ','SVZX_91 ','SVX_92  ',
     .'SVY_92  ','SVXY_92 ','SVYZ_92 ','SVZX_92 ','SVX_93  ','SVY_93  ',
     .'SVXY_93 ','SVYZ_93 ','SVZX_93 ','SVX_94  ','SVY_94  ','SVXY_94 ',
     .'SVYZ_94 ','SVZX_94 ','SVX_95  ','SVY_95  ','SVXY_95 ','SVYZ_95 ',
     .'SVZX_95 ','SVX_96  ','SVY_96  ','SVXY_96 ','SVYZ_96 ','SVZX_96 ',
     .'SVX_97  ','SVY_97  ','SVXY_97 ','SVYZ_97 ','SVZX_97 ','SVX_98  ',
     .'SVY_98  ','SVXY_98 ','SVYZ_98 ','SVZX_98 ','SVX_99  ','SVY_99  ',
     .'SVXY_99 ','SVYZ_99 ','SVZX_99 ','SVX_100 ','SVY_100 ','SVXY_100',
     .'SVYZ_100','SVZX_100'/
      DATA VARCPINCH/
     .'EPINCHXZ','EPINCHYZ','EPINCHZZ','FPINCH  ','MPINCHXZ','MPINCHYZ',
     .'THK     '/
      DATA VARCNLOC/
     .'NL_PLAS ','NL_EPSD '/
C   trusses
      DATA VART/
     . 'OFF     ','F       ','IE      ','A       ','L       ',
     . 'PLAS    '/
C   beams
      DATA VARP/
     . 'OFF     ','F1      ','F2      ','F3      ','M1      ',
     . 'M2      ','M3      ','IE      ','SX      ','SXY     ',
     . 'SZX     '/
C   springs
      DATA VARR/
     . 'OFF     ','FX      ','FY      ','FZ      ','MX      ',
     . 'MY      ','MZ      ','LX      ','LY      ','LZ      ',
     . 'RX      ','RY      ','RZ      ','IE      ','F1      ',
     . 'F2      ','F1G_X   ','F1G_Y   ','F1G_Z   ','F2G_X   ',
     . 'F2G_Y   ','F2G_Z   ','F1L_X   ','F1L_Y   ','F1L_Z   ',
     . 'F2L_X   ','F2L_Y   ','F2L_Z   ','M1G_X   ','M1G_Y   ',
     . 'M1G_Z   ','M2G_X   ','M2G_Y   ','M2G_Z   ','M1L_X   ',
     . 'M1L_Y   ','M1L_Z   ','M2L_X   ','M2L_Y   ','M2L_Z   ',
     . 'D1G_X   ','D1G_Y   ','D1G_Z   ','D2G_X   ','D2G_Y   ',
     . 'D2G_Z   ','D1L_X   ','D1L_Y   ','D1L_Z   ','D2L_X   ',
     . 'D2L_Y   ','D2L_Z   ','R1G_X   ','R1G_Y   ','R1G_Z   ',
     . 'R2G_X   ','R2G_Y   ','R2G_Z   ','R1L_X   ','R1L_Y   ',
     . 'G1L_Z   ','G2L_X   ','G2L_Y   ','G2L_Z   ','LENGTH  '/

      DATA VARNS/
     . 'OFF     ','FX      ','LX      ','IE      '/

      DATA VARSPH/
     . 'OFF     ','SX      ','SY      ','SZ      ','SXY     ',
     . 'SYZ     ','SXZ     ','IE      ','DENS    ','WFVIS   ',
     . 'VOL     ','PLAS    ','TEMP    ','PLSR    ','DAM1    ',
     . 'DAM2    ','DAM3    ','DAM4    ','DAMA    ','SA1     ',
     . 'SA2     ','SA3     ','CR      ','CAP     ','K0      ',
     . 'RK      ','TD      ','EFIB    ','ISTA    ','VPLA    ',
     . 'BFRAC   ','WPLA    ','SFIB    ','DAM5    ','LSX     ',
     . 'LSY     ','LSZ     ','LSXY    ','LSYZ    ','LSXZ    ',
     . 'DIAMETER'/
      DATA VARUR/
     . 'OFF     ','EINT    ','EINR    ','XN1     ','XN2     ',
     . 'XN3     ','DX1     ','DX2     ','DX3     ','XM1     ',
     . 'XM2     ','XM3     '/
C   interfaces
      DATA VARIN/
     . 'FNX     ','FNY     ','FNZ     ','FTX     ','FTY     ',
     . 'FTZ     ','SFW     ','|FNX|   ','|FNY|   ','|FNZ|   ',
     . '||FN||  ','|FX|    ','|FY|    ','|FZ|    ','||F||   ',
     . 'PVOL    ','PSURF   ','PMED    ','DELTAP  ','VOL     ',
     . 'SURF    ','MX      ','MY      ','MZ      ','QFRIC   ',
     . 'CE_ELAST','CE_FRIC ','CE_DAMP '/
C   rigid wall
      DATA VARRW/
     . 'FNX     ','FNY     ','FNZ     ','FTX     ','FTY     ',
     . 'FTZ     '/
C   rigid body
      DATA VARRB/
     . 'FX      ','FY      ','FZ      ','MX      ','MY      ',
     . 'MZ      ','RX      ','RY      ','RZ      ',
     . 'FXI     ','FYI     ','FZI     ',
     . 'MXI     ','MYI     ','MZI     '/
C   flexible body
      DATA VARFX/
     . 'IE      ','KE      ','EFW     ','DE      '/
C   flexible body mode
      DATA VARFXM/
     . 'D       ','V       ','A       '/
C   accel
      DATA VARAC/
     . 'AX      ','AY      ','AZ      ','WX      ','WY      ',
     . 'WZ      '/
C   section
      DATA VARSE/
     . 'FNX     ','FNY     ','FNZ     ','FTX     ','FTY     ',
     . 'FTZ     ','M1      ','M2      ','M3      ','WORK    ',
     . 'DFX     ','DFY     ','DFZ     ','DF2     ','WORKR   ',
     . 'DMX     ','DMY     ','DMZ     ','DM2     ','KIN     ',
     . 'KINR    ','DMVX    ','DMVY    ','DMVZ    ','DKIN    ',
     . 'DMVRX   ','DMVRY   ','DMVRZ   ','DKINR   ','TFEXT   ',
     . 'MX      ','MY      ','MZ      ','F1      ','F2      ',
     . 'F3      ','CX      ','CY      ','CZ      '/
C   joint cyl
      DATA VARJO/
     . 'FX      ','FY      ','FZ      ','MX      ','MY      ',
     . 'MZ      '/
C   airbag
      DATA VARAB/
     . 'MASS    ','VOL     ','P       ','RO      ','IE      ',
     . 'OE      ','OU      '/
C   monitored volume
      DATA VARMV4/
     . 'MASS    ','VOL     ','P       ','A       ','T       ',
     . 'AO      ','UO      ','AC      ','UC      '/
      DATA VARMV/
     . 'MASS    ','VOL     ','P       ','A       ','T       ',
     . 'AO      ','UO      ','AC      ','UC      ','CP      ',
     . 'CV      ','GAMA    ','DTBAG   ','NFV     ','MASS-IN ',
     . 'ENTHA-IN','ENER-INT','WORK    ','UPCRIT  '/
      DATA VARMVENT/
     . 'MASS    ','VOL     ','P       ','A       ','T       ',
     . 'AO      ','UO      ','AC      ','UC      ','CP      ',
     . 'CV      ','GAMA    ','DTBAG   ','NFV     ','MASS-IN ',
     . 'ENTHA-IN','ENER-INT','WORK    ','UPCRIT  ','UNUSED  ',
     . 'UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ',
     . 'UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ',
     . 'UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ',
     . 'UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ',
     . 'UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ',
     . 'UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ',
     . 'UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ',
     . 'UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ',
     . 'UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ',
     . 'UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ',
     . 'UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ',
     . 'UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ',
     . 'UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ',
     . 'UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ',
     . 'UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ',
     . 'UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ',
     . 'AOUT1   ','BOUT1   ','UOUT1   ','MOUT1   ','HOUT1   ',
     . 'AOUT2   ','BOUT2   ','UOUT2   ','MOUT2   ','HOUT2   ',
     . 'AOUT3   ','BOUT3   ','UOUT3   ','MOUT3   ','HOUT3   ',
     . 'AOUT4   ','BOUT4   ','UOUT4   ','MOUT4   ','HOUT4   ',
     . 'AOUT5   ','BOUT5   ','UOUT5   ','MOUT5   ','HOUT5   ',
     . 'AOUT6   ','BOUT6   ','UOUT6   ','MOUT6   ','HOUT6   ',
     . 'AOUT7   ','BOUT7   ','UOUT7   ','MOUT7   ','HOUT7   ',
     . 'AOUT8   ','BOUT8   ','UOUT8   ','MOUT8   ','HOUT8   ',
     . 'AOUT9   ','BOUT9   ','UOUT9   ','MOUT9   ','HOUT9   ',
     . 'AOUT10  ','BOUT10  ','UOUT10  ','MOUT10  ','HOUT10  '/
C   part
      DATA VARPA/
     . 'IE      ','KE      ','XMOM    ','YMOM    ','ZMOM    ',
     . 'MASS    ','HE      ','TURBKE  ','XCG     ','YCG     ',
     . 'ZCG     ','XXMOM   ','YYMOM   ','ZZMOM   ','IXX     ',
     . 'IYY     ','IZZ     ','IXY     ','IYZ     ','IZX     ',
     . 'RIE     ','KERB    ','RKERB   ','RKE     ','UNUSED  ',
     . 'UNUSED  ','UNUSED  ','HEAT    '/
C   rivets
      DATA VARRIV/
     . 'OFF     ','F1X     ','F1Y     ','F1Z     ','F2X     ',
     . 'F2Y     ','F2Z     ','FX      ','FY      ','FZ      '/
C   frames
      DATA VARF1/
     . 'OX      ','OY      ','OZ      ','R11     ','R12     ',
     . 'R13     ','R21     ','R22     ','R23     ','R31     ',
     . 'R32     ','R33     ','VX      ','VY      ','VZ      ',
     . 'VRX     ','VRY     ','VRZ     '/
      DATA VARFR/
     . 'OX      ','OY      ','OZ      ','R11     ','R12     ',
     . 'R13     ','R21     ','R22     ','R23     ','R31     ',
     . 'R32     ','R33     ','VX      ','VY      ','VZ      ',
     . 'VRX     ','VRY     ','VRZ     ','AX      ','AY      ',
     . 'AZ      ','ARX     ','ARY     ','ARZ     '/
      DATA VARGAU/
     . 'P       ','IE      ','DENS    ','T       ','PA      ',
     . 'PAX     ','PAY     ','PAZ     '/
C     element cluster
      DATA VARCLUS/
     . 'FX      ','FY      ','FZ      ','MX      ','MY      ',
     . 'MZ      ','FS      ','FN      ','MS      ','MN      ',
     . 'FAIL    '/
      DATA VARFLOW/
     . 'FLOW    '/
      DATA VARSURF/
     . 'AREA    ','MASSFLOW','VELOCITY'/
C
      DATA VARSLIP/
     . 'RINGSLIP','FN      ','F1      ','F2      ','THETA   ','GAMMA   '/
C
      DATA VARRET/
     . 'SLIP','FN      ','LOCK    '/
C-----------------------------------------------
C   TH GENERIC VARIABLES
C-----------------------------------------------
      DATA VARNG/'DEF     ','D       ','V       ',
     .           'A       ','VR      ','AR      ','XYZ     ',
     .           'PINCH   '/
      DATA IVARNG
     .        / 1, 2, 3, 4, 5, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           4, 5, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .          7, 8, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .         10,11,12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .         13,14,15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .         16,17,18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .         629,630,631,632,633,634,635,636,637, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA VARSG1/
     .'DEF     ','STRESS  ','LOCSTRS ','SXGP    ','SYGP    ','SZGP    ',
     .'SXYGP   ','SYZGP   ','SXZGP   ','LSXGP   ','LSYGP   ','LSZGP   ',
     .'LSXYGP  ','LSYZGP  ','LSXZGP  ','USERA   ','USERB   ','USERC   ',
     .'USERD   ',
     .'SIG111  ','SIG211  ','SIG311  ','SIG121  ','SIG221  ','SIG321  ',
     .'SIG131  ','SIG231  ','SIG331  ','SIG141  ','SIG241  ','SIG341  ',
     .'SIG151  ','SIG251  ','SIG351  ','SIG161  ','SIG261  ','SIG361  ',
     .'SIG171  ','SIG271  ','SIG371  ','SIG181  ','SIG281  ','SIG381  ',
     .'SIG191  ','SIG291  ','SIG391  ',
     .'SIG112  ','SIG212  ','SIG312  ','SIG122  ','SIG222  ','SIG322  ',
     .'SIG132  ','SIG232  ','SIG332  ','SIG142  ','SIG242  ','SIG342  ',
     .'SIG152  ','SIG252  ','SIG352  ','SIG162  ','SIG262  ','SIG362  ',
     .'SIG172  ','SIG272  ','SIG372  ','SIG182  ','SIG282  ','SIG382  ',
     .'SIG192  ','SIG292  ','SIG392  ',
     .'SIG113  ','SIG213  ','SIG313  ','SIG123  ','SIG223  ','SIG323  ',
     .'SIG133  ','SIG233  ','SIG333  ','SIG143  ','SIG243  ','SIG343  ',
     .'SIG153  ','SIG253  ','SIG353  ','SIG163  ','SIG263  ','SIG363  ',
     .'SIG173  ','SIG273  ','SIG373  ','SIG183  ','SIG283  ','SIG383  ',
     .'SIG193  ','SIG293  ','SIG393  ',
     .'SIG1U1  ','SIG2U1  ','SIG3U1  ','SIG1U2  ','SIG2U2  ','SIG3U2  ',
     .'SIG1U3  ','SIG2U3  ','SIG3U3  ',
     .'SIG1D1  ','SIG2D1  ','SIG3D1  ','SIG1D2  ','SIG2D2  ','SIG3D2  ',
     .'SIG1D3  ','SIG2D3  ','SIG3D3  ','USER111 ','USER211 ','USER311 ',
     .'USER121 ','USER221 ','USER321 ','USER131 ','USER231 ','USER331 ',
     .'USER141 ','USER241 ','USER341 ','USER151 ','USER251 ','USER351 ',
     .'USER161 ','USER261 ','USER361 ','USER171 ','USER271 ','USER371 ',
     .'USER181 ','USER281 ','USER381 ','USER191 ','USER291 ','USER391 ',
     .'USER112 ','USER212 ','USER312 ','USER122 ','USER222 ','USER322 ',
     .'USER132 ','USER232 ','USER332 ','USER142 ','USER242 ','USER342 ',
     .'USER152 ','USER252 ','USER352 ','USER162 ','USER262 ','USER362 ',
     .'USER172 ','USER272 ','USER372 ','USER182 ','USER282 ','USER382 ',
     .'USER192 ','USER292 ','USER392 ','USER113 ','USER213 ','USER313 ',
     .'USER123 ','USER223 ','USER323 ','USER133 ','USER233 ','USER333 ',
     .'USER143 ','USER243 ','USER343 ','USER153 ','USER253 ','USER353 ',
     .'USER163 ','USER263 ','USER363 ','USER173 ','USER273 ','USER373 ',
     .'USER183 ','USER283 ','USER383 ','USER193 ','USER293 ','USER393 ',
     .'STRAIN  ','DEF_MAT1','DEF_MAT2'/
C
c      DATA VARSG2
C Definition of VARSG2 : strain for /PROP/TYPE22
      DO J=1,200
        IF (J <= 9) THEN
          WRITE(CHJ,'(I1.1)')J
        ELSEIF (J <= 99) THEN
          WRITE(CHJ,'(I2.2)')J
        ELSE
          WRITE(CHJ,'(I3.3)')J
        ENDIF
        DO I=1,9
          WRITE(CHI,'(I1.1)')I
          DO K=1,9
            WRITE(CHK,'(I1.1)')K
            VAR_TMP(1:5)='EPS_'//CHI
            VAR_TMP=VAR_TMP(1:5)//CHK//'_'//CHJ
            VARSG2((J-1)*99+(I-1)*9+K)=VAR_TMP
          ENDDO
        ENDDO
      ENDDO
c      DATA VARSG3
C Definition of VARSG3 : stress for /PROP/TYPE22
      DO J=1,200
        IF (J <= 9) THEN
          WRITE(CHJ,'(I1.1)')J
        ELSEIF (J <= 99) THEN
          WRITE(CHJ,'(I2.2)')J
        ELSE
          WRITE(CHJ,'(I3.3)')J
        ENDIF
        DO I=1,9
          WRITE(CHI,'(I1.1)')I
          DO K=1,9
            WRITE(CHK,'(I1.1)')K
            VAR_TMP(1:5)='SIG_'//CHI
            VAR_TMP=VAR_TMP(1:5)//CHK//'_'//CHJ
            VARSG3((J-1)*9*9+(I-1)*9+K)=VAR_TMP
          ENDDO
        ENDDO
      ENDDO
c      DATA VARSG4
C Definition of VARSG4 
C
      DATA VARSG4/
     .'LOCSTRN '/

c VARCG = VARCG1 + VARCG2+ VARCG3
      DATA VARCG1/
     .           'DEF     ','STRESS  ','STRAIN  ','PLAS    ','USERA   ',
     .           'USERB   ','USERC   ','USERD   ','USER1   ','USER2   ',
     .           'USER3   ','USER4   ','USER5   ','USER6   ','USER7   ',
     .           'USER8   ','USER9   ','USER10  ','USER11  ','USER12  ',
     .           'USER13  ','USER14  ','USER15  ','USER16  ','USER17  ',
     .           'USER18  ','USER19  ','USER20  ',
     .           'SIG_1   ','SIG_2   ','SIG_3   ','SIG_4   ','SIG_5   ',
     .           'SIG_6   ','SIG_7   ','SIG_8   ','SIG_9   ','SIG_10  ',
     .           'SIG_11  ','SIG_12  ','SIG_13  ','SIG_14  ','SIG_15  ',
     .           'SIG_16  ','SIG_17  ','SIG_18  ','SIG_19  ','SIG_10  ',
     .           'SIG_21  ','SIG_22  ','SIG_23  ','SIG_24  ','SIG_25  ',
     .           'SIG_26  ','SIG_27  ','SIG_28  ','SIG_29  ','SIG_30  ',
     .           'SIG_31  ','SIG_32  ','SIG_33  ','SIG_34  ','SIG_35  ',
     .           'SIG_36  ','SIG_37  ','SIG_38  ','SIG_39  ','SIG_40  ',
     .           'SIG_41  ','SIG_42  ','SIG_43  ','SIG_44  ','SIG_45  ',
     .           'SIG_46  ','SIG_47  ','SIG_48  ','SIG_49  ','SIG_50  ',
     .           'SIG_51  ','SIG_52  ','SIG_53  ','SIG_54  ','SIG_55  ',
     .           'SIG_56  ','SIG_57  ','SIG_58  ','SIG_59  ','SIG_60  ',
     .           'SIG_61  ','SIG_62  ','SIG_63  ','SIG_64  ','SIG_65  ',
     .           'SIG_66  ','SIG_67  ','SIG_68  ','SIG_69  ','SIG_70  ',
     .           'SIG_71  ','SIG_72  ','SIG_73  ','SIG_74  ','SIG_75  ',
     .           'SIG_76  ','SIG_77  ','SIG_78  ','SIG_79  ','SIG_80  ',
     .           'SIG_81  ','SIG_82  ','SIG_83  ','SIG_84  ','SIG_85  ',
     .           'SIG_86  ','SIG_87  ','SIG_88  ','SIG_89  ','SIG_90  ',
     .           'SIG_91  ','SIG_92  ','SIG_93  ','SIG_94  ','SIG_95  ',
     .           'SIG_96  ','SIG_97  ','SIG_98  ','SIG_99  ',
     .           'USER21  ','USER22  ',
     .           'USER23  ','USER24  ','USER25  ','USER26  ','USER27  ',
     .           'USER28  ','USER29  ','USER30  ','USER31  ','USER32  ',
     .           'USER33  ','USER34  ','USER35  ','USER36  ','USER37  ',
     .           'USER38  ','USER39  ','USER40  ','USER41  ','USER42  ',
     .           'USER43  ','USER44  ','USER45  ','USER46  ','USER47  ',
     .           'USER48  ','USER49  ','USER50  ','USER51  ','USER52  ',
     .           'USER53  ','USER54  ','USER55  ','USER56  ','USER57  ',
     .           'USER58  ','USER59  ','USER60  '/
      DATA VARCG2/
     .           'USLAY011','USLAY012','USLAY013','USLAY014','USLAY021',
     .           'USLAY022','USLAY023','USLAY024','USLAY031','USLAY032',
     .           'USLAY033','USLAY034','USLAY041','USLAY042','USLAY043',
     .           'USLAY044','USLAY051','USLAY012','USLAY013','USLAY014',
     .           'USLAY061','USLAY062','USLAY063','USLAY064','USLAY071',
     .           'USLAY072','USLAY073','USLAY074','USLAY081','USLAY082',
     .           'USLAY083','USLAY084','USLAY091','USLAY092','USLAY093',
     .           'USLAY094','USLAY101','USLAY102','USLAY103','USLAY104',
     .           'USLAY111','USLAY112','USLAY113','USLAY114','USLAY121',
     .           'USLAY122','USLAY123','USLAY124','USLAY131','USLAY132',
     .           'USLAY133','USLAY134','USLAY141','USLAY142','USLAY143',
     .           'USLAY144','USLAY151','USLAY152','USLAY153','USLAY154',
     .           'USLAY161','USLAY162','USLAY163','USLAY164','USLAY171',
     .           'USLAY172','USLAY173','USLAY174','USLAY181','USLAY182',
     .           'USLAY183','USLAY184','USLAY191','USLAY192','USLAY193',
     .           'USLAY194','USLAY201','USLAY202','USLAY203','USLAY204',
     .           'USLAY211','USLAY212','USLAY213','USLAY214','USLAY221',
     .           'USLAY222','USLAY223','USLAY224','USLAY231','USLAY232',
     .           'USLAY233','USLAY234','USLAY241','USLAY242','USLAY243',
     .           'USLAY244','USLAY251','USLAY252','USLAY253','USLAY254',
     .           'USLAY261','USLAY262','USLAY263','USLAY264','USLAY271',
     .           'USLAY272','USLAY273','USLAY274','USLAY281','USLAY282',
     .           'USLAY283','USLAY284','USLAY291','USLAY292','USLAY293',
     .           'USLAY294','USLAY301','USLAY302','USLAY303','USLAY304',
     .           'USLAY311','USLAY312','USLAY313','USLAY314','USLAY321',
     .           'USLAY322','USLAY323','USLAY324','USLAY331','USLAY332',
     .           'USLAY333','USLAY334','USLAY341','USLAY342','USLAY343',
     .           'USLAY344','USLAY351','USLAY352','USLAY353','USLAY354',
     .           'USLAY361','USLAY362','USLAY363','USLAY364','USLAY371',
     .           'USLAY372','USLAY373','USLAY374','USLAY381','USLAY382',
     .           'USLAY383','USLAY384','USLAY391','USLAY392','USLAY393',
     .           'USLAY394','USLAY401','USLAY402','USLAY403','USLAY404',
     .           'USLAY411','USLAY412','USLAY413','USLAY414','USLAY421',
     .           'USLAY422','USLAY423','USLAY424','USLAY431','USLAY432',
     .           'USLAY433','USLAY434','USLAY441','USLAY442','USLAY443',
     .           'USLAY444','USLAY451','USLAY452','USLAY453','USLAY454',
     .           'USLAY461','USLAY462','USLAY463','USLAY464','USLAY471',
     .           'USLAY472','USLAY473','USLAY474','USLAY481','USLAY482',
     .           'USLAY483','USLAY484','USLAY491','USLAY492','USLAY493',
     .           'USLAY494','USLAY501','USLAY502','USLAY503','USLAY504',
     .           'USLAY511','USLAY512','USLAY513','USLAY514','USLAY521',
     .           'USLAY522','USLAY523','USLAY524','USLAY531','USLAY532',
     .           'USLAY533','USLAY534','USLAY541','USLAY542','USLAY543',
     .           'USLAY544','USLAY551','USLAY552','USLAY553','USLAY554',
     .           'USLAY561','USLAY562','USLAY563','USLAY564','USLAY571',
     .           'USLAY572','USLAY573','USLAY574','USLAY581','USLAY582',
     .           'USLAY583','USLAY584','USLAY591','USLAY592','USLAY593',
     .           'USLAY594','USLAY601','USLAY602','USLAY603','USLAY604',
     .           'USLAY611','USLAY612','USLAY613','USLAY614','USLAY621',
     .           'USLAY622','USLAY623','USLAY624','USLAY631','USLAY632',
     .           'USLAY633','USLAY634','USLAY641','USLAY642','USLAY643',
     .           'USLAY644','USLAY651','USLAY652','USLAY653','USLAY654',
     .           'USLAY661','USLAY662','USLAY663','USLAY664','USLAY671',
     .           'USLAY672','USLAY673','USLAY674','USLAY681','USLAY682',
     .           'USLAY683','USLAY684','USLAY691','USLAY692','USLAY693',
     .           'USLAY694','USLAY711','USLAY712','USLAY713','USLAY714',
     .           'USLAY701','USLAY702','USLAY703','USLAY704','USLAY721',
     .           'USLAY722','USLAY723','USLAY724','USLAY731','USLAY732',
     .           'USLAY733','USLAY734','USLAY741','USLAY742','USLAY743',
     .           'USLAY744','USLAY751','USLAY752','USLAY753','USLAY754',
     .           'USLAY761','USLAY762','USLAY763','USLAY764','USLAY771',
     .           'USLAY772','USLAY773','USLAY774','USLAY781','USLAY782',
     .           'USLAY783','USLAY784','USLAY791','USLAY792','USLAY793',
     .           'USLAY794','USLAY801','USLAY802','USLAY803','USLAY804',
     .           'USLAY811','USLAY812','USLAY813','USLAY814','USLAY821',
     .           'USLAY822','USLAY823','USLAY824','USLAY831','USLAY832',
     .           'USLAY833','USLAY834','USLAY841','USLAY842','USLAY843',
     .           'USLAY844','USLAY851','USLAY852','USLAY853','USLAY854',
     .           'USLAY861','USLAY862','USLAY863','USLAY864','USLAY871',
     .           'USLAY872','USLAY873','USLAY874','USLAY881','USLAY882',
     .           'USLAY883','USLAY884','USLAY891','USLAY892','USLAY893',
     .           'USLAY894','USLAY901','USLAY902','USLAY903','USLAY904',
     .           'USLAY911','USLAY912','USLAY913','USLAY914','USLAY921',
     .           'USLAY922','USLAY923','USLAY924','USLAY931','USLAY932',
     .           'USLAY933','USLAY934','USLAY941','USLAY942','USLAY943',
     .           'USLAY944','USLAY951','USLAY952','USLAY953','USLAY954',
     .           'USLAY961','USLAY962','USLAY963','USLAY964','USLAY971',
     .           'USLAY972','USLAY973','USLAY974','USLAY981','USLAY982',
     .           'USLAY983','USLAY984','USLAY991','USLAY992','USLAY993',
     .           'USLAY994','FAILURE ','WPL01_10','WPL11_20','WPL21_30',
     .           'WPL31_40','WPL41_50','WPL51_60','WPL61_70','WPL71_80',
     .           'WPL81_90','WPL91_99'/
      DATA VARCG3/
     .           'SIGV_1  ','SIGV_2  ','SIGV_3  ','SIGV_4  ','SIGV_5  ',
     .           'SIGV_6  ','SIGV_7  ','SIGV_8  ','SIGV_9  ','SIGV_10 ',
     .           'SIGV_11 ','SIGV_12 ','SIGV_13 ','SIGV_14 ','SIGV_15 ',
     .           'SIGV_16 ','SIGV_17 ','SIGV_18 ','SIGV_19 ','SIGV_10 ',
     .           'SIGV_21 ','SIGV_22 ','SIGV_23 ','SIGV_24 ','SIGV_25 ',
     .           'SIGV_26 ','SIGV_27 ','SIGV_28 ','SIGV_29 ','SIGV_30 ',
     .           'SIGV_31 ','SIGV_32 ','SIGV_33 ','SIGV_34 ','SIGV_35 ',
     .           'SIGV_36 ','SIGV_37 ','SIGV_38 ','SIGV_39 ','SIGV_40 ',
     .           'SIGV_41 ','SIGV_42 ','SIGV_43 ','SIGV_44 ','SIGV_45 ',
     .           'SIGV_46 ','SIGV_47 ','SIGV_48 ','SIGV_49 ','SIGV_50 ',
     .           'SIGV_51 ','SIGV_52 ','SIGV_53 ','SIGV_54 ','SIGV_55 ',
     .           'SIGV_56 ','SIGV_57 ','SIGV_58 ','SIGV_59 ','SIGV_60 ',
     .           'SIGV_61 ','SIGV_62 ','SIGV_63 ','SIGV_64 ','SIGV_65 ',
     .           'SIGV_66 ','SIGV_67 ','SIGV_68 ','SIGV_69 ','SIGV_70 ',
     .           'SIGV_71 ','SIGV_72 ','SIGV_73 ','SIGV_74 ','SIGV_75 ',
     .           'SIGV_76 ','SIGV_77 ','SIGV_78 ','SIGV_79 ','SIGV_80 ',
     .           'SIGV_81 ','SIGV_82 ','SIGV_83 ','SIGV_84 ','SIGV_85 ',
     .           'SIGV_86 ','SIGV_87 ','SIGV_88 ','SIGV_89 ','SIGV_90 ',
     .           'SIGV_91 ','SIGV_92 ','SIGV_93 ','SIGV_94 ','SIGV_95 ',
     .           'SIGV_96 ','SIGV_97 ','SIGV_98 ','SIGV_99 ','SIGV_100'/
      DATA VARCGPINCH/
     .           'PINCH  '/
      DATA VARTG/'DEF     '/
      DATA IVARTG
     .         / 1, 2, 3, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA VARPG/'DEF     '/
      DATA IVARPG
     .         / 1, 2, 3, 4, 5, 6, 7, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA VARRG/'DEF     '/
      DATA IVARRG
     .         / 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,65, 0, 0, 0/
      DATA VARURG/'DEF     '/
      DATA IVARURG
     .         / 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, 0, 0, 0, 0, 0, 0/
      DATA VARNSG/'DEF     '/
      DATA IVARNSG
     .         / 1, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA IVNS2R
     .         / 1, 2, 8,14, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA VARSPG/
     . 'DEF     ','STRESS  '/
      DATA IVARSPG
     .         / 1, 2, 3, 4, 5, 6, 7, 8, 9,12,13,41, 0, 0, 0, 0, 0, 0,
     .           2, 3, 4, 5, 6, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
C
      DATA VARING/'DEF     ','FN      ','FT      ','SFW     ',
     .            '|FN|    ','|F|     '/
      DATA IVARING
     .         / 1, 2, 3, 4, 5, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           4, 5, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           8, 9,10,11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .          12,13,14,15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA VARRWG/'DEF     ','FN      ','FT      '/
      DATA IVARRWG
     .         / 1, 2, 3, 4, 5, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           4, 5, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA VARRBG/'DEF     ','F       ','M       ','R       ',
     .            'FI      ','MI      '/
      DATA IVARRBG
     .         / 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           4, 5, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           7, 8, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .          10,11,12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .          13,14,15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA VARFXG/'DEF     '/
      DATA IVARFXG
     .         / 1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA VARFXMG/'DEF     '/
      DATA IVARFXMG
     .         / 1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA VARACG/'DEF     ','W       '/
      DATA IVARACG
     .         / 1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           4, 5, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA VARSEG/'DEF     ','FN      ','FT      ','M       ','CENTER ',
     .            'GLOBAL  ','LOCAL   '/
      DATA IVARSEG
     .         / 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           4, 5, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           7, 8, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .          37,38,39, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           1, 2, 3, 4, 5, 6,31,32,33, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           7, 8, 9,34,35,36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA VARJOG/'DEF     ','F       ','M       '/
      DATA IVARJOG
     .         / 1, 2, 3, 4, 5, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           4, 5, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA VARABG/'DEF     '/
      DATA IVARABG
     .         / 1, 2, 3, 4, 5, 6, 7,13,14, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA VARMVG4/'DEF     '/,VARMVG/'DEF     ','GAS     ',
     .     'OUT1    ','OUT2    ','OUT3    ','OUT4    ','OUT5    ',
     .     'OUT6    ','OUT7    ','OUT8    ','OUT9    ','OUT10   ',
     .     'AO1     ','BO1     ','UO1     ','MO1     ','HO1     ',
     .     'AO2     ','BO2     ','UO2     ','MO2     ','HO2     ',
     .     'AO3     ','BO3     ','UO3     ','MO3     ','HO3     ',
     .     'AO4     ','BO4     ','UO4     ','MO4     ','HO4     ',
     .     'AO5     ','BO5     ','UO5     ','MO5     ','HO5     ',
     .     'AO6     ','BO6     ','UO6     ','MO6     ','HO6     ',
     .     'AO7     ','BO7     ','UO7     ','MO7     ','HO7     ',
     .     'AO8     ','BO8     ','UO8     ','MO8     ','HO8     ',
     .     'AO9     ','BO9     ','UO9     ','MO9     ','HO9     ',
     .     'AO10    ','BO10    ','UO10    ','MO10    ','HO10    '/
      DATA VARMVENTG/'DEF     ','GAS     ',
     .     'OUT1    ','OUT2    ','OUT3    ','OUT4    ','OUT5    ',
     .     'OUT6    ','OUT7    ','OUT8    ','OUT9    ','OUT10   ',
     .     'AO1     ','BO1     ','UO1     ','MO1     ','HO1     ',
     .     'AO2     ','BO2     ','UO2     ','MO2     ','HO2     ',
     .     'AO3     ','BO3     ','UO3     ','MO3     ','HO3     ',
     .     'AO4     ','BO4     ','UO4     ','MO4     ','HO4     ',
     .     'AO5     ','BO5     ','UO5     ','MO5     ','HO5     ',
     .     'AO6     ','BO6     ','UO6     ','MO6     ','HO6     ',
     .     'AO7     ','BO7     ','UO7     ','MO7     ','HO7     ',
     .     'AO8     ','BO8     ','UO8     ','MO8     ','HO8     ',
     .     'AO9     ','BO9     ','UO9     ','MO9     ','HO9     ',
     .     'AO10    ','BO10    ','UO10    ','MO10    ','HO10    '/
      DATA IVARMG4
     .         / 1, 2, 3, 4, 5, 6, 7,13,14, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA IVARMVG
     .         / 1, 2, 3, 4, 5, 6, 7,13,14, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .          10,11,12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/


      DATA IVARMVENTG
     .         / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     101,102,103,104,105, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     106,107,108,109,110, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     111,112,113,114,115, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     116,117,118,119,120, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     121,122,123,124,125, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     126,127,128,129,130, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     131,132,133,134,135, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     136,137,138,139,140, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     141,142,143,144,145, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     146,147,148,149,150, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     101, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     102, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     103, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     104, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     105, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     106, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     107, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     108, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     109, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     110, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     111, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     112, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     113, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     114, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     115, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     116, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     117, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     118, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     119, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     120, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     121, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     122, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     123, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     124, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     125, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     126, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     127, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     129, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     130, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     131, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     132, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     133, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     134, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     135, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     136, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     137, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     138, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     139, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     140, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     141, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     142, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     143, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     144, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     145, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     146, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     147, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     148, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     149, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     150, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA VARPAG/'DEF     '/
      DATA IVARPAG
     .         / 1, 2, 3, 4, 5, 6, 7,28, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA VARRIVG/'DEF     '/
      DATA IVARRIV
     .         / 1, 2, 3, 4, 5, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
C   frames
      DATA VARF1G/'DEF     ','O+R     ','V+VR    '/
      DATA IVARF1G
     .         / 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,
     .           1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, 0, 0, 0, 0, 0, 0,
     .          13,14,15,16,17,18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA VARFRG/'DEF     ','O+R     ','V+VR    ','A+AR    '/
      DATA IVARFRG
     .         / 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,
     .           1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12, 0, 0, 0, 0, 0, 0,
     .          13,14,15,16,17,18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .          19,20,21,22,23,24, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA VARGAUG/'DEF     '/
      DATA IVARGAUG
     .         / 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA VARCLUSG/'DEF     ','FLOC     '/
      DATA IVARCLUSG
     .         / 1, 2, 3, 4, 5, 6,11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .           7, 8, 9,10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA VARFLOWG/'DEF     '/
      DATA IVARFLOWG
     .         / 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA VARSURFG/'DEF     '/
      DATA IVARSURFG
     .         / 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA VARSLIPG/'DEF     '/
      DATA IVARSLIPG
     .         / 1, 2, 3, 4, 5, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA VARRETG/'DEF     '/
      DATA IVARRETG
     .         / 1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
C=======================================================================
      TAGP=0
      TAGS=0
      IBID = 0
      BID = 0
C-----------------------------------------------
C  Brick
C ------------------------------------------
C   (VARS1, VARS2, VARS3, VARS4, VARS5, VARS6, VARS7 )----> VARS
      DO I=1,196
       VARS(I) = VARS1(I)
      ENDDO
C
      DO I=1,567
       VARS(196+ I) = VARS2(I)
      ENDDO
C
      DO I=1,369
       VARS(196+ 567 + I) = VARS3(I)
      ENDDO
      DO I=1,492
       VARS(196+ 567 + 369 + I) = VARS4(I)
      ENDDO
      DO I=1,22
       VARS(1624 + I) = VARS5(I)
      ENDDO
      DO I=1,97200
       VARS(1646 + I) = VARS6(I)
      ENDDO
      DO I=1,97200
       VARS(98846 + I) = VARS7(I)
      ENDDO
      DO I=1,516
       VARS(239030 + I) = VARS8(I)
      ENDDO
      DO I=1,6
       VARS(239546 + I) = VARS9(I)
      ENDDO
      DO I=1,2
       VARS(239552 + I) = VARSNLOC(I)
      ENDDO
C   (VARSG1, VARSG2, VARSG3 )----> VARSG
      DO I=1,202
       VARSG(I) = VARSG1(I)
      ENDDO
C
      DO I=1,16200
       VARSG(202 + I) = VARSG2(I)
      ENDDO
C
      DO I=1,16200
       VARSG(16402 + I) = VARSG3(I)
      ENDDO
c
      DO I=1,1
       VARSG(39766 + I) = VARSG4(I)
      ENDDO
c
      DO I=1,18
         DO J = 1,NVARSG
        IVARSG(I,J)=0
       ENDDO
      ENDDO
C  Option 1
      DO J=1,9
       IVARSG(J,1)=J
      ENDDO
      IVARSG(10,1)= 12
      IVARSG(11,1)= 13
C Option 2+3
      IVSG = 34
      DO J=1,6
       IVARSG(J,2)= J+1
       IVARSG(J,3)= IVSG + J
      ENDDO
      IVSG = IVSG + 6
C Option 4-->15      IVSG = 40
      DO I=4,15
       DO J=1,8
       IVARSG(J,I)= IVSG + J
       ENDDO
       IVSG = IVSG + 8
      ENDDO
C Option 16--->19  IVSG =  136
      DO I=16,18
       DO J=1,18
        IVARSG(J,I)=  IVSG + J
       ENDDO
       IVSG= IVSG  + 18
      ENDDO
C   Option 19 IVSG = 190
      DO J=1,6
       IVARSG(J,19)=  IVSG + J
      ENDDO
       IVSG = IVSG + 6
C   options 20--->100  IVSG=196
      DO I=20,100
       DO J=1,7
        IVARSG(J,I)= IVSG + J
       ENDDO
       IVSG = IVSG + 7
      ENDDO
C  Options  101--->118 IVSG= 763
      DO I=101,118
       DO J=1,7
        IVARSG(J,I)= IVSG + J
       ENDDO
       IVSG = IVSG + 7
      ENDDO
C  Options  118--->199 IVSG= 889
      DO I=119,199
       DO J=1,9
        IVARSG(J,I)= IVSG + J
       ENDDO
       IVSG = IVSG + 9
      ENDDO
       DO J=1,6
        IVARSG(J,200)= IVSG + J
       ENDDO
         IVSG = IVSG + 6
C  Options 201--->202 IVSG= 1624
      DO I=201,202
       DO J=1,11
        IVARSG(J,I)= IVSG + J
       ENDDO
       IVSG = IVSG + 11
      ENDDO
C  Options 203--->19984    IVSG= 1646
      DO I=203,19984
       DO J=1,6
        IVARSG(J,I)= IVSG + J
       ENDDO
       IVSG = IVSG + 6
      ENDDO
C  Options 203--->19984    IVSG= 120338
      DO I=19985,39766
       DO J=1,6
        IVARSG(J,I)= IVSG + J
       ENDDO
       IVSG = IVSG + 6
      ENDDO
C  Option LOCSTRN
      DO J=1,6
       IVARSG(J,39767)= 239030 + J
      ENDDO
C
       DO I=1,208
         VARC(I) = VARC1(I)
       ENDDO
       DO I=1,470
        VARC(208 + I) = VARC2(I)
       ENDDO
Ccc USRII_KK (II=1,20, KK=6,99
       DO II=1,20
        WRITE(CHII,'(I2.2)')II
        DO KK= 6,99
         WRITE(CHKK,'(I2.2)')KK
         IF(CHII(1:1) == '0')THEN
          VAR = 'USR'//CHII(2:2)//'_'
          IF(CHKK(1:1) == '0')THEN
           VAR = VAR(1:5)//CHKK(2:2)
          ELSE
           VAR = VAR(1:5)//CHKK
          ENDIF
         ELSE
          VAR ='USR'//CHII//'_'
          IF(CHKK(1:1) == '0')THEN
           VAR = VAR(1:6)//CHKK(2:2)
          ELSE
           VAR = VAR(1:6)//CHKK
          ENDIF
         ENDIF
         VARC(678 + (II -1)*94 + KK - 5) = VAR
        ENDDO
       ENDDO
Ccc USRII_KK(II=21,60, KK=1,99)
       DO II=21,60
        WRITE(CHII,'(I2.2)')II
        DO KK= 1,99
         WRITE(CHKK,'(I2.2)')KK
         IF(CHII(1:1) == '0')THEN
          VAR = 'USR'//CHII(2:2)//'_'
          IF(CHKK(1:1) == '0')THEN
           VAR = VAR(1:5)//CHKK(2:2)
          ELSE
           VAR = VAR(1:5)//CHKK
          ENDIF
         ELSE
          VAR ='USR'//CHII//'_'
          IF(CHKK(1:1) == '0')THEN
           VAR = VAR(1:6)//CHKK(2:2)
          ELSE
           VAR = VAR(1:6)//CHKK
          ENDIF
         ENDIF
         VARC(2558 + (II - 21)*99 + KK ) = VAR
        ENDDO
       ENDDO
C  USII_jKK (II=01, 60, j=1,4, KK=01,99)
       DO KK = 1, 99
        WRITE(CHKK,'(I2.2)')KK
        DO J = 1,4
         JJ = J*100 + KK
         WRITE(CHJJ,'(I3.3)')JJ
         DO II = 1,60
          WRITE(CHII,'(I2.2)')II
          VAR ='US'//CHII//'_'//CHJJ
          VARC(6518 + II + (KK - 1)*240 + (J-1)*60)=VAR
         ENDDO
        ENDDO
       ENDDO
       DO I = 1,104
         VARC(30278 + I ) = VARC2(470 + I)
       ENDDO
C
C viscous stress for shell 5*100
CC  USII_jKK (II=01, 60, j=1,4, KK=01,99)
       DO I = 1,500
          VARC(30382 + I ) = VARC3(I)
       ENDDO 
C   NVC_old = 30882
C  stress, strain, damage for interply      
C  SZILYjKK (j=1,4, KK=001,200)
C  SYZILYjKK (j=1,4, KK=001,200)
C  SXZILYjKK (j=1,4, KK=001,200)
C  EZILYjKK (j=1,4, KK=001,200)
C  EYZILYjKK (j=1,4, KK=001,200)
C  EXZILYjKK (j=1,4, KK=001,200)
C  SZILY_KK (KK=001,200)
C  SYZILY_KK ( KK=001,200)
C  SXZILY_KK ( KK=001,200)
C  EZILY_KK (KK=001,200)
C  EYZILY_KK ( KK=001,200)
C  EXZILY_KK ( KK=001,200)
C  DAM1ILYKK ( KK=001,200)
C  DAM2ILYKK (KK=001,200)
C  DAM3ILYKK ( KK=001,200)
C  DAM4ILYKK ( KK=001,200)
C  EINTILKK
        DO KK = 1, 199
C  3 stresss + 3 strain  + 3 average stress + 3 average strain + 4 max damage 
C   + EINT          
          II = 30882 + (KK -1)*(6*4 + 11)
          DO J = 1,4
           JJ = J*1000 + KK
           WRITE(CHJKK ,'(I4.4)')JJ
           DO L=1,6
              IF (L==1) VAR= 'SZILY'//CHJKK
              IF (L==2) VAR ='SYZILY'//CHJKK
              IF (L==3) VAR ='SXZILY'//CHJKK
C              
              IF (L==4) VAR= 'EZILY'//CHJKK
              IF (L==5) VAR ='EYZILY'//CHJKK
              IF (L==6) VAR ='EXZILY'//CHJKK
              VARC(II + L ) = VAR
           ENDDO   
           II = II + 6
          ENDDO 
            WRITE(CHJJ,'(I3.3)')KK
            DO L = 1,6              
              IF (L==1) VAR = 'SZILY'//'_'//CHJJ
              IF (L==2) VAR = 'SYZILY'//'_'//CHJJ     
              IF (L==3) VAR = 'SXZILY'//'_'//CHJJ
              IF (L==4) VAR = 'EZILY'//'_'//CHJJ
              IF (L==5) VAR = 'EYZILY'//'_'//CHJJ
              IF (L==6) VAR = 'EXZILY'//'_'//CHJJ
              VARC(II + L ) = VAR
            ENDDO
            II =II +6
            DO L = 1,4              
              IF (L==1) VAR = 'DAMILY'//CHJJ
              IF (L==2) VAR = 'DAM2ILY'//CHJJ     
              IF (L==3) VAR = 'DAM3ILY'//CHJJ
              IF (L==4) VAR = 'DAM4ILY'//CHJJ
              VARC(II + L ) = VAR
            ENDDO
            II = II + 4
            VAR = 'EINTILY'//CHJJ
            VARC(II + 1 ) = VAR
            II = II + 1
       ENDDO
C---pinching
       DO I=1,7
         VARC(37847+I) = VARCPINCH(I)
       ENDDO
C---non-local
       DO I=1,2
         VARC(37854+I) = VARCNLOC(I)
       ENDDO
C
C  NVC =   30882 + 199*34 = 37847     
Cccc array dimension of VARC  = 
C      dimension_varc =83 + 99*60 + 5*99 + 4*99*60 + 104 + 500 !  30882
C                                                  + (4*6 + 11)*199 =  37847 
C                                                  + (4*6 + 11)*199 =  37847 
C-----------------------------------------------------
C GENERALIZED KEYWORDS                          
C-----------------------------------------------------
       DO I =1,167
        VARCG(I)= VARCG1(I)
       ENDDO
       DO I = 1,407
        VARCG(167 + I) = VARCG2(I)
       ENDDO 
       DO I = 1,100
        VARCG(574 + I) = VARCG3(I)
       ENDDO
       VARCG(675) =   VARCGPINCH(1)     
C
       DO I=1,18
        DO J = 1, NVARCG
         IVARCG(I,J)=0
        ENDDO
       ENDDO
C keyword 1
       DO I=1,3
        IVARCG(I,1) = I
        IVARCG(I+3,1) = 5 + I
        IVARCG(I+6,1) = 8 + I
       ENDDO
       IVARCG(10,1) = 13
       IVARCG(11,1) = 14
C keyword 2 + 3
       DO  I = 1,8
         IVARCG(I,2) = I
         IVARCG(I,3) = 14 + I
       ENDDO
C keyword  4
       IVARCG(1,4) = 13
       IVARCG(2,4) = 14
C keyword 5,6,7
       DO I =1,18
        IVARCG(I,5)=  23 + I
        IVARCG(I,6)=  41 + I
        IVARCG(I,7)=  59 + I
       ENDDO
C keyword  8
       DO I =1,6
        IVARCG(I,8)= 77 + I
       ENDDO
       IVCG = 83
C keyword 9 -----> 28
       II = 678
       DO J = 9, 28
        DO I = 1,5
         IVARCG(I,J) = IVCG + I
        ENDDO
         IVCG = IVCG + 5
        DO I=6,18
         IVARCG(I,J) = II + I - 5
        ENDDO
         II = II + 94
       ENDDO
C keyword 29 -----> 127
        DO J = 29, 127
         DO I = 1,5
           IVARCG(I,J)= IVCG + I
         ENDDO
         IVCG = IVCG + 5
        ENDDO
C c ivcg = 183 + 5*99 + 20*94
        IVCG = IVCG + 20*94
C keyword 128 - 167
        DO J=128,167
         DO I=1,18
          IVARCG(I,J) = IVCG + I
         ENDDO
         IVCG = IVCG + 99
        ENDDO
C keyword 167 -----> 563
        DO J = 168, 563
         DO I = 1,18
           IVARCG(I,J)= IVCG + I
         ENDDO
          IVCG = IVCG + 60
        ENDDO
C keyword  564
        DO I = 1,5
          IVARCG(I,564)= IVCG + I
        ENDDO
       IVCG = IVCG + 5
c keyword  565 -----> 573
        DO J = 565,573
         DO I = 1,10
	  IVARCG(I,J)= IVCG + 1
	  IVCG = IVCG + 1
	 ENDDO
	ENDDO
c keyword  574
        DO I = 1,9
	 IVARCG(I,574)= IVCG + 1
	 IVCG = IVCG + 1
	ENDDO
C keyword de 575 -----> 674
        DO J = 575, 674
         DO I = 1,5
           IVARCG(I,J)= IVCG + I
         ENDDO
          IVCG = IVCG + 5
        ENDDO
C pinching
        DO I = 1,7 
          IVARCG(I,675)=  37847 + I
        ENDDO
C  
C to ctrole the value of ivcg                    
C icvg = 30882  
Csm end . ivcg = ivcg  + 4*99*60 + 5 + 99  + 100
C--------------fin --------------------
C--------------fin --------------------
C  ---- node + displacement by ply for shell
      DO I=1,19
       VARN(I) = VARN1(I)
      ENDDO
       DO J= 1,200
        WRITE(CHJJ,'(I3.3)')J
          VAR ='DX_'//CHJJ
          VARN(19 + (J-1)*3  + 1 )= VAR
          VAR ='DY_'//CHJJ
          VARN(19 + (J-1)*3  + 2) = VAR
          VAR ='DZ_'//CHJJ
          VARN(19 + (J-1)*3  + 3) = VAR
       ENDDO
      DO I=1,9
       VARN(619+I) = VARN2(I)
      ENDDO
      DO I=1,9 
       VARN(628+I) = VARNPINCH(I)
      ENDDO
      NNOD = 0
      CHNOD = 78
      NSHEL = 0
      NTRUS = 0
      NBEAM = 0
      NSPRG = 0
      NCHTG = 0
      NCHINT = 0
      NCHJNT = 0
C-----------------------------------------------
      NSNOD=0
      NSELS=0
      NSELQ=0
      NSELC=0
      NSELT=0
      NSELP=0
      NSELR=0
      NSELTG=0
      NSELU=0
      NSMAT=0
      NSRBY=0
C
      NSFXBY=0
C
      NSMOD=0
C     for parts and subsets:
      NTHWA=100
      NSELSP=0
      NSELRT=0
C
      OK=0
      IS_AVAILABLE = .FALSE.
      IGS=0
      IAD=1
      IREAC=0
      HM_NTHGRP=0
c
      ! Choose TH file type 
      IF (IFLAG == 0) THEN 
        THFILE = '/TH'
      ELSEIF (IFLAG == 1) THEN 
        THFILE = '/ATH'
      ELSEIF (IFLAG == 2) THEN
        THFILE = '/BTH'
      ELSEIF (IFLAG == 3) THEN
        THFILE = '/CTH'
      ELSEIF (IFLAG == 4) THEN
        THFILE = '/DTH'
      ELSEIF (IFLAG == 5) THEN
        THFILE = '/ETH'
      ELSEIF (IFLAG == 6) THEN
        THFILE = '/FTH'
      ELSEIF (IFLAG == 7) THEN
        THFILE = '/GTH'
      ELSEIF (IFLAG == 8) THEN
        THFILE = '/HTH'
      ELSEIF (IFLAG == 9) THEN
        THFILE = '/ITH'
      ENDIF
C
C      
C-----------------------------------------------      
!     **************************    !
!       /TH read with hm reader     !
!     **************************    !
C-----------------------------------------------
      !-------------------------------------------
      ! /TH/ACCEL
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/ACCEL' ,NTHACCEL  )
      IF (NTHACCEL > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/ACCEL') 
        HM_NTHGRP = HM_NTHGRP + NTHACCEL
        DO I = 1, NTHACCEL
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                            OPTION_ID      = ID, 
     .                            OPTION_TITR    = TITR)   
C
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 3
          KEY   = 'ACCEL     '
          CALL HM_READ_THGRKI(
     1      108      ,KEY      ,PTR_NOPT_ACC ,
     2      IAD      ,IFI      ,ITHGRP(1,IGS),ITHBUF   ,
     3      NVARAC   ,VARAC    ,NACCELM      ,VARACG   ,
     4      NVARACG  ,IVARACG  ,NSNE   ,NVALL,ITHVAR   ,
     5      FLAGABF  ,NVARABF  ,NOM_OPT,IGS  ,LSUBMODEL,LITHBUFMX)
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS))
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/INTER
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/INTER' ,NTHINTER  ) 
      IF (NTHINTER > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/INTER')
        HM_NTHGRP = HM_NTHGRP + NTHINTER
        DO I = 1, NTHINTER
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                            OPTION_ID      = ID, 
     .                            OPTION_TITR    = TITR)   
C
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          CHELT = 258
          CNT1  = 0
          NVALL = 7
          KEY   = 'INTER     '
          CALL HM_READ_THGRKI(
     1      101      ,KEY      ,PTR_NOPT_INTER,
     2      IAD      ,IFI      ,ITHGRP(1,IGS) ,ITHBUF  ,
     3      NVARIN   ,VARIN    ,NINTER+NINTSUB,VARING  ,NVARING  ,
     4      IVARING  ,NSNE     ,NVALL  ,ITHVAR,FLAGABF ,NVARABF   ,
     5      NOM_OPT  ,IGS      ,LSUBMODEL, LITHBUFMX)
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS))
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/RWALL
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/RWALL' ,NTHRWALL  )
      IF (NTHRWALL > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/RWALL')
        HM_NTHGRP = HM_NTHGRP + NTHRWALL
        DO I = 1, NTHRWALL
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                            OPTION_ID      = ID, 
     .                            OPTION_TITR    = TITR)   
C
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 6
          KEY   = 'RWALL     '
          CALL HM_READ_THGRKI(
     1      102      ,KEY      ,PTR_NOPT_RWALL,
     2      IAD      ,IFI      ,ITHGRP(1,IGS) ,ITHBUF  ,
     3      NVARRW   ,VARRW    ,NRWALL ,VARRWG  ,NVARRWG  ,
     4      IVARRWG  ,NSNE     ,NVALL,ITHVAR  ,FLAGABF ,NVARABF  ,
     5      NOM_OPT  ,IGS      ,LSUBMODEL, LITHBUFMX)
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS))
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/SECTIO
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/SECTIO' ,NTHSECTIO)
      IF (NTHSECTIO > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/SECTIO')
        HM_NTHGRP = HM_NTHGRP + NTHSECTIO
        DO I = 1, NTHSECTIO
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                            OPTION_ID      = ID, 
     .                            OPTION_TITR    = TITR)   
C
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 13
          KEY   = 'SECTIO    '
          CALL HM_READ_THGRKI(
     1      104      ,KEY      ,PTR_NOPT_SECT,
     2      IAD      ,IFI      ,ITHGRP(1,IGS) ,ITHBUF  ,
     3      NVARSE   ,VARSE    ,NSECT ,VARSEG ,NVARSEG ,
     4      IVARSEG  ,NSNE     ,NVALL ,ITHVAR ,FLAGABF ,NVARABF ,
     5      NOM_OPT  ,IGS      ,LSUBMODEL, LITHBUFMX)
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS))    
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/CLUSTER
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/CLUSTER' ,NTHCLUS)
      IF (NTHCLUS > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/CLUSTER')
        HM_NTHGRP = HM_NTHGRP + NTHCLUS
        DO I = 1, NTHCLUS
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                            OPTION_ID      = ID, 
     .                            OPTION_TITR    = TITR)   
C
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 11
          ITYP  = 114
          KEY   = 'CLUSTER   '
          CALL HM_READ_THGRKI(
     1      ITYP     ,KEY     ,PTR_NOPT_CLUS,
     3      IAD      ,IFI     ,ITHGRP(1,IGS),ITHBUF   ,
     4      NVARCLUS ,VARCLUS ,NCLUSTER     ,VARCLUSG ,NVARCLUSG  ,
     5      IVARCLUSG,NSNE    ,NVALL ,ITHVAR,FLAGABF  ,NVARABF    ,
     6      NOM_OPT  ,IGS     ,LSUBMODEL, LITHBUFMX    )    
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS)) 
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/BEAM
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/BEAM' ,NTHBEAM) 
      IF (NTHBEAM > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/BEAM')
        HM_NTHGRP = HM_NTHGRP + NTHBEAM
        DO I = 1, NTHBEAM
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)   
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 11
          ITYP  = 5
          KEY   = 'BEAM      ' 
          CALL HM_READ_THGRNE(
     1      ITYP  ,KEY   ,ITAB  ,ITABM1,IXP    ,
     3      NIXP  ,IAD   ,IFI   ,ITHGRP(1,IGS),ITHBUF ,
     4      NVARP ,VARP  ,NUMELP,VARPG ,NVARPG   ,
     5      IVARPG,NSELP,NVALL,IMERGE,ITHVAR   ,
     6      FLAGABF,NVARABF,RFI,LSUBMODEL,MAP_TABLES%IBEAMM, NUMELP)
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS)) 
        ENDDO
      ENDIF  
      !-------------------------------------------
      ! /TH/TRUS
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/TRUSS' ,NTHTRUS)    
      IF (NTHTRUS > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/TRUSS')
        HM_NTHGRP = HM_NTHGRP + NTHTRUS
        DO I = 1, NTHTRUS
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)   
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 6
          ITYP  = 4
          KEY   = 'TRUSS     '
          CALL HM_READ_THGRNE(
     1      ITYP  ,KEY   ,ITAB  ,ITABM1,IXT    ,
     3      NIXT  ,IAD   ,IFI   ,ITHGRP(1,IGS),ITHBUF ,    
     4      NVART ,VART  ,NUMELT,VARTG ,NVARTG   ,         
     5      IVARTG,NSELT,NVALL,IMERGE,ITHVAR   ,           
     6      FLAGABF,NVARABF,RFI,LSUBMODEL,MAP_TABLES%ITRUSSM, NUMELT)                          
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS)) 
        ENDDO
      ENDIF 
      !-------------------------------------------
      ! /TH/SPRI
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/SPRING' ,NTHSPRING)
      IF (NTHSPRING > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/SPRING') 
        HM_NTHGRP = HM_NTHGRP + NTHSPRING
        DO I = 1, NTHSPRING
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)   
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 16
          ITYP  = 6
          KEY   = 'SPRING    ' 
          CALL HM_READ_THGRNE(
     1      ITYP  ,KEY   ,ITAB  ,ITABM1,IXR    ,
     3      NIXR  ,IAD   ,IFI   ,ITHGRP(1,IGS),ITHBUF ,    
     4      NVARR ,VARR  ,NUMELR,VARRG ,NVARRG   ,         
     5      IVARRG,NSELR,NVALL,IMERGE,ITHVAR   ,           
     6      FLAGABF,NVARABF,RFI,LSUBMODEL,MAP_TABLES%ISPRINGM, NUMELR)                           
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS)) 
        ENDDO
      ENDIF 
      !-------------------------------------------
      ! /TH/BRIC
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/BRIC' ,NTHBRIC)      
      IF (NTHBRIC > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/BRIC')
        HM_NTHGRP = HM_NTHGRP + NTHBRIC
        DO I = 1, NTHBRIC
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)   
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 40
          ITYP  = 1
          KEY   = 'BRICK     ' 
          CALL HM_READ_THGRNE(
     1      ITYP  ,KEY   ,ITAB  ,ITABM1,IXS    ,
     3      NIXS  ,IAD   ,IFI   ,ITHGRP(1,IGS),ITHBUF ,
     4      NVARS ,VARS  ,NUMELS,VARSG ,NVARSG   ,
     5      IVARSG,NSELS,NVALL,IMERGE,ITHVAR   ,
     6      FLAGABF,NVARABF,RFI,LSUBMODEL,MAP_TABLES%ISOLM, NUMELS)
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS)) 
        ENDDO
      ENDIF 
      !-------------------------------------------
      ! /TH/NODE
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/NODE' ,NTHNODE)  
      IF (NTHNODE > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/NODE') 
        HM_NTHGRP = HM_NTHGRP + NTHNODE
        DO I = 1, NTHNODE
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)   
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 9
          ITYP  = 0
          KEY   = 'NODE      '
          CALL HM_READ_THGRNE(
     1      ITYP  ,KEY   ,ITAB  ,ITABM1  ,IXS    ,         
     3      NIXS  ,IAD   ,IFI   ,ITHGRP(1,IGS),ITHBUF ,    
     4      NVARN ,VARN  ,0     ,VARNG   ,NVARNG   ,       
     5      IVARNG,NSNOD ,NVALL ,IMERGE  ,ITHVAR   ,       
     6      FLAGABF,NVARABF,RFI,LSUBMODEL,IBID,IBID)                          
C         decode les numeros de skews.                               
          NNE =ITHGRP(4,IGS)                                         
          IAD2=ITHGRP(5,IGS)+NNE                                     
          DO K=1,NNE                                                 
           ISK=ITHBUF(IAD2)                                          
           DO 100 J=1,(NUMSKW+1)+NSUBMOD                             
            IF(ISK == ISKWN(4,J)) THEN                               
             ITHBUF(IAD2)=J-1                                        
             GO TO 110                                               
            ENDIF                                                    
 100       CONTINUE                                                  
C          decode les numeros de frames.                             
           DO 101 J=(NUMSKW+1)+MIN(IUN,NSPCOND)*NUMSPH+1+NSUBMOD,    
     .              (NUMSKW+1)+MIN(IUN,NSPCOND)*NUMSPH+NSUBMOD+      
     .               (NUMFRAM+1)                                     
            IF(ISK == ISKWN(4,J)) THEN                               
             ITHBUF(IAD2)=J-1                                        
             GO TO 110                                               
            ENDIF                                                    
 101       CONTINUE                                                  
           CALL ANCMSG(MSGID=434,                                    
     .                 MSGTYPE=MSGERROR,                             
     .                 ANMODE=ANINFO_BLIND_1,                        
     .                 I1=ID,                                        
     .                 C1=TITR,                                      
     .                 I2=ISK)                                       
 110       CONTINUE                                                  
           IAD2=IAD2+1                                               
          ENDDO                                                      
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS)) 
        ENDDO
      ENDIF 
      !-------------------------------------------
      ! /TH/SHEL
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/SHEL' ,NTHSHEL)      
      IF (NTHSHEL > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/SHEL')
        HM_NTHGRP = HM_NTHGRP + NTHSHEL
        DO I = 1, NTHSHEL
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)   
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 22
          ITYP  = 3
          KEY   = 'SHELL     '
          CALL HM_READ_THGRNE(
     1      ITYP  ,KEY   ,ITAB  ,ITABM1,IXC    ,
     3      NIXC  ,IAD   ,IFI   ,ITHGRP(1,IGS),ITHBUF ,
     4      NVARC ,VARC  ,NUMELC,VARCG ,NVARCG   ,
     5      IVARCG,NSELC,NVALL,IMERGE,ITHVAR   ,
     6      FLAGABF,NVARABF,RFI,LSUBMODEL,MAP_TABLES%ISH4NM, NUMELC)                           
C         decode les numeros de skews.                                  
          NNE =ITHGRP(4,IGS)                                            
          IAD2=ITHGRP(5,IGS)+3*NNE                                      
          DO K=1,NNE                                                    
           ISK=ITHBUF(IAD2)                                             
           DO 200 J=1,(NUMSKW+1)+NSUBMOD                                
            IF(ISK == ISKWN(4,J)) THEN                                  
             ITHBUF(IAD2)=J-1                                           
             GO TO 210                                                  
            ENDIF                                                       
 200       CONTINUE                                                     
C          frame numbers user -> internal                               
           DO 201 J=(NUMSKW+1)+MIN(IUN,NSPCOND)*NUMSPH+1+NSUBMOD,       
     .              (NUMSKW+1)+MIN(IUN,NSPCOND)*NUMSPH+NSUBMOD+         
     .               (NUMFRAM+1)                                        
            IF(ISK == ISKWN(4,J)) THEN                                  
             ITHBUF(IAD2)=J-1                                           
             GO TO 210                                                  
            ENDIF                                                       
 201       CONTINUE                                                     
           CALL ANCMSG(MSGID=434,                                       
     .                 MSGTYPE=MSGERROR,                                
     .                 ANMODE=ANINFO_BLIND_1,                           
     .                 I1=ID,                                           
     .                 C1=TITR,                                         
     .                 I2=ISK)                                          
 210       CONTINUE                                                     
           IAD2=IAD2+1                                                  
          ENDDO                                                         
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS)) 
        ENDDO
      ENDIF 
      !-------------------------------------------
      ! /TH/SH3N
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/SH3N' ,NTHSH3N)  
      IF (NTHSH3N > 0) THEN
        CALL HM_OPTION_START(TRIM(THFILE)//'/SH3N') 
        HM_NTHGRP = HM_NTHGRP + NTHSH3N
        DO I = 1, NTHSH3N
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)   
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 22
          ITYP  = 7
          KEY   = 'SH3N      '  
          CALL HM_READ_THGRNE(
     1      ITYP  ,KEY   ,ITAB  ,ITABM1,IXTG   ,
     3      NIXTG ,IAD   ,IFI   ,ITHGRP(1,IGS),ITHBUF ,    
     4      NVARC ,VARC  ,NUMELTG,VARCG ,NVARCG ,          
     5      IVARCG,NSELTG,NVALL,IMERGE,ITHVAR   ,          
     6      FLAGABF,NVARABF,RFI,LSUBMODEL,MAP_TABLES%ISH3NM, NUMELTG)                              
C          skew numbers user -> internal          
          NNE =ITHGRP(4,IGS)                                                   
          IAD2=ITHGRP(5,IGS)+3*NNE                                             
          DO K=1,NNE                                                           
           ISK=ITHBUF(IAD2)                                                    
           DO 300 J=1,(NUMSKW+1)+NSUBMOD                                       
            IF(ISK == ISKWN(4,J)) THEN                                         
             ITHBUF(IAD2)=J-1                                                  
             GO TO 310                                                         
            ENDIF                                                              
 300       CONTINUE                                                    
C          frame numbers user -> internal                                       
           DO 301 J=(NUMSKW+1)+MIN(IUN,NSPCOND)*NUMSPH+1+NSUBMOD,              
     .              (NUMSKW+1)+MIN(IUN,NSPCOND)*NUMSPH+NSUBMOD+                
     .               (NUMFRAM+1)                                               
            IF(ISK == ISKWN(4,J)) THEN                                         
             ITHBUF(IAD2)=J-1                                                  
             GO TO 310                                                         
            ENDIF                                                              
 301       CONTINUE                                                            
           CALL ANCMSG(MSGID=434,                                              
     .                 MSGTYPE=MSGERROR,                                       
     .                 ANMODE=ANINFO_BLIND_1,                                  
     .                 I1=ID,                                                  
     .                 C1=TITR,                                                
     .                 I2=ISK)                                                 
 310       CONTINUE                                                            
           IAD2=IAD2+1                                                         
          ENDDO                                                                
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS)) 
        ENDDO
      ENDIF  
      !-------------------------------------------
      ! /TH/RBODY
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/RBODY' ,NTHRBODY)         
      IF (NTHRBODY > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/RBODY')
        HM_NTHGRP = HM_NTHGRP + NTHRBODY
        DO I = 1, NTHRBODY
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)   
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 15
          ITYP  = 103
          KEY   = 'RBODY     '
          CALL HM_READ_THGRKI_RBODY(
     1          ITYP   ,KEY    ,0,
     3          IAD    ,IFI    ,ITHGRP(1,IGS),ITHBUF   ,
     4          NVARRB ,VARRB  ,NRBODY       ,VARRBG   ,NVARRBG,
     5          IVARRBG,NSRBY  ,NVALL        ,ITHVAR   ,FLAGABF,NVARABF,
     6          NOM_OPT,IGS    ,NPBY         ,LSUBMODEL    )    
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS)) 
        ENDDO
      ENDIF      
      !-------------------------------------------
      ! /TH/MONVOL
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/MONV' ,NTHMONVOL)    
      IF (NTHMONVOL > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/MONV') 
        HM_NTHGRP = HM_NTHGRP + NTHMONVOL
        DO I = 1, NTHMONVOL
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)   
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 9
          ITYP  = 107
          KEY   = 'MONVOL    '
          CALL HM_READ_THGRKI(
     1          ITYP   ,KEY    ,PTR_NOPT_MONV,
     3          IAD    ,IFI    ,ITHGRP(1,IGS),ITHBUF ,
     4          NVARMV ,VARMV  ,NVOLU ,VARMVG,NVARMVG,
     5          IVARMVG,NSNE,NVALL,ITHVAR,FLAGABF,NVARABF,
     6          NOM_OPT,IGS    ,LSUBMODEL,LITHBUFMX    )    
          NVALL = 9                                               
          IGS=IGS+1                                               
          TITR1(1:5)='VENT '                                      
   	      DO J=1,nchartitle-5                                 
   	        TITR1(J+5:J+5) = TITR(J:J)                        
   	      ENDDO                                               
          CALL FRETITL(TITR1,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)    
          ITHGRP(1,IGS)=ID_TH_AUTO                                
          ID_TH_AUTO = ID_TH_AUTO + 1                             
          CALL HM_THGRKI_VENT(                                       
     1      ITYP      ,KEY       ,PTR_NOPT_MONV,                        
     3      IAD       ,IFI       ,ITHGRP(1,IGS),ITHBUF ,                
     4      NVARMVENT ,VARMVENT  ,NVOLU        ,VARMVENTG, NVARMVENTG,    
     5      IVARMVENTG,NSNE      ,NVALL        ,ITHVAR   , FLAGABF   ,NVARABF,         
     6      NOM_OPT   ,IGS       ,T_MONVOL     ,NVARMVENT, LSUBMODEL)                       
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS)) 
        ENDDO
      ENDIF    
      !-------------------------------------------
      ! /TH/PART
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/PART' ,HM_NTHPART)  
      IF (HM_NTHPART > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/PART') 
        HM_NTHGRP = HM_NTHGRP + HM_NTHPART
        NUMTHPART = NPART + NTHPART
        DO I = 1, HM_NTHPART
          CALL HM_READ_THGRPA(
     .         IPART    ,LIPART1  ,ITHBUF  ,IAD      ,IFI       ,
     .         VARPA    ,NVARPA   ,VARPAG  ,NVARPAG  ,NUMTHPART ,
     .         IVARPAG  ,PATHID   ,TAGP    ,IPARTH   ,NPARTH    ,
     .         NVPARTH  ,LSUBMODEL)
        ENDDO
      ENDIF     
      !-------------------------------------------
      ! /TH/SUBS
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/SUBS' ,HM_NTHSUBS)
      IF (HM_NTHSUBS > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/SUBS') 
        HM_NTHGRP = HM_NTHGRP + HM_NTHSUBS
        DO I = 1, HM_NTHSUBS
          CALL HM_READ_THGRPA_SUB(
     .         IAD      ,IFI      ,ITHBUF   ,NVARPA   ,VARPA    ,
     .         VARPAG   ,NVARPAG  ,IVARPAG  ,PATHID   ,
     .         SUTHID   ,TAGS     ,SUBSET   ,ITHFLAG  ,LSUBMODEL)         
        ENDDO 
      ENDIF 
      !-------------------------------------------
      ! /TH/QUAD
      !-------------------------------------------
      TH_STRAIN = 0 ! Time history strain flag for QUADS -> TH output STRAIN TENSOR
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/QUAD' ,HM_NTHQUAD  )     
      IF (HM_NTHQUAD > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/QUAD')
        HM_NTHGRP = HM_NTHGRP + HM_NTHQUAD
        DO I = 1, HM_NTHQUAD
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID = ID, OPTION_TITR = TITR)   
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 40
          KEY   = 'QUAD      '          
          CALL HM_READ_THGRNE(
     1        2     ,KEY   ,ITAB  ,ITABM1,IXQ    ,
     3        NIXQ  ,IAD   ,IFI   ,ITHGRP(1,IGS),ITHBUF ,
     4        NVARS ,VARS  ,NUMELQ,VARSG ,NVARSG   ,
     5        IVARSG,NSELQ,NVALL,IMERGE,ITHVAR   ,
     6        FLAGABF,NVARABF,RFI,LSUBMODEL,MAP_TABLES%IQUADM, NUMELQ) 
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS))    
        ENDDO
      ENDIF 
      !-------------------------------------------
      ! /TH/SPHCEL
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/SPHCEL' ,HM_NTHSPHCEL  )  
      IF (HM_NTHSPHCEL > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/SPHCEL') 
        HM_NTHGRP = HM_NTHGRP + HM_NTHSPHCEL
        DO I = 1, HM_NTHSPHCEL
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID = ID, OPTION_TITR = TITR)   
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 41
          KEY   = 'SPHCEL    '          
          CALL HM_READ_THGRNE(
     1       51   ,KEY   ,ITAB  ,ITABM1,KXSP    ,
     2       NISP  ,IAD   ,IFI   ,ITHGRP(1,IGS),ITHBUF ,
     4       NVARSPH,VARSPH,NUMSPH,VARSPG,NVARSPG ,
     5       IVARSPG,NSELSP,NVALL,IMERGE,ITHVAR   ,
     6       FLAGABF,NVARABF,RFI,LSUBMODEL,MAP_TABLES%ISPHM,NUMSPH) 
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS))    
        ENDDO
      ENDIF    
      !-------------------------------------------
      ! /TH/CYL_JO
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/CYL_JO' ,HM_NTHCYLJO)
      IF (HM_NTHCYLJO > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/CYL_JO')
        HM_NTHGRP = HM_NTHGRP + HM_NTHCYLJO
        DO I = 1, HM_NTHCYLJO
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID = ID, OPTION_TITR = TITR)   
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 6
          KEY   = 'CYL_JO    '          
          CALL HM_READ_THGRKI(
     1      105      ,KEY      ,PTR_NOPT_JOINT,
     2      IAD      ,IFI      ,ITHGRP(1,IGS) ,ITHBUF  ,
     3      NVARJO   ,VARJO    ,NJOINT,VARJOG,NVARJOG,
     4      IVARJOG  ,NSNE     ,NVALL ,ITHVAR ,FLAGABF ,NVARABF ,
     5      NOM_OPT  ,IGS      ,LSUBMODEL, LITHBUFMX)
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS))    
        ENDDO
      ENDIF    
      !-------------------------------------------
      ! /TH/FXBODY
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/FXBODY' ,HM_NTHFXBODY)
      IF (HM_NTHFXBODY > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/FXBODY')
        HM_NTHGRP = HM_NTHGRP + HM_NTHFXBODY
        DO I = 1, HM_NTHFXBODY
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID = ID, OPTION_TITR = TITR)   
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 0
          KEY   = 'FXBODY    '          
          CALL HM_READ_THGRKI(
     1      111      ,KEY      ,PTR_NOPT_FXBY,
     2      IAD      ,IFI      ,ITHGRP(1,IGS) ,ITHBUF  ,
     3      NVARFX   ,VARFX    ,NFXBODY       ,VARFXG  ,NVARFXG,
     4      IVARFXG  ,NSNE     ,NVALL ,ITHVAR ,FLAGABF ,NVARABF ,
     5      NOM_OPT  ,IGS      ,LSUBMODEL, LITHBUFMX )
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS))    
        ENDDO
      ENDIF   
      !-------------------------------------------
      ! /TH/FRAME
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/FRAME' ,HM_NTHFRAME) 
      IF (HM_NTHFRAME > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/FRAME')
        HM_NTHGRP = HM_NTHGRP + HM_NTHFRAME
        DO I = 1, HM_NTHFRAME
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID = ID, OPTION_TITR = TITR)   
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 24
          KEY   = 'FRAME     '          
          CALL HM_READ_THGRKI(
     1      110      ,KEY      ,PTR_NOPT_SKW+NUMSKW+1,
     2      IAD      ,IFI      ,ITHGRP(1,IGS) ,ITHBUF  ,
     3      NVARFR   ,VARFR    ,NUMFRAM+1     ,VARFRG, NVARFRG  ,
     4      IVARFRG  ,NSNE     ,NVALL ,ITHVAR ,FLAGABF ,NVARABF ,
     5      NOM_OPT  ,IGS      ,LSUBMODEL, LITHBUFMX)
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS))    
        ENDDO
      ENDIF   
      !-------------------------------------------
      ! /TH/GAUGE
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/GAUGE' ,HM_NTHGAUGE)
      IF (HM_NTHGAUGE > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/GAUGE')
        HM_NTHGRP = HM_NTHGRP + HM_NTHGAUGE
        DO I = 1, HM_NTHGAUGE
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID = ID, OPTION_TITR = TITR)   
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 3
          KEY   = 'GAUGE     '          
          CALL HM_READ_THGRKI(
     1      113      ,KEY      ,PTR_NOPT_GAU,
     2      IAD      ,IFI      ,ITHGRP(1,IGS) ,ITHBUF  ,
     3      NVARGAU  ,VARGAU   ,NBGAUGE       ,VARGAUG,NVARGAUG  ,
     4      IVARGAUG ,NSNE     ,NVALL ,ITHVAR ,FLAGABF ,NVARABF ,
     5      NOM_OPT  ,IGS      ,LSUBMODEL, LITHBUFMX)
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS))    
        ENDDO
      ENDIF                           
      !-------------------------------------------
      ! /TH/SPH_FLOW
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/SPH_FLOW' ,HM_NTHSPHFLOW)   
      IF (HM_NTHSPHFLOW > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/SPH_FLOW')
        HM_NTHGRP = HM_NTHGRP + HM_NTHSPHFLOW
        DO I = 1, HM_NTHSPHFLOW
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID = ID, OPTION_TITR = TITR)   
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 1
          KEY   = 'SPH_FLOW  '          
          CALL HM_READ_THGRKI(
     1      115      ,KEY      ,PTR_NOPT_SPHIO,
     2      IAD      ,IFI      ,ITHGRP(1,IGS) ,ITHBUF  ,
     3      NVARFLOW,VARFLOW,NSPHIO,VARFLOWG,NVARFLOWG ,
     5      IVARFLOWG,NSNE     ,NVALL ,ITHVAR ,FLAGABF ,NVARABF ,
     5      NOM_OPT  ,IGS      ,LSUBMODEL,LITHBUFMX)
C             error message if type of inout is not correct
              DO J=1,ITHGRP(4,IGS)
                INOUT = ITHBUF(ITHGRP(5,IGS)+J-1)
                IF (INOUT>0) THEN
                  IF (ISPHIO(1,INOUT) == 1) THEN
                    CALL ANCMSG(MSGID=1111,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO_BLIND_1,
     .                     I1=ITHGRP(1,IGS),
     .                     C1=TITR,
     .                     I2=NOM_OPT(1,PTR_NOPT_SPHIO+INOUT))
                  ENDIF
                ENDIF
              END DO     
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS))    
        ENDDO
      ENDIF       
      !-------------------------------------------
      ! /TH/NSTRAND
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/NSTRAND' ,HM_NTHNSTRAND  )         
      IF (HM_NTHNSTRAND > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/NSTRAND')
        HM_NTHGRP = HM_NTHGRP + HM_NTHNSTRAND
        DO I = 1, HM_NTHNSTRAND
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)   
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 4
          ITYP  = 100
          KEY   = 'NSTRAND   '
          CALL HM_READ_THGRNS(
     1          100    ,KEY   ,ITAB   ,ITABM1,KXX    ,
     3          IXX    ,IAD   ,IFI    ,ITHGRP(1,IGS),ITHBUF ,
     4          NVARNS ,VARNS ,VARNSG ,NVARNSG ,IVARNSG,
     5          NSELR  ,IVNS2R,NVALL  ,ID,TITR,
     6          ITHVAR,FLAGABF,NVARABF,LSUBMODEL)           
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS)) 
        ENDDO
      ENDIF          
      !-------------------------------------------
      ! /TH/SURF
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/SURF' ,HM_NTHSURF  )          
      IF (HM_NTHSURF> 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/SURF')
        HM_NTHGRP = HM_NTHGRP + HM_NTHSURF
        DO I = 1, HM_NTHSURF
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)   
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 1
          ITYP  = 116
          KEY   = 'SURF      '
          CALL HM_READ_THGRSURF(
     1          ITYP   ,KEY   ,
     3          IAD    ,IFI   ,ITHGRP(1,IGS),ITHBUF ,
     4          NVARSURF,VARSURF,NSURF,VARSURFG,NVARSURFG ,
     5          IVARSURFG,NSNE,NVALL,ITHVAR,FLAGABF,NVARABF,
     6          IGRSURF,IGS,LSUBMODEL)          
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS)) 
        ENDDO
      ENDIF  
      !-------------------------------------------
      ! /TH/TRIA
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/TRIA' ,HM_NTHTRIA  )  
      IF (HM_NTHTRIA > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/TRIA')
        HM_NTHGRP = HM_NTHGRP + HM_NTHTRIA
        DO I = 1, HM_NTHTRIA
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID=ID, OPTION_TITR=TITR)   
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 40
          ITYP  = 117
          KEY   = 'TRIA      ' 
          CALL HM_READ_THGRNE(
     1      ITYP  ,KEY   ,ITAB  ,ITABM1,IXTG   ,
     3      NIXTG ,IAD   ,IFI   ,ITHGRP(1,IGS),ITHBUF ,    
     4      NVARS ,VARS  ,NUMELTG,VARSG ,NVARSG ,          
     5      IVARSG,NSELTG,NVALL,IMERGE,ITHVAR   ,          
     6      FLAGABF,NVARABF,RFI,LSUBMODEL,MAP_TABLES%ITRIAM, NUMELTG)        
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS)) 
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/SLIPRING
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/SLIPRING' ,HM_NTHSLIPRING)
      IF (HM_NTHSLIPRING > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/SLIPRING')
        HM_NTHGRP = HM_NTHGRP + HM_NTHSLIPRING
        DO I = 1, HM_NTHSLIPRING
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID = ID, OPTION_TITR = TITR)   
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 6
          ITYP  = 118
          KEY   = 'SLIPRING  '     
          CALL HM_READ_THGRKI(
     1      ITYP     ,KEY      ,PTR_NOPT_SLIPRING,
     2      IAD      ,IFI      ,ITHGRP(1,IGS) ,ITHBUF  ,
     3      NVARSLIP ,VARSLIP  ,NSLIPRING  ,VARSLIPG,NVARSLIPG  ,
     4      IVARSLIPG,NSNE     ,NVALL ,ITHVAR ,FLAGABF ,NVARABF ,
     5      NOM_OPT  ,IGS      ,LSUBMODEL, LITHBUFMX)
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS))    
        ENDDO
      ENDIF
      !-------------------------------------------
      ! /TH/RETRACTOR
      !-------------------------------------------
      CALL HM_OPTION_COUNT(TRIM(THFILE)//'/RETRACTOR' ,HM_NTHRETRACTOR)
      IF (HM_NTHRETRACTOR > 0) THEN 
        CALL HM_OPTION_START(TRIM(THFILE)//'/RETRACTOR')
        HM_NTHGRP = HM_NTHGRP + HM_NTHRETRACTOR
        DO I = 1, HM_NTHRETRACTOR
          TITR = ''   
          CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID = ID, OPTION_TITR = TITR)   
          IGS = IGS+1
          CALL FRETITL(TITR,ITHGRP(NITHGR-LTITR+1,IGS),LTITR)
          ITHGRP(1,IGS) = ID
          NVALL = 3
          ITYP  = 119
          KEY   = 'RETRACTOR '    
          CALL HM_READ_THGRKI(
     1      ITYP     ,KEY      ,PTR_NOPT_RETRACTOR,
     2      IAD      ,IFI      ,ITHGRP(1,IGS) ,ITHBUF  ,
     3      NVARRET  ,VARRET   ,NRETRACTOR  ,VARRETG,NVARRETG  ,
     4      IVARRETG ,NSNE     ,NVALL ,ITHVAR ,FLAGABF ,NVARABF ,
     5      NOM_OPT  ,IGS      ,LSUBMODEL, LITHBUFMX)
          IF(IGS/=0) NTHWA=MAX(NTHWA,ITHGRP(4,IGS)*ITHGRP(6,IGS))    
        ENDDO
      ENDIF
C
C=======================================================================
C NB of UNDEFINED TH SELECTED NODE FOR INVERS>=40
C ( for INVERS LT 40, REFER TO LECNOD )
C=======================================================================
c      CHNOD = 9
c      CALL ANCHECK(CHNOD)
C=======================================================================
C NB of UNDEFINED TH SELECTED SHEL FOR INVERS>=40
C ( for INVERS LT 40, REFER TO LEC16C )
C=======================================================================
c      CHELT = 19
c      CALL ANCHECK(CHELT)
C=======================================================================
C NB of UNDEFINED TH SELECTED TRUSS FOR INVERS>=40
C ( for INVERS LT 40, REFER TO LEC16T )
C=======================================================================
c      CHELT = 25
c      CALL ANCHECK(CHELT)
C=======================================================================
C NB of UNDEFINED TH SELECTED BEAM FOR INVERS>=40
C ( for INVERS LT 40, REFER TO LEC16P )
C=======================================================================
c      CHELT = 32
c      CALL ANCHECK(CHELT)
C=======================================================================
C NB of UNDEFINED TH SELECTED SPRING FOR INVERS>=40
C ( for INVERS LT 40, REFER TO LECELR )
C=======================================================================
c      CHELT = 39
c      CALL ANCHECK(CHELT)
C=======================================================================
C NB of UNDEFINED TH SELECTED SHELL3N FOR INVERS>=40
C ( for INVERS LT 40, REFER TO C3LECEL )
C=======================================================================
c      CHELT = 46
c      CALL ANCHECK(CHELT)
C=======================================================================
C NB of UNDEFINED TH SELECTED INTERFACE FOR INVERS>=40
C ( for INVERS LT 40, REFER TO  )
C=======================================================================
c      CHELT = 62
c      CALL ANCHECK(CHELT)
C=======================================================================
C NB of UNDEFINED TH SELECTED JOINT FOR INVERS>=40
C ( for INVERS LT 40, REFER TO  )
C=======================================================================
c      CHELT = 84
c      CALL ANCHECK(CHELT)
      DO N=1,NPART
        IF(IPART(8,N)>0) NSMAT=NSMAT+1
      ENDDO
C=======================================================================
C Tag des groupes avec des parts dans les TH PART et SUBSET non DEF
C=======================================================================
      DO NG=1,NGROUP
        NEL=IPARG(2,NG)
        NFT=IPARG(3,NG)
        ITY=IPARG(5,NG)
        IF (ITY == 1) THEN
          IPARTEL_P => IPARTS ! 3D solids
        ELSEIF (ITY == 2) THEN
          IPARTEL_P => IPARTQ ! Quads
        ELSEIF (ITY == 3) THEN
          IPARTEL_P => IPARTC ! Shells
        ELSEIF (ITY == 4) THEN 
          IPARTEL_P => IPARTT ! Truss
        ELSEIF (ITY == 5) THEN
          IPARTEL_P => IPARTP ! Beams
        ELSEIF (ITY == 6) THEN
          IPARTEL_P => IPARTR ! Springs
        ELSEIF (ITY == 7 .OR. ITY == 8) THEN
          IPARTEL_P => IPARTG ! SH3N or Tria
        ELSEIF (ITY == 51) THEN
          IPARTEL_P => IPARTSP ! Sph
        ELSEIF (ITY == 100) THEN
          IPARTEL_P => IPARTX ! Xelems
        ELSEIF (ITY == 101) THEN
          IPARTEL_P => IPARTIG3D ! Ig3d
        ENDIF
        DO J=NFT+1,NFT+NEL
          IF(IPARG(80,NG) == 1) EXIT
          NUMPART = IPARTEL_P(J)
          IF(IPARTH(NVPARTH,NUMPART)>0) THEN
            NVAR =IPARTH(NVPARTH,NUMPART)       
            IAD  =IPARTH(NVPARTH+1,NUMPART)       
            DO N=IAD,IAD+NVAR-1
              K=ITHBUF(N)
              IF(K>7.AND.K/=28) THEN
                IPARG(80,NG) = 1
                EXIT
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO
C=======================================================================
C print groupe de PART et SUBSET
C=======================================================================
      CALL THPRIN(
     1          'PART      ',IPART ,LIPART1    ,4     ,
     2          NVPARTH  ,ITHBUF ,VARPA  ,NPART + NTHPART  )
      CALL THPRIN_SUB(
     1          'SUBSET    ',SUBSET ,ITHFLAG  ,1     ,
     2          NVSUBTH  ,ITHBUF    ,VARPA    ,NSUBS )
C
C     LES GROUPES TH PART ET SUBSET NE SONT PLUS UTILISES
C
      NTHGRP2=IGS
C------------------------------------------------
C Recherce des parts ou subsets dans plusieurs TH
C------------------------------------------------
      DO I=1,TAGP
        DO J=1, TAGP
          IF((I < J).AND.(PATHID(I) == PATHID(J)))THEN
            CALL ANCMSG(MSGID=553,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  C1='PART',
     .                  I1=PATHID(I))
          ENDIF
       ENDDO
      ENDDO
      DO I=1,TAGS
        DO J=1, TAGS
          IF((I < J).AND.(SUTHID(I) == SUTHID(J)))THEN
            CALL ANCMSG(MSGID=553,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               C1='SUBSET',
     .               I1=SUTHID(I))
          ENDIF
        ENDDO
      ENDDO
C-------------------------------------
C Recherche des ID doubles
C-------------------------------------
      CALL UDOUBLE(ITHGRP,NITHGR,NTHGRP2,MESS,0,BID)
C
      RETURN
      END