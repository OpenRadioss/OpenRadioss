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
Chd|  RESTALLOC                     source/output/restart/arralloc.F
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        ALEFVM_INIT                   source/ale/alefvm/alefvm_init.F
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MULTI_ALLOCATE                source/multifluid/multi_allocate.F
Chd|        SZ_PRINT                      source/output/restart/arralloc.F
Chd|        ALEANIM_MOD                   share/modules/aleanim_mod.F   
Chd|        ALEFVM_MOD                    ../common_source/modules/alefvm_mod.F
Chd|        ALEMUSCL_MOD                  ../common_source/modules/alemuscl_mod.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        CLUSTER_MOD                   share/modules/cluster_mod.F   
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ECND_MOD                      share/modules/ecdn_mod.F      
Chd|        EIG_MOD                       share/modules/eig_mod.F       
Chd|        FSKYI_MOD                     share/modules/restart_mod.F   
Chd|        FXB_MOD                       share/modules/fxb_mod.F       
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFFRIC_MOD                share/modules/restart_mod.F   
Chd|        INTBUFMOD                     share/modules/restart_mod.F   
Chd|        INTBUF_MOD                    share/modules/restart_mod.F   
Chd|        INTSTAMP_GLOB_MOD             share/modules/intstamp_glob_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        OUTMAX_MOD                    ../common_source/modules/outmax_mod.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|        RBODY_MOD                     share/modules/rbody_mod.F     
Chd|        RESTMOD                       share/modules/restart_mod.F   
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|        SPH_MOD                       share/modules/sph_mod.F       
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|        TRIMAT_MOD                    share/modules/trimat.F        
Chd|====================================================================
      SUBROUTINE RESTALLOC(MULTI_FVM,H3D_DATA,PINCH_DATA,ALE_CONNECTIVITY,SEGVAR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFMOD
      USE INTBUF_MOD
      USE RESTMOD
      USE INTSTAMP_GLOB_MOD
      USE FSKYI_MOD
      USE MESSAGE_MOD
      USE SPH_MOD
      USE CLUSTER_MOD
      USE ALEFVM_MOD
      USE STACK_MOD
      USE ALEMUSCL_MOD
      USE TRIMAT_MOD
      USE ECND_MOD
      USE MULTI_FVM_MOD
      USE ALEANIM_MOD
      USE H3D_MOD
      USE INTBUFFRIC_MOD                     
      USE PINCHTYPE_MOD 
      USE DRAPE_MOD
      USE FXB_MOD
      USE EIG_MOD               
      USE RBODY_MOD               
      USE ALE_CONNECTIVITY_MOD
      USE SEATBELT_MOD    
      USE SEGVAR_MOD 
      USE OUTMAX_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "my_allocate.inc"
#include      "r4r8_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "sms_c.inc"
#include      "tabsiz_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "couple_c.inc"
#include      "warn_c.inc"
#include      "parit_c.inc"
#include      "intstamp_c.inc"
#include      "spmd_c.inc"
#include      "sphcom.inc"
#include      "alefvm.inc"
#include      "drape_c.inc"
#include      "thermal_c.inc"
#include      "alemuscl.inc"
#include      "ale_param.inc"
#include      "fxbcom.inc"
#include      "eigcom.inc"
#include      "inter18.inc"
#include      "stati_c.inc"
#include      "com09_c.inc"
C-----------------------------------------------
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(PINCH) :: PINCH_DATA
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
      TYPE(t_SEGVAR) :: SEGVAR
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IERR0,IERROR, NBMAT,I,NPT
C-----------------------------------------------
      IERROR = 0
C pour eviter conflit avec IERR dans commun
      IERR0 = IERR
C   -----------------------------------------------
C             allocations entieres 
C   -----------------------------------------------

C       
        ALLOCATE (ICODE(SICODE),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ICODE = 0

        ALLOCATE (LSHSUB(SLSHSUB),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LSHSUB = 0

        ALLOCATE (ISKEW(SISKEW),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ISKEW = 0

        ALLOCATE (ISKWN(SISKWN),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ISKWN = 0

        ALLOCATE (IFRAME(SIFRAME),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IFRAME = 0

        ALLOCATE (NETH(SNETH),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NETH = 0

        ALLOCATE (IBCSLAG(SIBCSLAG),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IBCSLAG = 0

        ALLOCATE (IPART(SIPART),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IPART = 0

        ALLOCATE (NOM_OPT(SNOM_OPT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NOM_OPT = 0

        ALLOCATE (NPC(SNPC),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NPC = 0

        ALLOCATE (IXUR(SIXUR),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IXUR = 0  

        ALLOCATE (IXTG(SIXTG),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IXTG = 0  

        ALLOCATE (IXTG1(SIXTG1),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IXTG1 = 0 

        ALLOCATE (IXS(SIXS),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IXS = 0  

        ALLOCATE (IXQ(SIXQ),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IXQ = 0  

        ALLOCATE (IXC(SIXC),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IXC = 0  

        ALLOCATE (IXT(SIXT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IXT = 0  

        ALLOCATE (IXP(SIXP),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IXP = 0  

        ALLOCATE (IXR(SIXR),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IXR = 0  

        ALLOCATE (ITAB(SITAB),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ITAB = 0  

        ALLOCATE (ITABM1(SITABM1),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ITABM1 = 0

        ALLOCATE (GJBUFI(SGJBUFI),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        GJBUFI = 0

        ALLOCATE (ALE_CONNECTIVITY%NALE(SNALE),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ALE_CONNECTIVITY%NALE(1:SNALE) = 0  

        ALLOCATE (ID_GLOBAL_VOIS(SIELVS),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ID_GLOBAL_VOIS = 0 

        ALLOCATE (FACE_VOIS(SIELVS),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FACE_VOIS = 0 

        ALLOCATE (IFILL(SIFILL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IFILL = 0 

        ALLOCATE (IMS(SIMS),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IMS = 0  

        ALLOCATE (KXX(SKXX),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        KXX = 0 

        ALLOCATE (IXX(SIXX),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IXX = 0 

        ALLOCATE (KXSP(SKXSP),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        KXSP = 0

        ALLOCATE (IXSP(SIXSP),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IXSP = 0

        ALLOCATE (NOD2SP(SNOD2SP),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NOD2SP = 0 

        ALLOCATE (ISPSYM(SISPSYM),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ISPSYM = 0 

        ALLOCATE (ISPCOND(SISPCOND),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ISPCOND = 0

        ALLOCATE (ISPHIO(SISPHIO),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ISPHIO = 0 

        ALLOCATE (LPRTSPH(SLPRTSPH),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LPRTSPH = 0 

        ALLOCATE (LONFSPH(SLONFSPH),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LONFSPH = 0 

        ALLOCATE (IBCL(SIBCL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IBCL = 0 
        
        ALLOCATE (IBFV(SIBFV),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IBFV = 0 

        ALLOCATE (IECRAN(SIECRAN),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IECRAN = 0  

        ALLOCATE (ILAS(SILAS),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ILAS = 0  

        ALLOCATE (LACCELM(SLACCELM),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LACCELM = 0 

        ALLOCATE (LGAUGE(3*NBGAUGE),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LGAUGE = 0 

        ALLOCATE (LSENSOR(SLSENSOR),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LSENSOR = 0 

        ALLOCATE (NNLINK(SNNLINK),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NNLINK = 0  

        ALLOCATE (LNLINK(SLNLINK),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LNLINK = 0  

        ALLOCATE (IUSER(SIUSER),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IUSER = 0  

        ALLOCATE (IPARG(SIPARG),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IPARG = 0  

        ALLOCATE (DD_IAD(SDD_IAD),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        DD_IAD = 0 

        ALLOCATE (IGRV(SIGRV),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IGRV = 0  

        ALLOCATE (LGRAV(SLGRAV),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LGRAV = 0  

        ALLOCATE (IEXLNK(SIEXLNK),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IEXLNK = 0 

        ALLOCATE (IBVEL(SIBVEL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IBVEL = 0  

        ALLOCATE (LBVEL(SLBVEL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LBVEL = 0  

        ALLOCATE (IRBE2(SIRBE2),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IRBE2 = 0  

        ALLOCATE (LRBE2(SLRBE2),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LRBE2 = 0  

        ALLOCATE (IRBE3(SIRBE3),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IRBE3 = 0  

        ALLOCATE (LRBE3(SLRBE3),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LRBE3 = 0  

        ALLOCATE (IACTIV(SIACTIV),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IACTIV = 0 

        ALLOCATE (IBMPC(SIBMPC),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IBMPC = 0  

        ALLOCATE (KINET(SKINET),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        KINET = 0  

        ALLOCATE (FASOLFR(SFASOLFR),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FASOLFR = 0

        ALLOCATE (SEGQUADFR(SSEGQUADFR),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        SEGQUADFR = 0

        ALLOCATE (IPARI(SIPARI),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IPARI = 0  

c        ALLOCATE (INBUF(SINBUF),STAT=IERR)
c        ALLOCATE (INBUF(0),STAT=IERR)
c        IF (IERR/=0) GOTO 1000
c        INBUF = 0  

        ALLOCATE (INTBUF_TAB(NINTER),STAT=IERR)

C
C     allocations structures Interface friction
        ALLOCATE(INTBUF_FRIC_TAB(NINTERFRIC),STAT=IERR)
        IF (IERR/=0) GOTO 1000

c        ALLOCATE (T8(NINTER),STAT=IERR)
c        IF (IERR/=0) GOTO 1000


        ALLOCATE (NPRW(SNPRW),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NPRW = 0  

        ALLOCATE (LPRW(SLPRW),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LPRW = 0  

        ALLOCATE (ICONX(SICONX),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ICONX = 0 

        ALLOCATE (MADPRT(SMADPRT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        MADPRT = 0

        ALLOCATE (MADSH4(SMADSH4),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        MADSH4 = 0

        ALLOCATE (MADSH3(SMADSH3),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        MADSH3 = 0

        ALLOCATE (MADSOL(SMADSOL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        MADSOL = 0

        ALLOCATE (MADNOD(SMADNOD),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        MADNOD = 0

        ALLOCATE (MADCLNOD(MADCLNODS),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        MADCLNOD = 0

        ALLOCATE (MADFAIL(SMADFAIL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        MADFAIL = 0 
        ALLOCATE (MADIDX(SMADIDX),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        MADIDX = 0 


        ALLOCATE (NPBY(SNPBY),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NPBY = 0  

        ALLOCATE (NPBYL(SNPBYL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NPBYL = 0 

        ALLOCATE (LPBY(SLPBY),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LPBY = 0  

        ALLOCATE (LPBYL(SLPBYL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LPBYL = 0 

        ALLOCATE (LRIVET(SLRIVET),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LRIVET = 0

        ALLOCATE (NSTRF(SNSTRF),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NSTRF = 0 

        ALLOCATE (NOM_SECT(SNOM_SECT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NOM_SECT = 0

        ALLOCATE (LJOINT(SLJOINT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LJOINT = 0

        ALLOCATE (NODPOR(SNODPOR),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NODPOR = 0

        ALLOCATE (MONVOL(SMONVOL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        MONVOL = 0

        ALLOCATE (LAGBUF(SLAGBUF),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LAGBUF = 0

        ALLOCATE (ITHGRP(SITHGRP),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ITHGRP = 0

        ALLOCATE (ITHBUF(SITHBUF),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ITHBUF = 0

        ALLOCATE (RTHBUF(SRTHBUF),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        RTHBUF = ZERO

        ALLOCATE (ITHGRPA(SITHGRPA),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ITHGRPA = 0 

        ALLOCATE (ITHBUFA(SITHBUFA),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ITHBUFA = 0 

        ALLOCATE (ITHGRPB(SITHGRPB),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ITHGRPB = 0 

        ALLOCATE (ITHBUFB(SITHBUFB),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ITHBUFB = 0 

        ALLOCATE (ITHGRPC(SITHGRPC),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ITHGRPC = 0 

        ALLOCATE (ITHBUFC(SITHBUFC),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ITHBUFC = 0 

        ALLOCATE (ITHGRPD(SITHGRPD),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ITHGRPD = 0 

        ALLOCATE (ITHBUFD(SITHBUFD),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ITHBUFD = 0 

        ALLOCATE (ITHGRPE(SITHGRPE),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ITHGRPE = 0 

        ALLOCATE (ITHBUFE(SITHBUFE),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ITHBUFE = 0 

        ALLOCATE (ITHGRPF(SITHGRPF),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ITHGRPF = 0 

        ALLOCATE (ITHBUFF(SITHBUFF),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ITHBUFF = 0 

        ALLOCATE (ITHGRPG(SITHGRPG),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ITHGRPG = 0 

        ALLOCATE (ITHBUFG(SITHBUFG),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ITHBUFG = 0 

        ALLOCATE (ITHGRPH(SITHGRPH),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ITHGRPH = 0 

        ALLOCATE (ITHBUFH(SITHBUFH),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ITHBUFH = 0 

        ALLOCATE (ITHGRPI(SITHGRPI),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ITHGRPI = 0 

        ALLOCATE (ITHBUFI(SITHBUFI),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ITHBUFI = 0 

        ALLOCATE (IAD_ELEM(SIAD_ELEM),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IAD_ELEM = 0

        ALLOCATE (IAD_RBY(SIAD_RBY),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IAD_RBY = 0 

        ALLOCATE (IAD_RBY2(SIAD_RBY2),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IAD_RBY2 = 0

        ALLOCATE (IAD_I2M(SIAD_I2M),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IAD_I2M = 0 

        ALLOCATE (IADCJ(SIADCJ),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IADCJ = 0  

        ALLOCATE (IAD_RBM(SIAD_RBM),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IAD_RBM = 0 

        ALLOCATE (IAD_RBM2(SIAD_RBM2),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IAD_RBM2 = 0

        ALLOCATE (IAD_RBE2(SIAD_RBE2),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IAD_RBE2 = 0 

        ALLOCATE (IAD_RBE3M(SIAD_RBE3M),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IAD_RBE3M = 0 

        ALLOCATE (IAD_SEC(SIAD_SEC),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IAD_SEC = 0 

        ALLOCATE (IAD_CUT(SIAD_CUT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IAD_CUT = 0 
C
        SIAD_RBYM = SIAD_RBY
        SIAD_RBYM2 = SIAD_RBY2
        ALLOCATE (IAD_RBYM(SIAD_RBYM),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IAD_RBYM = 0   
C          
        ALLOCATE (IAD_RBYM2(SIAD_RBYM2),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IAD_RBYM2 = 0
C          
        ALLOCATE (ADDCSRECT(NUMNOR+1),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ADDCSRECT = 0
C          
        ALLOCATE (IAD_FRNOR((NSPMD+1)*NINTER25),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IAD_FRNOR = 0
C          
        ALLOCATE (FR_EDG(2*NBDDEDGT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_EDG = 0
C          
        ALLOCATE (IAD_FREDG((NSPMD+1)*NINTER25),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IAD_FREDG = 0
C          
        ALLOCATE (FR_NOR(NBDDNORT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_NOR = 0
C          
        ALLOCATE (PROCNOR(NBCCNOR),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        PROCNOR = 0

        ALLOCATE (FR_ELEM(SFR_ELEM),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_ELEM = 0 

        ALLOCATE (FR_RBY(SFR_RBY),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_RBY = 0  

        ALLOCATE (FR_WALL(SFR_WALL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_WALL = 0 

        ALLOCATE (FR_RBY2(SFR_RBY2),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_RBY2 = 0 

        ALLOCATE (FR_I2M(SFR_I2M),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_I2M = 0  

        ALLOCATE (FR_MV(SFR_MV),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_MV = 0  

        ALLOCATE (FR_LL(SFR_LL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_LL = 0  

        ALLOCATE (FR_CJ(SFR_CJ),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_CJ = 0  

        ALLOCATE (FR_RBM(SFR_RBM),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_RBM = 0 

        ALLOCATE (FR_RBM2(SFR_RBM2),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_RBM2 = 0

        ALLOCATE (FR_RBE2(SFR_RBE2),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_RBE2 = 0

        ALLOCATE (FR_RBE3M(SFR_RBE3M),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_RBE3M = 0

        ALLOCATE (FR_RBE3MP(SFR_RBE3M),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_RBE3MP = 0

        ALLOCATE (FR_SEC(SFR_SEC),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_SEC = 0 

        ALLOCATE (FR_CUT(SFR_CUT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_CUT = 0 

        ALLOCATE (RG_CUT(SRG_CUT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        RG_CUT = 0 

        ALLOCATE (FR_MAD(SFR_MAD),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_MAD = 0 

        ALLOCATE (FR_I18(SFR_I18),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_I18 = 0 

        ALLOCATE (DD_R2R(SDD_R2R),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        DD_R2R = 0
        
        ALLOCATE (DD_R2R_ELEM(SDD_R2R_ELEM),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        DD_R2R_ELEM = 0

       ALLOCATE (FR_RBYM(SFR_RBYM),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_RBYM = 0  

        ALLOCATE (FR_RBYM2(SFR_RBYM2),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_RBYM2 = 0 
        ALLOCATE (ISKWP(SISKWP),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ISKWP = 0

        ALLOCATE (NSKWP(SNSKWP),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NSKWP = 0

        ALLOCATE (ISKWP_L(SISKWP),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ISKWP_L = 0

        ALLOCATE (ISENSP(SISENSP),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ISENSP = 0

        ALLOCATE (NSENSP(SNSENSP),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NSENSP = 0

        ALLOCATE (IACCP(SIACCP),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IACCP = 0

        ALLOCATE (NACCP(SNACCP),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NACCP = 0

        ALLOCATE (IGAUP(SIGAUP),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IGAUP = 0

        ALLOCATE (NGAUP(SNGAUP),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NGAUP = 0

        ALLOCATE (FR_LAGF(SFR_LAGF),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_LAGF = 0

        ALLOCATE (MAIN_PROC(SWEIGHT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        MAIN_PROC = 0 

        ALLOCATE (WEIGHT(SWEIGHT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        WEIGHT = 0 

        ALLOCATE (WEIGHT_MD(SWEIGHT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        WEIGHT_MD = 0 

        ALLOCATE (NEWFRONT(SNEWFRONT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NEWFRONT = 0

        ALLOCATE (NODGLOB(SNODGLOB),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NODGLOB = 0 

        ALLOCATE (NBRCVOIS(SNBRCVOIS),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NBRCVOIS = 0

        ALLOCATE (LNRCVOIS(SLNRCVOIS),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LNRCVOIS = 0

        ALLOCATE (NBSDVOIS(SNBSDVOIS),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NBSDVOIS = 0

        ALLOCATE (LNSDVOIS(SLNSDVOIS),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LNSDVOIS = 0

        ALLOCATE (NERCVOIS(SNERCVOIS),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NERCVOIS = 0

        ALLOCATE (LERCVOIS(SLERCVOIS),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LERCVOIS = 0

        ALLOCATE (NESDVOIS(SNESDVOIS),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NESDVOIS = 0

        ALLOCATE (LESDVOIS(SLESDVOIS),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LESDVOIS = 0
        
        IF(ALE_CONNECTIVITY%has_ne_connect)THEN          
          ALLOCATE (NE_NERCVOIS(NSPMD + 1),STAT=IERR)  
          IF (IERR/=0) GOTO 1000                     
          NE_NERCVOIS = 0                              

          ALLOCATE (NE_NESDVOIS(NSPMD + 1),STAT=IERR)  
          IF (IERR/=0) GOTO 1000                     
          NE_NESDVOIS = 0                              
        ENDIF                                          
        
        ALLOCATE (NPSEGCOM(SNPSEGCOM),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NPSEGCOM = 0

        ALLOCATE (LSEGCOM(SLSEGCOM),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LSEGCOM = 0 

        ALLOCATE (NPORGEO(SNPORGEO),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NPORGEO = 0 

        ALLOCATE (LNODPOR(SLNODPOR),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LNODPOR = 0 

        ALLOCATE (LLAGF(SLLAGF),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LLAGF = 0 

        ALLOCATE (ICODT(SICODT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ICODT = 0 

        ALLOCATE (ICODR(SICODR),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ICODR = 0 

        ALLOCATE (ISKY(SISKY),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ISKY = 0  

        ALLOCATE (ADSKY(SADSKY),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ADSKY = 0 

        ALLOCATE (PROCNE(SPROCNE),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        PROCNE = 0

        ALLOCATE (ADDCNI2(SADDCNI2),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ADDCNI2 = 0 

        ALLOCATE (PROCNI2(SPROCNI2),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        PROCNI2 = 0 

        ALLOCATE (IADSDP(SIADSDP),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IADSDP = 0  

        ALLOCATE (IADRCP(SIADRCP),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IADRCP = 0  

        ALLOCATE (IADS(SIADS),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IADS = 0 

        ALLOCATE (IADWAL(SIADWAL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IADWAL = 0

        ALLOCATE (IADRBK(SIADRBK),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IADRBK = 0

        ALLOCATE (IADI2(SIADI2),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IADI2 = 0 

        ALLOCATE (IADMV2(SIADMV2),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IADMV2 = 0

        ALLOCATE (IADMV3(SIADMV3),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IADMV3 = 0

        ALLOCATE (IADLL(SIADLL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IADLL = 0 

        ALLOCATE (IADRBM(SIADRBM),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IADRBM = 0

        ALLOCATE (IADI18(SIADI18),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IADI18 = 0
        ALLOCATE (IADRBMK(SIADRBMK),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IADRBMK = 0
        ALLOCATE (ILINK(SILINK),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ILINK = 0 

        ALLOCATE (FR_RL(SFR_RL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_RL = 0 

        ALLOCATE (LLINK(SLLINK),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LLINK = 0 

        ALLOCATE (IADRL(SIADRL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IADRL = 0 

        ALLOCATE (LINALE(SLINALE+LLINAL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LINALE = 0

        ALLOCATE (NEFLSW(SNEFLSW),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NEFLSW = 0

        ALLOCATE (NNFLSW(SNNFLSW),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        NNFLSW = 0

        ALLOCATE (ICUT(SICUT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ICUT = 0  

        ALLOCATE (IGEO(SIGEO),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IGEO = 0 

        ALLOCATE (IPM(SIPM),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IPM = 0  

        ALLOCATE (IPART_STATE(NPART),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IPART_STATE = 0 

        ALLOCATE (IPART_DYNAIN(NPART),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IPART_DYNAIN = 0 

        ALLOCATE (ICONTACT(SICONTACT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ICONTACT = 0  

        IF(NADMESH /= 0)THEN
          ALLOCATE (SH4TREE(KSH4TREE,NUMELC),STAT=IERR)
          IERROR = IERROR+IERR
          ALLOCATE (SH3TREE(KSH3TREE,NUMELTG),STAT=IERR)
          IERROR = IERROR+IERR
          ALLOCATE (IPADMESH(KIPADMESH,NPART),STAT=IERR)
          IERROR = IERROR+IERR
          ALLOCATE (SH4TRIM(ABS(LSH4TRIM)),STAT=IERR)
          IERROR = IERROR+IERR
          ALLOCATE (SH3TRIM(ABS(LSH3TRIM)),STAT=IERR)
          IERROR = IERROR+IERR
        ELSE
          ALLOCATE(SH4TREE(0,0))
          ALLOCATE(SH3TREE(0,0))
          ALLOCATE(IPADMESH(0,0))
          ALLOCATE(SH4TRIM(0))
          ALLOCATE(SH3TRIM(0))
        END IF
C
        ALLOCATE (ITHVAR(SITHVAR),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ITHVAR = 0      

        ALLOCATE (ICFIELD(SIZFIELD*NLOADC),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ICFIELD = 0  

        ALLOCATE (LCFIELD(SLCFIELD),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LCFIELD = 0
  
        IF(IUSERWI /= 0)THEN
          ALLOCATE(IADUSERW(NSPMD+1))
          ALLOCATE(FR_USERWI(SFR_USERWI))
        ENDIf

        ALLOCATE (ILOADP(SIZLOADP*NLOADP),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ILOADP = 0  

        ALLOCATE (LLOADP(SLLOADP),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LLOADP = 0
  
        ALLOCATE(IBUFSSG_IO(SIBUFSSG_IO),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IBUFSSG_IO = 0

        IF(NSPHSOL/=0)THEN
  
          ALLOCATE(SPH2SOL(NUMSPH),STAT=IERR)
          IF (IERR/=0) GOTO 1000
          SPH2SOL = 0
  
          ALLOCATE(SOL2SPH(2*NUMELS8),STAT=IERR)
          IF (IERR/=0) GOTO 1000
          SOL2SPH = 0
  
          ALLOCATE(IRST(3*NSPHSOL),STAT=IERR)
          IF (IERR/=0) GOTO 1000
          IRST = 0

          ALLOCATE(SOL2SPH_TYP(NUMELS8),STAT=IERR)
          IF (IERR/=0) GOTO 1000
          SOL2SPH_TYP = 0

        ELSE

          ALLOCATE(SPH2SOL(0),STAT=IERR)
          IF (IERR/=0) GOTO 1000
          ALLOCATE(SOL2SPH(0),STAT=IERR)
          IF (IERR/=0) GOTO 1000
          ALLOCATE(IRST(0),STAT=IERR)
          IF (IERR/=0) GOTO 1000
          ALLOCATE(SOL2SPH_TYP(0),STAT=IERR)
          IF (IERR/=0) GOTO 1000

        END IF

        ALLOCATE (FSKYD(SFSKYD),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FSKYD = ZERO 
        
        ALLOCATE (IBCSCYC(SIBCSCYC),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IBCSCYC = 0  

        ALLOCATE (LBCSCYC(SLBCSCYC),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LBCSCYC = 0  
C   -----------------------------------------------
C             allocations flottantes
C   -----------------------------------------------

        ALLOCATE (X(SX),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        X = 0 

        ALLOCATE (D(SD),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        D = 0 

        ALLOCATE (V(SV),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        V = 0 

        ALLOCATE (VR(SVR),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        VR = 0

        ALLOCATE (DR(SDR),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        DR = 0

        ALLOCATE (THKE(STHKE),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        THKE = 0 

        ALLOCATE (DAMPR(SDAMPR),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        DAMPR = 0

        ALLOCATE (DAMP(SDAMP),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        DAMP = 0 

        ALLOCATE (MS(SMS),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        MS = 0 

        ALLOCATE (IN(SIN),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IN = 0

        ALLOCATE (TF(STF),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        TF = 0

        ALLOCATE (PM(SPM),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        PM = 0

        ALLOCATE (SKEW(SSKEW),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        SKEW = 0  

        ALLOCATE (XFRAME(SXFRAME),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        XFRAME = 0

        ALLOCATE (GEO(SGEO),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        GEO = 0  

        ALLOCATE (EANI(SEANI),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        EANI = 0  

        ALLOCATE (BUFMAT(SBUFMAT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        BUFMAT = 0

        ALLOCATE (BUFGEO(SBUFGEO),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        BUFGEO = 0

        ALLOCATE (BUFSF(SBUFSF),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        BUFSF = 0 

        ALLOCATE (RBMPC(SRBMPC),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        RBMPC = 0 

        ALLOCATE (GJBUFR(SGJBUFR),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        GJBUFR = 0

        ALLOCATE (W(SW),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        W = 0 

        ALLOCATE (VEUL(SVEUL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        VEUL = 0  

        ALLOCATE (FILL(SFILL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FILL = 0  

        ALLOCATE (DFILL(SDFILL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        DFILL = 0 

        ALLOCATE (ALPH(SALPH),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ALPH = 0  

        ALLOCATE (WB(SWB),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        WB = 0  

        ALLOCATE (DSAVE(SDSAVE),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        DSAVE = 0 

        ALLOCATE (DRSAVE(SDRSAVE),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        DRSAVE = 0

        ALLOCATE (ASAVE(SASAVE),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ASAVE = 0 

        ALLOCATE (ARSAVE(SARSAVE),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ARSAVE = 0

        ALLOCATE (STSAVE(SSTSAVE),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        STSAVE = 0

        ALLOCATE (STRSAVE(SSTRSAVE),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        STRSAVE = 0 

        ALLOCATE (FSAVSAV(SFSAVSAV),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FSAVSAV = 0 

        ALLOCATE (SPBUF(SSPBUF),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        SPBUF = 0   

        ALLOCATE (VSPHIO(SVSPHIO),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        VSPHIO = 0  

        ALLOCATE (SPHVELN(SSPHVELN),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        SPHVELN = 0 

        ALLOCATE (MSNF(SMSNF),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        MSNF = 0  
C        

        ALLOCATE (FORC(SFORC),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FORC = 0 

        ALLOCATE (VEL(SVEL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        VEL = 0  

        ALLOCATE (FSAV(SFSAV),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FSAV = 0 

        ALLOCATE (FBSAV6(12,6,SFSAV6),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FBSAV6 = 0

        ALLOCATE (TABSENSOR(STABSEN),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        TABSENSOR = 0

        ALLOCATE (TAB_UMP(7,TAILLE),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        TAB_UMP = 0

        ALLOCATE (POIN_UMP(NUMMAT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        POIN_UMP = 0

        IF(ISPMD==0) THEN
          ALLOCATE (CPUTIME_MP_GLOB(NBR_GPMP,NSPMD+1),STAT=IERR)
          IF (IERR/=0) GOTO 1000
          CPUTIME_MP_GLOB = 0
        ENDIF

        ALLOCATE (CPUTIME_MP(TAILLE),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        CPUTIME_MP = 0 
        
        ALLOCATE (TAB_MAT(STAB_MAT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        TAB_MAT = ZERO

        ALLOCATE (FZERO(SFZERO),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FZERO = 0

        ALLOCATE (XLAS(SXLAS),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        XLAS = 0 

        ALLOCATE (ACCELM(SACCELM),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ACCELM = 0 

        ALLOCATE (GAUGE(LLGAUGE*NBGAUGE),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        GAUGE = 0 

        ALLOCATE (SENSOR(SSENSOR),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        SENSOR = 0 

        ALLOCATE (FBVEL(SFBVEL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FBVEL = 0  

        ALLOCATE (FRBE3(SFRBE3),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FRBE3 = 0  

        ALLOCATE (FACTIV(LRACTIV*NACTIV),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FACTIV = 0 

        ALLOCATE (GRAV(SGRAV),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        GRAV = 0  

        ALLOCATE (FR_WAVE(SFR_WAVE),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_WAVE = 0

        ALLOCATE (PARTS0(SPARTS0),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        PARTS0 = 0  

        ALLOCATE (USER(SUSER),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        USER = 0    

        ALLOCATE (ELBUF(SELBUF),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ELBUF = 0   

        ALLOCATE (RWBUF(SRWBUF),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        RWBUF = 0 

        ALLOCATE (RWSAV(SRWSAV),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        RWSAV = 0 

        ALLOCATE (RBY(SRBY),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        RBY = 0  

        ALLOCATE (RBYL(SRBYL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        RBYL = 0 

        ALLOCATE (RIVET(SRIVET),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        RIVET = 0

        ALLOCATE (SECBUF(SSECBUF),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        SECBUF = 0

        ALLOCATE (VOLMON(SVOLMON),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        VOLMON = 0

        ALLOCATE (LAMBDA(SLAMBDA),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LAMBDA = 0

        ALLOCATE (RCONX(SRCONX),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        RCONX = 0 

        ALLOCATE (RCONTACT(SRCONTACT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        RCONTACT = EP30

        ALLOCATE (ACONTACT(SRCONTACT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ACONTACT = EP30

        ALLOCATE (PCONTACT(SRCONTACT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        PCONTACT = ZERO

        ALLOCATE (WA(SWA),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        WA = 0 

        ALLOCATE (FV(SFV),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FV = 0 

        ALLOCATE (A(SA),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        A = 0  

        ALLOCATE (AR(SAR),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        AR = 0 

        ALLOCATE (STIFN(SSTIFN),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        STIFN = 0 

        ALLOCATE (VISCN(SVISCN),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        VISCN = 0 

        ALLOCATE (STIFR(SSTIFR),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        STIFR = 0 

        ALLOCATE (PARTSAV(SPARTSAV),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        PARTSAV = 0

        ALLOCATE (FSKY(SFSKY),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FSKY = 0 


        ALLOCATE (FSKYM(SFSKYM),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FSKYM = 0 

        ALLOCATE (FSKYI(LSKYI,NFSKYI),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FSKYI = 0 

        ALLOCATE (UWA(SUWA),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        UWA = 0 

        ALLOCATE (VAL2(SVAL2),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        VAL2 = 0

        ALLOCATE (PHI(SPHI),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        PHI = 0 
C        
        ALLOCATE (PHIE(SPHI),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        PHIE = ZERO
         ALLOCATE (POR(SPHI),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        POR = ZERO

        ALLOCATE (R(SR),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        R = 0

        ALLOCATE (ESTIF(SESTIF),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ESTIF = 0  

        ALLOCATE (CRFLSW(SCRFLSW),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        CRFLSW = 0 

        ALLOCATE (FLSW(SFLSW),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FLSW = 0  

        ALLOCATE (FANI(SFANI),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FANI = 0  

        ALLOCATE (FANI_CELL(SFANI_CELL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FANI_CELL = 0 
        
        ALLOCATE (XCUT(SXCUT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        XCUT = 0  

        ALLOCATE (ANIN(SANIN),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ANIN = 0  

        ALLOCATE (TANI(STANI),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        TANI = 0

        ALLOCATE (SECFCUM(SSECFCUM),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        SECFCUM = 0

        ALLOCATE (WASPH(SWASPH),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        WASPH = 0  

        ALLOCATE (W16(SW16),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        W16 = 0  

        ALLOCATE (DRETRIO(SDRETRIO),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        DRETRIO = 0

        ALLOCATE (LBVRS(SLBVRS),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LBVRS = 0

        ALLOCATE (PV(SPV),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        PV = 0 

        IF(NADMESH /= 0 .OR. IREST_MSELT/=0)THEN

          ALLOCATE(MSC(NUMELC),STAT=IERR)
          IERROR = IERROR+IERR

          ALLOCATE(INC(NUMELC),STAT=IERR)
          IERROR = IERROR+IERR

          ALLOCATE(MSTG(NUMELTG),STAT=IERR)
          IERROR = IERROR+IERR

          ALLOCATE(INTG(NUMELTG),STAT=IERR)
          IERROR = IERROR+IERR

          ALLOCATE(PTG(3,NUMELTG),STAT=IERR)
          IERROR = IERROR+IERR

        ELSE

          ALLOCATE(MSC(0))
          ALLOCATE(INC(0))
          ALLOCATE(MSTG(0))
          ALLOCATE(INTG(0))
          ALLOCATE(PTG(0,0))

        END IF

        IF(IREST_MSELT/=0)THEN

          ALLOCATE(MSSA(NUMELS),STAT=IERR)
          IERROR = IERROR+IERR

          ALLOCATE(MSTR(NUMELT),STAT=IERR)
          IERROR = IERROR+IERR

          ALLOCATE(MSP(NUMELP),STAT=IERR)
          IERROR = IERROR+IERR

          ALLOCATE(MSRT(NUMELR),STAT=IERR)
          IERROR = IERROR+IERR

        ELSE

          ALLOCATE(MSSA(0))
          ALLOCATE(MSTR(0))
          ALLOCATE(MSP(0))
          ALLOCATE(MSRT(0))

        END IF

        IF(NADMESH /= 0)THEN

          ALLOCATE(PADMESH(KPADMESH,NPART),STAT=IERR)
          IERROR = IERROR+IERR

          IF(ITHERM_FE > 0)THEN

            ALLOCATE(MCPC(NUMELC),STAT=IERR)
            IERROR = IERROR+IERR

            ALLOCATE(MCPTG(NUMELTG),STAT=IERR)
            IERROR = IERROR+IERR

          ELSE

            ALLOCATE(MCPC(0),MCPTG(0))

          END IF

        ELSE
          ALLOCATE(PADMESH(0,0))
          ALLOCATE(MCPC(0),MCPTG(0))
        END IF

        IF(ISTATCND/=0)THEN
         ALLOCATE(MSCND(NUMNOD),INCND(NUMNOD),STAT=IERR)
         IERROR = IERROR+IERR
        ELSE
         ALLOCATE(MSCND(0),INCND(0))
        END IF
C--------------------------------------------
C     rigid material
C
         IF(IRIGID_MAT > 0) THEN   
           ALLOCATE(RBYM(NFRBYM*NRBYM),STAT=IERR)
           IF (IERR/=0) GOTO 1000
            RBYM  = ZERO
           ALLOCATE(IRBYM(NIRBYM*NRBYM),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           IRBYM  = 0
           ALLOCATE(LNRBYM(NGSLNRBYM),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           LNRBYM  = 0 
           ALLOCATE(WEIGHT_RM(NRBYM),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           WEIGHT_RM  = 0
           ALLOCATE(ICODRBYM(NRBYM*2),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           ICODRBYM = 0
         ELSE
            ALLOCATE(RBYM(0),IRBYM(0), LNRBYM(0),ICODRBYM(0))
            ALLOCATE(WEIGHT_RM(0))
         ENDIF
C--------------------------------------------
C   thermal bc   
         IF(NUMCONV > 0) THEN                                          
          ALLOCATE(IBCV(NICONV*NUMCONV),STAT=IERR)
           IF (IERR/=0) GOTO 1000
            IBCV = 0  
          ALLOCATE(FCONV(LFACTHER*NUMCONV),STAT=IERR)
           IF (IERR/=0) GOTO 1000
            FCONV = ZERO 
         ELSE                                                          
           ALLOCATE(IBCV(0),FCONV(0))                                 
         ENDIF 
                 
         IF(NUMRADIA > 0) THEN                                          
          ALLOCATE(IBCR(NIRADIA*NUMRADIA),STAT=IERR)
           IF (IERR/=0) GOTO 1000
            IBCR = 0  
          ALLOCATE(FRADIA(LFACTHER*NUMRADIA),STAT=IERR)
           IF (IERR/=0) GOTO 1000
            FRADIA = ZERO 
         ELSE                                                          
           ALLOCATE(IBCR(0),FRADIA(0))                                 
         ENDIF 
                 
         IF(NFXFLUX > 0) THEN                                          
          ALLOCATE(IBFFLUX(NITFLUX*NFXFLUX),STAT=IERR)
           IF (IERR/=0) GOTO 1000
            IBFFLUX = 0  
          ALLOCATE(FBFFLUX(LFACTHER*NFXFLUX),STAT=IERR)
           IF (IERR/=0) GOTO 1000
            FBFFLUX = ZERO 
         ELSE                                                          
           ALLOCATE(IBFFLUX(0),FBFFLUX(0))                                 
         ENDIF 
                 
         IF(NFXTEMP > 0) THEN                                        
           ALLOCATE(IBFTEMP(NIFT*NFXTEMP),STAT=IERR)
           IF (IERR/=0) GOTO 1000
            IBFTEMP = 0  
           ALLOCATE(FBFTEMP(LFACTHER*NFXTEMP),STAT=IERR)
           IF(IERR/=0) GOTO 1000
            FBFTEMP = ZERO  
         ELSE                                                          
            ALLOCATE(IBFTEMP(0),FBFTEMP(0))                              
         ENDIF  
C

         IF(IALELAG > 0) THEN
           ALLOCATE (MSF(SMS),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           MSF = ZERO
         ENDIF
C--------------------------------------------
C ply xfem for composite shell
         IF(IPLYXFEM > 0   ) THEN
           ALLOCATE(MS_PLY(NPLYXFE*NPLYMAX),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           MS_PLY = ZERO 
           ALLOCATE(ZI_PLY(NPLYXFE*NPLYMAX),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           ZI_PLY = ZERO   
           ALLOCATE(MSZ2(NPLYXFE),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           MSZ2 = ZERO 
           ALLOCATE(INOD_PXFEM(NUMNOD),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           INOD_PXFEM = 0 
           ALLOCATE(IEL_PXFEM(NUMELC),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           IEL_PXFEM = 0 
           ALLOCATE(IADC_PXFEM(4*EPLYXFE),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           IADC_PXFEM = 0
           ALLOCATE(ADSKY_PXFEM(NPLYXFE+1),STAT=IERR)
           ADSKY_PXFEM = 0
C
           ALLOCATE (PROCNE_PXFEM(SPROCNE_PXFE),STAT=IERR)
           IF (IERR/=0) GOTO 1000
            PROCNE_PXFEM = 0  
           ALLOCATE (IADSDP_PXFEM(SIADSDP),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           IADSDP_PXFEM = 0  
C
           ALLOCATE (IADRCP_PXFEM(SIADRCP),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           IADRCP_PXFEM = 0                     
         ELSE
           ALLOCATE(MS_PLY(0),ZI_PLY(0),IADC_PXFEM(0),ADSKY_PXFEM(0))
           ALLOCATE(MSZ2(0)) 
           ALLOCATE(INOD_PXFEM(0),IEL_PXFEM(0)) 
           ALLOCATE(PROCNE_PXFEM(0),IADSDP_PXFEM(0),IADRCP_PXFEM(0))
         ENDIF  
         IF(IPLYBCS > 0) THEN
           ALLOCATE(ICODE_PLY(NUMNOD),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           ICODE_PLY = 0
           ALLOCATE(ICODT_PLY(NUMNOD),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           ICODT_PLY = 0
           ALLOCATE(ISKEW_PLY(NUMNOD),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           ISKEW_PLY = 0  
           ALLOCATE(IBC_PLY(NUMNOD),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           IBC_PLY = 0 
         ELSE
           ALLOCATE(ICODE_PLY(0),ICODT_PLY(0),ISKEW_PLY(0),IBC_PLY(0))
         ENDIF 
         IF(INTPLYXFEM > 0   ) THEN
           ALLOCATE(WAGAP(2*NUMNOD),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           WAGAP = ZERO
         ELSE
           ALLOCATE(WAGAP(0),STAT=IERR)
         ENDIF   
C--------------------------------------------
         ALLOCATE(MS0(NUMNOD),STAT=IERR)
         ALLOCATE(IN0(SIN),STAT=IERR)
         IF(IDTMINS_OLD==1) THEN
           ALLOCATE(ADMSMS(NUMNOD),STAT=IERR)
           IF (IERR/=0) GOTO 1000
         END IF
         IF(IDTMINS_OLD==2) THEN
           ALLOCATE(DMELC (NUMELC) ,STAT=IERR)
           IF (IERR/=0) GOTO 1000
           ALLOCATE(DMELTG(NUMELTG),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           ALLOCATE(DMELS (NUMELS) ,STAT=IERR)
           IF (IERR/=0) GOTO 1000
           ALLOCATE(DMELTR(NUMELT) ,STAT=IERR)
           IF (IERR/=0) GOTO 1000
           ALLOCATE(DMELP (NUMELP) ,STAT=IERR)
           IF (IERR/=0) GOTO 1000
           ALLOCATE(DMELRT(NUMELR) ,STAT=IERR)
           IF (IERR/=0) GOTO 1000
         END IF
         IF(IDTMINS_OLD/=0.OR.IDTMINS_INT_OLD/=0) THEN
           ALLOCATE(RES_SMS(3*NUMNOD),STAT=IERR)
           IF (IERR/=0) GOTO 1000
         END IF
         IF(IDTMINS_OLD==2.OR.IDTMINS_INT_OLD/=0) THEN
           ALLOCATE(DIAG_SMS(NUMNOD),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           ALLOCATE(DMINT2(4*I2NSN25),STAT=IERR)
           IF (IERR/=0) GOTO 1000
         END IF
         IF (ISMS_SELEC /= 0) THEN
           ALLOCATE(NATIV0_SMS(NUMNOD),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           NATIV0_SMS = 0
         ENDIF
C--------------------------------------------
C
         IF(NINTSTAMP/=0.AND.ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT >0)THEN
           ALLOCATE(FCONTG(3,NUMNODG),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           FCONTG=ZERO
         ELSE
           ALLOCATE(FCONTG(0,0),STAT=IERR)
         END IF
C
         SFCONT_MAX = 0
         IF(ANIM_V(26)+H3D_DATA%N_VECT_CONT_MAX >0.OR.IFCONTMAX > 0)THEN
           IF(NSPMD > 1 .AND. NINTSTAMP/=0) THEN
              IF(ISPMD == 0) THEN
                 ALLOCATE(FCONT_MAX(3*NUMNODG),STAT=IERR)
                 IF (IERR/=0) GOTO 1000
                 FCONT_MAX=ZERO
                 SFCONT_MAX = NUMNODG
              ELSE
                 ALLOCATE(FCONT_MAX(0),STAT=IERR)
               ENDIF
            ELSE
               ALLOCATE(FCONT_MAX(3*NUMNOD),STAT=IERR)
               IF (IERR/=0) GOTO 1000
               FCONT_MAX=ZERO
               SFCONT_MAX = NUMNOD
            ENDIF
         ELSE
            ALLOCATE(FCONT_MAX(0),STAT=IERR)
         END IF

C 
         IF(NINTSTAMP/=0.AND.ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT>0)THEN
           ALLOCATE(FNCONTG(3,NUMNODG),FTCONTG(3,NUMNODG),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           FNCONTG=ZERO
           FTCONTG=ZERO
         ELSE
           ALLOCATE(FNCONTG(0,0),FTCONTG(0,0),STAT=IERR)
         END IF
C
         SPCONT_MAX = 0
         IF(H3D_DATA%N_VECT_PCONT_MAX >0.OR.IFCONTPMAX > 0)THEN
           IF(NSPMD > 1 .AND. NINTSTAMP/=0) THEN
              IF(ISPMD == 0) THEN
                 ALLOCATE(FTCONT_MAX(3*NUMNODG),FNCONT_MAX(3*NUMNODG),STAT=IERR)
                 IF (IERR/=0) GOTO 1000
                 FTCONT_MAX=ZERO
                 FNCONT_MAX=ZERO
                 SPCONT_MAX = NUMNODG
              ELSE
                 ALLOCATE(FNCONT_MAX(0),FTCONT_MAX(0),STAT=IERR)
               ENDIF
            ELSE
               ALLOCATE(FTCONT_MAX(3*NUMNOD),FNCONT_MAX(3*NUMNOD),STAT=IERR)
               IF (IERR/=0) GOTO 1000
               FTCONT_MAX=ZERO
               FNCONT_MAX=ZERO
               SPCONT_MAX = NUMNOD
            ENDIF
         ELSE
            ALLOCATE(FNCONT_MAX(0),STAT=IERR)
            ALLOCATE(FTCONT_MAX(0),STAT=IERR)
         END IF
C
         IF(H3D_DATA%N_VECT_CONT2_MAX >0.OR.IFCONT2MAX > 0)THEN
            ALLOCATE(FCONT2_MAX(3*NUMNOD),STAT=IERR)
            IF (IERR/=0) GOTO 1000
            FCONT2_MAX=ZERO
         ELSE
            ALLOCATE(FCONT2_MAX(0),STAT=IERR)
         END IF
C
         IF(H3D_DATA%N_VECT_PCONT2_MAX >0.OR.IFCONTP2MAX > 0)THEN
            ALLOCATE(FTCONT2_MAX(3*NUMNOD),FNCONT2_MAX(3*NUMNOD),STAT=IERR)
            IF (IERR/=0) GOTO 1000
            FNCONT2_MAX=ZERO
            FTCONT2_MAX=ZERO
         ELSE
            ALLOCATE(FNCONT2_MAX(0),STAT=IERR)
            ALLOCATE(FTCONT2_MAX(0),STAT=IERR)
         END IF
C
         IF(H3D_DATA%N_VECT_CONT2_MIN >0.OR.IFCONT2MIN > 0)THEN
            ALLOCATE(FCONT2_MIN(3*NUMNOD),STAT=IERR)
            IF (IERR/=0) GOTO 1000
            FCONT2_MIN=ZERO
         ELSE
            ALLOCATE(FCONT2_MIN(0),STAT=IERR)
         END IF
C
         IF(H3D_DATA%N_VECT_PCONT2_MIN >0.OR.IFCONTP2MIN > 0)THEN
            ALLOCATE(FTCONT2_MIN(3*NUMNOD),FNCONT2_MIN(3*NUMNOD),STAT=IERR)
            IF (IERR/=0) GOTO 1000
            FNCONT2_MIN=ZERO
            FTCONT2_MIN=ZERO
         ELSE
            ALLOCATE(FNCONT2_MIN(0),STAT=IERR)
            ALLOCATE(FTCONT2_MIN(0),STAT=IERR)
         END IF
C 
C
         IF(NINTSTAMP/=0.AND.(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT>0.OR.
     .          ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT >0))THEN
           ALLOCATE(INDEXCONT(NUMNODG),TAGCONT(NUMNODG),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           INDEXCONT=0
           TAGCONT=0
         ELSE
           ALLOCATE(INDEXCONT(0),TAGCONT(0),STAT=IERR)
         ENDIF
c
         IF(NINTSKIDOLD >0)THEN
           IF(NINTSTAMP/=0) THEN
              ALLOCATE(PSKIDS(NINTSKIDOLD,NUMNODG),STAT=IERR)
              IF (IERR/=0) GOTO 1000
           ELSE
              ALLOCATE(PSKIDS(NINTSKIDOLD,NUMNOD),STAT=IERR)
              IF (IERR/=0) GOTO 1000
           ENDIF
         END IF
C
        ALLOCATE (CFIELD(LFACLOAD*NLOADC),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        CFIELD = 0  

        ALLOCATE (LOADP(LFACLOAD*NLOADP),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        LOADP = 0  

        IF (NINTLOADP>0) THEN
 
           ALLOCATE (KLOADPINTER(NINTER+1),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           KLOADPINTER = 0  

           ALLOCATE (LOADPINTER(NINTER*NLOADP_HYD),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           LOADPINTER = 0  

           ALLOCATE (DGAPLOADINT(NINTER*NLOADP_HYD),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           DGAPLOADINT = ZERO 
        ELSE
           ALLOCATE (KLOADPINTER(0),STAT=IERR)
           ALLOCATE (LOADPINTER(0),STAT=IERR) 
           ALLOCATE (DGAPLOADINT(0),STAT=IERR) 
        ENDIF
C
C       allocations structure CLUSTER
        IF (NCLUSTER > 0) THEN
          ALLOCATE(CLUSTER(NCLUSTER),STAT=IERR)
          IF (IERR /= 0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
        ELSE
          ALLOCATE(CLUSTER(0))
        ENDIF
C--------------------------------------------
C xfem for layered shell (cracks)
         IF(ICRACK3D > 0) THEN
           ALLOCATE(INOD_CRKXFEM(NUMNOD),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           INOD_CRKXFEM = 0
           ALLOCATE(IEL_CRKXFEM(NUMELC+NUMELTG),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           IEL_CRKXFEM = 0
           ALLOCATE(IADC_CRKXFEM(4*ECRKXFEC+3*ECRKXFETG),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           IADC_CRKXFEM = 0
           ALLOCATE(ADSKY_CRKXFEM(NCRKXFE+1),STAT=IERR)
           ADSKY_CRKXFEM = 0
           ALLOCATE (CNE_CRKXFEM(SPROCNE_CRKXFE),STAT=IERR)
           IF (IERR/=0) GOTO 1000
            CNE_CRKXFEM = 0
           ALLOCATE (PROCNE_CRKXFEM(SPROCNE_CRKXFE),STAT=IERR)
           IF (IERR/=0) GOTO 1000
            PROCNE_CRKXFEM = 0
           ALLOCATE (IADSDP_CRKXFEM(SIADSDP),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           IADSDP_CRKXFEM = 0
           ALLOCATE (IADRCP_CRKXFEM(SIADRCP),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           IADRCP_CRKXFEM = 0
           ALLOCATE (CRKNODIAD(SPROCNE_CRKXFE),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           CRKNODIAD = 0
         ELSE
           ALLOCATE(IADC_CRKXFEM(0),ADSKY_CRKXFEM(0))
           ALLOCATE(INOD_CRKXFEM(0),IEL_CRKXFEM(0))
           ALLOCATE(PROCNE_CRKXFEM(0),IADSDP_CRKXFEM(0),
     .              IADRCP_CRKXFEM(0),CRKNODIAD(0))
         ENDIF
C
         IF(ICRACK3D > 0)THEN
           ALLOCATE(IBORDNODE(SIBORDNODE),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           IBORDNODE = 0
C
           ALLOCATE(IEDGESH(SIEDGESH),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           IEDGESH = 0
C
           ALLOCATE (NODEDGE(2*NUMEDGES),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           NODEDGE = 0
C
           ALLOCATE (IEDGE(NUMEDGES),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           IEDGE = 0
C
           ALLOCATE (IAD_EDGE(SIAD_EDGE),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           IAD_EDGE = 0
C
           ALLOCATE (FR_EDGE(SFR_EDGE),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           FR_EDGE = 0
C
           ALLOCATE (FR_NBEDGE(SFR_NBEDGE),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           FR_NBEDGE = 0
C
           ALLOCATE (NODLEVXF(NCRKXFE),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           NODLEVXF = 0
C
           ALLOCATE (NODGLOBXFE(SNODGLOBXFE),STAT=IERR)
           IF (IERR/=0) GOTO 1000
           NODGLOBXFE = 0
         ELSE
           ALLOCATE(IBORDNODE(0))
           ALLOCATE(IEDGESH(0))
           ALLOCATE(NODEDGE(0))
           ALLOCATE(IEDGE(0))
           ALLOCATE(IAD_EDGE(0))
           ALLOCATE(FR_EDGE(0))
           ALLOCATE(FR_NBEDGE(0))
           ALLOCATE(NODLEVXF(0))
           ALLOCATE(NODGLOBXFE(0))
         END IF

        ALLOCATE (KXIG3D(SKXIG3D),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        KXIG3D = 0 

        ALLOCATE (IXIG3D(SIXIG3D),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IXIG3D = 0 

        ALLOCATE (KNOT(SKNOT),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        KNOT = ZERO

        ALLOCATE (WIGE(NUMNOD),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        WIGE = ZERO

        ALLOCATE (KNOTLOCPC(SKNOTLOCPC),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        KNOTLOCPC = ZERO

        ALLOCATE (KNOTLOCEL(SKNOTLOCEL),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        KNOTLOCEL = ZERO
C
        IF(ICRACK3D > 0)THEN
          ALLOCATE (ELCUTC(2*(NUMELC+NUMELTG)),STAT=IERR)
          IF (IERR/=0) GOTO 1000
          ELCUTC = 0
C
          ALLOCATE (NODENR(NCRKXFE),STAT=IERR)
          IF (IERR/=0) GOTO 1000
          NODENR = 0
C
          ALLOCATE (KXFENOD2ELC(NCRKXFE),STAT=IERR)
          IF (IERR/=0) GOTO 1000
          KXFENOD2ELC = 0
C
          ALLOCATE (ENRTAG(NUMNOD*IENRNOD*NLEVMAX),STAT=IERR)
          IF (IERR/=0) GOTO 1000
          ENRTAG = 0
C
        ELSE
          ALLOCATE (ELCUTC(1))
          ALLOCATE (NODENR(1))
          ALLOCATE (KXFENOD2ELC(1))
          ALLOCATE (ENRTAG(1))
        END IF
        
        IF(IALEFVM == 1)THEN
C        
          ALLOCATE (WFEXT_CELL(NUMELS)  ,STAT=IERR)
          IF (IERR /= 0) GOTO 1000
          WFEXT_CELL(:) = ZERO
C        
          ALLOCATE (FEXT_CELL(3,NUMELS)   ,STAT=IERR)
          IF (IERR /= 0) GOTO 1000
          FEXT_CELL(:,:) = ZERO
C        
          ALLOCATE (FCELL(6,NUMELS)       ,STAT=IERR)
          IF (IERR /= 0) GOTO 1000
          FCELL(:,:) = ZERO    
C   
          ALLOCATE (FINT_CELL(3,NUMELS)       ,STAT=IERR)
          IF (IERR /= 0) GOTO 1000
          FCELL(:,:) = ZERO    
C     
          ALLOCATE (VERTEX(4,NUMNOD)      ,STAT=IERR)
          IF (IERR /= 0) GOTO 1000
          VERTEX(:,:) = ZERO
C      
          ALLOCATE (F_FACE(1:3,6,NUMELS)    ,STAT=IERR)
          IF (IERR /= 0) GOTO 1000
          F_FACE(:,:,:) = ZERO
          
          !          index 2:    1                       2                       3                       4                       5                       6     
          ! index 1 
          !    1               rho                     ssp                   rho.c                       P                       0                       0
          !    2                S1                      S2                      S3                      S4                      S5                      S6         
          !    3             <U.n1>                  <U.n2>                  <U.n3>                  <U.n4>                  <U.n5>                  <U.n6>
              
C       
          CALL ALEFVM_INIT()
C          
        ENDIF

        IF (IALEMUSCL > 0 .AND. .NOT. MULTI_FVM%IS_USED) THEN
           IF (I_LAW == 51) THEN
              NBMAT = TRIMAT
           ELSEIF (I_LAW == 37) THEN
              NBMAT = 1
           ELSE
              NBMAT = 0
           ENDIF
           IF(NBMAT>0)THEN
             ALLOCATE(VOLUME_FRACTION(NBMAT, NUMELS + NSVOIS + NUMELQ + NQVOIS), STAT = IERR)
             IF (IERR /= 0) GOTO 1000
             VOLUME_FRACTION(:, :) = ZERO
             ALLOCATE(NODE_MAX_VALUE(NBMAT, NUMNOD), STAT = IERR)
             IF (IERR /= 0) GOTO 1000
             NODE_MAX_VALUE(:, :) = ZERO
             ALLOCATE(NODE_MIN_VALUE(NBMAT, NUMNOD), STAT = IERR)
             IF (IERR /= 0) GOTO 1000
             NODE_MIN_VALUE(:, :) = ZERO
             ALLOCATE(GRAD(NBMAT, 3,  NUMELS + NSVOIS + NUMELQ + NQVOIS), STAT = IERR)
             IF (IERR /= 0) GOTO 1000
             GRAD(:, :, :) = ZERO
             ALLOCATE(ELCENTER(3, NUMELS + NSVOIS + NUMELQ + NQVOIS), STAT = IERR)
             IF (IERR /= 0) GOTO 1000
             ELCENTER(:, :) = ZERO
           ENDIF
        ENDIF
        
        !/EBCS/NRF VOLUME FRACTION ALLOCATION
        IF(MULTI_FVM%IS_USED)THEN
           NBMAT = MULTI_FVM%NBMAT
        ELSEIF (I_LAW == 51) THEN       
           NBMAT = TRIMAT             
        ELSEIF (I_LAW == 37) THEN   
           NBMAT = 1                  
        ELSE                          
           NBMAT = 0                  
        ENDIF                         
        CALL SEGVAR%create(NBMAT)

        
        IF (MULTI_FVM%IS_USED) THEN
           CALL MULTI_ALLOCATE(MULTI_FVM)
           ALLOCATE (VERTEX(4,NUMNOD)      ,STAT=IERR)
           IF (IERR /= 0) GOTO 1000
           VERTEX(:,:) = ZERO
           ALLOCATE(MULTI_FVM%FVM_CONNECTIVITY%KVOIS(SIELVS), STAT = IERR)
           IF (IERR /= 0) GOTO 1000
           MULTI_FVM%FVM_CONNECTIVITY%KVOIS = 0
        ENDIF
C

C
        IF (NUMPLY > 0) THEN
          ALLOCATE (PLY_INFO(2,NUMPLY),STAT=IERR)
          IF (IERR /= 0) GOTO 1000
          PLY_INFO = 0
        ELSE
          ALLOCATE(PLY_INFO(0,0))
        END IF
C-------------Itet2 of S10      
        IF (NS10E > 0) THEN
          ALLOCATE (ITAGND(NUMNOD),ICNDS10(3*NS10E),VND(3*NS10E),STIFND(NS10E),STAT=IERR)
          IF (IERR /= 0) GOTO 1000
          ITAGND(1:NUMNOD) = 0
          ICNDS10(1:3*NS10E) = 0
          VND(1:3*NS10E) = ZERO
          STIFND(1:NS10E) = ZERO
          IF (IDAMP/=0.OR.NDAMP>0.OR.ISTAT/=0) THEN
            ALLOCATE (VMD(3*NUMNOD),STAT=IERR)
            IF (IERR /= 0) GOTO 1000
            VMD = ZERO
          END IF
          IF (IPARIT/=0.AND.IPARIT/=3) THEN
           ALLOCATE (FR_NBCCCND(2*NSPMD+2),FR_NBCCCND1(2*NSPMD+2),FSKYCND(4*LCNCND),STAT=IERR)
           IF (IERR /= 0) GOTO 1000
           FSKYCND(1:4*LCNCND)=ZERO
          ELSE
           ALLOCATE(FR_NBCCCND(0),FR_NBCCCND1(0),FSKYCND(0))
          END IF
        ELSE
          ALLOCATE(ICNDS10(0),ITAGND(0))
        END IF
C----- part SPMD        
        ALLOCATE (IAD_CNDM(SIAD_CNDM),IAD_CNDM1(SIAD_CNDM),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IAD_CNDM = 0 
        ALLOCATE (FR_CNDM(SFR_CNDM),FR_CNDM1(SFR_CNDM),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FR_CNDM = 0 
C----- part P/ON        
        ALLOCATE (ADDCNCND(SADDCNCND),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        ADDCNCND = 0 
        ALLOCATE (PROCNCND(SPROCNCND),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        PROCNCND = 0 
        ALLOCATE (IADCND(SIADCND),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        IADCND = 0  
C--------2D Mass----------------------------
        IF(N2D/=0) THEN
          ALLOCATE(MS_2D(NUMNOD),STAT=IERR)
          IF (IERR/=0) GOTO 1000
        ENDIF
C-------shell pinching----------------------
C       IF(NPINCH > 0) THEN
          MY_ALLOCATE2D(PINCH_DATA%XPINCH,3,MAX(1,NPINCH))
          PINCH_DATA%XPINCH = ZERO
          MY_ALLOCATE2D(PINCH_DATA%DPINCH,3,MAX(1,NPINCH))
          PINCH_DATA%DPINCH = ZERO
          MY_ALLOCATE2D(PINCH_DATA%VPINCH,3,MAX(1,NPINCH))
          PINCH_DATA%VPINCH = ZERO
          MY_ALLOCATE2D(PINCH_DATA%APINCH,3,MAX(1,PINCH_DATA%SAPINCH))
          PINCH_DATA%APINCH = ZERO
          MY_ALLOCATE(PINCH_DATA%STIFPINCH,MAX(1,PINCH_DATA%SAPINCH))
          PINCH_DATA%STIFPINCH = ZERO 
          MY_ALLOCATE(PINCH_DATA%MSPINCH,MAX(1,PINCH_DATA%SAPINCH))
          PINCH_DATA%MSPINCH = 0   
C       ENDIF
C------end shell pinching-------------------
C-------TAG_SKINS6----------------------
         ALLOCATE(TAG_SKINS6(NUMELS),STAT=IERR)
C-------ALE BUFFER----------------------
         IF ((IALE > 0 .AND. NWALE_RST == 6) ) THEN
            SXCELL = NUMELS + NUMELQ + NE_NSVOIS + NE_NQVOIS
            ALLOCATE(XCELL(3, SXCELL),STAT=IERR)
            IF (IERR/=0) GOTO 1000
            ALLOCATE(XFACE(3, 6, NUMELS + NUMELQ + NE_NSVOIS + NE_NQVOIS),STAT=IERR)
            IF (IERR/=0) GOTO 1000
         ENDIF
C-------INTER18 BUFFER (IAUTO=1)--------
         IF (INTER18_IS_VARIABLE_GAP_DEFINED) THEN
            SXCELL = NUMELS + NUMELQ + NE_NSVOIS + NE_NQVOIS
            IF(.NOT.ALLOCATED(XCELL))ALLOCATE(XCELL(3, SXCELL),STAT=IERR)
            IF (IERR/=0) GOTO 1000
         ENDIF         
C-------FXBODY (moved from radioss2.F) -----
        ALLOCATE (FXBIPM(NBIPM,NFXBODY),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FXBIPM = 0 
        ALLOCATE (FXBNOD(LENNOD),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FXBNOD = 0 
        ALLOCATE (FXBMOD(6,LENMOD),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FXBMOD = 0
        ALLOCATE (FXBGLM(LENGLM),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FXBGLM = 0
        ALLOCATE (FXBCPM(LENCP),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FXBCPM = 0
        ALLOCATE (FXBCPS(LENCP),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FXBCPS = 0
        ALLOCATE (FXBLM(LENLM),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FXBLM = 0
        ALLOCATE (FXBFLS(LENFLS),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FXBFLS = 0
        ALLOCATE (FXBDLS(LENDLS),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FXBDLS = 0
        ALLOCATE (FXBDEP(LENVAR),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FXBDEP = 0
        ALLOCATE (FXBVIT(LENVAR),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FXBVIT = 0
        ALLOCATE (FXBACC(LENVAR),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FXBACC = 0
        ALLOCATE (FXBRPM(LENRPM),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FXBRPM = 0
        ALLOCATE (FXBELM(LENELM),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FXBELM = 0
        ALLOCATE (FXBSIG(LENSIG),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FXBSIG = 0
        ALLOCATE (FXBGRVI(LENGRVI),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FXBGRVI = 0
        ALLOCATE (FXBGRVR(LENGRVR),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        FXBGRVR = 0
C-------EIGEN MODES (moved from radioss2.F) -----
        ALLOCATE (EIGIPM(NEIPM,NEIG),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        EIGIPM = 0
        ALLOCATE (EIGRPM(NERPM,NEIG),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        EIGRPM = 0
        ALLOCATE (EIGIBUF(LEIBUF),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        EIGIBUF = 0
C-------TAGSLV_RBY -----
        ALLOCATE (TAGSLV_RBY(NUMNOD),STAT=IERR)
        IF (IERR/=0) GOTO 1000
        TAGSLV_RBY = 0
C-------Seatbelt structures -----
        ALLOCATE (SLIPRING(NSLIPRING),STAT=IERR)
        IF (IERR/=0) GOTO 1000
C
        ALLOCATE (RETRACTOR(NRETRACTOR),STAT=IERR)
        IF (IERR/=0) GOTO 1000
C
        IF (N_ANCHOR_REMOTE > 0) THEN
          ALLOCATE (ANCHOR_REMOTE%ADD_PROC(NSPMD+1),STAT=IERR)
          IF (IERR/=0) GOTO 1000
          ALLOCATE (ANCHOR_REMOTE%NODE(N_ANCHOR_REMOTE),STAT=IERR)
          IF (IERR/=0) GOTO 1000
        ENDIF
        IF (N_ANCHOR_REMOTE_SEND > 0) THEN
          ALLOCATE (ANCHOR_REMOTE_SEND%ADD_PROC(NSPMD+1),STAT=IERR)
          IF (IERR/=0) GOTO 1000 
          ALLOCATE (ANCHOR_REMOTE_SEND%NODE(N_ANCHOR_REMOTE_SEND),STAT=IERR)
          IF (IERR/=0) GOTO 1000
        ENDIF
C-------TMAX(H3D) -----
         ALLOCATE (TM_DIS(LMAX_DIS*NUMNOD),STAT=IERR)
          IF (IERR/=0) GOTO 1000 
         ALLOCATE (TM_VEL(LMAX_VEL*NUMNOD),STAT=IERR)
          IF (IERR/=0) GOTO 1000 
         ALLOCATE (TM_NSIG1(LMAX_NSIG*NUMNOD),STAT=IERR)
          IF (IERR/=0) GOTO 1000 
         ALLOCATE (TM_NSTRA1(LMAX_NSTRA*NUMNOD),STAT=IERR)
          IF (IERR/=0) GOTO 1000 
         ALLOCATE (TM_NSIG3(LMAX_NSIG*NUMNOD),STAT=IERR)
          IF (IERR/=0) GOTO 1000 
         ALLOCATE (TM_NSTRA3(LMAX_NSTRA*NUMNOD),STAT=IERR)
          IF (IERR/=0) GOTO 1000 
         IF (LMAX_DIS > 0) THEN
           ALLOCATE (TM_PDIS(NUMNOD),STAT=IERR)
           IF (IERR/=0) GOTO 1000 
         ENDIF
         IF (LMAX_VEL > 0) THEN
           ALLOCATE (TM_PVEL(NUMNOD),STAT=IERR)
           IF (IERR/=0) GOTO 1000 
         ENDIF
         IF (LMAX_NSIG > 0) THEN
           ALLOCATE (TM_PNSIG(2*NUMNOD),STAT=IERR)
           IF (IERR/=0) GOTO 1000 
         ENDIF
         IF (LMAX_NSTRA > 0) THEN
           ALLOCATE (TM_PNSTRA(2*NUMNOD),STAT=IERR)
           IF (IERR/=0) GOTO 1000 
         ENDIF
         IF ((LMAX_NSIG+LMAX_NSTRA)>0) THEN
           ALLOCATE(GPSTMP(6*NUMNOD),P2TMP(2*NUMNOD),STAT=IERR)
           IF (IERR/=0) GOTO 1000 
         END IF
C
C  DEBUG MEMORY ALLOCATIONS
C
        IF(DEBUG(6)==1)THEN
          CALL SZ_PRINT()
        END IF
C
        RETURN
 1000   CONTINUE
        IERROR = IERROR + IERR
        IERR = IERR0
        IF (IERROR/=0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        END IF

      END


Chd|====================================================================
Chd|  SZ_PRINT                      source/output/restart/arralloc.F
Chd|-- called by -----------
Chd|        RESTALLOC                     source/output/restart/arralloc.F
Chd|-- calls ---------------
Chd|        SPMD_CHKW                     source/mpi/generic/spmd_chkw.F
Chd|        SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|        SPMD_WIOUT                    source/mpi/generic/spmd_wiout.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
        SUBROUTINE SZ_PRINT()
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "tabsiz_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
C----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER IWIOUT
C-----------------------------------------------
C   
C    For debug purposes only
C  
        IF (ISPMD == 0) THEN
          WRITE(IOUT,'(A)') ' '
          WRITE(IOUT,'(A)') 'MEMORY ADRESS MAP '
          WRITE(IOUT,'(A)') '--------------'
          WRITE(IOUT,'(A)') ' '
          WRITE(IOUT,'(A)') 'INTEGER ARRAYS'
          WRITE(IOUT,'(A)') '--------------'
          WRITE(IOUT,'(A)') ' '
          WRITE(IOUT,'(A)') 'NSPMD /  ARRAY  /  SIZE '
          WRITE(IOUT,'(A)') ' '
        END IF
C   I10
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SICODE :  ',SICODE
C   I10A
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLSHSUB : ',SLSHSUB
C   I11
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SISKEW :  ',SISKEW
C   I12
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SISKWN :  ',SISKWN
C   I12A
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIFRAME : ',SIFRAME
C   I13
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SNETH :   ',SNETH
C   I14
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIBCSLAG :',SIBCSLAG
C   I15
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIPART :  ',SIPART
C   I16
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SNOM_OPT :',SNOM_OPT
C   I17
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SNPC :    ',SNPC
C   I18
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIXUR :   ',SIXUR
C   I19
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIXTG :   ',SIXTG
C   I19A
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIXTG1 :  ',SIXTG1
C   I20
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIXS :    ',SIXS
C   I21
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIXQ :    ',SIXQ
C   I22
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIXC :    ',SIXC
C   I23
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIXT :    ',SIXT
C   I24
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIXP :    ',SIXP
C   I25
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIXR :    ',SIXR
C   I26
C
C   I27
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SITAB :   ',SITAB
C   I28
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SITABM1 : ',SITABM1
C   I29
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SGJBUFI : ',SGJBUFI
C   I30
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SNALE :   ',SNALE
C   I31
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIELVS :  ',SIELVS
C   I33
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIFILL :  ',SIFILL
C   I34
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIMS :    ',SIMS
C   I35
!!        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SISUBS :  ',SISUBS
C   I36
!!        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIGRN :   ',SIGRN
C   I37
!!        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SISURF :  ',SISURF
C   I38
!!        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SISLIN :  ',SISLIN
C   I39
!!        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIBUFSSG :',SIBUFSSG
C   I40
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SKXX :    ',SKXX
C   I40A
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIXX :    ',SIXX
C   I40B
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SKXSP :   ',SKXSP
C   I40C
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIXSP :   ',SIXSP
C   I40D
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SNOD2SP : ',SNOD2SP
C   I40E
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SISPSYM : ',SISPSYM
C   I40F
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SISPCOND :',SISPCOND
C   I40G
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SISPHIO : ',SISPHIO
C   I40H
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLPRTSPH :',SLPRTSPH
C   I40I
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLONFSPH :',SLONFSPH
C   I41
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIBCL :   ',SIBCL
C   I42
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIBFV :   ',SIBFV
C   I43
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIECRAN : ',SIECRAN
C   I44
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SILAS :   ',SILAS
C   I45
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIEBCS :  ',SIEBCS
C   I46
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLACCELM :',SLACCELM
C   I47
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLSENSOR :',SLSENSOR
C   I48
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SNNLINK : ',SNNLINK
C   I48A
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLNLINK : ',SLNLINK
C   I49
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIUSER :  ',SIUSER
C   I50
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIPARG :  ',SIPARG
C   I51
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SDD_IAD : ',SDD_IAD
C   I52
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIGRV :   ',SIGRV
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLGRAV :  ',SLGRAV
C   I54
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIEXLNK : ',SIEXLNK
C   I55
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIBVEL :  ',SIBVEL
C   I55A
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLBVEL :  ',SLBVEL
C   I56
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIACTIV : ',SIACTIV
C   I57
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIBMPC :  ',SIBMPC
C   I58
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SKINET :  ',SKINET
C   I59
C
C   I60
C
C   I61
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIPARI :  ',SIPARI
C   I62
c        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SINBUF :  ',SINBUF
C   I64
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SNPRW :   ',SNPRW
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLPRW :   ',SLPRW
C   I65

C   I66
C
C   I67
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SICONX :  ',SICONX
C   I67B
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SMADPRT : ',SMADPRT
C   I67C
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SMADSH4 : ',SMADSH4
C   I67D
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SMADSH3 : ',SMADSH3
C   I67E
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SMADSOL : ',SMADSOL
C   I67F
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SMADNOD : ',SMADNOD
C   I67G
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SMADFAIL: ',SMADFAIL
C   I68
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SNPBY :   ',SNPBY
C   I68A
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SNPBYL :  ',SNPBYL
C   I69
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLPBY :   ',SLPBY
C   I69A
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLPBYL :  ',SLPBYL
C   I70
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLRIVET : ',SLRIVET
C   I71
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SNSTRF :  ',SNSTRF
C   I72
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLJOINT : ',SLJOINT
C   I73
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SNODPOR : ',SNODPOR
C   I74
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SMONVOL : ',SMONVOL
C   I75
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLAGBUF : ',SLAGBUF
C   I76
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SITHGRP : ',SITHGRP
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SITHBUF : ',SITHBUF
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SRTHBUF : ',SRTHBUF
C   I76A - I76TH(1
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SITHGRPA: ',SITHGRPA
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SITHBUFA: ',SITHBUFA
C   I76B - I76TH(2
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SITHGRPB: ',SITHGRPB
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SITHBUFB: ',SITHBUFB
C   I76C - I76TH(3
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SITHGRPC: ',SITHGRPC
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SITHBUFC: ',SITHBUFC
C   I76D - I76TH(4
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SITHGRPD: ',SITHGRPD
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SITHBUFD: ',SITHBUFD
C   I76E - I76TH(5
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SITHGRPE: ',SITHGRPE
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SITHBUFE: ',SITHBUFE
C   I76F - I76TH(6
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SITHGRPF: ',SITHGRPF
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SITHBUFF: ',SITHBUFF
C   I76G - I76TH(7
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SITHGRPG: ',SITHGRPG
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SITHBUFG: ',SITHBUFG
C   I76H - I76TH(8
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SITHGRPH: ',SITHGRPH
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SITHBUFH: ',SITHBUFH
C   I76I - I76TH(9
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SITHGRPI: ',SITHGRPI
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SITHBUFI: ',SITHBUFI
C   I77
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIAD_ELEM:',SIAD_ELEM
C   I77B
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIAD_RBY: ',SIAD_RBY
C   I77C
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIAD_RBY2:',SIAD_RBY2
C   I77D
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIAD_I2M: ',SIAD_I2M
C   I77E
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIADCJ :  ',SIADCJ
C   I77F
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIAD_RBM: ',SIAD_RBM
C   I77G
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIAD_RBM2:',SIAD_RBM2
C   I77H
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIAD_SEC: ',SIAD_SEC
C   I77I
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIAD_CUT: ',SIAD_CUT
C   I78
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SFR_ELEM: ',SFR_ELEM
C   I78B
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SFR_RBY : ',SFR_RBY
C   I78C
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SFR_WALL: ',SFR_WALL
C   I78D
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SFR_RBY2: ',SFR_RBY2
C   I78E
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SFR_I2M : ',SFR_I2M
C   I78F
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SFR_MV :  ',SFR_MV
C   I78G
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SFR_LL :  ',SFR_LL
C   I78H
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SFR_CJ :  ',SFR_CJ
C   I78I
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SFR_RBM : ',SFR_RBM
C   I78J
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SFR_RBM2: ',SFR_RBM2
C   I78K
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SFR_SEC : ',SFR_SEC
C   I78L
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SFR_CUT : ',SFR_CUT
C   I78M
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SRG_CUT : ',SRG_CUT
C   I78N
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SFR_MAD : ',SFR_MAD
C   I78P
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SFR_I18 : ',SFR_I18
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SDD_R2R : ',SDD_R2R
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SFR_LAGF: ',SFR_LAGF
C   I79
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SWEIGHT : ',SWEIGHT
C   I79A
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SNEWFRONT:',SNEWFRONT
C   I79B
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SNODGLOB: ',SNODGLOB
C   I79C
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SNBRCVOIS:',SNBRCVOIS
C   I79D
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLNRCVOIS:',SLNRCVOIS
C   I79E
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SNBSDVOIS:',SNBSDVOIS
C   I79F
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLNSDVOIS:',SLNSDVOIS
C   I79G
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SNERCVOIS:',SNERCVOIS
C   I79H
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLERCVOIS:',SLERCVOIS
C   I79I
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SNESDVOIS:',SNESDVOIS
C   I79J
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLESDVOIS:',SLESDVOIS
C   I79K

C   I79L

C   I79M
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SNPSEGCOM:',SNPSEGCOM
C   I79N
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLSEGCOM: ',SLSEGCOM
C   I79P
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SNPORGEO: ',SNPORGEO
C   I79Q
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLNODPOR: ',SLNODPOR
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLLAGF :  ',SLLAGF
C   I80
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SICODT :  ',SICODT
C   I81
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SICODR :  ',SICODR

C   I82
C
C   I83
C
C   I84
C
C   I85
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SISKY :   ',SISKY
C   I86
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SADSKY :  ',SADSKY
C   I86B
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SPROCNE : ',SPROCNE
C   I86C
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SADDCNI2 : ',SADDCNI2
C   I86D
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SPROCNI2 : ',SPROCNI2
C   I86E
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIADSDP : ',SIADSDP
C   I86F
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIADRCP : ',SIADRCP
C   I87
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIADS : ',SIADS
C   I87B
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIADWAL : ',SIADWAL
C   I87C
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIADRBK : ',SIADRBK
C   I87D
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIADI2 : ',SIADI2
C   I87E
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIADMV2 : ',SIADMV2
C   I87F
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIADMV3 : ',SIADMV3
C   I87G
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIADLL : ',SIADLL
C   I87H
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIADRBM : ',SIADRBM
C   I87I
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIADI18 : ',SIADI18
C   I88
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SILINK : ',SILINK
C   I88A
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SFR_RL : ',SFR_RL
C   I89
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLLINK : ',SLLINK
C   I89A
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIADRL : ',SIADRL
C   I90
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLINALE : ',SLINALE
C   I91
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SNEFLSW : ',SNEFLSW
C   I92
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SNNFLSW : ',SNNFLSW
C   I93
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SICUT : ',SICUT
C   ICONTACT
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SICONTACT : ',SICONTACT
Csv TH VARIABLE NAMES
        WRITE(IOUT,'(A,I8)') 'SITHVAR : ',SITHVAR
cow
       IF(NSPMD>1) THEN
C traitement necessaire pour recuperer les output dans l'ordre
         IWIOUT = 0
         IF (ISPMD/=0) CALL SPMD_CHKW(IWIOUT,IOUT)
         CALL SPMD_GLOB_ISUM9(IWIOUT,1)
         CALL SPMD_IBCAST(IWIOUT,IWIOUT,1,1,0,2)
         IF (IWIOUT>0) THEN
           CALL SPMD_WIOUT(IOUT,IWIOUT)
           IWIOUT = 0
         ENDIF
       ENDIF
       IF (ISPMD == 0) THEN
          WRITE(IOUT,'(A)') ' '
          WRITE(IOUT,'(A)') 'REAL ARRAYS'
          WRITE(IOUT,'(A)') '-----------'
          WRITE(IOUT,'(A)') ' '
          WRITE(IOUT,'(A)') 'NSPMD /  ARRAY  /  SIZE '
          WRITE(IOUT,'(A)') ' '
        END IF
C   M11
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SX : ',SX
C   M11
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SD : ',SD
C   M12
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SV : ',SV
C   M13
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SVR : ',SVR
C   M14
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SDR : ',SDR
C   M15
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' STHKE : ',STHKE
C   M16
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SDAMPR : ',SDAMPR
C   M16A
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SDAMP : ',SDAMP
C   M17
C
C   M18
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SMS : ',SMS
C   M19
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SIN : ',SIN
C   M20
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' STF : ',STF
C   M21
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SPM : ',SPM
C   M22
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SSKEW : ',SSKEW
C   M22A
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SXFRAME : ',SXFRAME
C   M23
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SGEO : ',SGEO
C   M24
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SEANI : ',SEANI
C   M25
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SBUFMAT : ',SBUFMAT
C   M26
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SBUFGEO : ',SBUFGEO
C   M27
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SBUFSF : ',SBUFSF
C   M28
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SRBMPC : ',SRBMPC
C   M29
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SGJBUFR : ',SGJBUFR
C   M30
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SW : ',SW
C   M31
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SVEUL : ',SVEUL
C   M32
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SFILL : ',SFILL
C   M33
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SDFILL : ',SDFILL
C   M34
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SALPH : ',SALPH
C   M35
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SWB : ',SWB
C   M36
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SDSAVE : ',SDSAVE
C   M36A
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SDRSAVE : ',SDRSAVE
C   M37
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SASAVE : ',SASAVE
C   M37A
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SARSAVE : ',SARSAVE
C   M37B
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SSTSAVE : ',SSTSAVE
C   M37C
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SSTRSAVE : ',SSTRSAVE
C   M37D
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SFSAVSAV : ',SFSAVSAV
C   M38
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SSPBUF : ',SSPBUF
C   M38E
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SVSPHIO : ',SVSPHIO
C   M38F
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SSPHVELN : ',SSPHVELN
C   M39
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SMSNF : ',SMSNF
C   M40
C
C   M41
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SFORC : ',SFORC
C   M42
C
C   M43
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SVEL : ',SVEL
C   M44
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SFSAV : ',SFSAV
C   M45
C
C   M46
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SFZERO : ',SFZERO
C   M47
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SXLAS : ',SXLAS
C   M48A
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SREBCS : ',SREBCS
C   M48
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SACCELM : ',SACCELM
C   M49
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SSENSOR : ',SSENSOR
C   M50
C
C   M51
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SFBVEL : ',SFBVEL
C   M52
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SGRAV : ',SGRAV
C   M53
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SFR_WAVE : ',SFR_WAVE
c
c        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' FAILWAVE : ',IFAILWAVE*NUMNOD
C   M54
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SPARTS0 : ',SPARTS0
C   M55
C
C   M56
C
C   M57
C
C   M58
C
C   M59
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SUSER : ',SUSER
C   M60
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SELBUF : ',SELBUF
C   M64B
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SDBUFIN : ',SDBUFIN
C   M62
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SRWBUF : ',SRWBUF
C   M62A
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SRWSAV : ',SRWSAV
C   M63

C   M64
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SRBY : ',SRBY
C   M64A
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SRBYL : ',SRBYL
C   M65
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SRIVET : ',SRIVET
C   M66
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SSECBUF : ',SSECBUF
C   M67
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SVOLMON : ',SVOLMON
C   M68
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SLAMBDA : ',SLAMBDA
C   M69

C   RCONTACT
        WRITE(IOUT,'(I4,A13,I8)') ISPMD+1,' SRCONTACT : ',SRCONTACT

       IF(NSPMD>1) THEN
C traitement necessaire pour recuperer les output dans l'ordre
         IWIOUT = 0
         IF (ISPMD/=0) CALL SPMD_CHKW(IWIOUT,IOUT)
         CALL SPMD_GLOB_ISUM9(IWIOUT,1)
         CALL SPMD_IBCAST(IWIOUT,IWIOUT,1,1,0,2)
         IF (IWIOUT>0) THEN
           CALL SPMD_WIOUT(IOUT,IWIOUT)
           IWIOUT = 0
         ENDIF
       ENDIF

        END
