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
Chd|  FREFORM                       source/starter/freform.F      
Chd|-- called by -----------
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|        FREERR                        source/starter/freform.F      
Chd|        CHECK_MOD                     ../common_source/modules/check_mod.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE FREFORM(MULTI_FVM,LSUBMODEL,ISUB_HIERARCHY,TRALL_MAXVAL,DETONATORS)
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
      USE SUBMODEL_MOD
      USE CHECK_MOD
      USE INOUTFILE_MOD
      USE DETONATORS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr12_c.inc"
#include      "lon_c.inc"
#include      "sysunit.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      INTEGER ISUB_HIERARCHY,TRALL_MAXVAL(7)
      TYPE(DETONATOR_STRUCT_)::DETONATORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, J1, J2, IST, ID, ID1, KOPT, NCART,LC
      INTEGER IFILNAM(13)
      INTEGER IRFL0, IRFE, IRFL,NCHARMAX,ISUBMOD
      CHARACTER CART*ncharline, KEY*10,CHRUN*2, CHRUN0*2
C-----------------------------------------------
C   L o c a l   P a r a m e t e r s
C-----------------------------------------------
      INTEGER NOPT,K
      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=2148) :: TMP_NAME
C=======================================================================
C
C-------------------------------------------------------------------
C      STARTER D00
C-------------------------------------------------------------------
C     FACILEMENT DES OPTIONS FUTURES. 
C     L' ORDRE DES OPTION EST L' ORDRE DE TRAITEMENT PAR LE STARTER,
C     L'ORDRE D' INPUT DU D00 EST QUELCONQUE. (SAUF /END = FIN DES DONNEES)
C     L'OPTION 'END' KEND DOIT ETRE EN FIN DE LISTE.
C-------------------------------------------------------------------
      K=1

      KEY0(K)='TITLE'
      LKEY0(K)=5
      KTITL=K
      K=K+1

      KEY0(K)='MEMOR'
      LKEY0(K)=5
      KMEMOR=K
      K=K+1

      KEY0(K)='ARCH'
      LKEY0(K)=4
      KARCH=K
      K=K+1 

      KEY0(K)='SPMD'
      LKEY0(K)=4
      KPARAL=K
      K=K+1

      KEY0(K)='CAA'
      LKEY0(K)=3
      KCAA=K
      K=K+1
C
      KEY0(K)='IOFLA'
      LKEY0(K)=5
      KIOFLA=K
      K=K+1
C
      KEY0(K)='REFSTA'
      LKEY0(K)=6
      KREFSTA=K
      K=K+1
C
      KEY0(K)='XREF'
      LKEY0(K)=4
      KXREF=K
      K=K+1
C
      KEY0(K)='EREF'
      LKEY0(K)=4
      KEREF=K
      K=K+1
C
      KEY0(K)='ANALY'
      LKEY0(K)=5
      KANALY=K
      K=K+1
C
      KEY0(K)='DEF_SOLI'
      LKEY0(K)=6
      KDEFSO=K
      K=K+1
C
      KEY0(K)='DEF_SHEL'
      LKEY0(K)=6
      KDEFSH=K
      K=K+1
      
      KEY0(K)='ALE'
      LKEY0(K)=3
      KALE=K
      K=K+1

      KEY0(K)='UPWIND'
      LKEY0(K)=6
      KUPWIND=K
      K=K+1
C
      KEY0(K)='EULER'
      LKEY0(K)=5
      KEULER=K
      K=K+1

      KEY0(K)='EBCS'
      LKEY0(K)=4
      KEBCS=K
      K=K+1
C
      KEY0(K)='HEAT'
      LKEY0(K)=4
      KTHERM=K
      K=K+1
C
C  thermal material expansion
C
      KEY0(K)='THERM_ST'
      LKEY0(K)=8
      KTHERM_ST=K
      K=K+1
C
      KEY0(K)='DFS'
      LKEY0(K)=3
      KDFS=K
      K=K+1
C
      KEY0(K)='RANDOM'
      LKEY0(K)=6
      KRAND=K
      K=K+1
C
      KEY0(K)='LAGMUL'
      LKEY0(K)=6
      KLAGM=K
      K=K+1
C--------------------------
      KEY0(K)='MAT'
      LKEY0(K)=3
      KMAT=K
      K=K+1
C--------------------------
      KEY0(K)='NODE'
      LKEY0(K)=4
      KNODE=K
      K=K+1
C--------------------------
      KEY0(K)='BCS'
      LKEY0(K)=3
      KBCS=K
      K=K+1
C--------------------------
      KEY0(K)='SKEW'
      LKEY0(K)=4
      KSKEW=K
      K=K+1
C--------------------------
      KEY0(K)='PART'
      LKEY0(K)=4
      KPART=K
      K=K+1
C--------------------------
      KEY0(K)='THPART'
      LKEY0(K)=6
      KTHPART=K
      K=K+1
C--------------------------
      KEY0(K)='BRICK'
      LKEY0(K)=5
      KBRICK=K
      K=K+1

      KEY0(K)='TETRA4'
      LKEY0(K)=6
      KTETRA4=K
      K=K+1
C
      KEY0(K)='TETRA1'
      LKEY0(K)=6
      KTETRA10=K
      K=K+1

      KEY0(K)='BRIC20'
      LKEY0(K)=6
      KBRICK20=K
      K=K+1

      KEY0(K)='SHEL16'
      LKEY0(K)=6
      KSHELL16=K
      K=K+1  

      KEY0(K)='QUAD'
      LKEY0(K)=4
      KQUAD=K
      K=K+1
C
      KEY0(K)='SHELL'
      LKEY0(K)=5
      KSHELL=K
      K=K+1

      KEY0(K)='SH3N'
      LKEY0(K)=4
      KSH_3N=K
      K=K+1

      KEY0(K)='SHFRA'
      LKEY0(K)=5
      KSHFRA=K
      K=K+1

      KEY0(K)='TRUSS'
      LKEY0(K)=5
      KTRUSS=K
      K=K+1

      KEY0(K)='BEAM'
      LKEY0(K)=4
      KBEAM=K
      K=K+1

      KEY0(K)='SPRING'
      LKEY0(K)=6
      KSPRIN=K
      K=K+1
C--------------------------
      KEY0(K)='LIAISO'
      LKEY0(K)=6
      KLIAISON=K
      K=K+1
C--------------------------
      KEY0(K)='GRNOD'
      LKEY0(K)=5
      KGRNOD=K
      K=K+1
C--------------------------
      KEY0(K)='SUBSET'
      LKEY0(K)=6
      KSUBSET=K
      K=K+1
C--------------------------
      KEY0(K)='SURF'
      LKEY0(K)=4
      KSURF=K
      K=K+1
C--------------------------
      KEY0(K)='LINE'
      LKEY0(K)=4
      KSLIN=K
      K=K+1
C--------------------------
      KEY0(K)='PROP'
      LKEY0(K)=4
      KPROP=K
      K=K+1
C--------------------------
      KEY0(K)='FUNCT'
      LKEY0(K)=5
      KFUNCT=K
      K=K+1
C--------------------------
      KEY0(K)='FUNC_2D'
      LKEY0(K)=7
      KFUNC2D=K
      K=K+1
C--------------------------
      KEY0(K)='MOVE_FUN'
      LKEY0(K)=8
      KMFUNCT=K
      K=K+1
C--------------------------
      KEY0(K)='CLOAD'
      LKEY0(K)=5
      KCLOAD=K
      K=K+1

      KEY0(K)='PLOAD'
      LKEY0(K)=5
      KPLOAD=K
      K=K+1

      KEY0(K)='GRAV'
      LKEY0(K)=4
      KGRAV=K
      K=K+1
C--------------------------
      KEY0(K)='EXTERN'
      LKEY0(K)=6
      KR2R=K
      K=K+1
C--------------------------
      KEY0(K)='SUBDOMAIN'
      LKEY0(K)=9
      KSUBDOM=K
      K=K+1      

      KEY0(K)='INIVEL'
      LKEY0(K)=6
      KINIV=K
      K=K+1

      KEY0(K)='ACCEL'
      LKEY0(K)=5
      KACCEL=K
      K=K+1

      KEY0(K)='SENSOR'
      LKEY0(K)=6
      KSENSO=K
      K=K+1
C--------------------------
      KEY0(K)='IBVEL'
      LKEY0(K)=5
      KIBVEL=K
      K=K+1
C--------------------------
      KEY0(K)='ACTIV'
      LKEY0(K)=5
      KACTIV=K
      K=K+1
C--------------------------
      KEY0(K)='GJOINT'
      LKEY0(K)=6
      KGJOIN=K
      K=K+1
C--------------------------
      KEY0(K)='MPC'
      LKEY0(K)=3
      KMPC=K
      K=K+1

      KEY0(K)='INTER'
      LKEY0(K)=5
      KINTER=K
      K=K+1

      KEY0(K)='FRICTION'
      LKEY0(K)=8
      KFRICTION=K
      K=K+1

      KEY0(K)='FRIC_ORIEN'
      LKEY0(K)=10
      KFRIC_ORIENT=K
      K=K+1

      KEY0(K)='RWALL'
      LKEY0(K)=5
      KRWALL=K
      K=K+1

      KEY0(K)='RBODY'
      LKEY0(K)=5
      KRBODY=K
      K=K+1

      KEY0(K)='FXBODY'
      LKEY0(K)=6
      KFXBOD=K
      K=K+1  

      KEY0(K)='RLINK'
      LKEY0(K)=5
      KRLINK=K
      K=K+1

      KEY0(K)='MADYMO'
      LKEY0(K)=6
      KRCONX=K
      K=K+1

      KEY0(K)='ADMAS'
      LKEY0(K)=5
      KADMAS=K
      K=K+1
C---------IMP REMPLACE FIX POUR VEL ET DIS
      KEY0(K)='IMPVEL'
      LKEY0(K)=6
      KIMPVEL=K
      K=K+1
C--- fixVEL doit suivre IMPvel
      KEY0(K)='FIXVEL'
      LKEY0(K)=6
      KFIXVEL=K
      K=K+1
C--- fixdis doit suivre fixvel
      KEY0(K)='FIXDIS'
      LKEY0(K)=6
      KFIXDIS=K
      K=K+1
C--- IMPdis doit suivre fixDIS
      KEY0(K)='IMPDIS'
      LKEY0(K)=6
      KIMPDIS=K
      K=K+1
C--------------------------
      KEY0(K)='IMPACC'
      LKEY0(K)=6
      KIMPACC=K
      K=K+1

      KEY0(K)='RIVET'
      LKEY0(K)=5
      KRIVET=K
      K=K+1

      KEY0(K)='SECT'
      LKEY0(K)=4
      KSECT=K
      K=K+1

      KEY0(K)='CYL_JO'
      LKEY0(K)=6
      KCJOIN=K
      K=K+1

      KEY0(K)='MONVOL'
      LKEY0(K)=6
      KMONVO=K
      K=K+1

      KEY0(K)='TH'
      LKEY0(K)=2
      KTH=K
      K=K+1

      KEY0(K)='INISTA'
      LKEY0(K)=6
      KINISTA=K
      K=K+1

      KEY0(K)='INIMAP1D'
      LKEY0(K)=8
      KINIMAP1D=K
      K=K+1

      KEY0(K)='INIMAP2D'
      LKEY0(K)=8
      KINIMAP2D=K
      K=K+1

      KEY0(K)='XELEM'
      LKEY0(K)=5
      KXELEM=K
      K=K+1

      KEY0(K)='USERW'
      LKEY0(K)=5
      KUSERW=K
      K=K+1

      KEY0(K)='DAMP'
      LKEY0(K)=4
      KDAMP=K
      K=K+1

      KEY0(K)='FRAME'
      LKEY0(K)=5
      KFRAM=K
      K=K+1

      KEY0(K)='SPHCEL'
      LKEY0(K)=6
      KXSPH=K
      K=K+1

      KEY0(K)='SPHGLO'
      LKEY0(K)=6
      KSPGLO=K
      K=K+1

      KEY0(K)='SPHBCS'
      LKEY0(K)=6
      KSPCOND=K
      K=K+1

      KEY0(K)='INISHE'
      LKEY0(K)=6
      KINISHE=K
      K=K+1

      KEY0(K)='INISH3'
      LKEY0(K)=6
      KINISH3=K
      K=K+1

      KEY0(K)='INIBRI'
      LKEY0(K)=6
      KINIBRI=K
      K=K+1

      KEY0(K)='INIQUA'
      LKEY0(K)=6
      KINIQUA=K
      K=K+1

      KEY0(K)='INIBEA'
      LKEY0(K)=6
      KINIBEA=K
      K=K+1

      KEY0(K)='INITRU'
      LKEY0(K)=6
      KINITRU=K
      K=K+1
C--------------------------
      KEY0(K)='SPH'
      LKEY0(K)=3
      KSPHOPT=K
      K=K+1
C--------------------------
      KEY0(K)='ATH'
      LKEY0(K)=3
      KTH1(1)=K
      K=K+1 
     
      KEY0(K)='BTH'
      LKEY0(K)=3
      KTH1(2)=K
      K=K+1 
      
      KEY0(K)='CTH'
      LKEY0(K)=3
      KTH1(3)=K
      K=K+1 
      
      KEY0(K)='DTH'
      LKEY0(K)=3
      KTH1(4)=K
      K=K+1 
      
      KEY0(K)='ETH'
      LKEY0(K)=3
      KTH1(5)=K
      K=K+1 
      
      KEY0(K)='FTH'
      LKEY0(K)=3
      KTH1(6)=K
      K=K+1 
      
      KEY0(K)='GTH'
      LKEY0(K)=3
      KTH1(7)=K
      K=K+1 
      
      KEY0(K)='HTH'
      LKEY0(K)=3
      KTH1(8)=K
      K=K+1 
               
      KEY0(K)='ITH'
      LKEY0(K)=3
      KTH1(9)=K
      K=K+1 

      KEY0(K)='PENTA6'
      LKEY0(K)=6
      KPENTA6=K
      K=K+1  

      KEY0(K)='EIG'
      LKEY0(K)=3
      KEIG=K
      K=K+1

      KEY0(K)='UNIT'
      LKEY0(K)=4
      KUNIT=K
      K=K+1

      KEY0(K)='FAIL'
      LKEY0(K)=4
      KFAIL=K
      K=K+1  
 
      KEY0(K)='KEY'
      LKEY0(K)=3
      KKEY = K
      K=K+1 

      KEY0(K)='ENCRYPT'
      LKEY0(K)=7
      KKEY_ENCRYPT = K
      K=K+1 

      KEY0(K)='IMPLICIT'
      LKEY0(K)=8
      KIMPLICIT=K
      K=K+1

      KEY0(K)='IMPL'
      LKEY0(K)=4
      KIMPL=K
      K=K+1

      KEY0(K)='BEM'
      LKEY0(K)=3
      KBEM=K
      K=K+1

      KEY0(K)='ANIM'
      LKEY0(K)=4
      KANIM=K
      K=K+1
C remarque, cette option et lu dans le starter0.F
C a ne pas utiliser, elle sert pour lire le rootnam
      KEY0(K)='BEGIN'
      LKEY0(K)=5
cpas besoin !     KBEEGIN= K
      K=K+1
C-------------------------------------------------------------------
C optrion bidon pour preprocesseur
      KEY0(K)='PREPRO'
      LKEY0(K)=6
cpas besoin !     KPREPRO= K
      K=K+1
      KEY0(K)='CNODE'
      LKEY0(K)=5
      KCNODE=K
      K=K+1  
C-------------------------------------------------------------------
      KEY0(K)='TRANSF'
      LKEY0(K)=6
      KTRANSF =K
      K=K+1
C
      KEY0(K)='/SUBMOD'
      LKEY0(K)=6
      KSUBMOD =K
      K=K+1
C
      KEY0(K)='/ENDSU'
      LKEY0(K)=6
      KENDSU =K
      K=K+1
C-------------------------------------------------------------------
      KEY0(k)='LEVSET'
      LKEY0(K)=6
      KLEVSET=K
      K=K+1
C-------------------------------------------------------------------
      KEY0(K)='ADMESH'
      LKEY0(K)=6
      KADMESH =K
      K=K+1
C
      KEY0(K)='INITEM'
      LKEY0(K)=6
      KINTEMP=K
      K=K+1
C
      KEY0(K)='IMPTEM'
      LKEY0(K)=6
      KIMPTEMP=K
      K=K+1
C
      KEY0(K)='IMPFLUX'
      LKEY0(K)=7
      KIMPFLUX=K
      K=K+1
C
      KEY0(K)='CONVEC'
      LKEY0(K)=6
      KCONVEC=K
      K=K+1
C-------------------------------------------------------------------
      KEY0(K)='STAMPING'
      LKEY0(K)=8
      KSTAMPING=K
      K=K+1
      KEY0(K)='RBE2'
      LKEY0(K)=4
      KRBE2 =K
      K=K+1
      KEY0(K)='RBE3'
      LKEY0(K)=4
      KRBE3 =K
      K=K+1
      KEY0(K)='INTTHICK'
      LKEY0(K)=8
      KINTTHICK=K
      K=K+1
C      
      KEY0(K)='PLYXFEM'
      LKEY0(K)=7
      KPLYXFEM=K
      K=K+1         
C---------------------
      KEY0(K)='AMS'
      LKEY0(K)=3
      KSMS=K
      K=K+1
C      
      KEY0(K)='VISC'
      LKEY0(K)=4
      KVISC=K
      K=K+1      
C---------------------
      KEY0(K)='RADIATION'
      LKEY0(K)=9
      KRADIA=K
      K=K+1
C---------------------
      KEY0(K)='TABLE'
      LKEY0(K)=5
      KTABLE=K
      K=K+1
C---------------------
C new box option
      KEY0(K)='BOX'
      LKEY0(K)=3
      KBOX=K
      K=K+1
C
      KEY0(K)='STATE'
      LKEY0(K)=5
      KSTATE=K
      K=K+1
C---------------------
      KEY0(K)='INIVOL'
      LKEY0(K)=6
      KINIVOL=K
      K=K+1
C---------------------
      KEY0(K)='GAUGE'
      LKEY0(K)=5
      KGAUGE=K
      K=K+1
C---------------------
      KEY0(K)='EOS'
      LKEY0(K)=3
      KEOS=K
      K=K+1
C---------------------
      KEY0(K)='LOAD'
      LKEY0(K)=4
      KLOAD=K
      K=K+1
C---------------------
      KEY0(K)='LEAK'
      LKEY0(K)=4
      KLEAK=K
      K=K+1
C---------------------
      KEY0(K)='CRASH'
      LKEY0(K)=5
      KCRASH=K
      K=K+1
C---------------------
      KEY0(K)='INICRACK'
      LKEY0(K)=8
      KINICRACK=K
      K=K+1
C---------------------
      KEY0(K)='CLUSTER'
      LKEY0(K)=7
      KCLUSTER=K
      K=K+1
C
      KEY0(K)='DEFAULT'
      LKEY0(K)=7
      KDEFAULT=K
      K=K+1
C---------------------
      KEY0(K)='IGE3D'
      LKEY0(K)=5
      KIGE3D=K
      K=K+1
C---------------------
      KEY0(K)='PRIVATE'
      LKEY0(K)=7
      KPRIVATE=K
      K=K+1
C---------------------
      KEY0(K)='PERTURB'
      LKEY0(K)=7
      KPERTURB=K
      K=K+1
C---------------------
      KEY0(K)='STACK'
      LKEY0(K)=5
      KSTACK=K
      K=K+1 
C---------------------  
      KEY0(K)='PLY'
      LKEY0(K)=3
      KPLY=K
      K=K+1
C---------------------
      KEY0(K)='DRAPE'
      LKEY0(K)=5
      KDRAPE=K
      K=K+1
C---------------------
      KEY0(K)='INT2'
      LKEY0(K)=4
      KINT2=K
      K=K+1
C---------------------
      KEY0(K)='INISPRI'
      LKEY0(K)=7
      KINISPRI=K
      K=K+1
C---------------------
      KEY0(K)='SH_3NFRA'
      LKEY0(K)=8
      KSH3NFRA=K
      K=K+1
C---------------------
      KEY0(K)='S17_OLD'
      LKEY0(K)=7
      KS17_OLD=K
      K=K+1
C--------------------------
      KEY0(K)='NBCS'
      LKEY0(K)=3
      KNBCS=K
      K=K+1
C---------------------
      KEY0(K)='PRELOAD'   ! For bolt preloading
      LKEY0(K)=7
      KPRELOAD=K
      K=K+1
C---------------------
      KEY0(K)='INIGRAV'
      LKEY0(K)=7
      KINIGRAV=K
      K=K+1
C---------------------
      KEY0(K)='TRIA'
      LKEY0(K)=4
      KTRIA=K
      K=K+1
C--------------------------
      KEY0(K)='MERGE'
      LKEY0(K)=5
      KMERGE=K
      K=K+1
C--------------------------
      KEY0(K)='SET'
      LKEY0(K)=3
      KSET=K
      K=K+1
C--------------------------
      KEY0(K)='CHECK'
      LKEY0(K)=5
      KCHECK_STARTER=K
      K=K+1
C-------------------------------------------------------------------
      KEY0(K)='END'
      LKEY0(K)=3
      KEND=K
      NOPT=K
      IF(NOPT>MAXNOPT)STOP 999
C-------------------------------------------------------------------
C
      CART=' '

      DO KOPT=1,NOPT
        KOPTAD(KOPT)=0
        NLINE(KOPT)=0
        NSLASH(KOPT+2)=0
      ENDDO
      KOPTAD(1)=1
      KOPTAD(2)=1

      RETURN
 999  CALL FREERR(1)
      RETURN
      END
C
Chd|====================================================================
Chd|  FREDEC0                       source/starter/freform.F      
Chd|-- called by -----------
Chd|        LECIG3D                       source/elements/ige3d/lecig3d.F
Chd|        NBADIGEMESH                   source/elements/ige3d/nbadigemesh.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FREDEC0(ID)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
      INTEGER IOP,ID
      CHARACTER MOT1*ncharfield
      INTEGER I,J1,J2
C-----------------------------------------------
C   /KEYW/int_id
C-----------------------------------------------
      I=2
      DO WHILE(KLINE(I:I)/='/')
        I=I+1
        IF(I>ncharline)CALL FREERR(0)
      ENDDO
      I=I+1
      IF(I>ncharline)CALL FREERR(0)
      J1=I
      MOT1=KLINE(J1:J1-1+LFIELD)
      READ(MOT1,ERR=999,FMT=FMT_I)ID
      IF (ID>ID_LIMIT.OR.ID<=0) THEN
        CALL ANCMSG(MSGID=510,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              I1=ID,C1=KLINE)
      ENDIF
C
      RETURN
 999  CALL FREERR(0)
      CALL MY_EXIT(2)
      END
C
Chd|====================================================================
Chd|  FREDEC4                       source/starter/freform.F      
Chd|-- called by -----------
Chd|        PREREAD_INIVEL                source/initial_conditions/general/inivel/inivel.F
Chd|-- calls ---------------
Chd|        FREERR                        source/starter/freform.F      
Chd|====================================================================
      SUBROUTINE FREDEC4(COPT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
C
      CHARACTER COPT*ncharfield
C
      INTEGER I,J1,J2,J
C-----------------------------------------------
C   /KEYW/int_type/int_id
C-----------------------------------------------
      I=2
      DO WHILE(KLINE(I:I)/='/')
        I=I+1

        IF(I>ncharline)CALL FREERR(0)
      ENDDO
      I=I+1

      IF(I>ncharline)CALL FREERR(0)
      J1=I
      DO WHILE(KLINE(I:I)/='/')
        I=I+1
        IF(I>ncharline)CALL FREERR(0)

      ENDDO
      J2=I-1
      COPT=KLINE(J1:J2)
C
      RETURN
      END
Chd|====================================================================
Chd|  FREDEC5                       source/starter/freform.F      
Chd|-- called by -----------
Chd|        YCTRL                         source/initial_conditions/inista/yctrl.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FREDEC5(COPT,ID)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
      INTEGER ID
      CHARACTER COPT*ncharkey
C-----------------------------------------------
      CHARACTER MOT1*ncharfield
      INTEGER I,J1,J2,J
C-----------------------------------------------
C   /KEYW    ou
C   /KEYW/KEYW2/int_id
C-----------------------------------------------
      I=2

      DO WHILE(KLINE(I:I)/='/'.AND.I<ncharline)
        I=I+1
      ENDDO
      COPT=' '
      I=I+1

      IF(I>ncharline)RETURN
      J1=I

      DO WHILE(KLINE(I:I)/='/'.AND.I<ncharline)
        I=I+1
      ENDDO
      J2=I-1
      COPT=KLINE(J1:J2)
C
      ID=0
      I=I+1
      I=MIN(I,ncharline)
      IF(I>ncharline-LFIELD+1)RETURN
      J1=I
      DO WHILE(KLINE(I:I)/='/')
        I=I+1
	      IF(I>ncharline)EXIT
      ENDDO
      J2=I-1

      J2=MIN(I-1+LFIELD,J2)
      MOT1=KLINE(J1:J2)

      READ(MOT1,ERR=999,FMT=FMT_I)ID
C
      IF (ID>ID_LIMIT.OR.ID<=0) THEN
        CALL ANCMSG(MSGID=510,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              I1=ID,C1=KLINE)
      ENDIF

C
      RETURN
 999  CALL FREERR(0)
      CALL MY_EXIT(2)
      END
Chd|====================================================================
Chd|  FREDEC6                       source/starter/freform.F      
Chd|-- called by -----------
Chd|        YCTRL                         source/initial_conditions/inista/yctrl.F
Chd|-- calls ---------------
Chd|        FREERR                        source/starter/freform.F      
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|====================================================================
      SUBROUTINE FREDEC6(COPT,COPT2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
      CHARACTER*ncharkey COPT,COPT2
C-----------------------------------------------
      INTEGER I,J1,J2,J
C-----------------------------------------------
C   /KEYW/KEYW2/KEYW3
C-----------------------------------------------
      I=2

      DO WHILE(KLINE(I:I)/='/'.AND.I<ncharline)
        I=I+1
      ENDDO
      COPT=' '
      I=I+1

      IF(I>ncharline)RETURN
      J1=I

      DO WHILE(KLINE(I:I)/='/'.AND.I<ncharline)
        I=I+1
      ENDDO
      J2=I-1
      COPT=KLINE(J1:J2)
C
      COPT2=' '
      I=I+1
      I=MIN(I,ncharline)
      J1=I

      DO WHILE(KLINE(I:I)/='/'.AND.I<ncharline)
        I=I+1
      ENDDO
    
      IF(I>ncharline)RETURN
      J2=I-1
      COPT2=KLINE(J1:J2)
C
      RETURN
 999  CALL FREERR(0)
      CALL MY_EXIT(2)
      END
Chd|====================================================================
Chd|  NODGRNR5                      source/starter/freform.F      
Chd|-- called by -----------
Chd|        HM_READ_CLOAD                 source/loads/general/cload/hm_read_cload.F
Chd|        HM_READ_CYLJOINT              source/constraints/general/cyl_joint/hm_read_cyljoint.F
Chd|        HM_READ_GRAV                  source/loads/general/grav/hm_read_grav.F
Chd|        HM_READ_IMPACC                source/constraints/general/impvel/hm_read_impacc.F
Chd|        HM_READ_IMPTEMP               source/constraints/thermic/hm_read_imptemp.F
Chd|        HM_READ_LOAD_CENTRI           source/loads/general/load_centri/hm_read_load_centri.F
Chd|        HM_READ_MERGE                 source/constraints/general/merge/hm_read_merge.F
Chd|        HM_READ_XELEM                 source/elements/reader/hm_read_xelem.F
Chd|        LECSEC42                      source/tools/sect/hm_read_sect.F
Chd|        LECSEC4BOLT                   source/tools/sect/lecsec4bolt.F
Chd|        READ_DFS_DETCORD              source/initial_conditions/detonation/read_dfs_detcord.F
Chd|        READ_DFS_WAVE_SHAPER          source/initial_conditions/detonation/read_dfs_wave_shaper.F
Chd|        READ_IMPDISP                  source/constraints/general/impvel/read_impdisp.F
Chd|        READ_IMPVEL                   source/constraints/general/impvel/read_impvel.F
Chd|        READ_IMPVEL_LAGMUL            source/constraints/general/impvel/read_impvel_lagmul.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      INTEGER FUNCTION NODGRNR5(IGU    ,IGS    ,IBUF,IGRNOD,
     .                          ITABM1 ,MESS   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
      INTEGER IGU,IGS,IBUF(*),ITABM1(*)
      CHARACTER MESS*40
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
      INTEGER I
C=======================================================================
      NODGRNR5 = 0
      IF (IGU > 0) THEN
        IGS=0
        DO I=1,NGRNOD
          IF(IGRNOD(I)%ID == IGU) THEN
            IGS=I
            NODGRNR5 = IGRNOD(IGS)%NENTITY
            EXIT
          ENDIF
        ENDDO
C
        IF (IGS == 0)THEN
           CALL ANCMSG(MSGID=53,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 C1= MESS,
     .                 I1=IGU)
           RETURN
        ENDIF
C
        DO I=1,NODGRNR5
          IBUF(I)=IGRNOD(IGS)%ENTITY(I)
        ENDDO
      ENDIF
C---
      RETURN
      END
Chd|====================================================================
Chd|  NODGRNR6                      source/starter/freform.F      
Chd|-- called by -----------
Chd|        HM_READ_RBE2                  source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        HM_READ_RBODY                 source/constraints/general/rbody/hm_read_rbody.F
Chd|        HM_READ_RBODY_LAGMUL          source/constraints/general/rbody/hm_read_rbody_lagmul.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      INTEGER FUNCTION NODGRNR6(M      ,IGU    ,IGS    ,IBUF,IGRNOD,
     .                          ITABM1 ,MESS   ,ID     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C  THIS FUNCTION REMOVES main NODE FROM SECND NODAL SET
C  IN THE KINEMATIC CONSTRAINT DEFINITION 
C-----------------------------------------------
      INTEGER M,IGU,IGS,ID,IBUF(*),ITABM1(*)
      CHARACTER MESS*40
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
      INTEGER I, MFLAG
C=======================================================================
      NODGRNR6 = 0
      IF (IGU > 0) THEN
        IGS=0
        DO I=1,NGRNOD
          IF(IGRNOD(I)%ID == IGU) THEN
            IGS=I
            NODGRNR6 = IGRNOD(IGS)%NENTITY
            EXIT
          ENDIF
        ENDDO
C
        IF (IGS == 0)THEN
           CALL ANCMSG(MSGID=53,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 C1= MESS,
     .                 I1=IGU)
           RETURN
        ENDIF
C
        MFLAG=0
        DO I=1,NODGRNR6
          IF(IGRNOD(IGS)%ENTITY(I)==M) THEN
            MFLAG=1
          ELSE
            IBUF(I-MFLAG)=IGRNOD(IGS)%ENTITY(I)
          ENDIF
        ENDDO
        IF(MFLAG==1) THEN 
          NODGRNR6=NODGRNR6-1
          CALL ANCMSG(MSGID=1624,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID)
        ENDIF
      ENDIF
C---
      RETURN
      END
Chd|====================================================================
Chd|  GRFIND                        source/starter/freform.F      
Chd|-- called by -----------
Chd|        R2R_CHECK                     source/coupling/rad2rad/r2r_check.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      INTEGER FUNCTION GRFIND(IGU,IGRNOD,MESS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
      INTEGER IGU
      CHARACTER MESS*40
C-----------------------------------------------
      TYPE (GROUP_)  ,DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
      INTEGER I,IG
C-----------------------------------------------
      IG=0
      DO I=1,NGRNOD
        IF(IGRNOD(I)%ID==IGU)THEN
          IG=I
        ENDIF
      ENDDO
C
      IF(IG==0)THEN
        CALL ANCMSG(MSGID=53,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=IGU,
     .              C1='IN NODE GROUP SEARCH')
        RETURN                  
      ENDIF
      GRFIND = IG
C----
      RETURN
      END
C
C
Chd|====================================================================
Chd|  FREERR                        source/starter/freform.F      
Chd|-- called by -----------
Chd|        ADMLCNT                       source/model/remesh/admlist.F 
Chd|        ADMLIST                       source/model/remesh/admlist.F 
Chd|        BIGBOX                        source/model/box/bigbox.F     
Chd|        BIGBOX2                       source/model/box/bigbox.F     
Chd|        BIGSBOX                       source/model/box/bigbox.F     
Chd|        BOXBOX                        source/model/box/boxbox.F     
Chd|        BOXBOX2                       source/model/box/boxbox.F     
Chd|        BOXBOXS                       source/model/box/boxbox.F     
Chd|        BUILD_ADMESH                  source/model/remesh/build_admesh.F
Chd|        CONTRL                        source/starter/contrl.F       
Chd|        ELSTAGBOX                     source/model/box/bigbox.F     
Chd|        ELSTAGBOX2                    source/model/box/rdbox.F      
Chd|        ELSTAGBOXBOX                  source/model/box/boxbox.F     
Chd|        FACEBOX                       source/model/box/bigbox.F     
Chd|        FREDEC0                       source/starter/freform.F      
Chd|        FREDEC4                       source/starter/freform.F      
Chd|        FREDEC5                       source/starter/freform.F      
Chd|        FREDEC6                       source/starter/freform.F      
Chd|        FREDEC_2KEY_4ID               source/starter/freform.F      
Chd|        FREDEC_2KEY_4ID_T             source/starter/freform.F      
Chd|        FREDEC_2KEY_ID_OR_KEY_ID      source/starter/freform.F      
Chd|        FREDEC_KEY_3ID_T              source/starter/freform.F      
Chd|        FREFORM                       source/starter/freform.F      
Chd|        HM_READ_EIG                   source/general_controls/computation/hm_read_eig.F
Chd|        HM_READ_FXB2                  source/constraints/fxbody/hm_read_fxb.F
Chd|        HM_READ_INIVEL                source/initial_conditions/general/inivel/hm_read_inivel.F
Chd|        HM_READ_INJECT1               source/properties/injector/hm_read_inject1.F
Chd|        HM_READ_INJECT2               source/properties/injector/hm_read_inject2.F
Chd|        HM_READ_INTER_TYPE24          source/interfaces/int24/hm_read_inter_type24.F
Chd|        HM_READ_MAT02                 source/materials/mat/mat002/hm_read_mat02.F
Chd|        HM_READ_MONVOL_TYPE1          source/airbag/hm_read_monvol_type1.F
Chd|        HM_READ_MONVOL_TYPE10         source/airbag/hm_read_monvol_type10.F
Chd|        HM_READ_MONVOL_TYPE11         source/airbag/hm_read_monvol_type11.F
Chd|        HM_READ_MONVOL_TYPE2          source/airbag/hm_read_monvol_type2.F
Chd|        HM_READ_MONVOL_TYPE3          source/airbag/hm_read_monvol_type3.F
Chd|        HM_READ_MONVOL_TYPE4          source/airbag/hm_read_monvol_type4.F
Chd|        HM_READ_MONVOL_TYPE5          source/airbag/hm_read_monvol_type5.F
Chd|        HM_READ_MONVOL_TYPE6          source/airbag/hm_read_monvol_type6.F
Chd|        HM_READ_MONVOL_TYPE7          source/airbag/hm_read_monvol_type7.F
Chd|        HM_READ_MONVOL_TYPE8          source/airbag/hm_read_monvol_type8.F
Chd|        HM_READ_MONVOL_TYPE9          source/airbag/hm_read_monvol_type9.F
Chd|        HM_READ_PLOAD                 source/loads/general/pload/hm_read_pload.F
Chd|        HM_READ_THGRNS                source/output/th/hm_read_thgrns.F
Chd|        HM_SETFXRBYON                 source/constraints/fxbody/hm_setfxrbyon.F
Chd|        INIVEL                        source/initial_conditions/general/inivel/inivel.F
Chd|        LECG47                        source/properties/ige3d/lecgeo47.F
Chd|        LECIG3D                       source/elements/ige3d/lecig3d.F
Chd|        LECTRANS                      source/model/transformation/lectrans.F
Chd|        NBADIGEMESH                   source/elements/ige3d/nbadigemesh.F
Chd|        NEXTSLA                       source/starter/freform.F      
Chd|        PRELECIG3D                    source/elements/ige3d/prelecig3d.F
Chd|        R2R_MONVOL                    source/coupling/rad2rad/r2r_prelec.F
Chd|        R2R_PRELEC                    source/coupling/rad2rad/r2r_prelec.F
Chd|        SBOXBOXSURF                   source/model/box/bigbox.F     
Chd|        YCTRL                         source/initial_conditions/inista/yctrl.F
Chd|        R2R_LISTCNT                   source/coupling/rad2rad/routines_r2r.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FREERR(IT)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IT1
C=======================================================================
	IT1 = IT
      IF(IT1==3)THEN
        IT1=0
        READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
        IT1=1
 999    CONTINUE
      ENDIF
      IF(IT1==0)THEN
          CALL ANCMSG(MSGID=54,
     .                ANMODE=ANINFO,
     .                MSGTYPE=MSGERROR,
     .                C1=KLINE)
      ELSEIF(IT1==1)THEN
          CALL ANCMSG(MSGID=55,
     .                ANMODE=ANINFO,
     .                MSGTYPE=MSGERROR,
     .                C1=KEY0(KCUR),
     .                C2=KLINE,
     .                C3=LINE)
      ENDIF
      END
Chd|====================================================================
Chd|  FRETITL                       source/starter/freform.F      
Chd|-- called by -----------
Chd|        HM_PREREAD_BCSCYC             source/constraints/general/bcs/lecbcscyc.F
Chd|        HM_PREREAD_SPHIO              source/loads/sph/hm_preread_sphio.F
Chd|        HM_READ_BCS                   source/constraints/general/bcs/hm_read_bcs.F
Chd|        HM_READ_CLUSTER               source/output/cluster/hm_read_cluster.F
Chd|        HM_READ_CYLJOINT              source/constraints/general/cyl_joint/hm_read_cyljoint.F
Chd|        HM_READ_FRICTION              source/interfaces/friction/reader/hm_read_friction.F
Chd|        HM_READ_FRM                   source/tools/skew/hm_read_frm.F
Chd|        HM_READ_FUNCT                 source/tools/curve/hm_read_funct.F
Chd|        HM_READ_FXB1                  source/constraints/fxbody/hm_read_fxb.F
Chd|        HM_READ_GAUGE                 source/output/gauge/hm_read_gauge.F
Chd|        HM_READ_GJOINT                source/constraints/general/gjoint/hm_read_gjoint.F
Chd|        HM_READ_INTER_FSI             source/interfaces/reader/hm_read_inter_fsi.F
Chd|        HM_READ_INTER_LAGDT_TYPE07    source/interfaces/int07/hm_read_inter_lagdt_type07.F
Chd|        HM_READ_INTER_LAGMUL          source/interfaces/reader/hm_read_inter_lagmul.F
Chd|        HM_READ_INTER_STRUCT          source/interfaces/reader/hm_read_inter_struct.F
Chd|        HM_READ_INTSUB                source/output/subinterface/hm_read_intsub.F
Chd|        HM_READ_LINK                  source/constraints/rigidlink/hm_read_rlink.F
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|        HM_READ_MERGE                 source/constraints/general/merge/hm_read_merge.F
Chd|        HM_READ_MPC                   source/constraints/general/mpc/hm_read_mpc.F
Chd|        HM_READ_NBCS                  source/constraints/general/bcs/hm_read_nbcs.F
Chd|        HM_READ_PART                  source/model/assembling/hm_read_part.F
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|        HM_READ_RBE2                  source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        HM_READ_RBE3                  source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        HM_READ_RBODY                 source/constraints/general/rbody/hm_read_rbody.F
Chd|        HM_READ_RBODY_LAGMUL          source/constraints/general/rbody/hm_read_rbody_lagmul.F
Chd|        HM_READ_RETRACTOR             source/tools/seatbelts/hm_read_retractor.F
Chd|        HM_READ_RWALL_CYL             source/constraints/general/rwall/hm_read_rwall_cyl.F
Chd|        HM_READ_RWALL_LAGMUL          source/constraints/general/rwall/hm_read_rwall_lagmul.F
Chd|        HM_READ_RWALL_PARAL           source/constraints/general/rwall/hm_read_rwall_paral.F
Chd|        HM_READ_RWALL_PLANE           source/constraints/general/rwall/hm_read_rwall_plane.F
Chd|        HM_READ_RWALL_SPHER           source/constraints/general/rwall/hm_read_rwall_spher.F
Chd|        HM_READ_RWALL_THERM           source/constraints/general/rwall/hm_read_rwall_therm.F
Chd|        HM_READ_SKW                   source/tools/skew/hm_read_skw.F
Chd|        HM_READ_SLIPRING              source/tools/seatbelts/hm_read_slipring.F
Chd|        HM_READ_SPCND                 source/constraints/sph/hm_read_spcnd.F
Chd|        HM_READ_THGRNE                source/output/th/hm_read_thgrne.F
Chd|        HM_READ_THGRNS                source/output/th/hm_read_thgrns.F
Chd|        HM_READ_THGROU                source/output/th/hm_read_thgrou.F
Chd|        HM_READ_THGRSENS              source/output/th/hm_read_thgrsens.F
Chd|        HM_READ_THGRSURF              source/output/th/hm_read_thgrsurf.F
Chd|        HM_READ_THPART                source/output/thpart/hm_read_thpart.F
Chd|        HM_THGRKI_VENT                source/output/th/hm_thgrki_vent.F
Chd|        LECACC                        source/tools/accele/lecacc.F  
Chd|        LECSEC42                      source/tools/sect/hm_read_sect.F
Chd|        LECSTACK_PLY                  source/properties/composite_options/stack/lecstack_ply.F
Chd|        PRELECSEC                     source/tools/sect/prelecsec.F 
Chd|        PRELECSEC4BOLT                source/tools/sect/prelecsec4bolt.F
Chd|        R2R_VOID                      source/coupling/rad2rad/r2r_void.F
Chd|        READ_IMPDISP                  source/constraints/general/impvel/read_impdisp.F
Chd|        READ_IMPDISP_FGEO             source/constraints/general/impvel/read_impdisp_fgeo.F
Chd|        READ_IMPVEL                   source/constraints/general/impvel/read_impvel.F
Chd|        READ_IMPVEL_FGEO              source/constraints/general/impvel/read_impvel_fgeo.F
Chd|        READ_IMPVEL_LAGMUL            source/constraints/general/impvel/read_impvel_lagmul.F
Chd|        READ_MONVOL                   source/airbag/read_monvol.F   
Chd|        W_GR_ENTITY_E                 source/restart/ddsplit/w_gr_entity.F
Chd|        W_GR_ENTITY_N                 source/restart/ddsplit/w_gr_entity.F
Chd|        W_GR_ENTITY_P                 source/restart/ddsplit/w_gr_entity.F
Chd|        W_ISLIN_STR                   source/restart/ddsplit/w_islin_str.F
Chd|        W_ISURF_STR                   source/restart/ddsplit/w_isurf_str.F
Chd|        W_SUBSET_STR                  source/restart/ddsplit/w_subset_str.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FRETITL(TITR,IASC,L)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y  A r g u m e  n t s
C-----------------------------------------------
      INTEGER L,IASC(*)
      CHARACTER TITR*(*)
C-----------------------------------------------
C   L o c a l    V a r i a b  l e s
C-----------------------------------------------
      INTEGER I,J, JMAX
C-----------------------------------------------
      J=1
      JMAX=MIN(ncharline,LEN(TITR))
      DO I=1,L
        IASC(I)=  ICHAR(TITR(J:J))*65536
        J = J+1
        IF(J>JMAX) EXIT                          ! Replacing ncharline by JMAX
        IASC(I)=  IASC(I) + ICHAR(TITR(J:J))*256
        J = J+1
        IF(J>JMAX) EXIT
        IASC(I)=  IASC(I) + ICHAR(TITR(J:J))
        J = J+1
        IF(J>JMAX) EXIT
      ENDDO
      RETURN
      END
Chd|====================================================================
Chd|  FRETITL2                      source/starter/freform.F      
Chd|-- called by -----------
Chd|        ADDMASPART                    source/tools/admas/addmaspart.F
Chd|        ALE_EULER_INIT                source/materials/ale/ale_euler_init.F
Chd|        BSIGINI                       source/elements/beam/bsigini.F
Chd|        BUSERINI                      source/elements/beam/buserini.F
Chd|        C3GRHEAD                      source/elements/sh3n/coque3n/c3grhead.F
Chd|        C3GRTAILS                     source/elements/sh3n/coque3n/c3grtails.F
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        C3INMAS                       source/elements/sh3n/coque3n/c3inmas.F
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CGRTAILS                      source/elements/shell/coque/cgrtails.F
Chd|        CHECKMP                       source/elements/initia/initia.F
Chd|        CHECKRBY                      source/constraints/general/rbody/checkrby.F
Chd|        CHEKMP2                       source/elements/initia/initia.F
Chd|        CHKFUNCT                      source/tools/curve/lecfun.F   
Chd|        CHKTYP2                       source/interfaces/interf1/chktyp2.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|        CINMAS                        source/elements/shell/coque/cinmas.F
Chd|        CM27IN3                       source/materials/mat/mat027/cm27in3.F
Chd|        CMATINI4                      source/materials/mat_share/cmatini4.F
Chd|        CORTHINI                      source/elements/shell/coque/corthini.F
Chd|        CSIGINI                       source/elements/shell/coque/csigini.F
Chd|        CSIGINI4                      source/elements/shell/coqueba/scigini4.F
Chd|        CUSERINI4                     source/elements/shell/coqueba/cuserini4.F
Chd|        DESOUT                        source/output/outp/desout.F   
Chd|        ECRSUB2                       source/model/assembling/hm_read_subset.F
Chd|        FSDCOD                        source/system/fsdcod.F        
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|        HIREORBE3                     source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        HM_READ_EOS                   source/materials/eos/hm_read_eos.F
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|        HM_READ_FXB1                  source/constraints/fxbody/hm_read_fxb.F
Chd|        HM_READ_INISTATE_D00          source/elements/initia/hm_read_inistate_d00.F
Chd|        HM_READ_INTSUB                source/output/subinterface/hm_read_intsub.F
Chd|        HM_READ_MERGE                 source/constraints/general/merge/hm_read_merge.F
Chd|        HM_READ_NONLOCAL              source/materials/nonlocal/hm_read_nonlocal.F
Chd|        HM_READ_PART                  source/model/assembling/hm_read_part.F
Chd|        HM_READ_PROP04                source/properties/spring/hm_read_prop04.F
Chd|        HM_READ_PROP09                source/properties/shell/hm_read_prop09.F
Chd|        HM_READ_PROP10                source/properties/shell/hm_read_prop10.F
Chd|        HM_READ_PROP11                source/properties/shell/hm_read_prop11.F
Chd|        HM_READ_PROP16                source/properties/shell/hm_read_prop16.F
Chd|        HM_READ_PROP17                source/properties/shell/hm_read_prop17.F
Chd|        HM_READ_PROP19                source/properties/shell/hm_read_prop19.F
Chd|        HM_READ_PROP26                source/properties/spring/hm_read_prop26.F
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|        HM_READ_PROP_GENERIC          source/properties/hm_read_prop_generic.F
Chd|        HM_READ_RBODY                 source/constraints/general/rbody/hm_read_rbody.F
Chd|        HM_READ_SPHCEL                source/elements/reader/hm_read_sphcel.F
Chd|        HM_READ_SUBSET                source/model/assembling/hm_read_subset.F
Chd|        HM_READ_THERM                 source/materials/therm/hm_read_therm.F
Chd|        HM_READ_THERM_STRESS          source/materials/therm/hm_read_therm_stress.F
Chd|        HM_READ_THGRKI                source/output/th/hm_read_thgrki.F
Chd|        HM_READ_THGRKI_RBODY          source/output/th/hm_read_thgrki_rbody.F
Chd|        HM_READ_THGRNE                source/output/th/hm_read_thgrne.F
Chd|        HM_READ_THGRNS                source/output/th/hm_read_thgrns.F
Chd|        HM_READ_THGRSENS              source/output/th/hm_read_thgrsens.F
Chd|        HM_READ_THGRSURF              source/output/th/hm_read_thgrsurf.F
Chd|        HM_READ_XREF                  source/loads/reference_state/xref/hm_read_xref.F
Chd|        HM_THGRKI_VENT                source/output/th/hm_thgrki_vent.F
Chd|        INIGUSER                      source/system/iniguser.F      
Chd|        ININTR                        source/interfaces/interf1/inintr.F
Chd|        ININTR2                       source/interfaces/inter3d1/inintr2.F
Chd|        ININTR_THKVAR                 source/interfaces/interf1/inintr_thkvar.F
Chd|        ININTSUB                      source/interfaces/interf1/inintsub.F
Chd|        ININTSUB_11                   source/output/subinterface/inintsub_11.F
Chd|        ININTSUB_25                   source/output/subinterface/inintsub_25.F
Chd|        ININTSUB_7                    source/output/subinterface/inintsub_7.F
Chd|        INIRBE2                       source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        INIRBE3                       source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        INITIA                        source/elements/initia/initia.F
Chd|        INIVOID                       source/elements/initia/inivoid.F
Chd|        INI_FXBODY                    source/constraints/fxbody/ini_fxbody.F
Chd|        INTER_DCOD_FRICTION           source/interfaces/reader/inter_dcod_friction.F
Chd|        INTER_DCOD_FUNCTION           source/interfaces/reader/inter_dcod_function.F
Chd|        INTER_DCOD_SENSOR             source/interfaces/reader/inter_dcod_sensor.F
Chd|        ITAGSL2                       source/interfaces/inter3d1/itagsl2.F
Chd|        KINCHK                        source/constraints/general/kinchk.F
Chd|        LCGEO19                       source/elements/shell/coque/lcgeo19.F
Chd|        LECINS                        source/interfaces/interf1/lecins.F
Chd|        LECINT                        source/interfaces/interf1/lecint.F
Chd|        LECSTACK_PLY                  source/properties/composite_options/stack/lecstack_ply.F
Chd|        LECSTAMP                      source/interfaces/interf1/lecstamp.F
Chd|        LGMINI_BC                     source/tools/lagmul/lgmini_bc.F
Chd|        LGMINI_FXV                    source/tools/lagmul/lgmini_fxv.F
Chd|        LGMINI_GJ                     source/tools/lagmul/lgmini_gj.F
Chd|        LGMINI_I2                     source/tools/lagmul/lgmini_i2.F
Chd|        LGMINI_I7                     source/tools/lagmul/lgmini_i7.F
Chd|        LGMINI_MPC                    source/tools/lagmul/lgmini_mpc.F
Chd|        LGMINI_RBY                    source/tools/lagmul/lgmini_rby.F
Chd|        LGMINI_RWL                    source/tools/lagmul/lgmini_rwl.F
Chd|        M20DCOD                       source/system/fsdcod.F        
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|        MULTIFLUID_INIT3              source/multifluid/multifluid_init3.F
Chd|        NEWDBL2                       source/system/sysfus.F        
Chd|        OUTPART                       source/elements/initia/initia.F
Chd|        OUTPART5                      source/elements/initia/initia.F
Chd|        PGRTAILS                      source/elements/beam/pgrtails.F
Chd|        PORNOD                        source/ale/pornod.F           
Chd|        Q4INIT2                       source/elements/solid_2d/quad4/q4init2.F
Chd|        QGRHEAD                       source/elements/solid_2d/quad/qgrhead.F
Chd|        QGRTAILS                      source/elements/solid_2d/quad/qgrtails.F
Chd|        QINIT2                        source/elements/solid_2d/quad/qinit2.F
Chd|        QMORTH2                       source/elements/solid_2d/quad/qmorth2.F
Chd|        RCHECKMASS                    source/elements/spring/rcheckmass.F
Chd|        REMN_I2                       source/interfaces/inter3d1/i7remnode.F
Chd|        REMN_I2OP                     source/interfaces/inter3d1/i7remnode.F
Chd|        REMN_I2OP_EDG25               source/interfaces/int25/i25remlin.F
Chd|        RI2_INT24P_INI                source/interfaces/inter3d1/i7remnode.F
Chd|        RIGID_MAT                     source/materials/mat/mat019/rigid_mat.F
Chd|        RINI33_RB                     source/elements/joint/rjoint/rini33_rb.F
Chd|        RINI45_RB                     source/elements/joint/rjoint/rini45_rb.F
Chd|        RINIT3                        source/elements/spring/rinit3.F
Chd|        RKINI3                        source/elements/spring/rkini3.F
Chd|        S10INIT3                      source/elements/solid/solide10/s10init3.F
Chd|        S16INIT3                      source/elements/thickshell/solide16/s16init3.F
Chd|        S20INIT3                      source/elements/solid/solide20/s20init3.F
Chd|        S4INIT3                       source/elements/solid/solide4/s4init3.F
Chd|        S4REFSTA3                     source/elements/solid/solide4/s4refsta3.F
Chd|        S6CINIT3                      source/elements/thickshell/solide6c/s6cinit3.F
Chd|        S8CINIT3                      source/elements/thickshell/solide8c/s8cinit3.F
Chd|        S8ZINIT3                      source/elements/solid/solide8z/s8zinit3.F
Chd|        SCALEINI                      source/elements/initia/scaleini.F
Chd|        SCINIT3                       source/elements/thickshell/solidec/scinit3.F
Chd|        SCMORTH3                      source/elements/thickshell/solidec/scmorth3.F
Chd|        SGRTAILS                      source/elements/solid/solide/sgrtails.F
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|        SMORTH3                       source/elements/solid/solide/smorth3.F
Chd|        SMS_INIT                      source/ams/sms_init.F         
Chd|        SPGRTAILS                     source/elements/sph/spgrtails.F
Chd|        SPHDCOD                       source/elements/sph/sphdcod.F 
Chd|        ST_QAPRINT_CLUSTERS           source/output/qaprint/st_qaprint_clusters.F
Chd|        ST_QAPRINT_CONSTRAINTS        source/output/qaprint/st_qaprint_constraints.F
Chd|        ST_QAPRINT_FRICTION           source/output/qaprint/st_qaprint_friction.F
Chd|        ST_QAPRINT_INTERFACES         source/output/qaprint/st_qaprint_interfaces.F
Chd|        ST_QAPRINT_LOADS              source/output/qaprint/st_qaprint_loads.F
Chd|        ST_QAPRINT_MATERIALS          source/output/qaprint/st_qaprint_materials.F
Chd|        ST_QAPRINT_MODEL_TOOLS        source/output/qaprint/st_qaprint_model_tools.F
Chd|        ST_QAPRINT_OUTPUT_DATABASES   source/output/qaprint/st_qaprint_output_databases.F
Chd|        ST_QAPRINT_PROPERTIES         source/output/qaprint/st_qaprint_properties.F
Chd|        ST_QAPRINT_THGROU             source/output/qaprint/st_qaprint_time_histories.F
Chd|        SUINIT3                       source/elements/elbuf_init/suinit3.F
Chd|        T3GRHEAD                      source/elements/solid_2d/tria/t3grhead.F
Chd|        T3GRTAILS                     source/elements/solid_2d/tria/t3grtails.F
Chd|        THPRIN                        source/output/th/thprin.F     
Chd|        TINIT3                        source/elements/truss/tinit3.F
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|        WRITE_I_C_DEBUG               source/output/tools/write_debug.F
Chd|        XINIT3                        source/elements/xelem/xinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FRETITL2(TITR,IASC,L)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y  A r g u m e  n t s
C-----------------------------------------------
      INTEGER L,IASC(*)
      CHARACTER TITR*(*)
C-----------------------------------------------
C   L o c a l    V a r i a b  l e s
C-----------------------------------------------
      INTEGER I,J
C-----------------------------------------------
      J=1
      DO I=1,L
        TITR(J:J)=CHAR(IASC(I)/65536)
        J=J+1
        IF(J>nchartitle) EXIT
        TITR(J:J)=CHAR(MOD(IASC(I),65536)/256)
        J=J+1
        IF(J>nchartitle) EXIT
        TITR(J:J)=CHAR(MOD(IASC(I),256))
        J=J+1
        IF(J>nchartitle) EXIT
      ENDDO
      RETURN
      END
Chd|====================================================================
Chd|  NEXTSLA                       source/starter/freform.F      
Chd|-- called by -----------
Chd|        INIVEL                        source/initial_conditions/general/inivel/inivel.F
Chd|        PREREAD_INIVEL                source/initial_conditions/general/inivel/inivel.F
Chd|        R2R_PRELEC                    source/coupling/rad2rad/r2r_prelec.F
Chd|        SZ_R2R                        source/coupling/rad2rad/routines_r2r.F
Chd|-- calls ---------------
Chd|        FREERR                        source/starter/freform.F      
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|====================================================================
      SUBROUTINE NEXTSLA
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
      IF (IREC<=0) THEN
        IREC=1
        READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
      ELSE
        READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
        DO WHILE(LINE(1:1)/='/')
          IREC=IREC+1
          READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
        ENDDO
      END IF
      IF(LINE(2:1+LKEY0(KCUR))/=KEY0(KCUR)(1:LKEY0(KCUR)))GOTO 999
      KLINE=LINE
      RETURN
 999  CALL FREERR(1)
      CALL MY_EXIT(2)
      END
Chd|====================================================================
Chd|  FREDEC_2KEY_ID_OR_KEY_ID      source/starter/freform.F      
Chd|-- called by -----------
Chd|        YCTRL                         source/initial_conditions/inista/yctrl.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FREDEC_2KEY_ID_OR_KEY_ID(KEY2,KEY3,UID,SUB_ID)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
      INTEGER UID,SUB_ID
      CHARACTER TITR*nchartitle,KEY2*ncharkey,KEY3*ncharkey
C-----------------------------------------------
      INTEGER I,J1,J2,J3,J, JMAX
      CHARACTER MOT1*ncharfield
C-----------------------------------------------
C   /KEYW/KEY2/KEY3/u_id
C   /KEYW/KEY2/u_id
C   /KEYW/u_id
C-----------------------------------------------

      JMAX = ncharline

C     Pass KEY1
      UID=0
      SUB_ID = 0
      I=2
      DO WHILE(KLINE(I:I)/='/'.AND.I<ncharline)
        I=I+1
      ENDDO
      KEY2=' '
      KEY3=' '
      I=I+1
      IF(I>ncharline)RETURN
C     Read KEY2
      J1=I
      DO WHILE(KLINE(I:I)/='/'.AND.I<ncharline)
        I=I+1
      ENDDO
      IF (I>=ncharline) THEN
        MOT1=KLINE(J1:I)
          IF (KLINE(J1:MIN(JMAX,J1+2))/='AUX'     .AND.
     .        KLINE(J1:MIN(JMAX,J1+3))/='EPSP'    .AND.
     .        KLINE(J1:MIN(JMAX,J1+5))/='EPSP_F'  .AND.
     .        KLINE(J1:MIN(JMAX,J1+4))/='ORTHO'   .AND.
     .        KLINE(J1:MIN(JMAX,J1+5))/='STRA_F'  .AND.
     .        KLINE(J1:MIN(JMAX,J1+5))/='STRS_F'  .AND.
     .        KLINE(J1:MIN(JMAX,J1+4))/='THICK'   .AND.
     .        KLINE(J1:MIN(JMAX,J1+7))/='ORTH_LOC'.AND.
     .        KLINE(J1:MIN(JMAX,J1+5))/='STRESS'  .AND.  
     .        KLINE(J1:MIN(JMAX,J1+9))/='SCALE_YLD'.AND.
     .        KLINE(J1:MIN(JMAX,J1+4))/='FAIL'    .AND.  
     .        KLINE(J1:MIN(JMAX,J1+4))/='FILL'    .AND.
     .        KLINE(J1:MIN(JMAX,J1+4))/='FULL'    .AND.
     .        KLINE(J1:MIN(JMAX,J1+3))/='DENS'    .AND.
     .        KLINE(J1:MIN(JMAX,J1+3))/='EREF'    .AND.
     .        KLINE(J1:MIN(JMAX,J1+3))/='ENER'    ) THEN
           READ(MOT1,ERR=999,FMT=FMT_I)UID
           IF (UID > ID_LIMIT .OR. UID < 0) THEN
             CALL ANCMSG(MSGID=510,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                   I1=UID,C1=KLINE)
             RETURN
           ENDIF
           RETURN
          ENDIF  
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+2))=='AUX') THEN
        J2=J1+3
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+3))=='EPSP') THEN
        J2=J1+4
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+5))=='EPSP_F') THEN
        J2=J1+6
      ENDIF 
      IF (KLINE(J1:MIN(JMAX,J1+4))=='ORTHO') THEN
        J2=J1+5
      ENDIF   
      IF (KLINE(J1:MIN(JMAX,J1+8))=='STRA_FGLO') THEN
        J2=J1+6
      ELSEIF (KLINE(J1:MIN(JMAX,J1+5))=='STRA_F') THEN
        J2=J1+6
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+8))=='STRS_FGLO') THEN
        J2=J1+9
      ELSEIF (KLINE(J1:MIN(JMAX,J1+5))=='STRS_F') THEN
        J2=J1+6
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+4))=='THICK') THEN
        J2=J1+5
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+7))=='ORTH_LOC') THEN
        J2=J1+8
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+5))=='STRESS') THEN
        J2=J1+6
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+9))=='SCALE_YLD') THEN
        J2=J1+9
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+5))=='FAIL') THEN
        J2=J1+4
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+5))=='FILL') THEN
        J2=J1+4
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+5))=='FULL') THEN
        J2=J1+4
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+3))=='DENS') THEN
        J2=J1+4
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+3))=='ENER') THEN
        J2=J1+4
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+3))=='EREF') THEN
        J2=J1+4
      ENDIF
      KEY2=KLINE(J1:MIN(JMAX,J2-1))
C     Read KEY3
      DO WHILE(KLINE(I:I)/='/'.AND.I<ncharline)
        I=I+1
      ENDDO
      J2 = I+1
      J2 = MIN(JMAX,J2)
      IF (I>=ncharline) THEN
        MOT1=KLINE(J2:I)
        IF (KLINE(J2:MIN(JMAX,J2+5))/='STRA_F'.AND.
     .      KLINE(J2:MIN(JMAX,J2+5))/='STRS_F') THEN
          READ(MOT1,ERR=999,FMT=FMT_I)UID
          IF (UID > ID_LIMIT .OR. UID < 0) THEN
            CALL ANCMSG(MSGID=510,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=UID,C1=KLINE)
            RETURN
          ENDIF
          RETURN
        ENDIF
      ENDIF
      IF (KLINE(J2:MIN(JMAX,J2+3))=='GLOB') THEN
        J3=J2+3
        J3 = MIN(JMAX,J3)
        I = J3
        KEY3=KLINE(J2:J3)
        DO WHILE(KLINE(I:I)/='/' .AND. I<ncharline)
          I=I+1
        ENDDO
        I=I+1
        I=MIN(ncharline,I)
      ELSEIF (KLINE(J2:MIN(JMAX,J2+8))=='STRA_FGLO' .OR.
     .        KLINE(J2:MIN(JMAX,J2+8))=='STRS_FGLO'  ) THEN
        J3=J2+8
        J3 = MIN(JMAX,J3)
        I = J3
        KEY3=KLINE(J2:J3)
        DO WHILE(KLINE(I:I)/='/' .AND. I<ncharline)
          I=I+1
        ENDDO
        I=I+1
        I=MIN(JMAX,I)
      ELSEIF (KLINE(J2:MIN(JMAX,J2+5))=='STRA_F' .OR.
     .        KLINE(J2:MIN(JMAX,J2+5))=='STRS_F'  ) THEN
        J3=J2+5
        J3 = MIN(JMAX,J3)
        I = J3
        KEY3=KLINE(J2:J3)
        DO WHILE(KLINE(I:I)/='/' .AND. I<ncharline)
          I=I+1
        ENDDO
        I=I+1
        I = MIN(JMAX,I)
      ELSE
        J3=J2
        I=I+1
        I = MIN(JMAX,I)
        KEY3=' '
      ENDIF
C---  read UID
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I<ncharline)
        I=I+1
      ENDDO
      MOT1=KLINE(J1:I-1)
      READ(MOT1,ERR=999,FMT=FMT_I)UID
      IF (UID > ID_LIMIT .OR. UID < 0) THEN
        CALL ANCMSG(MSGID=510,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              I1=UID,C1=KLINE)
      ENDIF
C---  read SUB_ID
      I=I+1
      I = MIN(JMAX,I)
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I<ncharline)
        I=I+1
      ENDDO
      MOT1=KLINE(J1:I-1)
      READ(MOT1,ERR=999,FMT=FMT_I)SUB_ID
C---
      RETURN
 999  CALL FREERR(0)
      CALL MY_EXIT(2)
      END   
Chd|====================================================================
Chd|  FREDEC_2KEY_4ID_T             source/starter/freform.F      
Chd|-- called by -----------
Chd|        INIVEL                        source/initial_conditions/general/inivel/inivel.F
Chd|        NBADIGEMESH                   source/elements/ige3d/nbadigemesh.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FREDEC_2KEY_4ID_T(KEY2,ID,UID,VERS,SUB_ID,TITR)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
      INTEGER ID,UID,VERS,SUB_ID
      CHARACTER TITR*nchartitle,KEY2*ncharkey
C-----------------------------------------------
      INTEGER I,J1,J2,J
      CHARACTER MOT1*ncharfield
C-----------------------------------------------
C   /KEYW/KEY2/int_id/u_id/VERS
C + char_title
C-----------------------------------------------
C     Pass KEY1
      I=2
      DO WHILE(KLINE(I:I)/='/')
        I=I+1
        IF(I>ncharline)CALL FREERR(0)
      ENDDO
      I=I+1
C     Read KEY2
      J1=I
      DO WHILE(KLINE(I:I)/='/')
        I=I+1
        IF(I>ncharline)CALL FREERR(0)
      ENDDO
      J2=I-1
      KEY2=KLINE(J1:J2)
      I=I+1
C---  read ID
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I-J1 <= ncharfield)
        I=I+1
      ENDDO
      IF (I > ncharline) CALL FREERR(0)
      J2 = I-1
      MOT1=KLINE(J1:J2)
      READ(MOT1,ERR=999,FMT=FMT_I)ID
      IF (ID > ID_LIMIT .OR. ID <= 0) THEN
        CALL ANCMSG(MSGID=510,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              I1=ID,C1=KLINE)
      ENDIF
      I =I+1
C---  read UID
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I-J1 <= ncharfield)
        I=I+1
      ENDDO
      IF (I > ncharline) CALL FREERR(0)
      J2 = I-1
      MOT1=KLINE(J1:J2)
      READ(MOT1,ERR=999,FMT=FMT_I)UID
      IF (UID > ID_LIMIT .OR. UID < 0) THEN
        CALL ANCMSG(MSGID=510,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              I1=UID,C1=KLINE)
      ENDIF
      I =I+1
C---  read VERS
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I-J1 <= ncharfield)
        I=I+1
      ENDDO
      J2 = I-1
      MOT1=KLINE(J1:J2)
      READ(MOT1,ERR=999,FMT=FMT_I)VERS
C---  read SUB_ID
      I =I+1
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I-J1 <= ncharfield)
        I=I+1
      ENDDO
      J2 = I-1
      MOT1=KLINE(J1:J2)
      READ(MOT1,ERR=999,FMT=FMT_I)SUB_ID
C---  read TITR
      IREC=IREC+1                                
      READ(IIN,REC=IREC,ERR=999,FMT='(A)') TITR  
C---
      RETURN
 999  CALL FREERR(0)
      CALL MY_EXIT(2)
      END
Chd|====================================================================
Chd|  FREDEC_KEY_3ID_T              source/starter/freform.F      
Chd|-- called by -----------
Chd|        NBADIGEMESH                   source/elements/ige3d/nbadigemesh.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FREDEC_KEY_3ID_T(ID,UID,VERS,TITR)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
      INTEGER IOP,ID,UID,VERS
      CHARACTER MOT1*ncharfield,TITR*nchartitle
      INTEGER I,J1,J2
C-----------------------------------------------
C   /KEYW/int_id/uid/VERS
C-----------------------------------------------
      I=2
      DO WHILE(KLINE(I:I)/='/')
        I=I+1
        IF (I > ncharline)CALL FREERR(0)
      ENDDO
      I=I+1
      IF (I > ncharline)CALL FREERR(0)
C---  read ID
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I-J1 <= ncharfield)
        I=I+1
      ENDDO
      IF (I > ncharline) CALL FREERR(0)
      J2 = I-1
      MOT1=KLINE(J1:J2)
      READ(MOT1,ERR=999,FMT=FMT_I)ID
      IF (ID > ID_LIMIT .OR. ID <= 0) THEN
        CALL ANCMSG(MSGID=510,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              I1=ID,C1=KLINE)
      ENDIF
      I =I+1
C---  read UID
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I-J1 <= ncharfield)
        I=I+1
      ENDDO
      IF (I > ncharline) CALL FREERR(0)
      J2 = I-1
      MOT1=KLINE(J1:J2)
      READ(MOT1,ERR=999,FMT=FMT_I)UID
      IF (UID > ID_LIMIT .OR. UID < 0) THEN
        CALL ANCMSG(MSGID=510,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              I1=UID,C1=KLINE)
      ENDIF
      I =I+1
C---  read VERS
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I-J1 <= ncharfield)
        I=I+1
      ENDDO
      J2 = I-1
      MOT1=KLINE(J1:J2)
      READ(MOT1,ERR=999,FMT=FMT_I)VERS
C---  read TITR
      IREC=IREC+1                                
      READ(IIN,REC=IREC,ERR=999,FMT='(A)') TITR
C---
      RETURN
 999  CALL FREERR(0)
      CALL MY_EXIT(2)
      END
Chd|====================================================================
Chd|  FREDEC_2KEY_4ID               source/starter/freform.F      
Chd|-- called by -----------
Chd|        R2R_PRELEC                    source/coupling/rad2rad/r2r_prelec.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FREDEC_2KEY_4ID(KEY2,ID,UID,VERS,SUB_ID)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
      INTEGER ID,UID,VERS,SUB_ID
      CHARACTER TITR*nchartitle,KEY2*ncharkey
C-----------------------------------------------
      INTEGER I,J1,J2,J
      CHARACTER MOT1*ncharfield
C-----------------------------------------------
C   /KEYW/KEY2/int_id/u_id/VERS
C-----------------------------------------------
C     Pass KEY1
      I=2
      DO WHILE(KLINE(I:I)/='/')
        I=I+1
        IF(I>ncharline)CALL FREERR(0)
      ENDDO
      I=I+1
C     Read KEY2
      J1=I
      DO WHILE(KLINE(I:I)/='/')
        I=I+1
        IF(I>ncharline)CALL FREERR(0)
      ENDDO
      J2=I-1
      KEY2=KLINE(J1:J2)
      I=I+1
C---  read ID
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I-J1 <= ncharfield)
        I=I+1
      ENDDO
      IF (I > ncharline) CALL FREERR(0)
      J2 = I-1
      MOT1=KLINE(J1:J2)
      READ(MOT1,ERR=999,FMT=FMT_I)ID
      IF (ID > ID_LIMIT .OR. ID <= 0) THEN
        CALL ANCMSG(MSGID=510,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              I1=ID,C1=KLINE)
      ENDIF
      I =I+1
C---  read UID
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I-J1 <= ncharfield)
        I=I+1
      ENDDO
      IF (I > ncharline) CALL FREERR(0)
      J2 = I-1
      MOT1=KLINE(J1:J2)
      READ(MOT1,ERR=999,FMT=FMT_I)UID
      IF (UID > ID_LIMIT .OR. UID < 0) THEN
        CALL ANCMSG(MSGID=510,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              I1=UID,C1=KLINE)
      ENDIF
C---  read VERS
      I=I+1
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I-J1 <= ncharfield)
        I=I+1
      ENDDO
      J2=I-1
      MOT1=KLINE(J1:J2)
      READ(MOT1,ERR=999,FMT=FMT_I) VERS
C---  read SUB_ID
      I=I+1
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I-J1 <= ncharfield)
        I=I+1
      ENDDO
      J2=I-1
      MOT1=KLINE(J1:J2)
      READ(MOT1,ERR=999,FMT=FMT_I) SUB_ID
C---
      RETURN
 999  CALL FREERR(0)
      CALL MY_EXIT(2)
      END
