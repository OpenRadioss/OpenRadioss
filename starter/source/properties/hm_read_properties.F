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
Chd|  HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL                       source/starter/freform.F      
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_PROP_READ21                source/properties/thickshell/hm_read_prop21.F
Chd|        HM_READ_INJECT1               source/properties/injector/hm_read_inject1.F
Chd|        HM_READ_INJECT2               source/properties/injector/hm_read_inject2.F
Chd|        HM_READ_PROP0                 source/properties/void/hm_read_prop0.F
Chd|        HM_READ_PROP01                source/properties/shell/hm_read_prop01.F
Chd|        HM_READ_PROP02                source/properties/truss/hm_read_prop02.F
Chd|        HM_READ_PROP03                source/properties/beam/hm_read_prop03.F
Chd|        HM_READ_PROP04                source/properties/spring/hm_read_prop04.F
Chd|        HM_READ_PROP05                source/properties/rivet/hm_read_prop05.F
Chd|        HM_READ_PROP06                source/properties/solid/hm_read_prop06.F
Chd|        HM_READ_PROP08                source/properties/spring/hm_read_prop08.F
Chd|        HM_READ_PROP09                source/properties/shell/hm_read_prop09.F
Chd|        HM_READ_PROP10                source/properties/shell/hm_read_prop10.F
Chd|        HM_READ_PROP11                source/properties/shell/hm_read_prop11.F
Chd|        HM_READ_PROP12                source/properties/spring/hm_read_prop12.F
Chd|        HM_READ_PROP13                source/properties/spring/hm_read_prop13.F
Chd|        HM_READ_PROP14                source/properties/solid/hm_read_prop14.F
Chd|        HM_READ_PROP14F               source/properties/solid/hm_read_prop14.F
Chd|        HM_READ_PROP15                source/properties/solid/hm_read_prop15.F
Chd|        HM_READ_PROP16                source/properties/shell/hm_read_prop16.F
Chd|        HM_READ_PROP17                source/properties/shell/hm_read_prop17.F
Chd|        HM_READ_PROP18                source/properties/beam/hm_read_prop18.F
Chd|        HM_READ_PROP19                source/properties/shell/hm_read_prop19.F
Chd|        HM_READ_PROP20                source/properties/thickshell/hm_read_prop20.F
Chd|        HM_READ_PROP22                source/properties/thickshell/hm_read_prop22.F
Chd|        HM_READ_PROP23                source/properties/spring/hm_read_prop23.F
Chd|        HM_READ_PROP25                source/properties/spring/hm_read_prop25.F
Chd|        HM_READ_PROP26                source/properties/spring/hm_read_prop26.F
Chd|        HM_READ_PROP27                source/properties/spring/hm_read_prop27.F
Chd|        HM_READ_PROP51                source/properties/shell/hm_read_prop51.F
Chd|        HM_READ_PROP_GENERIC          source/properties/hm_read_prop_generic.F
Chd|        INI_PROP_VARS                 source/elements/elbuf_init/ini_prop_vars.F
Chd|        VDOUBLE                       source/system/sysfus.F        
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROPERTIES(GEO   ,X     ,IX   ,PM   ,ITABM1 ,
     .                  BUFGEO,LBUFGEO   ,ISKN ,IGEO ,IPM    ,
     .                  NPC   ,PLD   ,UNITAB ,RTRANS,LSUBMODEL  ,
     .                  PROP_TAG ,IPART ,KNOT,IDRAPEID,STACK_INFO,
     .                  NUMGEO_STACK,NPROP_STACK, MULTI_FVM,IADBUF)
C============================================================================
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ PROPERTIES WITH HM READER
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE STACK_MOD
      USE MULTI_FVM_MOD
      USE HM_OPTION_READ_MOD
      USE ALE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "tablen_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IX(*),ITABM1(*),LBUFGEO,ISKN(LISKN,*),
     .        IGEO(NPROPGI,*),IPM(NPROPMI,*),NPC(*),
     .        IPART(LIPART1,*),IDRAPEID(*),NUMGEO_STACK(NUMGEO+NUMSTACK),
     .        NPROP_STACK,IADBUF
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
      my_real GEO(NPROPG,NUMGEO), X(*), PM(NPROPM,NUMMAT),PLD(*),RTRANS(NTRANSF,*),KNOT(*)

      DOUBLE PRECISION BUFGEO(*)

      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(STACK_INFO_ ) , DIMENSION (NPROP_STACK) :: STACK_INFO
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER LAW_ID*4
      INTEGER I, PROP_ID, IGTYP, ISMSTR, NIP, J, IR1X, IR1Y, IR1Z, IREP,
     .   IR2X, IR2Y, IR2Z, ISHEAR, IRX, IROT, IMODE, IP, ISTRAIN,I8PT,
     .   ISK,ITU,IRB,IHON,IHBE,IPLAST,ITHK,IBID,IHBEOUTP,K,N,
     .   IGFLU, IDS, NSHELL, NSHSUP, NSHINF, FLGBADI, NBADI,IUNIT,UID,
     .   NSST_D, NSST_DS, NPSH, ICPRE, ICSTR ,NPTS,ISEN,ISORTH,
     .   PID1,IPID1, IHGFLU, IHBE_OLD,NSTACK,NN,NUMS
      INTEGER ISH3N,IFLAGUNIT,ICXFEM, IPPID,IPMAT,IPANG,IPTHK,
     .        IPPOS, JPID,N1,IPOS,ISROT,MLAWLY,MID,SUB_ID,
     .        PROP_SHELL,PROP_TSHEL,PROP_SOLID,PROP_SPH,PROP_BEAM,
     .        IAD_KNOT,IPDIR,ISTACK,PROP_TRUSS,PROP_SPRING,PROP_NSTRAND,IPINCH
     
      INTEGER JPID1,JPID2,NISUB,IPISUB,IFRAM,E_TYPE,OFF_DEF(NSUBMOD),CPT,
     .        CPT1,SUB_INDEX
      my_real FN, FT, DX, ANGL,PUN,HTEST,HCLOS,CVIS,RBID,VX,VY,VZ,
     .        FAC_L,FAC_T,FAC_M, TMIN,TMAX,DT,THICKT,IERREL,DN_P,ZSHIFT,PTHK
      CHARACTER IDTITL*ncharline, MESS*40,KEY*ncharline, KEY2*ncharkey,
     .          STRING*ncharfield,TITR1*nchartitle,
     .          CHROT*7,SOLVERKEYWORD*ncharline
      DATA NSHELL /0/, NSHSUP /0/, NSHINF /0/
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      DATA MESS/'PID DEFINITION                          '/
      DATA PUN/0.1/
C----------------------
C     GEO(3) : ISMSTR
C     GEO(5) : DT (ISMSTR=3 SAUF SOLIDES)
C     GEO(7) : VX  coques/solides ortho - vecteur de reference
C     GEO(8) : VY
C     GEO(9) : VZ
C     GEO(11): ISTRAIN (COQUE)
C     GEO(12): IGTYP      ->    IGEO(11)
C     GEO(35): ITHK
C     GEO(37): ISHEAR
C     GEO(38): FSHEAR
C     GEO(39): IPLAST
C     GEO(40): PROP_ID    v50  ->    IGEO(1)
C     GEO(42): PTHK 
C     GEO(43): FAILEXP 
C     GEO(20:34) : Milieu poreux (briques)
C     GEO(129): HCLOS (briques)
C     GEO(130): HTEST (briques)
C     GEO(131:170): LIBRE
C     GEO(171): IHBE
C     GEO(212): ANGLE BETWEEN two orthotropy directions (DIR1,DIR2) for 
C               the PID with LAW58
C-------------------
C    nouveau stockage:
C    IGEO(1)  : PROP_ID
C    IGEO(2)  : ISK
C    IGEO(3)  : ISEN
C    IGEO(4)  : NIP
C    IGEO(5)  : ISMSTR
C    IGEO(6)  : IREP
C    IGEO(7)  : ITHK
C    IGEO(8)  : ISHEAR
C    IGEO(9)  : IPLAST
C    IGEO(10) : IHBE
C    IGEO(11) : IGTYP
C    IGEO(12) : ISTRAIN
C    IGEO(13) : ICPRE
C    IGEO(14) : ICSTR
C    IGEO(15) : IINT
C    IGEO(16) : IFRAM
C    IGEO(17) : ISORTH
C    IGEO(18) : ISH3N
C    IGEO(19) : ICXFEM
C    IGEO(20) : ISROT 
C               ITET4    : PID6 & PID14
C               NFUNC    : PID26
C    IGEO(21) : NFUND    : PID26
C    IGEO(22) : I_INJECT : Injectors (1:'INJECT1' or 2:'INJECT2')
C    IGEO(23) : NGASES   : Injectors
C    IGEO(25) : F_IDMASS : /PROP/INJECT2
C    IGEO(26) : F_IDTEMP : /PROP/INJECT2
C    IGEO(27) and IGEO(28) are reserved for generic storage, don't use it for anything else !
C    IGEO(27) : NUVAR(1) : NUMBER OF USER ELEMENT VARIABLES
C    IGEO(28) : NUVAR(2) : NUMBER OF USER ELEMENT VARIABLES PER NODE (/PROP/NSTRAND)
C    IGEO(30) : NLAY     : /PROP/TYPE22
C    IGEO(32) : ISLV   
C    IGEO(33) : IVIS     : Solid properties
C    IGEO(34) : IVISC0   : Solid properties
C    IGEO(35) : ISVIS    : Solid properties (cf Navier Stokes viscosity)
C    IGEO(36) : IGFLU    : /PROP/PFLUID
C    IGEO(37) : NSPHDIR  : SOL2SPH
C    IGEO(38) : IPARTSPH : SOL2SPH
C    IGEO(39) : ID_SENS  : SOL2SPH
C    IGEO(40) : IAD_KNOT
C    IGEO(41) : POLYNOMIAL DEGREE in 1st direction
C    IGEO(42) : POLYNOMIAL DEGREE in 2nd direction
C    IGEO(43) : POLYNOMIAL DEGREE in 3rd direction
C    IGEO(44) : NUMBER OF CONTROL POINTS in 1st direction
C    IGEO(45) : NUMBER OF CONTROL POINTS in 2nd direction
C    IGEO(46) : NUMBER OF CONTROL POINTS in 3rd direction
C    IGEO(47) : INTEGRATION FORMULATION FLAG for PID51 (UNIFORM / GAUSS distribution)
C    IGEO(48) : DRAPE IDENTIFICATION NUMBER
C    IGEO(49) : =1 ORTHOTROPY ANGLE DEFINED AT ELEMENT LEVEL  ( /PROP/TYPE19/51/52 )
C               =2 ORTHOTROPY ANGLE DEFINED AT STACK LEVEL ( /PROP/TYPE19/51/52 )
C    IGEO(50) : ITET10 : PID6 & PID14
C    IGEO(51) : PINCHING DOF (1=ENABLE, 0=DISABLE)
C    IGEO(52) : NUPARAM ! reserved for generic storage, don't use it for anything else !
C    IGEO(53) : NJFUNC  ! reserved for generic storage, don't use it for anything else !
C    IGEO(54) : NJMAT   ! reserved for generic storage, don't use it for anything else !
C    IGEO(55) : NJPID   ! reserved for generic storage, don't use it for anything else !
C    IGEO(56) : NJTAB   ! reserved for generic storage, don't use it for anything else !
C    IGEO(57) : IADBUF  ! reserved for generic storage, don't use it for anything else !
C    IGEO(58) : IADFUN  ! reserved for generic storage, don't use it for anything else !
C    IGEO(59) : IADMAT  ! reserved for generic storage, don't use it for anything else !
C    IGEO(60) : IADPID  ! reserved for generic storage, don't use it for anything else !
C    IGEO(61) : IADTAB  ! reserved for generic storage, don't use it for anything else !
C    IGEO(62) : IALE_FLAG ! 1:Lagrange(default)  2:Euler 3:ALE
C    IGEO(63:100) : Unused
C    IGEO(101...) : Various data related to Integration points, Layers or Injectors (airbags)
C=======================================================================
C
C--------------------------------------------------
      WRITE(IOUT,1000)
C----------------------
      NBADI  = 0
      NSST_D = 0
      NSST_DS= 0
      NPSH   = 0
      CVIS =ZERO
      IAD_KNOT = 0
c----------      
C     Initialize variables used by all property types
c----
      CALL INI_PROP_VARS(PROP_TAG)
c----------    
C--------------------------------------------------
C START BROWSING MODEL PROPERTIES
C--------------------------------------------------
      CALL HM_OPTION_START('PROPERTY')
      I = 0
C--------------------------------------------------
C BROWSING MODEL PROPERTIES 1->HM_NUMGEO
C--------------------------------------------------
      DO CPT=1,HM_NUMGEO
        I = I + 1
        IGFLU = 0
        IF(ALE%GLOBAL%ICAA == 1)IGFLU=1
        ISORTH = 0
        IHGFLU = 0
c
        FAC_M = ONE
        FAC_L = ONE
        FAC_T = ONE
        KEY = ''
        SOLVERKEYWORD = ''
        IDTITL = ''
        E_TYPE = 2
C--------------------------------------------------
C EXTRACT DATAS OF /PROP/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = PROP_ID,
     .                       UNIT_ID = UID,
     .                       SUBMODEL_ID = SUB_ID,
     .                       SUBMODEL_INDEX = SUB_INDEX,
     .                       OPTION_TITR = IDTITL,
     .                       KEYWORD2 = KEY)
C--------------------------------------------------
C CHECK IF UID IS EXISTING
C--------------------------------------------------
        IFLAGUNIT = 0
        DO IUNIT=1,NUNITS
          IF (UNITAB%UNIT_ID(IUNIT) == UID) THEN
            IFLAGUNIT = 1
            EXIT
          ENDIF
        ENDDO
        IF (UID/=0.AND.IFLAGUNIT==0) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I2=UID,I1=PROP_ID,C1='PROPERTY',
     .                 C2='PROPERTY',
     .                 C3=IDTITL)
        ENDIF
C--------------------------------------------------
C WRITE TITLE IN OUT FILE
C--------------------------------------------------
        CALL FRETITL(IDTITL,IGEO(NPROPGI-LTITR+1,I),LTITR)
        WRITE(IOUT,'(A40)') IDTITL
C--------------------------------------------------
        IGTYP=0
        IHBE=0
        ISMSTR=0
        ISROT=0
        IPINCH=0
        PTHK = ZERO
c---------------------------------------
        SELECT CASE(KEY(1:LEN_TRIM(KEY)))
          CASE ('TYPE0','VOID')
C--------------------------------------------------
C            HM READING PROPERTY TYPE0 (VOID)
C--------------------------------------------------
             IGTYP=0
             CALL HM_READ_PROP0(GEO(1,I),IGEO(1,I),PROP_ID,IGTYP,UNITAB,LSUBMODEL)

          CASE ('TYPE1','TYPE01','SHELL')
C--------------------------------------------------
C            HM READING PROPERTY TYPE1 (SHELL)
C--------------------------------------------------
             IGTYP=1
             CALL HM_READ_PROP01(GEO(1,I),IGEO(1,I),PROP_TAG ,MULTI_FVM,IGTYP,PROP_ID,IDTITL,UNITAB,LSUBMODEL)

          CASE ('TYPE2','TYPE02','TRUS','TRUSS')
C--------------------------------------------------
C            HM READING PROPERTY TYPE2 (TRUSS)
C--------------------------------------------------
             IGTYP=2
             CALL HM_READ_PROP02(IGTYP ,PROP_ID  , IGEO(1,I)   ,  GEO(1,I)   ,PROP_TAG  ,UNITAB ,IDTITL,LSUBMODEL )

          CASE ('TYPE3','TYPE03','BEAM')
C--------------------------------------------------
C            HM READING PROPERTY TYPE3 (BEAM)
C--------------------------------------------------
             IGTYP=3
             CALL HM_READ_PROP03(GEO(1,I),IGEO(1,I),PROP_TAG,IGTYP,PROP_ID,IDTITL,UNITAB,LSUBMODEL)

          CASE ('TYPE4','TYPE04','SPRING')
C--------------------------------------------------
C      HM READING PROPERTY TYPE4 (SPRING)
C--------------------------------------------------
             IGTYP=4
             CALL HM_READ_PROP04(GEO(1,I),IGEO(1,I),UNITAB,PROP_ID,IGTYP,IDTITL,PROP_TAG,LSUBMODEL,IUNIT)

          CASE ('TYPE6','TYPE06','SOL_ORTH')
c------------------------------
C            HM READING PROPERTY TYPE6 (SOL_ORTH)
c------------------------------
             IGTYP=6
             CALL HM_READ_PROP06(GEO(1,I),IGEO(1,I),PROP_TAG ,MULTI_FVM,IGTYP  ,
     .                          PROP_ID  ,IDTITL   ,UNITAB   ,LSUBMODEL,RTRANS ,
     .                          SUB_ID   ,ISKN     ,IPART    ,SUB_INDEX)

          CASE ('TYPE5','TYPE05','RIVET')
c------------------------------
C            HM READING PROPERTY TYPE5 (RIVET)
c------------------------------
             IGTYP=5
             CALL HM_READ_PROP05(GEO(1,I),IGEO(1,I),PROP_ID ,IGTYP ,IDTITL ,UNITAB ,LSUBMODEL ) 

          CASE ('TYPE8','TYPE08','SPR_GENE')
C--------------------------------------------------
C      HM READING PROPERTY TYPE8 (general spring)
C--------------------------------------------------
             IGTYP=8
             CALL HM_READ_PROP08(GEO(1,I),IGEO(1,I),PROP_TAG ,IGTYP,PROP_ID,
     .                           ISKN,UNITAB,IUNIT,IDTITL,LSUBMODEL,SUB_INDEX)


          CASE ('TYPE9','TYPE09','SH_ORTH')
C--------------------------------------------------
C            HM READING PROPERTY TYPE9 (Orthotropic shell)
C--------------------------------------------------
             IGTYP=9
             CALL HM_READ_PROP09(IGTYP ,GEO(1,I),IGEO(1,I),PROP_TAG,UNITAB,
     .                           RTRANS,LSUBMODEL,IDTITL ,PROP_ID   ,SUB_ID,ISKN )
          CASE ('TYPE10','SH_COMP')
C--------------------------------------------------
C            HM READING PROPERTY TYPE10 
C--------------------------------------------------
             IGTYP=10
             CALL HM_READ_PROP10(IGTYP ,GEO(1,I),IGEO(1,I),PROP_TAG,UNITAB,
     .                           RTRANS,LSUBMODEL,IDTITL ,PROP_ID   ,SUB_ID,ISKN )
          CASE ('TYPE11','SH_SANDW')
C--------------------------------------------------
C      HM READING PROPERTY TYPE11 (Orthotropic shell)
C--------------------------------------------------
             IGTYP=11
             CALL HM_READ_PROP11(GEO(1,I)       ,IGEO(1,I)     ,PM       ,IPM      ,ISKN     ,
     .                   UNITAB    ,RTRANS   ,LSUBMODEL,SUB_ID   ,IDTITL   ,
     .                   PROP_TAG  ,PROP_ID       ,IGTYP    )
c
          CASE ('TYPE12','SPR_PUL')
C--------------------------------------------------
C            HM READING PROPERTY TYPE12 (SPR_PUL)
C--------------------------------------------------
             IGTYP=12
             CALL HM_READ_PROP12(GEO(1,I), IGEO(1,I),UNITAB, IGTYP, PROP_ID,
     .                PROP_TAG, IDTITL, LSUBMODEL)
c
          CASE ('TYPE13','SPR_BEAM')
C--------------------------------------------------
C            HM READING PROPERTY TYPE13 (SPR_BEAM)
C--------------------------------------------------
             IGTYP=13
             CALL HM_READ_PROP13(GEO(1,I),IGEO(1,I),PROP_ID, UNITAB, ISKN,
     .                           IDTITL  ,IGTYP    ,PROP_TAG, LSUBMODEL)

          CASE ('TYPE16','SH_FABR')
C--------------------------------------------------
C      HM READING PROPERTY TYPE16 (Anisotropic Layered Shell Property)
C--------------------------------------------------
             IGTYP=16
             CALL HM_READ_PROP16(GEO(1,I) ,IGEO(1,I),PM       ,IPM      ,ISKN     ,
     .                           UNITAB   ,RTRANS   ,LSUBMODEL,SUB_ID   ,PROP_TAG ,
     .                           IGTYP    ,PROP_ID       ,IDTITL   )

         CASE ('TYPE17','STACK')
C--------------------------------------------------
C      HM READING PROPERTY TYPE17 (Orthotropic shell)
C--------------------------------------------------
             IGTYP=17
             NUMS = NUMGEO_STACK(CPT)
             CALL HM_READ_PROP17(GEO(1,I)       ,IGEO(1,I)     ,PM       ,IPM      ,ISKN     ,
     .                   UNITAB    ,RTRANS   ,LSUBMODEL,SUB_ID   ,IDTITL   ,
     .                   PROP_ID   ,PROP_TAG  ,STACK_INFO(NUMS))
     
          CASE ('TYPE18','INT_BEAM')
C--------------------------------------------------
C            HM READING PROPERTY TYPE18 (INT_BEAM)
C--------------------------------------------------
             IGTYP=18
             CALL HM_READ_PROP18(GEO(1,I),IGEO(1,I),PROP_TAG,IGTYP,PROP_ID,IDTITL,UNITAB,LSUBMODEL)

          CASE('TYPE19','PLY')
C--------------------------------------------------
C            HM READING PROPERTY TYPE19 (PLY)
C--------------------------------------------------
             IGTYP=19
             ISTACK = 0
             CALL HM_READ_PROP19(PROP_ID, IGTYP, ISTACK, GEO(1,I),IGEO(1,I),PM,IPM,UNITAB,IDRAPEID,
     .            LSUBMODEL)

C--------------------------------------------------
C            HM READING PROPERTY TYPE14 (SOLID,FLUID)
C--------------------------------------------------
          CASE ('TYPE14','SOLID')
            IGTYP=14
            IF (ALE%GLOBAL%ICAA == 0 .AND. IGFLU == 0) THEN
             CALL HM_READ_PROP14(GEO(1,I),IGEO(1,I),PROP_TAG ,MULTI_FVM,IGTYP,PROP_ID,IDTITL,UNITAB,LSUBMODEL,
     .                          IPART )
            ELSE
             CALL HM_READ_PROP14F(GEO(1,I),IGEO(1,I),PROP_TAG ,MULTI_FVM,IGTYP,PROP_ID,IDTITL,UNITAB,LSUBMODEL)
            END IF
          CASE ('FLUID')
            IGTYP=14
            IGFLU=1
            CALL HM_READ_PROP14F(GEO(1,I),IGEO(1,I),PROP_TAG ,MULTI_FVM,IGTYP,PROP_ID,IDTITL,UNITAB,LSUBMODEL)

C--------------------------------------------------
C            HM READING PROPERTY TYPE15 (POROUS)
C--------------------------------------------------
          CASE ('TYPE15','POROUS')
             IGTYP=15
             CALL HM_READ_PROP15(PROP_ID ,IGTYP , GEO(1,I)   ,  IGEO(1,I)   ,PROP_TAG  ,UNITAB ,
     .                   LSUBMODEL,IDTITL , ISKN     ,ITABM1       )

          CASE ('TYPE20','TSHELL')
C--------------------------------------------------
C            HM READING PROPERTY TYPE20 (THICKSHELL)
C--------------------------------------------------
             IGTYP=20
             CALL HM_READ_PROP20(GEO(1,I),IGEO(1,I),PROP_TAG ,MULTI_FVM,IGTYP,PROP_ID,IDTITL,UNITAB,LSUBMODEL)

          CASE ('TYPE21','TSH_ORTH')
C--------------------------------------------------
C      HM READING PROPERTY TYPE21 (Orthotropic Thick shell)
C--------------------------------------------------
             
             IGTYP=21
            CALL HM_PROP_READ21(GEO(1,I) ,IGEO(1,I) ,PROP_ID    ,ISKN   ,UNITAB ,
     .                          RTRANS   ,LSUBMODEL ,SUB_ID,IDTITL ,IGTYP  ,PROP_TAG)
          CASE ('TYPE22','TSH_COMP')
C--------------------------------------------------
C      HM READING PROPERTY TYPE22 (Composite Thick shell)
C--------------------------------------------------
             
             IGTYP=22
            CALL HM_READ_PROP22(GEO(1,I) ,IGEO(1,I) ,IGTYP,PROP_ID    ,IDTITL   ,UNITAB ,
     .                          LSUBMODEL ,PM ,IPM ,RTRANS   ,SUB_ID ,ISKN,PROP_TAG)
C--------------------------------------------------
          CASE ('TYPE23','SPR_MAT')
C--------------------------------------------------
C            HM READING PROPERTY TYPE23 (SPR_MAT)
C            New spring defined as PID+MID
C--------------------------------------------------
             IGTYP=23
           CALL HM_READ_PROP23(GEO(1,I),IGEO(1,I),PROP_ID    ,IGTYP    ,UNITAB,
     .                         ISKN    ,IDTITL   ,LSUBMODEL, PROP_TAG)       
c
          CASE ('TYPE25','SPR_AXI')
C--------------------------------------------------
C            HM READING PROPERTY TYPE13 (SPR_BEAM)
C--------------------------------------------------
             IGTYP=25
             CALL HM_READ_PROP25(GEO(1,I), IGEO(1,I), UNITAB, ISKN,IGTYP, 
     .                           PROP_ID,PROP_TAG,IDTITL,LSUBMODEL,SUB_INDEX)

          CASE ('TYPE26','SPR_TAB')
C--------------------------------------------------
C            HM READING PROPERTY TYPE26 (SPR_TAB)
C--------------------------------------------------
             IGTYP=26
             CALL HM_READ_PROP26(GEO(1,I), IGEO(1,I), UNITAB, PROP_ID,IGTYP, 
     .                           PROP_TAG,IDTITL,LSUBMODEL)
c
          CASE ('TYPE27','SPR_BDAMP')
C--------------------------------------------------
C            HM READING PROPERTY TYPE27 (SPR_BDAMP)
C--------------------------------------------------
             IGTYP=27
             CALL HM_READ_PROP27(GEO(1,I),IGEO(1,I),PROP_TAG,IGTYP,PROP_ID,UNITAB,LSUBMODEL)
c
          CASE ('INJECT1')
C--------------------------------------------------
C            HM READING PROPERTY TYPE0 (VOID)
C--------------------------------------------------
             IGTYP=0
             CALL HM_READ_INJECT1(GEO(1,I),IGEO(1,I),PROP_TAG,IGTYP,PROP_ID,IDTITL,UNITAB,LSUBMODEL,IPM,PM,NPC,PLD)
c
          CASE ('INJECT2')
C--------------------------------------------------
C            HM READING PROPERTY TYPE0 (VOID)
C--------------------------------------------------
             IGTYP=0
             CALL HM_READ_INJECT2(GEO(1,I),IGEO(1,I),PROP_TAG,IGTYP,PROP_ID,IDTITL,UNITAB,LSUBMODEL,IPM,PM,NPC,PLD)
c
          CASE ('TYPE51')
C--------------------------------------------------
C           HM READING PROPERTY TYPE51
C--------------------------------------------------
            IGTYP= 51
            NUMS = NUMGEO_STACK(CPT)
            CALL HM_READ_PROP51(
     .           GEO(1,I) ,IGEO(1,I) ,PM       ,IPM      ,ISKN     ,
     .           PROP_ID  ,PROP_TAG  ,RTRANS   ,SUB_ID   ,STACK_INFO(NUMS)   ,
     .           IDTITL   ,UNITAB    ,LSUBMODEL)
c
          CASE ('PCOMPP')
C--------------------------------------------------
C           HM READING PROPERTY PCOMPP (Composite) 
C--------------------------------------------------
            IGTYP=52
c
            IGEO( 1,I) = PROP_ID
            IGEO(11,I)=IGTYP
            GEO (12,I) = IGTYP  ! double storage
c------------------------------
c
          CASE DEFAULT
C-----------------------------
C           GENERIC BRANCHING TO SO-CALLED "USER" PROPERTIES
C-----------------------------
            CALL HM_READ_PROP_GENERIC(
     1        IGTYP    ,PROP_ID ,IDTITL  ,KEY      ,GEO(1,I),
     2        IGEO(1,I),LBUFGEO ,BUFGEO  ,IADBUF   ,UNITAB,
     3        ISKN     ,KNOT    ,IAD_KNOT,PROP_TAG ,LSUBMODEL,
     4        RTRANS   ,SUB_ID  ,IUNIT   ,SUB_INDEX)
         
        END SELECT ! SELECT CASE(IGTYP)
c
C--------   Variables stored in element buffer 
        PROP_SHELL = 0
        IF (IGTYP == 1  .OR. IGTYP == 9  .OR. IGTYP == 10 .OR. IGTYP == 11 .OR.
     .      IGTYP == 16 .OR. IGTYP == 17 .OR. IGTYP == 19 .OR. IGTYP == 51 .OR.
     .      IGTYP == 52 ) PROP_SHELL = 1
C
        IF (PROP_SHELL == 1) THEN
c----   Shells
          PROP_TAG(IGTYP)%G_SIG = 0
          PROP_TAG(IGTYP)%G_FOR = 5
          PROP_TAG(IGTYP)%G_MOM = 3
          PROP_TAG(IGTYP)%G_THK = 1
          PROP_TAG(IGTYP)%G_EINT= 2
          PROP_TAG(IGTYP)%G_EINS= 0
          PROP_TAG(IGTYP)%L_SIG = 5
          IF (IGTYP == 17) PROP_TAG(IGTYP)%L_SIGPLY = 3
          PROP_TAG(IGTYP)%L_THK = 0
          PROP_TAG(IGTYP)%L_EINT= 2
          PROP_TAG(IGTYP)%L_EINS= 0
          PROP_TAG(IGTYP)%G_VOL = 1
          PROP_TAG(IGTYP)%L_VOL = 1
          PROP_TAG(IGTYP)%LY_DMG = 2
          IF (IGTYP == 9  .OR. IGTYP == 10 .OR. IGTYP == 11 .OR. 
     .        IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 ) THEN
            PROP_TAG(IGTYP)%LY_GAMA = 6
            PROP_TAG(IGTYP)%LY_DIRA = 2
          ELSEIF (IGTYP == 16) THEN
            PROP_TAG(IGTYP)%LY_GAMA = 6
            PROP_TAG(IGTYP)%LY_DIRA = 2
            PROP_TAG(IGTYP)%LY_DIRB = 2
          ENDIF
          PROP_TAG(IGTYP)%LY_PLAPT = 1      
          PROP_TAG(IGTYP)%LY_SIGPT = 5
          PROP_TAG(IGTYP)%G_FORPG  = 5
          PROP_TAG(IGTYP)%G_MOMPG  = 3
          PROP_TAG(IGTYP)%G_STRPG  = 8
C
          IF((IGTYP == 11 .OR. IGTYP == 17 ) )  PROP_TAG(IGTYP)%LY_DMG = 2
        ENDIF
C
      ENDDO
c
C     ----------------    C
C     FOR TYPE 19 PROP
C     ----------------    C
      NPLYMAX = 0
      DO CPT = 1, HM_NUMGEO
         IF (IGEO(11, CPT) == 19) THEN
            NPLYMAX = NPLYMAX + 1
            IGEO(102, CPT) = NPLYMAX
         ENDIF
      ENDDO
C
C  For TYPE 17 & 51       
C
      DO CPT = 1, HM_NUMGEO
        IGTYP=IGEO(11,CPT)
        NUMS= NUMGEO_STACK(CPT)
        IF (IGTYP == 17 .OR. IGTYP == 51 ) THEN
          N1 = IGEO(4,CPT)
          DO 100 J =1 , N1
C ply of stack JPID           
              JPID = STACK_INFO(NUMS)%PID(J)  
               IF (JPID > 0) THEN
                 DO K=1,HM_NUMGEO
                   NSTACK = 0
                   IF (IGEO(1,K) == JPID .AND.IGEO(11,K)==19) THEN
                     STACK_INFO(NUMS)%PID(J) = K
C number of stack where ply is attached  
                     NSTACK = IGEO(42,K)
                     NSTACK = NSTACK +  1
                     IGEO(42   ,K) = NSTACK
C stack whe e ply is belong                  
                     IGEO(200 + NSTACK ,K) = CPT
                     GOTO 100
                   ENDIF
                 ENDDO
                 CALL FRETITL2(TITR1,IGEO(NPROPGI-LTITR+1,CPT),LTITR)
                 CALL ANCMSG(MSGID=373,
     .                       MSGTYPE=MSGERROR,
     .                       ANMODE=ANINFO_BLIND_1,
     .                       I1=IGEO(1,CPT),C1=TITR1,
     .                       C2='PROPERTY',
     .                       I2=JPID)
               ENDIF
 100      CONTINUE
C interface substack
              NISUB = IGEO(44,CPT) 
              IF (NISUB > 0) THEN
                DO 110 J =1 , NISUB
                  JPID1 = STACK_INFO(NUMS)%ISUB( 3*(J-1) + 1 )
                  JPID2 = STACK_INFO(NUMS)%ISUB( 3*(J-1) + 2 )
                     IF (JPID1 > 0 .OR. JPID2 > 0) THEN
                         DO K=1,NUMGEO
                             NSTACK = 0
                             IF (IGEO(1,K) == JPID1) THEN
                               STACK_INFO(NUMS)%ISUB (3*(J-1) + 1) = K
                               GOTO 110
                             ELSEIF (IGEO(1,K) == JPID2) THEN 
                               STACK_INFO(NUMS)%ISUB (3*(J-1) + 2) = K
                               GOTO 110
                             ENDIF
                         ENDDO
                         CALL FRETITL2(TITR1,IGEO(NPROPGI-LTITR+1,CPT),LTITR)
                         CALL ANCMSG(MSGID=373,
     .                               MSGTYPE=MSGERROR,
     .                               ANMODE=ANINFO_BLIND_1,
     .                               I1=IGEO(1,CPT),C1=TITR1,
     .                               C2='PROPERTY',
     .                               I2=JPID1)
                         CALL FRETITL2(TITR1,IGEO(NPROPGI-LTITR+1,CPT),LTITR)
                         CALL ANCMSG(MSGID=373,
     .                               MSGTYPE=MSGERROR,
     .                               ANMODE=ANINFO_BLIND_1,
     .                               I1=IGEO(1,CPT),C1=TITR1,
     .                               C2='PROPERTY',
     .                               I2=JPID2)
                     ENDIF ! IF (JPID1 > 0 .OR. JPID2 > 0)
 110            CONTINUE 
              ENDIF ! IF (NISUB > 0)
C
          
              DO J=1,N1
                JPID = STACK_INFO(NUMS)%PID(J)
                STACK_INFO(NUMS)%THK(J)   = GEO(1,JPID)
                STACK_INFO(NUMS)%DIR(J)   = GEO(212,JPID) ! angle (DIR1,DIR2) - for compatibility of law58 with PID51)
                STACK_INFO(NUMS)%MID(J)   = IGEO(101,JPID)
              ENDDO
C
C  error message for no mixed law with TYPE_17
C
             J=STACK_INFO(NUMS)%MID(1)
             MLAWLY = NINT(PM(19,J))
             PROP_ID = IGEO(1,CPT)
C-----OTHER ply
             DO 350 N=2,N1
               J = STACK_INFO(NUMS)%MID(N)
               MID = IPM(1,J)
               IF (IGTYP == 51) GOTO 350
               IF (NINT(PM(19,J)) == MLAWLY) GOTO 350
               WRITE(LAW_ID,'(I2)')MLAWLY
               IF (MLAWLY==99) LAW_ID='USER'
               CALL ANCMSG(MSGID=899,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO_BLIND_1,
     .                     I1=PROP_ID,
     .                     C1=IDTITL,
     .                     I2=MID,
     .                     C2=LAW_ID)
  350        CONTINUE
C
C  check for layer materials compatibility with TYPE_17 to add when cfg file ready
C
            IF (IGTYP == 17) THEN
              DO 450 N=1,N1
                J = STACK_INFO(NUMS)%MID(N)
                MID = IPM(1,J)
                JPID = STACK_INFO(NUMS)%PID(N)
                MLAWLY = NINT(PM(19,J))
                IF (MLAWLY == 15 .OR. MLAWLY == 25 .OR. MLAWLY == 27 .OR.
     .             (MLAWLY >= 29 .AND.MLAWLY <= 31).OR. MLAWLY == 36 .OR.
     .              MLAWLY == 72 .OR. MLAWLY == 99) GOTO 450
C
                  WRITE(LAW_ID,'(I2)')MLAWLY
                  CALL FRETITL2(IDTITL,IGEO(NPROPGI-LTITR+1,JPID),LTITR)
                  CALL ANCMSG(MSGID=1202,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO,
     .                        I1=IGEO(1,JPID),
     .                        C1=IDTITL,
     .                        I2=MID,
     .                        C2=LAW_ID,
     .                        I3=MLAWLY)
  450         CONTINUE
            ENDIF !IF (IGTYP == 17) THEN
         ENDIF ! begin igtype = 17   
      ENDDO ! DO CPT = 1, HM_NUMGEO
C------------------------------
C SQRT precalculation
C------------------------------
      DO I = 1, NUMGEO
        GEO(100,I) = SQRT(GEO(38,I))      ! SHFSR
      END DO
C-------------------------------------
C Searching for duplicated prop_ids 
C-------------------------------------
      RBID = ZERO
      CALL VDOUBLE(IGEO(1,1),NPROPGI,NUMGEO,MESS,0,RBID)
      
C-------------------------------
      RETURN
C-------------------------------
 1000 FORMAT(//
     & 5X,'    PROPERTY SETS'/,
     & 5X,'    -------------'//)
      END
