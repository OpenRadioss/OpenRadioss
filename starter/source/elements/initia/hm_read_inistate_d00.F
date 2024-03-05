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
Chd|  HM_READ_INISTATE_D00          source/elements/initia/hm_read_inistate_d00.F
Chd|-- called by -----------
Chd|        LEC_INISTATE                  source/elements/initia/lec_inistate.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HM_GET_FLOAT_ARRAY            source/devtools/hm_reader/hm_get_float_array.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        LEC_INISTATE_D00_BRICK_CHECK  source/elements/initia/lec_inistate_d00_brick-check.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        SUBROTTENS                    source/model/submodel/subrot.F
Chd|        SUBROTVECT                    source/model/submodel/subrot.F
Chd|        UEL2SYS                       source/initial_conditions/inista/yctrl.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MAPPING_OPTION_MOD            share/modules1/dichotomy_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/mat_elem/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INISTATE_D00 (
     1                  IXS        ,IXQ          ,IXC          ,IXT          ,IXP       ,
     2                  IXR        ,GEO          ,PM           ,IXTG         ,INDEX     ,
     3                  ITRI       ,NSIGSH       ,IGEO      ,
     4                  IPM        ,NSIGS        ,NSIGSPH      ,KSYSUSR      ,NSIGRS    ,
     5                  UNITAB     ,ISOLNODD00   ,LSUBMODEL    ,RTRANS       ,IDRAPE    ,
     6                  NSIGI      ,NSIGBEAM     ,NSIGTRUSS    ,
     7                  SIGI       ,SIGSH        ,SIGSP        ,SIGSPH       ,SIGRS     ,
     8                  SIGBEAM    ,SIGTRUSS     ,STRSGLOB     ,STRAGLOB     ,ORTHOGLOB ,
     9                  ISIGSH     ,IYLDINI      ,FAIL_INI     ,IUSOLYLD     ,IUSER     , 
     A                  ID_SIGSH   ,ID_SOLID_SIGI,ID_QUAD_SIGI ,ID_SIGSPRI   ,ID_SIGBEAM,
     B                  ID_SIGTRUSS,WORK         ,IGRBRIC      ,NIBRICK      ,NIQUAD    ,
     C                  NISHELL    ,NISH3N       ,NISPRING     ,NIBEAM       ,NITRUSS   ,
     D                  MAP_TABLES ,VARMAX       ,IPARG        ,PTSHEL       ,PTSH3N    ,
     E                  STACK      ,IWORKSH      ,IOUT         ,MAT_PARAM    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE HM_OPTION_READ_MOD
      USE MAPPING_OPTION_MOD
      USE STACK_MOD
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "drape_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "scry_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IXS(NIXS,*), IXQ(NIXQ,*) ,IXC(NIXC,*),
     .   IGEO(NPROPGI,*) , IXT(NIXT,*) ,IXP(NIXP,*), IXR(NIXR,*),
     .   IXTG(NIXTG,*)   , INDEX(*)    ,ITRI(*)    ,IPM(NPROPMI,*),
     .   KSYSUSR(*)      , IDRAPE(NPLYMAX,*)
      INTEGER NSIGI, NSIGSH, NSIGS, NSIGSPH, NSIGRS,
     .        ISOLNODD00(*), NSIGBEAM, NSIGTRUSS, STRSGLOB(*),
     .        STRAGLOB(*), ORTHOGLOB(*), ISIGSH, IYLDINI, FAIL_INI(5),
     .        IUSOLYLD, IUSER,VARMAX
      INTEGER ID_SIGSH(*), ID_SOLID_SIGI(*), ID_QUAD_SIGI(*)
      INTEGER ID_SIGSPRI(*), ID_SIGBEAM(*), ID_SIGTRUSS(*)
      INTEGER WORK(*)
      INTEGER NIBRICK, NIQUAD, NISHELL, NISH3N, NISPRING, NIBEAM, NITRUSS
      my_real
     .   GEO(*),PM(NPROPM,*),RTRANS(NTRANSF,*),
     .   SIGI(NSIGS,*),SIGSH(MAX(1,NSIGSH),*),SIGTRUSS(NSIGTRUSS,*),
     .   SIGSP(NSIGI,*),SIGSPH(NSIGSPH,*),SIGRS(NSIGRS,*),SIGBEAM(NSIGBEAM,*)
C
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
C
      TYPE(MAPPING_STRUCT_) :: MAP_TABLES
      INTEGER, DIMENSION(NPARG,NGROUP) ,INTENT(IN):: IPARG
      INTEGER,   INTENT(INOUT) :: PTSHEL(NUMELC),PTSH3N(NUMELTG)
      TYPE (STACK_PLY) :: STACK
      INTEGER,   INTENT(IN) :: IWORKSH(3,NUMELC + NUMELTG)
      INTEGER,   INTENT(IN) :: IOUT
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(INOUT) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, N, I,J, L,IG, ISOLNOD,IGTYP,
     .        IHBE,NE, ISH3N,IIS,NIP,IPG,NPG,PT,NPP,
     .   J1,JJ1,NU,IP,JJ,NUVAR,L_SIGB,
     .   NVARSH,NUMR,NUMSOLID,KK,UID,IFLAGUNIT,
     .   IUNIT, IFRAM, JJHBE, IORTH, ND, NPTD00,
     .   NUVARD00, NDIR, NPGTMP,
     .   NPTR,NPTS,NPTT,JR,JS,JT,NFAIL(5),IMAT,ILAW,
     .   JL,NPT_MAX,MLAWLY,IPMAT,JDRP_ID,NVARBEAM,IFAIL,NEM1,
     .   IRUPT_TYP,NVAR_RUPT,IOK,NVMAX,CPTVAR,FLAGDEG,NUM_LINES,NMAX_AUX,NMAX_FAIL,
     .   ISUBSTACK,NSLICE,IPNPT_LAY,IPT
      INTEGER IE, KN, IR, IS, IT, BRIGLOB, SUB_ID, NLAY, ILAY, PID
      INTEGER KTRIELS, KTRIELC, KTRIELTG, KTRIELSPR, KTRIELBEAM, KTRIELTRUSS,
     .   KTRIELTQUAD
      INTEGER IGBR, JGBR, I1, SIZE,NSROT,NG,ITYR,NFTR,NELR,ISMRAD
!      INTEGER :: VARMAX
!      PARAMETER (VARMAX = MAX(NSIGSH,NSIGI,NSIGI,NSIGTRUSS,NSIGBEAM,NSIGRS))
      my_real
     .   EM , EB, H1, H2, H3,
     .   R0 , EIN, VX, VY, VZ, PHI1, PHI2, SCALEYLD,
     .   EXX, EYY, EXY, EYZ, EZX, FXX, FYY, FXY,
     .   EPSP, ANGLE1, ANGLE2, AA, AREA, FOR,ENER,DENS,
     .   FILL
      my_real
     .   S(6),
!     .   TMPVAL(NVSHELL),
     .   TMPVAL(VARMAX),
     .   TMPVAL1(VARMAX),TMPVAL2(VARMAX),TMPVAL3(VARMAX),
     .   TMPVAL4(VARMAX),TMPVAL5(VARMAX),TMPVAL6(VARMAX),
     .   TMPVAL7(VARMAX),TMPVAL8(VARMAX),TMPVAL9(VARMAX),
     .   TMPVAL10(VARMAX),TMPVAL11(VARMAX),TMPVAL12(VARMAX),
     .   TMPVAL13(VARMAX)
!
      INTEGER, DIMENSION(:), ALLOCATABLE :: KSYSUSRTG ,KSYSUSRS,INDEXS,ITRIS,
     . KSYSUSRQ,INDEXQ,ITRIQ,IES2IPARG,MLAW_LY

      CHARACTER KEY2*ncharkey,KEY3*ncharkey
      CHARACTER*nchartitle,TITR,TITR1  
      CHARACTER*15,KEYWORD 
      INTEGER NONEXIST
C-----------------------------------------------
      EXTERNAL UEL2SYS
      INTEGER  UEL2SYS
C-----------------------------------------------
      LOGICAL IS_AVAILABLE,GLOB
      CHARACTER KEY*ncharkey
      INTEGER  NB_INIBRI,NB_INISHE,NB_INISH3,NB_ELEMENTS,ID_ELEM,
     . ELT,ELTP,INI,K0,NB_INITRUSS,NB_INIBEAM,NB_INISPRI,NB_INIQUA,
     . SUB_INDEX,IDOUB,ISTRSF,ISTRSFG,ISTRAF,ISTRAFG,ISTAT
      my_real
     .   THK,FOR1,FOR2,FOR3,MOM1,MOM2,MOM3
!
      INTEGER  SET_USRTOS
      EXTERNAL SET_USRTOS
C=======================================================================
C
C -- LECTURE OF INITIAL STATE DATA - EXTRACTED FROM INITIA.F
C
C=======================================================================
      SUB_INDEX = 0
      NONEXIST = 0
C
      ALLOCATE (ITRIS(NUMELS))
      ALLOCATE (INDEXS(2*NUMELS))
      ALLOCATE (KSYSUSRS(2*NUMELS))
      ALLOCATE (KSYSUSRTG(2*NUMELTG))
      ALLOCATE (ITRIQ(NUMELQ))
      ALLOCATE (INDEXQ(2*NUMELQ))
      ALLOCATE (KSYSUSRQ(2*NUMELQ))
      ALLOCATE (IES2IPARG(NUMELS) ,STAT=ISTAT)
      IF (ISTAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IES2IPARG')
C
      IF (NUMELS  > 0) ITRIS(1:NUMELS)  = 0
      IF (NUMELS  > 0) INDEXS(1:2*NUMELS) = 0
      IF (NUMELS  > 0) KSYSUSRS(1:2*NUMELS)=0
      IF (NUMELTG > 0) KSYSUSRTG(1:2*NUMELTG)=0
      IF (NUMELQ  > 0) ITRIQ(1:NUMELQ)  = 0
      IF (NUMELQ  > 0) INDEXQ(1:2*NUMELQ) = 0
      IF (NUMELQ  > 0) KSYSUSRQ(1:2*NUMELQ)=0
      IF (NUMELS  > 0) IES2IPARG(1:NUMELS)  = 0
C
      KTRIELS = 0
      KTRIELC = 0
      KTRIELTG = 0
      KTRIELSPR = 0
      KTRIELBEAM = 0
      KTRIELTRUSS = 0
      KTRIELTQUAD = 0
      NEM1 = 0
C-----------------------------------------
C     CONTRAINTES INITIALES FICHIER D00
C-----------------------------------------
      IS_AVAILABLE = .FALSE.
      GLOB         = .FALSE.
!-----------------------------------------
!            ---  /INIBRI  ---
!-----------------------------------------
!- check incompatibility keywords first---
      CALL HM_OPTION_COUNT('/INIBRI', NB_INIBRI)
!
      IF ( NB_INIBRI > 0 ) THEN
!--
        DO NG=1,NGROUP
          ITYR=IPARG(5,NG)
          NFTR=IPARG(3,NG)
          NELR=IPARG(2,NG)
          IF (ITYR /= 1 ) CYCLE
          DO I=1,NELR
            IES2IPARG(I+NFTR) = NG
          ENDDO
        END DO
!        
        ! Start reading /INIBRI card 
        CALL HM_OPTION_START('/INIBRI')
        ISTRSF=0
        ISTRSFG=0
        ISTRAF=0
        ISTRAFG=0
!
        DO INI=1,NB_INIBRI
!
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            UNIT_ID  = UID,
     .                            SUBMODEL_INDEX = SUB_INDEX,
     .                            SUBMODEL_ID = SUB_ID,
     .                            KEYWORD2 = KEY)
!
c---------------------------------------
          SELECT CASE (KEY(1:LEN_TRIM(KEY)))
C---------
            CASE ( 'STRS_FGLO' )
              ISTRSFG=1
            CASE ( 'STRS_F' )
              ISTRSF=1
            CASE ( 'STRA_FGLO' )
              ISTRAFG=1
            CASE ( 'STRA_F' )
              ISTRAF=1
          END SELECT ! SELECT CASE(KEY)
        END DO 
        IF (ISTRSFG>0.AND.ISTRSF>0) THEN
           CALL ANCMSG(MSGID=2044,ANMODE=ANINFO,MSGTYPE=MSGWARNING)
        END IF
        IF (ISTRAFG>0.AND.ISTRAF>0) THEN
           CALL ANCMSG(MSGID=2045,ANMODE=ANINFO,MSGTYPE=MSGWARNING)
        END IF
      END IF !( NB_INIBRI > 0 ) THEN
          
      BRIGLOB = 0
      NIBRICK = 0
      I = 0
!
!
      IF ( NB_INIBRI > 0 ) THEN
!
        ! Start reading /INIBRI card 
        CALL HM_OPTION_START('/INIBRI')
!---
! to be replaced by ---  MAP_TABLES%ISOLM ---
           IF(KTRIELS==0)THEN
             DO IE = 1, NUMELS
               ITRIS(IE) = IXS(NIXS,IE)
             END DO
             CALL MY_ORDERS(0,WORK,ITRIS,INDEXS,NUMELS,1)
             DO J = 1, NUMELS
               IE=INDEXS(J)
               KSYSUSRS(J)       =IXS(NIXS,IE)
               KSYSUSRS(NUMELS+J)=IE
             END DO
             KTRIELS=1
           END IF
!---
        DO INI=1,NB_INIBRI
!
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            UNIT_ID  = UID,
     .                            SUBMODEL_INDEX = SUB_INDEX,
     .                            SUBMODEL_ID = SUB_ID,
     .                            KEYWORD2 = KEY)
!
          IFLAGUNIT = 0
          DO IUNIT=1,UNITAB%NUNITS
            IF (UNITAB%UNIT_ID(IUNIT) == UID) THEN    
              IFLAGUNIT = 1
              EXIT
            ENDIF
          ENDDO
          IF (UID/=0.AND.IFLAGUNIT == 0) THEN
            CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I2=UID,I1=SUB_ID,C1='INIBRI',
     .                  C2='INIBRI',C3=' ')
          ENDIF
c---------------------------------------
          SELECT CASE (KEY(1:LEN_TRIM(KEY)))
C---------
            CASE ( 'FILL' )
C---------
              CALL HM_GET_INTV('inibri_fill_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                ! Reading  ---  ID_ELEM, FILL  ---
                CALL HM_GET_INT_ARRAY_INDEX('brick_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_FLOAT_ARRAY_INDEX('value',FILL,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                I=I+1
                ID_SOLID_SIGI(I)  = ID_ELEM
                SIGI(11,I) = FILL
!
              ENDDO ! DO J=1,NB_ELEMENTS
C---------
            CASE ( 'EPSP' )
C---------
              CALL HM_GET_INTV('inibri_epsp_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                ! Reading  ---  ID_ELEM, EPSP  ---
                CALL HM_GET_INT_ARRAY_INDEX('brick_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_FLOAT_ARRAY_INDEX('value',EPSP,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                I=I+1
                ID_SOLID_SIGI(I)  = ID_ELEM
                SIGI(10,I) = EPSP
!
              ENDDO ! DO J=1,NB_ELEMENTS
C---------
            CASE ( 'ENER' )
C---------
              CALL HM_GET_INTV('inibri_ener_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                ! Reading  ---  ID_ELEM, ENER  ---
                CALL HM_GET_INT_ARRAY_INDEX('brick_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_FLOAT_ARRAY_INDEX('value',ENER,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                I=I+1
                ID_SOLID_SIGI(I)  = ID_ELEM
                SIGI(9,I) = ENER
!
              ENDDO ! DO J=1,NB_ELEMENTS
C---------
            CASE ( 'DENS' )
C---------
              CALL HM_GET_INTV('inibri_dens_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                ! Reading  ---  ID_ELEM, DENS  ---
                CALL HM_GET_INT_ARRAY_INDEX('brick_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_FLOAT_ARRAY_INDEX('value',DENS,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                I=I+1
                ID_SOLID_SIGI(I)  = ID_ELEM
                SIGI(8,I) = DENS
!
              ENDDO ! DO J=1,NB_ELEMENTS
C---------
            CASE ( 'STRESS' )
C---------
              CALL HM_GET_INTV('inibri_stress_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                ! Reading  ---  ID_ELEM, STRESS  ---
                CALL HM_GET_INT_ARRAY_INDEX('bric_IDst',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_FLOAT_ARRAY_INDEX('SIGMA_x',S(1),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                CALL HM_GET_FLOAT_ARRAY_INDEX('SIGMA_y',S(2),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                CALL HM_GET_FLOAT_ARRAY_INDEX('SIGMA_z',S(3),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                CALL HM_GET_FLOAT_ARRAY_INDEX('SIGMA_xy',S(4),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                CALL HM_GET_FLOAT_ARRAY_INDEX('SIGMA_yz',S(5),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                CALL HM_GET_FLOAT_ARRAY_INDEX('SIGMA_xz',S(6),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                I=I+1
                ID_SOLID_SIGI(I)  = ID_ELEM
                DO K=1,6
                  SIGI(K,I) = S(K)
                ENDDO
!
              ENDDO ! DO J=1,NB_ELEMENTS
C---------
            CASE ( 'AUX' )
C---------
              CALL HM_GET_INTV('inibri_aux_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                ! Reading  ---  ID_ELEM, ...  ---
                CALL HM_GET_INT_ARRAY_INDEX('brick_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('Nb_integr',NPT,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('Isolnod',ISOLNOD,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('Isolid',JJHBE,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('nvars',NUVAR,J,IS_AVAILABLE,LSUBMODEL)
!
                IE=UEL2SYS(ID_ELEM,KSYSUSRS,NUMELS)
!
                IF (IE == 0) THEN        
                  ! Shell was not found. Issue a Warning & Skip.
                  NONEXIST = NONEXIST+1
                ELSE
                  KEYWORD = '/INIBRI/AUX   '
                  I=I+1
                  NLAY=0
                  ID_SOLID_SIGI(I)  = ID_ELEM
                  IUSER = 1
                  SIGSP(NVSOLID1 + NVSOLID2 +1 , I) = ISOLNOD
                  SIGSP(NVSOLID1 + NVSOLID2 +2 , I) = NPT
                  SIGSP(NVSOLID1 + NVSOLID2 +3 , I) = NUVAR
                  SIGSP(NVSOLID1 + NVSOLID2 +4 , I) = JJHBE
!
                  CALL LEC_INISTATE_D00_BRICK_CHECK (
     1                 IXS            ,IGEO          ,ITRIS         ,ISOLNODD00    ,IE            ,
     2                 NPT            ,NLAY          ,ISOLNOD       ,JJHBE         ,IGTYP         ,
     3                 ISROT          ,KEYWORD )
!
                  IMAT = IXS(1,IE)
                  ILAW = IPM(2,IMAT)
                  NUVARD00 = IPM(8,IMAT)
                  IF (NUVARD00 > NUVAR) THEN
                    CALL ANCMSG(MSGID=1121,
     .                          MSGTYPE=MSGWARNING,
     .                          ANMODE=ANINFO,
     .                          I1=ITRIS(IE),
     .                          C1='NUMBER OF USER VARIABLES',
     .                          C2='MATERIAL LAW ',
     .                          I2=IPM(1,IXS(10,IE)),
     .                         C3='/INIBRI/AUX')
                  ENDIF
                  IF ((ILAW == 36 .and. (NUVAR < 4 .or. NUVARD00 > 3) .and. 
     .                 NUVARD00 < NUVAR) .or. 
     .                (ILAW /= 36 .and. ILAW /= 78 .and. ILAW /= 87 .and. ILAW /= 112 .and. NUVARD00 < NUVAR)) THEN
                    CALL ANCMSG(MSGID=695,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO,
     .                          I1=ITRIS(IE),
     .                          C1='NUMBER OF USER VARIABLES',
     .                          C2='MATERIAL LAW ',
     .                          I2=IPM(1,IXS(10,IE)),
     .                          C3='/INIBRI/AUX')
                  ENDIF
!
                  NMAX_AUX = NPT*NUVAR
                  CALL HM_GET_FLOAT_ARRAY('V',TMPVAL,NMAX_AUX,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                  DO JJ=1,NPT
                    IIS = NVSOLID1 + NVSOLID2 + 4 + NUVAR*(JJ - 1)
                    DO K=1,NUVAR
                      L = NUVAR*(JJ-1) + K
                      SIGSP(IIS + K,I) = TMPVAL(L)
                    ENDDO ! DO K=1,NUVAR
                  ENDDO ! DO JJ=1,NUM_LINES
                ENDIF ! IF (IE == 0) THEN 
              ENDDO ! DO J=1,NB_ELEMENTS
C---------
            CASE ( 'STRS_FGLO' )
C---------
              KEYWORD='/INIBRI/STRS_FG'
              IGTYP = 0
              BRIGLOB = 1
!
              CALL HM_GET_INTV('inibri_strs_fglo_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                ! Reading  ---  ID_ELEM, NIP, NPG, THK  ---
                CALL HM_GET_INT_ARRAY_INDEX('brick_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('Nb_integr',NPT,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('Isolnod',ISOLNOD,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('Isolid',JJHBE,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('nptr',NPTR,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('npts',NPTS,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('nptt',NPTT,J,IS_AVAILABLE,LSUBMODEL)
!     need to be added (cfg + doc)
                CALL HM_GET_INT_ARRAY_INDEX('nlay',NLAY,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('grbric_ID',IGBR,J,IS_AVAILABLE,LSUBMODEL)
!
                IF (JJHBE == 2) JJHBE = 1
                IF (IGBR > 0) THEN
                  DO K=1,NGRBRIC
                    IF (IGBR == IGRBRIC(K)%ID) THEN
                      JGBR = K
                      EXIT
                    ENDIF
                  ENDDO
                  IE = IGRBRIC(JGBR)%ENTITY(1)
                  ID_ELEM = IXS(NIXS,IE)
                ENDIF
!
!
!                ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISOLM,NUMELS)
!                IE = MAP_TABLES%ISOLM(ELT,2)
!
                IE=UEL2SYS(ID_ELEM,KSYSUSRS,NUMELS)
!
                IF (IE == 0) THEN        
                  ! Solid was not found. Issue a Warning & Skip.
                  NONEXIST = NONEXIST+1
                ELSEIF (STRSGLOB(IE) >= 0) THEN   
! ---     treated already
                ELSE
!
                  I=I+1
                  ID_SOLID_SIGI(I) = ID_ELEM
                  SIGSP(2,I) = NPT
                  ! STRSGLOB(IE)=0 -> stress read in element system
                  ! STRSGLOB(IE)=1 -> stress read in global reference system
                  IF (BRIGLOB == 1) STRSGLOB(IE)=1
!
                  CALL LEC_INISTATE_D00_BRICK_CHECK (
     1              IXS            ,IGEO          ,ITRIS         ,ISOLNODD00    ,IE            ,
     2              NPT            ,NLAY          ,ISOLNOD       ,JJHBE         ,IGTYP         ,
     3              ISROT          ,KEYWORD )
!---------not have to distinquate 2 groupes, can be cleaned later
                  IF ( (ISOLNOD == 8 .AND. (JJHBE==1.OR.JJHBE==2.OR.JJHBE==12.OR.JJHBE==24) 
     .            .AND. IGTYP /= 43) .OR.(ISOLNOD == 4 .AND. ISROT == 0)
     .            .OR.(ISOLNOD == 4 .AND. ISROT == 3)) THEN
!           -----------First Group of solids : 4 lines to be read-----------------
                    IF(ISOLNOD == 8 .AND. JJHBE == 12) THEN
                      SIGSP(2,I) = NPT
                      SIGSP(1,I) = 1
                      DO K=1,6
                        SIGI(K,I) = ZERO
                      ENDDO
                      SIGI(10,I) = ZERO
!
                      SIZE = NPT
                      CALL HM_GET_FLOAT_ARRAY('E_int'     ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('RHO'       ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA1'    ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA2'    ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA3'    ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA12'   ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA23'   ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA31'   ,TMPVAL8,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('EPSILON_p' ,TMPVAL9,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                      DO K=1,NPT
                        IIS= 4 + (K-1)*9
                        EIN = TMPVAL1(K)
                        R0  = TMPVAL2(K)
                        SIGSP(IIS+8,I) = EIN
                        SIGSP(IIS+9,I) = R0
!
                        S(1) = TMPVAL3(K)
                        S(2) = TMPVAL4(K)
                        S(3) = TMPVAL5(K)
!
                        S(4) = TMPVAL6(K)
                        S(5) = TMPVAL7(K)
                        S(6) = TMPVAL8(K)
                        IF (SUB_ID /= 0) CALL SUBROTTENS(S,RTRANS,SUB_ID,LSUBMODEL)
                        SIGSP(IIS+1,I) = S(1)
                        SIGSP(IIS+2,I) = S(2)
                        SIGSP(IIS+3,I) = S(3)
                        SIGSP(IIS+4,I) = S(4)
                        SIGSP(IIS+5,I) = S(5)
                        SIGSP(IIS+6,I) = S(6)
!
                        EPSP = TMPVAL9(K)
                        SIGSP(IIS+7,I) = EPSP
                      ENDDO ! DO K=1,NPT
                    ELSE ! IF(ISOLNOD == 8 .AND. JJHBE == 12)
!
                      SIZE = NPT
                      CALL HM_GET_FLOAT_ARRAY('E_int'     ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('RHO'       ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA1'    ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA2'    ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA3'    ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA12'   ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA23'   ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA31'   ,TMPVAL8,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('EPSILON_p' ,TMPVAL9,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                      SIGSP(2,I) = NPT
                      IF (NPT == 8) THEN
                        SIGSP(1,I) = 1
                        SIGI(8,I) = TMPVAL2(1)
                        SIGI(9,I) = TMPVAL1(1)
                      ELSEIF (NPT == 1) THEN
                        SIGI(8,I) = TMPVAL2(1)
                        SIGI(9,I) = TMPVAL1(1)
                      ENDIF
                      SIGSP(3,I) = TMPVAL1(1)
                      SIGSP(4,I) = TMPVAL2(1)
!
                      IF (NPT == 1) THEN
                        S(1) = TMPVAL3(1)
                        S(2) = TMPVAL4(1)
                        S(3) = TMPVAL5(1)
                        S(4) = TMPVAL6(1)
                        S(5) = TMPVAL7(1)
                        S(6) = TMPVAL8(1)
                        IF (SUB_ID /= 0) CALL SUBROTTENS(S,RTRANS,SUB_ID,LSUBMODEL)
                        SIGI(1,I) = S(1)
                        SIGI(2,I) = S(2)
                        SIGI(3,I) = S(3)
                        SIGI(4,I) = S(4)
                        SIGI(5,I) = S(5)
                        SIGI(6,I) = S(6)
                        SIGI(10,I)= TMPVAL9(1)
                      ELSE ! NPT /= 1
                        DO K=1,6
                          SIGI(K,I) = ZERO
                        ENDDO
                        SIGI(10,I) = ZERO
                        DO K=1,NPT
                          IIS= 4 + (K-1)*7
                          S(1) = TMPVAL3(K)
                          S(2) = TMPVAL4(K)
                          S(3) = TMPVAL5(K)
                          SIGSP(IIS+1,I) = S(1)
                          SIGSP(IIS+2,I) = S(2)
                          SIGSP(IIS+3,I) = S(3)
                          S(4) = TMPVAL6(K)
                          S(5) = TMPVAL7(K)
                          S(6) = TMPVAL8(K)
                          IF (SUB_ID /= 0) CALL SUBROTTENS(S,RTRANS,SUB_ID,LSUBMODEL)
                          SIGSP(IIS+1,I) = S(1)
                          SIGSP(IIS+2,I) = S(2)
                          SIGSP(IIS+3,I) = S(3)
                          SIGSP(IIS+4,I) = S(4)
                          SIGSP(IIS+5,I) = S(5)
                          SIGSP(IIS+6,I) = S(6)
                          EPSP = TMPVAL9(K)
                          SIGSP(IIS+7,I) = EPSP
                          DO L=1,6
                            SIGI(L,I) = SIGI(L,I) + FOURTH*SIGSP(IIS+L,I)
                          ENDDO
                          SIGI(10,I)= SIGI(10,I) + FOURTH*SIGSP(IIS+7,I)
                        ENDDO ! DO K=1,NPT
                      ENDIF ! NPT
                    ENDIF ! IF(ISOLNOD == 8 .AND. JJHBE == 12)
                  !---
                  ELSE
                  !---
!          --------------------Second Group of solids : 3 lines to be read-----------------
                    IF (ISOLNOD == 16) THEN
!
                      SIZE = NPTT*NPTR*NLAY
                      CALL HM_GET_FLOAT_ARRAY('SIGMA1'    ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA2'    ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA3'    ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA12'   ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA23'   ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA31'   ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('EPSILON_p' ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('E_int'     ,TMPVAL8,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('RHO'       ,TMPVAL9,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                      KK = 0
                      DO JT=1,NPTT
                        DO JR=1,NPTR
                          DO JL=1,NLAY
                            K = JR + ( (JL-1) + (JT-1)*NLAY )*NPTR
                            IIS = 1 + (K-1)*9
!
                            KK = KK + 1
!
                            S(1) = TMPVAL1(KK)
                            S(2) = TMPVAL2(KK)
                            S(3) = TMPVAL3(KK)
                            S(4) = TMPVAL4(KK)
                            S(5) = TMPVAL5(KK)
                            S(6) = TMPVAL6(KK)
                            IF (SUB_ID /= 0) CALL SUBROTTENS(S,RTRANS,SUB_ID,LSUBMODEL)
                            SIGSP(IIS+1,I) = S(1)
                            SIGSP(IIS+2,I) = S(2)
                            SIGSP(IIS+3,I) = S(3)
                            SIGSP(IIS+4,I) = S(4)
                            SIGSP(IIS+5,I) = S(5)
                            SIGSP(IIS+6,I) = S(6)
                            EPSP = TMPVAL7(KK)
                            EIN  = TMPVAL8(KK)
                            R0   = TMPVAL9(KK)
                            SIGSP(IIS+7,I) = EPSP
                            SIGSP(IIS+8,I) = EIN
                            SIGSP(IIS+9,I) = R0
                          ENDDO ! DO JL=1,NLAY
                        ENDDO ! DO JR=1,NPTR
                      ENDDO ! DO JT=1,NPTT
!
                    ELSEIF (ISOLNOD == 20) THEN
!
                      SIZE = NPTT*NPTS*NPTR
                      CALL HM_GET_FLOAT_ARRAY('SIGMA1'    ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA2'    ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA3'    ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA12'   ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA23'   ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA31'   ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('EPSILON_p' ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('E_int'     ,TMPVAL8,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('RHO'       ,TMPVAL9,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!
                      KK = 0
!
                      DO JT=1,NPTT
                        DO JS=1,NPTS
                          DO JR=1,NPTR
                            K = JR + ( (JS-1) + (JT-1)*NPTS )*NPTR
                            IIS = 1 + (K-1)*9
!
                            KK = KK + 1
!
                            S(1) = TMPVAL1(KK)
                            S(2) = TMPVAL2(KK)
                            S(3) = TMPVAL3(KK)
                            S(4) = TMPVAL4(KK)
                            S(5) = TMPVAL5(KK)
                            S(6) = TMPVAL6(KK)
                            IF (SUB_ID /= 0) CALL SUBROTTENS(S,RTRANS,SUB_ID,LSUBMODEL)
                            SIGSP(IIS+1,I) = S(1)
                            SIGSP(IIS+2,I) = S(2)
                            SIGSP(IIS+3,I) = S(3)
                            SIGSP(IIS+4,I) = S(4)
                            SIGSP(IIS+5,I) = S(5)
                            SIGSP(IIS+6,I) = S(6)
                            EPSP = TMPVAL7(KK)
                            EIN  = TMPVAL8(KK)
                            R0   = TMPVAL9(KK)
                            SIGSP(IIS+7,I) = EPSP
                            SIGSP(IIS+8,I) = EIN
                            SIGSP(IIS+9,I) = R0
                          ENDDO ! DO JT=1,NPTT
                        ENDDO ! DO JS=1,NPTS
                      ENDDO ! DO JR=1,NPTR
!
                    ELSE
!
                      IF (IGTYP == 22) THEN
!
                        SIZE = NPTR*NPTS*NPTT
                        CALL HM_GET_FLOAT_ARRAY('SIGMA1'    ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('SIGMA2'    ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('SIGMA3'    ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('SIGMA12'   ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('SIGMA23'   ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('SIGMA31'   ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('EPSILON_p' ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('E_int'     ,TMPVAL8,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('RHO'       ,TMPVAL9,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!
                        KK = 0
!
                        DO JR=1,NPTR
                          DO JS=1,NPTS
                            DO JT=1,NPTT
                              K = JR + ( (JS-1) + (JT-1)*NPTS )*NPTR
                              IIS = 1 + (K-1)*9
!
                              KK = KK + 1
!
                              S(1) = TMPVAL1(KK)
                              S(2) = TMPVAL2(KK)
                              S(3) = TMPVAL3(KK)
                              S(4) = TMPVAL4(KK)
                              S(5) = TMPVAL5(KK)
                              S(6) = TMPVAL6(KK)
                              IF (SUB_ID /= 0) CALL SUBROTTENS(S,RTRANS,SUB_ID,LSUBMODEL)
                              SIGSP(IIS+1,I) = S(1)
                              SIGSP(IIS+2,I) = S(2)
                              SIGSP(IIS+3,I) = S(3)
                              SIGSP(IIS+4,I) = S(4)
                              SIGSP(IIS+5,I) = S(5)
                              SIGSP(IIS+6,I) = S(6)
                              EPSP = TMPVAL7(KK)
                              EIN  = TMPVAL8(KK)
                              R0   = TMPVAL9(KK)
                              SIGSP(IIS+7,I) = EPSP
                              SIGSP(IIS+8,I) = EIN
                              SIGSP(IIS+9,I) = R0
                            ENDDO ! DO JT=1,NPTT
                          ENDDO ! DO JS=1,NPTS
                        ENDDO ! DO JR=1,NPTR
!
                      ELSE  ! (IGTYP /= 22)
!
                        SIZE = NPT
                        CALL HM_GET_FLOAT_ARRAY('SIGMA1'    ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('SIGMA2'    ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('SIGMA3'    ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('SIGMA12'   ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('SIGMA23'   ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('SIGMA31'   ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('EPSILON_p' ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('E_int'     ,TMPVAL8,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('RHO'       ,TMPVAL9,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                        DO K=1,NPT
                          IIS= 1 + (K-1)*9
                          S(1) = TMPVAL1(K)
                          S(2) = TMPVAL2(K)
                          S(3) = TMPVAL3(K)
                          S(4) = TMPVAL4(K)
                          S(5) = TMPVAL5(K)
                          S(6) = TMPVAL6(K)
                          IF (SUB_ID /= 0) CALL SUBROTTENS(S,RTRANS,SUB_ID,LSUBMODEL)
                          SIGSP(IIS+1,I) = S(1)
                          SIGSP(IIS+2,I) = S(2)
                          SIGSP(IIS+3,I) = S(3)
                          SIGSP(IIS+4,I) = S(4)
                          SIGSP(IIS+5,I) = S(5)
                          SIGSP(IIS+6,I) = S(6)
                          EPSP = TMPVAL7(K)
                          EIN  = TMPVAL8(K)
                          R0   = TMPVAL9(K)
                          SIGSP(IIS+7,I) = EPSP
                          SIGSP(IIS+8,I) = EIN
                          SIGSP(IIS+9,I) = R0
                        ENDDO ! DO K=1,NPT
                      ENDIF ! IF (IGTYP == 22)
                    ENDIF ! IF (ISOLNOD == 16)
                  ENDIF ! IF ( (ISOLNOD == 8 .AND. ...)
                ENDIF ! IF (IE == 0)
!
                IF (IGBR > 0) THEN
                  I1 = I
                  DO K = 2,IGRBRIC(JGBR)%NENTITY
                    IE = IGRBRIC(JGBR)%ENTITY(K)
                    CALL LEC_INISTATE_D00_BRICK_CHECK (
     1                  IXS            ,IGEO          ,ITRIS         ,ISOLNODD00    ,IE            ,
     2                  NPT            ,NLAY          ,ISOLNOD       ,JJHBE         ,IGTYP         ,
     3                  ISROT          ,KEYWORD )
                    IF (STRSGLOB(IE) >= 0) CYCLE  
                    IF (BRIGLOB == 1) STRSGLOB(IE)=1
                    I = I+1
                    ID_SOLID_SIGI(I) = IXS(11,IE)
                    DO L = 1,NSIGI
                      SIGSP(L,I) = SIGSP(L,I1)
                    ENDDO
                    DO L = 1,NSIGS
                      SIGI(L,I) = SIGI(L,I1)
                    ENDDO
                  ENDDO ! DO K = 2,IGRBRIC(JGBR)%NENTITY
                ENDIF ! IF (IGBR > 0)
!
              ENDDO ! DO J=1,NB_ELEMENTS
C---------
            CASE ( 'STRS_F' )
C---------
              KEYWORD='/INIBRI/STRS_F '
              IGTYP = 0
!
              CALL HM_GET_INTV('inibri_strs_f_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                ! Reading  ---  ID_ELEM, NIP, NPG, THK  ---
                CALL HM_GET_INT_ARRAY_INDEX('brick_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('Nb_integr',NPT,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('Isolnod',ISOLNOD,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('Isolid',JJHBE,J,IS_AVAILABLE,LSUBMODEL)
!     need to be added (cfg + doc)
                CALL HM_GET_INT_ARRAY_INDEX('nptr',NPTR,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('npts',NPTS,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('nptt',NPTT,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('nlay',NLAY,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('grbric_ID',IGBR,J,IS_AVAILABLE,LSUBMODEL)
!
                IF (IGBR > 0) THEN
                  DO K=1,NGRBRIC
                    IF (IGBR == IGRBRIC(K)%ID) THEN
                      JGBR = K
                      EXIT
                    ENDIF
                  ENDDO
                  IE = IGRBRIC(JGBR)%ENTITY(1)
                  ID_ELEM = IXS(NIXS,IE)
                ENDIF
!
!                ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISOLM,NUMELS)
!                IE = MAP_TABLES%ISOLM(ELT,2)
!
                IE=UEL2SYS(ID_ELEM,KSYSUSRS,NUMELS)
!
                IF (IE == 0) THEN        
                  ! Solid was not found. Issue a Warning & Skip.
                  NONEXIST = NONEXIST+1
                ELSEIF (STRSGLOB(IE) >= 0) THEN   
! ---     treated already
                ELSE
!
                  I=I+1
                  IF (JJHBE == 2) JJHBE = 1
                  ID_SOLID_SIGI(I)  = ID_ELEM
                  SIGSP(2,I) = NPT
!
                  CALL LEC_INISTATE_D00_BRICK_CHECK (
     1              IXS            ,IGEO          ,ITRIS         ,ISOLNODD00    ,IE            ,
     2              NPT            ,NLAY          ,ISOLNOD       ,JJHBE         ,IGTYP         ,
     3              ISROT          ,KEYWORD )
!
                  STRSGLOB(IE) = 0
                  IF ( (ISOLNOD == 8 .AND. (JJHBE==1.OR.JJHBE==2.OR.JJHBE==12.OR.JJHBE==24) 
     .            .AND. IGTYP /= 43) .OR.(ISOLNOD == 4 .AND. ISROT == 0)
     .            .OR.(ISOLNOD == 4 .AND. ISROT == 3)) THEN
!           -----------First Group of solids : 4 lines to be read-----------------
                    IF(ISOLNOD == 8 .AND. JJHBE == 12) THEN
                      SIGSP(2,I) = NPT
                      SIGSP(1,I) = 1
                      DO K=1,6
                        SIGI(K,I) = ZERO
                      ENDDO
                      SIGI(10,I) = ZERO
!
                      SIZE = NPT
                      CALL HM_GET_FLOAT_ARRAY('E_int'     ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('RHO'       ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA1'    ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA2'    ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA3'    ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA12'   ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA23'   ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA31'   ,TMPVAL8,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('EPSILON_p' ,TMPVAL9,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                      DO K=1,NPT
                        IIS= 4 + (K-1)*9
                        EIN = TMPVAL1(K)
                        R0  = TMPVAL2(K)
                        SIGSP(IIS+8,I) = EIN
                        SIGSP(IIS+9,I) = R0
!
                        S(1) = TMPVAL3(K)
                        S(2) = TMPVAL4(K)
                        S(3) = TMPVAL5(K)
!
                        S(4) = TMPVAL6(K)
                        S(5) = TMPVAL7(K)
                        S(6) = TMPVAL8(K)
!
                        SIGSP(IIS+1,I) = S(1)
                        SIGSP(IIS+2,I) = S(2)
                        SIGSP(IIS+3,I) = S(3)
                        SIGSP(IIS+4,I) = S(4)
                        SIGSP(IIS+5,I) = S(5)
                        SIGSP(IIS+6,I) = S(6)
!
                        EPSP = TMPVAL9(K)
                        SIGSP(IIS+7,I) = EPSP
                      ENDDO ! DO K=1,NPT
                    ELSE ! IF(ISOLNOD == 8 .AND. JJHBE == 12)
!
                      SIZE = NPT
                      CALL HM_GET_FLOAT_ARRAY('E_int'     ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('RHO'       ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA1'    ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA2'    ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA3'    ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA12'   ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA23'   ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA31'   ,TMPVAL8,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('EPSILON_p' ,TMPVAL9,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                      SIGSP(2,I) = NPT
                      IF(NPT == 8)THEN
                        SIGSP(1,I) = 1
                        SIGI(8,I) =   TMPVAL2(1)
                        SIGI(9,I) =   TMPVAL1(1)
                      ELSEIF(NPT == 1) THEN
                        SIGI(8,I) =   TMPVAL2(1)
                        SIGI(9,I) =   TMPVAL1(1)
                      ENDIF
                      SIGSP(3,I) = TMPVAL1(1)
                      SIGSP(4,I) = TMPVAL2(1)
!
                      IF (NPT == 1) THEN
!
                        S(1) = TMPVAL3(1)
                        S(2) = TMPVAL4(1)
                        S(3) = TMPVAL5(1)
                        S(4) = TMPVAL6(1)
                        S(5) = TMPVAL7(1)
                        S(6) = TMPVAL8(1)
!
                        SIGI(1,I) = S(1)
                        SIGI(2,I) = S(2)
                        SIGI(3,I) = S(3)
                        SIGI(4,I) = S(4)
                        SIGI(5,I) = S(5)
                        SIGI(6,I) = S(6)
                        SIGI(10,I)= TMPVAL9(1)
                      ELSE ! NPT /= 1
                        DO K=1,6
                          SIGI(K,I) = ZERO
                        ENDDO
                        SIGI(10,I) = ZERO
!
                        DO K=1,NPT
                          IIS= 4 + (K-1)*7
                          S(1) = TMPVAL3(K)
                          S(2) = TMPVAL4(K)
                          S(3) = TMPVAL5(K)
                          SIGSP(IIS+1,I) = S(1)
                          SIGSP(IIS+2,I) = S(2)
                          SIGSP(IIS+3,I) = S(3)
                          S(4) = TMPVAL6(K)
                          S(5) = TMPVAL7(K)
                          S(6) = TMPVAL8(K)
!
                          SIGSP(IIS+1,I) = S(1)
                          SIGSP(IIS+2,I) = S(2)
                          SIGSP(IIS+3,I) = S(3)
                          SIGSP(IIS+4,I) = S(4)
                          SIGSP(IIS+5,I) = S(5)
                          SIGSP(IIS+6,I) = S(6)
                          EPSP = TMPVAL9(K)
                          SIGSP(IIS+7,I) = EPSP
                          DO L=1,6
                            SIGI(L,I) = SIGI(L,I) + FOURTH*SIGSP(IIS+L,I)
                          ENDDO
                          SIGI(10,I)= SIGI(10,I) + FOURTH*SIGSP(IIS+7,I)
                        ENDDO ! DO K=1,NPT
                      ENDIF ! NPT
                    ENDIF ! IF(ISOLNOD == 8 .AND. JJHBE == 12)
                  !---
                  ELSE
                  !---
!          --------------------Second Group of solids : 3 lines to be read-----------------
                    IF (ISOLNOD == 16) THEN
!
                      SIZE = NPTT*NPTR*NLAY
                      CALL HM_GET_FLOAT_ARRAY('SIGMA1'    ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA2'    ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA3'    ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA12'   ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA23'   ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA31'   ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('EPSILON_p' ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('E_int'     ,TMPVAL8,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('RHO'       ,TMPVAL9,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!
                      KK = 0
!
                      DO JT=1,NPTT
                        DO JR=1,NPTR
                          DO JL=1,NLAY
                            K = JR + ( (JL-1) + (JT-1)*NLAY )*NPTR
                            IIS = 1 + (K-1)*9
!
                            KK = KK + 1
!
                            S(1) = TMPVAL1(KK)
                            S(2) = TMPVAL2(KK)
                            S(3) = TMPVAL3(KK)
                            S(4) = TMPVAL4(KK)
                            S(5) = TMPVAL5(KK)
                            S(6) = TMPVAL6(KK)
!
                            SIGSP(IIS+1,I) = S(1)
                            SIGSP(IIS+2,I) = S(2)
                            SIGSP(IIS+3,I) = S(3)
                            SIGSP(IIS+4,I) = S(4)
                            SIGSP(IIS+5,I) = S(5)
                            SIGSP(IIS+6,I) = S(6)
                            EPSP = TMPVAL7(KK)
                            EIN  = TMPVAL8(KK)
                            R0   = TMPVAL9(KK)
                            SIGSP(IIS+7,I) = EPSP
                            SIGSP(IIS+8,I) = EIN
                            SIGSP(IIS+9,I) = R0
                          ENDDO ! DO JL=1,NLAY
                        ENDDO ! DO JR=1,NPTR
                      ENDDO ! DO JT=1,NPTT
!
                    ELSEIF (ISOLNOD == 20) THEN
!
                      SIZE = NPTT*NPTS*NPTR
                      CALL HM_GET_FLOAT_ARRAY('SIGMA1'    ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA2'    ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA3'    ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA12'   ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA23'   ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('SIGMA31'   ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('EPSILON_p' ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('E_int'     ,TMPVAL8,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('RHO'       ,TMPVAL9,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!
                      KK = 0
!
                      DO JT=1,NPTT
                        DO JS=1,NPTS
                          DO JR=1,NPTR
                            K = JR + ( (JS-1) + (JT-1)*NPTS )*NPTR
                            IIS = 1 + (K-1)*9
!
                            KK = KK + 1
!
                            S(1) = TMPVAL1(KK)
                            S(2) = TMPVAL2(KK)
                            S(3) = TMPVAL3(KK)
                            S(4) = TMPVAL4(KK)
                            S(5) = TMPVAL5(KK)
                            S(6) = TMPVAL6(KK)
!
                            SIGSP(IIS+1,I) = S(1)
                            SIGSP(IIS+2,I) = S(2)
                            SIGSP(IIS+3,I) = S(3)
                            SIGSP(IIS+4,I) = S(4)
                            SIGSP(IIS+5,I) = S(5)
                            SIGSP(IIS+6,I) = S(6)
                            EPSP = TMPVAL7(KK)
                            EIN  = TMPVAL8(KK)
                            R0   = TMPVAL9(KK)
                            SIGSP(IIS+7,I) = EPSP
                            SIGSP(IIS+8,I) = EIN
                            SIGSP(IIS+9,I) = R0
                          ENDDO ! DO JT=1,NPTT
                        ENDDO ! DO JS=1,NPTS
                      ENDDO ! DO JR=1,NPTR
!
                    ELSE
!
                      IF (IGTYP == 22) THEN
!
                        SIZE = NPTR*NPTS*NPTT
                        CALL HM_GET_FLOAT_ARRAY('SIGMA1'    ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('SIGMA2'    ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('SIGMA3'    ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('SIGMA12'   ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('SIGMA23'   ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('SIGMA31'   ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('EPSILON_p' ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('E_int'     ,TMPVAL8,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('RHO'       ,TMPVAL9,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!
                        KK = 0
! 
                        DO JR=1,NPTR
                          DO JS=1,NPTS
                            DO JT=1,NPTT
                              K = JR + ( (JS-1) + (JT-1)*NPTS )*NPTR
                              IIS = 1 + (K-1)*9
!
                              KK = KK + 1
!
                              S(1) = TMPVAL1(KK)
                              S(2) = TMPVAL2(KK)
                              S(3) = TMPVAL3(KK)
                              S(4) = TMPVAL4(KK)
                              S(5) = TMPVAL5(KK)
                              S(6) = TMPVAL6(KK)
!
                              SIGSP(IIS+1,I) = S(1)
                              SIGSP(IIS+2,I) = S(2)
                              SIGSP(IIS+3,I) = S(3)
                              SIGSP(IIS+4,I) = S(4)
                              SIGSP(IIS+5,I) = S(5)
                              SIGSP(IIS+6,I) = S(6)
                              EPSP = TMPVAL7(KK)
                              EIN  = TMPVAL8(KK)
                              R0   = TMPVAL9(KK)
                              SIGSP(IIS+7,I) = EPSP
                              SIGSP(IIS+8,I) = EIN
                              SIGSP(IIS+9,I) = R0
                            ENDDO ! DO JT=1,NPTT
                          ENDDO ! DO JS=1,NPTS
                        ENDDO ! DO JR=1,NPTR
!
                      ELSE  ! (IGTYP /= 22)
!
                        SIZE = NPT
                        CALL HM_GET_FLOAT_ARRAY('SIGMA1'    ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('SIGMA2'    ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('SIGMA3'    ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('SIGMA12'   ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('SIGMA23'   ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('SIGMA31'   ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('EPSILON_p' ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('E_int'     ,TMPVAL8,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('RHO'       ,TMPVAL9,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                        DO K=1,NPT
                          IIS= 1 + (K-1)*9
                          S(1) = TMPVAL1(K)
                          S(2) = TMPVAL2(K)
                          S(3) = TMPVAL3(K)
                          S(4) = TMPVAL4(K)
                          S(5) = TMPVAL5(K)
                          S(6) = TMPVAL6(K)
!
                          SIGSP(IIS+1,I) = S(1)
                          SIGSP(IIS+2,I) = S(2)
                          SIGSP(IIS+3,I) = S(3)
                          SIGSP(IIS+4,I) = S(4)
                          SIGSP(IIS+5,I) = S(5)
                          SIGSP(IIS+6,I) = S(6)
                          EPSP = TMPVAL7(K)
                          EIN  = TMPVAL8(K)
                          R0   = TMPVAL9(K)
                          SIGSP(IIS+7,I) = EPSP
                          SIGSP(IIS+8,I) = EIN
                          SIGSP(IIS+9,I) = R0
                        ENDDO ! DO K=1,NPT
                      ENDIF ! IF (IGTYP == 22)
                    ENDIF ! IF (ISOLNOD == 16)
                  ENDIF ! IF ( (ISOLNOD == 8 .AND. ...)
                ENDIF ! IF (IE == 0)
!
                IF (IGBR > 0) THEN
                  I1 = I
                  DO K = 2,IGRBRIC(JGBR)%NENTITY
                    IE = IGRBRIC(JGBR)%ENTITY(K)
                    CALL LEC_INISTATE_D00_BRICK_CHECK (
     1                  IXS            ,IGEO          ,ITRIS         ,ISOLNODD00    ,IE            ,
     2                  NPT            ,NLAY          ,ISOLNOD       ,JJHBE         ,IGTYP         ,
     3                  ISROT          ,KEYWORD )
!!                    IF (BRIGLOB == 1) STRSGLOB(IE)=1
                    IF (STRSGLOB(IE) >= 0) CYCLE  
                    STRSGLOB(IE)=0
                    I = I+1
                    ID_SOLID_SIGI(I) = IXS(11,IE)
                    DO L = 1,NSIGI
                      SIGSP(L,I) = SIGSP(L,I1)
                    ENDDO
                    DO L = 1,NSIGS
                      SIGI(L,I) = SIGI(L,I1)
                    ENDDO
                  ENDDO ! DO K = 2,IGRBRIC(JGBR)%NENTITY
                ENDIF ! IF (IGBR > 0)
!
              ENDDO ! DO J=1,NB_ELEMENTS

C---------
            CASE ( 'STRA_F' )
C---------
              CALL HM_GET_INTV('inibri_stra_f_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              KEYWORD='/INIBRI/STRA_F '
              IGTYP = 0
!
              DO J=1,NB_ELEMENTS
                ! Reading  ---  ID_ELEM, ....  ---
                CALL HM_GET_INT_ARRAY_INDEX('brick_ID' ,ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('Nb_integr',NPT,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('Isolnod'  ,ISOLNOD,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('Isolid'   ,JJHBE,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('nptr'     ,NPTR,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('npts'     ,NPTS,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('nptt'     ,NPTT,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('nlay'     ,NLAY,J,IS_AVAILABLE,LSUBMODEL)
!
                I=I+1
                IF (JJHBE == 2) JJHBE = 1
                ID_SOLID_SIGI(I)  = ID_ELEM
!
!                ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISOLM,NUMELS)
!                IE = MAP_TABLES%ISOLM(ELT,2)
!
                IE=UEL2SYS(ID_ELEM,KSYSUSRS,NUMELS)
C         STRAGLOB(IE)=0 -> strain read in element system
C         STRAGLOB(IE)=1 -> strain read in global reference system
C         STRAGLOB(IE)=10-> reference configuration /INIBRI/EREF
!
                IF (IE == 0) THEN        
                  ! Solid was not found. Issue a Warning & Skip.
                  NONEXIST = NONEXIST+1
                ELSEIF (STRAGLOB(IE)>=0) THEN        
                ELSE
                  CALL LEC_INISTATE_D00_BRICK_CHECK (
     1                 IXS            ,IGEO          ,ITRIS         ,ISOLNODD00    ,IE            ,
     2                 NPT            ,NLAY          ,ISOLNOD       ,JJHBE         ,IGTYP         ,
     3                 ISROT          ,KEYWORD )
!
                  STRAGLOB(IE) = 0
                  IF ( ISOLNOD == 16 ) THEN
!
                    SIZE = NPTT*NPTR*NLAY
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_1'    ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_2'    ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_3'    ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_12'   ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_23'   ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_31'   ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!
                    KK = 0
!
                    DO JT=1,NPTT
                      DO JR=1,NPTR
                        DO JL=1,NLAY
                          K = JR + ( (JL-1) + (JT-1)*NLAY )*NPTR
                          IIS= NVSOLID1 + (K-1)*6
!
                          KK = KK + 1
!
                          S(1) = TMPVAL1(KK)
                          S(2) = TMPVAL2(KK)
                          S(3) = TMPVAL3(KK)
                          S(4) = TMPVAL4(KK)
                          S(5) = TMPVAL5(KK)
                          S(6) = TMPVAL6(KK)
!
                          SIGSP(IIS+1,I) = S(1)
                          SIGSP(IIS+2,I) = S(2)
                          SIGSP(IIS+3,I) = S(3)
                          SIGSP(IIS+4,I) = S(4)
                          SIGSP(IIS+5,I) = S(5)
                          SIGSP(IIS+6,I) = S(6)
                        ENDDO ! DO JL=1,NLAY
                      ENDDO ! DO JR=1,NPTR
                    ENDDO ! DO JT=1,NPTT
!
                  ELSEIF ( ISOLNOD == 20 ) THEN
!
                    SIZE = NPTT*NPTS*NPTR
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_1'    ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_2'    ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_3'    ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_12'   ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_23'   ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_31'   ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!
                    KK = 0
!
                    DO JT=1,NPTT
                      DO JS=1,NPTS
                        DO JR=1,NPTR
                          K = JR + ( (JS-1) + (JT-1)*NPTS )*NPTR
                          IIS= NVSOLID1 + (K-1)*6
!
                          KK = KK + 1
!
                          S(1) = TMPVAL1(KK)
                          S(2) = TMPVAL2(KK)
                          S(3) = TMPVAL3(KK)
                          S(4) = TMPVAL4(KK)
                          S(5) = TMPVAL5(KK)
                          S(6) = TMPVAL6(KK)
!
                          SIGSP(IIS+1,I) = S(1)
                          SIGSP(IIS+2,I) = S(2)
                          SIGSP(IIS+3,I) = S(3)
                          SIGSP(IIS+4,I) = S(4)
                          SIGSP(IIS+5,I) = S(5)
                          SIGSP(IIS+6,I) = S(6)
                        ENDDO ! DO JR=1,NPTR
                      ENDDO ! DO JS=1,NPTS
                    ENDDO ! DO JT=1,NPTT
!
                  ELSEIF ((IGTYP == 21 .OR. IGTYP == 22) .AND. JJHBE == 14) THEN
!
                    SIZE = NPTR*NPTS*NPTT
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_1'    ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_2'    ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_3'    ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_12'   ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_23'   ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_31'   ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!
                    KK = 0
!
                    DO JR=1,NPTR
                      DO JS=1,NPTS
                        DO JT=1,NPTT
                          K = JR + ( (JS-1) + (JT-1)*NPTS )*NPTR
                          IIS= NVSOLID1 + (K-1)*6
!
                          KK = KK + 1
!
                          S(1) = TMPVAL1(KK)
                          S(2) = TMPVAL2(KK)
                          S(3) = TMPVAL3(KK)
                          S(4) = TMPVAL4(KK)
                          S(5) = TMPVAL5(KK)
                          S(6) = TMPVAL6(KK)
!
                          SIGSP(IIS+1,I) = S(1)
                          SIGSP(IIS+2,I) = S(2)
                          SIGSP(IIS+3,I) = S(3)
                          SIGSP(IIS+4,I) = S(4)
                          SIGSP(IIS+5,I) = S(5)
                          SIGSP(IIS+6,I) = S(6)
                        ENDDO ! DO JT=1,NPTT
                      ENDDO ! DO JS=1,NPTS
                    ENDDO ! DO JR=1,NPTR
!
                  ELSE
!
                    SIZE = NPT
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_1'    ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_2'    ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_3'    ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_12'   ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_23'   ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_31'   ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    DO K=1,NPT
                      IIS= NVSOLID1  + (K-1)*6
                      S(1) = TMPVAL1(K)
                      S(2) = TMPVAL2(K)
                      S(3) = TMPVAL3(K)
                      S(4) = TMPVAL4(K)
                      S(5) = TMPVAL5(K)
                      S(6) = TMPVAL6(K)
!
                      SIGSP(IIS+1,I) =S(1)
                      SIGSP(IIS+2,I) =S(2)
                      SIGSP(IIS+3,I) =S(3)
                      SIGSP(IIS+4,I) =S(4)
                      SIGSP(IIS+5,I) =S(5)
                      SIGSP(IIS+6,I) =S(6)
                    ENDDO ! DO K=1,NPT

!
                  ENDIF ! IF ( ISOLNOD == 16 )
                ENDIF ! IF (IE == 0)
              ENDDO ! DO J=1,NB_ELEMENTS

C---------
            CASE ( 'STRA_FGLO' )
C---------
              CALL HM_GET_INTV('inibri_stra_fglo_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              KEYWORD='/INIBRI/STRA_F '
              IGTYP = 0
              BRIGLOB = 1
!
              DO J=1,NB_ELEMENTS
                ! Reading  ---  ID_ELEM, ....  ---
                CALL HM_GET_INT_ARRAY_INDEX('brick_ID' ,ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('Nb_integr',NPT,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('Isolnod'  ,ISOLNOD,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('Isolid'   ,JJHBE,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('nptr'     ,NPTR,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('npts'     ,NPTS,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('nptt'     ,NPTT,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('nlay'     ,NLAY,J,IS_AVAILABLE,LSUBMODEL)
!
                I=I+1
                IF (JJHBE == 2) JJHBE = 1
                ID_SOLID_SIGI(I)  = ID_ELEM
!
!                ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISOLM,NUMELS)
!                IE = MAP_TABLES%ISOLM(ELT,2)
!
                IE=UEL2SYS(ID_ELEM,KSYSUSRS,NUMELS)
C         STRAGLOB(IE)=0 -> strain read in element system
C         STRAGLOB(IE)=1 -> strain read in global reference system
C         STRAGLOB(IE)=10-> reference configuration /INIBRI/EREF
!
!
                IF (IE == 0) THEN        
                  ! Solid was not found. Issue a Warning & Skip.
                  NONEXIST = NONEXIST+1
                ELSEIF (STRAGLOB(IE)>=0) THEN        
                ELSE
                  CALL LEC_INISTATE_D00_BRICK_CHECK (
     1                 IXS            ,IGEO          ,ITRIS         ,ISOLNODD00    ,IE            ,
     2                 NPT            ,NLAY          ,ISOLNOD       ,JJHBE         ,IGTYP         ,
     3                 ISROT          ,KEYWORD )
                  IF (BRIGLOB == 1) STRAGLOB(IE)=1
!
                  IF ( ISOLNOD == 16 ) THEN
!
                    SIZE = NPTT*NPTR*NLAY
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_1'    ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_2'    ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_3'    ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_12'   ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_23'   ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_31'   ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!
                    KK = 0
!
                    DO JT=1,NPTT
                      DO JR=1,NPTR
                        DO JL=1,NLAY
                          K = JR + ( (JL-1) + (JT-1)*NLAY )*NPTR
                          IIS= NVSOLID1 + (K-1)*6
!
                          KK = KK + 1
!
                          S(1) = TMPVAL1(KK)
                          S(2) = TMPVAL2(KK)
                          S(3) = TMPVAL3(KK)
                          S(4) = TMPVAL4(KK)
                          S(5) = TMPVAL5(KK)
                          S(6) = TMPVAL6(KK)
                          IF (SUB_ID /= 0) CALL SUBROTTENS(S,RTRANS,SUB_ID,LSUBMODEL)
                          SIGSP(IIS+1,I) = S(1)
                          SIGSP(IIS+2,I) = S(2)
                          SIGSP(IIS+3,I) = S(3)
                          SIGSP(IIS+4,I) = S(4)
                          SIGSP(IIS+5,I) = S(5)
                          SIGSP(IIS+6,I) = S(6)
                        ENDDO ! DO JL=1,NLAY
                      ENDDO ! DO JR=1,NPTR
                    ENDDO ! DO JT=1,NPTT
!
                  ELSEIF ( ISOLNOD == 20 ) THEN
!
                    SIZE = NPTT*NPTS*NPTR
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_1'    ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_2'    ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_3'    ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_12'   ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_23'   ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_31'   ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!
                    KK = 0
!
                    DO JT=1,NPTT
                      DO JS=1,NPTS
                        DO JR=1,NPTR
                          K = JR + ( (JS-1) + (JT-1)*NPTS )*NPTR
                          IIS= NVSOLID1 + (K-1)*6
!
                          KK = KK + 1
!
                          S(1) = TMPVAL1(KK)
                          S(2) = TMPVAL2(KK)
                          S(3) = TMPVAL3(KK)
                          S(4) = TMPVAL4(KK)
                          S(5) = TMPVAL5(KK)
                          S(6) = TMPVAL6(KK)
                          IF (SUB_ID /= 0) CALL SUBROTTENS(S,RTRANS,SUB_ID,LSUBMODEL)
                          SIGSP(IIS+1,I) = S(1)
                          SIGSP(IIS+2,I) = S(2)
                          SIGSP(IIS+3,I) = S(3)
                          SIGSP(IIS+4,I) = S(4)
                          SIGSP(IIS+5,I) = S(5)
                          SIGSP(IIS+6,I) = S(6)
                        ENDDO ! DO JR=1,NPTR
                      ENDDO ! DO JS=1,NPTS
                    ENDDO ! DO JT=1,NPTT
!
                  ELSEIF ((IGTYP == 21 .OR. IGTYP == 22) .AND. JJHBE == 14) THEN
!
                    SIZE = NPTR*NPTS*NPTT
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_1'    ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_2'    ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_3'    ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_12'   ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_23'   ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_31'   ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    KK = 0
!
                    DO JR=1,NPTR
                      DO JS=1,NPTS
                        DO JT=1,NPTT
                          K = JR + ( (JS-1) + (JT-1)*NPTS )*NPTR
                          IIS= NVSOLID1 + (K-1)*6
!
                          KK = KK + 1
!
                          S(1) = TMPVAL1(KK)
                          S(2) = TMPVAL2(KK)
                          S(3) = TMPVAL3(KK)
                          S(4) = TMPVAL4(KK)
                          S(5) = TMPVAL5(KK)
                          S(6) = TMPVAL6(KK)
                          IF (SUB_ID /= 0) CALL SUBROTTENS(S,RTRANS,SUB_ID,LSUBMODEL)
                          SIGSP(IIS+1,I) = S(1)
                          SIGSP(IIS+2,I) = S(2)
                          SIGSP(IIS+3,I) = S(3)
                          SIGSP(IIS+4,I) = S(4)
                          SIGSP(IIS+5,I) = S(5)
                          SIGSP(IIS+6,I) = S(6)
                        ENDDO ! DO JT=1,NPTT
                      ENDDO ! DO JS=1,NPTS
                    ENDDO ! DO JR=1,NPTR
!
                  ELSE
!
                    SIZE = NPT
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_1'    ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_2'    ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_3'    ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_12'   ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_23'   ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('EPSILON_31'   ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    DO K=1,NPT
                      IIS= NVSOLID1  + (K-1)*6
                      S(1) = TMPVAL1(K)
                      S(2) = TMPVAL2(K)
                      S(3) = TMPVAL3(K)
                      S(4) = TMPVAL4(K)
                      S(5) = TMPVAL5(K)
                      S(6) = TMPVAL6(K)
                      IF (SUB_ID /= 0) CALL SUBROTTENS(S,RTRANS,SUB_ID,LSUBMODEL)
                      SIGSP(IIS+1,I) =S(1)
                      SIGSP(IIS+2,I) =S(2)
                      SIGSP(IIS+3,I) =S(3)
                      SIGSP(IIS+4,I) =S(4)
                      SIGSP(IIS+5,I) =S(5)
                      SIGSP(IIS+6,I) =S(6)
                    ENDDO ! DO K=1,NPT

!
                  ENDIF ! IF ( ISOLNOD == 16 )
                ENDIF ! IF (IE == 0)
              ENDDO ! DO J=1,NB_ELEMENTS

C---------
            CASE ( 'FAIL' )
C---------
              CALL HM_GET_INTV('inibri_fail_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                CALL HM_GET_INT_ARRAY_INDEX('brick_ID' ,ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('Nlay',NLAY,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('nptr',NPTR,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('npts',NPTS,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('nptt',NPTT,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('lay_ID',ILAY,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('fail_ID',IFAIL,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('Ifail_typ',IRUPT_TYP,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('Nvar',NVAR_RUPT,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('mat_ID',IMAT,J,IS_AVAILABLE,LSUBMODEL)
!
                CALL HM_GET_INT_ARRAY_INDEX('num_lines',NUM_LINES,J,IS_AVAILABLE,LSUBMODEL)
!
!                ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISOLM,NUMELS)
!                IE = MAP_TABLES%ISOLM(ELT,2)
!
                IE=UEL2SYS(ID_ELEM,KSYSUSRS,NUMELS)
!
                NVMAX = NVSOLID4 /(NPTR*NPTS*NPTT*NLAY*5)
!
                IF(ID_ELEM /= NEM1) I=I+1
                NEM1 = ID_ELEM
                IOK = 0
!
                DO K=1,NUMMAT
                  IF(IPM(1,K) == IMAT)THEN
                    IMAT = K
                    IOK = 1
                    EXIT
                  ENDIF
                ENDDO
!
                IF (IOK == 0) THEN
                  CALL ANCMSG(MSGID=1033,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO,
     .                        I1=ITRIS(IE),
     .                        C1='MATERIAL LAW',
     .                        C2='/INIBRI/FAIL')
                ENDIF
                ID_SOLID_SIGI(I)  = ID_ELEM
!
                IF (IE == 0) THEN        
                  ! Solid was not found. Issue a Warning & Skip.
                  NONEXIST = NONEXIST+1
                ELSE
                  IOK = 0
                  DO K=1,5
                    NFAIL(K) = MAT_PARAM(IMAT)%FAIL(K)%FAIL_ID
                    IF (IFAIL == NFAIL(K) .AND. 
     .                IRUPT_TYP == MAT_PARAM(IMAT)%FAIL(K)%IRUPT) THEN
                      IFAIL = K
                      FAIL_INI(IFAIL)=1
                      IOK = 1
                      EXIT
                    ENDIF
                  ENDDO
                  IF (IOK == 0) THEN
                    CALL ANCMSG(MSGID=1033,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO,
     .                        I1=ITRIS(IE),
     .                        C1='FAILURE CRITERIA',
     .                        C2='/INIBRI/FAIL')
                  ENDIF ! IF (IOK == 0)
!
                  IIS= NVSOLID1  + NVSOLID2 + 4 + NUSOLID + NVSOLID3
!
                  NMAX_FAIL = NUM_LINES*NVAR_RUPT
                  CALL HM_GET_FLOAT_ARRAY('V'  ,TMPVAL,NMAX_FAIL,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    DO JJ=1,NUM_LINES
                      DO K=1,NVAR_RUPT
                        L = NVAR_RUPT*(JJ-1) + K
                        SIGSP(IIS+L+(IFAIL-1)*NLAY*NPTR*NPTS*NPTT*NVMAX+
     .                              (ILAY-1)*NVMAX*NPTR*NPTS*NPTT,I) = TMPVAL(L)
                      ENDDO ! DO K=1,NVAR_RUPT
                    ENDDO ! DO JJ=1,NUM_LINE
!
                ENDIF ! IF (IE == 0)
              ENDDO ! DO J=1,NB_ELEMENTS
C---------
            CASE ( 'SCALE_YLD' )
C---------
                CALL HM_GET_INTV('inibri_scale_yld_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                IUSOLYLD = 1
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('brick_ID' ,ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nptr',NPTR,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npts',NPTS,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nptt',NPTT,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nlay',NLAY,J,IS_AVAILABLE,LSUBMODEL)
!
                  CALL HM_GET_INT_ARRAY_INDEX('num_lines',NUM_LINES,J,IS_AVAILABLE,LSUBMODEL)
!
!                ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISOLM,NUMELS)
!                IE = MAP_TABLES%ISOLM(ELT,2)
!
                  IE=UEL2SYS(ID_ELEM,KSYSUSRS,NUMELS)
!
                  I=I+1
                  IF (NLAY == 0) NLAY = 1
                  SIGI( 7,I)  = ID_ELEM
                  ID_SOLID_SIGI(I)  = ID_ELEM
                  IIS = NVSOLID1 + NVSOLID2 + NVSOLID3 + NUSOLID + 4 + NVSOLID4 
                  SIGSP(IIS + 7,I)  = ID_ELEM
!
                  SIGSP(IIS +1 , I) = NPTR
                  SIGSP(IIS +2 , I) = NPTS
                  SIGSP(IIS +3 , I) = NPTT
                  SIGSP(IIS +4 , I) = NLAY
!
                  IF (IE == 0) THEN        
                    ! Solid was not found. Issue a Warning & Skip.
                    NONEXIST = NONEXIST+1
                  ELSE
                    IIS = NVSOLID1 + NVSOLID2 + NVSOLID3 + NUSOLID + 4 + NVSOLID4 + 7
!
                    SIZE = NLAY*NPTT*NPTS*NPTR
                    CALL HM_GET_FLOAT_ARRAY('Alpha_lkji' ,TMPVAL,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    DO ILAY = 1,NLAY
                      DO IT=1,NPTT
                        DO IS=1,NPTS
                          DO IR=1,NPTR
                            JJ = NPTR*NPTS*NPTT*(ILAY-1)+ NPTR*NPTS*(IT-1)+NPTR*(IS-1)+IR
                            SIGSP(IIS+ JJ ,I) =    TMPVAL(JJ)
                          ENDDO ! DO IR=1,NPTR
                        ENDDO ! DO IS=1,NPTS
                      ENDDO ! DO IT=1,NPTT
                    ENDDO ! DO ILAY = 1,NLAY
!!                    IIS = IIS + NPTR*NPTS*NPTT*NLAY
!
                  ENDIF ! IF (IE == 0)
                ENDDO ! DO J=1,NB_ELEMENTS
C---------
            CASE ( 'ORTHO' )
C---------
                CALL HM_GET_INTV('inibri_ortho_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                KEYWORD='/INIBRI/ORTHO  '
                NPT = 0
!
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('brick_ID' ,ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Nb_layer' ,NLAY,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Isolnod'  ,ISOLNOD,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Prop_type',IGTYP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Isolid'   ,JJHBE,J,IS_AVAILABLE,LSUBMODEL)
!
                  I=I+1
                  ID_SOLID_SIGI(I)  = ID_ELEM
                  IF (JJHBE == 2) JJHBE = 1
!
!                ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISOLM,NUMELS)
!                IE = MAP_TABLES%ISOLM(ELT,2)
!
                  IE=UEL2SYS(ID_ELEM,KSYSUSRS,NUMELS)
!
!
                  IF (IE == 0) THEN        
                    ! Solid was not found. Issue a Warning & Skip.
                    NONEXIST = NONEXIST+1
                  ELSE
                    CALL LEC_INISTATE_D00_BRICK_CHECK (
     1                   IXS            ,IGEO          ,ITRIS         ,ISOLNODD00    ,IE            ,
     2                   NPT            ,NLAY          ,ISOLNOD       ,JJHBE         ,IGTYP         ,
     3                   ISROT          ,KEYWORD )
!
                    IIS= NVSOLID1  + NVSOLID2 + 4 + NUSOLID 
!!            IF(KEY2(8:10)=='GLO'.OR.
!!     .          (IGTYP /= 21 .AND. IGTYP /= 22)) THEN
                    IF (IGTYP /= 21 .AND. IGTYP /= 22) THEN
                      ORTHOGLOB(IE) = 1
                      SIZE = NLAY
                      CALL HM_GET_FLOAT_ARRAY('X1' ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('Y1' ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('Z1' ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('X2' ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('Y2' ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('Z3' ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                      DO K=1,NLAY
                        SIGSP(IIS+1,I) = TMPVAL1(K)
                        SIGSP(IIS+2,I) = TMPVAL2(K)
                        SIGSP(IIS+3,I) = TMPVAL3(K)
                        SIGSP(IIS+4,I) = TMPVAL4(K)
                        SIGSP(IIS+5,I) = TMPVAL5(K)
                        SIGSP(IIS+6,I) = TMPVAL6(K)
                        IIS = IIS + 6
                      ENDDO
                    ELSE
                      SIZE = NLAY
                      CALL HM_GET_FLOAT_ARRAY('cos_ALPHA' ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('sin_ALPHA' ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                      DO K=1,NLAY
                        SIGSP(IIS+1,I) = TMPVAL1(K)
                        SIGSP(IIS+2,I) = TMPVAL2(K)
                        IIS = IIS + 6
                      ENDDO
                    ENDIF ! IF (IGTYP /= 21 .AND. IGTYP /= 22)
!
                  ENDIF ! IF (IE == 0)
                ENDDO ! DO J=1,NB_ELEMENTS

C---------
            CASE ( 'EREF' )
C---------
                CALL HM_GET_INTV('inibri_eref_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                KEYWORD='/INIBRI/EREF '
!
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('brick_ID' ,ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Nb_integr',NPT,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Isolnod'  ,ISOLNOD,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Isolid'   ,JJHBE,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Ismstr'   ,ISMSTR,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Nsrot'   ,NSROT,J,IS_AVAILABLE,LSUBMODEL)
!
                  I=I+1
                  IF (JJHBE == 2) JJHBE = 1
                  ID_SOLID_SIGI(I)  = ID_ELEM
!
!                ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISOLM,NUMELS)
!                IE = MAP_TABLES%ISOLM(ELT,2)
!
                  IE=UEL2SYS(ID_ELEM,KSYSUSRS,NUMELS)
!
!
                  IF (IE == 0) THEN        
                    ! Solid was not found. Issue a Warning & Skip.
                    NONEXIST = NONEXIST+1
                  ELSE
                    CALL LEC_INISTATE_D00_BRICK_CHECK (
     1                   IXS            ,IGEO          ,ITRIS         ,ISOLNODD00    ,IE            ,
     2                   NPT            ,NLAY          ,ISOLNOD       ,JJHBE         ,IGTYP         ,
     3                   ISROT          ,KEYWORD )
C
C---------!!!add check Ismstr
                    PID = IXS(10,IE)
                    NG = IES2IPARG(IE)
                    ISMRAD = IPARG(9,NG)
                    IF (ISMRAD/=ISMSTR.OR.(ISMSTR/=1.AND.ISMSTR<10)) THEN
                      CALL ANCMSG(MSGID=695,
     .                       MSGTYPE=MSGERROR,
     .                       ANMODE=ANINFO,
     .                       I1=ITRIS(IE),
     .                       C1='SMALL STRAIN FORMULATION',
     .                       C2='SOLID PROPERTY',
     .                       I2=IGEO(1,PID),
     .                       C3=KEYWORD)
                    ENDIF
                    IIS= NVSOLID1 + NVSOLID2 + NVSOLID3 + NUSOLID+4 + NVSOLID4 +
     .                   NVSOLID5
!
                    SIZE = ISOLNOD
                    CALL HM_GET_FLOAT_ARRAY('Xref' ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('Yref' ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('Zref' ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    DO K=1,ISOLNOD
                      S(1) =TMPVAL1(K)
                      S(2) =TMPVAL2(K)
                      S(3) =TMPVAL3(K)
                      IF(SUB_ID /= 0 .AND.(ISMSTR==1.OR.ISMSTR==11)) 
     .                  CALL SUBROTVECT (S(1),S(2),S(3),RTRANS,SUB_ID,LSUBMODEL)
                      SIGSP(IIS+(K-1)*3+1,I) =S(1)
                      SIGSP(IIS+(K-1)*3+2,I) =S(2)
                      SIGSP(IIS+(K-1)*3+3,I) =S(3)
                    ENDDO ! DO J=1,ISOLNOD
                    SIZE = NSROT
                    CALL HM_GET_FLOAT_ARRAY('Rx' ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('Ry' ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('Rz' ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    IIS = IIS + 3*ISOLNOD
                    DO K=1,NSROT
                      S(1) =TMPVAL1(K)
                      S(2) =TMPVAL2(K)
                      S(3) =TMPVAL3(K)
                      IF(SUB_ID /= 0 .AND.(ISMSTR==1.OR.ISMSTR==11)) 
     .                  CALL SUBROTVECT (S(1),S(2),S(3),RTRANS,SUB_ID,LSUBMODEL)
                      SIGSP(IIS+(K-1)*3+1,I) =S(1)
                      SIGSP(IIS+(K-1)*3+2,I) =S(2)
                      SIGSP(IIS+(K-1)*3+3,I) =S(3)
                    ENDDO 
                    STRAGLOB(IE)=10
!
                  ENDIF ! IF (IE == 0)
                ENDDO ! DO J=1,NB_ELEMENTS

C---------
            CASE DEFAULT

          END SELECT ! SELECT CASE(KEY)
!---
        ENDDO ! DO INI=1,NB_INIBRI
      ENDIF ! IF ( NB_INIBRI > 0 )
!
      NIBRICK = I
!-----------------------------------------
!            ---  /INISHE  ---
!-----------------------------------------
      NISHELL = 0
      I = 0
!
      CALL HM_OPTION_COUNT('/INISHE', NB_INISHE)
!
      IF ( NB_INISHE > 0 ) THEN
!
        ! Start reading /INISHE card 
        CALL HM_OPTION_START('/INISHE')
!---
! to be replaced by ---  MAP_TABLES%ISH4NM ---
           IF (KTRIELC == 0) THEN
C             tri des elts du D00 par ID croissant (on ne trie qu'une fois)
              DO IE = 1, NUMELC
                ITRI(IE) = IXC(NIXC,IE)
              END DO
              CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELC,1)
              DO J = 1, NUMELC
                IE=INDEX(J)
                KSYSUSR(J)         =IXC(NIXC,IE)
                KSYSUSR(NUMELC+J)=IE
              END DO
              KTRIELC=1
           ENDIF
!---
        DO INI=1,NB_INISHE         
!
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            UNIT_ID  = UID,
     .                            SUBMODEL_INDEX = SUB_INDEX,
     .                            SUBMODEL_ID = SUB_ID,
     .                            KEYWORD2 = KEY,
     .                            KEYWORD3 = KEY2)
!
          IF (KEY2 /= ' ') GLOB = .TRUE.
!
          IFLAGUNIT = 0
          DO IUNIT=1,UNITAB%NUNITS
            IF (UNITAB%UNIT_ID(IUNIT) == UID) THEN    
              IFLAGUNIT = 1
              EXIT
            ENDIF
          ENDDO
!
          IF (UID /= 0.AND.IFLAGUNIT == 0) THEN
            CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I2=UID, I1=SUB_ID, C1='INISHE',
     .                  C2='INISHE',
     .                  C3=' ')
          ENDIF
c---------------------------------------
          SELECT CASE (KEY(1:LEN_TRIM(KEY)))
C---------
            CASE ( 'EPSP_F' )
C---------
              ISIGSH =1
!
              CALL HM_GET_INTV('inishe_epsp_f_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                ! Reading  ---  ID_ELEM, NIP, NPG, THK  ---
                CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_FLOAT_ARRAY_INDEX('Thick',THK,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!                ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH4NM,NUMELC)
!                IE = MAP_TABLES%ISH4NM(ELT,2)
!
                IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
!
                IF (IE == 0) THEN        
                  ! Shell was not found. Issue a Warning & Skip.
                  NONEXIST = NONEXIST+1
                ELSE
!
                ! check is SHELL is QEPH
                  IG   = IXC(6,IE)
                  IHBE = IGEO(10,IG)
                  IF (IHBE==12 .OR. IHBE==24) THEN
                    NPGTMP = 4       
                  ELSE               
                    NPGTMP = 1       
                  ENDIF
                  IF (NPGTMP /= NPG) THEN
                    CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,IG),LTITR)
                    CALL ANCMSG(MSGID=26,
     .                          ANMODE=ANINFO,
     .                          MSGTYPE=MSGERROR,
     .                          I1=IGEO(1,IG),
     .                          C1=TITR,
     .                          I2=ID_ELEM)
                  ENDIF
!
                  I = PTSHEL(IE)
                  ID_SIGSH(I) = ID_ELEM
                  SIGSH(1,I) = ID_ELEM
                  SIGSH(2,I) = NIP
                  SIGSH(3,I) = THK
                  SIGSH(NVSHELL - 1,I) = ONE
!
                  IF (NPG <= 1) THEN
!---
                    SIZE = NIP*MAX(NPG,1)
                    CALL HM_GET_FLOAT_ARRAY('Ep',TMPVAL,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    PT=22
                    JJ=NIP*MAX(NPG,1)
                    K0 = 0
                    DO WHILE(JJ > 0)
                      L=MIN(JJ,5)
                      DO K=1,L
                        SIGSH(PT+(K-1)*6+5,I) = TMPVAL(K+K0)
                      ENDDO
                      K0=K0+5
                      PT=PT+30
                      JJ=JJ-5
                    ENDDO ! DO WHILE(JJ > 0)
!--------------------
                  ELSEIF (NPG > 1) THEN
                    SIGSH(NVSHELL,I) = NPG
!
                    IF (NIP == 0) THEN
!---
                      SIZE = NPG
                      CALL HM_GET_FLOAT_ARRAY('Ep',TMPVAL,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                      PT=22
                      DO K=1,NPG
                        SIGSH(PT+(K-1)*9+5,I) = TMPVAL(K)
                      ENDDO
                    ELSE
!---
                      SIZE = NIP*NPG
                      CALL HM_GET_FLOAT_ARRAY('Ep',TMPVAL,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                      PT=22
                      JJ=NIP*NPG
                      K0 = 0
                      DO WHILE(JJ > 0)
                        L=MIN(JJ,5)
                        DO K=1,L
                          SIGSH(PT+(K-1)*6+5,I) = TMPVAL(K+K0)
                        ENDDO
!
                        K0=K0+5
                        PT=PT+30
                        JJ=JJ-5
                      END DO ! DO WHILE(JJ > 0)
!---------------------
                    END IF ! IF (NIP == 0)
                  END IF !(NPG<=1)
                ENDIF ! IF (IE /= 0)
              ENDDO ! DO I=1,NB_ELEMENTS
C---------
            CASE ( 'STRS_F' )
C---------
              ISIGSH =1
C------------------------------------
!            --- 'STRS_F/GLOB' ---
C------------------------------------
              IF ( GLOB ) THEN
                CALL HM_GET_INTV('inishe_strs_f_glob_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                DO J=1,NB_ELEMENTS
                  ! Reading  ---  ID_ELEM, NIP, NPG, THK  ---
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_FLOAT_ARRAY_INDEX('Thick',THK,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!                  ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH4NM,NUMELC)
!                  IE = MAP_TABLES%ISH4NM(ELT,2)
!
                  IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
!
                  IF (IE == 0) THEN        
                    ! Shell was not found. Issue a Warning & Skip.
                    NONEXIST = NONEXIST+1
                  ELSE
!
                    ! check is SHELL is QEPH
                    IG   = IXC(6,IE)
                    IHBE = IGEO(10,IG)
                    IF (IHBE==12 .OR. IHBE==24) THEN
                      NPGTMP = 4       
                    ELSE               
                      NPGTMP = 1       
                    ENDIF
                    IF (NPGTMP /= NPG) THEN
                      CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,IG),LTITR)
                      CALL ANCMSG(MSGID=26,
     .                            ANMODE=ANINFO,
     .                            MSGTYPE=MSGERROR,
     .                            I1=IGEO(1,IG),
     .                            C1=TITR,
     .                            I2=ID_ELEM)
                    ENDIF
!
                    ! Reading CARD_1 ---  EM,EB,H1,H2,H3  ---
                    CALL HM_GET_FLOAT_ARRAY_INDEX('Em',EM,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('Eb',EB,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('H1',H1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('H2',H2,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('H3',H3,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    I = PTSHEL(IE)
                    SIGSH(1,I) = ID_ELEM
                    ID_SIGSH(I) = ID_ELEM
                    SIGSH(2,I) = NIP
                    SIGSH(3,I) = THK
                    SIGSH(4,I) = EM
                    SIGSH(5,I) = EB
                    SIGSH(17,I) = ONE
                    SIGSH(NVSHELL - 1 , I) = ONE
!----
                    IF (NPG == 0 .OR. NPG == 1) THEN
!----
                      SIGSH(14,I) = H1
                      SIGSH(15,I) = H2
                      SIGSH(16,I) = H3
!
                      IF (NIP == 0) THEN
                        ! Reading CARD_2 ---  sigma_X, sigma_Y, sigma_Z  ---
                        CALL HM_GET_FLOAT_ARRAY('sigma_X',SIGSH(22,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_Y',SIGSH(23,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_Z',SIGSH(18,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        ! Reading CARD_3 ---  sigma_XY, sigma_YZ, sigma_ZX  ---
                        CALL HM_GET_FLOAT_ARRAY('sigma_XY',SIGSH(24,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_YZ',SIGSH(25,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_ZX',SIGSH(26,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                        ! Reading CARD_4 ---  sigma_bX, sigma_bY, sigma_bZ  ---
                        CALL HM_GET_FLOAT_ARRAY('sigma_bX',SIGSH(28,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_bY',SIGSH(29,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_bZ',SIGSH(19,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        ! Reading CARD_5 ---  sigma_bXY, sigma_bYZ, sigma_bZX, eps_p  ---
                        CALL HM_GET_FLOAT_ARRAY('sigma_bXY',SIGSH(30,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_bYZ',SIGSH(20,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_bZX',SIGSH(21,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('eps_p'    ,SIGSH(27,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                      ELSEIF (NIP /= 0) THEN
!
                        SIZE = NIP
                        CALL HM_GET_FLOAT_ARRAY('sigma_X' ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_Y' ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_Z' ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_XY',TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_YZ',TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_ZX',TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('eps_p'   ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('pos_nip' ,TMPVAL8,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
C------------potision Ti  [-1,1]  'pos_nip'       --->      undocumented FIELD
!
                        INISHVAR = 22 + NIP*6
                        DO N=1,NIP
                          PT = 22 + (N-1)*6
                          ! Reading CARD_2 ---  sigma_X, sigma_Y, sigma_Z  ---
                          SIGSH(PT,I)                  = TMPVAL1(N)
                          SIGSH(PT + 1,I)              = TMPVAL2(N)
                          SIGSH(INISHVAR + N,I)        = TMPVAL3(N)
                          SIGSH(PT + 2,I)              = TMPVAL4(N)
                          SIGSH(PT + 3,I)              = TMPVAL5(N)
                          SIGSH(PT + 4,I)              = TMPVAL6(N)
                          SIGSH(PT + 5,I)              = TMPVAL7(N)
                          SIGSH(INISHVAR +  NIP + N,I) = TMPVAL8(N)
                        ENDDO ! DO K=1,NIP
                      ENDIF ! IF (NIP = 0) THEN
!----
                    ELSEIF (NPG > 1) THEN
!----
                      SIGSH(NVSHELL,I) = NPG
!
                      IF (NIP == 0) THEN
!
                        SIZE = NPG
                        CALL HM_GET_FLOAT_ARRAY('sigma_X'  ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_Y'  ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_Z'  ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_XY' ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_YZ' ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_ZX' ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_bX' ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_bY' ,TMPVAL8,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_bZ' ,TMPVAL9,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_bXY',TMPVAL10,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_bYZ',TMPVAL11,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_bZX',TMPVAL12,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('eps_p'    ,TMPVAL13,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                        DO K=1,NPG
                          PT= 22 + (K-1)*13
!
                          SIGSH(PT  ,I)  = TMPVAL1(K)
                          SIGSH(PT+1,I)  = TMPVAL2(K)
                          SIGSH(PT+2,I)  = TMPVAL3(K)
                          SIGSH(PT+3,I)  = TMPVAL4(K)
                          SIGSH(PT+4,I)  = TMPVAL5(K)
                          SIGSH(PT+5,I)  = TMPVAL6(K)
                          SIGSH(PT+6,I)  = TMPVAL7(K)
                          SIGSH(PT+7,I)  = TMPVAL8(K)
                          SIGSH(PT+8,I)  = TMPVAL9(K)
                          SIGSH(PT+9,I)  = TMPVAL10(K)
                          SIGSH(PT+10,I) = TMPVAL11(K)
                          SIGSH(PT+11,I) = TMPVAL12(K)
                          SIGSH(PT+12,I) = TMPVAL13(K)
                        ENDDO ! DO K=1,NPG
!
                      ELSE  ! NIP > 0
!
                        SIZE = NIP*NPG
                        CALL HM_GET_FLOAT_ARRAY('sigma_X' ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_Y' ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_Z' ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_XY',TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_YZ',TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_ZX',TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('eps_p'   ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('pos_nip' ,TMPVAL8,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
C------------potision Ti  [-1,1]   'pos_nip'      --->      undocumented FIELD
!                   
                        PT = 22
                        DO N=1,NIP
                          DO K=1,NPG
                            L = (N-1)*NPG+K
                             ! Reading CARD_2 ---  sigma_X, sigma_Y, sigma_Z  ---
                            SIGSH(PT  ,I) = TMPVAL1(L)
                            SIGSH(PT+1,I) = TMPVAL2(L)
                            SIGSH(PT+2,I) = TMPVAL3(L)
                            SIGSH(PT+3,I) = TMPVAL4(L)
                            SIGSH(PT+4,I) = TMPVAL5(L)
                            SIGSH(PT+5,I) = TMPVAL6(L)
                            SIGSH(PT+6,I) = TMPVAL7(L)
                            SIGSH(PT+7,I) = TMPVAL8(L)
                            PT = PT + 8
                          ENDDO ! DO N=1,NPG
                        ENDDO ! DO N=1,NIP
                      ENDIF ! IF (NIP == 0) THEN
!----
                    ENDIF ! IF (NPG == 0 .OR. NPG == 1)
!----
                  ENDIF ! IF (IE == 0)
                ENDDO ! DO I=1,NB_ELEMENTS
C------------------------------------
!            --- 'STRS_F' ---
C------------------------------------
              ELSEIF ( .NOT. GLOB ) THEN
!
                CALL HM_GET_INTV('inishe_strs_f_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                DO J=1,NB_ELEMENTS
                  ! Reading  CARD_1 ---  ID_ELEM, NIP, NPG, THK  ---
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_FLOAT_ARRAY_INDEX('Thick',THK,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!                  ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH4NM,NUMELC)
!                  IE = MAP_TABLES%ISH4NM(ELT,2)
!
                  IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
!
                  IF (IE == 0) THEN        
                    ! Shell was not found. Issue a Warning & Skip.
                    NONEXIST = NONEXIST+1
                  ELSE
!
                    ! check is SHELL is QEPH
                    IG   = IXC(6,IE)
                    IHBE = IGEO(10,IG)
                    IF (IHBE==12 .OR. IHBE==24) THEN
                      NPGTMP = 4       
                    ELSE               
                      NPGTMP = 1       
                    ENDIF
                    IF (NPGTMP /= NPG) THEN
                      CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,IG),LTITR)
                      CALL ANCMSG(MSGID=26,
     .                            ANMODE=ANINFO,
     .                            MSGTYPE=MSGERROR,
     .                            I1=IGEO(1,IG),
     .                            C1=TITR,
     .                            I2=ID_ELEM)
                    ENDIF
!
                    ! Reading CARD_2 ---  EM,EB,H1,H2,H3  ---
                    CALL HM_GET_FLOAT_ARRAY_INDEX('Em',EM,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('Eb',EB,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('H1',H1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('H2',H2,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('H3',H3,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                     I = PTSHEL(IE)
                     
                    SIGSH(1,I) = ID_ELEM
                    ID_SIGSH(I) = ID_ELEM
                    SIGSH(2,I) = NIP
                    SIGSH(3,I) = THK
                    SIGSH(4,I) = EM
                    SIGSH(5,I) = EB
                    SIGSH(17,I) = ZERO
                    SIGSH(NVSHELL - 1 , I) = ONE
!----
                    IF (NPG == 0 .OR. NPG == 1) THEN
!----
                      SIGSH(14,I) = H1
                      SIGSH(15,I) = H2
                      SIGSH(16,I) = H3
!
                      IF (NIP == 0) THEN
                        ! Reading CARD_3 ---  sigma_1, sigma_2, sigma_12, sigma_23, sigma_31  ---
                        CALL HM_GET_FLOAT_ARRAY('sigma_1' ,SIGSH(22,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_2' ,SIGSH(23,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_12',SIGSH(24,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_23',SIGSH(25,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_31',SIGSH(26,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                        ! Reading CARD_4 ---  eps_p, sigma_b1, sigma_b2, sigma_b12  ---
                        CALL HM_GET_FLOAT_ARRAY('eps_p'    ,SIGSH(27,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_b1' ,SIGSH(28,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_b2' ,SIGSH(29,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_b12',SIGSH(30,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                      ELSEIF (NIP /= 0) THEN
!
                        SIZE = NIP
                        CALL HM_GET_FLOAT_ARRAY('sigma_1' ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_2' ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_12',TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_23',TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_31',TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('eps_p'   ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
! 
!
                        INISHVAR = 22 + NIP*6
                        DO N=1,NIP
                          PT = 22 + (N-1)*6
                          ! Reading CARD_3 ---  sigma_1, sigma_2, sigma_12  ---
                          SIGSH(PT  ,I) = TMPVAL1(N)
                          SIGSH(PT+1,I) = TMPVAL2(N)
                          SIGSH(PT+2,I) = TMPVAL3(N)
                          SIGSH(PT+3,I) = TMPVAL4(N)
                          SIGSH(PT+4,I) = TMPVAL5(N)
                          SIGSH(PT+6,I) = TMPVAL6(N)
                        ENDDO ! DO K=1,NIP
                      ENDIF ! IF (NIP = 0) THEN
!----
                    ELSEIF (NPG > 1) THEN
!----
                      SIGSH(NVSHELL,I) = NPG
!
                      IF (NIP == 0) THEN
!
                        SIZE = NPG
                        CALL HM_GET_FLOAT_ARRAY('sigma_1'  ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_2'  ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_12' ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_23' ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_31' ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('eps_p'    ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_b1' ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_b2' ,TMPVAL8,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_b12',TMPVAL9,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                        DO K=1,NPG
                          PT= 22 + (K-1)*9
                          ! Reading CARD_3 ---  sigma_1, sigma_2, sigma_12, sigma_23, sigma_31  ---
                          SIGSH(PT  ,I) = TMPVAL1(K)
                          SIGSH(PT+1,I) = TMPVAL2(K)
                          SIGSH(PT+2,I) = TMPVAL3(K)
                          SIGSH(PT+3,I) = TMPVAL4(K)
                          SIGSH(PT+4,I) = TMPVAL5(K)
                          SIGSH(PT+5,I) = TMPVAL6(K)
                          SIGSH(PT+6,I) = TMPVAL7(K)
                          SIGSH(PT+7,I) = TMPVAL8(K)
                          SIGSH(PT+8,I) = TMPVAL9(K)
                        ENDDO ! DO K=1,NPG
!
                      ELSE  ! NIP > 0
!
                        SIZE = NIP*NPG
                        CALL HM_GET_FLOAT_ARRAY('sigma_1' ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_2' ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_12',TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_23',TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_31',TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('eps_p'   ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!                   
                        PT = 22
                        DO N=1,NIP
                          DO K=1,NPG
                            L = (N-1)*NPG+K
                            ! Reading CARD_3 ---  sigma_1, sigma_2, sigma_12  ---
                            SIGSH(PT  ,I) = TMPVAL1(L)
                            SIGSH(PT+1,I) = TMPVAL2(L)
                            SIGSH(PT+2,I) = TMPVAL3(L)
                            SIGSH(PT+3,I) = TMPVAL4(L)
                            SIGSH(PT+4,I) = TMPVAL5(L)
                            SIGSH(PT+5,I) = TMPVAL6(L)

                            PT = PT + 6
                          ENDDO ! DO K=1,NPG
                        ENDDO ! DO N=1,NIP
                      ENDIF ! IF (NIP == 0) THEN
!----
                    ENDIF ! IF (NPG == 0 .OR. NPG == 1)
!----
                  ENDIF ! IF (IE == 0)
                ENDDO ! DO I=1,NB_ELEMENTS
              ENDIF ! IF ( GLOB )
C---------
            CASE ( 'STRA_F' )
C---------
C-------- use ITHKSHEL instead of ISIGSH to avoid memory issue in case of STRA_F w/o STRS_F
              ITHKSHEL =2
C-------global sys with diff format
              IF ( GLOB ) THEN
                CALL HM_GET_INTV('inishe_stra_f_glob_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                DO J=1,NB_ELEMENTS
                  ! Reading  CARD_1 ---  ID_ELEM, NIP, NPG, THK  ---
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_FLOAT_ARRAY_INDEX('Thick',THK,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!                  ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH4NM,NUMELC)
!                  IE = MAP_TABLES%ISH4NM(ELT,2)
!
                  IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
!
                  IF (IE == 0) THEN        
                    ! Shell was not found. Issue a Warning & Skip.
                    NONEXIST = NONEXIST+1
                  ELSE
                    I = PTSHEL(IE)
                    SIGSH(1,I)  = ID_ELEM
                    ID_SIGSH(I) = ID_ELEM
                    SIGSH(2,I)  = NIP
                    SIGSH(3,I)  = THK
                    SIGSH(17,I) = ONE
                    SIGSH(NVSHELL - 1 , I) = ONE
C----
                    IF (NPG == 0 .OR. NPG == 1) THEN
                      IG   = IXC(6,IE)
                      IHBE = IGEO(10,IG)
                      IF (IHBE==24) SIGSH(NVSHELL,I) = 4
                    ELSEIF (NPG>1 ) THEN
C----look at how to orginase SIGSH(,I)
                      SIGSH(NVSHELL,I) = NPG
                    ELSE
C                     CALL ANCERR(58,ANINFO_BLIND_2)
                    ENDIF ! IF (NPG == 0 .OR. NPG == 1)
                    PT = INISHVAR1
                    NPP = NIP
                    IF (NPP==0) NPP=2
!===============================================
                    SIZE = NPP*NPG
                    CALL HM_GET_FLOAT_ARRAY('eps_XX'  ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('eps_YY'  ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('eps_ZZ'  ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('eps_XY'  ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('eps_YZ'  ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('eps_ZX'  ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('T'       ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    DO N=1,MIN(2,NPP)
                      DO IPG=1,MAX(1,NPG)
                        L = (N-1)*MAX(1,NPG)+IPG
                        SIGSH(PT  ,I) = TMPVAL1(L)
                        SIGSH(PT+1,I) = TMPVAL2(L)
                        SIGSH(PT+2,I) = TMPVAL3(L)
                        SIGSH(PT+3,I) = TMPVAL4(L)
                        SIGSH(PT+4,I) = TMPVAL5(L)
                        SIGSH(PT+5,I) = TMPVAL6(L)
                        SIGSH(PT+6,I) = TMPVAL7(L)
                        PT=PT+7
                      ENDDO
                    ENDDO
!===============================================
                  ENDIF ! IF (IE == 0) THEN
                ENDDO ! DO J=1,NB_ELEMENTS
!
              ELSEIF (.NOT. GLOB ) THEN
!
                CALL HM_GET_INTV('inishe_stra_f_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                DO J=1,NB_ELEMENTS
                  ! Reading  CARD_1 ---  ID_ELEM, NIP, NPG, THK  ---
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_FLOAT_ARRAY_INDEX('Thick',THK,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!                  ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH4NM,NUMELC)
!                  IE = MAP_TABLES%ISH4NM(ELT,2)
!
                  IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
!
                  IF (IE == 0) THEN        
                    ! Shell was not found. Issue a Warning & Skip.
                    NONEXIST = NONEXIST+1
                  ELSE
                    I = PTSHEL(IE)
                    SIGSH(1,I) = ID_ELEM
                    ID_SIGSH(I) = ID_ELEM
                    SIGSH(3,I) = THK
                    SIGSH(NVSHELL - 1 , I) = ONE
!
                    IF (NPG == 0 .OR. NPG == 1) THEN
!
                      IG   = IXC(6,IE)
                      IHBE = IGEO(10,IG)
                      IF (IHBE==24) SIGSH(NVSHELL,I) = 4
!
                      CALL HM_GET_FLOAT_ARRAY('eps_1'  ,SIGSH(6,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('eps_2'  ,SIGSH(7,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('eps_12' ,SIGSH(8,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('eps_23' ,SIGSH(9,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('eps_31' ,SIGSH(10,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('k1'     ,SIGSH(11,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('k2'     ,SIGSH(12,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('k12'    ,SIGSH(13,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    ELSEIF (NPG>1 ) THEN
!
                      SIGSH(NVSHELL,I) = NPG

                      SIGSH(6,I) =ZERO
                      SIGSH(7,I) =ZERO
                      SIGSH(8,I) =ZERO
                      SIGSH(9,I) =ZERO
                      SIGSH(10,I)=ZERO
                      SIGSH(11,I)=ZERO
                      SIGSH(12,I)=ZERO
                      SIGSH(13,I)=ZERO
!
                      SIZE = NPG
                      CALL HM_GET_FLOAT_ARRAY('eps_1'   ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('eps_2'   ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('eps_12'  ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('eps_23'  ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('eps_31'  ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('k1'      ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('k2'      ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('k12'     ,TMPVAL8,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                      DO IPG=1,NPG
!                       average only :
                        SIGSH(6,I) =SIGSH(6,I) +TMPVAL1(IPG)/NPG
                        SIGSH(7,I) =SIGSH(7,I) +TMPVAL2(IPG)/NPG
                        SIGSH(8,I) =SIGSH(8,I) +TMPVAL3(IPG)/NPG
                        SIGSH(9,I) =SIGSH(9,I) +TMPVAL4(IPG)/NPG
                        SIGSH(10,I)=SIGSH(10,I)+TMPVAL5(IPG)/NPG
                        SIGSH(11,I)=SIGSH(11,I)+TMPVAL6(IPG)/NPG
                        SIGSH(12,I)=SIGSH(12,I)+TMPVAL7(IPG)/NPG
                        SIGSH(13,I)=SIGSH(13,I)+TMPVAL8(IPG)/NPG
                      END DO
                    ELSE
C                    CALL ANCERR(58,ANINFO_BLIND_2)
                    ENDIF ! IF (NPG == 0 .OR. NPG == 1)
                  ENDIF ! IF (IE == 0) THEN
                ENDDO ! DO J=1,NB_ELEMENTS
              ENDIF ! IF ( GLOB ) THEN

C---------
            CASE ( 'THICK' )
C---------
              ITHKSHEL = 1
!
              CALL HM_GET_INTV('no_elems',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_FLOAT_ARRAY_INDEX('Thick'  ,THK,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!
!                  ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH4NM,NUMELC)
!                  IE = MAP_TABLES%ISH4NM(ELT,2)
!
                IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
!
                IF (IE == 0) THEN        
                  ! Shell was not found. Issue a Warning & Skip.
                  NONEXIST = NONEXIST+1
                ELSE
                  I = PTSHEL(IE)
                  SIGSH(1,I) = ID_ELEM
                  ID_SIGSH(I) = ID_ELEM
                  SIGSH(2,I) = 0
                  SIGSH(3,I) = THK
                ENDIF ! IF (IE == 0)
              ENDDO ! DO J=1,NB_ELEMENTS
C---------
            CASE ( 'EPSP' )
C---------
!
              CALL HM_GET_INTV('no_blocks',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_FLOAT_ARRAY_INDEX('Ep'  ,EPSP,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!
!                  ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH4NM,NUMELC)
!                  IE = MAP_TABLES%ISH4NM(ELT,2)
!
                IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
!
                IF (IE == 0) THEN
                  ! Shell was not found. Issue a Warning & Skip.
                  NONEXIST = NONEXIST+1
                ELSE
                  I = PTSHEL(IE)
                  SIGSH(1,I) = ID_ELEM
                  ID_SIGSH(I) = ID_ELEM
                  SIGSH(2,I) = 0
                  SIGSH(27,I)= EPSP
                ENDIF ! IF (IE == 0) THEN
              ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
              CASE ( 'ORTHO' )
!-------------------
                CALL HM_GET_INTV('inishe_ortho_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
!!                  CALL HM_GET_INT_ARRAY_INDEX('prop_type',IGTYP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_FLOAT_ARRAY_INDEX('Vx',VX,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                  CALL HM_GET_FLOAT_ARRAY_INDEX('Vy',VY,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                  CALL HM_GET_FLOAT_ARRAY_INDEX('Vz',VZ,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!                  ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH4NM,NUMELC)
!                  IE = MAP_TABLES%ISH4NM(ELT,2)
!
                  IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
!
                  IF (IE == 0) THEN
                    ! Shell was not found. Issue a Warning & Skip.
                    NONEXIST = NONEXIST+1
                  ELSE
!
                    IG   = IXC(6,IE)
                    IHBE = IGEO(10,IG)
                    IGTYP=IGEO(11,IG)
                    IORTSHEL = 1
                    I = PTSHEL(IE)
                    PT = NVSHELL + NUSHELL
                    SIGSH(1,I) = ID_ELEM
                    ID_SIGSH(I) = ID_ELEM
                    IF ( IGTYP == 9) NIP = NINT(GEO(NPROPG*(IG-1)+6))
                    SIGSH(PT + 4     ,I) = NIP
                    SIGSH(PT + 5     ,I) = ONE
                    IF( IHBE==12 .OR. IHBE==24) THEN
                      SIGSH(NVSHELL,I) = 4
                    ELSE
                      SIGSH(NVSHELL,I) = 1
                    ENDIF
                    SIGSH(PT+1,I) = VX
                    SIGSH(PT+2,I) = VY
                    SIGSH(PT+3,I) = VZ
                    PT = PT + 5
                    IF ( IGTYP == 9 ) THEN
                      CALL HM_GET_FLOAT_ARRAY_INDEX('phi_1',PHI1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY_INDEX('phi_2',PHI2,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      SIGSH(PT+1,I) = PHI1*PI/HUNDRED80
                      SIGSH(PT+2,I) = PHI2*PI/HUNDRED80
                      PT = PT + 2
                    ELSEIF (IGTYP == 1 ) THEN
                      CALL ANCMSG(MSGID=761,
     .                            MSGTYPE=MSGERROR,
     .                            ANMODE=ANINFO,
     .                            C1='/INISHE/ORTHO',
     .                            C2='SHELL',
     .                            I2=ID_ELEM,I1=IGEO(1,IG))
                    ELSE
                      SIZE = NIP
                      CALL HM_GET_FLOAT_ARRAY('phi_1_array',TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('phi_2_array',TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      DO JJ = 1,NIP
                        SIGSH(PT+1,I) = TMPVAL1(JJ)*PI/HUNDRED80
                        SIGSH(PT+2,I) = TMPVAL2(JJ)*PI/HUNDRED80
                        PT = PT + 2
                      ENDDO ! DO JJ = 1,NIP
                    ENDIF ! IF ( IGTYP == 9)
                  ENDIF ! IF (IE == 0) THEN
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
              CASE ( 'ORTH_LOC' )
!-------------------
                CALL HM_GET_INTV('inishe_orth_loc_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nb_lay',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('ndir',NDIR,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Iunit',FLAGDEG,J,IS_AVAILABLE,LSUBMODEL)
!
!
!                  ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH4NM,NUMELC)
!                  IE = MAP_TABLES%ISH4NM(ELT,2)
!
                  IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
!
                  IF (IE == 0) THEN
                    ! Shell was not found. Issue a Warning & Skip.
                    NONEXIST = NONEXIST+1
                  ELSE
!
                    IG    = IXC(6,IE)
                    IHBE  = IGEO(10,IG)
                    IGTYP = IGEO(11,IG)
                    IORTSHEL = 2
                    I = PTSHEL(IE)
                    PT = NVSHELL + NUSHELL
                    ID_SIGSH(I) = ID_ELEM
                    SIGSH(1,I)  = ID_ELEM
                    IF (IGTYP == 9) NIP = NINT(GEO(NPROPG*(IG-1)+6))
                    SIGSH(PT + 4,I) = NIP
                    SIGSH(PT + 5,I) = ONE
                    IF( IHBE==12 .OR. IHBE==24) THEN
                      SIGSH(NVSHELL,I) = 4
                    ELSE
                      SIGSH(NVSHELL,I) = 1
                    ENDIF
!
                    PT = PT + 5 
                    IF (IGTYP == 51 .OR. IGTYP == 52) THEN
                      ISUBSTACK = IWORKSH(3, IE)
                      NLAY = STACK%IGEO(1,ISUBSTACK)
                      IPMAT  =  2 + NLAY 
                      IF (NDIR /= 2) THEN
                        DO JJ = 1,NLAY ! 
                          MLAWLY= STACK%IGEO(IPMAT + JJ,ISUBSTACK)! layer material
                          IF (MLAWLY == 58) THEN
                            CALL ANCMSG(MSGID=1126,
     .                                  MSGTYPE=MSGERROR,
     .                                  ANMODE=ANINFO,
     .                                  C1='SHELL',
     .                                  I1=ID_ELEM)
                          ENDIF
                        ENDDO
                      ENDIF
                    ENDIF ! IF (IGTYP == 51 .OR. IGTYP == 52)
!
                    SIZE = NIP
                    CALL HM_GET_FLOAT_ARRAY('phi_i'  ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('alpha_i',TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    ALLOCATE(MLAW_LY(NIP))
                    MLAW_LY = 0
                    IF (IGTYP == 9) THEN
                      ANGLE1 = TMPVAL1(1) ! one integration point
                      IF(FLAGDEG == 1) ANGLE1 = ANGLE1*PI/HUNDRED80
                      SIGSH(PT+1,I) = COS(ANGLE1)
                      SIGSH(PT+2,I) = SIN(ANGLE1)
                      PT = PT + 2
                    ELSEIF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 16 .OR.
     .                      IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
                     IF (IGTYP == 51 .OR. IGTYP == 52)THEN
                        ISUBSTACK = IWORKSH(3, IE) 
                        NLAY =  STACK%IGEO(1,ISUBSTACK) !                                     
                        IPMAT  =  2 + NLAY
                        IPNPT_LAY = IPMAT + 2*NLAY                                    
                        IF(NLAY /= NIP) THEN
                          IF(NDRAPE > 0) THEN 
                           IPT = 0                               
                           DO JJ =1,NLAY
                              NSLICE = STACK%IGEO(IPNPT_LAY  + JJ,ISUBSTACK)
                              DO N = 1, NSLICE
                                 IPT = IPT + 1
                                  MLAW_LY(IPT)= STACK%IGEO(IPMAT + JJ,ISUBSTACK)
                              ENDDO
                           ENDDO                                                             
                          ELSE
                           ! error message
                          ENDIF 
                        ELSE
                          DO JJ =1,NLAY                                                    
                              MLAW_LY(JJ)= STACK%IGEO(IPMAT + JJ,ISUBSTACK)! layer material 
                           ENDDO  
                        ENDIF                                                                
                      ENDIF 
                      DO JJ = 1,NIP
                        ANGLE1 = TMPVAL1(JJ)
                        ANGLE2 = TMPVAL2(JJ)
                        IF(FLAGDEG == 1) ANGLE1 = ANGLE1*PI/HUNDRED80
                        IF(FLAGDEG == 1) ANGLE2 = ANGLE2*PI/HUNDRED80
!                      
                        IF (IGTYP == 16 .OR.
     .                     (IGTYP == 51  .AND. MLAW_LY(JJ) == 58) .OR. 
     .                     (IGTYP == 52  .AND. MLAW_LY(JJ) == 58) ) THEN
!
                          ANGLE2 = ANGLE2 + ANGLE1
                          SIGSH(PT+1,I) = COS(ANGLE1)
                          SIGSH(PT+2,I) = SIN(ANGLE1)
                          SIGSH(PT+3,I) = COS(ANGLE2)
                          SIGSH(PT+4,I) = SIN(ANGLE2)
                          PT = PT + 4
                        ELSE
                          SIGSH(PT+1,I) = COS(ANGLE1)
                          SIGSH(PT+2,I) = SIN(ANGLE1)
                          PT = PT + 2
                        ENDIF
                      ENDDO ! DO JJ = 1,NIP
                    ELSEIF (IGTYP == 1) THEN
                      CALL ANCMSG(MSGID=761,
     .                            MSGTYPE=MSGERROR,
     .                            ANMODE=ANINFO,
     .                            C1='/INISHE/ORTH_LOC',
     .                            C2='SHELL',
     .                            I2=ID_ELEM,I1=IGEO(1,IG))
                    ENDIF ! IF (IGTYP == 9)
                    IF(ALLOCATED(MLAW_LY))DEALLOCATE(MLAW_LY)
                  ENDIF ! IF (IE == 0) THEN
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
              CASE ( 'SCALE_YLD' )
!-------------------
                CALL HM_GET_INTV('inishe_scale_yld_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
                IYLDINI = 1
!
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
!
!                  ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH4NM,NUMELC)
!                  IE = MAP_TABLES%ISH4NM(ELT,2)
!
                  IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
!
                  IF (IE == 0) THEN
                    ! Shell was not found. Issue a Warning & Skip.
                    NONEXIST = NONEXIST+1
                  ELSE
                    I = PTSHEL(IE)
                    SIGSH(NVSHELL + 1,I) = ID_ELEM ! elt ID
                    ID_SIGSH(I) = ID_ELEM
                    SIGSH(NVSHELL + 2,I) = NIP ! integ point
                    SIGSH(NVSHELL + 3,I) = NPG
!
                    SIZE = NPG*NIP
                    PT = NVSHELL+NUSHELL+NORTSHEL+NVSHELL1+3
!
                    CALL HM_GET_FLOAT_ARRAY('Alpha_ij'  ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    DO N = 1,NPG
                      DO IP = 1,NIP
                        L = (N-1)*NIP+IP
!!                        PT=NVSHELL + 3  !22
                        SCALEYLD = TMPVAL1(L)
                        SIGSH(PT+L,I) = SCALEYLD
                      ENDDO !IP = 1,NIP
                    ENDDO !N = 1,NPG
                    PT = PT + NIP * NPG
!
                  ENDIF ! IF (IE == 0) THEN
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
              CASE ( 'AUX' )
!-------------------
                CALL HM_GET_INTV('inishe_aux_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nvars',NUVAR,J,IS_AVAILABLE,LSUBMODEL)
!
!
!                  ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH4NM,NUMELC)
!                  IE = MAP_TABLES%ISH4NM(ELT,2)
!
                  IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
!
                  IF (IE == 0) THEN
                    ! Shell was not found. Issue a Warning & Skip.
                    NONEXIST = NONEXIST+1
                  ELSE
C----------
                    IMAT = IXC(1,IE)
                    ILAW = IPM(2,IMAT)
                    NUVARD00 = IPM(8,IMAT)
                    IF (NUVARD00 > NUVAR) THEN
                      CALL ANCMSG(MSGID=1121,
     .                            MSGTYPE=MSGWARNING,
     .                            ANMODE=ANINFO,
     .                            I1=ITRI(IE),
     .                            C1='NUMBER OF USER VARIABLES',
     .                            C2='MATERIAL LAW ',
     .                            I2=IPM(1,IMAT),
     .                            C3='/INISHE/AUX')
                    ENDIF
                    IF ((ILAW == 36 .and. (NUVAR < 4 .or. NUVARD00 > 3) .and. 
     .                   NUVARD00 < NUVAR) .or.  
     .                  (ILAW /= 36 .and. ILAW /= 78 .and. ILAW /= 87 .and. ILAW /= 112 .and. NUVARD00 < NUVAR)) THEN
                      CALL ANCMSG(MSGID=695,
     .                            MSGTYPE=MSGERROR,
     .                            ANMODE=ANINFO,
     .                            I1=ITRI(IE),
     .                            C1='NUMBER OF USER VARIABLES',
     .                            C2='MATERIAL LAW ',
     .                            I2=IPM(1,IMAT),
     .                            C3='/INISHE/AUX')
                    ENDIF
C----------
                    I = PTSHEL(IE)
                    IUSER    = 1
                    NVARSH = NVSHELL + 4
                    IF (NIP == 0) NIP = 1
                    IF (NPG == 0) NPG = 1
                    SIGSH(1,I) = ID_ELEM
                    ID_SIGSH(I) = ID_ELEM
                    SIGSH(2,I) = NIP
                    SIGSH(NVSHELL,I) = NPG
!----
                    IG   = IXC(6,IE)
                    IHBE = IGEO(10,IG)
                    IF (IHBE==24) SIGSH(NVSHELL,I) = 4
!----
                    SIGSH(NVSHELL + 2 ,I) = NIP
                    SIGSH(NVSHELL + 3 ,I) = NPG
                    SIGSH(NVSHELL + 4 ,I) = NUVAR
                    PT = 0
!
                    CALL HM_GET_INT_ARRAY_INDEX('num_lines',NUM_LINES,J,IS_AVAILABLE,LSUBMODEL)
                    NMAX_AUX = NUM_LINES*NUVAR
                    CALL HM_GET_FLOAT_ARRAY('V'  ,TMPVAL,NMAX_AUX,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    DO JJ=1,NUM_LINES
                      DO K=1,NUVAR
                        L = NUVAR*(JJ-1) + K
                        SIGSH(NVARSH+PT+K,I) = TMPVAL(L)
                      ENDDO ! DO K=1,NUVAR
                      PT = PT + NUVAR
                    ENDDO ! DO JJ=1,NUM_LINES
!
                  ENDIF ! IF (IE == 0) THEN
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
              CASE ( 'FAIL' )
!-------------------
                CALL HM_GET_INTV('inishe_fail_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID' ,ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Nlay'     ,NLAY,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg'      ,NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nptt'     ,NPTT,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('lay_ID'   ,ILAY,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('fail_ID'  ,IFAIL,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Ifail_typ',IRUPT_TYP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Nvar'     ,NVAR_RUPT,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('mat_ID'   ,IMAT,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('num_lines',NUM_LINES,J,IS_AVAILABLE,LSUBMODEL)
!
!                  ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH4NM,NUMELC)
!                  IE = MAP_TABLES%ISH4NM(ELT,2)
!
                  IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
!
                  IF (IE == 0) THEN
!
                    ! Shell was not found. Issue a Warning & Skip.
                    NONEXIST = NONEXIST+1
                  ELSE
C----------
                    NPTT = MAX(1,NPTT)
                    NLAY = MAX(1,NLAY)
                    NPT_MAX = MAX(NPTT,NLAY)
                    NVMAX = NVSHELL1 /(MAX(1,NPG)*NPT_MAX*5)
                    IF (ID_ELEM /= NEM1)  I = PTSHEL(IE)
                    NEM1 = ID_ELEM
                    IOK = 0
!
                    DO K=1,NUMMAT
                      IF (IPM(1,K) == IMAT) THEN
                        IMAT = K
                        IOK = 1
                        EXIT
                      ENDIF
                    ENDDO
                    IF (IOK == 0) THEN
                      CALL ANCMSG(MSGID=1033,
     .                            MSGTYPE=MSGERROR,
     .                            ANMODE=ANINFO,
     .                            I1=ITRI(IE),
     .                            C1='MATERIAL LAW',
     .                            C2='/INISHE/FAIL')
                    ENDIF ! IF (IOK == 0)
!
                    IG   = IXC(6,IE)
                    IHBE = IGEO(10,IG)
                    IGTYP=IGEO(11,IG)
                    SIGSH(1,I)  = ID_ELEM
                    ID_SIGSH(I) = ID_ELEM
                    IF ( IGTYP == 9 ) NLAY = NINT(GEO(NPROPG*(IG-1)+6))
                    IF ( IGTYP == 10 .OR. IGTYP == 11) THEN
                      SIGSH(2,I) = NLAY
                    ELSE
                      SIGSH(2,I) = NPTT*NLAY
                    ENDIF
                    IF (IHBE==12 .OR. IHBE==24) THEN
                      SIGSH(NVSHELL,I) = 4
                    ELSE
                      SIGSH(NVSHELL,I) = 1
                    ENDIF
!
!         check for consistency ( D00 & INIBRI)
                    IOK = 0
                    DO K=1,5
                      NFAIL(K) = MAT_PARAM(IMAT)%FAIL(K)%FAIL_ID
                      IF (IFAIL == NFAIL(K) .AND. 
     .                    IRUPT_TYP == MAT_PARAM(IMAT)%FAIL(K)%IRUPT) THEN
                        IFAIL = K
                        FAIL_INI(IFAIL)=1
                        IOK = 1
                        EXIT
                      ENDIF
                    ENDDO
                    IF (IOK == 0) THEN
                      CALL ANCMSG(MSGID=1033,
     .                            MSGTYPE=MSGERROR,
     .                            ANMODE=ANINFO,
     .                            I1=ITRI(IE),
     .                            C1='FAILURE CRITERIA',
     .                            C2='/INISHE/FAIL')
                    ENDIF
!
                    PT = NVSHELL+NUSHELL+3+NORTSHEL
                    NPG = MAX(1,NPG)
                    NPTT = MAX(1,NPTT)
                    NLAY = MAX(1,NLAY)
!
                    NMAX_FAIL = NUM_LINES*NVAR_RUPT
                    CALL HM_GET_FLOAT_ARRAY('V'  ,TMPVAL,NMAX_FAIL,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    DO JJ=1,NUM_LINES
                      DO K=1,NVAR_RUPT
                        L = NVAR_RUPT*(JJ-1) + K
                        SIGSH(PT+L+(IFAIL-1)*NPT_MAX*NPG*NVMAX+
     .                              (ILAY-1)*NVMAX*NPG*NPTT,I) = TMPVAL(L)
                      ENDDO ! DO K=1,NVAR_RUPT
                    ENDDO ! DO JJ=1,NUM_LINES
!
                  ENDIF ! IF (IE == 0) THEN
                ENDDO ! DO J=1,NB_ELEMENTS
C---------
            CASE DEFAULT

          END SELECT ! SELECT CASE(KEY)

        ENDDO ! DO INI=1,NB_INISHE

      ENDIF ! IF ( NB_INISHE > 0 )
!
      NISHELL = I

!      NUMSHEL = NISHELL

!-----------------------------------------
!            ---  /INISH3  ---
!-----------------------------------------
      I=NUMSHEL ! counted in yctrl.F
!
      CALL HM_OPTION_COUNT('/INISH3', NB_INISH3)
!
      IF ( NB_INISH3 > 0 ) THEN
!
        ! Start reading /INISH3 card 
        CALL HM_OPTION_START('/INISH3')
!---
! to be replaced by ---  MAP_TABLES%ISH3NM ---
        IF (KTRIELTG==0) THEN
C         tri des elts du D00 par ID croissant (on ne trie qu'une fois)
          DO IE = 1, NUMELTG
            ITRI(IE) = IXTG(NIXTG,IE)
          END DO
          CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELTG,1)
          DO J = 1, NUMELTG
            IE=INDEX(J)
            KSYSUSRTG(J)       =IXTG(NIXTG,IE)
            KSYSUSRTG(NUMELTG+J)=IE
          END DO
          KTRIELTG=1
        END IF
!---
        DO INI=1,NB_INISH3
!
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            UNIT_ID  = UID,
     .                            SUBMODEL_INDEX = SUB_INDEX,
     .                            SUBMODEL_ID = SUB_ID,
     .                            KEYWORD2 = KEY,
     .                            KEYWORD3 = KEY2)
!
          IF (KEY2 /= ' ') GLOB = .TRUE.
!
!          WRITE(iout,*) 'MIRC',KEY(1:LEN_TRIM(KEY))
!
          DO IUNIT=1,UNITAB%NUNITS
            IF (UNITAB%UNIT_ID(IUNIT) == UID) THEN    
              IFLAGUNIT = 1
              EXIT
            ENDIF
          ENDDO
!
          IF (UID /= 0.AND.IFLAGUNIT == 0) THEN
            CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I2=UID, I1=SUB_ID, C1='INISH3',
     .                  C2='INISH3',
     .                  C3=' ')
          ENDIF
c---------------------------------------
          SELECT CASE (KEY(1:LEN_TRIM(KEY)))

            CASE ( 'EPSP_F' )
              ISIGSH =1
C---------
!
              CALL HM_GET_INTV('inish3_epsp_f_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                ! Reading  ---  ID_ELEM, NIP, NPG, THK  ---
                CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_FLOAT_ARRAY_INDEX('Thick',THK,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!                ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH3NM,NUMELTG)
!                IE = MAP_TABLES%ISH3NM(ELT,2)
!
                IE = UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
!
                IF (IE == 0) THEN        
                  ! Shell was not found. Issue a Warning & Skip.
                  NONEXIST = NONEXIST+1
                ELSE
!
                  I = NUMSHEL + PTSH3N(IE)
!
                  ID_SIGSH(I) = ID_ELEM
                  SIGSH(1,I) = ID_ELEM
                  SIGSH(2,I) = NIP
                  SIGSH(3,I) = THK
                  SIGSH(NVSHELL - 1,I) = ONE
!
                  IF (NPG <= 1) THEN
!---
!
                    SIZE = NIP*MAX(NPG,1)
                    CALL HM_GET_FLOAT_ARRAY('Ep',TMPVAL,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)!
!
                    PT=22
                    JJ=NIP*MAX(NPG,1)
                    K0 = 0
                    DO WHILE(JJ > 0)
                      L=MIN(JJ,5)
                      DO K=1,L
                        SIGSH(PT+(K-1)*6+5,I) = TMPVAL(K+K0)
                      ENDDO
!
                      K0=K0+5
                      PT=PT+30
                      JJ=JJ-5
                    END DO ! DO WHILE(JJ > 0)
!---------------------
                  ELSEIF (NPG > 1) THEN
                    SIGSH(NVSHELL,I) = NPG
!
                    IF (NIP == 0) THEN
!---
                      SIZE = NPG
                      CALL HM_GET_FLOAT_ARRAY('Ep',TMPVAL,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)!
!
                      PT=22
                      DO K=1,NPG
                        SIGSH(PT+(K-1)*9+5,I) = TMPVAL(K)
                      ENDDO
                    ELSE
!---
                      SIZE = NIP*NPG
                      CALL HM_GET_FLOAT_ARRAY('Ep',TMPVAL,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)!
!
                      PT=22
                      JJ=NIP*NPG
                      K0 = 0
                      DO WHILE(JJ > 0)
                        L=MIN(JJ,5)
                        DO K=1,L
                          SIGSH(PT+(K-1)*6+5,I) = TMPVAL(K+K0)
                        ENDDO
!
                        K0=K0+5
                        PT=PT+30
                        JJ=JJ-5
                      END DO ! DO WHILE(JJ > 0)
!---------------------
                    END IF ! IF (NIP == 0)
                  END IF ! (NPG<=1)
                ENDIF ! IF (IE /= 0)
!
              ENDDO ! DO J=1,NB_ELEMENTS
C---------
            CASE ( 'STRS_F' )
C---------
              ISIGSH =1

C------------------------------------
!            --- 'STRS_F/GLOB' ---
C------------------------------------

              IF (GLOB ) THEN
                CALL HM_GET_INTV('inish3_strs_f_glob_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                DO J=1,NB_ELEMENTS
                  ! Reading  ---  ID_ELEM, NIP, NPG, THK  ---
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_FLOAT_ARRAY_INDEX('Thick',THK,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!                  ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH3NM,NUMELTG)
!                  IE = MAP_TABLES%ISH3NM(ELT,2)
!
                  IE=UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
!
                  IF (IE == 0) THEN        
                    ! Shell was not found. Issue a Warning & Skip.
                    NONEXIST = NONEXIST+1
                  ELSE
!
                   ! Reading CARD_1 ---  EM,EB,H1,H2,H3  ---
                    CALL HM_GET_FLOAT_ARRAY_INDEX('Em',EM,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('Eb',EB,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                     I = NUMSHEL + PTSH3N(IE)
!
                    SIGSH(1,I) = ID_ELEM
                    ID_SIGSH(I) = ID_ELEM
                    SIGSH(2,I) = NIP
                    SIGSH(3,I) = THK
                    SIGSH(4,I) = EM
                    SIGSH(5,I) = EB
                    SIGSH(14,I) = ZERO
                    SIGSH(15,I) = ZERO
                    SIGSH(16,I) = ZERO
                    SIGSH(17,I) = ONE
                    SIGSH(NVSHELL - 1,I) = ONE
!----
                    IF (NPG == 0 .OR. NPG == 1) THEN
!----
                      IF (NIP == 0) THEN
                        ! Reading CARD_2 ---  sigma_X, sigma_Y, sigma_Z  ---
                        CALL HM_GET_FLOAT_ARRAY('sigma_X',SIGSH(22,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_Y',SIGSH(23,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_Z',SIGSH(18,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        ! Reading CARD_3 ---  sigma_XY, sigma_YZ, sigma_ZX  ---
                        CALL HM_GET_FLOAT_ARRAY('sigma_XY',SIGSH(24,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_YZ',SIGSH(25,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_ZX',SIGSH(26,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                        ! Reading CARD_4 ---  sigma_bX, sigma_bY, sigma_bZ  ---
                        CALL HM_GET_FLOAT_ARRAY('sigma_bX',SIGSH(28,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_bY',SIGSH(29,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_bZ',SIGSH(19,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        ! Reading CARD_5 ---  sigma_bXY, sigma_bYZ, sigma_bZX, eps_p  ---
                        CALL HM_GET_FLOAT_ARRAY('sigma_bXY',SIGSH(30,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_bYZ',SIGSH(20,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_bZX',SIGSH(21,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('eps_p'    ,SIGSH(27,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                      ELSEIF (NIP /= 0) THEN
!
!!                      CALL HM_GET_FLOAT_ARRAY('Ep',TMPVAL,36,J,IS_AVAILABLE,LSUBMODEL,UNITAB)!
!
                        SIZE = NIP
                        CALL HM_GET_FLOAT_ARRAY('sigma_X' ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_Y' ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_Z' ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_XY',TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_YZ',TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_ZX',TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('eps_p'   ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('pos_nip' ,TMPVAL8,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
C------------potision Ti  [-1,1]   'pos_nip'      --->      undocumented FIELD
!
                        INISHVAR = 22 + NIP*6
                        DO N=1,NIP
                          PT = 22 + (N-1)*6
                          ! Reading CARD_2 ---  sigma_X, sigma_Y, sigma_Z  ---
                          SIGSH(PT,I)             = TMPVAL1(N)
                          SIGSH(PT + 1,I)         = TMPVAL2(N)
                          SIGSH(INISHVAR + N,I)   = TMPVAL3(N)
                          SIGSH(PT + 2,I)         = TMPVAL4(N)
                          SIGSH(PT + 3,I)         = TMPVAL5(N)
                          SIGSH(PT + 4,I)         = TMPVAL6(N)
                          SIGSH(PT + 5,I)         = TMPVAL7(N)
                          SIGSH(INISHVAR+NIP+N,I) = TMPVAL8(N)
                        ENDDO ! DO K=1,NIP
                      ENDIF ! IF (NIP = 0) THEN
!----
                    ELSEIF (NPG > 1) THEN
!----
                      SIGSH(NVSHELL,I) = NPG
!
                      IF (NIP == 0) THEN
!
                        SIZE = NPG
                        CALL HM_GET_FLOAT_ARRAY('sigma_X'  ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_Y'  ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_Z'  ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_XY' ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_YZ' ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_ZX' ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_bX' ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_bY' ,TMPVAL8,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_bZ' ,TMPVAL9,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_bXY',TMPVAL10,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_bYZ',TMPVAL11,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_bZX',TMPVAL12,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('eps_p'    ,TMPVAL13,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                        DO K=1,NPG
                          PT= 22 + (K-1)*13
                          ! Reading CARD_2 ---  sigma_X, sigma_Y, sigma_Z  ---
                          SIGSH(PT  ,I)  = TMPVAL1(K)
                          SIGSH(PT+1,I)  = TMPVAL2(K)
                          SIGSH(PT+2,I)  = TMPVAL3(K)
                          SIGSH(PT+3,I)  = TMPVAL4(K)
                          SIGSH(PT+4,I)  = TMPVAL5(K)
                          SIGSH(PT+5,I)  = TMPVAL6(K)
                          SIGSH(PT+6,I)  = TMPVAL7(K)
                          SIGSH(PT+7,I)  = TMPVAL8(K)
                          SIGSH(PT+8,I)  = TMPVAL9(K)
                          SIGSH(PT+9,I)  = TMPVAL10(K)
                          SIGSH(PT+10,I) = TMPVAL11(K)
                          SIGSH(PT+11,I) = TMPVAL12(K)
                          SIGSH(PT+12,I) = TMPVAL13(K)
!                          SIGSH(PT:PT+11,I) = SIGSH(PT:PT+11,I)
!                          SIGSH(PT+12,I) = SIGSH(PT+12,I) 
                        ENDDO ! DO K=1,NPG
!
                      ELSE  ! NIP > 0
!
                        SIZE = NIP*NPG
                        CALL HM_GET_FLOAT_ARRAY('sigma_X' ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_Y' ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_Z' ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_XY',TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_YZ',TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_ZX',TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('eps_p'   ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('pos_nip' ,TMPVAL8,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
C------------potision Ti  [-1,1]   'pos_nip'      --->      undocumented FIELD
!                    
                        PT = 22
                        DO N=1,NIP
                          DO K=1,NPG
                            L = (N-1)*NPG+K
                            ! Reading CARD_2 ---  sigma_X, sigma_Y, sigma_Z  ---
                            SIGSH(PT  ,I) = TMPVAL1(L)
                            SIGSH(PT+1,I) = TMPVAL2(L)
                            SIGSH(PT+2,I) = TMPVAL3(L)
                            SIGSH(PT+3,I) = TMPVAL4(L)
                            SIGSH(PT+4,I) = TMPVAL5(L)
                            SIGSH(PT+5,I) = TMPVAL6(L)
                            SIGSH(PT+6,I) = TMPVAL7(L)
                            SIGSH(PT+7,I) = TMPVAL8(L)
                            PT = PT + 8
                          ENDDO ! DO N=1,NPG
                        ENDDO ! DO J=1,NIP
                      ENDIF ! IF (NIP == 0) THEN
!----
                    ENDIF ! IF (NPG == 0 .OR. NPG == 1)
!----
                  ENDIF ! IF (IE == 0)
                ENDDO ! DO I=1,NB_ELEMENTS

C------------------------------------
!            --- 'STRS_F' ---
C------------------------------------
C---------
!!            CASE ( 'STRS_F' )
C---------
!!              ISIGSH =1
!
              ELSEIF ( .NOT. GLOB ) THEN
!
                CALL HM_GET_INTV('inish3_strs_f_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                DO J=1,NB_ELEMENTS
                  ! Reading  CARD_1 ---  ID_ELEM, NIP, NPG, THK  ---
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_FLOAT_ARRAY_INDEX('Thick',THK,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!                  ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH3NM,NUMELTG)
!                  IE = MAP_TABLES%ISH3NM(ELT,2)
!
                  IE=UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
!
                  IF (IE == 0) THEN        
                    ! Shell was not found. Issue a Warning & Skip.
                    NONEXIST = NONEXIST+1
                  ELSE
!
                   ! Reading CARD_2 ---  EM,EB,H1,H2,H3  ---
                    CALL HM_GET_FLOAT_ARRAY_INDEX('Em',EM,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('Eb',EB,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    I = NUMSHEL + PTSH3N(IE)
                    !!
                    SIGSH(1,I) = ID_ELEM
                    ID_SIGSH(I) = ID_ELEM
                    SIGSH(2,I) = NIP
                    SIGSH(3,I) = THK
                    SIGSH(4,I) = EM
                    SIGSH(5,I) = EB
                    SIGSH(14,I) = ZERO
                    SIGSH(15,I) = ZERO
                    SIGSH(16,I) = ZERO
                    SIGSH(17,I) = ZERO
                    SIGSH(NVSHELL - 1,I) = ONE
!----
                    IF (NPG == 0 .OR. NPG == 1) THEN
!----
                      IF (NIP == 0) THEN
                        ! Reading CARD_3 ---  sigma_1, sigma_2, sigma_12, sigma_23, sigma_31  ---
                        CALL HM_GET_FLOAT_ARRAY('sigma_1' ,SIGSH(22,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_2' ,SIGSH(23,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_12',SIGSH(24,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_23',SIGSH(25,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_31',SIGSH(26,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                        ! Reading CARD_4 ---  eps_p, sigma_b1, sigma_b2, sigma_b12  ---
                        CALL HM_GET_FLOAT_ARRAY('eps_p'    ,SIGSH(27,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_b1' ,SIGSH(28,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_b2' ,SIGSH(29,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_b12',SIGSH(30,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                      ELSEIF (NIP /= 0) THEN
!
!!                      CALL HM_GET_FLOAT_ARRAY('Ep',TMPVAL,36,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!
                        SIZE = NIP
                        CALL HM_GET_FLOAT_ARRAY('sigma_1'  ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_2'  ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_12' ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_23' ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_31' ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('eps_p'    ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!
                        INISHVAR = 22 + NIP*6
                        DO N=1,NIP
                          PT = 22 + (N-1)*6
                          ! Reading CARD_3 ---  sigma_1, sigma_2, sigma_12  ---
                          SIGSH(PT    ,I) = TMPVAL1(N)
                          SIGSH(PT + 1,I) = TMPVAL2(N)
                          SIGSH(PT + 2,I) = TMPVAL3(N)
                          SIGSH(PT + 3,I) = TMPVAL4(N)
                          SIGSH(PT + 4,I) = TMPVAL5(N)
                          SIGSH(PT + 5,I) = TMPVAL6(N)
                        ENDDO ! DO K=1,NIP
                      ENDIF ! IF (NIP = 0) THEN
!----
                    ELSEIF (NPG > 1) THEN
!----
                      SIGSH(NVSHELL,I) = NPG
!
                      IF (NIP == 0) THEN
!
                        SIZE = NPG
                        CALL HM_GET_FLOAT_ARRAY('sigma_1'   ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_2'   ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_12'  ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_23'  ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_31'  ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('eps_p'     ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_b1'  ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_b2'  ,TMPVAL8,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_b12' ,TMPVAL9,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                        DO K=1,NPG
                          PT= 22 + (K-1)*9
                          ! Reading CARD_3 ---  sigma_1, sigma_2, sigma_12, sigma_23, sigma_31  ---
                          SIGSH(PT  ,I) = TMPVAL1(K)
                          SIGSH(PT+1,I) = TMPVAL2(K)
                          SIGSH(PT+2,I) = TMPVAL3(K)
                          SIGSH(PT+3,I) = TMPVAL4(K)
                          SIGSH(PT+4,I) = TMPVAL5(K)
                          SIGSH(PT+5,I) = TMPVAL6(K)
                          SIGSH(PT+6,I) = TMPVAL7(K)
                          SIGSH(PT+7,I) = TMPVAL8(K)
                          SIGSH(PT+8,I) = TMPVAL9(K)
                        ENDDO ! DO K=1,NPG
!
                      ELSE  ! NIP > 0
!
                        SIZE = NIP*NPG
                        CALL HM_GET_FLOAT_ARRAY('sigma_1'   ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_2'   ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_12'  ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_23'  ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('sigma_31'  ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                        CALL HM_GET_FLOAT_ARRAY('eps_p'     ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!                   
                        PT = 22
                        DO N=1,NIP
                          DO K=1,NPG
                            L = (N-1)*NPG+K
                            ! Reading CARD_3 ---  sigma_1, sigma_2, sigma_12  ---
                            SIGSH(PT  ,I) = TMPVAL1(L)
                            SIGSH(PT+1,I) = TMPVAL2(L)
                            SIGSH(PT+2,I) = TMPVAL3(L)
                            SIGSH(PT+3,I) = TMPVAL4(L)
                            SIGSH(PT+4,I) = TMPVAL5(L)
                            SIGSH(PT+5,I) = TMPVAL6(L)
!
                            PT = PT + 6
                          ENDDO ! DO K=1,NPG
                        ENDDO ! DO N=1,NIP
                      ENDIF ! IF (NIP == 0) THEN
!----
                    ENDIF ! IF (NPG == 0 .OR. NPG == 1)
!----
                  ENDIF ! IF (IE == 0)
                ENDDO ! DO I=1,NB_ELEMENTS
C---------
              ENDIF ! IF (GLOB ) THEN
C---------
C---------
            CASE ( 'STRA_F' )
C---------
              ITHKSHEL =2
C-------global sys with diff format
              IF ( GLOB ) THEN
                CALL HM_GET_INTV('inish3_stra_f_glob_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                DO J=1,NB_ELEMENTS
                  ! Reading  CARD_1 ---  ID_ELEM, NIP, NPG, THK  ---
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_FLOAT_ARRAY_INDEX('Thick',THK,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!                  ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH3NM,NUMELTG)
!                  IE = MAP_TABLES%ISH3NM(ELT,2)
!
                  IE=UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
!
                  IF (IE == 0) THEN        
                    ! Shell was not found. Issue a Warning & Skip.
                    NONEXIST = NONEXIST+1
                  ELSE
                    I = NUMSHEL + PTSH3N(IE)
                    SIGSH(1,I) = ID_ELEM
                    ID_SIGSH(I) = ID_ELEM
                    SIGSH(2,I) = NIP
                    SIGSH(3,I) = THK
                    SIGSH(17,I) = ONE
                    SIGSH(NVSHELL,I) = MAX(1,NPG)
                    SIGSH(NVSHELL - 1,I) = ONE
C----
                    PT = INISHVAR1
                    NPP = NIP
                    IF (NPP==0) NPP=2
!===============================================
                    SIZE = NPP*NPG
                    CALL HM_GET_FLOAT_ARRAY('eps_XX'  ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('eps_YY'  ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('eps_ZZ'  ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('eps_XY'  ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('eps_YZ'  ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('eps_ZX'  ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('T'       ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    DO N=1,MIN(2,NPP)
                      DO IPG=1,MAX(1,NPG)
                        L = (N-1)*MAX(1,NPG)+IPG
                        SIGSH(PT  ,I) = TMPVAL1(L)
                        SIGSH(PT+1,I) = TMPVAL2(L)
                        SIGSH(PT+2,I) = TMPVAL3(L)
                        SIGSH(PT+3,I) = TMPVAL4(L)
                        SIGSH(PT+4,I) = TMPVAL5(L)
                        SIGSH(PT+5,I) = TMPVAL6(L)
                        SIGSH(PT+6,I) = TMPVAL7(L)
                        PT=PT+7
                      ENDDO
                    ENDDO
!===============================================
                  ENDIF ! IF (IE == 0) THEN
                ENDDO ! DO J=1,NB_ELEMENTS
!
              ELSEIF ( .NOT.  GLOB ) THEN
!C---------local sy
!
                CALL HM_GET_INTV('inish3_stra_f_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                DO J=1,NB_ELEMENTS
                  ! Reading  CARD_1 ---  ID_ELEM, NIP, NPG, THK  ---
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_FLOAT_ARRAY_INDEX('Thick',THK,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!                  ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH3NM,NUMELTG)
!                  IE = MAP_TABLES%ISH4NM(ELT,2)
!
                  IE=UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
!
                  IF (IE == 0) THEN        
                    ! Shell was not found. Issue a Warning & Skip.
                    NONEXIST = NONEXIST+1
                  ELSE
                    I = NUMSHEL + PTSH3N(IE)
                    SIGSH(1,I) = ID_ELEM
                    ID_SIGSH(I) = ID_ELEM
                    SIGSH(3,I) = THK
                    SIGSH(NVSHELL - 1,I) = ONE
!
                    IF (NPG == 0 .OR. NPG == 1) THEN
!
                      CALL HM_GET_FLOAT_ARRAY('eps_1'  ,SIGSH(6,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('eps_2'  ,SIGSH(7,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('eps_12' ,SIGSH(8,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('eps_23' ,SIGSH(9,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('eps_31' ,SIGSH(10,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('k1'     ,SIGSH(11,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('k2'     ,SIGSH(12,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('k12'    ,SIGSH(13,I),1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    ELSEIF (NPG>1 ) THEN
!
                      SIGSH(NVSHELL,I) = NPG

                      SIGSH(6,I) =ZERO
                      SIGSH(7,I) =ZERO
                      SIGSH(8,I) =ZERO
                      SIGSH(9,I) =ZERO
                      SIGSH(10,I)=ZERO
                      SIGSH(11,I)=ZERO
                      SIGSH(12,I)=ZERO
                      SIGSH(13,I)=ZERO
!
                      SIZE = NPG
                      CALL HM_GET_FLOAT_ARRAY('eps_1'   ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('eps_2'   ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('eps_12'  ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('eps_23'  ,TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('eps_31'  ,TMPVAL5,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('k1'      ,TMPVAL6,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('k2'      ,TMPVAL7,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('k12'     ,TMPVAL8,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                      DO IPG=1,NPG
                        SIGSH(6,I) =SIGSH(6,I) +TMPVAL1(IPG)/NPG
                        SIGSH(7,I) =SIGSH(7,I) +TMPVAL2(IPG)/NPG
                        SIGSH(8,I) =SIGSH(8,I) +TMPVAL3(IPG)/NPG
                        SIGSH(9,I) =SIGSH(9,I) +TMPVAL4(IPG)/NPG
                        SIGSH(10,I)=SIGSH(10,I)+TMPVAL5(IPG)/NPG
                        SIGSH(11,I)=SIGSH(11,I)+TMPVAL6(IPG)/NPG
                        SIGSH(12,I)=SIGSH(12,I)+TMPVAL7(IPG)/NPG
                        SIGSH(13,I)=SIGSH(13,I)+TMPVAL8(IPG)/NPG
                      END DO
                    ELSE
C                    CALL ANCERR(58,ANINFO_BLIND_2)
                    ENDIF ! IF (NPG == 0 .OR. NPG == 1)
                  ENDIF ! IF (IE == 0) THEN
                ENDDO ! DO J=1,NB_ELEMENTS
              ENDIF ! IF ( GLOB ) THEN
C---------
            CASE ( 'THICK' )
C---------
              ITHKSHEL = 1
!
              CALL HM_GET_INTV('no_elems',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_FLOAT_ARRAY_INDEX('Thick'  ,THK,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!
!                  ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH3NM,NUMELTG)
!                  IE = MAP_TABLES%ISH3NM(ELT,2)
!
                IE=UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
!
                IF (IE == 0) THEN        
                  ! Shell was not found. Issue a Warning & Skip.
                  NONEXIST = NONEXIST+1
                ELSE
                  I = NUMSHEL + PTSH3N(IE)
                  SIGSH(1,I) = ID_ELEM
                  ID_SIGSH(I) = ID_ELEM
                  SIGSH(2,I) = 0
                  SIGSH(3,I) = THK
                ENDIF ! IF (IE == 0)
              ENDDO ! DO J=1,NB_ELEMENTS
C---------
            CASE ( 'EPSP' )
C---------
!
              CALL HM_GET_INTV('no_blocks',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_FLOAT_ARRAY_INDEX('Ep'  ,EPSP,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!                  ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH3NM,NUMELTG)
!                  IE = MAP_TABLES%ISH3NM(ELT,2)
!
                IE=UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
!
                IF (IE == 0) THEN
                  ! Shell was not found. Issue a Warning & Skip.
                  NONEXIST = NONEXIST+1
                ELSE
                  I = NUMSHEL + PTSH3N(IE)
                  SIGSH(1,I) = ID_ELEM
                  ID_SIGSH(I) = ID_ELEM
                  SIGSH(2,I) = 0
                  SIGSH(27,I)= EPSP
                ENDIF ! IF (IE == 0) THEN
              ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
              CASE ( 'ORTHO' )
!-------------------
                CALL HM_GET_INTV('inish3_ortho_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
!!                  CALL HM_GET_INT_ARRAY_INDEX('prop_type',IGTYP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_FLOAT_ARRAY_INDEX('Vx',VX,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                  CALL HM_GET_FLOAT_ARRAY_INDEX('Vy',VY,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                  CALL HM_GET_FLOAT_ARRAY_INDEX('Vz',VZ,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
!                  ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH3NM,NUMELTH)
!                  IE = MAP_TABLES%ISH3NM(ELT,2)
!
                  IE=UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
!
                  IF (IE == 0) THEN
                    ! Shell was not found. Issue a Warning & Skip.
                    NONEXIST = NONEXIST+1
                  ELSE
!
                    IG   = IXTG(5,IE)
                    ISH3N = IGEO(18,IG)
                    IGTYP=IGEO(11,IG)
                    IORTSHEL = 1
                    I = NUMSHEL + PTSH3N(IE)
                    PT = NVSHELL+NUSHELL
                   !! SIGSH(1,I) = ID_ELEM
                    ID_SIGSH(I) = ID_ELEM
                    IF ( IGTYP == 9) NIP = NINT(GEO(NPROPG*(IG-1)+6))
                      SIGSH(PT + 4,I) = NIP  
                    IF( ISH3N == 30 ) THEN
                      SIGSH(NVSHELL,I) = 3
                    ELSE
                      SIGSH(NVSHELL,I) = 1
                    ENDIF
                    SIGSH(PT+1,I) = VX
                    SIGSH(PT+2,I) = VY
                    SIGSH(PT+3,I) = VZ
                    PT = PT+4
                    IF ( IGTYP == 9 ) THEN
                      CALL HM_GET_FLOAT_ARRAY_INDEX('phi_1',PHI1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY_INDEX('phi_2',PHI2,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      SIGSH(PT+1,I) = PHI1*PI/HUNDRED80
                      SIGSH(PT+2,I) = PHI2*PI/HUNDRED80
                      PT = PT + 2
                    ELSEIF (IGTYP == 1 ) THEN
                      CALL ANCMSG(MSGID=761,
     .                            MSGTYPE=MSGERROR,
     .                            ANMODE=ANINFO,
     .                            C1='/INISH3/ORTHO',
     .                            C2='SH3N',
     .                            I2=ID_ELEM,I1=IGEO(1,IG))
                    ELSE
                      SIZE = NIP
                      CALL HM_GET_FLOAT_ARRAY('phi_1_array',TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      CALL HM_GET_FLOAT_ARRAY('phi_2_array',TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                      DO JJ = 1,NIP
                        SIGSH(PT+1,I) = TMPVAL1(JJ)*PI/HUNDRED80
                        SIGSH(PT+2,I) = TMPVAL2(JJ)*PI/HUNDRED80
                        PT = PT + 2
                      ENDDO ! DO JJ = 1,NIP
                    ENDIF ! IF ( IGTYP == 9)
                  ENDIF ! IF (IE == 0) THEN
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
              CASE ( 'ORTH_LOC' )
!-------------------
                CALL HM_GET_INTV('inish3_orth_loc_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nb_lay',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('ndir',NDIR,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Iunit',FLAGDEG,J,IS_AVAILABLE,LSUBMODEL)
!
!
!                  ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH3NM,NUMELTG)
!                  IE = MAP_TABLES%ISH3NM(ELT,2)
!
                  IE=UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
!
                  IF (IE == 0) THEN
                    ! Shell was not found. Issue a Warning & Skip.
                    NONEXIST = NONEXIST+1
                  ELSE
!
                    IG    = IXTG(5,IE)
                    ISH3N = IGEO(18,IG)
                    IGTYP = IGEO(11,IG)
                    IORTSHEL = 2
                    I = NUMSHEL + PTSH3N(IE)
                    PT = NVSHELL + NUSHELL 
                    SIGSH(1,I) = ID_ELEM
                    ID_SIGSH(I) = ID_ELEM
                    IF (IGTYP == 9) NIP = NINT(GEO(NPROPG*(IG-1)+6))
                    SIGSH(PT + 4 ,I) = NIP
                    SIGSH(PT + 5,I)  = ONE
                    IF (ISH3N == 30) THEN
                      SIGSH(NVSHELL,I) = 3
                    ELSE
                      SIGSH(NVSHELL,I) = 1
                    ENDIF
                    PT = PT + 5 
!
                    IF (IGTYP == 51 .OR. IGTYP == 52) THEN
                          ISUBSTACK = IWORKSH(3, NUMELC + IE)
                          NLAY = STACK%IGEO(1,ISUBSTACK)
                          IPMAT  =  2 + NLAY 
                      IF (NDIR /= 2) THEN
                        DO JJ = 1,NLAY
                          MLAWLY= STACK%IGEO(IPMAT + JJ,ISUBSTACK)  ! layer material
                          IF (MLAWLY == 58) THEN
                            CALL ANCMSG(MSGID=1126,
     .                                  MSGTYPE=MSGERROR,
     .                                  ANMODE=ANINFO,
     .                                  C1='SH3N',
     .                                  I1=ID_ELEM)
                          ENDIF
                        ENDDO
                      ENDIF
                    ENDIF ! IF (IGTYP == 51 .OR. IGTYP == 52)
!
                    ALLOCATE(MLAW_LY(NIP))
                    MLAW_LY = 0
                    SIZE = NIP
                    CALL HM_GET_FLOAT_ARRAY('phi_i'  ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY('alpha_i',TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    IF (IGTYP == 9) THEN
                      ANGLE1 = TMPVAL1(1) ! one integration point
                      IF(FLAGDEG == 1) ANGLE1 = ANGLE1*PI/HUNDRED80
                      SIGSH(PT+1,I) = COS(ANGLE1)
                      SIGSH(PT+2,I) = SIN(ANGLE1)
                      PT = PT + 2
                    ELSEIF (IGTYP == 10 .OR. IGTYP == 11 .OR. IGTYP == 16 .OR.
     .                      IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
                      IF (IGTYP == 51 .OR. IGTYP == 52)THEN
                        ISUBSTACK = IWORKSH(3, NUMELC + IE)
                        NLAY =  STACK%IGEO(1,ISUBSTACK) !                                     
                        IPMAT  =  2 + NLAY
                        IPNPT_LAY = IPMAT + 2*NLAY      
                        IF(NLAY /= NIP) THEN
                          IF(NDRAPE > 0) THEN 
                           IPT = 0                               
                           DO JJ =1,NLAY
                              NSLICE = STACK%IGEO(IPNPT_LAY  + JJ,ISUBSTACK)
                              DO N = 1, NSLICE
                                 IPT = IPT + 1
                                  MLAW_LY(IPT)= STACK%IGEO(IPMAT + JJ,ISUBSTACK)
                              ENDDO
                           ENDDO                                                             
                          ELSE
                           ! error message
                          ENDIF ! ndrape
                        ELSE
                          DO JJ =1,NLAY                                                    
                              MLAW_LY(JJ)= STACK%IGEO(IPMAT + JJ,ISUBSTACK)! layer material 
                           ENDDO  
                        ENDIF                                                                
                      ENDIF    
                      DO JJ = 1,NIP
                        ANGLE1 = TMPVAL1(JJ)
                        ANGLE2 = TMPVAL2(JJ)
                        IF(FLAGDEG == 1) ANGLE1 = ANGLE1*PI/HUNDRED80
                        IF(FLAGDEG == 1) ANGLE2 = ANGLE2*PI/HUNDRED80
!
                        IF (IGTYP == 16 .OR.
     .                     (IGTYP == 51  .AND. MLAW_LY(JJ) == 58) .OR. 
     .                     (IGTYP == 52  .AND. MLAW_LY(JJ) == 58) ) THEN
!
                          ANGLE2 = ANGLE2 + ANGLE1
                          SIGSH(PT+1,I) = COS(ANGLE1)
                          SIGSH(PT+2,I) = SIN(ANGLE1)
                          SIGSH(PT+3,I) = COS(ANGLE2)
                          SIGSH(PT+4,I) = SIN(ANGLE2)
                          PT = PT + 4
                        ELSE
                          ANGLE1 = TMPVAL1(JJ)
                          ANGLE1 = ANGLE1*PI/HUNDRED80
                          SIGSH(PT+1,I) = COS(ANGLE1)
                          SIGSH(PT+2,I) = SIN(ANGLE1)
                          PT = PT + 2
                        ENDIF
                      ENDDO ! DO JJ = 1,NIP
                    ELSEIF (IGTYP == 1) THEN
                      CALL ANCMSG(MSGID=761,
     .                            MSGTYPE=MSGERROR,
     .                            ANMODE=ANINFO,
     .                            C1='/INISH3/ORTH_LOC',
     .                            C2='3 NODES SHELL',
     .                            I2=ID_ELEM,I1=IGEO(1,IG))
                    ENDIF ! IF (IGTYP == 9)
                    IF(ALLOCATED(MLAW_LY))DEALLOCATE(MLAW_LY)
                  ENDIF ! IF (IE == 0) THEN
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
              CASE ( 'SCALE_YLD' )
!-------------------
                CALL HM_GET_INTV('inish3_scale_yld_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
                IYLDINI = 1
!
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
!
!                  ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH3NM,NUMELTG)
!                  IE = MAP_TABLES%ISH3NM(ELT,2)
!
                  IE=UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
!
                  IF (IE == 0) THEN
                    ! Shell was not found. Issue a Warning & Skip.
                    NONEXIST = NONEXIST+1
                  ELSE
                    I = NUMSHEL + PTSH3N(IE)
                    SIGSH(NVSHELL + 1,I) = ID_ELEM ! elt ID
                    ID_SIGSH(I) = ID_ELEM
                    SIGSH(NVSHELL + 2,I) = NIP ! integ point
                    SIGSH(NVSHELL + 3,I) = NPG
!
                    SIZE = NPG*NIP
                    CALL HM_GET_FLOAT_ARRAY('Alpha_ij'  ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    DO N = 1,NPG
                      DO IP = 1,NIP
                        L = (N-1)*NIP+IP
                        PT=NVSHELL + 3  !22
                        SCALEYLD = TMPVAL1(L)
                        SIGSH(PT+ L,I) = SCALEYLD
                      ENDDO !IP = 1,NIP
                    ENDDO !N = 1,NPG
                    PT = PT + NIP * NPG
!
                  ENDIF ! IF (IE == 0) THEN
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
              CASE ( 'AUX' )
!-------------------
                CALL HM_GET_INTV('inish3_aux_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nvars',NUVAR,J,IS_AVAILABLE,LSUBMODEL)
!
!
!                  ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH3NM,NUMELTG)
!                  IE = MAP_TABLES%ISH3NM(ELT,2)
!
                  IE=UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
!
                  IF (IE == 0) THEN
                    ! Shell was not found. Issue a Warning & Skip.
                    NONEXIST = NONEXIST+1
                  ELSE
C----------
                    IMAT = IXTG(1,IE)
                    ILAW = IPM(2,IMAT)
                    NUVARD00 = IPM(8,IMAT)
                    IF (NUVARD00 > NUVAR) THEN
                      CALL ANCMSG(MSGID=1121,
     .                            MSGTYPE=MSGWARNING,
     .                            ANMODE=ANINFO,
     .                            I1=ITRI(IE),
     .                            C1='NUMBER OF USER VARIABLES',
     .                            C2='MATERIAL LAW ',
     .                            I2=IPM(1,IMAT),
     .                            C3='/INISH3/AUX')
                    ENDIF
                    IF ((ILAW == 36 .and. (NUVAR < 4 .or. NUVARD00 > 3) .and. 
     .                   NUVARD00 < NUVAR) .or.
     .                  (ILAW /= 36 .and. ILAW /= 78 .and. ILAW /= 87 .and. ILAW /= 112 .and. NUVARD00 < NUVAR)) THEN
                      CALL ANCMSG(MSGID=695,
     .                            MSGTYPE=MSGERROR,
     .                            ANMODE=ANINFO,
     .                            I1=ITRI(IE),
     .                            C1='NUMBER OF USER VARIABLES',
     .                            C2='MATERIAL LAW ',
     .                            I2=IPM(1,IMAT),
     .                            C3='/INISH3/AUX')
                    ENDIF
C----------
                    I = NUMSHEL + PTSH3N(IE)
                    IUSER    = 1
                    NVARSH = NVSHELL + 4
                    IF (NIP == 0) NIP = 1
                    IF (NPG == 0) NPG = 1
                    SIGSH(1,I) = ID_ELEM
                    ID_SIGSH(I) = ID_ELEM
                    SIGSH(2,I) = NIP
                    SIGSH(NVSHELL,I) = NPG
                    SIGSH(NVSHELL + 2 ,I) = NIP
                    SIGSH(NVSHELL + 3 ,I) = NPG
                    SIGSH(NVSHELL + 4 ,I) = NUVAR
                    PT = 0
!
                    CALL HM_GET_INT_ARRAY_INDEX('num_lines',NUM_LINES,J,IS_AVAILABLE,LSUBMODEL)
                    NMAX_AUX = NUM_LINES*NUVAR
                    CALL HM_GET_FLOAT_ARRAY('V'  ,TMPVAL,NMAX_AUX,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    DO JJ=1,NUM_LINES
                      DO K=1,NUVAR
                        L = NUVAR*(JJ-1) + K
                        SIGSH(NVARSH+PT+K,I) = TMPVAL(L)
                      ENDDO ! DO K=1,NUVAR
                      PT = PT + NUVAR
                    ENDDO ! DO JJ=1,NUM_LINES
!
                  ENDIF ! IF (IE == 0) THEN
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
              CASE ( 'FAIL' )
!-------------------
                CALL HM_GET_INTV('inish3_fail_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID' ,ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Nlay'     ,NLAY,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg'      ,NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nptt'     ,NPTT,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('lay_ID'   ,ILAY,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('fail_ID'  ,IFAIL,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Ifail_typ',IRUPT_TYP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Nvar'     ,NVAR_RUPT,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('mat_ID'   ,IMAT,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('num_lines',NUM_LINES,J,IS_AVAILABLE,LSUBMODEL)
!
!                  ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISH3NM,NUMELTG)
!                  IE = MAP_TABLES%ISH3NM(ELT,2)
!
                  IE=UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
!
                  IF (IE == 0) THEN
                    ! Shell was not found. Issue a Warning & Skip.
                    NONEXIST = NONEXIST+1
                  ELSE
C----------
                    NPTT = MAX(1,NPTT)
                    NLAY = MAX(1,NLAY)
                    NPT_MAX = MAX(NPTT,NLAY)
                    NVMAX = NVSHELL1 /(MAX(1,NPG)*NPT_MAX*5)
                    !!IF (ID_ELEM /= NEM1) I = PTSH3N(IE)
                    NEM1 = ID_ELEM
                    I = NUMSHEL + PTSH3N(IE)
                    IOK = 0
!
                    DO K=1,NUMMAT
                      IF (IPM(1,K) == IMAT) THEN
                        IMAT = K
                        IOK = 1
                        EXIT
                      ENDIF
                    ENDDO
                    IF (IOK == 0) THEN
                      CALL ANCMSG(MSGID=1033,
     .                            MSGTYPE=MSGERROR,
     .                            ANMODE=ANINFO,
     .                            I1=ITRI(IE),
     .                            C1='MATERIAL LAW',
     .                            C2='/INISHE/FAIL')
                    ENDIF ! IF (IOK == 0)
!
                    IG   = IXTG(5,IE)
                    ISH3N = IGEO(18,IG)
                    IGTYP=IGEO(11,IG)
                    SIGSH(1,I)  = ID_ELEM
                    ID_SIGSH(I) = ID_ELEM
                    IF ( IGTYP == 9 ) NLAY = NINT(GEO(NPROPG*(IG-1)+6))
                    IF ( IGTYP == 10 .OR. IGTYP == 11) THEN
                      SIGSH(2,I) = NLAY
                    ELSE
                      SIGSH(2,I) = NPTT*NLAY
                    ENDIF
                    IF( ISH3N == 30 ) THEN
                      SIGSH(NVSHELL,I) = 3
                    ELSE
                      SIGSH(NVSHELL,I) = 1
                    ENDIF
!
!         check for consistency ( D00 & INIBRI)
                    IOK = 0
                    DO K=1,5
                      NFAIL(K) = MAT_PARAM(IMAT)%FAIL(K)%FAIL_ID
                      IF (IFAIL == NFAIL(K) .AND. 
     .                    IRUPT_TYP == MAT_PARAM(IMAT)%FAIL(K)%IRUPT) THEN
                        IFAIL = K
                        FAIL_INI(IFAIL)=1
                        IOK = 1
                        EXIT
                      ENDIF
                    ENDDO
                    IF (IOK == 0) THEN
                      CALL ANCMSG(MSGID=1033,
     .                            MSGTYPE=MSGERROR,
     .                            ANMODE=ANINFO,
     .                            I1=ITRI(IE),
     .                            C1='FAILURE CRITERIA',
     .                            C2='/INISH3/FAIL')
                    ENDIF
!
                    PT = NVSHELL+NUSHELL+3+NORTSHEL
                    NPG = MAX(1,NPG)
                    NPTT = MAX(1,NPTT)
                    NLAY = MAX(1,NLAY)
!
                    NMAX_FAIL = NUM_LINES*NVAR_RUPT
                    CALL HM_GET_FLOAT_ARRAY('V'  ,TMPVAL,NMAX_FAIL,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    DO JJ=1,NUM_LINES
                      DO K=1,NVAR_RUPT
                        L = NVAR_RUPT*(JJ-1) + K
                        SIGSH(PT+L+(IFAIL-1)*NPT_MAX*NPG*NVMAX+
     .                              (ILAY-1)*NVMAX*NPG*NPTT,I) = TMPVAL(L)
                      ENDDO ! DO K=1,NVAR_RUPT
                    ENDDO ! DO JJ=1,NUM_LINES
!
                  ENDIF ! IF (IE == 0) THEN
                ENDDO ! DO J=1,NB_ELEMENTS
!---------------
            CASE DEFAULT

          END SELECT ! SELECT CASE(KEY)
!
        ENDDO ! DO INI=1,NB_INISH3

      ENDIF ! IF ( NB_INISH3 > 0 )
!
      NISH3N = I-NISHELL
!
!-----------------------------------------
!            ---  /INITRUSS  ---
!-----------------------------------------
      NITRUSS = 0
      I = 0
!
      CALL HM_OPTION_COUNT('/INITRUSS', NB_INITRUSS)
!
      IF ( NB_INITRUSS > 0 ) THEN
!
        ! Start reading /INITRUSS card 
        CALL HM_OPTION_START('/INITRUSS')
!---
! to be replaced by ---  MAP_TABLES%ITRUSSM ---
           IF (KTRIELTRUSS == 0) THEN
C             tri des elts du D00 par ID croissant (on ne trie qu'une fois)
              DO IE = 1, NUMELT
                ITRI(IE) = IXT(NIXT,IE)
              END DO
              CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELT,1)
              DO J = 1, NUMELT
                IE=INDEX(J)
                KSYSUSR(J)         =IXT(NIXT,IE)
                KSYSUSR(NUMELT+J)=IE
              END DO
              KTRIELTRUSS=1
           ENDIF
!---
        DO INI=1,NB_INITRUSS
!
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            UNIT_ID  = UID,
     .                            SUBMODEL_INDEX = SUB_INDEX,
     .                            SUBMODEL_ID = SUB_ID,
     .                            KEYWORD2 = KEY)
!
          IFLAGUNIT = 0
          DO IUNIT=1,UNITAB%NUNITS
            IF (UNITAB%UNIT_ID(IUNIT) == UID) THEN    
              IFLAGUNIT = 1
              EXIT
            ENDIF
          ENDDO
          IF (UID /= 0.AND.IFLAGUNIT == 0) THEN
            CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                 I2=UID,I1=SUB_ID,C1='INITRUSS',
     .                 C2='INITRUSS',
     .                 C3=' ')
          ENDIF
c---------------------------------------
          SELECT CASE (KEY(1:LEN_TRIM(KEY)))
C---------
            CASE ( 'FULL' )
C---------
!
              CALL HM_GET_INTV('no_of_elems',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                ! Reading  ---  ID_ELEM, Prop ...  ---
                CALL HM_GET_INT_ARRAY_INDEX('truss_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('prop_type',IGTYP,J,IS_AVAILABLE,LSUBMODEL)
!
!                ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ITRUSSM,NUMELT)
!                IE = MAP_TABLES%ITRUSSM(ELT,2)
!
                IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELT)
!
                IF (IE == 0) THEN        
                  ! Shell was not found. Issue a Warning & Skip.
                  NONEXIST = NONEXIST+1
                ELSE
!
                 CALL HM_GET_FLOAT_ARRAY_INDEX('EINT' ,EIN,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                 CALL HM_GET_FLOAT_ARRAY_INDEX('F'    ,FOR,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                 CALL HM_GET_FLOAT_ARRAY_INDEX('AREA' ,EPSP,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                 CALL HM_GET_FLOAT_ARRAY_INDEX('Eps_p',AREA,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                 I=I+1
                 ID_SIGTRUSS(I) = ID_ELEM
                 SIGTRUSS(1,I) = ID_ELEM
                 SIGTRUSS(2,I) = IGTYP
                 SIGTRUSS(3,I) = EIN
                 SIGTRUSS(4,I) = FOR
                 SIGTRUSS(5,I) = EPSP
                 SIGTRUSS(6,I) = AREA
!
                ENDIF ! IF (IE == 0)
              ENDDO ! DO J=1,NB_ELEMENTS
!
            CASE DEFAULT
!
          END SELECT ! SELECT CASE(KEY)
!
        ENDDO ! DO INI=1,NB_NITRUSS

      ENDIF ! IF ( NB_NITRUSS > 0 )
!
      NITRUSS = I



!-----------------------------------------
!            ---  /INIBEAM  ---
!-----------------------------------------
      NIBEAM = 0
      I = 0
!
      CALL HM_OPTION_COUNT('/INIBEAM', NB_INIBEAM)
!
      IF ( NB_INIBEAM > 0 ) THEN
!
        ! Start reading /INIBEAM card 
        CALL HM_OPTION_START('/INIBEAM')
!---
! to be replaced by ---  MAP_TABLES%IBEAMM ---
            IF (KTRIELBEAM == 0) THEN
! tri local des elts du D00 par ID croissant (on ne trie qu'une fois)
              DO IE = 1,NUMELP
                ITRI(IE) = IXP(NIXP,IE)
              ENDDO
              CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELP,1)
              DO J = 1,NUMELP
                IE = INDEX(J)
                KSYSUSR(J)       =IXP(NIXP,IE)
                KSYSUSR(NUMELP+J)=IE
              ENDDO
              KTRIELBEAM=1
            ENDIF ! IF (KTRIELBEAM==0)
!---
        DO INI=1,NB_INIBEAM
!
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            UNIT_ID  = UID,
     .                            SUBMODEL_INDEX = SUB_INDEX,
     .                            SUBMODEL_ID = SUB_ID,
     .                            KEYWORD2 = KEY)
!
          IFLAGUNIT = 0
          DO IUNIT=1,UNITAB%NUNITS
            IF (UNITAB%UNIT_ID(IUNIT) == UID) THEN    
              IFLAGUNIT = 1
              EXIT
            ENDIF
          ENDDO
          IF (UID /= 0.AND.IFLAGUNIT == 0) THEN
            CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                 I2=UID,I1=SUB_ID,C1='INIBEAM',
     .                 C2='INIBEAM',
     .                 C3=' ')
          ENDIF
c---------------------------------------
          SELECT CASE (KEY(1:LEN_TRIM(KEY)))
C---------
            CASE ( 'FULL' )
C---------
!
              CALL HM_GET_INTV('inibeam_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                ! Reading  ---  ID_ELEM, Prop ...  ---
                CALL HM_GET_INT_ARRAY_INDEX('beam_ID'  ,ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('prop_type',IGTYP,J,IS_AVAILABLE,LSUBMODEL)
!
!                ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%IBEAMM,NUMELP)
!                IE = MAP_TABLES%ITRUSSM(ELT,2)
!
                IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELP)
!
                IF (IE == 0) THEN        
                  ! Shell was not found. Issue a Warning & Skip.
                  NONEXIST = NONEXIST+1
                ELSE
                 CALL HM_GET_FLOAT_ARRAY_INDEX('EImemb' ,EM,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                 CALL HM_GET_FLOAT_ARRAY_INDEX('EIbend' ,EB,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                 CALL HM_GET_FLOAT_ARRAY_INDEX('F1' ,FOR1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                 CALL HM_GET_FLOAT_ARRAY_INDEX('F2' ,FOR2,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                 CALL HM_GET_FLOAT_ARRAY_INDEX('F3' ,FOR3,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                 CALL HM_GET_FLOAT_ARRAY_INDEX('M1' ,MOM1,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                 CALL HM_GET_FLOAT_ARRAY_INDEX('M2' ,MOM2,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                 CALL HM_GET_FLOAT_ARRAY_INDEX('M3' ,MOM3,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                 I=I+1
                 ID_SIGBEAM(I) = ID_ELEM
                 SIGBEAM(1,I) = ID_ELEM
                 SIGBEAM(2,I) = NIP
                 SIGBEAM(3,I) = IGTYP
!
                 SIGBEAM(4,I) = EM
                 SIGBEAM(5,I) = EB
!
                 SIGBEAM(6,I)  = FOR1
                 SIGBEAM(7,I)  = FOR2
                 SIGBEAM(8,I)  = FOR3
                 SIGBEAM(9,I)  = MOM1
                 SIGBEAM(10,I) = MOM2
                 SIGBEAM(11,I) = MOM3
!
                 PT = 11
                 IF (NIP == 0) THEN
                   IF (IGTYP == 3) THEN
                     CALL HM_GET_FLOAT_ARRAY_INDEX('EpsilonP' ,EPSP,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                     SIGBEAM(PT+1,I) = EPSP
                   ENDIF ! IF (IGTYP == 3)
                 ELSEIF (NIP > 0) THEN

                   IF (IGTYP == 18) THEN
                     SIZE = NIP
                     CALL HM_GET_FLOAT_ARRAY('Sigma1'        ,TMPVAL1,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                     CALL HM_GET_FLOAT_ARRAY('Sigma12'       ,TMPVAL2,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                     CALL HM_GET_FLOAT_ARRAY('Sigma13'       ,TMPVAL3,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                     CALL HM_GET_FLOAT_ARRAY('EpsilonP_array',TMPVAL4,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                     DO K=1,NIP
                       SIGBEAM(PT+1,I) = TMPVAL1(K)   ! SXX
                       SIGBEAM(PT+2,I) = TMPVAL2(K)   ! SXY
                       SIGBEAM(PT+3,I) = TMPVAL3(K)   ! SZX
                       SIGBEAM(PT+4,I) = TMPVAL4(K)   ! EPSP
!
                       PT = PT + 4
                     ENDDO ! DO K=1,NIP
                   ENDIF ! IF (IGTYP == 18)
!------
                 ENDIF ! IF (NIP == 0)
!
                ENDIF ! IF (IE == 0)
!
              ENDDO ! DO J=1,NB_ELEMENTS
!
C---------
            CASE ( 'AUX' )
C---------
!
!
              CALL HM_GET_INTV('inibeam_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                ! Reading  ---  ID_ELEM, Prop ...  ---
                CALL HM_GET_INT_ARRAY_INDEX('beam_ID'  ,ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('prop_type',IGTYP,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('nvars'    ,NUVAR,J,IS_AVAILABLE,LSUBMODEL)
!
!                ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%IBEAMM,NUMELP)
!                IE = MAP_TABLES%ITRUSSM(ELT,2)
!
                IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELP)
!
                IF (IE == 0) THEN        
                  ! Shell was not found. Issue a Warning & Skip.
                  NONEXIST = NONEXIST+1
                ELSE
!
!              -- UVAR --
!
                  I=I+1
                  ID_SIGBEAM(I) = ID_ELEM
                  SIGBEAM(1,I) = ID_ELEM
                  SIGBEAM(2,I) = NIP
!
                  IUSER    = 1
                  NVARBEAM = NVBEAM + 4
                  SIGBEAM(NVBEAM + 1 ,I) = ID_ELEM
                  SIGBEAM(NVBEAM + 2 ,I) = NIP
                  SIGBEAM(NVBEAM + 3 ,I) = IGTYP
                  SIGBEAM(NVBEAM + 4 ,I) = NUVAR
!
                  IF (IGTYP /= 18) THEN
                    CALL ANCMSG(MSGID=1236,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                          C1='AUX',
     .                          I1=ID_ELEM)
                  ENDIF
!
                  PT = 0
!
                  NMAX_AUX = NIP*NUVAR
                  CALL HM_GET_FLOAT_ARRAY('V'  ,TMPVAL,NMAX_AUX,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    DO JJ=1,NIP
                      DO K=1,NUVAR
                        L = NUVAR*(JJ-1) + K
                        SIGBEAM(NVARBEAM+PT+K,I) = TMPVAL(L)
                      ENDDO ! DO K=1,NUVAR
                      PT = PT + NUVAR
                    ENDDO ! DO JJ=1,NIP
!
                ENDIF ! IF (IE == 0)
!
              ENDDO ! DO J=1,NB_ELEMENTS
!
            CASE DEFAULT
!
          END SELECT ! SELECT CASE(KEY)
!
        ENDDO ! DO INI=1,NB_INIBEAM

      ENDIF ! IF ( NB_INIBEAM > 0 )
!
      NIBEAM = I



!-----------------------------------------
!            ---  /INISPRI  ---
!-----------------------------------------
      NISPRING = 0
      I = 0
!
      CALL HM_OPTION_COUNT('/INISPRI', NB_INISPRI)
!
      IF ( NB_INISPRI > 0 ) THEN
!
        ! Start reading /INISPRI card 
        CALL HM_OPTION_START('/INISPRI')
!---
! to be replaced by ---  MAP_TABLES%ISPRINGM ---
            IF (KTRIELSPR == 0) THEN
C tri local des elts du D00 par ID croissant (on ne trie qu'une fois)
              DO IE = 1,NUMELR
                ITRI(IE) = IXR(NIXR,IE)
              ENDDO
              CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELR,1)
              DO J = 1,NUMELR
                IE = INDEX(J)
                KSYSUSR(J)       =IXR(NIXR,IE)
                KSYSUSR(NUMELR+J)=IE
              ENDDO
              KTRIELSPR=1
            ENDIF ! IF (KTRIELSPR==0)
!---
        DO INI=1,NB_INISPRI
!
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            UNIT_ID  = UID,
     .                            SUBMODEL_INDEX = SUB_INDEX,
     .                            SUBMODEL_ID = SUB_ID,
     .                            KEYWORD2 = KEY)
!
          IFLAGUNIT = 0
          DO IUNIT=1,UNITAB%NUNITS
            IF (UNITAB%UNIT_ID(IUNIT) == UID) THEN    
              IFLAGUNIT = 1
              EXIT
            ENDIF
          ENDDO
          IF (UID /= 0.AND.IFLAGUNIT == 0) THEN
            CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                 I2=UID,I1=SUB_ID,C1='INISPRING',
     .                 C2='INISPRI',
     .                 C3=' ')
          ENDIF
c---------------------------------------
          SELECT CASE (KEY(1:LEN_TRIM(KEY)))
C---------
            CASE ( 'FULL' )
C---------
!
              CALL HM_GET_INTV('size_spring',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                ! Reading  ---  ID_ELEM, Prop ...  ---
                CALL HM_GET_INT_ARRAY_INDEX('spring_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('prop_type',IGTYP,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_INT_ARRAY_INDEX('nvars'    ,NUVAR,J,IS_AVAILABLE,LSUBMODEL)
!
!                ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%ISPRINGM,NUMELR)
!                IE = MAP_TABLES%ISPRINGM(ELT,2)
!
                IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELR)
!
                IF (IE == 0) THEN        
                  ! Shell was not found. Issue a Warning & Skip.
                  NONEXIST = NONEXIST+1
                ELSE
                  I=I+1
                  ID_SIGSPRI(I) = ID_ELEM
                  SIGRS(1,I) = ID_ELEM
C------
                  IF (IGTYP == 4  .OR. IGTYP == 12) THEN
C------
                    CALL HM_GET_FLOAT_ARRAY_INDEX('F_X'    ,SIGRS(2,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('D_X'    ,SIGRS(3,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('FEP_X'  ,SIGRS(4,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('DPL_X+' ,SIGRS(5,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('DPL_X-' ,SIGRS(6,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('L_X'    ,SIGRS(7,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('EI'     ,SIGRS(8,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    IF (IGTYP == 12) THEN
                      CALL HM_GET_FLOAT_ARRAY_INDEX('DFS'  ,SIGRS(9,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    ENDIF
C------
                  ELSEIF (IGTYP == 8 .OR. IGTYP == 13 .OR. IGTYP == 25 .OR. IGTYP == 23) THEN
C------
                    CALL HM_GET_FLOAT_ARRAY_INDEX('F_X'    ,SIGRS(2,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('D_X'    ,SIGRS(3,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('FEP_X'  ,SIGRS(4,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('DPL_X+' ,SIGRS(5,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('DPL_X-' ,SIGRS(6,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    CALL HM_GET_FLOAT_ARRAY_INDEX('F_Y'    ,SIGRS(7,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('D_Y'    ,SIGRS(8,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('FEP_Y'  ,SIGRS(9,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('DPL_Y+' ,SIGRS(10,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('DPL_Y-' ,SIGRS(11,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    CALL HM_GET_FLOAT_ARRAY_INDEX('F_Z'    ,SIGRS(12,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('D_Z'    ,SIGRS(13,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('FEP_Z'  ,SIGRS(14,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('DPL_Z+' ,SIGRS(15,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('DPL_Z-' ,SIGRS(16,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    CALL HM_GET_FLOAT_ARRAY_INDEX('M_X'    ,SIGRS(17,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('R_X'    ,SIGRS(18,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('MEP_X'  ,SIGRS(19,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('RPL_X+' ,SIGRS(20,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('RPL_X-' ,SIGRS(21,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    CALL HM_GET_FLOAT_ARRAY_INDEX('M_Y'    ,SIGRS(22,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('R_Y'    ,SIGRS(23,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('MEP_Y'  ,SIGRS(24,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('RPL_Y+' ,SIGRS(25,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('RPL_Y-' ,SIGRS(26,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    CALL HM_GET_FLOAT_ARRAY_INDEX('M_Z'    ,SIGRS(27,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('R_Z'    ,SIGRS(28,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('MEP_Z'  ,SIGRS(29,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('RPL_Z+' ,SIGRS(30,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('RPL_Z-' ,SIGRS(31,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    CALL HM_GET_FLOAT_ARRAY_INDEX('L_X'    ,SIGRS(32,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('L_Y'    ,SIGRS(33,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('L_Z'    ,SIGRS(34,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('EI'     ,SIGRS(35,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('ED_X'   ,SIGRS(36,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    CALL HM_GET_FLOAT_ARRAY_INDEX('ED_Y'   ,SIGRS(37,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('ED_Z'   ,SIGRS(38,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('ER_X'   ,SIGRS(39,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('ER_Y'   ,SIGRS(40,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('ER_Z'   ,SIGRS(41,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
C------
                  ELSEIF (IGTYP == 26) THEN
C------
                    CALL HM_GET_FLOAT_ARRAY_INDEX('F_X'   ,SIGRS(2,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('D_X'   ,SIGRS(3,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('FEP_X' ,SIGRS(4,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('L_X'   ,SIGRS(7,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('EI'    ,SIGRS(8,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('DV'    ,SIGRS(9,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
C------
C   user springs
                  ELSEIF (IGTYP == 29 .OR. IGTYP == 30 .OR. IGTYP == 31 .OR.
     .                    IGTYP == 32 .OR. IGTYP == 33 .OR. IGTYP == 35 .OR.
     .                    IGTYP == 36 .OR. IGTYP == 44 .OR. IGTYP == 45 .OR.
     .                    IGTYP == 46) THEN
C------!
                    CALL HM_GET_FLOAT_ARRAY_INDEX('F_X'   ,SIGRS(2,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('D_X'   ,SIGRS(3,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('F_Y'   ,SIGRS(4,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('D_Y'   ,SIGRS(5,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('F_Z'   ,SIGRS(6,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('D_Z'   ,SIGRS(7,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('M_X'   ,SIGRS(8,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('R_X'   ,SIGRS(9,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('M_Y'   ,SIGRS(10,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('R_Y'   ,SIGRS(11,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('M_Z'   ,SIGRS(12,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('R_Z'   ,SIGRS(13,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                    CALL HM_GET_FLOAT_ARRAY_INDEX('EI'    ,SIGRS(14,I),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    PT = 14
!
!                   -- UVAR --
!
                    SIZE = NUVAR
                    CALL HM_GET_FLOAT_ARRAY('VR'  ,TMPVAL,SIZE,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                    DO K=1,NUVAR
                      SIGBEAM(PT+K,I) = TMPVAL(K)
                    ENDDO ! DO K=1,NUVAR
                    PT = PT + NUVAR
!
                  ENDIF ! IF (IGTYP == 4  .OR. IGTYP == 12)
!
                ENDIF ! IF (IE == 0)
!
              ENDDO ! DO J=1,NB_ELEMENTS
!
            CASE DEFAULT
!
          END SELECT ! SELECT CASE(KEY)
!
        ENDDO ! DO INI=1,NB_INISPRI

      ENDIF ! IF ( NB_INISPRI > 0 )
!
      NISPRING = I



!-----------------------------------------
!            ---  /INIQUA  ---
!-----------------------------------------
      NIQUAD = 0
      I = 0
!
      CALL HM_OPTION_COUNT('/INIQUA', NB_INIQUA)
!
      IF ( NB_INIQUA > 0 ) THEN
!
        ! Start reading /INIQUA card 
        CALL HM_OPTION_START('/INIQUA')
!---
! to be replaced by ---  MAP_TABLES%IQUADM ---
           IF (KTRIELTQUAD == 0) THEN
C             tri des elts du D00 par ID croissant (on ne trie qu'une fois)
              DO IE = 1, NUMELQ
                ITRIQ(IE) = IXQ(NIXQ,IE)
              END DO
              CALL MY_ORDERS(0,WORK,ITRIQ,INDEXQ,NUMELQ,1)
              DO J = 1, NUMELQ
                IE=INDEXQ(J)
                KSYSUSRQ(J) = IXQ(NIXQ,IE)
                KSYSUSRQ(NUMELQ+J)=IE
              END DO
              KTRIELTQUAD=1
           ENDIF
!---
        DO INI=1,NB_INIQUA
!
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            UNIT_ID  = UID,
     .                            SUBMODEL_INDEX = SUB_INDEX,
     .                            SUBMODEL_ID = SUB_ID,
     .                            KEYWORD2 = KEY)
!
           IFLAGUNIT = 0
           DO IUNIT=1,UNITAB%NUNITS
             IF (UNITAB%UNIT_ID(IUNIT) == UID) THEN    
               IFLAGUNIT = 1
               EXIT
             ENDIF
           ENDDO
           IF (UID/=0.AND.IFLAGUNIT == 0) THEN
             CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                   I2=UID,I1=SUB_ID,C1='INIQUA',
     .                   C2='INIQUA',
     .                   C3=' ')
           ENDIF
c---------------------------------------
          SELECT CASE (KEY(1:LEN_TRIM(KEY)))
C---------
            CASE ( 'DENS' )
C---------
!
              CALL HM_GET_INTV('no_blocks',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                ! Reading  ---  ID_ELEM, ...  ---
                CALL HM_GET_INT_ARRAY_INDEX('quad_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
!
!                ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%IQUADM,NUMELQ)
!                IE = MAP_TABLES%IQUADM(ELT,2)
!
!!                IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELQ)
                IE=UEL2SYS(ID_ELEM,KSYSUSRQ,NUMELQ)
!
                IF (IE == 0) THEN        
                  ! Shell was not found. Issue a Warning & Skip.
                  NONEXIST = NONEXIST+1
                ELSE
                  CALL HM_GET_FLOAT_ARRAY_INDEX('value',DENS,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                  I=I+1
                  ID_QUAD_SIGI(I)  = ID_ELEM
                  SIGI(8,I) = DENS
                ENDIF ! IF (IE == 0)
              ENDDO ! DO J=1,NB_ELEMENTS
C---------
            CASE ( 'ENER' )
C---------
!
              CALL HM_GET_INTV('no_blocks',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                ! Reading  ---  ID_ELEM, ...  ---
                CALL HM_GET_INT_ARRAY_INDEX('quad_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
!
!                ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%IQUADM,NUMELQ)
!                IE = MAP_TABLES%IQUADM(ELT,2)
!
!!                IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELQ)
                IE=UEL2SYS(ID_ELEM,KSYSUSRQ,NUMELQ)
!
                IF (IE == 0) THEN        
                  ! Shell was not found. Issue a Warning & Skip.
                  NONEXIST = NONEXIST+1
                ELSE
                  CALL HM_GET_FLOAT_ARRAY_INDEX('value',ENER,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                  I=I+1
                  ID_QUAD_SIGI(I)  = ID_ELEM
                  SIGI(9,I) = ENER
                ENDIF ! IF (IE == 0)
              ENDDO ! DO J=1,NB_ELEMENTS
C---------
            CASE ( 'EPSP' )
C---------
!
              CALL HM_GET_INTV('no_blocks',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                ! Reading  ---  ID_ELEM, ...  ---
                CALL HM_GET_INT_ARRAY_INDEX('quad_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
!
!                ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%IQUADM,NUMELQ)
!                IE = MAP_TABLES%IQUADM(ELT,2)
!
                IE=UEL2SYS(ID_ELEM,KSYSUSRQ,NUMELQ)
!
                IF (IE == 0) THEN        
                  ! Shell was not found. Issue a Warning & Skip.
                  NONEXIST = NONEXIST+1
                ELSE
                  CALL HM_GET_FLOAT_ARRAY_INDEX('value',EPSP,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                  I=I+1
                  ID_QUAD_SIGI(I)  = ID_ELEM
                  SIGI(10,I) = EPSP
                ENDIF ! IF (IE == 0)
              ENDDO ! DO J=1,NB_ELEMENTS
C---------
            CASE ( 'STRESS' )
C---------

              CALL HM_GET_INTV('no_blocks',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                ! Reading  ---  ID_ELEM, ...  ---

                CALL HM_GET_INT_ARRAY_INDEX('quad_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
!
!                ELT = SET_USRTOS(ID_ELEM,MAP_TABLES%IQUADM,NUMELQ)
!                IE = MAP_TABLES%IQUADM(ELT,2)
!
                IE=UEL2SYS(ID_ELEM,KSYSUSRQ,NUMELQ)
!
                IF (IE == 0) THEN        
                  ! Shell was not found. Issue a Warning & Skip.
                  NONEXIST = NONEXIST+1
                ELSE
                  CALL HM_GET_FLOAT_ARRAY_INDEX('SIGMA_x' ,S(1),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                  CALL HM_GET_FLOAT_ARRAY_INDEX('SIGMA_y' ,S(2),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                  CALL HM_GET_FLOAT_ARRAY_INDEX('SIGMA_z' ,S(3),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
                  CALL HM_GET_FLOAT_ARRAY_INDEX('SIGMA_xy',S(4),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                  I=I+1
                  ID_QUAD_SIGI(I)  = ID_ELEM
                  DO K=1,4
                    SIGI(K,I) = S(K)
                  ENDDO
                ENDIF ! IF (IE == 0)
              ENDDO ! DO J=1,NB_ELEMENTS
!
            CASE DEFAULT
!
          END SELECT ! SELECT CASE(KEY)
!
!
        ENDDO ! DO INI=1,NB_INIQUA
!
      ENDIF ! IF ( NB_INIQUA > 0 )
!
      NIQUAD = I
!     message in case some elements was not found in the model
      IF (NONEXIST  > 0) THEN
           CALL ANCMSG(MSGID=3045,ANMODE=ANINFO,MSGTYPE=MSGWARNING,I1=NONEXIST)
      ENDIF ! IF (NONEXIST  > 0) 
C------------------------------------
      DEALLOCATE (ITRIS)
      DEALLOCATE (INDEXS)
      DEALLOCATE (KSYSUSRS)
      DEALLOCATE (KSYSUSRTG)
      DEALLOCATE (ITRIQ)
      DEALLOCATE (INDEXQ)
      DEALLOCATE (KSYSUSRQ)
      DEALLOCATE (IES2IPARG)
C------------------------------------
      RETURN
C
      END

