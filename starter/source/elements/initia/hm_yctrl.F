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
Chd|  HM_YCTRL                      source/elements/initia/hm_yctrl.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        UEL2SYS                       source/initial_conditions/inista/yctrl.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F
Chd|====================================================================
      SUBROUTINE HM_YCTRL(UNITAB,LSUBMODEL,IGRBRIC,IXC, IXTG,PTSHEL,PTSH3N)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE GROUPDEF_MOD
        USE UNITAB_MOD
        USE MESSAGE_MOD
        USE SUBMODEL_MOD
        USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scry_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
        TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
!
        TYPE(SUBMODEL_DATA) LSUBMODEL(*)
        INTEGER   IXTG(NIXTG,NUMELTG) ,IXC(NIXC,NUMELC)
        INTEGER,   INTENT(INOUT) :: PTSHEL(NUMELC),PTSH3N(NUMELTG)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER
     .        I,J,NGAUSS,NLAYER ,
     .        NUMS,NIP,NUVAR,JJHBE,J1,NU,IP,N,NPSOLID,
     .        K,IHBE,NPG,ND,NVAR_SHELL,NPT,NE,
     .        NVSHELL0,NUSHELL0,NORTSHEL0,NUSOLID0,NELS,KK,JJ,
     .        ISOLNOD,ISOLID,IFRAM,IORTH,IREP,IGTYP,ISH3N,NDIR,NLAYERS,
     .        UID,SUB_ID,NLAY,NPTR,NPTS,NPTT,IFAIL,IRUPT_TYP,NVAR_RUPT,
     .        ILAY,IMAT,NPT_MAX,NUBEAM0,NVSH_STRA,PROP,NSROT
        INTEGER IGBR, JGBR, IOK
        CHARACTER MESS*40,KEY2*ncharkey,KEY3*ncharkey
C-----------------------------------------------
        LOGICAL IS_AVAILABLE,GLOB
        CHARACTER KEY*ncharkey
        INTEGER  ID_ELEM,NB_INIBRI,NB_INISHE,NB_INISH3,NB_ELEMENTS,
     .        NB_INITRUSS,NB_INIBEAM,NB_INISPRI,NB_INIQUA,IE,KTRIELC,
     .        KTRIELTG,NELT
        my_real
     .     THK
        INTEGER, DIMENSION(:), ALLOCATABLE :: KSYSUSRTG ,KSYSUSR,WORK,ITRI,
     .                                        INDEX
C-----------------------------------------------
        EXTERNAL UEL2SYS
        INTEGER  UEL2SYS
C=======================================================================
!      NFILSOL=0
!      NUMSOL =0
!      NUMQUAD=0
!      NUMSHEL=0
!      NUMTRUS=0
!      NUMBEAM=0
!      NUMSPRI=0
!      NUMSH3N=0
        NVSHELL0   = 33
        NUSHELL0   = 4
        NORTSHEL0  = 5
        NVAR_SHELL = 0
        NUBEAM0 = 4
!      NUBEAM  = 0
!      NVBEAM  = 0
!      NVTRUSS = 0
!!      NVSPRI = 0
        NVSH_STRA =0
!
!       IUFACYLD = 0
!       IUSHELL = 0
!       NUSHELL = 0
!       NVSHELL1 = 0
!       NVSHELL2 = 0
!cc       NGAUSS = 0
!cc       NLAYER = 0
!cc       NVSHELL = 0
!       IUSOLID = 0
!       NUSOLID = 0
!       NVSOLID1 = 0
!       NVSOLID2 = 0
!       NVSOLID3 = 0
!       NVSOLID4 = 0
!       NVSOLID5 = 0
!cc       NPSOLID = 0
!cc       NVSOLID = 0
        ALLOCATE (KSYSUSR(2*NUMELC))
        ALLOCATE (KSYSUSRTG(2*NUMELTG))
        ALLOCATE (WORK(70000))
        NELT = MAX(NUMELC, NUMELTG)
        ALLOCATE(ITRI(NELT),INDEX(2*NELT))
        KTRIELC = 0
        KTRIELTG = 0
        KSYSUSR = 0
        KSYSUSRTG = 0
        WORK = 0
        ITRI = 0
        INDEX= 0
!-----------------------------------------

C-----------------------------------------
C     CONTRAINTES INITIALES FICHIER D00
C-----------------------------------------
        IS_AVAILABLE = .FALSE.
        GLOB         = .FALSE.
!
        IF (ISIGI==-3.OR.ISIGI==-4.OR.ISIGI==-5) THEN


C------------------------------------
C /INIBRI card
C------------------------------------
          CALL HM_OPTION_COUNT('/INIBRI', NB_INIBRI)
          IF ( NB_INIBRI > 0 ) THEN
            ! Start reading /INIBRI card
            CALL HM_OPTION_START('/INIBRI')
!
            DO I=1,NB_INIBRI
!
              CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                                KEYWORD2 = KEY)
!
              SELECT CASE (KEY(1:LEN_TRIM(KEY)))
!-------------------
               CASE ( 'FILL' )
!-------------------
                CALL HM_GET_INTV('inibri_fill_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
                NUMSOL = NUMSOL + NB_ELEMENTS
                NFILSOL = 1
!-------------------
               CASE ( 'EPSP' )
!-------------------
                CALL HM_GET_INTV('inibri_epsp_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
                NUMSOL = NUMSOL + NB_ELEMENTS
!-------------------
               CASE ( 'ENER' )
!-------------------
                CALL HM_GET_INTV('inibri_ener_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
                NUMSOL = NUMSOL + NB_ELEMENTS
!-------------------
               CASE ( 'DENS' )
!-------------------
                CALL HM_GET_INTV('inibri_dens_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
                NUMSOL = NUMSOL + NB_ELEMENTS
!-------------------
               CASE ( 'STRESS' )
!-------------------
                CALL HM_GET_INTV('inibri_stress_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
                NUMSOL = NUMSOL + NB_ELEMENTS
!-------------------
               CASE ( 'AUX' )
!-------------------
                CALL HM_GET_INTV('inibri_aux_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
                NUMSOL = NUMSOL + NB_ELEMENTS
!
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('brick_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Nb_integr',NPT,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Isolnod',ISOLNOD,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Isolid',JJHBE,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nvars',NUVAR,J,IS_AVAILABLE,LSUBMODEL)
!
                  IUSOLID = 1
                  NUSOLID = MAX(NUSOLID,NPT*NUVAR)
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
               CASE ( 'STRS_F' )
!-------------------
                CALL HM_GET_INTV('inibri_strs_f_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('Nb_integr',NPT,J,IS_AVAILABLE,LSUBMODEL)
!
                  NUMSOL = NUMSOL + 1
                  NVSOLID1 = MAX (NVSOLID1,NPT*9 + 4)
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
               CASE ( 'STRS_FGLO' )
!-------------------
                CALL HM_GET_INTV('inibri_strs_fglo_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('Nb_integr',NPT,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('grbric_ID',IGBR,J,IS_AVAILABLE,LSUBMODEL)
!
                  IF (IGBR > 0) THEN
                    IOK  = 0
                    JGBR = 0
                    DO K=1,NGRBRIC
                      IF (IGBR == IGRBRIC(K)%ID) THEN
                        JGBR = K
                        IOK  = 1
                        EXIT
                      ENDIF
                    ENDDO
                    IF (IOK == 0) THEN
                      CALL ANCMSG(MSGID=1611,MSGTYPE=MSGERROR,ANMODE=ANINFO,C1='STRS_FGLO',I1=IGBR)
                    ENDIF
                    NUMSOL = NUMSOL + IGRBRIC(JGBR)%NENTITY
                  ELSE
                    NUMSOL = NUMSOL + 1
                  ENDIF
                  NVSOLID1 = MAX (NVSOLID1,NPT*9 + 4)
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
               CASE ( 'STRA_F' )
!-------------------
                CALL HM_GET_INTV('inibri_stra_f_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                NUMSOL = NUMSOL + NB_ELEMENTS
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('Nb_integr',NPT,J,IS_AVAILABLE,LSUBMODEL)
                  NVSOLID2 = MAX(NVSOLID2, MAX(1,NPT)*6)
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
               CASE ( 'STRA_FGLO' )
!-------------------
                CALL HM_GET_INTV('inibri_stra_fglo_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                NUMSOL = NUMSOL + NB_ELEMENTS
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('Nb_integr',NPT,J,IS_AVAILABLE,LSUBMODEL)
                  NVSOLID2 = MAX(NVSOLID2, MAX(1,NPT)*6)
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
               CASE ( 'FAIL' )
!-------------------
                CALL HM_GET_INTV('inibri_fail_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                NUMSOL = NUMSOL + NB_ELEMENTS
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('Nlay',NLAY,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nptr',NPTR,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npts',NPTS,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nptt',NPTT,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Nvar',NVAR_RUPT,J,IS_AVAILABLE,LSUBMODEL)
                  NVSOLID4 = MAX(NVSOLID4,NPTR*NPTS*NPTT*NLAY*5*NVAR_RUPT)
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
               CASE ( 'SCALE_YLD' )
!-------------------
                CALL HM_GET_INTV('inibri_scale_yld_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                IUFACYLD = 1
                NUMSOL = NUMSOL + NB_ELEMENTS
!
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('nptr',NPTR,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npts',NPTS,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nptt',NPTT,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nlay',NLAY,J,IS_AVAILABLE,LSUBMODEL)
                  NVSOLID5 = MAX(NVSOLID5,NPTR*NPTS*NPTT*NLAY + 7)
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
               CASE ( 'ORTHO' )
!-------------------
                CALL HM_GET_INTV('inibri_ortho_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                NUMSOL = NUMSOL + NB_ELEMENTS
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('Nb_layer',NLAYERS,J,IS_AVAILABLE,LSUBMODEL)
                  NVSOLID3 = MAX(NVSOLID3,NLAYERS * 6)
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
               CASE ( 'EREF' )
!-------------------
                CALL HM_GET_INTV('inibri_eref_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                NUMSOL = NUMSOL + NB_ELEMENTS
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('Isolnod',ISOLNOD,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Nsrot',NSROT,J,IS_AVAILABLE,LSUBMODEL)
C------ use NVSOLID5 temporarily, read directly after new reader or add NVSOLID6
                  NVSOLID6 = MAX(NVSOLID6, (ISOLNOD+NSROT)*3)
                ENDDO ! DO J=1,NB_ELEMENTS
!
               CASE DEFAULT
!
              END SELECT ! SELECT CASE(KEY)
!
            ENDDO ! DO I=1,NB_INIBRI
          ENDIF ! IF ( NB_INIBRI > 0 )

C------------------------------------
C /INISHE card
C------------------------------------

          NPT = 0
!
          CALL HM_OPTION_COUNT('/INISHE', NB_INISHE)
          IF ( NB_INISHE > 0 ) THEN

            IF (KTRIELC == 0) THEN
C             tri des elts du D00 par ID croissant (on ne trie qu'une fois)
              DO IE = 1, NUMELC
                ITRI(IE) = IXC(NIXC,IE)
              END DO
              CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMELC,1)
              DO J = 1, NUMELC
                IE=INDEX(J)
                KSYSUSR(J)   =IXC(NIXC,IE)
                KSYSUSR(NUMELC+J)=IE
              END DO
              KTRIELC=1
            ENDIF
!!
            ! Start reading /INISHE card
            CALL HM_OPTION_START('/INISHE')
!
            NUMSHEL = 0
            DO I=1,NB_INISHE
!
              CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                                KEYWORD2 = KEY,
     .                                KEYWORD3 = KEY2)
!
              IF (KEY2 /= ' ') GLOB = .TRUE.
!
              SELECT CASE (KEY(1:LEN_TRIM(KEY)))

!-------------------
               CASE ( 'EPSP_F' )
!-------------------
                CALL HM_GET_INTV('inishe_epsp_f_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                DO J=1,NB_ELEMENTS
                  ! Reading  ---  ID_ELEM, NIP, NPG, THK  ---
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
!
                  IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
                  IF(IE == 0 ) CYCLE
                  IF(IE > 0 .AND. PTSHEL(IE)== 0 ) THEN
                    NUMSHEL = NUMSHEL + 1
                    PTSHEL(IE) = NUMSHEL
                  ENDIF
                  IF (NIP == 0) THEN
                    NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*9)
                  ELSE
                    NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*MAX(1,NPG)*6)
                  ENDIF
!
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
               CASE ( 'STRS_F' )
!-------------------
                IF ( GLOB ) THEN
                  CALL HM_GET_INTV('inishe_strs_f_glob_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)

!
                  DO J=1,NB_ELEMENTS
                    ! Reading  ---  ID_ELEM, NIP, NPG, THK  ---
                    CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                    CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                    CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                    !
                    IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
                    IF(IE == 0 ) CYCLE
                    IF(IE > 0 .AND. PTSHEL(IE)== 0 ) THEN
                      NUMSHEL = NUMSHEL + 1
                      PTSHEL(IE) = NUMSHEL
                    ENDIF
                    IF (NIP == 0) THEN
                      NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*13)
                    ELSE
                      NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*MAX(1,NPG)*8)
                    ENDIF
!
                  ENDDO ! DO J=1,NB_ELEMENTS
!
                ELSEIF ( .NOT. GLOB ) THEN
!
                  CALL HM_GET_INTV('inishe_strs_f_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                  DO J=1,NB_ELEMENTS
                    ! Reading  CARD_1 ---  ID_ELEM, NIP, NPG, THK  ---
                    CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                    CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                    CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                    IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
!

                    IF(IE == 0 ) CYCLE
                    IF(IE > 0 .AND. PTSHEL(IE)== 0 ) THEN
                      NUMSHEL = NUMSHEL + 1
                      PTSHEL(IE) = NUMSHEL
                    ENDIF
                    IF (NIP == 0) THEN
                      NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*9)
                    ELSE
                      NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*MAX(1,NPG)*6)
                    ENDIF
!
                  ENDDO ! DO J=1,NB_ELEMENTS
!
                ENDIF ! IF ( GLOB )
!-------------------
               CASE ( 'STRA_F' )
!-------------------
                IF ( GLOB ) THEN
                  CALL HM_GET_INTV('inishe_stra_f_glob_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                  DO J=1,NB_ELEMENTS
                    CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                    CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                    CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                    IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
!
                    IF (NIP==0) NIP=2
                    IF(IE == 0 ) CYCLE
                    IF(IE > 0 .AND. PTSHEL(IE)== 0 ) THEN
                      NUMSHEL = NUMSHEL + 1
                      PTSHEL(IE) = NUMSHEL
                    ENDIF ! IE > 0
C---------store only up to 2 pts of NIP eij(6)+T, pointer= INISHVAR1
                    NVSH_STRA = MAX(NVSH_STRA,2*MAX(1,NPG)*7)
!!                    NVSH_STRA = MAX(NVSH_STRA,NIP*MAX(1,NPG)*7)
                  ENDDO ! DO J=1,NB_ELEMENTS
!
                ELSEIF ( .NOT. GLOB ) THEN

                  CALL HM_GET_INTV('inishe_stra_f_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                  DO J=1,NB_ELEMENTS
                    CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                    CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                    IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
!
                    IF(IE == 0 ) CYCLE
                    NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*8)
                    IF(IE > 0 .AND. PTSHEL(IE)== 0 ) THEN
                      NUMSHEL = NUMSHEL + 1
                      PTSHEL(IE) = NUMSHEL
                    ENDIF ! IE > 0
                  ENDDO ! DO J=1,NB_ELEMENTS
                ENDIF ! IF ( GLOB )
!-------------------
               CASE ( 'THICK' )
!-------------------
                CALL HM_GET_INTV('no_elems',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
                  IF(IE == 0 ) CYCLE
                  IF(IE > 0 .AND. PTSHEL(IE)== 0 ) THEN
                    NUMSHEL = NUMSHEL + 1
                    PTSHEL(IE) = NUMSHEL
                  ENDIF
                ENDDO
!
!-------------------
               CASE ( 'EPSP' )
!-------------------
                CALL HM_GET_INTV('no_blocks',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
                  IF(IE == 0 ) CYCLE
                  IF(IE > 0 .AND. PTSHEL(IE)== 0 ) THEN
                    NUMSHEL = NUMSHEL + 1
                    PTSHEL(IE) = NUMSHEL
                  ENDIF
                ENDDO
!-------------------
               CASE ( 'ORTHO' )
!-------------------
                CALL HM_GET_INTV('inishe_ortho_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
                  IF(IE == 0 ) CYCLE
                  IF(IE > 0 .AND. PTSHEL(IE)== 0 ) THEN
                    NUMSHEL = NUMSHEL + 1
                    PTSHEL(IE) = NUMSHEL
                  ENDIF
                ENDDO
!
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
                  IF(IE == 0 ) CYCLE
                  IF(IE > 0 .AND. PTSHEL(IE)== 0 ) THEN
                    NUMSHEL = NUMSHEL + 1
                    PTSHEL(IE) = NUMSHEL
                  ENDIF
                  IF (NIP==0) THEN
                    NVAR_SHELL = MAX(NVAR_SHELL, 9)
                  ELSE
                    NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*24)
                  ENDIF
                  IORTSHEL = 1
                  NORTSHEL = MAX(NORTSHEL, NORTSHEL0 + MAX(1,NIP)*2)
                  NPT = MAX(1,NIP)
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
               CASE ( 'ORTH_LOC' )
!-------------------

                CALL HM_GET_INTV('inishe_orth_loc_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('nb_lay',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('ndir',NDIR,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  !
                  IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
                  IF(IE == 0 ) CYCLE
                  IF(IE > 0 .AND. PTSHEL(IE)== 0 ) THEN
                    NUMSHEL = NUMSHEL + 1
                    PTSHEL(IE) = NUMSHEL
                  ENDIF
                  IF (NIP==0) THEN
                    NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*9)
                  ELSE
                    NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*24)
                  ENDIF
                  IORTSHEL = 2
                  NORTSHEL = MAX(NORTSHEL, NORTSHEL0 + MAX(1,NIP)*2)
                  IF (NDIR == 2) NORTSHEL = MAX(NORTSHEL, NORTSHEL0 + MAX(1,NIP)*4)
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
               CASE ( 'SCALE_YLD' )
!-------------------
                CALL HM_GET_INTV('inishe_scale_yld_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
                IUFACYLD = 1
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  !!
                  IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
                  IF(IE == 0 ) CYCLE
                  IF(IE > 0 .AND. PTSHEL(IE)== 0 ) THEN
                    NUMSHEL = NUMSHEL + 1
                    PTSHEL(IE) = NUMSHEL
                  ENDIF
                  IF (NIP==0) THEN
                    NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*9)
                  ELSE
                    NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*MAX(1,NPG)*6)
                  ENDIF
                  NVSHELL2 = MAX(NVSHELL2,MAX(1,NPG)*MAX(1,NIP))
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
               CASE ( 'AUX' )
!-------------------
                CALL HM_GET_INTV('inishe_aux_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
                IUSHELL = 1
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nvars',NUVAR,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  !!
                  IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
                  IF(IE == 0 ) CYCLE
                  IF(IE > 0 .AND. PTSHEL(IE)== 0 ) THEN
                    NUMSHEL = NUMSHEL + 1
                    PTSHEL(IE) = NUMSHEL
                  ENDIF ! IE > 0
                  IF (NIP==0) THEN
                    NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*9)
                  ELSE
                    NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*24)
                  ENDIF
                  NUSHELL = MAX(NUSHELL,NUSHELL0+MAX(1,NPG)*MAX(1,NIP)*NUVAR)
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
               CASE ( 'FAIL' )
!-------------------
                CALL HM_GET_INTV('inishe_fail_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('Nlay',NLAY,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nptt',NPTT,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('lay_ID',ILAY,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Nvar',NVAR_RUPT,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  !!
                  IE=UEL2SYS(ID_ELEM,KSYSUSR,NUMELC)
                  IF(IE == 0 ) CYCLE
                  IF(IE > 0 .AND. PTSHEL(IE)== 0 ) THEN
                    NUMSHEL = NUMSHEL + 1
                    PTSHEL(IE) = NUMSHEL
                  ENDIF ! IE > 0
                  NPG = MAX(1,NPG)
                  NPTT = MAX(1,NPTT)
                  NLAY = MAX(1,NLAY)
                  NPT_MAX = MAX(NPTT,NLAY)
                  NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NLAY)*24)
                  NVSHELL1 = MAX(NVSHELL1,NPG*NPT_MAX*5*NVAR_RUPT)
                ENDDO ! DO J=1,NB_ELEMENTS
!
               CASE DEFAULT
!
              END SELECT ! SELECT CASE(KEY)

            ENDDO ! DO I=1,NB_INISHE
          ENDIF ! IF ( NB_INISHE > 0 )


C------------------------------------
C /INISH3 card
C------------------------------------
          CALL HM_OPTION_COUNT('/INISH3', NB_INISH3)
          IF ( NB_INISH3 > 0 ) THEN
            !!
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
            ! Start reading /INISH3 card
            CALL HM_OPTION_START('/INISH3')
!
            NUMSH3N = 0
            DO I=1,NB_INISH3
!
              CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                                KEYWORD2 = KEY,
     .                                KEYWORD3 = KEY2)
!
              IF (KEY2 /= ' ') GLOB = .TRUE.
!
              SELECT CASE (KEY(1:LEN_TRIM(KEY)))
!-------------------
               CASE ( 'EPSP_F' )
!-------------------
                CALL HM_GET_INTV('inish3_epsp_f_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!

                DO J=1,NB_ELEMENTS
                  ! Reading  ---  ID_ELEM, NIP, NPG, THK  ---
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  IE = UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
                  IF(IE == 0 ) CYCLE
                  IF(IE > 0 .AND. PTSH3N(IE) == 0) THEN
                    NUMSH3N = NUMSH3N + 1
                    PTSH3N(IE) = NUMSH3N
                  ENDIF
!
                  IF (NIP == 0) THEN
                    NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*9)
                  ELSE
                    NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*MAX(1,NPG)*6)
                  ENDIF
!
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
               CASE ( 'STRS_F' )
!-------------------
                IF ( GLOB ) THEN
!
                  CALL HM_GET_INTV('inish3_strs_f_glob_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                  DO J=1,NB_ELEMENTS
                    ! Reading  ---  ID_ELEM, NIP, NPG, THK  ---
                    CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                    CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                    CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                    IE = UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
                    IF(IE == 0 ) CYCLE
                    IF(IE > 0 .AND. PTSH3N(IE) == 0) THEN
                      NUMSH3N = NUMSH3N + 1
                      PTSH3N(IE) = NUMSH3N
                    ENDIF
!
                    IF (NIP == 0) THEN
                      NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*13)
                    ELSE
                      NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*MAX(1,NPG)*8)
                    ENDIF
!
                  ENDDO ! DO J=1,NB_ELEMENTS
!
!!                  CASE ( 'STRS_F' )
                ELSEIF ( .NOT. GLOB ) THEN
!
                  CALL HM_GET_INTV('inish3_strs_f_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                  DO J=1,NB_ELEMENTS
                    ! Reading  CARD_1 ---  ID_ELEM, NIP, NPG, THK  ---
                    CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                    CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                    CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                    IE = UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
                    IF(IE == 0 ) CYCLE
                    IF(IE > 0 .AND. PTSH3N(IE) == 0) THEN
                      NUMSH3N = NUMSH3N + 1
                      PTSH3N(IE) = NUMSH3N
                    ENDIF
!
                    IF (NIP == 0) THEN
                      NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*9)
                    ELSE
                      NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*MAX(1,NPG)*6)
                    ENDIF
!
                  ENDDO ! DO J=1,NB_ELEMENTS
!
                ENDIF ! IF ( GLOB )
!-------------------
               CASE ( 'STRA_F' )
!-------------------
                IF ( GLOB ) THEN
                  CALL HM_GET_INTV('inish3_stra_f_glob_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                  DO J=1,NB_ELEMENTS
                    CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                    CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                    CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                    IE = UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
                    IF(IE == 0 ) CYCLE
                    IF(IE > 0 .AND. PTSH3N(IE) == 0) THEN
                      NUMSH3N = NUMSH3N + 1
                      PTSH3N(IE) = NUMSH3N
                    ENDIF
!
                    IF (NIP==0) NIP=2
C---------store only up to 2 pts of NIP eij(6)+T, pointer= INISHVAR1
                    NVSH_STRA = MAX(NVSH_STRA,2*MAX(1,NPG)*7)
!!                    NVSH_STRA = MAX(NVSH_STRA,NIP*MAX(1,NPG)*7)
                  ENDDO ! DO J=1,NB_ELEMENTS
!
                ELSEIF ( .NOT. GLOB ) THEN

                  CALL HM_GET_INTV('inish3_stra_f_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                  DO J=1,NB_ELEMENTS
                    CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                    CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                    IE = UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
                    IF(IE == 0 ) CYCLE
                    IF(IE > 0 .AND. PTSH3N(IE) == 0) THEN
                      NUMSH3N = NUMSH3N + 1
                      PTSH3N(IE) = NUMSH3N
                    ENDIF
!
                    NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*8)
                  ENDDO ! DO J=1,NB_ELEMENTS
                ENDIF ! IF ( GLOB )
!-------------------
               CASE ( 'THICK' )
!-------------------
                CALL HM_GET_INTV('no_elems',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  IE = UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
                  IF(IE == 0 ) CYCLE
                  IF(IE > 0 .AND. PTSH3N(IE) == 0) THEN
                    NUMSH3N = NUMSH3N + 1
                    PTSH3N(IE) = NUMSH3N
                  ENDIF
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
               CASE ( 'EPSP' )
!-------------------
                CALL HM_GET_INTV('no_blocks',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  IE = UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
                  IF(IE == 0 ) CYCLE
                  IF(IE > 0 .AND. PTSH3N(IE) == 0) THEN
                    NUMSH3N = NUMSH3N + 1
                    PTSH3N(IE) = NUMSH3N
                  ENDIF
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
               CASE ( 'ORTHO' )
!-------------------
                CALL HM_GET_INTV('inish3_ortho_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  IE = UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
                  IF(IE == 0 ) CYCLE
                  IF(IE > 0 .AND. PTSH3N(IE) == 0) THEN
                    NUMSH3N = NUMSH3N + 1
                    PTSH3N(IE) = NUMSH3N
                  ENDIF
!
                  IF (NIP==0) THEN
                    NVAR_SHELL = MAX(NVAR_SHELL, 9)
                  ELSE
                    NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*24)
                  ENDIF
                  IORTSHEL = 1
                  NORTSHEL = MAX(NORTSHEL, NORTSHEL0 + MAX(1,NIP)*2)
                  NPT = MAX(1,NIP)
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
               CASE ( 'ORTH_LOC' )
!-------------------
                CALL HM_GET_INTV('inish3_orth_loc_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
!
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('nb_lay',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('ndir',NDIR,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  IE = UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
                  IF(IE == 0 ) CYCLE
                  IF(IE > 0 .AND. PTSH3N(IE) == 0) THEN
                    NUMSH3N = NUMSH3N + 1
                    PTSH3N(IE) = NUMSH3N
                  ENDIF
!
                  IF (NIP==0) THEN
                    NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*9)
                  ELSE
                    NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*24)
                  ENDIF
                  IORTSHEL = 2
                  NORTSHEL = MAX(NORTSHEL, NORTSHEL0 + MAX(1,NIP)*2)
                  IF (NDIR == 2) NORTSHEL = MAX(NORTSHEL, NORTSHEL0 + MAX(1,NIP)*4)
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
               CASE ( 'SCALE_YLD' )
!-------------------
                CALL HM_GET_INTV('inish3_scale_yld_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
                IUFACYLD = 1
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  IE = UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
                  IF(IE == 0 ) CYCLE
                  IF(IE > 0 .AND. PTSH3N(IE) == 0) THEN
                    NUMSH3N = NUMSH3N + 1
                    PTSH3N(IE) = NUMSH3N
                  ENDIF
!
                  IF (NIP==0) THEN
                    NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*9)
                  ELSE
                    NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*MAX(1,NPG)*6)
                  ENDIF
                  NVSHELL2 = MAX(NVSHELL2,MAX(1,NPG)*MAX(1,NIP))
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
               CASE ( 'AUX' )
!-------------------
                CALL HM_GET_INTV('inish3_aux_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
                IUSHELL = 1
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nvars',NUVAR,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  IE = UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
                  IF(IE == 0 ) CYCLE
                  IF(IE > 0 .AND. PTSH3N(IE) == 0) THEN
                    NUMSH3N = NUMSH3N + 1
                    PTSH3N(IE) = NUMSH3N
                  ENDIF
!
                  IF (NIP==0) THEN
                    NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NPG)*9)
                  ELSE
                    NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NIP)*24)
                  ENDIF
                  NUSHELL = MAX(NUSHELL,NUSHELL0+MAX(1,NPG)*MAX(1,NIP)*NUVAR)
                ENDDO ! DO J=1,NB_ELEMENTS
!-------------------
               CASE ( 'FAIL' )
!-------------------
                CALL HM_GET_INTV('inish3_fail_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
                DO J=1,NB_ELEMENTS
                  CALL HM_GET_INT_ARRAY_INDEX('Nlay',NLAY,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('npg',NPG,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nptt',NPTT,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('lay_ID',ILAY,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('Nvar',NVAR_RUPT,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('shell_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                  IE = UEL2SYS(ID_ELEM,KSYSUSRTG,NUMELTG)
                  IF(IE == 0 ) CYCLE
                  IF(IE > 0 .AND. PTSH3N(IE) == 0) THEN
                    NUMSH3N = NUMSH3N + 1
                    PTSH3N(IE) = NUMSH3N
                  ENDIF
                  NPG = MAX(1,NPG)
                  NPTT = MAX(1,NPTT)
                  NLAY = MAX(1,NLAY)
                  NPT_MAX = MAX(NPTT,NLAY)
                  NVAR_SHELL = MAX(NVAR_SHELL, MAX(1,NLAY)*24)
                  NVSHELL1 = MAX(NVSHELL1,NPG*NPT_MAX*5*NVAR_RUPT)
                ENDDO ! DO J=1,NB_ELEMENTS
!
!
               CASE DEFAULT
!
              END SELECT ! SELECT CASE(KEY)

            ENDDO ! DO I=1,NB_INISH3
          ENDIF ! IF ( NB_INISH3 > 0 )
!---


          INISHVAR1 = NVAR_SHELL + NVSHELL0 + NPT
          NVSHELL = INISHVAR1 + NVSH_STRA


C------------------------------------
C /INITRUSS card
C------------------------------------
          CALL HM_OPTION_COUNT('/INITRUSS', NB_INITRUSS)

          IF ( NB_INITRUSS > 0 ) THEN
            ! Start reading /INITRUSS card
            CALL HM_OPTION_START('/INITRUSS')
!
            DO I=1,NB_INITRUSS
!
              CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                                KEYWORD2 = KEY)
!
              SELECT CASE (KEY(1:LEN_TRIM(KEY)))
!-------------------
               CASE ( 'FULL' )
!-------------------
                CALL HM_GET_INTV('no_of_elems',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                NUMTRUS = NUMTRUS + NB_ELEMENTS
!
                NVTRUSS = NVTRUSS + 6*NB_ELEMENTS
!
               CASE DEFAULT
!
              END SELECT ! SELECT CASE(KEY)

            ENDDO ! DO I=1,NB_INITRUSS
          ENDIF ! IF ( NB_INITRUSS > 0 )


C------------------------------------
C /INIBEAM card
C------------------------------------
          CALL HM_OPTION_COUNT('/INIBEAM', NB_INIBEAM)
!
          IF ( NB_INIBEAM > 0 ) THEN
            ! Start reading /INIBEAM card
            CALL HM_OPTION_START('/INIBEAM')
!
            DO I=1,NB_INIBEAM
!
              CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                                KEYWORD2 = KEY)

!
              SELECT CASE (KEY(1:LEN_TRIM(KEY)))
!
!-------------------
               CASE ( 'FULL' )
!-------------------
!
                CALL HM_GET_INTV('inibeam_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                NUMBEAM = NUMBEAM + NB_ELEMENTS
!
                DO J=1,NB_ELEMENTS
                  ! Reading  ---  ID_ELEM, Prop ...  ---
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('prop_type',IGTYP,J,IS_AVAILABLE,LSUBMODEL)
!
                  NVBEAM = NVBEAM + 11
                  IF (IGTYP == 3) THEN
                    NVBEAM = NVBEAM + 1
                  ELSEIF (IGTYP == 18) THEN
                    NVBEAM = NVBEAM + 4*NIP
                  ENDIF
                ENDDO ! DO J=1,NB_ELEMENTS
!
!-------------------
               CASE ( 'AUX' )
!-------------------
!
                CALL HM_GET_INTV('inibeam_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                NUMBEAM = NUMBEAM + NB_ELEMENTS
!
                DO J=1,NB_ELEMENTS
                  ! Reading  ---  ID_ELEM, Prop ...  ---
                  CALL HM_GET_INT_ARRAY_INDEX('nb_integr',NIP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('prop_type',IGTYP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nvars'    ,NUVAR,J,IS_AVAILABLE,LSUBMODEL)
!
                  IF (IGTYP == 18) THEN
                    NUBEAM = MAX(NUBEAM,NUBEAM0 + NIP*NUVAR)
                  ENDIF
                ENDDO ! DO J=1,NB_ELEMENTS
!
               CASE DEFAULT
!
              END SELECT ! SELECT CASE(KEY)

            ENDDO ! DO I=1,NB_INIBEAM
          ENDIF ! IF ( NB_INIBEAM > 0 )



C------------------------------------
C /INISPRI card
C------------------------------------
          CALL HM_OPTION_COUNT('/INISPRI', NB_INISPRI)
!
          IF ( NB_INISPRI > 0 ) THEN
            ! Start reading /INISPRI card
            CALL HM_OPTION_START('/INISPRI')
!
            DO I=1,NB_INISPRI
!
              CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                                KEYWORD2 = KEY)

!
              SELECT CASE (KEY(1:LEN_TRIM(KEY)))
!
!-------------------
               CASE ( 'FULL' )
!-------------------
!
                CALL HM_GET_INTV('size_spring',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
                NUMSPRI = NUMSPRI + NB_ELEMENTS
!
                DO J=1,NB_ELEMENTS
                  ! Reading  ---  ID_ELEM, Prop ...  ---
                  CALL HM_GET_INT_ARRAY_INDEX('prop_type',IGTYP,J,IS_AVAILABLE,LSUBMODEL)
                  CALL HM_GET_INT_ARRAY_INDEX('nvars'    ,NUVAR,J,IS_AVAILABLE,LSUBMODEL)
!
C------
                  IF (IGTYP == 4) THEN
C------
                    NVSPRI = NVSPRI + 10
C------
                  ELSEIF (IGTYP == 12) THEN
C------
                    NVSPRI = NVSPRI + 11
C------
                  ELSEIF (IGTYP == 26) THEN
C------
                    NVSPRI = NVSPRI + 9
C------
                  ELSEIF (IGTYP == 8  .OR. IGTYP == 13 .OR.
     .                    IGTYP == 23 .OR. IGTYP == 25) THEN
C------
                    NVSPRI = NVSPRI + 43
C------
C   user springs
                  ELSEIF (IGTYP == 29 .OR. IGTYP == 30 .OR. IGTYP == 31 .OR.
     .                    IGTYP == 32 .OR. IGTYP == 33 .OR. IGTYP == 35 .OR.
     .                    IGTYP == 36 .OR. IGTYP == 44 .OR. IGTYP == 45 .OR.
     .                    IGTYP == 46) THEN
C------
                    NVSPRI = NVSPRI + 16 + NUVAR
C------
                  ENDIF ! IF (IGTYP == 4)
!
                ENDDO ! DO J=1,NB_ELEMENTS
!
               CASE DEFAULT
!
              END SELECT ! SELECT CASE(KEY)

            ENDDO ! DO I=1,NB_INIBEAM
          ENDIF ! IF ( NB_INIBEAM > 0 )



C------------------------------------
C /INIQUA card
C------------------------------------
          CALL HM_OPTION_COUNT('/INIQUA', NB_INIQUA)
!
          IF ( NB_INIQUA > 0 ) THEN
            ! Start reading /INIQUA card
            CALL HM_OPTION_START('/INIQUA')
!
            DO I=1,NB_INIQUA
!
              CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                                KEYWORD2 = KEY)
!
              CALL HM_GET_INTV('no_blocks',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!

              NUMQUAD = NUMQUAD + NB_ELEMENTS
!
            ENDDO ! DO I=1,NB_INIQUA
!
          ENDIF ! IF ( NB_INIQUA > 0 )
!---
        ENDIF ! IF (ISIGI==-3.OR.ISIGI==-4.OR.ISIGI==-5)
!
        RETURN
 399    CONTINUE
        CALL ANCMSG(MSGID=557,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1)

        RETURN
      END
