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
Chd|  LECFILL                       source/elements/initia/lecfill.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        UEL2SYS                       source/initial_conditions/inista/yctrl.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE LECFILL(IXS,FILLSOL,UNITAB,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE UNITAB_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scry_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*)
      my_real
     .      FILLSOL(*)
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .      INI,J,N,ID_ELEM,IE,UID,SUB_ID,STAT,NB_INIBRI,
     .      IFLAGUNIT,IUNIT,NB_ELEMENTS
      INTEGER WORKS(70000)
      INTEGER, DIMENSION(:), ALLOCATABLE :: KSYSUSRS,INDEXS,ITRIS
      CHARACTER MESS*40,KEY*ncharkey
      my_real
     .      FILL
      LOGICAL IS_AVAILABLE
      INTEGER UEL2SYS
      EXTERNAL UEL2SYS
C=======================================================================
      DO N=1,NUMELS
        FILLSOL(N)=ONE
      END DO
C------------------------------------
      IS_AVAILABLE = .FALSE.
!-----------------------------------------
!       pre lecture of FILL for interfaces stiffness computation
!             ---  /INIBRI/FILL  ---
!-----------------------------------------
      CALL HM_OPTION_COUNT('/INIBRI', NB_INIBRI)
!
      IF ( NB_INIBRI > 0 ) THEN
!
        ! Start reading /INIBRI card 
        CALL HM_OPTION_START('/INIBRI')
!
        NFILSOL=0
        DO INI=1,NB_INIBRI
!
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            UNIT_ID  = UID,
     .                            SUBMODEL_ID = SUB_ID,
     .                            KEYWORD2 = KEY)
!
          SELECT CASE (KEY(1:LEN_TRIM(KEY)))
            CASE ( 'FILL' )
              NFILSOL = 1
          END SELECT ! SELECT CASE(KEY)
!
        ENDDO ! DO INI=1,NB_INIBRI
C---------
C---------
        IF(NFILSOL==0)RETURN
C---------
C---------
C------------------------------------
        ALLOCATE (ITRIS(NUMELS)                      ,STAT=stat)
        IF (STAT /= 0) THEN
          CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                          MSGTYPE=MSGERROR,
     .                          C1='ITRIS')
          RETURN
        END IF
        ALLOCATE (INDEXS(2*NUMELS)                   ,STAT=stat)
        IF (STAT /= 0) THEN
          CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                          MSGTYPE=MSGERROR,
     .                          C1='INDEXS')
          RETURN
        END IF
        ALLOCATE (KSYSUSRS(2*NUMELS)                 ,STAT=stat)
        IF (STAT /= 0) THEN
          CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                          MSGTYPE=MSGERROR,
     .                          C1='KSYSUSRS')
          RETURN
        END IF
        ITRIS  = 0
        INDEXS = 0
        KSYSUSRS=0
!
        DO IE = 1, NUMELS
          ITRIS(IE) = IXS(NIXS,IE)
        END DO
        CALL MY_ORDERS(0,WORKS,ITRIS,INDEXS,NUMELS,1)
        DO J = 1, NUMELS
          IE=INDEXS(J)
          KSYSUSRS(J)       =IXS(NIXS,IE)
          KSYSUSRS(NUMELS+J)=IE
        END DO
C------------------------------------
        CALL HM_OPTION_START('/INIBRI')
        DO INI=1,NB_INIBRI
!
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            UNIT_ID  = UID,
     .                            SUBMODEL_ID = SUB_ID,
     .                            KEYWORD2 = KEY)
!
          IFLAGUNIT = 0
          DO IUNIT=1,NUNITS
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
              NFILSOL = 1
              CALL HM_GET_INTV('inibri_fill_count',NB_ELEMENTS,IS_AVAILABLE,LSUBMODEL)
!
              DO J=1,NB_ELEMENTS
                ! Reading  ---  ID_ELEM, FILL  ---
                CALL HM_GET_INT_ARRAY_INDEX('brick_ID',ID_ELEM,J,IS_AVAILABLE,LSUBMODEL)
                CALL HM_GET_FLOAT_ARRAY_INDEX('value',FILL,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
                IE=UEL2SYS(ID_ELEM,KSYSUSRS,NUMELS)
                IF(IE/=0) FILLSOL(IE)=FILL
              ENDDO ! DO J=1,NB_ELEMENTS
C---------
          END SELECT ! SELECT CASE(KEY)
!
        ENDDO ! DO INI=1,NB_INIBRI
!
        DEALLOCATE(KSYSUSRS,INDEXS,ITRIS)
!
      ENDIF ! IF ( NB_INIBRI > 0 ) THEN
!
C-----------------------------------------------
      RETURN
      END
