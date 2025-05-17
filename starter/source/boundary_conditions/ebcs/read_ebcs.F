Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    read_ebcs                ../starter/source/boundary_conditions/ebcs/read_ebcs.F
      !||--- called by ------------------------------------------------------
      !||    lectur                   ../starter/source/starter/lectur.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                   ../starter/source/output/message/message.F
      !||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_read_key       ../starter/source/devtools/hm_reader/hm_option_read_key.F
      !||    hm_option_start          ../starter/source/devtools/hm_reader/hm_option_start.F
      !||    hm_read_ebcs_fluxout     ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_fluxout.F
      !||    hm_read_ebcs_gradp0      ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_gradp0.F
      !||    hm_read_ebcs_inip        ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_inip.F
      !||    hm_read_ebcs_iniv        ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_iniv.F
      !||    hm_read_ebcs_inlet       ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
      !||    hm_read_ebcs_monvol      ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_monvol.F
      !||    hm_read_ebcs_normv       ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_normv.F
      !||    hm_read_ebcs_nrf         ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_nrf.F
      !||    hm_read_ebcs_pres        ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_pres.F
      !||    hm_read_ebcs_propergol   ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_propergol.F90
      !||    hm_read_ebcs_valvin      ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_valvin.F
      !||    hm_read_ebcs_valvout     ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_valvout.F
      !||    hm_read_ebcs_vel         ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_vel.F
      !||    ifrontplus               ../starter/source/spmd/node/frontplus.F
      !||--- uses       -----------------------------------------------------
      !||    front_mod                ../starter/share/modules1/front_mod.F
      !||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
      !||    message_mod              ../starter/share/message_module/message_mod.F
      !||    restmod                  ../starter/share/modules1/restart_mod.F
      !||    submodel_mod             ../starter/share/modules1/submodel_mod.F
      !||    table_mod                ../starter/share/modules1/table_mod.F
      !||====================================================================
      SUBROUTINE READ_EBCS(IGRSURF,MULTI_FVM,NPC1,LSUBMODEL,EBCS_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FRONT_MOD
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
      USE RESTMOD
      USE TABLE_MOD
      USE SUBMODEL_MOD
      USE ALE_EBCS_MOD
      USE EBCS_MOD
      USE HM_OPTION_READ_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "titr_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (SURF_), DIMENSION(NSURF), TARGET, INTENT(IN) :: IGRSURF
      TYPE (MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      INTEGER, INTENT(IN) :: NPC1
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
      TYPE(T_EBCS_TAB), INTENT(INOUT) :: EBCS_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: LOCAL_ID
      INTEGER :: ID,TYP,UID
      INTEGER :: II, SURF_ID, JJ, SUB_INDEX
      CHARACTER(LEN=NCHARTITLE) :: TITR
      CHARACTER(LEN=NCHARKEY) :: KEY, KEY2
      LOGICAL :: IS_AVAILABLE, IS_EBCS_PARALLEL
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      IF (NEBCS > 0) THEN
        WRITE(IOUT,1000)
        WRITE(ISTDO,'(A)')TITRE(69)
        CALL EBCS_TAB%CREATE(NEBCS)  !     Create structure for collecting ebcs
        CALL HM_OPTION_START('/EBCS') !     Prepare data structures
      ENDIF


      DO II = 1, NEBCS
         LOCAL_ID = II
         CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID = ID, UNIT_ID = UID, OPTION_TITR = TITR,
     .         KEYWORD2 = KEY, KEYWORD3 = KEY2, SUBMODEL_INDEX = SUB_INDEX)
!     Allocate type
        SELECT CASE(KEY(1:LEN_TRIM(KEY)))
         CASE ('GRADP0')

            TYP = 0
            allocate (t_ebcs_gradp0 :: EBCS_TAB%tab(ii)%poly)
            select type (twf => EBCS_TAB%tab(ii)%poly)
            type is (t_ebcs_gradp0)
              CALL HM_READ_EBCS_GRADP0(IGRSURF, NPC1, MULTI_FVM, UNITAB, ID, TITR, LSUBMODEL, twf)
            end select

         CASE ('PRES')

            TYP = 1
            allocate (t_ebcs_pres :: EBCS_TAB%tab(ii)%poly)
            select type (twf => EBCS_TAB%tab(ii)%poly)
            type is (t_ebcs_pres)
              CALL HM_READ_EBCS_PRES(IGRSURF, NPC1,  MULTI_FVM, UNITAB, ID, TITR, LSUBMODEL, twf)
            end select
    
         CASE ('VALVIN')

            TYP = 2
            allocate (t_ebcs_valvin :: EBCS_TAB%tab(ii)%poly)
            select type (twf => EBCS_TAB%tab(ii)%poly)
            type is (t_ebcs_valvin)
              CALL HM_READ_EBCS_VALVIN(IGRSURF, NPC1, MULTI_FVM, UNITAB, ID, TITR, LSUBMODEL, twf)
            end select
    
         CASE ('VALVOUT')

            TYP = 3
            allocate (t_ebcs_valvout :: EBCS_TAB%tab(ii)%poly)
            select type (twf => EBCS_TAB%tab(ii)%poly)
            type is (t_ebcs_valvout)
              CALL HM_READ_EBCS_VALVOUT( IGRSURF, NPC1, MULTI_FVM, UNITAB, ID, TITR,  LSUBMODEL, twf)
           end select

         CASE ('VEL')

            TYP = 4
            allocate (t_ebcs_vel :: EBCS_TAB%tab(ii)%poly)
            select type (twf => EBCS_TAB%tab(ii)%poly)
            type is (t_ebcs_vel)
              CALL HM_READ_EBCS_VEL(IGRSURF, NPC1, MULTI_FVM, UNITAB, ID, TITR, LSUBMODEL, twf)
           end select

         CASE ('NORMV')

            TYP = 5
            allocate (t_ebcs_normv :: EBCS_TAB%tab(ii)%poly)
            select type (twf => EBCS_TAB%tab(ii)%poly)

            type is (t_ebcs_normv)
              CALL HM_READ_EBCS_NORMV( IGRSURF, NPC1, MULTI_FVM, UNITAB, ID, TITR, LSUBMODEL, twf)
           end select

         CASE ('INIP')  

            TYP = 6
            allocate (t_ebcs_inip :: EBCS_TAB%tab(ii)%poly)
            select type (twf => EBCS_TAB%tab(ii)%poly)
            type is (t_ebcs_inip)
              CALL HM_READ_EBCS_INIP(IGRSURF, MULTI_FVM, UNITAB, ID, TITR, LSUBMODEL, twf)
            end select

         CASE ('INIV')  

            TYP = 7
            allocate (t_ebcs_iniv :: EBCS_TAB%tab(ii)%poly)
            select type (twf => EBCS_TAB%tab(ii)%poly)
            type is (t_ebcs_iniv)
                CALL HM_READ_EBCS_INIV(IGRSURF, MULTI_FVM, UNITAB, ID, TITR, LSUBMODEL, twf)
           end select

         CASE ('INLET')

            TYP = 8
            allocate (t_ebcs_inlet :: EBCS_TAB%tab(ii)%poly)
            select type (twf => EBCS_TAB%tab(ii)%poly)
            type is (t_ebcs_inlet)
              CALL HM_READ_EBCS_INLET(IGRSURF, NPC1, MULTI_FVM, UNITAB, ID, TITR, UID, LSUBMODEL, KEY2, SUB_INDEX, twf)
           end select

         CASE ('FLUXOUT') 

            TYP = 9
            allocate (t_ebcs_fluxout :: EBCS_TAB%tab(ii)%poly)
            select type (twf => EBCS_TAB%tab(ii)%poly)
            type is (t_ebcs_fluxout)
              CALL HM_READ_EBCS_FLUXOUT(IGRSURF, MULTI_FVM, UNITAB, ID, TITR, UID, LSUBMODEL, twf)
            end select

         CASE ('NRF')

            TYP = 10
            allocate (t_ebcs_nrf :: EBCS_TAB%tab(ii)%poly)
            select type (twf => EBCS_TAB%tab(ii)%poly)
            type is (t_ebcs_nrf)
              CALL HM_READ_EBCS_NRF(IGRSURF, MULTI_FVM, UNITAB, ID, TITR, UID, LSUBMODEL, twf)
            end select

         CASE ('PROPERGOL')

            TYP = 11
            allocate (t_ebcs_propergol :: EBCS_TAB%tab(ii)%poly)
            select type (twf => EBCS_TAB%tab(ii)%poly)
            type is (t_ebcs_propergol)
              CALL HM_READ_EBCS_PROPERGOL(IGRSURF, MULTI_FVM, UNITAB, ID, TITR, UID, LSUBMODEL,NSURF, twf)
            end select

         CASE ('MONVOL')

            TYP = 100
            allocate (t_ebcs_monvol :: EBCS_TAB%tab(ii)%poly)
            select type (twf => EBCS_TAB%tab(ii)%poly)
            type is (t_ebcs_monvol)
              CALL HM_READ_EBCS_MONVOL(IGRSURF,MULTI_FVM,UNITAB, ID, TITR, UID, LSUBMODEL, twf)
            end select

         CASE DEFAULT
           TYP = 0
           CALL ANCMSG(MSGID = 1602, MSGTYPE = MSGERROR, ANMODE = ANINFO,
     .          I1 = ID, C1 = TRIM(TITR), C2 = "\'"//TRIM(KEY)//"\'"//" IS NOT A VALID KEYWORD FOR EBCS OPTIONS")
         END SELECT

         EBCS_TAB%tab(II)%poly%type = TYP
         EBCS_TAB%tab(II)%poly%ebcs_id = ID
         !     Get surface id
         CALL HM_GET_INTV('entityid', SURF_ID, IS_AVAILABLE, LSUBMODEL)
         JJ = -HUGE(JJ)
         IF (SURF_ID > 0) THEN
            EBCS_TAB%tab(II)%poly%surf_id = 0
            DO JJ = 1, NSURF
               IF (IGRSURF(JJ)%ID == SURF_ID) THEN
                  EBCS_TAB%tab(II)%poly%surf_id = JJ
                  EXIT
               ENDIF
            ENDDO
         ENDIF
         IF (EBCS_TAB%tab(II)%poly%surf_id > 0) THEN
            CALL EBCS_TAB%tab(II)%poly%set_nodes_elems(IGRSURF(JJ)%NSEG, NUMNOD, IGRSURF(JJ)%NODES)         
            IF (IMACH == 3) THEN

              IS_EBCS_PARALLEL = .FALSE.
              IF(EBCS_TAB%tab(II)%poly%type == 10 .OR. EBCS_TAB%tab(II)%poly%type == 11)THEN
                IS_EBCS_PARALLEL = .TRUE.
              ENDIF

              IF(.NOT. IS_EBCS_PARALLEL) THEN
                DO JJ = 1, EBCS_TAB%tab(II)%poly%nb_node
                  CALL IFRONTPLUS(EBCS_TAB%tab(II)%poly%node_list(jj), 1)
                ENDDO
              ENDIF

              DO JJ = 1, EBCS_TAB%tab(II)%poly%nb_node
                FLAGKIN(EBCS_TAB%tab(II)%poly%node_list(jj)) = 1
              ENDDO

            ENDIF
         ELSE
           !     error
         ENDIF
         
      ENDDO

      RETURN
 1000 FORMAT(
     &     5X,'    ELEMENTARY BOUNDARY CONDITIONS'/,
     &     5X,'    ------------------------------')
      END SUBROUTINE
