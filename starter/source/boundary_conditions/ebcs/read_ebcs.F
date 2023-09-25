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
Chd|  READ_EBCS                     source/boundary_conditions/ebcs/read_ebcs.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_READ_EBCS_FLUXOUT          source/boundary_conditions/ebcs/hm_read_ebcs_fluxout.F
Chd|        HM_READ_EBCS_GRADP0           source/boundary_conditions/ebcs/hm_read_ebcs_gradp0.F
Chd|        HM_READ_EBCS_INIP             source/boundary_conditions/ebcs/hm_read_ebcs_inip.F
Chd|        HM_READ_EBCS_INIV             source/boundary_conditions/ebcs/hm_read_ebcs_iniv.F
Chd|        HM_READ_EBCS_INLET            source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
Chd|        HM_READ_EBCS_MONVOL           source/boundary_conditions/ebcs/hm_read_ebcs_monvol.F
Chd|        HM_READ_EBCS_NORMV            source/boundary_conditions/ebcs/hm_read_ebcs_normv.F
Chd|        HM_READ_EBCS_NRF              source/boundary_conditions/ebcs/hm_read_ebcs_nrf.F
Chd|        HM_READ_EBCS_PRES             source/boundary_conditions/ebcs/hm_read_ebcs_pres.F
Chd|        HM_READ_EBCS_VALVIN           source/boundary_conditions/ebcs/hm_read_ebcs_valvin.F
Chd|        HM_READ_EBCS_VALVOUT          source/boundary_conditions/ebcs/hm_read_ebcs_valvout.F
Chd|        HM_READ_EBCS_VEL              source/boundary_conditions/ebcs/hm_read_ebcs_vel.F
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        ALE_EBCS_MOD                  ../common_source/modules/ale/ale_ebcs_mod.F
Chd|        EBCS_MOD                      ../common_source/modules/boundary_conditions/ebcs_mod.F
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        RESTMOD                       share/modules1/restart_mod.F  
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE READ_EBCS(IGRSURF,MULTI_FVM,NPC1,IDX,LSUBMODEL,EBCS_TAB)
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
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "titr_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (SURF_), DIMENSION(NSURF), TARGET, INTENT(IN) :: IGRSURF
      TYPE (MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
      INTEGER, INTENT(IN) :: IDX, NPC1
      INTEGER LOCAL_ID,JFI,RFI,JBUF,RBUF
      INTEGER ID,TYP,UID

      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
      TYPE(T_EBCS_TAB), INTENT(INOUT) :: EBCS_TAB
!      INTEGER, INTENT(IN) :: IXQ(NIXQ,NUMELQ), IXTG(NIXTG,NUMELTG)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IFI, KK, I, KAD
      INTEGER STAT, II, SURF_ID, JJ
      CHARACTER*nchartitle :: TITR    
      CHARACTER*ncharkey :: KEY, KEY2
      LOGICAL :: IS_AVAILABLE
      INTEGER ::  INOD, NTAG, INODE, INDEX
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      IF (NEBCS > 0) THEN
         WRITE(IOUT,1000)
       
         WRITE(ISTDO,'(A)')TITRE(69)
         
!     Create structure for collecting ebcs
         CALL EBCS_TAB%CREATE(NEBCS)
!     Prepare data structures
         CALL HM_OPTION_START('/EBCS')
         DO II = 1, NEBCS
            LOCAL_ID = II
            CALL HM_OPTION_READ_KEY(LSUBMODEL, OPTION_ID = ID, UNIT_ID = UID, OPTION_TITR = TITR, 
     .           KEYWORD2 = KEY, KEYWORD3 = KEY2)
!     Allocate type
       SELECT CASE(KEY)
         CASE ('GRADP0')  
            TYP = 0

            allocate (t_ebcs_gradp0 :: EBCS_TAB%tab(ii)%poly)

            select type (twf => EBCS_TAB%tab(ii)%poly)
            type is (t_ebcs_gradp0)
              CALL HM_READ_EBCS_GRADP0(LOCAL_ID, IGRSURF,
     .             NPC1, NOM_OPT, MULTI_FVM,UNITAB, ID, TITR, TYP, UID, LSUBMODEL, twf)
            end select

         CASE ('PRES')  
            TYP = 1

            allocate (t_ebcs_pres :: EBCS_TAB%tab(ii)%poly)

            select type (twf => EBCS_TAB%tab(ii)%poly)
            type is (t_ebcs_pres)
              CALL HM_READ_EBCS_PRES(LOCAL_ID, IGRSURF,
     .             NPC1, NOM_OPT, MULTI_FVM,UNITAB, ID, TITR, TYP, UID, LSUBMODEL, twf)
            end select
    
         CASE ('VALVIN')  
            TYP = 2

            allocate (t_ebcs_valvin :: EBCS_TAB%tab(ii)%poly)

            select type (twf => EBCS_TAB%tab(ii)%poly)
            type is (t_ebcs_valvin)
              CALL HM_READ_EBCS_VALVIN(LOCAL_ID, IGRSURF,
     .             NPC1, NOM_OPT, MULTI_FVM,UNITAB, ID, TITR, TYP, UID, LSUBMODEL, twf)
            end select
    
         CASE ('VALVOUT')  
            TYP = 3

            allocate (t_ebcs_valvout :: EBCS_TAB%tab(ii)%poly)

            select type (twf => EBCS_TAB%tab(ii)%poly)
            type is (t_ebcs_valvout)
              CALL HM_READ_EBCS_VALVOUT(LOCAL_ID, IGRSURF,
     .             NPC1, NOM_OPT, MULTI_FVM,UNITAB, ID, TITR, TYP, UID, LSUBMODEL, twf)
           end select

          CASE ('VEL')  
            TYP = 4

            allocate (t_ebcs_vel :: EBCS_TAB%tab(ii)%poly)
            select type (twf => EBCS_TAB%tab(ii)%poly)
            type is (t_ebcs_vel)
              CALL HM_READ_EBCS_VEL(LOCAL_ID, IGRSURF,
     .             NPC1, NOM_OPT, MULTI_FVM,UNITAB, ID, TITR, TYP, UID, LSUBMODEL, twf)
           end select

         CASE ('NORMV') 
            TYP = 5
            allocate (t_ebcs_normv :: EBCS_TAB%tab(ii)%poly)
            select type (twf => EBCS_TAB%tab(ii)%poly)

            type is (t_ebcs_normv)
              CALL HM_READ_EBCS_NORMV(LOCAL_ID, IGRSURF,
     .             NPC1, NOM_OPT, MULTI_FVM,UNITAB, ID, TITR, TYP, UID, LSUBMODEL, twf)
           end select

         CASE ('INIP')  

            TYP = 6
            allocate (t_ebcs_inip :: EBCS_TAB%tab(ii)%poly)
            select type (twf => EBCS_TAB%tab(ii)%poly)
            type is (t_ebcs_inip)
              CALL HM_READ_EBCS_INIP(LOCAL_ID, IGRSURF,
     .             NPC1, NOM_OPT, MULTI_FVM,UNITAB, ID, TITR, TYP, UID, LSUBMODEL, twf)
            end select

         CASE ('INIV')  

            TYP = 7
            allocate (t_ebcs_iniv :: EBCS_TAB%tab(ii)%poly)
            select type (twf => EBCS_TAB%tab(ii)%poly)
            type is (t_ebcs_iniv)
                CALL HM_READ_EBCS_INIV(LOCAL_ID, IGRSURF,
     .             NPC1, NOM_OPT, MULTI_FVM,UNITAB, ID, TITR, TYP, UID, LSUBMODEL,twf)
           end select

         CASE ('INLET')

            TYP = 8
            allocate (t_ebcs_inlet :: EBCS_TAB%tab(ii)%poly)
            select type (twf => EBCS_TAB%tab(ii)%poly)
            type is (t_ebcs_inlet)

              CALL HM_READ_EBCS_INLET(LOCAL_ID, IGRSURF,
     .             NPC1, NOM_OPT, MULTI_FVM,UNITAB, ID, TITR, TYP, UID, LSUBMODEL, KEY2, twf)
           end select

         CASE ('FLUXOUT') 

            TYP = 9
            allocate (t_ebcs_fluxout :: EBCS_TAB%tab(ii)%poly)
            select type (twf => EBCS_TAB%tab(ii)%poly)
            type is (t_ebcs_fluxout)
              CALL HM_READ_EBCS_FLUXOUT(LOCAL_ID, IGRSURF,
     .             NPC1, NOM_OPT, MULTI_FVM,UNITAB, ID, TITR, TYP, UID, LSUBMODEL, twf)
            end select

         CASE ('NRF')

            TYP = 10
            allocate (t_ebcs_nrf :: EBCS_TAB%tab(ii)%poly)
            select type (twf => EBCS_TAB%tab(ii)%poly)
            type is (t_ebcs_nrf)
              CALL HM_READ_EBCS_NRF(LOCAL_ID, IGRSURF,
     .             NPC1, NOM_OPT, MULTI_FVM,UNITAB, ID, TITR, TYP, UID, LSUBMODEL, twf)     
            end select

         CASE ('MONVOL')

            TYP = 100
            allocate (t_ebcs_monvol :: EBCS_TAB%tab(ii)%poly)
            select type (twf => EBCS_TAB%tab(ii)%poly)
            type is (t_ebcs_monvol)
              CALL HM_READ_EBCS_MONVOL(LOCAL_ID, IGRSURF,
     .             NPC1, NOM_OPT, MULTI_FVM,UNITAB, ID, TITR, TYP, UID, LSUBMODEL, twf)
            end select

         CASE DEFAULT
           CALL ANCMSG(MSGID = 1602, MSGTYPE = MSGERROR, ANMODE = ANINFO,
     .          I1 = ID, C1 = TRIM(TITR), C2 = "\'"//TRIM(KEY)//"\'"//" IS NOT A VALID KEYWORD FOR EBCS OPTIONS")
         END SELECT  
         EBCS_TAB%tab(II)%poly%type = TYP
         EBCS_TAB%tab(II)%poly%ebcs_id = ID
!     Get surface id
         CALL HM_GET_INTV('entityid', SURF_ID, IS_AVAILABLE, LSUBMODEL)
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
             DO JJ = 1, EBCS_TAB%tab(II)%poly%nb_node    
                IF(EBCS_TAB%tab(II)%poly%type/=10) CALL IFRONTPLUS(EBCS_TAB%tab(II)%poly%node_list(jj), 1)  
                FLAGKIN(EBCS_TAB%tab(II)%poly%node_list(jj)) = 1         
             ENDDO                                                                    
            ENDIF
         ELSE
!     error
         ENDIF
         
      ENDDO
      ENDIF

      RETURN
 1000 FORMAT(
     &     5X,'    ELEMENTARY BOUNDARY CONDITIONS'/,
     &     5X,'    ------------------------------'//)
      END SUBROUTINE
