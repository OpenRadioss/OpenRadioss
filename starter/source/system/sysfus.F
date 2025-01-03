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
      !||    usr2sys                        ../starter/source/system/sysfus.F
      !||--- called by ------------------------------------------------------
      !||    fsdcod                         ../starter/source/system/fsdcod.F
      !||    hm_lecgrn                      ../starter/source/groups/hm_lecgrn.F
      !||    hm_prelce16s                   ../starter/source/elements/reader/hm_read_solid.F
      !||    hm_prelecgrns                  ../starter/source/groups/hm_prelecgrns.F
      !||    hm_read_admas                  ../starter/source/tools/admas/hm_read_admas.F
      !||    hm_read_ale_link               ../starter/source/constraints/ale/hm_read_ale_link_vel.F
      !||    hm_read_alebcs                 ../starter/source/constraints/ale/hm_read_alebcs.F
      !||    hm_read_bcs                    ../starter/source/constraints/general/bcs/hm_read_bcs.F
      !||    hm_read_beam                   ../starter/source/elements/reader/hm_read_beam.F
      !||    hm_read_convec                 ../starter/source/loads/thermic/hm_read_convec.F
      !||    hm_read_cyljoint               ../starter/source/constraints/general/cyl_joint/hm_read_cyljoint.F
      !||    hm_read_eig                    ../starter/source/general_controls/computation/hm_read_eig.F
      !||    hm_read_eref                   ../starter/source/loads/reference_state/eref/hm_read_eref.F
      !||    hm_read_frm                    ../starter/source/tools/skew/hm_read_frm.F
      !||    hm_read_fxb1                   ../starter/source/constraints/fxbody/hm_read_fxb.F
      !||    hm_read_gauge                  ../starter/source/output/gauge/hm_read_gauge.F
      !||    hm_read_gjoint                 ../starter/source/constraints/general/gjoint/hm_read_gjoint.F
      !||    hm_read_grav                   ../starter/source/loads/general/grav/hm_read_grav.F
      !||    hm_read_impflux                ../starter/source/constraints/thermic/hm_read_impflux.F
      !||    hm_read_inicrack               ../starter/source/initial_conditions/inicrack/hm_read_inicrack.F
      !||    hm_read_inimap1d               ../starter/source/initial_conditions/inimap/hm_read_inimap1d.F
      !||    hm_read_inimap2d               ../starter/source/initial_conditions/inimap/hm_read_inimap2d.F
      !||    hm_read_initemp                ../starter/source/initial_conditions/thermic/hm_read_initemp.F
      !||    hm_read_inivel                 ../starter/source/initial_conditions/general/inivel/hm_read_inivel.F
      !||    hm_read_inject1                ../starter/source/properties/injector/hm_read_inject1.F
      !||    hm_read_inject2                ../starter/source/properties/injector/hm_read_inject2.F
      !||    hm_read_inter_lagdt_type07     ../starter/source/interfaces/int07/hm_read_inter_lagdt_type07.F
      !||    hm_read_inter_type12           ../starter/source/interfaces/int12/hm_read_inter_type12.F
      !||    hm_read_inter_type22           ../starter/source/interfaces/int22/hm_read_inter_type22.F
      !||    hm_read_interfaces             ../starter/source/interfaces/reader/hm_read_interfaces.F
      !||    hm_read_lines                  ../starter/source/groups/hm_read_lines.F
      !||    hm_read_link                   ../starter/source/constraints/rigidlink/hm_read_rlink.F
      !||    hm_read_merge                  ../starter/source/constraints/general/merge/hm_read_merge.F
      !||    hm_read_monvol_type11          ../starter/source/airbag/hm_read_monvol_type11.F
      !||    hm_read_monvol_type4           ../starter/source/airbag/hm_read_monvol_type4.F
      !||    hm_read_monvol_type5           ../starter/source/airbag/hm_read_monvol_type5.F
      !||    hm_read_monvol_type7           ../starter/source/airbag/hm_read_monvol_type7.F
      !||    hm_read_monvol_type8           ../starter/source/airbag/hm_read_monvol_type8.F
      !||    hm_read_monvol_type9           ../starter/source/airbag/hm_read_monvol_type9.F
      !||    hm_read_mpc                    ../starter/source/constraints/general/mpc/hm_read_mpc.F
      !||    hm_read_nbcs                   ../starter/source/constraints/general/bcs/hm_read_nbcs.F
      !||    hm_read_pblast                 ../starter/source/loads/pblast/hm_read_pblast.F
      !||    hm_read_pload                  ../starter/source/loads/general/pload/hm_read_pload.F
      !||    hm_read_preload                ../starter/source/loads/general/preload/hm_read_preload.F
      !||    hm_read_prop15                 ../starter/source/properties/solid/hm_read_prop15.F
      !||    hm_read_properties             ../starter/source/properties/hm_read_properties.F
      !||    hm_read_quad                   ../starter/source/elements/reader/hm_read_quad.F
      !||    hm_read_radiation              ../starter/source/loads/thermic/hm_read_radiation.F
      !||    hm_read_rbe2                   ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    hm_read_rbe3                   ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
      !||    hm_read_rbody                  ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    hm_read_rbody_lagmul           ../starter/source/constraints/general/rbody/hm_read_rbody_lagmul.F
      !||    hm_read_retractor              ../starter/source/tools/seatbelts/hm_read_retractor.F
      !||    hm_read_rivet                  ../starter/source/elements/reader/hm_read_rivet.F
      !||    hm_read_rwall_cyl              ../starter/source/constraints/general/rwall/hm_read_rwall_cyl.F
      !||    hm_read_rwall_lagmul           ../starter/source/constraints/general/rwall/hm_read_rwall_lagmul.F
      !||    hm_read_rwall_paral            ../starter/source/constraints/general/rwall/hm_read_rwall_paral.F
      !||    hm_read_rwall_plane            ../starter/source/constraints/general/rwall/hm_read_rwall_plane.F
      !||    hm_read_rwall_spher            ../starter/source/constraints/general/rwall/hm_read_rwall_spher.F
      !||    hm_read_rwall_therm            ../starter/source/constraints/general/rwall/hm_read_rwall_therm.F
      !||    hm_read_sensors                ../starter/source/tools/sensor/hm_read_sensors.F
      !||    hm_read_sh3n                   ../starter/source/elements/reader/hm_read_sh3n.F
      !||    hm_read_shell                  ../starter/source/elements/reader/hm_read_shell.F
      !||    hm_read_skw                    ../starter/source/tools/skew/hm_read_skw.F
      !||    hm_read_slipring               ../starter/source/tools/seatbelts/hm_read_slipring.F
      !||    hm_read_solid                  ../starter/source/elements/reader/hm_read_solid.F
      !||    hm_read_sphcel                 ../starter/source/elements/reader/hm_read_sphcel.F
      !||    hm_read_sphio                  ../starter/source/loads/sph/hm_read_sphio.F
      !||    hm_read_spring                 ../starter/source/elements/reader/hm_read_spring.F
      !||    hm_read_surf                   ../starter/source/groups/hm_read_surf.F
      !||    hm_read_thgrne                 ../starter/source/output/th/hm_read_thgrne.F
      !||    hm_read_thgrou                 ../starter/source/output/th/hm_read_thgrou.F
      !||    hm_read_thgrsurf               ../starter/source/output/th/hm_read_thgrsurf.F
      !||    hm_read_tria                   ../starter/source/elements/reader/hm_read_tria.F
      !||    hm_read_truss                  ../starter/source/elements/reader/hm_read_truss.F
      !||    hm_read_xref                   ../starter/source/loads/reference_state/xref/hm_read_xref.F
      !||    hm_setfxrbyon                  ../starter/source/constraints/fxbody/hm_setfxrbyon.F
      !||    hm_submodgrn                   ../starter/source/groups/hm_submodgr.F
      !||    hm_thgrki_vent                 ../starter/source/output/th/hm_thgrki_vent.F
      !||    i24surf_pxfem                  ../starter/source/interfaces/inter3d1/inintr2.F
      !||    ini_fxbody                     ../starter/source/constraints/fxbody/ini_fxbody.F
      !||    init_monvol                    ../starter/source/airbag/init_monvol.F
      !||    inslin                         ../starter/source/interfaces/interf1/inslin.F
      !||    insurf                         ../starter/source/interfaces/interf1/insurf.F
      !||    insurfigeo                     ../starter/source/interfaces/interf1/insurfigeo.F
      !||    lce16q3                        ../starter/source/elements/solid_2d/quad/lce16q.F
      !||    lce16s3                        ../starter/source/elements/reader/hm_read_solid.F
      !||    lec_inimap2d_file              ../starter/source/initial_conditions/inimap/lec_inimap2d_file.F
      !||    lecacc                         ../starter/source/tools/accele/lecacc.F
      !||    lecig3d                        ../starter/source/elements/ige3d/lecig3d.F
      !||    lecrefsta                      ../starter/source/loads/reference_state/refsta/lecrefsta.F
      !||    lecsec42                       ../starter/source/tools/sect/hm_read_sect.F
      !||    lecsec4bolt                    ../starter/source/tools/sect/lecsec4bolt.F
      !||    lecstack_ply                   ../starter/source/properties/composite_options/stack/lecstack_ply.F
      !||    lecstamp                       ../starter/source/interfaces/interf1/lecstamp.F
      !||    nbadigemesh                    ../starter/source/elements/ige3d/nbadigemesh.F
      !||    nbadmesh                       ../starter/source/model/remesh/nbadmesh.F
      !||    prelecsec                      ../starter/source/tools/sect/prelecsec.F
      !||    prelecsec4bolt                 ../starter/source/tools/sect/prelecsec4bolt.F
      !||    preread_rbody_set              ../starter/source/model/sets/preread_rbody_set.F
      !||    printbcs                       ../starter/source/constraints/general/bcs/printbcs.F
      !||    r2r_clean_inter                ../starter/source/coupling/rad2rad/r2r_clean_inter.F
      !||    r2r_domdec                     ../starter/source/coupling/rad2rad/r2r_domdec.F
      !||    r2r_monvol                     ../starter/source/coupling/rad2rad/r2r_prelec.F
      !||    r2r_prelec                     ../starter/source/coupling/rad2rad/r2r_prelec.F
      !||    read_box_cyl                   ../starter/source/model/box/read_box_cyl.F
      !||    read_box_rect                  ../starter/source/model/box/read_box_rect.F
      !||    read_box_spher                 ../starter/source/model/box/read_box_spher.F
      !||    read_dfs_detcord               ../starter/source/initial_conditions/detonation/read_dfs_detcord.F
      !||    read_dfs_detline               ../starter/source/initial_conditions/detonation/read_dfs_detline.F
      !||    read_dfs_detplan               ../starter/source/initial_conditions/detonation/read_dfs_detplan.F
      !||    read_dfs_detpoint              ../starter/source/initial_conditions/detonation/read_dfs_detpoint.F
      !||    read_dfs_wave_shaper           ../starter/source/initial_conditions/detonation/read_dfs_wave_shaper.F
      !||    read_impdisp_fgeo              ../starter/source/constraints/general/impvel/read_impdisp_fgeo.F
      !||    read_impvel                    ../starter/source/constraints/general/impvel/read_impvel.F
      !||    read_impvel_fgeo               ../starter/source/constraints/general/impvel/read_impvel_fgeo.F
      !||    read_impvel_lagmul             ../starter/source/constraints/general/impvel/read_impvel_lagmul.F
      !||    read_pch_file                  ../starter/source/constraints/fxbody/read_pch_file.F
      !||    read_sensor_disp               ../starter/source/tools/sensor/read_sensor_disp.F
      !||    read_sensor_dist_surf          ../starter/source/tools/sensor/read_sensor_dist_surf.F
      !||    read_sensor_user               ../starter/source/tools/sensor/read_sensor_user.F
      !||    read_sensor_vel                ../starter/source/tools/sensor/read_sensor_vel.F
      !||    read_sensor_work               ../starter/source/tools/sensor/read_sensor_work.F
      !||    sensor_user_convert_local_id   ../starter/source/tools/sensor/sensor_user_convert_local_id.F
      !||    set_user_window_nodes          ../starter/source/user_interface/user_windows_tools.F
      !||    setrb2on                       ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    thprin                         ../starter/source/output/th/thprin.F
      !||    thprin_sub                     ../starter/source/output/th/thprin.F
      !||    uelt_spmd_additional_node      ../starter/source/user_interface/uaccess.F
      !||    w_itabm1                       ../starter/source/restart/ddsplit/w_itabm1.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                         ../starter/source/output/message/message.F
      !||    r2r_sys                        ../starter/source/coupling/rad2rad/routines_r2r.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod                    ../starter/share/message_module/message_mod.F
      !||====================================================================
      INTEGER FUNCTION USR2SYS(IU,ITABM1,MESS,ID)
      USE MESSAGE_MOD
C      FONCTION DONNE N0 SYSTEME DU NOEUD USER IU
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IU,ID
      CHARACTER MESS*40
      INTEGER ITABM1(*)
      LOGICAL :: HAS_SEARCH_FAILED
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER R2R_SYS      
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "hash_id.inc"
#include      "com04_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JINF, JSUP, J, NN
      
      J = -1
      CALL C_HASH_FIND(H_NODE,IU,J)
      USR2SYS = J

      IF(NSUBDOM > 0 .OR. USR2SYS < 0 .OR. ITABM1(MAX(1,J)) /= IU) THEN
        JINF=1
        JSUP=NUMNOD
        J=MAX(1,NUMNOD/2)
        HAS_SEARCH_FAILED=.FALSE.
   10   IF(J == 0)THEN
         HAS_SEARCH_FAILED = .TRUE.
        ELSE
          IF(JSUP <= JINF .AND. (IU-ITABM1(J)) /= 0) HAS_SEARCH_FAILED=.TRUE.
        ENDIF
        IF(HAS_SEARCH_FAILED) THEN
          IF ((NSUBDOM>0).AND.(FLG_SPLIT==1)) THEN
C-----  -------Multidomaines -> On check dans la liste des noeuds suprimes-----
            NN=R2R_SYS(IU,ITABM1,MESS)
            IF (NN==0) THEN
              CALL ANCMSG(MSGID=895,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANSTOP,
     .                    I1=IU)
            ENDIF
C-----  ------------------------------------------------------    
          ELSE
            CALL ANCMSG(MSGID=78,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  C1=MESS,
     .                  I1=ID,
     .                  I2=IU)
            USR2SYS=0
          ENDIF      
          RETURN
        ENDIF
        IF((IU-ITABM1(J))==0)THEN
C       >CAS IU=TABM FIN DE LA RECHERCHE
           USR2SYS=ITABM1(J+NUMNOD)
           RETURN
        ELSE IF (IU-ITABM1(J)<0) THEN
C       >CAS IU<TABM
           JSUP=J-1
        ELSE
C       >CAS IU>TABM
           JINF=J+1
        ENDIF
        J=(JSUP+JINF)/2
        GO TO 10
      ENDIF
      END
C
      !||====================================================================
      !||    usrtos                ../starter/source/system/sysfus.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_initemp       ../starter/source/initial_conditions/thermic/hm_read_initemp.F
      !||    hm_read_node          ../starter/source/elements/reader/hm_read_node.F
      !||    inivel                ../starter/source/initial_conditions/general/inivel/inivel.F
      !||    lecsubmod             ../starter/source/model/submodel/lecsubmod.F
      !||    lectrans              ../starter/source/model/transformation/lectrans.F
      !||    lectranssub           ../starter/source/model/submodel/lectranssub.F
      !||    merge                 ../starter/source/model/submodel/merge.F
      !||    merge_bucket_search   ../starter/source/elements/nodes/merge_bucket_search.F
      !||    merge_cnod_cnod       ../starter/source/model/submodel/merge_cnod_cnod.F
      !||    merge_node            ../starter/source/elements/nodes/merge_node.F
      !||====================================================================
      INTEGER FUNCTION USRTOS(IU,ITABM1)
C      IDENTIQUE A USR2SYS, SANS GENERER D'ERREUR
C      FONCTION DONNE N0 SYSTEME DU NOEUD USER IU
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IU
      INTEGER ITABM1(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JINF, JSUP, J
      ! Out of bounds at startup - no need to iterate
      IF(IU < ITABM1(1) .OR. IU > ITABM1(NUMNOD) ) THEN
         USRTOS=0
         RETURN
      ENDIF

      JINF=1
      JSUP=NUMNOD
      J=MAX(1,NUMNOD/2)
   10 IF(J < 1 .OR. J>NUMNOD)THEN                      ! out of bounds
         USRTOS=0
         RETURN
      ENDIF
      IF(JSUP<=JINF.AND.(IU-ITABM1(J))/=0) THEN    ! not found
        USRTOS=0
        RETURN
      ENDIF
      IF((IU-ITABM1(J))==0)THEN
C     >CAS IU=TABM FIN DE LA RECHERCHE
         USRTOS=ITABM1(J+NUMNOD)
         RETURN
      ELSE IF (IU-ITABM1(J)<0) THEN
C     >CAS IU<TABM
         JSUP=J-1
      ELSE
C     >CAS IU>TABM
         JINF=J+1
      ENDIF
      J=(JSUP+JINF)/2
      GO TO 10
      END
C
      !||====================================================================
      !||    itabm1_search   ../starter/source/system/sysfus.F
      !||====================================================================
      INTEGER FUNCTION ITABM1_SEARCH(IU,ITABM1)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C      ITABM1_SEARCH : Return INDEX in ITABM1 for a given User ID
C      Permits to have : * entry in ITABM1
C                           Internal NOD_ID with (ITABM1(ENTRY+NUMNOD)
C                        * -1 if node was no found
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME                      DESCRIPTION              
C
C     IU (INPUT)                Node User ID      
C     ITABM1(2*NUMNOD) (INPUT)  Array for UserID -> Internal NodID Mapping       
C============================================================================
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IU
      INTEGER, INTENT(IN) :: ITABM1(2*NUMNOD)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JINF, JSUP, J
      JINF=1
      JSUP=NUMNOD
      J=MAX(1,NUMNOD/2)
   10 IF(JSUP<=JINF.AND.(IU-ITABM1(J))/=0) THEN
        ITABM1_SEARCH=-1
        RETURN
      ENDIF
      IF((IU-ITABM1(J))==0)THEN
C     >CAS IU=TABM FIN DE LA RECHERCHE
         ITABM1_SEARCH=J
         RETURN
      ELSE IF (IU-ITABM1(J)<0) THEN
C     >CAS IU<TABM
         JSUP=J-1
      ELSE
C     >CAS IU>TABM
         JINF=J+1
      ENDIF
      J=(JSUP+JINF)/2
      GO TO 10
      END
      !||====================================================================
      !||    usr2sys2      ../starter/source/system/sysfus.F
      !||--- called by ------------------------------------------------------
      !||--- calls      -----------------------------------------------------
      !||    ancmsg        ../starter/source/output/message/message.F
      !||    r2r_sys       ../starter/source/coupling/rad2rad/routines_r2r.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod   ../starter/share/message_module/message_mod.F
      !||====================================================================
      INTEGER FUNCTION USR2SYS2(IU,ITABM1,MESS,JINDEX,ID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C      SAME AS USR2SYS, SENDING INDEX JINDEX CORRESPONDING TO
C      INTERNAL IDENTIFIER OF USER NODE IDENTIFIER IU
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IU, JINDEX
      CHARACTER MESS*40
      INTEGER ITABM1(*) 
      INTEGER,INTENT(IN) :: ID    
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER R2R_SYS      
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JINF, JSUP, J, NN
      JINDEX=0
      JINF=1
      JSUP=NUMNOD
      J=MAX(1,NUMNOD/2)
   10 IF(JSUP<=JINF.AND.(IU-ITABM1(J))/=0) THEN
        IF (NSUBDOM>0) THEN
C------------Multidomaines -> checking in list of deleted nodes-----
          NN=R2R_SYS(IU,ITABM1,MESS)
          IF (NN==0) THEN
            CALL ANCMSG(MSGID=895,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANSTOP,
     .                  I1=IU)
          ENDIF
C-----------------------------------------------------------    
        ELSE
          CALL ANCMSG(MSGID=78,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                C1=MESS,
     .                I1=ID,
     .                I2=IU)
          USR2SYS2=0
        ENDIF     
        RETURN
      ENDIF
      IF((IU-ITABM1(J))==0)THEN
C     >CASE IU=TABM : ENDING THE SEARCH ALGORITHM
         JINDEX=J
         USR2SYS2=ITABM1(J+NUMNOD)
         RETURN
      ELSE IF (IU-ITABM1(J)<0) THEN
C     >CASE IU<TABM
         JSUP=J-1
      ELSE
C     >CASE IU>TABM
         JINF=J+1
      ENDIF
      J=(JSUP+JINF)/2
      GO TO 10
      END
C
      !||====================================================================
      !||    ulist2s            ../starter/source/system/sysfus.F
      !||--- called by ------------------------------------------------------
      !||    hm_lecgrn          ../starter/source/groups/hm_lecgrn.F
      !||    hm_read_thgrne     ../starter/source/output/th/hm_read_thgrne.F
      !||    hm_read_thgrou     ../starter/source/output/th/hm_read_thgrou.F
      !||    hm_read_thgrsurf   ../starter/source/output/th/hm_read_thgrsurf.F
      !||    hm_thgrki_vent     ../starter/source/output/th/hm_thgrki_vent.F
      !||    thprin             ../starter/source/output/th/thprin.F
      !||    thprin_sub         ../starter/source/output/th/thprin.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg             ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod        ../starter/share/message_module/message_mod.F
      !||====================================================================
      INTEGER FUNCTION ULIST2S(LIST,NLIST,ITABM1,MESS,INDEX,ID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C      Function is sending back Internal node identifiers from a list of user node identifiers
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LIST(*),NLIST,ID
      CHARACTER MESS*40
      INTEGER ITABM1(*),INDEX(*)
C     ITABM1(1:NUMNOD) NO USER TRIE
C     ITABM1(1+NUMNOD:2*NUMNOD) INDEX NUMBER
C             ITABM1(NUMNOD+J) INTERNAL NODE IDENTIFIER IN ITABM1(J)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,NNOD,NOLD,K, IWORK(70000)
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
        INTEGER USR2SYS2
C-----------------------
C SORT (ASCENDING ORDER)
C-----------------------
        CALL MY_ORDERS(0,IWORK,LIST,INDEX,NLIST,1)
        DO I=1,NLIST
          INDEX(NLIST+I) = LIST(INDEX(I))
        ENDDO
        K=1
        NOLD = INDEX(NLIST+1)
        DO I=1,NLIST
          IF(NOLD/=INDEX(NLIST+I))K=K+1
          LIST(K) = INDEX(NLIST+I)
          NOLD    = INDEX(NLIST+I)
        ENDDO
        NNOD=K
C-----------------------
C SEARCH NODES FROM LIST() IN ITABM1()
C  ALGO < NLIST+NUMNOD
C-----------------------
C        I=1
C        J=1
C USR2SYS2 is sending back J, index in ITABM1 array such as LIST(1)=ITABM1(J)
C cursor is then directly positioned on the correct address in ITABM1
        LIST(1)=USR2SYS2(LIST(1),ITABM1,MESS,J,ID)
        IF(J==0)THEN
          ! in case of error, node does not exist
          ULIST2S=0
        ELSE
C
          DO I=2,NNOD
            DO WHILE(LIST(I)>ITABM1(J).AND.J<NUMNOD)
              J=J+1
            ENDDO
            IF(LIST(I)==ITABM1(J))THEN
              LIST(I)=ITABM1(NUMNOD+J)
            ELSE
              CALL ANCMSG(MSGID=78,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    C1=MESS,
     .                    I1=ID,
     .                    I2=LIST(I))
              ULIST2S=I-1
              RETURN
            ENDIF
          ENDDO
C
          ULIST2S=NNOD
          
        ENDIF

        RETURN
        END
C
      !||====================================================================
      !||    udouble               ../starter/source/system/sysfus.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_cyljoint      ../starter/source/constraints/general/cyl_joint/hm_read_cyljoint.F
      !||    hm_read_drape         ../starter/source/properties/composite_options/drape/hm_read_drape.F
      !||    hm_read_frm           ../starter/source/tools/skew/hm_read_frm.F
      !||    hm_read_fxb1          ../starter/source/constraints/fxbody/hm_read_fxb.F
      !||    hm_read_impacc        ../starter/source/constraints/general/impvel/hm_read_impacc.F
      !||    hm_read_impvel        ../starter/source/constraints/general/impvel/hm_read_impvel.F
      !||    hm_read_initemp       ../starter/source/initial_conditions/thermic/hm_read_initemp.F
      !||    hm_read_inivel        ../starter/source/initial_conditions/general/inivel/hm_read_inivel.F
      !||    hm_read_intsub        ../starter/source/output/subinterface/hm_read_intsub.F
      !||    hm_read_link          ../starter/source/constraints/rigidlink/hm_read_rlink.F
      !||    hm_read_part          ../starter/source/model/assembling/hm_read_part.F
      !||    hm_read_perturb       ../starter/source/general_controls/computation/hm_read_perturb.F
      !||    hm_read_prelecdrape   ../starter/source/properties/composite_options/drape/hm_read_drape.F
      !||    hm_read_rbody         ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    hm_read_retractor     ../starter/source/tools/seatbelts/hm_read_retractor.F
      !||    hm_read_sensors       ../starter/source/tools/sensor/hm_read_sensors.F
      !||    hm_read_skw           ../starter/source/tools/skew/hm_read_skw.F
      !||    hm_read_slipring      ../starter/source/tools/seatbelts/hm_read_slipring.F
      !||    hm_read_solid         ../starter/source/elements/reader/hm_read_solid.F
      !||    hm_read_sphcel        ../starter/source/elements/reader/hm_read_sphcel.F
      !||    hm_read_sphio         ../starter/source/loads/sph/hm_read_sphio.F
      !||    hm_read_table2        ../starter/source/tools/curve/hm_read_table.F
      !||    hm_read_thgrne        ../starter/source/output/th/hm_read_thgrne.F
      !||    hm_read_thgrou        ../starter/source/output/th/hm_read_thgrou.F
      !||    hm_read_thpart        ../starter/source/output/thpart/hm_read_thpart.F
      !||    hm_read_xelem         ../starter/source/elements/reader/hm_read_xelem.F
      !||    lecig3d               ../starter/source/elements/ige3d/lecig3d.F
      !||    lecsec42              ../starter/source/tools/sect/hm_read_sect.F
      !||    lecsec4bolt           ../starter/source/tools/sect/lecsec4bolt.F
      !||    read_rwall            ../starter/source/constraints/general/rwall/read_rwall.F
      !||--- calls      -----------------------------------------------------
      !||    udoubl2               ../starter/source/system/sysfus.F
      !||====================================================================
      SUBROUTINE UDOUBLE(LIST,ILIST,NLIST,MESS,IR,RLIST)
C      TEST LES N0 DOUBLES
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C remonte la declaration des entiers pour la compile sur Compaq
      INTEGER ILIST,NLIST,LIST(ILIST,NLIST),IR
      my_real
     .    RLIST(ILIST,NLIST)     
      CHARACTER MESS*40
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C   ALLOC FREE
C-----------------------------------------------
#if CPP_comp == f90
      INTEGER, DIMENSION(:),ALLOCATABLE :: INDEX
#else
      POINTER(IINDEX,INDEX(1))
      INTEGER INDEX
#endif
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      IF (NLIST>=2)THEN
#if CPP_comp == f90
         ALLOCATE(INDEX(3*NLIST))
#else
        CALL MY_ALLOC(IINDEX,3*NLIST,0)
#endif
        CALL UDOUBL2(INDEX,NLIST,MESS,LIST,ILIST,IR,RLIST) 
#if CPP_comp == f90
         DEALLOCATE(INDEX)
#else
        CALL MY_FREE(IINDEX)
#endif
      ENDIF
C
      RETURN
      END

      !||====================================================================
      !||    udoublex   ../starter/source/system/sysfus.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE UDOUBLEX(NLIST,ILIST,IXX,KXX)
C      TEST LES N0 DOUBLES
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C remonte la declaration des entiers pour la compile sur Compaq
      INTEGER ILIST,NLIST,IXX(*),N,KXX(ILIST,*),
     .    IAD,nnod

C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C   ALLOC FREE
C-----------------------------------------------
#if CPP_comp == f90
      INTEGER, DIMENSION(:),ALLOCATABLE :: INDEX
#else
      POINTER(IINDEX,INDEX(1))
      INTEGER INDEX
#endif
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      IF (NLIST>=2)THEN
#if CPP_comp == f90
         ALLOCATE(INDEX(3*NLIST))
#else
        CALL MY_ALLOC(IINDEX,3*NLIST,0)
#endif
      DO n=1,nlist
        IAD=KXX(4,N)
        print*,'UBOUBLE X - MULTIBRIN NUM :',n
        NNOD = KXX(3,N)
        do i=1,NNOD
          print*,'IXX:', IXX(IAD+I-1)
        enddo
      enddo
#if CPP_comp == f90
         DEALLOCATE(INDEX)
#else
        CALL MY_FREE(IINDEX)
#endif
      ENDIF
C
      RETURN
      END

      !||====================================================================
      !||    udoubl2       ../starter/source/system/sysfus.F
      !||--- called by ------------------------------------------------------
      !||    udouble       ../starter/source/system/sysfus.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg        ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod   ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE UDOUBL2(INDEX,NLIST,MESS,LIST,ILIST,IR,RLIST)
      USE MESSAGE_MOD
C      TEST LES N0 DOUBLES
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NLIST,ILIST,IR
      CHARACTER MESS*40
      INTEGER INDEX(NLIST,3),LIST(ILIST,NLIST)
      my_real
     .    RLIST(ILIST,NLIST)     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,NNOD,NOLD,K,ID,IDM,
     .        IWORK(70000)
C-----------------------
C TRI DE LIST EN ORDRE CROISSANT
C-----------------------
      IF(IR==1)THEN
        DO I=1,NLIST
          INDEX(I,3)=NINT(RLIST(1,I))
        ENDDO
      ELSE
        DO I=1,NLIST
          INDEX(I,3)=LIST(1,I)
        ENDDO
      ENDIF
C
      CALL MY_ORDERS(0,IWORK,INDEX(1,3),INDEX,NLIST,1)
      ID=INDEX(INDEX(1,1),3)
      DO I=2,NLIST
          IDM=ID
          ID=INDEX(INDEX(I,1),3)
          IF(ID==IDM .AND. ID/=0)THEN
             CALL ANCMSG(MSGID=79,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   C1=MESS,
     .                   I1=ID)
          ENDIF
      ENDDO
C-----------------------
      RETURN
      END
C
      !||====================================================================
      !||    newdbl          ../starter/source/system/sysfus.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_fxb1    ../starter/source/constraints/fxbody/hm_read_fxb.F
      !||    hm_read_rbody   ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||--- calls      -----------------------------------------------------
      !||    newdbl2         ../starter/source/system/sysfus.F
      !||====================================================================
      SUBROUTINE NEWDBL(LIST,ILIST,NLIST,TAB,ERRID,STATUS,NOM_OPT)
C      TEST LES N0 DOUBLES SUR DES LISTES D'ID de noeuds ou elt ou ...
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
#include      "scr17_c.inc"
      INTEGER TAB(*)
      INTEGER ILIST,NLIST,LIST(ILIST,NLIST),ERRID,STATUS
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
C-----------------------------------------------
C   ALLOC FREE
C-----------------------------------------------
#if CPP_comp == f90
      INTEGER, DIMENSION(:),ALLOCATABLE :: INDEX
#else
      POINTER(IINDEX,INDEX(1))
      INTEGER INDEX
#endif
      IF (INVERS>=40.AND.NLIST>=2)THEN
#if CPP_comp == f90
         ALLOCATE(INDEX(3*NLIST))
#else
        CALL MY_ALLOC(IINDEX,3*NLIST,0)
#endif
        CALL NEWDBL2(INDEX,NLIST,LIST,ILIST,TAB,ERRID,STATUS,NOM_OPT) 
#if CPP_comp == f90
         DEALLOCATE(INDEX)
#else
        CALL MY_FREE(IINDEX)
#endif
      ENDIF
C
      RETURN
      END
      !||====================================================================
      !||    newdbl2                ../starter/source/system/sysfus.F
      !||--- called by ------------------------------------------------------
      !||    newdbl                 ../starter/source/system/sysfus.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||    fretitl2               ../starter/source/starter/freform.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE NEWDBL2(INDEX,NLIST,LIST,ILIST,TAB,ERRID,STATUS,
     .                   NOM_OPT)
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C      TEST LES N0 DOUBLES
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
#include      "scr17_c.inc"
      INTEGER NLIST,ILIST,ERRID,STATUS
      INTEGER TAB(*), INDEX(NLIST,3),LIST(ILIST,NLIST)
      INTEGER NOM_OPT(LNOPT1,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ID,IDM, IWORK(70000),ID1
      CHARACTER(LEN=NCHARTITLE)::TITR
C-----------------------
C TRI DE LIST EN ORDRE CROISSANT
C-----------------------
      DO I=1,NLIST
         INDEX(I,3)=LIST(1,I)
      ENDDO
C     
      CALL MY_ORDERS(0,IWORK,INDEX(1,3),INDEX,NLIST,1)
      ID=INDEX(INDEX(1,1),3)
      DO I=2,NLIST
         IDM=ID
         ID=INDEX(INDEX(I,1),3)
         IF(ID==IDM)THEN
            IF (STATUS < 0) THEN
C     CAS D ONE WARNING STATUS Negatif
               STATUS = -1*STATUS
               CALL ANCMSG(MSGID=ERRID,
     .                     MSGTYPE=MSGWARNING,
     .                     ANMODE=STATUS,I1=TAB(ID))
               STATUS = -1*STATUS
            ELSE
C     CAS D UNE ERREUR STATUS Positif
               ID1=NOM_OPT(1,I)
               CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,I),LTITR)
               CALL ANCMSG(MSGID=ERRID,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=STATUS,I1=ID1,C1=TITR,I2=TAB(ID))
            ENDIF
         ENDIF
      ENDDO
C-----------------------
      RETURN
      END
C
      !||====================================================================
      !||    vdouble              ../starter/source/system/sysfus.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_beam         ../starter/source/elements/reader/hm_read_beam.F
      !||    hm_read_gauge        ../starter/source/output/gauge/hm_read_gauge.F
      !||    hm_read_mat          ../starter/source/materials/mat/hm_read_mat.F90
      !||    hm_read_properties   ../starter/source/properties/hm_read_properties.F
      !||    hm_read_quad         ../starter/source/elements/reader/hm_read_quad.F
      !||    hm_read_rivet        ../starter/source/elements/reader/hm_read_rivet.F
      !||    hm_read_sh3n         ../starter/source/elements/reader/hm_read_sh3n.F
      !||    hm_read_shell        ../starter/source/elements/reader/hm_read_shell.F
      !||    hm_read_spring       ../starter/source/elements/reader/hm_read_spring.F
      !||    hm_read_tria         ../starter/source/elements/reader/hm_read_tria.F
      !||    hm_read_truss        ../starter/source/elements/reader/hm_read_truss.F
      !||    lecacc               ../starter/source/tools/accele/lecacc.F
      !||    lecstack_ply         ../starter/source/properties/composite_options/stack/lecstack_ply.F
      !||--- calls      -----------------------------------------------------
      !||    vdoubl2              ../starter/source/system/sysfus.F
      !||====================================================================
      SUBROUTINE VDOUBLE(LIST,ILIST,NLIST,MESS,IR,RLIST)
C      TEST LES N0 DOUBLES , TOUS FORMATS, ERREUR
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ILIST,NLIST,LIST(ILIST,NLIST),IR
      my_real
     .    RLIST(ILIST,NLIST)     
      CHARACTER MESS*40
C-----------------------------------------------
C   ALLOC FREE
C-----------------------------------------------
#if CPP_comp == f90
      INTEGER, DIMENSION(:),ALLOCATABLE :: INDEX
#else
      POINTER(IINDEX,INDEX(1))
      INTEGER INDEX
#endif
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
#if CPP_comp == f90
         ALLOCATE(INDEX(3*NLIST))
#else
        CALL MY_ALLOC(IINDEX,3*NLIST,0)
#endif
      CALL VDOUBL2(INDEX,NLIST,MESS,LIST,ILIST,IR,RLIST) 
#if CPP_comp == f90
         DEALLOCATE(INDEX)
#else
        CALL MY_FREE(IINDEX)
#endif
C
      RETURN
      END
      !||====================================================================
      !||    vdoubl2       ../starter/source/system/sysfus.F
      !||--- called by ------------------------------------------------------
      !||    vdouble       ../starter/source/system/sysfus.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg        ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod   ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE VDOUBL2(INDEX,NLIST,MESS,LIST,ILIST,IR,RLIST)
      USE MESSAGE_MOD
C      TEST LES N0 DOUBLES , TOUS FORMATS, ERREUR
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NLIST,ILIST,IR
      CHARACTER MESS*40
      INTEGER INDEX(NLIST,3),LIST(ILIST,NLIST)
      my_real
     .    RLIST(ILIST,NLIST)     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,NNOD,NOLD,K,ID,IDM,
     .        IWORK(70000)
C-----------------------
C TRI DE LIST EN ORDRE CROISSANT
C-----------------------
      IF(IR==1)THEN
        DO I=1,NLIST
          INDEX(I,3)=NINT(RLIST(1,I))
        ENDDO
      ELSE
        DO I=1,NLIST
          INDEX(I,3)=LIST(1,I)
        ENDDO
      ENDIF
C
      CALL MY_ORDERS(0,IWORK,INDEX(1,3),INDEX,NLIST,1)
      ID=INDEX(INDEX(1,1),3)
      DO I=2,NLIST
          IDM=ID
          ID=INDEX(INDEX(I,1),3)
          IF(ID==IDM .AND. ID/=0)THEN
             CALL ANCMSG(MSGID=79,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   C1=MESS,
     .                   I1=ID)
          ENDIF
      ENDDO
C-----------------------
      RETURN
      END
      !||====================================================================
      !||    udouble_wo_title   ../starter/source/system/sysfus.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_unit       ../starter/source/general_controls/computation/hm_read_unit.F
      !||--- calls      -----------------------------------------------------
      !||    udoubl2_wo_title   ../starter/source/system/sysfus.F
      !||====================================================================
      SUBROUTINE UDOUBLE_WO_TITLE(LIST,ILIST,NLIST,MESS,IR,RLIST)
C      TEST LES N0 DOUBLES
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C remonte la declaration des entiers pour la compile sur Compaq
      INTEGER ILIST,NLIST,LIST(ILIST,NLIST),IR
      my_real
     .    RLIST(ILIST,NLIST)     
      CHARACTER MESS*40
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C   ALLOC FREE
C-----------------------------------------------
#if CPP_comp == f90
      INTEGER, DIMENSION(:),ALLOCATABLE :: INDEX
#else
      POINTER(IINDEX,INDEX(1))
      INTEGER INDEX
#endif
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      IF (NLIST>=2)THEN
#if CPP_comp == f90
         ALLOCATE(INDEX(3*NLIST))
#else
        CALL MY_ALLOC(IINDEX,3*NLIST,0)
#endif
        CALL UDOUBL2_WO_TITLE(INDEX,NLIST,MESS,LIST,ILIST,IR,RLIST) 
#if CPP_comp == f90
         DEALLOCATE(INDEX)
#else
        CALL MY_FREE(IINDEX)
#endif
      ENDIF
C
      RETURN
      END
      !||====================================================================
      !||    udoubl2_wo_title   ../starter/source/system/sysfus.F
      !||--- called by ------------------------------------------------------
      !||    udouble_wo_title   ../starter/source/system/sysfus.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg             ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod        ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE UDOUBL2_WO_TITLE(INDEX,NLIST,MESS,LIST,ILIST,IR,RLIST)
      USE MESSAGE_MOD
C      TEST LES N0 DOUBLES
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NLIST,ILIST,IR
      CHARACTER MESS*40
      INTEGER INDEX(NLIST,3),LIST(ILIST,NLIST)
      my_real
     .    RLIST(ILIST,NLIST)     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,NNOD,NOLD,K,ID,IDM,IDS,
     .        IWORK(70000)
C-----------------------
C TRI DE LIST EN ORDRE CROISSANT
C-----------------------
      IF(IR==1)THEN
        DO I=1,NLIST
          INDEX(I,3)=NINT(RLIST(1,I))
        ENDDO
      ELSE
        DO I=1,NLIST
          INDEX(I,3)=LIST(1,I)
        ENDDO
      ENDIF
C
      CALL MY_ORDERS(0,IWORK,INDEX(1,3),INDEX,NLIST,1)
      ID=INDEX(INDEX(1,1),3)
      DO I=2,NLIST
          IDM=ID
          ID=INDEX(INDEX(I,1),3)
          IF(ID==IDM)THEN
             IDS=LIST(1,I)
             CALL ANCMSG(MSGID=1108,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   C1=MESS,
     .                   I1=ID)
          ENDIF
      ENDDO
C-----------------------
      RETURN
      END
      !||====================================================================
      !||    udouble3        ../starter/source/system/sysfus.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_drape   ../starter/source/properties/composite_options/drape/hm_read_drape.F
      !||--- calls      -----------------------------------------------------
      !||    udoubl3         ../starter/source/system/sysfus.F
      !||====================================================================
      SUBROUTINE UDOUBLE3(LIST,ILIST,NLIST,MESS,MESS2,IR,RLIST)
C      TEST LES N0 DOUBLES
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C remonte la declaration des entiers pour la compile sur Compaq
      INTEGER ILIST,NLIST,LIST(ILIST,NLIST),IR
      my_real
     .    RLIST(ILIST,NLIST)     
      CHARACTER MESS*40,MESS2*40
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C   ALLOC FREE
C-----------------------------------------------
#if CPP_comp == f90
      INTEGER, DIMENSION(:),ALLOCATABLE :: INDEX
#else
      POINTER(IINDEX,INDEX(1))
      INTEGER INDEX
#endif
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      IF (NLIST>=2)THEN
#if CPP_comp == f90
         ALLOCATE(INDEX(3*NLIST))
#else
        CALL MY_ALLOC(IINDEX,3*NLIST,0)
#endif
        CALL UDOUBL3(INDEX,NLIST,MESS,MESS2,LIST,ILIST,IR,RLIST) 
#if CPP_comp == f90
         DEALLOCATE(INDEX)
#else
        CALL MY_FREE(IINDEX)
#endif
      ENDIF
C
      RETURN
      END
      !||====================================================================
      !||    udoubl3       ../starter/source/system/sysfus.F
      !||--- called by ------------------------------------------------------
      !||    udouble3      ../starter/source/system/sysfus.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg        ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod   ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE UDOUBL3(INDEX,NLIST,MESS,MESS2,LIST,ILIST,IR,RLIST)
      USE MESSAGE_MOD
C      TEST LES N0 DOUBLES
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NLIST,ILIST,IR
      CHARACTER MESS*40,MESS2*40
      INTEGER INDEX(NLIST,3),LIST(ILIST,NLIST)
      my_real
     .    RLIST(ILIST,NLIST)     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,NNOD,NOLD,K,ID,IDM,IDS,
     .        IWORK(70000)
C-----------------------
C TRI DE LIST EN ORDRE CROISSANT
C-----------------------
      IF(IR==1)THEN
        DO I=1,NLIST
          INDEX(I,3)=NINT(RLIST(1,I))
        ENDDO
      ELSE
        DO I=1,NLIST
          INDEX(I,3)=LIST(1,I)
        ENDDO
      ENDIF
C
      CALL MY_ORDERS(0,IWORK,INDEX(1,3),INDEX,NLIST,1)
      ID=INDEX(INDEX(1,1),3)
      DO I=2,NLIST
          IDM=ID
          ID=INDEX(INDEX(I,1),3)
          IF(ID==IDM)THEN
             IDS=LIST(2,I)
             CALL ANCMSG(MSGID=1154,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   C1=MESS,
     .                   I1=IDS,
     .                   C2=MESS2,
     .                   I2=ID)
          ENDIF
      ENDDO
C-----------------------
      RETURN
      END
      !||====================================================================
      !||    udouble_igr        ../starter/source/system/sysfus.F
      !||--- called by ------------------------------------------------------
      !||    hm_lecgre          ../starter/source/groups/hm_lecgre.F
      !||    hm_lecgrn          ../starter/source/groups/hm_lecgrn.F
      !||    hm_read_box        ../starter/source/model/box/hm_read_box.F
      !||    hm_read_grpart     ../starter/source/groups/hm_read_grpart.F
      !||    hm_read_inicrack   ../starter/source/initial_conditions/inicrack/hm_read_inicrack.F
      !||    hm_read_lines      ../starter/source/groups/hm_read_lines.F
      !||    hm_read_subset     ../starter/source/model/assembling/hm_read_subset.F
      !||    hm_read_surf       ../starter/source/groups/hm_read_surf.F
      !||--- calls      -----------------------------------------------------
      !||    udoubl2_igr        ../starter/source/system/sysfus.F
      !||====================================================================
      SUBROUTINE UDOUBLE_IGR(LIST,NLIST,MESS,IR,RLIST)
C      TEST LES N0 DOUBLES
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C remonte la declaration des entiers pour la compile sur Compaq
      INTEGER NLIST,LIST(NLIST),IR
      my_real
     .    RLIST(NLIST)     
      CHARACTER MESS*40
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C   ALLOC FREE
C-----------------------------------------------
#if CPP_comp == f90
      INTEGER, DIMENSION(:),ALLOCATABLE :: INDEX
#else
      POINTER(IINDEX,INDEX(1))
      INTEGER INDEX
#endif
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      IF (NLIST>=2)THEN
#if CPP_comp == f90
         ALLOCATE(INDEX(3*NLIST))
#else
        CALL MY_ALLOC(IINDEX,3*NLIST,0)
#endif
        CALL UDOUBL2_IGR(INDEX,NLIST,MESS,LIST,IR,RLIST) 
#if CPP_comp == f90
         DEALLOCATE(INDEX)
#else
        CALL MY_FREE(IINDEX)
#endif
      ENDIF
C
      RETURN
      END
      !||====================================================================
      !||    udouble_set   ../starter/source/system/sysfus.F
      !||--- calls      -----------------------------------------------------
      !||    udoubl2_set   ../starter/source/system/sysfus.F
      !||====================================================================
      SUBROUTINE UDOUBLE_SET(LIST,NLIST,MESS,IR,RLIST)
C      TEST LES N0 DOUBLES
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C remonte la declaration des entiers pour la compile sur Compaq
      INTEGER NLIST,LIST(NLIST),IR
      my_real
     .    RLIST(NLIST)     
      CHARACTER MESS*40
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C   ALLOC FREE
C-----------------------------------------------
#if CPP_comp == f90
      INTEGER, DIMENSION(:),ALLOCATABLE :: INDEX
#else
      POINTER(IINDEX,INDEX(1))
      INTEGER INDEX
#endif
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      IF (NLIST>=2)THEN
#if CPP_comp == f90
         ALLOCATE(INDEX(3*NLIST))
#else
        CALL MY_ALLOC(IINDEX,3*NLIST,0)
#endif
        CALL UDOUBL2_SET(INDEX,NLIST,MESS,LIST,IR,RLIST) 
#if CPP_comp == f90
         DEALLOCATE(INDEX)
#else
        CALL MY_FREE(IINDEX)
#endif
      ENDIF
C
      RETURN
      END
      !||====================================================================
      !||    udoubl2_igr   ../starter/source/system/sysfus.F
      !||--- called by ------------------------------------------------------
      !||    udouble_igr   ../starter/source/system/sysfus.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg        ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod   ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE UDOUBL2_IGR(INDEX,NLIST,MESS,LIST,IR,RLIST)
      USE MESSAGE_MOD
C      TEST LES N0 DOUBLES
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NLIST,IR
      CHARACTER MESS*40
      INTEGER INDEX(NLIST,3),LIST(NLIST)
      my_real
     .    RLIST(NLIST)     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,NNOD,NOLD,K,ID,IDM,
     .        IWORK(70000)
C-----------------------
C TRI DE LIST EN ORDRE CROISSANT
C-----------------------
      IF(IR==1)THEN
        DO I=1,NLIST
          INDEX(I,3)=NINT(RLIST(I))
        ENDDO
      ELSE
        DO I=1,NLIST
          INDEX(I,3)=LIST(I)
        ENDDO
      ENDIF
C
      CALL MY_ORDERS(0,IWORK,INDEX(1,3),INDEX,NLIST,1)
      ID=INDEX(INDEX(1,1),3)
      DO I=2,NLIST
          IDM=ID
          ID=INDEX(INDEX(I,1),3)
          IF(ID==IDM .AND. ID/=0)THEN
             CALL ANCMSG(MSGID=79,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   C1=MESS,
     .                   I1=ID)
          ENDIF
      ENDDO
C-----------------------
      RETURN
      END
      !||====================================================================
      !||    udoubl2_set   ../starter/source/system/sysfus.F
      !||--- called by ------------------------------------------------------
      !||    udouble_set   ../starter/source/system/sysfus.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg        ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod   ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE UDOUBL2_SET(INDEX,NLIST,MESS,LIST,IR,RLIST)
      USE MESSAGE_MOD
C      TEST LES N0 DOUBLES
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NLIST,IR
      CHARACTER MESS*40
      INTEGER INDEX(NLIST,3),LIST(NLIST)
      my_real
     .    RLIST(NLIST)     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,NNOD,NOLD,K,ID,IDM,
     .        IWORK(70000)
C-----------------------
C TRI DE LIST EN ORDRE CROISSANT
C-----------------------
      IF(IR==1)THEN
        DO I=1,NLIST
          INDEX(I,3)=NINT(RLIST(I))
        ENDDO
      ELSE
        DO I=1,NLIST
          INDEX(I,3)=LIST(I)
        ENDDO
      ENDIF
C
      CALL MY_ORDERS(0,IWORK,INDEX(1,3),INDEX,NLIST,1)
      ID=INDEX(INDEX(1,1),3)
      DO I=2,NLIST
          IDM=ID
          ID=INDEX(INDEX(I,1),3)
          IF(ID==IDM)THEN
             CALL ANCMSG(MSGID=1814,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   C1=MESS,
     .                   I1=ID)
          ENDIF
      ENDDO
C-----------------------
      RETURN
      END
