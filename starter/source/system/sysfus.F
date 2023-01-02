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
Chd|  USR2SYS                       source/system/sysfus.F        
Chd|-- called by -----------
Chd|        ELSTAGBOX2                    source/model/box/rdbox.F      
Chd|        FSDCOD                        source/system/fsdcod.F        
Chd|        HM_LECGRN                     source/groups/hm_lecgrn.F     
Chd|        HM_PRELECGRNS                 source/groups/hm_prelecgrns.F 
Chd|        HM_READ_ADMAS                 source/tools/admas/hm_read_admas.F
Chd|        HM_READ_ALE_LINK              source/constraints/ale/hm_read_ale_link_vel.F
Chd|        HM_READ_BEAM                  source/elements/reader/hm_read_beam.F
Chd|        HM_READ_CYLJOINT              source/constraints/general/cyl_joint/hm_read_cyljoint.F
Chd|        HM_READ_EIG                   source/general_controls/computation/hm_read_eig.F
Chd|        HM_READ_EREF                  source/loads/reference_state/eref/hm_read_eref.F
Chd|        HM_READ_FRM                   source/tools/skew/hm_read_frm.F
Chd|        HM_READ_FXB1                  source/constraints/fxbody/hm_read_fxb.F
Chd|        HM_READ_GAUGE                 source/output/gauge/hm_read_gauge.F
Chd|        HM_READ_GJOINT                source/constraints/general/gjoint/hm_read_gjoint.F
Chd|        HM_READ_INICRACK              source/initial_conditions/inicrack/hm_read_inicrack.F
Chd|        HM_READ_INIMAP1D              source/initial_conditions/inimap/hm_read_inimap1d.F
Chd|        HM_READ_INIMAP2D              source/initial_conditions/inimap/hm_read_inimap2d.F
Chd|        HM_READ_INITEMP               source/initial_conditions/thermic/hm_read_initemp.F
Chd|        HM_READ_INIVEL                source/initial_conditions/general/inivel/hm_read_inivel.F
Chd|        HM_READ_INTERFACES            source/interfaces/reader/hm_read_interfaces.F
Chd|        HM_READ_INTER_TYPE12          source/interfaces/int12/hm_read_inter_type12.F
Chd|        HM_READ_LINES                 source/groups/hm_read_lines.F 
Chd|        HM_READ_MAT11                 source/materials/mat/mat011/hm_read_mat11.F
Chd|        HM_READ_MAT11_K_EPS           source/materials/mat/mat011/hm_read_mat11_k_eps.F
Chd|        HM_READ_MERGE                 source/constraints/general/merge/hm_read_merge.F
Chd|        HM_READ_MONVOL_TYPE11         source/airbag/hm_read_monvol_type11.F
Chd|        HM_READ_MONVOL_TYPE4          source/airbag/hm_read_monvol_type4.F
Chd|        HM_READ_MONVOL_TYPE5          source/airbag/hm_read_monvol_type5.F
Chd|        HM_READ_MONVOL_TYPE7          source/airbag/hm_read_monvol_type7.F
Chd|        HM_READ_MONVOL_TYPE8          source/airbag/hm_read_monvol_type8.F
Chd|        HM_READ_MONVOL_TYPE9          source/airbag/hm_read_monvol_type9.F
Chd|        HM_READ_MPC                   source/constraints/general/mpc/hm_read_mpc.F
Chd|        HM_READ_NBCS                  source/constraints/general/bcs/hm_read_nbcs.F
Chd|        HM_READ_PBLAST                source/loads/pblast/hm_read_pblast.F
Chd|        HM_READ_PROP15                source/properties/solid/hm_read_prop15.F
Chd|        HM_READ_QUAD                  source/elements/reader/hm_read_quad.F
Chd|        HM_READ_RBE2                  source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        HM_READ_RBE3                  source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        HM_READ_RBODY                 source/constraints/general/rbody/hm_read_rbody.F
Chd|        HM_READ_RBODY_LAGMUL          source/constraints/general/rbody/hm_read_rbody_lagmul.F
Chd|        HM_READ_RETRACTOR             source/tools/seatbelts/hm_read_retractor.F
Chd|        HM_READ_RIVET                 source/elements/reader/hm_read_rivet.F
Chd|        HM_READ_RWALL_CYL             source/constraints/general/rwall/hm_read_rwall_cyl.F
Chd|        HM_READ_RWALL_LAGMUL          source/constraints/general/rwall/hm_read_rwall_lagmul.F
Chd|        HM_READ_RWALL_PARAL           source/constraints/general/rwall/hm_read_rwall_paral.F
Chd|        HM_READ_RWALL_PLANE           source/constraints/general/rwall/hm_read_rwall_plane.F
Chd|        HM_READ_RWALL_SPHER           source/constraints/general/rwall/hm_read_rwall_spher.F
Chd|        HM_READ_SH3N                  source/elements/reader/hm_read_sh3n.F
Chd|        HM_READ_SHELL                 source/elements/reader/hm_read_shell.F
Chd|        HM_READ_SKW                   source/tools/skew/hm_read_skw.F
Chd|        HM_READ_SLIPRING              source/tools/seatbelts/hm_read_slipring.F
Chd|        HM_READ_SOLID                 source/elements/reader/hm_read_solid.F
Chd|        HM_READ_SPHCEL                source/elements/reader/hm_read_sphcel.F
Chd|        HM_READ_SPHIO                 source/loads/sph/hm_read_sphio.F
Chd|        HM_READ_SPRING                source/elements/reader/hm_read_spring.F
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|        HM_READ_THGRNE                source/output/th/hm_read_thgrne.F
Chd|        HM_READ_TRIA                  source/elements/reader/hm_read_tria.F
Chd|        HM_READ_TRUSS                 source/elements/reader/hm_read_truss.F
Chd|        HM_READ_XREF                  source/loads/reference_state/xref/hm_read_xref.F
Chd|        HM_SETFXRBYON                 source/constraints/fxbody/hm_setfxrbyon.F
Chd|        INIVEL                        source/initial_conditions/general/inivel/inivel.F
Chd|        LECACC                        source/tools/accele/lecacc.F  
Chd|        LECIG3D                       source/elements/ige3d/lecig3d.F
Chd|        LECREFSTA                     source/loads/reference_state/refsta/lecrefsta.F
Chd|        LECSEC42                      source/tools/sect/hm_read_sect.F
Chd|        LECSEC4BOLT                   source/tools/sect/lecsec4bolt.F
Chd|        NBADIGEMESH                   source/elements/ige3d/nbadigemesh.F
Chd|        NBADMESH                      source/model/remesh/nbadmesh.F
Chd|        PRELECSEC                     source/tools/sect/prelecsec.F 
Chd|        PREREAD_RBODY_SET             source/model/sets/preread_rbody_set.F
Chd|        R2R_DOMDEC                    source/coupling/rad2rad/r2r_domdec.F
Chd|        R2R_MONVOL                    source/coupling/rad2rad/r2r_prelec.F
Chd|        R2R_PRELEC                    source/coupling/rad2rad/r2r_prelec.F
Chd|        READ_BOX_CYL                  source/model/box/read_box_cyl.F
Chd|        READ_BOX_RECT                 source/model/box/read_box_rect.F
Chd|        READ_BOX_SPHER                source/model/box/read_box_spher.F
Chd|        READ_DFS_DETLINE              source/initial_conditions/detonation/read_dfs_detline.F
Chd|        READ_DFS_DETPLAN              source/initial_conditions/detonation/read_dfs_detplan.F
Chd|        READ_DFS_DETPOINT             source/initial_conditions/detonation/read_dfs_detpoint.F
Chd|        READ_IMPDISP_FGEO             source/constraints/general/impvel/read_impdisp_fgeo.F
Chd|        READ_IMPVEL_FGEO              source/constraints/general/impvel/read_impvel_fgeo.F
Chd|        READ_PCH_FILE                 source/constraints/fxbody/read_pch_file.F
Chd|        READ_SENSOR_DISP              source/tools/sensor/read_sensor_disp.F
Chd|        READ_SENSOR_DIST_SURF         source/tools/sensor/read_sensor_dist_surf.F
Chd|        READ_SENSOR_VEL               source/tools/sensor/read_sensor_vel.F
Chd|        READ_SENSOR_WORK              source/tools/sensor/read_sensor_work.F
Chd|        SENSOR_USER_CONVERT_LOCAL_ID  source/tools/sensor/sensor_user_convert_local_id.F
Chd|        SETRB2ON                      source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        UELT_SPMD_ADDITIONAL_NODE     source/user_interface/uaccess.F
Chd|        W_ITABM1                      source/restart/ddsplit/w_itabm1.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        R2R_SYS                       source/coupling/rad2rad/routines_r2r.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
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
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER R2R_SYS      
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "hash_id.inc"
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
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
   10   IF(JSUP<=JINF.AND.(IU-ITABM1(J))/=0) THEN
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
Chd|====================================================================
Chd|  USRTOS                        source/system/sysfus.F        
Chd|-- called by -----------
Chd|        ELSTAGBOX2                    source/model/box/rdbox.F      
Chd|        HM_READ_NODE                  source/elements/reader/hm_read_node.F
Chd|        INIVEL                        source/initial_conditions/general/inivel/inivel.F
Chd|        LECTRANS                      source/model/transformation/lectrans.F
Chd|        LECTRANSSUB                   source/model/submodel/lectranssub.F
Chd|        MERGE                         source/model/submodel/merge.F 
Chd|        MERGE_BUCKET_SEARCH           source/elements/nodes/merge_bucket_search.F
Chd|        MERGE_NODE                    source/elements/nodes/merge_node.F
Chd|-- calls ---------------
Chd|====================================================================
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
Chd|====================================================================
Chd|  ITABM1_SEARCH                 source/system/sysfus.F        
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
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
Chd|====================================================================
Chd|  USR2SYS2                      source/system/sysfus.F        
Chd|-- called by -----------
Chd|        ULIST2S                       source/system/sysfus.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        R2R_SYS                       source/coupling/rad2rad/routines_r2r.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      INTEGER FUNCTION USR2SYS2(IU,ITABM1,MESS,JINDEX)
      USE MESSAGE_MOD
C      IDENTIQUE A USR2SYS, RENVOYE L'INDEX JINDEX
C      FONCTION DONNE N0 SYSTEME DU NOEUD USER IU
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
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER R2R_SYS      
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
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
C------------Multidomaines -> On check dans la liste des noeuds suprimes-----
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
     .                I1=IU)
          USR2SYS2=0
        ENDIF     
        RETURN
      ENDIF
      IF((IU-ITABM1(J))==0)THEN
C     >CAS IU=TABM FIN DE LA RECHERCHE
         JINDEX=J
         USR2SYS2=ITABM1(J+NUMNOD)
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
Chd|====================================================================
Chd|  ULIST2S                       source/system/sysfus.F        
Chd|-- called by -----------
Chd|        HM_LECGRN                     source/groups/hm_lecgrn.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        USR2SYS2                      source/system/sysfus.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      INTEGER FUNCTION ULIST2S(LIST,NLIST,ITABM1,MESS,INDEX,ID)
      USE MESSAGE_MOD
C      FONCTION DONNE N0 SYSTEME D'UNE LISTE DE NOEUDS USER
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
C     ITABM1(1+NUMNOD:2*NUMNOD) INDEX SYSTEM 
C             ITABM1(NUMNOD+J) NO SYSTEM DU NOEUDS USER ITABM1(J)
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,NNOD,NOLD,K,
     .        IWORK(70000)
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
        INTEGER USR2SYS2
C-----------------------
C TRI DE LIST EN ORDRE CROISSANT
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
C RECHERCHE DES NOEUDS DE LIST() DANS ITABM1()
C  ALGO < NLIST+NUMNOD
C-----------------------
C        I=1
C        J=1
C USR2SYS2 renvoie J, index dans ITABM1 tq LIST(1)=ITABM1(J)
C de maniere a se positionner directement a la bonne adresse dans ITABM1
        LIST(1)=USR2SYS2(LIST(1),ITABM1,MESS,J)
        IF(J==0)THEN
C cas d erreur
          ULIST2S=0
        END IF
C
        DO I=2,NNOD
          DO WHILE(LIST(I)>ITABM1(J).AND.J<NUMNOD)
            J=J+1
          ENDDO
          IF(LIST(I)==ITABM1(J))THEN
            LIST(I)=ITABM1(NUMNOD+J)
          ELSE
            CALL ANCMSG(MSGID=78,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  C1=MESS,
     .                  I1=ID,
     .                  I2=LIST(I))
            ULIST2S=I-1
            RETURN
          ENDIF
        ENDDO
C
        ULIST2S=NNOD
        RETURN
        END
C
Chd|====================================================================
Chd|  UDOUBLE                       source/system/sysfus.F        
Chd|-- called by -----------
Chd|        HM_READ_CYLJOINT              source/constraints/general/cyl_joint/hm_read_cyljoint.F
Chd|        HM_READ_DRAPE                 source/properties/composite_options/drape/hm_read_drape.F
Chd|        HM_READ_FRM                   source/tools/skew/hm_read_frm.F
Chd|        HM_READ_FXB1                  source/constraints/fxbody/hm_read_fxb.F
Chd|        HM_READ_IMPACC                source/constraints/general/impvel/hm_read_impacc.F
Chd|        HM_READ_IMPVEL                source/constraints/general/impvel/hm_read_impvel.F
Chd|        HM_READ_INITEMP               source/initial_conditions/thermic/hm_read_initemp.F
Chd|        HM_READ_INIVEL                source/initial_conditions/general/inivel/hm_read_inivel.F
Chd|        HM_READ_INTSUB                source/output/subinterface/hm_read_intsub.F
Chd|        HM_READ_LINK                  source/constraints/rigidlink/hm_read_rlink.F
Chd|        HM_READ_PART                  source/model/assembling/hm_read_part.F
Chd|        HM_READ_PERTURB               source/general_controls/computation/hm_read_perturb.F
Chd|        HM_READ_PRELECDRAPE           source/properties/composite_options/drape/hm_read_drape.F
Chd|        HM_READ_RBODY                 source/constraints/general/rbody/hm_read_rbody.F
Chd|        HM_READ_RETRACTOR             source/tools/seatbelts/hm_read_retractor.F
Chd|        HM_READ_SENSORS               source/tools/sensor/hm_read_sensors.F
Chd|        HM_READ_SKW                   source/tools/skew/hm_read_skw.F
Chd|        HM_READ_SLIPRING              source/tools/seatbelts/hm_read_slipring.F
Chd|        HM_READ_SOLID                 source/elements/reader/hm_read_solid.F
Chd|        HM_READ_SPHCEL                source/elements/reader/hm_read_sphcel.F
Chd|        HM_READ_SPHIO                 source/loads/sph/hm_read_sphio.F
Chd|        HM_READ_TABLE2                source/tools/curve/hm_read_table.F
Chd|        HM_READ_THGRNE                source/output/th/hm_read_thgrne.F
Chd|        HM_READ_THGROU                source/output/th/hm_read_thgrou.F
Chd|        HM_READ_THPART                source/output/thpart/hm_read_thpart.F
Chd|        HM_READ_XELEM                 source/elements/reader/hm_read_xelem.F
Chd|        INIVEL                        source/initial_conditions/general/inivel/inivel.F
Chd|        LECIG3D                       source/elements/ige3d/lecig3d.F
Chd|        LECSEC42                      source/tools/sect/hm_read_sect.F
Chd|        LECSEC4BOLT                   source/tools/sect/lecsec4bolt.F
Chd|        READ_RWALL                    source/constraints/general/rwall/read_rwall.F
Chd|-- calls ---------------
Chd|        UDOUBL2                       source/system/sysfus.F        
Chd|====================================================================
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
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "hardware.inc"
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

Chd|====================================================================
Chd|  UDOUBLEX                      source/system/sysfus.F        
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
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
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "hardware.inc"
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

Chd|====================================================================
Chd|  UDOUBL2                       source/system/sysfus.F        
Chd|-- called by -----------
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
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
Chd|====================================================================
Chd|  NEWDBL                        source/system/sysfus.F        
Chd|-- called by -----------
Chd|        HM_READ_FXB1                  source/constraints/fxbody/hm_read_fxb.F
Chd|        HM_READ_RBODY                 source/constraints/general/rbody/hm_read_rbody.F
Chd|-- calls ---------------
Chd|        NEWDBL2                       source/system/sysfus.F        
Chd|====================================================================
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
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "hardware.inc"
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
Chd|====================================================================
Chd|  NEWDBL2                       source/system/sysfus.F        
Chd|-- called by -----------
Chd|        NEWDBL                        source/system/sysfus.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE NEWDBL2(INDEX,NLIST,LIST,ILIST,TAB,ERRID,STATUS,
     .                   NOM_OPT)
      USE MESSAGE_MOD
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ID,IDM, IWORK(70000),ID1
      CHARACTER TITR*nchartitle
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
               CALL FRETITL2(TITR,
     .                       NOM_OPT(LNOPT1-LTITR+1,I),LTITR)
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
Chd|====================================================================
Chd|  VDOUBLE                       source/system/sysfus.F        
Chd|-- called by -----------
Chd|        HM_READ_BEAM                  source/elements/reader/hm_read_beam.F
Chd|        HM_READ_GAUGE                 source/output/gauge/hm_read_gauge.F
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|        HM_READ_QUAD                  source/elements/reader/hm_read_quad.F
Chd|        HM_READ_RIVET                 source/elements/reader/hm_read_rivet.F
Chd|        HM_READ_SH3N                  source/elements/reader/hm_read_sh3n.F
Chd|        HM_READ_SHELL                 source/elements/reader/hm_read_shell.F
Chd|        HM_READ_SPRING                source/elements/reader/hm_read_spring.F
Chd|        HM_READ_TRIA                  source/elements/reader/hm_read_tria.F
Chd|        HM_READ_TRUSS                 source/elements/reader/hm_read_truss.F
Chd|        LECACC                        source/tools/accele/lecacc.F  
Chd|        LECSTACK_PLY                  source/properties/composite_options/stack/lecstack_ply.F
Chd|-- calls ---------------
Chd|        VDOUBL2                       source/system/sysfus.F        
Chd|====================================================================
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "hardware.inc"
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
Chd|====================================================================
Chd|  VDOUBL2                       source/system/sysfus.F        
Chd|-- called by -----------
Chd|        VDOUBLE                       source/system/sysfus.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
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
Chd|====================================================================
Chd|  UDOUBLE_WO_TITLE              source/system/sysfus.F        
Chd|-- called by -----------
Chd|        HM_READ_UNIT                  source/general_controls/computation/hm_read_unit.F
Chd|-- calls ---------------
Chd|        UDOUBL2_WO_TITLE              source/system/sysfus.F        
Chd|====================================================================
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
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "hardware.inc"
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
Chd|====================================================================
Chd|  UDOUBL2_WO_TITLE              source/system/sysfus.F        
Chd|-- called by -----------
Chd|        UDOUBLE_WO_TITLE              source/system/sysfus.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
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
Chd|====================================================================
Chd|  UDOUBLE3                      source/system/sysfus.F        
Chd|-- called by -----------
Chd|        HM_READ_DRAPE                 source/properties/composite_options/drape/hm_read_drape.F
Chd|-- calls ---------------
Chd|        UDOUBL3                       source/system/sysfus.F        
Chd|====================================================================
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
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "hardware.inc"
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
Chd|====================================================================
Chd|  UDOUBL3                       source/system/sysfus.F        
Chd|-- called by -----------
Chd|        UDOUBLE3                      source/system/sysfus.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
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
Chd|====================================================================
Chd|  UDOUBLE_IGR                   source/system/sysfus.F        
Chd|-- called by -----------
Chd|        HM_LECGRE                     source/groups/hm_lecgre.F     
Chd|        HM_LECGRN                     source/groups/hm_lecgrn.F     
Chd|        HM_READ_BOX                   source/model/box/hm_read_box.F
Chd|        HM_READ_GRPART                source/groups/hm_read_grpart.F
Chd|        HM_READ_INICRACK              source/initial_conditions/inicrack/hm_read_inicrack.F
Chd|        HM_READ_LINES                 source/groups/hm_read_lines.F 
Chd|        HM_READ_SUBSET                source/model/assembling/hm_read_subset.F
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|-- calls ---------------
Chd|        UDOUBL2_IGR                   source/system/sysfus.F        
Chd|====================================================================
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
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "hardware.inc"
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
Chd|====================================================================
Chd|  UDOUBLE_SET                   source/system/sysfus.F        
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        UDOUBL2_SET                   source/system/sysfus.F        
Chd|====================================================================
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
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "hardware.inc"
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
Chd|====================================================================
Chd|  UDOUBL2_IGR                   source/system/sysfus.F        
Chd|-- called by -----------
Chd|        UDOUBLE_IGR                   source/system/sysfus.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
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
Chd|====================================================================
Chd|  UDOUBL2_SET                   source/system/sysfus.F        
Chd|-- called by -----------
Chd|        UDOUBLE_SET                   source/system/sysfus.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
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
