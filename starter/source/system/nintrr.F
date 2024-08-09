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
      !||====================================================================
      !||    nintri                 ../starter/source/system/nintrr.F
      !||--- called by ------------------------------------------------------
      !||    ale_euler_init         ../starter/source/materials/ale/ale_euler_init.F
      !||    fill_buffer_51         ../starter/source/materials/mat/mat051/fill_buffer_51.F
      !||    fsdcod                 ../starter/source/system/fsdcod.F
      !||    hm_preread_part        ../starter/source/model/assembling/hm_read_part.F
      !||    hm_read_eref           ../starter/source/loads/reference_state/eref/hm_read_eref.F
      !||    hm_read_inject1        ../starter/source/properties/injector/hm_read_inject1.F
      !||    hm_read_inject2        ../starter/source/properties/injector/hm_read_inject2.F
      !||    hm_read_part           ../starter/source/model/assembling/hm_read_part.F
      !||    hm_read_retractor      ../starter/source/tools/seatbelts/hm_read_retractor.F
      !||    hm_read_slipring       ../starter/source/tools/seatbelts/hm_read_slipring.F
      !||    hm_read_thpart         ../starter/source/output/thpart/hm_read_thpart.F
      !||    lecint                 ../starter/source/interfaces/interf1/lecint.F
      !||    nbadigemesh            ../starter/source/elements/ige3d/nbadigemesh.F
      !||    read_dfs_detcord       ../starter/source/initial_conditions/detonation/read_dfs_detcord.F
      !||    read_dfs_detline       ../starter/source/initial_conditions/detonation/read_dfs_detline.F
      !||    read_dfs_detplan       ../starter/source/initial_conditions/detonation/read_dfs_detplan.F
      !||    read_dfs_detpoint      ../starter/source/initial_conditions/detonation/read_dfs_detpoint.F
      !||    read_dfs_wave_shaper   ../starter/source/initial_conditions/detonation/read_dfs_wave_shaper.F
      !||====================================================================
      INTEGER FUNCTION NINTRI(IEXT,ANTN,M,N,M1)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IEXT, M, N, M1, ANTN(M,N)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IE
      DO I=1,N
        IE=ANTN(M1,I)
        IF(IE==IEXT)THEN
          NINTRI=I
          RETURN
        ENDIF
      ENDDO
      NINTRI=0
      RETURN
      END
C
      !||====================================================================
      !||    nintlst                ../starter/source/system/nintrr.F
      !||--- called by ------------------------------------------------------
      !||    hm_lecgre              ../starter/source/groups/hm_lecgre.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||====================================================================
      INTEGER FUNCTION NINTLST (LIST,NLIST,IX,NIX,NUMEL,MESS,
     .                          IX1,IX2,INDEX,KK,
     .                          TYPE,ID,TITR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C      FONCTION DONNE N0 SYSTEME D'UNE LISTE D'ELEMENTS USER
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NLIST,KK,NIX,NUMEL
      CHARACTER MESS*40
      INTEGER LIST(*),IX(NIX,*),INDEX(*),IX1(*),IX2(*)
      INTEGER ID
      CHARACTER(LEN=NCHARTITLE) :: TYPE,TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,NEL,NOLD,K,
     .        IWORK(70000)
C-----------------------
C TRI DE LIST EN ORDRE CROISSANT
C AVEC SUPPRESSION DES No DOUBLES
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
        NEL=K
C-----------------------
C TRI DE IX EN ORDRE CROISSANT si KK = 0
C-----------------------
        IF(KK==0)THEN
          DO I=1,NUMEL
            IX2(I) = IX(NIX,I)
          ENDDO
          CALL MY_ORDERS(0,IWORK,IX2,INDEX,NUMEL,1)
          DO I=1,NUMEL
            IX1(I) = IX2(INDEX(I))
          ENDDO
          DO I=1,NUMEL
            IX2(I) = INDEX(I)
          ENDDO
        ENDIF
C-----------------------
C RECHERCHE DES ELEMENTS DE LIST() DANS IX()
C  ALGO < NLIST+NUMEL
C-----------------------
        I=1
        J=1
        DO I=1,NEL
          DO WHILE(LIST(I)>IX1(J).AND.J<NUMEL)
            J=J+1
          ENDDO
          IF(LIST(I)==IX1(J))THEN
            LIST(I)=IX2(J)
          ELSE
             CALL ANCMSG(MSGID=70,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   C1=TYPE,
     .                   I1=ID,
     .                   C2=TITR,
     .                   I2=LIST(I))
            NINTLST=I-1
            RETURN
          ENDIF
        ENDDO
C
        NINTLST=NEL
        RETURN
        END
C
      !||====================================================================
      !||    nintlst2       ../starter/source/system/nintrr.F
      !||--- called by ------------------------------------------------------
      !||    state_admesh   ../starter/source/model/remesh/state_admesh.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg         ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod    ../starter/share/message_module/message_mod.F
      !||====================================================================
      INTEGER FUNCTION NINTLST2 (LIST,NLIST,INDEXL,IX,NIX,NUMEL,
     .                           MESS,IX1,IX2,INDEX,KK)
      USE MESSAGE_MOD
C      FONCTION DONNE N0 SYSTEME D'UNE LISTE D'ELEMENTS USER, AUTORISE LES 0.
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NLIST,KK,NIX,NUMEL
      CHARACTER MESS*40
      INTEGER LIST(*),INDEXL(*),IX(NIX,*),INDEX(*),IX1(*),IX2(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,NOLD,K,
     .        IWORK(70000)
C-----------------------
C TRI DE LIST EN ORDRE CROISSANT
C-----------------------
        CALL MY_ORDERS(0,IWORK,LIST,INDEXL,NLIST,1)
C-----------------------
C TRI DE IX EN ORDRE CROISSANT si KK = 0
C-----------------------
        IF(KK==0)THEN
          DO I=1,NUMEL
            IX2(I) = IX(NIX,I)
          ENDDO
          CALL MY_ORDERS(0,IWORK,IX2,INDEX,NUMEL,1)
          DO I=1,NUMEL
            IX1(I) = IX2(INDEX(I))
          ENDDO
          DO I=1,NUMEL
            IX2(I) = INDEX(I)
          ENDDO
        ENDIF
C-----------------------
C RECHERCHE DES ELEMENTS DE LIST() DANS IX()
C  ALGO < NLIST+NUMEL
C-----------------------
        I=1
        J=1
        DO I=1,NLIST
          DO WHILE(LIST(INDEXL(I))>IX1(J).AND.J<NUMEL)
            J=J+1
          ENDDO
          IF(LIST(INDEXL(I))==IX1(J))THEN
            LIST(INDEXL(I))=IX2(J)
          ELSEIF(LIST(INDEXL(I))/=0)THEN
C            WRITE(IOUT,*)MESS
C            WRITE(IOUT,*)' ** ERROR : ELEMENT ',LIST(I),
C     .                   ' DOESNT''EXIST'
C            WRITE(ISTDO,*)MESS
C            WRITE(ISTDO,*)' ** ERROR : ELEMENT ',LIST(I),
C     .                    ' DOESNT''EXIST'
C            IERR=IERR+1
             CALL ANCMSG(MSGID=71,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   C1=MESS,
     .                   I1=LIST(INDEXL(I)))
            NINTLST2=I-1
            RETURN
          ENDIF
        ENDDO
C
        NINTLST2=NLIST
        RETURN
        END
C
      !||====================================================================
      !||    ngr2usr                       ../starter/source/system/nintrr.F
      !||--- called by ------------------------------------------------------
      !||    hm_preread_bcscyc             ../starter/source/constraints/general/bcs/lecbcscyc.F
      !||    hm_preread_load_centri        ../starter/source/loads/general/load_centri/hm_preread_load_centri.F
      !||    hm_preread_pblast             ../starter/source/loads/pblast/hm_preread_pblast.F
      !||    hm_read_ale_link              ../starter/source/constraints/ale/hm_read_ale_link_vel.F
      !||    hm_read_alebcs                ../starter/source/constraints/ale/hm_read_alebcs.F
      !||    hm_read_bcs                   ../starter/source/constraints/general/bcs/hm_read_bcs.F
      !||    hm_read_damp                  ../starter/source/general_controls/damping/hm_read_damp.F
      !||    hm_read_ebcs_fluxout          ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_fluxout.F
      !||    hm_read_ebcs_gradp0           ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_gradp0.F
      !||    hm_read_ebcs_inip             ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_inip.F
      !||    hm_read_ebcs_iniv             ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_iniv.F
      !||    hm_read_ebcs_inlet            ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
      !||    hm_read_ebcs_monvol           ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_monvol.F
      !||    hm_read_ebcs_normv            ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_normv.F
      !||    hm_read_ebcs_nrf              ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_nrf.F
      !||    hm_read_ebcs_pres             ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_pres.F
      !||    hm_read_ebcs_valvin           ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_valvin.F
      !||    hm_read_ebcs_valvout          ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_valvout.F
      !||    hm_read_ebcs_vel              ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_vel.F
      !||    hm_read_inter_hertz_type17    ../starter/source/interfaces/int17/hm_read_inter_hertz_type17.F
      !||    hm_read_inter_lagdt_type07    ../starter/source/interfaces/int07/hm_read_inter_lagdt_type07.F
      !||    hm_read_inter_lagmul_type02   ../starter/source/interfaces/int02/hm_read_inter_lagmul_type02.F
      !||    hm_read_inter_lagmul_type07   ../starter/source/interfaces/int07/hm_read_inter_lagmul_type07.F
      !||    hm_read_inter_lagmul_type16   ../starter/source/interfaces/int16/hm_read_inter_lagmul_type16.F
      !||    hm_read_inter_lagmul_type17   ../starter/source/interfaces/int17/hm_read_inter_lagmul_type17.F
      !||    hm_read_inter_type01          ../starter/source/interfaces/int01/hm_read_inter_type01.F
      !||    hm_read_inter_type02          ../starter/source/interfaces/int02/hm_read_inter_type02.F
      !||    hm_read_inter_type03          ../starter/source/interfaces/int03/hm_read_inter_type03.F
      !||    hm_read_inter_type05          ../starter/source/interfaces/int05/hm_read_inter_type05.F
      !||    hm_read_inter_type06          ../starter/source/interfaces/int06/hm_read_inter_type06.F
      !||    hm_read_inter_type07          ../starter/source/interfaces/int07/hm_read_inter_type07.F
      !||    hm_read_inter_type08          ../starter/source/interfaces/int08/hm_read_inter_type08.F
      !||    hm_read_inter_type09          ../starter/source/interfaces/int09/hm_read_inter_type09.F
      !||    hm_read_inter_type10          ../starter/source/interfaces/int10/hm_read_inter_type10.F
      !||    hm_read_inter_type11          ../starter/source/interfaces/int11/hm_read_inter_type11.F
      !||    hm_read_inter_type12          ../starter/source/interfaces/int12/hm_read_inter_type12.F
      !||    hm_read_inter_type14          ../starter/source/interfaces/int14/hm_read_inter_type14.F
      !||    hm_read_inter_type15          ../starter/source/interfaces/int15/hm_read_inter_type15.F
      !||    hm_read_inter_type18          ../starter/source/interfaces/int18/hm_read_inter_type18.F
      !||    hm_read_inter_type20          ../starter/source/interfaces/int20/hm_read_inter_type20.F
      !||    hm_read_inter_type21          ../starter/source/interfaces/int21/hm_read_inter_type21.F
      !||    hm_read_inter_type22          ../starter/source/interfaces/int22/hm_read_inter_type22.F
      !||    hm_read_inter_type23          ../starter/source/interfaces/int23/hm_read_inter_type23.F
      !||    hm_read_inter_type24          ../starter/source/interfaces/int24/hm_read_inter_type24.F
      !||    hm_read_inter_type25          ../starter/source/interfaces/int25/hm_read_inter_type25.F
      !||    hm_read_intsub                ../starter/source/output/subinterface/hm_read_intsub.F
      !||    hm_read_link                  ../starter/source/constraints/rigidlink/hm_read_rlink.F
      !||    hm_read_merge_node            ../starter/source/elements/reader/hm_read_merge_node.F
      !||    hm_read_nbcs                  ../starter/source/constraints/general/bcs/hm_read_nbcs.F
      !||    hm_read_pblast                ../starter/source/loads/pblast/hm_read_pblast.F
      !||    hm_read_pcyl                  ../starter/source/loads/general/load_pcyl/hm_read_pcyl.F
      !||    hm_read_rand                  ../starter/source/general_controls/computation/hm_read_rand.F
      !||    hm_read_rbody                 ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    hm_read_rwall_cyl             ../starter/source/constraints/general/rwall/hm_read_rwall_cyl.F
      !||    hm_read_rwall_lagmul          ../starter/source/constraints/general/rwall/hm_read_rwall_lagmul.F
      !||    hm_read_rwall_paral           ../starter/source/constraints/general/rwall/hm_read_rwall_paral.F
      !||    hm_read_rwall_plane           ../starter/source/constraints/general/rwall/hm_read_rwall_plane.F
      !||    hm_read_rwall_spher           ../starter/source/constraints/general/rwall/hm_read_rwall_spher.F
      !||    hm_read_rwall_therm           ../starter/source/constraints/general/rwall/hm_read_rwall_therm.F
      !||    hm_read_slipring              ../starter/source/tools/seatbelts/hm_read_slipring.F
      !||    hm_read_spcnd                 ../starter/source/constraints/sph/hm_read_spcnd.F
      !||    lectrans                      ../starter/source/model/transformation/lectrans.F
      !||    lectranssub                   ../starter/source/model/submodel/lectranssub.F
      !||    printbcs                      ../starter/source/constraints/general/bcs/printbcs.F
      !||    setrb2on                      ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                        ../starter/source/output/message/message.F
      !||    hm_get_current_option         ../starter/source/devtools/hm_reader/hm_get_current_option.F
      !||--- uses       -----------------------------------------------------
      !||    hm_current_option_mod         ../starter/share/modules1/hm_current_option_mod.F
      !||    message_mod                   ../starter/share/message_module/message_mod.F
      !||====================================================================
      INTEGER FUNCTION NGR2USR(IU,IGR,NGR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE HM_CURRENT_OPTION_MOD
            USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "ngr2usr_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: IU,IGR(*),NGR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IE, ID
      CHARACTER(LEN=NCHARKEY) ::  KEY
      CHARACTER(LEN=NCHARTITLE) :: TITR
C-----------------------------------------------
C   S o u r c e   L i n e s 
C----------------------------------------------- 
      NGR2USR=0
      IF(IU==0)THEN
        NGR2USR=0
        RETURN
      ENDIF
      DO I=1,NGR
        IE=IGR(I)
        IF(IE==IU)THEN
          NGR2USR=I
          RETURN
        ENDIF
      ENDDO
      IF(ISKIP_NGR2USR_ERROR==0) THEN

        CALL HM_GET_CURRENT_OPTION(OPTION_ID = ID,
     .                           OPTION_TITR = TITR,
     .                           KEYWORD1 = KEY)
        CALL ANCMSG(MSGID=2087,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              C1=KEY,
     .              I1=ID,
     .              C2=KEY,
     .              C3=TITR,
     .              I2=IU)
      ENDIF
      ISKIP_NGR2USR_ERROR = 0
      RETURN
      END
C
      !||====================================================================
      !||    ngr2usrn                 ../starter/source/system/nintrr.F
      !||--- called by ------------------------------------------------------
      !||    fail_windshield_init     ../starter/source/materials/fail/windshield_alter/fail_windshield_init.F
      !||    hm_pre_read_link         ../starter/source/constraints/rigidlink/hm_pre_read_rlink.F
      !||    hm_preread_cload         ../starter/source/loads/general/cload/hm_preread_cload.F
      !||    hm_preread_convec        ../starter/source/loads/thermic/hm_preread_convec.F
      !||    hm_preread_grav          ../starter/source/loads/general/grav/hm_preread_grav.F
      !||    hm_preread_impacc        ../starter/source/constraints/general/impvel/hm_preread_impacc.F
      !||    hm_preread_impdisp       ../starter/source/constraints/general/impvel/hm_preread_impdisp.F
      !||    hm_preread_impflux       ../starter/source/constraints/thermic/hm_preread_impflux.F
      !||    hm_preread_imptemp       ../starter/source/constraints/thermic/hm_preread_imptemp.F
      !||    hm_preread_impvel        ../starter/source/constraints/general/impvel/hm_preread_impvel.F
      !||    hm_preread_load_centri   ../starter/source/loads/general/load_centri/hm_preread_load_centri.F
      !||    hm_preread_pload         ../starter/source/loads/general/pload/hm_preread_pload.F
      !||    hm_preread_radiation     ../starter/source/loads/thermic/hm_preread_radiation.F
      !||    hm_preread_rbe2          ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    hm_preread_xelem         ../starter/source/elements/reader/hm_preread_xelem.F
      !||    prelecdet                ../starter/source/initial_conditions/detonation/prelecdet.F
      !||    random_walk_dmg          ../starter/source/materials/fail/fractal/random_walk_dmg.F90
      !||    setrb2on                 ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod              ../starter/share/message_module/message_mod.F
      !||====================================================================
      INTEGER FUNCTION NGR2USRN(IU,IGRNOD,NGRNOD,NUM)
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IU,NGRNOD,NUM
C-----------------------------------------------
      TYPE (GROUP_)  ,DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      NGR2USRN = 0
      IF(IU == 0)THEN
        NUM = 0
        NGR2USRN=0
        RETURN
      ENDIF
      DO I=1,NGRNOD
        IF (IU == IGRNOD(I)%ID) THEN
          NUM = IGRNOD(I)%NENTITY
          NGR2USRN=I
          RETURN
        ENDIF
      ENDDO
C-----------
      RETURN
      END
C
      !||====================================================================
      !||    grsize         ../starter/source/system/nintrr.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      INTEGER FUNCTION GRSIZE(IGU,IGRNOD,GRLEN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGU,GRLEN
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IGS
C-----------------------------------------------
      GRSIZE = 0
      IF (IGU > 0) THEN
        DO I=1,GRLEN
          IF (IGU == IGRNOD(I)%ID) THEN
            GRSIZE = IGRNOD(I)%NENTITY
            IGS = I
            EXIT
          ENDIF
        ENDDO
      ENDIF
C-----------
      RETURN
      END
      !||====================================================================
      !||    grsizen                ../starter/source/system/nintrr.F
      !||--- called by ------------------------------------------------------
      !||    hm_prelecjoi           ../starter/source/constraints/general/cyl_joint/hm_prelecjoi.F
      !||    hm_preread_merge       ../starter/source/constraints/general/merge/hm_preread_merge.F
      !||    hm_preread_rbody       ../starter/source/constraints/general/rbody/hm_preread_rbody.F
      !||    prelecsec              ../starter/source/tools/sect/prelecsec.F
      !||    prelecsec4bolt         ../starter/source/tools/sect/prelecsec4bolt.F
      !||    preread_rbody_lagmul   ../starter/source/constraints/general/rbody/preread_rbody_lagmul.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      INTEGER FUNCTION GRSIZEN(IGU,IGRNOD,GRLEN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGU,GRLEN
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(GRLEN)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IGS
C-----------------------------------------------
      GRSIZEN = 0
      IF (IGU > 0) THEN
        DO I=1,GRLEN
          IF (IGU == IGRNOD(I)%ID) THEN
            GRSIZEN = IGRNOD(I)%NENTITY
            IGS = I
            EXIT
          ENDIF
        ENDDO
      ENDIF
C-----------
      RETURN
      END
      !||====================================================================
      !||    grsize_ele       ../starter/source/system/nintrr.F
      !||--- called by ------------------------------------------------------
      !||    prelecsec        ../starter/source/tools/sect/prelecsec.F
      !||    prelecsec4bolt   ../starter/source/tools/sect/prelecsec4bolt.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      INTEGER FUNCTION GRSIZE_ELE(IGU,IGRELEM,NGRELEM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGU,NGRELEM
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRELEM)  :: IGRELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IGS
C-----------------------------------------------
      GRSIZE_ELE = 0
      IF (IGU > 0) THEN
        DO I=1,NGRELEM
          IF (IGU == IGRELEM(I)%ID) THEN
            GRSIZE_ELE = IGRELEM(I)%NENTITY
            IGS = I
            EXIT
          ENDIF
        ENDDO
      ENDIF
C-----------
      RETURN
      END
      !||====================================================================
      !||    grsize_ele_trans   ../starter/source/system/nintrr.F
      !||--- called by ------------------------------------------------------
      !||    lecsec42           ../starter/source/tools/sect/hm_read_sect.F
      !||    prelecsec          ../starter/source/tools/sect/prelecsec.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      INTEGER FUNCTION GRSIZE_ELE_TRANS(IGU,IGRELEM,NGRELEM,SEATBELT_SHELL_TO_SPRING)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN)::IGU,NGRELEM
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRELEM)  , INTENT(IN) :: IGRELEM
      INTEGER,INTENT(IN)::SEATBELT_SHELL_TO_SPRING(NUMELC,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IE
C-----------------------------------------------
      GRSIZE_ELE_TRANS = 0
      IF (IGU > 0) THEN
        DO I=1,NGRELEM
          IF (IGU == IGRELEM(I)%ID) THEN
            DO J=1,IGRELEM(I)%NENTITY
              IE=IGRELEM(I)%ENTITY(J)
              IF(SEATBELT_SHELL_TO_SPRING(IE,1) /= 0)
     .                  GRSIZE_ELE_TRANS = GRSIZE_ELE_TRANS + 1
              IF(SEATBELT_SHELL_TO_SPRING(IE,2) /= 0)
     .                  GRSIZE_ELE_TRANS = GRSIZE_ELE_TRANS + 1
            ENDDO
          ENDIF
        ENDDO
      ENDIF
C-----------
      RETURN
      END
      !||====================================================================
      !||    sortgroup   ../starter/source/system/nintrr.F
      !||--- called by ------------------------------------------------------
      !||    lectur      ../starter/source/starter/lectur.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE SORTGROUP(
     1 IXS_S ,IXS_S_IND,IXQ_S,IXQ_S_IND,IXC_S,IXC_S_IND,
     2 IXT_S ,IXT_S_IND,IXP_S,IXP_S_IND,IXR_S,IXR_S_IND,
     3 IXTG_S,IXTG_S_IND,IXS,IXQ,IXC,IXT,IXP,IXR,IXTG,LENI)
C      GROUP SORTING vs USER ID
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS_S(*),IXS_S_IND(*),IXQ_S(*),IXQ_S_IND(*),IXC_S(*),
     1        IXC_S_IND(*),IXT_S(*),IXT_S_IND(*),IXP_S(*),
     2        IXP_S_IND(*),IXR_S(*),IXR_S_IND(*),
     3        IXTG_S(*),IXTG_S_IND(*),
     4        IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*),
     5        IXT(NIXT,*), IXP(NIXP,*), IXR(NIXR,*), IXTG(NIXTG,*),
     6        LENI
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,NEL,NOLD,K,
     .        IWORK(70000)
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX ! LENI*2
C-----------------------
        ALLOCATE(INDEX(LENI*2))
        DO I=1,NUMELS  
          IXS_S_IND(I) = IXS(NIXS,I)  
        ENDDO  
        CALL MY_ORDERS(0,IWORK,IXS_S_IND,INDEX,NUMELS,1)  
        DO I=1,NUMELS  
          IXS_S(I) = IXS_S_IND(INDEX(I))  
        ENDDO  
        DO I=1,NUMELS  
          IXS_S_IND(I) = INDEX(I)  
        ENDDO  

        DO I=1,NUMELQ  
          IXQ_S_IND(I) = IXQ(NIXQ,I)  
        ENDDO  
        CALL MY_ORDERS(0,IWORK,IXQ_S_IND,INDEX,NUMELQ,1)  
        DO I=1,NUMELQ  
          IXQ_S(I) = IXQ_S_IND(INDEX(I))  
        ENDDO  
        DO I=1,NUMELQ  
          IXQ_S_IND(I) = INDEX(I)  
        ENDDO  
        
        DO I=1,NUMELC  
          IXC_S_IND(I) = IXC(NIXC,I)  
        ENDDO  
        CALL MY_ORDERS(0,IWORK,IXC_S_IND,INDEX,NUMELC,1)  
        DO I=1,NUMELC  
          IXC_S(I) = IXC_S_IND(INDEX(I))  
        ENDDO  
        DO I=1,NUMELC  
          IXC_S_IND(I) = INDEX(I)  
        ENDDO  
       
        DO I=1,NUMELT  
          IXT_S_IND(I) = IXT(NIXT,I)  
        ENDDO  
        CALL MY_ORDERS(0,IWORK,IXT_S_IND,INDEX,NUMELT,1)  
        DO I=1,NUMELT  
          IXT_S(I) = IXT_S_IND(INDEX(I))  
        ENDDO  
        DO I=1,NUMELT  
          IXT_S_IND(I) = INDEX(I)  
        ENDDO  

        DO I=1,NUMELP  
          IXP_S_IND(I) = IXP(NIXP,I)  
        ENDDO  
        CALL MY_ORDERS(0,IWORK,IXP_S_IND,INDEX,NUMELP,1)  
        DO I=1,NUMELP  
          IXP_S(I) = IXP_S_IND(INDEX(I))  
        ENDDO  
        DO I=1,NUMELP  
          IXP_S_IND(I) = INDEX(I)  
        ENDDO  

        DO I=1,NUMELR  
          IXR_S_IND(I) = IXR(NIXR,I)  
        ENDDO  
        CALL MY_ORDERS(0,IWORK,IXR_S_IND,INDEX,NUMELR,1)  
        DO I=1,NUMELR  
          IXR_S(I) = IXR_S_IND(INDEX(I))  
        ENDDO  
        DO I=1,NUMELR  
          IXR_S_IND(I) = INDEX(I)  
        ENDDO  
        
        DO I=1,NUMELTG  
          IXTG_S_IND(I) = IXTG(NIXTG,I)  
        ENDDO  
        CALL MY_ORDERS(0,IWORK,IXTG_S_IND,INDEX,NUMELTG,1)  
        DO I=1,NUMELTG  
          IXTG_S(I) = IXTG_S_IND(INDEX(I))  
        ENDDO  
        DO I=1,NUMELTG  
          IXTG_S_IND(I) = INDEX(I)  
        ENDDO  
        DEALLOCATE(INDEX) 
        RETURN
        END

      !||====================================================================
      !||    user2sys   ../starter/source/system/nintrr.F
      !||--- called by ------------------------------------------------------
      !||    nintlstn   ../starter/source/system/nintrr.F
      !||====================================================================
      INTEGER FUNCTION USER2SYS(IU,IXX_S,NIX,CUR,LAST)
C      FONCTION DONNE N0 SYSTEME D'UNE LISTE D'ELEMENTS USER OR ZERO IF NOT FOUND
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIX,NUMEL, CUR, LAST
      INTEGER IU,IXX_S(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, JINF, JSUP
C-----------------------

      IF (LAST==0) THEN
        USER2SYS=0
        RETURN
      END IF
      JINF=CUR
      JSUP=LAST
      J=MIN(CUR,(LAST+CUR)/2)
   10 IF(JSUP<=JINF.AND.(IU-IXX_S(J))/=0) THEN
C     >CAS ELEM non trouve
        USER2SYS=0
        RETURN
      ENDIF
      IF((IU-IXX_S(J))==0)THEN
C     >CAS IU=TABM FIN DE LA RECHERCHE
         USER2SYS=J
         RETURN
      ELSE IF (IU-IXX_S(J)<0) THEN
C     >CAS IU<TABM
         JSUP=J-1
      ELSE
C     >CAS IU>TABM
         JINF=J+1
      ENDIF
      J=(JSUP+JINF)/2
      GO TO 10
      
      RETURN
      END

C
        
      !||====================================================================
      !||    nintlstn               ../starter/source/system/nintrr.F
      !||--- called by ------------------------------------------------------
      !||    hm_lecgre              ../starter/source/groups/hm_lecgre.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||    user2sys               ../starter/source/system/nintrr.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||====================================================================
      INTEGER FUNCTION NINTLSTN(LIST,NLIST,IXX_S,NIX,NUMEL,MESS,
     .                          IXX_S_IND,INDEX,TYPE,ID,TITR)
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C      FONCTION DONNE N0 SYSTEME D'UNE LISTE D'ELEMENTS USER
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NLIST,NIX,NUMEL
      CHARACTER MESS*40
      INTEGER LIST(*),IXX_S(*),INDEX(*),IXX_S_IND(*)
      INTEGER ID
      CHARACTER(LEN=NCHARTITLE) :: TITR
      CHARACTER TYPE*4
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,NEL,NOLD,K,
     .        IWORK(70000)
      INTEGER USER2SYS
C-----------------------
C TRI DE LIST EN ORDRE CROISSANT
C AVEC SUPPRESSION DES No DOUBLES
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
        NEL=K
C-----------------------
C RECHERCHE DES ELEMENTS DE LIST() DANS IXX_S (sorted)
C-----------------------
        J=0
        DO I=1,NEL
          J=USER2SYS(LIST(I),IXX_S,NIX,J+1,NUMEL)
          IF(J /= 0)THEN
            LIST(I)=IXX_S_IND(J)
          ELSE
            CALL ANCMSG(MSGID=70,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  C1=TYPE,
     .                  I1=ID,
     .                  C2=TITR,
     .                  I2=LIST(I))
            NINTLSTN=I-1
            RETURN
          ENDIF
        ENDDO
C
        NINTLSTN=NEL
        
        RETURN
        END
C
      !||====================================================================
      !||    nintrigr         ../starter/source/system/nintrr.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_thpart   ../starter/source/output/thpart/hm_read_thpart.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      INTEGER FUNCTION NINTRIGR(IEXT,IGR,NGR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IEXT,NGR
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGR)  :: IGR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IE
C-----------------------------------------------
      DO I=1,NGR
        IE=IGR(I)%ID
        IF(IE==IEXT)THEN
          NINTRIGR=I
          RETURN
        ENDIF
      ENDDO
      NINTRIGR=0
!
      RETURN
      END
