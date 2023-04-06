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
Chd|  NINTRI                        source/system/nintrr.F        
Chd|-- called by -----------
Chd|        ALE_EULER_INIT                source/materials/ale/ale_euler_init.F
Chd|        FILL_BUFFER_51                source/materials/mat/mat051/fill_buffer_51.F
Chd|        FSDCOD                        source/system/fsdcod.F        
Chd|        HM_PREREAD_PART               source/model/assembling/hm_read_part.F
Chd|        HM_READ_EREF                  source/loads/reference_state/eref/hm_read_eref.F
Chd|        HM_READ_INJECT1               source/properties/injector/hm_read_inject1.F
Chd|        HM_READ_INJECT2               source/properties/injector/hm_read_inject2.F
Chd|        HM_READ_PART                  source/model/assembling/hm_read_part.F
Chd|        HM_READ_RETRACTOR             source/tools/seatbelts/hm_read_retractor.F
Chd|        HM_READ_SLIPRING              source/tools/seatbelts/hm_read_slipring.F
Chd|        LECINT                        source/interfaces/interf1/lecint.F
Chd|        NBADIGEMESH                   source/elements/ige3d/nbadigemesh.F
Chd|-- calls ---------------
Chd|====================================================================
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
Chd|====================================================================
Chd|  NINTLST                       source/system/nintrr.F        
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      INTEGER FUNCTION NINTLST (LIST,NLIST,IX,NIX,NUMEL,MESS,
     .                          IX1,IX2,INDEX,KK,
     .                          TYPE,ID,TITR)
      USE MESSAGE_MOD
C      FONCTION DONNE N0 SYSTEME D'UNE LISTE D'ELEMENTS USER
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NLIST,KK,NIX,NUMEL
      CHARACTER MESS*40
      INTEGER LIST(*),IX(NIX,*),INDEX(*),IX1(*),IX2(*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TYPE,TITR
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
Chd|====================================================================
Chd|  NINTLST2                      source/system/nintrr.F        
Chd|-- called by -----------
Chd|        STATE_ADMESH                  source/model/remesh/state_admesh.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
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
Chd|====================================================================
Chd|  NGR2USR                       source/system/nintrr.F        
Chd|-- called by -----------
Chd|        HM_PREREAD_BCSCYC             source/constraints/general/bcs/lecbcscyc.F
Chd|        HM_PREREAD_PBLAST             source/loads/pblast/hm_preread_pblast.F
Chd|        HM_READ_ALEBCS                source/constraints/ale/hm_read_alebcs.F
Chd|        HM_READ_ALE_LINK              source/constraints/ale/hm_read_ale_link_vel.F
Chd|        HM_READ_BCS                   source/constraints/general/bcs/hm_read_bcs.F
Chd|        HM_READ_DAMP                  source/general_controls/damping/hm_read_damp.F
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
Chd|        HM_READ_INTER_HERTZ_TYPE17    source/interfaces/int17/hm_read_inter_hertz_type17.F
Chd|        HM_READ_INTER_LAGDT_TYPE07    source/interfaces/int07/hm_read_inter_lagdt_type07.F
Chd|        HM_READ_INTER_LAGMUL_TYPE02   source/interfaces/int02/hm_read_inter_lagmul_type02.F
Chd|        HM_READ_INTER_LAGMUL_TYPE07   source/interfaces/int07/hm_read_inter_lagmul_type07.F
Chd|        HM_READ_INTER_LAGMUL_TYPE16   source/interfaces/int16/hm_read_inter_lagmul_type16.F
Chd|        HM_READ_INTER_LAGMUL_TYPE17   source/interfaces/int17/hm_read_inter_lagmul_type17.F
Chd|        HM_READ_INTER_TYPE01          source/interfaces/int01/hm_read_inter_type01.F
Chd|        HM_READ_INTER_TYPE02          source/interfaces/int02/hm_read_inter_type02.F
Chd|        HM_READ_INTER_TYPE03          source/interfaces/int03/hm_read_inter_type03.F
Chd|        HM_READ_INTER_TYPE05          source/interfaces/int05/hm_read_inter_type05.F
Chd|        HM_READ_INTER_TYPE06          source/interfaces/int06/hm_read_inter_type06.F
Chd|        HM_READ_INTER_TYPE07          source/interfaces/int07/hm_read_inter_type07.F
Chd|        HM_READ_INTER_TYPE08          source/interfaces/int08/hm_read_inter_type08.F
Chd|        HM_READ_INTER_TYPE09          source/interfaces/int09/hm_read_inter_type09.F
Chd|        HM_READ_INTER_TYPE10          source/interfaces/int10/hm_read_inter_type10.F
Chd|        HM_READ_INTER_TYPE11          source/interfaces/int11/hm_read_inter_type11.F
Chd|        HM_READ_INTER_TYPE12          source/interfaces/int12/hm_read_inter_type12.F
Chd|        HM_READ_INTER_TYPE14          source/interfaces/int14/hm_read_inter_type14.F
Chd|        HM_READ_INTER_TYPE15          source/interfaces/int15/hm_read_inter_type15.F
Chd|        HM_READ_INTER_TYPE18          source/interfaces/int18/hm_read_inter_type18.F
Chd|        HM_READ_INTER_TYPE20          source/interfaces/int20/hm_read_inter_type20.F
Chd|        HM_READ_INTER_TYPE21          source/interfaces/int21/hm_read_inter_type21.F
Chd|        HM_READ_INTER_TYPE22          source/interfaces/int22/hm_read_inter_type22.F
Chd|        HM_READ_INTER_TYPE23          source/interfaces/int23/hm_read_inter_type23.F
Chd|        HM_READ_INTER_TYPE24          source/interfaces/int24/hm_read_inter_type24.F
Chd|        HM_READ_INTER_TYPE25          source/interfaces/int25/hm_read_inter_type25.F
Chd|        HM_READ_INTSUB                source/output/subinterface/hm_read_intsub.F
Chd|        HM_READ_LINK                  source/constraints/rigidlink/hm_read_rlink.F
Chd|        HM_READ_PBLAST                source/loads/pblast/hm_read_pblast.F
Chd|        HM_READ_RAND                  source/general_controls/computation/hm_read_rand.F
Chd|        HM_READ_RBODY                 source/constraints/general/rbody/hm_read_rbody.F
Chd|        HM_READ_RWALL_CYL             source/constraints/general/rwall/hm_read_rwall_cyl.F
Chd|        HM_READ_RWALL_LAGMUL          source/constraints/general/rwall/hm_read_rwall_lagmul.F
Chd|        HM_READ_RWALL_PARAL           source/constraints/general/rwall/hm_read_rwall_paral.F
Chd|        HM_READ_RWALL_PLANE           source/constraints/general/rwall/hm_read_rwall_plane.F
Chd|        HM_READ_RWALL_SPHER           source/constraints/general/rwall/hm_read_rwall_spher.F
Chd|        HM_READ_RWALL_THERM           source/constraints/general/rwall/hm_read_rwall_therm.F
Chd|        HM_READ_SLIPRING              source/tools/seatbelts/hm_read_slipring.F
Chd|        HM_READ_SPCND                 source/constraints/sph/hm_read_spcnd.F
Chd|        LECTRANS                      source/model/transformation/lectrans.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_CURRENT_OPTION         source/devtools/hm_reader/hm_get_current_option.F
Chd|        HM_CURRENT_OPTION_MOD         share/modules1/hm_current_option_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      INTEGER FUNCTION NGR2USR(IU,IGR,NGR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE HM_CURRENT_OPTION_MOD
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
      CHARACTER KEY*ncharkey,TITR*nchartitle
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
Chd|====================================================================
Chd|  NGR2USRN                      source/system/nintrr.F        
Chd|-- called by -----------
Chd|        FAIL_WINDSHIELD_INIT          source/materials/fail/windshield_alter/fail_windshield_init.F
Chd|        HM_PREREAD_CLOAD              source/loads/general/cload/hm_preread_cload.F
Chd|        HM_PREREAD_GRAV               source/loads/general/grav/hm_preread_grav.F
Chd|        HM_PREREAD_IMPACC             source/constraints/general/impvel/hm_preread_impacc.F
Chd|        HM_PREREAD_IMPDISP            source/constraints/general/impvel/hm_preread_impdisp.F
Chd|        HM_PREREAD_IMPVEL             source/constraints/general/impvel/hm_preread_impvel.F
Chd|        HM_PREREAD_LOAD_CENTRI        source/loads/general/load_centri/hm_preread_load_centri.F
Chd|        HM_PREREAD_RBE2               source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        HM_PREREAD_XELEM              source/elements/reader/hm_preread_xelem.F
Chd|        HM_PRE_READ_LINK              source/constraints/rigidlink/hm_pre_read_rlink.F
Chd|        PRELECDET                     source/initial_conditions/detonation/prelecdet.F
Chd|        SETRB2ON                      source/constraints/general/rbe2/hm_read_rbe2.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
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
Chd|====================================================================
Chd|  GRSIZE                        source/system/nintrr.F        
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
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
Chd|====================================================================
Chd|  GRSIZEN                       source/system/nintrr.F        
Chd|-- called by -----------
Chd|        HM_PRELECJOI                  source/constraints/general/cyl_joint/hm_prelecjoi.F
Chd|        HM_PREREAD_MERGE              source/constraints/general/merge/hm_preread_merge.F
Chd|        HM_PREREAD_RBODY              source/constraints/general/rbody/hm_preread_rbody.F
Chd|        PRELECSEC                     source/tools/sect/prelecsec.F 
Chd|        PRELECSEC4BOLT                source/tools/sect/prelecsec4bolt.F
Chd|        PREREAD_RBODY_LAGMUL          source/constraints/general/rbody/preread_rbody_lagmul.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
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
Chd|====================================================================
Chd|  GRSIZE_ELE                    source/system/nintrr.F        
Chd|-- called by -----------
Chd|        PRELECSEC                     source/tools/sect/prelecsec.F 
Chd|        PRELECSEC4BOLT                source/tools/sect/prelecsec4bolt.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
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
Chd|====================================================================
Chd|  GRSIZE_ELE_TRANS              source/system/nintrr.F        
Chd|-- called by -----------
Chd|        LECSEC42                      source/tools/sect/hm_read_sect.F
Chd|        PRELECSEC                     source/tools/sect/prelecsec.F 
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
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
Chd|====================================================================
Chd|  SORTGROUP                     source/system/nintrr.F        
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|====================================================================
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
     .        IWORK(70000),
     .        INDEX(LENI*2) 
C-----------------------
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
          
        RETURN
        END

Chd|====================================================================
Chd|  USER2SYS                      source/system/nintrr.F        
Chd|-- called by -----------
Chd|        NINTLSTN                      source/system/nintrr.F        
Chd|-- calls ---------------
Chd|====================================================================
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
        
Chd|====================================================================
Chd|  NINTLSTN                      source/system/nintrr.F        
Chd|-- called by -----------
Chd|        HM_LECGRE                     source/groups/hm_lecgre.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        USER2SYS                      source/system/nintrr.F        
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      INTEGER FUNCTION NINTLSTN(LIST,NLIST,IXX_S,NIX,NUMEL,MESS,
     .                          IXX_S_IND,INDEX,TYPE,ID,TITR)
      USE MESSAGE_MOD
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
      CHARACTER*nchartitle,
     .   TITR
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
Chd|====================================================================
Chd|  NINTRIGR                      source/system/nintrr.F        
Chd|-- called by -----------
Chd|        HM_READ_THPART                source/output/thpart/hm_read_thpart.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
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
