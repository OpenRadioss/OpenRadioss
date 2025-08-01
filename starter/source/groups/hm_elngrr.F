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
!||    hm_elngrr                ../starter/source/groups/hm_elngrr.F
!||--- called by ------------------------------------------------------
!||    hm_lecgrn                ../starter/source/groups/hm_lecgrn.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_int_array_index   ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_ELNGRR(IXR      ,GEO      ,NGRELE   ,ID       ,
     .                     IGRELEM  ,TAGBUF   ,TITR     ,FLAG     ,
     .                     LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXR(NIXR,*),TAGBUF(*)
      INTEGER NGRELE,ID,FLAG
      MY_REAL
     .      GEO(NPROPG,*)
      CHARACTER(LEN=NCHARTITLE) :: TITR
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRELE) :: IGRELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,L,IE,IADV,JREC,K,ISU,NIX2,IPIDT,KK,JJ,NEMAX
      LOGICAL IS_AVAILABLE
C=======================================================================
      IS_AVAILABLE = .FALSE.
      CALL HM_GET_INTV('idsmax' ,NEMAX,IS_AVAILABLE,LSUBMODEL)
      DO KK = 1,NEMAX
        CALL HM_GET_INT_ARRAY_INDEX ('ids',JJ  ,KK,IS_AVAILABLE,LSUBMODEL)       
        IF (JJ /= 0) THEN
          ISU=0
          DO K=1,NGRELE
            IF (JJ == IGRELEM(K)%ID) ISU=K
          ENDDO
          IF (ISU /= 0) THEN
            DO L=1,IGRELEM(ISU)%NENTITY
              IE=IGRELEM(ISU)%ENTITY(L)
              IPIDT=NINT(GEO(1,IXR(1,IE)))
              IF(IPIDT == 12)THEN
                NIX2=4
              ELSE
                NIX2=3
              ENDIF
              DO K=2,NIX2
C                 tag les noeuds connectes a l'element 
                TAGBUF(IXR(K,IE))=1
              ENDDO
            ENDDO
          ELSEIF (FLAG == 0) THEN
            CALL ANCMSG(MSGID=172,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO,
     .                  I1=ID,C1=TITR,
     .                  C2='GRSPRI',
     .                  I2=JJ)
          ENDIF
        ENDIF
      ENDDO
C-----------
      RETURN
      END
