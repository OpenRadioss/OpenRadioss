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
!||    hm_preread_impdisp     ../starter/source/constraints/general/impvel/hm_preread_impdisp.F
!||--- called by ------------------------------------------------------
!||    lectur                 ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    hm_get_intv            ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_count        ../starter/source/devtools/hm_reader/hm_option_count.F
!||    hm_option_read_key     ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start        ../starter/source/devtools/hm_reader/hm_option_start.F
!||    ngr2usrn               ../starter/source/system/nintrr.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_PREREAD_IMPDISP(NIMPDISP ,IGRNOD  ,IPART  ,IPARTR  ,
     .                              UNITAB   ,LSUBMODEL)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE UNITAB_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(OUT)   :: NIMPDISP
      INTEGER IPART(LIPART1,*), IPARTR(*)
      TYPE(UNIT_TYPE_) ,INTENT(IN) :: UNITAB
      TYPE (GROUP_)      , DIMENSION(NGRNOD)  ,INTENT(IN) :: IGRNOD
      TYPE(SUBMODEL_DATA), DIMENSION(*)       ,INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,N,OPTID,NFDISP,NFGEO,IGS,GRNOD_ID,PART_ID,NNOD,JPART
      CHARACTER(LEN=NCHARKEY)   :: KEY
      CHARACTER(LEN=NCHARTITLE) :: TITR
      LOGICAL :: IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NGR2USRN
      EXTERNAL NGR2USRN
c--------------------------------------------------
c     Count number of nodes with imposed displacement => NIMPDISP
C======================================================================|
      IS_AVAILABLE = .FALSE.
c
      NIMPDISP = 0
c--------------------------------------------------
c     /IMPDISP
c--------------------------------------------------
      CALL HM_OPTION_COUNT('/IMPDISP',NFDISP)
c
      CALL HM_OPTION_START('/IMPDISP')
c
      DO I=1,NFDISP
c
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = OPTID,
     .                          OPTION_TITR = TITR,
     .                          KEYWORD2    = KEY)
c
        IF (KEY(1:4) /= 'FGEO') THEN
          CALL HM_GET_INTV('entityid' ,GRNOD_ID ,IS_AVAILABLE,LSUBMODEL)
          IGS = NGR2USRN(GRNOD_ID,IGRNOD,NGRNOD,NNOD)
          IF (IGS > 0) NIMPDISP = NIMPDISP + NNOD                                         
        ENDIF
c
      ENDDO ! DO I=1,NFDISP
c--------------------------      
c     /IMPDISP/FGEO    
c--------------------------         
      CALL HM_OPTION_COUNT('/IMPDISP/FGEO',NFGEO)
c
      CALL HM_OPTION_START('/IMPDISP/FGEO')
c     
      DO I=1,NFGEO
c
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = OPTID,
     .                          OPTION_TITR = TITR,
     .                          KEYWORD2    = KEY)
        IF (KEY(1:4) == 'FGEO') THEN
          CALL HM_GET_INTV('rad_spring_part' ,PART_ID ,IS_AVAILABLE,LSUBMODEL)
          IF (PART_ID > 0) THEN
            JPART = 0
            DO N=1,NPART
              IF (IPART(4,N) == PART_ID) JPART = N
            ENDDO
            IF (JPART == 0) THEN
             CALL ANCMSG(MSGID=1077, MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   I1=OPTID,
     .                   C1=TITR,
     .                   I2=PART_ID)
            ENDIF
            DO N=1,NUMELR
              IF (IPARTR(N) == JPART) NIMPDISP = NIMPDISP + 1
            ENDDO
          ENDIF
c
          CALL HM_GET_INTV('distribution_table_count' ,NNOD ,IS_AVAILABLE,LSUBMODEL)
c
          NIMPDISP = NIMPDISP + NNOD
        ENDIF
      ENDDO ! DO I=1,NFGEO
c-----------
      RETURN
      END
