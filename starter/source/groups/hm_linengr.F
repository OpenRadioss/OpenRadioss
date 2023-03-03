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
Chd|  HM_LINENGR                    source/groups/hm_linengr.F    
Chd|-- called by -----------
Chd|        HM_LECGRN                     source/groups/hm_lecgrn.F     
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_LINENGR(ID       ,IGRSLIN  ,TAGBUF   ,TITR     ,NSETS    ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ID,TAGBUF(*),NSETS
      CHARACTER*nchartitle,
     .   TITR
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
      TYPE (SURF_)  , DIMENSION(NSLIN+NSETS) :: IGRSLIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,L,IE,JREC,K,ISU,JJ,LL,N,KK,NENTITY
      LOGICAL IS_AVAILABLE
C=======================================================================
      IS_AVAILABLE = .FALSE.
      CALL HM_GET_INTV('idsmax' ,NENTITY,IS_AVAILABLE,LSUBMODEL)
      DO KK = 1,NENTITY
        CALL HM_GET_INT_ARRAY_INDEX ('ids',JJ  ,KK,IS_AVAILABLE,LSUBMODEL)
        IF (JJ /= 0) THEN
          ISU=0
          DO K=1,NSLIN
            IF (JJ == IGRSLIN(K)%ID) ISU=K
          ENDDO
          IF (ISU /= 0) THEN
            DO LL=1,IGRSLIN(ISU)%NSEG
              DO K=1,2
                N=IGRSLIN(ISU)%NODES(LL,K)
C                 tag les noeuds DU SEGMENT 
                IF (N /= 0) TAGBUF(N)=1
              ENDDO
            ENDDO
          ELSE
            CALL ANCMSG(MSGID=193,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=JJ)
          ENDIF
        ENDIF
      ENDDO
C-----------
      RETURN
      END
