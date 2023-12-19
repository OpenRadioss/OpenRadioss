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
Chd|  NEW_SEATBELT                  source/tools/seatbelts/new_seatbelt.F
Chd|-- called by -----------
Chd|        CREATE_SEATBELT               source/tools/seatbelts/create_seatbelt.F
Chd|        INI_SEATBELT                  source/tools/seatbelts/ini_seatbelt.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE NEW_SEATBELT(IXR,ITAB,KNOD2EL1D,NOD2EL1D,NOD_START,
     .                        ELEM_CUR,TAG_RES,TAG_NOD,ID,FLAG,
     .                        NNOD,IPM,NB_ELEM_1D,NB_BRANCH,BRANCH_TAB,
     .                        BRANCH_CPT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE SEATBELT_MOD
      USE SUBMODEL_MOD , ONLY : NSUBMOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXR(NIXR,*),ITAB(*),KNOD2EL1D(*),NOD2EL1D(*),NOD_START,TAG_RES(*),TAG_NOD(*),
     .        ELEM_CUR,ID,FLAG,NNOD,IPM(NPROPMI,*)
      INTEGER, INTENT(IN)    :: NB_ELEM_1D
      INTEGER, INTENT(INOUT) :: NB_BRANCH,BRANCH_TAB(2*NB_ELEM_1D),BRANCH_CPT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,NODE_CUR,NRES_FOUND,ELEM_NEWT,NODE_NEXT,ELEM_NEXT,ID_PREV,MTYP,MID,ELEM_TEST
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C
C--   Loop to find elements of the seatbelt 
C
      NODE_CUR = NOD_START
      ELEM_NEXT = 0
      IF (FLAG == 0) TAG_RES(ELEM_CUR) = ID
      TAG_NOD(IXR(2,ELEM_CUR)) = ID
      TAG_NOD(IXR(3,ELEM_CUR)) = ID
      NNOD = NNOD + 1
      NRES_FOUND = 1
C
      DO WHILE (NRES_FOUND > 0)
        NRES_FOUND = 0
C
        IF (IXR(2,ELEM_CUR) == NODE_CUR) THEN
          NODE_NEXT = IXR(3,ELEM_CUR)
        ELSE
          NODE_NEXT = IXR(2,ELEM_CUR)
        ENDIF
C     
        DO K=KNOD2EL1D(NODE_NEXT)+1,KNOD2EL1D(NODE_NEXT+1)
          IF ((NOD2EL1D(K) > NUMELT+NUMELP).AND.(NOD2EL1D(K) /= ELEM_CUR+NUMELT+NUMELP)) THEN
            ELEM_TEST = NOD2EL1D(K)-NUMELT-NUMELP
            MID = IXR(5,ELEM_TEST)
            IF (MID > 0) THEN
              MTYP = IPM(2,MID)
              IF ((MTYP == 114).AND.(TAG_RES(ELEM_TEST) == 0)) THEN
                NRES_FOUND = NRES_FOUND + 1
                IF(NRES_FOUND > 1) THEN
                  IF (FLAG > 0) THEN
C--                 loop inisde retractor : bifurcation is not allowded inside retractor or at entry
                    CALL ANCMSG(MSGID=2005,
     .                         MSGTYPE=MSGERROR,
     .                         ANMODE=ANINFO,
     .                         I1=ITAB(NODE_NEXT))
                    NRES_FOUND = 0
                  ELSE
C--                 start of the secondary branch is saved
                    NB_BRANCH = NB_BRANCH + 1
                    BRANCH_CPT = BRANCH_CPT + 1
                    CALL ANCMSG(MSGID=2098,
     .                          MSGTYPE=MSGWARNING,
     .                          ANMODE=ANINFO,
     .                          I1=ITAB(NODE_NEXT))
                    BRANCH_TAB(2*(BRANCH_CPT-1)+1) = NODE_NEXT
                    BRANCH_TAB(2*(BRANCH_CPT-1)+2) = ELEM_TEST
                  ENDIF
                ELSE
                  ELEM_NEXT = ELEM_TEST
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
C
        IF (NRES_FOUND > 0) THEN
          IF (FLAG == 0) THEN
            TAG_RES(ELEM_NEXT) = ID
            TAG_NOD(IXR(2,ELEM_NEXT)) = ID
            TAG_NOD(IXR(3,ELEM_NEXT)) = ID
            NNOD = NNOD + 1
          ELSE
            IF (TAG_RES(ELEM_NEXT) > 0) THEN
              ID_PREV = RETRACTOR(TAG_RES(ELEM_NEXT))%ID
              IF ((ID_PREV > 0).AND.(NRES_FOUND > 0)) CALL ANCMSG(MSGID=2010,
     .                                            MSGTYPE=MSGERROR,
     .                                            ANMODE=ANINFO,
     .                                            I1=ID_PREV,I2=IXR(NIXR,ELEM_NEXT),I3=RETRACTOR(ID)%ID)
            ENDIF
            TAG_RES(ELEM_NEXT) = ID
            TAG_NOD(IXR(2,ELEM_NEXT)) = ID
            TAG_NOD(IXR(3,ELEM_NEXT)) = ID
          ENDIF
        ENDIF
C 
        ELEM_CUR = ELEM_NEXT
        NODE_CUR = NODE_NEXT
      ENDDO                      
C
      END SUBROUTINE NEW_SEATBELT

