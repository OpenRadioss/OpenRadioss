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
Chd|====================================================================
Chd|  MAT11CHECK                    source/materials/mat/mat011/mat11check.F
Chd|-- called by -----------
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE MAT11CHECK(PM,NIX,IX,ALE_CONNECTIVITY,NUMEL,JALE_FROM_PROP,NEL,NFT,MAT_ID,NUMMAT,NPROPM)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutines is checking if material boundary
C law11 has compatible material law with adjacent element
C
C This subroutine is called when group of elem (LFT,LLT)
C is associated with material law 11
C
C NUMEL            : total number of solid elements (2d or 3d)
C IX               : element connectivity + mat_id (size NIX,NUMEL)
C PM               : material properties (real)
C ALE_CONNECTIVITY : connectivity buffer (ALE)
C JALE_FROM_PROP   : property flag which set ALE or EULER
C NEL              : number of element in current group
C MFT              : shift index to retrieve global internal identifier
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NUMEL,NIX,IX(NIX,NUMEL),JALE_FROM_PROP,NEL,NFT
      INTEGER,INTENT(IN) :: MAT_ID,NUMMAT, NPROPM
      my_real,INTENT(IN) :: PM(NPROPM,NUMMAT)
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IVMAT,IMAT, ELEM_UID, MLN, J, J2, IE, IEV, K,IAD1,LGTH
      INTEGER lFOUND_monomat, lFOUND_multimat
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        IF(JALE_FROM_PROP == 0)THEN
          CALL ANCMSG(MSGID = 122,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                I1    = MAT_ID,
     .                C1    = "MATERIAL LAW 11 MUST BE DEFINED WITH ALE OR EULER FRAMEWORK" )
        ENDIF

        DO I=1,NEL
          IE = I + NFT
          ELEM_UID = IX(NIX,IE)
          IMAT = IX(1,IE)
          lFOUND_monomat = 0
          lFOUND_multimat = 0
          !loop over all adjacent elems
          IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
          LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(IE+1) - IAD1
          DO J=1,LGTH
            IEV = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J - 1)
            IF(IEV == 0)CYCLE
            IVMAT = ABS(IX(1,IEV))
            MLN = NINT(PM(19,IVMAT))
            IF(MLN == 51 .OR. MLN == 151 .OR. MLN == 37)lFOUND_multimat = lFOUND_multimat + 1
            IF(MLN /= 11)lFOUND_monomat = lFOUND_monomat + 1
          ENDDO! next J

          IF(lFOUND_multimat /= 0)THEN
              CALL ANCMSG(MSGID = 122,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                    I1    = ELEM_UID,
     .                    C1    = "MATERIAL LAW 11 IS NOT COMPATIBLE WITH MULTIMATERIAL FORMULATION" )
          ENDIF
          IF(lFOUND_monomat == 0)THEN
              CALL ANCMSG(MSGID = 123,MSGTYPE=MSGWARNING,ANMODE=ANINFO,
     .                    I1    = ELEM_UID,
     .                    C1    = "HAS NO ADJACENT FACE IN COMPUTATION DOMAIN (MATERIAL LAW11)" )
          ENDIF
          IF(lFOUND_monomat >= 2)THEN
              CALL ANCMSG(MSGID = 122,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                    I1    = ELEM_UID,
     .                    C1    = "MUST HAVE ONLY ONE FACE ADJACENT TO COMPUTATION DOMAIN (MATERIAL LAW11)" )
          ENDIF
        ENDDO !next I
C-----------------------------------------------
      RETURN
      END SUBROUTINE MAT11CHECK
