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
Chd|  TAGNOD_R2R_NL                 source/coupling/rad2rad/tagnod_r2r_nl.F
Chd|-- called by -----------
Chd|        R2R_GROUP                     source/coupling/rad2rad/r2r_group.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE TAGNOD_R2R_NL(IXC,IXTG,IXS,IXS10,IXS20,IXS16,TAG_NLOCAL,IPM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(IN)    :: IXC(NIXC,NUMELC),IXTG(NIXTG,NUMELTG),IXS(NIXS,NUMELS),IXS10(6,NUMELS10),
     .                            IXS20(12,NUMELS20),IXS16(8,NUMELS16),IPM(NPROPMI,NUMMAT)
        INTEGER, INTENT(INOUT) :: TAG_NLOCAL(NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,L,MID
C=======================================================================
C
C-----------------------------------------------------------------------------------------------
c-----Tag of nodes with non local dof-----------------------------------------------------------
C-----------------------------------------------------------------------------------------------
C
C-----------------------------------------------------------------------------------------------
        DO J=1,NUMELC
          MID = IXC(1,J)
          IF (IPM(253,MID) > 0)THEN
            DO L=2,5
              TAG_NLOCAL(IXC(L,J))=1
            ENDDO
          ENDIF
        ENDDO
C-----------------------------------------------------------------------------------------------
        DO J=1,NUMELTG
          MID = IXTG(1,J)
          IF (IPM(253,MID) > 0)THEN
            DO L=2,4
              TAG_NLOCAL(IXTG(L,J))=1
            ENDDO
          ENDIF
        ENDDO
C-----------
        DO J=1,NUMELS8
          MID = IXS(1,J)
          IF (IPM(253,MID) > 0)THEN
            DO L=2,9
              TAG_NLOCAL(IXS(L,J))=1
            ENDDO
          ENDIF
        ENDDO
C-----------
        DO I=1,NUMELS10
          J = I + NUMELS8
          MID = IXS(1,J)
          IF (IPM(253,MID) > 0)THEN
            DO L=2,9
              TAG_NLOCAL(IXS(L,J))=1
            ENDDO
            DO L=1,6
              IF (IXS10(L,I) /= 0) THEN
                TAG_NLOCAL(IXS10(L,I))=1
              ENDIF
            ENDDO
          ENDIF
        ENDDO
C-----------
        DO I=1,NUMELS20
          J = I + NUMELS8 + NUMELS10
          MID = IXS(1,J)
          IF (IPM(253,MID) > 0)THEN
            DO L=2,9
              TAG_NLOCAL(IXS(L,J))=1
            ENDDO
            DO L=1,12
              IF (IXS20(L,I) /= 0) THEN
                TAG_NLOCAL(IXS20(L,I))=1
              ENDIF
            ENDDO
          ENDIF
        ENDDO
C-----------
        DO I=1,NUMELS16
          J = I + NUMELS8 + NUMELS10 + NUMELS20
          MID = IXS(1,J)
          IF (IPM(253,MID) > 0)THEN
            DO L=2,9
              TAG_NLOCAL(IXS(L,J))=0
            ENDDO
            DO L=1,8
              IF (IXS16(L,I) /= 0) THEN
                TAG_NLOCAL(IXS16(L,I))=0
              ENDIF
            ENDDO
          ENDIF
        ENDDO
C
        RETURN
C
      END SUBROUTINE TAGNOD_R2R_NL
