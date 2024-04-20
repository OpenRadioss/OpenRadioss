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
Chd|  CHECK_SURF                    source/groups/check_surf.F    
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE CHECK_SURF(IGRSURF,IFLAG,EBCS_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE EBCS_MOD , ONLY : t_ebcs_tab
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
      INTEGER IFLAG
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE(t_ebcs_tab), INTENT(INOUT) :: EBCS_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, NSEG, EBCS_SURF_ID, KK
      LOGICAL :: FOUND
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(IFLAG == 1) THEN
C-------------------------------------------
C  IGRSURF(I)%ISH4N3N = 1 FOR SURFACE COMPOSED OF SHELL ELEMS
C-------------------------------------------
        DO I=1,NSURF
          NSEG = IGRSURF(I)%NSEG
          IGRSURF(I)%ISH4N3N = 1
            DO J = 1,NSEG
              K = IGRSURF(I)%ELTYP(J)
              IF(K/=3 .AND. K/=7) THEN
                 IGRSURF(I)%ISH4N3N = 0
                 CYCLE
              ENDIF
            ENDDO
        ENDDO

      ELSEIF(IFLAG == 2) THEN
C------------------------------------
C     CHECK SURFACE FOR /TH/SURF OPTION
C------------------------------------
        DO I=1,NSURF
           IF(IGRSURF(I)%TH_SURF == 2 ) THEN
              IGRSURF(I)%TH_SURF = 1
              !AIRBAG CASE
              IF(IGRSURF(I)%ISH4N3N == 1) CYCLE
              !EBCS CASE (mark /EBCS for which time history is requested)
              FOUND = .FALSE.
              DO KK=1,EBCS_TAB%NEBCS
                EBCS_SURF_ID = EBCS_TAB%TAB(KK)%poly%SURF_ID
                IF(I == EBCS_SURF_ID)THEN
                  EBCS_TAB%TAB(KK)%poly%HAS_TH = .TRUE.
                  FOUND = .TRUE.
                  EXIT
                ENDIF
              ENDDO
              IF(.NOT. FOUND)THEN
                CALL ANCMSG(MSGID=1135,ANMODE=ANINFO,MSGTYPE=MSGERROR,I1=IGRSURF(I)%ID)
              ENDIF
           ENDIF
        ENDDO
      ENDIF
      RETURN
      END
