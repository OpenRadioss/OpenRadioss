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
Chd|  ELEGROR_SEATBELT              source/groups/elegror_seatbelt.F
Chd|-- called by -----------
Chd|        LECSEC42                      source/tools/sect/hm_read_sect.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      INTEGER FUNCTION ELEGROR_SEATBELT(IGU,IGRELE,NGRELE,IBUF,NIB,
     .                                  SIBUF,NFRAM,TAGELEMS,ISTYP,SEATBELT_SHELL_TO_SPRING)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   FUNCTION TO COUNT & FOUND SPRING ELEMENTS CREATED FROM 2D SEATBELT FOR SECTION
C-----------------------------------------------
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN)::IGU,NIB,NGRELE,NFRAM,SIBUF,
     .        TAGELEMS(NUMELC),ISTYP
      INTEGER,INTENT(INOUT)::IBUF(NIB,SIBUF)
      INTEGER,INTENT(IN)::SEATBELT_SHELL_TO_SPRING(NUMELC,2)
      TYPE (GROUP_) ,DIMENSION(NGRELE) ,INTENT(IN)::IGRELE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IG,L,IE,IADV
C-----------------------------------------------
      ELEGROR_SEATBELT=0
      IF(IGU /= 0)THEN
C
        IG=0
        DO I=1,NGRELE
          IF(IGRELE(I)%ID==IGU)IG=I
        ENDDO
C
        IF(IG /= 0)THEN
          IF (NFRAM > 0 .OR. ISTYP >= 1) THEN
            DO L=1,IGRELE(IG)%NENTITY
              IE=IGRELE(IG)%ENTITY(L)
              IF ( TAGELEMS(IE) /= 0 ) THEN
                IF(SEATBELT_SHELL_TO_SPRING(IE,1) /= 0)
     .                  ELEGROR_SEATBELT = ELEGROR_SEATBELT + 1
                IF(SEATBELT_SHELL_TO_SPRING(IE,2) /= 0)
     .                  ELEGROR_SEATBELT = ELEGROR_SEATBELT + 1
              ENDIF
            ENDDO
          ELSE
            DO J=1,IGRELE(IG)%NENTITY
              IE=IGRELE(IG)%ENTITY(J)
              IF(SEATBELT_SHELL_TO_SPRING(IE,1) /= 0)
     .                  ELEGROR_SEATBELT = ELEGROR_SEATBELT + 1
              IF(SEATBELT_SHELL_TO_SPRING(IE,2) /= 0)
     .                  ELEGROR_SEATBELT = ELEGROR_SEATBELT + 1
            ENDDO
          ENDIF

          J = 0
          IF (NFRAM > 0 .OR. ISTYP >= 1) THEN
            DO I=1,IGRELE(IG)%NENTITY
              IF (TAGELEMS(IGRELE(IG)%ENTITY(I)) == 1 ) THEN
                IE=IGRELE(IG)%ENTITY(I)
                IF(SEATBELT_SHELL_TO_SPRING(IE,1) /= 0) THEN
                  J = J + 1
                  IBUF(1,J) = SEATBELT_SHELL_TO_SPRING(IE,1)
                ENDIF
                IF(SEATBELT_SHELL_TO_SPRING(IE,2) /= 0) THEN
                  J = J + 1
                  IBUF(1,J) = SEATBELT_SHELL_TO_SPRING(IE,2)
                ENDIF
              ENDIF
            ENDDO
          ELSE
            DO I=1,IGRELE(IG)%NENTITY
              IE=IGRELE(IG)%ENTITY(I)
              IF(SEATBELT_SHELL_TO_SPRING(IE,1) /= 0) THEN
                J = J + 1
                IBUF(1,J) = SEATBELT_SHELL_TO_SPRING(IE,1)
              ENDIF
              IF(SEATBELT_SHELL_TO_SPRING(IE,2) /= 0) THEN
                J = J + 1
                IBUF(1,J) = SEATBELT_SHELL_TO_SPRING(IE,2)
              ENDIF
            ENDDO
          ENDIF
        ENDIF
      ENDIF
C
      RETURN
      END
