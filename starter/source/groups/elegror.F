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
!||    elegror                ../starter/source/groups/elegror.F
!||--- called by ------------------------------------------------------
!||    lecsec42               ../starter/source/tools/sect/hm_read_sect.F
!||    lecsec4bolt            ../starter/source/tools/sect/lecsec4bolt.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||====================================================================
      INTEGER FUNCTION ELEGROR(IGU,IGRELE,NGRELE,MOT,
     .                  IBUF,NIB,MESS,NFRAM,TAGELEMS,ISTYP,
     .                  ID,TITR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
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
      INTEGER IGU,NIB,NGRELE,NFRAM,
     .        IBUF(NIB,*),TAGELEMS(*),ISTYP
      INTEGER ID
C-----------------------------------------------
      TYPE (GROUP_)  ,DIMENSION(NGRELE)   :: IGRELE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER MESS*40,MOT*4
      INTEGER I,J,K,IG,L,IE,IADV
      CHARACTER(LEN=NCHARTITLE) :: TITR
C-----------------------------------------------
      ELEGROR=0
      IF(IGU==0)RETURN
C
      IG=0
      DO I=1,NGRELE
        IF(IGRELE(I)%ID==IGU)IG=I
      ENDDO
C
      IF(IG /= 0) THEN
        IF (NFRAM > 0 .OR. ISTYP >= 1) THEN
          DO L=1,IGRELE(IG)%NENTITY
            IE=IGRELE(IG)%ENTITY(L)
            IF ( TAGELEMS(IE) /= 0 ) ELEGROR = ELEGROR + 1
          ENDDO
        ELSE
          ELEGROR=IGRELE(IG)%NENTITY
        ENDIF
        J = 0
        IF (NFRAM > 0 .OR. ISTYP >= 1) THEN
          DO I=1,IGRELE(IG)%NENTITY
            IF (TAGELEMS(IGRELE(IG)%ENTITY(I)) == 1 ) THEN
                J = J + 1
              IBUF(1,J)=IGRELE(IG)%ENTITY(I)
              ENDIF
          ENDDO
        ELSE
          DO I=1,IGRELE(IG)%NENTITY
            IBUF(1,I)=IGRELE(IG)%ENTITY(I)
          ENDDO
        ENDIF
      ELSE
        ELEGROR = 0
      ENDIF
C
      RETURN
      END
