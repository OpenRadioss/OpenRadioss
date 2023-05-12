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
Chd|  CHECK_SURF                    source/groups/check_surf.F    
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE CHECK_SURF(IGRSURF,IFLAG)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFLAG
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, NSEG
C        
      IF(IFLAG == 1) THEN
C-------------------------------------------
C  IGRSURF(I)%ISH4N3N = 1 POUR DES SURFACES DE COQUES
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
C     CHECK SURFACE FOR TH/SURF
C------------------------------------
        DO I=1,NSURF
           IF(IGRSURF(I)%TH_SURF ==2 ) THEN
              IGRSURF(I)%TH_SURF = 1
              IF(IGRSURF(I)%ISH4N3N == 1) CYCLE
              CALL ANCMSG(MSGID=1135,ANMODE=ANINFO,
     .                    MSGTYPE=MSGERROR,I1=IGRSURF(I)%ID)
           ENDIF
        ENDDO
      ENDIF
      RETURN
      END
