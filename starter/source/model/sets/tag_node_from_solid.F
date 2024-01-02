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
Chd|  TAG_NODE_FROM_SOLID           source/model/sets/tag_node_from_solid.F
Chd|-- called by -----------
Chd|        CREATE_NODE_FROM_ELEMENT      source/model/sets/create_node_from_element.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE TAG_NODE_FROM_SOLID(
     .                         IXS   ,IXS10  ,IXS20  ,IXS16  ,NUMEL,
     .                         ELEM  ,TAGNOD ,CLAUSE_NODE,IND)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMEL,IND
      INTEGER IXS(NIXS,*),IXS10(6,*),IXS16(8,*),IXS20(12,*),TAGNOD(*),
     .        ELEM(*),CLAUSE_NODE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,JJ,L,JJ10,JJ20,JJ16
C=======================================================================
!
      DO J=1,NUMEL
        JJ = ELEM(J)

        DO L=2,9
           IF (IXS(L,JJ) == 0) CYCLE
           IF(TAGNOD(IXS(L,JJ)) == 0)THEN
              TAGNOD(IXS(L,JJ)) = 1
              IND = IND+1
              CLAUSE_NODE(IND) = IXS(L,JJ)
           ENDIF
        ENDDO

        IF (JJ  > NUMELS8 .AND. JJ <= (NUMELS8+NUMELS10) )THEN

          JJ10 = JJ - NUMELS8
          DO L=1,6
            IF (IXS10(L,JJ10) /= 0) THEN
              IF(TAGNOD(IXS10(L,JJ10)) == 0)THEN
                TAGNOD(IXS10(L,JJ10)) = 1
                IND = IND+1
                CLAUSE_NODE(IND) = IXS10(L,JJ10)
              ENDIF
            ENDIF
          ENDDO

        ELSEIF (JJ  > (NUMELS8+NUMELS10).AND. JJ <= (NUMELS8+NUMELS10+NUMELS20) ) THEN
          JJ20 = JJ - NUMELS8 - NUMELS10
          DO L=1,12
            IF (IXS20(L,JJ20) /= 0) THEN
              IF(TAGNOD(IXS20(L,JJ20)) == 0)THEN
                TAGNOD(IXS20(L,JJ20)) = 1
                IND = IND+1
                CLAUSE_NODE(IND) = IXS20(L,JJ20)
              ENDIF
            ENDIF
          ENDDO
        
        ELSEIF (JJ  > (NUMELS8+NUMELS10+NUMELS20).AND. JJ <= (NUMELS8+NUMELS10+NUMELS20+NUMELS16) ) THEN
          JJ16 = JJ - NUMELS8 - NUMELS10 - NUMELS20
          DO L=1,8
            IF (IXS16(L,JJ16) /= 0) THEN
              IF(TAGNOD(IXS16(L,JJ16)) == 0)THEN
                TAGNOD(IXS16(L,JJ16)) = 1
                IND = IND+1
                CLAUSE_NODE(IND) = IXS16(L,JJ16)
              ENDIF
            ENDIF
          ENDDO

        ELSE
        ENDIF


      ENDDO !  DO J=1,NUMEL

      RETURN
      END
