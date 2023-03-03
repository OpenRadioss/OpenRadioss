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
Chd|  ST_QAPRINT_NODES              source/output/qaprint/st_qaprint_nodes.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_NODES(ITAB,X,IMERGE,MERGE_NODE_TOL,MERGE_NODE_TAB,NMERGE_TOT)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
      USE MESSAGE_MOD
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
      INTEGER, INTENT(IN) :: ITAB(*),IMERGE(*),MERGE_NODE_TAB(4,*),NMERGE_TOT
      my_real, INTENT(IN) ::
     .                       X(3,NUMNOD),MERGE_NODE_TOL(*)
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, MY_ID
      CHARACTER *nchartitle TITR
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE
C-----------------------------------------------
C     Nodes + merged nodes
C-----------------------------------------------

      IF (MYQAKEY('NODES')) THEN
C
        DO I=1,NUMNOD
C
          MY_ID = ITAB(I)
          CALL QAPRINT('NODE_ID', MY_ID,0.0_8)
C
          DO J=1,3
            WRITE(VARNAME,'(A,I0)') 'X_',J
            TEMP_DOUBLE = X(J,I)
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          END DO
C
        END DO ! DO I=1,NUMNOD
C
        IF (NB_MERGE_NODE > 0) THEN
          DO I=1,NB_MERGE_NODE
            CALL QAPRINT('/MERGE/NODE ID ',MERGE_NODE_TAB(4,I),0.0_8)
            CALL QAPRINT('/MERGE/NODE TYPE ',MERGE_NODE_TAB(1,I),0.0_8)
            CALL QAPRINT('/MERGE/NODE GR_ID ',MERGE_NODE_TAB(3,I),0.0_8)
            WRITE(VARNAME,'(A,I0)') 'MERGE_NODE',I
            TEMP_DOUBLE = MERGE_NODE_TOL(I)
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          ENDDO
C
          DO I=1,NMERGED
            MY_ID = ITAB(IMERGE(I))
            WRITE(VARNAME,'(A,X,I0,X,A)') 'NODE ID',MY_ID,'IS MERGED WITH'
            MY_ID = ITAB(IMERGE(NMERGE_TOT+I))
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), MY_ID,0.0_8)
          ENDDO
C
        ENDIF
C
      END IF
C-----------------------------------------------
      RETURN
      END
