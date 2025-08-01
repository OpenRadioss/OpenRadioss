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
!||    constit        ../starter/source/elements/nodes/constit.F
!||--- called by ------------------------------------------------------
!||    build_admesh   ../starter/source/model/remesh/build_admesh.F
!||    hm_read_node   ../starter/source/elements/reader/hm_read_node.F
!||    init_monvol    ../starter/source/airbag/init_monvol.F
!||    nbadigemesh    ../starter/source/elements/ige3d/nbadigemesh.F
!||    nbadmesh       ../starter/source/model/remesh/nbadmesh.F
!||    r2r_split      ../starter/source/coupling/rad2rad/r2r_split.F
!||--- calls      -----------------------------------------------------
!||====================================================================
      SUBROUTINE CONSTIT(ITAB,ITABM1,NUMNOD)
C
C    CONSTITUTION DU TABLEAU INVERSE DES NOEUDS
C    ITABM1(J)=NO USER ITABM1(NUMNOD+J)=NO SYSTEME
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMNOD
      INTEGER ITAB(*), ITABM1(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      INTEGER WORK(70000)
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDX
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      ALLOCATE(INDX(2*NUMNOD))
      DO I = 1, NUMNOD
        INDX(I) = I
      END DO
      
      CALL MY_ORDERS(0,WORK,ITAB,INDX,NUMNOD,1)
      
      IF(NUMNOD>=1)THEN
        ITABM1(1)        = ITAB(INDX(1))
        ITABM1(NUMNOD+1) = INDX(1)
      ENDIF
      
      DO I = 2, NUMNOD
        ITABM1(I) = ITAB(INDX(I))
        IF(ITABM1(I)==ITABM1(I-1))THEN
C         An error might have been written during pre-reading of Nodes and Cnodes
C         CALL ANCMSG(MSGID=56,
C    .                MSGTYPE=MSGERROR,
C    .                ANMODE=ANINFO,
C    .                I1=ITABM1(I))
          INDX(I)=INDX(I-1)
        END IF
        ITABM1(NUMNOD+I) = INDX(I)
      END DO
      DEALLOCATE(INDX)

      RETURN
      END
      
