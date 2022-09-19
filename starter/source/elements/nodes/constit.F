Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  CONSTIT                       source/elements/nodes/constit.F
Chd|-- called by -----------
Chd|        BUILD_ADMESH                  source/model/remesh/build_admesh.F
Chd|        HM_READ_NODE                  source/elements/reader/hm_read_node.F
Chd|        INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|        NBADIGEMESH                   source/elements/ige3d/nbadigemesh.F
Chd|        NBADMESH                      source/model/remesh/nbadmesh.F
Chd|        R2R_SPLIT                     source/coupling/rad2rad/r2r_split.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|====================================================================
      SUBROUTINE CONSTIT(ITAB,ITABM1,NUMNOD)
C
C    CONSTITUTION DU TABLEAU INVERSE DES NOEUDS
C    ITABM1(J)=NO USER ITABM1(NUMNOD+J)=NO SYSTEME
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMNOD
      INTEGER ITAB(*), ITABM1(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      INTEGER WORK(70000), INDX(2*NUMNOD)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
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

      RETURN
      END

