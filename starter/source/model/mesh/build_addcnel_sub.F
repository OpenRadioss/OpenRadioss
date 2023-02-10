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
Chd|  BUILD_ADDCNEL_SUB             source/model/mesh/build_addcnel_sub.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BUILD_ADDCNEL_SUB(ADDCNEL  ,CNEL     ,ADDCNEL_SUB,
     .                             INDX     ,SUBSIZE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      ! ADDCNE table for all nodes
      INTEGER,INTENT(IN)    :: ADDCNEL(*)      ! (0:NUMNOD+1)
      ! CNE table for all nodes
      INTEGER,INTENT(IN)    :: CNEL(*)         ! (ADDCNEL(NUMNOD+1)-1)
      ! Index table for corresping number in the node subset
      INTEGER,INTENT(IN)    :: INDX(*)         ! (SUBSIZE)
      ! Size of the subset
      INTEGER,INTENT(IN)    :: SUBSIZE
      ! ADDCNE table for the subset of nodes
      INTEGER,INTENT(INOUT) :: ADDCNEL_SUB(*)  ! (0:SUBSIZE + 1) => SUBSIZE+2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NS  ! Number of the node in the subset
      INTEGER NG  ! Number of the node in the global set
      INTEGER NEL ! Number of element attached to the node 
      INTEGER NUMG
      INTEGER I,K
C-----------------------------------------------
C
C Building the ADDCNEL table for a subset of nodes
C    
      ! The first node is in first position in FSKY
      ADDCNEL_SUB(2) = 1     
      ! Boucle ici pour indexer les elements non-locaux
      ! Loop over the nodes of the subset
      DO NS=1,SUBSIZE
        ! Number of the corresponding global node
        NG = INDX(NS)
        ! Finding the number of elements attached to the previous node
        NEL = ADDCNEL(NG+2) - ADDCNEL(NG+1)
        ! Initialization of the subsize element counter
        K = 0
        ! Loop over attached element
        DO I = 1,NEL          
          ! Number of the element
          NUMG = CNEL(ADDCNEL(NG+1)+I-1)
          ! Treatment according to element type
          IF (NUMG<=NUMELS) THEN  
            K = K + 1
          ELSEIF (NUMG<=NUMELS+NUMELQ) THEN
            CONTINUE
          ELSEIF (NUMG<=NUMELS+NUMELQ+NUMELC) THEN
            K = K + 1
          ELSEIF (NUMG<=NUMELS+NUMELQ+NUMELC+NUMELT) THEN
            CONTINUE
          ELSEIF (NUMG<=NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP) THEN
            CONTINUE
          ELSEIF (NUMG<=NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+          !   If the element is a triangle shell
     .            NUMELR) THEN
            CONTINUE
          ELSEIF (NUMG<=NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+          !   If the element is a triangle shell
     .            NUMELR+NUMELTG) THEN
            K = K + 1
          ELSE
            CONTINUE
          ENDIF   
        ENDDO
        ! Computing the position of the current subset node in FSKY
        ADDCNEL_SUB(NS+2) = ADDCNEL_SUB(NS+1) + K
      ENDDO
C-----------------------------------------------
      RETURN
      END
