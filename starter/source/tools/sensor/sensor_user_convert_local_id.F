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
Chd|  SENSOR_USER_CONVERT_LOCAL_ID  source/tools/sensor/sensor_user_convert_local_id.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|====================================================================
        SUBROUTINE SENSOR_USER_CONVERT_LOCAL_ID(ITABM1,POINTER_ENTITY,NUMBER_ENTITY,LIST_ENTITY,MODE0,IPART)
!$COMMENT
!       SENSOR_USER_CONVERT_LOCAL_I description
!       convert a list of User ID into a local ID
!       
!       SENSOR_USER_CONVERT_LOCAL_I organization :
!           - sort the list
!           - remove the duplicated ID
!           - compact the list
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: MODE0
        INTEGER, INTENT(inout) :: POINTER_ENTITY
        INTEGER, INTENT(inout) :: NUMBER_ENTITY
        INTEGER, DIMENSION(*), INTENT(inout) :: LIST_ENTITY
        INTEGER ,DIMENSION(*)   ,INTENT(IN) :: ITABM1
        INTEGER,INTENT(IN)::IPART(LIPART1,*)
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
        INTEGER USR2SYS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: SIZE_LOCAL
        INTEGER :: MODE,WORK(70000)
        INTEGER, DIMENSION(:), ALLOCATABLE :: LOCAL_ARRAY,INDX
        INTEGER :: N1,NS,U_PART
        INTEGER :: I,J,K,IJK
        CHARACTER MESS*40
        DATA MESS/'SENSOR DEFINITION                       '/
!   -------------------------------
        NS = 1

        ! -------------------
        ! sort the list of ID
        MODE=0
        SIZE_LOCAL = POINTER_ENTITY
        ALLOCATE( INDX(2*SIZE_LOCAL) )
        ALLOCATE( LOCAL_ARRAY(SIZE_LOCAL) )
        DO I=1,SIZE_LOCAL
            INDX(I) = I
            LOCAL_ARRAY(I) = LIST_ENTITY(I)
        ENDDO
        CALL MY_ORDERS(MODE,WORK,LIST_ENTITY,INDX,SIZE_LOCAL,1)
        DO I=1,SIZE_LOCAL
            J = INDX(I)
            LIST_ENTITY(I) = LOCAL_ARRAY(J)
        ENDDO
        ! -------------------
        ! remove the duplicated ID

        ! example of list :
        ! position in the list + list
        ! 1  2  3  4  5  6  7  8  9  10    <-- position
        ! a  a  a  b  c  c  d  d  d  e     <-- list
        ! if node(i)/=node(i+1) --> save i
        ! 1  4  5  7  10  <-- index of saved nodes
        ! a  b  c  d  e   <-- compacted list

        ! compute the index of saved nodes
        K = 1
        LOCAL_ARRAY(1) = 1
        J = 0 
        DO I=2,SIZE_LOCAL
            J = J + 1
            IF(LIST_ENTITY(I)/=LIST_ENTITY(I-1)) THEN
                K = K + 1
                LOCAL_ARRAY(K) = LOCAL_ARRAY(K-1) + J
                J = 0
            ENDIF
        ENDDO

        ! compact the list of node & save the new number of node + pointer
        DO I=1,K
            LIST_ENTITY(I) = LIST_ENTITY(LOCAL_ARRAY(I))
        ENDDO
        POINTER_ENTITY = K
        NUMBER_ENTITY = K
        ! -------------------
        ! convert User node ID --> Local node ID
        IF(MODE0==1)THEN
            DO I=1,NUMBER_ENTITY
                N1 = LIST_ENTITY(I)
                LIST_ENTITY(I) = USR2SYS(N1,ITABM1,MESS,NS)
            ENDDO
        ! -------------------
        ! convert User part ID --> Local part ID
        ELSEIF(MODE0==2) THEN
            DO I=1,NUMBER_ENTITY
                U_PART = LIST_ENTITY(I)
                IJK = 0
                DO J=1,NPART
                    IF(U_PART == IPART(4,J)) THEN
                        IJK = J
                        EXIT
                    ENDIF
                ENDDO
                LIST_ENTITY(I) = IJK
            ENDDO
        ENDIF
        ! -------------------

        DEALLOCATE( INDX )
        DEALLOCATE( LOCAL_ARRAY )
        RETURN
        END SUBROUTINE SENSOR_USER_CONVERT_LOCAL_ID
