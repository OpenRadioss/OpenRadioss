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
Chd|  CREATE_ELEMENT_FROM_PART      source/model/sets/create_element_from_part.F
Chd|-- called by -----------
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        INVERTED_GROUP_MOD            share/modules1/inverted_group_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|====================================================================
      SUBROUTINE CREATE_ELEMENT_FROM_PART(CLAUSE  ,INV_GROUP)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   Create Element list from PART list
C------------------------------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME          DESCRIPTION                         
C
C     CLAUSE        (SET structure) Clause to be treated
C     INV_GROUP     Direct access to the list of elements from a given PART
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INVERTED_GROUP_MOD
      USE SETDEF_MOD
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
      TYPE(INVERTGROUP_STRUCT_) :: INV_GROUP
      TYPE (SET_) ::  CLAUSE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ID,IE,EL,IP,EL_FIRST,EL_LAST,SIZE,IND,SZELMAX,I
      INTEGER IWORK(70000)
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDX,SORT
C=======================================================================
      SZELMAX = MAX(NUMELS,NUMELQ,NUMELC,NUMELTG,NUMELTRIA,NUMELT,NUMELP,NUMELR)
      ALLOCATE(INDX(SZELMAX*2))
      ALLOCATE(SORT(SZELMAX))
!
!               SOLID
      IF (NUMELS > 0) THEN
        SIZE = 0
        DO ID=1,CLAUSE%NB_PART
          IP=CLAUSE%PART(ID)
          EL_FIRST = INV_GROUP%INDPARTS(IP)
          EL_LAST  = INV_GROUP%INDPARTS(IP+1)
          SIZE = SIZE + EL_LAST - EL_FIRST
        ENDDO
        CLAUSE%NB_SOLID = SIZE
        ALLOCATE( CLAUSE%SOLID(SIZE) )
!
        IND = 0
        DO ID=1,CLAUSE%NB_PART
          IP=CLAUSE%PART(ID)
          EL_FIRST = INV_GROUP%INDPARTS(IP)
          EL_LAST  = INV_GROUP%INDPARTS(IP+1)-1
          DO EL=EL_FIRST,EL_LAST 
            IND = IND+1
            IE = INV_GROUP%PARTS(EL)
            CLAUSE%SOLID(IND) = IE
          ENDDO 
        ENDDO

        IWORK(1:70000) = 0
        SORT(1:CLAUSE%NB_SOLID) = CLAUSE%SOLID(1:CLAUSE%NB_SOLID)
        CALL MY_ORDERS(0,IWORK,SORT,INDX,CLAUSE%NB_SOLID,1)

        DO I=1,CLAUSE%NB_SOLID
           CLAUSE%SOLID(I) = SORT(INDX(I))
        ENDDO

      ENDIF ! IF (NUMELS > 0)
      
!
!               QUAD
      IF (NUMELQ > 0) THEN
        SIZE = 0
        DO ID=1,CLAUSE%NB_QUAD
          IP=CLAUSE%PART(ID)
          EL_FIRST = INV_GROUP%INDPARTQ(IP)
          EL_LAST  = INV_GROUP%INDPARTQ(IP+1)
          SIZE = SIZE + EL_LAST - EL_FIRST
        ENDDO
        CLAUSE%NB_QUAD = SIZE
        ALLOCATE( CLAUSE%QUAD(SIZE) )
!
        IND = 0
        DO ID=1,CLAUSE%NB_QUAD
          IP=CLAUSE%PART(ID)
          EL_FIRST = INV_GROUP%INDPARTQ(IP)
          EL_LAST  = INV_GROUP%INDPARTQ(IP+1)-1
          DO EL=EL_FIRST,EL_LAST 
            IND = IND+1
            IE = INV_GROUP%PARTQ(EL)
            CLAUSE%QUAD(IND) = IE
          ENDDO 
        ENDDO

        IWORK(1:70000) = 0
        SORT(1:CLAUSE%NB_QUAD) = CLAUSE%QUAD(1:CLAUSE%NB_QUAD)
        CALL MY_ORDERS(0,IWORK,SORT,INDX,CLAUSE%NB_QUAD,1)

        DO I=1,CLAUSE%NB_QUAD
           CLAUSE%QUAD(I) = SORT(INDX(I))
        ENDDO

      ENDIF ! IF (NUMELQ > 0)
!
!               SHELL (4N)
      IF (NUMELC > 0) THEN
        SIZE = 0
        DO ID=1,CLAUSE%NB_PART
          IP=CLAUSE%PART(ID)
          EL_FIRST = INV_GROUP%INDPARTC(IP)
          EL_LAST  = INV_GROUP%INDPARTC(IP+1)
          SIZE = SIZE + EL_LAST - EL_FIRST
        ENDDO
        CLAUSE%NB_SH4N = SIZE
        ALLOCATE( CLAUSE%SH4N(SIZE) )
!
        IND = 0
        DO ID=1,CLAUSE%NB_PART
          IP=CLAUSE%PART(ID)
          EL_FIRST = INV_GROUP%INDPARTC(IP)
          EL_LAST  = INV_GROUP%INDPARTC(IP+1)-1
          DO EL=EL_FIRST,EL_LAST 
            IND = IND+1
            IE = INV_GROUP%PARTC(EL)
            CLAUSE%SH4N(IND) = IE
          ENDDO 
        ENDDO

        IWORK(1:70000) = 0
        SORT(1:CLAUSE%NB_SH4N) = CLAUSE%SH4N(1:CLAUSE%NB_SH4N)
        CALL MY_ORDERS(0,IWORK,SORT,INDX,CLAUSE%NB_SH4N,1)

        DO I=1,CLAUSE%NB_SH4N
           CLAUSE%SH4N(I) = SORT(INDX(I))
        ENDDO

      ENDIF ! IF (NUMELC > 0)

!
!               SHELL (3N)
      IF (NUMELTG > 0) THEN
        SIZE = 0
        DO ID=1,CLAUSE%NB_PART
          IP=CLAUSE%PART(ID)
          EL_FIRST = INV_GROUP%INDPARTTG(IP)
          EL_LAST  = INV_GROUP%INDPARTTG(IP+1)
          SIZE = SIZE + EL_LAST - EL_FIRST
        ENDDO
        CLAUSE%NB_SH3N = SIZE
        ALLOCATE( CLAUSE%SH3N(SIZE) )
!
        IND=0
        DO ID=1,CLAUSE%NB_PART
          IP=CLAUSE%PART(ID)
          EL_FIRST = INV_GROUP%INDPARTTG(IP)
          EL_LAST  = INV_GROUP%INDPARTTG(IP+1)-1
          DO EL=EL_FIRST,EL_LAST 
            IND = IND+1
            IE = INV_GROUP%PARTTG(EL)
            CLAUSE%SH3N(IND) = IE
          ENDDO 
        ENDDO
        IWORK(1:70000) = 0
        SORT(1:CLAUSE%NB_SH3N) = CLAUSE%SH3N(1:CLAUSE%NB_SH3N)
        CALL MY_ORDERS(0,IWORK,SORT,INDX,CLAUSE%NB_SH3N,1)

        DO I=1,CLAUSE%NB_SH3N
           CLAUSE%SH3N(I) = SORT(INDX(I))
        ENDDO

      ENDIF ! IF (NUMELTG > 0)

!
!               TRIA
      IF (NUMELTRIA > 0) THEN
        SIZE = 0
        DO ID=1,CLAUSE%NB_PART
          IP=CLAUSE%PART(ID)
          EL_FIRST = INV_GROUP%INDPARTTRIA(IP)
          EL_LAST  = INV_GROUP%INDPARTTRIA(IP+1)
          SIZE = SIZE + EL_LAST - EL_FIRST
        ENDDO
        CLAUSE%NB_TRIA = SIZE
        ALLOCATE( CLAUSE%TRIA(SIZE) )
!
        IND = 0
        DO ID=1,CLAUSE%NB_PART
          IP=CLAUSE%PART(ID)
          EL_FIRST = INV_GROUP%INDPARTTRIA(IP)
          EL_LAST  = INV_GROUP%INDPARTTRIA(IP+1)-1
          DO EL=EL_FIRST,EL_LAST 
            IND = IND+1
            IE = INV_GROUP%PARTTRIA(EL)
            CLAUSE%TRIA(IND) = IE
          ENDDO 
        ENDDO
        IWORK(1:70000) = 0
        SORT(1:CLAUSE%NB_TRIA) = CLAUSE%SH3N(1:CLAUSE%NB_TRIA)
        CALL MY_ORDERS(0,IWORK,SORT,INDX,CLAUSE%NB_TRIA,1)

        DO I=1,CLAUSE%NB_TRIA
           CLAUSE%SH3N(I) = SORT(INDX(I))
        ENDDO

      ENDIF ! IF (NUMELTRIA > 0)
!
!               TRUSS
      IF (NUMELT > 0) THEN
        SIZE = 0
        DO ID=1,CLAUSE%NB_PART
          IP=CLAUSE%PART(ID)
          EL_FIRST = INV_GROUP%INDPARTT(IP)
          EL_LAST  = INV_GROUP%INDPARTT(IP+1)
          SIZE = SIZE + EL_LAST - EL_FIRST
        ENDDO
        CLAUSE%NB_TRUSS = SIZE
        ALLOCATE( CLAUSE%TRUSS(SIZE) )
!
        IND = 0
        DO ID=1,CLAUSE%NB_PART
          IP=CLAUSE%PART(ID)
          EL_FIRST = INV_GROUP%INDPARTT(IP)
          EL_LAST  = INV_GROUP%INDPARTT(IP+1)-1
          DO EL=EL_FIRST,EL_LAST 
            IND = IND+1
            IE = INV_GROUP%PARTT(EL)
            CLAUSE%TRUSS(IND) = IE
          ENDDO 
        ENDDO

        IWORK(1:70000) = 0
        SORT(1:CLAUSE%NB_TRUSS) = CLAUSE%TRUSS(1:CLAUSE%NB_TRUSS)
        CALL MY_ORDERS(0,IWORK,SORT,INDX,CLAUSE%NB_TRUSS,1)
        
        DO I=1,CLAUSE%NB_TRUSS
           CLAUSE%TRUSS(I) = SORT(INDX(I))
        ENDDO

      ENDIF ! IF (NUMELT > 0)

!               BEAM
      IF (NUMELP > 0) THEN

        SIZE = 0
        DO ID=1,CLAUSE%NB_PART
          IP=CLAUSE%PART(ID)
          EL_FIRST = INV_GROUP%INDPARTP(IP)
          EL_LAST  = INV_GROUP%INDPARTP(IP+1)
          SIZE = SIZE + EL_LAST - EL_FIRST
        ENDDO

        CLAUSE%NB_BEAM = SIZE
        ALLOCATE( CLAUSE%BEAM(SIZE) )
!
        IND = 0
        DO ID=1,CLAUSE%NB_PART
          IP=CLAUSE%PART(ID)
          EL_FIRST = INV_GROUP%INDPARTP(IP)
          EL_LAST  = INV_GROUP%INDPARTP(IP+1)-1
          DO EL=EL_FIRST,EL_LAST 
            IND = IND+1
            IE = INV_GROUP%PARTP(EL)
            CLAUSE%BEAM(IND) = IE
          ENDDO 
        ENDDO
        IWORK(1:70000) = 0
        SORT(1:CLAUSE%NB_BEAM) = CLAUSE%BEAM(1:CLAUSE%NB_BEAM)
        CALL MY_ORDERS(0,IWORK,SORT,INDX,CLAUSE%NB_BEAM,1)
        
        DO I=1,CLAUSE%NB_BEAM
           CLAUSE%BEAM(I) = SORT(INDX(I))
        ENDDO

      ENDIF ! IF (NUMELP > 0)


!
!     SPRING
      IF (NUMELR > 0) THEN

        SIZE = 0
        DO ID=1,CLAUSE%NB_PART
          IP=CLAUSE%PART(ID)
          EL_FIRST = INV_GROUP%INDPARTR(IP)
          EL_LAST  = INV_GROUP%INDPARTR(IP+1)
          SIZE = SIZE + EL_LAST - EL_FIRST
        ENDDO

        CLAUSE%NB_SPRING = SIZE
        ALLOCATE( CLAUSE%SPRING(SIZE) )
!
        IND = 0
        DO ID=1,CLAUSE%NB_PART
          IP=CLAUSE%PART(ID)
          EL_FIRST = INV_GROUP%INDPARTR(IP)
          EL_LAST  = INV_GROUP%INDPARTR(IP+1)-1
          DO EL=EL_FIRST,EL_LAST 
            IND = IND+1
            IE = INV_GROUP%PARTR(EL)
            CLAUSE%SPRING(IND) = IE
          ENDDO 
        ENDDO
        IWORK(1:70000) = 0
        SORT(1:CLAUSE%NB_SPRING) = CLAUSE%SPRING(1:CLAUSE%NB_SPRING)
        CALL MY_ORDERS(0,IWORK,SORT,INDX,CLAUSE%NB_SPRING,1)
        
        DO I=1,CLAUSE%NB_SPRING
           CLAUSE%SPRING(I) = SORT(INDX(I))
        ENDDO

      ENDIF ! IF (NUMELR > 0)
!
C-------------------------
      RETURN
      END
