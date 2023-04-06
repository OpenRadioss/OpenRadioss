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
Chd|  COMPUTE_CONNECT_PARTELM       source/model/sets/compute_connect_partelm.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        INVERTED_GROUP_ALLOC          source/model/sets/inverted_group_alloc.F
Chd|        INVERTED_GROUP_INIT           source/model/sets/inverted_group_init.F
Chd|        INVERTED_GROUP_MOD            share/modules1/inverted_group_mod.F
Chd|====================================================================
      SUBROUTINE COMPUTE_CONNECT_PARTELM( IPARTS,IPARTQ,IPARTC,IPARTT,IPARTP,
     .                                    IPARTTG,IPARTR ,INV_GROUP)
!$COMMENT
!       COMPUTE_CONNECT_PARTELM description
!       computation of the inverted connectivity between part and element
!       
!       COMPUTE_CONNECT_PARTELM organization :
!           for each kind of element :
!            - compute an index array based on the number of
!              element in each part
!            - fill the INV_GROUP%PARTxxx array with the part ID
!            
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INVERTED_GROUP_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s 
C-----------------------------------------------             
C
      INTEGER, INTENT(IN),DIMENSION(NUMELS) :: IPARTS
      INTEGER, INTENT(IN),DIMENSION(NUMELQ) :: IPARTQ
      INTEGER, INTENT(IN),DIMENSION(NUMELC) :: IPARTC
      INTEGER, INTENT(IN),DIMENSION(NUMELTG):: IPARTTG
      INTEGER, INTENT(IN),DIMENSION(NUMELR) :: IPARTR
      INTEGER, INTENT(IN),DIMENSION(NUMELT) :: IPARTT
      INTEGER, INTENT(IN),DIMENSION(NUMELP) :: IPARTP

      TYPE(INVERTGROUP_STRUCT_) :: INV_GROUP

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,EL,NCOUNT,PART,IND,FIRST,LAST
      INTEGER, DIMENSION (:), ALLOCATABLE :: PART_COUNT
C-----------------------------------------------
      ! allocation of INV_GROUP% array
      IF( .NOT.(INV_GROUP%ALREADY_ALLOC) ) CALL INVERTED_GROUP_ALLOC(INV_GROUP)
      ! check INV_GROUP% array was already computed, if already computed, flush to 0
      IF( INV_GROUP%ALREADY_DONE ) CALL INVERTED_GROUP_INIT(1,INV_GROUP)

      ALLOCATE(PART_COUNT(NPART))


C ------------------------------------------------
C  Solid elements
C ------------------------------------------------
      PART_COUNT(1:NPART)=0

      DO I=1,NUMELS
        PART=IPARTS(I)
        PART_COUNT(PART)=PART_COUNT(PART)+1
      ENDDO 

      INV_GROUP%INDPARTS(1)=1
      DO I=2,NPART+1
        INV_GROUP%INDPARTS(I)=INV_GROUP%INDPARTS(I-1)+PART_COUNT(I-1)
      ENDDO

      PART_COUNT(1:NPART)=0
      DO I=1,NUMELS
        PART=IPARTS(I)

        IND=INV_GROUP%INDPARTS(PART) + PART_COUNT(PART)
        INV_GROUP%PARTS(IND)=I
        PART_COUNT(PART)=PART_COUNT(PART)+1
      ENDDO         
C ------------------------------------------------
C  Quad elements
C ------------------------------------------------
      PART_COUNT(1:NPART)=0

      DO I=1,NUMELQ
        PART=IPARTQ(I)
        PART_COUNT(PART)=PART_COUNT(PART)+1
      ENDDO 

      INV_GROUP%INDPARTQ(1)=1
      DO I=2,NPART+1
        INV_GROUP%INDPARTQ(I)=INV_GROUP%INDPARTQ(I-1)+PART_COUNT(I-1)
      ENDDO

      PART_COUNT(1:NPART)=0
      DO I=1,NUMELQ
        PART=IPARTQ(I)

        IND=INV_GROUP%INDPARTQ(PART) + PART_COUNT(PART)
        INV_GROUP%PARTQ(IND)=I
        PART_COUNT(PART)=PART_COUNT(PART)+1
      ENDDO        
C ------------------------------------------------
C  Shell elements
C ------------------------------------------------
      PART_COUNT(1:NPART)=0

      DO I=1,NUMELC
        PART=IPARTC(I)
        PART_COUNT(PART)=PART_COUNT(PART)+1
      ENDDO 

      INV_GROUP%INDPARTC(1)=1
      DO I=2,NPART+1
        INV_GROUP%INDPARTC(I)=INV_GROUP%INDPARTC(I-1)+PART_COUNT(I-1)
      ENDDO

      PART_COUNT(1:NPART)=0
      DO I=1,NUMELC
        PART=IPARTC(I)

        IND=INV_GROUP%INDPARTC(PART) + PART_COUNT(PART)
        INV_GROUP%PARTC(IND)=I
        PART_COUNT(PART)=PART_COUNT(PART)+1
      ENDDO       
C ------------------------------------------------
C  TRUSS elements
C ------------------------------------------------
      PART_COUNT(1:NPART)=0

      DO I=1,NUMELT
        PART=IPARTT(I)
        PART_COUNT(PART)=PART_COUNT(PART)+1
      ENDDO 

      INV_GROUP%INDPARTT(1)=1
      DO I=2,NPART+1
        INV_GROUP%INDPARTT(I)=INV_GROUP%INDPARTT(I-1)+PART_COUNT(I-1)
      ENDDO

      PART_COUNT(1:NPART)=0
      DO I=1,NUMELT
        PART=IPARTT(I)

        IND=INV_GROUP%INDPARTT(PART) + PART_COUNT(PART)
        INV_GROUP%PARTT(IND)=I
        PART_COUNT(PART)=PART_COUNT(PART)+1
      ENDDO       
C ------------------------------------------------
C  Beam elements
C ------------------------------------------------
      PART_COUNT(1:NPART)=0

      DO I=1,NUMELP
        PART=IPARTP(I)
        PART_COUNT(PART)=PART_COUNT(PART)+1
      ENDDO 

      INV_GROUP%INDPARTP(1)=1
      DO I=2,NPART+1
        INV_GROUP%INDPARTP(I)=INV_GROUP%INDPARTP(I-1)+PART_COUNT(I-1)
      ENDDO

      PART_COUNT(1:NPART)=0
      DO I=1,NUMELP
        PART=IPARTP(I)
        IND=INV_GROUP%INDPARTP(PART) + PART_COUNT(PART)
        INV_GROUP%PARTP(IND)=I
        PART_COUNT(PART)=PART_COUNT(PART)+1
      ENDDO                  
C ------------------------------------------------
C  Triangle elements
C ------------------------------------------------
      PART_COUNT(1:NPART)=0

      DO I=1,NUMELTG
        PART=IPARTTG(I)
        PART_COUNT(PART)=PART_COUNT(PART)+1
      ENDDO 

      INV_GROUP%INDPARTTG(1)=1
      DO I=2,NPART+1
        INV_GROUP%INDPARTTG(I)=INV_GROUP%INDPARTTG(I-1)+PART_COUNT(I-1)
      ENDDO

      PART_COUNT(1:NPART)=0
      DO I=1,NUMELTG
        PART=IPARTTG(I)

        IND=INV_GROUP%INDPARTTG(PART) + PART_COUNT(PART)
        INV_GROUP%PARTTG(IND)=I
        PART_COUNT(PART)=PART_COUNT(PART)+1
      ENDDO         
C ------------------------------------------------
C  TRIA ELTs
C ------------------------------------------------
      PART_COUNT(1:NPART)=0

      DO I=1,NUMELTRIA
        PART=IPARTTG(I)
        PART_COUNT(PART)=PART_COUNT(PART)+1
      ENDDO 

      INV_GROUP%INDPARTTRIA(1)=1
      DO I=2,NPART+1
        INV_GROUP%INDPARTTRIA(I)=INV_GROUP%INDPARTTRIA(I-1)+PART_COUNT(I-1)
      ENDDO

      PART_COUNT(1:NPART)=0
      DO I=1,NUMELTRIA
        PART=IPARTTG(I)

        IND=INV_GROUP%INDPARTTRIA(PART) + PART_COUNT(PART)
        INV_GROUP%PARTTRIA(IND)=I
        PART_COUNT(PART)=PART_COUNT(PART)+1
      ENDDO          

C ------------------------------------------------
C  Spring elements
C ------------------------------------------------
      PART_COUNT(1:NPART)=0

      DO I=1,NUMELR
        PART=IPARTR(I)
        PART_COUNT(PART)=PART_COUNT(PART)+1
      ENDDO 

      INV_GROUP%INDPARTR(1)=1
      DO I=2,NPART+1
        INV_GROUP%INDPARTR(I)=INV_GROUP%INDPARTR(I-1)+PART_COUNT(I-1)
      ENDDO
      PART_COUNT(1:NPART)=0
      DO I=1,NUMELR
        PART=IPARTR(I)

        IND=INV_GROUP%INDPARTR(PART) + PART_COUNT(PART)
        INV_GROUP%PARTR(IND)=I
        PART_COUNT(PART)=PART_COUNT(PART)+1
      ENDDO          

      DEALLOCATE(PART_COUNT)

      INV_GROUP%ALREADY_DONE = .TRUE.
      RETURN
      END SUBROUTINE COMPUTE_CONNECT_PARTELM
C-----------------------------------------------
