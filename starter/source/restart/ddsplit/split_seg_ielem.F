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
Chd|  SPLIT_SEG_IELEM               source/restart/ddsplit/split_seg_ielem.F
Chd|-- called by -----------
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPLIT_SEG_IELEM(IELEM_M    ,PROC_MVOISIN  ,MVOISIN  ,NRTM      ,
     .                           NRTM_L     ,TAG_SEG       ,CEP      ,CEL       ,
     .                           IDEL_SOLID,INTERCEP       ,PROC     ,NUMELS    )
c
c split & write segments array (type INTEGER) with local value
c
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD 
      USE FRONT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  , INTENT(IN) :: NRTM ,NRTM_L ! number of main segments global and local
      INTEGER  , INTENT(IN) ::  PROC        ! current proc
      INTEGER  , INTENT(IN) ::  NUMELS      ! number of solid elements
      INTEGER  , INTENT(IN) :: IDEL_SOLID   ! solid eroosion flag
      INTEGER  , INTENT(IN) :: IELEM_M(2,NRTM) !  elements connected to main segments 
      INTEGER  , INTENT(IN) :: TAG_SEG(NRTM_L)   ! local seg to global segement
      INTEGER  , INTENT(IN) :: CEP(*),CEL(*)     ! arrays of proc of elements and local number of element
      INTEGER  , INTENT(IN) :: MVOISIN(4,NRTM)   ! neighbhoors of main segments 
      INTEGER  , INTENT(INOUT) :: PROC_MVOISIN(4,NRTM) ! procs of neighbhoors of main segments : to be build
      TYPE(INTERSURFP)     :: INTERCEP ! procs of main segments
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,GLOB,IE1,IEL1,IE2,IEL2,N
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF1,IBUF3
C ----------------------------------------
      ALLOCATE(IBUF1(2*NRTM_L))
      ALLOCATE(IBUF3(4*NRTM_L))
      IBUF1(1:2*NRTM_L) = 0
      IF(IDEL_SOLID > 0) IBUF3(1:4*NRTM_L) = 0

      DO I=1, NRTM_L
        K=TAG_SEG(I)
        IE1 = IELEM_M(1,K)
        IF(IE1 <= NUMELS) THEN ! distinguish case solid or not
           IEL1 = CEL(IE1)
        ELSE
          IEL1 = NUMELS+CEL(IE1)
        ENDIF
        IE2 = IELEM_M(2,K)
        IEL2 = 0
        IF(IE2 > 0) THEN
          IEL2 = CEL(IE2)
        ENDIF
        IF(CEP(IE1)==PROC ) THEN
          IBUF1(2*(I-1)+1) =IEL1
          IF(IE2 >0) THEN
            IF(CEP(IE2)==PROC ) THEN
              IBUF1(2*(I-1)+2) =IEL2
            ELSE  
              IBUF1(2*(I-1)+2) =-IE2
            ENDIF            
          ENDIF        
        ENDIF 
      ENDDO

        DO I = 1, NRTM        
           DO J = 1,4
              K = MVOISIN(J,I)
              IF(K /= 0) PROC_MVOISIN(J,I) = INTERCEP%P(K)
           ENDDO
        ENDDO
        DO I = 1, NRTM_L         
           K=TAG_SEG(I)
           DO J = 1,4
             N = MVOISIN(J,K)
             IF(N /= 0) IBUF3(4*(I-1)+J) = PROC_MVOISIN(J,K)
           ENDDO
       ENDDO
      
      CALL WRITE_I_C(IBUF1,2*NRTM_L)
      CALL WRITE_I_C(IBUF3,4*NRTM_L)

!     WRITE(6,*) __FILE__,__LINE__,IBUF(1)
      DEALLOCATE(IBUF1)
      DEALLOCATE(IBUF3)

      RETURN
      END
