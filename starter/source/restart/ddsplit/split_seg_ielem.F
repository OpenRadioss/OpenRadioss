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
      !||    split_seg_ielem    ../starter/source/restart/ddsplit/split_seg_ielem.F
      !||--- called by ------------------------------------------------------
      !||    split_interfaces   ../starter/source/restart/ddsplit/split_interfaces.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    front_mod          ../starter/share/modules1/front_mod.F
      !||====================================================================
      SUBROUTINE SPLIT_SEG_IELEM(IELEM_M    ,PROC_MVOISIN  ,MVOISIN  ,NRTM      ,
     .                           NRTM_L     ,TAG_SEG       ,CEP      ,CEL       ,
     .                           IDEL_SOLID,INTERCEP       ,PROC     ,NUMELS    ,
     .                           NUMELQ    ,NUMELC         ,NUMELT   ,NUMELP    ,
     .                           NUMELR    ,NUMELTG)
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
      INTEGER  , INTENT(IN) :: NRTM !< number of main segments (global)
      INTEGER  , INTENT(IN) :: NRTM_L !< number of main segments (local)
      INTEGER  , INTENT(IN) ::  PROC        !< current proc
      INTEGER  , INTENT(IN) ::  NUMELS      !< number of solid elements (global)
      INTEGER  , INTENT(IN) ::  NUMELQ      !< number of quad elements (global)
      INTEGER  , INTENT(IN) ::  NUMELC      !< number of shell elements (global)
      INTEGER  , INTENT(IN) ::  NUMELT      !< number of truss elements (global)
      INTEGER  , INTENT(IN) ::  NUMELP      !< number of beam elements (global)
      INTEGER  , INTENT(IN) ::  NUMELR      !< number of spring elements (global)
      INTEGER  , INTENT(IN) ::  NUMELTG      !< number of shell3n elements (global)

      INTEGER  , INTENT(IN) :: IDEL_SOLID   !< solid eroosion flag
      INTEGER  , INTENT(IN) :: IELEM_M(2,NRTM) !< elements connected to main segments 
      INTEGER  , INTENT(IN) :: TAG_SEG(NRTM_L)   !< local seg to global segment
      INTEGER  , INTENT(IN) :: CEP(*),CEL(*)     !< arrays of proc of elements and local number of element
      INTEGER  , INTENT(IN) :: MVOISIN(4,NRTM)   !< neighbhoors of main segments 
      INTEGER  , INTENT(INOUT) :: PROC_MVOISIN(4,NRTM) !< procs of neighbhoors of main segments : to be built
      TYPE(INTERSURFP)     :: INTERCEP !< procs of main segments
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,GLOB,IE1,IEL1,IE2,IEL2,N
      INTEGER :: OFFSET_SHELL,OFFSET_SHELL3N
      INTEGER, DIMENSION(:),ALLOCATABLE :: IBUF1,IBUF3
C ----------------------------------------
      ALLOCATE(IBUF1(2*NRTM_L))
      ALLOCATE(IBUF3(4*NRTM_L))
      IBUF1(1:2*NRTM_L) = 0
      IF(IDEL_SOLID > 0) IBUF3(1:4*NRTM_L) = 0
      OFFSET_SHELL=NUMELS+NUMELQ
      OFFSET_SHELL3N=OFFSET_SHELL+NUMELC+NUMELT+NUMELP+NUMELR
      ! ---------------------
      ! loop over the segment of the current proc
      ! a segment can be connected to :
      !  * a solid --> no offset
      !  * a shell --> offset = global number of solid+quad, ielem_ m = local shell id + offset
      !  * a shell 3n --> offset = global number of solid+quad+shell+truss+beam+spring ielem_ m = local shell3n id + offset
      DO I=1, NRTM_L
        K=TAG_SEG(I) ! get the segment id
        IE1 = IELEM_M(1,K) ! get the element id (element of the segment)
        ! ----------------
        IEL1 = 0
        ! convert the global element id to local element id (local to the processor)
        IF(IE1 <= NUMELS) THEN ! segment is connected to a solid
           IEL1 = CEL(IE1)
        ELSEIF(OFFSET_SHELL<IE1.AND.IE1<=OFFSET_SHELL+NUMELC) THEN  ! segment is connected to a shell
          IEL1 = OFFSET_SHELL+CEL(IE1)
        ELSEIF(OFFSET_SHELL3N<IE1.AND.IE1<=OFFSET_SHELL3N+NUMELTG) THEN ! segment is connected to a shell3n
          IEL1 = OFFSET_SHELL3N+CEL(IE1)
        ENDIF
        ! ----------------
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
      ! ---------------------
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

      DEALLOCATE(IBUF1)
      DEALLOCATE(IBUF3)

      RETURN
      END
