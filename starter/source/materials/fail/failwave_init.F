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
Chd|  FAILWAVE_INIT                 source/materials/fail/failwave_init.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FAILWAVE_INIT(FAILWAVE,IPARG,IXC,IXTG,NUMNOD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE FAILWAVE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMNOD
      INTEGER ,DIMENSION(NPARG,NGROUP), INTENT(IN) :: IPARG
      INTEGER ,DIMENSION(NIXC,*)  ,INTENT(IN) :: IXC
      INTEGER ,DIMENSION(NIXTG,*) ,INTENT(IN) :: IXTG
      TYPE (FAILWAVE_STR_) :: FAILWAVE 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IFAILWAVE,NG,NEL,NFT,ITY,IGTYP,IFRW,INOD,NNOD,NDDL,SIZE,MAXLEV
      INTEGER ,DIMENSION(:), ALLOCATABLE :: TAGNOD,INDX,IDXI
C=======================================================================

      ALLOCATE( TAGNOD(NUMNOD),INDX(NUMNOD),IDXI(NUMNOD) )
c
      IFAILWAVE = FAILWAVE%WAVE_MOD
      IF (IFAILWAVE == 0) THEN
        FAILWAVE%NNOD   = 0
        FAILWAVE%NDDL   = 0
        FAILWAVE%SIZE   = 0
        ALLOCATE (FAILWAVE%FWAVE_NOD(0,0,0))
        ALLOCATE (FAILWAVE%FWAVE_NOD_STACK(0,0,0))
        ALLOCATE (FAILWAVE%MAXLEV(0))
        ALLOCATE (FAILWAVE%INDX(0))
        ALLOCATE (FAILWAVE%IDXI(0))
c
      ELSE
c
        TAGNOD(:) = 0
        INDX(:)   = 0
        IDXI(:)   = 0
c
        DO NG=1,NGROUP
          IFRW = IPARG(79,NG)
          IF (IFRW > 0) THEN
            NEL   = IPARG(2,NG)
            NFT   = IPARG(3,NG)
            ITY   = IPARG(5,NG)
            IGTYP = IPARG(38,NG)
            IF (ITY == 3) THEN
              DO I=1,NEL
                DO J=2,5
                  INOD = IXC(J,I+NFT)
                  TAGNOD(INOD) = 1
                ENDDO      
              ENDDO      
            ELSE IF (ITY == 7) THEN
              DO I=1,NEL
                DO J=2,4
                  INOD = IXC(J,I+NFT)
                  TAGNOD(INOD) = 1
                ENDDO      
              ENDDO      
            ENDIF
          ENDIF
        ENDDO      
c
        NNOD = 0
        DO I=1,NUMNOD
          IF (TAGNOD(I) == 1) THEN
            NNOD = NNOD + 1
            INDX(NNOD) = I                ! INDX(nnod)   -> NUMNOD
            IDXI(I)    = NNOD             ! IDXI(numnod) -> NNOD
          ENDIF
        ENDDO
c
        IF (IFAILWAVE == 1) THEN   ! isotropic propagation
          NDDL   = 1
          SIZE   = 1
        ELSE                       ! directional propagation
          NDDL   = 2
          SIZE   = 10
        ENDIF
        FAILWAVE%NNOD = NNOD
        FAILWAVE%SIZE = SIZE
        FAILWAVE%NDDL = NDDL
        MY_ALLOCATE (FAILWAVE%INDX,NNOD)
        MY_ALLOCATE (FAILWAVE%IDXI,NUMNOD)
        MY_ALLOCATE (FAILWAVE%MAXLEV,0)
        MY_ALLOCATE (FAILWAVE%MAXLEV_STACK,0)
        ALLOCATE (FAILWAVE%FWAVE_NOD(NDDL,NUMNOD,SIZE))
        ALLOCATE (FAILWAVE%FWAVE_NOD_STACK(NDDL,NUMNOD,SIZE))
c
        FAILWAVE%INDX(1:NNOD)  = INDX(1:NNOD)
        FAILWAVE%IDXI(1:NUMNOD)= IDXI(1:NUMNOD)
        FAILWAVE%FWAVE_NOD = 0
        FAILWAVE%FWAVE_NOD_STACK = 0
      ENDIF

      DEALLOCATE( TAGNOD,INDX,IDXI )    
c
c--------------------------------
      RETURN
      END
