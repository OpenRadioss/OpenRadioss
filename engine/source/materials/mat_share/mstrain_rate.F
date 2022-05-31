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
Chd|  MSTRAIN_RATE                  source/materials/mat_share/mstrain_rate.F
Chd|-- called by -----------
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        SIGEPS44                      source/materials/mat/mat044/sigeps44.F
Chd|        SIGEPS93                      source/materials/mat/mat093/sigeps93.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MSTRAIN_RATE(NEL    ,ISRATE ,ASRATE ,EPSD   ,IDEV   ,
     .                        EP1    ,EP2    ,EP3    ,EP4    ,EP5    ,EP6)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NEL,ISRATE,IDEV
      my_real  ASRATE
      my_real, DIMENSION(NEL), INTENT(IN)    :: EP1, EP2, EP3, EP4, EP5, EP6
      my_real, DIMENSION(NEL), INTENT(INOUT) :: EPSD
C---------------------------------------------------
C   L o c a l   V a r i a b l e s
C---------------------------------------------------
      INTEGER I
      my_real :: E1, E2, E3, E4, E5, E6, DAV, EPSP
      my_real :: EPSDOT(NEL)
c-----------------------------------------------------------------------
c  COMPUTE EQUIVALENT STRAIN RATE
c        IDEV = 0 => total strain rate
c        IDEV = 1 => deviatoric strain rate
c=======================================================================
      IF (ISRATE >= 0) THEN   ! calculate strain rate
        IF (IDEV == 0) THEN   ! total strain rate
          DO I=1,NEL
            E1 = EP1(I)
            E2 = EP2(I)
            E3 = EP3(I)
            E4 = HALF*EP4(I)
            E5 = HALF*EP5(I)
            E6 = HALF*EP6(I)
            EPSP = E1**2 + E2**2 + E3**2 + TWO*(E4**2 + E5**2 + E6**2)
            EPSDOT(I) = SQRT(EPSP)
          ENDDO
        ELSE                  ! deviatoric strain rate
          DO I=1,NEL
            DAV = (EP1(I)+EP2(I)+EP3(I))*THIRD
            E1  = EP1(I) - DAV
            E2  = EP2(I) - DAV
            E3  = EP3(I) - DAV
            E4  = HALF*EP4(I)
            E5  = HALF*EP5(I)
            E6  = HALF*EP6(I)
            EPSP = HALF*(E1**2 + E2**2 + E3**2) + E4**2 + E5**2 + E6**2
            EPSDOT(I) = SQRT(THREE*EPSP)/THREE_HALF
          ENDDO
        ENDIF
      ENDIF
c
      IF (ISRATE == 0) THEN
        EPSD(:) = EPSDOT(:)
      ELSEIF (ISRATE > 0) THEN
        EPSD(:) = ASRATE*EPSDOT(:) + (ONE - ASRATE)*EPSD(:)
      ENDIF
c-----------
      RETURN
      END
