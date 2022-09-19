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
Chd|  SHELL_SURFACE_BUFFER          source/model/sets/shell_surface_buffer.F
Chd|-- called by -----------
Chd|        SURFACE_BUFFER                source/model/sets/surface_buffer.F
Chd|-- calls ---------------
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|====================================================================
      SUBROUTINE SHELL_SURFACE_BUFFER(
     .                   IX     ,NIX      ,NIX1      ,NIX2      ,IELTYP,
     .                   NSEG   ,IAD_SURF ,OPT_O     ,BUFTMPSURF,CLAUSE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SETDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIX,NIX1,NIX2,IELTYP,NSEG,IAD_SURF,OPT_O
      INTEGER IX(NIX,*),BUFTMPSURF(*)
!
      TYPE (SET_) :: CLAUSE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,JJ,K,INV4(4),INV3(3),NODE(4),NELEM
      DATA INV4/4,3,2,1/
      DATA INV3/3,2,1/
C=======================================================================
!     ! SURF segments
      IF (IELTYP == 3) NELEM = CLAUSE%NB_SH4N
      IF (IELTYP == 7) NELEM = CLAUSE%NB_SH3N
!
      DO J=1,NELEM
        IF(IELTYP==3) JJ = CLAUSE%SH4N(J)
        IF(IELTYP==7) JJ = CLAUSE%SH3N(J)
!
        NSEG=NSEG+1
!
        IF ( OPT_O == 0 ) THEN ! no normal reversed
          DO K=NIX1,NIX2
            NODE(K-1) = IX(K,JJ)
          ENDDO
        ELSE ! reversed normal
          DO K=NIX2,NIX1,-1
            NODE(INV4(K-1)) = IX(K,JJ)
          ENDDO
        ENDIF ! IF ( OPT_O == 0 )
!
        DO K=NIX1,NIX2
          BUFTMPSURF(IAD_SURF) = NODE(K-1)
          IAD_SURF=IAD_SURF+1
        ENDDO
!
        IF (NIX2-NIX1 == 2) THEN  ! SH3N (NOD4 == NOD3)
          BUFTMPSURF(IAD_SURF) = BUFTMPSURF(IAD_SURF-1)
          IAD_SURF=IAD_SURF+1
        ENDIF
!
        BUFTMPSURF(IAD_SURF) = IELTYP
        IAD_SURF=IAD_SURF+1
        BUFTMPSURF(IAD_SURF) = JJ
        IAD_SURF=IAD_SURF+1
      ENDDO ! DO J=1,NELEM
C-----------
      RETURN
      END
