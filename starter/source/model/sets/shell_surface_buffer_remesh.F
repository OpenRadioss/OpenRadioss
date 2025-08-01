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
!||    shell_surface_buffer_remesh   ../starter/source/model/sets/shell_surface_buffer_remesh.F
!||--- called by ------------------------------------------------------
!||    surface_buffer                ../starter/source/model/sets/surface_buffer.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE SHELL_SURFACE_BUFFER_REMESH(
     .                   IX     ,NIX      ,NIX1      ,NIX2    ,IELTYP ,
     .                   NSEG   ,IAD_SURF ,BUFTMPSURF,CLAUSE  ,IPARTE ,
     .                   IPART  ,KSHTREE  ,SHTREE    )
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
      INTEGER NIX,NIX1,NIX2,IELTYP,NSEG,IAD_SURF
      INTEGER IX(NIX,*),BUFTMPSURF(*),IPARTE(*),
     .        IPART(LIPART1,*),KSHTREE,SHTREE(KSHTREE,*)
!
      TYPE (SET_) :: CLAUSE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,JJ,K,IP,NLEV,MY_LEV,INV4(4),INV3(3),NELEM
      DATA INV4/4,3,2,1/
      DATA INV3/3,2,1/
C-----------------------------------------------
!
      NELEM = 0
      IF (IELTYP == 3) NELEM = CLAUSE%NB_SH4N
      IF (IELTYP == 7) NELEM = CLAUSE%NB_SH3N
!
      DO J=1,NELEM
        JJ= -HUGE(JJ)
        IF(IELTYP==3) JJ = CLAUSE%SH4N(J)
        IF(IELTYP==7) JJ = CLAUSE%SH3N(J)
!
        IP=IPARTE(JJ)
        NLEV  =IPART(10,IP)
        MY_LEV=SHTREE(3,JJ)
        IF (MY_LEV < 0) MY_LEV=-(MY_LEV+1)
        IF (MY_LEV==NLEV) THEN
          NSEG=NSEG+1
          DO K=NIX1,NIX2
            BUFTMPSURF(IAD_SURF) = IX(K,JJ)
            IAD_SURF=IAD_SURF+1
          ENDDO
!
          IF (NIX2-NIX1 == 2) THEN
            BUFTMPSURF(IAD_SURF) = BUFTMPSURF(IAD_SURF-1)  ! SH3N (NOD4 == NOD3)
            IAD_SURF=IAD_SURF+1
          ENDIF
!
          BUFTMPSURF(IAD_SURF) = IELTYP
          IAD_SURF=IAD_SURF+1
          BUFTMPSURF(IAD_SURF) = JJ
          IAD_SURF=IAD_SURF+1
        ENDIF ! IF (MY_LEV==NLEV)
      ENDDO ! DO J=1,NELEM
C------------
      RETURN
      END
