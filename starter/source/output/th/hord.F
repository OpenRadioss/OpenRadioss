Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
C
!||====================================================================
!||    hord                   ../starter/source/output/th/hord.F
!||--- called by ------------------------------------------------------
!||    hm_read_thgrki         ../starter/source/output/th/hm_read_thgrki.F
!||    hm_read_thgrki_rbody   ../starter/source/output/th/hm_read_thgrki_rbody.F
!||    hm_read_thgrsens       ../starter/source/output/th/hm_read_thgrsens.F
!||    hm_read_thgrsurf       ../starter/source/output/th/hm_read_thgrsurf.F
!||    hm_thgrki_vent         ../starter/source/output/th/hm_thgrki_vent.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
        SUBROUTINE HORD(NEL,NSEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      use my_alloc_mod
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NSEL
        INTEGER NEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I, MODE
        INTEGER, DIMENSION(:), ALLOCATABLE :: ITRI,INDX
        INTEGER, DIMENSION(70000) :: WORK
!   ---------------------------------------------
        call my_alloc(ITRI, NSEL, "ITRI")
        call my_alloc(INDX, 2*NSEL, "HORD/INDX")

        ITRI(1:NSEL) = NEL(1:NSEL)
        DO I=1,NSEL
            INDX(I) = I
        ENDDO
        MODE=0
        CALL MY_ORDERS( MODE, WORK, ITRI, INDX, NSEL , 1)
        
        DO I=1,NSEL
            NEL(I) = ITRI(INDX(I))
        ENDDO

        CALL MY_DEALLOC(ITRI)
        CALL MY_DEALLOC(INDX)

        RETURN
        END SUBROUTINE HORD
