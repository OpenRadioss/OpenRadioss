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
!||====================================================================
!||    fvdeal           ../engine/source/airbag/fvdeal.F
!||--- called by ------------------------------------------------------
!||    radioss2         ../engine/source/engine/radioss2.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    fvbag_mod        ../engine/share/modules/fvbag_mod.F
!||    my_dealloc_mod   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
      SUBROUTINE FVDEAL()            
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C DEALLOCATE 
C   (FVDATA data structure)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      use my_dealloc_mod, only : my_dealloc
#include      "implicit_f.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C
      IF (ALLOCATED(FVDATA)) THEN
         DO I=1,NFVBAG
            IF (ASSOCIATED(FVDATA(I)%IFVNOD)) call my_dealloc(FVDATA(I)%IFVNOD)
            IF (ASSOCIATED(FVDATA(I)%RFVNOD)) call my_dealloc(FVDATA(I)%RFVNOD)
            IF (ASSOCIATED(FVDATA(I)%IFVTRI)) call my_dealloc(FVDATA(I)%IFVTRI)
            IF (ASSOCIATED(FVDATA(I)%IFVPOLY)) call my_dealloc(FVDATA(I)%IFVPOLY)
            IF (ASSOCIATED(FVDATA(I)%IFVTADR)) call my_dealloc(FVDATA(I)%IFVTADR)
            IF (ASSOCIATED(FVDATA(I)%IFVPOLH)) call my_dealloc(FVDATA(I)%IFVPOLH)
            IF (ASSOCIATED(FVDATA(I)%IFVPADR)) call my_dealloc(FVDATA(I)%IFVPADR)
            IF (ASSOCIATED(FVDATA(I)%IDPOLH)) call my_dealloc(FVDATA(I)%IDPOLH)
            IF (ASSOCIATED(FVDATA(I)%IBPOLH)) call my_dealloc(FVDATA(I)%IBPOLH)
            IF (ASSOCIATED(FVDATA(I)%IFVPOLY_ANIM)) call my_dealloc(FVDATA(I)%IFVPOLY_ANIM)
            IF (ASSOCIATED(FVDATA(I)%IFVTADR_ANIM)) call my_dealloc(FVDATA(I)%IFVTADR_ANIM)
            IF (ASSOCIATED(FVDATA(I)%IFVPOLH_ANIM)) call my_dealloc(FVDATA(I)%IFVPOLH_ANIM)
            IF (ASSOCIATED(FVDATA(I)%IFVPADR_ANIM)) call my_dealloc(FVDATA(I)%IFVPADR_ANIM)
            IF (ASSOCIATED(FVDATA(I)%REDIR_ANIM)) call my_dealloc(FVDATA(I)%REDIR_ANIM)
            IF (ASSOCIATED(FVDATA(I)%NOD_ANIM)) call my_dealloc(FVDATA(I)%NOD_ANIM)
            IF (ASSOCIATED(FVDATA(I)%MPOLH)) call my_dealloc(FVDATA(I)%MPOLH)
            IF (ASSOCIATED(FVDATA(I)%QPOLH)) call my_dealloc(FVDATA(I)%QPOLH)
            IF (ASSOCIATED(FVDATA(I)%EPOLH)) call my_dealloc(FVDATA(I)%EPOLH)
            IF (ASSOCIATED(FVDATA(I)%PPOLH)) call my_dealloc(FVDATA(I)%PPOLH)
            IF (ASSOCIATED(FVDATA(I)%RPOLH)) call my_dealloc(FVDATA(I)%RPOLH)
            IF (ASSOCIATED(FVDATA(I)%GPOLH)) call my_dealloc(FVDATA(I)%GPOLH)
            IF (ASSOCIATED(FVDATA(I)%CPAPOLH)) call my_dealloc(FVDATA(I)%CPAPOLH)
            IF (ASSOCIATED(FVDATA(I)%CPBPOLH)) call my_dealloc(FVDATA(I)%CPBPOLH)
            IF (ASSOCIATED(FVDATA(I)%CPCPOLH)) call my_dealloc(FVDATA(I)%CPCPOLH)
            IF (ASSOCIATED(FVDATA(I)%RMWPOLH)) call my_dealloc(FVDATA(I)%RMWPOLH)
            IF (ASSOCIATED(FVDATA(I)%VPOLH_INI)) call my_dealloc(FVDATA(I)%VPOLH_INI)
            IF (ASSOCIATED(FVDATA(I)%DTPOLH)) call my_dealloc(FVDATA(I)%DTPOLH)
            IF (ASSOCIATED(FVDATA(I)%TPOLH)) call my_dealloc(FVDATA(I)%TPOLH)
            IF (ASSOCIATED(FVDATA(I)%CPDPOLH)) call my_dealloc(FVDATA(I)%CPDPOLH)
            IF (ASSOCIATED(FVDATA(I)%CPEPOLH)) call my_dealloc(FVDATA(I)%CPEPOLH)
            IF (ASSOCIATED(FVDATA(I)%CPFPOLH)) call my_dealloc(FVDATA(I)%CPFPOLH)
            IF (ASSOCIATED(FVDATA(I)%SSPPOLH)) call my_dealloc(FVDATA(I)%SSPPOLH)
            IF (ASSOCIATED(FVDATA(I)%CENTROID_POLH)) call my_dealloc(FVDATA(I)%CENTROID_POLH)
         ENDDO
         DEALLOCATE(FVDATA)
      ENDIF
C
      IF (ALLOCATED(FVDATA_OLD)) THEN
         DO I=1,NFVBAG
            IF (ASSOCIATED(FVDATA_OLD(I)%IFVNOD)) call my_dealloc(FVDATA_OLD(I)%IFVNOD)
            IF (ASSOCIATED(FVDATA_OLD(I)%RFVNOD)) call my_dealloc(FVDATA_OLD(I)%RFVNOD)
            IF (ASSOCIATED(FVDATA_OLD(I)%IFVTRI)) call my_dealloc(FVDATA_OLD(I)%IFVTRI)
            IF (ASSOCIATED(FVDATA_OLD(I)%IFVPOLY)) call my_dealloc(FVDATA_OLD(I)%IFVPOLY)
            IF (ASSOCIATED(FVDATA_OLD(I)%IFVTADR)) call my_dealloc(FVDATA_OLD(I)%IFVTADR)
            IF (ASSOCIATED(FVDATA_OLD(I)%IFVPOLH)) call my_dealloc(FVDATA_OLD(I)%IFVPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%IFVPADR)) call my_dealloc(FVDATA_OLD(I)%IFVPADR)
            IF (ASSOCIATED(FVDATA_OLD(I)%IDPOLH)) call my_dealloc(FVDATA_OLD(I)%IDPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%IBPOLH)) call my_dealloc(FVDATA_OLD(I)%IBPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%IFVPOLY_ANIM)) call my_dealloc(FVDATA_OLD(I)%IFVPOLY_ANIM)
            IF (ASSOCIATED(FVDATA_OLD(I)%IFVTADR_ANIM)) call my_dealloc(FVDATA_OLD(I)%IFVTADR_ANIM)
            IF (ASSOCIATED(FVDATA_OLD(I)%IFVPOLH_ANIM)) call my_dealloc(FVDATA_OLD(I)%IFVPOLH_ANIM)
            IF (ASSOCIATED(FVDATA_OLD(I)%IFVPADR_ANIM)) call my_dealloc(FVDATA_OLD(I)%IFVPADR_ANIM)
            IF (ASSOCIATED(FVDATA_OLD(I)%REDIR_ANIM)) call my_dealloc(FVDATA_OLD(I)%REDIR_ANIM)
            IF (ASSOCIATED(FVDATA_OLD(I)%NOD_ANIM)) call my_dealloc(FVDATA_OLD(I)%NOD_ANIM)
            IF (ASSOCIATED(FVDATA_OLD(I)%MPOLH)) call my_dealloc(FVDATA_OLD(I)%MPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%QPOLH)) call my_dealloc(FVDATA_OLD(I)%QPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%EPOLH)) call my_dealloc(FVDATA_OLD(I)%EPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%PPOLH)) call my_dealloc(FVDATA_OLD(I)%PPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%RPOLH)) call my_dealloc(FVDATA_OLD(I)%RPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%GPOLH)) call my_dealloc(FVDATA_OLD(I)%GPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%CPAPOLH)) call my_dealloc(FVDATA_OLD(I)%CPAPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%CPBPOLH)) call my_dealloc(FVDATA_OLD(I)%CPBPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%CPCPOLH)) call my_dealloc(FVDATA_OLD(I)%CPCPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%RMWPOLH)) call my_dealloc(FVDATA_OLD(I)%RMWPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%VPOLH_INI)) call my_dealloc(FVDATA_OLD(I)%VPOLH_INI)
            IF (ASSOCIATED(FVDATA_OLD(I)%DTPOLH)) call my_dealloc(FVDATA_OLD(I)%DTPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%TPOLH)) call my_dealloc(FVDATA_OLD(I)%TPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%CPDPOLH)) call my_dealloc(FVDATA_OLD(I)%CPDPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%CPEPOLH)) call my_dealloc(FVDATA_OLD(I)%CPEPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%CPFPOLH)) call my_dealloc(FVDATA_OLD(I)%CPFPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%SSPPOLH)) call my_dealloc(FVDATA_OLD(I)%SSPPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%CENTROID_POLH)) call my_dealloc(FVDATA_OLD(I)%CENTROID_POLH)
         ENDDO
         DEALLOCATE(FVDATA_OLD)
      ENDIF
C
      IF (ALLOCATED(FVSPMD)) THEN
            DO I=1,NFVBAG
               IF (ASSOCIATED(FVSPMD(I)%IBUF_L)) call my_dealloc(FVSPMD(I)%IBUF_L)
               IF (ASSOCIATED(FVSPMD(I)%IBUFA_L)) call my_dealloc(FVSPMD(I)%IBUFA_L)
               IF (ASSOCIATED(FVSPMD(I)%IBUFSA_L)) call my_dealloc(FVSPMD(I)%IBUFSA_L)
               IF (ASSOCIATED(FVSPMD(I)%IXSA)) call my_dealloc(FVSPMD(I)%IXSA)
               IF (ASSOCIATED(FVSPMD(I)%ELEMSA)) call my_dealloc(FVSPMD(I)%ELEMSA)
               IF (ASSOCIATED(FVSPMD(I)%ITAB)) call my_dealloc(FVSPMD(I)%ITAB)
            ENDDO
            DEALLOCATE(FVSPMD)
      ENDIF
C
      RETURN
      END
