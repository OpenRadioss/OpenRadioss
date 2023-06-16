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
Chd|  FVDEAL                        source/airbag/fvdeal.F        
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
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
#include      "implicit_f.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C
      IF (ALLOCATED(FVDATA)) THEN
         DO I=1,NFVBAG
            IF (ASSOCIATED(FVDATA(I)%IFVNOD)) 
     .              DEALLOCATE(FVDATA(I)%IFVNOD)
            IF (ASSOCIATED(FVDATA(I)%RFVNOD)) 
     .              DEALLOCATE(FVDATA(I)%RFVNOD)
            IF (ASSOCIATED(FVDATA(I)%IFVTRI)) 
     .              DEALLOCATE(FVDATA(I)%IFVTRI)
            IF (ASSOCIATED(FVDATA(I)%IFVPOLY)) 
     .              DEALLOCATE(FVDATA(I)%IFVPOLY)
            IF (ASSOCIATED(FVDATA(I)%IFVTADR)) 
     .              DEALLOCATE(FVDATA(I)%IFVTADR)
            IF (ASSOCIATED(FVDATA(I)%IFVPOLH)) 
     .              DEALLOCATE(FVDATA(I)%IFVPOLH)
            IF (ASSOCIATED(FVDATA(I)%IFVPADR)) 
     .              DEALLOCATE(FVDATA(I)%IFVPADR)
            IF (ASSOCIATED(FVDATA(I)%IDPOLH)) 
     .              DEALLOCATE(FVDATA(I)%IDPOLH)
            IF (ASSOCIATED(FVDATA(I)%IBPOLH)) 
     .              DEALLOCATE(FVDATA(I)%IBPOLH)
            IF (ASSOCIATED(FVDATA(I)%IFVPOLY_ANIM)) 
     .              DEALLOCATE(FVDATA(I)%IFVPOLY_ANIM)
            IF (ASSOCIATED(FVDATA(I)%IFVTADR_ANIM)) 
     .              DEALLOCATE(FVDATA(I)%IFVTADR_ANIM)
            IF (ASSOCIATED(FVDATA(I)%IFVPOLH_ANIM)) 
     .              DEALLOCATE(FVDATA(I)%IFVPOLH_ANIM)
            IF (ASSOCIATED(FVDATA(I)%IFVPADR_ANIM)) 
     .              DEALLOCATE(FVDATA(I)%IFVPADR_ANIM)
            IF (ASSOCIATED(FVDATA(I)%REDIR_ANIM)) 
     .              DEALLOCATE(FVDATA(I)%REDIR_ANIM)
            IF (ASSOCIATED(FVDATA(I)%NOD_ANIM)) 
     .              DEALLOCATE(FVDATA(I)%NOD_ANIM)
            IF (ASSOCIATED(FVDATA(I)%MPOLH)) 
     .              DEALLOCATE(FVDATA(I)%MPOLH)
            IF (ASSOCIATED(FVDATA(I)%QPOLH)) 
     .              DEALLOCATE(FVDATA(I)%QPOLH)
            IF (ASSOCIATED(FVDATA(I)%EPOLH)) 
     .              DEALLOCATE(FVDATA(I)%EPOLH)
            IF (ASSOCIATED(FVDATA(I)%PPOLH)) 
     .              DEALLOCATE(FVDATA(I)%PPOLH)
            IF (ASSOCIATED(FVDATA(I)%RPOLH)) 
     .              DEALLOCATE(FVDATA(I)%RPOLH)
            IF (ASSOCIATED(FVDATA(I)%GPOLH)) 
     .              DEALLOCATE(FVDATA(I)%GPOLH)
            IF (ASSOCIATED(FVDATA(I)%CPAPOLH)) 
     .              DEALLOCATE(FVDATA(I)%CPAPOLH)
            IF (ASSOCIATED(FVDATA(I)%CPBPOLH)) 
     .              DEALLOCATE(FVDATA(I)%CPBPOLH)
            IF (ASSOCIATED(FVDATA(I)%CPCPOLH)) 
     .              DEALLOCATE(FVDATA(I)%CPCPOLH)
            IF (ASSOCIATED(FVDATA(I)%RMWPOLH)) 
     .              DEALLOCATE(FVDATA(I)%RMWPOLH)
            IF (ASSOCIATED(FVDATA(I)%VPOLH_INI)) 
     .              DEALLOCATE(FVDATA(I)%VPOLH_INI)
            IF (ASSOCIATED(FVDATA(I)%DTPOLH)) 
     .              DEALLOCATE(FVDATA(I)%DTPOLH)
            IF (ASSOCIATED(FVDATA(I)%TPOLH)) 
     .              DEALLOCATE(FVDATA(I)%TPOLH)
            IF (ASSOCIATED(FVDATA(I)%CPDPOLH)) 
     .              DEALLOCATE(FVDATA(I)%CPDPOLH)
            IF (ASSOCIATED(FVDATA(I)%CPEPOLH)) 
     .              DEALLOCATE(FVDATA(I)%CPEPOLH)
            IF (ASSOCIATED(FVDATA(I)%CPFPOLH)) 
     .              DEALLOCATE(FVDATA(I)%CPFPOLH)
            IF (ASSOCIATED(FVDATA(I)%SSPPOLH)) 
     .              DEALLOCATE(FVDATA(I)%SSPPOLH)
            IF (ASSOCIATED(FVDATA(I)%CENTROID_POLH)) 
     .              DEALLOCATE(FVDATA(I)%CENTROID_POLH)     
         ENDDO
         DEALLOCATE(FVDATA)
      ENDIF
C
      IF (ALLOCATED(FVDATA_OLD)) THEN
         DO I=1,NFVBAG
            IF (ASSOCIATED(FVDATA_OLD(I)%IFVNOD)) 
     .              DEALLOCATE(FVDATA_OLD(I)%IFVNOD)
            IF (ASSOCIATED(FVDATA_OLD(I)%RFVNOD)) 
     .              DEALLOCATE(FVDATA_OLD(I)%RFVNOD)
            IF (ASSOCIATED(FVDATA_OLD(I)%IFVTRI)) 
     .              DEALLOCATE(FVDATA_OLD(I)%IFVTRI)
            IF (ASSOCIATED(FVDATA_OLD(I)%IFVPOLY)) 
     .              DEALLOCATE(FVDATA_OLD(I)%IFVPOLY)
            IF (ASSOCIATED(FVDATA_OLD(I)%IFVTADR)) 
     .              DEALLOCATE(FVDATA_OLD(I)%IFVTADR)
            IF (ASSOCIATED(FVDATA_OLD(I)%IFVPOLH)) 
     .              DEALLOCATE(FVDATA_OLD(I)%IFVPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%IFVPADR)) 
     .              DEALLOCATE(FVDATA_OLD(I)%IFVPADR)
            IF (ASSOCIATED(FVDATA_OLD(I)%IDPOLH)) 
     .              DEALLOCATE(FVDATA_OLD(I)%IDPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%IBPOLH)) 
     .              DEALLOCATE(FVDATA_OLD(I)%IBPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%IFVPOLY_ANIM)) 
     .              DEALLOCATE(FVDATA_OLD(I)%IFVPOLY_ANIM)
            IF (ASSOCIATED(FVDATA_OLD(I)%IFVTADR_ANIM)) 
     .              DEALLOCATE(FVDATA_OLD(I)%IFVTADR_ANIM)
            IF (ASSOCIATED(FVDATA_OLD(I)%IFVPOLH_ANIM)) 
     .              DEALLOCATE(FVDATA_OLD(I)%IFVPOLH_ANIM)
            IF (ASSOCIATED(FVDATA_OLD(I)%IFVPADR_ANIM)) 
     .              DEALLOCATE(FVDATA_OLD(I)%IFVPADR_ANIM)
            IF (ASSOCIATED(FVDATA_OLD(I)%REDIR_ANIM)) 
     .              DEALLOCATE(FVDATA_OLD(I)%REDIR_ANIM)
            IF (ASSOCIATED(FVDATA_OLD(I)%NOD_ANIM)) 
     .              DEALLOCATE(FVDATA_OLD(I)%NOD_ANIM)
            IF (ASSOCIATED(FVDATA_OLD(I)%MPOLH)) 
     .              DEALLOCATE(FVDATA_OLD(I)%MPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%QPOLH)) 
     .              DEALLOCATE(FVDATA_OLD(I)%QPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%EPOLH)) 
     .              DEALLOCATE(FVDATA_OLD(I)%EPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%PPOLH)) 
     .              DEALLOCATE(FVDATA_OLD(I)%PPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%RPOLH)) 
     .              DEALLOCATE(FVDATA_OLD(I)%RPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%GPOLH)) 
     .              DEALLOCATE(FVDATA_OLD(I)%GPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%CPAPOLH)) 
     .              DEALLOCATE(FVDATA_OLD(I)%CPAPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%CPBPOLH)) 
     .              DEALLOCATE(FVDATA_OLD(I)%CPBPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%CPCPOLH)) 
     .              DEALLOCATE(FVDATA_OLD(I)%CPCPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%RMWPOLH)) 
     .              DEALLOCATE(FVDATA_OLD(I)%RMWPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%VPOLH_INI)) 
     .              DEALLOCATE(FVDATA_OLD(I)%VPOLH_INI)
            IF (ASSOCIATED(FVDATA_OLD(I)%DTPOLH)) 
     .              DEALLOCATE(FVDATA_OLD(I)%DTPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%TPOLH)) 
     .              DEALLOCATE(FVDATA_OLD(I)%TPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%CPDPOLH)) 
     .              DEALLOCATE(FVDATA_OLD(I)%CPDPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%CPEPOLH)) 
     .              DEALLOCATE(FVDATA_OLD(I)%CPEPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%CPFPOLH)) 
     .              DEALLOCATE(FVDATA_OLD(I)%CPFPOLH)
            IF (ASSOCIATED(FVDATA_OLD(I)%SSPPOLH)) 
     .              DEALLOCATE(FVDATA_OLD(I)%SSPPOLH)   
            IF (ASSOCIATED(FVDATA_OLD(I)%CENTROID_POLH)) 
     .              DEALLOCATE(FVDATA_OLD(I)%CENTROID_POLH)      
         ENDDO
         DEALLOCATE(FVDATA_OLD)
      ENDIF
C
      IF (ALLOCATED(FVSPMD)) THEN
            DO I=1,NFVBAG
               IF (ASSOCIATED(FVSPMD(I)%IBUF_L)) 
     .            DEALLOCATE(FVSPMD(I)%IBUF_L)
               IF (ASSOCIATED(FVSPMD(I)%IBUFA_L)) 
     .            DEALLOCATE(FVSPMD(I)%IBUFA_L)
               IF (ASSOCIATED(FVSPMD(I)%IBUFSA_L)) 
     .            DEALLOCATE(FVSPMD(I)%IBUFSA_L)
               IF (ASSOCIATED(FVSPMD(I)%IXSA)) 
     .            DEALLOCATE(FVSPMD(I)%IXSA)
               IF (ASSOCIATED(FVSPMD(I)%ELEMSA)) 
     .            DEALLOCATE(FVSPMD(I)%ELEMSA)
               IF (ASSOCIATED(FVSPMD(I)%ITAB)) 
     .            DEALLOCATE(FVSPMD(I)%ITAB)
            ENDDO
            DEALLOCATE(FVSPMD)
      ENDIF
C
      RETURN
      END
