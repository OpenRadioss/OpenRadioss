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
Chd|  SJACIDP                       source/elements/solid/solide/sjacidp.F
Chd|-- called by -----------
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SJACIDP(
     .              XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .              YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .              ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   ,
     .              JAC_I,NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER , INTENT(IN) :: NEL
      my_real , DIMENSION(10,NEL),INTENT(OUT) :: JAC_I     
      DOUBLE PRECISION , DIMENSION(MVSIZ), INTENT(IN) ::
     .   XD1, XD2, XD3, XD4, XD5, XD6, XD7, XD8,
     .   YD1, YD2, YD3, YD4, YD5, YD6, YD7, YD8,
     .   ZD1, ZD2, ZD3, ZD4, ZD5, ZD6, ZD7, ZD8
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I
C
      my_real
     .   JAC1(MVSIZ), JAC2(MVSIZ), JAC3(MVSIZ), 
     .   JAC4(MVSIZ), JAC5(MVSIZ), JAC6(MVSIZ),
     .   JAC7(MVSIZ), JAC8(MVSIZ), JAC9(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ)
C
      my_real
     .   DET(MVSIZ), DETT(MVSIZ), 
     .   X17(MVSIZ), X28(MVSIZ), X35(MVSIZ), X46(MVSIZ),
     .   Y17(MVSIZ), Y28(MVSIZ), Y35(MVSIZ), Y46(MVSIZ),
     .   Z17(MVSIZ), Z28(MVSIZ), Z35(MVSIZ), Z46(MVSIZ)
C=======================================================================
      DO I=1,NEL
        X17(I)=XD7(I)-XD1(I)
        X28(I)=XD8(I)-XD2(I)
        X35(I)=XD5(I)-XD3(I)
        X46(I)=XD6(I)-XD4(I)
C
        Y17(I)=YD7(I)-YD1(I)
        Y28(I)=YD8(I)-YD2(I)
        Y35(I)=YD5(I)-YD3(I)
        Y46(I)=YD6(I)-YD4(I)
C
        Z17(I)=ZD7(I)-ZD1(I)
        Z28(I)=ZD8(I)-ZD2(I)
        Z35(I)=ZD5(I)-ZD3(I)
        Z46(I)=ZD6(I)-ZD4(I)
      ENDDO
C
C Jacobian Matrix
      DO I=1,NEL               
        JAC1(I)=X17(I)+X28(I)-X35(I)-X46(I)
        JAC2(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
        JAC3(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
C
        JAC4(I)=X17(I)+X46(I)+X28(I)+X35(I)
        JAC5(I)=Y17(I)+Y46(I)+Y28(I)+Y35(I)
        JAC6(I)=Z17(I)+Z46(I)+Z28(I)+Z35(I)
C
        JAC7(I)=X17(I)+X46(I)-X28(I)-X35(I)
        JAC8(I)=Y17(I)+Y46(I)-Y28(I)-Y35(I)
        JAC9(I)=Z17(I)+Z46(I)-Z28(I)-Z35(I)
      ENDDO
C
      DO I=1,NEL
        JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
        JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
        JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
      ENDDO
C
      DO I=1,NEL
       DET(I) =ONE_OVER_64*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
       DETT(I)=ONE_OVER_64/MAX(DET(I),EM20)
      ENDDO     
C
C Jacobian matrix inverse
      DO I=1,NEL
        JAC_I(1,I)=DETT(I)*JAC_59_68(I)
        JAC_I(4,I)=DETT(I)*JAC_67_49(I)
        JAC_I(7,I)=DETT(I)*JAC_48_57(I)
        JAC_I(2,I)=DETT(I)*(-JAC2(I)*JAC9(I)+JAC3(I)*JAC8(I))
        JAC_I(5,I)=DETT(I)*( JAC1(I)*JAC9(I)-JAC3(I)*JAC7(I))
        JAC_I(8,I)=DETT(I)*(-JAC1(I)*JAC8(I)+JAC2(I)*JAC7(I))
        JAC_I(3,I)=DETT(I)*( JAC2(I)*JAC6(I)-JAC3(I)*JAC5(I))
        JAC_I(6,I)=DETT(I)*(-JAC1(I)*JAC6(I)+JAC3(I)*JAC4(I))
        JAC_I(9,I)=DETT(I)*( JAC1(I)*JAC5(I)-JAC2(I)*JAC4(I))
      ENDDO
C
      DO I=1,NEL
        JAC_I(10,I) = DET(I)
      ENDDO
C
      RETURN
      END SUBROUTINE SJACIDP
