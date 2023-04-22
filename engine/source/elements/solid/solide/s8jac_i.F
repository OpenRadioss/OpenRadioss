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
Chd|  S8JAC_I                       source/elements/solid/solide/s8jac_i.F
Chd|-- called by -----------
Chd|        S8UPD11T12                    source/elements/solid/solide/s8upd11t12.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S8JAC_I(
     .              X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .              Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .              Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .              JAC_I,NEL  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C------------------------------------------------------
C------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NEL
      my_real :: JAC_I(10,*)
      DOUBLE PRECISION
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J     
      my_real
     .   JAC1(MVSIZ), JAC2(MVSIZ), JAC3(MVSIZ), 
     .   JAC4(MVSIZ), JAC5(MVSIZ), JAC6(MVSIZ),
     .   JAC7(MVSIZ), JAC8(MVSIZ), JAC9(MVSIZ)
      my_real
     .   DET(MVSIZ), DETT(MVSIZ), 
     .   X17(MVSIZ), X28(MVSIZ), X35(MVSIZ), X46(MVSIZ),
     .   Y17(MVSIZ), Y28(MVSIZ), Y35(MVSIZ), Y46(MVSIZ),
     .   Z17(MVSIZ), Z28(MVSIZ), Z35(MVSIZ), Z46(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   A17_46(MVSIZ),
     .   A28_35(MVSIZ),
     .   B17_46(MVSIZ),
     .   B28_35(MVSIZ),
     .   C17_46(MVSIZ),
     .   C28_35(MVSIZ)
C=======================================================================
      DO I=1,NEL
        X17(I)=X7(I)-X1(I)
        X28(I)=X8(I)-X2(I)
        X35(I)=X5(I)-X3(I)
        X46(I)=X6(I)-X4(I)
C
        Y17(I)=Y7(I)-Y1(I)
        Y28(I)=Y8(I)-Y2(I)
        Y35(I)=Y5(I)-Y3(I)
        Y46(I)=Y6(I)-Y4(I)
C
        Z17(I)=Z7(I)-Z1(I)
        Z28(I)=Z8(I)-Z2(I)
        Z35(I)=Z5(I)-Z3(I)
        Z46(I)=Z6(I)-Z4(I)
      ENDDO

      DO I=1,NEL
        JAC1(I)=X17(I)+X28(I)-X35(I)-X46(I)
        JAC2(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
        JAC3(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
        A17_46(I)=X17(I)+X46(I)
        A28_35(I)=X28(I)+X35(I)
        B17_46(I)=Y17(I)+Y46(I)
        B28_35(I)=Y28(I)+Y35(I)
        C17_46(I)=Z17(I)+Z46(I)
        C28_35(I)=Z28(I)+Z35(I)
      ENDDO
         
      DO I=1,NEL
        JAC4(I)=A17_46(I)+A28_35(I)
        JAC5(I)=B17_46(I)+B28_35(I)
        JAC6(I)=C17_46(I)+C28_35(I)
        JAC7(I)=A17_46(I)-A28_35(I)
        JAC8(I)=B17_46(I)-B28_35(I)
        JAC9(I)=C17_46(I)-C28_35(I)
      ENDDO

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
C Inverse Jacobian Matrix
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
      END
