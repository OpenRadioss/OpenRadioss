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
Chd|  ALEFVM_TFEXT                  source/ale/alefvm/alefvm_tfext.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|        ALEFVM_MOD                    ../common_source/modules/alefvm_mod.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE ALEFVM_TFEXT  (
     1                          IXS,  NV46, SIG,  QVIS, ALE_CONNECT, VOL ,
     2                          N1X,  N2X,  N3X,  N4X,  N5X,   N6X ,
     3                          N1Y,  N2Y,  N3Y,  N4Y,  N5Y,   N6Y ,
     4                          N1Z,  N2Z,  N3Z,  N4Z,  N5Z,   N6Z ,
     5                          DXX,  DYY,  DZZ,  X  ,  IPM,   NALE,
     6                          W  ,  P  ,  VEUL, NEL)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C 'alefvm' is related to a collocated scheme (built from FVM and based on Godunov scheme)
C  which was temporarily introduced for experimental option /INTER/TYPE22 (FSI coupling with cut cell method)
C This cut cell method is not completed, abandonned, and is not an official option.
C There is no other use for this scheme which is automatically enabled when /INTER/TYPE22 is defined (INT22>0 => IALEFVM=1).
C
C This subroutine is treating an uncut cell.
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALEFVM_MOD
      USE ALE_CONNECTIVITY_MOD
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
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "nsvis_c.inc"
#include      "fluid_scr_c.inc"
#include      "inter22.inc"
#include      "param_c.inc"
#include      "alefvm.inc"
#include      "task_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D e s c r i p t i o n
C----------------------------------------------- 
C This subroutines computes External force
C work for internal energy computation.
C
CC WARNING THIS SUBROUTINE CAN BE OPTIMIZED (TEMPORARY ONE)
C
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IXS(NIXS,*),NV46,IPM(NPROPMI,*),NALE(*),NEL
      my_real :: SIG(NEL,6),QVIS(*),VOL(MVSIZ),DXX(MVSIZ),DYY(MVSIZ), DZZ(MVSIZ),X(3,*),W(3,*),P(MVSIZ)
      my_real :: N1X(*), N2X(*),  N3X(*), N4X(*), N5X(*), N6X(*),
     .           N1Y(*), N2Y(*),  N3Y(*), N4Y(*), N5Y(*), N6Y(*),
     .           N1Z(*), N2Z(*),  N3Z(*), N4Z(*), N5Z(*), N6Z(*),
     .           VEUL(LVEUL,*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, II, IV, J, MT, IALEFVM_FLG, ICF(4,6),IX(4)
      my_real :: VALEL(MVSIZ)  , VALVOIS(NV46,MVSIZ),
     .           S1(MVSIZ)     , S2(MVSIZ)          , S3(MVSIZ),
     .           S4(MVSIZ)     , S5(MVSIZ)          , S6(MVSIZ),
     .           PVOIS(6,MVSIZ), Vface(3,6)         , GradV(3,3) ,
     .           EPSDOT(6)
      INTEGER :: NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ),NC4(MVSIZ),NC5(MVSIZ),NC6(MVSIZ),NC7(MVSIZ),NC8(MVSIZ)
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ) , X4(MVSIZ), X5(MVSIZ), X6(MVSIZ), X7(MVSIZ), X8(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ) , Y4(MVSIZ), Y5(MVSIZ), Y6(MVSIZ), Y7(MVSIZ), Y8(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ) , Z4(MVSIZ), Z5(MVSIZ), Z6(MVSIZ), Z7(MVSIZ), Z8(MVSIZ),
     .   SWn(6)   ,  Wface(3,6,MVSIZ), WFEXT_ADD, TFEXTT

      DATA ICF/1,4,3,2,3,4,8,7,5,6,7,8,1,2,6,5,2,3,7,6,1,5,8,4/
      INTEGER :: IAD2
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------      
      IF(IALEFVM==0)RETURN
      MT          = IXS(1,NFT+LFT)
      IALEFVM_FLG = IPM(251,MT)
      IF(IALEFVM_FLG <= 1)RETURN

      IF(JALE == 0)RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------

      !-------------------------------------------------------------!
      ! NORMAL VECTOR FOR ALE                                       !
      !-------------------------------------------------------------!
      IF(JALE==1)THEN
        DO I=LFT,LLT
          II     = I + NFT
	  !---8 local node numbers NC1 TO NC8 for solid element I ---!
          NC1(I)=IXS(2,II)
          NC2(I)=IXS(3,II)
          NC3(I)=IXS(4,II)
          NC4(I)=IXS(5,II)
          NC5(I)=IXS(6,II)
          NC6(I)=IXS(7,II)
          NC7(I)=IXS(8,II)
          NC8(I)=IXS(9,II)
	  !
          !---Coordinates of the 8 nodes
          X1(I)=X(1,NC1(I))
          Y1(I)=X(2,NC1(I))
          Z1(I)=X(3,NC1(I))
          !
          X2(I)=X(1,NC2(I))
          Y2(I)=X(2,NC2(I))
          Z2(I)=X(3,NC2(I))
          !
          X3(I)=X(1,NC3(I))
          Y3(I)=X(2,NC3(I))
          Z3(I)=X(3,NC3(I))
          !
          X4(I)=X(1,NC4(I))
          Y4(I)=X(2,NC4(I))
          Z4(I)=X(3,NC4(I))
          !
          X5(I)=X(1,NC5(I))
          Y5(I)=X(2,NC5(I))
          Z5(I)=X(3,NC5(I))
          !
          X6(I)=X(1,NC6(I))
          Y6(I)=X(2,NC6(I))
          Z6(I)=X(3,NC6(I))
          !
          X7(I)=X(1,NC7(I))
          Y7(I)=X(2,NC7(I))
          Z7(I)=X(3,NC7(I))
          !
          X8(I)=X(1,NC8(I))
          Y8(I)=X(2,NC8(I))
          Z8(I)=X(3,NC8(I))
        ENDDO
        DO I=LFT,LLT
          ! Face-1
          N1X(I)=(Y3(I)-Y1(I))*(Z2(I)-Z4(I)) - (Z3(I)-Z1(I))*(Y2(I)-Y4(I))
          N1Y(I)=(Z3(I)-Z1(I))*(X2(I)-X4(I)) - (X3(I)-X1(I))*(Z2(I)-Z4(I))
          N1Z(I)=(X3(I)-X1(I))*(Y2(I)-Y4(I)) - (Y3(I)-Y1(I))*(X2(I)-X4(I))
          ! Face-2
          N2X(I)=(Y7(I)-Y4(I))*(Z3(I)-Z8(I)) - (Z7(I)-Z4(I))*(Y3(I)-Y8(I))
          N2Y(I)=(Z7(I)-Z4(I))*(X3(I)-X8(I)) - (X7(I)-X4(I))*(Z3(I)-Z8(I))
          N2Z(I)=(X7(I)-X4(I))*(Y3(I)-Y8(I)) - (Y7(I)-Y4(I))*(X3(I)-X8(I))
          ! Face-3
          N3X(I)=(Y6(I)-Y8(I))*(Z7(I)-Z5(I)) - (Z6(I)-Z8(I))*(Y7(I)-Y5(I))
          N3Y(I)=(Z6(I)-Z8(I))*(X7(I)-X5(I)) - (X6(I)-X8(I))*(Z7(I)-Z5(I))
          N3Z(I)=(X6(I)-X8(I))*(Y7(I)-Y5(I)) - (Y6(I)-Y8(I))*(X7(I)-X5(I))
          ! Face-4
          N4X(I)=(Y2(I)-Y5(I))*(Z6(I)-Z1(I)) - (Z2(I)-Z5(I))*(Y6(I)-Y1(I))
          N4Y(I)=(Z2(I)-Z5(I))*(X6(I)-X1(I)) - (X2(I)-X5(I))*(Z6(I)-Z1(I))
          N4Z(I)=(X2(I)-X5(I))*(Y6(I)-Y1(I)) - (Y2(I)-Y5(I))*(X6(I)-X1(I))
          ! Face-5
          N5X(I)=(Y7(I)-Y2(I))*(Z6(I)-Z3(I)) - (Z7(I)-Z2(I))*(Y6(I)-Y3(I))
          N5Y(I)=(Z7(I)-Z2(I))*(X6(I)-X3(I)) - (X7(I)-X2(I))*(Z6(I)-Z3(I))
          N5Z(I)=(X7(I)-X2(I))*(Y6(I)-Y3(I)) - (Y7(I)-Y2(I))*(X6(I)-X3(I))
          ! Face-6
          N6X(I)=(Y8(I)-Y1(I))*(Z4(I)-Z5(I)) - (Z8(I)-Z1(I))*(Y4(I)-Y5(I))
          N6Y(I)=(Z8(I)-Z1(I))*(X4(I)-X5(I)) - (X8(I)-X1(I))*(Z4(I)-Z5(I))
          N6Z(I)=(X8(I)-X1(I))*(Y4(I)-Y5(I)) - (Y8(I)-Y1(I))*(X4(I)-X5(I))
        ENDDO

        DO I=LFT,LLT
          Wface(1,1,I) = FOURTH*(W(1,NC1(I))+W(1,NC2(I))+W(1,NC3(I))+W(1,NC4(I)))
          Wface(2,1,I) = FOURTH*(W(2,NC1(I))+W(2,NC2(I))+W(2,NC3(I))+W(2,NC4(I)))
          Wface(3,1,I) = FOURTH*(W(3,NC1(I))+W(3,NC2(I))+W(3,NC3(I))+W(3,NC4(I)))

          Wface(1,2,I) = FOURTH*(W(1,NC3(I))+W(1,NC4(I))+W(1,NC7(I))+W(1,NC8(I)))
          Wface(2,2,I) = FOURTH*(W(2,NC3(I))+W(2,NC4(I))+W(2,NC7(I))+W(2,NC8(I)))
          Wface(3,2,I) = FOURTH*(W(3,NC3(I))+W(3,NC4(I))+W(3,NC7(I))+W(3,NC8(I)))

          Wface(1,3,I) = FOURTH*(W(1,NC5(I))+W(1,NC6(I))+W(1,NC7(I))+W(1,NC8(I)))
          Wface(2,3,I) = FOURTH*(W(2,NC5(I))+W(2,NC6(I))+W(2,NC7(I))+W(2,NC8(I)))
          Wface(3,3,I) = FOURTH*(W(3,NC5(I))+W(3,NC6(I))+W(3,NC7(I))+W(3,NC8(I)))

          Wface(1,4,I) = FOURTH*(W(1,NC1(I))+W(1,NC2(I))+W(1,NC5(I))+W(1,NC6(I)))
          Wface(2,4,I) = FOURTH*(W(2,NC1(I))+W(2,NC2(I))+W(2,NC5(I))+W(2,NC6(I)))
          Wface(3,4,I) = FOURTH*(W(3,NC1(I))+W(3,NC2(I))+W(3,NC5(I))+W(3,NC6(I)))

          Wface(1,5,I) = FOURTH*(W(1,NC2(I))+W(1,NC3(I))+W(1,NC6(I))+W(1,NC7(I)))
          Wface(2,5,I) = FOURTH*(W(2,NC2(I))+W(2,NC3(I))+W(2,NC6(I))+W(2,NC7(I)))
          Wface(3,5,I) = FOURTH*(W(3,NC2(I))+W(3,NC3(I))+W(3,NC6(I))+W(3,NC7(I)))

          Wface(1,6,I) = FOURTH*(W(1,NC1(I))+W(1,NC4(I))+W(1,NC5(I))+W(1,NC8(I)))
          Wface(2,6,I) = FOURTH*(W(2,NC1(I))+W(2,NC4(I))+W(2,NC5(I))+W(2,NC8(I)))
          Wface(3,6,I) = FOURTH*(W(3,NC1(I))+W(3,NC4(I))+W(3,NC5(I))+W(3,NC8(I)))
        ENDDO

      !-------------------------------------------------------------!
      ! NORMAL VECTOR FOR EULER                                     !
      !-------------------------------------------------------------!
      !ELSE
      !  DO I=LFT,LLT
      !    II = I+NFT
      !    N1X(I)=VEUL(14,II)
      !    N2X(I)=VEUL(15,II)
      !    N3X(I)=VEUL(16,II)
      !    N4X(I)=VEUL(17,II)
      !    N5X(I)=VEUL(18,II)
      !    N6X(I)=VEUL(19,II)
      !    N1Y(I)=VEUL(20,II)
      !    N2Y(I)=VEUL(21,II)
      !    N3Y(I)=VEUL(22,II)
      !    N4Y(I)=VEUL(23,II)
      !    N5Y(I)=VEUL(24,II)
      !    N6Y(I)=VEUL(25,II)
      !    N1Z(I)=VEUL(26,II)
      !    N2Z(I)=VEUL(27,II)
      !    N3Z(I)=VEUL(28,II)
      !    N4Z(I)=VEUL(29,II)
      !    N5Z(I)=VEUL(30,II)
      !    N6Z(I)=VEUL(31,II)
      !  ENDDO
      !  DO I=LFT,LLT
      !    Wface(1:3,1:6,I) = ZERO
      !  ENDDO

      ENDIF

      !-------------------------------------------------------------!
      ! EXTERNAL FORCE WORK                                         !
      !-------------------------------------------------------------!
      WFEXT_CELL(LFT+NFT:LLT+NFT) = ZERO
      TFEXTT                      = ZERO
      DO I=LFT,LLT
        II                        = I + NFT
        IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
        ! < W, 2S.n> = 2S.wn
        SWn(1)                    = Wface(1,1,I)*N1X(I) + Wface(2,1,I)*N1Y(I) + Wface(3,1,I)*N1Z(I)
        SWn(2)                    = Wface(1,2,I)*N2X(I) + Wface(2,2,I)*N2Y(I) + Wface(3,2,I)*N2Z(I)
        SWn(3)                    = Wface(1,3,I)*N3X(I) + Wface(2,3,I)*N3Y(I) + Wface(3,3,I)*N3Z(I)
        SWn(4)                    = Wface(1,4,I)*N4X(I) + Wface(2,4,I)*N4Y(I) + Wface(3,4,I)*N4Z(I)
        SWn(5)                    = Wface(1,5,I)*N5X(I) + Wface(2,5,I)*N5Y(I) + Wface(3,5,I)*N5Z(I)
        SWn(6)                    = Wface(1,6,I)*N6X(I) + Wface(2,6,I)*N6Y(I) + Wface(3,6,I)*N6Z(I)
        WFEXT_ADD                 = ZERO
        Do J=1,NV46
          IV = ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
          !IF(IV /= 0)CYCLE                                   !domain border only
          IX(1)                   = IXS(ICF(1,J)+1,II)
          IX(2)                   = IXS(ICF(2,J)+1,II)
          IX(3)                   = IXS(ICF(3,J)+1,II)
          IX(4)                   = IXS(ICF(4,J)+1,II)
          IF(SUM(IABS(NALE(IX(1:4))))==4)CYCLE          !4 lagrangian nodes only
          !dt * P*S*wn
          WFEXT_ADD               = WFEXT_ADD  - DT1*THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))*SWn(J)*HALF  !Swn=2S.w.n
        ENDDO
        WFEXT_CELL(II)            = WFEXT_ADD
        TFEXTT                    = TFEXTT + WFEXT_ADD
      ENDDO

#include "lockon.inc"
      TFEXT = TFEXT + TFEXTT
#include "lockoff.inc"



        !DEBUG-OUTPUT---------------!
        if(IALEFVM_OUTP_TFEXT == 1)then
          IF(TFEXTT /= ZERO)THEN
!#!include "lockon.inc"
            print *, "    |----alefvm_tfext.F------|"
            print *, "    |   THREAD INFORMATION   |"
            print *, "    |------------------------|"
            print *, "     NCYCLE =", NCYCLE
            do i=lft,llt
              ii = nft + i
              if(WFEXT_CELL(II)==ZERO)CYCLE
              write(*,FMT='(A,I10,A,F30.16,A,F30.16,A,6F30.16)')   "      brique=", ixs(11,ii),"      Wfext=",
     .             WFEXT_CELL(II), "P(I)=", P(I), "SWn(1:6)=",Swn(1:6)
            enddo
          ENDIF
!#!include "lockoff.inc"
        endif
      !-----------------------------------------!

      RETURN
      END
