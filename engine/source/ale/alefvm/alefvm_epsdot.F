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
Chd|  ALEFVM_EPSDOT                 source/ale/alefvm/alefvm_epsdot.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|        ALEFVM_MOD                    ../common_source/modules/alefvm_mod.F
Chd|====================================================================
      SUBROUTINE ALEFVM_EPSDOT (
     1                          IXS,  NV46, SIG,  QVIS, VOL,
     2                          N1X,  N2X,  N3X,  N4X,  N5X,   N6X,
     3                          N1Y,  N2Y,  N3Y,  N4Y,  N5Y,   N6Y,
     4                          N1Z,  N2Z,  N3Z,  N4Z,  N5Z,   N6Z,
     5                          DXX,  DYY,  DZZ,  X  ,  IPM,   NEL)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C 'alefvm' is related to a collocated scheme (based on Godunov scheme)
C  which was temporarily introduced for experimental option /INTER/TYPE22 (FSI coupling with cut cell method)
C This cut cell method is not completed, abandonned, and is not an official option.
C There is no other use for this scheme which is automatically enabled when /INTER/TYPE22 is defined (INT22>0 => IALEFVM=1).
C
C This subroutine is treating an uncut cell.
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALEFVM_MOD
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
C-----------------------------------------------
C   D e s c r i p t i o n
C----------------------------------------------- 
C This subroutines computes strain rate tensor
C
C If option is not detected in input file then
C subroutine is unplugged
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IXS(NIXS,*),NV46,IPM(NPROPMI,*),NEL
      my_real :: SIG(NEL,6),QVIS(*),VOL(MVSIZ),DXX(MVSIZ),DYY(MVSIZ), DZZ(MVSIZ),X(3,*)
      my_real :: N1X(*), N2X(*),  N3X(*), N4X(*), N5X(*), N6X(*),
     .           N1Y(*), N2Y(*),  N3Y(*), N4Y(*), N5Y(*), N6Y(*),
     .           N1Z(*), N2Z(*),  N3Z(*), N4Z(*), N5Z(*), N6Z(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, II, IV, J, MT, IALEFVM_FLG
      my_real :: VALEL(MVSIZ)  , VALVOIS(NV46,MVSIZ), P(MVSIZ) , 
     .           S1(MVSIZ)     , S2(MVSIZ)          , S3(MVSIZ), 
     .           S4(MVSIZ)     , S5(MVSIZ)          , S6(MVSIZ),
     .           PVOIS(6,MVSIZ), Vface(3,6)         , GradV(3,3) ,
     .           EPSDOT(6) 
      INTEGER :: NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ),NC4(MVSIZ),NC5(MVSIZ),NC6(MVSIZ),NC7(MVSIZ),NC8(MVSIZ)
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), X5(MVSIZ), X6(MVSIZ), X7(MVSIZ), X8(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ), Y5(MVSIZ), Y6(MVSIZ), Y7(MVSIZ), Y8(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ), Z5(MVSIZ), Z6(MVSIZ), Z7(MVSIZ), Z8(MVSIZ)
     
      LOGICAL :: debug_outp
      INTEGER :: idbf,idbl
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------      
      IF(IALEFVM==0)RETURN
      MT          = IXS(1,NFT+LFT)
      IALEFVM_FLG = IPM(251,MT)
      IF(IALEFVM_FLG <= 1)RETURN
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
      ENDIF      

      !-------------------------------------------------------------!
      ! TOTAL TENSOR                                                !
      !-------------------------------------------------------------! 
      DO I=LFT,LLT   
        II           = I + NFT
        Vface(1:3,1) = F_FACE(1:3,1,II)
        Vface(1:3,2) = F_FACE(1:3,2,II)
        Vface(1:3,3) = F_FACE(1:3,3,II)
        Vface(1:3,4) = F_FACE(1:3,4,II)                
        Vface(1:3,5) = F_FACE(1:3,5,II)
        Vface(1:3,6) = F_FACE(1:3,6,II)                
      ENDDO  
      
      !-------------------------------------------------------------!
      ! VELOCITY GRADIENT : Grad(v(1:3)) \in Matrix(3,3)            !
      !-------------------------------------------------------------! 
      ![EPSDOT]=1/2 (grad V + t{gradV} )   -> EPSDOT_if = 1/2 (dvi/dxj + dvj/dxi )
      ! mean (grad V) = 1/V * Integral (grad V, dV) = 1/V * Integral (V (*) n , dS)   where (*) is tensorial product
      !               = 1/V * SUM(  Vf (*) nf . Sf )
      !               = 1/2/V * SUM (  Vf (*) Nf ) where Nf=2Sf.n
      
      ! WARNING : Tangentiel velocity is here missing since F_FACE is godunov normal velocity
      
      DO I=LFT,LLT
        II=I+NFT
        GradV(1,1) =  HALF/VOL(I) * ( Vface(1,1)*N1X(I) + Vface(1,2)*N2X(I) + Vface(1,3)*N3X(I) 
     .                                + Vface(1,4)*N4X(I) + Vface(1,5)*N5X(I) + Vface(1,6)*N6X(I) )
        GradV(1,2) =  HALF/VOL(I) * ( Vface(1,1)*N1Y(I) + Vface(1,2)*N2Y(I) + Vface(1,3)*N3Y(I) 
     .                                + Vface(1,4)*N4Y(I) + Vface(1,5)*N5Y(I) + Vface(1,6)*N6Y(I) )
        GradV(1,3) =  HALF/VOL(I) * ( Vface(1,1)*N1Z(I) + Vface(1,2)*N2Z(I) + Vface(1,3)*N3Z(I) 
     .                                + Vface(1,4)*N4Z(I) + Vface(1,5)*N5Z(I) + Vface(1,6)*N6Z(I) )
      
        GradV(2,1) =  HALF/VOL(I) * ( Vface(2,1)*N1X(I) + Vface(2,2)*N2X(I) + Vface(2,3)*N3X(I) 
     .                                + Vface(2,4)*N4X(I) + Vface(2,5)*N5X(I) + Vface(2,6)*N6X(I) )
        GradV(2,2) =  HALF/VOL(I) * ( Vface(2,1)*N1Y(I) + Vface(2,2)*N2Y(I) + Vface(2,3)*N3Y(I) 
     .                                + Vface(2,4)*N4Y(I) + Vface(2,5)*N5Y(I) + Vface(2,6)*N6Y(I) )
        GradV(2,3) =  HALF/VOL(I) * ( Vface(2,1)*N1Z(I) + Vface(2,2)*N2Z(I) + Vface(2,3)*N3Z(I) 
     .                                + Vface(2,4)*N4Z(I) + Vface(2,5)*N5Z(I) + Vface(2,6)*N6Z(I) )
     
        GradV(3,1) =  HALF/VOL(I) * ( Vface(3,1)*N1X(I) + Vface(3,2)*N2X(I) + Vface(3,3)*N3X(I) 
     .                                + Vface(3,4)*N4X(I) + Vface(3,5)*N5X(I) + Vface(3,6)*N6X(I) )      
        GradV(3,2) =  HALF/VOL(I) * ( Vface(3,1)*N1Y(I) + Vface(3,2)*N2Y(I) + Vface(3,3)*N3Y(I) 
     .                                + Vface(3,4)*N4Y(I) + Vface(3,5)*N5Y(I) + Vface(3,6)*N6Y(I) )    
        GradV(3,3) =  HALF/VOL(I) * ( Vface(3,1)*N1Z(I) + Vface(3,2)*N2Z(I) + Vface(3,3)*N3Z(I) 
     .                                + Vface(3,4)*N4Z(I) + Vface(3,5)*N5Z(I) + Vface(3,6)*N6Z(I) )    

        EPSDOT(1) = GradV(1,1)
        EPSDOT(2) = GradV(2,2)
        EPSDOT(3) = GradV(3,3)                
        EPSDOT(4) = HALF*(GradV(1,2)+GradV(2,1))
        EPSDOT(5) = HALF*(GradV(2,3)+GradV(3,2))
        EPSDOT(6) = HALF*(GradV(1,3)+GradV(3,1)) 
        
        DXX(I)    = EPSDOT(1)
        DYY(I)    = EPSDOT(2)
        DZZ(I)    = EPSDOT(3)                

        !14.0.220 : strain rate not yet validated. Set to zero while waiting for specification validation
        DXX(I)    = ZERO
        DYY(I)    = ZERO
        DZZ(I)    = ZERO   

      ENDDO
      

        !DEBUG-OUTPUT---------------!
        if(IALEFVM_OUTP_EPSDOT /= 0)then
          debug_outp = .FALSE.
          if(IALEFVM_OUTP_EPSDOT>0)then
            do i=lft,llt
              ii = nft + i
              if(ixs(11,ii)==IALEFVM_OUTP_EPSDOT)THEN
                debug_outp = .TRUE.
                idbf   = i
                idbl   = i
                EXIT
              endif
             enddo
          elseif(IALEFVM_OUTP_EPSDOT==-1)then
            debug_outp=.TRUE.
                idbf   = lft
                idbl   = llt          
          endif
          if(debug_outp)then     
!#!include "lockon.inc"       
          print *, "    |----alefvm_stress.F-----|"
          print *, "    |   THREAD INFORMATION   |"
          print *, "    |------------------------|" 
          print *, "     NCYCLE =", NCYCLE
          do i=idbf,idbl
            ii = nft + i
            print *,                    "      brique=", ixs(11,nft+i)
            write(*,FMT='(A24,1A26)')   "                        ",                     
     .                                  "#-stress Tensor (P+VIS+Q)#"          

!!            write (*,FMT='(A,3E26.14,A)')  "                            | ", GradV(1,1),GradV(1,2),GradV(1,3),   " |"
!!            write (*,FMT='(A,3E26.14,A)')  "           GradV           =| ", GradV(2,1),GradV(2,2),GradV(2,3),   " |"  
!!            write (*,FMT='(A,3E26.14,A)')  "                            |_", GradV(3,1),GradV(3,2),GradV(3,3),   "_|" 

!            write (*,FMT='(A,3E26.14,A)')  "                            | ", EPSDOT(1),EPSDOT(4),EPSDOT(6),   " |"
!            write (*,FMT='(A,3E26.14,A)')  "         Eps_dot           =| ", EPSDOT(4),EPSDOT(2),EPSDOT(5),   " |"  
!            write (*,FMT='(A,3E26.14,A)')  "                            |_", EPSDOT(6),EPSDOT(5),EPSDOT(3),   "_|" 
            write (*,FMT='(A,1E26.14  )')  "         tr(Eps_dot)/3.    =  ", THIRD*(EPSDOT(1)+EPSDOT(2)+EPSDOT(3))
 
          enddo
!#!include "lockoff.inc"       
        endif
        endif
      !-----------------------------------------!
        
      RETURN
      END
