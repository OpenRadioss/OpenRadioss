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
Chd|  ALEFVM_STRESS                 source/ale/alefvm/alefvm_stress.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|        ALEFVM_MOD                    ../common_source/modules/alefvm_mod.F
Chd|====================================================================
      SUBROUTINE ALEFVM_STRESS (
     1                          IXS,  NV46, SIG,  QVIS, 
     2                          N1X,  N2X,  N3X,  N4X,  N5X,  N6X,
     3                          N1Y,  N2Y,  N3Y,  N4Y,  N5Y,  N6Y,
     4                          N1Z,  N2Z,  N3Z,  N4Z,  N5Z,  N6Z,
     5                          X  ,  IPM,  RHO,  VOL,  A  ,  IAD22,
     .                          NEL,  MOM,  SSP)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C 'alefvm' is related to a collocated scheme (built from FVM and based on Godunov scheme)
C  which was temporarily introduced for experimental option /INTER/TYPE22 (FSI coupling with cut cell method)
C This cut cell method is not completed, abandoned, and is not an official option.
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
#include      "nsvis_c.inc"
#include      "param_c.inc"
#include      "alefvm.inc"
C-----------------------------------------------
C   D e s c r i p t i o n
C----------------------------------------------- 
C This subroutines computes internal forces for
C finite volume scheme (IALEFVM==1)
C
C If option is not detected in input file then
C subroutine is unplugged
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NEL
      INTEGER :: IXS(NIXS,*),NV46,IPM(NPROPMI,*)
      my_real :: SIG(NEL,6),QVIS(*),X(3,*),RHO(*), VOL(*), A(3,*), IAD22(*)
      my_real,INTENT(IN) :: MOM(NEL,3), SSP(*)
      my_real :: N1X(*), N2X(*),  N3X(*), N4X(*), N5X(*), N6X(*),
     .           N1Y(*), N2Y(*),  N3Y(*), N4Y(*), N5Y(*), N6Y(*),
     .           N1Z(*), N2Z(*),  N3Z(*), N4Z(*), N5Z(*), N6Z(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, II, IV, J, IMAT, IALEFVM_FLG, IB
      INTEGER :: NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ),NC4(MVSIZ),NC5(MVSIZ),NC6(MVSIZ),NC7(MVSIZ),NC8(MVSIZ)
      
      my_real :: VALEL(MVSIZ)  , VALVOIS(NV46,MVSIZ), P(MVSIZ) , 
     .           S1(MVSIZ)     , S2(MVSIZ)          , S3(MVSIZ), 
     .           S4(MVSIZ)     , S5(MVSIZ)          , S6(MVSIZ),
     .           PVOIS(6,MVSIZ), FN                 , FAC      , MASS
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), X5(MVSIZ), X6(MVSIZ), X7(MVSIZ), X8(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ), Y5(MVSIZ), Y6(MVSIZ), Y7(MVSIZ), Y8(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ), Z5(MVSIZ), Z6(MVSIZ), Z7(MVSIZ), Z8(MVSIZ),
     .   U_N(6), SURF(6), NORM(6)
     
      my_real :: trSIG(mVSIZ), trSVIS(MVSIZ),AF(3),An
     
      INTEGER :: idbf,idbl,IX(4)
      LOGICAL :: debug_outp
      INTEGER :: ICF(4,6)
      DATA ICF/1,4,3,2,3,4,8,7,5,6,7,8,1,2,6,5,2,3,7,6,1,5,8,4/            
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------      
      IF(IALEFVM==0)RETURN
      IMAT        = IXS(1,LFT+NFT)
      IALEFVM_FLG = IPM(251,IMAT)  
      IF(IALEFVM_FLG <= 1)RETURN       
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
      
      ! attention : il faut que mmain soit traite pour chacun des groupes 
      ! afin de connaitre les tenseurs voisins (SIG,SVIS,QVIS) !
      ! l'assemblage est ensuite realis    apr   s tous les appel de 
      ! ALEMAIN>SFORC3() pour chaque groupe.

      !-------------------------------------------------------------!
      ! TOTAL TENSOR                                                !
      !-------------------------------------------------------------! 
      DO I=LFT,LLT                          
        S1(I) = SIG(I,1) + SVIS(I,1) - QVIS(I) 
        S2(I) = SIG(I,2) + SVIS(I,2) - QVIS(I) 
        S3(I) = SIG(I,3) + SVIS(I,3) - QVIS(I) 
        S4(I) = SIG(I,4) + SVIS(I,4)
        S5(I) = SIG(I,5) + SVIS(I,5) 
        S6(I) = SIG(I,6) + SVIS(I,6)		   
      ENDDO 
      
      DO I=LFT,LLT
        P(I)      = -THIRD*(S1(I)+S2(I)+S3(I))        
      ENDDO
      
      
      !-------------------------------------------------------------!
      ! NORMAL VECTOR FOR ALE                                       !
      !-------------------------------------------------------------!                                       
!      IF(JALE==1 .OR. JEUL==0)THEN  !lagrangian bricks also
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
!      ENDIF

      !--------------------------------------------------------------!
      ! PREPARING BUFFER FOR ALEFVM_SFINT3                           !
      ! 0   : cell centroid data                                     !
      ! 1:3 : INTEGRAL ON EACH FACE  from Integral(DIV(SIGMA),Volume)!
      ! 4   : max(u+x,u-c)                                           !
      !--------------------------------------------------------------!
      DO I=LFT,  LLT
        II                  = I + NFT
        MASS                = RHO(I)*VOL(I)
        NORM(1)             = SQRT(N1X(I)*N1X(I) + N1Y(I)*N1Y(I) + N1Z(I)*N1Z(I)) 
        NORM(2)             = SQRT(N2X(I)*N2X(I) + N2Y(I)*N2Y(I) + N2Z(I)*N2Z(I))  
        NORM(3)             = SQRT(N3X(I)*N3X(I) + N3Y(I)*N3Y(I) + N3Z(I)*N3Z(I)) 
        NORM(4)             = SQRT(N4X(I)*N4X(I) + N4Y(I)*N4Y(I) + N4Z(I)*N4Z(I))
        NORM(5)             = SQRT(N5X(I)*N5X(I) + N5Y(I)*N5Y(I) + N5Z(I)*N5Z(I)) 
        NORM(6)             = SQRT(N6X(I)*N6X(I) + N6Y(I)*N6Y(I) + N6Z(I)*N6Z(I)) 
        U_N(1)              = (MOM(I,1)*N1X(I) + MOM(I,2)*N1Y(I) + MOM(I,3)*N1Z(I)) / (MASS*NORM(1))
        U_N(2)              = (MOM(I,1)*N2X(I) + MOM(I,2)*N2Y(I) + MOM(I,3)*N2Z(I)) / (MASS*NORM(2))
        U_N(3)              = (MOM(I,1)*N3X(I) + MOM(I,2)*N3Y(I) + MOM(I,3)*N3Z(I)) / (MASS*NORM(3))
        U_N(4)              = (MOM(I,1)*N4X(I) + MOM(I,2)*N4Y(I) + MOM(I,3)*N4Z(I)) / (MASS*NORM(4))
        U_N(5)              = (MOM(I,1)*N5X(I) + MOM(I,2)*N5Y(I) + MOM(I,3)*N5Z(I)) / (MASS*NORM(5))
        U_N(6)              = (MOM(I,1)*N6X(I) + MOM(I,2)*N6Y(I) + MOM(I,3)*N6Z(I)) / (MASS*NORM(6))
        SURF(1)             = HALF*NORM(1)
        SURF(2)             = HALF*NORM(2)
        SURF(3)             = HALF*NORM(3)
        SURF(4)             = HALF*NORM(4)
        SURF(5)             = HALF*NORM(5)
        SURF(6)             = HALF*NORM(6)
        !---storage
        !   index1=1 centroid data
        F_FACE(1  ,1  ,II)  = RHO(I)
        F_FACE(1  ,2  ,II)  = SSP(I)
        F_FACE(1  ,3  ,II)  = RHO(I)*SSP(I)
        F_FACE(1  ,4  ,II)  = P(I)
        F_FACE(1  ,5  ,II)  = SQRT(MOM(I,1)*MOM(I,1)+MOM(I,2)*MOM(I,2)+MOM(I,3)*MOM(I,3))/RHO(I)/VOL(I) / SSP(I)    ! material velocity (rho.U/rho) divided by sound speed is mach number : M=U/ssps
        F_FACE(1  ,6  ,II)  = ZERO   !available
        !   index1=2 Surface
        F_FACE(2  ,1:6,II)  = SURF(1:6)
        !   index1=3 Normal Velocity
        F_FACE(3  ,1:6,II)  = U_N(1:6)
      ENDDO

        !DEBUG-OUTPUT---------------!
        if(IALEFVM_OUTP_STRESS /= 0)then
          debug_outp = .FALSE.
          if(IALEFVM_OUTP_STRESS>0)then
            do i=lft,llt
              ii = nft + i
              if(ixs(11,ii)==IALEFVM_OUTP_STRESS)THEN
                debug_outp = .TRUE.
                idbf   = i
                idbl   = i
                EXIT
              endif
             enddo
          elseif(IALEFVM_OUTP_STRESS==-1)then
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

            write (*,FMT='(A,3E26.14,A)')  "                            | ", SIG(I,1),SIG(I,4),SIG(I,6),   " |"
            write (*,FMT='(A,3E26.14,A)')  "           P               =| ", SIG(I,4),SIG(I,2),SIG(I,5),   " |"  
            write (*,FMT='(A,3E26.14,A)')  "                            |_", SIG(I,6),SIG(I,5),SIG(I,3),   "_|" 
            write (*,FMT='(A,3E26.14,A)')  "                            | ", SVIS(I,1),SVIS(I,4),SVIS(I,6)," |"
            write (*,FMT='(A,3E26.14,A)')  "           VIS             =| ", SVIS(I,4),SVIS(I,2),SVIS(I,5)," |"  
            write (*,FMT='(A,3E26.14,A)')  "                            |_", SVIS(I,6),SVIS(I,5),SVIS(I,3),"_|"                   
            write (*,FMT='(A,3E26.14,A)')  "                            | ", S1(I),S4(I),S6(I)," |"
            write (*,FMT='(A,3E26.14,A)')  "           SIGMA = P+VIS+Q =| ", S4(I),S2(I),S5(I)," |"  
            write (*,FMT='(A,3E26.14,A)')  "                            |_", S6(I),S5(I),S3(I),"_|" 
            write (*,FMT='(A,2E26.14)')    "           p               =  ",-THIRD*SUM(SIG(I,1:3)),P(I)
            write (*,FMT='(A,1E26.14)')    "           q               =  ",QVIS(I)           
            write (*,FMT='(A,1E26.14)')    "           M               =  ",F_FACE(1  ,5  ,II)  
            write (*,FMT='(A,1E26.14)')    "         rho               =  ",F_FACE(1  ,1  ,II)
            write (*,FMT='(A,1E26.14)')    "           u               =  ",F_FACE(1  ,5  ,II)*SSP(I)  
            write (*,FMT='(A,1E26.14)')    "         ssp               =  ",F_FACE(1  ,2  ,II)
            write (*,FMT='(A,1E26.14)')    "           z               =  ",F_FACE(1  ,3  ,II)   
            write (*,FMT='(A,3E26.14)')    "       rho.U               =  ",MOM(I,1:3)  
            write (*,FMT='(A,3E26.14)')    "           V               =  ",VOL(I)                                                                                     
            write(*,FMT='(A34,6A26)')      "                                  ",    
     .                                     "#-------- face_1 ---------","#-------- face_2 ---------",
     .                                     "#-------- face_3 ---------","#-------- face_4 ---------",
     .                                     "#-------- face_5 ---------","#-------- face_6 --------#"  
            write (*,FMT='(A,8E26.14)')    "    SURF                   =", F_FACE(1,1:6,II)          
            write (*,FMT='(A,8E26.14)')    "    <U,n>                  =", F_FACE(2,1:6,II)
            print *, "      "          
          enddo
!#!include "lockoff.inc"       
        endif
        endif
      !-----------------------------------------!
        
      RETURN
      END
