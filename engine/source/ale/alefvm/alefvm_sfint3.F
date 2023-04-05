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
Chd|  ALEFVM_SFINT3                 source/ale/alefvm/alefvm_sfint3.F
Chd|-- called by -----------
Chd|        ALEFVM_MAIN                   source/ale/alefvm/alefvm_main.F
Chd|-- calls ---------------
Chd|        ALEFVM_MOD                    ../common_source/modules/ale/alefvm_mod.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE ALEFVM_SFINT3 (
     1                          IXS,  NV46 ,  ALE_CONNECT, IALEFVM_FLG, ITASK,
     2                          IPM,  IPARG,  NG   , NALE       , A    ,
     3                          X  ,  V    ,  RHO  , VOL        , MS   ,
     4                          MOM,  SIG  ,  W    , IAD22      ,NEL   )
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
      USE I22BUFBRIC_MOD !debug
      USE I22TRI_MOD
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
#include      "inter22.inc"
#include      "param_c.inc"
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
      INTEGER NEL
      INTEGER :: IXS(NIXS,*),NV46,IALEFVM_FLG, ITASK,IPM(NPROPMI,*),IPARG(NPARG,*),NG ,NALE(*)
      my_real :: A(3,*),X(3,*),V(3,*),RHO(*),VOL(*),MS(*),MOM(NEL,3),SIG(NEL,6),W(3,*)
      my_real :: IAD22(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: debug_outp
      INTEGER :: idebug, idbf,idbl, IAD2, IAD3
      INTEGER :: I, II, IV, J, JV, IMAT, ILAW,IFLG_ALE,IFLG_EUL
      INTEGER :: NIN, IB, IPRES_MOM
      INTEGER :: NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8
      my_real :: F0(3,MVSIZ), FFACE(3,NV46,MVSIZ),LMAX
      my_real :: NX(6,mvsiz), NY(6,mvsiz),  NZ(6,mvsiz),  NF1, NF2,P1,P2,DENOM,Pf
      my_real :: THETA, MACH, VEL, M1, M2, Mf
      my_real :: 
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), X5(MVSIZ), X6(MVSIZ), X7(MVSIZ), X8(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ), Y5(MVSIZ), Y6(MVSIZ), Y7(MVSIZ), Y8(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ), Z5(MVSIZ), Z6(MVSIZ), Z7(MVSIZ), Z8(MVSIZ),
     .   Z_1, Z_2, U1(3), U2(3),  U1N1, U2N1, R_1, R_2
      
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------      
      IF(ALEFVM_Param%IEnabled==0)    RETURN
      IMAT        = IXS(1,1+NFT)
      ILAW        = IPM(2,IMAT)
      IALEFVM_FLG = IPM(251,IMAT)
      IF(IALEFVM_FLG <= 1)RETURN       
      IF(ILAW==11)          RETURN  
      IFLG_ALE    = IPARG(7,NG)
      IFLG_EUL    = IPARG(11,NG)   
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------

      NIN = 1
      
      !-------------------------------------------------------------!
      ! NORMAL VECTOR FOR ALE                                       !
      !-------------------------------------------------------------!                                       
!      IF(JALE==1 .OR. JEUL==0)THEN  !lagrangian bricks also
        DO I=1,NEL
          II     = I + NFT
	  !---8 local node numbers NC1 TO NC8 for solid element I ---!
          NC1=IXS(2,II)
          NC2=IXS(3,II)
          NC3=IXS(4,II)
          NC4=IXS(5,II)
          NC5=IXS(6,II)
          NC6=IXS(7,II)
          NC7=IXS(8,II)
          NC8=IXS(9,II)
	  !
          !---Coordinates of the 8 nodes
          X1(I)=X(1,NC1)
          Y1(I)=X(2,NC1)
          Z1(I)=X(3,NC1)
          !
          X2(I)=X(1,NC2)
          Y2(I)=X(2,NC2)
          Z2(I)=X(3,NC2)
          !
          X3(I)=X(1,NC3)
          Y3(I)=X(2,NC3)
          Z3(I)=X(3,NC3)
          !
          X4(I)=X(1,NC4)
          Y4(I)=X(2,NC4)
          Z4(I)=X(3,NC4)
          !
          X5(I)=X(1,NC5)
          Y5(I)=X(2,NC5)
          Z5(I)=X(3,NC5)
          !
          X6(I)=X(1,NC6)
          Y6(I)=X(2,NC6)
          Z6(I)=X(3,NC6)
          !
          X7(I)=X(1,NC7)
          Y7(I)=X(2,NC7)
          Z7(I)=X(3,NC7)
          !
          X8(I)=X(1,NC8)
          Y8(I)=X(2,NC8)
          Z8(I)=X(3,NC8)
        ENDDO    
        DO I=1,NEL        
          ! Face-1
          NX(1,I)=(Y3(I)-Y1(I))*(Z2(I)-Z4(I)) - (Z3(I)-Z1(I))*(Y2(I)-Y4(I))
          NY(1,I)=(Z3(I)-Z1(I))*(X2(I)-X4(I)) - (X3(I)-X1(I))*(Z2(I)-Z4(I))
          NZ(1,I)=(X3(I)-X1(I))*(Y2(I)-Y4(I)) - (Y3(I)-Y1(I))*(X2(I)-X4(I))
          ! Face-2
          NX(2,I)=(Y7(I)-Y4(I))*(Z3(I)-Z8(I)) - (Z7(I)-Z4(I))*(Y3(I)-Y8(I))
          NY(2,I)=(Z7(I)-Z4(I))*(X3(I)-X8(I)) - (X7(I)-X4(I))*(Z3(I)-Z8(I))
          NZ(2,I)=(X7(I)-X4(I))*(Y3(I)-Y8(I)) - (Y7(I)-Y4(I))*(X3(I)-X8(I))
          ! Face-3
          NX(3,I)=(Y6(I)-Y8(I))*(Z7(I)-Z5(I)) - (Z6(I)-Z8(I))*(Y7(I)-Y5(I))
          NY(3,I)=(Z6(I)-Z8(I))*(X7(I)-X5(I)) - (X6(I)-X8(I))*(Z7(I)-Z5(I))
          NZ(3,I)=(X6(I)-X8(I))*(Y7(I)-Y5(I)) - (Y6(I)-Y8(I))*(X7(I)-X5(I))
          ! Face-4
          NX(4,I)=(Y2(I)-Y5(I))*(Z6(I)-Z1(I)) - (Z2(I)-Z5(I))*(Y6(I)-Y1(I))
          NY(4,I)=(Z2(I)-Z5(I))*(X6(I)-X1(I)) - (X2(I)-X5(I))*(Z6(I)-Z1(I))
          NZ(4,I)=(X2(I)-X5(I))*(Y6(I)-Y1(I)) - (Y2(I)-Y5(I))*(X6(I)-X1(I))
          ! Face-5
          NX(5,I)=(Y7(I)-Y2(I))*(Z6(I)-Z3(I)) - (Z7(I)-Z2(I))*(Y6(I)-Y3(I))
          NY(5,I)=(Z7(I)-Z2(I))*(X6(I)-X3(I)) - (X7(I)-X2(I))*(Z6(I)-Z3(I))
          NZ(5,I)=(X7(I)-X2(I))*(Y6(I)-Y3(I)) - (Y7(I)-Y2(I))*(X6(I)-X3(I))
          ! Face-6
          NX(6,I)=(Y8(I)-Y1(I))*(Z4(I)-Z5(I)) - (Z8(I)-Z1(I))*(Y4(I)-Y5(I))
          NY(6,I)=(Z8(I)-Z1(I))*(X4(I)-X5(I)) - (X8(I)-X1(I))*(Z4(I)-Z5(I))
          NZ(6,I)=(X8(I)-X1(I))*(Y4(I)-Y5(I)) - (Y8(I)-Y1(I))*(X4(I)-X5(I))
        ENDDO
!      ENDIF
      
      !-------------------------------------------------------------!
      ! Assembling Forces                                           !
      !-------------------------------------------------------------!
      
      !IPRES_MOM = 0,1,2,3,4  : (P1+P2)/2
      !IPRES_MOM =   5        : (rho1c1P1+rho2c2P2)/(rho1c1+rho2c2) + (rho1c1*rho2c2/(rho1c1+rho2c2)) <uel-uadj,nel>  acoustic solver 
      
      IPRES_MOM = ALEFVM_Param%ISOLVER
      
!      print *, "IPRES_MOM, SFINT3", IPRES_MOM
      
      SELECT CASE (IPRES_MOM)
        CASE(5) ! acoustic solver
          DO I=1,NEL
            II               = I + NFT
            IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
            P1               = ALEFVM_Buffer%F_FACE(1,4,II)
            M1               = ALEFVM_Buffer%F_FACE(1,5,II)
            Z_1              = ALEFVM_Buffer%F_FACE(1,3,II) ! impedance
            DO J=1,NV46
              IV  = ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
              IF(IV > 0)THEN
                !--ADJACENT ELEM DOES EXIST
                 IAD3 = ALE_CONNECT%ee_connect%iad_connect(IV)
                DO JV=1,NV46
                  IF(ALE_CONNECT%ee_connect%connected(IAD3 + JV - 1)==II)EXIT
                ENDDO
                U1N1         = + ALEFVM_Buffer%F_FACE(3, J,II)
                U2N1         = - ALEFVM_Buffer%F_FACE(3,JV,IV)     !UN2 = <U2,N2> = <U2,-N1> 
                Z_2          =   ALEFVM_Buffer%F_FACE(1, 3,IV)
                P2           =   ALEFVM_Buffer%F_FACE(1, 4,IV)
                M2           =   ALEFVM_Buffer%F_FACE(1, 5,IV)
                DENOM        =   Z_1 + Z_2
                Mf           = MIN(M1,M2)
                THETA        = MIN(ONE,Mf)                 !see dellacherie,omnes,raviart : fixing low mach issue
                Pf           = (Z_1*P2 + Z_2*P1)/DENOM  +  THETA*(Z_1*Z_2*(U1N1-U2N1)/DENOM)
              ELSE
              !--ADJACENT ELEM DOES NOT EXIST
                !SLIDING RWALL BC
                U1N1         = + ALEFVM_Buffer%F_FACE(3, J,II)
                !Pf          =  P1
                Mf           = M1
                THETA        = MIN(ONE,Mf)                 !see dellacherie,omnes,raviart : fixing low mach issue
                Pf           =  P1  + THETA*HALF*Z_1*U1N1
                !Pf = P1
              ENDIF 
              FFACE(1,J,I)   = -HALF*Pf*NX(J,I)    !N= 2.SURF.n  , where abs(n)=1    => abs(N) = 2S , this lines leads to  FFACE(1J,I) = Pf.S.n
              FFACE(2,J,I)   = -HALF*Pf*NY(J,I)    ! negative sign because int(div.sigma,DV) => int(-P,dS)
              FFACE(3,J,I)   = -HALF*Pf*NZ(J,I)                               
            ENDDO!next J
          ENDDO!next I     
        CASE DEFAULT
          DO I=1,NEL                                                       
            II    = I + NFT 
            IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
            P1    = ALEFVM_Buffer%F_FACE(1,4,II)                                                
            DO J=1,NV46                                                      
              IV  = ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
              IF(IV > 0)THEN                                              
                !--ADJACENT ELEM DOES EXIST 
                 IAD3 = ALE_CONNECT%ee_connect%iad_connect(IV)
                DO JV=1,NV46                                                 
                  IF(ALE_CONNECT%ee_connect%connected(IAD3 + JV - 1)==II)EXIT                                   
                ENDDO                                                        
                P2           = ALEFVM_Buffer%F_FACE(1,4,IV)
                Pf           = HALF*(P1+P2) 
              ELSE                                                           
              !--ADJACENT ELEM DOES NOT EXIST                                
                !SLIDING RWALL BC   
                Pf           = P1                                                                          
              ENDIF   
              FFACE(1,J,I)   = -HALF*Pf*NX(J,I) ! negative sign because int(div.sigma,DV) => int(-P,dS)                              
              FFACE(2,J,I)   = -HALF*Pf*NY(J,I)                             
              FFACE(3,J,I)   = -HALF*Pf*NZ(J,I) 
              !print  *, "P1,P2,Pf", P1,P2,Pf 
              !print  *, " normal ", NX(J,I), NY(J,I), NZ(J,I) 
c              write ( *, FMT='(A,I10,A,I3,A,3F20.12,A,F20.12)') 
c     ." id=", ixs(11,ii)," ncycle=", NCYCLE, " F=", FFACE(1:3,J,I) , "Pf=", Pf                                                               
            ENDDO!next J                                                     
          ENDDO!next I                                                       
      END SELECT
       
      !-------------------------------------------------------------!
      ! INTEGRAL ON EACH FACE  from Integral(DIV(SIGMA),Volume)     !
      !-------------------------------------------------------------!
      DO I=1,NEL
        II           = I + NFT
        IF(INT22/=0)THEN
          IB           = NINT(IAD22(I))
          IF(IB>0) CYCLE
        ENDIF
        F0(1,I)      = SUM(FFACE(1,1:NV46,I)) 
        F0(2,I)      = SUM(FFACE(2,1:NV46,I)) 
        F0(3,I)      = SUM(FFACE(3,1:NV46,I))  
        
              !  write (*,FMT='(A,I6,A,3F30.16)') "   brick ID=", ixs(11,II),"  Fint=", F0(1:3,I)

        
        ALEFVM_Buffer%FINT_CELL(1,II) = F0(1,I)
        ALEFVM_Buffer%FINT_CELL(2,II) = F0(2,I)
        ALEFVM_Buffer%FINT_CELL(3,II) = F0(3,I)
        
        !Fcell already contains gravity force, so do not erase but add               
        ALEFVM_Buffer%FCELL(1,II)  = ALEFVM_Buffer%FCELL(1,II) + F0(1,I) + ALEFVM_Buffer%FEXT_CELL(1,II)
        ALEFVM_Buffer%FCELL(2,II)  = ALEFVM_Buffer%FCELL(2,II) + F0(2,I) + ALEFVM_Buffer%FEXT_CELL(2,II)
        ALEFVM_Buffer%FCELL(3,II)  = ALEFVM_Buffer%FCELL(3,II) + F0(3,I) + ALEFVM_Buffer%FEXT_CELL(3,II)              
      ENDDO!next I



      !-------------------------------------------------------------!
      !  DBUG OUTPUT                                                !
      !-------------------------------------------------------------!      
!        if(IALEFVM_OUTP_FINT /= 0)then
!
!          if(itask==0)then     
!            print *, "    |----alefvm_sfint3.F-----|"
!            print *, "    |   THREAD INFORMATION   |"
!            print *, "    |------------------------|" 
!            print *, "     NCYCLE =", NCYCLE
!
!            do i=1,nb
!              ii = nft + i
!              IF(INT22/=0)THEN
!                IB           = NINT(IAD22(I))
!                IF(IB>0) CYCLE
!              ENDIF              
!              print *,                    "      brique=", ixs(11,nft+i)
!              write(*,FMT='(A34,6A26)')   "                                  ",    
!     .                                  "#--- internal force -----#"  
!              write (*,FMT='(A,8E26.14)') "         force-X           =", FCELL(1,II)          
!              write (*,FMT='(A,8E26.14)') "         force-Y           =", FCELL(2,II)
!              write (*,FMT='(A,8E26.14)') "         force-Z           =", FCELL(3,II)                      
!              print *, "      "          
!            enddo
!            
!          endif!if(itask)
!     
!        endif!if(IALEFVM_OUTP_FINT /= 0)
      !-----------------------------------------!
        
      RETURN
      END
