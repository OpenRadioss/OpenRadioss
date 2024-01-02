Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  ALEFVM_EFLUX3                 source/ale/alefvm/alefvm_eflux3.F
Chd|-- called by -----------
Chd|        AFLUX0                        source/ale/aflux0.F           
Chd|-- calls ---------------
Chd|        ALEFVM_MOD                    ../common_source/modules/ale/alefvm_mod.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE ALEFVM_EFLUX3(PM   ,IXS   ,V       ,FLUX  ,FLU1,
     2                         VEUL ,ALE_CONNECT ,TAG22 ,IPM ,
     3                         NV46 ,MOM   ,ITASK   ,X     ,NEL )
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
      USE I22TRI_MOD
      USE ALEFVM_MOD
      USE ALE_CONNECTIVITY_MOD
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
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IXS(NIXS,NUMELS), NV46, IPM(NPROPMI,*),NEL
      my_real :: PM(NPROPM,NUMMAT), V(3,NUMNOD), FLUX(6,*), FLU1(*), VEUL(LVEUL,*), TAG22(*),MOM(NEL,3),X(3,NUMNOD)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MAT(MVSIZ), NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ), NC5(MVSIZ), NC6(MVSIZ),
     .   NC7(MVSIZ), NC8(MVSIZ), I, II,J,IMAT,IALEFVM_FLG,IDV, ITASK,ICF(4,6),
     .   IX1,IX2,IX3,IX4
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), X5(MVSIZ), X6(MVSIZ), X7(MVSIZ), X8(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ), Y5(MVSIZ), Y6(MVSIZ), Y7(MVSIZ), Y8(MVSIZ), Z1(MVSIZ),
     .   Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ), Z5(MVSIZ), Z6(MVSIZ), Z7(MVSIZ), Z8(MVSIZ), N1X(MVSIZ),
     .   N2X(MVSIZ), N3X(MVSIZ), N4X(MVSIZ), N5X(MVSIZ), N6X(MVSIZ), N1Y(MVSIZ), N2Y(MVSIZ), N3Y(MVSIZ),
     .   N4Y(MVSIZ), N5Y(MVSIZ), N6Y(MVSIZ), N1Z(MVSIZ), N2Z(MVSIZ), N3Z(MVSIZ), N4Z(MVSIZ), N5Z(MVSIZ),
     .   N6Z(MVSIZ), FLUX1(MVSIZ), FLUX2(MVSIZ), FLUX3(MVSIZ), FLUX4(MVSIZ), FLUX5(MVSIZ),
     .   FLUX6(MVSIZ), VX1(MVSIZ), VX2(MVSIZ), VX3(MVSIZ), VX4(MVSIZ), VX5(MVSIZ), VX6(MVSIZ),
     .   VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), VY4(MVSIZ), VY5(MVSIZ), VY6(MVSIZ), VZ1(MVSIZ), VZ2(MVSIZ),
     .   VZ3(MVSIZ), VZ4(MVSIZ), VZ5(MVSIZ), VZ6(MVSIZ), VDX1(MVSIZ), VDX2(MVSIZ), VDX3(MVSIZ),
     .   VDX4(MVSIZ), VDX5(MVSIZ), VDX6(MVSIZ), VDX7(MVSIZ), VDX8(MVSIZ), VDY1(MVSIZ), VDY2(MVSIZ),
     .   VDY3(MVSIZ), VDY4(MVSIZ), VDY5(MVSIZ), VDY6(MVSIZ), VDY7(MVSIZ), VDY8(MVSIZ), VDZ1(MVSIZ),
     .   VDZ2(MVSIZ), VDZ3(MVSIZ), VDZ4(MVSIZ), VDZ5(MVSIZ), VDZ6(MVSIZ), VDZ7(MVSIZ), VDZ8(MVSIZ),
     .   REDUC,UPWL(6,MVSIZ),VALVOIS(6,NV46,MVSIZ), VALEL(6,MVSIZ), VEC(3,6),Cf(3,MVSIZ),
     .   Z(3),Zadj(3),ZCf(3),ZZadj(3),Zcf_,ZZadj_,cos, PS, denom, denom1,denom2,lambda,Xsi,SR1,SR2, SRF,
     .   SURF(6),TERM2
      LOGICAL debug_outp
      INTEGER :: idbf,idbl,NC(8,MVSIZ),IAD2,LGTH
C-----------------------------------------------
      DATA ICF/1,4,3,2,3,4,8,7,5,6,7,8,1,2,6,5,2,3,7,6,1,5,8,4/      
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------      
C This subroutine enables to compute face velocities
C from centroid values [rho*U] stored in Fcell vector 
C (later used for centroid forces)
C Face velocities are stored in vectors F_Face (ALEFVM_MOD)
C This same vector is used later to store forces on faces 
C (used to compute centroid force)
        !IPM(251,MAT(1)) = I_ALE_SOLVER
        ! 0: Default = 1 expect if /ALE/MAT or /EULER/MAT has IFROM flag defined.
        ! 1 : FEM
        ! 2 : FVM U average
        ! 3 : FVM rho.U average
        ! 4 : FVM rho.c.U average
        ! 5 : Godunov Acoustic
        ! 6 : experimental
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------      
      IF(ALEFVM_Param%IEnabled == 0)    RETURN
      IMAT        = IXS(1,1+NFT)
      IALEFVM_FLG = IPM(251,IMAT)
      IF(IALEFVM_FLG <= 1)RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      
      DO I=1,NEL
        II     = I + NFT
        MAT(I) = IXS(1,II)
      ENDDO
      
      !======================================================!
      ! INITIALIZATION : ADJAENT VALUES                      !
      !======================================================!
      !FCELL = [rhoU]. this quantity must be know for all groups
      DO I=1,NEL
        II                 = I + NFT
        IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
        LGTH = ALE_CONNECT%ee_connect%iad_connect(II+1) - IAD2
        VALEL(1,I)         = ALEFVM_Buffer%FCELL(1,II) !rho.ux
        VALEL(2,I)         = ALEFVM_Buffer%FCELL(2,II) !rho.uy
        VALEL(3,I)         = ALEFVM_Buffer%FCELL(3,II) !rho.uz
        VALEL(4,I)         = ALEFVM_Buffer%FCELL(4,II) !rho
        VALEL(5,I)         = ALEFVM_Buffer%FCELL(5,II) !ssp
        VALEL(6,I)         = ALEFVM_Buffer%FCELL(6,II) !P                   
        DO J=1,LGTH
          IDV = ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
          IF(IDV <= 0) THEN
            !no adjacent elem
            VALVOIS(1,J,I) = -ALEFVM_Buffer%FCELL(1,II) 
            VALVOIS(2,J,I) = -ALEFVM_Buffer%FCELL(2,II) 
            VALVOIS(3,J,I) = -ALEFVM_Buffer%FCELL(3,II) 
            VALVOIS(4,J,I) =  ALEFVM_Buffer%FCELL(4,II) 
            VALVOIS(5,J,I) =  ALEFVM_Buffer%FCELL(5,II) 
            VALVOIS(6,J,I) =  ALEFVM_Buffer%FCELL(6,II)               
          ELSE
            !adjacent elem does exist
            VALVOIS(1,J,I) = ALEFVM_Buffer%FCELL(1,IDV)
            VALVOIS(2,J,I) = ALEFVM_Buffer%FCELL(2,IDV)
            VALVOIS(3,J,I) = ALEFVM_Buffer%FCELL(3,IDV) 
            VALVOIS(4,J,I) = ALEFVM_Buffer%FCELL(4,IDV)  
            VALVOIS(5,J,I) = ALEFVM_Buffer%FCELL(5,IDV) 
            VALVOIS(6,J,I) = ALEFVM_Buffer%FCELL(6,IDV)
          ENDIF
          
        ENDDO!next J
         ! write(*,FMT='(  I6,A,I8,A,F26.12,A,6F26.12)')NCYCLE, "  id=",ixs(11,ii),"   P=",VALEL(6,I)," Pvois =  ", VALVOIS(6,1:6,I)

      ENDDO!next I
      
      !======================================================!
      ! NORMAL VECTORS ON EACH DACE                          !
      !    2S[n] = [diag1] x [diag2]                         ! 
      !    where                                             !
      !      [n] : unitary normal vector on face             !
      !======================================================!
      DO I=1,NEL
        II=I+NFT
        N1X(I)=VEUL(14,II)
        N2X(I)=VEUL(15,II)
        N3X(I)=VEUL(16,II)
        N4X(I)=VEUL(17,II)
        N5X(I)=VEUL(18,II)
        N6X(I)=VEUL(19,II)

        N1Y(I)=VEUL(20,II)
        N2Y(I)=VEUL(21,II)
        N3Y(I)=VEUL(22,II)
        N4Y(I)=VEUL(23,II)
        N5Y(I)=VEUL(24,II)
        N6Y(I)=VEUL(25,II)

        N1Z(I)=VEUL(26,II)
        N2Z(I)=VEUL(27,II)
        N3Z(I)=VEUL(28,II)
        N4Z(I)=VEUL(29,II)
        N5Z(I)=VEUL(30,II)
        N6Z(I)=VEUL(31,II)
      ENDDO
      
      !======================================================!
      ! RELATIVE VELOCITIES ON EACH FACE                     !     
      !======================================================!
      !UPDATE LATER WITH LINEAR INTERPOLATION INSTEAD OF MEAN VALUE
      !MEAN VALUE OK FOR CARTESIAN REGULAR MESH
      IF(ALEFVM_Param%IFORM/=0)IALEFVM_FLG = ALEFVM_Param%IFORM !for debug purpose if set in alefvm_init.F
  
  
      SELECT CASE(ALEFVM_Param%ISOLVER)

        CASE(1)
        !CENTERED FVM - U average
          DO I=1,NEL
            II               = I + NFT
            DO J=1,NV46
               VEC(1,J)      = HALF * (VALEL(1,I)/VALEL(4,I) + VALVOIS(1,J,I)/VALVOIS(4,J,I)) 
               VEC(2,J)      = HALF * (VALEL(2,I)/VALEL(4,I) + VALVOIS(2,J,I)/VALVOIS(4,J,I)) 
               VEC(3,J)      = HALF * (VALEL(3,I)/VALEL(4,I) + VALVOIS(3,J,I)/VALVOIS(4,J,I)) 
            ENDDO!next J 
            DO J=1,NV46
              ALEFVM_Buffer%F_FACE(1,J,II) = VEC(1,J)
              ALEFVM_Buffer%F_FACE(2,J,II) = VEC(2,J)
              ALEFVM_Buffer%F_FACE(3,J,II) = VEC(3,J)
            ENDDO        
            !---face-1
            VX1(I)           = VEC(1,1)
            VY1(I)           = VEC(2,1)
            VZ1(I)           = VEC(3,1)                
            !---face-2
            VX2(I)           = VEC(1,2)
            VY2(I)           = VEC(2,2)
            VZ2(I)           = VEC(3,2)                
            !---face-3
            VX3(I)           = VEC(1,3)
            VY3(I)           = VEC(2,3)
            VZ3(I)           = VEC(3,3)                
            !---face-4
            VX4(I)           = VEC(1,4)
            VY4(I)           = VEC(2,4)
            VZ4(I)           = VEC(3,4)                
            !---face-5
            VX5(I)           = VEC(1,5)
            VY5(I)           = VEC(2,5)
            VZ5(I)           = VEC(3,5)                
            !---face-6
            VX6(I)           = VEC(1,6)
            VY6(I)           = VEC(2,6)
            VZ6(I)           = VEC(3,6)                
          ENDDO!next I 

        CASE(2)
        !CENTERED FVM - rho.U average
          DO I=1,NEL
            II               = I + NFT
            DO J=1,NV46
               VEC(1,J)      = (VALEL(1,I) + VALVOIS(1,J,I)) / (VALEL(4,I)+VALVOIS(4,J,I))
               VEC(2,J)      = (VALEL(2,I) + VALVOIS(2,J,I)) / (VALEL(4,I)+VALVOIS(4,J,I))
               VEC(3,J)      = (VALEL(3,I) + VALVOIS(3,J,I)) / (VALEL(4,I)+VALVOIS(4,J,I))
            ENDDO!next J 
            DO J=1,NV46
              ALEFVM_Buffer%F_FACE(1,J,II) = VEC(1,J)
              ALEFVM_Buffer%F_FACE(2,J,II) = VEC(2,J)
              ALEFVM_Buffer%F_FACE(3,J,II) = VEC(3,J)
            ENDDO        
            !---face-1
            VX1(I)           = VEC(1,1)
            VY1(I)           = VEC(2,1)
            VZ1(I)           = VEC(3,1)                
            !---face-2
            VX2(I)           = VEC(1,2)
            VY2(I)           = VEC(2,2)
            VZ2(I)           = VEC(3,2)                
            !---face-3
            VX3(I)           = VEC(1,3)
            VY3(I)           = VEC(2,3)
            VZ3(I)           = VEC(3,3)                
            !---face-4
            VX4(I)           = VEC(1,4)
            VY4(I)           = VEC(2,4)
            VZ4(I)           = VEC(3,4)                
            !---face-5
            VX5(I)           = VEC(1,5)
            VY5(I)           = VEC(2,5)
            VZ5(I)           = VEC(3,5)                
            !---face-6
            VX6(I)           = VEC(1,6)
            VY6(I)           = VEC(2,6)
            VZ6(I)           = VEC(3,6)                
          ENDDO!next I 

        CASE(3)
        !CENTERED FVM - Roe-average
          DO I=1,NEL
            II               = I + NFT
            SR1 = SQRT(VALEL(4,I))
            DO J=1,NV46
              SR2           =  SQRT(VALVOIS(4,J,I))
              VEC(1,J)      = (VALEL(1,I)/SR1 + VALVOIS(1,J,I)/SR2) / (SR1+SR2) 
              VEC(2,J)      = (VALEL(2,I)/SR1 + VALVOIS(2,J,I)/SR2) / (SR1+SR2) 
              VEC(3,J)      = (VALEL(3,I)/SR1 + VALVOIS(3,J,I)/SR2) / (SR1+SR2) 
            ENDDO!next J 
            DO J=1,NV46
              ALEFVM_Buffer%F_FACE(1,J,II) = VEC(1,J)
              ALEFVM_Buffer%F_FACE(2,J,II) = VEC(2,J)
              ALEFVM_Buffer%F_FACE(3,J,II) = VEC(3,J)
            ENDDO        
            !---face-1
            VX1(I)           = VEC(1,1)
            VY1(I)           = VEC(2,1)
            VZ1(I)           = VEC(3,1)                
            !---face-2
            VX2(I)           = VEC(1,2)
            VY2(I)           = VEC(2,2)
            VZ2(I)           = VEC(3,2)                
            !---face-3
            VX3(I)           = VEC(1,3)
            VY3(I)           = VEC(2,3)
            VZ3(I)           = VEC(3,3)                
            !---face-4
            VX4(I)           = VEC(1,4)
            VY4(I)           = VEC(2,4)
            VZ4(I)           = VEC(3,4)                
            !---face-5
            VX5(I)           = VEC(1,5)
            VY5(I)           = VEC(2,5)
            VZ5(I)           = VEC(3,5)                
            !---face-6
            VX6(I)           = VEC(1,6)
            VY6(I)           = VEC(2,6)
            VZ6(I)           = VEC(3,6)                
          ENDDO!next I 
          
        CASE(4)
        !CENTERED FVM - rho.c.U average
          DO I=1,NEL
            II               = I + NFT
            DO J=1,NV46
              IF(DT1==ZERO)THEN            
               VEC(1,J)      = (VALEL(1,I)            + VALVOIS(1,J,I)               )
     .                       / (VALEL(4,I)            + VALVOIS(4,J,I)               )
               VEC(2,J)      = (VALEL(2,I)            + VALVOIS(2,J,I)               )
     .                       / (VALEL(4,I)            + VALVOIS(4,J,I)               )
               VEC(3,J)      = (VALEL(3,I)            + VALVOIS(3,J,I)               )
     .                       / (VALEL(4,I)            + VALVOIS(4,J,I)               )
              ELSE
               VEC(1,J)      = (VALEL(1,I)*VALEL(5,I) + VALVOIS(1,J,I)*VALVOIS(5,J,I)) 
     .                       / (VALEL(4,I)*VALEL(5,I) + VALVOIS(4,J,I)*VALVOIS(5,J,I))
               VEC(2,J)      = (VALEL(2,I)*VALEL(5,I) + VALVOIS(2,J,I)*VALVOIS(5,J,I)) 
     .                       / (VALEL(4,I)*VALEL(5,I) + VALVOIS(4,J,I)*VALVOIS(5,J,I))
               VEC(3,J)      = (VALEL(3,I)*VALEL(5,I) + VALVOIS(3,J,I)*VALVOIS(5,J,I)) 
     .                       / (VALEL(4,I)*VALEL(5,I) + VALVOIS(4,J,I)*VALVOIS(5,J,I))            
              ENDIF
            ENDDO!next J 
            DO J=1,NV46
              ALEFVM_Buffer%F_FACE(1,J,II) = VEC(1,J)
              ALEFVM_Buffer%F_FACE(2,J,II) = VEC(2,J)
              ALEFVM_Buffer%F_FACE(3,J,II) = VEC(3,J)
            ENDDO        
            !---face-1
            VX1(I)           = VEC(1,1)
            VY1(I)           = VEC(2,1)
            VZ1(I)           = VEC(3,1)                
            !---face-2
            VX2(I)           = VEC(1,2)
            VY2(I)           = VEC(2,2)
            VZ2(I)           = VEC(3,2)                
            !---face-3
            VX3(I)           = VEC(1,3)
            VY3(I)           = VEC(2,3)
            VZ3(I)           = VEC(3,3)                
            !---face-4
            VX4(I)           = VEC(1,4)
            VY4(I)           = VEC(2,4)
            VZ4(I)           = VEC(3,4)                
            !---face-5
            VX5(I)           = VEC(1,5)
            VY5(I)           = VEC(2,5)
            VZ5(I)           = VEC(3,5)                
            !---face-6
            VX6(I)           = VEC(1,6)
            VY6(I)           = VEC(2,6)
            VZ6(I)           = VEC(3,6)                
          ENDDO!next I 
          
        CASE(5)
        !Godunov Acoustic for Riemann problem
          DO I=1,NEL
            II               = I + NFT
            IF(DT1==ZERO)THEN
              DO J=1,NV46
                 VEC(1,J)      = (VALEL(1,I)             + VALVOIS(1,J,I)) 
     .                         / (VALEL(4,I)             + VALVOIS(4,J,I)) 
                 VEC(2,J)      = (VALEL(2,I)             + VALVOIS(2,J,I)) 
     .                         / (VALEL(4,I)             + VALVOIS(4,J,I))
                 VEC(3,J)      = (VALEL(3,I)             + VALVOIS(3,J,I)) 
     .                         / (VALEL(4,I)             + VALVOIS(4,J,I))
              ENDDO!next J 
            
            ELSE
              SURF(1) = ONE/SQRT(N1X(I)*N1X(I)+N1Y(I)*N1Y(I)+N1Z(I)*N1Z(I))
              SURF(2) = ONE/SQRT(N2X(I)*N2X(I)+N2Y(I)*N2Y(I)+N2Z(I)*N2Z(I))
              SURF(3) = ONE/SQRT(N3X(I)*N3X(I)+N3Y(I)*N3Y(I)+N3Z(I)*N3Z(I))
              SURF(4) = ONE/SQRT(N4X(I)*N4X(I)+N4Y(I)*N4Y(I)+N4Z(I)*N4Z(I))
              SURF(5) = ONE/SQRT(N5X(I)*N5X(I)+N5Y(I)*N5Y(I)+N5Z(I)*N5Z(I))
              SURF(6) = ONE/SQRT(N6X(I)*N6X(I)+N6Y(I)*N6Y(I)+N6Z(I)*N6Z(I))
              DO J=1,NV46
                 
                 TERM2         = +SURF(J) * ( VALEL(6,I)-VALVOIS(6,J,I) ) / (VALEL(4,I)*VALEL(5,I)+VALVOIS(4,J,I)*VALVOIS(5,J,I))
                 VEC(1,J)      = (VALEL(1,I)*VALEL(5,I) + VALVOIS(1,J,I)*VALVOIS(5,J,I)) 
     .                         / (VALEL(4,I)*VALEL(5,I)+VALVOIS(4,J,I)*VALVOIS(5,J,I)) + TERM2*VEUL(14-1+J,II)
                 VEC(2,J)      = (VALEL(2,I)*VALEL(5,I) + VALVOIS(2,J,I)*VALVOIS(5,J,I)) 
     .                         / (VALEL(4,I)*VALEL(5,I)+VALVOIS(4,J,I)*VALVOIS(5,J,I)) + TERM2*VEUL(20-1+J,II)
                 VEC(3,J)      = (VALEL(3,I)*VALEL(5,I) + VALVOIS(3,J,I)*VALVOIS(5,J,I)) 
     .                         / (VALEL(4,I)*VALEL(5,I)+VALVOIS(4,J,I)*VALVOIS(5,J,I)) + TERM2*VEUL(26-1+J,II)
              ENDDO!next J 
            ENDIF
            DO J=1,NV46
              ALEFVM_Buffer%F_FACE(1,J,II) = VEC(1,J)
              ALEFVM_Buffer%F_FACE(2,J,II) = VEC(2,J)
              ALEFVM_Buffer%F_FACE(3,J,II) = VEC(3,J)
            ENDDO        
            !---face-1
            VX1(I)           = VEC(1,1)
            VY1(I)           = VEC(2,1)
            VZ1(I)           = VEC(3,1)                
            !---face-2
            VX2(I)           = VEC(1,2)
            VY2(I)           = VEC(2,2)
            VZ2(I)           = VEC(3,2)                
            !---face-3
            VX3(I)           = VEC(1,3)
            VY3(I)           = VEC(2,3)
            VZ3(I)           = VEC(3,3)                
            !---face-4
            VX4(I)           = VEC(1,4)
            VY4(I)           = VEC(2,4)
            VZ4(I)           = VEC(3,4)                
            !---face-5
            VX5(I)           = VEC(1,5)
            VY5(I)           = VEC(2,5)
            VZ5(I)           = VEC(3,5)                
            !---face-6
            VX6(I)           = VEC(1,6)
            VY6(I)           = VEC(2,6)
            VZ6(I)           = VEC(3,6)                
          ENDDO!next I         
          
        CASE(6)
        !INTERPOLATED
        !WARNING : ADD SYMMETRY LATER IF FORMULATION IS KEPT (mean value for identical value on both sides)
          DO I=1,NEL
            II               = I + NFT
            NC(1,I)          = IXS(2,II)
            NC(2,I)          = IXS(3,II)
            NC(3,I)          = IXS(4,II)
            NC(4,I)          = IXS(5,II)
            NC(5,I)          = IXS(6,II)
            NC(6,I)          = IXS(7,II)
            NC(7,I)          = IXS(8,II)
            NC(8,I)          = IXS(9,II)
            Z(1)             = ONE_OVER_8*SUM(X(1,NC(1:8,I)))
            Z(2)             = ONE_OVER_8*SUM(X(2,NC(1:8,I)))
            Z(3)             = ONE_OVER_8*SUM(X(3,NC(1:8,I)))   
            IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
            LGTH = ALE_CONNECT%ee_connect%iad_connect(II+1) - IAD2
            DO J=1,LGTH
              IDV = ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
              IF(IDV<=0)THEN
                VEC(1:3,J) = ZERO
                CYCLE
              ENDIF
              IX1=IXS(ICF(1,J)+1,II)          
              IX2=IXS(ICF(2,J)+1,II)          
              IX3=IXS(ICF(3,J)+1,II)          
              IX4=IXS(ICF(4,J)+1,II) 
              CF(1,I) = FOURTH*(X(1,IX1)+X(1,IX2)+X(1,IX3)+X(1,IX4))
              CF(2,I) = FOURTH*(X(2,IX1)+X(2,IX2)+X(2,IX3)+X(2,IX4))
              CF(3,I) = FOURTH*(X(3,IX1)+X(3,IX2)+X(3,IX3)+X(3,IX4))                
              NC(1,I)=IXS(2,IDV)
              NC(2,I)=IXS(3,IDV)
              NC(3,I)=IXS(4,IDV)
              NC(4,I)=IXS(5,IDV)
              NC(5,I)=IXS(6,IDV)
              NC(6,I)=IXS(7,IDV)
              NC(7,I)=IXS(8,IDV)
              NC(8,I)=IXS(9,IDV)
              Zadj(1) = ONE_OVER_8*SUM(X(1,NC(1:8,I)))
              Zadj(2) = ONE_OVER_8*SUM(X(2,NC(1:8,I)))
              Zadj(3) = ONE_OVER_8*SUM(X(3,NC(1:8,I)))                    
              ZZadj(1)  = Zadj(1)-Z(1)
              ZZadj(2)  = Zadj(2)-Z(2)
              ZZadj(3)  = Zadj(3)-Z(3)                    
              ZCf(1)    = Cf(1,I) - Z(1)
              ZCf(2)    = Cf(2,I) - Z(2)
              ZCf(3)    = Cf(3,I) - Z(3)                    
              PS        = ZCf(1)*ZZadj(1) + ZCf(2)*ZZadj(2) + ZCf(3)*ZZadj(3)
              ZZadj_    = ZZadj(1)**2 + ZZadj(2)**2 + ZZadj(3)**2
              LAMBDA    = PS / MAX(EM20,ZZadj_)
C             IF(lambda<ZERO .OR. lambda >ONE)then
C               print *, "labmda=", lambda
C               pause
C             endif
              LAMBDA    = MIN(MAX(ZERO,LAMBDA) , ONE)
              LAMBDA    = SIN(HALF*3.14159265358979D00*LAMBDA)
              LAMBDA    = LAMBDA * LAMBDA
              !face density
              SR1       = VALEL(4,I)
              SR2       = VALVOIS(4,J,I)              
              SRF       = SR1 + LAMBDA*(SR2-SR1)
              !face momentum density
              VEC(1,J)  = VALEL(1,I) + LAMBDA*(VALVOIS(1,J,I)-VALEL(1,I))
              VEC(2,J)  = VALEL(2,I) + LAMBDA*(VALVOIS(2,J,I)-VALEL(2,I))
              VEC(3,J)  = VALEL(3,I) + LAMBDA*(VALVOIS(3,J,I)-VALEL(3,I))                    
              !face velocity
              VEC(1,J)  = VEC(1,J) / SRF
              VEC(2,J)  = VEC(2,J) / SRF
              VEC(3,J)  = VEC(3,J) / SRF
            ENDDO!next J
            DO J=1,NV46
              ALEFVM_Buffer%F_FACE(1,J,II) = VEC(1,J)
              ALEFVM_Buffer%F_FACE(2,J,II) = VEC(2,J)
              ALEFVM_Buffer%F_FACE(3,J,II) = VEC(3,J)
            ENDDO        
            !---face-1
            VX1(I)           = VEC(1,1)
            VY1(I)           = VEC(2,1)
            VZ1(I)           = VEC(3,1)                
            !---face-2
            VX2(I)           = VEC(1,2)
            VY2(I)           = VEC(2,2)
            VZ2(I)           = VEC(3,2)                
            !---face-3
            VX3(I)           = VEC(1,3)
            VY3(I)           = VEC(2,3)
            VZ3(I)           = VEC(3,3)                
            !---face-4
            VX4(I)           = VEC(1,4)
            VY4(I)           = VEC(2,4)
            VZ4(I)           = VEC(3,4)                
            !---face-5
            VX5(I)           = VEC(1,5)
            VY5(I)           = VEC(2,5)
            VZ5(I)           = VEC(3,5)                
            !---face-6
            VX6(I)           = VEC(1,6)
            VY6(I)           = VEC(2,6)
            VZ6(I)           = VEC(3,6)                
          ENDDO!next I
          
      END SELECT !I_ALE_SOLVER 
      
      !======================================================!
      ! FLUXES CALCULATION ON EACH FACE                      !
      !    FLUX_face = [0.5*V_face] . [2S*n]                 !       
      !======================================================!
      DO I=1,NEL
        FLUX1(I)=HALF*(VX1(I)*N1X(I)+VY1(I)*N1Y(I)+VZ1(I)*N1Z(I))
        FLUX2(I)=HALF*(VX2(I)*N2X(I)+VY2(I)*N2Y(I)+VZ2(I)*N2Z(I))
        FLUX3(I)=HALF*(VX3(I)*N3X(I)+VY3(I)*N3Y(I)+VZ3(I)*N3Z(I))
        FLUX4(I)=HALF*(VX4(I)*N4X(I)+VY4(I)*N4Y(I)+VZ4(I)*N4Z(I))
        FLUX5(I)=HALF*(VX5(I)*N5X(I)+VY5(I)*N5Y(I)+VZ5(I)*N5Z(I))
        FLUX6(I)=HALF*(VX6(I)*N6X(I)+VY6(I)*N6Y(I)+VZ6(I)*N6Z(I))
      ENDDO



        !DEBUG-OUTPUT---------------!
        if(ALEFVM_Param%IOUTP_FLUX /= 0)then
          debug_outp = .FALSE.
          if(ALEFVM_Param%IOUTP_FLUX>0)then
            do i=lft,llt
              ii = nft + i
              if(ixs(11,ii)==ALEFVM_Param%IOUTP_FLUX)THEN
                debug_outp = .TRUE.
                idbf   = i
                idbl   = i
                EXIT
              endif
             enddo
          elseif(ALEFVM_Param%IOUTP_FLUX==-1)then
            debug_outp=.TRUE.
                idbf   = lft
                idbl   = llt          
          endif
!#!include "lockon.inc"  
          if(debug_outp)then     
!#!include "lockon.inc"       
          print *, "    |----alefvm_eflux3.F-----|"
          print *, "    |   THREAD INFORMATION   |"
          print *, "    |------------------------|" 
          print *, "     NCYCLE =", NCYCLE
          do i=idbf,idbl
            ii = nft + i
            print *,                    "      brique=", ixs(11,nft+i)
            write (*,FMT='(A,6E26.14)') "       Flux(1:6) =", FLUX1(I),FLUX2(I),FLUX3(I),FLUX4(I),FLUX5(I),FLUX6(I)
            write(*,FMT='(A24,1A20)')       "                        ",  
     .                                  "#--------- cell-----------"          
            write (*,FMT='(A,1E26.14)') "       V_cell-X  =", ALEFVM_Buffer%FCELL(1,II)           
            write (*,FMT='(A,1E26.14)') "       V_cell-Y  =", ALEFVM_Buffer%FCELL(2,II)           
            write (*,FMT='(A,1E26.14)') "       V_cell-Z  =", ALEFVM_Buffer%FCELL(3,II)  
            write(*,FMT='(A24,6A26)')   "                        ",    
     .                                  "#--------- adj_1 ---------","#--------- adj_2 ---------",
     .                                  "#--------- adj_3 ---------","#--------- adj_4 ---------",
     .                                  "#--------- adj_5 ---------","#--------- adj_6 --------#"
            write (*,FMT='(A,6E26.14)') "       V_faces-X =", ALEFVM_Buffer%F_FACE(1,1:6,II)           
            write (*,FMT='(A,6E26.14)') "       V_faces-Y =", ALEFVM_Buffer%F_FACE(2,1:6,II)
            write (*,FMT='(A,6E26.14)') "       V_faces-Z =", ALEFVM_Buffer%F_FACE(3,1:6,II)                        
            print *, "      "          
          enddo
!#!include "lockoff.inc"       
        endif
        endif
      !-----------------------------------------!


      !======================================================!
      ! TRIMATERIAL CASE INITIALIZATION (LAW51)              !
      ! -->RETURN                                            !
      !======================================================!
      IF(NINT(PM(19,MAT(1)))==51)THEN
        DO I=1,NEL
          FLUX(1,I)=FLUX1(I)
          FLUX(2,I)=FLUX2(I)
          FLUX(3,I)=FLUX3(I)
          FLUX(4,I)=FLUX4(I)
          FLUX(5,I)=FLUX5(I)
          FLUX(6,I)=FLUX6(I)
        ENDDO !next I
        RETURN
      ENDIF

      !======================================================!
      !  BOUNDARY FACE : no volume flux by default           !
      !    slip wall bc                                      !
      !======================================================!
      DO J=1,6
        DO I=1,NEL
          UPWL(J,I)=PM(16,MAT(I))
        ENDDO !next I
      ENDDO !next J

      DO I=1,NEL
       IAD2 = ALE_CONNECT%ee_connect%iad_connect(I + NFT)
       REDUC=PM(92,MAT(I))
       !---face1---!
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
       IF(II==0)THEN
        FLUX1(I)=FLUX1(I)*REDUC
       ENDIF
       !---face2---!
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
       IF(II==0)THEN
        FLUX2(I)=FLUX2(I)*REDUC
       ENDIF
       !---face3---!
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
       IF(II==0)THEN
        FLUX3(I)=FLUX3(I)*REDUC
       ENDIF
       !---face4---!
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
       IF(II==0)THEN
        FLUX4(I)=FLUX4(I)*REDUC
       ENDIF
       !---face5---!
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 5 - 1)
       IF(II==0)THEN
        FLUX5(I)=FLUX5(I)*REDUC
       ENDIF
       !---face6---!
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 6 - 1)
       IF(II==0)THEN
        FLUX6(I)=FLUX6(I)*REDUC
       ENDIF
      ENDDO !next I

      DO I=1,NEL
        FLUX(1,I)=FLUX1(I)-UPWL(1,I)*ABS(FLUX1(I))
        FLUX(2,I)=FLUX2(I)-UPWL(2,I)*ABS(FLUX2(I))
        FLUX(3,I)=FLUX3(I)-UPWL(3,I)*ABS(FLUX3(I))
        FLUX(4,I)=FLUX4(I)-UPWL(4,I)*ABS(FLUX4(I))
        FLUX(5,I)=FLUX5(I)-UPWL(5,I)*ABS(FLUX5(I))
        FLUX(6,I)=FLUX6(I)-UPWL(6,I)*ABS(FLUX6(I))

        FLU1(I)  =FLUX1(I)+UPWL(1,I)*ABS(FLUX1(I))
     .           +FLUX2(I)+UPWL(2,I)*ABS(FLUX2(I))
     .           +FLUX3(I)+UPWL(3,I)*ABS(FLUX3(I))
     .           +FLUX4(I)+UPWL(4,I)*ABS(FLUX4(I))
     .           +FLUX5(I)+UPWL(5,I)*ABS(FLUX5(I))
     .           +FLUX6(I)+UPWL(6,I)*ABS(FLUX6(I))
      ENDDO !next I





      RETURN
      END
