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
Chd|  INIMOM_FVM                    source/elements/solid/solide/inimom_fvm.F
Chd|-- called by -----------
Chd|        SINIT3                        source/elements/solid/solide/sinit3.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        ALEFVM_MOD                    ../common_source/modules/ale/alefvm_mod.F
Chd|====================================================================
      SUBROUTINE INIMOM_FVM( 
     .                       V  ,   RHO, VOL   , MOM ,IXS ,
     .                       IPM,   MAT, IPARG1, NPF ,TF  ,
     .                       PM ,   SSP, SIG   , NEL
     .                     )
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
#include      "param_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NEL
      my_real :: V(3,*)     , RHO(*)        , VOL(*)    , MOM(NEL,3)     , TF(*) , PM(NPROPM,*) , SSP(*), SIG(NEL,6)
      INTEGER :: IXS(NIXS,*), IPM(NPROPMI,*), MAT(MVSIZ), IPARG1(NPARG), NPF(*) 
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C
C  +------------+------------+ -----> v2(1:3)  =>  q2(1:3) = RHO*V/4 * v2(1:3)
C  |            |            |
C  |            |    V/4     |
C  |    q1      |    RHO     |      
C  |            |   q2(1:3)  |
C  |            |            |
C  +------------+------------+         q_elem(1:3) = sum( qi ,i=1..4)
C  |            |            |         ______________________________
C  |            |            |
C  |    q4      |     q3     |
C  |            |            |
C  |            |            |
C  +------------+------------+
C  
C                             ACONVE()
C Warning : rho * vel [in]   --------->  rho*V * vel [out]
C   Here rho*vel are stored.
C
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER          :: I, J, II, IALEFVM_FLG,IALEL,IVxFUN, IVyFUN, IVzFUN, MX, MLW, ITYP
      my_real          :: SUBVOL, Vcell(3), Vnod(3,8),
     .                    VX1(MVSIZ), VX2(MVSIZ), VX3(MVSIZ), VX4(MVSIZ), VX5(MVSIZ), VX6(MVSIZ), VX7(MVSIZ), VX8(MVSIZ),
     .                    VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), VY4(MVSIZ), VY5(MVSIZ), VY6(MVSIZ), VY7(MVSIZ), VY8(MVSIZ),
     .                    VZ1(MVSIZ), VZ2(MVSIZ), VZ3(MVSIZ), VZ4(MVSIZ), VZ5(MVSIZ), VZ6(MVSIZ), VZ7(MVSIZ), VZ8(MVSIZ),
     .                    Vx0       , Vy0       , Vz0,
     .                    Vx        , Vy        , Vz ,
     .                    DYDX      , T0
      my_real,EXTERNAL :: FINTER   
C-----------------------------------------------
C   P r e - C o n d i t i o n s 
C-----------------------------------------------
C Check activation flag for FVM momentum

        IALEL = IPARG1(7)+IPARG1(11)
        IF(IALEL == 0)RETURN
        IF(ALEFVM_Param%IEnabled==0)RETURN !global unplug if option not present in input file
                
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------

      !-----------------------------------------
      !   IF DEFINED /ALE/SOLVER/
      !-----------------------------------------      
      !automatic FVM SOLVER for ALE
      IF(INT22>0)THEN
        IF(ALEFVM_Param%ISOLVER<=1) THEN
          ALEFVM_Param%ISOLVER=5
          IF(ALEFVM_Param%IPRINT_1==0)THEN
            !print *, "** WARNING INT22 : UNDEFINED /ALE/SOLVER CARD"
            !print *, "                   ENABLING IT AUTOMATICALLY WITH IMOM_VEL=5"
            ALEFVM_Param%IPRINT_1 = 1
          ENDIF
        ELSEIF(ALEFVM_Param%ISOLVER/=5)THEN
          ALEFVM_Param%ISOLVER=5
          IF(ALEFVM_Param%IPRINT_2==0)THEN
            print *, "** WARNING INT22 : /ALE/SOLVER CARD"
            print *, "                   RECOMMENDED VALUE IS IMOM_VEL=5"
            ALEFVM_Param%IPRINT_2 = 1
          ENDIF
        ENDIF
      ENDIF
      
      IF(ALEFVM_Param%ISOLVER>0)THEN
        IPM(251,MAT(1)) = ALEFVM_Param%ISOLVER
        ! 0: Default = 1 expect if /ALE/MAT or /EULER/MAT has IFROM flag defined.
        ! 1 : FEM
        ! 2 : FVM U average
        ! 3 : FVM rho.U average
        ! 4 : FVM rho.c.U average
        ! 5 : Godunov Acoustic
        ! 6 : experimental
      ENDIF


      MLW = IPARG1(1)
      !-----------------------------------------
      
      !-----------------------------------------      
      IF(MLW /= 11) THEN

        DO  I=LFT,LLT
          II          = I+NFT
          !---nod-1
          VX1(I)      = V(1,IXS(2,II))
          VY1(I)      = V(2,IXS(2,II))    
          VZ1(I)      = V(3,IXS(2,II))
          !---nod-2
          VX2(I)      = V(1,IXS(3,II))
          VY2(I)      = V(2,IXS(3,II))    
          VZ2(I)      = V(3,IXS(3,II))
          !---nod-3
          VX3(I)      = V(1,IXS(4,II))
          VY3(I)      = V(2,IXS(4,II))    
          VZ3(I)      = V(3,IXS(4,II))
          !---nod-4
          VX4(I)      = V(1,IXS(5,II))
          VY4(I)      = V(2,IXS(5,II))    
          VZ4(I)      = V(3,IXS(5,II))
          !---nod-5
          VX5(I)      = V(1,IXS(6,II))
          VY5(I)      = V(2,IXS(6,II))    
          VZ5(I)      = V(3,IXS(6,II))
          !---nod-6
          VX6(I)      = V(1,IXS(7,II))
          VY6(I)      = V(2,IXS(7,II))    
          VZ6(I)      = V(3,IXS(7,II))
          !---nod-7
          VX7(I)      = V(1,IXS(8,II))
          VY7(I)      = V(2,IXS(8,II))    
          VZ7(I)      = V(3,IXS(8,II))
          !---nod-8
          VX8(I)      = V(1,IXS(9,II))
          VY8(I)      = V(2,IXS(9,II))    
          VZ8(I)      = V(3,IXS(9,II))
          !---cell_velocity---!
          Vcell(1)    = ONE_OVER_8 * (VX1(I)+VX2(I)+VX3(I)+VX4(I)+VX5(I)+VX6(I)+VX7(I)+VX8(I))
          Vcell(2)    = ONE_OVER_8 * (VY1(I)+VY2(I)+VY3(I)+VY4(I)+VY5(I)+VY6(I)+VY7(I)+VY8(I))
          Vcell(3)    = ONE_OVER_8 * (VZ1(I)+VZ2(I)+VZ3(I)+VZ4(I)+VZ5(I)+VZ6(I)+VZ7(I)+VZ8(I))               
          !---momentum---!
          !MOM(I,1:3)  = RHO(I) * Vcell(1:3)
          !rho*VOL* VEL in aconve. VEL is needed before for FLUXES
          MOM(I,1) = Vcell(1)*RHO(I)
          MOM(I,2) = Vcell(2)*RHO(I)
          MOM(I,3) = Vcell(3)*RHO(I)         
        ENDDO
      
        !write(*,*) "MOMENTUM from inimom_fvm.F"
        DO I=LFT,LLT
          II          = I+NFT
          ALEFVM_Buffer%FCELL(1,II) = MOM(I,1)
          ALEFVM_Buffer%FCELL(2,II) = MOM(I,2)
          ALEFVM_Buffer%FCELL(3,II) = MOM(I,3)  
          ALEFVM_Buffer%FCELL(4,II) = RHO(I) 
          ALEFVM_Buffer%FCELL(5,II) = SSP(I)
          ALEFVM_Buffer%FCELL(6,II) = -THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3)) 
        ENDDO
        
      !-----------------------------------------
      !   MATERIAL BOUNDARY : IMPOSED
      !-----------------------------------------  
      ELSE

        !MLW = 11
        MX     = MAT(LFT)        
        IVxFUN = IPM(18,MX)
        IVyFUN = IPM(19,MX)
        IVzFUN = IPM(20,MX)
        ITYP   = PM(50,MX)        
        Vx0    = PM(101,MX)     
        Vy0    = PM(102,MX)
        Vz0    = PM(103,MX)
        
        IF(ITYP == 2)THEN
          T0 = ZERO
          
          IF(IVxFUN>0)THEN
            Vx = Vx0*FINTER(IVxFUN,T0,NPF,TF,DYDX)
          ELSE
            Vx = Vx0
          ENDIF  

          IF(IVyFUN>0)THEN
            Vy = Vy0*FINTER(IVyFUN,T0,NPF,TF,DYDX)
          ELSE
            Vy = Vy0
          ENDIF  

          IF(IVzFUN>0)THEN
            Vz = Vz0*FINTER(IVzFUN,T0,NPF,TF,DYDX)
          ELSE
            Vz = Vz0
          ENDIF
          
          DO I=LFT,LLT
            II          = I+NFT
            MOM(I,1)	= Vx*RHO(I)
            MOM(I,2)	= Vy*RHO(I)
            MOM(I,3)	= Vz*RHO(I)		    
            ALEFVM_Buffer%FCELL(1,II) = MOM(I,1)
            ALEFVM_Buffer%FCELL(2,II) = MOM(I,2)
            ALEFVM_Buffer%FCELL(3,II) = MOM(I,3)  
            ALEFVM_Buffer%FCELL(4,II) = RHO(I)
            ALEFVM_Buffer%FCELL(5,II) = SSP(I)
            ALEFVM_Buffer%FCELL(6,II) = -THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
          ENDDO                              
        ELSEIF(ITYP == 3)THEN
          T0 = ZERO
          Vx=ZERO
          Vy=ZERO
          Vz=ZERO
          DO I=LFT,LLT
            II          = I+NFT
            MOM(I,1)	= Vx*RHO(I)
            MOM(I,2)	= Vy*RHO(I)
            MOM(I,3)	= Vz*RHO(I)		    
            ALEFVM_Buffer%FCELL(1,II) = MOM(I,1)
            ALEFVM_Buffer%FCELL(2,II) = MOM(I,2)
            ALEFVM_Buffer%FCELL(3,II) = MOM(I,3)  
            ALEFVM_Buffer%FCELL(4,II) = RHO(I)
            ALEFVM_Buffer%FCELL(5,II) = SSP(I)
            ALEFVM_Buffer%FCELL(6,II) = -THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
          ENDDO                              
        ENDIF
        
      ENDIF


C-----------------------------------------------
      RETURN
      END
