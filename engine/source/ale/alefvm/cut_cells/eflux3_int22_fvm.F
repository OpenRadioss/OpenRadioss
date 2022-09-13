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
Chd|  EFLUX3_INT22_FVM              source/ale/alefvm/cut_cells/eflux3_int22_fvm.F
Chd|-- called by -----------
Chd|        AFLUX0                        source/ale/aflux0.F           
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        ALEFVM_MOD                    ../common_source/modules/alefvm_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE EFLUX3_INT22_FVM(
     .           PM         , IXS   , V      , FLUX     , FLU1    ,
     .           VEUL       , IPARG  , ELBUF_TAB, ITASK   ,
     .           NV46       , IPM   , X)
C-----------------------------------------------
C   D e s c r i p t i o n
C----------------------------------------------- 
C 'alefvm' is related to a collocated scheme (built from FVM and based on Godunov scheme)
C  which was temporarily introduced for experimental option /INTER/TYPE22 (FSI coupling with cut cell method)
C This cut cell method is not completed, abandonned, and is not an official option.
C There is no other use for this scheme which is automatically enabled when /INTER/TYPE22 is defined (INT22>0 => IALEFVM=1).
C
C This subroutine is computing fluxes for cut cells.
C Result are stored in cut cell buffer (currently called BRICK_LIST) 
C Main Cell flux is already computed with the usual
C subroutine (EFLUX3)
C All Cell not in cut cell buffer only have uncut adjacent cells. Since
C a cut cell has all its adjacent cells in the cut cell buffer.
C Cells in cut cell buffer might have adjacent cut cells.
        !IPM(251,MAT(1)) = I_ALE_SOLVER
        ! 0: Default = 1 expect if /ALE/MAT or /EULER/MAT has IFROM flag defined.
        ! 1 : FEM
        ! 2 : FVM U average
        ! 3 : FVM rho.U average
        ! 4 : FVM rho.c.U average
        ! 5 : Godunov Acoustic
        ! 6 : experimental
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE I22BUFBRIC_MOD
      USE I22TRI_MOD 
      USE ELBUFDEF_MOD 
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
#include      "i22edges.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "alefvm.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IXS(NIXS,*), IPARG(NPARG,*),ISILENT, NV46,IPM(NPROPMI,*)
      my_real :: PM(NPROPM,*), V(3,*), FLUX(6,*), FLU1(*),VEUL(LVEUL,*),X(3,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP),TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MAT, MLW, NC(8), I, IE,J, K, Kv,L,ITASK, IDm, IDV, IDV2, IFLAG,IFV,IFvv,IC,II
      INTEGER ID, IDvm, IBvm, NGvm, IEvm, IEV, Jm, IMAT, IALEFVM_FLG
      INTEGER IB,IBv,IBvv, NIN, NBCUT,NBCUTv, ICELL,NCELL,NCELLv,FV,NGv,NGm, IPOS,NadjCell
      my_real
     .   X1(1), X2(1), X3(1), X4(1), X5(1), X6(1), X7(1), X8(1),
     .   Y1(1), Y2(1), Y3(1), Y4(1), Y5(1), Y6(1), Y7(1), Y8(1), Z1(1),
     .   Z2(1), Z3(1), Z4(1), Z5(1), Z6(1), Z7(1), Z8(1), NX(6),
     .   NY(6), NZ(6),VX1(MVSIZ), VX2(MVSIZ), VX3(MVSIZ), VX4(MVSIZ), VX5(MVSIZ), VX6(MVSIZ),
     .   VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), VY4(MVSIZ), VY5(MVSIZ), VY6(MVSIZ), VZ1(MVSIZ), VZ2(MVSIZ),
     .   VZ3(MVSIZ), VZ4(MVSIZ), VZ5(MVSIZ), VZ6(MVSIZ), VD(3,8),cellFLUX(6,9,NB,5),
     .   UPW(1),REDUC,UPWL(6),MLN
      my_real :: RATIOFACE(6), VF(3), Norm(3,6), LNorm(6),TERM2
      INTEGER :: NBF,NBL, MCELL,MNOD,iNOD,NNOD(6),ICELLv,ICELLvv, numnod, numnodV
      INTEGER,DIMENSION(:),   POINTER     :: pIsMain, pIsMainV  
      INTEGER,                POINTER     :: pNum, pNumv
      TYPE(G_BUFEL_)  ,POINTER            :: GBUF   
      my_real                             :: AREA , BAK , PHI , SGN, FACE , Z(3), Zadj(3), ZZadj_, CF(3), ZCf(3),ZZadj(3)
      my_real                             :: PS, LAMBDA
      INTEGER,POINTER                     :: pMainID, pMainIDv 
      INTEGER                             :: NUM, NADJ, IADJ, JV, NG, IE_M, IEm, IBm
      my_real                             :: FACEv, DDVOL, VALEL(6), VALVOIS(6,6,5), SR1, SR2, SRF
      LOGICAL debug_outp,bFOUND
C-----------------------------------------------
C-----------------------------------------------
C   P r e - C o n d i t i o n 
C-----------------------------------------------
      IF(INT22==0)RETURN      
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------      

      !======================================================!
      ! INITIALIZATIONS                                      !
      !======================================================! 
      NIN = 1
      NBF = 1+ITASK*NB/NTHREAD
      NBL = (ITASK+1)*NB/NTHREAD
      NBL = MIN(NBL,NB)

      !======================================================!
      ! DEBUG OUTPUT                                         !
      !======================================================!             
      !INTERFACE 22 ONLY - OUTPUT---------------!
      debug_outp = .false.
      if(ibug22_flux22/=0)then
        debug_outp = .false.
        if(ibug22_flux22>0)then
          do ib=nbf,nbl
             ie = brick_list(nin,ib)%id
            if(ixs(11,ie)==ibug22_flux22)debug_outp=.true.
          enddo
        elseif(ibug22_flux22==-1)then
          debug_outp = .true.
        endif
      endif
      if(debug_outp .AND. ITASK==0)then
        print *, "    |---------eflux3_int22_fvm.F---------|"
        print *, "    |       THREAD INFORMATION           |"
        print *, "    |------------------------------------|" 
        print *, "    NCYCLE  =", NCYCLE    
      endif
      !INTERFACE 22 ONLY - OUTPUT---------------!      



      !======================================================!
      ! STEP A : CUT CELL FLUXES                             !
      !    A.1 : main & SECND CELLS FLUXES                 !
      !======================================================!        
      DO IB=NBF,NBL  
        IE                =  BRICK_LIST(NIN,IB)%ID 
        MLW               =  BRICK_LIST(NIN,IB)%MLW               
        NCELL             =  BRICK_LIST(NIN,IB)%NBCUT 
        MCELL             =  BRICK_LIST(NIN,IB)%MainID       
        ICELL             =  0  
        
        !======================================================!
        ! RESET FLUXES                                         !
        !======================================================!
        FLUX(1:6,IE) = ZERO
        FLU1(IE)     = ZERO
        
        !======================================================!
        ! NORMAL VECTORS ON EACH DACE                          !
        !    2S[n] = [diag1] x [diag2]                         ! 
        !    where                                             !
        !      [n] : unitary normal vector on face             !
        !======================================================!
        Norm(1,1:6)   = BRICK_LIST(NIN,IB)%N(1:6,1) !VEUL(14:19,IE)
        Norm(2,1:6)   = BRICK_LIST(NIN,IB)%N(1:6,2) !VEUL(20:25,IE)
        Norm(3,1:6)   = BRICK_LIST(NIN,IB)%N(1:6,3) !VEUL(26:31,IE) 
        DO J=1,NV46
          LNorm(J)    = SQRT( Norm(1,J)**2 +  Norm(2,J)**2 + Norm(3,J)**2 )
          Norm(1:3,J) = Norm(1:3,J) / LNorm(J)
        ENDDO
        
        !======================================================!
        !COMPUTES LOCAL(VALEL) AND ADJACENT (VALVOIS) MOMENTUMS!
        !======================================================!        
        DO WHILE (ICELL<=NCELL) ! loop on polyhedron {1:NCELL} U {9}
          ICELL      = ICELL +1
          IF (ICELL>NCELL .AND. NCELL/=0)ICELL=9
          NBCUT                         = BRICK_LIST(NIN,IB)%NBCUT
          Jm                            = BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(1)
          IEm                           = BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(3) 
          IBm                           = BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(4)          
          IDm                           = BRICK_LIST(NIN,IBm)%ID
          NGm                           = BRICK_LIST(NIN,IBm)%NG
          GBUF                          =>ELBUF_TAB(NGm)%GBUF                                
          VALEL(1)                      = FCELL(1,IEm) !rho.ux
          VALEL(2)                      = FCELL(2,IEm) !rho.uy
          VALEL(3)                      = FCELL(3,IEm) !rho.uz
          VALEL(4)                      = FCELL(4,IEm) !rho
          VALEL(5)                      = FCELL(5,IEm) !ssp
          VALEL(6)                      = FCELL(6,IEm) !P  
          SR1                           = SQRT(VALEL(4))     
          DO J=1,NV46
            NAdj                        = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%NAdjCell
            IEv                         = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,1) 
            IBv                         = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,4)
            Jv                          = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,5)
            cellFLUX(J,ICELL,IB,1:5)    = ZERO             
            DO IADJ=1,NAdj
              ICELLv                    = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Adjacent_Cell(IADJ)
              IF(ICELLv==0)THEN
                VALVOIS(J,1:3,IADj)     = -VALEL(1:3)  
                VALVOIS(J,4,IADj)       =  VALEL(4)                  
              ELSE
                !IF INSIDE CUT CELL                
                IF(IBv/=0)THEN
                  IEvm                    = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%WhereIsMain(3)
                  IBvm                    = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%WhereIsMain(4) 
                  NGvm                    = BRICK_LIST(NIN,IBvm)%NG
                  IDvm                    = BRICK_LIST(NIN,IBvm)%IDLOC
                  VALVOIS(J,1,IADj)       = FCELL(1,IEvm)
                  VALVOIS(J,2,IADj)       = FCELL(2,IEvm)
                  VALVOIS(J,3,IADj)       = FCELL(3,IEvm)
                  VALVOIS(J,4,IADj)       = FCELL(4,IEvm)
                  VALVOIS(J,5,IADj)       = FCELL(5,IEvm)
                  VALVOIS(J,6,IADj)       = FCELL(6,IEvm)
                ELSE
                !NOT IN CUT CELL (FRONTIER TO USUAL DOMAIN)
                  VALVOIS(J,1,IADj)       = FCELL(1,IEv)
                  VALVOIS(J,2,IADj)       = FCELL(2,IEv)
                  VALVOIS(J,3,IADj)       = FCELL(3,IEv)
                  VALVOIS(J,4,IADj)       = FCELL(4,IEv)
                  VALVOIS(J,5,IADj)       = FCELL(5,IEv)  
                  VALVOIS(J,6,IADj)       = FCELL(6,IEv)                                  
                ENDIF                                             
              ENDIF
              SR2                         = SQRT(VALVOIS(J,4,IADj))
              !======================================================!
              ! FLUXES CALCULATION ON EACH FACE(J) AND EACH ADJ CELL !
              !======================================================!
              IMAT                          = IXS(1,IE)
              IALEFVM_FLG                   = IPM(251,IMAT)
              IF(IALEFVM_IFORM/=0)IALEFVM_FLG = IALEFVM_IFORM !for debug purpose if set in alefvm_init.F
              
              
              SELECT CASE(I_ALE_SOLVER)
                CASE(1)
                  !CENTERED FVM - U average              
                  VF(1)                     = HALF * (VALEL(1)/VALEL(4)+VALVOIS(J,1,IADj)/VALVOIS(J,4,IADj))
                  VF(2)                     = HALF * (VALEL(2)/VALEL(4)+VALVOIS(J,2,IADj)/VALVOIS(J,4,IADj))
                  VF(3)                     = HALF * (VALEL(3)/VALEL(4)+VALVOIS(J,3,IADj)/VALVOIS(J,4,IADj))                         
                CASE(2)
                  !CENTERED FVM - rho.U average              
                  VF(1)                     = (VALEL(1)+VALVOIS(J,1,IADj))/(VALEL(4)+VALVOIS(J,4,IADj))
                  VF(2)                     = (VALEL(2)+VALVOIS(J,2,IADj))/(VALEL(4)+VALVOIS(J,4,IADj))
                  VF(3)                     = (VALEL(3)+VALVOIS(J,3,IADj))/(VALEL(4)+VALVOIS(J,4,IADj))                                             
                CASE(3)
                  !CENTERED FVM - Roe-average             
                  VF(1)                     = (VALEL(1)/SR1+VALVOIS(J,1,IADj)/SR2)/(SR1+SR2)
                  VF(2)                     = (VALEL(2)/SR1+VALVOIS(J,2,IADj)/SR2)/(SR1+SR2)
                  VF(3)                     = (VALEL(3)/SR1+VALVOIS(J,3,IADj)/SR2)/(SR1+SR2)                                         
                CASE(4)
                  IF(DT1==ZERO)THEN
                    VF(1)                     = (VALEL(1)         +VALVOIS(J,1,IADj)                 ) 
     .                                         /(VALEL(4)         +VALVOIS(J,4,IADj)                 )
                    VF(2)                     = (VALEL(2)         +VALVOIS(J,2,IADj)                 )
     .                                         /(VALEL(4)         +VALVOIS(J,4,IADj)                 )
                    VF(3)                     = (VALEL(3)         +VALVOIS(J,3,IADj)                 )
     .                                         /(VALEL(4)         +VALVOIS(J,4,IADj)                 ) 
                  ELSE                
                
                  !CENTERED FVM - rho.c.U average              
                  VF(1)                     = (VALEL(1)*VALEL(5)+VALVOIS(J,1,IADj)*VALVOIS(J,5,IADj))
     .                                       /(VALEL(4)*VALEL(5)+VALVOIS(J,4,IADj)*VALVOIS(J,5,IADj))
                  VF(2)                     = (VALEL(2)*VALEL(5)+VALVOIS(J,2,IADj)*VALVOIS(J,5,IADj))
     .                                       /(VALEL(4)*VALEL(5)+VALVOIS(J,4,IADj)*VALVOIS(J,5,IADj))
                  VF(3)                     = (VALEL(3)*VALEL(5)+VALVOIS(J,3,IADj)*VALVOIS(J,5,IADj))
     .                                       /(VALEL(4)*VALEL(5)+VALVOIS(J,4,IADj)*VALVOIS(J,5,IADj))
     
                  ENDIF
                              
                CASE(5)
                  !Godunoc/Riemann Acoustic
                  IF(DT1==ZERO)THEN
                    VF(1)                     = (VALEL(1)         +VALVOIS(J,1,IADj)                 ) 
     .                                         /(VALEL(4)         +VALVOIS(J,4,IADj)                 )
                    VF(2)                     = (VALEL(2)         +VALVOIS(J,2,IADj)                 )
     .                                         /(VALEL(4)         +VALVOIS(J,4,IADj)                 )
                    VF(3)                     = (VALEL(3)         +VALVOIS(J,3,IADj)                 )
     .                                         /(VALEL(4)         +VALVOIS(J,4,IADj)                 ) 
                  ELSE
                    TERM2 = ( VALEL(6)-VALVOIS(J,6,IADj) )/ (VALEL(4)*VALEL(5)+VALVOIS(J,4,IADj)*VALVOIS(J,5,IADj))
                    VF(1)                     = (VALEL(1)*VALEL(5)+VALVOIS(J,1,IADj)*VALVOIS(J,5,IADj))
     .                                         /(VALEL(4)*VALEL(5)+VALVOIS(J,4,IADj)*VALVOIS(J,5,IADj)) + TERM2 *Norm(1,J)
                    VF(2)                     = (VALEL(2)*VALEL(5)+VALVOIS(J,2,IADj)*VALVOIS(J,5,IADj))
     .                                         /(VALEL(4)*VALEL(5)+VALVOIS(J,4,IADj)*VALVOIS(J,5,IADj)) + TERM2 *Norm(2,J)
                    VF(3)                     = (VALEL(3)*VALEL(5)+VALVOIS(J,3,IADj)*VALVOIS(J,5,IADj))
     .                                         /(VALEL(4)*VALEL(5)+VALVOIS(J,4,IADj)*VALVOIS(J,5,IADj)) + TERM2 *Norm(3,J)   
                  ENDIF                         
                CASE(6)              
                  Z(1:3)                    = BRICK_LIST(NIN,IBm)%SCellCenter(1:3)
                  IF(IBv /=0)THEN
                    Zadj(1:3)               = BRICK_LIST(NIN,IBvm)%ScellCenter(1:3)
                  ELSE
                    NC(1)                   = IXS(2,IEv)    
                    NC(2)                   = IXS(3,IEv)    
                    NC(3)                   = IXS(4,IEv)    
                    NC(4)                   = IXS(5,IEv)    
                    NC(5)                   = IXS(6,IEv)    
                    NC(6)                   = IXS(7,IEv)    
                    NC(7)                   = IXS(8,IEv)    
                    NC(8)                   = IXS(9,IEv)    
                    Zadj(1)                 = ONE_OVER_8*SUM(X(1,NC(1:8)))          
                    Zadj(2)                 = ONE_OVER_8*SUM(X(2,NC(1:8)))          
                    Zadj(3)                 = ONE_OVER_8*SUM(X(3,NC(1:8)))          
                  ENDIF
                  
                  CF(1:3)                   = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Center(1:3)
                  IF(IBv /= 0)THEN
                    FACE                    = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Surf
                    FACEv                   = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%FACE(Jv)%Surf 
                    IF(FACEv<FACE) CF(1:3) = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%FACE(Jv)%Center(1:3)
                  ENDIF
                  ZZadj(1)                  = Zadj(1)-Z(1)                                            
                  ZZadj(2)                  = Zadj(2)-Z(2)                                            
                  ZZadj(3)                  = Zadj(3)-Z(3)                                            
                  ZCf(1)                    = Cf(1) - Z(1)                                          
                  ZCf(2)                    = Cf(2) - Z(2)                                          
                  ZCf(3)                    = Cf(3) - Z(3)                                          
                  PS                        = ZCf(1)*ZZadj(1) + ZCf(2)*ZZadj(2) + ZCf(3)*ZZadj(3)     
                  ZZadj_                    = ZZadj(1)**2 + ZZadj(2)**2 + ZZadj(3)**2                 
                  LAMBDA                    = PS / MAX(EM20,ZZadj_)                                   
                  IF(lambda<ZERO .OR. lambda >ONE)then                          
                    print *, "labmda=", lambda                                       
c                   pause                                                            
                  endif                                                              
                  LAMBDA                    = MIN(MAX(ZERO,LAMBDA) , ONE)                             
                  LAMBDA                    = SIN(HALF*3.14159265358979D00*LAMBDA)                 
                  LAMBDA                    = LAMBDA * LAMBDA                                        
                  !face density                                                                      
                  SR1                       = VALEL(4)                                             
                  SR2                       = VALVOIS(J,4,IADj)                                         
                  SRF                       = SR1 + LAMBDA*(SR2-SR1)                                 
                  !face momentum density                                                             
                  VF(1)                     = VALEL(1) + LAMBDA*(VALVOIS(J,1,IADj)-VALEL(1))            
                  VF(2)                     = VALEL(2) + LAMBDA*(VALVOIS(J,2,IADj)-VALEL(2))            
                  VF(3)                     = VALEL(3) + LAMBDA*(VALVOIS(J,3,IADj)-VALEL(3))            
                  !face velocity                                                                     
                  VF(1)                     = VF(1) / SRF                                         
                  VF(2)                     = VF(2) / SRF                                         
                  VF(3)                     = VF(3) / SRF                                         
                  !CASE(2) gives
                  !VF(1)                     = (VALEL(1)+VALVOIS(J,1,IADj))/(VALEL(4)+VALVOIS(J,4,IADj))
                  !VF(2)                     = (VALEL(2)+VALVOIS(J,2,IADj))/(VALEL(4)+VALVOIS(J,4,IADj))
                  !VF(3)                     = (VALEL(3)+VALVOIS(J,3,IADj))/(VALEL(4)+VALVOIS(J,4,IADj))                   
              END SELECT
              
              
              cellFLUX(J,ICELL,IB,IADj)      = (VF(1)*Norm(1,J) + VF(2)*Norm(2,J) + VF(3)*Norm(3,J)) 
             !if(ncycle>=1 )then
             !if(ixs(11,ie)==1802081 )then
             ! print *, "brick   =", ixs(11,ie)
             ! print *, "brickV  =", ixs(11,brick_list(nin,ibv)%id), brick_list(nin,ibv)%id
             ! print *, "eflux3 - brickID=", ixs(11,ie), icell
             ! print *, "VF      =", VF(1:3)
             ! print *, "VALEL   =", VALEL(1:3)
             ! print *, "LAMBDA  =", LAMBDA
             ! print *, "VALVOIS =", VALVOIS(J,1:3,IADj)
             ! print *, "Norm    =", Norm(1:3,J)
             ! print *, "ICELLv  =", ICELLv
             ! print *, "IBv     =", IBV
             ! print *, "FCELL   =", FCELL(1:4,IEvm)
             !endif
             !endif
              
              BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Vel(1) = VF(1)
              BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Vel(2) = VF(2)
              BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Vel(3) = VF(3)  
                       
                           
              !======================================================!
              ! REDUCTION FACTOR FOR POLYHEDRA FACES                 !
              !======================================================!   
              !facette non communicante
              NUMNOD                        = BRICK_LIST(NIN,IB)%POLY(ICELL)%NumNOD
              K                             = 0
              DO L=1,NUMNOD                                     
                iNOD                        = BRICK_LIST(NIN,IB)%POLY(ICELL)%ListNodID(L)               
                IF(IsNodeOnFace(iNOD,J))K   = K +1                         
              ENDDO
              Face                          = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Surf
              IF(IBv==0)THEN
                !calcul de facette
                FaceV                       = Face
                NUMNODv                     = 8
                Kv                          = 1
              ELSE
                FaceV                       = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%FACE(Jv)%Surf 
                NUMNOD                      = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%NumNOD
                Kv                          = 0
                DO L=1,NUMNOD                                     
                  iNOD                      = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%ListNodID(L)               
                  IF(IsNodeOnFace(iNOD,Jv))Kv= Kv +1                         
                ENDDO   
              ENDIF
              Face                          = min(Face,FaceV)
              If(K==0 .OR. Kv==0) Face = ZERO !facette non communicante si pas de noeud de brique.
              IF(IBv/=0 .AND. IBm==IBvm) Face=ZERO         !blocage flux entre le main et son escalve
              cellFLUX(J,ICELL,IB,IADj)     = Face * cellFLUX(J,ICELL,IB,IADj)
!              write (*,FMT='(5I10,E30.16)') ixs(11,IE), J,ICELL,IB,IADj, Face 
              brick_list(nin,ib)%POLY(icell)%FACE(j)%Adjacent_FLUX(iadj) = cellFLUX(J,ICELL,IB,IADj)
            ENDDO!next IADj
          ENDDO!next J
        ENDDO!next ICELL
      ENDDO!next IB

      !======================================================!                             
      ! debug output                                         !                             
      !======================================================!                             
      if(debug_outp)then    
      call my_barrier
        if(itask==0)then                                                               
        if(ibug22_flux22>0 .OR. ibug22_flux22==-1)then                     
          print *, "    |------e22flux3_int22_fvm.F------|"  
          do ib=1,nb                                                    
            ie = brick_list(nin,ib)%Id 
            if (ibug22_flux22>0 .and. ibug22_flux22/= ixs(11,ie))CYCLE 
            icell = 0
            ncell = brick_list(nin,ib)%nbcut
            mcell = brick_list(nin,ib)%mainid
            NBCUT = ncell
            DO WHILE (ICELL<=NCELL) ! loop on polyhedron {1:NCELL} U {9}
              ICELL      = ICELL +1
              IF (ICELL>NCELL .AND. NCELL/=0)ICELL=9
              !print *, "      brique     =", ixs(11,ie)      
              !print *, "          id     =", brick_list(nin,ib)%id                                      
              !print *, "      NCYCLE     =", NCYCLE                                              
              !print *, "       icell     =", ICELL                                               
              !print *, "      _mcell     =", mcell                                               
              !print *, "       nbcut     =", NBCUT                                               
              do i=1,6                    
                NADJ = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(i)%NAdjCell  
                !do k=1,NADJ
                !IF(ABS(cellFLUX(i,ICELL,IB,K))<=EM20)   cellFLUX(i,ICELL,IB,K) = ZERO 
                !enddo
                                                                         
!         write (*,FMT='(A,I10,A,5E26.14)') "        phi(1:5,",i,")=", brick_list(nin,ib)%POLY(icell)%FACE(1)%Adjacent_FLUX(1:Nadj)
!         write (*,FMT='(A,I10,A,5E26.14)') "        phi(1:5,",i,")=", brick_list(nin,ib)%POLY(icell)%FACE(2)%Adjacent_FLUX(1:Nadj)
!         write (*,FMT='(A,I10,A,5E26.14)') "        phi(1:5,",i,")=", brick_list(nin,ib)%POLY(icell)%FACE(3)%Adjacent_FLUX(1:Nadj)
!         write (*,FMT='(A,I10,A,5E26.14)') "        phi(1:5,",i,")=", brick_list(nin,ib)%POLY(icell)%FACE(4)%Adjacent_FLUX(1:Nadj)
!         write (*,FMT='(A,I10,A,5E26.14)') "        phi(1:5,",i,")=", brick_list(nin,ib)%POLY(icell)%FACE(5)%Adjacent_FLUX(1:Nadj)
!         write (*,FMT='(A,I10,A,5E26.14)') "        phi(1:5,",i,")=", brick_list(nin,ib)%POLY(icell)%FACE(6)%Adjacent_FLUX(1:Nadj)
         
!                write (*,FMT='(A,6E26.14)')       "        vel-x         =", BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%Vel(1)
!                write (*,FMT='(A,6E26.14)')       "        vel-y         =", BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%Vel(2)
!                write (*,FMT='(A,6E26.14)')       "        vel-z         =", BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%Vel(3)
!                write (*,FMT='(A,6E26.14)')       "          N-x         =", BRICK_LIST(NIN,IB)%N(1:6,1)
!                write (*,FMT='(A,6E26.14)')       "          N-y         =", BRICK_LIST(NIN,IB)%N(1:6,2)
!                write (*,FMT='(A,6E26.14)')       "          N-z         =", BRICK_LIST(NIN,IB)%N(1:6,3)
              enddo 
            enddo! next ICELL
          enddo!next IB
          print *, "    ------------------------"                                            
        endif  
        endif                                                                              
      endif                                                                                
        
      !======================================================!


      !==============!
      CALL MY_BARRIER
      !==============!
              
      !======================================================!
      ! STEP B : NON COFORM MESH                             !
      !    USE CONSISTENT FLUX                               !
      !======================================================!        
      DO IB=NBF,NBL  
        IE                =  BRICK_LIST(NIN,IB)%ID 
        MLW               =  BRICK_LIST(NIN,IB)%MLW               
        NCELL             =  BRICK_LIST(NIN,IB)%NBCUT 
        MCELL             =  BRICK_LIST(NIN,IB)%MainID       
        ICELL             =  0  
        DO WHILE (ICELL<=NCELL) ! loop on polyhedron {1:NCELL} U {9}
          ICELL = ICELL +1
          IF (ICELL>NCELL .AND. NCELL/=0)ICELL=9 
          DO J=1,6
          BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Adjacent_upwFLUX (1)  = ZERO
          BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Adjacent_upwFLUX (2)  = ZERO
          BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Adjacent_upwFLUX (3)  = ZERO
          BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Adjacent_upwFLUX (4)  = ZERO
          BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Adjacent_upwFLUX (5)  = ZERO
          ENDDO!next J
          
          BRICK_LIST(NIN,IB)%POLY(ICELL)%Adjacent_FLU1         = ZERO
          !======================================================!
          !  MONOMATERIAL UPWIND TREATMENT                       !          
          !======================================================!          
          IE_M      = BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(3)
          MAT       = IXS(1,IE_M)               
          UPWL(1:6) = PM(16,MAT)
          REDUC     = PM(92,MAT)
          DDVOL     = ZERO
          DO J=1,6
            NADJ   = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%NAdjCell
            ICLOSE = BRICK_LIST(NIN,IB)%ClosedSurf(J)   !pour l'hypothese de surface fermee : double coupe sur la facette et pas de partitionnement de l'autre cote => ON FERME.
            IF(ICLOSE==1) NADJ=0
            DO IADJ = 1,NADJ
              IDV    = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,1)
              IBv    = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,4)
              Jv     = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,5)              
              ICELLv = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Adjacent_Cell(IADJ)
              IF(IDV==0)THEN
               cellFLUX(J,ICELL,IB,IADJ)=cellFLUX(J,ICELL,IB,IADJ)*REDUC
              ELSEIF(IDV>0)THEN
               NG      = BRICK_LIST(NIN,IB)%NG
               ISILENT = IPARG(64,NG)
               IF(ISILENT==1)THEN
                 UPWL(J)=ONE
                 cellFLUX(J,ICELL,IB,IADJ)=cellFLUX(J,ICELL,IB,IADJ)*PM(92,IXS(1,IDV))
               ENDIF
              ENDIF 
              BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Adjacent_upwFLUX(IADJ) = 
     .         cellFLUX(J,ICELL,IB,IADJ)-UPWL(J)*ABS(cellFLUX(J,ICELL,IB,IADJ))
              BRICK_LIST(NIN,IB)%POLY(ICELL)%Adjacent_FLU1 = 
     .        BRICK_LIST(NIN,IB)%POLY(ICELL)%Adjacent_FLU1 + cellFLUX(J,ICELL,IB,IADJ)+UPWL(J)*ABS(cellFLUX(J,ICELL,IB,IADJ))
              !-----DDVOL
              DDVOL = DDVOL + cellFLUX(J,ICELL,IB,IADJ) 
              !-----DDVOL*DT IS SUM FOR INCOMING AND OUTCOMING VOLUMES             
            ENDDO!next IADJ
          ENDDO!next J
          
          BRICK_LIST(NIN,IB)%POLY(ICELL)%DDVOL = DDVOL          

          !======================================================!                  
        ENDDO!next ICELL                                                               
      ENDDO!next IB

      !==============!
      CALL MY_BARRIER
      !==============!

      !---------------------------------------------------------!
      ! SECND CELLS STACK                                       !
      !---------------------------------------------------------!
      !STACK Secnd cells values from ones connected to current main cell  
      IF(INT22>0)THEN                                                       
        NIN = 1                                                             
        DO IB=NBF,NBL                                                                                                        
          MLW   =  BRICK_LIST(NIN,IB)%MLW              
          NUM   = BRICK_LIST(NIN,IB)%SecndList%Num                          
          MCELL = BRICK_LIST(NIN,IB)%mainID      
          DDVOL = ZERO 
          DO K=1,NUM                                                        
            IBV    = BRICK_LIST(NIN,IB)%SecndList%IBV(K)                    
            ICELLv = BRICK_LIST(NIN,IB)%SecndList%ICELLv(K)                 
            DDVOL  = DDVOL + BRICK_LIST(NIN,IBv)%POLY(ICELLv)%DDVOL 
          ENDDO                                                             
          DDVOL = DDVOL + BRICK_LIST(NIN,IB)%POLY(MCELL)%DDVOL 
          !updating ddvol cumulative stack
          BRICK_LIST(NIN,IB)%POLY(MCELL)%DDVOL = DDVOL                          
        ENDDO!next IB                                                        
      ENDIF              

      !==============!
      CALL MY_BARRIER
      !==============!

      RETURN
      END
