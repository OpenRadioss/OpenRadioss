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
Chd|  ALEFVM_SFINT3_INT22           source/ale/alefvm/alefvm_sfint3_int22.F
Chd|-- called by -----------
Chd|        ALEFVM_MAIN                   source/ale/alefvm/alefvm_main.F
Chd|-- calls ---------------
Chd|        ALEFVM_MOD                    ../common_source/modules/alefvm_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE ALEFVM_SFINT3_INT22
     .                         (
     1                          IXS ,  NV46 ,  IALEFVM_FLG, ITASK,
     2                          IPM ,  IPARG,  NG   , NALE       , A    ,
     3                          X   ,  V    ,  BID1 , BID2       , MS   ,
     4                          BID3,  BID4 ,  W    , NBF        , NBL  ,
     5                          tNB ,  NIN
     .                          )
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
      USE I22BUFBRIC_MOD
      USE I22TRI_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "nsvis_c.inc"
#include      "fluid_scr_c.inc"
#include      "inter22.inc"
#include      "param_c.inc"
#include      "alefvm.inc"
#include      "task_c.inc"
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
      INTEGER :: IXS(NIXS,*),NV46,IALEFVM_FLG, ITASK,IPM(NPROPMI,*),IPARG(NPARG,*),NG ,NALE(*)
      INTEGER :: NBF, NBL, tNB, NIN
      my_real :: A(3,*),X(3,*),V(3,*),MS(*),W(3,*),BID1,BID2,BID3,BID4
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      LOGICAL :: debug_outp
      INTEGER :: idebug, idbf,idbl, IB, IBm, IBs, IADJ, MCELL, NumSECND, NADJ, ISECND
      INTEGER :: I, II, J, IBv, NBCUT, NBCUTv, K, IV
      INTEGER :: ICELLv, JV, ICELLs, ClosedSurface, IPRES_MOM, ISGN_NORM
      my_real :: F0(3), SURF, Pf
      my_real :: NORM, FN,FNX,FNY,FNZ, Z1,Z2, P1,P2, UN1,UN2, DENOM
      my_real :: N(3), AREA, AREAv, AREAmin, Js, SumAREAv, AREA_trunc, COEF1, COEF2
      my_real :: THETA, M1, M2, Mf
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------      
      IF(IALEFVM==0)    RETURN
      IF(INT22 == 0) RETURN   
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------

c      0. Domaine d'integration : supercell = main u (U secnd)
c      1. Calcul de Pf
c      2. Calcul de Ff(1:3) = -Pf.S.n
c      3. Assemblge de FINT(1:3) = Sum Ff(1:3)

      !IPRES_MOM = 0,1,2,3,4  : (P1+P2)/2
      !IPRES_MOM =         5  : (rho1c1P1+rho2c2P2)/(rho1c1+rho2c2) + (rho1c1*rho2c2/(rho1c1+rho2c2)) <uel-uadj,nel>  acoustic solver 
      
      IPRES_MOM =  I_ALE_SOLVER
      
!      print *, "IPRES_MOM, SFINT3, int22", IPRES_MOM

      SELECT CASE(IPRES_MOM)
        CASE (5)
          !Godunov
          COEF1 = ZERO
          COEF2 = ONE
        CASE DEFAULT
          COEF1 = ONE
          COEF2 = ZERO
      END SELECT
     
      !UN1 is <U1,N1>
      !UN2 is <U2,N2>
      !then (<Ucell-Uadj,ncell> = UN1+UN2
     
      !Inner Forces
      !------------
      DO IB=NBF,NBL
        !---STARTING WITH main CELL
        !-----------------------------------
        F0(1:3)           = ZERO
        II                = BRICK_LIST(NIN,IB)%ID
        MCELL             = BRICK_LIST(NIN,IB)%MainID
        NBCUT             = BRICK_LIST(NIN,IB)%NBCUT         
        IF (MCELL==0)CYCLE  !no internal force to compute at this centroid
        !init. with main cut faces
        P1  = BRICK_LIST(NIN,IB)%SIG(0)
        Z1  = BRICK_LIST(NIN,IB)%RHOC
        M1  = BRICK_LIST(NIN,IB)%MACH
        
        IF(NBCUT>0)THEN
        !---Face-0
        !-----------------------------------
          ISGN_NORM = ONE
          IF(MCELL==9)THEN
            ISGN_NORM=-ONE
            DO K=1,NBCUT
              N(1:3) = ISGN_NORM* BRICK_LIST(NIN,IB)%PCUT(K)%N(1:3)
              NORM   = SQRT(N(1)**2+N(2)**2+N(3)**2) 
              N(1)   = N(1)/NORM
              N(2)   = N(2)/NORM
              N(3)   = N(3)/NORM
              SURF   = BRICK_LIST(NIN,IB)%PCUT(K)%SCUT(1)
              UN1    = BRICK_LIST(NIN,IB)%POLY(MCELL)%FACE0%U_N(K)
              Mf     = M1
              THETA  = MIN(ONE,Mf) !see dellacherie,omnes,raviart : fixing low mach issue
              Pf     = P1 + COEF2*THETA*HALF*Z1*UN1
              F0(1)  = F0(1) -Pf*SURF*N(1)
              F0(2)  = F0(2) -Pf*SURF*N(2)
              F0(3)  = F0(3) -Pf*SURF*N(3)
            ENDDO!next K
          ELSE
              ISGN_NORM = ONE
              K         = MCELL
              N(1:3)    = ISGN_NORM* BRICK_LIST(NIN,IB)%PCUT(K)%N(1:3)
              NORM      = SQRT(N(1)**2+N(2)**2+N(3)**2) 
              N(1)      = N(1)/NORM
              N(2)      = N(2)/NORM
              N(3)      = N(3)/NORM
              SURF      = BRICK_LIST(NIN,IB)%PCUT(K)%SCUT(1)
              UN1       = BRICK_LIST(NIN,IB)%POLY(MCELL)%FACE0%U_N(K)
              !Pf        = P1
              Mf        = M1
              THETA     = MIN(ONE,Mf) !see dellacherie,omnes,raviart : fixing low mach issue
              Pf        = P1 + COEF2*THETA*HALF*Z1*UN1
              F0(1)     = F0(1) -Pf*SURF*N(1)
              F0(2)     = F0(2) -Pf*SURF*N(2)
              F0(3)     = F0(3) -Pf*SURF*N(3)  
          ENDIF    
        ENDIF
        
        !--Face-1:6
        !-----------------------------------
        DO J=1,NV46
          NADJ              =  BRICK_LIST(NIN,IB)%POLY(MCELL)%FACE(J)%NAdjCell
          ClosedSurface     =  BRICK_LIST(NIN,IB)%ClosedSurf(J)
          !
          !   +---------+---------+
          !   +       \ +         +
          !   +        \+         +
          !   +         +         +
          !   +         +  <-----------    closed surface
          !   +         +         +         
          !   +        /+         +
          !   +       / +         +
          !   +---------+---------+
          !
          !if(closedsurface == 1)then
          !  print *, "brick_id=",ixs(11,ii),"    face=",J, "  has a closed surface"
          !  print *, "internal forces treated without adjacent cell."
          !endif
          IF(NADJ==0 .OR. ClosedSurface == 1)THEN
            !sliding rigid wall boundary condition
            UN1    = BRICK_LIST(NIN,IB)%POLY(MCELL)%FACE(J)%U_N
            UN2    = ZERO
            Mf     = M1
            THETA  = MIN(ONE,Mf) !see dellacherie,omnes,raviart : fixing low mach issue
            !Pf     = P1
            Pf     = P1 + COEF2*THETA*HALF*Z1*UN1
            N(1:3) = BRICK_LIST(NIN,IB)%N(J,1:3) 
            SURF   = BRICK_LIST(NIN,IB)%POLY(MCELL)%FACE(J)%Surf
            F0(1)  =  F0(1) -Pf*SURF*N(1)
            F0(2)  =  F0(2) -Pf*SURF*N(2)
            F0(3)  =  F0(3) -Pf*SURF*N(3)
          ELSE
            N(1:3)          = BRICK_LIST(NIN,IB)%N(J,1:3)
            AREA            = BRICK_LIST(NIN,IB)%POLY(MCELL)%FACE(J)%Surf 
            IV              = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,1)
            IBV             = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,4)
            JV              = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,5)   
            UN1             = BRICK_LIST(NIN,IB)%POLY(MCELL)%FACE(J)%U_N  
            IF(AREA<=ZERO)CYCLE             
            DO IADJ=1,NADJ     
              ICELLv        = BRICK_LIST(NIN,IB)%POLY(MCELL)%FACE(J)%Adjacent_Cell(IADJ)
              IBM           = 0
              IF(IBv/=0)THEN
                IBM         = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%WhereIsMain(4)
                IF(IBM == IB)CYCLE   ! inner face skipped  
                AREAv       = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%FACE(Jv)%Surf
                SURF        = MIN(AREA,AREAv)
                P2          = BRICK_LIST(NIN,IBm)%SIG(0)
                Z2          = BRICK_LIST(NIN,IBm)%RHOC
                M2          = BRICK_LIST(NIN,IBm)%MACH
                UN2         = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%FACE(JV)%U_N
                DENOM       = Z1+Z2
                !Pf          = COEF1*(HALF*(P1+P2)) + COEF2*((Z1*P2 + Z2*P1)/DENOM 
                Mf          = MIN(M1,M2)
                THETA       = MIN(ONE,Mf) !see dellacherie,omnes,raviart : fixing low mach issue
                Pf          = COEF1*(HALF*(P1+P2)) + COEF2*((Z1*P2 + Z2*P1)/DENOM  +  THETA*Z1*Z2*(UN1+UN2)/DENOM)                                                                           
                F0(1)       = F0(1) - Pf*SURF*N(1) 
                F0(2)       = F0(2) - Pf*SURF*N(2) 
                F0(3)       = F0(3) - Pf*SURF*N(3)
              ELSE! THEN IBV==0
                P2          = F_FACE(1  ,4  ,IV)
                Z2          = F_FACE(1  ,3  ,IV)
                UN2         = F_FACE(3  ,JV ,IV)
                M2          = F_FACE(1  ,5  ,IV)
                SURF        = AREA
                DENOM       = Z1+Z2
                !Pf          = COEF1*(HALF*(P1+P2)) + COEF2*((Z1*P2 + Z2*P1)/DENOM 
                Mf          = MIN(M1,M2)
                THETA       = MIN(ONE,Mf) !see dellacherie,omnes,raviart : fixing low mach issue
                Pf          = COEF1*(HALF*(P1+P2)) + COEF2*((Z1*P2 + Z2*P1)/DENOM  +  THETA*Z1*Z2*(UN1+UN2)/DENOM)                
                F0(1)       = F0(1) - Pf*SURF*N(1)
                F0(2)       = F0(2) - Pf*SURF*N(2)                  
                F0(3)       = F0(3) - Pf*SURF*N(3)     
              ENDIF
            ENDDO!next IADJ
          ENDIF  !(NADJ==0)  
          
        ENDDO!next J
                
        !---GO ON WITH ATTACHED SECND CELLS
        !-----------------------------------
        NumSECND = BRICK_LIST(NIN,IB)%SecndList%Num    
        DO ISECND=1,NumSECND
          IBs           = BRICK_LIST(NIN,IB)%SecndList%IBV(ISecnd)                                                                 
          ICELLs        = BRICK_LIST(NIN,IB)%SecndList%ICELLv(ISecnd)                                                              
          Js            = BRICK_LIST(NIN,IBs)%POLY(ICELLs)%WhereIsMain(1)                                                        
          NBCUTv        = BRICK_LIST(NIN,IBs)%NBCUT
          !---Face-0                                                                                                               
          !----------------------------------- 
          
          !non ne pas sommer sur les plans de coupes il ne sont pas forcemment tous voisins :
          !example : inter22/1D_EXPANSION/9ELEMS/LAW51/PerfectGas/EULER/SMALL_MAIN/0.FVM/MAT51
          !print *, "pause" ; pause
          
         !! boucler si icells=9
          
                                                                                              
          ISGN_NORM = ONE                                                                                                           
          IF(ICELLs==9)THEN
            ISGN_NORM=-ONE 
            DO K=1,NBCUTv                                                                                              
              N(1:3) = ISGN_NORM* BRICK_LIST(NIN,IBs)%PCUT(K)%N(1:3)
              NORM   = SQRT(N(1)**2+N(2)**2+N(3)**2) 
              N(1)   = N(1)/NORM
              N(2)   = N(2)/NORM
              N(3)   = N(3)/NORM                                                                              
              SURF   = BRICK_LIST(NIN,IBs)%PCUT(K)%SCUT(1)                                                                            
              UN1    = BRICK_LIST(NIN,IBs)%POLY(ICELLs)%FACE0%U_N(K) 
              !Pf     = P1      
              Mf     = M1
              THETA  = MIN(ONE,Mf) !see dellacherie,omnes,raviart : fixing low mach issue                                                             
              Pf     = P1 + COEF2*THETA*HALF*Z1*UN1                                                                                             
              F0(1)  = F0(1) -Pf*SURF*N(1)                                                                                           
              F0(2)  = F0(2) -Pf*SURF*N(2)                                                                                           
              F0(3)  = F0(3) -Pf*SURF*N(3) 
            ENDDO  
          ELSE
            K=ICELLs                                                                                             
            N(1:3) = ISGN_NORM* BRICK_LIST(NIN,IBs)%PCUT(K)%N(1:3)
            NORM   = SQRT(N(1)**2+N(2)**2+N(3)**2) 
            N(1)   = N(1)/NORM
            N(2)   = N(2)/NORM
            N(3)   = N(3)/NORM                                                                              
            SURF   = BRICK_LIST(NIN,IBs)%PCUT(K)%SCUT(1)                                                                            
            UN1    = BRICK_LIST(NIN,IBs)%POLY(ICELLs)%FACE0%U_N(K)
            Mf     = M1
            THETA  = MIN(ONE,Mf) !see dellacherie,omnes,raviart : fixing low mach issue                                                             
            !Pf     = P1                                                                   
            Pf     = P1 + COEF2*THETA*HALF*Z1*UN1                                                                                             
            F0(1)  = F0(1) -Pf*SURF*N(1)                                                                                           
            F0(2)  = F0(2) -Pf*SURF*N(2)                                                                                           
            F0(3)  = F0(3) -Pf*SURF*N(3) 
          ENDIF                                                                                            

          !---Face-1:6                                                                                                               
          !-----------------------------------                                                                                     
          DO J=1,NV46                                                                                                              
            IF(J==Js) CYCLE ! do no take into account internal face (inside supercell), i.e. between main cell and secnd cell  
            NADJ     = BRICK_LIST(NIN,IBs)%POLY(ICELLs)%FACE(J)%NAdjCell 
            Z2       = ZERO                                                          
            IF(NADJ==0 .OR. ClosedSurface == 1)THEN
              !sliding rigid wall boundary condition
              UN1    = BRICK_LIST(NIN,IBs)%POLY(ICELLs)%FACE(J)%U_N
              !Pf     = P1
              Mf     = M1
              THETA  = MIN(ONE,Mf) !see dellacherie,omnes,raviart : fixing low mach issue                                                             
              Pf     = P1 + COEF2*THETA*HALF*Z1*UN1
              N(1:3) = BRICK_LIST(NIN,IBs)%N(J,1:3) 
              SURF   = BRICK_LIST(NIN,IBs)%POLY(ICELLs)%FACE(J)%Surf
              F0(1)  =  F0(1) -Pf*SURF*N(1)
              F0(2)  =  F0(2) -Pf*SURF*N(2)
              F0(3)  =  F0(3) -Pf*SURF*N(3)                                               
            ELSE
              N(1:3)          = BRICK_LIST(NIN,IBs)%N(J,1:3)                                                                                                                                      
              AREA            = BRICK_LIST(NIN,IBs)%POLY(ICELLs)%FACE(J)%Surf 
              IV              = BRICK_LIST(NIN,IBs)%Adjacent_Brick(J,1)
              IBV             = BRICK_LIST(NIN,IBs)%Adjacent_Brick(J,4)
              JV              = BRICK_LIST(NIN,IBs)%Adjacent_Brick(J,5)   
              UN1             = BRICK_LIST(NIN,IBs)%POLY(ICELLs)%FACE(J)%U_N  
              IF(AREA<=ZERO)CYCLE             
              DO IADJ=1,NADJ     
                ICELLv        = BRICK_LIST(NIN,IBs)%POLY(ICELLs)%FACE(J)%Adjacent_Cell(IADJ)
                IBM           = 0
                IF(IBv/=0)THEN
                  IBM         = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%WhereIsMain(4)
                  AREAv       = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%FACE(Jv)%Surf
                  SURF        = MIN(AREA,AREAv)
                  P2          = BRICK_LIST(NIN,IBm)%SIG(0)
                  Z2          = BRICK_LIST(NIN,IBm)%RHOC
                  UN2         = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%FACE(JV)%U_N
                  DENOM       = Z1+Z2
                  M2          = BRICK_LIST(NIN,IBm)%MACH
                  Mf          = MIN(M1,M2)
                  THETA       = MIN(ONE,Mf) !see dellacherie,omnes,raviart : fixing low mach issue                                                             
                  ! Pf          = COEF1*(HALF*(P1+P2)) + COEF2*((Z1*P2 + Z2*P1)/DENOM 
                  Pf          = COEF1*(HALF*(P1+P2)) + COEF2*((Z1*P2 + Z2*P1)/DENOM  +  THETA*Z1*Z2*(UN1+UN2)/DENOM)
                  IF(IBM == IB)CYCLE   ! inner face skipped                                                                             
                  F0(1)       = F0(1) - Pf*SURF*N(1) 
                  F0(2)       = F0(2) - Pf*SURF*N(2) 
                  F0(3)       = F0(3) - Pf*SURF*N(3)                       
                ELSE! THEN IBV==0
                  P2          = F_FACE(1  ,4  ,IV)
                  Z2          = F_FACE(1  ,3  ,IV)
                  UN2         = F_FACE(3  ,JV ,IV)
                  M2          = F_FACE(1  ,5  ,IV)
                  SURF        = AREA
                  DENOM       = Z1+Z2
                  !Pf          = COEF1*(HALF*(P1+P2)) + COEF2*((Z1*P2 + Z2*P1)/DENOM 
                  Mf          = MIN(M1,M2)
                  THETA       = MIN(ONE,Mf) !see dellacherie,omnes,raviart : fixing low mach issue                                                             
                  Pf          = COEF1*(HALF*(P1+P2)) + COEF2*((Z1*P2 + Z2*P1)/DENOM  +  THETA*Z1*Z2*(UN1+UN2)/DENOM)                
                  F0(1)       = F0(1) - Pf*SURF*N(1)
                  F0(2)       = F0(2) - Pf*SURF*N(2)                  
                  F0(3)       = F0(3) - Pf*SURF*N(3)     
                ENDIF
              ENDDO!next IADJ
            ENDIF  !(NADJ==0)  
          ENDDO!next J   
        ENDDO! next ISECND 
        

        FINT_CELL(1,II) = F0(1)
        FINT_CELL(2,II) = F0(2)
        FINT_CELL(3,II) = F0(3)
        
              !  write (*,FMT='(A,I6,A,3F30.16)') "22.brick ID=", ixs(11,II),"  Fint=", F0(1:3)


        !Fcell already contains gravity force, so do not erase but add               
        FCELL(1,II)       = FCELL(1,II) + F0(1) !+ FEXT_CELL(1,II)
        FCELL(2,II)       = FCELL(2,II) + F0(2) !+ FEXT_CELL(2,II)
        FCELL(3,II)       = FCELL(3,II) + F0(3) !+ FEXT_CELL(3,II)              

      ENDDO!next IB

      !-------------------------------------------------------------!
      ! Debug Output                                                !
      !-------------------------------------------------------------!
        if(IALEFVM_OUTP_FINT /= 0)then

          call my_barrier

          if(itask==0)then     
            print *, "    |---alefvm_sfint3_int22.F---|"
            print *, "    |      THREAD INFORMATION   |"
            print *, "    |---------------------------|" 
            print *, "     NCYCLE =", NCYCLE

            do i=1,NB
              ii       = brick_list(nin,i)%id
              MCELL    = BRICK_LIST(NIN,I)%MainID
              IF (MCELL==0)CYCLE               
              print *,                    "      brique_=", ixs(11,ii)
              write(*,FMT='(A34,6A26)')   "                                  ",    
     .                                  "#--- internal force -----#"  
     

              write (*,FMT='(A,8E26.14)' ) "         force-X           =", FCELL(1,II)          
              write (*,FMT='(A,8E26.14)' ) "         force-Y           =", FCELL(2,II)
              write (*,FMT='(A,8E26.14)' ) "         force-Z           =", FCELL(3,II)  
              write (*,FMT='(A,8E26.14)' ) "               P           =", BRICK_LIST(NIN,I)%SIG(0)  

                           
              print *, "      "          
            enddo
            
          endif!if(itask)

          call my_barrier

        endif!if(IALEFVM_OUTP_FINT /= 0)
      !-----------------------------------------!
        
      RETURN
      END
