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
Chd|  ALEFVM_STRESS_INT22           source/ale/alefvm/alefvm_stress_int22.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|        ALEFVM_MOD                    ../common_source/modules/alefvm_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE ALEFVM_STRESS_INT22 (
     1                          IXS,      NV46, SIG,  QVIS, 
     2                          N1X,      N2X,  N3X,  N4X,  N5X,  N6X,
     3                          N1Y,      N2Y,  N3Y,  N4Y,  N5Y,  N6Y,
     4                          N1Z,      N2Z,  N3Z,  N4Z,  N5Z,  N6Z,
     5                          X  ,      IPM,  RHO,  VOL,  A  ,  IAD22,
     6                          ELBUF_TAB,NEL,  MOM,  SSP,  ITASK)
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
      USE ALEFVM_MOD       !F_FACE
      USE ELBUFDEF_MOD     !TYPE(ELBUF_STRUCT_)
      USE I22BUFBRIC_MOD   !BRICK_LIST
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
C This subroutines computes internal forces for
C finite volume scheme (IALEFVM==1)
C
C If option is not detected in input file then
C subroutine is unplugged
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NEL, ITASK
      INTEGER :: IXS(NIXS,*),NV46,IPM(NPROPMI,*)
      my_real :: SIG(NEL,6),QVIS(NEL),X(3,*),RHO(NEL), VOL(NEL), A(3,*), IAD22(*)
      my_real :: N1X(*), N2X(*),  N3X(*), N4X(*), N5X(*), N6X(*),
     .           N1Y(*), N2Y(*),  N3Y(*), N4Y(*), N5Y(*), N6Y(*),
     .           N1Z(*), N2Z(*),  N3Z(*), N4Z(*), N5Z(*), N6Z(*),
     .           MOM(NEL,3), SSP(NEL), ISGN, V(3), W(3), VmW(3)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, II, J, K, IMAT, IALEFVM_FLG, IB, NIN, IBM, NGM, IDLOCM, ICUT,MCELL,NUM,IBv,ICELLv
      INTEGER :: ISECND, ID, NP, IBs, ICELLs, Js, NumSECND
      
      my_real :: P(MVSIZ) , 
     .           S1(MVSIZ)     , S2(MVSIZ)          , S3(MVSIZ), 
     .           S4(MVSIZ)     , S5(MVSIZ)          , S6(MVSIZ),
     .           PVOIS(6,MVSIZ), NORM    , S(0:6), S0(14)
      my_real
     .   N0X(14), N0Y(14), N0Z(14),
     .   NS(3,0:6), MASS,U_N(0:6), SURF(6)
     
      INTEGER :: idbf,idbl,ICELL,NBCUT,NCELL,NBCUTv,IDEB,IFIN
      LOGICAL :: debug_outp
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
      
      !inter22
      NIN = 1
      
      ! attention : il faut que mmain soit traite pour chacun des groupes 
      ! afin de connaitre les tenseurs voisins (SIG,SVIS,QVIS) !
      ! l'assemblage est ensuite realise apres tous les appel de 
      ! ALEMAIN>SFORC3() pour chaque groupe.

      !-------------------------------------------------------------!
      ! PRESSURE                                                    !
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
      ! STORAGE
      DO I=LFT,LLT                           
        IB = NINT(IAD22(I))                  
        IF(IB<=0) CYCLE 
        BRICK_LIST(NIN,IB)%RHOC   = RHO(I)*SSP(I)                   
        BRICK_LIST(NIN,IB)%SIG(0) = P(I)     
        BRICK_LIST(NIN,IB)%SIG(1) = S1(I)    
        BRICK_LIST(NIN,IB)%SIG(2) = S2(I)    
        BRICK_LIST(NIN,IB)%SIG(3) = S3(I)    
        BRICK_LIST(NIN,IB)%SIG(4) = S4(I)    
        BRICK_LIST(NIN,IB)%SIG(5) = S5(I)    
        BRICK_LIST(NIN,IB)%SIG(6) = S6(I)  
        BRICK_LIST(NIN,IB)%MACH   = SQRT(MOM(I,1)*MOM(I,1)+MOM(I,2)*MOM(I,2)+MOM(I,3)*MOM(I,3))/RHO(I)/VOL(I) / SSP(I)   
      ENDDO!next I                           
      
      !-------------------------------------------------------------!
      ! NORMAL VECTOR FOR ALE  (can be optimize for EULER           !
      !-------------------------------------------------------------!                                       
      DO I=LFT,LLT                          
      II      = I + NFT                     
      IB      = NINT(IAD22(I))              
      IF(IB<=0)CYCLE                      
        ! Face-1                            
        N1X(I) = BRICK_LIST(NIN,IB)%N(1,1)  
        N1Y(I) = BRICK_LIST(NIN,IB)%N(1,2)  
        N1Z(I) = BRICK_LIST(NIN,IB)%N(1,3)  
        ! Face-2                            
        N2X(I) = BRICK_LIST(NIN,IB)%N(2,1)  
        N2Y(I) = BRICK_LIST(NIN,IB)%N(2,2)  
        N2Z(I) = BRICK_LIST(NIN,IB)%N(2,3)  
        ! Face-3                            
        N3X(I) = BRICK_LIST(NIN,IB)%N(3,1)  
        N3Y(I) = BRICK_LIST(NIN,IB)%N(3,2)  
        N3Z(I) = BRICK_LIST(NIN,IB)%N(3,3)  
        ! Face-4                            
        N4X(I) = BRICK_LIST(NIN,IB)%N(4,1)  
        N4Y(I) = BRICK_LIST(NIN,IB)%N(4,2)  
        N4Z(I) = BRICK_LIST(NIN,IB)%N(4,3)  
        ! Face-5                            
        N5X(I) = BRICK_LIST(NIN,IB)%N(5,1)  
        N5Y(I) = BRICK_LIST(NIN,IB)%N(5,2)  
        N5Z(I) = BRICK_LIST(NIN,IB)%N(5,3)  
        ! Face-6                            
        N6X(I) = BRICK_LIST(NIN,IB)%N(6,1)  
        N6Y(I) = BRICK_LIST(NIN,IB)%N(6,2)  
        N6Z(I) = BRICK_LIST(NIN,IB)%N(6,3)  
      ENDDO                                 

      !-------------------------------------------------------------!
      ! DATA PRE-TREATMENT ON EACH FACE OF EACH POLYHEDRON          !
      ! COMPOSING THE SUPERCELL                                     !
      !   NEEDED FOR ALE_FVM_SFINT3_INT22 INTEGRAL : Pface interp.  !
      !-------------------------------------------------------------!
      DO I=LFT, LLT
        !WARNING : Normal vectors computed in EPXLE3() if IALEFVM>0
        II                                               = I + NFT
        IB                                               = NINT(IAD22(I))
        IF(IB<=0)CYCLE 
        NBCUT                                            = BRICK_LIST(NIN,IB)%NBCUT
        MCELL                                            = BRICK_LIST(NIN,IB)%MainID
        NCELL                                            = NBCUT
        ICELL                                            = MCELL
        BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%U_N     = ZERO
        !---face-0  
        IF(NBCUT>0)THEN
          S(1:6)                                         = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%Surf                                                            
          S(0)                                           = BRICK_LIST(NIN,IB)%PCUT(ICELL)%SCUT(1)                                                                   
          IBM                                            = BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(4)                                                          
          NGM                                            = BRICK_LIST(NIN,IBM)%NG                                                                                   
          IDLOCM                                         = BRICK_LIST(NIN,IBM)%IDLOC                                                                                                                                                                                                                              
          ISGN                                           = ONE                                                                                                                      
          IF(ICELL==9)ISGN = -ONE                                                                                                                                                  
          BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%U_N(1:9)  = ZERO                                                                                                                   
          DO K=1,NBCUT                                                                                                                                                           
            ICUT                                         = K                                                                                                                      
            S0(K)                                        = BRICK_LIST(NIN,IB)%PCUT(ICUT)%Scut(1)                                                                                  
            N0X(K)                                       = BRICK_LIST(NIN,IB)%PCUT(ICUT)%N(1)    !icell=9 : normal 0 are inversed                                                
            N0Y(K)                                       = BRICK_LIST(NIN,IB)%PCUT(ICUT)%N(2)                                                                                    
            N0Z(K)                                       = BRICK_LIST(NIN,IB)%PCUT(ICUT)%N(3)                                                                                    
            NORM                                         = SQRT(N0X(K)**2+N0Y(K)**2+N0Z(K)**2)                                                                                    
            N0X(K)                                       = N0X(K) / NORM                                                                                                          
            N0Y(K)                                       = N0Y(K) / NORM                                                                                                          
            N0Z(K)                                       = N0Z(K) / NORM                                                                                                          
            MASS                                         = RHO(I)*VOL(I) 
            V(1:3)                                       = MOM(I,1:3) / MASS  
            W(1:3)                                       = BRICK_LIST(NIN,IB)%PCUT(ICUT)%VEL(1:3)
            VmW(1)                                       = V(1)-W(1)
            VmW(2)                                       = V(2)-W(2)
            VmW(3)                                       = V(3)-W(3)                                                                                                       
            U_N(0)                                       = ISGN*(VmW(1)*N0x(K) + VmW(2)*N0Y(K) + VmW(3)*N0Z(K))                                                     
            BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%U_N(K)  =  U_N(0)                                                                                                                
          ENDDO                                                                                                                                                     
        ENDIF
        !---face-1:6                                                                 
        MASS                                             = RHO(I)*VOL(I)                                                     
        U_N(1)                                           = (MOM(I,1)*N1X(I) + MOM(I,2)*N1Y(I) + MOM(I,3)*N1Z(I)) / (MASS)    
        U_N(2)                                           = (MOM(I,1)*N2X(I) + MOM(I,2)*N2Y(I) + MOM(I,3)*N2Z(I)) / (MASS)    
        U_N(3)                                           = (MOM(I,1)*N3X(I) + MOM(I,2)*N3Y(I) + MOM(I,3)*N3Z(I)) / (MASS)    
        U_N(4)                                           = (MOM(I,1)*N4X(I) + MOM(I,2)*N4Y(I) + MOM(I,3)*N4Z(I)) / (MASS)    
        U_N(5)                                           = (MOM(I,1)*N5X(I) + MOM(I,2)*N5Y(I) + MOM(I,3)*N5Z(I)) / (MASS)    
        U_N(6)                                           = (MOM(I,1)*N6X(I) + MOM(I,2)*N6Y(I) + MOM(I,3)*N6Z(I)) / (MASS)    
        BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1)%U_N       = U_N(1)                                                            
        BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(2)%U_N       = U_N(2)                                                            
        BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(3)%U_N       = U_N(3)                                                            
        BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(4)%U_N       = U_N(4)                                                            
        BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(5)%U_N       = U_N(5)                                                            
        BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(6)%U_N       = U_N(6)                                                            
        
        NumSECND = BRICK_LIST(NIN,IB)%SecndList%Num    
        DO ISECND=1,NumSECND
          IBs                                            = BRICK_LIST(NIN,IB)%SecndList%IBV(ISecnd)                                
          ICELLs                                         = BRICK_LIST(NIN,IB)%SecndList%ICELLv(ISecnd)                             
          Js                                             = BRICK_LIST(NIN,IBs)%POLY(ICELLs)%WhereIsMain(1)                       
          NBCUTv                                         = BRICK_LIST(NIN,IBs)%NBCUT
          !---face-0  
          !forcemment NBCUTv > 0                                                                                                                                                                                                                                                                                                                               
          S(1:6)                                         = BRICK_LIST(NIN,IBs)%POLY(ICELLs)%FACE(1:6)%Surf                                                                                                
          S(0)                                           = BRICK_LIST(NIN,IBs)%PCUT(ICELLs)%SCUT(1)                                                                                                       
          IBM                                            = BRICK_LIST(NIN,IBs)%POLY(ICELLs)%WhereIsMain(4)                                                                                                                                                                                                                                                                                                                                                                   
          ISGN                                           = ONE    
          IDEB                                           = ICELLs
          IFIN                                           = ICELLs                                                                                                                                         
          IF(ICELLs==9)THEN
            ISGN                                         = -ONE                                                                                                                                                         
            IDEB                                         = 1
            IFIN                                         = NBCUTv
          ENDIF
          BRICK_LIST(NIN,IBs)%POLY(ICELLs)%FACE0%U_N(1:9)= ZERO  
          MASS                                           = RHO(I)*VOL(I)           
          DO K=IDEB,IFIN                                                                                                                                         
          ICUT                                           = K                                                                                                                                          
          S0(K)                                          = BRICK_LIST(NIN,IBs)%PCUT(ICUT)%Scut(1)                                                                                                      
          N0X(K)                                         = BRICK_LIST(NIN,IBs)%PCUT(ICUT)%N(1)    !icell=9 : normal 0 are inversed                                                                    
          N0Y(K)                                         = BRICK_LIST(NIN,IBs)%PCUT(ICUT)%N(2)                                                                                                        
          N0Z(K)                                         = BRICK_LIST(NIN,IBs)%PCUT(ICUT)%N(3)                                                                                                        
          NORM                                           = SQRT(N0X(K)**2+N0Y(K)**2+N0Z(K)**2)                                                                                                        
          N0X(K)                                         = N0X(K) / NORM                                                                                                                              
          N0Y(K)                                         = N0Y(K) / NORM                                                                                                                              
          N0Z(K)                                         = N0Z(K) / NORM                                                                                                                                                                                                                                                          
          V(1:3)                                         = MOM(I,1:3) / MASS  
          W(1:3)                                         = BRICK_LIST(NIN,IBs)%PCUT(ICUT)%VEL(1:3)
          VmW(1)                                         = V(1)-W(1)
          VmW(2)                                         = V(2)-W(2)
          VmW(3)                                         = V(3)-W(3)                                                                                                       
          U_N(0)                                         = ISGN*(VmW(1)*N0x(K) + VmW(2)*N0Y(K) + VmW(3)*N0Z(K))                                                     
          BRICK_LIST(NIN,IBs)%POLY(ICELLs)%FACE0%U_N(K)  =  U_N(0)   
          ENDDO!next K                                                                                                                                                                                                                                                                                                                          
          !---face-1:6                                                                  
          U_N(1)                                         = (MOM(I,1)*N1X(I) + MOM(I,2)*N1Y(I) + MOM(I,3)*N1Z(I)) / MASS    
          U_N(2)                                         = (MOM(I,1)*N2X(I) + MOM(I,2)*N2Y(I) + MOM(I,3)*N2Z(I)) / MASS    
          U_N(3)                                         = (MOM(I,1)*N3X(I) + MOM(I,2)*N3Y(I) + MOM(I,3)*N3Z(I)) / MASS    
          U_N(4)                                         = (MOM(I,1)*N4X(I) + MOM(I,2)*N4Y(I) + MOM(I,3)*N4Z(I)) / MASS    
          U_N(5)                                         = (MOM(I,1)*N5X(I) + MOM(I,2)*N5Y(I) + MOM(I,3)*N5Z(I)) / MASS    
          U_N(6)                                         = (MOM(I,1)*N6X(I) + MOM(I,2)*N6Y(I) + MOM(I,3)*N6Z(I)) / MASS    
          BRICK_LIST(NIN,IBs)%POLY(ICELLs)%FACE(1)%U_N   = U_N(1)                                                            
          BRICK_LIST(NIN,IBs)%POLY(ICELLs)%FACE(2)%U_N   = U_N(2)                                                            
          BRICK_LIST(NIN,IBs)%POLY(ICELLs)%FACE(3)%U_N   = U_N(3)                                                            
          BRICK_LIST(NIN,IBs)%POLY(ICELLs)%FACE(4)%U_N   = U_N(4)                                                            
          BRICK_LIST(NIN,IBs)%POLY(ICELLs)%FACE(5)%U_N   = U_N(5)                                                            
          BRICK_LIST(NIN,IBs)%POLY(ICELLs)%FACE(6)%U_N   = U_N(6) 
        ENDDO !next ISECND       
      ENDDO!next I
      

 
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
          print *, "    |--alefvm_stress_int22.F---|"
          print *, "    |    THREAD INFORMATION    |"
          print *, "    |--------------------------|" 
          print *, "     NCYCLE =", NCYCLE
          do i=idbf,idbl
            ii = nft + i
            IB               = NINT(IAD22(I))
            if(ib<=0)CYCLE
            NBCUT            = BRICK_LIST(NIN,IB)%NBCUT
            IF(NBCUT>0)THEN
             ICELL                 =  BRICK_LIST(NIN,IB)%mainID
             IF(ICELL==0)CYCLE                        
                        
               print *,                    "      brique=", ixs(11,nft+i), "icell=", ICELL
               print *,                    "        vold=", brick_list(nin,ib)%vold_scell
               print *,                    "        vnew=", brick_list(nin,ib)%vnew_scell
               write(*,FMT='(A24,1A26)')   "                        ",                     
     .                                    "#-stress Tensor (P+VIS+Q)#"          

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
               write(*,FMT='(A34,8A26)')      "                                  ",    
     .                                        "#-------- face_1 ---------",
     .                                        "#-------- face_2 ---------","#-------- face_3 ---------",
     .                                        "#-------- face_4 ---------","#-------- face_5 ---------",  
     .                                        "#-------- face_6 --------#"      
               write (*,FMT='(A,8E26.14)')    "    <U,N>                  =", BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%U_N          
               write (*,FMT='(A,9E26.14)')    "    U0N0                   =", BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%U_N(1:9)
            ELSE
             print *,                    "      brique=", ixs(11,nft+i), "uncut"
             print *,                    "        vold=", brick_list(nin,ib)%vold_scell
             print *,                    "        vnew=", brick_list(nin,ib)%vnew_scell             
             write(*,FMT='(A24,1A26)')   "                        ",                     
     .                                   "#-stress Tensor (P+VIS+Q)#"          

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
             write(*,FMT='(A34,6A26)')      "                                  ",    
     .                                      "#-------- face_1 ---------","#-------- face_2 ---------",
     .                                      "#-------- face_3 ---------","#-------- face_4 ---------",
     .                                      "#-------- face_5 ---------","#-------- face_6 --------#"  
               write (*,FMT='(A,1E26.14)')    "    <U,N>                  =", BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%U_N          
            ENDIF!(NBCUT>0)   
             
             NUM = BRICK_LIST(NIN,IB)%SecndList%Num
             IBM = IB
             DO ISECND=1,NUM
               IB                                         = BRICK_LIST(NIN,IBM)%SecndList%IBV(ISECND)              
               ICELL                                      = BRICK_LIST(NIN,IBM)%SecndList%ICELLv(ISECND) 
               id                                         = BRICK_LIST(NIN,IB)%ID
               print *,                    "      secnd=", ixs(11,id), "icell=", ICELL
               write(*,FMT='(A24,1A26)')   "                        ",                     
     .                                    "#-stress Tensor (P+VIS+Q)#"          

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
               write(*,FMT='(A34,8A26)')      "                                  ",    
     .                                        "#-------- face_1 ---------",
     .                                        "#-------- face_2 ---------","#-------- face_3 ---------",
     .                                        "#-------- face_4 ---------","#-------- face_5 ---------",  
     .                                        "#-------- face_6 --------#"      
               write (*,FMT='(A,8E26.14)')    "    <U,N>                  =", BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%U_N          
               write (*,FMT='(A,9E26.14)')    "    U0N0                   =", BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%U_N(1:9)
             ENDDO !next ICELL                    
            

            
                         
                                 

            print *, "      "          
          enddo!next i
!#!include "lockoff.inc"       
        endif
        endif
      !-----------------------------------------!
        
      RETURN
      END
