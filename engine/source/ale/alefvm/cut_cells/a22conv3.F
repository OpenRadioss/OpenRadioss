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
Chd|  A22CONV3                      source/ale/alefvm/cut_cells/a22conv3.F
Chd|-- called by -----------
Chd|        ACONVE                        source/ale/aconve.F           
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        ALEFVM_MOD                    ../common_source/modules/ale/alefvm_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE A22CONV3(            PHI  ,  FLUX ,   FLU1,
     .                    IOFF ,  QMV  ,   IFLG,
     .                    ITRIMAT   , NVAR ,  ITASK,
     .                    ELBUF_TAB , IXS  ,  IPARG)
C-----------------------------------------------
C   D e s c r i p  t i o n
C-----------------------------------------------
C 'alefvm' is related to a collocated scheme (built from FVM and based on Godunov scheme)
C  which was temporarily introduced for experimental option /INTER/TYPE22 (FSI coupling with cut cell method)
C This cut cell method is not completed, abandoned, and is not an official option.
C There is no other use for this scheme which is automatically enabled when /INTER/TYPE22 is defined (INT22>0 => IALEFVM=1).
C
C  This subroutine is handling transportation with 
C  polyhedra from cut cells
C  In cut cell buffer :
C    %PHI is the physical value
C    %dPHI is the transported quantity : can be negative for small SECONDARY cells
C  Stability of small cell issue is handled by stacking %dPHI using MAIN cell
C  and its linked SECONDARY cells.
C
C   %UpwFLux(6,9,5) : flux on polyhedra full face
C   %Adjacent_upwFLUX : list of flux on a given polyhedra face. To be used for transportation because may be not conform
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE I22TRI_MOD
      USE ELBUFDEF_MOD
      USE I22BUFBRIC_MOD
      USE ALEFVM_MOD , only:ALEFVM_Param
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IOFF, IFLG,ITASK,NVAR, IXS(NIXS,*),IPARG(NPARG,*)
      my_real
     .    PHI(*), FLUX(6,*), FLU1(*), QMV(12,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB     
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
#include      "inter22.inc"
#include      "param_c.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IE, IV,J,ITRIMAT,NIN,NBF,NBL,IB,IADJ,IFACE, NG,IDLOC,
     . IBV,JV,ICELL,ICELLv,NCELL,NUM, MCELL,MLW, NADJ, LLT_
      my_real
     .   VALVOIS,VALEL,VL, dPHI
      INTEGER               , POINTER      :: pNum     
      TYPE(L_BUFEL_)        , POINTER      :: LBUF     
      TYPE(BUF_MAT_)        , POINTER      :: MBUF 
      my_real,  DIMENSION(:), POINTER      :: VAR, pRHO , pVOL , pEINT ,pIAD22
      INTEGER                              :: ADD, ADD0 ,K    
      INTEGER,DIMENSION(:,:),  POINTER     :: pAdjBRICK                  
      LOGICAL :: debug_outp              
      my_real, TARGET                      :: tmpVAR(MVSIZ)      
C-----------------------------------------------

      !---------------------------------------------------------!
      ! INITIALIZATION                                          !
      !---------------------------------------------------------!
      NIN     = 1
      NBF     = 1+ITASK*NB/NTHREAD
      NBL     = (ITASK+1)*NB/NTHREAD
      NBL     = MIN(NBL,NB)
      
      !---------------------------------------------------------!
      ! SECONDARY CELLS : GET MATERIAL BUFFER VALUE  (%PHI)         !
      !---------------------------------------------------------!
      ! ALREADY DONE IN ACONVE()  

      !---------------------------------------------------------!
      ! DEBUG OUTPUT                                            !
      !---------------------------------------------------------!
      !INTERFACE 22 ONLY - OUTPUT---------------!
       debug_outp = .false.
       if(ibug22_convec/=0)then
         debug_outp = .false.
         if(ibug22_convec>0)then
           do ib=nbf,nbl
           ie=brick_list(nin,ib)%id
             if(ixs(11,ie)==ibug22_convec)then
               debug_outp=.true.
               exit
             endif
           enddo
         elseif(ibug22_convec==-1)then
           debug_outp = .true.
         endif
         if(((itrimat>0) .and. (ibug22_itrimat/=trimat)))debug_outp=.false.
         if(((itrimat>0) .and. (ibug22_itrimat==-1)))debug_outp=.true.
       endif



      !---------------------------------------------------------!
      ! CELL TRANSPORTATION (CUT CELL BUFFER)                   !
      !---------------------------------------------------------!
      DO IB=NBF,NBL
        IE                =  BRICK_LIST(NIN,IB)%ID
        VL                =  ZERO  
        NCELL             =  BRICK_LIST(NIN,IB)%NBCUT      
        ICELL             =  0 
        dPHI              =  ZERO 
        MLW               = BRICK_LIST(NIN,IB)%MLW
        IF(ITRIMAT/=0 .AND. MLW/=51)CYCLE
        DO WHILE (ICELL<=NCELL) ! loop on polyhedron {1:NCELL} U {9}
          ICELL = ICELL +1
          IF (ICELL>NCELL .AND. NCELL/=0)ICELL=9 
          BRICK_LIST(NIN,IB)%POLY(ICELL)%dPHI = ZERO !init
          pAdjBRICK => BRICK_LIST(NIN,IB)%Adjacent_Brick(1:6,1:5)          
          DO J=1,6
            NADJ = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%NAdjCell
            DO IADJ=1,NADJ   !plusieurs voisins possible par face.                                                                     
              IV     = pAdjBRICK(J,1)                                                          
              IBV    = pAdjBRICK(J,4)                                                          
              JV     = pAdjBRICK(J,5)                                                           
              ICELLv = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Adjacent_Cell(IADJ)                                                     
              IF(IV>0)THEN                                                                   
                IF(IBv==0)THEN                                                                  
                  VALVOIS      = PHI(IV)
                ELSE 
                  !IBv>0                                                                           
                  VALVOIS = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%PHI                                    
                ENDIF                                                                           
              ELSEIF(IV==0)THEN                                                               
                VALVOIS = PHI(IE)                                                               
              !ELSE                                                                             
              !  VALVOIS = PHI(-IV+IOFF)                                                        
              ENDIF  
!              dPHI = dPHI + (VALVOIS * BRICK_LIST(NIN,IB)%upwFLUX(J,ICELL))       
              dPHI = dPHI + (VALVOIS * BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Adjacent_upwFLUX(IADJ))
            ENDDO!next IADJ 
          ENDDO!next J      
          VALEL = BRICK_LIST(NIN,IB)%POLY(ICELL)%PHI
!          dPHI = dPHI + VALEL* BRICK_LIST(NIN,IB)%upwFLU1(ICELL)
          dPHI = dPHI + VALEL* BRICK_LIST(NIN,IB)%POLY(ICELL)%Adjacent_FLU1
          dPHI = -HALF * DT1 * dPHI
          BRICK_LIST(NIN,IB)%POLY(ICELL)%dPHI = dPHI 
          dPHI = ZERO             
        ENDDO!next ICELL
      ENDDO

      !-------------!
      CALL MY_BARRIER
      !-------------!      

      !---------------------------------------------------------!
      ! SECONDARY CELLS STACK                                       !
      !---------------------------------------------------------!
      !STACK SECONDARY cells values from ones connected to current main cell  
      IF(INT22>0)THEN                                                       
        NIN = 1                                                             
        DO IB=NBF,NBL                                                                                                        
          NUM   = BRICK_LIST(NIN,IB)%SecndList%Num                          
          MCELL = BRICK_LIST(NIN,IB)%mainID      
          dPHI  = ZERO        
          MLW   = BRICK_LIST(NIN,IB)%MLW
          IF(ITRIMAT/=0 .AND. MLW/=51)CYCLE                
          DO K=1,NUM                                                        
            IBV    = BRICK_LIST(NIN,IB)%SecndList%IBV(K)                    
            ICELLv = BRICK_LIST(NIN,IB)%SecndList%ICELLv(K)                 
            dPHI   = dPHI + BRICK_LIST(NIN,IBv)%POLY(ICELLv)%dPHI != PHI(J)                        
          ENDDO                                                             
          dPHI = dPHI + BRICK_LIST(NIN,IB)%POLY(MCELL)%dPHI
          BRICK_LIST(NIN,IB)%POLY(MCELL)%dPHI = dPHI                        
        ENDDO!next IB                                                        
      ENDIF                                                                 

      !---------------------------------------------------------!
      ! MAIN CELL CONVECTION                                  !
      !---------------------------------------------------------!
      DO IB=NBF,NBL
        IE    =  BRICK_LIST(NIN,IB)%ID
        MLW   =  BRICK_LIST(NIN,IB)%MLW
        MCELL =  BRICK_LIST(NIN,IB)%mainID
        dPHI   = BRICK_LIST(NIN,IB)%POLY(MCELL)%dPHI
        NG    =  BRICK_LIST(NIN,IB)%NG
        IDLOC =  BRICK_LIST(NIN,IB)%IDLOC       
        LBUF  => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
        MBUF  => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
        LLT_  =  IPARG(2,NG)
        MLW   = BRICK_LIST(NIN,IB)%MLW
        IF(ITRIMAT/=0 .AND. MLW/=51)CYCLE

        !----------------------------!
        !        N V A R = 1         !
        !----------------------------!
        IF (NVAR == 1) THEN
           IF(ITRIMAT==0 .OR. MLW/=51)THEN
             pRHO => LBUF%RHO(1:LLT_) 
           ELSE
             !USE PHASIS DATA
             ADD0 =  N0PHAS + (ITRIMAT-1)*NVPHAS                
             ADD  =  ADD0 + 9                                   ! ADD+9 => RHO
             K    =  LLT_*(ADD-1)                                ! UVAR(I,ADD) = UVAR(K+I)
             pRHO => MBUF%VAR(K+1:K+LLT_)
           END IF
          VAR => pRHO
        !----------------------------!
        !        N V A R = 2         !
        !----------------------------!
        ELSEIF (NVAR == 2) THEN
           IF(ITRIMAT==0 .OR. MLW/=51)THEN
             pEINT=> LBUF%EINT(1:LLT_) 
           ELSE
             !USE PHASIS DATA
             ADD0 =  N0PHAS + (ITRIMAT-1)*NVPHAS                
             ADD  =  ADD0 + 8                                   ! ADD+9 => RHO
             K    =  LLT_*(ADD-1)                                ! UVAR(I,ADD) = UVAR(K+I)
             pEINT => MBUF%VAR(K+1:K+LLT_)
           END IF
          VAR => pEINT
        !----------------------------!
        !        N V A R = 3         !
        !----------------------------!
        ELSEIF (NVAR == 3) THEN
          VAR => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)%RK(1:LLT_)
        !----------------------------!
        !        N V A R = 4         !
        !----------------------------!
        ELSEIF (NVAR == 4) THEN
          VAR => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)%RE(1:LLT_)
        !----------------------------!
        !        N V A R = 5         !
        !----------------------------!
        ELSEIF (NVAR == 5) THEN
          VAR => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(1:LLT_)
        !----------------------------!
        !        N V A R = 6         !
        !----------------------------!
        ELSEIF (NVAR == 6) THEN
          IF(ALEFVM_Param%IEnabled>0)THEN
              VAR => ELBUF_TAB(NG)%GBUF%MOM( 1 : LLT_ )       
          ENDIF
        !----------------------------!
        !        N V A R = 7         !
        !----------------------------!
        ELSEIF (NVAR == 7) THEN
          IF(ALEFVM_Param%IEnabled>0)THEN
              VAR => ELBUF_TAB(NG)%GBUF%MOM( LLT_*1+1 : LLT_*1+LLT_ )       
          ENDIF
        !----------------------------!
        !        N V A R = 8         !
        !----------------------------!
        ELSEIF (NVAR == 8) THEN
          IF(ALEFVM_Param%IEnabled>0)THEN
              VAR => ELBUF_TAB(NG)%GBUF%MOM( LLT_*2+1 : LLT_*2+LLT_ )         
          ENDIF
        !----------------------------!
        !        N V A R = 9         !
        !----------------------------!
        ELSEIF (NVAR == 9) THEN
          !
        ENDIF
        !----------------------------!
        !   TRANSPORTS CONVECTIFS    !
        !----------------------------!
         IF(MLW/=51.AND.ITRIMAT>0)THEN !si law51 dans jdd TRIMAT=4 
           CYCLE
         ELSE
           VAR(IDLOC) = VAR(IDLOC) + dPHI   !transport convectif des voisins additionels

             
         ENDIF         
      
      ENDDO!next IB



           !INTERFACE 22 ONLY------------------------!
           
            !INTERFACE 22 ONLY------------------------!
            if(debug_outp .AND. nvar==IBUG22_NVAR)then
              call my_barrier
              if(itask==0)then
                print *, "    |--------a22conv3.F--------|"
                print *, "    |     THREAD INFORMATION   |"
                print *, "    |--------------------------|" 
                print *, "    NCYCLE  =", NCYCLE      
                print *, "    ITRIMAT =", ITRIMAT         
                do ib=1,nb   
                  IE     =  BRICK_LIST(NIN,IB)%ID
                  MLW    =  BRICK_LIST(NIN,IB)%MLW
                  MCELL  =  BRICK_LIST(NIN,IB)%mainID
                  dPHI   =  BRICK_LIST(NIN,IB)%POLY(MCELL)%dPHI
                  NG     =  BRICK_LIST(NIN,IB)%NG
                  IDLOC  =  BRICK_LIST(NIN,IB)%IDLOC      
                  LBUF   => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
                  MBUF   => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)
                  LLT_   =  IPARG(2,NG)   
                  if(itrimat>0 .and. mlw/=51)cycle  
                  ie     = brick_list(nin,ib)%id  
                  IF(ITRIMAT==0)THEN
                    pRHO => LBUF%RHO(1:LLT_) 
                  ELSE
                    !USE PHASIS DATA
                    ADD0 =  N0PHAS + (ITRIMAT-1)*NVPHAS                
                    ADD  =  ADD0 + 9                                   ! ADD+9 => RHO
                    K    =  LLT_*(ADD-1)                                ! UVAR(I,ADD) = UVAR(K+I)
                    pRHO => MBUF%VAR(K+1:K+LLT_)
                  END IF
                  if(ibug22_convec > 0 .and. brick_list(nin,ib)%id==ibug22_convec )cycle
                  if(nvar==1)then
                  VAR => pRHO           
                  else
                  VAR => pEINT
                  endif    
                  print *, "      brique=", ixs(11,ie)                 
                  print *, "        NVAR=", NVAR                        
                  print *, "        dval=", dPHI                       
                  print *, "         was:", VAR(IDLOC)-dPHI            
                  print *, "          is:", VAR(IDLOC)  
                  print *, "         MLW:", MLW            
                  print *, "      ------------------------"               
                enddo          
              endif
            endif

            !-----------------------------------------!            


      !----------------------------!
      !   MOMENTUM DATA            !
      !----------------------------!      
      IF(TRIMAT>0.AND.IFLG==1)THEN
      !A TRAITER
          !QMV(6,I) = QMV(6,I) - VL(6,I) - VALEL(I)*QMV(12,I)  
      ENDIF
C-----------
      RETURN
      END
C
