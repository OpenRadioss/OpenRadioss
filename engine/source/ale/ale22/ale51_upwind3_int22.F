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
Chd|  ALE51_UPWIND3_INT22           source/ale/ale22/ale51_upwind3_int22.F
Chd|-- called by -----------
Chd|        AFLUXT                        source/ale/ale51/afluxt.F     
Chd|        ALE51_FINISH                  source/ale/ale51/ale51_finish.F
Chd|        ALE51_INIT                    source/ale/ale51/ale51_init.F 
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE ALE51_UPWIND3_INT22(
     .                         PM     , IXS  , ITRIMAT,        IFLG ,
     .                         NV46   , IPARG, ELBUF_TAB, ITASK  )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C  This subroutines computes from direct fluxes :
C  -1- FLUXES
C  -2- QMV(7:12)            : QMV*DT = OUTGOING VOLUME  (DT*FLUX(i,i) is INCOMING ONE), FLU1 IS SUM OF QMV
C  -3- DDVOL                : D/DV . D/DT . VOL = DV/DT
C
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE I22BUFBRIC_MOD
      USE I22TRI_MOD 
      USE ELBUFDEF_MOD  
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
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "inter22.inc"
#include      "i22edges.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: ITRIMAT,IFLG
      INTEGER IXS(NIXS,*), NV46,IPARG(NPARG,*),ITASK
      my_real ::
     .   PM(NPROPM,*),DDVOLi(MVSIZ)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER                             :: J,K,ISILENT, MLW, MAT
      my_real                             :: REDUC
      INTEGER                             :: NBF,NBL, MCELL,ICELLv
      INTEGER                             :: NUM, NADJ, IADJ, JV, NG
      
      INTEGER                             :: IB,IBv, NIN, ICELL,NCELL,IDLOC
      INTEGER                             :: IE,IDV,ADD, IE_M
      my_real                             :: cellFLUX(6,9,NB),UPWL(6) 
      
      my_real, DIMENSION(:), POINTER      ::  UVAR,pDDVOL           
      my_real                             :: DDVOL
      
      LOGICAL :: debug_outp      
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------
      IF(TRIMAT==0)RETURN
      IF(INT22==0)RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      !=================
      ! INITIALIZATIONS 
      !=================
      NIN = 1
      NBF = 1+ITASK*NB/NTHREAD
      NBL = (ITASK+1)*NB/NTHREAD
      NBL = MIN(NBL,NB)


      !INTERFACE 22 ONLY - OUTPUT---------------!
      debug_outp = .false.
      if(ibug22_upwind.ne.0)then
        if(ibug22_upwind.GT.0)then
          do ib=nbf,nbl
            ie  = brick_list(nin,ib)%id
            mlw = brick_list(nin,ib)%mlw
            if(ixs(11,ie).eq.ibug22_upwind)debug_outp=.true.
            if(mlw.ne.51)debug_outp=.false.
          enddo
        elseif(ibug22_upwind==-1)then
          debug_outp = .true.
        endif
        if(((itrimat.ne.ibug22_itrimat).and.(ibug22_itrimat.ne.-1)))debug_outp=.false.
      endif
      if(debug_outp)then
             print *, "    |----ale51_upwind3_int22.F-----|"
             print *, "    |      THREAD INFORMATION      |"
             print *, "    |------------------------------|" 
             print *, "    NCYCLE =", NCYCLE
             print *, "    ITRIMAT=", ITRIMAT                     
      endif
       
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
        IDLOC             =  BRICK_LIST(NIN,IB)%IDLOC
        IF(MLW.NE.51)CYCLE
        DO WHILE (ICELL.LE.NCELL) ! loop on polyhedron {1:NCELL} U {9}
          ICELL = ICELL +1
          IF (ICELL>NCELL .AND. NCELL.NE.0)ICELL=9 
          !======================================================!
          !  MULTIMATERIAL UPWIND TREATMENT                      !          
          !======================================================!          
          IE_M      = BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(3)
          MAT       = IXS(1,IE_M)               
          UPWL(1:6) = PM(16,MAT)
          REDUC     = PM(92,MAT)
          DDVOL     = ZERO
          BRICK_LIST(NIN,IB)%POLY(ICELL)%Adjacent_FLU1 = ZERO
          DO J=1,6
            NADJ = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%NAdjCell          
            DO IADJ = 1,NADJ
              IDV    = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,1)
              IBv    = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,4)
              Jv     = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,5)              
              ICELLv = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Adjacent_Cell(IADJ)
              cellFLUX(J,ICELL,IB) = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Adjacent_UpwFLUX(IADJ)
              IF(IDV.EQ.0)THEN
               cellFLUX(J,ICELL,IB)=cellFLUX(J,ICELL,IB)*REDUC
              ELSEIF(IDV.GT.0)THEN
               NG      = BRICK_LIST(NIN,IB)%NG
               ISILENT = IPARG(64,NG)
               IF(ISILENT==1)THEN
                 UPWL(J)=UN
                 cellFLUX(J,ICELL,IB)=cellFLUX(J,ICELL,IB)*PM(92,IXS(1,IDV))
               ENDIF
              ENDIF 
              BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Adjacent_upwFLUX(IADJ) = cellFLUX(J,ICELL,IB)-UPWL(J)*ABS(cellFLUX(J,ICELL,IB))
              BRICK_LIST(NIN,IB)%POLY(ICELL)%Adjacent_FLU1 = 
     .        BRICK_LIST(NIN,IB)%POLY(ICELL)%Adjacent_FLU1 + cellFLUX(J,ICELL,IB)+UPWL(J)*ABS(cellFLUX(J,ICELL,IB))
              IF(IFLG.EQ.10)THEN
                DDVOL = DDVOL + cellFLUX(J,ICELL,IB)
C                DDVOL = DDVOL + cellFLUX(J,ICELL,IB)+UPWL(J)*ABS(cellFLUX(J,ICELL,IB))                
C                DDVOL = DDVOL + cellFLUX(J,ICELL,IB)-UPWL(J)*ABS(cellFLUX(J,ICELL,IB))   
                !DDVOL*DT IS SUM FOR INCOMING AND OUTCOMING VOLUMES. 2 * Sum(Nadj(j),j=1..6)             
              ENDIF 
            ENDDO!next IADJ
          ENDDO!next J
         
          BRICK_LIST(NIN,IB)%POLY(ICELL)%DDVOL_upw = DDVOL  !UNDEMI*DDVOL

           !INTERFACE 22 ONLY - OUTPUT---------------!
!#!include "lockon.inc"      
           if(debug_outp)then
            if(ibug22_upwind.eq.ixs(11,ie) .OR. ibug22_upwind.eq.-1)then          
             print *,                    "      brique         =", ixs(11,ie)
             print *,                    "      icell          =", ICELL 
            write (*,FMT='(A,1E26.14)') "       Flu1           =", BRICK_LIST(NIN,IB)%POLY(ICELL)%Adjacent_FLU1             
             DO J=1,6
               NADJ = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%NAdjCell          
               DO IADJ = 1,NADJ                        
                 print *,                "      FACE           =", J
            write (*,FMT='(A,6E26.14)') "       Flux(IAD:NADJ) =", BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(J)%Adjacent_upwFLUX(IADJ)
               ENDDO
             ENDDO
             print *, "      ------------------------" 
!#!include "lockoff.inc"       
           endif
           endif
           
           
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
      NIN = 1													       
      DO IB=NBF,NBL												             
     	NG    = BRICK_LIST(NIN,IB)%NG										       
     	IE    = BRICK_LIST(NIN,IB)%ID										       
     	IDLOC = BRICK_LIST(NIN,IB)%IDLOC									       
     	MLW   =  BRICK_LIST(NIN,IB)%MLW 									       
     	NUM   = BRICK_LIST(NIN,IB)%SecndList%Num								       
     	MCELL = BRICK_LIST(NIN,IB)%mainID									       
     	IF(MLW.NE.51)CYCLE											       
     	DDVOL = ZERO												       
     	DO K=1,NUM												       
     	  IBV	 = BRICK_LIST(NIN,IB)%SecndList%IBV(K)  							       
     	  ICELLv = BRICK_LIST(NIN,IB)%SecndList%ICELLv(K)							       
     	  DDVOL  = DDVOL + BRICK_LIST(NIN,IBv)%POLY(ICELLv)%DDVOL_upw						       
     	ENDDO													       
     	DDVOL = DDVOL + BRICK_LIST(NIN,IB)%POLY(MCELL)%DDVOL_upw						       
     	!updating law51 material buffer with computed stacked value						       
     	IF(ITRIMAT.GT.0)THEN											       
     	  LFT	 =  1												       
     	  LLT	 =  IPARG(2,NG) 										       
     	  UVAR   => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR							       
     	  ADD	 = (N0PHAS + (ITRIMAT-1)*NVPHAS+12)*LLT 							       
     	  pDDVOL => UVAR(ADD+1:ADD+LLT) 									       
     	  pDDVOL(IDLOC) =  DDVOL   !*UNDEMI  : deja applique facteur 1/2					       
     	  if(ixs(11,IE)==26354)then										       
     	    print *, "itrimat, ddvoli", itrimat, DDVOL  							       
     	  endif 												       
     	ELSE													       
     	  BRICK_LIST(NIN,IB)%POLY(MCELL)%DDVOL_upw = DDVOL							       
     	ENDIF													       
      ENDDO!next IB												       
      

      RETURN
      END
C
