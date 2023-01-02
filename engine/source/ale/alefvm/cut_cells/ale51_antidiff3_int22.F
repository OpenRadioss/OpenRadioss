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
Chd|  ALE51_ANTIDIFF3_INT22         source/ale/alefvm/cut_cells/ale51_antidiff3_int22.F
Chd|-- called by -----------
Chd|        AFLUXT                        source/ale/ale51/afluxt.F     
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE ALE51_ANTIDIFF3_INT22(FLUX   , ITRIMAT, IXS      , FLUX_VOIS,
     .                                 N4_VOIS, ITAB , NV46   , ELBUF_TAB, IPARG    ,
     .                                 ITASK  , VFRAC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD 
      USE I22BUFBRIC_MOD 
      USE I22TRI_MOD      
C-----------------------------------------------
C   D e c r i p t i o n
C-----------------------------------------------
C 'alefvm' is related to a collocated scheme (built from FVM and based on Godunov scheme)
C  which was temporarily introduced for experimental option /INTER/TYPE22 (FSI coupling with cut cell method)
C This cut cell method is not completed, abandoned, and is not an official option.
C There is no other use for this scheme which is automatically enabled when /INTER/TYPE22 is defined (INT22>0 => IALEFVM=1).
C
c        Same as ALE51_ANTIDIFF3 but for cut cells 
C                                        (inter22)
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
#include      "spmd_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com07_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "inter22.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),N4_VOIS(NUMELS+NSVOIS,8),ITAB(*),NV46,ITRIMAT,ITASK
      INTEGER IPARG(NPARG,*)      
      my_real
     .    FLUX(NV46,*),FLUX_VOIS(NUMELS+NSVOIS,NV46),VFRAC(*)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,KK,J1,J2,J
      my_real
     .   VOL0,AV0,UAV0,ALPHI,UALPHI,AAA,FF(NV46,5),UDT,PHI0   !FF(NV46,5=NADJ_MAX)
      INTEGER :: IE, MLW, IADJv, NADJv, IB, NBF, NBL, ICELL,ICELLM, MCELL, IE_M, IBM,NG,IDLOC,NADJ,IADJ
      INTEGER :: NIN,NCELL,IBV,IFV,ICELLv, IEV
      my_real :: VOLG, ALPH, ALPHv(6,5), tmpFLUX(NV46,5) !5=NAdj_max
      my_real :: debug_tmp
      LOGICAL :: debug_outp      
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------
      IF(TRIMAT==0)RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      IF(DT1>ZERO)THEN
        UDT = ONE/DT1
      ELSE
        UDT = ZERO
      ENDIF

      NIN = 1
      NBF = 1+ITASK*NB/NTHREAD
      NBL = (ITASK+1)*NB/NTHREAD
      NBL = MIN(NBL,NB)


      !INTERFACE 22 ONLY - OUTPUT---------------!
      debug_outp = .false.
      if(ibug22_antidiff/=0)then
        debug_outp = .false.
        if(ibug22_antidiff>0)then
          do ib=nbf,nbl
             ie = brick_list(nin,ib)%id
            if(ixs(11,ie)==ibug22_antidiff)then
              MLW =  BRICK_LIST(NIN,IB)%MLW
              if(mlw==51)then
                debug_outp=.true.
              endif
            endif
          enddo
        elseif(ibug22_antidiff==-1)then
          debug_outp = .true.
          KK =  1
          do ib=nbf,nbl
              MLW =  BRICK_LIST(NIN,IB)%MLW 
              if(mlw/=51)then
                KK = 0
              endif          
          enddo
          if (KK==0)debug_outp=.false.
        endif
        if(((itrimat/=ibug22_itrimat).and.(ibug22_itrimat/=-1)))debug_outp=.false.
      endif
      if(debug_outp)then
        print *, "    |----ale51_antidiff3_int22.F-----|"
        print *, "    |       THREAD INFORMATION       |"
        print *, "    |--------------------------------|" 
        print *, "    NCYCLE  =", NCYCLE      
        print *, "    ITRIMAT =", ITRIMAT         
      endif
      !INTERFACE 22 ONLY - OUTPUT---------------!
      
      
      DO IB=NBF,NBL  
        IE                =  BRICK_LIST(NIN,IB)%ID 
        MLW               =  BRICK_LIST(NIN,IB)%MLW               
        NCELL             =  BRICK_LIST(NIN,IB)%NBCUT 
        MCELL             =  BRICK_LIST(NIN,IB)%MainID       
        ICELL             =  0  
        DO WHILE (ICELL<=NCELL) ! loop on polyhedron {1:NCELL} U {9}
          ICELL = ICELL +1
          IF (ICELL>NCELL .AND. NCELL/=0)ICELL=9 
          !get_main_data
          J      = BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(1)
          ICELLM = BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(2)
          IF(J==0)THEN
            IE_M   = IE
            IBM    = IB
            ICELLM = MCELL
          ELSEIF(J<=NV46)THEN
            IE_M   = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,1)
            IBM    = BRICK_LIST(NIN,IB)%Adjacent_Brick(J,4)
          ELSE
            J1     = J/10
            J2     = MOD(J,10)
            IBv    = BRICK_LISt(NIN,IB )%Adjacent_Brick(J1,4)
            IE_M   = BRICK_LIST(NIN,IBv)%Adjacent_Brick(J2,1)
            IBM    = BRICK_LIST(NIN,IBv)%Adjacent_Brick(J2,4)            
          ENDIF 
          NG       = BRICK_LIST(NIN,IBM)%NG
          IDLOC    = BRICK_LIST(NIN,IBM)%IDLOC
          MLW      = BRICK_LIST(NIN,IBM)%MLW
          IF(MLW/=51)CYCLE
          ALPH     = BRICK_LIST(NIN,IBM)%POLY(ICELLM)%VFRACm(ITRIMAT)
          VOLG     = ELBUF_TAB(NG)%GBUF%VOL(IDLOC)
          VOL0     = VOLG*UDT
          AV0      = ALPH * VOL0
          UAV0     = VOL0 - AV0
          ALPHI    = ZERO
          UALPHI   = ZERO 
          PHI0     = ZERO              
          !-----------------------------------------------
          !       face voisine du voisin  
          !       et flux total sortant 
          !-----------------------------------------------
          DO K=1,NV46
            NADJ = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(K)%NAdjCell
            DO IADJ=1,NADJ 
              tmpFLUX(K,IADJ) = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(K)%Adjacent_UpwFLUX(IADJ)
              IF(tmpFLUX(K,IADJ)>ZERO)THEN
                IEV    = BRICK_LIST(NIN,IB)%Adjacent_Brick(K,1)
                IBV    = BRICK_LIST(NIN,IB)%Adjacent_Brick(K,4)
                IFV    = BRICK_LIST(NIN,IB)%Adjacent_Brick(K,5)
                ICELLv = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(K)%Adjacent_Cell(IADJ)
                IF(ICELLv==0) THEN !adj elem does not exist
                  ALPHv(K,IADJ) = ALPH
                ELSE !adjacent elem does exist
                  IF(IBv==0)THEN
                    IF(IEV==0)print *, "inter22 : potential material leakage, Check domain boundaries..."
                    ALPHv(K,IADJ) = VFRAC(IEV)
                  ELSE
                    ALPHv(K,IADJ) = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%VFRACm(ITRIMAT)                                      
                  ENDIF
                ENDIF
                FF(K,IADJ)= ALPHv(K,IADJ) * tmpFLUX(K,IADJ)
                !flux sortant estime
                ALPHI  = ALPHI  + FF(K,IADJ)
                !flux sortant initial
                PHI0 = PHI0 + tmpFLUX(K,IADJ) 
              ENDIF
            ENDDO!next IADJ
          ENDDO!next K
          !vide sortant estime
          UALPHI = PHI0 - ALPHI 
          !-----------------------------------------------
          !       flux sortant par face
          !-----------------------------------------------
          IF(ALPHI>AV0.AND.AV0>ZERO)THEN
            !-----------------------------------------------
            !         flux sortant > volume non vide
            !-----------------------------------------------
            AAA = AV0 / ALPHI
            DO K=1,NV46
              NADJ = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(K)%NAdjCell
              DO IADJ=1,NADJ             
                IF(tmpFLUX(K,IADJ)>ZERO)THEN
                  FF(K,IADJ) = FF(K,IADJ) * AAA
                ENDIF
              ENDDO!necti IADJ
            ENDDO!next K  
          ELSEIF(UALPHI>UAV0.AND.UAV0>ZERO)THEN
            !-----------------------------------------------
            !        vide sortant > vide disponible
            !-----------------------------------------------
            AAA = UAV0/UALPHI
            DO K=1,NV46
              NADJ = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(K)%NAdjCell
              DO IADJ=1,NADJ                 
                IF(tmpFLUX(K,IADJ)>ZERO)THEN
                  FF(K,IADJ) = tmpFLUX(K,IADJ) + (FF(K,IADJ)-tmpFLUX(K,IADJ))*AAA
                ENDIF
              ENDDO!next IADJ
            ENDDO!next K
          ENDIF
          !-----------------------------------------------
          !         flux sortant 
          !-----------------------------------------------
          DO K=1,NV46
            NADJ = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(K)%NAdjCell
            DO IADJ=1,NADJ  
              IEV    = BRICK_LIST(NIN,IB)%Adjacent_Brick(K,1)
              IBV    = BRICK_LIST(NIN,IB)%Adjacent_Brick(K,4)
              IFV    = BRICK_LIST(NIN,IB)%Adjacent_Brick(K,5)
              ICELLv = BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(K)%Adjacent_Cell(IADJ)                         
              IF(tmpFLUX(K,IADJ)>ZERO)THEN
                FF(K,IADJ) = HALF * ( FF(K,IADJ)*(ONE-UPWSM)+ALPH*tmpFLUX(K,IADJ)*(ONE+UPWSM) )

                
                
                !INTERFACE 22 ONLY------------------------!
                if(debug_outp)then 
                ie = brick_list(nin,ib)%Id 
                 if(ibug22_antidiff==ixs(11,ie) .OR. ibug22_antidiff==-1)then                            
                           
                   print *,                    "      brique       =", ixs(11,ie)
                   print *,                    "      icell        =", ICELL                 
                   print *,                    "      FACE         =", K
                   print *,                    "      ALPH         =", ALPH                   
                   print *,                    "      ALPHv        =", ALPHv(K,IADJ)                   
                   write (*,FMT='(A,6E26.14)')"       WAS  Flux(J) =", BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(K)%Adjacent_upwFLUX(IADJ)
                   write (*,FMT='(A,6E26.14)')"       IS  Flux(J)  =", FF(K,IADJ)             
                   print *, "      ------------------------"               
                 endif
                endif
                !-----------------------------------------!                
                
                !flux is here updated
                BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(K)%Adjacent_UpwFLUX(IADJ) = FF(K,IADJ)                
                
                !adjacent flux is also updated to be consistent and conservative
                IF(ICELLv>0)THEN
                  !recherche du flux voisin 
                  !OPTIM PLUS TARD : si deja calcule dans sinit, stocker pour economiser acces memoires
                  IF(IBv>0)THEN
                    !--IN CUT CELL BUFFER
                    NADJv = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%FACE(IFv)%NAdjCell
                    DO IADJv=1,NADJv
                      IF(BRICK_LIST(NIN,IBv)%POLY(ICELLv)%FACE(IFv)%Adjacent_Cell(IADjv)==ICELL)EXIT
                      !(IB,ICELL,IADjv) <---bijected to---> (IBv,ICELLv,IADJ)
                    ENDDO
                    debug_tmp = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%FACE(IFv)%Adjacent_UpwFLUX(IADJv)
                    BRICK_LIST(NIN,IBv)%POLY(ICELLv)%FACE(IFv)%Adjacent_UpwFLUX(IADJv) = -FF(K,IADJ)
                  ELSE
                    !--NOT IN CUT CELL BUFFER
                    debug_tmp = FLUX(IFV,IEV)                  
                    FLUX(IFV,IEV) = -FF(K,IADJ)
                  ENDIF
                  
                  !INTERFACE 22 ONLY------------------------!
                  if(debug_outp)then 
                  if(ibug22_antidiff==ixs(11,ie) .OR. ibug22_antidiff==-1)then                              
                    print *,               "        => Setting adjacent flux consequently :"
                    print *,               "        brique.V     =", ixs(11,iev)
                    print *,               "        icell.V      =", ICELLv                
                    print *,               "        FACE.V       =", IFv
               write (*,FMT='(A,6E26.14)')     
     .                                    "         WAS  Flux(J) =", debug_tmp
               write (*,FMT='(A,6E26.14)')     
     .                                    "         IS  Flux(J)  =", -FF(K,IADJ)             
                    print *,              "      ---"               
                  endif
                  endif
                  !-----------------------------------------!  
                ELSE
                !TRAITEMENT SPMD HERE : see ale51_antidiff3.F
                ENDIF
              ENDIF
           ENDDO!next IADJ
          ENDDO!next K
          !-----------------------------------------------          
        ENDDO!next ICELL
      ENDDO!next IB
      
C-------------
      RETURN
      END
C
