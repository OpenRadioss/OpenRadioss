Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    alefvm_gravity_int22   ../engine/source/ale/alefvm/alefvm_gravity_int22.F
!||--- called by ------------------------------------------------------
!||    sforc3                 ../engine/source/elements/solid/solide/sforc3.F
!||--- uses       -----------------------------------------------------
!||    alefvm_mod             ../common_source/modules/ale/alefvm_mod.F
!||    i22bufbric_mod         ../common_source/modules/interfaces/cut-cell-search_mod.F
!||====================================================================
      SUBROUTINE ALEFVM_GRAVITY_INT22 (VOLN, IXS , RHO , IAD22 )
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
#include      "inter22.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D e s c r i p t i o n
C----------------------------------------------- 
C This subroutines computes gravity forces for
C finite volume scheme (IALEFVM==1)
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IXS(NIXS,*)
      my_real :: VOLN(MVSIZ),RHO(MVSIZ),IAD22(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, J, K, IB, MNOD
      INTEGER :: NVERTEX,INOd,II, NIN
      my_real :: M_CELL(MVSIZ),ACCEL(3,MVSIZ)
      LOGICAL :: debug_outp
      INTEGER :: idbf,idbl , MCELL
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------      
      IF(ALEFVM_Param%IEnabled==0)RETURN
      IF(INT22==0)  RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------

      NIN = 1

      !-------------------------------------------------------------!
      ! GRAVITY COMPUTED FOR CELL CENTROIDS                         !
      !-------------------------------------------------------------!      
      DO I=LFT,LLT
        IB            = NINT(IAD22(I))
        IF(IB<=0)CYCLE
        II            = I+NFT
        NVERTEX       = 0
        ACCEL(1:3,I)  = ZERO
        ALEFVM_Buffer%FCELL(1:3,II) = ZERO
        !loop on main cell vertexes
        MCELL         = BRICK_LIST(NIN,IB)%MainID
        MNOD          = BRICK_LIST(NIN,IB)%POLY(MCELL)%NumNOD 
        DO K=1,MNOD
          J           = BRICK_LIST(NIN,IB)%POLY(MCELL)%ListNodID(K)
          INOD        = IXS(1+J,I+NFT)
          IF(ALEFVM_Buffer%VERTEX(4,INOD)==ZERO) CYCLE
          NVERTEX     = NVERTEX + 1
          ACCEL(1,I)  = ACCEL(1,I) + ALEFVM_Buffer%VERTEX(1,INOD)
          ACCEL(2,I)  = ACCEL(2,I) + ALEFVM_Buffer%VERTEX(2,INOD)
          ACCEL(3,I)  = ACCEL(3,I) + ALEFVM_Buffer%VERTEX(3,INOD) 
        ENDDO        
        IF(NVERTEX>0)THEN
          ACCEL(1,I) = ACCEL(1,I) / NVERTEX
          ACCEL(2,I) = ACCEL(2,I) / NVERTEX
          ACCEL(3,I) = ACCEL(3,I) / NVERTEX 
        ENDIF                   
      ENDDO!next I
      
      DO I=LFT,LLT
        M_CELL(I)   = RHO(I)*VOLN(I) !Check if VOLN is the supercell volume. Should be unchanged since december 2015
      ENDDO!next I

      DO I=LFT,LLT
        IB          = NINT(IAD22(I))
        IF(IB<=0)CYCLE      
        II          = I + NFT
        ALEFVM_Buffer%FCELL(1,II) = ALEFVM_Buffer%FCELL(1,II) + M_CELL(I) * ACCEL(1,I)
        ALEFVM_Buffer%FCELL(2,II) = ALEFVM_Buffer%FCELL(2,II) + M_CELL(I) * ACCEL(2,I)
        ALEFVM_Buffer%FCELL(3,II) = ALEFVM_Buffer%FCELL(3,II) + M_CELL(I) * ACCEL(3,I)                
      ENDDO!next I

        !DEBUG-OUTPUT---------------!
        if(ALEFVM_Param%IOUTP_GRAV /= 0)then
          debug_outp = .FALSE.
          if(ALEFVM_Param%IOUTP_GRAV>0)then
            do i=lft,llt
              ii = nft + i
              if(ixs(11,ii)==ALEFVM_Param%IOUTP_GRAV)THEN
                debug_outp = .TRUE.
                idbf   = i
                idbl   = i
                EXIT
              endif
             enddo
          elseif(ALEFVM_Param%IOUTP_GRAV==-1)then
            debug_outp=.TRUE.
                idbf   = lft
                idbl   = llt          
          endif
!#!include "lockon.inc"  
          if(debug_outp)then     
!#!include "lockon.inc"       
          print *, "    |----alefvm_gravity.F----|"
          print *, "    |   THREAD INFORMATION   |"
          print *, "    |------------------------|" 
          print *, "     NCYCLE =", NCYCLE
          do i=idbf,idbl
            ii = nft + i
            IB = NINT(IAD22(I))
            IF(IB<=0)CYCLE            
            print *,                    "      brique=", ixs(11,nft+i)
            write(*,FMT='(A24,1A26)')   "                        ",             
     .                                  "#--------- cell----------#"          
            write (*,FMT='(A,1E26.14)') "            Rho  =", RHO(I)
            write (*,FMT='(A,1E26.14)') "            Vol  =", VOLN(I)
            write (*,FMT='(A,1E26.14)') "           Mass  =", M_CELL(I)
            write (*,FMT='(A,1E26.14)') "        Accel-X  =", ACCEL(1,I)           
            write (*,FMT='(A,1E26.14)') "        Accel-Y  =", ACCEL(2,I)          
            write (*,FMT='(A,1E26.14)') "        Accel-Z  =", ACCEL(3,I) 
            write(*,FMT='(A24,8A26)')   "                        ",    
     .                                  "#--------- nod_1 ---------","#--------- nod_2 ---------",
     .                                  "#--------- nod_3 ---------","#--------- nod_4 ---------",
     .                                  "#--------- nod_5 ---------","#--------- nod_6 ---------",
     .                                  "#--------- nod_7 ---------","#--------- nod_8 --------#"     
            write (*,FMT='(A,8E26.14)') "           acc-X =", ALEFVM_Buffer%VERTEX(1,IXS(2:9,I))          
            write (*,FMT='(A,8E26.14)') "           acc-Y =", ALEFVM_Buffer%VERTEX(2,IXS(2:9,I))
            write (*,FMT='(A,8E26.14)') "           acc-Z =", ALEFVM_Buffer%VERTEX(3,IXS(2:9,I))                       
            print *, "      "          
          enddo
!#!include "lockoff.inc"       
        endif
        endif
      !-----------------------------------------!

 

      RETURN
      END
