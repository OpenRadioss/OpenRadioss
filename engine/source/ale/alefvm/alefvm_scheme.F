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
Chd|  ALEFVM_SCHEME                 source/ale/alefvm/alefvm_scheme.F
Chd|-- called by -----------
Chd|        ALEFVM_MAIN                   source/ale/alefvm/alefvm_main.F
Chd|-- calls ---------------
Chd|        ALEFVM_EXPAND_MOM2            source/ale/alefvm/alefvm_expand_mom2.F
Chd|        ALEFVM_MOD                    ../common_source/modules/alefvm_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE ALEFVM_SCHEME (
     1                          IXS,  NV46,  IALEFVM_FLG, ITASK ,
     2                          MOM,  VOL ,  RHO  , V          , VEUL  ,
     3                          IPM,  MSNF,  X    , NALE       , IAD22 ,
     4                          SSP,  SIG ,  NEL  )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C 'alefvm' is related to a collocated scheme (built from FVM and based on Godunov scheme)
C  which was temporarily introduced for experimental option /INTER/TYPE22 (FSI coupling with cut cell method)
C This cut cell method is not completed, abandonned, and is not an official option.
C There is no other use for this scheme which is automatically enabled when /INTER/TYPE22 is defined (INT22>0 => IALEFVM=1).
C
C This subroutine is treating an uncut cell.
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALEFVM_MOD
      USE I22TRI_MOD
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
C This subroutines computes cell momentum
C using finit volume scheme
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: IXS(NIXS,*), NV46, IALEFVM_FLG, ITASK,IPM(NPROPMI,*),NALE(*),NEL
      my_real :: MOM(NEL,3), VOL(MVSIZ), RHO(MVSIZ), V(3,*),VEUL(*),MSNF(NUMNOD),X(3,*),IAD22(*),SSP(*),SIG(LLT,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, II, IV, J, IMAT, ILAW, NIN, IB
      my_real :: FORCE(3,6), F0(3), MASS, DMOM(3,MVSIZ)
      LOGICAL :: debug_outp
      INTEGER :: idbf,idbl
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------      
      IF(IALEFVM==0)    RETURN
      IF(IALEFVM_FLG <= 1)RETURN
      IMAT = IXS(1,LFT+NFT)
      ILAW = IPM(2,IMAT)

C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------

      IF(ILAW /= 11)THEN
        !-------------------------------------------------------------!
        ! INTEGRAL ON EACH FACE  from Integral(DIV(SIGMA),Volume)     !
        !-------------------------------------------------------------!
        DO I=LFT,LLT
          II          = I + NFT
          DMOM(1:3,I) = FCELL(1:3,II)
          IF(DT1==ZERO)THEN
            DMOM(1:3,I) = HALF*DT2 * DMOM(1:3,I)
          ELSE
            DMOM(1:3,I) =        DT2 * DMOM(1:3,I)
          ENDIF
        ENDDO!next I

        DO I=LFT,LLT
          II          = I + NFT
          MOM(I,1)    = MOM(I,1) + dMOM(1,I)
          MOM(I,2)    = MOM(I,2) + dMOM(2,I)
          MOM(I,3)    = MOM(I,3) + dMOM(3,I)		
        ENDDO!next I


        !DEBUG-OUTPUT---------------!
        if(IALEFVM_OUTP_SCHEME /= 0)then
          debug_outp = .FALSE.
          if(IALEFVM_OUTP_SCHEME>0)then
            do i=lft,llt
              ii = nft + i
              if(ixs(11,ii)==IALEFVM_OUTP_SCHEME)THEN
                debug_outp = .TRUE.
                idbf   = i
                idbl   = i
                EXIT
              endif
             enddo
          elseif(IALEFVM_OUTP_SCHEME==-1)then
            debug_outp=.TRUE.
                idbf   = lft
                idbl   = llt
          endif
          if(debug_outp)then
  !#!include "lockon.inc"
            print *, "    |----alefvm_scheme.F-----|"
            print *, "    |   THREAD INFORMATION   |"
            print *, "    |------------------------|"
            print *, "     NCYCLE =", NCYCLE
            do i=idbf,idbl
              ii = nft + i
              print *,                       "      brique=", ixs(11,nft+i)
              write(*,FMT='(A,1E26.14)')     "             RHO           =", RHO(I)
              write(*,FMT='(A,1E26.14)')     "             VOL           =", VOL(I)
              write(*,FMT='(A,1E26.14)')     "            MASS           =", RHO(I)*VOL(I)
              write(*,FMT='(A)')             "       #-- cell momentum --#"
              write (*,FMT='(3(A,1E26.14))') "             Q-X           =", MOM(I,1) -dMOM(1,I)," +",dMOM(1,I)," =",MOM(I,1)	
              write (*,FMT='(3(A,1E26.14))') "             Q-Y           =", MOM(I,2) -dMOM(2,I)," +",dMOM(2,I)," =",MOM(I,2)
              write (*,FMT='(3(A,1E26.14))') "             Q-Z           =", MOM(I,3) -dMOM(3,I)," +",dMOM(3,I)," =",MOM(I,3)			
              write(*,FMT='(A)')             "       #-- cell momentum densities--#"
              write (*,FMT='(3(A,1E26.14))') "             rho.Ux        =", MOM(I,1)  / VOL(I)
              write (*,FMT='(3(A,1E26.14))') "             rho.Uy        =", MOM(I,2)  / VOL(I)
              write (*,FMT='(3(A,1E26.14))') "             rho.Uz        =", MOM(I,3)  / VOL(I)
              write(*,FMT='(A)')             "       #-- cell velocities--#"
              write (*,FMT='(3(A,1E26.14))') "             Ux            =", MOM(I,1)  / VOL(I)/RHO(I)
              write (*,FMT='(3(A,1E26.14))') "             Uy            =", MOM(I,2)  / VOL(I)/RHO(I)
              write (*,FMT='(3(A,1E26.14))') "             Uz            =", MOM(I,3)  / VOL(I)/RHO(I)
              print *, "      "
            enddo
  !#!include "lockoff.inc"
          endif
          endif
        !-----------------------------------------!
      ENDIF  !IF(ILAW /= 11)THEN

      !EXPAND MOMENTUM TO NODES FOR POST-TREATMENT
      !call my_barrier
      CALL ALEFVM_EXPAND_MOM2 (
     .                        X   ,V   , IXS  , NV46,
     .                        MOM ,VEUL, ITASK, NALE , NEL )

      DO I=LFT,LLT
        !MOM=[rhoU]*VOL -> U : needed for fluxes calculation
        II          = I + NFT
        !MASS        = RHO(I) * VOL(I)
        !MOM(1,I)    = MOM(1,I) / MASS
        !MOM(2,I)    = MOM(2,I) / MASS
        !MOM(3,I)    = MOM(3,I) / MASS
        MOM(I,1)    = MOM(I,1) / VOL(I)
        MOM(I,2)    = MOM(I,2) / VOL(I)
        MOM(I,3)    = MOM(I,3) / VOL(I) 	
      ENDDO!next I

      !internal force in animation file USER7 -> ||Fint||
      IF(INT22 > 0)THEN

        DO I=LFT,LLT
          II                    = I + NFT
          INT22_FCELL_ANIM(II)  = SQRT ( FCELL(1,II)*FCELL(1,II) + FCELL(2,II)*FCELL(2,II) + FCELL(3,II)*FCELL(3,II) )
        ENDDO!next I

        NIN = 1
        DO I=LFT,LLT
          II = I+NFT
          IB = NINT(IAD22(I))
          IF (IB>0)THEN
            BRICK_LIST(NIN,IB)%FCELL(1:3) = FCELL(1:3,II)
          ENDIF
        ENDDO

      ENDIF


      DO I=LFT,LLT
        II          = I + NFT
        FCELL(1,II) = MOM(I,1)
        FCELL(2,II) = MOM(I,2)
        FCELL(3,II) = MOM(I,3)
        FCELL(4,II) = RHO(I)
        FCELL(5,II) = SSP(I)
        FCELL(6,II) = -THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
      ENDDO!next I





      RETURN
      END
