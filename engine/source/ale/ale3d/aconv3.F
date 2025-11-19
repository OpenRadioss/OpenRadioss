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
!||    aconv3                 ../engine/source/ale/ale3d/aconv3.F
!||--- called by ------------------------------------------------------
!||    aconve                 ../engine/source/ale/aconve.F
!||--- calls      -----------------------------------------------------
!||    my_barrier             ../engine/source/system/machine.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod   ../common_source/modules/ale/ale_connectivity_mod.F
!||    alefvm_mod             ../common_source/modules/ale/alefvm_mod.F
!||    element_mod            ../common_source/modules/elements/element_mod.F90
!||    i22tri_mod             ../common_source/modules/interfaces/cut-cell-search_mod.F
!||====================================================================
      SUBROUTINE ACONV3(VTOT ,PHI,FLUX,FLU1,IXS,
     .                  ALE_CONNECT,IOFF,QMV,IFLG,
     .                  TAG22,NVAR,ITASK,s_flux,s_qmv,offset,nv46)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE I22TRI_MOD  ! , use only : ibug22_convec
      USE ALEFVM_MOD
      USE ALE_CONNECTIVITY_MOD
      use element_mod , only : nixs
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
#include      "comlock.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      integer, intent(in) :: s_flux !< first dimension of FLUX array
      integer, intent(in) :: s_qmv  !< first dimension of QMV array
      integer, intent(in) :: offset !< offset for FLUX and QMV arrays
      integer, intent(in) :: nv46  !< second dimension of FLUX and QMV arrays
      INTEGER IOFF, IFLG,IXS(NIXS,NUMELS),NVAR,ITASK
      my_real VTOT(*), PHI(*), FLUX(s_flux,nv46), FLU1(*), QMV(s_qmv,2*nv46), TAG22(MVSIZ)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, IE, IV,J,IAD2,LGTH
      my_real :: VALVOIS(MVSIZ,6),VALEL(MVSIZ),VL(MVSIZ,6), DELTA(MVSIZ)
      LOGICAL debug_outp
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      !---------------------------------------------------------!
      ! CONVECTION                                              !
      !---------------------------------------------------------!
      DO I=LFT,LLT
        IE               = NFT+I
        VALEL(I)         = PHI(IE)
      ENDDO
      DO I=LFT,LLT
         IE             = NFT+I
         IAD2 = ALE_CONNECT%ee_connect%iad_connect(IE)
         LGTH = ALE_CONNECT%ee_connect%iad_connect(IE+1) - IAD2
         DO J=1,LGTH
            
            IV = ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
            IF(IV > 0)THEN
               VALVOIS(I,J) = PHI(IV)
            ELSEIF(IV == 0)THEN
               VALVOIS(I,J) = PHI(IE)
            ELSE
               !-IV is segment ID
               ! ebcs PHI(NUMEL + 1:NSEGFLU) is filled in aconve.F using SEGVAR (filled in ebcs[0-9][0-9].F)
               VALVOIS(I,J) = PHI(-IV+IOFF)
            ENDIF
         ENDDO
      ENDDO

      DO I=LFT,LLT
        IE               = NFT+I+offset
        VL(I,1)          = VALVOIS(I,1)*FLUX(IE,1)
        VL(I,2)          = VALVOIS(I,2)*FLUX(IE,2)
        VL(I,3)          = VALVOIS(I,3)*FLUX(IE,3)
        VL(I,4)          = VALVOIS(I,4)*FLUX(IE,4)
        VL(I,5)          = VALVOIS(I,5)*FLUX(IE,5)
        VL(I,6)          = VALVOIS(I,6)*FLUX(IE,6) 
        DELTA(I)         = HALF * DT1 *(-VALEL(I)*FLU1(I) - VL(I,1)-VL(I,2)-VL(I,3)-VL(I,4)-VL(I,5)-VL(I,6))
      ENDDO

      IF(ALEFVM_Param%IEnabled/=0)THEN
        IF(NVAR==2)THEN
          DO I=LFT,LLT
            DELTA(I) = DELTA(I) + (ALEFVM_Param%IWFEXT)*DT1*ALEFVM_Buffer%WFEXT_CELL(I+NFT)
          ENDDO
        ENDIF
      ENDIF

      IF(INT22==0)THEN
        DO I=LFT,LLT
          VTOT(I) = VTOT(I) + DELTA(I)        
        ENDDO
      ELSE
        DO I=LFT,LLT
          IF(TAG22(I)==ZERO) VTOT(I) = VTOT(I) + DELTA(I)  !can be optimized later (conditional test is currently within the loop)
        ENDDO     
      ENDIF

      IF(TRIMAT > 0.AND.IFLG == 1)THEN
        DO I=LFT,LLT
          ie = nft+i+offset
          QMV(ie,1) = QMV(ie,1) - VL(I,1) - VALEL(I)*QMV(ie,07) 
          QMV(ie,2) = QMV(ie,2) - VL(I,2) - VALEL(I)*QMV(ie,08) 
          QMV(ie,3) = QMV(ie,3) - VL(I,3) - VALEL(I)*QMV(ie,09) 
          QMV(ie,4) = QMV(ie,4) - VL(I,4) - VALEL(I)*QMV(ie,10) 
          QMV(ie,5) = QMV(ie,5) - VL(I,5) - VALEL(I)*QMV(ie,11) 
          QMV(ie,6) = QMV(ie,6) - VL(I,6) - VALEL(I)*QMV(ie,12)  
        ENDDO
      ENDIF
C-----------


      !INTERFACE 22 ONLY / OUTPUT---------------!(OBSOLETE)
      !---------------------------------------------------------!
      ! DEBUG OUTPUT                                            !
      !---------------------------------------------------------!
      !INTERFACE 22 ONLY - OUTPUT---------------!
      debug_outp = .false.
      if(int22>0)then
        debug_outp=.false. 
        if(ibug22_convec /= 0 .AND. IBUG22_NVAR==NVAR)then
           debug_outp=.true.  
        endif

        if(debug_outp .AND. IBUG22_NVAR==NVAR)then

          call my_barrier
          
          if(itask==0)then
            if(IBUG22_NVAR==NVAR)then
              do i=lft,llt
                if(int22>0)then;if (tag22(i)/=zero)then;cycle                 ;endif;endif
                if(ibug22_convec == ixs(11,i+nft) .OR. ibug22_convec == -1)then                
                  if((ibug22_convec==ixs(11,nft+i).OR.ibug22_convec==-1) .AND. nvar==1)then 
                  if(delta(i) == zero)cycle                             
                    !if(int22>0)then;if (tag22(i)==zero)then;print *,"       UNCUT";endif;endif               
                    print *, "      brique=", ixs(11,nft+i)
                    print *, "        nvar=", NVAR
                    print *, "        dval=", delta(i)
                    print *, "         was:", vtot(i)-delta(i)
                    print *, "          is:", vtot(i)
                    print *, "      ------------------------" 
                  endif   
                endif      
              enddo
            endif!(IBUG22_NVAR==NVAR)
          endif !itask
          
          call my_barrier

          if(itask==1)then       
            if(IBUG22_NVAR==NVAR)then
              do i=lft,llt
                if(int22>0)then;if (tag22(i)/=zero)then;cycle                 ;endif;endif
                if(ibug22_convec == ixs(11,i+nft) .OR. ibug22_convec == -1)then                
                  if((ibug22_convec==ixs(11,nft+i).OR.ibug22_convec==-1) .AND. nvar==1)then 
                  if(delta(i) == zero)cycle                             
                    !if(int22>0)then;if (tag22(i)==zero)then;print *,"       UNCUT";endif;endif       
                    print *, "      brique=", ixs(11,nft+i)
                    print *, "        nvar=", NVAR
                    print *, "        dval=", delta(i)
                    print *, "         was:", vtot(i)-delta(i)
                    print *, "          is:", vtot(i)
                    print *, "      ------------------------" 
                  endif   
                endif      
              enddo
            endif!(IBUG22_NVAR==NVAR)
          endif !itask

          call my_barrier

          if(itask==2)then       
            if(IBUG22_NVAR==NVAR)then
              do i=lft,llt
                if(int22>0)then;if (tag22(i)/=zero)then;cycle                 ;endif;endif
                if(ibug22_convec == ixs(11,i+nft) .OR. ibug22_convec == -1)then                
                  if((ibug22_convec==ixs(11,nft+i).OR.ibug22_convec==-1) .AND. nvar==1)then 
                  if(delta(i) == zero)cycle                             
                    !if(int22>0)then;if (tag22(i)==zero)then;print *,"       UNCUT";endif;endif       
                    print *, "      brique=", ixs(11,nft+i)
                    print *, "        nvar=", NVAR
                    print *, "        dval=", delta(i)
                    print *, "         was:", vtot(i)-delta(i)
                    print *, "          is:", vtot(i)
                    print *, "      ------------------------" 
                  endif   
                endif      
              enddo
            endif!(IBUG22_NVAR==NVAR)
          endif !itask
          
          call my_barrier

          if(itask==3)then       
            if(IBUG22_NVAR==NVAR)then
              do i=lft,llt
                if(int22>0)then;if (tag22(i)/=zero)then;cycle                 ;endif;endif
                if(ibug22_convec == ixs(11,i+nft) .OR. ibug22_convec == -1)then                
                  if((ibug22_convec==ixs(11,nft+i).OR.ibug22_convec==-1) .AND. nvar==1)then 
                  if(delta(i) == zero)cycle                             
                    !if(int22>0)then;if (tag22(i)==zero)then;print *,"       UNCUT";endif;endif       
                    print *, "      brique=", ixs(11,nft+i)
                    print *, "        nvar=", NVAR
                    print *, "        dval=", delta(i)
                    print *, "         was:", vtot(i)-delta(i)
                    print *, "          is:", vtot(i)
                    print *, "      ------------------------" 
                  endif   
                endif      
              enddo
            endif!(IBUG22_NVAR==NVAR)
          endif !itask

        endif!(debug_outp==.true.)
      endif!(int22>0)
      !-----------------------------------------!

C-----------------------------------------------   
      RETURN
      END
C
