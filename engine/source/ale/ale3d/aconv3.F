Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  ACONV3                        source/ale/ale3d/aconv3.F     
Chd|-- called by -----------
Chd|        ACONVE                        source/ale/aconve.F           
Chd|-- calls ---------------
Chd|        ALEFVM_MOD                    ../common_source/modules/ale/alefvm_mod.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE ACONV3(VTOT ,PHI,FLUX,FLU1,IXS,
     .                  ALE_CONNECT,IOFF,QMV,IFLG,ITRIMAT,
     .                  TAG22,NVAR,ITASK)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE I22TRI_MOD  ! , use only : ibug22_convec
      USE ALEFVM_MOD
      USE ALE_CONNECTIVITY_MOD
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
      INTEGER IOFF, IFLG,IXS(NIXS,NUMELS),NVAR,ITASK
      my_real VTOT(*), PHI(*), FLUX(6,*), FLU1(*), QMV(12,*), TAG22(MVSIZ)
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
      INTEGER :: I, IE, IV,J,ITRIMAT,IAD2,LGTH
      my_real :: VALVOIS(6,MVSIZ),VALEL(MVSIZ),VL(6,MVSIZ), DELTA(MVSIZ)
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
               VALVOIS(J,I) = PHI(IV)
            ELSEIF(IV == 0)THEN
               VALVOIS(J,I) = PHI(IE)
            ELSE
               !-IV is segment ID
               ! ebcs PHI(NUMEL + 1:NSEGFLU) is filled in aconve.F using SEGVAR (filled in ebcs[0-9][0-9].F)
               VALVOIS(J,I) = PHI(-IV+IOFF)
            ENDIF
         ENDDO
      ENDDO

      DO I=LFT,LLT
        VL(1,I)          = VALVOIS(1,I)*FLUX(1,I)
        VL(2,I)          = VALVOIS(2,I)*FLUX(2,I)
        VL(3,I)          = VALVOIS(3,I)*FLUX(3,I)
        VL(4,I)          = VALVOIS(4,I)*FLUX(4,I)
        VL(5,I)          = VALVOIS(5,I)*FLUX(5,I)
        VL(6,I)          = VALVOIS(6,I)*FLUX(6,I) 
        DELTA(I)         = HALF * DT1 *(-VALEL(I)*FLU1(I) - VL(1,I)-VL(2,I)-VL(3,I)-VL(4,I)-VL(5,I)-VL(6,I))
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
          QMV(1,I) = QMV(1,I) - VL(1,I) - VALEL(I)*QMV(07,I) 
          QMV(2,I) = QMV(2,I) - VL(2,I) - VALEL(I)*QMV(08,I) 
          QMV(3,I) = QMV(3,I) - VL(3,I) - VALEL(I)*QMV(09,I) 
          QMV(4,I) = QMV(4,I) - VL(4,I) - VALEL(I)*QMV(10,I) 
          QMV(5,I) = QMV(5,I) - VL(5,I) - VALEL(I)*QMV(11,I) 
          QMV(6,I) = QMV(6,I) - VL(6,I) - VALEL(I)*QMV(12,I)  
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
            !print *, "    |------ale3d_conv3.F------ |"
            !print *, "    |     THREAD INFORMATION   |"
            !print *, "    |--------------------------|" 
            !print *, "    NCYCLE  =", NCYCLE      
            !print *, "    ITRIMAT =", ITRIMAT         
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
