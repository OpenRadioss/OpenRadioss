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
!||    ale51_upwind3          ../engine/source/ale/ale51/ale51_upwind3.F
!||--- called by ------------------------------------------------------
!||    afluxt                 ../engine/source/ale/ale51/afluxt.F
!||    ale51_finish           ../engine/source/ale/ale51/ale51_finish.F
!||    ale51_init             ../engine/source/ale/ale51/ale51_init.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod   ../common_source/modules/ale/ale_connectivity_mod.F
!||    element_mod            ../common_source/modules/elements/element_mod.F90
!||    i22tri_mod             ../common_source/modules/interfaces/cut-cell-search_mod.F
!||====================================================================
      SUBROUTINE ALE51_UPWIND3(PM     ,IXS  ,FLUX ,FLU1 ,ALE_CONNECT,
     .                         ITRIMAT,DDVOL,QMV  ,IFLG ,
     .                         NV46,s_flux,s_qmv,
     .                         nb_element_with_bc,list_element_with_bc)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C  This subroutines computes from direct fluxes :
C  -1- FLUX(1:6,*), FLU1(*) : Decomposition Factors from Biased Upwind Method
C  -2- QMV(7:12)            :
C  -3- DDVOL                : D/DV . D/DT . VOL = DV/DT
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE I22TRI_MOD
      USE ALE_CONNECTIVITY_MOD
      use element_mod , only : nixs
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
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      integer, intent(in) :: s_flux !< first dimension of flux array
      integer, intent(in) :: s_qmv  !< first dimension of QMV array
      integer, intent(in) :: nb_element_with_bc !< number of element with boundary conditions
      integer, dimension(nb_element_with_bc), intent(in) :: list_element_with_bc !< list of element with boundary conditions
      INTEGER IXS(NIXS,NUMELS), ITRIMAT,IFLG,NV46
      my_real PM(NPROPM,NUMMAT), FLUX(s_flux,NV46), FLU1(s_flux),DDVOL(*)
      my_real,TARGET :: QMV(s_qmv,2*NV46)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,MAT(MVSIZ),IAD2
      integer :: ijk
      my_real REDUC, UPWL(mvsiz,NV46),FLUX1(MVSIZ), FLUX2(MVSIZ), FLUX3(MVSIZ), FLUX4(MVSIZ),FLUX5(MVSIZ), FLUX6(MVSIZ)
      my_real, TARGET :: QMVL(MVSIZ,7:12)
      my_real, DIMENSION(:) ,POINTER :: pQMV7,pQMV8,pQMV9,pQMV10,pQMV11,pQMV12
      integer :: mat_id
      logical :: only_one_mat
      LOGICAL :: debug_outp
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
      DO I=LFT,LLT
        FLUX1(I) = FLUX(NFT+I,1)
        FLUX2(I) = FLUX(NFT+I,2)
        FLUX3(I) = FLUX(NFT+I,3)
        FLUX4(I) = FLUX(NFT+I,4)
      ENDDO
      IF(NV46==6)THEN
      DO I=LFT,LLT
        FLUX5(I) = FLUX(NFT+I,5)
        FLUX6(I) = FLUX(NFT+I,6)
      ENDDO      
      ENDIF
C-----------------------------------------------
      only_one_mat = .true.
      II=1+NFT
      MAT(1)=IXS(1,II)
      mat_id = MAT(1)
      DO I=LFT+1,LLT
        II=I+NFT
        MAT(I)=IXS(1,II)
        if(mat_id/=mat(i)) only_one_mat = .false.
      ENDDO
C-----------------------------------------------
C     UPWIND
C-----------------------------------------------
      if(only_one_mat) then
        do j=1,nv46
          do i=lft,llt
            upwl(i,j) = pm(16,mat_id)
          enddo
        enddo
      else       
        do j=1,nv46
#include "vectorize.inc"
          do i=lft,llt
            upwl(i,j) = pm(16,mat(i))
          enddo
        enddo
      endif
      !======================================================!
      !  BOUNDARY FACE : no volume flux by default           !
      !    slip wall bc                                      !
      !======================================================!
      do ijk=1,nb_element_with_bc
        i = list_element_with_bc(ijk) ! get the index of the element with boudary condition
        iad2 = ale_connect%ee_connect%iad_connect(i+nft)
        reduc = pm(92,mat(i))
        do j=1,nv46
          ii = ale_connect%ee_connect%connected(iad2+j-1) ! get the neighbor element id
          if(ii==0) then
            if(j==1) flux1(i) = flux1(i)*reduc
            if(j==2) flux2(i) = flux2(i)*reduc
            if(j==3) flux3(i) = flux3(i)*reduc
            if(j==4) flux4(i) = flux4(i)*reduc
            if(j==5) flux5(i) = flux5(i)*reduc
            if(j==6) flux6(i) = flux6(i)*reduc
          endif
        enddo
      enddo
      
      DO I=LFT,LLT
        FLUX(NFT+I,1) = FLUX1(I)-UPWL(I,1)*ABS(FLUX1(I))
        FLUX(NFT+I,2) = FLUX2(I)-UPWL(I,2)*ABS(FLUX2(I))
        FLUX(NFT+I,3) = FLUX3(I)-UPWL(I,3)*ABS(FLUX3(I))
        FLUX(NFT+I,4) = FLUX4(I)-UPWL(I,4)*ABS(FLUX4(I))
      ENDDO  
      IF(NV46==6)THEN
      DO I=LFT,LLT
        FLUX(NFT+I,5) = FLUX5(I)-UPWL(I,5)*ABS(FLUX5(I))
        FLUX(NFT+I,6) = FLUX6(I)-UPWL(I,6)*ABS(FLUX6(I))
      ENDDO            
      ENDIF    
      
      IF(IFLG == 1)THEN
        pQMV7  => QMV(NFT+LFT:NFT+LLT,7)
        pQMV8  => QMV(NFT+LFT:NFT+LLT,8)
        pQMV9  => QMV(NFT+LFT:NFT+LLT,9)
        pQMV10 => QMV(NFT+LFT:NFT+LLT,10)
        pQMV11 => QMV(NFT+LFT:NFT+LLT,11)
        pQMV12 => QMV(NFT+LFT:NFT+LLT,12)                                        
      ELSE
        pQMV7  => QMVL(LFT:LLT,7)
        pQMV8  => QMVL(LFT:LLT,8)
        pQMV9  => QMVL(LFT:LLT,9)
        pQMV10 => QMVL(LFT:LLT,10)
        pQMV11 => QMVL(LFT:LLT,11)
        pQMV12 => QMVL(LFT:LLT,12)      
      ENDIF           
      !MOMENTUM ADVECTION NEEDED FOR AMOMT3.F

      IF(NV46==6)THEN
        DO I=LFT,LLT
          pQMV7(I)  = FLUX1(I)+UPWL(I,1)*ABS(FLUX1(I))
          pQMV8(I)  = FLUX2(I)+UPWL(I,2)*ABS(FLUX2(I))
          pQMV9(I)  = FLUX3(I)+UPWL(I,3)*ABS(FLUX3(I))
          pQMV10(I) = FLUX4(I)+UPWL(I,4)*ABS(FLUX4(I))
          pQMV11(I) = FLUX5(I)+UPWL(I,5)*ABS(FLUX5(I))
          pQMV12(I) = FLUX6(I)+UPWL(I,6)*ABS(FLUX6(I))                    
          FLU1(NFT+I) = pQMV7(I) + pQMV8(I) + pQMV9(I) + pQMV10(I) +pQMV11(I) + pQMV12(I)
        ENDDO           
        IF(ITRIMAT  > 0)THEN
          DO I=LFT,LLT
            DDVOL(I)=HALF*( FLU1(NFT+I)+ FLUX(NFT+I,1)+FLUX(NFT+I,2)+FLUX(NFT+I,3)+
     .                                   FLUX(NFT+I,4)+FLUX(NFT+I,5)+FLUX(NFT+I,6) )
          ENDDO
        ENDIF                
      ELSE
        DO I=LFT,LLT
          pQMV7(I)  = FLUX1(I)+UPWL(I,1)*ABS(FLUX1(I))
          pQMV8(I)  = FLUX2(I)+UPWL(I,2)*ABS(FLUX2(I))
          pQMV9(I)  = FLUX3(I)+UPWL(I,3)*ABS(FLUX3(I))
          pQMV10(I) = FLUX4(I)+UPWL(I,4)*ABS(FLUX4(I))                
          FLU1(NFT+I) = pQMV7(I) + pQMV8(I) + pQMV9(I) + pQMV10(I)
        ENDDO        
        IF(ITRIMAT  > 0)THEN
          DO I=LFT,LLT
            DDVOL(I)=HALF*( FLU1(NFT+I)+ FLUX(NFT+I,1)+FLUX(NFT+I,2)+
     .                                   FLUX(NFT+I,3)+FLUX(NFT+I,4) )
          ENDDO
        ENDIF        
      ENDIF
 
      !INTERFACE 22 ONLY - OUTPUT---------------! OBSOLETE
      debug_outp = .false.      
      if(ibug22_upwind /= 0)then
        if(ibug22_upwind  > 0)then
          do i=lft,llt
            if(ixs(11,I+nft) == ibug22_upwind)debug_outp=.true.
          enddo
        elseif(ibug22_upwind==-1)then
          debug_outp = .true.
        endif
        if(((itrimat /= ibug22_itrimat).and.(ibug22_itrimat /= -1)))debug_outp=.false.
      endif
!#!include "lockon.inc"      
      if(debug_outp)then
        print *, "    |----ale51_upwind3.F-----|"
        print *, "    |   THREAD INFORMATION   |"
        print *, "    |------------------------|" 
        print *, "    NCYCLE=", NCYCLE
        do i=lft,llt
          if(ibug22_upwind/=ixs(11,I+NFT).and.ibug22_upwind/=-1)cycle
          !if (tag22(i)==zero)print *,"       UNCUT"!cycle
          print *,                    "      brique    =", ixs(11,nft+i)
          print *,                    "      itrimat   =", itrimat
          write (*,FMT='(A,6E26.14)')"       Flux(1:6) =", FLUX(NFT+I,1:6)
          write (*,FMT='(A,1E26.14)')"       Flu1      =", FLU1(NFT+I)
          print *, "      ------------------------"          
        enddo
!#!include "lockoff.inc"       
      endif
      !-----------------------------------------!

C-----------------------------------------------
      RETURN
      END
C
