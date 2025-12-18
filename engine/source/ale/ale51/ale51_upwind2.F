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
!||    ale51_upwind2          ../engine/source/ale/ale51/ale51_upwind2.F
!||--- called by ------------------------------------------------------
!||    afluxt                 ../engine/source/ale/ale51/afluxt.F
!||    ale51_finish           ../engine/source/ale/ale51/ale51_finish.F
!||    ale51_init             ../engine/source/ale/ale51/ale51_init.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod   ../common_source/modules/ale/ale_connectivity_mod.F
!||    element_mod            ../common_source/modules/elements/element_mod.F90
!||====================================================================
      SUBROUTINE ALE51_UPWIND2(PM,X,IXQ,FLUX,FLU1,ALE_CONNECT,
     .               ITRIMAT,DDVOL,QMV,IFLG,nv46,s_flux,s_qmv,
     .               nb_element_with_bc,list_element_with_bc)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_CONNECTIVITY_MOD
      use element_mod , only : nixq
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
      integer, intent(in) :: nv46 !< second dimension of FLUX & QMV arrays
      integer, intent(in) :: nb_element_with_bc !< number of element with boundary conditions
      integer, dimension(nb_element_with_bc), intent(in) :: list_element_with_bc !< list of element with boundary conditions      
      INTEGER IXQ(NIXQ,NUMELQ), ITRIMAT,IFLG,IAD2
      my_real PM(NPROPM,NUMMAT), FLUX(s_flux,nv46), FLU1(s_flux),DDVOL(*),QMV(s_qmv,2*NV46), X(3,NUMNOD)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,MAT(MVSIZ)
      integer :: ijk
      integer :: mat_id
      logical :: only_one_mat      
      my_real REDUC,QMV5,QMV6,QMV7,QMV8,
     .        FLUX1(MVSIZ), FLUX2(MVSIZ), FLUX3(MVSIZ), FLUX4(MVSIZ),
     .        UPWL(MVSIZ,4), R1, R2, R3, R4
C-----------------------------------------------
      DO I=LFT,LLT
        II = NFT + I
        FLUX1(I) = FLUX(II,1)
        FLUX2(I) = FLUX(II,2)
        FLUX3(I) = FLUX(II,3)
        FLUX4(I) = FLUX(II,4)
      ENDDO
C-----------------------------------------------
      only_one_mat = .true.
      II=1+NFT
      MAT(1)=IXQ(1,II)      
      mat_id = MAT(1)
      DO I=LFT+1,LLT
        II=I+NFT
        MAT(I)=IXQ(1,II)
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

C
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
          endif
        enddo
      enddo
C     
      IF(IFLG == 1)THEN
         IF (N2D  ==  1) THEN
            !!! Planar integration of momentum when 2D axi
            DO I=LFT,LLT
               II = I + NFT           
               FLUX(II,1) = FLUX1(I)-UPWL(I,1)*ABS(FLUX1(I))
               FLUX(II,2) = FLUX2(I)-UPWL(I,2)*ABS(FLUX2(I))
               FLUX(II,3) = FLUX3(I)-UPWL(I,3)*ABS(FLUX3(I))
               FLUX(II,4) = FLUX4(I)-UPWL(I,4)*ABS(FLUX4(I))
C     
               !!! Divide by mean radius
               QMV(II,5)  = FLUX1(I)+UPWL(I,1)*ABS(FLUX1(I))
               QMV(II,6)  = FLUX2(I)+UPWL(I,2)*ABS(FLUX2(I))
               QMV(II,7)  = FLUX3(I)+UPWL(I,3)*ABS(FLUX3(I))
               QMV(II,8)  = FLUX4(I)+UPWL(I,4)*ABS(FLUX4(I))
C                                   
               FLU1(ii) = QMV(II,5)  +  QMV(II,6)  +  QMV(II,7)  +  QMV(II,8)

               R1 = HALF * (X(2, IXQ(2, II)) + X(2, IXQ(3, II)))
               R2 = HALF * (X(2, IXQ(3, II)) + X(2, IXQ(4, II)))
               R3 = HALF * (X(2, IXQ(4, II)) + X(2, IXQ(5, II)))
               R4 = HALF * (X(2, IXQ(5, II)) + X(2, IXQ(2, II)))
               
               !!! Beware of r=0 axis
               IF (R1 /= ZERO) QMV(II,5) = QMV(II,5) / R1
               IF (R2 /= ZERO) QMV(II,6) = QMV(II,6) / R2
               IF (R3 /= ZERO) QMV(II,7) = QMV(II,7) / R3
               IF (R4 /= ZERO) QMV(II,8) = QMV(II,8) / R4
            ENDDO
         ELSE
            DO I=LFT,LLT
              II = I + NFT
              FLUX(II,1) = FLUX1(I)-UPWL(I,1)*ABS(FLUX1(I))
              FLUX(II,2) = FLUX2(I)-UPWL(I,2)*ABS(FLUX2(I))
              FLUX(II,3) = FLUX3(I)-UPWL(I,3)*ABS(FLUX3(I))
              FLUX(II,4) = FLUX4(I)-UPWL(I,4)*ABS(FLUX4(I))
               
              QMV(II,5)  = FLUX1(I)+UPWL(I,1)*ABS(FLUX1(I))
              QMV(II,6)  = FLUX2(I)+UPWL(I,2)*ABS(FLUX2(I))
              QMV(II,7)  = FLUX3(I)+UPWL(I,3)*ABS(FLUX3(I))
              QMV(II,8)  = FLUX4(I)+UPWL(I,4)*ABS(FLUX4(I))
                 
              FLU1(ii) = QMV(II,5)  +  QMV(II,6)  +  QMV(II,7)  +  QMV(II,8)
            ENDDO
         ENDIF
      ELSE
         DO I=LFT,LLT
            II = I + NFT          
            FLUX(II,1) = FLUX1(I)-UPWL(I,1)*ABS(FLUX1(I))
            FLUX(II,2) = FLUX2(I)-UPWL(I,2)*ABS(FLUX2(I))
            FLUX(II,3) = FLUX3(I)-UPWL(I,3)*ABS(FLUX3(I))
            FLUX(II,4) = FLUX4(I)-UPWL(I,4)*ABS(FLUX4(I))

            QMV5  = FLUX1(I)+UPWL(I,1)*ABS(FLUX1(I))
            QMV6  = FLUX2(I)+UPWL(I,2)*ABS(FLUX2(I))
            QMV7  = FLUX3(I)+UPWL(I,3)*ABS(FLUX3(I))
            QMV8  = FLUX4(I)+UPWL(I,4)*ABS(FLUX4(I))

            FLU1(ii) = QMV5  +  QMV6  +  QMV7  +  QMV8
         ENDDO
      END IF
C
      IF(ITRIMAT > 0)THEN
        DO I=LFT,LLT
          II = I + NFT          
          DDVOL(I)=HALF * ( FLU1(ii)+FLUX(II,1)+FLUX(II,2)+FLUX(II,3)+FLUX(II,4) )
        ENDDO
      ENDIF
C----------------------------------------------- 
      RETURN
      END
C
