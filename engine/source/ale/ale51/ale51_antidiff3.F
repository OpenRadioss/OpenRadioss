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
!||    ale51_antidiff3        ../engine/source/ale/ale51/ale51_antidiff3.F
!||--- called by ------------------------------------------------------
!||    afluxt                 ../engine/source/ale/ale51/afluxt.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod   ../common_source/modules/ale/ale_connectivity_mod.F
!||    ale_mod                ../common_source/modules/ale/ale_mod.F
!||    element_mod            ../common_source/modules/elements/element_mod.F90
!||    i22bufbric_mod         ../common_source/modules/interfaces/cut-cell-search_mod.F
!||    i22tri_mod             ../common_source/modules/interfaces/cut-cell-search_mod.F
!||    segvar_mod             ../engine/share/modules/segvar_mod.F
!||====================================================================
      SUBROUTINE ALE51_ANTIDIFF3(FLUX     ,flux_save, ALE_CONNECT  , alpha_mat,s_alpha_mat, VOL , IXS,
     .                           flux_vois_mat,NV46, trimat,
     .                           SEGVAR   ,s_flux,s_flux_vois)
C-----------------------------------------------
C   D e c r i p t i o n
C-----------------------------------------------
C
C ** Warning, this formulation (for volume fractions) using \eta_3 parameter from /UPWIND option is not conservative. 
C It was marked as obsolete. Forget it and use MUSCL instead
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE I22BUFBRIC_MOD 
      USE I22TRI_MOD 
      USE SEGVAR_MOD     
      USE ALE_CONNECTIVITY_MOD
      USE ALE_MOD
      use element_mod , only : nixs
      use precision_mod , only : WP
      use debug_mod , only : ncycle_debug
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
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      integer, intent(in) :: s_flux !< first dimension of FLUX array
      integer, intent(in) :: s_alpha_mat !< first dimension of alpha_mat array
      integer, intent(in) :: s_flux_vois !< first dimension of flux_vois_mat
      integer, intent(in) :: trimat !< number of material      
      INTEGER IXS(NIXS,NUMELS),NV46
      real(kind=WP), dimension(s_flux,nv46), intent(in) :: flux_save !< saved flux
      my_real FLUX(s_flux,NV46,trimat),VOL(*),flux_vois_mat(s_flux_vois,NV46,trimat)
      real(kind=WP), dimension(s_alpha_mat,trimat), intent(inout) :: alpha_mat !< alpha array
      TYPE(t_segvar),INTENT(IN) :: SEGVAR
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,K,JV(NV46),KV(NV46),IFV,NIN, ib, ie,ISKIP(MVSIZ),IEV,IAD2, IAD3
      integer :: itrimat
      my_real VOL0,AV0(trimat),UAV0(trimat),ALPHI(trimat),UALPHI(trimat),AAA,FF(NV46,trimat),UDT,PHI0(trimat),facev
      my_real :: debug_tmp
      LOGICAL :: debug_outp     
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
     
      !----------------------------------------------
      !     WET SURFACE
      !-----------------------------------------------
      IF(DT1 > ZERO)THEN
        UDT = ONE/DT1
      ELSE
        UDT = ZERO
      ENDIF
      
      !INTER22
      NIN = 1

      !INTERFACE 22 ONLY - OUTPUT---------------!
      debug_outp = .false.
      if(int22>0)then
       if(ibug22_antidiff /= 0)then
         debug_outp = .false.
         if(ibug22_antidiff > 0)then
           do i=lft,llt
             ie=i+nft
             if(ixs(11,ie) == ibug22_antidiff)then
               debug_outp=.true.
               exit
             endif
           enddo
         elseif(ibug22_antidiff==-1)then
           debug_outp = .true.
         endif         
!         if(((itrimat /= ibug22_itrimat).and.(ibug22_itrimat /= -1)))debug_outp=.false.
       endif
       if(debug_outp)then
         print *, "    |----ale51_antidiff3.F-----|"
         print *, "    |     THREAD INFORMATION   |"
         print *, "    |--------------------------|" 
         print *, "    NCYCLE  =", NCYCLE_debug      
         !print *, "    ITRIMAT =", ITRIMAT         
       endif
      endif

      !INTER22 : skip bricks in cut cell buffer
      NIN = 1
      ISKIP(1:MVSIZ) = ZERO
      IF(INT22>0)THEN
        DO I=LFT,LLT      
          IB=IIAD22(NIN,I+NFT)
          IF(IB /= 0)ISKIP(I)=1
        ENDDO
      ENDIF  

      DO I=LFT,LLT
        IF(ISKIP(I)==1)CYCLE !unplug for cut cells.
        II   = I + NFT
        IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
        VOL0 = VOL(I)*UDT
        do itrimat=1,trimat
          AV0(itrimat)  = alpha_mat(ii,itrimat) * VOL0
          UAV0(itrimat) = VOL0 - AV0(itrimat)
          ALPHI(itrimat)  = ZERO
          UALPHI(itrimat) = ZERO 
          PHI0(itrimat) = ZERO
        enddo 
        !-----------------------------------------------
        !       face number from adjacent elem  
        !       & total volume flux (outgoing)
        !-----------------------------------------------
        DO K=1,NV46
          IF(FLUX_save(II,K) > ZERO)THEN
            JV(K) = ALE_CONNECT%ee_connect%connected(IAD2 + K - 1)
            KV(K) = K
            IF(JV(K) == 0)THEN
              JV(K)  = II
              do itrimat=1,trimat
                FF(K,itrimat)  =alpha_mat(ii,itrimat)*FLUX_save(II,K)
              enddo
            ELSEIF(JV(K) < 0)THEN
              !case <0 : ebcs (-IVOIS is then segment identifier)
              do itrimat=1,trimat              
                FF(K,itrimat)  = SEGVAR%PHASE_ALPHA(ITRIMAT,-JV(K)) * FLUX_save(II,K)
              enddo
            ELSEIF(JV(K) <= NUMELS)THEN
              IAD3 = ALE_CONNECT%ee_connect%iad_connect(JV(K))
              DO J=1,NV46
                IF(ALE_CONNECT%ee_connect%connected(IAD3 + J - 1) == II) KV(K) = J
              ENDDO
              do itrimat=1,trimat              
                FF(K,itrimat)  = alpha_mat(JV(K),itrimat)*FLUX_save(II,K)
              enddo
            ELSE
              !spmd
              do itrimat=1,trimat
                FF(K,itrimat)  = alpha_mat(JV(K),itrimat)*FLUX_save(II,K)
              enddo
            ENDIF
            !outgoing volume flux (estimation)
            do itrimat=1,trimat
              ALPHI(itrimat)  = ALPHI(itrimat)  + FF(K,itrimat)
              !outgoing volume flux (initial)
              PHI0(itrimat) = PHI0(itrimat) + FLUX_save(II,K) 
            enddo
          ENDIF
        ENDDO
        do itrimat=1,trimat
          !outgoing rest (estimation)
          UALPHI(itrimat) = PHI0(itrimat) - ALPHI(itrimat)
          !-----------------------------------------------
          !       outgoing volume flux (by face)
          !-----------------------------------------------
          IF(ALPHI(itrimat) > AV0(itrimat).AND.AV0(itrimat) > ZERO)THEN
            !-----------------------------------------------
            !      outgoing volume flux > volume
            !-----------------------------------------------
            AAA = AV0(itrimat) / ALPHI(itrimat)
            DO K=1,NV46
              IF(FLUX_save(II,K) > ZERO)THEN
                FF(K,itrimat) = FF(K,itrimat) * AAA
              ENDIF
            ENDDO
          ELSEIF(UALPHI(itrimat) > UAV0(itrimat).AND.UAV0(itrimat) > ZERO)THEN
            !-----------------------------------------------
            !      outgoing rest > available
            !-----------------------------------------------
            AAA = UAV0(itrimat)/UALPHI(itrimat)
            DO K=1,NV46
              IF(FLUX_save(II,K) > ZERO)THEN
                FF(K,itrimat) = FLUX_save(II,K) + (FF(K,itrimat)-FLUX_save(II,K))*AAA
              ENDIF
            ENDDO
          ENDIF
          !-----------------------------------------------
          !         outgoing volume fluxes
          !-----------------------------------------------
          DO K=1,NV46
            IF(FLUX_save(II,K) > ZERO)THEN
              FF(K,itrimat) = HALF * ( FF(K,itrimat)*
     .                         (ONE-ALE%UPWIND%UPWSM)+alpha_mat(II,itrimat)*FLUX_save(II,K)*(ONE+ALE%UPWIND%UPWSM) )

              !INTERFACE 22 ONLY------------------------! (OBSOLETE)
              if(int22>0)then
              if(debug_outp)then
              if(ibug22_antidiff == ixs(11,i+nft) .OR. ibug22_antidiff == -1)then          
                print *,              "      brique       =", ixs(11,i+nft)             
                print *,              "      FACE         =", K
          write (*,FMT='(A,6E26.14)')"       WAS  Flux(J) =", FLUX_save(II,K)
          write (*,FMT='(A,6E26.14)')"       IS  Flux(J)  =", FF(K,itrimat)             
                print *, "      ------------------------"               
              endif
              endif
              endif
              !-----------------------------------------!      


              FLUX(II,K,itrimat) = FF(K,itrimat)

              IF(JV(K) < 0)THEN
                ! do nothing
              ELSEIF(JV(K) <= NUMELS)THEN
                IF(INT22 == 0)THEN
                  debug_tmp         = FLUX(JV(K),KV(K),itrimat)
                  FLUX(JV(K),KV(K),itrimat) = -FLUX(II,K,itrimat)              
                ELSE              
                  IF(IIAD22(NIN,JV(K))==0)THEN
                  !NOT IN CUT CELL BUFFER(INTER22)
                    debug_tmp         =  FLUX(JV(K),KV(K),itrimat)
                    FLUX(JV(K),KV(K),itrimat) = -FLUX(II,K,itrimat)
                  ELSE
                  !IN CUT CELL BUFFER (INTER22)
                    NIN = 1
                    iFV = KV(K)               
                    !(icellv,iadjv) = (1,1) since it is supposed to be an uncut cell from cut cell buffer (TZINF ensures this)
                    debug_tmp = BRICK_LIST(NIN,IIAD22(NIN,JV(K)))%POLY(1)%FACE(IFv)%Adjacent_UpwFLUX(1)
                    BRICK_LIST(NIN,IIAD22(NIN,JV(K)))%POLY(1)%FACE(IFv)%Adjacent_UpwFLUX(1) = -FLUX(II,K,itrimat)
                  ENDIF
                ENDIF
              ELSE
                flux_vois_mat(II,K,itrimat) = FLUX(II,K,itrimat)     !Sign is updated in AFLUX3TA after calling SPMD_I8VOIS
              ENDIF
              
              !INTERFACE 22 ONLY------------------------!
              if(int22>0)then
              if(debug_outp)then 
              if(ibug22_antidiff == ixs(11,i+nft) .OR. ibug22_antidiff == -1)then                        
                iev = jv(k)
                facev=kv(k)
                print *, "        => Setting adjacent flux consequently :"
                print *, "        brique.V=", ixs(11,iev)              
                print *, "        FACE.V       =", IFv
                write (*,FMT='(A,6E26.14)')      
     .                "         WAS  Flux(J) =", debug_tmp
                write (*,FMT='(A,6E26.14)')      
     .                "         IS  Flux(J)  =", -FLUX(II,K,itrimat)             
                print *, "        ---"               
              endif
              endif
              endif
              !-----------------------------------------! 
              
            ENDIF
          ENDDO
        enddo           
C-----------------------------------------------
C           incoming volume flux from EBCS
C-----------------------------------------------
        IF(NSEGFLU > 0)THEN
          IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
          DO K=1,NV46
            IF(FLUX_save(II,K) < ZERO .AND. ALE_CONNECT%ee_connect%connected(IAD2 + K - 1) < 0)THEN
              do itrimat=1,trimat
                FLUX(II,K,itrimat) = SEGVAR%PHASE_ALPHA(ITRIMAT,-ALE_CONNECT%ee_connect%connected(IAD2 + K - 1))*FLUX_save(II,K)
              enddo
            ENDIF
          ENDDO       
        ENDIF     
C-------------
      ENDDO!next I
C-----------------------------------------------
      RETURN
      END
C
