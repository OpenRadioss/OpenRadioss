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
!||    ale51_antidiff2        ../engine/source/ale/ale51/ale51_antidiff2.F
!||--- called by ------------------------------------------------------
!||    afluxt                 ../engine/source/ale/ale51/afluxt.F
!||--- uses       -----------------------------------------------------
!||    ale_connectivity_mod   ../common_source/modules/ale/ale_connectivity_mod.F
!||    ale_mod                ../common_source/modules/ale/ale_mod.F
!||    element_mod            ../common_source/modules/elements/element_mod.F90
!||    segvar_mod             ../engine/share/modules/segvar_mod.F
!||====================================================================
      SUBROUTINE ALE51_ANTIDIFF2(FLUX,flux_save,ALE_CONNECT,alpha_mat,s_alpha_mat,VOL,flux_vois_mat,
     .                   trimat,
     .                   SEGVAR,s_flux,nv46,s_flux_vois)
C-----------------------------------------------
C   D e c r i p t i o n
C-----------------------------------------------
C
C ** Warning, this formulation (for volume fractions) using \eta_3 parameter from /UPWIND option is not conservative. 
C It was marked as OBSOLETE. Forget it and use MUSCL instead
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
       USE SEGVAR_MOD     
       USE ALE_CONNECTIVITY_MOD
       USE ALE_MOD
       use precision_mod , only : WP
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "spmd_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      integer, intent(in) :: s_flux !< first dimension of FLUX array
      integer, intent(in) :: s_alpha_mat !< first dimension of alpha_mat array
      integer, intent(in) :: s_flux_vois !< first dimension of flux_vois_mat
      integer, intent(in) :: trimat !< number of material
      integer, intent(in) :: nv46 !< second dimension of FLUX & QMV arrays   
      real(kind=WP), dimension(s_alpha_mat,trimat), intent(inout) :: alpha_mat !< alpha array     
      real(kind=WP), dimension(s_flux,nv46), intent(in) :: flux_save !< saved flux
      my_real FLUX(s_flux,nv46,trimat),VOL(*),flux_vois_mat(s_flux_vois,nv46,trimat)
      TYPE(t_segvar),INTENT(IN) :: SEGVAR     
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      integer :: itrimat
      INTEGER I,II,K,JV(6),KV(6),IAD2,IAD3
      my_real
     .   VOL0,AV0(trimat),UAV0(trimat),ALPHI(trimat),UALPHI(trimat),AAA,FF(nv46,trimat),UDT,PHI0(trimat)
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------  
      JV = -HUGE(JV(1))
      KV = -HUGE(KV(1))
      FF = -HUGE(FF(1,1))
C-----------------------------------------------
C     WET SURFACE
C-----------------------------------------------
      IF(DT1 > ZERO)THEN
        UDT = ONE/DT1
      ELSE
        UDT = ZERO
      ENDIF
C
      DO I=LFT,LLT
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
C-----------------------------------------------
C       adjacent face & total outgoing volume flux 
C-----------------------------------------------
        DO K=1,nv46
          IF(flux_save(II,K) > ZERO)THEN
            JV(K) = ALE_CONNECT%ee_connect%connected(IAD2 + K - 1)
            KV(K) = K
            IF(JV(K) == 0)THEN
              JV(K)  = II
              do itrimat=1,trimat
                FF(K,itrimat)  = alpha_mat(ii,itrimat)*flux_save(II,K)
              enddo
            ELSEIF(JV(K) < 0)THEN
              do itrimat=1,trimat              
                !case <0 : ebcs (-IVOIS is then segment identifier)               
                FF(K,itrimat)  = SEGVAR%PHASE_ALPHA(ITRIMAT,-JV(K))  *flux_save(II,K)         
              enddo
            ELSEIF(JV(K) <= NUMELQ)THEN
               IAD3 = ALE_CONNECT%ee_connect%iad_connect(JV(K))
              IF(ALE_CONNECT%ee_connect%connected(IAD3 + 1 - 1) == II) KV(K) = 1
              IF(ALE_CONNECT%ee_connect%connected(IAD3 + 2 - 1) == II) KV(K) = 2
              IF(ALE_CONNECT%ee_connect%connected(IAD3 + 3 - 1) == II) KV(K) = 3
              IF(ALE_CONNECT%ee_connect%connected(IAD3 + 4 - 1) == II) KV(K) = 4
              do itrimat=1,trimat
                FF(K,itrimat)  = alpha_mat(JV(K),itrimat)*flux_save(II,K)
              enddo
            ELSE
              !spmd
              do itrimat=1,trimat
                FF(K,itrimat)  = alpha_mat(JV(K),itrimat)*flux_save(II,K)              
              enddo
            ENDIF
            do itrimat=1,trimat
              ! outgoing volume flux (estimation)
              ALPHI(itrimat)  = ALPHI(itrimat)  + FF(K,itrimat)
              ! initial outgoing flux
              PHI0(itrimat) = PHI0(itrimat) + flux_save(II,K) 
            enddo
          ENDIF
        ENDDO

        do itrimat=1,trimat
c         outgoing rest
          UALPHI(itrimat) = PHI0(itrimat) - ALPHI(itrimat) 
C-----------------------------------------------
C         outgoing volume flux by face
C-----------------------------------------------
          IF(ALPHI(itrimat) > AV0(itrimat).AND.AV0(itrimat) > ZERO)THEN
C-----------------------------------------------
C           outgoing volume flux > volume
C-----------------------------------------------
            AAA = AV0(itrimat) / ALPHI(itrimat)
            DO K=1,4
              IF(flux_save(II,K) > ZERO)THEN
                FF(K,itrimat) = FF(K,itrimat) * AAA
              ENDIF
            ENDDO
         ELSEIF(UALPHI(itrimat) > UAV0(itrimat).AND.UAV0(itrimat) > ZERO)THEN

C-----------------------------------------------
C         outgoing rest > available volume
C-----------------------------------------------
          AAA = UAV0(itrimat)/UALPHI(itrimat)
          DO K=1,4
            IF(flux_save(II,K) > ZERO)THEN
              FF(K,itrimat) = flux_save(II,K) + (FF(K,itrimat)-flux_save(II,K))*AAA
            ENDIF
          ENDDO
c        ELSE
        ENDIF
C-----------------------------------------------
C         outgoing flux
C-----------------------------------------------
          DO K=1,4
            IF(flux_save(II,K) > ZERO)THEN
              FF(K,itrimat) = 0.5* (FF(K,itrimat)*(1.-ALE%UPWIND%UPWSM)
     .                    + alpha_mat(II,itrimat)*flux_save(II,K)*(1.+ALE%UPWIND%UPWSM))
              FLUX(II,K,itrimat) = FF(K,itrimat)
              IF(JV(K) < 0)THEN
                !nothing to do
              ELSEIF(JV(K) <= NUMELQ)THEN
                FLUX(JV(K),KV(K),itrimat) = -FLUX(II,K,itrimat)
              ELSE
                flux_vois_mat(II,K,itrimat) = FLUX(ii,K,itrimat)
              ENDIF
            ENDIF
          ENDDO
        enddo
C-----------------------------------------------
C         incoming volume flux from EBCS
C-----------------------------------------------
        IF(NSEGFLU > 0)THEN
          IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
          DO K=1,4
            IF(flux_save(II,K) < ZERO .AND. ALE_CONNECT%ee_connect%connected(IAD2 + K - 1) < 0)THEN
              do itrimat = 1,trimat
                FLUX(II,K,itrimat) = SEGVAR%PHASE_ALPHA(ITRIMAT,-ALE_CONNECT%ee_connect%connected(IAD2 + K - 1))*flux_save(II,K)
              enddo
            ENDIF
          ENDDO       
        ENDIF   
C-------------
      ENDDO! next I
C-----------------------------------------------
      RETURN
      END
C
