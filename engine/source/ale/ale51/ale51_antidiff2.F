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
Chd|  ALE51_ANTIDIFF2               source/ale/ale51/ale51_antidiff2.F
Chd|-- called by -----------
Chd|        AFLUXT                        source/ale/ale51/afluxt.F     
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|====================================================================
      SUBROUTINE ALE51_ANTIDIFF2(FLUX,ALE_CONNECT,ALPH,VOL,IXQ,FLUX_VOIS,
     .                   N4_VOIS,ITAB,ITRIMAT,
     .                   SEGVAR)
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
      INTEGER IXQ(NIXQ,NUMELQ),N4_VOIS(NUMELQ+NQVOIS,4),ITAB(NUMNOD)
      INTEGER,INTENT(IN) :: ITRIMAT
      my_real FLUX(4,*),ALPH(*),VOL(*),FLUX_VOIS(NUMELQ+NQVOIS,6)
      TYPE(t_segvar),INTENT(IN) :: SEGVAR     
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,K,JV(6),KV(6),IAD2,IAD3
      my_real
     .   VOL0,AV0,UAV0,ALPHI,UALPHI,PHISE,AAA,FF(6),UDT,PHI0
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------  
   
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
        AV0  = ALPH(II) * VOL0
        UAV0 = VOL0 - AV0
        ALPHI  = ZERO
        UALPHI = ZERO 
        PHI0 = ZERO  
C-----------------------------------------------
C       adjacent face & total outgoing volume flux 
C-----------------------------------------------
        DO K=1,4
          IF(FLUX(K,II) > ZERO)THEN
            JV(K) = ALE_CONNECT%ee_connect%connected(IAD2 + K - 1)
            KV(K) = K
            IF(JV(K) == 0)THEN
              JV(K)  = II
              FF(K)  = ALPH(II)*FLUX(K,II)
            ELSEIF(JV(K) < 0)THEN
              !case <0 : ebcs (-IVOIS is then segment identifier)               
              FF(K)  = SEGVAR%PHASE_ALPHA(ITRIMAT,-JV(K))  *FLUX(K,II)         
            ELSEIF(JV(K) <= NUMELQ)THEN
               IAD3 = ALE_CONNECT%ee_connect%iad_connect(JV(K))
              IF(ALE_CONNECT%ee_connect%connected(IAD3 + 1 - 1) == II) KV(K) = 1
              IF(ALE_CONNECT%ee_connect%connected(IAD3 + 2 - 1) == II) KV(K) = 2
              IF(ALE_CONNECT%ee_connect%connected(IAD3 + 3 - 1) == II) KV(K) = 3
              IF(ALE_CONNECT%ee_connect%connected(IAD3 + 4 - 1) == II) KV(K) = 4
              FF(K)  = ALPH(JV(K))*FLUX(K,II)
            ELSE
              !spmd
              FF(K)  = ALPH(JV(K))*FLUX(K,II)              
            ENDIF
c           outgoing volume flux (estimation)
            ALPHI  = ALPHI  + FF(K)
c           initial outgoing flux
            PHI0 = PHI0 + FLUX(K,II) 
          ENDIF
        ENDDO
c       outgoing rest
        UALPHI = PHI0 - ALPHI 
C-----------------------------------------------
C       outgoing volume flux by face
C-----------------------------------------------
        IF(ALPHI > AV0.AND.AV0 > ZERO)THEN
C-----------------------------------------------
C         outgoing volume flux > volume
C-----------------------------------------------
          AAA = AV0 / ALPHI
          DO K=1,4
            IF(FLUX(K,II) > ZERO)THEN
              FF(K) = FF(K) * AAA
            ENDIF
          ENDDO
       ELSEIF(UALPHI > UAV0.AND.UAV0 > ZERO)THEN

C-----------------------------------------------
C         outgoing rest > available volume
C-----------------------------------------------
          AAA = UAV0/UALPHI
          DO K=1,4
            IF(FLUX(K,II) > ZERO)THEN
              FF(K) = FLUX(K,II) + (FF(K)-FLUX(K,II))*AAA
            ENDIF
          ENDDO
c        ELSE
        ENDIF
C-----------------------------------------------
C         outgoing flux
C-----------------------------------------------
          DO K=1,4
            IF(FLUX(K,II) > ZERO)THEN
              FF(K) = 0.5* (FF(K)*(1.-ALE%UPWIND%UPWSM)
     .                    + ALPH(II)*FLUX(K,II)*(1.+ALE%UPWIND%UPWSM))
              FLUX(K,II) = FF(K)
              IF(JV(K) < 0)THEN
                !nothing to do
              ELSEIF(JV(K) <= NUMELQ)THEN
                FLUX(KV(K),JV(K)) = -FLUX(K,II)
              ELSE
                FLUX_VOIS(II,K) = FLUX(K,II)
                N4_VOIS(II,1) = ITAB(IXQ(2,II))
                N4_VOIS(II,2) = ITAB(IXQ(3,II))
                N4_VOIS(II,3) = ITAB(IXQ(4,II))
                N4_VOIS(II,4) = ITAB(IXQ(5,II))
              ENDIF
            ENDIF
          ENDDO
C-----------------------------------------------
C         incoming volume flux from EBCS
C-----------------------------------------------
          IF(NSEGFLU > 0)THEN
             IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
            DO K=1,4
              IF(FLUX(K,II) < ZERO .AND. ALE_CONNECT%ee_connect%connected(IAD2 + K - 1) < 0)THEN
                FLUX(K,II) = SEGVAR%PHASE_ALPHA(ITRIMAT,-ALE_CONNECT%ee_connect%connected(IAD2 + K - 1))*FLUX(K,II)
              ENDIF
            ENDDO       
          ENDIF   
C-------------
      ENDDO! next I
C-----------------------------------------------
      RETURN
      END
C
