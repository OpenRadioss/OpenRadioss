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
Chd|  ALE51_ANTIDIFF2               source/ale/ale51/ale51_antidiff2.F
Chd|-- called by -----------
Chd|        AFLUXT                        source/ale/ale51/afluxt.F     
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|====================================================================
      SUBROUTINE ALE51_ANTIDIFF2(FLUX,ALE_CONNECT,ALPH,VOL,IXQ,FLUX_VOIS,
     .                   N4_VOIS,ITAB,ITRIMAT,
     .                   SEGVAR)
C-----------------------------------------------
C   D e c r i p t i o n
C-----------------------------------------------
C
C ts  :Warning, this formulation (for volume fractions) using \eta_3 parameter from /UPWIND option is not conservative. It was marked as obsolete.
C ts : Forget it and use MUSCL instead
C
c gw     Traitement anti-diffusif multimateriau 51.
c        hypotheses de disposition du vide et de la phase
c              entre deux elements communiquants
c         en fonction du taux de remplissage volumique
c
c   +----------+----------+          +----------+----------+
c   |      ------->       |          |      ------->       |
c   +----------+----------+          +----------+----------+
c   |   a0     |    a1    |          |   a0     |    a1    |
c   +----------+----------+          +----------+----------+
c   |//////////|          |          |          |//////////|
c   |//////////|          |          |          |//////////|
c   |/// 1 ////|    0     | [1]      |    0     |/// 1 ////| [0]
c   |//////////|          |          |          |//////////|
c   |//////////|          |          |          |//////////|
c   +----------+----------+          +----------+----------+
c   |////////  |          |          |          |  ////////|
c   |////////  |          |          |          |  ////////|
c   |///0.8//  |    0     | [0;1[    |    0     |  /0.8////| [0]
c   |////////  |          |          |          |  ////////|
c   |////////  |          |          |          |  ////////|
c   +----------+----------+          +----------+----------+
c   |//////////|          |          |        //|//////////|
c   |//////////|          |          |        //|//////////|
c   |/// 1 ////|    0.2   | [1]      |   0.2  //|/// 1 ////| ]0;1]
c   |//////////|          |          |        //|//////////|
c   |//////////|//////////|          |        //|//////////|
c   +----------+----------+          +----------+----------+
c   |///////   |          |          |          |          |
c   |///////   |          |          |       ///|//////////|
c   |///0.8/   |    0.2   | [0.2;1[  |   0.2 ///|///0.8////| ]0;0.8]
c   |///////   |          |          |       ///|//////////|
c   |//////////|//////////|          |       ///|//////////|
c   +----------+----------+          +----------+----------+
c   |///       |          |          |          |          |  
c   |///       |          |          |   0.4    |    0.6   |
c   |///0.6    |    0.4   | [0.4;1[  |   ///////|//////////| ]0;0.6]
c   |//////////|//////////|          |   ///////|//////////|
c   |//////////|//////////|          |   ///////|//////////|
c   +----------+----------+          +----------+----------+
c
c   1- monodim (si sortant)
c
c     DV = min[ a0 V0, a2 phi01 dt + max[0,(1-a1)phi01 dt-(1-a0)V0]]
c     PHI01 = min[ a0 V0/dt, a1 phi01 + max[0,(1-a1)phi01-(1-a0)V0/dt]]
c
c
c   2- tridim
c
c     DVsortant tot = DVTe =
c        min[a0 V0,
c            Somme(ai phi0i dt) + max[0,
c                                     Somme((1-ai)phi0i dt)-(1-a0)V0]]
c
c     si DVsortant tot != Somme(PHI0i sortant) dt = DVTc
c         => correction sur sortants
c              PHI0i = PHI0i DVTe / DVTc
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
       USE SEGVAR_MOD     
       USE ALE_CONNECTIVITY_MOD
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXQ(NIXQ,NUMELQ),N4_VOIS(NUMELQ+NQVOIS,4),ITAB(NUMNOD)
      INTEGER,INTENT(IN) :: ITRIMAT
      my_real
     .   FLUX(4,*),ALPH(*),VOL(*),
     .   FLUX_VOIS(NUMELQ+NQVOIS,6)
      TYPE(t_segvar),INTENT(IN) :: SEGVAR     
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,K,KK,JV(6),KV(6),IAD2,IAD3
      my_real
     .   VOL0,AV0,UAV0,ALPHI,UALPHI,PHISE,PHISC,AAA,FF(6),UDT,PHI0,
     .   BBB,CCC,DDD,EEE
C-----------------------------------------------
C     SURFACE MOUILLEE PAR LA PHASE DU TRIMATERIAUX
C-----------------------------------------------
      IF(DT1.GT.ZERO)THEN
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
C       face voisine du voisin  
C       et flux total sortant 
C-----------------------------------------------
        DO K=1,4
          IF(FLUX(K,II).GT.ZERO)THEN
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
              IF(ALE_CONNECT%ee_connect%connected(IAD3 + 1 - 1).EQ.II) KV(K) = 1
              IF(ALE_CONNECT%ee_connect%connected(IAD3 + 2 - 1).EQ.II) KV(K) = 2
              IF(ALE_CONNECT%ee_connect%connected(IAD3 + 3 - 1).EQ.II) KV(K) = 3
              IF(ALE_CONNECT%ee_connect%connected(IAD3 + 4 - 1).EQ.II) KV(K) = 4
              FF(K)  = ALPH(JV(K))*FLUX(K,II)
            ELSE
              !spmd
              FF(K)  = ALPH(JV(K))*FLUX(K,II)              
            ENDIF
            
c           flux sortant estime
            ALPHI  = ALPHI  + FF(K)
c           flux sortant initial
            PHI0 = PHI0 + FLUX(K,II) 
          ENDIF
        ENDDO
c       vide sortant estime
        UALPHI = PHI0 - ALPHI 
C-----------------------------------------------
C       flux sortant par face
C-----------------------------------------------
        IF(ALPHI.GT.AV0.AND.AV0.GT.ZERO)THEN
C-----------------------------------------------
C         flux sortant > volume non vide
C-----------------------------------------------
          AAA = AV0 / ALPHI
          DO K=1,4
            IF(FLUX(K,II).GT.ZERO)THEN
              FF(K) = FF(K) * AAA
            ENDIF
          ENDDO
       ELSEIF(UALPHI.GT.UAV0.AND.UAV0.GT.ZERO)THEN

C-----------------------------------------------
C         vide sortant > vide disponible
C-----------------------------------------------
          AAA = UAV0/UALPHI
c          BBB = (UALPHI-UAV0)/UALPHI
          DO K=1,4
            IF(FLUX(K,II).GT.ZERO)THEN
              FF(K) = FLUX(K,II) + (FF(K)-FLUX(K,II))*AAA
            ENDIF
          ENDDO
c        ELSE
        ENDIF
C-----------------------------------------------
C         flux sortant 
C-----------------------------------------------
          DO K=1,4
            IF(FLUX(K,II).GT.ZERO)THEN
              FF(K) = 0.5* (FF(K)*(1.-UPWSM)
     .                    + ALPH(II)*FLUX(K,II)*(1.+UPWSM))
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
C         flux entrant par EBCS 
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
C-------------
      RETURN
      END
C
