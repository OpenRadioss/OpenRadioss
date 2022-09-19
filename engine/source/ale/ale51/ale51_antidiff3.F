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
Chd|  ALE51_ANTIDIFF3               source/ale/ale51/ale51_antidiff3.F
Chd|-- called by -----------
Chd|        AFLUXT                        source/ale/ale51/afluxt.F     
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|====================================================================
      SUBROUTINE ALE51_ANTIDIFF3(FLUX     , ALE_CONNECT  , ALPH, VOL , IXS,
     .                           FLUX_VOIS, N4_VOIS, ITAB, NV46, ITRIMAT,
     .                           SEGVAR)
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
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,NUMELS),N4_VOIS(NUMELS+NSVOIS,8),ITAB(NUMNOD),NV46,ITRIMAT
      my_real FLUX(NV46,*),ALPH(*),VOL(*),FLUX_VOIS(NUMELS+NSVOIS,NV46)
      TYPE(t_segvar),INTENT(IN) :: SEGVAR
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,K,JV(NV46),KV(NV46),IFV,NIN, ib, ie,ISKIP(MVSIZ),IEV,IAD2, IAD3
      my_real VOL0,AV0,UAV0,ALPHI,UALPHI,AAA,FF(NV46),UDT,PHI0,facev
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
         if(((itrimat /= ibug22_itrimat).and.(ibug22_itrimat /= -1)))debug_outp=.false.
       endif
       if(debug_outp)then
         print *, "    |----ale51_antidiff3.F-----|"
         print *, "    |     THREAD INFORMATION   |"
         print *, "    |--------------------------|"
         print *, "    NCYCLE  =", NCYCLE
         print *, "    ITRIMAT =", ITRIMAT
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
        AV0  = ALPH(II) * VOL0
        UAV0 = VOL0 - AV0
        ALPHI  = ZERO
        UALPHI = ZERO
        PHI0 = ZERO
        !-----------------------------------------------
        !       face number from adjacent elem
        !       & total volume flux (outgoing)
        !-----------------------------------------------
        DO K=1,NV46
          IF(FLUX(K,II) > ZERO)THEN
            JV(K) = ALE_CONNECT%ee_connect%connected(IAD2 + K - 1)
            KV(K) = K
            IF(JV(K) == 0)THEN
              JV(K)  = II
              FF(K)  = ALPH(II)*FLUX(K,II)
            ELSEIF(JV(K) < 0)THEN
              !case <0 : ebcs (-IVOIS is then segment identifier)
              FF(K)  = SEGVAR%PHASE_ALPHA(ITRIMAT,-JV(K)) * FLUX(K,II)
            ELSEIF(JV(K) <= NUMELS)THEN
               IAD3 = ALE_CONNECT%ee_connect%iad_connect(JV(K))
              DO J=1,NV46
                IF(ALE_CONNECT%ee_connect%connected(IAD3 + J - 1) == II) KV(K) = J
              ENDDO
              FF(K)  = ALPH(JV(K))*FLUX(K,II)
            ELSE
              !spmd
              FF(K)  = ALPH(JV(K))*FLUX(K,II)
            ENDIF
            !outgoing volume flux (estimation)
            ALPHI  = ALPHI  + FF(K)
            !outgoing volume flux (initial)
            PHI0 = PHI0 + FLUX(K,II)
          ENDIF
        ENDDO
        !outgoing rest (estimation)
        UALPHI = PHI0 - ALPHI
        !-----------------------------------------------
        !       outgoing volume flux (by face)
        !-----------------------------------------------
        IF(ALPHI > AV0.AND.AV0 > ZERO)THEN
          !-----------------------------------------------
          !      outgoing volume flux > volume
          !-----------------------------------------------
          AAA = AV0 / ALPHI
          DO K=1,NV46
            IF(FLUX(K,II) > ZERO)THEN
              FF(K) = FF(K) * AAA
            ENDIF
          ENDDO
        ELSEIF(UALPHI > UAV0.AND.UAV0 > ZERO)THEN
          !-----------------------------------------------
          !      outgoing rest > available
          !-----------------------------------------------
          AAA = UAV0/UALPHI
          DO K=1,NV46
            IF(FLUX(K,II) > ZERO)THEN
              FF(K) = FLUX(K,II) + (FF(K)-FLUX(K,II))*AAA
            ENDIF
          ENDDO
        ENDIF
        !-----------------------------------------------
        !         outgoing volume fluxes
        !-----------------------------------------------
        DO K=1,NV46
          IF(FLUX(K,II) > ZERO)THEN
            FF(K) = HALF * ( FF(K)*(ONE-UPWSM)+ALPH(II)*FLUX(K,II)*(ONE+UPWSM) )

            !INTERFACE 22 ONLY------------------------! (OBSOLETE)
            if(int22>0)then
            if(debug_outp)then
            if(ibug22_antidiff == ixs(11,i+nft) .OR. ibug22_antidiff == -1)then
              print *,              "      brique       =", ixs(11,i+nft)
              print *,              "      FACE         =", K
        write (*,FMT='(A,6E26.14)')"       WAS  Flux(J) =", FLUX(K,II)
        write (*,FMT='(A,6E26.14)')"       IS  Flux(J)  =", FF(K)
              print *, "      ------------------------"
            endif
            endif
            endif
            !-----------------------------------------!


            FLUX(K,II) = FF(K)

            IF(JV(K) < 0)THEN
              ! do nothing
            ELSEIF(JV(K) <= NUMELS)THEN
              IF(INT22 == 0)THEN
                debug_tmp         = FLUX(KV(K),JV(K))
                FLUX(KV(K),JV(K)) = -FLUX(K,II)
              ELSE
                IF(IIAD22(NIN,JV(K))==0)THEN
                !NOT IN CUT CELL BUFFER(INTER22)
                  debug_tmp         =  FLUX(KV(K),JV(K))
                  FLUX(KV(K),JV(K)) = -FLUX(K,II)
                ELSE
                !IN CUT CELL BUFFER (INTER22)
                  NIN = 1
                  iFV = KV(K)
                  !(icellv,iadjv) = (1,1) since it is supposed to be an uncut cell from cut cell buffer (TZINF ensures this)
                  debug_tmp = BRICK_LIST(NIN,IIAD22(NIN,JV(K)))%POLY(1)%FACE(IFv)%Adjacent_UpwFLUX(1)
                  BRICK_LIST(NIN,IIAD22(NIN,JV(K)))%POLY(1)%FACE(IFv)%Adjacent_UpwFLUX(1) = -FLUX(K,II)
                ENDIF
              ENDIF
            ELSE
              FLUX_VOIS(II,K) = FLUX(K,II)     !Sign is updated in AFLUX3TA after calling SPMD_I8VOIS
              N4_VOIS(II,1) = ITAB(IXS(2,II))
              N4_VOIS(II,2) = ITAB(IXS(3,II))
              N4_VOIS(II,3) = ITAB(IXS(4,II))
              N4_VOIS(II,4) = ITAB(IXS(5,II))
              N4_VOIS(II,5) = ITAB(IXS(6,II))
              N4_VOIS(II,6) = ITAB(IXS(7,II))
              N4_VOIS(II,7) = ITAB(IXS(8,II))
              N4_VOIS(II,8) = ITAB(IXS(9,II))
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
     .                "         IS  Flux(J)  =", -FLUX(K,II)
              print *, "        ---"
            endif
            endif
            endif
            !-----------------------------------------!

          ENDIF
        ENDDO
C-----------------------------------------------
C         incoming volume flux from EBCS
C-----------------------------------------------
          IF(NSEGFLU > 0)THEN
             IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
            DO K=1,NV46
              IF(FLUX(K,II) < ZERO .AND. ALE_CONNECT%ee_connect%connected(IAD2 + K - 1) < 0)THEN
                FLUX(K,II) = SEGVAR%PHASE_ALPHA(ITRIMAT,-ALE_CONNECT%ee_connect%connected(IAD2 + K - 1))*FLUX(K,II)
              ENDIF
            ENDDO
          ENDIF

C-------------
      ENDDO!next I
C-----------------------------------------------
      RETURN
      END
C
