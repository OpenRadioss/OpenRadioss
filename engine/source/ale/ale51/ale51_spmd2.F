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
Chd|  ALE51_SPMD2                   source/ale/ale51/ale51_spmd2.F
Chd|-- called by -----------
Chd|        AFLUXT                        source/ale/ale51/afluxt.F     
Chd|-- calls ---------------
Chd|        SYSFUS2                       source/system/sysfus.F        
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE ALE51_SPMD2(IXQ      ,FLUX   ,FLU1  ,ALE_CONNECT,
     .                       FLUX_VOIS,N4_VOIS,ITABM1,NV46)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
       USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "spmd_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXQ(NIXQ,NUMELQ), N4_VOIS(NUMELQ+NQVOIS,4), ITABM1(*),NV46
      my_real FLUX(4,*), FLU1(*),FLUX_VOIS(NUMELQ+NQVOIS,NV46)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,K,KK,L,IV,NN,IPERM(2,4),TAG(0:NUMNOD), NI(2), IAD2
      my_real M1E20
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER SYSFUS2
C-----------------------------------------------
      DATA IPERM/1,2,
     .           2,3,
     .           3,4,
     .           4,1/
      M1E20 = -EP20
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
      DO I=LFT,LLT
          II = I+NFT
          DO L=1,4
            TAG(IXQ(1+L,II)) = 0
          ENDDO
      ENDDO

      DO I=LFT,LLT
          II = I+NFT
          IAD2 = ALE_CONNECT%ee_connect%iad_connect(II)
          DO K=1,4
              IV = ALE_CONNECT%ee_connect%connected(IAD2 + K - 1)
              IF(IV > NUMELS)THEN
                DO KK=1,4
                 IF(N4_VOIS(IV,1) /= 0.AND.
     .              FLUX_VOIS(IV,KK) /= M1E20)THEN
                  NN =0
                  DO L=1,2
                    NI(L) = SYSFUS2(N4_VOIS(IV,IPERM(L,KK)),ITABM1,
     .                              NUMNOD                        )                    
                    TAG(NI(L)) = 1
                  ENDDO
                  DO L=1,2
                    NN=NN+TAG(IXQ(1+IPERM(L,K),II))
                  ENDDO
                  DO L=1,2
                    TAG(NI(L)) = 0                    
                  ENDDO
                  IF(NN == 2)THEN
                    FLUX(K,I)=-FLUX_VOIS(IV,KK)
                    EXIT !KK
                  ENDIF
                 ENDIF
                ENDDO
              ENDIF
          ENDDO
      ENDDO
C
C-----------------------------------------------
      RETURN
      END
C
