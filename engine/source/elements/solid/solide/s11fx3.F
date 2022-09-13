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
Chd|  S11FX3                        source/elements/solid/solide/s11fx3.F
Chd|-- called by -----------
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE S11FX3(PM,FLUX,ALE_CONNECT,IXS,IPM,BUFMAT)
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "scr06_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .  PM(NPROPM,*),FLUX(6,*),BUFMAT(*)
      INTEGER
     .  IXS(NIXS,*),IPM(NPROPMI,*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,JJ,II,K,IFLG,IADBUF,IAD2
C-----------------------------------------------
C     Enthalpie entrante par loi 11
C-----------------------------------------------
      IF(JEUL+JALE==1)THEN
          DO I=LFT,LLT
             IAD2 = ALE_CONNECT%ee_connect%iad_connect(I + NFT)
            DO K=1,6
            JJ = ALE_CONNECT%ee_connect%connected(IAD2 + K - 1)
            IF(JJ>0)THEN         ! negatif si ebcs
              J = IXS(1,JJ)
              IF(NINT(PM(50,J))==8)THEN
                !TEST LAW11
                IF(NINT(PM(19,J))==11)THEN
                    FLUX(K,I) = MAX(FLUX(K,I),ZERO)
                !TEST LAW51 IFORM=3
                ELSEIF(NINT(PM(19,J))==51)THEN
                  IADBUF = NINT(PM(27,J))
                  IFLG   = NINT(BUFMAT(31+IADBUF-1))
                  IF (IFLG == 6)THEN
                    FLUX(K,I) = MAX(FLUX(K,I),ZERO)
                  ENDIF
                ENDIF!LAW TEST
              ENDIF!ISOLNOD==8             
            ENDIF!JJ>0
          ENDDO
        ENDDO
      ENDIF
C-----------------------------------------------
      RETURN
      END
