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
C
Chd|====================================================================
Chd|  W_SECBUF                      source/restart/ddsplit/w_secbuf.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE W_SECBUF(SECBUF ,LSECBUF_L, NSTRF, PROC,
     2                    LEN_AM )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER  PROC, LSECBUF_L, LEN_AM,
     .          NSTRF(*)
      my_real
     .          SECBUF(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL    
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NNOD_S, NNOD_S_L, TYP_S, NSINT_S,
     .        N, IP, J, K0, K2, KR_L, KR, K
      my_real
     .        SECBUF_L(LSECBUF_L)
C
      KR_L = 0
      KR = 0
C header commun
      DO J = 1, 10
        SECBUF_L(KR_L+J) = SECBUF(KR+J)
      END DO
      KR_L = KR_L + 10
      KR = KR + 10
C
      K0 = NSTRF(25)
      DO N = 1, NSECT
        TYP_S  = NSTRF(K0)
        NNOD_S = NSTRF(K0+6)
        NSINT_S = NSTRF(K0+14)
        K2=K0+30+NSINT_S
C noeuds locaux
        NNOD_S_L = 0
        DO J = 1, NNOD_S
          K = NSTRF(K2 + J -1)
          IF(NLOCAL(K,PROC+1)==1)THEN
            NNOD_S_L = NNOD_S_L + 1
          END IF
        END DO
        DO J = 1, 10
          SECBUF_L(KR_L+J) = SECBUF(KR+J)
        END DO
        KR_L = KR_L + 10
        KR = KR + 10
        IF(TYP_S.GE.100) THEN
          DO J = 1, 12*NNOD_S_L
            SECBUF_L(KR_L+J) = ZERO
          END DO
          KR_L = KR_L + 12*NNOD_S_L
          KR = KR + 12*NNOD_S
        END IF
        IF(TYP_S.GE.101) THEN
          DO J = 1, 12*NNOD_S_L
            SECBUF_L(KR_L+J) = ZERO
          END DO
          KR_L = KR_L + 12*NNOD_S_L
          KR = KR + 12*NNOD_S
        END IF
        IF(TYP_S.GE.102) THEN
          DO J = 1, 6*NNOD_S_L
            SECBUF_L(KR_L+J) = ZERO
          END DO
          KR_L = KR_L + 6*NNOD_S_L
          KR = KR + 6*NNOD_S
        END IF
        K0  = NSTRF(K0+24)
      ENDDO
C
      CALL WRITE_DB(SECBUF_L,LSECBUF_L)
      LEN_AM = LEN_AM + LSECBUF_L
      if(LSECBUF_L.NE.KR_L)print*,'error secbuf',LSECBUF_L,KR_L,NNOD_S_L
C
      RETURN
      END
