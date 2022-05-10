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
Chd|  C_SECTIO                      source/restart/ddsplit/c_sectio.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE C_SECTIO(NSTRF    ,CEP    ,PROC,NSTRF_L,
     .                    LSECBUF_L,NNODT_L,NNODL_L)
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER  PROC, NSTRF_L, LSECBUF_L, NNODT_L, NNODL_L,
     .          NSTRF(*), CEP(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NNOD_S, NSELS_S, NSELQ_S, NSELC_S, NSELT_S, NSELP_S,
     .        NSELR_S, NSELTG_S, NSINT_S, NSELS_S_L, NSELQ_S_L,
     .        NSELC_S_L, NSELT_S_L, NSELP_S_L, NSELR_S_L, NSELTG_S_L,
     .        N, IP, P, J, K, OFF, NNOD_S_L, TYP_S
C
      NSTRF_L = 30
C      LSECBUF_L = 0
      LSECBUF_L = 10
      NNODT_L = 0
      NNODL_L = 0
      IP = 30
      DO N = 1, NSECT
        TYP_S   = NSTRF(IP+1)
        NNOD_S  = NSTRF(IP+7)
        NSELS_S = NSTRF(IP+8)
        NSELQ_S = NSTRF(IP+9)
        NSELC_S = NSTRF(IP+10)
        NSELT_S = NSTRF(IP+11)
        NSELP_S = NSTRF(IP+12)
        NSELR_S = NSTRF(IP+13)
        NSELTG_S= NSTRF(IP+14)
        NSINT_S = NSTRF(IP+15)
        NSELS_S_L = 0
        NSELQ_S_L = 0
        NSELC_S_L = 0
        NSELT_S_L = 0
        NSELP_S_L = 0
        NSELR_S_L = 0
        NSELTG_S_L= 0
c        IP = IP + 30 + NSINT_S + NNOD_S
c        NSTRF_L = NSTRF_L + 30 + NSINT_S + NNOD_S
        IP = IP + 30 + NSINT_S
        NSTRF_L = NSTRF_L + 30 + NSINT_S
C noeuds
        NNOD_S_L = 0
        DO J = 1, NNOD_S
          K = NSTRF(IP + J)
          IF(NLOCAL(K,PROC+1)==1)THEN
            NNOD_S_L = NNOD_S_L + 1
          END IF
        END DO
        IF(ISECUT.EQ.1) THEN
C section cut
          IF (TYP_S.GE.1)THEN
            NNODL_L = NNODL_L + NNOD_S_L
          ENDIF
C section cut type 1 et superieur sur p0
          IF(PROC.EQ.0.AND.TYP_S.GE.1) THEN
            NNODT_L = NNODT_L + NNOD_S_L
            DO P = 2, NSPMD
              DO J = 1, NNOD_S
                K = NSTRF(IP + J)
                IF(NLOCAL(K,P)==1)THEN
                  NNODT_L = NNODT_L + 1
                END IF
              END DO
            END DO
          END IF
        END IF
C
        IP = IP + NNOD_S
        NSTRF_L = NSTRF_L + NNOD_S_L
C
        OFF = 0
C solides
        DO J = 1, NSELS_S
          K = NSTRF(IP + J*2 - 1)
          IF(CEP(K+OFF).EQ.PROC) NSELS_S_L = NSELS_S_L + 1
        END DO
        NSTRF_L = NSTRF_L + 2*NSELS_S_L
        IP = IP + 2*NSELS_S
        OFF = OFF + NUMELS
C quad
        DO J = 1, NSELQ_S
          K = NSTRF(IP + J*2 - 1)
          IF(CEP(K+OFF).EQ.PROC) NSELQ_S_L = NSELQ_S_L + 1
        END DO
        NSTRF_L = NSTRF_L + 2*NSELQ_S_L
        IP = IP + 2*NSELQ_S
        OFF = OFF + NUMELQ
C shell
        DO J = 1, NSELC_S
          K = NSTRF(IP + J*2 - 1)
          IF(CEP(K+OFF).EQ.PROC) NSELC_S_L = NSELC_S_L + 1
        END DO
        NSTRF_L = NSTRF_L + 2*NSELC_S_L
        IP = IP + 2*NSELC_S
        OFF = OFF + NUMELC
C truss
        DO J = 1, NSELT_S
          K = NSTRF(IP + J*2 - 1)
          IF(CEP(K+OFF).EQ.PROC) NSELT_S_L = NSELT_S_L + 1
        END DO
        NSTRF_L = NSTRF_L + 2*NSELT_S_L
        IP = IP + 2*NSELT_S
        OFF = OFF + NUMELT
C poutre
        DO J = 1, NSELP_S
          K = NSTRF(IP + J*2 - 1)
          IF(CEP(K+OFF).EQ.PROC) NSELP_S_L = NSELP_S_L + 1
        END DO
        NSTRF_L = NSTRF_L + 2*NSELP_S_L
        IP = IP + 2*NSELP_S
        OFF = OFF + NUMELP
C ressort
        DO J = 1, NSELR_S
          K = NSTRF(IP + J*2 - 1)
          IF(CEP(K+OFF).EQ.PROC) NSELR_S_L = NSELR_S_L + 1
        END DO
        NSTRF_L = NSTRF_L + 2*NSELR_S_L
        IP = IP + 2*NSELR_S
        OFF = OFF + NUMELR
C triangle
        DO J = 1, NSELTG_S
          K = NSTRF(IP + J*2 - 1)
          IF(CEP(K+OFF).EQ.PROC) NSELTG_S_L = NSELTG_S_L + 1
        END DO
        NSTRF_L = NSTRF_L + 2*NSELTG_S_L
        IP = IP + 2*NSELTG_S
        OFF = OFF + NUMELTG
C cut
        LSECBUF_L=LSECBUF_L+10
        IF(TYP_S.GE.100)LSECBUF_L=LSECBUF_L+12*NNOD_S_L
        IF(TYP_S.GE.101)LSECBUF_L=LSECBUF_L+12*NNOD_S_L
        IF(TYP_S.GE.102)LSECBUF_L=LSECBUF_L+6*NNOD_S_L
C.
      ENDDO
C
      RETURN
      END
