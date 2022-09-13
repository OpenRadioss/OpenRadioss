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
Chd|  W_SECTIO                      source/restart/ddsplit/w_sectio.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE W_SECTIO(NSTRF  ,CEP   ,CEL,PROC,
     2                    NSTRF_L,NODLOCAL,LEN_IA)
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
       INTEGER  PROC, NSTRF_L, LEN_IA,
     .          NSTRF(*), CEP(*), CEL(*), NODLOCAL(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NNOD_S, NSELS_S, NSELQ_S, NSELC_S, NSELT_S,NSELP_S,TYP_S,
     .        NNOD_S_L,NSELR_S,NSELTG_S, NSINT_S, NSELS_S_L, NSELQ_S_L,
     .        NSELC_S_L, NSELT_S_L, NSELP_S_L, NSELR_S_L, NSELTG_S_L,
     .        N, N1, N2, N3, IP, J, K, OFF, IP_L, K0_L, KR0_L, LEN,
     .        NSTR_L(NSTRF_L)
C
      IP_L = 30
      IP = 30
      KR0_L=11
      DO J = 1, IP
        NSTR_L(J) = NSTRF(J)
      ENDDO
C
      DO N = 1, NSECT
        TYP_S   = NSTRF(IP+1)
        N1      = NSTRF(IP+4)
        N2      = NSTRF(IP+5)
        N3      = NSTRF(IP+6)
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
        K0_L = IP_L
C        LEN = 30 + NSINT_S + NNOD_S
        LEN = 30 + NSINT_S
        DO J = 1, LEN
          NSTR_L(IP_L+J) = NSTRF(IP+J)
        ENDDO
        IF(N1/=0) THEN
          IF(NLOCAL(N1,PROC+1)==1)THEN
            N1 = NODLOCAL(N1)
            NSTR_L(IP_L+4) = N1
          ELSE
            NSTR_L(IP_L+4) = -N1
          END IF
        END IF
        IF(N2/=0) THEN
          IF(NLOCAL(N2,PROC+1)==1)THEN
            N2 = NODLOCAL(N2)
            NSTR_L(IP_L+5) = N2
          ELSE
            NSTR_L(IP_L+5) = -N2
          END IF
        END IF
        IF(N3/=0) THEN
          IF(NLOCAL(N3,PROC+1)==1)THEN
            N3 = NODLOCAL(N3)
            NSTR_L(IP_L+6) = N3
          ELSE
            NSTR_L(IP_L+6) = -N3
          END IF
        END IF
        IP = IP + LEN
        IP_L = IP_L + LEN
C noeuds
        NNOD_S_L = 0
        DO J = 1, NNOD_S
          K = NSTRF(IP + J)
          IF(NLOCAL(K,PROC+1)==1)THEN     
            NNOD_S_L = NNOD_S_L + 1
            NSTR_L(IP_L + NNOD_S_L) = NODLOCAL(K)
          END IF
        END DO
        NSTR_L(K0_L+7) = NNOD_S_L
        IP = IP + NNOD_S
        IP_L = IP_L + NNOD_S_L
C
        OFF = 0
C solides
        DO J = 1, NSELS_S
          K = NSTRF(IP + J*2 - 1)
          IF(CEP(K+OFF)==PROC) THEN
            NSELS_S_L = NSELS_S_L + 1
            NSTR_L(IP_L+NSELS_S_L*2-1) = CEL(K+OFF)
            NSTR_L(IP_L+NSELS_S_L*2) = NSTRF(IP+J*2)
          ENDIF
        END DO
        NSTR_L(K0_L+8) = NSELS_S_L
        IP_L = IP_L + 2*NSELS_S_L
        IP = IP + 2*NSELS_S
        OFF = OFF + NUMELS
C quad
        DO J = 1, NSELQ_S
          K = NSTRF(IP + J*2 - 1)
          IF(CEP(K+OFF)==PROC) THEN
            NSELQ_S_L = NSELQ_S_L + 1
            NSTR_L(IP_L+NSELQ_S_L*2-1) = CEL(K+OFF)
            NSTR_L(IP_L+NSELQ_S_L*2) = NSTRF(IP+J*2)
          ENDIF
        END DO
        NSTR_L(K0_L+9) = NSELQ_S_L
        IP_L = IP_L + 2*NSELQ_S_L
        IP = IP + 2*NSELQ_S
        OFF = OFF + NUMELQ
C shell
        DO J = 1, NSELC_S
          K = NSTRF(IP + J*2 - 1)
          IF(CEP(K+OFF)==PROC) THEN
            NSELC_S_L = NSELC_S_L + 1
            NSTR_L(IP_L+NSELC_S_L*2-1) = CEL(K+OFF)
            NSTR_L(IP_L+NSELC_S_L*2) = NSTRF(IP+J*2)
          ENDIF
        END DO
        NSTR_L(K0_L+10) = NSELC_S_L
        IP_L = IP_L + 2*NSELC_S_L
        IP = IP + 2*NSELC_S
        OFF = OFF + NUMELC
C truss
        DO J = 1, NSELT_S
          K = NSTRF(IP + J*2 - 1)
          IF(CEP(K+OFF)==PROC) THEN
            NSELT_S_L = NSELT_S_L + 1
            NSTR_L(IP_L+NSELT_S_L*2-1) = CEL(K+OFF)
            NSTR_L(IP_L+NSELT_S_L*2) = NSTRF(IP+J*2)
          ENDIF
        END DO
        NSTR_L(K0_L+11) = NSELT_S_L
        IP_L = IP_L + 2*NSELT_S_L
        IP = IP + 2*NSELT_S
        OFF = OFF + NUMELT
C poutre
        DO J = 1, NSELP_S
          K = NSTRF(IP + J*2 - 1)
          IF(CEP(K+OFF)==PROC) THEN
            NSELP_S_L = NSELP_S_L + 1
            NSTR_L(IP_L+NSELP_S_L*2-1) = CEL(K+OFF)
            NSTR_L(IP_L+NSELP_S_L*2) = NSTRF(IP+J*2)
          ENDIF
        END DO
        NSTR_L(K0_L+12) = NSELP_S_L
        IP_L = IP_L + 2*NSELP_S_L
        IP = IP + 2*NSELP_S
        OFF = OFF + NUMELP
Cel ressort
        DO J = 1, NSELR_S
          K = NSTRF(IP + J*2 - 1)
          IF(CEP(K+OFF)==PROC) THEN
            NSELR_S_L = NSELR_S_L + 1
            NSTR_L(IP_L+NSELR_S_L*2-1) = CEL(K+OFF)
            NSTR_L(IP_L+NSELR_S_L*2) = NSTRF(IP+J*2)
          ENDIF
        END DO
        NSTR_L(K0_L+13) = NSELR_S_L
        IP_L = IP_L + 2*NSELR_S_L
        IP = IP + 2*NSELR_S
        OFF = OFF + NUMELR
Cel triangle
        DO J = 1, NSELTG_S
          K = NSTRF(IP + J*2 - 1)
          IF(CEP(K+OFF)==PROC) THEN
            NSELTG_S_L = NSELTG_S_L + 1
            NSTR_L(IP_L+NSELTG_S_L*2-1) = CEL(K+OFF)
            NSTR_L(IP_L+NSELTG_S_L*2) = NSTRF(IP+J*2)
          ENDIF
        END DO
        NSTR_L(K0_L+14) = NSELTG_S_L
        IP_L = IP_L + 2*NSELTG_S_L
        IP = IP + 2*NSELTG_S
        OFF = OFF + NUMELTG
C K0NEXT
        NSTR_L(K0_L+25) = K0_L+30+NSINT_S+NNOD_S_L+
     +                    2*(NSELS_S_L+NSELQ_S_L+NSELC_S_L+NSELT_S_L+
     +                       NSELP_S_L+NSELR_S_L+NSELTG_S_L)+1
        NSTR_L(K0_L+26) = KR0_L+10
        IF(TYP_S>=100) NSTR_L(K0_L+26)=NSTR_L(K0_L+26)+12*NNOD_S_L
        IF(TYP_S>=101) NSTR_L(K0_L+26)=NSTR_L(K0_L+26)+12*NNOD_S_L
        IF(TYP_S>=102) NSTR_L(K0_L+26)=NSTR_L(K0_L+26)+6*NNOD_S_L
        KR0_L = NSTR_L(K0_L+26)
      ENDDO
C
      CALL WRITE_I_C(NSTR_L,NSTRF_L)
      LEN_IA = LEN_IA + NSTRF_L
C
      RETURN
      END
