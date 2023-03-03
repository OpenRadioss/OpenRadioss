Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
c      SUBROUTINE W_RRIVET(IXRI,FRONT,PROC,NRIVET_L,RIVET,LEN_AM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
c#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
c      INTEGER PROC, NRIVET_L, LEN_AM,
c     .        IXRI(4,*), FRONT(NUMNOD,*)
c      my_real
c     .        RIVET(NRIVF,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
c      INTEGER N_L, N, N1, N2, J
c      my_real
c     .        RIVET_L(NRIVF,NRIVET_L)
C
c      N_L = 0
c      DO N = 1, NRIVET
c        N1=IXRI(2,N)
c        N2=IXRI(3,N)
c        IF(MOD(FRONT(N1,PROC+1),10)==1.AND.
c     +     MOD(FRONT(N2,PROC+1),10)==1)THEN
c          N_L = N_L + 1
c          DO J = 1, NRIVF
c            RIVET_L(J,N_L) = RIVET(J,N)
c          END DO
c        ENDIF
c      ENDDO
C
c      CALL WRITE_DB(RIVET_L,NRIVET_L*NRIVF)
c      LEN_AM = LEN_AM + NRIVET_L*NRIVF
C
c      RETURN
c      END
C
Chd|====================================================================
Chd|  W_MAD                         source/restart/ddsplit/w_mad.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|====================================================================
      SUBROUTINE W_MAD(IEXMAD  ,NMADSH4_L,NMADSH3_L,NMADSOL_L,NMADNOD_L,
     +                 MADCL_NMADNOD_L,CEP,PROC,NODLOCAL ,CEL  ,
     +                 NUMELS_L,NUMELC_L ,NUMELTG_L,LEN_IA   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER  PROC, NMADSH4_L, NMADSH3_L, NMADSOL_L, NMADNOD_L,
     .          LEN_IA, NUMELS_L, NUMELC_L, NUMELTG_L,MADCL_NMADNOD_L,
     .          IEXMAD(*), CEP(*), NODLOCAL(*),CEL(*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL    
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IDEB, K, ESHIFT, NMAD_L, P
      INTEGER, DIMENSION(:),ALLOCATABLE :: MADCL_TMPNOD,TMPSH3,TMPSH4,TMPSOL,
     .        TMPNOD,FAILSH4,FAILSH3,FAILSOL,ICONX_L
C-----------------------------------------------
!     allocate 1d arrays
      ALLOCATE(MADCL_TMPNOD(MADCL_NMADNOD_L))
      ALLOCATE( TMPSH3(NMADSH3_L),TMPSH4(NMADSH4_L) )
      ALLOCATE( TMPSOL(NMADSOL_L),TMPNOD(NMADNOD_L) )
      ALLOCATE( ICONX_L(7*NCONX) )
! -------------------------------
C
C Couplage etendu
C
      IF(NEXMAD/=0) THEN
C
C ELEM shell4
C
        IDEB = 1 + 7*NCONX + NMADPRT
        ESHIFT = NUMELS+NUMELQ
        NMAD_L = 0
        DO I = 1, NMADSH4
          K = IEXMAD(IDEB+I-1)
          IF(CEP(K+ESHIFT)==PROC) THEN
            NMAD_L = NMAD_L+1
            TMPSH4(NMAD_L) = CEL(K+ESHIFT)
          END IF
        END DO
C
C Elem shell3
C
        IDEB = IDEB + NMADSH4
        ESHIFT = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR
        NMAD_L = 0
        DO I = 1, NMADSH3
          K = IEXMAD(IDEB+I-1)
          IF(CEP(K+ESHIFT)==PROC) THEN
            NMAD_L = NMAD_L+1
            TMPSH3(NMAD_L) = CEL(K+ESHIFT)
          END IF
        END DO
C
C Elem solides
C
        IDEB = IDEB + NMADSH3
        ESHIFT = 0
        NMAD_L = 0
        DO I = 1, NMADSOL
          K = IEXMAD(IDEB+I-1)
          IF(CEP(K+ESHIFT)==PROC) THEN
            NMAD_L = NMAD_L+1
            TMPSOL(NMAD_L) = CEL(K+ESHIFT)
          END IF
        END DO
C
C Noeuds
C
        IDEB = IDEB + NMADSOL
        NMAD_L = 0
        DO I = 1, NMADNOD
          K = IEXMAD(IDEB+I-1)
          IF(NLOCAL(K,PROC+1)==1) THEN
            DO P = 1, PROC
              IF(NLOCAL(K,P)==1) GOTO 100
            END DO
            NMAD_L = NMAD_L+1
            TMPNOD(NMAD_L) = NODLOCAL(K)
          END IF
 100      CONTINUE
        END DO
C
C Noeuds MADCL
C
        NMAD_L = 0
        DO I = 1, NMADNOD
          K = IEXMAD(IDEB+I-1)
          IF(NLOCAL(K,PROC+1)==1) THEN
            NMAD_L = NMAD_L+1
            MADCL_TMPNOD(NMAD_L) = NODLOCAL(K)
          END IF
        END DO
C
      END IF
C
      IF(PROC==0) THEN
        DO I=1,7*NCONX
          ICONX_L(I) = IEXMAD(I)
        END DO
        DO I=1,NCONX
          ICONX_L(7*(I-1)+4) = NODLOCAL(IEXMAD(7*(I-1)+4))
        END DO
        CALL WRITE_I_C(ICONX_L,7*NCONX)
        LEN_IA = LEN_IA + 7*NCONX
      END IF
      IF(NEXMAD/=0) THEN
        IF(PROC==0) THEN
          CALL WRITE_I_C(IEXMAD(7*NCONX+1),NMADPRT)
          LEN_IA = LEN_IA + NMADPRT
        END IF
        CALL WRITE_I_C(TMPSH4,NMADSH4_L)
        LEN_IA = LEN_IA + NMADSH4_L
        CALL WRITE_I_C(TMPSH3,NMADSH3_L)
        LEN_IA = LEN_IA + NMADSH3_L
        CALL WRITE_I_C(TMPSOL,NMADSOL_L)
        LEN_IA = LEN_IA + NMADSOL_L
        CALL WRITE_I_C(TMPNOD,NMADNOD_L)
        LEN_IA = LEN_IA + NMADNOD_L
        CALL WRITE_I_C(MADCL_TMPNOD,MADCL_NMADNOD_L)
        LEN_IA = LEN_IA + MADCL_NMADNOD_L
        IF(PROC==0)THEN
          IDEB = 7*NCONX+NMADPRT+NMADSH4+NMADSH3+NMADSOL+NMADNOD
     +         + NUMELC+NUMELTG+NUMELS + 1
          CALL WRITE_I_C(IEXMAD(IDEB),2*NMADNOD+NMADSH4+NMADSH3+NMADSOL)
          LEN_IA = LEN_IA + 2*NMADNOD+NMADSH4+NMADSH3+NMADSOL
        END IF
      END IF
C
! -------------------------------
!     deallocate 1d arrays
      DEALLOCATE(MADCL_TMPNOD)
      DEALLOCATE( TMPSH3,TMPSH4 )
      DEALLOCATE( TMPSOL,TMPNOD )
      DEALLOCATE( ICONX_L )
! -------------------------------
      RETURN
      END
