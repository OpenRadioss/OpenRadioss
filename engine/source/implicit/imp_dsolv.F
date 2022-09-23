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
Chd|  IMP_DSOLV                     source/implicit/imp_dsolv.F   
Chd|-- called by -----------
Chd|        LIN_SOLV2                     source/implicit/lin_solv.F    
Chd|        LIN_SOLVP2                    source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        DSCOND                        source/implicit/dsolve/dscond.F
Chd|        DSDESC                        source/implicit/dsolve/dsdesc.F
Chd|        DSPGRID                       source/implicit/dsolve/dspgrid.F
Chd|        DSRECU                        source/implicit/dsolve/dsrecu.F
Chd|        DSRESOL                       source/implicit/dsolve/dsresol.F
Chd|        DSRESOLP                      source/implicit/dsolve/dsresolp.F
Chd|        SPMD_BARRIER                  source/mpi/generic/spmd_barrier.F
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE IMP_DSOLV(GRAPHE, K_DIAG, K_LT , IADK , JDIK   ,
     .                     NDDL  , NB    , B    , X    , ITAB   ,
     .                     IPRINT, NDDLI7, IADI7, JDII7, I7_DIAG,
     .                     I7_LT , I7TOK )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "scr05_c.inc"
#include "task_c.inc"
#include "units_c.inc"
#include "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IADK(*), JDIK(*), NDDL, NB, ITAB(*), IPRINT, NDDLI7, 
     .        IADI7(*), JDII7(*), I7TOK(*)
      my_real
     .        K_DIAG(*), K_LT(*), B(NB,*), X(NB,*),
     .        I7_DIAG(*), I7_LT(*)
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IOPT, IBID, NDDLG, IPRI, IWKLI, LTITR1, NDDLC
      my_real
     .        RBID
      CHARACTER TITRE*80
C
#if defined(DNC) && !defined(WITHOUT_LINALG)
      IOPT=1
      IWKLI=0
      NDDLI7=0
C Condensation statique
      IF (IPRINT/=0) THEN
         IF (IMACH==3.AND.ISPMD==0) WRITE(ISTDO,*)
         IF (IMACH/=3.OR.ISPMD==0) THEN
            WRITE(ISTDO,'(A)') ' * MULTI-LEVEL STATIC CONDENSATION *'
            WRITE(IOUT,'(A)') ' * MULTI-LEVEL STATIC CONDENSATION *'
         ENDIF
      ENDIF
      CALL DSCOND(GRAPHE, K_DIAG, K_LT,  IADK,  JDIK,
     .            RBID,   NDDL,   NB,    B,     IOPT,
     .            IBID,   IBID,   RBID,  IBID,  ITAB,
     .            IWKLI,  NDDLI7, IADI7, JDII7, I7_DIAG,
     .            I7_LT,  I7TOK )
      IF (IMACH==3.AND.ICONDP==1)
     .   CALL DSPGRID(GRAPHE(1), GRAPHE(1)%NDDL_GLOB, NDDLC, 2)
C Resolution
      NDDLG=NDDL
      IPRI=GRAPHE(1)%IPRI
      IF (IMACH==3.AND.ISPMD==0) NDDLG=GRAPHE(1)%NDDL_GLOB
#ifndef MPI
C     IF (IMACH/=3.OR.ICONDP==0) THEN
         LTITR1=42
         TITRE=' * CONDENSED PROBLEM SEQUENTIAL SOLUTION *'
         CALL DSRESOL(GRAPHE, IOPT,  NB,     NDDLG, B,
     .                IBID,   IBID,  RBID,   RBID,  IBID,
     .                IPRI,   IBID,  RBID,   IBID,  RBID,
     .                TITRE,  LTITR1, IPRINT)
C     ELSE
#else
         LTITR1=40
         TITRE=' * CONDENSED PROBLEM PARALLEL SOLUTION *'

         CALL DSRESOLP(GRAPHE(1), IOPT,  NB,     NDDLC, B,
     .                 IBID,      IBID,  RBID,   RBID,  IBID,
     .                 IPRI,      IBID,  RBID,   IBID,  RBID,
     .                 TITRE,     LTITR1, IPRINT)

#endif
C Redescente
      IF (IPRINT/=0) THEN
         IF (IMACH==3.AND.ISPMD==0) WRITE(ISTDO,*)
         IF (IMACH/=3.OR.ISPMD==0) THEN
            WRITE(ISTDO,'(A)') ' * STATIC SOLUTION EXPANSION *'
            WRITE(IOUT,'(A)') ' * STATIC SOLUTION EXPANSION *'
         ENDIF
      ENDIF
      CALL DSDESC(GRAPHE, NB, NDDL, IOPT)
C Recuperation du vecteur solution
      CALL DSRECU(GRAPHE, NB, X, NDDL, NB,
     .            0     )     
C
      IF (IMACH==3.AND.IPRI<0) THEN
         CALL SPMD_BARRIER()
         IF (ISPMD==0) WRITE(ISTDO,*)
      ENDIF
#else
      write(6,*) "ERROR: DSOLVE is deprecated"  
      CALL ARRET(5)
#endif
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_DSOLV_ITER                source/implicit/imp_dsolv.F   
Chd|-- called by -----------
Chd|        LIN_SOLV2                     source/implicit/lin_solv.F    
Chd|        LIN_SOLVP2                    source/implicit/lin_solv.F    
Chd|-- calls ---------------
Chd|        DSCDF                         source/implicit/dsolve/dscdf.F
Chd|        DSCOND                        source/implicit/dsolve/dscond.F
Chd|        DSDESC                        source/implicit/dsolve/dsdesc.F
Chd|        DSPGRID                       source/implicit/dsolve/dspgrid.F
Chd|        DSRECU                        source/implicit/dsolve/dsrecu.F
Chd|        DSRESOLP_ITER                 source/implicit/dsolve/dsresolp.F
Chd|        DSRESOL_ITER                  source/implicit/dsolve/dsresol.F
Chd|        SPMD_BARRIER                  source/mpi/generic/spmd_barrier.F
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE IMP_DSOLV_ITER(GRAPHE, K_DIAG, K_LT  , IADK   , JDIK  ,
     .                          NDDL  , NB    , B     , X      , ITAB  ,
     .                          IT    , FAC_K , IPIV_K, NK     , IPRINT,
     .                          NDDLI7, IADI7 , JDII7 , I7_DIAG, I7_LT ,
     .                          I7TOK )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "scr05_c.inc"
#include "task_c.inc"
#include "units_c.inc"
#include "impl1_c.inc"
#include "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IADK(*), JDIK(*), NDDL, NB, ITAB(*), IT, IPIV_K(*), NK,
     .        IPRINT, NDDLI7, IADI7(*), JDII7(*), I7TOK(*)
      my_real
     .        K_DIAG(*), K_LT(*), B(NB,*), X(NB,*), FAC_K(*),
     .        I7_DIAG(*), I7_LT(*)
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IOPT, NDDLG, IPRI, IBID, IWKLI, LTITR1, NDDLC, NR
      my_real
     .        RBID
      CHARACTER TITRE*80
#if defined(DNC) && !defined(WITHOUT_LINALG)
C Deprecated feature
      IOPT=1
      IWKLI=1
C Condensation statique si necessaire
      IF (IDSC==1) THEN
         IF (IPRINT/=0) THEN
            IF (IMACH==3.AND.ISPMD==0) WRITE(ISTDO,*)
            IF (IMACH/=3.OR.ISPMD==0) 
     .         WRITE(ISTDO,'(A)') ' * MULTI-LEVEL STATIC CONDENSATION *'
            WRITE(IOUT,'(A)') ' * MULTI-LEVEL STATIC CONDENSATION *'
         ENDIF
         CALL DSCOND(GRAPHE, K_DIAG, K_LT,  IADK,  JDIK,
     .               RBID,   NDDL,   NB,    B,     IOPT,
     .               IBID,   IBID,   RBID,  IBID,  ITAB,
     .               IWKLI,  NDDLI7, IADI7, JDII7, I7_DIAG,
     .               I7_LT,  I7TOK )
         IF (IMACH==3.AND.ICONDP==1) THEN
            CALL DSPGRID(GRAPHE(1), GRAPHE(1)%NDDL_GLOB, NDDLC, 2)
            GRAPHE(1)%NDDLC=NDDLC
         ENDIF
      ELSE
C Condensation du vecteur des forces seul
         IF (IPRINT/=0) THEN
            IF (IMACH==3.AND.ISPMD==0) WRITE(ISTDO,*)
            IF (IMACH/=3.OR.ISPMD==0) 
     .         WRITE(ISTDO,'(A)') ' * MULTI-LEVEL FORCES CONDENSATION *'
            WRITE(IOUT,'(A)') ' * MULTI-LEVEL FORCES CONDENSATION *'
         ENDIF
         CALL DSCDF(GRAPHE, NB, B, NDDL)
      ENDIF
C Resolution
      NDDLG=NDDL
      IPRI=GRAPHE(1)%IPRI
      IF (IMACH==3.AND.ISPMD==0) NDDLG=GRAPHE(1)%NDDL_GLOB
      IF (IMACH/=3.OR.ICONDP==0) THEN
         LTITR1=42
         TITRE=' * CONDENSED PROBLEM SEQUENTIAL SOLUTION *'
         CALL DSRESOL_ITER(GRAPHE, IT,     NB, NDDLG, B,
     .                     FAC_K,  IPIV_K, NK, TITRE, LTITR1,
     .                     IPRINT)
      ELSE
         LTITR1=40
         TITRE=' * CONDENSED PROBLEM PARALLEL SOLUTION *'
         NDDLC=GRAPHE(1)%NDDLC
         NR=GRAPHE(1)%NNRP
         CALL DSRESOLP_ITER(GRAPHE(1), IT, NB,    NDDLC,  B,
     .                      FAC_K,     NR, TITRE, LTITR1,  IPRINT,
     .                      IBID,      1        ) 
      ENDIF
C Redescente
      IF (IPRINT/=0) THEN
         IF (IMACH/=3.OR.ISPMD==0) 
     .      WRITE(ISTDO,'(A)') ' * STATIC SOLUTION EXPANSION *'
         WRITE(IOUT,'(A)') ' * STATIC SOLUTION EXPANSION *'
      ENDIF
      CALL DSDESC(GRAPHE, NB, NDDL, IOPT)
C Recuperation du vecteur solution
      CALL DSRECU(GRAPHE, NB, X, NDDL, NB,
     .            0     )     
C
      IF (IMACH==3.AND.IPRI<0) THEN
         CALL SPMD_BARRIER()
         IF (ISPMD==0) WRITE(ISTDO,*)
      ENDIF
C
#else
      write(6,*) "ERROR: DSOLVE is deprecated"  
      CALL ARRET(5)
#endif

      RETURN
      END
            
