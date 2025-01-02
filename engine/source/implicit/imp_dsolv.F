Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    imp_dsolv     ../engine/source/implicit/imp_dsolv.F
      !||--- called by ------------------------------------------------------
      !||    lin_solv2     ../engine/source/implicit/lin_solv.F
      !||    lin_solvp2    ../engine/source/implicit/lin_solv.F
      !||--- calls      -----------------------------------------------------
      !||    arret         ../engine/source/system/arret.F
      !||--- uses       -----------------------------------------------------
      !||    dsgraph_mod   ../engine/share/modules/dsgraph_mod.F
      !||====================================================================
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
      write(6,*) "ERROR: DSOLVE is deprecated"  
      CALL ARRET(5)
C
      RETURN
      END
      !||====================================================================
      !||    imp_dsolv_iter   ../engine/source/implicit/imp_dsolv.F
      !||--- called by ------------------------------------------------------
      !||    lin_solv2        ../engine/source/implicit/lin_solv.F
      !||    lin_solvp2       ../engine/source/implicit/lin_solv.F
      !||--- calls      -----------------------------------------------------
      !||    arret            ../engine/source/system/arret.F
      !||--- uses       -----------------------------------------------------
      !||    dsgraph_mod      ../engine/share/modules/dsgraph_mod.F
      !||====================================================================
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
      write(6,*) "ERROR: DSOLVE is deprecated"  
      CALL ARRET(5)

      RETURN
      END
            
