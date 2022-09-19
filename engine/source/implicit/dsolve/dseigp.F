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
Chd|  DSEIGP                        source/implicit/dsolve/dseigp.F
Chd|-- called by -----------
Chd|        DSRESOLP                      source/implicit/dsolve/dsresolp.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE DSEIGP(N     , NEV   , NCV   , WHICH, INFO  ,
     .                  MAXN  , MAXNEV, MAXNCV, LDV  , ISHFTS,
     .                  MAXITR, MODE  , TOL   , RIGIS, MASS  ,
     .                  D     , V     , IPRI  , SIGMA, DESC_K,
     .                  DESC_M, NLOC  , ICTXT , NPROW, NPCOL ,
     .                  NBLOC )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "debug_arpack.inc"
#include      "units_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, NEV, NCV, INFO, MAXN, MAXNEV, MAXNCV, LDV, ISHFTS,
     .        MAXITR, MODE, IPRI, DESC_K(*), DESC_M(*), NLOC, ICTXT,
     .        NPROW, NPCOL, NBLOC
      my_real
     .        TOL, RIGIS(NLOC,*), MASS(NLOC,*), D(MAXNCV,*), V(LDV,*),
     .        SIGMA
      CHARACTER WHICH*2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER INFFAC, INFRES, I, IPIV(NLOC+NBLOC), MYPROW, MYPCOL,
     .        DESC_W(9)
      my_real
     .        B(NLOC)
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: RIGIS_INI

        WRITE(6,*) "Deprecated feature!"
        CALL FLUSH(6)
        CALL ARRET(5)

      RETURN
      END

