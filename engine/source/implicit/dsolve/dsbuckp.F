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
#if defined(MYREAL8) && !defined(WITHOUT_LINALG)
Chd|====================================================================
Chd|  DSBUCKP                       source/implicit/dsolve/dsbuckp.F
Chd|-- called by -----------
Chd|        DSRESOLP_BUCK                 source/implicit/dsolve/dsresolp.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE DSBUCKP(N      , NEV   , NCV    , WHICH, INFO  ,
     .                   MAXN   , MAXNEV, MAXNCV , LDV  , ISHFTS,
     .                   MAXITR , MODE  , TOL    , OP   , RIGI  ,
     .                   RIGIG  , D     , V      , IPRI , SIGMA ,
     .                   DESC_OP, DESC_K, DESC_KG, NLOC , ICTXT ,
     .                   NPROW  , NPCOL , NBLOC  ) 
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, NEV, NCV, INFO, MAXN, MAXNEV, MAXNCV, LDV, ISHFTS,
     .        MAXITR, MODE, IPRI, DESC_OP(*), DESC_K(*), DESC_KG(*), 
     .        NLOC, ICTXT, NPROW, NPCOL, NBLOC
      my_real
     .        TOL, OP(NLOC,*), RIGI(NLOC,*), RIGIG(NLOC,*),
     .        D(MAXNCV,*), V(LDV,*), SIGMA
      CHARACTER*2 WHICH
        WRITE(6,*) "Deprecated feature!"
        CALL FLUSH(6)
        CALL ARRET(5)
      RETURN
      END
#endif
