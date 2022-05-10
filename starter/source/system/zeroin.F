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
Chd|  ZEROIN                        source/system/zeroin.F        
Chd|-- called by -----------
Chd|        C3GRTAILS                     source/elements/sh3n/coque3n/c3grtails.F
Chd|        CGRTAILS                      source/elements/shell/coque/cgrtails.F
Chd|        HM_READ_THGRKI                source/output/th/hm_read_thgrki.F
Chd|        HM_READ_THGRKI_RBODY          source/output/th/hm_read_thgrki_rbody.F
Chd|        HM_READ_THGRNE                source/output/th/hm_read_thgrne.F
Chd|        HM_READ_THGRNS                source/output/th/hm_read_thgrns.F
Chd|        HM_READ_THGRSURF              source/output/th/hm_read_thgrsurf.F
Chd|        HM_THGRKI_VENT                source/output/th/hm_thgrki_vent.F
Chd|        I11NINTDD                     source/spmd/domain_decomposition/grid2mat.F
Chd|        I24NINTDD                     source/spmd/domain_decomposition/grid2mat.F
Chd|        I25NINTDD                     source/spmd/domain_decomposition/grid2mat.F
Chd|        I7NINTDD                      source/spmd/domain_decomposition/grid2mat.F
Chd|        IG3DGRTAILS                   source/elements/ige3d/ig3dgrtails.F
Chd|        PGRTAILS                      source/elements/beam/pgrtails.F
Chd|        QGRTAILS                      source/elements/solid_2d/quad/qgrtails.F
Chd|        RGRTAILS                      source/elements/spring/rgrtails.F
Chd|        SGRTAILS                      source/elements/solid/solide/sgrtails.F
Chd|        SPGRTAILS                     source/elements/sph/spgrtails.F
Chd|        T3GRTAILS                     source/elements/solid_2d/tria/t3grtails.F
Chd|        TGRTAILS                      source/elements/truss/tgrtails.F
Chd|        XGRTAILS                      source/elements/xelem/xgrtails.F
Chd|        NODREAD                       source/starter/freform.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ZEROIN(N1,N2,MA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N1, N2
      INTEGER MA(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J
      DO 100 J=N1,N2
  100 MA(J)=0
      RETURN
      END
