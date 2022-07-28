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
#ifndef DNC
Chd|====================================================================
Chd|  C_TRICALL                     stub/fvmbags_stub.F           
Chd|-- called by -----------
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|        POLYHEDR1                     source/airbag/polyhedr1.F     
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|====================================================================
         SUBROUTINE C_TRICALL(PNODES, PSEG, PHOLES, PTRI, NNP,
     .                  NSEG,   NHOL, NELP  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real :: PNODES(*),PHOLES(*),PSEG(:),PTRI(*)
      INTEGER :: NNP,NSEG,NHOL,NELP
C          
          CALL ARRET(5)
      END SUBROUTINE
Chd|====================================================================
Chd|  TRIBOX3                       stub/fvmbags_stub.F           
Chd|-- called by -----------
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|====================================================================
      SUBROUTINE TRIBOX3(ICUT, BCENTER, BHALFSIZE, TVERTS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER :: ICUT
        my_real :: BCENTER(3),BHALFSIZE(3),TVERTS(9)
          CALL ARRET(5)
      END SUBROUTINE
Chd|====================================================================
Chd|  TRITRI3                       stub/fvmbags_stub.F           
Chd|-- called by -----------
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|====================================================================
      SUBROUTINE TRITRI3(ICUT, FV0, FV1, FV2, FU0, FU1, FU2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER :: ICUT
        my_real :: FV0, FV1, FV2, FU0, FU1, FU2
        CALL ARRET(5)
      END SUBROUTINE
Chd|====================================================================
Chd|  MESHGEMS_TETRA                stub/fvmbags_stub.F           
Chd|-- called by -----------
Chd|        INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|-- calls ---------------
Chd|        FVMBAG_MESHCONTROL_MOD        ../common_source/modules/fvmbag_meshcontrol_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|====================================================================
      SUBROUTINE MESHGEMS_TETRA(T_MONVOLN, NIXSL, N, KM, 
     .     NB_EXTRA_VERTICES, NB_TETRA, NB_VERTICES)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE FVMBAG_MESHCONTROL_MOD
      USE MESSAGE_MOD
      USE MONVOL_STRUCT_MOD
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NIXSL
      INTEGER, INTENT(IN) :: N, KM
      INTEGER, INTENT(OUT) :: NB_EXTRA_VERTICES, NB_TETRA, NB_VERTICES
      TYPE(MONVOL_STRUCT_), INTENT(INOUT) :: T_MONVOLN
        WRITE(6,*) "FVMBAGS require a mesher"
        STOP
      END SUBROUTINE
Chd|====================================================================
Chd|  HYPERMESH_TETRA               stub/fvmbags_stub.F           
Chd|-- called by -----------
Chd|        INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|-- calls ---------------
Chd|        FVMBAG_MESHCONTROL_MOD        ../common_source/modules/fvmbag_meshcontrol_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|====================================================================
      SUBROUTINE HYPERMESH_TETRA(T_MONVOLN, NIXSL, N, KM, 
     .     NB_EXTRA_VERTICES, NB_TETRA, NB_VERTICES)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE FVMBAG_MESHCONTROL_MOD
      USE MESSAGE_MOD
      USE MONVOL_STRUCT_MOD
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NIXSL
      INTEGER, INTENT(IN) :: N, KM
      INTEGER, INTENT(INOUT) :: NB_EXTRA_VERTICES, NB_TETRA, NB_VERTICES
      TYPE(MONVOL_STRUCT_), INTENT(IN) :: T_MONVOLN
        WRITE(6,*) "FVMBAGS require a mesher"
        STOP
      END SUBROUTINE
Chd|====================================================================
Chd|  LOAD_MESHGEMS_LIBRARY         stub/fvmbags_stub.F           
Chd|-- called by -----------
Chd|        INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LOAD_MESHGEMS_LIBRARY(N)
        IMPLICIT NONE
        INTEGER :: N
        WRITE(6,*) "FVMBAGS require a mesher"
        STOP
      END SUBROUTINE
#endif
