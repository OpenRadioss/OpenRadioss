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
Chd|====================================================================
Chd|  NODALP                        source/output/anim/generate/nodalp.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        H3D_NODAL_SCALAR              source/output/h3d/h3d_results/h3d_nodal_scalar.F
Chd|-- calls ---------------
Chd|        ANINFLOW                      source/output/anim/generate/aninflow.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE ANIM_NODAL_VECTOR_FVMBAGS(KEY, WA4, MONVOL,VOLMON, FVDATA, NFVBAG, SMONVOL, SVOLMON,
     .                                     AIRBAGS_TOTAL_FVM_IN_H3D, IS_WRITTEN_NODE_FVM)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     This suroutine computes nodal vectors from fvmbag polyhedra (fvm case)
C     Done when requested by Engine keyword
C        velocity    : /ANIM/NODA/VEL or /H3D/NODA/VEL
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------
C     none
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USEGROUPDEF_MOD , only:GROUP_
      USE FVBAG_MOD , only:FVBAG_DATA,AIRBAGS_NODE_ID_SHIFT !data structure definition
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*4, INTENT(IN) :: KEY
      INTEGER,INTENT(IN) :: NFVBAG, SMONVOL,SVOLMON,AIRBAGS_TOTAL_FVM_IN_H3D
      my_real,INTENT(INOUT) :: WA4(3,AIRBAGS_TOTAL_FVM_IN_H3D)
      my_real,INTENT(IN) :: VOLMON(SVOLMON)
      INTEGER,INTENT(IN) :: MONVOL(SMONVOL)
      TYPE(FVBAG_DATA), INTENT(IN) :: FVDATA(NFVBAG) 
      INTEGER,INTENT(INOUT) :: IS_WRITTEN_NODE_FVM(AIRBAGS_TOTAL_FVM_IN_H3D)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: K1        !< index shift for MONVOL ARRAY
      INTEGER :: NN, II    !< loop
      INTEGER :: ITYP      !< monvol type (refer to read_monvol.F)
      INTEGER :: IH3D_FLAG !< node group identifier
      INTEGER :: NNODES    !< number of nodes in group
      INTEGER :: IFV       !< FVMBAG identifier in [1, NFVBAG] where NFVBAG <= NVOLU
      INTEGER :: nodeID    !< node identifier (internal)
      my_real :: VALUE(3)  !< polyhedron value
      my_real :: VAL_MIN   !< minimum contour value
      INTEGER :: K_SHIFT   !< shift value for index : sum of previous NPOLH
      INTEGER :: INDX
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------    


      !loop over all airbags, skip ones which are not FVMBAG (no internal mesh)
      ! then if grnod_id is provided get corresponding value in the output buffer WA4 depending on KEY value (pressure, temperature, density ...)  
      K1=1
      K_SHIFT=0
      DO NN=1,NVOLU
         ITYP=MONVOL(K1-1+2)
         IF (ITYP == 6.OR.ITYP == 8 .OR. ITYP == 11) THEN   ! /MONVOL/FVMBAG, or FVMBAG1, or FVMBAG2
            IH3D_FLAG = MONVOL(K1-1 +75)  !GRNOD internal identifier in [1:NGRNOD]
            IFV = MONVOL(K1-1 +45)
            !check if grnod_id was provided, otherwise skip
            IF(IH3D_FLAG == 1 .AND. IFV /= 0)THEN
              
              IF(FVDATA(IFV)%NPOLH > 0)THEN !if GRNOD is not empty and if there are any polyhedron
                 
                 VAL_MIN=EP20
                                 
                 SELECT CASE (TRIM(KEY))
                 
                   CASE('VEL')
                     DO II=1,FVDATA(IFV)%NPOLH !loop over polyhedra composing the airbag mesh
                       VALUE(1:3) = ZERO
                       IF(FVDATA(IFV)%MPOLH(II) /= ZERO)THEN
                         VALUE(1) = FVDATA(IFV)%QPOLH(1,II) / FVDATA(IFV)%MPOLH(II)
                         VALUE(2) = FVDATA(IFV)%QPOLH(2,II) / FVDATA(IFV)%MPOLH(II)
                         VALUE(3) = FVDATA(IFV)%QPOLH(3,II) / FVDATA(IFV)%MPOLH(II)                                                  
                       ENDIF
                       nodeID = AIRBAGS_NODE_ID_SHIFT+K_SHIFT+II
                       WA4(1:3,K_SHIFT+II) = VALUE(1:3)
                       IS_WRITTEN_NODE_FVM(K_SHIFT+II) = 1
                    ENDDO! next I (next polyhedron)                                        

                END SELECT

              ENDIF !NPOLH>0>0 
            ENDIF !IH3D_FLAG>0
            K_SHIFT = K_SHIFT+FVDATA(IFV)%NPOLH
         ENDIF ! ITYP
         K1=K1+NIMV

      ENDDO !next NN
      
      ! set minimum value to other nodes in the group (otherwise visualization may lead to unsuitable color legend/gradient)          
      IF(K_SHIFT < AIRBAGS_TOTAL_FVM_IN_H3D)THEN                                                                       
        DO II= 1,AIRBAGS_TOTAL_FVM_IN_H3D-AIRBAGS_NODE_ID_SHIFT-K_SHIFT                                                                      
          nodeID = AIRBAGS_NODE_ID_SHIFT+K_SHIFT+II                                                                                   
          WA4(1:3, K_SHIFT+II) = ZERO
          IS_WRITTEN_NODE_FVM(K_SHIFT+II) = 0                                                                                                 
        ENDDO                                                                                                                         
      ENDIF        

      
      RETURN
      END
