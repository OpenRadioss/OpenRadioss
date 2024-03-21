Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  H3D_CREATE_FVMBAG_CENTROIDS   source/output/h3d/h3d_build_fortran/h3d_create_fvmbag_centroids.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE H3D_CREATE_FVMBAG_CENTROIDS(MONVOL,VOLMON, FVDATA, NFVBAG, SMONVOL, SVOLMON, AIRBAGS_NODE_ID_SHIFT)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     This suroutine generates orphan nodes entities located at polyhedra centroids
C     (Visualization purpose)
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------
C     none
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FVBAG_MOD , only:FVBAG_DATA !data structure definition
      USE GROUPDEF_MOD , only:GROUP_
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NFVBAG, SMONVOL, SVOLMON
      INTEGER,INTENT(IN) :: AIRBAGS_NODE_ID_SHIFT
      my_real,INTENT(IN) :: VOLMON(SVOLMON)
      INTEGER,INTENT(IN) :: MONVOL(SMONVOL)
      TYPE(FVBAG_DATA), INTENT(IN) :: FVDATA(NFVBAG)                     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: K1        !< index shift for MONVOL ARRAY
      INTEGER :: NN, II    !< loop
      INTEGER :: ITYP      !< monvol type (refer to read_monvol.F)
      INTEGER :: IH3D_FLAG !< flag for H3D output
      INTEGER :: NNODES    !< number of centroids
      INTEGER :: IFV       !< FVMBAG identifier in [1, NFVBAG] where NFVBAG <= NVOLU
      INTEGER :: nodeID    !< node identifier (internal)
      INTEGER :: NPOLH     !< number of polyhedra for a given airbag
      my_real :: VALUE_X   !< polyhedron centroid X-value
      my_real :: VALUE_Y   !< polyhedron centroid Y-value
      my_real :: VALUE_Z   !< polyhedron centroid Z-value            
      
      INTEGER, ALLOCATABLE,DIMENSION(:) :: ITAB_FVMBAG,TAGNOD_FVMBAG
      my_real, ALLOCATABLE,DIMENSION(:,:) :: XYZ_FVMBAG,DISP_FVMBAG
      INTEGER :: ILEN
      
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------    


      !loop over all airbags, skip ones which are not FVMBAG (no internal mesh)
      ! then generate H3D (orphan) nodes at centroids
      K1=1
      NNODES = 0
      DO NN=1,NVOLU
         ITYP=MONVOL(K1-1+2)
         IF (ITYP == 6.OR.ITYP == 8 .OR. ITYP == 11) THEN   ! /MONVOL/FVMBAG, or FVMBAG1, or FVMBAG2
            IH3D_FLAG = MONVOL(K1-1 +75)  !GRNOD internal identifier in [1:NGRNOD]
            IFV = MONVOL(K1-1 +45)
            !check if grnod_id was provided, otherwise skip
            IF(IH3D_FLAG == 1 .AND. IFV /= 0)THEN
              NPOLH = FVDATA(IFV)%NPOLH
              
              IF(NPOLH > 0)THEN ! if there are any polyhedron

                 ALLOCATE(ITAB_FVMBAG(NPOLH))
                 ALLOCATE(TAGNOD_FVMBAG(NPOLH))
                 ALLOCATE(XYZ_FVMBAG(3,NPOLH))
                 ALLOCATE(DISP_FVMBAG(3,NPOLH))
                 
                 DO II=1, FVDATA(IFV)%NPOLH !loop over polyhedra composing the airbag mesh
                   
                   XYZ_FVMBAG(1,II) = ZERO
                   XYZ_FVMBAG(2,II) = ZERO
                   XYZ_FVMBAG(3,II) = ZERO                                     

                   DISP_FVMBAG(1,II) = ZERO! -FVDATA(IFV)%CENTROID_POLH(1,II)
                   DISP_FVMBAG(2,II) = ZERO! -FVDATA(IFV)%CENTROID_POLH(2,II)
                   DISP_FVMBAG(3,II) = ZERO! -FVDATA(IFV)%CENTROID_POLH(3,II)                                      

                   ITAB_FVMBAG(II) = AIRBAGS_NODE_ID_SHIFT + NNODES + II
                   TAGNOD_FVMBAG(II) = 1
                   
                 ENDDO! next I (next polyhedron)

                 CALL C_H3D_CREATE_NODES(ITAB_FVMBAG,NPOLH,XYZ_FVMBAG,TAGNOD_FVMBAG,DISP_FVMBAG)
                 
                 DEALLOCATE(ITAB_FVMBAG)
                 DEALLOCATE(TAGNOD_FVMBAG)
                 DEALLOCATE(XYZ_FVMBAG)
                 DEALLOCATE(DISP_FVMBAG)
                 NNODES = NNODES + FVDATA(IFV)%NPOLH

              ENDIF !NNODES>0
               
            ENDIF !IH3D_FLAG>0
             
         ENDIF ! ITYP
         K1=K1+NIMV
      ENDDO !next NN
                
      RETURN
      END
