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
Chd|  H3D_UPDATE_FVMBAG_CENTROIDS   source/output/h3d/h3d_build_fortran/h3d_update_fvmbag_centroids.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE H3D_UPDATE_FVMBAG_CENTROIDS(H3DTITLE,LEN_H3DTITLE,IH3D,MONVOL,VOLMON, NFVBAG, SMONVOL, SVOLMON,
     .                                       FVDATA_P, AIRBAGS_NODE_ID_SHIFT)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     This suroutine updates orphan nodes entities located at polyhedra centroids
C     (Visualization purpose)
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------
C     none
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FVBAG_MOD , only: FVBAG_DATA, AIRBAGS_TOTAL_FVM_IN_H3D
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
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NFVBAG, SMONVOL,SVOLMON, AIRBAGS_NODE_ID_SHIFT
      my_real,INTENT(IN) :: VOLMON(SVOLMON)
      INTEGER,INTENT(IN) :: MONVOL(SMONVOL) 
      CHARACTER*80,INTENT(IN):: H3DTITLE  
      INTEGER,INTENT(IN) :: LEN_H3DTITLE , IH3D 
      TYPE(FVBAG_DATA),INTENT(IN) :: FVDATA_P(NFVBAG)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: K1        !< index shift for MONVOL ARRAY
      INTEGER :: NN, II    !< loop
      INTEGER :: ITYP      !< monvol type (refer to read_monvol.F)
      INTEGER :: IH3D_FLAG !< flag for H3D output
      INTEGER :: NNODES    !< number of centroids
      INTEGER :: IFV       !< FVMBAG identifier in [1, NFVBAG] where NFVBAG <= NVOLU
      INTEGER :: NPOLH     !< number of polyhedra for a given airbag
      my_real :: VALUE_X   !< polyhedron centroid X-value
      my_real :: VALUE_Y   !< polyhedron centroid Y-value
      my_real :: VALUE_Z   !< polyhedron centroid Z-value            
      INTEGER :: ILEN      !< loop upper bound
      
      INTEGER, ALLOCATABLE,DIMENSION(:) :: ITAB_FVMBAG
      my_real, ALLOCATABLE,DIMENSION(:,:) :: XYZ_FVMBAG,DISP_FVMBAG
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
              NPOLH = FVDATA_P(IFV)%NPOLH
              
              IF(NPOLH > 0)THEN ! if there are any polyhedron

                 ALLOCATE(ITAB_FVMBAG(NPOLH))
                 ALLOCATE(DISP_FVMBAG(3,NPOLH))
                 
                 DO II=1, FVDATA_P(IFV)%NPOLH !loop over polyhedra composing the airbag mesh
                   
                   DISP_FVMBAG(1,II) = FVDATA_P(IFV)%CENTROID_POLH(1,II)
                   DISP_FVMBAG(2,II) = FVDATA_P(IFV)%CENTROID_POLH(2,II)
                   DISP_FVMBAG(3,II) = FVDATA_P(IFV)%CENTROID_POLH(3,II)

                   ITAB_FVMBAG(II) = AIRBAGS_NODE_ID_SHIFT+NNODES+II
                 ENDDO! next I (next polyhedron)

                 CALL C_H3D_UPDATE_FVMBAG_NODES(H3DTITLE,LEN_H3DTITLE,IH3D,ITAB_FVMBAG,NPOLH,DISP_FVMBAG)
                 
                 DEALLOCATE(ITAB_FVMBAG)
                 DEALLOCATE(DISP_FVMBAG)
                 NNODES = NNODES + FVDATA_P(IFV)%NPOLH

              ENDIF !NNODES>0
               
            ENDIF !IH3D_FLAG>0
             
         ENDIF ! ITYP
         K1=K1+NIMV
      ENDDO !next NN
      
      
      
      !loop from NNODES +1 to AIRBAGS_TOTAL_FVM_IN_H3D : locate them to  0,0,0
      ILEN = AIRBAGS_TOTAL_FVM_IN_H3D - NNODES
      ALLOCATE(ITAB_FVMBAG(ILEN+1))
      ALLOCATE(DISP_FVMBAG(3,ILEN+1))

      DO II=1,ILEN
        ITAB_FVMBAG(II) = AIRBAGS_NODE_ID_SHIFT+NNODES+II
        DISP_FVMBAG(1,II) = ZERO
        DISP_FVMBAG(2,II) = ZERO
        DISP_FVMBAG(3,II) = ZERO                    
      ENDDO 

      CALL C_H3D_UPDATE_FVMBAG_NODES(H3DTITLE,LEN_H3DTITLE,IH3D,ITAB_FVMBAG,ILEN,DISP_FVMBAG)

      DEALLOCATE(ITAB_FVMBAG)
      DEALLOCATE(DISP_FVMBAG)

      
      RETURN
      END
