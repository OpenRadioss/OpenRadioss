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
Chd|  ANIM_NODAL_CONTOUR_FVMBAGS    source/output/anim/generate/anim_nodal_contour_fvmbags.F
Chd|-- called by -----------
Chd|        NODALD                        source/output/anim/generate/nodald.F
Chd|        NODALDT                       source/output/anim/generate/nodaldt.F
Chd|        NODALP                        source/output/anim/generate/nodalp.F
Chd|        NODALSSP                      source/output/anim/generate/nodalssp.F
Chd|        NODALT                        source/output/anim/generate/nodalt.F
Chd|        NODALVOL                      source/output/anim/generate/nodalvol.F
Chd|-- calls ---------------
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE ANIM_NODAL_CONTOUR_FVMBAGS(KEY, WA4, MONVOL,VOLMON, FVDATA_P, NFVBAG, SMONVOL, SVOLMON, AIRBAGS_TOTAL_FVM,
     .                                      IS_WRITTEN_NODE_FVM, AIRBAGS_NODE_ID_SHIFT)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     This suroutine computes nodal contour from fvmbag polyhedra (fvm case)
C     Done when requested by Engine keyword
C        pressure    : /ANIM/NODA/P or /H3D/NODA/P
C        density     : /ANIM/NODA/DENS or /H3D/NODA/DENS
C        temperature : /ANIM/NODA/TEMP or /H3D/NODA/TEMP
C        volume      : /ANIM/NODA/ZVOL or /H3D/NODA/ZVOL
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
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*4, INTENT(IN) :: KEY
      INTEGER,INTENT(IN) :: NFVBAG, SMONVOL,SVOLMON, AIRBAGS_TOTAL_FVM, AIRBAGS_NODE_ID_SHIFT
      REAL,INTENT(INOUT) :: WA4(AIRBAGS_TOTAL_FVM)
      my_real,INTENT(IN) :: VOLMON(SVOLMON)
      INTEGER,INTENT(IN) :: MONVOL(SMONVOL)
      TYPE(FVBAG_DATA), INTENT(IN) :: FVDATA_P(NFVBAG) 
      INTEGER,INTENT(INOUT) :: IS_WRITTEN_NODE_FVM(AIRBAGS_TOTAL_FVM)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: K1        !< index shift for MONVOL ARRAY
      INTEGER :: NN, II    !< loop
      INTEGER :: ITYP      !< monvol type (refer to read_monvol.F)
      INTEGER :: IH3D_FLAG !< node group identifier
      INTEGER :: NNODES    !< number of nodes in group
      INTEGER :: IFV       !< FVMBAG identifier in [1, NFVBAG] where NFVBAG <= NVOLU
      my_real :: VALUE     !< polyhedron value
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
              
              IF(FVDATA_P(IFV)%NPOLH > 0)THEN !if GRNOD is not empty and if there are any polyhedron
                                 
                 SELECT CASE (TRIM(KEY))
                 
                   CASE('P','PRES')
                     DO II=1,FVDATA_P(IFV)%NPOLH !loop over polyhedra composing the airbag mesh
                       VALUE = FVDATA_P(IFV)%PPOLH(II)
                       WA4(K_SHIFT+II) = VALUE
                       IS_WRITTEN_NODE_FVM(K_SHIFT+II) = 1
                    ENDDO! next I (next polyhedron)

                   CASE('T','TEMP')
                     DO II=1, FVDATA_P(IFV)%NPOLH !loop over polyhedra composing the airbag mesh
                       VALUE = FVDATA_P(IFV)%TPOLH(II)
                       WA4(K_SHIFT+II) = VALUE
                       IS_WRITTEN_NODE_FVM(K_SHIFT+II) = 1
                    ENDDO! next I (next polyhedron)

                   CASE('D','DENS')
                     DO II=1, FVDATA_P(IFV)%NPOLH !loop over polyhedra composing the airbag mesh
                       VALUE = FVDATA_P(IFV)%RPOLH(II)
                       WA4(K_SHIFT+II) = VALUE
                       IS_WRITTEN_NODE_FVM(K_SHIFT+II) = 1
                    ENDDO! next I (next polyhedron)
                    
                   CASE('SSP')
                     DO II=1, FVDATA_P(IFV)%NPOLH !loop over polyhedra composing the airbag mesh
                       VALUE = FVDATA_P(IFV)%SSPPOLH(II)
                       WA4(K_SHIFT+II) = VALUE
                       IS_WRITTEN_NODE_FVM(K_SHIFT+II) = 1
                    ENDDO! next I (next polyhedron)           
                    
                   CASE('DT')
                     DO II=1, FVDATA_P(IFV)%NPOLH !loop over polyhedra composing the airbag mesh
                       VALUE = FVDATA_P(IFV)%DTPOLH(II)
                       WA4(K_SHIFT+II) = VALUE
                       IS_WRITTEN_NODE_FVM(K_SHIFT+II) = 1
                    ENDDO! next I (next polyhedron)                               

                   CASE('V', 'VOL', 'ZVOL')
                     DO II=1, FVDATA_P(IFV)%NPOLH !loop over polyhedra composing the airbag mesh
                       VALUE = ZERO
                       IF(ABS(FVDATA_P(IFV)%RPOLH(II)) > EM20 )THEN
                         VALUE = FVDATA_P(IFV)%MPOLH(II) / FVDATA_P(IFV)%RPOLH(II)
                       ENDIF
                       WA4(K_SHIFT+II) = VALUE
                       IS_WRITTEN_NODE_FVM(K_SHIFT+II) = 1
                    ENDDO! next I (next polyhedron)                                                    

                END SELECT

 

              ENDIF !NPOLH>0>0 
            ENDIF !IH3D_FLAG>0
            K_SHIFT = K_SHIFT + FVDATA_P(IFV)%NPOLH
         ENDIF ! ITYP
         K1=K1+NIMV

      ENDDO !next NN
      
      ! set minimum value to other nodes in the group (otherwise visualization may lead to unsuitable color legend/gradient)          
      IF(K_SHIFT < AIRBAGS_TOTAL_FVM)THEN                                                                       
        DO II= K_SHIFT+1,AIRBAGS_TOTAL_FVM                                                                      
          WA4(II) = ZERO      
          IS_WRITTEN_NODE_FVM(II) = 0                                                                                                 
        ENDDO                                                                                                                         
      ENDIF        

      
      RETURN
      END
