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
Chd|  MULTI_FACE_ELEM_DATA          source/multifluid/multi_face_data_elem.F
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|        MULTI_TIMEEVOLUTION           source/multifluid/multi_timeevolution.F
Chd|-- calls ---------------
Chd|        CENTROID2                     source/multifluid/centroid.F  
Chd|        CENTROID2T                    source/multifluid/centroid.F  
Chd|        CENTROID3                     source/multifluid/centroid.F  
Chd|        CENTROID3T                    source/multifluid/centroid.F  
Chd|        QNORM2                        source/multifluid/qnorm2.F    
Chd|        QNORM2T                       source/multifluid/qnorm2t.F   
Chd|        SNORM3                        source/multifluid/snorm3.F    
Chd|        SNORM3T                       source/multifluid/snorm3t.F   
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE MULTI_FACE_ELEM_DATA(ITASK, IPARG, IXS, IXQ, IXTG, XGRID, WGRID, MULTI_FVM)
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE MULTI_FVM_MOD
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"      
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
C     NIXS, NIXQ, NIXTG, NPARG
#include      "param_c.inc"
C     NGROUP
#include      "com01_c.inc"
C     NTHREAD
#include      "task_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: ITASK, IPARG(NPARG, *), IXS(NIXS, *), IXQ(NIXQ, *), IXTG(NIXTG, *)
      my_real, INTENT(IN) :: XGRID(3, *), WGRID(3, *)
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NG, MTN, NEL, ITY, ISOLNOD, NFT, JALE
      LOGICAL :: COMPUTE_CENTROID
      
      COMPUTE_CENTROID = (MULTI_FVM%MUSCL > 0) .OR. MULTI_FVM%NS_DIFF 

      DO NG = ITASK + 1, NGROUP, NTHREAD
         MTN = IPARG(1, NG)
         IF (MTN == 151) THEN
            NEL = IPARG(2, NG)
            NFT = IPARG(3, NG)
            ITY = IPARG(5, NG)
            JALE = IPARG(7, NG)
            ISOLNOD = IPARG(28, NG)
            IF (ITY == 1) THEN
C     Solid
               IF (ISOLNOD == 4) THEN
C     TETRA
                  CALL SNORM3T(NEL, NFT, JALE, IXS, XGRID, WGRID, 
     .                 MULTI_FVM%FACE_DATA%NORMAL(1:3, 1:6, 1 + NFT : NEL + NFT), 
     .                 MULTI_FVM%FACE_DATA%WFAC(1:3, 1:6, 1 + NFT : NEL + NFT), 
     .                 MULTI_FVM%FACE_DATA%SURF(1:6, 1 + NFT : NEL + NFT))
                  IF (COMPUTE_CENTROID) THEN
                     CALL CENTROID3T(NEL, MULTI_FVM%NELEM, NFT, IXS, XGRID, 
     .                    MULTI_FVM%ELEM_DATA%CENTROID(:, 1 + NFT : NEL + NFT), 
     .                    MULTI_FVM%FACE_DATA%CENTROID(:, :, 1 + NFT : NEL + NFT))
                  ENDIF
               ELSE 
C     OTHER SOLIDS
                  CALL SNORM3(NEL, NFT, JALE, IXS, XGRID, WGRID, 
     .                 MULTI_FVM%FACE_DATA%NORMAL(1:3, 1:6, 1 + NFT : NEL + NFT), 
     .                 MULTI_FVM%FACE_DATA%WFAC(1:3, 1:6, 1 + NFT : NEL + NFT), 
     .                 MULTI_FVM%FACE_DATA%SURF(1:6, 1 + NFT : NEL + NFT))
C     
                  IF (COMPUTE_CENTROID) THEN
                     CALL CENTROID3(NEL, MULTI_FVM%NELEM, NFT, IXS, XGRID, 
     .                    MULTI_FVM%ELEM_DATA%CENTROID(:, 1 + NFT : NEL + NFT), 
     .                    MULTI_FVM%FACE_DATA%CENTROID(:, :, 1 + NFT : NEL + NFT))
                  ENDIF
               ENDIF
            ELSE IF (ITY == 2) THEN
C     QUAD
               CALL QNORM2(NEL, NFT, JALE, MULTI_FVM%SYM, IXQ, XGRID, WGRID, 
     .              MULTI_FVM%FACE_DATA%NORMAL(1:3, 1:4, 1 + NFT : NEL + NFT), 
     .              MULTI_FVM%FACE_DATA%WFAC(1:3, 1:4, 1 + NFT : NEL + NFT), 
     .              MULTI_FVM%FACE_DATA%SURF(1:4, 1 + NFT : NEL + NFT))
               IF (COMPUTE_CENTROID) THEN
                  CALL CENTROID2(NEL, MULTI_FVM%NELEM, NFT, IXQ, XGRID, 
     .                 MULTI_FVM%ELEM_DATA%CENTROID(:, 1 + NFT : NEL + NFT), 
     .                 MULTI_FVM%FACE_DATA%CENTROID(:, :, 1 + NFT : NEL + NFT))
               ENDIF
            ELSE IF (ITY == 7) THEN
C     TRIANGLE
               CALL QNORM2T(NEL, NFT, JALE, MULTI_FVM%SYM, IXTG, XGRID, WGRID, 
     .              MULTI_FVM%FACE_DATA%NORMAL(1:3, 1:3, 1 + NFT : NEL + NFT), 
     .              MULTI_FVM%FACE_DATA%WFAC(1:3, 1:3, 1 + NFT : NEL + NFT), 
     .              MULTI_FVM%FACE_DATA%SURF(1:3, 1 + NFT : NEL + NFT))
               IF (COMPUTE_CENTROID) THEN
                  CALL CENTROID2T(NEL, MULTI_FVM%NELEM, NFT, IXTG, XGRID, 
     .                 MULTI_FVM%ELEM_DATA%CENTROID(:, 1 + NFT : NEL + NFT), 
     .                 MULTI_FVM%FACE_DATA%CENTROID(:, :, 1 + NFT : NEL + NFT))
               ENDIF
            ENDIF
         ENDIF
      ENDDO      
      END SUBROUTINE MULTI_FACE_ELEM_DATA
