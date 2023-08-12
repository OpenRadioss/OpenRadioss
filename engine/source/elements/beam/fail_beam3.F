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
Chd|  FAIL_BEAM3                    source/elements/beam/fail_beam3.F
Chd|-- called by -----------
Chd|        MAIN_BEAM3                    source/elements/beam/main_beam3.F
Chd|-- calls ---------------
Chd|        FAIL_JOHNSON_B                source/materials/fail/johnson_cook/fail_johnson_b.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE FAIL_BEAM3(ELBUF_STR    ,NUMMAT  ,NPROPM  ,
     .                  NPROPMI ,SBUFMAT ,SNPC    ,STF     ,
     .                  NEL     ,IMAT    ,JTHE    ,DPLA    ,
     .                  TEMPEL  ,NGL     ,PM      ,IPM     ,
     .                  OFF     ,EPSD    ,BUFMAT  ,NPF     ,TF      ,
     .                  TIME    ,IOUT    ,ISTDO   ,
     .                  SVM     ,PRESSURE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include  "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL        ! size of element group
      INTEGER ,INTENT(IN) :: IMAT       ! material law number
      INTEGER ,INTENT(IN) :: JTHE       ! thermal dependency flag
      INTEGER ,INTENT(IN) :: NUMMAT     ! number of defined materials
      INTEGER ,INTENT(IN) :: NPROPM     ! size of real material parameter table
      INTEGER ,INTENT(IN) :: NPROPMI    ! size of int  material parameter table 
      INTEGER ,INTENT(IN) :: SBUFMAT    ! size of material parameter array
      INTEGER ,INTENT(IN) :: SNPC   
      INTEGER ,INTENT(IN) :: STF   
      INTEGER ,INTENT(IN) :: IOUT       ! output file unit
      INTEGER ,INTENT(IN) :: ISTDO      ! output file unit
      INTEGER ,DIMENSION(SNPC) ,INTENT(IN) :: NPF
      INTEGER ,DIMENSION(NEL)  ,INTENT(IN) :: NGL   ! table of element identifiers
      INTEGER ,DIMENSION(NPROPMI,NUMMAT) ,INTENT(IN)    :: IPM
      my_real                            ,INTENT(IN)    :: TIME
      my_real ,DIMENSION(NPROPM ,NUMMAT) ,INTENT(IN)    :: PM
      my_real ,DIMENSION(SBUFMAT)        ,INTENT(IN)    :: BUFMAT
      my_real ,DIMENSION(NEL)            ,INTENT(IN)    :: DPLA
      my_real ,DIMENSION(NEL)            ,INTENT(IN)    :: SVM
      my_real ,DIMENSION(NEL)            ,INTENT(IN)    :: PRESSURE
      my_real ,DIMENSION(NEL)            ,INTENT(IN)    :: EPSD
      my_real ,DIMENSION(NEL)            ,INTENT(IN)    :: TEMPEL
      my_real ,DIMENSION(STF)            ,INTENT(IN)    :: TF
      my_real ,DIMENSION(NEL)            ,INTENT(INOUT) :: OFF
C
      TYPE (ELBUF_STRUCT_), INTENT(INOUT) :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IFUNC(100)
      INTEGER :: I,IFL,IADBUF,NFUNC,NPARAM,IRUPT
      INTEGER :: II(3)
      my_real :: T0,TM
      my_real ,DIMENSION(NEL) :: TSTAR
      my_real :: bidon
C
      my_real, DIMENSION(:) ,POINTER :: UPARAMF
C=======================================================================
c      to avoid compilation error with unused arguments
c      they will be necessary for next development step
      bidon = npf(1)
      bidon = tf(1)
c-----------------------------------------------------
      IFL  = 1              ! only one failure model for beams      
C--------------------------------------   
      DO I=1,3
        II(I) = NEL*(I-1)
      ENDDO
      NPARAM  = IPM(112, IMAT)
      NFUNC   = IPM(115, IMAT)
      IADBUF  = IPM(114 ,IMAT)
      IRUPT   = IPM(242 ,IMAT)                           
c      UPARAMF => BUFMAT(IADBUF:IADBUF+NPARAM)
      DO I=1,NFUNC                
        IFUNC(I) = IPM(115 + I,IMAT)
      ENDDO 
c-------------------------------------
c     progressive element erosion      
c
      DO I = 1,NEL
        IF (OFF(I) < EM01)  OFF(I) = ZERO
        IF (OFF(I) < ONE )  OFF(I) = OFF(I)*FOUR_OVER_5
      ENDDO      
c-----------------------------------------            
      IRUPT = ELBUF_STR%GBUF%FAIL(1)%ILAWF                                
c-----------------------------------------            
      SELECT CASE (IRUPT)                                            
c------------------------------------            
c
        CASE (1)     !    Johnson-Cook                                                   
          !  Tstar computation for Jhonson-Cook failure : T* = (T-T0)/(TM-T0)
          IF (JTHE > 0) THEN
            T0 = PM(79, IMAT) 
            TM = PM(80, IMAT) 
            DO I=1,NEL  
              TSTAR(I) = MAX(ZERO,(TEMPEL(I)-T0)/(TM-T0))
            ENDDO
          ELSE
            TSTAR(1:NEL) = ZERO
          ENDIF

          CALL FAIL_JOHNSON_B(NEL      ,NGL      ,NPARAM   ,BUFMAT(IADBUF)  ,
     .                        TIME     ,TSTAR    ,SVM      ,PRESSURE ,
     .                        DPLA     ,EPSD     ,OFF      ,ELBUF_STR%GBUF%FAIL(1)%DAMMX,
     .                        ELBUF_STR%GBUF%FAIL(1)%TDEL     ,IOUT     ,ISTDO    )
c
c-------------
      END SELECT
c-----------
      RETURN
      END SUBROUTINE FAIL_BEAM3
