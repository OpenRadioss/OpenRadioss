Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    fail_beam3          ../engine/source/elements/beam/fail_beam3.F
      !||--- called by ------------------------------------------------------
      !||    main_beam3          ../engine/source/elements/beam/main_beam3.F
      !||--- calls      -----------------------------------------------------
      !||    fail_biquad_b       ../engine/source/materials/fail/biquad/fail_biquad_b.F
      !||    fail_energy_b       ../engine/source/materials/fail/energy/fail_energy_b.F
      !||    fail_gene1_b        ../engine/source/materials/fail/gene1/fail_gene1_b.F90
      !||    fail_inievo_b       ../engine/source/materials/fail/inievo/fail_inievo_b.F90
      !||    fail_johnson_b      ../engine/source/materials/fail/johnson_cook/fail_johnson_b.F
      !||    fail_tab2_b         ../engine/source/materials/fail/tabulated/fail_tab2_b.F90
      !||    fail_tensstrain_b   ../engine/source/materials/fail/tensstrain/fail_tensstrain_b.F
      !||    fail_visual_b       ../engine/source/materials/fail/visual/fail_visual_b.F90
      !||    nvar                ../engine/source/input/nvar.F
      !||--- uses       -----------------------------------------------------
      !||    elbufdef_mod        ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    fail_gene1_b_mod    ../engine/source/materials/fail/gene1/fail_gene1_b.F90
      !||    fail_inievo_b_mod   ../engine/source/materials/fail/inievo/fail_inievo_b.F90
      !||    fail_tab2_b_mod     ../engine/source/materials/fail/tabulated/fail_tab2_b.F90
      !||    fail_visual_b_mod   ../engine/source/materials/fail/visual/fail_visual_b.F90
      !||    mat_elem_mod        ../common_source/modules/mat_elem/mat_elem_mod.F90
      !||====================================================================
      SUBROUTINE FAIL_BEAM3(ELBUF_STR    ,FAIL    ,NUMMAT  ,
     .                  NPROPM  ,SNPC    ,STF     ,
     .                  NEL     ,IMAT    ,JTHE    ,DPLA    ,
     .                  TEMPEL  ,NGL     ,PM      ,
     .                  OFF     ,EPSD    ,NPF     ,TF      ,
     .                  TIME    ,IOUT    ,ISTDO   ,
     .                  SVM     ,PRESSURE,AREA    ,AL      ,
     .                  F1      ,F2      ,F3      ,M1      ,M2      ,
     .                  M3      ,ISMSTR  ,EPSXX   ,EPSXY   ,EPSXZ   ,
     .                  KXX     ,KYY     ,KZZ     ,DTIME   , 
     .                 NTABLE   ,TABLE   ,PLA     ,SIGY    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MAT_ELEM_MOD
      USE ELBUFDEF_MOD
      USE FAIL_VISUAL_B_MOD
      USE FAIL_GENE1_B_MOD
      USE FAIL_INIEVO_B_MOD
      USE FAIL_TAB2_B_MOD
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
      INTEGER ,INTENT(IN) :: SNPC   
      INTEGER ,INTENT(IN) :: STF   
      INTEGER ,INTENT(IN) :: IOUT       ! output file unit
      INTEGER ,INTENT(IN) :: ISTDO      ! output file unit
      INTEGER ,INTENT(IN) :: ISMSTR
      INTEGER ,DIMENSION(SNPC) ,INTENT(IN) :: NPF
      INTEGER ,DIMENSION(NEL)  ,INTENT(IN) :: NGL   ! table of element identifiers
      my_real                            ,INTENT(IN)    :: TIME
      my_real                            ,INTENT(IN)    :: AREA
      my_real ,DIMENSION(NPROPM ,NUMMAT) ,INTENT(IN)    :: PM
      my_real ,DIMENSION(NEL)            ,INTENT(IN)    :: DPLA
      my_real ,DIMENSION(NEL)            ,INTENT(IN)    :: AL
      my_real ,DIMENSION(NEL)            ,INTENT(IN)    :: SVM
      my_real ,DIMENSION(NEL)            ,INTENT(IN)    :: PRESSURE
      my_real ,DIMENSION(NEL)            ,INTENT(IN)    :: EPSD
      my_real ,DIMENSION(NEL)            ,INTENT(IN)    :: EPSXX,EPSXY,EPSXZ 
      my_real ,DIMENSION(NEL)            ,INTENT(IN)    :: KXX,KYY,KZZ 
      my_real ,DIMENSION(NEL)            ,INTENT(IN)    :: TEMPEL
      my_real ,DIMENSION(NEL)            ,INTENT(INOUT) :: F1,F2,F3
      my_real ,DIMENSION(NEL)            ,INTENT(INOUT) :: M1,M2,M3

      my_real ,DIMENSION(STF)            ,INTENT(IN)    :: TF
      my_real ,DIMENSION(NEL)            ,INTENT(INOUT) :: OFF
      my_real ,DIMENSION(NEL)            ,INTENT(IN)    :: PLA      ! the plastic strain of the current element
      my_real                            ,INTENT(IN)    :: DTIME         
      INTEGER                            ,INTENT(IN)    :: NTABLE
      TYPE(TTABLE), DIMENSION(NTABLE), INTENT(INOUT)    :: TABLE      ! TABLE DATA        
      my_real ,DIMENSION(NEL)            ,INTENT(IN)    :: SIGY                                                
C                                                         
      TYPE (ELBUF_STRUCT_), INTENT(INOUT) :: ELBUF_STR
      TYPE (FAIL_PARAM_)   ,INTENT(IN)    :: FAIL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: IFUNC(100)
      INTEGER :: I,IFL,NFUNC,NPARAM,IRUPT
      my_real :: T0,TM,G_MODULE
      my_real ,DIMENSION(NEL) :: TSTAR
      my_real :: bidon
C
      my_real, DIMENSION(:) ,POINTER :: UPARAMF
C=======================================================================
c      to avoid compilation error with unused arguments
c      they will be necessary for next development step
c-----------------------------------------------------
      IFL  = 1              ! only one failure model for beams      
C--------------------------------------   
      NPARAM  = FAIL%NUPARAM
      NFUNC   = FAIL%NFUNC
c-----------------------------------------            
      IRUPT = ELBUF_STR%GBUF%FAIL(1)%ILAWF    
c-------------------------------------
c     progressive element erosion      
c
      DO I = 1,NEL   

            IF (OFF(I) < EM01)  OFF(I) = ZERO
            IF (OFF(I) < ONE )  OFF(I) = OFF(I)*FOUR_OVER_5
      ENDDO      
                            
c-----------------------------------------            
      SELECT CASE (IRUPT)                                            
c------------------------------------   
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
          CALL FAIL_JOHNSON_B(NEL      ,NGL      ,NPARAM   ,FAIL%UPARAM,
     .                        TIME     ,TSTAR    ,SVM      ,PRESSURE ,
     .                        DPLA     ,EPSD     ,OFF      ,ELBUF_STR%GBUF%FAIL(1)%DAMMX,
     .                        ELBUF_STR%GBUF%FAIL(1)%TDEL  ,IOUT    ,ISTDO    )
c---------------      
        CASE (10)     !    Tension Strain failure model
          IF (JTHE > 0) THEN
            T0 = PM(79, IMAT) 
            TM = PM(80, IMAT) 
            DO I=1,NEL  
              TSTAR(I) = MAX(ZERO,(TEMPEL(I)-T0)/(TM-T0))
            ENDDO
          ELSE
            TSTAR(1:NEL) = ZERO
          ENDIF
          
          CALL FAIL_TENSSTRAIN_B(                                         
     .          NEL      ,NGL      ,NPARAM   ,FAIL%UPARAM,
     .          TIME     ,EPSD     ,OFF      ,ELBUF_STR%GBUF%FAIL(1)%DAMMX,  
     .          ELBUF_STR%GBUF%FAIL(1)%TDEL,IOUT     ,ISTDO    ,FAIL%IFUNC    ,
     .          EPSXX    ,AL       ,TSTAR    ,
     .          SNPC     ,NPF      ,STF      ,
     .          TF       ,ISMSTR   ,ELBUF_STR%GBUF%FAIL(1)%NVAR, ELBUF_STR%GBUF%FAIL(1)%VAR,
     .          ELBUF_STR%GBUF%DMGSCL)     
        
c--------------- 
        CASE (11)     ! Energy failure model                                        
          CALL FAIL_ENERGY_B(NEL      ,NGL      ,NPARAM   ,FAIL%UPARAM,
     .                       TIME     ,EPSD     ,OFF      ,ELBUF_STR%GBUF%FAIL(1)%DAMMX,                        
     .                       ELBUF_STR%GBUF%FAIL(1)%TDEL  ,IOUT     ,ISTDO    ,FAIL%IFUNC   ,
     .                       AREA     ,F1       ,F2       ,F3       ,
     .                       M1       ,M2       ,M3       ,SNPC     ,NPF    ,STF     ,
     .                       TF       ,EPSXX    ,EPSXY    ,EPSXZ    ,KXX    ,KYY     ,
     .                       KZZ      ,ELBUF_STR%GBUF%FAIL(1)%NVAR,ELBUF_STR%GBUF%FAIL(1)%VAR,
     .                       ELBUF_STR%GBUF%DMGSCL)
c--------------- 

        CASE (30)     !    BIQUAD                                                  
          CALL FAIL_BIQUAD_B(NEL      ,NGL      ,NPARAM   ,FAIL%UPARAM,
     .                       TIME         ,SVM      ,PRESSURE ,
     .                       DPLA         ,OFF      ,ELBUF_STR%GBUF%FAIL(1)%DAMMX,
     .                       ELBUF_STR%GBUF%FAIL(1)%TDEL     ,IOUT    ,ISTDO  ,FAIL%IFUNC   ,
     .                       NFUNC,SNPC     ,NPF     ,STF     ,TF,
     .                       ELBUF_STR%GBUF%FAIL(1)%NVAR, ELBUF_STR%GBUF%FAIL(1)%VAR,AL,
     .                       ELBUF_STR%GBUF%DMGSCL)
c---------------   
        CASE (36)     !    VISUAL                                           
         CALL FAIL_VISUAL_B(NEL      ,NGL      ,NPARAM   ,FAIL%UPARAM,
     .                       TIME     ,ELBUF_STR%GBUF%FAIL(1)%DAMMX,                        
     .                       IOUT     ,ISTDO    ,
     .                       AREA     ,F1       ,
     .                       EPSXX    ,EPSXY    ,EPSXZ    ,
     .                       ELBUF_STR%GBUF%FAIL(1)%NVAR,
     .                       ELBUF_STR%GBUF%FAIL(1)%VAR,ISMSTR, DTIME)
      
         CASE (39)     ! Gene1 failure model                                        
          CALL FAIL_GENE1_B(                         
     .                       NEL  ,FAIL%NUPARAM  ,ELBUF_STR%GBUF%FAIL(1)%NVAR  ,FAIL%NFUNC,   
     .                FAIL%IFUNC  ,NPF           ,TF           ,            
     .                      TIME  ,DTIME         ,FAIL%UPARAM  ,    
     .                       NGL  ,ELBUF_STR%GBUF%DT    ,EPSD  ,ELBUF_STR%GBUF%FAIL(1)%VAR ,
     .                       OFF  ,
     .                      AREA  ,F1       ,
     .                     EPSXX  ,EPSXY    ,EPSXZ      ,
     .                     TEMPEL ,   
     .                     ELBUF_STR%GBUF%FAIL(1)%DAMMX ,AL    ,TABLE,     
     .                     FAIL%NTABLE,    
     .                     FAIL%TABLE ,ELBUF_STR%GBUF%FAIL(1)%LF_DAMMX  ,FAIL%NIPARAM  ,FAIL%IPARAM,    
     .                     SNPC       ,STF  ,NTABLE)

          CASE (42)     ! INIEVO failure model  
            CALL FAIL_INIEVO_B(                         
     .                       NEL  ,FAIL%NUPARAM ,ELBUF_STR%GBUF%FAIL(1)%NVAR                  ,
     .                     TABLE  ,FAIL%NTABLE  ,FAIL%TABLE ,TIME    ,FAIL%UPARAM             ,
     .                       NGL  ,AL           ,DPLA       ,EPSD , ELBUF_STR%GBUF%FAIL(1)%VAR,
     .                        F1  ,AREA         ,
     .                       PLA  ,SIGY         ,OFF        ,ELBUF_STR%GBUF%FAIL(1)%DAMMX     ,
     .                    ELBUF_STR%GBUF%FAIL(1)%TDEL , 
     .                    ELBUF_STR%GBUF%FAIL(1)%DAMINI,
     .                    NTABLE,ELBUF_STR%GBUF%DMGSCL)

         CASE (41)     ! TAB2 failure model    
           CALL FAIL_TAB2_B(                         
     .                       NEL  ,FAIL%NUPARAM ,ELBUF_STR%GBUF%FAIL(1)%NVAR   ,FAIL%NFUNC  ,FAIL%IFUNC  ,
     .                       NPF  ,TABLE   ,TF        ,TIME  ,FAIL%UPARAM  ,
     .                       NGL  ,AL           ,DPLA  ,EPSD  ,ELBUF_STR%GBUF%FAIL(1)%VAR,  
     .                        F1  ,AREA         ,
     .                    TEMPEL  ,OFF          ,ELBUF_STR%GBUF%FAIL(1)%DAMMX ,ELBUF_STR%GBUF%FAIL(1)%TDEL  ,
     .                FAIL%NTABLE ,FAIL%TABLE,
     .                      SNPC  ,STF  ,NTABLE ,ELBUF_STR%GBUF%DMGSCL)
c-------------
      END SELECT
c-----------
      DO I= 1,NEL
          IF (OFF(I) == FOUR_OVER_5) THEN
#include "lockon.inc"
            WRITE(IOUT, 1000) NGL(I),TIME
            WRITE(ISTDO,1000) NGL(I),TIME
#include "lockoff.inc" 
          END IF
      END DO
c------------------
 1000 FORMAT(1X,'DELETED BEAM ELEMENT in 1',I10,1X,'AT TIME :',1PE12.4)


      RETURN
      END SUBROUTINE FAIL_BEAM3
