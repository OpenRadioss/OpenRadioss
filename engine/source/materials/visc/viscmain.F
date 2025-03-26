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
      !||    viscmain                  ../engine/source/materials/visc/viscmain.F
      !||--- called by ------------------------------------------------------
      !||    mmain                     ../engine/source/materials/mat_share/mmain.F90
      !||    mulaw                     ../engine/source/materials/mat_share/mulaw.F90
      !||    usermat_solid             ../engine/source/materials/mat_share/usermat_solid.F
      !||--- calls      -----------------------------------------------------
      !||    damping_range_solid       ../engine/source/general_controls/damping/damping_range_solid.F90
      !||    visc_plas                 ../engine/source/materials/visc/visc_plas.F90
      !||    visc_prony                ../engine/source/materials/visc/visc_prony.F
      !||    visc_prony_lstrain        ../engine/source/materials/visc/visc_prony_lstrain.F
      !||--- uses       -----------------------------------------------------
      !||    damping_range_solid_mod   ../engine/source/general_controls/damping/damping_range_solid.F90
      !||    elbufdef_mod              ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    visc_param_mod            ../common_source/modules/mat_elem/visc_param_mod.F90
      !||    visc_plas_mod             ../engine/source/materials/visc/visc_plas.F90
      !||====================================================================
      SUBROUTINE VISCMAIN(VISC    ,NEL     ,
     .           NVARVIS ,UVARVIS ,RHO     ,VISCMAX ,SOUNDSP ,TIMESTEP, 
     .           EPSPXX  ,EPSPYY  ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX  , 
     .           DEPSXX  ,DEPSYY  ,DEPSZZ  ,DEPSXY  ,DEPSYZ  ,DEPSZX  ,
     .           SVO1    ,SVO2    ,SVO3    ,SVO4    ,SVO5    ,SVO6      ,
     .           SV1     ,SV2     ,SV3     ,SV4     ,SV5     ,SV6     ,
     .           MFXX    ,MFXY    ,MFXZ    ,MFYX    ,MFYY    ,MFYZ    ,
     .           MFZX    ,MFZY    ,MFZZ    ,
     .           S1      ,S2      ,S3      ,S4      ,S5      ,S6      ,
     .           DAMP_BUF,IDAMP_FREQ_RANGE ,MVSIZ   ,ET      ,YOUNG   ,
     .           SHEAR_MODULUS,IVISC ) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE VISC_PARAM_MOD
      USE ELBUFDEF_MOD, only: buf_damp_range_
      USE damping_range_solid_mod
      USE visc_plas_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NEL,NVARVIS
      INTEGER ,INTENT(IN) :: IDAMP_FREQ_RANGE
      INTEGER ,INTENT(IN) :: MVSIZ
      INTEGER ,INTENT(IN) :: IVISC
      my_real ,INTENT(IN) :: TIMESTEP
      my_real ,INTENT(IN) :: YOUNG
      my_real, INTENT(IN) :: SHEAR_MODULUS
      my_real, DIMENSION(NEL), INTENT(INOUT):: S1,S2,S3,S4,S5,S6
     .                            
      my_real, DIMENSION(NEL) ,INTENT(INOUT) :: SOUNDSP, VISCMAX
      my_real, DIMENSION(NEL) ,INTENT(IN) :: EPSPXX,EPSPYY,EPSPZZ,EPSPXY,EPSPYZ,EPSPZX,
     .                                       MFXX,MFXY,MFXZ,MFYX,MFYY,MFYZ,MFZX,MFZY,MFZZ,
     .                                       DEPSXX,DEPSYY,DEPSZZ,DEPSXY,DEPSYZ,DEPSZX, 
     .                                       SVO1,SVO2,SVO3,SVO4,SVO5,SVO6,RHO
      my_real, DIMENSION(NEL) ,INTENT(OUT) :: SV1,SV2,SV3,SV4,SV5,SV6

      my_real, DIMENSION(NEL*NVARVIS) ,INTENT(INOUT) :: UVARVIS
      my_real, DIMENSION(MVSIZ) ,INTENT(IN) :: ET      
      TYPE(VISC_PARAM_) ,INTENT(IN) :: VISC
      TYPE(buf_damp_range_) ,INTENT(IN) :: DAMP_BUF      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NPRONY,J,NVAR_DAMP
      my_real, DIMENSION(NEL) :: SDAMP1,SDAMP2,SDAMP3,SDAMP4,SDAMP5,SDAMP6
C======================================================================= 
C  
      IF (IDAMP_FREQ_RANGE > 0) THEN
        NVAR_DAMP = 21
        SV1(1:NEL) = ZERO
        SV2(1:NEL) = ZERO
        SV3(1:NEL) = ZERO
        SV4(1:NEL) = ZERO
        SV5(1:NEL) = ZERO
        SV6(1:NEL) = ZERO
      ELSE
        NVAR_DAMP = 0
      ENDIF
C
      IF (IVISC > 0) THEN    
C--     /VISC PRONY     
        IF (VISC%ILAW == 1) THEN
c
          NPRONY = VISC%IPARAM(1)
          CALL VISC_PRONY(VISC     ,NPRONY  ,NEL     ,NVARVIS ,UVARVIS ,
     .           EPSPXX  ,EPSPYY  ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX  , 
     .           SV1     ,SV2     ,SV3     ,SV4     ,SV5     ,SV6     ,
     .           TIMESTEP,RHO     ,VISCMAX ,SOUNDSP ,NVAR_DAMP)
c
        ELSEIF (VISC%ILAW == 2) THEN
c
          NPRONY  = VISC%IPARAM(1)
          CALL VISC_PRONY_LSTRAIN(
     .           VISC    ,NPRONY  ,NEL     ,NVARVIS ,UVARVIS ,
     .           TIMESTEP,MFXX    ,MFXY    ,MFXZ    ,MFYX    ,MFYY    ,
     .           MFYZ    ,MFZX    ,MFZY    ,MFZZ    ,
     .           S1      ,S2      ,S3      ,S4      ,S5      ,S6)
        ELSEIF (VISC%ILAW == 3) THEN
          call visc_plas(
     .           visc    ,nel      , rho     ,soundsp   , 
     .           depsxx  ,depsyy   ,depszz   ,depsxy    ,depsyz    ,depszx,
     .           svo1    ,svo2     ,svo3     ,svo4      ,svo5      , svo6,               
     .           sv1      ,sv2     ,sv3      ,sv4       ,sv5        ,sv6)    
        ENDIF  
      ENDIF
C      
C--   Damping frequency range - same formulation as visc/prony with 3 components
      IF (IDAMP_FREQ_RANGE > 0) THEN   
        call damping_range_solid(damp_buf,nel     ,nvarvis ,uvarvis ,nvar_damp  ,et   ,
     .                           epspxx  ,epspyy  ,epspzz  ,epspxy  ,epspyz  ,epspzx  ,
     .                           sdamp1  ,sdamp2  ,sdamp3  ,sdamp4  ,sdamp5  ,sdamp6  ,
     .                           timestep,rho     ,soundsp ,young   ,shear_modulus)       
        SV1(1:NEL) = SV1(1:NEL) + SDAMP1(1:NEL)
        SV2(1:NEL) = SV2(1:NEL) + SDAMP2(1:NEL)
        SV3(1:NEL) = SV3(1:NEL) + SDAMP3(1:NEL)
        SV4(1:NEL) = SV4(1:NEL) + SDAMP4(1:NEL)
        SV5(1:NEL) = SV5(1:NEL) + SDAMP5(1:NEL)
        SV6(1:NEL) = SV6(1:NEL) + SDAMP6(1:NEL)   
      ENDIF
      
c-----------
      RETURN
      END
