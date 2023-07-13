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
Chd|  VISCMAIN                      source/materials/visc/viscmain.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|-- calls ---------------
Chd|        VISC_PRONY                    source/materials/visc/visc_prony.F
Chd|        VISC_PRONY_LSTRAIN            source/materials/visc/visc_prony_lstrain.F
Chd|====================================================================
      SUBROUTINE VISCMAIN(VISC    ,NEL     ,
     .           NVARVIS ,UVARVIS ,RHO     ,VISCMAX ,SOUNDSP ,TIMESTEP, 
     .           EPSPXX  ,EPSPYY  ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX  , 
     .           SV1     ,SV2     ,SV3     ,SV4     ,SV5     ,SV6     ,
     .           MFXX    ,MFXY    ,MFXZ    ,MFYX    ,MFYY    ,MFYZ    ,
     .           MFZX    ,MFZY    ,MFZZ    ,
     .           S1      ,S2      ,S3      ,S4      ,S5      ,S6      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE VISC_PARAM_MOD
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
      my_real ,INTENT(IN) :: TIMESTEP
      my_real, DIMENSION(NEL) :: RHO,VISCMAX,SOUNDSP,EPSPXX,EPSPYY,EPSPZZ, 
     .   EPSPXY,EPSPYZ,EPSPZX,SV1,SV2,SV3,SV4,SV5,SV6,S1,S2,S3,S4,S5,S6,
     .   MFXX,MFXY,MFXZ,MFYX,MFYY,MFYZ,MFZX,MFZY,MFZZ
      my_real, DIMENSION(NEL*NVARVIS) ,INTENT(INOUT) :: UVARVIS
      TYPE(VISC_PARAM_) ,INTENT(IN) :: VISC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NPRONY
C=======================================================================        
       IF (VISC%ILAW == 1) THEN
c
         NPRONY = VISC%IPARAM(1)
         CALL VISC_PRONY(VISC     ,NPRONY  ,NEL     ,NVARVIS ,UVARVIS ,
     .           EPSPXX  ,EPSPYY  ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX  , 
     .           SV1     ,SV2     ,SV3     ,SV4     ,SV5     ,SV6     ,
     .           TIMESTEP,RHO     ,VISCMAX ,SOUNDSP )
c
       ELSEIF (VISC%ILAW == 2) THEN
c
         NPRONY  = VISC%IPARAM(1)
         CALL VISC_PRONY_LSTRAIN(
     .           VISC    ,NPRONY  ,NEL     ,NVARVIS ,UVARVIS ,
     .           TIMESTEP,MFXX    ,MFXY    ,MFXZ    ,MFYX    ,MFYY    ,
     .           MFYZ    ,MFZX    ,MFZY    ,MFZZ    ,
     .           S1      ,S2      ,S3      ,S4      ,S5      ,S6      )
      ENDIF       
c-----------
      RETURN
      END
