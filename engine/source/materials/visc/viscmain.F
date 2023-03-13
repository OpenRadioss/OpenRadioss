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
Chd|====================================================================
      SUBROUTINE VISCMAIN(
     .           NEL     ,IMAT    ,IVISC   ,IPM     ,UPARAM  ,
     .           UVAR    ,RHO     ,VISCMAX ,SOUNDSP ,TIMESTEP, 
     .           EPSPXX  ,EPSPYY  ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX  , 
     .           SV1     ,SV2     ,SV3     ,SV4     ,SV5     ,SV6     )
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
      INTEGER NEL,IMAT,IVISC,IADBUF
      INTEGER IPM(NPROPMI,*)
      my_real
     .   UPARAM(*),UVAR(*),RHO(NEL),VISCMAX(NEL),
     .   SOUNDSP(NEL),EPSPXX(NEL),EPSPYY(NEL),EPSPZZ(NEL), 
     .   EPSPXY(NEL),EPSPYZ(NEL),EPSPZX(NEL),
     .   SV1(NEL),SV2(NEL),SV3(NEL),SV4(NEL),SV5(NEL),SV6(NEL)
      my_real
     .   TIMESTEP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J IADBUF, NUVAR, NPRONY
C=======================================================================        
       IADBUF = IPM(223,IMAT)  
       NUVAR  = IPM(225,IMAT)  
c
       IF (IVISC == 1) THEN
         NPRONY  = NINT(UPARAM(IADBUF + 1))
         CALL VISC_PRONY(
     .           NEL     ,NUVAR   ,NPRONY  ,IADBUF  ,UPARAM  ,UVAR    ,
     .           EPSPXX  ,EPSPYY  ,EPSPZZ  ,EPSPXY  ,EPSPYZ  ,EPSPZX  , 
     .           SV1     ,SV2     ,SV3     ,SV4     ,SV5     ,SV6     ,
     .           TIMESTEP,RHO     ,VISCMAX ,SOUNDSP )
       ENDIF       
c------------
       RETURN
       END
