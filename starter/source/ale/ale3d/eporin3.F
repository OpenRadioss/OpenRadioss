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
Chd|  EPORIN3                       source/ale/ale3d/eporin3.F    
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE EPORIN3(IXS ,VEUL,ALE_CONNECTIVITY,GEO,NFT,NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IXS(NIXS,NUMELS), NFT, NEL
      my_real :: GEO(NPROPG,NUMGEO),VEUL(LVEUL,*)
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,II,J,NG,JAD,JVOI,IGT,NG1,IAD1
      my_real :: NX,NY,NZ,PORO
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO I=1,NEL
        II=NFT+I
        IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(II)
        DO J=1,6
          JAD=13+J
          NX=VEUL(JAD  ,II)
          NY=VEUL(JAD+6,II)
          NZ=VEUL(JAD+12,II)
          PORO=ONE
          NG=IXS(10,II)
          IF(GEO(12,NG) == 15)THEN
            ! --- Normal vector is modified for porous element
            PORO=GEO(21,NG)
            NX=NX*PORO
            NY=NY*PORO
            NZ=NZ*PORO
          ENDIF
          JVOI = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J - 1)
          ! --- Normal vector is modified if adjacent element is porous
          IF(JVOI > 0)THEN
            NG1=IXS(10,JVOI)
            IF(NG1 /= 0.AND.NG1 /= NG)THEN
              IGT=INT(GEO(12,NG1))
              IF(IGT  == 15.AND.GEO(21,NG1) < PORO) THEN
               NX=NX*GEO(21,NG1)
               NY=NY*GEO(21,NG1)
               NZ=NZ*GEO(21,NG1)
              ENDIF
            ENDIF
          ENDIF
          VEUL(JAD,II)=NX
          VEUL(JAD+6,II)=NY
          VEUL(JAD+12,II)=NZ
        END DO !J=1,6
      END DO !I=1,NEL
C-----------------------------------------------      
      RETURN
      END
