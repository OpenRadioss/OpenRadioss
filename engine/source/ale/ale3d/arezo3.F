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
Chd|  AREZO3                        source/ale/ale3d/arezo3.F     
Chd|-- called by -----------
Chd|        AREZON                        source/ale/arezon.F           
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE AREZO3(ALE_CONNECT,VAR,NV,PHI,NDIM,FLUX,VOL,IAD22,IXS,ITRIMAT)
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NDIM,NV,ITRIMAT
      INTEGER :: IXS(NIXS,*)
      my_real :: VAR(NDIM,*), PHI(*), FLUX(6,MVSIZ), VOL(MVSIZ), IAD22(MVSIZ)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IE, IV1, IV2, IV3, IV4, IV5, IV6, IAD2
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------

C NOT YET TREATED WITH INTER22, MUST STACK REZONED VALUE WITH SUPERCELLS LIKE FOR CONVECTION

      IF(INT22==0)THEN
        DO I=LFT,LLT
          IE =NFT+I
          IAD2 = ALE_CONNECT%ee_connect%iad_connect(IE)
          IV1=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
          IV2=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
          IV3=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
          IV4=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
          IV5=ALE_CONNECT%ee_connect%connected(IAD2 + 5 - 1)
          IV6=ALE_CONNECT%ee_connect%connected(IAD2 + 6 - 1)
          IF(IV1.LE.0)IV1=IE
          IF(IV2.LE.0)IV2=IE
          IF(IV3.LE.0)IV3=IE
          IF(IV4.LE.0)IV4=IE
          IF(IV5.LE.0)IV5=IE
          IF(IV6.LE.0)IV6=IE
          IF (VOL(I) > ZERO) VAR(NV,I) = VAR(NV,I) + HALF*DT1*(PHI(IE)*
     2     (FLUX(1,I)+FLUX(2,I)+FLUX(3,I)+FLUX(4,I)+FLUX(5,I)+FLUX(6,I))
     3     -PHI(IV1)*FLUX(1,I)-PHI(IV2)*FLUX(2,I)-PHI(IV3)*FLUX(3,I)
     4     -PHI(IV4)*FLUX(4,I)-PHI(IV5)*FLUX(5,I)-PHI(IV6)*FLUX(6,I))/
     5      VOL(I)
        ENDDO
      ELSE
        DO I=LFT,LLT
          IE =NFT+I
          IAD2 = ALE_CONNECT%ee_connect%iad_connect(IE)
          IF(IAD22(I)==ZERO)CYCLE
          IV1=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
          IV2=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
          IV3=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
          IV4=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
          IV5=ALE_CONNECT%ee_connect%connected(IAD2 + 5 - 1)
          IV6=ALE_CONNECT%ee_connect%connected(IAD2 + 6 - 1) 
          IF(IV1.LE.0)IV1=IE
          IF(IV2.LE.0)IV2=IE
          IF(IV3.LE.0)IV3=IE
          IF(IV4.LE.0)IV4=IE
          IF(IV5.LE.0)IV5=IE
          IF(IV6.LE.0)IV6=IE
          IF (VOL(I) > ZERO) VAR(NV,I) = VAR(NV,I) + HALF*DT1*(PHI(IE)*
     2     (FLUX(1,I)+FLUX(2,I)+FLUX(3,I)+FLUX(4,I)+FLUX(5,I)+FLUX(6,I))
     3     -PHI(IV1)*FLUX(1,I)-PHI(IV2)*FLUX(2,I)-PHI(IV3)*FLUX(3,I)
     4     -PHI(IV4)*FLUX(4,I)-PHI(IV5)*FLUX(5,I)-PHI(IV6)*FLUX(6,I))/
     5      VOL(I)
        ENDDO       
      ENDIF
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  AREZO3_SIG                    source/ale/ale3d/arezo3.F     
Chd|-- called by -----------
Chd|        AREZON                        source/ale/arezon.F           
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE AREZO3_SIG(ALE_CONNECT,SIG,NV,PHI,NDIM,FLUX,
     .                      VOL,IAD22,IXS,ITRIMAT,NEL)
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NDIM,NV,ITRIMAT,NEL
      INTEGER :: IXS(NIXS,*)
      my_real :: SIG(NEL,NDIM), PHI(*), FLUX(6,MVSIZ), VOL(MVSIZ), IAD22(MVSIZ)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IE, IV1, IV2, IV3, IV4, IV5, IV6, IAD2
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------

C NOT YET TREATED WITH INTER22, MUST STACK REZONED VALUE WITH SUPERCELLS LIKE FOR CONVECTION

      IF(INT22==0)THEN
        DO I=LFT,LLT
          IE =NFT+I
          IAD2 = ALE_CONNECT%ee_connect%iad_connect(IE)
          IV1=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
          IV2=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
          IV3=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
          IV4=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
          IV5=ALE_CONNECT%ee_connect%connected(IAD2 + 5 - 1)
          IV6=ALE_CONNECT%ee_connect%connected(IAD2 + 6 - 1)
          IF(IV1.LE.0)IV1=IE
          IF(IV2.LE.0)IV2=IE
          IF(IV3.LE.0)IV3=IE
          IF(IV4.LE.0)IV4=IE
          IF(IV5.LE.0)IV5=IE
          IF(IV6.LE.0)IV6=IE
          IF (VOL(I) > ZERO) SIG(I,NV) = SIG(I,NV) + HALF*DT1*(PHI(IE)*
     2     (FLUX(1,I)+FLUX(2,I)+FLUX(3,I)+FLUX(4,I)+FLUX(5,I)+FLUX(6,I))
     3     -PHI(IV1)*FLUX(1,I)-PHI(IV2)*FLUX(2,I)-PHI(IV3)*FLUX(3,I)
     4     -PHI(IV4)*FLUX(4,I)-PHI(IV5)*FLUX(5,I)-PHI(IV6)*FLUX(6,I))/
     5      VOL(I)
        ENDDO
      ELSE
        DO I=LFT,LLT
          IE =NFT+I
          IAD2 = ALE_CONNECT%ee_connect%iad_connect(IE)
          IF(IAD22(I)==ZERO)CYCLE
          IV1=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
          IV2=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
          IV3=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
          IV4=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
          IV5=ALE_CONNECT%ee_connect%connected(IAD2 + 5 - 1)
          IV6=ALE_CONNECT%ee_connect%connected(IAD2 + 6 - 1)
          IF(IV1.LE.0)IV1=IE
          IF(IV2.LE.0)IV2=IE
          IF(IV3.LE.0)IV3=IE
          IF(IV4.LE.0)IV4=IE
          IF(IV5.LE.0)IV5=IE
          IF(IV6.LE.0)IV6=IE
          IF (VOL(I) > ZERO) SIG(I,NV) = SIG(I,NV) + HALF*DT1*(PHI(IE)*
     2     (FLUX(1,I)+FLUX(2,I)+FLUX(3,I)+FLUX(4,I)+FLUX(5,I)+FLUX(6,I))
     3     -PHI(IV1)*FLUX(1,I)-PHI(IV2)*FLUX(2,I)-PHI(IV3)*FLUX(3,I)
     4     -PHI(IV4)*FLUX(4,I)-PHI(IV5)*FLUX(5,I)-PHI(IV6)*FLUX(6,I))/
     5      VOL(I)
        ENDDO       
      ENDIF
C-----------
      RETURN
      END

