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
Chd|  BREZO2                        source/ale/ale2d/brezo2.F     
Chd|-- called by -----------
Chd|        AREZON                        source/ale/arezon.F           
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE BREZO2(ALE_CONNECT,VAR,NV,PHI,NDIM,FLUX,VOL,BHOLE,NM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDIM,NV
      INTEGER BHOLE(NMULT,*),NM
      my_real VAR(NDIM,*), PHI(*), FLUX(4,*), VOL(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IE, IV1, IV2, IV3, IV4, IAD2
      my_real VOLN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IF(JMULT == 0)THEN
       DO 10 I=LFT,LLT
       IE =NFT+I
       IAD2 = ALE_CONNECT%ee_connect%iad_connect(IE)
       IV1=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
       IV2=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
       IV3=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
       IV4=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
       IF(IV1 <= 0.)THEN
         IV1=IE       
       ELSEIF(BHOLE(NM,IE) < 0. .AND.ABS(BHOLE(NM,IV1)) /= ABS(BHOLE(NM,IE)))THEN
         IV1=IE
       ENDIF
       IF(IV2 <= 0)THEN
         IV2=IE       
       ELSEIF(BHOLE(NM,IE) < 0. .AND.ABS(BHOLE(NM,IV2)) /= ABS(BHOLE(NM,IE)))THEN
         IV2=IE
       ENDIF
       IF(IV3 <= 0)THEN
         IV3=IE       
       ELSEIF(BHOLE(NM,IE) < 0. .AND.ABS(BHOLE(NM,IV3)) /= ABS(BHOLE(NM,IE)))THEN
         IV3=IE
       ENDIF
       IF(IV4 <= 0)THEN
         IV4=IE
       ELSEIF(BHOLE(NM,IE) < 0. .AND.ABS(BHOLE(NM,IV4)) /= ABS(BHOLE(NM,IE)))THEN
         IV4=IE
       ENDIF
       VAR(NV,I) = VAR(NV,I) + HALF * DT1 * (PHI(IE)*
     2 (FLUX(1,I)+FLUX(2,I)+FLUX(3,I)+FLUX(4,I))
     3 -PHI(IV1)*FLUX(1,I)-PHI(IV2)*FLUX(2,I)-PHI(IV3)*FLUX(3,I)
     4 -PHI(IV4)*FLUX(4,I)) / VOL(I)
  10  CONTINUE
      ELSE
         DO 20 I=LFT,LLT
            IE =NFT+I
            IAD2 = ALE_CONNECT%ee_connect%iad_connect(IE)
            IV1=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
            IV2=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
            IV3=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
            IV4=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
      
       IF(IV1 <= 0.)THEN
         IV1=IE       
       ELSEIF(BHOLE(NM,IE) < 0. .AND.ABS(BHOLE(NM,IV1)) /= ABS(BHOLE(NM,IE)))THEN
         IV1=IE
       ENDIF
       IF(IV2 <= 0.)THEN 
         IV2=IE       
       ELSEIF(BHOLE(NM,IE) < 0. .AND.ABS(BHOLE(NM,IV2)) /= ABS(BHOLE(NM,IE)))THEN
         IV2=IE
       ENDIF
       IF(IV3 <= 0.)THEN 
         IV3=IE       
       ELSEIF(BHOLE(NM,IE) < 0. .AND.ABS(BHOLE(NM,IV3)) /= ABS(BHOLE(NM,IE)))THEN
         IV3=IE
       ENDIF
       IF(IV4 <= 0.)THEN 
         IV4=IE       
       ELSEIF(BHOLE(NM,IE) < 0. .AND.ABS(BHOLE(NM,IV4)) /= ABS(BHOLE(NM,IE)))THEN
         IV4=IE
       ENDIF
        VOLN=VOL(I)-DT1 * (FLUX(1,I)+FLUX(2,I)+FLUX(3,I)+FLUX(4,I))
       IF (VOLN > EM15) THEN
         VAR(NV,I) = VAR(NV,I) + HALF * DT1 * (PHI(IE)*
     2 (FLUX(1,I)+FLUX(2,I)+FLUX(3,I)+FLUX(4,I))
     3 -PHI(IV1)*FLUX(1,I)-PHI(IV2)*FLUX(2,I)-PHI(IV3)*FLUX(3,I)
     4 -PHI(IV4)*FLUX(4,I)) / MAX(EM15,VOLN)
        ENDIF
  20  CONTINUE
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  BREZO2_SIG                    source/ale/ale2d/brezo2.F     
Chd|-- called by -----------
Chd|        AREZON                        source/ale/arezon.F           
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE BREZO2_SIG(ALE_CONNECT,SIG,NV,PHI,NDIM,FLUX,VOL,BHOLE,NM,NEL)
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDIM,NV,NEL
      INTEGER BHOLE(NMULT,*),NM
      my_real SIG(NEL,NDIM), PHI(*), FLUX(4,*), VOL(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IE, IV1, IV2, IV3, IV4, IAD2
      my_real VOLN
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
      IF(JMULT == 0)THEN
       DO   I=LFT,LLT
          IE =NFT+I
          IAD2 = ALE_CONNECT%ee_connect%iad_connect(IE)
          IV1=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
          IV2=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
          IV3=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
          IV4=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
       
         IF(IV1 <= 0.)THEN
           IV1=IE       
         ELSEIF(BHOLE(NM,IE) < 0. .AND.ABS(BHOLE(NM,IV1)) /= ABS(BHOLE(NM,IE)))THEN
           IV1=IE
         ENDIF
         IF(IV2 <= 0)THEN
           IV2=IE       
         ELSEIF(BHOLE(NM,IE) < 0. .AND.ABS(BHOLE(NM,IV2)) /= ABS(BHOLE(NM,IE)))THEN
           IV2=IE
         ENDIF
         IF(IV3 <= 0)THEN
           IV3=IE       
         ELSEIF(BHOLE(NM,IE) < 0. .AND.ABS(BHOLE(NM,IV3)) /= ABS(BHOLE(NM,IE)))THEN
           IV3=IE
         ENDIF
         IF(IV4 <= 0)THEN
           IV4=IE
         ELSEIF(BHOLE(NM,IE) < 0. .AND.ABS(BHOLE(NM,IV4)) /= ABS(BHOLE(NM,IE)))THEN
           IV4=IE
         ENDIF
         SIG(I,NV) = SIG(I,NV) + HALF * DT1 * ( 
     .                                          PHI(IE)*(FLUX(1,I)+FLUX(2,I)+FLUX(3,I)+FLUX(4,I))
     .                                         -PHI(IV1)*FLUX(1,I)-PHI(IV2)*FLUX(2,I)-PHI(IV3)*FLUX(3,I)-PHI(IV4)*FLUX(4,I)
     .                                        ) / VOL(I)
       ENDDO
      ELSE
       DO I=LFT,LLT
         IE =NFT+I
         IAD2 = ALE_CONNECT%ee_connect%iad_connect(IE)
         IV1=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
         IV2=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
         IV3=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
         IV4=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
         IF(IV1 <= 0.)THEN
           IV1=IE       
         ELSEIF(BHOLE(NM,IE) < 0. .AND.ABS(BHOLE(NM,IV1)) /= ABS(BHOLE(NM,IE)))THEN
           IV1=IE
           ENDIF
         IF(IV2 <= 0.)THEN 
           IV2=IE       
         ELSEIF(BHOLE(NM,IE) < 0. .AND.ABS(BHOLE(NM,IV2)) /= ABS(BHOLE(NM,IE)))THEN
           IV2=IE
         ENDIF
         IF(IV3 <= 0.)THEN 
           IV3=IE       
         ELSEIF(BHOLE(NM,IE) < 0. .AND.ABS(BHOLE(NM,IV3)) /= ABS(BHOLE(NM,IE)))THEN
           IV3=IE
         ENDIF
         IF(IV4 <= 0.)THEN 
           IV4=IE       
         ELSEIF(BHOLE(NM,IE) < 0. .AND.ABS(BHOLE(NM,IV4)) /= ABS(BHOLE(NM,IE)))THEN
           IV4=IE
         ENDIF
          VOLN=VOL(I)-DT1 * (FLUX(1,I)+FLUX(2,I)+FLUX(3,I)+FLUX(4,I))
         IF (VOLN > EM15) THEN
           SIG(I,NV) = SIG(I,NV) + HALF * DT1 * (PHI(IE)* (FLUX(1,I)+FLUX(2,I)+FLUX(3,I)+FLUX(4,I))
     .                 -PHI(IV1)*FLUX(1,I)-PHI(IV2)*FLUX(2,I)-PHI(IV3)*FLUX(3,I)-PHI(IV4)*FLUX(4,I)) / MAX(EM15,VOLN)
          ENDIF
      ENDDO !next I=LFT,LLT
      ENDIF
C-----------------------------------------------
      RETURN
      END
