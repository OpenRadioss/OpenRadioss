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
Chd|  I11STO_VOX1                   source/interfaces/inter3d1/i11sto.F
Chd|-- called by -----------
Chd|        I11TRIVOX1                    source/interfaces/inter3d1/i11trivox1.F
Chd|-- calls ---------------
Chd|        I11PEN3_VOX1                  source/interfaces/inter3d1/i11pen3.F
Chd|====================================================================
      SUBROUTINE I11STO_VOX1(
     1      J_STOK,IRECTS,IRECTM,X     ,II_STOK,
     4      CAND_N,CAND_E,NSN   ,NOINT ,MARGE  ,
     5      I_MEM ,PROV_N,PROV_E,MULTIMP,ADDCM ,
     4      CHAINE,IADFIN,GAPMIN,DRAD   ,IGAP  ,
     5      GAP_S ,GAP_M ,GAP_S_L,GAP_M_L,DGAPLOAD)
C============================================================================
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_MEM,MULTIMP,IADFIN
      INTEGER J_STOK,NSN,NOINT,II_STOK, IGAP
      INTEGER IRECTS(2,*),IRECTM(2,*),CAND_N(*),CAND_E(*)
      INTEGER PROV_N(MVSIZ),PROV_E(MVSIZ),IAD0(MVSIZ),ADDCM(*),
     .        CHAINE(2,*)
C     REAL
      my_real
     .   X(3,*), GAPMIN, DRAD, MARGE
      my_real , INTENT(IN) :: DGAPLOAD
      my_real
     .   GAP_S(*), GAP_M(*), GAP_S_L(*), GAP_M_L(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K_STOK,I_STOK,IAD,CONT
C     REAL
      my_real
     .   PENE(MVSIZ)
C-----------------------------------------------
        CALL I11PEN3_VOX1(J_STOK ,PROV_N ,PROV_E ,GAPMIN ,DRAD    , 
     .                    MARGE  ,GAP_S ,GAP_M   ,GAP_S_L,GAP_M_L ,
     .                    IGAP   ,X     ,IRECTS ,IRECTM  ,PENE    ,
     .                    DGAPLOAD)
C-----------------------------------------------
        K_STOK = 0
        I_STOK = II_STOK
C-----------------------------------------------
C elimination des couples deja trouves dans 1 boite precedente
C-----------------------------------------------
        DO I=1,J_STOK
          IF(PENE(I)/=0.0)THEN
            IAD=ADDCM(PROV_E(I))
            J=0
            DO WHILE(IAD/=0.AND.J<MULTIMP*NSN)
              J=J+1
              IF(CHAINE(1,IAD)==PROV_N(I))THEN
                PENE(I) = ZERO
                IAD=0
              ELSE
                IAD0(I)=IAD
                IAD=CHAINE(2,IAD)
              ENDIF
            ENDDO
            IF(PENE(I)/=ZERO)THEN
              K_STOK = K_STOK + 1
              IF(I_STOK+K_STOK>MULTIMP*NSN) THEN
                I_MEM = 2
                RETURN
              ENDIF
              IADFIN=IADFIN+1
              CHAINE(1,IADFIN)=PROV_N(I)
              CHAINE(2,IADFIN)=0
              IF(ADDCM(PROV_E(I))==0)THEN
                ADDCM(PROV_E(I))=IADFIN
              ELSE
                CHAINE(2,IAD0(I))=IADFIN
              ENDIF
            ENDIF
          ENDIF
        ENDDO
C
        IF(K_STOK==0)RETURN
C
        II_STOK   = I_STOK + K_STOK
C-----------------------------------------------
C stockage des couples candidats
C-----------------------------------------------
        DO I=1,J_STOK
          IF(PENE(I)/=ZERO)THEN
            I_STOK = I_STOK + 1
            CAND_N(I_STOK) = PROV_N(I)
            CAND_E(I_STOK) = PROV_E(I)
          ENDIF
        ENDDO
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  I11STO                        source/interfaces/inter3d1/i11sto.F
Chd|-- called by -----------
Chd|        I11BUC1                       source/interfaces/inter3d1/i11buc1.F
Chd|        I11TRI                        source/interfaces/inter3d1/i11tri.F
Chd|-- calls ---------------
Chd|        I11PEN3                       source/interfaces/inter3d1/i11pen3.F
Chd|====================================================================
      SUBROUTINE I11STO(
     1      J_STOK,IRECTS,IRECTM,X     ,II_STOK,
     4      CAND_N,CAND_E,NSN   ,NOINT ,TZINF ,
     5      I_MEM ,PROV_N,PROV_E,MULTIMP,ADDCM,
     4      CHAINE,IADFIN)
C============================================================================
C  cette routine est appelee par : I11TRI(/inter3d1/i11tri.F)
C----------------------------------------------------------------------------
C  cette routine appelle : I11PEN3(/inter3d1/i11pen3.F)
C============================================================================
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_MEM,MULTIMP,IADFIN
      INTEGER J_STOK,NSN,NOINT,II_STOK
      INTEGER IRECTS(2,*),IRECTM(2,*),CAND_N(*),CAND_E(*)
      INTEGER PROV_N(MVSIZ),PROV_E(MVSIZ),IAD0(MVSIZ),ADDCM(*),
     .        CHAINE(2,*)
C     REAL
      my_real
     .   X(3,*),TZINF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K_STOK,I_STOK,IAD,CONT
C     REAL
      my_real
     .   PENE(MVSIZ)
C-----------------------------------------------
        CALL I11PEN3(J_STOK ,PROV_N,PROV_E,TZINF ,X ,
     .               IRECTS  ,IRECTM ,PENE  )
C-----------------------------------------------
        K_STOK = 0
        I_STOK = II_STOK
C-----------------------------------------------
C elimination des couples deja trouves dans 1 boite precedente
C-----------------------------------------------
        DO I=1,J_STOK
          IF(PENE(I)/=0.0)THEN
            IAD=ADDCM(PROV_E(I))
            J=0
            DO WHILE(IAD/=0.AND.J<MULTIMP*NSN)
              J=J+1
              IF(CHAINE(1,IAD)==PROV_N(I))THEN
                PENE(I) = ZERO
                IAD=0
              ELSE
                IAD0(I)=IAD
                IAD=CHAINE(2,IAD)
              ENDIF
            ENDDO
            IF(PENE(I)/=ZERO)THEN
              K_STOK = K_STOK + 1
              IF(I_STOK+K_STOK>MULTIMP*NSN) THEN
                I_MEM = 2
                RETURN
              ENDIF
              IADFIN=IADFIN+1
              CHAINE(1,IADFIN)=PROV_N(I)
              CHAINE(2,IADFIN)=0
              IF(ADDCM(PROV_E(I))==0)THEN
                ADDCM(PROV_E(I))=IADFIN
              ELSE
                CHAINE(2,IAD0(I))=IADFIN
              ENDIF
            ENDIF
          ENDIF
        ENDDO
C
        IF(K_STOK==0)RETURN
C
        II_STOK   = I_STOK + K_STOK
C-----------------------------------------------
C stockage des couples candidats
C-----------------------------------------------
        DO I=1,J_STOK
          IF(PENE(I)/=ZERO)THEN
            I_STOK = I_STOK + 1
            CAND_N(I_STOK) = PROV_N(I)
            CAND_E(I_STOK) = PROV_E(I)
          ENDIF
        ENDDO
C-----------------------------------------------
      RETURN
      END
