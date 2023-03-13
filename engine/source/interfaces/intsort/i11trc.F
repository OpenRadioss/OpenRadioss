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
Chd|  I11TRC                        source/interfaces/intsort/i11trc.F
Chd|-- called by -----------
Chd|        I11MAIN_TRI                   source/interfaces/intsort/i11main_tri.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I11TRC(
     1      I_STOK  ,CAND_N ,CAND_E,CAND_FX,CAND_FY ,
     2      CAND_FZ ,MFROT  ,ADDCM ,CHAINE ,NSN4    ,
     3      ITAB    , JLT   , NFT  ,IFPEN,STFS ,NIN ,NRTS)
C============================================================================
      USE TRI7BOX      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C   ROLE DE LA ROUTINE:
C   ===================
C   TRI sur N de CAND_N CAND_E CAND_F
C   et elimination des noeuds en rebond 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_STOK,MFROT,NSN4,JLT,NFT ,NIN,NRTS
      INTEGER CAND_N(*),CAND_E(*),ADDCM(*),CHAINE(2,*),IFPEN(*),ITAB(*)
C     REAL
      my_real
     .   CAND_FX(*),CAND_FY(*),CAND_FZ(*),STFS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,K,IADFIN,IAD,IAD0,J,II,SLVE,NN
C-----------------------------------------------
C=======================================================================
C
C Remove deleted Candidates from impacting list.
C
      DO I=1,JLT
        II = I + NFT
        IF(IFPEN(II) == 1)THEN
          SLVE = CAND_N(II)
          IF(SLVE<=NRTS)THEN
             IF(STFS(SLVE)==ZERO)THEN
                IFPEN(II) = 0 
             ENDIF
          ELSE
             NN = SLVE - NRTS
             IF(STIFI(NIN)%P(NN)==ZERO)THEN
                IFPEN(II) = 0  
             ENDIF
          ENDIF
        ENDIF
      ENDDO
C

      DO I=1,JLT
        II = I + NFT
        IF(IFPEN(II) == 1)THEN
          IAD=ADDCM(CAND_E(II))
          J=0
          DO WHILE(IAD/=0.AND.J<NSN4)
            J=J+1
            IF(CHAINE(1,IAD)==CAND_N(II))THEN
              IFPEN(II) = 0
              IAD=0
            ELSE
              IAD0=IAD
              IAD=CHAINE(2,IAD)
            ENDIF
          ENDDO
          IF(IFPEN(II) == 1)THEN
            I_STOK = I_STOK + 1
              IADFIN=I_STOK
              IF(IADFIN>NSN4) THEN
                RETURN
              ENDIF   
            CHAINE(1,IADFIN)=CAND_N(II)
            CHAINE(2,IADFIN)=0
            IF(ADDCM(CAND_E(II))==0)THEN
              ADDCM(CAND_E(II))=IADFIN
            ELSE
              CHAINE(2,IAD0)=IADFIN
            ENDIF
            CAND_FX(I_STOK) = CAND_FX(II)

            CAND_FY(I_STOK) = CAND_FY(II)
            CAND_FZ(I_STOK) = CAND_FZ(II)
            CAND_E(I_STOK) = CAND_E(II)
            CAND_N(I_STOK) = CAND_N(II)
            IFPEN(I_STOK) = IFPEN(II)
          ENDIF
        ENDIF
      ENDDO
C
      RETURN
      END
