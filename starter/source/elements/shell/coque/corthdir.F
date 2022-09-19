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
Chd|  CORTHDIR                      source/elements/shell/coque/corthdir.F
Chd|-- called by -----------
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|        CMAINI3                       source/elements/sh3n/coquedk/cmaini3.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|====================================================================
      SUBROUTINE CORTHDIR(ELBUF_STR,
     .                    IGEO   ,GEO       ,VX        ,VY     ,VZ       ,
     .                    PHI1   ,PHI2      ,COOR1     ,COOR2  ,COOR3    ,
     .                    COOR4  ,IORTHLOC  ,NLAY      ,IREP   ,ISUBSTACK,
     .                    STACK  ,GEO_STACK ,IGEO_STACK,IR     ,IS       ,
     .                    NEL    ,IMAT      ,IPROP     ,
     .                    X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .                    Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .                    E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE STACK_MOD
C-----------------------------------------------
C INITIALISE LES COORDONEES LOCALES DES AXES D'ORTHOTROPIE
C INITIALISE LES EPAISSEURS ET MATERIAUX DES COUCHES
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
#include      "warn_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NLAY,IREP,ISUBSTACK,IS,IR,NEL,IMAT,IPROP
      INTEGER IGEO(NPROPGI,*),IORTHLOC(*),IGEO_STACK(NPROPGI,*)
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z,
     .                                         X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4
      my_real
     .   GEO(NPROPG,*),VX(*),VY(*),VZ(*),PHI1(NLAY,*),PHI2(NLAY,*),
     .   COOR1(NLAY,MVSIZ),COOR2(NLAY,MVSIZ),
     .   COOR3(NLAY,MVSIZ),COOR4(NLAY,MVSIZ),
     .   GEO_STACK(NPROPG,*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (STACK_PLY):: STACK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,I1,I2,I3,IL,IGTYP,IPTHK,IPMAT,ILAW,L_DMG
      my_real R,S,D1,D2,D11,D12,D21,D22,U1X,U1Y,U2X,U2Y,
     .        DET,W1X,W2X,W1Y,W2Y,CSP,SNP
      my_real,  DIMENSION(MVSIZ)      :: E11,E12,E13,E21,E22,E23,VR,VS
      my_real,  DIMENSION(:), POINTER :: DIR1,DIR2,DIR_DMG
C      
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(L_BUFEL_) ,POINTER :: LBUF
C======================================================================|
      IGTYP = IGEO(11,IPROP)
C
      IF (IGTYP == 1) THEN
C       non orthotropic property
          IF( ELBUF_STR%BUFLY(1)%LY_DIRA  /= 0) THEN
            DIR1 => ELBUF_STR%BUFLY(1)%DIRA
            DO I=LFT,LLT
              DIR1(I) = ONE
              DIR1(I+NEL) = ZERO
            ENDDO
          ENDIF
      ELSE
        IPMAT = 100
        IPTHK = 300
C
C---    isoparametric (material) axes
        IF (ITY == 3) THEN
C---      shell 4N
          DO I=LFT,LLT
            E11(I)= X2(I)+X3(I)-X1(I)-X4(I)
            E12(I)= Y2(I)+Y3(I)-Y1(I)-Y4(I)
            E13(I)= Z2(I)+Z3(I)-Z1(I)-Z4(I)
            E21(I)= X3(I)+X4(I)-X1(I)-X2(I)
            E22(I)= Y3(I)+Y4(I)-Y1(I)-Y2(I)
            E23(I)= Z3(I)+Z4(I)-Z1(I)-Z2(I)
          ENDDO
        ELSE
C---      shell 3N
          DO I=LFT,LLT
            E11(I)= X2(I)-X1(I)
            E12(I)= Y2(I)-Y1(I)
            E13(I)= Z2(I)-Z1(I)
            E21(I)= X3(I)-X1(I)
            E22(I)= Y3(I)-Y1(I)
            E23(I)= Z3(I)-Z1(I)
          ENDDO
        ENDIF
C---    vecteur de projection dans repere elementaire
        DO I=LFT,LLT
          VR(I)=VX(I)*E1X(I)+VY(I)*E1Y(I)+VZ(I)*E1Z(I)
          VS(I)=VX(I)*E2X(I)+VY(I)*E2Y(I)+VZ(I)*E2Z(I)
        ENDDO
C-------
        IF (IGTYP == 9) THEN
          DIR1 => ELBUF_STR%BUFLY(1)%DIRA
C-------
          DO I=LFT,LLT
            CSP=COS(PHI1(1,I))
            SNP=SIN(PHI1(1,I))
            DIR1(I)     = VR(I)*CSP-VS(I)*SNP
            DIR1(I+NEL) = VS(I)*CSP+VR(I)*SNP
            IF (IORTHLOC(I) /= 0) THEN
              DIR1(I)     = COOR1(1,I)
              DIR1(I+NEL) = COOR2(1,I)
            ENDIF
          ENDDO
C-------
        ELSEIF (IGTYP == 10) THEN
C-------
          DO IL=1,NLAY
            DIR1 => ELBUF_STR%BUFLY(IL)%DIRA
            DO I=LFT,LLT
              CSP=COS(PHI1(IL,I))
              SNP=SIN(PHI1(IL,I))
              DIR1(I)     = VR(I)*CSP-VS(I)*SNP
              DIR1(I+NEL) = VS(I)*CSP+VR(I)*SNP
              IF (IORTHLOC(I) /= 0) THEN
                DIR1(I)     = COOR1(IL,I)
                DIR1(I+NEL) = COOR2(IL,I)
              ENDIF
            ENDDO
          ENDDO
          DO IL=1,NLAY
            I2=IPTHK+IL
            I3=IPMAT+IL
            DO I=LFT,LLT
              GEO (I2,IPROP) = ONE/NLAY
              IGEO(I3,IPROP) = IMAT
            ENDDO
          ENDDO
C-------
        ELSEIF (IGTYP == 11 .OR. IGTYP == 17) THEN

C-------
          DO IL=1,NLAY
            DIR1 => ELBUF_STR%BUFLY(IL)%DIRA
C
            DO I=LFT,LLT
              CSP=COS(PHI1(IL,I))
              SNP=SIN(PHI1(IL,I))
              IF(IGTYP == 17) THEN
                IF(STACK%IGEO(2+IL,ISUBSTACK) /= 0) THEN
                  IF (IGEO(49,STACK%IGEO(2+IL,ISUBSTACK)) == 1) THEN
                    DIR1(I)     = CSP
                    DIR1(I+NEL) = SNP
                  ELSE
                    DIR1(I)     = VR(I)*CSP-VS(I)*SNP
                    DIR1(I+NEL) = VS(I)*CSP+VR(I)*SNP
                  ENDIF
                ELSE
                   DIR1(I)     = VR(I)*CSP-VS(I)*SNP
                   DIR1(I+NEL) = VS(I)*CSP+VR(I)*SNP
                ENDIF
              ELSE
                DIR1(I)     = VR(I)*CSP-VS(I)*SNP
                DIR1(I+NEL) = VS(I)*CSP+VR(I)*SNP
              ENDIF
              IF (IORTHLOC(I) /= 0) THEN
                DIR1(I)     = COOR1(IL,I)
                DIR1(I+NEL) = COOR2(IL,I)
              ENDIF
C              
            ENDDO
          ENDDO
          L_DMG = ELBUF_STR%BUFLY(1)%L_DMG
          IF(L_DMG > 0) THEN
            DO IL=1,NLAY
              LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,1)
              DIR_DMG => LBUF%DMG(1:L_DMG*LLT)
              DIR_DMG(LFT:LLT) = ONE
              IF(L_DMG > 1) DIR_DMG(LLT+1:L_DMG*LLT) = ZERO
            ENDDO
          ENDIF
          IF (IREP == 1) THEN
            DO I=LFT,LLT
              U1X = E11(I)*E1X(I)+E12(I)*E1Y(I)+E13(I)*E1Z(I)
              U1Y = E11(I)*E2X(I)+E12(I)*E2Y(I)+E13(I)*E2Z(I)
              U2X = E21(I)*E1X(I)+E22(I)*E1Y(I)+E23(I)*E1Z(I)
              U2Y = E21(I)*E2X(I)+E22(I)*E2Y(I)+E23(I)*E2Z(I)
              DET = U1X*U2Y-U1Y*U2X
              W1X = U2Y/DET
              W2Y = U1X/DET
              W1Y = -U1Y/DET
              W2X = -U2X/DET
              DO IL=1,NLAY
                DIR1 => ELBUF_STR%BUFLY(IL)%DIRA
                IF ( IORTHLOC(I) /= 0 ) THEN
                  DIR1(I)     = COOR1(IL,I)
                  DIR1(I+NEL) = COOR2(IL,I)
                ENDIF
                D1 = DIR1(I)
                D2 = DIR1(I+NEL)
                DIR1(I)     = W1X*D1 + W2X*D2
                DIR1(I+NEL) = W1Y*D1 + W2Y*D2
                S = SQRT(DIR1(I)**2 + DIR1(I+NEL)**2)
                DIR1(I)     = DIR1(I)/S
                DIR1(I+NEL) = DIR1(I+NEL)/S
              ENDDO
            ENDDO
          ENDIF
C-------
        ELSEIF (IGTYP == 51 .OR. IGTYP == 52) THEN
          DO IL=1,NLAY
            DIR1 => ELBUF_STR%BUFLY(IL)%DIRA
            DO I=LFT,LLT
              CSP=COS(PHI1(IL,I))
              SNP=SIN(PHI1(IL,I))
              IF(IGTYP == 51 .AND. STACK%IGEO(2+IL,ISUBSTACK) /= 0)THEN
                IF (IGEO(49,STACK%IGEO(2+IL,ISUBSTACK)) == 1) THEN
                  DIR1(I)     = CSP
                  DIR1(I+NEL) = SNP
                ELSE
                  DIR1(I)     = VR(I)*CSP-VS(I)*SNP
                  DIR1(I+NEL) = VS(I)*CSP+VR(I)*SNP
                ENDIF
              ELSEIF(IGTYP == 52 .AND.
     .               IGEO_STACK(49,STACK%IGEO(2+IL,ISUBSTACK)) == 1)THEN
                  DIR1(I)     = CSP
                  DIR1(I+NEL) = SNP
              ELSE
                DIR1(I)     = VR(I)*CSP-VS(I)*SNP
                DIR1(I+NEL) = VS(I)*CSP+VR(I)*SNP
              ENDIF
              IF (IORTHLOC(I) /= 0) THEN
                DIR1(I)     = COOR1(IL,I)
                DIR1(I+NEL) = COOR2(IL,I)
              ENDIF
            ENDDO
          ENDDO
C---
          IF (IREP == 1) THEN
            DO I=LFT,LLT
              U1X = E11(I)*E1X(I)+E12(I)*E1Y(I)+E13(I)*E1Z(I)
              U1Y = E11(I)*E2X(I)+E12(I)*E2Y(I)+E13(I)*E2Z(I)
              U2X = E21(I)*E1X(I)+E22(I)*E1Y(I)+E23(I)*E1Z(I)
              U2Y = E21(I)*E2X(I)+E22(I)*E2Y(I)+E23(I)*E2Z(I)
              DET = U1X*U2Y-U1Y*U2X
              W1X = U2Y/DET
              W2Y = U1X/DET
              W1Y = -U1Y/DET
              W2X = -U2X/DET
              DO IL=1,NLAY
                DIR1 => ELBUF_STR%BUFLY(IL)%DIRA
                IF (IORTHLOC(I) /= 0) THEN
                  DIR1(I)     = COOR1(IL,I)
                  DIR1(I+NEL) = COOR2(IL,I)
                ENDIF
                D1 = DIR1(I)
                D2 = DIR1(I+NEL)
                DIR1(I)     = W1X*D1 + W2X*D2
                DIR1(I+NEL) = W1Y*D1 + W2Y*D2
                S = SQRT(DIR1(I)**2 + DIR1(I+NEL)**2)
                DIR1(I)     = DIR1(I)/S
                DIR1(I+NEL) = DIR1(I+NEL)/S
              ENDDO
            ENDDO
          ELSEIF (IREP == 2) THEN
C---      Axe I d'anisotropie
            DO IL=1,NLAY
              DIR1 => ELBUF_STR%BUFLY(IL)%DIRA
              DIR2 => ELBUF_STR%BUFLY(IL)%DIRB
C---      Axe I d'anisotropie
              DO I=LFT,LLT
                CSP=COS(PHI1(IL,I))
                SNP=SIN(PHI1(IL,I))
                DIR1(I)     = VR(I)*CSP-VS(I)*SNP
                DIR1(I+NEL) = VS(I)*CSP+VR(I)*SNP
              ENDDO
C---      Axe II d'anisotropie
              DO I=LFT,LLT
                CSP=COS(PHI2(IL,I))
                SNP=SIN(PHI2(IL,I))
                R = DIR1(I)
                S = DIR1(I+NEL)
                DIR2(I)     = R*CSP-S*SNP
                DIR2(I+NEL) = S*CSP+R*SNP
              ENDDO
            ENDDO ! DO IL=1,NLAY
C---
            DO IL=1,NLAY
              DIR1 => ELBUF_STR%BUFLY(IL)%DIRA
              DIR2 => ELBUF_STR%BUFLY(IL)%DIRB
              DO I=LFT,LLT
                IF (IORTHLOC(I) /= 0) THEN
                  DIR1(I)     =  COOR1(IL,I)
                  DIR1(I+NEL) =  COOR2(IL,I)
                  DIR2(I)     =  COOR3(IL,I)
                  DIR2(I+NEL) =  COOR4(IL,I)
                ENDIF
              ENDDO
            ENDDO ! DO IL=1,NLAY
C---
            DO I=LFT,LLT
              U1X = E11(I)*E1X(I)+E12(I)*E1Y(I)+E13(I)*E1Z(I)
              U1Y = E11(I)*E2X(I)+E12(I)*E2Y(I)+E13(I)*E2Z(I)
              U2X = E21(I)*E1X(I)+E22(I)*E1Y(I)+E23(I)*E1Z(I)
              U2Y = E21(I)*E2X(I)+E22(I)*E2Y(I)+E23(I)*E2Z(I)
              DET = U1X*U2Y-U1Y*U2X
              W1X = U2Y/DET
              W2Y = U1X/DET
              W1Y = -U1Y/DET
              W2X = -U2X/DET
              DO IL=1,NLAY
                DIR1 => ELBUF_STR%BUFLY(IL)%DIRA
                DIR2 => ELBUF_STR%BUFLY(IL)%DIRB
C---          dir I
                D11 = DIR1(I)
                D21 = DIR1(I+NEL)
                DIR1(I)     = W1X*D11 + W2X*D21
                DIR1(I+NEL) = W1Y*D11 + W2Y*D21
                S = SQRT(DIR1(I)**2 + DIR1(I+NEL)**2)
                DIR1(I)     = DIR1(I)/S
                DIR1(I+NEL) = DIR1(I+NEL)/S
C---          dir II
                D12 = DIR2(I)
                D22=  DIR2(I+NEL)
                DIR2(I)     = W1X*D12 + W2X*D22
                DIR2(I+NEL) = W1Y*D12 + W2Y*D22
                S = SQRT(DIR2(I)**2 + DIR2(I+NEL)**2)
                DIR2(I)     = DIR2(I)/S
                DIR2(I+NEL) = DIR2(I+NEL)/S
              ENDDO
            ENDDO
          ELSEIF (IREP == 3) THEN
C   mixing law58 with other user laws with IREP = 0 within PID51
            DO IL=1,NLAY
              ILAW = ELBUF_STR%BUFLY(IL)%ILAW
              IF (ILAW == 58) THEN
                DIR1 => ELBUF_STR%BUFLY(IL)%DIRA
                DIR2 => ELBUF_STR%BUFLY(IL)%DIRB
C---      Axe I d'anisotropie
                DO I=LFT,LLT
                  CSP=COS(PHI1(IL,I))
                  SNP=SIN(PHI1(IL,I))
                  DIR1(I)     = VR(I)*CSP-VS(I)*SNP
                  DIR1(I+NEL) = VS(I)*CSP+VR(I)*SNP
                ENDDO
C---      Axe II d'anisotropie
                DO I=LFT,LLT
                  CSP=COS(PHI2(IL,I))
                  SNP=SIN(PHI2(IL,I))
                  R = DIR1(I)
                  S = DIR1(I+NEL)
                  DIR2(I)     = R*CSP-S*SNP
                  DIR2(I+NEL) = S*CSP+R*SNP
                ENDDO
C---
                DO I=LFT,LLT
                  IF (IORTHLOC(I) /= 0) THEN
                    DIR1(I)     =  COOR1(IL,I)
                    DIR1(I+NEL) =  COOR2(IL,I)
                    DIR2(I)     =  COOR3(IL,I)
                    DIR2(I+NEL) =  COOR4(IL,I)
                  ENDIF
                ENDDO
C---
                DO I=LFT,LLT
                  U1X = E11(I)*E1X(I)+E12(I)*E1Y(I)+E13(I)*E1Z(I)
                  U1Y = E11(I)*E2X(I)+E12(I)*E2Y(I)+E13(I)*E2Z(I)
                  U2X = E21(I)*E1X(I)+E22(I)*E1Y(I)+E23(I)*E1Z(I)
                  U2Y = E21(I)*E2X(I)+E22(I)*E2Y(I)+E23(I)*E2Z(I)
                  DET = U1X*U2Y-U1Y*U2X
                  W1X = U2Y/DET
                  W2Y = U1X/DET
                  W1Y = -U1Y/DET
                  W2X = -U2X/DET
C---          dir I
                  D11 = DIR1(I)
                  D21 = DIR1(I+NEL)
                  DIR1(I) = W1X*D11 + W2X*D21
                  DIR1(I+NEL) = W1Y*D11 + W2Y*D21
                  S = SQRT(DIR1(I)**2 + DIR1(I+NEL)**2)
                  DIR1(I)     = DIR1(I)/S
                  DIR1(I+NEL) = DIR1(I+NEL)/S
C---          dir II
                  D12 = DIR2(I)
                  D22=  DIR2(I+NEL)
                  DIR2(I)     = W1X*D12 + W2X*D22
                  DIR2(I+NEL) = W1Y*D12 + W2Y*D22
                  S = SQRT(DIR2(I)**2 + DIR2(I+NEL)**2)
                  DIR2(I)     = DIR2(I)/S
                  DIR2(I+NEL) = DIR2(I+NEL)/S
                ENDDO
              ELSE  ! IREP = 0 within PID51
                DIR1 => ELBUF_STR%BUFLY(IL)%DIRA
                DO I=LFT,LLT
                  CSP=COS(PHI1(IL,I))
                  SNP=SIN(PHI1(IL,I))
                  DIR1(I)     = VR(I)*CSP-VS(I)*SNP
                  DIR1(I+NEL) = VS(I)*CSP+VR(I)*SNP
                  IF (IORTHLOC(I) /= 0) THEN
                    DIR1(I)     = COOR1(IL,I)
                    DIR1(I+NEL) = COOR2(IL,I)
                  ENDIF
                ENDDO
              ENDIF ! IF (ILAW == 58) THEN
            ENDDO ! DO N=1,NLAY
          ELSEIF (IREP == 4) THEN
C   mixing law58 with other user laws with IREP = 1 within PID51
            DO IL=1,NLAY
              ILAW = ELBUF_STR%BUFLY(IL)%ILAW
              IF (ILAW == 58) THEN
                DIR1 => ELBUF_STR%BUFLY(IL)%DIRA
                DIR2 => ELBUF_STR%BUFLY(IL)%DIRB
C---      Axe I d'anisotropie
                DO I=LFT,LLT
                  CSP=COS(PHI1(IL,I))
                  SNP=SIN(PHI1(IL,I))
                  DIR1(I)     = VR(I)*CSP-VS(I)*SNP
                  DIR1(I+NEL) = VS(I)*CSP+VR(I)*SNP
                ENDDO
C---      Axe II d'anisotropie
                DO I=LFT,LLT
                  CSP=COS(PHI2(IL,I))
                  SNP=SIN(PHI2(IL,I))
                  R = DIR1(I)
                  S = DIR1(I+NEL)
                  DIR2(I)     = R*CSP-S*SNP
                  DIR2(I+NEL) = S*CSP+R*SNP
                ENDDO
C---
                DO I=LFT,LLT
                  IF (IORTHLOC(I) /= 0) THEN
                    DIR1(I)     =  COOR1(IL,I)
                    DIR1(I+NEL) =  COOR2(IL,I)
                    DIR2(I)     =  COOR3(IL,I)
                    DIR2(I+NEL) =  COOR4(IL,I)
                  ENDIF
                ENDDO
C---
                DO I=LFT,LLT
                  U1X = E11(I)*E1X(I)+E12(I)*E1Y(I)+E13(I)*E1Z(I)
                  U1Y = E11(I)*E2X(I)+E12(I)*E2Y(I)+E13(I)*E2Z(I)
                  U2X = E21(I)*E1X(I)+E22(I)*E1Y(I)+E23(I)*E1Z(I)
                  U2Y = E21(I)*E2X(I)+E22(I)*E2Y(I)+E23(I)*E2Z(I)
                  DET = U1X*U2Y-U1Y*U2X
                  W1X = U2Y/DET
                  W2Y = U1X/DET
                  W1Y = -U1Y/DET
                  W2X = -U2X/DET
C---          dir I
                  D11 = DIR1(I)
                  D21 = DIR1(I+NEL)
                  DIR1(I)     = W1X*D11 + W2X*D21
                  DIR1(I+NEL) = W1Y*D11 + W2Y*D21
                  S = SQRT(DIR1(I)**2 + DIR1(I+NEL)**2)
                  DIR1(I)     = DIR1(I)/S
                  DIR1(I+NEL) = DIR1(I+NEL)/S
C---          dir II
                  D12 = DIR2(I)
                  D22=  DIR2(I+NEL)
                  DIR2(I)     = W1X*D12 + W2X*D22
                  DIR2(I+NEL) = W1Y*D12 + W2Y*D22
                  S = SQRT(DIR2(I)**2 + DIR2(I+NEL)**2)
                  DIR2(I)     = DIR2(I)/S
                  DIR2(I+NEL) = DIR2(I+NEL)/S
                ENDDO
              ELSE  ! IREP = 1 within PID51
                DIR1 => ELBUF_STR%BUFLY(IL)%DIRA
                DO I=LFT,LLT
                  U1X = E11(I)*E1X(I)+E12(I)*E1Y(I)+E13(I)*E1Z(I)
                  U1Y = E11(I)*E2X(I)+E12(I)*E2Y(I)+E13(I)*E2Z(I)
                  U2X = E21(I)*E1X(I)+E22(I)*E1Y(I)+E23(I)*E1Z(I)
                  U2Y = E21(I)*E2X(I)+E22(I)*E2Y(I)+E23(I)*E2Z(I)
                  DET = U1X*U2Y-U1Y*U2X
                  W1X = U2Y/DET
                  W2Y = U1X/DET
                  W1Y = -U1Y/DET
                  W2X = -U2X/DET
                  IF (IORTHLOC(I) /= 0) THEN
                    DIR1(I)     = COOR1(IL,I)
                    DIR1(I+NEL) = COOR2(IL,I)
                  ENDIF
                  D1 = DIR1(I)
                  D2 = DIR1(I+NEL)
                  DIR1(I)     = W1X*D1 + W2X*D2
                  DIR1(I+NEL) = W1Y*D1 + W2Y*D2
                  S = SQRT(DIR1(I)**2 + DIR1(I+NEL)**2)
                  DIR1(I)     = DIR1(I)/S
                  DIR1(I+NEL) = DIR1(I+NEL)/S
                ENDDO
              ENDIF ! IF (ILAW == 58) THEN
            ENDDO ! DO N=1,NLAY
          ENDIF ! IF (IREP == 1)
C-------
        ELSEIF (IGTYP == 16) THEN
C-------
C---      Axe I d'anisotropie
          DO IL=1,NLAY
            DIR1 => ELBUF_STR%BUFLY(IL)%DIRA
            DO I=LFT,LLT
              CSP=COS(PHI1(IL,I))
              SNP=SIN(PHI1(IL,I))
              DIR1(I)     = VR(I)*CSP-VS(I)*SNP
              DIR1(I+NEL) = VS(I)*CSP+VR(I)*SNP
            ENDDO
          ENDDO
C---      Axe II d'anisotropie
          DO IL=1,NLAY
            DIR1 => ELBUF_STR%BUFLY(IL)%DIRA
            DIR2 => ELBUF_STR%BUFLY(IL)%DIRB
             DO I=LFT,LLT
              CSP=COS(PHI2(IL,I))
              SNP=SIN(PHI2(IL,I))
              R = DIR1(I)
              S = DIR1(I+NEL)
              DIR2(I)     = R*CSP-S*SNP
              DIR2(I+NEL) = S*CSP+R*SNP
            ENDDO
          ENDDO
          DO IL=1,NLAY
            DIR1 => ELBUF_STR%BUFLY(IL)%DIRA
            DIR2 => ELBUF_STR%BUFLY(IL)%DIRB
            DO I=LFT,LLT
              IF (IORTHLOC(I) /= 0) THEN
                DIR1(I)     =  COOR1(IL,I)
                DIR1(I+NEL) =  COOR2(IL,I)
                DIR2(I)     =  COOR3(IL,I)
                DIR2(I+NEL) =  COOR4(IL,I)
              ENDIF
            ENDDO
          ENDDO
C------
          DO I=LFT,LLT
            U1X = E11(I)*E1X(I)+E12(I)*E1Y(I)+E13(I)*E1Z(I)
            U1Y = E11(I)*E2X(I)+E12(I)*E2Y(I)+E13(I)*E2Z(I)
            U2X = E21(I)*E1X(I)+E22(I)*E1Y(I)+E23(I)*E1Z(I)
            U2Y = E21(I)*E2X(I)+E22(I)*E2Y(I)+E23(I)*E2Z(I)
            DET = U1X*U2Y-U1Y*U2X
            W1X = U2Y/DET
            W2Y = U1X/DET
            W1Y = -U1Y/DET
            W2X = -U2X/DET
            DO IL=1,NLAY
              DIR1 => ELBUF_STR%BUFLY(IL)%DIRA
              DIR2 => ELBUF_STR%BUFLY(IL)%DIRB
C---          dir I
              D11 = DIR1(I)
              D21 = DIR1(I+NEL)
              DIR1(I)     = W1X*D11 + W2X*D21
              DIR1(I+NEL) = W1Y*D11 + W2Y*D21
              S = SQRT(DIR1(I)**2 + DIR1(I+NEL)**2)
              DIR1(I)     = DIR1(I)/S
              DIR1(I+NEL) = DIR1(I+NEL)/S
C---          dir II
              D12 = DIR2(I)
              D22=  DIR2(I+NEL)
              DIR2(I)     = W1X*D12 + W2X*D22
              DIR2(I+NEL) = W1Y*D12 + W2Y*D22
              S = SQRT(DIR2(I)**2 + DIR2(I+NEL)**2)
              DIR2(I)     = DIR2(I)/S
              DIR2(I+NEL) = DIR2(I+NEL)/S
            ENDDO
          ENDDO
C----
        ENDIF
C----
      ENDIF ! IF (IGTYP == 1)
C-----------
      RETURN
      END
