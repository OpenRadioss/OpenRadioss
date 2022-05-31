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
Chd|  ANISKEW                       source/output/anim/aniskew.F  
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        WRITE_S_C                     source/output/tools/write_routines.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE ANISKEW(ELBUF_TAB,SKEW ,IPARG ,X   , IXT,
     .                   IXP      ,IXR  ,GEO   ,BUFL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   X(3,*), SKEW(LSKEW,*), GEO(NPROPG,*)
      INTEGER IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),IPARG(NPARG,*),
     .        BUFL
C
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ISK(6),JJ,NEL,LFT,LLT,NG,
     .   ITY,MLW,NFT,N,II,LEN,IPROP,IGTYP,WA(BUFL)
      my_real
     .   EX(9),S3000,X1,Y1,Z1,X2,Y2,Z2,S
C
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
C-----------------------------------------------
      S3000 = THREE1000
C-----------------------------------------------
C      SKEW
C-----------------------------------------------
      DO I=1,NUMSKW
        ISK(1)=NINT(SKEW(1,I)*S3000)
        ISK(2)=NINT(SKEW(2,I)*S3000)
        ISK(3)=NINT(SKEW(3,I)*S3000)
        ISK(4)=NINT(SKEW(4,I)*S3000)
        ISK(5)=NINT(SKEW(5,I)*S3000)
        ISK(6)=NINT(SKEW(6,I)*S3000)
        CALL WRITE_S_C(ISK,6)
      ENDDO
C-----------------------------------------------
C      SKEW ELEMENT 1D
C-----------------------------------------------
      DO NG=1,NGROUP
        MLW = IPARG(1,NG)
        NEL = IPARG(2,NG)
        ITY = IPARG(5,NG)
        NFT = IPARG(3,NG)
        LFT = 1
        LLT = NEL
C
        GBUF => ELBUF_TAB(NG)%GBUF
C-----------------------------------------------
C      TRUSS
C-----------------------------------------------
        IF (ITY == 4) THEN
          DO I=LFT,LLT
            N = I + NFT
            X1=X(1,IXT(3,I))-X(1,IXT(2,I))
            Y1=X(2,IXT(3,I))-X(2,IXT(2,I))
            Z1=X(3,IXT(3,I))-X(3,IXT(2,I))
            S=ONE/MAX(EM20,SQRT(X1*X1+Y1*Y1+Z1*Z1))
            X1=X1*S
            Y1=Y1*S
            Z1=Z1*S
C
            IF (ABS(Z1) < HALF) THEN
              X2 =    -Z1*X1
              Y2 =    -Z1*Y1
              Z2 = ONE -Z1*Z1
            ELSE
              X2 = ONE -X1*X1
              Y2 =    -X1*Y1
              Z2 =    -X1*Z1
            ENDIF
            S=S3000/SQRT(X2*X2+Y2*Y2+Z2*Z2)
            ISK(1)=NINT(X1*S3000)
            ISK(2)=NINT(Y1*S3000)
            ISK(3)=NINT(Z1*S3000)
            ISK(4)=NINT(X2*S)
            ISK(5)=NINT(Y2*S)
            ISK(6)=NINT(Z2*S)
            CALL WRITE_S_C(ISK,6)
          ENDDO
C-----------------------------------------------
C       POUTRES
C-----------------------------------------------
        ELSEIF (ITY == 5) THEN
          DO I=LFT,LLT
             JJ = 3*(I-1)
             N = I + NFT
             X1=X(1,IXP(3,N))-X(1,IXP(2,N))
             Y1=X(2,IXP(3,N))-X(2,IXP(2,N))
             Z1=X(3,IXP(3,N))-X(3,IXP(2,N))
             S=S3000/MAX(EM20,SQRT(X1*X1+Y1*Y1+Z1*Z1))
             X2 = GBUF%SKEW(JJ + 1)
             Y2 = GBUF%SKEW(JJ + 2)
             Z2 = GBUF%SKEW(JJ + 3)
cc             X2=  BUFEL(NB6+3*I-3)
cc             Y2=  BUFEL(NB6+3*I-2)
cc             Z2=  BUFEL(NB6+3*I-1)
             ISK(1)=NINT(X1*S)
             ISK(2)=NINT(Y1*S)
             ISK(3)=NINT(Z1*S)
             ISK(4)=NINT(X2*S3000)
             ISK(5)=NINT(Y2*S3000)
             ISK(6)=NINT(Z2*S3000)
             CALL WRITE_S_C(ISK,6)
          ENDDO
C-----------------------------------------------
C       RESSORTS
C-----------------------------------------------
        ELSEIF (ITY == 6) THEN
          IPROP = IXR(1,NFT+1)
          IGTYP =  NINT(GEO(12,IPROP))
C
          IF (IGTYP == 4) THEN
            DO I=LFT,LLT
              N = I + NFT
              X1=X(1,IXR(3,N))-X(1,IXR(2,N))
              Y1=X(2,IXR(3,N))-X(2,IXR(2,N))
              Z1=X(3,IXR(3,N))-X(3,IXR(2,N))
              S=X1*X1+Y1*Y1+Z1*Z1
              IF (S < EM30) THEN
                X1=ONE
                Y1=ZERO
                Z1=ZERO
              ELSE
                S=ONE/SQRT(S)
                X1=X1*S
                Y1=Y1*S
                Z1=Z1*S
              ENDIF
              IF (ABS(Z1) < HALF) THEN
                X2 =    -Z1*X1
                Y2 =    -Z1*Y1
                Z2 = ONE -Z1*Z1
              ELSE
                X2 = ONE -X1*X1
                Y2 =    -X1*Y1
                Z2 =    -X1*Z1
              ENDIF
              S=X2*X2+Y2*Y2+Z2*Z2
              S=S3000/MAX(EM20,SQRT(S))
              ISK(1)=NINT(X1*S3000)
              ISK(2)=NINT(Y1*S3000)
              ISK(3)=NINT(Z1*S3000)
              ISK(4)=NINT(X2*S)
              ISK(5)=NINT(Y2*S)
              ISK(6)=NINT(Z2*S)
              CALL WRITE_S_C(ISK,6)
            ENDDO
C
          ELSEIF (IGTYP == 12) THEN
            DO I=LFT,LLT
              N = I + NFT
              X1=X(1,IXR(3,N))-X(1,IXR(2,N))
              Y1=X(2,IXR(3,N))-X(2,IXR(2,N))
              Z1=X(3,IXR(3,N))-X(3,IXR(2,N))
              S=ONE/MAX(EM20,SQRT(X1*X1+Y1*Y1+Z1*Z1))
              X1=X1*S
              Y1=Y1*S
              Z1=Z1*S
              IF (ABS(Z1) < HALF) THEN
                X2 =    -Z1*X1
                Y2 =    -Z1*Y1
                Z2 = ONE -Z1*Z1
              ELSE
                X2 = ONE -X1*X1
                Y2 =    -X1*Y1
                Z2 =    -X1*Z1
              ENDIF
              S=S3000/MAX(EM20,SQRT(X2*X2+Y2*Y2+Z2*Z2))
              ISK(1)=NINT(X1*S3000)
              ISK(2)=NINT(Y1*S3000)
              ISK(3)=NINT(Z1*S3000)
              ISK(4)=NINT(X2*S)
              ISK(5)=NINT(Y2*S)
              ISK(6)=NINT(Z2*S)
              CALL WRITE_S_C(ISK,6)
              X1=X(1,IXR(4,N))-X(1,IXR(3,N))
              Y1=X(2,IXR(4,N))-X(2,IXR(3,N))
              Z1=X(3,IXR(4,N))-X(3,IXR(3,N))
              S=ONE/MAX(EM20,SQRT(X1*X1+Y1*Y1+Z1*Z1))
              X1=X1*S
              Y1=Y1*S
              Z1=Z1*S
              IF (Z1 < HALF) THEN
                X2 =    -Z1*X1
                Y2 =    -Z1*Y1
                Z2 = ONE -Z1*Z1
              ELSE
                X2 = ONE -X1*X1
                Y2 =    -X1*Y1
                Z2 =    -X1*Z1
              ENDIF
              S=S3000/MAX(EM20,SQRT(X2*X2+Y2*Y2+Z2*Z2))
              ISK(1)=NINT(X1*S3000)
              ISK(2)=NINT(Y1*S3000)
              ISK(3)=NINT(Z1*S3000)
              ISK(4)=NINT(X2*S)
              ISK(5)=NINT(Y2*S)
              ISK(6)=NINT(Z2*S)
              CALL WRITE_S_C(ISK,6)
            ENDDO
C
          ELSEIF (IGTYP == 13 .OR. IGTYP == 23) THEN
            DO I=LFT,LLT
             JJ = 3*(I-1)
             N = I + NFT
             X1=X(1,IXR(3,N))-X(1,IXR(2,N))
             Y1=X(2,IXR(3,N))-X(2,IXR(2,N))
             Z1=X(3,IXR(3,N))-X(3,IXR(2,N))
             S=S3000/MAX(EM20,SQRT(X1*X1+Y1*Y1+Z1*Z1))
             X2 = GBUF%SKEW(JJ + 1)
             Y2 = GBUF%SKEW(JJ + 2)
             Z2 = GBUF%SKEW(JJ + 3)
cc             X2=  BUFEL(NB14+3*I-3)
cc             Y2=  BUFEL(NB14+3*I-2)
cc             Z2=  BUFEL(NB14+3*I-1)
             ISK(1)=NINT(X1*S)
             ISK(2)=NINT(Y1*S)
             ISK(3)=NINT(Z1*S)
             ISK(4)=NINT(X2*S3000)
             ISK(5)=NINT(Y2*S3000)
             ISK(6)=NINT(Z2*S3000)
             CALL WRITE_S_C(ISK,6)
            ENDDO
          ELSEIF (IGTYP == 25) THEN
C
            DO I=LFT,LLT
             JJ = 3*(I-1)
             N = I + NFT
             X1=X(1,IXR(3,N))-X(1,IXR(2,N))
             Y1=X(2,IXR(3,N))-X(2,IXR(2,N))
             Z1=X(3,IXR(3,N))-X(3,IXR(2,N))
             S=S3000/MAX(EM20,SQRT(X1*X1+Y1*Y1+Z1*Z1))
             X2 = GBUF%SKEW(JJ + 1)
             Y2 = GBUF%SKEW(JJ + 2)
             Z2 = GBUF%SKEW(JJ + 3)
cc             X2=  BUFEL(NB14+3*I-3)
cc             Y2=  BUFEL(NB14+3*I-2)
cc             Z2=  BUFEL(NB14+3*I-1)
             ISK(1)=NINT(X1*S)
             ISK(2)=NINT(Y1*S)
             ISK(3)=NINT(Z1*S)
             ISK(4)=NINT(X2*S3000)
             ISK(5)=NINT(Y2*S3000)
             ISK(6)=NINT(Z2*S3000)
             CALL WRITE_S_C(ISK,6)
            ENDDO
C
          ELSEIF (IGTYP >= 29 .AND. IGTYP <= 32) THEN
C
            DO I=LFT,LLT
             JJ = 3*(I-1)
             N = I + NFT
             X1=X(1,IXR(3,N))-X(1,IXR(2,N))
             Y1=X(2,IXR(3,N))-X(2,IXR(2,N))
             Z1=X(3,IXR(3,N))-X(3,IXR(2,N))
             S=S3000/MAX(EM20,SQRT(X1*X1+Y1*Y1+Z1*Z1))
             X2 = GBUF%SKEW(JJ + 1)
             Y2 = GBUF%SKEW(JJ + 2)
             Z2 = GBUF%SKEW(JJ + 3)
cc             X2=  BUFEL(NB14+3*I-3)
cc             Y2=  BUFEL(NB14+3*I-2)
cc             Z2=  BUFEL(NB14+3*I-1)
             ISK(1)=NINT(X1*S)
             ISK(2)=NINT(Y1*S)
             ISK(3)=NINT(Z1*S)
             ISK(4)=NINT(X2*S3000)
             ISK(5)=NINT(Y2*S3000)
             ISK(6)=NINT(Z2*S3000)
             CALL WRITE_S_C(ISK,6)
            ENDDO
C
          ELSEIF (IGTYP == 33 .OR. IGTYP == 45) THEN
            DO I=LFT,LLT
              JJ = 22*(I-1)
              N = I + NFT
              EX(1) = GBUF%VAR(JJ + 1)  ! UVAR(22,I)= EX(1)
              EX(2) = GBUF%VAR(JJ + 2)  ! UVAR(23,I)= EX(2)
              EX(3) = GBUF%VAR(JJ + 3)  ! UVAR(24,I)= EX(3)
              EX(4) = GBUF%VAR(JJ + 4)  ! UVAR(25,I)= EX(4)
              EX(5) = GBUF%VAR(JJ + 5)  ! UVAR(26,I)= EX(5)
              EX(6) = GBUF%VAR(JJ + 6)  ! UVAR(27,I)= EX(6)
cc              EX(1) = BUFEL(NB15+22)
cc              EX(2) = BUFEL(NB15+23)
cc              EX(3) = BUFEL(NB15+24)
cc              EX(4) = BUFEL(NB15+25)
cc              EX(5) = BUFEL(NB15+26)
cc              EX(6) = BUFEL(NB15+27)
              ISK(1)=NINT(EX(1)*S3000)
              ISK(2)=NINT(EX(2)*S3000)
              ISK(3)=NINT(EX(3)*S3000)
              ISK(4)=NINT(EX(4)*S3000)
              ISK(5)=NINT(EX(5)*S3000)
              ISK(6)=NINT(EX(6)*S3000)
              CALL WRITE_S_C(ISK,6)
            ENDDO
          ENDIF ! IF (IGTYP)
        ENDIF ! IF (ITY)
C-----------------------------------------------
C       FIN DE BOUCLE
C-----------------------------------------------
      ENDDO ! DO NG=1,NGROUP
C
      RETURN
      END
