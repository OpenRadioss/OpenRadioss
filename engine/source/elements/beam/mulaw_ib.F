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
Chd|  MULAW_IB                      source/elements/beam/mulaw_ib.F
Chd|-- called by -----------
Chd|        MAIN_BEAM18                   source/elements/beam/main_beam18.F
Chd|-- calls ---------------
Chd|        SIGEPS34PI                    source/materials/mat/mat034/sigeps34pi.F
Chd|        SIGEPS36PI                    source/materials/mat/mat036/sigeps36pi.F
Chd|        SIGEPS44PI                    source/materials/mat/mat044/sigeps44pi.F
Chd|        SIGEPS71PI                    source/materials/mat/mat071/sigeps71pi.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE MULAW_IB(ELBUF_STR,
     1                    NEL     ,NPT     ,MTN     ,IMAT    ,
     2                    PID     ,NGL     ,IPM     ,
     3                    GEO     ,OFF     ,FOR     ,MOM     ,
     4                    EPSP    ,BUFMAT  ,NPF     ,TF      ,
     5                    EXX     ,EXY     ,EXZ     ,KXX     ,
     6                    KYY     ,KZZ     ,JTHE    ,TEMPEL  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,MTN,IMAT,NPT,JTHE    
      INTEGER IPM(NPROPMI,*),PID(*),NPF(*),NGL(*)
      my_real 
     .   GEO(NPROPG,*),EPSP(*),BUFMAT(*),
     .   OFF(*),TF(*),
     .   FOR(NEL,3),MOM(NEL,3),EXX(NEL),EXY(NEL),
     .   EXZ(NEL),KXX(NEL),KYY(NEL),KZZ(NEL),
     .   TEMPEL(NEL)
C
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TARGET :: BUFMAT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IFUNC(100)
      INTEGER I,K,J1,IADBUF,IPT,JPLA,NFUNC,IPY,IPZ,IPA,NUVAR,NVARTMP,
     .        IR,IS,II(3),ILAYER,NUPARAM
      my_real 
     .   YPT(NEL),ZPT(NEL),APT(NEL),G(NEL),E(NEL),ETSE(NEL),
     .   DEPSXX(NEL),DEPSXY(NEL),DEPSXZ(NEL),
     .   SIGOXX(NEL),SIGOXY(NEL),SIGOXZ(NEL),
     .   SIGNXX(NEL),SIGNXY(NEL),SIGNXZ(NEL)
      my_real :: SHFACT,DFXX,DFXY,DFXZ
      my_real ,DIMENSION(NEL) :: EPSXX,EPSXY,EPSXZ,DPLA
C
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(L_BUFEL_) ,POINTER :: LBUF
      TYPE(G_BUFEL_) ,POINTER :: GBUF      
      my_real, DIMENSION(:) ,POINTER  :: UVAR
      INTEGER, DIMENSION(:), POINTER :: VARTMP
C=======================================================================
      IPY  = 200        
      IPZ  = 300        
      IPA  = 400        
      SHFACT = FIVE_OVER_6
C--------------------------------------   
      IADBUF = IPM(7,IMAT)
      NUPARAM= IPM(9,IMAT)       
      NFUNC  = IPM(10,IMAT)
      DO I=1,NFUNC
        IFUNC(I)=IPM(10+I,IMAT)
      ENDDO
C
      DO I=1,3
        II(I) = NEL*(I-1)
      ENDDO
C-------------------------------------
C     DEBUT DE BOUCLE SUR POINTS INTEGRATION
C--------------------------------------
      DO IPT=1,NPT
        ILAYER=1
        IR = 1
        IS = 1
        BUFLY => ELBUF_STR%BUFLY(ILAYER)
        LBUF  => BUFLY%LBUF(IR,IS,IPT)
        GBUF  => ELBUF_STR%GBUF           
        UVAR  => BUFLY%MAT(IR,IS,IPT)%VAR
        VARTMP=> BUFLY%MAT(IR,IS,IPT)%VARTMP
        NUVAR =  BUFLY%NVAR_MAT
        NVARTMP= BUFLY%NVARTMP

C---    Coordonnees du point d'integration
        DO I=1,NEL                                 
          YPT(I) = GEO(IPY+IPT,PID(I))              
          ZPT(I) = GEO(IPZ+IPT,PID(I))           
          APT(I) = GEO(IPA+IPT,PID(I))    
        ENDDO                                        
C
        DO I=1,NEL
          SIGNXX(I) = ZERO           
          SIGNXY(I) = ZERO           
          SIGNXZ(I) = ZERO
C
          SIGOXX(I) = LBUF%SIG(II(1)+I)
          SIGOXY(I) = LBUF%SIG(II(2)+I)
          SIGOXZ(I) = LBUF%SIG(II(3)+I)
        ENDDO
c---    Deformations Incrementales
        DO I = 1,NEL
          DEPSXX(I) = EXX(I) - YPT(I)*KZZ(I) + ZPT(I)*KYY(I)
          DEPSXY(I) = EXY(I) + ZPT(I)*KXX(I) 
          DEPSXZ(I) = EXZ(I) - YPT(I)*KXX(I)
          DEPSXY(I) = DEPSXY(I) / SHFACT
          DEPSXZ(I) = DEPSXZ(I) / SHFACT 
        ENDDO
c---    Total strain   
        IF (BUFLY%L_STRA > 0) THEN
          DO I = 1,NEL
            LBUF%STRA(II(1)+I) = LBUF%STRA(II(1)+I) + DEPSXX(I)
            LBUF%STRA(II(2)+I) = LBUF%STRA(II(2)+I) + DEPSXY(I)
            LBUF%STRA(II(3)+I) = LBUF%STRA(II(3)+I) + DEPSXZ(I)
          ENDDO
          DO I = 1,NEL
            EPSXX(I) = LBUF%STRA(II(1)+I)
            EPSXY(I) = LBUF%STRA(II(2)+I)
            EPSXZ(I) = LBUF%STRA(II(3)+I)
          END DO
        ENDIF
C
C----------------------------------------------------------------
C       CONTRAINTES ELASTO-PLASTIQUES (LOIS DE COMPORTEMENT)
C------------------------------------------
        SELECT CASE(MTN)
C---
          CASE (34)
            CALL SIGEPS34PI(
     1         NEL      ,NUPARAM  ,BUFMAT   ,IPM      ,IMAT     ,
     2         NUVAR    ,UVAR     ,DEPSXX   ,DEPSXY   ,DEPSXZ   ,
     3         EPSXX    ,SIGOXX   ,SIGOXY   ,SIGOXZ   ,
     4         SIGNXX   ,SIGNXY   ,SIGNXZ   ,DT1      ,
     5         EPSXY    ,EPSXZ )
C---
          CASE (36)
            CALL SIGEPS36PI(
     1         NEL      ,IFUNC    ,IPT      ,NPF      ,TF       ,
     2         NGL      ,TT       ,DT1      ,BUFMAT   ,IPM     ,
     3         IMAT     ,E        ,G        ,OFF      ,DEPSXX  ,
     4         DEPSXY   ,DEPSXZ   ,SIGOXX   ,SIGOXY   ,SIGOXZ  ,
     5         SIGNXX   ,SIGNXY   ,SIGNXZ   ,ETSE     ,LBUF%PLA,
     6         EPSP     ,NVARTMP  ,VARTMP)
C---
          CASE (44)
            CALL SIGEPS44PI(
     1         NEL      ,NUPARAM  ,BUFMAT   ,IPM      ,IMAT     ,
     2         OFF      ,LBUF%PLA ,DEPSXX   ,DEPSXY   ,DEPSXZ   ,
     3         SIGOXX   ,SIGOXY   ,SIGOXZ   ,EPSXX    ,EPSP     ,
     4         SIGNXX   ,SIGNXY   ,SIGNXZ   ,ETSE     ,NUVAR    ,
     5         UVAR     ,IFUNC    ,NVARTMP  ,VARTMP   ,NPF      ,
     6         TF       ,NFUNC    )
C---
          CASE (71)
            CALL SIGEPS71PI(
     1         NEL      ,NUPARAM  ,BUFMAT   ,IPM      ,IMAT     ,
     2         OFF      ,DEPSXX   ,DEPSXY   ,DEPSXZ   ,
     3         SIGOXX   ,SIGOXY   ,SIGOXZ   ,EPSXX    ,EPSXY  ,
     4         EPSXZ    ,SIGNXX   ,SIGNXY   ,SIGNXZ   ,ETSE     ,
     5         NUVAR    ,UVAR     ,IFUNC    ,NPF      ,
     6         TF       ,NFUNC    ,JTHE     ,TEMPEL   ,LBUF%FRAC,
     7         LBUF%PLA)   ! transformation strain is stored in PLA

               DO I = 1,NEL
                GBUF%MAXFRAC(I)      = MAX(GBUF%MAXFRAC(I)      , LBUF%FRAC(I) )
                GBUF%MAXEPS(II(1)+I) = MAX(GBUF%MAXEPS(II(1)+I) , ABS(EPSXX(I)))
                GBUF%MAXEPS(II(2)+I) = MAX(GBUF%MAXEPS(II(2)+I) , ABS(EPSXY(I)))
                GBUF%MAXEPS(II(3)+I) = MAX(GBUF%MAXEPS(II(3)+I) , ABS(EPSXZ(I)))
               ENDDO

          CASE DEFAULT
C---
        END SELECT
C----------------------------------------------------------------
C---    Save stress components
        DO I=1,NEL
          LBUF%SIG(II(1)+I) = SIGNXX(I)
          LBUF%SIG(II(2)+I) = SIGNXY(I)
          LBUF%SIG(II(3)+I) = SIGNXZ(I)
        ENDDO
C-----------------------
C      FORCES ET MOMENTS
C-----------------------
        DO I=1,NEL
          DFXX = APT(I)*SIGNXX(I)
          DFXY = APT(I)*SIGNXY(I)
          DFXZ = APT(I)*SIGNXZ(I)
          FOR(I,1) = FOR(I,1) + DFXX
          FOR(I,2) = FOR(I,2) + DFXY
          FOR(I,3) = FOR(I,3) + DFXZ
          MOM(I,1) = MOM(I,1) + DFXY*ZPT(I) - DFXZ*YPT(I)
          MOM(I,2) = MOM(I,2) + DFXX*ZPT(I)
          MOM(I,3) = MOM(I,3) - DFXX*YPT(I)
        ENDDO
C-------------------------------------
C       FIN DE BOUCLE SUR POINT INTEGRATION
C-------------------------------------
      ENDDO
C------------------------------------------
      RETURN
      END SUBROUTINE MULAW_IB
