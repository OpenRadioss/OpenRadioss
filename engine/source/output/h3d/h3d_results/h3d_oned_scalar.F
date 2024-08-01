Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||====================================================================
      !||    h3d_oned_scalar_mod   ../engine/source/output/h3d/h3d_results/h3d_oned_scalar.F
      !||--- called by ------------------------------------------------------
      !||    genh3d                ../engine/source/output/h3d/h3d_results/genh3d.F
      !||====================================================================
      MODULE H3D_ONED_SCALAR_MOD
        CONTAINS
      !||====================================================================
      !||    h3d_oned_scalar        ../engine/source/output/h3d/h3d_results/h3d_oned_scalar.F
      !||--- called by ------------------------------------------------------
      !||    genh3d                 ../engine/source/output/h3d/h3d_results/genh3d.F
      !||--- calls      -----------------------------------------------------
      !||    h3d_write_scalar       ../engine/source/output/h3d/h3d_results/h3d_write_scalar.F
      !||--- uses       -----------------------------------------------------
      !||    elbufdef_mod           ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
      !||====================================================================
      SUBROUTINE H3D_ONED_SCALAR(
     .                  ELBUF_TAB      , IPARG    , GEO   , IXT     ,
     .                  IXP            , IXR     , PM    , ANIM    ,
     .                  ONED_SCALAR    , ID_ELEM , ITY_ELEM ,  
     .                  IS_WRITTEN_ONED, IPARTT  , IPARTP   , IPARTR, H3D_PART,
     .                  KEYWORD        , X       , D        ,IPT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
!ngroup
#include      "com01_c.inc"
!NUMELP, NUMELT, NUMELR
#include      "com04_c.inc"
!ANIM_FE
#include      "scr14_c.inc"
! nparg, npropm, npropg,
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*),
     .   ANIM(*),ONED_SCALAR(*),X(3,*),D(3,*)
      INTEGER IPARG(NPARG,*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),
     .        IPT,
     .        IS_WRITTEN_ONED(*),ID_ELEM(*),ITY_ELEM(*),
     .        IPARTT(*) ,IPARTP(*),IPARTR(*),H3D_PART(*)
C
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      CHARACTER(LEN=NCHARLINE100) :: KEYWORD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   MASS(MVSIZ) ,
     .   OFF, VALUE(MVSIZ),
     .   A1,B1,B2,B3,YEQ,F1,M1,M2,M3, XM, DAMMX, VOL,   
     .   FOR, AREA, FEQ, EPLAS, RHO0, A0, XX1, YY1, ZZ1, AL0,EFRAC
      INTEGER I,  NG, NEL, NFT,  ITY,  NPT,  
     .        N,   MLW,   
     .        IGTYP,IFAIL,
     .        IPID,
     .        NN1,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,
     .        OFFSET,K,KK,ILAYER,IR,IS,JJ(6),IOK_PART(MVSIZ),
     .        IS_WRITTEN_VALUE(MVSIZ),N1, N2
C-----------------------------------------------
C
      NN1 = 1
      NN3 = 1
      NN4 = NN3 
      NN5 = NN4 
      NN6 = NN5 
      NN7 = NN6 + NUMELT
      NN8 = NN7 + NUMELP
      NN9 = NN8 + NUMELR
      NN10= NN9 
C
      DO NG=1,NGROUP
        MLW   =IPARG(1,NG)
        NEL   =IPARG(2,NG)
        ITY   =IPARG(5,NG)
        IGTYP =IPARG(38,NG)
        IFAIL =IPARG(43,NG)
C---
        NFT   =IPARG(3,NG)
        DO I=1,6
          JJ(I) = NEL*(I-1)
        ENDDO

        DO I=1,NEL
          VALUE(I) = ZERO
          IS_WRITTEN_VALUE(I) = 0
        ENDDO   
c
        IF (ITY == 4) OFFSET = 0
        IF (ITY == 5) OFFSET = NUMELT
        IF (ITY == 6) OFFSET = NUMELT+NUMELP
c
        DO  I=1,NEL 
          IF (ITY == 4) THEN
            ID_ELEM(OFFSET+NFT+I) = IXT(NIXT,NFT+I)
            ITY_ELEM(OFFSET+NFT+I) = 4
            IF( H3D_PART(IPARTT(NFT+I)) == 1) IOK_PART(I) = 1
          ELSEIF (ITY == 5) THEN 
            ID_ELEM(OFFSET+NFT+I) = IXP(NIXP,NFT+I)
            ITY_ELEM(OFFSET+NFT+I) = 5
            IF( H3D_PART(IPARTP(NFT+I)) == 1) IOK_PART(I) = 1
          ELSEIF (ITY == 6) THEN 
            ID_ELEM(OFFSET+NFT+I) = IXR(NIXR,NFT+I)
            ITY_ELEM(OFFSET+NFT+I) = 6
            IF( H3D_PART(IPARTR(NFT+I)) == 1) IOK_PART(I) = 1
          ENDIF
        ENDDO 

        IF(ITY==4 .OR. ITY==5 .OR. ITY==6)THEN
          DO I=1,NEL
            ONED_SCALAR(OFFSET+NFT+I) = ZERO   ! Default = zero in all cases !
          ENDDO
        ENDIF
C-----------------------------------------------
C Mass computation
C-----------------------------------------------
        IF (KEYWORD == 'MASS') THEN
C-----------------------------------------------
C       TRUSS
C-----------------------------------------------
                IF (ITY == 4) THEN
                  DO I=1,NEL
                    N = I + NFT
                    RHO0 = PM(1,IXT(1,N))
                    A0 = GEO(1,IXT(4,N))
                    N1 = IXT(2,N)
                    N2 = IXT(3,N)
                    XX1 = X(1,N2)-D(1,N2)-X(1,N1)+D(1,N1)
                    YY1 = X(2,N2)-D(2,N2)-X(2,N1)+D(2,N1)
                    ZZ1 = X(3,N2)-D(3,N2)-X(3,N1)+D(3,N1)
                    AL0  = HALF*SQRT(XX1*XX1 + YY1*YY1 + ZZ1*ZZ1)
                    MASS(I) = RHO0*AL0*A0
                  ENDDO
C-----------------------------------------------
C       POUTRES
C-----------------------------------------------
                ELSEIF (ITY == 5) THEN
                  DO I=1,NEL
                    N = I + NFT
                    RHO0 = PM(1,IXP(1,N))
                    A0 = GEO(1,IXP(5,N))
                    N1 = IXP(2,N)
                    N2 = IXP(3,N)
                    XX1 = X(1,N2)-D(1,N2)-X(1,N1)+D(1,N1)
                    YY1 = X(2,N2)-D(2,N2)-X(2,N1)+D(2,N1)
                    ZZ1 = X(3,N2)-D(3,N2)-X(3,N1)+D(3,N1)
                    AL0  = HALF*SQRT(XX1*XX1 + YY1*YY1 + ZZ1*ZZ1)
                    MASS(I) = RHO0*AL0*A0
                  ENDDO
C-----------------------------------------------
C       RESSORTS
C-----------------------------------------------
                ELSEIF (ITY == 6) THEN
                  IF(MLW==3)THEN
                    DO I=1,NEL
                      N = I + NFT
                      MASS(I) = HALF*GEO(1,IXR(1,N))
                    ENDDO
                  ELSEIF (MLW == 5) THEN
                    DO I=1,NEL
                      N = I + NFT
                      MASS(I) = ELBUF_TAB(NG)%GBUF%MASS(I)
                    ENDDO
                  ELSE
                    DO I=1,NEL
                      N = I + NFT
                      MASS(I) = GEO(1,IXR(1,N))
                    ENDDO
                  ENDIF ! IF(MLW)
                ENDIF ! IF (ITY)
            ENDIF
C-----------------------------------------------
C       TRUSS
C-----------------------------------------------
        IF(ITY==4)THEN
C--------------------------------------------------
          IF (KEYWORD == 'MASS') THEN
C--------------------------------------------------
            DO I=1,NEL
              VALUE(I) = MASS(I)
              IS_WRITTEN_VALUE(I) = 1
            ENDDO
C--------------------------------------------------
          ELSEIF(KEYWORD == 'EPSP')THEN
C--------------------------------------------------
           IF(MLW/=1)THEN
            DO  I=1,NEL
              OFF = ELBUF_TAB(NG)%GBUF%OFF(I)
              VALUE(I) = ELBUF_TAB(NG)%GBUF%PLA(I)
              IS_WRITTEN_VALUE(I) = 1
            ENDDO
           ENDIF
C--------------------------------------------------
          ELSEIF(KEYWORD == 'ENER')THEN
C--------------------------------------------------
            DO I=1,NEL
             VALUE(I) =ELBUF_TAB(NG)%GBUF%EINT(I)/MAX(EM30,MASS(I))
             IS_WRITTEN_VALUE(I) = 1
            ENDDO
C--------------------------------------------------
          ELSEIF(KEYWORD == 'EINT')THEN
C--------------------------------------------------
            DO I=1,NEL
             VALUE(I) =ELBUF_TAB(NG)%GBUF%EINT(I)
             IS_WRITTEN_VALUE(I) = 1
            ENDDO
C--------------------------------------------------
          ELSEIF(KEYWORD == 'VONM')THEN
C--------------------------------------------------
            DO I=1,NEL
             FOR = ELBUF_TAB(NG)%GBUF%FOR(I)
             AREA = ELBUF_TAB(NG)%GBUF%AREA(I)
             FEQ = FOR*FOR
             VALUE(I) = SQRT(FEQ)/AREA
             IS_WRITTEN_VALUE(I) = 1
            ENDDO
C--------------------------------------------------
          ELSEIF(KEYWORD == 'SIGX')THEN  
C--------------------------------------------------
            DO I=1,NEL                 
             VALUE(I)=(ELBUF_TAB(NG)%GBUF%FOR(I))/(ELBUF_TAB(NG)%GBUF%AREA(I))
             IS_WRITTEN_VALUE(I) = 1
            ENDDO
C--------------------------------------------------
          ELSEIF(KEYWORD == 'DT')THEN 
C--------------------------------------------------
            IF(ELBUF_TAB(NG)%GBUF%G_DT>0)THEN                      
              DO I=1,NEL
                VALUE(I) = ELBUF_TAB(NG)%GBUF%DT(I)
                IS_WRITTEN_VALUE(I) = 1
             ENDDO 
            ENDIF
C--------------------------------------------------
          ELSEIF (KEYWORD == 'AMS'.AND.(ELBUF_TAB(NG)%GBUF%G_ISMS>0)) THEN 
C--------------------------------------------------                      
              DO I=1,NEL
                VALUE(I) = ELBUF_TAB(NG)%GBUF%ISMS(I)
                IS_WRITTEN_VALUE(I) = 1
              ENDDO 
C--------------------------------------------------
          ELSEIF(KEYWORD == 'OFF')THEN  
C--------------------------------------------------                     
            DO I=1,NEL
              IF (ELBUF_TAB(NG)%GBUF%G_OFF > 0) THEN
                IF(ELBUF_TAB(NG)%GBUF%OFF(I) > ONE) THEN
                  VALUE(I) = ELBUF_TAB(NG)%GBUF%OFF(I) - ONE
                ELSEIF((ELBUF_TAB(NG)%GBUF%OFF(I) >= ZERO .AND. ELBUF_TAB(NG)%GBUF%OFF(I) <= ONE)) THEN
                  VALUE(I) = ELBUF_TAB(NG)%GBUF%OFF(I)
                ELSE
                  VALUE(I) = -ONE
                ENDIF
              ENDIF
              IS_WRITTEN_VALUE(I) = 1
            ENDDO
C--------------------------------------------------
          ELSEIF(KEYWORD == 'GROUP')THEN 
C-------------------------------------------------- 
             DO I=1,NEL                    
               VALUE(I) = NG               
               IS_WRITTEN_VALUE(I) = 1     
             ENDDO                         
C--------------------------------------------------
          ELSEIF(KEYWORD == 'INTERNAL.ID')THEN 
C-------------------------------------------------- 
            DO I=1,NEL
              VALUE(I) = I+NFT
              IS_WRITTEN_VALUE(I) = 1  
            ENDDO
C--------------------------------------------------
          ELSEIF(KEYWORD == 'LOCAL.ID')THEN 
C-------------------------------------------------- 
            DO I=1,NEL
              VALUE(I) = I
              IS_WRITTEN_VALUE(I) = 1  
            ENDDO              
          ENDIF
C-----------------------------------------------
C       POUTRES
C-----------------------------------------------
        ELSEIF(ITY==5)THEN
C--------------------------------------------------
          IF (KEYWORD == 'MASS') THEN
C--------------------------------------------------
            DO I=1,NEL
              VALUE(I) = MASS(I)
              IS_WRITTEN_VALUE(I) = 1
            ENDDO
C--------------------------------------------------
          ELSEIF (KEYWORD == 'EPSP') THEN
C--------------------------------------------------
            IF (MLW /= 1) THEN
              IF (IGTYP == 18) THEN
!   ILAYER=NULL,   NPT=NULL
                ILAYER=1
                IR = 1
                IS = 1
                NPT  = IPARG(6,NG)
                IF (IPT == -1 .AND. ELBUF_TAB(NG)%GBUF%G_PLA > 0) THEN
                  DO  I=1,NEL
                    EPLAS = ZERO
                    DO K = 1,NPT
                      EPLAS = EPLAS + ELBUF_TAB(NG)%BUFLY(ILAYER)%LBUF(IR,IS,K)%PLA(I)
                    ENDDO
                    VALUE(I) = EPLAS/NPT
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO
!   ILAYER=NULL,   NPT=IPT
                ELSEIF ( IPT <= NPT .AND. IPT > 0 .AND. ELBUF_TAB(NG)%GBUF%G_PLA > 0) THEN
                  DO  I=1,NEL
                    VALUE(I) = ELBUF_TAB(NG)%BUFLY(ILAYER)%LBUF(IR,IS,IPT)%PLA(I)
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO
                ENDIF ! IF (IPT == -1 .AND. ELBUF_TAB(NG)%GBUF%G_PLA > 0)
              ELSE ! (IGTYP == 3)
                DO  I=1,NEL
                  OFF = ELBUF_TAB(NG)%GBUF%OFF(I)
                  VALUE(I) = ELBUF_TAB(NG)%GBUF%PLA(I)
                  IS_WRITTEN_VALUE(I) = 1
                ENDDO
              ENDIF ! IF (IGTYP == 18)
            ENDIF ! IF (MLW /= 1)
C--------------------------------------------------
          ELSEIF(KEYWORD == 'ENER')THEN
C--------------------------------------------------
            DO I=1,NEL
ca mass a recalculer !!        VALUE(I) = (ELBUF_TAB(NG)%GBUF%EINT(I) + ELBUF_TAB(NG)%GBUF%EINT(I)) / MAX(EM30,MASS(NFT+I))
             VALUE = (ELBUF_TAB(NG)%GBUF%EINT(I) + ELBUF_TAB(NG)%GBUF%EINT(I)) 
             IS_WRITTEN_VALUE(I) = 1
           
            ENDDO
C--------------------------------------------------
          ELSEIF(KEYWORD == 'EINTV')THEN
C--------------------------------------------------
            DO I=1,NEL
              N  = I + NFT
              IPID = IXP(5,N)
              VOL = GEO(1,IPID)*ELBUF_TAB(NG)%GBUF%LENGTH(I)
              VALUE(I) = (ELBUF_TAB(NG)%GBUF%EINT(I)+ELBUF_TAB(NG)%GBUF%EINT(I+NEL))/MAX(VOL,EM20)
              IS_WRITTEN_VALUE(I) = 1             
            ENDDO              
C--------------------------------------------------
          ELSEIF(KEYWORD == 'VONM')THEN
C--------------------------------------------------
            DO I=1,NEL  
             N = I + NFT
             A1 = GEO(1,IXP(5,N))
             B1 = GEO(2,IXP(5,N))
             B2 = GEO(18,IXP(5,N))
             B3 = GEO(4,IXP(5,N))
             F1 = ELBUF_TAB(NG)%GBUF%FOR(JJ(1)+I)
             M1 = ELBUF_TAB(NG)%GBUF%MOM(JJ(1) + I)
             M2 = ELBUF_TAB(NG)%GBUF%MOM(JJ(2) + I)
             M3 = ELBUF_TAB(NG)%GBUF%MOM(JJ(3) + I)
             YEQ= F1*F1 + THREE* A1 *
     +                  ( M1*M1 / MAX(B3,EM30)
     +                  + M2*M2 / MAX(B1,EM30)
     +                  + M3*M3 / MAX(B2,EM30) )
             VALUE(I) = SQRT(YEQ)/A1
             IS_WRITTEN_VALUE(I) = 1
            ENDDO
C--------------------------------------------------
          ELSEIF(KEYWORD == 'SIGX')THEN
C--------------------------------------------------
            DO I=1,NEL
             N = I + NFT
             VALUE(I) = ELBUF_TAB(NG)%GBUF%FOR(JJ(1)+I) / GEO(1,IXP(5,N))
             IS_WRITTEN_VALUE(I) = 1
            ENDDO
C--------------------------------------------------
          ELSEIF(KEYWORD == 'SIGXY')THEN
C--------------------------------------------------
            DO I=1,NEL
             N = I + NFT
             VALUE(I) = ELBUF_TAB(NG)%GBUF%FOR(JJ(2)+I) / GEO(1,IXP(5,N))
             IS_WRITTEN_VALUE(I) = 1
            ENDDO
C--------------------------------------------------
          ELSEIF(KEYWORD == 'SIGZX')THEN
C--------------------------------------------------
            DO I=1,NEL
             N = I + NFT
             VALUE(I) = ELBUF_TAB(NG)%GBUF%FOR(JJ(3)+I) / GEO(1,IXP(5,N))
             IS_WRITTEN_VALUE(I) = 1
            ENDDO
C--------------------------------------------------
          ELSEIF(KEYWORD == 'DT')THEN 
C--------------------------------------------------                      
            DO I=1,NEL
              VALUE(I) = ELBUF_TAB(NG)%GBUF%DT(I)
              IS_WRITTEN_VALUE(I) = 1
            ENDDO
C--------------------------------------------------
          ELSEIF (KEYWORD == 'AMS'.AND.(ELBUF_TAB(NG)%GBUF%G_ISMS>0)) THEN 
C--------------------------------------------------                      
              DO I=1,NEL
                VALUE(I) = ELBUF_TAB(NG)%GBUF%ISMS(I)
              IS_WRITTEN_VALUE(I) = 1
              ENDDO 
C--------------------------------------------------
          ELSEIF(KEYWORD == 'OFF')THEN  
C--------------------------------------------------                     
            DO I=1,NEL
              VALUE(I) = ELBUF_TAB(NG)%GBUF%OFF(I)
              IS_WRITTEN_VALUE(I) = 1
            ENDDO
C--------------------------------------------------
          ELSEIF(KEYWORD == 'FRACTION/MARTENSITE')THEN  
C--------------------------------------------------                     
           IF (IGTYP == 18) THEN
!   ILAYER=NULL,   NPT=NULL
            IF (MLW == 71) THEN
                ILAYER=1
                IR = 1
                IS = 1
                NPT  = IPARG(6,NG)
                IF (IPT == -1 ) THEN ! no NPT= in engine file
                  DO  I=1,NEL
                    EFRAC = ZERO
                    DO K = 1,NPT
                      EFRAC = EFRAC + ELBUF_TAB(NG)%BUFLY(ILAYER)%LBUF(IR,IS,K)%FRAC(I)
                    ENDDO
                    VALUE(I) = EFRAC/NPT
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO
!   ILAYER=NULL,   NPT=IPT
                ELSEIF ( IPT <= NPT .AND. IPT > 0 ) THEN
                  DO  I=1,NEL
                    VALUE(I) =  ELBUF_TAB(NG)%BUFLY(ILAYER)%LBUF(IR,IS,IPT)%FRAC(I)
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO
                ENDIF ! IF (IPT == -1 .AND. ELBUF_TAB(NG)%GBUF%G_PLA > 0)
            ENDIF !(MLW /= 71) 
           ENDIF !(IGTYP == 18) 
C--------------------------------------------------
          ELSEIF(KEYWORD == 'TEPS')THEN  
C--------------------------------------------------                     
           IF (IGTYP == 18) THEN
!   ILAYER=NULL,   NPT=NULL
            IF (MLW == 71) THEN
                ILAYER=1
                IR = 1
                IS = 1
                NPT  = IPARG(6,NG)
                IF (IPT == -1 ) THEN ! no NPT= in engine file
                  DO  I=1,NEL
                    EFRAC = ZERO
                    DO K = 1,NPT
                      EFRAC = EFRAC + ELBUF_TAB(NG)%BUFLY(ILAYER)%LBUF(IR,IS,K)%PLA(I)
                    ENDDO
                    VALUE(I) = EFRAC/NPT
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO
!   ILAYER=NULL,   NPT=IPT
                ELSEIF ( IPT <= NPT .AND. IPT > 0 ) THEN
                  
                  DO  I=1,NEL
                    VALUE(I) = ELBUF_TAB(NG)%BUFLY(ILAYER)%LBUF(IR,IS,IPT)%PLA(I)
                    IS_WRITTEN_VALUE(I) = 1
                  ENDDO
                ENDIF ! IF (IPT == -1 .AND. ELBUF_TAB(NG)%GBUF%G_PLA > 0)
            ENDIF !(MLW /= 71) 
           ENDIF !(IGTYP == 18) 

C--------------------------------------------------
          ELSEIF(KEYWORD == 'FRACTION/MARTENSITE/TMAX')THEN  
C--------------------------------------------------                     
            DO I=1,NEL
              VALUE(I) = ELBUF_TAB(NG)%GBUF%MAXFRAC(I)
              IS_WRITTEN_VALUE(I) = 1
            ENDDO
C--------------------------------------------------
          ELSEIF(KEYWORD == 'GROUP')THEN 
C-------------------------------------------------- 
            DO I=1,NEL
              VALUE(I) = NG
              IS_WRITTEN_VALUE(I) = 1  
            ENDDO
C--------------------------------------------------
          ELSEIF(KEYWORD == 'INTERNAL.ID')THEN 
C-------------------------------------------------- 
            DO I=1,NEL
              VALUE(I) = I+NFT
              IS_WRITTEN_VALUE(I) = 1  
            ENDDO
C--------------------------------------------------
          ELSEIF(KEYWORD == 'LOCAL.ID')THEN 
C-------------------------------------------------- 
            DO I=1,NEL
              VALUE(I) = I
              IS_WRITTEN_VALUE(I) = 1  
            ENDDO   
C--------------------------------------------------
          ELSEIF(KEYWORD == 'EPSD')THEN  
C--------------------------------------------------                     
            DO I=1,NEL
              VALUE(I) = ELBUF_TAB(NG)%GBUF%EPSD(I)
              IS_WRITTEN_VALUE(I) = 1
            ENDDO          
C--------------------------------------------------
          ELSEIF (KEYWORD == 'DAMA' .and. IFAIL > 0) THEN  
C--------------------------------------------------
            IF (IGTYP == 3) THEN
              DO I=1,NEL
                VALUE(I) = ELBUF_TAB(NG)%GBUF%FAIL(1)%DAMMX(I)
                IS_WRITTEN_VALUE(I) = 1
              ENDDO
            ELSE IF (IGTYP == 18) THEN
              DO I=1,NEL
                DAMMX  = ZERO
                DO K = 1,ELBUF_TAB(NG)%BUFLY(1)%NPTT
                  DAMMX = MAX(DAMMX ,ELBUF_TAB(NG)%BUFLY(1)%FAIL(1,1,K)%FLOC(1)%DAMMX(I))
                ENDDO
                VALUE(I) = DAMMX
                IS_WRITTEN_VALUE(I) = 1
              ENDDO
            ENDIF
c           
          ENDIF
C-----------------------------------------------
C       RESSORTS
C-----------------------------------------------
        ELSEIF(ITY==6)THEN
C--------------------------------------------------
          IF (KEYWORD == 'MASS') THEN
C--------------------------------------------------
            DO I=1,NEL
              VALUE(I) = MASS(I)
              IS_WRITTEN_VALUE(I) = 1
            ENDDO
C--------------------------------------------------
          ELSEIF(KEYWORD == 'ENER')THEN
C--------------------------------------------------
           IF (MLW==1) THEN
            XM = ONE/GEO(1,IXR(1,1+NFT))
            DO  I=1,NEL
C             XM cannot be zero (was checked in starter).
              VALUE(I) = ELBUF_TAB(NG)%GBUF%EINT(I)*XM
              IS_WRITTEN_VALUE(I) = 1
            ENDDO
           ELSEIF (MLW==2) THEN
            XM = ONE/GEO(1,IXR(1,1+NFT))
            DO  I=1,NEL
C             XM cannot be zero (was checked in starter).
              VALUE(I) = ELBUF_TAB(NG)%GBUF%EINT(I)*XM
              IS_WRITTEN_VALUE(I) = 1
            ENDDO
           ELSEIF (MLW==3) THEN
            XM = ONE/GEO(1,IXR(1,1+NFT))
            DO  I=1,NEL
C             XM cannot be zero (was checked in starter).
              VALUE(I) = ELBUF_TAB(NG)%GBUF%EINT(I)*XM
              IS_WRITTEN_VALUE(I) = 1
            ENDDO
           ELSEIF (MLW==4) THEN
            XM = ONE/GEO(1,IXR(1,1+NFT))
            DO  I=1,NEL
C             XM cannot be zero (was checked in starter).
              VALUE(I) = ELBUF_TAB(NG)%GBUF%EINT(I)*XM
              IS_WRITTEN_VALUE(I) = 1
            ENDDO
           ELSEIF (MLW==5) THEN
C           user springs.
            DO  I=1,NEL
              VALUE(I) = ELBUF_TAB(NG)%GBUF%EINT(I)/MAX(EM30,ELBUF_TAB(NG)%GBUF%MASS(I))
              IS_WRITTEN_VALUE(I) = 1
            ENDDO
C           Spring axi
           ELSEIF (MLW==6) THEN
            XM = ONE/GEO(1,IXR(1,1+NFT))
            DO  I=1,NEL
C             XM cannot be zero (was checked in starter).
              VALUE(I) = ELBUF_TAB(NG)%GBUF%EINT(I)*XM
              IS_WRITTEN_VALUE(I) = 1
            ENDDO
           ELSEIF (MLW==7) THEN
            XM = ONE/GEO(1,IXR(1,1+NFT))
            DO  I=1,NEL
              VALUE(I) = ELBUF_TAB(NG)%GBUF%EINT(I)*XM
              IS_WRITTEN_VALUE(I) = 1
            ENDDO
           ENDIF
C--------------------------------------------------
          ELSEIF(KEYWORD == 'DAM1')THEN
C--------------------------------------------------
            DO  I=1,NEL
              VALUE(I) = ANIM(I)
              IS_WRITTEN_VALUE(I) = 1
            ENDDO
C--------------------------------------------------
          ELSEIF(KEYWORD == 'DAM2')THEN
C--------------------------------------------------
            KK = NUMELR * ANIM_FE(11)
            DO  I=1,NEL
              VALUE(I) = ANIM(I+KK)
              IS_WRITTEN_VALUE(I) = 1
            ENDDO
C--------------------------------------------------
          ELSEIF(KEYWORD == 'DAM3')THEN
C--------------------------------------------------
            KK = NUMELR * (ANIM_FE(11)+ANIM_FE(12))
            DO  I=1,NEL
              VALUE = ANIM(I+KK)
              IS_WRITTEN_VALUE(I) = 1
            ENDDO
C--------------------------------------------------
          ELSEIF(KEYWORD == 'DT' .AND. ELBUF_TAB(NG)%GBUF%G_DT/=0)THEN  
C--------------------------------------------------                     
            DO I=1,NEL
              VALUE(I) = ELBUF_TAB(NG)%GBUF%DT(I)
              IS_WRITTEN_VALUE(I) = 1
            ENDDO
C--------------------------------------------------
          ELSEIF (KEYWORD == 'AMS'.AND.(ELBUF_TAB(NG)%GBUF%G_ISMS>0)) THEN   
C--------------------------------------------------                    
            DO I=1,NEL
              VALUE(I) = ELBUF_TAB(NG)%GBUF%ISMS(I)
              IS_WRITTEN_VALUE(I) = 1
            ENDDO 
C--------------------------------------------------
          ELSEIF(KEYWORD == 'OFF')THEN  
C--------------------------------------------------                     
            DO I=1,NEL
              VALUE(I) = ELBUF_TAB(NG)%GBUF%OFF(I)
              IS_WRITTEN_VALUE(I) = 1
            ENDDO 
C--------------------------------------------------
          ELSEIF(KEYWORD == 'GROUP')THEN 
C-------------------------------------------------- 
            DO I=1,NEL
              VALUE(I) = NG
              IS_WRITTEN_VALUE(I) = 1  
            ENDDO
          ENDIF
C--------------------------------------------------
          ELSEIF(KEYWORD == 'INTERNAL.ID')THEN 
C-------------------------------------------------- 
            DO I=1,NEL
              VALUE(I) = I+NFT
              IS_WRITTEN_VALUE(I) = 1  
            ENDDO
C--------------------------------------------------
          ELSEIF(KEYWORD == 'LOCAL.ID')THEN 
C-------------------------------------------------- 
            DO I=1,NEL
              VALUE(I) = I
              IS_WRITTEN_VALUE(I) = 1  
            ENDDO              
          ENDIF
        
        IF (ITY == 4 .OR. ITY == 5 .OR. ITY == 6)
     .    CALL H3D_WRITE_SCALAR(IOK_PART,IS_WRITTEN_ONED,ONED_SCALAR,NEL,OFFSET,NFT,
     .               VALUE,IS_WRITTEN_VALUE)
      ENDDO ! DO NG=1,NGROUP

 
      RETURN
      END

      END MODULE
