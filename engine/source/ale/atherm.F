Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||    atherm                 ../engine/source/ale/atherm.F
      !||--- called by ------------------------------------------------------
      !||    alethe                 ../engine/source/ale/alethe.F
      !||--- calls      -----------------------------------------------------
      !||    adiff2                 ../engine/source/ale/ale2d/adiff2.F
      !||    adiff3                 ../engine/source/ale/ale3d/adiff3.F
      !||    afimp2                 ../engine/source/ale/ale2d/afimp2.F
      !||    afimp3                 ../engine/source/ale/ale3d/afimp3.F
      !||    initbuf                ../engine/share/resol/initbuf.F
      !||    m18th                  ../engine/source/materials/mat/mat018/m18th.F
      !||    m26th                  ../engine/source/materials/mat/mat026/m26th.F
      !||    m51th                  ../engine/source/materials/mat/mat051/heat51.F
      !||    spmd_evois             ../engine/source/mpi/fluid/spmd_cfd.F
      !||--- uses       -----------------------------------------------------
      !||    ale_connectivity_mod   ../common_source/modules/ale/ale_connectivity_mod.F
      !||    elbufdef_mod           ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    initbuf_mod            ../engine/share/resol/initbuf.F
      !||    matparam_def_mod       ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    multimat_param_mod     ../common_source/modules/multimat_param_mod.F90
      !||====================================================================
      SUBROUTINE ATHERM(
     1   IPARG,      PM,         ELBUF_TAB,  FLUX,
     2   VAL2,       T,          ALE_CONNECT,IXS,
     3   IXQ,        FV,         X,          BUFMAT,
     4   TF,         NPF,        NERCVOIS,   NESDVOIS,
     5   LERCVOIS,   LESDVOIS,   LENCOM,     IPM,
     6   MATPARAM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD            
      USE ALE_CONNECTIVITY_MOD
      USE MULTIMAT_PARAM_MOD , ONLY : M51_N0PHAS, M51_NVPHAS
      USE MATPARAM_DEF_MOD, ONLY : MATPARAM_STRUCT_
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IPARG(NPARG,NGROUP), IXS(NIXS,NUMELS), IXQ(7,NUMELQ), NPF(*),
     .         NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*),
     .         IPM(NPROPMI,NUMMAT), LENCOM
      my_real PM(NPROPM,NUMMAT), FLUX(*), VAL2(*), T(*), FV(*), X(3,NUMNOD),TF(*),BUFMAT(*)
      TYPE (ELBUF_STRUCT_), DIMENSION (NGROUP), TARGET :: ELBUF_TAB
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      TYPE(MATPARAM_STRUCT_),DIMENSION(NUMMAT),INTENT(IN) :: MATPARAM !< material buffer
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG, I, J, NPH1, NPH2, NPH3, IADBUF
      INTEGER MAT(MVSIZ)
      my_real RK, RE, R, YP0, XMU, AX, E, A, CMU, RPR, YPLUS, P, XMT
      my_real, DIMENSION(:) ,POINTER :: PH1,PH2,PH3
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
      INTEGER :: NEL !< number of elements in current group
      INTEGER :: MID !< material identifier
      my_real :: rhocp !< density * heat capacity at constant pressure
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
      DO NG=1,NGROUP
        IF(IPARG(8,NG) == 1)CYCLE
        IF (IPARG(76, NG) == 1) CYCLE ! --> OFF  (ALE ON/OFF)
        GBUF => ELBUF_TAB(NG)%GBUF
        CALL INITBUF(IPARG    ,NG      ,                  
     2     MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,   
     3     NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,   
     4     JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,   
     5     NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,   
     6     IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,   
     7     ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        IF((ITY /= 1).AND.(ITY /= 2))CYCLE
        IF (MTN == 1)CYCLE

        LFT=1
        NEL = IPARG(2, NG)
c
        IF(N2D == 0)THEN
          DO I=1,NEL
            MAT(I)=IXS(1,I+NFT)
          ENDDO
        ELSE
          DO I=1,NEL
            MAT(I)=IXQ(1,I+NFT)
          ENDDO
        ENDIF

        ! thermal diffusivity (alpha) : alpha = k / rhocp   where k=A+B*T
        ! VAL2 is alpha*rhocp=k instead of alpha since scheme applied to E instead of T and dE = rhocp.dT
        DO I=1,NEL
          J=I+NFT
          T(J) = GBUF%TEMP(I)
          IF(T(J) <= PM(80,MAT(I)))THEN
            VAL2(J)=PM(75,MAT(I))+PM(76,MAT(I))*T(J)
          ELSE
            VAL2(J)=PM(77,MAT(I))+PM(78,MAT(I))*T(J)
          ENDIF
        ENDDO

        IF (MTN == 17)THEN
          DO I=1,NEL
           J=I+NFT
           RK = GBUF%RK(I)
           RE = GBUF%RE(I)
           R  = GBUF%RHO(I)
           YP0=PM(51,MAT(I))
           XMU=R*PM(24,MAT(I))
           AX =PM(47,MAT(I))
           E  =PM(48,MAT(I))
           A  =PM(49,MAT(I))
           CMU=PM(81,MAT(I))
           RPR=PM(95,MAT(I))
           YPLUS =CMU*RK**2/MAX(AX*RE*XMU,EM15)
           IF(YPLUS < YP0)CYCLE
            P = NINEP24*(RPR-ONE)/(RPR**FOURTH)
            VAL2(J)=VAL2(J) * RPR*AX*YPLUS / (A*LOG(E*YPLUS) + AX*P)
          ENDDO!next I
        ELSEIF (MTN == 18)THEN
          CALL M18TH( GBUF%TEMP,VAL2,     MAT,      PM,
     2                IPM,      TF,       NPF,      NEL)
        ELSEIF (MTN == 26)THEN
          CALL M26TH( MAT,      GBUF%RHO, GBUF%TEMP,VAL2,
     2                PM,       BUFMAT,   GBUF%RE,  NEL,
     3                NFT)
        ELSEIF (MTN == 51) THEN
          NPH1 = (M51_N0PHAS)*NEL
          NPH2 = (M51_N0PHAS + M51_NVPHAS)*NEL
          NPH3 = (M51_N0PHAS + M51_NVPHAS*2)*NEL
          IADBUF = IPM(7,MAT(1))
          PH1 =>ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(NPH1+1:NPH1+1+NEL)
          PH2 =>ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(NPH2+1:NPH2+1+NEL)
          PH3 =>ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(NPH3+1:NPH3+1+NEL)
          CALL M51TH( T(1+NFT),      PH1,           PH2,           PH3,
     2                BUFMAT(IADBUF),VAL2(1+NFT),   NEL)
        ELSEIF (JTUR /= 0.AND.MTN /= 11) THEN
          DO I=1,NEL
            J=I+NFT
            RK = GBUF%RK(I)
            RE = GBUF%RE(I)
            R  = GBUF%RHO(I)
            XMT= PM(81,MAT(I))*RK*RK / MAX(EM15,RE)
            XMU= R*PM(24,MAT(I))
            RPR= PM(95,MAT(I))
            VAL2(J)=VAL2(J)*(ONE+RPR*XMT/XMU)
          ENDDO
        ENDIF
      ENDDO!next NG
C-----------------------------
C       SPMD EXCHANGE T, VAL2
C-----------------------------
        IF (NSPMD > 1)THEN
          CALL SPMD_EVOIS(T, VAL2, NERCVOIS, NESDVOIS, LERCVOIS, LESDVOIS, LENCOM)
        ENDIF
C------------------------------
C     IMPOSED FLUX
C------------------------------
      DO NG=1,NGROUP
        IF (IPARG(76, NG) == 1) CYCLE ! --> OFF (ALE ON/OFF)
        MTN=IPARG(1,NG)
        IF (MTN /= 11) CYCLE
        JTHE=IPARG(13,NG)
        IF (JTHE /= 1) CYCLE
        LFT=1
        LLT=IPARG(2,NG)
        NEL=IPARG(2,NG)
        NFT=IPARG(3,NG)
        IAD=IPARG(4,NG)
        LFT=1
        IF(N2D == 0)THEN
          CALL AFIMP3(PM,X,IXS,T,FLUX(6*NFT+1),VAL2,ALE_CONNECT,FV)
        ELSE
          CALL AFIMP2(PM,X,IXQ,T,FLUX(4*NFT+1),VAL2,ALE_CONNECT,FV)
        ENDIF
      ENDDO

C------------------------------
C     NUMERICAL SCHEME
C------------------------------
      DO NG=1,NGROUP
        IF (IPARG(76, NG) == 1) CYCLE ! --> OFF (ALE ON/OFF)
        GBUF => ELBUF_TAB(NG)%GBUF
        CALL INITBUF(IPARG    ,NG      ,                  
     2     MTN     ,LLT     ,NFT     ,IAD     ,ITY     ,   
     3     NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,   
     4     JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,   
     5     NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,   
     6     IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,   
     7     ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        IF (IPARG(8,NG) == 1)CYCLE
        IF (JTHE /= 1 .OR. ITY == 51)CYCLE
        LFT=1
        NEL=IPARG(2,NG)
        MID=IPARG(18,NG)
        rhocp = pm(69,mid)
        if(rhocp == zero)then
          rhocp = pm(89,mid)*matparam(MID)%eos%cp
        end if
        IF (MTN == 51)THEN
          DO I=1,NEL
            GBUF%TEMP(I) = ZERO
          ENDDO
          IF (N2D == 0) THEN                                            
            CALL ADIFF3(GBUF%TEMP,T,FLUX(6*NFT+1),VAL2,ALE_CONNECT,GBUF%VOL,GBUF%TEMP,rhocp,NEL)
          ELSE                                                        
            CALL ADIFF2(GBUF%TEMP,T,FLUX(4*NFT+1),VAL2,ALE_CONNECT,GBUF%VOL,GBUF%TEMP,rhocp,NEL)
          ENDIF                                                       
        ELSE
          IF (N2D == 0) THEN
            CALL ADIFF3(GBUF%EINT,T,FLUX(6*NFT+1),VAL2,ALE_CONNECT,GBUF%VOL,GBUF%TEMP,rhocp,NEL)
          ELSE
            CALL ADIFF2(GBUF%EINT,T,FLUX(4*NFT+1),VAL2,ALE_CONNECT,GBUF%VOL,GBUF%TEMP,rhocp,NEL)
          ENDIF
        ENDIF
      ENDDO!next NG
C-----------
      RETURN
      END
