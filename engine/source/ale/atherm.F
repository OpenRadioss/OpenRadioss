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
Chd|  ATHERM                        source/ale/atherm.F           
Chd|-- called by -----------
Chd|        ALETHE                        source/ale/alethe.F           
Chd|-- calls ---------------
Chd|        ADIFF2                        source/ale/ale2d/adiff2.F     
Chd|        ADIFF3                        source/ale/ale3d/adiff3.F     
Chd|        AFIMP2                        source/ale/ale2d/afimp2.F     
Chd|        AFIMP3                        source/ale/ale3d/afimp3.F     
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        M18TH                         source/materials/mat/mat018/m18th.F
Chd|        M26TH                         source/materials/mat/mat026/m26th.F
Chd|        M51TH                         source/materials/mat/mat051/heat51.F
Chd|        SPMD_EVOIS                    source/mpi/fluid/spmd_cfd.F   
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE ATHERM(
     1   IPARG,      PM,         ELBUF_TAB,  FLUX,
     2   VAL2,       T,          ALE_CONNECT,IXS,
     3   IXQ,        FV,         X,          BUFMAT,
     4   TF,         NPF,        NERCVOIS,   NESDVOIS,
     5   LERCVOIS,   LESDVOIS,   LENCOM,     IPM,
     6   NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD            
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "mmale51_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER  IPARG(NPARG,NGROUP), IXS(NIXS,NUMELS), IXQ(7,NUMELQ), NPF(*),
     .         NERCVOIS(*),NESDVOIS(*),LERCVOIS(*),LESDVOIS(*),
     .         IPM(NPROPMI,NUMMAT), LENCOM
      my_real PM(NPROPM,NUMMAT), FLUX(*), VAL2(*), T(*), FV(*), X(3,NUMNOD),TF(*),BUFMAT(*)
      TYPE (ELBUF_STRUCT_), DIMENSION (NGROUP), TARGET :: ELBUF_TAB
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NG, I, J, NPH1, NPH2, NPH3, IADBUF
      INTEGER MAT(MVSIZ)
      my_real RK, RE, R, YP0, XMU, AX, E, A, CMU, RPR, YPLUS, P, XMT
      my_real, DIMENSION(:) ,POINTER :: PH1,PH2,PH3
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------
      DO NG=1,NGROUP
        IF(IPARG(8,NG) == 1)CYCLE
c     
C     ALE ON / OFF
        IF (IPARG(76, NG) == 1) CYCLE ! --> OFF
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
c
        LFT=1
c
        IF(N2D == 0)THEN
          DO I=LFT,LLT
            MAT(I)=IXS(1,I+NFT)
          ENDDO
        ELSE
          DO I=LFT,LLT
            MAT(I)=IXQ(1,I+NFT)
          ENDDO
        ENDIF
c
        DO I=LFT,LLT
          J=I+NFT
          T(J) = GBUF%TEMP(I)
          IF(T(J) <= PM(80,MAT(I)))THEN
            VAL2(J)=PM(75,MAT(I))+PM(76,MAT(I))*T(J)
          ELSE
            VAL2(J)=PM(77,MAT(I))+PM(78,MAT(I))*T(J)
          ENDIF
        ENDDO
c
        IF (MTN == 17)THEN
          DO I=LFT,LLT
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
          CALL M18TH(
     1   GBUF%TEMP,VAL2,     MAT,      PM,
     2   IPM,      TF,       NPF,      NEL)
        ELSEIF (MTN == 26)THEN
          CALL M26TH(
     1   MAT,      GBUF%RHO, GBUF%TEMP,VAL2,
     2   PM,       BUFMAT,   GBUF%RE,  NEL,
     3   NFT)
        ELSEIF (MTN == 51) THEN
          NPH1 = (N0PHAS)*LLT
          NPH2 = (N0PHAS + NVPHAS)*LLT
          NPH3 = (N0PHAS + NVPHAS*2)*LLT
          IADBUF = IPM(7,MAT(1))
          PH1 =>ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(NPH1+1:NPH1+1+LLT)
          PH2 =>ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(NPH2+1:NPH2+1+LLT)
          PH3 =>ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)%VAR(NPH3+1:NPH3+1+LLT)
          CALL M51TH(
     1   T(1+NFT),      PH1,           PH2,           PH3,
     2   BUFMAT(IADBUF),VAL2(1+NFT),   NEL)
        ELSEIF (JTUR /= 0.AND.MTN /= 11) THEN
          DO I=LFT,LLT
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
        IF (NSPMD > 1)
     1    CALL SPMD_EVOIS(T       ,VAL2  ,NERCVOIS,NESDVOIS,LERCVOIS,
     2                    LESDVOIS,LENCOM)
C------------------------------
C     IMPOSED FLUX
C------------------------------
      DO NG=1,NGROUP
C     ALE ON / OFF
        IF (IPARG(76, NG) == 1) CYCLE ! --> OFF
        MTN=IPARG(1,NG)
        IF (MTN /= 11) CYCLE
        JTHE=IPARG(13,NG)
        IF (JTHE /= 1) CYCLE
        LLT=IPARG(2,NG)
        NFT=IPARG(3,NG)
        IAD=IPARG(4,NG)
        LFT=1
        IF(N2D == 0)THEN
          CALL AFIMP3(PM,X,IXS,T,FLUX(6*NFT+1),VAL2,ALE_CONNECT,FV)
        ELSE
          CALL AFIMP2(PM,X,IXQ,T,FLUX(4*NFT+1),VAL2,ALE_CONNECT,FV)
        ENDIF
      ENDDO
C
      DO NG=1,NGROUP
C     ALE ON / OFF
        IF (IPARG(76, NG) == 1) CYCLE ! --> OFF

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
        IF (MTN == 51)THEN
          DO I=LFT,LLT
c            EV(NB11+I-1) = ZERO
            GBUF%TEMP(I) = ZERO
          ENDDO
          IF (N2D == 0) THEN                                            
            CALL ADIFF3(GBUF%TEMP,T,FLUX(6*NFT+1),VAL2,ALE_CONNECT,GBUF%VOL)
          ELSE                                                        
            CALL ADIFF2(GBUF%TEMP,T,FLUX(4*NFT+1),VAL2,ALE_CONNECT,GBUF%VOL)
          ENDIF                                                       
        ELSE
          IF (N2D == 0) THEN
            CALL ADIFF3(GBUF%EINT,T,FLUX(6*NFT+1),VAL2,ALE_CONNECT,GBUF%VOL)
          ELSE
            CALL ADIFF2(GBUF%EINT,T,FLUX(4*NFT+1),VAL2,ALE_CONNECT,GBUF%VOL)
          ENDIF
        ENDIF
      ENDDO!next NG
C-----------
      RETURN
      END
