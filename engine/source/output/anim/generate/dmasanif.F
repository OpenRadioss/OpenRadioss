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
Chd|  DMASANIF                      source/output/anim/generate/dmasanif.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DMASANIF(ELBUF_TAB,X   ,D    ,GEO  ,IPARG,
     .                    IXT     ,IXP  ,IXR  ,MAS  ,PM   ,
     .                    EL2FA   ,NBF  )
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
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   MAS(*) ,PM(NPROPM,*),GEO(NPROPG,*),X(3,*),
     .   D(3,*)
      INTEGER IPARG(NPARG,*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),EL2FA(*),NBF
C
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   OFF, P, VONM2, VONM, S1, S2, S12, S3, VALUE,THK0,A0,AL0,
     .   RHO0,XX1,XX2,XX3,YY1,YY2,YY3,ZZ1,ZZ2,ZZ3
      INTEGER I, NG, NEL, NFT, IAD, ITY, LFT, NPT, ISS, ISC,
     .        IADD, N, J, LLT, MLW, NB1, NB2, NB3, NB4, NB5,NB6, 
     .        NB7, NB8, NB9, NB10, NB11, NB12, NB13, NB14, NB15, NB16,
     .        ISTRAIN,NN, K1, K2,JTURB,MT,JALE, IMID, IALEL,IPID,
     .        N1,N2,N3,N4,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,
     .        OFFSET,NEL_OLD,ITY_OLD,NFT_FA,N_FA,
     .        NUVAR
      REAL R4
C
      TYPE(G_BUFEL_) ,POINTER :: GBUF
C-----------------------------------------------
      NN1 = 1
      NN2 = 1
      NN3 = 1
      NN4 = NN3 
      NN5 = NN4 
      NN6 = NN5 
      NN7 = NN6 + NUMELT
      NN8 = NN7 + NUMELP
      NN9 = NN8 + NUMELR
      NN10= NN9
C-----------------------------------------------
      NEL_OLD = 0
      ITY_OLD = 0
      DO NG=1,NGROUP
        MLW   =IPARG(1,NG)
        NEL   =IPARG(2,NG)
        ITY   =IPARG(5,NG)
        GBUF => ELBUF_TAB(NG)%GBUF
        IF (IMACH == 3 .AND. ISPMD == 0) THEN
          IF (ITY /= ITY_OLD) THEN
            NEL_OLD = 0
            ITY_OLD= ITY
          ENDIF
          NFT_FA = NEL_OLD
          NEL_OLD = NEL_OLD + NEL
        ENDIF
        NFT   =IPARG(3,NG)
        IAD   =IPARG(4,NG)
        LFT=1
        LLT=NEL
        IF (IMACH == 3 .AND. ISPMD == 0) THEN
          NFT_FA = NEL_OLD - NEL 
        ELSE
          NFT_FA = NFT
        ENDIF
C-----------------------------------------------
C       TRUSS
C-----------------------------------------------
        IF (ITY == 4) THEN
          DO I=LFT,LLT
            N = I + NFT
            N_FA = I + NFT_FA
            RHO0 = PM(1,IXT(1,N))
            A0 = GEO(1,IXT(4,N))
            N1 = IXT(2,N)
            N2 = IXT(3,N)
            XX1 = X(1,N2)-D(1,N2)-X(1,N1)+D(1,N1)
            YY1 = X(2,N2)-D(2,N2)-X(2,N1)+D(2,N1)
            ZZ1 = X(3,N2)-D(3,N2)-X(3,N1)+D(3,N1)
            AL0  = HALF*SQRT(XX1*XX1 + YY1*YY1 + ZZ1*ZZ1)
            MAS(EL2FA(NN6+N_FA)) = RHO0*AL0*A0
          ENDDO
C-----------------------------------------------
C       POUTRES
C-----------------------------------------------
        ELSEIF (ITY == 5) THEN
          DO I=LFT,LLT
            N = I + NFT
            N_FA = I + NFT_FA
            RHO0 = PM(1,IXP(1,N))
            A0 = GEO(1,IXP(5,N))
            N1 = IXP(2,N)
            N2 = IXP(3,N)
            XX1 = X(1,N2)-D(1,N2)-X(1,N1)+D(1,N1)
            YY1 = X(2,N2)-D(2,N2)-X(2,N1)+D(2,N1)
            ZZ1 = X(3,N2)-D(3,N2)-X(3,N1)+D(3,N1)
            AL0  = HALF*SQRT(XX1*XX1 + YY1*YY1 + ZZ1*ZZ1)
            MAS(EL2FA(NN7+N_FA)) = RHO0*AL0*A0
          ENDDO
C-----------------------------------------------
C       RESSORTS
C-----------------------------------------------
        ELSEIF (ITY == 6) THEN
          IF(MLW==3)THEN
            DO I=LFT,LLT
              N = I + NFT
              N_FA = I + NFT_FA
              MAS(EL2FA(NN8+N_FA))   = HALF*GEO(1,IXR(1,N))
              MAS(EL2FA(NN8+N_FA)+1) = HALF*GEO(1,IXR(1,N))
            ENDDO
          ELSEIF (MLW == 5) THEN
c           NB1 =IAD - 1
c           NB2 =NB1  + NEL
c           NUVAR =  NINT(GEO(25,IXR(1,1+NFT)))
c           NB3 =NB2  + 3*NEL
c           NB4 =NB3  + NEL
c           NB5 =NB4  + 3*NEL
c           NB6 =NB5  
c           NB7 =NB6  
c           NB8 =NB7  
c           NB9 =NB8  + 3*NEL
c           NB10=NB9  + 3*NEL
c           NB11=NB10 
c           NB12=NB11 
c           NB13=NB12 
c           NB14=NB13 
c           NB15 = NB14 + 3*NEL
c           NB16 = NB15 + NUVAR*NEL
            DO I=LFT,LLT
              N = I + NFT
              N_FA = I + NFT_FA
              MAS(EL2FA(NN8+N_FA)) = GBUF%MASS(I)
cc            MAS(EL2FA(NN8+N_FA)) = BUFEL(NB16+I)
            ENDDO
          ELSE
            DO I=LFT,LLT
              N = I + NFT
              N_FA = I + NFT_FA
              MAS(EL2FA(NN8+N_FA)) = GEO(1,IXR(1,N))
            ENDDO
          ENDIF ! IF(MLW)
        ENDIF ! IF (ITY)
C-----------------------------------------------
C       FIN DE BOUCLE SUR LES OFFSET
C-----------------------------------------------
      ENDDO
C-----------------------------------------------
      RETURN
      END
