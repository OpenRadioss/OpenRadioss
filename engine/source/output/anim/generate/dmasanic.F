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
Chd|  DMASANIC                      source/output/anim/generate/dmasanic.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE DMASANIC(ELBUF_TAB,X   ,D    ,GEO  ,IPARG,
     .                    IXQ  ,IXC  ,IXTG ,MAS  ,PM   ,
     .                    EL2FA,NBF  ,IXUR ,IGEO,STACK)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE STACK_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "param_c.inc"
#include      "rnur_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   MAS(*) ,PM(NPROPM,*),GEO(NPROPG,*),X(3,*),
     .   D(3,*)
      INTEGER IPARG(NPARG,*),IXC(NIXC,*),IXTG(NIXTG,*),IXUR(NIXUR,*),
     .        IXQ(NIXQ,*),EL2FA(*),NBF,IGEO(NPROPGI,*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   OFF, P, VONM2, VONM, S1, S2, S12, S3, VALUE,THK0,A0,AL0,
     .   RHO0,XX1,XX2,XX3,YY1,YY2,YY3,ZZ1,ZZ2,ZZ3
      INTEGER I, NG, NEL, NFT, IAD, ITY, LFT, NPT, ISS, ISC,
     .        IADD, N, J, LLT, MLW,
     .        ISTRAIN,NN, K1, K2,JTURB,MT,JALE, IMID, IALEL,IPID,
     .        N1,N2,N3,N4,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,
     .        OFFSET,NEL_OLD,ITY_OLD,NFT_FA,N_FA
      INTEGER ISUBSTACK,NTHK,IADR,IIGEO,IGTYP
      REAL R4
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      NN1 = 1
      NN2 = 1
      NN3 = 1
      NN4 = NN3 + NUMELQ
      NN5 = NN4 + NUMELC
      NN6 = NN5 + NUMELTG
      NN7 = NN6
      NN8 = NN7
      NN9 = NN8
      NN10= NN9 + NUMELUR
C-----------------------------------------------
C
      NEL_OLD = 0
      ITY_OLD = 0
      DO 490 NG=1,NGROUP
        GBUF => ELBUF_TAB(NG)%GBUF
        NEL   =IPARG(2,NG)
        ITY   =IPARG(5,NG)
        NPT   =IABS(IPARG(6,NG))
        IF (IMACH==3.AND.ISPMD==0) THEN
          IF (ITY/=ITY_OLD) THEN
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
        IF (IMACH==3.AND.ISPMD==0) THEN
            NFT_FA = NEL_OLD - NEL
        ELSE
            NFT_FA = NFT
        ENDIF
C-----------------------------------------------
C       QUAD
C-----------------------------------------------
        IF(ITY==2)THEN
         IALEL=(IPARG(7,NG)+IPARG(11,NG))
C
         DO I=LFT,LLT
               N = I + NFT
               N_FA = I + NFT_FA
               IF(IALEL==0)THEN
                 MT=IXQ(1,N)
                 VALUE=PM(89,MT)*GBUF%VOL(I)
               ELSE
                 OFF = MIN(GBUF%OFF(I),ONE)
                 VALUE=GBUF%RHO(I)*GBUF%VOL(I)*OFF
               ENDIF
               MAS(EL2FA(NN3+N_FA)) = VALUE
         ENDDO
C-----------------------------------------------
C       COQUES 4 N
C-----------------------------------------------
        ELSEIF(ITY==3)THEN
C
          ISUBSTACK=IPARG(71,NG)
          IGTYP   = IPARG(38,NG)
          IF(IGTYP /= 17 . AND. IGTYP /= 51 .AND. IGTYP /= 52) THEN
            DO I=LFT,LLT
              N = I + NFT
              N_FA = I + NFT_FA
              RHO0 = PM(1,IXC(1,N))
              THK0 = GEO(1,IXC(6,N))
              N1 = IXC(2,N)
              N2 = IXC(3,N)
              N3 = IXC(4,N)
              N4 = IXC(5,N)
              XX1 = X(1,N3)-D(1,N3)-X(1,N1)+D(1,N1)
              YY1 = X(2,N3)-D(2,N3)-X(2,N1)+D(2,N1)
              ZZ1 = X(3,N3)-D(3,N3)-X(3,N1)+D(3,N1)
              XX2 = X(1,N4)-D(1,N4)-X(1,N2)+D(1,N2)
              YY2 = X(2,N4)-D(2,N4)-X(2,N2)+D(2,N2)
              ZZ2 = X(3,N4)-D(3,N4)-X(3,N2)+D(3,N2)
              XX3 = YY1*ZZ2 - ZZ1*YY2
              YY3 = ZZ1*XX2 - XX1*ZZ2
              ZZ3 = XX1*YY2 - YY1*XX2
              A0  = HALF*SQRT(XX3*XX3 + YY3*YY3 + ZZ3*ZZ3)
              MAS(EL2FA(NN4+N_FA)) = RHO0*THK0*A0
            ENDDO
          ELSE
!!             IIGEO   =  40 + 5*(ISUBSTACK - 1)
!!             IADR    = IGEO(IIGEO + 4,IXC(6,NFT + 1))
!!             NTHK   =  IADR + 3*NPT
            DO I=LFT,LLT
              N = I + NFT
              N_FA = I + NFT_FA
              RHO0 = PM(1,IXC(1,N))
              THK0 = STACK%GEO(1,ISUBSTACK)
              N1 = IXC(2,N)
              N2 = IXC(3,N)
              N3 = IXC(4,N)
              N4 = IXC(5,N)
              XX1 = X(1,N3)-D(1,N3)-X(1,N1)+D(1,N1)
              YY1 = X(2,N3)-D(2,N3)-X(2,N1)+D(2,N1)
              ZZ1 = X(3,N3)-D(3,N3)-X(3,N1)+D(3,N1)
              XX2 = X(1,N4)-D(1,N4)-X(1,N2)+D(1,N2)
              YY2 = X(2,N4)-D(2,N4)-X(2,N2)+D(2,N2)
              ZZ2 = X(3,N4)-D(3,N4)-X(3,N2)+D(3,N2)
              XX3 = YY1*ZZ2 - ZZ1*YY2
              YY3 = ZZ1*XX2 - XX1*ZZ2
              ZZ3 = XX1*YY2 - YY1*XX2
              A0  = HALF*SQRT(XX3*XX3 + YY3*YY3 + ZZ3*ZZ3)
              MAS(EL2FA(NN4+N_FA)) = RHO0*THK0*A0
            ENDDO
          ENDIF
C-----------------------------------------------
C       COQUES 3 N
C-----------------------------------------------
        ELSEIF(ITY==7)THEN
C       
          ISUBSTACK=IPARG(71,NG)
          IGTYP   = IPARG(38,NG)
          IF(IGTYP /= 17 . AND. IGTYP /= 51 .AND. IGTYP /= 52) THEN
          DO I=LFT,LLT
            N = I + NFT
            N_FA = I + NFT_FA
            RHO0 = PM(1,IXTG(1,N))
            THK0 = GEO(1,IXTG(5,N))
            N1 = IXTG(2,N)
            N2 = IXTG(3,N)
            N3 = IXTG(4,N)
            XX1 = X(1,N2)-D(1,N2)-X(1,N1)+D(1,N1)
            YY1 = X(2,N2)-D(2,N2)-X(2,N1)+D(2,N1)
            ZZ1 = X(3,N2)-D(3,N2)-X(3,N1)+D(3,N1)
            XX2 = X(1,N3)-D(1,N3)-X(1,N1)+D(1,N1)
            YY2 = X(2,N3)-D(2,N3)-X(2,N1)+D(2,N1)
            ZZ2 = X(3,N3)-D(3,N3)-X(3,N1)+D(3,N1)
            XX3 = YY1*ZZ2 - ZZ1*YY2
            YY3 = ZZ1*XX2 - XX1*ZZ2
            ZZ3 = XX1*YY2 - YY1*XX2
            A0  = HALF*SQRT(XX3*XX3 + YY3*YY3 + ZZ3*ZZ3)
            MAS(EL2FA(NN5+N_FA)) = RHO0*THK0*A0
          ENDDO
          ELSE
              N = 1 + NFT
!!             IIGEO   =  40 + 5*(ISUBSTACK - 1)
!!             IADR    = IGEO(IIGEO + 4,IXC(6,N))
!!             NTHK   =  IADR + 3*NPT
             DO I=LFT,LLT
              N = I + NFT
              N_FA = I + NFT_FA
              RHO0 = PM(1,IXTG(1,N))
              THK0 = STACK%GEO(1,ISUBSTACK)
              N1 = IXTG(2,N)
              N2 = IXTG(3,N)
              N3 = IXTG(4,N)
              XX1 = X(1,N2)-D(1,N2)-X(1,N1)+D(1,N1)
              YY1 = X(2,N2)-D(2,N2)-X(2,N1)+D(2,N1)
              ZZ1 = X(3,N2)-D(3,N2)-X(3,N1)+D(3,N1)
              XX2 = X(1,N3)-D(1,N3)-X(1,N1)+D(1,N1)
              YY2 = X(2,N3)-D(2,N3)-X(2,N1)+D(2,N1)
              ZZ2 = X(3,N3)-D(3,N3)-X(3,N1)+D(3,N1)
              XX3 = YY1*ZZ2 - ZZ1*YY2
              YY3 = ZZ1*XX2 - XX1*ZZ2
              ZZ3 = XX1*YY2 - YY1*XX2
              A0  = HALF*SQRT(XX3*XX3 + YY3*YY3 + ZZ3*ZZ3)
              MAS(EL2FA(NN5+N_FA)) = RHO0*THK0*A0
             ENDDO
          ENDIF
C
C-----------------------------------------------
C       RNUR
C-----------------------------------------------
        ELSEIF(ITY==50)THEN
         DO I=LFT,LLT
            N = I + NFT
            N_FA = I + NFT_FA
            MAS(EL2FA(NN9+N_FA)) = GEO(21,IXUR(1,N))
         ENDDO
C
        ELSE
        ENDIF
C-----------------------------------------------
C       FIN DE BOUCLE SUR LES OFFSET
C-----------------------------------------------
 490  CONTINUE
C-----------------------------------------------
C
      RETURN
      END
