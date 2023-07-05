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
Chd|  OUTP_R_T                      source/output/sty/outp_r_t.F  
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|-- calls ---------------
Chd|        SPMD_RGATHER9_1COMM           source/mpi/interfaces/spmd_outp.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE OUTP_R_T(KEY   ,TEXT ,ELBUF_TAB,IPARG ,DD_IAD,
     2                    IXR   ,IGEO ,GEO      ,SIZLOC,SIZP0 ,
     3                    SIZ_WR)
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
#include      "param_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*8 KEY
      CHARACTER*40 TEXT
      INTEGER NBX
      INTEGER IPARG(NPARG,*), DD_IAD(NSPMD+1,*),
     .  IXR(NIXR,*),IGEO(NPROPGI,*),SIZLOC,SIZP0,SIZ_WR
      my_real
     .   GEO(NPROPG,*)
C
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,JJ,ICAS_OLD,K
      INTEGER NG, NEL, NFT, ITY, LFT,IADD, LLT,N,MLW,
     .        JJ_OLD, NGF, NGL, NN, LEN, NUVAR,
     .        LIAD, IUS, MLW2,NAD,
     .        MT,IPROP,IGTYP,COMPTEUR,L,II(6),IV
      INTEGER, DIMENSION(NSPGROUP) :: JJ_LOC
      INTEGER, DIMENSION(NSPGROUP+1,NSPMD) :: ADRESS 
      my_real
     .   WA(SIZLOC),WAP0(SIZ_WR),WAP0_LOC(SIZP0)
C
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
C-----------------------------------------------
      IF (ISPMD == 0) THEN
        WRITE(IUGEO,'(2A)')'/SPRING    /',KEY
        WRITE(IUGEO,'(A)') TEXT
      ENDIF
C 
      JJ_OLD = 1 
      NGF = 1
      NGL = 0
      JJ = 0
      COMPTEUR = 0
      DO NN=1,NSPGROUP
        NGL = NGL + DD_IAD(ISPMD+1,NN)
        DO NG=NGF,NGL
          ITY = IPARG(5,NG)
          NFT = IPARG(3,NG)
          GBUF => ELBUF_TAB(NG)%GBUF
C------------  
          IF (ITY == 6) THEN
            IPROP = IXR(1,NFT+1)
            IGTYP = IGEO(11,IPROP)
            NEL   = IPARG(2,NG)
            NFT   = IPARG(3,NG)
C------------     
            IF (IGTYP == 4) THEN
              WA(JJ + 1) = IGTYP 
              WA(JJ + 2) = NEL
              WA(JJ + 3) = 0
              JJ = JJ + 3
              DO I=1,NEL
                WA(JJ + 1) = GBUF%FOR(I)
                WA(JJ + 2) = GBUF%TOTDEPL(I)
                WA(JJ + 3) = GBUF%FOREP(I)
                WA(JJ + 4) = GBUF%DEP_IN_TENS(I)
                WA(JJ + 5) = GBUF%DEP_IN_COMP(I)
                WA(JJ + 6) = GBUF%LENGTH(I)
                WA(JJ + 7) = GBUF%EINT(I)
                JJ = JJ + 7
              ENDDO
C------------  
            ELSEIF (IGTYP == 12) THEN
              WA(JJ + 1) = IGTYP 
              WA(JJ + 2) = NEL
              WA(JJ + 3) = 0
              JJ = JJ + 3
              DO I=1,NEL
                WA(JJ + 1) = GBUF%FOR(I)
                WA(JJ + 2) = GBUF%TOTDEPL(I)
                WA(JJ + 3) = GBUF%FOREP(I)
                WA(JJ + 4) = GBUF%DEP_IN_TENS(I)
                WA(JJ + 5) = GBUF%DEP_IN_COMP(I)
                WA(JJ + 6) = GBUF%LENGTH(I)
                WA(JJ + 7) = GBUF%EINT(I)
                WA(JJ + 8) = GBUF%DFS(I)
                JJ = JJ + 8
              ENDDO
C------------ 
            ELSEIF (IGTYP == 8 .OR. IGTYP == 13 .OR. IGTYP == 25 
     .                                          .OR. IGTYP == 23) THEN
              WA(JJ + 1) = IGTYP
              WA(JJ + 2) = NEL
              WA(JJ + 3) = 0 
              JJ = JJ + 3
              DO J=1,6
                II(J) = (J-1)*NEL + 1
              ENDDO
              DO I=1,NEL
                DO J=1,3
                  WA(JJ + (J-1)*5 + 1) = GBUF%FOR(II(J) + I - 1)
                  WA(JJ + (J-1)*5 + 2) = GBUF%TOTDEPL(II(J) + I - 1)
                  WA(JJ + (J-1)*5 + 3) = GBUF%FOREP(II(J) + I - 1)
                  WA(JJ + (J-1)*5 + 4) = GBUF%DEP_IN_TENS(II(J) + I - 1)
                  WA(JJ + (J-1)*5 + 5) = GBUF%DEP_IN_COMP(II(J) + I - 1)
                  WA(JJ + (J-1)*5 + 16)= GBUF%MOM(II(J) + I - 1)
                  WA(JJ + (J-1)*5 + 17)= GBUF%TOTROT(II(J) + I - 1)
                  WA(JJ + (J-1)*5 + 18)= GBUF%MOMEP(II(J) + I - 1)
                  WA(JJ + (J-1)*5 + 19)= GBUF%ROT_IN_TENS(II(J) + I - 1)
                  WA(JJ + (J-1)*5 + 20)= GBUF%ROT_IN_COMP(II(J) + I - 1)
                  WA(JJ + J + 30)      = GBUF%LENGTH(II(J) + I - 1)
                ENDDO
                WA(JJ + 34) = GBUF%EINT(I)
c
                DO J=1,6
                  WA(JJ + J + 34) = GBUF%E6(II(J) + I - 1) ! E6(1:6)
                ENDDO
                JJ = JJ + 40
c
              ENDDO
C------------  
            ELSEIF (IGTYP == 26) THEN
              WA(JJ + 1) = IGTYP 
              WA(JJ + 2) = NEL
              WA(JJ + 3) = 0
              JJ = JJ + 3
              DO I=1,NEL
                WA(JJ + 1) = GBUF%FOR(I)
                WA(JJ + 2) = GBUF%TOTDEPL(I)
                WA(JJ + 3) = GBUF%FOREP(I)
                WA(JJ + 4) = GBUF%LENGTH(I)
                WA(JJ + 5) = GBUF%EINT(I)
                WA(JJ + 6) = GBUF%DV(I)
                JJ = JJ + 6
              ENDDO
C------------  
            ELSEIF (IGTYP == 29 .OR. IGTYP == 30 .OR. IGTYP == 31 .OR.
     .              IGTYP == 32 .OR. IGTYP == 33 .OR. IGTYP == 35 .OR.
     .              IGTYP == 36 .OR. IGTYP == 44 .OR. IGTYP == 45 .OR.
     .              IGTYP == 46) THEN
              NUVAR = NINT(GEO(25,IPROP))         
              WA(JJ +1) = IGTYP
              WA(JJ +2) = NEL
              WA(JJ +3) = NUVAR
              JJ = JJ + 3
              DO I=1,NEL
                DO J=1,3
                  II(J) = (J-1)*NEL + 1
                  WA(JJ + (J-1)*2 + 1) = GBUF%FOR(II(J) + I - 1)
                  WA(JJ + (J-1)*2 + 2) = GBUF%V_REPCVT(II(J) + I - 1)
                  WA(JJ + (J-1)*2 + 7) = GBUF%MOM(II(J) + I - 1)
                  WA(JJ + (J-1)*2 + 8) = GBUF%VR_REPCVT(II(J) + I - 1)
                ENDDO
                WA(JJ + 13) = GBUF%EINT(I)
                JJ = JJ + 13
c
!!                IF (IGTYP /= 32 .AND. IGTYP /= 33 .AND. IGTYP /= 45) THEN
!!                  WA(JJ + 1) = GBUF%MOM(II(4) + I - 1)
!!                  WA(JJ + 2) = GBUF%MOM(II(5) + I - 1)
!!                  JJ = JJ + 2
!!                ENDIF
c
                DO J=1,NUVAR
                  IV = NUVAR*(I-1) + J
                  WA(JJ + J) = GBUF%VAR(IV)
                ENDDO
                JJ = JJ + NUVAR               
              ENDDO ! DO I=1,NEL
            ENDIF ! IF (IGTYP)
C------------ 
          ENDIF ! IF (ITY)
        ENDDO ! DO NG=NGF,NGL
        NGF = NGL + 1
        JJ_LOC(NN) = JJ - COMPTEUR            ! size of each group
        COMPTEUR = JJ                         
      ENDDO ! DO NN=1,NSPGROUP                ! nn=1,nspdgroup
!---
!     ++++++++++
!---
      IF (NSPMD > 1) THEN
        CALL SPMD_RGATHER9_1COMM(WA,JJ,JJ_LOC,WAP0_LOC,SIZP0,ADRESS)
      ELSE
        WAP0_LOC(1:JJ) = WA(1:JJ)
        ADRESS(1,1) = 1
        DO NN = 2,NSPGROUP+1
          ADRESS(NN,1) = JJ_LOC(NN-1) + ADRESS(NN-1,1)
        ENDDO
      ENDIF
!---
!     ++++++++++
!---
      IF (ISPMD == 0) THEN
        DO NN=1,NSPGROUP
          COMPTEUR = 0
          DO K = 1,NSPMD
            IF ((ADRESS(NN+1,K)-1-ADRESS(NN,K)) > =0) THEN
              DO L = ADRESS(NN,K),ADRESS(NN+1,K)-1
                COMPTEUR = COMPTEUR + 1
                WAP0(COMPTEUR) = WAP0_LOC(L)
              ENDDO  ! l=... , ...
            ENDIF   !if(size_loc>0)
          ENDDO    ! k=1,nspmd
C
          JJ_OLD = COMPTEUR
          IF (JJ_OLD > 0) THEN
            ICAS_OLD = 0
            J = 1
            DO WHILE (J < JJ_OLD+1)
              IGTYP = NINT(WAP0(J))
              NEL   = NINT(WAP0(J+1))
              NUVAR = NINT(WAP0(J+2))
              J = J + 3
C--------------------------------------
              IF (IGTYP == 4) THEN
C--------------------------------------
                IF (ICAS_OLD /= 1) THEN
                  ICAS_OLD = 1

                  IF (OUTYY_FMT == 2) THEN
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P5E12.5) #(F(I),DL(I),FEP(I),DPL(I),DPL2(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P2E12.5) #(XLO(I),EI(I),I=1,NEL)'
                  ELSE
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P5E20.13) #(F(I),DL(I),FEP(I),DPL(I),DPL2(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P2E20.13) #(XLO(I),EI(I),I=1,NEL)'
                  ENDIF 
                ENDIF ! IF (ICAS_OLD /= 1)
                IF (OUTYY_FMT == 2) THEN
                  WRITE(IUGEO,'(2I8)')IGTYP,NEL 
                ELSE
                  WRITE(IUGEO,'(2I10)')IGTYP,NEL           
                ENDIF
                DO I=1,NEL
                  IF (OUTYY_FMT == 2) THEN
!!                    WRITE(IUGEO,'(1P5E12.5)') (WAP0(J-1+K),K=1,7)
                    WRITE(IUGEO,'(1P5E12.5)') (WAP0(J-1+K),K=1,5)
                    WRITE(IUGEO,'(1P2E12.5)') (WAP0(J-1+K),K=6,7)
                  ELSE
!!                    WRITE(IUGEO,'(1P5E20.13)')(WAP0(J-1+K),K=1,7)
                    WRITE(IUGEO,'(1P5E20.13)')(WAP0(J-1+K),K=1,5)
                    WRITE(IUGEO,'(1P2E20.13)')(WAP0(J-1+K),K=6,7)
                  ENDIF
                  J = J + 7
                ENDDO ! DO I=1,NEL
C--------------------------------------
              ELSEIF (IGTYP == 12) THEN
C--------------------------------------
                IF (ICAS_OLD /= 2) THEN
                  ICAS_OLD = 2
                  IF (OUTYY_FMT == 2) THEN
                    WRITE(IUGEO,'(A)') '#FORMAT:(1P5E12.5) #(F(I),DL(I),FEP(I),DPL(I),DPL2(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)') '#FORMAT:(1P3E12.5) #(XL0(I),EI(I),DFS(I),I=1,NEL)'
                  ELSE
                    WRITE(IUGEO,'(A)') '#FORMAT:(1P5E20.13) #(F(I),DL(I),FEP(I),DPL(I),DPL2(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)') '#FORMAT:(1P3E20.13) #(XL0(I),EI(I),DFS(I),I=1,NEL)'
                  ENDIF ! IF (OUTYY_FMT == 2)
                ENDIF ! IF (ICAS_OLD /= 2)
                IF (OUTYY_FMT == 2) THEN
                  WRITE(IUGEO,'(2I8)')IGTYP,NEL 
                ELSE
                  WRITE(IUGEO,'(2I10)')IGTYP,NEL           
                ENDIF
                DO I=1,NEL
                  IF (OUTYY_FMT == 2) THEN
!!                    WRITE(IUGEO,'(1P5E12.5)') (WAP0(J-1+K),K=1,8)
                    WRITE(IUGEO,'(1P5E12.5)') (WAP0(J-1+K),K=1,5)
                    WRITE(IUGEO,'(1P3E12.5)') (WAP0(J-1+K),K=6,8)
                  ELSE
!!                    WRITE(IUGEO,'(1P5E20.13)')(WAP0(J-1+K),K=1,8)         
                    WRITE(IUGEO,'(1P5E20.13)')(WAP0(J-1+K),K=1,5) 
                    WRITE(IUGEO,'(1P3E20.13)')(WAP0(J-1+K),K=6,8) 
                  ENDIF
                  J = J + 8
                ENDDO ! DO I=1,NEL
C--------------------------------------
              ELSEIF (IGTYP == 8 .OR. IGTYP == 13 .OR. IGTYP == 25 
     .                                            .OR. IGTYP == 23 ) THEN
C--------------------------------------
                IF (ICAS_OLD /= 3) THEN
                  ICAS_OLD = 3
                  IF (OUTYY_FMT == 2) THEN
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P5E12.5) #(FX(I),DX(I),FXEP(I),DPX(I),DPX2(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P5E12.5) #(FY(I),DY(I),FYEP(I),DPY(I),DPY2(I),I=1,NEL)' 
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P5E12.5) #(FZ(I),DZ(I),FZEP(I),DPZ(I),DPZ2(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P5E12.5) #(XMOM(I),RX(I),XMEP(I),RPX(I),RPX2(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P5E12.5) #(YMOM(I),RY(I),YMEP(I),RPY(I),RPY2(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P5E12.5) #(ZMOM(I),RZ(I),ZMEP(I),RPZ(I),RPZ2(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P5E12.5) #(XLO(I),YL0(I),ZL0(I),EI(I),E1(I),I=1,NEL)'
                    WRITE(IUGEO,'(2A)')'#FORMAT:(1P5E12.5) #(E2(I),E3(I),E4(I),E5(I),E6(I),I=1,NEL)'
                  ELSE 
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P5E20.13) #(FX(I),DX(I),FXEP(I),DPX(I),DPX2(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P5E20.13) #(FY(I),DY(I),FYEP(I),DPY(I),DPY2(I),I=1,NEL)' 
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P5E20.13) #(FZ(I),DZ(I),FZEP(I),DPZ(I),DPZ2(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P5E20.13) #(XMOM(I),RX(I),XMEP(I),RPX(I),RPX2(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P5E20.13) #(YMOM(I),RY(I),YMEP(I),RPY(I),RPY2(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P5E20.13) #(ZMOM(I),RZ(I),ZMEP(I),RPZ(I),RPZ2(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P5E20.13) #(XLO(I),YL0(I),ZL0(I),EI(I),E1(I),I=1,NEL)'
                    WRITE(IUGEO,'(2A)')'#FORMAT:(1P5E20.13) #(E2(I),E3(I),E4(I),E5(I),E6(I),I=1,NEL)'
                  ENDIF ! IF (OUTYY_FMT == 2)
                ENDIF ! IF (ICAS_OLD /= 3)
                IF (OUTYY_FMT == 2) THEN
                  WRITE(IUGEO,'(2I8)')IGTYP,NEL 
                ELSE
                  WRITE(IUGEO,'(2I10)')IGTYP,NEL          
                ENDIF
                DO I=1,NEL
                  IF (OUTYY_FMT == 2) THEN         
                    WRITE(IUGEO,'(1P5E12.5)') (WAP0(J-1+K),K=1,40)
                  ELSE
                    WRITE(IUGEO,'(1P5E20.13)')(WAP0(J-1+K),K=1,40)           
                  ENDIF
                  J = J + 40
                ENDDO ! DO I=1,NEL
C--------------------------------------
              ELSEIF (IGTYP == 26) THEN
C--------------------------------------
                IF (ICAS_OLD /= 2) THEN
                  ICAS_OLD = 2
                  IF (OUTYY_FMT == 2) THEN
                    WRITE(IUGEO,'(A)') '#FORMAT:(1P3E12.5) #(F(I),DL(I),FEP(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)') '#FORMAT:(1P3E12.5) #(XL0(I),EI(I),DV(I),I=1,NEL)'
                  ELSE
                    WRITE(IUGEO,'(A)') '#FORMAT:(1P3E20.13) #(F(I),DL(I),FEP(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)') '#FORMAT:(1P3E20.13) #(XL0(I),EI(I),DV(I),I=1,NEL)'
                  ENDIF ! IF (OUTYY_FMT == 2)
                ENDIF ! IF (ICAS_OLD /= 2)
                IF (OUTYY_FMT == 2) THEN
                  WRITE(IUGEO,'(2I8)') IGTYP,NEL 
                ELSE
                  WRITE(IUGEO,'(2I10)')IGTYP,NEL           
                ENDIF
                DO I=1,NEL
                  IF (OUTYY_FMT == 2) THEN
                    WRITE(IUGEO,'(1P3E12.5)') (WAP0(J-1+K),K=1,3)
                    WRITE(IUGEO,'(1P3E12.5)') (WAP0(J-1+K),K=4,6)
                  ELSE
                    WRITE(IUGEO,'(1P3E20.13)')(WAP0(J-1+K),K=1,3)
                    WRITE(IUGEO,'(1P3E20.13)')(WAP0(J-1+K),K=4,6)
                  ENDIF
                  J = J + 6
                ENDDO ! DO I=1,NEL
C--------------------------------------
              ELSEIF (IGTYP == 29 .OR. IGTYP == 30 .OR. IGTYP == 31 .OR.
     .                IGTYP == 32 .OR. IGTYP == 33 .OR. IGTYP == 35 .OR.
     .                IGTYP == 36 .OR. IGTYP == 44 .OR. IGTYP == 45 .OR.
     .                IGTYP == 46) THEN
C--------------------------------------
                IF (ICAS_OLD /= 4) THEN
                  ICAS_OLD = 4
                  IF (OUTYY_FMT == 2) THEN
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P2E12.5) #(FX(I),DX(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P2E12.5) #(FY(I),DY(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P2E12.5) #(FZ(I),DZ(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P2E12.5) #(XMOM(I),RX(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P2E12.5) #(YMOM(I),RY(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P2E12.5) #(ZMOM(I),RZ(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P1E12.5) #(EI(I),I=1,NEL)'
!!                    IF (IGTYP /= 32 .AND. IGTYP /= 33 .AND. IGTYP /= 45) THEN
!!                      WRITE(IUGEO,'(A)')'#FORMAT:(1P2E12.5)
!!     .        #(MOM1Y(I),MOM1Z(I),I=1,NEL)'
!!                    ENDIF
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P5E12.5) #(UVAR(I,J),J=1,NUVAR),I=1,NEL)'
                  ELSE 
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P2E20.13) #(FX(I),DX(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P2E20.13) #(FY(I),DY(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P2E20.13) #(FZ(I),DZ(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P2E20.13) #(XMOM(I),RX(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P2E20.13) #(YMOM(I),RY(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P2E20.13) #(ZMOM(I),RZ(I),I=1,NEL)'
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P1E20.13) #(EI(I),I=1,NEL)'
!!                    IF (IGTYP /= 32 .AND. IGTYP /= 33 .AND. IGTYP /= 45) THEN
!!                      WRITE(IUGEO,'(A)')'#FORMAT:(1P2E20.13)
!!     .        #(MOM1Y(I),MOM1Z(I),I=1,NEL)'
!!                    ENDIF
                    WRITE(IUGEO,'(A)')'#FORMAT:(1P5E20.13) #(UVAR(I,J),J=1,NUVAR),I=1,NEL)'
                  ENDIF ! IF (OUTYY_FMT == 2)
                ENDIF ! IF (ICAS_OLD /= 4)
                IF (OUTYY_FMT == 2) THEN
                  WRITE(IUGEO,'(3I8)') IGTYP,NEL,NUVAR 
                ELSE
                  WRITE(IUGEO,'(3I10)')IGTYP,NEL,NUVAR        
                ENDIF
                DO I=1,NEL
                  IF (OUTYY_FMT == 2) THEN
!!                    WRITE(IUGEO,'(1P5E12.5)') (WAP0(J-1+K),K=1,12)
                    WRITE(IUGEO,'(1P2E12.5)') (WAP0(J-1+K),K=1,2)
                    WRITE(IUGEO,'(1P2E12.5)') (WAP0(J-1+K),K=3,4)
                    WRITE(IUGEO,'(1P2E12.5)') (WAP0(J-1+K),K=5,6)
                    WRITE(IUGEO,'(1P2E12.5)') (WAP0(J-1+K),K=7,8)
                    WRITE(IUGEO,'(1P2E12.5)') (WAP0(J-1+K),K=9,10)
                    WRITE(IUGEO,'(1P2E12.5)') (WAP0(J-1+K),K=11,12)
                    WRITE(IUGEO,'(1P1E12.5)')  WAP0(J-1+13)  ! EINT
                  ELSE
!!                    WRITE(IUGEO,'(1P5E20.13)') (WAP0(J-1+K),K=1,12)
                    WRITE(IUGEO,'(1P2E20.13)') (WAP0(J-1+K),K=1,2)
                    WRITE(IUGEO,'(1P2E20.13)') (WAP0(J-1+K),K=3,4)
                    WRITE(IUGEO,'(1P2E20.13)') (WAP0(J-1+K),K=5,6)
                    WRITE(IUGEO,'(1P2E20.13)') (WAP0(J-1+K),K=7,8)
                    WRITE(IUGEO,'(1P2E20.13)') (WAP0(J-1+K),K=9,10)
                    WRITE(IUGEO,'(1P2E20.13)') (WAP0(J-1+K),K=11,12)
                    WRITE(IUGEO,'(1P1E20.13)')  WAP0(J-1+13)  ! EINT
                  ENDIF
                  J = J + 13
c
!!                  IF (IGTYP /= 32 .AND. IGTYP /= 33 .AND. IGTYP /= 45) THEN
!!                    IF (OUTYY_FMT == 2) THEN
!!                      WRITE(IUGEO,'(1P2E12.5)')  (WAP0(J-1+K),K=1,2)
!!                    ELSE
!!                      WRITE(IUGEO,'(1P2E20.13)') (WAP0(J-1+K),K=1,2) ! MOM1Y, MOM1Z
!!                    ENDIF
!!                  ENDIF
c
                  IF (OUTYY_FMT == 2) THEN
                    WRITE(IUGEO,'(1P5E12.5)') (WAP0(J-1+K),K=1,NUVAR)
                  ELSE
                    WRITE(IUGEO,'(1P5E20.13)')(WAP0(J-1+K),K=1,NUVAR)           
                  ENDIF
                  J = J + NUVAR
                ENDDO ! DO I=1,NEL
              ENDIF ! igtyp          
            ENDDO  ! do while
          ENDIF   ! jj_old>0
        ENDDO   ! nn=1,nspgroup
      ENDIF   ! ispmd=0
C---
      RETURN
      END
Chd|====================================================================
Chd|  COUNT_ARSZ_RT                 source/output/sty/outp_r_t.F  
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|        OUTP_ARSZ_RT                  source/mpi/interfaces/spmd_outp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE COUNT_ARSZ_RT(IPARG,IGEO,GEO,IXR,DD_IAD,WASZ,SIZ_WRITE_LOC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "task_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IPARG(NPARG,*),DD_IAD(NSPMD+1,*),WASZ,WASZP,
     .           IXR(NIXR,*),IGEO(NPROPGI,*),SIZ_WRITE_LOC(NSPGROUP+1)
       my_real 
     .   GEO(NPROPG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGF,NGL,NN,ITY,NEL,NG,JJ,
     .        P0ARSZ2,WASZ2,IPROP,NUVAR,IGTYP,NFT,I
C-----------------------------------------------
      WASZP = 0
      WASZ = 0
      IF (OUTP_RS(2) == 1) THEN
        NGF = 1
        NGL = 0
        DO NN=1,NSPGROUP
          JJ = 0
          NGL = NGL + DD_IAD(ISPMD+1,NN)
          DO NG = NGF,NGL
            ITY = IPARG(5,NG)
            NFT = IPARG(3,NG)
            IF (ITY == 6) THEN
              NEL = IPARG(2,NG)
              IPROP = IXR(1,NFT+1)
              IGTYP = IGEO(11,IPROP)
              JJ = JJ + 3
              IF (IGTYP == 4) THEN
                JJ = JJ + 7*NEL
              ELSEIF (IGTYP == 12) THEN
                JJ = JJ + 8*NEL
              ELSEIF (IGTYP == 8 .OR. IGTYP == 13 .OR. IGTYP == 25 
     .                                            .OR. IGTYP == 23 ) THEN             
                JJ = JJ + 40*NEL
              ELSEIF (IGTYP == 26) THEN             
                JJ = JJ + 6*NEL
              ELSEIF (IGTYP == 29 .OR. IGTYP == 30 .OR. IGTYP == 31 .OR.
     .                IGTYP == 32 .OR. IGTYP == 33 .OR. IGTYP == 35 .OR.
     .                IGTYP == 36 .OR. IGTYP == 44 .OR. IGTYP == 45 .OR.
     .                IGTYP == 46) THEN
                JJ = JJ + 13*NEL
!!                IF (IGTYP /= 32 .AND. IGTYP /= 33 .AND. IGTYP /= 45) THEN
!!                  JJ = JJ + 2    !     MOM1Y, MOM1Z
!!                ENDIF
                NUVAR = NINT(GEO(25,IPROP))         
                JJ = JJ + NUVAR*NEL             
              ENDIF ! IF(IGTYP)
            ENDIF ! IF (ITY)
          ENDDO ! DO NG = NGF,NGL 
          NGF = NGL + 1
          WASZ = WASZ + JJ
          SIZ_WRITE_LOC(NN) = JJ
        ENDDO ! DO NN=1,NSPGROUP
        SIZ_WRITE_LOC(NSPGROUP+1) = WASZ
      ENDIF ! 
C---
      RETURN
      END
