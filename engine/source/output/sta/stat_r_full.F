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
Chd|  STAT_R_FULL                   source/output/sta/stat_r_full.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        SPMD_RGATHER9_DP              source/mpi/interfaces/spmd_outp.F
Chd|        SPMD_STAT_PGATHER             source/mpi/output/spmd_stat.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE STAT_R_FULL(
     1                  ELBUF_TAB ,IPARG ,GEO    ,IGEO        ,IXR       ,
     2                  WA        ,WAP0  ,IPARTR ,IPART_STATE ,STAT_INDXR,
     3                  SIZP0     )
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
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SIZLOC,SIZP0
      INTEGER IXR(NIXR,*),IPARG(NPARG,*),IGEO(NPROPGI,*),
     .        IPARTR(*),IPART_STATE(*),STAT_INDXR(*)
      my_real   
     .   GEO(NPROPG,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      double precision WA(*),WAP0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,II(6),IV,JJ,LEN,IOFF,IE,NG,NEL,NFT,LFT,
     .   LLT,ITY,ID,IPRT0,IPRT,IGTYP,IPROP,NUVAR,J_FIN
      INTEGER PTWA(STAT_NUMELR),
     .        PTWA_P0(0:MAX(1,STAT_NUMELR_G))
      CHARACTER*100 DELIMIT,LINE
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C-----------------------------------------------
      DATA DELIMIT(1:60)
     ./'#---1----|----2----|----3----|----4----|----5----|----6----|'/
      DATA DELIMIT(61:100)
     ./'----7----|----8----|----9----|----10---|'/
C=======================================================================
C     SPRING
C-----------------------------------------------
      JJ = 0
C
      IF (STAT_NUMELR == 0) GOTO 100
C
      IE=0
      DO NG=1,NGROUP
        ITY = IPARG(5,NG)
        IF (ITY == 6) THEN
          GBUF => ELBUF_TAB(NG)%GBUF   
          NEL  = IPARG(2,NG)
          NFT  = IPARG(3,NG)
          IPROP = IXR(1,NFT+1)
          IGTYP = IGEO(11,IPROP)
          LFT=1
          LLT=NEL
C
          DO I=LFT,LLT
            N = I + NFT
            IPRT=IPARTR(N)
            IF (IPART_STATE(IPRT) == 0) CYCLE
            WA(JJ + 1) = GBUF%OFF(I)
            WA(JJ + 2) = IPRT
            WA(JJ + 3) = IXR(NIXR,N)
            WA(JJ + 4) = IGTYP
            WA(JJ + 5) = 0  ! for NUVAR
            JJ = JJ + 5
C------------
            IF (IGTYP == 4) THEN
C------------
              WA(JJ + 1) = GBUF%FOR(I)
              WA(JJ + 2) = GBUF%TOTDEPL(I)
              WA(JJ + 3) = GBUF%FOREP(I)
              WA(JJ + 4) = GBUF%DEP_IN_TENS(I)
              WA(JJ + 5) = GBUF%DEP_IN_COMP(I)
              WA(JJ + 6) = GBUF%LENGTH(I)
              WA(JJ + 7) = GBUF%EINT(I)
cc
              JJ = JJ + 7
C------------
            ELSEIF (IGTYP == 12) THEN
C------------
              WA(JJ + 1) = GBUF%FOR(I)
              WA(JJ + 2) = GBUF%TOTDEPL(I)
              WA(JJ + 3) = GBUF%FOREP(I)
              WA(JJ + 4) = GBUF%DEP_IN_TENS(I)
              WA(JJ + 5) = GBUF%DEP_IN_COMP(I)
              WA(JJ + 6) = GBUF%LENGTH(I)
              WA(JJ + 7) = GBUF%EINT(I)
              WA(JJ + 8) = GBUF%DFS(I)
cc
              JJ = JJ + 8
C------------
            ELSEIF (IGTYP == 8 .OR. IGTYP == 13 .OR. IGTYP == 25 
     .                                          .OR. IGTYP == 23 ) THEN
C------------
              DO J=1,6
                II(J) = (J-1)*NEL + 1
              ENDDO
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
              DO J=1,6
                WA(JJ + J + 34) = GBUF%E6(II(J) + I - 1) ! E6(1:6)
              ENDDO
              JJ = JJ + 40
C------------
            ELSEIF (IGTYP == 26) THEN
C------------
              WA(JJ + 1) = GBUF%FOR(I)
              WA(JJ + 2) = GBUF%TOTDEPL(I)
              WA(JJ + 3) = GBUF%FOREP(I)
              WA(JJ + 4) = GBUF%LENGTH(I)
              WA(JJ + 5) = GBUF%EINT(I)
              WA(JJ + 6) = GBUF%DV(I)
              JJ = JJ + 6
C------------
            ELSEIF (IGTYP == 29 .OR. IGTYP == 30 .OR. IGTYP == 31 .OR.
     .              IGTYP == 32 .OR. IGTYP == 33 .OR. IGTYP == 35 .OR.
     .              IGTYP == 36 .OR. IGTYP == 44 .OR. IGTYP == 45 .OR.
     .              IGTYP == 46) THEN
C------------
              NUVAR = NINT(GEO(25,IPROP))
              WA(JJ) = NUVAR
              DO J=1,3
                II(J) = (J-1)*NEL + 1
                WA(JJ + (J-1)*2 + 1) = GBUF%FOR(II(J) + I - 1)
                WA(JJ + (J-1)*2 + 2) = GBUF%V_REPCVT(II(J) + I - 1)
                WA(JJ + (J-1)*2 + 7) = GBUF%MOM(II(J) + I - 1)
                WA(JJ + (J-1)*2 + 8) = GBUF%VR_REPCVT(II(J) + I - 1)
              ENDDO
              WA(JJ + 13) = GBUF%EINT(I)
              JJ = JJ + 13
!!              IF (IGTYP /= 32 .AND. IGTYP /= 33 .AND. IGTYP /= 45) THEN
!!                WA(JJ + 1) = GBUF%MOM(II(4) + I - 1)  !     MOM1Y
!!                WA(JJ + 2) = GBUF%MOM(II(5) + I - 1)  !     MOM1Z
!!                JJ = JJ + 2
!!              ENDIF
              DO J=1,NUVAR
                IV = NUVAR*(I-1) + J
                WA(JJ + J) = GBUF%VAR(IV)
              ENDDO
              JJ = JJ + NUVAR 
C------------
            ENDIF ! IF (IGTYP)
C
            IE=IE+1
C         pointeur de fin de zone dans WA
            PTWA(IE)=JJ
          ENDDO  !  DO I=LFT,LLT
c------- end loop over spring elements
        ENDIF ! ITY == 6
      ENDDO   ! NG = 1, NGROUP
C
 100  CONTINUE
c-----------------------------------------------------------------------
c     SPRING - WRITE
c-----------------------------------------------------------------------
      IF (NSPMD == 1) THEN
C     recopies inutiles pour simplification du code.
        PTWA_P0(0)=0
        DO N=1,STAT_NUMELR
          PTWA_P0(N)=PTWA(N)
        ENDDO
        LEN=JJ
        DO J=1,LEN
          WAP0(J)=WA(J)
        ENDDO
      ELSE
C       construit les pointeurs dans le tableau global WAP0
        CALL SPMD_STAT_PGATHER(PTWA,STAT_NUMELR,PTWA_P0,STAT_NUMELR_G)
        LEN = 0
        CALL SPMD_RGATHER9_DP(WA,JJ,WAP0,SIZP0,LEN)
      ENDIF
c-------------------------------------
      IF (ISPMD == 0 .AND. LEN > 0) THEN
        IPRT0 = 0
        DO N=1,STAT_NUMELR_G
C         retrouve le nieme elt dans l'ordre d'id croissant
          K=STAT_INDXR(N)
C         retrouve l'adresse dans WAP0
          J=PTWA_P0(K-1)
C
          IOFF = NINT(WAP0(J + 1))
          IF (IOFF >= 1) THEN
            IPRT  = NINT(WAP0(J + 2)) 
            ID    = NINT(WAP0(J + 3))
            IGTYP = NINT(WAP0(J + 4))
            NUVAR = NINT(WAP0(J + 5))
            J = J + 5
C--------------------------------------
            IF (IGTYP == 4) THEN
C--------------------------------------
              IF (IPRT /= IPRT0) THEN
                WRITE(IUGEO,'(A)') DELIMIT
                WRITE(IUGEO,'(A)')'/INISPRI/FULL'
                WRITE(IUGEO,'(A)')
     .          '#----------------------------------------------------------'
                WRITE(IUGEO,'(A)')'#SPRING_ID PROP_TYPE     NUVAR'
                WRITE(IUGEO,'(A)')'#FORMAT:(1P5E20.13) #(F(I),DL(I),FEP(I),DPL(I),DPL2(I),I=SPRING_ID)'
                WRITE(IUGEO,'(A)')'#FORMAT:(1P2E20.13) #(XLO(I),EI(I),I=SPRING_ID)'
                WRITE(IUGEO,'(A)')
     .          '#----------------------------------------------------------'
                IPRT0=IPRT
                IPRT0=IPRT
              ENDIF ! IF (IPRT /= IPRT0)
C---
              WRITE(IUGEO,'(3I10)') ID,IGTYP,NUVAR
!!              WRITE(IUGEO,'(1P5E20.13)')(WAP0(J+K),K=1,7)
              WRITE(IUGEO,'(1P5E20.13)')(WAP0(J+K),K=1,5)
              WRITE(IUGEO,'(1P2E20.13)')(WAP0(J+K),K=6,7)
C--------------------------------------
            ELSEIF (IGTYP == 12) THEN
C--------------------------------------
              IF (IPRT /= IPRT0) THEN
                WRITE(IUGEO,'(A)') DELIMIT
                WRITE(IUGEO,'(A)')'/INISPRI/FULL'
                WRITE(IUGEO,'(A)')
     .          '#----------------------------------------------------------'
                WRITE(IUGEO,'(A)')
     .          '#SPRING_ID PROP_TYPE     NUVAR'
                WRITE(IUGEO,'(A)') '#FORMAT:(1P5E20.13) #(F(I),DL(I),FEP(I),DPL(I),DPL2(I),I=SPRING_ID)'
                WRITE(IUGEO,'(A)') '#FORMAT:(1P3E20.13) #(XL0(I),EI(I),DFS(I),I=SPRING_ID)'
                WRITE(IUGEO,'(A)')
     .          '#----------------------------------------------------------'
                IPRT0=IPRT
              ENDIF ! IF (IPRT /= IPRT0)
C---
              WRITE(IUGEO,'(3I10)') ID,IGTYP,NUVAR
!!              WRITE(IUGEO,'(1P5E20.13)')(WAP0(J+K),K=1,8)
              WRITE(IUGEO,'(1P5E20.13)')(WAP0(J+K),K=1,5)
              WRITE(IUGEO,'(1P3E20.13)')(WAP0(J+K),K=6,8)
C--------------------------------------
            ELSEIF (IGTYP == 8 .OR. IGTYP == 13 .OR. IGTYP == 25
     .                                          .OR. IGTYP == 23 ) THEN
C--------------------------------------
              IF (IPRT /= IPRT0) THEN
                WRITE(IUGEO,'(A)') DELIMIT
                WRITE(IUGEO,'(A)')'/INISPRI/FULL'
                WRITE(IUGEO,'(A)')
     .          '#----------------------------------------------------------'
                WRITE(IUGEO,'(A)')
     .          '#SPRING_ID PROP_TYPE     NUVAR'
                WRITE(IUGEO,'(A)')'#FORMAT:(1P5E20.13) #(FX(I),DX(I),FXEP(I),DPX(I),DPX2(I),I=SPRING_ID)'
                WRITE(IUGEO,'(A)')'#FORMAT:(1P5E20.13) #(FY(I),DY(I),FYEP(I),DPX(I),DPX2(I),I=SPRING_ID)' 
                WRITE(IUGEO,'(A)')'#FORMAT:(1P5E20.13) #(FZ(I),DZ(I),FZEP(I),DPX(I),DPX2(I),I=SPRING_ID)'
                WRITE(IUGEO,'(A)')'#FORMAT:(1P5E20.13) #(XMOM(I),RX(I),XMEP(I),RPX(I),RPX2(I),I=SPRING_ID)'
                WRITE(IUGEO,'(A)')'#FORMAT:(1P5E20.13) #(YMOM(I),RY(I),YMEP(I),RPY(I),RPY2(I),I=SPRING_ID)'
                WRITE(IUGEO,'(A)')'#FORMAT:(1P5E20.13) #(ZMOM(I),RZ(I),ZMEP(I),RPZ(I),RPZ2(I),I=SPRING_ID)'
                WRITE(IUGEO,'(A)')'#FORMAT:(1P5E20.13) #(XLO(I),YL0(I),ZL0(I),EI(I),E1(I),I=SPRING_ID)'
                WRITE(IUGEO,'(2A)')'#FORMAT:(1P5E20.13) #(E2(I),E3(I),E4(I),E5(I),E6(I),I=SPRING_ID)'
                WRITE(IUGEO,'(A)')
     .          '#----------------------------------------------------------'
                IPRT0=IPRT
              ENDIF ! IF (IPRT /= IPRT0)
C---
              WRITE(IUGEO,'(3I10)') ID,IGTYP,NUVAR
              WRITE(IUGEO,'(1P5E20.13)')(WAP0(J+K),K=1,40)
C--------------------------------------
            ELSEIF (IGTYP == 26) THEN
C--------------------------------------
              IF (IPRT /= IPRT0) THEN
                WRITE(IUGEO,'(A)') DELIMIT
                WRITE(IUGEO,'(A)')'/INISPRI/FULL'
                WRITE(IUGEO,'(A)')
     .          '#----------------------------------------------------------'
                WRITE(IUGEO,'(A)')
     .          '#SPRING_ID PROP_TYPE     NUVAR'
                WRITE(IUGEO,'(A)') '#FORMAT:(1P3E20.13) #(F(I),DL(I),FEP(I),I=SPRING_ID)'
                WRITE(IUGEO,'(A)') '#FORMAT:(1P3E20.13) #(XL0(I),EI(I),DV(I),I=SPRING_ID)'
                WRITE(IUGEO,'(A)')
     .          '#----------------------------------------------------------'
                IPRT0=IPRT
              ENDIF ! IF (IPRT /= IPRT0)
C---
              WRITE(IUGEO,'(3I10)') ID,IGTYP,NUVAR
              WRITE(IUGEO,'(1P3E20.13)')(WAP0(J+K),K=1,3)
              WRITE(IUGEO,'(1P3E20.13)')(WAP0(J+K),K=4,6)
C--------------------------------------
            ELSEIF (IGTYP == 29 .OR. IGTYP == 30 .OR. IGTYP == 31 .OR.
     .              IGTYP == 32 .OR. IGTYP == 33 .OR. IGTYP == 35 .OR.
     .              IGTYP == 36 .OR. IGTYP == 44 .OR. IGTYP == 45 .OR.
     .              IGTYP == 46) THEN
C--------------------------------------
              IF (IPRT /= IPRT0) THEN
                WRITE(IUGEO,'(A)') DELIMIT
                WRITE(IUGEO,'(A)')'/INISPRI/FULL'
                WRITE(IUGEO,'(A)')
     .          '#----------------------------------------------------------'
                WRITE(IUGEO,'(A)')
     .          '#SPRING_ID PROP_TYPE     NUVAR'
                WRITE(IUGEO,'(A)')'#FORMAT:(1P2E20.13) #(FX(I),DX(I),I=SPRING_ID)'
                WRITE(IUGEO,'(A)')'#FORMAT:(1P2E20.13) #(FY(I),DY(I),I=SPRING_ID)'
                WRITE(IUGEO,'(A)')'#FORMAT:(1P2E20.13) #(FZ(I),DZ(I),I=SPRING_ID)'
                WRITE(IUGEO,'(A)')'#FORMAT:(1P2E20.13) #(XMOM(I),RX(I),I=SPRING_ID)'
                WRITE(IUGEO,'(A)')'#FORMAT:(1P2E20.13) #(YMOM(I),RY(I),I=SPRING_ID)'
                WRITE(IUGEO,'(A)')'#FORMAT:(1P2E20.13) #(ZMOM(I),RZ(I),I=SPRING_ID)'
                WRITE(IUGEO,'(A)')'#FORMAT:(1P1E20.13) #(EI(I),I=1,NEL)'
!!                IF (IGTYP /= 32 .AND. IGTYP /= 33 .AND. IGTYP /= 45) THEN
!!                  WRITE(IUGEO,'(A)')'#FORMAT:(1P2E20.13)
!!     .        #(MOM1Y(I),MOM1Z(I),I=SPRING_ID)'
!!                ENDIF
                WRITE(IUGEO,'(A)')'#FORMAT:(1P5E20.13) #(UVAR(I,J),J=1,NUVAR),I=SPRING_ID)'
                WRITE(IUGEO,'(A)')
     .          '#----------------------------------------------------------'
                IPRT0=IPRT
              ENDIF ! IF (IPRT /= IPRT0)
C---
              WRITE(IUGEO,'(3I10)') ID,IGTYP,NUVAR
!!              WRITE(IUGEO,'(1P5E20.13)') (WAP0(J+K),K=1,12)
              WRITE(IUGEO,'(1P2E20.13)') (WAP0(J+K),K=1,2)
              WRITE(IUGEO,'(1P2E20.13)') (WAP0(J+K),K=3,4)
              WRITE(IUGEO,'(1P2E20.13)') (WAP0(J+K),K=5,6)
              WRITE(IUGEO,'(1P2E20.13)') (WAP0(J+K),K=7,8)
              WRITE(IUGEO,'(1P2E20.13)') (WAP0(J+K),K=9,10)
              WRITE(IUGEO,'(1P2E20.13)') (WAP0(J+K),K=11,12)
              WRITE(IUGEO,'(1P1E20.13)')  WAP0(J+13)  ! EINT
              J_FIN = J + 13
C
!!              IF (IGTYP /= 32 .AND. IGTYP /= 33 .AND. IGTYP /= 45) THEN
!!                WRITE(IUGEO,'(1P2E20.13)') (WAP0(J_FIN+K),K=1,2) ! MOM1Y, MOM1Z
!!                J_FIN = J_FIN + 2
!!              ENDIF
C  ( + NUVAR )
              WRITE(IUGEO,'(1P5E20.13)')(WAP0(J_FIN+K),K=1,NUVAR)
C--------------------------------------
            ENDIF ! IF (IGTYP)
C--------------------------------------
          ENDIF  !  IF (IOFF >= 1)
        ENDDO  !  DO N=1,STAT_NUMELR_G
      ENDIF  !  IF (ISPMD == 0.AND.LEN > 0)
C---
      RETURN
      END
