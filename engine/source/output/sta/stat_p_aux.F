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
Chd|  STAT_P_AUX                    source/output/sta/stat_p_aux.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F
Chd|-- calls ---------------
Chd|        SPMD_RGATHER9_DP              source/mpi/interfaces/spmd_outp.F
Chd|        SPMD_STAT_PGATHER             source/mpi/output/spmd_stat.F
Chd|        STRS_TXT50                    source/output/sta/sta_txt.F
Chd|        TAB_STRS_TXT50                source/output/sta/sta_txt.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE STAT_P_AUX(
     1                  ELBUF_TAB  ,IPARG  ,IPM   ,IGEO       ,IXP       ,
     2                  WA         ,WAP0   ,IPARTP,IPART_STATE,STAT_INDXP,
     3                  SIZP0      )
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
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER SIZLOC,SIZP0
        INTEGER IXP(NIXP,*),
     .          IPARG(NPARG,*),IPM(NPROPMI,*),IGEO(NPROPGI,*),
     .          IPARTP(*),IPART_STATE(*),STAT_INDXP(*)
        TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
        double precision WA(*),WAP0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,N,J,K,JJ,LEN,IOFF,NG,NEL,NFT,ITY,LFT,LLT,ID,IPRT0,IPRT,IE,
     .          NPT,IR,IS,IPT,IL,IVAR,NUVAR,MY_NUVAR,IGTYP,IPROP,MLW
        INTEGER PTWA(STAT_NUMELP),
     .          PTWA_P0(0:MAX(1,STAT_NUMELP_G))
        CHARACTER*100 DELIMIT,LINE
        TYPE(G_BUFEL_)  ,POINTER :: GBUF
        my_real,
     .    DIMENSION(:)  ,POINTER :: UVAR
C-----------------------------------------------
        DATA DELIMIT(1:60)
     .  /'#---1----|----2----|----3----|----4----|----5----|----6----|'/
        DATA DELIMIT(61:100)
     .  /'----7----|----8----|----9----|----10---|'/
C-----------------------------------------------
C     BEAM
C-----------------------------------------------
        JJ = 0
!
        IF (STAT_NUMELP /= 0) THEN
!
          IE=0
          DO NG=1,NGROUP
            ITY = IPARG(5,NG)
            IF (ITY == 5) THEN
              GBUF => ELBUF_TAB(NG)%GBUF
              MLW  = IPARG(1,NG)
              NEL  = IPARG(2,NG)
              NFT  = IPARG(3,NG)
              NPT  = IPARG(6,NG)
              IPROP = IXP(5,NFT+1)
              IGTYP = IGEO(11,IPROP)
              LFT=1
              LLT=NEL
!
              DO I=LFT,LLT
                N  = I + NFT
                IPRT=IPARTP(N)
                IF (IPART_STATE(IPRT) /= 0) THEN
                  WA(JJ + 1) = GBUF%OFF(I)
                  WA(JJ + 2) = IPRT
                  WA(JJ + 3) = IXP(NIXP,N)
                  WA(JJ + 4) = IGTYP
                  WA(JJ + 5) = NPT
                  JJ = JJ + 5
!---
                  IF (MLW == 36) THEN ! only one user law compatible with beams
!---
                    MY_NUVAR = IPM(8,IXP(1,N))
                    JJ = JJ + 1
                    WA(JJ) = MY_NUVAR
!
                    DO IPT=1,NPT
                      IL = 1
                      IR = 1
                      IS = 1
!!                    NUVAR = ELBUF_STR%BUFLY(ILAY)%NVAR_MAT
                      UVAR => ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IPT)%VAR
                      DO IVAR=1,MY_NUVAR
                        JJ = JJ + 1
                        WA(JJ) = UVAR((IVAR-1)*NEL + I)
                      ENDDO
                    ENDDO ! DO IPT=1,NPT
!---
                  ELSE    ! Not User law
!---
                    MY_NUVAR = 0
                    JJ = JJ + 1
                    WA(JJ) = MY_NUVAR
                  ENDIF ! IF (MLW == 36)
!---
                  IE=IE+1
!               pointeur de fin de zone dans WA
                  PTWA(IE)=JJ
                ENDIF ! IF (IPART_STATE(IPRT) /= 0)
              ENDDO  ! DO I=LFT,LLT
            ENDIF  ! IF (ITY == 5)
          ENDDO  ! DO NG=1,NGROUP
        ENDIF ! IF (STAT_NUMELP /= 0)
!-----------------------------------------------------------------------
!     BEAM - WRITE
!-----------------------------------------------------------------------
        IF (NSPMD == 1) THEN
!       recopies inutiles pour simplification du code.
          PTWA_P0(0)=0
          DO N=1,STAT_NUMELP
            PTWA_P0(N)=PTWA(N)
          ENDDO
          LEN=JJ
          DO J=1,LEN
            WAP0(J)=WA(J)
          ENDDO
        ELSE
!       construit les pointeurs dans le tableau global WAP0
          CALL SPMD_STAT_PGATHER(PTWA,STAT_NUMELP,PTWA_P0,STAT_NUMELP_G)
          LEN = 0
          CALL SPMD_RGATHER9_DP(WA,JJ,WAP0,SIZP0,LEN)
        END IF
!-------------------------------------
        IF (ISPMD == 0 .AND. LEN > 0) THEN
          IPRT0 = 0
          DO N=1,STAT_NUMELP_G
!         retrouve le nieme elt dans l'ordre d'id croissant
            K=STAT_INDXP(N)
!         retrouve l'adresse dans WAP0
            J=PTWA_P0(K-1)
!
            IOFF  = NINT(WAP0(J + 1))
            MY_NUVAR = NINT(WAP0(J + 6))
            IF (IOFF >= 1 .AND. MY_NUVAR /= 0) THEN
              IPRT = NINT(WAP0(J + 2))
              IF (IPRT /= IPRT0) THEN
                IF (IZIPSTRS == 0) THEN
                  WRITE(IUGEO,'(A)') DELIMIT
                  WRITE(IUGEO,'(A)')'/INIBEAM/AUX'
                  WRITE(IUGEO,'(A)')
     .            '#------------------------ REPEAT --------------------------'
                  WRITE(IUGEO,'(A)')
     .            '#  BEAMID       NPT       PROP_TYPE      NVAR'
                  WRITE(IUGEO,'(A/A)')
     .            '# THE CONTENT OF THE FOLLOWING CARDS WILL NOT BE DISCLOSED',
     .            '# THEY MUST NOT BE CHANGED.'
                  WRITE(IUGEO,'(A)')
     .            '#---------------------- END REPEAT ------------------------'
                  WRITE(IUGEO,'(A)') DELIMIT
                ELSE
                  WRITE(LINE,'(A)') DELIMIT
                  CALL STRS_TXT50(LINE,100)
                  WRITE(LINE,'(A)')'/INIBEAM/AUX'
                  CALL STRS_TXT50(LINE,100)
                  WRITE(LINE,'(A)')
     .            '#------------------------ REPEAT --------------------------'
                  CALL STRS_TXT50(LINE,100)
                  WRITE(LINE,'(A)')
     .            '#  BEAMID       NPT       PROP_TYPE      NVAR'
                  CALL STRS_TXT50(LINE,100)
                  WRITE(LINE,'(A)')
     .            '# THE CONTENT OF THE FOLLOWING CARDS WILL NOT BE DISCLOSED'
                  CALL STRS_TXT50(LINE,100)
                  WRITE(LINE,'(A)')
     .            '# THEY MUST NOT BE CHANGED.'
                  CALL STRS_TXT50(LINE,100)
                  WRITE(LINE,'(A)')
     .            '#---------------------- END REPEAT ------------------------'
                  CALL STRS_TXT50(LINE,100)
                  WRITE(LINE,'(A)') DELIMIT
                  CALL STRS_TXT50(LINE,100)
                ENDIF  !  IF (IZIPSTRS == 0)
                IPRT0=IPRT
              ENDIF  ! IF (IPRT /= IPRT0)
              ID    = NINT(WAP0(J + 3))
              IGTYP = NINT(WAP0(J + 4))
              NPT   = NINT(WAP0(J + 5))
              MY_NUVAR = NINT(WAP0(J + 6))
              J = J + 6
              IF (IZIPSTRS == 0) THEN
                WRITE(IUGEO,'(4I10)')ID,NPT,IGTYP,MY_NUVAR
              ELSE
                WRITE(LINE,'(4I10)')ID,NPT,IGTYP,MY_NUVAR
                CALL STRS_TXT50(LINE,100)
              ENDIF
              DO JJ=1,NPT
                IF (IZIPSTRS == 0) THEN
                  WRITE(IUGEO,'(1P5E20.13)')(WAP0(J + K),K=1,MY_NUVAR)
                ELSE
                  CALL TAB_STRS_TXT50(WAP0(1),MY_NUVAR,J,SIZP0,5)
                ENDIF
                J=J+MY_NUVAR
              ENDDO
            ENDIF  !  IF (IOFF == 1 .AND. MY_NUVAR /= 0)
          ENDDO  !  DO N=1,STAT_NUMELP_G
        ENDIF  !  IF (ISPMD == 0.AND.LEN > 0)
c-----------
        RETURN
      END
