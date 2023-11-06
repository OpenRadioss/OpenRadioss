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
Chd|  STAT_C_FAIL                   source/output/sta/stat_c_fail.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F
Chd|-- calls ---------------
Chd|        SPMD_RGATHER9_DP              source/mpi/interfaces/spmd_outp.F
Chd|        SPMD_STAT_PGATHER             source/mpi/output/spmd_stat.F
Chd|        STRS_TXT50                    source/output/sta/sta_txt.F
Chd|        TAB_STRS_TXT50                source/output/sta/sta_txt.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE STAT_C_FAIL(ELBUF_TAB ,IPARG ,IPM ,IGEO ,IXC ,
     2                        IXTG  ,WA,WAP0 ,IPARTC, IPARTTG,
     3                      IPART_STATE,STAT_INDXC,STAT_INDXTG,SIZP0)
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
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER SIZLOC,SIZP0
        INTEGER IXC(NIXC,*),IXTG(NIXTG,*),
     .          IPARG(NPARG,*),IPM(NPROPMI,*),IGEO(NPROPGI,*),
     .          IPARTC(*), IPARTTG(*), IPART_STATE(*),
     .          STAT_INDXC(*), STAT_INDXTG(*)
        TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
        double precision WA(*),WAP0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,N,J,K,L,II,JJ,ID,IE,LEN,NG,NEL,NFT,ITY,LFT,LLT,NPT,
     .     MLW,IGTYP,IPRT0,IPRT,IVAR,IMAT,
     .     NPG,IPG,NLAY,NPTR,NPTS,NPTT,IL,IR,IS,IT,IPT,IC,IFAIL,NV,
     .     NFAIL,NVAR_RUPT,NPTG,IRUPT,IRUPT_TYPE,ISUBSTACK
        INTEGER MAT(MVSIZ), PID(MVSIZ),
     .     PTWA(MAX(STAT_NUMELC ,STAT_NUMELTG)),
     .     PTWA_P0(0:MAX(1,STAT_NUMELC_G,STAT_NUMELTG_G))
        double precision
     .      THK, EM, EB, H1, H2, H3
        CHARACTER*100 DELIMIT,LINE
C----
        TYPE(L_BUFEL_) ,POINTER :: LBUF
        TYPE(G_BUFEL_) ,POINTER :: GBUF
        TYPE(BUF_MAT_) ,POINTER :: MBUF
        TYPE(BUF_FAIL_),POINTER :: FBUF
        my_real,
     .    DIMENSION(:), POINTER  :: UVARF,DFMAX
C----
        DATA DELIMIT(1:60)
     .  /'#---1----|----2----|----3----|----4----|----5----|----6----|'/
        DATA DELIMIT(61:100)
     .  /'----7----|----8----|----9----|----10---|'/
C=======================================================================
C     4-NODE SHELLS
C-----------------------------------------------
        JJ = 0
        ISUBSTACK = 0
        IF (STAT_NUMELC==0) GOTO 200
C
        IE=0
        DO NG=1,NGROUP
          ITY = IPARG(5,NG)
          IF (ITY == 3) THEN
            MLW   =IPARG(1,NG)
            NEL   =IPARG(2,NG)
            NFT   =IPARG(3,NG)
            LFT=1
            LLT=NEL
C
c          DO I=1,NEL
c            MAT(I) = IXC(1,I)
c            PID(I) = IXC(6,I)
c          ENDDO
C
            GBUF => ELBUF_TAB(NG)%GBUF
            NLAY = ELBUF_TAB(NG)%NLAY
            NPTR = ELBUF_TAB(NG)%NPTR
            NPTS = ELBUF_TAB(NG)%NPTS
c         NPTT = ELBUF_TAB(NG)%NPTT
c         NPT  = NPTT * NLAY
            NPG  = NPTR*NPTS
            ISUBSTACK = IPARG(71,NG)
c
c-------------------------------------------------------
c
            DO I=LFT,LLT
              N = I+NFT
              IPRT=IPARTC(N)
              IF (IPART_STATE(IPRT)==0) CYCLE
C
              JJ = JJ + 1
              IF (MLW /= 0 .AND. MLW /= 13) THEN
                WA(JJ) = GBUF%OFF(I)
              ELSE
                WA(JJ) = ZERO
              ENDIF
              JJ = JJ + 1
              WA(JJ) = IPRT
              JJ = JJ + 1
              WA(JJ) = IXC(NIXC,N)
              JJ = JJ + 1
              WA(JJ) = NLAY
cc           JJ = JJ + 1
cc           WA(JJ) = NPTT
              JJ = JJ + 1
              WA(JJ) = NPG
c
              DO IL = 1,NLAY
                NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL
                IMAT  = ELBUF_TAB(NG)%BUFLY(IL)%IMAT
                FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(1,1,1)
                NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                JJ = JJ + 1
                WA(JJ) = NFAIL
                JJ = JJ + 1
                IMAT  = ELBUF_TAB(NG)%BUFLY(IL)%IMAT
                WA(JJ) = IPM(1,IMAT)
                JJ = JJ + 1
                WA(JJ) = NPTT
c
                DO IFAIL = 1,NFAIL
                  IRUPT = IPM(236+IFAIL,IMAT)
                  IRUPT_TYPE = IPM(241+IFAIL,IMAT)
                  NVAR_RUPT = FBUF%FLOC(IFAIL)%NVAR
                  JJ = JJ + 1
                  WA(JJ) = NVAR_RUPT + 1
                  JJ = JJ + 1
                  WA(JJ) = IRUPT
                  JJ = JJ + 1
                  WA(JJ) = IRUPT_TYPE
!
                  IF (IRUPT == 0) CYCLE
!
                  DO IT=1,NPTT
                    DO IS=1,NPTS
                      DO IR=1,NPTR
                        FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)
                        UVARF => FBUF%FLOC(IFAIL)%VAR
                        DFMAX => FBUF%FLOC(IFAIL)%DAMMX
                        JJ = JJ + 1
                        WA(JJ) = DFMAX(I)
                        DO NV=1,NVAR_RUPT
                          JJ = JJ + 1
                          WA(JJ) = UVARF((NV-1)*LLT+I)
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO
                ENDDO  ! IFAIL = 1,NFAIL
c
              ENDDO      ! IL = 1,NLAY
c
              IE=IE+1
C       pointeur de fin de zone
              PTWA(IE)=JJ
            ENDDO        ! I=LFT,LLT
c--------------
          ENDIF    !  ITY == 3
        ENDDO       !  NG = 1,NGROUP
C
 200    CONTINUE
c
c-----------------------------------------------------------------------
c
        IF (NSPMD == 1) THEN
          PTWA_P0(0)=0
          DO N=1,STAT_NUMELC
            PTWA_P0(N)=PTWA(N)
          ENDDO
          LEN=JJ
          DO J=1,LEN
            WAP0(J)=WA(J)
          ENDDO
        ELSE
C       construit les pointeurs dans le tableau global WAP0
          CALL SPMD_STAT_PGATHER(PTWA,STAT_NUMELC,PTWA_P0,STAT_NUMELC_G)
          LEN = 0
          CALL SPMD_RGATHER9_DP(WA,JJ,WAP0,SIZP0,LEN)
        ENDIF
c
c-----------------------------------------------------------------------
c
        IF (ISPMD == 0.AND.LEN > 0) THEN
          IPRT0=0
          DO N=1,STAT_NUMELC_G
C         retrouve le nieme elt dans l'ordre d'id croissant
            K=STAT_INDXC(N)
C         retrouve l'adresse dans WAP0
            J=PTWA_P0(K-1)
            IPRT  = NINT(WAP0(J + 2))
            IF (IPRT /= IPRT0) THEN
              IF (IZIPSTRS == 0) THEN
                WRITE(IUGEO,'(A)') DELIMIT
                WRITE(IUGEO,'(A)')'/INISHE/FAIL'
                WRITE(IUGEO,'(A)')
     .          '#------------------------ REPEAT --------------------------'
                WRITE(IUGEO,'(A)')
     .          '#  SHELLID      NLAY       NPG      NPTT      ILAY     IRUPT IRUPT_TYP     NUVAR      IMAT'
                WRITE(IUGEO,'(A/A/A)')
     .          '# REPEAT K=1,NPG                                     ',
     .          '#    UVAR(1,I) .............                         ',
     .          '#    ...............              UVAR(NUVAR,I)      '
                WRITE(IUGEO,'(A)')
     .          '#---------------------- END REPEAT ------------------------'
                WRITE(IUGEO,'(A)') DELIMIT
              ELSE
                WRITE(LINE,'(A)') DELIMIT
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'/INISHE/FAIL'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .          '#------------------------ REPEAT --------------------------'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .          '#  SHELLID      NLAY       NPG      NPTT      ILAY     IRUPT IRUPT_TYP     NUVAR      IMAT'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .          '# REPEAT K=1,NPG                                           '
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .          '#  UVAR(1,I) .............                                 '
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .          '#    ...............              UVAR(NUVAR,I)            '
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .          '#---------------------- END REPEAT ------------------------'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)') DELIMIT
                CALL STRS_TXT50(LINE,100)
              ENDIF  ! IF (IZIPSTRS == 0)
              IPRT0=IPRT
            ENDIF  ! IF (IPRT /= IPRT0)
c
            ID     = NINT(WAP0(J+3))
            NLAY   = NINT(WAP0(J+4))
cc            NPTT   = NINT(WAP0(J+5))
            NPTG   = NINT(WAP0(J+5))
            J = J + 5
c
            DO IL=1,NLAY
              IC = NINT(WAP0(J+1))
              J = J + 1
              IMAT = NINT(WAP0(J+1))
              J = J + 1
              NPTT = NINT(WAP0(J+1))
              J = J + 1
              DO II=1,IC
                NVAR_RUPT = NINT(WAP0(J+1))
                J = J + 1
                IRUPT = NINT(WAP0(J+1))
                J = J + 1
                IRUPT_TYPE = NINT(WAP0(J+1))
                J = J + 1
!
                IF (IRUPT == 0) CYCLE
!
                IF (IZIPSTRS == 0) THEN
                  WRITE(IUGEO,'(9I10)') ID,NLAY,NPTG,NPTT,IL,IRUPT,IRUPT_TYPE,NVAR_RUPT,
     .                                  IMAT
                ELSE
                  WRITE(LINE,'(9I10)') ID,NLAY,NPTG,NPTT,IL,IRUPT,IRUPT_TYPE,NVAR_RUPT,
     .                                 IMAT
                  CALL STRS_TXT50(LINE,100)
                ENDIF
                IF (IRUPT /= 0 .AND. NVAR_RUPT /= 0) THEN
                  IF (IZIPSTRS == 0) THEN
                    DO IT=1,NPTT
                      DO IPG=1,NPTG
                        WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + L),L=1,NVAR_RUPT)
                        J = J + NVAR_RUPT
                      ENDDO
                    ENDDO
                  ELSE
                    DO IT=1,NPTT
                      DO IPG=1,NPTG
                        CALL TAB_STRS_TXT50(WAP0(1),NVAR_RUPT,J,SIZP0,3)
                        J = J + NVAR_RUPT
                      ENDDO
                    ENDDO
                  ENDIF  ! IF (IZIPSTRS == 0)
                ENDIF  ! IF (IRUPT /= 0 .AND. NVAR_RUPT /= 0)
              ENDDO  !  DO II=1,IC
            ENDDO  ! DO IL=1,NLAY
          ENDDO  !  DO N=1,STAT_NUMELC_G
        ENDIF  ! IF (ISPMD == 0.AND.LEN > 0)
C-----------------------------------------------
C     3-NODE SHELLS
C-----------------------------------------------
        JJ = 0
        ISUBSTACK = 0
        IF (STAT_NUMELTG==0) GOTO 300
C
        IE=0
        DO NG=1,NGROUP
          ITY     =IPARG(5,NG)
          IF (ITY == 7) THEN
            MLW   =IPARG(1,NG)
            NEL   =IPARG(2,NG)
            NFT   =IPARG(3,NG)
            LFT=1
            LLT=NEL
C
c          DO I=1,NEL
c            MAT(I) = IXTG(1,I)
c            PID(I) = IXTG(6,I)
c          ENDDO
C
            GBUF => ELBUF_TAB(NG)%GBUF
            NLAY = ELBUF_TAB(NG)%NLAY
            NPTR = ELBUF_TAB(NG)%NPTR
            NPTS = ELBUF_TAB(NG)%NPTS
c          NPTT = ELBUF_TAB(NG)%NPTT
c          NPT  = NPTT * NLAY
            NPG  = NPTR*NPTS
            ISUBSTACK = IPARG(71,NG)
c
c-------------------------------------------------------
c
            DO I=LFT,LLT
              N = I+NFT
              IPRT=IPARTTG(N)
              IF (IPART_STATE(IPRT)==0) CYCLE
C
              JJ = JJ + 1
              IF (MLW /= 0 .AND. MLW /= 13) THEN
                WA(JJ) = GBUF%OFF(I)
              ELSE
                WA(JJ) = ZERO
              ENDIF
              JJ = JJ + 1
              WA(JJ) = IPRT
              JJ = JJ + 1
              WA(JJ) = IXTG(NIXTG,N)
              JJ = JJ + 1
              WA(JJ) = NLAY
cc           JJ = JJ + 1
cc           WA(JJ) = NPTT
              JJ = JJ + 1
              WA(JJ) = NPG
c
              DO IL = 1,NLAY
                NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL
                IMAT = ELBUF_TAB(NG)%BUFLY(IL)%IMAT
                FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(1,1,1)
                NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                JJ = JJ + 1
                WA(JJ) = NFAIL
                JJ = JJ + 1
                IMAT  = ELBUF_TAB(NG)%BUFLY(IL)%IMAT
                WA(JJ) = IPM(1,IMAT)
                JJ = JJ + 1
                WA(JJ) = NPTT
c
                DO IFAIL = 1,NFAIL
                  IRUPT = IPM(236+IFAIL,IMAT)
                  IRUPT_TYPE = IPM(241+IFAIL,IMAT)
                  NVAR_RUPT  = FBUF%FLOC(IFAIL)%NVAR
                  JJ = JJ + 1
                  WA(JJ) = NVAR_RUPT + 1
                  JJ = JJ + 1
                  WA(JJ) = IRUPT
                  JJ = JJ + 1
                  WA(JJ) = IRUPT_TYPE
!
                  IF (IRUPT == 0) CYCLE
!
                  DO IT = 1,NPTT
                    DO IS=1,NPTS
                      DO IR=1,NPTR
                        FBUF => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)
                        UVARF => FBUF%FLOC(IFAIL)%VAR
                        DFMAX => FBUF%FLOC(IFAIL)%DAMMX
                        JJ = JJ + 1
                        WA(JJ) = DFMAX(I)
                        DO NV=1,NVAR_RUPT
                          JJ = JJ + 1
                          WA(JJ) = UVARF((NV-1)*LLT+I)
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO    ! IT = 1,NPTT
                ENDDO  ! IFAIL = 1,NFAIL
              ENDDO      ! IL = 1,NLAY
c
              IE=IE+1
C       pointeur de fin de zone
              PTWA(IE)=JJ
            ENDDO        ! I=LFT,LLT
c--------------
          ENDIF    !  ITY == 7
        ENDDO       !  NG = 1,NGROUP
C
 300    CONTINUE
c
c-----------------------------------------------------------------------
c
        IF (NSPMD == 1) THEN
          PTWA_P0(0)=0
          DO N=1,STAT_NUMELTG
            PTWA_P0(N)=PTWA(N)
          ENDDO
          LEN=JJ
          DO J=1,LEN
            WAP0(J)=WA(J)
          ENDDO
        ELSE
C       construit les pointeurs dans le tableau global WAP0
          CALL SPMD_STAT_PGATHER(PTWA,STAT_NUMELTG,PTWA_P0,STAT_NUMELTG_G)
          LEN = 0
          CALL SPMD_RGATHER9_DP(WA,JJ,WAP0,SIZP0,LEN)
        ENDIF
c
c-----------------------------------------------------------------------
c
        IF (ISPMD == 0.AND.LEN > 0) THEN
          IPRT0=0
          DO N=1,STAT_NUMELTG_G
C         retrouve le nieme elt dans l'ordre d'id croissant
            K=STAT_INDXTG(N)
C         retrouve l'adresse dans WAP0
            J=PTWA_P0(K-1)
            IPRT = NINT(WAP0(J + 2))
            IF (IPRT /= IPRT0) THEN
              IF (IZIPSTRS == 0) THEN
                WRITE(IUGEO,'(A)') DELIMIT
                WRITE(IUGEO,'(A)')'/INISH3/FAIL'
                WRITE(IUGEO,'(A)')
     .          '#------------------------ REPEAT --------------------------'
                WRITE(IUGEO,'(A)')
     .          '#  SHELLID      NLAY       NPG     NPTT      ILAY     IRUPT IRUPT_TYP     NUVAR      IMAT'
                WRITE(IUGEO,'(A/A/A)')
     .          '# REPEAT K=1,NPG                                     ',
     .          '#    UVAR(1,I) .............                         ',
     .          '#    ...............              UVAR(NUVAR,I)      '
                WRITE(IUGEO,'(A)')
     .          '#---------------------- END REPEAT ------------------------'
                WRITE(IUGEO,'(A)') DELIMIT
              ELSE
                WRITE(LINE,'(A)') DELIMIT
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'/INISH3/FAIL'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .          '#------------------------ REPEAT --------------------------'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .          '#  SHELLID      NLAY       NPG      NPTT      ILAY     IRUPT IRUPT_TYP     NUVAR      IMAT'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .          '# REPEAT K=1,NPG                                           '
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .          '#  UVAR(1,I) .............                                 '
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .          '#    ...............              UVAR(NUVAR,I)            '
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .          '#---------------------- END REPEAT ------------------------'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)') DELIMIT
                CALL STRS_TXT50(LINE,100)
              ENDIF  !  IF (IZIPSTRS == 0)
              IPRT0=IPRT
            ENDIF  !  IF (IPRT /= IPRT0)
c
            ID     = NINT(WAP0(J+3))
            NLAY   = NINT(WAP0(J+4))
cc            NPTT   = NINT(WAP0(J+5))
            NPTG   = NINT(WAP0(J+5))
            J = J + 5
c
            DO IL=1,NLAY
              IC = NINT(WAP0(J+1))
              J = J + 1
              IMAT = NINT(WAP0(J+1))
              J = J + 1
              NPTT = NINT(WAP0(J+1))
              J = J + 1
              DO II=1,IC
                NVAR_RUPT = NINT(WAP0(J+1))
                J = J + 1
                IRUPT = NINT(WAP0(J+1))
                J = J + 1
                IRUPT_TYPE = NINT(WAP0(J+1))
                J = J + 1
!
                IF (IRUPT == 0) CYCLE
!
                IF (IZIPSTRS == 0) THEN
                  WRITE(IUGEO,'(9I10)') ID,NLAY,NPTG,NPTT,IL,IRUPT,IRUPT_TYPE,NVAR_RUPT,
     .                                  IMAT
                ELSE
                  WRITE(LINE,'(9I10)') ID,NLAY,NPTG,NPTT,IL,IRUPT,IRUPT_TYPE,NVAR_RUPT,
     .                                 IMAT
                  CALL STRS_TXT50(LINE,100)
                ENDIF
                IF (IRUPT /= 0 .AND. NVAR_RUPT /= 0) THEN
                  IF (IZIPSTRS == 0) THEN
                    DO IT=1,NPTT
                      DO IPG=1,NPTG
                        WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + L),L=1,NVAR_RUPT)
                        J = J + NVAR_RUPT
                      ENDDO
                    ENDDO
                  ELSE
                    DO IT=1,NPTT
                      DO IPG=1,NPTG
                        CALL TAB_STRS_TXT50(WAP0(1),NVAR_RUPT,J,SIZP0,3)
                        J = J + NVAR_RUPT
                      ENDDO
                    ENDDO
                  ENDIF
                ENDIF  !  IF (IRUPT /= 0 .AND. NVAR_RUPT /= 0)
              ENDDO  !  DO II=1,IC
            ENDDO  !  DO IL=1,NLAY
          ENDDO  !  DO N=1,STAT_NUMELTG_G
        ENDIF  !  IF (ISPMD == 0.AND.LEN > 0)
C
        RETURN
      END
