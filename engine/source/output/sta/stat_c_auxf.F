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
Chd|  STAT_C_AUXF                   source/output/sta/stat_c_auxf.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        SPMD_RGATHER9_DP              source/mpi/interfaces/spmd_outp.F
Chd|        SPMD_STAT_PGATHER             source/mpi/output/spmd_stat.F 
Chd|        STRS_TXT50                    source/output/sta/sta_txt.F   
Chd|        TAB_STRS_TXT50                source/output/sta/sta_txt.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE STAT_C_AUXF(ELBUF_TAB ,IPARG ,IPM ,IGEO ,IXC ,
     2                       IXTG  ,WA,WAP0 ,IPARTC, IPARTTG,
     3                       IPART_STATE,STAT_INDXC,STAT_INDXTG,SIZP0)
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
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr01_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SIZLOC,SIZP0
      INTEGER IXC(NIXC,*),IXTG(NIXTG,*),
     .        IPARG(NPARG,*),IPM(NPROPMI,*),IGEO(NPROPGI,*),
     .        IPARTC(*), IPARTTG(*), IPART_STATE(*),
     .        STAT_INDXC(*), STAT_INDXTG(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      double precision WA(*),WAP0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,N,J,K,JJ,LEN, IOFF,
     .        NG, NEL, NFT, ITY, LFT,LLT, MLW, ID, IPRT0,IPRT,IE, 
     .        NPG,NPT,NPTR,NPTS,NPTT,NLAY,IR,IS,IT,IPT,IL,
     .        IVAR,NUVAR,MY_NUVAR,NPT_ALL,IGTYP
      INTEGER PTWA(MAX(STAT_NUMELC ,STAT_NUMELTG)),
     .        PTWA_P0(0:MAX(1,STAT_NUMELC_G,STAT_NUMELTG_G))
      double precision   
     .   THK, EM, EB, H1, H2, H3
      CHARACTER*100 DELIMIT,LINE
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY     
      my_real, DIMENSION(:)  ,POINTER :: UVAR,SIGA,SIGB,SIGC 
C-----------------------------------------------
      DATA DELIMIT(1:60)
     ./'#---1----|----2----|----3----|----4----|----5----|----6----|'/
      DATA DELIMIT(61:100)
     ./'----7----|----8----|----9----|----10---|'/
C-----------------------------------------------
C     4-NODE SHELLS
C-----------------------------------------------
      JJ = 0
      IF (STAT_NUMELC==0) GOTO 200
C
      IE=0
      DO NG=1,NGROUP
        ITY = IPARG(5,NG)
        IF (ITY == 3) THEN
          GBUF => ELBUF_TAB(NG)%GBUF   
          MLW   = IPARG(1,NG)
          NEL   = IPARG(2,NG)
          NFT   = IPARG(3,NG)
          IGTYP = IPARG(38,NG)
          NPTR = ELBUF_TAB(NG)%NPTR    
          NPTS = ELBUF_TAB(NG)%NPTS    
          NPTT = ELBUF_TAB(NG)%NPTT    
          NLAY = ELBUF_TAB(NG)%NLAY
          NPG  = NPTR*NPTS
          NPT  = NLAY*NPTT 
          LFT=1
          LLT=NEL
C
C pre counting of all NPTT (especially for PID_51)
C
          IF (IGTYP == 51 .OR. IGTYP == 52) THEN
            NPT_ALL = 0
            DO IL=1,NLAY
              NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(IL)%NPTT
            ENDDO
            NPT = MAX(1,NPT_ALL)
          ENDIF
c--------------------
          DO I=LFT,LLT
            N  = I + NFT
C
            IPRT=IPARTC(N)
            IF (IPART_STATE(IPRT)==0) CYCLE
C
            JJ = JJ + 1
            IF (MLW /= 0 .AND. MLW /= 13) THEN
              WA(JJ) = GBUF%OFF(I)
            ELSE
              WA(JJ) = 0
            ENDIF
            JJ = JJ + 1
            WA(JJ) = IPRT
            JJ = JJ + 1
            WA(JJ) = IXC(NIXC,N)
            JJ = JJ + 1
            WA(JJ) = NPT
            JJ = JJ + 1
            WA(JJ) = NPG
C
            IF (MLW == 36) THEN     ! STA/AUX contains only backstress
              MY_NUVAR = 0
              DO IL = 1,NLAY
                NUVAR = ELBUF_TAB(NG)%BUFLY(IL)%L_SIGB
                MY_NUVAR = MAX(MY_NUVAR, NUVAR)
              END DO
              JJ = JJ + 1
              WA(JJ) = MY_NUVAR
c
              IF (NUVAR > 0) THEN
                DO IS=1,NPTS
                  DO IR=1,NPTR                                                       
                    DO IL = 1,NLAY
                      BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                      NUVAR = ELBUF_TAB(NG)%BUFLY(IL)%L_SIGB
                      NPTT  = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                      DO IT=1,NPTT
                        SIGB => BUFLY%LBUF(IR,IS,IT)%SIGB
                        DO IVAR=1,NUVAR
                          JJ = JJ + 1
                          WA(JJ) = SIGB((IVAR-1)*NEL + I)
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO
                ENDDO
              ELSE
                DO IR=1,NPTR                                                       
                  DO IS=1,NPTS
                    DO IL = 1,NLAY
                      DO IT=1,NPTT
                        DO IVAR=1,MY_NUVAR
                          JJ = JJ + 1
                          WA(JJ) = ZERO
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO
                ENDDO
              END IF
C
            ELSEIF (MLW == 78) THEN     ! STA/AUX contains only backstress
              MY_NUVAR = ELBUF_TAB(NG)%BUFLY(1)%NVAR_MAT + 18  ! 3 x 6 for backstress
              JJ = JJ + 1
              WA(JJ) = MY_NUVAR 
c
              DO IS=1,NPTS
                DO IR=1,NPTR                                                       
                 DO IL = 1,NLAY
                   BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                   NUVAR = BUFLY%NVAR_MAT
                   NPTT  = BUFLY%NPTT
                    DO IT=1,NPTT
                      LBUF => BUFLY%LBUF(IR,IS,IT)
                      UVAR => BUFLY%MAT(IR,IS,IT)%VAR
                      SIGA => LBUF%SIGA
                      SIGB => LBUF%SIGB
                      SIGC => LBUF%SIGC
                      DO IVAR=1,NUVAR
                        JJ = JJ + 1
                        WA(JJ) = UVAR((IVAR-1)*NEL + I)
                      ENDDO
                      DO IVAR=1,BUFLY%L_SIGA
                        JJ = JJ + 1
                        WA(JJ) = SIGA((IVAR-1)*NEL + I)
                      ENDDO
                      DO IVAR=1,BUFLY%L_SIGB
                        JJ = JJ + 1
                        WA(JJ) = SIGB((IVAR-1)*NEL + I)
                      ENDDO
                      DO IVAR=1,BUFLY%L_SIGC
                        JJ = JJ + 1
                        WA(JJ) = SIGC((IVAR-1)*NEL + I)
                      ENDDO
                    ENDDO
                  ENDDO
                ENDDO
              ENDDO  ! DO IL = 1,NLAY
C
            ELSEIF (MLW == 87) THEN     ! STA/AUX contains only backstress
              BUFLY => ELBUF_TAB(NG)%BUFLY(1)
              MY_NUVAR = BUFLY%NVAR_MAT + BUFLY%L_SIGB
              JJ = JJ + 1
              WA(JJ) = MY_NUVAR 
c
              DO IS=1,NPTS
                DO IR=1,NPTR                                                       
                 DO IL = 1,NLAY
                   BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                   NUVAR = BUFLY%NVAR_MAT
                   NPTT  = BUFLY%NPTT
                    DO IT=1,NPTT
                      LBUF => BUFLY%LBUF(IR,IS,IT)
                      UVAR => BUFLY%MAT(IR,IS,IT)%VAR
                      SIGB => LBUF%SIGB
                      DO IVAR=1,NUVAR
                        JJ = JJ + 1
                        WA(JJ) = UVAR((IVAR-1)*NEL + I)
                      ENDDO
                      DO IVAR=1,BUFLY%L_SIGB
                        JJ = JJ + 1
                        WA(JJ) = SIGB((IVAR-1)*NEL + I)
                      ENDDO
                    ENDDO
                  ENDDO
                ENDDO
              ENDDO  ! DO IL = 1,NLAY
c
            ELSE IF (MLW >= 28 .and. MLW /= 32) THEN
              MY_NUVAR = IPM(8,IXC(1,N))
              JJ = JJ + 1
              WA(JJ) = MY_NUVAR
C
              IF (NLAY > 1) THEN    ! PID11
                DO IS=1,NPTS
                  DO IR=1,NPTR                                                         
                    DO IL = 1,NLAY
                      NUVAR = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT
                      NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                      DO IT=1,NPTT
                        UVAR => ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)%VAR
                        DO IVAR=1,MY_NUVAR
                          JJ = JJ + 1
                          WA(JJ) = UVAR((IVAR-1)*NEL + I)
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO
                ENDDO  ! DO IL = 1,NLAY
              ELSE   !  NLAY == 1 ->  PID1
                NPTT = ELBUF_TAB(NG)%BUFLY(1)%NPTT
                DO IS=1,NPTS       
                  DO IR=1,NPTR                                                           
                    DO IT=1,NPTT    
                      UVAR => ELBUF_TAB(NG)%BUFLY(1)%MAT(IR,IS,IT)%VAR
                      DO IVAR=1,MY_NUVAR
                        JJ = JJ + 1
                        WA(JJ) = UVAR((IVAR-1)*NEL + I)
                      ENDDO      
                    ENDDO
                  ENDDO
                ENDDO
              ENDIF ! NLAY 
            ELSE    ! Not User law
              MY_NUVAR = 0
              JJ = JJ + 1
              WA(JJ) = MY_NUVAR
            ENDIF
c--------------------
            IE=IE+1
C         pointeur de fin de zone dans WA
            PTWA(IE)=JJ
          ENDDO  !  DO I=LFT,LLT
        ENDIF  !  IF (ITY == 3)
      ENDDO  !  DO NG=1,NGROUP
C
 200  CONTINUE
C
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
      END IF
C
      IF (ISPMD == 0.AND.LEN > 0) THEN
        IPRT0=0
        DO N=1,STAT_NUMELC_G
C         retrouve le nieme elt dans l'ordre d'id croissant
          K=STAT_INDXC(N)
C         retrouve l'adresse dans WAP0
          J=PTWA_P0(K-1)
C
          IOFF  = NINT(WAP0(J + 1))
          MY_NUVAR = NINT(WAP0(J + 6))
C
          IF (IOFF >= 1 .AND. MY_NUVAR /= 0) THEN
            IPRT = NINT(WAP0(J + 2)) 
            IF (IPRT /= IPRT0) THEN
              IF (IZIPSTRS == 0) THEN
                WRITE(IUGEO,'(A)') DELIMIT
                WRITE(IUGEO,'(A)')'/INISHE/AUX'
                WRITE(IUGEO,'(A)')
     .'#------------------------ REPEAT --------------------------' 
                WRITE(IUGEO,'(A)')
     .      '#  SHELLID       NPT       NPG      NVAR' 
                WRITE(IUGEO,'(A/A)')
     .'# THE CONTENT OF THE FOLLOWING CARDS WILL NOT BE DISCLOSED',
     .'# THEY MUST NOT BE CHANGED.'
                WRITE(IUGEO,'(A)')
     .'#---------------------- END REPEAT ------------------------' 
                WRITE(IUGEO,'(A)') DELIMIT
              ELSE
                WRITE(LINE,'(A)') DELIMIT
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'/INISHE/AUX'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .'#------------------------ REPEAT --------------------------'
                CALL STRS_TXT50(LINE,100) 
                WRITE(LINE,'(A)')
     .      '#  SHELLID       NPT       NPG      NVAR' 
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .'# THE CONTENT OF THE FOLLOWING CARDS WILL NOT BE DISCLOSED'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .'# THEY MUST NOT BE CHANGED.'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .'#---------------------- END REPEAT ------------------------' 
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)') DELIMIT
                CALL STRS_TXT50(LINE,100)
              ENDIF  !  IF (IZIPSTRS == 0)
              IPRT0=IPRT
            ENDIF  ! IF (IPRT /= IPRT0)
            ID  = NINT(WAP0(J + 3)) 
            NPT = NINT(WAP0(J + 4)) 
            NPG = NINT(WAP0(J + 5)) 
            MY_NUVAR = NINT(WAP0(J + 6))
            J = J + 6
            IF (IZIPSTRS == 0) THEN
              WRITE(IUGEO,'(4I10)')ID,NPT,NPG,MY_NUVAR
            ELSE
              WRITE(LINE,'(4I10)')ID,NPT,NPG,MY_NUVAR
              CALL STRS_TXT50(LINE,100)
            ENDIF
            DO JJ=1,NPT*NPG
              IF (IZIPSTRS == 0) THEN
                WRITE(IUGEO,'(1P5E20.13)')(WAP0(J + K),K=1,MY_NUVAR)
              ELSE
                CALL TAB_STRS_TXT50(WAP0(1),MY_NUVAR,J,SIZP0,5)
              ENDIF
              J=J+MY_NUVAR
            ENDDO
          ENDIF  !  IF (IOFF == 1 .AND. MY_NUVAR /= 0)
        ENDDO  !  DO N=1,STAT_NUMELC_G		    
      ENDIF  !  IF (ISPMD == 0.AND.LEN > 0)
C-----------------------------------------------
C     3-NODE SHELLS
C-----------------------------------------------
      JJ = 0
      IF (STAT_NUMELTG==0) GOTO 300
C
      IE=0
C
      DO NG=1,NGROUP
        ITY = IPARG(5,NG)
        IF (ITY == 7) THEN
          GBUF => ELBUF_TAB(NG)%GBUF   
          MLW   = IPARG(1,NG)
          NEL   = IPARG(2,NG)
          NFT   = IPARG(3,NG)
          IGTYP = IPARG(38,NG)
          NPTR = ELBUF_TAB(NG)%NPTR    
          NPTS = ELBUF_TAB(NG)%NPTS    
          NPTT = ELBUF_TAB(NG)%NPTT    
          NLAY = ELBUF_TAB(NG)%NLAY
          NPG  = NPTR*NPTS
          NPT  = NLAY*NPTT 
          LFT=1
          LLT=NEL
C
C pre counting of all NPTT (especially for PID_51)
C
          IF (IGTYP == 51 .OR. IGTYP == 52) THEN
            NPT_ALL = 0
            DO IL=1,NLAY
              NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(IL)%NPTT
            ENDDO
            NPT = MAX(1,NPT_ALL)
          ENDIF
c--------------------
          DO I=LFT,LLT
            N = I + NFT
C
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
            WA(JJ) = NPT
            JJ = JJ + 1
            WA(JJ) = NPG
c
            IF (MLW == 36) THEN     ! STA/AUX contains only backstress
              MY_NUVAR = 0
              DO IL = 1,NLAY
                NUVAR = ELBUF_TAB(NG)%BUFLY(IL)%L_SIGB
                MY_NUVAR = MAX(MY_NUVAR, NUVAR)
              END DO
              JJ = JJ + 1
              WA(JJ) = MY_NUVAR
c
              DO IL = 1,NLAY
                BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                NUVAR = BUFLY%L_SIGB
                NPTT  = BUFLY%NPTT
                IF (NUVAR > 0) THEN
                  DO IR=1,NPTR                                                       
                    DO IS=1,NPTS
                      DO IT=1,NPTT
                        SIGB => BUFLY%LBUF(IR,IS,IT)%SIGB
                        DO IVAR=1,NUVAR
                          JJ = JJ + 1
                          WA(JJ) = SIGB((IVAR-1)*NEL + I)
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO
                ELSE
                  DO IR=1,NPTR                                                       
                    DO IS=1,NPTS
                      DO IT=1,NPTT
                        DO IVAR=1,MY_NUVAR
                          JJ = JJ + 1
                          WA(JJ) = ZERO
                        ENDDO
                      ENDDO
                    ENDDO
                  ENDDO
                END IF
              ENDDO  ! DO IL = 1,NLAY
C
            ELSEIF (MLW == 78) THEN     ! STA/AUX contains only backstress
              MY_NUVAR = 0
              DO IL = 1,NLAY
                NUVAR = ELBUF_TAB(NG)%BUFLY(IL)%L_SIGB
                MY_NUVAR = MAX(MY_NUVAR, NUVAR)
              END DO
              MY_NUVAR = MY_NUVAR + 18   ! 3 x 6 for backstress
              JJ = JJ + 1
              WA(JJ) = MY_NUVAR 
c
              DO IS=1,NPTS
                DO IR=1,NPTR                                                       
                  DO IL = 1,NLAY
                    BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                    NUVAR = BUFLY%NVAR_MAT
                    NPTT  = BUFLY%NPTT
                    DO IT=1,NPTT
                      LBUF => BUFLY%LBUF(IR,IS,IT)
                      UVAR => BUFLY%MAT(IR,IS,IT)%VAR
                      SIGA => LBUF%SIGA
                      SIGB => LBUF%SIGB
                      SIGC => LBUF%SIGC
                      DO IVAR=1,NUVAR
                        JJ = JJ + 1
                        WA(JJ) = UVAR((IVAR-1)*NEL + I)
                      ENDDO
                      DO IVAR=1,BUFLY%L_SIGA
                        JJ = JJ + 1
                        WA(JJ) = SIGA((IVAR-1)*NEL + I)
                      ENDDO
                      DO IVAR=1,BUFLY%L_SIGB
                        JJ = JJ + 1
                        WA(JJ) = SIGB((IVAR-1)*NEL + I)
                      ENDDO
                      DO IVAR=1,BUFLY%L_SIGC
                        JJ = JJ + 1
                        WA(JJ) = SIGC((IVAR-1)*NEL + I)
                      ENDDO
                    ENDDO
                  ENDDO
                ENDDO
              ENDDO  ! DO IL = 1,NLAY
C
            ELSEIF (MLW == 87) THEN     ! STA/AUX contains only backstress
              MY_NUVAR = ELBUF_TAB(NG)%BUFLY(1)%NVAR_MAT + 12  ! 2 x 6 for backstress
              JJ = JJ + 1
              WA(JJ) = MY_NUVAR 
c
              DO IS=1,NPTS
                DO IR=1,NPTR                                                       
                 DO IL = 1,NLAY
                   BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
                   NUVAR = BUFLY%NVAR_MAT
                   NPTT  = BUFLY%NPTT
                    DO IT=1,NPTT
                      LBUF => BUFLY%LBUF(IR,IS,IT)
                      UVAR => BUFLY%MAT(IR,IS,IT)%VAR
                      SIGB => LBUF%SIGB
                      DO IVAR=1,NUVAR
                        JJ = JJ + 1
                        WA(JJ) = UVAR((IVAR-1)*NEL + I)
                      ENDDO
                      DO IVAR=1,BUFLY%L_SIGB
                        JJ = JJ + 1
                        WA(JJ) = SIGB((IVAR-1)*NEL + I)
                      ENDDO
                    ENDDO
                  ENDDO
                ENDDO
              ENDDO  ! DO IL = 1,NLAY
c
            ELSE IF (MLW >= 28 .and. MLW /= 32) THEN
              MY_NUVAR = IPM(8,IXTG(1,N))
              JJ = JJ + 1
              WA(JJ) = MY_NUVAR
c
              IF (NLAY > 1) THEN    ! PID11
                DO IR=1,NPTS                                                        
                  DO IS=1,NPTR
                    DO IL = 1,NLAY
                      NUVAR = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT
                      NPTT = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
                      DO IT=1,NPTT
                        UVAR => ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)%VAR
                        DO IVAR=1,MY_NUVAR
                          JJ = JJ + 1
                          WA(JJ) = UVAR((IVAR-1)*NEL + I)
                        ENDDO
                      ENDDO     
                    ENDDO      
                  ENDDO
                ENDDO
              ELSE   !  NLAY ==1 ->  PID1
                NPTT = ELBUF_TAB(NG)%BUFLY(1)%NPTT
                DO IS=1,NPTS       
                  DO IR=1,NPTR                                                           
                    DO IT=1,NPTT    
                      UVAR => ELBUF_TAB(NG)%BUFLY(1)%MAT(IR,IS,IT)%VAR
                      DO IVAR=1,MY_NUVAR
                        JJ = JJ + 1
                        WA(JJ) = UVAR((IVAR-1)*NEL + I)
                      ENDDO      
                    ENDDO
                  ENDDO
                ENDDO
              ENDIF ! NLAY 
            ELSE  ! Not User law
              MY_NUVAR = 0
              JJ = JJ + 1
              WA(JJ) = MY_NUVAR
            ENDIF
c--------------------
            IE=IE+1
C         pointeur de fin de zone
            PTWA(IE)=JJ
          ENDDO  !  DO I=LFT,LLT
        ENDIF  !  IF (ITY == 7) THEN
      ENDDO  !  DO NG=1,NGROUP
C
 300  CONTINUE
C
      IF (NSPMD == 1) THEN
        LEN=JJ
        DO J=1,LEN
          WAP0(J)=WA(J)
        ENDDO
        PTWA_P0(0)=0
        DO N=1,STAT_NUMELTG
          PTWA_P0(N)=PTWA(N)
        ENDDO
      ELSE
C       construit les pointeurs dans le tableau global WAP0
        CALL SPMD_STAT_PGATHER(PTWA,STAT_NUMELTG,PTWA_P0,STAT_NUMELTG_G)
        LEN = 0
        CALL SPMD_RGATHER9_DP(WA,JJ,WAP0,SIZP0,LEN)
      ENDIF
C
      IF (ISPMD == 0.AND.LEN > 0) THEN
C
        IPRT0=0
        DO N=1,STAT_NUMELTG_G
C         retrouve le nieme elt dans l'ordre d'id croissant
          K=STAT_INDXTG(N)
C         retrouve l'adresse dans WAP0
          J=PTWA_P0(K-1)
C
          IOFF  = NINT(WAP0(J + 1))
          MY_NUVAR = NINT(WAP0(J + 6))
C
          IF (IOFF >= 1 .AND. MY_NUVAR /= 0) THEN
            IPRT = NINT(WAP0(J + 2)) 
            IF (IPRT /= IPRT0) THEN
              IF (IZIPSTRS == 0) THEN
                WRITE(IUGEO,'(A)') DELIMIT
                WRITE(IUGEO,'(A)')'/INISH3/AUX'
                WRITE(IUGEO,'(A)')
     .'#------------------------ REPEAT --------------------------' 
                WRITE(IUGEO,'(A)')
     .      '#   SH3NID       NPT       NPG      NVAR' 
                WRITE(IUGEO,'(A/A)')
     .'# THE CONTENT OF THE FOLLOWING CARDS WILL NOT BE DISCLOSED',
     .'# THEY MUST NOT BE CHANGED.'
                WRITE(IUGEO,'(A)')
     .'#---------------------- END REPEAT ------------------------' 
                WRITE(IUGEO,'(A)') DELIMIT
              ELSE
                WRITE(LINE,'(A)') DELIMIT
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')'/INISH3/AUX'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .'#------------------------ REPEAT --------------------------' 
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .      '#   SH3NID       NPT       NPG      NVAR'
                CALL STRS_TXT50(LINE,100) 
                WRITE(LINE,'(A)')
     .'# THE CONTENT OF THE FOLLOWING CARDS WILL NOT BE DISCLOSED'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .'# THEY MUST NOT BE CHANGED.'
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)')
     .'#---------------------- END REPEAT ------------------------' 
                CALL STRS_TXT50(LINE,100)
                WRITE(LINE,'(A)') DELIMIT
                CALL STRS_TXT50(LINE,100)
              ENDIF  !  IF (IZIPSTRS == 0)
              IPRT0=IPRT
            ENDIF  !  IF (IPRT /= IPRT0)
            ID  = NINT(WAP0(J + 3)) 
            NPT = NINT(WAP0(J + 4)) 
            NPG = NINT(WAP0(J + 5)) 
            MY_NUVAR = NINT(WAP0(J + 6))
            J = J + 6
            IF (IZIPSTRS == 0) THEN
              WRITE(IUGEO,'(4I10)')ID,NPT,NPG,MY_NUVAR
            ELSE
              WRITE(LINE,'(4I10)')ID,NPT,NPG,MY_NUVAR
              CALL STRS_TXT50(LINE,100)
            ENDIF
            DO JJ=1,NPT*NPG
              IF (IZIPSTRS == 0) THEN
                WRITE(IUGEO,'(1P5E20.13)')(WAP0(J + K),K=1,MY_NUVAR)
              ELSE
                CALL TAB_STRS_TXT50(WAP0(1),MY_NUVAR,J,SIZP0,5)
              ENDIF
              J=J+MY_NUVAR
            ENDDO
          ENDIF  !  IF (IOFF == 1 .AND. MY_NUVAR /= 0)
        ENDDO  !  DO N=1,STAT_NUMELTG_G
      ENDIF  !  IF (ISPMD == 0.AND.LEN > 0)
c-----------
      RETURN
      END
