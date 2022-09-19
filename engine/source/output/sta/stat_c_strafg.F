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
Chd|  STAT_C_STRAFG                 source/output/sta/stat_c_strafg.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        GET_Q4L                       source/output/sta/stat_c_strafg.F
Chd|        GET_T3L                       source/output/sta/stat_c_strafg.F
Chd|        LAYINI                        source/elements/shell/coque/layini.F
Chd|        SHELL2G                       source/output/sta/stat_c_strafg.F
Chd|        SPMD_RGATHER9_DP              source/mpi/interfaces/spmd_outp.F
Chd|        SPMD_STAT_PGATHER             source/mpi/output/spmd_stat.F 
Chd|        STRS_TXT50                    source/output/sta/sta_txt.F   
Chd|        TAB_STRS_TXT50                source/output/sta/sta_txt.F   
Chd|        DRAPE_MOD                     share/modules/drape_mod.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE STAT_C_STRAFG(ELBUF_TAB,
     1                        X    ,IPARG ,IPM ,IGEO ,IXC ,
     2                        IXTG  ,WA,WAP0 ,IPARTC, IPARTTG,
     3                      IPART_STATE,STAT_INDXC,STAT_INDXTG,THKE,SIZP0,
     4                       GEO   ,STACK,DRAPE_Q4,DRAPE_T3)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE STACK_MOD
      USE DRAPE_MOD
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
      my_real
     .   THKE(*),X(3,*),GEO(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
      TYPE (DRAPE_)  :: DRAPE_Q4, DRAPE_T3
      double precision WA(*),WAP0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,II,JJ,LEN, IOFF, NG, NEL, NFT, ITY, LFT, NPT,
     .    LLT, MLW, ISTRAIN,ID, IPRT0, IPRT,NPG,IPG,IE,NPTR,NPTS,G_STRA,
     .    ITHK,KK(8),NF1,IGTYP,IREL,IHBE,NLAY,IBID0,MAT_1,PID_1,ILAY,NF3
      INTEGER PTWA(MAX(STAT_NUMELC ,STAT_NUMELTG)),
     .        PTWA_P0(0:MAX(1,STAT_NUMELC_G,STAT_NUMELTG_G))
      double precision
     .   THK, EM, EB, H1, H2, H3
      CHARACTER*100 DELIMIT,LINE
      TYPE(G_BUFEL_)  ,POINTER :: GBUF
      TYPE(L_BUFEL_)  ,POINTER :: LBUF
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY
      INTEGER LAYNPT_MAX,LAY_MAX,ISUBSTACK,IPT_ALL,NPTT,IT,IPT,NPT_ALL,MPT
      PARAMETER (LAYNPT_MAX = 10)
      PARAMETER (LAY_MAX = 100)
      INTEGER MATLY(MVSIZ*LAY_MAX)
      my_real,
     .   DIMENSION(:),POINTER  :: STRAIN
      my_real
     .   QT(9,MVSIZ),STRAING(6),ZH,THKP ,THK0(MVSIZ)
      my_real
     .   THKLY(MVSIZ*LAY_MAX*LAYNPT_MAX),POSLY(MVSIZ,LAY_MAX*LAYNPT_MAX),
     .   THK_LY(MVSIZ,LAY_MAX*LAYNPT_MAX)
C-----------------------------------------------
      DATA DELIMIT(1:60)
     ./'#---1----|----2----|----3----|----4----|----5----|----6----|'/
      DATA DELIMIT(61:100)
     ./'----7----|----8----|----9----|----10---|'/
C-----------------------------------------------
C     4-NODE SHELLS
C-----------------------------------------------
      JJ = 0
      IF(STAT_NUMELC==0) GOTO 200

      IE=0
      DO NG=1,NGROUP
       ITY   =IPARG(5,NG)
       IF (ITY == 3) THEN
         GBUF => ELBUF_TAB(NG)%GBUF
         MLW   =IPARG(1,NG)
         NEL   =IPARG(2,NG)
         NFT   =IPARG(3,NG)
         NPT  = IPARG(6,NG)
         ITHK  =IPARG(28,NG)
         NPTR = ELBUF_TAB(NG)%NPTR
         NPTS = ELBUF_TAB(NG)%NPTS
         NLAY = ELBUF_TAB(NG)%NLAY
         IHBE  =IPARG(23,NG)
         IGTYP= IPARG(38,NG)
         ISUBSTACK=IPARG(71,NG)
         NPG  = NPTR*NPTS
         LFT=1
         LLT=NEL
         G_STRA = GBUF%G_STRA
         NF1 = NFT+1
         IF (IHBE>10.OR.IGTYP==16.OR.ISHFRAM ==0) THEN
           IREL=0
         ELSEIF (ISHFRAM ==1) THEN
           IREL=2
         ELSE
           IREL=1
         END IF
!
         DO J=1,8  ! lenght max of GBUF%G_STRA = 8
           KK(J) = NEL*(J-1)
         ENDDO
!
         IBID0 = 0
         MAT_1 = IXC(1,NF1)
         PID_1 = IXC(6,NF1)
         IF (ITHK >0 ) THEN
          THK0(LFT:LLT) = GBUF%THK(LFT:LLT)
         ELSE
          THK0(LFT:LLT) = THKE(LFT+NFT:LLT+NFT)
         END IF
         CALL LAYINI(ELBUF_TAB(NG),LFT      ,LLT      ,GEO      ,IGEO    ,
     .              MAT_1    ,PID_1    ,THKLY    ,MATLY    ,POSLY    ,
     .              IGTYP    ,IBID0    ,IBID0    ,NLAY     ,NPT      ,
     .              ISUBSTACK,STACK    ,DRAPE_Q4 ,NFT      ,THK0     ,
     .              NEL      ,THK_LY   )
         CALL GET_Q4L(LFT ,LLT ,IXC(1,NF1),X ,GBUF%OFF,IREL ,QT )
          NPT_ALL = 0
          DO ILAY=1,NLAY
            NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(ILAY)%NPTT
          ENDDO
          MPT  = MAX(1,NPT_ALL)
          IF (NPT==0) MPT=0
c--------------------
         DO I=LFT,LLT
          N  = I + NFT

          IPRT=IPARTC(N)
          IF(IPART_STATE(IPRT)==0)CYCLE

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
C----         
          WA(JJ) = MPT
          JJ = JJ + 1
          WA(JJ) = NPG
          JJ = JJ + 1
          IF (MLW /= 0 .AND. MLW /= 13) THEN
           WA(JJ) = THK0(I)
          ELSE
            WA(JJ) = ZERO
          ENDIF
          THKP = WA(JJ)
c         Strain in Gauss points
          IF (MLW == 0 .or. MLW == 13) THEN
            DO IPG=1,NPG
              DO J=1,14
                JJ    = JJ + 1
                WA(JJ)=ZERO
              END DO
            END DO
          ELSEIF (NPT==0 .AND. G_STRA /= 0) THEN
            IF (NPG > 1) THEN
              STRAIN => GBUF%STRPG
            ELSE
              STRAIN => GBUF%STRA
            ENDIF
C------first point Z=0  7 real  to print npg w/ QEPH     
            DO IPG=1,NPG
              K = (IPG-1)*NEL*G_STRA
              STRAING(1:2)=STRAIN(KK(1:2)+I+K)
              STRAING(3:5)=HALF*STRAIN(KK(3:5)+I+K)
              CALL SHELL2G(STRAING,QT(1,I))
C
              DO J=1,6
                JJ    = JJ + 1
                WA(JJ) = STRAING(J)
              END DO
               JJ    = JJ + 1
               WA(JJ) = ZERO
            END DO
C------2nd point Z=0.5-> 1.0(LSD) 7 real      
            DO IPG=1,NPG
              K = (IPG-1)*NEL*G_STRA
              ZH = 1.0*THKP
              STRAING(1:3)=STRAIN(KK(1:3)+I+K)+ZH*STRAIN(KK(6:8)+I+K)
              STRAING(3)=HALF*STRAING(3)
              STRAING(4:5)=HALF*STRAIN(KK(4:5)+I+K)
              CALL SHELL2G(STRAING,QT(1,I))
C
              DO J=1,6
                JJ    = JJ + 1
                WA(JJ) = STRAING(J)
              END DO
               JJ    = JJ + 1
               WA(JJ) = ONE
            END DO
          ELSEIF (G_STRA /= 0) THEN
            IF (NPG > 1) THEN
              STRAIN => GBUF%STRPG
            ELSE
              STRAIN => GBUF%STRA
            ENDIF
            IPT_ALL = 0
            DO ILAY =1,NLAY
              BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
              NPTT   = BUFLY%NPTT
              DO IT=1,NPTT
                IPT = IPT_ALL + IT
C--   
                DO IPG=1,NPG
                  K = (IPG-1)*NEL*G_STRA
                  ZH = POSLY(I,IPT)*THKP
                  STRAING(1:3)=STRAIN(KK(1:3)+I+K)+ZH*STRAIN(KK(6:8)+I+K)
                  STRAING(3)=HALF*STRAING(3)
                  STRAING(4:5)=HALF*STRAIN(KK(4:5)+I+K)
                  CALL SHELL2G(STRAING,QT(1,I))
C               
                  DO J=1,6
                    JJ    = JJ + 1
                    WA(JJ) = STRAING(J)
                  END DO
                   JJ    = JJ + 1
                   WA(JJ) = POSLY(I,IPT)*TWO
                END DO
              END DO !IT=1,NPTT
              IPT_ALL = IPT_ALL + NPTT
            END DO !ILAY =1,NLAY
          END IF

          IE=IE+1
C         pointeur de fin de zone dans WA
          PTWA(IE)=JJ
c
         ENDDO  ! I=LFT,LLT
       END IF   ! ITY==3
      ENDDO     ! NG=1,NGROUP

 200  CONTINUE

      IF(NSPMD == 1)THEN
        PTWA_P0(0)=0
        DO N=1,STAT_NUMELC
          PTWA_P0(N)=PTWA(N)
        END DO
        LEN=JJ
        DO J=1,LEN
          WAP0(J)=WA(J)
        END DO
      ELSE
C       construit les pointeurs dans le tableau global WAP0
        CALL SPMD_STAT_PGATHER(PTWA,STAT_NUMELC,PTWA_P0,STAT_NUMELC_G)
        LEN = 0
        CALL SPMD_RGATHER9_DP(WA,JJ,WAP0,SIZP0,LEN)
      END IF

      IF(ISPMD==0.AND.LEN>0) THEN

        IPRT0=0
        DO N=1,STAT_NUMELC_G

C         retrouve le nieme elt dans l'ordre d'id croissant
          K=STAT_INDXC(N)
C         retrouve l'adresse dans WAP0
          J=PTWA_P0(K-1)

          IOFF  = NINT(WAP0(J + 1))
          IF(IOFF >= 1)THEN
            IPRT  = NINT(WAP0(J + 2))
            IF(IPRT /= IPRT0)THEN
             IF (IZIPSTRS == 0) THEN
               WRITE(IUGEO,'(A)') DELIMIT
               WRITE(IUGEO,'(A)')'/INISHE/STRA_F/GLOB'
               WRITE(IUGEO,'(A)')
     .'#------------------------ REPEAT --------------------------'
               WRITE(IUGEO,'(A)')
     .      '#  SHELLID       NPT       NPG                 THK'
               WRITE(IUGEO,'(A/A/A)')
     .'# REPEAT I=1,NPG :',
     .'#   E11, E22, E33,',
     .'#   E12, E23, E31, T,'
               WRITE(IUGEO,'(A)')
     .'#---------------------- END REPEAT ------------------------'
                WRITE(IUGEO,'(A)') DELIMIT
             ELSE
               WRITE(LINE,'(A)') DELIMIT
               CALL STRS_TXT50(LINE,100)
               WRITE(LINE,'(A)')'/INISHE/STRA_F/GLOB'
               CALL STRS_TXT50(LINE,100)
               WRITE(LINE,'(A)')
     .'#------------------------ REPEAT --------------------------'
               CALL STRS_TXT50(LINE,100)
               WRITE(LINE,'(A)')
     .      '#  SHELLID       NPT       NPG                 THK'
               CALL STRS_TXT50(LINE,100)
               WRITE(LINE,'(A)')'# REPEAT I=1,NPG :'
               CALL STRS_TXT50(LINE,100)
               WRITE(LINE,'(A)')'#   E11, E22, E33,'
               CALL STRS_TXT50(LINE,100)
               WRITE(LINE,'(A)')'#   E12, E23, E31, T '
               CALL STRS_TXT50(LINE,100)
               WRITE(LINE,'(A)')
     .'#---------------------- END REPEAT ------------------------'
               CALL STRS_TXT50(LINE,100)
               WRITE(LINE,'(A)') DELIMIT
               CALL STRS_TXT50(LINE,100)
             ENDIF
              IPRT0=IPRT
            END IF
            ID    = NINT(WAP0(J + 3))
            NPT   = NINT(WAP0(J + 4))
            NPG   = NINT(WAP0(J + 5))
            THK   = WAP0(J + 6)
            J = J + 6
            IF (IZIPSTRS == 0) THEN
              WRITE(IUGEO,'(3I10,1PE20.13)')ID,NPT,NPG,THK
            ELSE
              WRITE(LINE,'(3I10,1PE20.13)')ID,NPT,NPG,THK
              CALL STRS_TXT50(LINE,100)
            ENDIF
            IF (NPT == 0) THEN
              DO IPG=1,NPG
                IF (IZIPSTRS == 0) THEN
                  WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=1,3)
                  WRITE(IUGEO,'(1P4E20.13)')(WAP0(J + K),K=4,7)
                ELSE
                  CALL TAB_STRS_TXT50(WAP0(1),3,J,SIZP0,3)
                  CALL TAB_STRS_TXT50(WAP0(4),4,J,SIZP0,4)
                ENDIF
               J = J + 7
              END DO
C----- 2nd point            
              DO IPG=1,NPG
                IF (IZIPSTRS == 0) THEN
                  WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=1,3)
                  WRITE(IUGEO,'(1P4E20.13)')(WAP0(J + K),K=4,7)
                ELSE
                  CALL TAB_STRS_TXT50(WAP0(1),3,J,SIZP0,3)
                  CALL TAB_STRS_TXT50(WAP0(4),4,J,SIZP0,4)
                ENDIF
               J = J + 7
              END DO
            ELSE
              DO IT=1,NPT
                DO IPG=1,NPG
                  IF (IZIPSTRS == 0) THEN
                    WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=1,3)
                    WRITE(IUGEO,'(1P4E20.13)')(WAP0(J + K),K=4,7)
                  ELSE
                    CALL TAB_STRS_TXT50(WAP0(1),3,J,SIZP0,3)
                    CALL TAB_STRS_TXT50(WAP0(4),4,J,SIZP0,4)
                  ENDIF
                 J = J + 7
                END DO
              END DO
            ENDIF
          END IF
C
        ENDDO		
      ENDIF
C-----------------------------------------------
C     3-NODE SHELLS
C-----------------------------------------------
      JJ = 0
      IF (STAT_NUMELTG==0) GOTO 300
      IE=0

      DO NG=1,NGROUP
       ITY   =IPARG(5,NG)
       IF (ITY == 7) THEN
         GBUF => ELBUF_TAB(NG)%GBUF
         G_STRA = GBUF%G_STRA
         MLW   =IPARG(1,NG)
         NEL   =IPARG(2,NG)
         NFT   =IPARG(3,NG)
         NPT  = IPARG(6,NG)
         ITHK = IPARG(28,NG)
         IHBE  =IPARG(23,NG)
         IGTYP= IPARG(38,NG)
         ISUBSTACK=IPARG(71,NG)
         NPTR = ELBUF_TAB(NG)%NPTR
         NPTS = ELBUF_TAB(NG)%NPTS
         NLAY = ELBUF_TAB(NG)%NLAY
         NPG  = NPTR*NPTS
         LFT=1
         LLT=NEL
         NF1 = NFT+1
         IF (IHBE>=30) THEN
           IREL=0
         ELSE
           IREL=2
         END IF
!
         DO J=1,8  ! lenght max of GBUF%G_STRA = 8
           KK(J) = NEL*(J-1)
         ENDDO
!
         IBID0 = 0
         MAT_1 = IXTG(1,NF1)
         PID_1 = IXTG(NIXTG-1,NF1)
         IF (ITHK >0 ) THEN
          THK0(LFT:LLT) = GBUF%THK(LFT:LLT)
         ELSE
          NF3 = NFT+NUMELC
          THK0(LFT:LLT) = THKE(LFT+NF3:LLT+NF3)
         END IF
         CALL LAYINI(ELBUF_TAB(NG),LFT      ,LLT      ,GEO      ,IGEO    ,
     .              MAT_1    ,PID_1    ,THKLY    ,MATLY    ,POSLY    ,
     .              IGTYP    ,IBID0    ,IBID0    ,NLAY     ,NPT      ,
     .              ISUBSTACK,STACK    ,DRAPE_T3 ,NFT      ,THK0     ,
     .              NEL      ,THK_LY   )
         CALL GET_T3L(LFT    ,LLT    ,IXTG(1,NF1),X    ,GBUF%OFF,
     .                IREL   ,QT     )
          NPT_ALL = 0
          DO ILAY=1,NLAY
            NPT_ALL = NPT_ALL + ELBUF_TAB(NG)%BUFLY(ILAY)%NPTT
          ENDDO
          MPT  = MAX(1,NPT_ALL)
          IF (NPT==0) MPT=0
c--------------------
         DO I=LFT,LLT
          N  = I + NFT

          IPRT=IPARTTG(N)
          IF(IPART_STATE(IPRT)==0)CYCLE


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
          WA(JJ) = MPT
          JJ = JJ + 1
          WA(JJ) = NPG
          JJ = JJ + 1
          IF (MLW /= 0 .AND. MLW /= 13) THEN
            WA(JJ) = THK0(I)
          ELSE
            WA(JJ) = ZERO
          ENDIF
          THKP = WA(JJ)

c         Strain in Gauss points
          IF (MLW == 0 .or. MLW == 13) THEN
            DO IPG=1,NPG
              DO J=1,14
                JJ    = JJ + 1
                WA(JJ) = ZERO
              END DO
            END DO
          ELSEIF (NPT==0 .AND. G_STRA /= 0) THEN
            IF (NPG > 1) THEN
              STRAIN => GBUF%STRPG
            ELSE
              STRAIN => GBUF%STRA
            ENDIF
C------first point Z=0  7 real     
            DO IPG=1,NPG
              K = (IPG-1)*NEL*G_STRA
              STRAING(1:2)=STRAIN(KK(1:2)+I+K)
              STRAING(3:5)=HALF*STRAIN(KK(3:5)+I+K)
              CALL SHELL2G(STRAING,QT(1,I))
C
              DO J=1,6
                JJ    = JJ + 1
                WA(JJ) = STRAING(J)
              END DO
               JJ    = JJ + 1
               WA(JJ) = ZERO
            END DO
C------2nd point Z=0.5-> 1.0(LSD) 7 real      
            DO IPG=1,NPG
              K = (IPG-1)*NEL*G_STRA
              ZH = 1.0*THKP
              STRAING(1:3)=STRAIN(KK(1:3)+I+K)+ZH*STRAIN(KK(6:8)+I+K)
              STRAING(3)=HALF*STRAING(3)
              STRAING(4:5)=HALF*STRAIN(KK(4:5)+I+K)
              CALL SHELL2G(STRAING,QT(1,I))
C
              DO J=1,6
                JJ    = JJ + 1
                WA(JJ) = STRAING(J)
              END DO
               JJ    = JJ + 1
               WA(JJ) = ONE
            END DO
          ELSEIF (G_STRA > 0) THEN
            IF (NPG > 1) THEN
              STRAIN => GBUF%STRPG
            ELSE
              STRAIN => GBUF%STRA
            ENDIF
            IPT_ALL = 0
            DO ILAY =1,NLAY
              BUFLY => ELBUF_TAB(NG)%BUFLY(ILAY)
              NPTT   = BUFLY%NPTT
              DO IT=1,NPTT
                IPT = IPT_ALL + IT
C--   
                DO IPG=1,NPG
                  K = (IPG-1)*NEL*G_STRA
                  ZH = POSLY(I,IPT)*THKP
                  STRAING(1:3)=STRAIN(KK(1:3)+I+K)+ZH*STRAIN(KK(6:8)+I+K)
                  STRAING(3)=HALF*STRAING(3)
                  STRAING(4:5)=HALF*STRAIN(KK(4:5)+I+K)
                  CALL SHELL2G(STRAING,QT(1,I))
C               
                  DO J=1,6
                    JJ    = JJ + 1
                    WA(JJ) = STRAING(J)
                  END DO
                   JJ    = JJ + 1
                   WA(JJ) = POSLY(I,IPT)*TWO
                END DO
              END DO !IT=1,NPTT
              IPT_ALL = IPT_ALL + NPTT
            END DO !ILAY =1,NLAY
          END IF  ! IF (MLW == 0 .or. MLW == 13)

          IE=IE+1
C         pointeur de fin de zone
          PTWA(IE)=JJ
c
         ENDDO  ! I=LFT,LLT
       END IF   ! ITY==3
      ENDDO     ! NG=1,NGROUP

 300  CONTINUE

      IF(NSPMD == 1)THEN
        LEN=JJ
        DO J=1,LEN
          WAP0(J)=WA(J)
        END DO
        PTWA_P0(0)=0
        DO N=1,STAT_NUMELTG
          PTWA_P0(N)=PTWA(N)
        END DO
      ELSE
C       construit les pointeurs dans le tableau global WAP0
        CALL SPMD_STAT_PGATHER(PTWA,STAT_NUMELTG,PTWA_P0,STAT_NUMELTG_G)
        LEN = 0
        CALL SPMD_RGATHER9_DP(WA,JJ,WAP0,SIZP0,LEN)
      END IF

      IF(ISPMD==0.AND.LEN>0) THEN

        IPRT0=0
        DO N=1,STAT_NUMELTG_G

C         retrouve le nieme elt dans l'ordre d'id croissant
          K=STAT_INDXTG(N)
C         retrouve l'adresse dans WAP0
          J=PTWA_P0(K-1)

          IOFF  = NINT(WAP0(J + 1))
          IF(IOFF >= 1)THEN
            IPRT  = NINT(WAP0(J + 2))
            IF(IPRT /= IPRT0)THEN
             IF (IZIPSTRS == 0) THEN
              WRITE(IUGEO,'(A)') DELIMIT
              WRITE(IUGEO,'(A)')'/INISH3/STRA_F/GLOB'
              WRITE(IUGEO,'(A)')
     .'#------------------------ REPEAT --------------------------'
              WRITE(IUGEO,'(A)')
     .      '#   SH3NID       NPT       NPG                 THK'
             WRITE(IUGEO,'(A/A/A)')
     .'# REPEAT I=1,NPG :',
     .'#   E11, E22, E33,',
     .'#   E12, E23, E31, T '
              WRITE(IUGEO,'(A)')
     .'#---------------------- END REPEAT ------------------------'
              WRITE(IUGEO,'(A)') DELIMIT
             ELSE
              WRITE(LINE,'(A)') DELIMIT
              CALL STRS_TXT50(LINE,100)
              WRITE(LINE,'(A)')'/INISH3/STRA_F/GLOB'
              CALL STRS_TXT50(LINE,100)
              WRITE(LINE,'(A)')
     .'#------------------------ REPEAT --------------------------'
              CALL STRS_TXT50(LINE,100)
              WRITE(LINE,'(A)')
     .      '#   SH3NID       NPT       NPG                 THK'
              CALL STRS_TXT50(LINE,100)
              WRITE(LINE,'(A)')'# REPEAT I=1,NPG :'
              CALL STRS_TXT50(LINE,100)
              WRITE(LINE,'(A)')'#   E11, E22, E33,'
              CALL STRS_TXT50(LINE,100)
              WRITE(LINE,'(A)')'#   E12, E23, E31, T '
              CALL STRS_TXT50(LINE,100)
              WRITE(LINE,'(A)')
     .'#---------------------- END REPEAT ------------------------'
              CALL STRS_TXT50(LINE,100)
              WRITE(LINE,'(A)') DELIMIT
              CALL STRS_TXT50(LINE,100)
             END IF
             IPRT0=IPRT
            END IF
            ID    = NINT(WAP0(J + 3))
            NPT   = NINT(WAP0(J + 4))
            NPG   = NINT(WAP0(J + 5))
            THK   = WAP0(J + 6)
            J = J + 6
            IF (IZIPSTRS == 0) THEN
              WRITE(IUGEO,'(3I10,1PE20.13)')ID,NPT,NPG,THK
            ELSE
              WRITE(LINE,'(3I10,1PE20.13)')ID,NPT,NPG,THK
              CALL STRS_TXT50(LINE,100)
            ENDIF
            IF (NPT == 0) THEN
              DO IPG=1,NPG
                IF (IZIPSTRS == 0) THEN
                  WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=1,3)
                  WRITE(IUGEO,'(1P4E20.13)')(WAP0(J + K),K=4,7)
                ELSE
                  CALL TAB_STRS_TXT50(WAP0(1),3,J,SIZP0,3)
                  CALL TAB_STRS_TXT50(WAP0(4),4,J,SIZP0,4)
                ENDIF
               J = J + 7
              END DO
C----- 2nd point            
              DO IPG=1,NPG
                IF (IZIPSTRS == 0) THEN
                  WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=1,3)
                  WRITE(IUGEO,'(1P4E20.13)')(WAP0(J + K),K=4,7)
                ELSE
                  CALL TAB_STRS_TXT50(WAP0(1),3,J,SIZP0,3)
                  CALL TAB_STRS_TXT50(WAP0(4),4,J,SIZP0,4)
                ENDIF
               J = J + 7
              END DO
            ELSE
              DO IT=1,NPT
                DO IPG=1,NPG
                  IF (IZIPSTRS == 0) THEN
                    WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=1,3)
                    WRITE(IUGEO,'(1P4E20.13)')(WAP0(J + K),K=4,7)
                  ELSE
                    CALL TAB_STRS_TXT50(WAP0(1),3,J,SIZP0,3)
                    CALL TAB_STRS_TXT50(WAP0(4),4,J,SIZP0,4)
                  ENDIF
                 J = J + 7
                END DO
              END DO
            ENDIF
          END IF

        ENDDO		
      ENDIF

      RETURN
      END
Chd|====================================================================
Chd|  GET_Q4L                       source/output/sta/stat_c_strafg.F
Chd|-- called by -----------
Chd|        DYNAIN_C_STRAG                source/output/dynain/dynain_c_strag.F
Chd|        STAT_C_STRAFG                 source/output/sta/stat_c_strafg.F
Chd|-- calls ---------------
Chd|        CLSKEW3                       source/elements/sh3n/coquedk/cdkcoor3.F
Chd|====================================================================
        SUBROUTINE GET_Q4L(JFT    ,JLT    ,IXC    ,X    ,OFFG   ,
     .                     IREL   ,VQ     )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
c-----------------------------------------------
c   g l o b a l   p a r a m e t e r s
c-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER IXC(NIXC,*),JFT,JLT,IREL
      my_real
     .   X(3,*), OFFG(*),VQ(3,3,MVSIZ)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,K,L
      INTEGER IXCTMP2,IXCTMP3,IXCTMP4,IXCTMP5
      my_real
     .   RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),SX(MVSIZ),SY(MVSIZ),
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),R21(MVSIZ),R22(MVSIZ),
     .   R23(MVSIZ),R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .   SZ(MVSIZ),DETA1(MVSIZ)
C----------------------------------------------
        DO I=JFT,JLT
          IXCTMP2=IXC(2,I)
          IXCTMP3=IXC(3,I)
          IXCTMP4=IXC(4,I)
          IXCTMP5=IXC(5,I)

          RX(I)=X(1,IXCTMP3)+X(1,IXCTMP4)-X(1,IXCTMP2)-X(1,IXCTMP5)
          SX(I)=X(1,IXCTMP4)+X(1,IXCTMP5)-X(1,IXCTMP2)-X(1,IXCTMP3)
          RY(I)=X(2,IXCTMP3)+X(2,IXCTMP4)-X(2,IXCTMP2)-X(2,IXCTMP5)
          SY(I)=X(2,IXCTMP4)+X(2,IXCTMP5)-X(2,IXCTMP2)-X(2,IXCTMP3)
          RZ(I)=X(3,IXCTMP3)+X(3,IXCTMP4)-X(3,IXCTMP2)-X(3,IXCTMP5)
          SZ(I)=X(3,IXCTMP4)+X(3,IXCTMP5)-X(3,IXCTMP2)-X(3,IXCTMP3)
        ENDDO
C----------------------------
C     LOCAL SYSTEM
C----------------------------
      CALL CLSKEW3(JFT,JLT,IREL,
     .   RX, RY, RZ,
     .   SX, SY, SZ,
     .   R11,R12,R13,R21,R22,R23,R31,R32,R33,DETA1,OFFG )
      DO I=JFT,JLT
        VQ(1,1,I)=R11(I)
        VQ(2,1,I)=R21(I)
        VQ(3,1,I)=R31(I)
        VQ(1,2,I)=R12(I)
        VQ(2,2,I)=R22(I)
        VQ(3,2,I)=R32(I)
        VQ(1,3,I)=R13(I)
        VQ(2,3,I)=R23(I)
        VQ(3,3,I)=R33(I)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  GET_T3L                       source/output/sta/stat_c_strafg.F
Chd|-- called by -----------
Chd|        DYNAIN_C_STRAG                source/output/dynain/dynain_c_strag.F
Chd|        STAT_C_STRAFG                 source/output/sta/stat_c_strafg.F
Chd|-- calls ---------------
Chd|        CLSKEW3                       source/elements/sh3n/coquedk/cdkcoor3.F
Chd|====================================================================
        SUBROUTINE GET_T3L(JFT    ,JLT    ,IXTG    ,X    ,OFFG   ,
     .                     IREL   ,VQ     )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
c-----------------------------------------------
c   g l o b a l   p a r a m e t e r s
c-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      INTEGER IXTG(NIXTG,*),JFT,JLT,IREL
      my_real
     .   X(3,*), OFFG(*),VQ(3,3,MVSIZ)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,J,K,L
      INTEGER I2,I3,I1
      my_real
     .   RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),SX(MVSIZ),SY(MVSIZ),
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),R21(MVSIZ),R22(MVSIZ),
     .   R23(MVSIZ),R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .   SZ(MVSIZ),DETA1(MVSIZ)
C----------------------------------------------
        DO I=JFT,JLT
          I1=IXTG(2,I)
          I2=IXTG(3,I)
          I3=IXTG(4,I)

          RX(I)=X(1,I2)-X(1,I1)
          RY(I)=X(2,I2)-X(2,I1)
          RZ(I)=X(3,I2)-X(3,I1)
          SX(I)=X(1,I3)-X(1,I1)
          SY(I)=X(2,I3)-X(2,I1)
          SZ(I)=X(3,I3)-X(3,I1)
        ENDDO
C----------------------------
C     LOCAL SYSTEM
C----------------------------
      CALL CLSKEW3(JFT,JLT,IREL,
     .   RX, RY, RZ,
     .   SX, SY, SZ,
     .   R11,R12,R13,R21,R22,R23,R31,R32,R33,DETA1,OFFG )
      DO I=JFT,JLT
        VQ(1,1,I)=R11(I)
        VQ(2,1,I)=R21(I)
        VQ(3,1,I)=R31(I)
        VQ(1,2,I)=R12(I)
        VQ(2,2,I)=R22(I)
        VQ(3,2,I)=R32(I)
        VQ(1,3,I)=R13(I)
        VQ(2,3,I)=R23(I)
        VQ(3,3,I)=R33(I)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SHELL2G                       source/output/sta/stat_c_strafg.F
Chd|-- called by -----------
Chd|        DYNAIN_C_STRAG                source/output/dynain/dynain_c_strag.F
Chd|        STAT_C_STRAFG                 source/output/sta/stat_c_strafg.F
Chd|        STAT_C_STRSFG                 source/output/sta/stat_c_strsfg.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SHELL2G(EPS,QT)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8
#include      "implicit_f.inc"
c-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   EPS(6),QT(3,3)
C------------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .       TXX,TYY,TZZ,TXY,TYZ,TZX,UXX,UYY,UZZ,UXY,UYZ,UZX,A,B,C
C--convention input EPS : exx,eyy,exy,eyz,ezx,0; output exx,eyy,ezz,exy,eyz,ezx
             TXX = EPS(1)
             TYY = EPS(2)
             TZZ = ZERO
             TXY = EPS(3)
             TYZ = EPS(4)
             TZX = EPS(5)
C
              A = QT(1,1)*TXX + QT(1,2)*TXY + QT(1,3)*TZX
              B = QT(1,1)*TXY + QT(1,2)*TYY + QT(1,3)*TYZ
              C = QT(1,1)*TZX + QT(1,2)*TYZ + QT(1,3)*TZZ
              UXX = A*QT(1,1) + B*QT(1,2) + C*QT(1,3)
              UXY = A*QT(2,1) + B*QT(2,2) + C*QT(2,3)
              UZX = A*QT(3,1) + B*QT(3,2) + C*QT(3,3)
              A = QT(2,1)*TXX + QT(2,2)*TXY + QT(2,3)*TZX
              B = QT(2,1)*TXY + QT(2,2)*TYY + QT(2,3)*TYZ
              C = QT(2,1)*TZX + QT(2,2)*TYZ + QT(2,3)*TZZ
              UYY = A*QT(2,1) + B*QT(2,2) + C*QT(2,3)
              UYZ = A*QT(3,1) + B*QT(3,2) + C*QT(3,3)
              A = QT(3,1)*TXX + QT(3,2)*TXY + QT(3,3)*TZX
              B = QT(3,1)*TXY + QT(3,2)*TYY + QT(3,3)*TYZ
              C = QT(3,1)*TZX + QT(3,2)*TYZ + QT(3,3)*TZZ
              UZZ = A*QT(3,1) + B*QT(3,2) + C*QT(3,3)
C              
             EPS(1) = UXX
             EPS(2) = UYY
             EPS(3) = UZZ
             EPS(4) = UXY
             EPS(5) = UYZ
             EPS(6) = UZX
C             
      RETURN
      END
