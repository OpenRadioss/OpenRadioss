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
Chd|  STAT_C_STRAF                  source/output/sta/stat_c_straf.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        SPMD_RGATHER9_DP              source/mpi/interfaces/spmd_outp.F
Chd|        SPMD_STAT_PGATHER             source/mpi/output/spmd_stat.F 
Chd|        STRS_TXT50                    source/output/sta/sta_txt.F   
Chd|        TAB_STRS_TXT50                source/output/sta/sta_txt.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE STAT_C_STRAF(ELBUF_TAB,IPARG ,IPM ,IGEO ,IXC ,
     2                        IXTG  ,WA,WAP0 ,IPARTC, IPARTTG,
     3                      IPART_STATE,STAT_INDXC,STAT_INDXTG,THKE,SIZP0)
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
      my_real   
     .   THKE(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      double precision WA(*),WAP0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,II,JJ,LEN, IOFF, NG, NEL, NFT, ITY, LFT, NPT,
     .    LLT, MLW, ISTRAIN,ID, IPRT0, IPRT,NPG,IPG,IE,NPTR,NPTS,G_STRA,
     .    ITHK,KK(8)
      INTEGER PTWA(MAX(STAT_NUMELC ,STAT_NUMELTG)),
     .        PTWA_P0(0:MAX(1,STAT_NUMELC_G,STAT_NUMELTG_G))
      double precision   
     .   THK, EM, EB, H1, H2, H3
      CHARACTER*100 DELIMIT,LINE
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY     
      my_real, 
     .   DIMENSION(:),POINTER  :: STRAIN    
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
         NPG  = NPTR*NPTS
         LFT=1
         LLT=NEL
         G_STRA = GBUF%G_STRA
!
         DO J=1,8  ! length max of GBUF%G_STRA = 8
           KK(J) = NEL*(J-1)
         ENDDO
!
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
          WA(JJ) = NPT
          JJ = JJ + 1
          WA(JJ) = NPG
          JJ = JJ + 1
          IF (MLW /= 0 .AND. MLW /= 13) THEN
           IF (ITHK >0 ) THEN
            WA(JJ) = GBUF%THK(I)
           ELSE
            WA(JJ) = THKE(N)
           END IF
          ELSE
            WA(JJ) = ZERO
          ENDIF
c         Strain in Gauss points
          IF (MLW == 0 .or. MLW == 13) THEN
            DO IPG=1,NPG
              DO J=1,G_STRA
                JJ    = JJ + 1
                WA(JJ)=ZERO
              END DO      
            END DO        
          ELSEIF (G_STRA /= 0) THEN
            IF (NPG > 1) THEN
              STRAIN => GBUF%STRPG
          ELSE
              STRAIN => GBUF%STRA
            ENDIF
            II = G_STRA*(I-1)
            DO IPG=1,NPG
              K = (IPG-1)*NEL*G_STRA              
              DO J=1,G_STRA
                JJ    = JJ + 1
                WA(JJ) = STRAIN(KK(J)+I+K)
              END DO      
            END DO        
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
               WRITE(IUGEO,'(A)')'/INISHE/STRA_F'
               WRITE(IUGEO,'(A)')
     .'#------------------------ REPEAT --------------------------' 
               WRITE(IUGEO,'(A)')
     .      '#  SHELLID       NPT       NPG                 THK' 
               WRITE(IUGEO,'(A/A/A)')
     .'# REPEAT I=1,NPG :',
     .'#   E1, E2, E12, E23, E31,',
     .'#   K1, K2, K12'
               WRITE(IUGEO,'(A)')
     .'#---------------------- END REPEAT ------------------------' 
                WRITE(IUGEO,'(A)') DELIMIT
             ELSE
               WRITE(LINE,'(A)') DELIMIT
               CALL STRS_TXT50(LINE,100)
               WRITE(LINE,'(A)')'/INISHE/STRA_F'
               CALL STRS_TXT50(LINE,100)
               WRITE(LINE,'(A)')
     .'#------------------------ REPEAT --------------------------' 
               CALL STRS_TXT50(LINE,100)
               WRITE(LINE,'(A)')
     .      '#  SHELLID       NPT       NPG                 THK' 
               CALL STRS_TXT50(LINE,100)
               WRITE(LINE,'(A)')'# REPEAT I=1,NPG :'
               CALL STRS_TXT50(LINE,100)
               WRITE(LINE,'(A)')'#   E1, E2, E12, E23, E31,'
               CALL STRS_TXT50(LINE,100)
               WRITE(LINE,'(A)')'#   K1, K2, K12'
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

            DO IPG=1,NPG
              IF (IZIPSTRS == 0) THEN
                WRITE(IUGEO,'(1P5E20.13)')(WAP0(J + K),K=1,5)
                WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=6,8)
              ELSE
                CALL TAB_STRS_TXT50(WAP0(1),5,J,SIZP0,5)
                CALL TAB_STRS_TXT50(WAP0(6),3,J,SIZP0,3)
              ENDIF
            END DO
          END IF
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
         NPTR = ELBUF_TAB(NG)%NPTR    
         NPTS = ELBUF_TAB(NG)%NPTS    
         NPG  = NPTR*NPTS
         LFT=1
         LLT=NEL
!
         DO J=1,8  ! length max of GBUF%G_STRA = 8
           KK(J) = NEL*(J-1)
         ENDDO
!
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
          WA(JJ) = NPT
          JJ = JJ + 1
          WA(JJ) = NPG
          JJ = JJ + 1
          IF (MLW /= 0 .AND. MLW /= 13) THEN
           IF (ITHK >0 ) THEN
            WA(JJ) = GBUF%THK(I)
           ELSE
            WA(JJ) = THKE(N+NUMELC)
           END IF
          ELSE
            WA(JJ) = ZERO
          ENDIF

c         Strain in Gauss points
          IF (MLW == 0 .or. MLW == 13) THEN
            DO IPG=1,NPG
              DO J=1,G_STRA
                JJ    = JJ + 1
                WA(JJ) = ZERO
              END DO      
            END DO        
          ELSEIF (G_STRA > 0) THEN
            IF (NPG > 1) THEN
              STRAIN => GBUF%STRPG
            ELSE
              STRAIN => GBUF%STRA
            ENDIF
            II = G_STRA*(I-1)
            DO IPG=1,NPG
              K = (IPG-1)*NEL*G_STRA              
              DO J=1,G_STRA
                JJ = JJ + 1
                WA(JJ) = STRAIN(KK(J)+I+K)
              END DO      
            END DO        
          END IF  ! ISTRAIN /=0

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
              WRITE(IUGEO,'(A)')'/INISH3/STRA_F'
              WRITE(IUGEO,'(A)')
     .'#------------------------ REPEAT --------------------------' 
              WRITE(IUGEO,'(A)')
     .      '#   SH3NID       NPT       NPG                 THK' 
             WRITE(IUGEO,'(A/A/A)')
     .'# REPEAT I=1,NPG :',
     .'#   E1, E2, E12, E23, E31,',
     .'#   K1, K2, K12'
              WRITE(IUGEO,'(A)')
     .'#---------------------- END REPEAT ------------------------' 
              WRITE(IUGEO,'(A)') DELIMIT
             ELSE
              WRITE(LINE,'(A)') DELIMIT
              CALL STRS_TXT50(LINE,100)
              WRITE(LINE,'(A)')'/INISH3/STRA_F'
              CALL STRS_TXT50(LINE,100)
              WRITE(LINE,'(A)')
     .'#------------------------ REPEAT --------------------------'
              CALL STRS_TXT50(LINE,100) 
              WRITE(LINE,'(A)')
     .      '#   SH3NID       NPT       NPG                 THK' 
              CALL STRS_TXT50(LINE,100)
              WRITE(LINE,'(A)')'# REPEAT I=1,NPG :' 
              CALL STRS_TXT50(LINE,100)
              WRITE(LINE,'(A)')'#   E1, E2, E12, E23, E31,' 
              CALL STRS_TXT50(LINE,100)
              WRITE(LINE,'(A)')'#   K1, K2, K12'
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
            DO IPG=1,NPG
              IF (IZIPSTRS == 0) THEN
                WRITE(IUGEO,'(1P5E20.13)')(WAP0(J + K),K=1,5)
                WRITE(IUGEO,'(1P3E20.13)')(WAP0(J + K),K=6,8)
              ELSE
                CALL TAB_STRS_TXT50(WAP0(1),5,J,SIZP0,5)
                CALL TAB_STRS_TXT50(WAP0(6),3,J,SIZP0,3)
              ENDIF
            END DO
          END IF

        ENDDO		    
      ENDIF

      RETURN
      END
