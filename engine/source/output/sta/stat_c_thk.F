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
Chd|  STAT_C_THK                    source/output/sta/stat_c_thk.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        SPMD_RGATHER9_DP              source/mpi/interfaces/spmd_outp.F
Chd|        STRS_TXT50                    source/output/sta/sta_txt.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE STAT_C_THK(ELBUF_TAB,IPARG ,IPM ,IGEO ,IXC ,
     2                      IXTG  ,WA,WAP0 ,IPARTC, IPARTTG,
     3                      IPART_STATE,STAT_INDXC,STAT_INDXTG  ,
     4                      THKE  ,SIZP0)
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
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SIZLOC,SIZP0
      INTEGER IXC(NIXC,*),IXTG(NIXTG,*),
     .        IPARG(NPARG,*),IPM(NPROPMI,*),IGEO(NPROPGI,*),
     .        IPARTC(*), IPARTTG(*), IPART_STATE(*),
     .        STAT_INDXC(*), STAT_INDXTG(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      my_real
     .   THKE(*)
      double precision WA(*),WAP0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, J, JJ, WRTLEN, RES, LEN, K, IOFF
      INTEGER NG, NEL, NFT, IAD, ITY, LFT,
     .        LLT, MLW, ISTRAIN, IHBE, ISH3N, IPRT
      INTEGER ID,IEXPAN,ISROT,ITHK
      double precision   
     .   THK
      CHARACTER*100 LINE
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C-----------------------------------------------
C     4-NODE SHELLS
C-----------------------------------------------
      JJ = 0
      IF(STAT_NUMELC==0) GOTO 200

      DO NG=1,NGROUP
       ITY   =IPARG(5,NG)
       IF(ITY.EQ.3) THEN
         GBUF => ELBUF_TAB(NG)%GBUF   
         MLW   =IPARG(1,NG)
         NEL   =IPARG(2,NG)
         NFT   =IPARG(3,NG)
         ITHK  =IPARG(28,NG)
         LFT   =1
         LLT   =NEL

         DO I=LFT,LLT
          N  = I + NFT

          IPRT=IPARTC(N)
          IF(IPART_STATE(IPRT)==0)CYCLE

          JJ = JJ + 1
          IF (MLW .NE. 0 .AND. MLW .NE. 13) THEN
            WA(JJ) = GBUF%OFF(I) 
          ELSE
            WA(JJ) = ZERO
          ENDIF
          JJ = JJ + 1
          WA(JJ) = IXC(NIXC,N)
          JJ = JJ + 1
          IF (MLW .NE. 0 .AND. MLW .NE. 13) THEN
           IF (ITHK >0 ) THEN
            WA(JJ) = GBUF%THK(I)
           ELSE
            WA(JJ) = THKE(N)
           END IF
          ELSE
            WA(JJ) = ZERO
          ENDIF
         ENDDO
       ENDIF        
      ENDDO

 200  CONTINUE

      IF(NSPMD == 1)THEN
        LEN=JJ
        DO J=1,LEN
          WAP0(J)=WA(J)
        END DO
      ELSE
        LEN = 0
        CALL SPMD_RGATHER9_DP(WA,JJ,WAP0,SIZP0,LEN)
      END IF

      IF(ISPMD.EQ.0.AND.LEN.GT.0) THEN
         IF (IZIPSTRS == 0) THEN
           WRITE(IUGEO,'(A)')'/INISHE/THICK'
           WRITE(IUGEO,'(A)')
     .      '#  SHELLID                                     THK' 
         ELSE
           WRITE(LINE,'(A)')'/INISHE/THICK'
           CALL STRS_TXT50(LINE,100) 
           WRITE(LINE,'(A)')
     .      '#  SHELLID                                     THK' 
           CALL STRS_TXT50(LINE,100) 
         END IF

        DO N=1,STAT_NUMELC_G
          K=STAT_INDXC(N)
          J=3*(K-1)
          IOFF  = NINT(WAP0(J + 1)) 
          IF(IOFF >= 1)THEN
            ID  =NINT(WAP0(J+2))
            THK =WAP0(J+3)
            IF (IZIPSTRS == 0) THEN
                WRITE(IUGEO,'(I10,20X,1PE20.13)')ID,THK
            ELSE
                WRITE(LINE,'(I10,20X,1PE20.13)')ID,THK 
               CALL STRS_TXT50(LINE,100) 
            END IF
          END IF
        END DO

      ENDIF

C-----------------------------------------------
C     3-NODE SHELLS
C-----------------------------------------------
      JJ = 0
      IF(STAT_NUMELTG==0) GOTO 300

      DO NG=1,NGROUP
       ITY   =IPARG(5,NG)
       IF(ITY.EQ.7) THEN
         GBUF => ELBUF_TAB(NG)%GBUF   
         MLW   =IPARG(1,NG)
         NEL   =IPARG(2,NG)
         NFT   =IPARG(3,NG)
         ITHK  =IPARG(28,NG)

         LFT   =1
         LLT   =NEL
         DO I=LFT,LLT
          N  = I + NFT

          IPRT=IPARTTG(N)
          IF(IPART_STATE(IPRT)==0)CYCLE

          JJ = JJ + 1
          IF (MLW .NE. 0 .AND. MLW .NE. 13) THEN
            WA(JJ) = GBUF%OFF(I)
          ELSE
            WA(JJ) = ZERO
          ENDIF
          JJ = JJ + 1
          WA(JJ) = IXTG(NIXTG,N)
          JJ = JJ + 1
          IF (MLW .NE. 0 .AND. MLW .NE. 13) THEN
           IF (ITHK >0 ) THEN
            WA(JJ) = GBUF%THK(I)
           ELSE
            WA(JJ) = THKE(N+NUMELC)
           END IF
          ELSE
            WA(JJ) = ZERO
          ENDIF
         ENDDO
       ENDIF        
      ENDDO

 300  CONTINUE

      IF(NSPMD == 1)THEN
        LEN=JJ
        DO J=1,LEN
          WAP0(J)=WA(J)
        END DO
      ELSE
        LEN = 0
        CALL SPMD_RGATHER9_DP(WA,JJ,WAP0,SIZP0,LEN)
      END IF

      IF(ISPMD.EQ.0.AND.LEN.GT.0) THEN
        IF (IZIPSTRS == 0) THEN
          WRITE(IUGEO,'(A)')'/INISH3/THICK'
          WRITE(IUGEO,'(A)')
     .      '#   SH3NID                                     THK' 
         ELSE
           WRITE(LINE,'(A)')'/INISH3/THICK'
           CALL STRS_TXT50(LINE,100) 
           WRITE(LINE,'(A)')
     .      '#   SH3NID                                     THK' 
           CALL STRS_TXT50(LINE,100) 
         END IF

        DO N=1,STAT_NUMELTG_G
          K=STAT_INDXTG(N)
          J=3*(K-1)
          IOFF  = NINT(WAP0(J + 1)) 
          IF(IOFF >= 1)THEN
            ID  =NINT(WAP0(J+2))
            THK =WAP0(J+3)
            IF (IZIPSTRS == 0) THEN
               WRITE(IUGEO,'(I10,20X,1PE20.13)')ID,THK
            ELSE
               WRITE(LINE,'(I10,20X,1PE20.13)')ID,THK 
               CALL STRS_TXT50(LINE,100) 
            END IF
          END IF
        END DO
      ENDIF

      RETURN
      END
