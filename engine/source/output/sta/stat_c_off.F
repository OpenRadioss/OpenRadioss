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
Chd|  STAT_C_OFF                    source/output/sta/stat_c_off.F
Chd|-- called by -----------
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|-- calls ---------------
Chd|        SPMD_RGATHER9_DP              source/mpi/interfaces/spmd_outp.F
Chd|        STRS_TXT50                    source/output/sta/sta_txt.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE STAT_C_OFF(ELBUF_TAB,IPARG ,IPM ,IGEO ,IXC ,
     2                      IXTG  ,WA,WAP0 ,IPARTC, IPARTTG,
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
      double precision WA(*),WAP0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,JJ,LEN,NG, NEL, NFT, ITY, LFT,
     .   LLT, MLW, IPRT,ID,IOFF
      CHARACTER*100 LINE
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C-----------------------------------------------
C     4-NODE SHELLS
C-----------------------------------------------
      JJ = 0
      IF(STAT_NUMELC==0) GOTO 200
C
      DO NG=1,NGROUP
       ITY   =IPARG(5,NG)
       IF(ITY==3) THEN
         GBUF => ELBUF_TAB(NG)%GBUF   
         MLW   =IPARG(1,NG)
         NEL   =IPARG(2,NG)
         NFT   =IPARG(3,NG)

         LFT   =1
         LLT   =NEL
         DO I=LFT,LLT
          N  = I + NFT

          IPRT=IPARTC(N)
          IF(IPART_STATE(IPRT)==0)CYCLE

          JJ = JJ + 1
          WA(JJ) = IXC(NIXC,N)
          JJ = JJ + 1
          IF (MLW /= 0 .AND. MLW /= 13) THEN
            WA(JJ) = GBUF%OFF(I)
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

      IF(ISPMD==0.AND.LEN>0) THEN

        IF (IZIPSTRS == 0) THEN
          WRITE(IUGEO,'(A)')'/INISHE/OFF'
          WRITE(IUGEO,'(A)')
     .      '#  SHELLID      IOFF' 
        ELSE
          WRITE(LINE,'(A)')'/INISHE/OFF' 
               CALL STRS_TXT50(LINE,100)
          WRITE(LINE,'(A)')
     .      '#  SHELLID      IOFF' 
               CALL STRS_TXT50(LINE,100)
        ENDIF
        DO N=1,STAT_NUMELC_G
          K=STAT_INDXC(N)
          J=2*(K-1)
          ID  =NINT(WAP0(J+1))
          IOFF=NINT(WAP0(J+2))
          IF (IZIPSTRS == 0) THEN
            WRITE(IUGEO,'(2I10)')ID,IOFF
          ELSE
            WRITE(LINE,'(2I10)')ID,IOFF
            CALL STRS_TXT50(LINE,100)
          ENDIF
        END DO
      ENDIF

C-----------------------------------------------
C     3-NODE SHELLS
C-----------------------------------------------
      JJ = 0
      IF(STAT_NUMELTG==0) GOTO 300

C
      DO NG=1,NGROUP
       ITY   =IPARG(5,NG)
       IF(ITY==7) THEN
         GBUF => ELBUF_TAB(NG)%GBUF   
         MLW   =IPARG(1,NG)
         NEL   =IPARG(2,NG)
         NFT   =IPARG(3,NG)
c
         LFT   =1
         LLT   =NEL
         DO I=LFT,LLT
          N  = I + NFT

          IPRT=IPARTTG(N)
          IF(IPART_STATE(IPRT)==0)CYCLE

          JJ = JJ + 1
          WA(JJ) = IXTG(NIXTG,N)
          JJ = JJ + 1
          IF (MLW /= 0 .AND. MLW /= 13) THEN
            WA(JJ) = GBUF%OFF(I)
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

      IF(ISPMD==0.AND.LEN>0) THEN

        IF (IZIPSTRS == 0) THEN
          WRITE(IUGEO,'(A)')'/INISH3/OFF'
          WRITE(IUGEO,'(A)')
     .      '#   SH3NID      IOFF' 
        ELSE
          WRITE(LINE,'(A)')'/INISH3/OFF' 
               CALL STRS_TXT50(LINE,100)
          WRITE(LINE,'(A)')
     .      '#   SH3NID      IOFF' 
               CALL STRS_TXT50(LINE,100)
        ENDIF

        DO N=1,STAT_NUMELTG_G
          K=STAT_INDXTG(N)
          J=2*(K-1)
          ID  =NINT(WAP0(J+1))
          IOFF=NINT(WAP0(J+2))
          IF (IZIPSTRS == 0) THEN
            WRITE(IUGEO,'(2I10)')ID,IOFF
          ELSE
            WRITE(LINE,'(2I10)')ID,IOFF
            CALL STRS_TXT50(LINE,100)
          ENDIF
        END DO
      ENDIF
c-----------
      RETURN
      END
