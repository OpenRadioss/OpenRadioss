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
Chd|  MULTI_I18_FORCE_PON           source/interfaces/int18/multi_i18_force_pon.F
Chd|-- called by -----------
Chd|        I18FOR3                       source/interfaces/int18/i18for3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FOAT_TO_6_FLOAT               source/system/parit.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE MULTI_I18_FORCE_PON(JLT   ,IX1   ,IX2  ,IX3  ,IX4   ,
     2                  NSVG  ,H1    ,H2   ,H3   ,H4    ,STIF   ,
     3                  FX1   ,FY1   ,FZ1  ,FX2  ,FY2   ,FZ2    ,
     4                  FX3   ,FY3   ,FZ3  ,FX4  ,FY4   ,FZ4    ,
     5                  FXI   ,FYI   ,FZI  ,FSKYI,ISKY  ,NISKYFI,
     6                  NIN   ,NOINT ,MULTI_FVM,DT,JTASK)     
!$COMMENT
!       MULTI_I18_FORCE_PON description
!       accumulation of force for local and remote nodes
!       
!       MULTI_I18_FORCE_PON organization :
!       - secondary nodes:
!            * if NSV > 0 --> local node (phantom node id = NSV - NUMNOD)
!                             accumulation in FORC_INT array
!            * if NSV < 0 --> remote node
!                             accumulation in AFI array
!       for each secondary node, a float_to_6_float operation is perfomed
!       in order to garantee the parith/on
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,NISKYFI,NIN,NOINT,INTTH,
     .        ISKY(*),JTASK,
     .        IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),NSVG(MVSIZ)
      my_real
     .    H1(MVSIZ),H2(MVSIZ),H3(MVSIZ),H4(MVSIZ),STIF(MVSIZ),
     .    FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .    FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .    FX3(MVSIZ),FY3(MVSIZ),FZ3(MVSIZ),
     .    FX4(MVSIZ),FY4(MVSIZ),FZ4(MVSIZ),
     .    FXI(MVSIZ),FYI(MVSIZ),FZI(MVSIZ),
     .    FSKYI(LSKYI,NFSKYI),DT
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, IG, NISKYL1, NISKYL,IGP,IGM,IDR,NISKYFIL
      INTEGER :: NODFI
      REAL(kind=8), DIMENSION(6,3*MVSIZ) :: LOCAL_DP,REMOTE_DP
      my_real, DIMENSION(3*MVSIZ) :: F_LOCAL, F_REMOTE
      INTEGER :: NINDEX_LOCAL,NINDEX_REMOTE
      INTEGER, DIMENSION(MVSIZ) :: INDEX_LOCAL,INDEX_REMOTE
      INTEGER :: SHIFT_FORCE_INT
C
      NISKYL1 = 0
      DO I = 1, JLT
        IF (H1(I)/=ZERO) NISKYL1 = NISKYL1 + 1
      ENDDO
      DO I = 1, JLT
        IF (H2(I)/=ZERO) NISKYL1 = NISKYL1 + 1
      ENDDO
      DO I = 1, JLT
        IF (H3(I)/=ZERO) NISKYL1 = NISKYL1 + 1
      ENDDO
      DO I = 1, JLT
        IF (H4(I)/=ZERO) NISKYL1 = NISKYL1 + 1
      ENDDO
C
C Precalcul impact locaux / remote
C
      IGP = 0
      IGM = 0
C
#include "lockon.inc"
      NISKYL = NISKY
      NISKY = NISKY + NISKYL1 + IGP
      NISKYFIL = NISKYFI
      NISKYFI = NISKYFI + IGM
#include "lockoff.inc"
C
      IF (NISKYL+NISKYL1+IGP > LSKYI) THEN
         CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
         CALL ARRET(2)
      ENDIF
      IF (NISKYFIL+IGM > NLSKYFI(NIN)) THEN
        CALL ANCMSG(MSGID=26,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
      DO I=1,JLT
         IF (H1(I)/=0.) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX1(I)
            FSKYI(NISKYL,2)=FY1(I)
            FSKYI(NISKYL,3)=FZ1(I)
            FSKYI(NISKYL,4)=STIF(I)*ABS(H1(I))
            ISKY(NISKYL) = IX1(I)
         ENDIF
      ENDDO
      DO I=1,JLT
         IF (H2(I)/=ZERO) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX2(I)
            FSKYI(NISKYL,2)=FY2(I)
            FSKYI(NISKYL,3)=FZ2(I)
            FSKYI(NISKYL,4)=STIF(I)*ABS(H2(I))
            ISKY(NISKYL) = IX2(I)
         ENDIF
      ENDDO
      DO I=1,JLT
         IF (H3(I)/=ZERO) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX3(I)
            FSKYI(NISKYL,2)=FY3(I)
            FSKYI(NISKYL,3)=FZ3(I)
            FSKYI(NISKYL,4)=STIF(I)*ABS(H3(I))
            ISKY(NISKYL) = IX3(I)
         ENDIF
      ENDDO
      DO I=1,JLT
         IF (H4(I)/=ZERO) THEN
            NISKYL = NISKYL + 1
            FSKYI(NISKYL,1)=FX4(I)
            FSKYI(NISKYL,2)=FY4(I)
            FSKYI(NISKYL,3)=FZ4(I)
            FSKYI(NISKYL,4)=STIF(I)*ABS(H4(I))
            ISKY(NISKYL) = IX4(I)
         ENDIF
      ENDDO
C     
      NINDEX_LOCAL = 0
      NINDEX_REMOTE = 0
      DO I=1,JLT
         IG = NSVG(I)
         ! --------------------
         ! local node
         IF(IG>0) THEN
            IG = IG - NUMNOD
            NINDEX_LOCAL = NINDEX_LOCAL + 1 
            INDEX_LOCAL(NINDEX_LOCAL) = IG
            F_LOCAL( 3*(NINDEX_LOCAL-1)+ 1 ) = -DT * FXI(I)
            F_LOCAL( 3*(NINDEX_LOCAL-1)+ 2 ) = -DT * FYI(I)
            F_LOCAL( 3*(NINDEX_LOCAL-1)+ 3 ) = -DT * FZI(I)
         ! --------------------
         ! remote node
         ELSE
            IG = -IG
            NINDEX_REMOTE = NINDEX_REMOTE + 1 
            INDEX_REMOTE(NINDEX_REMOTE) = IG
            F_REMOTE( 3*(NINDEX_REMOTE-1)+ 1 ) = -DT * FXI(I)
            F_REMOTE( 3*(NINDEX_REMOTE-1)+ 2 ) = -DT * FYI(I)
            F_REMOTE( 3*(NINDEX_REMOTE-1)+ 3 ) = -DT * FZI(I)
         ENDIF  
         ! --------------------
      ENDDO

      ! --------------------
      ! local value
      CALL FOAT_TO_6_FLOAT(1,3*NINDEX_LOCAL,F_LOCAL,LOCAL_DP)
      SHIFT_FORCE_INT = (JTASK-1)*NUMELS
      DO I=1,NINDEX_LOCAL
        IG=INDEX_LOCAL(I)
        MULTI_FVM%FORCE_INT_PON(1,1:6,IG+SHIFT_FORCE_INT) = 
     .  MULTI_FVM%FORCE_INT_PON(1,1:6,IG+SHIFT_FORCE_INT) + LOCAL_DP(1:6,3*(I-1)+1)
        MULTI_FVM%FORCE_INT_PON(2,1:6,IG+SHIFT_FORCE_INT) = 
     .  MULTI_FVM%FORCE_INT_PON(2,1:6,IG+SHIFT_FORCE_INT) + LOCAL_DP(1:6,3*(I-1)+2)
        MULTI_FVM%FORCE_INT_PON(3,1:6,IG+SHIFT_FORCE_INT) = 
     .  MULTI_FVM%FORCE_INT_PON(3,1:6,IG+SHIFT_FORCE_INT) + LOCAL_DP(1:6,3*(I-1)+3)
      ENDDO
      ! --------------------
      ! remote node
      CALL FOAT_TO_6_FLOAT(1,3*NINDEX_REMOTE,F_REMOTE,REMOTE_DP)

      NODFI = MULTI_FVM%R_AFI(NIN)%NODFI
      SHIFT_FORCE_INT = (JTASK-1)*NODFI
      DO I=1,NINDEX_REMOTE
        IG=INDEX_REMOTE(I)
        MULTI_FVM%R_AFI(NIN)%R_FORCE_INT(1,1:6,IG+SHIFT_FORCE_INT) = 
     .    MULTI_FVM%R_AFI(NIN)%R_FORCE_INT(1,1:6,IG+SHIFT_FORCE_INT) + 
     .    REMOTE_DP(1:6,3*(I-1)+1)
        MULTI_FVM%R_AFI(NIN)%R_FORCE_INT(2,1:6,IG+SHIFT_FORCE_INT) = 
     .    MULTI_FVM%R_AFI(NIN)%R_FORCE_INT(2,1:6,IG+SHIFT_FORCE_INT) + 
     .    REMOTE_DP(1:6,3*(I-1)+2)
        MULTI_FVM%R_AFI(NIN)%R_FORCE_INT(3,1:6,IG+SHIFT_FORCE_INT) = 
     .    MULTI_FVM%R_AFI(NIN)%R_FORCE_INT(3,1:6,IG+SHIFT_FORCE_INT) + 
     .    REMOTE_DP(1:6,3*(I-1)+3)
      ENDDO
      ! --------------------
      RETURN  
      END SUBROUTINE MULTI_I18_FORCE_PON
