Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    lecstat        ../engine/source/input/lecstat.F
!||--- called by ------------------------------------------------------
!||    lectur         ../engine/source/input/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg         ../engine/source/output/message/message.F
!||    arret          ../engine/source/system/arret.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod   ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    mat_elem_mod   ../common_source/modules/mat_elem/mat_elem_mod.F90
!||    message_mod    ../engine/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE LECSTAT(IPART ,IPART_STATE,ELBUF_TAB,IPM,IPARG,
     .                   IPARTS,IPARTC     ,IPARTG,MAT_PARAM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE MAT_ELEM_MOD
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
#include      "scr16_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPART(LIPART1,*),IPART_STATE(*),IPARTS(*),IPARTC(*),IPARTG(*)
      INTEGER,INTENT(INOUT) :: IPM(NPROPMI,NUMMAT),IPARG(NPARG,*)
C     REAL
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(INOUT) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IDPRT,K_STAT,J,IP,IPID,IGTYP,
     .        IFAIL,NLAY,FAIL_ID,MAT_ID,IL,IMAT,
     .        NFAIL,NG,ITY,NFT,IPRT
      INTEGER MAT_CHECKED(NUMMAT)
C-----------------------------------------------
      IPRT = 0
      IF (NSTATPRT /= 0) THEN
        MAT_CHECKED = 0
        DO I=1,NSTATPRT
          READ(IIN,'(I10)') IDPRT
          IP=0
          DO J=1,NPART
            IF (IPART(4,J) == IDPRT)IP=J
          ENDDO
          IF (IP == 0) THEN
            CALL ANCMSG(MSGID=119,ANMODE=ANINFO,I1=IDPRT)
            CALL ARRET(2)
          ENDIF ! IF (IP==0)
          IPART_STATE(IP)=1
          !-------------
          ! for /INISHE/FAIL, fail_ID in /FAIL card is mandatory
          ! check if fail_ID is defined in failure model
          !-------------
          IF ( STAT_C(8) == 1) THEN
!---
            DO NG=1,NGROUP
              ITY = IPARG(5,NG)
              NFT = IPARG(3,NG)
              IPID=IPART(2,IP) ! internal
              IF (ITY == 1 .or. ITY == 3 .or. ITY == 7) THEN
                IF (ITY == 1) IPRT=IPARTS(1+NFT)
                IF (ITY == 3) IPRT=IPARTC(1+NFT)
                IF (ITY == 7) IPRT=IPARTG(1+NFT)
                IF ( IPRT /= IP ) CYCLE
!
                NLAY = ELBUF_TAB(NG)%NLAY
!
                DO IL = 1,NLAY
                  IMAT  = ELBUF_TAB(NG)%BUFLY(IL)%IMAT
                  NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL
                  MAT_ID = IPM(1,IMAT)  ! user mat ID
                  IF (MAT_CHECKED(IMAT) == 0 ) THEN
                    MAT_CHECKED(IMAT) = 1
                    DO IFAIL = 1,NFAIL 
                      FAIL_ID = MAT_PARAM(IMAT)%FAIL(IFAIL)%FAIL_ID  
                      IF (FAIL_ID == 0)
     .                  CALL ANCMSG(MSGID=297,ANMODE=ANINFO,I1=MAT_ID)
                    ENDDO ! DO IFAIL
                  ENDIF ! IF (MAT_CHECKED(IMAT) == 0 )
                ENDDO ! DO IL = 1,NLAY
              ENDIF ! IF (ITY == 1 .or. ITY == 3 .or. ITY == 7)
            ENDDO ! DO NG=1,NGROUP
C---
          ENDIF ! IF ( part de coque & STAT_C(8) = 1) THEN
        END DO ! DO I=1,NSTATPRT
      ELSEIF (NSTATALL /= 0) THEN
        DO J=1,NPART
          IPART_STATE(J) = 1
        END DO
      ENDIF ! IF (NSTATPRT /= 0)
C
      RETURN
      END
