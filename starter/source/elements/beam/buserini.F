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
!||    buserini               ../starter/source/elements/beam/buserini.F
!||--- called by ------------------------------------------------------
!||    pinit3                 ../starter/source/elements/beam/pinit3.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    fretitl2               ../starter/source/starter/freform.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE BUSERINI(ELBUF_STR,
     .                    IXP      ,SIGBEAM  ,NSIGBEAM ,PTBEAM ,IGEO  ,
     .                    NEL      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NSIGBEAM
      INTEGER IXP(NIXP,*),PTBEAM(*),IGEO(NPROPGI,*)
      my_real
     .   SIGBEAM(NSIGBEAM,*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C------------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,II,JJ,NPTI,NU,NUVAR,NVARS,IPT,ILAY,IR,IS,PID,IPID,IGTYP
      CHARACTER(LEN=NCHARTITLE)::TITR1
      my_real, DIMENSION(:), POINTER :: UVAR
C=======================================================================
!     INITIAL USER VARIABLES
      DO I=LFT,LLT
        II = NFT+I
        JJ = PTBEAM(II)
        IF (JJ > 0) THEN
          NPTI  = NINT(SIGBEAM(NVBEAM + 2,JJ))
          IGTYP = NINT(SIGBEAM(NVBEAM + 3,JJ))
          NVARS = NINT(SIGBEAM(NVBEAM + 4,JJ))
!---
! check NPT /= NPTI
          IF (NPT /= NPTI  .AND . NPTI /= 0) THEN
            IPID=IXP(5,I)
            PID=IGEO(1,IXP(5,I))
            CALL FRETITL2(TITR1,IGEO(NPROPGI-LTITR+1,IPID),LTITR)
            CALL ANCMSG(MSGID=1237,ANMODE=ANINFO,MSGTYPE=MSGERROR,I1=PID,I2=IXP(NIXP,I),C1=TITR1)
          ENDIF

          DO IPT=1,NPT
            ILAY=1
            IR = 1
            IS = 1
            NUVAR = ELBUF_STR%BUFLY(ILAY)%NVAR_MAT
            UVAR => ELBUF_STR%BUFLY(ILAY)%MAT(IR,IS,IPT)%VAR
            DO NU = 1,MIN(NVARS,NUVAR)
              UVAR((NU-1)*NEL + I) =  SIGBEAM(NVBEAM + 4  + NU + (IPT -1)*NVARS,JJ)
            ENDDO
          ENDDO  !  DO ILAY=1,NPT
        ENDIF ! IF (JJ > 0)
      ENDDO ! DO I=JFT,JLT
C-----------
      RETURN
      END
