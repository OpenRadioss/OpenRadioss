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
Chd|  BUSERINI                      source/elements/beam/buserini.F
Chd|-- called by -----------
Chd|        PINIT3                        source/elements/beam/pinit3.F 
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE BUSERINI(ELBUF_STR,
     .                    IXP      ,SIGBEAM  ,NSIGBEAM ,PTBEAM ,IGEO  ,
     .                    NEL      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE MESSAGE_MOD           
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
      INTEGER I,II,JJ,NPTI,NU,NUVAR,NVARS,IPT,
     .        ILAY,IR,IS,PID,IPID,IGTYP
      INTEGER KN,PT,J1,JJ1,K
!
      CHARACTER*nchartitle,
     .   TITR1
!
      my_real,
     .   DIMENSION(:), POINTER :: UVAR
C=======================================================================
!---
!     INITIAL USER VARIABLES
!---
      DO I=LFT,LLT
        II = NFT+I
        JJ = PTBEAM(II)
        IF (JJ > 0) THEN
          NPTI  = NINT(SIGBEAM(NVBEAM + 2,JJ))
          IGTYP = NINT(SIGBEAM(NVBEAM + 3,JJ))
          NVARS = NINT(SIGBEAM(NVBEAM + 4,JJ))
!---
!
! check NPT /= NPTI
!
          IF (NPT /= NPTI  .AND . NPTI /= 0) THEN
            IPID=IXP(5,I)
            PID=IGEO(1,IXP(5,I))
            CALL FRETITL2(TITR1,IGEO(NPROPGI-LTITR+1,IPID),LTITR)
            CALL ANCMSG(MSGID=1237,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=PID,
     .                  I2=IXP(NIXP,I),
     .                  C1=TITR1)
          ENDIF
!
          DO IPT=1,NPT
            ILAY=1
            IR = 1
            IS = 1
            NUVAR = ELBUF_STR%BUFLY(ILAY)%NVAR_MAT
            UVAR => ELBUF_STR%BUFLY(ILAY)%MAT(IR,IS,IPT)%VAR
            DO NU = 1,MIN(NVARS,NUVAR)
              UVAR((NU-1)*NEL + I) = 
     .              SIGBEAM(NVBEAM + 4  + NU + (IPT -1)*NVARS,JJ)
            ENDDO
          ENDDO  !  DO ILAY=1,NPT
!---!
! only test print
!           NU = MAX(1,(NVARS - MOD(NVARS,5))/5)
!           KN = MIN(5,NVARS)
!           PT = 0
!           DO IPT = 1,NPT
!             DO J1 = 1,NU
!               JJ1 = (J1 -1)*5
!               WRITE(IOUT,FMT=FMT_5F)(SIGBEAM(NVBEAM+4+PT+K+JJ1,JJ),K=1,KN)
!             ENDDO
!             IF (NUVAR > 5*NU)THEN
!               WRITE(IOUT,FMT=FMT_5F)
!     .             (SIGBEAM(NVBEAM+4+PT+NU*5+K,JJ),K=1,NVARS-5*NU)
!             ENDIF
!             PT = PT + NVARS
!           ENDDO ! DO IP = 1,NIP
!---!
!---
        ENDIF ! IF (JJ > 0)
      ENDDO ! DO I=JFT,JLT
C-----------
      RETURN
      END
