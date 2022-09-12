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
Chd|  CUSERINI4                     source/elements/shell/coqueba/cuserini4.F
Chd|-- called by -----------
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CDKINIT3                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE CUSERINI4(ELBUF_STR,
     1           JFT    ,JLT    ,NFT   ,NEL    ,ISTRAIN ,
     2           SIGSH  ,NSIGSH ,NUMEL ,IX     ,NIX     ,
     3           NUMSH  ,PTSH   ,IR    ,IS     ,NPT     ,
     4           IGTYP  ,IGEO   ,NLAY  ,NPG    ,IPG     )
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
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NFT,NEL,IR,IS,NPT,NUMEL,NIX,ISTRAIN,NSIGSH,NUMSH,
     . IGTYP,IGEO(NPROPGI,*),NLAY,NPG,IPG
      INTEGER IX(NIX,*),PTSH(*)
      my_real
     . SIGSH(NSIGSH,*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C------------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER*nchartitle,
     .   TITR
      INTEGER I,J,II,JJ,KK,N,NPTI,I1,I2,PT,NPGI,NU,NUVAR,NVARS,IPT,NPI,
     .  IPID1,PID1,C1,IPT_ALL,IT,ILAY,NPTT,L_SIGB
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      TYPE(BUF_LAY_)  ,POINTER :: BUFLY     
      my_real, DIMENSION(:), POINTER :: UVAR,SIGA,SIGB,SIGC
C--------------------------------------------------------------
      DO I=JFT,JLT
        IF (ABS(ISIGI)/=3 .AND. ABS(ISIGI)/=4 .AND. ABS(ISIGI)/=5)THEN
          II = I+NFT
          N = NINT(SIGSH(1,II))
          IF (N.NE.IX(NIX,II)) THEN
            JJ = II
            DO J = 1,NUMEL
              II= J
              N = NINT(SIGSH(1,II))
              IF (N == 0) GOTO 200
              IF (N == IX(NIX,JJ)) GOTO 60
            ENDDO
 60         CONTINUE
          ENDIF
        ELSE
          JJ=NFT+I
          N =IX(NIX,JJ)
          II=PTSH(JJ)
          IF (II == 0) GOTO 200
        ENDIF
C
        NPI   = NINT(SIGSH(NVSHELL + 2,II))
        NPGI  = NINT(SIGSH(NVSHELL + 3,II))
        NVARS = NINT(SIGSH(NVSHELL + 4,II))
C
        IF ((NPGI /= NPG.OR.NPI /= NPT) .AND. IGTYP/=51 .AND. IGTYP/=52) THEN
c         for IGTYP == 51, usually NPT <= NPTI (NPTI = NTPP --> for all layers)
          IPID1=IX(NIX-1,NFT+I)
          PID1=IGEO(1,IPID1)
          CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,IPID1),LTITR)
          CALL ANCMSG(MSGID=26,
     .                ANMODE=ANINFO,
     .                MSGTYPE=MSGERROR,
     .                C1=TITR,
     .                I1=PID1,
     .                I2=N)
C
        ELSE
          IF (NPGI > 0) THEN
            IF (NPI > 0) THEN
c
              IF (ELBUF_STR%BUFLY(1)%ILAW == 36) THEN  ! backstress is no more stored in uvar
                L_SIGB = ELBUF_STR%BUFLY(1)%L_SIGB
                IF (NVARS > 3 .and. NPI > 0 .and. L_SIGB > 0) THEN
                  KK = NVSHELL + 4 +(IPG-1)*NVARS*NPI
c
                  DO ILAY=1,NLAY
                    BUFLY => ELBUF_STR%BUFLY(ILAY)
                    NPTT  = BUFLY%NPTT
                    NUVAR = BUFLY%NVAR_MAT
                    DO IT=1,NPTT
                      SIGB => BUFLY%LBUF(IR,IS,IT)%SIGB
                      DO J = 1,L_SIGB
                        KK = KK + 1
                        JJ = (J-1)*NEL + I
                        SIGB(JJ) = SIGSH(KK,II)
                      ENDDO
                    ENDDO
                  ENDDO  !  DO ILAY=1,NPT
                ENDIF
c
              ELSE IF (ELBUF_STR%BUFLY(1)%ILAW == 78) THEN  ! backstress is no more stored in uvar
                L_SIGB = ELBUF_STR%BUFLY(1)%L_SIGB
                NUVAR  = ELBUF_STR%BUFLY(1)%NVAR_MAT
                KK  = NVSHELL + 4 +(IPG-1)*NVARS*NPI
c
                DO ILAY=1,NLAY
                  BUFLY => ELBUF_STR%BUFLY(ILAY)
                  NPTT  = BUFLY%NPTT
                  DO IT=1,NPTT
                    UVAR => BUFLY%MAT(IR,IS,IT)%VAR
                    SIGA => BUFLY%LBUF(IR,IS,IT)%SIGA
                    SIGB => BUFLY%LBUF(IR,IS,IT)%SIGB
                    SIGC => BUFLY%LBUF(IR,IS,IT)%SIGC
c
                    DO NU = 1,NUVAR
                      KK = KK + 1
                      JJ = (NU-1)*NEL + I
                      UVAR(JJ) = SIGSH(KK,II)
                    ENDDO
                    DO J = 1,L_SIGB
                      KK = KK + 1
                      JJ = (J-1)*NEL + I
                      SIGA(JJ) = SIGSH(KK,II)
                    ENDDO
                    DO J = 1,L_SIGB
                      KK = KK + 1
                      JJ = (J-1)*NEL + I
                      SIGB(JJ) = SIGSH(KK,II)
                    ENDDO
                    DO J = 1,L_SIGB
                      KK = KK + 1
                      JJ = (J-1)*NEL + I
                      SIGC(JJ) = SIGSH(KK,II)
                    ENDDO
                  ENDDO
                ENDDO  !  DO ILAY=1,NPT
c
              ELSE IF (ELBUF_STR%BUFLY(1)%ILAW == 87) THEN  ! backstress is no more stored in uvar
                L_SIGB = ELBUF_STR%BUFLY(1)%L_SIGB
                NUVAR  = ELBUF_STR%BUFLY(1)%NVAR_MAT
                KK  = NVSHELL + 4 +(IPG-1)*NVARS*NPI
c
                DO ILAY=1,NLAY
                  BUFLY => ELBUF_STR%BUFLY(ILAY)
                  NPTT  = BUFLY%NPTT
                  DO IT=1,NPTT
                    UVAR => BUFLY%MAT(IR,IS,IT)%VAR
   
                    SIGB => BUFLY%LBUF(IR,IS,IT)%SIGB
c
                    DO NU = 1,NUVAR
                      KK = KK + 1
                      JJ = (NU-1)*NEL + I
                      UVAR(JJ) = SIGSH(KK,II)
                    ENDDO
                    DO J = 1,L_SIGB
                      KK = KK + 1
                      JJ = (J-1)*NEL + I
                      SIGB(JJ) = SIGSH(KK,II)
                    ENDDO
                  ENDDO
                ENDDO  !  DO ILAY=1,NPT

              ELSE ! ILAW
                IPT_ALL = 0
                DO ILAY=1,NLAY
                  NPTT  = ELBUF_STR%BUFLY(ILAY)%NPTT
                  NUVAR = ELBUF_STR%BUFLY(ILAY)%NVAR_MAT
                  DO IT=1,NPTT
                    IPT = IPT_ALL + IT
                    UVAR => ELBUF_STR%BUFLY(ILAY)%MAT(IR,IS,IT)%VAR                      
                    DO NU = 1,MIN(NVARS,NUVAR) 
                      UVAR((NU -1)*NEL + I) = 
     .                SIGSH(NVSHELL+4+(IPG-1)*NVARS*NPI+(IPT-1)*NVARS+NU,II)
                    ENDDO
                  ENDDO
                  IPT_ALL = IPT_ALL + NPTT
                ENDDO  !  DO ILAY=1,NPT
              END IF   ! ILAW
c
            ELSE
            ENDIF ! IF (NPI > 0)
          ELSE
          ENDIF ! IF (NPGI > 0)
        ENDIF ! IF ((NPGI /= NPG.OR.NPI /= NPT) .AND. IGTYP /= 51)
C
200    CONTINUE
C
      ENDDO ! DO I=JFT,JLT
C---
      RETURN
      END
