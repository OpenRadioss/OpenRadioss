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
Chd|  CMATINI4                      source/materials/mat_share/cmatini4.F
Chd|-- called by -----------
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CDKINIT3                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE CMATINI4(ELBUF_STR,
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
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
      INTEGER I,II,J,JJ,N,NPTI,I1,I2,PT,NPGI,NU,NUVAR,NVARS,IPT,NPI,
     .  IPID1,PID1,C1,IPT_ALL,IT,ILAY,NPTT
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
C--------------------------------------------------------------
      DO I=JFT,JLT
        IF (ABS(ISIGI)/=3 .AND. ABS(ISIGI)/=4 .AND. ABS(ISIGI)/=5)THEN
          II = I+NFT
          N = NINT(SIGSH(NVSHELL + 1,II))
          IF (N.NE.IX(NIX,II)) THEN
            JJ = II
            DO J = 1,NUMEL
              II= J
              N = NINT(SIGSH(NVSHELL + 1,II))
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
        NPI   = NINT(SIGSH(NVSHELL +  2,II))
        NPGI  = NINT(SIGSH(NVSHELL +  3,II))
!!        PT=NVSHELL+3  ! wrong position, overwriting FAIL
        PT = NVSHELL+NUSHELL+NORTSHEL+NVSHELL1+3
C
C for IGTYP == 51, usualy NPT <= NPTI (NPTI = NTPP --> for all layers)
        IF ((NPGI /= NPG.OR.NPI /= NPT) .AND. IGTYP /= 51 
     .                                  .AND. IGTYP /=52) THEN
          IPID1=IX(NIX-1,NFT+I)
          PID1=IGEO(1,IPID1)
          CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,IPID1),LTITR)
          CALL ANCMSG(MSGID=1215,
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
              IPT_ALL = 0
              DO ILAY=1,NLAY
                NPTT  = ELBUF_STR%BUFLY(ILAY)%NPTT
                DO IT=1,NPTT
                  IPT = IPT_ALL + IT

                  LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(IR,IS,IT)
                  LBUF%FAC_YLD(I) = SIGSH(PT+(IPG-1)*NPI+IPT,II)
                ENDDO
                IPT_ALL = IPT_ALL + NPTT
              ENDDO  !  DO ILAY=1,NPT
c
            ELSE
!----------------------------------------------------------
!!  ILAY --> not initialised here
!!  SIGSH(PT, II) --> not filled within initia for NIP = 0
!----------------------------------------------------------
!!             LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(IR,IS,IT)                         
!!             LBUF%FAC_YLD(I) = SIGSH(PT+IPG, II)                                          
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
