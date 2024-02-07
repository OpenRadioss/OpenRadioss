Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  CSIGINI4                      source/elements/shell/coqueba/scigini4.F
Chd|-- called by -----------
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CDKINIT3                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CG2LEPS                       source/elements/shell/coqueba/scigini4.F
Chd|        CG2LSIG                       source/elements/shell/coqueba/scigini4.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        LOC2ORTH                      source/elements/shell/coqueba/scigini4.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE CSIGINI4(ELBUF_STR,
     1           IHBE   ,JFT    ,JLT   ,NFT    ,NPT    ,
     2           ISTRAIN,THK    ,EINT  ,GSTR   ,HH     ,
     3           FOR    ,MOM    ,SIGSH ,NSIGSH ,NUMEL  ,
     4           IX     ,NIX    ,NUMSH ,PTSH   ,IGEO   ,
     5           IR     ,IS     ,IPG   ,NPG    ,G_PLA  ,
     6           EPSP   ,THKE   ,IGTYP ,NEL    ,ISIGSH ,
     7           E1X    ,E2X    ,E3X   ,E1Y    ,E2Y  ,E3Y,
     8           E1Z    ,E2Z   ,E3Z   ,DIR_A   ,DIR_B  ,
     9           POSLY  )
!     points Gauss a corriger !!!!!!
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
#include      "com01_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,IR,IS,IT,NUMEL,NIX,NFT,NPT,ISTRAIN,NEL
      INTEGER IX(NIX,*),NSIGSH ,IHBE ,NPGI,IPG,NPG,G_PLA,
     .        NUMSH, PTSH(*),IGEO(NPROPGI,*),IGTYP,ISIGSH
      my_real
     . THK(*)   ,EINT(JLT,2),GSTR(NEL,8),SIGSH(NSIGSH,*),
     .   E1X(MVSIZ),E2X(MVSIZ),E3X(MVSIZ),
     .   E1Y(MVSIZ),E2Y(MVSIZ),E3Y(MVSIZ),
     .   E1Z(MVSIZ),E2Z(MVSIZ),E3Z(MVSIZ),DIR_A(*),DIR_B(*),  
     .   FOR(NEL,5) ,MOM(NEL,3) ,HH(NEL,12),EPSP(*),THKE(*),POSLY(MVSIZ,*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C------------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER*nchartitle,
     .   TITR
      INTEGER I,II,J,JJ,KK(8),N,NPTI,I1,I2,PT,PID1,IPID1,L_PLA,NLAY,
     .   ILAY,LAYNPT_MAX,LAY_MAX,NPTT,NPTMX,IP,PTS,LENS,IPT_ALL,
     .   IPT,PTN,JDIR,ILAW
      INTEGER LI(MVSIZ)
      PARAMETER (LAYNPT_MAX = 10)
      PARAMETER (LAY_MAX = 100)
      my_real
     . S1(LAYNPT_MAX*LAY_MAX),S2(LAYNPT_MAX*LAY_MAX),S3(LAYNPT_MAX*LAY_MAX),
     . S4(LAYNPT_MAX*LAY_MAX),SM(LAYNPT_MAX*LAY_MAX),PG2I,PG,FM,S6(6),
     . POSI(MVSIZ,NPT)
      my_real 
     .    E1(6),E2(6),Z1,Z2,Z0,AA,E1G(6,4),E2G(6,4),Z1G(4),Z2G(4),UNG,
     .    FORJ(5,4),MOMJ(3,4),TJ,UNPT
C--------------------------------------------------------------
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
c
      PARAMETER (PG=.577350269189626)
C=======================================================================
      GBUF => ELBUF_STR%GBUF
      NLAY = ELBUF_STR%NLAY
!
      DO I=1,8
        KK(I) = NEL*(I-1)
      ENDDO
!       
      IF (IHBE == 23) NPG=4
      DO I=JFT,JLT
        IF (ABS(ISIGI) /= 3.AND.ABS(ISIGI) /= 4.AND.ABS(ISIGI) /= 5)THEN
          II = PTSH(I + NFT)
          IF(II == 0) GOTO 100
          N = NINT(SIGSH(1,II))
          IF (N /= IX(NIX,I + NFT)) THEN
            JJ = I + NFT
            DO J = 1,NUMEL
              II = J
              N = NINT(SIGSH(1,II))
              IF (N == 0) GOTO 100
              IF (N == IX(NIX,JJ)) GOTO 60
            ENDDO
 60         CONTINUE
          ENDIF
        ELSE
          JJ=NFT+I
          N =IX(NIX,JJ)
          II=PTSH(JJ)
          IF (II == 0) GOTO 100
        ENDIF
        LI(I) = II
        IF(SIGSH(NVSHELL - 1 ,II) == ZERO ) CYCLE
        NPTI=NINT(SIGSH(2,II))
        NPGI=NINT(SIGSH(NVSHELL,II))
        IF (SIGSH(3,II) /= ZERO) THEN
          THK(I)=SIGSH(3,II)
          THKE(I)=THK(I)
        ENDIF
        EINT(I,1)=SIGSH(4,II)
        EINT(I,2)=SIGSH(5,II)
C for IGTYP == 51, NPT now =NPTI
        IF ((NPT /= NPTI.AND.NPT/=0) .OR. NPG /= NPGI) THEN
             IPID1=IX(NIX-1,NFT+I) 
             PID1=IGEO(1,IPID1)
             CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,IPID1),LTITR)
             IF (NPT > 0 .AND. (NPTI == 0 .AND. ITHKSHEL /= 2)) THEN
C----limite number of error out
               IF (IPG<=1) then 
                   CALL ANCMSG(MSGID=2020,
     .                   ANMODE=ANINFO,
     .                   MSGTYPE=MSGERROR,
c     .                   C1=TITR,
     .                   I1=PID1,
     .                   I2=N,
     .                   PRMOD=MSG_CUMU)
              ENDIF
             ELSEIF(ISIGSH /= 0) THEN
               CALL ANCMSG(MSGID=26,
     .                   ANMODE=ANINFO,
     .                   MSGTYPE=MSGERROR,
     .                   C1=TITR,
     .                   I1=PID1,
     .                   I2=N)
             ENDIF
        ENDIF
        IF (ISTRAIN /= 0.AND.ITHKSHEL==2) THEN
C--- reading in another routine for QEPH 
          IF(SIGSH(17,II) == ONE .AND. IHBE /= 23)THEN
            PT = INISHVAR1          
            IF(NPGI <= 1.OR.IHBE == 23)THEN
             IF (NPTI==1) THEN
              E1(1:6) = SIGSH(PT:PT+5,II)
              Z1 = SIGSH(PT+6,II)
              CALL CG2LEPS(
     7           E1X(I) ,E2X(I),E3X(I),E1Y(I),E2Y(I),E3Y(I),
     8           E1Z(I) ,E2Z(I),E3Z(I),E1   )
              GSTR(I,1:5)=E1(1:5)
             ELSE
              E1(1:6) = SIGSH(PT:PT+5,II)
              Z1 = SIGSH(PT+6,II)
              E2(1:6) = SIGSH(PT+7:PT+12,II)
              Z2 = SIGSH(PT+13,II)
              AA = HALF*THKE(I)
              CALL CG2LEPS(
     7           E1X(I) ,E2X(I),E3X(I),E1Y(I),E2Y(I),E3Y(I),
     8           E1Z(I) ,E2Z(I),E3Z(I),E1   )
              CALL CG2LEPS(
     7           E1X(I) ,E2X(I),E3X(I),E1Y(I),E2Y(I),E3Y(I),
     8           E1Z(I) ,E2Z(I),E3Z(I),E2   )
              IF (Z1==Z2) THEN
c             error out
               CALL ANCMSG(MSGID=1904,
     .                ANMODE=ANINFO,
     .                MSGTYPE=MSGERROR,
     .                I1=N,
     .                R1=Z1)
              ELSEIF (Z1==ZERO) THEN
               GSTR(I,1:5)=E1(1:5)
               Z0 = AA*Z2
               GSTR(I,6:8)=(E2(1:3)-E1(1:3))/Z0
              ELSEIF (Z2==ZERO) THEN
               GSTR(I,1:5)=E2(1:5)
               Z0 = AA*Z1
               GSTR(I,6:8)=(E1(1:3)-E2(1:3))/Z0
              ELSE
               Z0 = AA*(Z2-Z1)
               GSTR(I,6:8)=(E2(1:3)-E1(1:3))/Z0
               GSTR(I,1:3)=E1(1:3)-AA*Z1*GSTR(I,6:8)
               GSTR(I,4:5)= HALF*(E2(4:5) + E1(4:5))
              END IF
             END IF
C------   NPG > 1 : GSTR-> GBUF%STRPG        
            ELSE
             IF (NPTI==1) THEN
               DO IP = 1,NPG
                 E1(1:6) = SIGSH(PT:PT+5,II)
                 Z1 = SIGSH(PT+6,II)
                 IF (IP==IPG) THEN
                   CALL CG2LEPS(
     7              E1X(I) ,E2X(I),E3X(I),E1Y(I),E2Y(I),E3Y(I),
     8              E1Z(I) ,E2Z(I),E3Z(I),E1   )
c                    GBUF%STRPG(PTS+KK(1:5))=E1(1:5)
                   GSTR(I,1:5) = E1(1:5)
                 END IF !(IP==IPG) 
                 PT = PT + 7
               END DO 
             ELSE
               AA = HALF*THKE(I)
               DO IP = 1,NPG
                 E1G(1:6,IP) = SIGSH(PT:PT+5,II)
                 Z1G(IP) = SIGSH(PT+6,II)
                 PT = PT + 7
               END DO 
               DO IP = 1,NPG
                 E2G(1:6,IP) = SIGSH(PT:PT+5,II)
                 Z2G(IP) = SIGSH(PT+6,II)
                 PT = PT + 7
               END DO 
               DO IP = 1,NPG
                IF (IP==IPG) THEN
                 CALL CG2LEPS(
     7              E1X(I) ,E2X(I),E3X(I),E1Y(I),E2Y(I),E3Y(I),
     8              E1Z(I) ,E2Z(I),E3Z(I),E1G(1,IP))
                 CALL CG2LEPS(
     7              E1X(I) ,E2X(I),E3X(I),E1Y(I),E2Y(I),E3Y(I),
     8              E1Z(I) ,E2Z(I),E3Z(I),E2G(1,IP))
                 IF (Z1G(IP)==Z2G(IP)) THEN
c                error out
                 CALL ANCMSG(MSGID=1904,
     .                ANMODE=ANINFO,
     .                MSGTYPE=MSGERROR,
     .                I1=N,
     .                R1=Z1G(IP))
                 ELSEIF (Z1G(IP)==ZERO) THEN
                  GSTR(I,1:5)=E1G(1:5,IP)
                  Z0 = AA*Z2G(IP)
                  GSTR(I,6:8)=(E2G(1:3,IP)-E1G(1:3,IP))/Z0
                 ELSEIF (Z2G(IP)==ZERO) THEN
                  GSTR(I,1:5)=E2G(1:5,IP)
                  Z0 = AA*Z1G(IP)
                  GSTR(I,6:8)=(E1G(1:3,IP)-E2G(1:3,IP))/Z0
                 ELSE
                  Z0 = AA*(Z2G(IP)-Z1G(IP))
                  GSTR(I,6:8)=(E2G(1:3,IP)-E1G(1:3,IP))/Z0
                  GSTR(I,1:3)=E1G(1:3,IP)-AA*Z1G(IP)*GSTR(I,6:8)
                  GSTR(I,4:5)= HALF*(E2G(4:5,IP)+E1G(4:5,IP))
                 END IF
                END IF !(IP==IPG) THEN
               END DO 
             END IF
            END IF !IF(NPGI <= 1
          ELSE
            GSTR(I,1)=SIGSH(6,II)
            GSTR(I,2)=SIGSH(7,II)
            GSTR(I,3)=SIGSH(8,II)
            GSTR(I,4)=SIGSH(9,II)
            GSTR(I,5)=SIGSH(10,II)
            GSTR(I,6)=SIGSH(11,II)
            GSTR(I,7)=SIGSH(12,II)
            GSTR(I,8)=SIGSH(13,II)
          ENDIF
        ENDIF
        IF (ISIGSH==0) CYCLE
C--- in global sys modifying directly SIGSH
        IF(SIGSH(17,II) == ONE)THEN
          IF (NPT == 0)THEN
           IF (NPTI>1) THEN
            UNPT = ONE/NPTI
            IF (NPG>1) THEN
              IF (IHBE == 23) THEN      !  QEPH-----
                PT = 22 
                FORJ(1:5,1:4) = ZERO
                MOMJ(1:3,1:4) = ZERO
                DO IT=1,NPTI
                  DO IP=1,NPG
                    S6(1:6)=SIGSH(PT:PT+5,II)
                    CALL CG2LSIG(
     7              E1X(I) ,E2X(I),E3X(I),E1Y(I),E2Y(I),E3Y(I),
     8              E1Z(I) ,E2Z(I),E3Z(I),S6   )
                    TJ=SIX*SIGSH(PT+7,II)
                    PT = PT + 8
                    FORJ(1:5,IP) = FORJ(1:5,IP) + UNPT*S6(1:5)
                    IF (TJ>ZERO) MOMJ(1:3,IP) = S6(1:3)/TJ
                  END DO 
                ENDDO
               DO IP =1,NPG
                PTN= 22 + 9*(IP-1)
                SIGSH(PTN:PTN+4,II)=FORJ(1:5,IP)
                SIGSH(PTN+5,II)=ZERO
                SIGSH(PTN+6:PTN+8,II)=MOMJ(1:3,IP)
               END DO !IP =1,NPG
              ELSE
                PT = 22 + 8*(IPG -1)
                FORJ(1:5,1) = ZERO
                MOMJ(1:3,1) = ZERO
                DO IT=1,NPTI
                  S6(1:6)=SIGSH(PT:PT+5,II)
                  CALL CG2LSIG(
     7            E1X(I) ,E2X(I),E3X(I),E1Y(I),E2Y(I),E3Y(I),
     8            E1Z(I) ,E2Z(I),E3Z(I),S6   )
                  TJ=SIX*SIGSH(PT+7,II)
                  PT = PT + 8
                  FORJ(1:5,1) = FORJ(1:5,1) + UNPT*S6(1:5)
                  IF (TJ>ZERO) MOMJ(1:3,1) = S6(1:3)/TJ
                ENDDO
                FOR(I,1:5)=FORJ(1:5,1)
                MOM(I,1:3)=MOMJ(1:3,1)
              END IF ! IHBE
            ELSE
C------ Q4,T3 
              FORJ(1:5,1) = ZERO
              MOMJ(1:3,1) = ZERO
               DO IT=1,NPTI
                 PT = 22 + 6*(IT-1)
                 S6(1:2)=SIGSH(PT:PT+1,II)
                 S6(3)=SIGSH(INISHVAR+IT,II)
                 S6(4:6)=SIGSH(PT+2:PT+4,II)
                 TJ=SIX*SIGSH(INISHVAR+NPTI+IT,II)
                CALL CG2LSIG(
     7          E1X(I) ,E2X(I),E3X(I),E1Y(I),E2Y(I),E3Y(I),
     8          E1Z(I) ,E2Z(I),E3Z(I),S6   )
                FORJ(1:5,1) = FORJ(1:5,1) + UNPT*S6(1:5)
                IF (TJ>ZERO) MOMJ(1:3,1) = S6(1:3)/TJ
               ENDDO
               SIGSH(22:26,II)=FORJ(1:5,1)
               SIGSH(27:29,II)=MOMJ(1:3,1)
            END IF !(NPG>1) THEN
           ELSE ! NPTI=0
C            
            IF (NPG>1) THEN
              IF (IHBE == 23) THEN
               DO IP =1,NPG
                PT = 22 + 13*(IP-1)
                PTN= 22 + 9*(IP-1)
!              
                S6(1:6)=SIGSH(PT:PT+5,II)
                CALL CG2LSIG(
     7            E1X(I) ,E2X(I),E3X(I),E1Y(I),E2Y(I),E3Y(I),
     8            E1Z(I) ,E2Z(I),E3Z(I),S6   )
                SIGSH(PTN:PTN+4,II)=S6(1:5)
                SIGSH(PTN+5,II)=SIGSH(PT+12,II)
                S6(1:6)=SIGSH(PT+6:PT+11,II)
                CALL CG2LSIG(
     7              E1X(I) ,E2X(I),E3X(I),E1Y(I),E2Y(I),E3Y(I),
     8              E1Z(I) ,E2Z(I),E3Z(I),S6   )
                SIGSH(PTN+6:PTN+8,II)=S6(1:3)
               END DO !IP =1,NPG
              ELSE
                PT = 22 + 13*(IPG-1)
                L_PLA = ELBUF_STR%BUFLY(1)%L_PLA
                LBUF => ELBUF_STR%BUFLY(1)%LBUF(IR,IS,1)
                S6(1:6)=SIGSH(PT:PT+5,II)
                CALL CG2LSIG(
     7              E1X(I) ,E2X(I),E3X(I),E1Y(I),E2Y(I),E3Y(I),
     8              E1Z(I) ,E2Z(I),E3Z(I),S6   )
C     
                IF (NPG<4) S6(4:5)=ZERO 
                FOR(I,1:5)=S6(1:5)
                IF (L_PLA > 0) LBUF%PLA(I)=SIGSH(PT+12,II)
                S6(1:6)=SIGSH(PT+6:PT+11,II)
                CALL CG2LSIG(
     7              E1X(I) ,E2X(I),E3X(I),E1Y(I),E2Y(I),E3Y(I),
     8              E1Z(I) ,E2Z(I),E3Z(I),S6   )
                 MOM(I,1:3)=S6(1:3)
              END IF ! IHBE
C------------NPG=1               
            ELSE
              S6(1:2)=SIGSH(22:23,II)
              S6(3)=SIGSH(18,II)
              S6(4:6)=SIGSH(24:26,II)
              CALL CG2LSIG(
     7           E1X(I) ,E2X(I),E3X(I),E1Y(I),E2Y(I),E3Y(I),
     8           E1Z(I) ,E2Z(I),E3Z(I),S6   )
              SIGSH(22:26,II)=S6(1:5)
C---MOM              
              S6(1:2)=SIGSH(28:29,II)
              S6(3)=SIGSH(19,II)
              S6(4)=SIGSH(30,II)
              S6(5:6)=SIGSH(20:21,II)
              CALL CG2LSIG(
     7           E1X(I) ,E2X(I),E3X(I),E1Y(I),E2Y(I),E3Y(I),
     8           E1Z(I) ,E2Z(I),E3Z(I),S6   )
              SIGSH(27:29,II)=S6(1:3)
            END IF ! NPG
           END IF !(NPTI>1) THEN
          ELSE
C-------npt>1       
            IF (NPG>1) THEN
              IF (IHBE == 23) THEN      !  QEPH-----
                PT = 22 
                PTN = 22 
                IPT_ALL = 0           
                DO ILAY=1,NLAY
                   NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
                   ILAW = ELBUF_STR%BUFLY(ILAY)%ILAW
                   DO IT=1,NPTT
                     IPT =IPT_ALL+IT
                     JDIR = 1 + (ILAY-1)*NEL*2
                     JJ = JDIR + I-1
                     DO IP=1,NPG
                       S6(1:6)=SIGSH(PT:PT+5,II)
                       CALL CG2LSIG(
     7                 E1X(I) ,E2X(I),E3X(I),E1Y(I),E2Y(I),E3Y(I),
     8                 E1Z(I) ,E2Z(I),E3Z(I),S6   )
                       CALL LOC2ORTH(S6,DIR_A,DIR_B,JJ,ILAW,IGTYP,NEL)
                       SIGSH(PTN:PTN+4,II) = S6(1:5)
                       SIGSH(PTN+5,II) = SIGSH(PT+6,II)
                       POSI(I,IPT)=SIGSH(PT+7,II)
                       PT = PT + 8
                       PTN = PTN + 6
                     END DO !IT=1,NPG
                   ENDDO
                   IPT_ALL = IPT_ALL +  NPTT         
                ENDDO
              ELSE
C----------unlike QEPH, SIGSH can't be updated w/ reduced dim              
                PT = 22 + 8*(IPG -1)
                IPT_ALL = 0           
                DO ILAY=1,NLAY
                   NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
                   ILAW = ELBUF_STR%BUFLY(ILAY)%ILAW
                   L_PLA = ELBUF_STR%BUFLY(ILAY)%L_PLA
                   JDIR = 1 + (ILAY-1)*NEL*2
                   JJ = JDIR + I-1
                   DO IT=1,NPTT
                     LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(IR,IS,IT)
                     IPT =IPT_ALL+IT
                       S6(1:6)=SIGSH(PT:PT+5,II)
                       CALL CG2LSIG(
     7                 E1X(I) ,E2X(I),E3X(I),E1Y(I),E2Y(I),E3Y(I),
     8                 E1Z(I) ,E2Z(I),E3Z(I),S6   )
C---------no transvers shear for DKT
                      IF (NPG<4) S6(4:5)=ZERO 
                       CALL LOC2ORTH(S6,DIR_A,DIR_B,JJ,ILAW,IGTYP,NEL)
                       LBUF%SIG(KK(1:5)+I) = S6(1:5)
                       IF (L_PLA > 0) LBUF%PLA(I) = SIGSH(PT+6,II)
                       POSI(I,IPT)=SIGSH(PT+7,II)
                       PT = PT + 8*NPG
                   ENDDO
                   IPT_ALL = IPT_ALL +  NPTT         
                ENDDO
              END IF !(IHBE == 23)
            ELSE
C------ Q4,T3 
              IPT_ALL = 0           
              DO ILAY=1,NLAY
                 NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
                 ILAW = ELBUF_STR%BUFLY(ILAY)%ILAW
                 DO IT=1,NPTT
                   IPT =IPT_ALL+IT
                   JDIR = 1 + (ILAY-1)*NEL*2
                   JJ = JDIR + I-1
                   PT = 22 + 6*(IPT-1)
                   S6(1:2)=SIGSH(PT:PT+1,II)
                   S6(3)=SIGSH(INISHVAR+IPT,II)
                   S6(4:6)=SIGSH(PT+2:PT+4,II)
                   POSI(I,IPT)=SIGSH(INISHVAR+NPT+IPT,II)
                  CALL CG2LSIG(
     7              E1X(I) ,E2X(I),E3X(I),E1Y(I),E2Y(I),E3Y(I),
     8              E1Z(I) ,E2Z(I),E3Z(I),S6   )
                  CALL LOC2ORTH(S6,DIR_A,DIR_B,JJ,ILAW,IGTYP,NEL)
                  SIGSH(PT:PT+4,II) = S6(1:5)
                 ENDDO
                 IPT_ALL = IPT_ALL +  NPTT         
              ENDDO
            END IF
          END IF
        ENDIF
        IF (IHBE == 23) THEN      !  QEPH-----
c---
          PG2I=HALF/PG
          IF (NPT == 0)THEN

            PT = 22
            S1(1)=SIGSH(PT,II)
            S2(1)=SIGSH(PT+9,II)
            S3(1)=SIGSH(PT+18,II)
            S4(1)=SIGSH(PT+27,II)
            FOR(I,1)=FOURTH*(S1(1)+S2(1)+S3(1)+S4(1))
            HH(I,1)=(S3(1)+S4(1)-TWO*FOR(I,1))*PG2I
            HH(I,7)=-(S2(1)+S3(1)-TWO*FOR(I,1))*PG2I         

            PT = 23
            S1(1)=SIGSH(PT,II)
            S2(1)=SIGSH(PT+9,II)
            S3(1)=SIGSH(PT+18,II)
            S4(1)=SIGSH(PT+27,II)
            FOR(I,2)=FOURTH*(S1(1)+S2(1)+S3(1)+S4(1))
            HH(I,2)=-(S3(1)+S4(1)-TWO*FOR(I,2))*PG2I
            HH(I,8)=(S2(1)+S3(1)-TWO*FOR(I,2))*PG2I

            PT = 24
            S1(1)=SIGSH(PT,II)
            S2(1)=SIGSH(PT+9,II)
            S3(1)=SIGSH(PT+18,II)
            S4(1)=SIGSH(PT+27,II)
            FOR(I,3)=FOURTH*(S1(1)+S2(1)+S3(1)+S4(1))

            PT = 25
            S1(1)=SIGSH(PT,II)
            S2(1)=SIGSH(PT+9,II)
            S3(1)=SIGSH(PT+18,II)
            S4(1)=SIGSH(PT+27,II)
            FOR(I,4)=FOURTH*(S1(1)+S2(1)+S3(1)+S4(1))
            HH(I,6)=-(S3(1)+S4(1)-TWO*FOR(I,4))*PG2I
            HH(I,12)=(S2(1)+S3(1)-TWO*FOR(I,4))*PG2I

            PT = 26
            S1(1)=SIGSH(PT,II)
            S2(1)=SIGSH(PT+9,II)
            S3(1)=SIGSH(PT+18,II)
            S4(1)=SIGSH(PT+27,II)
            FOR(I,5)=FOURTH*(S1(1)+S2(1)+S3(1)+S4(1))
            HH(I,5)=(S3(1)+S4(1)-TWO*FOR(I,5))*PG2I
            HH(I,11)=-(S2(1)+S3(1)-TWO*FOR(I,5))*PG2I

            PT = 27
            S1(1)=SIGSH(PT,II)
            S2(1)=SIGSH(PT+9,II)
            S3(1)=SIGSH(PT+18,II)
            S4(1)=SIGSH(PT+27,II)
            IF (G_PLA > 0) EPSP(I)=MIN(S1(1),S2(1),S3(1),S4(1))

            PT = 28
            S1(1)=SIGSH(PT,II)
            S2(1)=SIGSH(PT+9,II)
            S3(1)=SIGSH(PT+18,II)
            S4(1)=SIGSH(PT+27,II)
            MOM(I,1)=FOURTH*(S1(1)+S2(1)+S3(1)+S4(1))
            HH(I,3)= (S3(1)+S4(1)-TWO*MOM(I,1))*PG2I
            HH(I,9)=-(S2(1)+S3(1)-TWO*MOM(I,1))*PG2I

            PT = 29
            S1(1)=SIGSH(PT,II)
            S2(1)=SIGSH(PT+9,II)
            S3(1)=SIGSH(PT+18,II)
            S4(1)=SIGSH(PT+27,II)
            MOM(I,2)=FOURTH*(S1(1)+S2(1)+S3(1)+S4(1))
            HH(I,4)=-(S3(1)+S4(1)-TWO*MOM(I,2))*PG2I
            HH(I,10)=(S2(1)+S3(1)-TWO*MOM(I,2))*PG2I

            PT = 30
            S1(1)=SIGSH(PT,II)
            S2(1)=SIGSH(PT+9,II)
            S3(1)=SIGSH(PT+18,II)
            S4(1)=SIGSH(PT+27,II)
            MOM(I,3)=FOURTH*(S1(1)+S2(1)+S3(1)+S4(1))

          ELSE  ! IHBE=23, NPT > 0

            FM = -PG2I/THK(I)
C---------Sx-----------------
            PT = 22
            NPTMX = 0
            DO ILAY=1,NLAY
              NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
              DO IT=1,NPTT
                LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(IR,IS,IT)
                IP = NPTMX + IT
                S1(IP)=SIGSH(PT,II)
                S2(IP)=SIGSH(PT+6,II)
                S3(IP)=SIGSH(PT+12,II)
                S4(IP)=SIGSH(PT+18,II)
                SM(IP)=FOURTH*(S1(IP)+S2(IP)+S3(IP)+S4(IP))
                LBUF%SIG(KK(1)+I)=SM(IP)
                PT = PT + 6*NPG
              ENDDO
              NPTMX = NPTMX + NPTT
            ENDDO
            HH(I,1)=HALF*PG2I*(S3(1)+S4(1)-TWO*SM(1)+
     .                           S3(NPTMX)+S4(NPTMX)-TWO*SM(NPTMX))
            HH(I,7)=-HALF*PG2I*(S2(1)+S3(1)-TWO*SM(1)+
     .                            S2(NPTMX)+S3(NPTMX)-TWO*SM(NPTMX))
            HH(I,3)=FM*(S3(1)-S3(NPTMX)+S4(1)-S4(NPTMX)
     .                           -TWO*(SM(1)-SM(NPTMX)))
            HH(I,9)=-FM*(S2(1)-S2(NPTMX)+S3(1)-S3(NPTMX)
     .                            -TWO*(SM(1)-SM(NPTMX)))
C---------Sy-----------------------
            PT = 22
            NPTMX = 0
            DO ILAY=1,NLAY
              NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
              DO IT=1,NPTT
                LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(IR,IS,IT)
                IP = NPTMX + IT
                S1(IP)=SIGSH(PT+1,II)
                S2(IP)=SIGSH(PT+7,II)
                S3(IP)=SIGSH(PT+13,II)
                S4(IP)=SIGSH(PT+19,II)
                SM(IP)=FOURTH*(S1(IP)+S2(IP)+S3(IP)+S4(IP))
                LBUF%SIG(KK(2)+I)=SM(IP)
                PT = PT + 6*NPG
              ENDDO
              NPTMX = NPTMX + NPTT
            ENDDO
            HH(I,2)=-HALF*PG2I*(S3(1)+S4(1)-TWO*SM(1)+
     .                            S3(NPTMX)+S4(NPTMX)-TWO*SM(NPTMX))
            HH(I,8)=HALF*PG2I*(S2(1)+S3(1)-TWO*SM(1)+
     .                           S2(NPTMX)+S3(NPTMX)-TWO*SM(NPTMX))
            HH(I,4)=-FM*(S3(1)-S3(NPTMX)+S4(1)-S4(NPTMX)
     .                            -TWO*(SM(1)-SM(NPTMX)))
            HH(I,10)=FM*(S2(1)-S2(NPTMX)+S3(1)-S3(NPTMX)
     .                            -TWO*(SM(1)-SM(NPTMX)))
C---------Sxy,ep-----------------------
            PT = 22
            NPTMX = 0
            DO ILAY=1,NLAY
              NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
              L_PLA = ELBUF_STR%BUFLY(ILAY)%L_PLA
              DO IT=1,NPTT
                LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(IR,IS,IT)
                IP = NPTMX + IT
                S1(IP)=SIGSH(PT+2,II)
                S2(IP)=SIGSH(PT+8,II)
                S3(IP)=SIGSH(PT+14,II)
                S4(IP)=SIGSH(PT+20,II)
                SM(IP)=FOURTH*(S1(IP)+S2(IP)+S3(IP)+S4(IP))
                LBUF%SIG(KK(3)+I)=SM(IP)
                S1(IP)=SIGSH(PT+5,II)
                S2(IP)=SIGSH(PT+11,II)
                S3(IP)=SIGSH(PT+17,II)
                S4(IP)=SIGSH(PT+23,II)
                SM(IP)=MIN(S1(IP),S2(IP),S3(IP),S4(IP))
C                SM(IT)=FOURTH*(S1(IT)+S2(IT)+S3(IT)+S4(IT))
                IF (L_PLA > 0) LBUF%PLA(I)=SM(IP)
                PT = PT + 6*NPG
              ENDDO
              NPTMX = NPTMX + NPTT
            ENDDO
C---------Syz-----------------------
            PT = 22
            NPTMX = 0
            DO ILAY=1,NLAY
              NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
              DO IT=1,NPTT
                LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(IR,IS,IT)
                IP = NPTMX + IT
                S1(IP)=SIGSH(PT+3,II)
                S2(IP)=SIGSH(PT+9,II)
                S3(IP)=SIGSH(PT+15,II)
                S4(IP)=SIGSH(PT+21,II)
                SM(IP)=FOURTH*(S1(IP)+S2(IP)+S3(IP)+S4(IP))
                LBUF%SIG(KK(4)+I)=SM(IP)
                PT = PT + 6*NPG
              ENDDO
              NPTMX = NPTMX + NPTT
            ENDDO
            HH(I,6)=-HALF*PG2I*(S3(1)+S4(1)-TWO*SM(1)+
     .                            S3(NPTMX)+S4(NPTMX)-TWO*SM(NPTMX))
            HH(I,12)=HALF*PG2I*(S2(1)+S3(1)-TWO*SM(1)+
     .                            S2(NPTMX)+S3(NPTMX)-TWO*SM(NPTMX))
C---------Sxz-----------------------
            PT = 22
            NPTMX = 0
            DO ILAY=1,NLAY
              NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
              DO IT=1,NPTT
                LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(IR,IS,IT)
                IP = NPTMX + IT
                S1(IP)=SIGSH(PT+4,II)
                S2(IP)=SIGSH(PT+10,II)
                S3(IP)=SIGSH(PT+16,II)
                S4(IP)=SIGSH(PT+22,II)
                SM(IP)=FOURTH*(S1(IP)+S2(IP)+S3(IP)+S4(IP))
                LBUF%SIG(KK(5)+I)=SM(IP)
                PT = PT + 6*NPG
              ENDDO
              NPTMX = NPTMX + NPTT
            ENDDO
            HH(I,5)=HALF*PG2I*(S3(1)+S4(1)-TWO*SM(1)+
     .                           S3(NPTMX)+S4(NPTMX)-TWO*SM(NPTMX))
            HH(I,11)=-HALF*PG2I*(S2(1)+S3(1)-TWO*SM(1)+
     .                             S2(NPTMX)+S3(NPTMX)-TWO*SM(NPTMX))
          ENDIF
c---
        ELSEIF (IHBE == 11.AND.SIGSH(17,II) == ZERO) THEN  ! QBAT,DKT18-----
c---
          IF (NPT == 0) THEN
            PT = 22 + 9*(IPG-1)
!
            L_PLA = ELBUF_STR%BUFLY(1)%L_PLA
            LBUF => ELBUF_STR%BUFLY(1)%LBUF(IR,IS,1)
!
            FOR(I,1)=SIGSH(PT,II)
            FOR(I,2)=SIGSH(PT+1,II)
            FOR(I,3)=SIGSH(PT+2,II)
            FOR(I,4)=SIGSH(PT+3,II)
            FOR(I,5)=SIGSH(PT+4,II)
            IF (L_PLA > 0) LBUF%PLA(I)=SIGSH(PT+5,II)
            MOM(I,1)=SIGSH(PT+6,II)
            MOM(I,2)=SIGSH(PT+7,II)
            MOM(I,3)=SIGSH(PT+8,II)
          ELSE  !  NPT /= 0       
            PT = 22 + 6*(IPG -1)
            DO ILAY=1,NLAY
              NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
              L_PLA = ELBUF_STR%BUFLY(ILAY)%L_PLA 
              DO IT=1,NPTT
                LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(IR,IS,IT)
                LBUF%SIG(KK(1)+I) = SIGSH(PT,II)
                LBUF%SIG(KK(2)+I) = SIGSH(PT+1,II)
                LBUF%SIG(KK(3)+I) = SIGSH(PT+2,II)
                LBUF%SIG(KK(4)+I) = SIGSH(PT+3,II)
                LBUF%SIG(KK(5)+I) = SIGSH(PT+4,II)
                IF (L_PLA > 0) LBUF%PLA(I) = SIGSH(PT+5,II)
                PT = PT + 6*NPG
              ENDDO
            ENDDO
          ENDIF
c
        ELSEIF (SIGSH(17,II) == ZERO) THEN   !   QPH,QPPS, take account quadrature points only-----
          IF (NPT == 0) THEN
            II = LI(I)
!
            L_PLA = ELBUF_STR%BUFLY(1)%L_PLA
            LBUF => ELBUF_STR%BUFLY(1)%LBUF(1,1,1)
!
            FOR(I,1)=SIGSH(22,II)
            FOR(I,2)=SIGSH(23,II)
            FOR(I,3)=SIGSH(24,II)
            FOR(I,4)=SIGSH(25,II)
            FOR(I,5)=SIGSH(26,II)
            IF (L_PLA > 0) LBUF%PLA(I)=SIGSH(27,II)
            MOM(I,1)=SIGSH(28,II)
            MOM(I,2)=SIGSH(29,II)
            MOM(I,3)=SIGSH(30,II)
          ELSE
            DO ILAY=1,NLAY
              NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
              L_PLA = ELBUF_STR%BUFLY(ILAY)%L_PLA
              DO IT=1,NPTT
                LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(IR,IS,IT)
                LBUF%SIG(KK(1)+I) = SIGSH(22 +(IT-1)*6,II) 
                LBUF%SIG(KK(2)+I) = SIGSH(23 +(IT-1)*6,II) 
                LBUF%SIG(KK(3)+I) = SIGSH(24 +(IT-1)*6,II) 
                LBUF%SIG(KK(4)+I) = SIGSH(25 +(IT-1)*6,II) 
                LBUF%SIG(KK(5)+I) = SIGSH(26 +(IT-1)*6,II) 
                IF (L_PLA > 0) LBUF%PLA(I) = SIGSH(27+(IT-1)*6,II)
              ENDDO
            ENDDO
          ENDIF
        ENDIF ! IF (IHBE == 23)
 100  CONTINUE
      ENDDO
      CALL ANCMSG(MSGID=2020,                 
     .            ANMODE=ANINFO_BLIND_2,
     .            MSGTYPE=MSGERROR,
     .            PRMOD=MSG_PRINT)       
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  CG2LEPS                       source/elements/shell/coqueba/scigini4.F
Chd|-- called by -----------
Chd|        CSIGINI                       source/elements/shell/coque/csigini.F
Chd|        CSIGINI4                      source/elements/shell/coqueba/scigini4.F
Chd|        CSTRAINI4                     source/elements/shell/coqueba/cstraini4.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CG2LEPS(
     7           E1X    ,E2X   ,E3X   ,E1Y   ,E2Y  ,E3Y,
     8           E1Z    ,E2Z   ,E3Z   ,EPS   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   EPS(6),E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z  
C------------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real 
     .       TXX,TYY,TZZ,TXY,TYZ,TZX,UXX,UYY,UZZ,UXY,UYZ,UZX,A,B,C
C---------out EPS : exx,eyy,2exy,2eyz,2ezx,0
             TXX = EPS(1)
             TYY = EPS(2)
             TZZ = EPS(3)
             TXY = EPS(4)
             TYZ = EPS(5)
             TZX = EPS(6)
C
              A = E1X*TXX + E1Y*TXY + E1Z*TZX   
              B = E1X*TXY + E1Y*TYY + E1Z*TYZ   
              C = E1X*TZX + E1Y*TYZ + E1Z*TZZ   
              UXX = A*E1X + B*E1Y + C*E1Z   
              UXY = A*E2X + B*E2Y + C*E2Z 
              UZX = A*E3X + B*E3Y + C*E3Z   
              A = E2X*TXX + E2Y*TXY + E2Z*TZX   
              B = E2X*TXY + E2Y*TYY + E2Z*TYZ   
              C = E2X*TZX + E2Y*TYZ + E2Z*TZZ   
              UYY = A*E2X + B*E2Y + C*E2Z   
              UYZ = A*E3X + B*E3Y + C*E3Z   
             EPS(1) = UXX  
             EPS(2) = UYY  
             EPS(3) = TWO*UXY 
             EPS(4) = TWO*UYZ 
             EPS(5) = TWO*UZX 
             EPS(6) = ZERO 
      RETURN
      END
Chd|====================================================================
Chd|  CG2LSIG                       source/elements/shell/coqueba/scigini4.F
Chd|-- called by -----------
Chd|        CSIGINI                       source/elements/shell/coque/csigini.F
Chd|        CSIGINI4                      source/elements/shell/coqueba/scigini4.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CG2LSIG(
     7           E1X    ,E2X   ,E3X   ,E1Y   ,E2Y  ,E3Y,
     8           E1Z    ,E2Z   ,E3Z   ,SIG   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   SIG(6),E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z  
C------------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real 
     .       TXX,TYY,TZZ,TXY,TYZ,TZX,UXX,UYY,UZZ,UXY,UYZ,UZX,A,B,C
C---------out EPS : exx,eyy,exy,eyz,ezx,0
             TXX = SIG(1)
             TYY = SIG(2)
             TZZ = SIG(3)
             TXY = SIG(4)
             TYZ = SIG(5)
             TZX = SIG(6)
C
              A = E1X*TXX + E1Y*TXY + E1Z*TZX   
              B = E1X*TXY + E1Y*TYY + E1Z*TYZ   
              C = E1X*TZX + E1Y*TYZ + E1Z*TZZ   
              UXX = A*E1X + B*E1Y + C*E1Z   
              UXY = A*E2X + B*E2Y + C*E2Z 
              UZX = A*E3X + B*E3Y + C*E3Z   
              A = E2X*TXX + E2Y*TXY + E2Z*TZX   
              B = E2X*TXY + E2Y*TYY + E2Z*TYZ   
              C = E2X*TZX + E2Y*TYZ + E2Z*TZZ   
              UYY = A*E2X + B*E2Y + C*E2Z   
              UYZ = A*E3X + B*E3Y + C*E3Z   
             SIG(1) = UXX  
             SIG(2) = UYY  
             SIG(3) = UXY 
             SIG(4) = UYZ 
             SIG(5) = UZX 
             SIG(6) = ZERO 
      RETURN
      END
Chd|====================================================================
Chd|  LOC2ORTH                      source/elements/shell/coqueba/scigini4.F
Chd|-- called by -----------
Chd|        CSIGINI                       source/elements/shell/coque/csigini.F
Chd|        CSIGINI4                      source/elements/shell/coqueba/scigini4.F
Chd|-- calls ---------------
Chd|        ROTOVS                        source/elements/shell/coqueba/scigini4.F
Chd|====================================================================
      SUBROUTINE LOC2ORTH(TENS,DIR_A,DIR_B,II,ILAW,IGTYP,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER II,ILAW,IGTYP,NEL
C     REAL
      my_real
     .   TENS(6), DIR_A(*),DIR_B(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J
C     REAL
      my_real
     .   R1,R2,R3,S1,S2,S3,R12A,R22A,S12B,S22B,RS1,RS2,RS3,
     .   T1,T2,T3,PHI,SUM1,SUM2,FACT,R3R3,S3S3 
c------------------------------------------------
          IF (IGTYP /= 1) THEN                                                     
c------------------------------------------------
            IF (IGTYP == 16) THEN                                                  
c
            ELSEIF ((IGTYP == 51 .OR. IGTYP == 52) .AND. ILAW == 58) THEN           
c                II = JDIR + I-1
            ELSE                                                             
              IF (ILAW /= 1 .and. ILAW /= 2 .and. ILAW /= 19 .and. ILAW /= 27 .and. ILAW /= 32)
     .          CALL ROTOVS(TENS,DIR_A(II),DIR_A(II+NEL))
            ENDIF                                                                  
          ENDIF   ! IGTYP                                                          
C
      RETURN
      END
Chd|====================================================================
Chd|  ROTOVS                        source/elements/shell/coqueba/scigini4.F
Chd|-- called by -----------
Chd|        LOC2ORTH                      source/elements/shell/coqueba/scigini4.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ROTOVS(SIG,DIR1,DIR2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   SIG(6), DIR1,DIR2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   S1, S2, S3, S4, S5
C-----------------------------------------------
       S1 = DIR1*DIR1*SIG(1)
     .     +DIR2*DIR2*SIG(2)+TWO*DIR1*DIR2*SIG(3)
       S2 = DIR2*DIR2*SIG(1)
     .     +DIR1*DIR1*SIG(2)-TWO*DIR2*DIR1*SIG(3)
       S3 =-DIR1*DIR2*SIG(1)
     .     +DIR2*DIR1*SIG(2)
     .    +(DIR1*DIR1-DIR2*DIR2)*SIG(3)
       S4 =-DIR2*SIG(5)+DIR1*SIG(4)
       S5 = DIR1*SIG(5)+DIR2*SIG(4)
       SIG(1)=S1
       SIG(2)=S2
       SIG(3)=S3
       SIG(4)=S4
       SIG(5)=S5
C
      RETURN
      END
