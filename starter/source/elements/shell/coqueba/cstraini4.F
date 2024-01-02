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
Chd|  CSTRAINI4                     source/elements/shell/coqueba/cstraini4.F
Chd|-- called by -----------
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CG2LEPS                       source/elements/shell/coqueba/scigini4.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE CSTRAINI4(
     1           JFT    ,JLT   ,NFT    ,NEL    ,NUMSHEL,
     2           ISTRAIN,GSTR   ,SIGSH ,NSIGSH ,NUMEL  ,
     4           IX     ,NIX    ,PTSH  ,THKE   ,GSTRM  ,
     7           E1X    ,E2X    ,E3X    ,E1Y   ,E2Y    ,
     8           E3Y    ,E1Z    ,E2Z    ,E3Z   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: JFT,JLT,NUMEL,NIX,NFT,
     .                       ISTRAIN,NEL,NSIGSH,NUMSHEL
      INTEGER, DIMENSION(NIX,NUMEL), INTENT(IN) :: IX
      INTEGER, DIMENSION(NUMEL), INTENT(IN) :: PTSH
      my_real, DIMENSION(NSIGSH,NUMSHEL),INTENT(IN) :: SIGSH
      my_real, DIMENSION(NEL,8,4),INTENT(OUT) :: GSTR
      my_real, DIMENSION(NEL,8),  INTENT(OUT) :: GSTRM
      my_real, DIMENSION(NEL),INTENT(IN) :: THKE 
      my_real, DIMENSION(MVSIZ),INTENT(IN) :: 
     .               E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z  
C------------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,JJ,N,NPTI,I1,I2,PT,PID1,IPID1,L_PLA,NLAY,
     .   ILAY,LAYNPT_MAX,LAY_MAX,NPTT,NPTMX,IP,PTS,LENS,IPT_ALL,
     .   IPT,PTN,JDIR,ILAW,NPGI,NPG
      INTEGER LI(MVSIZ)
      my_real 
     .    E1(6),E2(6),Z1,Z2,Z0,AA,E1G(6,4),E2G(6,4),Z1G(4),Z2G(4),UNG,R1
C=======================================================================
!
      NPG=4
      DO I=JFT,JLT
        IF (ABS(ISIGI) /= 3.AND.ABS(ISIGI) /= 4.AND.ABS(ISIGI) /= 5)THEN
          II = I+NFT
          N = NINT(SIGSH(1,II))
          IF (N /= IX(NIX,II)) THEN
            JJ = II
            DO J = 1,NUMEL
              II= J
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
        NPTI=NINT(SIGSH(2,II))
        NPGI=NINT(SIGSH(NVSHELL,II))
C--- in global sys NPGI=4 for QEPH like stress now
         IF(SIGSH(17,II) == ONE .AND. NPGI==NPG )THEN
            PT = INISHVAR1          
             IF (NPTI==1) THEN
               DO IP = 1,NPG
                 E1(1:6) = SIGSH(PT:PT+5,II)
                 Z1 = SIGSH(PT+6,II)
                   CALL CG2LEPS(
     7              E1X(I) ,E2X(I),E3X(I),E1Y(I),E2Y(I),E3Y(I),
     8              E1Z(I) ,E2Z(I),E3Z(I),E1   )
                   GSTR(I,1:5,IP) = E1(1:5)
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
                  GSTR(I,1:5,IP)=E1G(1:5,IP)
                  Z0 = AA*Z2G(IP)
                  GSTR(I,6:8,IP)=(E2G(1:3,IP)-E1G(1:3,IP))/Z0
                 ELSEIF (Z2G(IP)==ZERO) THEN
                  GSTR(I,1:5,IP)=E2G(1:5,IP)
                  Z0 = AA*Z1G(IP)
                  GSTR(I,6:8,IP)=(E1G(1:3,IP)-E2G(1:3,IP))/Z0
                 ELSE
                  Z0 = AA*(Z2G(IP)-Z1G(IP))
                  GSTR(I,6:8,IP)=(E2G(1:3,IP)-E1G(1:3,IP))/Z0
                  GSTR(I,1:3,IP)=E1G(1:3,IP)-AA*Z1G(IP)*GSTR(I,6:8,IP)
                  GSTR(I,4:5,IP)= HALF*(E2G(4:5,IP)+E1G(4:5,IP))
                 END IF
               END DO 
             END IF
           GSTRM(I,1:8) = ZERO
           UNG = ONE/NPG          
           DO IP = 1,NPG
             GSTRM(I,1:8) = GSTRM(I,1:8) + UNG*GSTR(I,1:8,IP)
           END DO
         ENDIF
 100  CONTINUE
      ENDDO
C-----------
      RETURN
      END
