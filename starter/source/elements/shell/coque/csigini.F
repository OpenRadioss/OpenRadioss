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
Chd|  CSIGINI                       source/elements/shell/coque/csigini.F
Chd|-- called by -----------
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CG2LEPS                       source/elements/shell/coqueba/scigini4.F
Chd|        CG2LSIG                       source/elements/shell/coqueba/scigini4.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        LOC2ORTH                      source/elements/shell/coqueba/scigini4.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE CSIGINI(ELBUF_STR,
     1           JFT   ,JLT   ,NFT   ,NPT  ,ISTRAIN,
     2           THK   ,EINT  ,GSTR  ,HH   ,PLAS   ,
     3           FOR   ,MOM   ,SIGSH ,NLAY ,G_HOURG,
     4           NUMEL ,IX    ,NIX   ,NSIGSH,NUMSH ,
     5           PTSH  ,IGEO  ,THKE  ,NEL  ,E1X    ,
     6           E2X   ,E3X   ,E1Y   ,E2Y  ,E3Y    ,
     7           E1Z   ,E2Z   ,E3Z   ,ISIGSH,DIR_A ,
     9           DIR_B ,POSLY ,IGTYP )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE MESSAGE_MOD
C-----------------------------------------------
C////////////////////////////////////////////////
C   ROUTINE GENERIQUE 4NOEUDS-3NOEUDS 
C////////////////////////////////////////////////
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
      INTEGER JFT,JLT,NUMEL,NIX,NFT,NLAY,NPT,ISTRAIN,NSIGSH,NUMSH,
     .   G_HOURG,NEL,ISIGSH,IGTYP
      INTEGER IX(NIX,*),PTSH(*),IGEO(NPROPGI,*)
      my_real
     .   THK(*),EINT(NEL,2),GSTR(NEL,8),FOR(NEL,5),MOM(NEL,3),
     .   HH(NEL,5),PLAS(*),SIGSH(NSIGSH,*),THKE(*),
     .   E1X(MVSIZ),E2X(MVSIZ),E3X(MVSIZ),
     .   E1Y(MVSIZ),E2Y(MVSIZ),E3Y(MVSIZ),
     .   E1Z(MVSIZ),E2Z(MVSIZ),E3Z(MVSIZ),DIR_A(*),DIR_B(*),
     .   POSLY(MVSIZ,*)     
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C------------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, J, JJ, KK(5),N, NPTI, I1, PID, IPID, L_PLA,PT,
     .        ILAY,IPT_ALL,IPT,PTN,JDIR,ILAW,NPTT,IT
      my_real 
     .         TXX,TYY,TZZ,TXY,TYZ,TZX,UXX,UYY,UZZ,UXY,UYZ,UZX,A,B,C,
     .         E1(6),E2(6),Z1,Z2,Z0,AA,S6(6),POSI(MVSIZ,NPT)
      CHARACTER*nchartitle, TITR1
      TYPE(L_BUFEL_) ,POINTER :: LBUF
      TYPE(BUF_LAY_) ,POINTER :: BUFLY    
C=======================================================================
      BUFLY => ELBUF_STR%BUFLY(1)
      L_PLA =  ELBUF_STR%GBUF%G_PLA
!
      DO I=1,5
        KK(I) = NEL*(I-1)
      ENDDO
!
      DO I=JFT,JLT
        IF(ABS(ISIGI) /= 3.AND.ABS(ISIGI) /= 4.AND.ABS(ISIGI) /= 5)THEN
          II = PTSH(I+NFT)
          IF(II == 0 ) GOTO 100
          N = NINT(SIGSH(1,II))
          IF(N == IX(NIX,I+NFT))THEN
            JJ = I+NFT
          ELSE
            JJ = I+NFT
            DO J = 1,NUMEL
              II= J
              N = NINT(SIGSH(1,II))
	      IF (N == 0) GOTO 100
              IF (N == IX(NIX,JJ)) GOTO 60
            ENDDO
            GOTO 100
 60         CONTINUE
          ENDIF
        ELSE
          JJ=NFT+I
          N =IX(NIX,JJ)
          II=PTSH(JJ)
          IF(II == 0)GOTO 100
        END IF
        IF(SIGSH(NVSHELL - 1 ,II) == ZERO ) CYCLE
c
        NPTI=NINT(SIGSH(2,II))
        IF(SIGSH(3,II) /= ZERO) THEN
          THK(I)=SIGSH(3,II)
          THKE(I)=THK(I)
        ENDIF
        EINT(I,1)=SIGSH(4,II)
        EINT(I,2)=SIGSH(5,II)
        IF (G_HOURG > 0) THEN
          HH(I,1)= SIGSH(14,II)
          HH(I,2)= SIGSH(15,II)
          HH(I,3)= SIGSH(16,II)
        ENDIF
  
        IF(NPT /= NPTI ) THEN
                IPID=IX(NIX-1,NFT+I)
                PID=IGEO(1,IPID)            
                CALL FRETITL2(TITR1,IGEO(NPROPGI-LTITR+1,IPID),LTITR)
                IF(NPTI == 0 .AND. ITHKSHEL /= 2)THEN
                  CALL ANCMSG(MSGID=2020,
     .                      ANMODE=ANINFO,
     .                      MSGTYPE=MSGERROR,
     .                      C1=TITR1,
     .                      I1=PID,
     .                      I2=N,
     .                      PRMOD=MSG_CUMU)
                ELSEIF(ISIGSH /= 0) THEN
                  CALL ANCMSG(MSGID=26,
     .                        ANMODE=ANINFO,
     .                        MSGTYPE=MSGERROR,
     .                        I2=N,
     .                        I1=PID,
     .                        C1=TITR1)
                ENDIF
         ENDIF   
        IF (ISTRAIN /= 0.AND.ITHKSHEL==2) THEN
          IF(SIGSH(17,II) == ONE)THEN
C--- in global sys
            PT = INISHVAR1          
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
        IF (NPT == 0) THEN
           IF(SIGSH(17,II) == ONE)THEN
             TXX = SIGSH(22,II)
             TYY = SIGSH(23,II)
             TZZ = SIGSH(18,II)
             TXY = SIGSH(24,II)
             TYZ = SIGSH(25,II)
             TZX = SIGSH(26,II)
C
              A = E1X(I)*TXX + E1Y(I)*TXY + E1Z(I)*TZX   
              B = E1X(I)*TXY + E1Y(I)*TYY + E1Z(I)*TYZ   
              C = E1X(I)*TZX + E1Y(I)*TYZ + E1Z(I)*TZZ   
              UXX = A*E1X(I) + B*E1Y(I) + C*E1Z(I)   
              UXY = A*E2X(I) + B*E2Y(I) + C*E2Z(I)   
              UZX = A*E3X(I) + B*E3Y(I) + C*E3Z(I)   
              A = E2X(I)*TXX + E2Y(I)*TXY + E2Z(I)*TZX   
              B = E2X(I)*TXY + E2Y(I)*TYY + E2Z(I)*TYZ   
              C = E2X(I)*TZX + E2Y(I)*TYZ + E2Z(I)*TZZ   
              UYY = A*E2X(I) + B*E2Y(I) + C*E2Z(I)   
              UYZ = A*E3X(I) + B*E3Y(I) + C*E3Z(I)   
C
              SIGSH(22,II) = UXX
              SIGSH(23,II) = UYY
              SIGSH(24,II) = UXY
              SIGSH(25,II) = UYZ
              SIGSH(26,II) = UZX
C
              TXX = SIGSH(28,II)
              TYY = SIGSH(29,II)
              TZZ = SIGSH(19,II)
              TXY = SIGSH(30,II)
              TYZ = SIGSH(20,II)
              TZX = SIGSH(21,II)
C
              A = E1X(I)*TXX + E1Y(I)*TXY + E1Z(I)*TZX   
              B = E1X(I)*TXY + E1Y(I)*TYY + E1Z(I)*TYZ   
              C = E1X(I)*TZX + E1Y(I)*TYZ + E1Z(I)*TZZ   
              UXX = A*E1X(I) + B*E1Y(I) + C*E1Z(I)   
              UXY = A*E2X(I) + B*E2Y(I) + C*E2Z(I)   
              A = E2X(I)*TXX + E2Y(I)*TXY + E2Z(I)*TZX   
              B = E2X(I)*TXY + E2Y(I)*TYY + E2Z(I)*TYZ   
              C = E2X(I)*TZX + E2Y(I)*TYZ + E2Z(I)*TZZ   
              UYY = A*E2X(I) + B*E2Y(I) + C*E2Z(I)   
C
              SIGSH(28,II) = UXX
              SIGSH(29,II) = UYY
              SIGSH(30,II) = UXY
          ENDIF
          FOR(I,1)=SIGSH(22,II)
          FOR(I,2)=SIGSH(23,II)
          FOR(I,3)=SIGSH(24,II)
          FOR(I,4)=SIGSH(25,II)
          FOR(I,5)=SIGSH(26,II)
          IF (L_PLA > 0) PLAS(I) =SIGSH(27,II)
          MOM(I,1)=SIGSH(28,II)
          MOM(I,2)=SIGSH(29,II)
          MOM(I,3)=SIGSH(30,II)
        ELSEIF(NPTI == 0)THEN
          IF(SIGSH(17,II) == ONE)THEN
              TXX = SIGSH(22,II)
              TYY = SIGSH(23,II)
              TZZ = SIGSH(18,II)
              TXY = SIGSH(24,II)
              TYZ = SIGSH(25,II)
              TZX = SIGSH(26,II)
C
              A = E1X(I)*TXX + E1Y(I)*TXY + E1Z(I)*TZX   
              B = E1X(I)*TXY + E1Y(I)*TYY + E1Z(I)*TYZ   
              C = E1X(I)*TZX + E1Y(I)*TYZ + E1Z(I)*TZZ   
              UXX = A*E1X(I) + B*E1Y(I) + C*E1Z(I)   
              UXY = A*E2X(I) + B*E2Y(I) + C*E2Z(I)   
              UZX = A*E3X(I) + B*E3Y(I) + C*E3Z(I)   
              A = E2X(I)*TXX + E2Y(I)*TXY + E2Z(I)*TZX   
              B = E2X(I)*TXY + E2Y(I)*TYY + E2Z(I)*TYZ   
              C = E2X(I)*TZX + E2Y(I)*TYZ + E2Z(I)*TZZ   
              UYY = A*E2X(I) + B*E2Y(I) + C*E2Z(I)   
              UYZ = A*E3X(I) + B*E3Y(I) + C*E3Z(I)   
C
              SIGSH(22,II) = UXX
              SIGSH(23,II) = UYY
              SIGSH(24,II) = UXY
              SIGSH(25,II) = UYZ
              SIGSH(26,II) = UZX
          ENDIF
          DO J=1,NPT          
            IF (NLAY > 1) THEN
              LBUF => ELBUF_STR%BUFLY(J)%LBUF(1,1,1)
              L_PLA = ELBUF_STR%BUFLY(J)%L_PLA
            ELSE
              LBUF  => ELBUF_STR%BUFLY(1)%LBUF(1,1,J)
              L_PLA =  ELBUF_STR%BUFLY(1)%L_PLA
            ENDIF
            LBUF%SIG(KK(1)+I)=SIGSH(22,II)
            LBUF%SIG(KK(2)+I)=SIGSH(23,II)
            LBUF%SIG(KK(3)+I)=SIGSH(24,II)
            LBUF%SIG(KK(4)+I)=SIGSH(25,II)
            LBUF%SIG(KK(5)+I)=SIGSH(26,II)
            IF (L_PLA > 0) LBUF%PLA(I) = SIGSH(27,II)
          ENDDO
        ELSEIF(NPT /= 0)THEN
          IF(SIGSH(17,II) == ONE)THEN
             PT = 22 
             IPT_ALL = 0 
             DO ILAY=1,NLAY
                NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
                ILAW = ELBUF_STR%BUFLY(ILAY)%ILAW
                JDIR = 1 + (ILAY-1)*NEL*2
                JJ = JDIR + I-1
                DO IT=1,NPTT
                  IPT =IPT_ALL+IT
                  S6(1:2)=SIGSH(PT:PT+1,II)
                  PTN = INISHVAR+IPT             
                  S6(3)=SIGSH(PTN,II)
                  S6(4:6)=SIGSH(PT+2:PT+4,II)
                  CALL CG2LSIG(
     7            E1X    ,E2X   ,E3X   ,E1Y   ,E2Y  ,E3Y,
     8            E1Z    ,E2Z   ,E3Z   ,S6    )
                  CALL LOC2ORTH(S6,DIR_A,DIR_B,JJ,ILAW,IGTYP,NEL)
                  SIGSH(PT:PT+4,II) = S6(1:5)
                  POSI(I,IPT)=SIGSH(PTN+NPT,II)
                  PT = PT + 6
                ENDDO
                IPT_ALL = IPT_ALL +  NPTT         
             ENDDO
          END IF !IF(SIGSH(17,II)
          IPT_ALL = 0 
          PT = 22 
          DO ILAY=1,NLAY
            NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
            L_PLA = ELBUF_STR%BUFLY(ILAY)%L_PLA
            DO IT=1,NPTT
              LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(1,1,IT)
              LBUF%SIG(KK(1)+I)=SIGSH(PT,II)	     
              LBUF%SIG(KK(2)+I)=SIGSH(PT+1,II)	     
              LBUF%SIG(KK(3)+I)=SIGSH(PT+2,II)	     
              LBUF%SIG(KK(4)+I)=SIGSH(PT+3,II)	     
              LBUF%SIG(KK(5)+I)=SIGSH(PT+4,II)	     
              IF (L_PLA > 0) LBUF%PLA(I) = SIGSH(PT+5,II)        
              PT = PT + 6
            END DO !IT=1,NPTT
          ENDDO
        ENDIF
 100    CONTINUE
      ENDDO
      CALL ANCMSG(MSGID=2020,                 
     .            ANMODE=ANINFO_BLIND_2,
     .            MSGTYPE=MSGERROR,
     .            PRMOD=MSG_PRINT)       
C-----------
      RETURN
      END

