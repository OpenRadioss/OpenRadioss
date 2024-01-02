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
Chd|  CHK_DTTSH                     source/elements/thickshell/solidec/scdtchk3.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE CHK_DTTSH(ELBUF_STR,IXS ,IPARG ,IKINE)
C=======================================================================
      USE MESSAGE_MOD
      USE ELBUFDEF_MOD     
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,DIMENSION(NIXS,NUMELS),INTENT(IN) :: IXS
      INTEGER,DIMENSION(NPARG,NGROUP),INTENT(IN) :: IPARG
      INTEGER,DIMENSION(NUMNOD*3),INTENT(IN) :: IKINE
      TYPE(ELBUF_STRUCT_),DIMENSION(NGROUP), TARGET :: ELBUF_STR
C     REAL
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NG, NEL, NFT,N, ITY,JHBE,IGTYP,ICSTR,ISOLNOD
      INTEGER ITAG(NUMNOD),N1,N2,NC(MVSIZ,8),NEDG,IER1,IER2,IKIN
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
C     REAL
C-------
      ITAG(1:NUMNOD)=0
      NEDG = 0
      DO NG=1,NGROUP
       NEL=IPARG(2,NG)
       NFT=IPARG(3,NG)
       ITY=IPARG(5,NG)
       ICSTR= IPARG(17,NG)
       JHBE = IPARG(23,NG)
       IGTYP  = IPARG(38,NG)
       ISOLNOD= IPARG(28,NG)
       IF (IPARG(8,NG)==1) CYCLE                                 
       IF (ITY /= 1) CYCLE
       IF (IGTYP == 20.OR.IGTYP == 21.OR.IGTYP == 22)THEN
C--------only linear for the moment       
        IF(ISOLNOD==6)THEN
          DO I=1,NEL
                  N = NFT+I
            NC(I,1:3)=IXS(2:4,N)
            NC(I,4:6)=IXS(6:8,N)
          ENDDO
            DO I=1,NEL
                    N1 = NC(I,1)
                    N2 = NC(I,4)
                    IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
                      NEDG = NEDG + 1
                      ITAG(N1)=NEDG
                      ITAG(N2)=NEDG
              END IF
                    N1 = NC(I,2)
                    N2 = NC(I,5)
                    IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
                      NEDG = NEDG + 1
                      ITAG(N1)=NEDG
                      ITAG(N2)=NEDG
              END IF
                    N1 = NC(I,3)
                    N2 = NC(I,6)
                    IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
                      NEDG = NEDG + 1
                      ITAG(N1)=NEDG
                      ITAG(N2)=NEDG
              END IF
            ENDDO
        ELSEIF(ISOLNOD==8)THEN
          DO I=1,NEL
                  N = NFT+I
            NC(I,1:8)=IXS(2:9,N)
          ENDDO
          IF (JHBE==14) THEN
             SELECT CASE (ICSTR)
              CASE(100)                                             
                DO I=1,NEL
                        N1 = NC(I,1)
                        N2 = NC(I,4)
                        IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
                          NEDG = NEDG + 1
                          ITAG(N1)=NEDG
                          ITAG(N2)=NEDG
                  END IF
                        N1 = NC(I,2)
                        N2 = NC(I,3)
                        IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
                          NEDG = NEDG + 1
                          ITAG(N1)=NEDG
                          ITAG(N2)=NEDG
                  END IF
                        N1 = NC(I,5)
                        N2 = NC(I,8)
                        IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
                          NEDG = NEDG + 1
                          ITAG(N1)=NEDG
                          ITAG(N2)=NEDG
                  END IF
                        N1 = NC(I,6)
                        N2 = NC(I,7)
                        IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
                          NEDG = NEDG + 1
                          ITAG(N1)=NEDG
                          ITAG(N2)=NEDG
                  END IF
                ENDDO
              CASE(10)                                          
                DO I=1,NEL
                        N1 = NC(I,1)
                        N2 = NC(I,5)
                        IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
                          NEDG = NEDG + 1
                          ITAG(N1)=NEDG
                          ITAG(N2)=NEDG
                  END IF
                        N1 = NC(I,2)
                        N2 = NC(I,6)
                        IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
                          NEDG = NEDG + 1
                          ITAG(N1)=NEDG
                          ITAG(N2)=NEDG
                  END IF
                        N1 = NC(I,3)
                        N2 = NC(I,7)
                        IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
                          NEDG = NEDG + 1
                          ITAG(N1)=NEDG
                          ITAG(N2)=NEDG
                  END IF
                        N1 = NC(I,4)
                        N2 = NC(I,8)
                        IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
                          NEDG = NEDG + 1
                          ITAG(N1)=NEDG
                          ITAG(N2)=NEDG
                  END IF
                ENDDO
              CASE(1)                                          
                DO I=1,NEL
                        N1 = NC(I,1)
                        N2 = NC(I,2)
                        IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
                          NEDG = NEDG + 1
                          ITAG(N1)=NEDG
                          ITAG(N2)=NEDG
                  END IF
                        N1 = NC(I,4)
                        N2 = NC(I,3)
                        IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
                          NEDG = NEDG + 1
                          ITAG(N1)=NEDG
                          ITAG(N2)=NEDG
                  END IF
                        N1 = NC(I,5)
                        N2 = NC(I,6)
                        IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
                          NEDG = NEDG + 1
                          ITAG(N1)=NEDG
                          ITAG(N2)=NEDG
                  END IF
                        N1 = NC(I,8)
                        N2 = NC(I,7)
                        IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
                          NEDG = NEDG + 1
                          ITAG(N1)=NEDG
                          ITAG(N2)=NEDG
                  END IF
                ENDDO
             END SELECT 
          ELSEIF (JHBE==15) THEN
            DO I=1,NEL
                    N1 = NC(I,1)
                    N2 = NC(I,5)
                    IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
                      NEDG = NEDG + 1
                      ITAG(N1)=NEDG
                      ITAG(N2)=NEDG
              END IF
                    N1 = NC(I,2)
                    N2 = NC(I,6)
                    IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
                      NEDG = NEDG + 1
                      ITAG(N1)=NEDG
                      ITAG(N2)=NEDG
              END IF
                    N1 = NC(I,3)
                    N2 = NC(I,7)
                    IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
                      NEDG = NEDG + 1
                      ITAG(N1)=NEDG
                      ITAG(N2)=NEDG
              END IF
                    N1 = NC(I,4)
                    N2 = NC(I,8)
                    IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
                      NEDG = NEDG + 1
                      ITAG(N1)=NEDG
                      ITAG(N2)=NEDG
              END IF
            ENDDO
          END IF
        END IF
       END IF
      ENDDO 
C-------uncompatibility commun node dif edge
       IER1=0  !mesh
       IER2=0  !kin
      DO NG=1,NGROUP
       NEL=IPARG(2,NG)
       NFT=IPARG(3,NG)
       ITY=IPARG(5,NG)
       ICSTR= IPARG(17,NG)
       JHBE = IPARG(23,NG)
       IGTYP  = IPARG(38,NG)
       ISOLNOD= IPARG(28,NG)
       GBUF => ELBUF_STR(NG)%GBUF
       IF (IPARG(8,NG)==1) CYCLE                                 
       IF (ITY /= 1) CYCLE
       IF (IGTYP == 20.OR.IGTYP == 21.OR.IGTYP == 22)THEN
C--------only linear for the moment       
        IF(ISOLNOD==6)THEN
          DO I=1,NEL
                  N = NFT+I
            NC(I,1:3)=IXS(2:4,N)
            NC(I,4:6)=IXS(6:8,N)
          ENDDO
            DO I=1,NEL
              IKIN = 0
                    N1 = NC(I,1)
                    N2 = NC(I,4)
                    IF (ITAG(N1)/=ITAG(N2)) GBUF%IDT_TSH(I)=0
                    IF (IKINE(N1)/=IKINE(N2)) IKIN = 1
                    N1 = NC(I,2)
                    N2 = NC(I,5)
                    IF (ITAG(N1)/=ITAG(N2)) GBUF%IDT_TSH(I)=0
                    IF (IKINE(N1)/=IKINE(N2)) IKIN = 1
                    N1 = NC(I,3)
                    N2 = NC(I,6)
                    IF (ITAG(N1)/=ITAG(N2)) GBUF%IDT_TSH(I)=0
                    IF (IKINE(N1)/=IKINE(N2)) IKIN = 1
                    IF (IKIN==1) GBUF%IDT_TSH(I)=-1
            ENDDO
        ELSEIF(ISOLNOD==8)THEN
          DO I=1,NEL
                  N = NFT+I
            NC(I,1:8)=IXS(2:9,N)
          ENDDO
          IF (JHBE==14) THEN
             SELECT CASE (ICSTR)
              CASE(100)                                             
                DO I=1,NEL
                  IKIN = 0
                        N1 = NC(I,1)
                        N2 = NC(I,4)
                        IF (ITAG(N1)/=ITAG(N2)) GBUF%IDT_TSH(I)=0
                        IF (IKINE(N1)/=IKINE(N2)) IKIN = 1
                        N1 = NC(I,2)
                        N2 = NC(I,3)
                        IF (ITAG(N1)/=ITAG(N2)) GBUF%IDT_TSH(I)=0
                        IF (IKINE(N1)/=IKINE(N2)) IKIN = 1
                        N1 = NC(I,5)
                        N2 = NC(I,8)
                        IF (ITAG(N1)/=ITAG(N2)) GBUF%IDT_TSH(I)=0
                        IF (IKINE(N1)/=IKINE(N2)) IKIN = 1
                        N1 = NC(I,6)
                        N2 = NC(I,7)
                        IF (ITAG(N1)/=ITAG(N2)) GBUF%IDT_TSH(I)=0
                        IF (IKIN==1) GBUF%IDT_TSH(I)=-1
                ENDDO
              CASE(10)                                          
                DO I=1,NEL
                  IKIN = 0
                        N1 = NC(I,1)
                        N2 = NC(I,5)
                        IF (ITAG(N1)/=ITAG(N2)) GBUF%IDT_TSH(I)=0
                        IF (IKINE(N1)/=IKINE(N2)) IKIN = 1
                        N1 = NC(I,2)
                        N2 = NC(I,6)
                        IF (ITAG(N1)/=ITAG(N2)) GBUF%IDT_TSH(I)=0
                        IF (IKINE(N1)/=IKINE(N2)) IKIN = 1
                        N1 = NC(I,3)
                        N2 = NC(I,7)
                        IF (ITAG(N1)/=ITAG(N2)) GBUF%IDT_TSH(I)=0
                        IF (IKINE(N1)/=IKINE(N2)) IKIN = 1
                        N1 = NC(I,4)
                        N2 = NC(I,8)
                        IF (ITAG(N1)/=ITAG(N2)) GBUF%IDT_TSH(I)=0
                        IF (IKINE(N1)/=IKINE(N2)) IKIN = 1
                        IF (IKIN==1) GBUF%IDT_TSH(I)=-1
                ENDDO
              CASE(1)                                          
                DO I=1,NEL
                  IKIN = 0
                        N1 = NC(I,1)
                        N2 = NC(I,2)
                        IF (ITAG(N1)/=ITAG(N2)) GBUF%IDT_TSH(I)=0
                        IF (IKINE(N1)/=IKINE(N2)) IKIN = 1
                        N1 = NC(I,4)
                        N2 = NC(I,3)
                        IF (IKINE(N1)/=IKINE(N2)) IKIN = 1
                        IF (ITAG(N1)/=ITAG(N2)) GBUF%IDT_TSH(I)=0
                        N1 = NC(I,5)
                        N2 = NC(I,6)
                        IF (IKINE(N1)/=IKINE(N2)) IKIN = 1
                        IF (ITAG(N1)/=ITAG(N2)) GBUF%IDT_TSH(I)=0
                        N1 = NC(I,8)
                        N2 = NC(I,7)
                        IF (IKINE(N1)/=IKINE(N2)) IKIN = 1
                        IF (ITAG(N1)/=ITAG(N2)) GBUF%IDT_TSH(I)=0
                        IF (IKIN==1) GBUF%IDT_TSH(I)=-1
                ENDDO
             END SELECT 
          ELSEIF (JHBE==15) THEN
            DO I=1,NEL
              IKIN = 0
                    N1 = NC(I,1)
                    N2 = NC(I,5)
                    IF (ITAG(N1)/=ITAG(N2)) GBUF%IDT_TSH(I)=0
                    IF (IKINE(N1)/=IKINE(N2)) IKIN = 1
                    N1 = NC(I,2)
                    N2 = NC(I,6)
                    IF (ITAG(N1)/=ITAG(N2)) GBUF%IDT_TSH(I)=0
                    IF (IKINE(N1)/=IKINE(N2)) IKIN = 1
                    N1 = NC(I,3)
                    N2 = NC(I,7)
                    IF (ITAG(N1)/=ITAG(N2)) GBUF%IDT_TSH(I)=0
                    IF (IKINE(N1)/=IKINE(N2)) IKIN = 1
                    N1 = NC(I,4)
                    N2 = NC(I,8)
                    IF (ITAG(N1)/=ITAG(N2)) GBUF%IDT_TSH(I)=0
                    IF (IKINE(N1)/=IKINE(N2)) IKIN = 1
                    IF (IKIN==1) GBUF%IDT_TSH(I)=-1
            ENDDO
          END IF
        END IF
         IF (IDTTSH>0 .AND.(ISOLNOD==6.OR.ISOLNOD==8)) THEN
            DO I=1,NEL
                     N = NFT+I
                      IF (GBUF%IDT_TSH(I)==0) THEN
                  IF (IPRI>0 ) CALL ANCMSG(MSGID=2070,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=IXS(11,N),
     .                PRMOD=MSG_CUMU)
                  IER1=IER1+1
                END IF
                      IF (GBUF%IDT_TSH(I)==-1) THEN
                  IF (IPRI>0 ) CALL ANCMSG(MSGID=2071,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=IXS(11,N),
     .                PRMOD=MSG_CUMU)
                  IER2=IER2+1
                END IF
            ENDDO
         END IF
       END IF
      ENDDO 
C-------message out      
      IF (IDTTSH>0 .AND.(IER1+IER2)>0) THEN
        IF (IER1>0.AND. IPRI>0 ) CALL ANCMSG(MSGID=2070,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO_BLIND_1,
     .                PRMOD=MSG_PRINT)
        IF (IER2>0.AND. IPRI>0 ) CALL ANCMSG(MSGID=2071,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO_BLIND_1,
     .                PRMOD=MSG_PRINT)
        IF (IPRI==0 ) CALL ANCMSG(MSGID=2069,
     .                MSGTYPE=MSGINFO,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=IER1,
     .                I2=IER2)
       END IF
C       
      RETURN
      END SUBROUTINE CHK_DTTSH
