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
Chd|  FATHER                        source/model/remesh/build_admesh.F
Chd|-- called by -----------
Chd|        ORIGIN                        source/model/remesh/build_admesh.F
Chd|-- calls ---------------
Chd|====================================================================
      INTEGER FUNCTION FATHER(NN,IXC,IPARTC,IPART,SONTYPE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NN,IXC(NIXC,*),IPARTC(*),IPART(LIPART1,*)
        INTEGER SONTYPE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------

        IF(NN<=NUMELC0)THEN
          FATHER=NN
          SONTYPE=0
        ELSE
          FATHER =NUMELC0+1+5*INT(((NN-NUMELC0)-1)/5)
          SONTYPE=NN-FATHER
        END IF
        RETURN
      END
Chd|====================================================================
Chd|  ORIGIN                        source/model/remesh/build_admesh.F
Chd|-- called by -----------
Chd|        I11REMLINE                    source/interfaces/inter3d1/i11remlin.F
Chd|        I25REMLINE                    source/interfaces/int25/i25remlin.F
Chd|-- calls ---------------
Chd|        FATHER                        source/model/remesh/build_admesh.F
Chd|====================================================================
      INTEGER FUNCTION ORIGIN(NN,IXC,IPARTC,IPART)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NN,IXC(NIXC,*),IPARTC(*),IPART(LIPART1,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER SONTYPE, LEVEL
C-----------------------------------------------
C   E x t e r n a l
C-----------------------------------------------
        INTEGER FATHER
        EXTERNAL FATHER
C-----------------------------------------------
        LEVEL=0
        DO WHILE(NN>NUMELC0)
          NN=FATHER(NN,IXC,IPARTC,IPART,SONTYPE)
          LEVEL=LEVEL+1
        END DO

        ORIGIN=NN
        RETURN
      END
      RECURSIVE SUBROUTINE IDENTSON4(LEVEL,NN,IXC,SH4TREE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER LEVEL,NN,IXC(NIXC,*),SH4TREE(KSH4TREE,*)
        INTEGER, SAVE :: ID
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER PP
        IF(LEVEL==0)THEN
          ID=ID_LIMIT_ADMESH+(NN-1)*((4**(LEVELMAX+1)-1)/3-1)
        END IF
        PP=SH4TREE(2,NN)
        IF(PP/=0) THEN

          ID=ID+1
          IXC(NIXC,PP)=ID
          ID=ID+1
          IXC(NIXC,PP+1)=ID
          ID=ID+1
          IXC(NIXC,PP+2)=ID
          ID=ID+1
          IXC(NIXC,PP+3)=ID

          CALL IDENTSON4(LEVEL+1,PP,IXC,SH4TREE)
          PP=PP+1
          CALL IDENTSON4(LEVEL+1,PP,IXC,SH4TREE)
          PP=PP+1
          CALL IDENTSON4(LEVEL+1,PP,IXC,SH4TREE)
          PP=PP+1
          CALL IDENTSON4(LEVEL+1,PP,IXC,SH4TREE)
        END IF
        RETURN
      END
      RECURSIVE SUBROUTINE IDENTSON3(LEVEL,NN,IXTG,SH3TREE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER LEVEL,NN,IXTG(NIXTG,*),SH3TREE(KSH3TREE,*)
        INTEGER, SAVE :: ID
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER PP
        IF(LEVEL==0)THEN
          ID=ID_LIMIT_ADMESH+(NN-1)*((4**(LEVELMAX+1)-1)/3-1)
        END IF
        PP=SH3TREE(2,NN)
        IF(PP/=0) THEN

          ID=ID+1
          IXTG(NIXTG,PP)=ID
          ID=ID+1
          IXTG(NIXTG,PP+1)=ID
          ID=ID+1
          IXTG(NIXTG,PP+2)=ID
          ID=ID+1
          IXTG(NIXTG,PP+3)=ID

          CALL IDENTSON3(LEVEL+1,PP,IXTG,SH3TREE)
          PP=PP+1
          CALL IDENTSON3(LEVEL+1,PP,IXTG,SH3TREE)
          PP=PP+1
          CALL IDENTSON3(LEVEL+1,PP,IXTG,SH3TREE)
          PP=PP+1
          CALL IDENTSON3(LEVEL+1,PP,IXTG,SH3TREE)
        END IF

        RETURN
      END
Chd|====================================================================
Chd|  BUILD_ADMESH                  source/model/remesh/build_admesh.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CONSTIT                       source/elements/nodes/constit.F
Chd|        FREERR                        source/starter/freform.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE BUILD_ADMESH(
     .                    IPART ,IPARTC,IPARTTG,IXC   ,IXTG  ,
     .                    X     ,ITAB  ,ITABM1 ,SH4TREE,SH3TREE,
     .                    IPADMESH,PADMESH)
        USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "remesh_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IPART(LIPART1,*), IPARTC(*), IPARTTG(*),
     .          IXC(NIXC,*), IXTG(NIXTG,*),ITAB(*),ITABM1(*),
     .          SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*),
     .          IPADMESH(KIPADMESH,*)
C     REAL
        my_real
     .         X(3,*), PADMESH(KPADMESH,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER ID,NP,J10(10),
     .    N,IP,I,J,NLEV,NI,NJ,NK,NL,NN,
     .    K,L,P,Q,QQ,STAT,
     .    LEVEL,NUMELC_LEV,NUMELTG_LEV,
     .    NUMELC_OLD,NUMELTG_OLD,
     .    NUMELC_OLD_OLD,NUMELTG_OLD_OLD,
     .    NUMELC_NEW,NUMELTG_NEW,NUMNOD_NEW,
     .    INILEV
        INTEGER, DIMENSION(:),ALLOCATABLE ::
     .    KNOD2SH, NOD2SH
        INTEGER, DIMENSION(:,:),ALLOCATABLE :: TAG
        my_real
     .    ANGL,XA,XB
C     REAL
        CHARACTER MESS*40,TITR*nchartitle,KEY*ncharkey
C-----------------------------------------------
        DATA MESS /'OPTIONS FOR ADAPTIVE MESHING DEFINITION '/
C-----------------------------------------------
        IF(IADMSTAT /= 0)RETURN
C------

        NUMNOD_NEW =NUMNOD0
        NUMELC_OLD =0
        NUMELC_NEW =NUMELC0
        NUMELTG_OLD=0
        NUMELTG_NEW=NUMELTG0

        DO 100 LEVEL=1,LEVELMAX
          NUMELC_OLD_OLD=NUMELC_OLD
          NUMELC_OLD    =NUMELC_NEW
          NUMELTG_OLD_OLD=NUMELTG_OLD
          NUMELTG_OLD    =NUMELTG_NEW

          NUMELC_LEV =NUMELC_OLD-NUMELC_OLD_OLD
          NUMELTG_LEV=NUMELTG_OLD-NUMELTG_OLD_OLD
C
C     connectivite inverse au niveau precedent
C
          ALLOCATE(KNOD2SH(0:NUMNOD_NEW),STAT=stat)
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                              MSGTYPE=MSGERROR,
     .                             C1='KNOD2SH')
C
          KNOD2SH=0
          DO N=NUMELC_OLD_OLD+1,NUMELC_OLD
            DO I=1,4
              NI=IXC(I+1,N)
              KNOD2SH(NI)=KNOD2SH(NI)+1
            END DO
          END DO
C
          DO N=NUMELTG_OLD_OLD+1,NUMELTG_OLD
            DO I=1,3
              NI=IXTG(I+1,N)
              KNOD2SH(NI)=KNOD2SH(NI)+1
            END DO
          END DO
C
          DO N=2,NUMNOD_NEW
            KNOD2SH(N)=KNOD2SH(N)+KNOD2SH(N-1)
          END DO
C
          ALLOCATE(NOD2SH(4*NUMELC_LEV+3*NUMELTG_LEV),STAT=stat)
          IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                               MSGTYPE=MSGERROR,
     .                             C1='NOD2SH')
C
          DO N=1,NUMELC_LEV
            NN=NUMELC_OLD_OLD+N
            DO I=1,4
              NI=IXC(I+1,NN)-1
              KNOD2SH(NI)=KNOD2SH(NI)+1
              NOD2SH(KNOD2SH(NI))=N
            END DO
          END DO
C
          DO N=1,NUMELTG_LEV
            NN=NUMELTG_OLD_OLD+N
            DO I=1,3
              NI=IXTG(I+1,NN)-1
              KNOD2SH(NI)=KNOD2SH(NI)+1
              NOD2SH(KNOD2SH(NI))=NUMELC_LEV+N
            END DO
          END DO
C
          DO N=NUMNOD_NEW,1,-1
            KNOD2SH(N)=KNOD2SH(N-1)
          END DO
          KNOD2SH(0)=0
C
C     allocation tag
          ALLOCATE(TAG(5,NUMELC_LEV+NUMELTG_LEV),STAT=stat)
          IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                               MSGTYPE=MSGERROR,
     .                             C1='TAG')
          TAG=0
C
          DO N=1,NUMELC_LEV
            NN=NUMELC_OLD_OLD+N
            IP  =IPARTC(NN)
            NLEV=IPART(10,IP)
            IF(NLEV<LEVEL) CYCLE
            DO I=1,4
              IF(TAG(I,N)==0)THEN
                NI=IXC(I+1,NN)
                NJ=IXC(MOD(I,4)+2,NN)

                NUMNOD_NEW=NUMNOD_NEW+1
                TAG(I,N)=NUMNOD_NEW
                DO J=1,3
                  X(J,NUMNOD_NEW)=HALF*(X(J,NI)+X(J,NJ))
                END DO
                DO K=KNOD2SH(NI-1)+1,KNOD2SH(NI)
                  P=NOD2SH(K)
                  IF(P/=N)THEN
                    DO L=KNOD2SH(NJ-1)+1,KNOD2SH(NJ)
                      Q=NOD2SH(L)
                      IF(Q==P)THEN
                        IF(Q<=NUMELC_LEV)THEN
                          QQ=NUMELC_OLD_OLD+Q
                          DO J=1,4
                            NK=IXC(J+1,QQ)
                            NL=IXC(MOD(J,4)+2,QQ)

                            IF((NK==NI.AND.NL==NJ).OR.
     .                         (NL==NI.AND.NK==NJ))THEN
                              TAG(J,Q)=NUMNOD_NEW
                            END IF
                          END DO
                        ELSE
                          QQ=NUMELTG_OLD_OLD+Q-NUMELC_LEV
                          DO J=1,3
                            NK=IXTG(J+1,QQ)
                            NL=IXTG(MOD(J,3)+2,QQ)

                            IF((NK==NI.AND.NL==NJ).OR.
     .                         (NL==NI.AND.NK==NJ))THEN
                              TAG(J,Q)=NUMNOD_NEW
                            END IF
                          END DO
                        END IF
                      END IF
                    END DO
                  END IF
                END DO
              END IF
            END DO
          END DO
C
          DO N=1,NUMELC_LEV
            NN=NUMELC_OLD_OLD+N
            IP  =IPARTC(NN)
            NLEV=IPART(10,IP)
            IF(NLEV<LEVEL) CYCLE
            NUMNOD_NEW=NUMNOD_NEW+1
            TAG(5,N)=NUMNOD_NEW
            NI=TAG(1,N)
            NJ=TAG(3,N)
            NK=TAG(2,N)
            NL=TAG(4,N)
            DO J=1,3
              XA=HALF*(X(J,NI)+X(J,NJ))
              XB=HALF*(X(J,NK)+X(J,NL))
              X(J,NUMNOD_NEW)=HALF*(XA+XB)
            END DO
          END DO
C
          DO N=1,NUMELTG_LEV
            NN=NUMELTG_OLD_OLD+N
            IP  =IPARTTG(NN)
            NLEV=IPART(10,IP)
            IF(NLEV<LEVEL) CYCLE
            DO I=1,3
              IF(TAG(I,N+NUMELC_LEV)==0)THEN
                NI=IXTG(I+1,NN)
                NJ=IXTG(MOD(I,3)+2,NN)

                NUMNOD_NEW=NUMNOD_NEW+1
                TAG(I,N+NUMELC_LEV)=NUMNOD_NEW
                DO J=1,3
                  X(J,NUMNOD_NEW)=HALF*(X(J,NI)+X(J,NJ))
                END DO
                DO K=KNOD2SH(NI-1)+1,KNOD2SH(NI)
                  P=NOD2SH(K)
                  IF(P/=N+NUMELC)THEN
                    DO L=KNOD2SH(NJ-1)+1,KNOD2SH(NJ)
                      Q=NOD2SH(L)
                      IF(Q==P)THEN
                        IF(Q<=NUMELC_LEV)THEN
                          QQ=NUMELC_OLD_OLD+Q
                          DO J=1,4
                            NK=IXC(J+1,QQ)
                            NL=IXC(MOD(J,4)+2,QQ)

                            IF((NK==NI.AND.NL==NJ).OR.
     .                         (NL==NI.AND.NK==NJ))THEN
                              TAG(J,Q)=NUMNOD_NEW
                            END IF
                          END DO
                        ELSE
                          QQ=NUMELTG_OLD_OLD+Q-NUMELC_LEV
                          DO J=1,3
                            NK=IXTG(J+1,QQ)
                            NL=IXTG(MOD(J,3)+2,QQ)

                            IF((NK==NI.AND.NL==NJ).OR.
     .                         (NL==NI.AND.NK==NJ))THEN
                              TAG(J,Q)=NUMNOD_NEW
                            END IF
                          END DO
                        END IF
                      END IF
                    END DO
                  END IF
                END DO
              END IF
            END DO
          END DO
C
          NUMELC_NEW=NUMELC_OLD
          DO N=1,NUMELC_LEV
            NN=NUMELC_OLD_OLD+N
            IP  =IPARTC(NN)
            NLEV=IPART(10,IP)
            IF(NLEV<LEVEL) CYCLE

            DO J=1,NIXC
              DO I=1,4
                IXC(J,NUMELC_NEW+I)=IXC(J,NN)
              END DO
            END DO

            NUMELC_NEW=NUMELC_NEW+1
            IXC(2,NUMELC_NEW)=IXC(2,NN)
            IXC(3,NUMELC_NEW)=TAG(1,N)
            IXC(4,NUMELC_NEW)=TAG(5,N)
            IXC(5,NUMELC_NEW)=TAG(4,N)
            IPARTC(NUMELC_NEW)=IP

            SH4TREE(1,NUMELC_NEW)=NN
            SH4TREE(2,NN)=NUMELC_NEW
C
C       SH4TREE(3,NN)   = LEVEL SI ACTIVE
C                       = -(LEVEL+1) SINON
            SH4TREE(3,NUMELC_NEW)=-(LEVEL+1)

            NUMELC_NEW=NUMELC_NEW+1
            IXC(2,NUMELC_NEW)=TAG(1,N)
            IXC(3,NUMELC_NEW)=IXC(3,NN)
            IXC(4,NUMELC_NEW)=TAG(2,N)
            IXC(5,NUMELC_NEW)=TAG(5,N)
            IPARTC(NUMELC_NEW)=IP

            SH4TREE(1,NUMELC_NEW)=NN
            SH4TREE(3,NUMELC_NEW)=-(LEVEL+1)

            NUMELC_NEW=NUMELC_NEW+1
            IXC(2,NUMELC_NEW)=TAG(5,N)
            IXC(3,NUMELC_NEW)=TAG(2,N)
            IXC(4,NUMELC_NEW)=IXC(4,NN)
            IXC(5,NUMELC_NEW)=TAG(3,N)
            IPARTC(NUMELC_NEW)=IP

            SH4TREE(1,NUMELC_NEW)=NN
            SH4TREE(3,NUMELC_NEW)=-(LEVEL+1)

            NUMELC_NEW=NUMELC_NEW+1
            IXC(2,NUMELC_NEW)=TAG(4,N)
            IXC(3,NUMELC_NEW)=TAG(5,N)
            IXC(4,NUMELC_NEW)=TAG(3,N)
            IXC(5,NUMELC_NEW)=IXC(5,NN)
            IPARTC(NUMELC_NEW)=IP

            SH4TREE(1,NUMELC_NEW)=NN
            SH4TREE(3,NUMELC_NEW)=-(LEVEL+1)
          END DO
C
          NUMELTG_NEW=NUMELTG_OLD
          DO N=1,NUMELTG_LEV
            NN=NUMELTG_OLD_OLD+N
            IP  =IPARTTG(NN)
            NLEV=IPART(10,IP)
            IF(NLEV<LEVEL) CYCLE

            DO J=1,NIXTG
              DO I=1,4
                IXTG(J,NUMELTG_NEW+I)=IXTG(J,NN)
              END DO
            END DO

            NUMELTG_NEW=NUMELTG_NEW+1
            IXTG(2,NUMELTG_NEW)= IXTG(2,NN)
            IXTG(3,NUMELTG_NEW)= TAG(1,N+NUMELC_LEV)
            IXTG(4,NUMELTG_NEW)= TAG(3,N+NUMELC_LEV)
            IPARTTG(NUMELTG_NEW)=IP

            SH3TREE(1,NUMELTG_NEW)=NN
            SH3TREE(2,NN)=NUMELTG_NEW
            SH3TREE(3,NUMELTG_NEW)=-(LEVEL+1)

            NUMELTG_NEW=NUMELTG_NEW+1
            IXTG(2,NUMELTG_NEW)= TAG(1,N+NUMELC_LEV)
            IXTG(3,NUMELTG_NEW)= IXTG(3,NN)
            IXTG(4,NUMELTG_NEW)= TAG(2,N+NUMELC_LEV)
            IPARTTG(NUMELTG_NEW)=IP

            SH3TREE(1,NUMELTG_NEW)=NN
            SH3TREE(3,NUMELTG_NEW)=-(LEVEL+1)

            NUMELTG_NEW=NUMELTG_NEW+1
            IXTG(2,NUMELTG_NEW)= TAG(3,N+NUMELC_LEV)
            IXTG(3,NUMELTG_NEW)= TAG(2,N+NUMELC_LEV)
            IXTG(4,NUMELTG_NEW)= IXTG(4,NN)
            IPARTTG(NUMELTG_NEW)=IP

            SH3TREE(1,NUMELTG_NEW)=NN
            SH3TREE(3,NUMELTG_NEW)=-(LEVEL+1)

            NUMELTG_NEW=NUMELTG_NEW+1
            IXTG(2,NUMELTG_NEW)= TAG(2,N+NUMELC_LEV)
            IXTG(3,NUMELTG_NEW)= TAG(3,N+NUMELC_LEV)
            IXTG(4,NUMELTG_NEW)= TAG(1,N+NUMELC_LEV)
            IPARTTG(NUMELTG_NEW)=IP

            SH3TREE(1,NUMELTG_NEW)=NN
            SH3TREE(3,NUMELTG_NEW)=-(LEVEL+1)
          END DO

C     next level
          DEALLOCATE(TAG)
          DEALLOCATE(NOD2SH)
          DEALLOCATE(KNOD2SH)
 100    CONTINUE

C
C     identifiers of created shells and 3-node shells
        DO N=1,NUMELC0
          CALL IDENTSON4(0,N,IXC,SH4TREE)
        END DO

        DO N=1,NUMELTG0
          CALL IDENTSON3(0,N,IXTG,SH3TREE)
        END DO
C     inverse connectivity at level 0
C
C      ALLOCATE(KNOD2SH(0:NUMNOD0+1),STAT=stat)
C      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR)
C
C      KNOD2SH=0
C      DO N=1,NUMELC0
C        DO I=1,4
C          NI=IXC(I+1,N)
C          KNOD2SH(NI)=KNOD2SH(NI)+1
C        END DO
C      END DO
C
C      DO N=1,NUMELTG0
C        DO I=1,3
C          NI=IXTG(I+1,N)
C          KNOD2SH(NI)=KNOD2SH(NI)+1
C        END DO
C      END DO
C
C      DO N=2,NUMNOD0
C        KNOD2SH(N)=KNOD2SH(N)+KNOD2SH(N-1)
C      END DO
C
C      ALLOCATE(NOD2SH(4*NUMELC0+3*NUMELTG0),STAT=stat)
C      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR)
C
C      DO N=1,NUMELC0
C        DO I=1,4
C          NI=IXC(I+1,N)-1
C          KNOD2SH(NI)=KNOD2SH(NI)+1
C          NOD2SH(KNOD2SH(NI))=N
C        END DO
C      END DO
C
C      DO N=1,NUMELTG0
C        DO I=1,3
C          NI=IXTG(I+1,N)-1
C          KNOD2SH(NI)=KNOD2SH(NI)+1
C          NOD2SH(KNOD2SH(NI))=NUMELC0+N
C        END DO
C      END DO
C
C      DO N=NUMNOD0,1,-1
C        KNOD2SH(N)=KNOD2SH(N-1)
C      END DO
C      KNOD2SH(0)=0
C
C      DO N=1,NUMELC0
C        IP  =IPARTC(N)
C        NLEV=IPART(10,IP)
C        IF(NLEV==0) CYCLE
C        DO I=1,4
C          NI=IXC(I+1,N)
C          NJ=IXC(MOD(I,4)+2,N)
C
C          DO K=KNOD2SH(NI-1)+1,KNOD2SH(NI)
C            P=NOD2SH(K)
C            IF(P/=N)THEN
C              DO L=KNOD2SH(NJ-1)+1,KNOD2SH(NJ)
C            Q=NOD2SH(L)
C            IF(Q==P)THEN
C                  IF(SH4NEIGHB(I,N)/=0)THEN
C                    CALL ANSTCKI(IXC(NIXC,N))
C                    CALL ANSTCKI(IPART(1,IP))
C                    CALL ANCERR(640,ANINFO_BLIND_1)
C                  ELSE
C                    SH4NEIGHB(I,N)=Q
C                  END IF
C            END IF
C              END DO
C            END IF
C          END DO
C        END DO
C      END DO
C
C      DO N=1,NUMELTG0
C        IP  =IPARTTG(N)
C        NLEV=IPART(10,IP)
C        IF(NLEV==0) CYCLE
C        DO I=1,3
C          NI=IXTG(I+1,N)
C          NJ=IXTG(MOD(I,3)+2,N)
C
C          DO K=KNOD2SH(NI-1)+1,KNOD2SH(NI)
C            P=NOD2SH(K)
C            IF(P/=N)THEN
C              DO L=KNOD2SH(NJ-1)+1,KNOD2SH(NJ)
C            Q=NOD2SH(L)
C            IF(Q==P)THEN
C                  IF(SH3NEIGHB(I,N)/=0)THEN
C                    CALL ANSTCKI(IXTG(NIXTG,N))
C                    CALL ANSTCKI(IPART(1,IP))
C                    CALL ANCERR(641,ANINFO_BLIND_1)
C                  ELSE
C                    SH3NEIGHB(I,N)=Q
C              END IF
C            END IF
C              END DO
C            END IF
C          END DO
C        END DO
C      END DO
C-------------------------------------
        NUMNOD=NUMNOD_NEW
C--------------------------------------------------
C     RE-CONSTITUTION DU TABLEAU INVERSE DES NOEUDS
        CALL CONSTIT(ITAB,ITABM1,NUMNOD)
        RETURN
C-------------------------------------
  999   CALL FREERR(1)
        RETURN
      END
