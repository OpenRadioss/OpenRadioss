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
Chd|  ADMINI                        source/model/remesh/admini.F  
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|        ADMMAP3                       source/model/remesh/admmap3.F 
Chd|        ADMMAP4                       source/model/remesh/admmap4.F 
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        REMESH_MOD                    share/modules/remesh_mod.F    
Chd|====================================================================
      SUBROUTINE ADMINI(IXC ,IPARTC ,IXTG    ,IPARTTG  ,IPART ,
     .                  IGEO,IPM    ,IPARG   ,X        ,MS    ,
     .                  IN  ,ELBUF_TAB,SH4TREE ,IPADMESH ,MSC   ,
     .                  INC ,SH3TREE,MSTG    ,INTG     ,PTG   ,
     .                  SH4TRIM ,SH3TRIM ,MSCND ,INCND ,PM    ,
     .                  MCP ,MCPC   ,MCPTG   ,TAGTRIMC,TAGTRIMTG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE REMESH_MOD
      USE MESSAGE_MOD
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "scr17_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*), IPARTC(*), IXTG(NIXTG,*), IPARTTG(*),
     .        IPART(LIPART1,*), IPARG(NPARG,*),
     .        IGEO(NPROPGI,*), IPM(NPROPMI,*),
     .        SH4TREE(KSH4TREE,*), IPADMESH(KIPADMESH,*),
     .        SH3TREE(KSH3TREE,*), SH4TRIM(*), SH3TRIM(*),
     .        TAGTRIMC(*), TAGTRIMTG(*)
      my_real
     .        X(3,*), MS(*), IN(*), MSC(*), INC(*),
     .        MSTG(*), INTG(*), PTG(3,*), MSCND(*), INCND(*),
     .        PM(NPROPM,*), MCP(*), MCPC(*), MCPTG(*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,IP,INILEV,MYLEV,KINILEV,NTMP,IERR,
     .      LEVEL,LE,LELT,NELT(2*(4**LEVELMAX)),LEV,NE,SON,LELT1,LELT2,
     .      CND2MAP(2*(4**LEVELMAX))
      INTEGER NN,IB,M,N1,N2,N3,N4,NG1
      INTEGER ITRIM, KTRIM
      INTEGER I,NG,MLW,KAD,NEL,ISTRA,ISH3N,IEXPAN,LEVSON
      my_real
     .  MBIG, MCPM, MCPN
C-----------------------------------------------
      IF(ISTATCND/=0.AND.TT==ZERO)THEN
        MSCND  (1:NUMNOD) =ZERO
        INCND  (1:NUMNOD) =ZERO

        DO N=1,NUMELC

          IF(IPART(10,IPARTC(N)) > 0)THEN

            LEVEL = SH4TREE(3,N)
            IF(LEVEL==0 .OR. LEVEL==-1)THEN

              N1 = IXC(2,N)
              N2 = IXC(3,N)
              N3 = IXC(4,N)
              N4 = IXC(5,N)
              MSCND(N1)=MSCND(N1)+MSC(N)
              MSCND(N2)=MSCND(N2)+MSC(N)
              MSCND(N3)=MSCND(N3)+MSC(N)
              MSCND(N4)=MSCND(N4)+MSC(N)
              INCND(N1)=INCND(N1)+INC(N)
              INCND(N2)=INCND(N2)+INC(N)
              INCND(N3)=INCND(N3)+INC(N)
              INCND(N4)=INCND(N4)+INC(N)

              LELT   =1
              NELT(1)=N
   
              LELT1  =0
              LELT2  =1

              LEV=0

              CND2MAP=0
              IF(LEVEL < 0) CND2MAP(1)=1

              DO WHILE (LEV < LEVELMAX)
                DO LE=LELT1+1,LELT2
  
                  NE =NELT(LE)
                  DO IB=1,4

                    M = SH4TREE(2,NE)+IB-1

                    LELT=LELT+1
                    NELT(LELT)=M
C
                    IF(CND2MAP(LE)==1)THEN
                      N1 = IXC(2,M)
                      N2 = IXC(3,M)
                      N3 = IXC(4,M)
                      N4 = IXC(5,M)
                      MBIG=MSC(N)
                      MSCND(N1)=MSCND(N1)+MBIG
                      MSCND(N2)=MSCND(N2)+MBIG
                      MSCND(N3)=MSCND(N3)+MBIG
                      MSCND(N4)=MSCND(N4)+MBIG
                      MBIG=INC(N)
                      INCND(N1)=INCND(N1)+MBIG
                      INCND(N2)=INCND(N2)+MBIG
                      INCND(N3)=INCND(N3)+MBIG
                      INCND(N4)=INCND(N4)+MBIG

                      IF(SH4TREE(3,M) < 0) CND2MAP(LELT)=1
                    END IF

                  END DO

                  IF(CND2MAP(LE)==1)THEN
                    N1 = IXC(2,NE)
                    N2 = IXC(3,NE)
                    N3 = IXC(4,NE)
                    N4 = IXC(5,NE)
                    MBIG=MSC(N)
                    MSCND(N1)=MAX(ZERO,MSCND(N1)-MBIG)
                    MSCND(N2)=MAX(ZERO,MSCND(N2)-MBIG)
                    MSCND(N3)=MAX(ZERO,MSCND(N3)-MBIG)
                    MSCND(N4)=MAX(ZERO,MSCND(N4)-MBIG)
                    MBIG=INC(N)
                    INCND(N1)=MAX(ZERO,INCND(N1)-MBIG)
                    INCND(N2)=MAX(ZERO,INCND(N2)-MBIG)
                    INCND(N3)=MAX(ZERO,INCND(N3)-MBIG)
                    INCND(N4)=MAX(ZERO,INCND(N4)-MBIG)
                  END IF

                END DO

                LEV   =LEV+1
                LELT1 =LELT2
                LELT2 =LELT

              END DO

              DO LE=1,LELT
                MSC(NELT(LE))=MSC(N)
                INC(NELT(LE))=INC(N)
              END DO
            END IF

          END IF

        END DO


        DO N=1,NUMELTG

          IF(IPART(10,IPARTTG(N)) > 0)THEN

            LEVEL = SH3TREE(3,N)
            IF(LEVEL==0 .OR. LEVEL==-1)THEN

              N1 = IXTG(2,N)
              N2 = IXTG(3,N)
              N3 = IXTG(4,N)
              MSCND(N1)=MSCND(N1)+MSTG(N)
              MSCND(N2)=MSCND(N2)+MSTG(N)
              MSCND(N3)=MSCND(N3)+MSTG(N)
              INCND(N1)=INCND(N1)+INTG(N)
              INCND(N2)=INCND(N2)+INTG(N)
              INCND(N3)=INCND(N3)+INTG(N)

              LELT   =1
              NELT(1)=N
   
              LELT1  =0
              LELT2  =1

              LEV=0

              CND2MAP=0
              IF(LEVEL < 0) CND2MAP(1)=1

              DO WHILE (LEV < LEVELMAX)
                DO LE=LELT1+1,LELT2

                  NE =NELT(LE)

                  DO IB=1,4

                    M = SH3TREE(2,NE)+IB-1

                    LELT=LELT+1
                    NELT(LELT)=M

                    IF(CND2MAP(LE)==1)THEN

                      N1 = IXTG(2,M)
                      N2 = IXTG(3,M)
                      N3 = IXTG(4,M)
                      MSCND(N1)=MSCND(N1)+MSTG(N)
                      MSCND(N2)=MSCND(N2)+MSTG(N)
                      MSCND(N3)=MSCND(N3)+MSTG(N)
                      INCND(N1)=INCND(N1)+INTG(N)
                      INCND(N2)=INCND(N2)+INTG(N)
                      INCND(N3)=INCND(N3)+INTG(N)

                      IF(SH3TREE(3,M) < 0) CND2MAP(LELT)=1
                    END IF

                  END DO

                  IF(CND2MAP(LE)==1)THEN
                    N1 = IXTG(2,NE)
                    N2 = IXTG(3,NE)
                    N3 = IXTG(4,NE)
                    MBIG=MSTG(N)
                    MSCND(N1)=MAX(ZERO,MSCND(N1)-MBIG)
                    MSCND(N2)=MAX(ZERO,MSCND(N2)-MBIG)
                    MSCND(N3)=MAX(ZERO,MSCND(N3)-MBIG)
                    MBIG=INTG(N)
                    INCND(N1)=MAX(ZERO,INCND(N1)-MBIG)
                    INCND(N2)=MAX(ZERO,INCND(N2)-MBIG)
                    INCND(N3)=MAX(ZERO,INCND(N3)-MBIG)
                  END IF

                END DO

                LEV   =LEV+1
                LELT1 =LELT2
                LELT2 =LELT

              END DO

              DO LE=1,LELT
                MSTG(NELT(LE))=MSTG(N)
                INTG(NELT(LE))=INTG(N)
              END DO
            END IF

          END IF

        END DO

      END IF
C-----------------------------------------------
      NSH4ACT=0
      DO N=1,NUMELC
        IF(IPART(10,IPARTC(N)) > 0 .AND.
     .     SH4TREE(3,N) >= 0)THEN
          NSH4ACT=NSH4ACT+1
          LSH4ACT(NSH4ACT)=N
        END IF 
      END DO
C-----------------------------------------------
      IF(LSH4TRIM > 0)THEN

 5      CONTINUE

        KTRIM=0
        NTMP   =NSH4ACT
        DO NN=1,NTMP
          N    =LSH4ACT(NN)

          ITRIM=SH4TRIM(N)
          IF(ITRIM/=0)THEN

            KTRIM=1

            MYLEV=SH4TREE(3,N)
            IF(MYLEV == LEVELMAX)THEN
C
C             destruction
              IF(ITRIM/=-1)THEN
                CALL ANCMSG(MSGID=154,ANMODE=ANINFO,
     .            I1=ixc(nixc,n),I2=mylev,I3=itrim)
                CALL ARRET(2)
              END IF
              NG   =SH4TREE(4,N)
              MLW  = IPARG(1,NG)
              NEL  = IPARG(2,NG)
              NFT  = IPARG(3,NG)
              KAD  = IPARG(4,NG)
              NPT  = IPARG(6,NG)
              ISTRA= IPARG(44,NG)
              JHBE = IPARG(23,NG)
              IGTYP= IPARG(38,NG)
              IEXPAN=IPARG(49,NG)
              I    =N-NFT
              ELBUF_TAB(NG)%GBUF%OFF(I) = ZERO ! off
C
C             goes to sleep
              LSH4ACT(NN) =0
              SH4TREE(3,N)=-(SH4TREE(3,N)+1)

            ELSE  
C
C             mapping et descente au niveau suivant
C
              IF(ITRIM==-1)THEN
              NG   =SH4TREE(4,N)
              NFT  = IPARG(3,NG)
                 I    =N-NFT
                 ELBUF_TAB(NG)%GBUF%OFF(I) = ZERO ! off
                 
              ENDIF
              DO IB=1,4

                M = SH4TREE(2,N)+IB-1
C
                N1 = IXC(2,M)
                N2 = IXC(3,M)
                N3 = IXC(4,M)
                N4 = IXC(5,M)
C
C               wake up the son
                SH4TREE(3,M)=-SH4TREE(3,M)-1
#include "lockon.inc"
                NSH4ACT=NSH4ACT+1
                LSH4ACT(NSH4ACT)=M
C	    
C               1/4 of the element mass has been stored
                IF(ISTATCND==0)THEN          
                  MS(N1)=MS(N1)+MSC(M)
                  MS(N2)=MS(N2)+MSC(M)
                  MS(N3)=MS(N3)+MSC(M)
                  MS(N4)=MS(N4)+MSC(M)
                  IN(N1)=IN(N1)+INC(M)
                  IN(N2)=IN(N2)+INC(M)
                  IN(N3)=IN(N3)+INC(M)
                  IN(N4)=IN(N4)+INC(M)
                ELSE
                  MBIG=MSC(M)
                  MSCND(N1)=MSCND(N1)+MBIG
                  MSCND(N2)=MSCND(N2)+MBIG
                  MSCND(N3)=MSCND(N3)+MBIG
                  MSCND(N4)=MSCND(N4)+MBIG
                  MBIG=INC(M)
                  INCND(N1)=INCND(N1)+MBIG
                  INCND(N2)=INCND(N2)+MBIG
                  INCND(N3)=INCND(N3)+MBIG
                  INCND(N4)=INCND(N4)+MBIG
                END IF
C
                IF(ITHERM_FE > 0)THEN 
                  MCPM=MCPC(M)
                  MCP(N1)=MCP(N1)+MCPM
                  MCP(N2)=MCP(N2)+MCPM
                  MCP(N3)=MCP(N3)+MCPM
                  MCP(N4)=MCP(N4)+MCPM
                END IF
C
C               map fields to the son
                NG1 =SH4TREE(4,M)
                IPARG(8,NG1)=0
#include "lockoff.inc"
              END DO
C
              CALL ADMMAP4(N, IXC, X, IPARG, ELBUF_TAB,
     .                    IGEO, IPM ,SH4TREE)
C
              N1 = IXC(2,N)
              N2 = IXC(3,N)
              N3 = IXC(4,N)
              N4 = IXC(5,N)
#include "lockon.inc"
              IF(ISTATCND==0)THEN          
                MS(N1)=MAX(ZERO,MS(N1)-MSC(N))
                MS(N2)=MAX(ZERO,MS(N2)-MSC(N))
                MS(N3)=MAX(ZERO,MS(N3)-MSC(N))
                MS(N4)=MAX(ZERO,MS(N4)-MSC(N))
                IN(N1)=MAX(ZERO,IN(N1)-INC(N))
                IN(N2)=MAX(ZERO,IN(N2)-INC(N))
                IN(N3)=MAX(ZERO,IN(N3)-INC(N))
                IN(N4)=MAX(ZERO,IN(N4)-INC(N))
              ELSE
                MBIG=MSC(N)
                MSCND(N1)=MAX(ZERO,MSCND(N1)-MBIG)
                MSCND(N2)=MAX(ZERO,MSCND(N2)-MBIG)
                MSCND(N3)=MAX(ZERO,MSCND(N3)-MBIG)
                MSCND(N4)=MAX(ZERO,MSCND(N4)-MBIG)
                MBIG=INC(N)
                INCND(N1)=MAX(ZERO,INCND(N1)-MBIG)
                INCND(N2)=MAX(ZERO,INCND(N2)-MBIG)
                INCND(N3)=MAX(ZERO,INCND(N3)-MBIG)
                INCND(N4)=MAX(ZERO,INCND(N4)-MBIG)
              END IF
#include "lockoff.inc"
C 
              IF(ITHERM_FE > 0)THEN 
#include "lockon.inc"
                MCPN=MCPC(N)
                MCP(N1)=MAX(ZERO,MCP(N1)-MCPN)
                MCP(N2)=MAX(ZERO,MCP(N2)-MCPN)
                MCP(N3)=MAX(ZERO,MCP(N3)-MCPN)
                MCP(N4)=MAX(ZERO,MCP(N4)-MCPN)
#include "lockoff.inc"
              END IF
C
C             goes to sleep
              LSH4ACT(NN) =0
              SH4TREE(3,N)=-(SH4TREE(3,N)+1)

              IF(ITRIM==-1)THEN
                DO IB=1,4
                  M = SH4TREE(2,N)+IB-1
                  IF(SH4TRIM(M)/=-1)THEN
                    CALL ANCMSG(MSGID=155,ANMODE=ANINFO,
     .              I1=ixc(nixc,n),I2=itrim,
     .              I3=ixc(nixc,m),I4=SH4TRIM(M))
                    call arret(2)
                  END IF
                END DO
              END IF
            END IF
          END IF
        END DO

        IF(KTRIM/=0)THEN
C
          IDEL7NOK=1
C
C         compactage de LSH4ACT
          NTMP   =NSH4ACT
          NSH4ACT=0
          DO NN=1,NTMP
            N=LSH4ACT(NN)
            IF(N/=0)THEN
              NSH4ACT=NSH4ACT+1
              LSH4ACT(NSH4ACT)=N
            END IF
          END DO 
          GOTO 5
        END IF
C
C nothing to trim anymore
        LSH4TRIM=-LSH4TRIM
      END IF

C-----------------------------------------------
      NSH4OLD=NSH4ACT
 10   CONTINUE

      KINILEV=0

      NTMP   =NSH4ACT
      DO NN=1,NTMP
        N    =LSH4ACT(NN)
        MYLEV=SH4TREE(3,N)
        IP=IPARTC(N)
        INILEV=IPADMESH(1,IP)
        IF(MYLEV<INILEV)THEN
          IADMESH=1
          KINILEV=1
 
          DO IB=1,4

            M = SH4TREE(2,N)+IB-1
C
            N1 = IXC(2,M)
            N2 = IXC(3,M)
            N3 = IXC(4,M)
            N4 = IXC(5,M)
C
C           wake up the son
            SH4TREE(3,M)=-SH4TREE(3,M)-1
#include "lockon.inc"
            NSH4ACT=NSH4ACT+1
            LSH4ACT(NSH4ACT)=M
C	  
C           1/4 of the element mass has been stored
            IF(ISTATCND==0)THEN          
              MS(N1)=MS(N1)+MSC(M)
              MS(N2)=MS(N2)+MSC(M)
              MS(N3)=MS(N3)+MSC(M)
              MS(N4)=MS(N4)+MSC(M)
              IN(N1)=IN(N1)+INC(M)
              IN(N2)=IN(N2)+INC(M)
              IN(N3)=IN(N3)+INC(M)
              IN(N4)=IN(N4)+INC(M)
            ELSE
              MBIG=MSC(M)
              MSCND(N1)=MSCND(N1)+MBIG
              MSCND(N2)=MSCND(N2)+MBIG
              MSCND(N3)=MSCND(N3)+MBIG
              MSCND(N4)=MSCND(N4)+MBIG
              MBIG=INC(M)
              INCND(N1)=INCND(N1)+MBIG
              INCND(N2)=INCND(N2)+MBIG
              INCND(N3)=INCND(N3)+MBIG
              INCND(N4)=INCND(N4)+MBIG
            END IF
C
            IF(ITHERM_FE > 0)THEN 
              MCPM=MCPC(M)
              MCP(N1)=MCP(N1)+MCPM
              MCP(N2)=MCP(N2)+MCPM
              MCP(N3)=MCP(N3)+MCPM
              MCP(N4)=MCP(N4)+MCPM
            END IF
C
C
C           map fields to the son
            NG1 =SH4TREE(4,M)
            IPARG(8,NG1)=0
#include "lockoff.inc"
          END DO
C
          CALL ADMMAP4(N, IXC, X, IPARG, ELBUF_TAB,
     .                IGEO, IPM ,SH4TREE)
C
          N1 = IXC(2,N)
          N2 = IXC(3,N)
          N3 = IXC(4,N)
          N4 = IXC(5,N)
#include "lockon.inc"
          IF(ISTATCND==0)THEN          
            MS(N1)=MAX(ZERO,MS(N1)-MSC(N))
            MS(N2)=MAX(ZERO,MS(N2)-MSC(N))
            MS(N3)=MAX(ZERO,MS(N3)-MSC(N))
            MS(N4)=MAX(ZERO,MS(N4)-MSC(N))
            IN(N1)=MAX(ZERO,IN(N1)-INC(N))
            IN(N2)=MAX(ZERO,IN(N2)-INC(N))
            IN(N3)=MAX(ZERO,IN(N3)-INC(N))
            IN(N4)=MAX(ZERO,IN(N4)-INC(N))
          ELSE
            MBIG=MSC(N)
            MSCND(N1)=MAX(ZERO,MSCND(N1)-MBIG)
            MSCND(N2)=MAX(ZERO,MSCND(N2)-MBIG)
            MSCND(N3)=MAX(ZERO,MSCND(N3)-MBIG)
            MSCND(N4)=MAX(ZERO,MSCND(N4)-MBIG)
            MBIG=INC(N)
            INCND(N1)=MAX(ZERO,INCND(N1)-MBIG)
            INCND(N2)=MAX(ZERO,INCND(N2)-MBIG)
            INCND(N3)=MAX(ZERO,INCND(N3)-MBIG)
            INCND(N4)=MAX(ZERO,INCND(N4)-MBIG)
          END IF
#include "lockoff.inc"
C
          IF(ITHERM_FE > 0)THEN 
#include "lockon.inc"
            MCPN=MCPC(N)
            MCP(N1)=MAX(ZERO,MCP(N1)-MCPN)
            MCP(N2)=MAX(ZERO,MCP(N2)-MCPN)
            MCP(N3)=MAX(ZERO,MCP(N3)-MCPN)
            MCP(N4)=MAX(ZERO,MCP(N4)-MCPN)
#include "lockoff.inc"
          END IF
C
C         goes to sleep
          LSH4ACT(NN) =0
          SH4TREE(3,N)=-(SH4TREE(3,N)+1)
        END IF
      END DO

      IF(KINILEV/=0)THEN
C
C     compactage de LSH4ACT
        NTMP   =NSH4ACT
        NSH4ACT=0
        DO NN=1,NTMP
          N=LSH4ACT(NN)
          IF(N/=0)THEN
            NSH4ACT=NSH4ACT+1
            LSH4ACT(NSH4ACT)=N
          END IF
        END DO 
        GOTO 10
      END IF

C----------------------------------------------
C     TRIANGLES
C----------------------------------------------
      NSH3ACT=0
      DO N=1,NUMELTG
        IF(IPART(10,IPARTTG(N)) > 0 .AND.
     .     SH3TREE(3,N) >= 0)THEN
          NSH3ACT=NSH3ACT+1
          LSH3ACT(NSH3ACT)=N
        END IF 
      END DO
C-----------------------------------------------
      IF(LSH3TRIM > 0)THEN

 15     CONTINUE

        KTRIM=0
        NTMP   =NSH3ACT
        DO NN=1,NTMP
          N    =LSH3ACT(NN)

          ITRIM=SH3TRIM(N)
          IF(ITRIM/=0)THEN

            KTRIM=1

            MYLEV=SH3TREE(3,N)
            IF(MYLEV == LEVELMAX)THEN
C
C             destruction
              IF(ITRIM/=-1)THEN
                CALL ANCMSG(MSGID=156,ANMODE=ANINFO)
                call arret(2)
              END IF
              NG   = SH3TREE(4,N)
              MLW  = IPARG(1,NG)
              NEL  = IPARG(2,NG)
              NFT  = IPARG(3,NG)
              KAD  = IPARG(4,NG)
              NPT  = IPARG(6,NG)
              ISTRA= IPARG(44,NG)
              ISH3N= IPARG(23,NG)
              IGTYP= IPARG(38,NG)
              IEXPAN=IPARG(49,NG)
              I    =N-NFT
              ELBUF_TAB(NG)%GBUF%OFF(I) = ZERO ! off
C
C             goes to sleep
              LSH3ACT(NN) =0
              SH3TREE(3,N)=-(SH3TREE(3,N)+1)

            ELSE  
C
C             mapping et descente au niveau suivant

              DO IB=1,4

                M = SH3TREE(2,N)+IB-1
C
                N1 = IXTG(2,M)
                N2 = IXTG(3,M)
                N3 = IXTG(4,M)
C
C               wake up the son
                SH3TREE(3,M)=-SH3TREE(3,M)-1
#include "lockon.inc"
                NSH3ACT=NSH3ACT+1
                LSH3ACT(NSH3ACT)=M
C	    
C             1/4 of the element mass has been stored
                IF(ISTATCND==0)THEN          
                  MS(N1)=MS(N1)+MSTG(M)*PTG(1,M)
                  MS(N2)=MS(N2)+MSTG(M)*PTG(2,M)
                  MS(N3)=MS(N3)+MSTG(M)*PTG(3,M)
                  IN(N1)=IN(N1)+INTG(M)*PTG(1,M)
                  IN(N2)=IN(N2)+INTG(M)*PTG(2,M)
                  IN(N3)=IN(N3)+INTG(M)*PTG(3,M)
                ELSE
                  MBIG=MSTG(M)
                  MSCND(N1)=MSCND(N1)+MBIG
                  MSCND(N2)=MSCND(N2)+MBIG
                  MSCND(N3)=MSCND(N3)+MBIG
                  MBIG=INTG(M)
                  INCND(N1)=INCND(N1)+MBIG
                  INCND(N2)=INCND(N2)+MBIG
                  INCND(N3)=INCND(N3)+MBIG
                END IF
C
                IF(ITHERM_FE > 0)THEN 
                  MCP(N1)=MCP(N1)+MCPTG(M)*PTG(1,M)
                  MCP(N2)=MCP(N2)+MCPTG(M)*PTG(2,M)
                  MCP(N3)=MCP(N3)+MCPTG(M)*PTG(3,M)
                END IF
C
C               map fields to the son
                NG1 =SH3TREE(4,M)
                IPARG(8,NG1)=0
#include "lockoff.inc"
              END DO
C
              CALL ADMMAP3(N, IXTG, X, IPARG, ELBUF_TAB,
     .                    IGEO, IPM ,SH3TREE)
C
              N1 = IXTG(2,N)
              N2 = IXTG(3,N)
              N3 = IXTG(4,N)
              IF(ISTATCND==0)THEN          
                MS(N1)=MAX(ZERO,MS(N1)-MSTG(N)*PTG(1,N))
                MS(N2)=MAX(ZERO,MS(N2)-MSTG(N)*PTG(2,N))
                MS(N3)=MAX(ZERO,MS(N3)-MSTG(N)*PTG(3,N))
                IN(N1)=MAX(ZERO,IN(N1)-INTG(N)*PTG(1,N))
                IN(N2)=MAX(ZERO,IN(N2)-INTG(N)*PTG(2,N))
                IN(N3)=MAX(ZERO,IN(N3)-INTG(N)*PTG(3,N))
              ELSE
                MBIG=MSTG(N)
                MSCND(N1)=MAX(ZERO,MSCND(N1)-MBIG)
                MSCND(N2)=MAX(ZERO,MSCND(N2)-MBIG)
                MSCND(N3)=MAX(ZERO,MSCND(N3)-MBIG)
                MBIG=INTG(N)
                INCND(N1)=MAX(ZERO,INCND(N1)-MBIG)
                INCND(N2)=MAX(ZERO,INCND(N2)-MBIG)
                INCND(N3)=MAX(ZERO,INCND(N3)-MBIG)
              END IF
C
              IF(ITHERM_FE > 0)THEN 
#include "lockon.inc"
                MCP(N1)=MAX(ZERO,MCP(N1)-MCPTG(N)*PTG(1,N))
                MCP(N2)=MAX(ZERO,MCP(N2)-MCPTG(N)*PTG(2,N))
                MCP(N3)=MAX(ZERO,MCP(N3)-MCPTG(N)*PTG(3,N))
#include "lockoff.inc"
              END IF
C
C             goes to sleep
              LSH3ACT(NN) =0
              SH3TREE(3,N)=-(SH3TREE(3,N)+1)

              IF(ITRIM==-1)THEN
                DO IB=1,4
                  M = SH3TREE(2,N)+IB-1
                  IF(SH3TRIM(M)/=-1)THEN
                    CALL ANCMSG(MSGID=156,ANMODE=ANINFO)
                    call arret(2)
                  END IF
                END DO
              END IF
            END IF
          END IF
        END DO

        IF(KTRIM/=0)THEN
C
          IDEL7NOK=1
C
C         compactage de LSH4ACT
          NTMP   =NSH3ACT
          NSH3ACT=0
          DO NN=1,NTMP
            N=LSH3ACT(NN)
            IF(N/=0)THEN
              NSH3ACT=NSH3ACT+1
              LSH3ACT(NSH3ACT)=N
            END IF
          END DO 
          GOTO 15
        END IF
C
C nothing to trim anymore
        LSH3TRIM=-LSH3TRIM
      END IF
C-----------------------------------------------
      NSH3OLD=NSH3ACT
 20   CONTINUE

      KINILEV=0

      NTMP   =NSH3ACT
      DO NN=1,NTMP
        N    =LSH3ACT(NN)
        MYLEV=SH3TREE(3,N)
        IP   =IPARTTG(N)
        INILEV=IPADMESH(1,IP)
        IF(MYLEV<INILEV)THEN
          IADMESH=1
          KINILEV=1

          DO IB=1,4

            M = SH3TREE(2,N)+IB-1
C
            N1 = IXTG(2,M)
            N2 = IXTG(3,M)
            N3 = IXTG(4,M)
C
C           wake up the son
            SH3TREE(3,M)=-SH3TREE(3,M)-1
#include "lockon.inc"
            NSH3ACT=NSH3ACT+1
            LSH3ACT(NSH3ACT)=M
C	  
C           1/4 of the element mass has been stored
            IF(ISTATCND==0)THEN          
              MS(N1)=MS(N1)+MSTG(M)*PTG(1,M)
              MS(N2)=MS(N2)+MSTG(M)*PTG(2,M)
              MS(N3)=MS(N3)+MSTG(M)*PTG(3,M)
              IN(N1)=IN(N1)+INTG(M)*PTG(1,M)
              IN(N2)=IN(N2)+INTG(M)*PTG(2,M)
              IN(N3)=IN(N3)+INTG(M)*PTG(3,M)
            ELSE
              MBIG=MSTG(M)
              MSCND(N1)=MSCND(N1)+MBIG
              MSCND(N2)=MSCND(N2)+MBIG
              MSCND(N3)=MSCND(N3)+MBIG
              MBIG=INTG(M)
              INCND(N1)=INCND(N1)+MBIG
              INCND(N2)=INCND(N2)+MBIG
              INCND(N3)=INCND(N3)+MBIG
            END IF
C
            IF(ITHERM_FE > 0)THEN 
              MCP(N1)=MCP(N1)+MCPTG(M)*PTG(1,M)
              MCP(N2)=MCP(N2)+MCPTG(M)*PTG(2,M)
              MCP(N3)=MCP(N3)+MCPTG(M)*PTG(3,M)
            END IF
C
C           map fields to the son
            NG1 =SH3TREE(4,M)
            IPARG(8,NG1)=0
#include "lockoff.inc"
          END DO
C
          CALL ADMMAP3(N, IXTG, X, IPARG, ELBUF_TAB,
     .                IGEO, IPM , SH3TREE)
C
          N1 = IXTG(2,N)
          N2 = IXTG(3,N)
          N3 = IXTG(4,N)
          IF(ISTATCND==0)THEN          
            MS(N1)=MAX(ZERO,MS(N1)-MSTG(N)*PTG(1,N))
            MS(N2)=MAX(ZERO,MS(N2)-MSTG(N)*PTG(2,N))
            MS(N3)=MAX(ZERO,MS(N3)-MSTG(N)*PTG(3,N))
            IN(N1)=MAX(ZERO,IN(N1)-INTG(N)*PTG(1,N))
            IN(N2)=MAX(ZERO,IN(N2)-INTG(N)*PTG(2,N))
            IN(N3)=MAX(ZERO,IN(N3)-INTG(N)*PTG(3,N))
          ELSE
            MBIG=MSTG(N)
            MSCND(N1)=MAX(ZERO,MSCND(N1)-MBIG)
            MSCND(N2)=MAX(ZERO,MSCND(N2)-MBIG)
            MSCND(N3)=MAX(ZERO,MSCND(N3)-MBIG)
            MBIG=INTG(N)
            INCND(N1)=MAX(ZERO,INCND(N1)-MBIG)
            INCND(N2)=MAX(ZERO,INCND(N2)-MBIG)
            INCND(N3)=MAX(ZERO,INCND(N3)-MBIG)
          END IF
C
          IF(ITHERM_FE > 0)THEN 
#include "lockon.inc"
            MCP(N1)=MAX(ZERO,MCP(N1)-MCPTG(N)*PTG(1,N))
            MCP(N2)=MAX(ZERO,MCP(N2)-MCPTG(N)*PTG(2,N))
            MCP(N3)=MAX(ZERO,MCP(N3)-MCPTG(N)*PTG(3,N))
#include "lockoff.inc"
          END IF
C
C         goes to sleep
          LSH3ACT(NN) =0
          SH3TREE(3,N)=-(SH3TREE(3,N)+1)
        END IF
      END DO

      IF(KINILEV/=0)THEN
C
C     compactage de LSH4ACT
        NTMP   =NSH3ACT
        NSH3ACT=0
        DO NN=1,NTMP
          N=LSH3ACT(NN)
          IF(N/=0)THEN
            NSH3ACT=NSH3ACT+1
            LSH3ACT(NSH3ACT)=N
          END IF
        END DO 
        GOTO 20
      END IF
C
C     Tag element that are inactifs and one of its sons is actif for Idel in interface
       IF(NADMESH/=0.AND.IDEL7NG>=1)THEN
         TAGTRIMC(1:NUMELC) = 0 
         TAGTRIMTG(1:NUMELTG) = 0 
         DO N=1,NUMELC

           IF(IPART(10,IPARTC(N)) > 0)THEN
             LEVEL = SH4TREE(3,N)
             ITRIM=SH4TRIM(N)
             IF(LEVEL <0.AND.LEVEL/=(-LEVELMAX-1).AND.ITRIM >=0) THEN
               LELT   =1
               NELT(1)=N
   
               LELT1  =0
               LELT2  =1
 
               LEV=0
               DO WHILE (LEV < LEVELMAX)
                DO LE=LELT1+1,LELT2
  
                  NE =NELT(LE)
                  DO IB=1,4

                    M = SH4TREE(2,NE)+IB-1

                    LELT=LELT+1
                    NELT(LELT)=M 

                    LEVSON =    SH4TREE(3,M)       
                    IF(LEVSON >= 0) THEN
                       TAGTRIMC(N) = 1
                    ENDIF
                  ENDDO
                ENDDO
                LEV   =LEV+1
                LELT1 =LELT2
                LELT2 =LELT
               ENDDO
c            ELSEIF (LEVEL==(-LEVELMAX-1)) THEN
            ELSEIF (LEVEL <0.AND.ITRIM == -1) THEN
                TAGTRIMC(N) = 1
                NG   =SH4TREE(4,N)
                NFT  = IPARG(3,NG)
                I    =N-NFT
                ELBUF_TAB(NG)%GBUF%OFF(I) = ZERO ! off
            ENDIF
          ENDIF

        ENDDO
        DO N=1,NUMELTG

          IF(IPART(10,IPARTTG(N)) > 0)THEN
            LEVEL = SH3TREE(3,N)
            ITRIM=SH3TRIM(N)
            IF(LEVEL <0.AND.ITRIM >=0) THEN
              LELT   =1
              NELT(1)=N
   
              LELT1  =0
              LELT2  =1

              LEV=0
               DO WHILE (LEV < LEVELMAX)
                DO LE=LELT1+1,LELT2
  
                  NE =NELT(LE)
                  DO IB=1,4

                    M = SH3TREE(2,NE)+IB-1

                    LELT=LELT+1
                    NELT(LELT)=M             
                    IF(SH3TREE(3,M) >= 0) THEN
                       TAGTRIMTG(N) = 1
                    ENDIF
                  ENDDO
                ENDDO
                LEV   =LEV+1
                LELT1 =LELT2
                LELT2 =LELT
               ENDDO
             ELSEIF (LEVEL <0.AND.ITRIM == -1) THEN
                TAGTRIMTG(N) = 1
                NG   =SH3TREE(4,N)
                NFT  = IPARG(3,NG)
                I    =N-NFT
                ELBUF_TAB(NG)%GBUF%OFF(I) = ZERO ! off
             ENDIF
          ENDIF
        ENDDO
      ENDIF
C
C     tableaux de travail.
      ALLOCATE(TAGNOD(NUMNOD),STAT=IERR)
      IF (IERR /= 0) CALL ARRET(2)

      ALLOCATE(NODNORM(3,NUMNOD),STAT=IERR)
      IF (IERR /= 0) CALL ARRET(2)

      RETURN
      END     


