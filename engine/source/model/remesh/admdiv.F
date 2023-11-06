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
Chd|  ADMDIV                        source/model/remesh/admdiv.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F
Chd|-- calls ---------------
Chd|        ADMMAP3                       source/model/remesh/admmap3.F
Chd|        ADMMAP4                       source/model/remesh/admmap4.F
Chd|        ADMNORM3                      source/model/remesh/admnorm.F
Chd|        ADMNORM4                      source/model/remesh/admnorm.F
Chd|        MY_BARRIER                    source/system/machine.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        REMESH_MOD                    share/modules/remesh_mod.F
Chd|====================================================================
      SUBROUTINE ADMDIV(IXC  ,IPARTC  ,IXTG   ,IPARTTG,IPART,
     .                  ITASK,ICONTACT,IPARG  ,X      ,MS   ,
     .                  IN   ,RCONTACT,ELBUF_TAB,NODFT  ,NODLT,
     .                  IGEO ,IPM     ,SH4TREE,PADMESH,MSC  ,
     .                  INC  ,SH3TREE ,MSTG   ,INTG   ,PTG  ,
     .              ACONTACT ,PCONTACT ,ERR_THK_SH4, ERR_THK_SH3 ,MSCND,
     .                  INCND,PM       ,MCP   ,MCPC   ,MCPTG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE REMESH_MOD
        USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "remesh_c.inc"
#include      "task_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IXC(NIXC,*),IPARTC(*),IXTG(NIXTG,*),IPARTTG(*),
     .          IPART(LIPART1,*),ITASK,ICONTACT(*),IPARG(NPARG,*),
     .          NODFT, NODLT, IGEO(NPROPGI,*), IPM(NPROPMI,*),
     .          SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*)
        my_real
     .          X(3,*),MS(*),IN(*),RCONTACT(*),
     .          PADMESH(KPADMESH,*), MSC(*), INC(*),
     .          MSTG(*), INTG(*), PTG(3,*), ACONTACT(*), PCONTACT(*),
     .          ERR_THK_SH4(*), ERR_THK_SH3(*), MSCND(*), INCND(*),
     .          PM(NPROPM,*), MCP(*), MCPC(*), MCPTG(*)
        TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER SH4FT, SH4LT, SH3FT, SH3LT
        INTEGER NN,N,IB,M,N1,N2,N3,N4,M1,M2,M3,M4,NG1
        INTEGER LEVEL,KDIV,NTMP,L,LLNOD,LNOD(NUMNOD),
     .          LE,LELT,NELT(2*(4**LEVELMAX)),LEV,NE,SON,LELT1,LELT2,
     .          NI,IP,MYLEV
        INTEGER NSKYML, WORK(70000), I, J, K,
     .          ITRI(MAX(NUMELC,NUMELTG)), INDEX(2*MAX(NUMELC,NUMELTG))
        my_real
     .     NX,NY,NZ,AAA,
     .     X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,
     .     AL1,AL2,AL3,AL4,AL,
     .     X13,Y13,Z13,X24,Y24,Z24,X12,Y12,Z12,
     .     CC,CMAX,PP,RR,MSBIG,INBIG,
     .     MCPM, MCPN
        my_real
     .     TN1,TN2,TN3,TN4,UNT,ERR
C-----------------------------------------------
        IF(ICHKADM /= 0)THEN

          IF(ITASK==0)THEN

            TAGNOD = 0
            NODNORM= ZERO
c
c         parcours des feuilles
            LEVEL=LEVELMAX
            DO NN=PSH4KIN(LEVEL)+1,PSH4KIN(LEVEL+1)
              N   =LSH4KIN(NN)
              CALL ADMNORM4(N,IXC,X)
            END DO

            DO NN=PSH3KIN(LEVEL)+1,PSH3KIN(LEVEL+1)
              N   =LSH3KIN(NN)
              CALL ADMNORM3(N,IXTG,X)
            END DO
c
          END IF
C
          CALL MY_BARRIER
C
          DO N=NODFT,NODLT

            IF(TAGNOD(N)/=0)THEN

              NX=NODNORM(1,N)
              NY=NODNORM(2,N)
              NZ=NODNORM(3,N)

              AAA=ONE/MAX(EM30,SQRT(NX*NX+NY*NY+NZ*NZ))
              NX = NX * AAA
              NY = NY * AAA
              NZ = NZ * AAA

              NODNORM(1,N)=NX
              NODNORM(2,N)=NY
              NODNORM(3,N)=NZ
            END IF

          END DO

        END IF

        NSKYMSH4=0
        NSKYMSH3=0
C
        SH4FT = 1+ITASK*NSH4ACT/ NTHREAD
        SH4LT = (ITASK+1)*NSH4ACT/NTHREAD
C
        SH3FT = 1+ITASK*NSH3ACT/ NTHREAD
        SH3LT = (ITASK+1)*NSH3ACT/NTHREAD
C
        CALL MY_BARRIER
C
        DO NN=SH4FT,SH4LT
          N =LSH4ACT(NN)

          LEVEL=SH4TREE(3,N)
          IF( LEVEL == LEVELMAX ) CYCLE

          KDIV=0
C---
C       KDIV=1 if elt needs to be divided
C---
          N1 = IXC(2,N)
          N2 = IXC(3,N)
          N3 = IXC(4,N)
          N4 = IXC(5,N)

          X1=X(1,N1)
          Y1=X(2,N1)
          Z1=X(3,N1)
          X2=X(1,N2)
          Y2=X(2,N2)
          Z2=X(3,N2)
          X3=X(1,N3)
          Y3=X(2,N3)
          Z3=X(3,N3)
          X4=X(1,N4)
          Y4=X(2,N4)
          Z4=X(3,N4)
          AL1=(X2-X1)*(X2-X1)+(Y2-Y1)*(Y2-Y1)+(Z2-Z1)*(Z2-Z1)
          AL2=(X3-X2)*(X3-X2)+(Y3-Y2)*(Y3-Y2)+(Z3-Z2)*(Z3-Z2)
          AL3=(X4-X3)*(X4-X3)+(Y4-Y3)*(Y4-Y3)+(Z4-Z3)*(Z4-Z3)
          AL4=(X1-X4)*(X1-X4)+(Y1-Y4)*(Y1-Y4)+(Z1-Z4)*(Z1-Z4)
          AL =MAX(AL1,AL2,AL3,AL4)

          LELT   =1
          NELT(1)=N

          LELT1  =0
          LELT2  =1

          LEV=LEVEL
          DO WHILE (LEV < LEVELMAX)
            DO LE=LELT1+1,LELT2

              NE =NELT(LE)
              SON=SH4TREE(2,NE)

              LELT=LELT+1
              NELT(LELT)=SON

              LELT=LELT+1
              NELT(LELT)=SON+1

              LELT=LELT+1
              NELT(LELT)=SON+2

              LELT=LELT+1
              NELT(LELT)=SON+3

            END DO

            LEV   =LEV+1
            LELT1 =LELT2
            LELT2 =LELT

          END DO

          LLNOD=0
          DO LE=LELT1+1,LELT2

            NE=NELT(LE)
            LLNOD=LLNOD+1
            LNOD(LLNOD)=IXC(2,NE)
            LLNOD=LLNOD+1
            LNOD(LLNOD)=IXC(3,NE)
            LLNOD=LLNOD+1
            LNOD(LLNOD)=IXC(4,NE)
            LLNOD=LLNOD+1
            LNOD(LLNOD)=IXC(5,NE)

          END DO

          DO L=1,LLNOD

            NI=LNOD(L)

            PP=PCONTACT(NI)
            CC=ACONTACT(NI)
            IF(PP > ONE .AND. CC < ZEP9999)THEN
              KDIV=1
              EXIT
            END IF

            RR=RCONTACT(NI)
            IF(AL > HALF*RR*RR)THEN
              KDIV=1
              EXIT
            END IF

          END DO

          IF(KDIV==0.AND.ICHKADM/=0)THEN


C
C         Angle criteria
            IP   =IPARTC(N)
            CMAX =PADMESH(1,IP)

            X13 = X3 - X1
            Y13 = Y3 - Y1
            Z13 = Z3 - Z1

            X24 = X4 - X2
            Y24 = Y4 - Y2
            Z24 = Z4 - Z2

            NX = Y13*Z24 - Z13*Y24
            NY = Z13*X24 - X13*Z24
            NZ = X13*Y24 - Y13*X24

            AAA=ONE/MAX(EM30,SQRT(NX*NX+NY*NY+NZ*NZ))
            NX = NX * AAA
            NY = NY * AAA
            NZ = NZ * AAA

            DO L=1,LLNOD
              NI=LNOD(L)
              CC=NODNORM(1,NI)*NX+NODNORM(2,NI)*NY+NODNORM(3,NI)*NZ
              IF(CC <= CMAX)THEN
                KDIV=1
                EXIT
              END IF
            END DO

C
C         Criteria / Error on thickness
            IF(IADMERRT /= 0)THEN
              ERR=ERR_THK_SH4(N)
              IF(ERR >= PADMESH(2,IP))THEN
                KDIV=1
              END IF
            END IF
          END IF

          IF( KDIV == 0 ) CYCLE

#include "lockon.inc"
          IADMESH=1
          IF(IPARIT/=0)THEN
            NSKYML   =NSKYMSH4
            NSKYMSH4 =NSKYMSH4+5
          END IF
#include "lockoff.inc"
C---
C       Divide elt N
C---
          DO IB=1,4

            M = SH4TREE(2,N)+IB-1
C
            M1 = IXC(2,M)
            M2 = IXC(3,M)
            M3 = IXC(4,M)
            M4 = IXC(5,M)
C
C         wake up the son
            SH4TREE(3,M)=-SH4TREE(3,M)-1
#include "lockon.inc"
            NSH4ACT=NSH4ACT+1
            LSH4ACT(NSH4ACT)=M
#include "lockoff.inc"
C
C         1/4 of the element mass has been stored
            IF(IPARIT==0)THEN
              IF(ISTATCND==0)THEN
#include "lockon.inc"
                MS(M1)=MS(M1)+MSC(M)
                MS(M2)=MS(M2)+MSC(M)
                MS(M3)=MS(M3)+MSC(M)
                MS(M4)=MS(M4)+MSC(M)
                IN(M1)=IN(M1)+INC(M)
                IN(M2)=IN(M2)+INC(M)
                IN(M3)=IN(M3)+INC(M)
                IN(M4)=IN(M4)+INC(M)
#include "lockoff.inc"
              ELSE
#include "lockon.inc"
                MSBIG=MSC(M)
                MSCND(M1)=MSCND(M1)+MSBIG
                MSCND(M2)=MSCND(M2)+MSBIG
                MSCND(M3)=MSCND(M3)+MSBIG
                MSCND(M4)=MSCND(M4)+MSBIG
                INBIG=INC(M)
                INCND(M1)=INCND(M1)+INBIG
                INCND(M2)=INCND(M2)+INBIG
                INCND(M3)=INCND(M3)+INBIG
                INCND(M4)=INCND(M4)+INBIG
#include "lockoff.inc"
              END IF
C
              IF(ITHERM_FE > 0)THEN
#include "lockon.inc"
                MCPM=MCPC(M)
                MCP(M1)=MCP(M1)+MCPM
                MCP(M2)=MCP(M2)+MCPM
                MCP(M3)=MCP(M3)+MCPM
                MCP(M4)=MCP(M4)+MCPM
#include "lockoff.inc"
              END IF
C
            ELSE
              NSKYML=NSKYML+1
              MSH4SKY(NSKYML)=M
            END IF
C
C         map fields to the son
            NG1 =SH4TREE(4,M)
            IPARG(8,NG1)=0

          END DO
C
          CALL ADMMAP4(N, IXC, X, IPARG, ELBUF_TAB,
     .              IGEO, IPM ,SH4TREE)
C
          IF(IPARIT==0)THEN
            IF(ISTATCND==0)THEN
#include "lockon.inc"
              MS(N1)=MAX(ZERO,MS(N1)-MSC(N))
              MS(N2)=MAX(ZERO,MS(N2)-MSC(N))
              MS(N3)=MAX(ZERO,MS(N3)-MSC(N))
              MS(N4)=MAX(ZERO,MS(N4)-MSC(N))
              IN(N1)=MAX(ZERO,IN(N1)-INC(N))
              IN(N2)=MAX(ZERO,IN(N2)-INC(N))
              IN(N3)=MAX(ZERO,IN(N3)-INC(N))
              IN(N4)=MAX(ZERO,IN(N4)-INC(N))
#include "lockoff.inc"
            ELSE
#include "lockon.inc"
              MSBIG=MSC(N)
              MSCND(N1)=MAX(ZERO,MSCND(N1)-MSBIG)
              MSCND(N2)=MAX(ZERO,MSCND(N2)-MSBIG)
              MSCND(N3)=MAX(ZERO,MSCND(N3)-MSBIG)
              MSCND(N4)=MAX(ZERO,MSCND(N4)-MSBIG)
              INBIG=INC(N)
              INCND(N1)=MAX(ZERO,INCND(N1)-INBIG)
              INCND(N2)=MAX(ZERO,INCND(N2)-INBIG)
              INCND(N3)=MAX(ZERO,INCND(N3)-INBIG)
              INCND(N4)=MAX(ZERO,INCND(N4)-INBIG)
#include "lockoff.inc"
            END IF
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
          ELSE
            NSKYML=NSKYML+1
            MSH4SKY(NSKYML)=-N
          END IF
C
C       goes to sleep
          LSH4ACT(NN) =0
          SH4TREE(3,N)=-(SH4TREE(3,N)+1)

        END DO
C
        DO NN=SH3FT,SH3LT
          N =LSH3ACT(NN)

          LEVEL=SH3TREE(3,N)
          IF( LEVEL == LEVELMAX ) CYCLE

          KDIV=0
C---
C       KDIV=1 if elt needs to be divided
C---
          N1 = IXTG(2,N)
          N2 = IXTG(3,N)
          N3 = IXTG(4,N)
          X1=X(1,N1)
          Y1=X(2,N1)
          Z1=X(3,N1)
          X2=X(1,N2)
          Y2=X(2,N2)
          Z2=X(3,N2)
          X3=X(1,N3)
          Y3=X(2,N3)
          Z3=X(3,N3)
          AL1=(X2-X1)*(X2-X1)+(Y2-Y1)*(Y2-Y1)+(Z2-Z1)*(Z2-Z1)
          AL2=(X3-X2)*(X3-X2)+(Y3-Y2)*(Y3-Y2)+(Z3-Z2)*(Z3-Z2)
          AL3=(X1-X3)*(X1-X3)+(Y1-Y3)*(Y1-Y3)+(Z1-Z3)*(Z1-Z3)
          AL =MAX(AL1,AL2,AL3)


          LELT   =1
          NELT(1)=N

          LELT1  =0
          LELT2  =1

          LEV=LEVEL
          DO WHILE (LEV < LEVELMAX)
            DO LE=LELT1+1,LELT2

              NE =NELT(LE)
              SON=SH3TREE(2,NE)

              LELT=LELT+1
              NELT(LELT)=SON

              LELT=LELT+1
              NELT(LELT)=SON+1

              LELT=LELT+1
              NELT(LELT)=SON+2

              LELT=LELT+1
              NELT(LELT)=SON+3

            END DO

            LEV   =LEV+1
            LELT1 =LELT2
            LELT2 =LELT

          END DO

          LLNOD=0
          DO LE=LELT1+1,LELT2

            NE=NELT(LE)
            LLNOD=LLNOD+1
            LNOD(LLNOD)=IXTG(2,NE)
            LLNOD=LLNOD+1
            LNOD(LLNOD)=IXTG(3,NE)
            LLNOD=LLNOD+1
            LNOD(LLNOD)=IXTG(4,NE)

          END DO

          DO L=1,LLNOD

            NI=LNOD(L)

            PP=PCONTACT(NI)
            CC=ACONTACT(NI)
            IF(PP > ONE .AND. CC < ZEP9999)THEN
              KDIV=1
              EXIT
            END IF

            RR=RCONTACT(NI)
            IF(AL > HALF*RR*RR)THEN
              KDIV=1
              EXIT
            END IF

          END DO


          IF(KDIV==0.AND.ICHKADM/=0)THEN

            IP   =IPARTTG(N)
            CMAX =PADMESH(1,IP)

            X12 = X2 - X1
            Y12 = Y2 - Y1
            Z12 = Z2 - Z1

            X13 = X3 - X1
            Y13 = Y3 - Y1
            Z13 = Z3 - Z1

            NX = Y12*Z13 - Z12*Y13
            NY = Z12*X13 - X12*Z13
            NZ = X12*Y13 - Y12*X13

            AAA=ONE/MAX(EM30,SQRT(NX*NX+NY*NY+NZ*NZ))
            NX = NX * AAA
            NY = NY * AAA
            NZ = NZ * AAA

            DO L=1,LLNOD
              NI=LNOD(L)
              CC=NODNORM(1,NI)*NX+NODNORM(2,NI)*NY+NODNORM(3,NI)*NZ
              IF(CC <= CMAX)THEN
                KDIV=1
                EXIT
              END IF
            END DO

          END IF

          IF( KDIV == 0 ) CYCLE

#include "lockon.inc"
          IADMESH=1
          IF(IPARIT/=0)THEN
            NSKYML=NSKYMSH3
            NSKYMSH3 =NSKYMSH3+5
          END IF
#include "lockoff.inc"
C---
C       Divide elt N
C---
          DO IB=1,4

            M = SH3TREE(2,N)+IB-1
C
            M1 = IXTG(2,M)
            M2 = IXTG(3,M)
            M3 = IXTG(4,M)
C
C         wake up the son
            SH3TREE(3,M)=-SH3TREE(3,M)-1
#include "lockon.inc"
            NSH3ACT=NSH3ACT+1
            LSH3ACT(NSH3ACT)=M
#include "lockoff.inc"
C
C         1/4 of the element mass has been stored
            IF(IPARIT==0)THEN
              IF(ISTATCND==0)THEN
#include "lockon.inc"
                MS(M1)=MS(M1)+MSTG(M)*PTG(1,M)
                MS(M2)=MS(M2)+MSTG(M)*PTG(2,M)
                MS(M3)=MS(M3)+MSTG(M)*PTG(3,M)
                IN(M1)=IN(M1)+INTG(M)*PTG(1,M)
                IN(M2)=IN(M2)+INTG(M)*PTG(2,M)
                IN(M3)=IN(M3)+INTG(M)*PTG(3,M)
#include "lockoff.inc"
              ELSE
#include "lockon.inc"
                MYLEV=SH3TREE(3,N)
                MSBIG=MSTG(M)
                MSCND(M1)=MSCND(M1)+MSBIG
                MSCND(M2)=MSCND(M2)+MSBIG
                MSCND(M3)=MSCND(M3)+MSBIG
                INBIG=INTG(M)
                INCND(M1)=INCND(M1)+INBIG
                INCND(M2)=INCND(M2)+INBIG
                INCND(M3)=INCND(M3)+INBIG
#include "lockoff.inc"
              END IF
C
              IF(ITHERM_FE > 0)THEN
#include "lockon.inc"
                MCP(M1)=MCP(M1)+MCPTG(M)*PTG(1,M)
                MCP(M2)=MCP(M2)+MCPTG(M)*PTG(2,M)
                MCP(M3)=MCP(M3)+MCPTG(M)*PTG(3,M)
#include "lockoff.inc"
              END IF
C
            ELSE
              NSKYML=NSKYML+1
              MSH3SKY(NSKYML)=M
            END IF
C
C         map fields to the son
            NG1 =SH3TREE(4,M)
            IPARG(8,NG1)=0
          END DO
C
          CALL ADMMAP3(N, IXTG, X, IPARG,ELBUF_TAB,
     .             IGEO, IPM ,SH3TREE )
C
          IF(IPARIT==0)THEN
            IF(ISTATCND==0)THEN
#include "lockon.inc"
              MS(N1)=MAX(ZERO,MS(N1)-MSTG(N)*PTG(1,N))
              MS(N2)=MAX(ZERO,MS(N2)-MSTG(N)*PTG(2,N))
              MS(N3)=MAX(ZERO,MS(N3)-MSTG(N)*PTG(3,N))
              IN(N1)=MAX(ZERO,IN(N1)-INTG(N)*PTG(1,N))
              IN(N2)=MAX(ZERO,IN(N2)-INTG(N)*PTG(2,N))
              IN(N3)=MAX(ZERO,IN(N3)-INTG(N)*PTG(3,N))
#include "lockoff.inc"
            ELSE
#include "lockon.inc"
              MSBIG=MSTG(N)
              MSCND(N1)=MAX(ZERO,MSCND(N1)-MSBIG)
              MSCND(N2)=MAX(ZERO,MSCND(N2)-MSBIG)
              MSCND(N3)=MAX(ZERO,MSCND(N3)-MSBIG)
              INBIG=INTG(N)
              INCND(N1)=MAX(ZERO,INCND(N1)-INBIG)
              INCND(N2)=MAX(ZERO,INCND(N2)-INBIG)
              INCND(N3)=MAX(ZERO,INCND(N3)-INBIG)
#include "lockoff.inc"
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
          ELSE
            NSKYML=NSKYML+1
            MSH3SKY(NSKYML)=-N
          END IF
C
C       goes to sleep
          LSH3ACT(NN) =0
          SH3TREE(3,N)=-(SH3TREE(3,N)+1)

        END DO
C
        CALL MY_BARRIER
C
        IF(IPARIT/=0 .AND. ITASK==0 .AND. NSKYMSH4 > 0)THEN
          DO I = 1, NSKYMSH4
            ITRI(I) = IXC(NIXC,ABS(MSH4SKY(I)))
          ENDDO
          CALL MY_ORDERS(0,WORK,ITRI,INDEX,NSKYMSH4,1)
          IF(ISTATCND==0)THEN
            DO J = 1, NSKYMSH4
              N=MSH4SKY(INDEX(J))
              IF(N < 0)THEN
                N=-N
                DO K=1,4
                  I = IXC(K+1,N)
                  MS(I) = MAX(ZERO , MS(I) - MSC(N))
                  IN(I) = MAX(ZERO , IN(I) - INC(N))
                END DO
              ELSE
                DO K=1,4
                  I = IXC(K+1,N)
                  MS(I) = MS(I) + MSC(N)
                  IN(I) = IN(I) + INC(N)
                END DO
              END IF
            END DO
          ELSE
            DO J = 1, NSKYMSH4
              N=MSH4SKY(INDEX(J))
              IF(N < 0)THEN
                N=-N
                MSBIG=MSC(N)
                INBIG=INC(N)
                DO K=1,4
                  I = IXC(K+1,N)
                  MSCND(I) = MAX(ZERO , MSCND(I) - MSBIG)
                  INCND(I) = MAX(ZERO , INCND(I) - INBIG)
                END DO
              ELSE
                MSBIG=MSC(N)
                INBIG=INC(N)
                DO K=1,4
                  I = IXC(K+1,N)
                  MSCND(I) = MSCND(I) + MSBIG
                  INCND(I) = INCND(I) + INBIG
                END DO
              END IF
            END DO
          END IF
C
          IF(ITHERM_FE > 0)THEN
            DO J = 1, NSKYMSH4
              N=MSH4SKY(INDEX(J))
              IF(N < 0)THEN
                N=-N
                DO K=1,4
                  I = IXC(K+1,N)
                  MCP(I) = MAX(ZERO , MCP(I) - MCPC(N))
                END DO
              ELSE
                DO K=1,4
                  I = IXC(K+1,N)
                  MCP(I) = MCP(I) + MCPC(N)
                END DO
              END IF
            END DO
          END IF
C
        END IF
C
        IF(IPARIT/=0 .AND. ITASK==0 .AND. NSKYMSH3 > 0)THEN
          DO I = 1, NSKYMSH3
            ITRI(I) = IXTG(NIXTG,ABS(MSH3SKY(I)))
          ENDDO
          CALL MY_ORDERS(0,WORK,ITRI,INDEX,NSKYMSH3,1)
          IF(ISTATCND==0)THEN
            DO J = 1, NSKYMSH3
              N=MSH3SKY(INDEX(J))
              IF(N < 0)THEN
                N=-N
                DO K=1,3
                  I = IXTG(K+1,N)
                  MS(I) = MAX(ZERO , MS(I) - MSTG(N)*PTG(K,N))
                  IN(I) = MAX(ZERO , IN(I) - INTG(N)*PTG(K,N))
                END DO
              ELSE
                DO K=1,3
                  I = IXTG(K+1,N)
                  MS(I) = MS(I) + MSTG(N)*PTG(K,N)
                  IN(I) = IN(I) + INTG(N)*PTG(K,N)
                END DO
              END IF
            END DO
          ELSE
            DO J = 1, NSKYMSH3
              N=MSH3SKY(INDEX(J))
              IF(N < 0)THEN
                N=-N
                MSBIG=MSTG(N)
                INBIG=INTG(N)
                DO K=1,3
                  I = IXTG(K+1,N)
                  MSCND(I) = MAX(ZERO , MSCND(I) - MSBIG)
                  INCND(I) = MAX(ZERO , INCND(I) - INBIG)
                END DO
              ELSE
                MSBIG=MSTG(N)
                INBIG=INTG(N)
                DO K=1,3
                  I = IXTG(K+1,N)
                  MSCND(I) = MSCND(I) + MSBIG
                  INCND(I) = INCND(I) + INBIG
                END DO
              END IF
            END DO
          END IF
C
          IF(ITHERM_FE > 0)THEN
            DO J = 1, NSKYMSH3
              N=MSH3SKY(INDEX(J))
              IF(N < 0)THEN
                N=-N
                DO K=1,3
                  I = IXTG(K+1,N)
                  MCP(I) = MAX(ZERO , MCP(I) - MCPTG(N)*PTG(K,N))
                END DO
              ELSE
                DO K=1,3
                  I = IXTG(K+1,N)
                  MCP(I) = MCP(I) + MCPTG(N)*PTG(K,N)
                END DO
              END IF
            END DO
          END IF
C
        END IF
C
C     compactage de LSH4ACT
        IF(IADMESH==1)THEN
          IF(ITASK==0)THEN
            NTMP   =NSH4ACT
            NSH4ACT=0
            DO NN=1,NTMP
              N=LSH4ACT(NN)
              IF(N/=0)THEN
                NSH4ACT=NSH4ACT+1
                LSH4ACT(NSH4ACT)=N
              END IF
            END DO

            NTMP   =NSH3ACT
            NSH3ACT=0
            DO NN=1,NTMP
              N=LSH3ACT(NN)
              IF(N/=0)THEN
                NSH3ACT=NSH3ACT+1
                LSH3ACT(NSH3ACT)=N
              END IF
            END DO
          END IF
        END IF
C
C----6---------------------------------------------------------------7---------8
        RETURN
      END


