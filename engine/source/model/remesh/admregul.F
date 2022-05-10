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
Chd|  ADMREGUL                      source/model/remesh/admregul.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ADMMAP3                       source/model/remesh/admmap3.F 
Chd|        ADMMAP4                       source/model/remesh/admmap4.F 
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        SYNC_DATA                     source/system/machine.F       
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        REMESH_MOD                    share/modules/remesh_mod.F    
Chd|====================================================================
      SUBROUTINE ADMREGUL(IXC  ,IPARTC  ,IXTG ,IPARTTG,IPART,
     .                    ITASK,IPARG   ,X    ,MS     ,IN   ,
     .                    ELBUF_TAB,NODFT  ,NODLT ,IGEO   ,IPM  ,
     .                    SH4TREE,MSC  ,INC   ,SH3TREE,MSTG ,
     .                    INTG ,PTG    ,MSCND ,INCND  ,PM   ,
     .                    MCP  ,MCPC   ,MCPTG )
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
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "remesh_c.inc"
#include      "task_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*),IPARTC(*),IXTG(NIXTG,*),IPARTTG(*),
     .        IPART(LIPART1,*),ITASK,IPARG(NPARG,*),
     .        NODFT, NODLT, IGEO(NPROPGI,*), IPM(NPROPMI,*),
     .        SH4TREE(KSH4TREE,*),SH3TREE(KSH3TREE,*)
      my_real
     .        X(3,*),MS(*),IN(*),MSC(*), INC(*),
     .        MSTG(*), INTG(*), PTG(3,*), MSCND(*), INCND(*),
     .        PM(NPROPM,*), MCP(*), MCPC(*), MCPTG(*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER SH4FT, SH4LT, SH3FT, SH3LT
      INTEGER NN,N,IB,M,N1,N2,N3,N4
      INTEGER I,J,K,NG1,
     .   NA1, NA2, NA3, NA4, NA5, NA6, NA7, NA8, NA9, NA10, NA11,
     .   NA12, NA13,NA14,NA15,NA16,NA17,NA18,NA19,NA20,NA21,NA22,
     .   NA17A,NA17B,NB17A,NB17B,LLL,
     .   MATLY,MY_NUVAR,MY_NUVARR,NUVAR,NUVARR,II,IVAR, 
     .   NA16A,NB16A,MPT,NPTM,NAM_S,NBM_S,IG,IH,IS,
     .   PTF,PTM,PTE,PTP,PTS,QTF,QTM,QTE,QTP,QTS,NPG
      INTEGER LEVEL,NTMP,LEV,P,NI,MYLEV,IP,
     .        KDIVIDE4(NUMELC),KDIVIDE3(NUMELTG)
      INTEGER NSKYML, WORK(70000),
     .        ITRI(MAX(NUMELC,NUMELTG)), INDEX(2*MAX(NUMELC,NUMELTG))
      my_real
     .        MSBIG, INBIG, MCPM, MCPN
C-----------------------------------------------
 10   CONTINUE 

      IF(ITASK.EQ.0) THEN

        ILEVNOD=0

        DO NN=1,NSH4ACT
          N=LSH4ACT(NN)
          LEV=SH4TREE(3,N)
          DO I=1,4
            NI=IXC(I+1,N)-1
            ILEVNOD(NI)=MAX(ILEVNOD(NI),LEV)
          END DO
        END DO

        DO NN=1,NSH3ACT
          N=LSH3ACT(NN)
          LEV=SH3TREE(3,N)
          DO I=1,3
            NI=IXTG(I+1,N)-1
            ILEVNOD(NI)=MAX(ILEVNOD(NI),LEV)
          END DO
        END DO

      END IF

      KADMRULE=0
C
      CALL MY_BARRIER
C
      IF(NSH4ACT/=0) KDIVIDE4=0

      SH4FT = 1+ITASK*NSH4ACT/ NTHREAD
      SH4LT = (ITASK+1)*NSH4ACT/NTHREAD
C
      DO NN=SH4FT,SH4LT

        N =LSH4ACT(NN)

        LEVEL=SH4TREE(3,N)
        IF( LEVEL >= LEVELMAX-1 ) CYCLE

        DO I=1,4
          NI=IXC(I+1,N)-1
          LEV=ILEVNOD(NI)
          IF(LEV-LEVEL > 1) THEN
             KDIVIDE4(N)=1
             KADMRULE=1
             GO TO 100
          END IF
        END DO

 100    CONTINUE
        CALL SYNC_DATA(KDIVIDE4(N))

      END DO
C
      IF(NSH3ACT/=0) KDIVIDE3=0

      SH3FT = 1+ITASK*NSH3ACT/ NTHREAD
      SH3LT = (ITASK+1)*NSH3ACT/NTHREAD
C
      DO NN=SH3FT,SH3LT

        N =LSH3ACT(NN)

        LEVEL=SH3TREE(3,N)
        IF( LEVEL >= LEVELMAX-1 ) CYCLE

        DO I=1,3
          NI=IXTG(I+1,N)-1
          LEV=ILEVNOD(NI)
          IF(LEV-LEVEL > 1) THEN
             KDIVIDE3(N)=1
             KADMRULE=1
             GO TO 200
          END IF
        END DO
 200    CONTINUE
        CALL SYNC_DATA(KDIVIDE3(N))

      END DO
C
      NSKYMSH4=0
      NSKYMSH3=0
C
      CALL MY_BARRIER
C
      IF(KADMRULE==0) RETURN
      DO NN=SH4FT,SH4LT
        N =LSH4ACT(NN)

        IF( KDIVIDE4(N) == 0 ) CYCLE

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
          N1 = IXC(2,M)
          N2 = IXC(3,M)
          N3 = IXC(4,M)
          N4 = IXC(5,M)
C
C         wake up the son
          SH4TREE(3,M)=-SH4TREE(3,M)-1
#include "lockon.inc"
          NSH4ACT=NSH4ACT+1
          LSH4ACT(NSH4ACT)=M
#include "lockoff.inc"
C
Clm       1/4 of the element mass has been stored
          IF(IPARIT==0)THEN
           IF(ISTATCND==0)THEN
#include "lockon.inc"
            MS(N1)=MS(N1)+MSC(M)
            MS(N2)=MS(N2)+MSC(M)
            MS(N3)=MS(N3)+MSC(M)
            MS(N4)=MS(N4)+MSC(M)
            IN(N1)=IN(N1)+INC(M)
            IN(N2)=IN(N2)+INC(M)
            IN(N3)=IN(N3)+INC(M)
            IN(N4)=IN(N4)+INC(M)
#include "lockoff.inc"
           ELSE
#include "lockon.inc"
             MSBIG=MSC(M)
             MSCND(N1)=MSCND(N1)+MSBIG
             MSCND(N2)=MSCND(N2)+MSBIG
             MSCND(N3)=MSCND(N3)+MSBIG
             MSCND(N4)=MSCND(N4)+MSBIG
             INBIG=INC(M)
             INCND(N1)=INCND(N1)+INBIG
             INCND(N2)=INCND(N2)+INBIG
             INCND(N3)=INCND(N3)+INBIG
             INCND(N4)=INCND(N4)+INBIG
#include "lockoff.inc"
           END IF
C
           IF(ITHERM_FE > 0)THEN 
#include "lockon.inc"
             MCPM=MCPC(M)
             MCP(N1)=MCP(N1)+MCPM
             MCP(N2)=MCP(N2)+MCPM
             MCP(N3)=MCP(N3)+MCPM
             MCP(N4)=MCP(N4)+MCPM
#include "lockoff.inc"
           END IF
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
     .  	 IGEO, IPM ,SH4TREE)
C
        N1 = IXC(2,N)
        N2 = IXC(3,N)
        N3 = IXC(4,N)
        N4 = IXC(5,N)
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
        ELSE
            NSKYML=NSKYML+1
            MSH4SKY(NSKYML)=-N
        END IF
C
C       goes to sleep
        LSH4ACT(NN) =0
        SH4TREE(3,N)=-(SH4TREE(3,N)+1)

      END DO


      DO NN=SH3FT,SH3LT
        N =LSH3ACT(NN)

        IF( KDIVIDE3(N) == 0 ) CYCLE

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
          N1 = IXTG(2,M)
          N2 = IXTG(3,M)
          N3 = IXTG(4,M)
C
C         wake up the son
          SH3TREE(3,M)=-SH3TREE(3,M)-1
#include "lockon.inc"
          NSH3ACT=NSH3ACT+1
          LSH3ACT(NSH3ACT)=M
#include "lockoff.inc"
C
Clm       1/4 of the element mass has been stored
          IF(IPARIT==0)THEN
           IF(ISTATCND==0)THEN
#include "lockon.inc"
            MS(N1)=MS(N1)+MSTG(M)*PTG(1,M)
            MS(N2)=MS(N2)+MSTG(M)*PTG(2,M)
            MS(N3)=MS(N3)+MSTG(M)*PTG(3,M)
            IN(N1)=IN(N1)+INTG(M)*PTG(1,M)
            IN(N2)=IN(N2)+INTG(M)*PTG(2,M)
            IN(N3)=IN(N3)+INTG(M)*PTG(3,M)
#include "lockoff.inc"
           ELSE
#include "lockon.inc"
             MSBIG=MSTG(M)
             MSCND(N1)=MSCND(N1)+MSBIG
             MSCND(N2)=MSCND(N2)+MSBIG
             MSCND(N3)=MSCND(N3)+MSBIG
             INBIG=INTG(M)
             INCND(N1)=INCND(N1)+INBIG
             INCND(N2)=INCND(N2)+INBIG
             INCND(N3)=INCND(N3)+INBIG
#include "lockoff.inc"
           END IF
C
           IF(ITHERM_FE > 0)THEN 
#include "lockon.inc"
             MCP(N1)=MCP(N1)+MCPTG(M)*PTG(1,M)
             MCP(N2)=MCP(N2)+MCPTG(M)*PTG(2,M)
             MCP(N3)=MCP(N3)+MCPTG(M)*PTG(3,M)
#include "lockoff.inc"
           END IF
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
        CALL ADMMAP3(N, IXTG, X, IPARG, ELBUF_TAB,
     .  	 IGEO, IPM , SH3TREE)
C
        N1 = IXTG(2,N)
        N2 = IXTG(3,N)
        N3 = IXTG(4,N)
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
C--
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
C--
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
      IF(ITASK==0)THEN
        NTMP   =NSH4ACT
        NSH4ACT=0
        DO NN=1,NTMP
          N=LSH4ACT(NN)
          IF(N.NE.0)THEN
            NSH4ACT=NSH4ACT+1
            LSH4ACT(NSH4ACT)=N
          END IF
        END DO 

      END IF
C
C     compactage de LSH3ACT
      IF(ITASK==0)THEN
        NTMP   =NSH3ACT
        NSH3ACT=0
        DO NN=1,NTMP
          N=LSH3ACT(NN)
          IF(N.NE.0)THEN
            NSH3ACT=NSH3ACT+1
            LSH3ACT(NSH3ACT)=N
          END IF
        END DO 
      END IF

      GO TO 10
C----6---------------------------------------------------------------7---------8
      RETURN
      END     
