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
Chd|  ANI_SEGQUADFR1                source/output/anim/ani_segquadfr.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ANI_SEGQUADFR1(IXQ ,SEGTAG ,KNOD2ELQ ,NOD2ELQ ,X ,NSEG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER  
     .   NSEG,
     .   IXQ(NIXQ,*),SEGTAG(4,*),KNOD2ELQ(*),NOD2ELQ(*)
      my_real
     .   X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JQ,JJ,K,NQQ,N1,N2,ISEG,KK,KQ,N,L1,L2,L, TRUEAXE, NQQ1,NQQ2
      INTEGER NODTAG(4),LINES(2,4),NQ(4)
      DATA LINES/1,2,
     .           2,3,
     .           3,4,
     .           4,1/
C     REAL
C-----------------------------------------------
C
        DO JQ=1,NUMELQ
           NODTAG(1:4)=1
           DO L=1,4
              NQ(L) = IXQ(L+1,JQ)
              L1 = LINES(1,L)
              L2 = LINES(2,L) 
              NQQ1 = IXQ(L1+1,JQ)         
              NQQ2 = IXQ(L2+1,JQ)
              DO K=KNOD2ELQ(NQQ1)+1,KNOD2ELQ(NQQ1+1)            
                KQ=NOD2ELQ(K)
                IF(KQ==JQ .OR. KQ > NUMELQ)CYCLE
                DO KK=1,4
                  IF(IXQ(LINES(1,KK)+1,KQ)==NQQ1.AND.IXQ(LINES(2,KK)+1,KQ)==NQQ2) THEN
                     NODTAG(L)=0
                  ELSEIF(IXQ(LINES(1,KK)+1,KQ)==NQQ2.AND.IXQ(LINES(2,KK)+1,KQ)==NQQ1) THEN
                     NODTAG(L)=0
                  ENDIF
                ENDDO
             ENDDO
          ENDDO
C
          DO L=1,4
             L1 = LINES(1,L)
             L2 = LINES(2,L)
             TRUEAXE= 1
             N1 = NQ(L1)
             N2 = NQ(L2)
             IF(N2D==1.AND.X(2,N1)<=EM10.AND.X(2,N2)<=EM10) THEN ! Case Axi omit nodes of revolution axe z ( y=0)
                TRUEAXE= 0
             ENDIF

             IF(TRUEAXE==1)THEN
               IF(NODTAG(L)==1) THEN ! nodes of external lines
                  NSEG=NSEG+1  
                  SEGTAG(L,JQ) = 1
               ENDIF
             ENDIF
         ENDDO
      ENDDO
C     
      RETURN
      END
Chd|====================================================================
Chd|  ANI_SEGQUADFR2                source/output/anim/ani_segquadfr.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ANI_SEGQUADFR2(SEGTAG  ,SEGQUADFR )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  
     .   SEGTAG(4,*), SEGQUADFR(2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,JJ,LL
C     REAL
C-----------------------------------------------
C
      NSEGQUADFR=0
      DO N=1,NUMELQ
        DO JJ=1,4
          IF(SEGTAG(JJ,N)==1)THEN
            NSEGQUADFR=NSEGQUADFR+1
            SEGQUADFR(1,NSEGQUADFR)=N
            SEGQUADFR(2,NSEGQUADFR)=JJ
          END IF
        END DO
      END DO    
      RETURN
      END
