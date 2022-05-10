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
Chd|  QSURFTAG                      source/groups/qsurftag.F      
Chd|-- called by -----------
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|-- calls ---------------
Chd|        QSURF                         source/groups/qsurftag.F      
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE QSURFTAG(IXQ   ,IPARTQ   ,NSEG0   ,IGRSURF ,TAGBUF,
     .                    NSEG  ,KNOD2ELQ ,NOD2ELQ ,IEXT    ,FLAG  ,
     .                    X     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXQ(NIXQ,*),IPARTQ(*),TAGBUF(*),
     .        KNOD2ELQ(*),NOD2ELQ(*)
      INTEGER IEXT,FLAG,NSEG0
      INTEGER, INTENT(INOUT) :: NSEG
      my_real
     .   X(3,*)
      TYPE (SURF_) :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JQ,JJ,K,NQQ,N1,N2,ISEG,KK,KQ,N,L1,L2,L,TRUEAXE,NQQ1,NQQ2
      INTEGER NODTAG(4),LINES(2,4),NQ(4)
      DATA LINES/1,2,
     .           2,3,
     .           3,4,
     .           4,1/
      my_real
     .   Y1,Z1,Y2,Z2,Y3,Z3,Y4,Z4,
     .   YG,ZG,PVECT,PSCA,DY,DZ,NY,NZ   
C=======================================================================
C
      IF(IEXT==1)THEN
C
C       External surface only.
        DO JQ=1,NUMELQ
          IF (TAGBUF(IPARTQ(JQ))==0) CYCLE  !case of tagged parts
            NODTAG(1:4)=1
c            NQ(N) = IXQ(JJ-1,JQ)
            DO L=1,4
              NQ(L) = IXQ(L+1,JQ)
              L1 = LINES(1,L)
              L2 = LINES(2,L) 
              NQQ1 = IXQ(L1+1,JQ)         
              NQQ2 = IXQ(L2+1,JQ)
              DO K=KNOD2ELQ(NQQ1)+1,KNOD2ELQ(NQQ1+1)            
                KQ=NOD2ELQ(K)
                IF(KQ==JQ .OR. KQ > NUMELQ)CYCLE
                IF (TAGBUF(IPARTQ(KQ))==0)CYCLE  
                DO KK=1,4
                  IF(IXQ(LINES(1,KK)+1,KQ)==NQQ1.AND.IXQ(LINES(2,KK)+1,KQ)==NQQ2) THEN
                     NODTAG(L)=0
                  ELSEIF(IXQ(LINES(1,KK)+1,KQ)==NQQ2.AND.IXQ(LINES(2,KK)+1,KQ)==NQQ1) THEN
                     NODTAG(L)=0
                  ENDIF
                ENDDO
              ENDDO
            ENDDO
C           looks for the center of elements to check normal ext
          Y1 = X(2,NQ(1))
          Z1 = X(3,NQ(1))
c
          Y2 = X(2,NQ(2))
          Z2 = X(3,NQ(2))
c
          Y3 = X(2,NQ(3))
          Z3 = X(3,NQ(3))
c
          Y4 = X(2,NQ(4))
          Z4 = X(3,NQ(4))
c        
          YG = (Y1+Y2+Y3+Y4)/QUATRE
          ZG = (Z1+Z2+Z3+Z4)/QUATRE 

          DO L=1,4
             L1 = LINES(1,L)
             L2 = LINES(2,L)      
             TRUEAXE= 1
             N1 = NQ(L1)
             N2 = NQ(L2)
             IF(N2D==1.AND.X(2,N1).LE.EM10.AND.X(2,N2).LE.EM10) THEN ! Case Axi ommit nodes of revolution axe z ( y=0)
                TRUEAXE= 0
             ENDIF

             IF(TRUEAXE==1)THEN
               IF(NODTAG(L)==1) THEN ! nodes of external lines      
                 NSEG=NSEG+1  
                 IF (FLAG == 1) THEN
C           normal computation
                  DY = X(2,N2)-X(2,N1)
                  DZ = X(3,N2)-X(3,N1)
                  NY = -DZ
                  NZ = DY
                  PVECT = DY*DZ
                  IF(PVECT.LT.ZERO) THEN
                    NY = DZ
                    NZ = -DY
                  ENDIF
C           check external normal 
                  PSCA = NY*(Y1-YG)+NZ*(Z1-ZG)
                  ISEG = NSEG
                  IF(PSCA.LE.ZERO) THEN
                    CALL QSURF(N1  ,N2  ,JQ  ,NSEG0  ,ISEG,
     .                         IGRSURF%ELTYP,IGRSURF%ELEM,IGRSURF%NODES)
                  ELSE
                    CALL QSURF(N2  ,N1  ,JQ  ,NSEG0  ,ISEG,
     .                         IGRSURF%ELTYP,IGRSURF%ELEM,IGRSURF%NODES)
                  ENDIF

                 ENDIF

               ENDIF

             ENDIF

           ENDDO


        ENDDO
      ENDIF
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  QSURF                         source/groups/qsurftag.F      
Chd|-- called by -----------
Chd|        QSURFTAG                      source/groups/qsurftag.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE QSURF(N1        ,N2        ,JQ       ,NSEG0 ,ISEG,
     .                 SURF_ELTYP,SURF_ELEM,SURF_NODES)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N1,N2,JQ,NSEG0,ISEG
      INTEGER SURF_ELTYP(NSEG0),SURF_ELEM(NSEG0),SURF_NODES(NSEG0,4)
C-----------------------------------------------
      SURF_NODES(ISEG,1) = N1
      SURF_NODES(ISEG,2) = N2
      SURF_NODES(ISEG,3) = 0
      SURF_NODES(ISEG,4) = 0
      SURF_ELTYP(ISEG) = 2
      SURF_ELEM(ISEG)  = JQ
!---
      RETURN
      END
