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
Chd|  SPHTRI                        source/elements/sph/sphtri.F  
Chd|-- called by -----------
Chd|        SPHPREP                       source/elements/sph/sphprep.F 
Chd|-- calls ---------------
Chd|        SPBUC3                        source/elements/sph/spbuc3.F  
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPHTRI(X       ,SPBUF   ,KXSP  ,IXSP  ,NOD2SP,
     2                  IREDUCE ,WSP2SORT,BMINMA,NSP2SORTF,NSP2SORTL,
     3                  ITASK   ,KREDUCE ,LGAUGE  ,GAUGE   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "bucki_c.inc"
#include      "buckr_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*),IXSP(KVOISPH,*),NOD2SP(*), WSP2SORT(*),
     .        IREDUCE,NSP2SORTF,NSP2SORTL,ITASK, KREDUCE(*),
     .        LGAUGE(3,*)
C     REAL
      my_real
     .        X(3,*),SPBUF(NSPBUF,*), BMINMA(12), GAUGE(LLGAUGE,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NSN,IG,JG
      INTEGER
     .   N,INOD,JNOD,J,NVOIS,M,NCAND,NVOIS1,NVOIS2,
     .   KL, K, L, JK, JL, IERROR, NS, SS,
     .   MWA(15*(NUMSPH+NSPHR)),
     .   JVOIS(NUMSPH+NSPHR), JSTOR(NUMSPH+NSPHR),
     .   JPERM(NUMSPH+NSPHR)
      my_real
     .       CMS2,DMS,DMS2,
     .       XI,YI,ZI,XJ,YJ,ZJ,D,DK,DL,
     .       DVOIS(NUMSPH+NSPHR)    
C-----------------------------------------------
         NSN=0
         DO NS=1,NSP2SORT
           N=WSP2SORT(NS)
           NSN=NSN+1
           MWA(NSN) =N
           KXSP(5,N)=0
         END DO
C
         DO IG=1,NBGAUGE
           KXSP(5,NUMSPH+IG)=0
         END DO
C
         DO NS = 1, NSPHR                ! candidats remote SPMD
           NSN=NSN+1
           MWA(NSN)=NUMSPH+NS
         END DO
C--------
         IF (NSP2SORT/=0) CALL SPBUC3(
     1              X     ,KXSP   ,IXSP  ,NOD2SP,NSP2SORT,
     2              SPBUF ,MWA    ,JVOIS ,JSTOR ,JPERM  ,
     3              DVOIS ,IREDUCE,BMINMA,NSPHR ,NSP2SORTF,
     4              NSP2SORTL,ITASK,KREDUCE,LGAUGE ,GAUGE ) 
C
         DO NS=NSP2SORTF,NSP2SORTL
          N=WSP2SORT(NS)
          INOD=KXSP(3,N)
          SPBUF(5,N)=X(1,INOD)
          SPBUF(6,N)=X(2,INOD)
          SPBUF(7,N)=X(3,INOD)
          SPBUF(8,N)=SPBUF(1,N)
         ENDDO
C
         
         DO N=ITASK+1,NBGAUGE,NTHREAD
           IF(LGAUGE(1,N) <= -(NUMELS+1))THEN
             GAUGE(6,N)=GAUGE(2,N)
             GAUGE(7,N)=GAUGE(3,N)
             GAUGE(8,N)=GAUGE(4,N)
           END IF
         END DO
C-----------------------------------------------
      RETURN
      END
