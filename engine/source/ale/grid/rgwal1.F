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
Chd|  RGWAL1                        source/ale/grid/rgwal1.F      
Chd|-- called by -----------
Chd|        ALEWDX                        source/ale/grid/alewdx.F      
Chd|-- calls ---------------
Chd|        INTERP                        source/tools/curve/interp.F   
Chd|        RGWATH                        source/interfaces/int09/rgwath.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE RGWAL1(X       ,A	,V	  ,W	  ,RWBUF   ,
     2                  LPRW    ,NPRW	, 
     3                  ICODT   ,ISKEW  ,SKEW	  ,MS	  ,FSAV    ,
     4                  IXS     ,IXQ	,ELBUF_TAB,IPARG  ,
     5                  PM      ,NALE	,TF	  ,NPF	  ,WEIGHT  ,
     6                  IAD_ELEM,FR_ELEM,FR_WALL )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "secur_c.inc"
#include      "units_c.inc"
#include      "scr05_c.inc"
#include      "parit_c.inc"
#include      "com08_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LPRW(*), NPRW(*), IXS(NIXS,NUMELS),
     .        IXQ(NIXQ,NUMELQ),IPARG(NPARG,NGROUP),
     .        ICODT(*), ISKEW(*), NALE(*),NPF(*),
     .        WEIGHT(*),IAD_ELEM(*), FR_ELEM(*),
     .        FR_WALL(NSPMD+2,*)
      my_real X(3,NUMNOD), A(3,NUMNOD), V(3,NUMNOD), W(3,NUMNOD), RWBUF(NRWLP,*), MS(*),
     .        FSAV(NTHVKI,*), SKEW(LSKEW,*),
     .        PM(NPROPM,NUMMAT), TF(*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, N, N2, N3, N4, ITYP, NE, NSN, IFUNC, NPOINT, NTAG(NUMNOD)
      my_real TEMP,TIME,BID,E(NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------

C-----------------------
C         RIGID WALL ALE-TH (OBSOLETE)
C-----------------------
      IF(NRWALL > 0)THEN
          K=1
          DO N=1,NRWALL
            N2=N +NRWALL
            N3=N2+NRWALL
            N4=N3+NRWALL
            ITYP=NPRW(N4)
            IF(ITYP == -1)THEN
              NSN =NPRW(N)
              NE  =NINT(RWBUF(8,N))
              IFUNC=NINT(RWBUF(9,N))
              TIME = TT * RWBUF(16,N)
              NPOINT=(NPF(IFUNC+1)-NPF(IFUNC))/2
              CALL INTERP(TF(NPF(IFUNC)),TIME,NPOINT,TEMP,BID)
              TEMP = TEMP * RWBUF(10,N)
              CALL RGWATH(
     1          X       ,V	     ,W       ,RWBUF(1,N),LPRW(K)    ,
     2          NSN     ,NPRW(N3)    ,MS      ,FSAV(1,N) ,IXS	     ,
     3          IXQ     ,ELBUF_TAB   ,IPARG   ,PM	 ,
     4          NTAG    ,LPRW(K+NSN) ,NE      ,TEMP	 ,RWBUF(11,N),
     5          E       ,A	     ,NPRW(N2),WEIGHT	 ,IAD_ELEM   ,
     6          FR_ELEM ,FR_WALL(1,N))
              K=K+NINT(RWBUF(8,N))
            ENDIF
            K=K+NPRW(N)
          ENDDO!next N
      ENDIF
      RETURN
      END
