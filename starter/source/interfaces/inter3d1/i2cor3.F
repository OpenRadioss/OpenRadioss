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
Chd|  I2COR3                        source/interfaces/inter3d1/i2cor3.F
Chd|-- called by -----------
Chd|        I2BUC1                        source/interfaces/inter3d1/i2buc1.F
Chd|        I2TRI                         source/interfaces/inter3d1/i2tri.F
Chd|-- calls ---------------
Chd|        INCOQ3                        source/interfaces/inter3d1/incoq3.F
Chd|        INSOL3                        source/interfaces/inter3d1/insol3.F
Chd|        VOLINT                        source/interfaces/inter3d1/volint.F
Chd|====================================================================
      SUBROUTINE I2COR3(X     ,IRECT ,NSV   ,CAND_E  ,CAND_N,
     2                  STF   ,STFN  ,GAPV  ,IGAP    ,GAP   ,
     3                  GAP_S ,GAP_M ,ISTF  ,NINT    ,IXC   , 
     4                  IXTG  ,THK_PART,IPARTC,GEO   , NOINT,
     5                  IXS   ,IXS10 ,PM    ,THK     ,KNOD2ELS,
     6                  KNOD2ELC,KNOD2ELTG,NOD2ELS,NOD2ELC,NOD2ELTG,
     7                  IGNORE,IXS16 ,IXS20 ,IPARTTG,IGEO,DSEARCH ,
     8                  PM_STACK , IWORKSH  ,IX1     ,IX2   ,
     5                  IX3    ,IX4    ,NSVG,X1      ,X2    ,
     6                  X3     ,X4     ,Y1  ,Y2      ,Y3    ,
     7                  Y4     ,Z1     ,Z2  ,Z3      ,Z4    ,
     8                  XI     ,YI     ,ZI  ,STIF)
C============================================================================
C  cette routine est appelee par : I7BUC1(/inter3d1/i7buc1.F)
C                                  I7TRI(/inter3d1/i7tri.F)
C                                  I2TRI(/inter3d1/i2tri.F)
C                                  I2BUC1(/inter3d1/i2buc1.F)
C                                  ININT3(/inter3d1/inint3.F)
C----------------------------------------------------------------------------
C  cette routine appelle : -
C============================================================================
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGAP, IRECT(4,*), NSV(*), CAND_E(*), CAND_N(*),ISTF,
     .        IXC(NIXC,*),IXTG(NIXTG,*),IPARTC(*),NOINT,
     .        IXS(NIXS,*), IXS10(*),KNOD2ELS(*), 
     .        KNOD2ELC(*),KNOD2ELTG(*), NOD2ELS(*), NOD2ELC(*), 
     .        NOD2ELTG(*),NINT,IGNORE,
     .        IXS16(*), IXS20(*),IPARTTG(*),IGEO(NPROPGI,*),
     .        IWORKSH(*)
C     REAL
      my_real
     .   GAP, X(3,*), STF(*), STFN(*), GAPV(*), GAP_S(*), GAP_M(*),
     .   GEO(NPROPG,*),THK(*),THK_PART(*),PM(*),DSEARCH,PM_STACK(*)
      INTEGER, DIMENSION(MVSIZ), INTENT(INOUT) :: IX1,IX2,IX3,IX4,NSVG
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: X1,X2,X3,X4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: Y1,Y2,Y3,Y4
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: Z1,Z2,Z3,Z4
      my_real, DIMENSION(MVSIZ), INTENT(OUT) :: XI,YI,ZI
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) :: STIF
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr08_c.inc"
#include      "vect07_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL, L, NN, IG,NODE1,NODE2,NODE3,NODE4,II,IAD,JJ,JJJ,
     .        IEL,MG,IP,NELS,NELC,NELTG
C     REAL
      my_real
     .     THKSECND,THKMAIN,DD,DX1,DY1,DZ1,DX3,DY3,DZ3,VOL,AREA
C-----------------------------------------------
C
!      EQUIVALENCE (IX1,IV(1,1)) ,(IX2,IV(1,2)) ,(IX3,IV(1,3))
!      EQUIVALENCE (IX4,IV(1,4)) ,(NSVG,IV(1,5))
C
!      EQUIVALENCE (X1,BV(1,1)) ,(Y1,BV(1,2)) ,(Z1,BV(1,3))
!      EQUIVALENCE (X2,BV(1,4)) ,(Y2,BV(1,5)) ,(Z2,BV(1,6))
!      EQUIVALENCE (X3,BV(1,7)) ,(Y3,BV(1,8)) ,(Z3,BV(1,9))
!      EQUIVALENCE (X4,BV(1,10)),(Y4,BV(1,11)),(Z4,BV(1,12))
!      EQUIVALENCE (XI,BV(1,13)),(YI,BV(1,14)),(ZI,BV(1,15))
!      EQUIVALENCE (STIF,BV(1,30))
C
      DO I=LFT,LLT
       IG = NSV(CAND_N(I))
       NSVG(I) = IG
       XI(I) = X(1,IG)
       YI(I) = X(2,IG)
       ZI(I) = X(3,IG)
      ENDDO
C
      IF(IGAP.EQ.0 .AND. IGNORE .LE. 1)THEN
        DO I=LFT,LLT
         GAPV(I) = GAP
        ENDDO 
      ELSEIF((IGNORE == 2 .OR. IGNORE == 3) .AND. DSEARCH .NE. ZERO)THEN
        DO I=LFT,LLT
          GAPV(I) = GAP
        ENDDO
      ELSEIF(IGNORE >= 2)THEN
        DO I=LFT,LLT
          THKSECND = ZERO
          THKMAIN = ZERO
          II=CAND_N(I)
          DO IAD = KNOD2ELC(NSVG(I))+1,KNOD2ELC(NSVG(I)+1)
            IEL  = NOD2ELC(IAD)
            MG=IXC(6,IEL)
	    IP = IPARTC(IEL)
            IF ( THK_PART(IP) .NE. ZERO) THEN  
              THKSECND = THK_PART(IP)
	    ELSEIF ( THK(IEL) .NE. ZERO) THEN
	      THKSECND = THK(IEL)
	    ELSE
              THKSECND = GEO(1,MG)
	    ENDIF
          ENDDO 
C--         Cooques 3n
          DO IAD = KNOD2ELTG(NSVG(I))+1,KNOD2ELTG(NSVG(I)+1)
            IEL  = NOD2ELTG(IAD) 
            MG=IXTG(5,IEL)
	    IP = IPARTTG(IEL)
            IF ( THK_PART(IP) .NE. ZERO) THEN  
              THKSECND = THK_PART(IP)
	    ELSEIF ( THK(NUMELC+IEL) .NE. ZERO) THEN
	      THKSECND = THK(NUMELC+IEL)
	    ELSE
              THKSECND = GEO(1,MG)
	    ENDIF
          ENDDO
          NELS = 0
          NELC = 0
          NELTG = 0
          CALL INSOL3(X,IRECT,IXS,NINT,NELS,CAND_E(I),
     .            AREA,NOINT,KNOD2ELS ,NOD2ELS ,0 ,IXS10,
     .            IXS16,IXS20)
          CALL INCOQ3(IRECT,IXC ,IXTG ,NINT   ,NELC     ,
     .            NELTG,CAND_E(I),GEO  ,PM     ,KNOD2ELC ,
     .            KNOD2ELTG ,NOD2ELC ,NOD2ELTG,THK,2,IGEO,
     .            PM_STACK , IWORKSH )
          IF(NELTG.NE.0) THEN
            MG=IXTG(5,NELTG)
	    IP = IPARTTG(NELTG) 
            IF ( THK_PART(IP) .NE. ZERO) THEN  
              THKMAIN = THK_PART(IP)
	    ELSEIF ( THK(NUMELC+NELTG) .NE. ZERO) THEN
              THKMAIN = THK(NUMELC+NELTG)
	    ELSE
              THKMAIN = GEO(1,MG)
	    ENDIF
          ELSEIF(NELC.NE.0) THEN
            MG=IXC(6,NELC)
	    IP = IPARTC(NELC) 
            IF ( THK_PART(IP) .NE. ZERO) THEN  
              THKMAIN = THK_PART(IP)
	    ELSEIF ( THK(NELC) .NE. ZERO) THEN
              THKMAIN = THK(NELC)
	    ELSE
              THKMAIN = GEO(1,MG)
	    ENDIF
          ELSEIF(NELS.NE.0 .AND. NELS <= NUMELS8 .AND. IGNORE == 2) THEN
            DO JJ=1,8
              JJJ=IXS(JJ+1,NELS)
              XC(JJ)=X(1,JJJ)
              YC(JJ)=X(2,JJJ)
              ZC(JJ)=X(3,JJJ)
            END DO
            CALL VOLINT(VOL)
C
            THKMAIN = VOL/AREA
          ENDIF
          DD = ZERO
          NODE1=IRECT(1,CAND_E(I))
          NODE2=IRECT(2,CAND_E(I))
          NODE3=IRECT(3,CAND_E(I))
          NODE4=IRECT(4,CAND_E(I))
C         LONGUEUR DIAG 1
          DX1=(X(1,NODE1)-X(1,NODE3))
          DY1=(X(2,NODE1)-X(2,NODE3))
          DZ1=(X(3,NODE1)-X(3,NODE3))
          DD=SQRT(DX1**2+DY1**2+DZ1**2)
C         LONGUEUR DIAG 2
          DX3=(X(1,NODE2)-X(1,NODE4))
          DY3=(X(2,NODE2)-X(2,NODE4))
          DZ3=(X(3,NODE2)-X(3,NODE4))
          DD=MIN(DD,SQRT(DX3**2+DY3**2+DZ3**2))
          GAPV(I) = MAX(ZEP05*DD,ZEP6*(THKSECND+THKMAIN))
        ENDDO
      ELSE
        DO I=LFT,LLT
         GAPV(I) = GAP_S(CAND_N(I))+GAP_M(CAND_E(I))
         GAPV(I) = MAX(GAP,GAPV(I))
        ENDDO
      ENDIF
C
      IF(ISTF.NE.0)THEN
        DO I=LFT,LLT
          STIF(I)=STF(CAND_E(I))*STFN(CAND_N(I))
        ENDDO
      ENDIF
C
      DO I=LFT,LLT
C
        L  = CAND_E(I)
C
        IX1(I)=IRECT(1,L)
        IX2(I)=IRECT(2,L)
        IX3(I)=IRECT(3,L)
        IX4(I)=IRECT(4,L)
C
      ENDDO
C
      DO I=LFT,LLT
C
        NN=IX1(I)
        X1(I)=X(1,NN)
        Y1(I)=X(2,NN)
        Z1(I)=X(3,NN)
C
        NN=IX2(I)
        X2(I)=X(1,NN)
        Y2(I)=X(2,NN)
        Z2(I)=X(3,NN)
C
        NN=IX3(I)
        X3(I)=X(1,NN)
        Y3(I)=X(2,NN)
        Z3(I)=X(3,NN)
C
        NN=IX4(I)
        X4(I)=X(1,NN)
        Y4(I)=X(2,NN)
        Z4(I)=X(3,NN)
C
      ENDDO
C
      RETURN
      END

