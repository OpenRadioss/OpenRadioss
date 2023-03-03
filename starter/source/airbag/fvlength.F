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
Chd|  FVLENGTH                      source/airbag/fvlength.F      
Chd|-- called by -----------
Chd|        FVMESH0                       source/airbag/fvmesh0.F       
Chd|-- calls ---------------
Chd|        FVNORMAL                      source/airbag/fvmbag1.F       
Chd|====================================================================
      SUBROUTINE FVLENGTH(IFV  ,NNS    ,NNTR   ,NPOLH,
     1                  IBUF   ,IBUFA  ,ELEMA  ,TAGELA ,
     2                  X      ,IVOLU  ,RVOLU  ,
     3                  IFVNOD ,RFVNOD ,IFVTRI , 
     4                  IFVPOLY,IFVTADR,IFVPOLH, 
     5                  IFVPADR,IBPOLH ,DLH    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFV, NNS, NNTR, NPOLH
      INTEGER IBUF(*),   IBUFA(*),  ELEMA(3,*), TAGELA(*), 
     .        IVOLU(*),  IBPOLH(*), IFVNOD(3,*),IFVTRI(6,*),
     .        IFVPOLY(*),IFVTADR(*),IFVPOLH(*), IFVPADR(*)
      my_real
     .        X(3,*), RVOLU(*), RFVNOD(2,*), DLH 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J,  K,  I1,  I2,  IEL, JJ,  KK,
     .        N1, N2, N3, NN1, NN2, NN3,
     .        NADBR
      INTEGER IDBR1, IDBR2, IDBR3
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, 
     .        NX, NY, NZ, AREA2, KSI, ETA, AREA, FAC,
     .        AREAPOLY, AREAPOLYMAX, 
     .        PNOD(3,NNS),  PVOLU(NPOLH),
     .        PAREA(NNTR),  PNORM(3,NNTR)
      my_real
     .        X12, Y12, Z12, X23, Y23, Z23, X31, Y31, Z31, 
     .        L12, L23, L31, DL0, DL1, DL2, DLL, DLBR1, DLBR2, DLBR3
C---------------------------------------------------
C Noeuds des volumes finis
C---------------------------------------------------
      DO I=1,NNS
         IF (IFVNOD(1,I)==1) THEN
            IEL=IFVNOD(2,I)
            KSI=RFVNOD(1,I)
            ETA=RFVNOD(2,I)
C
            N1=ELEMA(1,IEL)
            N2=ELEMA(2,IEL)
            N3=ELEMA(3,IEL)
C
            IF (TAGELA(IEL)>0) THEN
               NN1=IBUF(N1)
               NN2=IBUF(N2)
               NN3=IBUF(N3)
               X1=X(1,NN1)
               Y1=X(2,NN1)
               Z1=X(3,NN1)
               X2=X(1,NN2)
               Y2=X(2,NN2)
               Z2=X(3,NN2)
               X3=X(1,NN3)
               Y3=X(2,NN3)
               Z3=X(3,NN3)
            ELSEIF (TAGELA(IEL)<0) THEN
               NN1=IBUFA(N1)
               NN2=IBUFA(N2)
               NN3=IBUFA(N3)
               X1=X(1,NN1)
               Y1=X(2,NN1)
               Z1=X(3,NN1)
               X2=X(1,NN2)
               Y2=X(2,NN2)
               Z2=X(3,NN2)
               X3=X(1,NN3)
               Y3=X(2,NN3)
               Z3=X(3,NN3)
            ENDIF
            PNOD(1,I)=(ONE-KSI-ETA)*X1+KSI*X2+ETA*X3
            PNOD(2,I)=(ONE-KSI-ETA)*Y1+KSI*Y2+ETA*Y3
            PNOD(3,I)=(ONE-KSI-ETA)*Z1+KSI*Z2+ETA*Z3
C
         ELSEIF (IFVNOD(1,I)==2) THEN
            I2=IFVNOD(2,I)
            PNOD(1,I)=X(1,I2)
            PNOD(2,I)=X(2,I2)
            PNOD(3,I)=X(3,I2)
         ENDIF
      ENDDO
C
      DO I=1,NNS
         IF (IFVNOD(1,I)==3) THEN
            I1=IFVNOD(2,I)
            I2=IFVNOD(3,I)
            FAC=RFVNOD(1,I)
            PNOD(1,I)=FAC*PNOD(1,I1)+(ONE-FAC)*PNOD(1,I2)
            PNOD(2,I)=FAC*PNOD(2,I1)+(ONE-FAC)*PNOD(2,I2)
            PNOD(3,I)=FAC*PNOD(3,I1)+(ONE-FAC)*PNOD(3,I2)
         ENDIF
      ENDDO
C----------------------------
C Normale, aire des triangles
C----------------------------
      DO I=1,NNTR
         N1=IFVTRI(1,I)
         N2=IFVTRI(2,I)
         N3=IFVTRI(3,I)
         CALL FVNORMAL(PNOD,N1,N2,N3,0,NX,NY,NZ)
         AREA2=SQRT(NX*NX+NY*NY+NZ*NZ)
         PAREA(I)=HALF*AREA2
         IF (AREA2>0) THEN
            PNORM(1,I)=NX/AREA2
            PNORM(2,I)=NY/AREA2
            PNORM(3,I)=NZ/AREA2
         ELSE
            PNORM(1,I)=ZERO
            PNORM(2,I)=ZERO
            PNORM(3,I)=ZERO
         ENDIF
      ENDDO
C------------------------------------------
C Volume des polyhedres et longueur minimum
C------------------------------------------
      DL0=DLH
      DL1=EP30
      DL2=EP30
      DLBR1=EP30
      DLBR2=EP30 
      DLBR3=EP30 
      NADBR=0
      DO I=1,NPOLH
         PVOLU(I)=ZERO
         AREAPOLYMAX=ZERO
C Boucle sur les polygones du polyhedre
         DO J=IFVPADR(I),IFVPADR(I+1)-1
            JJ=IFVPOLH(J)
            AREAPOLY=ZERO
C Boucle sur les triangles du polygone
            DO K=IFVTADR(JJ), IFVTADR(JJ+1)-1
               KK=IFVPOLY(K)
               AREA=PAREA(KK)
               AREAPOLY=AREAPOLY+AREA
               IEL=IFVTRI(4,KK)
               IF (IEL>0) THEN
                  NX=PNORM(1,KK)
                  NY=PNORM(2,KK)
                  NZ=PNORM(3,KK)
               ELSE
                  IF (IFVTRI(5,KK)==I) THEN
                     NX=PNORM(1,KK)
                     NY=PNORM(2,KK)
                     NZ=PNORM(3,KK)
                  ELSEIF (IFVTRI(6,KK)==I) THEN
                     NX=-PNORM(1,KK)
                     NY=-PNORM(2,KK)
                     NZ=-PNORM(3,KK)
                  ENDIF   
               ENDIF      
               N1=IFVTRI(1,KK)
               X1=PNOD(1,N1)
               Y1=PNOD(2,N1)
               Z1=PNOD(3,N1)
               PVOLU(I)=PVOLU(I)+THIRD*AREA*(X1*NX+Y1*NY+Z1*NZ)
C Compute minimum length DL2
               N2=IFVTRI(2,KK)
               X2=PNOD(1,N2)
               Y2=PNOD(2,N2)
               Z2=PNOD(3,N2)
               N3=IFVTRI(3,KK)
               X3=PNOD(1,N3)
               Y3=PNOD(2,N3)
               Z3=PNOD(3,N3)
               X12=X2-X1
               Y12=Y2-Y1
               Z12=Z2-Z1
               X23=X3-X2
               Y23=Y3-Y2
               Z23=Z3-Z2
               X31=X1-X3
               Y31=Y1-Y3
               Z31=Z1-Z3
               L12=X12**2+Y12**2+Z12**2
               L23=X23**2+Y23**2+Z23**2
               L31=X31**2+Y31**2+Z31**2
               IF(IBPOLH(I)==0) THEN
                 DL2=MIN(DL2,L12,L23,L31)
               ELSE
                 DLL=MIN(L12,L23,L31)
                 IF(DLL < DLBR2) THEN
                    DLBR2=DLL
                    IDBR2=I
                 ENDIF
               ENDIF
            ENDDO
            AREAPOLYMAX=MAX(AREAPOLYMAX,AREAPOLY)
         ENDDO
C Compute minimum length DL1
         IF(IBPOLH(I)==0) THEN
           DL1=MIN(DL1,PVOLU(I))
         ELSE
           NADBR=NADBR+1
           IF(PVOLU(I) < DLBR1) THEN
              DLBR1=PVOLU(I)
              IDBR1=I
           ENDIF
           DLL=PVOLU(I)/AREAPOLYMAX
           IF(DLL < DLBR3) THEN
              DLBR3=DLL
              IDBR3=I
           ENDIF
C
           IF(PVOLU(I)<0)THEN
              WRITE(IOUT,'(A,E12.4,3I10)') 'NEGATIVE VOLUME',
     .              PVOLU(I),I,IBPOLH(I),NADBR
           ENDIF
         ENDIF
      ENDDO  ! I=1,NPOLH
C
      IF(DL1 > ZERO) THEN
         DL1=DL1**THIRD
      ELSE
         DL1=ZERO
      ENDIF
      DL2=SQRT(DL2)
      IF(DLBR1 > ZERO) THEN
         DLBR1=DLBR1**THIRD
      ELSE
         DLBR1=ZERO
      ENDIF
      DLBR2=SQRT(DLBR2)
      IF(DL0==ZERO) DLH=DLBR2
C---------------------------------------------------
C Impressions
C---------------------------------------------------
      WRITE(IOUT,1000) IVOLU(1),NPOLH,NPOLH-NADBR,DL0,DL1,DL2
      IF(NADBR > 0) THEN
         WRITE(IOUT,1001) NADBR,
     .                    DLBR1,IDBR1,IABS(IBPOLH(IDBR1)),
     .                    DLBR2,IDBR2,IABS(IBPOLH(IDBR2)),
     .                    DLBR3,IDBR3,IABS(IBPOLH(IDBR3))
      ENDIF
C
1000  FORMAT(
     . //'     FVMBAG: FINITE VOLUME MINIMUM LENGTH '/
     .   '     -------------------------------------'/
     .    /5X,'VOLUME NUMBER ',I10,
     .    /5X,'TOTAL NUMBER OF FINITE VOLUMES.. . . . . . .=',I10,
     .    /5X,'NUMBER OF POLYHEDRA . . . . .. . . . . . . .=',I10,
     .    /5X,'    MINIMUM LENGTH USED FOR TIME STEP. . . .=',1PG20.13,
     .    /5X,'    MINIMUM LENGTH BASED ON VOLUME . . . . .=',1PG20.13,
     .    /5X,'    MINIMUM LENGTH BASED ON NODAL DISTANCE .=',1PG20.13)
1001  FORMAT(
     .     5X,'NUMBER OF ADDITIONAL BRICKS. . . . . . . . .=',I10,
     .    /5X,'    MINIMUM LENGTH BASED ON VOLUME . . . . .=',1PG20.13,' (FINITE VOLUME ID ',I10,')',' (BRICK ID ',I10,')',
     .    /5X,'    MINIMUM LENGTH BASED ON NODAL DISTANCE .=',1PG20.13,' (FINITE VOLUME ID ',I10,')',' (BRICK ID ',I10,')',
     .    /5X,'    MINIMUM LENGTH BASED ON VOLUME/AREA. . .=',1PG20.13,' (FINITE VOLUME ID ',I10,')',' (BRICK ID ',I10,')',/)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3--
      RETURN
      END