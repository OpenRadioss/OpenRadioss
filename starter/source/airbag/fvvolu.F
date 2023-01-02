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
C|============================================================
C|  FVVOLU                                  /airbag1/fvvolu.F
C|------------------------------------------------------------
C|-- appelee par -----------
C|         FVMESH0                          /airbag1/fvmesh0.F
C|-- appelle ---------------
C|============================================================
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
Chd|====================================================================
Chd|  FVVOLU                        source/airbag/fvvolu.F        
Chd|-- called by -----------
Chd|        FVMESH0                       source/airbag/fvmesh0.F       
Chd|-- calls ---------------
Chd|        FVNORMAL                      source/airbag/fvmbag1.F       
Chd|====================================================================
      SUBROUTINE FVVOLU(IFV    ,ITYP   ,NNS    ,NNTR    ,NPOLH,
     1                  IBUF   ,IBUFA  ,ELEMA  ,TAGELA ,
     2                  X      ,IVOLU  ,RVOLU  ,
     3                  IFVNOD ,RFVNOD ,IFVTRI , 
     4                  IFVPOLY,IFVTADR,IFVPOLH, 
     5                  IFVPADR,IDPOLH ,MPOLH  ,
     6                  EPOLH  ,VPOLH_INI)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
CC      USE FVBAG_MOD
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
      INTEGER IFV, ITYP, NNS, NNTR, NPOLH
      INTEGER IBUF(*),   IBUFA(*),  ELEMA(3,*), TAGELA(*), 
     .        IVOLU(*),  IDPOLH(*), IFVNOD(3,*),IFVTRI(6,*),
     .        IFVPOLY(*),IFVTADR(*),IFVPOLH(*), IFVPADR(*)
      my_real
     .        X(3,*), RVOLU(*), RFVNOD(2,*), 
     .        MPOLH(*), EPOLH(*),VPOLH_INI(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  J,  K,  I1,  I2,  IEL, JJ,  KK,
     .        N1, N2, N3, NN1, NN2, NN3,
     .        NSTR, NCTR, NPOLH_N
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, 
     .        NX, NY, NZ, AREA2, KSI, ETA, AREA, FAC, 
     .        PNOD(3,NNS),  PVOLU(NPOLH),  VOLPH,
     .        PAREA(NNTR),  PNORM(3,NNTR), AREAP
      my_real
     .        CPAI, CPBI, CPCI, CPDI, CPEI, CPFI,
     .        RMWI, PINI, TI,   TI2,  RHOI, EFAC 
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
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
C----------------------
C Volume des polyhedres
C----------------------
      DO I=1,NPOLH
         PVOLU(I)=ZERO
C Boucle sur les polygones du polyhedre
         DO J=IFVPADR(I),IFVPADR(I+1)-1
            JJ=IFVPOLH(J)
C Boucle sur les triangles du polygone
            DO K=IFVTADR(JJ), IFVTADR(JJ+1)-1
               KK=IFVPOLY(K)
               AREA=PAREA(KK)
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
            ENDDO
         ENDDO
      ENDDO
C---------------------------------------------------
C Impressions
C---------------------------------------------------
      VOLPH=ZERO
      AREAP=ZERO
      NPOLH_N=0
      DO I=1,NPOLH
         VOLPH=VOLPH+PVOLU(I)
         IF (PVOLU(I)<=ZERO) NPOLH_N=NPOLH_N+1
      ENDDO
C
      NSTR=0
      NCTR=0
      DO I=1,NNTR
         IF (IFVTRI(4,I)>0) THEN
            NSTR=NSTR+1
            AREAP=AREAP+PAREA(I)
         ELSE
            NCTR=NCTR+1
         ENDIF
      ENDDO
C
      WRITE(IOUT,1000) IVOLU(1),NSTR,NCTR,NPOLH,NPOLH_N,VOLPH,AREAP
C
C---------------------------------------------------
C Update des quantites dans les polyhedres
C---------------------------------------------------
      CPAI =RVOLU(7)
      CPBI =RVOLU(8)
      CPCI =RVOLU(9)
      RMWI =RVOLU(10)
      PINI =RVOLU(12)
      TI   =RVOLU(13)
      TI2 =TI*TI
      EFAC=TI*(CPAI+HALF*CPBI*TI+THIRD*CPCI*TI2-RMWI)
      RHOI=PINI/(TI*RMWI)
      CPDI=RVOLU(56)
      CPEI=RVOLU(57)
      CPFI=RVOLU(58)
      IF(ITYP==8) THEN
         EFAC=EFAC+FOURTH*CPDI*TI2*TI2-CPEI/TI+ONE_FIFTH*CPFI*TI2*TI2*TI          
      ENDIF
      DO I=1,NPOLH
         MPOLH(I)=RHOI*PVOLU(I)
         EPOLH(I)=MPOLH(I)*EFAC
         VPOLH_INI(I)=PVOLU(I)
      ENDDO
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
1000  FORMAT(
     . //'     FVMBAG: FINITE VOLUME MESH ON INITIAL GEOMETRY'/
     .   '     ----------------------------------------------'/
     .       /5X,'VOLUME NUMBER ',I10,
     .       /5X,'NUMBER OF SURFACE TRIANGLES . . . . . . .=',I10,
     .       /5X,'NUMBER OF COMMUNICATION TRIANGLES . . . .=',I10,
     .       /5X,'NUMBER OF FINITE VOLUMES. . . . . . . . .=',I10,
     .       /5X,'NUMBER OF FINITE VOLUMES WITH VOLUME <0 .=',I10,
     .       /5X,'SUM VOLUME OF FINITE VOLUMES. . . . . . .=',1PG20.13,
     .       /5X,'SUM AREA SURFACE TRIANGLES. . . . . . . .=',1PG20.13/)
      RETURN
      END