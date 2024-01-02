Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  INTER_NODAL_AREAS             source/output/inter_nodal_areas.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        SPMD_EXCH_NODAREA             source/mpi/anim/spmd_exch_nodarea.F
Chd|====================================================================
      SUBROUTINE INTER_NODAL_AREAS(IXS     ,IXC     ,IXTG    ,FASOLFR  ,X       ,
     .                             IAD_ELEM,FR_ELEM ,WEIGHT  ,IXQ     ,SEGQUADFR,
     .                             IXS10   ,INTAREAN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER , INTENT(IN) :: 
     .   IXS(NIXS,NUMELS)  , IXC(NIXC,NUMELC) , IXTG(NIXTG,NUMELTG), FASOLFR(2,NFASOLFR),
     .   IAD_ELEM(2,NSPMD+1), FR_ELEM(SFR_ELEM), WEIGHT(*) ,IXQ(NIXQ,NUMELQ),SEGQUADFR(2,NSEGQUADFR),
     .   IXS10(6,NUMELS10)
      my_real, INTENT(IN) :: 
     .   X(3,NUMNOD)
      my_real, INTENT(INOUT) :: INTAREAN(NUMNOD)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N1, N2, N3, N4, NN1, NN2, NN3, J, I, N, IFAC, ILINE,
     .        LENR, NI
       my_real
     .     AREA, 
     .     X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,
     .     X31,Y31,Z31,X42,Y42,Z42,X32,Y32,Z32,E3X,E3Y,E3Z,
     .     FS2, FS3, FT2, FT3, E, F, G, RAYON,AY1,AY2,AY3,AY4
      INTEGER FACES(4,6),LINES(2,4),FACES10(3,24)
C     REAL
      DATA FACES/1,2,3,4,
     .           2,1,5,6,
     .           1,5,8,4,
     .           5,6,7,8,
     .           3,4,8,7,
     .           2,6,7,3/
      DATA LINES/1,2,
     .           2,3,
     .           3,4,
     .           4,1/
      DATA FACES10/0,0,0,
     .             0,0,0,
     .             0,0,0,
     .             0,0,0,
     .             1,13,14,
     .             5,14,16,
     .             6,13,16,
     .             13,14,16,
     .             1,11,13,
     .             3,11,15,
     .             5,14,15,
     .             11,14,15,
     .             0,0,0,
     .             0,0,0,
     .             0,0,0,
     .             0,0,0,
     .             3,12,15,
     .             5,15,16,
     .             6,12,16,
     .             12,15,16,
     .             1,11,13,
     .             3,11,12,
     .             6,12,13,
     .             11,12,13/
C-----------------------------------------------
C
      DO N=1,NUMNOD
        INTAREAN(N)=ZERO
      END DO
C
      DO I=1,NFASOLFR
        N   =FASOLFR(1,I)
        IFAC=FASOLFR(2,I)
C
        IF( N  <= NUMELS8 ) THEN

           N1=IXS(FACES(1,IFAC)+1,N)
           N2=IXS(FACES(2,IFAC)+1,N)
           N3=IXS(FACES(3,IFAC)+1,N)
           N4=IXS(FACES(4,IFAC)+1,N)
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
C
           X31=X3-X1
           Y31=Y3-Y1
           Z31=Z3-Z1
           X42=X4-X2
           Y42=Y4-Y2
           Z42=Z4-Z2
C
           E3X=Y31*Z42-Z31*Y42
           E3Y=Z31*X42-X31*Z42
           E3Z=X31*Y42-Y31*X42
C
           IF(    N4/=N3
     .       .AND.N3/=N2
     .       .AND.N2/=N1
     .       .AND.N1/=N4)THEN
             AREA=ONE_OVER_8*SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
             INTAREAN(N1)=INTAREAN(N1)+AREA
             INTAREAN(N2)=INTAREAN(N2)+AREA
             INTAREAN(N3)=INTAREAN(N3)+AREA
             INTAREAN(N4)=INTAREAN(N4)+AREA
           ELSE
             AREA=ONE_OVER_6*SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
             IF(N4==N3)THEN
                IF(N2/=N1) THEN
                  INTAREAN(N1)=INTAREAN(N1)+AREA
                  INTAREAN(N2)=INTAREAN(N2)+AREA
                  INTAREAN(N3)=INTAREAN(N3)+AREA
                ENDIF
             ELSEIF(N3==N2)THEN
                IF(N4/=N1) THEN
                  INTAREAN(N1)=INTAREAN(N1)+AREA
                  INTAREAN(N2)=INTAREAN(N2)+AREA
                  INTAREAN(N4)=INTAREAN(N4)+AREA
                ENDIF
             ELSEIF(N2==N1)THEN
                IF(N4/=N3) THEN
                   INTAREAN(N2)=INTAREAN(N2)+AREA
                   INTAREAN(N3)=INTAREAN(N3)+AREA
                   INTAREAN(N4)=INTAREAN(N4)+AREA
                ENDIF
             ELSEIF(N1==N4)THEN
                IF(N2/=N3) THEN
                   INTAREAN(N2)=INTAREAN(N2)+AREA
                   INTAREAN(N3)=INTAREAN(N3)+AREA
                   INTAREAN(N4)=INTAREAN(N4)+AREA
                ENDIF
             END IF
           END IF

        ELSEIF( N  <= NUMELS8+NUMELS10 ) THEN

C SubFac1 
           DO J=1,4         
              NN1=FACES10(1,4*(IFAC-1)+J)
              NN2=FACES10(2,4*(IFAC-1)+J)
              NN3=FACES10(3,4*(IFAC-1)+J)
       
              IF(NN1 > 0 ) THEN 
                 IF(NN1 >0.AND.NN1 < 10) THEN
                    N1=IXS(NN1+1,N) 
                 ELSE
                    N1=IXS10(NN1-10,N-NUMELS8)
                 ENDIF
              ENDIF

              IF(NN2 > 0 ) THEN 
                IF(NN2 < 10) THEN
                   N2=IXS(NN2+1,N) 
                 ELSE
                   N2=IXS10(NN2-10,N-NUMELS8)
                 ENDIF
              ENDIF 

              IF(NN3 > 0 ) THEN 
                 IF(NN3 < 10) THEN
                    N3=IXS(NN3+1,N) 
                 ELSE
                    N3=IXS10(NN3-10,N-NUMELS8)
                 ENDIF
              ENDIF


              IF(NN1 > 0 .AND. NN2 > 0 .AND.NN3 > 0) THEN
                X1=X(1,N1)
                Y1=X(2,N1)
                Z1=X(3,N1)
                X2=X(1,N2)
                Y2=X(2,N2)
                Z2=X(3,N2)
                X3=X(1,N3)
                Y3=X(2,N3)
                Z3=X(3,N3)
C
                X31=X3-X1
                Y31=Y3-Y1
                Z31=Z3-Z1
                X32=X3-X2
                Y32=Y3-Y2
                Z32=Z3-Z2
C
                E3X=Y31*Z32-Z31*Y32
                E3Y=Z31*X32-X31*Z32
                E3Z=X31*Y32-Y31*X32
C
                AREA=ONE_OVER_6*SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)

                INTAREAN(N1)=INTAREAN(N1)+AREA
                INTAREAN(N2)=INTAREAN(N2)+AREA
                INTAREAN(N3)=INTAREAN(N3)+AREA
              ENDIF
           ENDDO
        ENDIF
      END DO
C
      DO N=1,NUMELC
        N1=IXC(2,N)
        N2=IXC(3,N)
        N3=IXC(4,N)
        N4=IXC(5,N)
        IF(N4/=N3)THEN
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
C
          X31=X3-X1
          Y31=Y3-Y1
          Z31=Z3-Z1
          X42=X4-X2
          Y42=Y4-Y2
          Z42=Z4-Z2
C
          E3X=Y31*Z42-Z31*Y42
          E3Y=Z31*X42-X31*Z42
          E3Z=X31*Y42-Y31*X42
C
          AREA=ONE_OVER_8*SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
          INTAREAN(N1)=INTAREAN(N1)+AREA
          INTAREAN(N2)=INTAREAN(N2)+AREA
          INTAREAN(N3)=INTAREAN(N3)+AREA
          INTAREAN(N4)=INTAREAN(N4)+AREA
C
        ELSE
          X1=X(1,N1)
          Y1=X(2,N1)
          Z1=X(3,N1)
          X2=X(1,N2)
          Y2=X(2,N2)
          Z2=X(3,N2)
          X3=X(1,N3)
          Y3=X(2,N3)
          Z3=X(3,N3)
          X31=X3-X1
          Y31=Y3-Y1
          Z31=Z3-Z1
          X32=X3-X2
          Y32=Y3-Y2
          Z32=Z3-Z2
C
          E3X=Y31*Z32-Z31*Y32
          E3Y=Z31*X32-X31*Z32
          E3Z=X31*Y32-Y31*X32
C
          AREA=ONE_OVER_6*SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
          INTAREAN(N1)=INTAREAN(N1)+AREA
          INTAREAN(N2)=INTAREAN(N2)+AREA
          INTAREAN(N3)=INTAREAN(N3)+AREA
        END IF
      END DO
C
      DO N=1,NUMELTG
        N1=IXTG(2,N)
        N2=IXTG(3,N)
        N3=IXTG(4,N)
        X1=X(1,N1)
        Y1=X(2,N1)
        Z1=X(3,N1)
        X2=X(1,N2)
        Y2=X(2,N2)
        Z2=X(3,N2)
        X3=X(1,N3)
        Y3=X(2,N3)
        Z3=X(3,N3)
        X31=X3-X1
        Y31=Y3-Y1
        Z31=Z3-Z1
        X32=X3-X2
        Y32=Y3-Y2
        Z32=Z3-Z2
C
        E3X=Y31*Z32-Z31*Y32
        E3Y=Z31*X32-X31*Z32
        E3Z=X31*Y32-Y31*X32
C
        AREA=ONE_OVER_6*SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
        INTAREAN(N1)=INTAREAN(N1)+AREA
        INTAREAN(N2)=INTAREAN(N2)+AREA
        INTAREAN(N3)=INTAREAN(N3)+AREA
      END DO
C
      DO I=1,NSEGQUADFR
        N  =SEGQUADFR(1,I)
        ILINE=SEGQUADFR(2,I)

        N1=IXQ(LINES(1,ILINE)+1,N)
        N2=IXQ(LINES(2,ILINE)+1,N)

        Y1=X(2,N1)
        Z1=X(3,N1)
        Y2=X(2,N2)
        Z2=X(3,N2)

        AREA = SQRT((Y2-Y1)*(Y2-Y1)+(Z2-Z1)*(Z2-Z1))
        AREA = AREA*HALF


        INTAREAN(N1)=INTAREAN(N1)+AREA
        INTAREAN(N2)=INTAREAN(N2)+AREA

      ENDDO
C
      IF(NSPMD > 1)THEN
        LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
        CALL SPMD_EXCH_NODAREA(INTAREAN,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
      END IF
C
      RETURN
      END SUBROUTINE INTER_NODAL_AREAS

