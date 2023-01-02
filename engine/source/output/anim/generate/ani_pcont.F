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
Chd|  ANI_PCONT                     source/output/anim/generate/ani_pcont.F
Chd|-- called by -----------
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|-- calls ---------------
Chd|        SPMD_EXCH_NODAREA             source/mpi/anim/spmd_exch_nodarea.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE ANI_PCONT(IXS ,IXC    ,IXTG    ,FASOLFR  ,X       ,
     .                 CONTN   ,CONTT  ,IAD_ELEM,FR_ELEM,WEIGHT    ,
     .                 IXQ     ,SEGQUADFR,IXS10 ,FNCONTP2,FTCONTP2 ,
     .                 H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
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
#include      "units_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER  
     .   IXS(NIXS,*), IXC(NIXC,*), IXTG(NIXTG,*), FASOLFR(2,*),
     .   IAD_ELEM(2,*), FR_ELEM(*), WEIGHT(*),IXQ(7,*),SEGQUADFR(2,*),
     .   IXS10(6,*)
      my_real
     .   X(3,*), CONTN(3,*), CONTT(3,*),FNCONTP2(3,*),FTCONTP2(3,*)
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N1,N2,N3,N4,NN1,NN2,NN3,J,
     .        I,N,IFAC,ILINE,
     .        IERROR, LENR
       my_real
     .     AREA, 
     .     X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,
     .     X31,Y31,Z31,X42,Y42,Z42,X32,Y32,Z32,E3X,E3Y,E3Z,
     .     FS2, FS3, FT2, FT3, E, F, G, RAYON,AY1,AY2,AY3,AY4
      INTEGER FACES(4,6),LINES(2,4),FACES10(3,24)
       my_real, DIMENSION(:), ALLOCATABLE ::  NODAREA
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
      ALLOCATE(NODAREA(NUMNOD))
      DO N=1,NUMNOD
        NODAREA(N)=ZERO
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
             E3X=ONE_OVER_8*E3X
             E3Y=ONE_OVER_8*E3Y
             E3Z=ONE_OVER_8*E3Z
             AREA=SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
             NODAREA(N1)=NODAREA(N1)+AREA
             NODAREA(N2)=NODAREA(N2)+AREA
             NODAREA(N3)=NODAREA(N3)+AREA
             NODAREA(N4)=NODAREA(N4)+AREA
           ELSE
             E3X=ONE_OVER_6*E3X
             E3Y=ONE_OVER_6*E3Y
             E3Z=ONE_OVER_6*E3Z
             AREA=SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
             IF(N4==N3)THEN
                IF(N2/=N1) THEN
                  NODAREA(N1)=NODAREA(N1)+AREA
                  NODAREA(N2)=NODAREA(N2)+AREA
                  NODAREA(N3)=NODAREA(N3)+AREA
                ENDIF
             ELSEIF(N3==N2)THEN
                IF(N4/=N1) THEN
                  NODAREA(N1)=NODAREA(N1)+AREA
                  NODAREA(N2)=NODAREA(N2)+AREA
                  NODAREA(N4)=NODAREA(N4)+AREA
                ENDIF
             ELSEIF(N2==N1)THEN
                IF(N4/=N3) THEN
                   NODAREA(N2)=NODAREA(N2)+AREA
                   NODAREA(N3)=NODAREA(N3)+AREA
                   NODAREA(N4)=NODAREA(N4)+AREA
                ENDIF
             ELSEIF(N1==N4)THEN
                IF(N2/=N3) THEN
                   NODAREA(N2)=NODAREA(N2)+AREA
                   NODAREA(N3)=NODAREA(N3)+AREA
                   NODAREA(N4)=NODAREA(N4)+AREA
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
                E3X=ONE_OVER_6*E3X
                E3Y=ONE_OVER_6*E3Y
                E3Z=ONE_OVER_6*E3Z
                AREA=SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)

                NODAREA(N1)=NODAREA(N1)+AREA
                NODAREA(N2)=NODAREA(N2)+AREA
                NODAREA(N3)=NODAREA(N3)+AREA
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
          E3X=ONE_OVER_8*E3X
          E3Y=ONE_OVER_8*E3Y
          E3Z=ONE_OVER_8*E3Z
C
          AREA=SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
          NODAREA(N1)=NODAREA(N1)+AREA
          NODAREA(N2)=NODAREA(N2)+AREA
          NODAREA(N3)=NODAREA(N3)+AREA
          NODAREA(N4)=NODAREA(N4)+AREA
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
          E3X=ONE_OVER_6*E3X
          E3Y=ONE_OVER_6*E3Y
          E3Z=ONE_OVER_6*E3Z
C
          AREA=SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
          NODAREA(N1)=NODAREA(N1)+AREA
          NODAREA(N2)=NODAREA(N2)+AREA
          NODAREA(N3)=NODAREA(N3)+AREA
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
        E3X=ONE_OVER_6*E3X
        E3Y=ONE_OVER_6*E3Y
        E3Z=ONE_OVER_6*E3Z
C
        AREA=SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
        NODAREA(N1)=NODAREA(N1)+AREA
        NODAREA(N2)=NODAREA(N2)+AREA
        NODAREA(N3)=NODAREA(N3)+AREA
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


       NODAREA(N1)=NODAREA(N1)+AREA
       NODAREA(N2)=NODAREA(N2)+AREA

      ENDDO
C
      IF(NSPMD > 1)THEN
        LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
        CALL SPMD_EXCH_NODAREA(NODAREA,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
      END IF
C
      IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT /=0) THEN
         DO N=1,NUMNOD
           IF (NODAREA(N) == ZERO) THEN
             CONTN(1,N)=ZERO
             CONTN(2,N)=ZERO
             CONTN(3,N)=ZERO
             CONTT(1,N)=ZERO
             CONTT(2,N)=ZERO
             CONTT(3,N)=ZERO
           ELSE
             NODAREA(N)=ONE/MAX(EM30,NODAREA(N))
             CONTN(1,N)=NODAREA(N)*CONTN(1,N)
             CONTN(2,N)=NODAREA(N)*CONTN(2,N)
             CONTN(3,N)=NODAREA(N)*CONTN(3,N)
             CONTT(1,N)=NODAREA(N)*CONTT(1,N)
             CONTT(2,N)=NODAREA(N)*CONTT(2,N)
             CONTT(3,N)=NODAREA(N)*CONTT(3,N)
           ENDIF
         END DO
       ENDIF
C
      IF(ANIM_V(27)+H3D_DATA%N_VECT_PCONT2 /=0) THEN
         DO N=1,NUMNOD
           IF (NODAREA(N) == ZERO) THEN
             FNCONTP2(1,N)=ZERO
             FNCONTP2(2,N)=ZERO
             FNCONTP2(3,N)=ZERO
             FTCONTP2(1,N)=ZERO
             FTCONTP2(2,N)=ZERO
             FTCONTP2(3,N)=ZERO
           ELSE
             NODAREA(N)=ONE/MAX(EM30,NODAREA(N))
             FNCONTP2(1,N)=NODAREA(N)*FNCONTP2(1,N)
             FNCONTP2(2,N)=NODAREA(N)*FNCONTP2(2,N)
             FNCONTP2(3,N)=NODAREA(N)*FNCONTP2(3,N)
             FTCONTP2(1,N)=NODAREA(N)*FTCONTP2(1,N)
             FTCONTP2(2,N)=NODAREA(N)*FTCONTP2(2,N)
             FTCONTP2(3,N)=NODAREA(N)*FTCONTP2(3,N)
           ENDIF
         END DO
       ENDIF
C
      DEALLOCATE(NODAREA)
C
      RETURN
      END
Chd|====================================================================
Chd|  ANI_PCONT21                   source/output/anim/generate/ani_pcont.F
Chd|-- called by -----------
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|-- calls ---------------
Chd|        SPMD_EXCH_NODAREA             source/mpi/anim/spmd_exch_nodarea.F
Chd|        SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE ANI_PCONT21(IXS ,IXC    ,IXTG    ,FASOLFR  ,X       ,
     .                 CONTN   ,CONTT  ,IAD_ELEM,FR_ELEM,WEIGHT      ,
     .                 NODGLOB ,FNCONTG ,FTCONTG,FNCONTP2,FTCONTP2   ,
     .                 H3D_DATA )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
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
#include      "spmd_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER  
     .   IXS(NIXS,*), IXC(NIXC,*), IXTG(NIXTG,*), FASOLFR(2,*),
     .   IAD_ELEM(2,*), FR_ELEM(*), WEIGHT(*), NODGLOB(*)
      my_real
     .   X(3,*), CONTN(3,*), CONTT(3,*), FNCONTG(3,*), FTCONTG(3,*),
     .   FNCONTP2(3,*),FTCONTP2(3,*)
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N1,N2,N3,N4,
     .        I,K,N,IFAC,
     .        IERROR, LENR
       my_real
     .     AREA, 
     .     X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,
     .     X31,Y31,Z31,X42,Y42,Z42,X32,Y32,Z32,E3X,E3Y,E3Z,
     .     TMP
      INTEGER FACES(4,6)
      my_real, DIMENSION(:), ALLOCATABLE ::  NODAREA,NODAREAG
C     REAL
      DATA FACES/1,2,3,4,
     .           2,1,5,6,
     .           1,5,8,4,
     .           5,6,7,8,
     .           3,4,8,7,
     .           2,6,7,3/
C-----------------------------------------------
C
      ALLOCATE(NODAREA(NUMNOD))
      DO N=1,NUMNOD
        NODAREA(N)=ZERO
      END DO
C
      DO I=1,NFASOLFR
        N   =FASOLFR(1,I)
        IFAC=FASOLFR(2,I)
C
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
     .    .AND.N3/=N2
     .    .AND.N2/=N1
     .    .AND.N1/=N4)THEN
          E3X=ONE_OVER_8*E3X
          E3Y=ONE_OVER_8*E3Y
          E3Z=ONE_OVER_8*E3Z
          AREA=SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
          NODAREA(N1)=NODAREA(N1)+AREA
          NODAREA(N2)=NODAREA(N2)+AREA
          NODAREA(N3)=NODAREA(N3)+AREA
          NODAREA(N4)=NODAREA(N4)+AREA
        ELSE
          E3X=ONE_OVER_6*E3X
          E3Y=ONE_OVER_6*E3Y
          E3Z=ONE_OVER_6*E3Z
          AREA=SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
          IF(N4==N3)THEN
            IF(N2/=N1) THEN
              NODAREA(N1)=NODAREA(N1)+AREA
              NODAREA(N2)=NODAREA(N2)+AREA
              NODAREA(N3)=NODAREA(N3)+AREA
            ENDIF
          ELSEIF(N3==N2)THEN
            IF(N4/=N1) THEN
              NODAREA(N1)=NODAREA(N1)+AREA
              NODAREA(N2)=NODAREA(N2)+AREA
              NODAREA(N4)=NODAREA(N4)+AREA
            ENDIF
          ELSEIF(N2==N1)THEN
            IF(N4/=N3) THEN
              NODAREA(N2)=NODAREA(N2)+AREA
              NODAREA(N3)=NODAREA(N3)+AREA
              NODAREA(N4)=NODAREA(N4)+AREA
            ENDIF
          ELSEIF(N1==N4)THEN
            IF(N2/=N3) THEN
              NODAREA(N2)=NODAREA(N2)+AREA
              NODAREA(N3)=NODAREA(N3)+AREA
              NODAREA(N4)=NODAREA(N4)+AREA
            ENDIF
          END IF
        END IF
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
          E3X=ONE_OVER_8*E3X
          E3Y=ONE_OVER_8*E3Y
          E3Z=ONE_OVER_8*E3Z
C
          AREA=SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
          NODAREA(N1)=NODAREA(N1)+AREA
          NODAREA(N2)=NODAREA(N2)+AREA
          NODAREA(N3)=NODAREA(N3)+AREA
          NODAREA(N4)=NODAREA(N4)+AREA
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
          E3X=ONE_OVER_6*E3X
          E3Y=ONE_OVER_6*E3Y
          E3Z=ONE_OVER_6*E3Z
C
          AREA=SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
          NODAREA(N1)=NODAREA(N1)+AREA
          NODAREA(N2)=NODAREA(N2)+AREA
          NODAREA(N3)=NODAREA(N3)+AREA
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
        E3X=ONE_OVER_6*E3X
        E3Y=ONE_OVER_6*E3Y
        E3Z=ONE_OVER_6*E3Z
C
        AREA=SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
        NODAREA(N1)=NODAREA(N1)+AREA
        NODAREA(N2)=NODAREA(N2)+AREA
        NODAREA(N3)=NODAREA(N3)+AREA
      END DO
C
      IF(NSPMD > 1)THEN
        LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
        CALL SPMD_EXCH_NODAREA(NODAREA,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
      END IF
C-----

      IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT /=0) THEN
C     INTER == 21
        IF (NSPMD == 1) THEN
          DO N=1,NUMNOD
            I=NODGLOB(N)
            IF (NODAREA(N) == ZERO) THEN
              FNCONTG(1,I)=ZERO
              FNCONTG(2,I)=ZERO
              FNCONTG(3,I)=ZERO
              FTCONTG(1,I)=ZERO
              FTCONTG(2,I)=ZERO
              FTCONTG(3,I)=ZERO
            ELSE
              TMP=ONE/MAX(EM30,NODAREA(N))
              FNCONTG(1,I)=FNCONTG(1,I)*TMP
              FNCONTG(2,I)=FNCONTG(2,I)*TMP
              FNCONTG(3,I)=FNCONTG(3,I)*TMP
              FTCONTG(1,I)=FTCONTG(1,I)*TMP
              FTCONTG(2,I)=FTCONTG(2,I)*TMP
              FTCONTG(3,I)=FTCONTG(3,I)*TMP
            ENDIF
          END DO
        ELSE
          ALLOCATE(NODAREAG(NUMNODG))
           DO I=1,NUMNODG
             NODAREAG(I)=ZERO
           ENDDO
           DO K=1,NUMNOD
             I=NODGLOB(K)
             NODAREAG(I)=NODAREA(K)*WEIGHT(K)
           ENDDO

           CALL SPMD_GLOB_DSUM9(NODAREAG,NUMNODG)
           CALL SPMD_GLOB_DSUM9(FNCONTG,3*NUMNODG)
           CALL SPMD_GLOB_DSUM9(FTCONTG,3*NUMNODG)

           IF(ISPMD==0)THEN
             DO N=1,NUMNODG
               IF (NODAREAG(N) == ZERO) THEN
                FNCONTG(1,N)=ZERO
                FNCONTG(2,N)=ZERO
                FNCONTG(3,N)=ZERO
                FTCONTG(1,N)=ZERO
                FTCONTG(2,N)=ZERO
                FTCONTG(3,N)=ZERO
               ELSE
                TMP=ONE/MAX(EM30,NODAREAG(N))
                FNCONTG(1,N)=FNCONTG(1,N)*TMP
                FNCONTG(2,N)=FNCONTG(2,N)*TMP
                FNCONTG(3,N)=FNCONTG(3,N)*TMP
                FTCONTG(1,N)=FTCONTG(1,N)*TMP
                FTCONTG(2,N)=FTCONTG(2,N)*TMP
                FTCONTG(3,N)=FTCONTG(3,N)*TMP
               ENDIF
             END DO
           ELSE
             DO N=1,NUMNODG
               FNCONTG(1,N)=ZERO
               FNCONTG(2,N)=ZERO
               FNCONTG(3,N)=ZERO
               FTCONTG(1,N)=ZERO
               FTCONTG(2,N)=ZERO
               FTCONTG(3,N)=ZERO
             END DO
           END IF
           DEALLOCATE(NODAREAG)
         END IF
       ENDIF
C
      IF(ANIM_V(27)+H3D_DATA%N_VECT_PCONT2 /=0) THEN
         DO N=1,NUMNOD
           IF (NODAREA(N) == ZERO) THEN
             FNCONTP2(1,N)=ZERO
             FNCONTP2(2,N)=ZERO
             FNCONTP2(3,N)=ZERO
             FTCONTP2(1,N)=ZERO
             FTCONTP2(2,N)=ZERO
             FTCONTP2(3,N)=ZERO
           ELSE
             NODAREA(N)=ONE/MAX(EM30,NODAREA(N))
             FNCONTP2(1,N)=NODAREA(N)*FNCONTP2(1,N)
             FNCONTP2(2,N)=NODAREA(N)*FNCONTP2(2,N)
             FNCONTP2(3,N)=NODAREA(N)*FNCONTP2(3,N)
             FTCONTP2(1,N)=NODAREA(N)*FTCONTP2(1,N)
             FTCONTP2(2,N)=NODAREA(N)*FTCONTP2(2,N)
             FTCONTP2(3,N)=NODAREA(N)*FTCONTP2(3,N)
           ENDIF
         END DO
       ENDIF
C
      DEALLOCATE(NODAREA)
C
      RETURN
      END

