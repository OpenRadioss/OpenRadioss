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
Chd|  ADMTHKE                       source/model/remesh/admthke.F 
Chd|-- called by -----------
Chd|        ADMERR                        source/model/remesh/admerr.F  
Chd|        ERR_THK                       source/elements/shell/err_thk.F
Chd|-- calls ---------------
Chd|        SPMD_EXCH_NODAREA             source/mpi/anim/spmd_exch_nodarea.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        REMESH_MOD                    share/modules/remesh_mod.F    
Chd|====================================================================
      SUBROUTINE ADMTHKE(
     .       IXC     ,IXTG    ,X       ,IPARG   ,ELBUF_TAB,
     .       IPART   ,IPARTC  ,IPARTTG ,IAD_ELEM,FR_ELEM ,
     .       WEIGHT  ,AREA_SH4,AREA_SH3,AREA_NOD,THICK_SH4 ,
     .       THICK_SH3 ,THICK_NOD , ERR_THK_SH4, ERR_THK_SH3,
     .       SH4TREE ,SH3TREE )
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "vect01_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER  
     .   IXC(NIXC,*), IXTG(NIXTG,*), IPARG(NPARG,*),
     .   IPART(LIPART1,*), IPARTC(*), IPARTTG(*),
     .   IAD_ELEM(2,*), FR_ELEM(*), WEIGHT(*),
     .   SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*)
      my_real
     .   X(3,*), AREA_SH4(*), AREA_SH3(*), AREA_NOD(*),
     .   THICK_SH4(*), THICK_SH3(*), THICK_NOD(*),
     .   ERR_THK_SH4(*), ERR_THK_SH3(*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N1,N2,N3,N4,
     .        I,N,NG,NEL,LENR,
     .        NN,LEVEL,MY_LEVEL,M,SON,LL,M1,M2,M3,M4,MC
C     REAL
       my_real
     .     AREA, A, AT, THK, 
     .     X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,
     .     X31,Y31,Z31,X42,Y42,Z42,X32,Y32,Z32,E3X,E3Y,E3Z
       my_real
     .     TN1,TN2,TN3,TN4,TPG1,TPG2,TPG3,TPG4,UNT
      TYPE(G_BUFEL_) ,POINTER :: GBUF
C-----------------------------------------------
C     Retrieve Thickness on active and secnd levels
C-----------------------------------------------
      DO LEVEL=0,LEVELMAX-1

        DO NN=PSH4KIN(LEVEL)+ 1,PSH4KIN(LEVEL+1)

          N    =LSH4KIN(NN)

          IF(SH4TREE(3,N)>=0)THEN

            NG    =SH4TREE(4,N)
            NEL   =IPARG(2,NG)
            NFT   =IPARG(3,NG)
            LFT=1
            LLT=MIN(NVSIZ,NEL)
            GBUF => ELBUF_TAB(NG)%GBUF

            I=N-NFT
            IF (GBUF%OFF(I) == ZERO) THEN
              THK=ZERO
            ELSE
              THK = GBUF%THK(I)
            END IF
            THICK_SH4(N)=THK

          END IF

          THK  = THICK_SH4(N)
          SON  = SH4TREE(2,N)
          THICK_SH4(SON)  =THK
          THICK_SH4(SON+1)=THK
          THICK_SH4(SON+2)=THK
          THICK_SH4(SON+3)=THK
        END DO

      END DO
C
      LEVEL=LEVELMAX
      DO NN=PSH4KIN(LEVEL)+ 1,PSH4KIN(LEVEL+1)

        N    =LSH4KIN(NN)

        IF(SH4TREE(3,N)>=0)THEN

          NG    =SH4TREE(4,N)
          NEL   =IPARG(2,NG)
          NFT   =IPARG(3,NG)
          LFT=1
          LLT=MIN(NVSIZ,NEL)
          GBUF => ELBUF_TAB(NG)%GBUF
          I=N-NFT
c
          IF (GBUF%OFF(I) == ZERO) THEN
            THK=ZERO
          ELSE
            THK=GBUF%THK(I)
          END IF
          THICK_SH4(N)=THK

        END IF

      END DO
C
      DO LEVEL=0,LEVELMAX-1

        DO NN=PSH3KIN(LEVEL)+ 1,PSH3KIN(LEVEL+1)

          N    =LSH3KIN(NN)

          IF(SH3TREE(3,N)>=0)THEN

            NG   =SH3TREE(4,N)
            NEL   =IPARG(2,NG)
            NFT   =IPARG(3,NG)
            LFT=1
            LLT=MIN(NVSIZ,NEL)
            GBUF => ELBUF_TAB(NG)%GBUF
            I=N-NFT
c
            IF (GBUF%OFF(I) == ZERO) THEN
              THK=ZERO
            ELSE
              THK=GBUF%THK(I)
            END IF
            THICK_SH3(N)=THK

          END IF

          THK  = THICK_SH3(N)
          SON  = SH3TREE(2,N)
          THICK_SH3(SON)  =THK
          THICK_SH3(SON+1)=THK
          THICK_SH3(SON+2)=THK
          THICK_SH3(SON+3)=THK
        END DO
      END DO
C
      LEVEL=LEVELMAX
      DO NN=PSH3KIN(LEVEL)+ 1,PSH3KIN(LEVEL+1)

        N    =LSH3KIN(NN)

        IF(SH3TREE(3,N)>=0)THEN

          NG    =SH3TREE(4,N)
          NEL   =IPARG(2,NG)
          NFT   =IPARG(3,NG)
          LFT=1
          LLT=MIN(NVSIZ,NEL)
          GBUF => ELBUF_TAB(NG)%GBUF
c
          I=N-NFT
          IF (GBUF%OFF(I) == ZERO) THEN
            THK=ZERO
          ELSE
            THK=GBUF%THK(I)
          END IF
          THICK_SH3(N)=THK

        END IF

      END DO
C-----------------------------------------------
C     ... Through the maximum (finest) level ...
C-----------------------------------------------
      LEVEL=LEVELMAX
      DO NN=PSH4KIN(LEVEL)+1,PSH4KIN(LEVEL+1)

        N    =LSH4KIN(NN)

        NG   =SH4TREE(4,N)
        NEL   =IPARG(2,NG)
        NFT   =IPARG(3,NG)
        LFT=1
        LLT=MIN(NVSIZ,NEL)
        GBUF => ELBUF_TAB(NG)%GBUF
c
        I=N-NFT
        IF (GBUF%OFF(I) == ZERO) CYCLE

        N1=IXC(2,N)
        N2=IXC(3,N)
        N3=IXC(4,N)
        N4=IXC(5,N)

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

        E3X=Y31*Z42-Z31*Y42
        E3Y=Z31*X42-X31*Z42
        E3Z=X31*Y42-Y31*X42

        E3X=ONE_OVER_8*E3X
        E3Y=ONE_OVER_8*E3Y
        E3Z=ONE_OVER_8*E3Z

        AREA=SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
        AREA_SH4(N)=AREA
        AT = AREA * THICK_SH4(N)

        AREA_NOD(N1)=AREA_NOD(N1)+AREA
        AREA_NOD(N2)=AREA_NOD(N2)+AREA
        AREA_NOD(N3)=AREA_NOD(N3)+AREA
        AREA_NOD(N4)=AREA_NOD(N4)+AREA
        THICK_NOD(N1)=THICK_NOD(N1)+AT
        THICK_NOD(N2)=THICK_NOD(N2)+AT
        THICK_NOD(N3)=THICK_NOD(N3)+AT
        THICK_NOD(N4)=THICK_NOD(N4)+AT

      END DO
C
      LEVEL=LEVELMAX
      DO NN=PSH3KIN(LEVEL)+1,PSH3KIN(LEVEL+1)

        N    =LSH3KIN(NN)

        NG    =SH3TREE(4,N)
        NEL   =IPARG(2,NG)
        NFT   =IPARG(3,NG)
        LFT=1
        LLT=MIN(NVSIZ,NEL)
        GBUF => ELBUF_TAB(NG)%GBUF
c
        I=N-NFT
        IF (GBUF%OFF(I) == ZERO) CYCLE

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

        E3X=Y31*Z32-Z31*Y32
        E3Y=Z31*X32-X31*Z32
        E3Z=X31*Y32-Y31*X32
        E3X=ONE_OVER_6*E3X
        E3Y=ONE_OVER_6*E3Y
        E3Z=ONE_OVER_6*E3Z

        AREA=SQRT(E3X*E3X+E3Y*E3Y+E3Z*E3Z)
        AREA_SH3(N)=AREA
        AT=  AREA * THICK_SH3(N)

        AREA_NOD(N1)=AREA_NOD(N1)+AREA
        AREA_NOD(N2)=AREA_NOD(N2)+AREA
        AREA_NOD(N3)=AREA_NOD(N3)+AREA
        THICK_NOD(N1)=THICK_NOD(N1)+AT
        THICK_NOD(N2)=THICK_NOD(N2)+AT
        THICK_NOD(N3)=THICK_NOD(N3)+AT

      END DO
C-----------------------------------------------
C     Error for active elements at max level
C-----------------------------------------------
      DO NN=PSH4KIN(LEVEL)+1,PSH4KIN(LEVEL+1)

        N    =LSH4KIN(NN)

        IF(SH4TREE(3,N) >= 0)THEN

          N1=IXC(2,N)
          N2=IXC(3,N)
          N3=IXC(4,N)
          N4=IXC(5,N)

          UNT=ONE/THICK_SH4(N)
          TN1=ABS(THICK_NOD(N1)/MAX(EM30,AREA_NOD(N1))*UNT-ONE)
          TN2=ABS(THICK_NOD(N2)/MAX(EM30,AREA_NOD(N2))*UNT-ONE)
          TN3=ABS(THICK_NOD(N3)/MAX(EM30,AREA_NOD(N3))*UNT-ONE)
          TN4=ABS(THICK_NOD(N4)/MAX(EM30,AREA_NOD(N4))*UNT-ONE)
          ERR_THK_SH4(N)=FOURTH*(TN1+TN2+TN3+TN4)
        END IF

      END DO
C
      DO NN=PSH3KIN(LEVEL)+1,PSH3KIN(LEVEL+1)

        N    =LSH3KIN(NN)

        IF(SH3TREE(3,N) >= 0)THEN

          N1=IXTG(2,N)
          N2=IXTG(3,N)
          N3=IXTG(4,N)

          UNT=ONE/THICK_SH3(N)
          TN1=ABS(THICK_NOD(N1)/MAX(EM30,AREA_NOD(N1))*UNT-ONE)
          TN2=ABS(THICK_NOD(N2)/MAX(EM30,AREA_NOD(N2))*UNT-ONE)
          TN3=ABS(THICK_NOD(N3)/MAX(EM30,AREA_NOD(N3))*UNT-ONE)
          ERR_THK_SH3(N)=THIRD*(TN1+TN2+TN3)
        END IF

      END DO
C-----------------------------------------------
C     Error for all active elements (from bottom to top)
C-----------------------------------------------
      TAGNOD=0
      DO LEVEL=LEVELMAX-1,0,-1

        DO NN=PSH4KIN(LEVEL)+1,PSH4KIN(LEVEL+1)
          N    =LSH4KIN(NN)

          SON=SH4TREE(2,N)

          N1=IXC(2,N)
          N2=IXC(3,N)
          N3=IXC(4,N)
          N4=IXC(5,N)

          MC=IXC(4,SON)

          AREA=FOURTH*AREA_NOD(MC)
          AT  =FOURTH*THICK_NOD(MC)

          AREA_NOD(N1) =AREA_NOD(N1)+AREA
          AREA_NOD(N2) =AREA_NOD(N2)+AREA
          AREA_NOD(N3) =AREA_NOD(N3)+AREA
          AREA_NOD(N4) =AREA_NOD(N4)+AREA
          THICK_NOD(N1)=THICK_NOD(N1)+AT
          THICK_NOD(N2)=THICK_NOD(N2)+AT
          THICK_NOD(N3)=THICK_NOD(N3)+AT
          THICK_NOD(N4)=THICK_NOD(N4)+AT

          TAGNOD(MC)=1


          M1=IXC(3,SON  )
          IF(TAGNOD(M1)==0)THEN

            TAGNOD(M1)=1

            AREA=HALF*AREA_NOD(M1)
            AT  =HALF*THICK_NOD(M1)

            AREA_NOD(N1) =AREA_NOD(N1)+AREA
            AREA_NOD(N2) =AREA_NOD(N2)+AREA
            THICK_NOD(N1)=THICK_NOD(N1)+AT
            THICK_NOD(N2)=THICK_NOD(N2)+AT

          END IF

          M2=IXC(4,SON+1)
          IF(TAGNOD(M2)==0)THEN

            TAGNOD(M2)=1

            AREA=HALF*AREA_NOD(M2)
            AT  =HALF*THICK_NOD(M2)

            AREA_NOD(N2) =AREA_NOD(N2)+AREA
            AREA_NOD(N3) =AREA_NOD(N3)+AREA
            THICK_NOD(N2)=THICK_NOD(N2)+AT
            THICK_NOD(N3)=THICK_NOD(N3)+AT

          END IF

          M3=IXC(5,SON+2)
          IF(TAGNOD(M3)==0)THEN

            TAGNOD(M3)=1

            AREA=HALF*AREA_NOD(M3)
            AT  =HALF*THICK_NOD(M3)

            AREA_NOD(N3) =AREA_NOD(N3)+AREA
            AREA_NOD(N4) =AREA_NOD(N4)+AREA
            THICK_NOD(N3)=THICK_NOD(N3)+AT
            THICK_NOD(N4)=THICK_NOD(N4)+AT

          END IF

          M4=IXC(2,SON+3)
          IF(TAGNOD(M4)==0)THEN

            TAGNOD(M4)=1

            AREA=HALF*AREA_NOD(M4)
            AT  =HALF*THICK_NOD(M4)

            AREA_NOD(N4) =AREA_NOD(N4)+AREA
            AREA_NOD(N1) =AREA_NOD(N1)+AREA
            THICK_NOD(N4)=THICK_NOD(N4)+AT
            THICK_NOD(N1)=THICK_NOD(N1)+AT

          END IF

        END DO

        DO NN=PSH3KIN(LEVEL)+1,PSH3KIN(LEVEL+1)
          N    =LSH3KIN(NN)

          SON=SH3TREE(2,N)

          N1=IXTG(2,N)
          N2=IXTG(3,N)
          N3=IXTG(4,N)
C
          M1=IXTG(4,SON+3)
          IF(TAGNOD(M1)==0)THEN

            TAGNOD(M1)=1

            AREA=HALF*AREA_NOD(M1)
            AT  =HALF*THICK_NOD(M1)

            AREA_NOD(N1) =AREA_NOD(N1)+AREA
            AREA_NOD(N2) =AREA_NOD(N2)+AREA
            THICK_NOD(N1)=THICK_NOD(N1)+AT
            THICK_NOD(N2)=THICK_NOD(N2)+AT

          END IF
C
          M2=IXTG(2,SON+3)
          IF(TAGNOD(M2)==0)THEN
            TAGNOD(M2)=1

            AREA=HALF*AREA_NOD(M2)
            AT  =HALF*THICK_NOD(M2)

            AREA_NOD(N2) =AREA_NOD(N2)+AREA
            AREA_NOD(N3) =AREA_NOD(N3)+AREA
            THICK_NOD(N2)=THICK_NOD(N2)+AT
            THICK_NOD(N3)=THICK_NOD(N3)+AT

          END IF

          M3=IXTG(3,SON+3)
          IF(TAGNOD(M3)==0)THEN
            TAGNOD(M3)=1

            AREA=HALF*AREA_NOD(M3)
            AT  =HALF*THICK_NOD(M3)

            AREA_NOD(N3) =AREA_NOD(N3)+AREA
            AREA_NOD(N1) =AREA_NOD(N1)+AREA
            THICK_NOD(N3)=THICK_NOD(N3)+AT
            THICK_NOD(N1)=THICK_NOD(N1)+AT

          END IF

        END DO

        DO NN=PSH4KIN(LEVEL)+1,PSH4KIN(LEVEL+1)
          N    =LSH4KIN(NN)

          IF(SH4TREE(3,N) >= 0)THEN

            N1=IXC(2,N)
            N2=IXC(3,N)
            N3=IXC(4,N)
            N4=IXC(5,N)

            UNT=ONE/THICK_SH4(N)
            TN1=ABS(THICK_NOD(N1)/MAX(EM30,AREA_NOD(N1))*UNT-ONE)
            TN2=ABS(THICK_NOD(N2)/MAX(EM30,AREA_NOD(N2))*UNT-ONE)
            TN3=ABS(THICK_NOD(N3)/MAX(EM30,AREA_NOD(N3))*UNT-ONE)
            TN4=ABS(THICK_NOD(N4)/MAX(EM30,AREA_NOD(N4))*UNT-ONE)
            ERR_THK_SH4(N)=FOURTH*(TN1+TN2+TN3+TN4)
          END IF
        END DO

        DO NN=PSH3KIN(LEVEL)+1,PSH3KIN(LEVEL+1)
          N    =LSH3KIN(NN)

          IF(SH3TREE(3,N) >= 0)THEN

            N1=IXTG(2,N)
            N2=IXTG(3,N)
            N3=IXTG(4,N)

            UNT=ONE/THICK_SH3(N)
            TN1=ABS(THICK_NOD(N1)/MAX(EM30,AREA_NOD(N1))*UNT-ONE)
            TN2=ABS(THICK_NOD(N2)/MAX(EM30,AREA_NOD(N2))*UNT-ONE)
            TN3=ABS(THICK_NOD(N3)/MAX(EM30,AREA_NOD(N3))*UNT-ONE)
            ERR_THK_SH3(N)=THIRD*(TN1+TN2+TN3)
          END IF
        END DO

      END DO
C
      IF(IMACH==3)THEN
        LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
        CALL SPMD_EXCH_NODAREA(AREA_NOD,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
c        LENR = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
        CALL SPMD_EXCH_NODAREA(THICK_NOD,IAD_ELEM,FR_ELEM,LENR,WEIGHT)
      END IF

      RETURN
      END
