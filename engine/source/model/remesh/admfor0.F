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
Chd|  ADMFOR0                       source/model/remesh/admfor0.F 
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        REMESH_MOD                    share/modules/remesh_mod.F    
Chd|====================================================================
      SUBROUTINE ADMFOR0(IXC ,IPARTC,IXTG  ,IPARTTG,IPART ,
     .                 A     ,STIFN ,AR     ,STIFR ,X     ,
     .                 SH4TREE,SH3TREE,STCONT,FTHE ,CONDN )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE REMESH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*), IPARTC(*), IXTG(NIXTG,*), IPARTTG(*),
     .        IPART(LIPART1,*), SH4TREE(KSH4TREE,*), SH3TREE(KSH3TREE,*)
       my_real
     .        A(3,*), STIFN(*), AR(3,*), STIFR(*), X(3,*),
     .        STCONT(*), FTHE(*),CONDN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER KN, KN1, KN2, KN3, KN4
      INTEGER N, NN, LEVEL, IP, NLEV
      INTEGER SON,M1,M2,M3,M4,MC,N1,N2,N3,N4,J,K
      INTEGER I,LLNOD,LNOD(NUMNOD),
     .        LE,LELT,NELT(2*(4**LEVELMAX)),LEV,NE,LELT1,LELT2,
     .        NI,LL, LKINNOD(NUMNOD)
      my_real
     .        A1,A2,A3,A4,AC,
     .        PHI,FACM,FACI,
     .        RNOD(NUMNOD),SNOD(NUMNOD),R,S
C-----------------------------------------------
      LKINNOD=0
      DO LEVEL=LEVELMAX-1,0,-1

        DO NN=PSH4KIN(LEVEL)+1,PSH4KIN(LEVEL+1)
          N    =LSH4KIN(NN)

          SON=SH4TREE(2,N)

          N1=IXC(2,N)
          N2=IXC(3,N)
          N3=IXC(4,N)
          N4=IXC(5,N)
C
          MC=IXC(4,SON)
          DO J=1,3
            AC= FOURTH*A(J,MC)
            A(J,N1)=A(J,N1)+AC
            A(J,N2)=A(J,N2)+AC
            A(J,N3)=A(J,N3)+AC
            A(J,N4)=A(J,N4)+AC
          END DO
          AC=FOURTH*STIFN(MC)
          STIFN(N1)=STIFN(N1)+AC
          STIFN(N2)=STIFN(N2)+AC
          STIFN(N3)=STIFN(N3)+AC
          STIFN(N4)=STIFN(N4)+AC
          IF(ISTATCND/=0)THEN
            AC=FOURTH*STCONT(MC)
            STCONT(N1)=STCONT(N1)+AC
            STCONT(N2)=STCONT(N2)+AC
            STCONT(N3)=STCONT(N3)+AC
            STCONT(N4)=STCONT(N4)+AC
          END IF

          DO J=1,3
            AC= FOURTH*AR(J,MC)
            AR(J,N1)=AR(J,N1)+AC
            AR(J,N2)=AR(J,N2)+AC
            AR(J,N3)=AR(J,N3)+AC
            AR(J,N4)=AR(J,N4)+AC
          END DO	
          AC=FOURTH*STIFR(MC)
          STIFR(N1)=STIFR(N1)+AC
          STIFR(N2)=STIFR(N2)+AC
          STIFR(N3)=STIFR(N3)+AC
          STIFR(N4)=STIFR(N4)+AC

          IF(ITHERM_FE > 0)THEN
            AC= FOURTH*FTHE(MC)
            FTHE(N1)=FTHE(N1)+AC
            FTHE(N2)=FTHE(N2)+AC
            FTHE(N3)=FTHE(N3)+AC
            FTHE(N4)=FTHE(N4)+AC
          END IF

          IF(NODADT_THERM > 0)THEN
            AC= FOURTH*CONDN(MC)
            CONDN(N1)=CONDN(N1)+AC
            CONDN(N2)=CONDN(N2)+AC
            CONDN(N3)=CONDN(N3)+AC
            CONDN(N4)=CONDN(N4)+AC
          END IF

          LKINNOD(MC)=1
          STIFN(MC)=EM20
          STIFR(MC)=EM20
C
          M1=IXC(3,SON  )
          IF(LKINNOD(M1)==0)THEN
          LKINNOD(M1)=1
          DO J=1,3
            A1=HALF*A(J,M1)
            A(J,N1)=A(J,N1)+A1
            A(J,N2)=A(J,N2)+A1
          END DO
          A1=HALF*STIFN(M1)
          STIFN(N1)=STIFN(N1)+A1
          STIFN(N2)=STIFN(N2)+A1

          IF(ISTATCND/=0)THEN
            A1=HALF*STCONT(M1)
            STCONT(N1)=STCONT(N1)+A1
            STCONT(N2)=STCONT(N2)+A1
          END IF

	  DO J=1,3
	    A1=HALF*AR(J,M1)
	    AR(J,N1)=AR(J,N1)+A1
	    AR(J,N2)=AR(J,N2)+A1
	  END DO
	  A1=HALF*STIFR(M1)
	  STIFR(N1)=STIFR(N1)+A1
	  STIFR(N2)=STIFR(N2)+A1

          IF(ITHERM_FE > 0)THEN
            A1= HALF*FTHE(M1)
            FTHE(N1)=FTHE(N1)+A1
            FTHE(N2)=FTHE(N2)+A1
          END IF

          IF(NODADT_THERM > 0)THEN
            A1= HALF*CONDN(M1)
            CONDN(N1)=CONDN(N1)+A1
            CONDN(N2)=CONDN(N2)+A1
          END IF

          STIFN(M1)=EM20
          STIFR(M1)=EM20
          END IF
C
          M2=IXC(4,SON+1)
          IF(LKINNOD(M2)==0)THEN
          LKINNOD(M2)=1
          DO J=1,3
            A2=HALF*A(J,M2)
            A(J,N2)=A(J,N2)+A2
            A(J,N3)=A(J,N3)+A2
          END DO
          A2=HALF*STIFN(M2)
          STIFN(N2)=STIFN(N2)+A2
          STIFN(N3)=STIFN(N3)+A2

          IF(ISTATCND/=0)THEN
            A2=HALF*STCONT(M2)
            STCONT(N2)=STCONT(N2)+A2
            STCONT(N3)=STCONT(N3)+A2
          END IF

	  DO J=1,3
	    A2=HALF*AR(J,M2)
            AR(J,N2)=AR(J,N2)+A2
            AR(J,N3)=AR(J,N3)+A2
	  END DO
	  A2=HALF*STIFR(M2)
	  STIFR(N2)=STIFR(N2)+A2
	  STIFR(N3)=STIFR(N3)+A2

          IF(ITHERM_FE > 0)THEN
            A2= HALF*FTHE(M2)
            FTHE(N2)=FTHE(N2)+A2
            FTHE(N3)=FTHE(N3)+A2
          END IF

          IF(NODADT_THERM > 0)THEN
            A2= HALF*CONDN(M2)
            CONDN(N2)=CONDN(N2)+A2
            CONDN(N3)=CONDN(N3)+A2
          END IF

          STIFN(M2)=EM20
          STIFR(M2)=EM20
          END IF

          M3=IXC(5,SON+2)
          IF(LKINNOD(M3)==0)THEN
          LKINNOD(M3)=1
          DO J=1,3
            A3=HALF*A(J,M3)
            A(J,N3)=A(J,N3)+A3
            A(J,N4)=A(J,N4)+A3
          END DO
          A3=HALF*STIFN(M3)
          STIFN(N3)=STIFN(N3)+A3
          STIFN(N4)=STIFN(N4)+A3

          IF(ISTATCND/=0)THEN
            A3=HALF*STCONT(M3)
            STCONT(N3)=STCONT(N3)+A3
            STCONT(N4)=STCONT(N4)+A3
          END IF

	  DO J=1,3
	    A3=HALF*AR(J,M3)
            AR(J,N3)=AR(J,N3)+A3
            AR(J,N4)=AR(J,N4)+A3
	  END DO
   	  A3=HALF*STIFR(M3)
          STIFR(N3)=STIFR(N3)+A3
          STIFR(N4)=STIFR(N4)+A3

          IF(ITHERM_FE > 0)THEN
              A3= HALF*FTHE(M3)
              FTHE(N3)=FTHE(N3)+A3
              FTHE(N4)=FTHE(N4)+A3
          END IF

          IF(NODADT_THERM  > 0)THEN
              A3= HALF*CONDN(M3)
              CONDN(N3)=CONDN(N3)+A3
              CONDN(N4)=CONDN(N4)+A3
          END IF

          STIFN(M3)=EM20
          STIFR(M3)=EM20
          END IF
C
          M4=IXC(2,SON+3)
          IF(LKINNOD(M4)==0)THEN
          LKINNOD(M4)=1
          DO J=1,3
            A4=HALF*A(J,M4)
            A(J,N1)=A(J,N1)+A4
            A(J,N4)=A(J,N4)+A4
          END DO
          A4=HALF*STIFN(M4)
          STIFN(N1)=STIFN(N1)+A4
          STIFN(N4)=STIFN(N4)+A4

          IF(ISTATCND/=0)THEN
            A4=HALF*STCONT(M4)
            STCONT(N1)=STCONT(N1)+A4
            STCONT(N4)=STCONT(N4)+A4
          END IF

	  DO J=1,3
	    A4=HALF*AR(J,M4)
            AR(J,N1)=AR(J,N1)+A4
            AR(J,N4)=AR(J,N4)+A4
	  END DO
 	  A4=HALF*STIFR(M4)
          STIFR(N1)=STIFR(N1)+A4
          STIFR(N4)=STIFR(N4)+A4

          IF(ITHERM_FE > 0)THEN
            A4= HALF*FTHE(M4)
            FTHE(N1)=FTHE(N1)+A4
            FTHE(N4)=FTHE(N4)+A4
          END IF

          IF(NODADT_THERM > 0)THEN
            A4= HALF*CONDN(M4)
            CONDN(N1)=CONDN(N1)+A4
            CONDN(N4)=CONDN(N4)+A4
          END IF

          STIFN(M4)=EM20
          STIFR(M4)=EM20
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
          IF(LKINNOD(M1)==0)THEN
          LKINNOD(M1)=1
          DO J=1,3
            A1=HALF*A(J,M1)
            A(J,N1)=A(J,N1)+A1
            A(J,N2)=A(J,N2)+A1
          END DO
          A1=HALF*STIFN(M1)
          STIFN(N1)=STIFN(N1)+A1
          STIFN(N2)=STIFN(N2)+A1

          IF(ISTATCND/=0)THEN
            A1=HALF*STCONT(M1)
            STCONT(N1)=STCONT(N1)+A1
            STCONT(N2)=STCONT(N2)+A1
          END IF

	  DO J=1,3
	    A1=HALF*AR(J,M1)
	    AR(J,N1)=AR(J,N1)+A1
	    AR(J,N2)=AR(J,N2)+A1
	  END DO
	  A1=HALF*STIFR(M1)
	  STIFR(N1)=STIFR(N1)+A1
	  STIFR(N2)=STIFR(N2)+A1

          IF(ITHERM_FE > 0)THEN
            A1= HALF*FTHE(M1)
            FTHE(N1)=FTHE(N1)+A1
            FTHE(N2)=FTHE(N2)+A1
          END IF

          IF(NODADT_THERM > 0)THEN
            A1= HALF*CONDN(M1)
            CONDN(N1)=CONDN(N1)+A1
            CONDN(N2)=CONDN(N2)+A1
          END IF

          STIFN(M1)=EM20
          STIFR(M1)=EM20
          END IF
C
          M2=IXTG(2,SON+3)
          IF(LKINNOD(M2)==0)THEN
          LKINNOD(M2)=1
          DO J=1,3
            A2=HALF*A(J,M2)
            A(J,N2)=A(J,N2)+A2
            A(J,N3)=A(J,N3)+A2
          END DO
          A2=HALF*STIFN(M2)
          STIFN(N2)=STIFN(N2)+A2
          STIFN(N3)=STIFN(N3)+A2

          IF(ISTATCND/=0)THEN
            A2=HALF*STCONT(M2)
            STCONT(N2)=STCONT(N2)+A2
            STCONT(N3)=STCONT(N3)+A2
          END IF

	  DO J=1,3
	    A2=HALF*AR(J,M2)
            AR(J,N2)=AR(J,N2)+A2
            AR(J,N3)=AR(J,N3)+A2
	  END DO
	  A2=HALF*STIFR(M2)
	  STIFR(N2)=STIFR(N2)+A2
	  STIFR(N3)=STIFR(N3)+A2

          IF(ITHERM_FE > 0)THEN
            A2= HALF*FTHE(M2)
            FTHE(N2)=FTHE(N2)+A2
            FTHE(N3)=FTHE(N3)+A2
          END IF

          IF(NODADT_THERM > 0)THEN
            A2= HALF*CONDN(M2)
            CONDN(N2)=CONDN(N2)+A2
            CONDN(N3)=CONDN(N3)+A2
          END IF

          STIFN(M2)=EM20
          STIFR(M2)=EM20
          END IF

          M3=IXTG(3,SON+3)
          IF(LKINNOD(M3)==0)THEN
          LKINNOD(M3)=1
          DO J=1,3
            A3=HALF*A(J,M3)
            A(J,N3)=A(J,N3)+A3
            A(J,N1)=A(J,N1)+A3
          END DO
          A3=HALF*STIFN(M3)
          STIFN(N3)=STIFN(N3)+A3
          STIFN(N1)=STIFN(N1)+A3

          IF(ISTATCND/=0)THEN
            A3=HALF*STCONT(M3)
            STCONT(N3)=STCONT(N3)+A3
            STCONT(N1)=STCONT(N1)+A3
          END IF

	  DO J=1,3
	    A3=HALF*AR(J,M3)
            AR(J,N3)=AR(J,N3)+A3
            AR(J,N1)=AR(J,N1)+A3
	  END DO
   	  A3=HALF*STIFR(M3)
          STIFR(N3)=STIFR(N3)+A3
          STIFR(N1)=STIFR(N1)+A3

          IF(ITHERM_FE > 0)THEN
            A3= HALF*FTHE(M3)
            FTHE(N3)=FTHE(N3)+A3
            FTHE(N1)=FTHE(N1)+A3
          END IF

          IF(NODADT_THERM > 0)THEN
            A3= HALF*CONDN(M3)
            CONDN(N3)=CONDN(N3)+A3
            CONDN(N1)=CONDN(N1)+A3
          END IF

          STIFN(M3)=EM20
          STIFR(M3)=EM20
          END IF

        END DO

      END DO
C-----
      IF(ISTATCND==0) RETURN

      TAGNOD=0
C     Store forces.
      ACND (1:3,1:NUMNOD)=A (1:3,1:NUMNOD)
      ARCND(1:3,1:NUMNOD)=AR(1:3,1:NUMNOD)

      LL=PSH4UPL(1)
      DO NN=1,LL
        N    =LSH4UPL(NN)
C
        N1=IXC(2,N)
        N2=IXC(3,N)
        N3=IXC(4,N)
        N4=IXC(5,N)
C
C-------
        RNOD(N1)=-ONE
        SNOD(N1)=-ONE
        RNOD(N2)= ONE
        SNOD(N2)=-ONE
        RNOD(N3)= ONE
        SNOD(N3)= ONE
        RNOD(N4)=-ONE
        SNOD(N4)= ONE
C
C-------
        LELT   =1
        NELT(1)=N

        LELT1  =0
        LELT2  =1

        LEV=0

        LLNOD=0
        DO WHILE (LEV < LEVELMAX)
          DO LE=LELT1+1,LELT2

            NE =NELT(LE)
            IF(SH4TREE(3,NE) >= 0) CYCLE

            M1=IXC(2,NE)
            M2=IXC(3,NE)
            M3=IXC(4,NE)
            M4=IXC(5,NE)

            SON=SH4TREE(2,NE)

            LELT=LELT+1
            NELT(LELT)=SON

            LELT=LELT+1
            NELT(LELT)=SON+1

            LELT=LELT+1
            NELT(LELT)=SON+2

            LELT=LELT+1
            NELT(LELT)=SON+3

            NI=IXC(3,SON)
            IF(LKINNOD(NI)==0.AND.TAGNOD(NI)==0)THEN
C
C             nodes w/kinematic condition are not condensed
              TAGNOD(NI)=1
              LLNOD=LLNOD+1
              LNOD(LLNOD)=NI
            END IF
            RNOD(NI)=HALF*(RNOD(M1)+RNOD(M2))
            SNOD(NI)=HALF*(SNOD(M1)+SNOD(M2))

            NI=IXC(4,SON+1)
            IF(LKINNOD(NI)==0.AND.TAGNOD(NI)==0)THEN
              TAGNOD(NI)=1
              LLNOD=LLNOD+1
              LNOD(LLNOD)=NI
            END IF
            RNOD(NI)=HALF*(RNOD(M2)+RNOD(M3))
            SNOD(NI)=HALF*(SNOD(M2)+SNOD(M3))

            NI=IXC(5,SON+2)
            IF(LKINNOD(NI)==0.AND.TAGNOD(NI)==0)THEN
              TAGNOD(NI)=1
              LLNOD=LLNOD+1
              LNOD(LLNOD)=NI
            END IF
            RNOD(NI)=HALF*(RNOD(M3)+RNOD(M4))
            SNOD(NI)=HALF*(SNOD(M3)+SNOD(M4))

            NI=IXC(2,SON+3)
            IF(LKINNOD(NI)==0.AND.TAGNOD(NI)==0)THEN
              TAGNOD(NI)=1
              LLNOD=LLNOD+1
              LNOD(LLNOD)=NI
            END IF
            RNOD(NI)=HALF*(RNOD(M4)+RNOD(M1))
            SNOD(NI)=HALF*(SNOD(M4)+SNOD(M1))

            NI=IXC(4,SON)
            IF(LKINNOD(NI)==0)THEN
              TAGNOD(NI)=1
              LLNOD=LLNOD+1
              LNOD(LLNOD)=NI
            END IF
            RNOD(NI)=FOURTH*(RNOD(M1)+RNOD(M2)+RNOD(M3)+RNOD(M4))
            SNOD(NI)=FOURTH*(SNOD(M1)+SNOD(M2)+SNOD(M3)+SNOD(M4))

          END DO

          LEV   =LEV+1
          LELT1 =LELT2
          LELT2 =LELT

        END DO
C
C-------
        DO I=1,LLNOD
          NI=LNOD(I)
          R =RNOD(NI)
          S =SNOD(NI)
          PHI =FOURTH*(ONE-R)*(ONE-S)
          DO J=1,3
            AC= PHI*A(J,NI)
            A(J,N1)=A(J,N1)+AC
          END DO
          STIFN(N1)=STIFN(N1)+PHI*STCONT(NI)
          DO J=1,3
            AC= PHI*AR(J,NI)
            AR(J,N1)=AR(J,N1)+AC
          END DO
          PHI=FOURTH*(ONE+R)*(ONE-S)
          DO J=1,3
            AC= PHI*A(J,NI)
            A(J,N2)=A(J,N2)+AC
          END DO
          STIFN(N2)=STIFN(N2)+PHI*STCONT(NI)
          DO J=1,3
            AC= PHI*AR(J,NI)
            AR(J,N2)=AR(J,N2)+AC
          END DO
          PHI=FOURTH*(ONE+R)*(ONE+S)
          DO J=1,3
            AC= PHI*A(J,NI)
            A(J,N3)=A(J,N3)+AC
          END DO
          STIFN(N3)=STIFN(N3)+PHI*STCONT(NI)
          DO J=1,3
            AC= PHI*AR(J,NI)
            AR(J,N3)=AR(J,N3)+AC
          END DO
          PHI=FOURTH*(ONE-R)*(ONE+S)
          DO J=1,3
            AC= PHI*A(J,NI)
            A(J,N4)=A(J,N4)+AC
          END DO
          STIFN(N4)=STIFN(N4)+PHI*STCONT(NI)
          DO J=1,3
            AC= PHI*AR(J,NI)
            AR(J,N4)=AR(J,N4)+AC
          END DO
        END DO


      END DO
C
C-----

      LL=PSH3UPL(1)
      DO NN=1,LL
        N    =LSH3UPL(NN)
C
        N1=IXTG(2,N)
        N2=IXTG(3,N)
        N3=IXTG(4,N)
C
C-------
        RNOD(N1)= ZERO
        SNOD(N1)= ZERO
        RNOD(N2)= ONE
        SNOD(N2)= ZERO
        RNOD(N3)= ZERO
        SNOD(N3)= ONE
C
C-------
        LELT   =1
        NELT(1)=N

        LELT1  =0
        LELT2  =1

        LEV=0

        LLNOD=0
        DO WHILE (LEV < LEVELMAX)
          DO LE=LELT1+1,LELT2

            NE =NELT(LE)
            IF(SH3TREE(3,NE) >= 0) CYCLE

            M1=IXTG(2,NE)
            M2=IXTG(3,NE)
            M3=IXTG(4,NE)

            SON=SH3TREE(2,NE)

            LELT=LELT+1
            NELT(LELT)=SON

            LELT=LELT+1
            NELT(LELT)=SON+1

            LELT=LELT+1
            NELT(LELT)=SON+2

            LELT=LELT+1
            NELT(LELT)=SON+3

            NI=IXTG(4,SON+3)
            IF(LKINNOD(NI)==0.AND.TAGNOD(NI)==0)THEN
              TAGNOD(NI)=1
              LLNOD=LLNOD+1
              LNOD(LLNOD)=NI
            END IF
            RNOD(NI)=HALF*(RNOD(M1)+RNOD(M2))
            SNOD(NI)=HALF*(SNOD(M1)+SNOD(M2))

            NI=IXTG(2,SON+3)
            IF(LKINNOD(NI)==0.AND.TAGNOD(NI)==0)THEN
              TAGNOD(NI)=1
              LLNOD=LLNOD+1
              LNOD(LLNOD)=NI
            END IF
            RNOD(NI)=HALF*(RNOD(M2)+RNOD(M3))
            SNOD(NI)=HALF*(SNOD(M2)+SNOD(M3))

            NI=IXTG(3,SON+3)
            IF(LKINNOD(NI)==0.AND.TAGNOD(NI)==0)THEN
              TAGNOD(NI)=1
              LLNOD=LLNOD+1
              LNOD(LLNOD)=NI
            END IF
            RNOD(NI)=HALF*(RNOD(M3)+RNOD(M1))
            SNOD(NI)=HALF*(SNOD(M3)+SNOD(M1))

          END DO

          LEV   =LEV+1
          LELT1 =LELT2
          LELT2 =LELT

        END DO
C
C-------
        DO I=1,LLNOD
          NI=LNOD(I)
          R =RNOD(NI)
          S =SNOD(NI)
          PHI =ONE-R-S
          DO J=1,3
            AC= PHI*A(J,NI)
            A(J,N1)=A(J,N1)+AC
          END DO
          STIFN(N1)=STIFN(N1)+PHI*STCONT(NI)
          DO J=1,3
            AC= PHI*AR(J,NI)
            AR(J,N1)=AR(J,N1)+AC
          END DO
          PHI=R
          DO J=1,3
            AC= PHI*A(J,NI)
            A(J,N2)=A(J,N2)+AC
          END DO
          STIFN(N2)=STIFN(N2)+PHI*STCONT(NI)
          DO J=1,3
            AC= PHI*AR(J,NI)
            AR(J,N2)=AR(J,N2)+AC
          END DO
          PHI=S
          DO J=1,3
            AC= PHI*A(J,NI)
            A(J,N3)=A(J,N3)+AC
          END DO
          STIFN(N3)=STIFN(N3)+PHI*STCONT(NI)
          DO J=1,3
            AC= PHI*AR(J,NI)
            AR(J,N3)=AR(J,N3)+AC
          END DO
        END DO


      END DO
C
C-----
      RETURN
      END
