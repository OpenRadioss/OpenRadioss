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
Chd|  S10LEN3                       source/elements/solid/solide10/s10len3.F
Chd|-- called by -----------
Chd|        INIRIG_MAT                    source/elements/initia/inirig_mat.F
Chd|        S10INIT3                      source/elements/solid/solide10/s10init3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10LEN3(VOL,NGL,DELTAX,DELTAX2,
     .   PX,  PY,  PZ,VOLU,VOLN,VOLG,
     .   RX,  RY,  RZ,  SX,  SY,  SZ , TX, TY, TZ,
     .   NEL,MXT,PM ,V_PITER,IINT )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8 
      INTEGER NEL,MXT(MVSIZ),IINT
      my_real
     .   VOL(MVSIZ,5),DELTAX(*),DELTAX2(*),
     .   RX(*), RY(*), RZ(*), SX(*), SY(*), SZ(*), TX(*),TY(*),TZ(*),
     .   VOLU(*),VOLN(*),VOLG(MVSIZ),
     .   PX(MVSIZ,10,5),PY(MVSIZ,10,5),PZ(MVSIZ,10,5), PM(NPROPM,*),
     .   V_PITER(NEL,3,10)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I,IP,N,M,J,K
      INTEGER ITER,NITER,INIT_PITER,NITERX,MAXITER
      INTEGER NINDX, NINDX2, INDEX(MVSIZ)
      my_real
     .   UL(MVSIZ,10),
     .   PXX(MVSIZ),PYY(MVSIZ),PZZ(MVSIZ),PXY(MVSIZ),PYZ(MVSIZ),PXZ(MVSIZ),
     .   QX(MVSIZ,10),QY(MVSIZ,10),QZ(MVSIZ,10)
      my_real
     .   D(MVSIZ),GFAC,BFAC,LD,P,MASS,MREF,FAC,
     .   AA,BB,A1,A2,A3,A4,
     .   A1X,A2X,A3X,A4X,A1Y,A2Y,A3Y,A4Y,A1Z,A2Z,A3Z,A4Z,AA0
      my_real
     .   UX(MVSIZ,10), UY(MVSIZ,10), UZ(MVSIZ,10), VX(MVSIZ,10), VY(MVSIZ,10), VZ(MVSIZ,10), 
     .   NV(MVSIZ), NVOLD(MVSIZ), LL, BU(MVSIZ,6), NINV(MVSIZ)
      my_real
     .   B1(MVSIZ), B2(MVSIZ), B3(MVSIZ), BB4(MVSIZ), 
     .   C1(MVSIZ), C2(MVSIZ), C3(MVSIZ), C4(MVSIZ),
     .   D1(MVSIZ), D2(MVSIZ), D3(MVSIZ), D4(MVSIZ),
     .   P4X1(MVSIZ), P4X2(MVSIZ), P4X3(MVSIZ), P4X4(MVSIZ),  
     .   P4Y1(MVSIZ), P4Y2(MVSIZ), P4Y3(MVSIZ), P4Y4(MVSIZ),  
     .   P4Z1(MVSIZ), P4Z2(MVSIZ), P4Z3(MVSIZ), P4Z4(MVSIZ),
     .   DET6,DD
      INTEGER ILEAT
      my_real 
     .        ALEAT, NN, WX(10), WY(10), WZ(10)
      DATA MAXITER/10/
C-----------------------------------------------
      FAC     = ONE/(NINE+THIRD) ! cf FACIROT == NINE+THIRD 
C      
      IF(IDT1TET10/=0 .AND. ISROT==0)THEN

        NITER = IDT1TET10-1
        IF(NITER==0)THEN
          IF(IFORMDT==0)THEN

            DO I=LFT,LLT
             PXX(I)=ZERO
             PYY(I)=ZERO
             PZZ(I)=ZERO
C            PXY(I)=ZERO
C            PXZ(I)=ZERO
C            PYZ(I)=ZERO
            END DO
            DO IP=1,NPT
              DO N=1,4
         	DO I=LFT,LLT
         	  PXX(I) =PXX(I) + VOL(I,IP)*PX(I,N,IP)*PX(I,N,IP)
         	  PYY(I) =PYY(I) + VOL(I,IP)*PY(I,N,IP)*PY(I,N,IP)
         	  PZZ(I) =PZZ(I) + VOL(I,IP)*PZ(I,N,IP)*PZ(I,N,IP)
C        	  PXY(I) =PXY(I) + VOL(I,IP)*PX(I,N,IP)*PY(I,N,IP)
C        	  PXZ(I) =PXZ(I) + VOL(I,IP)*PX(I,N,IP)*PZ(I,N,IP)
C        	  PYZ(I) =PYZ(I) + VOL(I,IP)*PY(I,N,IP)*PZ(I,N,IP)
         	ENDDO
              ENDDO
              DO N=5,10
         	DO I=LFT,LLT
         	  PXX(I) =PXX(I) + TRHEE_OVER_14*VOL(I,IP)*PX(I,N,IP)*PX(I,N,IP)
         	  PYY(I) =PYY(I) + TRHEE_OVER_14*VOL(I,IP)*PY(I,N,IP)*PY(I,N,IP)
         	  PZZ(I) =PZZ(I) + TRHEE_OVER_14*VOL(I,IP)*PZ(I,N,IP)*PZ(I,N,IP)
C        	  PXY(I) =PXY(I) + TRHEE_OVER_14*VOL(I,IP)*PX(I,N,IP)*PY(I,N,IP)
C        	  PXZ(I) =PXZ(I) + TRHEE_OVER_14*VOL(I,IP)*PX(I,N,IP)*PZ(I,N,IP)
C        	  PYZ(I) =PYZ(I) + TRHEE_OVER_14*VOL(I,IP)*PY(I,N,IP)*PZ(I,N,IP)
         	ENDDO
              ENDDO
            ENDDO
            DO I=LFT,LLT
              D(I) = PXX(I)+PYY(I)+PZZ(I)
            ENDDO

          ELSEIF(IFORMDT==1)THEN

            D(LFT:LLT)=ZERO

            GFAC=PM(105,MXT(1))
            LD  =TWO*SQRT(MAX(ONE-GFAC,ZERO))+ONE

            DO IP=1,NPT

              DO I=LFT,LLT
         	PXX(I)=PX(I,1,IP)*PX(I,1,IP)+PX(I,2,IP)*PX(I,2,IP)+PX(I,3,IP)*PX(I,3,IP)+PX(I,4,IP)*PX(I,4,IP)+
     .   	       TRHEE_OVER_14*(PX(I,5,IP)*PX(I,5,IP)+PX(I,6,IP)*PX(I,6,IP)+PX(I,7,IP)*PX(I,7,IP)+
     .   			  PX(I,8,IP)*PX(I,8,IP)+PX(I,9,IP)*PX(I,9,IP)+PX(I,10,IP)*PX(I,10,IP))
         	PYY(I)=PY(I,1,IP)*PY(I,1,IP)+PY(I,2,IP)*PY(I,2,IP)+PY(I,3,IP)*PY(I,3,IP)+PY(I,4,IP)*PY(I,4,IP)+
     .   	       TRHEE_OVER_14*(PY(I,5,IP)*PY(I,5,IP) +PY(I,6,IP)*PY(I,6,IP)+PY(I,7,IP)*PY(I,7,IP)+
     .   			  PY(I,8,IP)*PY(I,8,IP)+PY(I,9,IP)*PY(I,9,IP)+PY(I,10,IP)*PY(I,10,IP))
         	PZZ(I)=PZ(I,1,IP)*PZ(I,1,IP)+PZ(I,2,IP)*PZ(I,2,IP)+PZ(I,3,IP)*PZ(I,3,IP)+PZ(I,4,IP)*PZ(I,4,IP)+
     .   	       TRHEE_OVER_14*(PZ(I,5,IP)*PZ(I,5,IP)+PZ(I,6,IP)*PZ(I,6,IP)+PZ(I,7,IP)*PZ(I,7,IP)+
     .   			  PZ(I,8,IP)*PZ(I,8,IP)+PZ(I,9,IP)*PZ(I,9,IP)+PZ(I,10,IP)*PZ(I,10,IP))
         	PXY(I)=PX(I,1,IP)*PY(I,1,IP)+PX(I,2,IP)*PY(I,2,IP)+PX(I,3,IP)*PY(I,3,IP)+PX(I,4,IP)*PY(I,4,IP)+
     .   	       TRHEE_OVER_14*(PX(I,5,IP)*PY(I,5,IP)+PX(I,6,IP)*PY(I,6,IP)+PX(I,7,IP)*PY(I,7,IP)+
     .   			  PX(I,8,IP)*PY(I,8,IP)+PX(I,9,IP)*PY(I,9,IP)+PX(I,10,IP)*PY(I,10,IP)) 
         	PXZ(I)=PX(I,1,IP)*PZ(I,1,IP)+PX(I,2,IP)*PZ(I,2,IP)+PX(I,3,IP)*PZ(I,3,IP)+PX(I,4,IP)*PZ(I,4,IP)+ 
     .   	       TRHEE_OVER_14*(PX(I,5,IP)*PZ(I,5,IP)+PX(I,6,IP)*PZ(I,6,IP)+PX(I,7,IP)*PZ(I,7,IP)+
     .   			  PX(I,8,IP)*PZ(I,8,IP)+PX(I,9,IP)*PZ(I,9,IP)+PX(I,10,IP)*PZ(I,10,IP)) 
         	PYZ(I)=PY(I,1,IP)*PZ(I,1,IP)+PY(I,2,IP)*PZ(I,2,IP)+PY(I,3,IP)*PZ(I,3,IP)+PY(I,4,IP)*PZ(I,4,IP)+
     .   	       TRHEE_OVER_14*(PY(I,5,IP)*PZ(I,5,IP)+PY(I,6,IP)*PZ(I,6,IP)+PY(I,7,IP)*PZ(I,7,IP)+
     .   			  PY(I,8,IP)*PZ(I,8,IP)+PY(I,9,IP)*PZ(I,9,IP)+PY(I,10,IP)*PZ(I,10,IP)) 
              ENDDO

              DO I=LFT,LLT
         	AA = -(PXX(I)+PYY(I)+PZZ(I))
         	BB =  (PXX(I)*PYY(I)+PXX(I)*PZZ(I)+PYY(I)*PZZ(I)-PXY(I)**2-PXZ(I)**2-PYZ(I)**2) 
         	P  = BB-THIRD*AA*AA
         	D(I)  = D(I)+ LD * VOL(I,IP) * TWO*SQRT(THIRD*MAX(-P,ZERO))-THIRD*AA
              ENDDO
            ENDDO

          ELSEIF(IFORMDT==2)THEN

            D(LFT:LLT)=ZERO

            GFAC=PM(105,MXT(1))

            DO IP=1,NPT

              DO I=LFT,LLT
         	PXX(I)=PX(I,1,IP)*PX(I,1,IP)+PX(I,2,IP)*PX(I,2,IP)+PX(I,3,IP)*PX(I,3,IP)+PX(I,4,IP)*PX(I,4,IP)+
     .   	       TRHEE_OVER_14*(PX(I,5,IP)*PX(I,5,IP)+PX(I,6,IP)*PX(I,6,IP)+PX(I,7,IP)*PX(I,7,IP)+
     .   			  PX(I,8,IP)*PX(I,8,IP)+PX(I,9,IP)*PX(I,9,IP)+PX(I,10,IP)*PX(I,10,IP))
         	PYY(I)=PY(I,1,IP)*PY(I,1,IP)+PY(I,2,IP)*PY(I,2,IP)+PY(I,3,IP)*PY(I,3,IP)+PY(I,4,IP)*PY(I,4,IP)+
     .   	       TRHEE_OVER_14*(PY(I,5,IP)*PY(I,5,IP) +PY(I,6,IP)*PY(I,6,IP)+PY(I,7,IP)*PY(I,7,IP)+
     .   			  PY(I,8,IP)*PY(I,8,IP)+PY(I,9,IP)*PY(I,9,IP)+PY(I,10,IP)*PY(I,10,IP))
         	PZZ(I)=PZ(I,1,IP)*PZ(I,1,IP)+PZ(I,2,IP)*PZ(I,2,IP)+PZ(I,3,IP)*PZ(I,3,IP)+PZ(I,4,IP)*PZ(I,4,IP)+
     .   	       TRHEE_OVER_14*(PZ(I,5,IP)*PZ(I,5,IP)+PZ(I,6,IP)*PZ(I,6,IP)+PZ(I,7,IP)*PZ(I,7,IP)+
     .   			  PZ(I,8,IP)*PZ(I,8,IP)+PZ(I,9,IP)*PZ(I,9,IP)+PZ(I,10,IP)*PZ(I,10,IP))
         	PXY(I)=PX(I,1,IP)*PY(I,1,IP)+PX(I,2,IP)*PY(I,2,IP)+PX(I,3,IP)*PY(I,3,IP)+PX(I,4,IP)*PY(I,4,IP)+
     .   	       TRHEE_OVER_14*(PX(I,5,IP)*PY(I,5,IP)+PX(I,6,IP)*PY(I,6,IP)+PX(I,7,IP)*PY(I,7,IP)+
     .   			  PX(I,8,IP)*PY(I,8,IP)+PX(I,9,IP)*PY(I,9,IP)+PX(I,10,IP)*PY(I,10,IP)) 
         	PXZ(I)=PX(I,1,IP)*PZ(I,1,IP)+PX(I,2,IP)*PZ(I,2,IP)+PX(I,3,IP)*PZ(I,3,IP)+PX(I,4,IP)*PZ(I,4,IP)+ 
     .   	       TRHEE_OVER_14*(PX(I,5,IP)*PZ(I,5,IP)+PX(I,6,IP)*PZ(I,6,IP)+PX(I,7,IP)*PZ(I,7,IP)+
     .   			  PX(I,8,IP)*PZ(I,8,IP)+PX(I,9,IP)*PZ(I,9,IP)+PX(I,10,IP)*PZ(I,10,IP)) 
         	PYZ(I)=PY(I,1,IP)*PZ(I,1,IP)+PY(I,2,IP)*PZ(I,2,IP)+PY(I,3,IP)*PZ(I,3,IP)+PY(I,4,IP)*PZ(I,4,IP)+
     .   	       TRHEE_OVER_14*(PY(I,5,IP)*PZ(I,5,IP)+PY(I,6,IP)*PZ(I,6,IP)+PY(I,7,IP)*PZ(I,7,IP)+
     .   			  PY(I,8,IP)*PZ(I,8,IP)+PY(I,9,IP)*PZ(I,9,IP)+PY(I,10,IP)*PZ(I,10,IP)) 
              ENDDO

              DO I=LFT,LLT
               AA = -(PXX(I)+PYY(I)+PZZ(I))
               BB =  GFAC*(PXX(I)*PYY(I)+PXX(I)*PZZ(I)+PYY(I)*PZZ(I)-PXY(I)**2-PXZ(I)**2-PYZ(I)**2) 
               P  = BB-THIRD*AA*AA

               D(I)  = D(I)+VOL(I,IP)*(TWO*SQRT(THIRD*MAX(-P,ZERO))-THIRD*AA)
              ENDDO
            ENDDO

          END IF ! IF(IFORMDT == ...)

        ELSE ! IF(NITER==0)THEN
C--------------------------------------------------------------------------------------
C         /DT1TET10/NITER
C         Iterative Power    
C--------------------------------------------------------------------------------------
          DO I=LFT,LLT
            INDEX(I)=I
          END DO
          NINDX=NEL
C
C         Initialization - Note : U normalized
          ILEAT=0
          DO N=1,10
            ILEAT=MOD(25173*ILEAT+13849,65536)
            ALEAT=(ILEAT-32768.)/32768.
            WX(N)=EM03*ALEAT
            ILEAT=MOD(25173*ILEAT+13849,65536)
            ALEAT=(ILEAT-32768.)/32768.
            WY(N)=EM03*ALEAT
            ILEAT=MOD(25173*ILEAT+13849,65536)
            ALEAT=(ILEAT-32768.)/32768.
            WZ(N)=EM03*ALEAT
          END DO

          NN = ZERO
          DO N=1,10
            NN = NN + WX(N)*WX(N)+WY(N)*WY(N)+WZ(N)*WZ(N)
          END DO
          NN=ONE/MAX(EM20,NN)

          DO N=1,10
            WX(N)=NN * WX(N)
            WY(N)=NN * WY(N)
            WZ(N)=NN * WZ(N)
          END DO

          DO N=1,10
            DO J=1,NINDX
              UX(J,N)=WX(N)
              UY(J,N)=WY(N)
              UZ(J,N)=WZ(N)
            END DO
          END DO

          IF(IFORMDT==2)THEN
C
C           It is fine so far as both ratio 3B/rho0 c**2 and G / rho0 c**2
C           are computed using Gmax and c is also computed in the material using Gmax. 
            GFAC=HALF*PM(105,MXT(1)) !  G/(B+4/3G)
            BFAC=THREE-FOUR*GFAC     ! 3B/(B+4/3G)
          ELSE ! conservative formulation in all other cases
            GFAC=HALF ! over-estimation of G/(B+4/3G)
            BFAC=THREE  ! 
          END IF

          NITERX=0
          NVOLD(1:NINDX)  =ZERO
          DO WHILE(NINDX/=0 .AND. NITERX < MAXITER) ! In RD Starter, iterate until convergence.

            NITERX=NITERX+1

            VX(1:NINDX,1:10)=ZERO
            VY(1:NINDX,1:10)=ZERO
            VZ(1:NINDX,1:10)=ZERO

            DO IP=1,NPT
C
C             Bip.U
              BU(1:NINDX,1:6)=ZERO
              DO N=1,10
                DO J=1,NINDX
                  I=INDEX(J)
                  BU(J,1)=BU(J,1)+PX(I,N,IP)*UX(J,N)
                  BU(J,2)=BU(J,2)+PY(I,N,IP)*UY(J,N)
                  BU(J,3)=BU(J,3)+PZ(I,N,IP)*UZ(J,N)
                  BU(J,4)=BU(J,4)+PY(I,N,IP)*UX(J,N)+PX(I,N,IP)*UY(J,N)
                  BU(J,5)=BU(J,5)+PZ(I,N,IP)*UY(J,N)+PY(I,N,IP)*UZ(J,N)
                  BU(J,6)=BU(J,6)+PZ(I,N,IP)*UX(J,N)+PX(I,N,IP)*UZ(J,N)
                END DO
              END DO
C
              DO K=1,3
                DO J=1,NINDX
                  BU(J,K)=BFAC*BU(J,K)
                END DO
              END DO
              DO K=4,6
                DO J=1,NINDX
                  BU(J,K)=GFAC*BU(J,K)
                END DO
              END DO
C
C             Vol_ip * Transpose(Bip).Bip.U
              DO N=1,10
                DO J=1,NINDX
                  I=INDEX(J)
                  VX(J,N)=VX(J,N)+VOL(I,IP)*(PX(I,N,IP)*BU(J,1)+PY(I,N,IP)*BU(J,4)+PZ(I,N,IP)*BU(J,6))
                  VY(J,N)=VY(J,N)+VOL(I,IP)*(PY(I,N,IP)*BU(J,2)+PX(I,N,IP)*BU(J,4)+PZ(I,N,IP)*BU(J,5))
                  VZ(J,N)=VZ(J,N)+VOL(I,IP)*(PZ(I,N,IP)*BU(J,3)+PY(I,N,IP)*BU(J,5)+PX(I,N,IP)*BU(J,6))
                END DO
              END DO

            END DO

            NV(1:NINDX) = ZERO
            DO N=1,4
              DO J=1,NINDX
                NV(J) = NV(J) + VX(J,N)*VX(J,N)+VY(J,N)*VY(J,N)+VZ(J,N)*VZ(J,N)
              END DO
            END DO
            DO N=5,10
              DO J=1,NINDX
	        VX(J,N)=TRHEE_OVER_14*VX(J,N)
	        VY(J,N)=TRHEE_OVER_14*VY(J,N)
	        VZ(J,N)=TRHEE_OVER_14*VZ(J,N)
                NV(J) = NV(J) + VX(J,N)*VX(J,N)+VY(J,N)*VY(J,N)+VZ(J,N)*VZ(J,N)
              END DO
            END DO
            DO J=1,NINDX
              NV(J)  =SQRT(NV(J))
              NINV(J)=ONE/MAX(EM20,NV(J))
            END DO
            DO N=1,10
              DO J=1,NINDX
                VX(J,N)=NINV(J) * VX(J,N)
                VY(J,N)=NINV(J) * VY(J,N)
                VZ(J,N)=NINV(J) * VZ(J,N)
              END DO
            END DO

            NINDX2=0
            DO J=1,NINDX
              I=INDEX(J)
              IF(ABS(NV(J)-NVOLD(J)) <= EM03*NV(J) .OR. NITERX==MAXITER)THEN ! converged
	        D(I)=NV(J)
 	  	V_PITER(I,1,1:10)=VX(J,1:10)
 	  	V_PITER(I,2,1:10)=VY(J,1:10)
 	  	V_PITER(I,3,1:10)=VZ(J,1:10)
              ELSE
                NINDX2=NINDX2+1
                INDEX(NINDX2)=I
                NVOLD(NINDX2)  =NV(J)
                UX(NINDX2,1:10)=VX(J,1:10)
                UY(NINDX2,1:10)=VY(J,1:10)
                UZ(NINDX2,1:10)=VZ(J,1:10)
              END IF
            END DO
            NINDX=NINDX2

          END DO ! DO WHILE(NINDX/=0)
C
C         2 * Max diagonal as an (under) estimation of Lambda 
C         Because Iterative Power may be too much under-estimated before convergence.
          DO N=1,10
            DO I=LFT,LLT
              UL(I,N) = ZERO
            ENDDO
          ENDDO
C
          DO IP=1,NPT
            DO N=1,10
              DO I=LFT,LLT
                UL(I,N) =UL(I,N) + VOL(I,IP) *
     +          ( PX(I,N,IP)*PX(I,N,IP) + PY(I,N,IP)*PY(I,N,IP)
     +         + PZ(I,N,IP)*PZ(I,N,IP) )
              ENDDO
            ENDDO
          ENDDO
C
          DO I=LFT,LLT
            AA   = MAX(UL(I,1),UL(I,2),UL(I,3),UL(I,4))
            BB   = TRHEE_OVER_14*MAX(UL(I,5),UL(I,6),UL(I,7),UL(I,8),UL(I,9),UL(I,10))
            D(I) = MAX(D(I),TWO*MAX(AA,BB))
          ENDDO
C
        END IF ! IF(NITER==0)THEN

        DO I=LFT,LLT
C
C         DELTAX2 is not used w/IDT1TET10
          DELTAX2(I) = ONE 
C
C         beware : VOLO = GBUF%VOL stored in RD Starter = VOLG / 4 
          MASS       = VOLG(I) ! (multiplied by RHOG0)

          MREF       = ONE/THIRTY2 * MASS
          DELTAX(I)  = TWO*SQRT(MREF/D(I))
        END DO

      ELSEIF(IDT1TET10/=0 .AND. ISROT==2)THEN

        NITER = IDT1TET10-1
        IF(NITER==0)THEN
C--------------------------------------------------------------------------------------
C         /DT1TET10 Conservative Time Step
C--------------------------------------------------------------------------------------
          IF(IFORMDT==0)THEN

            DO I=LFT,LLT
             PXX(I)=ZERO
             PYY(I)=ZERO
             PZZ(I)=ZERO
C            PXY(I)=ZERO
C            PXZ(I)=ZERO
C            PYZ(I)=ZERO
            END DO

            DO IP=1,NPT
c-----------------------------------------------------------------------------
C            Spectral Radius(M-1K)
C-----------------------------------------------------------------------------
C
C             Q = M-1 Transpose(B)
 	      DO I=LFT,LLT
 	        QX(I,1)=PX(I,1,IP)+HALF*(PX(I,5,IP)+PX(I,7,IP)+PX(I,8,IP))   
 	        QX(I,2)=PX(I,2,IP)+HALF*(PX(I,5,IP)+PX(I,6,IP)+PX(I,9,IP))
 	        QX(I,3)=PX(I,3,IP)+HALF*(PX(I,6,IP)+PX(I,7,IP)+PX(I,10,IP))
 	        QX(I,4)=PX(I,4,IP)+HALF*(PX(I,8,IP)+PX(I,9,IP)+PX(I,10,IP))
C	        QX(I,5)=HALF*(PX(I,1,IP)+PX(I,2,IP))+(HALF+ONE/FACIROT)*PX(I,5,IP)
C	    	  	 +FOURTH(PX(I,6,IP)+PX(I,7,IP)+PX(I,8,IP)+PX(I,9,IP)
 	        QX(I,5) =HALF*(QX(I,1)+QX(I,2))+FAC*PX(I,5,IP)
 	        QX(I,6) =HALF*(QX(I,2)+QX(I,3))+FAC*PX(I,6,IP)
 	        QX(I,7) =HALF*(QX(I,1)+QX(I,3))+FAC*PX(I,7,IP)
 	        QX(I,8) =HALF*(QX(I,1)+QX(I,4))+FAC*PX(I,8,IP)
 	        QX(I,9) =HALF*(QX(I,2)+QX(I,4))+FAC*PX(I,9,IP)
 	        QX(I,10)=HALF*(QX(I,3)+QX(I,4))+FAC*PX(I,10,IP)

 	        QY(I,1)=PY(I,1,IP)+HALF*(PY(I,5,IP)+PY(I,7,IP)+PY(I,8,IP))
 	        QY(I,2)=PY(I,2,IP)+HALF*(PY(I,5,IP)+PY(I,6,IP)+PY(I,9,IP))
 	        QY(I,3)=PY(I,3,IP)+HALF*(PY(I,6,IP)+PY(I,7,IP)+PY(I,10,IP))
 	        QY(I,4)=PY(I,4,IP)+HALF*(PY(I,8,IP)+PY(I,9,IP)+PY(I,10,IP))
 	        QY(I,5) =HALF*(QY(I,1)+QY(I,2))+FAC*PY(I,5,IP)
 	        QY(I,6) =HALF*(QY(I,2)+QY(I,3))+FAC*PY(I,6,IP)
 	        QY(I,7) =HALF*(QY(I,1)+QY(I,3))+FAC*PY(I,7,IP)
 	        QY(I,8) =HALF*(QY(I,1)+QY(I,4))+FAC*PY(I,8,IP)
 	        QY(I,9) =HALF*(QY(I,2)+QY(I,4))+FAC*PY(I,9,IP)
 	        QY(I,10)=HALF*(QY(I,3)+QY(I,4))+FAC*PY(I,10,IP)

 	        QZ(I,1)=PZ(I,1,IP)+HALF*(PZ(I,5,IP)+PZ(I,7,IP)+PZ(I,8,IP))
 	        QZ(I,2)=PZ(I,2,IP)+HALF*(PZ(I,5,IP)+PZ(I,6,IP)+PZ(I,9,IP))
 	        QZ(I,3)=PZ(I,3,IP)+HALF*(PZ(I,6,IP)+PZ(I,7,IP)+PZ(I,10,IP))
 	        QZ(I,4)=PZ(I,4,IP)+HALF*(PZ(I,8,IP)+PZ(I,9,IP)+PZ(I,10,IP))
 	        QZ(I,5) =HALF*(QZ(I,1)+QZ(I,2))+FAC*PZ(I,5,IP)
 	        QZ(I,6) =HALF*(QZ(I,2)+QZ(I,3))+FAC*PZ(I,6,IP)
 	        QZ(I,7) =HALF*(QZ(I,1)+QZ(I,3))+FAC*PZ(I,7,IP)
 	        QZ(I,8) =HALF*(QZ(I,1)+QZ(I,4))+FAC*PZ(I,8,IP)
 	        QZ(I,9) =HALF*(QZ(I,2)+QZ(I,4))+FAC*PZ(I,9,IP)
 	        QZ(I,10)=HALF*(QZ(I,3)+QZ(I,4))+FAC*PZ(I,10,IP)

 	      END DO
C
C	      B M-1 Transpose(B)
 	      DO I=LFT,LLT
 	        PXX(I)=PXX(I)+
     .                 PX(I,1,IP)*QX(I,1)+PX(I,2,IP)*QX(I,2)+PX(I,3,IP)*QX(I,3)+PX(I,4,IP)*QX(I,4)+
     .      	       PX(I,5,IP)*QX(I,5)+PX(I,6,IP)*QX(I,6)+PX(I,7,IP)*QX(I,7)+
     .      	  		  PX(I,8,IP)*QX(I,8)+PX(I,9,IP)*QX(I,9)+PX(I,10,IP)*QX(I,10)
 	        PYY(I)=PYY(I)+
     .                 PY(I,1,IP)*QY(I,1)+PY(I,2,IP)*QY(I,2)+PY(I,3,IP)*QY(I,3)+PY(I,4,IP)*QY(I,4)+
     .      	       PY(I,5,IP)*QY(I,5) +PY(I,6,IP)*QY(I,6)+PY(I,7,IP)*QY(I,7)+
     .      	  		  PY(I,8,IP)*QY(I,8)+PY(I,9,IP)*QY(I,9)+PY(I,10,IP)*QY(I,10)
 	        PZZ(I)=PZZ(I)+
     .                 PZ(I,1,IP)*QZ(I,1)+PZ(I,2,IP)*QZ(I,2)+PZ(I,3,IP)*QZ(I,3)+PZ(I,4,IP)*QZ(I,4)+
     .      	       PZ(I,5,IP)*QZ(I,5)+PZ(I,6,IP)*QZ(I,6)+PZ(I,7,IP)*QZ(I,7)+
     .      	  		  PZ(I,8,IP)*QZ(I,8)+PZ(I,9,IP)*QZ(I,9)+PZ(I,10,IP)*QZ(I,10)
C	        PXY(I)=PX(I,1,IP)*QY(I,1)+PX(I,2,IP)*QY(I,2)+PX(I,3,IP)*QY(I,3)+PX(I,4,IP)*QY(I,4)+
C     .      	       PX(I,5,IP)*QY(I,5)+PX(I,6,IP)*QY(I,6)+PX(I,7,IP)*QY(I,7)+
C     .      	  		  PX(I,8,IP)*QY(I,8)+PX(I,9,IP)*QY(I,9)+PX(I,10,IP)*QY(I,10)
C 	        PXZ(I)=PX(I,1,IP)*QZ(I,1)+PX(I,2,IP)*QZ(I,2)+PX(I,3,IP)*QZ(I,3)+PX(I,4,IP)*QZ(I,4)+ 
C     .      	       PX(I,5,IP)*QZ(I,5)+PX(I,6,IP)*QZ(I,6)+PX(I,7,IP)*QZ(I,7)+
C     .      	  		  PX(I,8,IP)*QZ(I,8)+PX(I,9,IP)*QZ(I,9)+PX(I,10,IP)*QZ(I,10)
C 	        PYZ(I)=PY(I,1,IP)*QZ(I,1)+PY(I,2,IP)*QZ(I,2)+PY(I,3,IP)*QZ(I,3)+PY(I,4,IP)*QZ(I,4)+
C     .      	       PY(I,5,IP)*QZ(I,5)+PY(I,6,IP)*QZ(I,6)+PY(I,7,IP)*QZ(I,7)+
C     .      	  		  PY(I,8,IP)*QZ(I,8)+PY(I,9,IP)*QZ(I,9)+PY(I,10,IP)*QZ(I,10)
 	      ENDDO

            ENDDO

            DO I=LFT,LLT
              D(I) = PXX(I)+PYY(I)+PZZ(I)
            ENDDO

          ELSEIF(IFORMDT==1)THEN

            D(LFT:LLT)=ZERO

            GFAC=PM(105,MXT(1))
            LD  =TWO*SQRT(MAX(ONE-GFAC,ZERO))+ONE

            DO IP=1,NPT
c-----------------------------------------------------------------------------
C            Spectral Radius(M-1K)
C-----------------------------------------------------------------------------
C
C             Q = M-1 Transpose(B)
 	      DO I=LFT,LLT
 	        QX(I,1)=PX(I,1,IP)+HALF*(PX(I,5,IP)+PX(I,7,IP)+PX(I,8,IP))   
 	        QX(I,2)=PX(I,2,IP)+HALF*(PX(I,5,IP)+PX(I,6,IP)+PX(I,9,IP))
 	        QX(I,3)=PX(I,3,IP)+HALF*(PX(I,6,IP)+PX(I,7,IP)+PX(I,10,IP))
 	        QX(I,4)=PX(I,4,IP)+HALF*(PX(I,8,IP)+PX(I,9,IP)+PX(I,10,IP))
C	        QX(I,5)=HALF*(PX(I,1,IP)+PX(I,2,IP))+(HALF+ONE/FACIROT)*PX(I,5,IP)
C	    	  	 +FOURTH(PX(I,6,IP)+PX(I,7,IP)+PX(I,8,IP)+PX(I,9,IP)
 	        QX(I,5) =HALF*(QX(I,1)+QX(I,2))+FAC*PX(I,5,IP)
 	        QX(I,6) =HALF*(QX(I,2)+QX(I,3))+FAC*PX(I,6,IP)
 	        QX(I,7) =HALF*(QX(I,1)+QX(I,3))+FAC*PX(I,7,IP)
 	        QX(I,8) =HALF*(QX(I,1)+QX(I,4))+FAC*PX(I,8,IP)
 	        QX(I,9) =HALF*(QX(I,2)+QX(I,4))+FAC*PX(I,9,IP)
 	        QX(I,10)=HALF*(QX(I,3)+QX(I,4))+FAC*PX(I,10,IP)

 	        QY(I,1)=PY(I,1,IP)+HALF*(PY(I,5,IP)+PY(I,7,IP)+PY(I,8,IP))
 	        QY(I,2)=PY(I,2,IP)+HALF*(PY(I,5,IP)+PY(I,6,IP)+PY(I,9,IP))
 	        QY(I,3)=PY(I,3,IP)+HALF*(PY(I,6,IP)+PY(I,7,IP)+PY(I,10,IP))
 	        QY(I,4)=PY(I,4,IP)+HALF*(PY(I,8,IP)+PY(I,9,IP)+PY(I,10,IP))
 	        QY(I,5) =HALF*(QY(I,1)+QY(I,2))+FAC*PY(I,5,IP)
 	        QY(I,6) =HALF*(QY(I,2)+QY(I,3))+FAC*PY(I,6,IP)
 	        QY(I,7) =HALF*(QY(I,1)+QY(I,3))+FAC*PY(I,7,IP)
 	        QY(I,8) =HALF*(QY(I,1)+QY(I,4))+FAC*PY(I,8,IP)
 	        QY(I,9) =HALF*(QY(I,2)+QY(I,4))+FAC*PY(I,9,IP)
 	        QY(I,10)=HALF*(QY(I,3)+QY(I,4))+FAC*PY(I,10,IP)

 	        QZ(I,1)=PZ(I,1,IP)+HALF*(PZ(I,5,IP)+PZ(I,7,IP)+PZ(I,8,IP))
 	        QZ(I,2)=PZ(I,2,IP)+HALF*(PZ(I,5,IP)+PZ(I,6,IP)+PZ(I,9,IP))
 	        QZ(I,3)=PZ(I,3,IP)+HALF*(PZ(I,6,IP)+PZ(I,7,IP)+PZ(I,10,IP))
 	        QZ(I,4)=PZ(I,4,IP)+HALF*(PZ(I,8,IP)+PZ(I,9,IP)+PZ(I,10,IP))
 	        QZ(I,5) =HALF*(QZ(I,1)+QZ(I,2))+FAC*PZ(I,5,IP)
 	        QZ(I,6) =HALF*(QZ(I,2)+QZ(I,3))+FAC*PZ(I,6,IP)
 	        QZ(I,7) =HALF*(QZ(I,1)+QZ(I,3))+FAC*PZ(I,7,IP)
 	        QZ(I,8) =HALF*(QZ(I,1)+QZ(I,4))+FAC*PZ(I,8,IP)
 	        QZ(I,9) =HALF*(QZ(I,2)+QZ(I,4))+FAC*PZ(I,9,IP)
 	        QZ(I,10)=HALF*(QZ(I,3)+QZ(I,4))+FAC*PZ(I,10,IP)

 	      END DO
C
C	      B M-1 Transpose(B)
 	      DO I=LFT,LLT
 	        PXX(I)=PX(I,1,IP)*QX(I,1)+PX(I,2,IP)*QX(I,2)+PX(I,3,IP)*QX(I,3)+PX(I,4,IP)*QX(I,4)+
     .      	       PX(I,5,IP)*QX(I,5)+PX(I,6,IP)*QX(I,6)+PX(I,7,IP)*QX(I,7)+
     .      	  		  PX(I,8,IP)*QX(I,8)+PX(I,9,IP)*QX(I,9)+PX(I,10,IP)*QX(I,10)
 	        PYY(I)=PY(I,1,IP)*QY(I,1)+PY(I,2,IP)*QY(I,2)+PY(I,3,IP)*QY(I,3)+PY(I,4,IP)*QY(I,4)+
     .      	       PY(I,5,IP)*QY(I,5) +PY(I,6,IP)*QY(I,6)+PY(I,7,IP)*QY(I,7)+
     .      	  		  PY(I,8,IP)*QY(I,8)+PY(I,9,IP)*QY(I,9)+PY(I,10,IP)*QY(I,10)
 	        PZZ(I)=PZ(I,1,IP)*QZ(I,1)+PZ(I,2,IP)*QZ(I,2)+PZ(I,3,IP)*QZ(I,3)+PZ(I,4,IP)*QZ(I,4)+
     .      	       PZ(I,5,IP)*QZ(I,5)+PZ(I,6,IP)*QZ(I,6)+PZ(I,7,IP)*QZ(I,7)+
     .      	  		  PZ(I,8,IP)*QZ(I,8)+PZ(I,9,IP)*QZ(I,9)+PZ(I,10,IP)*QZ(I,10)
 	        PXY(I)=PX(I,1,IP)*QY(I,1)+PX(I,2,IP)*QY(I,2)+PX(I,3,IP)*QY(I,3)+PX(I,4,IP)*QY(I,4)+
     .      	       PX(I,5,IP)*QY(I,5)+PX(I,6,IP)*QY(I,6)+PX(I,7,IP)*QY(I,7)+
     .      	  		  PX(I,8,IP)*QY(I,8)+PX(I,9,IP)*QY(I,9)+PX(I,10,IP)*QY(I,10)
 	        PXZ(I)=PX(I,1,IP)*QZ(I,1)+PX(I,2,IP)*QZ(I,2)+PX(I,3,IP)*QZ(I,3)+PX(I,4,IP)*QZ(I,4)+ 
     .      	       PX(I,5,IP)*QZ(I,5)+PX(I,6,IP)*QZ(I,6)+PX(I,7,IP)*QZ(I,7)+
     .      	  		  PX(I,8,IP)*QZ(I,8)+PX(I,9,IP)*QZ(I,9)+PX(I,10,IP)*QZ(I,10)
 	        PYZ(I)=PY(I,1,IP)*QZ(I,1)+PY(I,2,IP)*QZ(I,2)+PY(I,3,IP)*QZ(I,3)+PY(I,4,IP)*QZ(I,4)+
     .      	       PY(I,5,IP)*QZ(I,5)+PY(I,6,IP)*QZ(I,6)+PY(I,7,IP)*QZ(I,7)+
     .      	  		  PY(I,8,IP)*QZ(I,8)+PY(I,9,IP)*QZ(I,9)+PY(I,10,IP)*QZ(I,10)
 	      ENDDO

              DO I=LFT,LLT
          	AA = -(PXX(I)+PYY(I)+PZZ(I))
          	BB =  (PXX(I)*PYY(I)+PXX(I)*PZZ(I)+PYY(I)*PZZ(I)-PXY(I)**2-PXZ(I)**2-PYZ(I)**2) 
          	P  = BB-THIRD*AA*AA
          	D(I)  = D(I)+ LD * VOL(I,IP) * TWO*SQRT(THIRD*MAX(-P,ZERO))-THIRD*AA
              ENDDO
            ENDDO

          ELSEIF(IFORMDT==2)THEN

            D(LFT:LLT)=ZERO

            GFAC=PM(105,MXT(1))

C           FAC     = ONE/(NINE+THIRD) !  FACIROT2 = NINE+THIRD
            DO IP=1,NPT
c-----------------------------------------------------------------------------
C            Spectral Radius(M-1K)
C-----------------------------------------------------------------------------
C
C             Q = M-1 Transpose(B)
 	      DO I=LFT,LLT
 	        QX(I,1)=PX(I,1,IP)+HALF*(PX(I,5,IP)+PX(I,7,IP)+PX(I,8,IP))   
 	        QX(I,2)=PX(I,2,IP)+HALF*(PX(I,5,IP)+PX(I,6,IP)+PX(I,9,IP))
 	        QX(I,3)=PX(I,3,IP)+HALF*(PX(I,6,IP)+PX(I,7,IP)+PX(I,10,IP))
 	        QX(I,4)=PX(I,4,IP)+HALF*(PX(I,8,IP)+PX(I,9,IP)+PX(I,10,IP))
C	        QX(I,5)=HALF*(PX(I,1,IP)+PX(I,2,IP))+(HALF+ONE/FACIROT)*PX(I,5,IP)
C	    	  	 +FOURTH(PX(I,6,IP)+PX(I,7,IP)+PX(I,8,IP)+PX(I,9,IP)
 	        QX(I,5) =HALF*(QX(I,1)+QX(I,2))+FAC*PX(I,5,IP)
 	        QX(I,6) =HALF*(QX(I,2)+QX(I,3))+FAC*PX(I,6,IP)
 	        QX(I,7) =HALF*(QX(I,1)+QX(I,3))+FAC*PX(I,7,IP)
 	        QX(I,8) =HALF*(QX(I,1)+QX(I,4))+FAC*PX(I,8,IP)
 	        QX(I,9) =HALF*(QX(I,2)+QX(I,4))+FAC*PX(I,9,IP)
 	        QX(I,10)=HALF*(QX(I,3)+QX(I,4))+FAC*PX(I,10,IP)

 	        QY(I,1)=PY(I,1,IP)+HALF*(PY(I,5,IP)+PY(I,7,IP)+PY(I,8,IP))
 	        QY(I,2)=PY(I,2,IP)+HALF*(PY(I,5,IP)+PY(I,6,IP)+PY(I,9,IP))
 	        QY(I,3)=PY(I,3,IP)+HALF*(PY(I,6,IP)+PY(I,7,IP)+PY(I,10,IP))
 	        QY(I,4)=PY(I,4,IP)+HALF*(PY(I,8,IP)+PY(I,9,IP)+PY(I,10,IP))
 	        QY(I,5) =HALF*(QY(I,1)+QY(I,2))+FAC*PY(I,5,IP)
 	        QY(I,6) =HALF*(QY(I,2)+QY(I,3))+FAC*PY(I,6,IP)
 	        QY(I,7) =HALF*(QY(I,1)+QY(I,3))+FAC*PY(I,7,IP)
 	        QY(I,8) =HALF*(QY(I,1)+QY(I,4))+FAC*PY(I,8,IP)
 	        QY(I,9) =HALF*(QY(I,2)+QY(I,4))+FAC*PY(I,9,IP)
 	        QY(I,10)=HALF*(QY(I,3)+QY(I,4))+FAC*PY(I,10,IP)

 	        QZ(I,1)=PZ(I,1,IP)+HALF*(PZ(I,5,IP)+PZ(I,7,IP)+PZ(I,8,IP))
 	        QZ(I,2)=PZ(I,2,IP)+HALF*(PZ(I,5,IP)+PZ(I,6,IP)+PZ(I,9,IP))
 	        QZ(I,3)=PZ(I,3,IP)+HALF*(PZ(I,6,IP)+PZ(I,7,IP)+PZ(I,10,IP))
 	        QZ(I,4)=PZ(I,4,IP)+HALF*(PZ(I,8,IP)+PZ(I,9,IP)+PZ(I,10,IP))
 	        QZ(I,5) =HALF*(QZ(I,1)+QZ(I,2))+FAC*PZ(I,5,IP)
 	        QZ(I,6) =HALF*(QZ(I,2)+QZ(I,3))+FAC*PZ(I,6,IP)
 	        QZ(I,7) =HALF*(QZ(I,1)+QZ(I,3))+FAC*PZ(I,7,IP)
 	        QZ(I,8) =HALF*(QZ(I,1)+QZ(I,4))+FAC*PZ(I,8,IP)
 	        QZ(I,9) =HALF*(QZ(I,2)+QZ(I,4))+FAC*PZ(I,9,IP)
 	        QZ(I,10)=HALF*(QZ(I,3)+QZ(I,4))+FAC*PZ(I,10,IP)

 	      END DO
C
C	      B M-1 Transpose(B)
 	      DO I=LFT,LLT
 	        PXX(I)=PX(I,1,IP)*QX(I,1)+PX(I,2,IP)*QX(I,2)+PX(I,3,IP)*QX(I,3)+PX(I,4,IP)*QX(I,4)+
     .      	       PX(I,5,IP)*QX(I,5)+PX(I,6,IP)*QX(I,6)+PX(I,7,IP)*QX(I,7)+
     .      	  		  PX(I,8,IP)*QX(I,8)+PX(I,9,IP)*QX(I,9)+PX(I,10,IP)*QX(I,10)
 	        PYY(I)=PY(I,1,IP)*QY(I,1)+PY(I,2,IP)*QY(I,2)+PY(I,3,IP)*QY(I,3)+PY(I,4,IP)*QY(I,4)+
     .      	       PY(I,5,IP)*QY(I,5) +PY(I,6,IP)*QY(I,6)+PY(I,7,IP)*QY(I,7)+
     .      	  		  PY(I,8,IP)*QY(I,8)+PY(I,9,IP)*QY(I,9)+PY(I,10,IP)*QY(I,10)
 	        PZZ(I)=PZ(I,1,IP)*QZ(I,1)+PZ(I,2,IP)*QZ(I,2)+PZ(I,3,IP)*QZ(I,3)+PZ(I,4,IP)*QZ(I,4)+
     .      	       PZ(I,5,IP)*QZ(I,5)+PZ(I,6,IP)*QZ(I,6)+PZ(I,7,IP)*QZ(I,7)+
     .      	  		  PZ(I,8,IP)*QZ(I,8)+PZ(I,9,IP)*QZ(I,9)+PZ(I,10,IP)*QZ(I,10)
 	        PXY(I)=PX(I,1,IP)*QY(I,1)+PX(I,2,IP)*QY(I,2)+PX(I,3,IP)*QY(I,3)+PX(I,4,IP)*QY(I,4)+
     .      	       PX(I,5,IP)*QY(I,5)+PX(I,6,IP)*QY(I,6)+PX(I,7,IP)*QY(I,7)+
     .      	  		  PX(I,8,IP)*QY(I,8)+PX(I,9,IP)*QY(I,9)+PX(I,10,IP)*QY(I,10)
 	        PXZ(I)=PX(I,1,IP)*QZ(I,1)+PX(I,2,IP)*QZ(I,2)+PX(I,3,IP)*QZ(I,3)+PX(I,4,IP)*QZ(I,4)+ 
     .      	       PX(I,5,IP)*QZ(I,5)+PX(I,6,IP)*QZ(I,6)+PX(I,7,IP)*QZ(I,7)+
     .      	  		  PX(I,8,IP)*QZ(I,8)+PX(I,9,IP)*QZ(I,9)+PX(I,10,IP)*QZ(I,10)
 	        PYZ(I)=PY(I,1,IP)*QZ(I,1)+PY(I,2,IP)*QZ(I,2)+PY(I,3,IP)*QZ(I,3)+PY(I,4,IP)*QZ(I,4)+
     .      	       PY(I,5,IP)*QZ(I,5)+PY(I,6,IP)*QZ(I,6)+PY(I,7,IP)*QZ(I,7)+
     .      	  		  PY(I,8,IP)*QZ(I,8)+PY(I,9,IP)*QZ(I,9)+PY(I,10,IP)*QZ(I,10)
 	      ENDDO
C-------------------------------------------------------------------------------
              DO I=LFT,LLT
               AA = -(PXX(I)+PYY(I)+PZZ(I))
               BB =  GFAC*(PXX(I)*PYY(I)+PXX(I)*PZZ(I)+PYY(I)*PZZ(I)-PXY(I)**2-PXZ(I)**2-PYZ(I)**2) 
               P  = BB-THIRD*AA*AA

               D(I)  = D(I)+VOL(I,IP)*(TWO*SQRT(THIRD*MAX(-P,ZERO))-THIRD*AA)
              ENDDO

            ENDDO

          END IF! IF(IFORMDT == ...)

        ELSE ! IF(NITER==0)THEN
C--------------------------------------------------------------------------------------
C         /DT1TET10/NITER
C         Iterative Power    
C--------------------------------------------------------------------------------------
          DO I=LFT,LLT
            INDEX(I)=I
          END DO
          NINDX=NEL
C
C         Initialization - Note : U normalized
          ILEAT=0
          DO N=1,10
            ILEAT=MOD(25173*ILEAT+13849,65536)
            ALEAT=(ILEAT-32768.)/32768.
            WX(N)=EM03*ALEAT
            ILEAT=MOD(25173*ILEAT+13849,65536)
            ALEAT=(ILEAT-32768.)/32768.
            WY(N)=EM03*ALEAT
            ILEAT=MOD(25173*ILEAT+13849,65536)
            ALEAT=(ILEAT-32768.)/32768.
            WZ(N)=EM03*ALEAT
          END DO

          NN = ZERO
          DO N=1,10
            NN = NN + WX(N)*WX(N)+WY(N)*WY(N)+WZ(N)*WZ(N)
          END DO
          NN=ONE/MAX(EM20,NN)

          DO N=1,10
            WX(N)=NN * WX(N)
            WY(N)=NN * WY(N)
            WZ(N)=NN * WZ(N)
          END DO

          DO N=1,10
            DO J=1,NINDX
              UX(J,N)=WX(N)
              UY(J,N)=WY(N)
              UZ(J,N)=WZ(N)
            END DO
          END DO

          IF(IFORMDT==2)THEN
C
C           It is fine so far as both ratio 3B/rho0 c**2 and G / rho0 c**2
C           are computed using Gmax and c is also computed in the material using Gmax. 
            GFAC=HALF*PM(105,MXT(1)) !  G/(B+4/3G)
            BFAC=THREE-FOUR*GFAC     ! 3B/(B+4/3G)
          ELSE ! conservative formulation in all other cases
            GFAC=HALF ! over-estimation of G/(B+4/3G)
            BFAC=THREE  ! 
          END IF

          NITERX=0
          NVOLD(1:NINDX)  =ZERO
          DO WHILE(NINDX/=0 .AND. NITERX < MAXITER) ! In RD Starter, iterate until convergence.

            NITERX=NITERX+1

            VX(1:NINDX,1:10)=ZERO
            VY(1:NINDX,1:10)=ZERO
            VZ(1:NINDX,1:10)=ZERO

            DO IP=1,NPT
C
C             Bip.U
              BU(1:NINDX,1:6)=ZERO
              DO N=1,10
                DO J=1,NINDX
                  I=INDEX(J)
                  BU(J,1)=BU(J,1)+PX(I,N,IP)*UX(J,N)
                  BU(J,2)=BU(J,2)+PY(I,N,IP)*UY(J,N)
                  BU(J,3)=BU(J,3)+PZ(I,N,IP)*UZ(J,N)
                  BU(J,4)=BU(J,4)+PY(I,N,IP)*UX(J,N)+PX(I,N,IP)*UY(J,N)
                  BU(J,5)=BU(J,5)+PZ(I,N,IP)*UY(J,N)+PY(I,N,IP)*UZ(J,N)
                  BU(J,6)=BU(J,6)+PZ(I,N,IP)*UX(J,N)+PX(I,N,IP)*UZ(J,N)
                END DO
              END DO
C
              DO K=1,3
                DO J=1,NINDX
                  BU(J,K)=BFAC*BU(J,K)
                END DO
              END DO
              DO K=4,6
                DO J=1,NINDX
                  BU(J,K)=GFAC*BU(J,K)
                END DO
              END DO
C
C             Vol_ip * Transpose(Bip).Bip.U
              DO N=1,10
                DO J=1,NINDX
                  I=INDEX(J)
                  VX(J,N)=VX(J,N)+VOL(I,IP)*(PX(I,N,IP)*BU(J,1)+PY(I,N,IP)*BU(J,4)+PZ(I,N,IP)*BU(J,6))
                  VY(J,N)=VY(J,N)+VOL(I,IP)*(PY(I,N,IP)*BU(J,2)+PX(I,N,IP)*BU(J,4)+PZ(I,N,IP)*BU(J,5))
                  VZ(J,N)=VZ(J,N)+VOL(I,IP)*(PZ(I,N,IP)*BU(J,3)+PY(I,N,IP)*BU(J,5)+PX(I,N,IP)*BU(J,6))
                END DO
              END DO

            END DO
C
C           M-1 K . U
 	    DO J=1,NINDX
 	      VX(J,1)=VX(J,1)+HALF*(VX(J,5)+VX(J,7)+VX(J,8))   
 	      VX(J,2)=VX(J,2)+HALF*(VX(J,5)+VX(J,6)+VX(J,9))
 	      VX(J,3)=VX(J,3)+HALF*(VX(J,6)+VX(J,7)+VX(J,10))
 	      VX(J,4)=VX(J,4)+HALF*(VX(J,8)+VX(J,9)+VX(J,10))
C	      VX(J,5)=HALF*(VX(J,1)+VX(J,2))+(HALF+ONE/FACIROT)*VX(J,5)
C	               +FOURTH(VX(J,6)+VX(J,7)+VX(J,8)+VX(J,9)
 	      VX(J,5) =HALF*(VX(J,1)+VX(J,2))+FAC*VX(J,5)
 	      VX(J,6) =HALF*(VX(J,2)+VX(J,3))+FAC*VX(J,6)
 	      VX(J,7) =HALF*(VX(J,1)+VX(J,3))+FAC*VX(J,7)
 	      VX(J,8) =HALF*(VX(J,1)+VX(J,4))+FAC*VX(J,8)
 	      VX(J,9) =HALF*(VX(J,2)+VX(J,4))+FAC*VX(J,9)
 	      VX(J,10)=HALF*(VX(J,3)+VX(J,4))+FAC*VX(J,10)

 	      VY(J,1)=VY(J,1)+HALF*(VY(J,5)+VY(J,7)+VY(J,8))   
 	      VY(J,2)=VY(J,2)+HALF*(VY(J,5)+VY(J,6)+VY(J,9))
 	      VY(J,3)=VY(J,3)+HALF*(VY(J,6)+VY(J,7)+VY(J,10))
 	      VY(J,4)=VY(J,4)+HALF*(VY(J,8)+VY(J,9)+VY(J,10))
 	      VY(J,5) =HALF*(VY(J,1)+VY(J,2))+FAC*VY(J,5)
 	      VY(J,6) =HALF*(VY(J,2)+VY(J,3))+FAC*VY(J,6)
 	      VY(J,7) =HALF*(VY(J,1)+VY(J,3))+FAC*VY(J,7)
 	      VY(J,8) =HALF*(VY(J,1)+VY(J,4))+FAC*VY(J,8)
 	      VY(J,9) =HALF*(VY(J,2)+VY(J,4))+FAC*VY(J,9)
 	      VY(J,10)=HALF*(VY(J,3)+VY(J,4))+FAC*VY(J,10)

 	      VZ(J,1)=VZ(J,1)+HALF*(VZ(J,5)+VZ(J,7)+VZ(J,8))   
 	      VZ(J,2)=VZ(J,2)+HALF*(VZ(J,5)+VZ(J,6)+VZ(J,9))
 	      VZ(J,3)=VZ(J,3)+HALF*(VZ(J,6)+VZ(J,7)+VZ(J,10))
 	      VZ(J,4)=VZ(J,4)+HALF*(VZ(J,8)+VZ(J,9)+VZ(J,10))
 	      VZ(J,5) =HALF*(VZ(J,1)+VZ(J,2))+FAC*VZ(J,5)
 	      VZ(J,6) =HALF*(VZ(J,2)+VZ(J,3))+FAC*VZ(J,6)
 	      VZ(J,7) =HALF*(VZ(J,1)+VZ(J,3))+FAC*VZ(J,7)
 	      VZ(J,8) =HALF*(VZ(J,1)+VZ(J,4))+FAC*VZ(J,8)
 	      VZ(J,9) =HALF*(VZ(J,2)+VZ(J,4))+FAC*VZ(J,9)
 	      VZ(J,10)=HALF*(VZ(J,3)+VZ(J,4))+FAC*VZ(J,10)

 	    END DO

            NV(1:NINDX) = ZERO
            DO N=1,10
              DO J=1,NINDX
                NV(J) = NV(J) + VX(J,N)*VX(J,N)+VY(J,N)*VY(J,N)+VZ(J,N)*VZ(J,N)
              END DO
            END DO
            DO J=1,NINDX
              NV(J)  =SQRT(NV(J))
              NINV(J)=ONE/MAX(EM20,NV(J))
            END DO
            DO N=1,10
              DO J=1,NINDX
                VX(J,N)=NINV(J) * VX(J,N)
                VY(J,N)=NINV(J) * VY(J,N)
                VZ(J,N)=NINV(J) * VZ(J,N)
              END DO
            END DO

            NINDX2=0
            DO J=1,NINDX
              I=INDEX(J)
              IF(ABS(NV(J)-NVOLD(J)) <= EM03*NV(J) .OR. NITERX==MAXITER)THEN ! converged
	        D(I)=NV(J)
 	  	V_PITER(I,1,1:10)=VX(J,1:10)
 	  	V_PITER(I,2,1:10)=VY(J,1:10)
 	  	V_PITER(I,3,1:10)=VZ(J,1:10)
              ELSE
                NINDX2=NINDX2+1
                INDEX(NINDX2)=I
                NVOLD(NINDX2)  =NV(J)
                UX(NINDX2,1:10)=VX(J,1:10)
                UY(NINDX2,1:10)=VY(J,1:10)
                UZ(NINDX2,1:10)=VZ(J,1:10)
              END IF
            END DO
            NINDX=NINDX2

          END DO ! DO WHILE(NINDX/=0)
C
C         2 * Max diagonal as an (under) estimation of Lambda 
C         Because Iterative Power may be too much under-estimated before convergence.
          DO N=1,10
            DO I=LFT,LLT
              UL(I,N) = ZERO
            ENDDO
          ENDDO
C
          DO IP=1,NPT
C
C           Q = M-1 Transpose(B)
 	    DO I=LFT,LLT
 	      QX(I,1)=PX(I,1,IP)+HALF*(PX(I,5,IP)+PX(I,7,IP)+PX(I,8,IP))   
 	      QX(I,2)=PX(I,2,IP)+HALF*(PX(I,5,IP)+PX(I,6,IP)+PX(I,9,IP))
 	      QX(I,3)=PX(I,3,IP)+HALF*(PX(I,6,IP)+PX(I,7,IP)+PX(I,10,IP))
 	      QX(I,4)=PX(I,4,IP)+HALF*(PX(I,8,IP)+PX(I,9,IP)+PX(I,10,IP))
C	      QX(I,5)=HALF*(PX(I,1,IP)+PX(I,2,IP))+(HALF+ONE/FACIROT)*PX(I,5,IP)
C	               +FOURTH(PX(I,6,IP)+PX(I,7,IP)+PX(I,8,IP)+PX(I,9,IP)
 	      QX(I,5) =HALF*(QX(I,1)+QX(I,2))+FAC*PX(I,5,IP)
 	      QX(I,6) =HALF*(QX(I,2)+QX(I,3))+FAC*PX(I,6,IP)
 	      QX(I,7) =HALF*(QX(I,1)+QX(I,3))+FAC*PX(I,7,IP)
 	      QX(I,8) =HALF*(QX(I,1)+QX(I,4))+FAC*PX(I,8,IP)
 	      QX(I,9) =HALF*(QX(I,2)+QX(I,4))+FAC*PX(I,9,IP)
 	      QX(I,10)=HALF*(QX(I,3)+QX(I,4))+FAC*PX(I,10,IP)

 	      QY(I,1)=PY(I,1,IP)+HALF*(PY(I,5,IP)+PY(I,7,IP)+PY(I,8,IP))
 	      QY(I,2)=PY(I,2,IP)+HALF*(PY(I,5,IP)+PY(I,6,IP)+PY(I,9,IP))
 	      QY(I,3)=PY(I,3,IP)+HALF*(PY(I,6,IP)+PY(I,7,IP)+PY(I,10,IP))
 	      QY(I,4)=PY(I,4,IP)+HALF*(PY(I,8,IP)+PY(I,9,IP)+PY(I,10,IP))
 	      QY(I,5) =HALF*(QY(I,1)+QY(I,2))+FAC*PY(I,5,IP)
 	      QY(I,6) =HALF*(QY(I,2)+QY(I,3))+FAC*PY(I,6,IP)
 	      QY(I,7) =HALF*(QY(I,1)+QY(I,3))+FAC*PY(I,7,IP)
 	      QY(I,8) =HALF*(QY(I,1)+QY(I,4))+FAC*PY(I,8,IP)
 	      QY(I,9) =HALF*(QY(I,2)+QY(I,4))+FAC*PY(I,9,IP)
 	      QY(I,10)=HALF*(QY(I,3)+QY(I,4))+FAC*PY(I,10,IP)

 	      QZ(I,1)=PZ(I,1,IP)+HALF*(PZ(I,5,IP)+PZ(I,7,IP)+PZ(I,8,IP))
 	      QZ(I,2)=PZ(I,2,IP)+HALF*(PZ(I,5,IP)+PZ(I,6,IP)+PZ(I,9,IP))
 	      QZ(I,3)=PZ(I,3,IP)+HALF*(PZ(I,6,IP)+PZ(I,7,IP)+PZ(I,10,IP))
 	      QZ(I,4)=PZ(I,4,IP)+HALF*(PZ(I,8,IP)+PZ(I,9,IP)+PZ(I,10,IP))
 	      QZ(I,5) =HALF*(QZ(I,1)+QZ(I,2))+FAC*PZ(I,5,IP)
 	      QZ(I,6) =HALF*(QZ(I,2)+QZ(I,3))+FAC*PZ(I,6,IP)
 	      QZ(I,7) =HALF*(QZ(I,1)+QZ(I,3))+FAC*PZ(I,7,IP)
 	      QZ(I,8) =HALF*(QZ(I,1)+QZ(I,4))+FAC*PZ(I,8,IP)
 	      QZ(I,9) =HALF*(QZ(I,2)+QZ(I,4))+FAC*PZ(I,9,IP)
 	      QZ(I,10)=HALF*(QZ(I,3)+QZ(I,4))+FAC*PZ(I,10,IP)

 	    END DO
C
C           Diagonal of M-1 Transpose(B).B
            DO N=1,10
              DO I=LFT,LLT
                UL(I,N) =UL(I,N) + VOL(I,IP) *
     +          ( QX(I,N)*PX(I,N,IP) + QY(I,N)*PY(I,N,IP) + QZ(I,N)*PZ(I,N,IP) )
              ENDDO
            ENDDO
          ENDDO
C
          DO I=LFT,LLT
            AA   = MAX(UL(I,1),UL(I,2),UL(I,3),UL(I,4))
            BB   = MAX(UL(I,5),UL(I,6),UL(I,7),UL(I,8),UL(I,9),UL(I,10))
            D(I) = MAX(D(I),TWO*MAX(AA,BB))
          ENDDO
C
        END IF ! IF(NITER==0)THEN

        DO I=LFT,LLT
C
C         DELTAX2 is not used w/IDT1TET10
          DELTAX2(I) = ONE 
C
C         beware : VOLO = GBUF%VOL stored in RD Starter = VOLG / 4 
          MASS       = VOLG(I) ! (multiplied by RHOG0)
          MREF       = FOURTH * MASS
          DELTAX(I)  = TWO*SQRT(MREF/D(I))
        END DO

      ELSE ! IDT1TET10==0 .OR. ISROT==1
        IF(ISROT == 0)THEN
C
          DO N=1,10
            DO I=LFT,LLT
              UL(I,N) = ZERO
            ENDDO
          ENDDO
C
          DO IP=1,NPT
C
            DO N=1,10
              DO I=LFT,LLT
        	UL(I,N) =UL(I,N) + VOL(I,IP) *
     +  	( PX(I,N,IP)*PX(I,N,IP) + PY(I,N,IP)*PY(I,N,IP)
     +         + PZ(I,N,IP)*PZ(I,N,IP) )
              ENDDO
            ENDDO
          ENDDO
C
          DO I=LFT,LLT
            AA = MAX(UL(I,1),UL(I,2),UL(I,3),UL(I,4))
            BB = MAX(UL(I,5),UL(I,6),UL(I,7),UL(I,8),UL(I,9),UL(I,10))
            DELTAX2(I) = AA/MAX(AA,BB)
            AA = AA*THIRTY2
            BB = BB*FOURTY8/SEVEN
            DELTAX(I) = SQRT(TWO*VOLG(I)/MAX(AA,BB))
          END DO
        ELSEIF (ISROT==2.AND.(IINT==0.OR.IDT1SOL>0)) THEN
            DO I=LFT,LLT
             B1(I) =  TY(I)*SZ(I) - SY(I)*TZ(I)
             B2(I) =  RY(I)*TZ(I) - TY(I)*RZ(I)
             B3(I) =  SY(I)*RZ(I) - RY(I)*SZ(I)
             BB4(I) =  -(B1(I) + B2(I) + B3(I))
C
             C1(I) =  TZ(I)*SX(I) - SZ(I)*TX(I)
             C2(I) =  RZ(I)*TX(I) - TZ(I)*RX(I)
             C3(I) =  SZ(I)*RX(I) - RZ(I)*SX(I)
             C4(I) =  -(C1(I) + C2(I) + C3(I))
C
             D1(I) =  TX(I)*SY(I) - SX(I)*TY(I)
             D2(I) =  RX(I)*TY(I) - TX(I)*RY(I)
             D3(I) =  SX(I)*RY(I) - RX(I)*SY(I)
             D4(I) =  -(D1(I) + D2(I) + D3(I))
             DET6 = RX(I)*B1(I)+RY(I)*C1(I)+RZ(I)*D1(I)
             DD = ONE/DET6
             P4X1(I)= B1(I)*DD
             P4Y1(I)= C1(I)*DD
             P4Z1(I)= D1(I)*DD
             P4X2(I)= B2(I)*DD
             P4Y2(I)= C2(I)*DD
             P4Z2(I)= D2(I)*DD
             P4X3(I)= B3(I)*DD
             P4Y3(I)= C3(I)*DD
             P4Z3(I)= D3(I)*DD
             P4X4(I)= BB4(I)*DD
             P4Y4(I)= C4(I)*DD
             P4Z4(I)= D4(I)*DD
c             DELTAX2(I) = ONE
            END DO
            DO I=LFT,LLT
             AA  = MAX(RX(I)*RX(I)+RY(I)*RY(I)+RZ(I)*RZ(I),
     .   	       SX(I)*SX(I)+SY(I)*SY(I)+SZ(I)*SZ(I),
     .   	       TX(I)*TX(I)+TY(I)*TY(I)+TZ(I)*TZ(I),
     .   	       (RX(I)-SX(I))*(RX(I)-SX(I))
     +   	 +     (RY(I)-SY(I))*(RY(I)-SY(I))
     +   	 +     (RZ(I)-SZ(I))*(RZ(I)-SZ(I)),
     .   	       (SX(I)-TX(I))*(SX(I)-TX(I))
     +   	 +     (SY(I)-TY(I))*(SY(I)-TY(I))
     +   	 +     (SZ(I)-TZ(I))*(SZ(I)-TZ(I)),
     .   	       (TX(I)-RX(I))*(TX(I)-RX(I))
     +   	 +     (TY(I)-RY(I))*(TY(I)-RY(I))
     +   	 +     (TZ(I)-RZ(I))*(TZ(I)-RZ(I)))
             DELTAX2(I) = AA
            ENDDO
            IF(IFORMDT==0)THEN
             DO I=LFT,LLT
               DD = P4X1(I)*P4X1(I)+P4Y1(I)*P4Y1(I)+P4Z1(I)*P4Z1(I)
     .   	+ P4X2(I)*P4X2(I)+P4Y2(I)*P4Y2(I)+P4Z2(I)*P4Z2(I)
     .   	+ P4X3(I)*P4X3(I)+P4Y3(I)*P4Y3(I)+P4Z3(I)*P4Z3(I)
     .   	+ P4X4(I)*P4X4(I)+P4Y4(I)*P4Y4(I)+P4Z4(I)*P4Z4(I)
               DELTAX(I) = ONE / SQRT(DD)
             END DO

            ELSEIF(IFORMDT==1)THEN

             GFAC=PM(105,MXT(1))
             LD  =TWO*SQRT(MAX(ONE-GFAC,ZERO))+ONE
             DO I=LFT,LLT
              PXX(I)=P4X1(I)*P4X1(I)+P4X2(I)*P4X2(I)+P4X3(I)*P4X3(I)+P4X4(I)*P4X4(I)
              PYY(I)=P4Y1(I)*P4Y1(I)+P4Y2(I)*P4Y2(I)+P4Y3(I)*P4Y3(I)+P4Y4(I)*P4Y4(I)
              PZZ(I)=P4Z1(I)*P4Z1(I)+P4Z2(I)*P4Z2(I)+P4Z3(I)*P4Z3(I)+P4Z4(I)*P4Z4(I)
              PXY(I)=P4X1(I)*P4Y1(I)+P4X2(I)*P4Y2(I)+P4X3(I)*P4Y3(I)+P4X4(I)*P4Y4(I)
              PXZ(I)=P4X1(I)*P4Z1(I)+P4X2(I)*P4Z2(I)+P4X3(I)*P4Z3(I)+P4X4(I)*P4Z4(I)
              PYZ(I)=P4Y1(I)*P4Z1(I)+P4Y2(I)*P4Z2(I)+P4Y3(I)*P4Z3(I)+P4Y4(I)*P4Z4(I)
C
              AA = -(PXX(I)+PYY(I)+PZZ(I))
              BB =  (PXX(I)*PYY(I)+PXX(I)*PZZ(I)+PYY(I)*PZZ(I)-PXY(I)**2-PXZ(I)**2-PYZ(I)**2) 
              P  = BB-THIRD*AA*AA
              DD = TWO*SQRT(THIRD*MAX(-P,ZERO))-THIRD*AA
C
              DD=LD*DD
C
              DELTAX(I) = ONE / SQRT(DD)
             END DO

            ELSEIF(IFORMDT==2)THEN

             GFAC=PM(105,MXT(1))
             DO I=LFT,LLT
              PXX(I)=P4X1(I)*P4X1(I)+P4X2(I)*P4X2(I)+P4X3(I)*P4X3(I)+P4X4(I)*P4X4(I)
              PYY(I)=P4Y1(I)*P4Y1(I)+P4Y2(I)*P4Y2(I)+P4Y3(I)*P4Y3(I)+P4Y4(I)*P4Y4(I)
              PZZ(I)=P4Z1(I)*P4Z1(I)+P4Z2(I)*P4Z2(I)+P4Z3(I)*P4Z3(I)+P4Z4(I)*P4Z4(I)
              PXY(I)=P4X1(I)*P4Y1(I)+P4X2(I)*P4Y2(I)+P4X3(I)*P4Y3(I)+P4X4(I)*P4Y4(I)
              PXZ(I)=P4X1(I)*P4Z1(I)+P4X2(I)*P4Z2(I)+P4X3(I)*P4Z3(I)+P4X4(I)*P4Z4(I)
              PYZ(I)=P4Y1(I)*P4Z1(I)+P4Y2(I)*P4Z2(I)+P4Y3(I)*P4Z3(I)+P4Y4(I)*P4Z4(I)
C
              AA = -(PXX(I)+PYY(I)+PZZ(I))
              BB =  GFAC*(PXX(I)*PYY(I)+PXX(I)*PZZ(I)+PYY(I)*PZZ(I)-PXY(I)**2-PXZ(I)**2-PYZ(I)**2) 
              P  = BB-THIRD*AA*AA
              DD = TWO*SQRT(THIRD*MAX(-P,ZERO))-THIRD*AA
C
              DELTAX(I) = ONE / SQRT(DD)
             END DO
            END IF!(IFORMDT==0)THEN
        ELSE
          DO I=LFT,LLT

            A1X = RY(I)*SZ(I)-RZ(I)*SY(I)
            A1Y = RZ(I)*SX(I)-RX(I)*SZ(I)
            A1Z = RX(I)*SY(I)-RY(I)*SX(I)
            A1 = A1X*A1X+A1Y*A1Y+A1Z*A1Z

            A2X = SY(I)*TZ(I)-SZ(I)*TY(I)
            A2Y = SZ(I)*TX(I)-SX(I)*TZ(I)
            A2Z = SX(I)*TY(I)-SY(I)*TX(I)
            A2 = A2X*A2X+A2Y*A2Y+A2Z*A2Z

            A3X = TY(I)*RZ(I)-TZ(I)*RY(I)
            A3Y = TZ(I)*RX(I)-TX(I)*RZ(I)
            A3Z = TX(I)*RY(I)-TY(I)*RX(I)
            A3 = A3X*A3X+A3Y*A3Y+A3Z*A3Z

            A4X = A1X+A2X+A3X
            A4Y = A1Y+A2Y+A3Y
            A4Z = A1Z+A2Z+A3Z
            A4 = A4X*A4X+A4Y*A4Y+A4Z*A4Z

            DELTAX(I) = SIX*VOLG(I)/SQRT(MAX(A1,A2,A3,A4))
c minoration deltax car inertie constante avec marge 
c d'un facteur sqrt(8) sur le cot    AA0 
c approche a partir du volume initial
            AA0 = ((SIX*SQR2*VOLG(I))**TWO_THIRD) * EIGHT
            AA  = MAX(RX(I)*RX(I)+RY(I)*RY(I)+RZ(I)*RZ(I),
     .  	      SX(I)*SX(I)+SY(I)*SY(I)+SZ(I)*SZ(I),
     .  	      TX(I)*TX(I)+TY(I)*TY(I)+TZ(I)*TZ(I),
     .  	      (RX(I)-SX(I))*(RX(I)-SX(I))
     +  	+     (RY(I)-SY(I))*(RY(I)-SY(I))
     +  	+     (RZ(I)-SZ(I))*(RZ(I)-SZ(I)),
     .  	      (SX(I)-TX(I))*(SX(I)-TX(I))
     +  	+     (SY(I)-TY(I))*(SY(I)-TY(I))
     +  	+     (SZ(I)-TZ(I))*(SZ(I)-TZ(I)),
     .  	      (TX(I)-RX(I))*(TX(I)-RX(I))
     +  	+     (TY(I)-RY(I))*(TY(I)-RY(I))
     +  	+     (TZ(I)-RZ(I))*(TZ(I)-RZ(I)))
c            DELTAX(I) = DELTAX(I)*MIN(ONE,AA0/AA)
            DELTAX2(I) = AA
          END DO
        END IF

      END IF
C
      RETURN
      END

