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
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10LEN3(VOL,NGL,DELTAX,DELTAX2,
     .   PX,  PY,  PZ,VOLG,VOLGO,
     .   RX,  RY,  RZ,  SX,  SY,  SZ , TX, TY, TZ,
     .   NC,NEL,MXT,PM,
     .   TAGELEM_SMS,V_PITER)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "units_c.inc"
#include      "impl1_c.inc"
#include      "com01_c.inc"
#include      "com06_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "scr07_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGL(*), NC(MVSIZ,10), NEL, MXT(MVSIZ), TAGELEM_SMS(*)
C     REAL
      my_real
     .   VOL(MVSIZ,5),DELTAX(*),DELTAX2(*),VOLG(*),
     .   RX(*),RY(*),RZ(*), SX(*),SY(*),SZ(*), TX(*),TY(*),TZ(*),
     .   PX(MVSIZ,10,5),PY(MVSIZ,10,5),PZ(MVSIZ,10,5),
     .   VOLGO(NEL),
     .   PM(NPROPM,*),V_PITER(NEL,3,10)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IP,N,M,J
      INTEGER ITER,NITER,INIT_PITER
      my_real
     .   UL(MVSIZ,10),
     .   PXX(MVSIZ),PYY(MVSIZ),PZZ(MVSIZ),PXY(MVSIZ),PYZ(MVSIZ),PXZ(MVSIZ),FACEIGV(MVSIZ),
     .   QX(MVSIZ,10),QY(MVSIZ,10),QZ(MVSIZ,10)
      my_real
     .   D(MVSIZ),GFAC,BFAC,LD,P,MASS,MREF,FAC,
     .   AA,BB,A1,A2,A3,A4,
     .   A1X,A2X,A3X,A4X,A1Y,A2Y,A3Y,A4Y,A1Z,A2Z,A3Z,A4Z,AA0
      my_real
     .   UX(MVSIZ,10), UY(MVSIZ,10), UZ(MVSIZ,10), VX(MVSIZ,10), VY(MVSIZ,10), VZ(MVSIZ,10), 
     .   NV(MVSIZ), LL, BU(MVSIZ,6), NINV(MVSIZ)
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
C-----------------------------------------------
      FAC     = ONE/(NINE+THIRD) ! cf FACIROT == NINE+THIRD 
      IF(IDT1TET10/=0 .AND. ISROT==0)THEN

        IF(IDTMINS==0)THEN
          DO I=LFT,LLT
            FACEIGV(I)=TRHEE_OVER_14    ! computing eigenvalues of Me-1 Ke
          END DO
        ELSE
C-------- AMS (possible mix of elements inside the group)
          DO I=LFT,LLT
            IF(TAGELEM_SMS(I)==0)THEN
              FACEIGV(I)=TRHEE_OVER_14    ! computing eigenvalues of Me-1 Ke, TRHEE_OVER_14 = (1/32) / (7/48) rapport des masses
            ELSE
              FACEIGV(I)=ONE           ! compute here maximum eigenvalue K of Ke 
                                      ! => Retrieve K in mqviscb.F
                                      ! Non diagonal mass is computed in mqviscb.F, considering Maximum eigenvalue of Me-1 Ke >= K / Minimum eigenvalue of Me
            END IF
          END DO
        ENDIF
C
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
              DO N=1,4
                DO I=LFT,LLT
                  PXX(I) =PXX(I) + VOL(I,IP)*PX(I,N,IP)*PX(I,N,IP)
                  PYY(I) =PYY(I) + VOL(I,IP)*PY(I,N,IP)*PY(I,N,IP)
                  PZZ(I) =PZZ(I) + VOL(I,IP)*PZ(I,N,IP)*PZ(I,N,IP)
C                 PXY(I) =PXY(I) + VOL(I,IP)*PX(I,N,IP)*PY(I,N,IP)
C                 PXZ(I) =PXZ(I) + VOL(I,IP)*PX(I,N,IP)*PZ(I,N,IP)
C                 PYZ(I) =PYZ(I) + VOL(I,IP)*PY(I,N,IP)*PZ(I,N,IP)
                ENDDO
              ENDDO
              DO N=5,10
                DO I=LFT,LLT
                  PXX(I) =PXX(I) + FACEIGV(I)*VOL(I,IP)*PX(I,N,IP)*PX(I,N,IP)
                  PYY(I) =PYY(I) + FACEIGV(I)*VOL(I,IP)*PY(I,N,IP)*PY(I,N,IP)
                  PZZ(I) =PZZ(I) + FACEIGV(I)*VOL(I,IP)*PZ(I,N,IP)*PZ(I,N,IP)
C                 PXY(I) =PXY(I) + FACEIGV(I)*VOL(I,IP)*PX(I,N,IP)*PY(I,N,IP)
C                 PXZ(I) =PXZ(I) + FACEIGV(I)*VOL(I,IP)*PX(I,N,IP)*PZ(I,N,IP)
C                 PYZ(I) =PYZ(I) + FACEIGV(I)*VOL(I,IP)*PY(I,N,IP)*PZ(I,N,IP)
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
     .                 FACEIGV(I)*(PX(I,5,IP)*PX(I,5,IP)+PX(I,6,IP)*PX(I,6,IP)+PX(I,7,IP)*PX(I,7,IP)+
     .                            PX(I,8,IP)*PX(I,8,IP)+PX(I,9,IP)*PX(I,9,IP)+PX(I,10,IP)*PX(I,10,IP))
                PYY(I)=PY(I,1,IP)*PY(I,1,IP)+PY(I,2,IP)*PY(I,2,IP)+PY(I,3,IP)*PY(I,3,IP)+PY(I,4,IP)*PY(I,4,IP)+
     .                 FACEIGV(I)*(PY(I,5,IP)*PY(I,5,IP) +PY(I,6,IP)*PY(I,6,IP)+PY(I,7,IP)*PY(I,7,IP)+
     .                            PY(I,8,IP)*PY(I,8,IP)+PY(I,9,IP)*PY(I,9,IP)+PY(I,10,IP)*PY(I,10,IP))
                PZZ(I)=PZ(I,1,IP)*PZ(I,1,IP)+PZ(I,2,IP)*PZ(I,2,IP)+PZ(I,3,IP)*PZ(I,3,IP)+PZ(I,4,IP)*PZ(I,4,IP)+
     .                 FACEIGV(I)*(PZ(I,5,IP)*PZ(I,5,IP)+PZ(I,6,IP)*PZ(I,6,IP)+PZ(I,7,IP)*PZ(I,7,IP)+
     .                            PZ(I,8,IP)*PZ(I,8,IP)+PZ(I,9,IP)*PZ(I,9,IP)+PZ(I,10,IP)*PZ(I,10,IP))
                PXY(I)=PX(I,1,IP)*PY(I,1,IP)+PX(I,2,IP)*PY(I,2,IP)+PX(I,3,IP)*PY(I,3,IP)+PX(I,4,IP)*PY(I,4,IP)+
     .                 FACEIGV(I)*(PX(I,5,IP)*PY(I,5,IP)+PX(I,6,IP)*PY(I,6,IP)+PX(I,7,IP)*PY(I,7,IP)+
     .                            PX(I,8,IP)*PY(I,8,IP)+PX(I,9,IP)*PY(I,9,IP)+PX(I,10,IP)*PY(I,10,IP)) 
                PXZ(I)=PX(I,1,IP)*PZ(I,1,IP)+PX(I,2,IP)*PZ(I,2,IP)+PX(I,3,IP)*PZ(I,3,IP)+PX(I,4,IP)*PZ(I,4,IP)+ 
     .                 FACEIGV(I)*(PX(I,5,IP)*PZ(I,5,IP)+PX(I,6,IP)*PZ(I,6,IP)+PX(I,7,IP)*PZ(I,7,IP)+
     .                            PX(I,8,IP)*PZ(I,8,IP)+PX(I,9,IP)*PZ(I,9,IP)+PX(I,10,IP)*PZ(I,10,IP)) 
                PYZ(I)=PY(I,1,IP)*PZ(I,1,IP)+PY(I,2,IP)*PZ(I,2,IP)+PY(I,3,IP)*PZ(I,3,IP)+PY(I,4,IP)*PZ(I,4,IP)+
     .                 FACEIGV(I)*(PY(I,5,IP)*PZ(I,5,IP)+PY(I,6,IP)*PZ(I,6,IP)+PY(I,7,IP)*PZ(I,7,IP)+
     .                            PY(I,8,IP)*PZ(I,8,IP)+PY(I,9,IP)*PZ(I,9,IP)+PY(I,10,IP)*PZ(I,10,IP)) 
              ENDDO

              DO I=LFT,LLT
                AA = -(PXX(I)+PYY(I)+PZZ(I))
                BB =  (PXX(I)*PYY(I)+PXX(I)*PZZ(I)+PYY(I)*PZZ(I)-PXY(I)**2-PXZ(I)**2-PYZ(I)**2) 
                P  = BB-THIRD*AA*AA
                D(I)  = D(I)+ LD * VOL(I,IP) * (TWO*SQRT(THIRD*MAX(-P,ZERO))-THIRD*AA)
              ENDDO
            ENDDO

          ELSEIF(IFORMDT==2)THEN

            D(LFT:LLT)=ZERO

            GFAC=PM(105,MXT(1))

            DO IP=1,NPT

              DO I=LFT,LLT
                PXX(I)=PX(I,1,IP)*PX(I,1,IP)+PX(I,2,IP)*PX(I,2,IP)+PX(I,3,IP)*PX(I,3,IP)+PX(I,4,IP)*PX(I,4,IP)+
     .                 FACEIGV(I)*(PX(I,5,IP)*PX(I,5,IP)+PX(I,6,IP)*PX(I,6,IP)+PX(I,7,IP)*PX(I,7,IP)+
     .                            PX(I,8,IP)*PX(I,8,IP)+PX(I,9,IP)*PX(I,9,IP)+PX(I,10,IP)*PX(I,10,IP))
                PYY(I)=PY(I,1,IP)*PY(I,1,IP)+PY(I,2,IP)*PY(I,2,IP)+PY(I,3,IP)*PY(I,3,IP)+PY(I,4,IP)*PY(I,4,IP)+
     .                 FACEIGV(I)*(PY(I,5,IP)*PY(I,5,IP) +PY(I,6,IP)*PY(I,6,IP)+PY(I,7,IP)*PY(I,7,IP)+
     .                            PY(I,8,IP)*PY(I,8,IP)+PY(I,9,IP)*PY(I,9,IP)+PY(I,10,IP)*PY(I,10,IP))
                PZZ(I)=PZ(I,1,IP)*PZ(I,1,IP)+PZ(I,2,IP)*PZ(I,2,IP)+PZ(I,3,IP)*PZ(I,3,IP)+PZ(I,4,IP)*PZ(I,4,IP)+
     .                 FACEIGV(I)*(PZ(I,5,IP)*PZ(I,5,IP)+PZ(I,6,IP)*PZ(I,6,IP)+PZ(I,7,IP)*PZ(I,7,IP)+
     .                            PZ(I,8,IP)*PZ(I,8,IP)+PZ(I,9,IP)*PZ(I,9,IP)+PZ(I,10,IP)*PZ(I,10,IP))
                PXY(I)=PX(I,1,IP)*PY(I,1,IP)+PX(I,2,IP)*PY(I,2,IP)+PX(I,3,IP)*PY(I,3,IP)+PX(I,4,IP)*PY(I,4,IP)+
     .                 FACEIGV(I)*(PX(I,5,IP)*PY(I,5,IP)+PX(I,6,IP)*PY(I,6,IP)+PX(I,7,IP)*PY(I,7,IP)+
     .                            PX(I,8,IP)*PY(I,8,IP)+PX(I,9,IP)*PY(I,9,IP)+PX(I,10,IP)*PY(I,10,IP)) 
                PXZ(I)=PX(I,1,IP)*PZ(I,1,IP)+PX(I,2,IP)*PZ(I,2,IP)+PX(I,3,IP)*PZ(I,3,IP)+PX(I,4,IP)*PZ(I,4,IP)+ 
     .                 FACEIGV(I)*(PX(I,5,IP)*PZ(I,5,IP)+PX(I,6,IP)*PZ(I,6,IP)+PX(I,7,IP)*PZ(I,7,IP)+
     .                            PX(I,8,IP)*PZ(I,8,IP)+PX(I,9,IP)*PZ(I,9,IP)+PX(I,10,IP)*PZ(I,10,IP)) 
                PYZ(I)=PY(I,1,IP)*PZ(I,1,IP)+PY(I,2,IP)*PZ(I,2,IP)+PY(I,3,IP)*PZ(I,3,IP)+PY(I,4,IP)*PZ(I,4,IP)+
     .                 FACEIGV(I)*(PY(I,5,IP)*PZ(I,5,IP)+PY(I,6,IP)*PZ(I,6,IP)+PY(I,7,IP)*PZ(I,7,IP)+
     .                            PY(I,8,IP)*PZ(I,8,IP)+PY(I,9,IP)*PZ(I,9,IP)+PY(I,10,IP)*PZ(I,10,IP)) 
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
C         Iterative Power (Could be adapted vs IFORM taking into account Hook's Matrix)    
C--------------------------------------------------------------------------------------
          INIT_PITER=0
          IF(NCYCLE==0)THEN
            INIT_PITER=1
            DO N=1,10
              IF(V_PITER(1,1,N)/=ZERO.OR.V_PITER(1,2,N)/=ZERO.OR.V_PITER(1,3,N)/=ZERO)INIT_PITER=0
            END DO
          END IF

          IF(INIT_PITER==1)THEN
C
C           Initialize at 1st time DT1TET10 has been encountered... Note : U normalized
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
              DO I=LFT,LLT
                UX(I,N)=WX(N)
                UY(I,N)=WY(N)
                UZ(I,N)=WZ(N)
              END DO
            END DO
          ELSE
C
C           Get vector from previous cycle / run
            UX(LFT:LLT,1:10)=V_PITER(LFT:LLT,1,1:10)
            UY(LFT:LLT,1:10)=V_PITER(LFT:LLT,2,1:10)
            UZ(LFT:LLT,1:10)=V_PITER(LFT:LLT,3,1:10)
          END IF

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

          DO ITER=1,NITER


            VX(LFT:LLT,1:10)=ZERO
            VY(LFT:LLT,1:10)=ZERO
            VZ(LFT:LLT,1:10)=ZERO

            DO IP=1,NPT
C
C             Bip.U
              BU(LFT:LLT,1:6)=ZERO
              DO N=1,10
                DO I=LFT,LLT
                  BU(I,1)=BU(I,1)+PX(I,N,IP)*UX(I,N)
                  BU(I,2)=BU(I,2)+PY(I,N,IP)*UY(I,N)
                  BU(I,3)=BU(I,3)+PZ(I,N,IP)*UZ(I,N)
                  BU(I,4)=BU(I,4)+PY(I,N,IP)*UX(I,N)+PX(I,N,IP)*UY(I,N)
                  BU(I,5)=BU(I,5)+PZ(I,N,IP)*UY(I,N)+PY(I,N,IP)*UZ(I,N)
                  BU(I,6)=BU(I,6)+PZ(I,N,IP)*UX(I,N)+PX(I,N,IP)*UZ(I,N)
                END DO
              END DO
C
              DO J=1,3
                DO I=LFT,LLT
                  BU(I,J)=BFAC*BU(I,J)
                END DO
              END DO
              DO J=4,6
                DO I=LFT,LLT
                  BU(I,J)=GFAC*BU(I,J)
                END DO
              END DO
C
C             Vol_ip * Transpose(Bip).Bip.U
              DO N=1,10
                DO I=LFT,LLT
                  VX(I,N)=VX(I,N)+VOL(I,IP)*(PX(I,N,IP)*BU(I,1)+PY(I,N,IP)*BU(I,4)+PZ(I,N,IP)*BU(I,6))
                  VY(I,N)=VY(I,N)+VOL(I,IP)*(PY(I,N,IP)*BU(I,2)+PX(I,N,IP)*BU(I,4)+PZ(I,N,IP)*BU(I,5))
                  VZ(I,N)=VZ(I,N)+VOL(I,IP)*(PZ(I,N,IP)*BU(I,3)+PY(I,N,IP)*BU(I,5)+PX(I,N,IP)*BU(I,6))
                END DO
              END DO

            END DO

            NV(LFT:LLT) = ZERO
            DO N=1,4
              DO I=LFT,LLT
                NV(I) = NV(I) + VX(I,N)*VX(I,N)+VY(I,N)*VY(I,N)+VZ(I,N)*VZ(I,N)
              END DO
            END DO
            DO N=5,10
              DO I=LFT,LLT
                VX(I,N)=FACEIGV(I)*VX(I,N)
                VY(I,N)=FACEIGV(I)*VY(I,N)
                VZ(I,N)=FACEIGV(I)*VZ(I,N)
                NV(I) = NV(I) + VX(I,N)*VX(I,N)+VY(I,N)*VY(I,N)+VZ(I,N)*VZ(I,N)
              END DO
            END DO
            DO I=LFT,LLT
              NV(I)  =SQRT(NV(I))
              NINV(I)=ONE/MAX(EM20,NV(I))
            END DO
            DO N=1,10
              DO I=LFT,LLT
                VX(I,N)=NINV(I) * VX(I,N)
                VY(I,N)=NINV(I) * VY(I,N)
                VZ(I,N)=NINV(I) * VZ(I,N)
              END DO
            END DO
            UX(LFT:LLT,1:10)=VX(LFT:LLT,1:10)
            UY(LFT:LLT,1:10)=VY(LFT:LLT,1:10)
            UZ(LFT:LLT,1:10)=VZ(LFT:LLT,1:10)

          END DO ! DO ITER=1,NITER
C
          DO I=LFT,LLT
            D(I)=NV(I)
          END DO
          V_PITER(LFT:LLT,1,1:10)=UX(LFT:LLT,1:10)
          V_PITER(LFT:LLT,2,1:10)=UY(LFT:LLT,1:10)
          V_PITER(LFT:LLT,3,1:10)=UZ(LFT:LLT,1:10)
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
            BB   = FACEIGV(I)*MAX(UL(I,5),UL(I,6),UL(I,7),UL(I,8),UL(I,9),UL(I,10))
            D(I) = MAX(D(I),TWO*MAX(AA,BB))
          ENDDO
C
        END IF ! IF(NITER==0)THEN

        DO I=LFT,LLT
C
C         DELTAX2 is not used w/IDT1TET10
          DELTAX2(I) = ONE 
C
          MASS       = VOLGO(I) ! (multiplied by RHOG0)

          MREF       = ONE/THIRTY2 * MASS
          DELTAX(I)  = TWO*SQRT(MREF/D(I))
        ENDDO

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
C               QX(I,5)=HALF*(PX(I,1,IP)+PX(I,2,IP))+(HALF+ONE/FACIROT)*PX(I,5,IP)
C                        +FOURTH(PX(I,6,IP)+PX(I,7,IP)+PX(I,8,IP)+PX(I,9,IP)
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
C             B M-1 Transpose(B)
              DO I=LFT,LLT
                PXX(I)=PXX(I)+
     .                 PX(I,1,IP)*QX(I,1)+PX(I,2,IP)*QX(I,2)+PX(I,3,IP)*QX(I,3)+PX(I,4,IP)*QX(I,4)+
     .                 PX(I,5,IP)*QX(I,5)+PX(I,6,IP)*QX(I,6)+PX(I,7,IP)*QX(I,7)+
     .                            PX(I,8,IP)*QX(I,8)+PX(I,9,IP)*QX(I,9)+PX(I,10,IP)*QX(I,10)
                PYY(I)=PYY(I)+
     .                 PY(I,1,IP)*QY(I,1)+PY(I,2,IP)*QY(I,2)+PY(I,3,IP)*QY(I,3)+PY(I,4,IP)*QY(I,4)+
     .                 PY(I,5,IP)*QY(I,5) +PY(I,6,IP)*QY(I,6)+PY(I,7,IP)*QY(I,7)+
     .                            PY(I,8,IP)*QY(I,8)+PY(I,9,IP)*QY(I,9)+PY(I,10,IP)*QY(I,10)
                PZZ(I)=PZZ(I)+
     .                 PZ(I,1,IP)*QZ(I,1)+PZ(I,2,IP)*QZ(I,2)+PZ(I,3,IP)*QZ(I,3)+PZ(I,4,IP)*QZ(I,4)+
     .                 PZ(I,5,IP)*QZ(I,5)+PZ(I,6,IP)*QZ(I,6)+PZ(I,7,IP)*QZ(I,7)+
     .                            PZ(I,8,IP)*QZ(I,8)+PZ(I,9,IP)*QZ(I,9)+PZ(I,10,IP)*QZ(I,10)
C               PXY(I)=PX(I,1,IP)*QY(I,1)+PX(I,2,IP)*QY(I,2)+PX(I,3,IP)*QY(I,3)+PX(I,4,IP)*QY(I,4)+
C     .                PX(I,5,IP)*QY(I,5)+PX(I,6,IP)*QY(I,6)+PX(I,7,IP)*QY(I,7)+
C     .                           PX(I,8,IP)*QY(I,8)+PX(I,9,IP)*QY(I,9)+PX(I,10,IP)*QY(I,10)
C               PXZ(I)=PX(I,1,IP)*QZ(I,1)+PX(I,2,IP)*QZ(I,2)+PX(I,3,IP)*QZ(I,3)+PX(I,4,IP)*QZ(I,4)+ 
C     .                PX(I,5,IP)*QZ(I,5)+PX(I,6,IP)*QZ(I,6)+PX(I,7,IP)*QZ(I,7)+
C     .                           PX(I,8,IP)*QZ(I,8)+PX(I,9,IP)*QZ(I,9)+PX(I,10,IP)*QZ(I,10)
C               PYZ(I)=PY(I,1,IP)*QZ(I,1)+PY(I,2,IP)*QZ(I,2)+PY(I,3,IP)*QZ(I,3)+PY(I,4,IP)*QZ(I,4)+
C     .                PY(I,5,IP)*QZ(I,5)+PY(I,6,IP)*QZ(I,6)+PY(I,7,IP)*QZ(I,7)+
C     .                           PY(I,8,IP)*QZ(I,8)+PY(I,9,IP)*QZ(I,9)+PY(I,10,IP)*QZ(I,10)
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
C               QX(I,5)=HALF*(PX(I,1,IP)+PX(I,2,IP))+(HALF+ONE/FACIROT)*PX(I,5,IP)
C                        +FOURTH(PX(I,6,IP)+PX(I,7,IP)+PX(I,8,IP)+PX(I,9,IP)
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
C             B M-1 Transpose(B)
              DO I=LFT,LLT
                PXX(I)=PX(I,1,IP)*QX(I,1)+PX(I,2,IP)*QX(I,2)+PX(I,3,IP)*QX(I,3)+PX(I,4,IP)*QX(I,4)+
     .                 PX(I,5,IP)*QX(I,5)+PX(I,6,IP)*QX(I,6)+PX(I,7,IP)*QX(I,7)+
     .                            PX(I,8,IP)*QX(I,8)+PX(I,9,IP)*QX(I,9)+PX(I,10,IP)*QX(I,10)
                PYY(I)=PY(I,1,IP)*QY(I,1)+PY(I,2,IP)*QY(I,2)+PY(I,3,IP)*QY(I,3)+PY(I,4,IP)*QY(I,4)+
     .                 PY(I,5,IP)*QY(I,5) +PY(I,6,IP)*QY(I,6)+PY(I,7,IP)*QY(I,7)+
     .                            PY(I,8,IP)*QY(I,8)+PY(I,9,IP)*QY(I,9)+PY(I,10,IP)*QY(I,10)
                PZZ(I)=PZ(I,1,IP)*QZ(I,1)+PZ(I,2,IP)*QZ(I,2)+PZ(I,3,IP)*QZ(I,3)+PZ(I,4,IP)*QZ(I,4)+
     .                 PZ(I,5,IP)*QZ(I,5)+PZ(I,6,IP)*QZ(I,6)+PZ(I,7,IP)*QZ(I,7)+
     .                            PZ(I,8,IP)*QZ(I,8)+PZ(I,9,IP)*QZ(I,9)+PZ(I,10,IP)*QZ(I,10)
                PXY(I)=PX(I,1,IP)*QY(I,1)+PX(I,2,IP)*QY(I,2)+PX(I,3,IP)*QY(I,3)+PX(I,4,IP)*QY(I,4)+
     .                 PX(I,5,IP)*QY(I,5)+PX(I,6,IP)*QY(I,6)+PX(I,7,IP)*QY(I,7)+
     .                            PX(I,8,IP)*QY(I,8)+PX(I,9,IP)*QY(I,9)+PX(I,10,IP)*QY(I,10)
                PXZ(I)=PX(I,1,IP)*QZ(I,1)+PX(I,2,IP)*QZ(I,2)+PX(I,3,IP)*QZ(I,3)+PX(I,4,IP)*QZ(I,4)+ 
     .                 PX(I,5,IP)*QZ(I,5)+PX(I,6,IP)*QZ(I,6)+PX(I,7,IP)*QZ(I,7)+
     .                            PX(I,8,IP)*QZ(I,8)+PX(I,9,IP)*QZ(I,9)+PX(I,10,IP)*QZ(I,10)
                PYZ(I)=PY(I,1,IP)*QZ(I,1)+PY(I,2,IP)*QZ(I,2)+PY(I,3,IP)*QZ(I,3)+PY(I,4,IP)*QZ(I,4)+
     .                 PY(I,5,IP)*QZ(I,5)+PY(I,6,IP)*QZ(I,6)+PY(I,7,IP)*QZ(I,7)+
     .                            PY(I,8,IP)*QZ(I,8)+PY(I,9,IP)*QZ(I,9)+PY(I,10,IP)*QZ(I,10)
              ENDDO

              DO I=LFT,LLT
                AA = -(PXX(I)+PYY(I)+PZZ(I))
                BB =  (PXX(I)*PYY(I)+PXX(I)*PZZ(I)+PYY(I)*PZZ(I)-PXY(I)**2-PXZ(I)**2-PYZ(I)**2) 
                P  = BB-THIRD*AA*AA
                D(I)  = D(I)+ LD * VOL(I,IP) * (TWO*SQRT(THIRD*MAX(-P,ZERO))-THIRD*AA)
              ENDDO
            ENDDO

          ELSEIF(IFORMDT==2)THEN

            D(LFT:LLT)=ZERO

            GFAC=PM(105,MXT(1))

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
C               QX(I,5)=HALF*(PX(I,1,IP)+PX(I,2,IP))+(HALF+ONE/FACIROT)*PX(I,5,IP)
C                        +FOURTH(PX(I,6,IP)+PX(I,7,IP)+PX(I,8,IP)+PX(I,9,IP)
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
C             B M-1 Transpose(B)
              DO I=LFT,LLT
                PXX(I)=PX(I,1,IP)*QX(I,1)+PX(I,2,IP)*QX(I,2)+PX(I,3,IP)*QX(I,3)+PX(I,4,IP)*QX(I,4)+
     .                 PX(I,5,IP)*QX(I,5)+PX(I,6,IP)*QX(I,6)+PX(I,7,IP)*QX(I,7)+
     .                            PX(I,8,IP)*QX(I,8)+PX(I,9,IP)*QX(I,9)+PX(I,10,IP)*QX(I,10)
                PYY(I)=PY(I,1,IP)*QY(I,1)+PY(I,2,IP)*QY(I,2)+PY(I,3,IP)*QY(I,3)+PY(I,4,IP)*QY(I,4)+
     .                 PY(I,5,IP)*QY(I,5) +PY(I,6,IP)*QY(I,6)+PY(I,7,IP)*QY(I,7)+
     .                            PY(I,8,IP)*QY(I,8)+PY(I,9,IP)*QY(I,9)+PY(I,10,IP)*QY(I,10)
                PZZ(I)=PZ(I,1,IP)*QZ(I,1)+PZ(I,2,IP)*QZ(I,2)+PZ(I,3,IP)*QZ(I,3)+PZ(I,4,IP)*QZ(I,4)+
     .                 PZ(I,5,IP)*QZ(I,5)+PZ(I,6,IP)*QZ(I,6)+PZ(I,7,IP)*QZ(I,7)+
     .                            PZ(I,8,IP)*QZ(I,8)+PZ(I,9,IP)*QZ(I,9)+PZ(I,10,IP)*QZ(I,10)
                PXY(I)=PX(I,1,IP)*QY(I,1)+PX(I,2,IP)*QY(I,2)+PX(I,3,IP)*QY(I,3)+PX(I,4,IP)*QY(I,4)+
     .                 PX(I,5,IP)*QY(I,5)+PX(I,6,IP)*QY(I,6)+PX(I,7,IP)*QY(I,7)+
     .                            PX(I,8,IP)*QY(I,8)+PX(I,9,IP)*QY(I,9)+PX(I,10,IP)*QY(I,10)
                PXZ(I)=PX(I,1,IP)*QZ(I,1)+PX(I,2,IP)*QZ(I,2)+PX(I,3,IP)*QZ(I,3)+PX(I,4,IP)*QZ(I,4)+ 
     .                 PX(I,5,IP)*QZ(I,5)+PX(I,6,IP)*QZ(I,6)+PX(I,7,IP)*QZ(I,7)+
     .                            PX(I,8,IP)*QZ(I,8)+PX(I,9,IP)*QZ(I,9)+PX(I,10,IP)*QZ(I,10)
                PYZ(I)=PY(I,1,IP)*QZ(I,1)+PY(I,2,IP)*QZ(I,2)+PY(I,3,IP)*QZ(I,3)+PY(I,4,IP)*QZ(I,4)+
     .                 PY(I,5,IP)*QZ(I,5)+PY(I,6,IP)*QZ(I,6)+PY(I,7,IP)*QZ(I,7)+
     .                            PY(I,8,IP)*QZ(I,8)+PY(I,9,IP)*QZ(I,9)+PY(I,10,IP)*QZ(I,10)
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
C         Iterative Power (Could be adapted vs IFORM taking into account Hook's Matrix)    
C--------------------------------------------------------------------------------------
          INIT_PITER=0
          IF(NCYCLE==0)THEN
            INIT_PITER=1
            DO N=1,10
              IF(V_PITER(1,1,N)/=ZERO.OR.V_PITER(1,2,N)/=ZERO.OR.V_PITER(1,3,N)/=ZERO)INIT_PITER=0
            END DO

          END IF
C

          IF(INIT_PITER==1)THEN
C
C           Initialize at 1st time DT1TET10 has been encountered... Note : U normalized
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
              DO I=LFT,LLT
                UX(I,N)=WX(N)
                UY(I,N)=WY(N)
                UZ(I,N)=WZ(N)
              END DO
            END DO
          ELSE
C
C           Get vector from previous cycle / run
            UX(LFT:LLT,1:10)=V_PITER(LFT:LLT,1,1:10)
            UY(LFT:LLT,1:10)=V_PITER(LFT:LLT,2,1:10)
            UZ(LFT:LLT,1:10)=V_PITER(LFT:LLT,3,1:10)
          END IF

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

          DO ITER=1,NITER

            VX(LFT:LLT,1:10)=ZERO
            VY(LFT:LLT,1:10)=ZERO
            VZ(LFT:LLT,1:10)=ZERO

            DO IP=1,NPT
C
C             Bip.U
              BU(LFT:LLT,1:6)=ZERO
              DO N=1,10
                DO I=LFT,LLT
                  BU(I,1)=BU(I,1)+PX(I,N,IP)*UX(I,N)
                  BU(I,2)=BU(I,2)+PY(I,N,IP)*UY(I,N)
                  BU(I,3)=BU(I,3)+PZ(I,N,IP)*UZ(I,N)
                  BU(I,4)=BU(I,4)+PY(I,N,IP)*UX(I,N)+PX(I,N,IP)*UY(I,N)
                  BU(I,5)=BU(I,5)+PZ(I,N,IP)*UY(I,N)+PY(I,N,IP)*UZ(I,N)
                  BU(I,6)=BU(I,6)+PZ(I,N,IP)*UX(I,N)+PX(I,N,IP)*UZ(I,N)
                END DO
              END DO
C
              DO J=1,3
                DO I=LFT,LLT
                  BU(I,J)=BFAC*BU(I,J)
                END DO
              END DO
              DO J=4,6
                DO I=LFT,LLT
                  BU(I,J)=GFAC*BU(I,J)
                END DO
              END DO
C
C             Vol_ip * Transpose(Bip).Bip.U
              DO N=1,10
                DO I=LFT,LLT
                  VX(I,N)=VX(I,N)+VOL(I,IP)*(PX(I,N,IP)*BU(I,1)+PY(I,N,IP)*BU(I,4)+PZ(I,N,IP)*BU(I,6))
                  VY(I,N)=VY(I,N)+VOL(I,IP)*(PY(I,N,IP)*BU(I,2)+PX(I,N,IP)*BU(I,4)+PZ(I,N,IP)*BU(I,5))
                  VZ(I,N)=VZ(I,N)+VOL(I,IP)*(PZ(I,N,IP)*BU(I,3)+PY(I,N,IP)*BU(I,5)+PX(I,N,IP)*BU(I,6))
                END DO
              END DO

            END DO

C           M-1 K . U
            DO I=LFT,LLT
              VX(I,1)=VX(I,1)+HALF*(VX(I,5)+VX(I,7)+VX(I,8))   
              VX(I,2)=VX(I,2)+HALF*(VX(I,5)+VX(I,6)+VX(I,9))
              VX(I,3)=VX(I,3)+HALF*(VX(I,6)+VX(I,7)+VX(I,10))
              VX(I,4)=VX(I,4)+HALF*(VX(I,8)+VX(I,9)+VX(I,10))
C             VX(I,5)=HALF*(VX(I,1)+VX(I,2))+(HALF+ONE/FACIROT)*VX(I,5)
C                      +FOURTH(VX(I,6)+VX(I,7)+VX(I,8)+VX(I,9)
              VX(I,5) =HALF*(VX(I,1)+VX(I,2))+FAC*VX(I,5)
              VX(I,6) =HALF*(VX(I,2)+VX(I,3))+FAC*VX(I,6)
              VX(I,7) =HALF*(VX(I,1)+VX(I,3))+FAC*VX(I,7)
              VX(I,8) =HALF*(VX(I,1)+VX(I,4))+FAC*VX(I,8)
              VX(I,9) =HALF*(VX(I,2)+VX(I,4))+FAC*VX(I,9)
              VX(I,10)=HALF*(VX(I,3)+VX(I,4))+FAC*VX(I,10)

              VY(I,1)=VY(I,1)+HALF*(VY(I,5)+VY(I,7)+VY(I,8))   
              VY(I,2)=VY(I,2)+HALF*(VY(I,5)+VY(I,6)+VY(I,9))
              VY(I,3)=VY(I,3)+HALF*(VY(I,6)+VY(I,7)+VY(I,10))
              VY(I,4)=VY(I,4)+HALF*(VY(I,8)+VY(I,9)+VY(I,10))
              VY(I,5) =HALF*(VY(I,1)+VY(I,2))+FAC*VY(I,5)
              VY(I,6) =HALF*(VY(I,2)+VY(I,3))+FAC*VY(I,6)
              VY(I,7) =HALF*(VY(I,1)+VY(I,3))+FAC*VY(I,7)
              VY(I,8) =HALF*(VY(I,1)+VY(I,4))+FAC*VY(I,8)
              VY(I,9) =HALF*(VY(I,2)+VY(I,4))+FAC*VY(I,9)
              VY(I,10)=HALF*(VY(I,3)+VY(I,4))+FAC*VY(I,10)

              VZ(I,1)=VZ(I,1)+HALF*(VZ(I,5)+VZ(I,7)+VZ(I,8))   
              VZ(I,2)=VZ(I,2)+HALF*(VZ(I,5)+VZ(I,6)+VZ(I,9))
              VZ(I,3)=VZ(I,3)+HALF*(VZ(I,6)+VZ(I,7)+VZ(I,10))
              VZ(I,4)=VZ(I,4)+HALF*(VZ(I,8)+VZ(I,9)+VZ(I,10))
              VZ(I,5) =HALF*(VZ(I,1)+VZ(I,2))+FAC*VZ(I,5)
              VZ(I,6) =HALF*(VZ(I,2)+VZ(I,3))+FAC*VZ(I,6)
              VZ(I,7) =HALF*(VZ(I,1)+VZ(I,3))+FAC*VZ(I,7)
              VZ(I,8) =HALF*(VZ(I,1)+VZ(I,4))+FAC*VZ(I,8)
              VZ(I,9) =HALF*(VZ(I,2)+VZ(I,4))+FAC*VZ(I,9)
              VZ(I,10)=HALF*(VZ(I,3)+VZ(I,4))+FAC*VZ(I,10)

            END DO

            NV(LFT:LLT) = ZERO
            DO N=1,10
              DO I=LFT,LLT
                NV(I) = NV(I) + VX(I,N)*VX(I,N)+VY(I,N)*VY(I,N)+VZ(I,N)*VZ(I,N)
              END DO
            END DO
            DO I=LFT,LLT
              NV(I)  =SQRT(NV(I))
              NINV(I)=ONE/MAX(EM20,NV(I))
            END DO
            DO N=1,10
              DO I=LFT,LLT
                VX(I,N)=NINV(I) * VX(I,N)
                VY(I,N)=NINV(I) * VY(I,N)
                VZ(I,N)=NINV(I) * VZ(I,N)
              END DO
            END DO
            UX(LFT:LLT,1:10)=VX(LFT:LLT,1:10)
            UY(LFT:LLT,1:10)=VY(LFT:LLT,1:10)
            UZ(LFT:LLT,1:10)=VZ(LFT:LLT,1:10)

          END DO ! DO ITER=1,NITER
C
          DO I=LFT,LLT
            D(I)=NV(I)
          END DO
          V_PITER(LFT:LLT,1,1:10)=UX(LFT:LLT,1:10)
          V_PITER(LFT:LLT,2,1:10)=UY(LFT:LLT,1:10)
          V_PITER(LFT:LLT,3,1:10)=UZ(LFT:LLT,1:10)
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
C             QX(I,5)=HALF*(PX(I,1,IP)+PX(I,2,IP))+(HALF+ONE/FACIROT)*PX(I,5,IP)
C                      +FOURTH(PX(I,6,IP)+PX(I,7,IP)+PX(I,8,IP)+PX(I,9,IP)
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
          MASS       = VOLGO(I) ! (multiplied by RHOG0)
          MREF       = FOURTH * MASS
          DELTAX(I)  = TWO*SQRT(MREF/D(I)) !

        ENDDO

      ELSE ! IDT1TET10==0 .OR. ISROT==1
C
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
     +          ( PX(I,N,IP)*PX(I,N,IP) + PY(I,N,IP)*PY(I,N,IP)
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
          ENDDO

        ELSE

          IF(ISROT==2.AND.(IINT==0.OR.IDT1SOL>0))THEN
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
     .                 SX(I)*SX(I)+SY(I)*SY(I)+SZ(I)*SZ(I),
     .                 TX(I)*TX(I)+TY(I)*TY(I)+TZ(I)*TZ(I),
     .                 (RX(I)-SX(I))*(RX(I)-SX(I))
     +           +     (RY(I)-SY(I))*(RY(I)-SY(I))
     +           +     (RZ(I)-SZ(I))*(RZ(I)-SZ(I)),
     .                 (SX(I)-TX(I))*(SX(I)-TX(I))
     +           +     (SY(I)-TY(I))*(SY(I)-TY(I))
     +           +     (SZ(I)-TZ(I))*(SZ(I)-TZ(I)),
     .                 (TX(I)-RX(I))*(TX(I)-RX(I))
     +           +     (TY(I)-RY(I))*(TY(I)-RY(I))
     +           +     (TZ(I)-RZ(I))*(TZ(I)-RZ(I)))
             DELTAX2(I) = AA
            ENDDO
            IF(IFORMDT==0)THEN
             DO I=LFT,LLT
               DD = P4X1(I)*P4X1(I)+P4Y1(I)*P4Y1(I)+P4Z1(I)*P4Z1(I)
     .          + P4X2(I)*P4X2(I)+P4Y2(I)*P4Y2(I)+P4Z2(I)*P4Z2(I)
     .          + P4X3(I)*P4X3(I)+P4Y3(I)*P4Y3(I)+P4Z3(I)*P4Z3(I)
     .          + P4X4(I)*P4X4(I)+P4Y4(I)*P4Y4(I)+P4Z4(I)*P4Z4(I)
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
C        
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
cc minoration deltax car inertie constante avec marge 
cc d'un facteur sqrt(8) sur le cot    AA0 
cc approche a partir du volume initial
c            AA0 = ((SIX*SQR2*VOLO(I))**TWO_THIRD) * EIGHT
c
c inertie major   e d'un facteur 9.6 (ratio des Lc^2) 
c sur +grand cot    initial
c et non plus le cot    moyen (SIX*SQR2*VOLO(I))**TWO_THIRD
c => il faudrait recalculer ou stocker AA0 avec la m   me 
c    formule que AA
              AA  = MAX(RX(I)*RX(I)+RY(I)*RY(I)+RZ(I)*RZ(I),
     .                  SX(I)*SX(I)+SY(I)*SY(I)+SZ(I)*SZ(I),
     .                  TX(I)*TX(I)+TY(I)*TY(I)+TZ(I)*TZ(I),
     .                  (RX(I)-SX(I))*(RX(I)-SX(I))
     +            +     (RY(I)-SY(I))*(RY(I)-SY(I))
     +            +     (RZ(I)-SZ(I))*(RZ(I)-SZ(I)),
     .                  (SX(I)-TX(I))*(SX(I)-TX(I))
     +            +     (SY(I)-TY(I))*(SY(I)-TY(I))
     +            +     (SZ(I)-TZ(I))*(SZ(I)-TZ(I)),
     .                  (TX(I)-RX(I))*(TX(I)-RX(I))
     +            +     (TY(I)-RY(I))*(TY(I)-RY(I))
     +            +     (TZ(I)-RZ(I))*(TZ(I)-RZ(I)))
c              DELTAX(I) = DELTAX(I)*MIN(ONE,AA0/AA)
              DELTAX2(I) = AA
            ENDDO
          END IF !(IDT1SOL/=0 )THEN
        END IF

      ENDIF
C-----------
 1000 FORMAT(/' ZERO OR NEGATIVE VOLUME : 10 NODES TETRAHEDRON NB ',I10,
     .        ' INTEGRATION POINT NB ',I1/)
 1100 FORMAT(/' ZERO OR NEGATIVE VOLUME : 4 NODES TETRAHEDRON NB ',I10,
     .        ' INTEGRATION POINT NB ',I1/)
 2000 FORMAT(/' ZERO OR NEGATIVE VOLUME : DELETE 3D-ELEMENT NB',I10/)
 3000 FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB:',I10/,
     +        ' ELEMENT IS SWITCHED TO SMALL STRAIN OPTION'/) 
 4000 FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB:',I10/) 
C-----------
      RETURN
      END
