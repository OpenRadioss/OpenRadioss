Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    i9avel        ../engine/source/interfaces/int09/i9avel.F
!||--- called by ------------------------------------------------------
!||    i9main2       ../engine/source/interfaces/int09/i9main2.F
!||    i9main3       ../engine/source/interfaces/int09/i9main3.F
!||--- calls      -----------------------------------------------------
!||    ancmsg        ../engine/source/output/message/message.F
!||    arret         ../engine/source/system/arret.F
!||    bcs2          ../engine/source/constraints/general/bcs/bcs2.F
!||    shapeh        ../engine/source/ale/inter/shapeh.F
!||--- uses       -----------------------------------------------------
!||    message_mod   ../engine/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE I9AVEL(X    , SKEW, A     , FN  , FT   ,
     2                  IRECT, LMSR, CRST  , MSR , NSV  ,
     3                  ILOC , IRTL, LCODE , V   , ISKEW,
     4                  NOR  , MS  , FRIGAP, MSMN, MSMT ,
     5                  EFRIC, ITAB, FSAV  , NMN , NSN  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NMN,NSN
      INTEGER IRECT(4,*), LMSR(4,*), MSR(*), NSV(NSN), ILOC(*), IRTL(*),LCODE(*), ISKEW(*), ITAB(*)
      my_real
     .   X(3,*), SKEW(LSKEW,*), A(*), FN(*), FT(*), MSMN(*), MSMT(*),
     .   CRST(2,*), V(*), NOR(3,*), MS(*), EFRIC(*), FSAV(NTHVKI),
     .   FRIGAP(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "scr08_a_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JBC(3), NIR, I, J, I3, J3, I2, J2, I1, J1, ISK, LCOD, II,
     .   L, JJ, NN, LK, IBC
      my_real
     .   XN(3), YN(3), ZN(3), H(4), N1, N2, N3, AX, AY, AZ,
     .   VX, VY, VZ, AMN, VMN, AMOD, VMOD, BVZ, BAZ, BVX, BVY, BAX, BAY,
     .   A11, A12, A13, A21, A22, A23, A31, A32, A33, AXT, AYT, AZT,
     .   VXT, VYT, VZT, AXN, AYN, AZN, VT, AT, FTT, FAC, VXN, VYN,
     .   VZN, FNN, DET, FRIC, FHEAT
C-----------------------------------------------
      FRIC = FRIGAP(1)
      FHEAT= FRIGAP(4)
      NIR=2
      IF(N2D == 0)NIR=4
      !-------------------------------------
      !     ACCELERATIONS OF LAGRANGIAN NODES
      !-------------------------------------
      DO I=1,NMN
        J=MSR(I)
        I3=3*I
        J3=3*J
        I2=I3-1
        J2=J3-1
        I1=I2-1
        J1=J2-1
        FSAV(1)=FSAV(1)+FN(I1)*DT12
        FSAV(2)=FSAV(2)+FN(I2)*DT12
        FSAV(3)=FSAV(3)+FN(I3)*DT12
        FSAV(4)=FSAV(4)+FT(I1)*DT12
        FSAV(5)=FSAV(5)+FT(I2)*DT12
        FSAV(6)=FSAV(6)+FT(I3)*DT12
        IF(MSMN(I1) > ZERO)THEN
         A(J1)=A(J1) + FN(I1)/MSMN(I1)
         A(J2)=A(J2) + FN(I2)/MSMN(I1)
         A(J3)=A(J3) + FN(I3)/MSMN(I1)
        ENDIF
        IF(MSMT(I1) > ZERO)THEN
         A(J1)=A(J1) + FT(I1)/MSMT(I1)
         A(J2)=A(J2) + FT(I2)/MSMT(I1)
         A(J3)=A(J3) + FT(I3)/MSMT(I1)
        ENDIF
        ISK=ISKEW(J)
        LCOD=LCODE(J)
        CALL BCS2(A(J1),SKEW(1,ISK),ISK,LCOD)
      ENDDO
      !--------------------------------------
      !     ACCELERATIONS OF ALE NODES
      !--------------------------------------
      DO II=1,NSN
        IF(ILOC(II) >= 1) THEN
          I=NSV(II)
          L=IRTL(II)
          DO JJ=1,NIR
            NN=IRECT(JJ,L)
            IX(JJ)=MSR(NN)
          ENDDO
          N1=NOR(1,II)
          N2=NOR(2,II)
          N3=NOR(3,II)
          IF(N2D == 0)THEN
            CALL SHAPEH(H,CRST(1,II),CRST(2,II))
          ELSE
            H(1) = HALF*(ONE - CRST(1,II))
            H(2) = HALF*(ONE + CRST(1,II))
          ENDIF
          I3=3*I
          I2=I3-1
          I1=I2-1
          AX=ZERO
          AY=ZERO
          AZ=ZERO
          VX=ZERO
          VY=ZERO
          VZ=ZERO
          DO JJ=1,NIR
            J3=3*IX(JJ)
            J2=J3-1
            J1=J2-1
            AX=AX+A(J1)*H(JJ)
            AY=AY+A(J2)*H(JJ)
            AZ=AZ+A(J3)*H(JJ)
            VX=VX+V(J1)*H(JJ)
            VY=VY+V(J2)*H(JJ)
            VZ=VZ+V(J3)*H(JJ)
          ENDDO

          AMN=N1*AX+N2*AY+N3*AZ
          VMN=N1*VX+N2*VY+N3*VZ
          AMOD=AMN-N1*A(I1)-N2*A(I2)-N3*A(I3)
          VMOD=VMN-N1*V(I1)-N2*V(I2)-N3*V(I3)

          AXN = AMOD*N1
          AYN = AMOD*N2
          AZN = AMOD*N3
          VXN = VMOD*N1
          VYN = VMOD*N2
          VZN = VMOD*N3
          FNN = (VMOD/DT12 + AMOD) * MS(I)

          AXT = AX - A(I1) - AXN
          AYT = AY - A(I2) - AYN
          AZT = AZ - A(I3) - AZN
          VXT = VX - V(I1) - VXN
          VYT = VY - V(I2) - VYN
          VZT = VZ - V(I3) - VZN
          VT = SQRT(VXT**2+VYT**2+VZT**2)
          AT = SQRT(AXT**2+AYT**2+AZT**2)
          FTT = (VT/DT12 + AT) * MS(I)

          FAC = MIN(ONE,FRIC*FNN/MAX(EM30,FTT))
          FTT  = FTT*FAC
          AXT = AXT*FAC
          AYT = AYT*FAC
          AZT = AZT*FAC
          VXT = VXT*FAC
          VYT = VYT*FAC
          VZT = VZT*FAC
          !--------------------------------
          !     NO BOUNDARY CONDITION
          !--------------------------------
          A(I1)=A(I1)+AXN+AXT
          A(I2)=A(I2)+AYN+AYT
          A(I3)=A(I3)+AZN+AZT
          V(I1)=V(I1)+VXN+VXT
          V(I2)=V(I2)+VYN+VYT
          V(I3)=V(I3)+VZN+VZT

          LCOD=LCODE(I)
          IF(LCOD > 0)THEN
            !--------------------------------
            !     NODE WITH DEFINED BC(S)
            !--------------------------------
            XN(1)=N1
            YN(1)=N2
            ZN(1)=N3

            JBC(1:3) = 0
            IF(LCODE(I) > 0) THEN  ! LCODE(I) is a 3-bit-integer representing XYZ tags
              JBC(3) = IAND(LCODE(I), 1) !1st bit
              JBC(2) = IAND(LCODE(I), 2) !2nd bit
              JBC(1) = IAND(LCODE(I), 4) !3rd bit
            ENDIF

            LK=ISKEW(I)
            IBC=2

            IF(JBC(1) /= 0)THEN
             XN(IBC)=SKEW(1,LK)
             YN(IBC)=SKEW(2,LK)
             ZN(IBC)=SKEW(3,LK)
             IBC=IBC+1
            ENDIF
            IF(JBC(2) /= 0)THEN
             XN(IBC)=SKEW(4,LK)
             YN(IBC)=SKEW(5,LK)
             ZN(IBC)=SKEW(6,LK)
             IBC=IBC+1
            ENDIF
            IF(JBC(3) /= 0)THEN
             IF(IBC==4)THEN
               !BCS check also in Starter, but BCS may be updated in Engine file (/BCS, /BCSR)
               CALL ANCMSG(MSGID=11,ANMODE=ANINFO,I1=ITAB(I))
               CALL ARRET(2)
             ENDIF
             XN(IBC)=SKEW(7,LK)
             YN(IBC)=SKEW(8,LK)
             ZN(IBC)=SKEW(9,LK)
             IBC=IBC+1
            ENDIF
            IF(IBC==3)THEN
             !---------------------------
             !     NODE WITH A SINGLE BC
             !---------------------------
             XN(3)=YN(1)*ZN(2)-ZN(1)*YN(2)
             YN(3)=ZN(1)*XN(2)-XN(1)*ZN(2)
             ZN(3)=XN(1)*YN(2)-YN(1)*XN(2)
             BVZ=V(I1)*XN(3)+V(I2)*YN(3)+V(I3)*ZN(3)
             BAZ=A(I1)*XN(3)+A(I2)*YN(3)+A(I3)*ZN(3)
            ELSE
             !-----------------------------
             !     NODE WITH 2 BCS
             !-----------------------------
             BVZ=ZERO
             BAZ=ZERO
            ENDIF

            BVX=VMN
            BVY=ZERO
            BAX=AMN
            BAY=ZERO

            A11=YN(2)*ZN(3)-ZN(2)*YN(3)
            A12=ZN(2)*XN(3)-XN(2)*ZN(3)
            A13=XN(2)*YN(3)-YN(2)*XN(3)
            A21=YN(3)*ZN(1)-ZN(3)*YN(1)
            A22=ZN(3)*XN(1)-XN(3)*ZN(1)
            A23=XN(3)*YN(1)-YN(3)*XN(1)
            A31=YN(1)*ZN(2)-ZN(1)*YN(2)
            A32=ZN(1)*XN(2)-XN(1)*ZN(2)
            A33=XN(1)*YN(2)-YN(1)*XN(2)
            DET=XN(1)*A11+YN(1)*A12+ZN(1)*A13
            !-----------------------------------
            !     VELOCITY AND ACCELERATION
            !-----------------------------------
            IF(DET /= ZERO) THEN
              V(I1)=(A11*BVX+A21*BVY+A31*BVZ)/DET
              V(I2)=(A12*BVX+A22*BVY+A32*BVZ)/DET
              V(I3)=(A13*BVX+A23*BVY+A33*BVZ)/DET

              A(I1)=(A11*BAX+A21*BAY+A31*BAZ)/DET
              A(I2)=(A12*BAX+A22*BAY+A32*BAZ)/DET
              A(I3)=(A13*BAX+A23*BAY+A33*BAZ)/DET
            ENDIF

          ENDIF !IF(LCOD > 0)THEN

          !-----------------------------------
          !     FRICTION ENERGY
          !-----------------------------------
          VT = SQRT((V(I1)-VX)**2 + (V(I2)-VY)**2 + (V(I3)-VZ)**2 )
          EFRIC(II) = FHEAT * FTT * VT * DT1

        ENDIF !IF(ILOC(II) >= 1)

      ENDDO !next II
C-----------------------------------------------
      RETURN
      END
