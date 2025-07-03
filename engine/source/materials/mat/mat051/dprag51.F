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
      !||    dprag51    ../engine/source/materials/mat/mat051/dprag51.F
      !||--- called by ------------------------------------------------------
      !||    sigeps51   ../engine/source/materials/mat/mat051/sigeps51.F90
      !||====================================================================
      SUBROUTINE DPRAG51
     .          (NEL    ,SIGD   ,VOL      ,EPSEQ  , VFRAC ,
     .           DEPS   ,UPARAM ,VOLUME   ,EINT   , PLAS  ,
     .           PFRAC , POLD   ,
     .           OFF    ,PEXT   ,DE )


!common points with M10LAW()
        !SIGD   = deviatoric stress
        !T      = SIGD copy
        !G*DEPSXX = G * DT1*D1

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NEL
      my_real, INTENT(IN) :: PFRAC,OFF(NEL),DEPS(6,NEL),VOL(NEL),UPARAM(*),VOLUME(NEL),PEXT,DE(NEL)
      my_real,INTENT(INOUT) :: SIGD(6,NEL),POLD(NEL),EINT(NEL),EPSEQ(NEL),PLAS(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,KK
      my_real :: GG,A0,A1,A2,AMAX,TMELT,THETL,AMUMX,SIGMX
      my_real :: T0,FACT
      my_real :: PTOT
      my_real, intent(in) :: vfrac(nel)
      my_real :: YOUNG, NU
      my_real :: T1(NEL), T2(NEL), T3(NEL), T4(NEL), T5(NEL), T6(NEL)
      my_real :: RATIO(NEL)
      my_real :: BULK(NEL), BULK2(NEL), G(NEL), G43(NEL), G0(NEL), G2(NEL)
      my_real :: J2(NEL),YIELD2(NEL)
      my_real :: PSTAR,SIGDO(6,NEL)
      my_real :: einc, vm, vol_avg
      my_real :: dpla
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      A0           = UPARAM(16)
      A1           = UPARAM(17)
      A2           = UPARAM(18)
      AMAX         = UPARAM(19)
      YOUNG        = UPARAM(02)
      NU           = UPARAM(22)
      PSTAR        = UPARAM(23)
      TMELT        = UPARAM(08)
      THETL        = UPARAM(09)
      AMUMX        = UPARAM(20)
      SIGMX        = UPARAM(11)
      T0           = UPARAM(13)
      GG           = UPARAM(01)
      BULK(1:NEL)  = UPARAM(21)
      BULK2(1:NEL) = UPARAM(21)

      DO I=1,NEL
        G(I)    = GG !* DT1
        G2(I)   = TWO * G(I)
        G43(I)  = ONEP333*GG

        T1(I)=SIGD(1,I)
        T2(I)=SIGD(2,I)
        T3(I)=SIGD(3,I)
        T4(I)=SIGD(4,I)
        T5(I)=SIGD(5,I)
        T6(I)=SIGD(6,I)

        SIGDO(1:6,I) = SIGD(1:6,I)
        FACT = VFRAC(I)
        T1(I) = T1(I) + G2(I)* (DEPS(1,I)-DE(I))*FACT
        T2(I) = T2(I) + G2(I)* (DEPS(2,I)-DE(I))*FACT
        T3(I) = T3(I) + G2(I)* (DEPS(3,I)-DE(I))*FACT
        T4(I) = T4(I) + G(I) * DEPS(4,I)*FACT
        T5(I) = T5(I) + G(I) * DEPS(5,I)*FACT
        T6(I) = T6(I) + G(I) * DEPS(6,I)*FACT

        J2(I)=HALF*(T1(I)**2+T2(I)**2+T3(I)**2)+T4(I)**2+T5(I)**2+T6(I)**2
        vm=sqrt(three*j2(i))
        PTOT = POLD(I)+PEXT
        G0(I) =A0+A1*PTOT+A2*PTOT*PTOT
        G0(I)= MIN(AMAX,G0(I))
        G0(I)= MAX(ZERO,G0(I))
        IF(PTOT <= PSTAR)G0(I)=ZERO
        IF(POLD(I) < PFRAC)G0(I)=ZERO
        YIELD2(I)=J2(I)-G0(I)


        if(vfrac(i) > two*em02) then
        
          RATIO(I)=ZERO
          IF(G0(I) > ZERO)THEN
            IF(YIELD2(I) > ZERO)THEN
              RATIO(I)= sqrt(three*G0(I))/(vm+ EM14)
              dpla = (one -ratio(i))*vm  /max(three*g(i),em15)
              plas(i)  = plas(i) + dpla
              epseq(i) = epseq(i)+ dpla
            ELSE
              RATIO(I)=ONE
            ENDIF
          ENDIF
                
          ! deviatoric stress
          sigd(1,i)=ratio(i)*t1(i)*off(i)
          sigd(2,i)=ratio(i)*t2(i)*off(i)
          sigd(3,i)=ratio(i)*t3(i)*off(i)
          sigd(4,i)=ratio(i)*t4(i)*off(i)
          sigd(5,i)=ratio(i)*t5(i)*off(i)
          sigd(6,i)=ratio(i)*t6(i)*off(i)
          !plastic work
          vol_avg = half*(vfrac(i)*volume(i)+vol(i))
          einc    = half*vol_avg*
     .                       ( (sigdo(1,i)+sigd(1,i)) * deps(1,i)
     .                       + (sigdo(2,i)+sigd(2,i)) * deps(2,i)
     .                       + (sigdo(3,i)+sigd(3,i)) * deps(3,i)
     .                       + (sigdo(4,i)+sigd(4,i)) * deps(4,i)
     .                       + (sigdo(5,i)+sigd(5,i)) * deps(5,i)
     .                       + (sigdo(6,i)+sigd(6,i)) * deps(6,i))
          eint(i) = eint(i) + einc
        elseif(vfrac(i) < em02)then
          plas(i) = zero
        else
          !smooth transition vfrac \in [0.01 0.02]
          plas(i) = (vfrac(i)-em02)*hundred * plas(i)
        end if

      ENDDO !next I


      RETURN
      END
C
