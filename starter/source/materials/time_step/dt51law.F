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
!||    dt51law   ../starter/source/materials/time_step/dt51law.F
!||--- called by ------------------------------------------------------
!||    dtmain    ../starter/source/materials/time_step/dtmain.F
!||--- calls      -----------------------------------------------------
!||    dtel      ../starter/source/materials/time_step/dtel.F
!||    dtsph     ../starter/source/materials/time_step/dtsph.F
!||====================================================================
      SUBROUTINE DT51LAW (PM,GEO,PID,MAT, BUFMAT, IPM, DELTAX, AIRE, VOL, DTX)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     ELEMENTARY TIME STEPS FOR ALE MULTI MATERIAL LAW 51 (/MAT/MULTIMAT)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, INTENT(IN) :: PM(NPROPM, *), GEO(NPROPG, *), AIRE(*), VOL(*)
      my_real, INTENT(INOUT) :: DTX(*)
      my_real, INTENT(IN), DIMENSION(:), TARGET :: BUFMAT(*), DELTAX(*)
      INTEGER, INTENT(IN) :: PID(*),MAT(*),IPM(NPROPMI, *)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I, IADBUF,IFLG(MVSIZ), BIJ(4)
      my_real,DIMENSION(:),POINTER :: UPARAM
      my_real :: SSP(MVSIZ)  , RHO0(MVSIZ) , BULK(MVSIZ) ,
     .           AV1(MVSIZ)  , AV2(MVSIZ)  , AV3(MVSIZ)  , AV4(MVSIZ)  ,
     .           RHO10(MVSIZ), RHO20(MVSIZ), RHO30(MVSIZ), RHO40(MVSIZ),
     .           SSP1(MVSIZ) , SSP2(MVSIZ) , SSP3(MVSIZ) , SSP4(MVSIZ),
     .           VIS(MVSIZ)  , RHO0_BAK(MVSIZ)
      INTEGER :: IAV(4),IRHO(4),ISSP(4)

c-----------------------------------------------
       IAV(1:4)  = (/ 4,5,6,46 /)
       IRHO(1:4) = (/ 9,10,11,47 /)
       ISSP(1:4)  = (/ 124,174,224,273 /)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IFLG(1) = 0
      DO I=1,LLT
        IADBUF   = IPM(7,MAT(I))
        UPARAM   =>BUFMAT(IADBUF:IADBUF+280)
        BIJ(1:4) = NINT(UPARAM(277:280))
        AV1(I)   = UPARAM(IAV(BIJ(1)))
        AV2(I)   = UPARAM(IAV(BIJ(2)))
        AV3(I)   = UPARAM(IAV(BIJ(3)))
        AV4(I)   = UPARAM(IAV(BIJ(4)))
        RHO10(I) = UPARAM(IRHO(BIJ(1)))
        RHO20(I) = UPARAM(IRHO(BIJ(2)))
        RHO30(I) = UPARAM(IRHO(BIJ(3)))
        RHO40(I) = UPARAM(IRHO(BIJ(4)))
        IFLG(I)  = NINT(UPARAM(31))
        VIS(I)   = ZERO
        SSP1(I)  = UPARAM(ISSP(BIJ(1)))
        SSP2(I)  = UPARAM(ISSP(BIJ(2)))
        SSP3(I)  = UPARAM(ISSP(BIJ(3)))
        SSP4(I)  = UPARAM(ISSP(BIJ(4)))
      ENDDO!next I

      IF (IFLG(1) == 6) THEN
        DO I=LFT,LLT
          DTX(I)=EP20
        ENDDO
        RETURN 
      ENDIF

      DO I=LFT,LLT
        RHO0(I) = AV1(I)*RHO10(I) + AV2(I)*RHO20(I) + AV3(I)*RHO30(I) + AV4(I)*RHO40(I)
      ENDDO

      DO I=LFT,LLT
        BULK(I) =   AV1(I)*RHO10(I)*SSP1(I)**2 
     .            + AV2(I)*RHO20(I)*SSP2(I)**2 
     .            + AV3(I)*RHO30(I)*SSP3(I)**2 
     .            + AV4(I)*RHO40(I)*SSP4(I)**2
        IF (RHO0(I) > ZERO) THEN
           SSP(I)  = SQRT(BULK(I)/RHO0(I))
           RHO0_BAK(I)=RHO0(I)
        ELSE
           SSP(I) = EM20
           RHO0_BAK(I)=EM20
        ENDIF
      ENDDO!next I

      !----------------------------------------------!
      !  ELEMENTARY TIME STEP (ARTIFICIAL VISCOSITY) !
      !----------------------------------------------!
      IF(JSPH==0)THEN
       CALL DTEL(SSP,PM,GEO,PID,MAT, RHO0_BAK, VIS, DELTAX, AIRE, VOL, DTX)
      ELSE
       CALL DTSPH(SSP,PM,GEO,PID,MAT, RHO0_BAK, VIS, DELTAX, VOL, DTX)
      ENDIF
C-----------
      RETURN
      END
