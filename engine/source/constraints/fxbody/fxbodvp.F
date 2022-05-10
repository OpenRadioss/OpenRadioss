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
Chd|  FXBODVP1                      source/constraints/fxbody/fxbodvp.F
Chd|-- called by -----------
Chd|        FXBYVIT                       source/constraints/fxbody/fxbyvit.F
Chd|-- calls ---------------
Chd|        FXBSYS                        source/constraints/fxbody/fxbsys.F
Chd|        FXLINK                        source/constraints/fxbody/fxbodv.F
Chd|        SPLINK                        source/constraints/fxbody/fxbodv.F
Chd|====================================================================
      SUBROUTINE FXBODVP1(FXBRPM, FXBGLM, FXBLM , MVN   , MCD  ,
     .                    SE    , SV    , FXBVIT, FXBACC, NME  ,
     .                    NMOD  , ISH   , DMT   , FSAV  , FXBFC,
     .                    FXBEDP, IBLO  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NME, NMOD, ISH, DMT, IBLO
      my_real
     .        FXBRPM(*), FXBGLM(*), FXBLM(*), MVN(*), MCD(NME,*),
     .        SE(*), SV(*), FXBVIT(*), FXBACC(*), FSAV(*),
     .        FXBFC(*), FXBEDP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, IAD
      my_real
     .        CR(6,NME), SR(6), MT(DMT,DMT), ST(DMT), ALPHA, FAC,
     .        DT05, VITN(NME+NMOD), GLM(NME,NME), ECIN, DWDAMP
C
      IF (IBLO.EQ.1) THEN
         DO I=1,NME
            FXBACC(I)=ZERO
         ENDDO
         GOTO 100
      ENDIF
C-----------------------------------------------
C   RESOLUTION SYSTEME LOCAL
C-----------------------------------------------
      CALL FXLINK(CR    , SR, DT1, DT2, FXBRPM,
     .            FXBVIT, NME)
C     
      DO I=1,NME
         DO II=1,NME
            MT(I,II)=MCD(I,II)
         ENDDO
         DO II=1,6
            MT(NME+II,I)=-CR(II,I)
            MT(I,NME+II)=-CR(II,I)
         ENDDO
         ST(I)=SE(I)
      ENDDO
      DO I=1,6
         DO II=1,6
            MT(NME+I,NME+II)=ZERO
         ENDDO
         ST(NME+I)=-SR(I)
      ENDDO
      IF (ISH.GT.0) CALL SPLINK(MT, ST, DT1, FXBRPM, FXBVIT,
     .                          DMT)
C
      CALL FXBSYS(MT,ST,DMT)      
C
      DO I=1,NME
         FXBACC(I)=ST(I)
      ENDDO
C
  100 CONTINUE
C
      ALPHA=FXBRPM(13)
      FAC=UN+UNDEMI*DT2*ALPHA
      IF (NMOD.GT.0) THEN
         DO I=1,NMOD
            FXBACC(NME+I)=SV(I)/FXBLM(I)/FAC
         ENDDO
         IF (IBLO.EQ.0) THEN
            DO I=1,NME
               IAD=NMOD*(I-1)
               DO II=1,NMOD
                  FXBACC(NME+II)=FXBACC(NME+II)-MVN(IAD+II)*FXBACC(I)
               ENDDO
            ENDDO
         ENDIF
      ENDIF
C         
      DT05=UNDEMI*DT1
      DO I=1,NME+NMOD
         VITN(I)=FXBVIT(I)+DT05*FXBACC(I)
         FXBVIT(I)=FXBVIT(I)+DT12*FXBACC(I)
      ENDDO
      IAD=0
      DO I=1,NME
         DO II=I,NME
            IAD=IAD+1
            GLM(I,II)=FXBGLM(IAD)
            IF (I.NE.II) GLM(II,I)=GLM(I,II)
         ENDDO
      ENDDO
      ECIN=ZERO
      DO I=1,NME
         DO II=1,NME
            ECIN=ECIN+UNDEMI*VITN(I)*GLM(I,II)*VITN(II)
         ENDDO
         IAD=NMOD*(I-1)
         DO II=1,NMOD
            ECIN=ECIN+UNDEMI*VITN(I)
     .               *MVN(IAD+II)*FXBLM(II)*VITN(NME+II)
         ENDDO
      ENDDO
      DWDAMP=ZERO
      DO I=1,NMOD
         DO II=1,NME
            ECIN=ECIN+UNDEMI*VITN(NME+I)
     .               *FXBLM(I)*MVN(NMOD*(II-1)+I)*VITN(II)
         ENDDO
         ECIN=ECIN+UNDEMI*VITN(NME+I)*FXBLM(I)*VITN(NME+I)
         DWDAMP=DWDAMP+VITN(NME+I)*
     .                 (FXBFC(I)+ALPHA*FXBLM(I)*VITN(NME+I))
      ENDDO
      FXBEDP=FXBEDP+DWDAMP*DT12
      FXBRPM(11)=FXBRPM(11)+FXBEDP
      FXBRPM(12)=ECIN
      FSAV(2)=ECIN
      FSAV(4)=FXBEDP
C
      RETURN
      END
Chd|====================================================================
Chd|  FXBODVP2                      source/constraints/fxbody/fxbodvp.F
Chd|-- called by -----------
Chd|        FXBYVIT                       source/constraints/fxbody/fxbyvit.F
Chd|-- calls ---------------
Chd|        FXSPIN                        source/constraints/fxbody/fxbodv.F
Chd|====================================================================
      SUBROUTINE FXBODVP2(FXBRPM, FXBNOD , FXBMOD  , FXBVIT , FXBACC,
     .                    NME   , NMOD   , V       , VR     , A     ,
     .                    AR    , MS     , IN      , NSN    , IDMAST,
     .                    ISH   , LMOD   , NSNT    , IFILE  , NSNI  ,
     .                    IRCM  , PMASTER, IAD_ELEM, FR_ELEM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FXBNOD(*), NME, NMOD, NSN, IDMAST, ISH, LMOD, NSNT,
     .        IFILE, NSNI, IRCM, PMASTER, IAD_ELEM(2,*), FR_ELEM(*)
      my_real
     .        FXBRPM(*), FXBMOD(*), FXBVIT(*), FXBACC(*), V(3,*),
     .        VR(3,*), A(3,*), AR(3,*), MS(*), IN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IAD, II, N, J, IFAC(NUMNOD), JJ
      my_real
     .        SPIN(3), R12(9), VT(3,NSN), VTR(3,NSN), VMOD(NSNT*6),
     .        USDT, ECBIDT, ECBIDR, VV(6), DT05, VX, VY, VZ, VRX, VRY,
     .        VRZ
C-----------------------------------------------
C   RESTITUTION DES VITESSES SUR LES SLAVES
C-----------------------------------------------
      CALL FXSPIN(FXBRPM, FXBVIT, SPIN, R12, DT2)
C
      DO I=1,NSN
         VT(1,I)=ZERO
         VT(2,I)=ZERO
         VT(3,I)=ZERO
         IF (ISH.GT.0) THEN
            VTR(1,I)=ZERO
            VTR(2,I)=ZERO
            VTR(3,I)=ZERO
         ELSE
            VTR(1,I)=SPIN(1)
            VTR(2,I)=SPIN(2)
            VTR(3,I)=SPIN(3)
         ENDIF
      ENDDO
      DO I=1,12
         IAD=(I-1)*LMOD
         DO II=1,LMOD
            VMOD(II)=FXBMOD(IAD+II)
         ENDDO
         IF (IFILE.EQ.1.AND.NSN.GT.NSNI) THEN
            IAD=NSNI*6
            DO II=1,NSN-NSNI
               IRCM=IRCM+1
               READ(IFXM,REC=IRCM) (VV(J),J=1,6)
               DO J=1,6
                  VMOD(IAD+J)=VV(J)
               ENDDO
               IAD=IAD+6
            ENDDO
         ENDIF
         IAD=0
         DO II=1,NSN
            VT(1,II)=VT(1,II)+FXBVIT(I)*VMOD(IAD+1)
            VT(2,II)=VT(2,II)+FXBVIT(I)*VMOD(IAD+2)
            VT(3,II)=VT(3,II)+FXBVIT(I)*VMOD(IAD+3)
            IAD=IAD+6
         ENDDO
      ENDDO
      IF (ISH.GT.0) THEN
         DO I=13,NME
            IAD=(I-1)*LMOD
            DO II=1,LMOD
               VMOD(II)=FXBMOD(IAD+II)
            ENDDO
            IF (IFILE.EQ.1.AND.NSN.GT.NSNI) THEN
               IAD=NSNI*6
               DO II=1,NSN-NSNI
                  IRCM=IRCM+1
                  READ(IFXM,REC=IRCM) (VV(J),J=1,6)
                  DO J=1,6
                     VMOD(IAD+J)=VV(J)
                  ENDDO
                  IAD=IAD+6
               ENDDO
            ENDIF
            IAD=0
            DO II=1,NSN
               VTR(1,II)=VTR(1,II)+FXBVIT(I)*VMOD(IAD+4)
               VTR(2,II)=VTR(2,II)+FXBVIT(I)*VMOD(IAD+5)
               VTR(3,II)=VTR(3,II)+FXBVIT(I)*VMOD(IAD+6)
               IAD=IAD+6
            ENDDO
         ENDDO
      ENDIF   
C     
      IF (NMOD.GT.0) THEN
         DO I=1,NMOD
            IAD=(NME+I-1)*LMOD
            DO II=1,LMOD
               VMOD(II)=FXBMOD(IAD+II)
            ENDDO
            IF (IFILE.EQ.1.AND.NSN.GT.NSNI) THEN
               IAD=NSNI*6
               DO II=1,NSN-NSNI
                  IRCM=IRCM+1
                  READ(IFXM,REC=IRCM) (VV(J),J=1,6)
                  DO J=1,6
                     VMOD(IAD+J)=VV(J)
                  ENDDO
                  IAD=IAD+6
               ENDDO
            ENDIF
            IAD=0
            DO II=1,NSN
               VT(1,II)=VT(1,II)+FXBVIT(NME+I)*
     .              (R12(1)*VMOD(IAD+1)+R12(2)*VMOD(IAD+2)+
     .               R12(3)*VMOD(IAD+3))
               VT(2,II)=VT(2,II)+FXBVIT(NME+I)*
     .              (R12(4)*VMOD(IAD+1)+R12(5)*VMOD(IAD+2)+
     .               R12(6)*VMOD(IAD+3))
               VT(3,II)=VT(3,II)+FXBVIT(NME+I)*
     .              (R12(7)*VMOD(IAD+1)+R12(8)*VMOD(IAD+2)+
     .               R12(9)*VMOD(IAD+3))
               VTR(1,II)=VTR(1,II)+FXBVIT(NME+I)*
     .              (R12(1)*VMOD(IAD+4)+R12(2)*VMOD(IAD+5)+
     .               R12(3)*VMOD(IAD+6))
               VTR(2,II)=VTR(2,II)+FXBVIT(NME+I)*
     .              (R12(4)*VMOD(IAD+4)+R12(5)*VMOD(IAD+5)+
     .               R12(6)*VMOD(IAD+6))
               VTR(3,II)=VTR(3,II)+FXBVIT(NME+I)*
     .              (R12(7)*VMOD(IAD+4)+R12(8)*VMOD(IAD+5)+
     .               R12(9)*VMOD(IAD+6))
               IAD=IAD+6
            ENDDO
         ENDDO
      ENDIF
      USDT=UN/DT12
      ECBIDT=ZERO
      ECBIDR=ZERO
      DT05 = UNDEMI*DT2
C
      DO I=1,NUMNOD
         IFAC(I)=1
      ENDDO
      IF (NSPMD.GT.1) THEN
         DO I=1,NSPMD
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
               JJ=FR_ELEM(J)
               IFAC(JJ)=IFAC(JJ)+1
            ENDDO
         ENDDO
      ENDIF
C
      DO I=1,NSN
         N=FXBNOD(I)
         A(1,N)=(VT(1,I)-V(1,N))*USDT
         A(2,N)=(VT(2,I)-V(2,N))*USDT
         A(3,N)=(VT(3,I)-V(3,N))*USDT
         AR(1,N)=(VTR(1,I)-VR(1,N))*USDT
         AR(2,N)=(VTR(2,I)-VR(2,N))*USDT
         AR(3,N)=(VTR(3,I)-VR(3,N))*USDT
         VX=V(1,N)+DT05*A(1,N)
         VY=V(2,N)+DT05*A(2,N)
         VZ=V(3,N)+DT05*A(3,N)
         VRX=VR(1,N)+DT05*AR(1,N)
         VRY=VR(2,N)+DT05*AR(2,N)
         VRZ=VR(3,N)+DT05*AR(3,N)
         ECBIDT=ECBIDT+UNDEMI*MS(N)*(VX*VX+VY*VY+VZ*VZ)/IFAC(N)
         ECBIDR=ECBIDR+UNDEMI*IN(N)*(VRX*VRX+VRY*VRY+VRZ*VRZ)/IFAC(N)
      ENDDO
      DO I=NSN+1,NSNT
         N=FXBNOD(I)
         A(1,N)=ZERO
         A(2,N)=ZERO
         A(3,N)=ZERO
         AR(1,N)=ZERO
         AR(2,N)=ZERO
         AR(3,N)=ZERO
         VX=V(1,N)
         VY=V(2,N)
         VZ=V(3,N)
         VRX=VR(1,N)
         VRY=VR(2,N)
         VRZ=VR(3,N)
         ECBIDT=ECBIDT+UNDEMI*MS(N)*(VX*VX+VY*VY+VZ*VZ)/IFAC(N)
         ECBIDR=ECBIDR+UNDEMI*IN(N)*(VRX*VRX+VRY*VRY+VRZ*VRZ)/IFAC(N)
      ENDDO
      IF (PMASTER.NE.ISPMD) FXBRPM(12)=ZERO  
      FXBRPM(12)=FXBRPM(12)-ECBIDT-ECBIDR
C-----------------------------------------------
C   RESTITUTION SUR LE MASTER
C-----------------------------------------------
      IF (IDMAST.NE.0) THEN
         A(1,IDMAST)=FXBACC(10)
         A(2,IDMAST)=FXBACC(11)
         A(3,IDMAST)=FXBACC(12)
         AR(1,IDMAST)=(SPIN(1)-VR(1,IDMAST))*USDT
         AR(2,IDMAST)=(SPIN(2)-VR(2,IDMAST))*USDT
         AR(3,IDMAST)=(SPIN(3)-VR(3,IDMAST))*USDT
      ENDIF
C
      RETURN
      END
