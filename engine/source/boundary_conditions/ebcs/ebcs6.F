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
Chd|  EBCS6                         source/boundary_conditions/ebcs/ebcs6.F
Chd|-- called by -----------
Chd|        EBCS_MAIN                     source/boundary_conditions/ebcs/ebcs_main.F
Chd|-- calls ---------------
Chd|        EBCS_MOD                      ../common_source/modules/ebcs_mod.F
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|====================================================================
      SUBROUTINE EBCS6(NSEG,ISEG,SEGVAR,
     .                 A,V,X,
     .                 LISTE,NOD,IRECT,
     .                 RO0,EN0,P0,
     .                 VO,PO,LA,
     .                 MS,STIFN,IPARG,EBCS)
      USE EBCS_MOD
      USE SEGVAR_MOD
C-----------------------------------------------
C Pression arret initiale imposee avec impedance
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "conv_c.inc"
#include      "scr11_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSEG,NOD,ISEG(NSEG),LISTE(NOD),IRECT(4,NSEG),
     .        IPARG(NPARG,*)
      my_real
     .        A(3,*),X(3,*),V(3,*),LA(3,NOD),
     .        RO0(NSEG),EN0(NSEG),P0(NOD),VO(NOD),PO(NOD),
     .        MS(*),STIFN(*)
      TYPE(t_ebcs_inip), INTENT(IN) :: EBCS
      TYPE(t_segvar) :: SEGVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IS,K,L,KSEG,ESEG,N1,N2,N3,N4,NG1,NG2,NG3,NG4,N,
     .        KLT,KTY,MFT,EAD
C      
      my_real
     .       ORIENT,PRES,RHO,C,LCAR,ROC,ALP,FAC,
     .       X13,Y13,Z13,X24,Y24,Z24,NX,NY,NZ,S,
     .       ROOU,ENOU,VMX,VMY,VMZ,FLUXI,FLUXO,VN,PN,DU,DP,P,DPDV
C-----------------------------------------------
      C=EBCS%c
      RHO=EBCS%rho
      ROC=RHO*C
      LCAR=EBCS%lcar
      ALP=ZERO
      IF (LCAR > 0) ALP=C*DT1/LCAR
C Sauve densite et energie initiale
      IF(TT == 0)THEN
        DO IS=1,NSEG
          KSEG=ABS(ISEG(IS))
          RO0(IS) = SEGVAR%RHO(KSEG)
          EN0(IS) = SEGVAR%EINT(KSEG)
        ENDDO
      ENDIF
C SURFACE NORMALE NODALES
      DO I=1,NOD
         LA(1,I)=ZERO
         LA(2,I)=ZERO
         LA(3,I)=ZERO
      ENDDO
C
      DO IS=1,NSEG
        KSEG=ABS(ISEG(IS))
        ORIENT=FLOAT(ISEG(IS)/KSEG)
        N1=IRECT(1,IS)
        N2=IRECT(2,IS)
        N3=IRECT(3,IS)
        N4=IRECT(4,IS)
        IF(N4 == 0 .OR. N4 == N3) THEN
          FAC=ONE_OVER_6*ORIENT
	         N4=N3
        ELSE
          FAC=ONE_OVER_8*ORIENT
        ENDIF
c
        NG1=LISTE(N1)
        NG2=LISTE(N2)
        NG3=LISTE(N3)
        NG4=LISTE(N4)
        X13=X(1,NG3)-X(1,NG1)
        Y13=X(2,NG3)-X(2,NG1)
        Z13=X(3,NG3)-X(3,NG1)
        X24=X(1,NG4)-X(1,NG2)
        Y24=X(2,NG4)-X(2,NG2)
        Z24=X(3,NG4)-X(3,NG2)
c
        NX=(Y13*Z24-Z13*Y24)*FAC
        NY=(Z13*X24-X13*Z24)*FAC
        NZ=(X13*Y24-Y13*X24)*FAC
c
        LA(1,N1)=LA(1,N1)+NX
        LA(2,N1)=LA(2,N1)+NY
        LA(3,N1)=LA(3,N1)+NZ
        LA(1,N2)=LA(1,N2)+NX
        LA(2,N2)=LA(2,N2)+NY
        LA(3,N2)=LA(3,N2)+NZ
        LA(1,N3)=LA(1,N3)+NX
        LA(2,N3)=LA(2,N3)+NY
        LA(3,N3)=LA(3,N3)+NZ
C
        VMX=V(1,NG1)+V(1,NG2)+V(1,NG3)
        VMY=V(2,NG1)+V(2,NG2)+V(2,NG3)
        VMZ=V(3,NG1)+V(3,NG2)+V(3,NG3)
        IF(N4/=N3) THEN
           LA(1,N4)=LA(1,N4)+NX
           LA(2,N4)=LA(2,N4)+NY
           LA(3,N4)=LA(3,N4)+NZ
           VMX=VMX+V(1,NG4)
           VMY=VMY+V(2,NG4)
           VMZ=VMZ+V(3,NG4)
        ENDIF
C
c bilan masse et energie totale
c
        ROOU = SEGVAR%RHO(KSEG)
        ENOU = SEGVAR%EINT(KSEG)
C
        FLUXO=(VMX*NX+VMY*NY+VMZ*NZ)*DT1
        FLUXI=MIN(FLUXO,ZERO)
        FLUXO=MAX(FLUXO,ZERO)
C
        DMF=DMF-FLUXO*ROOU-FLUXI*RO0(IS)
        DEF=DEF-FLUXO*ENOU-FLUXI*EN0(IS)
C
C stockage densit  et  nergie entrante dans buffer facette
C
        SEGVAR%RHO(KSEG)=RO0(IS)
        SEGVAR%EINT(KSEG)=EN0(IS)

      ENDDO
C
C calcul pression initiale nodale
C
      IF (TT == 0) THEN
        DO I=1,NOD
          N=LISTE(I)
          S=LA(1,I)**2+LA(2,I)**2+LA(3,I)**2
          VN=(V(1,N)*LA(1,I)+V(2,N)*LA(2,I)+V(3,N)*LA(3,I))/SQRT(S)
c on memorise la pression d'arret
          P0(I)=P0(I)/S
          IF(VN<ZERO)P0(I)=P0(I)+HALF*RHO*VN**2
c          write(6,*)'P nodale', P0(I)
          VO(I)=VN
          PO(I)=P0(I)
        ENDDO
      ENDIF
C
      DO I=1,NOD
        N=LISTE(I)
        S=SQRT(LA(1,I)**2+LA(2,I)**2+LA(3,I)**2)
        VN=(V(1,N)*LA(1,I)+V(2,N)*LA(2,I)+V(3,N)*LA(3,I))/S
        DPDV=ROC
c condition darret
        PN=P0(I)
        IF(VN<0)THEN
          PN=P0(I)-HALF*RHO*VN**2
          DPDV=DPDV-RHO*VN
        ENDIF
c frontiere silencieuse
        DU=ROC*(VN-VO(I))
        DP=ALP*(PN-PO(I))
        P=PO(I)+DP+DU
c        write(6,*)'calcul', P,P0(I)
c
        A(1,N)=A(1,N)-P*LA(1,I)
        A(2,N)=A(2,N)-P*LA(2,I)
        A(3,N)=A(3,N)-P*LA(3,I)
        STIFN(N)=STIFN(N)+(TWO*(S*DPDV)**2)/MS(N)
C
        DEF=DEF-HALF*(PO(I)+P)*DT1*VN*S
C
        VO(I)=VN
        PO(I)=P
      ENDDO
c-----------
      RETURN
      END
