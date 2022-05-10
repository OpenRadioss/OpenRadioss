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
Chd|  EBCS1                         source/boundary_conditions/ebcs/ebcs1.F
Chd|-- called by -----------
Chd|        EBCS_MAIN                     source/boundary_conditions/ebcs/ebcs_main.F
Chd|-- calls ---------------
Chd|        EBCS_MOD                      ../common_source/modules/ebcs_mod.F
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|====================================================================
      SUBROUTINE EBCS1(NSEG,ISEG,SEGVAR,
     .                 A,V,X,
     .                 LISTE,NOD,IRECT,
     .                 VO,PO,LA,
     .                 FV,MS,STIFN,TYP,EBCS_TAB, IEBCS)
      USE EBCS_MOD
      USE SEGVAR_MOD
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
      INTEGER NSEG,NOD,ISEG(NSEG),LISTE(NOD),TYP,IRECT(4,NSEG)
      my_real
     .        A(3,*),X(3,*),V(3,*),LA(3,NOD),
     .        VO(NOD),PO(NOD),FV(*),MS(*),STIFN(*)
      TYPE(t_ebcs_tab), TARGET, INTENT(IN) :: EBCS_TAB
      INTEGER, INTENT(IN) :: IEBCS
      TYPE(t_segvar) :: SEGVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IPRES,IRHO,I,IS,KSEG,N1,N2,N3,N4,NG1,NG2,NG3,NG4,
     .        N,IENER
      my_real
     .       ORIENT,PRES,RHO,C,LCAR,R1,R2,ROC,ALP,FAC,
     .       X13,Y13,Z13,X24,Y24,Z24,NX,NY,NZ,S,
     .       ROOU,ENOU,VMX,VMY,VMZ,FLUXI,FLUXO,VN,PN,DU,DP,P,
     .       ENER,DPDV
      CLASS(t_ebcs), POINTER :: EBCS
C-----------------------------------------------
C Presssion et densite imposes
      EBCS => EBCS_TAB%tab(IEBCS)%poly
      SELECT TYPE (EBCS)
      TYPE IS (t_ebcs_pres)
        IPRES=EBCS%ipres
        IRHO=EBCS%irho
        IENER=EBCS%iener
        IF(IPRES.GT.0)THEN
           PRES=EBCS%pres*FV(IPRES)
        ELSE
           PRES=EBCS%pres
        ENDIF
        IF(IRHO.GT.0)THEN
           RHO=EBCS%rho*FV(IRHO)
        ELSE
           RHO=EBCS%rho
        ENDIF
        IF(IENER.GT.0)THEN
           ENER=EBCS%ener*FV(IENER)
        ELSE
           ENER=EBCS%ener
        ENDIF
        C=EBCS%c
        LCAR=EBCS%lcar
        R1=EBCS%r1
        R2=EBCS%r2
      TYPE IS (t_ebcs_valvin)
        IPRES=EBCS%ipres
        IRHO=EBCS%irho
        IENER=EBCS%iener
        IF(IPRES.GT.0)THEN
           PRES=EBCS%pres*FV(IPRES)
        ELSE
           PRES=EBCS%pres
        ENDIF
        IF(IRHO.GT.0)THEN
           RHO=EBCS%rho*FV(IRHO)
        ELSE
           RHO=EBCS%rho
        ENDIF
        IF(IENER.GT.0)THEN
           ENER=EBCS%ener*FV(IENER)
        ELSE
           ENER=EBCS%ener
        ENDIF
        C=EBCS%c
        LCAR=EBCS%lcar
        R1=EBCS%r1
        R2=EBCS%r2
      TYPE IS (t_ebcs_valvout)
        IPRES=EBCS%ipres
        IRHO=EBCS%irho
        IENER=EBCS%iener
        IF(IPRES.GT.0)THEN
           PRES=EBCS%pres*FV(IPRES)
        ELSE
           PRES=EBCS%pres
        ENDIF
        IF(IRHO.GT.0)THEN
           RHO=EBCS%rho*FV(IRHO)
        ELSE
           RHO=EBCS%rho
        ENDIF
        IF(IENER.GT.0)THEN
           ENER=EBCS%ener*FV(IENER)
        ELSE
           ENER=EBCS%ener
        ENDIF
        C=EBCS%c
        LCAR=EBCS%lcar
        R1=EBCS%r1
        R2=EBCS%r2
      END SELECT

      ROC=RHO*C
c
c      write(6,*)R1,R2,LCAR
c
      ALP=ZERO
      IF (LCAR.GT.0)ALP=C*DT1/LCAR
C SURFACE NORMALE NODALES
      DO I=1,NOD
         LA(1,I)=ZERO
         LA(2,I)=ZERO
         LA(3,I)=ZERO
      ENDDO
      DO IS=1,NSEG
        KSEG=ABS(ISEG(IS))
        ORIENT=FLOAT(ISEG(IS)/KSEG)
        N1=IRECT(1,IS)
        N2=IRECT(2,IS)
        N3=IRECT(3,IS)
        N4=IRECT(4,IS)
        IF(N4.EQ.0 .OR. N4.EQ.N3) THEN
          FAC=UNSIX*ORIENT
          N4=N3
        ELSE
          FAC=UNHUIT*ORIENT
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
        IF(N4.NE.N3) THEN
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
        DMF=DMF-FLUXO*ROOU-FLUXI*RHO
        DEF=DEF-FLUXO*ENOU-FLUXI*ENER
C
C stockage densite et energie entrante dans buffer facette
C
        SEGVAR%RHO(KSEG)=RHO
        SEGVAR%EINT(KSEG)=ENER
      ENDDO
C
      DO I=1,NOD
        N=LISTE(I)
        S=SQRT(LA(1,I)**2+LA(2,I)**2+LA(3,I)**2)
        VN=(V(1,N)*LA(1,I)+V(2,N)*LA(2,I)+V(3,N)*LA(3,I))/S
C ajout resistance
        PN=PRES+R1*VN+R2*VN*ABS(VN)
        DPDV=ROC+R1+DEUX*R2*ABS(VN)
c condition darret
c        write(6,*)i,n,pn,pres,-0.5*RHO*VN**2
        IF(VN.LT.0)THEN
          PN=PN-UNDEMI*RHO*VN**2
          DPDV=DPDV-RHO*VN
        ENDIF
c frontiere silencieuse
        IF(TT.GT.0)THEN
          DU=ROC*(VN-VO(I))
        ELSE
          DU=ZERO
          PO(I)=PN
        ENDIF
        DP=ALP*(PN-PO(I))
c        write(6,*)'vitesse',vn,vo(i)
        VO(I)=VN
        P=PO(I)+DP+DU
        IF(C.EQ.ZERO)P=PN
c        write(6,*)'ebcs1 pression',p,'vn',vn,'dp',dp,'du',du
c
        A(1,N)=A(1,N)-P*LA(1,I)
        A(2,N)=A(2,N)-P*LA(2,I)
        A(3,N)=A(3,N)-P*LA(3,I)
C contribution des forces a la perte d'energie globale
        DEF=DEF-UNDEMI*(PO(I)+P)*DT1*VN*S
        PO(I)=P
        STIFN(N)=STIFN(N)+(DEUX*(S*DPDV)**2)/MS(N)
c        write(6,*)'stifn',STIFN(N)
      ENDDO
c
      RETURN
      END



















