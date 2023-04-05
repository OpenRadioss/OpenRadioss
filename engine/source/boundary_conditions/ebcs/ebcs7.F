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
Chd|  EBCS7                         source/boundary_conditions/ebcs/ebcs7.F
Chd|-- called by -----------
Chd|        EBCS_MAIN                     source/boundary_conditions/ebcs/ebcs_main.F
Chd|-- calls ---------------
Chd|        EBCS_MOD                      ../common_source/modules/boundary_conditions/ebcs_mod.F
Chd|        SEGVAR_MOD                    share/modules/segvar_mod.F    
Chd|====================================================================
      SUBROUTINE EBCS7(NSEG,ISEG,SEGVAR,
     .                 A,V,X,
     .                 LISTE,NOD,IRECT,
     .                 RO0,EN0,V0,
     .                 LA,MS,STIFN,EBCS)
      USE EBCS_MOD
      USE SEGVAR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "scr11_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSEG,NOD,ISEG(NSEG),LISTE(NOD),IRECT(4,NSEG)
      my_real
     .        A(3,*),V(3,*),X(3,*),RO0(NSEG),EN0(NSEG),
     .        V0(3,NOD),LA(3,NOD),MS(*),STIFN(*)
      TYPE(t_ebcs_iniv), INTENT(IN) :: EBCS
      TYPE(t_segvar) :: SEGVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IS,K,L,KSEG,N1,N2,N3,N4,NG1,NG2,NG3,NG4,N
      my_real
     .       ORIENT,RHO,C,FC,ROC,ALP,FAC,
     .       X13,Y13,Z13,X24,Y24,Z24,NX,NY,NZ,S,
     .       ROOU,ENOU,VMX,VMY,VMZ,FLUXI,FLUXO,P,DVX,DVY,DVZ
C-----------------------------------------------
      C=EBCS%c
      RHO=EBCS%rho
      ROC=RHO*C
C
C     INITIALISATION DES DENSITES ET ENERGIES INITIALES
C
      IF(TT==0)THEN
        DO IS=1,NSEG
          KSEG=ABS(ISEG(IS))
          RO0(IS) = SEGVAR%RHO(KSEG)
          EN0(IS) = SEGVAR%EINT(KSEG)
        ENDDO            
        DO I=1,NOD
          N=LISTE(I)
          V0(1,I)=V(1,N)
          V0(2,I)=V(2,N)
          V0(3,I)=V(3,N)
c        write(6,*)V0(3,I)
        ENDDO
      ENDIF
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
        IF(N4==0 .OR. N4==N3) THEN
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
cow46e16+++
        VMX=V(1,NG1)+V(1,NG2)+V(1,NG3)
        VMY=V(2,NG1)+V(2,NG2)+V(2,NG3)
        VMZ=V(3,NG1)+V(3,NG2)+V(3,NG3)
cow46e16---
cow46e16        VMX=V(1,NG1)+V(1,NG2)+V(1,N3)
cow46e16        VMY=V(2,NG1)+V(2,NG2)+V(2,N3)
cow46e16        VMZ=V(3,NG1)+V(3,NG2)+V(3,N3)
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
c
        FLUXO=(VMX*NX+VMY*NY+VMZ*NZ)*DT1
        FLUXI=MIN(FLUXO,ZERO)
        FLUXO=MAX(FLUXO,ZERO)
c
        DMF=DMF-FLUXO*ROOU-FLUXI*RO0(IS)
        DEF=DEF-FLUXO*ENOU-FLUXI*EN0(IS)
C
C stockage densit  et  nergie entrante dans buffer facette
C
        SEGVAR%RHO(KSEG)=RO0(IS)
        SEGVAR%EINT(KSEG)=EN0(IS)
      ENDDO
C
      DO I=1,NOD
        N=LISTE(I)
        S=SQRT(LA(1,I)**2+LA(2,I)**2+LA(3,I)**2)
        DVX=V(1,N)-V0(1,I)
        DVY=V(2,N)-V0(2,I)
        DVZ=V(3,N)-V0(3,I)
c        write(6,*)I,N,v(3,N),v0(3,I),roc
        P=ROC*(DVX*LA(1,I)+DVY*LA(2,I)+DVZ*LA(3,I))/S
c
        A(1,N)=A(1,N)-P*LA(1,I)
        A(2,N)=A(2,N)-P*LA(2,I)
        A(3,N)=A(3,N)-P*LA(3,I)
        STIFN(N)=STIFN(N)+(TWO*(S*ROC)**2)/MS(N)
      ENDDO
c
      RETURN
      END



















