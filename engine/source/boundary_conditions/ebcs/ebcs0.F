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
!||    ebcs0          ../engine/source/boundary_conditions/ebcs/ebcs0.F
!||--- called by ------------------------------------------------------
!||    ebcs_main      ../engine/source/boundary_conditions/ebcs/ebcs_main.F
!||--- uses       -----------------------------------------------------
!||    ebcs_mod       ../common_source/modules/boundary_conditions/ebcs_mod.F90
!||    elbufdef_mod   ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    segvar_mod     ../engine/share/modules/segvar_mod.F
!||====================================================================
      SUBROUTINE EBCS0(NSEG,ISEG,SEGVAR,
     .                 A,V,X,
     .                 LISTE,NOD,IRECT,IELEM,
     .                 VO,PO,P0,
     .                 LA,FV,MS,STIFN,IPARG,ELBUF_TAB,EBCS)
C-----------------------------------------------
C   Gradient de pression
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
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
#include      "scr11_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSEG,NOD,ISEG(NSEG),LISTE(NOD),IRECT(4,NSEG),
     .        IPARG(NPARG,NGROUP),IELEM(NSEG)
      my_real
     .        A(3,NUMNOD),X(3,NUMNOD),V(3,NUMNOD),LA(3,NOD),
     .        P0(NOD),VO(NOD),PO(NOD),MS(*),STIFN(*),FV(*)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE(t_ebcs_gradp0), INTENT(IN) :: EBCS
      TYPE(t_segvar) :: SEGVAR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IS,KSEG,ESEG,N1,N2,N3,N4,NG1,NG2,NG3,NG4,N,
     .        KLT,KTY,MFT,EAD,IRHO,IENER,II(6)
C      
      my_real
     .       ORIENT,RHO,C,LCAR,ROC,ALP,FAC,
     .       X13,Y13,Z13,X24,Y24,Z24,NX,NY,NZ,S,
     .       ROOU,ENOU,VMX,VMY,VMZ,FLUXI,FLUXO,VN,PN,DU,DP,P,
     .       ENER,R1,R2,S2,DPDV
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C-----------------------------------------------
      IRHO = EBCS%irho
      IENER = EBCS%iener
      KLT = 0
      MFT = 0
      IF(IRHO>0)THEN
        RHO=EBCS%rho*FV(IRHO)
      ELSE
        RHO=EBCS%rho
      ENDIF
      IF(IENER>0)THEN
        ENER=EBCS%ener*FV(IENER)
      ELSE
        ENER=EBCS%ener
      ENDIF
      C=EBCS%c
      LCAR=EBCS%lcar
      R1=EBCS%r1
      R2=EBCS%r1
      ROC=RHO*C
      ALP=ZERO
      IF(LCAR>0)ALP=C*DT1/LCAR

C
C SURFACE NORMALE NODALES
C
      DO I=1,NOD
         LA(1,I)=ZERO
         LA(2,I)=ZERO
         LA(3,I)=ZERO
      ENDDO
C
      DO IS=1,NSEG
        KSEG=ABS(ISEG(IS))
        ORIENT=FLOAT(ISEG(IS)/KSEG)
        ESEG=IELEM(IS)
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
        DMF=DMF-FLUXO*ROOU-FLUXI*RHO
        DEF=DEF-FLUXO*ENOU-FLUXI*ENER
C
C stockage densit  et  nergie entrante dans buffer facette
C
        SEGVAR%RHO(KSEG)=RHO
        SEGVAR%EINT(KSEG)=ENER
C Pression voisin
        DO N=1,NGROUP
         KTY = IPARG(5,N)
         KLT = IPARG(2,N)
         MFT = IPARG(3,N)
         IF (KTY==1 .AND. ESEG<=KLT+MFT) EXIT
        ENDDO  
        EAD = ESEG-MFT
        GBUF => ELBUF_TAB(N)%GBUF
        DO I=1,6
          II(I) = KLT*(I-1)
        ENDDO
      
        P  =-(GBUF%SIG(II(1)+EAD)+GBUF%SIG(II(2)+EAD)+GBUF%SIG(II(3)+EAD))*THIRD
C
        P0(N1)=P0(N1)+P*(NX*LA(1,N1)+NY*LA(2,N1)+NZ*LA(3,N1))
        P0(N2)=P0(N2)+P*(NX*LA(1,N2)+NY*LA(2,N2)+NZ*LA(3,N2))
        P0(N3)=P0(N3)+P*(NX*LA(1,N3)+NY*LA(2,N3)+NZ*LA(3,N3))
        IF(N4/=N3) THEN
          P0(N4)=P0(N4)+P*(NX*LA(1,N4)+NY*LA(2,N4)+NZ*LA(3,N4))
        ENDIF
c        write(6,*)'init P',IS,EAD,P
      ENDDO

C
      DO I=1,NOD
        N=LISTE(I)
        S2=LA(1,I)**2+LA(2,I)**2+LA(3,I)**2
        S=SQRT(S2)
        VN=(V(1,N)*LA(1,I)+V(2,N)*LA(2,I)+V(3,N)*LA(3,I))/S
c condition darret
        PN=P0(I)/S2+R1*VN+R2*VN*ABS(VN)
        DPDV=ROC+R1+TWO*R2*ABS(VN)
c frontiere silencieuse
        IF(TT>0)THEN
          DU=ROC*(VN-VO(I))
        ELSE
          DU=ZERO
          PO(I)=PN
        ENDIF
        DP=ALP*(PN-PO(I))
        P=PO(I)+ALP*DP+DU
        IF(C==ZERO)P=PN
c        write(6, *)'ebcs4',P0(I),PN,P,DP,DU,R1*VN,R2*VN
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
