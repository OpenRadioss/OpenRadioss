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
Chd|  INIEBCSP                      source/boundary_conditions/ebcs/iniebcsp.F
Chd|-- called by -----------
Chd|        INIEBCSP0                     source/boundary_conditions/ebcs/iniebcsp0.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INIEBCSP(NSEG,NOD,ISEG,IELEM,IRECT,LISTE,
     .                    LA,IPARG,ELBUF_STR,P0,RHO,X)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSEG,NOD,ISEG(NSEG),IRECT(4,NSEG),LISTE(NOD),IPARG(NPARG,*), IELEM(NSEG)
      my_real
     .     LA(3,NOD),P0(NOD),RHO,X(3,*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER                                         
     .  N1,N2,N3,N4,I,N,IS,                                       
     .  KSEG,NG1,NG2,NG3,NG4,ESEG,EAD,KTY,KLT,MFT,II(6)
      my_real                                         
     .  ORIENT, FAC, X13,Y13,Z13,X24,Y24,Z24, NX,NY,NZ,S,VN,P,DPDV                             
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
C=======================================================================
      GBUF => ELBUF_STR%GBUF
C   Initialisation de la surface nodale
          DO I=1,NOD
            LA(1,I)=ZERO
            LA(2,I)=ZERO
            LA(3,I)=ZERO
          ENDDO
C
          DO IS=1,NSEG
            
            KSEG = ABS(ISEG(IS))
            ORIENT = ZERO
            IF(KSEG /= 0)ORIENT=FLOAT(ISEG(IS)/KSEG)

            N1=IRECT(1,IS)
            N2=IRECT(2,IS)
            N3=IRECT(3,IS)
            N4=IRECT(4,IS)

            IF(N4.EQ.0 .OR. N4.EQ.N3) THEN
              FAC=ONE_OVER_6*ORIENT
              N4=N3
            ELSE
              FAC=ONE_OVER_8*ORIENT
            ENDIF
C   
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
            IF(N4.NE.N3) THEN
              LA(1,N4)=LA(1,N4)+NX
              LA(2,N4)=LA(2,N4)+NY
              LA(3,N4)=LA(3,N4)+NZ
            ENDIF
          ENDDO
C
C Calcul pression initiale nodale
C
          DO I=1,NOD
            P0(I)=ZERO
          ENDDO

        DO IS=1,NSEG
          KSEG=ABS(ISEG(IS))
          ORIENT=ZERO
          IF(KSEG /= 0)ORIENT=FLOAT(ISEG(IS)/KSEG)
c         pression voisin
          ESEG=IELEM(IS)
          DO N=1,NGROUP
            KTY = IPARG(5,N)
            KLT = IPARG(2,N)
            MFT = IPARG(3,N)
            IF (KTY.EQ.1 .AND. ESEG.LE.KLT+MFT) GOTO 60
          ENDDO
 60       CONTINUE
!
          DO I=1,6
            II(I) = KLT*(I-1)
          ENDDO
!
          EAD = ESEG-MFT
          P = -(GBUF%SIG(II(1)+EAD)+GBUF%SIG(II(2)+EAD)+GBUF%SIG(II(3)+EAD))*THIRD

c          write(6,*)'NG',N,KTY,KLT,MFT,IPARG(4,N)
c          write (6,*)'voisin',IS,KSEG,ESEG,P
c          write(6,*)'sig',(GBUF%SIG(EAD+I),I=1,3)
c
          N1=IRECT(1,IS)
          N2=IRECT(2,IS)
          N3=IRECT(3,IS)
          N4=IRECT(4,IS)
          IF(N4.EQ.0 .OR. N4.EQ.N3) THEN
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
          P0(N1)=P0(N1)+P*(NX*LA(1,N1)+NY*LA(2,N1)+NZ*LA(3,N1))
          P0(N2)=P0(N2)+P*(NX*LA(1,N2)+NY*LA(2,N2)+NZ*LA(3,N2))
          P0(N3)=P0(N3)+P*(NX*LA(1,N3)+NY*LA(2,N3)+NZ*LA(3,N3))
          IF(N4.NE.N3) THEN
            P0(N4)=P0(N4)+P*(NX*LA(1,N4)+NY*LA(2,N4)+NZ*LA(3,N4))
          ENDIF
        ENDDO


      END
