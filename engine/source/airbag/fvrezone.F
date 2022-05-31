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
Chd|  FVREZONE0                     source/airbag/fvrezone.F      
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        FVREZONE1                     source/airbag/fvrezone.F      
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE FVREZONE0(MONVOL, X)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MONVOL(*)
      my_real
     .        X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K1, K2, KIBJET, KIBHOL, KIBALE, IADPOLH, IADPOLHO, N,
     .        ITYP, NNS, NTG, NBRIC, KI1, KI2, NNFV1, NTRFV1, NPOLY1,
     .        NPOLH1, NNFV0, NTRFV0, NPOLY0, NPOLH0, IFV, IMESH, NSTEP,
     .        ILVOUT, NNA
C
      K1=1
      K2=1+NIMV*NVOLU
      KIBJET=K2+LICBAG
      KIBHOL=KIBJET+LIBAGJET
      KIBALE=KIBHOL+LIBAGHOL
      IFV=0
      DO N=1,NVOLU
         ITYP=MONVOL(K1-1+2)
         IF (ITYP.EQ.6.OR.ITYP.EQ.8) THEN
           IFV=MONVOL(K1-1+45)
           IMESH=MONVOL(K1-1+56)
           IF (IMESH.NE.0) THEN
C
            NNS=MONVOL(K1-1+32)
            NTG=MONVOL(K1-1+33)
            NBRIC=MONVOL(K1-1+35)
            KI1=KIBALE+MONVOL(K1-1+31)
            KI2=KI1+NNS
C
            NNFV1=MONVOL(K1-1+46)
            NTRFV1=MONVOL(K1-1+47)
            NPOLY1=MONVOL(K1-1+48)
            NPOLH1=MONVOL(K1-1+49)
C
            NNFV0=MONVOL(K1-1+50)
            NTRFV0=MONVOL(K1-1+51)
            NPOLY0=MONVOL(K1-1+52)
            NPOLH0=MONVOL(K1-1+53)
            NSTEP=MONVOL(K1-1+58)
            ILVOUT=MONVOL(K1-1+44)
            NNA=MONVOL(K1-1+64)
C
            CALL FVREZONE1(
     1MONVOL(KI1), MONVOL(KI2),  X,            NTG,  FVDATA(IFV)%IFVNOD,
     2FVDATA(IFV)%RFVNOD,  FVDATA(IFV)%IFVTRI,  FVDATA(IFV)%IFVPOLY,
     3FVDATA(IFV)%IFVTADR, FVDATA(IFV)%IFVPOLH, FVDATA(IFV)%IFVPADR,
     4FVDATA(IFV)%MPOLH,   FVDATA(IFV)%QPOLH,   FVDATA(IFV)%EPOLH,
     5FVDATA(IFV)%PPOLH,   FVDATA(IFV)%RPOLH,   FVDATA(IFV)%GPOLH,
     6FVDATA_OLD(IFV)%IFVNOD,  FVDATA_OLD(IFV)%RFVNOD,
     7FVDATA_OLD(IFV)%IFVTRI,  FVDATA_OLD(IFV)%IFVPOLY,
     8FVDATA_OLD(IFV)%IFVTADR, FVDATA_OLD(IFV)%IFVPOLH,
     9FVDATA_OLD(IFV)%IFVPADR, FVDATA_OLD(IFV)%MPOLH,
     AFVDATA_OLD(IFV)%QPOLH,   FVDATA_OLD(IFV)%EPOLH,
     BFVDATA_OLD(IFV)%PPOLH,   FVDATA_OLD(IFV)%RPOLH,
     CFVDATA_OLD(IFV)%GPOLH, NNFV1, NTRFV1,    NPOLH1, NNFV0,
     DNTRFV0,      NPOLH0,      NPOLY1,        NPOLY0, NSTEP,
     EMONVOL(K1),          FVDATA(IFV)%CPAPOLH, FVDATA(IFV)%CPBPOLH,
     FFVDATA(IFV)%CPCPOLH,     FVDATA(IFV)%RMWPOLH,
     GFVDATA_OLD(IFV)%CPAPOLH, FVDATA_OLD(IFV)%CPBPOLH,
     HFVDATA_OLD(IFV)%CPCPOLH, FVDATA_OLD(IFV)%RMWPOLH, ILVOUT, NNS,NNA,
     IIFV                    )
           ENDIF
         ENDIF
         K1=K1+NIMV
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  FVREZONE1                     source/airbag/fvrezone.F      
Chd|-- called by -----------
Chd|        FVREZONE0                     source/airbag/fvrezone.F      
Chd|-- calls ---------------
Chd|        PINPOLH                       source/airbag/fvrezone.F      
Chd|        PROGBAR_C                     ../common_source/sortie/progbar_c.c
Chd|        SPMD_FVB_GATH                 source/mpi/airbags/spmd_fvb_gath.F
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE FVREZONE1(
     1      IBUF    , ELEM    , X       , NEL   , IFVNOD1,
     2      RFVNOD1 , IFVTRI1 , IFVPOLY1, 
     3      IFVTADR1, IFVPOLH1, IFVPADR1, 
     4      MPOLH1  , QPOLH1  , EPOLH1  , 
     5      PPOLH1  , RPOLH1  , GPOLH1  , 
     6      IFVNOD0 , RFVNOD0 , 
     7      IFVTRI0 , IFVPOLY0, 
     8      IFVTADR0, IFVPOLH0,
     9      IFVPADR0, MPOLH0  , 
     A      QPOLH0  , EPOLH0  , 
     B      PPOLH0  , RPOLH0  ,
     C      GPOLH0  , NNS1    , NNTR1   , NPOLH1, NNS0   ,
     D      NNTR0   , NPOLH0  , NPOLY1  , NPOLY0, NSTEP  ,
     E      ID      , CPAPOLH1, CPBPOLH1,
     F      CPCPOLH1, RMWPOLH1,
     G      CPAPOLH0, CPBPOLH0,
     H      CPCPOLH0, RMWPOLH0, ILVOUT  , NNF   , NNA    ,
     I      IFV     )                  
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBUF(*), ELEM(3,*), NEL, IFVNOD1(3,*), IFVTRI1(6,*),
     .        IFVPOLY1(*), IFVTADR1(*), IFVPOLH1(*), IFVPADR1(*),
     .        IFVNOD0(3,*), IFVTRI0(6,*), IFVPOLY0(*), IFVTADR0(*), 
     .        IFVPOLH0(*), IFVPADR0(*), NNS1, NNTR1, NPOLH1, NNS0,
     .        NNTR0, NPOLH0, NPOLY1, NPOLY0, NSTEP, ID, ILVOUT, 
     .        NNF, NNA, IFV
      my_real
     .        X(3,*), RFVNOD1(2,*), MPOLH1(*), QPOLH1(3,*), EPOLH1(*),
     .        PPOLH1(*), RPOLH1(*), GPOLH1(*), RFVNOD0(2,*),
     .        MPOLH0(*), QPOLH0(3,*), EPOLH0(*), PPOLH0(*),
     .        RPOLH0(*), GPOLH0(*), CPAPOLH1(*), CPBPOLH1(*), 
     .        CPCPOLH1(*), RMWPOLH1(*), CPAPOLH0(*), CPBPOLH0(*),
     .        CPCPOLH0(*), RMWPOLH0(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IEL, N1, N2, N3, NN1, NN2, NN3, J, JJ, K, KK, NNT,
     .        L, NN, PNTR0(NNTR0), PNTR1(NNTR1), NNT1, NNT0, LL, NNTI,
     .        INNER, INN(3), NTI, ICUT, NB, NNB, NBI, M, II, I1, I2,
     .        NNSA
      my_real
     .        KSI, ETA, X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, 
     .        PX0(3,NNS0), PX1(3,NNS1), X12, Y12, Z12, X13, Y13, Z13,
     .        NRX, NRY, NRZ, AREA2, TAREA0(NNTR0), NORM0(3,NNTR0),
     .        TAREA1(NNTR1), NORM1(3,NNTR1), VOLU1(NPOLH1), AREA,
     .        NX, NY, NZ, XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, XX, YY,
     .        ZZ, BBOX0(6,NPOLH0), BBOX1(6,NPOLH1), CRIT, XMIN1, XMAX1,
     .        YMIN1, YMAX1, ZMIN1, ZMAX1, XMIN0, XMAX0, YMIN0,
     .        YMAX0, ZMIN0, ZMAX0, VOLU0(NPOLH0), DXB, DYB, DZB, VOLB,
     .        VOL, VOLG, VOLT, RR, MASS0, QX0, QY0, QZ0, ENER0, MASS1,
     .        QX1, QY1, QZ1, ENER1, GAMA, FAC, XXX(3,NNF), XXXA(3,NNA)
      INTEGER, ALLOCATABLE :: PTRI1(:,:), PTRI0(:,:), TCUT(:), ITAGT(:),
     .                        BB(:,:), INB(:), INB_TMP(:), LISTB(:)
      my_real
     .       , ALLOCATABLE :: XB(:,:), XXXSA(:,:)
      CHARACTER NAME*18
C
      my_real
     .        DLAMCH
      EXTERNAL DLAMCH
C
C 0 = Anciens polyhedres
C 1 = Nouveaux polyhedres
C
      NNSA=FVSPMD(IFV)%NNSA
      ALLOCATE(XXXSA(3,NNSA))
      IF (NSPMD == 1) THEN
C traitement necessaire pour rester p/on
         DO I=1,FVSPMD(IFV)%NN_L
            I1=FVSPMD(IFV)%IBUF_L(1,I)
            I2=FVSPMD(IFV)%IBUF_L(2,I)
            XXX(1,I1)=X(1,I2)
            XXX(2,I1)=X(2,I2)
            XXX(3,I1)=X(3,I2)
         ENDDO
         DO I=1,FVSPMD(IFV)%NNA_L
               I1=FVSPMD(IFV)%IBUFA_L(1,I)
               I2=FVSPMD(IFV)%IBUFA_L(2,I)
               XXXA(1,I1)=X(1,I2)
               XXXA(2,I1)=X(2,I2)
               XXXA(3,I1)=X(3,I2)
         ENDDO
         DO I=1,FVSPMD(IFV)%NNSA_L
            I1=FVSPMD(IFV)%IBUFSA_L(1,I)
            I2=FVSPMD(IFV)%IBUFSA_L(2,I)
            XXXSA(1,I1)=X(1,I2)
            XXXSA(2,I1)=X(2,I2)
            XXXSA(3,I1)=X(3,I2)
         ENDDO
c         DO I=1,NNF
c            II=IBUF(I)
c            XXX(1,I)=X(1,II)
c            XXX(2,I)=X(2,II)
c            XXX(3,I)=X(3,II)
c         ENDDO
      ELSE
         CALL SPMD_FVB_GATH(IFV, X, XXX, XXXA, XXXSA,
     .                      2  )
         IF (ISPMD.NE.FVSPMD(IFV)%PMAIN-1) RETURN
      ENDIF
C Critere pour test d'interiorite
      CRIT=TEN
C---------------------------------------------------
C Preliminaires
C---------------------------------------------------
C Coordonnees des noeuds 
      DO I=1,NNS0
         IF (IFVNOD0(1,I).EQ.1) THEN
            IEL=IFVNOD0(2,I)
            KSI=RFVNOD0(1,I)
            ETA=RFVNOD0(2,I)
C
            N1=ELEM(1,IEL)
            N2=ELEM(2,IEL)
            N3=ELEM(3,IEL)
            X1=XXX(1,N1)
            X2=XXX(1,N2)
            X3=XXX(1,N3)
            Y1=XXX(2,N1)
            Y2=XXX(2,N2)
            Y3=XXX(2,N3)
            Z1=XXX(3,N1)
            Z2=XXX(3,N2)
            Z3=XXX(3,N3)
            PX0(1,I)=(ONE-KSI-ETA)*X1+KSI*X2+ETA*X3
            PX0(2,I)=(ONE-KSI-ETA)*Y1+KSI*Y2+ETA*Y3
            PX0(3,I)=(ONE-KSI-ETA)*Z1+KSI*Z2+ETA*Z3
         ELSEIF (IFVNOD0(1,I).EQ.2) THEN
            II=IFVNOD0(2,I)
c            IF (NSPMD == 1) THEN
c               PX0(1,I)=X(1,II)
c               PX0(2,I)=X(2,II)
c               PX0(3,I)=X(3,II)
c            ELSE
               PX0(1,I)=XXXSA(1,II)
               PX0(2,I)=XXXSA(2,II)
               PX0(3,I)=XXXSA(3,II)
c            ENDIF
         ENDIF
      ENDDO
      DO I=1,NNS0
         IF (IFVNOD0(1,I).EQ.3) THEN
            I1=IFVNOD0(2,I)
            I2=IFVNOD0(3,I)
            FAC=RFVNOD0(1,I)
            PX0(1,I)=FAC*PX0(1,I1)+(ONE-FAC)*PX0(1,I2)
            PX0(2,I)=FAC*PX0(2,I1)+(ONE-FAC)*PX0(2,I2)
            PX0(3,I)=FAC*PX0(3,I1)+(ONE-FAC)*PX0(3,I2)
         ENDIF
      ENDDO
C
      DO I=1,NNS1
         IF (IFVNOD1(1,I).EQ.1) THEN
            IEL=IFVNOD1(2,I)
            KSI=RFVNOD1(1,I)
            ETA=RFVNOD1(2,I)
C
            N1=ELEM(1,IEL)
            N2=ELEM(2,IEL)
            N3=ELEM(3,IEL)
            X1=XXX(1,N1)
            X2=XXX(1,N2)
            X3=XXX(1,N3)
            Y1=XXX(2,N1)
            Y2=XXX(2,N2)
            Y3=XXX(2,N3)
            Z1=XXX(3,N1)
            Z2=XXX(3,N2)
            Z3=XXX(3,N3)
            PX1(1,I)=(ONE-KSI-ETA)*X1+KSI*X2+ETA*X3
            PX1(2,I)=(ONE-KSI-ETA)*Y1+KSI*Y2+ETA*Y3
            PX1(3,I)=(ONE-KSI-ETA)*Z1+KSI*Z2+ETA*Z3
         ELSEIF (IFVNOD1(1,I).EQ.2) THEN
            II=IFVNOD1(2,I)
c            IF (NSPMD == 1) THEN
c               PX1(1,I)=X(1,II)
c               PX1(2,I)=X(2,II)
c               PX1(3,I)=X(3,II)
c            ELSE
               PX1(1,I)=XXXSA(1,II)
               PX1(2,I)=XXXSA(2,II)
               PX1(3,I)=XXXSA(3,II)
c            ENDIF
         ENDIF
      ENDDO
      DO I=1,NNS1
         IF (IFVNOD1(1,I).EQ.3) THEN
            I1=IFVNOD1(2,I)
            I2=IFVNOD1(3,I)
            FAC=RFVNOD1(1,I)
            PX1(1,I)=FAC*PX1(1,I1)+(ONE-FAC)*PX1(1,I2)
            PX1(2,I)=FAC*PX1(2,I1)+(ONE-FAC)*PX1(2,I2)
            PX1(3,I)=FAC*PX1(3,I1)+(ONE-FAC)*PX1(3,I2)
         ENDIF
      ENDDO
      DEALLOCATE(XXXSA)
C Normale et aire des triangles
      DO I=1,NNTR0
         N1=IFVTRI0(1,I)
         N2=IFVTRI0(2,I)
         N3=IFVTRI0(3,I)
         X1=PX0(1,N1)
         Y1=PX0(2,N1)
         Z1=PX0(3,N1)
         X2=PX0(1,N2)
         Y2=PX0(2,N2)
         Z2=PX0(3,N2)
         X3=PX0(1,N3)
         Y3=PX0(2,N3)
         Z3=PX0(3,N3)
         X12=X2-X1
         Y12=Y2-Y1
         Z12=Z2-Z1
         X13=X3-X1
         Y13=Y3-Y1
         Z13=Z3-Z1
         NRX=Y12*Z13-Z12*Y13
         NRY=Z12*X13-X12*Z13
         NRZ=X12*Y13-Y12*X13
         AREA2=SQRT(NRX**2+NRY**2+NRZ**2)
         TAREA0(I)=HALF*AREA2
         IF (AREA2.GT.0) THEN
            NORM0(1,I)=NRX/AREA2
            NORM0(2,I)=NRY/AREA2
            NORM0(3,I)=NRZ/AREA2
         ELSE
            NORM0(1,I)=ZERO
            NORM0(2,I)=ZERO
            NORM0(3,I)=ZERO
         ENDIF
      ENDDO
C      
      DO I=1,NNTR1
         N1=IFVTRI1(1,I)
         N2=IFVTRI1(2,I)
         N3=IFVTRI1(3,I)
         X1=PX1(1,N1)
         Y1=PX1(2,N1)
         Z1=PX1(3,N1)
         X2=PX1(1,N2)
         Y2=PX1(2,N2)
         Z2=PX1(3,N2)
         X3=PX1(1,N3)
         Y3=PX1(2,N3)
         Z3=PX1(3,N3)
         X12=X2-X1
         Y12=Y2-Y1
         Z12=Z2-Z1
         X13=X3-X1
         Y13=Y3-Y1
         Z13=Z3-Z1
         NRX=Y12*Z13-Z12*Y13
         NRY=Z12*X13-X12*Z13
         NRZ=X12*Y13-Y12*X13
         AREA2=SQRT(NRX**2+NRY**2+NRZ**2)
         TAREA1(I)=HALF*AREA2
         IF (AREA2.GT.1) THEN
            NORM1(1,I)=NRX/AREA2
            NORM1(2,I)=NRY/AREA2
            NORM1(3,I)=NRZ/AREA2
         ELSE
            NORM1(1,I)=ZERO
            NORM1(2,I)=ZERO
            NORM1(3,I)=ZERO
         ENDIF
      ENDDO
C Volume des polyhedres
      DO I=1,NPOLH0
         VOLU0(I)=ZERO
C Boucle sur les polygones du polyhedre
         DO J=IFVPADR0(I),IFVPADR0(I+1)-1
            JJ=IFVPOLH0(J)
C Boucle sur les triangles du polygone
            DO K=IFVTADR0(JJ), IFVTADR0(JJ+1)-1
               KK=IFVPOLY0(K)
               AREA=TAREA0(KK)
               IEL=IFVTRI0(4,KK)
               IF (IEL.GT.0) THEN
                  NX=NORM0(1,KK)
                  NY=NORM0(2,KK)
                  NZ=NORM0(3,KK)
               ELSE
                  IF (IFVTRI0(5,KK).EQ.I) THEN
                     NX=NORM0(1,KK)
                     NY=NORM0(2,KK)
                     NZ=NORM0(3,KK)
                  ELSEIF (IFVTRI0(6,KK).EQ.I) THEN
                     NX=-NORM0(1,KK)
                     NY=-NORM0(2,KK)
                     NZ=-NORM0(3,KK)
                  ENDIF   
               ENDIF      
               N1=IFVTRI0(1,KK)
               X1=PX0(1,N1)
               Y1=PX0(2,N1)
               Z1=PX0(3,N1)
               VOLU0(I)=VOLU0(I)+THIRD*AREA*(X1*NX+Y1*NY+Z1*NZ)
            ENDDO
         ENDDO
      ENDDO 
C
      DO I=1,NPOLH1
         VOLU1(I)=ZERO
C Boucle sur les polygones du polyhedre
         DO J=IFVPADR1(I),IFVPADR1(I+1)-1
            JJ=IFVPOLH1(J)
C Boucle sur les triangles du polygone
            DO K=IFVTADR1(JJ), IFVTADR1(JJ+1)-1
               KK=IFVPOLY1(K)
               AREA=TAREA1(KK)
               IEL=IFVTRI1(4,KK)
               IF (IEL.GT.0) THEN
                  NX=NORM1(1,KK)
                  NY=NORM1(2,KK)
                  NZ=NORM1(3,KK)
               ELSE
                  IF (IFVTRI1(5,KK).EQ.I) THEN
                     NX=NORM1(1,KK)
                     NY=NORM1(2,KK)
                     NZ=NORM1(3,KK)
                  ELSEIF (IFVTRI1(6,KK).EQ.I) THEN
                     NX=-NORM1(1,KK)
                     NY=-NORM1(2,KK)
                     NZ=-NORM1(3,KK)
                  ENDIF   
               ENDIF      
               N1=IFVTRI1(1,KK)
               X1=PX1(1,N1)
               Y1=PX1(2,N1)
               Z1=PX1(3,N1)
               VOLU1(I)=VOLU1(I)+THIRD*AREA*(X1*NX+Y1*NY+Z1*NZ)
            ENDDO
         ENDDO
      ENDDO   
C Bounding box pour les polyhedres
      DO I=1,NPOLH0
         XMIN=EP30
         XMAX=-EP30
         YMIN=EP30
         YMAX=-EP30
         ZMIN=EP30
         ZMAX=-EP30
         NNT=0
         DO J=IFVPADR0(I),IFVPADR0(I+1)-1
            JJ=IFVPOLH0(J)
            DO K=IFVTADR0(JJ),IFVTADR0(JJ+1)-1
               NNT=NNT+1
               KK=IFVPOLY0(K)
               DO L=1,3
                  NN=IFVTRI0(L,KK)
                  XX=PX0(1,NN)
                  YY=PX0(2,NN)
                  ZZ=PX0(3,NN)
                  XMIN=MIN(XMIN,XX)
                  XMAX=MAX(XMAX,XX)
                  YMIN=MIN(YMIN,YY)
                  YMAX=MAX(YMAX,YY)
                  ZMIN=MIN(ZMIN,ZZ)
                  ZMAX=MAX(ZMAX,ZZ)
               ENDDO
            ENDDO
         ENDDO
         BBOX0(1,I)=XMIN
         BBOX0(2,I)=XMAX
         BBOX0(3,I)=YMIN
         BBOX0(4,I)=YMAX
         BBOX0(5,I)=ZMIN
         BBOX0(6,I)=ZMAX
         PNTR0(I)=NNT
      ENDDO
C
      DO I=1,NPOLH1
         XMIN=EP30
         XMAX=-EP30
         YMIN=EP30
         YMAX=-EP30
         ZMIN=EP30
         ZMAX=-EP30
         NNT=0
         DO J=IFVPADR1(I),IFVPADR1(I+1)-1
            JJ=IFVPOLH1(J)
            DO K=IFVTADR1(JJ),IFVTADR1(JJ+1)-1
               NNT=NNT+1
               KK=IFVPOLY1(K)
               DO L=1,3
                  NN=IFVTRI1(L,KK)
                  XX=PX1(1,NN)
                  YY=PX1(2,NN)
                  ZZ=PX1(3,NN)
                  XMIN=MIN(XMIN,XX)
                  XMAX=MAX(XMAX,XX)
                  YMIN=MIN(YMIN,YY)
                  YMAX=MAX(YMAX,YY)
                  ZMIN=MIN(ZMIN,ZZ)
                  ZMAX=MAX(ZMAX,ZZ)
               ENDDO
            ENDDO
         ENDDO
         BBOX1(1,I)=XMIN
         BBOX1(2,I)=XMAX
         BBOX1(3,I)=YMIN
         BBOX1(4,I)=YMAX
         BBOX1(5,I)=ZMIN
         BBOX1(6,I)=ZMAX
         PNTR1(I)=NNT
      ENDDO
C---------------------------------------------------
C Rezone
C---------------------------------------------------
      NB=NSTEP**3
      ALLOCATE(BB(8,NB))
      NN=0
      DO I=1,NSTEP
         DO J=1,NSTEP
            DO K=1,NSTEP
               NN=NN+1
               BB(1,NN)=(I-1)*(NSTEP+1)**2+(J-1)*(NSTEP+1)+K
               BB(2,NN)=(I-1)*(NSTEP+1)**2+(J-1)*(NSTEP+1)+K+1
               BB(3,NN)=(I-1)*(NSTEP+1)**2+J*(NSTEP+1)+K+1
               BB(4,NN)=(I-1)*(NSTEP+1)**2+J*(NSTEP+1)+K
               BB(5,NN)=I*(NSTEP+1)**2+(J-1)*(NSTEP+1)+K
               BB(6,NN)=I*(NSTEP+1)**2+(J-1)*(NSTEP+1)+K+1
               BB(7,NN)=I*(NSTEP+1)**2+J*(NSTEP+1)+K+1
               BB(8,NN)=I*(NSTEP+1)**2+J*(NSTEP+1)+K
            ENDDO
         ENDDO
      ENDDO
C
      IF (ILVOUT.NE.0) WRITE(ISTDO,'(A25,I8,A14)') 
     .' ** MONITORED VOLUME ID: ',ID,' - REZONING **'
      NNB=(NSTEP+1)**3
      DO I=1,NPOLH1
         MPOLH1(I)=ZERO
         QPOLH1(1,I)=ZERO
         QPOLH1(2,I)=ZERO
         QPOLH1(3,I)=ZERO
         EPOLH1(I)=ZERO
         GPOLH1(I)=ZERO
         CPAPOLH1(I)=ZERO
         CPBPOLH1(I)=ZERO
         CPCPOLH1(I)=ZERO
         RMWPOLH1(I)=ZERO
C
         XMIN1=BBOX1(1,I)
         XMAX1=BBOX1(2,I)
         YMIN1=BBOX1(3,I)
         YMAX1=BBOX1(4,I)
         ZMIN1=BBOX1(5,I)
         ZMAX1=BBOX1(6,I)
C
         NNT1=PNTR1(I)
         ALLOCATE(PTRI1(3,NNT1))
         NNT=0
         DO J=IFVPADR1(I),IFVPADR1(I+1)-1
            JJ=IFVPOLH1(J)
            DO K=IFVTADR1(JJ),IFVTADR1(JJ+1)-1
               NNT=NNT+1
               KK=IFVPOLY1(K)
               IF (IFVTRI1(4,KK).GT.0) THEN
                  PTRI1(1,NNT)=IFVTRI1(1,KK)
                  PTRI1(2,NNT)=IFVTRI1(2,KK)
                  PTRI1(3,NNT)=IFVTRI1(3,KK)
               ELSEIF (IFVTRI1(5,KK).EQ.I) THEN
                  PTRI1(1,NNT)=IFVTRI1(1,KK)
                  PTRI1(2,NNT)=IFVTRI1(2,KK)
                  PTRI1(3,NNT)=IFVTRI1(3,KK)
               ELSEIF (IFVTRI1(6,KK).EQ.I) THEN
                  PTRI1(1,NNT)=IFVTRI1(1,KK)
                  PTRI1(2,NNT)=IFVTRI1(3,KK)
                  PTRI1(3,NNT)=IFVTRI1(2,KK)
               ENDIF   
            ENDDO
         ENDDO
C 
         ALLOCATE(XB(3,NNB), INB(NNB))
         DXB=(XMAX1-XMIN1)/NSTEP
         DYB=(YMAX1-YMIN1)/NSTEP
         DZB=(ZMAX1-ZMIN1)/NSTEP
         VOLB=DXB*DYB*DZB
         NN=0
         DO J=1,NSTEP+1
            ZZ=ZMIN1+(J-1)*DZB
            DO K=1,NSTEP+1
               YY=YMIN1+(K-1)*DYB
               DO L=1,NSTEP+1
                  XX=XMIN1+(L-1)*DXB
                  NN=NN+1
                  XB(1,NN)=XX
                  XB(2,NN)=YY
                  XB(3,NN)=ZZ
               ENDDO
            ENDDO
         ENDDO
         CALL PINPOLH(NNT1,  PTRI1, XB,    NNB,   PX1,
     .                INB,   CRIT , XMIN1, XMAX1, YMIN1,
     .                YMAX1, ZMIN1, ZMAX1)
C
         VOLT=ZERO
         JJ=0
         DO J=1,NPOLH0
            XMIN0=BBOX0(1,J)
            XMAX0=BBOX0(2,J)
            YMIN0=BBOX0(3,J)
            YMAX0=BBOX0(4,J)
            ZMIN0=BBOX0(5,J)
            ZMAX0=BBOX0(6,J)
C
            IF (XMAX1.LT.XMIN0.OR.YMAX1.LT.YMIN0.OR.ZMAX1.LT.ZMIN0.OR.
     .          XMIN1.GT.XMAX0.OR.YMIN1.GT.YMAX0.OR.ZMIN1.GT.ZMAX0)
     .         CYCLE
C
            NNT0=PNTR0(J)
            ALLOCATE(PTRI0(3,NNT0))
            NNT=0
            DO K=IFVPADR0(J),IFVPADR0(J+1)-1
               KK=IFVPOLH0(K)
               DO L=IFVTADR0(KK),IFVTADR0(KK+1)-1
                  NNT=NNT+1
                  LL=IFVPOLY0(L)
                  IF (IFVTRI0(4,LL).GT.0) THEN
                     PTRI0(1,NNT)=IFVTRI0(1,LL)
                     PTRI0(2,NNT)=IFVTRI0(2,LL)
                     PTRI0(3,NNT)=IFVTRI0(3,LL)
                  ELSEIF (IFVTRI0(5,LL).EQ.J) THEN
                     PTRI0(1,NNT)=IFVTRI0(1,LL)
                     PTRI0(2,NNT)=IFVTRI0(2,LL)
                     PTRI0(3,NNT)=IFVTRI0(3,LL)
                  ELSEIF (IFVTRI0(6,LL).EQ.J) THEN
                     PTRI0(1,NNT)=IFVTRI0(1,LL)
                     PTRI0(2,NNT)=IFVTRI0(3,LL)
                     PTRI0(3,NNT)=IFVTRI0(2,LL)
                  ENDIF   
               ENDDO
            ENDDO
C
            ALLOCATE(INB_TMP(NNB))
            CALL PINPOLH(NNT0,    PTRI0, XB,    NNB,   PX0,
     .                   INB_TMP, CRIT,  XMIN0, XMAX0, YMIN0,
     .                   YMAX0,   ZMIN0, ZMAX0)
            DO K=1,NNB
               INB_TMP(K)=INB_TMP(K)*INB(K)
            ENDDO
            ALLOCATE(LISTB(NB))
            NBI=0
            VOL=ZERO
            DO K=1,NB
               NN=0
               DO L=1,8
                  LL=BB(L,K)
                  NN=NN+INB_TMP(LL)
               ENDDO
               IF (NN.GT.0) THEN
                  NBI=NBI+1
                  LISTB(NBI)=K
               ENDIF
               VOL=VOL+NN*VOLB/EIGHT
            ENDDO
C
            RR=VOL/VOLU0(J)
            MPOLH1(I)=MPOLH1(I)+RR*MPOLH0(J)
            QPOLH1(1,I)=QPOLH1(1,I)+RR*QPOLH0(1,J)
            QPOLH1(2,I)=QPOLH1(2,I)+RR*QPOLH0(2,J)
            QPOLH1(3,I)=QPOLH1(3,I)+RR*QPOLH0(3,J)
            EPOLH1(I)=EPOLH1(I)+RR*EPOLH0(J)
            GPOLH1(I)=GPOLH1(I)+RR*GPOLH0(J)
            CPAPOLH1(I)=CPAPOLH1(I)+RR*CPAPOLH0(J)
            CPBPOLH1(I)=CPBPOLH1(I)+RR*CPBPOLH0(J)
            CPCPOLH1(I)=CPCPOLH1(I)+RR*CPCPOLH0(J)
            RMWPOLH1(I)=RMWPOLH1(I)+RR*RMWPOLH0(J)
C
            VOLT=VOLT+VOL
C
            DEALLOCATE(PTRI0, INB_TMP)   
         ENDDO
C
         DEALLOCATE(PTRI1, XB, INB)
C
         IF (ILVOUT.LT.0) CALL PROGBAR_C(I,NPOLH1)
      ENDDO
      DEALLOCATE(BB)
C
      MASS0=ZERO
      QX0=ZERO
      QY0=ZERO
      QZ0=ZERO
      ENER0=ZERO
      MASS1=ZERO
      QX1=ZERO
      QY1=ZERO
      QZ1=ZERO
      ENER1=ZERO
      DO I=1,NPOLH0
         MASS0=MASS0+MPOLH0(I)
         QX0=QX0+QPOLH0(1,I)
         QY0=QY0+QPOLH0(2,I)
         QZ0=QZ0+QPOLH0(3,I)
         ENER0=ENER0+EPOLH0(I)
      ENDDO
      DO I=1,NPOLH1
         MASS1=MASS1+MPOLH1(I)
         QX1=QX1+QPOLH1(1,I)
         QY1=QY1+QPOLH1(2,I)
         QZ1=QZ1+QPOLH1(3,I)
         ENER1=ENER1+EPOLH1(I)
      ENDDO
      
      WRITE(ISTDO,'(A18,G11.4,A15,G11.4,A6,F5.2,A1)') 
     .   '    INITIAL MASS: ',MASS0,' REZONED MASS: ',MASS1,
     .     ' ERR: ',MIN(ABS((MASS1-MASS0)/MASS0*HUNDRED),99.99D0),'%'
      WRITE(ISTDO,'(A18,G11.4,A15,G11.4,A6,F5.2,A1)') 
     .   '    INITIAL QX  : ',QX0,' REZONED QX  : ',QX1,
     .     ' ERR: ',MIN(ABS((QX1-QX0)/QX0*HUNDRED),99.99D0),'%'
      WRITE(ISTDO,'(A18,G11.4,A15,G11.4,A6,F5.2,A1)') 
     .   '    INITIAL QY  : ',QY0,' REZONED QY  : ',QY1,
     .     ' ERR: ',MIN(ABS((QY1-QY0)/QY0*HUNDRED),99.99D0),'%'
      WRITE(ISTDO,'(A18,G11.4,A15,G11.4,A6,F5.2,A1)') 
     .   '    INITIAL QZ  : ',QZ0,' REZONED QZ  : ',QZ1,
     .     ' ERR: ',MIN(ABS((QZ1-QZ0)/QZ0*HUNDRED),99.99D0),'%'
      WRITE(ISTDO,'(A18,G11.4,A15,G11.4,A6,F5.2,A1)') 
     .   '    INITIAL ENER: ',ENER0,' REZONED ENER: ',ENER1,
     .     ' ERR: ',MIN(ABS((ENER1-ENER0)/ENER0*HUNDRED),99.99D0),'%'
      WRITE(ISTDO,*)
      WRITE(IOUT,'(A18,G11.4,A15,G11.4,A6,F5.2,A1)') 
     .   '    INITIAL MASS: ',MASS0,' REZONED MASS: ',MASS1,
     .     ' ERR: ',MIN(ABS((MASS1-MASS0)/MASS0*HUNDRED),99.99D0),'%'
      WRITE(IOUT,'(A18,G11.4,A15,G11.4,A6,F5.2,A1)') 
     .   '    INITIAL QX  : ',QX0,' REZONED QX  : ',QX1,
     .     ' ERR: ',MIN(ABS((QX1-QX0)/QX0*HUNDRED),99.99D0),'%'
      WRITE(IOUT,'(A18,G11.4,A15,G11.4,A6,F5.2,A1)') 
     .   '    INITIAL QY  : ',QY0,' REZONED QY  : ',QY1,
     .     ' ERR: ',MIN(ABS((QY1-QY0)/QY0*HUNDRED),99.99D0),'%'
      WRITE(IOUT,'(A18,G11.4,A15,G11.4,A6,F5.2,A1)') 
     .   '    INITIAL QZ  : ',QZ0,' REZONED QZ  : ',QZ1,
     .     ' ERR: ',MIN(ABS((QZ1-QZ0)/QZ0*HUNDRED),99.99D0),'%'
      WRITE(IOUT,'(A18,G11.4,A15,G11.4,A6,F5.2,A1)') 
     .   '    INITIAL ENER: ',ENER0,' REZONED ENER: ',ENER1,
     .     ' ERR: ',MIN(ABS((ENER1-ENER0)/ENER0*HUNDRED),99.99D0),'%'
      WRITE(IOUT,*)
C
      DO I=1,NPOLH1
         GAMA=GPOLH1(I)
         RPOLH1(I)=MPOLH1(I)/VOLU1(I)
         PPOLH1(I)=(GAMA-ONE)*EPOLH1(I)/VOLU1(I)
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  PINPOLH                       source/airbag/fvrezone.F      
Chd|-- called by -----------
Chd|        FVREZONE1                     source/airbag/fvrezone.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PINPOLH(NEL , ELEM, XB  , NNB , X   ,
     .                   IN  , TOLE, XMIN, XMAX, YMIN,
     .                   YMAX, ZMIN, ZMAX)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL, ELEM(3,*), NNB, IN(*)
      my_real
     .        XB(3,*), X(3,*), TOLE, XMIN, XMAX, YMIN, YMAX,
     .        ZMIN, ZMAX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IEL, N1, N2, N3, II
      my_real
     .        XX, YY, ZZ, ST, X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3,
     .        VX1, VY1, VZ1, VX2, VY2, VZ2, VX3, VY3, VZ3, NX1, NY1,
     .        NZ1, NX2, NY2, NZ2, NX3, NY3, NZ3, RR1, RR2, RR3, SS1,
     .        SS2, SS3, SAREA, NX, NY, NZ, XC, YC, ZC, SS
C
      DO I=1,NNB
         XX=XB(1,I)
         YY=XB(2,I)
         ZZ=XB(3,I)
         IN(I)=0
C
         IF (XX.LT.XMIN.OR.XX.GT.XMAX.OR.
     .       YY.LT.YMIN.OR.YY.GT.YMAX.OR.
     .       ZZ.LT.ZMIN.OR.ZZ.GT.ZMAX) CYCLE
C
         ST=ZERO
         DO IEL=1,NEL
            N1=ELEM(1,IEL)
            N2=ELEM(2,IEL)
            N3=ELEM(3,IEL)
            X1=X(1,N1)
            Y1=X(2,N1)
            Z1=X(3,N1)
            X2=X(1,N2)
            Y2=X(2,N2)
            Z2=X(3,N2)
            X3=X(1,N3)
            Y3=X(2,N3)
            Z3=X(3,N3)
            VX1=X1-XX
            VY1=Y1-YY
            VZ1=Z1-ZZ
            VX2=X2-XX
            VY2=Y2-YY
            VZ2=Z2-ZZ
            VX3=X3-XX
            VY3=Y3-YY
            VZ3=Z3-ZZ
C
            NX1=VY1*VZ2-VZ1*VY2
            NY1=VZ1*VX2-VX1*VZ2
            NZ1=VX1*VY2-VY1*VX2
            NX2=VY2*VZ3-VZ2*VY3
            NY2=VZ2*VX3-VX2*VZ3
            NZ2=VX2*VY3-VY2*VX3
            NX3=VY3*VZ1-VZ3*VY1
            NY3=VZ3*VX1-VX3*VZ1
            NZ3=VX3*VY1-VY3*VX1
            RR1=SQRT(NX1**2+NY1**2+NZ1**2)
            RR2=SQRT(NX2**2+NY2**2+NZ2**2)
            RR3=SQRT(NX3**2+NY3**2+NZ3**2)
            SS1=-(NX1*NX2+NY1*NY2+NZ1*NZ2)/RR1/RR2
            SS2=-(NX2*NX3+NY2*NY3+NZ2*NZ3)/RR2/RR3
            SS3=-(NX3*NX1+NY3*NY1+NZ3*NZ1)/RR3/RR1
            IF (SS1.GT.ONE) SS1=ONE
            IF (SS1.LT.-ONE) SS1=-ONE
            IF (SS2.GT.ONE) SS2=ONE
            IF (SS2.LT.-ONE) SS2=-ONE
            IF (SS3.GT.ONE) SS3=ONE
            IF (SS3.LT.-ONE) SS3=-ONE
            SAREA=ACOS(SS1)+ACOS(SS2)+ACOS(SS3)-PI
C
            VX1=X2-X1
            VY1=Y2-Y1
            VZ1=Z2-Z1
            VX2=X3-X1
            VY2=Y3-Y1
            VZ2=Z3-Z1
            NX=VY1*VZ2-VZ1*VY2
            NY=VZ1*VX2-VX1*VZ2
            NZ=VX1*VY2-VY1*VX2
            XC=THIRD*(X1+X2+X3)
            YC=THIRD*(Y1+Y2+Y3)
            ZC=THIRD*(Z1+Z2+Z3)
            SS=NX*(XC-XX)+NY*(YC-YY)+NZ*(ZC-ZZ)
            IF (SS.LT.ZERO) SAREA=-SAREA
            ST=ST+SAREA
         ENDDO
C
         IF (ABS(ST).GT.TOLE) IN(I)=1        
      ENDDO
C
      RETURN
      END
