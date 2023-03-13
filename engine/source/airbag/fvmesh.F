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
Chd|  FVMESH1                       source/airbag/fvmesh.F        
Chd|-- called by -----------
Chd|        FVMESH0                       source/airbag/fvmesh0.F       
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        COORLOC                       source/airbag/fvmesh.F        
Chd|        C_TRICALL                     stub/fvmbags_stub.F           
Chd|        FACEPOLY                      source/airbag/fvmesh.F        
Chd|        GPOLCUT                       source/airbag/fvmesh.F        
Chd|        HORIEDGE                      source/airbag/fvmesh.F        
Chd|        HORIPOLY                      source/airbag/fvmesh.F        
Chd|        ITRIBOX                       source/airbag/fvmesh.F        
Chd|        POLYHEDR                      source/airbag/fvmesh.F        
Chd|        PROGBAR_C                     ../common_source/sortie/progbar_c.c
Chd|        SPMD_FVB_GATH                 source/mpi/airbags/spmd_fvb_gath.F
Chd|        TRIBOX3                       stub/fvmbags_stub.F           
Chd|        TRITRI3                       stub/fvmbags_stub.F           
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE FVMESH1(IBUF , ELEM  , X     , IVOLU, BRIC ,
     .                   XB   , RVOLU , NEL   , NBRIC, TBRIC,
     .                   SFAC , DXM   , NBA   , NELA , NNA  ,
     .                   TBA  , TFACA , TAGELS, IBUFA,
     .                   ELEMA, TAGELA, IXS   , NNF  )
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
#include      "units_c.inc"
#include      "sysunit.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBUF(*), ELEM(3,*), IVOLU(*), BRIC(8,*),
     .        NEL, NBRIC, TBRIC(13,*), NBA, NELA, NNA, TBA(2,*),
     .        TFACA(12,*), TAGELS(*), IBUFA(*), ELEMA(3,*), TAGELA(*),
     .        IXS(NIXS,*), NNF
      my_real
     .        X(3,*), XB(3,*), RVOLU(*), SFAC(6,4,*), DXM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ILVOUT, NLAYER, NFACMAX, NPPMAX, I, J, IEL, N1, N2, N3,
     .        NN1, NN2, NN3, GRBRIC, IAD, NPOLY, NNS, ITAGB(NBRIC),
     .        NVMAX, NG, INFO, ICUT, NVERTS, NF1, NF2, NF3, NF4, NS,
     .        NSMAX, NV, K, KK, NNP, NPOLMAX, NRPMAX, N, M, MM, NPOLH,
     .        NPOLB, ITY, NN, NPHMAX, NPOLHMAX, JJ, JJJ, II,
     .        NNS_OLD, NNP_OLD, NPA, JMAX, IMAX, JJMAX, NP, NNTR,
     .        NPOLY_OLD, IPSURF, IC1, IC2, NHOL, NELP, NPOLH_OLD,
     .        L, LL, IFV, NNS2, NPOLY2, NPL, POLC, NSPOLY, NCPOLY,
     .        NPOLHF, NNB, FILEN, LENP, LENH, IP, JMIN, LENIMAX,
     .        LENRMAX, KKK, NSEG, IMIN, NFAC, N4, NN4, IB, IFOUND,
     .        ITAGBA(NBA), IBSA(NBA), NALL, III,
     .        NNS_ANIM, NBZ, NBNEDGE, NNS3, NPOLY_NEW, NNSP, NEDGE,
     .        ITYZ, INZ, J0, NNS1, K0, I1, I2, IDEP, NSTR, NCTR, IIZ,
     .        NNSA
      PARAMETER (NVMAX=12)
      my_real
     .        VOLG, X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, X12, Y12, Z12, 
     .        X13, Y13, Z13, NRX, NRY, NRZ, AREA2, ELAREA(NEL),
     .        NORM(3,NEL), XBMAX, YBMAX, ZBMAX, XBMIN, YBMIN, ZBMIN,
     .        XC, YC, ZC, XX, YY, ZZ, PP(3,3), XL7(3), BCENTER(3),
     .        BHALFSIZE(3), XTMAX, YTMAX, ZTMAX, XTMIN, YTMIN, ZTMIN,
     .        TVERTS(9), TMPTRI(3,3), TMPBOX(3,8), TMPNORM(3,6), TOLE,
     .        XG, YG, ZG, FV0(3), FV1(3), FV2(3), FU0(3), FU1(3),
     .        FU2(3), QUAD(3,4), NR(3), AREA, NX, NY, NZ, NNX,
     .        NNY, NNZ, X0, Y0, Z0, LMAX2, TOLE2, DD, VM, VOLUMIN, 
     .        AREAMAX, VOLPH, AREAP, AREAEL, VPX, VPY, VPZ, 
     .        V1X, V1Y, V1Z, V2X, V2Y, V2Z, NRM1, VX, VY, VZ, SS,
     .        NORMF(3,4), KSI, ETA, VX1, VY1, VZ1, VX2, VY2, VZ2,
     .        VMIN, VTMP, X4, Y4, Z4, X14, Y14, Z14, NORMA(3,NELA),
     .        DD2, XN(3), ZLMIN, ZLMAX, ZL, VNORM, VX3, VY3, VZ3, LZ,
     .        DZ, ALPHA, GAMAI, CPAI, CPBI, CPCI, RMWI, PINI, TI, CPI,
     .        CVI, RHOI, ZL1, ZL2, ZL3, ZLC, VAL, XXX(3,NNF),
     .        XXXA(3,NNA)
      CHARACTER CHFVB*7, CHMESH*5, FILNAM*116
C
      INTEGER, ALLOCATABLE :: FACET(:,:), IPOLY(:,:),
     .                        IELNOD(:,:), POLH(:,:), IPOLY_F(:,:),
     .                        POLH_F(:,:), IFVNOD(:), IFVNOD_OLD(:),
     .                        REDIR_POLY(:), PSEG(:,:), REDIR(:),
     .                        PTRI(:,:), REDIR_POLH(:), POLB(:),
     .                        IPOLY_OLD(:), POLH_NEW(:,:), ITAGT(:),
     .                        TRI(:,:), ADR(:), ADR_OLD(:), IMERGED(:),
     .                        IPOLY_F_OLD(:,:), INEDGE(:,:), NREF(:,:),
     .                        IZ(:,:), LEDGE(:), IFVNOD2(:,:), ITAGN(:)
      my_real
     .       , ALLOCATABLE :: NORMT(:,:), POLY(:,:), RPOLY(:,:),
     .                        RPOLY_F(:,:), VOLU(:), PNODES(:,:),
     .                        PHOLES(:,:), RPOLY_OLD(:), VOLUSORT(:),
     .                        VOLU_OLD(:), RPOLY_F_OLD(:,:),
     .                        RFVNOD_OLD(:,:), XNEW(:,:), RNEDGE(:,:),
     .                        AREF(:,:), RFVNOD2(:,:), XNS(:,:), 
     .                        XNS2(:,:), XXXSA(:,:)
C
      INTEGER FAC(4,6), FACNOR(4,6), FAC4(3,4)
      DATA FAC /1,4,3,2,
     .          5,6,7,8,
     .          1,2,6,5,
     .          2,3,7,6,
     .          3,4,8,7,
     .          4,1,5,8/
      DATA FACNOR /3,4,5,6,
     .             3,4,5,6,
     .             1,4,2,6,
     .             1,5,2,3,
     .             1,6,2,4,
     .             1,3,2,5/
      DATA FAC4 /1,5,3,
     .           3,5,6,
     .           6,5,1,
     .           1,3,6/
C liste chainee polygones
      TYPE POLYGONE
         INTEGER IZ(3), NNSP
         INTEGER, DIMENSION(:), POINTER :: IPOLY, IELNOD
         INTEGER, DIMENSION(:,:), POINTER :: NREF
         my_real
     .          , DIMENSION(:), POINTER :: RPOLY
         my_real
     .          , DIMENSION(:,:), POINTER ::  AREF
         TYPE(POLYGONE), POINTER :: PTR
      END TYPE POLYGONE
      TYPE(POLYGONE), POINTER :: FIRST, PTR_PREC, PTR_CUR, PTR_TMP,
     .                           PTR_OLD, FIRST2, PTR_CUR2, PTR_PREC2,
     .                           PTR_TMP2
C liste chainee polyhedres
      TYPE POLYHEDRE
         INTEGER RANK
         INTEGER, DIMENSION(:), POINTER :: POLH
         TYPE(POLYHEDRE), POINTER :: PTR
      END TYPE POLYHEDRE
      TYPE(POLYHEDRE), POINTER :: PHFIRST, PH_PREC, PH_CUR, PH_TMP
C
C Structure de donnees maillage volumes finis
C    - NNS   : Nombre de nouveaux noeuds ( = IVOLU(46) )
C    - NNTR  : Nombre de nouveaux triangles ( = IVOLU(47) )
C    - NPOLY : Nombre de polygones ( = IVOLU(48) )
C    - NPOLH : Nombre de polyhedres ( volumes finis) ( = IVOLU(49) )
C
C    - IFVNOD(NNS)   : Triangle airbag de reference pour le nouveau noeud
C    - RFVNOD(2,NNS) : Coordonnees locales du noeud dans le triangle
C
C    - IFVTRI(6,NNTR) : Connectivite nouveaux triangles (1->3)
C                       Element de reference si appuye sur surface (4)
C                       Polyhedres connectes si interne (5->6)
C
C    - IFVPOLY(NNTR)    : Attribution triangles-polygones
C    - IFVTADR(NPOLY+1) : Adresses dans le tableau precedent
C
C    - IFVPOLH(NPOLY)   : Attribution polygones-polyhedres
C    - IFVPADR(NPOLH+1) : Adresses dans le tableau precedent
C
      IFV=IVOLU(45)
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
      ELSE
         CALL SPMD_FVB_GATH(IFV, X, XXX, XXXA, XXXSA,
     .                      2  )
         IF (ISPMD/=FVSPMD(IFV)%PMAIN-1) RETURN
      ENDIF
      NULLIFY(FIRST)
      NULLIFY(PHFIRST)
C
      ILVOUT=IVOLU(44)
C
      NLAYER=IVOLU(40)
      NFACMAX=IVOLU(41)
      NPPMAX=IVOLU(42)
C
      DO I=1,NBRIC
         TBRIC(7,I)=0
         DO J=1,6
            TBRIC(7+J,I)=0
         ENDDO
      ENDDO    
C---------------------------------------------------
C Normales elementaires et volume de l'airbag
C---------------------------------------------------
      VOLG=ZERO
      DO IEL=1,NEL
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
         ELAREA(IEL)=HALF*AREA2
         NORM(1,IEL)=NRX/AREA2
         NORM(2,IEL)=NRY/AREA2
         NORM(3,IEL)=NRZ/AREA2
C
         VOLG=VOLG+ONE_OVER_6*(X1*NRX+Y1*NRY+Z1*NRZ)
      ENDDO
C
      DO IEL=1,NELA
         N1=ELEMA(1,IEL)
         N2=ELEMA(2,IEL)
         N3=ELEMA(3,IEL)
         IF (TAGELA(IEL)>0) THEN
            X1=XXX(1,N1)
            Y1=XXX(2,N1)
            Z1=XXX(3,N1)
            X2=XXX(1,N2)
            Y2=XXX(2,N2)
            Z2=XXX(3,N2)
            X3=XXX(1,N3)
            Y3=XXX(2,N3)
            Z3=XXX(3,N3)
         ELSEIF (TAGELA(IEL)<0) THEN
            X1=XXXA(1,N1)
            Y1=XXXA(2,N1)
            Z1=XXXA(3,N1)
            X2=XXXA(1,N2)
            Y2=XXXA(2,N2)
            Z2=XXXA(3,N2)
            X3=XXXA(1,N3)
            Y3=XXXA(2,N3)
            Z3=XXXA(3,N3)
         ENDIF
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
         NORMA(1,IEL)=NRX/AREA2
         NORMA(2,IEL)=NRY/AREA2
         NORMA(3,IEL)=NRZ/AREA2
      ENDDO   
C---------------------------------------------------
C Creation de tous les polygones
C---------------------------------------------------
      NPOLY=0
      NNS=0
      NPOLY2=0
      NNS2=0
      IF (NELA==0) THEN
         NPOLY=0
         NPOLH=0
         GOTO 370
      ENDIF
C
      DO I=1,NBRIC
         ITAGB(I)=0
      ENDDO
C 1. Facettes airbag coupees
      IF (ILVOUT/=0) THEN
         WRITE(ISTDO,*)
         WRITE(ISTDO,'(A25,I10,A23)') 
     . ' ** MONITORED VOLUME ID: ',IVOLU(1),' - BUILDING POLYGONS **'
      ENDIF
C
C nullify sinon test sur associated vrai second appel fvmesh1
      NULLIFY(FIRST2)
      DO I=1,NBRIC
C        IF (ILVOUT<0) CALL PROGBAR_C(I,NBRIC)
C         
         PTR_OLD => PTR_CUR
         NPOLY_OLD=NPOLY
         NNS_OLD=NNS
  100    CONTINUE
         IF (ALLOCATED(FACET)) DEALLOCATE(FACET)
         IF (ALLOCATED(TRI))   DEALLOCATE(TRI)
         IF (ALLOCATED(NORMT)) DEALLOCATE(NORMT)
         IF (ALLOCATED(POLY))  DEALLOCATE(POLY)
         ALLOCATE(FACET(6,1+NFACMAX), TRI(NFACMAX,4),
     .            NORMT(3,NFACMAX), POLY(3,NVMAX))
         DO J=1,6
            FACET(J,1)=0
         ENDDO
C
         XBMAX=-EP30
         YBMAX=-EP30
         ZBMAX=-EP30
         XBMIN=EP30
         YBMIN=EP30
         ZBMIN=EP30
         XC=ZERO
         YC=ZERO
         ZC=ZERO
         DO J=1,8
            XX=XB(1,BRIC(J,I))
            YY=XB(2,BRIC(J,I))
            ZZ=XB(3,BRIC(J,I))
            XBMAX=MAX(XBMAX,XX)
            YBMAX=MAX(YBMAX,YY)
            ZBMAX=MAX(ZBMAX,ZZ)
            XBMIN=MIN(XBMIN,XX)
            YBMIN=MIN(YBMIN,YY)
            ZBMIN=MIN(ZBMIN,ZZ)
            XC=XC+ONE_OVER_8*XX
            YC=YC+ONE_OVER_8*YY
            ZC=ZC+ONE_OVER_8*ZZ
         ENDDO
C Passage repere local brique
         PP(1,1)=SFAC(4,2,I)
         PP(2,1)=SFAC(4,3,I)
         PP(3,1)=SFAC(4,4,I)
         PP(1,2)=SFAC(5,2,I)
         PP(2,2)=SFAC(5,3,I)
         PP(3,2)=SFAC(5,4,I)
         PP(1,3)=SFAC(2,2,I)
         PP(2,3)=SFAC(2,3,I)
         PP(3,3)=SFAC(2,4,I)
C
         XX=XB(1,BRIC(7,I))
         YY=XB(2,BRIC(7,I))
         ZZ=XB(3,BRIC(7,I))
         XL7(1)=PP(1,1)*(XX-XC)+PP(2,1)*(YY-YC)+PP(3,1)*(ZZ-ZC)
         XL7(2)=PP(1,2)*(XX-XC)+PP(2,2)*(YY-YC)+PP(3,2)*(ZZ-ZC)
         XL7(3)=PP(1,3)*(XX-XC)+PP(2,3)*(YY-YC)+PP(3,3)*(ZZ-ZC)
         BCENTER(1)=ZERO
         BCENTER(2)=ZERO
         BCENTER(3)=ZERO
         BHALFSIZE(1)=XL7(1)
         BHALFSIZE(2)=XL7(2)
         BHALFSIZE(3)=XL7(3)
         INFO=0
         DO IEL=1,NELA
            N1=ELEMA(1,IEL)
            N2=ELEMA(2,IEL)
            N3=ELEMA(3,IEL)
            IF (TAGELA(IEL)>0) THEN
               X1=XXX(1,N1)
               Y1=XXX(2,N1)
               Z1=XXX(3,N1)
               X2=XXX(1,N2)
               Y2=XXX(2,N2)
               Z2=XXX(3,N2)
               X3=XXX(1,N3)
               Y3=XXX(2,N3)
               Z3=XXX(3,N3)
            ELSEIF (TAGELA(IEL)<0) THEN
               X1=XXXA(1,N1)
               Y1=XXXA(2,N1)
               Z1=XXXA(3,N1)
               X2=XXXA(1,N2)
               Y2=XXXA(2,N2)
               Z2=XXXA(3,N2)
               X3=XXXA(1,N3)
               Y3=XXXA(2,N3)
               Z3=XXXA(3,N3)
            ENDIF
            XTMAX=MAX(X1,X2,X3)
            YTMAX=MAX(Y1,Y2,Y3)
            ZTMAX=MAX(Z1,Z2,Z3)
            XTMIN=MIN(X1,X2,X3)
            YTMIN=MIN(Y1,Y2,Y3)
            ZTMIN=MIN(Z1,Z2,Z3)
            IF (XBMAX<XTMIN.OR.YBMAX<YTMIN.OR.ZBMAX<ZTMIN.OR.
     .          XBMIN>XTMAX.OR.YBMIN>YTMAX.OR.ZBMIN>ZTMAX) 
     .         CYCLE
C
            TVERTS(1)=PP(1,1)*(X1-XC)+PP(2,1)*(Y1-YC)+PP(3,1)*(Z1-ZC)
            TVERTS(2)=PP(1,2)*(X1-XC)+PP(2,2)*(Y1-YC)+PP(3,2)*(Z1-ZC)
            TVERTS(3)=PP(1,3)*(X1-XC)+PP(2,3)*(Y1-YC)+PP(3,3)*(Z1-ZC)
            TVERTS(4)=PP(1,1)*(X2-XC)+PP(2,1)*(Y2-YC)+PP(3,1)*(Z2-ZC)
            TVERTS(5)=PP(1,2)*(X2-XC)+PP(2,2)*(Y2-YC)+PP(3,2)*(Z2-ZC)
            TVERTS(6)=PP(1,3)*(X2-XC)+PP(2,3)*(Y2-YC)+PP(3,3)*(Z2-ZC)
            TVERTS(7)=PP(1,1)*(X3-XC)+PP(2,1)*(Y3-YC)+PP(3,1)*(Z3-ZC)
            TVERTS(8)=PP(1,2)*(X3-XC)+PP(2,2)*(Y3-YC)+PP(3,2)*(Z3-ZC)
            TVERTS(9)=PP(1,3)*(X3-XC)+PP(2,3)*(Y3-YC)+PP(3,3)*(Z3-ZC)
C
            CALL TRIBOX3(ICUT, BCENTER, BHALFSIZE, TVERTS)
C Liste elargie pour faces laterales
            IF (ICUT==0) THEN
               TOLE=EM5
*               TOLE=ZERO
               XG=THIRD*(X1+X2+X3)
               YG=THIRD*(Y1+Y2+Y3)
               ZG=THIRD*(Z1+Z2+Z3)
               X1=XG+(ONE+TOLE)*(X1-XG)
               Y1=YG+(ONE+TOLE)*(Y1-YG)
               Z1=ZG+(ONE+TOLE)*(Z1-ZG)
               X2=XG+(ONE+TOLE)*(X2-XG)
               Y2=YG+(ONE+TOLE)*(Y2-YG)
               Z2=ZG+(ONE+TOLE)*(Z2-ZG)
               X3=XG+(ONE+TOLE)*(X3-XG)
               Y3=YG+(ONE+TOLE)*(Y3-YG)
               Z3=ZG+(ONE+TOLE)*(Z3-ZG)
C
               TVERTS(1)=PP(1,1)*(X1-XC)+PP(2,1)*(Y1-YC)+PP(3,1)*(Z1-ZC)
               TVERTS(2)=PP(1,2)*(X1-XC)+PP(2,2)*(Y1-YC)+PP(3,2)*(Z1-ZC)
               TVERTS(3)=PP(1,3)*(X1-XC)+PP(2,3)*(Y1-YC)+PP(3,3)*(Z1-ZC)
               TVERTS(4)=PP(1,1)*(X2-XC)+PP(2,1)*(Y2-YC)+PP(3,1)*(Z2-ZC)
               TVERTS(5)=PP(1,2)*(X2-XC)+PP(2,2)*(Y2-YC)+PP(3,2)*(Z2-ZC)
               TVERTS(6)=PP(1,3)*(X2-XC)+PP(2,3)*(Y2-YC)+PP(3,3)*(Z2-ZC)
               TVERTS(7)=PP(1,1)*(X3-XC)+PP(2,1)*(Y3-YC)+PP(3,1)*(Z3-ZC)
               TVERTS(8)=PP(1,2)*(X3-XC)+PP(2,2)*(Y3-YC)+PP(3,2)*(Z3-ZC)
               TVERTS(9)=PP(1,3)*(X3-XC)+PP(2,3)*(Y3-YC)+PP(3,3)*(Z3-ZC)
C
               CALL TRIBOX3(ICUT, BCENTER, BHALFSIZE, TVERTS)
               IF (ICUT==1) ICUT=2
            ENDIF
C               
            IF (ICUT>=1) THEN
               IF (ICUT==1) THEN
                  TBRIC(7,I)=2
                  TMPTRI(1,1)=X1
                  TMPTRI(2,1)=Y1
                  TMPTRI(3,1)=Z1
                  TMPTRI(1,2)=X2
                  TMPTRI(2,2)=Y2
                  TMPTRI(3,2)=Z2
                  TMPTRI(1,3)=X3
                  TMPTRI(2,3)=Y3
                  TMPTRI(3,3)=Z3
                  DO J=1,8
                     TMPBOX(1,J)=XB(1,BRIC(J,I))
                     TMPBOX(2,J)=XB(2,BRIC(J,I))
                     TMPBOX(3,J)=XB(3,BRIC(J,I))
                  ENDDO
                  DO J=1,6
                     TMPNORM(1,J)=-SFAC(J,2,I)
                     TMPNORM(2,J)=-SFAC(J,3,I)
                     TMPNORM(3,J)=-SFAC(J,4,I)
                  ENDDO
                  CALL ITRIBOX(TMPTRI, TMPBOX, TMPNORM, NVERTS, POLY,
     .                         NVMAX )
                  IF (NVERTS>0) THEN
                     NPOLY=NPOLY+1
                     IF (.NOT.ASSOCIATED(FIRST)) THEN
                        ALLOCATE(FIRST)
                        PTR_CUR => FIRST
                     ELSE
                        ALLOCATE(PTR_CUR)
                        PTR_PREC%PTR => PTR_CUR
                     ENDIF
                     ALLOCATE(PTR_CUR%IPOLY(6+NVERTS),
     .                        PTR_CUR%IELNOD(NVERTS),
     .                        PTR_CUR%RPOLY(4+3*NVERTS))
                     PTR_CUR%IPOLY(1)=1
                     PTR_CUR%IPOLY(2)=NVERTS
                     PTR_CUR%IPOLY(3)=IEL
                     PTR_CUR%IPOLY(4)=I
                     PTR_CUR%IPOLY(5)=0
                     PTR_CUR%IPOLY(6)=0
                     PTR_CUR%RPOLY(1)=ZERO
                     PTR_CUR%RPOLY(2)=NORMA(1,IEL)
                     PTR_CUR%RPOLY(3)=NORMA(2,IEL)
                     PTR_CUR%RPOLY(4)=NORMA(3,IEL)
                     DO J=1,NVERTS
                        NNS=NNS+1
                        PTR_CUR%IPOLY(6+J)=NNS
                        PTR_CUR%IELNOD(J)=IEL
                        PTR_CUR%RPOLY(4+3*(J-1)+1)=POLY(1,J)
                        PTR_CUR%RPOLY(4+3*(J-1)+2)=POLY(2,J)
                        PTR_CUR%RPOLY(4+3*(J-1)+3)=POLY(3,J)
                     ENDDO
                     PTR_PREC => PTR_CUR
                  ENDIF
               ENDIF
C Facettes coupees (1 quadrangle=2 triangles)
               TOLE=EM5
*               TOLE=ZERO
               XG=THIRD*(X1+X2+X3)
               YG=THIRD*(Y1+Y2+Y3)
               ZG=THIRD*(Z1+Z2+Z3)
               X1=XG+(ONE+TOLE)*(X1-XG)
               Y1=YG+(ONE+TOLE)*(Y1-YG)
               Z1=ZG+(ONE+TOLE)*(Z1-ZG)
               X2=XG+(ONE+TOLE)*(X2-XG)
               Y2=YG+(ONE+TOLE)*(Y2-YG)
               Z2=ZG+(ONE+TOLE)*(Z2-ZG)
               X3=XG+(ONE+TOLE)*(X3-XG)
               Y3=YG+(ONE+TOLE)*(Y3-YG)
               Z3=ZG+(ONE+TOLE)*(Z3-ZG)
C
               FV0(1)=X1
               FV0(2)=Y1
               FV0(3)=Z1
               FV1(1)=X2
               FV1(2)=Y2
               FV1(3)=Z2
               FV2(1)=X3
               FV2(2)=Y3
               FV2(3)=Z3
               DO J=1,6
                  NF1=BRIC(FAC(1,J),I)
                  NF2=BRIC(FAC(2,J),I)
                  NF3=BRIC(FAC(3,J),I)
                  NF4=BRIC(FAC(4,J),I)
                  FU0(1)=XB(1,NF1)
                  FU0(2)=XB(2,NF1)
                  FU0(3)=XB(3,NF1)
                  FU1(1)=XB(1,NF2)
                  FU1(2)=XB(2,NF2)
                  FU1(3)=XB(3,NF2)
                  FU2(1)=XB(1,NF3)
                  FU2(2)=XB(2,NF3)
                  FU2(3)=XB(3,NF3)
C
                  CALL TRITRI3(ICUT, FV0, FV1, FV2, FU0, FU1, FU2)
                  IF (ICUT==1) THEN
                     TBRIC(7+J,I)=2
                     NS=FACET(J,1)
                     NS=NS+1
                     FACET(J,1)=NS
                     IF (NS>NFACMAX) THEN
                        INFO=1
                     ELSE
                        FACET(J,1+NS)=IEL
                     ENDIF
                     CYCLE
                  ENDIF
C
                  FU1(1)=XB(1,NF3)
                  FU1(2)=XB(2,NF3)
                  FU1(3)=XB(3,NF3)
                  FU2(1)=XB(1,NF4)
                  FU2(2)=XB(2,NF4)
                  FU2(3)=XB(3,NF4)
C
                  CALL TRITRI3(ICUT, FV0, FV1, FV2, FU0, FU1, FU2)
                  IF (ICUT==1) THEN
                     TBRIC(7+J,I)=2
                     NS=FACET(J,1)
                     NS=NS+1
                     FACET(J,1)=NS
                     IF (NS>NFACMAX) THEN
                        INFO=1
                     ELSE
                        FACET(J,1+NS)=IEL
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
C
         IF (INFO==1) THEN
            NSMAX=0
            DO J=1,6
               NSMAX=MAX(NSMAX,FACET(J,1))
            ENDDO
            NFACMAX=NSMAX
C Destruction des polygones crees pour rien
            IF (NPOLY_OLD>0) THEN
               PTR_CUR => PTR_OLD%PTR
            ELSE
               PTR_CUR => FIRST
            ENDIF
            DO J=1,NPOLY-NPOLY_OLD
               PTR_TMP => PTR_CUR%PTR
               DEALLOCATE(PTR_CUR%IPOLY, PTR_CUR%RPOLY, 
     .                    PTR_CUR%IELNOD)
               DEALLOCATE(PTR_CUR)
               PTR_CUR => PTR_TMP
            ENDDO
            IF (NPOLY_OLD>0) THEN
               PTR_PREC => PTR_OLD
            ELSE
               NULLIFY(FIRST)
            ENDIF
            NPOLY=NPOLY_OLD
            NNS=NNS_OLD
C
            GOTO 100
         ENDIF
C 2. Faces laterales
         IF (TBRIC(7,I)<=1) THEN
           DEALLOCATE(FACET, TRI, NORMT, POLY)
           CYCLE
         END IF
         DO J=1,6
           NV=TBRIC(J,I)
            IF (NV==0) CYCLE
            IF (ITAGB(NV)==1) CYCLE
            IF (TBRIC(7+J,I)==2) THEN
               DO K=1,4
                  KK=FAC(K,J)
                  QUAD(1,K)=XB(1,BRIC(KK,I))
                  QUAD(2,K)=XB(2,BRIC(KK,I))
                  QUAD(3,K)=XB(3,BRIC(KK,I))
               ENDDO
               NS=FACET(J,1)
               DO K=1,NS
                  IEL=FACET(J,1+K)
                  IF (TAGELA(IEL)>0) THEN
                     DO L=1,3
                        LL=ELEMA(L,IEL)
                        TRI(K,L)=LL
                     ENDDO
                  ELSEIF (TAGELA(IEL)<0) THEN
                     DO L=1,3
                        LL=-ELEMA(L,IEL)
                        TRI(K,L)=LL
                     ENDDO
                  ENDIF
                  TRI(K,4)=IEL
                  NORMT(1,K)=NORMA(1,IEL)
                  NORMT(2,K)=NORMA(2,IEL)
                  NORMT(3,K)=NORMA(3,IEL)
               ENDDO
               DO K=1,4
                  NORMF(1,K)=SFAC(FACNOR(K,J),2,I)
                  NORMF(2,K)=SFAC(FACNOR(K,J),3,I)
                  NORMF(3,K)=SFAC(FACNOR(K,J),4,I)
               ENDDO
               NR(1)=SFAC(J,2,I)
               NR(2)=SFAC(J,3,I)
               NR(3)=SFAC(J,4,I)
C
               NNP=0
               NPOLMAX=NLAYER
C
  200          CONTINUE
               NRPMAX=NPPMAX*3+4
               IF (ALLOCATED(IPOLY))  DEALLOCATE(IPOLY)
               IF (ALLOCATED(RPOLY))  DEALLOCATE(RPOLY)
               IF (ALLOCATED(IELNOD)) DEALLOCATE(IELNOD)
               ALLOCATE(IPOLY(6+NPPMAX+1+NPOLMAX,NPOLMAX), 
     .                  RPOLY(NRPMAX+3*NPOLMAX,NPOLMAX),
     .                  IELNOD(NPPMAX,NPOLMAX))
C
               CALL FACEPOLY(QUAD,   TRI,   NS,     IPOLY,   RPOLY,
     .                       NR,     NORMF, NORMT,  NFACMAX, NNP,
     .                       NRPMAX, I,     NV,     DXM    , NPOLMAX,
     .                       NPPMAX, INFO,  IELNOD, XXX,     ELEMA,
     .                       IBUF,   NELA,  I,      J,       IVOLU(1),
     .                       ILVOUT, IBUFA, TAGELA, XXXA   )
               IF (INFO==1) GOTO 200
C
               DO N=1,NNP
                  IF (IPOLY(1,N)==-1) CYCLE
                  NPOLY2=NPOLY2+1
                  IF (.NOT.ASSOCIATED(FIRST2)) THEN
                     ALLOCATE(FIRST2)
                     PTR_CUR2 => FIRST2
                  ELSE
                     ALLOCATE(PTR_CUR2)
                     PTR_PREC2%PTR => PTR_CUR2
                  ENDIF
                  NN=IPOLY(2,N)
                  NHOL=IPOLY(6+NN+1,N)
                  ALLOCATE(PTR_CUR2%IPOLY(6+NN+1+NHOL),
     .                     PTR_CUR2%IELNOD(NN),
     .                     PTR_CUR2%RPOLY(4+3*NN+3*NHOL))
                  DO M=1,6
                     PTR_CUR2%IPOLY(M)=IPOLY(M,N)
                  ENDDO
                  DO M=1,4
                     PTR_CUR2%RPOLY(M)=RPOLY(M,N)
                  ENDDO
                  DO M=1,IPOLY(2,N)
                     NNS2=NNS2+1
                     PTR_CUR2%IPOLY(6+M)=NNS2
                     MM=IELNOD(M,N)
                     PTR_CUR2%IELNOD(M)=FACET(J,1+MM)
                     PTR_CUR2%RPOLY(4+3*(M-1)+1)=RPOLY(4+3*(M-1)+1,N)
                     PTR_CUR2%RPOLY(4+3*(M-1)+2)=RPOLY(4+3*(M-1)+2,N)
                     PTR_CUR2%RPOLY(4+3*(M-1)+3)=RPOLY(4+3*(M-1)+3,N)
                  ENDDO
                  PTR_CUR2%IPOLY(6+NN+1)=NHOL
                  DO M=1,NHOL
                     PTR_CUR2%IPOLY(6+NN+1+M)=IPOLY(6+NN+1+M,N)
                     PTR_CUR2%RPOLY(4+3*NN+3*(M-1)+1)=
     .                        RPOLY(4+3*NN+3*(M-1)+1,N)
                     PTR_CUR2%RPOLY(4+3*NN+3*(M-1)+2)=
     .                        RPOLY(4+3*NN+3*(M-1)+2,N)
                     PTR_CUR2%RPOLY(4+3*NN+3*(M-1)+3)=
     .                        RPOLY(4+3*NN+3*(M-1)+3,N)
                  ENDDO
                  PTR_PREC2 => PTR_CUR2
               ENDDO
C
               DEALLOCATE(IPOLY, RPOLY, IELNOD)
            ENDIF
         ENDDO
         ITAGB(I)=1
C
         DEALLOCATE(FACET, TRI, NORMT, POLY)
      ENDDO
C Concatenation des deux listes
      PTR_CUR2 => FIRST2
      PTR_PREC => PTR_CUR
      DO I=1,NPOLY2
         NPOLY=NPOLY+1
         ALLOCATE(PTR_CUR)
         PTR_PREC%PTR => PTR_CUR
         NN=PTR_CUR2%IPOLY(2)
         NHOL=PTR_CUR2%IPOLY(6+NN+1)
         ALLOCATE(PTR_CUR%IPOLY(6+NN+1+NHOL),
     .            PTR_CUR%IELNOD(NN),
     .            PTR_CUR%RPOLY(4+3*NN+3*NHOL))
         DO J=1,6
            PTR_CUR%IPOLY(J)=PTR_CUR2%IPOLY(J)
         ENDDO
         DO J=1,4
            PTR_CUR%RPOLY(J)=PTR_CUR2%RPOLY(J)
         ENDDO
         DO J=1,NN
            PTR_CUR%IPOLY(6+J)=NNS+PTR_CUR2%IPOLY(6+J)
            PTR_CUR%IELNOD(J)=PTR_CUR2%IELNOD(J)
            PTR_CUR%RPOLY(4+3*(J-1)+1)=PTR_CUR2%RPOLY(4+3*(J-1)+1)
            PTR_CUR%RPOLY(4+3*(J-1)+2)=PTR_CUR2%RPOLY(4+3*(J-1)+2)
            PTR_CUR%RPOLY(4+3*(J-1)+3)=PTR_CUR2%RPOLY(4+3*(J-1)+3)
         ENDDO
         PTR_CUR%IPOLY(6+NN+1)=NHOL
         DO J=1,NHOL
            PTR_CUR%IPOLY(6+NN+1+J)=PTR_CUR2%IPOLY(6+NN+1+J)
            PTR_CUR%RPOLY(4+3*NN+3*(J-1)+1)=
     .              PTR_CUR2%RPOLY(4+3*NN+3*(J-1)+1)
            PTR_CUR%RPOLY(4+3*NN+3*(J-1)+2)=
     .              PTR_CUR2%RPOLY(4+3*NN+3*(J-1)+2)
            PTR_CUR%RPOLY(4+3*NN+3*(J-1)+3)=
     .              PTR_CUR2%RPOLY(4+3*NN+3*(J-1)+3)
         ENDDO
         PTR_TMP2 => PTR_CUR2%PTR
         DEALLOCATE(PTR_CUR2%IPOLY, PTR_CUR2%RPOLY, PTR_CUR2%IELNOD)
         DEALLOCATE(PTR_CUR2)
         PTR_CUR2 => PTR_TMP2
         PTR_PREC => PTR_CUR
      ENDDO
C
      NNS=NNS+NNS2
C---------------------------------------------------
C Decoupage des polygones selon la verticale locale
C---------------------------------------------------
      NBZ=IVOLU(65)
      IF (NBZ>1) THEN
         PTR_CUR => FIRST
         DO I=1,NPOLY
            PTR_CUR%NNSP=0
            PTR_CUR => PTR_CUR%PTR
         ENDDO
C
         VX3=RVOLU(35)
         VY3=RVOLU(36)
         VZ3=RVOLU(37)
         VX1=RVOLU(38)
         VY1=RVOLU(39)
         VZ1=RVOLU(40)
C Repere local
         VNORM=SQRT(VX3**2+VY3**2+VZ3**2)
         VX3=VX3/VNORM
         VY3=VY3/VNORM
         VZ3=VZ3/VNORM
         SS=VX3*VX1+VY3*VY1+VZ3*VZ1
         VX1=VX1-SS*VX3
         VY1=VY1-SS*VY3
         VZ1=VZ1-SS*VZ3
         VNORM=SQRT(VX1**2+VY1**2+VZ1**2)
         VX1=VX1/VNORM
         VY1=VY1/VNORM
         VZ1=VZ1/VNORM
         VX2=VY3*VZ1-VZ3*VY1
         VY2=VZ3*VX1-VX3*VZ1
         VZ2=VX3*VY1-VY3*VX1
C
         X0=RVOLU(41)
         Y0=RVOLU(42)
         Z0=RVOLU(43)
         LZ=RVOLU(53)
         DZ=TWO*LZ/NBZ
         DXM=MIN(DXM,DZ)
C
         ZBMIN=EP30
         ZBMAX=-EP30
         DO IEL=1,NEL
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
            X12=X2-X1
            Y12=Y2-Y1
            Z12=Z2-Z1
            X13=X3-X1
            Y13=Y3-Y1
            Z13=Z3-Z1
            ZL1=(X1-X0)*VX3+(Y1-Y0)*VY3+(Z1-Z0)*VZ3
            ZL2=(X2-X0)*VX3+(Y2-Y0)*VY3+(Z2-Z0)*VZ3
            ZL3=(X3-X0)*VX3+(Y3-Y0)*VY3+(Z3-Z0)*VZ3
            ZBMIN=MIN(ZBMIN,ZL1)
            ZBMIN=MIN(ZBMIN,ZL2)
            ZBMIN=MIN(ZBMIN,ZL3)
            ZBMAX=MAX(ZBMAX,ZL1)
            ZBMAX=MAX(ZBMAX,ZL2)
            ZBMAX=MAX(ZBMAX,ZL3)
         ENDDO
C Clipping des polygones
         X0=X0-LZ*VX3
         Y0=Y0-LZ*VY3
         Z0=Z0-LZ*VZ3
         ZLC=-LZ
         ALLOCATE(INEDGE(6,NPOLY*NPPMAX), RNEDGE(6,NPOLY*NPPMAX))
         NBNEDGE=0
         NNS3=0
         NPOLY_NEW=NPOLY
         IIZ=0
         DO I=1,NBZ+1
            IF (ZLC<ZBMIN.OR.ZLC>ZBMAX) THEN
               X0=X0+DZ*VX3
               Y0=Y0+DZ*VY3
               Z0=Z0+DZ*VZ3
               ZLC=ZLC+DZ
               CYCLE
            ENDIF
            IIZ=IIZ+1
            PTR_CUR => FIRST
            DO J=1,NPOLY
               ZLMIN=EP30
               ZLMAX=-EP30
               DO K=1,PTR_CUR%IPOLY(2)
                  XX=PTR_CUR%RPOLY(4+3*(K-1)+1)
                  YY=PTR_CUR%RPOLY(4+3*(K-1)+2)
                  ZZ=PTR_CUR%RPOLY(4+3*(K-1)+3)
                  ZL=(XX-X0)*VX3+(YY-Y0)*VY3+(ZZ-Z0)*VZ3
                  ZLMIN=MIN(ZLMIN,ZL)
                  ZLMAX=MAX(ZLMAX,ZL)
               ENDDO
               IF (ZLMIN*ZLMAX<ZERO) THEN
                  ITY=PTR_CUR%IPOLY(1)
                  NN=PTR_CUR%IPOLY(2)
                  NHOL=0
                  IF (ITY==2) NHOL=PTR_CUR%IPOLY(6+NN+1)
                  ALLOCATE(IPOLY(6+2*NN+1+NHOL,NN),
     .                     RPOLY(4+3*2*NN+3*NHOL,NN),
     .                     IELNOD(2*NN,NN),
     .                     NREF(2,NN), AREF(4,NN),
     .                     IZ(3,NN))
                  CALL GPOLCUT(
     .    IPOLY,   RPOLY,   PTR_CUR%IPOLY, PTR_CUR%RPOLY,  INEDGE,
     .    RNEDGE,  NBNEDGE, VX3,           VY3,            VZ3,   
     .    X0,      Y0,      Z0,            NREF,           AREF,    
     .    NN,      NHOL,    IIZ,           IZ,             NNS3,
     .    NNP   ,  NNSP,    IELNOD,        PTR_CUR%IELNOD)
C
                  PTR_TMP => PTR_CUR%PTR
                  NPOLY_NEW=NPOLY_NEW-1
                  DO N=1,NNP
                     NPOLY_NEW=NPOLY_NEW+1
                     IF (N==1) THEN
                        DEALLOCATE(PTR_CUR%IPOLY, PTR_CUR%RPOLY,
     .                             PTR_CUR%IELNOD)
                     ELSE
                        ALLOCATE(PTR_CUR)
                        PTR_PREC%PTR => PTR_CUR
                        PTR_CUR%NNSP=0
                     ENDIF
                     NN=IPOLY(2,N)
                     NHOL=IPOLY(6+NN+1,N)
                     ALLOCATE(PTR_CUR%IPOLY(6+NN+1+NHOL),
     .                        PTR_CUR%IELNOD(NN),
     .                        PTR_CUR%RPOLY(4+3*NN+3*NHOL))
                     DO M=1,6
                        PTR_CUR%IPOLY(M)=IPOLY(M,N)
                     ENDDO
                     DO M=1,4
                        PTR_CUR%RPOLY(M)=RPOLY(M,N)
                     ENDDO
                     DO M=1,NN
                        MM=IPOLY(6+M,N)
                        PTR_CUR%IPOLY(6+M)=MM
                        PTR_CUR%IELNOD(M)=IELNOD(M,N)
                        PTR_CUR%RPOLY(4+3*(M-1)+1)=RPOLY(4+3*(M-1)+1,N)
                        PTR_CUR%RPOLY(4+3*(M-1)+2)=RPOLY(4+3*(M-1)+2,N)
                        PTR_CUR%RPOLY(4+3*(M-1)+3)=RPOLY(4+3*(M-1)+3,N)
                     ENDDO
                     NHOL=IPOLY(6+NN+1,N)
                     PTR_CUR%IPOLY(6+NN+1)=NHOL
                     DO M=1,NHOL
                        PTR_CUR%IPOLY(6+NN+1+M)=IPOLY(6+NN+1+M,N)
                        PTR_CUR%RPOLY(4+3*NN+3*(M-1)+1)=
     .                          RPOLY(4+3*NN+3*(M-1)+1,N)
                        PTR_CUR%RPOLY(4+3*NN+3*(M-1)+2)=
     .                          RPOLY(4+3*NN+3*(M-1)+2,N)
                        PTR_CUR%RPOLY(4+3*NN+3*(M-1)+3)=
     .                          RPOLY(4+3*NN+3*(M-1)+3,N)
                     ENDDO
                     PTR_CUR%IZ(1)=1
                     PTR_CUR%IZ(2)=IZ(2,N)
                     IF (N==NNP) THEN
                        PTR_CUR%NNSP=NNSP
                        ALLOCATE(PTR_CUR%NREF(3,NNSP),
     .                           PTR_CUR%AREF(4,NNSP))
                        DO M=1,NNSP
                           NNS3=NNS3+1
                           PTR_CUR%NREF(1,M)=NNS3
                           PTR_CUR%NREF(2,M)=NREF(1,M)
                           PTR_CUR%NREF(3,M)=NREF(2,M)
                           PTR_CUR%AREF(1,M)=AREF(1,M)
                           PTR_CUR%AREF(2,M)=AREF(2,M)
                           PTR_CUR%AREF(3,M)=AREF(3,M)
                           PTR_CUR%AREF(4,M)=AREF(4,M)
                        ENDDO
                     ENDIF
                     PTR_PREC => PTR_CUR
                     IF (N==NNP) PTR_CUR%PTR => PTR_TMP
                  ENDDO
C
                  DEALLOCATE(IPOLY, RPOLY, IELNOD, NREF, AREF, IZ)
               ELSE
                  IF (ZLMIN==ZERO) THEN
                     NN=PTR_CUR%IPOLY(2)
                     ALLOCATE(NREF(2,2*NN), AREF(4,2*NN))
                     CALL HORIEDGE(
     .                 PTR_CUR%IPOLY, PTR_CUR%RPOLY, VX3,    VY3,  VZ3,
     .                 NBNEDGE,       INEDGE,        RNEDGE, X0,   Y0,
     .                 Z0,            IIZ          , NNS3,   NREF, AREF,
     .                 NNSP         )
C
                     IF (NNSP>0) THEN
                        ALLOCATE(PTR_CUR%NREF(3,NNSP), 
     .                           PTR_CUR%AREF(4,NNSP))
                        PTR_CUR%NNSP=NNSP
                        DO N=1,NNSP
                           NNS3=NNS3+1
                           PTR_CUR%NREF(1,N)=NNS3
                           PTR_CUR%NREF(2,N)=NREF(1,N)
                           PTR_CUR%NREF(3,N)=NREF(2,N)
                           PTR_CUR%AREF(1,N)=AREF(1,N)
                           PTR_CUR%AREF(2,N)=AREF(2,N)
                           PTR_CUR%AREF(3,N)=AREF(3,N)
                           PTR_CUR%AREF(4,N)=AREF(4,N)
                        ENDDO
                     ENDIF
                     DEALLOCATE(NREF, AREF)   
                  ENDIF
                  IF (ZLMIN>=ZERO) THEN 
                     PTR_CUR%IZ(1)=1
                     PTR_CUR%IZ(2)=IIZ+1
                  ELSEIF (IIZ==1.AND.ZLMAX<=ZERO) THEN
                     PTR_CUR%IZ(1)=1
                     PTR_CUR%IZ(2)=1
                  ENDIF
                  PTR_PREC => PTR_CUR
               ENDIF
               PTR_CUR => PTR_CUR%PTR
            ENDDO
            NPOLY=NPOLY_NEW
            X0=X0+DZ*VX3
            Y0=Y0+DZ*VY3
            Z0=Z0+DZ*VZ3
            ZLC=ZLC+DZ
         ENDDO
C
         ALLOCATE(REDIR(NNS))
         PTR_CUR => FIRST
         NNS=0
         DO I=1,NPOLY
            NN=PTR_CUR%IPOLY(2)
            DO J=1,NN
               JJ=PTR_CUR%IPOLY(6+J)
               IF (JJ>0) THEN
                  NNS=NNS+1
                  REDIR(JJ)=NNS
               ENDIF
            ENDDO
            PTR_CUR => PTR_CUR%PTR
         ENDDO
C
         PTR_CUR => FIRST
         DO I=1,NPOLY
            NNSP=PTR_CUR%NNSP
            IF (NNSP>0) THEN
               DO J=1,NNSP
                  N1=PTR_CUR%NREF(2,J)
                  N2=PTR_CUR%NREF(3,J)
                  IF (N1>0) PTR_CUR%NREF(2,J)=REDIR(N1)
                  IF (N2>0) PTR_CUR%NREF(3,J)=REDIR(N2)
               ENDDO
            ENDIF
            PTR_CUR => PTR_CUR%PTR
         ENDDO
         DEALLOCATE(REDIR)
C Creation des nouveaux polygones horizontaux
         PTR_CUR => FIRST
         DO I=1,NPOLY
            PTR_PREC => PTR_CUR
            PTR_CUR => PTR_CUR%PTR
         ENDDO
         ALLOCATE(LEDGE(NBNEDGE))
         DO I=1,IIZ
            DO J=1,NBRIC
               NEDGE=0
               DO K=1,NBNEDGE
                  IF (INEDGE(6,K)/=I) CYCLE
                  IF (INEDGE(1,K)==1.AND.INEDGE(5,K)/=J) CYCLE
                  IF (INEDGE(1,K)==2.AND.INEDGE(4,K)/=J.AND.
     .                                     INEDGE(5,K)/=J) CYCLE 
                  NEDGE=NEDGE+1
                  LEDGE(NEDGE)=K
               ENDDO
               IF (NEDGE==0) CYCLE
               ALLOCATE(IPOLY(6+2*NEDGE+1+NEDGE,NEDGE), 
     .                  RPOLY(4+6*NEDGE+3*NEDGE,NEDGE),
     .                  IZ(3,NEDGE), IELNOD(NEDGE,NEDGE))
C               
               CALL HORIPOLY(INEDGE, RNEDGE, LEDGE,  NEDGE, IPOLY,
     .                       RPOLY,  IZ,     IELNOD, NNP,   VX3,
     .                       VY3,    VZ3,    I     , J    )
C
               DO N=1,NNP
                  IF (IPOLY(1,N)==-1) CYCLE
                  NPOLY=NPOLY+1
                  ALLOCATE(PTR_CUR)
                  PTR_PREC%PTR => PTR_CUR
                  NN=IPOLY(2,N)
                  NHOL=IPOLY(6+NN+1,N)
                  ALLOCATE(PTR_CUR%IPOLY(6+NN+1+NHOL), 
     .                     PTR_CUR%RPOLY(4+3*NN+3*NHOL),
     .                     PTR_CUR%IELNOD(NN))
                  PTR_CUR%NNSP=0
                  DO M=1,6
                     PTR_CUR%IPOLY(M)=IPOLY(M,N)
                  ENDDO
                  DO M=1,4
                     PTR_CUR%RPOLY(M)=RPOLY(M,N)
                  ENDDO
                  DO M=1,NN
                     PTR_CUR%IPOLY(6+M)=IPOLY(6+M,N)
                     PTR_CUR%IELNOD(M)=IELNOD(M,N)
                     PTR_CUR%RPOLY(4+3*(M-1)+1)=RPOLY(4+3*(M-1)+1,N)
                     PTR_CUR%RPOLY(4+3*(M-1)+2)=RPOLY(4+3*(M-1)+2,N)
                     PTR_CUR%RPOLY(4+3*(M-1)+3)=RPOLY(4+3*(M-1)+3,N)
                  ENDDO
                  NHOL=IPOLY(6+NN+1,N)
                  PTR_CUR%IPOLY(6+NN+1)=NHOL
                  DO M=1,NHOL
                     PTR_CUR%IPOLY(6+NN+1+M)=IPOLY(6+NN+1+M,N)
                     PTR_CUR%RPOLY(4+3*NN+3*(M-1)+1)=
     .                       RPOLY(4+3*NN+3*(M-1)+1,N)
                     PTR_CUR%RPOLY(4+3*NN+3*(M-1)+2)=
     .                       RPOLY(4+3*NN+3*(M-1)+2,N)
                     PTR_CUR%RPOLY(4+3*NN+3*(M-1)+3)=
     .                       RPOLY(4+3*NN+3*(M-1)+3,N)
                  ENDDO
                  DO M=1,3
                     PTR_CUR%IZ(M)=IZ(M,N)
                  ENDDO
                  PTR_PREC => PTR_CUR
               ENDDO
C
               DEALLOCATE(IPOLY, RPOLY, IZ, IELNOD)
            ENDDO
         ENDDO
         DEALLOCATE(LEDGE)
      ELSE
         PTR_CUR => FIRST
         DO I=1,NPOLY
            PTR_CUR%NNSP=0
            PTR_CUR%IZ(1)=1
            PTR_CUR%IZ(2)=1
            PTR_CUR => PTR_CUR%PTR
            IIZ=0
         ENDDO
      ENDIF
C---------------------------------------------------
C Construction des polyhedres
C---------------------------------------------------
      NPOLH=0
      IF (ILVOUT/=0) WRITE(ISTDO,'(A25,I10,A24)') 
     . ' ** MONITORED VOLUME ID: ',IVOLU(1),' - BUILDING POLYHEDRA **'
C     
      DO I=1,NBRIC
         IF (ILVOUT<0) CALL PROGBAR_C(I,NBRIC)
C
         IF (TBRIC(7,I)/=2) CYCLE
C Polygones de la brique
         NPOLB=0
         NPPMAX=0
         PTR_CUR => FIRST
         DO J=1,NPOLY
            ITY=PTR_CUR%IPOLY(1)
            IF ((ITY==1.AND.PTR_CUR%IPOLY(4)==I).OR.
     .          (ITY==2.AND.
     .          (PTR_CUR%IPOLY(3)==I.OR.PTR_CUR%IPOLY(4)==I))) THEN
               NPOLB=NPOLB+1
               NPPMAX=MAX(NPPMAX,PTR_CUR%IPOLY(2))
            ENDIF
            PTR_CUR => PTR_CUR%PTR
         ENDDO
         IF (NPOLB==0) CYCLE
C
         NRPMAX=4+3*NPPMAX
         ALLOCATE(POLB(NPOLB), IPOLY(6+NPPMAX,NPOLB),
     .            RPOLY(NRPMAX,NPOLB), REDIR(NPOLB))
         DO INZ=1,IIZ+1
            NPOLB=0
            PTR_CUR => FIRST
            DO J=1,NPOLY
               ITY=PTR_CUR%IPOLY(1)
               ITYZ=PTR_CUR%IZ(1)
               IF (((ITY==1.AND.PTR_CUR%IPOLY(4)==I).OR.
     .              (ITY==2.AND.(PTR_CUR%IPOLY(3)==I.OR.
     .                             PTR_CUR%IPOLY(4)==I)))
     .              .AND.
     .             ((ITYZ==1.AND.PTR_CUR%IZ(2)==INZ).OR.
     .              (ITYZ==2.AND.(PTR_CUR%IZ(2)==INZ.OR.
     .                              PTR_CUR%IZ(3)==INZ)))) THEN
                  NPOLB=NPOLB+1
                  REDIR(NPOLB)=J
                  NN=PTR_CUR%IPOLY(2)
                  DO K=1,6+NN
                     IPOLY(K,NPOLB)=PTR_CUR%IPOLY(K)
                  ENDDO
                  DO K=1,4+3*NN
                     RPOLY(K,NPOLB)=PTR_CUR%RPOLY(K)
                  ENDDO
                  POLB(NPOLB)=NPOLB
               ENDIF
               PTR_CUR => PTR_CUR%PTR
            ENDDO
            IF (NPOLB==0) CYCLE
C
            NPHMAX=NPOLB
            NPOLHMAX=NLAYER
  300       CONTINUE
            IF (ALLOCATED(POLH)) DEALLOCATE(POLH)
            ALLOCATE(POLH(NPHMAX+2,NPOLHMAX))
C
            CALL POLYHEDR(IPOLY, RPOLY   , POLB,   NPOLB,     POLH,
     .                    NNP,   NRPMAX  , NPHMAX, I,         DXM ,
     .                    INFO , NPOLHMAX, NPPMAX, RVOLU(50))
            IF (INFO==1) GOTO 300
C Modifications des polygones
            PTR_CUR => FIRST
            NPL=1
            POLC=REDIR(POLB(NPL))
            DO J=1,NPOLY
               IF (J==POLC) THEN
                  ITY=PTR_CUR%IPOLY(1)
                  IF (ITY==1) THEN
                     PTR_CUR%IPOLY(5)=NPOLH+IPOLY(5,NPL)
                  ELSEIF (ITY==2) THEN
                     IF (PTR_CUR%IPOLY(5)==0) THEN
                        PTR_CUR%IPOLY(5)=NPOLH+IPOLY(5,NPL)
                     ELSE
                        PTR_CUR%IPOLY(6)=NPOLH+IPOLY(6,NPL)
                     ENDIF
                  ENDIF
                  IF (NPL==NPOLB) GOTO 350
                  NPL=NPL+1
                  POLC=REDIR(POLB(NPL))
               ENDIF
               PTR_CUR => PTR_CUR%PTR
            ENDDO
  350       CONTINUE
C
            DO N=1,NNP
               NPOLH=NPOLH+1
               IF (.NOT.ASSOCIATED(PHFIRST)) THEN
                  ALLOCATE(PHFIRST)
                  PH_CUR => PHFIRST
               ELSE
                  ALLOCATE(PH_CUR)
                  PH_PREC%PTR => PH_CUR
               ENDIF
               NN=POLH(1,N)
               ALLOCATE(PH_CUR%POLH(2+NN))
               PH_CUR%RANK=NPOLH
               PH_CUR%POLH(1)=POLH(1,N)
               PH_CUR%POLH(2)=POLH(2,N)
               DO M=1,NN
                  PH_CUR%POLH(2+M)=REDIR(POLH(2+M,N))
               ENDDO
               PH_PREC => PH_CUR
            ENDDO
C
         ENDDO
         DEALLOCATE(IPOLY, RPOLY, POLB, POLH, REDIR)
      ENDDO
C---------------------------------------------------
C Passage listes chainees -> tableaux classiques pour la suite des traitements
C---------------------------------------------------
  370 CONTINUE     
C      
      PTR_CUR => FIRST
      LENIMAX=0
      LENRMAX=0
      NNS=0
      NNS2=0
      DO I=1,NPOLY
         NN=PTR_CUR%IPOLY(2)
         NNS=NNS+NN
         NNS2=NNS2+PTR_CUR%NNSP
         IF (PTR_CUR%IPOLY(1)==1) THEN
            LENIMAX=MAX(LENIMAX,6+NN+1)
            LENRMAX=MAX(LENRMAX,4+3*NN)
         ELSEIF (PTR_CUR%IPOLY(1)==2) THEN
            NHOL=PTR_CUR%IPOLY(6+NN+1)
            LENIMAX=MAX(LENIMAX,6+NN+1+NHOL)
            LENRMAX=MAX(LENRMAX,4+3*NN+3*NHOL)
         ENDIF
         PTR_CUR => PTR_CUR%PTR
      ENDDO
      PH_CUR => PHFIRST
      NPHMAX=0
      DO I=1,NPOLH
         NN=PH_CUR%POLH(1)
         NPHMAX=MAX(NPHMAX,NN)
         PH_CUR => PH_CUR%PTR
      ENDDO
C
      NPOLY_OLD=NPOLY
      NPOLH_OLD=NPOLH
      IF (NBA>0) THEN
c         IF (NSPMD == 1) THEN
c            ALLOCATE(ITAGN(NUMNOD))            
c            NPOLH=NPOLH+NBA
c            DO I=1,NBA
c               ITAGBA(I)=0
c            ENDDO
cC
c            DO I=1,NUMNOD
c               ITAGN(I)=0
c            ENDDO
c            DO IEL=1,NEL
c               N1=IBUF(ELEM(1,IEL))
c               N2=IBUF(ELEM(2,IEL))
c               N3=IBUF(ELEM(3,IEL))
c               ITAGN(N1)=1
c               ITAGN(N2)=1
c               ITAGN(N3)=1
c            ENDDO
cC
c            DO I=1,NBA
c               NFAC=TBA(2,I)
c               NNP=0
c               DO J=1,NFAC
c                  ITY=TFACA(2*(J-1)+1,I)
c                  IF (ITY==1) THEN
cC Face interne brique/brique
c                     NV=TFACA(2*(J-1)+2,I)
c                     IF (ITAGBA(NV)==0) THEN
c                        LENIMAX=MAX(LENIMAX,6+3+1)
c                        LENRMAX=MAX(LENRMAX,6+3*3)
c                        IF (NFAC==4) THEN
c                           NPOLY=NPOLY+1
c                           NNS=NNS+3
c                        ELSEIF (NFAC==6) THEN
c                           NPOLY=NPOLY+2
c                           NNS=NNS+6
c                        ENDIF
c                     ENDIF
c                     IF (NFAC==4) THEN
c                        NNP=NNP+1
c                     ELSEIF (NFAC==6) THEN
c                        NNP=NNP+2
c                     ENDIF
c                  ELSEIF (ITY==2) THEN
cC Face brique appuyee sur surface
c                     IF (NFAC==4) THEN
c                        NPOLY=NPOLY+1
c                        NNS=NNS+3
c                        NNP=NNP+1
c                     ELSEIF (NFAC==6) THEN
c                        NPOLY=NPOLY+2
c                        NNS=NNS+6
c                        NNP=NNP+2
c                     ENDIF
c                  ELSEIF (ITY==3) THEN
cC Face interne brique/polyhedres
c                     PTR_CUR => FIRST
c                     DO K=1,NPOLY_OLD
c                        ITY=PTR_CUR%IPOLY(1)
c                        IF (ITY==1) THEN
c                           IEL=PTR_CUR%IPOLY(3)
c                           IF (-TAGELA(IEL)==I) NNP=NNP+1
c                        ENDIF
c                        PTR_CUR => PTR_CUR%PTR
c                     ENDDO
c                  ENDIF
c               ENDDO
c               ITAGBA(I)=1
c               NPHMAX=MAX(NPHMAX,NNP)
cC
c               II=TBA(1,I)
c               NALL=1
c               IF (NFAC==4) THEN
c                  DO J=1,4
c                     JJ=IXS(1+2*(J-1)+1,II)
c                     NALL=NALL*ITAGN(JJ)
c                  ENDDO
c               ELSEIF (NFAC==6) THEN
c                  DO J=1,8
c                     JJ=IXS(1+J,II)
c                     NALL=NALL*ITAGN(JJ)
c                  ENDDO
c               ENDIF
c               IBSA(I)=NALL
c            ENDDO
c            DEALLOCATE(ITAGN)
c         ELSE
            ALLOCATE(ITAGN(NNSA))
            NPOLH=NPOLH+NBA
            DO I=1,NBA
               ITAGBA(I)=0
            ENDDO
C
            DO I=1,NNSA
               ITAGN(I)=0
            ENDDO
            DO IEL=1,NEL
               N1=FVSPMD(IFV)%ELEMSA(1,IEL)
               N2=FVSPMD(IFV)%ELEMSA(2,IEL)
               N3=FVSPMD(IFV)%ELEMSA(3,IEL)
               IF (N1>0) ITAGN(N1)=1
               IF (N1>0) ITAGN(N2)=1
               IF (N1>0) ITAGN(N3)=1
            ENDDO
C
            DO I=1,NBA
               NFAC=TBA(2,I)
               NNP=0
               DO J=1,NFAC
                  ITY=TFACA(2*(J-1)+1,I)
                  IF (ITY==1) THEN
C Face interne brique/brique
                     NV=TFACA(2*(J-1)+2,I)
                     IF (ITAGBA(NV)==0) THEN
                        LENIMAX=MAX(LENIMAX,6+3+1)
                        LENRMAX=MAX(LENRMAX,6+3*3)
                        IF (NFAC==4) THEN
                           NPOLY=NPOLY+1
                           NNS=NNS+3
                        ELSEIF (NFAC==6) THEN
                           NPOLY=NPOLY+2
                           NNS=NNS+6
                        ENDIF
                     ENDIF
                     IF (NFAC==4) THEN
                        NNP=NNP+1
                     ELSEIF (NFAC==6) THEN
                        NNP=NNP+2
                     ENDIF
                  ELSEIF (ITY==2) THEN
C Face brique appuyee sur surface
                     IF (NFAC==4) THEN
                        NPOLY=NPOLY+1
                        NNS=NNS+3
                        NNP=NNP+1
                     ELSEIF (NFAC==6) THEN
                        NPOLY=NPOLY+2
                        NNS=NNS+6
                        NNP=NNP+2
                     ENDIF
                  ELSEIF (ITY==3) THEN
C Face interne brique/polyhedres
                     PTR_CUR => FIRST
                     DO K=1,NPOLY_OLD
                        ITY=PTR_CUR%IPOLY(1)
                        IF (ITY==1) THEN
                           IEL=PTR_CUR%IPOLY(3)
                           IF (-TAGELA(IEL)==I) NNP=NNP+1
                        ENDIF
                        PTR_CUR => PTR_CUR%PTR
                     ENDDO
                  ENDIF
               ENDDO
               ITAGBA(I)=1
               NPHMAX=MAX(NPHMAX,NNP)
C
               NALL=1
               IF (NFAC==4) THEN
                  DO J=1,4
                     JJ=FVSPMD(IFV)%IXSA(2*(J-1)+1,I)
                     NALL=NALL*ITAGN(JJ)
                  ENDDO
               ELSEIF (NFAC==6) THEN
                  DO J=1,8
                     JJ=FVSPMD(IFV)%IXSA(J,I)
                     NALL=NALL*ITAGN(JJ)
                  ENDDO
               ENDIF
               IBSA(I)=NALL
            ENDDO
c         ENDIF
      ENDIF
C
      ALLOCATE(IPOLY_F(LENIMAX,NPOLY), RPOLY_F(LENRMAX,NPOLY),
     .         POLH_F(2+NPHMAX,NPOLH), IFVNOD(NNS), VOLU(NPOLH),
     .         IFVNOD2(2,NNS2), RFVNOD2(4,NNS2), XNS(3,NNS), 
     .         XNS2(3,NNS2))
C
      NPOLY=NPOLY_OLD
      NPOLH=NPOLH_OLD
      PTR_CUR => FIRST
      NNS=0
      DO I=1,NPOLY
         NN=PTR_CUR%IPOLY(2)
         DO J=1,NN
            JJ=PTR_CUR%IPOLY(6+J)
            IF (JJ>0) THEN
               NNS=NNS+1
               XNS(1,NNS)=PTR_CUR%RPOLY(4+3*(J-1)+1)
               XNS(2,NNS)=PTR_CUR%RPOLY(4+3*(J-1)+2)
               XNS(3,NNS)=PTR_CUR%RPOLY(4+3*(J-1)+3)
            ENDIF
         ENDDO
         PTR_CUR => PTR_CUR%PTR
      ENDDO
C
      NNS=0
      PTR_CUR => FIRST
      DO I=1,NPOLY
         NN=PTR_CUR%IPOLY(2)
         DO J=1,6
            IPOLY_F(J,I)=PTR_CUR%IPOLY(J)
         ENDDO
         DO J=1,4+3*NN
            RPOLY_F(J,I)=PTR_CUR%RPOLY(J)
         ENDDO
         DO J=1,NN
            JJ=PTR_CUR%IPOLY(6+J)
            IF (JJ>0) THEN
               NNS=NNS+1
               IPOLY_F(6+J,I)=NNS
               IFVNOD(NNS)=PTR_CUR%IELNOD(J)
            ELSE
               IPOLY_F(6+J,I)=JJ
            ENDIF
         ENDDO
         IF (PTR_CUR%IPOLY(1)==1) THEN
            IPOLY_F(4,I)=IPOLY_F(5,I)
            IPOLY_F(5,I)=0
         ELSEIF (PTR_CUR%IPOLY(1)==2) THEN
            NHOL=PTR_CUR%IPOLY(6+NN+1)
            IPOLY_F(6+NN+1,I)=NHOL
            DO J=1,NHOL
               IPOLY_F(6+NN+1+J,I)=PTR_CUR%IPOLY(6+NN+1+J)
               RPOLY_F(4+3*NN+3*(J-1)+1,I)=
     .                    PTR_CUR%RPOLY(4+3*NN+3*(J-1)+1)
               RPOLY_F(4+3*NN+3*(J-1)+2,I)=
     .                    PTR_CUR%RPOLY(4+3*NN+3*(J-1)+2)
               RPOLY_F(4+3*NN+3*(J-1)+3,I)=
     .                    PTR_CUR%RPOLY(4+3*NN+3*(J-1)+3)
            ENDDO
         ENDIF
         NNSP=PTR_CUR%NNSP
         IF (NNSP>0) THEN
            DO J=1,NNSP
               JJ=PTR_CUR%NREF(1,J)
               IFVNOD2(1,JJ)=PTR_CUR%NREF(2,J)
               IFVNOD2(2,JJ)=PTR_CUR%NREF(3,J)
               RFVNOD2(1,JJ)=PTR_CUR%AREF(1,J)
               RFVNOD2(2,JJ)=PTR_CUR%AREF(2,J)
               RFVNOD2(3,JJ)=PTR_CUR%AREF(3,J)
               RFVNOD2(4,JJ)=PTR_CUR%AREF(4,J)
            ENDDO
         ENDIF
         PTR_TMP => PTR_CUR%PTR
         DEALLOCATE(PTR_CUR%IPOLY, PTR_CUR%RPOLY, PTR_CUR%IELNOD)
         IF (NNSP>0) DEALLOCATE(PTR_CUR%NREF, PTR_CUR%AREF)
         DEALLOCATE(PTR_CUR)
         PTR_CUR => PTR_TMP
      ENDDO
C
      DO I=1,NNS2
         N1=IFVNOD2(1,I)
         N2=IFVNOD2(2,I)
         IDEP=0
         IF (N1<0) THEN
            II=-N1
            IF (IFVNOD2(1,II)/=N2.AND.IFVNOD2(2,II)/=N2) THEN
               WRITE(ISTDO,*) 'PROBLEM DEPENDANT NODE ',I
               CALL ARRET(2)
            ENDIF
            N1=IFVNOD2(1,II)
            N2=IFVNOD2(2,II)
            IDEP=1
         ELSEIF (N2<0) THEN
            II=-N2
            IF (IFVNOD2(1,II)/=N1.AND.IFVNOD2(2,II)/=N1) THEN
               WRITE(ISTDO,*) 'PROBLEM DEPENDANT NODE ',I
               CALL ARRET(2)
            ENDIF
            N1=IFVNOD2(1,II)
            N2=IFVNOD2(2,II)
            IDEP=1
         ENDIF
         IFVNOD2(1,I)=N1
         IFVNOD2(2,I)=N2
         IF (IDEP==1) THEN
C Recalcul des coordonnees barycentriques du point dependant
            II=1
            VAL=ABS(XNS(1,N1)-XNS(1,N2))
            DO J=2,3
               IF (ABS(XNS(J,N1)-XNS(J,N2))>VAL) THEN
                  II=J
                  VAL=ABS(XNS(J,N1)-XNS(J,N2))
               ENDIF
            ENDDO
            RFVNOD2(1,I)=(RFVNOD2(II+1,I)-XNS(II,N2))/
     .                   (XNS(II,N1)-XNS(II,N2))
         ENDIF
      ENDDO
C
      PH_CUR => PHFIRST
      DO I=1,NPOLH
         NN=PH_CUR%POLH(1)
         DO J=1,2+NN
            POLH_F(J,I)=PH_CUR%POLH(J)
         ENDDO
         PH_TMP => PH_CUR%PTR
         DEALLOCATE(PH_CUR%POLH)
         DEALLOCATE(PH_CUR)
         PH_CUR => PH_TMP
      ENDDO
C
      IF (NBA>0) THEN
         DO I=1,NBA
            ITAGBA(I)=0
         ENDDO
C
         NPOLY_OLD=NPOLY
         DO I=1,NBA
            II=TBA(1,I)
            NFAC=TBA(2,I)
            NNP=0
            DO J=1,NFAC
               ITY=TFACA(2*(J-1)+1,I)
               IF (ITY==1) THEN
C Face interne brique/brique
                  NV=TFACA(2*(J-1)+2,I)
                  IF (ITAGBA(NV)==0) THEN
                     IF (NFAC==4) THEN
                        NPOLY=NPOLY+1
                        N1=FAC4(1,J)
                        N2=FAC4(2,J)
                        N3=FAC4(3,J)
                        IPOLY_F(1,NPOLY)=2
                        IPOLY_F(2,NPOLY)=3
                        IPOLY_F(3,NPOLY)=0
                        IPOLY_F(4,NPOLY)=0
                        IPOLY_F(5,NPOLY)=NPOLH+I
                        IPOLY_F(6,NPOLY)=NPOLH+NV
                        IPOLY_F(6+1,NPOLY)=NNS+1
                        IPOLY_F(6+2,NPOLY)=NNS+2
                        IPOLY_F(6+3,NPOLY)=NNS+3
                        IPOLY_F(6+3+1,NPOLY)=0
C
c                        IF (NSPMD == 1) THEN
c                           NN1=IXS(1+N1,II)
c                           NN2=IXS(1+N2,II)
c                           NN3=IXS(1+N3,II)
c                           IFVNOD(NNS+1)=-NN1
c                           IFVNOD(NNS+2)=-NN2
c                           IFVNOD(NNS+3)=-NN3
c                           NNS=NNS+3
c                           X1=X(1,NN1)
c                           Y1=X(2,NN1)
c                           Z1=X(3,NN1)
c                           X2=X(1,NN2)
c                           Y2=X(2,NN2)
c                           Z2=X(3,NN2)
c                           X3=X(1,NN3)
c                           Y3=X(2,NN3)
c                           Z3=X(3,NN3)
c                        ELSE
                           NN1=FVSPMD(IFV)%IXSA(N1,I)
                           NN2=FVSPMD(IFV)%IXSA(N2,I)
                           NN3=FVSPMD(IFV)%IXSA(N3,I)
                           IFVNOD(NNS+1)=-NN1
                           IFVNOD(NNS+2)=-NN2
                           IFVNOD(NNS+3)=-NN3
                           NNS=NNS+3
                           X1=XXXSA(1,NN1)
                           Y1=XXXSA(2,NN1)
                           Z1=XXXSA(3,NN1)
                           X2=XXXSA(1,NN2)
                           Y2=XXXSA(2,NN2)
                           Z2=XXXSA(3,NN2)
                           X3=XXXSA(1,NN3)
                           Y3=XXXSA(2,NN3)
                           Z3=XXXSA(3,NN3)
c                        ENDIF
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
                        RPOLY_F(2,NPOLY)=NRX/AREA2
                        RPOLY_F(3,NPOLY)=NRY/AREA2
                        RPOLY_F(4,NPOLY)=NRZ/AREA2
                        RPOLY_F(4+1,NPOLY)=X1
                        RPOLY_F(4+2,NPOLY)=Y1
                        RPOLY_F(4+3,NPOLY)=Z1
                        RPOLY_F(4+4,NPOLY)=X2
                        RPOLY_F(4+5,NPOLY)=Y2
                        RPOLY_F(4+6,NPOLY)=Z2
                        RPOLY_F(4+7,NPOLY)=X3
                        RPOLY_F(4+8,NPOLY)=Y3
                        RPOLY_F(4+9,NPOLY)=Z3
C
                        NNP=NNP+1
                        POLH_F(2+NNP,NPOLH+I)=NPOLY
                     ELSEIF (NFAC==6) THEN
                        N1=FAC(1,J)
                        N2=FAC(2,J)
                        N3=FAC(3,J)
                        N4=FAC(4,J)
c                        IF (NSPMD == 1) THEN
c                           NN1=IXS(1+N1,II)
c                           NN2=IXS(1+N2,II)
c                           NN3=IXS(1+N3,II)
c                           NN4=IXS(1+N4,II)
c                           X1=X(1,NN1)
c                           Y1=X(2,NN1)
c                           Z1=X(3,NN1)
c                           X2=X(1,NN2)
c                           Y2=X(2,NN2)
c                           Z2=X(3,NN2)
c                           X3=X(1,NN3)
c                           Y3=X(2,NN3)
c                           Z3=X(3,NN3)
c                           X4=X(1,NN4)
c                           Y4=X(2,NN4)
c                           Z4=X(3,NN4)
c                        ELSE
                           NN1=FVSPMD(IFV)%IXSA(N1,I)
                           NN2=FVSPMD(IFV)%IXSA(N2,I)
                           NN3=FVSPMD(IFV)%IXSA(N3,I)
                           NN4=FVSPMD(IFV)%IXSA(N4,I)
                           X1=XXXSA(1,NN1)
                           Y1=XXXSA(2,NN1)
                           Z1=XXXSA(3,NN1)
                           X2=XXXSA(1,NN2)
                           Y2=XXXSA(2,NN2)
                           Z2=XXXSA(3,NN2)
                           X3=XXXSA(1,NN3)
                           Y3=XXXSA(2,NN3)
                           Z3=XXXSA(3,NN3)
                           X4=XXXSA(1,NN4)
                           Y4=XXXSA(2,NN4)
                           Z4=XXXSA(3,NN4)
c                        ENDIF
C
                        NPOLY=NPOLY+1
                        IPOLY_F(1,NPOLY)=2
                        IPOLY_F(2,NPOLY)=3
                        IPOLY_F(3,NPOLY)=0
                        IPOLY_F(4,NPOLY)=0
                        IPOLY_F(5,NPOLY)=NPOLH+I
                        IPOLY_F(6,NPOLY)=NPOLH+NV
                        IPOLY_F(6+1,NPOLY)=NNS+1
                        IPOLY_F(6+2,NPOLY)=NNS+2
                        IPOLY_F(6+3,NPOLY)=NNS+3
                        IPOLY_F(6+3+1,NPOLY)=0
                        IFVNOD(NNS+1)=-NN1
                        IFVNOD(NNS+2)=-NN2
                        IFVNOD(NNS+3)=-NN3
                        NNS=NNS+3
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
                        RPOLY_F(2,NPOLY)=NRX/AREA2
                        RPOLY_F(3,NPOLY)=NRY/AREA2
                        RPOLY_F(4,NPOLY)=NRZ/AREA2
                        RPOLY_F(4+1,NPOLY)=X1
                        RPOLY_F(4+2,NPOLY)=Y1
                        RPOLY_F(4+3,NPOLY)=Z1
                        RPOLY_F(4+4,NPOLY)=X2
                        RPOLY_F(4+5,NPOLY)=Y2
                        RPOLY_F(4+6,NPOLY)=Z2
                        RPOLY_F(4+7,NPOLY)=X3
                        RPOLY_F(4+8,NPOLY)=Y3
                        RPOLY_F(4+9,NPOLY)=Z3
C
                        NNP=NNP+1
                        POLH_F(2+NNP,NPOLH+I)=NPOLY
C
                        NPOLY=NPOLY+1
                        IPOLY_F(1,NPOLY)=2
                        IPOLY_F(2,NPOLY)=3
                        IPOLY_F(3,NPOLY)=0
                        IPOLY_F(4,NPOLY)=0
                        IPOLY_F(5,NPOLY)=NPOLH+I
                        IPOLY_F(6,NPOLY)=NPOLH+NV
                        IPOLY_F(6+1,NPOLY)=NNS+1
                        IPOLY_F(6+2,NPOLY)=NNS+2
                        IPOLY_F(6+3,NPOLY)=NNS+3
                        IPOLY_F(6+3+1,NPOLY)=0
                        IFVNOD(NNS+1)=-NN1
                        IFVNOD(NNS+2)=-NN3
                        IFVNOD(NNS+3)=-NN4
                        NNS=NNS+3
                        X13=X3-X1
                        Y13=Y3-Y1
                        Z13=Z3-Z1
                        X14=X4-X1
                        Y14=Y4-Y1
                        Z14=Z4-Z1
                        NRX=Y13*Z14-Z13*Y14
                        NRY=Z13*X14-X13*Z14
                        NRZ=X13*Y14-Y13*X14
                        AREA2=SQRT(NRX**2+NRY**2+NRZ**2)
                        RPOLY_F(2,NPOLY)=NRX/AREA2
                        RPOLY_F(3,NPOLY)=NRY/AREA2
                        RPOLY_F(4,NPOLY)=NRZ/AREA2
                        RPOLY_F(4+1,NPOLY)=X1
                        RPOLY_F(4+2,NPOLY)=Y1
                        RPOLY_F(4+3,NPOLY)=Z1
                        RPOLY_F(4+4,NPOLY)=X3
                        RPOLY_F(4+5,NPOLY)=Y3
                        RPOLY_F(4+6,NPOLY)=Z3
                        RPOLY_F(4+7,NPOLY)=X4
                        RPOLY_F(4+8,NPOLY)=Y4
                        RPOLY_F(4+9,NPOLY)=Z4
C
                        NNP=NNP+1
                        POLH_F(2+NNP,NPOLH+I)=NPOLY
                     ENDIF
                  ELSE
                     DO K=1,POLH_F(1,NPOLH+NV)
                        KK=POLH_F(2+K,NPOLH+NV)
                        IF (IPOLY_F(1,KK)==2.AND.
     .                      IPOLY_F(6,KK)==NPOLH+I) THEN
                           NNP=NNP+1
                           POLH_F(2+NNP,NPOLH+I)=KK
                        ENDIF
                     ENDDO
                  ENDIF
               ELSEIF (ITY==2) THEN        
C Face brique appuyee sur surface (traitee plus loin)
               ELSEIF (ITY==3) THEN
C Face interne brique/polyhedres
                  DO K=1,NPOLY_OLD
                     ITY=IPOLY_F(1,K)
                     IF (ITY==1) THEN
                        IEL=IPOLY_F(3,K)
                        IF (-TAGELA(IEL)==I) THEN
                           NNP=NNP+1
                           POLH_F(2+NNP,NPOLH+I)=K
                           IPOLY_F(1,K)=2
                           IPOLY_F(3,K)=0
                           IP=IPOLY_F(4,K)
                           IPOLY_F(4,K)=0
                           IPOLY_F(5,K)=IP
                           IPOLY_F(6,K)=NPOLH+I
                        ENDIF
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
            POLH_F(1,NPOLH+I)=NNP
            POLH_F(2,NPOLH+I)=-I
            ITAGBA(I)=1
         ENDDO
C
         NPOLH=NPOLH+NBA
C
         DO I=1,NPOLY_OLD
            IF (IPOLY_F(1,I)==1) THEN
               IEL=IPOLY_F(3,I)
               IF (TAGELA(IEL)>0) IPOLY_F(3,I)=TAGELA(IEL)
            ENDIF
         ENDDO
C
         DO IEL=1,NEL
            IF (TAGELS(IEL)>0) THEN
               NPOLY=NPOLY+1
               IPOLY_F(1,NPOLY)=1
               IPOLY_F(2,NPOLY)=3
               IPOLY_F(3,NPOLY)=IEL
               IPOLY_F(4,NPOLY)=NPOLH_OLD+TAGELS(IEL)
               IPOLY_F(5,NPOLY)=0
               IPOLY_F(6,NPOLY)=0
               IPOLY_F(7,NPOLY)=NNS+1
               IPOLY_F(8,NPOLY)=NNS+2
               IPOLY_F(9,NPOLY)=NNS+3
c               IF (NSPMD == 1) THEN
c                  N1=ELEM(1,IEL)
c                  N2=ELEM(2,IEL)
c                  N3=ELEM(3,IEL)
c                  NN1=IBUF(N1)
c                  NN2=IBUF(N2)
c                  NN3=IBUF(N3)
c                  IFVNOD(NNS+1)=-NN1
c                  IFVNOD(NNS+2)=-NN2
c                  IFVNOD(NNS+3)=-NN3
c                  NNS=NNS+3
c                  RPOLY_F(1,NPOLY)=ELAREA(IEL)
c                  RPOLY_F(2,NPOLY)=NORM(1,IEL)
c                  RPOLY_F(3,NPOLY)=NORM(2,IEL)
c                  RPOLY_F(4,NPOLY)=NORM(3,IEL)
c                  X1=X(1,NN1)
c                  Y1=X(2,NN1)
c                  Z1=X(3,NN1)
c                  X2=X(1,NN2)
c                  Y2=X(2,NN2)
c                  Z2=X(3,NN2)
c                  X3=X(1,NN3)
c                  Y3=X(2,NN3)
c                  Z3=X(3,NN3)
c               ELSE
                  NN1=FVSPMD(IFV)%ELEMSA(1,IEL)
                  NN2=FVSPMD(IFV)%ELEMSA(2,IEL)
                  NN3=FVSPMD(IFV)%ELEMSA(3,IEL)
                  IFVNOD(NNS+1)=-NN1
                  IFVNOD(NNS+2)=-NN2
                  IFVNOD(NNS+3)=-NN3
                  NNS=NNS+3
                  RPOLY_F(1,NPOLY)=ELAREA(IEL)
                  RPOLY_F(2,NPOLY)=NORM(1,IEL)
                  RPOLY_F(3,NPOLY)=NORM(2,IEL)
                  RPOLY_F(4,NPOLY)=NORM(3,IEL)
                  X1=XXXSA(1,NN1)
                  Y1=XXXSA(2,NN1)
                  Z1=XXXSA(3,NN1)
                  X2=XXXSA(1,NN2)
                  Y2=XXXSA(2,NN2)
                  Z2=XXXSA(3,NN2)
                  X3=XXXSA(1,NN3)
                  Y3=XXXSA(2,NN3)
                  Z3=XXXSA(3,NN3)
c               ENDIF
               RPOLY_F(5,NPOLY)=X1
               RPOLY_F(6,NPOLY)=Y1
               RPOLY_F(7,NPOLY)=Z1
               RPOLY_F(8,NPOLY)=X2
               RPOLY_F(9,NPOLY)=Y2
               RPOLY_F(10,NPOLY)=Z2
               RPOLY_F(11,NPOLY)=X3
               RPOLY_F(12,NPOLY)=Y3
               RPOLY_F(13,NPOLY)=Z3
C
               NNP=POLH_F(1,NPOLH_OLD+TAGELS(IEL))
               NNP=NNP+1
               POLH_F(1,NPOLH_OLD+TAGELS(IEL))=NNP
               POLH_F(2+NNP,NPOLH_OLD+TAGELS(IEL))=NPOLY
            ENDIF
         ENDDO
      ENDIF   
C---------------------------------------------------
C Surface des polygones
C---------------------------------------------------
      DO I=1,NPOLY
         ITY=IPOLY_F(1,I)
         NN=IPOLY_F(2,I)
         NHOL=0
         IF (ITY==2) NHOL=IPOLY_F(6+NN+1,I)
         AREA=ZERO
         NX=RPOLY_F(2,I)
         NY=RPOLY_F(3,I)
         NZ=RPOLY_F(4,I)
         IF (NHOL==0) THEN
            X1=RPOLY_F(5,I)
            Y1=RPOLY_F(6,I)
            Z1=RPOLY_F(7,I)
            DO J=1,NN-2
               JJ=J+1
               JJJ=JJ+1
               X2=RPOLY_F(4+3*(JJ-1)+1,I)
               Y2=RPOLY_F(4+3*(JJ-1)+2,I)
               Z2=RPOLY_F(4+3*(JJ-1)+3,I)
               X3=RPOLY_F(4+3*(JJJ-1)+1,I)
               Y3=RPOLY_F(4+3*(JJJ-1)+2,I)
               Z3=RPOLY_F(4+3*(JJJ-1)+3,I)
               X12=X2-X1
               Y12=Y2-Y1
               Z12=Z2-Z1
               X13=X3-X1
               Y13=Y3-Y1
               Z13=Z3-Z1
               NNX=Y12*Z13-Z12*Y13
               NNY=Z12*X13-X12*Z13
               NNZ=X12*Y13-Y12*X13
               AREA=AREA+(NNX*NX+NNY*NY+NNZ*NZ)
            ENDDO
         ELSE
            ALLOCATE(ADR(NHOL))
            DO J=1,NHOL
               ADR(J)=IPOLY_F(6+NN+1+J,I)
            ENDDO
            ADR(NHOL+1)=NN
            X1=RPOLY_F(5,I)
            Y1=RPOLY_F(6,I)
            Z1=RPOLY_F(7,I)
            DO J=1,ADR(1)-2
               JJ=J+1
               JJJ=JJ+1
               X2=RPOLY_F(4+3*(JJ-1)+1,I)
               Y2=RPOLY_F(4+3*(JJ-1)+2,I)
               Z2=RPOLY_F(4+3*(JJ-1)+3,I)
               X3=RPOLY_F(4+3*(JJJ-1)+1,I)
               Y3=RPOLY_F(4+3*(JJJ-1)+2,I)
               Z3=RPOLY_F(4+3*(JJJ-1)+3,I)
               X12=X2-X1
               Y12=Y2-Y1
               Z12=Z2-Z1
               X13=X3-X1
               Y13=Y3-Y1
               Z13=Z3-Z1
               NNX=Y12*Z13-Z12*Y13
               NNY=Z12*X13-X12*Z13
               NNZ=X12*Y13-Y12*X13
               AREA=AREA+(NNX*NX+NNY*NY+NNZ*NZ)
            ENDDO
            AREA=ABS(AREA)
C On retranche la surface des trous
            DO J=1,NHOL
               X1=RPOLY_F(4+3*ADR(J)+1,I)
               Y1=RPOLY_F(4+3*ADR(J)+2,I)
               Z1=RPOLY_F(4+3*ADR(J)+3,I)
               AREA2=ZERO
               DO K=ADR(J)+1,ADR(J+1)-2
                  KK=K+1
                  KKK=KK+1
                  X2=RPOLY_F(4+3*(KK-1)+1,I)
                  Y2=RPOLY_F(4+3*(KK-1)+2,I)
                  Z2=RPOLY_F(4+3*(KK-1)+3,I)
                  X3=RPOLY_F(4+3*(KKK-1)+1,I)
                  Y3=RPOLY_F(4+3*(KKK-1)+2,I)
                  Z3=RPOLY_F(4+3*(KKK-1)+3,I)
                  X12=X2-X1
                  Y12=Y2-Y1
                  Z12=Z2-Z1
                  X13=X3-X1
                  Y13=Y3-Y1
                  Z13=Z3-Z1
                  NNX=Y12*Z13-Z12*Y13
                  NNY=Z12*X13-X12*Z13
                  NNZ=X12*Y13-Y12*X13
                  AREA2=AREA2+(NNX*NX+NNY*NY+NNZ*NZ)
               ENDDO
               AREA=AREA-ABS(AREA2)
            ENDDO
            DEALLOCATE(ADR)
         ENDIF
         RPOLY_F(1,I)=HALF*ABS(AREA)
      ENDDO
C---------------------------------------------------
C Volume des polyhedres
C---------------------------------------------------
      DO I=1,NPOLH
         VOLU(I)=ZERO
         DO J=1,POLH_F(1,I)
            JJ=POLH_F(2+J,I)
            AREA=RPOLY_F(1,JJ)
            ITY=IPOLY_F(1,JJ)
            IF (ITY==1) THEN
               NX=RPOLY_F(2,JJ)
               NY=RPOLY_F(3,JJ)
               NZ=RPOLY_F(4,JJ)
            ELSEIF (ITY==2) THEN
               IF (IPOLY_F(5,JJ)==I) THEN
                  NX=RPOLY_F(2,JJ)
                  NY=RPOLY_F(3,JJ)
                  NZ=RPOLY_F(4,JJ)
               ELSEIF (IPOLY_F(6,JJ)==I) THEN
                  NX=-RPOLY_F(2,JJ)
                  NY=-RPOLY_F(3,JJ)
                  NZ=-RPOLY_F(4,JJ)
               ENDIF
            ENDIF
            X1=RPOLY_F(5,JJ)
            Y1=RPOLY_F(6,JJ)
            Z1=RPOLY_F(7,JJ)
            VOLU(I)=VOLU(I)+THIRD*AREA*(X1*NX+Y1*NY+Z1*NZ)
         ENDDO
      ENDDO
C---------------------------------------------------
C Elimination des segments de longueur nulle dans les polygones
C---------------------------------------------------
      NNS_OLD=NNS
      ALLOCATE(IFVNOD_OLD(NNS_OLD), REDIR(NNS_OLD))
      DO I=1,NNS_OLD
         IFVNOD_OLD(I)=IFVNOD(I)
      ENDDO
      NNS_OLD=0
      NNS=0
C
      TOLE=RVOLU(50)
      DO I=1,NPOLY
         NNP_OLD=IPOLY_F(2,I)
C
         LMAX2=ZERO
         X0=RPOLY_F(5,I)
         Y0=RPOLY_F(6,I)
         Z0=RPOLY_F(7,I)
         DO J=2,NNP_OLD
            X1=RPOLY_F(4+3*(J-1)+1,I)
            Y1=RPOLY_F(4+3*(J-1)+2,I)
            Z1=RPOLY_F(4+3*(J-1)+3,I)
            LMAX2=MAX(LMAX2,(X1-X0)**2+(Y1-Y0)**2+(Z1-Z0)**2)
            X0=X1
            Y0=Y1
            Z0=Z1
         ENDDO
         X1=RPOLY_F(5,I)
         Y1=RPOLY_F(6,I)
         Z1=RPOLY_F(7,I)
         LMAX2=MAX(LMAX2,(X1-X0)**2+(Y1-Y0)**2+(Z1-Z0)**2)
         TOLE2=TOLE**2*LMAX2
C
         NHOL=0
         IF (IPOLY_F(1,I)==2) NHOL=IPOLY_F(6+NNP_OLD+1,I)
         ALLOCATE(IPOLY_OLD(NNP_OLD), RPOLY_OLD(3*NNP_OLD),
     .            ADR_OLD(NHOL+2), ADR(NHOL+2))
         DO J=1,NNP_OLD
            IPOLY_OLD(J)=IPOLY_F(6+J,I)
         ENDDO
         DO J=1,3*NNP_OLD
            RPOLY_OLD(J)=RPOLY_F(4+J,I)
         ENDDO
         NNP=0
         IF (NHOL==0) THEN
            X0=RPOLY_F(5,I)
            Y0=RPOLY_F(6,I)
            Z0=RPOLY_F(7,I)
            IF (IPOLY_OLD(1)>0) THEN
               NNS_OLD=NNS_OLD+1
               NNS=NNS+1
               REDIR(NNS_OLD)=NNS
               IPOLY_F(7,I)=NNS
               IFVNOD(NNS)=IFVNOD_OLD(NNS_OLD)
            ELSE
               IPOLY_F(7,I)=IPOLY_OLD(1)
            ENDIF
            NNP=NNP+1
            J0=1
            NNS1=NNS
            DO J=2,NNP_OLD
               IF (IPOLY_OLD(J)>0) NNS_OLD=NNS_OLD+1
               X1=RPOLY_OLD(3*(J-1)+1)
               Y1=RPOLY_OLD(3*(J-1)+2)
               Z1=RPOLY_OLD(3*(J-1)+3)
               DD=(X1-X0)**2+(Y1-Y0)**2+(Z1-Z0)**2
               IF (DD>TOLE2) THEN
                  NNP=NNP+1
                  IF (IPOLY_OLD(J)>0) THEN
                     NNS=NNS+1
                     IFVNOD(NNS)=IFVNOD_OLD(NNS_OLD)
                     RPOLY_F(4+3*(NNP-1)+1,I)=X1
                     RPOLY_F(4+3*(NNP-1)+2,I)=Y1
                     RPOLY_F(4+3*(NNP-1)+3,I)=Z1
                     IPOLY_F(6+NNP,I)=NNS
                  ELSE
                     RPOLY_F(4+3*(NNP-1)+1,I)=X1
                     RPOLY_F(4+3*(NNP-1)+2,I)=Y1
                     RPOLY_F(4+3*(NNP-1)+3,I)=Z1
                     IPOLY_F(6+NNP,I)=IPOLY_OLD(J)
                  ENDIF
                  X0=X1
                  Y0=Y1
                  Z0=Z1
               ELSEIF (IPOLY_OLD(J)>0.AND.
     .                 IPOLY_OLD(J0)<0) THEN
                  NNS=NNS+1
                  IFVNOD(NNS)=IFVNOD_OLD(NNS_OLD)
                  RPOLY_F(4+3*(NNP-1)+1,I)=X1
                  RPOLY_F(4+3*(NNP-1)+2,I)=Y1
                  RPOLY_F(4+3*(NNP-1)+3,I)=Z1
                  IPOLY_F(6+NNP,I)=NNS
               ENDIF
               IF (IPOLY_OLD(J)>0) REDIR(NNS_OLD)=NNS
               J0=J
            ENDDO
            X1=RPOLY_F(5,I)
            Y1=RPOLY_F(6,I)
            Z1=RPOLY_F(7,I)
            DD=(X1-X0)**2+(Y1-Y0)**2+(Z1-Z0)**2
            IF (DD<=TOLE2.AND.IPOLY_OLD(1)>0) THEN
               REDIR(NNS_OLD)=NNS1
               NNS=NNS-1
               NNP=NNP-1
            ELSEIF (DD<=TOLE2.AND.IPOLY_OLD(1)<0) THEN
               NNP=NNP-1
               RPOLY_F(5,I)=X0
               RPOLY_F(6,I)=Y0
               RPOLY_F(7,I)=Z0
               IPOLY_F(7,I)=NNS
            ENDIF
            IPOLY_F(6+NNP+1,I)=0
         ELSE
            ADR_OLD(1)=0
            ADR(1)=0
            DO J=1,NHOL
               ADR_OLD(J+1)=IPOLY_F(6+NNP_OLD+1+J,I)
            ENDDO
            ADR_OLD(NHOL+2)=NNP_OLD
            DO J=1,NHOL+1
               X0=RPOLY_F(4+3*ADR(J)+1,I)
               Y0=RPOLY_F(4+3*ADR(J)+2,I)
               Z0=RPOLY_F(4+3*ADR(J)+3,I)
               IF (IPOLY_OLD(ADR_OLD(J)+1)>0) THEN
                  NNS_OLD=NNS_OLD+1
                  NNS=NNS+1
                  REDIR(NNS_OLD)=NNS
                  IPOLY_F(6+ADR(J)+1,I)=NNS
                  IFVNOD(NNS)=IFVNOD_OLD(NNS_OLD)
               ELSE
                  IPOLY_F(6+ADR(J)+1,I)=IPOLY_OLD(ADR_OLD(J)+1)
               ENDIF
               NNP=NNP+1
               K0=1
               NNS1=NNS
               DO K=ADR_OLD(J)+2,ADR_OLD(J+1)
                  NNS_OLD=NNS_OLD+1
                  X1=RPOLY_OLD(3*(K-1)+1)
                  Y1=RPOLY_OLD(3*(K-1)+2)
                  Z1=RPOLY_OLD(3*(K-1)+3)
                  DD=(X1-X0)**2+(Y1-Y0)**2+(Z1-Z0)**2
                  IF (DD>TOLE2) THEN
                     NNP=NNP+1                       
                     IF (IPOLY_OLD(K)>0) THEN           
                        NNS=NNS+1                       
                        IFVNOD(NNS)=IFVNOD_OLD(NNS_OLD) 
                        RPOLY_F(4+3*(NNP-1)+1,I)=X1     
                        RPOLY_F(4+3*(NNP-1)+2,I)=Y1     
                        RPOLY_F(4+3*(NNP-1)+3,I)=Z1     
                        IPOLY_F(6+NNP,I)=NNS
                     ELSE
                        RPOLY_F(4+3*(NNP-1)+1,I)=X1     
                        RPOLY_F(4+3*(NNP-1)+2,I)=Y1     
                        RPOLY_F(4+3*(NNP-1)+3,I)=Z1     
                        IPOLY_F(6+NNP,I)=IPOLY_OLD(K)
                     ENDIF           
                     X0=X1                           
                     Y0=Y1                           
                     Z0=Z1                           
                  ELSEIF (IPOLY_OLD(K)>0.AND.
     .                    IPOLY_OLD(K0)<0) THEN
                     NNS=NNS+1
                     IFVNOD(NNS)=IFVNOD_OLD(NNS_OLD)
                     RPOLY_F(4+3*(NNP-1)+1,I)=X1
                     RPOLY_F(4+3*(NNP-1)+2,I)=Y1
                     RPOLY_F(4+3*(NNP-1)+3,I)=Z1
                     IPOLY_F(6+NNP,I)=NNS
                  ENDIF
                  IF (IPOLY_OLD(K)>0) REDIR(NNS_OLD)=NNS
                  K0=K
               ENDDO
               X1=RPOLY_F(4+3*(ADR(J)-1)+1,I)
               Y1=RPOLY_F(4+3*(ADR(J)-1)+2,I)
               Z1=RPOLY_F(4+3*(ADR(J)-1)+3,I)
               DD=(X1-X0)**2+(Y1-Y0)**2+(Z1-Z0)**2
               IF (DD<=TOLE2.AND.IPOLY_OLD(ADR_OLD(J)+1)>0) THEN
                  REDIR(NNS_OLD)=NNS1
                  NNS=NNS-1
                  NNP=NNP-1
               ELSEIF (DD<=TOLE2.AND.
     .                 IPOLY_OLD(ADR_OLD(J)+1)<0) THEN
                  NNP=NNP-1
                  RPOLY_F(4+3*(ADR(J)-1)+1,I)=X0
                  RPOLY_F(4+3*(ADR(J)-1)+2,I)=Y0
                  RPOLY_F(4+3*(ADR(J)-1)+3,I)=Z0
                  IPOLY_F(6+ADR(J)+1,I)=NNS
               ENDIF
               ADR(J+1)=NNP                             
            ENDDO
            IPOLY_F(6+NNP+1,I)=NHOL
            DO J=1,NHOL
               IPOLY_F(6+NNP+1+J,I)=ADR(J+1)
               RPOLY_F(4+3*NNP+3*(J-1)+1,I)=
     .                 RPOLY_F(4+3*NNP_OLD+3*(J-1)+1,I)
               RPOLY_F(4+3*NNP+3*(J-1)+2,I)=
     .                 RPOLY_F(4+3*NNP_OLD+3*(J-1)+2,I)
               RPOLY_F(4+3*NNP+3*(J-1)+3,I)=
     .                 RPOLY_F(4+3*NNP_OLD+3*(J-1)+3,I)
            ENDDO
         ENDIF      
         IPOLY_F(2,I)=NNP
C
         IF (NNP<NNP_OLD) THEN
C Nouvelle aire du polygone
            AREA=ZERO
            NX=RPOLY_F(2,I)
            NY=RPOLY_F(3,I)
            NZ=RPOLY_F(4,I)
            IF (NHOL==0) THEN
               X1=RPOLY_F(5,I)
               Y1=RPOLY_F(6,I)
               Z1=RPOLY_F(7,I)
               DO J=1,IPOLY_F(2,I)-2
                  JJ=J+1
                  JJJ=JJ+1
                  X2=RPOLY_F(4+3*(JJ-1)+1,I)
                  Y2=RPOLY_F(4+3*(JJ-1)+2,I)
                  Z2=RPOLY_F(4+3*(JJ-1)+3,I)
                  X3=RPOLY_F(4+3*(JJJ-1)+1,I)
                  Y3=RPOLY_F(4+3*(JJJ-1)+2,I)
                  Z3=RPOLY_F(4+3*(JJJ-1)+3,I)
                  X12=X2-X1
                  Y12=Y2-Y1
                  Z12=Z2-Z1
                  X13=X3-X1
                  Y13=Y3-Y1
                  Z13=Z3-Z1
                  NNX=Y12*Z13-Z12*Y13
                  NNY=Z12*X13-X12*Z13
                  NNZ=X12*Y13-Y12*X13
                  AREA=AREA+(NNX*NX+NNY*NY+NNZ*NZ)
               ENDDO
            ELSE
               X1=RPOLY_F(5,I)
               Y1=RPOLY_F(6,I)
               Z1=RPOLY_F(7,I)
               DO J=1,ADR(1)-2
                  JJ=J+1
                  JJJ=JJ+1
                  X2=RPOLY_F(4+3*(JJ-1)+1,I)
                  Y2=RPOLY_F(4+3*(JJ-1)+2,I)
                  Z2=RPOLY_F(4+3*(JJ-1)+3,I)
                  X3=RPOLY_F(4+3*(JJJ-1)+1,I)
                  Y3=RPOLY_F(4+3*(JJJ-1)+2,I)
                  Z3=RPOLY_F(4+3*(JJJ-1)+3,I)
                  X12=X2-X1
                  Y12=Y2-Y1
                  Z12=Z2-Z1
                  X13=X3-X1
                  Y13=Y3-Y1
                  Z13=Z3-Z1
                  NNX=Y12*Z13-Z12*Y13
                  NNY=Z12*X13-X12*Z13
                  NNZ=X12*Y13-Y12*X13
                  AREA=AREA+(NNX*NX+NNY*NY+NNZ*NZ)
               ENDDO
            ENDIF
C               
            DO J=1,NHOL
               X1=RPOLY_F(4+3*ADR(J+1)+1,I)
               Y1=RPOLY_F(4+3*ADR(J+1)+2,I)
               Z1=RPOLY_F(4+3*ADR(J+1)+3,I)
               DO K=ADR(J+1)+1,ADR(J+2)
                  KK=K+1
                  KKK=KK+1
                  X2=RPOLY_F(4+3*(KK-1)+1,I)
                  Y2=RPOLY_F(4+3*(KK-1)+2,I)
                  Z2=RPOLY_F(4+3*(KK-1)+3,I)
                  X3=RPOLY_F(4+3*(KKK-1)+1,I)
                  Y3=RPOLY_F(4+3*(KKK-1)+2,I)
                  Z3=RPOLY_F(4+3*(KKK-1)+3,I)
                  X12=X2-X1
                  Y12=Y2-Y1
                  Z12=Z2-Z1
                  X13=X3-X1
                  Y13=Y3-Y1
                  Z13=Z3-Z1
                  NNX=Y12*Z13-Z12*Y13
                  NNY=Z12*X13-X12*Z13
                  NNZ=X12*Y13-Y12*X13
                  AREA=AREA-(NNX*NX+NNY*NY+NNZ*NZ)
               ENDDO
            ENDDO
            RPOLY_F(1,I)=HALF*ABS(AREA)
         ENDIF
C
         DEALLOCATE(IPOLY_OLD, RPOLY_OLD, ADR_OLD, ADR)
      ENDDO
C
C
      DO I=1,NNS2
         I1=IFVNOD2(1,I)
         I2=IFVNOD2(2,I)
         IFVNOD2(1,I)=REDIR(I1)
         IFVNOD2(2,I)=REDIR(I2)
      ENDDO
C
      DEALLOCATE(IFVNOD_OLD, REDIR)
C---------------------------------------------------
C Fusion des polyhedres a volume infinitesimal a leur voisin
C---------------------------------------------------
      NPHMAX=2*NPHMAX
      INFO=0
      ALLOCATE(VOLU_OLD(NPOLH), RPOLY_F_OLD(LENRMAX,NPOLY),
     .         IPOLY_F_OLD(LENIMAX,NPOLY))
      DO I=1,NPOLH
         VOLU_OLD(I)=VOLU(I)
      ENDDO
      DO I=1,NPOLY
         DO J=1,LENIMAX
            IPOLY_F_OLD(J,I)=IPOLY_F(J,I)
         ENDDO
         DO J=1,LENRMAX
            RPOLY_F_OLD(J,I)=RPOLY_F(J,I)
         ENDDO
      ENDDO
  400 IF (ALLOCATED(POLH_NEW)) DEALLOCATE(POLH_NEW)
      IF (ALLOCATED(IMERGED)) DEALLOCATE(IMERGED)
      ALLOCATE(POLH_NEW(2+NPHMAX,NPOLH), IMERGED(NPOLH))
C
      IF (INFO==1) THEN
         DO I=1,NPOLH
            VOLU(I)=VOLU_OLD(I)
         ENDDO
         DO I=1,NPOLY
            DO J=1,LENIMAX
               IPOLY_F(J,I)=IPOLY_F_OLD(J,I)
            ENDDO
            DO J=1,LENRMAX
               RPOLY_F(J,I)=RPOLY_F_OLD(J,I)
            ENDDO
         ENDDO
      ENDIF
C
      DO I=1,NPOLH
         IMERGED(I)=0
         DO J=1,2+POLH_F(1,I)
            POLH_NEW(J,I)=POLH_F(J,I)
         ENDDO
      ENDDO
C
      DO I=1,NPOLH
         IF (POLH_F(2,I)<0) CYCLE
         IF (VOLU(I)<ZERO) THEN
            DO J=1,POLH_F(1,I)
               JJ=POLH_F(2+J,I)
               RPOLY_F(1,JJ)=-ONE
            ENDDO
            VOLU(I)=ZERO
         ENDIF
      ENDDO
C
C Volume moyen
      VM=ZERO
      NPA=0
      DO I=1,NPOLH
         IF (VOLU(I)>ZERO) THEN
            VM=VM+VOLU(I)
            NPA=NPA+1
         ENDIF
      ENDDO
      VM=VM/NPA
C
      RVOLU(33)=VM
      VOLUMIN=VM*RVOLU(31)
      INFO=0
      DO I=1,NPOLH
         IF (POLH_NEW(2,I)<0) THEN
            II=-POLH_NEW(2,I)
C On ne merge pas les briques additionnelles qui ont des noeuds a l'interieur de l'airbag
            IF(IBSA(II)==0) CYCLE
         ENDIF
C
         IF (VOLU(I)<=VOLUMIN) THEN
            AREAMAX=ZERO
            JMAX=0
            IMAX=0
            DO J=1,POLH_NEW(1,I)
               JJ=POLH_NEW(2+J,I)
               ITY=IPOLY_F(1,JJ)
               IF (ITY==1) CYCLE
               AREA=RPOLY_F(1,JJ)
               IF (AREA>AREAMAX) THEN
                  IF (IPOLY_F(5,JJ)==I) THEN
                     II=IPOLY_F(6,JJ)
                     IF (POLH_NEW(2,II)<0) THEN
                        III=-POLH_NEW(2,II)
                        IF (IBSA(III)==0) CYCLE
                     ENDIF
C
                     JMAX=J
                     IMAX=II
                  ELSEIF (IPOLY_F(6,JJ)==I) THEN
                     II=IPOLY_F(5,JJ)
                     IF (POLH_NEW(2,II)<0) THEN
                        III=-POLH_NEW(2,II)
                        IF (IBSA(III)==0) CYCLE
                     ENDIF
C
                     JMAX=J
                     IMAX=II
                  ENDIF
                  AREAMAX=AREA
               ENDIF
            ENDDO
            IF (JMAX/=0) THEN
               JJMAX=POLH_NEW(2+JMAX,I)
               RPOLY_F(1,JJMAX)=-ONE
            ELSE
               JMAX=1
               JJMAX=POLH_NEW(2+JMAX,I)
               ITY=IPOLY_F(1,JJMAX)
               IF (ITY==2) RPOLY_F(1,JJMAX)=-ONE
C On merge tous les polyhedres de volume nul avec le polyhedre 1
               IMAX=1
            ENDIF
            IF (IMERGED(IMAX)==1) IMAX=POLH_NEW(2,IMAX)
            NP=POLH_NEW(1,IMAX)
            VOLU(IMAX)=VOLU(IMAX)+VOLU(I)
            VOLU(I)=-ONE
            IMERGED(I)=1
            DO J=1,POLH_NEW(1,I)
               IF (J/=JMAX) THEN
                  NP=NP+1
                  IF (NP>NPHMAX) INFO=1
                  IF (INFO==0) THEN
                     JJ=POLH_NEW(2+J,I)
                     POLH_NEW(2+NP,IMAX)=JJ
                     ITY=IPOLY_F(1,JJ)
                     IF (ITY==2) THEN
                        IF (IPOLY_F(5,JJ)==I) THEN
                           IPOLY_F(5,JJ)=IMAX
                        ELSEIF (IPOLY_F(6,JJ)==I) THEN
                           IPOLY_F(6,JJ)=IMAX
                        ENDIF
                     ENDIF
                  ELSE
                     NPHMAX=MAX(NPHMAX,NP)
                  ENDIF
               ENDIF
            ENDDO
            POLH_NEW(2,I)=IMAX
            IF (INFO==0) POLH_NEW(1,IMAX)=NP   
         ENDIF
      ENDDO
      IF (INFO==1) GOTO 400
C
      VMIN=EP30
      DO I=1,NPOLH
         IF (VOLU(I)<=ZERO) CYCLE
         IF (VOLU(I)<VMIN) THEN
            VMIN=VOLU(I)
            IMIN=I
         ENDIF
         DO J=1,POLH_NEW(1,I)
            JJ=POLH_NEW(2+J,I)
            IF (IPOLY_F(1,JJ)==1.OR.RPOLY_F(1,JJ)<ZERO) CYCLE
            IF (IPOLY_F(5,JJ)==IPOLY_F(6,JJ)) RPOLY_F(1,JJ)=-ONE
         ENDDO
      ENDDO
      DEALLOCATE(VOLU_OLD, RPOLY_F_OLD, IPOLY_F_OLD, IMERGED)
C---------------------------------------------------
C Verifications
C---------------------------------------------------
      VOLPH=ZERO
      AREAP=ZERO
      AREAEL=ZERO
      NPOLHF=0
      DO I=1,NPOLH
         IF (VOLU(I)>ZERO) THEN
            NPOLHF=NPOLHF+1
            VOLPH=VOLPH+VOLU(I)
         ENDIF
      ENDDO
      NSPOLY=0
      NCPOLY=0
      DO I=1,NPOLY
         ITY=IPOLY_F(1,I)
         IF (ITY==1.AND.RPOLY_F(1,I)>ZERO) THEN
            NSPOLY=NSPOLY+1
            AREAP=AREAP+RPOLY_F(1,I)
         ELSEIF (ITY==2.AND.RPOLY_F(1,I)>ZERO) THEN
            NCPOLY=NCPOLY+1
         ENDIF
      ENDDO
      DO IEL=1,NEL
         AREAEL=AREAEL+ELAREA(IEL)
      ENDDO
C---------------------------------------------------
C Coordonnees elementaires des nouveaux noeuds
C---------------------------------------------------
      ALLOCATE(FVDATA(IFV)%IFVNOD(3,NNS+NNS2),
     .         FVDATA(IFV)%RFVNOD(2,NNS+NNS2))
      DO I=1,NNS+NNS2
         FVDATA(IFV)%IFVNOD(1,I)=0
      ENDDO
C
      NNS_OLD=NNS
      NNS=0
      DO I=1,NPOLY
         NNP=IPOLY_F(2,I)
         DO J=1,NNP
            IF (IPOLY_F(6+J,I)>0) THEN
               NNS=NNS+1
               IF (IFVNOD(NNS)<0) THEN
                  FVDATA(IFV)%IFVNOD(1,NNS)=2
                  FVDATA(IFV)%IFVNOD(2,NNS)=-IFVNOD(NNS)
                  CYCLE
               ENDIF
               FVDATA(IFV)%IFVNOD(1,NNS)=1
               IEL=IFVNOD(NNS)
               FVDATA(IFV)%IFVNOD(2,NNS)=IEL
               XX=RPOLY_F(4+3*(J-1)+1,I)
               YY=RPOLY_F(4+3*(J-1)+2,I)
               ZZ=RPOLY_F(4+3*(J-1)+3,I)
C
               N1=ELEMA(1,IEL)
               N2=ELEMA(2,IEL)
               N3=ELEMA(3,IEL)
               IF (TAGELA(IEL)>0) THEN
                  X1=XXX(1,N1)
                  Y1=XXX(2,N1)
                  Z1=XXX(3,N1)
                  X2=XXX(1,N2)
                  Y2=XXX(2,N2)
                  Z2=XXX(3,N2)
                  X3=XXX(1,N3)
                  Y3=XXX(2,N3)
                  Z3=XXX(3,N3)
               ELSEIF (TAGELA(IEL)<0) THEN
                  X1=XXXA(1,N1)
                  Y1=XXXA(2,N1)
                  Z1=XXXA(3,N1)
                  X2=XXXA(1,N2)
                  Y2=XXXA(2,N2)
                  Z2=XXXA(3,N2)
                  X3=XXXA(1,N3)
                  Y3=XXXA(2,N3)
                  Z3=XXXA(3,N3)
               ENDIF
C
               VPX=XX-X1
               VPY=YY-Y1
               VPZ=ZZ-Z1
               V1X=X2-X1
               V1Y=Y2-Y1
               V1Z=Z2-Z1            
               V2X=X3-X1
               V2Y=Y3-Y1
               V2Z=Z3-Z1
               CALL COORLOC(VPX, VPY, VPZ, V1X, V1Y,
     .                      V1Z, V2X, V2Y, V2Z, KSI,
     .                      ETA)
C
               FVDATA(IFV)%RFVNOD(1,NNS)=KSI
               FVDATA(IFV)%RFVNOD(2,NNS)=ETA
            ELSE
               JJ=-IPOLY_F(6+J,I)
               FVDATA(IFV)%IFVNOD(1,NNS_OLD+JJ)=3
               FVDATA(IFV)%IFVNOD(2,NNS_OLD+JJ)=IFVNOD2(1,JJ)
               FVDATA(IFV)%IFVNOD(3,NNS_OLD+JJ)=IFVNOD2(2,JJ)
               FVDATA(IFV)%RFVNOD(1,NNS_OLD+JJ)=RFVNOD2(1,JJ)
            ENDIF
         ENDDO
      ENDDO
      DO I=1,NNS2
         IF (FVDATA(IFV)%IFVNOD(1,NNS+I)==0) THEN
            FVDATA(IFV)%IFVNOD(1,NNS+I)=3
            FVDATA(IFV)%IFVNOD(2,NNS+I)=1
            FVDATA(IFV)%IFVNOD(3,NNS+I)=2
            FVDATA(IFV)%RFVNOD(1,NNS+I)=ONE
         ENDIF
      ENDDO
C---------------------------------------------------
C Triangulation des polygones
C---------------------------------------------------
      NNTR=0
      DO I=1,NPOLY
         NN=IPOLY_F(2,I)
         NNTR=NNTR+NN
      ENDDO
      ALLOCATE(FVDATA(IFV)%IFVTRI(6,NNTR),
     .         FVDATA(IFV)%IFVPOLY(NNTR),
     .         FVDATA(IFV)%IFVTADR(NPOLY+1))
C
      NNTR=0
      NPOLY_OLD=NPOLY
      NPOLY=0
      ALLOCATE(REDIR_POLY(NPOLY_OLD))
      DO I=1,NPOLY_OLD
         REDIR_POLY(I)=0
      ENDDO
C
      NNS=0
      DO I=1,NPOLY_OLD
         IF (RPOLY_F(1,I)<=ZERO) THEN
            DO J=1,IPOLY_F(2,I)
               IF (IPOLY_F(6+J,I)>0) NNS=NNS+1
            ENDDO
            CYCLE
         ENDIF
         NPOLY=NPOLY+1
         REDIR_POLY(I)=NPOLY
         NNP=IPOLY_F(2,I)
         NHOL=0
         IF (IPOLY_F(1,I)==2) NHOL=IPOLY_F(6+NNP+1,I)
         ALLOCATE(PNODES(2,NNP), PSEG(2,NNP), PHOLES(2,NHOL),
     .            PTRI(3,NNP), REDIR(NNP))   
         PTRI(1:3,1:NNP)=0
C
         DO J=1,NNP
            IF (IPOLY_F(6+J,I)>0) THEN
               NNS=NNS+1
               REDIR(J)=NNS
            ELSE
               JJ=-IPOLY_F(6+J,I)
               REDIR(J)=NNS_OLD+JJ
            ENDIF
         ENDDO
         IF (IPOLY_F(1,I)==1) THEN
            IPSURF=IPOLY_F(3,I)
            IC1=0
            IC2=0
         ELSEIF (IPOLY_F(1,I)==2) THEN
            IPSURF=0
            IC1=IPOLY_F(5,I)
            IC2=IPOLY_F(6,I)
         ENDIF 
C
C Coordonnees des sommets dans le plan du polygone
         NX=RPOLY_F(2,I)
         NY=RPOLY_F(3,I)
         NZ=RPOLY_F(4,I)
C
         X0=RPOLY_F(5,I)
         Y0=RPOLY_F(6,I)
         Z0=RPOLY_F(7,I)
         X1=RPOLY_F(8,I)
         Y1=RPOLY_F(9,I)
         Z1=RPOLY_F(10,I)
         NRM1=SQRT((X1-X0)**2+(Y1-Y0)**2+(Z1-Z0)**2)
         VX1=(X1-X0)/NRM1
         VY1=(Y1-Y0)/NRM1
         VZ1=(Z1-Z0)/NRM1
         VX2=NY*VZ1-NZ*VY1
         VY2=NZ*VX1-NX*VZ1
         VZ2=NX*VY1-NY*VX1
C
         PNODES(1,1)=ZERO
         PNODES(2,1)=ZERO
         IF (NHOL==0) THEN
            DO J=2,NNP
               XX=RPOLY_F(4+3*(J-1)+1,I)
               YY=RPOLY_F(4+3*(J-1)+2,I)
               ZZ=RPOLY_F(4+3*(J-1)+3,I)
               VX=XX-X0
               VY=YY-Y0
               VZ=ZZ-Z0
               PNODES(1,J)=VX*VX1+VY*VY1+VZ*VZ1
               PNODES(2,J)=VX*VX2+VY*VY2+VZ*VZ2
               PSEG(1,J-1)=J-1
               PSEG(2,J-1)=J
            ENDDO
            PSEG(1,NNP)=NNP
            PSEG(2,NNP)=1
            NSEG=NNP
         ELSE
            ALLOCATE(ADR(NHOL+1))
            DO J=1,NHOL
               ADR(J)=IPOLY_F(6+NNP+1+J,I)
            ENDDO
            ADR(NHOL+1)=NNP
            NSEG=0
            DO J=2,ADR(1)
               XX=RPOLY_F(4+3*(J-1)+1,I)
               YY=RPOLY_F(4+3*(J-1)+2,I)
               ZZ=RPOLY_F(4+3*(J-1)+3,I)
               VX=XX-X0
               VY=YY-Y0
               VZ=ZZ-Z0
               PNODES(1,J)=VX*VX1+VY*VY1+VZ*VZ1
               PNODES(2,J)=VX*VX2+VY*VY2+VZ*VZ2
               NSEG=NSEG+1
               PSEG(1,NSEG)=J-1
               PSEG(2,NSEG)=J
            ENDDO
            NSEG=NSEG+1
            PSEG(1,NSEG)=ADR(1)
            PSEG(2,NSEG)=1
            DO J=1,NHOL
               XX=RPOLY_F(4+3*ADR(J)+1,I)
               YY=RPOLY_F(4+3*ADR(J)+2,I)
               ZZ=RPOLY_F(4+3*ADR(J)+3,I)
               VX=XX-X0
               VY=YY-Y0
               VZ=ZZ-Z0
               PNODES(1,ADR(J)+1)=VX*VX1+VY*VY1+VZ*VZ1
               PNODES(2,ADR(J)+1)=VX*VX2+VY*VY2+VZ*VZ2
               DO K=ADR(J)+2,ADR(J+1)
                  XX=RPOLY_F(4+3*(K-1)+1,I)
                  YY=RPOLY_F(4+3*(K-1)+2,I)
                  ZZ=RPOLY_F(4+3*(K-1)+3,I)
                  VX=XX-X0
                  VY=YY-Y0
                  VZ=ZZ-Z0
                  PNODES(1,K)=VX*VX1+VY*VY1+VZ*VZ1
                  PNODES(2,K)=VX*VX2+VY*VY2+VZ*VZ2
                  NSEG=NSEG+1
                  PSEG(1,NSEG)=K-1
                  PSEG(2,NSEG)=K
               ENDDO
               NSEG=NSEG+1
               PSEG(1,NSEG)=ADR(J+1)
               PSEG(2,NSEG)=ADR(J)+1
C
               XX=RPOLY_F(4+3*NNP+3*(J-1)+1,I)
               YY=RPOLY_F(4+3*NNP+3*(J-1)+2,I)
               ZZ=RPOLY_F(4+3*NNP+3*(J-1)+3,I)
               VX=XX-X0
               VY=YY-Y0
               VZ=ZZ-Z0
               PHOLES(1,J)=VX*VX1+VY*VY1+VZ*VZ1
               PHOLES(2,J)=VX*VX2+VY*VY2+VZ*VZ2
            ENDDO
         ENDIF
C
         NELP = 0
         CALL C_TRICALL(PNODES, PSEG, PHOLES, PTRI, NNP,
     .                  NSEG,   NHOL, NELP  )
C
         FVDATA(IFV)%IFVTADR(NPOLY)=NNTR+1
         DO J=1,NELP
            NNTR=NNTR+1
            N1=PTRI(1,J)
            N2=PTRI(2,J)
            N3=PTRI(3,J)
C Verification de la normale
            X1=RPOLY_F(4+3*(N1-1)+1,I)
            Y1=RPOLY_F(4+3*(N1-1)+2,I)
            Z1=RPOLY_F(4+3*(N1-1)+3,I)
            X2=RPOLY_F(4+3*(N2-1)+1,I)
            Y2=RPOLY_F(4+3*(N2-1)+2,I)
            Z2=RPOLY_F(4+3*(N2-1)+3,I)
            X3=RPOLY_F(4+3*(N3-1)+1,I)
            Y3=RPOLY_F(4+3*(N3-1)+2,I)
            Z3=RPOLY_F(4+3*(N3-1)+3,I)
            X12=X2-X1
            Y12=Y2-Y1
            Z12=Z2-Z1
            X13=X3-X1
            Y13=Y3-Y1
            Z13=Z3-Z1
            NRX=Y12*Z13-Z12*Y13
            NRY=Z12*X13-X12*Z13
            NRZ=X12*Y13-Y12*X13
            SS=NRX*NX+NRY*NY+NRZ*NZ
C
            IF (SS>ZERO) THEN
               FVDATA(IFV)%IFVTRI(1,NNTR)=REDIR(N1)
               FVDATA(IFV)%IFVTRI(2,NNTR)=REDIR(N2)
               FVDATA(IFV)%IFVTRI(3,NNTR)=REDIR(N3)
            ELSE
               FVDATA(IFV)%IFVTRI(1,NNTR)=REDIR(N1)
               FVDATA(IFV)%IFVTRI(2,NNTR)=REDIR(N3)
               FVDATA(IFV)%IFVTRI(3,NNTR)=REDIR(N2)
            ENDIF 
            FVDATA(IFV)%IFVTRI(4,NNTR)=IPSURF
            FVDATA(IFV)%IFVTRI(5,NNTR)=IC1
            FVDATA(IFV)%IFVTRI(6,NNTR)=IC2
            FVDATA(IFV)%IFVPOLY(NNTR)=NNTR
         ENDDO
C
         DEALLOCATE(PNODES, PSEG, PHOLES, PTRI, REDIR)
      ENDDO
      FVDATA(IFV)%IFVTADR(NPOLY+1)=NNTR+1
C---------------------------------------------------
C Structure des polyhedres
C---------------------------------------------------
      ALLOCATE(FVDATA(IFV)%IFVPOLH(2*NPOLY),
     .         FVDATA(IFV)%IFVPADR(NPOLH+1),
     .         FVDATA(IFV)%IDPOLH(NPOLH),
     .         FVDATA(IFV)%IBPOLH(NPOLH))
      NNP=0
      NPOLH_OLD=NPOLH
      NPOLH=0
      ALLOCATE(REDIR_POLH(NPOLH_OLD))
      DO I=1,NPOLH_OLD
         REDIR_POLH(I)=0
      ENDDO
C
      DO I=1,NPOLH_OLD
         IF (VOLU(I)<=ZERO) CYCLE
         NPOLH=NPOLH+1
         REDIR_POLH(I)=NPOLH
         FVDATA(IFV)%IFVPADR(NPOLH)=NNP+1
         DO J=1,POLH_NEW(1,I)
            JJ=REDIR_POLY(POLH_NEW(2+J,I))
            IF (JJ==0) CYCLE
            NNP=NNP+1
            FVDATA(IFV)%IFVPOLH(NNP)=REDIR_POLY(POLH_NEW(2+J,I))
         ENDDO
C
         FVDATA(IFV)%IDPOLH(NPOLH)=NPOLH
C
         II=POLH_NEW(2,I)
         IF (II>=0) II=0
         IF (II<0.AND.IBSA(-II)==1) II=-II
         FVDATA(IFV)%IBPOLH(NPOLH)=II
      ENDDO
      FVDATA(IFV)%IFVPADR(NPOLH+1)=NNP+1
C
      DO I=1,NNTR
         IF (FVDATA(IFV)%IFVTRI(4,I)==0) THEN
            IC1=FVDATA(IFV)%IFVTRI(5,I)
            IC2=FVDATA(IFV)%IFVTRI(6,I)
            FVDATA(IFV)%IFVTRI(5,I)=REDIR_POLH(IC1)
            FVDATA(IFV)%IFVTRI(6,I)=REDIR_POLH(IC2)
         ENDIF
      ENDDO
C
      IVOLU(46)=NNS+NNS2
      IVOLU(47)=NNTR
      IVOLU(48)=NPOLY
      IVOLU(49)=NPOLH
C
      FVDATA(IFV)%NNS=NNS+NNS2
      FVDATA(IFV)%NNTR=NNTR
      FVDATA(IFV)%LENP=FVDATA(IFV)%IFVTADR(NPOLY+1)-1
      FVDATA(IFV)%NPOLY=NPOLY
      FVDATA(IFV)%LENH=FVDATA(IFV)%IFVPADR(NPOLH+1)-1
      FVDATA(IFV)%NPOLH=NPOLH
C
      ALLOCATE(FVDATA(IFV)%MPOLH(NPOLH), FVDATA(IFV)%QPOLH(3,NPOLH),
     .         FVDATA(IFV)%EPOLH(NPOLH), FVDATA(IFV)%PPOLH(NPOLH),
     .         FVDATA(IFV)%RPOLH(NPOLH), FVDATA(IFV)%GPOLH(NPOLH),
     .         FVDATA(IFV)%CPAPOLH(NPOLH), FVDATA(IFV)%CPBPOLH(NPOLH),
     .         FVDATA(IFV)%CPCPOLH(NPOLH), FVDATA(IFV)%RMWPOLH(NPOLH),
     .         FVDATA(IFV)%VPOLH_INI(NPOLH), FVDATA(IFV)%DTPOLH(NPOLH),
     .         FVDATA(IFV)%TPOLH(NPOLH), FVDATA(IFV)%CPDPOLH(NPOLH),
     .         FVDATA(IFV)%CPEPOLH(NPOLH), FVDATA(IFV)%CPFPOLH(NPOLH))
C
      DO I=1,NPOLH_OLD
         II=REDIR_POLH(I)
         IF (II==0) CYCLE
         FVDATA(IFV)%VPOLH_INI(II)=VOLU(I)
      ENDDO
C
      DEALLOCATE(IPOLY_F, RPOLY_F, POLH_F, VOLU, REDIR_POLY, REDIR_POLH,
     .           POLH_NEW, IFVNOD, IFVNOD2, RFVNOD2, XNS, XNS2)
C---------------------------------------------------
C Impressions
C---------------------------------------------------
      NSTR=0
      NCTR=0
      DO I=1,NNTR
         IF (FVDATA(IFV)%IFVTRI(4,I)>0) THEN
            NSTR=NSTR+1
         ELSE
            NCTR=NCTR+1
         ENDIF
      ENDDO
C
      WRITE(ISTDO,*)
      WRITE(ISTDO,'(A25,I10,A24)') 
     .' ** MONITORED VOLUME ID: ',IVOLU(1),' - FINITE VOLUME MESH **'
       WRITE(ISTDO,'(A42,I8)') 
     .   '    NUMBER OF SURFACE POLYGONS          : ',NSPOLY
       WRITE(ISTDO,'(A42,I8)') 
     .   '    NUMBER OF SURFACE TRIANGLES         : ',NSTR
       WRITE(ISTDO,'(A42,I8)') 
     .   '    NUMBER OF COMMUNICATION POLYGONS    : ',NCPOLY
       WRITE(ISTDO,'(A42,I8)') 
     .   '    NUMBER OF COMMUNICATION TRIANGLES   : ',NCTR
       WRITE(ISTDO,'(A42,I8)') 
     .   '    NUMBER OF POLYHEDRA (FINITE VOLUMES): ',NPOLHF
       IF (ILVOUT>=1) WRITE(ISTDO,'(A29,G16.9,A8,I8,A1)')
     .   '    MIN. POLYHEDRON VOLUME : ',VMIN,' (POLY. ',IMIN,')'
       IF (ILVOUT>=1) WRITE(ISTDO,'(A29,G16.9)')
     .   '    INITIAL MERGING VOLUME : ',VOLUMIN
       WRITE(ISTDO,'(A29,G16.9,A17,G16.9,A1)')
     .'    SUM VOLUME POLYHEDRA   : ',VOLPH,' (VOLUME AIRBAG: ',VOLG,')'
       WRITE(ISTDO,'(A29,G16.9,A17,G16.9,A1)') 
     .'    SUM AREA SURF. POLYGONS: ',AREAP,
     .                   ' (AREA AIRBAG  : ',AREAEL,')'
       WRITE(ISTDO,*)
       WRITE(IOUT,*)
       WRITE(IOUT,'(A25,I10,A24)') 
     .' ** MONITORED VOLUME ID: ',IVOLU(1),' - FINITE VOLUME MESH **'
       WRITE(IOUT,'(A42,I8)') 
     .   '    NUMBER OF SURFACE POLYGONS          : ',NSPOLY
       WRITE(IOUT,'(A42,I8)') 
     .   '    NUMBER OF COMMUNICATION POLYGONS    : ',NCPOLY
       WRITE(IOUT,'(A42,I8)') 
     .   '    NUMBER OF POLYHEDRA (FINITE VOLUMES): ',NPOLHF
       IF (ILVOUT>=1) WRITE(IOUT,'(A29,G16.9,A8,I8,A1)')
     .   '    MIN. POLYHEDRON VOLUME : ',VMIN,' (POLY. ',IMIN,')'
       IF (ILVOUT>=1) WRITE(IOUT,'(A29,G16.9)')
     .   '    INITIAL MERGING VOLUME : ',VOLUMIN
       WRITE(IOUT,'(A29,G16.9,A17,G16.9,A1)')
     .'    SUM VOLUME POLYHEDRA   : ',VOLPH,' (VOLUME AIRBAG: ',VOLG,')'
       WRITE(IOUT,'(A29,G16.9,A17,G16.9,A1)') 
     .'    SUM AREA SURF. POLYGONS: ',AREAP,
     .                   ' (AREA AIRBAG  : ',AREAEL,')'
       WRITE(IOUT,*)
C---------------------------------------------------
C Anim des volumes finis
C---------------------------------------------------
      FVDATA(IFV)%NPOLH_ANIM=NPOLH
      LENP=FVDATA(IFV)%IFVTADR(NPOLY+1)
      LENH=FVDATA(IFV)%IFVPADR(NPOLH+1)
      ALLOCATE(FVDATA(IFV)%IFVPOLY_ANIM(LENP),
     .         FVDATA(IFV)%IFVTADR_ANIM(NPOLY+1),
     .         FVDATA(IFV)%IFVPOLH_ANIM(LENH),
     .         FVDATA(IFV)%IFVPADR_ANIM(NPOLH+1),
     .         FVDATA(IFV)%IFVTRI_ANIM(6,NNTR),
     .         FVDATA(IFV)%REDIR_ANIM(NNS+NNS2),
     .         FVDATA(IFV)%NOD_ANIM(3,NNS+NNS2),
     .         REDIR(NNS+NNS2), ITAGT(NNTR))
      DO I=1,LENP
         FVDATA(IFV)%IFVPOLY_ANIM(I)=FVDATA(IFV)%IFVPOLY(I)
      ENDDO
      DO I=1,NPOLY+1
         FVDATA(IFV)%IFVTADR_ANIM(I)=FVDATA(IFV)%IFVTADR(I)
      ENDDO
      DO I=1,LENH
         FVDATA(IFV)%IFVPOLH_ANIM(I)=FVDATA(IFV)%IFVPOLH(I)
      ENDDO
      DO I=1,NPOLH+1
         FVDATA(IFV)%IFVPADR_ANIM(I)=FVDATA(IFV)%IFVPADR(I)
      ENDDO
C Elimination des noeuds doubles pour anim
      TOLE=EM05*FAC_LENGTH
      TOLE2=TOLE**2
      NNS_ANIM=0
      IF (ILVOUT/=0) WRITE(ISTDO,'(A25,I10,A39)')
     . ' ** MONITORED VOLUME ID: ',IVOLU(1),
     . ' - MERGING COINCIDENT NODES FOR ANIM **'
      ALLOCATE(PNODES(3,NNS+NNS2))
      DO I=1,NNS
         IF (FVDATA(IFV)%IFVNOD(1,I)==1) THEN
            IEL=FVDATA(IFV)%IFVNOD(2,I)
            KSI=FVDATA(IFV)%RFVNOD(1,I)
            ETA=FVDATA(IFV)%RFVNOD(2,I)
            N1=ELEMA(1,IEL)
            N2=ELEMA(2,IEL)
            N3=ELEMA(3,IEL)
            IF (TAGELA(IEL)>0) THEN
               X1=XXX(1,N1)
               Y1=XXX(2,N1)
               Z1=XXX(3,N1)
               X2=XXX(1,N2)
               Y2=XXX(2,N2)
               Z2=XXX(3,N2)
               X3=XXX(1,N3)
               Y3=XXX(2,N3)
               Z3=XXX(3,N3)
            ELSEIF (TAGELA(IEL)<0) THEN
               X1=XXXA(1,N1)
               Y1=XXXA(2,N1)
               Z1=XXXA(3,N1)
               X2=XXXA(1,N2)
               Y2=XXXA(2,N2)
               Z2=XXXA(3,N2)
               X3=XXXA(1,N3)
               Y3=XXXA(2,N3)
               Z3=XXXA(3,N3)
            ENDIF
            PNODES(1,I)=(ONE-KSI-ETA)*X1+KSI*X2+ETA*X3
            PNODES(2,I)=(ONE-KSI-ETA)*Y1+KSI*Y2+ETA*Y3
            PNODES(3,I)=(ONE-KSI-ETA)*Z1+KSI*Z2+ETA*Z3
         ELSEIF (FVDATA(IFV)%IFVNOD(1,I)==2) THEN
c            IF (NSPMD == 1) THEN
c               II=FVDATA(IFV)%IFVNOD(2,I)
c               PNODES(1,I)=X(1,II)
c               PNODES(2,I)=X(2,II)
c               PNODES(3,I)=X(3,II)
c            ELSE
               II=FVDATA(IFV)%IFVNOD(2,I)
               PNODES(1,I)=XXXSA(1,II)
               PNODES(2,I)=XXXSA(2,II)
               PNODES(3,I)=XXXSA(3,II)
c            ENDIF
         ENDIF
      ENDDO
      DO I=1,NNS2
         II=NNS+I
         I1=FVDATA(IFV)%IFVNOD(2,II)
         I2=FVDATA(IFV)%IFVNOD(3,II)
         ALPHA=FVDATA(IFV)%RFVNOD(1,II)
         PNODES(1,II)=ALPHA*PNODES(1,I1)+(ONE-ALPHA)*PNODES(1,I2)
         PNODES(2,II)=ALPHA*PNODES(2,I1)+(ONE-ALPHA)*PNODES(2,I2)
         PNODES(3,II)=ALPHA*PNODES(3,I1)+(ONE-ALPHA)*PNODES(3,I2)
      ENDDO
      DO I=1,NNS+NNS2
         IF (ILVOUT<0) CALL PROGBAR_C(I,NNS+NNS2)
C
         XX=PNODES(1,I)
         YY=PNODES(2,I)
         ZZ=PNODES(3,I)
         IFOUND=0
         DO J=1,NNS_ANIM
            XN(1)=FVDATA(IFV)%NOD_ANIM(1,J)
            XN(2)=FVDATA(IFV)%NOD_ANIM(2,J)
            XN(3)=FVDATA(IFV)%NOD_ANIM(3,J)
            DD2=(XX-XN(1))**2+(YY-XN(2))**2+(ZZ-XN(3))**2
            IF (DD2<=TOLE2) THEN
               IFOUND=J
               EXIT
            ENDIF
         ENDDO
         IF (IFOUND==0) THEN
            NNS_ANIM=NNS_ANIM+1
            REDIR(I)=NNS_ANIM
            FVDATA(IFV)%REDIR_ANIM(NNS_ANIM)=I
            FVDATA(IFV)%NOD_ANIM(1,NNS_ANIM)=XX
            FVDATA(IFV)%NOD_ANIM(2,NNS_ANIM)=YY         
            FVDATA(IFV)%NOD_ANIM(3,NNS_ANIM)=ZZ
         ELSE
            REDIR(I)=IFOUND
         ENDIF
      ENDDO
      FVDATA(IFV)%NNS_ANIM=NNS_ANIM
      FVDATA(IFV)%ID=IVOLU(1)
C
      DO I=1,NNTR
         N1=FVDATA(IFV)%IFVTRI(1,I)        
         N2=FVDATA(IFV)%IFVTRI(2,I)
         N3=FVDATA(IFV)%IFVTRI(3,I)
         FVDATA(IFV)%IFVTRI_ANIM(1,I)=REDIR(N1)
         FVDATA(IFV)%IFVTRI_ANIM(2,I)=REDIR(N2)
         FVDATA(IFV)%IFVTRI_ANIM(3,I)=REDIR(N3)
         FVDATA(IFV)%IFVTRI_ANIM(4,I)=
     .                   FVDATA(IFV)%IFVTRI(4,I)
         FVDATA(IFV)%IFVTRI_ANIM(5,I)=
     .                   FVDATA(IFV)%IFVTRI(5,I)
         FVDATA(IFV)%IFVTRI_ANIM(6,I)=
     .                   FVDATA(IFV)%IFVTRI(6,I)
      ENDDO
      DEALLOCATE(PNODES)
C
      DEALLOCATE(REDIR, ITAGT)
      DEALLOCATE(XXXSA)
C
      RETURN
C
      END
C
Chd|====================================================================
Chd|  ITRIBOX                       source/airbag/fvmesh.F        
Chd|-- called by -----------
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|-- calls ---------------
Chd|        POLCLIP                       source/airbag/fvmesh.F        
Chd|====================================================================
      SUBROUTINE ITRIBOX(TRI  , BOX, NORM, NVERTS, POLY,
     .                   NVMAX)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NVERTS, NVMAX
      my_real
     .        TRI(3,*), BOX(3,*), NORM(3,*), POLY(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NPOUT, J
      my_real
     .        A(3), N(3), POLYOUT(3,NVMAX)
      INTEGER P_REF(6)
      DATA P_REF /1,5,1,2,3,4/
C
C Intersection triangle-box
      NVERTS=3
      DO I=1,NVERTS
         POLY(1,I)=TRI(1,I)
         POLY(2,I)=TRI(2,I)
         POLY(3,I)=TRI(3,I)
      ENDDO
C      
      DO I=1,6
         A(1)=BOX(1,P_REF(I))
         A(2)=BOX(2,P_REF(I))
         A(3)=BOX(3,P_REF(I))
         N(1)=NORM(1,I)
         N(2)=NORM(2,I)
         N(3)=NORM(3,I)
         CALL POLCLIP(POLY,  NVERTS, A, N, POLYOUT,
     .                NPOUT)
         NVERTS=NPOUT
         DO J=1,NVERTS
            POLY(1,J)=POLYOUT(1,J)
            POLY(2,J)=POLYOUT(2,J)
            POLY(3,J)=POLYOUT(3,J)
         ENDDO
      ENDDO   
C
      RETURN
      END
C
Chd|====================================================================
Chd|  POLCLIP                       source/airbag/fvmesh.F        
Chd|-- called by -----------
Chd|        ITRIBOX                       source/airbag/fvmesh.F        
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE POLCLIP(POLYIN, NPIN, A, N, POLYOUT,
     .                   NPOUT )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPIN, NPOUT
      my_real
     .        POLYIN(3,*), A(*), N(*), POLYOUT(3,*)   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, IC1, IC2
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, SS1, SS2, X12, Y12, Z12,
     .        XA1, YA1, ZA1, ALPHA, XM, YM, ZM, SSN
C
      my_real
     .        DLAMCH
      EXTERNAL DLAMCH
C
C Clipping d'un polygone par un plan
      NPOUT=0
      DO I=1,NPIN
         IF (I/=NPIN) THEN
            II=I+1
         ELSE
            II=1
         ENDIF 
C
         X1=POLYIN(1,I)
         Y1=POLYIN(2,I)
         Z1=POLYIN(3,I)
         X2=POLYIN(1,II)
         Y2=POLYIN(2,II)
         Z2=POLYIN(3,II)
C
         SS1=(X1-A(1))*N(1)+(Y1-A(2))*N(2)+(Z1-A(3))*N(3)
         SS2=(X2-A(1))*N(1)+(Y2-A(2))*N(2)+(Z2-A(3))*N(3)
         IF (SS1<ZERO.AND.SS2<ZERO) CYCLE
         IF (SS1>=ZERO.AND.SS2>=ZERO) THEN
            NPOUT=NPOUT+1
            POLYOUT(1,NPOUT)=X1
            POLYOUT(2,NPOUT)=Y1
            POLYOUT(3,NPOUT)=Z1
            CYCLE
         ENDIF       
C
         X12=X2-X1
         Y12=Y2-Y1
         Z12=Z2-Z1
         XA1=X1-A(1)
         YA1=Y1-A(2)
         ZA1=Z1-A(3)
         SSN=X12*N(1)+Y12*N(2)+Z12*N(3)
         ALPHA=-(XA1*N(1)+YA1*N(2)+ZA1*N(3))/SSN
         XM=X1+ALPHA*X12
         YM=Y1+ALPHA*Y12
         ZM=Z1+ALPHA*Z12
         IF (SS1>=ZERO) THEN
            NPOUT=NPOUT+1
            POLYOUT(1,NPOUT)=X1
            POLYOUT(2,NPOUT)=Y1
            POLYOUT(3,NPOUT)=Z1
            NPOUT=NPOUT+1
            POLYOUT(1,NPOUT)=XM
            POLYOUT(2,NPOUT)=YM
            POLYOUT(3,NPOUT)=ZM
         ELSEIF (SS2>=ZERO) THEN
            NPOUT=NPOUT+1
            POLYOUT(1,NPOUT)=XM
            POLYOUT(2,NPOUT)=YM
            POLYOUT(3,NPOUT)=ZM        
         ENDIF
      ENDDO       
C
      RETURN
      END
C
Chd|====================================================================
Chd|  FACEPOLY                      source/airbag/fvmesh.F        
Chd|-- called by -----------
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE FACEPOLY(QUAD  , TRI  , NTRI  , IPOLY, RPOLY  ,
     .                    N     , NORMF, NORMT , NSMAX, NNP    ,
     .                    NRPMAX, NB   , NV    , LMIN , NPOLMAX,
     .                    NPPMAX, INFO , IELNOD, XXX  , ELEMA  ,
     .                    IBUF  , NELA , IBRIC , IFAC , MVID   ,
     .                    ILVOUT, IBUFA, TAGELA, XXXA ) 
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TRI(NSMAX,*), NTRI, NPPMAX, IPOLY(6+NPPMAX+1+NPOLMAX,*), 
     .        NSMAX, NRPMAX, NNP, NB, NV, NPOLMAX, INFO, 
     .        IELNOD(NPPMAX,*), ELEMA(3,*), IBUF(*), NELA, IBRIC, IFAC,
     .        MVID, ILVOUT, IBUFA(*), TAGELA(*)
      my_real
     .        QUAD(3,*), RPOLY(NRPMAX+3*NPOLMAX,*), N(*), NORMF(3,*), 
     .        NORMT(3,*), LMIN, XXX(3,*), XXXA(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NINTER(4), NP, J, JJ, IDBL, NPI, NSEG, REDIR(NTRI), K,
     .        KK, JJ1, NNO, ID1, ID2, ISEG(3,5*NTRI), NSEGF, ISTOP,
     .        ISSEG, IPSEG, ITAGSEG(5*NTRI), ITAG(5*NTRI*2), ICLOSE,
     .        INEXT, NN, NPOLY, IADPOLY(NTRI), LENPOLY(NTRI), IAD, II,
     .        POLY(2,5*NTRI+1), I0, JJJ, REDIR_TMP(NTRI), NPI_TMP,
     .        ELSEG(5*NTRI,2), ELINTER(4,NTRI), ELNODE(5*NTRI*2),
     .        NNS, LENMAX, NEDGE, J1, J2, IEDGE, K1, K2, 
     .        TEDGE(3*NTRI,3), EDGE(3*NTRI,3), N1, N2, IPOUT, M, MM,
     .        NHOL, IADHOL(NPOLMAX), ISEG3_OLD(5*NTRI), IDIFF, NSEC,
     .        KSMIN
      my_real
     .        TOLE, TOLE2, A(3), X1, Y1, Z1, X2, Y2, Z2, SS1, SS2,
     .        X12, Y12, Z12, XA1, YA1, ZA1, ALPHA, XM, YM, ZM, 
     .        STMP(4,3), SEG(5*NTRI,3,2), NX, NY, NZ, XA2, YA2, ZA2,
     .        XA12, YA12, ZA12, XA1P1, YA1P1, ZA1P1, BETA, 
     .        PINTER(4,NTRI,4), XL(NTRI), XLREF, XP1, YP1, ZP1, XP2, 
     .        YP2, ZP2, NODE(3,5*NTRI*2), XX1, YY1, ZZ1, DD1, DD2,
     .        XLC, LL, TOLEI, T1, T2, DD, XA1P2, YA1P2, ZA1P2,
     .        XEDGE(3*NTRI,4), XX2, YY2, ZZ2, NR1, NR2, VX1, VY1, VZ1,
     .        VX2, VY2, VZ2, SS, VVX, VVY, VVZ, XX, YY, ZZ,
     .        PHOL(3,NPOLMAX), XLOC(2,NPPMAX), XSEC(NPPMAX), YLMIN,
     .        YLMAX, YLSEC, XSMIN1, XSMIN2, XS, YS
C
C
*      TOLE2=EM5
      TOLE2 = ZERO
      TOLE2 = EPSILON(TOLE2)
      TOLEI=TOLE2*TEN
      TOLE=TOLE2*LMIN
C
      A(1)=QUAD(1,1)
      A(2)=QUAD(2,1)
      A(3)=QUAD(3,1)
C Recensement des aretes de triangles
      NEDGE=0
      DO I=1,NTRI
         DO J=1,3
            JJ=J+1
            IF (J==3) JJ=1
            J1=TRI(I,J)
            J2=TRI(I,JJ)
            IEDGE=0
            DO K=1,NEDGE
               K1=EDGE(K,1)
               K2=EDGE(K,2)
               IF ((K1==J1.AND.K2==J2).OR.
     .             (K2==J1.AND.K1==J2)) IEDGE=K
            ENDDO
            IF (IEDGE==0) THEN
               NEDGE=NEDGE+1
               TEDGE(I,J)=NEDGE
               EDGE(NEDGE,1)=J1
               EDGE(NEDGE,2)=J2
            ELSE
               TEDGE(I,J)=IEDGE
            ENDIF
         ENDDO
      ENDDO
C Calcul des intersections des aretes avec le plan de la facette
      DO I=1,NEDGE
         N1=EDGE(I,1)
         N2=EDGE(I,2)
         IF (N1>0) THEN
            X1=XXX(1,N1)
            Y1=XXX(2,N1)
            Z1=XXX(3,N1)
         ELSEIF (N1<0) THEN
            X1=XXXA(1,-N1)
            Y1=XXXA(2,-N1)
            Z1=XXXA(3,-N1)
         ENDIF
         IF (N2>0) THEN
            X2=XXX(1,N2)
            Y2=XXX(2,N2)
            Z2=XXX(3,N2)
         ELSEIF (N2<0) THEN
            X2=XXXA(1,-N2)
            Y2=XXXA(2,-N2)
            Z2=XXXA(3,-N2)
         ENDIF
         SS1=(X1-A(1))*N(1)+(Y1-A(2))*N(2)+(Z1-A(3))*N(3)
         SS2=(X2-A(1))*N(1)+(Y2-A(2))*N(2)+(Z2-A(3))*N(3)
         EDGE(I,3)=0
         IF (ABS(SS1)<=TOLE.OR.ABS(SS2)<=TOLE) THEN
            IF (ABS(SS1)<=TOLE.AND.ABS(SS2)<=TOLE) CYCLE
         ELSEIF (SS1<ZERO.AND.SS2<ZERO) THEN
            CYCLE
         ELSEIF (SS1>=ZERO.AND.SS2>=ZERO) THEN
            CYCLE
         ENDIF
         X12=X2-X1
         Y12=Y2-Y1
         Z12=Z2-Z1
         XA1=X1-A(1)
         YA1=Y1-A(2)
         ZA1=Z1-A(3)
         ALPHA=-(XA1*N(1)+YA1*N(2)+ZA1*N(3))
     .         /(X12*N(1)+Y12*N(2)+Z12*N(3))
         XM=X1+ALPHA*X12
         YM=Y1+ALPHA*Y12
         ZM=Z1+ALPHA*Z12
C
         EDGE(I,3)=1
         XEDGE(I,1)=XM
         XEDGE(I,2)=YM
         XEDGE(I,3)=ZM
         XEDGE(I,4)=SS1*SS2
      ENDDO   
C Calcul des intersections des triangles avec le plan de la facette (segments)
      DO I=1,4
         NINTER(I)=0
      ENDDO
      DO I=1,NTRI
         NP=0
         ELSEG(I,1)=I
         ELSEG(I,2)=I
         DO J=1,3
            IEDGE=TEDGE(I,J)
            IF (EDGE(IEDGE,3)==0) CYCLE
            NP=NP+1
            STMP(1,NP)=XEDGE(IEDGE,1)
            STMP(2,NP)=XEDGE(IEDGE,2)
            STMP(3,NP)=XEDGE(IEDGE,3)
            STMP(4,NP)=XEDGE(IEDGE,4)
         ENDDO
         IF (NP==0) THEN
            SEG(I,1,1)=ZERO
            SEG(I,2,1)=ZERO
            SEG(I,3,1)=ZERO
            SEG(I,1,2)=ZERO
            SEG(I,2,2)=ZERO
            SEG(I,3,2)=ZERO
            CYCLE
         ELSEIF (NP==3) THEN
            NP=0
            IDBL=0
            DO J=1,3
               IF (STMP(4,J)<ZERO) THEN
                  NP=NP+1
                  SEG(I,1,NP)=STMP(1,J)
                  SEG(I,2,NP)=STMP(2,J)
                  SEG(I,3,NP)=STMP(3,J)
               ELSEIF (STMP(4,J)==ZERO) THEN
                  IF (IDBL==0) THEN
                     NP=NP+1
                     SEG(I,1,NP)=STMP(1,J)
                     SEG(I,2,NP)=STMP(2,J)
                     SEG(I,3,NP)=STMP(3,J)
                     IDBL=1
                  ENDIF
               ENDIF
            ENDDO
         ELSEIF (NP==2) THEN
            SEG(I,1,1)=STMP(1,1)
            SEG(I,2,1)=STMP(2,1)
            SEG(I,3,1)=STMP(3,1)
            SEG(I,1,2)=STMP(1,2)
            SEG(I,2,2)=STMP(2,2)
            SEG(I,3,2)=STMP(3,2)
         ENDIF
C Intersection (eventuelle) de chaque segment avec les aretes de la facette
         DO J=1,4
            NPI=NINTER(J)
            IF (J/=4) THEN
               JJ=J+1
            ELSE
               JJ=1
            ENDIF
            NX=NORMF(1,J)
            NY=NORMF(2,J)
            NZ=NORMF(3,J)
            X1=SEG(I,1,1)
            Y1=SEG(I,2,1)
            Z1=SEG(I,3,1)
            X2=SEG(I,1,2)
            Y2=SEG(I,2,2)
            Z2=SEG(I,3,2)
            X12=X2-X1
            Y12=Y2-Y1
            Z12=Z2-Z1
            XA1=QUAD(1,J)
            YA1=QUAD(2,J)
            ZA1=QUAD(3,J)
            XA2=QUAD(1,JJ)
            YA2=QUAD(2,JJ)
            ZA2=QUAD(3,JJ)
            XA12=XA2-XA1
            YA12=YA2-YA1
            ZA12=ZA2-ZA1
            XA1P1=X1-XA1
            YA1P1=Y1-YA1
            ZA1P1=Z1-ZA1
            XA1P2=X2-XA1
            YA1P2=Y2-YA1
            ZA1P2=Z2-ZA1
C
            SS1=XA1P1*NX+YA1P1*NY+ZA1P1*NZ
            SS2=XA1P2*NX+YA1P2*NY+ZA1P2*NZ
            IF (SS1>ZERO.AND.SS2>ZERO) THEN
               SEG(I,1,1)=ZERO
               SEG(I,2,1)=ZERO
               SEG(I,3,1)=ZERO
               SEG(I,1,2)=ZERO
               SEG(I,2,2)=ZERO
               SEG(I,3,2)=ZERO
               CYCLE
            ENDIF
C
            SS2=X12*NX+Y12*NY+Z12*NZ
            IF (SS2==ZERO) CYCLE
            ALPHA=-SS1/SS2
            IF (ALPHA<-TOLEI.OR.ALPHA>ONE+TOLEI) CYCLE
            XM=X1+ALPHA*X12
            YM=Y1+ALPHA*Y12
            ZM=Z1+ALPHA*Z12
            BETA=((XM-XA1)*XA12+(YM-YA1)*YA12+(ZM-ZA1)*ZA12)
     .          /(XA12**2+YA12**2+ZA12**2)
            IF (BETA<-TOLEI.OR.BETA>ONE+TOLEI) CYCLE
            SS1=(X1-XA1)*NX+(Y1-YA1)*NY+(Z1-ZA1)*NZ
            SS2=(X2-XA1)*NX+(Y2-YA1)*NY+(Z2-ZA1)*NZ
C
            IF (SS1*SS2>ZERO) THEN
               IF (ABS(SS1)<=ABS(SS2)) THEN
                  SEG(I,1,1)=XM
                  SEG(I,2,1)=YM
                  SEG(I,3,1)=ZM
               ELSE
                  SEG(I,1,2)=XM
                  SEG(I,2,2)=YM
                  SEG(I,3,2)=ZM
               ENDIF
            ELSEIF (SS1*SS2<ZERO) THEN
               IF (SS1<ZERO) THEN
                  SEG(I,1,2)=XM
                  SEG(I,2,2)=YM
                  SEG(I,3,2)=ZM
               ELSEIF (SS2<ZERO) THEN
                  SEG(I,1,1)=XM
                  SEG(I,2,1)=YM
                  SEG(I,3,1)=ZM
               ENDIF   
            ELSE     
               IF (SS1==ZERO) THEN
                  IF (SS2<ZERO) THEN
                     SEG(I,1,1)=XM
                     SEG(I,2,1)=YM
                     SEG(I,3,1)=ZM
                  ELSEIF (SS2>=ZERO) THEN
                     SEG(I,1,2)=XM
                     SEG(I,2,2)=YM
                     SEG(I,3,2)=ZM
                  ENDIF
               ELSEIF (SS2==ZERO) THEN
                  IF (SS1<ZERO) THEN
                     SEG(I,1,2)=XM
                     SEG(I,2,2)=YM
                     SEG(I,3,2)=ZM
                  ELSEIF (SS1>=ZERO) THEN
                     SEG(I,1,1)=XM
                     SEG(I,2,1)=YM
                     SEG(I,3,1)=ZM
                  ENDIF
               ENDIF
            ENDIF
C
            NPI=NPI+1
            NINTER(J)=NPI
            PINTER(J,NPI,1)=XM
            PINTER(J,NPI,2)=YM
            PINTER(J,NPI,3)=ZM
            ELINTER(J,NPI)=I
            NX=NORMT(1,I)
            NY=NORMT(2,I)
            NZ=NORMT(3,I)
            SS1=XA12*NX+YA12*NY+ZA12*NZ
            IF (SS1<=ZERO) THEN
               PINTER(J,NPI,4)=ONE
            ELSEIF (SS1>=ZERO) THEN
               PINTER(J,NPI,4)=-ONE
            ENDIF
         ENDDO
      ENDDO
C
      NSEG=NTRI
      DO I=1,4
         IF (I/=4) THEN
            II=I+1
         ELSE
            II=1
         ENDIF
         XA1=QUAD(1,I)
         YA1=QUAD(2,I)
         ZA1=QUAD(3,I)
         XA2=QUAD(1,II)
         YA2=QUAD(2,II)
         ZA2=QUAD(3,II)
         XA12=XA2-XA1
         YA12=YA2-YA1
         ZA12=ZA2-ZA1
         IF (NINTER(I)==0) CYCLE
C Tri des points sur les bords de l'arete
         NPI=NINTER(I)
         DO J=1,NPI
            XM=PINTER(I,J,1)
            YM=PINTER(I,J,2)
            ZM=PINTER(I,J,3)
            XL(J)=((XM-XA1)*XA12+(YM-YA1)*YA12+(ZM-ZA1)*ZA12)
     .           /(XA12**2+YA12**2+ZA12**2)
         ENDDO
         DO J=1,NPI
            REDIR(J)=J
         ENDDO
         DO J=1,NPI
            XLREF=XL(REDIR(J))
            DO K=J+1,NPI
               XLC=XL(REDIR(K))
               IF (XLC<=XLREF) THEN
                  KK=REDIR(K)
                  REDIR(K)=REDIR(J)
                  REDIR(J)=KK
                  XLREF=XLC
               ENDIF
            ENDDO
         ENDDO
C Identification des doubles
         DO J=1,NPI
            JJ=REDIR(J)
            IF (JJ>0) THEN
               X1=PINTER(I,JJ,1)
               Y1=PINTER(I,JJ,2)
               Z1=PINTER(I,JJ,3)
               T1=PINTER(I,JJ,4)
               DO K=J+1,NPI
                  KK=REDIR(K)
                  IF (KK>0) THEN
                     X2=PINTER(I,KK,1)
                     Y2=PINTER(I,KK,2)
                     Z2=PINTER(I,KK,3)
                     T2=PINTER(I,KK,4)
                     DD=(X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2+(T2-T1)**2
                     IF (DD<=TOLE) REDIR(K)=0
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
C Elimination des segments de longueur nulle
         DO J=1,NPI
           JJ=REDIR(J)
           IF (JJ>0) THEN
               X1=PINTER(I,JJ,1)
               Y1=PINTER(I,JJ,2)
               Z1=PINTER(I,JJ,3)
               DO K=J+1,NPI
                  KK=REDIR(K)
                  IF (KK>0) THEN
                     X2=PINTER(I,KK,1)
                     Y2=PINTER(I,KK,2)
                     Z2=PINTER(I,KK,3)
                     DD=(X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2
                     IF (DD<=TOLE) THEN
                        REDIR(J)=0
                        REDIR(K)=0
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
C Condensation du tableau REDIR
         DO J=1,NPI
            REDIR_TMP(J)=REDIR(J)
         ENDDO
         NPI_TMP=NPI
         NPI=0
         DO J=1,NPI_TMP
            IF (REDIR_TMP(J)>0) THEN
               NPI=NPI+1
               REDIR(NPI)=REDIR_TMP(J)
            ENDIF
         ENDDO
         IF (NPI==0) CYCLE
C Creation des segments sur les aretes
         IF (PINTER(I,REDIR(1),4)==-ONE) THEN
            NSEG=NSEG+1
            SEG(NSEG,1,1)=XA1
            SEG(NSEG,2,1)=YA1
            SEG(NSEG,3,1)=ZA1
            ELSEG(NSEG,1)=0
            SEG(NSEG,1,2)=PINTER(I,REDIR(1),1)
            SEG(NSEG,2,2)=PINTER(I,REDIR(1),2)
            SEG(NSEG,3,2)=PINTER(I,REDIR(1),3)
            ELSEG(NSEG,2)=ELINTER(I,REDIR(1))
         ENDIF
         DO J=1,NPI-1
            JJ=REDIR(J)
            JJ1=REDIR(J+1)
            IF (PINTER(I,JJ,4)==-ONE) CYCLE
            XP1=PINTER(I,JJ,1)
            YP1=PINTER(I,JJ,2)
            ZP1=PINTER(I,JJ,3)
            XP2=PINTER(I,JJ1,1)
            YP2=PINTER(I,JJ1,2)
            ZP2=PINTER(I,JJ1,3)
            NSEG=NSEG+1
            SEG(NSEG,1,1)=XP1
            SEG(NSEG,2,1)=YP1
            SEG(NSEG,3,1)=ZP1
            ELSEG(NSEG,1)=ELINTER(I,JJ)
            SEG(NSEG,1,2)=XP2
            SEG(NSEG,2,2)=YP2
            SEG(NSEG,3,2)=ZP2
            ELSEG(NSEG,2)=ELINTER(I,JJ1)
         ENDDO
         IF (PINTER(I,REDIR(NPI),4)==ONE) THEN
            NSEG=NSEG+1
            SEG(NSEG,1,1)=PINTER(I,REDIR(NPI),1)
            SEG(NSEG,2,1)=PINTER(I,REDIR(NPI),2)
            SEG(NSEG,3,1)=PINTER(I,REDIR(NPI),3)
            ELSEG(NSEG,1)=ELINTER(I,REDIR(NPI))
            SEG(NSEG,1,2)=XA2
            SEG(NSEG,2,2)=YA2
            SEG(NSEG,3,2)=ZA2
            ELSEG(NSEG,2)=0
         ENDIF
      ENDDO
C Recensement des noeuds
      NNO=0
      DO I=1,NSEG
         X1=SEG(I,1,1)
         Y1=SEG(I,2,1)
         Z1=SEG(I,3,1)
         X2=SEG(I,1,2)
         Y2=SEG(I,2,2)
         Z2=SEG(I,3,2)
         IF (X1==ZERO.AND.Y1==ZERO.AND.Z1==ZERO.AND.
     .       X2==ZERO.AND.Y2==ZERO.AND.Z2==ZERO) THEN
            ISEG(1,I)=0
            ISEG(2,I)=0
            ISEG(3,I)=1
            CYCLE
         ENDIF
         ID1=0
         ID2=0
         DO J=1,NNO
            XX1=NODE(1,J)
            YY1=NODE(2,J)
            ZZ1=NODE(3,J)
            DD1=(XX1-X1)**2+(YY1-Y1)**2+(ZZ1-Z1)**2
            DD2=(XX1-X2)**2+(YY1-Y2)**2+(ZZ1-Z2)**2
            IF (DD1<=TOLE) ID1=J
            IF (DD2<=TOLE) ID2=J
         ENDDO
         LL=(X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2
         IF (ID1==0) THEN
            NNO=NNO+1
            NODE(1,NNO)=X1
            NODE(2,NNO)=Y1
            NODE(3,NNO)=Z1
            ELNODE(NNO)=ELSEG(I,1)
            ID1=NNO
         ENDIF
         IF (LL<=TOLE) ID2=ID1
         IF (ID2==0) THEN
            NNO=NNO+1
            NODE(1,NNO)=X2
            NODE(2,NNO)=Y2
            NODE(3,NNO)=Z2
            ELNODE(NNO)=ELSEG(I,2)
            ID2=NNO
         ENDIF
         ISEG(1,I)=ID1
         ISEG(2,I)=ID2
         ISEG(3,I)=0
         IF (ID1==ID2) ISEG(3,I)=1
      ENDDO
C Elimination des segments doubles
      DO I=1,NSEG
         IF (ISEG(3,I)==1) CYCLE
         ID1=ISEG(1,I)
         ID2=ISEG(2,I)
         DO J=I+1,NSEG
            IF ((ISEG(1,J)==ID1.AND.ISEG(2,J)==ID2).OR.
     .          (ISEG(1,J)==ID2.AND.ISEG(2,J)==ID1)) ISEG(3,J)=1
         ENDDO
      ENDDO
C
      NSEGF=0
      DO I=1,NSEG
         IF (ISEG(3,I)==0) NSEGF=NSEGF+1
      ENDDO
      IF (NSEGF<=1) RETURN
C
C Reconstruction des polygones
      ISTOP=0
      NPOLY=0
      NP=0
C
      DO I=1,NSEG
         ISEG3_OLD(I)=ISEG(3,I)
      ENDDO
C
      DO WHILE (ISTOP==0)
         I=1
         DO WHILE (ISEG(3,I)==1.AND.I<=NSEG)
            I=I+1
         ENDDO
         IF (I==NSEG+1) THEN
            ISTOP=1
            CYCLE
         ENDIF
         ISSEG=I
         IPSEG=2
         DO J=1,NNO
            ITAG(J)=0
         ENDDO
         DO J=1,NSEG
            ITAGSEG(J)=0
         ENDDO
         ITAG(ISEG(1,ISSEG))=I
         ITAGSEG(ISSEG)=I
         ICLOSE=0
         I0=I-1
         DO WHILE (ICLOSE==0.AND.I<NSEG)
            I=I+1
            INEXT=0
            DO J=1,NSEG
               IF (J==ISSEG.OR.ISEG(3,J)==1.OR.INEXT/=0) CYCLE
               ID1=ISEG(1,J)
               ID2=ISEG(2,J)
               IF (ID1==ISEG(IPSEG,ISSEG)) THEN
                  INEXT=1
                  ISSEG=J
                  IPSEG=2
                  ITAG(ISEG(1,ISSEG))=I
                  ITAGSEG(J)=I
               ELSEIF (ID2==ISEG(IPSEG,ISSEG)) THEN
                  INEXT=1
                  ISSEG=J
                  IPSEG=1
                  ITAG(ISEG(2,ISSEG))=I
                  ITAGSEG(J)=-I
               ENDIF
            ENDDO
            IF (INEXT==0) THEN
               ISEG(3,ISSEG)=1
               I=NSEG
            ELSE
               NN=ITAG(ISEG(IPSEG,ISSEG))
               IF (NN/=0) ICLOSE=NN
            ENDIF
         ENDDO
         IF (ICLOSE/=0) THEN
            NPOLY=NPOLY+1
            IADPOLY(NPOLY)=NP
            IAD=NP
            DO J=1,NSEG
               JJ=ITAGSEG(J)
               IF (ABS(JJ)>=ICLOSE) THEN
                  NP=NP+1
                  POLY(1,IAD+ABS(JJ)-I0)=J
                  IF (JJ>0) THEN
                     POLY(2,IAD+ABS(JJ)-I0)=2
                  ELSEIF (JJ<0) THEN
                     POLY(2,IAD+ABS(JJ)-I0)=1
                  ENDIF
                  ISEG(3,J)=1
               ENDIF
            ENDDO
            LENPOLY(NPOLY)=NP-IADPOLY(NPOLY)
         ENDIF      
C
         IDIFF=0
         DO J=1,NSEG
            IDIFF=MAX(IDIFF,ABS(ISEG3_OLD(J)-ISEG(3,J)))
         ENDDO
         IF (IDIFF==0) THEN
           CALL ANCMSG(MSGID=9,ANMODE=ANINFO,
     .                 I1=MVID)
           IF (ILVOUT/=0) THEN
             CALL ANCMSG(MSGID=10,ANMODE=ANINFO_BLIND,
     .                   I1=IBRIC,I2=IFAC)
           END IF
           CALL ARRET(2)
         ENDIF
      ENDDO
C
      INFO=0
      IF (NPOLY>NPOLMAX) THEN
         NPOLMAX=NPOLY
         INFO=1
      ENDIF
      LENMAX=0
      DO I=1,NPOLY
         LENMAX=MAX(LENMAX,LENPOLY(I))
      ENDDO
      IF (LENMAX>NPPMAX) THEN
         NPPMAX=LENMAX
         INFO=1
      ENDIF
      IF (INFO==1) RETURN
      NNP=NPOLY
C
      NNS=0
      DO I=1,NPOLY
         IAD=IADPOLY(I)
         IPOLY(1,I)=2
         IPOLY(2,I)=LENPOLY(I)
         IPOLY(3,I)=NB
         IPOLY(4,I)=NV
         IPOLY(5,I)=0
         IPOLY(6,I)=0
         RPOLY(1,I)=ZERO
         RPOLY(2,I)=N(1)
         RPOLY(3,I)=N(2)
         RPOLY(4,I)=N(3) 
         DO J=1,LENPOLY(I)
            JJ=POLY(1,IAD+J)
            JJJ=POLY(2,IAD+J)
            ID1=ISEG(JJJ,JJ)
            NNS=NNS+1
            IPOLY(6+J,I)=NNS
            IELNOD(J,I)=ELNODE(ID1)
            RPOLY(4+3*(J-1)+1,I)=NODE(1,ID1)
            RPOLY(4+3*(J-1)+2,I)=NODE(2,ID1)
            RPOLY(4+3*(J-1)+3,I)=NODE(3,ID1)
         ENDDO
      ENDDO
C
C Recherche des trous
      INFO=0
      DO I=1,NPOLY
C
         NHOL=0
         DO J=1,NPOLY
            IADHOL(J)=0
         ENDDO
C
         DO J=1,NPOLY
            IF (I==J) CYCLE
            ALPHA=ZERO
            X1=RPOLY(5,J)
            Y1=RPOLY(6,J)
            Z1=RPOLY(7,J)
            DO K=1,LENPOLY(I)
               KK=K+1
               IF (K==LENPOLY(I)) KK=1
               XX1=RPOLY(4+3*(K-1)+1,I)
               YY1=RPOLY(4+3*(K-1)+2,I)
               ZZ1=RPOLY(4+3*(K-1)+3,I)
               XX2=RPOLY(4+3*(KK-1)+1,I)
               YY2=RPOLY(4+3*(KK-1)+2,I)
               ZZ2=RPOLY(4+3*(KK-1)+3,I)
               VX1=XX1-X1
               VY1=YY1-Y1
               VZ1=ZZ1-Z1
               VX2=XX2-X1
               VY2=YY2-Y1
               VZ2=ZZ2-Z1
               NR1=SQRT(VX1**2+VY1**2+VZ1**2)
               NR2=SQRT(VX2**2+VY2**2+VZ2**2)
               VX1=VX1/NR1
               VY1=VY1/NR1
               VZ1=VZ1/NR1
               VX2=VX2/NR2
               VY2=VY2/NR2
               VZ2=VZ2/NR2
               SS=VX1*VX2+VY1*VY2+VZ1*VZ2
               VVX=VY1*VZ2-VZ1*VY2
               VVY=VZ1*VX2-VX1*VZ2
               VVZ=VX1*VY2-VY1*VX2
               SS1=N(1)*VVX+N(2)*VVY+N(3)*VVZ
               IF (SS1>=ZERO) THEN
                  ALPHA=ALPHA+ACOS(SS)
               ELSE
                  ALPHA=ALPHA-ACOS(SS)
               ENDIF
            ENDDO
C
            IF (ABS(ALPHA)>=ONE) THEN
C Le premier point du polygone i est a l'interieur du polygone j
C On teste tous les autres
               IPOUT=0
               DO K=2,LENPOLY(J)
                  X2=RPOLY(4+3*(K-1)+1,J)
                  Y2=RPOLY(4+3*(K-1)+2,J)
                  Z2=RPOLY(4+3*(K-1)+3,J)
                  ALPHA=ZERO
                  DO M=1,LENPOLY(I)
                     MM=M+1
                     IF (M==LENPOLY(I)) MM=1
                     XX1=RPOLY(4+3*(M-1)+1,I)
                     YY1=RPOLY(4+3*(M-1)+2,I)
                     ZZ1=RPOLY(4+3*(M-1)+3,I)
                     XX2=RPOLY(4+3*(MM-1)+1,I)
                     YY2=RPOLY(4+3*(MM-1)+2,I)
                     ZZ2=RPOLY(4+3*(MM-1)+3,I)
                     VX1=XX1-X1
                     VY1=YY1-Y1
                     VZ1=ZZ1-Z1
                     VX2=XX2-X1
                     VY2=YY2-Y1
                     VZ2=ZZ2-Z1
                     NR1=SQRT(VX1**2+VY1**2+VZ1**2)
                     NR2=SQRT(VX2**2+VY2**2+VZ2**2)
                     VX1=VX1/NR1
                     VY1=VY1/NR1
                     VZ1=VZ1/NR1
                     VX2=VX2/NR2
                     VY2=VY2/NR2
                     VZ2=VZ2/NR2
                     SS=VX1*VX2+VY1*VY2+VZ1*VZ2
                     VVX=VY1*VZ2-VZ1*VY2
                     VVY=VZ1*VX2-VX1*VZ2
                     VVZ=VX1*VY2-VY1*VX2
                     SS1=N(1)*VVX+N(2)*VVY+N(3)*VVZ
                     IF (SS1>=ZERO) THEN
                        ALPHA=ALPHA+ACOS(SS)
                     ELSE
                        ALPHA=ALPHA-ACOS(SS)
                     ENDIF
                  ENDDO
                  IF (ABS(ALPHA)<ONE) IPOUT=1
               ENDDO
C
               IF (IPOUT==1) CYCLE
C Le polygone j est un trou dans le polygone i
               IPOLY(1,J)=-1
               NHOL=NHOL+1
               IADHOL(NHOL)=LENPOLY(I)
               IF (LENPOLY(I)+LENPOLY(J)>NPPMAX) THEN
                  NPPMAX=LENPOLY(I)+LENPOLY(J)
                  INFO=1
               ELSE
                  DO K=1,LENPOLY(J)
                     IPOLY(6+IADHOL(NHOL)+K,I)=IPOLY(6+K,J)
                     IELNOD(IADHOL(NHOL)+K,I)=IELNOD(K,J)
                     RPOLY(4+3*IADHOL(NHOL)+3*(K-1)+1,I)=
     .                                      RPOLY(4+3*(K-1)+1,J)
                     RPOLY(4+3*IADHOL(NHOL)+3*(K-1)+2,I)=
     .                                      RPOLY(4+3*(K-1)+2,J)
                     RPOLY(4+3*IADHOL(NHOL)+3*(K-1)+3,I)=
     .                                      RPOLY(4+3*(K-1)+3,J)
                  ENDDO
               ENDIF
               LENPOLY(I)=LENPOLY(I)+LENPOLY(J)
C Point interieur polygone j
               VX1=QUAD(1,2)-QUAD(1,1)
               VY1=QUAD(2,2)-QUAD(2,1)
               VZ1=QUAD(3,2)-QUAD(3,1)
               SS=SQRT(VX1**2+VY1**2+VZ1**2)
               VX1=VX1/SS
               VY1=VY1/SS
               VZ1=VZ1/SS
               VX2=N(2)*VZ1-N(3)*VY1
               VY2=N(3)*VX1-N(1)*VZ1
               VZ2=N(1)*VY1-N(2)*VX1
               X1=RPOLY(5,J)
               Y1=RPOLY(6,J)
               Z1=RPOLY(7,J)
               XLOC(1,1)=ZERO
               XLOC(2,1)=ZERO
               YLMIN=EP30
               YLMAX=-EP30
               DO K=2,LENPOLY(J)
                  XX=RPOLY(4+3*(K-1)+1,J)
                  YY=RPOLY(4+3*(K-1)+2,J)
                  ZZ=RPOLY(4+3*(K-1)+3,J)
                  VVX=XX-X1
                  VVY=YY-Y1
                  VVZ=ZZ-Z1
                  XLOC(1,K)=VVX*VX1+VVY*VY1+VVZ*VZ1
                  XLOC(2,K)=VVX*VX2+VVY*VY2+VVZ*VZ2
                  IF (XLOC(2,K)<YLMIN) YLMIN=XLOC(2,K)
                  IF (XLOC(2,K)>YLMAX) YLMAX=XLOC(2,K)
               ENDDO
               YLSEC=HALF*(YLMIN+YLMAX)
C
               NSEC=0
               DO K=1,LENPOLY(J)
                  KK=K+1
                  IF (K==LENPOLY(J)) KK=1
                  X1=XLOC(1,K)
                  Y1=XLOC(2,K)
                  X2=XLOC(1,KK)
                  Y2=XLOC(2,KK)
                  IF (Y1-Y2/=ZERO) THEN
                     ALPHA=(YLSEC-Y2)/(Y1-Y2)
                     IF (ALPHA>=ZERO.AND.ALPHA<=ONE) THEN
                        NSEC=NSEC+1
                        XSEC(NSEC)=ALPHA*X1+(ONE-ALPHA)*X2
                     ENDIF
                  ELSE
                     IF (Y1==YLSEC) THEN
                        NSEC=NSEC+1
                        XSEC(NSEC)=X1
                        NSEC=NSEC+1
                        XSEC(NSEC)=X2
                     ENDIF
                  ENDIF
               ENDDO
C
               XSMIN1=EP30
               DO K=1,NSEC
                  IF (XSEC(K)<XSMIN1) THEN
                     XSMIN1=XSEC(K)
                     KSMIN=K
                  ENDIF
               ENDDO
               XSMIN2=EP30
               DO K=1,NSEC
                  IF (K==KSMIN) CYCLE
                  IF (XSEC(K)<XSMIN2) XSMIN2=XSEC(K)
               ENDDO
C
               XS=HALF*(XSMIN1+XSMIN2)
               YS=YLSEC
               PHOL(1,NHOL)=RPOLY(5,J)+XS*VX1+YS*VX2
               PHOL(2,NHOL)=RPOLY(6,J)+XS*VY1+YS*VY2
               PHOL(3,NHOL)=RPOLY(7,J)+XS*VZ1+YS*VZ2
            ENDIF
         ENDDO
C
         IF (INFO==0) THEN
            IPOLY(2,I)=LENPOLY(I)
            IPOLY(6+LENPOLY(I)+1,I)=NHOL
            DO J=1,NHOL
               IPOLY(6+LENPOLY(I)+1+J,I)=IADHOL(J)
               RPOLY(4+3*LENPOLY(I)+3*(J-1)+1,I)=PHOL(1,J)
               RPOLY(4+3*LENPOLY(I)+3*(J-1)+2,I)=PHOL(2,J)
               RPOLY(4+3*LENPOLY(I)+3*(J-1)+3,I)=PHOL(3,J)
            ENDDO
         ENDIF
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  POLYHEDR                      source/airbag/fvmesh.F        
Chd|-- called by -----------
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE POLYHEDR(IPOLY, RPOLY   , POLB  , NPOLB, POLH,
     .                    NPOLH, NRPMAX  , NPHMAX, IBRIC, LMIN,
     .                    INFO , NPOLHMAX, NPPMAX, TOLE2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPPMAX, IPOLY(6+NPPMAX,*), POLB(*), NPOLB, NPHMAX, 
     .        POLH(NPHMAX+2,*),NPOLH, NRPMAX, IBRIC, INFO, NPOLHMAX
      my_real
     .        RPOLY(NRPMAX,*), LMIN, TOLE2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ITAG(NPOLB), ICMAX, ICUR, II, J, JJ, K, KK, ISTOP,
     .        L, LL, ICUR_OLD, ITY, JMIN, PMIN, POLD
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, XX1, YY1, ZZ1, XX2, YY2, ZZ2,
     .        DD11, DD12, DD21, DD22, TOLE
C
      TOLE=TOLE2*LMIN
C
      DO I=1,NPOLB
         ITAG(I)=0
      ENDDO
C
      ICMAX=0
      DO I=1,NPOLB
         IF (ITAG(I)==0) THEN
            ICMAX=ICMAX+1
            ITAG(I)=ICMAX
            ICUR=ICMAX
         ELSE
            ICUR=ITAG(I)
         ENDIF
         II=POLB(I)
         DO J=1,IPOLY(2,II)
            IF (J/=IPOLY(2,II)) THEN
               JJ=J+1
            ELSE
               JJ=1
            ENDIF
            X1=RPOLY(4+3*(J-1)+1,II)
            Y1=RPOLY(4+3*(J-1)+2,II)
            Z1=RPOLY(4+3*(J-1)+3,II)
            X2=RPOLY(4+3*(JJ-1)+1,II)
            Y2=RPOLY(4+3*(JJ-1)+2,II)
            Z2=RPOLY(4+3*(JJ-1)+3,II)
            DO K=1,NPOLB
               IF (K==I) CYCLE
               KK=POLB(K)
               ISTOP=0
               L=0
               DO WHILE (ISTOP==0.AND.L<IPOLY(2,KK))
                  L=L+1
                  IF (L/=IPOLY(2,KK)) THEN
                     LL=L+1
                  ELSE
                     LL=1
                  ENDIF
                  XX1=RPOLY(4+3*(L-1)+1,KK)
                  YY1=RPOLY(4+3*(L-1)+2,KK)
                  ZZ1=RPOLY(4+3*(L-1)+3,KK)
                  XX2=RPOLY(4+3*(LL-1)+1,KK)
                  YY2=RPOLY(4+3*(LL-1)+2,KK)
                  ZZ2=RPOLY(4+3*(LL-1)+3,KK)
                  DD11=(XX1-X1)**2+(YY1-Y1)**2+(ZZ1-Z1)**2
                  DD21=(XX2-X1)**2+(YY2-Y1)**2+(ZZ2-Z1)**2
                  DD12=(XX1-X2)**2+(YY1-Y2)**2+(ZZ1-Z2)**2
                  DD22=(XX2-X2)**2+(YY2-Y2)**2+(ZZ2-Z2)**2
                  IF ((DD11<=TOLE.AND.DD22<=TOLE).OR.
     .                (DD21<=TOLE.AND.DD12<=TOLE)) ISTOP=L
               ENDDO
               IF (ISTOP/=0) THEN
                  IF (ITAG(K)==0) THEN
                     ITAG(K)=ICUR
                  ELSE
                     ICUR_OLD=ICUR
                     ICUR=ITAG(K)
                     DO L=1,NPOLB
                        IF (ITAG(L)==ICUR_OLD) ITAG(L)=ICUR
                     ENDDO
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
      ENDDO
C
      NPOLH=0
      DO I=1,ICMAX
         II=0
         DO J=1,NPOLB
            IF (ITAG(J)==I) II=II+1
         ENDDO
         IF (II/=0) NPOLH=NPOLH+1
      ENDDO
      IF (NPOLH>NPOLHMAX) THEN
         INFO=1
         NPOLHMAX=NPOLH
         RETURN
      ENDIF
C
      NPOLH=0
      DO I=1,ICMAX
         II=0
         DO J=1,NPOLB
            IF (ITAG(J)==I) II=II+1
         ENDDO
         IF (II/=0) THEN
            NPOLH=NPOLH+1
            POLH(1,NPOLH)=II
            POLH(2,NPOLH)=IBRIC
            II=0
            DO J=1,NPOLB
               IF (ITAG(J)==I) THEN
                  II=II+1
                  JJ=POLB(J)
                  POLH(2+II,NPOLH)=JJ
                  ITY=IPOLY(1,JJ)
                  IF (ITY==1) THEN
                     IPOLY(5,JJ)=NPOLH
                     CYCLE
                  ENDIF
                  IF (IPOLY(5,JJ)==0) THEN
                     IPOLY(5,JJ)=NPOLH
                  ELSE
                     IPOLY(6,JJ)=NPOLH
                  ENDIF
               ENDIF
            ENDDO
C Tri par ordre croissant de polh
            DO J=1,POLH(1,NPOLH)
               JMIN=J
               PMIN=POLH(2+J,NPOLH)
               DO K=J+1,POLH(1,NPOLH)
                  IF (POLH(2+K,NPOLH)<PMIN) THEN
                     JMIN=K
                     PMIN=POLH(2+K,NPOLH)
                  ENDIF
               ENDDO
               POLD=POLH(2+J,NPOLH)
               POLH(2+J,NPOLH)=PMIN
               POLH(2+JMIN,NPOLH)=POLD
            ENDDO
         ENDIF
      ENDDO
C
      INFO=0
      RETURN
      END
C
Chd|====================================================================
Chd|  COORLOC                       source/airbag/fvmesh.F        
Chd|-- called by -----------
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE COORLOC(VPX, VPY, VPZ, V1X, V1Y,
     .                   V1Z, V2X, V2Y, V2Z, KSI,
     .                   ETA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .        VPX, VPY, VPZ, V1X, V1Y,
     .        V1Z, V2X, V2Y, V2Z, KSI,
     .        ETA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .        SP1, SP2, S11, S22, S12, D
C
      SP1=VPX*V1X+VPY*V1Y+VPZ*V1Z
      SP2=VPX*V2X+VPY*V2Y+VPZ*V2Z
      S11=V1X*V1X+V1Y*V1Y+V1Z*V1Z
      S22=V2X*V2X+V2Y*V2Y+V2Z*V2Z
      S12=V1X*V2X+V1Y*V2Y+V1Z*V2Z
C
      D=S11*S22-S12*S12
C
      KSI=(SP1*S22-SP2*S12)/D
      ETA=(SP2*S11-SP1*S12)/D
C
      RETURN
      END
C
Chd|====================================================================
Chd|  GPOLCUT                       source/airbag/fvmesh.F        
Chd|-- called by -----------
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GPOLCUT(IPOLY , RPOLY  , IPOLY_OLD, RPOLY_OLD , INEDGE,
     .                   RNEDGE, NBNEDGE, NX       , NY        , NZ    , 
     .                   X0    , Y0     , Z0       , INS       , RNS   ,
     .                   NN    , NHOL   , INZ      , IZ        , NNS3  ,
     .                   NPOLY , NS     , IELNOD   , IELNOD_OLD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NN, NHOL, IPOLY(6+2*NN+1+NHOL,*), IPOLY_OLD(*), 
     .        INEDGE(6,*), NBNEDGE, INS(2,*), INZ, IZ(3,*), NNS3, 
     .        NPOLY, NS, IELNOD(2*NN,*), IELNOD_OLD(*)
      my_real
     .        RPOLY(4+3*2*NN+3*NHOL,*), RPOLY_OLD(*), RNEDGE(6,*), NX,
     .        NY, NZ, X0, Y0, Z0, RNS(4,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NN1, I, IADHOL(NHOL+1), NSEG, ITAG(2*NN+1), II,
     .        TSEG(3,2*NN), NHOL_OLD, ICUT, J, JJ, NSEG_INI, NSEG1,
     .        REDIR(NN), N1, N2, ISTOP, NNP, ICLOSE, ISEG,
     .        POLY(2*NN,NN), IN, IN1, IN2, LENPOLY(NN), K, KK,
     .        THOL(NHOL), JN, NHOLP, IADHOLP(NHOL), KN, I1, I2,
     .        ITAG2(2*NN), ITAGN(NN), ITAGS(NN), ISEG_OLD
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, ZL1, ZL2, ALPHA, NPX, NPY, NPZ,
     .        XP0, YP0, ZP0, VX, VY, VZ, XX, YY, ZZ, XL(NN), XLMIN,
     .        XLC, XX1, YY1, ZZ1, XX2, YY2, ZZ2, VX1, VY1, 
     .        VZ1, VX2, VY2, VZ2, NR1, NR2, SS, VVX, VVY, VVZ, 
     .        SS1, ZL, XNS(3,NN), TOLE, LL, ZLM
C
C
      TOLE = ZERO
      TOLE = EPSILON(TOLE)
C
C Liste des segments
      IF (NHOL==0) THEN
         NN1=NN
      ELSE
         NN1=IPOLY_OLD(6+NN+1+1)-1
         DO I=1,NHOL
            IADHOL(I)=IPOLY_OLD(6+NN+1+I)
         ENDDO
         IADHOL(NHOL+1)=NN+1
      ENDIF
      NS=0
      NSEG=0
C
      DO I=1,2*NN
         ITAG(I)=0
         ITAG2(I)=0
      ENDDO
      ITAG(2*NN+1)=0
      DO I=1,NN
         ITAGN(I)=0
         ITAGS(I)=0
      ENDDO
C Segments du contour exterieur
      DO I=1,NN1
         II=I+1
         IF (I==NN1) II=1
         X1=RPOLY_OLD(4+3*(I-1)+1)
         Y1=RPOLY_OLD(4+3*(I-1)+2)
         Z1=RPOLY_OLD(4+3*(I-1)+3)
         X2=RPOLY_OLD(4+3*(II-1)+1)
         Y2=RPOLY_OLD(4+3*(II-1)+2)
         Z2=RPOLY_OLD(4+3*(II-1)+3)
         ZL1=(X1-X0)*NX+(Y1-Y0)*NY+(Z1-Z0)*NZ
         ZL2=(X2-X0)*NX+(Y2-Y0)*NY+(Z2-Z0)*NZ
         IF (ZL1-ZL2/=ZERO) THEN
            ALPHA=ZL2/(ZL2-ZL1)
            IF (ALPHA>ZERO.AND.ALPHA<ONE) THEN
               NS=NS+1
               INS(1,NS)=IPOLY_OLD(6+I)
               INS(2,NS)=IPOLY_OLD(6+II)
               RNS(1,NS)=ALPHA
               XNS(1,NS)=ALPHA*X1+(ONE-ALPHA)*X2
               XNS(2,NS)=ALPHA*Y1+(ONE-ALPHA)*Y2
               XNS(3,NS)=ALPHA*Z1+(ONE-ALPHA)*Z2
               NSEG=NSEG+1
               TSEG(1,NSEG)=I
               TSEG(2,NSEG)=-NS
               TSEG(3,NSEG)=NSEG+1
               NSEG=NSEG+1
               TSEG(1,NSEG)=-NS
               TSEG(2,NSEG)=II
               TSEG(3,NSEG)=NSEG+1
            ELSEIF (ALPHA==ZERO) THEN
               IF (ITAGN(II)==0) THEN
                  NS=NS+1
                  ITAGN(II)=NS
                  INS(1,NS)=IPOLY_OLD(6+I)
                  INS(2,NS)=IPOLY_OLD(6+II)
                  RNS(1,NS)=ZERO
                  XNS(1,NS)=X2
                  XNS(2,NS)=Y2
                  XNS(3,NS)=Z2
                  ITAGS(NS)=1
                  NSEG=NSEG+1
                  TSEG(1,NSEG)=I
                  TSEG(2,NSEG)=-NS
                  TSEG(3,NSEG)=NSEG+1
                  NSEG=NSEG+1
                  TSEG(1,NSEG)=-NS
                  TSEG(2,NSEG)=II
                  TSEG(3,NSEG)=NSEG+1
               ELSE
                  NSEG=NSEG+1
                  TSEG(1,NSEG)=I
                  TSEG(2,NSEG)=II
                  TSEG(3,NSEG)=NSEG+1
               ENDIF
            ELSEIF (ALPHA==ONE) THEN
               IF (ITAGN(I)==0) THEN
                  NS=NS+1
                  ITAGN(I)=NS
                  INS(1,NS)=IPOLY_OLD(6+I)
                  INS(2,NS)=IPOLY_OLD(6+II)
                  RNS(1,NS)=ONE
                  XNS(1,NS)=X1
                  XNS(2,NS)=Y1
                  XNS(3,NS)=Z1
                  ITAGS(NS)=1
                  NSEG=NSEG+1
                  TSEG(1,NSEG)=I
                  TSEG(2,NSEG)=-NS
                  TSEG(3,NSEG)=NSEG+1
                  NSEG=NSEG+1
                  TSEG(1,NSEG)=-NS
                  TSEG(2,NSEG)=II
                  TSEG(3,NSEG)=NSEG+1
               ELSE
                  NSEG=NSEG+1
                  TSEG(1,NSEG)=I
                  TSEG(2,NSEG)=II
                  TSEG(3,NSEG)=NSEG+1
               ENDIF
            ELSE
               NSEG=NSEG+1
               TSEG(1,NSEG)=I
               TSEG(2,NSEG)=II
               TSEG(3,NSEG)=NSEG+1
            ENDIF
         ELSE
            NSEG=NSEG+1
            TSEG(1,NSEG)=I
            TSEG(2,NSEG)=II
            TSEG(3,NSEG)=NSEG+1
         ENDIF
      ENDDO
      TSEG(3,NSEG)=1
C Segments des trous
      NHOL_OLD=NHOL
      NHOL=0
      DO I=1,NHOL_OLD
         ICUT=0
         NSEG_INI=NSEG
         DO J=IADHOL(I),IADHOL(I+1)-1
            JJ=J+1
            IF (J==(IADHOL(I+1)-1)) JJ=IADHOL(I)
            X1=RPOLY_OLD(4+3*(J-1)+1)
            Y1=RPOLY_OLD(4+3*(J-1)+2)
            Z1=RPOLY_OLD(4+3*(J-1)+3)
            X2=RPOLY_OLD(4+3*(JJ-1)+1)
            Y2=RPOLY_OLD(4+3*(JJ-1)+2)
            Z2=RPOLY_OLD(4+3*(JJ-1)+3)
            ZL1=(X1-X0)*NX+(Y1-Y0)*NY+(Z1-Z0)*NZ
            ZL2=(X2-X0)*NX+(Y2-Y0)*NY+(Z2-Z0)*NZ
            IF (ZL1-ZL2/=ZERO) THEN
               ALPHA=ZL2/(ZL2-ZL1)
               IF (ALPHA>ZERO.AND.ALPHA<ONE) THEN
                  ICUT=1
                  NS=NS+1
                  INS(1,NS)=IPOLY_OLD(6+J)
                  INS(2,NS)=IPOLY_OLD(6+JJ)
                  RNS(1,NS)=ALPHA
                  XNS(1,NS)=ALPHA*X1+(ONE-ALPHA)*X2
                  XNS(2,NS)=ALPHA*Y1+(ONE-ALPHA)*Y2
                  XNS(3,NS)=ALPHA*Z1+(ONE-ALPHA)*Z2
                  NSEG=NSEG+1
                  TSEG(1,NSEG)=J
                  TSEG(2,NSEG)=-NS
                  TSEG(3,NSEG)=NSEG+1
                  NSEG=NSEG+1
                  TSEG(1,NSEG)=-NS
                  TSEG(2,NSEG)=JJ
                  TSEG(3,NSEG)=NSEG+1
               ELSEIF (ALPHA==ZERO) THEN
                  ICUT=1
                  IF (ITAGN(II)==0) THEN
                     NS=NS+1
                     ITAGN(JJ)=NS
                     INS(1,NS)=IPOLY_OLD(6+J)
                     INS(2,NS)=IPOLY_OLD(6+JJ)
                     RNS(1,NS)=ZERO
                     XNS(1,NS)=X2
                     XNS(2,NS)=Y2
                     XNS(3,NS)=Z2
                     ITAGS(NS)=1
                     NSEG=NSEG+1
                     TSEG(1,NSEG)=J
                     TSEG(2,NSEG)=-NS
                     TSEG(3,NSEG)=NSEG+1
                     NSEG=NSEG+1
                     TSEG(1,NSEG)=-NS
                     TSEG(2,NSEG)=JJ
                     TSEG(3,NSEG)=NSEG+1
                  ELSE
                     NSEG=NSEG+1
                     TSEG(1,NSEG)=J
                     TSEG(2,NSEG)=JJ
                     TSEG(3,NSEG)=NSEG+1
                  ENDIF
               ELSEIF (ALPHA==ONE) THEN
                  ICUT=1
                  IF (ITAGN(I)==0) THEN
                     NS=NS+1
                     ITAGN(J)=NS
                     INS(1,NS)=IPOLY_OLD(6+J)
                     INS(2,NS)=IPOLY_OLD(6+JJ)
                     RNS(1,NS)=ONE
                     XNS(1,NS)=X1
                     XNS(2,NS)=Y1
                     XNS(3,NS)=Z1
                     ITAGS(NS)=1
                     NSEG=NSEG+1
                     TSEG(1,NSEG)=J
                     TSEG(2,NSEG)=-NS
                     TSEG(3,NSEG)=NSEG+1
                     NSEG=NSEG+1
                     TSEG(1,NSEG)=-NS
                     TSEG(2,NSEG)=JJ
                     TSEG(3,NSEG)=NSEG+1
                  ELSE
                     NSEG=NSEG+1
                     TSEG(1,NSEG)=J
                     TSEG(2,NSEG)=JJ
                     TSEG(3,NSEG)=NSEG+1
                  ENDIF
               ELSE
                  NSEG=NSEG+1
                  TSEG(1,NSEG)=J
                  TSEG(2,NSEG)=JJ
                  TSEG(3,NSEG)=NSEG+1
               ENDIF
            ELSE
               NSEG=NSEG+1
               TSEG(1,NSEG)=J
               TSEG(2,NSEG)=JJ
               TSEG(3,NSEG)=NSEG+1
            ENDIF
         ENDDO
         TSEG(3,NSEG)=NSEG_INI+1
C
         IF (ICUT==0) THEN
C Le trou n'est pas coupe
            NHOL=NHOL+1
            DO J=NSEG_INI+1,NSEG
               ITAG(J)=-NHOL
            ENDDO
         ENDIF
      ENDDO
C
      NSEG1=NSEG
C Creation des nouvelles aretes dans le plan horizontal
      NPX=RPOLY_OLD(2)
      NPY=RPOLY_OLD(3)
      NPZ=RPOLY_OLD(4)
      XP0=RPOLY_OLD(5)
      YP0=RPOLY_OLD(6)
      ZP0=RPOLY_OLD(7)
      VX=NY*NPZ-NZ*NPY
      VY=NZ*NPX-NX*NPZ
      VZ=NX*NPY-NY*NPX
      DO I=1,NS
         XX=XNS(1,I)
         YY=XNS(2,I)
         ZZ=XNS(3,I)
         XL(I)=(XX-XP0)*VX+(YY-YP0)*VY+(ZZ-ZP0)*VZ
      ENDDO
      XLMIN=EP30
      DO I=1,NS
         REDIR(I)=I
      ENDDO
      DO I=1,NS
         XLMIN=XL(REDIR(I))
         DO J=I+1,NS
            XLC=XL(REDIR(J))
            IF (XLC<XLMIN) THEN
               JJ=REDIR(J)
               REDIR(J)=REDIR(I)
               REDIR(I)=JJ
               XLMIN=XLC
            ENDIF
         ENDDO
      ENDDO
      II=0
      DO I=1,NS
         II=II+1
         IF (II==1) THEN
            N1=REDIR(I)
            N2=REDIR(I+1)
            X1=XNS(1,N1)
            Y1=XNS(2,N1)
            Z1=XNS(3,N1)
            X2=XNS(1,N2)
            Y2=XNS(2,N2)
            Z2=XNS(3,N2)
            LL=SQRT((X1-X2)**2+(Y1-Y2)**2+(Z1-Z2)**2)
C
            IF (LL>TOLE) THEN
               NSEG=NSEG+1
               TSEG(1,NSEG)=-N1
               TSEG(2,NSEG)=-N2
               TSEG(3,NSEG)=-1
            ENDIF
         ELSE
            II=0
         ENDIF
      ENDDO
C Nouveaux polygones
      ISTOP=0
      NPOLY=1
      NNP=0
      DO WHILE (ISTOP==0)
         I=1
         DO WHILE (ITAG(I)/=0.AND.I<=NSEG) 
            I=I+1
         ENDDO
         IF (I==NSEG+1) THEN
            ISTOP=1
            CYCLE
         ENDIF
C
         ICLOSE=0
         ISEG=I
         DO WHILE (ICLOSE==0)
            NNP=NNP+1
            POLY(NNP,NPOLY)=TSEG(1,ISEG)
            IN=TSEG(2,ISEG)
            IF (TSEG(3,ISEG)>0) THEN
               ITAG(ISEG)=1
               IF (IN<0) THEN
                  ISEG_OLD=ISEG
                  ISEG=0
                  DO J=NSEG1+1,NSEG
                     IF (ITAG(J)/=0) CYCLE
                     IN1=TSEG(1,J)
                     IN2=TSEG(2,J)
                     IF (IN1==IN) THEN
                        ISEG=J
                     ELSEIF (IN2==IN) THEN
                        ISEG=J
                        TSEG(1,J)=IN2
                        TSEG(2,J)=IN1
                     ENDIF
                  ENDDO
                  IF (ISEG==0) ISEG=TSEG(3,ISEG_OLD)
               ELSE
                  ISEG=TSEG(3,ISEG)
               ENDIF
            ELSE
               IF (ITAG2(ISEG)==1) ITAG(ISEG)=1
               ITAG2(ISEG)=1
               ISEG=0
               DO J=1,NSEG1
                  IN1=TSEG(1,J)
                  IF (IN1==IN) ISEG=J
               ENDDO
            ENDIF
C
            IF (ITAG(ISEG)/=0) THEN
               ICLOSE=1
               LENPOLY(NPOLY)=NNP
               NPOLY=NPOLY+1
               NNP=0
            ENDIF
         ENDDO
      ENDDO
      NPOLY=NPOLY-1
C Attribution des trous
      DO I=1,NHOL
         J=1
         DO WHILE (ITAG(J)/=-I)
            J=J+1
         ENDDO
         N1=TSEG(1,J)
         XX=RPOLY_OLD(4+3*(N1-1)+1)
         YY=RPOLY_OLD(4+3*(N1-1)+2)
         ZZ=RPOLY_OLD(4+3*(N1-1)+3)
         DO J=1,NPOLY
            ALPHA=ZERO
            DO K=1,LENPOLY(J)
               KK=K+1
               IF (K==LENPOLY(J)) KK=1
               N1=POLY(K,I)
               N2=POLY(KK,I)
               IF (N1>0) THEN
                  XX1=RPOLY_OLD(4+3*(N1-1)+1)      
                  YY1=RPOLY_OLD(4+3*(N1-1)+2)      
                  ZZ1=RPOLY_OLD(4+3*(N1-1)+3)
               ELSE
                  XX1=XNS(1,-N1)
                  YY1=XNS(2,-N1)
                  ZZ1=XNS(3,-N1)
               ENDIF
               IF (N2>0) THEN
                  XX2=RPOLY_OLD(4+3*(N2-1)+1)      
                  YY2=RPOLY_OLD(4+3*(N2-1)+2)      
                  ZZ2=RPOLY_OLD(4+3*(N2-1)+3)
               ELSE
                  XX2=XNS(1,-N2)
                  YY2=XNS(2,-N2)
                  ZZ2=XNS(3,-N2)
               ENDIF
               VX1=XX1-XX
               VY1=YY1-YY
               VZ1=ZZ1-ZZ
               VX2=XX2-XX
               VY2=YY2-YY
               VZ2=ZZ2-ZZ
               NR1=SQRT(VX1**2+VY1**2+VZ1**2)
               NR2=SQRT(VX2**2+VY2**2+VZ2**2)
               VX1=VX1/NR1
               VY1=VY1/NR1
               VZ1=VZ1/NR1
               VX2=VX2/NR2
               VY2=VY2/NR2
               VZ2=VZ2/NR2
               SS=VX1*VX2+VY1*VY2+VZ1*VZ2
               VVX=VY1*VZ2-VZ1*VY2
               VVY=VZ1*VX2-VX1*VZ2
               VVZ=VX1*VY2-VY1*VX2
               SS1=NPX*VVX+NPY*VVY+NPZ*VVZ
               IF (SS1>=ZERO) THEN
                  ALPHA=ALPHA+ACOS(SS)
               ELSE
                  ALPHA=ALPHA-ACOS(SS)
               ENDIF
            ENDDO
C
            IF (ABS(ALPHA)>=ONE) THOL(I)=J
         ENDDO
      ENDDO
C Creation des tableaux de sortie
      DO I=1,NS
         RNS(2,I)=XNS(1,I)
         RNS(3,I)=XNS(2,I)
         RNS(4,I)=XNS(3,I)
      ENDDO
C
      DO I=1,NPOLY
         IPOLY(1,I)=IPOLY_OLD(1)
         IPOLY(3,I)=IPOLY_OLD(3)
         IPOLY(4,I)=IPOLY_OLD(4)
         IPOLY(5,I)=IPOLY_OLD(5)
         IPOLY(6,I)=IPOLY_OLD(6)
         RPOLY(1,I)=ZERO
         RPOLY(2,I)=NPX
         RPOLY(3,I)=NPY
         RPOLY(4,I)=NPZ
         NNP=0
         DO J=1,LENPOLY(I)
            JN=POLY(J,I)
            IF (JN>0) THEN
               NNP=NNP+1
               IPOLY(6+NNP,I)=IPOLY_OLD(6+JN)
               RPOLY(4+3*(NNP-1)+1,I)=RPOLY_OLD(4+3*(JN-1)+1)
               RPOLY(4+3*(NNP-1)+2,I)=RPOLY_OLD(4+3*(JN-1)+2)
               RPOLY(4+3*(NNP-1)+3,I)=RPOLY_OLD(4+3*(JN-1)+3)
               IELNOD(NNP,I)=IELNOD_OLD(JN)
            ELSE
               IF (ITAGS(-JN)==0) THEN
                  NNP=NNP+1
                  IPOLY(6+NNP,I)=-NNS3+JN
                  RPOLY(4+3*(NNP-1)+1,I)=XNS(1,-JN)
                  RPOLY(4+3*(NNP-1)+2,I)=XNS(2,-JN)
                  RPOLY(4+3*(NNP-1)+3,I)=XNS(3,-JN)
                  IELNOD(NNP,I)=-1
               ELSE
                  JJ=POLY(J+1,I)
                  IF (J==LENPOLY(I)) JJ=POLY(1,I)
                  X1=XNS(1,-JN)
                  Y1=XNS(2,-JN)
                  Z1=XNS(3,-JN)
                  IF (JJ>0) THEN
                     X2=RPOLY_OLD(4+3*(JJ-1)+1)
                     Y2=RPOLY_OLD(4+3*(JJ-1)+2)
                     Z2=RPOLY_OLD(4+3*(JJ-1)+3)
                  ELSE
                     X2=XNS(1,-JJ)
                     Y2=XNS(2,-JJ)
                     Z2=XNS(3,-JJ)
                  ENDIF
                  LL=SQRT((X1-X2)**2+(Y1-Y2)**2+(Z1-Z2)**2)
                  IF (LL>TOLE) THEN
                     NNP=NNP+1
                     IPOLY(6+NNP,I)=-NNS3+JN
                     RPOLY(4+3*(NNP-1)+1,I)=XNS(1,-JN)
                     RPOLY(4+3*(NNP-1)+2,I)=XNS(2,-JN)
                     RPOLY(4+3*(NNP-1)+3,I)=XNS(3,-JN)
                     IELNOD(NNP,I)=-1
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
C
         NHOLP=0
         
         DO J=1,NHOL
            IF (THOL(J)==I) THEN
               NHOLP=NHOLP+1
               IADHOLP(NHOLP)=NNP+1
               DO K=1,NSEG
                  IF (TSEG(3,K)==-J) THEN
                     NNP=NNP+1
                     KN=TSEG(1,K)
                     IPOLY(6+NNP,I)=KN
                     RPOLY(4+3*(NNP-1)+1,I)=RPOLY_OLD(4+3*(KN-1)+1)
                     RPOLY(4+3*(NNP-1)+2,I)=RPOLY_OLD(4+3*(KN-1)+2)
                     RPOLY(4+3*(NNP-1)+3,I)=RPOLY_OLD(4+3*(KN-1)+3)
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
         IPOLY(2,I)=NNP
         IPOLY(6+NNP+1,I)=NHOLP
         DO J=1,NHOLP
            IPOLY(6+NNP+1+J,I)=IADHOLP(J)
         ENDDO
      ENDDO
C
      DO I=NSEG1+1,NSEG
         NBNEDGE=NBNEDGE+1
         N1=-TSEG(1,I)
         N2=-TSEG(2,I)
         INEDGE(1,NBNEDGE)=IPOLY_OLD(1)
         INEDGE(2,NBNEDGE)=NNS3+N1
         INEDGE(3,NBNEDGE)=NNS3+N2
         INEDGE(4,NBNEDGE)=IPOLY_OLD(3)
         INEDGE(5,NBNEDGE)=IPOLY_OLD(4)
         INEDGE(6,NBNEDGE)=INZ
C
         XX1=XNS(1,N1)
         YY1=XNS(2,N1)
         ZZ1=XNS(3,N1)
C
         XX2=XNS(1,N2)
         YY2=XNS(2,N2)
         ZZ2=XNS(3,N2)
C
         RNEDGE(1,NBNEDGE)=XX1         
         RNEDGE(2,NBNEDGE)=YY1         
         RNEDGE(3,NBNEDGE)=ZZ1         
         RNEDGE(4,NBNEDGE)=XX2         
         RNEDGE(5,NBNEDGE)=YY2         
         RNEDGE(6,NBNEDGE)=ZZ2
      ENDDO
C
      DO I=1,NPOLY
         ZL=ZERO
         ZLM=ZERO
         DO J=1,IPOLY(2,I)
            XX=RPOLY(4+3*(J-1)+1,I)
            YY=RPOLY(4+3*(J-1)+2,I)       
            ZZ=RPOLY(4+3*(J-1)+3,I)
            ZL=(XX-X0)*NX+(YY-Y0)*NY+(ZZ-Z0)*NZ
            IF (ABS(ZL)>ABS(ZLM)) ZLM=ZL
         ENDDO
         IZ(1,I)=1
         IF (ZLM>ZERO) THEN
            IZ(2,I)=INZ+1
         ELSEIF (ZLM<ZERO) THEN
            IZ(2,I)=INZ
         ENDIF
      ENDDO     
C 
      RETURN
      END
C
Chd|====================================================================
Chd|  HORIEDGE                      source/airbag/fvmesh.F        
Chd|-- called by -----------
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE HORIEDGE(IPOLY  , RPOLY , NX    , NY  , NZ  ,
     .                    NBNEDGE, INEDGE, RNEDGE, X0  , Y0  ,
     .                    Z0     , INZ   , NNS3  , NREF, AREF,
     .                    NNSP   ) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPOLY(*), NBNEDGE, INEDGE(6,*), INZ, NNS3, NREF(2,*), NNSP
      my_real
     .        RPOLY(*), NX, NY, NZ, RNEDGE(6,*), X0, Y0, Z0, AREF(4,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NN, I, II
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, ZL1, ZL2, TOLE, DD 
C
*      TOLE=DLAMCH('Epsmach')
      TOLE=EM10   
      NN=IPOLY(2)
      NNSP=0
      DO I=1,NN
         II=I+1
         IF (I==NN) II=1
         X1=RPOLY(4+3*(I-1)+1)
         Y1=RPOLY(4+3*(I-1)+2)
         Z1=RPOLY(4+3*(I-1)+3)
         X2=RPOLY(4+3*(II-1)+1)
         Y2=RPOLY(4+3*(II-1)+2)
         Z2=RPOLY(4+3*(II-1)+3)
         DD=(X1-X2)**2+(Y1-Y2)**2+(Z1-Z2)**2
         ZL1=(X1-X0)*NX+(Y1-Y0)*NY+(Z1-Z0)*NZ
         ZL2=(X2-X0)*NX+(Y2-Y0)*NY+(Z2-Z0)*NZ
         IF (ZL1==ZERO.AND.ZL2==ZERO.AND.DD>=TOLE) THEN
            NBNEDGE=NBNEDGE+1
C
            NNSP=NNSP+1
            NREF(1,NNSP)=IPOLY(6+I)
            NREF(2,NNSP)=IPOLY(6+II)
            AREF(1,NNSP)=ONE
            AREF(2,NNSP)=X1
            AREF(3,NNSP)=Y1
            AREF(4,NNSP)=Z1
            NNSP=NNSP+1
            NREF(1,NNSP)=IPOLY(6+I)
            NREF(2,NNSP)=IPOLY(6+II)
            AREF(1,NNSP)=ZERO
            AREF(2,NNSP)=X2
            AREF(3,NNSP)=Y2
            AREF(4,NNSP)=Z2
C
            INEDGE(1,NBNEDGE)=IPOLY(1)
            INEDGE(2,NBNEDGE)=NNS3+NNSP-1
            INEDGE(3,NBNEDGE)=NNS3+NNSP
            INEDGE(4,NBNEDGE)=IPOLY(3)
            INEDGE(5,NBNEDGE)=IPOLY(4)
            INEDGE(6,NBNEDGE)=INZ
C
            RNEDGE(1,NBNEDGE)=RPOLY(4+3*(I-1)+1)    
            RNEDGE(2,NBNEDGE)=RPOLY(4+3*(I-1)+2)      
            RNEDGE(3,NBNEDGE)=RPOLY(4+3*(I-1)+3)     
            RNEDGE(4,NBNEDGE)=RPOLY(4+3*(II-1)+1)      
            RNEDGE(5,NBNEDGE)=RPOLY(4+3*(II-1)+2)      
            RNEDGE(6,NBNEDGE)=RPOLY(4+3*(II-1)+3)
         ENDIF
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  HORIPOLY                      source/airbag/fvmesh.F        
Chd|-- called by -----------
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE HORIPOLY(INEDGE, RNEDGE, LEDGE , NEDGE, IPOLY,
     .                    RPOLY , IZ    , IELNOD, NPOLY, NX   ,
     .                    NY    , NZ    , INZ   , IBRIC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER INEDGE(6,*), LEDGE(*), NEDGE, IPOLY(6+2*NEDGE+1+NEDGE,*),
     .        IZ(3,*), IELNOD(NEDGE,*), NPOLY, INZ, IBRIC
      my_real
     .        RNEDGE(6,*), RPOLY(4+6*NEDGE+3*NEDGE,*), NX, NY, NZ
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NN, I, II, TNOD(2*NEDGE), TSEG(2,NEDGE), NN_OLD, JFOUND,
     .        J, JJ, REDIR1(2*NEDGE), REDIR2(2*NEDGE), ITAG(2*NEDGE),
     .        ITAGSEG(NEDGE+1), ISTOP, ICLOSE, N1, N2, IN, NNP, 
     .        POLY(2*NEDGE,NEDGE), ISEG, LENPOLY(NEDGE), IFOUND,
     .        IADHOL(NEDGE), NHOL, K, KK, IPOUT, M, MM, NSEC, KSMIN,
     .        NEDGE_OLD, TSEG_OLD(2,NEDGE)
      my_real
     .        TOLE, XNOD(3,2*NEDGE), XX1, YY1, ZZ1, XX2, YY2, ZZ2,
     .        DD, XLOC(2,NEDGE), XSEC(NEDGE), PHOL(3,NEDGE), ALPHA,
     .        X1, Y1, Z1, VX1, VY1, VZ1, VX2, VY2, VZ2, NR1, NR2,
     .        SS, VVX, VVY, VVZ, SS1, X2, Y2, Z2, YLMIN, YLMAX, XSMIN1,
     .        XSMIN2, XX, YY, ZZ, YLSEC, XS, YS, LMAX, LL
C
      my_real
     .        DLAMCH
      EXTERNAL DLAMCH
C
*      TOLE=DLAMCH('Epsmach')
      TOLE=EM10   
C Creation de la liste des noeuds
      NN=0
      LMAX=ZERO
      DO I=1,NEDGE
         II=LEDGE(I)
         NN=NN+1
         TNOD(NN)=INEDGE(2,II)
         XNOD(1,NN)=RNEDGE(1,II)
         XNOD(2,NN)=RNEDGE(2,II)
         XNOD(3,NN)=RNEDGE(3,II)
         TSEG(1,I)=NN
         NN=NN+1
         TNOD(NN)=INEDGE(3,II)  
         XNOD(1,NN)=RNEDGE(4,II)
         XNOD(2,NN)=RNEDGE(5,II)
         XNOD(3,NN)=RNEDGE(6,II)
         TSEG(2,I)=NN
         LL=(RNEDGE(1,II)-RNEDGE(4,II))**2+
     .      (RNEDGE(2,II)-RNEDGE(5,II))**2+
     .      (RNEDGE(3,II)-RNEDGE(6,II))**2
         LMAX=MAX(LMAX,LL)
      ENDDO
*      TOLE=TOLE*LMAX
C Elimination des noeuds doubles
      NN_OLD=NN
      NN=0
      DO I=1,NN_OLD
         XX1=XNOD(1,I)
         YY1=XNOD(2,I)
         ZZ1=XNOD(3,I)
         JFOUND=0
         DO J=1,NN
            JJ=REDIR1(J)
            XX2=XNOD(1,JJ)
            YY2=XNOD(2,JJ)
            ZZ2=XNOD(3,JJ)
            DD=SQRT((XX1-XX2)**2+(YY1-YY2)**2+(ZZ1-ZZ2)**2)
            IF (DD<=TOLE) JFOUND=J
         ENDDO
         IF (JFOUND==0) THEN
            NN=NN+1
            REDIR1(NN)=I
            REDIR2(I)=NN
         ELSE
            REDIR2(I)=JFOUND
         ENDIF
      ENDDO
C
      DO I=1,NEDGE
         N1=TSEG(1,I)
         N2=TSEG(2,I)
         TSEG(1,I)=REDIR2(N1)
         TSEG(2,I)=REDIR2(N2)
      ENDDO
C Elimination des segments de longueur nulle
      NEDGE_OLD=NEDGE
      NEDGE=0
      DO I=1,NEDGE_OLD
         TSEG_OLD(1,I)=TSEG(1,I)
         TSEG_OLD(2,I)=TSEG(2,I)
      ENDDO
      DO I=1,NEDGE_OLD
         IF (TSEG_OLD(1,I)/=TSEG_OLD(2,I)) THEN
            NEDGE=NEDGE+1
            TSEG(1,NEDGE)=TSEG_OLD(1,I)
            TSEG(2,NEDGE)=TSEG_OLD(2,I)
         ENDIF
      ENDDO
C Construction des polygones
      DO I=1,NN
         ITAG(I)=0
      ENDDO
      DO I=1,NEDGE+1
         ITAGSEG(I)=0
      ENDDO
      NPOLY=1
      ISTOP=0
      DO WHILE (ISTOP==0)
         I=1
         DO WHILE (ITAGSEG(I)==1.AND.I<=NEDGE)
            I=I+1
         ENDDO
         IF (I==NEDGE+1) THEN
            ISTOP=1
            CYCLE
         ENDIF
C
         ICLOSE=0
         ISEG=I
         ITAGSEG(ISEG)=1
         N1=TSEG(1,ISEG)
         N2=TSEG(2,ISEG)
         ITAG(N1)=1
         ITAG(N2)=1
         IN=N2
         NNP=1
         POLY(1,NPOLY)=REDIR1(N1)
         DO WHILE (ICLOSE==0)
            IFOUND=0
            I=0
            DO WHILE (IFOUND==0)
               I=I+1
               IF (ITAGSEG(I)==1) CYCLE
               N1=TSEG(1,I)
               N2=TSEG(2,I)
               IF (N1==IN) THEN
                  IFOUND=1
                  IF (ITAG(N2)==1) ICLOSE=1
                  ISEG=I
                  IN=N2
                  NNP=NNP+1
                  POLY(NNP,NPOLY)=REDIR1(N1)
                  ITAG(N2)=1
               ELSEIF (N2==IN) THEN
                  IFOUND=1
                  IF (ITAG(N1)==1) ICLOSE=1
                  ISEG=I
                  IN=N1
                  NNP=NNP+1
                  POLY(NNP,NPOLY)=REDIR1(N2)
                  ITAG(N1)=1
               ENDIF
            ENDDO
            ITAGSEG(ISEG)=1
         ENDDO
C
         IF (ICLOSE==1) THEN
            LENPOLY(NPOLY)=NNP
            NPOLY=NPOLY+1
         ENDIF
      ENDDO
      NPOLY=NPOLY-1
C Creation des tableaux de sortie
      DO I=1,NPOLY
         IPOLY(1,I)=2
         IPOLY(2,I)=LENPOLY(I)
         IPOLY(3,I)=IBRIC
         IPOLY(4,I)=IBRIC
         IPOLY(5,I)=0
         IPOLY(6,I)=0
         RPOLY(1,I)=ZERO
         RPOLY(2,I)=NX
         RPOLY(3,I)=NY
         RPOLY(4,I)=NZ
         DO J=1,LENPOLY(I)
            JJ=POLY(J,I)
            IPOLY(6+J,I)=-TNOD(JJ)
            RPOLY(4+3*(J-1)+1,I)=XNOD(1,JJ)
            RPOLY(4+3*(J-1)+2,I)=XNOD(2,JJ)
            RPOLY(4+3*(J-1)+3,I)=XNOD(3,JJ)
            IELNOD(J,I)=-1
         ENDDO
         IPOLY(6+LENPOLY(I)+1,I)=0
C
         IZ(1,I)=2
         IZ(2,I)=INZ
         IZ(3,I)=INZ+1
      ENDDO
C
C Recherche des trous
      DO I=1,NPOLY
C
         NHOL=0
         DO J=1,NPOLY
            IADHOL(J)=0
         ENDDO
C
         DO J=1,NPOLY
            IF (I==J) CYCLE
            ALPHA=ZERO
            X1=RPOLY(5,J)
            Y1=RPOLY(6,J)
            Z1=RPOLY(7,J)
            DO K=1,LENPOLY(I)
               KK=K+1
               IF (K==LENPOLY(I)) KK=1
               XX1=RPOLY(4+3*(K-1)+1,I)
               YY1=RPOLY(4+3*(K-1)+2,I)
               ZZ1=RPOLY(4+3*(K-1)+3,I)
               XX2=RPOLY(4+3*(KK-1)+1,I)
               YY2=RPOLY(4+3*(KK-1)+2,I)
               ZZ2=RPOLY(4+3*(KK-1)+3,I)
               VX1=XX1-X1
               VY1=YY1-Y1
               VZ1=ZZ1-Z1
               VX2=XX2-X1
               VY2=YY2-Y1
               VZ2=ZZ2-Z1
               NR1=SQRT(VX1**2+VY1**2+VZ1**2)
               NR2=SQRT(VX2**2+VY2**2+VZ2**2)
               VX1=VX1/NR1
               VY1=VY1/NR1
               VZ1=VZ1/NR1
               VX2=VX2/NR2
               VY2=VY2/NR2
               VZ2=VZ2/NR2
               SS=VX1*VX2+VY1*VY2+VZ1*VZ2
               VVX=VY1*VZ2-VZ1*VY2
               VVY=VZ1*VX2-VX1*VZ2
               VVZ=VX1*VY2-VY1*VX2
               SS1=NX*VVX+NY*VVY+NZ*VVZ
               IF (SS1>=ZERO) THEN
                  ALPHA=ALPHA+ACOS(SS)
               ELSE
                  ALPHA=ALPHA-ACOS(SS)
               ENDIF
            ENDDO
C
            IF (ABS(ALPHA)>=ONE) THEN
C Le premier point du polygone i est a l'interieur du polygone j
C On teste tous les autres
               IPOUT=0
               DO K=2,LENPOLY(J)
                  X2=RPOLY(4+3*(K-1)+1,J)
                  Y2=RPOLY(4+3*(K-1)+2,J)
                  Z2=RPOLY(4+3*(K-1)+3,J)
                  ALPHA=ZERO
                  DO M=1,LENPOLY(I)
                     MM=M+1
                     IF (M==LENPOLY(I)) MM=1
                     XX1=RPOLY(4+3*(M-1)+1,I)
                     YY1=RPOLY(4+3*(M-1)+2,I)
                     ZZ1=RPOLY(4+3*(M-1)+3,I)
                     XX2=RPOLY(4+3*(MM-1)+1,I)
                     YY2=RPOLY(4+3*(MM-1)+2,I)
                     ZZ2=RPOLY(4+3*(MM-1)+3,I)
                     VX1=XX1-X1
                     VY1=YY1-Y1
                     VZ1=ZZ1-Z1
                     VX2=XX2-X1
                     VY2=YY2-Y1
                     VZ2=ZZ2-Z1
                     NR1=SQRT(VX1**2+VY1**2+VZ1**2)
                     NR2=SQRT(VX2**2+VY2**2+VZ2**2)
                     VX1=VX1/NR1
                     VY1=VY1/NR1
                     VZ1=VZ1/NR1
                     VX2=VX2/NR2
                     VY2=VY2/NR2
                     VZ2=VZ2/NR2
                     SS=VX1*VX2+VY1*VY2+VZ1*VZ2
                     VVX=VY1*VZ2-VZ1*VY2
                     VVY=VZ1*VX2-VX1*VZ2
                     VVZ=VX1*VY2-VY1*VX2
                     SS1=NX*VVX+NY*VVY+NZ*VVZ
                     IF (SS1>=ZERO) THEN
                        ALPHA=ALPHA+ACOS(SS)
                     ELSE
                        ALPHA=ALPHA-ACOS(SS)
                     ENDIF
                  ENDDO
                  IF (ABS(ALPHA)<ONE) IPOUT=1
               ENDDO
C
               IF (IPOUT==1) CYCLE
C Le polygone j est un trou dans le polygone i
               IPOLY(1,J)=-1
               NHOL=NHOL+1
               IADHOL(NHOL)=LENPOLY(I)
               DO K=1,LENPOLY(J)
                  IPOLY(6+IADHOL(NHOL)+K,I)=IPOLY(6+K,J)
                  IELNOD(IADHOL(NHOL)+K,I)=IELNOD(K,J)
                  RPOLY(4+3*IADHOL(NHOL)+3*(K-1)+1,I)=
     .                                   RPOLY(4+3*(K-1)+1,J)
                  RPOLY(4+3*IADHOL(NHOL)+3*(K-1)+2,I)=
     .                                   RPOLY(4+3*(K-1)+2,J)
                  RPOLY(4+3*IADHOL(NHOL)+3*(K-1)+3,I)=
     .                                   RPOLY(4+3*(K-1)+3,J)
               ENDDO
               LENPOLY(I)=LENPOLY(I)+LENPOLY(J)
C Point interieur polygone j
               VX1=RPOLY(5,J)-RPOLY(8,J)
               VY1=RPOLY(6,J)-RPOLY(9,J)
               VZ1=RPOLY(7,J)-RPOLY(10,J)
               SS=SQRT(VX1**2+VY1**2+VZ1**2)
               VX1=VX1/SS
               VY1=VY1/SS
               VZ1=VZ1/SS
               VX2=NY*VZ1-NZ*VY1
               VY2=NZ*VX1-NX*VZ1
               VZ2=NX*VY1-NY*VX1
               X1=RPOLY(5,J)
               Y1=RPOLY(6,J)
               Z1=RPOLY(7,J)
               XLOC(1,1)=ZERO
               XLOC(2,1)=ZERO
               YLMIN=EP30
               YLMAX=-EP30
               DO K=2,LENPOLY(J)
                  XX=RPOLY(4+3*(K-1)+1,J)
                  YY=RPOLY(4+3*(K-1)+2,J)
                  ZZ=RPOLY(4+3*(K-1)+3,J)
                  VVX=XX-X1
                  VVY=YY-Y1
                  VVZ=ZZ-Z1
                  XLOC(1,K)=VVX*VX1+VVY*VY1+VVZ*VZ1
                  XLOC(2,K)=VVX*VX2+VVY*VY2+VVZ*VZ2
                  IF (XLOC(2,K)<YLMIN) YLMIN=XLOC(2,K)
                  IF (XLOC(2,K)>YLMAX) YLMAX=XLOC(2,K)
               ENDDO
               YLSEC=HALF*(YLMIN+YLMAX)
C
               NSEC=0
               DO K=1,LENPOLY(J)
                  KK=K+1
                  IF (K==LENPOLY(J)) KK=1
                  X1=XLOC(1,K)
                  Y1=XLOC(2,K)
                  X2=XLOC(1,KK)
                  Y2=XLOC(2,KK)
                  IF (Y1-Y2/=ZERO) THEN
                     ALPHA=(YLSEC-Y2)/(Y1-Y2)
                     IF (ALPHA>=ZERO.AND.ALPHA<=ONE) THEN
                        NSEC=NSEC+1
                        XSEC(NSEC)=ALPHA*X1+(ONE-ALPHA)*X2
                     ENDIF
                  ELSE
                     IF (Y1==YLSEC) THEN
                        NSEC=NSEC+1
                        XSEC(NSEC)=X1
                        NSEC=NSEC+1
                        XSEC(NSEC)=X2
                     ENDIF
                  ENDIF
               ENDDO
C
               XSMIN1=EP30
               DO K=1,NSEC
                  IF (XSEC(K)<XSMIN1) THEN
                     XSMIN1=XSEC(K)
                     KSMIN=K
                  ENDIF
               ENDDO
               XSMIN2=EP30
               DO K=1,NSEC
                  IF (K==KSMIN) CYCLE
                  IF (XSEC(K)<XSMIN2) XSMIN2=XSEC(K)
               ENDDO
C
               XS=HALF*(XSMIN1+XSMIN2)
               YS=YLSEC
               PHOL(1,NHOL)=RPOLY(5,J)+XS*VX1+YS*VX2
               PHOL(2,NHOL)=RPOLY(6,J)+XS*VY1+YS*VY2
               PHOL(3,NHOL)=RPOLY(7,J)+XS*VZ1+YS*VZ2
            ENDIF
         ENDDO
C
         IPOLY(2,I)=LENPOLY(I)
         IPOLY(6+LENPOLY(I)+1,I)=NHOL
         DO J=1,NHOL
            IPOLY(6+LENPOLY(I)+1+J,I)=IADHOL(J)
            RPOLY(4+3*LENPOLY(I)+3*(J-1)+1,I)=PHOL(1,J)
            RPOLY(4+3*LENPOLY(I)+3*(J-1)+2,I)=PHOL(2,J)
            RPOLY(4+3*LENPOLY(I)+3*(J-1)+3,I)=PHOL(3,J)
         ENDDO
      ENDDO
C
      RETURN
      END

