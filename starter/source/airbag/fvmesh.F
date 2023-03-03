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
Chd|        ANCMSG                        source/output/message/message.F
Chd|        COORLOC                       source/airbag/fvmesh.F        
Chd|        C_TRICALL                     stub/fvmbags_stub.F           
Chd|        FACEPOLY                      source/airbag/facepoly.F      
Chd|        FVNORMAL                      source/airbag/fvmbag1.F       
Chd|        GPOLCUT                       source/airbag/gpolcut.F       
Chd|        HORIEDGE                      source/airbag/horipoly.F      
Chd|        HORIPOLY                      source/airbag/horipoly.F      
Chd|        ITRIBOX                       source/airbag/itribox.F       
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        POLYHEDR                      source/airbag/polyhedr.F      
Chd|        POLYHEDR1                     source/airbag/polyhedr1.F     
Chd|        TRIBOX3                       stub/fvmbags_stub.F           
Chd|        TRITRI3                       stub/fvmbags_stub.F           
Chd|        FVBAG_MOD                     share/modules1/fvbag_mod.F    
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        REORDER_MOD                   share/modules1/reorder_mod.F  
Chd|====================================================================
      SUBROUTINE FVMESH1(IBUF , ELEM  , X     , IVOLU, BRIC ,
     .                   XB   , RVOLU , NEL   , NELI,  NBRIC, TBRIC,
     .                   SFAC , DXM   , NBA   , NELA , NNA  ,
     .                   TBA  , TFACA , TAGELS, IBUFA,
     .                   ELEMA, TAGELA, IXS   ,ID    ,TITR, NB_NODE, ITYP)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE MESSAGE_MOD
      USE FVBAG_MOD
      USE REORDER_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBUF(*), ELEM(3,*), IVOLU(*), BRIC(8,*),
     .        NEL, NELI, NBRIC, TBRIC(13,*), NBA, NELA, NNA, TBA(2,*),
     .        TFACA(12,*), TAGELS(*), IBUFA(*), ELEMA(3,*), TAGELA(*),
     .        IXS(NIXS,*), NB_NODE, ITYP
      my_real
     .        X(3,*), XB(3,*), RVOLU(*), SFAC(6,4,*), DXM
      INTEGER ID
      CHARACTER*nchartitle,
     .        TITR
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
     .        ITAGBA(NBA), IBSA(NBA), ITAGN(NB_NODE), NALL, III,
     .        NNS_ANIM, NBZ, NBNEDGE, NNS3, NPOLY_NEW, NNSP, NEDGE,
     .        ITYZ, INZ, J0, NNS1, K0, I1, I2, IDEP, NSTR, NCTR, IIZ,
     .        NNS_OLD_SAVE,ITYZINT,IAD1, IAD2, ITMP1, ITMP2
      INTEGER, DIMENSION(:), ALLOCATABLE :: IFLAG
      PARAMETER (NVMAX=12)
      my_real
     .        X1, Y1, Z1, X2, Y2, Z2, X3, Y3, Z3, X12, Y12, Z12, 
     .        X13, Y13, Z13, NRX, NRY, NRZ, AREA2, ELAREA(NEL+NELI),
     .        NORM(3,NEL+NELI), XBMAX, YBMAX, ZBMAX, XBMIN, YBMIN, ZBMIN,
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
     .        CVI, RHOI, ZL1, ZL2, ZL3, ZLC, VAL, TMASS,
     .        TI2, CPDI, CPEI, CPFI, R1, RTMP1, RTMP2

      INTEGER :: COUNT_ELEM_INT, COUNT_ELEM_EXT, COUNT_TRIA_INTERNE
      INTEGER :: COUNT_ELEM_INT_OLD, COUNT_ELEM_EXT_OLD, COUNT_TRIA_INTERNE_OLD
C
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
     .                        IZ(:,:), LEDGE(:), IFVNOD2(:,:)
      my_real
     .       , ALLOCATABLE :: NORMT(:,:), POLY(:,:), RPOLY(:,:),
     .                        RPOLY_F(:,:), VOLU(:), PNODES(:,:),
     .                        PHOLES(:,:), RPOLY_OLD(:), VOLUSORT(:),
     .                        VOLU_OLD(:), RPOLY_F_OLD(:,:),
     .                        RFVNOD_OLD(:,:), XNEW(:,:), RNEDGE(:,:),
     .                        AREF(:,:), RFVNOD2(:,:), XNS(:,:), 
     .                        XNS2(:,:), AREATR(:)
C
      INTEGER FAC(4,6), FACNOR(4,6), FAC4(3,4), FAC6(4,5)
      INTEGER FAC5(4,5), NFACE(4), NTYPE, IQUAD
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
      DATA FAC6 /1,3,2,0,
     .           5,6,7,0,
     .           1,2,6,5,
     .           2,3,7,6,
     .           3,4,8,7/
      DATA FAC5 /1,2,5,0,
     .           2,3,5,0,
     .           3,4,5,0,
     .           4,1,5,0,
     .           1,4,3,2/
      DATA NFACE/6,4,5,5/
C-------------------------
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
C liste chainee polyedres
      TYPE POLYHEDRE
         INTEGER RANK
         INTEGER, DIMENSION(:), POINTER :: POLH
         TYPE(POLYHEDRE), POINTER :: PTR
      END TYPE POLYHEDRE
      TYPE(POLYHEDRE), POINTER :: PHFIRST, PH_PREC, PH_CUR, PH_TMP

      my_real, DIMENSION(:), ALLOCATABLE :: RADIUS
      INTEGER, DIMENSION(:), ALLOCATABLE :: IDX
C
C Structure de donnees maillage volumes finis
C    - NNS   : Nombre de nouveaux noeuds ( = IVOLU(46) )
C    - NNTR  : Nombre de nouveaux triangles ( = IVOLU(47) )
C    - NPOLY : Nombre de polygones ( = IVOLU(48) )
C    - NPOLH : Nombre de polyedres ( volumes finis) ( = IVOLU(49) )
C
C    - IFVNOD(NNS)   : Triangle airbag de reference pour le nouveau noeud
C    - RFVNOD(2,NNS) : Coordonnees locales du noeud dans le triangle
C
C    - IFVTRI(6,NNTR) : Connectivite nouveaux triangles (1->3)
C                       Element de reference si appuye sur surface (4)
C                       Polyedres connectes si interne (5->6)
C
C    - IFVPOLY(NNTR)    : Attribution triangles-polygones
C    - IFVTADR(NPOLY+1) : Adresses dans le tableau precedent
C
C    - IFVPOLH(NPOLY)   : Attribution polygones-polyedres
C    - IFVPADR(NPOLH+1) : Adresses dans le tableau precedent
C
C
      NULLIFY(FIRST)
      NULLIFY(PHFIRST)
      NULLIFY(PTR_CUR)
C
      ILVOUT=IVOLU(44)
      IF (ILVOUT >=1.AND.NBA == 0) WRITE(ISTDO,'(A19,I10)') 
     .   '    --> FVMBAG ID: ',IVOLU(1)
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
C Normales elementaires
C---------------------------------------------------
      DO IEL=1,NEL+NELI
         N1=ELEM(1,IEL)
         N2=ELEM(2,IEL)
         N3=ELEM(3,IEL)
         NN1=IBUF(N1)
         NN2=IBUF(N2)
         NN3=IBUF(N3)
         CALL FVNORMAL(X,NN1,NN2,NN3,0,NRX,NRY,NRZ)
         AREA2=SQRT(NRX**2+NRY**2+NRZ**2)
         ELAREA(IEL)=HALF*AREA2
         NORM(1,IEL)=NRX/AREA2
         NORM(2,IEL)=NRY/AREA2
         NORM(3,IEL)=NRZ/AREA2
      ENDDO
C
      DO IEL=1,NELA
         N1=ELEMA(1,IEL)
         N2=ELEMA(2,IEL)
         N3=ELEMA(3,IEL)
         IF (TAGELA(IEL) > 0) THEN
            NN1=IBUF(N1)
            NN2=IBUF(N2)
            NN3=IBUF(N3)
         ELSEIF (TAGELA(IEL) < 0) THEN
            NN1=IBUFA(N1)
            NN2=IBUFA(N2)
            NN3=IBUFA(N3)
         ENDIF
         CALL FVNORMAL(X,NN1,NN2,NN3,0,NRX,NRY,NRZ)
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
      IF (NELA == 0) THEN
         NPOLY=0
         NPOLH=0
         GOTO 370
      ENDIF
C
      DO I=1,NBRIC
         ITAGB(I)=0
      ENDDO
C 1. Facettes airbag coupees
C nullify sinon test sur associated vrai second appel fvmesh1
      NULLIFY(FIRST2)
      IF (ILVOUT >=1) WRITE(ISTDO,'(8X,A)') 'BUILDING POLYGONS'
C
      DO I=1,NBRIC
        !IF (ILVOUT >=1) CALL PROGALE_C(I, NBRIC, 1)    !dynamic screen output
C         
         PTR_OLD => PTR_CUR
         NPOLY_OLD=NPOLY
         NNS_OLD=NNS
  100    CONTINUE
C
         ALLOCATE(FACET(6,1+NFACMAX), TRI(NFACMAX,5),
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
            IF (TAGELA(IEL) > 0) THEN
               NN1=IBUF(N1)
               NN2=IBUF(N2)
               NN3=IBUF(N3)
            ELSEIF (TAGELA(IEL) < 0) THEN
               NN1=IBUFA(N1)
               NN2=IBUFA(N2)
               NN3=IBUFA(N3)
            ENDIF
            X1=X(1,NN1)
            Y1=X(2,NN1)
            Z1=X(3,NN1)
            X2=X(1,NN2)
            Y2=X(2,NN2)
            Z2=X(3,NN2)
            X3=X(1,NN3)
            Y3=X(2,NN3)
            Z3=X(3,NN3)
            XTMAX=MAX(X1,X2,X3)
            YTMAX=MAX(Y1,Y2,Y3)
            ZTMAX=MAX(Z1,Z2,Z3)
            XTMIN=MIN(X1,X2,X3)
            YTMIN=MIN(Y1,Y2,Y3)
            ZTMIN=MIN(Z1,Z2,Z3)
            IF (XBMAX < XTMIN.OR.YBMAX < YTMIN.OR.ZBMAX < ZTMIN.OR.
     .          XBMIN > XTMAX.OR.YBMIN > YTMAX.OR.ZBMIN > ZTMAX) 
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
            IF (ICUT == 0) THEN
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
               IF (ICUT == 1) ICUT=2
            ENDIF
C               
            IF (ICUT >= 1) THEN
               IF (ICUT == 1) THEN
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
                  IF (NVERTS > 2) THEN
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
                  IF (ICUT == 1) THEN
                     TBRIC(7+J,I)=2
                     NS=FACET(J,1)
                     NS=NS+1
                     FACET(J,1)=NS
                     IF (NS > NFACMAX) THEN
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
                  IF (ICUT == 1) THEN
                     TBRIC(7+J,I)=2
                     NS=FACET(J,1)
                     NS=NS+1
                     FACET(J,1)=NS
                     IF (NS > NFACMAX) THEN
                        INFO=1
                     ELSE
                        FACET(J,1+NS)=IEL
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
C
         IF (INFO == 1) THEN
            NSMAX=0
            DO J=1,6
               NSMAX=MAX(NSMAX,FACET(J,1))
            ENDDO
            NFACMAX=NSMAX
            IF(ILVOUT >=1) WRITE(IOUT,'(A,I10,A,I10)') 
     .                      ' MONITORED VOLUME ID: ',IVOLU(1),
     .                      ' NFACMAX IS RESET TO ',NSMAX
C Destruction des polygones crees pour rien
            IF (NPOLY_OLD > 0) THEN
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
            IF (NPOLY_OLD > 0) THEN
               PTR_PREC => PTR_OLD
            ELSE
               NULLIFY(FIRST)
            ENDIF
            NPOLY=NPOLY_OLD
            NNS=NNS_OLD
C
C
            DEALLOCATE(FACET, TRI, NORMT, POLY)
            GOTO 100
         ENDIF
C 2. Faces laterales
         IF (TBRIC(7,I) <= 1) THEN
           DEALLOCATE(FACET, TRI, NORMT, POLY)
           CYCLE
         END IF
C Debut boucle sur les 6 faces de la brique i
         DO J=1,6
            NV=TBRIC(J,I)
            IF (NV == 0) CYCLE
            IF (ITAGB(NV) == 1) CYCLE
            IF (TBRIC(7+J,I) == 2) THEN
               DO K=1,4
                  KK=FAC(K,J)
                  QUAD(1,K)=XB(1,BRIC(KK,I))
                  QUAD(2,K)=XB(2,BRIC(KK,I))
                  QUAD(3,K)=XB(3,BRIC(KK,I))
               ENDDO
               NS=FACET(J,1)
               DO K=1,NS
                  IEL=FACET(J,1+K) !  1<IEL< NELA
                  IF (TAGELA(IEL) > 0) THEN
                     DO L=1,3
                        LL=IBUF(ELEMA(L,IEL))
                        TRI(K,L)=LL
                     ENDDO
                     IF(TAGELA(IEL) <= NEL) THEN
C Triangle sur airbag
                       TRI(K,5)=1
                     ELSE
C Triangle sur structure interne
                       TRI(K,5)=3
                     ENDIF
                  ELSEIF (TAGELA(IEL) < 0) THEN
C Triangle sur brique additionelle
                     DO L=1,3
                        LL=IBUFA(ELEMA(L,IEL))
                        TRI(K,L)=LL
                     ENDDO
                     TRI(K,5)=2
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
               ALLOCATE(IPOLY(6+NPPMAX+1+NPOLMAX,NPOLMAX), 
     .                  RPOLY(NRPMAX+3*NPOLMAX,NPOLMAX),
     .                  IELNOD(NPPMAX,NPOLMAX))
C
               CALL FACEPOLY(QUAD,   TRI,   NS,     IPOLY,   RPOLY,
     .                       NR,     NORMF, NORMT,  NFACMAX, NNP,
     .                       NRPMAX, I,     NV,     DXM,     NPOLMAX,
     .                       NPPMAX, INFO,  IELNOD, X,       J,
     .                       ILVOUT, IVOLU(1) )
               IF (INFO == 1) THEN
                  DEALLOCATE(IPOLY, RPOLY, IELNOD)
                  GOTO 200
               ENDIF
C
               DO N=1,NNP
                  IF (IPOLY(1,N) == -1) CYCLE
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
C Fin de boucle sur les faces des briques
         ITAGB(I)=1
C
         DEALLOCATE(FACET, TRI, NORMT, POLY)
      ENDDO
C Fin de boucle sur les briques
C Fin de creation des polygones
C-----------------------------------------------------------------------------------
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
      IF (NBZ > 1) THEN
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
            NN1=IBUF(N1)
            NN2=IBUF(N2)
            NN3=IBUF(N3)
            X1=X(1,NN1)
            X2=X(1,NN2)
            X3=X(1,NN3)
            Y1=X(2,NN1)
            Y2=X(2,NN2)
            Y3=X(2,NN3)
            Z1=X(3,NN1)
            Z2=X(3,NN2)
            Z3=X(3,NN3)
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
            IF (ZLC < ZBMIN.OR.ZLC > ZBMAX) THEN
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
C
               IF (ZLMIN*ZLMAX < ZERO) THEN
                  ITY=PTR_CUR%IPOLY(1)
                  NN=PTR_CUR%IPOLY(2)
                  NHOL=0
                  IF (ITY == 2) NHOL=PTR_CUR%IPOLY(6+NN+1)
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
                  IF(NNP >= 1) THEN
                  PTR_TMP => PTR_CUR%PTR
                  NPOLY_NEW=NPOLY_NEW-1
                  DO N=1,NNP
                     NPOLY_NEW=NPOLY_NEW+1
                     IF (N == 1) THEN
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
                     IF (N == NNP) THEN
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
                     IF (N == NNP) PTR_CUR%PTR => PTR_TMP
                  ENDDO
                  ENDIF
C
                  DEALLOCATE(IPOLY, RPOLY, IELNOD, NREF, AREF, IZ)
               ELSE
C
                  IF (ZLMIN == ZERO.AND.ZLMAX > ZERO) THEN
                     NN=PTR_CUR%IPOLY(2)
                     ALLOCATE(NREF(2,2*NN), AREF(4,2*NN))
                     CALL HORIEDGE(
     .                 PTR_CUR%IPOLY, PTR_CUR%RPOLY, VX3,    VY3,  VZ3,
     .                 NBNEDGE,       INEDGE,        RNEDGE, X0,   Y0,
     .                 Z0,            IIZ          , NNS3,   NREF, AREF,
     .                 NNSP         )
C
                     IF (NNSP > 0) THEN
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
C
                  IF (ZLMIN >= ZERO) THEN 
                     PTR_CUR%IZ(1)=1
                     PTR_CUR%IZ(2)=IIZ+1
                  ELSEIF (IIZ == 1.AND.ZLMAX <= ZERO) THEN
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
               IF (JJ > 0) THEN
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
            IF (NNSP > 0) THEN
               DO J=1,NNSP
                  N1=PTR_CUR%NREF(2,J)
                  N2=PTR_CUR%NREF(3,J)
                  IF (N1 > 0) PTR_CUR%NREF(2,J)=REDIR(N1)
                  IF (N2 > 0) PTR_CUR%NREF(3,J)=REDIR(N2)
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
C
         ALLOCATE(LEDGE(NBNEDGE))
         DO I=1,IIZ
            DO J=1,NBRIC
               NEDGE=0
               DO K=1,NBNEDGE
                  IF (INEDGE(6,K) /= I) CYCLE
                  IF (INEDGE(1,K) == 1.AND.INEDGE(5,K) /= J) CYCLE
                  IF (INEDGE(1,K) == 2.AND.INEDGE(4,K) /= J.AND.
     .                                     INEDGE(5,K) /= J) CYCLE 
                  NEDGE=NEDGE+1
                  LEDGE(NEDGE)=K
               ENDDO
               IF (NEDGE == 0) CYCLE
               ALLOCATE(IPOLY(6+2*NEDGE+1+NEDGE,NEDGE), 
     .                  RPOLY(4+6*NEDGE+3*NEDGE,NEDGE),
     .                  IZ(3,NEDGE), IELNOD(NEDGE,NEDGE))
C               
               CALL HORIPOLY(INEDGE, RNEDGE, LEDGE,  NEDGE, IPOLY,
     .                       RPOLY,  IZ,     IELNOD, NNP,   VX3,
     .                       VY3,    VZ3,    I     , J  ,   NEL,
     .                       TAGELA )
C
               DO N=1,NNP
                  IF (IPOLY(1,N) == -1) CYCLE
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
C
            IIZ=0
         ENDDO
      ENDIF
C---------------------------------------------------
C Construction des polyedres
C---------------------------------------------------
      NPOLH=0
C     
      IF (ILVOUT >=1) WRITE(ISTDO,'(8X,A)') 'BUILDING FINITE VOLUMES'
      DO I=1,NBRIC
         !IF (ILVOUT >=1) CALL PROGALE_C(I, NBRIC, 2)    !dynamic screen output
C
         IF (TBRIC(7,I) /= 2) CYCLE
C Polygones de la brique
         NPOLB=0
         NPPMAX=0
         PTR_CUR => FIRST
         DO J=1,NPOLY
            ITY=PTR_CUR%IPOLY(1)
            IF ((ITY == 1.AND.PTR_CUR%IPOLY(4) == I).OR.
     .          (ITY == 2.AND.
     .          (PTR_CUR%IPOLY(3) == I.OR.PTR_CUR%IPOLY(4) == I))) THEN
               NPOLB=NPOLB+1
               NPPMAX=MAX(NPPMAX,PTR_CUR%IPOLY(2))
            ENDIF
            PTR_CUR => PTR_CUR%PTR
         ENDDO
         IF (NPOLB == 0) CYCLE
C
         NRPMAX=4+3*NPPMAX
         ALLOCATE(POLB(NPOLB), IPOLY(6+NPPMAX,NPOLB),
     .            RPOLY(NRPMAX,NPOLB), REDIR(NPOLB))
         DO INZ=1,IIZ+1
            NPOLB=0
            ITYZINT=0
            PTR_CUR => FIRST
            DO J=1,NPOLY
               ITY=PTR_CUR%IPOLY(1)
               ITYZ=PTR_CUR%IZ(1)
               IF (((ITY == 1.AND.PTR_CUR%IPOLY(4) == I).OR.
     .              (ITY == 2.AND.(PTR_CUR%IPOLY(3) == I.OR.
     .                             PTR_CUR%IPOLY(4) == I)))
     .              .AND.
     .             ((ITYZ == 1.AND.PTR_CUR%IZ(2) == INZ).OR.
     .              (ITYZ == 2.AND.(PTR_CUR%IZ(2) == INZ.OR.
     .                              PTR_CUR%IZ(3) == INZ)))) THEN
                  NPOLB=NPOLB+1
                  REDIR(NPOLB)=J
                  NN=PTR_CUR%IPOLY(2)
                  IF(ITY==1.AND.ITYZINT==0) THEN
                     IEL=PTR_CUR%IPOLY(3)   ! 1<IEL<NELA
                     IF(TAGELA(IEL) > NEL) ITYZINT=1
                  ENDIF
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
            IF (NPOLB == 0) CYCLE
C
            NPHMAX=NPOLB
            NPOLHMAX=NLAYER
C
  300       CONTINUE
            IF (ALLOCATED(POLH)) DEALLOCATE(POLH)
            ALLOCATE(POLH(NPHMAX+2,NPOLHMAX))
C
            IF(ITYZINT==0) THEN
              CALL POLYHEDR(IPOLY, RPOLY   , POLB,   NPOLB,     POLH,
     .                      NNP,   NRPMAX  , NPHMAX, I,         DXM ,
     .                      INFO , NPOLHMAX, NPPMAX  )
            ELSE
              CALL POLYHEDR1(IPOLY, RPOLY   , POLB,   NPOLB,     POLH,
     .                       NNP,   NRPMAX  , NPHMAX, I,         DXM ,
     .                       INFO , NPOLHMAX, NPPMAX, NEL ,      INZ ,
     .                       TAGELA )
            ENDIF
            IF (INFO == 1) GOTO 300
C Modifications des polygones
            PTR_CUR => FIRST
            NPL=1
            POLC=REDIR(POLB(NPL))
            DO J=1,NPOLY
               IF (J == POLC) THEN
                  ITY=PTR_CUR%IPOLY(1)
                  IF (ITY == 1) THEN
                     PTR_CUR%IPOLY(5)=NPOLH+IPOLY(5,NPL)
                     IEL=PTR_CUR%IPOLY(3)
                     IF(TAGELA(IEL) > NEL) THEN
                        PTR_CUR%IPOLY(6)=NPOLH+IPOLY(6,NPL)
                     ENDIF
                  ELSEIF (ITY == 2) THEN
                     IF (PTR_CUR%IPOLY(5) == 0) THEN
                        PTR_CUR%IPOLY(5)=NPOLH+IPOLY(5,NPL)
                     ELSE
                        PTR_CUR%IPOLY(6)=NPOLH+IPOLY(6,NPL)
                     ENDIF
                  ENDIF
                  IF (NPL == NPOLB) GOTO 350
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
         ENDDO  !INZ=1,IIZ+1
         DEALLOCATE(IPOLY, RPOLY, POLB, POLH, REDIR)
      ENDDO  !I=1,NBRIC
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
         IF (PTR_CUR%IPOLY(1) == 1) THEN
            LENIMAX=MAX(LENIMAX,6+NN+1)
            LENRMAX=MAX(LENRMAX,4+3*NN)
         ELSEIF (PTR_CUR%IPOLY(1) == 2) THEN
            NHOL=PTR_CUR%IPOLY(6+NN+1)
            LENIMAX=MAX(LENIMAX,6+NN+1+NHOL)
            LENRMAX=MAX(LENRMAX,4+3*NN+3*NHOL)
         ENDIF
         PTR_CUR => PTR_CUR%PTR
      ENDDO
      PH_CUR => PHFIRST
      NPHMAX=0
C
      DO I=1,NPOLH
         NN=PH_CUR%POLH(1)
         NPHMAX=MAX(NPHMAX,NN)
         PH_CUR => PH_CUR%PTR
      ENDDO
C
      NPOLY_OLD=NPOLY
      NPOLH_OLD=NPOLH
C--------------------------------------------
C Briques additionelles
C--------------------------------------------
      IF (NBA > 0) THEN
         NPOLH=NPOLH+NBA
         DO I=1,NBA
            ITAGBA(I)=0
         ENDDO
C
         DO I=1,NB_NODE
            ITAGN(I)=0
         ENDDO
         DO IEL=1,NEL
            N1=IBUF(ELEM(1,IEL))
            N2=IBUF(ELEM(2,IEL))
            N3=IBUF(ELEM(3,IEL))
            ITAGN(N1)=1
            ITAGN(N2)=1
            ITAGN(N3)=1
         ENDDO
C
         COUNT_ELEM_INT_OLD = 0
         COUNT_ELEM_EXT_OLD = 0
         COUNT_TRIA_INTERNE_OLD = 0
         DO I=1,NBA
            NTYPE=TBA(2,I)
            NFAC=NFACE(NTYPE)
            NNP=0
            DO J=1,NFAC
               ITY=TFACA(2*(J-1)+1,I)
               IF (ITY == 1 .OR. ITY == -2) THEN
C Face interne brique/brique
                  NV=TFACA(2*(J-1)+2,I)
                  IF (ITAGBA(NV) == 0) THEN
                     IF (ITY == 1) COUNT_TRIA_INTERNE_OLD = COUNT_TRIA_INTERNE_OLD + 1
                     IF (ITY == -2) COUNT_ELEM_INT_OLD = COUNT_ELEM_INT_OLD + 1
                     LENIMAX=MAX(LENIMAX,6+3+1)
                     LENRMAX=MAX(LENRMAX,6+3*3)
                     IF (NTYPE==2) THEN
                        NPOLY=NPOLY+1
                        NNS=NNS+3
                     ELSEIF (NTYPE==3) THEN
                        IF(J <= 2) THEN
                          NPOLY=NPOLY+1
                          NNS=NNS+3
                        ELSE
                          NPOLY=NPOLY+2
                          NNS=NNS+6
                        ENDIF
                     ELSEIF (NTYPE==4) THEN
                        IF(J <= 4) THEN
                          NPOLY=NPOLY+1
                          NNS=NNS+3
                        ELSE
                          NPOLY=NPOLY+2
                          NNS=NNS+6
                        ENDIF
                     ELSEIF (NTYPE==1) THEN
                        NPOLY=NPOLY+2
                        NNS=NNS+6
                     ENDIF
                  ENDIF
C
                  IF (NTYPE==2) THEN
                     NNP=NNP+1
                  ELSEIF (NTYPE==3) THEN
                      IF(J <= 2) THEN
                        NNP=NNP+1
                      ELSE
                        NNP=NNP+2
                      ENDIF
                  ELSEIF (NTYPE==4) THEN
                      IF(J <= 4) THEN
                        NNP=NNP+1
                      ELSE
                        NNP=NNP+2
                      ENDIF
                  ELSEIF (NTYPE==1) THEN
                     NNP=NNP+2
                  ENDIF
C
               ELSEIF (ITY == 2) THEN
C Face brique appuyee sur surface externe (ity == 2)
                  COUNT_ELEM_EXT_OLD = COUNT_ELEM_EXT_OLD + 1
                  IF (NTYPE==2) THEN
                     NPOLY=NPOLY+1
                     NNS=NNS+3
                     NNP=NNP+1
                     ELSEIF (NTYPE==3) THEN
                        IF(J <= 2) THEN
                          NPOLY=NPOLY+1
                          NNS=NNS+3
                          NNP=NNP+1
                        ELSE
                          NPOLY=NPOLY+2
                          NNS=NNS+6
                          NNP=NNP+2
                        ENDIF
                     ELSEIF (NTYPE==4) THEN
                        IF(J <= 4) THEN
                          NPOLY=NPOLY+1
                          NNS=NNS+3
                          NNP=NNP+1
                        ELSE
                          NPOLY=NPOLY+2
                          NNS=NNS+6
                          NNP=NNP+2
                        ENDIF
                  ELSEIF (NTYPE==1) THEN
                     NPOLY=NPOLY+2
                     NNS=NNS+6
                     NNP=NNP+2
                  ENDIF
C
               ELSEIF (ITY == 3) THEN
C Face interne brique/polyedres
                  PTR_CUR => FIRST
                  DO K=1,NPOLY_OLD
                     IF (PTR_CUR%IPOLY(1) == 1) THEN
                        IEL=PTR_CUR%IPOLY(3)
                        IF (-TAGELA(IEL) == I) NNP=NNP+1
                     ENDIF
                     PTR_CUR => PTR_CUR%PTR
                  ENDDO
               ENDIF
            ENDDO    !J=1,NFAC
            ITAGBA(I)=1
            NPHMAX=MAX(NPHMAX,NNP)
C Calcul de IBSA =1 si tous les noeuds de la brique sont sur l'airbag =0 autrement
            II=TBA(1,I)
            NALL=1
            IF (NTYPE==2) THEN
               DO J=1,4
                  JJ=IXS(1+2*(J-1)+1,II)
                  NALL=NALL*ITAGN(JJ)
               ENDDO
            ELSEIF (NTYPE==3) THEN
               DO J=1,3
                  JJ=IXS(1+J,II)
                  NALL=NALL*ITAGN(JJ)
               ENDDO
               DO J=4,6
                  JJ=IXS(1+J+1,II)
                  NALL=NALL*ITAGN(JJ)
               ENDDO
            ELSEIF (NTYPE==4) THEN
               DO J=1,5
                  JJ=IXS(1+J,II)
                  NALL=NALL*ITAGN(JJ)
               ENDDO
            ELSEIF (NTYPE==1) THEN
               DO J=1,8
                  JJ=IXS(1+J,II)
                  NALL=NALL*ITAGN(JJ)
               ENDDO
            ENDIF
            IBSA(I)=NALL
         ENDDO   !I=1,NBA
      ENDIF
C
      ALLOCATE(IPOLY_F(LENIMAX,NPOLY), RPOLY_F(LENRMAX,NPOLY),
     .         POLH_F(2+NPHMAX,NPOLH), IFVNOD(NNS), VOLU(NPOLH),
     .         IFVNOD2(2,NNS2), RFVNOD2(4,NNS2), XNS(3,NNS), 
     .         XNS2(3,NNS2))
C
      NPOLY=NPOLY_OLD
      NPOLH=NPOLH_OLD

      VOLU(1:NPOLH) = 0
      IMAX = 0
      PTR_CUR => FIRST
      NNS=0
      DO I=1,NPOLY
         NN=PTR_CUR%IPOLY(2)
         DO J=1,NN
            JJ=PTR_CUR%IPOLY(6+J)
            IF (JJ > 0) THEN
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
            IF (JJ > 0) THEN
               NNS=NNS+1
               IPOLY_F(6+J,I)=NNS
               IFVNOD(NNS)=PTR_CUR%IELNOD(J)
            ELSE
               IPOLY_F(6+J,I)=JJ
            ENDIF
         ENDDO
         IF (PTR_CUR%IPOLY(1) == 1) THEN
            IEL=IPOLY_F(3,I)
            IF(TAGELA(IEL) < 0 ) THEN
              IPOLY_F(4,I)=IPOLY_F(5,I)
              IPOLY_F(5,I)=0
            ELSEIF(TAGELA(IEL) > 0) THEN
              IF(TAGELA(IEL) <= NEL) THEN           
                IPOLY_F(4,I)=IPOLY_F(5,I)
                IPOLY_F(5,I)=0
              ENDIF
            ENDIF
         ELSEIF (PTR_CUR%IPOLY(1) == 2) THEN
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
         IF (NNSP > 0) THEN
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
         IF (NNSP > 0) DEALLOCATE(PTR_CUR%NREF, PTR_CUR%AREF)
         DEALLOCATE(PTR_CUR)
         PTR_CUR => PTR_TMP
      ENDDO
C
      DO I=1,NNS2
         N1=IFVNOD2(1,I)
         N2=IFVNOD2(2,I)
         IDEP=0
         IF (N1 < 0) THEN
            II=-N1
            IF (IFVNOD2(1,II) /= N2.AND.IFVNOD2(2,II) /= N2) THEN
               WRITE(ISTDO,*) 'PROBLEM DEPENDANT NODE ',I
               CALL MY_EXIT(2)
            ENDIF
            N1=IFVNOD2(1,II)
            N2=IFVNOD2(2,II)
            IDEP=1
         ELSEIF (N2 < 0) THEN
            II=-N2
            IF (IFVNOD2(1,II) /= N1.AND.IFVNOD2(2,II) /= N1) THEN
               WRITE(ISTDO,*) 'PROBLEM DEPENDANT NODE ',I
               CALL MY_EXIT(2)
            ENDIF
            N1=IFVNOD2(1,II)
            N2=IFVNOD2(2,II)
            IDEP=1
         ENDIF
         IFVNOD2(1,I)=N1
         IFVNOD2(2,I)=N2
         IF (IDEP == 1) THEN
C Recalcul des coordonnees barycentriques du point dependant
            II=1
            VAL=ABS(XNS(1,N1)-XNS(1,N2))
            DO J=2,3
               IF (ABS(XNS(J,N1)-XNS(J,N2)) > VAL) THEN
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
      ALLOCATE(IFLAG(NB_NODE))
      IFLAG(1:NB_NODE) = 0

      IF (NBA > 0) THEN
         DO I=1,NBA
            ITAGBA(I)=0
         ENDDO
C 
         COUNT_ELEM_INT = 0
         COUNT_ELEM_EXT = 0
         COUNT_TRIA_INTERNE = 0
         NPOLY_OLD=NPOLY
         DO I=1,NBA
            II=TBA(1,I)
            NTYPE=TBA(2,I)
            NFAC=NFACE(NTYPE)
            NNP=0
            DO J=1,NFAC
               ITY=TFACA(2*(J-1)+1,I)
               IF (ITY == 1) THEN
C Face interne brique/brique
                  NV=TFACA(2*(J-1)+2,I)
                  IF (ITAGBA(NV) == 0) THEN
                     COUNT_TRIA_INTERNE = COUNT_TRIA_INTERNE + 1
                     IF (NTYPE==2) THEN
                        IQUAD=0
                        N1=FAC4(1,J)
                        N2=FAC4(2,J)
                        N3=FAC4(3,J)
                     ELSEIF (NTYPE==3) THEN
                        IF(J <= 2) THEN
                          IQUAD=0
                          N1=FAC6(1,J)
                          N2=FAC6(2,J)
                          N3=FAC6(3,J)
                        ELSE
                          IQUAD=1
                          N1=FAC6(1,J)
                          N2=FAC6(2,J)
                          N3=FAC6(3,J)
                          N4=FAC6(4,J)
						ENDIF
                     ELSEIF (NTYPE==4) THEN
                        IF(J <= 4) THEN
                          IQUAD=0
                          N1=FAC5(1,J)
                          N2=FAC5(2,J)
                          N3=FAC5(3,J)
                        ELSE
                          IQUAD=1
                          N1=FAC5(1,J)
                          N2=FAC5(2,J)
                          N3=FAC5(3,J)
                          N4=FAC5(4,J)
						ENDIF
                     ELSEIF (NTYPE==1) THEN
                        IQUAD=1
                        N1=FAC(1,J)
                        N2=FAC(2,J)
                        N3=FAC(3,J)
                        N4=FAC(4,J)
                     ENDIF						  
C
                     IF(IQUAD==0) THEN
C Face=triangle
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
C
                        NN1=IXS(1+N1,II)
                        NN2=IXS(1+N2,II)
                        NN3=IXS(1+N3,II)
                        IFVNOD(NNS+1)=-NN1
                        IFVNOD(NNS+2)=-NN2
                        IFVNOD(NNS+3)=-NN3
                        NNS=NNS+3
                        X1=X(1,NN1)
                        Y1=X(2,NN1)
                        Z1=X(3,NN1)
                        X2=X(1,NN2)
                        Y2=X(2,NN2)
                        Z2=X(3,NN2)
                        X3=X(1,NN3)
                        Y3=X(2,NN3)
                        Z3=X(3,NN3)
                        CALL FVNORMAL(X,NN1,NN2,NN3,0,NRX,NRY,NRZ)
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
                     ELSEIF(IQUAD==1) THEN
C Face=quadrangle					 
                        NN1=IXS(1+N1,II)
                        NN2=IXS(1+N2,II)
                        NN3=IXS(1+N3,II)
                        NN4=IXS(1+N4,II)
                        X1=X(1,NN1)
                        Y1=X(2,NN1)
                        Z1=X(3,NN1)
                        X2=X(1,NN2)
                        Y2=X(2,NN2)
                        Z2=X(3,NN2)
                        X3=X(1,NN3)
                        Y3=X(2,NN3)
                        Z3=X(3,NN3)
                        X4=X(1,NN4)
                        Y4=X(2,NN4)
                        Z4=X(3,NN4)
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
                        CALL FVNORMAL(X,NN1,NN2,NN3,0,NRX,NRY,NRZ)
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
                        CALL FVNORMAL(X,NN1,NN3,NN4,0,NRX,NRY,NRZ)
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
                        IF (IPOLY_F(1,KK) == 2.AND.
     .                      IPOLY_F(6,KK) == NPOLH+I) THEN
                           NNP=NNP+1
                           POLH_F(2+NNP,NPOLH+I)=KK
                        ENDIF
                     ENDDO
                  ENDIF
               ELSEIF (ITY == 2) THEN        
C Face brique appuyee sur surface externe (traitee plus loin)
                  COUNT_ELEM_EXT = COUNT_ELEM_EXT + 1
               ELSEIF (ITY == -2) THEN
C     Brick face lying on an internal surface element. 
C     Need to compute the normal in order to ensure proper orientation
                  COUNT_ELEM_INT = COUNT_ELEM_INT + 1
                  IF (NTYPE==2) THEN
                     IQUAD=0
                     N1=FAC4(1,J)
                     N2=FAC4(2,J)
                     N3=FAC4(3,J)
                  ELSEIF (NTYPE==3) THEN
                     IF(J <= 2) THEN
                        IQUAD=0
                        N1=FAC6(1,J)
                        N2=FAC6(2,J)
                        N3=FAC6(3,J)
                     ELSE
                        IQUAD=1
                        N1=FAC6(1,J)
                        N2=FAC6(2,J)
                        N3=FAC6(3,J)
                        N4=FAC6(4,J)
                     ENDIF
                  ELSEIF (NTYPE==4) THEN
                     IF(J <= 4) THEN
                        IQUAD=0
                        N1=FAC5(1,J)
                        N2=FAC5(2,J)
                        N3=FAC5(3,J)
                     ELSE
                        IQUAD=1
                        N1=FAC5(1,J)
                        N2=FAC5(2,J)
                        N3=FAC5(3,J)
                        N4=FAC5(4,J)
                     ENDIF
                  ELSEIF (NTYPE==1) THEN
                     IQUAD=1
                     N1=FAC(1,J)
                     N2=FAC(2,J)
                     N3=FAC(3,J)
                     N4=FAC(4,J)
                  ENDIF
                  IF (IQUAD == 0) THEN
                     NN1=IXS(1+N1,II)
                     NN2=IXS(1+N2,II)
                     NN3=IXS(1+N3,II)
                     CALL FVNORMAL(X,NN1,NN2,NN3,0,NRX,NRY,NRZ)
                     IFLAG(NN1) = 1
                     IFLAG(NN2) = 1
                     IFLAG(NN3) = 1
                     DO IEL = NEL + 1, NEL + NELI
                        IF (IFLAG(IBUF(ELEM(1, IEL))) * IFLAG(IBUF(ELEM(2, IEL))) *
     .                     IFLAG(IBUF(ELEM(3, IEL))) == 1) THEN
                           IF (NORM(1, IEL) * NRX + NORM(2, IEL) * NRY + NORM(3, IEL) * NRZ < 0) THEN
                              IF (TAGELS(2*IEL - NEL - 1) == I) THEN
                                 NORM(1, IEL) = -NORM(1, IEL)
                                 NORM(2, IEL) = -NORM(2, IEL)
                                 NORM(3, IEL) = -NORM(3, IEL)
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDDO
                     IFLAG(NN1) = 0
                     IFLAG(NN2) = 0
                     IFLAG(NN3) = 0
                  ELSE IF (IQUAD == 1) THEN
                     NN1=IXS(1+N1,II)
                     NN2=IXS(1+N2,II)
                     NN3=IXS(1+N3,II)
                     NN4=IXS(1+N4,II)
                     CALL FVNORMAL(X,NN1,NN2,NN3,0,NRX,NRY,NRZ)
                     IFLAG(NN1) = 1
                     IFLAG(NN2) = 1
                     IFLAG(NN3) = 1
                     IFLAG(NN4) = 1
                     DO IEL = NEL + 1, NEL + NELI
                        IF (IFLAG(IBUF(ELEM(1, IEL))) * IFLAG(IBUF(ELEM(2, IEL))) *
     .                     IFLAG(IBUF(ELEM(3, IEL))) == 1) THEN
                           IF (NORM(1, IEL) * NRX + NORM(2, IEL) * NRY + NORM(3, IEL) * NRZ < 0) THEN
                              IF (TAGELS(2*IEL - NEL - 1) == I) THEN
                                 NORM(1, IEL) = -NORM(1, IEL)
                                 NORM(2, IEL) = -NORM(2, IEL)
                                 NORM(3, IEL) = -NORM(3, IEL)
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDDO
                     IFLAG(NN1) = 0
                     IFLAG(NN2) = 0
                     IFLAG(NN3) = 0
                     IFLAG(NN4) = 0
                  ENDIF
               ELSEIF (ITY == 3) THEN
C Face interne brique/polyedres
                  DO K=1,NPOLY_OLD
                     IF (IPOLY_F(1,K) == 1) THEN
                        IEL=IPOLY_F(3,K)
                        IF (-TAGELA(IEL) == I) THEN
                           NNP=NNP+1
                           POLH_F(2+NNP,NPOLH+I)=K
                           IPOLY_F(1,K)=2
                           IPOLY_F(3,K)=0
                           IP=IPOLY_F(4,K)
                           IPOLY_F(4,K)=0
                           IPOLY_F(5,K)=IP
                           IPOLY_F(6,K)=NPOLH+I
                           NN=IPOLY_F(2,K)
                           IPOLY_F(6+NN+1,K)=0
                        ENDIF
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
            POLH_F(1,NPOLH+I)=NNP
            POLH_F(2,NPOLH+I)=-I
            ITAGBA(I)=1
         ENDDO  ! I=1,NBA
C
         NPOLH=NPOLH+NBA
C
         DO I=1,NPOLY_OLD
            IF (IPOLY_F(1,I) == 1) THEN
               IEL=IPOLY_F(3,I)
               IF (TAGELA(IEL) > 0) IPOLY_F(3,I)=TAGELA(IEL)
            ENDIF
         ENDDO
C     External airbag surface
         DO IEL=1,NEL
            IF (TAGELS(IEL) > 0) THEN
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
               N1=ELEM(1,IEL)
               N2=ELEM(2,IEL)
               N3=ELEM(3,IEL)
               NN1=IBUF(N1)
               NN2=IBUF(N2)
               NN3=IBUF(N3)
               IFVNOD(NNS+1)=-NN1
               IFVNOD(NNS+2)=-NN2
               IFVNOD(NNS+3)=-NN3
               NNS=NNS+3
               RPOLY_F(1,NPOLY)=ELAREA(IEL)
               RPOLY_F(2,NPOLY)=NORM(1,IEL)
               RPOLY_F(3,NPOLY)=NORM(2,IEL)
               RPOLY_F(4,NPOLY)=NORM(3,IEL)
               X1=X(1,NN1)
               Y1=X(2,NN1)
               Z1=X(3,NN1)
               X2=X(1,NN2)
               Y2=X(2,NN2)
               Z2=X(3,NN2)
               X3=X(1,NN3)
               Y3=X(2,NN3)
               Z3=X(3,NN3)
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
C     Internal airbag surface
         DO IEL=NEL + 1,NEL + NELI
            IF (TAGELS(2*IEL-NEL-1) > 0) THEN
               NPOLY=NPOLY+1
               IPOLY_F(1,NPOLY)=3
               IPOLY_F(2,NPOLY)=3
               IPOLY_F(3,NPOLY)=IEL
               IPOLY_F(4,NPOLY)=NPOLH_OLD + TAGELS(2 * IEL - NEL - 1)
               IPOLY_F(5,NPOLY)=NPOLH_OLD + TAGELS(2*IEL-NEL-1)
               IPOLY_F(6,NPOLY)=NPOLH_OLD + TAGELS(2*IEL-NEL)
               IPOLY_F(7,NPOLY)=NNS+1
               IPOLY_F(8,NPOLY)=NNS+2
               IPOLY_F(9,NPOLY)=NNS+3
               N1=ELEM(1,IEL)
               N2=ELEM(2,IEL)
               N3=ELEM(3,IEL)
               NN1=IBUF(N1)
               NN2=IBUF(N2)
               NN3=IBUF(N3)
               IFVNOD(NNS+1)=-NN1
               IFVNOD(NNS+2)=-NN2
               IFVNOD(NNS+3)=-NN3
               NNS=NNS+3
               RPOLY_F(1,NPOLY)=ELAREA(IEL)
               RPOLY_F(2,NPOLY)=NORM(1,IEL)
               RPOLY_F(3,NPOLY)=NORM(2,IEL)
               RPOLY_F(4,NPOLY)=NORM(3,IEL)
               X1=X(1,NN1)
               Y1=X(2,NN1)
               Z1=X(3,NN1)
               X2=X(1,NN2)
               Y2=X(2,NN2)
               Z2=X(3,NN2)
               X3=X(1,NN3)
               Y3=X(2,NN3)
               Z3=X(3,NN3)
               RPOLY_F(5,NPOLY)=X1
               RPOLY_F(6,NPOLY)=Y1
               RPOLY_F(7,NPOLY)=Z1
               RPOLY_F(8,NPOLY)=X2
               RPOLY_F(9,NPOLY)=Y2
               RPOLY_F(10,NPOLY)=Z2
               RPOLY_F(11,NPOLY)=X3
               RPOLY_F(12,NPOLY)=Y3
               RPOLY_F(13,NPOLY)=Z3
C     Internal surface elements have two neighbours
               NNP=POLH_F(1,NPOLH_OLD+TAGELS(2*IEL-NEL-1))
               NNP=NNP+1
               POLH_F(1,NPOLH_OLD+TAGELS(2*IEL-NEL-1))=NNP
               POLH_F(2+NNP,NPOLH_OLD+TAGELS(2*IEL-NEL-1))=NPOLY
               NNP=POLH_F(1,NPOLH_OLD+TAGELS(2*IEL-NEL))
               NNP=NNP+1
               POLH_F(1,NPOLH_OLD+TAGELS(2*IEL-NEL))=NNP
               POLH_F(2+NNP,NPOLH_OLD+TAGELS(2*IEL-NEL))=NPOLY
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
         IF (ITY == 2) NHOL=IPOLY_F(6+NN+1,I)
         AREA=ZERO
         NX=RPOLY_F(2,I)
         NY=RPOLY_F(3,I)
         NZ=RPOLY_F(4,I)
         IF (NHOL == 0) THEN
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
            ALLOCATE(ADR(NHOL+1))
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
C Volume des polyedres
C---------------------------------------------------
      DO I=1,NPOLH
         VOLU(I)=ZERO
         DO J=1,POLH_F(1,I)
            JJ=POLH_F(2+J,I)
            AREA=RPOLY_F(1,JJ)
            ITY=IPOLY_F(1,JJ)
            IF (ITY == 1) THEN
               IEL=IPOLY_F(3,JJ)
               IF(TAGELS(IEL) > 0) THEN
                 NX=RPOLY_F(2,JJ)
                 NY=RPOLY_F(3,JJ)
                 NZ=RPOLY_F(4,JJ)
               ELSEIF(TAGELS(IEL) == 0) THEN
                 IF(IEL <= NEL) THEN
                   NX=RPOLY_F(2,JJ)
                   NY=RPOLY_F(3,JJ)
                   NZ=RPOLY_F(4,JJ)
                 ELSEIF(IEL > NEL) THEN
                   IF (IPOLY_F(5,JJ) == I) THEN
                      NX=RPOLY_F(2,JJ)
                      NY=RPOLY_F(3,JJ)
                      NZ=RPOLY_F(4,JJ)
                   ELSEIF (IPOLY_F(6,JJ) == I) THEN
                      NX=-RPOLY_F(2,JJ)
                      NY=-RPOLY_F(3,JJ)
                      NZ=-RPOLY_F(4,JJ)
                   ENDIF
                 ENDIF
               ENDIF
            ELSEIF (ITY == 2.OR.ITY == 3) THEN
               IF (IPOLY_F(5,JJ) == I) THEN
                  NX=RPOLY_F(2,JJ)
                  NY=RPOLY_F(3,JJ)
                  NZ=RPOLY_F(4,JJ)
               ELSEIF (IPOLY_F(6,JJ) == I) THEN
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
C     Check negative volumes
C---------------------------------------------------
      IF (ITYP  ==  8) THEN
         DO I = 1, NPOLH
            IF (VOLU(I)  <=  ZERO) THEN
               CALL ANCMSG(MSGID = 1627, MSGTYPE = MSGERROR,
     .              ANMODE = ANINFO,
     .              I1 = ID,
     .              C1 = TITR,
     .              I2 = IXS(11, TBA(1, I)))
            ENDIF
         ENDDO
      ENDIF 
C---------------------------------------------------
C Elimination des segments de longueur nulle dans les polygones
C---------------------------------------------------
      NNS_OLD=NNS
      NNS_OLD_SAVE=NNS_OLD
      ALLOCATE(IFVNOD_OLD(NNS_OLD), REDIR(NNS_OLD))
      DO I=1,NNS_OLD
         IFVNOD_OLD(I)=IFVNOD(I)
      ENDDO
      NNS_OLD=0
      NNS=0
C
C      TOLE=RVOLU(50)
      TOLE=EPSILON(ZERO)*0.5
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
         IF (IPOLY_F(1,I) == 2) NHOL=IPOLY_F(6+NNP_OLD+1,I)
         ALLOCATE(IPOLY_OLD(NNP_OLD), RPOLY_OLD(3*NNP_OLD),
     .            ADR_OLD(NHOL+2), ADR(NHOL+2))
         DO J=1,NNP_OLD
            IPOLY_OLD(J)=IPOLY_F(6+J,I)
         ENDDO
         DO J=1,3*NNP_OLD
            RPOLY_OLD(J)=RPOLY_F(4+J,I)
         ENDDO
         NNP=0
         IF (NHOL == 0) THEN
            X0=RPOLY_F(5,I)
            Y0=RPOLY_F(6,I)
            Z0=RPOLY_F(7,I)
            IF (IPOLY_OLD(1) > 0) THEN
               NNS_OLD=NNS_OLD+1
               IF (NNS_OLD > NNS_OLD_SAVE) THEN
                 CALL ANCMSG(MSGID=744,
     .                       MSGTYPE=MSGERROR,
     .                       ANMODE=ANINFO,
     .                       I1=ID,
     .                       C1=TITR)
               END IF
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
               IF (IPOLY_OLD(J) > 0) THEN
                 NNS_OLD=NNS_OLD+1
                 IF (NNS_OLD > NNS_OLD_SAVE) THEN
                   CALL ANCMSG(MSGID=744,
     .                         MSGTYPE=MSGERROR,
     .                         ANMODE=ANINFO,
     .                         I1=ID,
     .                         C1=TITR)
                 END IF
               END IF
               X1=RPOLY_OLD(3*(J-1)+1)
               Y1=RPOLY_OLD(3*(J-1)+2)
               Z1=RPOLY_OLD(3*(J-1)+3)
               DD=(X1-X0)**2+(Y1-Y0)**2+(Z1-Z0)**2
               IF (DD > TOLE2) THEN
                  NNP=NNP+1
                  IF (IPOLY_OLD(J) > 0) THEN
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
               ELSEIF (IPOLY_OLD(J) > 0.AND.
     .                 IPOLY_OLD(J0) < 0) THEN
                  NNS=NNS+1
                  IFVNOD(NNS)=IFVNOD_OLD(NNS_OLD)
                  RPOLY_F(4+3*(NNP-1)+1,I)=X1
                  RPOLY_F(4+3*(NNP-1)+2,I)=Y1
                  RPOLY_F(4+3*(NNP-1)+3,I)=Z1
                  IPOLY_F(6+NNP,I)=NNS
               ENDIF
               IF (IPOLY_OLD(J) > 0) REDIR(NNS_OLD)=NNS
               J0=J
            ENDDO
            X1=RPOLY_F(5,I)
            Y1=RPOLY_F(6,I)
            Z1=RPOLY_F(7,I)
            DD=(X1-X0)**2+(Y1-Y0)**2+(Z1-Z0)**2
            IF (DD <= TOLE2.AND.IPOLY_OLD(1) > 0) THEN
               REDIR(NNS_OLD)=NNS1
               NNS=NNS-1
               NNP=NNP-1
            ELSEIF (DD <= TOLE2.AND.IPOLY_OLD(1) < 0) THEN
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
               IF (IPOLY_OLD(ADR_OLD(J)+1) > 0) THEN
                  NNS_OLD=NNS_OLD+1
                  IF (NNS_OLD > NNS_OLD_SAVE) THEN
                    CALL ANCMSG(MSGID=744,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO,
     .                          I1=ID,
     .                          C1=TITR)
                  END IF
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
                  IF (NNS_OLD > NNS_OLD_SAVE) THEN
                    CALL ANCMSG(MSGID=744,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO,
     .                          I1=ID,
     .                          C1=TITR)
                  END IF
                  X1=RPOLY_OLD(3*(K-1)+1)
                  Y1=RPOLY_OLD(3*(K-1)+2)
                  Z1=RPOLY_OLD(3*(K-1)+3)
                  DD=(X1-X0)**2+(Y1-Y0)**2+(Z1-Z0)**2
                  IF (DD > TOLE2) THEN
                     NNP=NNP+1                       
                     IF (IPOLY_OLD(K) > 0) THEN           
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
                  ELSEIF (IPOLY_OLD(K) > 0.AND.
     .                    IPOLY_OLD(K0) < 0) THEN
                     NNS=NNS+1
                     IFVNOD(NNS)=IFVNOD_OLD(NNS_OLD)
                     RPOLY_F(4+3*(NNP-1)+1,I)=X1
                     RPOLY_F(4+3*(NNP-1)+2,I)=Y1
                     RPOLY_F(4+3*(NNP-1)+3,I)=Z1
                     IPOLY_F(6+NNP,I)=NNS
                  ENDIF
                  IF (IPOLY_OLD(K) > 0) REDIR(NNS_OLD)=NNS
                  K0=K
               ENDDO
               X1=RPOLY_F(4+3*(ADR(J)-1)+1,I)
               Y1=RPOLY_F(4+3*(ADR(J)-1)+2,I)
               Z1=RPOLY_F(4+3*(ADR(J)-1)+3,I)
               DD=(X1-X0)**2+(Y1-Y0)**2+(Z1-Z0)**2
               IF (DD <= TOLE2.AND.IPOLY_OLD(ADR_OLD(J)+1) > 0) THEN
                  REDIR(NNS_OLD)=NNS1
                  NNS=NNS-1
                  NNP=NNP-1
               ELSEIF (DD <= TOLE2.AND.
     .                 IPOLY_OLD(ADR_OLD(J)+1) < 0) THEN
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
         IF (NNP < NNP_OLD) THEN
C Nouvelle aire du polygone
            AREA=ZERO
            NX=RPOLY_F(2,I)
            NY=RPOLY_F(3,I)
            NZ=RPOLY_F(4,I)
            IF (NHOL == 0) THEN
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
      ENDDO  ! I=1,NPOLY
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
C Fusion des polyedres a volume infinitesimal a leur voisin
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
      IF (INFO == 1) THEN
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
         IF (POLH_F(2,I) < 0) CYCLE   ! tetraedre, ... solide additionel
C         IF (VOLU(I) < ZERO) THEN
         IF (VOLU(I) < -EM10) THEN
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
         IF (VOLU(I) > ZERO) THEN
            VM = MIN(VM ,VOLU(I))
            NPA=NPA+1
         ENDIF
      ENDDO
      VM=VM/NPA
C
      RVOLU(33)=VM
C     No merging will be performed during starter.
C     Starter merging algorithm does not take internal surfaces into account and
C     therefore may create holes through internal surfaces ...
      VOLUMIN = EM30 * VM
      INFO=0
      DO I=1,NPOLH
         IF (VOLU(I) <= VOLUMIN) THEN
            AREAMAX=ZERO
            JMAX=0
            IMAX=0
            DO J=1,POLH_NEW(1,I)
               JJ=POLH_NEW(2+J,I)
               ITY=IPOLY_F(1,JJ)
               IF (ITY == 1) CYCLE
               AREA=RPOLY_F(1,JJ)
               IF (AREA > AREAMAX) THEN
                  IF (IPOLY_F(5,JJ) == I) THEN
                     II=IPOLY_F(6,JJ)
                     JMAX=J
                     IMAX=II
                  ELSEIF (IPOLY_F(6,JJ) == I) THEN
                     II=IPOLY_F(5,JJ)
                     JMAX=J
                     IMAX=II
                  ENDIF
                  AREAMAX=AREA
               ENDIF
            ENDDO
C
            IF (JMAX /= 0) THEN
               JJMAX=POLH_NEW(2+JMAX,I)
               RPOLY_F(1,JJMAX)=-ONE
            ELSE
               JMAX=1
               JJMAX=POLH_NEW(2+JMAX,I)
               ITY=IPOLY_F(1,JJMAX)
               IF (ITY == 2) RPOLY_F(1,JJMAX)=-ONE
C On merge certain polyedres de volume < volumin avec le polyedre 1
C on ne devrait plus passer la
               IMAX=1
            ENDIF
            IF (IMERGED(IMAX) == 1) IMAX=POLH_NEW(2,IMAX)
            NP=POLH_NEW(1,IMAX)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--            
            IF(ILVOUT >= 2) THEN
               WRITE(IOUT,'(A,I8,A6,G11.4,A1,A,I8,A6,G11.4,A1)') 
     .         '** GLOBAL MERGE: MERGING FINITE VOLUME ',I,' (VOL=',
     .         VOLU(I),')',' WITH FINITE VOLUME ',IMAX,
     .         ' (VOL=',VOLU(IMAX),')'
            ENDIF
            VOLU(IMAX)=VOLU(IMAX)+VOLU(I)
            VOLU(I)=-ONE
            IMERGED(I)=1
            DO J=1,POLH_NEW(1,I)
               IF (J /= JMAX) THEN
                  NP=NP+1
                  IF (NP > NPHMAX) INFO=1
                  IF (INFO == 0) THEN
                     JJ=POLH_NEW(2+J,I)
                     POLH_NEW(2+NP,IMAX)=JJ
                     ITY=IPOLY_F(1,JJ)
                     IEL=IPOLY_F(3,JJ)  ! 1<IEL<NEL
                     IF(ITY == 1 .AND.IEL > NEL)ITY=3
                     IF (ITY >= 2) THEN
                        IF (IPOLY_F(5,JJ) == I) THEN
                           IPOLY_F(5,JJ)=IMAX
                        ELSEIF (IPOLY_F(6,JJ) == I) THEN
                           IPOLY_F(6,JJ)=IMAX
                        ENDIF
                     ENDIF
                  ELSE
                     NPHMAX=MAX(NPHMAX,NP)
                  ENDIF
               ENDIF
            ENDDO
            POLH_NEW(2,I)=IMAX
            IF (INFO == 0) POLH_NEW(1,IMAX)=NP   
         ENDIF
      ENDDO  !I=1,NPOLH
      IF (INFO == 1) GOTO 400
C
      VMIN=EP30
      DO I=1,NPOLH
         IF (VOLU(I) <= ZERO) CYCLE
         IF (VOLU(I) < VMIN) THEN
            VMIN=VOLU(I)
            IMIN=I
         ENDIF
         DO J=1,POLH_NEW(1,I)
            JJ=POLH_NEW(2+J,I)
            IF (IPOLY_F(1,JJ) == 1.OR.RPOLY_F(1,JJ) < ZERO) CYCLE
            IF (IPOLY_F(5,JJ) == IPOLY_F(6,JJ)) RPOLY_F(1,JJ)=-ONE
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
         IF (VOLU(I) > ZERO) THEN
            NPOLHF=NPOLHF+1
            VOLPH=VOLPH+VOLU(I)
         ENDIF
      ENDDO
      NSPOLY=0
      NCPOLY=0
      DO I=1,NPOLY
         ITY=IPOLY_F(1,I)
         IF (ITY == 1.AND.RPOLY_F(1,I) > ZERO) THEN
            NSPOLY=NSPOLY+1
            AREAP=AREAP+RPOLY_F(1,I)
         ELSEIF ((ITY == 2 .OR. ITY == 3) .AND.RPOLY_F(1,I) > ZERO) THEN
            NCPOLY=NCPOLY+1
         ENDIF
      ENDDO
      DO IEL=1,NEL
         AREAEL=AREAEL+ELAREA(IEL)
      ENDDO
C---------------------------------------------------
C Coordonnees elementaires des nouveaux noeuds
C---------------------------------------------------
      IFV=IVOLU(45)
      ALLOCATE(FVDATA(IFV)%IFVNOD(3,NNS+NNS2),
     .         FVDATA(IFV)%RFVNOD(2,NNS+NNS2))
C
      FVDATA(IFV)%RFVNOD(1:2,1:NNS+NNS2) = 0
      DO I=1,NNS+NNS2
         FVDATA(IFV)%IFVNOD(1,I)=0
         FVDATA(IFV)%IFVNOD(2,I)=0
         FVDATA(IFV)%IFVNOD(3,I)=0
      ENDDO
C
      NNS_OLD=NNS
      NNS=0
      DO I=1,NPOLY
         NNP=IPOLY_F(2,I)
         DO J=1,NNP
            IF (IPOLY_F(6+J,I) > 0) THEN
               NNS=NNS+1
               IF (NNS > NNS_OLD) THEN
                 CALL ANCMSG(MSGID=744,
     .                       MSGTYPE=MSGERROR,
     .                       ANMODE=ANSTOP,
     .                       I1=ID,
     .                       C1=TITR)
               END IF

               IF (IFVNOD(NNS) < 0) THEN
C point de solide additionnel
                  FVDATA(IFV)%IFVNOD(1,NNS)=2
                  FVDATA(IFV)%IFVNOD(2,NNS)=-IFVNOD(NNS)
                  CYCLE
               ENDIF
C point appuye sur airbag
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
               IF (TAGELA(IEL) > 0) THEN
                  NN1=IBUF(N1)
                  NN2=IBUF(N2)
                  NN3=IBUF(N3)
               ELSEIF (TAGELA(IEL) < 0) THEN
                  NN1=IBUFA(N1)
                  NN2=IBUFA(N2)
                  NN3=IBUFA(N3)
               ENDIF
               X1=X(1,NN1)
               Y1=X(2,NN1)
               Z1=X(3,NN1)
               X2=X(1,NN2)
               Y2=X(2,NN2)
               Z2=X(3,NN2)
               X3=X(1,NN3)
               Y3=X(2,NN3)
               Z3=X(3,NN3)
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
C point interne
               JJ=-IPOLY_F(6+J,I)
               FVDATA(IFV)%IFVNOD(1,NNS_OLD+JJ)=3
               FVDATA(IFV)%IFVNOD(2,NNS_OLD+JJ)=IFVNOD2(1,JJ)
               FVDATA(IFV)%IFVNOD(3,NNS_OLD+JJ)=IFVNOD2(2,JJ)
               FVDATA(IFV)%RFVNOD(1,NNS_OLD+JJ)=RFVNOD2(1,JJ)
            ENDIF
         ENDDO
      ENDDO
C
      DO I=1,NNS2
         IF (FVDATA(IFV)%IFVNOD(1,NNS+I) == 0) THEN
            FVDATA(IFV)%IFVNOD(1,NNS+I)=3
            FVDATA(IFV)%IFVNOD(2,NNS+I)=1
            FVDATA(IFV)%IFVNOD(3,NNS+I)=2
            FVDATA(IFV)%RFVNOD(1,NNS+I)=ONE
         ENDIF
      ENDDO
C
      IF (NNS > NNS_OLD) THEN
         CALL ANCMSG(MSGID=744,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO,
     .               I1=ID,
     .               C1=TITR)
      END IF
C---------------------------------------------------
C Triangulation des polygones
C---------------------------------------------------
      NNTR=0
      DO I=1,NPOLY
         NN=IPOLY_F(2,I)
         NNTR=NNTR+NN
      ENDDO
C
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
         IF (RPOLY_F(1,I) <= ZERO) THEN
            DO J=1,IPOLY_F(2,I)
               IF (IPOLY_F(6+J,I) > 0) NNS=NNS+1
            ENDDO
            CYCLE
         ENDIF
         NPOLY=NPOLY+1
         REDIR_POLY(I)=NPOLY
         NNP=IPOLY_F(2,I)
         NHOL=0
         IF (IPOLY_F(1,I) == 2) NHOL=IPOLY_F(6+NNP+1,I)
         ALLOCATE(PNODES(2,NNP), PSEG(2,NNP), PHOLES(2,NHOL),PTRI(3,NNP), REDIR(NNP)) 
         PTRI(1:3,1:NNP) = 0       
C
         DO J=1,NNP
            IF (IPOLY_F(6+J,I) > 0) THEN
               NNS=NNS+1
               REDIR(J)=NNS
            ELSE
               JJ=-IPOLY_F(6+J,I)
               REDIR(J)=NNS_OLD+JJ
            ENDIF
         ENDDO
         IF (IPOLY_F(1,I) == 1) THEN
            IPSURF=IPOLY_F(3,I)
            IF( IPSURF > NEL) THEN
              IPSURF=-IPSURF
              IC1=IPOLY_F(5,I)
              IC2=IPOLY_F(6,I)
            ELSE
              IC1=0
              IC2=0
            ENDIF
         ELSEIF (IPOLY_F(1,I) == 2 .OR. IPOLY_F(1,I) == 3) THEN
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
         IF (NHOL == 0) THEN
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
            DEALLOCATE(ADR)
         ENDIF
C
         NELP = 0
         CALL C_TRICALL(PNODES, PSEG, PHOLES, PTRI, NNP, NSEG, NHOL, NELP)
C
         FVDATA(IFV)%IFVTADR(NPOLY)=NNTR+1
C avoid bug with PGI => comp O1
C         CALL BIDON1(IFV)
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
            IF (SS > ZERO) THEN
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
C Structure des polyedres
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
         IF (VOLU(I) <= ZERO) CYCLE
         NPOLH=NPOLH+1
         REDIR_POLH(I)=NPOLH
         FVDATA(IFV)%IFVPADR(NPOLH)=NNP+1
         DO J=1,POLH_NEW(1,I)
            JJ=REDIR_POLY(POLH_NEW(2+J,I))
            IF (JJ == 0) CYCLE
            NNP=NNP+1
            FVDATA(IFV)%IFVPOLH(NNP)=REDIR_POLY(POLH_NEW(2+J,I))
         ENDDO
C
         FVDATA(IFV)%IDPOLH(NPOLH)=NPOLH
C
         II=POLH_NEW(2,I)
         IF (II >= 0) II=0
         IF (II < 0)THEN
          IF(IBSA(-II) == 1) II=-II
         ENDIF
         FVDATA(IFV)%IBPOLH(NPOLH)=II
      ENDDO
      FVDATA(IFV)%IFVPADR(NPOLH+1)=NNP+1
C
      DO I=1,NNTR
         IF (FVDATA(IFV)%IFVTRI(4,I) <= 0) THEN
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
     .         FVDATA(IFV)%TPOLH(NPOLH), 
     .         FVDATA(IFV)%CPAPOLH(NPOLH), FVDATA(IFV)%CPBPOLH(NPOLH),
     .         FVDATA(IFV)%CPCPOLH(NPOLH), FVDATA(IFV)%RMWPOLH(NPOLH),
     .         FVDATA(IFV)%VPOLH_INI(NPOLH), FVDATA(IFV)%DTPOLH(NPOLH))
      ALLOCATE(FVDATA(IFV)%CPDPOLH(NPOLH), FVDATA(IFV)%CPEPOLH(NPOLH),
     .         FVDATA(IFV)%CPFPOLH(NPOLH))
C---------------------------------------------------
C Initialisation des quantites dans les polyedres
C---------------------------------------------------
      GAMAI=RVOLU(1)
      CPAI=RVOLU(7)
      CPBI=RVOLU(8)
      CPCI=RVOLU(9)
      RMWI=RVOLU(10)
      PINI=RVOLU(12)
      TI=RVOLU(13)
      CPDI=RVOLU(56)
      CPEI=RVOLU(57)
      CPFI=RVOLU(58)
      TI2=TI*TI
      RHOI=PINI/(TI*RMWI)
C
      CPI=CPAI+HALF*CPBI*TI+THIRD*CPCI*TI2
      IF(IVOLU(2) == 8) THEN
        CPI=CPI+FOURTH*CPDI*TI2*TI-CPEI/TI2+ONE_FIFTH*CPFI*TI2*TI2
      ENDIF      
      CVI=CPI-RMWI
C
      TMASS=0
      DO I=1,NPOLH_OLD
         II=REDIR_POLH(I)
         IF (II == 0) CYCLE
         FVDATA(IFV)%MPOLH(II)=RHOI*VOLU(I)
         FVDATA(IFV)%QPOLH(1,II)=ZERO
         FVDATA(IFV)%QPOLH(2,II)=ZERO
         FVDATA(IFV)%QPOLH(3,II)=ZERO
         FVDATA(IFV)%EPOLH(II)=FVDATA(IFV)%MPOLH(II)*CVI*TI
         FVDATA(IFV)%PPOLH(II)=PINI
         FVDATA(IFV)%RPOLH(II)=RHOI
         FVDATA(IFV)%GPOLH(II)=GAMAI
         FVDATA(IFV)%CPAPOLH(II)=CPAI
         FVDATA(IFV)%CPBPOLH(II)=CPBI
         FVDATA(IFV)%CPCPOLH(II)=CPCI
         FVDATA(IFV)%TPOLH(II)=TI
         FVDATA(IFV)%CPDPOLH(II)=CPDI
         FVDATA(IFV)%CPEPOLH(II)=CPEI
         FVDATA(IFV)%CPFPOLH(II)=CPFI
         FVDATA(IFV)%RMWPOLH(II)=RMWI
         FVDATA(IFV)%DTPOLH(II)=EP30
         FVDATA(IFV)%VPOLH_INI(II)=VOLU(I)
         TMASS=TMASS+RHOI*VOLU(I)
      ENDDO
C
      IMIN=REDIR_POLH(IMIN)
      DEALLOCATE(IPOLY_F, RPOLY_F, POLH_F, VOLU, REDIR_POLY, REDIR_POLH,
     .           POLH_NEW, IFVNOD, IFVNOD2, RFVNOD2, XNS, XNS2)
C---------------------------------------------------
C Impressions
C---------------------------------------------------
      NSTR=0
      NCTR=0
      DO I=1,NNTR
         IF (FVDATA(IFV)%IFVTRI(4,I) > 0) THEN
            NSTR=NSTR+1
         ELSE
            NCTR=NCTR+1
         ENDIF
      ENDDO
C
C NSPOLY  NUMBER OF SURFACE POLYGONS
C NSTR    NUMBER OF SURFACE TRIANGLES
C NCPOLY  NUMBER OF COMMUNICATION POLYGONS
C NCTR    NUMBER OF COMMUNICATION TRIANGLES
C NPOLHF  NUMBER OF POLYHEDRA (FINITE VOLUMES)
C
      WRITE(IOUT,1000) IVOLU(1),NSPOLY,NSTR,NCPOLY,NCTR,NPOLHF
      WRITE(IOUT,1010) VMIN,IMIN,VOLUMIN
      WRITE(IOUT,1020) VOLPH,AREAP,TMASS
C
C---------------------------------------------------
C Anim des volumes finis
C---------------------------------------------------
      IVOLU(43)=0
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
C-----------------------------------------
C Elimination des noeuds doubles pour anim
C-----------------------------------------
      TOLE=EM05*FAC_LENGTH
      TOLE2=TOLE**2
      NNS_ANIM=0
      ALLOCATE(PNODES(3,NNS+NNS2))
      DO I=1,NNS
         IF (FVDATA(IFV)%IFVNOD(1,I) == 1) THEN
            IEL=FVDATA(IFV)%IFVNOD(2,I)
            KSI=FVDATA(IFV)%RFVNOD(1,I)
            ETA=FVDATA(IFV)%RFVNOD(2,I)
            N1=ELEMA(1,IEL)
            N2=ELEMA(2,IEL)
            N3=ELEMA(3,IEL)
            IF (TAGELA(IEL) > 0) THEN
               N1=IBUF(N1)
               N2=IBUF(N2)
               N3=IBUF(N3)
            ELSEIF (TAGELA(IEL) < 0) THEN
               N1=IBUFA(N1)
               N2=IBUFA(N2)
               N3=IBUFA(N3)
            ENDIF
            X1=X(1,N1)
            Y1=X(2,N1)
            Z1=X(3,N1)
            X2=X(1,N2)
            Y2=X(2,N2)
            Z2=X(3,N2)
            X3=X(1,N3)
            Y3=X(2,N3)
            Z3=X(3,N3)
            PNODES(1,I)=(ONE-KSI-ETA)*X1+KSI*X2+ETA*X3
            PNODES(2,I)=(ONE-KSI-ETA)*Y1+KSI*Y2+ETA*Y3
            PNODES(3,I)=(ONE-KSI-ETA)*Z1+KSI*Z2+ETA*Z3
         ELSEIF (FVDATA(IFV)%IFVNOD(1,I) == 2) THEN
            II=FVDATA(IFV)%IFVNOD(2,I)
            PNODES(1,I)=X(1,II)
            PNODES(2,I)=X(2,II)
            PNODES(3,I)=X(3,II)
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
C--------------------------
C Impression
C--------------------------
      IF(ILVOUT >=3 ) THEN
       ALLOCATE(AREATR(NNTR))
       DO I=1,NNTR
         N1=FVDATA(IFV)%IFVTRI(1,I)        
         N2=FVDATA(IFV)%IFVTRI(2,I)
         N3=FVDATA(IFV)%IFVTRI(3,I)
         X1=PNODES(1,N1)
         Y1=PNODES(2,N1)
         Z1=PNODES(3,N1)
         X2=PNODES(1,N2)
         Y2=PNODES(2,N2)
         Z2=PNODES(3,N2)
         X3=PNODES(1,N3)
         Y3=PNODES(2,N3)
         Z3=PNODES(3,N3)
         CALL FVNORMAL(PNODES,N1,N2,N3,0,NRX,NRY,NRZ)
         AREA2=SQRT(NRX**2+NRY**2+NRZ**2)
         AREATR(I)=HALF*AREA2
        ENDDO
C
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
        WRITE(IOUT,'(/A,10X,3A)')' FINITE VOLUME','  BRICK   VOLUME  ',
     .                           ' POLYGONE       TRIANGLE       AREA'
C
        DO I=1,NPOLH
         N1= FVDATA(IFV)%IDPOLH(I)
         N2= FVDATA(IFV)%IBPOLH(I)
         N3= 0
         IF(N2 /= 0) THEN
         N3= TBA(1,IABS(N2))
         ENDIF
         VOLPH=FVDATA(IFV)%VPOLH_INI(I)
         II=0
         JJJ=0
         KKK=0
          DO J=FVDATA(IFV)%IFVPADR(I),FVDATA(IFV)%IFVPADR(I+1)-1
           JJJ=JJJ+1
           JJ=FVDATA(IFV)%IFVPOLH(J)
            DO K=FVDATA(IFV)%IFVTADR(JJ),FVDATA(IFV)%IFVTADR(JJ+1)-1
             KKK=KKK+1
             KK=FVDATA(IFV)%IFVPOLY(K)
             AREA=AREATR(KK)
             IEL=FVDATA(IFV)%IFVTRI(4,KK)
             IC1=FVDATA(IFV)%IFVTRI(5,KK)
             IC2=FVDATA(IFV)%IFVTRI(6,KK)
             IF(KKK == 1) THEN
               WRITE(IOUT,'(3I10,2X,F10.2,4X,I5,5X,I10,5X,G14.6,3I10)')
     .                      N1,N2,N3,VOLPH,JJJ,KK,AREA,IEL,IC1,IC2
             ELSE
               WRITE(IOUT,'(46X,I5,5X,I10,5X,G14.6,3I10)') 
     .                                     JJJ,KK,AREA,IEL,IC1,IC2
             ENDIF
            ENDDO
          ENDDO
        ENDDO
       DEALLOCATE(AREATR)
      ENDIF
C-------------------------------------------
C Merge des points coincidents
C-------------------------------------------
      IF (ILVOUT >=1) WRITE(ISTDO,'(8X,A)') 
     .   'MERGING COINCIDENT NODES FOR ANIM'

      ALLOCATE(RADIUS(NNS + NNS2), IDX(NNS + NNS2))
      DO I = 1, NNS + NNS2
         XX=PNODES(1,I)
         YY=PNODES(2,I)
         ZZ=PNODES(3,I)
         RADIUS(I) = XX*XX + YY*YY + ZZ*ZZ
         IDX(I) = I
      ENDDO
      
      CALL QUICKSORT(RADIUS, IDX, 1, NNS + NNS2)
      I = 1
      DO WHILE(I  <  NNS+ NNS2)
         !IF (ILVOUT >=1) CALL PROGALE_C(I, NNS+NNS2, 3)    !dynamic screen output
         IAD1 = IDX(I)
         XX = PNODES(1,IAD1)
         YY = PNODES(2,IAD1)
         ZZ = PNODES(3,IAD1)
         R1 = RADIUS(I)
         NNS_ANIM = NNS_ANIM + 1
         REDIR(IAD1) = NNS_ANIM
         FVDATA(IFV)%REDIR_ANIM(NNS_ANIM)=IAD1
         FVDATA(IFV)%NOD_ANIM(1,NNS_ANIM)=XX
         FVDATA(IFV)%NOD_ANIM(2,NNS_ANIM)=YY         
         FVDATA(IFV)%NOD_ANIM(3,NNS_ANIM)=ZZ
         J = I
         DO WHILE(J  <=  NNS + NNS2)
            IF (ABS(R1 - RADIUS(J))  <=  TOLE2) THEN
               J = J + 1
            ELSE
               EXIT
            ENDIF
         ENDDO
         J = J - 1
         
         IAD = I + 1
         DO WHILE (IAD  <=  J) 
            IAD2 = IDX(IAD)
            XN(1) = PNODES(1,IAD2)
            XN(2) = PNODES(2,IAD2)
            XN(3) = PNODES(3,IAD2)
            DD2=(XX-XN(1))**2+(YY-XN(2))**2+(ZZ-XN(3))**2
            IF (DD2 <= TOLE2) THEN
C     NODE IAD2 is duplicated with node IAD1
               REDIR(IAD2) = REDIR(IAD1)
               IAD = IAD + 1
            ELSE
C     put it at the end
               ITMP1 = IDX(J)
               IDX(J) = IAD2
               IDX(IAD) = ITMP1
               J = J - 1
            ENDIF
         ENDDO
         I = J + 1
      ENDDO
      
      DEALLOCATE(RADIUS, IDX)

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
      DEALLOCATE(IFLAG)
C
      RETURN
C
1000  FORMAT(/5X,'VOLUME NUMBER ',I10,
     .       /5X,'NUMBER OF SURFACE POLYGONS. . . . . . .=',I10,
     .       /5X,'NUMBER OF SURFACE TRIANGLES . . . . . .=',I10,
     .       /5X,'NUMBER OF COMMUNICATION POLYGONS. . . .=',I10,
     .       /5X,'NUMBER OF COMMUNICATION TRIANGLES . . .=',I10,
     .       /5X,'NUMBER OF FINITE VOLUMES. . . . . . . .=',I10)
1010  FORMAT( 5X,'MIN FINITE VOLUME VOLUME. . . . . . . .=',1PG20.13,
     .        5X,'(FINITE VOLUME ID ',I10,')',
     .       /5X,'INITIAL MERGING VOLUME. . . . . . . . .=',1PG20.13)
1020  FORMAT( 5X,'SUM VOLUME OF FINITE VOLUMES. . . . . .=',1PG20.13,
     .       /5X,'SUM AREA SURFACE TRIANGLES. . . . . . .=',1PG20.13,
     .       /5X,'SUM MASS OF FINITE VOLUMES. . . . . . .=',1PG20.13)
C
      END
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
Chd|====================================================================
Chd|  FVDIM                         source/airbag/fvmesh.F        
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        FVBAG_MOD                     share/modules1/fvbag_mod.F    
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|====================================================================
      SUBROUTINE FVDIM(T_MONVOL)
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
      USE MONVOL_STRUCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(INOUT) :: T_MONVOL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, ITYP
C
      ALLOCATE(FVID(NVOLU))
      FVID(:) = -1
      NFVBAG=0
      DO N = 1, NVOLU
         ITYP = T_MONVOL(N)%TYPE
         IF (ITYP == 6 .OR. ITYP == 8) THEN
            NFVBAG = NFVBAG+1
            T_MONVOL(N)%IVOLU(45) = NFVBAG
            T_MONVOL(N)%IVOLU(57) = 0
            FVID(NFVBAG) = N
         ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  FVDEAL                        source/airbag/fvmesh.F        
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        FVBAG_MOD                     share/modules1/fvbag_mod.F    
Chd|====================================================================
      SUBROUTINE FVDEAL()            
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
C
#include      "scr05_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C
      IF (ALLOCATED(FVDATA)) THEN
         DO I=1,NFVBAG
            IF (ASSOCIATED(FVDATA(I)%IFVNOD)) 
     .              DEALLOCATE(FVDATA(I)%IFVNOD)
            IF (ASSOCIATED(FVDATA(I)%RFVNOD)) 
     .              DEALLOCATE(FVDATA(I)%RFVNOD)
            IF (ASSOCIATED(FVDATA(I)%IFVTRI)) 
     .              DEALLOCATE(FVDATA(I)%IFVTRI)
            IF (ASSOCIATED(FVDATA(I)%IFVPOLY)) 
     .              DEALLOCATE(FVDATA(I)%IFVPOLY)
            IF (ASSOCIATED(FVDATA(I)%IFVTADR)) 
     .              DEALLOCATE(FVDATA(I)%IFVTADR)
            IF (ASSOCIATED(FVDATA(I)%IFVPOLH)) 
     .              DEALLOCATE(FVDATA(I)%IFVPOLH)
            IF (ASSOCIATED(FVDATA(I)%IFVPADR)) 
     .              DEALLOCATE(FVDATA(I)%IFVPADR)
            IF (ASSOCIATED(FVDATA(I)%IDPOLH)) 
     .              DEALLOCATE(FVDATA(I)%IDPOLH)
            IF (ASSOCIATED(FVDATA(I)%IBPOLH)) 
     .              DEALLOCATE(FVDATA(I)%IBPOLH)
            IF (ASSOCIATED(FVDATA(I)%IFVPOLY_ANIM)) 
     .              DEALLOCATE(FVDATA(I)%IFVPOLY_ANIM)
            IF (ASSOCIATED(FVDATA(I)%IFVTADR_ANIM)) 
     .              DEALLOCATE(FVDATA(I)%IFVTADR_ANIM)
            IF (ASSOCIATED(FVDATA(I)%IFVPOLH_ANIM)) 
     .              DEALLOCATE(FVDATA(I)%IFVPOLH_ANIM)
            IF (ASSOCIATED(FVDATA(I)%IFVPADR_ANIM)) 
     .              DEALLOCATE(FVDATA(I)%IFVPADR_ANIM)
            IF (ASSOCIATED(FVDATA(I)%REDIR_ANIM)) 
     .              DEALLOCATE(FVDATA(I)%REDIR_ANIM)
            IF (ASSOCIATED(FVDATA(I)%NOD_ANIM)) 
     .              DEALLOCATE(FVDATA(I)%NOD_ANIM)
            IF (ASSOCIATED(FVDATA(I)%MPOLH)) 
     .              DEALLOCATE(FVDATA(I)%MPOLH)
            IF (ASSOCIATED(FVDATA(I)%QPOLH)) 
     .              DEALLOCATE(FVDATA(I)%QPOLH)
            IF (ASSOCIATED(FVDATA(I)%EPOLH)) 
     .              DEALLOCATE(FVDATA(I)%EPOLH)
            IF (ASSOCIATED(FVDATA(I)%PPOLH)) 
     .              DEALLOCATE(FVDATA(I)%PPOLH)
            IF (ASSOCIATED(FVDATA(I)%RPOLH)) 
     .              DEALLOCATE(FVDATA(I)%RPOLH)
            IF (ASSOCIATED(FVDATA(I)%GPOLH)) 
     .              DEALLOCATE(FVDATA(I)%GPOLH)
            IF (ASSOCIATED(FVDATA(I)%CPAPOLH)) 
     .              DEALLOCATE(FVDATA(I)%CPAPOLH)
            IF (ASSOCIATED(FVDATA(I)%CPBPOLH)) 
     .              DEALLOCATE(FVDATA(I)%CPBPOLH)
            IF (ASSOCIATED(FVDATA(I)%CPCPOLH)) 
     .              DEALLOCATE(FVDATA(I)%CPCPOLH)
            IF (ASSOCIATED(FVDATA(I)%RMWPOLH)) 
     .              DEALLOCATE(FVDATA(I)%RMWPOLH)
            IF (ASSOCIATED(FVDATA(I)%VPOLH_INI)) 
     .              DEALLOCATE(FVDATA(I)%VPOLH_INI)
            IF (ASSOCIATED(FVDATA(I)%DTPOLH)) 
     .              DEALLOCATE(FVDATA(I)%DTPOLH)
C
            IF (ASSOCIATED(FVDATA(I)%BRIC)) 
     .              DEALLOCATE(FVDATA(I)%BRIC)
            IF (ASSOCIATED(FVDATA(I)%TBRIC)) 
     .              DEALLOCATE(FVDATA(I)%TBRIC)
            IF (ASSOCIATED(FVDATA(I)%XB)) 
     .              DEALLOCATE(FVDATA(I)%XB)
            IF (ASSOCIATED(FVDATA(I)%SFAC)) 
     .              DEALLOCATE(FVDATA(I)%SFAC)
C
            IF (ASSOCIATED(FVDATA(I)%TPOLH)) 
     .              DEALLOCATE(FVDATA(I)%TPOLH)
            IF (ASSOCIATED(FVDATA(I)%CPDPOLH)) 
     .              DEALLOCATE(FVDATA(I)%CPDPOLH)
            IF (ASSOCIATED(FVDATA(I)%CPEPOLH)) 
     .              DEALLOCATE(FVDATA(I)%CPEPOLH)
            IF (ASSOCIATED(FVDATA(I)%CPFPOLH)) 
     .              DEALLOCATE(FVDATA(I)%CPFPOLH)
         ENDDO
         DEALLOCATE(FVDATA)
      ENDIF
C
      IF (IMACH == 3) THEN
         IF (ALLOCATED(FVSPMD)) THEN
            DO I=1,NFVBAG
               IF (ASSOCIATED(FVSPMD(I)%IBUF_L)) 
     .            DEALLOCATE(FVSPMD(I)%IBUF_L)
               IF (ASSOCIATED(FVSPMD(I)%IBUFA_L)) 
     .            DEALLOCATE(FVSPMD(I)%IBUFA_L)
               IF (ASSOCIATED(FVSPMD(I)%IBUFSA_L)) 
     .            DEALLOCATE(FVSPMD(I)%IBUFSA_L)
               IF (ASSOCIATED(FVSPMD(I)%IXSA)) 
     .            DEALLOCATE(FVSPMD(I)%IXSA)
               IF (ASSOCIATED(FVSPMD(I)%ELEMSA)) 
     .            DEALLOCATE(FVSPMD(I)%ELEMSA)
            ENDDO
            DEALLOCATE(FVSPMD)
         ENDIF
      ENDIF
C
      RETURN
      END
