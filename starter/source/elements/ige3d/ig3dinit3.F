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
Chd|  IG3DINIT3                     source/elements/ige3d/ig3dinit3.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        BULKIGE3                      source/elements/ige3d/bulkigeo3.F
Chd|        DTMAIN                        source/materials/time_step/dtmain.F
Chd|        IG3DMASS3                     source/elements/ige3d/ig3dmass3.F
Chd|        IG3DONEDERIV                  source/elements/ige3d/ig3donederiv.F
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|        SCZERO3                       source/elements/thickshell/solidec/scinit3.F
Chd|        SVALUE0                       source/elements/thickshell/solidec/scinit3.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE IG3DINIT3(ELBUF_STR,MS    ,KXIG3D ,IXIG3D  ,PM    ,X,
     .                   DETONATORS,GEO     ,VEUL   ,ALE_CONNECTIVITY   ,IPARG   ,
     .                   DTELEM   ,SIGI    ,NEL    ,SKEW    ,IGEO    ,
     .                   STIFN    ,PARTSAV ,V      ,IPARTIG3D,MSS    ,
     .                   IPART    ,SIGSP   ,
     .                   NSIGI    ,IN      ,VR     ,IPM     ,NSIGS   ,
     .                   VNIGE    ,BNIGE   ,PTSOL  ,
     .                   BUFMAT   ,NPF     ,TF     ,FAIL_INI,NCTRL,
     .                   MSIG3D   ,KNOT    ,NCTRLMAX,WIGE   ,PX,PY,PZ,
     .                   KNOTLOCPC,KNOTLOCEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE MESSAGE_MOD
      USE DETONATORS_MOD      
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "analyse_name.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r  s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr01_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr12_c.inc"
#include      "scr17_c.inc"
#include      "scry_c.inc"
#include      "vect01_c.inc"
#include      "warn_c.inc"
#include      "scr15_c.inc"
#include      "userlib.inc"
#include      "ige3d_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXIG3D(*), IPARG(*),
     .    NEL, IPART(LIPART1,*), 
     .    IGEO(NPROPGI,*), IPM(NPROPMI,*), PTSOL(*), NSIGI, NSIGS,
     .    NPF(*),FAIL_INI(*),KXIG3D(NIXIG3D,*),NCTRL,NCTRLMAX,
     .    IPARTIG3D(*),PX,PY,PZ
      my_real
     .   MS(*), X(3,*), GEO(NPROPG,*),PM(NPROPM,*),
     .   VEUL(LVEUL,*), DTELEM(*),SIGI(NSIGS,*),SKEW(LSKEW,*),STIFN(*),
     .   PARTSAV(20,*), V(3,*), MSS(8,*), KNOTLOCPC(DEG_MAX,3,*),
     .   KNOTLOCEL(2,3,*), SIGSP(NSIGI,*) , IN(*), VR(3,*),
     .   VNIGE(NCTRLMAX,*), BNIGE(NCTRLMAX,*),BUFMAT(*), TF(*),
     .   MSIG3D(NUMELIG3D,NCTRLMAX),KNOT(*),WIGE(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE(DETONATOR_STRUCT_)::DETONATORS
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,NF1,IBID,JHBE,IGTYP,IREP,NCC,NUVAR,IP,NREFSTA,
     .        IPID,NPTR,NPTS,NPTT,NLAY,ITEL
      INTEGER MAT(MVSIZ), PID(MVSIZ), NGL(MVSIZ),
     .        IPROP(MVSIZ) ,IMAT(MVSIZ) ,IAD_KNOT,
     .        EL_ID(MVSIZ),N1,N2,N3,NKNOT1,NKNOT2,NKNOT3,
     .        IDX(MVSIZ),IDY(MVSIZ),IDZ(MVSIZ),
     .        IDX2(MVSIZ),IDY2(MVSIZ),IDZ2(MVSIZ)
      CHARACTER*nchartitle,
     .   TITR1
      my_real
     .   BID, FV, V8LOC(51,MVSIZ), VOLU(MVSIZ), DTX(MVSIZ),DTX0(MVSIZ),
     .   MASS(NCTRL,NEL),INN(MVSIZ,8),XX(NCTRL,MVSIZ),
     .   YY(NCTRL,MVSIZ),ZZ(NCTRL,MVSIZ),WW(NCTRL,MVSIZ),
     .   VX(NCTRL,MVSIZ),VY(NCTRL,MVSIZ), VZ(NCTRL,MVSIZ),VRX(MVSIZ,8),
     .   VRY(MVSIZ,8),VRZ(MVSIZ,8),STI(MVSIZ),STIR(MVSIZ),VISCM(MVSIZ),
     .   VISCR(MVSIZ),
     .   X1(MVSIZ),X2(MVSIZ),X3(MVSIZ),X4(MVSIZ),X5(MVSIZ),X6(MVSIZ),
     .   X7(MVSIZ),X8(MVSIZ),Y1(MVSIZ),Y2(MVSIZ),Y3(MVSIZ),Y4(MVSIZ),
     .   Y5(MVSIZ),Y6(MVSIZ),Y7(MVSIZ),Y8(MVSIZ),Z1(MVSIZ),Z2(MVSIZ),
     .   Z3(MVSIZ),Z4(MVSIZ),Z5(MVSIZ),Z6(MVSIZ),Z7(MVSIZ),Z8(MVSIZ),
     .   RX(MVSIZ) ,RY(MVSIZ) ,RZ(MVSIZ) ,SX(MVSIZ) ,
     .   SY(MVSIZ) ,SZ(MVSIZ) ,TX(MVSIZ) ,TY(MVSIZ) ,TZ(MVSIZ) ,
     .   E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ),E2X(MVSIZ),
     .   E2Y(MVSIZ),E2Z(MVSIZ),E3X(MVSIZ),E3Y(MVSIZ),E3Z(MVSIZ),
     .   F1X(MVSIZ) ,F1Y(MVSIZ) ,F1Z(MVSIZ) ,
     .   F2X(MVSIZ) ,F2Y(MVSIZ) ,F2Z(MVSIZ),
     .   PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),PX4(MVSIZ),
     .   PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),PY4(MVSIZ),
     .   PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),PZ4(MVSIZ),
     .   KX ,KY ,KZ, ZR, ZS, ZT

      my_real
     .   VOLO(MVSIZ),DTE(MVSIZ),PGAUSS,DETJAC
      my_real
     .   R(NCTRL),DRDXI(3,NCTRL),KNOTLOCX(PX+1,NCTRL,MVSIZ),
     .   KNOTLOCY(PY+1,NCTRL,MVSIZ),KNOTLOCZ(PZ+1,NCTRL,MVSIZ),
     .   KNOTLOCELX(2,MVSIZ),
     .   KNOTLOCELY(2,MVSIZ),KNOTLOCELZ(2,MVSIZ)
      my_real
     .   TBID(MVSIZ), TBID2(8,MVSIZ)   ! SUREMENT A CHANGER LA FORME ET LE NOM
C-----------------------------------------------    
      TYPE(G_BUFEL_) ,POINTER :: GBUF 
      TYPE(L_BUFEL_) ,POINTER :: LBUF
      TYPE(BUF_MAT_) ,POINTER :: MBUF
C-----------------------------------------------    
      DOUBLE PRECISION
     .  W_GAUSS(9,9),A_GAUSS(9,9)
      DATA W_GAUSS / 
     1 2.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     2 1.D0               ,1.D0               ,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               ,
     3 0.555555555555556D0,0.888888888888889D0,0.555555555555556D0,
     3 0.D0               ,0.D0               ,0.D0               ,
     3 0.D0               ,0.D0               ,0.D0               ,
     4 0.347854845137454D0,0.652145154862546D0,0.652145154862546D0,
     4 0.347854845137454D0,0.D0               ,0.D0               ,
     4 0.D0               ,0.D0               ,0.D0               ,
     5 0.236926885056189D0,0.478628670499366D0,0.568888888888889D0,
     5 0.478628670499366D0,0.236926885056189D0,0.D0               ,
     5 0.D0               ,0.D0               ,0.D0               ,
     6 0.171324492379170D0,0.360761573048139D0,0.467913934572691D0,
     6 0.467913934572691D0,0.360761573048139D0,0.171324492379170D0,
     6 0.D0               ,0.D0               ,0.D0               ,
     7 0.129484966168870D0,0.279705391489277D0,0.381830050505119D0,
     7 0.417959183673469D0,0.381830050505119D0,0.279705391489277D0,
     7 0.129484966168870D0,0.D0               ,0.D0               ,
     8 0.101228536290376D0,0.222381034453374D0,0.313706645877887D0,
     8 0.362683783378362D0,0.362683783378362D0,0.313706645877887D0,
     8 0.222381034453374D0,0.101228536290376D0,0.D0               ,
     9 0.081274388361574D0,0.180648160694857D0,0.260610696402935D0,
     9 0.312347077040003D0,0.330239355001260D0,0.312347077040003D0,
     9 0.260610696402935D0,0.180648160694857D0,0.081274388361574D0/
      DATA A_GAUSS / 
     1 0.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     1 0.D0               ,0.D0               ,0.D0               ,
     2 -.577350269189625D0,0.577350269189625D0,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               ,
     2 0.D0               ,0.D0               ,0.D0               , 	
     3 -.774596669241483D0,0.D0               ,0.774596669241483D0,
     3 0.D0               ,0.D0               ,0.D0               ,
     3 0.D0               ,0.D0               ,0.D0               ,
     4 -.861136311594053D0,-.339981043584856D0,0.339981043584856D0,
     4 0.861136311594053D0,0.D0               ,0.D0               ,
     4 0.D0               ,0.D0               ,0.D0               ,
     5 -.906179845938664D0,-.538469310105683D0,0.D0               ,
     5 0.538469310105683D0,0.906179845938664D0,0.D0               ,
     5 0.D0               ,0.D0               ,0.D0               ,
     6 -.932469514203152D0,-.661209386466265D0,-.238619186083197D0,
     6 0.238619186083197D0,0.661209386466265D0,0.932469514203152D0,
     6 0.D0               ,0.D0               ,0.D0               ,
     7 -.949107912342759D0,-.741531185599394D0,-.405845151377397D0,
     7 0.D0               ,0.405845151377397D0,0.741531185599394D0,
     7 0.949107912342759D0,0.D0               ,0.D0               ,
     8 -.960289856497536D0,-.796666477413627D0,-.525532409916329D0,
     8 -.183434642495650D0,0.183434642495650D0,0.525532409916329D0,
     8 0.796666477413627D0,0.960289856497536D0,0.D0               ,
     9 -.968160239507626D0,-.836031107326636D0,-.613371432700590D0,
     9 -.324253423403809D0,0.D0               ,0.324253423403809D0,
     9 0.613371432700590D0,0.836031107326636D0,0.968160239507626D0/
C
C=======================================================================
C   S o u r c e  L i n e s
C=======================================================================
C
      GBUF  => ELBUF_STR%GBUF
      BID   = ZERO
      IBID  = 0
      IGTYP = IPARG(38)
      MASS = ZERO
      KNOTLOCX = ZERO
      KNOTLOCY = ZERO
      KNOTLOCZ = ZERO
      KNOTLOCELX = ZERO
      KNOTLOCELY = ZERO
      KNOTLOCELZ = ZERO
      DO I=LFT,LLT
       TBID(I)=ZERO
       DO J=1,NCTRL
        MASS(J,I) = ZERO 
       ENDDO
       DO J=1,8
        TBID2(J,I)=ZERO
       ENDDO
      ENDDO
C
      NF1=NFT+1
C-----------------------------------------------
      DO I=LFT,LLT
        DO J=1,NCTRL  !  IXI ... certainement en dehors de numnod
          XX(J,I)=X(1,IXIG3D(KXIG3D(4,I+NFT)+J-1))
          YY(J,I)=X(2,IXIG3D(KXIG3D(4,I+NFT)+J-1))
          ZZ(J,I)=X(3,IXIG3D(KXIG3D(4,I+NFT)+J-1))
          VX(J,I)=V(1,IXIG3D(KXIG3D(4,I+NFT)+J-1))
          VY(J,I)=V(2,IXIG3D(KXIG3D(4,I+NFT)+J-1))
          VZ(J,I)=V(3,IXIG3D(KXIG3D(4,I+NFT)+J-1))
          WW(J,I)=1!WIGE(IXIG3D(KXIG3D(4,I+NFT)+J-1))
          DO K=1,PX+1
            KNOTLOCX(K,J,I)=KNOTLOCPC(K,1,(KXIG3D(2,I+NFT)-1)*NUMNOD+IXIG3D(KXIG3D(4,I+NFT)+J-1))
          ENDDO
          DO K=1,PY+1
            KNOTLOCY(K,J,I)=KNOTLOCPC(K,2,(KXIG3D(2,I+NFT)-1)*NUMNOD+IXIG3D(KXIG3D(4,I+NFT)+J-1))
          ENDDO
          DO K=1,PZ+1
            KNOTLOCZ(K,J,I)=KNOTLOCPC(K,3,(KXIG3D(2,I+NFT)-1)*NUMNOD+IXIG3D(KXIG3D(4,I+NFT)+J-1))
          ENDDO
        ENDDO
        EL_ID(I)=KXIG3D(5,I+NFT)
        IDX(I) = KXIG3D(6,I+NFT)
        IDY(I) = KXIG3D(7,I+NFT)
        IDZ(I) = KXIG3D(8,I+NFT)
        IDX2(I) = KXIG3D(9,I+NFT)
        IDY2(I) = KXIG3D(10,I+NFT)
        IDZ2(I) = KXIG3D(11,I+NFT)
        KNOTLOCELX(1,I) = KNOTLOCEL(1,1,I+NFT)
        KNOTLOCELY(1,I) = KNOTLOCEL(1,2,I+NFT)
        KNOTLOCELZ(1,I) = KNOTLOCEL(1,3,I+NFT)
        KNOTLOCELX(2,I) = KNOTLOCEL(2,1,I+NFT)
        KNOTLOCELY(2,I) = KNOTLOCEL(2,2,I+NFT)
        KNOTLOCELZ(2,I) = KNOTLOCEL(2,3,I+NFT)
      ENDDO
      IPID = IPARG(62)
      IAD_KNOT = IGEO(40,IPID)
      N1 = IGEO(44,IPID)
      N2 = IGEO(45,IPID)
      N3 = IGEO(46,IPID)
      NKNOT1 = N1+PX
      NKNOT2 = N2+PY
      NKNOT3 = N3+PZ
C-----------------------------------------------
      DO I=LFT,LLT
        IPROP(I)=KXIG3D(2,I+NFT)
        IMAT(I) =KXIG3D(1,I+NFT)
      ENDDO
C----------------------------------------
C     INITIALISATION
C----------------------------------------
      IF(IGTYP == 47) THEN

      DO ITEL=LFT,LLT
       GBUF%VOL(ITEL)=ZERO 
      ENDDO

      CALL SCZERO3(GBUF%RHO,GBUF%SIG,GBUF%EINT,NEL)

      N=0

      DO I=1,PX
       DO J=1,PY
        DO K=1,PZ 

         LBUF => ELBUF_STR%BUFLY(1)%LBUF(I,J,K)
         MBUF => ELBUF_STR%BUFLY(1)%MAT(I,J,K)
         N=N+1
 
         DO ITEL=LFT,LLT 

          LBUF%RHO(ITEL) = PM(89,IMAT(ITEL))
          ZR = A_GAUSS(I,PX)
          ZS = A_GAUSS(J,PY)
          ZT = A_GAUSS(K,PZ)
          PGAUSS=W_GAUSS(I,PX)*W_GAUSS(J,PY)*W_GAUSS(K,PZ)

c          CALL IG3DDERIV(
c     1      ITEL      ,N             ,XX(:,ITEL)  ,YY(:,ITEL),
c     2      ZZ(:,ITEL),WW(:,ITEL)    ,IDX(ITEL)   ,IDY(ITEL) ,
c     3      IDZ(ITEL) ,DRDXI         ,R           ,DETJAC    ,
c     4      NCTRL     ,ZR            ,ZS          ,ZT        ,
c     5      KNOT(IAD_KNOT+1),KNOT(IAD_KNOT+NKNOT1+1),KNOT(IAD_KNOT+NKNOT1+NKNOT2+1),PX-1, 
c     6      PY-1      ,PZ-1          ,1)

          CALL IG3DONEDERIV(
     1      ITEL      ,N             ,XX(:,ITEL)  ,YY(:,ITEL),
     2      ZZ(:,ITEL),WW(:,ITEL)    ,IDX(ITEL)   ,IDY(ITEL) ,
     3      IDZ(ITEL) ,KNOTLOCX(:,:,ITEL) ,KNOTLOCY(:,:,ITEL),KNOTLOCZ(:,:,ITEL) ,
     4      DRDXI     ,R             ,DETJAC      ,NCTRL    ,
     5      ZR        ,ZS            ,ZT          ,KNOT(IAD_KNOT+1),
     6      KNOT(IAD_KNOT+NKNOT1+1),KNOT(IAD_KNOT+NKNOT1+NKNOT2+1),PX-1, 
     7      PY-1      ,PZ-1          ,1           ,
     8      IDX2(ITEL),IDY2(ITEL)    ,IDZ2(ITEL)  ,
     9      KNOTLOCELX(:,ITEL),KNOTLOCELY(:,ITEL),KNOTLOCELZ(:,ITEL))
        
          LBUF%VOL(ITEL)= PGAUSS*DETJAC 

          IF (PX*PY*PZ/=1) THEN
           GBUF%VOL(ITEL)=GBUF%VOL(ITEL) + LBUF%VOL(ITEL)
          ENDIF

C----------------------------------------
C     INITIALISATION DES MASSES   
C----------------------------------------

          CALL IG3DMASS3(
     1      LBUF%RHO,MS      ,PARTSAV ,XX     ,YY         ,   
     2      ZZ      ,VX      ,VY      ,VZ     ,IPARTIG3D(NF1),
     3      MSIG3D  ,LBUF%VOL,TBID    ,TBID2  ,BID        ,
     4      BID     ,BID     ,TBID    ,TBID   ,TBID       ,
     5      TBID2   ,TBID    ,BID     ,BID    ,NCTRL      ,
     6      KXIG3D  ,IXIG3D  ,R       ,DETJAC ,PGAUSS     ,
     7      ITEL)

         ENDDO

         CALL MATINI(PM      ,IXIG3D ,SIXIG3D   ,X      ,  
     1               GEO     ,ALE_CONNECTIVITY  ,DETONATORS,IPARG  ,
     2               SIGI    ,NEL    ,SKEW      ,IGEO   ,
     3               IPART   ,IPARTIG3D ,
     4               IMAT    ,IPM    ,NSIGS     ,NUMSOL ,PTSOL  ,
     5               IBID    ,NGL    ,NPF       ,TF     ,BUFMAT ,
     6               GBUF    ,LBUF   ,MBUF      ,ELBUF_STR,IBID ,
     7               TBID    ,TBID   )

C------------------------------------------
C       CALCUL DES DT ELEMENTAIRES
C------------------------------------------
        CALL DTMAIN(GEO       ,PM        ,IPM         ,IPROP   ,IMAT    ,FV    ,
     .              LBUF%EINT ,LBUF%TEMP ,LBUF%DELTAX ,LBUF%RK ,LBUF%RE ,BUFMAT,
     .              TBID      ,TBID      ,TBID        ,TBID    ,IGEO    )

        ENDDO
       ENDDO
      ENDDO

      IF (PX*PY*PZ/=1) THEN

       DO I=1,PX
        DO J=1,PY
         DO K=1,PZ 

          LBUF => ELBUF_STR%BUFLY(1)%LBUF(I,J,K)

          CALL SVALUE0(
     1      LBUF%RHO,LBUF%VOL,LBUF%OFF,LBUF%SIG,LBUF%EINT,DTX , 
     2      GBUF%RHO,GBUF%VOL,GBUF%OFF,GBUF%SIG,GBUF%EINT,DTX0,
     3      NEL     )

         ENDDO
        ENDDO
       ENDDO
      ENDIF

C------------------------------------------
C     assemblage des Volumes nodaux et Modules nodaux
C     (pour rigidites d'interface)
C------------------------------------------
      IF(I7STIFS/=0)THEN
        CALL BULKIGE3(GBUF%VOL ,NCTRL     ,IMAT      ,PM     ,
     2                VNIGE(1,NF1),BNIGE(1,NF1),PX ,
     3                PY       ,PZ     ,NCTRLMAX  )
      ENDIF

      ENDIF

C------------------------------------------

      RETURN
      END
C

