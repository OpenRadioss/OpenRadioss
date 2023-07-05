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
Chd|  INIVOID                       source/elements/initia/inivoid.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        C3COORI                       source/elements/sh3n/coque3n/c3coori.F
Chd|        C3EVEC3                       source/elements/sh3n/coque3n/c3evec3.F
Chd|        C3INMAS                       source/elements/sh3n/coque3n/c3inmas.F
Chd|        C3VEOK3                       source/elements/sh3n/coque3n/c3veok3.F
Chd|        CCOORI                        source/elements/shell/coque/ccoori.F
Chd|        CEVECI                        source/elements/shell/coque/ceveci.F
Chd|        CINMAS                        source/elements/shell/coque/cinmas.F
Chd|        CVEOK3                        source/elements/shell/coque/cveok3.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        PCOORI                        source/elements/beam/pcoori.F 
Chd|        PMASS                         source/elements/beam/pmass.F  
Chd|        R23MASS                       source/elements/spring/rmass.F
Chd|        S4COOR3                       source/elements/solid/solide4/s4coor3.F
Chd|        S4DERI3                       source/elements/solid/solide4/s4deri3.F
Chd|        S4MASS3                       source/elements/solid/solide4/s4mass3.F
Chd|        SBULK3                        source/elements/solid/solide/sbulk3.F
Chd|        SCOOR3                        source/elements/solid/solide/scoor3.F
Chd|        SDERI3                        source/elements/solid/solide/sderi3.F
Chd|        SDLEN3                        source/elements/solid/solide/sdlen3.F
Chd|        SMASS3                        source/elements/solid/solide/smass3.F
Chd|        TCOORI                        source/elements/truss/tcoori.F
Chd|        TMASS                         source/elements/truss/tmass.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|====================================================================
      SUBROUTINE INIVOID(ELBUF_STR   ,
     +                   IXC    ,IXS    ,IXTG   ,X      ,V      ,
     1                   PM     ,GEO    ,MS     ,IN     ,PTG    ,
     2                   MSC    ,MSS    ,MSTG   ,INC    ,INTG   ,
     3                   THKC   ,THKT   ,PARTSAV,IPARTS ,
     4                   IPARTC ,IPARTT ,VEUL   ,DTELEM ,IHBE   ,
     5                   ISOLNOD,NVC    ,I8MI   ,MSNF    ,MSSF  ,
     6                   IGEO   ,ETNOD  ,NSHNOD ,STC     ,STTG  ,
     7                   WMA    ,SH4TREE,SH3TREE,MCP     ,MCPC  ,
     8                   TEMP   ,MCPS   ,XREFC  ,XREFTG  ,XREFS ,
     9                   MSSA   ,VOLNOD ,BVOLNOD,VNS     ,BNS   ,
     A                   SH3TRIM,ISUBSTACK,STACK,RNOISE  ,PERTURB,
     B                   ELE_AREA,PART_AREA,IPARTTR,IXT  ,IPARTP,
     C                   IXP    ,MST    ,MSP    ,STT    ,STP    ,
     D                   STRP   ,INP    ,STIFINT,MCPP   ,INR    ,
     E                   MSR    ,MSRT   ,STR    ,IPARTR ,ITAB   ,
     F                   IXR    ,IMERGE2 , IADMERGE2,NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE STACK_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr12_c.inc"
#include      "units_c.inc"
#include      "random_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NVC, IHBE, ISOLNOD, NDDIM, ILEV,ISUBSTACK
      INTEGER IXC(NIXC,*),IXS(NIXS,*),IXTG(NIXTG,*),
     .        IPARTS(*),IPARTC(*),IPARTT(*),IGEO(NPROPGI,*),
     .        NSHNOD(*), SH4TREE(*), SH3TREE(*),SH3TRIM(*),
     .        PERTURB(NPERTURB),IXT(NIXT,*),IPARTTR(*),IXP(NIXP,*),IPARTP(*),
     .        ITAB(*),IXR(NIXR,*),IMERGE2(NUMNOD+1),NEL,IPARTR(*),
     .        IADMERGE2(NUMNOD+1)

      INTEGER*8 I8MI(6,*)

      my_real
     .   PM(NPROPM,*), GEO(NPROPG,*),MS(*),MSC(*),MSS(8,*),
     .   MSTG(*),INTG(*),PTG(3,*),IN(*),INC(*),THKC(*),THKT(*),
     .   X(3,*),V(3,*),VEUL(LVEUL,*),DTELEM(*),PARTSAV(20,*),
     .   MSNF(*), MSSF(8,*), WMA(*), ETNOD(*), STC(*), STTG(*),
     .   MCP(*),MCPC(*),TEMP(*),MCPS(8,*),
     .   XREFC(4,3,*),XREFTG(3,3,*),XREFS(8,3,*), MSSA(*), VOLNOD(*),
     .   BVOLNOD(*), BNS(8,*), VNS(8,*),RNOISE(*),PART_AREA(*),ELE_AREA(*),
     .   MST(*),MSP(*),STT(*),STP(*),STRP(*),INP(*),STIFINT(*),MCPP(*),
     .   INR(3,*),MSR(3,*),MSRT(*),STR(*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IGTYP,IMAT,IPROP, NDEPAR, NREFSTA, NCC, NF1
      INTEGER MXT(MVSIZ), PID(MVSIZ), NGL(MVSIZ),
     .   IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),
     .   IX5(MVSIZ),IX6(MVSIZ),IX7(MVSIZ),IX8(MVSIZ),IBID(MVSIZ),IP,II(6),
     .   ID, IPID, J, I0,I1,I2,I3,ITMP, KK, K,ILENG,IMASS,KK1
      my_real
     .   AREA(MVSIZ), VOID(MVSIZ),RHO(MVSIZ),VOL(MVSIZ),
     .   X1(MVSIZ),X2(MVSIZ),X3(MVSIZ),X4(MVSIZ),X5(MVSIZ),X6(MVSIZ),
     .   X7(MVSIZ),X8(MVSIZ),Y1(MVSIZ),Y2(MVSIZ),Y3(MVSIZ),Y4(MVSIZ),
     .   Y5(MVSIZ),Y6(MVSIZ),Y7(MVSIZ),Y8(MVSIZ),Z1(MVSIZ),Z2(MVSIZ),
     .   Z3(MVSIZ),Z4(MVSIZ),Z5(MVSIZ),Z6(MVSIZ),Z7(MVSIZ),Z8(MVSIZ),
     .   RX(MVSIZ) ,RY(MVSIZ) ,RZ(MVSIZ) ,SX(MVSIZ) ,
     .   SY(MVSIZ) ,SZ(MVSIZ) ,TX(MVSIZ) ,TY(MVSIZ) ,TZ(MVSIZ) ,
     .   E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ),E2X(MVSIZ),
     .   E2Y(MVSIZ),E2Z(MVSIZ),E3X(MVSIZ),E3Y(MVSIZ),E3Z(MVSIZ),
     .   F1X(MVSIZ) ,F1Y(MVSIZ) ,F1Z(MVSIZ) ,
     .   F2X(MVSIZ) ,F2Y(MVSIZ) ,F2Z(MVSIZ),BID(MVSIZ),RHOCP(MVSIZ),
     .   TEMP0(MVSIZ) ,FILL(MVSIZ),BIDG(MVSIZ),
     .   PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),PX4(MVSIZ),
     .   PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),PY4(MVSIZ),
     .   PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),PZ4(MVSIZ),VOLU(MVSIZ),
     .   X2L(MVSIZ),X3L(MVSIZ),Y2L(MVSIZ),Y3L(MVSIZ),
     .   X31(MVSIZ),Y31(MVSIZ),Z31(MVSIZ), DELTAX(MVSIZ),STIFNTMP(NUMNOD),
     .   BIDON, NOISE, XL(MVSIZ),LENGTH,UINER(MVSIZ),MASSR(MVSIZ),
     .   XM, XINE,RATIO,STI,KX,EMS(MVSIZ),RHOR
      DOUBLE PRECISION
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ),VOLDP(MVSIZ)
       CHARACTER*nchartitle,
     .   TITR
C
      TYPE(ELBUF_STRUCT_) ,POINTER :: BIDBUF
      TYPE (STACK_PLY) :: STACK
      TYPE(G_BUFEL_),POINTER :: GBUF
C-----------------------------------------------
      GBUF => ELBUF_STR%GBUF

      BIDBUF => NULL()
C
      IBID(1:MVSIZ) = 0
      BID(1:MVSIZ) = ZERO
      NREFSTA = NXREF
      NXREF = 0
      NF1=NFT+1
      IF (ITY == 1.AND. ISMSTR == 10) ISMSTR = 4
C--------------------------------------
      IF(ITY == 1.AND.ISOLNOD == 4)THEN
C Solid 4 nodes tetrahedron
        CALL S4COOR3(X    ,XREFS(1,1,NFT+1),IXS(1,NFT+1),NGL  ,
     .               MXT  ,PID  ,IX1  ,IX2  ,IX3  ,IX4  ,
     .               X1   ,X2   ,X3   ,X4   ,Y1   ,Y2   ,
     .               Y3   ,Y4   ,Z1   ,Z2   ,Z3   ,Z4   )
        DO I=1,NEL
          RHO(I) = PM(89,MXT(I))
          DTELEM(NFT+I) = EP30
          FILL(I) = ONE
          RHOCP(I) =  PM(69,MXT(I))
          TEMP0(I) =  PM(79,MXT(I))
        ENDDO
        CALL S4DERI3(VOL,VEUL(1,NFT+1) ,GEO ,IGEO   ,RX  ,
     .       RY    ,RZ    ,SX    ,SY    ,
     .       SZ    ,TX    ,TY    ,TZ    ,
     .       X1    ,X2    ,X3    ,X4    ,Y1    ,Y2    ,
     .       Y3    ,Y4    ,Z1    ,Z2    ,Z3    ,Z4    ,
     .       PX1   ,PX2   ,PX3   ,PX4   ,
     .       PY1   ,PY2   ,PY3   ,PY4   ,
     .       PZ1   ,PZ2   ,PZ3   ,PZ4   ,BIDG,
     .       DELTAX,VOLU  ,NGL   ,PID   ,MXT ,
     .       PM    ,VOLDP )
        IF(JLAG+JALE+JEUL /= 0) THEN
          CALL S4MASS3(
     1      RHO          ,MS          ,PARTSAV,X            ,V   ,
     2      IPARTS(NFT+1),MSS(1,NFT+1),MSNF   ,MSSF(1,NFT+1),WMA ,
     3      RHOCP        ,MCP         ,MCPS(1,NFT+1)          ,TEMP0,
     4      TEMP         ,MSSA        ,IX1     ,IX2    ,IX3    ,IX4 ,
     5      FILL, VOLU)
        ENDIF
        IF(I7STIFS /= 0)THEN
          NCC=4
          CALL SBULK3(VOLU  ,IX1    ,NCC,MXT,PM ,
     2                VOLNOD,BVOLNOD,VNS(1,NF1),BNS(1,NF1),BID,
     3                BID   ,FILL   )
        ENDIF
C------------------------------------------
      ELSEIF(ITY == 1.AND.ISOLNOD == 10)THEN
C Solid 10 nodes tetrahedron not supported
      ELSEIF(ITY == 1.AND.ISOLNOD == 16)THEN
C Solid 16 nodes brick not supported
      ELSEIF(ITY == 1.AND.ISOLNOD == 20)THEN
C Solid 20 nodes brick not supported
C--------------------------------------
      ELSEIF(ITY == 1)THEN
C Solid  8 nodes brick
        DO I=1,NEL
          RHOCP(I) = ZERO
          TEMP0(I) = ZERO
        ENDDO
        CALL SCOOR3(X,XREFS(1,1,NFT+1),IXS(1,NFT+1),GEO  ,MXT  ,PID  ,NGL  ,
     .           IX1  ,IX2  ,IX3  ,IX4  ,IX5  ,IX6  ,IX7  ,IX8  ,
     .           X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .           Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .           Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,
     .           RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,
     .           E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  ,
     .           F1X  ,F1Y  ,F1Z  ,F2X  ,F2Y  ,F2Z  ,TEMP0, TEMP,
     .           XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .           YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .           ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   )
        IF((JEUL == 0.OR.INTEG8 == 0).AND. NPT /= 8) THEN
          DO I=1,NEL
            RHO(I) = PM(89,MXT(I))
            DTELEM(NFT+I) = EP30
            FILL(I) = ONE
          ENDDO
          CALL SDERI3(VOL ,VEUL(1,NFT+1) ,GEO  ,IGEO ,
     .            XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8  ,
     .            YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8  ,
     .            ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8  ,
     .            RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,NGL  ,PID  ,
     .            PX1  ,PX2  ,PX3  ,PX4  ,PY1  ,PY2  ,PY3  ,PY4  ,
     .            PZ1  ,PZ2  ,PZ3  ,PZ4, VOLU  ,VOLDP,NEL  ,JEUL ,
     .            NXREF,IMULTI_FVM )
          CALL SDLEN3(
     .            X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .            Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .            Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8,
     .            DELTAX, VOLU)
          CALL SMASS3(
     1         RHO          , MS           , PARTSAV, X    , V ,
     2         IPARTS(NFT+1), MSS(1,NFT+1) , VOLU   ,
     3         MSNF         , MSSF(1,NFT+1), BID    ,
     4         BID          , BID          , WMA    , RHOCP, MCP,
     5         MCPS(1,NFT+1), MSSA          ,BID    , BID ,FILL ,
     6         IX1, IX2, IX3, IX4, IX5, IX6, IX7, IX8)
        ENDIF
        IF(I7STIFS /= 0)THEN
          NCC=8
          CALL SBULK3(VOLU  ,IX1    ,NCC,MXT,PM ,
     2                VOLNOD,BVOLNOD,VNS(1,NF1),BNS(1,NF1),BID,
     3                BID   ,FILL   )
        ENDIF
C--------------------------------------
      ELSEIF (ITY == 3) THEN
C 4 nodes shell
        IMAT  = IXC(1,1+NFT)         ! material number
        IPROP = IXC(NIXC-1,1+NFT)    ! property number
C
        CALL CCOORI(X,XREFC(1,1,NFT+1),IXC(1,NFT+1),
     .              X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .              Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .              IX1 ,IX2 ,IX3 ,IX4 ,NGL )
        CALL CVEOK3(NVC,4,IX1,IX2,IX3,IX4)
C
        CALL CEVECI(LFT ,LLT ,AREA,
     .              X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .              Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .              E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )
C----------------------------------------------------------
C Save element area (needed in /ADMAS for shells)
        IF (IMASADD > 0) THEN
          DO I=1,NEL
            ELE_AREA(I+NFT) = AREA(I)
          ENDDO
        ENDIF
C
        CALL CINMAS(X,XREFC(1,1,NFT+1),IXC,GEO,PM,MS,IN,
     .              THKC,IHBE,PARTSAV,V,IPARTC(NFT+1),
     .              MSC(NFT+1),INC(NFT+1),AREA,
     .              I8MI ,IGEO  ,ETNOD ,IMAT     ,IPROP    ,
     .              NSHNOD ,STC(NFT+1),SH4TREE ,MCP   ,MCPC(NFT+1) ,
     .              TEMP    ,BID , BID,BID,BID,
     .              BID,BID,ISUBSTACK,IBID,BIDBUF,
     .              STACK,BIDG ,RNOISE,BID,
     .              PERTURB,IX1     ,IX2      ,IX3     ,IX4     ,IBID, IBID)

        NDEPAR=NUMELS+NFT
        DO I=1,NEL
          DTELEM(NDEPAR+I) = EP30
        ENDDO
C--------------------------------------
      ELSEIF(ITY == 7)THEN
C 3 nodes shell
        IMAT  = IXTG(1,1+NFT)          ! material number
        IPROP = IXTG(NIXTG-1,1+NFT)    ! property number
C
        CALL C3COORI(X,XREFTG(1,1,NFT+1),IXTG(1,NFT+1),NGL,
     .               X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .               Z1  ,Z2  ,Z3  ,IX1 ,IX2 ,IX3 )
        CALL C3VEOK3(NVC,IX1 ,IX2 ,IX3 )
        CALL C3EVEC3(LFT ,LLT ,AREA,
     .               X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .               Z1  ,Z2  ,Z3  ,E1X ,E2X ,E3X ,
     .               E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .               X31, Y31, Z31 ,X2L ,X3L ,Y3L )
C-------------------------------------------------
C Save element area (needed in /ADMAS for shells)
        IF (IMASADD > 0) THEN
          DO I=1,NEL
            ELE_AREA(I+NFT+NUMELC) = AREA(I)
          ENDDO
        ENDIF
C
        CALL C3INMAS(X,XREFTG(1,1,NFT+1),IXTG,GEO,PM,MS,IN,THKT,
     .               PARTSAV,V,IPARTT(NFT+1),MSTG(NFT+1),INTG(NFT+1),
     .               PTG(1,NFT+1),IGEO ,IMAT  ,IPROP   ,AREA    ,
     .               ETNOD,NSHNOD,STTG(NFT+1), SH3TREE,MCP   ,
     .               MCPS(1,NFT+1) , TEMP,SH3TRIM,ISUBSTACK,IBID,
     .               BIDBUF, STACK,BIDG ,RNOISE,
     .               BID,PERTURB,IX1   ,IX2      ,IX3    ,
     .               X2L      ,X3L    ,Y3L   ,IBID, IBID)

        NDEPAR=NUMELS+NUMELC+NUMELT+NUMELP+NUMELR+NFT
        DO I=1,NEL
          DTELEM(NDEPAR+I) = EP30
        ENDDO
C--------------------------------------
       ELSEIF(ITY == 4)THEN
C Truss element
        STIFNTMP(1:NUMNOD)=ZERO
        CALL TCOORI(X,IXT(1,NFT+1),MXT, PID, IX1, IX2,
     .             X1, X2, Y1, Y2, Z1, Z2)
C Avoid fail in output subroutine (Anim division by AREA)
        GBUF%AREA(1:NEL)= GEO(1,PID(1:NEL))
        CALL TMASS(X         ,IXT      ,GEO      ,PM          ,MS      ,
     .             STIFNTMP   ,PARTSAV ,V        ,IPARTTR(NFT+1),MST(NFT+1),
     .             STIFINT,STT(NFT+1) ,GBUF%AREA  , MXT, IX1, IX2,
     .             X1, X2, Y1, Y2, Z1, Z2)
           NDEPAR=NUMELS+NUMELC+NFT
           DO I=1,NEL
             DTELEM(NDEPAR + I) = EP30
           ENDDO
C--------------------------------------
       ELSEIF(ITY == 5)THEN
C Beam element
        STIFNTMP(1:NUMNOD)=ZERO
        CALL PCOORI(X,IXP(1,NFT+1),
     .                    MXT,PID ,IX1,IX2,IX3,DELTAX,
     .                    X1,X2,X3, Y1,Y2,Y3, Z1,Z2,Z3)
c
c        CALL PEVECI(GBUF%SKEW, X1,X3, Y1,Y3, Z1,Z3)
        CALL PMASS(GEO,PM,MS,IN,
     .             STIFNTMP,STIFNTMP,PARTSAV,V,IPARTP(NFT+1),
     .             MSP(NFT+1),INP(NFT+1),IGEO , STIFINT, STP(NFT+1),
     .             X1,X2, Y1,Y2, Z1,Z2,
     .             IX1,IX2,MXT,PID,AREA,DELTAX,STRP(NFT+1),MCP,MCPP,TEMP)
          NDEPAR=NUMELS+NUMELC+NUMELT+NFT
          DO I=1,NEL
            DTELEM(NDEPAR+I)=EP30
          ENDDO
C--------------------------------------
       ELSEIF (ITY == 6) THEN
C Spring element
          I0=IXR(1,1+NFT)
          IGTYP =  IGEO(11,I0)
C
        IF(IGTYP == 23) THEN
           BIDON = ZERO
           DO I=1,6
             II(I) = (I-1)*NEL + 1
           ENDDO
C
           NOISE = TWO*SQRT(THREE)*XALEA
C
           DO I=1,NUMGEO
             IGTYP=IGEO(11,I)
             ID=IGEO(1,I)
             CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,I),LTITR)
             IF(IGTYP == 23) GEO(4,I) = EP30 !
           ENDDO ! DO I=1,NUMGEO
C-----------------
            IPID=IXR(1,NFT+1)
            ID=IGEO(1,IPID)
            CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,IPID),LTITR)
            DO I=1,NEL
              J=I+NFT
              I0=IXR(1,J)
              I1=IXR(2,J)
              I2=IXR(3,J)
              I3=IXR(4,J)
C----------------------- to check
             IF (I1 == I2 .OR. I1 == I3 .OR. I2 == I3) THEN
               IF (I1 == I2 .OR. I1 == I3) ITMP = I1
               IF (I2 == I3) ITMP = I2
               IF (IMERGE2(ITMP) /= 0) THEN
                 CALL ANCMSG(MSGID=682,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=IXR(NIXR,J),
     .                  I2=ITAB(ITMP))
                 WRITE (IOUT,1000) ITAB(ITMP)
                 KK = 0
                 DO K=1,IADMERGE2(ITMP+1) - IADMERGE2(ITMP)
                   KK = KK + 1
                   IF (KK  ==  10) THEN
                      WRITE (IOUT,FMT=FMT_10I)
     .                (ITAB(IMERGE2(IADMERGE2(ITMP)+KK1)),KK1=0,KK-1)
                      KK = 0
                    ENDIF
                  ENDDO
                  IF (KK /= 0) THEN
                    WRITE (IOUT,FMT=FMT_10I)
     .              (ITAB(IMERGE2(IADMERGE2(ITMP)+KK1)),KK1=0,KK-1)
                  ENDIF
                ELSE
                  CALL ANCMSG(MSGID=681,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=IXR(NIXR,J) )
                ENDIF ! IF (IMERGE2(ITMP) /= 0)
              ENDIF ! IF (I1 == I2 .OR. I1 == I3 .OR. I2 == I3)
C
              IGTYP=IGEO(11,I0)
              IF (IGTYP /= 23 ) THEN
                 CALL ANCMSG(MSGID=243,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR)
              ENDIF
            ENDDO
C
           DO I=1,NEL
             J=I+NFT
             I0=IXR(1,J)
             I1=IXR(2,J)
             I2=IXR(3,J)
             I3=IXR(4,J)
             IGTYP=IGEO(11,I0)
C
             LENGTH =  SQRT(
     +               (X(1,I1)-X(1,I2))*(X(1,I1)-X(1,I2))
     +            +  (X(2,I1)-X(2,I2))*(X(2,I1)-X(2,I2))
     +            +  (X(3,I1)-X(3,I2))*(X(3,I1)-X(3,I2)) )
             IF(IGTYP  == 23) THEN
                 IMAT  = IXR(5,I+NFT)
                 RHOR = PM(1,IMAT)
                 IMASS = IGEO(4,I0)
                 MTN = 0
                 UINER(I) = ZERO
C
                 IF(IMASS == 1) THEN
                  MASSR(I) = GEO(1,I0)*LENGTH*RHOR
                  IF (LENGTH == ZERO .AND. RHOR /= ZERO) THEN
                    IPID = IXR(1,I)
                    CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,I0),LTITR)
                    CALL ANCMSG(MSGID=1664,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO_BLIND_1,
     .                          I1=ID,
     .                          C1=TITR,
     .                          I2=IXR(NIXR,I))
                  ENDIF
                 ELSEIF(IMASS == 2) THEN
                   MASSR(I) = GEO(1,I0)*RHOR
                 ENDIF
C
                XM  = MASSR(I)
                XINE= GEO(2,I0)
C
               RATIO = XM * LENGTH * LENGTH
             ENDIF ! IGTYP  == 23
           ENDDO ! DO I=1,NEL
C---------------------------------------------------------
C Initialization of nodal stiffness for contact interfaces
           IF (I7STIFS /= 0) THEN
               DO I=1,NEL
                 J=I+NFT
                 IMAT  = IXR(5,I+NFT)
                 KX    = PM(32, IMAT)
                 STR(J)= KX
               ENDDO
           ENDIF ! I7STIFS /= 0
C-------------------------------------------------------------------
C Spring type23
           MTN = 0
C
           CALL R23MASS(IXR         ,GEO ,MS        ,IN,PARTSAV     ,
     2                  X           ,V   ,IPARTR(NFT+1),XL ,MSR(1,NFT+1),
     3                  INR(1,NFT+1),MSRT,EMS      ,MASSR   ,UINER,MTN)
C
C----------------------------------------------
C Compute element time step and nodal time step
           NDEPAR=NUMELS+NUMELC+NUMELT+NUMELP+NFT
           DO I=1,NEL
             J=I+NFT
             I0=IXR(1,J)
             IGTYP=IGEO(11,I0)
             IPID=IXR(1,I+NFT)
             IF (IGTYP == 23) THEN ! to be checked carrefuly
               IMAT   = IXR(5,I+NFT)
               MTN = 0
               DTELEM(NDEPAR+I) = EP20
               GEO(4,I0)= MIN(GEO(4,I0),DTELEM(NDEPAR+I))
             ENDIF
           ENDDO
        ENDIF  ! IGTYP = 23
      ENDIF ! ITY element type
C
      NXREF = NREFSTA
C-----------
 1000 FORMAT('LIST OF POSSIBLE CNODES MERGED WITH NODE ID=',I10)
      RETURN
      END SUBROUTINE INIVOID

