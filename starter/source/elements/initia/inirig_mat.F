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
Chd|  INIRIG_MAT                    source/elements/initia/inirig_mat.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        C3COORI                       source/elements/sh3n/coque3n/c3coori.F
Chd|        C3DERII                       source/elements/sh3n/coque3n/c3derii.F
Chd|        C3EVEC3                       source/elements/sh3n/coque3n/c3evec3.F
Chd|        C3INMAS                       source/elements/sh3n/coque3n/c3inmas.F
Chd|        C3VEOK3                       source/elements/sh3n/coque3n/c3veok3.F
Chd|        CCOORI                        source/elements/shell/coque/ccoori.F
Chd|        CDERII                        source/elements/shell/coque/cderii.F
Chd|        CEVECI                        source/elements/shell/coque/ceveci.F
Chd|        CINMAS                        source/elements/shell/coque/cinmas.F
Chd|        CVEOK3                        source/elements/shell/coque/cveok3.F
Chd|        DTMAIN                        source/materials/time_step/dtmain.F
Chd|        S10COOR3_OLD                  source/elements/solid/solide10/s10coor3.F
Chd|        S10DERI3                      source/elements/solid/solide10/s10deri3.F
Chd|        S10LEN3                       source/elements/solid/solide10/s10len3.F
Chd|        S10MASS3                      source/elements/solid/solide10/s10mass3.F
Chd|        S4COOR3                       source/elements/solid/solide4/s4coor3.F
Chd|        S4DERI3                       source/elements/solid/solide4/s4deri3.F
Chd|        S4MASS3                       source/elements/solid/solide4/s4mass3.F
Chd|        SCOOR3                        source/elements/solid/solide/scoor3.F
Chd|        SDERI3                        source/elements/solid/solide/sderi3.F
Chd|        SDLEN3                        source/elements/solid/solide/sdlen3.F
Chd|        SMASS3                        source/elements/solid/solide/smass3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|====================================================================
      SUBROUTINE INIRIG_MAT(IXC ,IXS    ,IXTG   ,IXS10  ,X      ,
     2                      V   ,PM     ,GEO    ,MS     ,IN     ,
     3                      PTG ,MSC    ,MSS    ,MSTG   ,INC    ,
     4                      INTG,THKC   ,THKT   ,PARTSAV,IPARTS ,
     5                   IPARTC ,IPARTT ,VEUL   ,DTELEM ,IHBE   ,
     6                   ISOLNOD,NVC    ,I8MI   ,MSNF    ,MSSF  ,
     7                   IGEO   ,ETNOD  ,NSHNOD ,STC     ,STTG  ,
     8                   WMA    ,SH4TREE,SH3TREE,MCP     ,MCPC  ,
     9                   TEMP   ,MCPS   ,MSSX   ,MCPSX   ,INS   ,
     A                   STIFN  ,STIFR  ,CONNEC ,IRIG_NODE,NUMEL,
     B                   NINDX  ,XREFC  ,XREFTG ,XREFS   ,MSSA  ,
     C                   SH3TRIM,ISUBSTACK,BUFMAT,IPM    ,STACK ,
     D                   RNOISE ,STRC  ,STRTG   ,PERTURB ,NEL   ,
     E                   GROUP_PARAM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE STACK_MOD
      USE MATPARAM_DEF_MOD            
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
#include      "remesh_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NVC, IHBE, ISOLNOD, NDDIM, ILEV,NRIG_MAT,NUMEL,
     .        NINDX, NREFSTA,ISUBSTACK,NEL
       INTEGER IXC(NIXC,*),IXS(NIXS,*),IXTG(NIXTG,*),IXS10(6,*),
     .        IPARTS(*),IPARTC(*),IPARTT(*),IGEO(NPROPGI,*),
     .        NSHNOD(*), SH4TREE(KSH4TREE,*), SH3TREE(*),
     .        IRIG_NODE(*) ,CONNEC(NUMEL, 10),SH3TRIM(*)
      INTEGER *8 I8MI(6,*),IPM(NPROPMI,*),PERTURB(NPERTURB)
      my_real 
     .   PM(NPROPM,*), GEO(NPROPG,*),MS(*),MSC(*),MSS(8,*), 
     .   MSTG(*),INTG(*),PTG(3,*),IN(*),INC(*),THKC(*),THKT(*), 
     .   X(3,*),V(3,*),VEUL(LVEUL,*),DTELEM(*),PARTSAV(20,*),
     .   MSNF(*), MSSF(8,*), WMA(*), ETNOD(*), STC(*), STTG(*),
     .   MCP(*),MCPC(*),TEMP(*),MCPS(8,*),INS(8,*),MCPSX(12,*),
     .   MSSX(12,*),STIFN(*), STIFR(*),
     .   XREFC(4,3,*),XREFTG(3,3,*),XREFS(8,3,*), MSSA(*),BUFMAT(*),
     .   RNOISE(*),STRC(*),STRTG(*)
      TYPE(GRP_PARAM_STRUCT_)  :: GROUP_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IGTYP, NDEPAR, NDSA,NC(20),N,JJ,KK,J,K,IP,NF1,NF2,
     .   ILAW,IMAT,IPROP,IE,IBID(mvsiz)
      INTEGER NC10(MVSIZ,10)
      INTEGER, DIMENSION(MVSIZ) :: IX1,IX2,IX3,IX4,IX5,IX6,IX7,IX8,MXT,PID,NGL
      my_real
     .   VOID(MVSIZ),MSPHI(MVSIZ),INPHI(MVSIZ),XPHI(MVSIZ), 
     .   THKPHI(MVSIZ),PV(MVSIZ),
     .   RX(MVSIZ) ,RY(MVSIZ) ,RZ(MVSIZ) ,SX(MVSIZ) ,
     .   SY(MVSIZ) ,SZ(MVSIZ) ,TX(MVSIZ) ,TY(MVSIZ) ,TZ(MVSIZ) ,
     .   F1X(MVSIZ) ,F1Y(MVSIZ) ,F1Z(MVSIZ) ,
     .   F2X(MVSIZ) ,F2Y(MVSIZ) ,F2Z(MVSIZ),BID(mvsiz),RHOCP(MVSIZ),
     .   TEMP0(MVSIZ),FILL(MVSIZ),BIDG(MVSIZ),
     .   PX1(MVSIZ),PX2(MVSIZ),PX3(MVSIZ),PX4(MVSIZ),
     .   PY1(MVSIZ),PY2(MVSIZ),PY3(MVSIZ),PY4(MVSIZ),
     .   PZ1(MVSIZ),PZ2(MVSIZ),PZ3(MVSIZ),PZ4(MVSIZ),
     .   VOID6(NEL,6), MASS(MVSIZ),
     .   VOLP(MVSIZ,5),DELTAX(MVSIZ),DELTAX2(MVSIZ),
     .   XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10),
     .   VX(MVSIZ,10), VY(MVSIZ,10), VZ(MVSIZ,10),
     .   PX(MVSIZ,10,5),PY(MVSIZ,10,5),PZ(MVSIZ,10,5),
     .   MASSCP(MVSIZ),VOLU(MVSIZ),VOLG(MVSIZ),
     .   STI(MVSIZ) ,NX(MVSIZ,10,5),DTX(MVSIZ),STIE,
     .   A11,B1,B2,STIR,BID1(MVSIZ), DDELTAX(MVSIZ)
      my_real, DIMENSION(MVSIZ) :: AREA,VOL,RHO,ALDT,DT,
     .                             X1,X2,X3,X4,X5,X6,X7,X8,
     .                             Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,
     .                             Z1,Z2,Z3,Z4,Z5,Z6,Z7,Z8,
     .                             E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z,
     .                             X2L,X3L,X4L,Y2L,Y3L,Y4L,X31,Y31,Z31
      my_real
     .        V_PITER(NEL,3,10)
C
      TYPE(ELBUF_STRUCT_) ,POINTER :: BIDBUF
      TYPE (STACK_PLY) :: STACK
      DOUBLE PRECISION 
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ),VOLDP(MVSIZ)     
C-----------------------------------------------
      BIDBUF => NULL()
C
      IBID(1:MVSIZ) = 0
      BID(1:MVSIZ) = ZERO
      NREFSTA = NXREF
      NXREF = 0
      IF (ITY == 1.AND. ISMSTR == 10) ISMSTR = 4
c--------------------------------------
      IF (ITY == 1.AND. ISOLNOD == 4)THEN
C       SOLID TETRA 4 nodes
c--------------------------------------
        CALL S4COOR3(X    ,XREFS(1,1,NFT+1),IXS(1,NFT+1),NGL  ,MXT  ,
     .               PID  ,IX1   ,IX2    ,IX3     ,IX4  ,
     .               X1   ,X2   ,X3   ,X4   ,Y1   ,Y2   ,
     .               Y3   ,Y4   ,Z1   ,Z2   ,Z3   ,Z4   )
        DO I=LFT,LLT
          RHO(I) = PM(89,MXT(I))
          DTELEM(NFT+I) = 1.E30
          FILL(I) = UN
          RHOCP(I) =  PM(69,MXT(I))
          TEMP0(I) =  PM(79,MXT(I))
        ENDDO
C        
         DO I=LFT,LLT
          N = I + NFT
          NINDX = NINDX + 1
          NC(1)  =IXS(2,N)
          NC(2)  =IXS(4,N)
          NC(3)  =IXS(6,N)
          NC(4)  =IXS(7,N)
c          IRIG_ELEM(NINDX) = 1
           DO J = 1,4
              JJ = NC(J)
              IRIG_NODE(JJ) = 1 
              CONNEC(NINDX, J) = NC(J)
          ENDDO 
        ENDDO
C       
        CALL S4DERI3(VOL,VEUL(1,NFT+1) ,GEO ,IGEO ,RX    ,
     .       RY    ,RZ    ,SX    ,SY    ,
     .       SZ    ,TX    ,TY    ,TZ    ,      
     .       X1    ,X2    ,X3    ,X4    ,Y1    ,Y2    ,    
     .       Y3    ,Y4    ,Z1    ,Z2    ,Z3    ,Z4    ,    
     .       PX1   ,PX2   ,PX3   ,PX4   ,                   
     .       PY1   ,PY2   ,PY3   ,PY4   ,                   
     .       PZ1   ,PZ2   ,PZ3   ,PZ4   ,BID1, 
     .       DDELTAX,VOLU ,NGL   ,PID   ,MXT ,
     .       PM    ,VOLDP )                   
     
        IF(JLAG+JALE+JEUL > 0) THEN
          CALL S4MASS3(
     1      RHO          ,MS          ,PARTSAV,X            ,V   ,
     2      IPARTS(NFT+1),MSS(1,NFT+1),MSNF   ,MSSF(1,NFT+1),WMA ,
     3      RHOCP        ,MCP         ,MCPS(1,NFT+1)          ,TEMP0,
     4      TEMP         ,MSSA        ,IX1    ,IX2    ,IX3    ,IX4  ,
     5      FILL, VOLU)
        ENDIF                   
c
       CALL DTMAIN(GEO  ,PM   ,VOID  ,PID   ,MXT   ,VOID  ,
     .             VOID ,VOID ,VOID  ,VOID  ,VOID  ,BUFMAT, DDELTAX, AREA, VOLU, DTX, IGEO)
c
        DO  I=LFT,LLT
         DTELEM(NFT+I)=DTX(I)
         STIE = QUART * RHO(I) * VOL(I) / 
     .        MAX(EM20,DTX(I)*DTX(I))
         STIFN(IXS(2,I+NFT))=STIFN(IXS(2,I+NFT)) + STIE
         STIFN(IXS(4,I+NFT))=STIFN(IXS(4,I+NFT)) + STIE
         STIFN(IXS(6,I+NFT))=STIFN(IXS(6,I+NFT)) + STIE
         STIFN(IXS(7,I+NFT))=STIFN(IXS(7,I+NFT)) + STIE
       ENDDO     
c--------------------------------------
      ELSEIF (ITY == 1.AND. ISOLNOD == 10)THEN
c--------------------------------------
C       SOLID TETRA 10 nodes  -
        NF1 = NFT+1
        NF2 = NF1-NUMELS8
        CALL S10COOR3_OLD(X,V,IXS(1,NF1),IXS10(1,NF2),
     .                XX, YY, ZZ, VX, VY, VZ, NC10,
     .                NGL,MXT,PID,MASS,DTELEM(NFT+1),STI,
     .                VOID6,VOID,VOID,VOID, 
     .                TEMP0, TEMP   ,NEL)
        CALL S10DERI3(VOLP,NGL,
     .                XX, YY, ZZ, PX,PY,PZ, NX,
     .                RX, RY, RZ, SX, SY, SZ, TX, TY, TZ,VOLU,VOID,
     .                BIDBUF,VOLG)
 	CALL S10LEN3(VOLP,NGL,DELTAX,DELTAX2,
     .  	     PX,PY,PZ,VOLU,VOID,VOLG, 
     .  	     RX, RY, RZ, SX, SY, SZ, TX, TY, TZ,
     .  	     NEL,MXT,PM,V_PITER,IBID)
C
        DO I=LFT,LLT
          MASSCP(I) = ZERO 
          MASS(I) = ZERO
          RHO(I) = PM(89,MXT(I))
          FILL(I) = UN
          DTELEM(NFT+I) = 1.E30
          RHOCP(I) =  PM(69,MXT(I))
          TEMP0(I) =  PM(79,MXT(I))
        ENDDO
        DO IP=1,NPT                                   
          DO I=LFT,LLT                                
            MASS(I) = MASS(I) + VOLP(I,IP)*RHO(I)     
            STI(I) = STI(I) + RHO(I) * VOLP(I,IP) /   
     .      MAX(EM20,DTX(I)*DTX(I))                   
          ENDDO                                       
        ENDDO                                         
C
        CALL S10MASS3(MASS,MS,PARTSAV,IPARTS(NF1),MSS(1,NF1),VOLU,
     .              XX  ,YY ,ZZ     ,VX         ,VY         ,VZ  ,  
     .              NC10  ,STI,STIFN  ,DELTAX2    ,MSSX(1,NF1),MASSCP,
     .              MCP ,MCPS(1,NF1),MCPSX(1,NF1),IN        ,STIFR,
     .              INS(1,NF1),MSSA ,X    ,FILL)
C
C    creation de la dependance au mat_rigid
C               
        DO I=LFT,LLT
          NINDX = NINDX + 1
          N = I + NFT
          NC(1)   = NC10(I,1)
          NC(2)   = NC10(I,2)
          NC(3)   = NC10(I,3)
          NC(4)   = NC10(I,4)
          NC(5)   = NC10(I,5)
          NC(6)   = NC10(I,6)
          NC(7)   = NC10(I,7)
          NC(8)   = NC10(I,8)
          NC(9)   = NC10(I,9)
          NC(10)  = NC10(I,10) 
          DO J = 1,10                  
            IF(NC(J) > 0 ) THEN        
               JJ = NC(J)              
                IRIG_NODE(JJ) = 1      
                CONNEC(NINDX,J) = JJ   
             ENDIF                     
          ENDDO                        
        ENDDO                           
c--------------------------------------
      ELSEIF(ITY == 1.AND.ISOLNOD == 16)THEN
C       SOLID BRICK 16 nodes  - not supported
      ELSEIF(ITY == 1.AND.ISOLNOD == 20)THEN
C       SOLID BRICK 20 nodes  - not supported
c--------------------------------------
      ELSEIF(ITY == 1)THEN
c--------------------------------------
C       SOLID BRICK 8 nodes
        DO I=LFT,LLT     
         RHOCP(I) = ZERO 
         TEMP0(I) = ZERO 
        ENDDO            
C       creation de la dependance au mat_rigid
C                
        DO I=LFT,LLT
          N = I + NFT
          NINDX = NINDX + 1
          NC(1)  =IXS(2, N)
          NC(2)  =IXS(3, N)
          NC(3)  =IXS(4, N)
          NC(4)  =IXS(5, N)
          NC(5)  =IXS(6, N)
          NC(6)  =IXS(7, N)
          NC(7)  =IXS(8, N)
          NC(8)  =IXS(9, N)
C          IRIG_ELEM(NINDX) = 1
           DO J = 1,8
              JJ = NC(J)
              IRIG_NODE(JJ) = 1 
              IF(NC(J) > 0) CONNEC(NINDX, J) = NC(J)
          ENDDO   
        ENDDO
C          
        CALL SCOOR3(X,XREFS(1,1,NFT+1),IXS(1,NFT+1) ,GEO  ,MXT  ,PID  ,NGL  ,  
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
     
          DO I=LFT,LLT
            RHO(I) = PM(89,MXT(I))
            DTELEM(NFT+I) = EP30                                         
            FILL(I) = UN
          ENDDO
          CALL SDERI3(VOL,VEUL(1,NFT+1),GEO,IGEO,
     .           XD1  ,XD2  ,XD3  ,XD4  ,XD5  ,XD6  ,XD7  ,XD8   ,
     .           YD1  ,YD2  ,YD3  ,YD4  ,YD5  ,YD6  ,YD7  ,YD8   ,
     .           ZD1  ,ZD2  ,ZD3  ,ZD4  ,ZD5  ,ZD6  ,ZD7  ,ZD8   ,
     .            RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,NGL  ,PID  ,
     .            PX1  ,PX2  ,PX3  ,PX4  ,PY1  ,PY2  ,PY3  ,PY4  ,
     .            PZ1  ,PZ2  ,PZ3  ,PZ4, VOLU  ,VOLDP ) 
          CALL SDLEN3(
     .            X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   , 
     .            Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   , 
     .            Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8, 
     .            DDELTAX, VOLU)
          CALL SMASS3(
     1         RHO          ,MS           ,PARTSAV,X           ,V ,
     2         IPARTS(NFT+1),MSS(1,NFT+1) ,VOLU   ,  
     3         MSNF         ,MSSF(1,NFT+1),IN     ,       
     4         VOID         ,INS          ,WMA    , RHOCP     ,MCP  , 
     5         MCPS(1,NFT+1),MSSA         ,BID    ,BID        ,FILL, 
     6         IX1, IX2, IX3, IX4, IX5, IX6, IX7, IX8)
C  
c       CALCUL DES DT ELEMENTAIRES
C 
        CALL DTMAIN(GEO  ,PM   ,VOID  ,PID   ,MXT   ,VOID  ,  
     .              VOID ,VOID ,VOID  ,VOID  ,VOID  ,BUFMAT, DDELTAX, AREA, VOLU, DTX , IGEO)  
C
        DO  I=LFT,LLT
          N = I + NFT
          DTELEM(N)= EP30
          STIE = QUART * RHO(I) * VOL(I) /
     .          MAX(EM20,DTX(I)*DTX(I))
          STIFN(IXS(2,N))=STIFN(IXS(2,N))+STIE
          STIFN(IXS(3,N))=STIFN(IXS(3,N))+STIE
          STIFN(IXS(4,N))=STIFN(IXS(4,N))+STIE
          STIFN(IXS(5,N))=STIFN(IXS(5,N))+STIE
          STIFN(IXS(6,N))=STIFN(IXS(6,N))+STIE
          STIFN(IXS(7,N))=STIFN(IXS(7,N))+STIE
          STIFN(IXS(8,N))=STIFN(IXS(8,N))+STIE
          STIFN(IXS(9,N))=STIFN(IXS(9,N))+STIE
        ENDDO     
c--------------------------------------
      ELSEIF (ITY.EQ.3) THEN
c--------------------------------------
C       SHELL
c
        IMAT  = IXC(1,1+NFT)         ! mat N°
        IPROP = IXC(NIXC-1,1+NFT)    ! property N°
c
        CALL CCOORI(X,XREFC(1,1,NFT+1),IXC(1,NFT+1),
     .              X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .              Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .              IX1 ,IX2 ,IX3 ,IX4 ,NGL )
c
        CALL CVEOK3(NVC,4,IX1,IX2,IX3,IX4)
        CALL CEVECI(LFT ,LLT ,AREA,
     .              X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .              Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .              E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )
c
        CALL CINMAS(X,XREFC(1,1,NFT+1),IXC,GEO,PM,MS,IN,
     .              THKC,IHBE,PARTSAV,V,IPARTC(NFT+1),
     .              MSC(NFT+1),INC(NFT+1),AREA    ,
     .              I8MI ,IGEO  ,ETNOD ,IMAT     ,IPROP    ,
     .              NSHNOD ,STC(NFT+1),SH4TREE ,MCP   ,MCPC(NFT+1) , 
     .              TEMP    ,BID , BID,BID,BID,
     .              BID,BID,ISUBSTACK,IBID,BIDBUF,
     .              STACK,BIDG, RNOISE ,BID ,
     .              PERTURB,IX1     ,IX2      ,IX3     ,IX4      )
        CALL CDERII(PX1 ,PX2 ,PY1   ,PY2,
     .              X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  , 
     .              Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .              E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .              X2L ,X3L ,X4L ,Y2L ,Y3L ,Y4L )
c
c       DT NODAL
c
        IF(NADMESH == 0)THEN
         DO I=LFT,LLT
          N=NFT+I
          A11 =PM(24,IMAT)
          B1 = PX1(I)*PX1(I)+PY1(I)*PY1(I)
          B2 = PX2(I)*PX2(I)+PY2(I)*PY2(I)
          STIE =  MAX(B1,B2)
     .         * THKC(I) * A11 / (AREA(I) )
          STIR = UNDOUZE*STIE * (THKC(I)*THKC(I) + AREA(I)) 
          STIFN(IXC(2,N))=STIFN(IXC(2,N))+STIE
          STIFN(IXC(3,N))=STIFN(IXC(3,N))+STIE
          STIFN(IXC(4,N))=STIFN(IXC(4,N))+STIE
          STIFN(IXC(5,N))=STIFN(IXC(5,N))+STIE
          STIFR(IXC(2,N))=STIFR(IXC(2,N))+STIR
          STIFR(IXC(3,N))=STIFR(IXC(3,N))+STIR
          STIFR(IXC(4,N))=STIFR(IXC(4,N))+STIR
          STIFR(IXC(5,N))=STIFR(IXC(5,N))+STIR
          STRC(N) = STIR
         ENDDO
        ELSE
         DO I=LFT,LLT
          N=NFT+I
          IF(SH4TREE(3,N) >= 0)THEN
           A11 =PM(24,IMAT)
           B1 = PX1(I)*PX1(I)+PY1(I)*PY1(I)
           B2 = PX2(I)*PX2(I)+PY2(I)*PY2(I)
           STIE =  MAX(B1,B2)
     .          * THKC(I) * A11 / AREA(I)
           STIR = UNDOUZE*STIE * (THKC(I)*THKC(I) + AREA(I))  
           STIFN(IXC(2,N))=STIFN(IXC(2,N))+STIE
           STIFN(IXC(3,N))=STIFN(IXC(3,N))+STIE
           STIFN(IXC(4,N))=STIFN(IXC(4,N))+STIE
           STIFN(IXC(5,N))=STIFN(IXC(5,N))+STIE
           STIFR(IXC(2,N))=STIFR(IXC(2,N))+STIR
           STIFR(IXC(3,N))=STIFR(IXC(3,N))+STIR
           STIFR(IXC(4,N))=STIFR(IXC(4,N))+STIR
           STIFR(IXC(5,N))=STIFR(IXC(5,N))+STIR
           STRC(N) = STIR
          END IF
         END DO
        END IF 
 
        NDEPAR=NUMELS+NFT
        DO I=LFT,LLT
          DTELEM(NDEPAR+I) = EP30
        ENDDO
C
C    creation de la dependance au mat_rigid
C                
        DO I=LFT,LLT
          N = I + NFT
          NINDX = NINDX + 1
          NC(1) = IXC(2, N)
          NC(2) = IXC(3, N)  
          NC(3) = IXC(4, N)
          NC(4) = IXC(5, N)
          CONNEC(NINDX,1) = NC(1)
          CONNEC(NINDX,2) = NC(2)
          CONNEC(NINDX,3) = NC(3)
          CONNEC(NINDX,4) = NC(4)
C
          IRIG_NODE(NC(1)) = 1
          IRIG_NODE(NC(2)) = 1 
          IRIG_NODE(NC(3)) = 1
          IRIG_NODE(NC(4)) = 1
        ENDDO                             
C
c--------------------------------------
      ELSEIF(ITY.EQ.7)THEN
c--------------------------------------
C       SHELL 3N
        IMAT  = IXTG(1,1+NFT)          ! mat N°
        IPROP = IXTG(NIXTG-1,1+NFT)    ! property N°
c
        CALL C3COORI(X,XREFTG(1,1,NFT+1),IXTG(1,NFT+1),NGL,
     .               X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .               Z1  ,Z2  ,Z3  ,IX1 ,IX2 ,IX3 )
        CALL C3VEOK3(NVC ,IX1 ,IX2 ,IX3)
        CALL C3EVEC3(LFT,LLT,AREA,
     .               X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .               Z1  ,Z2  ,Z3  ,E1X ,E2X ,E3X ,
     .               E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .               X31, Y31, Z31 ,X2L ,X3L ,Y3L )
        CALL C3INMAS(X,XREFTG(1,1,NFT+1),IXTG,GEO,PM,MS,IN,THKT,
     .               PARTSAV,V,IPARTT(NFT+1),MSTG(NFT+1),INTG(NFT+1),
     .               PTG(1,NFT+1),IGEO ,IMAT  ,IPROP   ,AREA    ,
     .               ETNOD,NSHNOD,STTG(NFT+1), SH3TREE,MCP   ,
     .               MCPS(1,NFT+1) , TEMP,SH3TRIM,ISUBSTACK,IBID,
     .               BIDBUF, STACK,BIDG ,RNOISE,
     .               BID,PERTURB,IX1   ,IX2      ,IX3    ,
     .               X2L      ,X3L    ,Y3L   )
         CALL C3DERII(LFT,LLT,PM,GEO,PX1,PY1,PY2,S
     .                TIFN   ,STIFR   ,IXTG(1,NFT+1),
     .                THKT,SH3TREE,ALDT,BUFMAT , IPM   ,IGEO ,
     .                STACK%PM , ISUBSTACK, STRTG(NFT+1),IMAT,IPROP,
     .             AREA ,DT  ,X31 ,Y31 ,Z31 ,
     .             E1X  ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,
     .             E1Z  ,E2Z ,E3Z ,X2L ,X3L ,Y3L ,
     .             GROUP_PARAM)
c
        NDEPAR=NUMELS+NUMELC+NUMELT+NUMELP+NUMELR+NFT
        DO I=LFT,LLT
          DTELEM(NDEPAR+I) = EP30
        ENDDO

C
C    creation de la dependance au mat_rigid
C          
        DO I=LFT,LLT
          N = I + NFT
          NINDX = NINDX + 1
          NC(1) = IXTG(2, N)
          NC(2) = IXTG(3, N)
          NC(3) = IXTG(4, N)
C          
           CONNEC(NINDX , 1) = NC(1)
           CONNEC(NINDX , 2) = NC(2)
           CONNEC(NINDX , 3) = NC(3)          
C           IRIG_ELEM(NINDX)  = 1
          IRIG_NODE(NC(1)) = 1
          IRIG_NODE(NC(2)) = 1 
          IRIG_NODE(NC(3)) = 1 
        ENDDO                    
      ENDIF
C
      NXREF = NREFSTA
C-----------
      RETURN
      END

