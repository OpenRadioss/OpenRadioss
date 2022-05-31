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
Chd|  I20STI3                       source/interfaces/inter3d1/i20sti3.F
Chd|-- called by -----------
Chd|        I20INI3                       source/interfaces/inter3d1/i20ini3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        I20NELTS                      source/interfaces/inter3d1/inelt.F
Chd|        I4GMX3                        source/interfaces/inter3d1/i4gmx3.F
Chd|        INCOQ3                        source/interfaces/inter3d1/incoq3.F
Chd|        INELTC                        source/interfaces/inter3d1/inelt.F
Chd|        INSOL3                        source/interfaces/inter3d1/insol3.F
Chd|        VOLINT                        source/interfaces/inter3d1/volint.F
Chd|        BITSET                        source/interfaces/inter3d1/bitget.F
Chd|        BITUNSET                      source/interfaces/inter3d1/bitget.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I20STI3(
     1 PM        ,GEO       ,X         ,MS        ,
     2 IXS       ,IXC       ,IXTG      ,IXT       ,
     3 IXP       ,WA        ,NINT      ,NTY       ,
     4 NOINT     ,NRT       ,NSN       ,IRECT     ,
     5 NSV       ,INACTI    ,GAP       ,IGAP      ,
     6 GAP_S     ,GAP_M     ,GAPMIN    ,GAPINF    ,
     7 GAPMAX    ,STFAC     ,STF       ,STFN      ,
     8 KNOD2ELS  ,KNOD2ELC  ,KNOD2ELTG ,NOD2ELS   ,
     9 NOD2ELC   ,NOD2ELTG  ,IGRSURF1  ,IFS2      ,
     A IGRSURF2  ,INTTH     ,IELES     ,
     B IELEC     ,AREAS     ,IPARTC    ,IPARTTG   ,
     C THK       ,THK_PART  ,GAP_SH    ,XANEW     ,
     D GAPSHMAX  ,NBINFLG   ,MBINFLG   ,NLN       ,
     E NLG       ,GAPSOL    ,IXS10     ,IXS16     ,
     F IXS20     ,ID,TITR   ,IGEO      ,PM_STACK  ,
     G IWORKSH )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr08_c.inc"
#include      "scr17_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRT, NINT, NTY, NOINT,NSN,IGAP, NDDIM,
     .        INACTI,IFS2,NLN      
      INTEGER IRECT(4,*), IXS(NIXS,*), IXC(NIXC,*),
     .   NSV(*), IXTG(NIXTG,*), IXT(NIXT,*), IXP(NIXP,*),
     .   KNOD2ELS(*), KNOD2ELC(*), KNOD2ELTG(*), NOD2ELS(*), NOD2ELC(*), 
     .   NOD2ELTG(*),IELES(*),INTTH,IELEC(*),
     .   IPARTC(*), IPARTTG(*),NBINFLG(*),MBINFLG(*),NLG(*) ,
     .   IXS10(6,*), IXS16(*), IXS20(*),  IGEO(NPROPGI,*),IWORKSH(3,*)
C     REAL ou REAL*8
      my_real
     .   STFAC, GAP,GAPMIN,GAPINF, GAPMAX,GAPSHMAX,GAPSOLIDMAX,GAPSOL
C     REAL ou REAL*8
      my_real
     .   X(3,*), STF(*), PM(NPROPM,*), GEO(NPROPG,*),  STFN(*),
     .   MS(*),WA(*),GAP_S(*),GAP_M(*),GAP_SH(*),AREAS(*),
     .   THK(*),THK_PART(*),XANEW(3,*),PM_STACK(20,*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      TYPE (SURF_) :: IGRSURF1
      TYPE (SURF_) :: IGRSURF2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NDX, I, J, INRT, NELS,NELS2, MT, JJ, JJJ, NELC,
     .   MG, NUM, NPT, LL, L, NN, NELTG,N1,N2,N3,N4,IE,IP,NM1,
     .  IGTYP,IPGMAT,IGMAT,ISUBSTACK
C     REAL ou REAL*8
      my_real
     .   DXM, GAPMX, GAPMN, AREA, VOL, DX,GAPS1,GAPS2, GAPM, DDX, 
     .   GAPTMP, GAPSCALE,SX1,SY1,SZ1,SX2,SY2,SZ2,SX3,SY3,SZ3,
     .   SLSFAC,GAPINFS,GAPINFM,GAPSUPS,GAPSUPM,ST
      INTEGER TAG(NUMNOD)
      INTEGER BITUNSET,BITGET,BITSET
      EXTERNAL BITUNSET,BITGET,BITSET
C--------------------------------------------------------------
C     CALCUL DES RIGIDITES DES SEGMENTS 
C     V16 : DANS LE CAS OU ONE SEGMENT APPARTIENT A LA FOIS
C           A UNE BRIQUE ET A UNE COQUE ON CHOISIT LA RIGIDITE
C           DE LA COQUE SAUF SI LE MATERIAU COQUE EST NUL.
C---------------------------------------------------------------

C---------------------------------------------------------------
      SLSFAC = ONE
C---------------------------------------------------------------
      IPGMAT = 700
      IGMAT = 0
      DO I=1,NUMNOD
        XANEW(1,I)=X(1,I)
        XANEW(2,I)=X(2,I)
        XANEW(3,I)=X(3,I)
        TAG(I)=0
      ENDDO
      DXM=0.
      NDX=0
      GAPSOLIDMAX=EP30
      GAPMX=EP30
      GAPMN=EP30
      GAPS1=ZERO
      GAPS2=ZERO
      IF(IGAP.EQ.2)THEN
        IGAP = 1
        GAPSCALE = GAPMIN
        GAPMIN   = ZERO
      ELSE
        GAPSCALE = ONE
      ENDIF  
C------------------------------------
C     GAP NOEUDS SECONDS
C------------------------------------
      IF(IGAP.GE.1)THEN
       DO I=1,NUMNOD
        WA(I)=ZERO
       ENDDO
       DO I=1,NUMELC
              MG=IXC(6,I)
              IGTYP = IGEO(11,MG)
	       IP = IPARTC(I)
	       IF ( THK_PART(IP) .NE. ZERO .AND. IINTTHICK .EQ. 0) THEN
	         DX=HALF*THK_PART(IP)
	       ELSEIF ( THK(I) .NE. ZERO .AND. IINTTHICK .EQ. 0) THEN
	         DX=HALF*THK(I)
	       ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR.IGTYP == 52)THEN
	         DX=HALF*THK(I)
               ELSE
                 DX=HALF*GEO(1,MG)
	       ENDIF
        WA(IXC(2,I))=MAX(WA(IXC(2,I)),DX)
        WA(IXC(3,I))=MAX(WA(IXC(3,I)),DX)
        WA(IXC(4,I))=MAX(WA(IXC(4,I)),DX)
        WA(IXC(5,I))=MAX(WA(IXC(5,I)),DX)
       ENDDO
       DO I=1,NUMELTG
        MG=IXTG(5,I)
        IGTYP = IGEO(11,MG)
        IP = IPARTTG(I)
        IF ( THK_PART(IP) .NE. ZERO .AND. IINTTHICK .EQ. 0) THEN
          DX=HALF*THK_PART(IP)
        ELSEIF (THK(NUMELC+I).NE.ZERO .AND. IINTTHICK.EQ.0) THEN
          DX=HALF*THK(NUMELC+I)
        ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
          DX=HALF*THK(NUMELC+I)
        ELSE
          DX=HALF*GEO(1,MG)
        ENDIF
        WA(IXTG(2,I))=MAX(WA(IXTG(2,I)),DX)
        WA(IXTG(3,I))=MAX(WA(IXTG(3,I)),DX)
        WA(IXTG(4,I))=MAX(WA(IXTG(4,I)),DX)
       ENDDO
       DO I=1,NUMELT
        MG=IXT(4,I)
        DX=HALF*SQRT(GEO(1,MG))
        WA(IXT(2,I))=MAX(WA(IXT(2,I)),DX)
        WA(IXT(3,I))=MAX(WA(IXT(3,I)),DX)
       ENDDO
       DO I=1,NUMELP
        MG=IXP(5,I)
        DX=0.5*SQRT(GEO(1,MG))
        WA(IXP(2,I))=MAX(WA(IXP(2,I)),DX)
        WA(IXP(3,I))=MAX(WA(IXP(3,I)),DX)
       ENDDO
       DO I=1,NSN
        GAP_S(I)=GAPSCALE * WA(NSV(I))
        GAPS1=MAX(GAPS1,GAP_S(I))
       ENDDO
      ENDIF
C
C calcul du surface second. ---
      IF(INTTH > 0 ) THEN
       DO I = 1,NSN    
          AREAS(I) = ZERO
          DO J= KNOD2ELC(NSV(I))+1,KNOD2ELC(NSV(I)+1)
              IE = NOD2ELC(J)
              SX1 = X(1,IXC(4,IE)) - X(1,IXC(2,IE))
              SY1 = X(2,IXC(4,IE)) - X(2,IXC(2,IE))
              SZ1 = X(3,IXC(4,IE)) - X(3,IXC(2,IE))
              SX2 = X(1,IXC(5,IE)) - X(1,IXC(3,IE))
              SY2 = X(2,IXC(5,IE)) - X(2,IXC(3,IE))
              SZ2 = X(3,IXC(5,IE)) - X(3,IXc(3,IE))
              SX3  = SY1*SZ2 - SZ1*SY2
              SY3  = SZ1*SX2 - SX1*SZ2
              SZ3  = SX1*SY2 - SY1*SX2
              AREAS(I) = AREAS(I) + ONE_OVER_8*SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
          ENDDO
             IELEC(I) = IXC(1,IE)
        ENDDO
      ENDIF
C
C------------------------------------
C     STIF NOEUDS SECONDS
C------------------------------------
      IF(SLSFAC >= ZERO)THEN
       DO I=1,NUMELC
        MG=IXC(6,I)
        IGTYP = IGEO(11,MG)
	       IP = IPARTC(I)
	       IF ( THK_PART(IP) .NE. ZERO .AND. IINTTHICK .EQ. 0) THEN
	         DX=HALF*THK_PART(IP)
	       ELSEIF ( THK(I) .NE. ZERO .AND. IINTTHICK .EQ. 0) THEN
	         DX=HALF*THK(I)
	       ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52)THEN
	         DX=HALF*THK(I)
               ELSE
                 DX=HALF*GEO(1,MG)
	       ENDIF
        WA(IXC(2,I))=MAX(WA(IXC(2,I)),DX)
        WA(IXC(3,I))=MAX(WA(IXC(3,I)),DX)
        WA(IXC(4,I))=MAX(WA(IXC(4,I)),DX)
        WA(IXC(5,I))=MAX(WA(IXC(5,I)),DX)
       ENDDO
       DO I=1,NUMELTG
        MG=IXTG(5,I)
        IGTYP = IGEO(11,MG)
        IP = IPARTTG(I)
        IF ( THK_PART(IP) .NE. ZERO .AND. IINTTHICK .EQ. 0) THEN
          DX=HALF*THK_PART(IP)
        ELSEIF (THK(NUMELC+I).NE.ZERO .AND. IINTTHICK.EQ.0) THEN
          DX=HALF*THK(NUMELC+I)
        ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52)THEN
          DX=HALF*THK(NUMELC+I)
        ELSE
          DX=HALF*GEO(1,MG)
        ENDIF
        WA(IXTG(2,I))=MAX(WA(IXTG(2,I)),DX)
        WA(IXTG(3,I))=MAX(WA(IXTG(3,I)),DX)
        WA(IXTG(4,I))=MAX(WA(IXTG(4,I)),DX)
       ENDDO
       DO I=1,NUMELT
        MG=IXT(4,I)
        DX=HALF*SQRT(GEO(1,MG))
        WA(IXT(2,I))=MAX(WA(IXT(2,I)),DX)
        WA(IXT(3,I))=MAX(WA(IXT(3,I)),DX)
       ENDDO
       DO I=1,NUMELP
        MG=IXP(5,I)
        DX=0.5*SQRT(GEO(1,MG))
        WA(IXP(2,I))=MAX(WA(IXP(2,I)),DX)
        WA(IXP(3,I))=MAX(WA(IXP(3,I)),DX)
       ENDDO
       ! new interface buffer development : GAP_S not sized when IGAP=0 (cf bufintr)
       
c       DO I=1,NSN
c        GAP_S(I)=GAPSCALE * WA(NSV(I))
c        GAPS1=MAX(GAPS1,GAP_S(I))
c       ENDDO
      ENDIF

C------------------------------------
C     SURFACE DE COQUE OU SOLIDE
C------------------------------------
C------------------------------------
C     STIF FACES MAIN
C------------------------------------

      DO 500 I=1,NRT
      STF(I)=ZERO
      IF(INTTH > 0 ) IELES(I) = 0
      IF(SLSFAC.LT.ZERO)THEN
        STF(I)=SLSFAC
      ENDIF
      GAP_SH(I)=ZERO
      GAPM  =ZERO
      INRT=I
      CALL I4GMX3(X,IRECT,INRT,GAPMX)
C----------------------
      NM1=IGRSURF1%NSEG
      IF(INRT <= NM1)THEN
        CALL I20NELTS(X            ,IRECT(1,INRT),IXS  ,NINT,NELS          ,
     .                INRT         ,AREA         ,NOINT,0   ,IGRSURF1%ELTYP,
     .                IGRSURF1%ELEM)
      ELSE
        CALL I20NELTS(X            ,IRECT(1,INRT),IXS  ,NINT,NELS          ,
     .                INRT-NM1     ,AREA         ,NOINT,0   ,IGRSURF2%ELTYP,
     .                IGRSURF2%ELEM)
      ENDIF
      IF(NELS /= 0)THEN
        MT=IXS(1,NELS)
        IF(MT.GT.0)THEN
          DO JJ=1,8
            JJJ=IXS(JJ+1,NELS)
            XC(JJ)=X(1,JJJ)
            YC(JJ)=X(2,JJJ)
            ZC(JJ)=X(3,JJJ)
          END DO
          CALL VOLINT(VOL)
          STF(I)=SLSFAC*AREA*AREA*PM(100,MT)/VOL
        ELSE
          IF(NINT.GE.0) THEN
             CALL ANCMSG(MSGID=95,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IXS(NIXS,NELS),
     .                   C2='SOLID',
     .                   I3=I)
          ENDIF
          IF(NINT.LT.0) THEN 
             CALL ANCMSG(MSGID=96,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IXS(NIXS,NELS),
     .                   C2='SOLID',
     .                   I3=I)
          ENDIF 
        ENDIF
        IF(IGAP/=0)THEN
          GAP_SH(I)=MIN(VOL/AREA,SQRT(AREA))/SIX
          GAPSOLIDMAX = MIN(GAPSOLIDMAX,VOL/(AREA*FOUR))
          GAPMN=MIN(GAPMN,HALF*GAP_SH(I))
          GAP_M(I)=ZERO
          TAG(IRECT(1,INRT)) = 1
          TAG(IRECT(2,INRT)) = 1
          TAG(IRECT(3,INRT)) = 1
          TAG(IRECT(4,INRT)) = 1
c          NBINFLG(IRECT(1,INRT))=BITUNSET(NBINFLG(IRECT(1,INRT)),7)
c          NBINFLG(IRECT(2,INRT))=BITUNSET(NBINFLG(IRECT(2,INRT)),7)
c          NBINFLG(IRECT(3,INRT))=BITUNSET(NBINFLG(IRECT(3,INRT)),7)
c          NBINFLG(IRECT(4,INRT))=BITUNSET(NBINFLG(IRECT(4,INRT)),7)
        ENDIF
        MBINFLG(I)=BITSET(MBINFLG(I),8)
        GO TO 500
      ELSE
        IF(INRT <= NM1)THEN
          CALL INELTC(NELC ,NELTG ,INRT    ,IGRSURF1%ELTYP,IGRSURF1%ELEM)
        ELSE
          CALL INELTC(NELC ,NELTG ,INRT-NM1,IGRSURF2%ELTYP,IGRSURF2%ELEM)
        ENDIF
        IF(NELTG.NE.0) THEN
          MT=IXTG(1,NELTG)
          MG=IXTG(5,NELTG)
          IGTYP = IGEO(11,MG)
          IP = IPARTTG(NELTG)
          IF ( THK_PART(IP) .NE. ZERO .AND. IINTTHICK .EQ. 0) THEN
            DX=THK_PART(IP)*GAPSCALE
          ELSEIF(THK(NUMELC+NELTG).NE.ZERO.AND.IINTTHICK.EQ.0)THEN
            DX=THK(NUMELC+NELTG)*GAPSCALE
          ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52)THEN
            DX=THK(NUMELC+NELTG)*GAPSCALE
          ELSE
            DX=GEO(1,MG)*GAPSCALE
          ENDIF
          GAPM=HALF*DX
          GAPS2=MAX(GAPS2,GAPM)
          GAPMN = MIN(GAPMN,DX)
          DXM=DXM+DX
          NDX=NDX+1
          IGMAT = IGEO(98,MG)
          IF(MT.GT.0)THEN
            IF(IGTYP == 11 .AND. IGMAT > 0) THEN
               IF ( THK(NUMELC+NELTG) .NE.ZERO.AND.IINTTHICK.EQ.0)THEN
                 STF(I)=SLSFAC*THK(NUMELC+NELTG)*GEO(IPGMAT + 2 ,MG)
                ELSE
                 STF(I)=SLSFAC*GEO(1,MG)*GEO(IPGMAT + 2 ,MG)
               ENDIF
             ELSEIF(IGTYP == 52 .OR. 
     .            ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0))THEN  
              ISUBSTACK = IWORKSH(3,NUMELC+NELTG)
              ST=PM_STACK(2,ISUBSTACK) 
              STF(I)=SLSFAC*THK(NUMELC+NELTG)*ST
            ELSE
               IF ( THK(NUMELC+NELTG) .NE.ZERO.AND.IINTTHICK.EQ.0)THEN
                 STF(I)=SLSFAC*THK(NUMELC+NELTG)*PM(20,MT)
               ELSEIF(IGTYP == 17 .OR. IGTYP == 51) THEN
                 STF(I)=SLSFAC*THK(NUMELC+NELTG)*PM(20,MT)
                ELSE
                 STF(I)=SLSFAC*GEO(1,MG)*PM(20,MT)
               ENDIF
           ENDIF
          ELSE
            IF(NINT.GE.0) THEN
               CALL ANCMSG(MSGID=95,
     .                     MSGTYPE=MSGWARNING,
     .                     ANMODE=ANINFO_BLIND_2,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=IXTG(NIXTG,NELTG),
     .                     C2='SHELL',
     .                     I3=I)
            END IF
            IF(NINT.LT.0) THEN
               CALL ANCMSG(MSGID=96,
     .                     MSGTYPE=MSGWARNING,
     .                     ANMODE=ANINFO_BLIND_2,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=IXTG(NIXTG,NELTG),
     .                     C2='SHELL',
     .                     I3=I)
            END IF
          END IF
          IF(IGAP/=0) GAP_M(I)=GAPM
          MBINFLG(I)=BITSET(MBINFLG(I),3)
          GO TO 500
        ELSEIF(NELC.NE.0) THEN
          MT=IXC(1,NELC)
          MG=IXC(6,NELC)
          IGTYP = IGEO(11,MG)
          IP = IPARTC(NELC)
          IGMAT = IGEO(99,MG)
          IF ( THK_PART(IP) .NE. ZERO .AND. IINTTHICK .EQ. 0) THEN
            DX=THK_PART(IP)*GAPSCALE
          ELSEIF ( THK(NELC) .NE. ZERO .AND. IINTTHICK .EQ. 0) THEN
            DX=THK(NELC)*GAPSCALE
          ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
            DX=THK(NELC)*GAPSCALE
          ELSE
            DX=GEO(1,MG)*GAPSCALE
          ENDIF
          GAPM=HALF*DX
          GAPS2=MAX(GAPS2,GAPM)
          GAPMN = MIN(GAPMN,DX)
          DXM=DXM+DX
          NDX=NDX+1
          IF(MT.GT.0)THEN
            IF(IGTYP == 11 .AND. IGMAT > 0) THEN
              IF ( THK(NELC) .NE. ZERO .AND. IINTTHICK .EQ. 0) THEN 
                STF(I)=SLSFAC*THK(NELC)*GEO(IPGMAT + 2 ,MG)
              ELSE
                STF(I)=SLSFAC*GEO(1,MG)*GEO(IPGMAT + 2 ,MG)
              ENDIF            
            ELSEIF(IGTYP==52 .OR.
     .            ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0))THEN
              ISUBSTACK = IWORKSH(3,NELC)
               ST=PM_STACK(2,ISUBSTACK) 
               STF(I)=SLSFAC*THK(NELC)*ST
            ELSE
              IF ( THK(NELC) .NE. ZERO .AND. IINTTHICK .EQ. 0) THEN 
                STF(I)=SLSFAC*THK(NELC)*PM(20,MT)
              ELSEIF(IGTYP == 17) THEN
                STF(I)=SLSFAC*THK(NELC)*PM(20,MT)
              ELSE
                STF(I)=SLSFAC*GEO(1,MG)*PM(20,MT)
              ENDIF
           ENDIF 
          ELSE
            IF(NINT.GE.0) THEN
               CALL ANCMSG(MSGID=95,
     .                     MSGTYPE=MSGWARNING,
     .                     ANMODE=ANINFO_BLIND_2,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=IXC(NIXC,NELC),
     .                     C2='SHELL',
     .                     I3=I)
            END IF
            IF(NINT.LT.0) THEN
               CALL ANCMSG(MSGID=96,
     .                     MSGTYPE=MSGWARNING,
     .                     ANMODE=ANINFO_BLIND_2,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=IXC(NIXC,NELC),
     .                     C2='SHELL',
     .                     I3=I)
            END IF
          END IF
          IF(IGAP/=0) GAP_M(I)=GAPM
          MBINFLG(I)=BITSET(MBINFLG(I),4)
          GO TO 500
        END IF
      END IF
C----------------------
C     SURFACE DE SEGMENTS
C----------------------
C----------------------
C     ELEMENTS SOLIDES
C----------------------
      CALL INSOL3(X,IRECT,IXS,NINT,NELS,INRT,
     .            AREA,NOINT,KNOD2ELS ,NOD2ELS ,0,IXS10,
     .            IXS16,IXS20)
      IF(NELS.NE.0) THEN
       GAPM=ZERO
       MT=IXS(1,NELS)
       IF(INTTH > 0 ) IELES(I) = NELS
       IF(MT.GT.0)THEN
        DO 100 JJ=1,8
        JJJ=IXS(JJ+1,NELS)
        XC(JJ)=X(1,JJJ)
        YC(JJ)=X(2,JJJ)
        ZC(JJ)=X(3,JJJ)
  100   CONTINUE
        CALL VOLINT(VOL)
        STF(I)=SLSFAC*AREA*AREA*PM(100,MT)/VOL
       ELSE
          IF(NINT.GE.0) THEN
             CALL ANCMSG(MSGID=95,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IXS(NIXS,NELS),
     .                   C2='SOLID',
     .                   I3=I)
          ENDIF
          IF(NINT.LT.0) THEN 
             CALL ANCMSG(MSGID=96,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IXS(NIXS,NELS),
     .                   C2='SOLID',
     .                   I3=I)
          ENDIF
       ENDIF
       IF(IGAP/=0)THEN
          GAP_SH(I)=MIN(VOL/AREA,SQRT(AREA))/SIX
          GAPSOLIDMAX = MIN(GAPSOLIDMAX,VOL/(AREA*FOUR))
          GAPMN=MIN(GAPMN,HALF*GAP_SH(I))
          GAP_M(I)=ZERO
          TAG(IRECT(1,INRT)) = 1
          TAG(IRECT(2,INRT)) = 1
          TAG(IRECT(3,INRT)) = 1
          TAG(IRECT(4,INRT)) = 1
c          NBINFLG(IRECT(1,INRT))=BITUNSET(NBINFLG(IRECT(1,INRT)),7)
c          NBINFLG(IRECT(2,INRT))=BITUNSET(NBINFLG(IRECT(2,INRT)),7)
c          NBINFLG(IRECT(3,INRT))=BITUNSET(NBINFLG(IRECT(3,INRT)),7)
c          NBINFLG(IRECT(4,INRT))=BITUNSET(NBINFLG(IRECT(4,INRT)),7)
       ENDIF
       MBINFLG(I)=BITSET(MBINFLG(I),8)
      ENDIF
C---------------------
C     ELEMENTS COQUES
C---------------------
      CALL INCOQ3(IRECT,IXC ,IXTG ,NINT ,NELC     ,
     .            NELTG,INRT,GEO  ,PM   ,KNOD2ELC ,
     .            KNOD2ELTG ,NOD2ELC ,NOD2ELTG,THK,NTY,IGEO,
     .            PM_STACK , IWORKSH)
      IF(NELTG.NE.0) THEN
C
        MT=IXTG(1,NELTG)
        MG=IXTG(5,NELTG)
        IGTYP = IGEO(11,MG)
        IP = IPARTTG(NELTG)
        IF ( THK_PART(IP) .NE. ZERO .AND. IINTTHICK .EQ. 0) THEN
          DX=THK_PART(IP)*GAPSCALE
        ELSEIF ( THK(NUMELC+NELTG) .NE. ZERO .AND. IINTTHICK .EQ. 0)THEN
          DX=THK(NUMELC+NELTG)*GAPSCALE
        ELSEIF(IGTYP ==17) THEN
          DX=THK(NUMELC+NELTG)*GAPSCALE
        ELSE
          DX=GEO(1,MG)*GAPSCALE
        ENDIF
        GAPM=HALF*DX
        GAPS2=MAX(GAPS2,GAPM)
        GAPMN = MIN(GAPMN,DX)
        DXM=DXM+DX
        NDX=NDX+1
        IGMAT = IGEO(98,MG)
        IF(MT.GT.0)THEN
          IF(IGTYP == 11 .AND. IGMAT > 0) THEN
            IF ( THK(NUMELC+NELTG) .NE. ZERO .AND. IINTTHICK .EQ. 0) THEN
              STF(I)=SLSFAC*THK(NUMELC+NELTG)*GEO(IPGMAT + 2 ,MG)
            ELSE
              STF(I)=SLSFAC*GEO(1,MG)*GEO(IPGMAT + 2 ,MG)
            ENDIF 
           ELSEIF(IGTYP==52 .OR. 
     .          ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0))THEN
              ISUBSTACK = IWORKSH(3,NUMELC+NELTG)
              ST=PM_STACK(2,ISUBSTACK) 
              STF(I)=SLSFAC*THK(NUMELC+NELTG)*ST
          ELSE
            IF ( THK(NUMELC+NELTG) .NE. ZERO .AND. IINTTHICK .EQ. 0) THEN
              STF(I)=SLSFAC*THK(NUMELC+NELTG)*PM(20,MT)
            ELSEIF(IGTYP == 17) THEN
              STF(I)=SLSFAC*THK(NUMELC+NELTG)*PM(20,MT)
            ELSE
              STF(I)=SLSFAC*GEO(1,MG)*PM(20,MT)
            ENDIF
          ENDIF
        ELSE
           IF(NINT.GE.0) THEN
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXTG(NIXTG,NELTG),
     .                    C2='SHELL',
     .                    I3=I)
           ENDIF
           IF(NINT.LT.0) THEN
              CALL ANCMSG(MSGID=96,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXTG(NIXTG,NELTG),
     .                    C2='SHELL',
     .                    I3=I)
           ENDIF
        ENDIF
        IF(IGAP/=0) GAP_M(I)=GAPM
        MBINFLG(I)=BITSET(MBINFLG(I),3)
      ELSEIF(NELC.NE.0) THEN
        MT=IXC(1,NELC)
        MG=IXC(6,NELC)
        IGTYP = IGEO(11,MG)
        IP = IPARTC(NELC)
        IF ( THK_PART(IP) .NE. ZERO .AND. IINTTHICK .EQ. 0) THEN
          DX=THK_PART(IP)*GAPSCALE
        ELSEIF ( THK(NELC) .NE. ZERO .AND. IINTTHICK .EQ. 0) THEN
          DX=THK(NELC)*GAPSCALE
        ELSEIF(IGTYP ==17) THEN
          DX=THK(NELC)*GAPSCALE
        ELSE
          DX=GEO(1,MG)*GAPSCALE
        ENDIF
        GAPM=HALF*DX
        GAPS2=MAX(GAPS2,GAPM)
        GAPMN = MIN(GAPMN,DX)
        DXM=DXM+DX
        NDX=NDX+1
        IGMAT = IGEO(98,MG)
        IF(MT.GT.0)THEN
         IF(IGTYP == 11 .AND. IGMAT > 0) THEN
          IF ( THK(NELC) .NE. ZERO .AND. IINTTHICK .EQ. 0) THEN 
            STF(I)=SLSFAC*THK(NELC)*GEO(IPGMAT + 2 ,MG)
          ELSE
            STF(I)=SLSFAC*GEO(1,MG)*GEO(IPGMAT + 2 ,MG)
          ENDIF   
         ELSEIF(IGTYP==52 .OR. 
     .        ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0))THEN
          ISUBSTACK = IWORKSH(3,NELC)
          ST=PM_STACK(2,ISUBSTACK) 
          STF(I)=SLSFAC*THK(NELC)*ST
         ELSE 
          IF ( THK(NELC) .NE. ZERO .AND. IINTTHICK .EQ. 0) THEN 
            STF(I)=SLSFAC*THK(NELC)*PM(20,MT)
          ELSEIF(IGTYP ==17) THEN
            STF(I)=SLSFAC*THK(NELC)*PM(20,MT)
          ELSE
            STF(I)=SLSFAC*GEO(1,MG)*PM(20,MT)
          ENDIF
         ENDIF 
        ELSE
           IF(NINT.GE.0) THEN
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXC(NIXC,NELC),
     .                    C2='SHELL',
     .                    I3=I)
           ENDIF
           IF(NINT.LT.0) THEN
              CALL ANCMSG(MSGID=96,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXC(NIXC,NELC),
     .                    C2='SHELL',
     .                    I3=I)
           ENDIF
        ENDIF
        IF(IGAP/=0) GAP_M(I)=GAPM
        MBINFLG(I)=BITSET(MBINFLG(I),4)
      ENDIF
C
      IF(NELS+NELC+NELTG.EQ.0)THEN
       IF (IMACH.NE.3) THEN
         IF(NINT.GT.0) THEN
            CALL ANCMSG(MSGID=92,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
         ENDIF
         IF(NINT.LT.0) THEN
            CALL ANCMSG(MSGID=93,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
         ENDIF
       ELSE
C      en SPMD il faut un element associe a l'arrete sinon erreur
         IF(NINT.GT.0) THEN
            CALL ANCMSG(MSGID=481,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
         ENDIF
         IF(NINT.LT.0) THEN
            CALL ANCMSG(MSGID=482,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
         ENDIF
       ENDIF
      ENDIF
  500 CONTINUE
C---------------------------
C     GAP 
C---------------------------
       GAPMX=SQRT(GAPMX)
       IF(IGAP.EQ.0)THEN
C GAP FIXE
         IF(GAP.LE.ZERO)THEN
           IF(NDX.NE.0)THEN
             GAP = DXM/NDX
             GAP = MIN(HALF*GAPMX,GAP)
           ELSE
             GAP = EM01 * GAPMX
           ENDIF
c           WRITE(IOUT,1300)GAP
         ENDIF
         GAPMIN = GAP
         IF(INACTI.NE.7.AND.GAP.GT.0.5*GAPMX)THEN
          GAPTMP = HALF*GAPMX
          CALL ANCMSG(MSGID=94,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=ID,
     .                C1=TITR,
     .                R1=GAP,
     .                R2=GAPTMP)
         ENDIF
       ELSE
C GAP VARIABLE :
C    - GAPMIN CONTIENT ONE GAP MINIMUM UTILISE SI GAP_S(I)+GAP_M(J) < GAPMIN
C    - GAP CONTIENT LE SUP DE (GAP_S(I)+GAP_M(J),GAPMIN) 
         IF(GAP.LE.ZERO)THEN
           IF(NDX.NE.0)THEN
             GAPMIN = GAPMN
             GAPMIN = MIN(HALF*GAPMX,GAPMIN)
           ELSE
c             GAPMIN = EM01 * GAPMX
             GAPMIN = MIN(GAPMN,EM01 * GAPMX)
           ENDIF
c           WRITE(IOUT,1300)GAPMIN
         ELSE
           GAPMIN = GAP
         ENDIF
C SUP DES GAPS VARIABLES
         GAP = MAX(GAPS1+GAPS2,GAPMIN)
         GAP=MIN(GAP,GAPMAX)
         IF(INACTI.NE.7.AND.GAP.GT.HALF*GAPMX)THEN
          GAPTMP = 0.5*GAPMX
          CALL ANCMSG(MSGID=477,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=ID,
     .                C1=TITR,
     .                R1=GAP)
         ENDIF
       ENDIF
C---------------------------------------------
C     MISE A ONE DU MULTIPLICATEUR NODALE DES RIGIDITES 
C---------------------------------------------
c     STFN est temporairement de 1 a NSN au lien de 1 a NLN
      DO L=1,NSN
         STFN(L) = 1.
      ENDDO
C---------------------------------------------
C     LIMITATION DU GAP DES SOLIDES
C---------------------------------------------
      IF (IGAP/=0) THEN
        DO I = 1, NRT
          IF(GAP_M(I) == ZERO)THEN
            GAP_SH(I) = MIN(GAPSOLIDMAX,GAP_SH(I))
            GAP_SH(I) = MAX(GAPSOL,GAP_SH(I))
c GAP_M pour tri
            GAP_M(I)=GAP_M(I)+TWO*GAP_SH(I)
          ENDIF
        ENDDO
      ENDIF
C
C Calcul du gap reel a utiliser lors du critere de retri
C
      GAPSHMAX = ZERO
      IF (IGAP.EQ.0) THEN
        GAPINF=GAP
      ELSE
        GAPINFS=EP30
        GAPINFM=EP30
        GAPSUPS = ZERO
        GAPSUPM = ZERO
        DO I = 1, NSN
          GAPINFS = MIN(GAPINFS,GAP_S(I))
          GAPSUPS = MAX(GAPSUPS,GAP_S(I))
        ENDDO
        DO I = 1, NRT
c          GAP_M(I)=GAP_M(I)+TWO*GAP_SH(I)
          GAPINFM = MIN(GAPINFM,GAP_M(I))
          GAPSUPM = MAX(GAPSUPM,GAP_M(I))
          GAPSHMAX = MAX(GAPSHMAX,GAP_SH(I))
        ENDDO
        GAPINF= MAX(GAPINFS+GAPINFM,GAPMIN)
        GAP   = MIN(GAPSUPS+GAPSUPM,GAPMAX)
      ENDIF  

      DO I=1,NLN
        IF(TAG(NLG(I)) == 1)NBINFLG(I)=BITUNSET(NBINFLG(I),7)
      ENDDO    
C------------
      RETURN
 1300 FORMAT(2X,'GAP MIN = ',1PG20.13)
      END
Chd|====================================================================
Chd|  I20STI3E                      source/interfaces/inter3d1/i20sti3.F
Chd|-- called by -----------
Chd|        I20INI3                       source/interfaces/inter3d1/i20ini3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        I11COQ                        source/interfaces/inter3d1/i11coq.F
Chd|        I11FIL                        source/interfaces/inter3d1/i11coq.F
Chd|        I11GMX3                       source/interfaces/inter3d1/i11gmx3.F
Chd|        I11SOL                        source/interfaces/inter3d1/i11sol.F
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        VOLINT                        source/interfaces/inter3d1/volint.F
Chd|        GET_U_GEO                     source/user_interface/uaccess.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I20STI3E(
     1 X     ,IXLIN ,STF   ,IXS   ,PM    ,
     2 GEO   ,NRT   ,IXC   ,NINTR ,SLSFAC,
     3 NTY   ,GAPMAX,NOINT ,GAP_SM,
     4 MS    ,IXTG  ,IXT   ,IXP   ,IXR   ,
     5 IGAP  ,GAPMIN,GAP0  ,GAPINF,NSNE  ,
     6 IPARTC,IPARTTG,THK  ,THK_PART,IXS10,
     7 ID    ,TITR  ,KXX   ,IXX   ,IGEO,
     8 NOD2EL1D,KNOD2EL1D,KNOD2ELS,KNOD2ELC,KNOD2ELTG,
     9 NOD2ELS,NOD2ELC,NOD2ELTG   ,LELX ,PM_STACK,IWORKSH)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "scr08_c.inc"
#include      "scr23_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRT, NINTR, NTY, NOINT,IGAP,NSNE
C     REAL ou REAL*8
      my_real
     .   SLSFAC, GAPMAX,GAPMIN,GAP0
      INTEGER IXLIN(2,*), IXS(NIXS,*), IXC(NIXC,*),
     .   IXTG(NIXTG,*),IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),
     .   IPARTC(*), IPARTTG(*),IXS10(*),KXX(NIXX,*),IXX(*),
     .   IGEO(NPROPGI,*),
     .   KNOD2ELS(*), KNOD2ELC(*), KNOD2ELTG(*), 
     .   NOD2ELS(*), NOD2ELC(*), NOD2ELTG(*),
     .   NOD2EL1D(*),KNOD2EL1D(*),IWORKSH(3,*)
C     REAL ou REAL*8
      my_real
     .   X(3,*), STF(*), PM(NPROPM,*), GEO(NPROPG,*),
     .   MS(*),GAP_SM(*),XL2, GAPINF,THK(*),THK_PART(*),LELX(*),
     .   PM_STACK(20,*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NDX, I, INRT, NELS, MT, JJ, JJJ, NELC, J,
     .   MG, NUM, NPT, LL, L, NN, NELTG,NELT,NELP,NELR,
     .   IGTYP, I1, I2,IP,NELX,IPGMAT,IGMAT,ISUBSTACK
C     REAL ou REAL*8
      my_real
     .   DXM, GAPMX, GAPMN, AREA, VOL, DX,GAP1,GAPS1,GAPTMP,
     .   GET_U_GEO,ST
C-----------------------------------------------
      EXTERNAL GET_U_GEO
C--------------------------------------------------------------
C     CALCUL DES RIGIDITES DES SEGMENTS 
C     V16 : DANS LE CAS OU ONE SEGMENT APPARTIENT A LA FOIS
C           A UNE BRIQUE ET A UNE COQUE ON CHOISIT LA RIGIDITE
C           DE LA COQUE SAUF SI LE MATERIAU COQUE EST NUL.
C---------------------------------------------------------------
      DXM=ZERO
      NDX=0      
      GAPS1=ZERO
      GAPMX=EP30
      GAPMN=EP30

C
      DO 500 I=1,NRT
      STF(I)=ZERO
      GAP_SM(I)=ZERO
      INRT=I
      CALL I11GMX3(X,IXLIN,INRT,GAPMX,XL2)
C----------------------
C     ELEMENTS SOLIDES
C----------------------
      CALL I11SOL(X,IXLIN,IXS,NINTR,NELS,INRT,
     .            AREA,NOINT,KNOD2ELS,NOD2ELS,IXS10)
      IF(NELS.NE.0) THEN
       MT=IXS(1,NELS)
       IF(MT.GT.0)THEN
        DO 100 JJ=1,8
        JJJ=IXS(JJ+1,NELS)
        XC(JJ)=X(1,JJJ)
        YC(JJ)=X(2,JJJ)
        ZC(JJ)=X(3,JJJ)
  100   CONTINUE
        CALL VOLINT(VOL)
        IF(XL2.GT.0.0)THEN
          STF(I)=SLSFAC*VOL*PM(100,MT)/XL2
        ELSE
          STF(I)=ZERO
        ENDIF
       ELSE
C        IF(NINTR.GE.0)WRITE (IOUT,1500) IXS(11,NELS),I, NOINT
          IF(NINTR.GE.0) THEN
             CALL ANCMSG(MSGID=95,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IXS(NIXS,NELS),
     .                   C2='SOLID',
     .                   I3=I)
          ENDIF
C        IF(NINTR.LT.0)WRITE (IOUT,1600) IXS(11,NELS),I, NOINT
C        IWARN=IWARN+1
          IF(NINTR.LT.0) THEN
             CALL ANCMSG(MSGID=96,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IXS(NIXS,NELS),
     .                   C2='SOLID',
     .                   I3=I)
          ENDIF
       ENDIF
      ENDIF
C---------------------
C     ELEMENTS COQUES
C---------------------
      CALL I11COQ(IXLIN,IXC ,IXTG,NINTR,NELC ,
     .            NELTG,INRT,GEO,PM,THK,IGEO,
     .            KNOD2ELC,KNOD2ELTG,NOD2ELC,NOD2ELTG,
     .            PM_STACK, IWORKSH )
      IF(NELTG.NE.0) THEN
C
        MT=IXTG(1,NELTG)
        MG=IXTG(5,NELTG)
        IGTYP = IGEO(11,MG)
        DX=GEO(1,MG)
        IF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP ==52)
     .                                   DX = THK(NUMELC + NELTG)
        GAP_SM(I)=HALF*DX
        GAPS1=MAX(GAPS1,GAP_SM(I))
        GAPMN = MIN(GAPMN,DX)
        DXM=DXM+DX
        NDX=NDX+1
        IGMAT = IGEO(98,MG)
        IPGMAT=700
        IF(MT.GT.0)THEN
         IF(IGTYP == 11 .AND. IGMAT > 0)THEN
          STF(I)=SLSFAC*DX*GEO(IPGMAT + 2 ,MG)
         ELSEIF(IGTYP==52 .OR. 
     .        ((IGTYP == 17 .OR. IGTYP == 51 ) .AND. IGMAT > 0))THEN
            ISUBSTACK = IWORKSH(3,NELTG + NUMELC)
            STF(I)=SLSFAC*DX*PM_STACK(2,ISUBSTACK)
         ELSE
           STF(I)=SLSFAC*DX*PM(20,MT)
         ENDIF  
        ELSE
C         IF(NINTR.GE.0)WRITE (IOUT,1700) IXTG(NIXTG,NELTG),I, NOINT
           IF(NINTR.GE.0) THEN
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXTG(NIXTG,NELTG),
     .                    C2='SHELL',
     .                    I3=I)
           ENDIF
C         IF(NINTR.LT.0)WRITE (IOUT,1800) IXTG(NIXTG,NELTG),I, NOINT
C         IWARN=IWARN+1
           IF(NINTR.LT.0) THEN
              CALL ANCMSG(MSGID=96,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXTG(NIXTG,NELTG),
     .                    C2='SHELL',
     .                    I3=I)
           ENDIF
        ENDIF
      ELSEIF(NELC.NE.0) THEN
C
        MT=IXC(1,NELC)
        MG=IXC(6,NELC)
        IGTYP = IGEO(11,MG)
        DX=GEO(1,MG)
        IF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP ==52)
     .                                         DX = THK(NELC)
        GAP_SM(I)=HALF*DX
        GAPS1=MAX(GAPS1,GAP_SM(I))
        GAPMN = MIN(GAPMN,DX)
        DXM=DXM+DX
        NDX=NDX+1
        IGMAT = IGEO(98,MG)
        IF(MT.GT.0)THEN
           IF(IGTYP == 11 .AND. IGMAT > 0) THEN
            STF(I)=SLSFAC*DX*GEO(IPGMAT + 2 ,MG)
           ELSEIF(IGTYP ==52 .OR. 
     .          ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0))THEN
            ISUBSTACK = IWORKSH(3,NELC)
            STF(I)=SLSFAC*DX*PM_STACK(2,ISUBSTACK) 
          ELSE
           STF(I)=SLSFAC*DX*PM(20,MT)
          ENDIF   
        ELSE
C           IF(NINTR.GE.0)WRITE (IOUT,1700) IXC(7,NELC),I, NOINT
           IF(NINTR.GE.0) THEN
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXC(NIXC,NELC),
     .                    C2='SHELL',
     .                    I3=I)
           ENDIF
C           IF(NINTR.LT.0)WRITE (IOUT,1800) IXC(7,NELC),I, NOINT
C           IWARN=IWARN+1
           IF(NINTR.LT.0) THEN
              CALL ANCMSG(MSGID=96,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXC(NIXC,NELC),
     .                    C2='SHELL',
     .                    I3=I)
           ENDIF
        ENDIF
      ENDIF
C---------------------
C     ELEMENTS TIGE POUTRE RESSORT
C---------------------
      CALL I11FIL(IXLIN,IXT,IXP,IXR,NINTR,NELT ,
     .            NELP,NELR,NELX,INRT,NOD2EL1D,
     .            KNOD2EL1D,KXX,IXX)

      IF(NELT.NE.0) THEN
C
        MT=IXT(1,NELT)
        MG=IXT(4,NELT)
        DX=SQRT(GEO(1,MG))
        IF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52 )
     .                                  DX = SQRT(THK(NUMELC + NELT))
        GAP_SM(I)=MAX(GAP_SM(I),HALF*DX)
        GAPS1=MAX(GAPS1,GAP_SM(I))
        GAPMN = MIN(GAPMN,DX)
        DXM=DXM+DX
        NDX=NDX+1
        IGMAT = IGEO(98,MG)
        IF(MT.GT.0)THEN
         IF(IGTYP == 11 .AND. IGMAT > 0) THEN
          STF(I)=SLSFAC*DX*GEO(IPGMAT + 2 ,MG)
         ELSEIF((IGTYP == 17 .OR. IGTYP == 17) .AND. IGMAT > 0) THEN
            ISUBSTACK = IWORKSH(3,NUMELC + NELT)
             STF(I)=SLSFAC*DX*PM_STACK(2,ISUBSTACK) 
         ELSE
            STF(I)=SLSFAC*DX*PM(20,MT)
         ENDIF
        ELSE
C         IF(NINTR.GE.0)WRITE (IOUT,1700) IXT(NIXT,NELT),I, NOINT
           IF(NINTR.GE.0) THEN 
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXT(NIXT,NELT),
     .                    C2='TRUSS',
     .                    I3=I)
           ENDIF
C         IF(NINTR.LT.0)WRITE (IOUT,1800) IXT(NIXT,NELT),I, NOINT
C         IWARN=IWARN+1
           IF(NINTR.LT.0) THEN
              CALL ANCMSG(MSGID=96,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXT(NIXT,NELT),
     .                    C2='TRUSS',
     .                    I3=I)
           ENDIF
        ENDIF
      ELSEIF(NELP.NE.0) THEN
C
        MT=IXP(1,NELP)
        MG=IXP(5,NELP)
        DX=SQRT(GEO(1,MG))
        GAP_SM(I)=MAX(GAP_SM(I),HALF*DX)
        GAPS1=MAX(GAPS1,GAP_SM(I))
        GAPMN = MIN(GAPMN,DX)
        DXM=DXM+DX
        NDX=NDX+1
        IF(MT.GT.0)THEN
         STF(I)=SLSFAC*DX*PM(20,MT)
        ELSE
C         IF(NINTR.GE.0)WRITE (IOUT,1700) IXP(NIXP,NELP),I, NOINT
           IF(NINTR.GE.0) THEN
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXP(NIXP,NELP),
     .                    C2='BEAM',
     .                    I3=I)
           ENDIF
C         IF(NINTR.LT.0)WRITE (IOUT,1800) IXP(NIXP,NELP),I, NOINT
C         IWARN=IWARN+1
          IF(NINTR.LT.0) THEN
             CALL ANCMSG(MSGID=96,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IXP(NIXP,NELP),
     .                   C2='BEAM',
     .                   I3=I)
          ENDIF
        ENDIF
      ELSEIF(NELR.NE.0) THEN
C
        MG=IXR(1,NELR)
        MT = IXR(5,NELR)
        IF(MG.GT.0)THEN
         IGTYP=NINT(GEO(12,MG))
         IF(IGTYP.EQ.4.OR.IGTYP.EQ.12)THEN
           STF(I)=SLSFAC*GEO(2,MG)
         ELSEIF(IGTYP.EQ.8.OR.IGTYP.EQ.13)THEN
           STF(I)=SLSFAC*MAX(GEO(3,MG),GEO(10,MG),GEO(15,MG))
         ELSEIF(IGTYP == 23)THEN
           STF(I)=SLSFAC*MAX(PM(191,MT),PM(192,MT),PM(193,MT))
         ELSEIF(IGTYP.EQ.25)THEN
           STF(I)=SLSFAC*GEO(10,MG)
         ELSEIF(IGTYP.GE.29)THEN
           STF(I)=SLSFAC*GEO(3,MG)
         ELSE
            WRITE(6,'(A)') 'INTERNAL ERROR 987'
            CALL MY_EXIT(2)
C           STOP 987
         ENDIF
        ELSE
C         IF(NINTR.GE.0)WRITE (IOUT,1700) IXR(NIXR,NELR),I, NOINT
           IF(NINTR.GE.0) THEN
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXR(NIXR,NELR),
     .                    C2='SPRING',
     .                    I3=I)
           ENDIF
C         IF(NINTR.LT.0)WRITE (IOUT,1800) IXR(NIXR,NELR),I, NOINT
C         IWARN=IWARN+1
           IF(NINTR.LT.0) THEN
              CALL ANCMSG(MSGID=96,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXR(NIXR,NELR),
     .                    C2='SPRING',
     .                    I3=I)
           ENDIF
        ENDIF
      ELSEIF(NELX.NE.0) THEN
C
        MG=KXX(2,NELX)
        IF(MG.GT.0)THEN
         STF(I)=SLSFAC*GET_U_GEO(4,MG)*(KXX(3,NELX)-1)/LELX(NELX)
        ELSE
           IF(NINTR.GE.0) THEN
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=KXX(NIXX,NELX),
     .                    C2='XELEM',
     .                    I3=I)
           ENDIF
           IF(NINTR.LT.0) THEN
              CALL ANCMSG(MSGID=96,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=KXX(NIXX,NELX),
     .                    C2='XELEM',
     .                    I3=I)
           ENDIF
        ENDIF
      ENDIF
C---------------------------
      IF (IMACH.NE.3) THEN
       IF(NELS+NELC+NELTG+NELT+NELP+NELR+NUMELX.EQ.0
     .                       .AND.SLSFAC.GT.0.)THEN
C       IF(NINTR.GT.0) WRITE (IOUT,1100) I, NOINT
         IF(NINTR.GT.0) THEN
            CALL ANCMSG(MSGID=92,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
         ENDIF
C       IF(NINTR.LT.0) WRITE (IOUT,1200) I, NOINT
C       IWARN=IWARN+1
         IF(NINTR.LT.0) THEN
            CALL ANCMSG(MSGID=93,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
         ENDIF
       ENDIF
      ELSEIF(NELS+NELC+NELTG+NELT+NELP+NELR+NUMELX.EQ.0.)THEN
C en SPMD il faut un element associe a l'arrete sinon erreur
        IF(NINTR.GT.0) THEN
          CALL ANCMSG(MSGID=481,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=ID,
     .                C1=TITR,
     .                I2=I)
        ENDIF
        IF(NINTR.LT.0) THEN
          CALL ANCMSG(MSGID=482,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=ID,
     .                C1=TITR,
     .                I2=I)
        ENDIF
      ENDIF
  500 CONTINUE
C---------------------------
C     GAP 
C---------------------------
      GAPMX=SQRT(GAPMX)
      IF(IGAP.EQ.0)THEN
C---------------------------
C     GAP  FIXE
C---------------------------
        IF(GAP0.GT.ZERO)THEN
          GAP1 = GAP0
        ELSE
          IF(NDX.NE.0)THEN
           GAP1 = MIN(HALF*GAPMX,DXM/NDX)
          ELSE
           GAP1 = EM01* GAPMX
          ENDIF
          IF(NINTR.LT.0)WRITE(IOUT,1300)HALF*(GAPMIN+GAP1)
        ENDIF
C
        IF(NINTR.LT.0) GAP1 = HALF*(GAPMIN+GAP1)
        GAPMIN = GAP1
        GAPMAX = GAP1
C
        IF(GAP1.GT.HALF*GAPMX)THEN
C          WRITE(IOUT,1400)GAP1,0.5*GAPMX
C          IWARN=IWARN+1
          GAPTMP = HALF*GAPMX
          CALL ANCMSG(MSGID=94,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=ID,
     .                C1=TITR,
     .                R1=GAP1,
     .                R2=GAPTMP)
        ENDIF
      ELSE
C---------------------------
C GAP VARIABLE
C---------------------------
        IF(GAP0.GT.ZERO)THEN
          GAP1 = GAP0
        ELSE
          IF(NDX.NE.0)THEN
           GAP1 = MIN(HALF*GAPMX,GAPMN)
          ELSE
           GAP1 = EM01 * GAPMX
          ENDIF
          IF(NINTR.LT.0)WRITE(IOUT,1300)HALF*(GAPMIN+GAP1)
        ENDIF
C GAP MINI ET SUP DES GAPS VARIABLES
        IF(NINTR.GT.0)THEN
          GAPMIN = GAP1
          GAPMAX = GAPS1
        ELSE
          GAPMIN = HALF*(GAPMIN+GAP1)
          GAPMAX = MAX(GAPMAX+GAPS1,GAPMIN)
        ENDIF
C
        IF(GAPMAX.GT.HALF*GAPMX)THEN
           GAPTMP = HALF*GAPMX
           CALL ANCMSG(MSGID=94,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=ID,
     .                 C1=TITR,
     .                 R1=GAPMAX,
     .                 R2=GAPTMP)
        ENDIF
      ENDIF
C---------------------------
C     STIF GLOBAL
C---------------------------
      IF(SLSFAC.LT.ZERO)THEN
        DO I=1,NRT
          STF(I)=-SLSFAC
        ENDDO
      ENDIF
C---------------------------------------------
C
C Calcul du gap reel a utiliser lors du critere de retri
C
      IF (IGAP.EQ.0) THEN
        GAPINF=GAPMAX/TWO ! remultiplie par 2 dans i20ini3
      ELSE
        DO I = 1, NRT
          GAPINF = MIN(GAPINF,GAP_SM(I))
        ENDDO
      ENDIF      
      RETURN

 1300 FORMAT(2X,'COMPUTED GAP = ',1PG20.13)

      END
Chd|====================================================================
Chd|  I20NLG                        source/interfaces/inter3d1/i20sti3.F
Chd|-- called by -----------
Chd|        I20INI3                       source/interfaces/inter3d1/i20ini3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I20NLG(NLN  ,NRTM,NSN  ,NLINS ,NLINM ,
     .                  NLG  ,IRECT,NSV ,IXLINS,IXLINM,
     .                  NMN  ,NSNE ,NMNE,MSR   ,NSVE  ,
     .                  MSRE ,STFA ,DXANC,XANEW,X     ,
     .                  PENIA,ALPHAK)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NLN,NRTM, NSN,NLINS ,NLINM ,NMN  ,NSNE ,NMNE     
      INTEGER IRECT(4,NRTM), NSV(NSN),IXLINS(2,NLINS),IXLINM(2,NLINM),
     .        MSR(NMN),NSVE(NSNE),MSRE(NMNE),NLG(NLN)
      my_real
     .   STFA(*),DXANC(3,*),XANEW(3,*),X(3,*),PENIA(5,*),ALPHAK(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      INTEGER TAG(NUMNOD)
      my_real
     .   AAA,STIF(NLN)

      DO I=1,NLN
        J = NLG(I)
        TAG(J)=I
      ENDDO

      DO K=1,NSN
        NSV(K)=TAG(NSV(K))
      ENDDO
      DO K=1,NMN
        MSR(K)=TAG(MSR(K))
      ENDDO
      DO K=1,NSNE
        NSVE(K)=TAG(NSVE(K))
      ENDDO
      DO K=1,NMNE
        MSRE(K)=TAG(MSRE(K))
      ENDDO

      DO K=1,NRTM
        IRECT(1,K)=TAG(IRECT(1,K))
        IRECT(2,K)=TAG(IRECT(2,K))
        IRECT(3,K)=TAG(IRECT(3,K))
        IRECT(4,K)=TAG(IRECT(4,K))
      ENDDO
      DO K=1,NLINS
        IXLINS(1,K)=TAG(IXLINS(1,K))
        IXLINS(2,K)=TAG(IXLINS(2,K))
      ENDDO
      DO K=1,NLINM
        IXLINM(1,K)=TAG(IXLINM(1,K))
        IXLINM(2,K)=TAG(IXLINM(2,K))
      ENDDO

      DO I=1,NLN
        STIF(I) = ONE
        ALPHAK(1,I)  = ONE
        ALPHAK(2,I)  = ONE
        ALPHAK(3,I)  = ONE
      ENDDO

      DO I=1,NSN
c en input STFA(1:NLN) est STFN(1:NSN) eventuellement mis a zero si pene initiale
        J = NSV(I)
        STIF(J) = STFA(I)
      ENDDO

      DO I=1,NLN
        STFA(I) = STIF(I)
      ENDDO

c STFA sera recalcule dans I20STIFN /inter3d1/i20stifn.F
      DO I=1,NLN
        DXANC(1,I) = XANEW(1,NLG(I))-X(1,NLG(I))
        DXANC(2,I) = XANEW(2,NLG(I))-X(2,NLG(I))
        DXANC(3,I) = XANEW(3,NLG(I))-X(3,NLG(I))
        PENIA(4,I) = SQRT(DXANC(1,I)*DXANC(1,I)
     +                   +DXANC(2,I)*DXANC(2,I)
     +                   +DXANC(3,I)*DXANC(3,I))
        PENIA(5,I) = PENIA(4,I)
        AAA = ONE/MAX(PENIA(4,I),EM20)
        PENIA(1,I) = DXANC(1,I)*AAA
        PENIA(2,I) = DXANC(2,I)*AAA
        PENIA(3,I) = DXANC(3,I)*AAA
      ENDDO

      RETURN
      END
