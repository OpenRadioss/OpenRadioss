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
Chd|  I25STI3                       source/interfaces/inter3d1/i25sti3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRICTION_PARTS_SEARCH         source/interfaces/inter3d1/i7sti3.F
Chd|        I24NORMNS                     source/interfaces/inter3d1/i24sti3.F
Chd|        I25BORD                       source/interfaces/inter3d1/i25sti3.F
Chd|        I25GAPM                       source/interfaces/inter3d1/i25sti3.F
Chd|        INSOL3ET                      source/interfaces/inter3d1/i24sti3.F
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I25STI3(
     1 X         ,IRECT      ,STF       ,IXS       ,PM         ,
     2 GEO       ,NRT        ,IXC       ,NINT      ,STFAC      ,
     3 NTY       ,GAP        ,NOINT     , 
     4 STFN      ,NSN        ,MS        ,NSV       ,IXTG       ,
     5 IGAP      ,WA         ,GAP_S     ,GAP_M     ,GAPMIN     ,
     6 GAPSCALE  ,IXT ,IXP   ,GAPINF    ,GAPMAX_S  ,
     9 INACTI    ,KNOD2ELS   ,KNOD2ELC  ,KNOD2ELTG ,NOD2ELS    ,
     A NOD2ELC   ,NOD2ELTG   ,INTTH,
     B IELES     ,IELEM      ,AREAS     ,SH4TREE   ,SH3TREE    ,
     C IPART     ,IPARTC     ,IPARTTG   ,THK       ,THK_PART   ,
     D IXR       ,ITAB       ,BGAPSMX   ,IXS10     ,MSEGTYP    ,
     E NRT_SH    ,IXS16      ,IXS20     ,GAP_N     ,
     F ILEV      ,GAPMAX_M   ,ID,TITR   ,IGAP0     ,
     G PEN_OLD   ,IPARTS     ,IGEO      ,FILLSOL   ,
     H PM_STACK  , IWORKSH   ,PERCENT_SIZE,GAP_S_L ,GAP_M_L    ,
     I KNOD2EL1D ,NOD2EL1D   ,INTFRIC   ,TAGPRT_FRIC,IPARTFRICS,
     J IPARTFRICM,INTBUF_FRIC_TAB,IVIS2 ,GAPM_MX   ,GAPS_MX    ,
     K GAPM_L_MX ,GAPS_L_MX  ,IPARTSM   ,DRAD      ,IPARTT     ,
     J IPARTP    ,IPARTR   )
C-----------------------------------------------
      USE INTBUF_FRIC_MOD
      USE MESSAGE_MOD
C
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
#include      "remesh_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRT, NINT, NTY, NOINT,NSN,IGAP,INTFRIC,
     .        INACTI,NRT_SH ,ILEV ,IGAP0,IGEO(NPROPGI,*), IVIS2
      INTEGER IRECT(4,*), IXS(NIXS,*), IXC(NIXC,*),
     .   NSV(*), IXTG(NIXTG,*), IXT(NIXT,*), IXP(NIXP,*),
     .   KNOD2ELS(*), KNOD2ELC(*), KNOD2ELTG(*), NOD2ELS(*), NOD2ELC(*),
     .   NOD2ELTG(*), INTTH,
     .   SH3TREE(KSH3TREE,*), SH4TREE(KSH4TREE,*),IXR(NIXR,*) ,
     .   IPART(LIPART1,*), IPARTC(*), IPARTTG(*),
     .   ITAB(*), IXS10(6,*),MSEGTYP(*), IXS16(*), IXS20(*),
     .   IWORKSH(3,*), KNOD2EL1D(*),NOD2EL1D(*),TAGPRT_FRIC(*),
     .   IPARTFRICS(*),IPARTFRICM(*),IPARTSM(*),IELES(*),IELEM(*)
C     REAL
      my_real
     .   STFAC,   GAP, GAPSCALE, GAPMIN,GAPINF, GAPMAX_S,BGAPSMX ,GAPMAX_M,
     .   PERCENT_SIZE, GAPM_MX, GAPS_MX, GAPS_L_MX, GAPM_L_MX,DRAD
C     REAL
      my_real
     .   X(3,*), STF(*), PM(NPROPM,*), GEO(NPROPG,*), STFN(*),
     .   MS(*),WA(*),GAP_S(*),GAP_M(*),GAP_N(4,*),
     .   AREAS(*),THK(*),THK_PART(*),PEN_OLD(5,NSN), FILLSOL(*),
     .   PM_STACK(20,*),GAP_S_L(*),GAP_M_L(*)
      INTEGER ID,IPARTS(*)
      INTEGER, DIMENSION(NUMELT), INTENT(IN) :: IPARTT
      INTEGER, DIMENSION(NUMELP), INTENT(IN) :: IPARTP
      INTEGER, DIMENSION(NUMELR), INTENT(IN) :: IPARTR
      CHARACTER*nchartitle,
     .   TITR
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NDX, I, J, INRT, NELS, MT, JJ, JJJ, NELC,
     .   MG, NUM, NPT, LL, L, NN, NELTG,N1,N2,N3,N4,IE,
     .   IP, NLEV, MYLEV, K, P, R, T,IAD,
     .   TAGB(NUMNOD),NS,IGTYP,NRTT,NNOD,ISUBSTACK,IPFMAX,IPL,
     .   IPFLMAX,IPG,NELEM,STAT
      INTEGER JPERM(4)
C     REAL
      my_real
     .   DXM, GAPMX, GAPMN, AREA, VOL, DX, GAPM, DDX,
     .   GAPTMP, SX1,SY1,SZ1,SX2,SY2,SZ2,SX3,SY3,SZ3,
     .   SLSFAC,XL,GAPS_MN, STV
      DATA JPERM/2,3,4,1/
      INTEGER, DIMENSION(:),ALLOCATABLE ::INRTIE
C--------------------------------------------------------------
C     CALCUL DES RIGIDITES DES SEGMENTS
C     V16 : DANS LE CAS OU ONE SEGMENT APPARTIENT A LA FOIS
C           A UNE BRIQUE ET A UNE COQUE ON CHOISIT LA RIGIDITE
C           DE LA COQUE SAUF SI LE MATERIAU COQUE EST NUL.
C---------------------------------------------------------------
C      NRT->NRT0
      SLSFAC = STFAC
C
      IF(IGAP==3)THEN
        DO I=1,NRT
          GAP_M_L(I)=EP30
        ENDDO
        DO I=1,NSN
          GAP_S_L(I)=EP30
        ENDDO
      ENDIF
C
      DXM=ZERO
      NDX=0
      GAPMX=EP30
      GAPMN=EP30
      GAPM_MX  =ZERO
      GAPS_MX  =ZERO
      GAPS_MN  =EP30
      GAPS_L_MX=ZERO
      GAPM_L_MX=ZERO
C-----
      GAPMIN   = ZERO
C-----NRTT:NRTM
C     NRT_SH nb of shells before symetrization, NRT nb of MAIN segments before symetrization (symetrization in i25surfi)
      NRTT =NRT+NRT_SH
C-----
      IF(INTTH > 0)THEN
        NELEM = NUMELC+NUMELTG+NUMELS+NUMELR
     +      + NUMELP+NUMELT+NUMELQ+NUMELR+NUMELX+NUMELIG3D
        ALLOCATE(INRTIE(NELEM),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='INRTIE')
        INRTIE=0
      END IF
C------------------------------------
      IF(IGAP==3)THEN
        DO I=1,NUMNOD
          WA(I)=EP30
        ENDDO
        DO I=1,NRT
          XL = EP30

          DO J=1,4
            N1=IRECT(J,I)
            N2=IRECT(JPERM(J),I)
            IF(N1 /= N2 .AND. N1 /= 0)
     .        XL=MIN(XL,SQRT((X(1,N1)-X(1,N2))**2+(X(2,N1)-X(2,N2))**2+
     .                 (X(3,N1)-X(3,N2))**2))
          ENDDO

          GAP_M_L(I)=MIN(PERCENT_SIZE*XL,GAPMAX_M)
          GAPM_L_MX   =MAX(GAPM_L_MX,GAP_M_L(I))

          DO J=1,4
            WA(IRECT(J,I)) = MIN(WA(IRECT(J,I)),PERCENT_SIZE*XL)
          ENDDO
        ENDDO

        DO I=1,NSN
          GAP_S_L(I)=WA(NSV(I))
          GAP_S_L(I)=MIN(GAP_S_L(I),GAPMAX_S)
        ENDDO

      ENDIF
C------------------------------------
C     GAP NOEUDS SECONDS
C------------------------------------
      DO I=1,NUMNOD
        WA(I)=ZERO
      ENDDO
      DO I=1,NUMELC
        MG=IXC(6,I)
        IP = IPARTC(I)
        IGTYP = IGEO(11,MG)
        IF ( THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN
          DX=HALF*THK_PART(IP)
        ELSEIF ( THK(I) /= ZERO .AND. IINTTHICK == 0) THEN
          DX=HALF*THK(I)
        ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR.IGTYP ==52) THEN
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
        IP = IPARTTG(I)
        IGTYP = IGEO(11,MG)
        IF ( THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN
          DX=HALF*THK_PART(IP)
        ELSEIF ( THK(NUMELC+I) /= ZERO .AND. IINTTHICK == 0) THEN
          DX=HALF*THK(NUMELC+I)
        ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP ==52) THEN
          DX=HALF*THK(NUMELC+I)
        ELSE
          DX=HALF*GEO(1,MG)
        ENDIF
        WA(IXTG(2,I))=MAX(WA(IXTG(2,I)),DX)
        WA(IXTG(3,I))=MAX(WA(IXTG(3,I)),DX)
        WA(IXTG(4,I))=MAX(WA(IXTG(4,I)),DX)
      ENDDO
C-----for case of coating shell--
      IF (ILEV/=3) THEN
        DO I=1,NUMNOD
          TAGB(I) = 0
        END DO
        DO I=1,NRT
C       MSEGTYP==0 <=> Solide
          IF (MSEGTYP(I) /= 0) THEN
            DO J =1,4
              NN= IRECT(J,I)
              TAGB(NN) = 1
            END DO
          END IF
        END DO
        DO I=1,NUMNOD
          IF (TAGB(I)==0) WA(I)=0
        END DO
      END IF
C-------
      DO I=1,NUMELT
        MG=IXT(4,I)
        IP = IPARTT(I)
        IF ( THK_PART(IP) > ZERO ) THEN
          DX=HALF*THK_PART(IP)
        ELSE
          DX=HALF*SQRT(GEO(1,MG))
        END IF
        WA(IXT(2,I))=MAX(WA(IXT(2,I)),DX)
        WA(IXT(3,I))=MAX(WA(IXT(3,I)),DX)
      ENDDO
      DO I=1,NUMELP
        MG=IXP(5,I)
        IP = IPARTP(I)
        IF ( THK_PART(IP) > ZERO ) THEN
          DX=HALF*THK_PART(IP)
        ELSE
          DX=HALF*SQRT(GEO(1,MG))
        END IF
        WA(IXP(2,I))=MAX(WA(IXP(2,I)),DX)
        WA(IXP(3,I))=MAX(WA(IXP(3,I)),DX)
      ENDDO
      DO I=1,NUMELR
        IP = IPARTR(I)
        IF ( THK_PART(IP) > ZERO ) THEN
          MG=IXR(1,I)
          IGTYP = IGEO(11,MG)
          DX=HALF*THK_PART(IP)
          WA(IXR(2,I))=MAX(WA(IXR(2,I)),DX)
          WA(IXR(3,I))=MAX(WA(IXR(3,I)),DX)
          IF (IGTYP==12) WA(IXR(4,I))=MAX(WA(IXR(4,I)),DX)
       END IF
      ENDDO
      DO I=1,NSN
        GAP_S(I)=GAPSCALE * WA(NSV(I))
        GAP_S(I)=MIN(GAP_S(I),GAPMAX_S)
      ENDDO
C---------put SECONDARY node on the free edge to GAP=0
      IF(IGAP0 == 1)THEN
        DO I=1,NUMNOD
          TAGB(I)=0
        ENDDO
C
        IF(ILEV /= 3 )THEN
          CALL I25BORD(NRT     ,IRECT ,TAGB    ) ! provisoire (Igap0=1)
C         IAD =ISURF2(3)
C         CALL I24BORD(ISURF2  ,IBUFSSG(IAD) ,TAGB    )
        ENDIF
C       IF(ILEV == 2)THEN
C         IAD =ISURF(3)
C         CALL I24BORD(ISURF   ,IBUFSSG(IAD) ,TAGB    )
C       ENDIF
        DO I=1,NSN
          NS = NSV(I)
          IF( TAGB(NS) > 0 ) GAP_S(I) = ZERO
        ENDDO
      ENDIF
C
      DO I=1,NSN
        IF(IGAP /= 3) THEN
          GAPS_MX=MAX(GAPS_MX,GAP_S(I))
          GAPS_MN=MIN(GAPS_MN,GAP_S(I))
        ELSE
          GAPS_MX = MAX(GAPS_MX,GAP_S(I))
          GAPS_L_MX = MAX(GAPS_L_MX,GAP_S_L(I))
          GAPS_MN = MIN(GAPS_MN,GAP_S(I),GAP_S_L(I))
        END IF
      ENDDO
C
C -----Friction model  SECONDARY nodes parts------
C-----------if node connects to both shell and solid -> takes shells

      IF(INTFRIC > 0) THEN
C
        IF(NUMELS/=0)THEN
          DO I = 1,NSN
            IPFMAX = 0
            IPFLMAX = 0
            DO J= KNOD2ELS(NSV(I))+1,KNOD2ELS(NSV(I)+1)
              IE = NOD2ELS(J)
              IP = IPARTS(IE)
              IPG = TAGPRT_FRIC(IP)
              IF(IPG > 0.AND.IP>IPFMAX) THEN
                CALL FRICTION_PARTS_SEARCH (
     .                         IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                         INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )
                IF(IPL /=0) THEN
                  IPFMAX = IP
                  IPFLMAX = IPL
                ENDIF
              ENDIF
            ENDDO
C
C
            IF(IPFMAX/=0) THEN
              IPARTFRICS(I) = IPFLMAX
            ENDIF

          ENDDO
        ENDIF
C
        IF(NUMELC/=0.OR.NUMELTG/=0) THEN
          DO I = 1,NSN
            IPFMAX = 0
            IPFLMAX = 0
            DO J= KNOD2ELC(NSV(I))+1,KNOD2ELC(NSV(I)+1)
              IE = NOD2ELC(J)
              IP = IPARTC(IE)
              IPG = TAGPRT_FRIC(IP)
              IF(IPG > 0.AND.IP>IPFMAX) THEN
                CALL FRICTION_PARTS_SEARCH (
     .                         IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                           INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )
                IF(IPL /=0) THEN
                  IPFMAX = IP
                  IPFLMAX = IPL
                ENDIF
              ENDIF
            ENDDO
C
            DO J= KNOD2ELTG(NSV(I))+1,KNOD2ELTG(NSV(I)+1)
              IE = NOD2ELTG(J)
              IP = IPARTTG(IE)
              IPG = TAGPRT_FRIC(IP)
              IF(IPG > 0.AND.IP>IPFMAX) THEN
                CALL FRICTION_PARTS_SEARCH (
     .                         IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                         INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )

                IF(IPL /=0) THEN
                  IPFMAX = IP
                  IPFLMAX = IPL
                ENDIF
              ENDIF
            ENDDO
C
            IF(IPFMAX/=0) THEN
              IPARTFRICS(I) = IPFLMAX
            ENDIF

          ENDDO
        ENDIF
C

      ENDIF

C----------------------------------

C------------------------------------
C     GAP STIF FACES MAIN
C------------------------------------
      CALL I25GAPM(
     1 X     ,IRECT ,STF   ,IXS   ,PM    ,
     2 GEO   ,NRT   ,IXC   ,NINT  ,STFAC ,
     3 NTY   ,GAP   ,NOINT ,STFN  ,NSN   ,
     4 MS    ,NSV   ,IXTG  ,IGAP  ,GAP_M ,
     6 IXT   ,IXP   ,
     8 SLSFAC,DXM   ,NDX   ,
     9 KNOD2ELS ,KNOD2ELC ,KNOD2ELTG ,NOD2ELS ,
     A NOD2ELC,NOD2ELTG ,INTTH,
     B IELES  ,IELEM    ,AREAS    ,SH4TREE ,SH3TREE ,
     C IPART  ,IPARTC   ,IPARTTG  ,THK ,THK_PART    ,
     D IXR    ,ITAB    ,BGAPSMX   ,IXS10   ,MSEGTYP ,
     E IXS16  ,IXS20   ,GAP_N     ,GAPS_MX ,GAPM_MX ,
     F GAPMX  , GAPMN  ,GAPSCALE  ,GAPMAX_M,
     G ID     ,TITR    ,IGEO      ,FILLSOL ,NRTT ,
     H PM_STACK, IWORKSH,INTFRIC,TAGPRT_FRIC,IPARTFRICS,
     I IPARTFRICM,IPARTS,INTBUF_FRIC_TAB,IPARTSM,INRTIE,
     J IVIS2)
C---------------------------
C     GAP
C---------------------------
      GAPMX=SQRT(GAPMX)
      GAPMX=MIN(GAPMX,GAPMAX_M)
C GAP VARIABLE :
C    - GAPMIN CONTIENT ONE GAP MINIMUM UTILISE SI GAP_S(I)+GAP_M(J) < GAPMIN
C    - GAP CONTIENT LE SUP DE (GAP_S(I)+GAP_M(J),GAPMIN)
      IF(GAP<=ZERO)THEN
        IF(NDX/=0)THEN
          GAPMIN = GAPMN
          GAPMIN = MIN(HALF*GAPMX,GAPMIN)
        ELSE
C          GAPMIN = EM01 * GAPMX
          GAPMIN = ZERO
        ENDIF
C        WRITE(IOUT,1300)GAPMIN
      ELSE
        GAPMIN = GAP
      ENDIF
C------recalculate GAP_MIN,MAX
      GAPMX=ZERO
      GAPMN=EP30
      DO I=1,NRT
        GAPMX=MAX(GAPMX,GAP_M(I))
        GAPMN=MIN(GAPMN,GAP_M(I))
      END DO
      IF(IPRI>=1) THEN
        IF(GAP<=ZERO)THEN
          WRITE(IOUT,1400)GAPS_MN,GAPS_MX
          WRITE(IOUT,1500)GAPMN,GAPM_MX
        END IF
      END IF!(IPRI>=1) THEN
C
C SUP DES GAPS VARIABLES
      IF( IGAP == 3) THEN
        GAP = MIN(GAPS_MX+GAPM_MX,GAPS_L_MX+GAPM_L_MX)
      ELSE
        GAP = GAPS_MX+GAPM_MX
      END IF
C---------------------------------------------
C     MISE A ONE DU MULTIPLICATEUR NODALE DES RIGIDITES
C---------------------------------------------
      DO 610 L=1,NSN
        STFN(L) = ONE
  610 CONTINUE
C
C Calcul du gap reel a utiliser lors du critere de retri
C
      BGAPSMX = ZERO
      GAPINF=EP30
      DO I = 1, NSN
        BGAPSMX = MAX(BGAPSMX,GAP_S(I))
      ENDDO
      DO I = 1, NRT
C
C         GAPINF among shells only
        IF(MSEGTYP(I)/=0) GAPINF = MIN(GAPINF,GAP_M(I))
      ENDDO
      GAPINF=MAX(GAPINF,GAPMIN)
C
      DO I=1,NRT
        CALL INSOL3ET(X   ,IRECT ,IXS     ,NINT   ,NELS,I ,
     .                AREA ,NOINT ,KNOD2ELS,NOD2ELS,IXS10 ,
     .                IXS16,IXS20 ,NNOD)
C-------supposing only small segments (sub-triangles) for 10 nodes tetras --------------
        IF (NNOD==10) THEN
          GAP_N(1,I) = THREE*ONE_OVER_8*GAP_N(1,I)
          STF(I) = SIXTEEN*STF(I)
        ELSEIF (NNOD==16) THEN
          GAP_N(1,I) = GAP_N(1,I)/4
        END IF
      END DO
      IF (INACTI/=0) THEN
        CALL I24NORMNS(
     1  X     ,IRECT ,NRT   ,NSN   ,NSV   ,PEN_OLD)
      END IF
C calcul du surface second. ---
      IF(INTTH > 0 .OR. IVIS2==-1) THEN

        IF(NUMELC/=0) THEN

          IF(NADMESH==0)THEN
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
                AREAS(I) = AREAS(I)
     .                     + ONE_OVER_8*SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
C
                IF(INTTH > 0) THEN
                  IELES(I)  = IXC(1,IE)
                ENDIF
              END DO
C
              DO J= KNOD2ELTG(NSV(I))+1,KNOD2ELTG(NSV(I)+1)
                IE = NOD2ELTG(J)
                SX1 = X(1,IXTG(3,IE)) - X(1,IXTG(2,IE))
                SY1 = X(2,IXTG(3,IE)) - X(2,IXTG(2,IE))
                SZ1 = X(3,IXTG(3,IE)) - X(3,IXTG(2,IE))
                SX2 = X(1,IXTG(4,IE)) - X(1,IXTG(2,IE))
                SY2 = X(2,IXTG(4,IE)) - X(2,IXTG(2,IE))
                SZ2 = X(3,IXTG(4,IE)) - X(3,IXTG(2,IE))
                SX3  = SY1*SZ2 - SZ1*SY2
                SY3  = SZ1*SX2 - SX1*SZ2
                SZ3  = SX1*SY2 - SY1*SX2
                AREAS(I) = AREAS(I)
     .                     + ONE_OVER_6*SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
C
                IF(INTTH > 0) THEN
                  IELES(I)  = IXTG(1,IE)
                ENDIF
              END DO
            END DO
          ELSE
            DO I = 1,NSN
              AREAS(I) = ZERO
              DO J= KNOD2ELC(NSV(I))+1,KNOD2ELC(NSV(I)+1)
                IE = NOD2ELC(J)

                IP = IPARTC(IE)
                NLEV =IPART(10,IP)
                MYLEV=SH4TREE(3,IE)
                IF(MYLEV < 0) MYLEV=-(MYLEV+1)

                IF(MYLEV==NLEV)THEN
                  SX1 = X(1,IXC(4,IE)) - X(1,IXC(2,IE))
                  SY1 = X(2,IXC(4,IE)) - X(2,IXC(2,IE))
                  SZ1 = X(3,IXC(4,IE)) - X(3,IXC(2,IE))
                  SX2 = X(1,IXC(5,IE)) - X(1,IXC(3,IE))
                  SY2 = X(2,IXC(5,IE)) - X(2,IXC(3,IE))
                  SZ2 = X(3,IXC(5,IE)) - X(3,IXC(3,IE))
                  SX3  = SY1*SZ2 - SZ1*SY2
                  SY3  = SZ1*SX2 - SX1*SZ2
                  SZ3  = SX1*SY2 - SY1*SX2
                  AREAS(I) = AREAS(I)
     .                       + ONE_OVER_8*SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
C
                  IF(INTTH > 0) THEN
                    IELES(I)  = IXC(1,IE)
                  ENDIF
                END IF

              END DO
C
              DO J= KNOD2ELTG(NSV(I))+1,KNOD2ELTG(NSV(I)+1)
                IE = NOD2ELTG(J)

                IP = IPARTTG(IE)
                NLEV =IPART(10,IP)
                MYLEV=SH3TREE(3,IE)
                IF(MYLEV < 0) MYLEV=-(MYLEV+1)

                IF(MYLEV==NLEV)THEN
                  SX1 = X(1,IXTG(3,IE)) - X(1,IXTG(2,IE))
                  SY1 = X(2,IXTG(3,IE)) - X(2,IXTG(2,IE))
                  SZ1 = X(3,IXTG(3,IE)) - X(3,IXTG(2,IE))
                  SX2 = X(1,IXTG(4,IE)) - X(1,IXTG(2,IE))
                  SY2 = X(2,IXTG(4,IE)) - X(2,IXTG(2,IE))
                  SZ2 = X(3,IXTG(4,IE)) - X(3,IXTG(2,IE))
                  SX3  = SY1*SZ2 - SZ1*SY2
                  SY3  = SZ1*SX2 - SX1*SZ2
                  SZ3  = SX1*SY2 - SY1*SX2
                  AREAS(I) = AREAS(I)
     .                       + ONE_OVER_6*SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
C
                  IF(INTTH > 0) THEN
                    IELES(I)  = IXTG(1,IE)
                  ENDIF
                END IF

              END DO
            END DO
          END IF
        ENDIF
      ENDIF

C
      IF(INTTH > 0 ) THEN

        IF(ILEV /= 3 )THEN
          IF(NUMELS/=0)THEN
            DO I = 1,NSN
              AREAS(I) = ZERO
              DO J= KNOD2ELS(NSV(I))+1,KNOD2ELS(NSV(I)+1)
                IE = NOD2ELS(J)
                INRT = INRTIE(IE)
                IF(INRT/=0)THEN
                  N1=IRECT(1,INRT)
                  N2=IRECT(2,INRT)
                  N3=IRECT(3,INRT)
                  N4=IRECT(4,INRT)

                  IF(N3 /= N4) THEN
                    SX1 = X(1,N3) - X(1,N1)
                    SY1 = X(2,N3) - X(2,N1)
                    SZ1 = X(3,N3) - X(3,N1)
                    SX2 = X(1,N4) - X(1,N2)
                    SY2 = X(2,N4) - X(2,N2)
                    SZ2 = X(3,N4) - X(3,N2)
                    SX3  = SY1*SZ2 - SZ1*SY2
                    SY3  = SZ1*SX2 - SX1*SZ2
                    SZ3  = SX1*SY2 - SY1*SX2
                    AREA = ONE_OVER_8*SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
                    AREAS(I) = AREAS(I) + AREA
C

                  ELSE
                    SX1 = X(1,N2) - X(1,N1)
                    SY1 = X(2,N2) - X(2,N1)
                    SZ1 = X(3,N2) - X(3,N1)
                    SX2 = X(1,N3) - X(1,N1)
                    SY2 = X(2,N3) - X(2,N1)
                    SZ2 = X(3,N3) - X(3,N1)
                    SX3  = SY1*SZ2 - SZ1*SY2
                    SY3  = SZ1*SX2 - SX1*SZ2
                    SZ3  = SX1*SY2 - SY1*SX2
                    AREA = ONE_OVER_6*SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
                    AREAS(I) = AREAS(I) + AREA
                  ENDIF
                  IF(INTTH > 0) THEN
                    IELES(I)  = IXS(1,IE)
                  ENDIF
                ENDIF
              END DO
            ENDDO
          ENDIF
        ENDIF

      END IF
C
      IF(INTTH > 0)THEN
        IF(DRAD==ZERO)THEN
          DRAD=MAX(GAP,GAPMX)
        ELSEIF(DRAD<GAP)THEN
          DRAD=GAP
        END IF
        WRITE(IOUT,2001)DRAD

        IF(DRAD>GAPMX)THEN
          CALL ANCMSG(MSGID=918,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=ID,
     .                C1=TITR,
     .                R1=DRAD ,
     .                R2=GAPMX,
     .                I2=ID)
        END IF
      END IF

      IF(INTTH > 0) DEALLOCATE(INRTIE)

      RETURN
 1300 FORMAT(2X,'GAP MIN = ',1PG20.13)
 1400 FORMAT(2X,'MIN,MAX OF SECONDARY GAP: ',2(1PG20.13))
 1500 FORMAT(2X,'MIN,MAX OF MAIN GAP: ',2(1PG20.13)/)
 2001 FORMAT(2X,'Maximum distance for radiation computation = ',
     .                                                    1PG20.13)
      END
Chd|====================================================================
Chd|  I25GAPM                       source/interfaces/inter3d1/i25sti3.F
Chd|-- called by -----------
Chd|        I25STI3                       source/interfaces/inter3d1/i25sti3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRICTION_PARTS_SEARCH         source/interfaces/inter3d1/i7sti3.F
Chd|        I4GMX3                        source/interfaces/inter3d1/i4gmx3.F
Chd|        INCOQ3                        source/interfaces/inter3d1/incoq3.F
Chd|        INSOL3D                       source/interfaces/inter3d1/insol3.F
Chd|        VOLINT                        source/interfaces/inter3d1/volint.F
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I25GAPM(
     1 X     ,IRECT ,STF   ,IXS   ,PM    ,
     2 GEO   ,NRT   ,IXC   ,NINT  ,STFAC ,
     3 NTY   ,GAP   ,NOINT ,STFN  ,NSN   ,
     4 MS    ,NSV   ,IXTG  ,IGAP  ,GAP_M ,
     6 IXT   ,IXP   ,
     7 SLSFAC,DXM   ,NDX   ,
     9 KNOD2ELS ,KNOD2ELC ,KNOD2ELTG ,NOD2ELS ,
     A NOD2ELC,NOD2ELTG ,INTTH,
     B IELES  ,IELEM    ,AREAS    ,SH4TREE ,SH3TREE ,
     C IPART  ,IPARTC   ,IPARTTG  ,THK ,THK_PART    ,
     D IXR    ,ITAB    ,BGAPSMX   ,IXS10   ,MSEGTYP ,
     E IXS16  ,IXS20   ,GAP_N     ,GAPS1   ,GAPS2   ,
     F GAPMX  , GAPMN  ,GAPSCALE  ,GAPMAX_M,
     G ID     ,TITR    ,IGEO      ,FILLSOL ,NRTT ,
     H PM_STACK, IWORKSH,INTFRIC,TAGPRT_FRIC,IPARTFRICS,
     I IPARTFRICM,IPARTS,INTBUF_FRIC_TAB,IPARTSM,INRTIE,
     J IVIS2  )
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUF_FRIC_MOD
C
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
#include      "scr17_c.inc"
#include      "scr08_c.inc"
#include      "my_allocate.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRT, NINT, NTY, NOINT,NSN,IGAP, NDX,INTFRIC
      INTEGER IRECT(4,*), IXS(NIXS,*), IXC(NIXC,*),
     .   NSV(*), IXTG(NIXTG,*), IXT(NIXT,*), IXP(NIXP,*),
     .   KNOD2ELS(*), KNOD2ELC(*), KNOD2ELTG(*), NOD2ELS(*), NOD2ELC(*),
     .   NOD2ELTG(*), IELES(*), INTTH, IELEM(*),
     .   SH3TREE(KSH3TREE,*), SH4TREE(KSH4TREE,*),IXR(NIXR,*) ,
     .   IPART(LIPART1,*), IPARTC(*), IPARTTG(*),IPARTS(*),
     .   ITAB(*), IXS10(6,*),MSEGTYP(*), IXS16(*), IXS20(*),
     .   IGEO(NPROPGI,*),NRTT,IWORKSH(3,*),TAGPRT_FRIC(*),IPARTFRICS(*),
     .   IPARTFRICM(*) ,IPARTSM(*),INRTIE(*)
      INTEGER  , INTENT(IN) :: IVIS2
C     REAL
      my_real
     .   STFAC, GAP,BGAPSMX,GAPS1 ,GAPS2,GAPMX ,GAPMN ,GAPSCALE
C     REAL
      my_real
     .   X(3,*), STF(*), PM(NPROPM,*), GEO(NPROPG,*), STFN(*),
     .   MS(*),GAP_M(*),GAP_N(4,*),
     .   AREAS(*),THK(*),THK_PART(*),SLSFAC,DXM ,GAPMAX_M, FILLSOL(*),
     .   PM_STACK(20,*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, INRT, NELS, MT, JJ, JJJ, NELC,
     .   MG, NUM, NPT, LL, L, NN, NELTG,N1,N2,N3,N4,IE,
     .   IP, NLEV, MYLEV, K, P, R, T,IAD,NREV,IGTYP,IPGMAT,IGMAT,
     .   ISUBSTACK,IPL,IPG,ISOL
      INTEGER, DIMENSION(:),ALLOCATABLE  :: TAGELEMS,INDEXE
C     REAL
      my_real
     .   AREA, VOL, DX, GAPM, DDX,
     .   GAPTMP, SX1,SY1,SZ1,SX2,SY2,SZ2,SX3,SY3,SZ3,
     .   XL,STIFC, STV, STC
C----------------------
      ISOL = 0
      NREV=0
      IPGMAT = 700
      IF(NUMELS > 0) THEN
        MY_ALLOCATE(TAGELEMS,NUMELS)
        TAGELEMS = 0
        MY_ALLOCATE(INDEXE,NUMELS)
        INDEXE = 0
      ENDIF
      DO I=1,NRT
        STF(I)=ZERO
        IF(INTTH > 0 ) IELEM(I) = 0
        IF(SLSFAC<ZERO)STF(I)=SLSFAC
        GAPM    =ZERO
        GAP_M(I)=GAPM
        CALL I4GMX3(X,IRECT,I,GAPMX)
C----------------------
C      ELEMENTS SOLIDES
C----------------------
        CALL INSOL3D(X,IRECT,IXS,NINT,NELS,I   ,
     .             AREA,NOINT,KNOD2ELS ,NOD2ELS ,0,
     .             IXS10,IXS16,IXS20,TAGELEMS,INDEXE)
        IF(NELS/=0) THEN
          MT=IXS(1,NELS)
          IF(INTTH > 0 ) IELEM(I) = MT
          IF(INTTH > 0 ) INRTIE(NELS) = I
          IF(MT>0)THEN
            DO JJ=1,8
              JJJ=IXS(JJ+1,NELS)
              XC(JJ)=X(1,JJJ)
              YC(JJ)=X(2,JJJ)
              ZC(JJ)=X(3,JJJ)
            ENDDO
            CALL VOLINT(VOL)
            STF(I)=SLSFAC*FILLSOL(NELS)*AREA*AREA*PM(32,MT)/VOL
          ELSE
            IF(NINT>=0) THEN
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXS(NIXS,NELS),
     .                    C2='SOLID',
     .                    I3=I)
            ENDIF
            IF(NINT<0) THEN
              CALL ANCMSG(MSGID=96,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXS(NIXS,NELS),
     .                    C2='SOLID',
     .                    I3=I)
            ENDIF
          ENDIF
          ISOL = 1
          GAP_N(1,I)=VOL/AREA
C--------Correction for different elements
          IF(NELS>NUMELS8.AND.NELS<=NUMELS8+NUMELS10)THEN
            GAP_N(1,I) = THREE*ONE_OVER_8*GAP_N(1,I)
            STF(I) = SIXTEEN*STF(I)
          ELSEIF(NELS>NUMELS8+NUMELS10+NUMELS20.AND.NELS<=NUMELS8+NUMELS10+NUMELS20+NUMELS16) THEN
            GAP_N(1,I) = GAP_N(1,I)/4
          END IF

C -----Friction model ------
          IF(INTFRIC > 0) THEN
            IP= IPARTS(NELS)
            IPG = TAGPRT_FRIC(IP)
            IF(IPG > 0) THEN
              CALL FRICTION_PARTS_SEARCH (
     .             IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .             INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )
              IPARTFRICM(I) = IPL
              IPARTSM(I) = IP
            ENDIF
          ENDIF
C------------------------------------
        ENDIF
C---------------------
C     ELEMENTS COQUES
C---------------------
        CALL INCOQ3(IRECT,IXC ,IXTG ,NINT ,NELC     ,
     .              NELTG,I   ,GEO  ,PM   ,KNOD2ELC ,
     .              KNOD2ELTG ,NOD2ELC ,NOD2ELTG,THK,NTY,IGEO,
     .              PM_STACK  , IWORKSH)
        IF(NELTG/=0) THEN
C
          MT=IXTG(1,NELTG)
          MG=IXTG(5,NELTG)
          IGTYP = IGEO(11,MG)
          IP = IPARTTG(NELTG)
          IF(INTTH > 0 ) IELEM(I) = MT
          IF ( THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN
            DX=THK_PART(IP)*GAPSCALE
          ELSEIF ( THK(NUMELC+NELTG) /= ZERO .AND. IINTTHICK == 0)THEN
            DX=THK(NUMELC+NELTG)*GAPSCALE
          ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP ==52) THEN
            DX=THK(NUMELC+NELTG)*GAPSCALE
          ELSE
            DX=GEO(1,MG)*GAPSCALE
          ENDIF
          GAPM=HALF*DX
          GAPS2=MAX(GAPS2,GAPM)
          GAPMN = MIN(GAPMN,DX)
          DXM=DXM+DX
          NDX=NDX+1
          GAP_M(I)=MAX(GAP_M(I),GAPM)
C -----Friction model ------
          IF(INTFRIC > 0) THEN
            IP= IPARTTG(NELTG)
            IPG = TAGPRT_FRIC(IP)
            IF(IPG > 0) THEN
              CALL FRICTION_PARTS_SEARCH (
     .             IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .             INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )
              IPARTFRICM(I) = IPL
              IPARTSM(I) = IP
            ENDIF
          ENDIF
C------------------------------------
          IF(MT>0)THEN
            IF(IGTYP ==11 .AND. IGMAT > 0 ) THEN
              IF ( THK(NUMELC+NELTG) /= ZERO .AND. IINTTHICK == 0) THEN
                STC=SLSFAC*THK(NUMELC+NELTG)*GEO(IPGMAT + 2 ,MG)
              ELSE
                STC=SLSFAC*GEO(1,MG)*GEO(IPGMAT + 2 ,MG)
              ENDIF
            ELSEIF(IGTYP ==52.OR.
     .           ((IGTYP == 17 .OR. IGTYP == 51)  .AND. IGMAT > 0))THEN
              ISUBSTACK = IWORKSH(3,NUMELC+NELTG)
              STC=SLSFAC*THK(NUMELC+NELTG)*PM_STACK(2 ,ISUBSTACK)
            ELSE
              IF ( THK(NUMELC+NELTG) /= ZERO .AND. IINTTHICK == 0) THEN
                STC=SLSFAC*THK(NUMELC+NELTG)*PM(20,MT)
              ELSEIF(IGTYP == 17 .OR. IGTYP == 51) THEN
                STC=SLSFAC*THK(NUMELC+NELTG)*PM(20,MT)
              ELSE
                STC=SLSFAC*GEO(1,MG)*PM(20,MT)
              ENDIF
            ENDIF
C
            STF(I)=MAX(STF(I),STC)
            IF (MSEGTYP(I) > 0) THEN   ! (MSEGTYP /=0 .AND. MSEGTYP <= NRTT) .OR. MSEGTYP > NRTT
              J= MSEGTYP(I)
              IF(J > NRTT) J=J-NRTT
              STF(J)  = STC
              GAP_M(J)= GAP_M(I)
              IF(INTTH > 0 ) IELEM(J) = IELEM(I)
              IF(INTFRIC > 0) IPARTFRICM(J)=IPARTFRICM(I)
            END IF
C
          ELSE
            IF(NINT>=0) THEN
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXTG(NIXTG,NELTG),
     .                    C2='SHELL',
     .                    I3=I)
            ENDIF
            IF(NINT<0) THEN
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
        ELSEIF(NELC/=0) THEN
          MT=IXC(1,NELC)
          MG=IXC(6,NELC)
          IP = IPARTC(NELC)
          IGTYP = IGEO(11,MG)
          IGMAT = IGEO(98,MG)
          IF(INTTH > 0 ) IELEM(I) = MT
          IF ( THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN
            DX=THK_PART(IP)*GAPSCALE
          ELSEIF ( THK(NELC) /= ZERO .AND. IINTTHICK == 0) THEN
            DX=THK(NELC)*GAPSCALE
          ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP ==52) THEN
            DX=THK(NELC)*GAPSCALE
          ELSE
            DX=GEO(1,MG)*GAPSCALE
          ENDIF
          GAPM=HALF*DX
          GAPS2=MAX(GAPS2,GAPM)
          GAPMN = MIN(GAPMN,DX)
          DXM=DXM+DX
          NDX=NDX+1
          GAP_M(I)=MAX(GAP_M(I),GAPM)
C -----Friction model ------
          IF(INTFRIC > 0) THEN
            IP= IPARTC(NELC)
            IPG = TAGPRT_FRIC(IP)
            IF(IPG > 0) THEN
              CALL FRICTION_PARTS_SEARCH (
     .             IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .             INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )
              IPARTFRICM(I) = IPL
              IPARTSM(I) = IP
            ENDIF
          ENDIF
C------------------------------------
          IF(MT>0)THEN
            IF(IGTYP == 11 .AND. IGMAT > 0) THEN
              IF ( THK(NELC) /= ZERO .AND. IINTTHICK == 0) THEN
                STC=SLSFAC*THK(NELC)*GEO(IPGMAT + 2 ,MG)
              ELSE
                STC=SLSFAC*GEO(1,MG)*GEO(IPGMAT + 2 ,MG)
              ENDIF
            ELSEIF(IGTYP ==52.OR.
     .           ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0))THEN
              ISUBSTACK = IWORKSH(3,NELC)
              STC=SLSFAC*THK(NELC)*PM_STACK(2,ISUBSTACK)
            ELSE
              IF ( THK(NELC) /= ZERO .AND. IINTTHICK == 0) THEN
                STC=SLSFAC*THK(NELC)*PM(20,MT)
              ELSEIF(IGTYP == 17 .OR. IGTYP == 51) THEN
                STC=SLSFAC*THK(NELC)*PM(20,MT)
              ELSE
                STC=SLSFAC*GEO(1,MG)*PM(20,MT)
              ENDIF
            ENDIF
C
            STF(I)=MAX(STF(I),STC)
            IF (MSEGTYP(I) > 0) THEN   ! (MSEGTYP /=0 .AND. MSEGTYP <= NRTT) .OR. MSEGTYP > NRTT
              J= MSEGTYP(I)
              IF(J > NRTT) J=J-NRTT
              STF(J)  = STC
              GAP_M(J)= GAP_M(I)
              IF(INTTH > 0 ) IELEM(J) = IELEM(I)
              IF(INTFRIC > 0) IPARTFRICM(J)=IPARTFRICM(I)
            END IF
C
          ELSE
            IF(NINT>=0) THEN
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXC(NIXC,NELC),
     .                    C2='SHELL',
     .                    I3=I)
            ENDIF
            IF(NINT<0) THEN
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
C
        IF(NELS+NELC+NELTG==0)THEN
C      en SPMD il faut un element associe a l'arrete sinon erreur
          IF(NINT>0) THEN
            CALL ANCMSG(MSGID=481,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
          ENDIF
          IF(NINT<0) THEN
            CALL ANCMSG(MSGID=482,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
          ENDIF
        ENDIF
      END DO
C
      IF(NUMELS > 0) DEALLOCATE(TAGELEMS,INDEXE)
C
      IF(IVIS2 ==-1.AND.ISOL /=0) THEN
        CALL ANCMSG(MSGID=2096,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_2,
     .              I1=ID,
     .              C1=TITR)
      ENDIF
C
      DO I=1,NRTT
        GAP_M(I)=MIN(GAP_M(I),GAPMAX_M)
      END DO
C-----------------------------------------------
      RETURN
 1400 FORMAT(I10,' MAIN SEGMENTS',' OF INTERFACE',I10,
     +                      ' ARE REVERSED THE NORMAL DIRECTION')
      END
Chd|====================================================================
Chd|  I25BORD                       source/interfaces/inter3d1/i25sti3.F
Chd|-- called by -----------
Chd|        I25STI3                       source/interfaces/inter3d1/i25sti3.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|====================================================================
      SUBROUTINE I25BORD(NRTM   ,IRECT ,TAGB    )
C============================================================================
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM, IRECT(4,*), TAGB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,NLMAX,STAT,LL,I1,I2,I3,I4,I5,I1M,I2M,IS,BORD,BOLD
      INTEGER NEXTK(4),IWORK(70000),NL
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::
     .   LINEIX
      INTEGER, DIMENSION(:), ALLOCATABLE ::
     .   INDEX


      DATA NEXTK/2,3,4,1/
C=======================================================================
      NLMAX = 4*NRTM

      ALLOCATE (LINEIX(2,NLMAX)    ,STAT=stat)
      ALLOCATE (INDEX(2*NLMAX)     ,STAT=stat)
c---------------------------------------
c       recherche de toutes les lignes dans la surface
c---------------------------------------
      IS = 0
      LL = 0
      DO J=1,NRTM
        IS = IS+1
        I1=IRECT(1,J)
        I2=IRECT(2,J)
        I3=IRECT(3,J)
        I4=IRECT(4,J)
        DO K=1,3
          I1=IRECT(K,J)
          I2=IRECT(NEXTK(K),J)
          LL = LL+1
          IF(I2 > I1)THEN
            LINEIX(1,LL) = I1
            LINEIX(2,LL) = I2
          ELSE
            LINEIX(1,LL) = I2
            LINEIX(2,LL) = I1
          ENDIF
        ENDDO
        IF(I3/=I4)THEN
          K=4
          I1=IRECT(K,J)
          I2=IRECT(NEXTK(K),J)
          LL = LL+1
          IF(I2 > I1)THEN
            LINEIX(1,LL) = I1
            LINEIX(2,LL) = I2
          ELSE
            LINEIX(1,LL) = I2
            LINEIX(2,LL) = I1
          ENDIF
        END IF
      ENDDO
C
      CALL MY_ORDERS(0,IWORK,LINEIX,INDEX,LL,2)

c---------------------------------------
c       suppression des lignes doubles
c---------------------------------------
      I1M = LINEIX(1,INDEX(1))
      I2M = LINEIX(2,INDEX(1))
      BORD=1
      BOLD=1
      DO L=2,LL
        I1 = LINEIX(1,INDEX(L))
        I2 = LINEIX(2,INDEX(L))
        IF(I2 == I2M .and. I1 == I1M)THEN
C nest pas sur le bord
          BORD=0
          BOLD=0
        ELSEIF(BOLD == 0)THEN
C on vient de recoller, on repart avec cette nouvelle arete.
C           BORD=0
          BOLD=1
        ELSE
          BORD=1 ! bord
          BOLD=1
          TAGB(I1M) = 1
          TAGB(I2M) = 1
        ENDIF
        I1M = I1
        I2M = I2
      ENDDO

      IF(BORD==1)THEN
c         derniere arrete est un bord
        TAGB(I1) = 1
        TAGB(I2) = 1
      ENDIF


      DEALLOCATE (INDEX)
      DEALLOCATE (LINEIX)
C-----------
      RETURN
      END

