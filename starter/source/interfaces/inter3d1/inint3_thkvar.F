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
Chd|  ININT3_THKVAR                 source/interfaces/inter3d1/inint3_thkvar.F
Chd|-- called by -----------
Chd|        ININTR_THKVAR                 source/interfaces/interf1/inintr_thkvar.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        I21BUC1                       source/interfaces/inter3d1/i21buc3.F
Chd|        I21DST3                       source/interfaces/inter3d1/i21dst3.F
Chd|        I21GAP3                       source/interfaces/inter3d1/i21gap3.F
Chd|        I21NORM3                      source/interfaces/inter3d1/i21norm3.F
Chd|        I21PWR3                       source/interfaces/inter3d1/i21pwr3.F
Chd|        I21RCURV                      source/interfaces/inter3d1/i21norm3.F
Chd|        I21RESET                      source/interfaces/inter3d1/i21reset.F
Chd|        I21ROT                        source/interfaces/inter3d1/i21rot.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTSTAMP_MOD                  share/modules1/intstamp_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ININT3_THKVAR(
     1               INTBUF_TAB  ,INSCR      ,X    ,IXS     ,
     2               IXC    ,IXTG   ,PM      ,GEO  ,IPARI   ,
     3               NUMINT ,ITAB   ,MWA     ,RWA  ,IWRN    ,
     4               IKINE     ,KNOD2ELS,KNOD2ELC,KNOD2ELTG ,NOD2ELS ,
     5               NOD2ELC,NOD2ELTG                       ,
     6               THKSH4_VAR,THKSH3_VAR,THKNOD,INTSTAMP,SKEW   ,
     7               MS        ,IN        ,V       ,VR    ,RBY    ,
     8               NPBY      ,LPBY      ,I_MEM   ,RESORT,IPARTS ,
     9               IPARTC    ,IPARTG    ,THK_PART,ID       ,TITR,
     A               NOM_OPT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTSTAMP_MOD
      USE INTBUFDEF_MOD 
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr12_c.inc"
#include      "units_c.inc"
#include      "vect07_c.inc"
#include      "warn_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMINT, IWRN, I_MEM, RESORT    
      INTEGER INSCR(*), IXS(NIXS,*), IXC(NIXC,*),
     .   IXTG(NIXTG,*), IPARI(*), ITAB(*), MWA(*), IKINE(*),
     .   KNOD2ELS(*), KNOD2ELC(*), KNOD2ELTG(*),
     .   NOD2ELS(*), NOD2ELC(*), NOD2ELTG(*),
     .   NPBY(NNPBY,*), LPBY(*), IPARTS(*), IPARTC(*), IPARTG(*)
      TYPE(INTSTAMP_DATA) INTSTAMP
      INTEGER NOM_OPT(LNOPT1,*)
C     REAL
      my_real
     .   X(3,*), PM(*), GEO(*),RWA(6,*),
     .   THKSH4_VAR(*), THKSH3_VAR(*), THKNOD(*),
     .   SKEW(LSKEW,*), MS(*), IN(*), V(3,*), VR(3,*), RBY(NRBY,*),
     .   THK_PART(*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NRTS, NRTM, NSN, NMN, NTY, NST, MST, NOINT,
     .   I, I_STOK,IRS,IRM,
     .   NSEG, NGROUS, NG, INACTI,IWPENE,
     .   JLT_NEW,IGAP,MULTIMP,IADM,INTTH,
     .   ISU1, ISU2,
     .   ISK, IROT, J, MSR, IRB, K, NSL, N
      INTEGER, DIMENSION(MVSIZ) ::PROV_N,PROV_E
      INTEGER, DIMENSION(MVSIZ) :: IX1,IX2,IX3,IX4,NSVG
      my_real, DIMENSION(MVSIZ) :: X1,X2,X3,X4
      my_real, DIMENSION(MVSIZ) :: Y1,Y2,Y3,Y4
      my_real, DIMENSION(MVSIZ) :: Z1,Z2,Z3,Z4
      my_real, DIMENSION(MVSIZ) :: XI,YI,ZI
      my_real, DIMENSION(MVSIZ) :: X0,Y0,Z0,STIF
      my_real, DIMENSION(MVSIZ) :: N11,N21,N31,PENE1
      my_real, DIMENSION(MVSIZ) :: NX1,NY1,NZ1
      my_real, DIMENSION(MVSIZ) :: NX2,NY2,NZ2
      my_real, DIMENSION(MVSIZ) :: NX3,NY3,NZ3
      my_real, DIMENSION(MVSIZ) :: NX4,NY4,NZ4
      my_real, DIMENSION(MVSIZ) :: P1,P2,P3,P4
      my_real, DIMENSION(MVSIZ) :: LB1,LB2,LB3,LB4
      my_real, DIMENSION(MVSIZ) :: LC1,LC2,LC3,LC4
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGNOD
      my_real, DIMENSION(:) ,POINTER  :: GAP_S0
      my_real,TARGET, DIMENSION(1):: GAPS0_BID
C     REAL
      my_real
     .   MAXBOX,MINBOX
      my_real
     .   GAPV(MVSIZ), PENE(MVSIZ)
      CHARACTER MESS*40
      DATA MESS/'/INTER/TYPE21 INITIALIZATIONS           '/
C-----------------------------------------------
C
      ALLOCATE(TAGNOD(NUMNOD))
      IWPENE = 0
      NRTS  =IPARI(3)
      NRTM  =IPARI(4)
      NSN   =IPARI(5)
      NMN   =IPARI(6)
      NTY   =IPARI(7)
      NST   =IPARI(8)
      MST   =IPARI(9)
      NOINT =IPARI(15)
      IGAP  =IPARI(21)
      INACTI= IPARI(22)
      MULTIMP= IPARI(23)
      IRM= IPARI(24)
      IRS= IPARI(25)
      ISU1=IPARI(45)
      ISU2=IPARI(46)
      IADM=IPARI(44)
      INTTH=IPARI(47)
C
C
      IF (RESORT == 0)THEN
      WRITE(IOUT,2001)NOINT,NTY
      END IF
C
C=======================================================================
      IF(NTY==21) THEN
C=======================================================================
C
C      CALCUL DU GAP, apres INITIA (lecture des epaisseurs).
C
       IF(IGAP == 2) THEN
         GAP_S0 => INTBUF_TAB%GAP_S(1+NSN:2*NSN)
       ELSE
         GAP_S0 => GAPS0_BID
       ENDIF

       CALL I21GAP3(
     1 X         ,INTBUF_TAB%IRECTS,INTBUF_TAB%IRECTM,NRTS,NRTM,
     2 GEO       ,PM        ,IXS       ,IXC         ,IXTG      ,
     3 -NUMINT   ,NTY       ,NOINT     ,NSN         ,INTBUF_TAB%NSV,
     4 INTBUF_TAB%VARIABLES(2),IGAP    ,INTBUF_TAB%GAP_S,INTBUF_TAB%VARIABLES(13),
     .                                             INTBUF_TAB%VARIABLES(6),
     5 INTBUF_TAB%VARIABLES(16),INTBUF_TAB%IELES,INTBUF_TAB%STF,
     .                  NMN          ,INTBUF_TAB%MSR   ,
     6 KNOD2ELS  ,KNOD2ELC   ,KNOD2ELTG ,NOD2ELS      ,NOD2ELC   ,
     7 NOD2ELTG  ,THKNOD         ,
     8 IKINE        ,ITAB         ,INACTI  ,INTBUF_TAB%VARIABLES(19),
     .                                              INTBUF_TAB%STFNS,
     9 INTBUF_TAB%VARIABLES(23),GAP_S0,INTBUF_TAB%AREAS,
     .                             INTBUF_TAB%XM0,INTBUF_TAB%VARIABLES(27),
     A INTBUF_TAB%VARIABLES(28),INTBUF_TAB%VARIABLES(29),INTTH  ,INTBUF_TAB%VARIABLES(32),
     .                                           IPARTS          , 
     B IPARTC, IPARTG, THK_PART,INTBUF_TAB%THKNOD0      ,ID,TITR ,
     C INTBUF_TAB%VARIABLES(46))
C
C      INITIALISATIONS (PRE-CALCUL DES NORMALES, etc)
C
       CALL I21NORM3(
     1 X        ,INTBUF_TAB%IRECTM,NRTM  ,NMN          ,INTBUF_TAB%MSR ,
     2 -NUMINT  ,NTY          ,NOINT ,INTBUF_TAB%NOD_NORMAL,INTBUF_TAB%XM0)
C
       IF(IADM/=0)THEN
         CALL I21RCURV(
     1 NRTM    ,INTBUF_TAB%XM0,INTBUF_TAB%NOD_NORMAL,INTBUF_TAB%IRECTM,INTBUF_TAB%RCURV,
     2 INTBUF_TAB%ANGLM)
       END IF
C
       CALL I21RESET(NSN, INTBUF_TAB%IRTLM, INTBUF_TAB%CSTS)
C
C      IL FAUT ENCORE FAIRE ONE BUCKET SORT DANS LE STARTER
C
       MAXBOX = INTBUF_TAB%VARIABLES(9)
       MINBOX = INTBUF_TAB%VARIABLES(12)
       CALL I21BUC1(
     1   X  ,INTBUF_TAB%IRECTM  ,INTBUF_TAB%NSV,INTBUF_TAB%VARIABLES(4),NSEG ,
     2   NMN         ,NRTM        ,MWA       ,NSN       ,INTBUF_TAB%CAND_E ,
     3   INTBUF_TAB%CAND_N  ,INTBUF_TAB%VARIABLES(2),RWA ,NOINT,I_STOK    ,
     4   INTBUF_TAB%VARIABLES(5),INTBUF_TAB%VARIABLES(8),MAXBOX ,MINBOX ,INTBUF_TAB%MSR,
     5   INTBUF_TAB%STF  ,MULTIMP   ,ITAB        ,INTBUF_TAB%GAP_S  ,IGAP  ,
     6   INTBUF_TAB%VARIABLES(13),INTBUF_TAB%VARIABLES(16) ,INACTI ,NRTS ,INTBUF_TAB%IRECTS,
     7   INTBUF_TAB%XM0   ,INTBUF_TAB%VARIABLES(23),INTBUF_TAB%VARIABLES(22),
     8                    INTBUF_TAB%VARIABLES(32),ID,TITR,I_MEM,
     9   IX1 ,IX2    ,IX3    ,IX4    ,NSVG   ,
     1   X1  ,X2     ,X3     ,X4     ,Y1     ,
     2   Y2  ,Y3     ,Y4     ,Z1     ,Z2     ,
     3   Z3  ,Z4     ,XI     ,YI     ,ZI     ,
     4   X0  ,Y0     ,Z0     ,STIF   ,NX1    ,
     5   NY1 ,NZ1    ,NX2    ,NY2    ,NZ2    ,
     6   NX3 ,NY3    ,NZ3    ,NX4    ,NY4    ,
     7   NZ4 ,P1     ,P2     ,P3     ,P4     ,
     8   LB1 ,LB2    ,LB3    ,LB4    ,LC1    ,
     9   LC2 ,LC3    ,LC4    ,PENE1   ,PROV_N ,
     1   PROV_E,N11  ,N21    ,N31    ,INTBUF_TAB%VARIABLES(46))

         IF (I_MEM == 2 ) RETURN
       INTBUF_TAB%VARIABLES(9)  = MAXBOX
       INTBUF_TAB%VARIABLES(12) = MINBOX
C
C      PUIS CALCULER LES PENETRATIONS INITIALES AVEC ONE I21DST3
C
       NGROUS=1+(I_STOK-1)/NVSIZ
C
       DO 700 NG=1,NGROUS

        IF(IPRI>=1) WRITE(IOUT,2007)
        NFT = (NG-1) * NVSIZ
        LFT = 1
        LLT = MIN0( NVSIZ, I_STOK - NFT )
        CALL I21DST3(
     1    LLT ,INTBUF_TAB%CAND_N(1+NFT),INTBUF_TAB%CAND_E(1+NFT),INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,
     2    INTBUF_TAB%GAP_S,X     ,INTBUF_TAB%IRTLM,INTBUF_TAB%CSTS,
     .                                                INTBUF_TAB%VARIABLES(23),
     3    INTBUF_TAB%NOD_NORMAL,INTBUF_TAB%XM0,PENE,INTBUF_TAB%PENIS,INTBUF_TAB%IFPEN,
     4    IGAP ,INTBUF_TAB%VARIABLES(2), INTBUF_TAB%VARIABLES(16), INTBUF_TAB%VARIABLES(13) ,
     .                                             INTBUF_TAB%VARIABLES(32),
     5    INTBUF_TAB%VARIABLES(46))
 700   CONTINUE
C
        CALL I21PWR3(
     .   ITAB   ,INACTI,INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,INTBUF_TAB%STFNS,
     1   X      ,I_STOK,INTBUF_TAB%NSV,IWPENE ,INTBUF_TAB%PENIS,
     2   NOINT  ,NTY   ,INTBUF_TAB%GAP_S,INTBUF_TAB%MSR,INTBUF_TAB%IRTLM,
     3   INTBUF_TAB%IRECTM ,INTBUF_TAB%XM0,INTBUF_TAB%VARIABLES(13),INTBUF_TAB%VARIABLES(16),
     .                                                INTBUF_TAB%VARIABLES(23),
     4   NSN    ,MWA   ,INTBUF_TAB%VARIABLES(32),ID,TITR)

       IF(IWPENE==0.AND.(INACTI==5.OR.INACTI==6))
     .   IPARI(22) = -INACTI     ! passage en negatif pour dimensionnenment
       INTBUF_TAB%I_STOK(1)=IWPENE
C
C      IF(NUMELS10>0)CALL
C    .        INSOLT10(IXS(1,NUMELS8+1),IXS(1,NUMELS+1),INTBUF_TAB%IRECTM),
C    .                 NOINT,NRTM,ITAB,KNOD2ELS,NOD2ELS)
C=======================================================================
C       COPIE DONNEES RBODY => INTERFACE
C----------------------------------------------------------------
        IRB=INTSTAMP%IRB
        MSR=INTSTAMP%MSR
        INTSTAMP%XG(1)=X(1,MSR)
        INTSTAMP%XG(2)=X(2,MSR)
        INTSTAMP%XG(3)=X(3,MSR)
        INTSTAMP%V(1) =V(1,MSR)
        INTSTAMP%V(2) =V(2,MSR)
        INTSTAMP%V(3) =V(3,MSR)
        INTSTAMP%MASS =MS(MSR)
        DO J=1,9
          INTSTAMP%ROT(J)=RBY(J,IRB)
        END DO
        INTSTAMP%IN(1)=RBY(10,IRB)
        INTSTAMP%IN(2)=RBY(11,IRB)
        INTSTAMP%IN(3)=RBY(12,IRB)
        INTSTAMP%VR(1)=VR(1,MSR)
        INTSTAMP%VR(2)=VR(2,MSR)
        INTSTAMP%VR(3)=VR(3,MSR)
C----------------------------------------------------------------
C       CHECK MAIN NODES OF INTERFACE BELONG TO ASSOCIATED RBODY
C----------------------------------------------------------------
        K=0
        DO N=1,IRB-1
          K=K+NPBY(2,N)
        END DO
C
        TAGNOD(1:NUMNOD)=0
C
        NSL=NPBY(2,IRB)
        DO I=1,NSL
          N=LPBY(K+I)
          TAGNOD(N)=1
        END DO
C
        DO I=1,NMN
          N=INTBUF_TAB%MSR(I)
          IF(TAGNOD(N)==0)THEN
             CALL ANCMSG(MSGID=932,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=ITAB(N),
     .                   I3=NOM_OPT(1,IRB))
          END IF
        END DO
C----------------------------------------------------------------
C         PASSAGE DE NOD_NORMAL et XM0 en COORD LOCALES
C             - DS REP INERTIES PRINCIPALES)
C----------------------------------------------------------------
        IROT=INTSTAMP%IROT
        CALL I21ROT(
     1   IROT         ,INTSTAMP%XG,INTSTAMP%ROT,NMN ,INTBUF_TAB%NOD_NORMAL,
     2   INTBUF_TAB%XM0,INTSTAMP%BRACKET,NRTM,INTBUF_TAB%IRECTM,
     .                                            INTBUF_TAB%VARIABLES(27),
     3   INTBUF_TAB%VARIABLES(28),INTBUF_TAB%VARIABLES(29),NSN,INTBUF_TAB%XSAV)
C=======================================================================
      ENDIF
      DEALLOCATE(TAGNOD)
C
      RETURN
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
 2001 FORMAT(//,1X,'INTERFACE NUMBER. . . . . . . . . . . . . .',I10/
     +      ,1X,'INTERFACE TYPE. . . . . . . . . . . . . . .',I6/)
 2002 FORMAT(//
     +'   SECONDARY NEAREST  NEAREST        MAIN NODES            SECONDARY '/
     +'   NODE   MAIN  SEGMENT                               S     T')
 2022 FORMAT(//
     +'   SECONDARY  NEAREST        MAIN NODES            SECONDARY '/
     +'   NODE   SEGMENT                               ',
     +'      S      T     DIST')
 2003 FORMAT(//
     +'  MAIN NEAREST  NEAREST        SECONDARY  NODES            MAIN'/
     +'   NODE    SECONDARY  SEGMENT                               S     T')
 2007 FORMAT(//'    IMPACT CANDIDATES',/,
     +'  MAIN        SECONDARY  NODES '/
     +'   NODE ')
 2011 FORMAT(//'    IMPACT CANDIDATES',/,
     +'  MAIN   NODES     SECONDARY  NODES ')
C-----
      END
