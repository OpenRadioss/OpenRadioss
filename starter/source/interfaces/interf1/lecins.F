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
Chd|  LECINS                        source/interfaces/interf1/lecins.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ELTY_SURF                     source/interfaces/interf1/lecins.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        I11EDGE                       source/interfaces/inter3d1/i11edge.F
Chd|        I20SURFI                      source/interfaces/inter3d1/i20surfi.F
Chd|        I24SURFI                      source/interfaces/inter3d1/i24surfi.F
Chd|        I25SURFI                      source/interfaces/inter3d1/i25surfi.F
Chd|        INGRBRIC                      source/interfaces/interf1/ingrbric.F
Chd|        INGRBRIC_CENTROIDS            source/interfaces/interf1/ingrbric_centroids.F
Chd|        INGRBRIC_NODES                source/interfaces/interf1/ingrbric_nodes.F
Chd|        INPOINT                       source/interfaces/interf1/inpoint.F
Chd|        INSLIN                        source/interfaces/interf1/inslin.F
Chd|        INSURF                        source/interfaces/interf1/insurf.F
Chd|        INSURF23                      source/interfaces/interf1/insurf23.F
Chd|        INSURFIGEO                    source/interfaces/interf1/insurfigeo.F
Chd|        NSEGMT                        source/interfaces/interf1/nsegmt.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE LECINS(IPARI   ,ITAB       ,PM       ,IPM       ,BUFMAT  ,
     .                  IGRNOD  ,IGRSURF    ,IGRSLIN  ,XFILTR    ,STFAC   ,
     .                  FRIC_P  ,FRIGAP     ,I2RUPT   ,AREASL    ,LIXINT  ,
     .                  X       ,LINTER     ,IXS      ,NOM_OPT   ,
     .                  IXC     ,IXTG       ,KNOD2ELC ,KNOD2ELTG ,NOD2ELC ,
     .                  NOD2ELTG,INTBUF_TAB ,KNOD2ELS ,NOD2ELS   ,IXS10   ,
     .                  IXS16   ,IXS20      ,NIGE,RIGE,XIGE      ,VIGE    ,
     .                  IGRBRIC ,MULTI_FVM  ,NALE     ,IGEO)
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
      USE MULTI_FVM_MOD
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
#include      "com01_c.inc"
#include      "com02_c.inc"
#include      "com03_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr12_c.inc"
#include      "scr17_c.inc"
#include      "conv_c.inc"
#include      "ige3d_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,NINTER), ITAB(NUMNOD),IXS(NIXS,NUMELS),
     .        IXC(*),IXTG(*),KNOD2ELC(*),KNOD2ELTG(*),
     .        NOD2ELC(*),NOD2ELTG(*),KNOD2ELS(*),NOD2ELS(*),
     .        IXS10(6,*), IXS16(8,*), IXS20(12,*),NIGE(*)
      INTEGER LIXINT, NBRIC
      INTEGER,INTENT(IN) :: IPM(NPROPMI,NUMMAT), IGEO(NPROPGI,NUMGEO)
      my_real,INTENT(IN) :: BUFMAT(SBUFMAT)
      my_real XFILTR(*),STFAC(*),FRIC_P(10,*),FRIGAP(NPARIR,*),I2RUPT (6,*),AREASL(*),RIGE(*),XIGE(*),VIGE(*)
      my_real,INTENT(IN) :: X(3,NUMNOD)
      my_real, INTENT(IN) :: PM(*)
      INTEGER NOM_OPT(LNOPT1,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      INTEGER, INTENT(IN) :: NALE(NUMNOD)      
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (SURF_)   , DIMENSION(NSURF) ,TARGET  :: IGRSURF
      TYPE (SURF_)   , DIMENSION(NSLIN) ,TARGET  :: IGRSLIN
      TYPE(MULTI_FVM_STRUCT)                     :: MULTI_FVM      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J, K, IRS, IRM, NI, NSN, NMN,NMT,
     .   NTYP,IS1,IS2,NOINT,NRTS,NRTM,NST,ILEV, 
     .   MULTIMP,IGAP,INACTI,NME,IALLO,LINTER,
     .   ISU1,ISU2,NRTM0,NRTM_SH,
     .   NRTS_NEW, NRTM_NEW,IBID,INTTH,
     .   NRTM_IGE,NRTM_FE,NRTS_IGE,NRTS_FE,
     .   NMN_IGE,NMN_FE,NSN_IGE,NSN_FE,INTNITSCHE,IAD_IGE,GRBRIC_ID

      INTEGER :: STAT
      INTEGER, DIMENSION(:), ALLOCATABLE, TARGET ::  NTAG_TARGET
      INTEGER, DIMENSION(:), POINTER :: NTAG
      INTEGER, DIMENSION(:,:), POINTER :: SURF_NODES,SURF_NODES_IGE,LINE_NODES
      LOGICAL TYPE18
      CHARACTER MESS*40
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      DATA MESS/'INTERFACE INPUT                         '/
C=======================================================================
       ALLOCATE(NTAG_TARGET(2*NUMNOD+1), STAT=Stat)

       NTAG(0:2*NUMNOD) => NTAG_TARGET(1:2*NUMNOD+1)

       DO K=0,2*NUMNOD
        NTAG(K) = 0
       ENDDO
      LIXINT = 0
C---------------------
      DO NI = 1,LINTER
C
        NRTS   = IPARI(3,NI)
        NRTM   = IPARI(4,NI)
        NME    = IPARI(4,NI)
        NSN    = IPARI(5,NI)
        NMN    = IPARI(6,NI)
        NTYP   = IPARI(7,NI)
        NOINT  = IPARI(15,NI)
        ILEV   = IPARI(20,NI)
        IGAP   = IPARI(21,NI)
        INACTI = IPARI(22,NI)
        MULTIMP= IPARI(23,NI)
        IS1    = IPARI(13,NI)/10
        IS2    = MOD(IPARI(13,NI),10)
        ISU1   = IPARI(45,NI)
        ISU2   = IPARI(46,NI)
        INTTH  = IPARI(47,NI)
        INTNITSCHE  = IPARI(86,NI)
        GRBRIC_ID   = IPARI(83,NI) !int18
        IRM    = 0
        IRS    = 0
C- deactivated interfaces
        IF (NTYP==0) CYCLE
C-----Isogeometric elements
        IF(NTYP==7) THEN
         NRTM_IGE   = IPARI(73,NI)
         NRTM_FE    = IPARI(74,NI)
         NRTS_IGE   = IPARI(75,NI)
         NRTS_FE    = IPARI(76,NI)
         NSN_IGE    = IPARI(77,NI)
         NSN_FE     = IPARI(78,NI)
         NMN_IGE    = IPARI(79,NI)
         NMN_FE     = IPARI(80,NI)
        ELSE
         NRTM_IGE   = 0
         NRTM_FE    = NRTM
         NRTS_IGE   = 0
         NRTS_FE    = NRTS
         NSN_IGE    = 0
         NSN_FE     = NSN
         NMN_IGE    = 0
         NMN_FE     = NMN
        ENDIF
        TYPE18=.FALSE.
        IF(NTYP==7 .AND. INACTI==7)TYPE18=.TRUE.
C-------------------------------------------
        ID=NOM_OPT(1,NI)
        CALL FRETITL2(TITR,
     .                NOM_OPT(LNOPT1-LTITR+1,NI),LTITR)
C-------------------------------------------
        INTBUF_TAB(NI)%STFAC(1) = STFAC(NI)
        DO J = 1,NPARIR
          INTBUF_TAB(NI)%VARIABLES(J) = FRIGAP(J,NI)
        ENDDO
C-----
C      LECTURE DES RENSEIGNEMENTS SURFACE SECOND/MAIN
C-----
C      1) SURFACE SECONDARY
C
        IF ( NTYP == 15 ) THEN
          IF(NRTS == 0) THEN
             CALL ANCMSG(MSGID=118, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=ID, C1=TITR)
          ENDIF
          IF(IPRI>=1) WRITE(IOUT,'(//,A)')' SECONDARY SURFACE'
          SURF_NODES => IGRSURF(ISU1)%NODES(1:NRTS,1:4)
          CALL INSURF(NRTS,NSN,IRS,INTBUF_TAB(NI)%IRECTS,NOINT,
     .                SURF_NODES,ITAB,INTBUF_TAB(NI)%NSV,ID,TITR,
     .                NTAG,INTBUF_TAB(NI)%S_NSV,INTBUF_TAB(NI)%S_IRECTS,X,TYPE18)
        ELSEIF (NTYP == 17) THEN
        ELSEIF (NTYP == 20) THEN
        ELSEIF (NTYP == 22) THEN
            IF(ISU1>0)THEN
              IF(IPRI>=1) WRITE(IOUT,'(//,A)')' SECONDARY SURFACE'
              NBRIC=IGRBRIC(ISU1)%NENTITY
              CALL INGRBRIC(NSN,   NOINT,   IGRBRIC(ISU1)%ENTITY,   ITAB,  INTBUF_TAB(NI)%NSV,
     +                      IXS,   NBRIC,   PM, INTBUF_TAB(NI)%S_NSV,IGEO)
           ENDIF
C--------------------------------------------
        ELSEIF(NTYP == 23) THEN
          IF(IS1/=0) THEN
            IF(NRTS == 0.AND.IS1/=2) THEN
              CALL ANCMSG(MSGID=118, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=ID, C1=TITR)
            ENDIF
            IF(IPRI>=1) WRITE(IOUT,'(//,A)')' SECONDARY SURFACE'
            IF(IS1 == 1)THEN
              NRTS=IGRSURF(ISU1)%NSEG
              SURF_NODES => IGRSURF(ISU1)%NODES(1:NRTS,1:4)
              CALL INSURF23(NRTS,NSN,IRS,INTBUF_TAB(NI)%IRECTS,NOINT,
     .                      SURF_NODES,ITAB,INTBUF_TAB(NI)%NSV,NRTS_NEW,X,
     .                      NTAG)
              NRTS=NRTS_NEW
            ENDIF
          ENDIF
        ELSEIF (NTYP == 24) THEN
c            IF (IS1 == -2)THEN
c              IAD=IGRN(3,ISU1)
c              CALL INPOINT(NSN,NOINT,IBUFSSG(IAD),ITAB,INTBUF_TAB(NI)%NSV)
c            END IF
        ELSEIF (NTYP == 25) THEN
        ELSE
          IF(IS1/=0) THEN
            IF(NRTS == 0.AND.IS1/=2.AND.IS1/=5) THEN
              CALL ANCMSG(MSGID=118, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=ID, C1=TITR)
            ENDIF
            IF(IPRI>=1) WRITE(IOUT,'(//,A)')' SECONDARY SURFACE'
            IF(IS1 == 1)THEN
              IF (NTYP == 3 .OR. NTYP == 6) IRS = IPARI(25,NI)
              SURF_NODES => IGRSURF(ISU1)%NODES(1:NRTS_FE,1:4)
              CALL INSURF(NRTS_FE,NSN_FE,IRS,INTBUF_TAB(NI)%IRECTS,NOINT,
     .                   SURF_NODES,ITAB,INTBUF_TAB(NI)%NSV,ID,TITR,
     .                   NTAG,INTBUF_TAB(NI)%S_NSV,INTBUF_TAB(NI)%S_IRECTS,X,TYPE18)
              IF (IGRSURF(ISU1)%NSEG_IGE >= 1) THEN
                SURF_NODES_IGE => IGRSURF(ISU1)%NODES_IGE(1:NRTS_IGE,1:4)
                IAD_IGE = IGRSURF(ISU1)%IAD_IGE
                CALL INSURFIGEO(NRTS_IGE,NRTS_FE,NSN_IGE,0,IAD_IGE,IRS,
     .                         INTBUF_TAB(NI)%IRECTS,NOINT,
     .                         SURF_NODES_IGE,ITAB,INTBUF_TAB(NI)%NSV,ID,TITR,INTBUF_TAB(NI)%NIGE,
     .                         INTBUF_TAB(NI)%RIGE,INTBUF_TAB(NI)%XIGE,INTBUF_TAB(NI)%VIGE,
     .                         NIGE,RIGE,XIGE,VIGE)
              ENDIF
            ELSEIF(IS1 == 2)THEN
              CALL INPOINT(NSN,NOINT,IGRNOD(ISU1)%ENTITY,ITAB,INTBUF_TAB(NI)%NSV)
            ELSEIF(IS1 == 3)THEN
              LINE_NODES = > IGRSLIN(ISU1)%NODES(1:NRTS,1:2)
              CALL INSLIN(NRTS,NSN,INTBUF_TAB(NI)%IRECTS,NOINT,
     .                    LINE_NODES,ITAB,INTBUF_TAB(NI)%NSV,
     .                    NTAG)
            ELSEIF(IS1 == 5) THEN
              !INTER18 + GRBRIC_ID
              NBRIC = IGRBRIC(GRBRIC_ID)%NENTITY              
              IF (MULTI_FVM%IS_USED)THEN
               CALL INGRBRIC_CENTROIDS(NOINT, IGRBRIC(GRBRIC_ID)%ENTITY, ITAB, INTBUF_TAB(NI)%NSV,
     .                       IXS, NBRIC)                            
              ELSE
               CALL INGRBRIC_NODES(    NSN_FE, NOINT, IGRBRIC(GRBRIC_ID)%ENTITY, ITAB, INTBUF_TAB(NI)%NSV,
     .                       IXS, NBRIC, 2, NALE, IPM, BUFMAT, INTBUF_TAB(NI)%S_NSV)              
              ENDIF
            ENDIF
            IF (NTYP==3) THEN
              CALL ELTY_SURF(0,NRTS,IGRSURF(ISU1)%NSEG,IGRSURF(ISU1)%ELTYP,
     .                       INTBUF_TAB(NI)%MSEGTYP,ISU1)
            ENDIF
          ENDIF
          NSN = NSN_FE+NSN_IGE
        ENDIF
C
C----------------------------------------------
C      2)SURFACE MAIN :
C-----
        IF ( NTYP == 15 ) THEN
          IF(NRTM == 0) THEN
             CALL ANCMSG(MSGID=119, MSGTYPE=MSGERROR, ANMODE=ANINFO,I1=ID,C1=TITR)
          ENDIF
          IF(IPRI>=1) WRITE(IOUT,'(//,A,I10)')
     .    ' MAIN SURFACE IS THE HYPER-ELLIPSOIDAL SURFACE :',
     .      IGRSURF(ISU2)%ID
          INTBUF_TAB(NI)%KSURF(1)=ISU2
        ELSEIF ( NTYP == 14 ) THEN
          IF(NRTM == 0) THEN
             CALL ANCMSG(MSGID=119, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=ID,C1=TITR)
          ENDIF
          IF(IPRI>=1) WRITE(IOUT,'(//,A,I10)')
     .    ' MAIN SURFACE IS THE HYPER-ELLIPSOIDAL SURFACE :',
     .      IGRSURF(ISU2)%ID
          INTBUF_TAB(NI)%KSURF(1)=ISU2
        ELSEIF (NTYP == 20) THEN
C       
C--------------------------------------------
        ELSEIF( NTYP == 23) THEN
          IF(IS2/=0) THEN
            IF(NRTM == 0) THEN
              CALL ANCMSG(MSGID=119, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=ID, C1=TITR)
            ENDIF
            IF(IPRI>=1) WRITE(IOUT,'(//,A)')' MAIN SURFACE'
            IF(IS2 == 1)THEN
              NRTM=IGRSURF(ISU2)%NSEG
              SURF_NODES => IGRSURF(ISU2)%NODES(1:NRTM,1:4)
              CALL INSURF23(NRTM,NMN,IRM,INTBUF_TAB(NI)%IRECTM,NOINT,
     .                      SURF_NODES,ITAB,INTBUF_TAB(NI)%MSR,NRTM_NEW,X,
     .                      NTAG)
              NRTM   = NRTM_NEW
            ENDIF
            CALL ELTY_SURF(0,NRTM,IGRSURF(ISU2)%NSEG,IGRSURF(ISU2)%ELTYP,
     .                     INTBUF_TAB(NI)%MSEGTYP,ISU2)
          ENDIF
C--------------------------------------------
        ELSEIF (NTYP == 24) THEN
          NRTM_SH   = IPARI(42,NI)
          NRTM0   = NRTM -NRTM_SH
          IF (IS2/=0) THEN
            IF(NRTM0 == 0) THEN
              CALL ANCMSG(MSGID=119, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=ID, C1=TITR)
            ENDIF
            IF(IPRI>=1) WRITE(IOUT,'(//,A)')' MAIN SURFACE'
            IF(IS2 == -1)THEN
              NRTM0 = IGRSURF(ISU2)%NSEG
              SURF_NODES => IGRSURF(ISU2)%NODES(1:NRTM0,1:4)
              CALL INSURF(NRTM0,NMN,IRM,INTBUF_TAB(NI)%IRECTM,NOINT,
     .                    SURF_NODES,ITAB,INTBUF_TAB(NI)%MSR,ID,TITR,
     .                    NTAG,INTBUF_TAB(NI)%S_MSR,INTBUF_TAB(NI)%S_IRECTM,X,TYPE18)
            ENDIF
            IF(IS1/=-2)THEN
              NRTM0 = IGRSURF(ISU1)%NSEG
              SURF_NODES => IGRSURF(ISU1)%NODES(1:NRTM0,1:4)
              CALL INSURF(NRTM0,NMN,IRM,INTBUF_TAB(NI)%IRECTM,NOINT,
     .                    SURF_NODES,ITAB,INTBUF_TAB(NI)%MSR,ID,TITR,
     .                    NTAG,INTBUF_TAB(NI)%S_MSR,INTBUF_TAB(NI)%S_IRECTM,X,TYPE18)
            ENDIF
          ENDIF
        ELSEIF (NTYP == 25) THEN
        ELSE
          IF (NTYP == 3 .OR. NTYP == 5 .OR. NTYP == 6 .OR. NTYP == 8)
     .        IRM = IPARI(24,NI)
          IF (IS2/=0) THEN
            IF(NRTM_FE == 0 .AND. NRTM_IGE == 0) THEN
              CALL ANCMSG(MSGID=119, MSGTYPE=MSGERROR,  ANMODE=ANINFO, I1=ID, C1=TITR)
            ENDIF
            IF(IPRI>=1) WRITE(IOUT,'(//,A)')' MAIN SURFACE'
            IF(IS2 == 1)THEN
              SURF_NODES => IGRSURF(ISU2)%NODES(1:NRTM_FE,1:4)
              CALL INSURF(NRTM_FE,NMN_FE,IRM,INTBUF_TAB(NI)%IRECTM,NOINT,
     .                    SURF_NODES,ITAB,INTBUF_TAB(NI)%MSR,ID,TITR,
     .                    NTAG,INTBUF_TAB(NI)%S_MSR,INTBUF_TAB(NI)%S_IRECTM,X,TYPE18)
              IF (IGRSURF(ISU2)%NSEG_IGE >= 1) THEN
                SURF_NODES_IGE => IGRSURF(ISU2)%NODES_IGE(1:NRTM_IGE,1:4)
                IAD_IGE = IGRSURF(ISU2)%IAD_IGE
                CALL INSURFIGEO(NRTM_IGE,NRTM_FE,NMN_IGE,NSN_IGE,IAD_IGE,IRM,INTBUF_TAB(NI)%IRECTM,NOINT,
     .                     SURF_NODES_IGE,
     .                     ITAB,INTBUF_TAB(NI)%MSR,ID,TITR,INTBUF_TAB(NI)%NIGE(NSN_IGE+1),
     .                     INTBUF_TAB(NI)%RIGE(3*NSN_IGE+1),INTBUF_TAB(NI)%XIGE(3*NSN_IGE+1),INTBUF_TAB(NI)%VIGE(3*NSN_IGE+1),
     .                     NIGE,RIGE,XIGE,VIGE)
              ENDIF
            ELSEIF(IS2 == 3)THEN
              LINE_NODES => IGRSLIN(ISU2)%NODES(1:NRTM,1:2)
              CALL INSLIN(NRTM,NMN,INTBUF_TAB(NI)%IRECTM,NOINT,
     .                    LINE_NODES,ITAB,INTBUF_TAB(NI)%MSR,
     .                    NTAG)
            ELSEIF(IS2 == 4) THEN
              SURF_NODES => IGRSURF(ISU2)%NODES(1:NRTM,1:4)
              CALL INSURF(NRTM,NMN,IRM,INTBUF_TAB(NI)%IRECTM,NOINT,
     .                    SURF_NODES,ITAB,INTBUF_TAB(NI)%MSR,ID,TITR,
     .                    NTAG,INTBUF_TAB(NI)%S_MSR,INTBUF_TAB(NI)%S_IRECTM,X,TYPE18)
            ENDIF
            IF (NTYP==3 ) THEN
             CALL ELTY_SURF(NRTS,NRTM,IGRSURF(ISU2)%NSEG,IGRSURF(ISU2)%ELTYP,
     .                      INTBUF_TAB(NI)%MSEGTYP,ISU2)
            ELSEIF (NTYP==2 .OR. NTYP==5 .OR. NTYP==7 .OR. 
     +         NTYP==10 .OR. NTYP==21) THEN
             CALL ELTY_SURF(0,NRTM,IGRSURF(ISU2)%NSEG,IGRSURF(ISU2)%ELTYP,
     .                      INTBUF_TAB(NI)%MSEGTYP,ISU2)
            END IF
            NMN = NMN_FE+NMN_IGE
          ENDIF
C
        ENDIF
C--------------------------
        IF (NTYP == 1) THEN
C-------
          CALL NSEGMT(
     .         IPARI(9,NI),INTBUF_TAB(NI)%IRECTM,INTBUF_TAB(NI)%MSR,
     .         INTBUF_TAB(NI)%NSEGM,NMT,NRTM,NMN)
C-------
        ELSEIF (NTYP == 2) THEN
          LIXINT = LIXINT+6*NSN
          IF (ILEV == 20 .OR. ILEV == 21 .OR. ILEV == 22.OR.INTTH >0) THEN
           DO J = 1,NSN
            INTBUF_TAB(NI)%AREAS2(J) = AREASL(NI) 
           ENDDO
          ENDIF
          IF (ILEV == 20 .OR. ILEV == 21 .OR. ILEV == 22) THEN

            INTBUF_TAB(NI)%RUPT(1)   = I2RUPT(1,NI) 
            INTBUF_TAB(NI)%RUPT(2)   = I2RUPT(2,NI) 
            INTBUF_TAB(NI)%RUPT(3)   = I2RUPT(3,NI) 
            INTBUF_TAB(NI)%RUPT(4)   = I2RUPT(4,NI)
            INTBUF_TAB(NI)%RUPT(5)   = I2RUPT(5,NI)
            INTBUF_TAB(NI)%RUPT(6)   = I2RUPT(6,NI)
          ENDIF
C-------
        ELSEIF (NTYP == 3 .OR. NTYP == 4 .OR. NTYP == 9) THEN
          IF(IPARI(30,NI)/=0) THEN
            INTBUF_TAB(NI)%FRIC_P(1) = FRIC_P(1,NI)
            INTBUF_TAB(NI)%FRIC_P(2) = FRIC_P(2,NI)
            INTBUF_TAB(NI)%FRIC_P(3) = FRIC_P(3,NI)
            INTBUF_TAB(NI)%FRIC_P(4) = FRIC_P(4,NI)
            INTBUF_TAB(NI)%FRIC_P(5) = FRIC_P(5,NI)
            INTBUF_TAB(NI)%FRIC_P(6) = FRIC_P(6,NI)
          ENDIF
          IF(IPARI(31,NI)/=0) THEN
            INTBUF_TAB(NI)%XFILTR(1) = XFILTR(NI)
          ENDIF
          CALL NSEGMT(
     .         IPARI(8,NI),INTBUF_TAB(NI)%IRECTS,INTBUF_TAB(NI)%NSV,INTBUF_TAB(NI)%NSEGS,
     .         NST,NRTS,NSN)
          CALL NSEGMT(
     .         IPARI(9,NI),INTBUF_TAB(NI)%IRECTM,INTBUF_TAB(NI)%MSR,INTBUF_TAB(NI)%NSEGM,
     .         NMT,NRTM,NMN)
C-------
        ELSEIF (NTYP == 5) THEN
C-------
          IF (IPARI(30,NI)/=0) THEN
            INTBUF_TAB(NI)%FRIC_P(1)  =FRIC_P(1,NI)
            INTBUF_TAB(NI)%FRIC_P(2) = FRIC_P(2,NI)
            INTBUF_TAB(NI)%FRIC_P(3) = FRIC_P(3,NI)
            INTBUF_TAB(NI)%FRIC_P(4) = FRIC_P(4,NI)
            INTBUF_TAB(NI)%FRIC_P(5) = FRIC_P(5,NI)
            INTBUF_TAB(NI)%FRIC_P(6) = FRIC_P(6,NI)
          ENDIF
          IF(IPARI(31,NI)/=0) THEN
            INTBUF_TAB(NI)%XFILTR(1) = XFILTR(NI)
          ENDIF
          CALL NSEGMT(
     .         IPARI(8,NI),INTBUF_TAB(NI)%IRECTS,INTBUF_TAB(NI)%NSV,INTBUF_TAB(NI)%NSEGS,
     .         NST,NRTS,NSN)
          CALL NSEGMT(
     .         IPARI(9,NI),INTBUF_TAB(NI)%IRECTM,INTBUF_TAB(NI)%MSR,INTBUF_TAB(NI)%NSEGM,
     .         NMT,NRTM,NMN)
C-------
        ELSEIF (NTYP == 6) THEN
          CALL NSEGMT(
     .         IPARI(8,NI),INTBUF_TAB(NI)%IRECTS,INTBUF_TAB(NI)%NSV,INTBUF_TAB(NI)%NSEGS,
     .         NST,NRTS,NSN)
          CALL NSEGMT(
     .         IPARI(9,NI),INTBUF_TAB(NI)%IRECTM,INTBUF_TAB(NI)%MSR,INTBUF_TAB(NI)%NSEGM,
     .         NMT,NRTM,NMN)
C-------
        ELSEIF (NTYP == 7) THEN
          IF (IPARI(30,NI)/=0) THEN
             INTBUF_TAB(NI)%FRIC_P(1) = FRIC_P(1,NI)
             INTBUF_TAB(NI)%FRIC_P(2) = FRIC_P(2,NI)
             INTBUF_TAB(NI)%FRIC_P(3) = FRIC_P(3,NI)
             INTBUF_TAB(NI)%FRIC_P(4) = FRIC_P(4,NI)
             INTBUF_TAB(NI)%FRIC_P(5) = FRIC_P(5,NI)
             INTBUF_TAB(NI)%FRIC_P(6) = FRIC_P(6,NI)
          ENDIF
          IF(IPARI(31,NI)/=0) THEN
             INTBUF_TAB(NI)%XFILTR(1) = XFILTR(NI)
          ENDIF
          LIXINT = LIXINT+6*MULTIMP*(NSN_IGE+NSN_FE)
C-------
        ELSEIF (NTYP == 8) THEN
C-------
          CALL NSEGMT(
     .         IPARI(9,NI),INTBUF_TAB(NI)%IRECTM,INTBUF_TAB(NI)%MSR,INTBUF_TAB(NI)%NSEGM,
     .         NMT,NRTM,NMN)
C-------
        ELSEIF (NTYP == 11) THEN
          CALL I11EDGE( IPARI(1,NI),INTBUF_TAB(NI) ) 
C-------
        ELSEIF (NTYP == 12) THEN
          CALL NSEGMT(
     .         IPARI(8,NI),INTBUF_TAB(NI)%IRECTS,INTBUF_TAB(NI)%NSV,INTBUF_TAB(NI)%NSEGS,
     .         NST,NRTS,NSN)
          CALL NSEGMT(
     .         IPARI(9,NI),INTBUF_TAB(NI)%IRECTM,INTBUF_TAB(NI)%MSR,INTBUF_TAB(NI)%NSEGM,
     .         NMT,NRTM,NMN)
C-------
        ELSEIF (NTYP == 20) THEN
          IF (IPARI(30,NI)/=0) THEN
             INTBUF_TAB(NI)%FRIC_P(1) = FRIC_P(1,NI)
             INTBUF_TAB(NI)%FRIC_P(2) = FRIC_P(2,NI)
             INTBUF_TAB(NI)%FRIC_P(3) = FRIC_P(3,NI)
             INTBUF_TAB(NI)%FRIC_P(4) = FRIC_P(4,NI)
             INTBUF_TAB(NI)%FRIC_P(5) = FRIC_P(5,NI)
             INTBUF_TAB(NI)%FRIC_P(6) = FRIC_P(6,NI)
          ENDIF
          IF(IPARI(31,NI)/=0) THEN
            INTBUF_TAB(NI)%XFILTR(1) = XFILTR(NI)
          ENDIF
          LIXINT = LIXINT+6*MULTIMP*NSN
          IALLO = 2  ! initialisation des tableaux
          CALL I20SURFI(
     1      IALLO        ,IPARI(1,NI)  ,IGRNOD     ,IGRSURF  ,
     2      IGRSLIN      ,INTBUF_TAB(NI)%IRECTM,FRIGAP(1,NI) ,
     3      INTBUF_TAB(NI)%NSV,INTBUF_TAB(NI)%MSR,INTBUF_TAB(NI)%IXLINS,INTBUF_TAB(NI)%IXLINM,
     4      INTBUF_TAB(NI)%NSVL,INTBUF_TAB(NI)%MSRL,ITAB         ,INTBUF_TAB(NI)%ISLINS,
     5      INTBUF_TAB(NI)%ISLINM,INTBUF_TAB(NI)%NLG,X            ,INTBUF_TAB(NI)%NBINFLG,
     5      INTBUF_TAB(NI)%MBINFLG)

C-------
        ELSEIF (NTYP == 21) THEN
          IF (IPARI(30,NI)/=0) THEN
             INTBUF_TAB(NI)%FROT_P(1) = FRIC_P(1,NI)
             INTBUF_TAB(NI)%FROT_P(2) = FRIC_P(2,NI)
             INTBUF_TAB(NI)%FROT_P(3) = FRIC_P(3,NI)
             INTBUF_TAB(NI)%FROT_P(4) = FRIC_P(4,NI)
             INTBUF_TAB(NI)%FROT_P(5) = FRIC_P(5,NI)
             INTBUF_TAB(NI)%FROT_P(6) = FRIC_P(6,NI)
          ENDIF
          IF(IPARI(31,NI)/=0) THEN
            INTBUF_TAB(NI)%ALPHA0(1) = XFILTR(NI)
          ENDIF
          LIXINT = MAX(LIXINT,6*MULTIMP*NSN)
C-------
        ELSEIF (NTYP == 22) THEN
          IF (IPARI(30,NI)/=0) THEN
             INTBUF_TAB(NI)%FRIC_P(1) = FRIC_P(1,NI)
             INTBUF_TAB(NI)%FRIC_P(2) = FRIC_P(2,NI)
             INTBUF_TAB(NI)%FRIC_P(3) = FRIC_P(3,NI)
             INTBUF_TAB(NI)%FRIC_P(4) = FRIC_P(4,NI)
             INTBUF_TAB(NI)%FRIC_P(5) = FRIC_P(5,NI)
             INTBUF_TAB(NI)%FRIC_P(6) = FRIC_P(6,NI)
          ENDIF
          IF(IPARI(31,NI)/=0) THEN
            INTBUF_TAB(NI)%XFILTR(1) = XFILTR(NI)
          ENDIF
          LIXINT = LIXINT+6*MULTIMP*NSN
C-------
        ELSEIF (NTYP == 23) THEN
          IF (IPARI(30,NI)/=0) THEN
             INTBUF_TAB(NI)%FRIC_P(1) = FRIC_P(1,NI)
             INTBUF_TAB(NI)%FRIC_P(2) = FRIC_P(2,NI)
             INTBUF_TAB(NI)%FRIC_P(3) = FRIC_P(3,NI)
             INTBUF_TAB(NI)%FRIC_P(4) = FRIC_P(4,NI)
             INTBUF_TAB(NI)%FRIC_P(5) = FRIC_P(5,NI)
             INTBUF_TAB(NI)%FRIC_P(6) = FRIC_P(6,NI)
          ENDIF
          IF(IPARI(31,NI)/=0) THEN
            INTBUF_TAB(NI)%XFILTR(1) = XFILTR(NI)
          ENDIF
          LIXINT = MAX(LIXINT,6*MULTIMP*NSN)
C-------
        ELSEIF (NTYP == 24) THEN
          IF (IPARI(30,NI)/=0) THEN
             INTBUF_TAB(NI)%FRIC_P(1) = FRIC_P(1,NI)
             INTBUF_TAB(NI)%FRIC_P(2) = FRIC_P(2,NI)
             INTBUF_TAB(NI)%FRIC_P(3) = FRIC_P(3,NI)
             INTBUF_TAB(NI)%FRIC_P(4) = FRIC_P(4,NI)
             INTBUF_TAB(NI)%FRIC_P(5) = FRIC_P(5,NI)
             INTBUF_TAB(NI)%FRIC_P(6) = FRIC_P(6,NI)
          ENDIF
          IF(IPARI(31,NI)/=0) THEN
            INTBUF_TAB(NI)%XFILTR(1) = XFILTR(NI)
          ENDIF
          LIXINT = LIXINT+6*MULTIMP*NSN
C-----------inside I24SURFI NRTM=NRTM0          
          IALLO = 2  ! initialisation des tableaux
          CALL I24SURFI(
     1      IALLO                  , IPARI(1,NI)            ,IGRNOD                   ,IGRSURF              ,
     2      INTBUF_TAB(NI)%IRECTM  ,FRIGAP(1,NI)            ,
     3      INTBUF_TAB(NI)%NSV     , INTBUF_TAB(NI)%MSR     ,ITAB                     ,X                    ,
     4      INTBUF_TAB(NI)%NBINFLG , INTBUF_TAB(NI)%MBINFLG ,INTBUF_TAB(NI)%MSEGTYP24 ,INTBUF_TAB(NI)%ISEADD,
     5      INTBUF_TAB(NI)%ISEDGE  , IBID                   ,IBID                     ,IXC                  ,
     6      IXTG                   , KNOD2ELC               ,KNOD2ELTG                ,NOD2ELC              ,
     7      NOD2ELTG               , KNOD2ELS               ,NOD2ELS                  ,IXS                  ,
     8      IXS10                  ,IXS16                   ,IXS20                    ,INTBUF_TAB(NI)%IRTSE ,
     9      INTBUF_TAB(NI)%IS2SE   ,INTBUF_TAB(NI)%IS2PT    ,INTBUF_TAB(NI)%IS2ID     ,INTNITSCHE           )
C------initialization of doubler M_seg pour shells ---> move inside I24SURFI
C-------
        ELSEIF (NTYP == 25) THEN
          IF (IPARI(30,NI)/=0) THEN
             INTBUF_TAB(NI)%FRIC_P(1) = FRIC_P(1,NI)
             INTBUF_TAB(NI)%FRIC_P(2) = FRIC_P(2,NI)
             INTBUF_TAB(NI)%FRIC_P(3) = FRIC_P(3,NI)
             INTBUF_TAB(NI)%FRIC_P(4) = FRIC_P(4,NI)
             INTBUF_TAB(NI)%FRIC_P(5) = FRIC_P(5,NI)
             INTBUF_TAB(NI)%FRIC_P(6) = FRIC_P(6,NI)
          ENDIF
          IF(IPARI(31,NI)/=0) THEN
            INTBUF_TAB(NI)%XFILTR(1) = XFILTR(NI)
          ENDIF
          LIXINT = LIXINT+6*MULTIMP*NSN
C-----------inside I24SURFI NRTM=NRTM0          
          IALLO = 2  ! initialisation des tableaux
          IF(ILEV==2)THEN
C           necessaire au 2eme passage (cf IDDLEVEL)
            INTBUF_TAB(NI)%NBINFLG(1:INTBUF_TAB(NI)%S_NBINFLG)=0
            INTBUF_TAB(NI)%MBINFLG(1:INTBUF_TAB(NI)%S_MBINFLG)=0
          END IF
          CALL I25SURFI(
     1      IALLO                  ,IPARI(1,NI)            ,IGRNOD                   , IGRSURF               ,
     2      INTBUF_TAB(NI)%IRECTM  , FRIGAP(1,NI)          ,
     3      INTBUF_TAB(NI)%NSV     ,INTBUF_TAB(NI)%MSR     ,ITAB                     , X                     ,
     4      INTBUF_TAB(NI)%NBINFLG ,INTBUF_TAB(NI)%MBINFLG ,INTBUF_TAB(NI)%MSEGTYP24 , INTBUF_TAB(NI)%ISEADD ,
     5      INTBUF_TAB(NI)%ISEDGE  ,IBID                   ,IBID                     ,IXC                    ,
     6      IXTG                   ,KNOD2ELC               ,KNOD2ELTG                ,NOD2ELC                ,
     7      NOD2ELTG               ,KNOD2ELS               ,NOD2ELS                  ,IXS                    ,
     8      IXS10                  ,IXS16                  ,IXS20                    ,INTBUF_TAB(NI)%IRTSE   ,
     9      INTBUF_TAB(NI)%IS2SE   ,INTBUF_TAB(NI)%IS2PT   ,INTBUF_TAB(NI)%IS2ID     )
C------initialization of doubler M_seg pour shells ---> move inside I24SURFI
        ENDIF
C
        DO J = 1,NPARIR
          INTBUF_TAB(NI)%VARIABLES(J) = FRIGAP(J,NI)
        ENDDO
C-----
      ENDDO
C-----
       NULLIFY(NTAG)
       DEALLOCATE(NTAG_TARGET)

      RETURN
      END
Chd|====================================================================
Chd|  ELTY_SURF                     source/interfaces/interf1/lecins.F
Chd|-- called by -----------
Chd|        LECINS                        source/interfaces/interf1/lecins.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ELTY_SURF(NRTM0,NRTM,NSEG,SURF_ELTYP,MSEGTYP,ISU2)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM0,NRTM,NSEG,ISU2
      INTEGER MSEGTYP(*),SURF_ELTYP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, L
C=======================================================================
        DO I=1+NRTM0,NRTM+NRTM0
          MSEGTYP(I) = 0
        ENDDO
        L = NRTM0
        IF(ISU2 /= 0)THEN
          DO J=1,NSEG
            L = L+1
            MSEGTYP(L) = SURF_ELTYP(J)
C---------input by seg will not be avoided degenerate s10
            IF (MSEGTYP(L)==0) MSEGTYP(L)=10
          ENDDO
        ENDIF
C------------------------------------------------------------
      RETURN
      END

