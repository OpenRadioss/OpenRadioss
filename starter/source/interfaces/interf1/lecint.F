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
Chd|  LECINT                        source/interfaces/interf1/lecint.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        I20SURFI                      source/interfaces/inter3d1/i20surfi.F
Chd|        I24SURFI                      source/interfaces/inter3d1/i24surfi.F
Chd|        I25SURFI                      source/interfaces/inter3d1/i25surfi.F
Chd|        INGRBRIC                      source/interfaces/interf1/ingrbric.F
Chd|        INGRBRIC_DX                   source/interfaces/interf1/ingrbric_dx.F
Chd|        INGRBRIC_NODES                source/interfaces/interf1/ingrbric_nodes.F
Chd|        INPOINT                       source/interfaces/interf1/inpoint.F
Chd|        INSLIN                        source/interfaces/interf1/inslin.F
Chd|        INSURF                        source/interfaces/interf1/insurf.F
Chd|        INSURF23                      source/interfaces/interf1/insurf23.F
Chd|        INSURFIGEO                    source/interfaces/interf1/insurfigeo.F
Chd|        INSURF_DX                     source/interfaces/interf1/insurf_dx.F
Chd|        PRESEGMT                      source/interfaces/interf1/presegmt.F
Chd|        NINTRI                        source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|====================================================================
      SUBROUTINE LECINT(IPARI   ,LINTER  ,IPM     ,BUFMAT  ,
     .                  NMNT    ,ITAB    ,ITABM1  ,GEO     ,
     .                  PM      ,X       ,IGRNOD  ,IGRSURF ,IGRSLIN ,
     .                  NPC     ,PROBINT ,LAG_NCF ,
     .                  LAG_NKF ,LAG_NCL ,LAG_NKL ,LAG_NHF ,MAXRTM  ,
     .                  ISKN    ,MAXRTMS ,IGEO    ,
     .                  XFILTR  ,STFAC   ,FRIC_P  ,FRIGAP  ,
     .                  I2RUPT  ,AREASL  ,UNITAB  ,IXS     ,NOM_OPT ,
     .                  ITAG    ,IXC     ,IXTG    ,KNOD2ELC,KNOD2ELTG,
     .                  NOD2ELC ,NOD2ELTG,KNOD2ELS,NOD2ELS,IXS10   ,
     .                  IXS16   ,IXS20   ,DEF_INTER,MAXNSNE,
     .                  NPC1    ,LSENSOR, MULTI_FVM,NOM_OPTFRIC,INTBUF_FRIC_TAB,
     .                  IGRBRIC,IGRSH3N ,IGRTRUSS ,MAXRTM_T2,NSN_MULTI_CONNEC,T2_NB_CONNEC,
     .                  IDDLEVEL,NALE)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE R2R_MOD
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
      USE INTBUF_FRIC_MOD
      USE GROUPDEF_MOD
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
#include      "r2r_c.inc"
#include      "inter22.inc"
#include      "ige3d_c.inc"
#include      "inter18.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN)::IDDLEVEL
      INTEGER NOM_OPT(LNOPT1,*),MAXNSNE
      INTEGER LINTER,NMNT, LAG_NCF,LAG_NKF,
     .        LAG_NCL,LAG_NKL,LAG_NHF,MAXRTM,MAXRTMS, NBRIC
      INTEGER IPARI(NPARI,NINTER), ITAB(NUMNOD), ITABM1(*),
     .        NPC(*),ISKN(*),
     .        IGEO(NPROPGI,NUMGEO),IXS(NIXS,NUMELS),ITAG(*),
     .        IXC(*),IXTG(*),KNOD2ELC(*),KNOD2ELTG(*),
     .        NOD2ELC(*),NOD2ELTG(*),KNOD2ELS(*),NOD2ELS(*),
     .        IXS10(6,*), IXS16(8,*), IXS20(12,*),DEF_INTER(*),
     .        NPC1(*),LSENSOR(NSENSI,*),NOM_OPTFRIC(LNOPT1,*),MAXRTM_T2,
     .        T2_NB_CONNEC(*),NSN_MULTI_CONNEC
      INTEGER, INTENT(IN) :: NALE(NUMNOD)
      my_real
     .   GEO(NPROPG,NUMGEO), PM(*), XFILTR(*),STFAC(*),
     .   FRIC_P(10,*),I2RUPT(6,*),FRIGAP(NPARIR,*),AREASL(*)
      my_real
     .   PROBINT
      my_real
     .   X(3,NUMNOD)
      TYPE(MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
      INTEGER,INTENT(IN) :: IPM(NPROPMI,NUMMAT)
      my_real,INTENT(IN) :: BUFMAT(SBUFMAT)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (SURF_)   , DIMENSION(NSURF) ,TARGET   :: IGRSURF
      TYPE (SURF_)   , DIMENSION(NSLIN) ,TARGET   :: IGRSLIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,L,IRS,IRM,NI,NSN,NMN,
     .   NTYP,IS1,IS2,NOINT,NRTS,NRTM,IBUC,ILEV, 
     .   MULTIMP,IGAP,INACTI,NME,LAG_NC16,LAG_NK16,
     .   ILAGM,NCF_I2,NUVAR,
     .   NIN,NISUB,JSUB,IGR,ISU,ISU1,ISU2,PID,STAT,
     .   NRTMS,NRTMM,IALLO,NLINSA,NLINMA,NSNE,NLN,
     .   NRTS_NEW, NRTM_NEW,NRTM_FE,
     .   NRTM_IGE,NRTMM_IGE,
     .   NRTS_IGE,NRTMS_IGE,NRTS_FE,
     .   NMN_IGE,NMN_FE,NSN_IGE,NSN_FE,IAD_IGE,
     .   IEDGE,NCONTE,MULTIMPE,MULTIMPS,ISTIFF
      INTEGER KD(50),JD(50),IBID,NRTM_SH,ETYP,INTPLY
      INTEGER, DIMENSION(:), ALLOCATABLE :: 
     .   IRECTS,IRECTM,NSV,MSR
      INTEGER ID,ISL, GRBRIC_ID
      CHARACTER*nchartitle,
     .   TITR
      my_real
     .   RBID,RHO0_MAX,AUTO_RHO,AUTO_LENGTH, STIFF_STAT(3)

      INTEGER, DIMENSION(:), ALLOCATABLE, TARGET ::  NTAG_TARGET
      INTEGER, DIMENSION(:), POINTER :: NTAG
      INTEGER, DIMENSION(:,:), POINTER :: SURF_NODES,SURF_NODES_IGE,LINE_NODES
      LOGICAL IS_GAP_COMPUTED,TYPE18
      INTEGER S_MSR,S_NSV,S_IRECTS,S_IRECTM
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NINTRI
C
      CHARACTER MESS*40
      DATA MESS/'INTERFACE INPUT                         '/
C--------------------------------------------
C     ORGANISATION DES BUFFERS D'INTERFACES
C                    sauf Interface TYPE 14 (doc/doc_generale/int14.doc).
C                    et   Interface TYPE 15 (doc/doc_generale/int15.doc).
C--------------------------------------------
C     IPARI(NPARI,NINTER) ::  PARAMETER BUFFER
C--------------------------------------------
C  1 :JINBUF:ADRESSE DU BUFFER ENTIER
C  2 :JBUFIN:ADRESSE DU BUFFER REEL
C  3 :NRTS  :NBRE DE FACES SECONDS
C  4 :NRTM  :NBRE DE FACES MAIN
C  5 :NSN   :NBRE DE POINTS SECONDS
C  6 :NMN   :NBRE DE POINTS MAINS
C  7 :NTY   :TYPE D'INTERFACE
C  8 :NST   :LGR VECT FACES VOISINES SECONDS
C  9 :NMT   :LGR VECT FACES VOISINES MAIN
C 10 :JINSCR:ADRESSE DU BUFFER SCRATCH
C 11 :IBC   :FLAG POUR CONNDITION AUX LIMITES
C 12 :IBUC  :FLAG BUCKET SORT (SAUF TYPE 7)
C 13 :IDEF  :FLAG DE DEF DU TYPE D'INPUT DES SURFACES S/M
C 14 :IVIS2 :Viscosity flag TYPE                          :7, 24, 25  
C 14 :IVIS2==-1 : FLAG FOR ADHESION AT INTERFACES          :25
C 14 :NRTM  :ND DE CALCULS DE FORCE INTERFACE TYPE 4 ;
C 15 :NOINT :NO USER DE L'INTERFACE
C 16 :      : FLAG PARALLELILSATION BOUCLES INTERFACES
C 17 :IDELx : flag shooting nodes                         : 2,3,5,7,10,11
C 18 :NCONT :NOMBRE DE CONTACTS MOYEN EN SPMD 
C           (NCONT=NSN*NMN_L/NMN)                         : 7,10,11
C 19 :ISINT :FLAG TH
C 20 :ILEV  :FLAG DE FORMULATION
C 21 :IGAP  :FLAG DE GAP VARIABLE
C 22 :INACTI:inactivation pene init                       :(3,4,5,6)7 
C 23 :MULTIMP:number of possible muliple impact           :     7,10,11
C 24 :NSNR  :NOMBRE DE NOEUDS SECONDARY ADDITIONNELS DES  
C            PROC REMOTEEN SPMD                           :     7,10,11
C    :IRM   :RENUMBER MAIN FLAG                         : 3,5,6;8
C 25 :IRS   :RENUMBER  SECONDARY FLAG                         : 3,6
C 26 :HIERA :FLAG DE TRAITEMENT HIERARCHIE                : 2 ,(12,13) 
C    :NOD1  : ID Node groug                               : 20
C    :NB DE SHELL PARMI NRTM                              : 24
C 27 :IADFIN:1ere ADRESSE LIBRE DANS TABLEAU CHAINE       : 11 
C 28 :INTSEC:Nombre de Section concernant cette interface 
C 29 :ICONT :flag de contact (pour sensor) 
C 30 :MFROT :MODELE DU FROTTEMENT                         : 5,7,20
C 31 :IFQ   :FLAG DE FILTRAGE DU FROTTEMENT               : 5,7,20
C 32 :IBAG  :FLAG COUPLAGE POROSITE D'AIRBAG              : 5,7,20
C 33 :ILAGM :
C 34 :IGSTI : 
C 34 :IGE   : I17 
C 35 :ISTOK : I17
C 35 :NUVAR : user property for interf 2 + rupture
C 36 :IGN   : I17
C    :NLN   :total number of nodes (secondary+main+edge)    20
C 36 :NISUB : sous interfaces                              7 10 20
C 37 :NISUBS: sous interfaces                              7 10 20
C 38 :NISUBM: sous interfaces                              7 10 20
C 39 :ICURV : courbure fixe                                   7 20
C 40 :NA1   : courbure fixe                                   7 20
C 41 :NA2   : courbure fixe                                   7 20
C 42 :ISYME : EDGE SYMMETRIE                                 11 20
C 43 :PID  :  user property for interf 2 + rupture          2
C    :ISYM  : surface symmetriques                         20
C 44 :IADM  :FLAG COUPLAGE ADAPTIVE MESHING                 5 7 20
C 45 :ISU1  : Secondary Gpe or Surface                         20
C 46 :ISU2  : Main Gpe or Surface                        20
C 47 :IFILTR: Flag filtrage interf type 2 with rupture
C    :INTTH : Flag thermique                               7...
C 48 :IFORM : Choice of formulation                       7
C               (constant tempurature or contact shell&brick)
C    :IFUNS : Function ID for type 2 with rupture          2
C 49 :IFUNN : Function ID for type 2 with rupture          2
C 49 :NRADM:Nb of elements within an arc, if adaptive meshing.
C 50 :IFNOR : for computing normal force                  8
C 50 :IFUNT : Function ID for type 2 with rupture
C 51 :NLINS : NBRE DE LIGNES SECONDS                      20
C 52 :MLINM : NBRE DE LIGNES MAIN                    20
C 53 :NLINSA: NBRE LIGNES SECONDS ACTIVES                20
C 54 :MLINMA: NBRE LIGNES MAIN ACTIVES              20
C 55 :NSNE  : NBRE DE POINTS DES LIGNES SECONDS           20
C 56 :NMNE  : NBRE DE POINTS DES LIGNES MAINS            20
C 57 :NSNER  :NOMBRE DE LIGNES SECONDARY ADDITIONNELS REMOTE  20  
C 58 :IEDGE : FLAG TYPE d'EDGE                             20
C 59 :LINE1 : ID Line 1                                    20
C 60 :LINE2 : ID Line 2                                    20
C 61 :IDELKEEP: Keep non-connected secondary nodes (IDEL)       3,5,7,10,11,20,21,23,24
C
C 73 :NRTM_IGE  :NBRE DE FACES ISOGEOMETRIQUES MAIN
C 74 :NRTM_FE   :NBRE DE FACES ELEMENTS FINIS  MAIN
C 75 :NTRS_IGE  :NBRE DE FACES ISOGEOMETRIQUES SECONDS
C 76 :NTRS_FE   :NBRE DE FACES ELEMENTS FINIS  SECONDS
C 77 :NSN_IGE   :NBRE DE POINTS ISOGEOMETRIQUES SECONDS
C 78 :NSN_FE    :NBRE DE POINTS ELEMENTS FINIS SECONDS
C 79 :NMN_IGE   :NBRE DE POINTS ISOGEOMETRIQUES MAINS
C 80 :NMN_FE    :NBRE DE POINTS ELEMENTS FINIS MAINS
C=======================================================================
      ALLOCATE(NTAG_TARGET(2*NUMNOD+1), STAT=Stat)
      NTAG(0:2*NUMNOD) => NTAG_TARGET(1:2*NUMNOD+1)

      DO K=0,2*NUMNOD
        NTAG(K) = 0
      ENDDO

      LAG_NC16  = 0
      LAG_NK16  = 0
      IMAXIMP   = 0
      MAXRTM    = 0
      MAXRTMS   = 0
      MAXRTM_T2 = 0
      NRTMS     = 0 
      NRTMM     = 0 
      NMNT      = 0 
      NRTMM_IGE = 0
      NRTMS_IGE = 0
      STIFF_STAT = ZERO
      
C=======================================================================
      DO NI=1,LINTER
C
        IGAP   = IPARI(21,NI)
        INACTI = IPARI(22,NI)
        MULTIMP= IPARI(23,NI)
        IEDGE   = IPARI(58,NI)
        MULTIMPE= IPARI(87,NI)
        MULTIMPS= IPARI(89,NI)
        NTYP   = IPARI(7,NI)
        IS1    = IPARI(13,NI)/10
        IS2    = MOD(IPARI(13,NI),10)
        NOINT  = IPARI(15,NI)
        ISU1 = IPARI(45,NI) 
        ISU2 = IPARI(46,NI)  
        ISTIFF=IPARI(29,NI)       
        ILAGM = IPARI(33,NI)
        TYPE18=.FALSE.
        IF(NTYP==7 .AND. INACTI==7 )TYPE18=.TRUE.
        GRBRIC_ID  = ISU1
        IF(TYPE18)GRBRIC_ID  = IPARI(83,NI) 
C-----  --
        NSN=0
        NSN_FE=0
        NSN_IGE=0
        NMN=0
        NMN_FE=0
        NMN_IGE=0
        NRTS=0
        NRTS_IGE=0
        NRTS_FE =0
        NRTM=0
        NRTM_IGE=0
        NRTM_FE =0
        IF(IS1 == 1)THEN
            NRTS_IGE=IGRSURF(ISU1)%NSEG_IGE
            NRTS_FE =IGRSURF(ISU1)%NSEG
            NRTS = NRTS_FE + NRTS_IGE
        ELSEIF(IS1 == 2) THEN
            NSN_FE=IGRNOD(ISU1)%NENTITY
            NSN = NSN_FE
        ELSEIF(IS1 == 3)THEN
            NRTS_FE=IGRSLIN(ISU1)%NSEG
            NRTS = NRTS_FE
        ELSEIF(IS1 == 4)THEN
            NSN=0
        ELSEIF(IS1 == 5) THEN
            NSN_FE=IGRBRIC(ISU1)%NENTITY
            NSN = NSN_FE            
        ENDIF
        IF(IS2 == 1)THEN
            NRTM_IGE=IGRSURF(ISU2)%NSEG_IGE
            NRTM_FE =IGRSURF(ISU2)%NSEG
            NRTM = NRTM_FE + NRTM_IGE
        ELSEIF(IS2 == 3)THEN
            NRTM_FE=IGRSLIN(ISU2)%NSEG
            NRTM = NRTM_FE
        ELSEIF(IS2 == 4)THEN
            NRTM_IGE=IGRSURF(ISU2)%NSEG_IGE
            NRTM_FE=IGRSURF(ISU2)%NSEG
            NRTM = NRTM_FE + NRTM_IGE
C     IS2=4 is Input by Surface Type of
C              ISURF(4,NS) == 100 Hyper-Ellipsoid MaDyMo coupled with.
C              ISURF(4,NS) == 101 Hyper-Ellipsoid Radioss defined.
C     IS2=4 is available for Interface TYPE 14 only :
C     should be checked for all others interfaces in LECIN4 ...
        ENDIF
C
        IPARI(3,NI)  = NRTS
        IPARI(6,NI)  = NMN 
        IPARI(4,NI)  = NRTM
        IPARI(5,NI)  = NSN 
        IPARI(73,NI) = NRTM_IGE
        IPARI(74,NI) = NRTM_FE
        IPARI(75,NI) = NRTS_IGE
        IPARI(76,NI) = NRTS_FE
        IPARI(77,NI) = NSN_IGE
        IPARI(78,NI) = NSN_FE
        IPARI(79,NI) = NMN_IGE
        IPARI(80,NI) = NMN_FE
C
        IF (NTYP == 14) THEN
          NMNT = MAX(NMNT,4*NUMNOD)
        ELSEIF (NTYP == 15) THEN
          NMNT = MAX(NMNT,9*NUMNOD+12*NUMELS+2*NUMELC+2*NUMELTG)
        ELSEIF(NTYP == 16)THEN
          NSN   =IGRNOD(ISU1)%NENTITY
          NME   =IGRBRIC(ISU2)%NENTITY
          IPARI(4,NI)=NME
          IPARI(5,NI)=NSN
          IPARI(36,NI)=ISU1
          IPARI(34,NI)=ISU2
          LAG_NC16 = NUMNOD
          LAG_NK16 = NUMNOD*51
          NMNT = MAX(NMNT,4*(NME+100)+ 2*NSN )
        ELSEIF(NTYP == 17)THEN
          NSN   =IGRBRIC(ISU1)%NENTITY
          NME   =IGRBRIC(ISU2)%NENTITY
          IPARI(4,NI)=NME
          IPARI(5,NI)=NSN
          IPARI(36,NI)=ISU1
          IPARI(34,NI)=ISU2
          NMNT = MAX(NMNT, 4*(NME+100)+ 4*(NSN+100)) 
          IF(IPARI(33,NI)==0)THEN
            IMAXIMP = IMAXIMP + (MULTIMP*NSN*16)/5 + 1
          ELSE
            LAG_NC16 = NUMNOD                          
            LAG_NK16 = NUMNOD*51                       
          ENDIF
C--------
C       n_mul_mx et l_mul_lag sont surestimes
c        NKMAX = 51
c        N_MUL_MX   = NUMNOD
c        N_MUL_MX_I = NUMNOD
c        N_BCS      = 0
c        NH = N_MUL_MX * 5
c        NMNT =MAX(NMNT,
c     .    8*(NME+100) + (1+4*NKMAX) * N_MUL_MX_I 
c     .    + 6*(NUMELS16+NUMELS20))
c        L_MUL_LAG = MAX(L_MUL_LAG,
c     .   (1+4*NKMAX)*N_MUL_MX_I + 6 * (NUMELS16+NUMELS20),
c     .   (1+4*NKMAX)*N_MUL_MX_I + 5*N_MUL_MX + 3*NUMNOD + 2*NH)
C--------
        ELSEIF(NTYP == 20)THEN
          IALLO = 1  ! evaluation de la memoire
          CALL I20SURFI(IALLO      ,IPARI(1,NI),IGRNOD    ,IGRSURF     ,
     2                  IGRSLIN    ,IBID       ,FRIGAP(1,NI),
     3                  IBID       ,IBID       ,IBID      ,IBID        ,
     4                  IBID       ,IBID       ,IBID      ,IBID        ,
     5                  IBID       ,IBID       ,X         ,IBID        ,
     6                  IBID       )
          NRTS = IPARI(3,NI)
          NMN  = IPARI(6,NI)
          NRTM = IPARI(4,NI)
          NSN  = IPARI(5,NI)
          NLN  = IPARI(35,NI)

        ELSEIF(NTYP == 24)THEN
          IALLO = 1  ! evaluation de la memoire
!!          INTPLY = 0
          CALL I24SURFI(
     1      IALLO        ,IPARI(1,NI)  ,IGRNOD      ,IGRSURF      ,
     2      IBID         ,FRIGAP(1,NI) ,
     3      IBID         ,IBID         ,ITAB        ,X            ,
     4      IBID         ,IBID         ,IBID        ,IBID         ,
     5      IBID         ,ITAG         ,INTPLY      ,IXC          ,
     6      IXTG         ,KNOD2ELC     ,KNOD2ELTG   ,NOD2ELC      ,
     7      NOD2ELTG     ,KNOD2ELS     ,NOD2ELS     ,IXS          ,
     8      IXS10        ,IXS16        ,IXS20       ,IBID         ,
     9      IBID         ,IBID         ,IBID        ,IPARI(86,NI) )
!!            IPARI(66,NI) = INTPLY
            INTPLY= IPARI(66,NI) 
            IF(INTPLY > 0) INTPLYXFEM = 1 
C----------NRTS,NRTM is calculated in I24SURFI     
C--------------number of shell seg is not doubled yet for NRTM 
          NRTM = IPARI(4,NI)
C!!!!!re-calculate NRTM_SH taking into account to ISU1  :inside I24SURFI        
          NRTS = IPARI(3,NI)
          NMN  = IPARI(6,NI)
          NSN  = IPARI(5,NI)
          NLN  = IPARI(35,NI)
          NRTM_SH=IPARI(42,NI)
C          
        ELSEIF(NTYP == 25)THEN
          IALLO = 1  ! evaluation de la memoire
          INTPLY = 0
          CALL I25SURFI(
     1      IALLO        ,IPARI(1,NI)  ,IGRNOD     ,IGRSURF      ,
     2      IBID         ,FRIGAP(1,NI) ,
     3      IBID         ,IBID         ,ITAB        ,X            ,
     4      IBID         ,IBID         ,IBID        ,IBID         ,
     5      IBID         ,ITAG         ,INTPLY      ,IXC          ,
     6      IXTG         ,KNOD2ELC     ,KNOD2ELTG   ,NOD2ELC      ,
     7      NOD2ELTG     ,KNOD2ELS     ,NOD2ELS     ,IXS          ,
     8      IXS10        ,IXS16        ,IXS20       ,IBID         ,
     9      IBID         ,IBID         ,IBID        )
            IPARI(66,NI) = INTPLY
            IF(INTPLY > 0) INTPLYXFEM = 1          
C----------NRTS,NRTM is calculated in I25SURFI     
C--------------number of shell seg is not doubled yet for NRTM 
          NRTM = IPARI(4,NI)
C!!!!!re-calculate NRTM_SH taking into account to ISU1  :inside I25SURFI        
          NRTS = IPARI(3,NI)
          NMN  = IPARI(6,NI)
          NSN  = IPARI(5,NI)
          NLN  = IPARI(35,NI)
          NRTM_SH=IPARI(42,NI)
        ENDIF
C--------------------------------------------     
C     Dimensionnement du aux sous-interfaces
C--------------------------------------------     
        NISUB=0
C--------------------------------------------
        IF(NTYP==25)THEN
C--------------------------------------------
          DO JSUB=1,NINTSUB

C Subinter : Case Inter corresponding to id inter

            IF(NOM_OPT(2,NINTER+JSUB) == NOINT .AND. 
     .         NOM_OPT(5,NINTER+JSUB) == 1)THEN
              NISUB=NISUB+1
              IGR  =NOM_OPT(4,NINTER+JSUB)
              ISU1 =NOM_OPT(3,NINTER+JSUB)
              ISU2 =NOM_OPT(6,NINTER+JSUB)
              IF(IGR/=0)
     .          IPARI(37,NI)=IPARI(37,NI)+IGRNOD(IGR)%NENTITY
              IPARI(38,NI)=IPARI(38,NI)+IGRSURF(ISU1)%NSEG
              IF(ISU2/=0)THEN
                IPARI(38,NI)=IPARI(38,NI)+IGRSURF(ISU2)%NSEG
                IPARI(37,NI)=IPARI(37,NI)+4*IGRSURF(ISU1)%NSEG
                IPARI(37,NI)=IPARI(37,NI)+4*IGRSURF(ISU2)%NSEG
                IF(IEDGE/=0)THEN
                  IPARI(90,NI)=IPARI(90,NI)+4*IGRSURF(ISU1)%NSEG
                  IPARI(90,NI)=IPARI(90,NI)+4*IGRSURF(ISU2)%NSEG
                END IF
              END IF
            END IF

C Subinter :  Case Inter 0

            IF(NOM_OPT(2,NINTER+JSUB) == 0 
     .         .AND. NOM_OPT(5,NINTER+JSUB) == 1) THEN
              NISUB=NISUB+1
              ISU1 =NOM_OPT(3,NINTER+JSUB)
              ISU2 =NOM_OPT(6,NINTER+JSUB)
              IF(ISU2 /= 0 ) THEN 
                 IPARI(37,NI) = IPARI(37,NI) + 4* IGRSURF(ISU2)%NSEG
                 IPARI(38,NI)=IPARI(38,NI)+IGRSURF(ISU2)%NSEG
                 IF(IEDGE/=0)THEN
                   IPARI(90,NI)=IPARI(90,NI)+4*IGRSURF(ISU2)%NSEG
                 END IF          
              ENDIF
              IF(ISU1 /=0 ) THEN
                 IPARI(37,NI) = IPARI(37,NI) + 4* IGRSURF(ISU1)%NSEG
                 IPARI(38,NI)=IPARI(38,NI)+IGRSURF(ISU1)%NSEG
                 IF(IEDGE/=0)THEN
                   IPARI(90,NI)=IPARI(90,NI)+4*IGRSURF(ISU1)%NSEG
                 END IF 
              ENDIF
            END IF
          END DO
          IPARI(36,NI)=NISUB

C
        ELSEIF(NTYP==7.OR.NTYP==10.OR.NTYP==20
     . .OR.NTYP==22.OR.NTYP==24)THEN
C--------------------------------------------

C Subinter : Case Inter corresponding to id inter

          DO JSUB=1,NINTSUB
            IF(NOM_OPT(2,NINTER+JSUB) == NOINT .AND. 
     .         NOM_OPT(5,NINTER+JSUB) == 1)THEN
              NISUB=NISUB+1
              IF (IPARI(71,NI).EQ.0) THEN
                IGR =NOM_OPT(4,NINTER+JSUB)
                IPARI(37,NI)=IPARI(37,NI)+IGRNOD(IGR)%NENTITY
                ISU  =NOM_OPT(3,NINTER+JSUB)
                IPARI(38,NI)=IPARI(38,NI)+IGRSURF(ISU)%NSEG
              ELSEIF (IPARI(71,NI).EQ.-1) THEN
C--             Type7 of type19
                ISU1 =NOM_OPT(4,NINTER+JSUB)
                IPARI(37,NI)=IPARI(37,NI)+4*IGRSURF(ISU1)%NSEG
                ISU2  =NOM_OPT(3,NINTER+JSUB)
                IPARI(38,NI)=IPARI(38,NI)+IGRSURF(ISU2)%NSEG
              ELSE
C--             Type7 sym of type19
                ISU1 =NOM_OPT(3,NINTER+JSUB)
                IPARI(37,NI)=IPARI(37,NI)+4*IGRSURF(ISU1)%NSEG
                ISU2  =NOM_OPT(4,NINTER+JSUB)
                IPARI(38,NI)=IPARI(38,NI)+IGRSURF(ISU2)%NSEG
              ENDIF
            END IF

C Subinter :  Case Inter 0

            IF(NOM_OPT(2,NINTER+JSUB) == 0 
     .         .AND. NOM_OPT(5,NINTER+JSUB) == 1) THEN
              NISUB=NISUB+1
              ISU1 =NOM_OPT(3,NINTER+JSUB)
              ISU2 =NOM_OPT(6,NINTER+JSUB)
              IF(ISU2 /= 0 ) THEN 
                 IPARI(37,NI) = IPARI(37,NI) + 4* IGRSURF(ISU2)%NSEG 
                 IPARI(38,NI)=IPARI(38,NI)+IGRSURF(ISU2)%NSEG     
              ENDIF
              IF(ISU1 /=0 ) THEN
                 IPARI(37,NI) = IPARI(37,NI) + 4* IGRSURF(ISU1)%NSEG 
                 IPARI(38,NI)=IPARI(38,NI)+IGRSURF(ISU1)%NSEG
              ENDIF
            END IF

          END DO
          IPARI(36,NI)=NISUB
C
        ELSEIF (NTYP.EQ.11) THEN
C
          DO JSUB=1,NINTSUB
            IF(NOM_OPT(2,NINTER+JSUB) == NOINT .AND. 
     .         NOM_OPT(5,NINTER+JSUB) == 1)THEN
              NISUB=NISUB+1
              IF (IPARI(71,NI).EQ.0) THEN
                ISU1 =NOM_OPT(4,NINTER+JSUB)
                IPARI(37,NI)=IPARI(37,NI)+IGRSLIN(ISU1)%NSEG
                ISU2  =NOM_OPT(3,NINTER+JSUB)
                IPARI(38,NI)=IPARI(38,NI)+IGRSLIN(ISU2)%NSEG
              ELSE
C--             Type11 of type19
                ISU1 =NOM_OPT(4,NINTER+JSUB)
                 IPARI(37,NI)=IPARI(37,NI)+4*IGRSURF(ISU1)%NSEG
                ISU2  =NOM_OPT(3,NINTER+JSUB)
                IPARI(38,NI)=IPARI(38,NI)+4*IGRSURF(ISU2)%NSEG
              ENDIF
            END IF

C Subinter :  Case Inter 0

            IF(NOM_OPT(2,NINTER+JSUB) == 0 
     .         .AND. NOM_OPT(5,NINTER+JSUB) == 1) THEN
              NISUB=NISUB+1
              ISU1 =NOM_OPT(3,NINTER+JSUB)
              ISU2 =NOM_OPT(6,NINTER+JSUB)
              IF(ISU2 /= 0 ) THEN 
                 IPARI(37,NI) = IPARI(37,NI) + 4* IGRSURF(ISU2)%NSEG    
                 IPARI(38,NI) = IPARI(38,NI) + 4* IGRSURF(ISU2)%NSEG        
              ENDIF
              IF(ISU1 /=0 ) THEN
                 IPARI(38,NI)=IPARI(38,NI)+4*IGRSURF(ISU1)%NSEG
                 IPARI(37,NI) = IPARI(37,NI) + 4* IGRSURF(ISU1)%NSEG    
              ENDIF
            END IF
          END DO
          IPARI(36,NI)=NISUB          
C
        END IF
C
        IF(NTYP/=23)THEN
          NRTMS = MAX(NRTMS,NRTS_FE+NRTS_IGE)
          NRTMM = MAX(NRTMM,NRTM_FE+NRTM_IGE)
          NRTMS_IGE = MAX(NRTMS_IGE,NRTS_IGE)
          NRTMM_IGE = MAX(NRTMM_IGE,NRTM_IGE)
        ELSE
          NRTMS = MAX(NRTMS,2*NRTS)
          NRTMM = MAX(NRTMM,2*NRTM)
        END IF
C
      ENDDO
C=======================================================================
      S_IRECTS = NRTMS*4
      S_IRECTM = NRTMM*4
      ALLOCATE (IRECTS(S_IRECTS)    ,STAT=stat)
      ALLOCATE (IRECTM(S_IRECTM)    ,STAT=stat)
      S_NSV=MAX(NUMNOD,NRTMS_IGE*16)
      ALLOCATE (NSV(S_NSV) ,STAT=stat)
      S_MSR=MAX(NUMNOD,NRTMM_IGE*16)
      ALLOCATE (MSR(S_MSR) ,STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268, ANMODE=ANINFO, MSGTYPE=MSGERROR, C1='IRECTS')
      IRECTS = 0
      IRECTM = 0
      NSV(1:S_NSV) = 0
      MSR(1:S_MSR) = 0
      MAXNSNE = 0
C
C----
C      LECTURE DES RENSEIGNEMENTS SURFACE SECONDARY/MAIN
C----
C=======================================================================
      DO NI=1,LINTER
        
        NRTS   = IPARI(3,NI)
        NRTM   = IPARI(4,NI)
        NSN    = IPARI(5,NI)
        NMN    = IPARI(6,NI)
        NTYP   = IPARI(7,NI)
        NOINT  = IPARI(15,NI)
        IS1    = IPARI(13,NI)/10
        ILAGM  = IPARI(33,NI)
        IS2    = MOD(IPARI(13,NI),10)
        ISU1   = IPARI(45,NI)
        ISU2   = IPARI(46,NI)
        IRS    = 0
        IRM    = 0
        IEDGE   = IPARI(58,NI)
C-- deactivated interfaces
        IF (NTYP.EQ.0) CYCLE
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
C-------------------------------------------
        ID=NOM_OPT(1,NI)
        CALL FRETITL2(TITR,
     .                NOM_OPT(LNOPT1-LTITR+1,NI),LTITR)
C--------------------------------------------
C      1) SECONDARY SIDE FROM NODES IN GRBRIC (INT18)
C--------------------------------------------
        IF (TYPE18)THEN
        !TYPE18 + GRNOD_ID
          IF(GRBRIC_ID > 0)THEN
            NBRIC = IGRBRIC(GRBRIC_ID)%NENTITY
            IF (MULTI_FVM%IS_USED)THEN
              NSN_FE = NBRIC  
            ELSE
              NBRIC=IGRBRIC(ISU1)%NENTITY
              CALL INGRBRIC_NODES(NSN_FE ,NOINT  ,IGRBRIC(GRBRIC_ID)%ENTITY   ,ITAB   ,NSV   , 
     .                       IXS  , NBRIC, 1, NALE ,IPM, BUFMAT,S_NSV)                         
            ENDIF
          ELSE
            NBRIC = 0
            NSN   = 0
          ENDIF
          NSN = NSN_FE+NSN_IGE
          IPARI(5,NI)  = NSN 
          IPARI(78,NI) = NSN_FE
        
          IF(GRBRIC_ID > 0)THEN
            IS_GAP_COMPUTED = .FALSE.
            CALL INGRBRIC_DX(NBRIC, IGRBRIC(GRBRIC_ID)%ENTITY, FRIGAP(2,NI), IXS, X,
     .                       NOINT, TITR, IS_GAP_COMPUTED, PM, RHO0_MAX, IDDLEVEL,ISTIFF,AUTO_RHO,AUTO_LENGTH)
            IF(IS_GAP_COMPUTED)THEN
              WRITE(IOUT,1000)NOINT
              WRITE(IOUT,3020)FRIGAP(2,NI)
            ENDIF
          ENDIF
C--------------------------------------------
C      2) SECONDARY FROM SURFACE
C--------------------------------------------
        ELSEIF ( NTYP/=15.and.NTYP/=17.and.NTYP/=20.and.NTYP/=22.and.
     .       NTYP/=23.and.NTYP/=24.and.NTYP/=25) THEN
C--------------------------------------------
          IF(IS1.NE.0) THEN
            IF(NRTS_FE == 0.AND.NRTS_IGE == 0.AND.IS1.NE.2.AND.IS1.NE.5) THEN
              CALL ANCMSG(MSGID=118,  MSGTYPE=MSGERROR,  ANMODE=ANINFO, I1=ID, C1=TITR)
            ENDIF
            IF(IS1 == 1)THEN
              IF (NTYP == 3 .OR. NTYP == 6) IRS = IPARI(25,NI)
              SURF_NODES => IGRSURF(ISU1)%NODES(1:NRTS_FE,1:4)
              CALL INSURF(NRTS_FE,NSN_FE,IRS,IRECTS,NOINT,
     .                    SURF_NODES,ITAB,NSV,ID,TITR,
     .                    NTAG,S_NSV,S_IRECTS,X,TYPE18)
              IF (IGRSURF(ISU1)%NSEG_IGE .GE. 1) THEN
                SURF_NODES_IGE => IGRSURF(ISU1)%NODES_IGE(1:NRTS_IGE,1:4)
                IAD_IGE = IGRSURF(ISU1)%IAD_IGE               
                CALL INSURFIGEO(NRTS_IGE,NRTS_FE,NSN_IGE,0,IAD_IGE,IRM,IRECTS,NOINT,
     .                     SURF_NODES_IGE,ITAB,NSV,ID,TITR,
     .                     RBID,RBID,RBID,RBID,RBID,RBID,RBID,RBID)
              ENDIF
            ELSEIF(IS1 == 2)THEN
              CALL INPOINT(NSN_FE,NOINT,IGRNOD(ISU1)%ENTITY,ITAB,NSV)
            ELSEIF(IS1 == 3)THEN
              LINE_NODES => IGRSLIN(ISU1)%NODES(1:NRTS,1:2)
              CALL INSLIN(NRTS,NSN_FE,IRECTS,NOINT,
     .                    LINE_NODES,ITAB,NSV,
     .                    NTAG)
              
            ELSEIF(IS1==5) THEN
              !stop 151018
               !  GRBRIC_ID + TYPE 18
               NBRIC = IGRBRIC(ISU1)%NENTITY
               CALL INGRBRIC(NSN, NOINT, IGRBRIC(ISU1)%ENTITY, ITAB, NSV,
     .                       IXS, NBRIC, PM,S_NSV)
            ENDIF
          ENDIF
          NSN = NSN_FE+NSN_IGE
          IPARI(5,NI)  = NSN
          IPARI(77,NI) = NSN_IGE
          IPARI(78,NI) = NSN_FE
C--------------------------------------------
C      2) SECONDARY GRBRIC (INT22)
C--------------------------------------------
        ELSEIF(NTYP==22) THEN
          IF(ISU1 > 0)THEN
            NBRIC=IGRBRIC(ISU1)%NENTITY
            CALL INGRBRIC(NSN   ,NOINT  ,IGRBRIC(ISU1)%ENTITY   ,ITAB   ,NSV   , 
     .                    IXS  , NBRIC  ,PM, S_NSV)
          ELSE
            NBRIC = 0
            NSN   = 0
          ENDIF
          IPARI(5,NI)  = NSN
          IPARI(32,NI) = NBRIC  !IBAG type7
          IPARI(30,NI) = ISU1   !IBAG type7
C--------------------------------------------
        ELSEIF(NTYP==23) THEN
          IF(IS1.NE.0) THEN
            IF(NRTS == 0.AND.IS1.NE.2) THEN
              CALL ANCMSG(MSGID=118, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=ID, C1=TITR)
            ENDIF
            IF(IS1 == 1)THEN
              SURF_NODES => IGRSURF(ISU1)%NODES(1:NRTS,1:4)
              CALL INSURF23(NRTS,NSN,IRS,IRECTS,NOINT,
     .                      SURF_NODES,ITAB,NSV,NRTS_NEW,X,
     .                      NTAG)
            ENDIF
            NRTS   = NRTS_NEW
            IPARI(3,NI) = NRTS
          ENDIF
          IPARI(5,NI) = NSN
C--------------------------------------------
        ENDIF
C-----
C      2)SURFACE MAIN :
C-----
C--------------------------------------------
        IF (NTYP == 14) THEN
C--------------------------------------------
          IF(NRTM == 0) THEN
            CALL ANCMSG(MSGID=119,  MSGTYPE=MSGERROR,  ANMODE=ANINFO, I1=ID, C1=TITR)
          ENDIF
          IPARI(19,NI)=1
C--------------------------------------------
        ELSEIF ( NTYP == 15) THEN
C--------------------------------------------
C         SURFACE SECONDARY
          IF(NRTS == 0) THEN
            CALL ANCMSG(MSGID=118, MSGTYPE=MSGERROR, ANMODE=ANINFO, I1=ID, C1=TITR)
          ENDIF
          SURF_NODES => IGRSURF(ISU1)%NODES(1:NRTS,1:4)
          CALL INSURF(NRTS,NSN,IRS,IRECTS,NOINT,
     .                SURF_NODES,ITAB,NSV,ID,TITR,
     .                NTAG,S_NSV,S_IRECTS,X,TYPE18)
          IPARI(5,NI) = NSN
C         SURFACE MAIN :
          IF(NRTM == 0) THEN
            CALL ANCMSG(MSGID=119, MSGTYPE=MSGERROR,  ANMODE=ANINFO,  I1=ID, C1=TITR)
          ENDIF
          IPARI(19,NI)=1
C--------------------------------------------
        ELSEIF ( NTYP == 20) THEN
C--------------------------------------------
C--------------------------------------------
        ELSEIF ( NTYP == 22) THEN
C--------------------------------------------
              SURF_NODES => IGRSURF(ISU2)%NODES(1:NRTM,1:4)
              CALL INSURF(NRTM,NMN,IRM,IRECTM,NOINT,
     .                    SURF_NODES,ITAB,MSR,ID,TITR,
     .                    NTAG,S_MSR,S_IRECTM,X,TYPE18)
              IPARI(6,NI)  = NMN           !nombre de main node.
              IPARI(33,NI) = IGRSURF(ISU2)%NSEG ! nombre de facette main
              IPARI(4,NI)  = IGRSURF(ISU2)%NSEG
C--------------------------------------------
        ELSEIF( NTYP == 23) THEN
          IF(IS2.NE.0) THEN
            IF(NRTM == 0) THEN
              CALL ANCMSG(MSGID=119,  MSGTYPE=MSGERROR,  ANMODE=ANINFO, I1=ID, C1=TITR)
            ENDIF
            IF(IS2 == 1)THEN
              SURF_NODES => IGRSURF(ISU2)%NODES(1:NRTM,1:4)
              CALL INSURF23(NRTM,NMN,IRM,IRECTM,NOINT,
     .                      SURF_NODES,ITAB,MSR,NRTM_NEW,X,
     .                      NTAG)
            ENDIF
            NRTM   = NRTM_NEW
            IPARI(4,NI)=NRTM
            IPARI(6,NI)=NMN
          ENDIF
        ELSEIF ( NTYP == 24) THEN
C--------------------------------------------
C--------------------------------------------
        ELSEIF ( NTYP == 25) THEN
C--------------------------------------------
C--------------------------------------------
        ELSE
          IF (NTYP == 3 .OR. NTYP == 5 .OR. 
     .        NTYP == 6 .OR. NTYP == 8) IRM = IPARI(24,NI)
          IF(IS2.NE.0) THEN
            IF(NRTM_FE == 0 .AND. NRTM_IGE == 0) THEN
              CALL ANCMSG(MSGID=119,  MSGTYPE=MSGERROR,  ANMODE=ANINFO, I1=ID,  C1=TITR)
            ENDIF
            IF(IS2 == 1)THEN
              SURF_NODES => IGRSURF(ISU2)%NODES(1:NRTM_FE,1:4)
              CALL INSURF(NRTM_FE,NMN_FE,IRM,IRECTM,NOINT,
     .                    SURF_NODES,ITAB,MSR,ID,TITR,
     .                    NTAG,S_MSR,S_IRECTM,X,TYPE18)
              IF (IGRSURF(ISU2)%NSEG_IGE .GE. 1) THEN
              SURF_NODES_IGE => IGRSURF(ISU2)%NODES_IGE(1:NRTM_IGE,1:4)
              IAD_IGE = IGRSURF(ISU2)%IAD_IGE              
                CALL INSURFIGEO(NRTM_IGE,NRTM_FE,NMN_IGE,NSN_IGE,IAD_IGE,IRM,IRECTM,NOINT,
     .                     SURF_NODES_IGE,ITAB,MSR,ID,TITR,
     .                     RBID,RBID,RBID,RBID,RBID,RBID,RBID,RBID)
              ENDIF
            ELSEIF(IS2 == 3)THEN
              LINE_NODES => IGRSLIN(ISU2)%NODES(1:NRTM,1:2)
              CALL INSLIN(NRTM,NMN_FE,IRECTM,NOINT,
     .                    LINE_NODES,ITAB,MSR,
     .                    NTAG)
            ELSEIF(IS2 == 4) THEN
              SURF_NODES => IGRSURF(ISU2)%NODES(1:NRTM_FE,1:4)
              CALL INSURF(NRTM_FE,NMN_FE,IRM,IRECTM,NOINT,
     .                    SURF_NODES,ITAB,MSR,ID,TITR,
     .                    NTAG,S_MSR,S_IRECTM,X,TYPE18)
              IF (IGRSURF(ISU2)%NSEG_IGE .GE. 1) THEN
              SURF_NODES_IGE => IGRSURF(ISU2)%NODES_IGE(1:NRTM_IGE,1:4)
              IAD_IGE = IGRSURF(ISU2)%IAD_IGE              
                CALL INSURFIGEO(NRTM_IGE,NRTM_FE,NMN_IGE,0,IAD_IGE,IRM,IRECTM,NOINT,
     .                     SURF_NODES_IGE,ITAB,MSR,ID,TITR,
     .                     RBID,RBID,RBID,RBID,RBID,RBID,RBID,RBID)
              ENDIF
            ENDIF
            NMN = NMN_FE+NMN_IGE
            IPARI(6,NI)  = NMN
            IPARI(79,NI) = NMN_IGE
            IPARI(80,NI) = NMN_FE
          ENDIF
C--------------------------------------------
          IF ( TYPE18 ) THEN
            IF(ISTIFF == 2 .AND. ISU2 > 0)THEN
              STIFF_STAT(1) = -STFAC(NI)     !-STFAC*VREF*VREF
              STIFF_STAT(2) = AUTO_RHO       !rho0_max (also computed for multimaterials)
              STIFF_STAT(3) = FRIGAP(2,NI) !gap
              SURF_NODES => IGRSURF(ISU2)%NODES(1:NRTM,1:4)
              CALL INSURF_DX(NRTM,NMN,IRM,IRECTM,NOINT,
     .                       SURF_NODES,ITAB,MSR,ID,TITR,
     .                       NTAG,S_MSR,S_IRECTM,X, STIFF_STAT)
              STFAC(NI)=STIFF_STAT(1)
            ENDIF
          ENDIF
        ENDIF
C--------------------------------------------
C
C-------
        IF (NTYP == 1) THEN
C-------
          CALL PRESEGMT(IRECTM,MSR,NRTM,NMN,IPARI(9,NI))
          NMNT=MAX0(NMNT,6*NMN)
C-------
        ELSEIF (NTYP == 2) THEN
C-------
          ILEV  = IPARI(20,NI)
          IF (ILEV == 10 .OR. ILEV == 11 .OR. ILEV == 12) THEN
            PID = NINTRI(IPARI(43,NI),IGEO,NPROPGI,NUMGEO,1) 
            IF (PID > 0) THEN                      
              NUVAR = IGEO(27,PID)
              IPARI(35,NI) = NUVAR
              IPARI(43,NI) = PID                      
            ELSE
c              print*,'error interface user'
            ENDIF
C
          ELSEIF (ILEV == 27 . OR . ILEV == 28) THEN
C-------- Dimension of arrays for TYPE2 incompatible spt27 or 28
            DO I=1,IGRNOD(ISU1)%NENTITY
              ISL = IGRNOD(ISU1)%ENTITY(I)
              T2_NB_CONNEC(ISL) = T2_NB_CONNEC(ISL) + 1
              IF (T2_NB_CONNEC(ISL) == 2) NSN_MULTI_CONNEC = NSN_MULTI_CONNEC + 1
            ENDDO            
          ENDIF
C
          IF (ILAGM == 1)THEN
            NCF_I2 = NSN*6
            LAG_NHF = LAG_NHF + NCF_I2*(NCF_I2-1)/2
            LAG_NCF = LAG_NCF + NCF_I2
c           IF (ILEV == 30) THEN
c             LAG_NKF = LAG_NKF + NCF_I2*27
c           ELSE
            LAG_NKF = LAG_NKF + NCF_I2*13 
c           ENDIF
          ENDIF
          MAXRTM_T2=MAX(MAXRTM_T2,NRTM)    
C-------
        ELSEIF (NTYP == 3 ) THEN
C-------
          CALL PRESEGMT(IRECTS,NSV,NRTS,NSN,IPARI(8,NI))
          CALL PRESEGMT(IRECTM,MSR,NRTM,NMN,IPARI(9,NI))

          IMAXIMP = IMAXIMP + 2*NINT(NMN/PROBINT)
C-------
        ELSEIF (NTYP == 4) THEN
C-------
          CALL PRESEGMT(IRECTS,NSV,NRTS,NSN,IPARI(8,NI))
          CALL PRESEGMT(IRECTM,MSR,NRTM,NMN,IPARI(9,NI))

          IBUC =IPARI(12,NI)
          IF(IBUC.NE.0)NMNT =MAX0(NMNT,14*NSN)
          IMAXIMP = IMAXIMP + 2*NINT(NMN/PROBINT)
C-------
        ELSEIF (NTYP == 5) THEN
C-------
          CALL PRESEGMT(IRECTS,NSV,NRTS,NSN,IPARI(8,NI))
          CALL PRESEGMT(IRECTM,MSR,NRTM,NMN,IPARI(9,NI))
          IMAXIMP = IMAXIMP + 2*(NINT(NMN/PROBINT) + NINT(NSN/PROBINT))
C-------
        ELSEIF (NTYP == 6) THEN
C-------
          CALL PRESEGMT(IRECTS,NSV,NRTS,NSN,IPARI(8,NI))
          CALL PRESEGMT(IRECTM,MSR,NRTM,NMN,IPARI(9,NI))
          IMAXIMP = IMAXIMP + 2*NINT(NSN/PROBINT)
C-------
        ELSEIF (NTYP == 7) THEN
C-------
          IPARI(18,NI) = NSN_FE+NSN_IGE+NMN_FE+NMN_IGE
          IPARI(24,NI) = NSN_FE+NSN_IGE
          IPARI(25,NI) = NSN_FE+NSN_IGE+NMN_FE+NMN_IGE
          IMAXIMP = IMAXIMP + MULTIMP*(NSN_FE+NSN_IGE)
          NMNT =MAX0(NMNT,NSN_FE+NSN_IGE + 3) 
          IF (ILAGM > 0) THEN
            LAG_NC16 = NUMNOD
            LAG_NK16 = NUMNOD*15
            NMNT = MAX(NMNT, 4*(NMN+100)+ 2*(NSN_FE+NSN_IGE) + LAG_NC16 + 4*LAG_NK16)                       
          ENDIF
          MAXRTM=MAX(MAXRTM,NRTM)
C-------
        ELSEIF (NTYP == 8) THEN
C-------
          CALL PRESEGMT(IRECTM,MSR,NRTM,NMN,IPARI(9,NI))
          IPARI(8,NI) = 0
          IMAXIMP = IMAXIMP + 2*NINT(NSN/PROBINT)
C-------
        ELSEIF (NTYP == 9) THEN
C-------
          CALL PRESEGMT(IRECTS,NSV,NRTS,NSN,IPARI(8,NI))
          CALL PRESEGMT(IRECTM,MSR,NRTM,NMN,IPARI(9,NI))

          NMNT=MAX0(NMNT,8*NMN)
          IMAXIMP = IMAXIMP + 2*NINT(NMN/PROBINT)
C-------
        ELSEIF (NTYP == 10) THEN
C-------
          IPARI(18,NI) = NSN+NMN
          IPARI(24,NI) = NSN
          IPARI(25,NI) = NSN+NMN
          NMNT =MAX0(NMNT,NSN + 3) 
          IMAXIMP = IMAXIMP + MULTIMP*NSN
          MAXRTM=MAX(MAXRTM,NRTM)
C-------
        ELSEIF (NTYP == 11) THEN
C-------
          IPARI(18,NI) = NSN+NMN
          IPARI(24,NI) = NSN
          IPARI(25,NI) = NSN+NMN
C   tableaux alloues directement en stack en v40
          MAXRTMS=MAX(MAXRTMS,NRTM)
          MAXRTMS=MAX(MAXRTMS,NRTS)
          IMAXIMP = IMAXIMP + MULTIMP*NSN
C-------
        ELSEIF (NTYP == 12) THEN
C-------
          CALL PRESEGMT(IRECTS,NSV,NRTS,NSN,IPARI(8,NI))
          CALL PRESEGMT(IRECTM,MSR,NRTM,NMN,IPARI(9,NI))
          NMNT=MAX0(NMNT,2*NVCONV*NSN+NRTM+NMN*(NVCONV+1),3*(NSN+NMN))
          IF(IPARI(20,NI) == 1) NMNT=MAX0(NMNT,6*NMN)
C-------
        ELSEIF (NTYP == 14) THEN
C-------
          IMAXIMP = IMAXIMP + 2*NINT(NSN/PROBINT/5)
C-------
        ELSEIF (NTYP == 15) THEN
C-------
          IMAXIMP = IMAXIMP + 2*NINT(NRTS*4/PROBINT/5)
C-------
        ELSEIF (NTYP == 18) THEN
C-------
          IPARI(36,NI)=0
          IPARI(18,NI) = NSN+NMN
          IPARI(24,NI) = NSN
          IPARI(25,NI) = NSN+NMN
          IMAXIMP = IMAXIMP + MULTIMP*NSN
          NMNT =MAX0(NMNT,NSN + 3) 
          MAXRTM=MAX(MAXRTM,NRTM)
C-------
        ELSEIF (NTYP == 20) THEN
C-------
          IPARI(18,NI) = NSN+NMN
          IPARI(24,NI) = NSN
          IPARI(25,NI) = NSN+NMN
          IMAXIMP = IMAXIMP + MULTIMP*NSN
          NMNT =MAX0(NMNT,NSN + 3) 
          MAXRTM=MAX(MAXRTM,NRTM)
C dimension allocation starter i11buc1
          NLINSA = IPARI(53,NI)
          NLINMA = IPARI(54,NI)
          NSNE = IPARI(55,NI)
          NMNT =MAX0(NMNT,NSNE + 3) 
          MAXRTM=MAX(MAXRTM,NLINMA)
          MAXRTMS=MAX(MAXRTMS,NLINMA)
          MAXRTMS=MAX(MAXRTMS,NLINSA)
C-------
        ELSEIF (NTYP == 21) THEN
C-------
          IPARI(8,NI)  = NMN
          IPARI(36,NI) = 0
          IPARI(18,NI) = NSN+NMN
          IPARI(24,NI) = NSN
          IPARI(25,NI) = NSN+NMN
          IMAXIMP = IMAXIMP + MULTIMP*NSN
          NMNT =MAX0(NMNT,NSN + 3) 
          MAXRTM=MAX(MAXRTM,NRTM)
C-------
        ELSEIF (NTYP == 22) THEN
C-------
          IPARI(18,NI) = NSN+NMN
          IPARI(24,NI) = NSN
          IPARI(25,NI) = NSN+NMN
          IMAXIMP = IMAXIMP + MULTIMP*NSN
          NMNT =MAX0(NMNT,NSN + 3) 
          IF (ILAGM > 0) THEN
            LAG_NC16 = NUMNOD
            LAG_NK16 = NUMNOD*15
            NMNT = MAX(NMNT, 4*(NMN+100)+ 2*NSN + LAG_NC16 + 4*LAG_NK16)                       
          ENDIF
          MAXRTM=MAX(MAXRTM,NRTM) 
C-------
        ELSEIF (NTYP == 23) THEN
C-------
          IPARI(18,NI) = NSN+NMN
          IPARI(24,NI) = NSN
          IPARI(25,NI) = NSN+NMN
          NMNT =MAX0(NMNT,NSN + 3) 
          IMAXIMP = IMAXIMP + MULTIMP*NSN
          MAXRTM=MAX(MAXRTM,NRTM)
C-------
        ELSEIF (NTYP == 24) THEN
C-------
          NRTM = NRTM+IPARI(42,NI)
          IPARI(4,NI) = NRTM
          IPARI(18,NI) = NSN+NMN
          IPARI(24,NI) = NSN
          IPARI(25,NI) = NSN+NMN
          IMAXIMP = IMAXIMP + MULTIMP*NSN
          NMNT =MAX0(NMNT,NSN + 3) 
          IF (ILAGM > 0) THEN
            LAG_NC16 = NUMNOD
            LAG_NK16 = NUMNOD*15
            NMNT = MAX(NMNT, 4*(NMN+100)+ 2*NSN + LAG_NC16 + 4*LAG_NK16)                       
          ENDIF
          MAXRTM=MAX(MAXRTM,NRTM)
          MAXNSNE=MAX(MAXNSNE,IPARI(55,NI))
C-------
        ELSEIF (NTYP == 25) THEN
C-------
          NRTM = NRTM+IPARI(42,NI)
          IPARI(4,NI) = NRTM
          IPARI(18,NI) = NSN+NMN
          IPARI(24,NI) = NSN
          IPARI(25,NI) = NSN+NMN
C
C         NADMSR, NEDGE are over estimated for ALLOCATE
C         NADMSR, NEDGE will be over written before writing
          IPARI(67,NI)=4*NRTM
          IPARI(68,NI)=4*NRTM
C
          IF(IEDGE /= 0) THEN
            NCONTE=4*NRTM       ! cf NCONTE=NEDGE, cest a revoir
            IPARI(88,NI)=NCONTE
          ELSE
            NCONTE=0
          END IF
C
          IMAXIMP = IMAXIMP + MULTIMP*NSN + MULTIMPE*NCONTE + MULTIMPS*NCONTE
          NMNT =MAX0(NMNT,NSN + 3) 
          IF (ILAGM > 0) THEN
            LAG_NC16 = NUMNOD
            LAG_NK16 = NUMNOD*15
            NMNT = MAX(NMNT, 4*(NMN+100)+ 2*NSN + LAG_NC16 + 4*LAG_NK16)                       
          ENDIF
          MAXRTM=MAX(MAXRTM,NRTM)
          MAXNSNE=MAX(MAXNSNE,IPARI(55,NI))
        ENDIF 
C
C-------
      ENDDO
C-----
      DO NI=1,LINTER
          IPARI(19,NI)=0
      ENDDO      
C-----
      LAG_NCL = LAG_NCL + LAG_NC16
      LAG_NKL = LAG_NKL + LAG_NK16
C
C=======================================================================
C-
      DEALLOCATE (IRECTS)
      DEALLOCATE (IRECTM)
      DEALLOCATE (NSV)
      DEALLOCATE (MSR)

      NULLIFY(NTAG)
      DEALLOCATE(NTAG_TARGET)
C-----
      RETURN
C-----
 999  CALL FREERR(3)
      RETURN
C-----
! inter18 : automatic gap if not defined in input file 
1000  FORMAT(/1X,'  INTERFACE NUMBER :',I10,1X,A)
3020  FORMAT(
     .    '    COMPUTED GAP VALUE. . . . . . . . . . . . . ',1PG20.13)
          
      END


