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
Chd|  ELBUF_INI                     source/elements/elbuf_init/elbuf_ini.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ALLOCBUF_AUTO                 source/elements/elbuf_init/allocbuf_auto.F
Chd|        INITVARS_AUTO                 source/elements/elbuf_init/initvars_auto.F
Chd|        INI_OUTMAX_AUTO               source/elements/elbuf_init/ini_outmax_auto.F
Chd|        ZEROVARS_AUTO                 source/elements/elbuf_init/zerovars_auto.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MAT_ELEM_MOD                  ../common_source/modules/mat_elem/mat_elem_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|====================================================================
      SUBROUTINE ELBUF_INI(ELBUF_TAB ,MAT_PARAM,
     .           MLAW_TAG  ,PROP_TAG ,FAIL_TAG ,
     .           IGEO      ,IPM      ,PM       ,IPARG    ,IPART     ,
     .           IPARTSP   ,IXS      ,IXQ      ,IXC      ,IXTG      ,
     .           FLAG_XFEM ,IPARTIG3D,STACK    ,IGEO_STACK,
     .           IXT       ,IXP      ,IXR      ,KXX      ,GEO       ,
     .           EOS_TAG   ,MAXEOS   ,ISTR_24  ,PRINT_FLAG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MAT_ELEM_MOD            
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD       
      USE STACK_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
#include      "sphcom.inc"
#include      "tablen_c.inc"
#include      "my_allocate.inc"
#include      "sms_c.inc"
#include      "boltpr_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER , INTENT(IN) :: ISTR_24
      INTEGER IPARG(NPARG,NGROUP),IPART(LIPART1,*),
     .        IPARTSP(*),IPM(NPROPMI,NUMMAT),IGEO(NPROPGI,NUMGEO),
     .        IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*),IXTG(NIXTG,*),
     .        FLAG_XFEM,IPARTIG3D(*),IGEO_STACK(NPROPGI,*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),KXX(NIXX,*),MAXEOS
      my_real :: PM(NPROPM,NUMMAT),GEO(NPROPG,NUMGEO)
      INTEGER, INTENT(IN) :: PRINT_FLAG !< flag to print the element group data
      TYPE(ELBUF_STRUCT_),TARGET ,DIMENSION(NGROUP)     :: ELBUF_TAB
      TYPE(MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(IN) :: MAT_PARAM
      TYPE(MLAW_TAG_)    ,TARGET ,DIMENSION(NUMMAT)     :: MLAW_TAG
      TYPE(EOS_TAG_)     ,TARGET ,DIMENSION(0:MAXEOS)   :: EOS_TAG
      TYPE(PROP_TAG_)    ,TARGET ,DIMENSION(0:MAXPROP)  :: PROP_TAG
      TYPE(FAIL_TAG_)    ,TARGET ,DIMENSION(0:MAXFAIL ) :: FAIL_TAG
      TYPE (STACK_PLY) :: STACK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IL,IR,IS,IT,NG,BUFLEN,IRUPT,IPID,IMID,IPRT,IAD,
     .   NEL,NPT,NPG,MLW,ISORTH,ISRATE,ISROT,IREP,ISVIS,NLAY,
     .   ISTRA,IFAIL,NFAIL,IEOS,IXFEM,NPTR,NPTS,NPTT,ISHPLYXFEM,
     .   NFT,NFT0,KFTS,ITY,JALE,OFF,ISMSTR,ICPRE,NSG,ICNOD,ISNOD,JEUL,
     .   JTUR,JTHE,JLAG,IRS,IRE,ICSTR,NVC,JMULT,IADM1,IADM2,
     .   MMAT1,MMAT2,JHBE,JIVF,JPOR,IPLA,PROC,JCLOS,CODCONV,
     .   IINT,JCVT,ICSEN,ILEV,IEXPAN,IGRE,JSMS,MAXFLAY,IVISC,
     .   NUVAR,NUVARM,NVARTMP,NUVARV,IGTYP,TSHELL,TSH_ORT,LINTLOC,
     .   LINTLAY,LINTPAR,NPAR_LAY,NPAR_TAB,NPAR_LBUF,NIP_LAY,NINTLAY,LIMAT,
     .   LIFAIL,LGLOB,LLAY,LLOC,LFAIL,GFAIL,LVISC,LMAT,LPORO,LEOS,NCTRL,
     .   ISUBSTACK,IPPID,IPIDL,NPT_LAY,LEN_PLAPT,LEN_SIGPT,
     .   IPMAT,IPMAT_IPLY,NPTTOT,NUVARN,NX,IBOLTP,CRKDIR,INLOC,LNLOC,
     .   NONL, DEBUG_PRINT,IPINCH,NUVAREOS,IEOS_TYPE,IDRAPE,LLOC_SLICE,
     .   IPNPT_LAY,IDFAIL,LNLOCTS
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: TFAIL,FAILID
      INTEGER, ALLOCATABLE, DIMENSION(:)   :: IMAT,ILAW
      TYPE(BUF_LAY_)     , POINTER :: BUFLY
      TYPE(BUF_INTLAY_)  , POINTER :: INTLAY
      TYPE(G_BUFEL_)     , POINTER :: GBUF
      TYPE(FAIL_LOC_)    , POINTER :: FLOC
      TYPE(BUF_NLOC_)    , POINTER :: BUFNL
      TYPE(BUF_NLOCTS_)  , POINTER :: BUFNLTS
C=======================================================================
c      Element Buffer Routine (Solid) 
c------
c      1) taille du tableau ELBUF pour restart
c      2) allocation de la structure ELBUF_TAB
C=======================================================================
      DEBUG_PRINT = 0
c      
      IF(PRINT_FLAG>6) WRITE(IOUT, 1001)
c
      DO NG = 1,NGROUP 
c---      read IPARG
c
        MLW     = IPARG(1,NG)     ! type de loi mat            
        NEL     = IPARG(2,NG)                                             
        NFT     = IPARG(3,NG)                                             
        ITY     = IPARG(5,NG)   
        NPT     = IPARG(6,NG)                                       
        JALE    = IPARG(7,NG)                                       
        OFF     = IPARG(8,NG)                                       
        ISMSTR  = IPARG(9,NG)                                       
        ICPRE   = IPARG(10,NG)
        NSG     = ICPRE                                             
        ICNOD   = IPARG(11,NG)                                      
        JEUL    = ICNOD                                             
        JTUR    = IPARG(12,NG)                                      
        JTHE    = IPARG(13,NG)                                      
        JLAG    = IPARG(14,NG)                                      
        IRS     = IPARG(15,NG)                                      
        IRE     = IPARG(16,NG)                                      
        ICSTR   = IPARG(17,NG)                                      
        NVC     = IPARG(19,NG)                                      
        JMULT   = IPARG(20,NG)                                      
        IADM1   = IPARG(21,NG)                                      
        IADM2   = IPARG(22,NG)                                      
        JHBE    = IPARG(23,NG)                                      
        JIVF    = IPARG(24,NG)                                      
        MMAT1   = IPARG(25,NG)                                      
        MMAT2   = IPARG(26,NG)                                      
        JPOR    = IPARG(27,NG)                                      
        ISNOD   = IPARG(28,NG)                                      
        IPLA    = IPARG(29,NG)                                      
        KFTS    = IPARG(30,NG)                                      
        NFT0    = IPARG(31,NG)                                      
        PROC    = IPARG(32,NG)                                      
        JCLOS   = IPARG(33,NG)                                      
        CODCONV = IPARG(34,NG)  ! pm(10)                            
        IREP    = IPARG(35,NG)                                                
        IINT    = IPARG(36,NG)                                                
        JCVT    = IPARG(37,NG)                                                
        IGTYP   = IPARG(38,NG)                                           
        ICSEN   = IPARG(39,NG)                                              
        ISRATE  = IPARG(40,NG)                                             
        ISROT   = IPARG(41,NG) 
        IF (ISNOD == 10) ISROT = IPARG(74,NG)                                             
        ISORTH  = IPARG(42,NG)                                             
        IFAIL   = IPARG(43,NG)                                             
        ISTRA   = IPARG(44,NG)                                             
        ILEV    = IPARG(45,NG)                                             
        NPG     = IPARG(48,NG)                                             
        IEXPAN  = IPARG(49,NG) 
        ISHPLYXFEM  = IPARG(50,NG)                                  
        IGRE    = IPARG(51,NG)                                            
        JSMS    = IPARG(52,NG)                                            
        INVOL   = IPARG(53,NG)                                            
        IXFEM   = IPARG(54,NG)  ! flag XFEM                                                  
        IEOS    = IPARG(55,NG)                                            
        NPTR    = IPARG(56,NG)                                      
        NPTS    = IPARG(57,NG)                                      
        NPTT    = IPARG(58,NG)                          
        NLAY    = IPARG(59,NG)                                      
        ISVIS   = IPARG(60,NG)  ! viscous stress flag                              
        IVISC   = IPARG(61,NG)  ! /VISC models   flag                              
        IPID    = IPARG(62,NG)  ! property number
        NCTRL   = IPARG(75,NG)  ! Control point number , iso-geometric elements   
        INLOC   = IPARG(78,NG)  ! Flag for non-local regularization
        ISUBSTACK = IPARG(71,NG)
        IBOLTP  = IPARG(72,NG)  ! Bolt preloading flag
        IPINCH    = IPARG(90,NG)  ! Batoz shell pinching flag
        IDRAPE    = IPARG(92,NG)  ! Drape group shell flag
        NINTLAY = 0                                            
cc        NXEL    = 0
c----
        NPAR_TAB  = 10 ! number of integer parameters at top structure level
        NPAR_LAY  = 13 ! number of integer parameters at layer level
        NPAR_LBUF = 2  ! number of integer parameters at LBUF layer
c----
c----
c if xfem, count buffer's length only in the groups with xfem phantom elements  
cc        IF (ICRACK3D > 0 .AND. IXFEM == 0) CYCLE
c----
                                                      
         IF (DEBUG_PRINT == 1) THEN
            print*,'**** GROUP,ITY,IGTYP,MLW=',ng,ity,igtyp,mlw
            print*,'     NPAR_TAB,NPAR_LAY,NPAR_LBUF=',NPAR_TAB,NPAR_LAY,NPAR_LBUF
         ENDIF
c
        BUFLEN = 0                                                              
        NFAIL  = 0                                     
        LFAIL  = 0                                  
        LVISC  = 0                                  
        LMAT   = 0                                  
        LPORO  = 0                                  
        LEOS   = 0                                  
        LIMAT  = 0  
        LIFAIL = 0  
        NUVARN = 0
        CRKDIR = 0
        LNLOC  = 0 
        LNLOCTS= 0      
        
        IF (IXFEM == 0 .AND. FLAG_XFEM == 1) CYCLE ! no "XFEM_TAB" buffer for non xfem PARTS
c
c=======================================================================
c
        IF (ITY == 1 .OR. ITY == 2 .OR. ITY == 51 .OR. ITY == 101) THEN  !
c
c         solid, quad & SPH cell elements
c
c=======================================================================

          TSHELL  = 0
          TSH_ORT = 0
          IF (IGTYP==20 .OR. IGTYP==21 .OR. IGTYP==22 .or. 
     .        ISNOD == 16) TSHELL = 1
          IF (IGTYP==21 .OR. IGTYP==22) TSH_ORT = 1
c
c---------ELEMENT PROPERTY
          IF (ITY == 101) THEN
            NLAY = 1
          ELSEIF (NPT == 1) THEN
            NPTR  = 1                           
            NPTS  = 1                          
            NPTT  = 1 
            IF (JMULT > 0) THEN   ! multimateriau 
              NLAY = 2
            ELSE     
              NLAY = 1
            ENDIF      
C           Multifluid law
            IF (MLW == 151) THEN
               NLAY = IPARG(20, NG)
            ENDIF
          ELSEIF (TSHELL == 1) THEN    ! NLAY >= 1
            NPTS = NPT/100
            NPTT = MOD(NPT/10,10)
            NPTR = MOD(NPT,10)
            IF (JHBE==15) THEN   ! Thick shell HSPH                                                         
              NLAY = NPT
              NPTR = 1
              NPTS = 1
              NPTT = 1
            ELSEIF (JHBE == 14) THEN     !  Thick shell H8
c              plan = (r,s), layers = t    
c              ICS=ICSIG/100
c              ICT=MOD(ICSIG/10,10)
c              ICR=MOD(ICSIG,10)
              IF (ICSTR == 1) THEN   
                 NLAY = NPTR
                 NPTR = NPTS
                 NPTS = NPTT
                 NPTT = 1                 
              ELSEIF (ICSTR == 100) THEN   
                 NLAY = NPTS
                 NPTS = NPTR
                 NPTR = NPTT
                 NPTT = 1
              ELSE
                 NLAY = NPTT
                 NPTT = 1
              ENDIF
              IF (NLAY == 0) THEN
                IPID = IXS(10,NFT+1)                                                
                NLAY = IGEO(15,IPID)
              ENDIF
            ELSEIF (JHBE==16) THEN   ! Thick shell 16 nodes          
              NPTR = NPT/100
              NPTS = MOD(NPT/10,10)
              NPTT = MOD(NPT,10)
c              IF (TSH_ORT == 0) THEN
c                NLAY = NPTT
c                NPTT = 1
c              ELSE
                NLAY = NPTS
                NPTS = 1
c              ENDIF
            ENDIF
c---------
          ELSE  ! Solid property
            NLAY = 1    
            IF (JHBE == 16 .AND. ISNOD == 20) THEN
              NPTR = NPT/100
              NPTS = MOD(NPT/10,10)
              NPTT = MOD(NPT,10)  
            ELSEIF (JHBE == 14) THEN
              NPTS = ABS(NPT)/100         
              NPTT = MOD(ABS(NPT)/10,10)  
              NPTR = MOD(ABS(NPT),10)
            ELSEIF (JHBE == 12) THEN
              NPTR = 1
              NPTS = 1
              NPTT = NPT
              NPG  = NPT
            ELSEIF (JHBE == 17) THEN
              NPTS = 2
              NPTT = 2
              NPTR = 2
            ELSE
              NPTR = NPT
              NPTS = 1
              NPTT = 1
            ENDIF
            IF (N2D>0 .AND.(JHBE==17.OR.JHBE==22)) THEN
              NPTS = 2
              NPTT = 1
              NPTR = 2
              NPG  = NPT
            END IF
          ENDIF
c---------
c
          IPARG(56,NG) = NPTR                              
          IPARG(57,NG) = NPTS                              
          IPARG(58,NG) = NPTT                              
          IPARG(59,NG) = NLAY
          NPT = NPTR*NPTS*NPTT*NLAY
          IPARG(6,NG)  = NPT
        
          MAXFLAY = 7          ! max Nb of failure models per mat
          CALL MY_ALLOC(TFAIL,NLAY,MAXFLAY)
          CALL MY_ALLOC(FAILID,NLAY,MAXFLAY)
          CALL MY_ALLOC(IMAT,NLAY)
          CALL MY_ALLOC(ILAW,NLAY)
          TFAIL(:,:) = 0
          FAILID(:,:) = 0
          ILAW(:)  = 0
          IMAT(:)  = 0
          IF (ITY == 1) THEN
            IMID = IXS(1 ,NFT+1)                                                
            IPID = IXS(10,NFT+1)                                                
          ELSEIF (ITY == 2) THEN 
            IMID = IXQ(1,NFT+1)                                                
            IPID = IXQ(6,NFT+1)                                                
          ELSEIF (ITY == 51) THEN
            IPRT = IPARTSP(NFT+1)
            IMID = IPART(1,IPRT)                                            
            IPID = IPART(2,IPRT)
          ELSEIF (ITY == 101) THEN
            IPRT = IPARTIG3D(NFT+1)
            IMID = IPART(1,IPRT)                                            
            IPID = IPART(2,IPRT)                                         
          ENDIF
c----------------------------------------------------
c       allocation of element buffer sub-structures 
c----------------------------------------------------
          ALLOCATE(ELBUF_TAB(NG)%INTLAY(NINTLAY))
          ALLOCATE(ELBUF_TAB(NG)%BUFLY(NLAY))
          DO IL = 1, NLAY
            BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
            BUFLY%NPTT = NPTT
            ALLOCATE(BUFLY%LBUF(NPTR,NPTS,NPTT))  
            ALLOCATE(BUFLY%MAT (NPTR,NPTS,NPTT))      
            ALLOCATE(BUFLY%FAIL(NPTR,NPTS,NPTT))  
            ALLOCATE(BUFLY%PROP(NPTR,NPTS,NPTT))  
            ALLOCATE(BUFLY%EOS (NPTR,NPTS,NPTT))  
            ALLOCATE(BUFLY%VISC(NPTR,NPTS,NPTT))  
            ALLOCATE(BUFLY%PORO(NPTR,NPTS,NPTT))  
          ENDDO
c          ALLOCATE (ELBUF_TAB(NG)%XFEM(NPTR,NPTS,NPTT) ,STAT=Stat)   
          ELBUF_TAB(NG)%IGTYP = IGTYP                       
          ELBUF_TAB(NG)%NEL   = NEL                                          
          ELBUF_TAB(NG)%NLAY  = NLAY                        
          ELBUF_TAB(NG)%NINTLAY= NINTLAY                        
          ELBUF_TAB(NG)%NPTR  = NPTR                         
          ELBUF_TAB(NG)%NPTS  = NPTS                         
          ELBUF_TAB(NG)%NPTT  = NPTT                         
          ELBUF_TAB(NG)%IXFEM = IXFEM
          ELBUF_TAB(NG)%NXEL  = NXEL
          ELBUF_TAB(NG)%IDRAPE = IDRAPE
c                                  
          NPG = NPTR*NPTS*NPTT         
c--------------------------------------------------------
c       no non-local structure allocation if not needed
c--------------------------------------------------------
          IF (INLOC == 0) THEN 
            ALLOCATE(ELBUF_TAB(NG)%NLOCTS(0,0))
          ENDIF                     
c-------------------------------------------------
          CALL ZEROVARS_AUTO(ELBUF_TAB(NG))
c-------------------------------------------------
          CALL INI_OUTMAX_AUTO(ELBUF_TAB(NG),ITY,NG) 
c
c---  User Mat Buffer, Fail buffer - structure parameters
c
         IF (IGTYP == 22) THEN                         
            DO IL = 1,NLAY                            
              IMAT(IL) = IGEO(100+IL,IPID)                                        
              ILAW(IL) = IPM(2,IMAT(IL)) 
              NUVAR    = MLAW_TAG(IMAT(IL))%NUVAR
              NUVAREOS = 0
              IEOS_TYPE = IPM(4,IMAT(IL))
              IF(IEOS_TYPE>0)NUVAREOS = EOS_TAG(IEOS_TYPE)%NVAR                                
              NVARTMP  = MLAW_TAG(IMAT(IL))%NVARTMP                                
              NFAIL    = IPM(220,IMAT(IL))
              ELBUF_TAB(NG)%BUFLY(IL)%ILAW = ILAW(IL)        
              ELBUF_TAB(NG)%BUFLY(IL)%IMAT = IMAT(IL)        
              ELBUF_TAB(NG)%BUFLY(IL)%IEOS = IEOS        
              ELBUF_TAB(NG)%BUFLY(IL)%IVISC= MAT_PARAM(IMAT(IL))%VISC%ILAW
              ELBUF_TAB(NG)%BUFLY(IL)%IPORO= 0        
              ELBUF_TAB(NG)%BUFLY(IL)%NFAIL= NFAIL  
              ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT  = NUVAR
              ELBUF_TAB(NG)%BUFLY(IL)%NVAR_EOS  = NUVAREOS
              ELBUF_TAB(NG)%BUFLY(IL)%NVARTMP   = NVARTMP
              ELBUF_TAB(NG)%BUFLY(IL)%NVAR_VISC = MAT_PARAM(IMAT(IL))%VISC%NUVAR
              DO J = 1, NFAIL                                            
                IRUPT  = IPM(111 + (J-1)*15,IMAT(IL))   ! modele de rupture
                IDFAIL = IPM(236 + J,IMAT(IL)) 
                TFAIL(IL,J)  = IRUPT 
                FAILID(IL,J) = IDFAIL      
              ENDDO                                                           
              IF (MLAW_TAG(IMAT(IL))%L_PLA == 0 .and. IPARG(10,NG) == 2) THEN
                IPARG(10,NG) = 0  ! ICPRE
              ENDIF                                   
            ENDDO                                       
c---
         ELSEIF (IGTYP > 28 .and. IGTYP < 32) THEN    ! User solid property                       
            NUVAR   = MLAW_TAG(IMID)%NUVAR  
            NUVAREOS = 0
            IEOS_TYPE = IPM(4,IMID)
            IF(IEOS_TYPE>0)NUVAREOS= EOS_TAG(IEOS_TYPE)%NVAR                               
            NVARTMP = MLAW_TAG(IMID)%NVARTMP                                
            ELBUF_TAB(NG)%BUFLY(1)%ILAW = MLW                                          
            ELBUF_TAB(NG)%BUFLY(1)%IMAT = IMID                                        
            ELBUF_TAB(NG)%BUFLY(1)%IEOS = 0                                      
            ELBUF_TAB(NG)%BUFLY(1)%IVISC= 0                                     
            ELBUF_TAB(NG)%BUFLY(1)%IPORO= 0                                           
            ELBUF_TAB(NG)%BUFLY(1)%NFAIL = 0                                     
            ELBUF_TAB(NG)%BUFLY(1)%NVAR_MAT= NUVAR
            ELBUF_TAB(NG)%BUFLY(1)%NVAR_EOS= NUVAREOS
            ELBUF_TAB(NG)%BUFLY(1)%NVARTMP = NVARTMP
            IF (MLAW_TAG(IMID)%L_PLA == 0 .and. IPARG(10,NG) == 2) THEN
              IPARG(10,NG) = 0  ! ICPRE
            ENDIF                                   
c---
          ELSEIF (MLW == 20) THEN      ! Bimat  
            DO IL = 1,2
              ILAW(IL) = IPARG(24+IL,NG)
              NFAIL = IPM(220,NINT(PM(20+IL,IMID))) 
              NUVAREOS = 0
              IEOS_TYPE = IPM(4, NINT(PM(20+IL,IMID)))
              IF(IEOS_TYPE>0)NUVAREOS = EOS_TAG(IEOS_TYPE)%NVAR                           
              ELBUF_TAB(NG)%BUFLY(IL)%ILAW = ILAW(IL)
              ELBUF_TAB(NG)%BUFLY(IL)%IMAT = NINT(PM(20+IL,IMID))
              ELBUF_TAB(NG)%BUFLY(IL)%IEOS = IEOS        
              ELBUF_TAB(NG)%BUFLY(IL)%IVISC= 0
              ELBUF_TAB(NG)%BUFLY(IL)%IPORO= 0        
              ELBUF_TAB(NG)%BUFLY(IL)%NFAIL= 0
              ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT= 0
              ELBUF_TAB(NG)%BUFLY(IL)%NVAR_EOS= NUVAREOS
              ELBUF_TAB(NG)%BUFLY(IL)%NVARTMP = 0
              ELBUF_TAB(NG)%BUFLY(IL)%NVAR_VISC=0
              DO J = 1,NFAIL                                              
                IRUPT  = IPM(111 + (J-1)*15, NINT(PM(20+IL,IMID)))   ! modele de rupture 
                IDFAIL = IPM(236 + J, NINT(PM(20+IL,IMID))) 
                TFAIL(IL,J)  = IRUPT  
                FAILID(IL,J) = IDFAIL    
              ENDDO                                                         
            ENDDO                      
c---
          ELSEIF (MLW == 151) THEN   !  multiphase 151                                    
            DO IL = 1,NLAY
              IMAT(IL) = IPM(20+IL,IMID)   ! submaterial MID                         
              ILAW(IL) = IPM(2,IMAT(IL))  ! submaterial law type
              NUVAR   = MLAW_TAG(IMID)%NUVAR                                
              NVARTMP = MLAW_TAG(IMID)%NVARTMP 
              NUVAREOS = 0  
              IEOS_TYPE = IPM(4,IMAT(IL))                           
              IF(IEOS_TYPE>0)NUVAREOS = EOS_TAG(IEOS_TYPE)%NVAR   
              NFAIL    = IPM(220,IMAT(IL))
              ELBUF_TAB(NG)%BUFLY(IL)%ILAW = ILAW(IL)
              ELBUF_TAB(NG)%BUFLY(IL)%IMAT = IMAT(IL)       
              ELBUF_TAB(NG)%BUFLY(IL)%IEOS = IEOS_TYPE ! submaterial eos type       
              ELBUF_TAB(NG)%BUFLY(IL)%IVISC= MAT_PARAM(IMAT(IL))%VISC%ILAW      
              ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT  = NUVAR
              ELBUF_TAB(NG)%BUFLY(IL)%NVARTMP   = NVARTMP
              ELBUF_TAB(NG)%BUFLY(IL)%NVAR_EOS  = NUVAREOS              
              ELBUF_TAB(NG)%BUFLY(IL)%NVAR_VISC = MAT_PARAM(IMAT(IL))%VISC%NUVAR
              ELBUF_TAB(NG)%BUFLY(IL)%IPORO= 0  
              ELBUF_TAB(NG)%BUFLY(IL)%NFAIL = NFAIL                          
              DO J = 1,NFAIL                                              
                IRUPT  = IPM(111 + (J-1)*15, IMAT(IL))   ! modele de rupture 
                IDFAIL = IPM(236 + J, IMAT(IL)) 
                TFAIL(IL,J)  = IRUPT 
                FAILID(IL,J) = IDFAIL      
              ENDDO                                                         
              IF (MLAW_TAG(IMAT(IL))%L_PLA == 0 .and. IPARG(10,NG) == 2) THEN
                IPARG(10,NG) = 0  ! ICPRE
              ENDIF                                   
            ENDDO 
c---
          ELSEIF (NLAY > 1) THEN   ! thick shells,  multiphase                                     
            DO IL = 1,NLAY
              IMAT(IL) = IMID   ! global MID in all pts                          
              ILAW(IL) = MLW    ! same law type everywhere
              NUVAR   = MLAW_TAG(IMID)%NUVAR                                
              NVARTMP = MLAW_TAG(IMID)%NVARTMP                                
              NFAIL    = IPM(220,IMAT(IL))
              ELBUF_TAB(NG)%BUFLY(IL)%ILAW = MLW
              ELBUF_TAB(NG)%BUFLY(IL)%IMAT = IMID       
              ELBUF_TAB(NG)%BUFLY(IL)%IEOS = IEOS        
              ELBUF_TAB(NG)%BUFLY(IL)%IVISC= MAT_PARAM(IMID)%VISC%ILAW   
              ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT= NUVAR
              ELBUF_TAB(NG)%BUFLY(IL)%NVARTMP = NVARTMP
              ELBUF_TAB(NG)%BUFLY(IL)%NVAR_EOS= 0
              ELBUF_TAB(NG)%BUFLY(IL)%NVAR_VISC=MAT_PARAM(IMID)%VISC%NUVAR
              ELBUF_TAB(NG)%BUFLY(IL)%IPORO= 0  
              ELBUF_TAB(NG)%BUFLY(IL)%NFAIL = NFAIL                          
              DO J = 1,NFAIL                                              
                IRUPT  = IPM(111 + (J-1)*15, IMAT(IL))   ! modele de rupture 
                IDFAIL = IPM(236 + J, IMAT(IL)) 
                TFAIL(IL,J)  = IRUPT 
                FAILID(IL,J) = IDFAIL      
              ENDDO                                                         
              IF (MLAW_TAG(IMAT(IL))%L_PLA == 0 .and. IPARG(10,NG) == 2) THEN
                IPARG(10,NG) = 0  ! ICPRE
              ENDIF                                   
            ENDDO      
C---
            ! Non-local regularization in the shell thickness
            IF (INLOC > 0) THEN 
              ! Allocation of the non-local in thickness structural data
              ALLOCATE(ELBUF_TAB(NG)%NLOCTS(NPTR,NPTS))
              ! Number of non-local nodes in the thickness
              NONL = NLAY+1
              ! Loop over Gauss points in the plane of the shell
              DO IR = 1,ELBUF_TAB(NG)%NPTR
                DO IS = 1,ELBUF_TAB(NG)%NPTS
                  ! Non-local buffer
                  BUFNLTS => ELBUF_TAB(NG)%NLOCTS(IR,IS)
                  ! Non-local in-thickness masses
                  CALL MY_ALLOC(BUFNLTS%MASSTH,NEL,NONL) 
                  BUFNLTS%MASSTH = ZERO
                  LNLOCTS = LNLOCTS + NEL*NONL
                  ! Non-local in-thickness cumulated variables
                  CALL MY_ALLOC(BUFNLTS%UNLTH,NEL,NONL)  
                  BUFNLTS%UNLTH = ZERO
                  LNLOCTS = LNLOCTS + NEL*NONL
                  ! Non-local in-thickness velocities
                  CALL MY_ALLOC(BUFNLTS%VNLTH,NEL,NONL)    
                  BUFNLTS%VNLTH = ZERO
                  LNLOCTS = LNLOCTS + NEL*NONL
                  ! Non-local in-thickness forces
                  CALL MY_ALLOC(BUFNLTS%FNLTH,NEL,NONL)
                  BUFNLTS%FNLTH = ZERO
                  LNLOCTS = LNLOCTS + NEL*NONL
                ENDDO
              ENDDO
            ENDIF
c---
          ELSE  
            NUVAR   = MLAW_TAG(IMID)%NUVAR                                
            NVARTMP = MLAW_TAG(IMID)%NVARTMP 
            NUVAREOS = 0
            IEOS_TYPE = IPM(4,IMID)
            IF(IEOS_TYPE>0)NUVAREOS = EOS_TAG(IEOS_TYPE)%NVAR                                
            NFAIL   = IPM(220,IMID)
            ELBUF_TAB(NG)%BUFLY(1)%ILAW = MLW                                          
            ELBUF_TAB(NG)%BUFLY(1)%IMAT = IMID                                        
            ELBUF_TAB(NG)%BUFLY(1)%IEOS = IEOS                                        
            ELBUF_TAB(NG)%BUFLY(1)%NVAR_MAT= NUVAR
            ELBUF_TAB(NG)%BUFLY(1)%NVARTMP = NVARTMP
            ELBUF_TAB(NG)%BUFLY(1)%NVAR_EOS= NUVAREOS
            ELBUF_TAB(NG)%BUFLY(1)%IVISC   = MAT_PARAM(IMID)%VISC%ILAW                                  
            ELBUF_TAB(NG)%BUFLY(1)%NVAR_VISC = MAT_PARAM(IMID)%VISC%NUVAR
            ELBUF_TAB(NG)%BUFLY(1)%IPORO= 0                                           
            ELBUF_TAB(NG)%BUFLY(1)%NFAIL = NFAIL                                     
            DO J = 1,NFAIL                                                             
              IRUPT  = IPM(111 + (J-1)*15, IMID)   ! modele de rupture
              IDFAIL = IPM(236 + J, IMID)                
              TFAIL(1,J)  = IRUPT
              FAILID(1,J) = IDFAIL                                                       
            ENDDO                                                                     
c---
          ENDIF                                               
c---      User Mat Buffer, Fail buffer, Visc buffer :  alloc + init
          DO IL = 1, NLAY
           NUVARM  = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT
           NVARTMP = ELBUF_TAB(NG)%BUFLY(IL)%NVARTMP
           NUVARV  = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_VISC
           NFAIL   = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL
           NUVAREOS= ELBUF_TAB(NG)%BUFLY(IL)%NVAR_EOS
           DO IR = 1,NPTR                                       
            DO IS = 1,NPTS                                      
              DO IT = 1,NPTT
                ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)%MLAW = ELBUF_TAB(NG)%BUFLY(IL)%ILAW
                ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)%LawID = ELBUF_TAB(NG)%BUFLY(IL)%IMAT
                CALL MY_ALLOC(ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)%VAR,NUVARM*NEL)                
                CALL MY_ALLOC(ELBUF_TAB(NG)%BUFLY(IL)%VISC(IR,IS,IT)%VAR,NUVARV*NEL)                
                CALL MY_ALLOC(ELBUF_TAB(NG)%BUFLY(IL)%EOS(IR,IS,IT)%VAR,NUVAREOS*NEL)                
c
                ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)%VAR(1:NUVARM*NEL) = ZERO
                ELBUF_TAB(NG)%BUFLY(IL)%VISC(IR,IS,IT)%VAR(1:NUVARV*NEL) = ZERO
                ELBUF_TAB(NG)%BUFLY(IL)%EOS(IR,IS,IT)%VAR(1:NUVAREOS*NEL) = ZERO

                LMAT  = LMAT  + NUVARM*NEL
                LVISC = LVISC + NUVARV*NEL
                LEOS  = LEOS  + NUVAREOS*NEL
                
                ALLOCATE(ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(NFAIL))

                DO J = 1,NFAIL
                  FLOC=>ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(J)         
                  IRUPT  = TFAIL(IL,J)  
                  IDFAIL = FAILID(IL,J)   
c
                  FLOC%ILAWF = IRUPT                                           
                  LFAIL = LFAIL + 1 
c
                  FLOC%IDFAIL = IDFAIL
                  LFAIL = LFAIL + 1
c
                  FLOC%NVAR  = FAIL_TAG(IRUPT)%NUVAR                              
                  CALL MY_ALLOC(FLOC%VAR,FLOC%NVAR*NEL)
                  FLOC%VAR = ZERO     
                  LFAIL = LFAIL + FLOC%NVAR*NEL + 1 
c
                  FLOC%LF_DAM  = FAIL_TAG(IRUPT)%LF_DAM                        
                  CALL MY_ALLOC(FLOC%DAM,FLOC%LF_DAM*NEL)
                  FLOC%DAM = ZERO   
                  LFAIL = LFAIL + FLOC%LF_DAM*NEL + 1 
c
                  FLOC%LF_DAMMX  = FAIL_TAG(IRUPT)%LF_DAMMX                        
                  CALL MY_ALLOC(FLOC%DAMMX,FLOC%LF_DAMMX*NEL) 
                  FLOC%DAMMX = ZERO
                  LFAIL = LFAIL + FLOC%LF_DAMMX*NEL + 1 
c
                  FLOC%LF_DAMINI = FAIL_TAG(IRUPT)%LF_DAMINI                        
                  CALL MY_ALLOC(FLOC%DAMINI,FLOC%LF_DAMINI*NEL) 
                  FLOC%DAMINI = ZERO
                  LFAIL = LFAIL + FLOC%LF_DAMINI*NEL + 1 
c
                  FLOC%LF_TDEL  = FAIL_TAG(IRUPT)%LF_TDEL                        
                  CALL MY_ALLOC(FLOC%TDEL,FLOC%LF_TDEL*NEL)
                  FLOC%TDEL = ZERO                                                               
                  LFAIL = LFAIL + FLOC%LF_TDEL*NEL + 1 
c
                  FLOC%LF_INDX = FAIL_TAG(IRUPT)%LF_INDX                       
                  CALL MY_ALLOC(FLOC%INDX,FLOC%LF_INDX*NEL)
                  FLOC%INDX = 0                                                               
                  LFAIL = LFAIL + FLOC%LF_INDX*NEL + 1 
c
                  FLOC%LF_OFF = FAIL_TAG(IRUPT)%LF_OFF                       
                  CALL MY_ALLOC(FLOC%OFF,FLOC%LF_OFF*NEL)
                  FLOC%OFF = 1                                                               
                  LFAIL = LFAIL + FLOC%LF_OFF*NEL + 1 
c
                ENDDO
              ENDDO                                                           
            ENDDO                                                          
           ENDDO 
          ENDDO 
c----------------------------------------------------
          GBUF  => ELBUF_TAB(NG)%GBUF                                  
          IF (IGTYP == 43) THEN       
            ELBUF_TAB(NG)%BUFLY(1)%L_EPSA = 3
            ELBUF_TAB(NG)%BUFLY(1)%L_EPE  = 3
          ENDIF                                                        
          IF (ISMSTR /= 4 .OR. JLAG >0)THEN
           IF (IGTYP == 43) THEN       
             ELBUF_TAB(NG)%BUFLY(1)%L_VOL = 1
           ELSEIF (ITY == 1)THEN                     
            IF (ISNOD == 10 .OR. ISNOD == 16 .OR. ISNOD == 20 ) THEN                     
              GBUF%G_SMSTR = 3*ISNOD
C------for the future small strain option w/ Itetra=1      
            ELSEIF (ISNOD == 4  .AND.ISROT == 1) THEN                     
              GBUF%G_SMSTR = 3*(ISNOD+6)                                   
            ELSE                                                       
              GBUF%G_SMSTR = 3*(ISNOD-1)                               
            ENDIF    
           ENDIF                                                  
          ENDIF 
          IF (ITY == 1.AND.(IGTYP == 20.OR.IGTYP == 21.OR.IGTYP == 22)) GBUF%G_IDT_TSH=1      
C------
          ! Variable for ITET4 = 3
          IF (ITY == 1 .AND. ISNOD == 4 .AND. ISROT == 3 ) THEN
            GBUF%G_AMU = 1
          ENDIF
C------
          IF ((ISMSTR == 10.OR.ISMSTR == 12).AND.ITY == 1)THEN
           IF (ISNOD == 10.OR.(ISNOD==4 .AND. ISROT==1)) THEN 
            GBUF%G_JAC_I = 0    
            ELBUF_TAB(NG)%BUFLY(1)%L_JAC_I=0     
            ELBUF_TAB(NG)%BUFLY(1)%L_PIJ=3*10     
           ELSEIF (ISNOD == 8.AND.JHBE == 17 ) THEN 
            GBUF%G_JAC_I = 9 + 1    
            ELBUF_TAB(NG)%BUFLY(1)%L_JAC_I=0     
            ELBUF_TAB(NG)%BUFLY(1)%L_PIJ=3*ISNOD      
            IF (IINT == 2.AND.ICPRE == 0 ) ELBUF_TAB(NG)%BUFLY(1)%L_PIJ=9*ISNOD 
            IF (IINT == 2) GBUF%G_ETOTSH = 6
           ELSE           
            GBUF%G_JAC_I = 9 + 1    
            ELBUF_TAB(NG)%BUFLY(1)%L_JAC_I=GBUF%G_JAC_I      
c            IF (JHBE == 17) GBUF%G_ETOTSH = 6
           END IF !(ISNOD == 10 ) THEN 
C----- thickshell will be checked later----           
           IF (ISMSTR == 12 .AND. MLW==1 ) ELBUF_TAB(NG)%BUFLY(1)%L_SIGL=6 
          ENDIF
          IF (JTHE      /= 0) GBUF%G_TEMP = 1 
          IF (JTHE      /= 0 .AND. NPG > 1) GBUF%G_TEMPG = NPG 
          IF (JEUL+JALE /= 0 .AND. ISNOD >= 4) GBUF%G_MOM = 3
          IF (JHBE == 15 .OR. JHBE == 24) GBUF%G_HOURG = 12
          IF (JHBE == 15 .AND. ISNOD == 6) GBUF%G_HOURG = 2
          IF (JHBE == 1 .AND. IINT == 3) GBUF%G_HOURG = 12
C------------- for strain output, to 3*6 
          IF (JHBE == 24) GBUF%G_STRHG = 3*6
c           Allocate thermal stress and Thermal internal energy
          IF(IEXPAN > 0) THEN
             GBUF%G_FORTH = 1
             GBUF%G_EINTTH = 1
          ENDIF
          IF (MLW /= 115) GBUF%G_RHO = 1 
          GBUF%G_DT  = 1
          IF (ISMS_SELEC >= 1) GBUF%G_ISMS  = 1
          IF (JCVT>0.AND.ITY == 1) GBUF%G_GAMA_R = 6
c
c         local material variables   
          DO IL = 1, NLAY
            BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
            IF (MLW /= 115) BUFLY%L_RHO = 1 
c
            IF (ISORTH == 1 .AND. ISROT > 0) BUFLY%L_SIGD = 6
            IF ((ISTRA > 0 .AND. MLW /= 0) .OR. ITY == 101)   
     .                 BUFLY%L_STRA = 6                      
            IF (GBUF%G_GAMA > 0 .AND. NLAY > 1) BUFLY%L_GAMA = GBUF%G_GAMA
            IF (JTHE /= 0) BUFLY%L_TEMP = 1
            IF (ISVIS > 0) BUFLY%L_SIGV = 6                      
            IF (IVISC > 0) BUFLY%L_VISC = 6
c           Allocate thermal stress and Thermal internal energy
            IF(IEXPAN > 0) THEN
               BUFLY%L_FORTH = 1
               BUFLY%L_EINTTH = 1
            ENDIF
            IF(IUFACYLD == 1.AND.(MLW==36.OR.MLW==87))BUFLY%L_FAC_YLD = 1
C---- only useful with the law where P=Bulk*AMU(rho,rho0) but not do the distinc here         
            IF(ITY == 1) BUFLY%L_VOL0DP = 1
          ENDDO
C
C         Orthotropic SPH properties
          IF ((ISORTH == 1).AND.(IGTYP == 34)) THEN
            GBUF%G_GAMA = 6
            BUFLY%L_SIGL = 6
          ENDIF
C
c        Bolt Preloading
         GBUF%G_BPRELD = NBPRELD*IBOLTP
         
c        Corotational nodal forces (isolid 19)
         IF (ITY==1 .AND. JHBE==17 .AND. IINT==3) THEN
           GBUF%G_COR_NF = 24
c        Corotational frame  (isolid 19)
           GBUF%G_COR_FR =  9          !3*3                 ! sb Mis en standby pourl'instant
c        Corotational reference coordinates (isolid 19)
           GBUF%G_COR_XR = 21         !7*3
         ENDIF
C                            
c----------------------------------------------------
          CALL INITVARS_AUTO(
     .         ELBUF_TAB(NG),MLAW_TAG ,PROP_TAG ,FAIL_TAG ,
     .         IMID         ,MLW      ,IGTYP    ,JMULT    ,IFAIL  ,
     .         IEOS         ,MAXEOS   ,EOS_TAG  ,ISNOD    )  
c-------------------------------------------------
c       taille du tableau local ELBUF pour restart
c-------------------------------------------------
c         GBUF%NVAR_GLOB ! number of element variables in GBUF 
c         BUFLY%NVAR_LAY ! number of element variables in BUFLY
c         BUFLY%NVAR_LOC ! number of element variables in LBUF (defined on layer level)
c
          BUFLEN = NPAR_TAB + LMAT  + LFAIL + LVISC + LPORO + LEOS + LIMAT + LIFAIL + LNLOCTS
          LGLOB = 0                                 
          LGLOB = LGLOB + GBUF%G_NOFF  
          LGLOB = LGLOB + GBUF%G_IERR  
          LGLOB = LGLOB + GBUF%G_GAMA  
C This array is a double precision array even in SP executable.
C Remode from global buffer writing
c           LGLOB = LGLOB + GBUF%G_SMSTR
          LGLOB = LGLOB + GBUF%G_HOURG 
          LGLOB = LGLOB + GBUF%G_TAG22
          LGLOB = LGLOB + GBUF%G_STRA
          LGLOB = LGLOB + GBUF%G_SIGI
          LGLOB = LGLOB + GBUF%G_DMG
          LGLOB = LGLOB + GBUF%G_DAMDL
          LGLOB = LGLOB + GBUF%G_FOR     
          LGLOB = LGLOB + GBUF%G_MOM     
          LGLOB = LGLOB + GBUF%G_THK
          LGLOB = LGLOB + GBUF%G_STRW 
          LGLOB = LGLOB + GBUF%G_THK_I
          LGLOB = LGLOB + GBUF%G_JAC_I
          LGLOB = LGLOB + GBUF%G_DT
          LGLOB = LGLOB + GBUF%G_ISMS
          LGLOB = LGLOB + GBUF%G_BPRELD
          LGLOB = LGLOB + GBUF%G_COR_NF
          LGLOB = LGLOB + GBUF%G_COR_FR
          LGLOB = LGLOB + GBUF%G_COR_XR
          LGLOB = LGLOB + GBUF%G_MAXFRAC
          LGLOB = LGLOB + GBUF%G_MAXEPS   
          LGLOB = LGLOB + GBUF%G_AMU 

          IF (NPG > 1) THEN
            LGLOB = LGLOB + GBUF%G_FORPG
            LGLOB = LGLOB + GBUF%G_MOMPG
C
            LGLOB = LGLOB + GBUF%G_FORPGPINCH
            LGLOB = LGLOB + GBUF%G_MOMPGPINCH
            LGLOB = LGLOB + GBUF%G_EPGPINCHXZ
            LGLOB = LGLOB + GBUF%G_EPGPINCHYZ
            LGLOB = LGLOB + GBUF%G_EPGPINCHZZ
C
            LGLOB = LGLOB + GBUF%G_STRPG
            LGLOB = LGLOB + GBUF%G_STRWPG 
            LGLOB = LGLOB + GBUF%G_TEMPG 
          ENDIF
C
          IF (NPT /= 1) THEN 
            LGLOB = LGLOB + GBUF%G_BFRAC
            LGLOB = LGLOB + GBUF%G_OFF   
            LGLOB = LGLOB + GBUF%G_EINT  
            LGLOB = LGLOB + GBUF%G_EINS  
            LGLOB = LGLOB + GBUF%G_RHO   
            LGLOB = LGLOB + GBUF%G_QVIS  
            LGLOB = LGLOB + GBUF%G_DELTAX
            LGLOB = LGLOB + GBUF%G_VOL   
            LGLOB = LGLOB + GBUF%G_EPSD  
            LGLOB = LGLOB + GBUF%G_EPSQ
            LGLOB = LGLOB + GBUF%G_PLA   
            LGLOB = LGLOB + GBUF%G_TEMP  
            LGLOB = LGLOB + GBUF%G_TB    
            LGLOB = LGLOB + GBUF%G_RK    
            LGLOB = LGLOB + GBUF%G_RE    
            LGLOB = LGLOB + GBUF%G_SIG   
cc            LGLOB = LGLOB + GBUF%G_THK
            LGLOB = LGLOB + GBUF%G_FORTH
            LGLOB = LGLOB + GBUF%G_EINTTH
            LGLOB = LGLOB + GBUF%G_SEQ
            LGLOB = LGLOB + GBUF%G_ABURN  
            LGLOB = LGLOB + GBUF%G_MU    
          ENDIF
          IF (LFAIL > 0) THEN
            GBUF%G_UELR  = 1
            GBUF%G_UELR1 = 1
            LGLOB = LGLOB + GBUF%G_UELR
            LGLOB = LGLOB + GBUF%G_UELR1
          ENDIF
C
          LGLOB = LGLOB + GBUF%G_AREA
          LGLOB = LGLOB + GBUF%G_SKEW
          LGLOB = LGLOB + GBUF%G_LENGTH
          LGLOB = LGLOB + GBUF%G_TOTDEPL
          LGLOB = LGLOB + GBUF%G_TOTROT
          LGLOB = LGLOB + GBUF%G_FOREP
          LGLOB = LGLOB + GBUF%G_MOMEP
          LGLOB = LGLOB + GBUF%G_DEP_IN_TENS
          LGLOB = LGLOB + GBUF%G_DEP_IN_COMP
          LGLOB = LGLOB + GBUF%G_ROT_IN_TENS
          LGLOB = LGLOB + GBUF%G_ROT_IN_COMP
          LGLOB = LGLOB + GBUF%G_POSX
          LGLOB = LGLOB + GBUF%G_POSY
          LGLOB = LGLOB + GBUF%G_POSZ
          LGLOB = LGLOB + GBUF%G_POSXX
          LGLOB = LGLOB + GBUF%G_POSYY
          LGLOB = LGLOB + GBUF%G_POSZZ
          LGLOB = LGLOB + GBUF%G_YIELD
          LGLOB = LGLOB + GBUF%G_LENGTH_ERR
          LGLOB = LGLOB + GBUF%G_DV
          LGLOB = LGLOB + GBUF%G_DFS
          LGLOB = LGLOB + GBUF%G_SKEW_ERR
          LGLOB = LGLOB + GBUF%G_E6
          LGLOB = LGLOB + GBUF%G_RUPTCRIT
          LGLOB = LGLOB + GBUF%G_MASS
          LGLOB = LGLOB + GBUF%G_V_REPCVT
          LGLOB = LGLOB + GBUF%G_VR_REPCVT
          LGLOB = LGLOB + GBUF%G_NUVAR
          LGLOB = LGLOB + GBUF%G_DEFINI
          LGLOB = LGLOB + GBUF%G_FORINI
          LGLOB = LGLOB + GBUF%G_INIFRIC
          LGLOB = LGLOB + GBUF%G_STRHG
          LGLOB = LGLOB + GBUF%G_DT_PITER
          LGLOB = LGLOB + GBUF%G_IDT_TSH
C
          LGLOB = LGLOB + GBUF%G_FILL
C ---add H3D/TMAX in BUFLEN         
          LGLOB = LGLOB + GBUF%G_TM_YIELD
          LGLOB = LGLOB + GBUF%G_TM_SEQ  
          LGLOB = LGLOB + GBUF%G_TM_EINT 
          LGLOB = LGLOB + GBUF%G_TM_DMG  
          LGLOB = LGLOB + 2*GBUF%G_TM_SIG  
          LGLOB = LGLOB + 2*GBUF%G_TM_STRA

C          
          BUFLEN = BUFLEN + GBUF%NVAR_GLOB + 1 + LGLOB*NEL 
c
c---------   layer variables       
          LLAY = 0
          DO I=1,NLAY
            BUFLY => ELBUF_TAB(NG)%BUFLY(I)
c
            LLAY = LLAY + NPAR_LAY + BUFLY%NVAR_LAY + BUFLY%NVAR_LOC
            LLAY = LLAY + NEL*BUFLY%LY_DMG 
            LLAY = LLAY + NEL*BUFLY%LY_GAMA
            IF(IDRAPE == 0 .OR. (IDRAPE > 0 .AND . IGTYP == 17)) THEN
              LLAY = LLAY + NEL*BUFLY%LY_DIRA 
              LLAY = LLAY + NEL*BUFLY%LY_DIRB 
            ENDIF
            IF (NPG > 1) THEN
              IF (NLAY > 1) THEN
                LEN_PLAPT = NEL*BUFLY%LY_PLAPT
                LEN_SIGPT = NEL*BUFLY%LY_SIGPT
              ELSE
                IF (IGTYP /= 51 .AND. IGTYP /= 52) THEN
                  LEN_PLAPT = NEL*BUFLY%LY_PLAPT*NPT
                  LEN_SIGPT = NEL*BUFLY%LY_SIGPT*NPT
                ELSE
                  LEN_PLAPT = NEL*BUFLY%LY_PLAPT*BUFLY%NPTT
                  LEN_SIGPT = NEL*BUFLY%LY_SIGPT*BUFLY%NPTT
                ENDIF
              ENDIF
cc              LLAY = LLAY + NEL*BUFLY%LY_PLAPT
cc              LLAY = LLAY + NEL*BUFLY%LY_SIGPT
              LLAY = LLAY + LEN_PLAPT
              LLAY = LLAY + LEN_SIGPT
            ENDIF
            LLAY = LLAY + NEL*BUFLY%LY_HOURG
          ENDDO
          BUFLEN = BUFLEN  + LLAY
c---------  local variables        
          DO I=1,NLAY
            BUFLY => ELBUF_TAB(NG)%BUFLY(I)
c           ! tags for local variables : NVAR_LOC = 51
            LLOC = 0                                 
            LLOC = LLOC + BUFLY%L_OFF    
            LLOC = LLOC + BUFLY%L_GAMA   
            LLOC = LLOC + BUFLY%L_STRA 
            LLOC = LLOC + BUFLY%L_FRAC   
            LLOC = LLOC + BUFLY%L_BFRAC
            LLOC = LLOC + BUFLY%L_EINT  
            LLOC = LLOC + BUFLY%L_EINS   
            LLOC = LLOC + BUFLY%L_RHO         
            LLOC = LLOC + BUFLY%L_DP_DRHO     
            LLOC = LLOC + BUFLY%L_QVIS   
            LLOC = LLOC + BUFLY%L_DELTAX 
            LLOC = LLOC + BUFLY%L_VOL   
            LLOC = LLOC + BUFLY%L_EPSA   
            LLOC = LLOC + BUFLY%L_EPSD   
            LLOC = LLOC + BUFLY%L_EPSQ   
            LLOC = LLOC + BUFLY%L_EPSF   
            LLOC = LLOC + BUFLY%L_PLA    
            LLOC = LLOC + BUFLY%L_TEMP       
            LLOC = LLOC + BUFLY%L_TB     
            LLOC = LLOC + BUFLY%L_RK     
            LLOC = LLOC + BUFLY%L_RE     
            LLOC = LLOC + BUFLY%L_VK     
            LLOC = LLOC + BUFLY%L_SF     
            LLOC = LLOC + BUFLY%L_ROB    
            LLOC = LLOC + BUFLY%L_DAM    
            LLOC = LLOC + BUFLY%L_DSUM   
            LLOC = LLOC + BUFLY%L_DGLO     
            LLOC = LLOC + BUFLY%L_CRAK   
            LLOC = LLOC + BUFLY%L_ANG      
            LLOC = LLOC + BUFLY%L_EPE    
            LLOC = LLOC + BUFLY%L_EPC    
            LLOC = LLOC + BUFLY%L_XST    
            LLOC = LLOC + BUFLY%L_SSP    
            LLOC = LLOC + BUFLY%L_Z      
            LLOC = LLOC + BUFLY%L_VISC   
            LLOC = LLOC + BUFLY%L_SIGL   
            LLOC = LLOC + BUFLY%L_SIGV   
            LLOC = LLOC + BUFLY%L_SIGA   
            LLOC = LLOC + BUFLY%L_SIGB   
            LLOC = LLOC + BUFLY%L_SIGC        
            LLOC = LLOC + BUFLY%L_SIGD        
            LLOC = LLOC + BUFLY%L_SIGF   
            LLOC = LLOC + BUFLY%L_SIG    
            LLOC = LLOC + BUFLY%L_FOR    
            LLOC = LLOC + BUFLY%L_MOM
            LLOC = LLOC + BUFLY%L_THK    
            LLOC = LLOC + BUFLY%L_SMSTR    
            LLOC = LLOC + BUFLY%L_DMG  
            LLOC = LLOC + BUFLY%L_FORTH
            LLOC = LLOC + BUFLY%L_EINTTH
            LLOC = LLOC + BUFLY%L_SEQ   
            LLOC = LLOC + BUFLY%L_JAC_I 
            LLOC = LLOC + BUFLY%L_FAC_YLD  
            LLOC = LLOC + BUFLY%L_ABURN
            LLOC = LLOC + BUFLY%L_MU
            LLOC = LLOC + BUFLY%L_PIJ 
c            LLOC = LLOC + BUFLY%L_VOL0DP 
            LLOC = LLOC + BUFLY%L_PLANL
            LLOC = LLOC + BUFLY%L_EPSDNL
            LLOC = LLOC + BUFLY%L_DMGSCL
            LLOC = LLOC + BUFLY%L_TSAIWU
            BUFLEN = BUFLEN + (LLOC * NEL + NPAR_LBUF) * NPG       
          ENDDO
c          print*,'      local=',(LLOC * NEL + NPAR_LBUF) * NPG
c-----------------------------------
          IPARG(66,NG) = BUFLEN    !  size of restart transfer table 
c          print*,' elbuf_ini : buflen=',buflen
c          print*,' elbuf_ini : buflen=',buflen,ITY,NPG
c          print*,'   nlay,nptr,npts,nptt=',nlay,nptr,npts,nptt
c-------------------------------------------------
          CALL ALLOCBUF_AUTO(ELBUF_TAB,NLAY,NPTR,NPTS,NPTT,
     .                   NINTLAY,NEL,NPT,NG,NGROUP,ITY,
     .                   IGTYP)
c-------------------------------------------------
          DEALLOCATE(TFAIL)
          DEALLOCATE(FAILID)
          DEALLOCATE(IMAT)
          DEALLOCATE(ILAW)
c
c=======================================================================
c
        ELSEIF (ITY == 3 .OR. ITY == 7) THEN
c
c         shell elements
c
c=======================================================================
c
          IF (ITY == 3) THEN
            IMID = IXC(1,NFT+1)                                                
            IPID = IXC(6,NFT+1)      
          ELSEIF (ITY == 7) THEN 
            IMID = IXTG(1,NFT+1)                                                
            IPID = IXTG(5,NFT+1)                                       
          ENDIF
          ELBUF_TAB(NG)%GBUF%G_DT = 1
C
          NPTT = 1
          IF (ITY == 3 .and. JHBE == 11) THEN 
            NPTR = 2
            NPTS = 2
          ELSEIF (ITY==7 .and. JHBE == 30) THEN
            NPTR = 3
            NPTS = 1
          ELSE   ! one Gauss point on surface
            NPTR = 1
            NPTS = 1
          ENDIF
          IF (IGTYP == 10 .or. IGTYP == 11 .or. IGTYP == 16 .or. 
     .        IGTYP == 17 .or. IGTYP == 51 .or. IGTYP == 52) THEN
            NLAY = MAX(1,NPT)
            NPTT = 1
          ELSE  ! IGTYP = 1 .or. IGTYP = 9
            NLAY = 1
            NPTT = MAX(1,NPT)    ! attention to global integration 
          ENDIF
          IF (IGTYP == 17 .and. ISHPLYXFEM == 1) NINTLAY = NLAY-1
          IF (ITY == 7 .AND. MLW == 151) THEN
             NLAY = IPARG(20, NG)
          ENDIF
c
!--------------------------------
          IPARG(56,NG) = NPTR                              
          IPARG(57,NG) = NPTS                              
          IPARG(58,NG) = NPTT                              
          IPARG(59,NG) = NLAY
          NPG = NPTR*NPTS
c-------------------------------------------------
          ELBUF_TAB(NG)%IGTYP  = IGTYP                     
          ELBUF_TAB(NG)%NEL    = NEL                                        
          ELBUF_TAB(NG)%NLAY   = NLAY                      
          ELBUF_TAB(NG)%NINTLAY= NINTLAY                        
          ELBUF_TAB(NG)%NPTR   = NPTR                        
          ELBUF_TAB(NG)%NPTS   = NPTS                        
          ELBUF_TAB(NG)%NPTT   = NPTT                        
          ELBUF_TAB(NG)%IXFEM  = IXFEM       
          ELBUF_TAB(NG)%NXEL   = NXEL
          ELBUF_TAB(NG)%IDRAPE = IDRAPE
c-------------------------------------------------
c        print*,'****  JHBE,NPG=',JHBE,NPG
cc          NPT = NPG*NLAY*NPTT
          IF (IGTYP == 51 .OR. IGTYP == 52) THEN
            NPT = NPG*NPT
          ELSE
            NPT = NPG*NLAY*NPTT
          ENDIF
c         NPT may be 0 for PID1 <=> global integration  !
c          NIP_LAY = NPG*NPTT
c          print*,'   nlay,nptr,npts,nptt=',nlay,nptr,npts,nptt
c          print*,'   npt_iparg, nel=',iparg(6,ng), nel
c          print*,'nel,ISTRA, ISMSTR=',nel,ISTRA, ISMSTR
c
          MAXFLAY = 7          ! max Nb of failure models per mat
          CALL MY_ALLOC(TFAIL,NLAY,MAXFLAY)
          CALL MY_ALLOC(FAILID,NLAY,MAXFLAY)
          CALL MY_ALLOC(IMAT,NLAY)
          CALL MY_ALLOC(ILAW,NLAY)
c---------------------------------------------------------
c       no non-local structure allocation if not needed
c---------------------------------------------------------
          IF (INLOC == 0) THEN 
            ALLOCATE(ELBUF_TAB(NG)%NLOC(0,0))
          ENDIF  
c
          TFAIL(:,:) = 0
          FAILID(:,:) = 0
          ILAW(:)    = 0
          IMAT(:)    = 0
c------------------------------------------------------
c         allocation of element buffer sub-structures 
c------------------------------------------------------
          ALLOCATE(ELBUF_TAB(NG)%BUFLY(NLAY))
C---
          IF(IDRAPE > 0) THEN
            IF (IGTYP == 51 .OR. IGTYP == 52 ) THEN
C--- count (and store) total number of integration points through each stack group layer
             IPPID = 2
             IPNPT_LAY = IPPID + 3*NLAY
             DO IL = 1,NLAY
               NPT_LAY = STACK%IGEO(IPNPT_LAY + IL,ISUBSTACK)
               ELBUF_TAB(NG)%BUFLY(IL)%NPTT = NPT_LAY
             ENDDO
           ELSE
              DO IL = 1,NLAY
                ELBUF_TAB(NG)%BUFLY(IL)%NPTT = NPTT
              ENDDO
           ENDIF
         ELSE ! idrape = 0
           IF (IGTYP == 51 ) THEN
C--- count (and store) total number of integration points through each stack group layer
             IPPID = 2
             DO IL = 1,NLAY
               IPIDL = STACK%IGEO(IPPID+IL,ISUBSTACK)
               NPT_LAY = IGEO(4,IPIDL)
               ELBUF_TAB(NG)%BUFLY(IL)%NPTT = NPT_LAY
             ENDDO
           ELSEIF(IGTYP == 52) THEN
             IPPID = 2
             DO IL = 1,NLAY
               IPIDL = STACK%IGEO(IPPID+IL,ISUBSTACK)
               NPT_LAY = IGEO_STACK(4,IPIDL)
               ELBUF_TAB(NG)%BUFLY(IL)%NPTT = NPT_LAY
             ENDDO  
           ELSE
             DO IL = 1,NLAY
               ELBUF_TAB(NG)%BUFLY(IL)%NPTT = NPTT
             ENDDO
           ENDIF   
         ENDIF 
C---
          NPTTOT  = 0
          DO I=1,NLAY
            NPTTOT = NPTTOT + NPG*ELBUF_TAB(NG)%BUFLY(I)%NPTT
          ENDDO
C---
          DO IL = 1, NLAY
            BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
            NPTT = BUFLY%NPTT
            ALLOCATE(BUFLY%LBUF(NPTR,NPTS,NPTT)) 
            ALLOCATE(BUFLY%MAT (NPTR,NPTS,NPTT))      
            ALLOCATE(BUFLY%FAIL(NPTR,NPTS,NPTT))  
            ALLOCATE(BUFLY%PROP(NPTR,NPTS,NPTT))  
            ALLOCATE(BUFLY%EOS (NPTR,NPTS,NPTT))  
            ALLOCATE(BUFLY%VISC(NPTR,NPTS,NPTT))  
            ALLOCATE(BUFLY%PORO(NPTR,NPTS,NPTT))
            IF(IDRAPE > 0 .AND. (IGTYP == 51 .OR. IGTYP == 52)) 
     .         ALLOCATE(ELBUF_TAB(NG)%BUFLY(IL)%LBUF_DIR(NPTT))
          ENDDO
C---
          ! Non-local regularization in the shell thickness
          IF ((INLOC > 0).AND.(NPTT>1)) THEN 
            ! Allocation of the non-local in thickness structural data
            ALLOCATE(ELBUF_TAB(NG)%NLOC(NPTR,NPTS))
            ! Number of non-local nodes in the thickness
            IF (NPTT>2) THEN
              NONL = NPTT+1
            ELSE
              NONL = NPTT
            ENDIF
            ! Loop over Gauss points in the plane of the shell
            DO IR = 1,NPTR
              DO IS = 1,NPTS
                ! Non-local buffer
                BUFNL => ELBUF_TAB(NG)%NLOC(IR,IS)
                ! Non-local in-thickness masses
                CALL MY_ALLOC(BUFNL%MASSTH,NEL,NONL) 
                BUFNL%MASSTH = ZERO
                LNLOC = LNLOC + NEL*NONL
                ! Non-local in-thickness cumulated variables
                CALL MY_ALLOC(BUFNL%UNLTH,NEL,NONL)  
                BUFNL%UNLTH = ZERO
                LNLOC = LNLOC + NEL*NONL
                ! Non-local in-thickness velocities
                CALL MY_ALLOC(BUFNL%VNLTH,NEL,NONL)    
                BUFNL%VNLTH = ZERO
                LNLOC = LNLOC + NEL*NONL
                ! Non-local in-thickness forces
                CALL MY_ALLOC(BUFNL%FNLTH,NEL,NONL)
                BUFNL%FNLTH = ZERO
                LNLOC = LNLOC + NEL*NONL
              ENDDO
            ENDDO
          ENDIF
!-------------------------------------------------
C---
          ALLOCATE(ELBUF_TAB(NG)%INTLAY(NINTLAY))
          DO I = 1, NINTLAY                                      
            INTLAY => ELBUF_TAB(NG)%INTLAY(I)                  
            ALLOCATE(INTLAY%ILBUF(NPTR,NPTS))       
            ALLOCATE(INTLAY%MAT  (NPTR,NPTS)) 
            ALLOCATE(INTLAY%FAIL (NPTR,NPTS))
          ENDDO                                                  
c-------------------------------------------------
          CALL ZEROVARS_AUTO(ELBUF_TAB(NG))
c-------------------------------------------------
          CALL INI_OUTMAX_AUTO(ELBUF_TAB(NG),ITY,NG) 
c
c---  User Mat Buffer, Fail buffer - structure parameters
c
          IF (IGTYP == 11 .or. IGTYP == 16) THEN                         
            DO IL = 1,NLAY
              IMAT(IL) = IGEO(100+IL,IPID)                                        
              ILAW(IL) = IPM(2,IMAT(IL)) 
            ENDDO
          ELSEIF (IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP ==52) THEN 
            IPMAT   = 2 + NLAY    
            DO IL = 1,NLAY
              IMAT(IL)  = STACK%IGEO(IPMAT + IL ,ISUBSTACK)
              ILAW(IL) = IPM(2,IMAT(IL)) 
            ENDDO
          ELSE   ! IGTYP = 1,9,10
            DO IL = 1,NLAY
              IMAT(IL) = IMID                                       
              ILAW(IL) = MLW
            ENDDO
          END IF
c
c-----------------------------------------------          
          IF (MLW == 151) THEN  ! Multifluid law
            DO IL = 1,NLAY
              ILAW(IL) = MLW
              IMAT(IL) = IPM(20+IL,IMID)                                    
              IPM(220,IPM(20+IL,IMID)) = 0
              NUVAREOS = 0
              IEOS_TYPE = IPM(4,IMAT(IL))
              IF(IEOS_TYPE>0)NUVAREOS = EOS_TAG(IEOS_TYPE)%NVAR              
              ELBUF_TAB(NG)%BUFLY(IL)%ILAW = ILAW(IL)
              ELBUF_TAB(NG)%BUFLY(IL)%IMAT = IMAT(IL)
              ELBUF_TAB(NG)%BUFLY(IL)%IEOS = IEOS        
              ELBUF_TAB(NG)%BUFLY(IL)%IVISC= 0
              ELBUF_TAB(NG)%BUFLY(IL)%IPORO= 0        
              ELBUF_TAB(NG)%BUFLY(IL)%NFAIL= 0
              ELBUF_TAB(NG)%BUFLY(IL)%NVAR_VISC = 0
              ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT  = 0
              ELBUF_TAB(NG)%BUFLY(IL)%NVARTMP   = 0
              ELBUF_TAB(NG)%BUFLY(IL)%NVAR_EOS = NUVAREOS
            ENDDO
          ELSE     
            DO IL = 1,NLAY
              NFAIL    = IPM(220,IMAT(IL))
              NUVAREOS = 0
              IEOS_TYPE = IPM(4,IMAT(IL))
              IF(IEOS_TYPE>0)NUVAREOS = EOS_TAG(IEOS_TYPE)%NVAR               
              ELBUF_TAB(NG)%BUFLY(IL)%ILAW  = ILAW(IL)        
              ELBUF_TAB(NG)%BUFLY(IL)%IMAT  = IMAT(IL)
              ELBUF_TAB(NG)%BUFLY(IL)%IEOS  = IEOS        
              ELBUF_TAB(NG)%BUFLY(IL)%IVISC = MAT_PARAM(IMAT(IL))%VISC%ILAW
              ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT  = MLAW_TAG(IMAT(IL))%NUVAR
              ELBUF_TAB(NG)%BUFLY(IL)%NVARTMP   = MLAW_TAG(IMAT(IL))%NVARTMP
              ELBUF_TAB(NG)%BUFLY(IL)%NVAR_VISC = MAT_PARAM(IMAT(IL))%VISC%NUVAR
              ELBUF_TAB(NG)%BUFLY(IL)%NVAR_EOS = NUVAREOS              
              ELBUF_TAB(NG)%BUFLY(IL)%IPORO = 0        
              ELBUF_TAB(NG)%BUFLY(IL)%NFAIL = NFAIL  

              DO J = 1, NFAIL                                            
                IRUPT  = IPM(111 + (J-1)*15,IMAT(IL))   ! modele de rupture 
                IDFAIL = IPM(236 + J,IMAT(IL))
                TFAIL(IL,J)  = IRUPT  
                FAILID(IL,J) = IDFAIL    
              ENDDO                                                           
            ENDDO                                       
          ENDIF       
c--------------
         IF (DEBUG_PRINT == 1) THEN
c            print*,'      nlay,nptr,npts,nptt,npt=',nlay,nptr,npts,nptt,npt
         ENDIF
c--------------
c---      Layer variables
c---      User Mat Buffer, Fail buffer, Visc buffer :  alloc + init
          DO IL = 1, NLAY
           NUVARM = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT
           NUVARV = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_VISC
           NUVAREOS = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_EOS 
           NFAIL  = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL
           NPTT   = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
           DO IR = 1,NPTR                                       
            DO IS = 1,NPTS                                      
              DO IT = 1,NPTT
                ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)%MLAW = ELBUF_TAB(NG)%BUFLY(IL)%ILAW
                ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)%LawID = ELBUF_TAB(NG)%BUFLY(IL)%IMAT
                CALL MY_ALLOC(ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)%VAR,NUVARM*NEL)
                CALL MY_ALLOC(ELBUF_TAB(NG)%BUFLY(IL)%VISC(IR,IS,IT)%VAR,NUVARV*NEL)
                CALL MY_ALLOC(ELBUF_TAB(NG)%BUFLY(IL)%EOS(IR,IS,IT)%VAR,NUVAREOS*NEL)
                ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)%VAR = ZERO
                ELBUF_TAB(NG)%BUFLY(IL)%VISC(IR,IS,IT)%VAR = ZERO
                ELBUF_TAB(NG)%BUFLY(IL)%EOS(IR,IS,IT)%VAR = ZERO
c
                LMAT  = LMAT  + NUVARM*NEL
                LVISC = LVISC + NUVARV*NEL
                LEOS = LEOS + NUVAREOS*NEL

                ALLOCATE(ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(NFAIL))
                DO J = 1,NFAIL
                  FLOC=>ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(J)         
                  IRUPT = TFAIL(IL,J)     
                  FLOC%ILAWF = IRUPT
                  LFAIL = LFAIL + 1 
c
                  IDFAIL = FAILID(IL,J)     
                  FLOC%IDFAIL = IDFAIL
                  LFAIL = LFAIL + 1 
c
                  FLOC%NVAR  = FAIL_TAG(IRUPT)%NUVAR                              
                  CALL MY_ALLOC(FLOC%VAR,FLOC%NVAR*NEL)
                  FLOC%VAR = ZERO     
                  LFAIL = LFAIL + FLOC%NVAR*NEL + 1 
c
                  FLOC%LF_DAM  = FAIL_TAG(IRUPT)%LF_DAM                        
                  CALL MY_ALLOC(FLOC%DAM,FLOC%LF_DAM*NEL)
                  FLOC%DAM = ZERO   
                  LFAIL = LFAIL + FLOC%LF_DAM*NEL + 1 
c
                  FLOC%LF_DAMMX  = FAIL_TAG(IRUPT)%LF_DAMMX                        
                  CALL MY_ALLOC(FLOC%DAMMX,FLOC%LF_DAMMX*NEL) 
                  FLOC%DAMMX = ZERO
                  LFAIL = LFAIL + FLOC%LF_DAMMX*NEL + 1 
c
                  FLOC%LF_DAMINI = FAIL_TAG(IRUPT)%LF_DAMINI                      
                  CALL MY_ALLOC(FLOC%DAMINI,FLOC%LF_DAMINI*NEL) 
                  FLOC%DAMINI = ZERO
                  LFAIL = LFAIL + FLOC%LF_DAMINI*NEL + 1 
c
                  FLOC%LF_TDEL  = FAIL_TAG(IRUPT)%LF_TDEL                        
                  CALL MY_ALLOC(FLOC%TDEL,FLOC%LF_TDEL*NEL)
                  FLOC%TDEL = ZERO                                                               
                  LFAIL = LFAIL + FLOC%LF_TDEL*NEL + 1 
c
                  FLOC%LF_INDX = FAIL_TAG(IRUPT)%LF_INDX                       
                  CALL MY_ALLOC(FLOC%INDX,FLOC%LF_INDX*NEL)
                  FLOC%INDX = 0
                  LFAIL = LFAIL + FLOC%LF_INDX*NEL + 1 
c
                  FLOC%LF_OFF = FAIL_TAG(IRUPT)%LF_OFF                       
                  CALL MY_ALLOC(FLOC%OFF,FLOC%LF_OFF*NEL)
                  FLOC%OFF = 1                                                               
                  LFAIL = LFAIL + FLOC%LF_OFF*NEL + 1 
c
                ENDDO
              ENDDO                                                           
            ENDDO                                                          
           ENDDO 
          ENDDO
c
c---      Inter layer variables
c
          DO I=1,NINTLAY
            DO J=1,MAXFLAY
              TFAIL(I,J)  = 0
              FAILID(I,J) = 0
            ENDDO
            ILAW(I) = 0
            IMAT(I) = 0
          ENDDO
          
          IPMAT_IPLY   = 2 + 2*NLAY
          DO IL = 1,NINTLAY                                                   
            INTLAY  => ELBUF_TAB(NG)%INTLAY(IL)
C old stack IGEO(500+IL,IPID)                                
            IMAT(IL) = STACK%IGEO(IPMAT_IPLY + IL ,ISUBSTACK)                    
            ILAW(IL) = IPM(2,IMAT(IL))                                        
            NFAIL    = IPM(220,IMAT(IL))                                      
            ELBUF_TAB(NG)%INTLAY(IL)%ILAW = ILAW(IL)                          
            ELBUF_TAB(NG)%INTLAY(IL)%IMAT = IMAT(IL)                          
            ELBUF_TAB(NG)%INTLAY(IL)%NFAIL= NFAIL                             
            ELBUF_TAB(NG)%INTLAY(IL)%NVAR_MAT = MLAW_TAG(IMAT(IL))%NUVAR          
            DO J = 1, NFAIL                                                   
              IRUPT  = IPM(111 + (J-1)*15, IMAT(IL))   ! modele de rupture           
              IDFAIL = IPM(236 + J, IMAT(IL))
              TFAIL(IL,J)  = IRUPT 
              FAILID(IL,J) = IDFAIL                                            
            ENDDO                                                               
          ENDDO                                                               
c
          DO IL = 1, NINTLAY                                                       
            NUVARM = ELBUF_TAB(NG)%INTLAY(IL)%NVAR_MAT                            
c            NUVARV = ELBUF_TAB(NG)%INTLAY(IL)%NVAR_VISC                           
            NFAIL = ELBUF_TAB(NG)%INTLAY(IL)%NFAIL                                
            DO IR = 1,NPTR                                                         
              DO IS = 1,NPTS                                                         
                CALL MY_ALLOC(ELBUF_TAB(NG)%INTLAY(IL)%MAT(IR,IS)%VAR,NUVARM*NEL)
                ELBUF_TAB(NG)%INTLAY(IL)%MAT(IR,IS)%VAR = ZERO                 
c                ALLOCATE(ELBUF_TAB(NG)%INTLAY(IL)%VISC(IR,IS)%                 
c     .                   VAR(NUVARV*NEL) ,STAT=Stat)                               
c
                ALLOCATE(ELBUF_TAB(NG)%INTLAY(IL)%FAIL(IR,IS)%FLOC(NFAIL))
                LIMAT  = LIMAT  + NUVARM*NEL                                         
c                LVISC = LVISC + NUVARV*NEL                                         
                DO J = 1,NFAIL                                                     
                  FLOC=>ELBUF_TAB(NG)%INTLAY(IL)%FAIL(IR,IS)%FLOC(J)           
                  IRUPT = TFAIL(IL,J)                                              
                  FLOC%ILAWF = IRUPT  
                  IDFAIL = FAILID(IL,J)
                  FLOC%IDFAIL = IDFAIL                                             
                  FLOC%NVAR  = FAIL_TAG(IRUPT)%NUVAR                                
                  CALL MY_ALLOC(FLOC%VAR,FLOC%NVAR*NEL)
                  FLOC%VAR = ZERO                                                   
                  LIFAIL = LIFAIL + FLOC%NVAR*NEL + 2                                
                ENDDO                                                              
              ENDDO                                                                  
            ENDDO                                                                   
            IF (DEBUG_PRINT == 1) THEN
              print*,''
              print*,'    INTLAY ',IL
              print*,'      LIMAT,LIFAIL = ',LIMAT,LIFAIL
            ENDIF          
          ENDDO                                                                    
c----------------------------------
          IF (DEBUG_PRINT == 1) THEN
            print*,''
            print*,'      sum local variables mat,visc,fail on all layers'
            print*,'      LMAT,LVISC,LFAIL,LEOS = ',LMAT,LVISC,LFAIL,LEOS
          ENDIF          
c----------------------------------------------------
          GBUF  => ELBUF_TAB(NG)%GBUF      
          IF (JTHE /= 0     ) GBUF%G_TEMP = 1
          IF (JTHE      /= 0 .AND. NPG > 1) GBUF%G_TEMPG = NPG
          IF (ISMS_SELEC >= 1) GBUF%G_ISMS  = 1
          IF (JEUL+JALE /= 0 .AND. ISNOD >= 4) GBUF%G_MOM  = 3
          IF (ITY == 3) THEN
            IF (JHBE <= 4) THEN
              GBUF%G_HOURG = 5
            ELSEIF (JHBE == 23) THEN
              GBUF%G_HOURG = 12
            ENDIF
            IF (ISROT > 0) GBUF%G_HOURG = GBUF%G_HOURG + 13
            IF (ISMSTR == 10) THEN
C----------- -----            
             GBUF%G_SMSTR = 9 
            ELSEIF (ISMSTR /= 4) THEN
             GBUF%G_SMSTR = 6
            END IF
cc          

          ELSEIF (ITY == 7) THEN
            IF (JHBE >= 30) THEN  ! dkt6
              IF (ISMSTR /= 4) GBUF%G_SMSTR = 13
            ELSEIF (ISMSTR == 10) THEN  
              GBUF%G_SMSTR = 6 
            ELSEIF (ISMSTR /= 4) THEN
C------------- 3 is sufficient !!!!           
              GBUF%G_SMSTR = 6
            ENDIF
            IF (JHBE < 30 .and. ISROT > 0) GBUF%G_HOURG = 5
          ENDIF
c---------for accumulate WXY
          IF (ISMSTR == 10 )THEN
            GBUF%G_STRW = 1
            GBUF%G_STRWPG  = GBUF%G_STRW*NPG
            GBUF%G_STRA = 8
            GBUF%G_STRPG = GBUF%G_STRA*NPG
          ELSEIF (ISTRA > 0   .OR. IFAIL > 0 .OR.
     .       ISMSTR == 11 .OR.(ISMSTR==1 .AND. MLW==19)) THEN
            GBUF%G_STRA = 8
          ENDIF
          IF (ITY == 3 .AND. JHBE == 23 .AND. ISTR_24 > 0) GBUF%G_STRPG = 4*GBUF%G_STRA
c           Allocate thermal stress and Thermal internal energy
          IF(IEXPAN > 0) THEN
             GBUF%G_FORTH = 2
             GBUF%G_EINTTH = 1
          ENDIF              
c
c         local material variables   
          DO IL = 1, NLAY
            BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
            IF (ISORTH == 1 .AND. ISROT > 0) BUFLY%L_SIGD = 6
            IF (GBUF%G_GAMA > 0 .AND. NLAY > 1) BUFLY%L_GAMA = GBUF%G_GAMA
            IF (IREP == 1 ) THEN
              BUFLY%LY_DIRA = 2
            ELSEIF (IREP >= 2) THEN
              BUFLY%LY_DIRA = 2
              BUFLY%LY_DIRB = 2
            ENDIF
            IF (JTHE /= 0) BUFLY%L_TEMP = 1
            IF (ISVIS > 0) BUFLY%L_SIGV = 5                      
            IF (BUFLY%IVISC > 0) BUFLY%L_VISC = 5
            IF (BUFLY%IVISC > 0 .and. MLW == 25) BUFLY%L_SIGD = 6
            IF (MLW == 27) BUFLY%L_DMG  = 2
            IF (MLW == 2)  BUFLY%L_SIGB  = 3
            IF (IXFEM > 0) THEN
              IF (ISTRA > 0 .AND. MLW /= 0 .AND. NLAY > 1) BUFLY%L_STRA = 8
              BUFLY%L_SMSTR = 6
              BUFLY%L_THK = 1
              BUFLY%LY_HOURG = GBUF%G_HOURG
              IF (IGTYP == 11 .or. IGTYP == 51 .or. IGTYP == 52) THEN
                BUFLY%L_FOR = 5
                BUFLY%L_MOM = 3
              ENDIF
              IF (LFAIL > 0) THEN
                BUFLY%LY_UELR  = 1
                BUFLY%LY_UELR1 = 1
              ENDIF
            ENDIF
c           Allocate thermal stress and Thermal internal energy
            IF (IEXPAN > 0) THEN
               BUFLY%L_FORTH = 2
               BUFLY%L_EINTTH = 1
            ENDIF
            IF(IUFACYLD == 1.AND.(MLW==36.OR.MLW==87))BUFLY%L_FAC_YLD = 1

            IF (LFAIL > 0) THEN
              BUFLY%L_OFF = 1
              BUFLY%LY_OFFPG = NPG
              BUFLY%LY_OFF   = 1
            ENDIF
          ENDDO   

c           Allocate Orthotropic Beta angle (dyna input format)
          IF((ITY==3.OR.ITY==7).AND.IGTYP /= 1) THEN
             GBUF%G_BETAORTH = 1
          ENDIF                   
c----------------------------------------------------
          CALL INITVARS_AUTO(
     .         ELBUF_TAB(NG),MLAW_TAG ,PROP_TAG ,FAIL_TAG ,
     .         IMID         ,MLW      ,IGTYP    ,JMULT    ,IFAIL  ,
     .         IEOS         ,MAXEOS   ,EOS_TAG  ,ISNOD    )  
c-------------------------------------------------
!---
!          !  equivalent stress / criterion - other then VON MISES
!          IF( GBUF%G_SEQ > 0) THEN
!            DO IL = 1, NLAY
!              BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
!              LAW = BUFLY%ILAW
!              IF (LAW == 10 .OR. LAW == 12 .OR. LAW == 14 .OR.
!     .            LAW == 15 .OR. LAW == 21 .OR. LAW == 24 .OR.
!     .            LAW == 25 .OR. LAW == 32 .OR. LAW == 43 .OR.
!     .            LAW == 53 .OR. LAW == 57 .OR. LAW == 72 .OR.
!     .            LAW == 73 .OR. LAW == 74 .OR. LAW == 81 .OR.
!     .            LAW == 93) THEN
!                BUFLY%L_SEQ = 1
!              ENDIF
!            ENDDO
!          ENDIF
!---
c-------------------------------------------------
c       taille du tableau local ELBUF pour restart
c-------------------------------------------------
c         GBUF%NVAR_GLOB ! number of element variables in GBUF 
c         BUFLY%NVAR_LOC ! number of element variables in LBUF (defined on layer level)
c          
          BUFLEN = NPAR_TAB + LMAT  + LFAIL + LVISC + LPORO + LEOS + LIMAT + LIFAIL + LNLOC
c-------------------------------------------------
         IF (DEBUG_PRINT == 1) THEN
           print*,'       BUFLEN=NPAR_TAB+LMAT+LFAIL+LVISC+LPORO+LEOS+LIMAT+LIFAIL+LNLOC',buflen             
         ENDIF
c-------------------------------------------------
          IF (LFAIL > 0) THEN
            GBUF%G_UELR  = 1
            GBUF%G_UELR1 = 1
            DO IL=1,NLAY
              BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
              NFAIL = BUFLY%NFAIL
              DO J=1,NFAIL
                IRUPT = TFAIL(IL,J)
                BUFLY%LY_CRKDIR = MAX(FAIL_TAG(IRUPT)%LF_DIR,BUFLY%LY_CRKDIR)
                BUFLY%LY_DMG  = MAX(FAIL_TAG(IRUPT)%LF_DAM,BUFLY%LY_DMG)
              ENDDO
            ENDDO
          ENDIF
          IF (MLW /= 151) THEN
             GBUF%G_RHO = 0
          ENDIF
C
          LGLOB = 0                                 
          LGLOB = LGLOB + GBUF%G_NOFF  
          LGLOB = LGLOB + GBUF%G_IERR  
          LGLOB = LGLOB + GBUF%G_GAMA  
C This array is a double precision array even in SP executable.
C Remode from global buffer writing
c           LGLOB = LGLOB + GBUF%G_SMSTR
          LGLOB = LGLOB + GBUF%G_HOURG 
          LGLOB = LGLOB + GBUF%G_TAG22
          LGLOB = LGLOB + GBUF%G_STRA
          LGLOB = LGLOB + GBUF%G_SIGI
          LGLOB = LGLOB + GBUF%G_DMG
          LGLOB = LGLOB + GBUF%G_DAMDL  
          LGLOB = LGLOB + GBUF%G_FOR     
          LGLOB = LGLOB + GBUF%G_MOM     
          LGLOB = LGLOB + GBUF%G_THK
          LGLOB = LGLOB + GBUF%G_DT
          LGLOB = LGLOB + GBUF%G_ISMS
C-------G_THK_I =  G_THK          
          LGLOB = LGLOB + GBUF%G_STRW 
          GBUF%G_THK_I = GBUF%G_THK
          LGLOB = LGLOB + GBUF%G_THK_I
          LGLOB = LGLOB + GBUF%G_MAXFRAC
          LGLOB = LGLOB + GBUF%G_MAXEPS    
          IF (NPG > 1) THEN
            LGLOB = LGLOB + GBUF%G_FORPG
            LGLOB = LGLOB + GBUF%G_MOMPG
C
            LGLOB = LGLOB + GBUF%G_FORPGPINCH
            LGLOB = LGLOB + GBUF%G_MOMPGPINCH
            LGLOB = LGLOB + GBUF%G_EPGPINCHXZ
            LGLOB = LGLOB + GBUF%G_EPGPINCHYZ
            LGLOB = LGLOB + GBUF%G_EPGPINCHZZ
C
            LGLOB = LGLOB + GBUF%G_STRPG
            LGLOB = LGLOB + GBUF%G_STRWPG
            LGLOB = LGLOB + GBUF%G_TEMPG
          ELSEIF (GBUF%G_STRPG > GBUF%G_STRA) THEN
            LGLOB = LGLOB + GBUF%G_STRPG
          ENDIF
c
          IF (LFAIL > 0) THEN
            LGLOB = LGLOB + GBUF%G_UELR
            LGLOB = LGLOB + GBUF%G_UELR1
          ENDIF
C
          IF (NPTTOT /= 1) THEN 
            LGLOB = LGLOB + GBUF%G_BFRAC    
            LGLOB = LGLOB + GBUF%G_OFF   
            LGLOB = LGLOB + GBUF%G_EINT  
            LGLOB = LGLOB + GBUF%G_EINS  
            LGLOB = LGLOB + GBUF%G_RHO   
            LGLOB = LGLOB + GBUF%G_QVIS  
            LGLOB = LGLOB + GBUF%G_DELTAX
            LGLOB = LGLOB + GBUF%G_VOL   
            LGLOB = LGLOB + GBUF%G_EPSD  
            LGLOB = LGLOB + GBUF%G_EPSQ  
            LGLOB = LGLOB + GBUF%G_PLA   
            LGLOB = LGLOB + GBUF%G_TEMP  
            LGLOB = LGLOB + GBUF%G_TB    
            LGLOB = LGLOB + GBUF%G_RK    
            LGLOB = LGLOB + GBUF%G_RE    
            LGLOB = LGLOB + GBUF%G_SIG   
            LGLOB = LGLOB + GBUF%G_FORTH   
            LGLOB = LGLOB + GBUF%G_EINTTH
            LGLOB = LGLOB + GBUF%G_SEQ
            LGLOB = LGLOB + GBUF%G_ABURN
            LGLOB = LGLOB + GBUF%G_MU
          ENDIF   
C
          LGLOB = LGLOB + GBUF%G_AREA
          LGLOB = LGLOB + GBUF%G_SKEW
          LGLOB = LGLOB + GBUF%G_LENGTH
          LGLOB = LGLOB + GBUF%G_TOTDEPL
          LGLOB = LGLOB + GBUF%G_TOTROT
          LGLOB = LGLOB + GBUF%G_FOREP
          LGLOB = LGLOB + GBUF%G_MOMEP
          LGLOB = LGLOB + GBUF%G_DEP_IN_TENS
          LGLOB = LGLOB + GBUF%G_DEP_IN_COMP
          LGLOB = LGLOB + GBUF%G_ROT_IN_TENS
          LGLOB = LGLOB + GBUF%G_ROT_IN_COMP
          LGLOB = LGLOB + GBUF%G_POSX
          LGLOB = LGLOB + GBUF%G_POSY
          LGLOB = LGLOB + GBUF%G_POSZ
          LGLOB = LGLOB + GBUF%G_POSXX
          LGLOB = LGLOB + GBUF%G_POSYY
          LGLOB = LGLOB + GBUF%G_POSZZ
          LGLOB = LGLOB + GBUF%G_YIELD
          LGLOB = LGLOB + GBUF%G_LENGTH_ERR
          LGLOB = LGLOB + GBUF%G_DV
          LGLOB = LGLOB + GBUF%G_DFS
          LGLOB = LGLOB + GBUF%G_SKEW_ERR
          LGLOB = LGLOB + GBUF%G_E6
          LGLOB = LGLOB + GBUF%G_RUPTCRIT
          LGLOB = LGLOB + GBUF%G_MASS
          LGLOB = LGLOB + GBUF%G_V_REPCVT
          LGLOB = LGLOB + GBUF%G_VR_REPCVT
          LGLOB = LGLOB + GBUF%G_NUVAR
          LGLOB = LGLOB + GBUF%G_DEFINI
          LGLOB = LGLOB + GBUF%G_FORINI
          LGLOB = LGLOB + GBUF%G_INIFRIC
C
          LGLOB = LGLOB + GBUF%G_SLIPRING_ID
          LGLOB = LGLOB + GBUF%G_SLIPRING_FRAM_ID
          LGLOB = LGLOB + GBUF%G_INTVAR
          LGLOB = LGLOB + GBUF%G_ADD_NODE
          LGLOB = LGLOB + GBUF%G_UPDATE
C
          LGLOB = LGLOB + GBUF%G_FILL
          LGLOB = LGLOB + GBUF%G_BETAORTH            
C ---add H3D/TMAX in BUFLEN         
          LGLOB = LGLOB + GBUF%G_TM_YIELD
          LGLOB = LGLOB + GBUF%G_TM_SEQ  
          LGLOB = LGLOB + GBUF%G_TM_EINT 
          LGLOB = LGLOB + GBUF%G_TM_DMG  
          LGLOB = LGLOB + 2*GBUF%G_TM_SIG  
          LGLOB = LGLOB + 2*GBUF%G_TM_STRA 
C
c
          BUFLEN = BUFLEN + GBUF%NVAR_GLOB + 1 + LGLOB*NEL
c
c--------- 
        IF (DEBUG_PRINT == 1) THEN       
        print*,'    LGLOB, glob vars= ',LGLOB,LGLOB*NEL               
        print*,'     total glob: ',  GBUF%NVAR_GLOB + 1 + LGLOB*NEL            
        print*,'  buflen=BUFLEN + GBUF%NVAR_GLOB + 1 + LGLOB*NEL',buflen
        print*,' NPAR_LAY+NVAR_LAY,NVAR_LOC=',NPAR_LAY+BUFLY%NVAR_LAY+BUFLY%NVAR_LOC
        ENDIF
c---------          
c
          LLAY = 0
          IAD  = 0
          DO I=1,NLAY
            BUFLY => ELBUF_TAB(NG)%BUFLY(I)
            LLAY = LLAY + NPAR_LAY + BUFLY%NVAR_LAY + BUFLY%NVAR_LOC
            LLAY = LLAY + NEL*BUFLY%LY_DMG 
            LLAY = LLAY + NEL*BUFLY%LY_GAMA
            IF(IDRAPE == 0 .OR. (IDRAPE > 0 .AND. IGTYP == 17)) THEN
              LLAY = LLAY + NEL*BUFLY%LY_DIRA
              LLAY = LLAY + NEL*BUFLY%LY_DIRB
            ENDIF  
            IF (NPG > 1) THEN
              IF (NLAY > 1) THEN
                LEN_PLAPT = NEL*BUFLY%LY_PLAPT
                LEN_SIGPT = NEL*BUFLY%LY_SIGPT
              ELSE
                IF (IGTYP /= 51 .AND. IGTYP /= 52) THEN
                  LEN_PLAPT = NEL*BUFLY%LY_PLAPT*NPT
                  LEN_SIGPT = NEL*BUFLY%LY_SIGPT*NPT
                ELSE
                  LEN_PLAPT = NEL*BUFLY%LY_PLAPT*BUFLY%NPTT
                  LEN_SIGPT = NEL*BUFLY%LY_SIGPT*BUFLY%NPTT
                ENDIF
              ENDIF
cc              LLAY = LLAY + NEL*BUFLY%LY_PLAPT
cc              LLAY = LLAY + NEL*BUFLY%LY_SIGPT
              LLAY = LLAY + LEN_PLAPT
              LLAY = LLAY + LEN_SIGPT
            ENDIF   
            LLAY = LLAY + NEL*BUFLY%LY_HOURG
            IF (LFAIL > 0) THEN
              LLAY = LLAY + NEL*BUFLY%LY_UELR
              LLAY = LLAY + NEL*BUFLY%LY_UELR1
              LLAY = LLAY + NEL*BUFLY%LY_CRKDIR
              LLAY = LLAY + NEL*BUFLY%LY_OFFPG
              LLAY = LLAY + NEL*BUFLY%LY_OFF
            ENDIF
              IF (DEBUG_PRINT == 1) THEN
                 print*,'  **  LAYER=',IL
                 print*,'    vars NEL*LY_xxx, buflen =: ',  LLAY - IAD , buflen                           
                 IAD = LLAY
              ENDIF
          ENDDO
c
          BUFLEN = BUFLEN  + LLAY
c
c---------          
          IF (DEBUG_PRINT == 1) THEN
            print*,'    total vars LY_xxx, buflen =: ',  LLAY , buflen                           
          ENDIF
c---------          
c---------          
          DO IL=1,NLAY
            BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
c           ! tags for local variables : NVAR_LOC
            NIP_LAY = NPG*BUFLY%NPTT
c
            LLOC = 0                                 
            LLOC = LLOC + BUFLY%L_OFF    
            LLOC = LLOC + BUFLY%L_GAMA 
c
            IF(IXFEM == 0) BUFLY%L_STRA  = 0     ! not used for shell elements
            LLOC = LLOC + BUFLY%L_STRA   
c            
            LLOC = LLOC + BUFLY%L_FRAC   
            LLOC = LLOC + BUFLY%L_BFRAC
            LLOC = LLOC + BUFLY%L_EINT   
            LLOC = LLOC + BUFLY%L_EINS 
            IF (MLW /= 151) THEN
               BUFLY%L_RHO = 0
            ENDIF
            LLOC = LLOC + BUFLY%L_RHO         
            LLOC = LLOC + BUFLY%L_DP_DRHO     
            LLOC = LLOC + BUFLY%L_QVIS   
            LLOC = LLOC + BUFLY%L_DELTAX 
            LLOC = LLOC + BUFLY%L_VOL    
            LLOC = LLOC + BUFLY%L_EPSA   
            LLOC = LLOC + BUFLY%L_EPSD   
            LLOC = LLOC + BUFLY%L_EPSQ   
            LLOC = LLOC + BUFLY%L_EPSF   
            LLOC = LLOC + BUFLY%L_PLA    
            LLOC = LLOC + BUFLY%L_TEMP 
            LLOC = LLOC + BUFLY%L_TB     
            LLOC = LLOC + BUFLY%L_RK     
            LLOC = LLOC + BUFLY%L_RE     
            LLOC = LLOC + BUFLY%L_VK     
            LLOC = LLOC + BUFLY%L_SF     
            LLOC = LLOC + BUFLY%L_ROB    
            LLOC = LLOC + BUFLY%L_DAM    
            LLOC = LLOC + BUFLY%L_DSUM   
            LLOC = LLOC + BUFLY%L_DGLO     
            LLOC = LLOC + BUFLY%L_CRAK   
            LLOC = LLOC + BUFLY%L_ANG      
            LLOC = LLOC + BUFLY%L_EPE    
            LLOC = LLOC + BUFLY%L_EPC    
            LLOC = LLOC + BUFLY%L_XST    
            LLOC = LLOC + BUFLY%L_SSP    
            LLOC = LLOC + BUFLY%L_Z      
            LLOC = LLOC + BUFLY%L_VISC   
            LLOC = LLOC + BUFLY%L_SIGL   
            LLOC = LLOC + BUFLY%L_SIGV   
            LLOC = LLOC + BUFLY%L_SIGA   
            LLOC = LLOC + BUFLY%L_SIGB   
            LLOC = LLOC + BUFLY%L_SIGC        
            LLOC = LLOC + BUFLY%L_SIGD        
            LLOC = LLOC + BUFLY%L_SIGF   
            LLOC = LLOC + BUFLY%L_SIG
            LLOC = LLOC + BUFLY%L_SIGPLY
            LLOC = LLOC + BUFLY%L_FOR    
            LLOC = LLOC + BUFLY%L_MOM
            LLOC = LLOC + BUFLY%L_THK    
            LLOC = LLOC + BUFLY%L_SMSTR    
            LLOC = LLOC + BUFLY%L_DMG  
            LLOC = LLOC + BUFLY%L_FORTH
            LLOC = LLOC + BUFLY%L_EINTTH
            LLOC = LLOC + BUFLY%L_SEQ
            LLOC = LLOC + BUFLY%L_FAC_YLD 
            LLOC = LLOC + BUFLY%L_ABURN 
            LLOC = LLOC + BUFLY%L_MU 
            LLOC = LLOC + BUFLY%L_PLANL
            LLOC = LLOC + BUFLY%L_EPSDNL
            LLOC = LLOC + BUFLY%L_DMGSCL
            LLOC = LLOC + BUFLY%L_TSAIWU
c
            BUFLEN = BUFLEN + (LLOC * NEL + NPAR_LBUF) * NIP_LAY 
            IF(IDRAPE > 0 .AND. (IGTYP == 51 .OR. IGTYP == 52)) THEN
               LLOC_SLICE = BUFLY%LY_DIRA + BUFLY%LY_DIRB
               BUFLEN = BUFLEN + LLOC_SLICE*NEL*BUFLY%NPTT
            ENDIF
            IF (DEBUG_PRINT == 1) THEN
               print*,'  **  variables locales LAYER=',IL
               print*,'     (LLOC*NEL+NPAR_LBUF)*NIP_LAY=',(LLOC*NEL+NPAR_LBUF)*NIP_LAY 
            ENDIF
c          
          ENDDO
c---------          
c         INTERLAYER
c---------         
          LINTLOC = 6  !  (eps=3,  sig=3)                                
          LINTLAY = 2  !  (eint=1, count=1)
          LINTPAR = 5  ! ILAW + IMAT + NFAIL + NVAR_MAT + NVARTMP
          DO IL=1,NINTLAY
            BUFLEN = BUFLEN + LINTPAR
            BUFLEN = BUFLEN + (LINTLAY * NEL)
            BUFLEN = BUFLEN + (LINTLOC * NEL) * NPTR*NPTS

            IF (DEBUG_PRINT == 1) THEN
               print*,'  **  INTERLAYER=',IL
               print*,'    LEN=',(LINTLOC * NEL)*NPTR*NPTS +(LINTLAY * NEL)+lintpar  
            ENDIF
          ENDDO
          IPARG(66,NG) = BUFLEN    !  size of restart transfer table 
 
          IF(PRINT_FLAG>6) WRITE(IOUT,1002) NG, BUFLEN
c
c-------------------------------------------------
c          print*,' elbuf_ini : NG, buflen=',NG,buflen,ITY,IGTYP
c-------------------------------------------------
          CALL ALLOCBUF_AUTO(ELBUF_TAB,NLAY,NPTR,NPTS,NPTT,
     .                       NINTLAY,NEL,NPT,NG,NGROUP,ITY,
     .                       IGTYP)
c-------------------------------------------------
          DEALLOCATE(TFAIL)
          DEALLOCATE(FAILID)
          DEALLOCATE(IMAT)
          DEALLOCATE(ILAW)
c
c=======================================================================
c
        ELSEIF (ITY == 4 .OR. ITY == 5 .OR. ITY == 6 .OR. ITY == 100) THEN 
c
c         1-D elements (TRUSS, BEAM, SPRING)
c
c=======================================================================
          IF (ITY == 4) THEN      ! TRUSS
            IMID = IXT(1,NFT+1)
            IPID = IXT(4,NFT+1)
          ELSEIF (ITY == 5) THEN  ! BEAM
            IMID = IXP(1,NFT+1)     
            IPID = IXP(5,NFT+1)
          ELSEIF (ITY == 6) THEN  ! SPRING
            IMID = IXR(5,NFT+1)
            IPID = IXR(1,NFT+1)
          ELSEIF (ITY == 100) THEN  ! MULTIBRIN
            IMID = 0
            IPID = KXX(2,NFT+1)
          ENDIF
          IGTYP = IGEO(11,IPID)
C----
          GBUF  => ELBUF_TAB(NG)%GBUF 
C---
          GBUF%G_DT = 1
C
          NPTT = 0
          NPTR = 0
          NPTS = 0
          NLAY = 0
          NPG  = 0
C
          IF (IGTYP == 18) THEN  ! integrated beams
            NLAY = 1
            NPTR = 1
            NPTS = 1
            NPTT = NPT
          ENDIF
C
          IPARG(56,NG) = NPTR                              
          IPARG(57,NG) = NPTS                              
          IPARG(58,NG) = NPTT                              
          IPARG(59,NG) = NLAY
c-------------------------------------------------
          ELBUF_TAB(NG)%IGTYP  = IGTYP                     
          ELBUF_TAB(NG)%NEL    = NEL                                        
          ELBUF_TAB(NG)%NLAY   = NLAY
          ELBUF_TAB(NG)%NINTLAY= 0
          ELBUF_TAB(NG)%NPTR   = NPTR
          ELBUF_TAB(NG)%NPTS   = NPTS
          ELBUF_TAB(NG)%NPTT   = NPTT
          ELBUF_TAB(NG)%IXFEM  = 0
          ELBUF_TAB(NG)%NXEL   = 0
          ELBUF_TAB(NG)%IDRAPE = 0
c-------------------------------------------------
          CALL MY_ALLOC(IMAT,NLAY)
          CALL MY_ALLOC(ILAW,NLAY)
          ILAW(:) = 0
          IMAT(:) = 0
C
          LMAT = 0
          LEOS = 0
C
          IF (NLAY > 0) THEN  ! integrated beam (NLAY = 1)
            ALLOCATE(ELBUF_TAB(NG)%BUFLY(NLAY))
C---
            DO IL = 1,NLAY
              BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
              BUFLY%NPTT = NPTT
              ALLOCATE(BUFLY%LBUF(NPTR,NPTS,NPTT))  
              ALLOCATE(BUFLY%MAT (NPTR,NPTS,NPTT))
              ALLOCATE(BUFLY%EOS (NPTR,NPTS,NPTT))       
            ENDDO
C---
c
c---  User Mat Buffer - structure parameters
c
            DO IL = 1,NLAY
              IMAT(IL) = IMID                                                      
              ILAW(IL) = MLW
              NFAIL    = IPM(220,IMAT(IL))                                      
              NUVAREOS = 0
              IEOS_TYPE = IPM(4,IMAT(IL))
              IF(IEOS_TYPE>0)NUVAREOS = EOS_TAG(IEOS_TYPE)%NVAR
              ELBUF_TAB(NG)%BUFLY(IL)%ILAW = MLW                                   
              ELBUF_TAB(NG)%BUFLY(IL)%IMAT = IMID                                  
              ELBUF_TAB(NG)%BUFLY(IL)%IEOS = IEOS                                  
              ELBUF_TAB(NG)%BUFLY(IL)%IVISC = MAT_PARAM(IMID)%VISC%ILAW                              
              ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT = MLAW_TAG(IMAT(IL))%NUVAR
              ELBUF_TAB(NG)%BUFLY(IL)%NVARTMP = MLAW_TAG(IMAT(IL))%NVARTMP
              ELBUF_TAB(NG)%BUFLY(IL)%NVAR_VISC = MAT_PARAM(IMID)%VISC%NUVAR  
              ELBUF_TAB(NG)%BUFLY(IL)%NVAR_EOS = NUVAREOS           
              ELBUF_TAB(NG)%BUFLY(IL)%IPORO = 0
              ELBUF_TAB(NG)%BUFLY(IL)%NFAIL = NFAIL
            ENDDO
c
c---      User Mat Buffer, Fail buffer, Visc buffer :  alloc + init
c
            DO IL = 1,NLAY
              NUVARM  = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT
              NUVAREOS= ELBUF_TAB(NG)%BUFLY(IL)%NVAR_EOS
              NPTT    = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
              NFAIL   = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL
              IF (NFAIL == 1) ALLOCATE(BUFLY%FAIL(NPTR,NPTS,NPTT))                
              
              DO IR = 1,NPTR                                       
                DO IS = 1,NPTS                                      
                  DO IT = 1,NPTT
                    ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)%MLAW = ELBUF_TAB(NG)%BUFLY(IL)%ILAW
                    ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)%LawID =  ELBUF_TAB(NG)%BUFLY(IL)%IMAT
                    CALL MY_ALLOC(ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)%VAR,NUVARM*NEL)
                    CALL MY_ALLOC(ELBUF_TAB(NG)%BUFLY(IL)%EOS(IR,IS,IT)%VAR,NUVAREOS*NEL)
                    ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)%VAR = ZERO
                    ELBUF_TAB(NG)%BUFLY(IL)%EOS(IR,IS,IT)%VAR = ZERO
                    LMAT  = LMAT  + NUVARM*NEL

                    IF (NFAIL == 1) THEN
                      ALLOCATE(ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(NFAIL))
                      FLOC   => ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(1)         
                      IRUPT  = IPM(111,IMAT(IL))   ! modele de rupture
                      IDFAIL = IPM(236 + 1,IMAT(IL)) 
c
                      FLOC%ILAWF = IRUPT                                           
                      LFAIL = LFAIL + 1 
c
                      FLOC%IDFAIL = IDFAIL
                      LFAIL = LFAIL + 1
c
                      FLOC%NVAR  = FAIL_TAG(IRUPT)%NUVAR                              
                      CALL MY_ALLOC(FLOC%VAR,FLOC%NVAR*NEL)
                      FLOC%VAR = ZERO     
                      LFAIL = LFAIL + FLOC%NVAR*NEL + 1 
c
                      FLOC%LF_DAM  = FAIL_TAG(IRUPT)%LF_DAM                        
                      CALL MY_ALLOC(FLOC%DAM,FLOC%LF_DAM*NEL)
                      FLOC%DAM = ZERO   
                      LFAIL = LFAIL + FLOC%LF_DAM*NEL + 1 
c
                      FLOC%LF_DAMMX  = FAIL_TAG(IRUPT)%LF_DAMMX                       
                      CALL MY_ALLOC(FLOC%DAMMX,FLOC%LF_DAMMX*NEL) 
                      FLOC%DAMMX = ZERO
                      LFAIL = LFAIL + FLOC%LF_DAMMX*NEL + 1 
c
                      FLOC%LF_DAMINI = FAIL_TAG(IRUPT)%LF_DAMINI                      
                      CALL MY_ALLOC(FLOC%DAMINI,FLOC%LF_DAMINI*NEL) 
                      FLOC%DAMINI = ZERO
                      LFAIL = LFAIL + FLOC%LF_DAMINI*NEL + 1 
c
                      FLOC%LF_TDEL  = FAIL_TAG(IRUPT)%LF_TDEL                        
                      CALL MY_ALLOC(FLOC%TDEL,FLOC%LF_TDEL*NEL)
                      FLOC%TDEL = ZERO                                                           
                      LFAIL = LFAIL + FLOC%LF_TDEL*NEL + 1 
c
                      FLOC%LF_INDX = FAIL_TAG(IRUPT)%LF_INDX                       
                      CALL MY_ALLOC(FLOC%INDX,FLOC%LF_INDX*NEL)
                      FLOC%INDX = 0                                                           
                      LFAIL = LFAIL + FLOC%LF_INDX*NEL + 1 
c
                      FLOC%LF_OFF = FAIL_TAG(IRUPT)%LF_OFF                       
                      CALL MY_ALLOC(FLOC%OFF,FLOC%LF_OFF*NEL)
                      FLOC%OFF = 1                                                           
                      LFAIL = LFAIL + FLOC%LF_OFF*NEL + 1 
                    END IF

                  ENDDO                                                           
                ENDDO                                                          
              ENDDO 
            ENDDO 
c
C---
          ELSE  ! NLAY = 0  (spring std)
            ALLOCATE(ELBUF_TAB(NG)%BUFLY(0))
          ENDIF ! IF (NLAY > 0)
C
          BUFLEN = NPAR_TAB + LMAT  + LFAIL + LVISC + LPORO + LEOS + LIMAT + LIFAIL
c-------------------------------------------------
          CALL ZEROVARS_AUTO(ELBUF_TAB(NG))
c-------------------------------------------------
          CALL INITVARS_AUTO(
     .         ELBUF_TAB(NG),MLAW_TAG ,PROP_TAG ,FAIL_TAG ,
     .         IMID         ,MLW      ,IGTYP    ,JMULT    ,IFAIL  ,
     .         IEOS         ,MAXEOS   ,EOS_TAG  ,ISNOD    )  
C---
            DO IL = 1,NLAY
              BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
              IF (MLW == 2)  BUFLY%L_SIGB  = 0  ! NOT USED HERE
              BUFLY%L_OFF   = 0  ! NOT USED HERE
              BUFLY%L_EINT  = 0  ! NOT USED HERE
              BUFLY%L_FAC_YLD = 0 ! NOT USED HERE
            ENDDO
C---
C   - nstrand -
          IF (ITY == 100) THEN
            NUVARN = NINT(GEO(35,IPID))
            NX = KXX(3,NFT+1)
            GBUF%G_NUVARN = NUVARN*NX
          ENDIF          
c           Allocate thermal strain and Thermal internal energy
          IF(IEXPAN > 0) THEN
             GBUF%G_FORTH = 1  !
             GBUF%G_EINTTH = 1  
          ENDIF     
C
          IF (ISMS_SELEC >= 1) GBUF%G_ISMS  = 1
c-------------------------------------------------
c       taille du tableau local ELBUF pour restart
c-------------------------------------------------
          GBUF%G_THK_I = GBUF%G_THK

          LGLOB = 0                                 
C
          LGLOB = LGLOB + GBUF%G_NOFF  
          LGLOB = LGLOB + GBUF%G_IERR  
          LGLOB = LGLOB + GBUF%G_GAMA  
          LGLOB = LGLOB + GBUF%G_SMSTR 
          LGLOB = LGLOB + GBUF%G_HOURG 
          LGLOB = LGLOB + GBUF%G_TAG22
          LGLOB = LGLOB + GBUF%G_STRA
          LGLOB = LGLOB + GBUF%G_SIGI
          LGLOB = LGLOB + GBUF%G_DMG
          LGLOB = LGLOB + GBUF%G_DAMDL  
          LGLOB = LGLOB + GBUF%G_FOR     
          LGLOB = LGLOB + GBUF%G_MOM     
          LGLOB = LGLOB + GBUF%G_THK
          LGLOB = LGLOB + GBUF%G_DT
          LGLOB = LGLOB + GBUF%G_ISMS
          LGLOB = LGLOB + GBUF%G_STRW 
          LGLOB = LGLOB + GBUF%G_THK_I
          LGLOB = LGLOB + GBUF%G_MAXFRAC
          LGLOB = LGLOB + GBUF%G_MAXEPS    

          IF (NPG > 1) THEN                    ! not actually used
            LGLOB = LGLOB + GBUF%G_FORPG
            LGLOB = LGLOB + GBUF%G_MOMPG
C
            LGLOB = LGLOB + GBUF%G_FORPGPINCH
            LGLOB = LGLOB + GBUF%G_MOMPGPINCH
            LGLOB = LGLOB + GBUF%G_EPGPINCHXZ
            LGLOB = LGLOB + GBUF%G_EPGPINCHYZ
            LGLOB = LGLOB + GBUF%G_EPGPINCHZZ
C
            LGLOB = LGLOB + GBUF%G_STRPG
            LGLOB = LGLOB + GBUF%G_STRWPG
          ENDIF
C
          LGLOB = LGLOB + GBUF%G_BFRAC    
          LGLOB = LGLOB + GBUF%G_OFF   
          LGLOB = LGLOB + GBUF%G_EINT  
          LGLOB = LGLOB + GBUF%G_EINS  
          LGLOB = LGLOB + GBUF%G_RHO   
          LGLOB = LGLOB + GBUF%G_QVIS  
          LGLOB = LGLOB + GBUF%G_DELTAX
          LGLOB = LGLOB + GBUF%G_VOL   
          LGLOB = LGLOB + GBUF%G_EPSD  
          LGLOB = LGLOB + GBUF%G_EPSQ  
          LGLOB = LGLOB + GBUF%G_PLA   
          LGLOB = LGLOB + GBUF%G_TEMP  
          LGLOB = LGLOB + GBUF%G_TB    
          LGLOB = LGLOB + GBUF%G_RK    
          LGLOB = LGLOB + GBUF%G_RE    
          LGLOB = LGLOB + GBUF%G_SIG   
          LGLOB = LGLOB + GBUF%G_FORTH   
          LGLOB = LGLOB + GBUF%G_EINTTH
          LGLOB = LGLOB + GBUF%G_SEQ
          LGLOB = LGLOB + GBUF%G_ABURN
          LGLOB = LGLOB + GBUF%G_MU
C
          LGLOB = LGLOB + GBUF%G_FILL
C
          LGLOB = LGLOB + GBUF%G_AREA
          LGLOB = LGLOB + GBUF%G_SKEW
          LGLOB = LGLOB + GBUF%G_LENGTH
          LGLOB = LGLOB + GBUF%G_TOTDEPL
          LGLOB = LGLOB + GBUF%G_TOTROT
          LGLOB = LGLOB + GBUF%G_FOREP
          LGLOB = LGLOB + GBUF%G_MOMEP
          LGLOB = LGLOB + GBUF%G_DEP_IN_TENS
          LGLOB = LGLOB + GBUF%G_DEP_IN_COMP
          LGLOB = LGLOB + GBUF%G_ROT_IN_TENS
          LGLOB = LGLOB + GBUF%G_ROT_IN_COMP
          LGLOB = LGLOB + GBUF%G_POSX
          LGLOB = LGLOB + GBUF%G_POSY
          LGLOB = LGLOB + GBUF%G_POSZ
          LGLOB = LGLOB + GBUF%G_POSXX
          LGLOB = LGLOB + GBUF%G_POSYY
          LGLOB = LGLOB + GBUF%G_POSZZ
          LGLOB = LGLOB + GBUF%G_YIELD
          LGLOB = LGLOB + GBUF%G_LENGTH_ERR
          LGLOB = LGLOB + GBUF%G_DV
          LGLOB = LGLOB + GBUF%G_DFS
          LGLOB = LGLOB + GBUF%G_SKEW_ERR
          LGLOB = LGLOB + GBUF%G_E6
          LGLOB = LGLOB + GBUF%G_RUPTCRIT
          LGLOB = LGLOB + GBUF%G_MASS
          LGLOB = LGLOB + GBUF%G_V_REPCVT
          LGLOB = LGLOB + GBUF%G_VR_REPCVT
          LGLOB = LGLOB + GBUF%G_NUVAR
          LGLOB = LGLOB + GBUF%G_NUVARN
          LGLOB = LGLOB + GBUF%G_DEFINI
          LGLOB = LGLOB + GBUF%G_FORINI
          LGLOB = LGLOB + GBUF%G_INIFRIC
          LGLOB = LGLOB + GBUF%G_SKEW_ID
C
          LGLOB = LGLOB + GBUF%G_SLIPRING_ID
          LGLOB = LGLOB + GBUF%G_SLIPRING_FRAM_ID
          LGLOB = LGLOB + GBUF%G_SLIPRING_STRAND
          LGLOB = LGLOB + GBUF%G_RETRACTOR_ID
          LGLOB = LGLOB + GBUF%G_RINGSLIP
          LGLOB = LGLOB + GBUF%G_ADD_NODE
          LGLOB = LGLOB + GBUF%G_UPDATE
          LGLOB = LGLOB + GBUF%G_FRAM_FACTOR
          LGLOB = LGLOB + GBUF%G_INTVAR
c
c         failure struct for beams type 3 stored in global buffer
c
          GFAIL = 0
          IF (IGTYP == 3 .and. IFAIL > 0) THEN
             ALLOCATE(ELBUF_TAB(NG)%GBUF%FAIL(1))
             IRUPT  = IPM(111,IMID)   ! type of failure model
             IDFAIL = IPM(237,IMID)   ! failure model ID
c
             ELBUF_TAB(NG)%GBUF%FAIL(1)%ILAWF = IRUPT                                           
             GFAIL = GFAIL + 1 
c
             ELBUF_TAB(NG)%GBUF%FAIL(1)%IDFAIL = IDFAIL
             GFAIL = GFAIL + 1
c
             ELBUF_TAB(NG)%GBUF%FAIL(1)%NVAR  = FAIL_TAG(IRUPT)%NUVAR                              
             CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%FAIL(1)%VAR,ELBUF_TAB(NG)%GBUF%FAIL(1)%NVAR*NEL)
             ELBUF_TAB(NG)%GBUF%FAIL(1)%VAR = ZERO     
             GFAIL = GFAIL + ELBUF_TAB(NG)%GBUF%FAIL(1)%NVAR*NEL + 1
c
             ELBUF_TAB(NG)%GBUF%FAIL(1)%LF_DAM  = FAIL_TAG(IRUPT)%LF_DAM                        
             CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%FAIL(1)%DAM,ELBUF_TAB(NG)%GBUF%FAIL(1)%LF_DAM*NEL)
             ELBUF_TAB(NG)%GBUF%FAIL(1)%DAM = ZERO   
             GFAIL = GFAIL + ELBUF_TAB(NG)%GBUF%FAIL(1)%LF_DAM*NEL + 1
c
             ELBUF_TAB(NG)%GBUF%FAIL(1)%LF_DAMMX  = FAIL_TAG(IRUPT)%LF_DAMMX                       
             CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%FAIL(1)%DAMMX,ELBUF_TAB(NG)%GBUF%FAIL(1)%LF_DAMMX*NEL) 
             ELBUF_TAB(NG)%GBUF%FAIL(1)%DAMMX = ZERO
             GFAIL = GFAIL + ELBUF_TAB(NG)%GBUF%FAIL(1)%LF_DAMMX*NEL + 1
c
             ELBUF_TAB(NG)%GBUF%FAIL(1)%LF_DAMINI = FAIL_TAG(IRUPT)%LF_DAMINI                      
             CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%FAIL(1)%DAMINI,ELBUF_TAB(NG)%GBUF%FAIL(1)%LF_DAMINI*NEL) 
             ELBUF_TAB(NG)%GBUF%FAIL(1)%DAMINI = ZERO
             GFAIL = GFAIL + ELBUF_TAB(NG)%GBUF%FAIL(1)%LF_DAMINI*NEL + 1
c
             ELBUF_TAB(NG)%GBUF%FAIL(1)%LF_TDEL  = FAIL_TAG(IRUPT)%LF_TDEL                        
             CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%FAIL(1)%TDEL,ELBUF_TAB(NG)%GBUF%FAIL(1)%LF_TDEL*NEL)
             ELBUF_TAB(NG)%GBUF%FAIL(1)%TDEL = ZERO                                                           
             GFAIL = GFAIL + ELBUF_TAB(NG)%GBUF%FAIL(1)%LF_TDEL*NEL + 1
c
             ELBUF_TAB(NG)%GBUF%FAIL(1)%LF_INDX = FAIL_TAG(IRUPT)%LF_INDX                       
             CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%FAIL(1)%INDX,ELBUF_TAB(NG)%GBUF%FAIL(1)%LF_INDX*NEL)
             ELBUF_TAB(NG)%GBUF%FAIL(1)%INDX = 0                                                           
             GFAIL = GFAIL + ELBUF_TAB(NG)%GBUF%FAIL(1)%LF_INDX*NEL + 1
c
             ELBUF_TAB(NG)%GBUF%FAIL(1)%LF_OFF = FAIL_TAG(IRUPT)%LF_OFF                       
             CALL MY_ALLOC(ELBUF_TAB(NG)%GBUF%FAIL(1)%OFF,ELBUF_TAB(NG)%GBUF%FAIL(1)%LF_OFF*NEL)
             ELBUF_TAB(NG)%GBUF%FAIL(1)%OFF = 1                                                           
             GFAIL = GFAIL + ELBUF_TAB(NG)%GBUF%FAIL(1)%LF_OFF*NEL + 1
          END IF
c
          BUFLEN = BUFLEN + GBUF%NVAR_GLOB + 1 + LGLOB*NEL + GFAIL
c---------          
c---------   layer variables       
          LLAY = 0
          DO IL=1,NLAY
            BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
            LLAY = LLAY + NPAR_LAY + BUFLY%NVAR_LAY + BUFLY%NVAR_LOC
            LLAY = LLAY + NEL*BUFLY%LY_DMG 
            LLAY = LLAY + NEL*BUFLY%LY_GAMA
            LLAY = LLAY + NEL*BUFLY%LY_DIRA
            LLAY = LLAY + NEL*BUFLY%LY_DIRB
            LLAY = LLAY + NEL*BUFLY%LY_HOURG
            IF (LFAIL > 0) THEN
              LLAY = LLAY + NEL*BUFLY%LY_UELR
              LLAY = LLAY + NEL*BUFLY%LY_UELR1
              LLAY = LLAY + NEL*BUFLY%LY_OFFPG
              LLAY = LLAY + NEL*BUFLY%LY_OFF
            ENDIF
          ENDDO
C
          BUFLEN = BUFLEN  + LLAY
C
c---------  local variables 
          DO IL=1,NLAY  ! NLAY = 1 for INTGRATED BEAM
            BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
            NIP_LAY = BUFLY%NPTT
c
            LLOC = 0                                 
            LLOC = LLOC + BUFLY%L_OFF    
            LLOC = LLOC + BUFLY%L_GAMA 
c
            LLOC = LLOC + BUFLY%L_STRA   
c            
            LLOC = LLOC + BUFLY%L_FRAC   
            LLOC = LLOC + BUFLY%L_BFRAC
            LLOC = LLOC + BUFLY%L_EINT   
            LLOC = LLOC + BUFLY%L_EINS   
            BUFLY%L_RHO = 0
            LLOC = LLOC + BUFLY%L_RHO         
            LLOC = LLOC + BUFLY%L_DP_DRHO     
            LLOC = LLOC + BUFLY%L_QVIS   
            LLOC = LLOC + BUFLY%L_DELTAX 
            LLOC = LLOC + BUFLY%L_VOL    
            LLOC = LLOC + BUFLY%L_EPSA   
            LLOC = LLOC + BUFLY%L_EPSD   
            LLOC = LLOC + BUFLY%L_EPSQ   
            LLOC = LLOC + BUFLY%L_EPSF   
            LLOC = LLOC + BUFLY%L_PLA    
            LLOC = LLOC + BUFLY%L_TEMP 
            LLOC = LLOC + BUFLY%L_TB     
            LLOC = LLOC + BUFLY%L_RK     
            LLOC = LLOC + BUFLY%L_RE     
            LLOC = LLOC + BUFLY%L_VK     
            LLOC = LLOC + BUFLY%L_SF     
            LLOC = LLOC + BUFLY%L_ROB    
            LLOC = LLOC + BUFLY%L_DAM    
            LLOC = LLOC + BUFLY%L_DSUM   
            LLOC = LLOC + BUFLY%L_DGLO     
            LLOC = LLOC + BUFLY%L_CRAK   
            LLOC = LLOC + BUFLY%L_ANG      
            LLOC = LLOC + BUFLY%L_EPE    
            LLOC = LLOC + BUFLY%L_EPC    
            LLOC = LLOC + BUFLY%L_XST    
            LLOC = LLOC + BUFLY%L_SSP    
            LLOC = LLOC + BUFLY%L_Z      
            LLOC = LLOC + BUFLY%L_VISC   
            LLOC = LLOC + BUFLY%L_SIGL   
            LLOC = LLOC + BUFLY%L_SIGV   
            LLOC = LLOC + BUFLY%L_SIGA   
            LLOC = LLOC + BUFLY%L_SIGB   
            LLOC = LLOC + BUFLY%L_SIGC        
            LLOC = LLOC + BUFLY%L_SIGD        
            LLOC = LLOC + BUFLY%L_SIGF   
            LLOC = LLOC + BUFLY%L_SIG
            LLOC = LLOC + BUFLY%L_SIGPLY
            LLOC = LLOC + BUFLY%L_FOR    
            LLOC = LLOC + BUFLY%L_MOM
            LLOC = LLOC + BUFLY%L_THK    
            LLOC = LLOC + BUFLY%L_SMSTR    
            LLOC = LLOC + BUFLY%L_DMG  
            LLOC = LLOC + BUFLY%L_FORTH
            LLOC = LLOC + BUFLY%L_EINTTH
            LLOC = LLOC + BUFLY%L_SEQ
            LLOC = LLOC + BUFLY%L_FAC_YLD 
            LLOC = LLOC + BUFLY%L_ABURN 
            LLOC = LLOC + BUFLY%L_MU
            LLOC = LLOC + BUFLY%L_PLANL
            LLOC = LLOC + BUFLY%L_EPSDNL
            LLOC = LLOC + BUFLY%L_DMGSCL
            LLOC = LLOC + BUFLY%L_TSAIWU
c
            BUFLEN = BUFLEN + (LLOC * NEL + NPAR_LBUF) * NIP_LAY
          ENDDO
c---------
C----
c        print*, 'elbuf_ini =', NG,NEL,BUFLEN,LGLOB,
c     . ITY,IGTYP,GBUF%G_PLA,MLW,NLAY,NPTS
C----
c
c-------------------------------------------------
          IPARG(66,NG) = BUFLEN    !  size of restart transfer table 
c 
          IF(PRINT_FLAG>6) WRITE(IOUT,1002) NG, BUFLEN
c-------------------------------------------------
          CALL ALLOCBUF_AUTO(ELBUF_TAB,NLAY,NPTR,NPTS,NPTT,
     .                       NINTLAY,NEL,NPT,NG,NGROUP,ITY,
     .                       IGTYP)
C
          IF (ALLOCATED(IMAT)) DEALLOCATE(IMAT)
          IF (ALLOCATED(ILAW)) DEALLOCATE(ILAW)
c------------------------------------------------------
        ELSE    
          ALLOCATE(ELBUF_TAB(NG)%BUFLY(0))
c------------------------------------------------------
        ENDIF   ! ITY = 1,2,51,101 (solid group)

         IF (DEBUG_PRINT == 1) THEN
           print*,' elbuf_ini : NG,ITY,IGTYP,buflen=',NG,ITY,IGTYP,buflen
         ENDIF

      ENDDO     ! NG  = 1,NGROUP
c------------------------------------------------------
 1001 FORMAT(//,
     + 7X,'RESTART SIZE OF SHELL AND SOLID ELEMENT GROUPS'/
     + 7X,'----------------------------------------------'/)
 1002 FORMAT(
     + 7X,'ELEMENT GROUP',I10,',  SIZE=',I10)
C-----------
      RETURN
      END SUBROUTINE ELBUF_INI
