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
Chd|  DEALLOCATE_ELBUF              source/elements/elbuf_init/deallocate_buffer.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        DEALLOCATE_ONE_ELEMENT_GROUP  source/elements/elbuf_init/deallocate_one_element_group.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|====================================================================
      SUBROUTINE DEALLOCATE_ELBUF(
     .           ELBUF_TAB ,MLAW_TAG ,PROP_TAG ,FAIL_TAG ,VISC_TAG  ,
     .           IGEO      ,IPM      ,PM       ,IPARG    ,IPART     ,
     .           IPARTSP   ,IXS      ,IXQ      ,IXC      ,IXTG      ,
     .           KXSP      ,FLAG_XFEM,IPARTIG3D,STACK    ,IGEO_STACK,
     .           IXT       ,IXP      ,IXR      ,KXX      ,GEO       )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD             
      USE ELBUFTAG_MOD            
      USE MESSAGE_MOD
      USE STACK_MOD
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
#include      "com_xfem1.inc"
#include      "scr17_c.inc"
#include      "scr19_c.inc"
#include      "scr23_c.inc"
#include      "sphcom.inc"
#include      "tablen_c.inc"
#include      "elbuf_c.inc"
#include      "tabsiz_c.inc"
#include      "inter22.inc"
#include      "my_allocate.inc"
#include      "sms_c.inc"
#include      "boltpr_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,NGROUP),IPART(LIPART1,*),
     .        IPARTSP(*),IPM(NPROPMI,NUMMAT),IGEO(NPROPGI,NUMGEO),
     .        IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*),IXTG(NIXTG,*),
     .        KXSP(NISP,*),FLAG_XFEM,IPARTIG3D(*),IGEO_STACK(NPROPGI,*),
     .        IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),KXX(NIXX,*)
      my_real ::
     .    PM(NPROPM,*),GEO(NPROPG,*)
      TYPE(ELBUF_STRUCT_),TARGET ,DIMENSION(NGROUP)     :: ELBUF_TAB
      TYPE(MLAW_TAG_)    ,TARGET ,DIMENSION(NUMMAT)     :: MLAW_TAG
      TYPE(PROP_TAG_)    ,TARGET ,DIMENSION(0:MAXPROP)  :: PROP_TAG
      TYPE(FAIL_TAG_)    ,TARGET ,DIMENSION(0:MAXFAIL ) :: FAIL_TAG
      TYPE(VISC_TAG_)    ,TARGET ,DIMENSION(0:MAXVISC ) :: VISC_TAG
      TYPE (STACK_PLY) :: STACK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IL,IR,IS,IT,NG,BUFLEN,IRUPT,IPID,IMID,IPRT,
     .   NEL,NPT,NPG,MLW,ISORTH,ISRATE,ISROT,IREP,ISVIS,NLAY,STAT,
     .   ISTRA,IFAIL,NFAIL,NGFAIL,IEOS,IXFEM,NPTR,NPTS,NPTT,ISHPLYXFEM,
     .   NFT,NFT0,KFTS,ITY,JALE,OFF,ISMSTR,ICPRE,NSG,ICNOD,ISNOD,JEUL,
     .   JTUR,JTHE,JLAG,IRS,IRE,ICSTR,NVC,JMULT,IADM1,IADM2,IADM3,
     .   MMAT1,MMAT2,MMAT3,JHBE,JIVF,JPOR,IPLA,PROC,JCLOS,CODCONV,
     .   IINT,JCVT,ICSEN,ILEV,IEXPAN,IGRE,JSMS,MAXFLAY,IVISC,LAW_VIS,
     .   NUVAR,NUVARM,NUVARF,NUVARV,IGTYP,TSHELL,TSH_ORT,LINTLOC,
     .   LINTLAY,NPAR_LAY,NPAR_TAB,NPAR_LBUF,NIP_LAY,NINTLAY,LIMAT,
     .   LIFAIL,LGLOB,LLAY,LLOC,LFAIL,LVISC,LMAT,LPORO,LEOS,NCTRL,
     .   ISUBSTACK,IIGEO,IADI,IPPID,IPIDL,NPT_LAY,LEN_PLAPT,LEN_SIGPT,
     .   IPMAT,IPMAT_IPLY,NPTTOT,NUVARN,NX,LAW,IBOLTP,INLOC
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: TFAIL
      INTEGER, ALLOCATABLE, DIMENSION(:)   :: IMAT,ILAW
      TYPE(ELBUF_STRUCT_), POINTER :: ELBUF
      TYPE(BUF_MAT_)     , POINTER :: MATBUF
      TYPE(BUF_LAY_)     , POINTER :: BUFLY
      TYPE(BUF_INTLAY_)  , POINTER :: INTLAY
      TYPE(BUF_VISC_)    , POINTER :: VISCBUF
      TYPE(G_BUFEL_)     , POINTER :: GBUF
      TYPE(L_BUFEL_)     , POINTER :: LBUF
      TYPE(BUF_INTLOC_)  , POINTER :: ILBUF
      TYPE(PROP_TAG_)    , POINTER :: PTAG
      TYPE(FAIL_LOC_)    , POINTER :: FLOC

      DO NG = 1,NGROUP 
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
        NCTRL   = IPARG(63,NG)  ! Control point number , iso-geometric elements
        ISUBSTACK = IPARG(71,NG)
        IBOLTP  = IPARG(72,NG)  ! Bolt preloading flag
        INLOC   = IPARG(78,NG)  ! Non-local regularization flag
        NINTLAY = 0                                              
cc        NXEL    = 0
c----
        NPAR_TAB  = 9  ! number of integer parameters at top structure level
        NPAR_LAY  = 11 ! number of integer parameters at layer level
        NPAR_LBUF = 2  ! number of integer parameters at LBUF layer

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

        IF (IXFEM == 0 .AND. FLAG_XFEM == 1) CYCLE ! no "XFEM_TAB" buffer for non xfem PARTS
c------------------------------------------------------
        IF (ITY == 1 .OR. ITY == 2 .OR. ITY == 51 .OR. ITY == 101 ) THEN   ! solids, quads & SPH cells
c------------------------------------------------------
c          IF (MLAW_TAG(MLW)%L_PLA == 0 .and. IPARG(10,NG) == 2) THEN
c            IPARG(10,NG) = 0  ! ICPRE
c          ENDIF                                   
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
C     Multifluid law
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
              IF (TSH_ORT == 0) THEN
                NLAY = NPTT
                NPTT = 1
              ELSEIF (ICSTR == 1) THEN   
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

          CALL DEALLOCATE_ONE_ELEMENT_GROUP(ELBUF_TAB,NLAY,NPTR,NPTS,NPTT,
     .                   NINTLAY,NEL,NPT,NG,NGROUP,ITY,IGTYP)

          DO IL = 1, NLAY
           NUVARM = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT
           NUVARV = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_VISC
           NFAIL = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL
           DO IR = 1,NPTR                                       
            DO IS = 1,NPTS                                      
              DO IT = 1,NPTT
                LMAT  = LMAT  + NUVARM*NEL
                LVISC = LVISC + NUVARV*NEL
                DO J = 1,NFAIL
                  FLOC=>ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(J)         
c                          
                  IF(ASSOCIATED(FLOC%VAR)) DEALLOCATE(FLOC%VAR)
                  IF(ASSOCIATED(FLOC%DAM)) DEALLOCATE(FLOC%DAM)
                  IF(ASSOCIATED(FLOC%DAMMX)) DEALLOCATE(FLOC%DAMMX)
                  IF(ASSOCIATED(FLOC%TDEL)) DEALLOCATE(FLOC%TDEL)
                  IF(ASSOCIATED(FLOC%INDX))  DEALLOCATE(FLOC%INDX)
                  IF(ASSOCIATED(FLOC%OFF))  DEALLOCATE(FLOC%OFF)
                ENDDO
                IF(ASSOCIATED(ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)%VAR))  
     *                     DEALLOCATE(ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)%VAR)              
                IF(ASSOCIATED(ELBUF_TAB(NG)%BUFLY(IL)%VISC(IR,IS,IT)%VAR)) 
     *                     DEALLOCATE(ELBUF_TAB(NG)%BUFLY(IL)%VISC(IR,IS,IT)%VAR)       
                IF(ASSOCIATED(ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC))
     *                     DEALLOCATE(ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC)

              ENDDO                                                           
            ENDDO                                                          
           ENDDO 
          ENDDO
          
          DO IL = 1, NLAY
            BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
            IF(ASSOCIATED(BUFLY%LBUF)) DEALLOCATE(BUFLY%LBUF)
            IF(ASSOCIATED(BUFLY%MAT)) DEALLOCATE(BUFLY%MAT)
            IF(ASSOCIATED(BUFLY%FAIL)) DEALLOCATE(BUFLY%FAIL)
            IF(ASSOCIATED(BUFLY%PROP)) DEALLOCATE(BUFLY%PROP)
            IF(ASSOCIATED(BUFLY%EOS)) DEALLOCATE(BUFLY%EOS)
            IF(ASSOCIATED(BUFLY%VISC)) DEALLOCATE(BUFLY%VISC)
            IF(ASSOCIATED(BUFLY%PORO)) DEALLOCATE(BUFLY%PORO)
          ENDDO

          IF(ASSOCIATED(ELBUF_TAB(NG)%INTLAY)) DEALLOCATE(ELBUF_TAB(NG)%INTLAY)
          IF(ASSOCIATED(ELBUF_TAB(NG)%BUFLY)) DEALLOCATE(ELBUF_TAB(NG)%BUFLY)
c

c------------------------------------------------------
        ELSEIF (ITY == 3 .OR. ITY == 7) THEN   ! shell properties
c------------------------------------------------------
          IF (ITY == 3) THEN
            IMID = IXC(1,NFT+1)                                                
            IPID = IXC(6,NFT+1)      
          ELSEIF (ITY == 7) THEN 
            IMID = IXTG(1,NFT+1)                                                
            IPID = IXTG(5,NFT+1)                                       
          ENDIF

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

          NPG = NPTR*NPTS
c-------------------------------------------------
          IF (IGTYP == 51 .OR. IGTYP == 52) THEN
            NPT = NPG*NPT
          ELSE
            NPT = NPG*NLAY*NPTT
          ENDIF
c-----------------------------------------------------
c deallocation des sub-structures de l'element buffer
c-----------------------------------------------------
          CALL DEALLOCATE_ONE_ELEMENT_GROUP(ELBUF_TAB,NLAY,NPTR,NPTS,NPTT,NINTLAY, 
     *            NEL,NPT,NG,NGROUP,ITY,IGTYP)

          DO IL = 1, NINTLAY                                                       
            NUVARM = ELBUF_TAB(NG)%INTLAY(IL)%NVAR_MAT                            
            NFAIL = ELBUF_TAB(NG)%INTLAY(IL)%NFAIL                                
            DO IR = 1,NPTR                                                         
              DO IS = 1,NPTS     
C                                                    
                DO J = 1,NFAIL 
                  FLOC=>ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(J)         
                  IF(ASSOCIATED(FLOC%VAR)) DEALLOCATE(FLOC%VAR)                                
                ENDDO  
                                                            
                IF(ASSOCIATED(ELBUF_TAB(NG)%INTLAY(IL)%MAT(IR,IS)%VAR))
     *                    DEALLOCATE(ELBUF_TAB(NG)%INTLAY(IL)%MAT(IR,IS)%VAR)
                IF(ASSOCIATED(ELBUF_TAB(NG)%INTLAY(IL)%FAIL(IR,IS)%FLOC))
     *                    DEALLOCATE(ELBUF_TAB(NG)%INTLAY(IL)%FAIL(IR,IS)%FLOC)     
              ENDDO                                                                  
            ENDDO                                                                   
          ENDDO                                                                    

          DO IL = 1, NLAY
           NUVARM = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT
           NUVARV = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_VISC
           NFAIL  = ELBUF_TAB(NG)%BUFLY(IL)%NFAIL
           NPTT   = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
           DO IR = 1,NPTR                                       
            DO IS = 1,NPTS                                      
              DO IT = 1,NPTT
                DO J = 1,NFAIL
                  FLOC=>ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC(J)         
 
                  IF(ASSOCIATED(FLOC%VAR)) DEALLOCATE(FLOC%VAR)
                  IF(ASSOCIATED(FLOC%DAM)) DEALLOCATE(FLOC%DAM)
                  IF(ASSOCIATED(FLOC%DAMMX)) DEALLOCATE(FLOC%DAMMX)
                  IF(ASSOCIATED(FLOC%TDEL)) DEALLOCATE(FLOC%TDEL)
                  IF(ASSOCIATED(FLOC%INDX)) DEALLOCATE(FLOC%INDX)
                  IF(ASSOCIATED(FLOC%OFF)) DEALLOCATE(FLOC%OFF)
                ENDDO

                IF(ASSOCIATED(ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)%VAR))
     *              DEALLOCATE(ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)%VAR)
                IF(ASSOCIATED(ELBUF_TAB(NG)%BUFLY(IL)%VISC(IR,IS,IT)%VAR))
     *              DEALLOCATE(ELBUF_TAB(NG)%BUFLY(IL)%VISC(IR,IS,IT)%VAR)
                IF(ASSOCIATED(ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC))
     *              DEALLOCATE(ELBUF_TAB(NG)%BUFLY(IL)%FAIL(IR,IS,IT)%FLOC)
              ENDDO                                                           
            ENDDO                                                          
           ENDDO 
          ENDDO 

          DO I = 1, NINTLAY                                      
            INTLAY => ELBUF_TAB(NG)%INTLAY(I)                  
            IF(ASSOCIATED(INTLAY%ILBUF))   DEALLOCATE(INTLAY%ILBUF)  
            IF(ASSOCIATED(INTLAY%MAT)) DEALLOCATE(INTLAY%MAT)
            IF(ASSOCIATED(INTLAY%FAIL))DEALLOCATE(INTLAY%FAIL)
          ENDDO  

          IF(ASSOCIATED(ELBUF_TAB(NG)%INTLAY))DEALLOCATE(ELBUF_TAB(NG)%INTLAY)

          DO I = 1, NLAY
            BUFLY => ELBUF_TAB(NG)%BUFLY(I)
            NPTT = BUFLY%NPTT
            IF(ASSOCIATED(BUFLY%LBUF)) DEALLOCATE(BUFLY%LBUF)
            IF(ASSOCIATED(BUFLY%MAT))   DEALLOCATE(BUFLY%MAT)
            IF(ASSOCIATED(BUFLY%FAIL))  DEALLOCATE(BUFLY%FAIL)
            IF(ASSOCIATED(BUFLY%PROP))  DEALLOCATE(BUFLY%PROP)
            IF(ASSOCIATED(BUFLY%EOS))  DEALLOCATE(BUFLY%EOS)
            IF(ASSOCIATED(BUFLY%VISC))  DEALLOCATE(BUFLY%VISC)
            IF(ASSOCIATED(BUFLY%PORO))  DEALLOCATE(BUFLY%PORO)
          ENDDO

          IF(ASSOCIATED(ELBUF_TAB(NG)%BUFLY)) DEALLOCATE(ELBUF_TAB(NG)%BUFLY)
          
          ! Non-local table for regularization in the shell thickness
          IF ((INLOC>0).AND.(NPTT>1)) THEN
            ! Deallocating the buffer
            DO IR = 1,NPTR
              DO IS = 1,NPTS
                IF(ASSOCIATED(ELBUF_TAB(NG)%NLOC(IR,IS)%MASSTH))  
     *             DEALLOCATE(ELBUF_TAB(NG)%NLOC(IR,IS)%MASSTH)
                IF(ASSOCIATED(ELBUF_TAB(NG)%NLOC(IR,IS)%UNLTH))  
     *             DEALLOCATE(ELBUF_TAB(NG)%NLOC(IR,IS)%UNLTH)   
                IF(ASSOCIATED(ELBUF_TAB(NG)%NLOC(IR,IS)%VNLTH))  
     *             DEALLOCATE(ELBUF_TAB(NG)%NLOC(IR,IS)%VNLTH)   
                IF(ASSOCIATED(ELBUF_TAB(NG)%NLOC(IR,IS)%FNLTH))  
     *             DEALLOCATE(ELBUF_TAB(NG)%NLOC(IR,IS)%FNLTH) 
              ENDDO
            ENDDO
            IF(ASSOCIATED(ELBUF_TAB(NG)%NLOC)) DEALLOCATE(ELBUF_TAB(NG)%NLOC)
          ENDIF
c---
c 1-D elements (TRUSS, BEAM, SPRING)
c---
        ELSEIF (ITY == 4 .OR. ITY == 5 .OR. ITY == 6 .OR. ITY == 100) THEN 
c------------------------------------------------------
          IF (ITY == 4) THEN      ! TRUSS
            IMID = IXT(1,NFT+1)
            IPID = IXT(4,NFT+1)
          ELSEIF (ITY == 5) THEN  ! BEAM
            IMID = IXP(1,NFT+1)     
            IPID = IXP(5,NFT+1)
          ELSEIF (ITY == 6) THEN  ! SPRING
            IPID = IXR(1,NFT+1)
          ELSEIF (ITY == 100) THEN  ! MULTIBRIN
            IPID = KXX(2,NFT+1)
          ENDIF
          IGTYP = IGEO(11,IPID)
C----
          GBUF  => ELBUF_TAB(NG)%GBUF 
                                               
          CALL  DEALLOCATE_ONE_ELEMENT_GROUP(ELBUF_TAB,NLAY,NPTR,NPTS  ,NPTT, NINTLAY,
     .                         NEL      ,NPT ,NG  ,NGROUP,ITY,  IGTYP)

          IF (NLAY > 0) THEN  ! integrated beam (NLAY = 1)
c
c---      User Mat Buffer, Fail buffer, Visc buffer :  alloc + init
c
            DO IL = 1,NLAY
              NUVARM = ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT
              NPTT   = ELBUF_TAB(NG)%BUFLY(IL)%NPTT
              DO IR = 1,NPTR                                       
                DO IS = 1,NPTS                                      
                  DO IT = 1,NPTT

                    IF (ASSOCIATED(ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)%VAR))
     .                 DEALLOCATE(ELBUF_TAB(NG)%BUFLY(IL)%MAT(IR,IS,IT)%VAR)
                  ENDDO                                                           
                ENDDO                                                          
              ENDDO 
            ENDDO 

            DO IL = 1,NLAY
              BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
              
              IF (ASSOCIATED(BUFLY%LBUF))  DEALLOCATE(BUFLY%MAT)
              IF (ASSOCIATED(BUFLY%MAT))   DEALLOCATE(BUFLY%MAT)
            ENDDO

            IF (ASSOCIATED(ELBUF_TAB(NG)%BUFLY)) DEALLOCATE(ELBUF_TAB(NG)%BUFLY)
          ENDIF ! IF (NLAY > 0)

        ELSE    
           IF (ASSOCIATED(ELBUF_TAB(NG)%BUFLY)) DEALLOCATE(ELBUF_TAB(NG)%BUFLY)
        ENDIF   ! ITY = 1,2,51,101 (solid group)

      ENDDO     ! NG  = 1,NGROUP

      RETURN

      END

