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
C
Chd|====================================================================
Chd|  W_PON                         source/restart/ddsplit/w_pon.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE W_PON(
     1      ADDCNE   ,CNE      ,LCNE      ,NUMNOD_L  ,NODGLOB   ,
     2      LCNE_L   ,CEP      ,CEL       ,IXS       ,IXS10     ,
     3      IXS20    ,IXS16    ,IXQ       ,IXC       ,IXT       ,
     4      IXP      ,IXR      ,IXTG      ,IXUR      ,MONVOL    ,
     5      IB       ,GEO      ,IGEO      ,PROC      ,
     6      NUMELS_L ,NUMELS8_L,NUMELS10_L,NUMELS16_L,NUMELS20_L,
     7      NUMELQ_L ,NUMELC_L ,NUMELT_L  ,NUMELP_L  ,NUMELR_L  ,
     8      NUMELTG_L,NUMELUR_L,NSKYRW_L  ,NPRW      ,LPRW      ,
     9      NSKYRBK_L,NPBY      ,LPBY     ,DD_RBY2   ,
     A      I2NSNT   ,I2NSN_L  ,IPARI     ,NIR       ,
     B      LCNI2_L  ,NISKYI2_L,CEPI2     ,CELI2     ,CNI2      ,
     C      ADDCNI2  ,NBDDI2M  ,NCONLD_L  ,IXTG6     ,NUMELTG6_L,
     D      NNMV_L   ,NNMVC_L  ,NSKYLL_L  ,NNLINK    ,LLLINK    ,
     E      NSKYRBM_L,DD_RBM2  ,IBVEL     ,LBVEL     ,NBI18_L   ,
     F      NSKYI18_L,LEN_IA   ,NCONV_L   ,IBCV      ,NSKYRBE3_L,
     G      IRBE3    ,LRBE3    ,NSKYRBMK_L, IRBYM    , LCRBYM  ,
     H      FRONT_RM  ,DD_RBYM2,IBCR      ,NRADIA_L  ,ADDCNE_PXFEM,
     I      CNE_PXFEM ,CEL_PXFEM ,LCNEPXFEM_L,INOD_PXFEM,IEL_PXFEM,
     J      NUMELCPXFEM_L,NUMNODPXFEM_L   ,LLOADP   ,ILOADP   ,
     K      LLLOADP_L,ADDCNE_CRKXFEM,CNE_CRKXFEM,CEL_CRKXFEM,
     L      LCNECRKXFEM_L,INOD_CRKXFEM,IEL_CRKXFEM,NUMELCCRKXFE_L,
     M      NUMNODCRKXFE_L,NUMELTGCRKXFE_L,CEP_CRKXFEM,INOD_CRK_L,
     N      CRKNODIAD, INTBUF_TAB,NUMELIG3D_L,KXIG3D,IXIG3D,
     O      IBFFLUX  ,NFXFLUX_L ,CEPCND  ,CELCND   ,ADDCNCND    ,
     P      CNCND     ,NS10E_L  ,ICNDS10 ,LCNCND_L ,ITAGND ,IGRSURF,
     Q      IGRSURF_PROC ,LOCAL_NEBCS, EBCS_TAB_LOC_2)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
      USE EBCS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "rnur_c.inc"
#include      "com_xfem1.inc"
#include      "scr17_c.inc"
#include      "ige3d_c.inc"
#include      "thermal_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  LCNE, NUMNOD_L, LCNE_L, PROC, I2NSNT, I2NSN_L, NIR,
     .         LCNI2_L, NISKYI2_L, NBDDI2M, NSKYLL_L, NBI18_L,NSKYI18_L,
     .         NUMELS_L ,NUMELS8_L ,NUMELS10_L,NUMELS16_L,NUMELS20_L,
     .         NUMELC_L ,NUMELT_L  ,NUMELP_L  ,NUMELR_L  ,NUMELTG_L,
     .         NUMELQ_L ,NUMELUR_L, NSKYRW_L, NSKYRBK_L, NCONLD_L,
     .         NUMELTG6_L, NNMV_L, NNMVC_L, NSKYRBM_L,
     .         ADDCNE(0:NUMNOD+1), CNE(*), NODGLOB(*), CEP(*), CEL(*),
     .         IXS(NIXS,*),IXS10(6,*),IXS20(12,*),IXS16(8,*),
     .         IXQ(NIXQ,*),IXC(NIXC,*),IXT(NIXT,*),IXP(NIXP,*),
     .         IXR(NIXR,*),IXUR(NIXUR,*),IXTG(NIXTG,*),IXTG6(4,*),
     .         IB(NIBCLD,*),MONVOL(*), NPRW(*),
     .         LPRW(*), NPBY(NNPBY,*), LPBY(*),
     .         DD_RBY2(3,NRBYKIN), IPARI(NPARI,*),
     .         CEPI2(*), CELI2(*), CNI2(*), ADDCNI2(0:NUMNOD+1),
     .         NNLINK(10,*), LLLINK(*),
     .         DD_RBM2(3,NIBVEL), IBVEL(NBVELP,*), LBVEL(*),LEN_IA,
     .         NCONV_L  ,IBCV(NICONV,*),NSKYRBE3_L,
     .         IRBE3(NRBE3L,*),LRBE3(*),NSKYRBMK_L,
     .         IRBYM(NIRBYM,*) , LCRBYM(*) ,FRONT_RM(NRBYM,*),
     .         DD_RBYM2(3,NRBYM), IBCR(NIRADIA,*), NRADIA_L,
     .         CNE_PXFEM(*),ADDCNE_PXFEM(0:NPLYXFE + 1),CEL_PXFEM(*),
     .         NUMELCPXFEM_L,NUMNODPXFEM_L,INOD_PXFEM(*),IEL_PXFEM(*),
     .         LCNEPXFEM_L,LLOADP(*),ILOADP(SIZLOADP,*),LLLOADP_L,
     .         CNE_CRKXFEM(*),ADDCNE_CRKXFEM(0:NCRKXFE+1),
     .         CEL_CRKXFEM(*),NUMELCCRKXFE_L,NUMNODCRKXFE_L,
     .         INOD_CRKXFEM(*),IEL_CRKXFEM(*),LCNECRKXFEM_L,
     .         NUMELTGCRKXFE_L,CEP_CRKXFEM(*),INOD_CRK_L(*),
     .         CRKNODIAD(*),NUMELIG3D_L,KXIG3D(NIXIG3D,*),IXIG3D(*),
     .         CEPCND(*),CELCND(*),ADDCNCND(0:*),CNCND(*),NS10E_L,ICNDS10(3,*),
     .         LCNCND_L,ITAGND(*),IGEO(NPROPGI,*)
      INTEGER  NFXFLUX_L,IBFFLUX(NITFLUX,*) 
      my_real
     .        GEO(NPROPG,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
        TYPE(SURF_), DIMENSION(NSURF,NSPMD), INTENT(IN) :: IGRSURF_PROC
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       IGRSURF_PROC : SURF_ ; dimension=NSURF*NSPMD
!                 local surface property array (=IGRSURF for each proc)
!                 %ELTYP --> type of element (shell, triangle...)
!                 %ELEM  --> element id
!                 %NSEG --> total element number
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
      INTEGER, INTENT(IN) :: LOCAL_NEBCS ! number of parallelized ebcs 
      TYPE(T_EBCS_TAB), INTENT(INOUT) :: EBCS_TAB_LOC_2 ! ebcs structure
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL         
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I, PROC_L, CC, CC_L, N1, N2, N3, N4,
     .        K, K0, K1, K6, NV, KN, JJ, INACTI,NG,NUMG0,
     .        IS,NN,IAD,J,ITY,CLOAD,NUML,NUMG, II, MAIN,J_L,IPVENT,
     .        NSL, NSL_L, KK, P,K_ L, MSR, PMAIN, NTY, NRTS,
     .        NRTM, NSN, NMN, K10, K11, K12, K13, K14, L, NSN_L, OFFTG,
     .        OFFC,ITYP,NVENT,IV,IADHOL,KIBHOL,KIBJET,K2,NNC,KAD,NAV,J0,
     .        PROCNE(LCNE_L),
     .        ITAGIB(NCONLD),
     .        IADMV(4,NNMV_L),IADMV2(NNMV_L),IADMV3(NNMVC_L),
     .        IADWAL(NSKYRW_L),IADRBK(NSKYRBK_L),IDEBRBK(NSPMD),
     .        IADI2(NIR,I2NSN_L),I2TMP(NIR,I2NSN_L),IADLL(NSKYLL_L),
     .        PROCNI2(LCNI2_L),IADRBM(NSKYRBM_L),
     .        IADI18(NSKYI18_L),  IADIBCV(4,NCONV_L), IADIBFX(4,NFXFLUX_L),
     .        IADRBMK(NSKYRBMK_L), IADIBCR(4,NRADIA_L),
     .        PROCNE_PXFEM(LCNEPXFEM_L),IADC_PXFEM(4,NUMELCPXFEM_L),
     .        ADDCNEPXFEM_L(NUMNODPXFEM_L+1),NL_L,N0, 
     .        ITAGLOADP(SLLOADP),IADLOAD(4,LLLOADP_L),
     .        PROCNE_CRKXFEM(LCNECRKXFEM_L),
     .        ADDCNECRKXFEM_L(NUMNODCRKXFE_L+1),
     .        IADC_CRKXFEM(4,NUMELCCRKXFE_L),CNE_CRKXFEM_L(LCNECRKXFEM_L),
     .        IADTG_CRKXFEM(3,NUMELTGCRKXFE_L),CEL_CRKXFEM_L(LCNECRKXFEM_L),
     .        CRKNODIAD_L(LCNECRKXFEM_L),NRTM_FE,NRTS_FE,ICNDTMP(3,NS10E_L),
     .        PROCNCND(LCNCND_L),IADCND(2,NS10E_L),N_L
      INTEGER IUN,EMPL,COORD,SHFT,TESTVAL,KD(50),KFI
      INTEGER, DIMENSION(:), ALLOCATABLE :: SOLTAG,SOL10TAG,
     .      SOL20TAG,SOL16TAG,QUADTAG,SHTAG,TTAG,PTAG,RTAG,TGTAG,TG6TAG,
     .      URTAG,IBTAG,IBCVTAG,IBCRTAG,IBFXTAG,ILTAG,TAGIG3D
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAGC, ITAGTG,ADDCNE_L,ADDCNI2_L,
     .                                      ADDCNCND_L
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IADS,IADS10,
     .        IADS16,IADS20,IADQ,IADC,IADT,
     .        IADP,IADR,IADTG,IADUR, IADIB,
     .        IADTG1,IADIG3D
      TYPE(ebcs_parith_on), DIMENSION(:), ALLOCATABLE :: EBCS_PARITHON_L ! adress for the fsky array for parith/on
      LOGICAL, DIMENSION(:), ALLOCATABLE :: EBCS_TAG ! boolean : true if the element belongs to an ebcs
      INTEGER :: LOCAL_NODE_ID,ELEM_ID,NUMG_SAVE
C-----------------------------------------------
      IUN = 1
      ALLOCATE(SOLTAG(NUMELS))
      SOLTAG(1:NUMELS)=0

      ALLOCATE(SOL10TAG(NUMELS10))
      SOL10TAG(1:NUMELS10)=0

      ALLOCATE(SOL20TAG(NUMELS20))
      SOL20TAG(1:NUMELS20)=0

      ALLOCATE(SOL16TAG(NUMELS16))
      SOL16TAG(1:NUMELS16)=0

      ALLOCATE(QUADTAG(NUMELQ))
      QUADTAG(1:NUMELQ)=0

      ALLOCATE(SHTAG(NUMELC))
      SHTAG(1:NUMELC)=0

      ALLOCATE(TTAG(NUMELT))
      TTAG(1:NUMELT)=0

      ALLOCATE(PTAG(NUMELP))
      PTAG(1:NUMELP)=0

      ALLOCATE(RTAG(NUMELR))
      RTAG(1:NUMELR)=0

      ALLOCATE(TGTAG(NUMELTG))
      TGTAG(1:NUMELTG)=0

      ALLOCATE(TG6TAG(NUMELTG6))
      TG6TAG(1:NUMELTG6)=0

      ALLOCATE(URTAG(NUMELUR))
      URTAG(1:NUMELUR)=0

      ALLOCATE(IBTAG(NCONLD))
      IBTAG(1:NCONLD)=0

      ALLOCATE(IBCVTAG(NUMCONV))
      IBCVTAG(1:NUMCONV)=0

      ALLOCATE(IBCRTAG(NUMRADIA))
      IBCRTAG(1:NUMRADIA)=0

      ALLOCATE(IBFXTAG(NFXFLUX))
      IBFXTAG(1:NFXFLUX)=0

      ALLOCATE(ILTAG(SLLOADP/4))
      ILTAG(1:SLLOADP/4)=0

      ALLOCATE(TAGIG3D(NUMELIG3D))
      TAGIG3D(1:NUMELIG3D)=0
! ------------------------------
! allocate 1d arrays
      ALLOCATE( ITAGC(NUMELC),ITAGTG(NUMELTG) )
      ALLOCATE( ADDCNE_L(NUMNOD_L+1),ADDCNI2_L(NUMNOD_L+1))
      ADDCNE_L(1:NUMNOD_L + 1) = 0
      ALLOCATE( ADDCNCND_L(NUMNOD_L+1))
!     IAD 2D arrays
      ALLOCATE( IADS(8,NUMELS_L),IADS10(6,NUMELS10_L) )
      ALLOCATE( IADS16(8,NUMELS16_L),IADS20(12,NUMELS20_L) )
      ALLOCATE( IADQ(4,NUMELQ_L),IADC(4,NUMELC_L) )
      ALLOCATE( IADT(2,NUMELT_L),IADP(2,NUMELP_L) ) 
      ALLOCATE( IADR(3,NUMELR_L),IADTG(3,NUMELTG_L) )
      IADR(1:3,1:NUMELR_L) = 0
      IADTG(1:3,1:NUMELTG_L) = 0
      ALLOCATE( IADUR(4,NUMELUR_L), IADIB(4,NCONLD_L) )
      ALLOCATE( IADTG1(3,NUMELTG6_L),IADIG3D(100,NUMELIG3D_L) )
! ------------------------------

C-----------------------------------------------
C
C Pre-traitement ploads
C
      CLOAD = 0
      DO I = 1, NCONLD
        IF(IB(4,I)==-1)THEN
          ITAGIB(I) = 1
          CLOAD = 1
        ELSE
          ITAGIB(I) = 0
        ENDIF
      ENDDO
C Pre-traitement loads
C
      K=0
      DO I = 1, NLOADP
        DO J=1,ILOADP(1,I)/4
          K = K+1
          ITAGLOADP(K) = 0
        ENDDO
      ENDDO
C
C Pre-traitement mv
C
      IF (NVOLU>0) THEN
        DO I = 1, NUMELC
          ITAGC(I) = 0
        ENDDO
        DO I = 1, NUMELTG
          ITAGTG(I) = 0
        ENDDO
C
        K0 = 0
        K1 = 1
        K2 = 1 + NIMV*NVOLU
        KIBJET = K2  + LICBAG
        KIBHOL = KIBJET + LIBAGJET
        K6 = 0
        OFFC = NUMELS+NUMELQ
        OFFTG =NUMELS+NUMELQ+ NUMELC+NUMELT+NUMELP+NUMELR
        J_L = 0
        DO N = 1, NVOLU
          ITYP  = MONVOL(K1+1)
          IS    = MONVOL(K1+3)
          NAV   = MONVOL(K1+2)
          NVENT = MONVOL(K1+10)
          NN    = IGRSURF(IS)%NSEG
          IADHOL= KIBHOL+MONVOL(K1+11)
          J0 = J_L
          DO J = 1, NN
            ITY = IGRSURF(IS)%ELTYP(J)
            I   = IGRSURF(IS)%ELEM(J)
            IF (ITY==3) THEN
              ITAGC(I) = 1
              IF(CEP(I+OFFC)==PROC-1) THEN
                J_L = J_L + 1
                IADMV2(J_L) = J
C sauvegarde du no local J correspondant a I
                ITAGC(I) = J_L - J0
              END IF
            ELSEIF (ITY==7) THEN
              ITAGTG(I) = 1
              IF(CEP(I+OFFTG)==PROC-1) THEN
                J_L = J_L + 1
                IADMV2(J_L) = J
C sauvegarde du no local J correspondant a I
                ITAGTG(I) = J_L - J0
              END IF
            ELSE
            ENDIF
          ENDDO
C
C Traitement vent hole et volume communicant
C
          IF(ITYP==3.OR.ITYP==4.OR.ITYP==5.OR.ITYP==7.OR.ITYP==9) THEN
            DO IV = 1, NVENT
              IPVENT = MONVOL(IADHOL+NIBHOL*(IV-1)+2-1)
              IF(IPVENT/=0) THEN
                NNC=IGRSURF(IPVENT)%NSEG
                DO J = 1, NNC
                  ITY = IGRSURF(IPVENT)%ELTYP(J)
                  I   = IGRSURF(IPVENT)%ELEM(J)
                  IF (ITY==3) THEN
                    IF(CEP(I+OFFC)==PROC-1) THEN
                      K0 = K0 + 1
C restitution du no local J (surface) correspondant a I
                      IADMV3(K0) = ITAGC(I)
                    END IF
                  ELSEIF (ITY==7) THEN
                    IF(CEP(I+OFFTG)==PROC-1) THEN
                      K0 = K0 + 1
C restitution du no local J (surface) correspondant a I
                      IADMV3(K0) = ITAGTG(I)
                    END IF
                  END IF
                END DO
              END IF
            END DO
          END IF
          IF(ITYP==4.OR.ITYP==5.OR.ITYP==7.OR.ITYP==9)THEN
            DO IV = 1, NAV
              IPVENT = MONVOL(K2+NICBAG*(IV-1)+2-1)
              IF(IPVENT/=0) THEN
                NNC=IGRSURF(IPVENT)%NSEG
                DO J = 1, NNC
                  ITY = IGRSURF(IPVENT)%ELTYP(J)
                  I   = IGRSURF(IPVENT)%ELEM(J)
                  IF (ITY==3) THEN
                    IF(CEP(I+OFFC)==PROC-1) THEN
                      K0 = K0 + 1
C restitution du no local J (surface) correspondant a I
                      IADMV3(K0) = ITAGC(I)
                    END IF
                  ELSEIF (ITY==7) THEN
                    IF(CEP(I+OFFTG)==PROC-1) THEN
                      K0 = K0 + 1
C restitution du no local J (surface) correspondant a I
                      IADMV3(K0) = ITAGTG(I)
                    END IF
                  END IF
                END DO
              END IF
            END DO
          END IF
          K1 = K1 + NIMV
          K2 = K2 + NICBAG * NAV
          K6 = K6 + NN
        ENDDO
      ENDIF
C
      DO K = 1, 4
        DO I = 1, NNMV_L
          IADMV(K,I) = 0
        END DO
      END DO
C
C Elts penta
C
      IF(NUMELTG6_L>0)THEN
        DO I = 1, NUMELTG6_L
          DO K = 1,3
            IADTG1(K,I)=0
          ENDDO
        ENDDO
      ENDIF

      ! --------------------------
      ! tag the element belonging to an ebcs
      ALLOCATE( EBCS_TAG(NUMELS+NUMELQ+NUMELTG) )
      EBCS_TAG(1:NUMELS+NUMELQ+NUMELTG) = .FALSE.
      ALLOCATE(EBCS_PARITHON_L(LOCAL_NEBCS))
      IF(LOCAL_NEBCS>0) THEN
        ! ---------------------
        ! loop over the /EBCS
        DO I=1,LOCAL_NEBCS
            ! allocation of adress array
            ALLOCATE( EBCS_PARITHON_L(I)%ELEM_ADRESS(4,EBCS_TAB_LOC_2%tab(I)%poly%nb_elem) )
            EBCS_PARITHON_L(I)%ELEM_ADRESS(1:4,1:EBCS_TAB_LOC_2%tab(I)%poly%nb_elem) = 0
            ! check if a surface is associated to the ebcs
            IF(EBCS_TAB_LOC_2%tab(I)%poly%surf_id>0) THEN
                ! ---------------------
                ! loop over the element of the surface to tag the element
                DO J=1,EBCS_TAB_LOC_2%tab(I)%poly%nb_elem
                    ELEM_ID = EBCS_TAB_LOC_2%tab(I)%poly%global_ielem(J)
                    IF(N2D/=0) THEN
                        IF(ELEM_ID>NUMELS+NUMELQ) THEN
                            ELEM_ID = ELEM_ID - (NUMELC+NUMELT+NUMELP+NUMELR)
                        ENDIF
                    ENDIF
                    EBCS_TAG(ELEM_ID) = .TRUE.
                ENDDO
                ! ---------------------
            ENDIF
        ENDDO
        ! ---------------------
      ENDIF
      ! --------------------------
C-----------------------------------------------
C ADDCNE_L et IADS
C-----------------------------------------------
C
C-----------------------------------------------
C The algorithm has been modified to avoid using the element
C arrays as markers.
C INTEGER arrays for every elements types has been introduced
C Each entry is used as 31 bit array.
C The access is done with fortran intrinsics ISHIFT
C Verification with fortran Intrinsics IAND.
C-----------------------------------------------
C
      ADDCNE_L(1) = 1
      CC_L = 0
      DO I = 1, NUMNOD_L

        N = NODGLOB(I)
        N1 = ADDCNE(N)
        N2 = ADDCNE(N+1)
        ADDCNE_L(I+1) = ADDCNE_L(I) + N2-N1
        DO CC = N1, N2-1
          NUMG = CNE(CC)
          NUMG_SAVE = CNE(CC)
 
          NUML = CEL(NUMG)
          PROC_L = CEP(NUMG)+1 
          CC_L = CC_L + 1
          PROCNE(CC_L) = PROC_L
C
C Remplissage IADX si elt interne
C
          IF (PROC==PROC_L) THEN
C proc loc
           IF (NUMG<=NUMELS) THEN
             DO K = 1,8
               SHFT = ISHFT(IUN,K-1)
               TESTVAL = IAND(SOLTAG(NUMG),SHFT)
               IF (IXS(K+1,NUMG)==N.AND.TESTVAL==0) THEN
                 IADS(K,NUML) = CC_L
                 SOLTAG(NUMG)=SOLTAG(NUMG)+SHFT
                 GOTO 100
               ENDIF
             ENDDO
C
             IF(NUMELS10>0.AND.NUMG>NUMELS8.AND.
     +          NUMG<=NUMELS8+NUMELS10) THEN
               NUMG=NUMG-NUMELS8
               DO K=1,6
                 SHFT = ISHFT(IUN,K-1)
                 TESTVAL = IAND(SOL10TAG(NUMG),SHFT)
                 IF (IXS10(K,NUMG)==N.AND.TESTVAL==0) THEN
                   IADS10(K,NUML-NUMELS8_L) = CC_L
                   SOL10TAG(NUMG)=SOL10TAG(NUMG)+SHFT
                   GOTO 100
                 ENDIF
               ENDDO
             ELSEIF(NUMELS20>0.AND.NUMG>NUMELS8+NUMELS10.AND.
     +          NUMG<=NUMELS8+NUMELS10+NUMELS20)THEN
               NUMG=NUMG-NUMELS8-NUMELS10
               DO K=1,12
                 SHFT = ISHFT(IUN,K-1)
                 TESTVAL = IAND(SOL20TAG(NUMG),SHFT)
                 IF (IXS20(K,NUMG)==N.AND.TESTVAL==0 ) THEN
                   IADS20(K,NUML-NUMELS8_L-NUMELS10_L) = CC_L
                   SOL20TAG(NUMG)=SOL20TAG(NUMG)+SHFT
                   GOTO 100
                 ENDIF
               ENDDO
             ELSEIF(NUMELS16>0.AND.
     +              NUMG>NUMELS8+NUMELS10+NUMELS20)THEN
               NUMG=NUMG-NUMELS8-NUMELS10-NUMELS20
               DO K=1,8
                 SHFT = ISHFT(IUN,K-1)
                 TESTVAL =IAND(SOL16TAG(NUMG),SHFT)
                 IF (IXS16(K,NUMG)==N.AND.TESTVAL==0 ) THEN
                   IADS16(K,NUML-NUMELS8_L-NUMELS10_L-NUMELS20_L) = CC_L
                   SOL16TAG(NUMG)=SOL16TAG(NUMG)+SHFT
                   GOTO 100
                 ENDIF
               ENDDO
             ENDIF

             ! --------------------
             ! element belongs to an ebcs
             IF(EBCS_TAG(NUMG_SAVE)) THEN
                DO II=1,LOCAL_NEBCS
                ! check if a surface is associated to the ebcs
                    IF(EBCS_TAB_LOC_2%tab(II)%poly%surf_id>0) THEN
                        ! -------------
                        ! loop over the element of the surface
                        DO J=1,EBCS_TAB_LOC_2%tab(II)%poly%nb_elem
                            ELEM_ID = EBCS_TAB_LOC_2%tab(II)%poly%global_ielem(J) ! global element id
                            ! -------------
                            ! find the location of the node :
                            ! 4 nodes for a solid
                            ! 1     2
                            ! o-----o
                            ! |     |
                            ! o-----o
                            ! 4     3
                            IF(ELEM_ID==NUMG_SAVE) THEN
                                DO K=1,4
                                    LOCAL_NODE_ID = EBCS_TAB_LOC_2%tab(II)%poly%elem_list(K,J)
                                    LOCAL_NODE_ID = EBCS_TAB_LOC_2%tab(II)%poly%node_list(LOCAL_NODE_ID)
                                    IF(N==NODGLOB(LOCAL_NODE_ID)) THEN
                                        IF(EBCS_PARITHON_L(II)%ELEM_ADRESS(K,J)==0) THEN
                                            EBCS_PARITHON_L(II)%ELEM_ADRESS(K,J) = CC_L
                                            GOTO 100
                                        ENDIF                                        
                                    ENDIF
                                ENDDO
                            ENDIF
                            ! -------------
                        ENDDO
                        ! -------------
                    ENDIF
                ENDDO
             ENDIF

             ! --------------------
C
           ELSEIF(NUMG<=NUMELS+NUMELQ) THEN
             DO K=1,4
               SHFT = ISHFT(IUN,K-1)
               TESTVAL =IAND(QUADTAG(NUMG),SHFT)
               IF (IXQ(K+1,NUMG)==N.AND.TESTVAL==0) THEN
                 IADQ(K,NUML) = CC_L
                 QUADTAG(NUMG)=QUADTAG(NUMG)+SHFT
                 GOTO 100
               ENDIF
             ENDDO
             ! --------------------
             ! element belongs to an ebcs
             IF(EBCS_TAG(NUMG_SAVE)) THEN
                DO II=1,LOCAL_NEBCS
                ! check if a surface is associated to the ebcs
                    IF(EBCS_TAB_LOC_2%tab(II)%poly%surf_id>0) THEN
                        ! -------------
                        ! loop over the element of the surface
                        DO J=1,EBCS_TAB_LOC_2%tab(II)%poly%nb_elem
                            ELEM_ID = EBCS_TAB_LOC_2%tab(II)%poly%global_ielem(J) ! global element id
                            ! -------------
                            ! find the location of the node for a quad :
                            ! only 2 nodes for the surface (2D case) 
                            ! 1     2
                            ! o-----o
                            ! |     |
                            ! o-----o
                            ! 4     3
                            IF(ELEM_ID==NUMG_SAVE) THEN
                                DO K=1,2
                                    LOCAL_NODE_ID = EBCS_TAB_LOC_2%tab(II)%poly%elem_list(K,J)
                                    LOCAL_NODE_ID = EBCS_TAB_LOC_2%tab(II)%poly%node_list(LOCAL_NODE_ID)
                                    IF(N==NODGLOB(LOCAL_NODE_ID)) THEN
                                        IF(EBCS_PARITHON_L(II)%ELEM_ADRESS(K,J)==0) THEN
                                            EBCS_PARITHON_L(II)%ELEM_ADRESS(K,J) = CC_L
                                            GOTO 100
                                        ENDIF
                                    ENDIF
                                ENDDO
                            ENDIF
                            ! -------------
                        ENDDO
                        ! -------------
                    ENDIF
                ENDDO
             ENDIF
             ! --------------------

           ELSEIF(NUMG<=NUMELS+NUMELQ+NUMELC) THEN
             NUMG = NUMG - (NUMELS+NUMELQ)
             DO K=1,4
               SHFT = ISHFT(IUN,K-1)
               TESTVAL =IAND(SHTAG(NUMG),SHFT)
               IF (IXC(K+1,NUMG)==N.AND.TESTVAL==0) THEN
                 IADC(K,NUML) = CC_L
                 SHTAG(NUMG) = SHTAG(NUMG)+SHFT
                 GOTO 100
               ENDIF
             ENDDO
C mv coque
             IF (NVOLU>0) THEN
               IF(ITAGC(NUMG)>0) THEN
                 K1 = 1
                 K6 = 0
                 DO NV = 1, NVOLU
                   IS = MONVOL(K1+3)
                   NN = IGRSURF_PROC(IS,PROC)%NSEG
                   JJ = 0
                   DO J = 1, NN
                     ITY = IGRSURF_PROC(IS,PROC)%ELTYP(J)
                     II = IGRSURF_PROC(IS,PROC)%ELEM(J)
                     IF(ITY==3) THEN
                       IF(CEP(OFFC+II)==PROC-1) THEN
                         JJ = JJ+1
                         IF (II==NUMG) THEN
                           DO K = 2,5
                             IF(IXC(K,II)==N.AND.
     .                          IADMV(K-1,K6+JJ)==0) THEN
                               IADMV(K-1,K6+JJ) = CC_L
                               GOTO 100
                             END IF
                           END DO
                         END IF
                       END IF
                     ELSEIF(ITY==7)THEN
                       IF(CEP(OFFTG+II)==PROC-1) THEN
                         JJ = JJ+1
                       END IF
                     END IF
                   END DO
                   K1 = K1 + NIMV
                   K6 = K6 + JJ
                 ENDDO
               ENDIF
             ENDIF
C
           ELSEIF(NUMG<=NUMELS+NUMELQ+NUMELC+NUMELT) THEN
             NUMG = NUMG - (NUMELS+NUMELQ+NUMELC)
             DO K=1,2
               SHFT = ISHFT(IUN,K-1)
               TESTVAL =IAND(TTAG(NUMG),SHFT)
               IF (IXT(K+1,NUMG)==N.AND.TESTVAL==0) THEN
                 IADT(K,NUML) = CC_L
                 TTAG(NUMG)=TTAG(NUMG)+SHFT
                 GOTO 100
               ENDIF
             ENDDO
           ELSEIF(NUMG<=NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP) THEN
             NUMG = NUMG - (NUMELS+NUMELQ+NUMELC+NUMELT)
             DO K=1,2
               SHFT = ISHFT(IUN,K-1)
               TESTVAL =IAND(PTAG(NUMG),SHFT)
               IF (IXP(K+1,NUMG)==N.AND.TESTVAL==0) THEN
                 IADP(K,NUML) = CC_L
                 PTAG(NUMG)=PTAG(NUMG)+SHFT
                 GOTO 100
               ENDIF
             ENDDO
           ELSEIF(NUMG<=NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+
     .            NUMELR) THEN
             NUMG = NUMG - (NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP)
             DO K=1,2
               SHFT = ISHFT(IUN,K-1)
               TESTVAL =IAND(RTAG(NUMG),SHFT)
               IF (IXR(K+1,NUMG)==N.AND.TESTVAL==0) THEN
                 IADR(K,NUML) = CC_L
                 RTAG(NUMG)=RTAG(NUMG)+SHFT
                 GOTO 100
               ENDIF
             ENDDO
             IF(IGEO(11,IXR(1,NUMG))==12) THEN
               SHFT = ISHFT(IUN,3)
               TESTVAL =IAND(RTAG(NUMG),SHFT)
               IF (IXR(4,NUMG)==N.AND.TESTVAL==0) THEN
                 IADR(3,NUML) = CC_L
                 RTAG(NUMG)=RTAG(NUMG)+SHFT
                 GOTO 100
               ENDIF
             ENDIF
           ELSEIF(NUMG<=NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+
     .            NUMELR+NUMELTG) THEN
             NUMG = NUMG - (NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR)
             DO K=1,3
               SHFT = ISHFT(IUN,K-1)
               TESTVAL =IAND(TGTAG(NUMG),SHFT)
               IF (IXTG(K+1,NUMG)==N.AND.TESTVAL==0) THEN
                 IADTG(K,NUML) = CC_L
                 TGTAG(NUMG)=TGTAG(NUMG)+SHFT
                 GOTO 100
               ENDIF
             ENDDO
C
             IF(NUMELTG6>0.AND.
     .          NUMG>NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+
     .                  NUMELR+NUMELTG-NUMELTG6.AND.
     .          NUMG<=NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+
     .                  NUMELR+NUMELTG)THEN
               NUMG=NUMG-NUMELTG+NUMELTG6
               DO K=1,3
                 SHFT = ISHFT(IUN,K-1)
                 TESTVAL =IAND(TG6TAG(NUMG),SHFT)
                 IF (IXTG6(K,NUMG)==N.AND.TESTVAL==0) THEN
                   IADTG1(K,NUML-NUMELTG_L+NUMELTG6_L) = CC_L
                   TG6TAG(NUMG)=TG6TAG(NUMG)+SHFT
                   GOTO 100
                 ENDIF
               ENDDO
             ENDIF
C
C mv coque triangle
             IF (NVOLU>0) THEN
               IF(ITAGTG(NUMG)>0) THEN
                 K1 = 1
                 K6 = 0
                 DO NV = 1, NVOLU
                   IS = MONVOL(K1+3)
                   NN = IGRSURF_PROC(IS,PROC)%NSEG
                   JJ = 0
                   DO J = 1, NN
                     ITY = IGRSURF_PROC(IS,PROC)%ELTYP(J)
                     II  = IGRSURF_PROC(IS,PROC)%ELEM(J)
                     IF(ITY==7) THEN
                       IF(CEP(OFFTG+II)==PROC-1) THEN
                         JJ = JJ+1
                         IF (II==NUMG) THEN
                           DO K = 2,4
                             IF(IXTG(K,II)==N.AND.
     .                          IADMV(K-1,K6+JJ)==0) THEN
                               IADMV(K-1,K6+JJ) = CC_L
                               GOTO 100
                             END IF
                           END DO
                         END IF
                       END IF
                     ELSEIF(ITY==3) THEN
                       IF(CEP(OFFC+II)==PROC-1) THEN
                         JJ = JJ+1
                       END IF
                     END IF
                   END DO
                   K1 = K1 + NIMV
                   K6 = K6 + JJ
                 ENDDO
               ENDIF
             ENDIF

             ! --------------------
             ! element belongs to an ebcs
             IF(EBCS_TAG(NUMG_SAVE-(NUMELC+NUMELT+NUMELP+NUMELR))) THEN
                DO II=1,LOCAL_NEBCS
                ! check if a surface is associated to the ebcs
                    IF(EBCS_TAB_LOC_2%tab(II)%poly%surf_id>0) THEN
                        ! -------------
                        ! loop over the element of the surface
                        DO J=1,EBCS_TAB_LOC_2%tab(II)%poly%nb_elem
                            ELEM_ID = EBCS_TAB_LOC_2%tab(II)%poly%global_ielem(J) ! global element id
                            ! -------------
                            ! find the location of the node for a triangle : 
                            ! only 2 nodes for the surface (2D case) 
                            !    1
                            !    o
                            !   / \
                            !  /   \
                            ! o-----o
                            ! 3     2
                            IF(ELEM_ID==NUMG_SAVE) THEN
                                DO K=1,2
                                    LOCAL_NODE_ID = EBCS_TAB_LOC_2%tab(II)%poly%elem_list(K,J)
                                    IF(LOCAL_NODE_ID>0) THEN
                                        IF(N==NODGLOB(LOCAL_NODE_ID)) THEN
                                            IF(EBCS_PARITHON_L(II)%ELEM_ADRESS(K,J)==0) THEN
                                                EBCS_PARITHON_L(II)%ELEM_ADRESS(K,J) = CC_L
                                                GOTO 100
                                            ENDIF
                                        ENDIF
                                    ENDIF
                                ENDDO
                            ENDIF
                            ! -------------
                        ENDDO
                        ! -------------
                    ENDIF
                ENDDO
             ENDIF
             ! --------------------
C
           ELSEIF(NUMG<=NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+
     .            NUMELR+NUMELTG+NUMELUR) THEN
             NUMG = NUMG - (NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR+
     +                      NUMELTG)
             DO K=1,4
               SHFT = ISHFT(IUN,K-1)
               TESTVAL =IAND(URTAG(NUMG),SHFT)
               IF (IXUR(K+1,NUMG)==N.AND.TESTVAL==0) THEN
                 IADUR(K,NUML) = CC_L
                 URTAG(NUMG)=URTAG(NUMG)+SHFT
                 GOTO 100
               ENDIF
             ENDDO
           ELSEIF(NUMG<=NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+
     .            NUMELR+NUMELTG+NUMELUR+NUMELX+NCONLD)THEN
             NUMG = NUMG - (NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR+
     +                      NUMELTG+NUMELUR+NUMELX)
             IF(ITAGIB(NUMG)==0.AND.N2D==0)THEN
               KN = 4
             ELSEIF(ITAGIB(NUMG)==0.AND.N2D/=0)THEN
               KN = 2
             ELSE
               KN = 1
             ENDIF
             DO K=1,KN
               SHFT = ISHFT(IUN,K-1)
               TESTVAL =IAND(IBTAG(NUMG),SHFT)
               IF (IB(K,NUMG)==N.AND.TESTVAL==0) THEN
                 IADIB(K,NUML) = CC_L
                 IBTAG(NUMG)=IBTAG(NUMG)+SHFT
                 GOTO 100
               ELSE
               ENDIF
             ENDDO
C
           ELSEIF(NUMG<=NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+
     .            NUMELR+NUMELTG+NUMELUR+NUMELX+ NCONLD + NUMCONV)THEN
             NUMG = NUMG - (NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR+
     +                      NUMELTG+NUMELUR+NUMELX+NCONLD)
             IF(N2D==0)THEN
               KN = 4
             ELSEIF(N2D/=0)THEN
               KN = 2
             ELSE
               KN = 1
             ENDIF
             DO K=1,KN
               SHFT = ISHFT(IUN,K-1)
               TESTVAL =IAND(IBCVTAG(NUMG),SHFT)
               IF (IBCV(K,NUMG)==N.AND.TESTVAL==0) THEN
                 IADIBCV(K,NUML) = CC_L
                 IBCVTAG(NUMG)=IBCVTAG(NUMG)+SHFT
                 GOTO 100
               ELSE
               ENDIF
             ENDDO
C
           ELSEIF(NUMG<=NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+
     .            NUMELR+NUMELTG+NUMELUR+NUMELX+ NCONLD + NUMCONV +
     .            NUMRADIA)THEN
             NUMG = NUMG - (NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR+
     +                      NUMELTG+NUMELUR+NUMELX+NCONLD+NUMCONV)
             IF(N2D==0)THEN
               KN = 4
             ELSEIF(N2D/=0)THEN
               KN = 2
             ELSE
               KN = 1
             ENDIF
             DO K=1,KN
               SHFT = ISHFT(IUN,K-1)
               TESTVAL =IAND(IBCRTAG(NUMG),SHFT)
               IF (IBCR(K,NUMG)==N.AND.TESTVAL==0) THEN
                 IADIBCR(K,NUML) = CC_L
                 IBCRTAG(NUMG)= IBCRTAG(NUMG)+SHFT
                 GOTO 100
               ELSE
               ENDIF
             ENDDO
C             
           ELSEIF(NUMG<=NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+
     .            NUMELR+NUMELTG+NUMELUR+NUMELX+ NCONLD + NUMCONV +
     .            NUMRADIA+NFXFLUX)THEN
             NUMG = NUMG - (NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR+
     +                      NUMELTG+NUMELUR+NUMELX+NCONLD+NUMCONV+NUMRADIA)
             IF(N2D==0)THEN
               KN = 4
             ELSEIF(N2D/=0)THEN
               KN = 2
             ELSE
               KN = 1
             ENDIF
             DO K=1,KN
               SHFT = ISHFT(IUN,K-1)
               TESTVAL =IAND(IBFXTAG(NUMG),SHFT)
               IF (IBFFLUX(K,NUMG)==N.AND.TESTVAL==0) THEN
                 IADIBFX(K,NUML) = CC_L
                 IBFXTAG(NUMG)= IBFXTAG(NUMG)+SHFT
                 GOTO 100
               ELSE
               ENDIF
             ENDDO
C
           ELSEIF(NUMG<=NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+
     .            NUMELR+NUMELTG+NUMELUR+NUMELX+ NCONLD + NUMCONV +
     .            NUMRADIA+NFXFLUX+SLLOADP/4)THEN
             NUMG = NUMG - (NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR+
     .                      NUMELTG+NUMELUR+NUMELX+NCONLD+NUMCONV+
     .                      NUMRADIA+NFXFLUX)
             IF(ITAGLOADP(NUMG)==0.AND.N2D==0)THEN
               KN = 4
             ELSEIF(ITAGLOADP(NUMG)==0.AND.N2D/=0)THEN
               KN = 2
             ELSE
               KN = 1
             ENDIF
             DO K=1,KN
               SHFT = ISHFT(IUN,K-1)
               TESTVAL =IAND(ILTAG(NUMG),SHFT)
               IF (LLOADP(4*(NUMG-1)+K)==N.AND.TESTVAL==0) THEN
                 IADLOAD(K,NUML) = CC_L
                 ILTAG(NUMG)=ILTAG(NUMG)+SHFT
                 GOTO 100
               ELSE
               ENDIF
             ENDDO
C             
           ELSEIF(NUMG<=NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+
     .            NUMELR+NUMELTG+NUMELUR+NUMELX+ NCONLD + NUMCONV +
     .            NUMRADIA+NFXFLUX+SLLOADP/4+NUMELIG3D)THEN
             NUMG = NUMG - (NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR+
     .                      NUMELTG+NUMELUR+NUMELX+NCONLD+NUMCONV+
     .                      NUMRADIA+NFXFLUX+SLLOADP/4)
             DO K = 1,20
               SHFT = ISHFT(IUN,K-1)
               TESTVAL = IAND(TAGIG3D(NUMG),SHFT)
               IF (IXIG3D(KXIG3D(4,NUMG)+K-1)==N.AND.TESTVAL==0) THEN
                 IADIG3D(K,NUML) = CC_L
                 TAGIG3D(NUMG)=TAGIG3D(NUMG)+SHFT
                 GOTO 100
               ENDIF
             ENDDO
C             
C             
           ELSE
             print *,'**error assadd2 unknown elem type'
           ENDIF
 100       CONTINUE
          ELSE
C proc dist
          ENDIF
         ENDDO
       ENDDO
C
Cply xfem
C
C
C ADDCNEPXFEM_L et IADC_PXFE
C
      IF(IPLYXFEM > 0) THEN
         ADDCNEPXFEM_L(1) = 1
         CC_L = 0
         NL_L = 0
         DO I = 1, NUMNOD_L

           NG =NODGLOB(I)
           N = INOD_PXFEM(NG)
           IF(N > 0 ) THEN
            NL_L = NL_L + 1
            N1 = ADDCNE_PXFEM(N)
            N2 = ADDCNE_PXFEM(N+1)
            ADDCNEPXFEM_L(NL_L + 1) = ADDCNEPXFEM_L(NL_L) + N2 - N1
            DO CC = N1, N2-1
              NUMG0 = CNE_PXFEM(CC) ! ----> 1:numelc
              N0 = IEL_PXFEM(NUMG0) !---> 1:el_pxfem
              NUML = CEL_PXFEM(N0)  ! ----> proc local
              NUMG = NUMG0 + NUMELS + NUMELQ
              PROC_L = CEP(NUMG)+1

              CC_L = CC_L + 1
              PROCNE_PXFEM(CC_L) = PROC_L
C
C Remplissage IADX si elt interne
C
             IF (PROC==PROC_L) THEN
C procloc
               IF(NUMG<=NUMELS+NUMELQ+NUMELC) THEN
                 NUMG = NUMG - (NUMELS+NUMELQ)
                 DO K=1,4
                  SHFT = ISHFT(IUN,K-1)
                  TESTVAL =IAND(SHTAG(NUMG),SHFT)
                   IF (IXC(K+1,NUMG)==NG.AND.TESTVAL/=0) THEN
                      IADC_PXFEM(K,NUML) = CC_L
                      SHTAG(NUMG)=SHTAG(NUMG)-SHFT
cc                     GOTO 100
                  ENDIF
                ENDDO
              ENDIF
cc 100          CONTINUE
C
             ENDIF
            ENDDO
           ENDIF
         ENDDO
       ENDIF
C------------------------------
C     crack xfem for layered shell
C     ADDCNECRKXFEM_L et IADC_CRKXFE
C------------------------------
      IF (ICRACK3D > 0) THEN
        IADC_CRKXFEM = 0
        CRKNODIAD_L  = 0
        ADDCNECRKXFEM_L(1) = 1
        CC_L = 0
        NL_L = 0
        DO I = 1,NUMNOD_L
          NG = NODGLOB(I) 
cc          N = INOD_CRKXFEM(NG)
cc          IF(N > 0)THEN
          IF (INOD_CRK_L(I) > 0) THEN
            N  = INOD_CRKXFEM(NG)    ! Num noeud systeme xfem global   
            N1 = ADDCNE_CRKXFEM(N)   ! adresse global                  
            N2 = ADDCNE_CRKXFEM(N+1)                                   
            ! N2 - N1 = nb d'elements std connectes au noeud N
            NL_L = NL_L + 1
            ADDCNECRKXFEM_L(NL_L+1) = ADDCNECRKXFEM_L(NL_L) + N2 - N1
c
            DO CC = N1,N2-1    ! global sky adr
              NUMG0 = CNE_CRKXFEM(CC)    ! -> 1:numelc+numeltg (elements systeme std)
              N0    = IEL_CRKXFEM(NUMG0) ! -> 1:el_crkxfem     (elements systeme xfem glob)
              NUML  = CEL_CRKXFEM(N0)    ! -> proc local       (N element xfem local/proc)
cc              NUMG = NUMG0 + NUMELS + NUMELQ
cc              PROC_L = CEP(NUMG)+1
              PROC_L = CEP_CRKXFEM(N0) + 1   ! proc de l'element sys_xfem
C
              CC_L = CC_L + 1
              PROCNE_CRKXFEM(CC_L) = PROC_L
C
C             Remplissage IADX si elt interne
C
              IF (PROC == PROC_L) THEN
                IF (N0 <= ECRKXFEC) THEN
                  NUMG = NUMG0
                  DO K=1,4
                    SHFT    = ISHFT(IUN,K-1)
                    TESTVAL = IAND(SHTAG(NUMG),SHFT)
                     IF (IXC(K+1,NUMG) == NG .AND. TESTVAL /= 0) THEN
                       IADC_CRKXFEM(K,NUML) = CC_L
c                       CNE_CRKXFEM_L(CC_L)  = CNE_CRKXFEM(CC)  ! contient NUMG0
                       CNE_CRKXFEM_L(CC_L)  = NUML   ! Num sys xfem local par proc (ELCRK)
                       CRKNODIAD_L(CC_L)    = CRKNODIAD(CC)
                       SHTAG(NUMG) = SHTAG(NUMG)-SHFT
                     ENDIF
                  ENDDO 
                ELSEIF (N0 > ECRKXFEC .AND. N0 <= ECRKXFEC+ECRKXFETG) THEN
                  NUMG = NUMG0 -NUMELC
                  DO K=1,3
                    SHFT    = ISHFT(IUN,K-1)
                    TESTVAL = IAND(TGTAG(NUMG),SHFT)
                    IF (IXTG(K+1,NUMG) == NG .AND. TESTVAL /= 0) THEN
                      IADTG_CRKXFEM(K,NUML) = CC_L
c                      CNE_CRKXFEM_L(CC_L)   = CNE_CRKXFEM(CC)
                      CNE_CRKXFEM_L(CC_L)   = NUML + NUMELCCRKXFE_L
                      CRKNODIAD_L(CC_L)     = CRKNODIAD(CC)
                      TGTAG(NUMG)=TGTAG(NUMG)-SHFT
                    ENDIF
                  ENDDO
                ENDIF   
              ENDIF  ! PROC==PROC_L
            ENDDO    ! CC = N1,N2-1
          ENDIF      ! INOD_CRK_L(I) > 0
        ENDDO        ! I = 1,NUMNOD_L
      ENDIF
C
C RWALL specifique type sliding
C
      K = 0
      K_L = 0
      DO N = 1, NRWALL
        N3 = 2*NRWALL+N
        NSL=NPRW(N)
        MSR = NPRW(N3)
        IF(MSR/=0) THEN
          IF(NLOCAL(MSR,PROC)==1) THEN
            NSL_L = 0
            DO KK = 1, NSL
              NN = LPRW(K+KK)
              IF(NLOCAL(NN,PROC)==1) THEN
                NSL_L = NSL_L + 1
                main = 0
                DO P = 1, PROC-1
                  IF(NLOCAL(NN,P)==1) THEN
                    GOTO 200
                  ENDIF
                ENDDO
                main = 1
 200            IF(MAIN==1) THEN
                  IADWAL(K_L+NSL_L) = KK
                ELSE
                  IADWAL(K_L+NSL_L) = 0
                ENDIF
              ENDIF
            ENDDO
            K_L = K_L + NSL_L
          ENDIF
        ENDIF
        K = K + NSL
      ENDDO
C
C RBY specifique
C
      IF(NSKYRBK_L>0)THEN
       DO P = 1, NSPMD
        IDEBRBK(P) = 0
       ENDDO
       K = 0
       NSL_L = 0
       DO N = 1, NRBYKIN
        MSR=NPBY(1,N)
        NSL=NPBY(2,N)
        PMAIN = ABS(DD_RBY2(3,N))
        IF(NLOCAL(MSR,PROC)==1) THEN
          DO KK = 1, NSL
            NN = LPBY(K+KK)
            IF(NLOCAL(NN,PROC)==1)THEN
              NSL_L = NSL_L + 1
              main = 0
              DO P = 1, PROC-1
                IF(NLOCAL(NN,P)==1)THEN
                  GOTO 300
                ENDIF
              ENDDO
              main = 1
 300          IF(MAIN==1) THEN
C numerotation fonction du pmain des rby precedents
                IADRBK(NSL_L) = KK+IDEBRBK(PMAIN)
              ELSE
                IADRBK(NSL_L) = 0
              ENDIF
            ENDIF
          ENDDO
        ENDIF
        K = K + NSL
        IDEBRBK(PMAIN) = IDEBRBK(PMAIN) + NSL
       ENDDO
      ENDIF
CC
C
C Rigid material specifique
C
      IF(NSKYRBMK_L>0)THEN
       DO P = 1, NSPMD
        IDEBRBK(P) = 0
       ENDDO
       K = 0
       NSL_L = 0
       DO N = 1, NRBYM
        MSR=IRBYM(1,N)
        NSL=IRBYM(2,N)
        PMAIN = ABS(DD_RBYM2(3,N))
        IF(MOD(FRONT_RM(MSR,PROC),10)==1) THEN
          DO KK = 1, NSL
            NN = LCRBYM(K+KK)
            IF(NLOCAL(NN,PROC)==1)THEN
              NSL_L = NSL_L + 1
              main = 0
              DO P = 1, PROC-1
                IF(NLOCAL(NN,P)==1)THEN         
                  GOTO 333
                ENDIF
              ENDDO
              main = 1
 333          IF(MAIN==1) THEN
C numerotation fonction du pmain des rigid material precedents
                IADRBMK(NSL_L) = KK+IDEBRBK(PMAIN)
              ELSE
                IADRBMK(NSL_L) = 0
              ENDIF
            ENDIF
          ENDDO
        ENDIF
        K = K + NSL
        IDEBRBK(PMAIN) = IDEBRBK(PMAIN) + NSL
       ENDDO
      ENDIF

CC
C
C Int 2 specifique
C
Cpseudo elt type 2
      IF(I2NSNT>0) THEN
        NSN_L = 0
        DO N = 1, NINTER
          NTY = IPARI(7,N)
          IF (NTY==2) THEN
            NRTS  = IPARI(3,N)
            NRTM  = IPARI(4,N)
            NSN   = IPARI(5,N)
            NMN   = IPARI(6,N)
            DO I=1,NSN
              L = INTBUF_TAB(N)%IRTLM(I)
              K = INTBUF_TAB(N)%NSV(I)
              IF(NLOCAL(K,PROC)==1) THEN              
                DO P = 1, PROC-1
                  IF(NLOCAL(K,P)==1) GO TO 202            
                ENDDO
                NSN_L = NSN_L + 1
                DO J=1,NIR
                  KK = INTBUF_TAB(N)%IRECTM((L-1)*4+J)
C                  I2TMP(J,I+OFF) = KK
                  I2TMP(J,NSN_L) = KK
                END DO
 202            CONTINUE
              END IF
            END DO
          END IF
        END DO
        if(nsn_l/=I2NSN_L)print *,'error decomp i2 p/on'
C
        ADDCNI2_L(1) = 1
        CC_L = 0
        DO I = 1, NUMNOD_L
          N = NODGLOB(I)
          N1 = ADDCNI2(N)
          N2 = ADDCNI2(N+1)
          ADDCNI2_L(I+1) = ADDCNI2_L(I) + N2-N1
          DO CC = N1, N2-1
            NUMG = CNI2(CC)
            NUML = CELI2(NUMG)
            PROC_L = CEPI2(NUMG)+1
            CC_L = CC_L + 1
            PROCNI2(CC_L) = PROC_L
C
C Remplissage IADI2 si elt interne
C
            IF (PROC==PROC_L) THEN
              DO K = 1, NIR
                IF(I2TMP(K,NUML)==N) THEN
                  IADI2(K,NUML) = CC_L
                  I2TMP(K,NUML) = -N
                  GO TO 222
                ENDIF
              END DO
 222          CONTINUE
            END IF
          END DO
        END DO
      ENDIF
C
C RLink specifique
C
      K = 0
      K_L = 0
      DO I = 1, NLINK
        NSL = NNLINK(1,I)
        NSL_L = 0
        DO J = 1, NSL
          N = LLLINK(K+J)
          IF (NLOCAL(N,PROC)==1)THEN      
            NSL_L = NSL_L + 1
            IADLL(K_L+NSL_L) = J
          ENDIF
        ENDDO
        K = K + NSL
        K_L = K_L + NSL_L
      ENDDO
C
C RBM specifique
C
      IF(NSKYRBM_L>0)THEN
       DO P = 1, NSPMD
        IDEBRBK(P) = 0
       ENDDO
       K = 0
       NSL_L = 0
       DO N = 1, NIBVEL
        NSL=IBVEL(3,N)
        MSR=IBVEL(4,N)
        PMAIN = ABS(DD_RBM2(3,N))
        IF(NLOCAL(MSR,PROC)==1) THEN    
          DO KK = 1, NSL
            NN = LBVEL(K+KK)
            IF(NLOCAL(NN,PROC)==1)THEN      
              NSL_L = NSL_L + 1
              main = 0
              DO P = 1, PROC-1
                IF(NLOCAL(NN,P)==1)THEN         
                  GOTO 3000
                ENDIF
              ENDDO
              main = 1
 3000         IF(MAIN==1) THEN
C numerotation fonction du pmain des rby precedents
                IADRBM(NSL_L) = KK+IDEBRBK(PMAIN)
              ELSE
                IADRBM(NSL_L) = 0
              ENDIF
            ENDIF
          ENDDO
        ENDIF
        K = K + NSL
        IDEBRBK(PMAIN) = IDEBRBK(PMAIN) + NSL
       ENDDO
      ENDIF
C
C RBE3 specifique----plus tard---
C
      IF(NSKYRBE3_L>0)THEN
      ENDIF
CC
C
C Itet=2 of S10 specifique
C
C-----------------------------------------------
      IF(NS10E>0) THEN
C: N_L :NS10E_L, NSN_L compacted w/o sharering
        N_L = 0   
        NSN_L = 0
        DO N = 1, NS10E
            K = ICNDS10(1,N)
            N1= ICNDS10(2,N)
            N2= ICNDS10(3,N)
            IF(NLOCAL(K,PROC)==1.AND.ITAGND(K)<=NS10E) THEN
              N_L = N_L +1          
              DO P = 1, PROC-1
               IF(NLOCAL(K,P)==1) GO TO 332               
              ENDDO
c             IF (CEPCND(N)==PROC-1) THEN
               NSN_L = NSN_L + 1
               ICNDTMP(1,NSN_L) = N1
               ICNDTMP(2,NSN_L) = N2
               ICNDTMP(3,NSN_L) = N_L
c              END IF
 332         CONTINUE
            END IF
        END DO
        if(n_l/=NS10E_L)print *,'error decomp Itet2of S10 p/on',n_l,NS10E_L
c       print *,'NSN_L,NS10E_L,LCNCND_L,NS10E=',NSN_L,NS10E_L,LCNCND_L,NS10E
C
        IADCND(1:2,1:NS10E_L) = 0
        ADDCNCND_L(1) = 1
        CC_L = 0
        DO I = 1, NUMNOD_L
          N = NODGLOB(I)
          N1 = ADDCNCND(N)
          N2 = ADDCNCND(N+1)
          ADDCNCND_L(I+1) = ADDCNCND_L(I) + N2-N1
          DO CC = N1, N2-1
            NUMG = CNCND(CC)
            IF (NUMG==0) CYCLE
            NUML = CELCND(NUMG)
            PROC_L = CEPCND(NUMG)+1
            CC_L = CC_L + 1
            PROCNCND(CC_L) = PROC_L

C Remplissage IADCND si elt interne

            IF (PROC==PROC_L) THEN
              DO K = 1, 2
                IF(ICNDTMP(K,NUML)==N) THEN
                  N_L = ICNDTMP(3,NUML)
                  IADCND(K,N_L) = CC_L
                  ICNDTMP(K,NUML) = -N
                  GO TO 223
                ENDIF
              END DO
 223          CONTINUE
            END IF
          END DO
        END DO
      ENDIF
C
C Interface 18
C
      IF(NBI18_L>0)THEN
        NN = 0
        DO N=1,NINTER
          ITY = IPARI(7,N)
          INACTI = IPARI(22,N)
          IF((ITY==7.OR.ITY==22).AND.INACTI==7)THEN  ! interface 18   ! ON PASSE PAR PAR LA (INTER 18 ou 22)
            NRTS  = IPARI(3,N)
            NRTM  = IPARI(4,N)
            DO K=1,NRTM
Cel TAGE flag servant pour inacti
              N1 = INTBUF_TAB(N)%IRECTM(4*(K-1)+1)
              N2 = INTBUF_TAB(N)%IRECTM(4*(K-1)+2)
              N3 = INTBUF_TAB(N)%IRECTM(4*(K-1)+3)
              N4 = INTBUF_TAB(N)%IRECTM(4*(K-1)+4)
              IF(NLOCAL(N1,PROC)==1.AND.
     .             NLOCAL(N2,PROC)==1.AND.
     .             NLOCAL(N3,PROC)==1.AND.
     .             NLOCAL(N4,PROC)==1) THEN
                DO P = 1, PROC-1
                  IF(NLOCAL(N1,P)==1.AND.
     .               NLOCAL(N2,P)==1.AND.
     .               NLOCAL(N3,P)==1.AND.
     .               NLOCAL(N4,P)==1) THEN
                    GOTO 1300
                  END IF
                END DO
                NN = NN + 1
                IADI18(NN) = K
 1300           CONTINUE
              END IF
            END DO
          END IF
        END DO
      END IF
C-----------------------------------------------
C Ecriture tableaux P/ON propre au SPMD
C-----------------------------------------------
C
C elements
      CALL WRITE_I_C(ADDCNE_L,NUMNOD_L+1)
      LEN_IA = LEN_IA + NUMNOD_L+1
      CALL WRITE_I_C(PROCNE,LCNE_L)
      LEN_IA = LEN_IA + LCNE_L
C int 2
      IF(I2NSNT>0) THEN
        CALL WRITE_I_C(ADDCNI2_L,NUMNOD_L+1)
      LEN_IA = LEN_IA + NUMNOD_L+1
      ENDIF
      CALL WRITE_I_C(PROCNI2,LCNI2_L)
      LEN_IA = LEN_IA + LCNI2_L
C itet=2 of s10
      IF(NS10E_L>0) THEN
       CALL WRITE_I_C(ADDCNCND_L,NUMNOD_L+1)
       LEN_IA = LEN_IA + NUMNOD_L+1
      ENDIF
      CALL WRITE_I_C(PROCNCND,LCNCND_L)
      LEN_IA = LEN_IA + LCNCND_L
C adresses elements
      CALL WRITE_I_C(IADS,8*NUMELS_L)
      LEN_IA = LEN_IA + 8*NUMELS_L
      CALL WRITE_I_C(IADS10,6*NUMELS10_L)
      LEN_IA = LEN_IA + 6*NUMELS10_L
      CALL WRITE_I_C(IADS20,12*NUMELS20_L)
      LEN_IA = LEN_IA +12*NUMELS20_L
      CALL WRITE_I_C(IADS16,8*NUMELS16_L)
      LEN_IA = LEN_IA + 8*NUMELS16_L
      CALL WRITE_I_C(IADQ,4*NUMELQ_L)
      LEN_IA = LEN_IA + 4*NUMELQ_L
      CALL WRITE_I_C(IADC,4*NUMELC_L)
      LEN_IA = LEN_IA + 4*NUMELC_L
      CALL WRITE_I_C(IADT,2*NUMELT_L)
      LEN_IA = LEN_IA + 2*NUMELT_L
      CALL WRITE_I_C(IADP,2*NUMELP_L)
      LEN_IA = LEN_IA + 2*NUMELP_L
      CALL WRITE_I_C(IADR,3*NUMELR_L)
      LEN_IA = LEN_IA + 3*NUMELR_L
      CALL WRITE_I_C(IADTG,3*NUMELTG_L)
      LEN_IA = LEN_IA + 3*NUMELTG_L
      CALL WRITE_I_C(IADTG1,3*NUMELTG6_L)
      LEN_IA = LEN_IA + 3*NUMELTG6_L
      CALL WRITE_I_C(IADUR,4*NUMELUR_L)
      LEN_IA = LEN_IA + 4*NUMELUR_L
      CALL WRITE_I_C(IADMV,4*NNMV_L)
      LEN_IA = LEN_IA + 4*NNMV_L
      CALL WRITE_I_C(IADIB,4*NCONLD_L)
      LEN_IA = LEN_IA + 4*NCONLD_L
      CALL WRITE_I_C(IADIBCV,4*NCONV_L)
      LEN_IA = LEN_IA + 4*NCONV_L
      CALL WRITE_I_C(IADIBCR,4*NRADIA_L)
      LEN_IA = LEN_IA + 4*NRADIA_L 
      CALL WRITE_I_C(IADIBFX,4*NFXFLUX_L)
      LEN_IA = LEN_IA + 4*NFXFLUX_L 
      CALL WRITE_I_C(IADLOAD,LLLOADP_L)
      LEN_IA = LEN_IA + LLLOADP_L   
C adresses RW
      CALL WRITE_I_C(IADWAL,NSKYRW_L)
      LEN_IA = LEN_IA + NSKYRW_L
C adresses RB Kin
      CALL WRITE_I_C(IADRBK,NSKYRBK_L)
      LEN_IA = LEN_IA + NSKYRBK_L
C adresses int 2
      CALL WRITE_I_C(IADI2,NISKYI2_L)
      LEN_IA = LEN_IA + NISKYI2_L
C adresses itet2 S10
      CALL WRITE_I_C(IADCND,2*NS10E_L)
      LEN_IA = LEN_IA + 2*NS10E_L
C adresses MV partie force normale
      CALL WRITE_I_C(IADMV2,NNMV_L)
      LEN_IA = LEN_IA + NNMV_L
C adresses MV partie fuite et mv communicant
      CALL WRITE_I_C(IADMV3,NNMVC_L)
      LEN_IA = LEN_IA + NNMVC_L
C adresses RL starter
      CALL WRITE_I_C(IADLL,NSKYLL_L)
      LEN_IA = LEN_IA + NSKYLL_L
C adresses RBM starter
      CALL WRITE_I_C(IADRBM,NSKYRBM_L)
      LEN_IA = LEN_IA + NSKYRBM_L
C adresses RBE3 starter
c      CALL WRITE_I_C(IADRBE3,NSKYRBE3_L)
c      LEN_IA = LEN_IA + NSKYRBE3_L
      CALL WRITE_I_C(IADI18,NSKYI18_L)
      LEN_IA = LEN_IA + NSKYI18_L
C adresses rigid material Kin
      CALL WRITE_I_C(IADRBMK,NSKYRBMK_L)
      LEN_IA = LEN_IA + NSKYRBMK_L
C
C elements
      IF(IPLYXFEM > 0 ) THEN
        CALL WRITE_I_C(ADDCNEPXFEM_L,NUMNODPXFEM_L+1)
        LEN_IA = LEN_IA + NUMNODPXFEM_L+1
        CALL WRITE_I_C(PROCNE_PXFEM,LCNEPXFEM_L)
        LEN_IA = LEN_IA + LCNEPXFEM_L
        CALL WRITE_I_C(IADC_PXFEM,4*NUMELCPXFEM_L)
        LEN_IA = LEN_IA + 4*NUMELCPXFEM_L
      ENDIF
C
C crack xfem for layered shell
C
      IF (ICRACK3D > 0) THEN
        CALL WRITE_I_C(ADDCNECRKXFEM_L,NUMNODCRKXFE_L+1)
        LEN_IA = LEN_IA + NUMNODCRKXFE_L+1
        CALL WRITE_I_C(CNE_CRKXFEM_L,LCNECRKXFEM_L)
        LEN_IA = LEN_IA + LCNECRKXFEM_L 
        CALL WRITE_I_C(PROCNE_CRKXFEM,LCNECRKXFEM_L)
        LEN_IA = LEN_IA + LCNECRKXFEM_L 
        CALL WRITE_I_C(IADC_CRKXFEM,4*NUMELCCRKXFE_L)
        LEN_IA = LEN_IA + 4*NUMELCCRKXFE_L     
        CALL WRITE_I_C(IADTG_CRKXFEM,3*NUMELTGCRKXFE_L)
        LEN_IA = LEN_IA + 3*NUMELTGCRKXFE_L
        CALL WRITE_I_C(CRKNODIAD_L,LCNECRKXFEM_L)
        LEN_IA = LEN_IA + LCNECRKXFEM_L
      ENDIF

      ! -----------------------
      ! EBCS option : adress for parith/on 
      IF(LOCAL_NEBCS>0) THEN
        DO I=1,LOCAL_NEBCS
            CALL WRITE_I_C(EBCS_PARITHON_L(I)%ELEM_ADRESS,4*EBCS_TAB_LOC_2%tab(I)%poly%nb_elem)
            LEN_IA = LEN_IA + 4*EBCS_TAB_LOC_2%tab(I)%poly%nb_elem
        ENDDO
      ENDIF
      ! -----------------------
c
      DEALLOCATE (SOLTAG)
      DEALLOCATE (SOL10TAG)
      DEALLOCATE (SOL20TAG)
      DEALLOCATE (SOL16TAG)
      DEALLOCATE (QUADTAG)
      DEALLOCATE (SHTAG)
      DEALLOCATE (TTAG)
      DEALLOCATE (PTAG)
      DEALLOCATE (RTAG)
      DEALLOCATE (TGTAG)
      DEALLOCATE (TG6TAG)
      DEALLOCATE (URTAG)
      DEALLOCATE (IBTAG)
      DEALLOCATE (IBCVTAG)
      DEALLOCATE (IBCRTAG)
      DEALLOCATE (IBFXTAG)
      DEALLOCATE (ILTAG)
      DEALLOCATE (TAGIG3D)
! -----------------------------
!     deallocate 1d arrays
      DEALLOCATE( ITAGC,ITAGTG )
      DEALLOCATE( ADDCNE_L,ADDCNI2_L,ADDCNCND_L )
!     deallocate IAD arrays
      DEALLOCATE( IADS,IADS10 )
      DEALLOCATE( IADS16,IADS20 )
      DEALLOCATE( IADQ,IADC )
      DEALLOCATE( IADT,IADP ) 
      DEALLOCATE( IADR,IADTG )
      DEALLOCATE( IADUR,IADIB )
      DEALLOCATE( IADTG1,IADIG3D )
! -----------------------------
      ! EBCS option : deallocation
      DEALLOCATE( EBCS_TAG )
      IF(LOCAL_NEBCS>0) THEN
        DO I=1,LOCAL_NEBCS
            DEALLOCATE( EBCS_PARITHON_L(I)%ELEM_ADRESS )
        ENDDO
      ENDIF
      DEALLOCATE(EBCS_PARITHON_L)
! -----------------------------
      RETURN
      END
