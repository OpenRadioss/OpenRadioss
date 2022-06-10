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
Chd|  GENANI1                       source/output/anim/genani1.F  
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANIOFF0                       source/output/anim/anioff0.F  
Chd|        ANIOFFC                       source/output/anim/anioffc.F  
Chd|        ANIOFFF                       source/output/anim/aniofff.F  
Chd|        ANIOFFS                       source/output/anim/anioffs.F  
Chd|        ANISKEW                       source/output/anim/aniskew.F  
Chd|        ANISKEWF                      source/output/anim/aniskewf.F 
Chd|        ANI_TXT                       source/output/anim/ani_txt.F  
Chd|        ANI_TXT50                     source/output/anim/ani_txt.F  
Chd|        CLOSE_C                       source/output/tools/write_routines.c
Chd|        CUR_FIL_C                     source/output/tools/write_routines.c
Chd|        DELNUMB0                      source/output/anim/delnumb0.F 
Chd|        DELNUMBC                      source/output/anim/delnumbc.F 
Chd|        DELNUMBF                      source/output/anim/delnumbf.F 
Chd|        DELNUMBS                      source/output/anim/delnumbs.F 
Chd|        DELSUB                        source/output/anim/delsub.F   
Chd|        DFUNC0                        source/output/anim/dfunc0.F   
Chd|        DFUNCC                        source/output/anim/dfuncc.F   
Chd|        DFUNCF                        source/output/anim/dfuncf.F   
Chd|        DFUNCS                        source/output/anim/dfuncs.F   
Chd|        DMASANI0                      source/output/anim/dmasani0.F 
Chd|        DMASANIC                      source/output/anim/dmasanic.F 
Chd|        DMASANIF                      source/output/anim/dmasanif.F 
Chd|        DMASANIS                      source/output/anim/dmasanis.F 
Chd|        DONERBY                       source/output/anim/donerby.F  
Chd|        DONERWL                       source/output/anim/donerwl.F  
Chd|        DONESEC                       source/output/anim/donesec.F  
Chd|        DONESRG                       source/output/anim/donesrg.F  
Chd|        DPARRBY                       source/output/anim/dparrby.F  
Chd|        DPARRWS                       source/output/anim/dparrws.F  
Chd|        DPARSRG                       source/output/anim/dparsrg.F  
Chd|        DRBYCNT                       source/output/anim/drbycnt.F  
Chd|        DSECCNT                       source/output/anim/dseccnt.F  
Chd|        DSECNOR                       source/output/anim/dsecnor.F  
Chd|        DSPHCNT                       source/output/anim/dsphcnt.F  
Chd|        DSPHNOR                       source/output/anim/dsphnor.F  
Chd|        DSRGCNT                       source/output/anim/dsrgcnt.F  
Chd|        DSRGNOR                       source/output/anim/dsrgnor.F  
Chd|        DXYZSECT                      source/output/anim/dxyzsect.F 
Chd|        DXYZSPH                       source/output/anim/dxyzsph.F  
Chd|        DXYZSRG                       source/output/anim/dxyzsrg.F  
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        PARSOR0                       source/output/anim/parsor0.F  
Chd|        PARSORC                       source/output/anim/parsorc.F  
Chd|        PARSORF                       source/output/anim/parsorf.F  
Chd|        PARSORS                       source/output/anim/parsors.F  
Chd|        TENSOR0                       source/output/anim/tensor0.F  
Chd|        TENSORC                       source/output/anim/tensorc.F  
Chd|        TENSORS                       source/output/anim/tensors.F  
Chd|        VELVEC                        source/output/anim/velvec.F   
Chd|        WRITE_C_C                     source/output/tools/write_routines.c
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        WRITE_R_C                     source/output/tools/write_routines.c
Chd|        WRITE_S_C                     source/output/tools/write_routines.c
Chd|        XYZ16                         source/output/anim/genani1.F  
Chd|        XYZNOR                        source/output/anim/xyznor.F   
Chd|        XYZNOR16                      source/output/anim/genani1.F  
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        FVBAG_MOD                     share/modules1/fvbag_mod.F    
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE GENANI1(X      , BUFEL  , IXS   , IXQ    , IXC     ,
     2                   IXT    , IXP    , IXR   , IXTG   , SWAFT   ,
     3                   IPARG  , PM     , GEO   , SKEW   , ITAB    ,
     4                   LPBY   , NPBY   , NSTRF , RWBUF  , NPRW    ,
     5                   IXUR   , IPART  , IPARTS, IPARTQ , IPARTC  ,
     6                   IPARTT , IPARTP , IPARTR, IPARTUR, IPARTTG ,
     7                   RBY   , SWA4   , 
     8                   IGRSURF, BUFSF  , IPARTX, KXSP   , IXSP    ,
     9                   IPARTSP, SPBUF  , IXS10 , IXS20  , IXS16   ,
     A                   IPM    , IGEO   , SMATER, SEL2FA , SNFACPTX,
     B                   SIXEDGE, SOFFX1 , SNUMX1, SXNORM , SINVERT ,
     C                   SFUNC1 , SIAD   , NMANIM, D      , SMAS    ,
     D                   MS     , FXANI  , MBUFEL, MDEPL  , NLEVEL  ,
     E                   ELSUB  , DSANIM , NELEM , CEP    , CEPSP   ,
     F                   NOM_OPT,PTR_NOPT_RWALL,PTR_NOPT_SECT,
     G                   ELBUF_TAB,SPH2SOL,SUBSET)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FVBAG_MOD
      USE MESSAGE_MOD
      USE ELBUFDEF_MOD
      USE GROUPDEF_MOD
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com05_c.inc"
#include      "com08_c.inc"
#include      "com09_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "scr15_c.inc"
#include      "scr16_c.inc"
#include      "scr23_c.inc"
#include      "rnur_c.inc"
#include      "scr12_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*), IXQ(NIXQ,*), IXC(NIXC,*), IXT(NIXT,*),
     .        IXP(NIXP,*), IXR(NIXR,*), IXTG(NIXTG,*), SWAFT,
     .        IPARG(NPARG,*), ITAB(*), LPBY(*), NPBY(NNPBY,*),
     .        NSTRF(*), NPRW(*), IXUR(NIXUR,*), IPART(LIPART1,*),
     .        IPARTS(*), IPARTQ(*), IPARTC(*), IPARTT(*),
     .        IPARTP(*), IPARTR(*), IPARTUR(*), IPARTTG(*),SWA4, 
     .        IPARTX(*), KXSP(NISP,*),IXSP(KVOISPH,*),
     .        IPARTSP(*), IXS10(6,*), IXS20(12,*), IXS16(8,*),
     .        IPM(NPROPMI,*), IGEO(NPROPGI,*), SMATER, SEL2FA,
     .        SNFACPTX, SIXEDGE, SOFFX1, SNUMX1, SXNORM, SINVERT,
     .        SFUNC1, SIAD, NMANIM, SMAS, FXANI(2,*),
     .        NLEVEL, ELSUB(NLEVEL,*),DSANIM, NELEM, CEP(*), CEPSP(*),
     .        SPH2SOL(*)
      INTEGER NOM_OPT(LNOPT1,*),PTR_NOPT_RWALL,PTR_NOPT_SECT
C
      my_real
     .        X(3,*), BUFEL(*), PM(NPROPM,*), GEO(NPROPG,*),
     .        SKEW(LSKEW,*), RWBUF(NRWLP,*), RBY(NRBY,*), BUFSF(*),
     .        SPBUF(*), D(3,*), MS(*), MBUFEL(LBUFEL,*),
     .        MDEPL(3*NUMNOD,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
      TYPE (SUBSET_) , DIMENSION(NSUBS) :: SUBSET
      TYPE (SURF_)   , DIMENSION(NSURF) :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LTITL, ISPH3D, I161, I16A, I16B, I16C, I16D, I16E, I16F,
     .        I16G, I16H, I16I, FILEN, CTEXT(200), NSKEWA, NB1D, I,
     .        MATER(SMATER), NBPART, NBF, NBF_L, EL2FA(SEL2FA), NODCUT,
     .        NELCUT, NCUTS, NUMSPH_T, NESCT, NERWL, NNWL, NESBW2,
     .        NESRG, NNSRG, NSURG, NESMD, NNSMD, NSMAD, NESPH, NNSPH,
     .        NNSPHG, NUMELS_T, NUMELS16_T, NUMELT_T, NUMELR_T,
     .        NUMELP_T, MAGIC, IFLAG1D, BUFL, SNNSPHG, SZ16,
     .        BUF, NESCT1, NERWL1, ISECT, IRWL, NESRG1, ISRG, NESMD1,
     .        J, IB, NDMA2, IFUNC, SZNNSPH, SHFTSPH, SHFT16, INSPH, NNN,
     .        NERBY, NB1D_T, IPRT, NERBT(NRBODY), NERBY1, IRBY, LRBUF,
     .        NFACPTX(3,SNFACPTX), IXEDGE(SIXEDGE),
     .        IAD(SIAD), IOFFX1(SOFFX1), INUMX1(SNUMX1), MXSUBS, N1, N2,
     .        N3, K, M3, M4, N0, NESPHG, ISRF, INVERT(SINVERT), M01,
     .        M1, M2, NNNSRG, M, N, LID, NMFUNC(9)
      INTEGER FVOFF(2,NFVBAG), INOD(4), INORM(3), NFVTR, NFVNOD,
     .        NFVPART, NFVSUBS, IDMAX, KK, NN, FVIAD, JJ, OFFPART,
     .        ELOFF, IDCMAX, NND, NBID1, NBID2, NBID3, NFVNODT, IDP,
     .        NBPART2D, IDPART2DMAX, II
      INTEGER, DIMENSION(:), ALLOCATABLE :: OFFTR, ITAGT, FVEL2FA,
     .                                      FVINUM, FVPBUF
C
      my_real
     .        CDG(3), WAFT(SWAFT), XMIN, YMIN, ZMIN, XMAX, YMAX, ZMAX,
     .        XNORM(SXNORM), XFUNC1(SFUNC1), MAS(SMAS), RID
      REAL    R4, WA4(SWA4)
      CHARACTER  FILNAM*103, CHANIM*3, CHANIM1*4, TITL*nchartitle, CTMOD*100
      CHARACTER*80 STR
Cf51e11 +2
      INTEGER OFF
      my_real
     .        FUNC(MAX(NELEM,NUMSPH))
      CHARACTER*33 CTITR(MAX(1,NLEVEL))
      CHARACTER*80 STRZZ
C
        LTITL = 40
Cvf      ENDIF
      IF(ANIM_VERS.LT.44)THEN
       ISPH3D=1
      ELSE
       ISPH3D=0
      ENDIF
      I161=1
      I16A=I161+LNOPT1*NRBODY
      I16B=I16A+LNOPT1*NACCELM
      I16C=I16B+LNOPT1*NVOLU
      I16D=I16C+LNOPT1*(NINTER+NINTSUB)
      I16E=I16D+LNOPT1*NRWALL
      I16F=I16E !obsolete option removed
      I16G=I16F+LNOPT1*NJOINT
      I16H=I16G+LNOPT1*NSECT
      I16I=I16H+LNOPT1*NLINK
      MAS(1:SMAS) = ZERO
C-----------------------------------------------
C   OPEN FILE
C-----------------------------------------------
      IF(ANIM_VERS.GE.50)THEN
        IF(IANIM.GE.10000)IANIM=1
        WRITE(CHANIM1,'(I4.4)')IANIM
        FILNAM=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//
     .         ROOTNAM(1:ROOTLEN)//'_'//CHANIM1//'.ani'
        FILEN = OUTFILE_NAME_LEN + ROOTLEN + 9
      ELSE
        IF(IANIM.GE.1000)IANIM=1
        WRITE(CHANIM,'(I3.3)')IANIM
        FILNAM=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//
     .         ROOTNAM(1:ROOTLEN)//'A'//CHANIM
        FILEN = OUTFILE_NAME_LEN + ROOTLEN + 4
      ENDIF
C
      DO I=1,FILEN
        CTEXT(I)=ICHAR(FILNAM(I:I))
      ENDDO
      CALL CUR_FIL_C(0)
      CALL OPEN_C(CTEXT,FILEN,0)
C-----------------------------------------------
C      ANIM MULTI-LEVEL DOMDEC
C-----------------------------------------------
      IF (IMACH.EQ.3) THEN
         WRITE(CTITR(1),'(A25)') 'SPMD Domain Decomposition'
      ELSEIF (DSANIM.EQ.1) THEN
         WRITE(CTITR(1),'(A30,I3)') 'Impl. graph - Dom. Dec. Level ',1
      ENDIF
      DO I=2,NLEVEL
         WRITE(CTITR(I),'(A30,I3)') 'Impl. graph - Dom. Dec. Level ',I
      ENDDO
C-----------------------------------------------
C      SKEW + NB1D
C      NB1D includes all 1D elements except those from X-ELEMENTS.
C-----------------------------------------------
      NSKEWA=NUMELP + NUMELT + NUMSKW
      NB1D  =NUMELP + NUMELT + NUMELR
      DO I=1,NUMELR
        IF(NINT(GEO(12,IXR(1,I))).EQ.4 .OR.
     .     NINT(GEO(12,IXR(1,I))).EQ.13.OR.
     .     NINT(GEO(12,IXR(1,I))).EQ.45.OR.     
     .       (NINT(GEO(12,IXR(1,I))).GE.29.AND.
     .        NINT(GEO(12,IXR(1,I))).LE.33))THEN
          NSKEWA=NSKEWA+1
        ELSEIF(NINT(GEO(12,IXR(1,I))).EQ.12)THEN
          NSKEWA=NSKEWA+2
          NB1D =NB1D+1
        ENDIF
      ENDDO
C=======================================================================
C
C      COQUE 3N 4N
C
C=======================================================================
      DO I=1,NPART
        MATER(I)=0
      ENDDO
      DO I=1,NUMELQ
        MATER(IPARTQ(I))=1
      ENDDO
      DO I=1,NUMELC
        MATER(IPARTC(I))=1
      ENDDO
      DO I=1,NUMELTG
        MATER(IPARTTG(I))=1
      ENDDO
      DO I=1,NUMELUR
        MATER(IPARTUR(I))=1
      ENDDO
C
      NBPART = 0
      DO I=1,NPART
        NBPART = NBPART + MATER(I)
      ENDDO
C
      NBF =  NUMELQ + NUMELC + NUMELTG + NUMELUR
      NBF_L = NBF
C
      DO I=1,NUMELQ + NUMELC + NUMELTG + NUMELUR + 1
        EL2FA(I)=0
      ENDDO
C-----------------------------------------------
C   COUPES DANS LES SOLIDES
C-----------------------------------------------
      NODCUT=0
      NELCUT=0
      NCUTS=0
C-----------------------------------------------
      NUMSPH_T = NUMSPH
      NESCT = 0
      NERWL = 0
      NNWL  = 0
      NESBW2= 0
      IF(NSECT+NRWALL.GT.0) THEN
         CALL DSECCNT(NESCT,NERWL,NESBW2,NSTRF,
     1                RWBUF ,NPRW,NNWL,IXS)
      END IF

      NESRG=0
      NNSRG=0
      NSURG=0
      IF (NSURF.GT.0)
     . CALL DSRGCNT(IGRSURF, NSURG,NESRG,NNSRG,NESBW2)
      NESMD=0
      NNSMD=0
      NSMAD=0
      NESPH=0
      NNSPH=0
      NNSPHG = 0
      IF (ISPH3D.EQ.1.AND.NUMSPH_T+MAXPJET.GT.0)
     .  CALL DSPHCNT(NESPH,NNSPH,NESPHG,NNSPHG)
C-----------------------------------------------
C   MAILLAGE VOLUMES FINIS POUR FVMBAG
C-----------------------------------------------
      NFVNOD=0
      NFVTR=0
      NFVPART=0
      NFVSUBS=0
      IF (NFVBAG.GT.0) THEN
         IDMAX=0
         DO I=1,NUMNOD
            IDMAX=MAX(IDMAX,ITAB(I))
         ENDDO
      ENDIF
C
      IF (IFVANI.EQ.1) THEN
         DO I=1,NFVBAG
            NFVTR=NFVTR+FVDATA(I)%NNTR
            FVOFF(1,I)=NUMNOD+NODCUT+NSECT+NRWALL+NNWL
     .                +NNSRG+NNSMD+NNSPH+2*NUMELS16+NFVNOD
            FVOFF(2,I)=IDMAX+NFVNOD
            NFVNOD=NFVNOD+FVDATA(I)%NNS_ANIM
            NFVPART=NFVPART+FVDATA(I)%NPOLH_ANIM
            NFVSUBS=NFVSUBS+1
         ENDDO
      ENDIF
C
      IF (NFVTR.GT.0)
     .   ALLOCATE(FVEL2FA(NFVTR), FVINUM(NFVTR))
C
C-----------------------------------------------
C   WRITE CONTROL
C-----------------------------------------------
      NUMELS_T = NUMELS
      NUMELS16_T = NUMELS16
      NUMELT_T = NUMELT
      NUMELR_T = NUMELR
      NUMELP_T = NUMELP
C
      MAGIC = 21548
      CALL WRITE_I_C(MAGIC,1)
      R4=IANIM
      CALL WRITE_R_C(R4,1)
      CALL ANI_TXT('Mode number=',12)
      CALL ANI_TXT('Local mode',10)
      CALL ANI_TXT('Radioss Run=',12)
C
      CALL WRITE_I_C(ANIM_M,1)
      CALL WRITE_I_C(1,1)
C
      IF(NUMELS_T+ISPH3D*(NUMSPH_T+MAXPJET).EQ.0) THEN
        CALL WRITE_I_C(0,1)
      ELSE
        CALL WRITE_I_C(1,1)
      ENDIF
      IFLAG1D = NUMELT_T+NUMELP_T+NUMELR_T+NANIM1D+NRBODY
      IF (IFLAG1D.NE.0) IFLAG1D = 1
      CALL WRITE_I_C(IFLAG1D,1)
C
C HIERARCHIE
      CALL WRITE_I_C(1,1)
C TH
      CALL WRITE_I_C(0,1)
C REP. SHELL
      IF(ISHFRAM.EQ.1)THEN
        CALL WRITE_I_C(0,1)
      ELSE
        CALL WRITE_I_C(1,1)
      ENDIF
C
      IF(ISPH3D.EQ.0.AND.
     .   (NUMSPH_T+MAXPJET.NE.0))THEN
       CALL WRITE_I_C(1,1)
      ELSE
       CALL WRITE_I_C(0,1)
      ENDIF
C
        CALL WRITE_I_C(0,1)
      CALL WRITE_I_C(0,1)
C
      IF (NFVNOD.GT.0) THEN
         NFVNODT=NFVNOD+3
      ELSE
         NFVNODT=0
      ENDIF
C
      CALL WRITE_I_C(NUMNOD+NODCUT+NSECT+NRWALL+NNWL
     .              +NNSRG+NNSMD+NNSPH+2*NUMELS16+NFVNODT,1)
      CALL WRITE_I_C(NBF+NELCUT+NESBW2+NFVTR,1)
      NBPART2D=NBPART+NCUTS+NSECT+NRWALL+NSURG+NSMAD
      CALL WRITE_I_C(NBPART+NCUTS
     .     +NSECT+NRWALL+NSURG+NSMAD+NFVPART,1)
      CALL WRITE_I_C(NN_ANI,1)
      IF(NBF+NELCUT+NESBW2+NFVTR.EQ.0)THEN
        CALL WRITE_I_C(0,1)
      ELSE
        IF (DSANIM.EQ.1) THEN
           NCE_ANI=NCE_ANI+NLEVEL
        ELSEIF (DECANI.EQ.1) THEN
           NCE_ANI=NCE_ANI+1
        ENDIF
        CALL WRITE_I_C(NCE_ANI,1)
      ENDIF
      CALL WRITE_I_C(NV_ANI,1)
      IF(NBF+NELCUT+NESBW2+NFVTR.EQ.0)THEN
        CALL WRITE_I_C(0,1)
      ELSE
        CALL WRITE_I_C(NCT_ANI,1)
      ENDIF
      CALL WRITE_I_C(NSKEWA,1)
C-----------------------------------------------
C      SKEW
C-----------------------------------------------
      BUFL=1
      CALL ANISKEW(ELBUF_TAB,SKEW ,IPARG ,X    ,IXT,
     2             IXP      ,IXR  ,GEO   ,BUFL )
C-----------------------------------------------
C   NODE X Y Z
C-----------------------------------------------
      XMIN = EP30
      YMIN = EP30
      ZMIN = EP30
      XMAX = -EP30
      YMAX = -EP30
      ZMAX = -EP30
C
      DO N=1,NUMNOD
         XMIN = MIN(XMIN,(X(1,N)-D(1,N)))
         YMIN = MIN(YMIN,(X(2,N)-D(2,N)))
         ZMIN = MIN(ZMIN,(X(3,N)-D(3,N)))
         XMAX = MAX(XMAX,(X(1,N)-D(1,N)))
         YMAX = MAX(YMAX,(X(2,N)-D(2,N)))
         ZMAX = MAX(ZMAX,(X(3,N)-D(3,N)))
      END DO
C
      CDG(1) = HALF * (XMAX + XMIN)
      CDG(2) = HALF * (YMAX + YMIN)
      CDG(3) = HALF * (ZMAX + ZMIN)
C
      DO I=1,NUMNOD
         R4 = X(1,I)
         CALL WRITE_R_C(R4,1)
         R4 = X(2,I)
         CALL WRITE_R_C(R4,1)
         R4 = X(3,I)
         CALL WRITE_R_C(R4,1)
      END DO
C
      IF(NSECT+NRWALL.GT.0) CALL DXYZSECT(
     2                  NSTRF,RWBUF,NPRW  ,X,XMIN,
     3                  YMIN,ZMIN,XMAX,YMAX,ZMAX,
     4                  ITAB)
C
      IF (NSURG.GT.0) CALL DXYZSRG(NESRG,IGRSURF,BUFSF)
C
      SNNSPHG = 0
      IF (ISPH3D*(NUMSPH_T+MAXPJET).GT.0)
     .   CALL DXYZSPH(NESPH,KXSP,X,SPBUF,SNNSPHG,NNSPH)
C
      SZ16 = NUMELS16
      IF (SZ16.GT.0)
     2   CALL XYZ16(IXS,IXS10,IXS20,IXS16,X)
C
      IF (NFVNOD.GT.0) THEN
         DO I=1,NFVBAG
            DO J=1,FVDATA(I)%NNS_ANIM
               R4=FVDATA(I)%NOD_ANIM(1,J)
               CALL WRITE_R_C(R4,1)
               R4=FVDATA(I)%NOD_ANIM(2,J)
               CALL WRITE_R_C(R4,1)
               R4=FVDATA(I)%NOD_ANIM(3,J)
               CALL WRITE_R_C(R4,1)
            ENDDO
         ENDDO
C
         R4=EM10
         CALL WRITE_R_C(R4,1)
         R4=ZERO
         CALL WRITE_R_C(R4,1)
         R4=ZERO
         CALL WRITE_R_C(R4,1)
         R4=ZERO
         CALL WRITE_R_C(R4,1)
         R4=EM10
         CALL WRITE_R_C(R4,1)
         R4=ZERO
         CALL WRITE_R_C(R4,1)
         R4=ZERO
         CALL WRITE_R_C(R4,1)
         R4=ZERO
         CALL WRITE_R_C(R4,1)
         R4=EM10
         CALL WRITE_R_C(R4,1)
         NBID1=NUMNOD+NODCUT+NSECT+NRWALL+NNWL
     .        +NNSRG+NNSMD+NNSPH+2*NUMELS16+NFVNOD+1
         NBID2=NBID1+1
         NBID3=NBID2+1
C
      ENDIF
C-----------------------------------------------
C   PART SORT
C-----------------------------------------------
      CALL PARSORC(X   ,D, XNORM,IAD  ,CDG   ,
     .            BUFEL,IPARG,IXQ  ,IXC   ,IXTG ,
     .            INVERT,EL2FA,IXUR ,
     .            MATER,IPARTQ,IPARTC,IPARTUR,IPARTTG,
     .            ELBUF_TAB)
C
      IF(NSECT+NRWALL.GT.0) CALL DPARRWS(
     1            NESBW2,NSTRF, IXC  ,
     2            IXTG ,X    ,NODCUT,RWBUF,NPRW,
     3            IXS)
C
      IF (NSURG.GT.0) CALL DPARSRG(NSURG,NNWL,NODCUT)
C
      II=0
      IF (IFVANI.EQ.1) THEN
         ELOFF=0
         DO I=1,NFVBAG
            ALLOCATE(ITAGT(FVDATA(I)%NNTR))
            DO J=1,FVDATA(I)%NNTR
               ITAGT(J)=0
            ENDDO
C
            DO J=1,FVDATA(I)%NPOLH_ANIM
               DO K=FVDATA(I)%IFVPADR_ANIM(J),
     .                   FVDATA(I)%IFVPADR_ANIM(J+1)-1
                  KK=FVDATA(I)%IFVPOLH_ANIM(K)
                  DO N=FVDATA(I)%IFVTADR_ANIM(KK),
     .                      FVDATA(I)%IFVTADR_ANIM(KK+1)-1
                     NN=FVDATA(I)%IFVPOLY_ANIM(N)
                     IF (ITAGT(NN).EQ.1) CYCLE
                     INOD(1)=FVOFF(1,I)+FVDATA(I)%IFVTRI_ANIM(1,NN)-1
                     INOD(2)=FVOFF(1,I)+FVDATA(I)%IFVTRI_ANIM(2,NN)-1
                     INOD(3)=FVOFF(1,I)+FVDATA(I)%IFVTRI_ANIM(3,NN)-1
                     INOD(4)=INOD(3)
                     II=II+1
C Nombre de noeuds distincts de la coque (apres fusion dans FVMESH)
                     NND=1
                     IF (INOD(2).NE.INOD(1)) NND=NND+1
                     IF (INOD(3).NE.INOD(1).AND.
     .                   INOD(3).NE.INOD(2)) NND=NND+1
                     IF (NND.NE.3) THEN
                        INOD(1)=NBID1-1
                        INOD(2)=NBID2-1
                        INOD(3)=NBID3-1
                        INOD(4)=INOD(3)
                     ENDIF
C
                     CALL WRITE_I_C(INOD,4)
                     ITAGT(NN)=1
                     FVEL2FA(ELOFF+NN)=II
                     FVINUM(II)=ELOFF+NN
                  ENDDO
               ENDDO
            ENDDO
            ELOFF=ELOFF+FVDATA(I)%NNTR
            DEALLOCATE(ITAGT)
         ENDDO
      ENDIF
C-----------------------------------------------
C   OFF
C-----------------------------------------------
      CALL ANIOFFC(ELBUF_TAB,IPARG,WAFT ,EL2FA,NBF  )
C
      DO J=1,NESBW2+NELCUT
        CALL WRITE_C_C(1,1)
      ENDDO
C
      IF (IFVANI.EQ.1) THEN
         ALLOCATE(OFFTR(NFVTR))
         DO I=1,NFVTR
            OFFTR(I)=0
         ENDDO
         ELOFF=0
         DO I=1,NFVBAG
            DO J=1,FVDATA(I)%NPOLH
               DO K=FVDATA(I)%IFVPADR(J),FVDATA(I)%IFVPADR(J+1)-1
                  KK=FVDATA(I)%IFVPOLH(K)
                  DO N=FVDATA(I)%IFVTADR(KK),
     .                      FVDATA(I)%IFVTADR(KK+1)-1
                     NN=FVDATA(I)%IFVPOLY(N)
                     IF (NN.GT.0) THEN
                        N1=FVDATA(I)%IFVTRI_ANIM(1,NN)
                        N2=FVDATA(I)%IFVTRI_ANIM(2,NN)
                        N3=FVDATA(I)%IFVTRI_ANIM(3,NN)
                        NND=1
                        IF (N2.NE.N1) NND=NND+1
                        IF (N3.NE.N2.AND.N3.NE.N1) NND=NND+1
C
                        NN=FVEL2FA(ELOFF+NN)
                        IF (NND.EQ.3) OFFTR(NN)=1
                     ENDIF
                  ENDDO
               ENDDO
            ENDDO
            ELOFF=ELOFF+FVDATA(I)%NNTR
         ENDDO
C
         CALL WRITE_C_C(OFFTR,NFVTR)
         DEALLOCATE(OFFTR)
      ENDIF
C-----------------------------------------------
C   PART ADD
C-----------------------------------------------
      CALL WRITE_I_C(IAD,NBPART)
      NESCT1=0
      DO ISECT=1,NSECT
       CALL DONESEC(ISECT,NESCT1,NSTRF,IXS)
       CALL WRITE_I_C(NELCUT+NBF+NESCT1,1)
      END DO
C
      NERWL1=0
      DO IRWL=1,NRWALL
       CALL DONERWL(IRWL,NERWL1,NPRW)
       CALL WRITE_I_C(NELCUT+NBF+NESCT+NERWL1,1)
      END DO
      NESRG1=0
C
      DO ISRG=1,NSURG
       CALL DONESRG(ISRG,NESRG1)
       CALL WRITE_I_C(NELCUT+NBF+NESCT+NERWL+NESRG1,1)
      END DO
      NESMD1=0
C
      IF (IFVANI.EQ.1) THEN
         FVIAD=NELCUT+NBF+NESCT+NERWL+NESRG+NESMD1
         DO I=1,NFVBAG
            ALLOCATE(ITAGT(FVDATA(I)%NNTR))
            DO J=1,FVDATA(I)%NNTR
               ITAGT(J)=0
            ENDDO
C
            DO J=1,FVDATA(I)%NPOLH_ANIM
               DO K=FVDATA(I)%IFVPADR_ANIM(J),
     .                     FVDATA(I)%IFVPADR_ANIM(J+1)-1
                  KK=FVDATA(I)%IFVPOLH_ANIM(K)
                  DO N=FVDATA(I)%IFVTADR_ANIM(KK),
     .                       FVDATA(I)%IFVTADR_ANIM(KK+1)-1
                     NN=FVDATA(I)%IFVPOLY_ANIM(N)
                     IF (ITAGT(NN).EQ.0) THEN
                        FVIAD=FVIAD+1
                        ITAGT(NN)=1
                     ENDIF
                  ENDDO
               ENDDO
               CALL WRITE_I_C(FVIAD,1)
            ENDDO
C
            DEALLOCATE(ITAGT)
         ENDDO
      ENDIF
C-----------------------------------------------
C   PART HEAD
C-----------------------------------------------
      IDPART2DMAX=0
      DO I=1,NPART
       IF(MATER(I).NE.0)THEN
         IDPART2DMAX=MAX(IDPART2DMAX,IPART(4,I))
         WRITE(STR,'(I8,A1)')IPART(4,I),':'
         DO J=1,9
           CTEXT(J)=ICHAR(STR(J:J))
         ENDDO
         IB = 9
         CALL FRETITL2(TITL,IPART(LIPART1-LTITR+1,I),LTITL)
         DO J=1,LTITL
           IF(TITL(J:J).NE.' ') IB = J+9
           CTEXT(J+9)=ICHAR(TITL(J:J))
         ENDDO
         CTEXT(IB+1)=0
         CALL WRITE_C_C(CTEXT,10+LTITL)
       ENDIF
      ENDDO
C-----------------------------------------------
C     CUTS PART
C-----------------------------------------------
c       print*,'685(664)-870' 
c       return 
      IF (INVSTR.LT.40) THEN
       DO ISECT=1,NSECT
        WRITE(STR,'(I8,A2,A7)') ISECT,': ','Section'
         DO J=1,17
          CTEXT(J)=ICHAR(STR(J:J))
         ENDDO
         IB = 17
         CTEXT(IB+1)=0
         CALL WRITE_C_C(CTEXT,10+LTITL)
       END DO
      ELSE
       DO ISECT=1,NSECT
         WRITE(STR,'(I8,A2)') NOM_OPT(1,PTR_NOPT_SECT+ISECT),': '
         DO J=1,10
          CTEXT(J)=ICHAR(STR(J:J))
         ENDDO
         CALL FRETITL2(TITL,NOM_OPT(LNOPT1-LTITR+1,PTR_NOPT_SECT+ISECT),
     .                 LTITL)
         IB = LTITL+9
         DO J=1,LTITL
           CTEXT(J+10)=ICHAR(TITL(J:J))
         ENDDO
         CTEXT(IB+1)=0
         CALL WRITE_C_C(CTEXT,10+LTITL)
       END DO
      END IF
C
      IF (INVSTR.LT.40) THEN
       DO IRWL=1,NRWALL
         WRITE(STR,'(I8,A2,A10)') IRWL,': ','Rigid Wall'
         DO J=1,20
          CTEXT(J)=ICHAR(STR(J:J))
         ENDDO
         IB = 20
         CTEXT(IB+1)=0
         CALL WRITE_C_C(CTEXT,10+LTITL)
       END DO
      ELSE
       DO IRWL=1,NRWALL
         WRITE(STR,'(I8,A2)') NOM_OPT(1,PTR_NOPT_RWALL+IRWL),': '
         DO J=1,10
          CTEXT(J)=ICHAR(STR(J:J))
         ENDDO
         IB = LTITL+9
         CALL FRETITL2(TITL,NOM_OPT(LNOPT1-LTITR+1,PTR_NOPT_RWALL+IRWL),
     .                 LTITL)
         DO J=1,LTITL
           CTEXT(J+10)=ICHAR(TITL(J:J))
         END DO
         CTEXT(IB+1)=0
         CALL WRITE_C_C(CTEXT,10+LTITL)
       END DO
      ENDIF
C
      ISRG=1
      DO ISRF=1,NSURF
        IF (IGRSURF(ISRF)%TYPE.EQ.101) THEN
C        RADIOSS'S ellipsoid.
         WRITE(STR,'(I8,A1)') ISRG,':'
         DO J=1,9
          CTEXT(J)=ICHAR(STR(J:J))
         ENDDO
         IB=9
         TITL = IGRSURF(ISRF)%TITLE
         DO J=1,LTITL
           IF(TITL(J:J).NE.' ') IB = J+9
           CTEXT(J+9)=ICHAR(TITL(J:J))
         END DO
         CTEXT(IB+1)=0
         CALL WRITE_C_C(CTEXT,10+LTITL)
         ISRG=ISRG+1
        END IF
      END DO
      IDPART2DMAX = IDPART2DMAX + NSECT + NRWALL + NSURG + NSMAD + NCUTS
C-------------------------------------------------------
C   FVMBAG
C-------------------------------------------------------
      IF (IFVANI.EQ.1) THEN
         DO I=1,NFVBAG
            DO J=1,FVDATA(I)%NPOLH_ANIM
               WRITE(STR,'(I8,A1)') J+IDPART2DMAX,':'
               DO K=1,9
                  CTEXT(K)=ICHAR(STR(K:K))
               ENDDO
               TITL=' '
               WRITE(TITL,'(A11,I8)') 'POLYHEDRON ',J
               DO K=1,LTITL
                  CTEXT(K+9)=ICHAR(TITL(K:K))
               ENDDO
               CTEXT(29)=0
               CALL WRITE_C_C(CTEXT,10+LTITL)
            ENDDO
         IDPART2DMAX = IDPART2DMAX + FVDATA(I)%NPOLH_ANIM
         ENDDO
      ENDIF
C-----------------------------------------------
C   NORMAL
C-----------------------------------------------
      CALL XYZNOR(XNORM)
C
      CALL DSECNOR(X    ,RWBUF,NPRW)
      IF (NSURG.GT.0) CALL DSRGNOR(IGRSURF,BUFSF)
      SNNSPHG = 0
      IF (ISPH3D*(NUMSPH_T+MAXPJET).GT.0)
     .   CALL DSPHNOR(KXSP,X,SPBUF,NNSPHG)
      IF (NUMELS16.GT.0)
     .    CALL XYZNOR16(IXS,IXS10,IXS20,IXS16,X)
C
      IF (IFVANI.EQ.1) THEN
         DO I=1,NFVNOD
            INORM(1) = 0
            INORM(2) = 0
            INORM(3) = 0
            CALL WRITE_S_C(INORM,3)
         ENDDO
         IF (NFVNOD.GT.0) THEN
            DO I=1,3
               INORM(1) = 0
               INORM(2) = 0
               INORM(3) = 0
               CALL WRITE_S_C(INORM,3)
            ENDDO
         ENDIF
      ENDIF
C-----------------------------------------------
C   ELEMENT MASS FOR MAS & FUNC
C-----------------------------------------------
      IF(ANIM_M.EQ.1.OR.ANIM_CE(3).EQ.1.OR.
     .   ANIM_CE(25).EQ.1)THEN
         CALL DMASANIC(ELBUF_TAB, X    ,D    ,GEO  ,IPARG,
     .               IXQ  ,IXC  ,IXTG ,MAS  ,PM   ,
     .               EL2FA,NBF  ,IXUR )
      ENDIF
C-----------------------------------------------
C   N FUNC TEXT
C-----------------------------------------------
C-----------------------------------------------
C   E(quad+shell+truss+..) FUNC TEXT
C-----------------------------------------------
      IF(NBF+NELCUT+NESBW2.NE.0)THEN
       DO I=1,NMANIM
          WRITE(CTMOD,'(A7,I4,A8,I4,A18)')
     .  'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Specific Energy'
          CALL ANI_TXT(CTMOD,41)
          WRITE(CTMOD,'(A7,I4,A8,I4,A11)')
     .  'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Pressure'
          CALL ANI_TXT(CTMOD,34)
          WRITE(CTMOD,'(A7,I4,A8,I4,A12)')
     .  'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Von Mises'
          CALL ANI_TXT(CTMOD,35)
          WRITE(CTMOD,'(A7,I4,A8,I4,A12)')
     .  'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Stress X '
          CALL ANI_TXT(CTMOD,35)
          WRITE(CTMOD,'(A7,I4,A8,I4,A12)')
     .  'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Stress Y '
          CALL ANI_TXT(CTMOD,35)
          WRITE(CTMOD,'(A7,I4,A8,I4,A12)')
     .  'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Stress Z '
          CALL ANI_TXT(CTMOD,35)
          WRITE(CTMOD,'(A7,I4,A8,I4,A12)')
     .  'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Stress XY'
          CALL ANI_TXT(CTMOD,35)
          WRITE(CTMOD,'(A7,I4,A8,I4,A12)')
     .  'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Stress YZ'
          CALL ANI_TXT(CTMOD,35)
          WRITE(CTMOD,'(A7,I4,A8,I4,A12)')
     .  'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Stress XZ'
          CALL ANI_TXT(CTMOD,35)
       ENDDO
       IF (DSANIM.EQ.1) THEN
          DO I=1,NLEVEL
             CALL ANI_TXT(CTITR(I),33)
          ENDDO
       ELSEIF (DECANI.EQ.1) THEN
          CALL ANI_TXT(CTITR(1),25)
       ENDIF
      ENDIF
C-----------------------------------------------
C   NODAL FUNC
C-----------------------------------------------
C-----------------------------------------------
C   ELEMENT FUNC (quad+coque)
C-----------------------------------------------
      NDMA2= NUMNOD*(MIN(1,ANIM_N(1)+OUTP_N(1))
     .              +MIN(1,ANIM_N(2)+OUTP_N(2))
     .              +MIN(1,ANIM_N(12)+OUTP_N(3)))
      IF((NBF+NELCUT+NESBW2.NE.0)) THEN
       DO I = 1,MX_ANI
        IFUNC = I
        IF(ANIM_CE(I).EQ.1) THEN

          CALL DFUNCC(ELBUF_TAB,BUFEL,WAFT ,IFUNC,IPARG,
     .                IXQ      ,IXC  ,IXTG ,PM   ,EL2FA,
     .                NBF      )
          R4 = ZERO
          DO J=1,NESBW2
            CALL WRITE_R_C(R4,1)
          ENDDO
C
          IF (NFVTR.GT.0) THEN
             R4=ZERO
             DO J=1,NFVTR
                CALL WRITE_R_C(R4,1)
             ENDDO
          ENDIF
C
        ENDIF
       ENDDO
C
       NMFUNC(1)=3
       NMFUNC(2)=6
       NMFUNC(3)=7
       NMFUNC(4)=14
       NMFUNC(5)=15
       NMFUNC(6)=16
       NMFUNC(7)=17
       NMFUNC(8)=18
       NMFUNC(9)=19
       DO I=1,NMANIM    ! Don't work !!!
          DO J=1,9
             IFUNC=NMFUNC(J)
             CALL DFUNCC(ELBUF_TAB,MBUFEL(1,I), WAFT, IFUNC, IPARG,
     .                   IXQ,         IXC,  IXTG,  PM,
     .                   EL2FA,       NBF)
             R4 = ZERO
             DO K=1,NESBW2
               CALL WRITE_R_C(R4,1)
             ENDDO
             IF (NFVTR.GT.0) THEN
                R4=ZERO
                DO K=1,NFVTR
                   CALL WRITE_R_C(R4,1)
                ENDDO
             ENDIF
          ENDDO
       ENDDO
c
       IF (DSANIM.EQ.1) THEN
          DO I=1,NLEVEL
             DO J=1,NBF
                FUNC(J)=ZERO
             ENDDO
C Quad + Shell
             OFF=1+NUMELS
             CALL DELSUB(NLEVEL, ELSUB, I, OFF, NUMELQ+NUMELC,
     .                   EL2FA , FUNC)
C Shell 3 nodes
             OFF=OFF+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR
             CALL DELSUB(NLEVEL, ELSUB, I, OFF, NUMELTG,
     .                   EL2FA(1+NUMELQ+NUMELC), FUNC)
C
             DO J=1,NBF
                R4=FUNC(J)
                CALL WRITE_R_C(R4,1)
             ENDDO
             R4=ZERO
             DO J=1,NESBW2
               CALL WRITE_R_C(R4,1)
             ENDDO
             IF (NFVTR.GT.0) THEN
                R4=ZERO
                DO J=1,NFVTR
                   CALL WRITE_R_C(R4,1)
                ENDDO
             ENDIF
          ENDDO
       ELSEIF (DECANI.EQ.1) THEN
          DO I=1,NBF
             FUNC(I)=ZERO
          ENDDO
C Quad + Shell
          OFF=1+NUMELS
          CALL DELSUB(1,     CEP, 1, OFF, NUMELQ+NUMELC,
     .                EL2FA, FUNC)
C Shell 3 nodes
          OFF=OFF+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR
          CALL DELSUB(1,     CEP, 1, OFF, NUMELTG,
     .                EL2FA(1+NUMELQ+NUMELC), FUNC)
C
          DO I=1,NBF
             R4=FUNC(I)
             CALL WRITE_R_C(R4,1)
          ENDDO
          R4=ZERO
          DO I=1,NESBW2
            CALL WRITE_R_C(R4,1)
          ENDDO
          IF (NFVTR.GT.0) THEN
             R4=ZERO
             DO J=1,NFVTR
                CALL WRITE_R_C(R4,1)
             ENDDO
          ENDIF
       ENDIF
      ENDIF
C-----------------------------------------------
C   VECT TEXT
C-----------------------------------------------
      DO I=1,NMANIM
         WRITE(CTMOD,'(A7,I4,A8,I4,A15)')
     .  'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Displacement'
         CALL ANI_TXT(CTMOD,38)
      ENDDO
C
      NNNSRG=NNSRG+NNSMD+NNSPH+2*NUMELS16
      DO I=1,NMANIM
         CALL VELVEC(MDEPL(1,I),NNWL,NNNSRG)
      ENDDO
C-----------------------------------------------
C   2D TENSOR TEXT
C-----------------------------------------------
      IF((NBF+NELCUT+NESBW2.NE.0))THEN
       DO I=1,NMANIM
          WRITE(CTMOD,'(A7,I4,A8,I4,A21)')
     .  'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),
     .  ' - Stress (membrane)'
          CALL ANI_TXT(CTMOD,44)
          WRITE(CTMOD,'(A7,I4,A8,I4,A23)')
     .  'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),
     .  ' - Stress (moment/t^2)'
          CALL ANI_TXT(CTMOD,46)
       ENDDO
C-----------------------------------------------
C   2D TENSOR
C-----------------------------------------------
        DO I = 1,MX_ANI
         IFUNC = I
         IF(ANIM_CT(I).EQ.1)THEN
           CALL TENSORC(ELBUF_TAB ,IPARG,IFUNC,
     .             INVERT,NELCUT,EL2FA,NBF  ,WAFT ,
     .             IAD,NBF_L,NBPART,
     .             X, IXC, IGEO,IXTG )
           R4 = ZERO
           DO J=1,NESBW2
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
           ENDDO
         ENDIF
       ENDDO
       NMFUNC(1)=1
       NMFUNC(2)=2
       DO I=1,NMANIM      ! Don't work !!!
          DO J=1,2
             IFUNC=NMFUNC(J)
             CALL TENSORC(MBUFEL(1,I), IPARG, IFUNC,
     .                    INVERT, NELCUT, EL2FA, NBF, WAFT,
     .                    IAD, NBF_L, NBPART,
     .                    X, IXC, IGEO, IXTG)
             R4 = ZERO
             DO K=1,NESBW2
              CALL WRITE_R_C(R4,1)
              CALL WRITE_R_C(R4,1)
              CALL WRITE_R_C(R4,1)
             ENDDO
          ENDDO
       ENDDO
      ENDIF
C-----------------------------------------------
C   ELEMENT MASS
C-----------------------------------------------
      IF(ANIM_M.EQ.1)THEN
       DO I=1,NBF
          R4 = MAS(I)
          CALL WRITE_R_C(R4,1)
       ENDDO
C
       R4 = 0.
       DO J=1,NESBW2+NELCUT
        CALL WRITE_R_C(R4,1)
       ENDDO
       IF (NFVTR.GT.0) THEN
         R4=ZERO
         DO J=1,NFVTR
            CALL WRITE_R_C(R4,1)
         ENDDO
       ENDIF
C-----------------------------------------------
C   NODAL MASS (FLUX FOR CUT)
C-----------------------------------------------
       DO I=1,NUMNOD
          WA4(I)=MS(I)
       ENDDO

       DO N=1,NRBYKIN
         M=NPBY(1,N)
         IF (M.GT.0) THEN
           WA4(M)=WA4(M)+(RBY(15,N)-MS(M))
         ENDIF
       ENDDO

       DO K=1,NUMNOD
         R4 = WA4(N)
         CALL WRITE_R_C(R4,1)
       ENDDO
C
       R4 = ZERO
       SZ16 = NUMELS16
       SZNNSPH = NNSPH
       DO N=1,NSECT+NRWALL+NNWL+NNSRG+NNSMD+SZNNSPH+2*SZ16
         CALL WRITE_R_C(R4,1)
       ENDDO
       IF (NFVNOD.GT.0) THEN
          R4=ZERO
          DO N=1,NFVNOD+3
             CALL WRITE_R_C(R4,1)
          ENDDO
       ENDIF
      ENDIF
C-------------------
C   NODAL NUMBERING
C-------------------
      CALL WRITE_I_C(ITAB,NUMNOD)
      SZ16 = NUMELS16
      SZNNSPH = NNSPH
      DO I=1,NODCUT+NSECT+NRWALL+NNWL+NNSRG+NNSMD+SZNNSPH+2*SZ16
       CALL WRITE_I_C(0,1)
      ENDDO
C
      IF (NFVNOD.GT.0) THEN
         DO I=1,NFVBAG
            IF (FVDATA(I)%NPOLH_ANIM.GT.0) THEN
               DO J=1,FVDATA(I)%NNS_ANIM
                  JJ=FVOFF(2,I)+J
                  CALL WRITE_I_C(JJ,1)
               ENDDO
            ENDIF
         ENDDO
         CALL WRITE_I_C(IDMAX+NFVNOD+1,1)
         CALL WRITE_I_C(IDMAX+NFVNOD+2,1)
         CALL WRITE_I_C(IDMAX+NFVNOD+3,1)
      ENDIF
C--------------------
C   ELEMENT NUMBERING
C--------------------
      CALL DELNUMBC(IPARG,IXQ  ,IXC  ,IXTG ,IXUR,
     .              EL2FA,NBF  ,WAFT ,NELCUT,
     .              NBPART,IDCMAX)
      DO J=1,NESBW2
       CALL WRITE_I_C(0,1)
      ENDDO
      IF (NFVTR.GT.0) THEN
         DO I=1,NFVTR
            CALL WRITE_I_C(IDCMAX+FVINUM(I),1)
         ENDDO
         DEALLOCATE(FVEL2FA, FVINUM)
      ENDIF
C-----------------------------------------------
C   HIERARCHIE
C-----------------------------------------------
C       Transmis a ANIM ::
C       Subset Rbodies  == NSUBS
C       Subset Sections == NSUBS+MIN(1,NRBODY)
C       Subset Rwalls   == NSUBS+MIN(1,NSECT)+MIN(1,NRBODY)
C       Subset Surfaces == NSUBS+MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWAL
C       Subset global   == NSUBS+MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWAL
C                                +MIN(1,NSURG+NSMAD)
      DO I=1,NPART
        IF(MATER(I).EQ.1) THEN
         IF (IPART(3,I).LT.NSUBS) THEN
          CALL WRITE_I_C(IPART(3,I)-1,1)
         ELSE
          CALL WRITE_I_C(NSUBS
     .                   +MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .                   +MIN(1,NSURG+NSMAD)+NFVSUBS-1,1)
         END IF
        END IF
      ENDDO
      DO I=1,NCUTS
        CALL WRITE_I_C(NSUBS
     .       +MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .       +MIN(1,NSURG+NSMAD)-1,1)
      ENDDO
      DO I=1,NSECT
        CALL WRITE_I_C(NSUBS+MIN(1,NRBODY)-1,1)
      END DO
      DO I=1,NRWALL
        CALL WRITE_I_C(NSUBS+MIN(1,NSECT)+MIN(1,NRBODY)-1,1)
      END DO
      DO I=1,NSURG
        CALL WRITE_I_C(NSUBS+MIN(1,NSECT)+MIN(1,NRBODY)
     .                      +MIN(1,NRWALL)-1,1)
      END DO
      DO I=1,NSMAD
        CALL WRITE_I_C(NSUBS+MIN(1,NSECT)+MIN(1,NRBODY)
     .                      +MIN(1,NRWALL)-1,1)
      END DO
      IF (NFVTR.GT.0) THEN
         II=NSUBS
     .       +MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .       +MIN(1,NSURG+NSMAD)-1
         DO I=1,NFVBAG
            IF (FVDATA(I)%NPOLH_ANIM.GT.0) THEN
               II=II+1
               DO J=1,FVDATA(I)%NPOLH_ANIM
                  CALL WRITE_I_C(II-1,1)
               ENDDO
            ENDIF
         ENDDO
      ENDIF
C
      DO I=1,NPART
        IF(MATER(I).EQ.1)CALL WRITE_I_C(IPART(1,I),1)
      ENDDO
      DO I=1,NCUTS+NRWALL+NSECT+NSURG+NSMAD
        CALL WRITE_I_C(0,1)
      ENDDO
      DO I=1,NFVPART
        CALL WRITE_I_C(0,1)
      ENDDO
C
      DO I=1,NPART
        IF(MATER(I).EQ.1)CALL WRITE_I_C(IPART(2,I),1)
      ENDDO
      DO I=1,NCUTS+NRWALL+NSECT+NSURG+NSMAD
        CALL WRITE_I_C(0,1)
      ENDDO
      DO I=1,NFVPART
        CALL WRITE_I_C(0,1)
      ENDDO
C=======================================================================
C
C      BRICKS
C
C=======================================================================
      IF (NUMELS_T+NUMELS16_T+ISPH3D*(NUMSPH_T+MAXPJET).GE.0.OR.
     .    (ISPH3D.EQ.1.AND.NUMSPH_T+MAXPJET.GT.0)) THEN
         IF (DSANIM.EQ.1) THEN
            NSE_ANI=NSE_ANI+NLEVEL
         ELSEIF (DECANI.EQ.1) THEN
            NSE_ANI=NSE_ANI+1
         ENDIF
      ENDIF
      IF(NUMELS_T+NUMELS16_T+ISPH3D*(NUMSPH_T+MAXPJET).EQ.0)GOTO 400
C-----------------------------------------------
C   PART COUNT
C-----------------------------------------------
C
      DO I=1,NUMELS
        MATER(IPARTS(I))=2
        EL2FA(I)=0
      ENDDO

      DO I=1,3*NUMELS16
        EL2FA(NUMELS+I)=0
      ENDDO

C       3D geometry is not yet treated.

      IF(ISPH3D.NE.0)THEN
       DO I=1,NUMSPH+MAXPJET
        MATER(IPARTSP(I))=2
        EL2FA(NUMELS+3*NUMELS16+I)=0
       ENDDO
      ENDIF
C
      NBPART = 0
      DO I=1,NPART
        NBPART = NBPART + MATER(I)/2
      ENDDO
C-----------------------------------------------
C   WRITE CONTROL
C-----------------------------------------------
      CALL WRITE_I_C(NUMELS+ISPH3D*(NUMSPH+MAXPJET)
     .                     +3*NUMELS16,1)
      CALL WRITE_I_C(NBPART,1)
      CALL WRITE_I_C(NSE_ANI,1)
      CALL WRITE_I_C(NST_ANI,1)
C-----------------------------------------------
C   PART SORT
C-----------------------------------------------
      SHFTSPH = NUMNOD+NODCUT+NSECT+NRWALL+NNWL+NNSRG+NNSMD
      SHFT16 = NUMNOD+NODCUT+NSECT+NRWALL+NNWL+NNSRG+NNSMD+NNSPHG
      INSPH=NUMNOD+NODCUT+NSECT+NRWALL+NNWL
     .                 +NNSRG+NNSMD
      CALL PARSORS(IAD  ,IPARG     ,IXS    ,MATER,IPARTS,
     2            EL2FA ,
     3                             INSPH  ,KXSP  ,IPARTSP,
     4            IXS10 ,IXS20    ,IXS16  ,NNSPH ,ISPH3D,
     5            SHFT16  ,SHFTSPH,NNSPHG )
C-----------------------------------------------
C   OFF
C-----------------------------------------------
      NNN = NUMELS+ISPH3D*(NUMSPH+MAXPJET)+3*NUMELS16
      CALL ANIOFFS(ELBUF_TAB ,IPARG,WAFT         ,EL2FA  ,
     .             NNN   ,NBPART,ISPH3D )
C-----------------------------------------------
C   PART ADD
C-----------------------------------------------
      CALL WRITE_I_C(IAD,NBPART)
C-----------------------------------------------
C   PART HEAD
C-----------------------------------------------
      DO I=1,NPART
       IF(MATER(I).EQ.2)THEN
         WRITE(STR,'(I8,A1)')IPART(4,I),':'
         DO J=1,9
           CTEXT(J)=ICHAR(STR(J:J))
         ENDDO
         IB = 9
           CALL FRETITL2(TITL,IPART(LIPART1-LTITR+1,I),LTITL)
           DO J=1,LTITL
             IF(TITL(J:J).NE.' ') IB = J+9
             CTEXT(J+9)=ICHAR(TITL(J:J))
           END DO
           CTEXT(IB+1)=0
           CALL WRITE_C_C(CTEXT,10+LTITL)
       ENDIF
      ENDDO
C-----------------------------------------------
C   ELEMENT MASS FOR MAS & FUNC
C-----------------------------------------------
      IF(ANIM_M.EQ.1.OR.ANIM_SE(3).EQ.1.OR.
     .   ANIM_SE(25).EQ.1)THEN
         CALL DMASANIS(ELBUF_TAB,IPARG   ,
     2              IXS     ,MAS     ,PM      ,EL2FA   ,NUMELS  ,
     3              IPART   ,IPARTSP ,ISPH3D  )
      ENDIF
C-----------------------------------------------
C   BRICK FUNC TEXT
C-----------------------------------------------
      CTEXT(81)=0
      DO I=1,NMANIM
         WRITE(CTMOD,'(A7,I4,A8,I4,A18)')
     . 'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Specific Energy'
         CALL ANI_TXT(CTMOD,41)
         WRITE(CTMOD,'(A7,I4,A8,I4,A11)')
     . 'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Pressure'
         CALL ANI_TXT(CTMOD,34)
         WRITE(CTMOD,'(A7,I4,A8,I4,A12)')
     . 'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Von Mises'
         CALL ANI_TXT(CTMOD,35)
         WRITE(CTMOD,'(A7,I4,A8,I4,A12)')
     . 'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Stress X '
         CALL ANI_TXT(CTMOD,35)
         WRITE(CTMOD,'(A7,I4,A8,I4,A12)')
     . 'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Stress Y '
         CALL ANI_TXT(CTMOD,35)
         WRITE(CTMOD,'(A7,I4,A8,I4,A12)')
     . 'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Stress Z '
         CALL ANI_TXT(CTMOD,35)
         WRITE(CTMOD,'(A7,I4,A8,I4,A12)')
     . 'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Stress XY'
         CALL ANI_TXT(CTMOD,35)
         WRITE(CTMOD,'(A7,I4,A8,I4,A12)')
     . 'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Stress YZ'
         CALL ANI_TXT(CTMOD,35)
         WRITE(CTMOD,'(A7,I4,A8,I4,A12)')
     . 'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Stress XZ'
         CALL ANI_TXT(CTMOD,35)
      ENDDO
      IF (DSANIM.EQ.1) THEN
         DO I=1,NLEVEL
            CALL ANI_TXT(CTITR(I),33)
         ENDDO
      ELSEIF (DECANI.EQ.1) THEN
          CALL ANI_TXT(CTITR(1),25)
      ENDIF
C-----------------------------------------------
C   ELEMENT FUNC (brick)
C-----------------------------------------------
      NDMA2= NUMNOD*(MIN(1,ANIM_N(1)+OUTP_N(1))
     .              +MIN(1,ANIM_N(2)+OUTP_N(2))
     .              +MIN(1,ANIM_N(12)+OUTP_N(3)))
      NNN = NUMELS+ISPH3D*(NUMSPH+MAXPJET)+3*NUMELS16
      DO I = 1,MX_ANI
        IFUNC = I
        IF(ANIM_SE(I).EQ.1) THEN
          CALL DFUNCS(ELBUF_TAB ,WAFT    ,IFUNC   ,IPARG   ,
     2              IXS     ,PM      ,EL2FA   ,NNN     ,ISPH3D  )
        ENDIF
      ENDDO
C
      NMFUNC(1)=3
      NMFUNC(2)=6
      NMFUNC(3)=7
      NMFUNC(4)=14
      NMFUNC(5)=15
      NMFUNC(6)=16
      NMFUNC(7)=17
      NMFUNC(8)=18
      NMFUNC(9)=19
      DO I=1,NMANIM    ! Don't work !!!
         DO J=1,9
            IFUNC=NMFUNC(J)
            CALL DFUNCS(MBUFEL(1,I), WAFT,  IFUNC,   IPARG,
     .                  IXS,PM   ,EL2FA,   NNN,ISPH3D)
         ENDDO
      ENDDO
      IF (DSANIM.EQ.1) THEN
         DO I=1,NLEVEL
            DO J=1,NNN
               FUNC(J)=ZERO
            ENDDO
C Brick
            OFF=1
            CALL DELSUB(NLEVEL, ELSUB,  I, OFF, NUMELS,
     .                  EL2FA,  FUNC)
C
            DO J=1,NNN
               R4=FUNC(J)
               CALL WRITE_R_C(R4,1)
            ENDDO
         ENDDO
      ELSEIF (DECANI.EQ.1) THEN
         DO I=1,NNN
            FUNC(I)=ZERO
         ENDDO
C Brick
         OFF=1
         CALL DELSUB(1,     CEP,    1, OFF, NUMELS,
     .               EL2FA, FUNC)
C Particules SPH
         IF (ISPH3D.EQ.1) THEN
            OFF=1
            CALL DELSUB(1, CEPSP,1 ,OFF, NUMSPH,
     .                  EL2FA(1+NUMELS), FUNC)
         ENDIF
C
         DO I=1,NNN
            R4=FUNC(I)
            CALL WRITE_R_C(R4,1)
         ENDDO
      ENDIF
C-----------------------------------------------
C   3D TENSOR TEXT
C-----------------------------------------------
      DO I=1,NMANIM
         WRITE(CTMOD,'(A7,I4,A8,I4,A9)')
     . 'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Stress'
         CALL ANI_TXT(CTMOD,32)
      ENDDO
C-----------------------------------------------
C   3D TENSOR
C-----------------------------------------------
      DO I = 1,MX_ANI
       IFUNC = I
       IF(ANIM_ST(I).EQ.1)THEN
           CALL TENSORS(ELBUF_TAB ,IPARG   ,IFUNC   ,IXS     ,PM      ,
     2                  EL2FA   ,NNN     ,WAFT    ,
     3                  X       ,IPART   ,IPARTSP ,ISPH3D  ,IPM     )
       ENDIF
      ENDDO
      NMFUNC(1)=1
      DO I=1,NMANIM
         DO J=1,1
            IFUNC=NMFUNC(J)
            CALL TENSORS(MBUFEL(1,I), IPARG,  IFUNC, IXS,   PM,
     .                   EL2FA,       NNN,    WAFT,
     .                   X,     IPART,IPARTSP,     ISPH3D, IPM  )
         ENDDO
      ENDDO
C-----------------------------------------------
C   ELEMENT MASS
C-----------------------------------------------
      IF(ANIM_M.EQ.1)THEN
       DO I=1,NNN
          R4 = MAS(I)
          CALL WRITE_R_C(R4,1)
       ENDDO
      ENDIF
C-----------------------------------------------
C   BRICK NUMBERING
C-----------------------------------------------
       CALL DELNUMBS(IPARG,IXS  ,EL2FA,NNN ,WAFT ,
     .               KXSP ,ISPH3D )
C-----------------------------------------------
C   HIERARCHIE
C-----------------------------------------------
      DO I=1,NPART
       IF(MATER(I).EQ.2)THEN
         IF (IPART(3,I).LT.NSUBS) THEN
          CALL WRITE_I_C(IPART(3,I)-1,1)
         ELSE
          CALL WRITE_I_C(NSUBS
     .                   +MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .                   +MIN(1,NSURG+NSMAD)-1,1)
         END IF
       END IF
      ENDDO
      DO I=1,NPART
        IF(MATER(I).EQ.2)CALL WRITE_I_C(IPART(1,I),1)
      ENDDO
      DO I=1,NPART
        IF(MATER(I).EQ.2)CALL WRITE_I_C(IPART(2,I),1)
      ENDDO
C=======================================================================
 400  CONTINUE
C=======================================================================
C
C     POUTRE TRUSS SPRING
C     + RBODIES
C
C=======================================================================
      NERBY = 0
      IF (NRBODY.GT.0)
     .    CALL DRBYCNT(NERBY,NPBY)
      NB1D_T = NB1D
      IF(NB1D+NANIM1D+NERBY.EQ.0) GOTO 600
C-----------------------------------------------
C   PART COUNT
C-----------------------------------------------
C
      DO I=1,NUMELT
        MATER(IPARTT(I))=3
      ENDDO
      DO I=1,NUMELP
        MATER(IPARTP(I))=3
      ENDDO
      DO I=1,NUMELR
        MATER(IPARTR(I))=3
      ENDDO
      DO I=1,NUMELX
        IPRT=IPARTX(I)
        IF (NFACPTX(1,IPRT).GT.0) THEN
         MATER(IPRT)=3
        ELSE
         MATER(IPRT)=0
        ENDIF
      ENDDO
C
      NBPART = 0
      DO I=1,NPART
        NBPART = NBPART + MATER(I)/3
      ENDDO
C
      DO I=1,NB1D + 1
        EL2FA(I)=0
      ENDDO
C-----------------------------------------------
C   WRITE CONTROL
C-----------------------------------------------
      CALL WRITE_I_C(NB1D+NANIM1D+NERBY,1)
      CALL WRITE_I_C(NBPART+NRBODY,1)
      IF (DSANIM.EQ.1) THEN
         NFE_ANI=NFE_ANI+NLEVEL
      ELSEIF (DECANI.EQ.1) THEN
         NFE_ANI=NFE_ANI+1
      ENDIF
      CALL WRITE_I_C(NFE_ANI,1)
      CALL WRITE_I_C(NFT_ANI,1)
C FLAG POUR SKEW
      CALL WRITE_I_C(1,1)

C-----------------------------------------------
C   PART SORT
C-----------------------------------------------
      CALL PARSORF(IAD  ,IPARG,IXT  ,IXP  ,IXR  ,
     .            MATER,EL2FA,
     .            IPARTT,IPARTP,IPARTR,NFACPTX,IXEDGE)
      IF(NRBODY.GT.0) THEN
        CALL DPARRBY(LPBY ,NPBY )
      ENDIF
C-----------------------------------------------
C   OFF
C-----------------------------------------------
      CALL ANIOFFF(ELBUF_TAB,IPARG,WAFT,EL2FA,
     .             NB1D  ,IOFFX1)
      DO J=1,NERBY
        CALL WRITE_C_C(1,1)
      ENDDO
C-----------------------------------------------
C   PART ADD
C-----------------------------------------------
      CALL WRITE_I_C(IAD,NBPART)
      DO I=1,NRBODY
        NERBT(I)=0
      ENDDO
      NERBY1=0
      DO IRBY=1,NRBODY
       CALL DONERBY(IRBY,NERBY1,NPBY,NERBT)
       CALL WRITE_I_C(NB1D+NANIM1D+NERBY1,1)
      END DO
C-----------------------------------------------
C   PART HEAD
C-----------------------------------------------
      DO I=1,NPART
       IF(MATER(I).EQ.3)THEN
         WRITE(STR,'(I8,A1)')IPART(4,I),':'
         DO J=1,9
           CTEXT(J)=ICHAR(STR(J:J))
         ENDDO
         IB = 9

           CALL FRETITL2(TITL,IPART(LIPART1-LTITR+1,I),LTITL)
           DO J=1,LTITL
             IF(TITL(J:J).NE.' ') IB = J+9
             CTEXT(J+9)=ICHAR(TITL(J:J))
           END DO
           CTEXT(IB+1)=0
           CALL WRITE_C_C(CTEXT,10+LTITL)

       ENDIF
      ENDDO
C
      IF (INVSTR.LT.40) THEN
       DO IRBY=1,NRBODY
         WRITE(STR,'(I8,A2,A10)') IRBY,': ','Rigid Body'
         DO J=1,20
          CTEXT(J)=ICHAR(STR(J:J))
         ENDDO
         IB = 20
         CTEXT(IB+1)=0

         CALL WRITE_C_C(CTEXT,10+LTITL)
       END DO
      ELSE
       DO IRBY=1,NRBODY
         WRITE(STR,'(I8,A2)') NOM_OPT(1,IRBY),': '
         DO J=1,10
          CTEXT(J)=ICHAR(STR(J:J))
         ENDDO

         CALL FRETITL2(TITL,NOM_OPT(LNOPT1-LTITR+1,IRBY),
     .                 LTITL)
         IB = LTITL+9
         DO J=1,LTITL
           CTEXT(J+10)=ICHAR(TITL(J:J))
         END DO
         CTEXT(IB+1)=0
         CALL WRITE_C_C(CTEXT,10+LTITL)
       END DO
      END IF
C-----------------------------------------------
C   ELEMENT MASS FOR MAS & FUNC
C-----------------------------------------------
      IF(ANIM_M.EQ.1.OR.ANIM_FE(3).EQ.1)THEN
         CALL DMASANIF(X     ,D    ,ELBUF_TAB,GEO  ,IPARG,
     .                 IXT   ,IXP  ,IXR      ,MAS  ,PM   ,
     .                 EL2FA ,NB1D )
      ENDIF
C-----------------------------------------------
C   E(truss+..) FUNC TEXT
C-----------------------------------------------
      DO I=1,NMANIM
         WRITE(CTMOD,'(A7,I4,A8,I4,A18)')
     . 'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Specific Energy'
         CALL ANI_TXT(CTMOD,41)
         WRITE(CTMOD,'(A7,I4,A8,I4,A12)')
     . 'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Von Mises'
         CALL ANI_TXT(CTMOD,35)
         WRITE(CTMOD,'(A7,I4,A8,I4,A12)')
     . 'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Stress X '
         CALL ANI_TXT(CTMOD,35)
         WRITE(CTMOD,'(A7,I4,A8,I4,A12)')
     . 'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Stress Y '
         CALL ANI_TXT(CTMOD,35)
         WRITE(CTMOD,'(A7,I4,A8,I4,A12)')
     . 'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Stress Z '
         CALL ANI_TXT(CTMOD,35)
         WRITE(CTMOD,'(A7,I4,A8,I4,A12)')
     . 'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Stress XY'
         CALL ANI_TXT(CTMOD,35)
         WRITE(CTMOD,'(A7,I4,A8,I4,A12)')
     . 'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Stress YZ'
         CALL ANI_TXT(CTMOD,35)
         WRITE(CTMOD,'(A7,I4,A8,I4,A12)')
     . 'Fxbody ',FXANI(1,I),' - Mode ',FXANI(2,I),' - Stress XZ'
         CALL ANI_TXT(CTMOD,35)
      ENDDO
      IF (DSANIM.EQ.1) THEN
         DO I=1,NLEVEL
            CALL ANI_TXT(CTITR(I),33)
         ENDDO
      ELSEIF (DECANI.EQ.1) THEN
         CALL ANI_TXT(CTITR(1),25)
      ENDIF
C-----------------------------------------------
C   ELEMENT FUNC (truss+..)
C-----------------------------------------------
      NDMA2= NUMNOD*(MIN(1,ANIM_N(1)+OUTP_N(1))
     .              +MIN(1,ANIM_N(2)+OUTP_N(2))
     .              +MIN(1,ANIM_N(12)+OUTP_N(3)))
      DO I = 1,MX_ANI
        IFUNC = I
        IF(ANIM_FE(I).EQ.1) THEN

          CALL DFUNCF(ELBUF_TAB,WAFT  ,IFUNC ,IPARG  ,GEO   ,
     .                IXT      ,IXP   ,IXR   ,MAS    ,PM    ,
     .                EL2FA    ,NB1D  ,IAD   ,NBPART ,XFUNC1)
          R4 = ZERO
          DO J=1,NERBY
            CALL WRITE_R_C(R4,1)
          ENDDO
        ENDIF
      ENDDO
C
      NMFUNC(1)=3
      NMFUNC(2)=7
      NMFUNC(3)=14
      NMFUNC(4)=15
      NMFUNC(5)=16
      NMFUNC(6)=17
      NMFUNC(7)=18
      NMFUNC(8)=19
      DO I=1,NMANIM
         DO J=1,8
            IFUNC=NMFUNC(J)
            CALL DFUNCF(MBUFEL(1,I), WAFT, IFUNC, IPARG,  GEO,
     .                  IXT,         IXP,  IXR,   MAS,    PM,
     .                  EL2FA,       NB1D, IAD,   NBPART,
     .                  XFUNC1)
            R4 = ZERO
            DO K=1,NERBY
              CALL WRITE_R_C(R4,1)
            ENDDO
         ENDDO
      ENDDO
      IF (DSANIM.EQ.1) THEN
         DO I=1,NLEVEL
            DO J=1,NB1D
               FUNC(J)=ZERO
            ENDDO
C 1D elements
            OFF=1+NUMELS+NUMELQ+NUMELC
            CALL DELSUB(NLEVEL, ELSUB, I, OFF, NUMELT+NUMELP+NUMELR,
     .                  EL2FA,  FUNC )
C
            DO J=1,NB1D
               R4=FUNC(J)
               CALL WRITE_R_C(R4,1)
            ENDDO
            R4 = ZERO
            DO J=1,NANIM1D
               CALL WRITE_R_C(R4,1)
            ENDDO
            DO J=1,NERBY
              CALL WRITE_R_C(R4,1)
            ENDDO
         ENDDO
      ELSEIF (DECANI.EQ.1) THEN
         DO I=1,NB1D
            FUNC(I)=ZERO
         ENDDO
C 1D elements
         OFF=1+NUMELS+NUMELQ+NUMELC
         CALL DELSUB(1,     CEP,  1, OFF, NUMELT+NUMELP+NUMELR,
     .               EL2FA, FUNC)
C
         DO I=1,NB1D
            R4=FUNC(I)
            CALL WRITE_R_C(R4,1)
         ENDDO
         R4=ZERO
         DO I=1,NANIM1D
            CALL WRITE_R_C(R4,1)
         ENDDO
         DO I=1,NERBY
           CALL WRITE_R_C(R4,1)
         ENDDO
      ENDIF
C-----------------------------------------------
C   SKEW
C-----------------------------------------------
      LRBUF = 0

      CALL ANISKEWF(GEO,SKEW,IPARG,IXR,LRBUF)
      DO I=1,NANIM1D
       CALL WRITE_I_C(0,1)
      ENDDO
      DO J=1,NERBY
       CALL WRITE_I_C(0,1)
      ENDDO
C-----------------------------------------------
C   ELEMENT MASS
C-----------------------------------------------
      IF(ANIM_M.EQ.1)THEN
       DO I=1,NB1D
          R4 = MAS(I)
          CALL WRITE_R_C(R4,1)
       ENDDO
       R4 = 0.
       DO J=1,NERBY
        CALL WRITE_R_C(R4,1)
       ENDDO
      ENDIF
C-----------------------------------------------
C   ELEMENT NUMBERING
C-----------------------------------------------
      CALL DELNUMBF(IPARG,IXT  ,IXP  ,IXR  ,
     .             EL2FA,NB1D  ,WAFT       ,
     .             INUMX1 )
      DO J=1,NERBY
       CALL WRITE_I_C(0,1)
      ENDDO
C-----------------------------------------------
C   HIERARCHIE
C-----------------------------------------------
      DO I=1,NPART
         IF(MATER(I).EQ.3)THEN
           IF (IPART(3,I).LT.NSUBS) THEN
            CALL WRITE_I_C(IPART(3,I)-1,1)
           ELSE
            CALL WRITE_I_C(NSUBS
     .                     +MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .                     +MIN(1,NSURG+NSMAD)-1,1)
           END IF
         END IF
      ENDDO
c     Subset Rbodies  == NSUBS
      DO I=1,NRBODY
          CALL WRITE_I_C(NSUBS-1,1)
      END DO
      DO I=1,NPART
        IF(MATER(I).EQ.3)CALL WRITE_I_C(IPART(1,I),1)
      ENDDO
      DO I=1,NRBODY
          CALL WRITE_I_C(0,1)
      ENDDO
      DO I=1,NPART
        IF(MATER(I).EQ.3)CALL WRITE_I_C(IPART(2,I),1)
      ENDDO
      DO I=1,NRBODY
          CALL WRITE_I_C(0,1)
      ENDDO
C=======================================================================
 600  CONTINUE
C=======================================================================
C
C   HIERARCHIE
C
C=======================================================================
       J=0
       DO I=1,NPART
         IF(MATER(I).EQ.1)THEN
           J=J+1
           MATER(I)=J
         ELSE
           MATER(I)=-MATER(I)
         ENDIF
       ENDDO
       M01=J
       J=J+NCUTS+NRWALL+NSECT+NSURG+NSMAD
       M1=J
       DO I=1,NPART
         IF(MATER(I).EQ.-2)THEN
           J=J+1
           MATER(I)=J
         ENDIF
       ENDDO
       M2=J
       DO I=1,NPART
         IF(MATER(I).EQ.-3)THEN
           J=J+1
           MATER(I)=J
         ENDIF
       ENDDO
       M3=J+NRBODY
C-----------------------------------------------
C   WRITE CONTROL
C-----------------------------------------------
      CALL WRITE_I_C(NSUBS
     .     +MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .     +MIN(1,NSURG+NSMAD)+NFVSUBS,1)
C-----------------------------------------------
C   SUBSET HEAD/PARENT/
C-----------------------------------------------
       IF (NSUBS.EQ.1) THEN
C-----------------------------
C ONE SEUL SUBSET OU INPUT V31
C-----------------------------
          MXSUBS=1
C-----------------
C SUBSET RBODIES
C-----------------
          IF (NRBODY.GT.0) THEN
           WRITE(STR,'(I8,A14)')MXSUBS+1,':RBODIES MODEL'
           DO J=1,22
            CTEXT(J)=ICHAR(STR(J:J))
           ENDDO
           CTEXT(23)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
           CALL WRITE_I_C(NSUBS
     .                    +MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .                    +MIN(1,NSURG+NSMAD)+NFVSUBS-1,1)
C #SUBSETS FILS
           CALL WRITE_I_C(0,1)
C SUBSETS FILS
C #PARTS FILLES
           N1=0
           N2=0
           N3=NRBODY
C PARTS FILLES 2D
           CALL WRITE_I_C(N1,1)
C PARTS FILLES 3D
           CALL WRITE_I_C(N2,1)
C PARTS FILLES 1D
           CALL WRITE_I_C(N3,1)
           DO J=NRBODY,1,-1
              CALL WRITE_I_C(M3-J-M2,1)
           ENDDO
          END IF
C-----------------
C SUBSET SECTIONS
C-----------------
          IF (NSECT.GT.0) THEN
           WRITE(STR,'(I8,A15)')MXSUBS+MIN(1,NRBODY)+1,':SECTIONS MODEL'
           DO J=1,23
            CTEXT(J)=ICHAR(STR(J:J))
           ENDDO
           CTEXT(24)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
           CALL WRITE_I_C(NSUBS
     .                    +MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .                    +MIN(1,NSURG+NSMAD)+NFVSUBS-1,1)
C #SUBSETS FILS
           CALL WRITE_I_C(0,1)
C SUBSETS FILS
C #PARTS FILLES
           N1=NSECT
           N2=0
           N3=0
C PARTS FILLES 2D
           CALL WRITE_I_C(N1,1)
           DO J=NSECT,1,-1
              CALL WRITE_I_C(M1-NSURG-NSMAD-NRWALL-J,1)
           ENDDO
C PARTS FILLES 3D
           CALL WRITE_I_C(N2,1)
C PARTS FILLES 1D
           CALL WRITE_I_C(N3,1)
          END IF
C-----------------
C SUBSET RWALLS
C-----------------
          IF (NRWALL.GT.0) THEN
           WRITE(STR,'(I8,A13)')MXSUBS
     .              +MIN(1,NSECT)+MIN(1,NRBODY)+1,':RWALLS MODEL'
           DO J=1,21
            CTEXT(J)=ICHAR(STR(J:J))
           ENDDO
           CTEXT(22)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
           CALL WRITE_I_C(NSUBS
     .                    +MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .                    +MIN(1,NSURG+NSMAD)+NFVSUBS-1,1)
C #SUBSETS FILS
           CALL WRITE_I_C(0,1)
C SUBSETS FILS
C #PARTS FILLES
           N1=NRWALL
           N2=0
           N3=0
C PARTS FILLES 2D
           CALL WRITE_I_C(N1,1)
           DO J=NRWALL,1,-1
              CALL WRITE_I_C(M1-NSURG-NSMAD-J,1)
           ENDDO
C PARTS FILLES 3D
           CALL WRITE_I_C(N2,1)
C PARTS FILLES 1D
           CALL WRITE_I_C(N3,1)
          END IF
C-----------------
C SUBSET SURFACES
C-----------------
          IF (NSURG+NSMAD.GT.0) THEN
           WRITE(STR,'(I8,A15)')MXSUBS
     .              +MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)+1,
     .              ':SURFACES MODEL'
           DO J=1,23
            CTEXT(J)=ICHAR(STR(J:J))
           ENDDO
           CTEXT(24)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
           CALL WRITE_I_C(NSUBS
     .                    +MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .                    +MIN(1,NSURG+NSMAD)+NFVSUBS-1,1)
C #SUBSETS FILS
           CALL WRITE_I_C(0,1)
C SUBSETS FILS
C #PARTS FILLES
           N1=NSURG+NSMAD
           N2=0
           N3=0
C PARTS FILLES 2D
           CALL WRITE_I_C(N1,1)
           DO J=NSURG+NSMAD,1,-1
              CALL WRITE_I_C(M1-J,1)
           ENDDO
C PARTS FILLES 3D
           CALL WRITE_I_C(N2,1)
C PARTS FILLES 1D
           CALL WRITE_I_C(N3,1)
          END IF
C-----------------
C SUBSETS FVMBAG
C-----------------
          IF (NFVSUBS.GT.0) THEN
             II=NSUBS
     .         +MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .         +MIN(1,NSURG+NSMAD)
             OFFPART=NBPART2D
             DO I=1,NFVBAG
                IF (FVDATA(I)%NPOLH_ANIM.GT.0) THEN
                   II=II+1
                   WRITE(STR,'(I8,A11,I8)')
     .                 II,':FVMBAG ID ',FVDATA(I)%ID
                   DO J=1,27
                    CTEXT(J)=ICHAR(STR(J:J))
                   ENDDO
                   CTEXT(28)=0
                   CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
                   CALL WRITE_I_C(NSUBS
     .                    +MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .                    +MIN(1,NSURG+NSMAD)+NFVSUBS-1,1)
C #SUBSETS FILS
                   CALL WRITE_I_C(0,1)
C PARTS FILLES 2D
                   CALL WRITE_I_C(FVDATA(I)%NPOLH_ANIM,1)
                   DO J=1,FVDATA(I)%NPOLH_ANIM
                      CALL WRITE_I_C(OFFPART+J-1,1)
                   ENDDO
                   OFFPART=OFFPART+FVDATA(I)%NPOLH_ANIM
C PARTS FILLES 3D
                   CALL WRITE_I_C(0,1)
C PARTS FILLES 1D
                   CALL WRITE_I_C(0,1)
                ENDIF
             ENDDO
          ENDIF
C--------------
C GLOBAL MODEL
C--------------
          WRITE(STR,'(I8,A13)')1,':GLOBAL MODEL'
          DO J=1,21
            CTEXT(J)=ICHAR(STR(J:J))
          ENDDO
          CTEXT(22)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT
          CALL WRITE_I_C(-1,1)
C #SUBSETS FILS
          CALL WRITE_I_C(MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .                   +MIN(1,NSURG+NSMAD)+NFVSUBS,1)
C SUBSETS FILS
          IF (NRBODY.GT.0)
     .      CALL WRITE_I_C(NSUBS-1,1)
          IF (NSECT.GT.0)
     .      CALL WRITE_I_C(NSUBS+MIN(1,NRBODY)-1,1)
          IF (NRWALL.GT.0)
     .      CALL WRITE_I_C(NSUBS+MIN(1,NSECT)+MIN(1,NRBODY)-1,1)
          IF (NSURG+NSMAD.GT.0)
     .      CALL WRITE_I_C(NSUBS+MIN(1,NSECT)+MIN(1,NRBODY)
     .                          +MIN(1,NRWALL)-1,1)
          IF (NFVSUBS.GT.0) THEN
             II=MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .                   +MIN(1,NSURG+NSMAD)+1
             DO I=1,NFVBAG
                II=II+1
                CALL WRITE_I_C(II-1,1)
             ENDDO
          ENDIF
C #PARTS FILLES
          N1=0
          N2=0
          N3=0
          DO K=1,NPART
            IF(MATER(K).GT.0.AND.MATER(K).LE.M01)THEN
               N1=N1+1
            ELSEIF(MATER(K).GT.M1.AND.MATER(K).LE.M2)THEN
               N2=N2+1
            ELSEIF(MATER(K).GT.M2)THEN
               N3=N3+1
            ENDIF
          ENDDO
C CUTS DANS LE SUBSET GLOBAL
          N1=N1+NCUTS
C PARTS FILLES 2D
          CALL WRITE_I_C(N1,1)
          DO K=1,NPART
            IF(MATER(K).GT.0.AND.MATER(K).LE.M01)
     .         CALL WRITE_I_C(MATER(K)-1,1)
          ENDDO
C CUTS DANS LE SUBSET GLOBAL
          DO J=1,NCUTS
              CALL WRITE_I_C(M1-NRWALL-NSECT-NSURG-NSMAD-J,1)
          ENDDO
C PARTS FILLES 3D
          CALL WRITE_I_C(N2,1)
          DO K=1,NPART
            IF(MATER(K).GT.M1.AND.MATER(K).LE.M2)
     .          CALL WRITE_I_C(MATER(K)-M1-1,1)
          ENDDO
C PARTS FILLES 1D
          CALL WRITE_I_C(N3,1)
          DO K=1,NPART
            IF(MATER(K).GT.M2)CALL WRITE_I_C(MATER(K)-M2-1,1)
          ENDDO
       ELSE
C----------------
C +SIEURS SUBSET ET INPUT V4.X
C----------------
        MXSUBS=0
        DO I=1,NSUBS-1
          IF (SUBSET(I)%ID .GT. MXSUBS) MXSUBS=SUBSET(I)%ID
          WRITE(STR,'(I8,A1)')SUBSET(I)%ID,':'
          DO J=1,9
            CTEXT(J)=ICHAR(STR(J:J))
          ENDDO
          IB = 9
          TITL = SUBSET(I)%TITLE
          DO J=1,LTITL
            IF(TITL(J:J).NE.' ') IB = J+9
            CTEXT(J+9)=ICHAR(TITL(J:J))
          ENDDO
          CTEXT(IB+1)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT
          IF (SUBSET(I)%PARENT .LT. NSUBS) THEN
           CALL WRITE_I_C(SUBSET(I)%PARENT-1,1)
          ELSE
           CALL WRITE_I_C(SUBSET(I)%PARENT
     .          +MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .          +MIN(1,NSURG+NSMAD)+NFVSUBS-1,1)
          END IF
C #SUBSETS FILS
          CALL WRITE_I_C(SUBSET(I)%NCHILD,1)
C SUBSETS FILS
          DO J=1,SUBSET(I)%NCHILD
            CALL WRITE_I_C(SUBSET(I)%CHILD(J)-1,1)
          ENDDO
C #PARTS FILLES
          N1=0
          N2=0
          N3=0
          DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K).GT.0.AND.MATER(K).LE.M01)THEN
               N1=N1+1
            ELSEIF(MATER(K).GT.M1.AND.MATER(K).LE.M2)THEN
               N2=N2+1
            ELSEIF(MATER(K).GT.M2)THEN
               N3=N3+1
            ENDIF
          ENDDO
C PARTS FILLES 2D
          CALL WRITE_I_C(N1,1)
          DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K).GT.0.AND.MATER(K).LE.M01)
     .         CALL WRITE_I_C(MATER(K)-1,1)
          ENDDO
C PARTS FILLES 3D
          CALL WRITE_I_C(N2,1)
          DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K).GT.M1.AND.MATER(K).LE.M2)
     .          CALL WRITE_I_C(MATER(K)-M1-1,1)
          ENDDO
C PARTS FILLES 1D
          CALL WRITE_I_C(N3,1)
          DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K).GT.M2)CALL WRITE_I_C(MATER(K)-M2-1,1)
          ENDDO
        ENDDO
C-----------------
C SUBSET RBODIES
C-----------------
          IF (NRBODY.GT.0) THEN
           WRITE(STR,'(I8,A14)')MXSUBS+1,':RBODIES MODEL'
           DO J=1,22
            CTEXT(J)=ICHAR(STR(J:J))
           ENDDO
           CTEXT(23)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
           CALL WRITE_I_C(NSUBS
     .          +MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .          +MIN(1,NSURG+NSMAD)+NFVSUBS-1,1)
C #SUBSETS FILS
           CALL WRITE_I_C(0,1)
C SUBSETS FILS
C #PARTS FILLES
           N1=0
           N2=0
           N3=NRBODY
C PARTS FILLES 2D
           CALL WRITE_I_C(N1,1)
C PARTS FILLES 3D
           CALL WRITE_I_C(N2,1)
C PARTS FILLES 1D
           CALL WRITE_I_C(N3,1)
           DO J=NRBODY,1,-1
              CALL WRITE_I_C(M3-J-M2,1)
           ENDDO
          END IF
C-----------------
C SUBSET SECTIONS
C-----------------
          IF (NSECT.GT.0) THEN
           WRITE(STR,'(I8,A15)')MXSUBS+MIN(1,NRBODY)+1,':SECTIONS MODEL'
           DO J=1,23
            CTEXT(J)=ICHAR(STR(J:J))
           ENDDO
           CTEXT(24)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
           CALL WRITE_I_C(NSUBS
     .          +MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .          +MIN(1,NSURG+NSMAD)+NFVSUBS-1,1)
C #SUBSETS FILS
           CALL WRITE_I_C(0,1)
C SUBSETS FILS
C #PARTS FILLES
           N1=NSECT
           N2=0
           N3=0
C PARTS FILLES 2D
           CALL WRITE_I_C(N1,1)
           DO J=NSECT,1,-1
              CALL WRITE_I_C(M1-NSURG-NSMAD-NRWALL-J,1)
           ENDDO
C PARTS FILLES 3D
           CALL WRITE_I_C(N2,1)
C PARTS FILLES 1D
           CALL WRITE_I_C(N3,1)
          END IF
C-----------------
C SUBSET RWALLS
C-----------------
          IF (NRWALL.GT.0) THEN
           WRITE(STR,'(I8,A13)')MXSUBS
     .              +MIN(1,NSECT)+MIN(1,NRBODY)+1,':RWALLS MODEL'
           DO J=1,21
            CTEXT(J)=ICHAR(STR(J:J))
           ENDDO
           CTEXT(22)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
           CALL WRITE_I_C(NSUBS
     .          +MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .          +MIN(1,NSURG+NSMAD)+NFVSUBS-1,1)
C #SUBSETS FILS
           CALL WRITE_I_C(0,1)
C SUBSETS FILS
C #PARTS FILLES
           N1=NRWALL
           N2=0
           N3=0
C PARTS FILLES 2D
           CALL WRITE_I_C(N1,1)
           DO J=NRWALL,1,-1
              CALL WRITE_I_C(M1-NSURG-NSMAD-J,1)
           ENDDO
C PARTS FILLES 3D
           CALL WRITE_I_C(N2,1)
C PARTS FILLES 1D
           CALL WRITE_I_C(N3,1)
          END IF
C-----------------
C SUBSET SURFACES
C-----------------
          IF (NSURG+NSMAD.GT.0) THEN
           WRITE(STR,'(I8,A15)')MXSUBS
     .              +MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)+1,
     .              ':SURFACES MODEL'
           DO J=1,23
            CTEXT(J)=ICHAR(STR(J:J))
           ENDDO
           CTEXT(24)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
           CALL WRITE_I_C(NSUBS
     .                    +MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .                    +MIN(1,NSURG+NSMAD)+NFVSUBS-1,1)
C #SUBSETS FILS
           CALL WRITE_I_C(0,1)
C SUBSETS FILS
C #PARTS FILLES
           N1=NSURG+NSMAD
           N2=0
           N3=0
C PARTS FILLES 2D
           CALL WRITE_I_C(N1,1)
           DO J=NSURG+NSMAD,1,-1
              CALL WRITE_I_C(M1-J,1)
           ENDDO
C PARTS FILLES 3D
           CALL WRITE_I_C(N2,1)
C PARTS FILLES 1D
           CALL WRITE_I_C(N3,1)
          END IF
C-----------------
C SUBSETS FVMBAG
C-----------------
          IF (NFVSUBS.GT.0) THEN
             II=NSUBS
     .         +MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .         +MIN(1,NSURG+NSMAD)-1
             OFFPART=NBPART2D
             DO I=1,NFVBAG
                IF (FVDATA(I)%NPOLH_ANIM.GT.0) THEN
                   II=II+1
                   WRITE(STR,'(I8,A11,I8)')
     .                 II,':FVMBAG ID ',FVDATA(I)%ID
                   DO J=1,27
                    CTEXT(J)=ICHAR(STR(J:J))
                   ENDDO
                   CTEXT(28)=0
                   CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT == GLOBAL
                   CALL WRITE_I_C(NSUBS
     .                    +MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .                    +MIN(1,NSURG+NSMAD)+NFVSUBS-1,1)
C #SUBSETS FILS
                   CALL WRITE_I_C(0,1)
C PARTS FILLES 2D
                   CALL WRITE_I_C(FVDATA(I)%NPOLH_ANIM,1)
                   DO J=1,FVDATA(I)%NPOLH_ANIM
                      CALL WRITE_I_C(OFFPART+J-1,1)
                   ENDDO
                   OFFPART=OFFPART+FVDATA(I)%NPOLH_ANIM
C PARTS FILLES 3D
                   CALL WRITE_I_C(0,1)
C PARTS FILLES 1D
                   CALL WRITE_I_C(0,1)
                ENDIF
             ENDDO
          ENDIF
C--------------
C GLOBAL MODEL
C--------------
          WRITE(STR,'(I8,A1)') SUBSET(NSUBS)%ID,':'
          DO J=1,9
            CTEXT(J)=ICHAR(STR(J:J))
          ENDDO
          IB = 9
          TITL = SUBSET(NSUBS)%TITLE
          DO J=1,LTITL
            IF(TITL(J:J).NE.' ') IB = J+9
            CTEXT(J+9)=ICHAR(TITL(J:J))
          ENDDO
          CTEXT(IB+1)=0
          CALL WRITE_C_C(CTEXT,10+LTITL)
C SUBSET PARENT
          CALL WRITE_I_C(SUBSET(NSUBS)%PARENT-1,1)
C #SUBSETS FILS
          CALL WRITE_I_C(SUBSET(NSUBS)%NCHILD
     .         +MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .         +MIN(1,NSURG+NSMAD)+NFVSUBS,1)
C SUBSETS FILS
          DO J=1,SUBSET(NSUBS)%NCHILD
            CALL WRITE_I_C(SUBSET(NSUBS)%CHILD(J)-1,1)
          ENDDO
          IF (NRBODY.GT.0)
     .      CALL WRITE_I_C(NSUBS-1,1)
          IF (NSECT.GT.0)
     .      CALL WRITE_I_C(NSUBS+MIN(1,NRBODY)-1,1)
          IF (NRWALL.GT.0)
     .      CALL WRITE_I_C(NSUBS+MIN(1,NSECT)+MIN(1,NRBODY)-1,1)
          IF (NSURG+NSMAD.GT.0)
     .      CALL WRITE_I_C(NSUBS+MIN(1,NSECT)+MIN(1,NRBODY)
     .                          +MIN(1,NRWALL)-1,1)
          IF (NFVSUBS.GT.0) THEN
             II=NSUBS+MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .                   +MIN(1,NSURG+NSMAD)
             DO I=1,NFVBAG
                CALL WRITE_I_C(II-1,1)
                II=II+1
             ENDDO
          ENDIF
C #PARTS FILLES
          N1=0
          N2=0
          N3=0
          DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K).GT.0.AND.MATER(K).LE.M01)THEN
               N1=N1+1
            ELSEIF(MATER(K).GT.M1.AND.MATER(K).LE.M2)THEN
               N2=N2+1
            ELSEIF(MATER(K).GT.M2)THEN
               N3=N3+1
            ENDIF
          ENDDO
C CUTS DANS LE SUBSET GLOBAL
          N1=N1+NCUTS
C PARTS FILLES 2D
          CALL WRITE_I_C(N1,1)
          DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K).GT.0.AND.MATER(K).LE.M01)
     .         CALL WRITE_I_C(MATER(K)-1,1)
          ENDDO
C CUTS DANS LE SUBSET GLOBAL
          DO J=1,NCUTS
              CALL WRITE_I_C(M1-NRWALL-NSECT-NSURG-NSMAD-J,1)
          ENDDO
C PARTS FILLES 3D
          CALL WRITE_I_C(N2,1)
          DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K).GT.M1.AND.MATER(K).LE.M2)
     .          CALL WRITE_I_C(MATER(K)-M1-1,1)
          ENDDO
C PARTS FILLES 1D
          CALL WRITE_I_C(N3,1)
          DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K).GT.M2)CALL WRITE_I_C(MATER(K)-M2-1,1)
          ENDDO
       ENDIF
C-----------------------------------------------
C   WRITE CONTROL
C-----------------------------------------------
      CALL WRITE_I_C(NUMMAT+1,1)
      CALL WRITE_I_C(NUMGEO+1,1)
C-----------------------------------------------
C   MAT HEAD
C-----------------------------------------------
      CALL ANI_TXT50('Dummy Material',14)
      DO I=1,NUMMAT
         WRITE(STR,'(I8,A1)') IPM(1,I),':'
         DO J=1,9
           CTEXT(J)=ICHAR(STR(J:J))
         ENDDO
         IB = 9
         CALL FRETITL2(TITL,IPM(NPROPMI-LTITR+1,I),LTITL)
         DO J=1,LTITL
           IF(TITL(J:J).NE.' ') IB = J+9
           CTEXT(J+9)=ICHAR(TITL(J:J))
         ENDDO
         CTEXT(IB+1)=0
         CALL WRITE_C_C(CTEXT,10+LTITL)
      ENDDO
C-----------------------------------------------
C   MAT TYPE
C-----------------------------------------------
      CALL WRITE_I_C(0,1)
      DO I=1,NUMMAT
        CALL WRITE_I_C(NINT(PM(19,I)),1)
      ENDDO
C-----------------------------------------------
C   PROP HEAD
C-----------------------------------------------
      CALL ANI_TXT50('Dummy Property',14)
      DO I=1,NUMGEO
         WRITE(STR,'(I8,A1)') IGEO(1,I),':'
         DO J=1,9
           CTEXT(J)=ICHAR(STR(J:J))
         ENDDO
         IB = 9
         CALL FRETITL2(TITL,IGEO(NPROPGI-LTITR+1,I),LTITL)
         DO J=1,LTITL
           IF(TITL(J:J).NE.' ') IB = J+9
           CTEXT(J+9)=ICHAR(TITL(J:J))
         ENDDO
         CTEXT(IB+1)=0
         CALL WRITE_C_C(CTEXT,10+LTITL)
      ENDDO
C-----------------------------------------------
C   PROP TYPE
C-----------------------------------------------
      CALL WRITE_I_C(0,1)
      DO I=1,NUMGEO
        CALL WRITE_I_C(NINT(GEO(12,I)),1)
      ENDDO
C=======================================================================
C
C     Additional particles description, case of /ANIM/VERS/44 only.
C
C=======================================================================
      IF(ISPH3D.EQ.1.OR.NUMSPH_T+MAXPJET.EQ.0) GOTO 700
C-----------------------------------------------
C   prepare sorties SUBSET : PART FILLES meshless
C-----------------------------------------------
      DO I=1,NPART
        MATER(I)=-MATER(I)
      ENDDO
C-----------------------------------------------
C   PART COUNT
C-----------------------------------------------
      DO I=1,NUMSPH+MAXPJET
        MATER(IPARTSP(I))=4
        EL2FA(I)=0
      ENDDO
C
      NBPART = 0
      DO I=1,NPART
        IF(MATER(I).EQ.4)NBPART = NBPART + 1
      ENDDO
C-----------------------------------------------
C   WRITE CONTROL
C-----------------------------------------------
      CALL WRITE_I_C(NUMSPH+MAXPJET,1)
      CALL WRITE_I_C(NBPART,1)
      CALL WRITE_I_C(NSE_ANI+1,1)
      CALL WRITE_I_C(NST_ANI,1)
C-----------------------------------------------
C   PART SORT
C-----------------------------------------------
      CALL PARSOR0(IAD ,IPARG   ,MATER   ,EL2FA   ,
     3         KXSP    ,IPARTSP )
C-----------------------------------------------
C   OFF
C-----------------------------------------------
      NNN = NUMSPH+MAXPJET
      CALL ANIOFF0(ELBUF_TAB ,IPARG   ,WAFT   ,EL2FA   ,NNN   ,
     1             SWAFT, SPH2SOL)
C-----------------------------------------------
C   PART ADD
C-----------------------------------------------
      CALL WRITE_I_C(IAD,NBPART)
C-----------------------------------------------
C   PART HEAD
C-----------------------------------------------
      DO I=1,NPART
       IF(MATER(I).EQ.4)THEN
         WRITE(STR,'(I8,A1)')IPART(4,I),':'
         DO J=1,9
           CTEXT(J)=ICHAR(STR(J:J))
         ENDDO
         IB = 9
         CALL FRETITL2(TITL,IPART(LIPART1-LTITR+1,I),LTITL)
         DO J=1,LTITL
           IF(TITL(J:J).NE.' ') IB = J+9
           CTEXT(J+9)=ICHAR(TITL(J:J))
         ENDDO
         CTEXT(IB+1)=0
         CALL WRITE_C_C(CTEXT,10+LTITL)
       ENDIF
      ENDDO
C-----------------------------------------------
C   ELEMENT MASS FOR MAS & FUNC
C-----------------------------------------------
      IF(ANIM_M.EQ.1.OR.ANIM_SE(3).EQ.1.OR.
     .   ANIM_SE(25).EQ.1)THEN
         CALL DMASANI0(ELBUF_TAB   ,IPARG   ,
     2           MAS     ,PM      ,EL2FA   ,IPART   ,IPARTSP )
      ENDIF
C-----------------------------------------------
C   FUNC TEXT
C-----------------------------------------------
      CTEXT(81)=0
      CALL ANI_TXT('Diameter',8)
      IF (DSANIM.EQ.1) THEN
         DO I=1,NLEVEL
            CALL ANI_TXT(CTITR(I),33)
         ENDDO
      ELSEIF (DECANI.EQ.1) THEN
          CALL ANI_TXT(CTITR(1),25)
      ENDIF
C-----------------------------------------------
C   ELEMENT FUNC (SPH)
C-----------------------------------------------
      NNN = NUMSPH+MAXPJET
      DO I = 0,MX_ANI
        IFUNC = I
        IF(IFUNC.EQ.0.OR.(IFUNC.GT.0.AND.ANIM_SE(I).EQ.1)) THEN
          CALL DFUNC0(ELBUF_TAB ,WAFT   ,IFUNC   ,IPARG   ,PM      ,
     .               EL2FA      ,NNN    ,SPBUF   ,IPART   ,IPARTSP )
        ENDIF
      ENDDO
      IF (DECANI.EQ.1) THEN
         DO I=1,NUMSPH
            FUNC(I)=ZERO
         ENDDO
C Particules SPH
         OFF=1
         CALL DELSUB(1,     CEPSP, 1, OFF, NUMSPH,
     .               EL2FA, FUNC )
C
         DO I=1,NUMSPH
            R4=FUNC(I)
            CALL WRITE_R_C(R4,1)
         ENDDO
         R4=ZERO
         DO I=1,MAXPJET
            CALL WRITE_R_C(R4,1)
         ENDDO
      ENDIF
C-----------------------------------------------
C   3D TENSOR TEXT
C-----------------------------------------------
C-----------------------------------------------
C   3D TENSOR (SPH)
C-----------------------------------------------
      DO I = 1,MX_ANI
       IFUNC = I
       IF(ANIM_ST(I).EQ.1)THEN
           CALL TENSOR0(ELBUF_TAB ,IPARG   ,IFUNC   ,PM      ,EL2FA   ,
     2                  NNN     ,WAFT    ,IPART   ,IPARTSP )
       ENDIF
      ENDDO
C-----------------------------------------------
C   ELEMENT MASS (SPH)
C-----------------------------------------------
      IF(ANIM_M.EQ.1)THEN
       DO I=1,NNN
          R4 = MAS(I)
          CALL WRITE_R_C(R4,1)
       ENDDO
      ENDIF
C-----------------------------------------------
C   NUMBERING (SPH)
C-----------------------------------------------
       CALL DELNUMB0(IPARG,EL2FA,NNN ,WAFT,KXSP )
C-----------------------------------------------
C   HIERARCHIE
C-----------------------------------------------
       DO I=1,NPART
        IF(MATER(I).EQ.4)THEN
          IF (IPART(3,I).LT.NSUBS) THEN
           CALL WRITE_I_C(IPART(3,I)-1,1)
          ELSE
           CALL WRITE_I_C(NSUBS
     .                    +MIN(1,NSECT)+MIN(1,NRBODY)+MIN(1,NRWALL)
     .                    +MIN(1,NSURG+NSMAD)-1,1)
          END IF
        END IF
       ENDDO
       DO I=1,NPART
         IF(MATER(I).EQ.4)CALL WRITE_I_C(IPART(1,I),1)
       ENDDO
       DO I=1,NPART
         IF(MATER(I).EQ.4)CALL WRITE_I_C(IPART(2,I),1)
       ENDDO
C-----------------------------------------------
C   SUBSET : PART FILLES MESHLESS
C-----------------------------------------------
        J=M3
        DO I=1,NPART
          IF(MATER(I).EQ.4)THEN
            J=J+1
            MATER(I)=J
          ENDIF
        ENDDO
        M4=J
        IF (NSUBS.EQ.1) THEN
C #PARTS FILLES meshless
          N0=0
          DO K=1,NPART
            IF(MATER(K).GT.M3)THEN
               N0=N0+1
            ENDIF
          ENDDO
C PARTS FILLES meshless
          CALL WRITE_I_C(N0,1)
          DO K=1,NPART
            IF(MATER(K).GT.M3)
     .         CALL WRITE_I_C(MATER(K)-M3-1,1)
          ENDDO
        ELSE
C----------------
C +SIEURS SUBSET
C----------------
        DO I=1,NSUBS-1
C #PARTS FILLES meshless
          N0=0
          DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K).GT.M3)THEN
               N0=N0+1
            ENDIF
          ENDDO
C PARTS FILLES meshless
          CALL WRITE_I_C(N0,1)
          DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K).GT.M3)
     .         CALL WRITE_I_C(MATER(K)-M3-1,1)
          ENDDO
        ENDDO
C--------------
C GLOBAL MODEL
C--------------
C #PARTS FILLES meshless
          N0=0
          DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K).GT.M3)THEN
               N0=N0+1
            ENDIF
          ENDDO
C PARTS FILLES meshless
          CALL WRITE_I_C(N0,1)
          DO J=1,SUBSET(I)%NPART
            K = SUBSET(I)%PART(J)
            IF(MATER(K).GT.M3)
     .         CALL WRITE_I_C(MATER(K)-M3-1,1)
          ENDDO
        ENDIF
C--------------
        DO I=1,NPART
          IF(MATER(I).LT.0)MATER(I)=-MATER(I)
        ENDDO
C=======================================================================
 700  CONTINUE
C=======================================================================
      CALL CLOSE_C
C-----------------------------------------------
      WRITE (IOUT,1000)  FILNAM(1:FILEN)
      WRITE (ISTDO,1100) FILNAM(1:FILEN)
 1000 FORMAT (/'     ANIMATION FILE:',1X,A,' WRITTEN'/
     .         '     ---------------')
 1100 FORMAT (' .. ANIMATION FILE:',1X,A,' WRITTEN')
C
      RETURN
      END 
Chd|====================================================================
Chd|  XYZ16                         source/output/anim/genani1.F  
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        WRITE_R_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE XYZ16(IXS,IXS10,IXS20,IXS16,X)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .  X(3,*)
      INTEGER IXS(NIXS,*),
     .        IXS10(6,*) ,IXS16(8,*)  ,IXS20(12,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .  XX,YY,ZZ
      REAL R4,R4NP(6*NUMELS16)
      INTEGER I, J, K,N1,N2,N3,N4,N5,N6,N7,N8,
     . JJ,BUF
C-----------------------------------------------
      JJ = 0
      DO J=1,NUMELS16
        I = J+NUMELS8+NUMELS10+NUMELS20
        N1 = IXS(2,I)
        N2 = IXS(3,I)
        N3 = IXS(4,I)
        N4 = IXS(5,I)
        N5 = IXS16(1,J)
        N6 = IXS16(2,J)
        N7 = IXS16(3,J)
        N8 = IXS16(4,J)
        IF(N5.EQ.0)N5=N1
        IF(N6.EQ.0)N6=N2
        IF(N7.EQ.0)N7=N3
        IF(N8.EQ.0)N8=N4
        XX = HALF *(X(1,N5)+X(1,N6)+X(1,N7)+X(1,N8))
     .      -FOURTH*(X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4))
        YY = HALF *(X(2,N5)+X(2,N6)+X(2,N7)+X(2,N8))
     .      -FOURTH*(X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4))
        ZZ = HALF *(X(3,N5)+X(3,N6)+X(3,N7)+X(3,N8))
     .      -FOURTH*(X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4))
        R4 = XX
        CALL WRITE_R_C(R4,1)
        R4 = YY
        CALL WRITE_R_C(R4,1)
        R4 = ZZ
        CALL WRITE_R_C(R4,1)
        N1 = IXS(6,I)
        N2 = IXS(7,I)
        N3 = IXS(8,I)
        N4 = IXS(9,I)
        N5 = IXS16(5,J)
        N6 = IXS16(6,J)
        N7 = IXS16(7,J)
        N8 = IXS16(8,J)
        IF(N5.EQ.0)N5=N1
        IF(N6.EQ.0)N6=N2
        IF(N7.EQ.0)N7=N3
        IF(N8.EQ.0)N8=N4
        XX = HALF *(X(1,N5)+X(1,N6)+X(1,N7)+X(1,N8))
     .      -FOURTH*(X(1,N1)+X(1,N2)+X(1,N3)+X(1,N4))
        YY = HALF *(X(2,N5)+X(2,N6)+X(2,N7)+X(2,N8))
     .      -FOURTH*(X(2,N1)+X(2,N2)+X(2,N3)+X(2,N4))
        ZZ = HALF *(X(3,N5)+X(3,N6)+X(3,N7)+X(3,N8))
     .      -FOURTH*(X(3,N1)+X(3,N2)+X(3,N3)+X(3,N4))
        R4 = XX
        CALL WRITE_R_C(R4,1)
        R4 = YY
        CALL WRITE_R_C(R4,1)
        R4 = ZZ
        CALL WRITE_R_C(R4,1)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  XYZNOR16                      source/output/anim/genani1.F  
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        WRITE_S_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE XYZNOR16(IXS,IXS10,IXS20,IXS16,X)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .  X(3,*)
      INTEGER IXS(NIXS,*),
     .        IXS10(6,*) ,IXS16(8,*)  ,IXS20(12,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .  XX,YY,ZZ
      REAL R4
      INTEGER I, J, K,N1,N2,N3,N4,N5,N6,N7,N8,I3000,SIZ
C-----------------------------------------------
      I3000 = 3000
      SIZ = NUMELS16
      DO J=1,SIZ
         CALL WRITE_S_C(I3000,1)
         CALL WRITE_S_C(I3000,1)
         CALL WRITE_S_C(I3000,1)
         CALL WRITE_S_C(I3000,1)
         CALL WRITE_S_C(I3000,1)
         CALL WRITE_S_C(I3000,1)
      ENDDO
C
      RETURN
      END
