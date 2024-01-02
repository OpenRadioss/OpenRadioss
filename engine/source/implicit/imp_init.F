Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  IMP_INIT                      source/implicit/imp_init.F    
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|        DYNA_INI                      source/implicit/imp_dyna.F    
Chd|        KTBUF_INI                     source/implicit/imp_init.F    
Chd|        ZEROR                         source/system/zero.F          
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        IMP_ASPC                      share/modules/impbufdef_mod.F 
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        IMP_KBCS                      share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE IMP_INIT(V,VR,IPARG,IPM,IGEO,ELBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_ASPC
      USE IMP_KBCS
      USE IMP_INTM
      USE ELBUFDEF_MOD            
C----6------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "impl2_c.inc"
#include      "scr05_c.inc" 
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,NGROUP),IPM(NPROPMI,*),IGEO(*)
      my_real
     .    V(3,*),VR(3,*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K0,IDEFKZ,IER1,IER2,IFSTRUN
      my_real
     .    TREST,CS1(2)
      REAL FLMIN
C----------------------------------------------------------
C   INIT IMPLICIT 
C----------------------------------------------------------
      IMCONV=1
      INCONV =1
C ADD FOR SOLVER AUTO SELECTION
      IFSTRUN = 1

      IF(IMPL_S==1.OR.NEIG>0)THEN
       IDEFKZ=0
       IF (NEIG>0.AND.KZ_TOL==ZERO) IDEFKZ=1
       NNSIZ=1024
       IKPAT=0
       TREST=TSTOP-TT
       IF (D_TOL==ZERO.AND.N_PAT>1) D_TOL=-ONE
       IF (D_TOL==ZERO) D_TOL=THREE*EM5
       IF (IPREC==5)IKPAT=1
       IF (IPREC==6) THEN
        IPREC=5
        IKPAT=2
       ENDIF
       IF (SK_INT==ZERO) SK_INT=ONEP01
C--------direct-----
       IF (DT_IMP==ZERO.OR.DT_IMP>=TREST) DT_IMP=TREST
       IF (ILINE==1) THEN
        DT_IMP=TREST
        IF (KZ_TOL==ZERO) KZ_TOL=TWO*EM4
       ELSE
C--------non-linear-----
        IF (KZ_TOL==ZERO) KZ_TOL=FIVE*EM4
       ENDIF
       IF (ISPRB==1.AND.IDYNA==0) THEN
         CALL ZEROR(V,NUMNOD)
         IF (IRODDL/=0) CALL ZEROR(VR,NUMNOD)    
       ENDIF
       IF (NEXP==0.OR.IDYNA==1) NEXP=1
       IF (INTP_C>=0)THEN
        IF (IMACH==3.AND.NSPMD>1)THEN
         IF (ISOLV>=2) INTP_C = 1
        ELSE
         INTP_C = 0
        ENDIF
       ENDIF
      ENDIF
      IF (NEIG>0) THEN
         IF (IDEFKZ==1) KZ_TOL=TEN
C         IKG=0
         IKPAT=0
        IF (IMACH==3) IKPAT=1
                 IF (IAUTSPC==1) IAUTSPC=0
      ENDIF

      IF (IDYNA>0)CALL DYNA_INI(1 ,NUMNOD ,HHT_A ,NEWM_A,NEWM_B,V,VR)
C
       NSPCL = 0
       IF (B_MCORE<0) THEN
        LMEMN=1000000
       ELSE
        LMEMN = 2*LMEMV/3
       ENDIF
C       
C      IF(INTP_C<0) IRREF = MAX(2,IRREF)
C      IF(IRREF==1.AND.IDYNA>0) IRREF = 4
C      
      NDDL_SI = 0
      NDDL_SL = 0
      NZ_SI = 0
      NZ_SL = 0
C 
      IHELAS=0     
      IF (IKT > 0 .OR.ISPRB==1) CALL KTBUF_INI(ELBUF_TAB,IPARG ,IPM ,IGEO)
C
      IDTFIX=0     
      IF (NITOL==123) THEN
       N_TOL=MAX(N_TOLE,N_TOLF,N_TOLU)
      ELSEIF (NITOL==12) THEN
       N_TOL=MAX(N_TOLE,N_TOLF)
      ELSEIF (NITOL==23) THEN
       N_TOL=MAX(N_TOLF,N_TOLU)
      ELSEIF (NITOL==13) THEN
       N_TOL=MAX(N_TOLE,N_TOLU)
      ENDIF
C
       RETURN
       END
C-----------------------------------------------
Chd|====================================================================
Chd|  KTBUF_INI                     source/implicit/imp_init.F    
Chd|-- called by -----------
Chd|        IMP_INIT                      source/implicit/imp_init.F    
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        GRPREORDER                    source/implicit/imp_glob_k.F  
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        IMP_KTAN                      share/modules/impbufdef_mod.F 
Chd|        IMP_KTAN_DEF                  share/modules/impbufdef_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE KTBUF_INI(ELBUF_TAB, IPARG ,IPM ,IGEO)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_KTAN
      USE MESSAGE_MOD
      USE IMP_KTAN_DEF
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "scr19_c.inc"
#include      "units_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,NGROUP),IPM(NPROPMI,*),IGEO(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IFA,IR,IS,IT,NG,BUFLEN,ERR,
     .   NEL,NPT,NPG,MLW,ISORTH,ISRATE,ISROT,IREP,ISIGV,IGTYP,
     .   ISTRA,IFAIL,NFAIL,IEOS,IXFEM,NLAY,NPTR,NPTS,NPTT,NVAR,
     .   NFT,NFT0,KFTS,ITY,JALE,OFF,ISS,ICPRE,NSG,ICNOD,ISNOD,JEUL,
     .   JHBE,JIVF,JPOR,IPLA,L_ETFAC,L_SIGE,L_A_KT,L_SUBKT,LENF,
     .   IGROUC(NGROUP),IG
      TYPE(KTBUF_STRUCT_) , POINTER :: KTBUF
      TYPE(L_KTBUFEP_)    , POINTER :: LBUF
      TYPE(MLAW_TAG_)     , POINTER :: MTAG
      TYPE(ELBUF_STRUCT_) , DIMENSION(NGROUP) :: ELBUF_TAB
C=======================================================================
c      Element KT Buffer ini Routine (Solid,Shell)
c------
c      1) allocation of KTBUF_STR
C=======================================================================
      ALLOCATE (KTBUF_STR(NGROUP), STAT=ERR)
c-------------------------------------------------
       LENF =0
      CALL  GRPREORDER(IPARG, IGROUC)
      DO IG = 1,NGROUP
        NG = IGROUC(IG)
        MLW     = IPARG(1,NG)     ! type de loi mat
        NEL     = IPARG(2,NG)
        NFT     = IPARG(3,NG)
        NPT     = IPARG(6,NG)
        OFF     = IPARG(8,NG)
        ITY     = IPARG(5,NG)
        JHBE    = IPARG(23,NG)
        NPG     = IPARG(48,NG)
        NLAY    = 1
        NPTR    = 1
        NPTS    = 1
        NPTT    = 1
c
        IF (MLW == 0 .OR. MLW == 13 .OR. OFF==1) CYCLE
c-------------------------------------------------
        L_ETFAC   = 0
        L_SIGE   = 0
        L_A_KT  = 0
        L_SUBKT  = 0
c----------------------------------------------------
       IF (ITY == 1) THEN   ! solides

        NLAY    = ELBUF_TAB(NG)%NLAY   
        NPTR    = ELBUF_TAB(NG)%NPTR   
        NPTS    = ELBUF_TAB(NG)%NPTS   
        NPTT    = ELBUF_TAB(NG)%NPTT   
c       Compatibility :S8(HA8,HC8),S4,S10,S20,SC6
        ISNOD   = IPARG(28,NG)
        IPLA    = IPARG(29,NG)
        ISROT   = IPARG(41,NG)
C  create a int table law type-----
        IF (ISNOD==8 .AND. JHBE/=14 .AND. JHBE/=17) THEN
          WRITE(IOUT,*)' **WARNING : ONLY ISOLID=14,17 ARE AVAILABLE',
     1             ' WITH CONSISTING TANGENT MATRIX, OPTION IGNORED. '
          WRITE(ISTDO,*)' **WARNING : ONLY ISOLID=14,17 ARE AVAILABLE',
     1              ' WITH CONSISTING TANGENT MATRIX, OPTION IGNORED. '
C--------add warning for HEPH-HSEPH-S8--
        ELSE
         IF (MLW==42.OR.MLW==62.OR.MLW==69.OR.MLW==82) THEN
          L_ETFAC=1
          IF (IHELAS ==0 ) IHELAS=1
         ELSEIF (MLW==2.OR.MLW==36) THEN
          L_ETFAC=1
          L_SIGE   = 6
          L_A_KT  = 1
         ENDIF
        END IF
C---- shell 3n,4n
       ELSEIF(ITY==3.OR.ITY==7) THEN
c       Compatibility :QEPH,QBAT,C3
        IF (JHBE==11) THEN
C-------for the case when DKT18 was available for implicit
          NPTR = 1
          NPTS = NPG
          NPTT = NPT
        ELSE
          NPTR  = 1
          NPTS  = 1
          NPTT  = NPT
        ENDIF
        IF (NPTT == 0 .AND. MLW /= 1) THEN
             CALL ANCMSG(MSGID=227,ANMODE=ANINFO,
     .            C1='FOR IMPLICIT NONLINEAR')
             CALL ARRET(2)
        ENDIF
C  create a int table law type-----
         IF (MLW==78) THEN
          L_ETFAC=1
         ELSEIF (MLW==2.OR.MLW==36) THEN
C------resulting model is not available
          L_ETFAC=1
          L_SIGE   = 5  ! (4 : HK, 5: HH)
          L_A_KT  = 1
         END IF !(MLW==2.OR.MLW==36) THEN
       ENDIF   ! el type
c -------------------------------------------------
c       allocation des sub-structures de l'element buffer
c-------------------------------------------------
        IF (ITY /=1 .AND.ITY /=3 .AND.ITY /=7 ) CYCLE
        ALLOCATE (KTBUF_STR(NG)%MLAW_TAG(0:MAXLAW) ,STAT=ERR)
        ALLOCATE (KTBUF_STR(NG)%ETFAC(NEL*L_ETFAC) ,STAT=ERR)
        ALLOCATE (KTBUF_STR(NG)%KTBUFEP(NPTR,NPTS,NPTT) ,STAT=ERR)

        IF (L_ETFAC>0) KTBUF_STR(NG)%ETFAC = ONE
        MTAG => KTBUF_STR(NG)%MLAW_TAG(MLW)
        MTAG%L_ETFAC=L_ETFAC
        MTAG%L_A_KT =L_A_KT
        MTAG%L_SIGE =L_SIGE
        MTAG%L_SUBKT=L_SUBKT
C        LENF = LENF + 4
c-------------------------------------------------
c       Local variables per integration point
c-------------------------------------------------
        DO IR = 1,NPTR
          DO IS = 1,NPTS
            DO IT = 1,NPTT
c              GBUF => KTBUF_STR(NG)%KTBUFG(IR,IS,IT)
c              ALLOCATE(GBUF%ETFAC(NEL*L_ETFAC), STAT=ERR)
c              GBUF%ETFAC = ONE
              LBUF => KTBUF_STR(NG)%KTBUFEP(IR,IS,IT)
              ALLOCATE(LBUF%A_KT(NEL*L_A_KT), STAT=ERR)
              LBUF%A_KT = ZERO
              ALLOCATE(LBUF%SIGE(NEL*L_SIGE), STAT=ERR)
              LBUF%SIGE = ZERO
             LENF = LENF + NEL*(L_ETFAC+L_A_KT+L_SIGE)
            ENDDO
          ENDDO
        ENDDO
c-------------------------------------------------
        IF (ERR /= 0) THEN
             CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1='FOR IMPLICIT NONLINEAR')
             CALL ARRET(2)
        ENDIF
      ENDDO
C      IF (LENF==0 .AND. IKT > 0 ) IKT=0
C-----
      RETURN
      END SUBROUTINE KTBUF_INI
Chd|====================================================================
Chd|  IKTMAT_INI                    source/implicit/imp_init.F    
Chd|-- called by -----------
Chd|        GET_ETFAC_S                   source/elements/solid/solide8z/get_etfac_s.F
Chd|        NUL_ETFAC_S                   source/elements/solid/solide8z/nul_etfac_s.F
Chd|        PUT_ETFAC                     source/elements/solid/solide8z/put_etfac.F
Chd|-- calls ---------------
Chd|        IMP_KTAN                      share/modules/impbufdef_mod.F 
Chd|        IMP_KTAN_DEF                  share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE IKTMAT_INI(MTN,IKTMAT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_KTAN
      USE IMP_KTAN_DEF
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MTN,IKTMAT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       IF (IKT == 0 .AND. MTN /=78 ) THEN
        IKTMAT = 0
       ELSE
        IKTMAT = IHELAS+KTBUF_STR(NG_IMP)%MLAW_TAG(MTN)%L_ETFAC
       END IF
C       
      RETURN
      END SUBROUTINE IKTMAT_INI
Chd|====================================================================
Chd|  ETFAC_INI                     source/implicit/imp_init.F    
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        GRPREORDER                    source/implicit/imp_glob_k.F  
Chd|        IMP_KTAN                      share/modules/impbufdef_mod.F 
Chd|        IMP_KTAN_DEF                  share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE ETFAC_INI(IPARG )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_KTAN
      USE IMP_KTAN_DEF
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,NGROUP)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IFA,IR,IS,IT,NG,NEL,MLW,JHBE,L_ETFAC,ISNOD,
     .        ITY,OFF,IGROUC(NGROUP),IG
C=======================================================================
      IF (IKT == 0) RETURN
      CALL  GRPREORDER(IPARG, IGROUC)
      DO IG = 1,NGROUP
        NG = IGROUC(IG)
        MLW     = IPARG(1,NG)     ! type de loi mat
        NEL     = IPARG(2,NG)
        OFF     = IPARG(8,NG)
        ITY     = IPARG(5,NG)
        JHBE    = IPARG(23,NG)
        IF (MLW == 0 .OR. MLW == 13 .OR. OFF==1) CYCLE
c-------------------------------------------------
        L_ETFAC   = 0
c----------------------------------------------------
       IF (ITY == 1) THEN   ! solides
        ISNOD   = IPARG(28,NG)
        IF (ISNOD==8 .AND. JHBE/=14 .AND. JHBE/=17) CYCLE
        IF (MLW==42.OR.MLW==62.OR.MLW==69.OR.MLW==82) THEN
         L_ETFAC=1
        ELSEIF (MLW==2.OR.MLW==36) THEN
         L_ETFAC=1
        ENDIF   ! el type
C---- shell 3n,4n
       ELSEIF(ITY==3.OR.ITY==7) THEN
        IF (MLW==2.OR.MLW==36.OR.MLW==78) L_ETFAC=1
       ENDIF   ! el type
c -------------------------------------------------
c       allocation des sub-structures de l'element buffer
c-------------------------------------------------
        IF (ITY /=1 .AND.ITY /=3 .AND.ITY /=7 ) CYCLE
        IF (L_ETFAC>0) THEN
         DO I=1,NEL
          KTBUF_STR(NG)%ETFAC(I) = ONE
         END DO
        END IF
      END DO !IG = 1,NGROUP
C-----
      RETURN
      END SUBROUTINE ETFAC_INI

