Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||====================================================================
!||    sgrhead                  ../starter/source/elements/solid/solide/sgrhead.F
!||--- called by ------------------------------------------------------
!||    lectur                   ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    get_sort_key_solid       ../starter/source/elements/solid/get_sort_key_solid.F90
!||--- uses       -----------------------------------------------------
!||    cluster_mod              ../starter/share/modules1/cluster_mod.F
!||    get_sort_key_solid_mod   ../starter/source/elements/solid/get_sort_key_solid.F90
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    r2r_mod                  ../starter/share/modules1/r2r_mod.F
!||    reorder_mod              ../starter/share/modules1/reorder_mod.F
!||====================================================================
      SUBROUTINE SGRHEAD(
     1       IXS        ,PM            ,GEO       ,INUM    ,
     2       ITR1       ,EADD          ,INDEX     ,ITRI    ,IPARTS ,      
     3       ND         ,IGRSURF       ,IGRBRIC   ,ISOLNOD ,
     4       CEP        ,XEP           ,IXS10     ,IXS20   ,IXS16  ,
     5       IGEO       ,IPM           ,NOD2ELS   ,ISOLOFF ,
     6       TAGPRT_SMS ,SPH2SOL       ,SOL2SPH   ,MAT_PARAM,
     7       SOL2SPH_TYP,IFLAG_BPRELOAD,CLUSTERS  ,RNOISE  ,
     8       DAMP_RANGE_PART,TRIMAT)
C-----------------------------------------------
C            A R G U M E N T S
C-----------------------------------------------
C     IXS(11,NUMELS)       ARRAY: CONECS+PID+MID+NOS SOLIDS          E
C     PM(NPROPM,NUMMAT)    ARRAY: MATERIAL PARAMETERS (real)         E
C     IPM(NPROPMI,NUMMAT)  ARRAY: MATERIAL PARAMETERS (integer)      E
C     GEO(NPROPG,NUMGEO)   ARRAY: PROPERTY PARAMETERS (real)         E
C     IGEO(NPROPGI,NUMGEO) ARRAY: PROPERTY PARAMETERS (integer)      E
C     INUM(13,NUMELS)      ARRAY: WORKING                            E/S
C     ITR1(NSELS)          ARRAY: WORKING                            E/S
C     EADD(NUMELS)         ARRAY: IDAM INDEXES / checkboard            S
C     INDEX(NUMELS)        ARRAY: WORKING                            E/S
C     ITRI(8,NUMELS)       ARRAY: WORKING                            E/S
C     IPARTS(NUMELS)       ARRAY: PART                               E/S
C     CEP(NUMELS)          ARRAY: WORKING                            E/S
C     XEP(NUMELS)          ARRAY: WORKING                            E/S
C     NOD2ELS(8*NUMELS+6*NUMELS10+12*NUMELS20+16*NUMELS16)           E/S
C     ISOLOFF(NUMELS)      FLAG ELEM RBY ON/OFF                      E/S
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
      USE R2R_MOD
      USE REORDER_MOD
      USE GROUPDEF_MOD
      USE CLUSTER_MOD
      USE MATPARAM_DEF_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
      use element_mod , only : nixs
      use get_sort_key_solid_mod , only : get_sort_key_solid
C-----------------------------------------------
C   I M P L I C I T   T Y P E S
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C O M M O N   B L O C K S
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "sms_c.inc"
#include      "r2r_c.inc"
#include      "sphcom.inc"
#include      "boltpr_c.inc"
C-----------------------------------------------
C   D U M M Y   A R G U M E N T S
C-----------------------------------------------
      integer, parameter :: nb_key = 8
      INTEGER IXS(NIXS,NUMELS),INUM(16,*),IPARTS(*),
     .        EADD(*),ITR1(*),INDEX(*),ITRI(nb_key,*),
     .        ND, ISOLNOD(*), CEP(*),
     .        XEP(*),IXS10(6,*),IXS20(12,*),IXS16(8,*),
     .        NOD2ELS(*), ISOLOFF(*),
     .        TAGPRT_SMS(*), SPH2SOL(*), 
     .        SOL2SPH(2,*),SOL2SPH_TYP(*),IFLAG_BPRELOAD(*)
      INTEGER,INTENT(IN) :: IPM(NPROPMI,NUMMAT)
      INTEGER,INTENT(INOUT) :: IGEO(NPROPGI,NUMGEO)
      INTEGER, INTENT(IN)   :: DAMP_RANGE_PART(NPART)   ! < flag to compute the damping range
      my_real,INTENT(IN) :: PM(NPROPM,NUMMAT), GEO(NPROPG,NUMGEO)
      my_real,INTENT(INOUT) ::  RNOISE(NPERTURB,NUMELS)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE (CLUSTER_) ,DIMENSION(NCLUSTER) :: CLUSTERS
      TYPE(MATPARAM_STRUCT_) , TARGET, DIMENSION(NUMMAT),INTENT(IN) :: MAT_PARAM
      INTEGER,INTENT(INOUT) :: TRIMAT
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER 
     .        I,J,K, NN,
     .        II,II0,JJ0,II1,JJ1,II2,JJ2,JJ,II3,JJ3,II4,JJ4,II5,JJ5,
     .        II6,JJ6,MODE,
     .        INUM_R2R(1+R2R_SIU*NUMELS),
     .        IPERT,STAT
          
      INTEGER WORK(70000)
      EXTERNAL MY_SHIFTL,MY_SHIFTR,MY_AND
      INTEGER MY_SHIFTL,MY_SHIFTR,MY_AND
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX2
      INTEGER :: CLUSTER_TYP,CLUSTER_NEL
      INTEGER, DIMENSION(:), ALLOCATABLE :: SAVE_CLUSTER
      my_real, DIMENSION(:,:), ALLOCATABLE :: XNUM_RNOISE
C-----------------------------------------------
C   S O U R C E   L I N E S
C-----------------------------------------------

C   GLOBAL SORTING ON ALL CRITERIA FOR ALL ELEMENTS
  
      CALL MY_ALLOC(INDEX2,NUMELS) 
C
      IF (NPERTURB > 0) THEN
        ALLOCATE(XNUM_RNOISE(NPERTURB,NUMELS),STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                           C1='XNUM_RNOISE')
      ELSE
        ALLOCATE(XNUM_RNOISE(NPERTURB,0),STAT=stat)
      ENDIF
C
      DO I=1,NUMELS
        INDEX2(I)=PERMUTATION%SOLID(I)
        EADD(I)=1
        ITRI(5,I)=I
        INDEX(I)=I
        INUM(1,I)=IPARTS(I)
        INUM(2,I)=IXS(1,I)
        INUM(3,I)=IXS(2,I)
        INUM(4,I)=IXS(3,I)
        INUM(5,I)=IXS(4,I)
        INUM(6,I)=IXS(5,I)
        INUM(7,I)=IXS(6,I)
        INUM(8,I)=IXS(7,I)
        INUM(9,I)=IXS(8,I)
        INUM(10,I)=IXS(9,I)
        INUM(11,I)=IXS(10,I)
        INUM(12,I)=IXS(11,I)
        INUM(13,I)=ISOLNOD(I)
        IF (NSUBDOM>0) INUM_R2R(I) = TAG_ELSF(I)
        IF (NPERTURB > 0) THEN
          DO IPERT = 1, NPERTURB
            XNUM_RNOISE(IPERT,I) = RNOISE(IPERT,I) 
          ENDDO
        ENDIF
      ENDDO
      IF(NSPHSOL /= 0)THEN
        DO I=1,NUMELS8
          INUM(14,I)=SOL2SPH(1,I)
          INUM(15,I)=SOL2SPH(2,I)
          INUM(16,I)=SOL2SPH_TYP(I)
        ENDDO
      END IF
C
      DO I=1,NUMELS
        XEP(I)=CEP(I)
      ENDDO

      itri(1:8,1:numels) = 0
      call get_sort_key_solid(numels,nb_key,nummat,numgeo, 
     .                                 npart,npropgi, 
     .                                 npropmi,npropm,npropg, 
     .                                 isms,idtgrs,npreload,trimat,numels8a, 
     .                                 iparts,isoloff,isolnod,iflag_bpreload,damp_range_part,tagprt_sms, 
     .                                 itri,ixs,igeo, 
     .                                 ipm,pm,geo, 
     .                                 mat_param)

      MODE=0
      CALL MY_ORDERS( MODE, WORK, ITRI, INDEX, NUMELS , 8)
C
      DO I=1,NUMELS
        IPARTS(I) =INUM(1,INDEX(I))
        ISOLNOD(I)=INUM(13,INDEX(I))
        IF (NSUBDOM>0) TAG_ELSF(I)=INUM_R2R(INDEX(I))
        IF (NPERTURB > 0) THEN
          DO IPERT = 1, NPERTURB
            RNOISE(IPERT,I) = XNUM_RNOISE(IPERT,INDEX(I)) 
          ENDDO
        ENDIF
      ENDDO

      DO I=1,NUMELS
        CEP(I)=XEP(INDEX(I))
        PERMUTATION%SOLID(I)=INDEX2(INDEX(I))
      ENDDO
      
      DO  K=1,11
        DO  I=1,NUMELS
          IXS(K,I)=INUM(K+1,INDEX(I))
        ENDDO
      ENDDO
C
C ISOLOFF
C
      DO I = 1, NUMELS
        INUM(3,I) = ISOLOFF(I)
      END DO
C
      DO I = 1, NUMELS
        ISOLOFF(I) = INUM(3,INDEX(I))
      END DO
C
C BOLT_PRELOAD
C
      IF (NPRELOAD > 0) THEN
       DO I = 1, NUMELS
         INUM(4,I) = IFLAG_BPRELOAD(I)
       END DO
C
       DO I = 1, NUMELS
         IFLAG_BPRELOAD(I) = INUM(4,INDEX(I))
       END DO
      ENDIF

C
      IF (NUMELS10+NUMELS20+NUMELS16 > 0) THEN
        DO I = 1, NUMELS10
          II = I + NUMELS8
          INUM(1,II)=IXS10(1,I)
          INUM(2,II)=IXS10(2,I)
          INUM(3,II)=IXS10(3,I)
          INUM(4,II)=IXS10(4,I)
          INUM(5,II)=IXS10(5,I)
          INUM(6,II)=IXS10(6,I)
        ENDDO
C
        DO I = 1, NUMELS10
          II = I + NUMELS8
          IXS10(1,I)=INUM(1,INDEX(II))
          IXS10(2,I)=INUM(2,INDEX(II))
          IXS10(3,I)=INUM(3,INDEX(II))
          IXS10(4,I)=INUM(4,INDEX(II))
          IXS10(5,I)=INUM(5,INDEX(II))
          IXS10(6,I)=INUM(6,INDEX(II))
        ENDDO
C
        DO I = 1, NUMELS20
          II = I + NUMELS8 + NUMELS10
          INUM(1,II) =IXS20(1,I)
          INUM(2,II) =IXS20(2,I)
          INUM(3,II) =IXS20(3,I)
          INUM(4,II) =IXS20(4,I)
          INUM(5,II) =IXS20(5,I)
          INUM(6,II) =IXS20(6,I)
          INUM(7,II) =IXS20(7,I)
          INUM(8,II) =IXS20(8,I)
          INUM(9,II) =IXS20(9,I)
          INUM(10,II)=IXS20(10,I)
          INUM(11,II)=IXS20(11,I)
          INUM(12,II)=IXS20(12,I)
        ENDDO
C
        DO I = 1, NUMELS20
          II = I + NUMELS8 + NUMELS10
          IXS20(1,I)=INUM(1,INDEX(II))
          IXS20(2,I)=INUM(2,INDEX(II))
          IXS20(3,I)=INUM(3,INDEX(II))
          IXS20(4,I)=INUM(4,INDEX(II))
          IXS20(5,I)=INUM(5,INDEX(II))
          IXS20(6,I)=INUM(6,INDEX(II))
          IXS20(7,I)=INUM(7,INDEX(II))
          IXS20(8,I)=INUM(8,INDEX(II))
          IXS20(9,I)=INUM(9,INDEX(II))
          IXS20(10,I)=INUM(10,INDEX(II))
          IXS20(11,I)=INUM(11,INDEX(II))
          IXS20(12,I)=INUM(12,INDEX(II))
        ENDDO
C
        DO I = 1, NUMELS16
          II = I + NUMELS8 + NUMELS10 + NUMELS20
          INUM(1,II) =IXS16(1,I)
          INUM(2,II) =IXS16(2,I)
          INUM(3,II) =IXS16(3,I)
          INUM(4,II) =IXS16(4,I)
          INUM(5,II) =IXS16(5,I)
          INUM(6,II) =IXS16(6,I)
          INUM(7,II) =IXS16(7,I)
          INUM(8,II) =IXS16(8,I)
        ENDDO
C
        DO I = 1, NUMELS16
          II = I + NUMELS8 + NUMELS10 + NUMELS20
          IXS16(1,I)=INUM(1,INDEX(II))
          IXS16(2,I)=INUM(2,INDEX(II))
          IXS16(3,I)=INUM(3,INDEX(II))
          IXS16(4,I)=INUM(4,INDEX(II))
          IXS16(5,I)=INUM(5,INDEX(II))
          IXS16(6,I)=INUM(6,INDEX(II))
          IXS16(7,I)=INUM(7,INDEX(II))
          IXS16(8,I)=INUM(8,INDEX(II))
        ENDDO
C      
      ENDIF
C
C INDEX INVERSION (IN ITR1)
C
      DO I=1,NUMELS
        ITR1(INDEX(I))=I
      ENDDO
C
C RENUMBERING FOR SURFACES
C
      DO I=1,NSURF
        NN=IGRSURF(I)%NSEG
        DO J=1,NN
          IF (IGRSURF(I)%ELTYP(J) == 1)
     .        IGRSURF(I)%ELEM(J) = ITR1(IGRSURF(I)%ELEM(J))
        ENDDO
      ENDDO
C
C RENUMBERING FOR SOLID GROUPS
C
      DO I=1,NGRBRIC
        NN=IGRBRIC(I)%NENTITY
        DO J=1,NN
          IGRBRIC(I)%ENTITY(J) = ITR1(IGRBRIC(I)%ENTITY(J))
        ENDDO
      ENDDO
C
C RENUMBERING FOR SPH CONVERSION
C
      IF(NSPHSOL /= 0)THEN
        DO I=1,NUMSPH
          IF(SPH2SOL(I) /= 0)SPH2SOL(I)=ITR1(SPH2SOL(I))
        ENDDO
C
C rebuild SOL2SPH
        DO I=1,NUMELS8
          SOL2SPH(1,I)=INUM(14,INDEX(I))
          SOL2SPH(2,I)=INUM(15,INDEX(I))
          SOL2SPH_TYP(I)=INUM(16,INDEX(I))
        END DO
      END IF
C
C renumbering INVERSE CONNECTIVITY
C
      DO I=1,8*NUMELS+6*NUMELS10+12*NUMELS20+8*NUMELS16
        IF(NOD2ELS(I) /= 0)NOD2ELS(I)=ITR1(NOD2ELS(I))
      END DO

!   -----------------------
!   reordering for cluster typ=1 (solid cluster)
      DO I=1,NCLUSTER
        CLUSTER_TYP = CLUSTERS(I)%TYPE
        IF(CLUSTER_TYP==1) THEN
            CLUSTER_NEL = CLUSTERS(I)%NEL
            ALLOCATE( SAVE_CLUSTER( CLUSTER_NEL ) )
            SAVE_CLUSTER( 1:CLUSTER_NEL ) = CLUSTERS(I)%ELEM( 1:CLUSTER_NEL )
            DO J=1,CLUSTER_NEL
                CLUSTERS(I)%ELEM(J) = ITR1( SAVE_CLUSTER( J ) )
            ENDDO
            DEALLOCATE( SAVE_CLUSTER )
        ENDIF
      ENDDO
!   -----------------------
C
C--------------------------------------------------------------
C         SUPER_GROUPS DETERMINATION
C--------------------------------------------------------------
      ND=1
      DO I=2,NUMELS
        II0=ITRI(1,INDEX(I))
        JJ0=ITRI(1,INDEX(I-1))
        II=ITRI(2,INDEX(I))
        JJ=ITRI(2,INDEX(I-1))
        II1=ITRI(3,INDEX(I))
        JJ1=ITRI(3,INDEX(I-1))
        II2=ITRI(4,INDEX(I))
        JJ2=ITRI(4,INDEX(I-1))
        II3=ITRI(5,INDEX(I))
        JJ3=ITRI(5,INDEX(I-1))
        II4=ITRI(6,INDEX(I))
        JJ4=ITRI(6,INDEX(I-1))
        II5=ITRI(7,INDEX(I))
        JJ5=ITRI(7,INDEX(I-1))
        II6=ITRI(8,INDEX(I))
        JJ6=ITRI(8,INDEX(I-1))
        IF(II0/=JJ0.OR.II/=JJ.OR.II1/=JJ1.OR.II2/=JJ2.OR.
     .     II5/=JJ5.OR.II3/=JJ3.OR.II4/=JJ4.OR.
     .     II6/=JJ6) THEN
          ND=ND+1
          EADD(ND)=I
        ENDIF
      ENDDO
      EADD(ND+1) = NUMELS+1
      DEALLOCATE(INDEX2)
c
      IF (NPERTURB > 0) THEN
        IF (ALLOCATED(XNUM_RNOISE)) DEALLOCATE(XNUM_RNOISE) 
      ENDIF
C-----------
      RETURN
      END
