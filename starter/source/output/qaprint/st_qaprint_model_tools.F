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
Chd|  ST_QAPRINT_MODEL_TOOLS        source/output/qaprint/st_qaprint_model_tools.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        FUNC2D_MOD                    share/modules1/func2d_mod.F   
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_MODEL_TOOLS(NOM_OPT   ,INOM_OPT  ,
     .           IBOX      ,IPMAS     ,NOM_SECT ,NSTRF ,SECBUF    ,
     .           SKEW      ,ISKWN     ,XFRAME   ,NPC   ,PLD       ,
     .           TABLE     ,NPTS      ,IACTIV   ,FACTIV,SENSOR_TAB,
     .           FUNC2D)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
      USE OPTIONDEF_MOD
      USE TABLE_MOD
      USE SENSOR_MOD
      USE FUNC2D_MOD
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
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
#include      "sphcom.inc"
#include      "lagmult.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NOM_OPT(LNOPT1,SNOM_OPT1), INOM_OPT(SINOM_OPT)
      TYPE (BOX_)    ,DIMENSION(NBBOX)  ,INTENT(IN) :: IBOX
      TYPE (ADMAS_)  ,DIMENSION(NODMAS) ,INTENT(IN) :: IPMAS
      INTEGER, INTENT(IN) :: NPTS,NPC(*)
      INTEGER,INTENT(IN) :: NOM_SECT(SNOM_SECT),NSTRF(SNSTRF)
      my_real,INTENT(IN) :: SECBUF(SSECBUF)  
      INTEGER, INTENT(IN) :: ISKWN(LISKN,*)
      my_real, INTENT(IN) :: SKEW(LSKEW,*)
      my_real, INTENT(IN) :: XFRAME(NXFRAME,*)
      my_real, INTENT(IN) ::
     .                        PLD(*)
      INTEGER, INTENT(IN) :: IACTIV(LACTIV,*)
      my_real, INTENT(IN) :: FACTIV(LRACTIV,*)
      TYPE(TTABLE) TABLE(*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR), TARGET :: SENSOR_TAB
      TYPE(FUNC2D_STRUCT), DIMENSION(NFUNC2D), INTENT(IN) :: FUNC2D
C-----------------------------------------------
C      NOM_OPT(LNOPT1,SNOM_OPT1) 
C         * Possibly, NOM_OPT(1) = ID
C        NOM_OPT(LNOPT1-LTITL+1:LTITL) <=> TITLES of the OPTIONS
C--------------------------------------------------
C      SNOM_OPT1= NRBODY+NACCELM+NVOLU+NINTER+NINTSUB+
C     +           NRWALL+NJOINT+NSECT+NLINK+
C     +           NUMSKW+1+NUMFRAM+1+NFXBODY+NFLOW+NRBE2+
C     +           NRBE3+NSUBMOD+NFXVEL+NUMBCS+NUMMPC+
C     +           NGJOINT+NUNIT0+NFUNCT+NADMESH+
C     +           NSPHIO+NSPCOND+NRBYKIN+NEBCS+
C     +           NINICRACK+NODMAS+NBGAUGE+NCLUSTER+NINTERFRIC+
C     +           NRBMERGE
C-----------------------------------------------
C      INOM_OPT(SINOM_OPT)
C--------------------------------------------------
C      INOM_OPT(1) = NRBODY
C      INOM_OPT(2) = INOM_OPT(1) + NACCELM
C      INOM_OPT(3) = INOM_OPT(2) + NVOLU
C      INOM_OPT(4) = INOM_OPT(3) + NINTER
C      INOM_OPT(5) = INOM_OPT(4) + NINTSUB
C      INOM_OPT(6) = INOM_OPT(5) + NRWALL
C      INOM_OPT(7) = INOM_OPT(6)
C      INOM_OPT(8) = INOM_OPT(7) + NJOINT
C      INOM_OPT(9) = INOM_OPT(8) + NSECT
C      INOM_OPT(10)= INOM_OPT(9) + NLINK
C      INOM_OPT(11)= INOM_OPT(10)+ NUMSKW+1+NUMFRAM+1+NSUBMOD
C      INOM_OPT(12)= INOM_OPT(11)+ NFXBODY
C      INOM_OPT(13)= INOM_OPT(12)+ NFLOW
C      INOM_OPT(14)= INOM_OPT(13)+ NRBE2
C      INOM_OPT(15)= INOM_OPT(14)+ NRBE3
C      INOM_OPT(16)= INOM_OPT(15)+ NFXVEL
C      INOM_OPT(17)= INOM_OPT(16)+ NUMBCS
C      INOM_OPT(18)= INOM_OPT(17)+ NUMMPC
C      INOM_OPT(19)= INOM_OPT(18)+ NGJOINT
C      INOM_OPT(20)= INOM_OPT(19)+ NUNIT0
C      INOM_OPT(21)= INOM_OPT(20)+ NFUNCT
C      INOM_OPT(22)= INOM_OPT(21)+ NADMESH
C      INOM_OPT(23)= INOM_OPT(22)+ NSPHIO
C      INOM_OPT(24)= INOM_OPT(23)+ NSPCOND
C      INOM_OPT(25)= INOM_OPT(24)+ NEBCS
C      INOM_OPT(26)= INOM_OPT(25)+ NINICRACK
C      INOM_OPT(27)= INOM_OPT(26)+ NODMAS
C      INOM_OPT(28)= INOM_OPT(27)+ NBGAUGE
C      INOM_OPT(29)= INOM_OPT(28)+ NCLUSTER
C      INOM_OPT(30)= INOM_OPT(29)+ NINTERFRIC
C      INOM_OPT(31)= INOM_OPT(30)+ NRBMERGE
C      .. TO BE MAINTAINED (cf doc/inom_opt.txt) ..
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IAD,OPT_ID,NDIM,NY,NOTABLE
      CHARACTER(LEN=255)         :: VARNAME
      CHARACTER(LEN=nchartitle)  :: TITR, TEMP_STRING
      DOUBLE PRECISION TEMP_DOUBLE
      INTEGER :: TEMP_INTEGER
      INTEGER :: ISECT, K0, K1, K2, K3, K4, K5, K6, K7, K8, K9, K10, KR0
      INTEGER :: NNOD,NSEGS,NSEGQ,NSEGC,NSEGT,NSEGP,NSEGR,NSEGTG,NBINTER,
     .           ISEN,INTVAL
      INTEGER :: WORK(70000),INDEX(2*(NUMFRAM+1)),IFRAME,ITR1(NUMFRAM+1)
      INTEGER :: INDEXS(2*(NSENSOR+1)),ITRS(NSENSOR+1)
      LOGICAL :: OK_QA
      DOUBLE PRECISION :: TIME, FVAL, XX, YY, ZZ
      INTEGER :: NPT, ID, II, LENTITR, ICODE
      INTEGER, DIMENSION(NTABLE + NFUNC2D) :: IDX, IDS
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C-----------------------------------------------
C     /BOX/...
C-----------------------------------------------
      IF (MYQAKEY('/BOX')) THEN
        DO IAD = 1,NBBOX
         !Title of the option was not stored in NOM_OPT ... TBD
          TITR   = IBOX(IAD)%TITLE
          OPT_ID = IBOX(IAD)%ID
          IF (LEN_TRIM(TITR)/=0) THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),OPT_ID,0.0_8)
          ELSE
            CALL QAPRINT('BOX_FAKE_NAME',OPT_ID,0.0_8)
          END IF
c---
          WRITE(VARNAME,'(A)') 'TYPE'
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBOX(IAD)%TYPE,0.0_8)
c
          WRITE(VARNAME,'(A)') 'NBOXBOX'
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBOX(IAD)%NBOXBOX,0.0_8)
c
          WRITE(VARNAME,'(A)') 'NOD1'
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBOX(IAD)%NOD1,0.0_8)
c
          WRITE(VARNAME,'(A)') 'ISKBOX'
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBOX(IAD)%ISKBOX,0.0_8)
c
          WRITE(VARNAME,'(A)') 'NOD2'
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBOX(IAD)%NOD2,0.0_8)
c
          WRITE(VARNAME,'(A)') 'NBLEVELS'
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBOX(IAD)%NBLEVELS,0.0_8)
c
          WRITE(VARNAME,'(A)') 'LEVEL'
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBOX(IAD)%LEVEL,0.0_8)
c
          WRITE(VARNAME,'(A)') 'ACTIBOX'
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBOX(IAD)%ACTIBOX,0.0_8)
c
          WRITE(VARNAME,'(A)') 'NENTITY'
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBOX(IAD)%NENTITY,0.0_8)
c
          WRITE(VARNAME,'(A)') 'SURFIAD'
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBOX(IAD)%SURFIAD,0.0_8)
c
          WRITE(VARNAME,'(A)') 'DIAM'
          TEMP_DOUBLE = IBOX(IAD)%DIAM
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
c
          WRITE(VARNAME,'(A)') 'X1'
          TEMP_DOUBLE = IBOX(IAD)%X1
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          WRITE(VARNAME,'(A)') 'Y1'
          TEMP_DOUBLE = IBOX(IAD)%Y1
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
c
          WRITE(VARNAME,'(A)') 'Z1'
          TEMP_DOUBLE = IBOX(IAD)%Z1
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
c
          WRITE(VARNAME,'(A)') 'X2'
          TEMP_DOUBLE = IBOX(IAD)%X2
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
c
          WRITE(VARNAME,'(A)') 'Y2'
          TEMP_DOUBLE = IBOX(IAD)%Y2
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
c
          WRITE(VARNAME,'(A)') 'Z2'
          TEMP_DOUBLE = IBOX(IAD)%Z2
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
c
          IF (IBOX(IAD)%NBOXBOX > 0) THEN
            DO I=1,IBOX(IAD)%NBOXBOX
              WRITE(VARNAME,'(A,I0)') 'BOXID_',I
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBOX(IAD)%IBOXBOX(I),0.0_8)
            ENDDO
          ENDIF

        END DO        
      END IF  ! /BOX/

c-----------------------------------------------
c     /ADMAS
c-----------------------------------------------
      IF (MYQAKEY('/ADMAS')) THEN
        DO IAD = 1,NODMAS
          TITR   = IPMAS(IAD)%TITLE
          OPT_ID = IPMAS(IAD)%ID
          IF (LEN_TRIM(TITR)/=0) THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),OPT_ID,0.0_8)
          ELSE
            CALL QAPRINT('BOX_FAKE_NAME',OPT_ID,0.0_8)
          END IF
c
          WRITE(VARNAME,'(A)') 'TYPE'
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IPMAS(IAD)%TYPE,0.0_8)
c
          WRITE(VARNAME,'(A)') 'WEIGHT_FLAG'
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IPMAS(IAD)%WEIGHT_FLAG,0.0_8)
c
          WRITE(VARNAME,'(A)') 'NPART'
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IPMAS(IAD)%NPART,0.0_8)
c
          IF (IPMAS(IAD)%NPART > 0) THEN
            DO I=1,IPMAS(IAD)%NPART
              WRITE(VARNAME,'(A,I0)') 'PARTID_',I
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IPMAS(IAD)%PARTID(I),0.0_8)
              TEMP_DOUBLE = IPMAS(IAD)%PART(I)%RPMAS
              WRITE(VARNAME,'(A,I0)') 'MAS_',I
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            ENDDO
          ENDIF
        END DO
      END IF  ! /ADMAS

C-----------------------------------------------
C     SECTIONS : /SECT, /SECT/CIRCLE, /SECT/PARAL
C-----------------------------------------------
      IF ( MYQAKEY('SECTIONS') ) THEN
        DO I=1,30                                                                          
          IF(NSTRF(I) /= 0)THEN                                               
            WRITE(VARNAME,'(A,I0)') 'SECTIONS__NSTRF_',I                                 
            TEMP_INTEGER = NSTRF(I)                                 
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INTEGER,0.0_8)                  
          END IF                                                                           
        END DO                                                                             
        DO I=1,10                                                                         
          IF(SECBUF(I) /= 0)THEN                                               
            WRITE(VARNAME,'(A,I0)') 'SECTIONS__SECBUF_',I                                 
            TEMP_DOUBLE = SECBUF(I)                                  
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)                  
          END IF                                                                           
        END DO                                                                             
        K0=31
        KR0=11
        DO ISECT = 1,NSECT
          CALL FRETITL2(TITR, NOM_OPT(LNOPT1-LTITR+1, INOM_OPT(8) + ISECT), LTITR)
          OPT_ID = NOM_OPT(1,INOM_OPT(8)+ISECT)
          IF (LEN_TRIM(TITR)/=0) THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),OPT_ID,0.0_8)
          ELSE
            CALL QAPRINT('SECTION_NO_NAME',OPT_ID,0.0_8)
          END IF
          DO J=1,ncharline
            TEMP_STRING(J:J)=CHAR( NOM_SECT( (ISECT-1)*ncharline+J ))
          ENDDO
          IF (LEN_TRIM(TEMP_STRING) > 0) THEN
             CALL QAPRINT( TRIM(TEMP_STRING), 0 , 0.0_8)
          ELSE
             CALL QAPRINT( "NO_FILE_NAME", 0 , 0.0_8)
          ENDIF
          NNOD=NSTRF(K0+6)
          NSEGS=NSTRF(K0+7)
          NSEGQ=NSTRF(K0+8)
          NSEGC=NSTRF(K0+9)
          NSEGT=NSTRF(K0+10)
          NSEGP=NSTRF(K0+11)
          NSEGR=NSTRF(K0+12)
          NSEGTG=NSTRF(K0+13)
          NBINTER=NSTRF(K0+14)          
          K1=K0+30
          K2=K1+NNOD
          K3=K2+NBINTER
          K4=K3+2*NSEGS
          K5=K4+2*NSEGQ
          K6=K5+2*NSEGC
          K7=K6+2*NSEGT
          K8=K7+2*NSEGP
          K9=K8+2*NSEGR          
          DO I=K0,NSTRF(K0+24)-1
            IF(NSTRF(I) /= 0)THEN
              WRITE(VARNAME,'(A,I0,A,I0)') 'SECTIONS__',OPT_ID,"_NSTRF_",I
              TEMP_INTEGER = NSTRF(I)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INTEGER,0.0_8)
            END IF
          END DO              
          DO I=KR0,NSTRF(K0+25)
            IF(SECBUF(I) /= 0)THEN
          WRITE(VARNAME,'(A,I0,A,I0)') 'SECTIONS__',OPT_ID,"_SECBUF_",I
              TEMP_DOUBLE = SECBUF(I)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO    
          IF(K0+24 <= SNSTRF) K0 = NSTRF(K0+24) 
          if(K0+25 <= SNSTRF) KR0 = NSTRF(K0+25)
        ENDDO !next ISECT    
c-----------
      END IF  ! SECTIONS
C-----------------------------------------------
C     SKEWS : /SKEW/FIX, /SKEW/MOV, /SKEW/MOV2
C-----------------------------------------------
      IF ( MYQAKEY('SKEWS') ) THEN

        DO IAD=1,NUMSKW+1

          CALL FRETITL2(TITR, NOM_OPT(LNOPT1-LTITR+1,INOM_OPT(10)+IAD), LTITR)
C
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),ISKWN(4,IAD),0.0_8)
          ELSE
            CALL QAPRINT('A_SKEW_FAKE_NAME',ISKWN(4,IAD),0.0_8)
          END IF
C
          DO I=1,LISKN
            IF(ISKWN(I,IAD)/=0)THEN
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'ISKWN_',I      
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ISKWN(I,IAD),0.0_8)
            END IF
          END DO
C
          DO I=1,LSKEW
            IF(SKEW(I,IAD)/=ZERO)THEN
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'SKEW_',I
              TEMP_DOUBLE = SKEW(I,IAD)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C
        ENDDO

        DO IAD=NUMSKW+2,NUMSKW+1+MIN(1,NSPCOND)*NUMSPH
C
          CALL QAPRINT('A_SPH_SKEW_FAKE_NAME',ISKWN(4,IAD),0.0_8)
C
          DO I=1,LISKN
            IF(ISKWN(I,IAD)/=0)THEN
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'ISKWN_',I      
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ISKWN(I,IAD),0.0_8)
            END IF
          END DO
C
          DO I=1,LSKEW
            IF(SKEW(I,IAD)/=ZERO)THEN
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'SKEW_',I
              TEMP_DOUBLE = SKEW(I,IAD)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C
        ENDDO
C
        DO IAD=NUMSKW+1+MIN(1,NSPCOND)*NUMSPH+1,NUMSKW+1+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
C
          CALL QAPRINT('A_SUBMODEL_SKEW_FAKE_NAME',ISKWN(4,IAD),0.0_8)
C
          DO I=1,LISKN
            IF(ISKWN(I,IAD)/=0)THEN
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'ISKWN_',I      
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ISKWN(I,IAD),0.0_8)
            END IF
          END DO
C
          DO I=1,LSKEW
            IF(SKEW(I,IAD)/=ZERO)THEN
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'SKEW_',I
              TEMP_DOUBLE = SKEW(I,IAD)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C
        ENDDO
C
      END IF  ! SKEWS 
C    
C-----------------------------------------------
C     FRAMES : /FRAME/FIX, /FRAME/MOV, /FRAME/MOV2, /FRAME/NODE
C-----------------------------------------------
      IF ( MYQAKEY('FRAMES') ) THEN
C
        DO IFRAME=1,NUMFRAM+1
          ITR1(IFRAME)=ISKWN(4,NUMSKW+1+MIN(1,NSPCOND)*NUMSPH+NSUBMOD+IFRAME)+2
        ENDDO 
        CALL MY_ORDERS(0,WORK,ITR1,INDEX,NUMFRAM+1,1)
C
        DO IFRAME=1,NUMFRAM+1
          IAD = INDEX(IFRAME)
          CALL FRETITL2(TITR, NOM_OPT(LNOPT1-LTITR+1,INOM_OPT(10)+NUMSKW+1+IAD), LTITR)
C
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),ISKWN(4,NUMSKW+1+MIN(1,NSPCOND)*NUMSPH+NSUBMOD+IAD),0.0_8)
          ELSE
            CALL QAPRINT('A_FRAME_FAKE_NAME',ISKWN(4,NUMSKW+1+MIN(1,NSPCOND)*NUMSPH+NSUBMOD+IAD),0.0_8)
          END IF
C
          DO I=1,LISKN
            IF(ISKWN(I,NUMSKW+1+MIN(1,NSPCOND)*NUMSPH+NSUBMOD+IAD)/=0)THEN
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'ISKWN_',I      
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ISKWN(I,NUMSKW+1+MIN(1,NSPCOND)*NUMSPH+NSUBMOD+IAD),0.0_8)
            END IF
          END DO
C
          DO I=1,NXFRAME
            IF(XFRAME(I,IAD)/=ZERO)THEN
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'XFRAME_',I
              TEMP_DOUBLE = XFRAME(I,IAD)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C
        ENDDO
C
      END IF  ! FRAMES 
c-----------
C-----------------------------------------------
C     /TABLE
C-----------------------------------------------
      IF ( MYQAKEY('TABLE') ) THEN
         DO IAD = 1, NTABLE
            IDX(IAD) = IAD 
         ENDDO
         CALL QUICKSORT_I(IDX, 1,NTABLE )
C
C         Title of the option was not stored in NOM_OPT FOR TABLES ONLY FOR FUNCTIONS OR 1D TABLES
          !CALL FRETITL2(TITR, NOM_OPT(LNOPT1-LTITR+1,INOM_OPT(20)+IAD), LTITR)
          !OPT_ID = NOM_OPT(1,INOM_OPT(20)+IAD)
          !IF (LEN_TRIM(TITR)/=0) THEN
          !  CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),OPT_ID,0.0_8)
          !ELSE
          !  CALL QAPRINT('TABLE_NO_NAME',OPT_ID,0.0_8)
          !END IF

        DO II=1, NTABLE
          IAD = IDX(II)
          TITR(1:nchartitle)=''
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),IAD,0.0_8)
          ELSE
            CALL QAPRINT('TABLE_NO_NAME',IAD,0.0_8)
          END IF

          WRITE(VARNAME,'(A)') 'NOTABLE'
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TABLE(IAD)%NOTABLE,0.0_8) 
          NOTABLE = TABLE(IAD)%NOTABLE

          WRITE(VARNAME,'(A)') 'NDIM'
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TABLE(IAD)%NDIM,0.0_8) 
          NDIM = TABLE(IAD)%NDIM

          DO I=1,NDIM
            NY=SIZE(TABLE(IAD)%X(I)%VALUES)
            DO J=1,NY
             WRITE(VARNAME,'(A,I0,A,I0)') 'X',I,' ',J
             TEMP_DOUBLE = TABLE(IAD)%X(I)%VALUES(J)
             CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            ENDDO 
          END DO
          NY=SIZE(TABLE(IAD)%Y%VALUES)
          DO J=1,NY
             WRITE(VARNAME,'(A,I0)') 'Y',J
             TEMP_DOUBLE = TABLE(IAD)%Y%VALUES(J)
             CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          ENDDO 
        ENDDO
C
      END IF  ! TABLES 
c-----------
C-----------------------------------------------
C     /FUNCT
C-----------------------------------------------
      OK_QA = MYQAKEY('/FUNCT')
      OK_QA = OK_QA .OR. MYQAKEY('/MOVE_FUNCT')
      IF (OK_QA) THEN
         DO IAD = 1, NFUNCT
            IDS(IAD) = NOM_OPT(1, INOM_OPT(20) + IAD)
            IDX(IAD) = IAD 
         ENDDO
         CALL QUICKSORT_I2(IDS, IDX, 1, NFUNCT)
         DO II = 1, NFUNCT
            IAD = IDX(II)
            TITR(1:nchartitle) = ''
            ID = NOM_OPT(1, INOM_OPT(20) + IAD)
            CALL FRETITL2(TITR, NOM_OPT(LNOPT1-LTITR+1, INOM_OPT(20) + IAD), LTITR)
            LENTITR=LEN_TRIM(TITR) 
            ICODE=0
            IF(LENTITR>0)ICODE=iachar(TITR(1:1))
            IF (LENTITR /= 0 .AND. ICODE /= 0) THEN
               CALL QAPRINT(TITR(1:LENTITR), ID, 0.0_8)
            ELSE
               CALL QAPRINT('FUNCT_NO_NAME', ID, 0.0_8)
            ENDIF
! Number of points
            NPT = (NPC(IAD + 1) - NPC(IAD)) / 2
            WRITE(VARNAME,'(A,I0)') 'NB_POINTS_',ID
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NPT,0.0_8) 
            DO I = NPC(IAD), NPC(IAD + 1) - 1, 2
               TIME = PLD(I)
               FVAL = PLD(I + 1)
               WRITE(VARNAME,'(A,I0)') 'TIME_', (I - NPC(IAD) + 2) / 2
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), 0, TIME)
               WRITE(VARNAME,'(A,I0)') 'FUNCT_VALUE_', (I - NPC(IAD) + 2) / 2
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), 0, FVAL)
            ENDDO
         ENDDO 
      END IF                    ! /FUNCT
C-----------------------------------------------
C     /FUNC_2D
C-----------------------------------------------      
      IF (OK_QA) THEN
         DO II = 1, NFUNC2D
            IDS(II) = FUNC2D(II)%ID
            IDX(II) = II
         ENDDO
         CALL QUICKSORT_I2(IDS, IDX, 1, NFUNC2D)
         DO II = 1, NFUNC2D
            IAD = IDX(II)
            ID = FUNC2D(IAD)%ID
            CALL QAPRINT("FUNC2D_", ID, 0.0_8)
            DO I = 1, FUNC2D(IAD)%NPT
               XX = FUNC2D(IAD)%XVAL(1, I)
               YY = FUNC2D(IAD)%XVAL(2, I)
               WRITE(VARNAME,'(A,I0)') 'X_', I
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), 0, XX)
               WRITE(VARNAME,'(A,I0)') 'Y_', I
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), 0, YY)
               DO J = 1, FUNC2D(IAD)%DIM
                  ZZ = FUNC2D(IAD)%FVAL(J, I)
                  WRITE(VARNAME,'(A,I0,A,I0)') 'F_', J, '_', I
                  CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), 0, ZZ)
               ENDDO
            ENDDO
         ENDDO
      ENDIF
C-----------------------------------------------
C     /ACTIV
C-----------------------------------------------
      IF ( MYQAKEY('/ACTIV') ) THEN
        DO IAD=1,NACTIV
C
          CALL QAPRINT('ACTIV',IAD,0.0_8)

          DO I=1,LACTIV
            IF(IACTIV(I,IAD)/=0)THEN
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'IACTIV_',I      
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IACTIV(I,IAD),0.0_8)
            END IF
          END DO

          DO I=1,LRACTIV
            IF(FACTIV(I,IAD)/=0)THEN
             WRITE(VARNAME,'(A,I0)') 'FACTIV_',I
             TEMP_DOUBLE = FACTIV(I,IAD)
             CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
        ENDDO
      END IF  ! /ACTIV 
C-----------------------------------------------
C     SENSORS
C-----------------------------------------------
      IF (MYQAKEY('SENSOR') ) THEN
C
        DO ISEN=1,NSENSOR
          ITRS(ISEN) = SENSOR_TAB(ISEN)%SENS_ID
        ENDDO 
        CALL MY_ORDERS(0,WORK,ITRS,INDEXS,NSENSOR,1)
C
        DO ISEN=1,NSENSOR
          IAD = INDEXS(ISEN)
          OPT_ID = SENSOR_TAB(IAD)%SENS_ID
          IF (OPT_ID > 0 .and. SENSOR_TAB(IAD)%TYPE >= 0) THEN
            CALL QAPRINT('NEW SENSOR_NO_NAME', OPT_ID, 0.0_8)
c
            WRITE(VARNAME,'(A)') 'SENSOR_ID'    
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),SENSOR_TAB(IAD)%SENS_ID,0.0_8)
            WRITE(VARNAME,'(A)') 'SENSOR_TYPE'    
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),SENSOR_TAB(IAD)%TYPE,0.0_8)
            WRITE(VARNAME,'(A)') 'TDELAY'   
            TEMP_DOUBLE = SENSOR_TAB(IAD)%TDELAY
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            WRITE(VARNAME,'(A)') 'TMIN'
            TEMP_DOUBLE = SENSOR_TAB(IAD)%TMIN  
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            WRITE(VARNAME,'(A)') 'TCRIT' 
            TEMP_DOUBLE = SENSOR_TAB(IAD)%TCRIT  
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            WRITE(VARNAME,'(A)') 'TSTART' 
            TEMP_DOUBLE = SENSOR_TAB(IAD)%TSTART   
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            WRITE(VARNAME,'(A)') 'VALUE'  
            TEMP_DOUBLE = SENSOR_TAB(IAD)%VALUE
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            WRITE(VARNAME,'(A)') 'STATUS'    
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),SENSOR_TAB(IAD)%STATUS,0.0_8)
            WRITE(VARNAME,'(A)') 'NPARI'    
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),SENSOR_TAB(IAD)%NPARI,0.0_8)
            WRITE(VARNAME,'(A)') 'NPARR'    
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),SENSOR_TAB(IAD)%NPARR,0.0_8)
            WRITE(VARNAME,'(A)') 'NVAR'    
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),SENSOR_TAB(IAD)%NVAR,0.0_8)
c
            DO I = 1,SENSOR_TAB(IAD)%NPARI
              INTVAL = SENSOR_TAB(IAD)%IPARAM(I)
              WRITE(VARNAME,'(A,I0)') 'IPARAM_',I      
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),INTVAL,0.0_8)
            END DO
c
            DO I=1,SENSOR_TAB(IAD)%NPARR
              FVAL = SENSOR_TAB(IAD)%RPARAM(I)
              WRITE(VARNAME,'(A,I0)') 'RPARAM_',I      
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,FVAL)
            END DO
          END IF
c
        ENDDO
c
      END IF  ! /SENSOR
C-----------------------------------------------
C     /LAGMUL
C-----------------------------------------------
      IF (MYQAKEY('/LAGMUL')) THEN

        CALL QAPRINT('LAGMUL', 0,0.0_8)
C
        WRITE(VARNAME,'(A)') 'LAGMOD'
        TEMP_INTEGER = LAGMOD
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INTEGER,0.0_8)
c
        WRITE(VARNAME,'(A)') 'LAGOPT'
        TEMP_INTEGER = LAGOPT
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INTEGER,0.0_8)
c
        WRITE(VARNAME,'(A)') 'LAGM_TOL'
        TEMP_DOUBLE = LAGM_TOL
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
c
        WRITE(VARNAME,'(A)') 'LAG_ALPH'
        TEMP_DOUBLE = LAG_ALPH
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
c
        WRITE(VARNAME,'(A)') 'LAG_ALPHS'
        TEMP_DOUBLE = LAG_ALPHS
        CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
C
      END IF  ! /LAGMUL
c-----------
      RETURN
      END
c
