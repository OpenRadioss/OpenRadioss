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
Chd|  ST_QAPRINT_LOADS              source/output/qaprint/st_qaprint_loads.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        FRETITL2                      source/starter/freform.F      
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_LOADS(NOM_OPT   ,INOM_OPT  ,NUMLOADP  ,ILOADP    ,LLOADP    ,
     2                            LOADP     ,IBCL      ,FORC      ,IPRES     ,PRES      ,
     3                            IBCR      ,FRADIA    ,IBCV      ,FCONV     , IGRV     ,
     4                            LGRV      ,AGRV      ,ICFIELD   ,LCFIELD   ,CFIELD    ,
     5                            IPRELOAD  ,PRELOAD   ,IFLAG_BPRELOAD, 
     6                            LIFLOW, LRFLOW, IFLOW,RFLOW     ,ISPHIO    ,VSPHIO    )
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
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
#include      "boltpr_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NOM_OPT(LNOPT1,SNOM_OPT1), INOM_OPT(SINOM_OPT)
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
      INTEGER, INTENT(IN) :: NUMLOADP, ILOADP(SIZLOADP,NLOADP), LLOADP(NUMLOADP)
      INTEGER, INTENT(IN) :: ICFIELD(SIZFIELD,NLOADC), LCFIELD(SLCFIELD)
      INTEGER, INTENT(IN) :: IBCL(NIBCLD,NCONLD-NPRELD), IPRES(NIBCLD,NPRELD)
      INTEGER, INTENT(IN) :: IBCR(NIRADIA,NUMRADIA)
      INTEGER, INTENT(IN) :: IBCV(NICONV,NUMCONV)
      INTEGER, INTENT(IN) :: IGRV(NIGRV,NGRAV), LGRV(*)
      INTEGER, INTENT(IN) :: IPRELOAD(3,*), IFLAG_BPRELOAD(NUMELS)
      my_real, INTENT(IN) ::
     .                       LOADP(LFACLOAD,NLOADP), CFIELD(LFACLOAD,NLOADC),
     .                       FORC(LFACCLD,NCONLD-NPRELD), PRES(LFACCLD,NPRELD),
     .                       AGRV(LFACGRV,NGRAV),PRELOAD(6,*) 
      my_real, INTENT(IN) ::  
     .                       FRADIA(LFACTHER,NUMRADIA)
      my_real, INTENT(IN) ::  
     .                       FCONV(LFACTHER,NUMCONV)
      INTEGER, INTENT(IN) :: LIFLOW, LRFLOW
      INTEGER, DIMENSION(LIFLOW), INTENT(IN) :: IFLOW
      my_real, DIMENSION(LRFLOW), INTENT(IN) :: RFLOW
      INTEGER ISPHIO(NISPHIO,NSPHIO)
      my_real
     .        VSPHIO(SVSPHIO)
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IS,IPRE, MY_ID, MY_LOAD,J,K0,NSMAX,NS,LPRELOADTAB,
     .        IDS(NSPHIO),IDX(NSPHIO),II,MY_SPHIO,LVAD(NSPHIO),
     .        IADSPHIO_1,IADSPHIO_2,FIRST,LAST
      CHARACTER *nchartitle TITR
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE
      LOGICAL :: OK_QA 
      INTEGER :: COUNT,IOPT_FIRST,IOPT_LAST
C-----------------------------------------------
C     /LOAD/CENTRI
C-----------------------------------------------
      IF (MYQAKEY('/LOAD/CENTRI')) THEN
        DO MY_LOAD=1,NLOADC
C
C         Title of the option was not stored in NOM_OPT ... TBD
          TITR(1:nchartitle)=''
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),ICFIELD(9,MY_LOAD),0.0_8)
          ELSE
            CALL QAPRINT('A_LOAD_CENTRI_FAKE_NAME',ICFIELD(9,MY_LOAD),0.0_8)
          END IF
C
          DO I=1,SIZFIELD
            IF(ICFIELD(I,MY_LOAD) /=0)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'ICFIELD_',I      ! ICFIELD(11) => 'ICFIELD_11'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ICFIELD(I,MY_LOAD),0.0_8)
            END IF
          END DO
C
          DO I=ICFIELD(4,MY_LOAD),ICFIELD(4,MY_LOAD)+ICFIELD(1,MY_LOAD)-1
C
C           VARNAME: variable name in ref.extract (without blanks)
            WRITE(VARNAME,'(A,I0)') 'LCFIELD_',I      ! LCFIELD(11) => 'LCFIELD_11'
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),LCFIELD(I),0.0_8)
          END DO
C
          DO I=1,LFACLOAD
            IF(CFIELD(I,MY_LOAD)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'CFIELD_',I
              TEMP_DOUBLE = CFIELD(I,MY_LOAD)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C
        END DO ! MY_LOAD=1,NLOADC
      END IF
C-----------------------------------------------
C     /LOAD/PFLUID & /LOAD/PBLAST  & /LOAD/PRESSURE
C-----------------------------------------------
      OK_QA = MYQAKEY('/LOAD/PFLUID') .OR. MYQAKEY('/LOAD/PBLAST') .OR. MYQAKEY('/LOAD/PRESSURE') 
      
      IF (OK_QA) THEN

        IF(MYQAKEY('/LOAD/PFLUID'))THEN
          !output pfluid only
          IOPT_FIRST = 1
          IOPT_LAST = NLOADP_F
        ELSEIF(MYQAKEY('/LOAD/PBLAST'))THEN
          !output pblast only
          IOPT_FIRST = 1+NLOADP_F
          IOPT_LAST = NLOADP_F+NLOADP_B
        ELSEIF(MYQAKEY('/LOAD/PRESSURE'))THEN
          !output pressure only
          IOPT_FIRST = 1+NLOADP_F+NLOADP_B
          IOPT_LAST = NLOADP_F+NLOADP_B+NLOADP_HYD
        ENDIF
            
        !common procedure for /LOAD/PFLUID, /LOAD/PBLAST, /LOAD/PRESSURE options.
        DO MY_LOAD=IOPT_FIRST,IOPT_LAST
C
C         Title of the option was not stored in NOM_OPT ... TBD
          TITR(1:nchartitle)=''
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),ILOADP(2,MY_LOAD),0.0_8)
          ELSE
            CALL QAPRINT('A_LOAD_PFLUID_FAKE_NAME',ILOADP(2,MY_LOAD),0.0_8)
          END IF
C
          DO I=1,SIZLOADP
            IF(ILOADP(I,MY_LOAD) /=0)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'ILOADP_',I      ! ILOADP(11) => 'ILOADP_11'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ILOADP(I,MY_LOAD),0.0_8)
            END IF
          END DO

          DO I=1,LFACLOAD
            IF(LOADP(I,MY_LOAD)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'LOADP_',I
              TEMP_DOUBLE = LOADP(I,MY_LOAD)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
 
          FIRST=ILOADP(4,MY_LOAD)
          LAST=ILOADP(4,MY_LOAD)+ILOADP(1,MY_LOAD)-1
          
          IF(LAST-FIRST+1 <= 10 )THEN            
            !display all segments
            DO I=FIRST,LAST
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'LLOADP_',I      ! LLOADP(11) => 'LLOADP_11'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),LLOADP(I),0.0_8)
            END DO
          ELSE
            !display only 5 first ones and 5 last ones
            !first 5 segments
            FIRST=ILOADP(4,MY_LOAD)
            LAST=FIRST+5
            DO I=FIRST,LAST
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'LLOADP_',I      ! LLOADP(11) => 'LLOADP_11'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),LLOADP(I),0.0_8)
            END DO  
            !suspension points '...'
            CALL QAPRINT('...',0,0.0_8)
            !last 5 segments
            LAST=ILOADP(4,MY_LOAD)+ILOADP(1,MY_LOAD)-1
            FIRST=LAST-5
            DO I=FIRST,LAST
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'LLOADP_',I      ! LLOADP(11) => 'LLOADP_11'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),LLOADP(I),0.0_8)
            END DO                     
          ENDIF
C
        END DO ! MY_LOAD=IOPT_FIRST, IOPT_LAST
      END IF

C-----------------------------------------------
C     /CLOAD
C-----------------------------------------------
      IF (MYQAKEY('/CLOAD')) THEN
        DO MY_LOAD=1,NCONLD-NPRELD
C
C         Title of the option was not stored in NOM_OPT ... TBD
C         and Cload ID is not stored 
          TITR(1:nchartitle)=''
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_LOAD,0.0_8)
          ELSE
            CALL QAPRINT('A_CLOAD_FAKE_NAME',MY_LOAD,0.0_8)
          END IF
C
          DO I=1,NIBCLD
            IF(IBCL(I,MY_LOAD) /=0)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'IBCL_',I      ! IBCL(11) => 'IBCL_11'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBCL(I,MY_LOAD),0.0_8)
            END IF
          END DO
C
          DO I=1,LFACCLD
            IF(FORC(I,MY_LOAD)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'FORC_',I
              TEMP_DOUBLE = FORC(I,MY_LOAD)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C
        END DO ! MY_LOAD=1,NCONLD-NPRELD
      END IF
C-----------------------------------------------
C     /PLOAD
C-----------------------------------------------
      IF (MYQAKEY('/PLOAD')) THEN
        DO MY_LOAD=1,NPRELD
C
C         Title of the option was not stored in NOM_OPT ... TBD
C         and Pload ID is not stored 
          TITR(1:nchartitle)=''
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_LOAD,0.0_8)
          ELSE
            CALL QAPRINT('A_PLOAD_FAKE_NAME',MY_LOAD,0.0_8)
          END IF
C
          DO I=1,NIBCLD
            IF(IPRES(I,MY_LOAD) /=0)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'IPRES_',I      ! IPRES(11) => 'IPRES_11'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IPRES(I,MY_LOAD),0.0_8)
            END IF
          END DO
C
          DO I=1,LFACCLD
            IF(PRES(I,MY_LOAD)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'PRES_',I
              TEMP_DOUBLE = PRES(I,MY_LOAD)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C
        END DO ! MY_LOAD=1,NPRELD
      END IF
C-----------------------------------------------
C     /RADIATION
C-----------------------------------------------
      IF (MYQAKEY('/RADIATION')) THEN
        DO MY_LOAD=1,NUMRADIA
C
C         Title of the option was not stored in NOM_OPT ... TBD
C         and Radiation ID is not stored 
          TITR(1:nchartitle)=''
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_LOAD,0.0_8)
          ELSE
            CALL QAPRINT('A_RADIATION_FAKE_NAME',MY_LOAD,0.0_8)
          END IF
C
          DO I=1,NIRADIA
            IF(IBCR(I,MY_LOAD) /=0)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'IBCR_',I      ! IBCR(11) => 'IBCR_11'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBCR(I,MY_LOAD),0.0_8)
            END IF
          END DO
C
          DO I=1,LFACTHER
            IF(FRADIA(I,MY_LOAD)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'FRADIA_',I
              TEMP_DOUBLE = FRADIA(I,MY_LOAD)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C
        END DO ! MY_LOAD=1,NUMRADIA
      END IF
C-----------------------------------------------
C     /CONVEC
C-----------------------------------------------
      IF (MYQAKEY('/CONVEC')) THEN
        DO MY_LOAD=1,NUMCONV
C
C         Title of the option was not stored in NOM_OPT ... TBD
C         and Convev ID is not stored 
          TITR(1:nchartitle)=''
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_LOAD,0.0_8)
          ELSE
            CALL QAPRINT('A_CONVEC_FAKE_NAME',MY_LOAD,0.0_8)
          END IF
C
          DO I=1,NICONV
            IF(IBCV(I,MY_LOAD) /=0)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'IBCV_',I      ! IBCV(11) => 'IBCV_11'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBCV(I,MY_LOAD),0.0_8)
            END IF
          END DO
C
          DO I=1,LFACTHER
            IF(FCONV(I,MY_LOAD)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'FCONV_',I
              TEMP_DOUBLE = FCONV(I,MY_LOAD)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C
        END DO ! MY_LOAD=1,NUMCONV
      END IF
C-----------------------------------------------
C     /GRAV
C-----------------------------------------------
      IF (MYQAKEY('/GRAV')) THEN
        DO MY_LOAD=1,NGRAV
C
C         Title of the option was not stored in NOM_OPT ... TBD
          TITR(1:nchartitle)=''
          MY_ID=IGRV(5,MY_LOAD)
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_ID,0.0_8)
          ELSE
            CALL QAPRINT('A_GRAVITY_FAKE_NAME',MY_ID,0.0_8)
          END IF
C
          DO I=1,NIGRV
            IF(IGRV(I,MY_LOAD) /=0)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'IGRV_',I      ! IGRV(11) => 'IGRV_11'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IGRV(I,MY_LOAD),0.0_8)
            END IF
          END DO
C
          DO I=IGRV(4,MY_LOAD),IGRV(4,MY_LOAD)+IGRV(1,MY_LOAD)-1
C
C           VARNAME: variable name in ref.extract (without blanks)
            WRITE(VARNAME,'(A,I0)') 'LGRV_',I      
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),LGRV(I),0.0_8)
          END DO
C
          DO I=1,LFACGRV
            IF(AGRV(I,MY_LOAD)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'AGRV_',I
              TEMP_DOUBLE = AGRV(I,MY_LOAD)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C
        END DO ! MY_LOAD=1,NGRAV
      END IF
C-----------------------------------------------
C     /PRELOAD
C-----------------------------------------------
      IF (MYQAKEY('/PRELOAD')) THEN
C       
        ! ID of /PRELOAD is not stored and so not retrieved
        IF (NPRELOAD > 0) THEN 

          DO IPRE = 1,NUMPRELOAD
c
            TITR(1:nchartitle)=''
            IF(LEN_TRIM(TITR)/=0)THEN
              CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),IPRE,0.0_8)
            ELSE
              CALL QAPRINT('PRELOAD_FAKE_NAME',IPRE,0.0_8)
            END IF
c
            DO J = 1 , 3
             IF(IPRELOAD(J,IPRE) /=0)THEN
              WRITE(VARNAME,'(A,I0,I0)') 'IPRELOAD_',J,IPRE      
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IPRELOAD(J,IPRE),0.0_8)
             END IF           
            ENDDO
c
            IF(IPRELOAD(1,IPRE) /=0)THEN
             J = IPRELOAD(1,IPRE)
             IF(IFLAG_BPRELOAD(J) /=0)THEN ! this table is used in PRELOAD but modified in sgrhead and SGRTAILS
              WRITE(VARNAME,'(A,I0)') 'IFLAG_BPRELOAD_',J    
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IFLAG_BPRELOAD(J),0.0_8)
             END IF
            END IF
c 
            DO J = 1 , 6
             IF(PRELOAD(J,IPRE)/=ZERO)THEN
              WRITE(VARNAME,'(A,I0,I0)') 'PRELOAD_',J,IPRE
              TEMP_DOUBLE = PRELOAD(J,IPRE)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
             END IF
            ENDDO
c
          ENDDO
        ENDIF
      ENDIF

C     /BEM/DAA and /BEM/FLOW
      OK_QA = MYQAKEY('/BEM') .AND. NFLOW > 0
      IF (OK_QA) THEN
         WRITE(VARNAME, '(A)') "LIFLOW_=_"
         CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), LIFLOW, 0.0_8)
         WRITE(VARNAME, '(A)') "LRFLOW_=_"
         CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), LRFLOW, 0.0_8)
         COUNT = 0
         DO I = 1, LIFLOW
            WRITE(VARNAME, '(A, I0)') "IFLOW ", I
            IF (IFLOW(I) /= 0) THEN
               COUNT = COUNT + 1
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), IFLOW(I), 0.0_8)
            ENDIF
            IF (COUNT == 100) EXIT
         ENDDO
         COUNT = 0
         DO I = LIFLOW, 1, -1
            WRITE(VARNAME, '(A, I0)') "IFLOW ", I
            IF (IFLOW(I) /= 0) THEN
               COUNT = COUNT + 1
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)), IFLOW(I), 0.0_8)
            ENDIF
            IF (COUNT == 100) EXIT
         ENDDO
         COUNT = 0
         DO I = 1, LRFLOW
            WRITE(VARNAME, '(A, I0)') "RFLOW ", I
            TEMP_DOUBLE = RFLOW(I)
            IF (TEMP_DOUBLE /= 0.0_8) THEN
               COUNT = COUNT + 1
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            ENDIF
            IF (COUNT == 100) EXIT
         ENDDO
         COUNT = 0
         DO I = LRFLOW, 1, -1
            WRITE(VARNAME, '(A, I0)') "RFLOW ", I
            TEMP_DOUBLE = RFLOW(I)
            IF (TEMP_DOUBLE /= 0.0_8) THEN
               COUNT = COUNT + 1
               CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            ENDIF
            IF (COUNT == 100) EXIT
         ENDDO
      ENDIF
C-----------------------------------------------
C     /SPH/INOUT
C-----------------------------------------------
      IF (MYQAKEY('/SPH/INOUT')) THEN
        IF (NSPHIO > 0) THEN 
C        
!     Sort by ID to ensure internal order independant output
          DO I = 1, NSPHIO
            IDS(I) = ISPHIO(NISPHIO,I)
            IDX(I) = I
            IF (I /= NSPHIO) THEN 
              LVAD(I) = ISPHIO(4,I+1) - ISPHIO(4,I)
            ELSE
              LVAD(I) = SVSPHIO - ISPHIO(4,I)
            ENDIF
          ENDDO
          CALL QUICKSORT_I2(IDS, IDX, 1, NSPHIO)
C
!     Loop over /SPH/INOUT
          DO II = 1,NSPHIO
C
            MY_SPHIO = IDX(II)
            TITR(1:nchartitle)=''
            CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,INOM_OPT(22) + MY_SPHIO),LTITR)
            MY_ID = NOM_OPT(1,INOM_OPT(22)+MY_SPHIO)
            IF (LEN_TRIM(TITR) /= 0) THEN
              CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_ID,0.0_8)
            ELSE
              CALL QAPRINT('A_SPH_INOUT_FAKE_NAME',MY_ID,0.0_8)
            END IF
C
            DO I = 1,NISPHIO
              IF (ISPHIO(I,MY_SPHIO) /= 0) THEN
                WRITE(VARNAME,'(A,I0)') 'ISPHIO_',I
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ISPHIO(I,MY_SPHIO),0.0_8)
              END IF
            END DO
C             
            DO I = ISPHIO(4,MY_SPHIO),ISPHIO(4,MY_SPHIO)+LVAD(MY_SPHIO)-1
              IF ( VSPHIO(I) /= ZERO) THEN
                WRITE(VARNAME,'(A,I0)') 'VSPHIO_',I
                TEMP_DOUBLE = VSPHIO(I)
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
              END IF
            ENDDO
C
          END DO ! MY_LOAD=1,NGRAV
        ENDIF
      END IF

C-----------------------------------------------

C-----------------------------------------------

      RETURN
      END
