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
Chd|  ST_QAPRINT_CONSTRAINTS        source/output/qaprint/st_qaprint_constraints.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_CONSTRAINTS(NOM_OPT   ,INOM_OPT  ,NPBY      ,LPBY      ,RBY       ,
     2                                  IBFTEMP   ,FBFTEMP   ,IBFFLUX   ,FBFFLUX   ,ITAB      ,
     3                                  ICODE     ,ISKEW     ,IBCSLAG   ,IBFVEL    ,FBFVEL    ,
     4                                  NIMPDISP  ,NIMPVEL   ,NIMPACC   ,RWBUF     ,NPRW      ,
     5                                  LPRW      ,IBCSCYC   ,IRBE3     ,LRBE3     ,FRBE3     ,
     6                                  MGRBY     ,ISPCOND   ,IRBE2     ,LRBE2     ,NPBYL     ,
     7                                  LPBYL     ,RBYL      ,IBMPC     ,IBMPC2    ,IBMPC3    ,
     8                                  IBMPC4    ,RBMPC     ,LJOINT    ,NNLINK    ,LNLINK,
     9                                  LLINAL    ,LINALE    ,GJBUFI    ,GJBUFR    ,MS        ,
     9                                  IN        ,FXBIPM    ,FXBFILE_TAB)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
      USE R2R_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "lagmult.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
#include      "thermal_c.inc"
#include      "sphcom.inc"
#include      "fxbcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NOM_OPT(LNOPT1,SNOM_OPT1), INOM_OPT(SINOM_OPT)
      INTEGER, INTENT(IN) :: ITAB(NUMNOD)
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
      INTEGER, INTENT(IN) :: NIMPDISP,NIMPVEL,NIMPACC
      INTEGER, INTENT(IN) :: NPBY(NNPBY,NRBYKIN), NPBYL(NNPBY,NRBYLAG), 
     .                       LPBY(*), LPBYL(*), IBCSCYC(4,NBCSCYC)
      INTEGER, INTENT(IN) :: IBFTEMP(NIFT,NFXTEMP), IBFFLUX(NITFLUX,NFXFLUX)
      INTEGER, INTENT(IN) :: ICODE(NUMNOD), ISKEW(NUMNOD),IBFVEL(NIFV,NFXVEL)
      INTEGER, INTENT(IN) :: IBCSLAG(5,NBCSLAG),NPRW(NRWALL,NNPRW),LPRW(SLPRW)
      INTEGER, INTENT(IN) :: IRBE3(NRBE3L,NRBE3), LRBE3(SLRBE3)
      INTEGER, INTENT(IN) :: IRBE2(NRBE2L,NRBE2), LRBE2(SLRBE2)
      INTEGER, INTENT(IN) :: NNLINK(10,SNNLINK), LNLINK(SLNLINK)
      INTEGER, DIMENSION(NRWALL) :: IDX, IDS
      INTEGER, DIMENSION(NFXVEL) :: IDXVEL, IDSVEL
      INTEGER, DIMENSION(NFXBODY) :: IDXFX, IDSFX
      INTEGER, INTENT(IN) :: MGRBY(NMGRBY,SMGRBY)
      INTEGER, INTENT(IN) :: ISPCOND(NISPCOND,*),LJOINT(*),GJBUFI(LKJNI,*)
      INTEGER, INTENT(IN) :: IBMPC(NUMMPC),IBMPC2(LMPC),IBMPC3(LMPC),IBMPC4(LMPC)
      my_real, INTENT(IN) ::
     .                       RBY(NRBY,NRBYKIN),RBYL(NRBY,NRBYLAG),FRBE3(6,*),GJBUFR(LKJNR,*),MS(*),IN(*)
      my_real, INTENT(IN) ::  
     .                       FBFTEMP(LFACTHER,NFXTEMP), FBFFLUX(LFACTHER,NFXFLUX),
     .                       FBFVEL(LFXVELR,NFXVEL)
      my_real, INTENT(IN) ::
     .                       RWBUF(NRWLP,NRWALL)
      my_real, INTENT(IN) ::
     .                       RBMPC(SRBMPC)
      INTEGER, INTENT(IN) :: LLINAL
      INTEGER, DIMENSION(LLINAL), INTENT(IN) :: LINALE
      INTEGER, INTENT(IN) :: FXBIPM(NBIPM,NFXBODY)
      CHARACTER, DIMENSION(NFXBODY) :: FXBFILE_TAB*2148
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, MY_ID, MY_RBODY, MY_CONSTRAINT, MY_NODE, MY_RWALL, POSI(NRWALL+1),
     .        MY_MERGE, TNSL, K, NS, MY_FXBODY
      CHARACTER *nchartitle TITR
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE
      INTEGER IADS
C-----------------------------------------------
C     Rigid Bodies
C-----------------------------------------------
      IF (MYQAKEY('/RBODY')) THEN
        DO MY_RBODY=1,NRBYKIN
C
          CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,MY_RBODY),LTITR)
          MY_ID = NPBY(6,MY_RBODY)
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_ID,0.0_8)
          ELSE
            CALL QAPRINT('A_RIGID_BODY_FAKE_NAME',MY_ID,0.0_8)
          END IF
C
          DO I=1,NNPBY
            IF(NPBY(I,MY_RBODY) /=0)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'NPBY_',I      
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NPBY(I,MY_RBODY),0.0_8)
            END IF
          END DO
C
          DO I=NPBY(11,MY_RBODY)+1,NPBY(11,MY_RBODY)+NPBY(2,MY_RBODY)
C
C           VARNAME: variable name in ref.extract (without blanks)
            WRITE(VARNAME,'(A,I0)') 'LPBY_',I      
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),LPBY(I),0.0_8)
          END DO
C
          DO I=1,NRBY
            IF(RBY(I,MY_RBODY)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'RBY_',I
              TEMP_DOUBLE = RBY(I,MY_RBODY)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C
        END DO ! MY_RBODY=1,NRBYKIN
C-------
        TNSL=0
        DO MY_RBODY=1,NRBYLAG
C
          CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,NRBYKIN+MY_RBODY),LTITR)
          MY_ID = NPBYL(6,MY_RBODY)
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_ID,0.0_8)
          ELSE
            CALL QAPRINT('A_RIGID_BODY_FAKE_NAME',MY_ID,0.0_8)
          END IF
C
          DO I=1,NNPBY
            IF(NPBYL(I,MY_RBODY) /=0)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'NPBYL_',I      
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NPBYL(I,MY_RBODY),0.0_8)
            END IF
          END DO
C
          DO I=1,NPBYL(2,MY_RBODY)-1
C
C           VARNAME: variable name in ref.extract (without blanks)
            WRITE(VARNAME,'(A,I0)') 'LPBYL_',I      
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ITAB(LPBYL(TNSL+I)),0.0_8)
          END DO
C
          DO I=1,NRBY
            IF(RBYL(I,MY_RBODY)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'RBYL_',I
              TEMP_DOUBLE = RBYL(I,MY_RBODY)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C
          TNSL=TNSL+3*NPBYL(2,MY_RBODY)
        END DO ! MY_RBODY=1,NRBODY
      END IF
C-----------------------------------------------
C     BCS
C-----------------------------------------------
      IF (MYQAKEY('/BCS') .OR. MYQAKEY('/ALE/BCS')) THEN
        DO MY_NODE=1,NUMNOD
C
          MY_ID = ITAB(MY_NODE)
C
          IF(ICODE(MY_NODE)/=0)THEN
C
C           VARNAME: variable name in ref.extract (without blanks)
            WRITE(VARNAME,'(A,I0,I0)') 'ICODE_',MY_ID
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ICODE(MY_NODE),0.0_8)
          END IF
C
          IF(ISKEW(MY_NODE)/=0)THEN
C
C           VARNAME: variable name in ref.extract (without blanks)
            WRITE(VARNAME,'(A,I0,I0)') 'ISKEW_',MY_ID
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ISKEW(MY_NODE),0.0_8)
          END IF
C
        END DO ! MY_NODE=1,NUMNOD
      END IF
C-----------------------------------------------
C     /IMPTEMP
C-----------------------------------------------
      IF (MYQAKEY('/IMPTEMP')) THEN
        DO MY_CONSTRAINT=1,NFXTEMP
C
C         Title of the option was not stored in NOM_OPT ... TBD
C         and Imptemp ID is not stored 
          TITR(1:nchartitle)=''
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_CONSTRAINT,0.0_8)
          ELSE
            CALL QAPRINT('A_IMPTEMP_FAKE_NAME',MY_CONSTRAINT,0.0_8)
          END IF
C
          DO I=1,NIFT
            IF(IBFTEMP(I,MY_CONSTRAINT) /=0)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'IBFTEMP_',I      ! IBFTEMP(11) => 'IBFTEMP_11'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBFTEMP(I,MY_CONSTRAINT),0.0_8)
            END IF
          END DO
C
          DO I=1,LFACTHER
            IF(FBFTEMP(I,MY_CONSTRAINT)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'FBFTEMP_',I
              TEMP_DOUBLE = FBFTEMP(I,MY_CONSTRAINT)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C
        END DO ! MY_CONSTRAINT=1,NFXTEMP
      END IF
C-----------------------------------------------
C     /IMPDISP
C-----------------------------------------------
      IF (MYQAKEY('/IMPDISP')) THEN
        DO MY_CONSTRAINT=1,NIMPDISP
C
          TITR(1:nchartitle)=''
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_CONSTRAINT,0.0_8)
          ELSE
            CALL QAPRINT('A_IMPACC_FAKE_NAME',MY_CONSTRAINT,0.0_8)
          END IF
C
          DO I=1,NIFV
            IF (IBFVEL(I,MY_CONSTRAINT) /=0) THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'IBFVEL_',I
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBFVEL(I,MY_CONSTRAINT),0.0_8)
            END IF
          END DO
C
          DO I=1,LFXVELR
            IF(FBFVEL(I,MY_CONSTRAINT)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'FBFVEL_',I
              TEMP_DOUBLE = FBFVEL(I,MY_CONSTRAINT)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C
        END DO ! MY_CONSTRAINT=NFXVEL-NIMPACC+1,NFXVEL
      END IF
C-----------------------------------------------
C     /IMPVEL
C-----------------------------------------------
      IF (MYQAKEY('/IMPVEL')) THEN
        DO MY_CONSTRAINT=NIMPDISP+1,NIMPDISP+NIMPVEL
C
          TITR(1:nchartitle)=''
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_CONSTRAINT,0.0_8)
          ELSE
            CALL QAPRINT('A_IMPACC_FAKE_NAME',MY_CONSTRAINT,0.0_8)
          END IF
C
          DO I=1,NIFV
            IF (IBFVEL(I,MY_CONSTRAINT) /=0) THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'IBFVEL_',I
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBFVEL(I,MY_CONSTRAINT),0.0_8)
            END IF
          END DO
C
          DO I=1,LFXVELR
            IF(FBFVEL(I,MY_CONSTRAINT)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'FBFVEL_',I
              TEMP_DOUBLE = FBFVEL(I,MY_CONSTRAINT)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C
        END DO ! MY_CONSTRAINT=NFXVEL-NIMPACC+1,NFXVEL
      END IF
C-----------------------------------------------
C     /IMPACC
C-----------------------------------------------
      IF (MYQAKEY('/IMPACC')) THEN
        DO MY_CONSTRAINT=NFXVEL-NIMPACC+1,NFXVEL
C
C         Title of the option was not stored in NOM_OPT ... TBD
C         and Impvel ID is not stored 
          TITR(1:nchartitle)=''
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_CONSTRAINT,0.0_8)
          ELSE
            CALL QAPRINT('A_IMPACC_FAKE_NAME',MY_CONSTRAINT,0.0_8)
          END IF
C
          DO I=1,NIFV
            IF (IBFVEL(I,MY_CONSTRAINT) /=0) THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'IBFVEL_',I
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBFVEL(I,MY_CONSTRAINT),0.0_8)
            END IF
          END DO
C
          DO I=1,LFXVELR
            IF(FBFVEL(I,MY_CONSTRAINT)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'FBFVEL_',I
              TEMP_DOUBLE = FBFVEL(I,MY_CONSTRAINT)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C
        END DO ! MY_CONSTRAINT=NFXVEL-NIMPACC+1,NFXVEL
      END IF
C-----------------------------------------------
C     /IMPFLUX
C-----------------------------------------------
      IF (MYQAKEY('/IMPFLUX')) THEN
        DO MY_CONSTRAINT=1,NFXFLUX
C
C         Title of the option was not stored in NOM_OPT ... TBD
C         and Impflux ID is not stored 
          TITR(1:nchartitle)=''
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_CONSTRAINT,0.0_8)
          ELSE
            CALL QAPRINT('A_IMPFLUX_FAKE_NAME',MY_CONSTRAINT,0.0_8)
          END IF
C
          DO I=1,NITFLUX
            IF(IBFFLUX(I,MY_CONSTRAINT) /=0)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'IBFFLUX_',I      ! IBFFLUX(11) => 'IBFFLUX_11'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBFFLUX(I,MY_CONSTRAINT),0.0_8)
            END IF
          END DO
C
          DO I=1,LFACTHER
            IF(FBFFLUX(I,MY_CONSTRAINT)/=ZERO)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'FBFFLUX_',I
              TEMP_DOUBLE = FBFFLUX(I,MY_CONSTRAINT)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF
          END DO
C
        END DO ! MY_CONSTRAINT=1,NFXFLUX
      END IF
C-----------------------------------------------
C     /BCS/LAGMUL
C-----------------------------------------------
      IF (MYQAKEY('/BCS/LAGMUL')) THEN
        DO MY_CONSTRAINT=1,NBCSLAG
C
          TITR(1:nchartitle)=''
          MY_ID = IBCSLAG(5,MY_CONSTRAINT)
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_ID,0.0_8)
          ELSE
            CALL QAPRINT('A_BCS_LAGMUL_FAKE_NAME',MY_ID,0.0_8)
          END IF
C
          DO I=1,5
C
            IF(IBCSLAG(I,MY_CONSTRAINT)/=0)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0,I0)') 'IBCSLAG_',I      ! IBCSLAG(11) => 'IBCSLAG_11'
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBCSLAG(I,MY_CONSTRAINT),0.0_8)
            END IF
C
          END DO
C
        END DO ! MY_CONSTRAINT=1,NBCSLAG
      END IF
C-----------------------------------------------
C     /BCS/CYCLIC
C-----------------------------------------------
      IF (MYQAKEY('/BCS/CYCLIC')) THEN
        DO MY_CONSTRAINT=1,NBCSCYC
C
          TITR(1:nchartitle)=''
          MY_ID = IBCSCYC(4,MY_CONSTRAINT)
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_ID,0.0_8)
          ELSE
            CALL QAPRINT('A_BCS_CYCLIC_FAKE_NAME',MY_ID,0.0_8)
          END IF
C
          DO I=1,4
C
            IF(IBCSCYC(I,MY_CONSTRAINT)/=0)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0,I0)') 'IBCSCYC_',I      
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBCSCYC(I,MY_CONSTRAINT),0.0_8)
            END IF
C
          END DO
C
        END DO ! MY_CONSTRAINT=1,NBCSCYC
      END IF
C-----------------------------------------------
C     /RWALL
C-----------------------------------------------
      IF (MYQAKEY('/RWALL')) THEN
        IF (NRWALL > 0) THEN
C          
!     Sort by ID to ensure internal order independant output
          POSI(1) = 1
          DO I = 1, NRWALL
            IDS(I)    = NOM_OPT(LNOPT1*INOM_OPT(5)+1,I)
            IDX(I)    = I
            POSI(I+1) = POSI(I) + NPRW(I,1)+INT(RWBUF(8,I))
          ENDDO
          CALL QUICKSORT_I2(IDS, IDX, 1, NRWALL)
C         
!     Loop over RWALLs
          DO II = 1,NRWALL
C
            MY_RWALL = IDX(II)
            TITR(1:nchartitle)=''
            CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,MY_RWALL),LTITR)
            MY_ID = NOM_OPT(1,MY_RWALL + INOM_OPT(5))
            IF (LEN_TRIM(TITR) /= 0) THEN
              CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_ID,0.0_8)
            ELSE
              CALL QAPRINT('A_RWALL_FAKE_NAME',MY_ID,0.0_8)
            END IF
C
            DO I = 1,NNPRW
              IF (NPRW(MY_RWALL,I) /= 0) THEN
C
C               VARNAME: variable name in ref.extract (without blanks)
                WRITE(VARNAME,'(A,I0)') 'NPRW_',I
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NPRW(MY_RWALL,I),0.0_8)
              END IF
            END DO
C
            DO I = 1,NRWLP        
              IF (RWBUF(I,MY_RWALL) /= ZERO) THEN
C
C               VARNAME: variable name in ref.extract (without blanks)
                WRITE(VARNAME,'(A,I0)') 'RWBUF_',I
                TEMP_DOUBLE = RWBUF(I,MY_RWALL)
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
              END IF
            END DO
C
            DO I = POSI(MY_RWALL),POSI(MY_RWALL+1)-1
              IF (LPRW(I) /= 0) THEN
C
C           VARNAME: variable name in ref.extract (without blanks)
                WRITE(VARNAME,'(A,I0)') 'LPRW_',I-POSI(MY_RWALL)+1
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),LPRW(I),0.0_8)
              END IF
            END DO
C
          END DO
C    
        ENDIF
      ENDIF
C-----------------------------------------------
C     RBE3
C-----------------------------------------------
      IF (MYQAKEY('/RBE3')) THEN
        IADS = SLRBE3/2
        DO MY_CONSTRAINT=1,NRBE3
C
          CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,MY_CONSTRAINT + INOM_OPT(14)),LTITR)
          MY_ID = IRBE3(2,MY_CONSTRAINT)
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_ID,0.0_8)
          ELSE
            CALL QAPRINT('A_RBE3_FAKE_NAME',MY_ID,0.0_8)
          END IF
C
          DO I=1,NRBE3L
            IF(IRBE3(I,MY_CONSTRAINT) /=0)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'IRBE3_',I      
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IRBE3(I,MY_CONSTRAINT),0.0_8)
            END IF
          END DO
C
          DO I=IRBE3(1,MY_CONSTRAINT)+1,IRBE3(1,MY_CONSTRAINT)+IRBE3(5,MY_CONSTRAINT)
C
C           VARNAME: variable name in ref.extract (without blanks)
            WRITE(VARNAME,'(A,I0)') 'LRBE3_',I      
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),LRBE3(I),0.0_8)
          END DO
C          
          DO I=IRBE3(1,MY_CONSTRAINT)+1,IRBE3(1,MY_CONSTRAINT)+IRBE3(5,MY_CONSTRAINT)
C
C           VARNAME: variable name in ref.extract (without blanks)
            WRITE(VARNAME,'(A,I0)') 'LRBE3s_',I      
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),LRBE3(I+IADS),0.0_8)
          END DO
C          
          DO I=IRBE3(1,MY_CONSTRAINT)+1,IRBE3(1,MY_CONSTRAINT)+IRBE3(5,MY_CONSTRAINT)
C
            DO II = 1,6
              IF(FRBE3(II,I) /=ONE.AND.FRBE3(II,I) /=ZERO)THEN
                WRITE(VARNAME,'(A,I1,A,I0)') 'FRBE3_',II,'_',I
                TEMP_DOUBLE = FRBE3(II,I)
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
              END IF
            END DO !II = 1,6
          END DO
C
        END DO ! MY_CONSTRAINT=1,NRBE3
      END IF
C-----------------------------------------------
C     Merge Rigid Bodies
C-----------------------------------------------
      IF (MYQAKEY('/MERGE')) THEN
        II = 1
        DO MY_CONSTRAINT=1,NRBMERGE
C
          CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,INOM_OPT(30)+MY_CONSTRAINT),LTITR)
          MY_ID = MGRBY(6,II)
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_ID,0.0_8)
          ELSE
            CALL QAPRINT('A_MERGE_FAKE_NAME',MY_ID,0.0_8)
          END IF
C
          DO MY_MERGE=II,SMGRBY
            IF(MGRBY(6,MY_MERGE) /= MY_ID) THEN
              II = MY_MERGE
              EXIT
            ENDIF
            DO I=1,NMGRBY
              IF(MGRBY(I,MY_MERGE) /=0)THEN
C               VARNAME: variable name in ref.extract (without blanks)
                WRITE(VARNAME,'(A,I0)') 'MGRBY_',I      
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),MGRBY(I,MY_MERGE),0.0_8)
              END IF
            END DO
          END DO ! MY_MERGE=II,SMGRBY

        END DO ! MY_CONSTRAINT=1,NRBMERGE
      END IF
C-----------------------------------------------
C     /SPHBCS
C-----------------------------------------------
      IF (MYQAKEY('/SPHBCS')) THEN
        DO MY_CONSTRAINT=1,NSPCOND
CC
          TITR(1:nchartitle)=''
          MY_ID = ISPCOND(4,MY_CONSTRAINT)
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_ID,0.0_8)
          ELSE
            CALL QAPRINT('A_SPHBCS_FAKE_NAME',MY_ID,0.0_8)
          END IF
C
          DO I=1,NISPCOND
            IF(ISPCOND(I,MY_CONSTRAINT)/=0)THEN
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0,I0)') 'ISPCOND_',I      
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ISPCOND(I,MY_CONSTRAINT),0.0_8)
            END IF
C
          END DO
C
        END DO ! MY_CONSTRAINT=1,NSPCOND
      END IF
C-----------------------------------------------
C     /RBE2
C-----------------------------------------------
      IF (MYQAKEY('/RBE2')) THEN
        DO MY_CONSTRAINT=1,NRBE2
C
          CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,MY_CONSTRAINT + INOM_OPT(13)),LTITR)
          MY_ID = IRBE2(2,MY_CONSTRAINT)
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_ID,0.0_8)
          ELSE
            CALL QAPRINT('A_RBE2_FAKE_NAME',MY_ID,0.0_8)
          END IF
C
          DO I=1,NRBE2L
            IF(IRBE2(I,MY_CONSTRAINT) /=0)THEN
C
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'IRBE2_',I      
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IRBE2(I,MY_CONSTRAINT),0.0_8)
            END IF
          END DO
C
          DO I=IRBE2(1,MY_CONSTRAINT)+1,IRBE2(1,MY_CONSTRAINT)+IRBE2(5,MY_CONSTRAINT)
C
C           VARNAME: variable name in ref.extract (without blanks)
            WRITE(VARNAME,'(A,I0)') 'LRBE2_',I  
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),LRBE2(I),0.0_8)
          END DO
C
        END DO ! MY_CONSTRAINT=1,NRBE2
      END IF
C-----------------------------------------------
C     /MPC
C-----------------------------------------------
      IF (MYQAKEY('/MPC')) THEN
        II=0
        DO MY_CONSTRAINT=1,NUMMPC
C
          CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,INOM_OPT(17) + MY_CONSTRAINT),LTITR)

          MY_ID = NOM_OPT(1,INOM_OPT(17)+MY_CONSTRAINT)
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_ID,0.0_8)
          ELSE
            CALL QAPRINT('A_MPC_FAKE_NAME',MY_ID,0.0_8)
          END IF
C
          DO I=1,IBMPC(MY_CONSTRAINT)

            IF(IBMPC2(II+I) /=0)THEN
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'NOD_',I      
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBMPC2(II+I),0.0_8)
            END IF

            IF(IBMPC3(II+I) /=0)THEN
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'IDOF_',I      
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBMPC3(II+I),0.0_8)
            END IF

            IF(IBMPC4(II+I) /=0)THEN
C             VARNAME: variable name in ref.extract (without blanks)
              WRITE(VARNAME,'(A,I0)') 'ISKEW_',I      
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),IBMPC4(II+I),0.0_8)
            END IF

            IF(RBMPC(II+I) /=0)THEN
              WRITE(VARNAME,'(A,I1,A,I0)') 'ALPHA_',I
              TEMP_DOUBLE = RBMPC(II+I)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            END IF

          END DO
          II = II + IBMPC(MY_CONSTRAINT)
C
        END DO ! MY_CONSTRAINT=1,NUMMPC
      END IF
C-----------------------------------------------
C     /CYL_JOINT
C-----------------------------------------------
      IF (MYQAKEY('/CYL_JOINT')) THEN
C
        II = 1
C
        DO MY_CONSTRAINT=1,NJOINT
C
          CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,INOM_OPT(7)+MY_CONSTRAINT),LTITR)
          MY_ID = NOM_OPT(1,INOM_OPT(7)+MY_CONSTRAINT)
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_ID,0.0_8)
          ELSE
            CALL QAPRINT('A_CYLJOINT_FAKE_NAME',MY_ID,0.0_8)
          END IF
C
          NS = LJOINT(II)
C
          DO I=1,NS
            WRITE(VARNAME,'(A,I0)') 'NOD_',I      
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),ITAB(LJOINT(II+I)),0.0_8)
          ENDDO
C
          II=II+NS+1
C
        END DO 
      END IF
C-----------------------------------------------
C     /GJOINT
C-----------------------------------------------
      IF (MYQAKEY('/GJOINT')) THEN
C
        DO MY_CONSTRAINT=1,NGJOINT
C
          CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,INOM_OPT(18) + MY_CONSTRAINT),LTITR)
          MY_ID = NOM_OPT(1,INOM_OPT(18)+MY_CONSTRAINT)
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_ID,0.0_8)
          ELSE
            CALL QAPRINT('A_GJOINT_FAKE_NAME',MY_ID,0.0_8)
          END IF
C
          DO I=1,LKJNI
            WRITE(VARNAME,'(A,I0)') 'GJBUFI_',I      
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),GJBUFI(I,MY_CONSTRAINT),0.0_8)
          ENDDO
C
          DO I=1,LKJNR
            WRITE(VARNAME,'(A,I0)') 'GJBUFR_',I
            TEMP_DOUBLE = GJBUFR(I,MY_CONSTRAINT)
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          ENDDO
C
          DO I=1,4
            WRITE(VARNAME,'(A,I0)') 'MASS_',I
            IF (GJBUFI(2+I,MY_CONSTRAINT) > 0) THEN
              TEMP_DOUBLE = MS(GJBUFI(2+I,MY_CONSTRAINT))
            ELSE
              TEMP_DOUBLE = ZERO             
            ENDIF
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          ENDDO
C
          DO I=1,4
            WRITE(VARNAME,'(A,I0)') 'INER_',I
            IF (GJBUFI(2+I,MY_CONSTRAINT) > 0) THEN
              TEMP_DOUBLE = IN(GJBUFI(2+I,MY_CONSTRAINT))
            ELSE
              TEMP_DOUBLE = ZERO             
            ENDIF
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          ENDDO
C
        END DO 
      END IF
C-----------------------------------------------
C     RLINK 
C-----------------------------------------------
      IF (MYQAKEY('/RLINK')) THEN
C      
        IF (NLINK > 0) THEN 
C          
C         
          DO MY_CONSTRAINT = 1, NLINK
c            
           CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,INOM_OPT(9)+MY_CONSTRAINT),LTITR)
           MY_ID = NOM_OPT(1,INOM_OPT(9)+MY_CONSTRAINT)
           IF(LEN_TRIM(TITR)/=0)THEN
             CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_ID,0.0_8)
           ELSE
             CALL QAPRINT('A_RLINK_NAME',MY_ID,0.0_8)
           END IF
c
           DO I = 1,10
            IF(NNLINK(I,MY_CONSTRAINT) /=0)THEN
              WRITE(VARNAME,'(A,I0)') 'NNLINK_',I    
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),NNLINK(I,MY_CONSTRAINT),0.0_8)
            END IF
           ENDDO
c
          ENDDO
          DO I = 1,SLNLINK
           IF(LNLINK(I) /=0)THEN
              WRITE(VARNAME,'(A,I0)') 'LNLINK_',I    
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),LNLINK(I),0.0_8)
           END IF
          ENDDO


        ENDIF
      END IF
C-----------------------------------------------
C     /ALE/LINK 
C-----------------------------------------------
      IF (MYQAKEY('/ALE/LINK')) THEN
         DO II = 1, LLINAL
            WRITE(VARNAME,'(A,I0)') 'LINALE_', II
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),LINALE(II),0.0_8)
         ENDDO
      ENDIF
C-----------------------------------------------
C     /FXBODY
C-----------------------------------------------
      IF (MYQAKEY('/FXBODY')) THEN
        IF (NFXBODY > 0) THEN 
C          
!     Sort by ID to ensure internal order independant output
          DO I = 1, NFXBODY
            IDSFX(I)    = FXBIPM(1,I)
            IDXFX(I)    = I
          ENDDO
          CALL QUICKSORT_I2(IDSFX, IDXFX, 1, NFXBODY)
C         
!     Loop over FXBODY
          DO II = 1,NFXBODY
C
            MY_FXBODY = IDXFX(II)
            TITR(1:nchartitle)=''
            CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,INOM_OPT(11)+MY_FXBODY),LTITR)
            MY_ID = NOM_OPT(1,INOM_OPT(11)+MY_FXBODY)
            IF (LEN_TRIM(TITR) /= 0) THEN
              CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_ID,0.0_8)
            ELSE
              CALL QAPRINT('A_FXBODY_FAKE_NAME',MY_ID,0.0_8)
            END IF
C
            DO I = 1,NBIPM
              IF (FXBIPM(I,MY_FXBODY) /= 0) THEN
                WRITE(VARNAME,'(A,I0)') 'FXBIPM_',I
                CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),FXBIPM(I,MY_FXBODY),0.0_8)      
              ENDIF
            ENDDO
C
            CALL QAPRINT('FXBODY_FILE_NAME',0,0.0_8)
            CALL QAPRINT(FXBFILE_TAB(MY_FXBODY)(1:LEN_TRIM(FXBFILE_TAB(MY_FXBODY))),0,0.0_8)
C         
          ENDDO
        ENDIF
      ENDIF
C-----------------------------------------------
      RETURN
      END
