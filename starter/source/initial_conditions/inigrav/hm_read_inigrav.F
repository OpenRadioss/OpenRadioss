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
Chd|  HM_READ_INIGRAV               source/initial_conditions/inigrav/hm_read_inigrav.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        INIGRAV                       share/modules1/inigrav_mod.F  
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INIGRAV(IGRV     ,IBUF     ,AGRV     ,ITAB     ,ITABM1   ,
     .                           IGRPART  ,NPC      ,UNITAB   ,ISKN     ,
     .                           ITAGND   ,IGRSURF  ,PLD      ,BUFSF    ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE INIGRAV
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IGRV(NIGRV,*), IBUF(*), ITAB(*), ITABM1(*),NPC(*),
     .        ISKN(LISKN,*),ITAGND(*)
      my_real
     .   AGRV(LFACGRV,*)
      my_real PLD(*), BUFSF(*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRPART)  :: IGRPART
      TYPE (SURF_)   , DIMENSION(NSURF)    :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   FCX,FCY,FAC_M,FAC_L,FAC_T,NGX,NGY,NGZ,DOTPROD
      INTEGER I, NOD, NCUR, NOSKEW,NSKW,NN,IGS,UID,
     .        IAD,NS,IWA,J,K,ID,K1,K2,NCURS,N1,N2,NC,L,
     .        ITAG, IFLAGUNIT,FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP,IADPL
      CHARACTER XYZ*ncharfield, X*1, Y*1, Z*1, XX*2, YY*2, ZZ*2, MESS*40
      CHARACTER TITR*nchartitle
      
      INTEGER     :: IGU,ISU,IGRAV,IBID, IG, IS, IDIR, PN1, ICURS, IIGRAV,IIG,IIS
      my_real     :: BX_,BY_,BZ_, GRAV0,NX,NY,NZ,NORM,PSURF,BID
      LOGICAL     :: lFOUND, lPLANAR_SURF, lUSER_SURF, lOUTP, lGRAV, lUNIQUE, IS_AVAILABLE
      CHARACTER*2 :: CDIR

      INTEGER     :: M,ID_LIST(NINIGRAV)
C-----------------------------------------------
      DATA X/'X'/
      DATA Y/'Y'/
      DATA Z/'Z'/
      DATA XX/'XX'/
      DATA YY/'YY'/
      DATA ZZ/'ZZ'/
      DATA MESS/'INITIAL GRAVITY LOADING DEFINITION      '/
C=======================================================================
C     
      ! Initialization of variable
      lPLANAR_SURF = .FALSE.
      lUSER_SURF   = .FALSE.
      lGRAV        = .TRUE.
      lOUTP        = .TRUE.
      IS_AVAILABLE = .FALSE.
      NGX = ZERO
      NGY = ZERO
      NGZ = ZERO
C
      ! Start reading /INIGRAV card 
      CALL HM_OPTION_START('/INIGRAV')       
C
      ! Loop over /INIGRAV
      DO K=1,NINIGRAV
C
        ! Read title, ID and Unit ID
        TITR = ''   
        CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                          OPTION_ID      = ID, 
     .                          UNIT_ID        = UID,
     .                          OPTION_TITR    = TITR)           
C
        ! Checking unit ID
        IFLAGUNIT = 0
        DO J=1,NUNITS
          IF (UNITAB%UNIT_ID(J) == UID) THEN
            IFLAGUNIT = 1
            EXIT
          ENDIF
        ENDDO
C
        ID_LIST(K)=ID
        lUNIQUE = .TRUE.
        DO M=1,K-1
          IF(ID==ID_LIST(M))THEN
            lUNIQUE=.FALSE.
            EXIT
          ENDIF
        ENDDO 
C
        IF (UID /= 0 .AND. IFLAGUNIT == 0) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I2=UID,I1=ID,
     .                 C1='INITIAL GRAVITY LOADING',
     .                 C2='INITIAL GRAVITY LOADING',
     .                 C3= TITR) 
          lOUTP = .FALSE.
        ENDIF
C        
        ! Reading 1st card : ids
        CALL HM_GET_INTV('grpart_ID',IGU,IS_AVAILABLE,LSUBMODEL)  
        CALL HM_GET_INTV('surf_ID'  ,ISU,IS_AVAILABLE,LSUBMODEL)  
        CALL HM_GET_INTV('grav_ID'  ,IGRAV,IS_AVAILABLE,LSUBMODEL)  
C
        ! Reading 2nd card : pressure, etc
        CALL HM_GET_FLOATV('Pref'   ,PSURF,IS_AVAILABLE, LSUBMODEL, UNITAB)
        ! Only if ISU == 0
        IF (ISU == 0) THEN
          CALL HM_GET_FLOATV('Bx',BX_,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('By',BY_,IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('Bz',BZ_,IS_AVAILABLE, LSUBMODEL, UNITAB)
        ENDIF        
C
        ! Checking Gravity ID
        lFOUND = .FALSE.
        IIGRAV = 0
        GRAV0  = ZERO
        DO IG=1,NGRAV
          IF (IGRAV == IGRV(5,IG)) THEN
            lFOUND = .TRUE.
            IIGRAV = IG
            ICURS  = IGRV(3,IG)
            IF (ICURS > 0) THEN
              PN1 = NPC(ICURS)
              GRAV0   = AGRV(1,IG)*PLD(PN1+1)
            ELSE
              GRAV0   = AGRV(1,IG)
            ENDIF
            IDIR    = MOD(IGRV(2,IG),10)
            NGX = ZERO
            NGY = ZERO
            NGZ = ZERO
            SELECT CASE (IDIR)
              CASE(1)
                CDIR(1:2) =' X'
                NGX       = ONE
              CASE(2)
                CDIR(1:2) =' Y'
                NGY       = ONE
              CASE(3)
                CDIR(1:2) =' Z'
                NGZ       = ONE 
            END SELECT
            CDIR(1:1)="+"
            IF (GRAV0 < ZERO) THEN
              CDIR(1:1)="-"
              NGX = -NGX
              NGY = -NGY
              NGZ = -NGZ
            ENDIF
            EXIT
          ENDIF
        ENDDO
        ! Wrong gravity ID
        IF (.NOT.lFOUND) THEN
          CALL ANCMSG(MSGID=73,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1=ID,
     .                C1=TITR,
     .                I2= IGRAV,
     .                C2='DOES NOT REFER TO A VALID /GRAV ID')
          lOUTP = .FALSE.
          lGRAV = .FALSE.
        ENDIF
        ! Inigrav ID duplicated
        IF (.NOT.lUNIQUE) THEN
          CALL ANCMSG(MSGID=73,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1=ID,
     .                C1=TITR,
     .                I2= ID,
     .                C2='IDENTIFIER IS DUPLICATED')
        ENDIF
C
        ! Checking surface ID
        IIS = 0
        IF (ISU > 0) THEN    
          lfound = .FALSE.
          DO IS=1,NSURF
            IF (ISU == IGRSURF(IS)%ID)THEN
              SELECT CASE(IGRSURF(IS)%TYPE)
               CASE(0)
                IIS = IS
                lUSER_SURF=.TRUE.
                IADPL = IGRSURF(IS)%IAD_BUFR
                lfound = .TRUE.
                BX_  = ZERO
                BY_  = ZERO
                BZ_  = ZERO
                NX   = ZERO
                NY   = ZERO
                NZ   = ZERO
                NX   = ZERO
                NY   = ZERO
                NZ   = ZERO
                EXIT
              CASE(200)
                IIS = IS                                                 
                lPLANAR_SURF=.TRUE.                                            
                IADPL = IGRSURF(IS)%IAD_BUFR                             
                lfound = .TRUE.                                          
                BX_  =  BUFSF(IADPL+1)                                   
                BY_  =  BUFSF(IADPL+2)                                   
                BZ_  =  BUFSF(IADPL+3)                                   
                NX   =  BUFSF(IADPL+4)- BUFSF(IADPL+1)                   
                NY   =  BUFSF(IADPL+5)- BUFSF(IADPL+2)                   
                NZ   =  BUFSF(IADPL+6)- BUFSF(IADPL+3)                   
                NORM = SQRT(NX*NX+NY*NY+NZ*NZ)                           
                NX   = NX / NORM                                         
                NY   = NY / NORM                                         
                NZ   = NZ / NORM                                         
                EXIT                                                                                                       
              END SELECT
            ENDIF
          ENDDO
          ! Wrong surface ID
          IF (.NOT.lFOUND) THEN
            CALL ANCMSG(MSGID=73,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2= ISU,
     .                  C2='DOES NOT REFER TO A VALID /SURF ID')
            lOUTP = .FALSE.
          ENDIF
        ELSE
          NX = NGX
          NY = NGY
          NZ = NGZ
        ENDIF
C
        ! Checking GRPART ID
        lfound = .FALSE.
        IIG = 0
        IAD = NGRNOD+NGRBRIC+NGRQUAD+NGRSHEL+NGRSH3N+NGRTRUS+NGRBEAM+NGRSPRI
        IF (IGU > 0) THEN
          DO IG=1,NGRPART
            IF (IGU == IGRPART(IG)%ID) THEN
              IIG = IG
              lfound = .TRUE.
              EXIT
            ENDIF
          ENDDO
          ! Wrong GRPART ID
          IF (.NOT.lFOUND) THEN
            CALL ANCMSG(MSGID=73,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2= IGU,
     .                  C2='DOES NOT REFER TO A VALID GRPART ')
            lOUTP = .FALSE.
          ENDIF
        ENDIF                                                    
C
        ! Checking the normal
        IF (lPLANAR_SURF .AND. lGRAV) THEN
          DOTPROD = NX*NGX+NY*NGY+NZ*NGZ
          IF(ABS(DOTPROD)<=EM20)THEN
            CALL ANCMSG(MSGID=73,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2= IGRAV,
     .                  C2='REFER TO A GRAVITY DIRECTION COLINEAR TO THE INPUT SURFACE')
            lOUTP = .FALSE.
          ENDIF
        ENDIF
C
        ! Storing IDs in INIGRV table
        INIGRV(1,K)    = IIG
        INIGRV(2,K)    = IIS
        INIGRV(3,K)    = IIGRAV
        INIGRV(4,K)    = ID
C
        ! Storing real data in LINIGRAV table
        LINIGRAV(01,K) = BX_
        LINIGRAV(02,K) = BY_
        LINIGRAV(03,K) = BZ_
        LINIGRAV(04,K) = NX
        LINIGRAV(05,K) = NY
        LINIGRAV(06,K) = NZ
        LINIGRAV(07,K) = GRAV0
        LINIGRAV(08,K) = NGX
        LINIGRAV(09,K) = NGY
        LINIGRAV(10,K) = NGZ
        LINIGRAV(11,K) = PSURF
C
        ! Printout data
        IF (lPLANAR_SURF) THEN
          WRITE (IOUT,2000)
          WRITE (IOUT,FMT='(A)') ''
          WRITE (IOUT,3000) IGU,ISU,IGRAV,BX_,BY_,BZ_, PSURF
          WRITE (IOUT,3001) CDIR(2:2)
          WRITE (IOUT,3002) GRAV0
          IF(lPLANAR_SURF)  WRITE (IOUT,3003) NX,NY,NZ
        ELSEIF(lUSER_SURF)THEN
          WRITE (IOUT,2001)
          WRITE (IOUT,FMT='(A)') ''
          WRITE (IOUT,3005) IGU,ISU,IGRAV, PSURF
          WRITE (IOUT,3001) CDIR(2:2)
          WRITE (IOUT,3002) GRAV0
          IF(lUSER_SURF)WRITE (IOUT,3004)        
        ENDIF
      ENDDO !next K
C-----------
      RETURN
C-----------
 2000 FORMAT(//
     .'     INITIAL GRAVITY LOADING  '/
     .'     -----------------------  '/
     .'   GRPART_ID     SURF_ID     GRAV_ID          BX          BY          BZ       PSURF     ')

 2001 FORMAT(//
     .'     INITIAL GRAVITY LOADING  '/
     .'     -----------------------  '/
     .'   GRPART_ID     SURF_ID     GRAV_ID          PSURF     ')

 3000 FORMAT(2X,I10,2X,I10,2X,I10,2X,E12.4,2X,E12.4,2X,E12.4,2X,E12.4)
 3005 FORMAT(2X,I10,2X,I10,2X,I10,3X,E12.4)
 
 3001 FORMAT('     GRAVITY ORIENTATION : ',1X,A2)
 3002 FORMAT('     GRAVITY VALUE       : ',2X,E12.4)
 3003 FORMAT('     SURFACE ORIENTATION : ',2X,E12.4,2X,E12.4,2X,E12.4)
 3004 FORMAT('     USER DEFINED SURFACE') 
C-----------
      RETURN
      END
