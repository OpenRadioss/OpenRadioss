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
Chd|  READ_IMPVEL_LAGMUL            source/constraints/general/impvel/read_impvel_lagmul.F
Chd|-- called by -----------
Chd|        HM_READ_IMPVEL                source/constraints/general/impvel/hm_read_impvel.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        KINSET                        source/constraints/general/kinset.F
Chd|        NODGRNR5                      source/starter/freform.F      
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE READ_IMPVEL_LAGMUL(
     .           NLAGMUL  ,INUM     ,IOPT     ,FBFVEL   ,IBFVEL   ,
     .           ITAB     ,ITABM1   ,IGRNOD   ,NOM_OPT  ,X0       ,
     .           IXR      ,IPART    ,IPARTR   ,ISKN     ,IKINE    ,UNITAB  ,LSUBMODEL)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE UNITAB_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "sphcom.inc"
#include      "lagmult.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "submod_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN )    :: NLAGMUL
      INTEGER ,INTENT(INOUT)  :: INUM,IOPT
      INTEGER ,DIMENSION(*)              :: ITAB,ITABM1,IPARTR,IKINE
      INTEGER ,DIMENSION(LIPART1,*)      :: IPART
      INTEGER ,DIMENSION(NIXR,*)         :: IXR
      INTEGER ,DIMENSION(NIFV,NFXVEL)    :: IBFVEL
      INTEGER ,DIMENSION(LISKN,*),INTENT(IN)  :: ISKN
      INTEGER ,DIMENSION(LNOPT1,*) ,INTENT(OUT) :: NOM_OPT
      my_real ,DIMENSION(LFXVELR,NFXVEL) :: FBFVEL
      my_real ,DIMENSION(3,NUMNOD) ,INTENT(IN):: X0
      TYPE (UNIT_TYPE_)  ,INTENT(IN) ::  UNITAB
      TYPE (GROUP_)      ,DIMENSION(NGRNOD)  ,INTENT(IN) :: IGRNOD
      TYPE(SUBMODEL_DATA),DIMENSION(*)       ,INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,N1,N2,NOD,NUM0,ILAGMUL,IUN,JPART,NNOD,NOFRAME,INOD,NOSKEW,
     .   SENS_ID,PART_ID,OPTID,UID,FCT1_ID,FCT2_ID,ILAGM,GRNOD_ID,IGS,LEN,
     .   LAGMUL,IDIS,ICOOR,DISTRIBUTION,SKEW_ID
      INTEGER ,DIMENSION(NUMNOD)   :: NOD1,NOD2,NWORK 
      INTEGER ,DIMENSION(3*NUMNOD) :: IKINE1
      my_real :: XSCALE,YSCALE,FSCAL_T,FSCAL_V,T0,DMIN,DIST,
     .   XI,YI,ZI,XF,YF,ZF,TSTART,TSTOP
      CHARACTER(ncharkey)    :: KEY
      CHARACTER(ncharfield)  :: XYZ
      CHARACTER(nchartitle)  :: TITR,MESS 
      CHARACTER(2)           :: X,Y,Z,XX,YY,ZZ
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NODGRNR5,USR2SYS
      EXTERNAL NODGRNR5,USR2SYS
C-----------------------------------------------
C   D a t a
C-----------------------------------------------
      DATA X  /'X'/
      DATA Y  /'Y'/
      DATA Z  /'Z'/
      DATA XX /'XX'/
      DATA YY /'YY'/
      DATA ZZ /'ZZ'/

      DATA IUN/1/
      DATA MESS/'IMPOSED VELOCITY DEFINITION  '/
C======================================================================|
      IS_AVAILABLE = .FALSE.

      NUM0     = INUM+1
c
      IKINE1(:) = 0
c--------------------------------------------------
c
      CALL HM_OPTION_START('/IMPVEL/LAGMUL')
c
c--------------------------------------------------
      DO ILAGMUL = 1,NLAGMUL
c--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = OPTID,
     .                          UNIT_ID     = UID,
     .                          OPTION_TITR = TITR,
     .                          KEYWORD2    = KEY)
c        
        IOPT = IOPT + 1
        NOM_OPT(1,IOPT) = OPTID
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,IOPT),LTITR)
c        
c--------------------------------------------------
        ICOOR    = 0
        IDIS     = 1
        ILAGM    = 1
        NOFRAME  = 0
        SENS_ID  = 0
        LEN      = 1
        TSTART   = ZERO
        TSTOP    = INFINITY
c--------------------------------------------------
c       READ STRING VALUES from /IMPVEL/LAGMUL
c--------------------------------------------------        
        CALL HM_GET_INTV  ('curveid'   ,FCT1_ID,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_STRING('rad_dir'   ,XYZ      ,ncharfield,IS_AVAILABLE)
        CALL HM_GET_INTV  ('inputsystem'    ,SKEW_ID,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV  ('entityid'       ,GRNOD_ID    ,IS_AVAILABLE,LSUBMODEL)
c        
        CALL HM_GET_FLOATV('xscale'         ,XSCALE  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('magnitude'      ,YSCALE  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c--------------------------------------------------
c       
c--------------------------------------------------
         DO J=0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
            IF (SKEW_ID == ISKN(4,J+1)) THEN               
              NOSKEW = J+1                         
              EXIT                           
           ENDIF                                   
         ENDDO
         IF (SKEW_ID > 0 .and. NOSKEW == 0)
     .      CALL ANCMSG(MSGID=137,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1= OPTID,
     .                  I2= SKEW_ID,
     .                  C1='IMPOSED VELOCITY',
     .                  C2='IMPOSED VELOCITY',
     .                  C3= TITR)


c--------------------------------------------------
        IF (XSCALE == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('xscale'    ,FSCAL_T ,IS_AVAILABLE,LSUBMODEL,UNITAB)
          XSCALE = FSCAL_T
        ENDIF
        IF (YSCALE == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('magnitude' ,FSCAL_V ,IS_AVAILABLE,LSUBMODEL,UNITAB)
          YSCALE =  FSCAL_V
        ENDIF
c
        IF (XYZ(1:2) == XX .OR. XYZ(1:2) == YY .OR. XYZ(1:2) == ZZ) THEN
          LEN = 2
        ENDIF
        WRITE (IOUT,1000)
c       Read NODE numbers from the group
        NNOD = NODGRNR5(GRNOD_ID    ,IGS    ,NWORK,IGRNOD   ,ITABM1 ,MESS   )
               

        NFVLAG = NFVLAG+NNOD
        LAG_NCF = LAG_NCF + NNOD
        LAG_NHF = LAG_NHF + NNOD
        IF(NOSKEW == 0) THEN
           LAG_NKF = LAG_NKF + NNOD
        ELSE
           LAG_NKF = LAG_NKF + NNOD*3
        ENDIF
c--------------------------------------------------
c       Treatment of explicitely defined nodes
c--------------------------------------------------
        DO J=1,NNOD
            INUM = INUM + 1   
            INOD = IABS(NWORK(J))          
            NOD  = ITAB(INOD)
c
            IBFVEL(1 ,INUM) = NWORK(J)
            IBFVEL(2 ,INUM) = 0
            IBFVEL(3 ,INUM) = FCT1_ID
            IBFVEL(4 ,INUM) = SENS_ID
            IBFVEL(5 ,INUM) = 0
            IBFVEL(6 ,INUM) = 0 
            IBFVEL(7 ,INUM) = IDIS
            IBFVEL(8 ,INUM) = ILAGM
            IBFVEL(9 ,INUM) = NOFRAME
            IBFVEL(10,INUM) = ICOOR
            IBFVEL(11,INUM) = 0
            IBFVEL(12,INUM) = IOPT
            IBFVEL(13,INUM) = 0
            IBFVEL(14,INUM) = 0
c

c
            FBFVEL(1,INUM)  = YSCALE
            FBFVEL(2,INUM)  = TSTART
            FBFVEL(3,INUM)  = TSTOP
            FBFVEL(4,INUM)  = ZERO
            FBFVEL(5,INUM)  = ONE/XSCALE
            FBFVEL(6,INUM)  = ZERO

            IF(XYZ(1:2) == XX)THEN
              IBFVEL(2,INUM) = 4 + NOSKEW*10
              CALL KINSET(16,NOD,IKINE(INOD),4,NOSKEW,IKINE1(INOD))
            ELSEIF(XYZ(1:2) == YY)THEN
              IBFVEL(2,INUM) = 5 + NOSKEW*10
              CALL KINSET(16,NOD,IKINE(INOD),5,NOSKEW,IKINE1(INOD))
            ELSEIF(XYZ(1:2) == ZZ)THEN
              IBFVEL(2,INUM) = 6 + NOSKEW*10
              CALL KINSET(16,NOD,IKINE(INOD),6,NOSKEW,IKINE1(INOD))
            ELSEIF (XYZ(1:1) == X)THEN
              IBFVEL(2,INUM)=1 + NOSKEW*10
              CALL KINSET(16,NOD,IKINE(INOD),1,NOSKEW,IKINE1(INOD))
            ELSEIF(XYZ(1:1) == Y)THEN
              IBFVEL(2,INUM) = 2 + NOSKEW*10
              CALL KINSET(16,NOD,IKINE(INOD),2,NOSKEW,IKINE1(INOD))
            ELSEIF(XYZ(1:1) == Z)THEN
              IBFVEL(2,INUM) = 3 + NOSKEW*10
              CALL KINSET(16,NOD,IKINE(INOD),3,NOSKEW,IKINE1(INOD))
            ELSE
               CALL ANCMSG(MSGID=164, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .                    I1=OPTID,
     .                    C1=TITR,
     .                    C2=XYZ)
            ENDIF
c

            WRITE (IOUT,4000) NOD,ISKN(4,NOSKEW),0,XYZ(1:LEN),FCT1_ID,SENS_ID,
     .         YSCALE,XSCALE,TSTART,TSTOP,0
        END DO
c-----------
      END DO  ! 
c----------------------------------------------------------------------	  
 1000 FORMAT(//
     .'     IMPOSED VELOCITIES BY LAGRANGE MULTIPLIERS'/
     .'     ------------------------------------------'/
     .'         NODE         SKEW        FRAME  DIRECTION   LOAD_CURVE',
     .'       SENSOR       FSCALE                ASCALE')

 4000 FORMAT(3X,I10,3X,I10,3X,I10,9X,A2,3X,I10,3X,I10,
     .       2X,1PG20.13,2X,1PG20.13,2X,G20.13,2X,G20.13,16X,I10)
c----------------------------------------------------------------------	  
      RETURN
      END
