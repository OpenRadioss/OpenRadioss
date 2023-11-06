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
Chd|====================================================================
Chd|  READ_IMPVEL_FGEO              source/constraints/general/impvel/read_impvel_fgeo.F
Chd|-- called by -----------
Chd|        HM_READ_IMPVEL                source/constraints/general/impvel/hm_read_impvel.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL                       source/starter/freform.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        USR2SYS                       source/system/sysfus.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F
Chd|====================================================================
      SUBROUTINE READ_IMPVEL_FGEO(
     .           NFGEO    ,INUM     ,IOPT     ,FBFVEL   ,IBFVEL   ,
     .           ITAB     ,ITABM1   ,IGRNOD   ,NOM_OPT  ,X0       ,
     .           IXR      ,IPART    ,IPARTR   ,UNITAB   ,LSUBMODEL)
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
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ,INTENT(IN )    :: NFGEO
        INTEGER ,INTENT(INOUT)  :: INUM,IOPT
        INTEGER ,DIMENSION(*)              :: ITAB,ITABM1,IPARTR
        INTEGER ,DIMENSION(LIPART1,*)      :: IPART
        INTEGER ,DIMENSION(NIXR,*)         :: IXR
        INTEGER ,DIMENSION(NIFV,NFXVEL)    :: IBFVEL
        INTEGER ,DIMENSION(LNOPT1,*) ,INTENT(OUT) :: NOM_OPT
        my_real ,DIMENSION(LFXVELR,NFXVEL) :: FBFVEL
        my_real ,DIMENSION(3,NUMNOD) ,INTENT(IN):: X0
        TYPE (UNIT_TYPE_)  ,INTENT(IN) ::  UNITAB
        TYPE (GROUP_)      ,DIMENSION(NGRNOD)  ,INTENT(IN) :: IGRNOD
        TYPE(SUBMODEL_DATA),DIMENSION(*)       ,INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,K,N,N1,N2,NUM0,IFGEO,JPART,NNOD,NOFRAME,
     .     SENS_ID,PART_ID,OPTID,UID,FCT1_ID,FCT2_ID,ILAGM,
     .     FGEO,IDIS,ICOOR,DISTRIBUTION
        INTEGER ,DIMENSION(NUMNOD)   :: NOD1,NOD2
        my_real :: TSTART,XSCALE,YSCALE,FSCAL_T,FSCAL_V,T0,DMIN,DIST,
     .     XI,YI,ZI,XF,YF,ZF
        CHARACTER(ncharkey)    :: KEY
        CHARACTER(nchartitle)  :: TITR,MESS
        LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
        INTEGER  USR2SYS
        EXTERNAL USR2SYS
C-----------------------------------------------
C   D a t a
C-----------------------------------------------
        DATA MESS/'IMPOSED VELOCITY DEFINITION  '/
C======================================================================|
        IS_AVAILABLE = .FALSE.

        NUM0     = INUM+1
c--------------------------------------------------
c
        CALL HM_OPTION_START('/IMPVEL/FGEO')
c
c--------------------------------------------------
        DO IFGEO = 1,NFGEO
c--------------------------------------------------
          CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                            OPTION_ID   = OPTID,
     .                            UNIT_ID     = UID,
     .                            OPTION_TITR = TITR,
     .                            KEYWORD2    = KEY)
c
          IOPT = IOPT + 1
          NOM_OPT(1,IOPT) = OPTID
          CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,IOPT),LTITR)
c
c--------------------------------------------------
          ICOOR    = 0
          FGEO     = 2
          IDIS     = 0
          ILAGM    = 0
          NOFRAME  = 0
c--------------------------------------------------
c       READ STRING VALUES from /IMPVEL
c--------------------------------------------------
c        CALL HM_GET_INTV  ('distribution'  ,DISTRIBUTION ,IS_AVAILABLE,LSUBMODEL)
c
          CALL HM_GET_INTV  ('curveid'        ,FCT1_ID ,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV  ('rad_spring_part',PART_ID ,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV  ('rad_fct_l_id'   ,FCT2_ID ,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV  ('rad_sensor_id'  ,SENS_ID ,IS_AVAILABLE,LSUBMODEL)
c
          CALL HM_GET_FLOATV('xscale'         ,XSCALE  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('rad_t0'         ,T0      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('rad_tstart'     ,TSTART  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('magnitude'      ,YSCALE  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
          CALL HM_GET_FLOATV('rad_dmin'       ,DMIN    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c
          CALL HM_GET_INTV('distribution_table_count' ,NNOD ,IS_AVAILABLE,LSUBMODEL)
          DO I = 1,NNOD
            CALL HM_GET_INT_ARRAY_INDEX('location_unit_node' ,NOD1(I) ,I ,IS_AVAILABLE, LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('rad_node_id'        ,NOD2(I) ,I ,IS_AVAILABLE, LSUBMODEL)
          ENDDO
c
c--------------------------------------------------
c       Default scale factors
c--------------------------------------------------
          IF (T0 <= ZERO) THEN
            CALL ANCMSG(MSGID=1074, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .                  I1=OPTID, C1=TITR, R1=T0)
            CALL HM_GET_FLOATV_DIM('rad_t0'   ,FSCAL_T ,IS_AVAILABLE,LSUBMODEL,UNITAB)
            T0 = ONE * FSCAL_T
          ENDIF
          IF (XSCALE == ZERO) THEN
            CALL HM_GET_FLOATV_DIM('xscale'    ,FSCAL_T ,IS_AVAILABLE,LSUBMODEL,UNITAB)
            XSCALE = ONE * FSCAL_T
          ENDIF
          IF (YSCALE == ZERO) THEN
            CALL HM_GET_FLOATV_DIM('magnitude' ,FSCAL_V ,IS_AVAILABLE,LSUBMODEL,UNITAB)
            YSCALE = ONE * FSCAL_V
          ENDIF
c
          WRITE (IOUT,1000)
c--------------------------------------------------
c       Treatment of explicitly defined nodes
c--------------------------------------------------
          DO J=1,NNOD

            N2 = USR2SYS(NOD2(J),ITABM1,MESS,OPTID)
            XF = X0(1,N2)
            YF = X0(2,N2)
            ZF = X0(3,N2)
c
            IF (NOD1(J) > 0) THEN
              INUM = INUM + 1
              N1 = USR2SYS(NOD1(J),ITABM1,MESS,OPTID)
              XI = X0(1,N1)
              YI = X0(2,N1)
              ZI = X0(3,N1)
              DIST = SQRT((XF-XI)**2 + (YF-YI)**2 + (ZF-ZI)**2)
c
              IBFVEL(1 ,INUM) = N1
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
              IBFVEL(13,INUM) = FGEO
              IBFVEL(14,INUM) = N2
              IBFVEL(15,INUM) = FCT2_ID
c
              FBFVEL(1,INUM)  = DIST / T0
              FBFVEL(2,INUM)  = TSTART
              FBFVEL(3,INUM)  = INFINITY
              FBFVEL(4,INUM)  = ZERO
              FBFVEL(5,INUM)  = XSCALE
              FBFVEL(6,INUM)  = ZERO
              FBFVEL(7,INUM)  = DMIN
              FBFVEL(8,INUM)  = YSCALE
c
              WRITE (IOUT,2000) ITAB(N1),ITAB(N2),FCT1_ID,SENS_ID,FCT2_ID,
     .                          DIST/T0,ONE/XSCALE,TSTART,DMIN,YSCALE
            END IF
          END DO
c--------------------------------------------------
c       Treatment of nodes defined by spring part
c--------------------------------------------------
          IF (PART_ID > 0) THEN
            JPART = 0
            DO N=1,NPART
              IF (IPART(4,N) == PART_ID) JPART = N
            ENDDO
c
            DO N=1,NUMELR
              IF (IPARTR(N) == JPART) THEN
                INUM = INUM + 1
                N1  = IXR(2,N)
                N2  = IXR(3,N)
                XI  = X0(1,N1)
                YI  = X0(2,N1)
                ZI  = X0(3,N1)
                XF  = X0(1,N2)
                YF  = X0(2,N2)
                ZF  = X0(3,N2)
                DIST= SQRT((XF-XI)**2 + (YF-YI)**2 + (ZF-ZI)**2)
c
                IBFVEL(1 ,INUM) = N1
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
                IBFVEL(13,INUM) = FGEO
                IBFVEL(14,INUM) = N2
                IBFVEL(15,INUM) = FCT2_ID
c
                FBFVEL(1,INUM)  = DIST / T0
                FBFVEL(2,INUM)  = TSTART
                FBFVEL(3,INUM)  = INFINITY
                FBFVEL(4,INUM)  = ZERO
                FBFVEL(5,INUM)  = XSCALE
                FBFVEL(6,INUM)  = ZERO
                FBFVEL(7,INUM)  = DMIN
                FBFVEL(8,INUM)  = YSCALE
c
                WRITE (IOUT,2000) ITAB(N1),ITAB(N2),FCT1_ID,SENS_ID,FCT2_ID,
     .                            DIST/T0,ONE/XSCALE,TSTART,DMIN,YSCALE
              END IF
            END DO
          END IF  ! PART_ID > 0
c----------------------------------------------------------------------
c       /IMPVEL/FGEO CALCULE LE NOMBRE D'OCCURENCES D'ONE NOEUD DE DESTINATION
c--------------------------------------------------
          DO N = 1,INUM
            IF (IBFVEL(13,N) /= 2) CYCLE
            N2 = IBFVEL(14,N)
            K  = 1
            DO I = 1,INUM
              IF (I == N) CYCLE
              IF (IBFVEL(13,I) /= 2) CYCLE
              IF (IBFVEL(14,I) == N2) K = K + 1
            END DO
            IBFVEL(16,N) = K
          END DO
c-----------
        END DO  ! IFGEO = 1,NFGEO
c----------------------------------------------------------------------
 1000   FORMAT(//
     .  '     IMPOSED VELOCITIES  PRESCRIBED FINAL GEOMETRY '/
     .  '     ----------------------------------------------'/
     .  '      NODE1      NODE2  VEL_CURVE     SENSOR LOAD_CURVE ',
     .  '      FSCALE           ASCALE       START_TIME           ',
     .  '      DMIN   LOAD_SCALE')
 2000   FORMAT(5(1X,I10),5(1X,1PG16.9))
c----------------------------------------------------------------------
        RETURN
      END
