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
Chd|  READ_IMPDISP_FGEO             source/constraints/general/impvel/read_impdisp_fgeo.F
Chd|-- called by -----------
Chd|        HM_READ_IMPVEL                source/constraints/general/impvel/hm_read_impvel.F
Chd|-- calls ---------------
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
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
      SUBROUTINE READ_IMPDISP_FGEO(
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
      INTEGER ,DIMENSION(*)   :: ITAB,ITABM1,IPARTR
      INTEGER ,DIMENSION(LIPART1,*)      ,INTENT(IN)  :: IPART
      INTEGER ,DIMENSION(NIXR,*)         ,INTENT(IN)  :: IXR
      INTEGER ,DIMENSION(LNOPT1,*)       ,INTENT(OUT) :: NOM_OPT
      INTEGER ,DIMENSION(NIFV,NFXVEL)    ,INTENT(OUT) :: IBFVEL
      my_real ,DIMENSION(LFXVELR,NFXVEL) ,INTENT(OUT) :: FBFVEL
      my_real ,DIMENSION(3,NUMNOD) ,INTENT(IN):: X0
      TYPE (UNIT_TYPE_)  ,INTENT(IN) ::  UNITAB
      TYPE (GROUP_)      ,DIMENSION(NGRNOD)  ,INTENT(IN) :: IGRNOD
      TYPE(SUBMODEL_DATA),DIMENSION(*)       ,INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,N,N1,N2,NUM0,IFGEO,JPART,NNOD,NOSKEW,NOFRAME,
     .   SENS_ID,PART_ID,OPTID,FUN_ID,IFRA_OUT,ILAGM,
     .   FGEO,IDIS,ICOOR,DISTRIBUTION
      INTEGER ,DIMENSION(NUMNOD) :: NOD1
      my_real ,DIMENSION(NUMNOD) :: XF,YF,ZF
      my_real :: TSTART,TSTOP,XSCALE,FSCAL_T,FSCAL_V,DIST,XI,YI,ZI,XRF,YRF,ZRF
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
      DATA MESS/'IMPOSED DISPLACEMENT DEFINITION  '/
C======================================================================|
      IS_AVAILABLE = .FALSE.

      NUM0     = INUM+1
c--------------------------------------------------
c     READ /IMPVEL/FGEO cards
c--------------------------------------------------
c
      CALL HM_OPTION_START('/IMPDISP/FGEO')
c
c--------------------------------------------------
      DO IFGEO = 1,NFGEO
c--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = OPTID,
     .                          OPTION_TITR = TITR,
     .                          KEYWORD2    = KEY)
c        
        IOPT = IOPT + 1
        NOM_OPT(1,IOPT) = OPTID
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,IOPT),LTITR)
c        
c--------------------------------------------------
        ICOOR    = 0
        FGEO     = 1
        IDIS     = 0
        ILAGM    = 0
        IFRA_OUT = 0
        NOSKEW   = 0
        NOFRAME  = 0
c--------------------------------------------------
c       READ STRING VALUES from /IMPDISP
c--------------------------------------------------
c        CALL HM_GET_INTV  ('distribution'  ,DISTRIBUTION ,IS_AVAILABLE,LSUBMODEL)
c        
        CALL HM_GET_INTV  ('curveid'        ,FUN_ID ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV  ('rad_spring_part',PART_ID ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV  ('rad_sensor_id'  ,SENS_ID ,IS_AVAILABLE,LSUBMODEL)
c        
        CALL HM_GET_FLOATV('xscale'         ,XSCALE  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('rad_tstart'     ,TSTART  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('rad_tstop'      ,TSTOP   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c
        CALL HM_GET_INTV('distribution_table_count' ,NNOD ,IS_AVAILABLE,LSUBMODEL)
        DO I = 1,NNOD
          CALL HM_GET_INT_ARRAY_INDEX('location_unit_node',NOD1(I),I,IS_AVAILABLE,LSUBMODEL)                             
          CALL HM_GET_FLOAT_ARRAY_INDEX('rad_node_pos_X' ,XF(I) ,I ,IS_AVAILABLE,LSUBMODEL,UNITAB)                             
          CALL HM_GET_FLOAT_ARRAY_INDEX('rad_node_pos_Y' ,YF(I) ,I ,IS_AVAILABLE,LSUBMODEL,UNITAB)                             
          CALL HM_GET_FLOAT_ARRAY_INDEX('rad_node_pos_Z' ,ZF(I) ,I ,IS_AVAILABLE,LSUBMODEL,UNITAB)                             
        ENDDO
c
c--------------------------------------------------
c       Default scale factors
c--------------------------------------------------
        IF (XSCALE == ZERO) THEN
          CALL HM_GET_FLOATV_DIM('xscale'    ,FSCAL_T ,IS_AVAILABLE,LSUBMODEL,UNITAB)
          XSCALE = ONE * FSCAL_T
        ENDIF
        IF (TSTOP == ZERO) TSTOP = INFINITY
c
        WRITE (IOUT,1000)
c--------------------------------------------------
c       Treatment of explicitly defined nodes
c--------------------------------------------------
        DO I=1,NNOD
c
          IF (NOD1(I) > 0) THEN  
            INUM = INUM + 1
            N1 = USR2SYS(NOD1(I),ITABM1,MESS,OPTID)
            XI = X0(1,N1)
            YI = X0(2,N1)
            ZI = X0(3,N1)
            DIST = SQRT((XF(I) - XI)**2 + (YF(I) - YI)**2 + (ZF(I) - ZI)**2)
c
            IBFVEL(1 ,INUM) = N1
            IBFVEL(2 ,INUM) = 0
            IBFVEL(3 ,INUM) = FUN_ID
            IBFVEL(4 ,INUM) = SENS_ID
            IBFVEL(5 ,INUM) = 0
            IBFVEL(6 ,INUM) = 0  ! init dans lecrby (si vitesse de rotation sur main)
            IBFVEL(7 ,INUM) = IDIS
            IBFVEL(8 ,INUM) = ILAGM
            IBFVEL(9 ,INUM) = NOFRAME
            IBFVEL(10,INUM) = ICOOR
            IBFVEL(11,INUM) = 0
            IBFVEL(12,INUM) = IOPT
            IBFVEL(13,INUM) = FGEO
            IBFVEL(14,INUM) = 0
            IBFVEL(15,INUM) = 0
c
            FBFVEL(1,INUM)  = DIST
            FBFVEL(2,INUM)  = TSTART
            FBFVEL(3,INUM)  = TSTOP
            FBFVEL(4,INUM)  = ZERO
            FBFVEL(5,INUM)  = XSCALE
            FBFVEL(6,INUM)  = ZERO
            IF (DIST > ZERO) THEN
              FBFVEL(7,INUM)  = (XF(I) - XI) / DIST
              FBFVEL(8,INUM)  = (YF(I) - YI) / DIST
              FBFVEL(9,INUM)  = (ZF(I) - ZI) / DIST
            ELSE
              FBFVEL(7,INUM)  = ZERO
              FBFVEL(8,INUM)  = ZERO
              FBFVEL(9,INUM)  = ZERO
            END IF
c
            WRITE (IOUT,2000) ITAB(N1),FUN_ID,SENS_ID,
     .                        DIST,ONE/XSCALE,TSTART,TSTOP,XF(I),YF(I),ZF(I)
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
              XRF = X0(1,N2)
              YRF = X0(2,N2)
              ZRF = X0(3,N2)
              DIST= SQRT((XRF-XI)**2 + (YRF-YI)**2 + (ZRF-ZI)**2)
c
              IBFVEL(1 ,INUM) = N1
              IBFVEL(2 ,INUM) = 0
              IBFVEL(3 ,INUM) = FUN_ID
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
              IBFVEL(14,INUM) = 0
              IBFVEL(15,INUM) = 0
              IBFVEL(16,INUM) = 0
c
              FBFVEL(1,INUM)  = DIST
              FBFVEL(2,INUM)  = TSTART
              FBFVEL(3,INUM)  = TSTOP
              FBFVEL(4,INUM)  = ZERO
              FBFVEL(5,INUM)  = XSCALE
              FBFVEL(6,INUM)  = ZERO
              IF (DIST > ZERO) THEN
                FBFVEL(7,INUM)  = (XRF - XI) / DIST
                FBFVEL(8,INUM)  = (YRF - YI) / DIST
                FBFVEL(9,INUM)  = (ZRF - ZI) / DIST
              ELSE
                FBFVEL(7,INUM)  = ZERO
                FBFVEL(8,INUM)  = ZERO
                FBFVEL(9,INUM)  = ZERO
              END IF
c
              WRITE (IOUT,2000) ITAB(N1),FUN_ID,SENS_ID,
     .                          DIST,ONE/XSCALE,TSTART,TSTOP,XRF,YRF,ZRF
            END IF
          END DO
        END IF  ! PART_ID > 0
c----------------------------------------------------------------------    
c       /IMPDISP/FGEO CALCULE LE NOMBRE D'OCCURENCES D'ONE NOEUD DE DESTINATION
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
c--------------------------------------------------
 1000 FORMAT(//
     .'     IMPOSED DISPLACEMENTS  PRESCRIBED FINAL GEOMETRY  '/
     .'     ------------------------------------------------  '/
     .'       NODE LOAD_CURVE     SENSOR           FSCALE     ',
     .'          ASCALE           START_TIME                STOP_TIME',
     .'           X                    Y                    Z' )
 2000 FORMAT(3(1X,I10),3(1X,1PG20.13),4(1X,G20.13))
c--------------------------------------------------
      RETURN
      END
