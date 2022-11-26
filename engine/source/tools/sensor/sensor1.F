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
Chd|  SENSOR1                       source/tools/sensor/sensor1.F 
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        SENSOR_ACC                    source/tools/sensor/sensor_acc.F
Chd|        SENSOR_CONTACT                source/tools/sensor/sensor_contact.F
Chd|        SENSOR_DIST                   source/tools/sensor/sensor_dist.F
Chd|        SENSOR_DIST_SURF              source/tools/sensor/sensor_dist_surf.F
Chd|        SENSOR_ENERGY                 source/tools/sensor/sensor_energy.F
Chd|        SENSOR_GAUGE                  source/tools/sensor/sensor_gauge.F
Chd|        SENSOR_HIC                    source/tools/sensor/sensor_hic.F
Chd|        SENSOR_NIC                    source/tools/sensor/sensor_nic.F
Chd|        SENSOR_RBODY                  source/tools/sensor/sensor_rbody.F
Chd|        SENSOR_RWALL                  source/tools/sensor/sensor_rwall.F
Chd|        SENSOR_SECTION                source/tools/sensor/sensor_section.F
Chd|        SENSOR_TEMP                   source/tools/sensor/sensor_temp.F
Chd|        SENSOR_VEL                    source/tools/sensor/sensor_vel.F
Chd|        SENSOR_WORK                   source/tools/sensor/sensor_work.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SENSOR1(SENSOR_PTR,SENS1     ,XSENS    ,
     .                   X         ,V         ,A        ,IPARI    ,NPRW    ,
     .                   FSAV      ,GAUGE     ,DIMFB    ,TABS     ,FBSAV6  ,
     .                   STABS     ,IPARSENS  ,ISENS    ,PARTSAV2 ,
     .                   SUBSET    ,ACC       ,IGRSURF  ,IGRNOD   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER DIMFB,STABS,IPARSENS,ISENS
      INTEGER IPARI(NPARI,NINTER),NPRW(*),TABS(STABS)
      my_real ACC(LLACCELM,*),X(3,NUMNOD),V(3,NUMNOD),A(3,NUMNOD),
     . FSAV(NTHVKI,*), GAUGE(LLGAUGE,*),PARTSAV2(2,*),XSENS(12),SENS1(*)
      DOUBLE PRECISION FBSAV6(12,6,DIMFB)
      TYPE (SUBSET_) ,DIMENSION(NSUBS)  ,INTENT(IN) :: SUBSET
      TYPE(SURF_)    ,DIMENSION(NSURF)  ,INTENT(IN) :: IGRSURF
      TYPE(GROUP_)   ,DIMENSION(NGRNOD) ,INTENT(IN) :: IGRNOD
      TYPE (SENSOR_STR_)  :: SENSOR_PTR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER TYP, I,J,K,K0,IAD,ACTIVE,ACT1,ACT2,IDIR,NSENS,N1,N2,IACC,
     . IN,IS,N5,IGAU,ISECT,ID_INTER,IRWAL,IRBOD,IPART,ISUBS,NP,M
      my_real
     . AMIN, DURE, TACT,TACT1,TACT2,TDELAY, TSTART, DD, DM1, DM2,
     . V1,V2,V3,FV1,FV2,FV,VV,VV1,VV2,FREQ,VEL,TSTOPS,
     . FX,FY,FZ,FF,FMIN,FMAX,WMAX,DX,DY,DZ,WORK,IMP,EINT,EKIN,IEMIN,
     . IEMAX,KEMIN,KEMAX,IETOL,IETIME,KETOL,KETIME,EI_SAV,EK_SAV
       INTEGER :: NGR,NBR_GROUP,L,ITSK

       DOUBLE PRECISION LTT_DP,DT2_DP,TDELAY_DP,NEXT_CYCLE
C=======================================================================
C INITIALIALISATION DES COEFFICIENTS DU FILTRE
C-----------------------------------------------
      TYP    = SENSOR_PTR%TYPE
      NSENS  = SENSOR_PTR%SENS_ID
      TDELAY = SENSOR_PTR%TDELAY
      TSTART = SENSOR_PTR%TSTART
C--------------------------------
      IF (TYP == 0) THEN
C       SENSOR - START TIME
C--------------------------------
        LTT_DP = TT
        DT2_DP = DT2
        NEXT_CYCLE = TT + DT2
        TDELAY_DP = TDELAY
        IF ( NEXT_CYCLE >= TDELAY_DP .AND. SENSOR_PTR%STATUS == 0) THEN
            SENSOR_PTR%STATUS = 1
c            SENSOR_PTR%TSTART = TT
            SENSOR_PTR%TSTART = TDELAY
          IF (ISPMD == 0) THEN
#include "lockon.inc"
            WRITE (IOUT,'(A,I10,A,1PE12.5)')   
     .           ' SENSOR NUMBER ',NSENS,' ACTIVATED AT TIME ',TDELAY
#include "lockoff.inc"
          ENDIF
        ENDIF
C--------------------------------
      ELSEIF (TYP == 1) THEN
C       SENSOR - ACCELEROMETER
C--------------------------------
c
        CALL SENSOR_ACC(SENSOR_PTR ,ACC)
c
C--------------------------------
C         SENSOR - DISTANCE
C--------------------------------
      ELSEIF (TYP == 2) THEN
c
        CALL SENSOR_DIST(SENSOR_PTR ,X      ,XSENS   )
c
C--------------------------------
C         SENSOR - SENS, AND, NOT
C--------------------------------
      ELSEIF (TYP == 3 .OR. TYP == 4 .OR. TYP == 8) THEN
        IF (TT > = SENSOR_PTR%TSTART) THEN
          SENSOR_PTR%STATUS = 1
        ELSE
          SENSOR_PTR%STATUS = 0
        ENDIF
C--------------------------------
      ELSEIF (TYP == 5) THEN
C       SENSOR "OR"
        IF (TT >= SENSOR_PTR%TSTART) THEN
          SENSOR_PTR%STATUS = 1
        ELSE
          SENSOR_PTR%STATUS = 0
        ENDIF
C--------------------------------
      ELSEIF (TYP == 6)THEN
C       SENSOR - CONTACT
C--------------------------------
        CALL SENSOR_CONTACT(SENSOR_PTR,
     .           IPARI    ,DIMFB   ,STABS   ,TABS   ,FBSAV6)


C--------------------------------
      ELSEIF(TYP == 7)THEN
C       SENSOR - RWALL
C--------------------------------

        CALL SENSOR_RWALL(SENSOR_PTR ,
     .                    NPRW    ,DIMFB   ,STABS   ,TABS   ,FBSAV6)

C--------------------------------
      ELSEIF(TYP == 9)THEN
C       SENSOR - NODAL VELOCITY
C--------------------------------

        CALL SENSOR_VEL(SENSOR_PTR,V)

C--------------------------------
      ELSEIF (TYP == 10) THEN
C       SENSOR - GAUGE
C--------------------------------

        CALL SENSOR_GAUGE(SENSOR_PTR ,GAUGE)

C--------------------------------
      ELSEIF (TYP == 11) THEN
C       SENSOR - RBODY
C--------------------------------

        CALL SENSOR_RBODY(SENSOR_PTR,DIMFB   ,STABS   ,TABS   ,FBSAV6)


C--------------------------------
      ELSEIF (TYP == 12) THEN
C       SENSOR - SECTION
C--------------------------------

        CALL SENSOR_SECTION(SENSOR_PTR,IPARSENS,DIMFB   ,STABS   ,TABS   ,
     .                      FSAV      ,FBSAV6  )

C--------------------------------
      ELSEIF (TYP == 13)THEN
C--------------------------------
C       SENSOR WORK
C--------------------------------

        CALL SENSOR_WORK(SENSOR_PTR,X       ,XSENS   ,DIMFB   ,STABS   ,
     .                   TABS      ,FBSAV6  )
c
c--------------------------------
      ELSEIF (TYP == 14) THEN
c--------------------------------
c       SENSOR --- ENERGY ---
c--------------------------------        
      CALL SENSOR_ENERGY(SENSOR_PTR,ISENS ,SUBSET    ,PARTSAV2 )
c
C--------------------------------
      ELSEIF (TYP == 15) THEN
c--------------------------------
c       SENSOR Distance to surface
c
        CALL SENSOR_DIST_SURF(SENSOR_PTR ,X   ,IGRSURF  )
c
c--------------------------------
      ELSEIF (TYP == 16) THEN
c--------------------------------
c       SENSOR HIC
c
        CALL SENSOR_HIC(SENSOR_PTR ,A   ,ACC  )
c
c--------------------------------
      ELSEIF (TYP == 17) THEN
c--------------------------------
c       SENSOR TEMPERATURE
c
        CALL SENSOR_TEMP(SENSOR_PTR ,ISENS ,IGRNOD)
c
c--------------------------------
      ELSEIF (TYP == 19) THEN
c--------------------------------
c       SENSOR NIC-Nij
c
        CALL SENSOR_NIC(SENSOR_PTR )
c
c----
      ENDIF  ! TYP 
c-----------
      RETURN
      END
