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
Chd|  SENSOR_BASE                    source/tools/sensor/sensor_base.F 
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
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
Chd|        USENS_SHIFT_AB                source/user_interface/usensor.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SENSOR_BASE(SENSOR_TAB,
     .           XSENS     ,IPARSENS  ,TABS      ,STABS     ,IPARI     ,
     .           SFSAV6    ,FSAV      ,FBSAV6    ,PARTSAV2  ,GAUGE     ,
     .           X         ,V         ,A         ,ACC       ,NPRW      ,
     .           SUBSET    ,IGRSURF   ,IGRNOD    )
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
      INTEGER SFSAV6,STABS,IPARSENS
      INTEGER ,DIMENSION(NPARI,NINTER)  ,INTENT(IN) :: IPARI
      INTEGER ,DIMENSION(*)             ,INTENT(IN) :: NPRW
      INTEGER ,DIMENSION(STABS)         ,INTENT(IN) :: TABS
      my_real ,DIMENSION(3,NUMNOD)      ,INTENT(IN) :: X,V,A
      my_real ,DIMENSION(12,NSENSOR)    ,INTENT(IN) :: XSENS
      my_real ,DIMENSION(LLACCELM,*)    ,INTENT(IN) :: ACC
      my_real ,DIMENSION(NTHVKI,*)      ,INTENT(IN) :: FSAV
      my_real ,DIMENSION(LLGAUGE,*)     ,INTENT(IN) :: GAUGE
      my_real ,DIMENSION(2,*)           ,INTENT(IN) :: PARTSAV2
      TYPE (SUBSET_) ,DIMENSION(NSUBS)  ,INTENT(IN) :: SUBSET
      TYPE(SURF_)    ,DIMENSION(NSURF)  ,INTENT(IN) :: IGRSURF
      TYPE(GROUP_)   ,DIMENSION(NGRNOD) ,INTENT(IN) :: IGRNOD
      TYPE (SENSOR_STR_)  ,DIMENSION(NSENSOR)       :: SENSOR_TAB
      DOUBLE PRECISION ,DIMENSION(12,6,SFSAV6)      :: FBSAV6
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER TYP,ISENS
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     this routine checks activation condition of all basic sensor types
C=======================================================================
c     Parallel loop over sensor list
c--------------------------------------------------

!$OMP PARALLEL DO PRIVATE(ISENS,TYP)

c--------------------------------------------------
      DO ISENS = 1,NSENSOR

        TYP = SENSOR_TAB(ISENS)%TYPE
C--------------------------------
C       SENSOR - TIME
C--------------------------------
        IF (TYP == 0) THEN

        CALL SENSOR_TIME(SENSOR_TAB(ISENS))

C--------------------------------
C       SENSOR - ACCELEROMETER
C--------------------------------
        ELSEIF (TYP == 1) THEN
c
          CALL SENSOR_ACC(SENSOR_TAB(ISENS) ,ACC)
c
C--------------------------------
C       SENSOR - DISTANCE
C--------------------------------
        ELSEIF (TYP == 2) THEN
c
          CALL SENSOR_DIST(SENSOR_TAB(ISENS) ,X    ,XSENS(1,ISENS))
c
C--------------------------------
C       SENSOR - SENS, AND, NOT
C--------------------------------
        ELSEIF (TYP == 3 .OR. TYP == 4 .OR. TYP == 8) THEN
          IF (TT > = SENSOR_TAB(ISENS)%TSTART) THEN
            SENSOR_TAB(ISENS)%STATUS = 1
          ELSE
            SENSOR_TAB(ISENS)%STATUS = 0
          ENDIF
C--------------------------------
C       SENSOR "OR"
C--------------------------------
        ELSEIF (TYP == 5) THEN
          IF (TT >= SENSOR_TAB(ISENS)%TSTART) THEN
            SENSOR_TAB(ISENS)%STATUS = 1
          ELSE
            SENSOR_TAB(ISENS)%STATUS = 0
          ENDIF
C--------------------------------
C       SENSOR - CONTACT
C--------------------------------
        ELSEIF (TYP == 6)THEN
          CALL SENSOR_CONTACT(SENSOR_TAB(ISENS),
     .         IPARI    ,SFSAV6   ,STABS   ,TABS   ,FBSAV6)

C--------------------------------
C       SENSOR - RWALL
C--------------------------------
        ELSEIF(TYP == 7)THEN

          CALL SENSOR_RWALL(SENSOR_TAB(ISENS) ,
     .         NPRW    ,SFSAV6   ,STABS   ,TABS   ,FBSAV6)

C--------------------------------
C       SENSOR - NODAL VELOCITY
C--------------------------------
        ELSEIF(TYP == 9)THEN

          CALL SENSOR_VEL(SENSOR_TAB(ISENS),V)

C--------------------------------
C       SENSOR - GAUGE
C--------------------------------
        ELSEIF (TYP == 10) THEN

          CALL SENSOR_GAUGE(SENSOR_TAB(ISENS) ,GAUGE)

C--------------------------------
C       SENSOR - RBODY
C--------------------------------
        ELSEIF (TYP == 11) THEN

          CALL SENSOR_RBODY(SENSOR_TAB(ISENS),SFSAV6   ,STABS   ,TABS   ,FBSAV6)


C--------------------------------
C       SENSOR - SECTION
C--------------------------------
        ELSEIF (TYP == 12) THEN

          CALL SENSOR_SECTION(SENSOR_TAB(ISENS),
     .         IPARSENS,SFSAV6   ,STABS   ,TABS   ,FSAV      ,FBSAV6  )

C--------------------------------
C       SENSOR WORK
C--------------------------------
        ELSEIF (TYP == 13)THEN

          CALL SENSOR_WORK(SENSOR_TAB(ISENS),X   ,XSENS(1,ISENS)   ,
     .         SFSAV6   ,STABS   ,TABS      ,FBSAV6  )
c
c--------------------------------
c       SENSOR --- ENERGY ---
c--------------------------------        
        ELSEIF (TYP == 14) THEN
 
          CALL SENSOR_ENERGY(SENSOR_TAB(ISENS),ISENS ,SUBSET    ,PARTSAV2 )
c
c--------------------------------
c       SENSOR Distance to surface
C--------------------------------
        ELSEIF (TYP == 15) THEN
c
          CALL SENSOR_DIST_SURF(SENSOR_TAB(ISENS) ,X   ,IGRSURF  )
c
c--------------------------------
c       SENSOR HIC
c--------------------------------
        ELSEIF (TYP == 16) THEN
c
          CALL SENSOR_HIC(SENSOR_TAB(ISENS) ,A   ,ACC  )
c
c--------------------------------
c       SENSOR TEMPERATURE
c--------------------------------
        ELSEIF (TYP == 17) THEN
c
          CALL SENSOR_TEMP(SENSOR_TAB(ISENS) ,ISENS ,IGRNOD)
c
c--------------------------------
        ELSEIF (TYP == 19) THEN
c--------------------------------
c       SENSOR NIC-Nij
c
          CALL SENSOR_NIC(SENSOR_TAB(ISENS) )
c
c----
        ENDIF  ! TYP 
c----
      END DO  ! ISENS
c--------------------------------------------------
c     END of parallel loop

!$OMP END PARALLEL DO

c--------------------------------------------------
      IF (IPARSENS > 0) FBSAV6(1:12,1:6,1:SFSAV6) = ZERO

c-----------
      RETURN
      END
