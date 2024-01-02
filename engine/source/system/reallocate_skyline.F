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
Chd|  REALLOCATE_I_SKYLINE          source/system/reallocate_skyline.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|        SPMD_EXCH2_A_PON              source/mpi/forces/spmd_exch2_a_pon.F
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        FSKYI_MOD                     share/modules/restart_mod.F   
Chd|        HEAT_MOD                      share/modules/heat_mod.F      
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE REALLOCATE_I_SKYLINE(NEW_COUNT,CALL_ID)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE FSKYI_MOD
      USE HEAT_MOD
      USE MESSAGE_MOD
      USE PLYXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr18_c.inc"
#include      "parit_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEW_COUNT,CALL_ID
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IERROR
      INTEGER SISKY_OLD,LSKYI_OLD
      CHARACTER MSG*50,ID*50
      my_real, DIMENSION(:,:),ALLOCATABLE :: TEMP_FSKYI_PLY
     *                                       
c      INTEGER, DIMENSION(:),ALLOCATABLE :: TEMP_ISKY

C-----------------------------------------------
C ----------------------------------------------------
C Check if ISKY & FSKYI are sufficiently allocate
C If not reallocate them  
C ----------------------------------------------------
C NISKY : current counter stored stuff in ISKY & FSKYI
C SISKY - LSKYI : ISKY Size
C SFSKYI : FSKYI size (LSKYI*NFSKYI)

         WRITE(ID,'(I2.2)') CALL_ID
         MSG='(/PARITH/ON) - '//ID(1:2)
         SISKY_OLD = SISKY
         LSKYI_OLD = LSKYI

C
C Allocate Tempo FSKYI for copy
C
         ALLOCATE(TEMP_FSKYI(LSKYI,NFSKYI),STAT=IERROR)
         IF(IERROR/=0) THEN
           CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .         C1=MSG)
           CALL ARRET(2)
         ENDIF
         TEMP_FSKYI(1:NISKY,1:NFSKYI)=FSKYI(1:NISKY,1:NFSKYI)
         DEALLOCATE(FSKYI)

C
C Allocate Tempo ISKY for copy
C
         ALLOCATE(TEMP_ISKY(SISKY),STAT=IERROR)
         IF(IERROR/=0) THEN
           CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .         C1=MSG)
           CALL ARRET(2)
         ENDIF
         TEMP_ISKY(1:NISKY)=ISKY(1:NISKY)
         DEALLOCATE(ISKY)
          
C
C Allocate Tempo INTHEAT & FTHESKYI for copy
C
         IF(INTHEAT > 0  ) THEN
            ALLOCATE(TEMP_FTHESKYI(LSKYI),STAT=IERROR)
            IF(IERROR/=0) THEN
              CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .            C1=MSG)
              CALL ARRET(2)
            ENDIF
            TEMP_FTHESKYI(1:NISKY)=FTHESKYI(1:NISKY)
            DEALLOCATE(FTHESKYI)
            IF(NODADT_THERM ==1) THEN
              ALLOCATE(TEMP_CONDNSKYI(LSKYI))
              TEMP_CONDNSKYI(1:LSKYI)=CONDNSKYI(1:LSKYI)
              DEALLOCATE(CONDNSKYI)
            ENDIF
         ENDIF

C Resize SFSKYI & SISKY
C (LSKYI+NEW_COUNT)*1.2 can up to double the initial size.
         LSKYI = NINT( (LSKYI+NEW_COUNT)*1.2 )
         SISKY = LSKYI
         SFSKYI = LSKYI*NFSKYI

C FSKYI
         ALLOCATE(FSKYI(LSKYI,NFSKYI),STAT=IERROR)
         IF(IERROR/=0) THEN
           CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .         C1=MSG)
           CALL ARRET(2)
         ENDIF
         FSKYI(1:LSKYI_OLD,1:NFSKYI)=TEMP_FSKYI(1:LSKYI_OLD,1:NFSKYI)
         DEALLOCATE(TEMP_FSKYI)
         
C Reallocate & copy back stored stuff
C ISKY
         ALLOCATE(ISKY(SISKY),STAT=IERROR)
         IF(IERROR/=0) THEN
           CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .         C1=MSG)
           CALL ARRET(2)
         ENDIF
         ISKY(1:SISKY_OLD) = TEMP_ISKY(1:SISKY_OLD)
         DEALLOCATE(TEMP_ISKY)

C INTHEAT
         IF(INTHEAT > 0 ) THEN
           ALLOCATE(FTHESKYI(LSKYI),STAT=IERROR)
           IF(IERROR/=0) THEN
             CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .           C1=MSG)
             CALL ARRET(2)
           ENDIF
           FTHESKYI(1:LSKYI_OLD)=TEMP_FTHESKYI(1:LSKYI_OLD)
           DEALLOCATE (TEMP_FTHESKYI)

           IF(NODADT_THERM ==1) THEN
               ALLOCATE(CONDNSKYI(LSKYI))
               CONDNSKYI(1:LSKYI_OLD)=TEMP_CONDNSKYI(1:LSKYI_OLD)
               DEALLOCATE (TEMP_CONDNSKYI)

           ENDIF
         ENDIF
C
C for ply xfem
C
          IF(INTPLYXFEM > 0) THEN
C PLYSKYI%FSKYI
             ALLOCATE(TEMP_FSKYI_PLY(LSKYI_OLD,5))
             TEMP_FSKYI_PLY(1:LSKYI_OLD,1:5) = PLYSKYI%FSKYI(1:LSKYI_OLD,1:5)
             DEALLOCATE(PLYSKYI%FSKYI)

             ALLOCATE(PLYSKYI%FSKYI(LSKYI,5))
             PLYSKYI%FSKYI = ZERO
             PLYSKYI%FSKYI(1:LSKYI_OLD,1:5)=TEMP_FSKYI_PLY(1:LSKYI_OLD,1:5)
             DEALLOCATE(TEMP_FSKYI)
          ENDIF
C

       END
