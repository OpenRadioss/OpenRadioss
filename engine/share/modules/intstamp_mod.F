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
Chd|  INTSTAMP_MOD                  share/modules/intstamp_mod.F  
Chd|-- called by -----------
Chd|        I21ASS3                       source/interfaces/int21/i21ass3.F
Chd|        I21BUCE_CRIT                  source/interfaces/intsort/i21buce_crit.F
Chd|        I21MAINF                      source/interfaces/int21/i21mainf.F
Chd|        I21MAIN_CRIT_TRI              source/interfaces/intsort/i21main_crit_tri.F
Chd|        I21MAIN_GAP                   source/interfaces/int21/i21main_gap.F
Chd|        I21MAIN_OPT_TRI               source/interfaces/intsort/i21main_opt_tri.F
Chd|        I21MAIN_TRI                   source/interfaces/intsort/i21main_tri.F
Chd|        I21_ICRIT                     source/interfaces/intsort/i21_icrit.F
Chd|        I25MAIN_GAP                   source/interfaces/int25/i25main_gap.F
Chd|        INTFOP2                       source/interfaces/interf/intfop2.F
Chd|        INTSTAMP_ASS                  source/interfaces/int21/intstamp_ass.F
Chd|        INTSTAMP_DT                   source/interfaces/int21/intstamp_dt.F
Chd|        INTSTAMP_INIT                 source/interfaces/int21/intstamp_init.F
Chd|        INTSTAMP_MOVE                 source/interfaces/int21/intstamp_move.F
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|        SMS_BUILD_MAT_2               source/ams/sms_build_mat_2.F  
Chd|        SMS_ENCIN_2                   source/ams/sms_encin_2.F      
Chd|        SMS_INI_JAD_2                 source/ams/sms_init.F         
Chd|        SMS_INI_JAD_3                 source/ams/sms_init.F         
Chd|        SMS_MASS_SCALE_2              source/ams/sms_mass_scale_2.F 
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|        SPMD_I21FTHECOM               source/mpi/interfaces/send_cand.F
Chd|        SPMD_I21TEMPCOM               source/mpi/interfaces/send_cand.F
Chd|        SPMD_IFRONT_STAMP             source/mpi/interfaces/send_cand.F
Chd|        INTSTAMP_GLOB_MOD             share/modules/intstamp_glob_mod.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE INTSTAMP_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------
      TYPE INTSTAMP_DATA
C---------
C         reference to the interface index in INBUF, BUFIN
C---------
          INTEGER NOINTER
C---------
C         Associated Rbody 
C---------
          INTEGER IRB
C---------
C         Main node of Associated Rbody 
C---------
          INTEGER MSR
C---------
C         Flag / Free rotational ddls (0:NO/1:YES) 
C---------
          INTEGER IROT
C---------
C         reference interface for damping
          INTEGER INTDAMP
C---------
C         mass, gravity center, inertia, orientation matrix, damping
C---------
          my_real
     .       MASS, IN(3), ROT(9), DAMP, DAMPR
          DOUBLE PRECISION XG(3)
C---------
          my_real
     .            DW, BRACKET(3)
C---------
C         vitesse, deplacement, force
C---------
          my_real
     .           V(3), FC(3), STF,
     .           VR(3), MC(3), STR
          DOUBLE PRECISION FC6(6,3),ST6(6),MC6(6,3),STR6(6)
          DOUBLE PRECISION D(3), DR(3)

      END TYPE INTSTAMP_DATA
C-----------------------------------------------
C    S p e c i f i c   F u n c t i o n s 
C-----------------------------------------------
      CONTAINS
Chd|====================================================================
Chd|  INTSTAMP_ZERO                 share/modules/intstamp_mod.F  
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INTSTAMP_ZERO(INTSTAMP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "intstamp_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INTSTAMP_DATA) INTSTAMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, J, K
C--------------------------------------
C     ECRITURE DES DIMENSIONS
C--------------------------------------
      DO N=1,NINTSTAMP
        INTSTAMP(N)%NOINTER=0
        INTSTAMP(N)%IRB    =0
        INTSTAMP(N)%MSR    =0
        INTSTAMP(N)%IROT   =0
        INTSTAMP(N)%INTDAMP=0
        INTSTAMP(N)%MASS =ZERO
        DO J=1,3
          INTSTAMP(N)%XG(J) =ZERO
        END DO
        DO J=1,3
          INTSTAMP(N)%IN(J) =ZERO
        END DO
        DO J=1,9
          INTSTAMP(N)%ROT(J) =ZERO
        END DO
        INTSTAMP(N)%DAMP =ZERO
        INTSTAMP(N)%DAMPR=ZERO
        DO J=1,3
          INTSTAMP(N)%V(J)=ZERO
          INTSTAMP(N)%D(J)=ZERO
          INTSTAMP(N)%VR(J)=ZERO
          INTSTAMP(N)%DR(J)=ZERO
          INTSTAMP(N)%FC(J)=ZERO
          INTSTAMP(N)%MC(J)=ZERO
          DO K=1,6
          INTSTAMP(N)%FC6(K,J)=ZERO
          INTSTAMP(N)%MC6(K,J)=ZERO
          END DO
        END DO
        INTSTAMP(N)%DW =ZERO
        DO J=1,3
           INTSTAMP(N)%BRACKET(J) =ZERO
        END DO
        INTSTAMP(N)%STF=ZERO
        INTSTAMP(N)%STR=ZERO
        DO K=1,6
        INTSTAMP(N)%ST6(K) =ZERO
        INTSTAMP(N)%STR6(K)=ZERO
        END DO
      END DO
      RETURN
      END SUBROUTINE INTSTAMP_ZERO
Chd|====================================================================
Chd|  INTSTAMP_WRESTI               share/modules/intstamp_mod.F  
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|====================================================================
      SUBROUTINE INTSTAMP_WRESTI(INTSTAMP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "intstamp_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INTSTAMP_DATA) INTSTAMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, J, NCOND
C--------------------------------------
C     ECRITURE DES DIMENSIONS
C--------------------------------------
      DO N=1,NINTSTAMP
        LEN    =1
        CALL WRITE_I_C(INTSTAMP(N)%NOINTER,LEN)
        LEN    =1
        CALL WRITE_I_C(INTSTAMP(N)%IRB,LEN)
        LEN    =1
        CALL WRITE_I_C(INTSTAMP(N)%MSR,LEN)
        LEN    =1
        CALL WRITE_I_C(INTSTAMP(N)%IROT,LEN)
        LEN    =1
        CALL WRITE_I_C(INTSTAMP(N)%INTDAMP,LEN)
      END DO
      RETURN
      END SUBROUTINE INTSTAMP_WRESTI
Chd|====================================================================
Chd|  INTSTAMP_WRESTR               share/modules/intstamp_mod.F  
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_DPDB                    source/output/tools/write_db.F
Chd|====================================================================
      SUBROUTINE INTSTAMP_WRESTR(INTSTAMP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "intstamp_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INTSTAMP_DATA) INTSTAMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, J, NCOND
C--------------------------------------
      DO N=1,NINTSTAMP
        LEN    =1
        CALL WRITE_DB(INTSTAMP(N)%MASS,LEN)
        LEN    =3
        CALL WRITE_DPDB(INTSTAMP(N)%XG,LEN)
        LEN    =3
        CALL WRITE_DB(INTSTAMP(N)%IN,LEN)
        LEN    =9
        CALL WRITE_DB(INTSTAMP(N)%ROT,LEN)
        LEN    =1
        CALL WRITE_DB(INTSTAMP(N)%DAMP,LEN)
        LEN    =1
        CALL WRITE_DB(INTSTAMP(N)%DAMPR,LEN)
        LEN    =3
        CALL WRITE_DPDB(INTSTAMP(N)%D,LEN)
        LEN    =3
        CALL WRITE_DB(INTSTAMP(N)%V,LEN)
        LEN    =3
        CALL WRITE_DPDB(INTSTAMP(N)%DR,LEN)
        LEN    =3
        CALL WRITE_DB(INTSTAMP(N)%VR,LEN)
        LEN    =1
        CALL WRITE_DB(INTSTAMP(N)%DW,LEN)
        LEN    =3
        CALL WRITE_DB(INTSTAMP(N)%BRACKET,LEN)
      END DO
      RETURN
      END SUBROUTINE INTSTAMP_WRESTR
Chd|====================================================================
Chd|  INTSTAMP_RRESTI               share/modules/intstamp_mod.F  
Chd|-- called by -----------
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        READ_I_C                      ../common_source/tools/input_output/write_routtines.c
Chd|====================================================================
      SUBROUTINE INTSTAMP_RRESTI(INTSTAMP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "intstamp_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INTSTAMP_DATA) INTSTAMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, NCOND, J, STAT
C--------------------------------------
C     ECRITURE DES DIMENSIONS
C--------------------------------------
      DO N=1,NINTSTAMP
        LEN    =1
        CALL READ_I_C(INTSTAMP(N)%NOINTER,LEN)
        LEN    =1
        CALL READ_I_C(INTSTAMP(N)%IRB,LEN)
        LEN    =1
        CALL READ_I_C(INTSTAMP(N)%MSR,LEN)
        LEN    =1
        CALL READ_I_C(INTSTAMP(N)%IROT,LEN)
        LEN    =1
        CALL READ_I_C(INTSTAMP(N)%INTDAMP,LEN)
      END DO
      RETURN
      END SUBROUTINE INTSTAMP_RRESTI
Chd|====================================================================
Chd|  INTSTAMP_RRESTR               share/modules/intstamp_mod.F  
Chd|-- called by -----------
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_DPDB                     source/output/tools/read_db.F 
Chd|====================================================================
      SUBROUTINE INTSTAMP_RRESTR(INTSTAMP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "intstamp_c.inc"
#include      "scr05_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(INTSTAMP_DATA) INTSTAMP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER LEN, N, NCOND, J, STAT
C--------------------------------------
      DO N=1,NINTSTAMP
        LEN    =1
        CALL READ_DB(INTSTAMP(N)%MASS,LEN)
        LEN    =3
        CALL READ_DPDB(INTSTAMP(N)%XG,LEN)
        LEN    =3
        CALL READ_DB(INTSTAMP(N)%IN,LEN)
        LEN    =9
        CALL READ_DB(INTSTAMP(N)%ROT,LEN)
        LEN    =1
        CALL READ_DB(INTSTAMP(N)%DAMP,LEN)
        LEN    =1
        CALL READ_DB(INTSTAMP(N)%DAMPR,LEN)
        LEN    =3
        CALL READ_DPDB(INTSTAMP(N)%D,LEN)
        LEN    =3
        CALL READ_DB(INTSTAMP(N)%V,LEN)
        LEN    =3
        CALL READ_DPDB(INTSTAMP(N)%DR,LEN)
        LEN    =3
        CALL READ_DB(INTSTAMP(N)%VR,LEN)
        LEN    =1
        CALL READ_DB(INTSTAMP(N)%DW,LEN)
        LEN    =3
        CALL READ_DB(INTSTAMP(N)%BRACKET,LEN)
      END DO
      RETURN
      END SUBROUTINE INTSTAMP_RRESTR

      END MODULE INTSTAMP_MOD
