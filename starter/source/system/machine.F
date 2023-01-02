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
#include "hardware.inc"
C=======================================================================
C     // MEMOIRE PARTAGEE
C=======================================================================
Chd|====================================================================
Chd|  MY_BARRIER                    source/system/machine.F       
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MY_BARRIER
        RETURN
      END
Chd|====================================================================
Chd|  BIDON                         source/system/machine.F       
Chd|-- called by -----------
Chd|        INITWG_SHELL                  source/spmd/domain_decomposition/initwg_shell.F
Chd|        INITWG_SOLID                  source/spmd/domain_decomposition/initwg_solid.F
Chd|        INITWG_TRI                    source/spmd/domain_decomposition/initwg_tri.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BIDON
        RETURN
      END

Chd|====================================================================
Chd|  BIDON1                        source/system/machine.F       
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BIDON1(IBID)
        INTEGER IBID
        RETURN
      END

Chd|====================================================================
Chd|  BIDON2                        source/system/machine.F       
Chd|-- called by -----------
Chd|        LECSTACK_PLY                  source/properties/composite_options/stack/lecstack_ply.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BIDON2(FAC_L)
        DOUBLE PRECISION FAC_L
        RETURN
      END
C
C
C=======================================================================
C  ROUTINE VERIFICATION LIMITATIONS MACHINE
C=======================================================================
Chd|====================================================================
Chd|  MVERIF                        source/system/machine.F       
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE MVERIF()
      USE MESSAGE_MOD
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NODMAX
      DATA NODMAX/55000/
#if CPP_mach == CPP_w95 || CPP_mach == CPP_wmr  || CPP_mach == CPP_lmr 
      IF (NUMNOD>NODMAX) THEN
C        IWARN = IWARN + 1
C        WRITE(ISTDO,*)' ** WARNING NUMNOD TOO LARGE'
C        WRITE(IOUT,*)'** WARNING NUMNOD IS TOO LARGE FOR THIS VERSION'        
         CALL ANCMSG(MSGID=393,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=NUMNOD,
     .               I2=NODMAX)
      ENDIF
#endif
      RETURN
      END
C=======================================================================
C  ROUTINE ETIME
C=======================================================================
C|============================================================
C|  MY_ETIME			    /spe/machine.F
C|------------------------------------------------------------
C|-- appelee par -----------
C|	   STARTIME			   /resol/timer.F
C|	   STOPTIME			   /resol/timer.F
C|-- appelle ---------------
C|============================================================
Chd|====================================================================
Chd|  MY_ETIME                      source/system/machine.F       
Chd|-- called by -----------
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MY_ETIME(TIMER)
      REAL  RESULT
      REAL, DIMENSION(4) :: TIMER
#if CPP_mach == CPP_rs7 || CPP_mach == CPP_rs9 || CPP_mach == CPP_sp2  || CPP_mach == CPP_ppc || CPP_mach == CPP_pwr4 || CPP_mach == CPP_pwr4_spmd    
       RESULT = ETIME_(TIMER)
#elif CPP_mach == CPP_ymp || CPP_mach == CPP_c90 || CPP_mach == CPP_ymp_spmd || CPP_mach == CPP_t90  || CPP_mach == CPP_t90_i3e
      CALL TSECND(TIMER)
#elif CPP_mach == CPP_sx4 || CPP_mach == CPP_sx4_i3e
      RESULT = ETIME(TIMER)
/* el51e9 #elif CPP_mach == CPP_w95 || CPP_mach == CPP_ant || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr */
#elif CPP_mach == CPP_w95 || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr
      TIMER(1) = 0.
#elif CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_p4win64
      CALL CPU_TIME(TIMER(1))
#elif CPP_mach == CPP_cx1_spmd || CPP_mach == CPP_cx1
      RESULT = ETIMEC(TIMER)
#elif 1
      RESULT = ETIME(TIMER)
#endif
      RETURN
      END
C
C
C appel Fortran flush
Chd|====================================================================
Chd|  MY_FLUSH                      source/system/machine.F       
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE MY_FLUSH(IUNIT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IUNIT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C call a rendre specifique machine en cas de pb de compilation
#if CPP_mach == CPP_pwr4 || CPP_mach == CPP_pwr4_spmd
C pas de routine FLUSH sur AIX
#elif 1
      CALL FLUSH(IUNIT)
#endif
      RETURN
      END


#if CPP_mach == CPP_p4linux964 || CPP_mach == CPP_p4win64
ccc || CPP_mach == CPP_p4win64  || CPP_mach == CPP_p4win32 || CPP_mach==CPP_macosx64
Chd|====================================================================
Chd|  CHECK_KMP_STACK               source/system/machine.F       
Chd|-- called by -----------
Chd|        SET_STACK_VALUES              source/system/machine.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CHECK_KMP_STACK()
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#ifdef __INTEL_COMPILER
      use omp_lib
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER   (KIND=KMP_SIZE_T_KIND) NEWSIZ
      CHARACTER*256 KMP_STACKSIZE_VALUE
      INTEGER KMP_SIZE
 
      KMP_SET = 0
 
      CALL GETENV('KMP_STACKSIZE',KMP_STACKSIZE_VALUE)
C         
      IF (KMP_STACKSIZE_VALUE(1:1) == ' ') THEN
         KMP_SET = 1
         !set default KMP_STACKSIZE value to 380 M
         NEWSIZ = 398458880
         CALL KMP_SET_STACKSIZE_S(NEWSIZ)
      ELSE
         KMPSTSIZ = KMP_GET_STACKSIZE()
         !user warning
         IF (KMPSTSIZ < 398458880)THEN
           KMP_SET = 2
         ENDIF
      ENDIF
#elif 1
      KMP_SET =0
#endif
      END
Chd|====================================================================
Chd|  GET_KMP_STACK                 source/system/machine.F       
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GET_KMP_STACK()
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#ifdef __INTEL_COMPILER
      use omp_lib
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  KMPSTSIZ,NEWSIZ
C         
      KMPSTSIZ = KMP_GET_STACKSIZE()
      print*,'KMPSTSIZ=',KMPSTSIZ
#elif 1

#endif
      END
#elif 1
Chd|====================================================================
Chd|  CHECK_KMP_STACK               source/system/machine.F       
Chd|-- called by -----------
Chd|        SET_STACK_VALUES              source/system/machine.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CHECK_KMP_STACK()
#include      "com01_c.inc"
      KMP_SET =0
      RETURN
      END
C
Chd|====================================================================
Chd|  GET_KMP_STACK                 source/system/machine.F       
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GET_KMP_STACK()
      RETURN
      END
#endif


Chd|====================================================================
Chd|  SET_STACK_VALUES              source/system/machine.F       
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        CHECK_KMP_STACK               source/system/machine.F       
Chd|        SET_STACK_C                   source/system/machine_c.c     
Chd|====================================================================
      SUBROUTINE SET_STACK_VALUES()
#include      "com01_c.inc"
C Check & Set Stacksize value
       INTEGER Stack

       CALL SET_STACK_C(Stack)
       STACKSIZ = Stack

C Check KMP_STACKSIZE Value
       CALL  CHECK_KMP_STACK()
      END
