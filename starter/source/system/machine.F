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
#include "hardware.inc"
C=======================================================================
C     // MEMOIRE PARTAGEE
C=======================================================================
      !||====================================================================
      !||    my_barrier   ../starter/source/system/machine.F
      !||====================================================================
      SUBROUTINE MY_BARRIER
        RETURN
      END
      !||====================================================================
      !||    bidon          ../starter/source/system/machine.F
      !||--- called by ------------------------------------------------------
      !||    initwg_shell   ../starter/source/spmd/domain_decomposition/initwg_shell.F
      !||    initwg_solid   ../starter/source/spmd/domain_decomposition/initwg_solid.F
      !||    initwg_tri     ../starter/source/spmd/domain_decomposition/initwg_tri.F
      !||====================================================================
      SUBROUTINE BIDON
        RETURN
      END

      !||====================================================================
      !||    bidon1   ../starter/source/system/machine.F
      !||====================================================================
      SUBROUTINE BIDON1(IBID)
        INTEGER IBID
        RETURN
      END

      !||====================================================================
      !||    bidon2         ../starter/source/system/machine.F
      !||--- called by ------------------------------------------------------
      !||    lecstack_ply   ../starter/source/properties/composite_options/stack/lecstack_ply.F
      !||====================================================================
      SUBROUTINE BIDON2(FAC_L)
        DOUBLE PRECISION FAC_L
        RETURN
      END
C
C
C=======================================================================
C  ROUTINE VERIFICATION LIMITATIONS MACHINE
C=======================================================================
      !||====================================================================
      !||    mverif        ../starter/source/system/machine.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg        ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod   ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE MVERIF()
      USE MESSAGE_MOD
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
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

      !||====================================================================
      !||    my_etime   ../starter/source/system/machine.F
      !||--- called by ------------------------------------------------------
      !||    startime   ../starter/source/system/timer.F
      !||    stoptime   ../starter/source/system/timer.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
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
      !||====================================================================
      !||    my_flush   ../starter/source/system/machine.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE MY_FLUSH(IUNIT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
      !||====================================================================
      !||    check_kmp_stack    ../starter/source/system/machine.F
      !||--- called by ------------------------------------------------------
      !||    set_stack_values   ../starter/source/system/machine.F
      !||====================================================================
      SUBROUTINE CHECK_KMP_STACK()
#ifdef __INTEL_COMPILER
      use omp_lib
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"      
#include      "scr17_c.inc"
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
#include      "implicit_f.inc"
#include      "com01_c.inc"    
      KMP_SET =0
#endif
      END
      
      !||====================================================================
      !||    get_kmp_stack   ../starter/source/system/machine.F
      !||====================================================================
      SUBROUTINE GET_KMP_STACK()
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#ifdef __INTEL_COMPILER
      use omp_lib
#include      "implicit_f.inc"
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
      !||====================================================================
      !||    check_kmp_stack    ../starter/source/system/machine.F
      !||--- called by ------------------------------------------------------
      !||    set_stack_values   ../starter/source/system/machine.F
      !||====================================================================
      SUBROUTINE CHECK_KMP_STACK()
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s 
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
      KMP_SET =0
      RETURN
      END
C
      !||====================================================================
      !||    get_kmp_stack   ../starter/source/system/machine.F
      !||====================================================================
      SUBROUTINE GET_KMP_STACK()
      RETURN
      END
#endif


      !||====================================================================
      !||    set_stack_values   ../starter/source/system/machine.F
      !||--- called by ------------------------------------------------------
      !||    starter            ../starter/source/starter/starter.F
      !||--- calls      -----------------------------------------------------
      !||    check_kmp_stack    ../starter/source/system/machine.F
      !||====================================================================
      SUBROUTINE SET_STACK_VALUES()
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s 
C-----------------------------------------------
#include      "com01_c.inc"
C Check & Set Stacksize value
       INTEGER Stack

       CALL SET_STACK_C(Stack)
       STACKSIZ = Stack

C Check KMP_STACKSIZE Value
       CALL  CHECK_KMP_STACK()
      END
