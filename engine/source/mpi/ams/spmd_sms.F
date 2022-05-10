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
Chd|  SPMD_SORT_SMS                 source/mpi/ams/spmd_sms.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_SORT_SMS(ISKYI_SMS,MSKYI_SMS,FR_SMS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "parit_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISKYI_SMS(LSKYI_SMS,*), FR_SMS(NSPMD+1)
      my_real
     .        MSKYI_SMS(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER N, I2SORT(NISKY_SMS,3), NN, P, Q, PP, kk
      INTEGER LP(NSPMD+1), NP(NSPMD+1)
      my_real
     .        M2SORT(NISKY_SMS)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      DO N = 1, NISKY_SMS
        I2SORT(N,1)= ISKYI_SMS(N,1)
        I2SORT(N,2)= ISKYI_SMS(N,2)
        I2SORT(N,3)= ISKYI_SMS(N,3)
        M2SORT(N)  = MSKYI_SMS(N)
      ENDDO
C
      DO P=1,NSPMD
        LP(P)=0
      END DO
C
      DO N = 1, NISKY_SMS
        P = I2SORT(N,3)
        LP(P)=LP(P)+1
      END DO
C
      NP(1)=1
      DO P=1,NSPMD
        NP(P+1)=NP(P)+LP(P)
      END DO
C
      DO P=1,NSPMD+1
       FR_SMS(P)=NP(P)
      END DO
C
      DO N = 1, NISKY_SMS
        P = I2SORT(N,3)
        NN=NP(P)
        ISKYI_SMS(NN,1)=I2SORT(N,1)
        ISKYI_SMS(NN,2)=I2SORT(N,2)
        ISKYI_SMS(NN,3)=I2SORT(N,3)
        MSKYI_SMS(NN)  =M2SORT(N)
        NP(P)=NP(P)+1
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  AMS_PREPARE_POFF_ASSEMBLY     source/mpi/ams/spmd_sms.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE AMS_PREPARE_POFF_ASSEMBLY(IAD_ELEM,FR_ELEM,NB_FR,    FR_LOC,
     *                                     IAD_I2M, FR_I2M, NB_FRI2M, FR_LOC_I2M)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "parit_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*),FR_ELEM(*),FR_LOC(*),NB_FR,
     *        IAD_I2M(*),FR_I2M(*),NB_FRI2M, FR_LOC_I2M(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER P,J,NOD
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG

      ALLOCATE(TAG(NUMNOD))
      TAG(1:NUMNOD)=0

      DO P=1,NSPMD
        DO J=IAD_ELEM(1,P),IAD_ELEM(1,P+1)-1
           NOD = FR_ELEM(J)
           TAG(NOD)=1
        ENDDO 
      ENDDO

      NB_FR=0
      DO J=1,NUMNOD
        IF(TAG(J)==1) THEN
          NB_FR=NB_FR+1
          FR_LOC(NB_FR)=J
        ENDIF
      ENDDO

C Interface type2 
      TAG(1:NUMNOD)=0
      DO J=IAD_I2M(1),IAD_I2M(NSPMD+1)-1
         NOD = FR_I2M(J)
         TAG(NOD)=1
      ENDDO

      NB_FRI2M=0
      DO J=1,NUMNOD
        IF(TAG(J)==1) THEN
          NB_FRI2M=NB_FRI2M+1
          FR_LOC_I2M(NB_FRI2M)=J
        ENDIF
      ENDDO
     


      RETURN
      END
C
#include "hardware.inc"
#if defined(MPI)
Chd|====================================================================
Chd|  SPMD_NLIST_SMS                source/mpi/ams/spmd_sms.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_NLIST_SMS(FR_SMS,FR_RMS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "mpif.h"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FR_SMS(NSPMD+1), FR_RMS(NSPMD+1)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGTYP,I,LOC_PROC,IERROR,
     .        SIZ,J,L,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD)   
      INTEGER, DIMENSION(:), ALLOCATABLE:: BUFFER_SEND
      INTEGER, DIMENSION(:), ALLOCATABLE ::  BUFFER_RECV
      INTEGER MSGOFF
      DATA MSGOFF/17000/ 
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1

      ALLOCATE(BUFFER_RECV(NSPMD),BUFFER_SEND(NSPMD))
      DO I = 1,NSPMD
        IF(I == LOC_PROC) THEN
          BUFFER_SEND(I) = 0 
        ELSE
          BUFFER_SEND(I) = FR_SMS(I+1)-FR_SMS(I)
        ENDIF
      ENDDO 
      BUFFER_RECV = 0
      SIZ = 1

      ! Buffer copy can be avoided using MPI_INPLACE but has not been tested with all MPI implementations
      CALL MPI_ALLTOALL(BUFFER_SEND,SIZ,MPI_INTEGER,BUFFER_RECV,SIZ,MPI_INTEGER,MPI_COMM_WORLD,IERROR) 

      FR_RMS(1) = 1 
      DO I=1,NSPMD
        FR_RMS(I+1) = BUFFER_RECV(I)
      ENDDO

      FR_RMS(1)=1
      DO I = 1, NSPMD
        FR_RMS(I+1)=FR_RMS(I)+FR_RMS(I+1)
      END DO
c
      DEALLOCATE(BUFFER_SEND,BUFFER_RECV)
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_LIST_SMS                 source/mpi/ams/spmd_sms.F     
Chd|-- called by -----------
Chd|        SMS_BUILD_MAT_2               source/ams/sms_build_mat_2.F  
Chd|        SMS_ENCIN_2                   source/ams/sms_encin_2.F      
Chd|        SMS_MASS_SCALE_2              source/ams/sms_mass_scale_2.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_LIST_SMS(
     1           ISKYI_SMS,FR_SMS,FR_RMS,LIST_SMS,LIST_RMS,
     2           NPBY     ,TAGSLV_RBY_SMS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "mpif.h"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISKYI_SMS(LSKYI_SMS,*), FR_SMS(NSPMD+1),
     .        FR_RMS(NSPMD+1), LIST_SMS(*), LIST_RMS(*),
     .        NPBY(NNPBY,*), TAGSLV_RBY_SMS(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGTYP,I,LOC_PROC,IERROR,
     .        SIZ,J,L,M,TAG,
     .        STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD),REQ_S(NSPMD),
     .        SBUF(MAX(2*FR_SMS(NSPMD+1),FR_RMS(NSPMD+1))), 
     .        RBUF(MAX(2*FR_RMS(NSPMD+1),FR_SMS(NSPMD+1)))
      INTEGER MSGOFF,MSGOFF2
      DATA MSGOFF/17006/
      DATA MSGOFF2/17007/


C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      L = 1
      DO I=1,NSPMD
        SIZ = 2*(FR_RMS(I+1)-FR_RMS(I))
        IF(SIZ.NE.0)THEN
c          print *,'irecv1',loc_proc,i,siz
          MSGTYP = MSGOFF 
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ENDIF
      END DO
C
      L = 1
      M = 1
      DO I=1,NSPMD
        IF(I/=LOC_PROC)THEN
#include      "vectorize.inc"
          DO J=FR_SMS(I),FR_SMS(I+1)-1
            SBUF(L  )  = ISKYI_SMS(J,1)
            L = L + 1
            SBUF(L  )  = TAGSLV_RBY_SMS(ISKYI_SMS(J,2))
            L = L + 1
            LIST_SMS(M)= ISKYI_SMS(J,2)
            M = M + 1
          END DO
        ELSE
          L = L + 2*(FR_SMS(I+1)-FR_SMS(I))
        END IF
      ENDDO
C
C   echange messages
C
      DO I=1,NSPMD
       IF(I/=LOC_PROC.AND.FR_SMS(I+1)-FR_SMS(I).GT.0)THEN
          MSGTYP = MSGOFF 
          SIZ = 2*(FR_SMS(I+1)-FR_SMS(I))
c          print *,'isend1',loc_proc,i,siz
          L = 2*FR_SMS(I)-1
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF        
      ENDDO
C
      DO I = 1, NSPMD
        IF(FR_RMS(I+1)-FR_RMS(I).GT.0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)

          L = 2*FR_RMS(I)-1
          DO J=FR_RMS(I),FR_RMS(I+1)-1
            LIST_RMS(J)= RBUF(L)
            TAG        = RBUF(L+1)
            IF(TAG/=0.AND.TAG==TAGSLV_RBY_SMS(LIST_RMS(J)))THEN
              LIST_RMS(J)=0
            END IF
            L = L + 2
          END DO

        ENDIF
      END DO
C
      DO I = 1, NSPMD
        IF(I/=LOC_PROC.AND.FR_SMS(I+1)-FR_SMS(I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      END DO
C
C  rby
      L = 1
      DO I=1,NSPMD
        SIZ = FR_SMS(I+1)-FR_SMS(I)
        IF(I/=LOC_PROC.AND.SIZ.GT.0)THEN
c          print *,'irecv2',loc_proc,i,siz
          MSGTYP = MSGOFF2                               
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ELSE
          L = L + SIZ
        ENDIF
      END DO
C
      L = 1
      DO I=1,NSPMD
        IF(FR_RMS(I+1)-FR_RMS(I).GT.0)THEN
#include      "vectorize.inc"
          DO J=FR_RMS(I),FR_RMS(I+1)-1
C           LIST_RMS(J) ?= 0 
            SBUF(L  )  = LIST_RMS(J)
            L = L + 1
          END DO
        END IF
      ENDDO
C
      DO I=1,NSPMD
       IF(FR_RMS(I+1)-FR_RMS(I).GT.0)THEN
          MSGTYP = MSGOFF2 
          SIZ = FR_RMS(I+1)-FR_RMS(I)
c          print *,'isend2',loc_proc,i,siz
          L = FR_RMS(I)
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF        
      ENDDO
C
      M = 1
      DO I = 1, NSPMD
        IF(I/=LOC_PROC.AND.FR_SMS(I+1)-FR_SMS(I).GT.0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)

          L = FR_SMS(I)
          DO J=FR_SMS(I),FR_SMS(I+1)-1
            TAG        = RBUF(L)
            IF(TAG==0)THEN
              LIST_SMS(M)=0
            END IF
            L = L + 1
            M = M + 1
          END DO
        ENDIF
      END DO
C
      DO I = 1, NSPMD
        IF(FR_RMS(I+1)-FR_RMS(I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      END DO
C--------------------------------------------------------------------
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_MIJ_SMS                  source/mpi/ams/spmd_sms.F     
Chd|-- called by -----------
Chd|        SMS_BUILD_DIAG                source/ams/sms_build_diag.F   
Chd|        SMS_ENCIN_2                   source/ams/sms_encin_2.F      
Chd|        SMS_MASS_SCALE_2              source/ams/sms_mass_scale_2.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_MIJ_SMS(
     1           ISKYI_SMS,FR_SMS,FR_RMS,LIST_RMS,MSKYI_SMS,
     2           MIJ_SMS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "mpif.h"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "parit_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISKYI_SMS(LSKYI_SMS,*), FR_SMS(NSPMD+1),
     .        FR_RMS(NSPMD+1), LIST_RMS(*)
      my_real
     .        MSKYI_SMS(*), MIJ_SMS(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGTYP,I,LOC_PROC,IERROR,
     .        SIZ,J,L,
     .        STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      my_real
     .        SBUF(FR_SMS(NSPMD+1))
      INTEGER MSGOFF
      DATA MSGOFF/17008/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      L = 1
      DO I=1,NSPMD
        SIZ = FR_RMS(I+1)-FR_RMS(I)
        IF(SIZ.NE.0)THEN
          MSGTYP = MSGOFF 
c          print *,'mij-irecv',loc_proc,i,siz
          CALL MPI_IRECV(
     S      MIJ_SMS(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ENDIF
      END DO
      L = 1
      DO I=1,NSPMD
        IF(I/=LOC_PROC)THEN
#include      "vectorize.inc"
          DO J=FR_SMS(I),FR_SMS(I+1)-1
            SBUF(L  ) =  MSKYI_SMS(J)
            L = L + 1
          END DO
        ELSE
          L = L + FR_SMS(I+1)-FR_SMS(I)
        END IF
      ENDDO
C
C   echange messages
C
      DO I=1,NSPMD
       IF(I/=LOC_PROC.AND.FR_SMS(I+1)-FR_SMS(I).GT.0)THEN
          MSGTYP = MSGOFF 
          SIZ = FR_SMS(I+1)-FR_SMS(I)
c          print *,'mij-isend',loc_proc,i,siz
          L = FR_SMS(I)
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF        
      ENDDO
C
      DO I = 1, NSPMD
        IF(FR_RMS(I+1)-FR_RMS(I).GT.0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
        ENDIF
      END DO
C
      DO I = 1, NSPMD
        IF(I/=LOC_PROC.AND.FR_SMS(I+1)-FR_SMS(I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
C--------------------------------------------------------------------
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_GLOB_LMIN                source/mpi/ams/spmd_sms.F     
Chd|-- called by -----------
Chd|        SMS_CHECK                     source/ams/sms_fsa_inv.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_GLOB_LMIN(LMIN,IMIN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "mpif.h"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IMIN
      my_real LMIN
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGTYP,I,LOC_PROC,IERROR,
     .        SIZ,STATUS(MPI_STATUS_SIZE)
      DOUBLE PRECISION SBUF(2)
      INTEGER MSGOFF
      DATA MSGOFF/17009/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      SIZ = 2
      IF(LOC_PROC==1)THEN
        DO I=2,NSPMD
          MSGTYP = MSGOFF
          CALL MPI_RECV(
     S      SBUF,SIZ,MPI_DOUBLE_PRECISION,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,STATUS,IERROR)
          IF(SBUF(1)<LMIN)THEN
            LMIN=SBUF(1)
            IMIN=NINT(SBUF(2))
          END IF
        END DO
      ELSE
        SBUF(1)=LMIN
        SBUF(2)=IMIN
        MSGTYP = MSGOFF 
        CALL MPI_SEND(
     S    SBUF,SIZ,MPI_DOUBLE_PRECISION,IT_SPMD(1),MSGTYP,
     G    MPI_COMM_WORLD,IERROR)
      END IF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_GLOB_LMAX                source/mpi/ams/spmd_sms.F     
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_GLOB_LMAX(LMAX,IMAX)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "mpif.h"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IMAX
      my_real LMAX
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGTYP,I,LOC_PROC,IERROR,
     .        SIZ,STATUS(MPI_STATUS_SIZE)
      DOUBLE PRECISION SBUF(2)
      INTEGER MSGOFF
      DATA MSGOFF/17010/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      SIZ = 2
      IF(LOC_PROC==1)THEN
        DO I=2,NSPMD
          MSGTYP = MSGOFF                              
          CALL MPI_RECV(
     S      SBUF,SIZ,MPI_DOUBLE_PRECISION,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,STATUS,IERROR)
          IF(SBUF(1) > LMAX)THEN
            LMAX=SBUF(1)
            IMAX=NINT(SBUF(2))
          END IF
        END DO
      ELSE
        SBUF(1)=LMAX
        SBUF(2)=IMAX
        MSGTYP = MSGOFF                              
        CALL MPI_SEND(
     S    SBUF,SIZ,MPI_DOUBLE_PRECISION,IT_SPMD(1),MSGTYP,
     G    MPI_COMM_WORLD,IERROR)
      END IF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_NNDFT_SMS                source/mpi/ams/spmd_sms.F     
Chd|-- called by -----------
Chd|        SMS_CHECK                     source/ams/sms_fsa_inv.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_NNDFT_SMS(
     1           FR_SMS ,FR_RMS,LIST_SMS,LIST_RMS,IAD_ELEM,
     2           FR_ELEM,NNDFT0,NNDFT1,ISORTND)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "mpif.h"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FR_SMS(NSPMD+1), FR_RMS(NSPMD+1), 
     .        LIST_SMS(*), LIST_RMS(*), IAD_ELEM(2,*), FR_ELEM(*),
     .        NNDFT0,NNDFT1,ISORTND(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGTYP,I,LOC_PROC,IERROR,
     .        SIZ,J,L,M,NOD,
     .        ITAG(NUMNOD),NNDFT(NSPMD),MNDFT(NSPMD),KSORT,
     .        STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD),REQ_S(NSPMD),
     .        SBUF(MAX(2*FR_SMS(NSPMD+1),FR_RMS(NSPMD+1))), 
     .        RBUF(MAX(2*FR_RMS(NSPMD+1),FR_SMS(NSPMD+1)))
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
      DO I=1,NUMNOD
        ITAG(I)=0
      END DO
C
      NNDFT0=0
      DO J=IAD_ELEM(1,1),IAD_ELEM(1,ISPMD+1)-1
        NOD=FR_ELEM(J)
        IF(ITAG(NOD)==0)THEN
          NNDFT0=NNDFT0+1
          ISORTND(NNDFT0)=NOD
          ITAG(NOD)=1
        END IF
      END DO

      DO J=IAD_ELEM(1,ISPMD+1),IAD_ELEM(1,NSPMD+1)-1
        NOD=FR_ELEM(J)
        ITAG(NOD)=1
      END DO

      KSORT=NNDFT0
      DO NOD=1,NUMNOD
        IF(ITAG(NOD)==0)THEN
          KSORT=KSORT+1
          ISORTND(KSORT)=NOD
        END IF
      END DO
      DO J=IAD_ELEM(1,1),IAD_ELEM(1,NSPMD+1)-1
        NOD=FR_ELEM(J)
        ITAG(NOD)=0
      END DO

      DO J=IAD_ELEM(1,1),IAD_ELEM(1,ISPMD+1)-1
        NOD=FR_ELEM(J)
        ITAG(NOD)=1
      END DO
      NNDFT1=0
      DO J=IAD_ELEM(1,ISPMD+1),IAD_ELEM(1,NSPMD+1)-1
        NOD=FR_ELEM(J)
        IF(ITAG(NOD)==0)THEN
          NNDFT1=NNDFT1+1
          KSORT =KSORT+1
          ISORTND(KSORT)=NOD
          ITAG(NOD)=1
        END IF
      END DO
C--------------------------------------------------------------------
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_NNZ_SMS                  source/mpi/ams/spmd_sms.F     
Chd|-- called by -----------
Chd|        SMS_CHECK                     source/ams/sms_fsa_inv.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_NNZ_SMS(
     1           FR_SMS ,FR_RMS,LIST_SMS,LIST_RMS,IAD_ELEM,
     2           FR_ELEM,NNZM  ,IADK    ,KADM    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "mpif.h"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FR_SMS(NSPMD+1), FR_RMS(NSPMD+1), 
     .        LIST_SMS(*), LIST_RMS(*), IAD_ELEM(2,*), FR_ELEM(*),
     .        NNZM, IADK(*), KADM(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGTYP,I,LOC_PROC,IERROR,
     .        SIZ,J,L,M,NOD,P,
     .        NNZP(NSPMD),MNZP(NSPMD),
     .        STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD),REQ_S(NSPMD),
     .        SBUF(IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)), 
     .        RBUF(IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1))
      INTEGER MSGOFF,MSGOFF2
      DATA MSGOFF/17002/
      DATA MSGOFF2/17003/


C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
      L = 1
      M = 1
      SIZ=1
      DO P=1,NSPMD
        NNZP(P) = 0
        MNZP(P) = 0
        IF(P/=LOC_PROC)THEN
C
          DO J=IAD_ELEM(1,P), IAD_ELEM(1,P+1)-1
            NOD = FR_ELEM(J)
            NNZP(P) = NNZP(P) + IADK(NOD+1)-IADK(NOD)
          END DO
C
          !THIS IS A BUG => DEADLOCK
          MSGTYP = MSGOFF 
          CALL MPI_SEND(
     S      NNZP(P),SIZ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,IERROR)
          MSGTYP = MSGOFF 
          CALL MPI_RECV(
     S      MNZP(P),SIZ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     G      MPI_COMM_WORLD,STATUS,IERROR)
        END IF
      END DO
C
C   echange messages
C
c      SIZ=1
c      CALL MPI_ALLTOALL(NNZP,SIZ,MPI_INTEGER,MNZP,SIZ,MPI_INTEGER,
c     .  		MPI_COMM_WORLD,IERROR)
C
C sur dimensionne
      DO P = 1, NSPMD
       NNZM=NNZM+MNZP(P)
      END DO
C-----
      L = 1
      DO P=1,NSPMD
C
	SIZ = IAD_ELEM(1,P+1)-IAD_ELEM(1,P)
	IF(SIZ/=0)THEN
	  MSGTYP = MSGOFF2 
	  CALL MPI_IRECV(
     .      RBUF(L),SIZ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .      MPI_COMM_WORLD,REQ_R(P),IERROR    )
C
	  DO J=IAD_ELEM(1,P), IAD_ELEM(1,P+1)-1
	    NOD = FR_ELEM(J)
	    SBUF(J)=IADK(NOD+1)-IADK(NOD)
	  END DO
C
	  MSGTYP= MSGOFF2
	  CALL MPI_ISEND(
     .      SBUF(L),SIZ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .      MPI_COMM_WORLD,REQ_S(P),IERROR    )
	  L = L + SIZ
	END IF
C
      END DO
C
      L = 1
      DO P=1,NSPMD
	SIZ = IAD_ELEM(1,P+1)-IAD_ELEM(1,P)
	IF(SIZ/=0)THEN
	  CALL MPI_WAIT(REQ_R(P),STATUS,IERROR)
	  DO J=IAD_ELEM(1,P),IAD_ELEM(1,P+1)-1
	    NOD = FR_ELEM(J)
	    KADM(NOD)=KADM(NOD)+RBUF(L)
	    L = L + 1
	  END DO
	END IF
      END DO
C
      DO P=1,NSPMD
	IF(IAD_ELEM(1,P+1)-IAD_ELEM(1,P)/=0)THEN
	  CALL MPI_WAIT(REQ_S(P),STATUS,IERROR)
	END IF
      END DO
C--------------------------------------------------------------------
      RETURN
      END
C
C
Chd|====================================================================
Chd|  SPMD_EXCHM_SMS                source/mpi/ams/spmd_sms.F     
Chd|-- called by -----------
Chd|        SMS_CHECK                     source/ams/sms_fsa_inv.F      
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_EXCHM_SMS(
     1           FR_SMS ,FR_RMS,LIST_SMS,LIST_RMS,IAD_ELEM,
     2           FR_ELEM,IADK  ,JDIK    ,LT_K    ,KADM    , 
     3           JDIM   ,LT_M  ,INVND   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "mpif.h"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FR_SMS(NSPMD+1), FR_RMS(NSPMD+1), 
     .        LIST_SMS(*), LIST_RMS(*), IAD_ELEM(2,*), FR_ELEM(*),
     .        IADK(*), JDIK(*), KADM(*), JDIM(*), INVND(*)
      my_real
     .        LT_K(*), LT_M(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGTYP,I,LOC_PROC,IERROR,
     .        SIZ,J,K,L,M,N,NOD,P,LL,
     .        SIZS(NSPMD),SIZR(NSPMD),
     .        STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD), REQ_S(NSPMD), REQ_S2(NSPMD), 
     .        ITAG(NUMNOD)
      my_real, 
     .        DIMENSION(:), ALLOCATABLE :: SBUF, RBUF
      INTEGER MSGOFF,MSGOFF2
      DATA MSGOFF/17004/
      DATA MSGOFF2/17005/       
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C-----
      DO I=1,NUMNOD
        ITAG(I)=0
      END DO
C
      SIZ = 0
      DO P=1,NSPMD
        SIZR(P)=0
        SIZS(P)=0
        IF(P/=LOC_PROC)THEN
C
          DO J=IAD_ELEM(1,P), IAD_ELEM(1,P+1)-1
            NOD = FR_ELEM(J)
            ITAG(NOD)=J-IAD_ELEM(1,P)+1
          END DO
C
          MSGTYP = MSGOFF                        
          CALL MPI_IRECV(
     .      SIZR(P),1,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .      MPI_COMM_WORLD,REQ_R(P),IERROR    )
C
          IF(IAD_ELEM(1,P+1)-IAD_ELEM(1,P)/=0)THEN
            DO J=IAD_ELEM(1,P), IAD_ELEM(1,P+1)-1
              NOD = FR_ELEM(J)
              DO K=IADK(NOD),IADK(NOD+1)-1
                M=JDIK(K)
                IF(ITAG(M)/=0.AND.ITAG(M)<ITAG(NOD))THEN
                  SIZS(P) = SIZS(P) + 3
                END IF
              END DO
            END DO
          END IF
C
C reset
          DO J=IAD_ELEM(1,P), IAD_ELEM(1,P+1)-1
            NOD = FR_ELEM(J)
            ITAG(NOD)=0
          END DO
C
          MSGTYP = MSGOFF                               
          CALL MPI_ISEND(
     .      SIZS(P),1,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .      MPI_COMM_WORLD,REQ_S(P),IERROR    )
C
          SIZ = SIZ + SIZS(P)
        END IF
      END DO
C
      ALLOCATE(SBUF(SIZ),STAT=IERROR)
      IF(IERROR.NE.0) THEN
        CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .              C1='FOR FSAI')
        CALL ARRET(2)
      END IF
C
      SIZ = 0
      DO P=1,NSPMD
        IF(P/=LOC_PROC)THEN
          CALL MPI_WAIT(REQ_R(P),STATUS,IERROR)
          SIZ = SIZ +SIZR(P)
        END IF
      END DO
C
      ALLOCATE(RBUF(SIZ),STAT=IERROR)
      IF(IERROR.NE.0) THEN
        CALL ANCMSG(MSGID=19,ANMODE=ANINFO,
     .              C1='FOR FSAI')
        CALL ARRET(2)
      END IF
C
      L = 1
      DO P=1,NSPMD
        IF(P/=LOC_PROC)THEN
          CALL MPI_WAIT(REQ_R(P),STATUS,IERROR)
          IF(SIZR(P)/=0)THEN
            MSGTYP = MSGOFF2                          
            CALL MPI_IRECV(RBUF(L),SIZR(P),REAL ,IT_SPMD(P),
     .    		  MSGTYP,MPI_COMM_WORLD,REQ_R(P),IERROR)
            L = L +SIZR(P)
          END IF
        END IF
      END DO
C
      L = 1
      LL = 1
      DO P=1,NSPMD
        IF(SIZS(P)/=0)THEN
C
          DO J=IAD_ELEM(1,P), IAD_ELEM(1,P+1)-1
            NOD = FR_ELEM(J)
            ITAG(NOD)=J-IAD_ELEM(1,P)+1
          END DO
C
          DO J=IAD_ELEM(1,P), IAD_ELEM(1,P+1)-1
            NOD = FR_ELEM(J)
            DO K=IADK(NOD),IADK(NOD+1)-1
              M=JDIK(K)
              IF(ITAG(M)/=0.AND.ITAG(M)<ITAG(NOD))THEN
        	SBUF(LL  ) = ITAG(NOD)
        	SBUF(LL+1) = ITAG(M)
        	SBUF(LL+2) = LT_K(K)
        	LL = LL + 3
              END IF
            END DO
          END DO
C
          MSGTYP = MSGOFF2                               
          CALL MPI_ISEND(SBUF(L),SIZS(P),REAL ,IT_SPMD(P),
     .  		MSGTYP,MPI_COMM_WORLD,REQ_S2(P),IERROR)
          L = L + SIZS(P)
C
C reset
          DO J=IAD_ELEM(1,P), IAD_ELEM(1,P+1)-1
            NOD = FR_ELEM(J)
            ITAG(NOD)=0
          END DO
C
        END IF
      END DO
C
      L = 1
      DO P=1,NSPMD
        IF(SIZR(P)/=0)THEN
          CALL MPI_WAIT(REQ_R(P),STATUS,IERROR)
          DO J=1,SIZR(P)/3
            N = NINT(RBUF(L))
            N = FR_ELEM(IAD_ELEM(1,P) + N - 1)
            N = INVND(N)
            M = NINT(RBUF(L+1))
            M = FR_ELEM(IAD_ELEM(1,P) + M - 1)
            M = INVND(M)
            JDIM(KADM(N)) = M
            LT_M(KADM(N))=RBUF(L+2)
            KADM(N)=KADM(N)+1
            L = L + 3
          END DO
        END IF
      END DO
C
      DO P=1,NSPMD
        IF(P/=LOC_PROC)THEN
          CALL MPI_WAIT(REQ_S(P),STATUS,IERROR)
        END IF
        IF(SIZS(P)/=0)THEN
          CALL MPI_WAIT(REQ_S2(P),STATUS,IERROR)
        END IF
      END DO
C--------------------------------------------------------------------
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_EXCH_AWORK               source/mpi/ams/spmd_sms.F     
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_AWORK(
     1   A      ,IAD_ELEM ,FR_ELEM,SIZE,LENR     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "mpif.h"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr06_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_ELEM(2,*),FR_ELEM(*), SIZE, LENR
      my_real
     .        A(3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR,MSGOFF,
     .        SIZ,J,K,L,NB_NOD,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      DATA MSGOFF/17000/

      my_real
     .        RBUF(SIZE*LENR ),
     .        SBUF(SIZE*LENR )
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      L = 1
      IAD_RECV(1) = 1
      DO I=1,NSPMD
        SIZ = SIZE*(IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
        IF(SIZ.NE.0)THEN
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      END DO
      L = 1
      IAD_SEND(1) = 1
      DO I=1,NSPMD
C preparation envoi  partie fixe (elem) a proc I
#include      "vectorize.inc"
         DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
           NOD = FR_ELEM(J)
           SBUF(L  ) =  A(1,NOD)
           SBUF(L+1) =  A(2,NOD)
           SBUF(L+2) =  A(3,NOD)
           L = L + SIZE
         END DO
C
        IAD_SEND(I+1) = L
      ENDDO
C
C   echange messages
C
      DO I=1,NSPMD
C--------------------------------------------------------------------
C envoi a N+I mod P
Cel test si msg necessaire a envoyer a completer par test interface
       IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I).GT.0)THEN
          MSGTYP = MSGOFF 
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L = IAD_SEND(I)
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF
C--------------------------------------------------------------------
      ENDDO
C
C decompactage
C
      DO I = 1, NSPMD
Cel test si msg necessaire a envoyer a completer par test interface
        NB_NOD = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(NB_NOD.GT.0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          L = IAD_RECV(I)
#include        "vectorize.inc"
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            NOD = FR_ELEM(J)
            A(1,NOD) = A(1,NOD) + RBUF(L)
            A(2,NOD) = A(2,NOD) + RBUF(L+1)
            A(3,NOD) = A(3,NOD) + RBUF(L+2)
            L = L + SIZE
          END DO
C ---
        ENDIF
      END DO
C
Cel wait terminaison isend
C
      DO I = 1, NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_EXCH_RBE3_NODNX          source/mpi/ams/spmd_sms.F     
Chd|-- called by -----------
Chd|        SMS_RBE3_NODXI                source/ams/sms_rbe3.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_RBE3_NODNX(
     1   NODNX_SMS,FR_M   ,IAD_M ,LCOMM  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LCOMM, FR_M(*), IAD_M(*), NODNX_SMS(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGTYP,LOC_PROC,NOD,I,J,L,IDEB,IAD,LEN,
     .        NBINDEX,INDEX,MSGOFF,SIZ,IERROR,K,
     .        STATUS(MPI_STATUS_SIZE),ISIZE6,
     .        REQ_S(NSPMD),REQ_R(NSPMD),INDEXI(NSPMD)
      DATA MSGOFF/17001/
      INTEGER SBUF(LCOMM), RBUF(LCOMM)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
      IDEB = 1
      L = 0
      DO I = 1, NSPMD
        SIZ = IAD_M(I+1)-IAD_M(I)
        IF(SIZ.GT.0) THEN
          L=L+1
          INDEXI(L)=I
          MSGTYP = MSGOFF                         
          CALL MPI_IRECV(
     S      RBUF(IDEB),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(L),IERROR)
          IDEB = IDEB + SIZ
        ENDIF
      ENDDO
      NBINDEX = L
C
      IDEB = 1
      DO L = 1, NBINDEX
        I = INDEXI(L)
        LEN = IAD_M(I+1) - IAD_M(I)
        IAD = IAD_M(I)-1
#include      "vectorize.inc"
        DO J = 1, LEN
          NOD = FR_M(IAD+J)
          SBUF(IDEB)   = NODNX_SMS(NOD)
          IDEB = IDEB + 1
        ENDDO
      ENDDO
C
      IDEB = 1
      DO L=1,NBINDEX
        I = INDEXI(L)
        SIZ = IAD_M(I+1)-IAD_M(I)
        MSGTYP = MSGOFF 
        CALL MPI_ISEND(
     S    SBUF(IDEB),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G    MPI_COMM_WORLD,REQ_S(L),IERROR)
        IDEB = IDEB + SIZ
      ENDDO
C
      DO L=1,NBINDEX
        CALL MPI_WAITANY(NBINDEX,REQ_R,INDEX,STATUS,IERROR)
        I = INDEXI(INDEX)
        IDEB = 1+(IAD_M(I)-1)
        LEN = IAD_M(I+1)-IAD_M(I)
        IAD = IAD_M(I)-1
#include      "vectorize.inc"
        DO J = 1, LEN
          NOD = FR_M(IAD+J)
          NODNX_SMS(NOD)=NODNX_SMS(NOD)+RBUF(J)
          IDEB = IDEB + 1
        ENDDO
      ENDDO
C
      DO L=1,NBINDEX
        CALL MPI_WAITANY(NBINDEX,REQ_S,INDEX,STATUS,IERROR)
      ENDDO
C
      RETURN
      END
C
#elif 1
C
Chd|====================================================================
Chd|  SPMD_NLIST_SMS                source/mpi/ams/spmd_sms.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_NLIST_SMS(IDUM1,IDUM2)
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
      INTEGER
     .        IDUM1, IDUM2, IDUM3, IDUM4, IDUM5,
     .        IDUM6, IDUM7, IDUM8, IDUM9, IDUM10
      my_real
     .        RDUM1, RDUM2, RDUM3, RDUM4, RDUM5,
     .        RDUM6
      ENTRY SPMD_LIST_SMS(
     1   IDUM1,IDUM2,IDUM3,IDUM4,IDUM5,
     2   IDUM6,IDUM7)
      ENTRY SPMD_MIJ_SMS(
     1   IDUM1,IDUM2,IDUM3,IDUM4,RDUM1,
     2   RDUM2)
      ENTRY SPMD_GLOB_LMIN(RDUM1,IDUM1)
      ENTRY SPMD_GLOB_LMAX(RDUM1,IDUM1)
      ENTRY SPMD_NNDFT_SMS(
     1           IDUM1, IDUM2, IDUM3, IDUM4, IDUM5,
     2           IDUM6, IDUM7, IDUM8, IDUM9)
      ENTRY SPMD_NNZ_SMS(
     1           IDUM1, IDUM2, IDUM3, IDUM4, IDUM5,
     2           IDUM6, IDUM7, IDUM8, IDUM9)
      ENTRY SPMD_EXCHM_SMS(
     1           IDUM1, IDUM2, IDUM3, IDUM4, IDUM5,
     2           IDUM6, IDUM7, IDUM8, RDUM1, IDUM9,
     3           IDUM10,RDUM2)
      ENTRY SPMD_EXCH_AWORK(
     1   RDUM1  ,IDUM1 ,IDUM2  ,IDUM3  ,IDUM4 )
      ENTRY SPMD_EXCH_RBE3_NODNX(
     1   IDUM1, IDUM2, IDUM3, IDUM4)
      END

#endif

