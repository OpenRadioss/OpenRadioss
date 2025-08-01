Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    i25main_norm    ../engine/source/interfaces/int25/i25main_norm.F
!||--- called by ------------------------------------------------------
!||    inttri          ../engine/source/interfaces/intsort/inttri.F
!||--- calls      -----------------------------------------------------
!||    i25assnp        ../engine/source/interfaces/int25/i25norm.F
!||    i25normp        ../engine/source/interfaces/int25/i25norm.F
!||    i25tagn         ../engine/source/interfaces/int25/i25norm.F
!||    my_barrier      ../engine/source/system/machine.F
!||--- uses       -----------------------------------------------------
!||    intbufdef_mod   ../common_source/modules/interfaces/intbufdef_mod.F90
!||    mpi_commod      ../engine/share/modules/mpi_comm_mod.F
!||    sensor_mod      ../common_source/modules/sensor_mod.F90
!||====================================================================
      SUBROUTINE I25MAIN_NORM(
     1                   INTLIST25,IPARI   ,INTBUF_TAB ,JTASK    ,X       ,
     2                   ITAB    ,NSENSOR  ,SENSOR_TAB,IAD_FRNOR    ,FR_NOR   ,
     3                   IAD_FREDG,FR_EDG,IAD_ELEM,FR_ELEM,FSKYN25,
     4                   ADDCSRECT,PROCNOR)
C=======================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MPI_COMMOD
      USE INTBUFDEF_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: JTASK,NSENSOR
      INTEGER IPARI(NPARI,NINTER), INTLIST25(*), ITAB(*),
     .        IAD_FRNOR(NINTER25,*), FR_NOR(*), IAD_FREDG(NINTER25,*), FR_EDG(*), 
     .        IAD_ELEM(2,*), FR_ELEM(*),ADDCSRECT(*), PROCNOR(*)
C     REAL
      my_real  :: X(3,*)
      TYPE(INTBUF_STRUCT_),DIMENSION(NINTER) :: INTBUF_TAB
      TYPE(MPI_COMM_NOR_STRUCT) , DIMENSION(NINTER25) :: BUFFERS
      REAL*4 FSKYN25(3,*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) ,INTENT(IN) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NTY, ISENS, SIZE, NI25, NADMSR, IEDGE, NADMSRF6, NADMSRL6,
     .        NRTM, NSN, NSNR, NIN, NRTM_SH, NRTM0, FLAGREMN ,NCSHIFT ,ISHIFT,
     .        SIZE_NODNOR_E2S,I,J,NOD,CC,SOL_EDGE
      INTEGER :: NEDGE
C     REAL
      my_real
     .   TS, STARTT, STOPT
      REAL*4 RZERO
C-----------------------------------------------
      RZERO = 0.
C
C Calcul // des normales
C
      IF(JTASK==1) THEN
        DO I=1,NINTER25
            N = INTLIST25(I)
            NRTM   = IPARI(4,N)
            ALLOCATE(INTBUF_TAB(N)%TAGNOD(NUMNOD), INTBUF_TAB(N)%WNOD_NORMAL(3,4,NRTM))
            ALLOCATE(INTBUF_TAB(N)%TAGE(NRTM))
        ENDDO
      END IF
C
      CALL MY_BARRIER()
C
      NCSHIFT = 1
      ISHIFT = 0
      DO NI25=1,NINTER25
     
        N = INTLIST25(NI25)
C
        STARTT=INTBUF_TAB(N)%VARIABLES(3)
        STOPT =INTBUF_TAB(N)%VARIABLES(11)
        IF(STARTT>TT) CYCLE
        IF(TT>STOPT)  CYCLE
C
        ISENS = IPARI(64,N)  
        IF (ISENS > 0)  THEN         ! Interface activated by sensor
           TS = SENSOR_TAB(ISENS)%TSTART
        ELSE
           TS = TT
        ENDIF
        IF(TT<TS) CYCLE
C
        IF(JTASK == 1) THEN

          SIZE_NODNOR_E2S = 0

         IEDGE  = IPARI(58,N)
         SOL_EDGE =0
         IF(IEDGE /= 0) SOL_EDGE =IEDGE/10 ! solids

         IF(SOL_EDGE/=0)THEN
          DO I = 1, NSPMD
            IF(IAD_FRNOR(NI25,I+1)-IAD_FRNOR(NI25,I)>0) THEN
              DO J=IAD_FRNOR(NI25,I),IAD_FRNOR(NI25,I+1)-1
                NOD = ISHIFT + FR_NOR(J)
                DO CC = ADDCSRECT(NOD),ADDCSRECT(NOD+1)-1
                  IF(PROCNOR(CC)==I) THEN
                     SIZE_NODNOR_E2S = SIZE_NODNOR_E2S + 6
                  ENDIF
                END DO
              END DO
             ENDIF
          ENDDO
         ENDIF

          ALLOCATE(BUFFERS(NI25)%SEND_BUF(   3*(IAD_FREDG(NI25,NSPMD+1)-IAD_FREDG(NI25,1))
     .                                    +2*3*(IAD_FRNOR(NI25,NSPMD+1)-IAD_FRNOR(NI25,1))
     .                                    +    (IAD_FRNOR(NI25,NSPMD+1)-IAD_FRNOR(NI25,1))
     .                                    +     SIZE_NODNOR_E2S))
          ALLOCATE(BUFFERS(NI25)%RECV_BUF(   3*(IAD_FREDG(NI25,NSPMD+1)-IAD_FREDG(NI25,1))
     .                                    +2*3*(IAD_FRNOR(NI25,NSPMD+1)-IAD_FRNOR(NI25,1))
     .                                    +    (IAD_FRNOR(NI25,NSPMD+1)-IAD_FRNOR(NI25,1))
     .                                    +     SIZE_NODNOR_E2S))

          ALLOCATE(BUFFERS(NI25)%ISINDEX(NSPMD))
          ALLOCATE(BUFFERS(NI25)%IRINDEX(NSPMD))
          ALLOCATE(BUFFERS(NI25)%SEND_RQ(NSPMD))
          ALLOCATE(BUFFERS(NI25)%RECV_RQ(NSPMD))
          ALLOCATE(BUFFERS(NI25)%IAD_RECV(NSPMD+1))
          ALLOCATE(BUFFERS(NI25)%IAD_SEND(NSPMD+1))
        ENDIF
C
        NADMSR = IPARI(67,N)
        NRTM   = IPARI(4,N)
        NSN    = IPARI(5,N)
        NSNR   = IPARI(24,N)

        
 
        NRTM_SH=IPARI(42,N)
        NRTM0  =NRTM-NRTM_SH
        FLAGREMN = IPARI(63,N)

        IEDGE  = IPARI(58,N)
        NEDGE  = IPARI(68,N) ! Updated in i25normp
        SOL_EDGE =0
        IF(IEDGE /= 0) SOL_EDGE =IEDGE/10 ! solids

        CALL I25TAGN(NI25    ,N       ,NRTM     ,NSN         ,NSNR      ,
     2               JTASK   ,IAD_FRNOR ,FR_NOR  ,INTBUF_TAB(N)%IRTLM,INTBUF_TAB(N)%MSEGTYP24  ,
     3               INTBUF_TAB(N)%I_STOK(2), INTBUF_TAB(N)%I_STOK(3), INTBUF_TAB(N)%CAND_OPT_E, 
     .                           INTBUF_TAB(N)%STFNS, INTBUF_TAB(N)%ACTNOR,INTBUF_TAB(N)%IRECTM,    
     4               INTBUF_TAB(N)%TAGNOD,IAD_ELEM,FR_ELEM ,INTBUF_TAB(N)%ADMSR,INTBUF_TAB(N)%KNOR2MSR, 
     5               INTBUF_TAB(N)%NOR2MSR,FLAGREMN,INTBUF_TAB(N)%KREMNOR,INTBUF_TAB(N)%REMNOR,
     6               IEDGE, NEDGE, INTBUF_TAB(N)%LEDGE,INTBUF_TAB(N)%NRTM_FREE,
     .                                    INTBUF_TAB(N)%FREE_IRECT_ID,INTBUF_TAB(N)%I_STOK_E(2),
     7               INTBUF_TAB(N)%CANDM_E2S,INTBUF_TAB(N)%CANDS_E2S,INTBUF_TAB(N)%MVOISIN,
     8               INTBUF_TAB(N)%E2S_ACTNOR,NADMSR,INTBUF_TAB(N)%STFM,
     8               INTBUF_TAB(N)%NUMBER_EDGE_TYPE1,INTBUF_TAB(N)%NUMBER_EDGE_TYPE1_0,
     .               INTBUF_TAB(N)%EDGE_TYPE1,INTBUF_TAB(N)%EDGE_TYPE1_0 )


C
        NADMSRF6= 1+(JTASK-1)*6*NADMSR/ NTHREAD
        NADMSRL6= JTASK*6*NADMSR/NTHREAD
        INTBUF_TAB(N)%VTX_BISECTOR(NADMSRF6:NADMSRL6)=RZERO
C
        IF(JTASK==1) ALLOCATE(INTBUF_TAB(N)%TAGSEG(4,4*NRTM))

        CALL MY_BARRIER()
C

        CALL I25NORMP(
     1    NI25,NRTM,NRTM0,INTBUF_TAB(N)%IRECTM,X           ,
     2    INTBUF_TAB(N)%EDGE_BISECTOR,IPARI(6,N),INTBUF_TAB(N)%MSR   , 
     .                        JTASK,INTBUF_TAB(N)%STFM,INTBUF_TAB(N)%STFE,
     3    INTBUF_TAB(N)%ACTNOR,INTBUF_TAB(N)%MSEGTYP24,INTBUF_TAB(N)%TAGNOD,
     .                        INTBUF_TAB(N)%MVOISIN,INTBUF_TAB(N)%EVOISIN,
     4    IAD_FREDG,FR_EDG,INTBUF_TAB(N)%WNOD_NORMAL,BUFFERS(NI25),IEDGE,IPARI(68,N),
     5    INTBUF_TAB(N)%LEDGE,INTBUF_TAB(N)%LBOUND,NADMSR,INTBUF_TAB(N)%ADMSR,
     6    IAD_FRNOR,FR_NOR,INTBUF_TAB(N)%VTX_BISECTOR,1,
     7    INTBUF_TAB(N)%NB_TAGSEG, INTBUF_TAB(N)%TAGSEG,INTBUF_TAB(N)%TAGE,INTBUF_TAB(N)%FREE_IRECT_ID,INTBUF_TAB(N)%NRTM_FREE,
     8    FSKYN25(1,NCSHIFT),INTBUF_TAB(N)%IADNOR,ISHIFT,ADDCSRECT,PROCNOR,SOL_EDGE ,
     9    FSKYN25 )

C
        CALL MY_BARRIER()

        NCSHIFT=NCSHIFT+INTBUF_TAB(N)%ADSKYN(NADMSR+1)-1
        ISHIFT = ISHIFT + IPARI(67,N)

        IF(JTASK==1)  DEALLOCATE(INTBUF_TAB(N)%TAGSEG)

C
      END DO

C     ALLOCATE(INTBUF_TAB(I)%TAGSEG(1,1))

      NCSHIFT = 1
      ISHIFT = 0
      DO NI25=1,NINTER25
     
        N = INTLIST25(NI25)
C
        STARTT=INTBUF_TAB(N)%VARIABLES(3)
        STOPT =INTBUF_TAB(N)%VARIABLES(11)
        IF(STARTT>TT) CYCLE
        IF(TT>STOPT)  CYCLE
C
        ISENS = IPARI(64,N)  
        IF (ISENS > 0)  THEN         ! Interface activated by sensor
           TS = SENSOR_TAB(ISENS)%TSTART
        ELSE
           TS = TT
        ENDIF
        IF(TT<TS) CYCLE

        NADMSR = IPARI(67,N)
        NRTM   = IPARI(4,N)
        NSN    = IPARI(5,N)
        NSNR   = IPARI(24,N)

        NRTM_SH=IPARI(42,N)
        NRTM0  =NRTM-NRTM_SH

        IEDGE  = IPARI(58,N)
C       NEDGE  = IPARI(68,N) ! Updated in i25normp
        SOL_EDGE =0
        IF(IEDGE /= 0) SOL_EDGE =IEDGE/10 ! solids

        CALL MY_BARRIER()

        CALL I25NORMP(
     1    NI25,NRTM,NRTM0,INTBUF_TAB(N)%IRECTM,X           ,
     2    INTBUF_TAB(N)%EDGE_BISECTOR,IPARI(6,N),INTBUF_TAB(N)%MSR   , 
     .                    JTASK,INTBUF_TAB(N)%STFM,INTBUF_TAB(N)%STFE,
     3    INTBUF_TAB(N)%ACTNOR,INTBUF_TAB(N)%MSEGTYP24,INTBUF_TAB(N)%TAGNOD,
     .                    INTBUF_TAB(N)%MVOISIN,INTBUF_TAB(N)%EVOISIN,
     4    IAD_FREDG,FR_EDG,INTBUF_TAB(N)%WNOD_NORMAL,BUFFERS(NI25),IEDGE,IPARI(68,N),
     5    INTBUF_TAB(N)%LEDGE,INTBUF_TAB(N)%LBOUND,NADMSR,INTBUF_TAB(N)%ADMSR,
     6    IAD_FRNOR,FR_NOR,INTBUF_TAB(N)%VTX_BISECTOR,2,
     7    INTBUF_TAB(N)%NB_TAGSEG,INTBUF_TAB(N)%TAGSEG,INTBUF_TAB(N)%TAGE,INTBUF_TAB(N)%FREE_IRECT_ID,INTBUF_TAB(N)%NRTM_FREE,
     8    FSKYN25(1,NCSHIFT),INTBUF_TAB(N)%IADNOR,ISHIFT,ADDCSRECT,PROCNOR,SOL_EDGE,
     9    FSKYN25 )


        CALL MY_BARRIER()

        NCSHIFT=NCSHIFT+INTBUF_TAB(N)%ADSKYN(NADMSR+1)-1
        ISHIFT = ISHIFT + IPARI(67,N)

        IF(JTASK == 1) THEN
          DEALLOCATE(BUFFERS(NI25)%SEND_BUF)
          DEALLOCATE(BUFFERS(NI25)%RECV_BUF)
          DEALLOCATE(BUFFERS(NI25)%ISINDEX)
          DEALLOCATE(BUFFERS(NI25)%IRINDEX)
          DEALLOCATE(BUFFERS(NI25)%SEND_RQ)
          DEALLOCATE(BUFFERS(NI25)%RECV_RQ)
          DEALLOCATE(BUFFERS(NI25)%IAD_RECV)
          DEALLOCATE(BUFFERS(NI25)%IAD_SEND)
        ENDIF
        CALL MY_BARRIER()
      END DO
C

c         CALL STOPTIME(TIMERS,MACRO_TIMER_T25NORM)
      NCSHIFT = 1
      DO NI25=1,NINTER25
C
        N = INTLIST25(NI25)

        IEDGE  = IPARI(58,N)
C       NEDGE  = IPARI(68,N) ! Updated in i25normp
        SOL_EDGE =0
        IF(IEDGE /= 0) SOL_EDGE =IEDGE/10 ! solids

        IF(SOL_EDGE/=0)THEN

          ISENS = IPARI(64,N)  
          IF (ISENS > 0)  THEN         ! Interface activated by sensor
             TS = SENSOR_TAB(ISENS)%TSTART
          ELSE
             TS = TT
          ENDIF

          NADMSR = IPARI(67,N)
          IF(TT >= TS) THEN
            CALL I25ASSNP(
     1        JTASK  ,NADMSR      ,INTBUF_TAB(N)%E2S_NOD_NORMAL,INTBUF_TAB(N)%ADMSR,INTBUF_TAB(N)%ADSKYN,
     2        INTBUF_TAB(N)%IADNOR,INTBUF_TAB(N)%E2S_ACTNOR,FSKYN25(1,NCSHIFT))
          END IF

          NCSHIFT=NCSHIFT+INTBUF_TAB(N)%ADSKYN(NADMSR+1)-1
        ENDIF

      END DO
c          CALL STARTIME(TIMERS,MACRO_TIMER_T25NORM)  
C
      CALL MY_BARRIER()

      IF(JTASK==1)THEN
        DO I=1,NINTER25
           N = INTLIST25(I)
           DEALLOCATE(INTBUF_TAB(N)%TAGNOD, INTBUF_TAB(N)%WNOD_NORMAL,INTBUF_TAB(N)%TAGE)
        ENDDO
      ENDIF
C
      RETURN
      END
