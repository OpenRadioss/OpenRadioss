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
C
Chd|====================================================================
Chd|  W_ISPH                        source/restart/ddsplit/w_isph.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE W_ISPH(KXSP    ,IXSP    ,NUMSPH_L,CEPSP ,PROC   ,
     +                  NODLOCAL,NUMNOD_L,ISPCOND ,IPARG ,ISPHIO ,
     +                  LEN_IA,SLONFSPH_L,SLPRTSPH_L,IPARTSP,
     +                  LONFSPH,LPRTSPH, IBUFSSG_IO, CELSPH ,
     +                  NSPHSOL_L,FIRST_SPHSOL_L,SPH2SOL ,SOL2SPH,
     +                  IRST     ,NUMELS8_L,CEP ,CEL     ,SOL2SPH_TYP)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------      
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMSPH_L, PROC, LEN_IA, NUMNOD_L,
     .        KXSP(NISP,*), IXSP(KVOISPH,*), CEPSP(*), NODLOCAL(*),
     .        ISPCOND(*), IPARG(NPARG,*),
     .        ISPHIO(*), SLONFSPH_L, SLPRTSPH_L,
     .        IPARTSP(*),LONFSPH(*),LPRTSPH(*),
     .        IBUFSSG_IO(SIBUFSSG_IO),CELSPH(NUMSPH),
     .        NSPHSOL_L,FIRST_SPHSOL_L,SPH2SOL(*),SOL2SPH(2,*),
     .        IRST(3,*), NUMELS8_L,CEP(*),CEL(*),SOL2SPH_TYP(*)

      INTEGER, DIMENSION(:), ALLOCATABLE :: LPRTSPH_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: LONFSPH_L,IBUFSSG_IO_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: SPH2SOL_L,ISPSYM,SOL2SPH_TYPL
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: SOL2SPH_L,IRST_L,IXSP_L
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IE_L, NG, NG_L,
     .        KXSP_L(NISP,NUMSPH_L),
     .        SUIVSPH_L,IPRT,
     .        N,STAT, INULL, IUN
      INTEGER, DIMENSION(:), ALLOCATABLE ::  NOD2SP_L,NGLOCAL,ISPHIO_L
C-----------------------------------------------
!     allocate 1d array
      ALLOCATE( NOD2SP_L(NUMNOD_L),NGLOCAL(NGROUP) )
      ALLOCATE( ISPHIO_L(SISPHIO) )
! ------------------------------------
C
      ALLOCATE( IXSP_L(KVOISPH,NUMSPH_L)    ,STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='IXSP_L')
c inlet outl spmd
      IF(NSPHIO>0)THEN
        ALLOCATE(LONFSPH_L(SLONFSPH_L)    ,STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='DOMDEC')
        LONFSPH_L (1:SLONFSPH_L)= 0

        ALLOCATE(LPRTSPH_L(SLPRTSPH_L)    ,STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='DOMDEC')
        LPRTSPH_L(1:SLPRTSPH_L) = 0

        ALLOCATE(IBUFSSG_IO_L(SIBUFSSG_IO)    ,STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='DOMDEC')
        IBUFSSG_IO_L(1:SIBUFSSG_IO) = 0
      ENDIF 
c
      IF(NSPHSOL_L/=0)THEN
        ALLOCATE(SPH2SOL_L(NUMSPH_L)    ,STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='SPH2SOL_L')
        SPH2SOL_L (1:NUMSPH_L)= 0
        ALLOCATE(SOL2SPH_L(2,NUMELS8_L)    ,STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='SOL2SPH_L')
        SOL2SPH_L (1:2,1:NUMELS8_L)= 0
        ALLOCATE(IRST_L(3,NSPHSOL_L)    ,STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='IRST_L')
        IRST_L (1:3,1:NSPHSOL_L)= 0
        ALLOCATE(SOL2SPH_TYPL(NUMELS8_L)    ,STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='SOL2SPH_TYPL')
        SOL2SPH_TYPL (1:NUMELS8_L)= 0
      END IF
c
      IF(NSPCOND>0)THEN
        ALLOCATE(ISPSYM(NUMSPH_L*NSPCOND)    ,STAT=stat)
        IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='ISPSYM')
      END IF
c
      NG_L = 0
      DO NG = 1, NGROUP
        IF(IPARG(32,NG)==PROC) THEN
          NG_L = NG_L + 1
          NGLOCAL(NG) = NG_L
        ELSE
          NGLOCAL(NG) = 0
        END IF
      END DO
C
      DO I = 1, NUMNOD_L
        NOD2SP_L(I) = 0
      END DO
      DO I = 1, KVOISPH
        DO J = 1, NUMSPH_L
          IXSP_L(I,J) = 0
        END DO
      END DO
C
      IE_L = 0
C
      DO I = 1, NUMSPH
        IF(CEPSP(I)==PROC) THEN
          IE_L = IE_L + 1
c num locale des cellules SPH
          DO J = 1, NISP
            KXSP_L(J,IE_L) = KXSP(J,I)
          END DO
          KXSP_L(2,IE_L) = SIGN(NGLOCAL(SIGN(KXSP_L(2,IE_L),1)),
     .                                           KXSP_L(2,IE_L))
          KXSP_L(3,IE_L) = NODLOCAL(KXSP_L(3,IE_L))
          NOD2SP_L(KXSP_L(3,IE_L)) = IE_L
          DO J = 1, KXSP_L(5,IE_L)
            IXSP_L(J,IE_L) = NODLOCAL(IXSP(J,I))
          END DO
        END IF
      END DO
C
      CALL WRITE_I_C(KXSP_L,NUMSPH_L*NISP)
      INULL=0
      IUN  =1
      DO I=1,NBGAUGE*NISP
        CALL WRITE_I_C(INULL,IUN)
      END DO
      CALL WRITE_I_C(IXSP_L,NUMSPH_L*KVOISPH)
      DO I=1,NBGAUGE*KVOISPH
        CALL WRITE_I_C(INULL,IUN)
      END DO
      CALL WRITE_I_C(NOD2SP_L,NUMNOD_L)

      LEN_IA = LEN_IA + (NUMSPH_L+NBGAUGE)*NISP 
     .                + (NUMSPH_L+NBGAUGE)*KVOISPH + NUMNOD_L
C
      IF(NSPCOND>0)THEN
        DO I = 1, NSPCOND*NUMSPH_L
          ISPSYM(I) = 0
        END DO
        CALL WRITE_I_C(ISPSYM,NUMSPH_L*NSPCOND)
        CALL WRITE_I_C(ISPCOND,NISPCOND*NSPCOND)
        LEN_IA = LEN_IA + NUMSPH_L*NSPCOND + NISPCOND*NSPCOND
        DEALLOCATE(ISPSYM)
      END IF

      IF(NSPHIO > 0) THEN
c SLPRTSPH_L is already set to SLPRTSPH
        SUIVSPH_L = 0
        DO IPRT=1,NPART
         DO N=1,NUMSPH
          IF(CEPSP(N)==PROC.AND.IPARTSP(N)==IPRT.AND.
     .          (KXSP(2,N) > 0 .OR. (KXSP(2,N)/=0.AND.
     .           N >= FIRST_SPHSOL .AND. N < FIRST_SPHSOL+NSPHSOL)))THEN
           SUIVSPH_L=SUIVSPH_L+1
           LONFSPH_L(SUIVSPH_L)=CELSPH(N)
          ENDIF
         ENDDO
         LPRTSPH_L((IPRT-1)*2+1+2)=SUIVSPH_L
         DO N=1,NUMSPH
          IF(CEPSP(N)==PROC.AND.IPARTSP(N)==IPRT.AND.
     .       (KXSP(2,N) < 0.AND. 
     .           (N < FIRST_SPHSOL .OR. N >= FIRST_SPHSOL+NSPHSOL)))THEN
           SUIVSPH_L=SUIVSPH_L+1
           LONFSPH_L(SUIVSPH_L)=CELSPH(N)
          ENDIF
         ENDDO
         LPRTSPH_L((IPRT-1)*2+2+2)=SUIVSPH_L
        ENDDO

c on passe IBUFSSG_IO en local
        DO I = 1, SIBUFSSG_IO   
          IBUFSSG_IO_L(I) = NODLOCAL(IBUFSSG_IO(I))
        ENDDO    

c on passe ISPHIO en local pour les outlet definies par noeuds
        DO I = 1, NSPHIO
          DO N=1,NISPHIO
            ISPHIO_L(NISPHIO*(I-1)+N) = ISPHIO(NISPHIO*(I-1)+N)
          END DO   
          IF (ISPHIO(NISPHIO*(I-1)+12)==2) THEN
            ISPHIO_L(NISPHIO*(I-1)+13) = NODLOCAL(ISPHIO(NISPHIO*(I-1)+13))
            ISPHIO_L(NISPHIO*(I-1)+14) = NODLOCAL(ISPHIO(NISPHIO*(I-1)+14))
            ISPHIO_L(NISPHIO*(I-1)+15) = NODLOCAL(ISPHIO(NISPHIO*(I-1)+15))
          ENDIF
        ENDDO  
    
        CALL WRITE_I_C(ISPHIO_L,SISPHIO)
        CALL WRITE_I_C(LPRTSPH_L,SLPRTSPH_L)
        CALL WRITE_I_C(LONFSPH_L,SLONFSPH_L)
        CALL WRITE_I_C(IBUFSSG_IO_L,SIBUFSSG_IO)
        LEN_IA = LEN_IA + SISPHIO + SLPRTSPH_L + SLONFSPH_L + 
     .           SIBUFSSG_IO

        DEALLOCATE(LPRTSPH_L,LONFSPH_L,IBUFSSG_IO_L)
      END IF
C
      IF(NSPHSOL_L/=0)THEN

        IE_L = 0
        DO I = 1, NSPHSOL
          IF(CEPSP(FIRST_SPHSOL+I-1)==PROC) THEN
            IE_L = IE_L + 1
            IF(CEP(SPH2SOL(FIRST_SPHSOL+I-1))/=PROC)THEN
              write(6,'(A)') 
     .        'internal error - Solid and SPH not on the same domain'
              stop
            END IF
            SPH2SOL_L(FIRST_SPHSOL_L+IE_L-1)=
     .        CEL(SPH2SOL(FIRST_SPHSOL+I-1))
c
            IRST_L(1,IE_L)=IRST(1,I)
            IRST_L(2,IE_L)=IRST(2,I)
            IRST_L(3,IE_L)=IRST(3,I)
c
          END IF
        END DO
C
        IE_L = 0
        DO I=1,NUMELS8
          IF(CEP(I)==PROC)THEN
            IE_L = IE_L + 1
C           SOL2SPH(1,N)+1<=I<=SOLSPH(2,N) <=> N==SPH2SOL(I)
            SOL2SPH_L(1,IE_L)=CELSPH(SOL2SPH(1,I)+1)-1
            SOL2SPH_L(2,IE_L)=CELSPH(SOL2SPH(2,I))
            SOL2SPH_TYPL(IE_L)=SOL2SPH_TYP(I)
          END IF
        END DO
C
        CALL WRITE_I_C(SPH2SOL_L,NUMSPH_L)
        CALL WRITE_I_C(SOL2SPH_L,2*NUMELS8_L)
        CALL WRITE_I_C(IRST_L   ,3*NSPHSOL_L)
        CALL WRITE_I_C(SOL2SPH_TYPL,NUMELS8_L)
        LEN_IA = LEN_IA + NUMSPH_L + 2*NUMELS8_L + 3*NSPHSOL_L
C
        DEALLOCATE(SPH2SOL_L,SOL2SPH_L,IRST_L,SOL2SPH_TYPL)
C
      END IF
C
! ------------------------------------
!     deallocate 1d array
      DEALLOCATE(IXSP_L)
      DEALLOCATE( NOD2SP_L,NGLOCAL )
      DEALLOCATE( ISPHIO_L )
! ------------------------------------
C
      RETURN
      END
