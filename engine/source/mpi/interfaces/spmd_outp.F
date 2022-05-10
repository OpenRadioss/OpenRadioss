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
#include "hardware.inc"



Chd|====================================================================
Chd|  OUTP_ARSZ_SS                  source/mpi/interfaces/spmd_outp.F
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|-- calls ---------------
Chd|        COUNT_ARSZ_SS                 source/output/sty/outp_s_s.F  
Chd|====================================================================
      SUBROUTINE OUTP_ARSZ_SS(IPARG,DD_IAD,IPM,IXS,P0ARS,WASZ,WASZ_WR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M P I   I n c l u d e s
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "param_c.inc"
#include "com01_c.inc"
#include "scr16_c.inc"
#include "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),DD_IAD(NSPMD+1,*),P0ARS(2),WASZ(2),WASZ_WR(2),
     . IXS(NIXS,*),IPM(NPROPMI,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER NN,NG,SZP0(2*NSPGROUP+2),RSZP0(2*NSPGROUP+2),NGF,NGL,JJ,
     .        WASZ26,P0ARS26,MLW,NEL,NPT,JHBE,ISOLNOD, 
     .        NUVAR,I,LFT,LLT,NFT,ITY

      INTEGER IERROR,STATUS(MPI_STATUS_SIZE)
C-----------------------------------------------
      P0ARS = 0
      WASZ = 0
      CALL COUNT_ARSZ_SS(IPARG,DD_IAD,IPM,IXS,WASZ,SZP0)

      IF ( OUTP_SS(1)  == 1.OR.OUTP_SS(2)  == 1.OR.OUTP_SS(3)  == 1
     . .OR.OUTP_SS(4)  == 1.OR.OUTP_SS(5)  == 1.OR.OUTP_SS(6)  == 1 
     . .OR.OUTP_SS(7)  == 1.OR.OUTP_SS(25) == 1.OR.OUTP_SS(20) == 1
     . .OR.OUTP_SS(21) == 1.OR.OUTP_SS(22) == 1.OR.OUTP_SS(23) == 1
     . .OR.OUTP_SS(24) == 1.OR.OUTP_SS(26) == 1 ) THEN

        CALL MPI_REDUCE(SZP0,RSZP0,2*NSPGROUP+2,
     .                  MPI_INTEGER,MPI_SUM,IT_SPMD(1),
     .                  MPI_COMM_WORLD,IERROR )


        IF (ISPMD == 0) THEN
          P0ARS(1) =RSZP0(2*NSPGROUP+1)
          P0ARS(2) =RSZP0(2*NSPGROUP+2)
          WASZ_WR(:) = -1
          DO I=1,NSPGROUP
           WASZ_WR(1) = MAX(WASZ_WR(1),RSZP0(I)) 
           WASZ_WR(2) = MAX(WASZ_WR(2),RSZP0(NSPGROUP+I)) 
          ENDDO
          WASZ_WR(1) = WASZ_WR(1)+6
          WASZ_WR(2) = WASZ_WR(2)+6
        ELSE
          P0ARS(:) = 1 
          WASZ_WR(:) = 1
        ENDIF
      END IF
#endif
      RETURN
      END
 

Chd|====================================================================
Chd|  OUTP_ARSZ_CS                  source/mpi/interfaces/spmd_outp.F
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|-- calls ---------------
Chd|        COUNT_ARSZ_CS                 source/output/sty/outp_c_s.F  
Chd|====================================================================
      SUBROUTINE OUTP_ARSZ_CS(IPARG,IXC,IXTG,IGEO,IPM,DD_IAD,
     .                         P0ARS,WASZ,WASZ_WR) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M P I   I n c l u d e s
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr16_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXC(NIXC,*),IXTG(NIXTG,*),DD_IAD(NSPMD+1,*) ,
     .        IPARG(NPARG,*),IPM(NPROPMI,*),WASZ(2),IGEO(NPROPGI,*),
     ,        P0ARS(2),IUSER_FULL,J,WASZ_WR(2)

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER JJ,NGF,NGL,NN,ITY,IAD,NFT,LFT,LLT,NPT,
     .        IHBE,IGTYP,NUVAR,MLW,NG,NEL,NBX,I,NPG,MPT,
     .        SZP0(2*NSPGROUP+2),RSZP0(2*NSPGROUP+2),P0ARSZ26
      INTEGER IERROR,STATUS(MPI_STATUS_SIZE)


      P0ARS = 0
      WASZ = 0

      CALL COUNT_ARSZ_CS(IPARG,IXC,IXTG,IGEO,IPM,DD_IAD,
     .                         WASZ,SZP0)      

      IUSER_FULL = 0
      DO J=1,60
        IF(OUTP_CS(26 + J) == 1) IUSER_FULL = 1
      ENDDO

      IF ( OUTP_CS( 1) == 1.OR.OUTP_CS( 2) == 1.OR.OUTP_CS( 3) == 1
     . .OR.OUTP_CS( 4) == 1.OR.OUTP_CS( 7) == 1.OR.OUTP_CS(25) == 1
     . .OR.OUTP_CS(20) == 1.OR.OUTP_CS(21) == 1.OR.OUTP_CS(22) == 1
     . .OR.OUTP_CS(23) == 1.OR.OUTP_CS(24) == 1.OR.OUTP_CS(26) == 1
     . .OR.IUSER_FULL == 1) THEN


        CALL MPI_REDUCE(SZP0,RSZP0,2*NSPGROUP+2,
     .                  MPI_INTEGER,MPI_SUM,IT_SPMD(1),
     .                  MPI_COMM_WORLD,IERROR )

        IF (ISPMD == 0) THEN
          P0ARS(1:2) = RSZP0(2*NSPGROUP+1:2*NSPGROUP+2)
          WASZ_WR(1:2) = -1
          DO I=1,NSPGROUP
           WASZ_WR(1) = MAX(WASZ_WR(1),RSZP0(I)) 
           WASZ_WR(2) = MAX(WASZ_WR(2),RSZP0(NSPGROUP+I)) 
          ENDDO
          WASZ_WR(1) = WASZ_WR(1)+6
          WASZ_WR(2) = WASZ_WR(2)+6
        ELSE
          P0ARS = 1
          WASZ_WR(:) = 1
        ENDIF
      ENDIF
#endif
      RETURN
      END
c
Chd|====================================================================
Chd|  OUTP_ARSZ_ST                  source/mpi/interfaces/spmd_outp.F
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|-- calls ---------------
Chd|        COUNT_ARSZ_ST                 source/output/sty/outp_s_t.F  
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE OUTP_ARSZ_ST(IPARG,DD_IAD,WASZ,WASZ_WR,P0ARS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD         
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M P I   I n c l u d e s
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr16_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*), DD_IAD(NSPMD+1,*),WASZ(3),WASZ_WR(3),P0ARS(3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER NGL,ITY,NRL,KHBE,MLW,NPT,ISOLNOD,NGF,NN,NEL,
     .        NG,NLAY,NPTR,NPTS,NPTT,NPTG,JJ,I,
     .        SZP0(3*NSPGROUP+3),RSZP0(3*NSPGROUP+3)
      INTEGER IERROR,STATUS(MPI_STATUS_SIZE)
C=======================================================================
      WASZ = 0
      P0ARS = 0
c------------------------------
      IF (OUTP_ST(1)==1.OR.OUTP_ST(2)==1.OR.OUTP_ST(3)==1) THEN
        CALL COUNT_ARSZ_ST(IPARG,DD_IAD,WASZ,SZP0)

        CALL MPI_REDUCE(SZP0,RSZP0,3*NSPGROUP+3,
     .                  MPI_INTEGER,MPI_SUM,IT_SPMD(1),
     .                  MPI_COMM_WORLD,IERROR )
      
        IF (ISPMD == 0) THEN
          P0ARS(1:3) = RSZP0(3*NSPGROUP+1:3*NSPGROUP+3)+8
          WASZ_WR(1:3) = -1
          DO I=1,NSPGROUP
           WASZ_WR(1) = MAX(WASZ_WR(1),RSZP0(I)) 
           WASZ_WR(2) = MAX(WASZ_WR(2),RSZP0(NSPGROUP+I)) 
           WASZ_WR(3) = MAX(WASZ_WR(3),RSZP0(2*NSPGROUP+I)) 
          ENDDO
        ELSE
          P0ARS(:) = 1
          WASZ_WR(:) = 1
        ENDIF
      ENDIF
#endif
      RETURN
      END


Chd|====================================================================
Chd|  OUTP_ARSZ_CT                  source/mpi/interfaces/spmd_outp.F
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|-- calls ---------------
Chd|        COUNT_ARSZ_CT                 source/output/sty/outp_c_t.F  
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE OUTP_ARSZ_CT(IPARG,DD_IAD,WASZ,WASZP,WASZ_WR,ELBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M P I   I n c l u d e s
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IPARG(NPARG,*),DD_IAD(NSPMD+1,*),WASZ(3),WASZP(3),WASZ_WR(3)
        TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
        INTEGER NGF,NGL,NN,ITY,MLW,NEL,NPT,IHBE,NPG,MPT,NG,JJ,COUNT,I,
     .          SZP0(3*NSPGROUP+3),RSZP0(3*NSPGROUP+3),
     .          IL,NLAY,IGTYP,NPT_ALL,
     .          NPTR,NPTS
C-----------------------------------------------
      INTEGER IERROR,STATUS(MPI_STATUS_SIZE)

      WASZ = 0
      WASZP = 0
      WASZ_WR = 0
      CALL COUNT_ARSZ_CT(IPARG,DD_IAD,WASZ,SZP0,ELBUF_TAB)

      COUNT = 0
      DO I=1,30
        COUNT = COUNT + OUTP_CT(10+I)+OUTP_CT(50+I)+OUTP_CT(100+I)
      ENDDO

      IF ( OUTP_CT( 1) == 1.OR.OUTP_CT( 2) == 1.OR.OUTP_CT( 3) == 1
     . .OR.OUTP_CT( 4) == 1.OR.OUTP_CT( 5) == 1.OR.OUTP_CT( 6) == 1
     . .OR.OUTP_CT( 7) == 1.OR.OUTP_CT( 8) == 1.OR.OUTP_CT(91) == 1
     . .OR.OUTP_CT(92) == 1.OR.OUTP_CT(93) == 1.OR.OUTP_CT(94) == 1
     . .OR.COUNT.GT.0.OR.OUTP_CT(95)==1.OR.OUTP_CT(96)==1)   THEN


        CALL MPI_REDUCE(SZP0,RSZP0,3*NSPGROUP+3,
     .                  MPI_INTEGER,MPI_SUM,IT_SPMD(1),
     .                  MPI_COMM_WORLD,IERROR )

! ------
        IF ( OUTP_CT(95) == 1)   THEN
         IF (ISPMD == 0) THEN
           WASZP(2) = RSZP0(3*NSPGROUP+2) 
           DO I=1,NSPGROUP
            WASZ_WR(2) = MAX(WASZ_WR(2),RSZP0(NSPGROUP+I))
           ENDDO
         ELSE
           WASZP(2) = 1
           WASZ_WR(2) = 1
         ENDIF
        ENDIF
! ------
        IF ( OUTP_CT(96) == 1)   THEN
         IF (ISPMD == 0) THEN
           WASZP(3) = RSZP0(3*NSPGROUP+3)
           DO I=1,NSPGROUP
            WASZ_WR(3) = MAX(WASZ_WR(3),RSZP0(NSPGROUP+I))
           ENDDO
         ELSE
           WASZP(3) = 1
           WASZ_WR(3) = 1
         ENDIF
        ENDIF
! ------
        IF ( OUTP_CT( 1) == 1.OR.OUTP_CT( 2) == 1.OR.OUTP_CT( 3) == 1
     . .OR.OUTP_CT( 4) == 1.OR.OUTP_CT( 5) == 1.OR.OUTP_CT( 6) == 1
     . .OR.OUTP_CT( 7) == 1.OR.OUTP_CT( 8) == 1.OR.OUTP_CT(91) == 1
     . .OR.OUTP_CT(92) == 1.OR.OUTP_CT(93) == 1.OR.OUTP_CT(94) == 1
     . .OR.COUNT.GT.0) THEN
         IF (ISPMD == 0) THEN
           WASZP(1) = RSZP0(3*NSPGROUP+1)
           DO I=1,NSPGROUP
            WASZ_WR(1) = MAX(WASZ_WR(1),RSZP0(I))
           ENDDO
           WASZ_WR(1) = WASZ_WR(1) + 6
         ELSE
           WASZP(1) = 1
           WASZ_WR(1) = 1
         ENDIF
        ENDIF
! ------
      ENDIF
C--------
c-----------
#endif
      RETURN
      END


Chd|====================================================================
Chd|  OUTP_ARSZ_RS                  source/mpi/interfaces/spmd_outp.F
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|-- calls ---------------
Chd|        COUNT_ARSZ_RS                 source/output/sty/outp_r_s.F  
Chd|====================================================================
      SUBROUTINE OUTP_ARSZ_RS(IPARG,DD_IAD,WASZ,WASZP,WASZ_WR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M P I   I n c l u d e s
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IPARG(NPARG,*),DD_IAD(NSPMD+1,*),WASZ,WASZP,WASZ_WR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER NGF,NGL,NN,ITY,NEL,NG,JJ,SZP0(NSPGROUP+1),RSZP0(NSPGROUP+1),
     .        P0ARSZ2,WASZ2,I

      INTEGER IERROR,STATUS(MPI_STATUS_SIZE)
C-----------------------------------------------

      WASZP = 0
      WASZ = 0
      CALL COUNT_ARSZ_RS(IPARG,DD_IAD,WASZ,SZP0)

      IF (OUTP_RS(1) == 1) THEN
        CALL MPI_REDUCE(SZP0,RSZP0,NSPGROUP+1,
     .                MPI_INTEGER,MPI_SUM,IT_SPMD(1),
     .                MPI_COMM_WORLD,IERROR )

        IF (ISPMD == 0) THEN
         WASZP=RSZP0(NSPGROUP+1)
         WASZ_WR = -1
         DO I=1,NSPGROUP
          WASZ_WR = MAX(WASZ_WR,RSZP0(I)) 
         ENDDO
         WASZ_WR = WASZ_WR + 6
        ELSE
         WASZP = 1
         WASZ_WR = 1
        END IF
      ENDIF 

#endif
      RETURN
      END
Chd|====================================================================
Chd|  OUTP_ARSZ_RT                  source/mpi/interfaces/spmd_outp.F
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|-- calls ---------------
Chd|        COUNT_ARSZ_RT                 source/output/sty/outp_r_t.F  
Chd|====================================================================
      SUBROUTINE OUTP_ARSZ_RT(IPARG,IGEO,GEO,IXR,DD_IAD,WASZ,WASZP,WASZ_WR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M P I   I n c l u d e s
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IPARG(NPARG,*),DD_IAD(NSPMD+1,*),WASZ,WASZP,
     .           IXR(NIXR,*),IGEO(NPROPGI,*),WASZ_WR
       my_real 
     .   GEO(NPROPG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER NGF,NGL,NN,ITY,NEL,NG,JJ,SZP0(NSPGROUP+1),RSZP0(NSPGROUP+1),
     .        P0ARSZ2,WASZ2,IPROP,NUVAR,IGTYP,NFT,I

      INTEGER IERROR,STATUS(MPI_STATUS_SIZE)
C-----------------------------------------------

      WASZP = 0
      WASZ = 0
      CALL COUNT_ARSZ_RT(IPARG,IGEO,GEO,IXR,DD_IAD,WASZ,SZP0)

      IF (OUTP_RS(2) == 1) THEN          
        CALL MPI_REDUCE(SZP0,RSZP0,NSPGROUP+1,
     .                MPI_INTEGER,MPI_SUM,IT_SPMD(1),
     .                MPI_COMM_WORLD,IERROR )

        IF (ISPMD == 0) THEN
         WASZP=RSZP0(NSPGROUP+1)
         WASZ_WR = -1
         DO I=1,NSPGROUP
          WASZ_WR = MAX(WASZ_WR,RSZP0(I)) 
         ENDDO
         WASZ_WR = WASZ_WR + 6
        ELSE
         WASZP = 1
         WASZ_WR = 1
        END IF
      ENDIF 
#endif
      RETURN
      END
Chd|====================================================================
Chd|  OUTP_ARSZ_SPS                 source/mpi/interfaces/spmd_outp.F
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|-- calls ---------------
Chd|        COUNT_ARSZ_SPS                source/output/sty/outp_sp_s.F 
Chd|====================================================================
      SUBROUTINE OUTP_ARSZ_SPS(IPARG,DD_IAD,WASZ,WASZP,WASZ_WR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M P I   I n c l u d e s
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IPARG(NPARG,*),DD_IAD(NSPMD+1,*),WASZ,WASZP,WASZ_WR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER NGF,NGL,NN,ITY,NEL,NG,JJ,SZP0(NSPGROUP+1),RSZP0(NSPGROUP+1),
     .        P0ARSZ2,WASZ2,I

      INTEGER IERROR,STATUS(MPI_STATUS_SIZE)
C-----------------------------------------------
      WASZP = 0
      WASZ = 0
      CALL COUNT_ARSZ_SPS(IPARG,DD_IAD,WASZ,SZP0)

      IF (OUTP_SPS( 1) == 1.OR.OUTP_SPS( 2) == 1.OR.
     .    OUTP_SPS( 3) == 1.OR.OUTP_SPS( 4) == 1.OR.
     .    OUTP_SPS( 5) == 1.OR.OUTP_SPS( 6) == 1.OR.
     .    OUTP_SPS( 7) == 1.OR.OUTP_SPS(25) == 1.OR.
     .    OUTP_SPS(20) == 1.OR.OUTP_SPS(21) == 1.OR.
     .    OUTP_SPS(22) == 1.OR.OUTP_SPS(23) == 1.OR.
     .    OUTP_SPS(24) == 1 ) THEN


        CALL MPI_REDUCE(SZP0,RSZP0,NSPGROUP+1,
     .                MPI_INTEGER,MPI_SUM,IT_SPMD(1),
     .                MPI_COMM_WORLD,IERROR )

        IF (ISPMD == 0) THEN
         WASZP=RSZP0(NSPGROUP+1)
         WASZ_WR = -1
         DO I=1,NSPGROUP
          WASZ_WR = MAX(WASZ_WR,RSZP0(I)) 
         ENDDO
         WASZ_WR = WASZ_WR + 6 
        ELSE
         WASZP = 1
         WASZ_WR = 1
        END IF
      ENDIF 
#endif
      RETURN
      END
Chd|====================================================================
Chd|  OUTP_ARSZ_SPT                 source/mpi/interfaces/spmd_outp.F
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|-- calls ---------------
Chd|        COUNT_ARSZ_SPT                source/output/sty/outp_sp_t.F 
Chd|====================================================================
      SUBROUTINE OUTP_ARSZ_SPT(IPARG,DD_IAD,WASZ,WASZP,WASZ_WR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M P I   I n c l u d e s
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IPARG(NPARG,*),DD_IAD(NSPMD+1,*),WASZ,WASZP,WASZ_WR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER NGF,NGL,NN,ITY,NEL,NG,JJ,SZP0(NSPGROUP+1),RSZP0(NSPGROUP+1),
     .        P0ARSZ2,WASZ2,I

      INTEGER IERROR,STATUS(MPI_STATUS_SIZE)
C-----------------------------------------------
      WASZP = 0
      WASZ = 0
      CALL COUNT_ARSZ_SPT(IPARG,DD_IAD,WASZ,SZP0)

      IF (OUTP_SPT( 1) == 1 ) THEN
        CALL MPI_REDUCE(SZP0,RSZP0,NSPGROUP+1,
     .                MPI_INTEGER,MPI_SUM,IT_SPMD(1),
     .                MPI_COMM_WORLD,IERROR )

        IF (ISPMD == 0) THEN
         WASZP=RSZP0(NSPGROUP+1)
         WASZ_WR = -1
         DO I=1,NSPGROUP
          WASZ_WR = MAX(WASZ_WR,RSZP0(I)) 
         ENDDO
         WASZ_WR = WASZ_WR + 6 
        ELSE
         WASZP = 1 
         WASZ_WR = 1
        END IF
      ENDIF 
#endif
      RETURN
      END
Chd|====================================================================
Chd|  OUTP_ARSZ_SPTT                source/mpi/interfaces/spmd_outp.F
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|-- calls ---------------
Chd|        COUNT_ARSZ_SPTT               source/output/sty/outp_sp_t.F 
Chd|====================================================================
      SUBROUTINE OUTP_ARSZ_SPTT(IPARG,DD_IAD,WASZ,WASZP,WASZ_WR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M P I   I n c l u d e s
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IPARG(NPARG,*),DD_IAD(NSPMD+1,*),WASZ,WASZP,WASZ_WR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER NGF,NGL,NN,ITY,NEL,NG,JJ,SZP0(NSPGROUP+1),RSZP0(NSPGROUP+1),
     .        P0ARSZ2,WASZ2,I

      INTEGER IERROR,STATUS(MPI_STATUS_SIZE)
C-----------------------------------------------
      WASZP = 0
      WASZ = 0
      CALL COUNT_ARSZ_SPTT(IPARG,DD_IAD,WASZ,SZP0)

      IF (OUTP_SPT( 1) == 1 ) THEN

        CALL MPI_REDUCE(SZP0,RSZP0,NSPGROUP+1,
     .                MPI_INTEGER,MPI_SUM,IT_SPMD(1),
     .                MPI_COMM_WORLD,IERROR )

        IF (ISPMD /= 0) THEN
         WASZP = 1
         WASZ_WR = 1
        ELSE
         WASZ_WR = -1
         DO I=1,NSPGROUP
          WASZ_WR = MAX(WASZ_WR,RSZP0(I)) 
         ENDDO
         WASZP = RSZP0(NSPGROUP+1)
        END IF
      ENDIF 
#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_DOUTP_VGATH              source/mpi/interfaces/spmd_outp.F
Chd|-- called by -----------
Chd|        OUTP_N_V                      source/output/sty/outp_n_v.F  
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SPMD_DOUTP_VGATH(V,NODGLOB,WEIGHT,VGATH)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
#ifdef MPI
#include      "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       my_real
     .   V(3,*),VGATH(3,*)
       INTEGER WEIGHT(*),NODGLOB(*),NUM,SRECBUF
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF
      INTEGER SIZ,MSGTYP,I,K,NG,NREC,MSGOFF2

      DATA MSGOFF/9001/
      DATA MSGOFF2/9002/
      my_real 
     .       BUFSR(3,NUMNODM)
      INTEGER IBUF(NUMNODM)
Cow Tableau utilise par proc 0
         
       IF (ISPMD.NE.0) THEN

         SIZ = 0
         DO I=1,NUMNOD
           IF (WEIGHT(I) == 1) THEN
             SIZ = SIZ + 1
             IBUF(SIZ) = NODGLOB(I)
             BUFSR(1,SIZ) = V(1,I)
             BUFSR(2,SIZ) = V(2,I) 
             BUFSR(3,SIZ) = V(3,I) 
           END IF
         END DO

Cow a cause de la version simple precision, on ne peux pas metre l'entier
Cow dans le buffer flottant car on n a que 2puiss 24 bits dispo ~ 16 Million
Cow de noeuds au max

         MSGTYP = MSGOFF2
         CALL MPI_SEND(IBUF,SIZ,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .     MPI_COMM_WORLD,ierror)

         MSGTYP = MSGOFF
         CALL MPI_SEND(BUFSR,3*SIZ,REAL,IT_SPMD(1),MSGTYP,
     .     MPI_COMM_WORLD,ierror)

          
       ELSE

          DO I=1,NUMNOD
            IF (WEIGHT(I) == 1) THEN
              NG = NODGLOB(I)
              VGATH(1,NG) = V(1,I)
              VGATH(2,NG) = V(2,I)
              VGATH(3,NG) = V(3,I)
            ENDIF
          ENDDO


          DO I=2,NSPMD

Cow Reception du buffer entier des adresses NODGLOB
            MSGTYP = MSGOFF2

            CALL MPI_PROBE(IT_SPMD(I),MSGTYP,
     .                    MPI_COMM_WORLD,STATUS,ierror)
            CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,SIZ,ierror)

            CALL MPI_RECV(IBUF,SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                  MPI_COMM_WORLD,STATUS,ierror)

Cow Reception du buffer flottant double des adresses NODGLOB

            MSGTYP = MSGOFF
            CALL MPI_RECV(BUFSR,3*SIZ,REAL,IT_SPMD(I),MSGTYP,
     .                  MPI_COMM_WORLD,STATUS,ierror)
         
           NREC = SIZ
           DO K = 1, NREC
             NG = IBUF(K)
             VGATH(1,NG) = BUFSR(1,K)
             VGATH(2,NG) = BUFSR(2,K)
             VGATH(3,NG) = BUFSR(3,K)
           ENDDO
          ENDDO


       ENDIF

#endif
       RETURN
       END

Chd|====================================================================
Chd|  SPMD_DOUTP_GATH               source/mpi/interfaces/spmd_outp.F
Chd|-- called by -----------
Chd|        OUTP_NO                       source/output/sty/outp_no.F   
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SPMD_DOUTP_GATH(V,NODGLOB,WEIGHT,VGATH)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
#ifdef MPI
#include      "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       my_real
     .   V(*),VGATH(*)
       INTEGER WEIGHT(*),NODGLOB(*),NUM,SRECBUF
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF
      INTEGER SIZ,MSGTYP,I,K,NG,NREC,MSGOFF2

      DATA MSGOFF/9003/
      DATA MSGOFF2/9004/
      my_real 
     .       BUFSR(NUMNODM)
      INTEGER IBUF(NUMNODM)
Cow Tableau utilise par proc 0
         
       IF (ISPMD.NE.0) THEN

         SIZ = 0
         DO I=1,NUMNOD
           IF (WEIGHT(I) == 1) THEN
             SIZ = SIZ + 1
             IBUF(SIZ) = NODGLOB(I)
             BUFSR(SIZ) = V(I)
           END IF
         END DO

Cow a cause de la version simple precision, on ne peux pas metre l'entier
Cow dans le buffer flottant car on n a que 2puiss 24 bits dispo ~ 16 Million
Cow de noeuds au max

         MSGTYP = MSGOFF2 
         CALL MPI_SEND(IBUF,SIZ,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .     MPI_COMM_WORLD,ierror)

         MSGTYP = MSGOFF 
         CALL MPI_SEND(BUFSR,SIZ,REAL,IT_SPMD(1),MSGTYP,
     .     MPI_COMM_WORLD,ierror)
  
       ELSE

          DO I=1,NUMNOD
            IF (WEIGHT(I) == 1) THEN
              NG = NODGLOB(I)
              VGATH(NG) = V(I)
            ENDIF
          ENDDO

          DO I=2,NSPMD

Cow Reception du buffer entier des adresses NODGLOB
            MSGTYP = MSGOFF2

            CALL MPI_PROBE(IT_SPMD(I),MSGTYP,
     .                    MPI_COMM_WORLD,STATUS,ierror)
            CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,SIZ,ierror)

            CALL MPI_RECV(IBUF,SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                  MPI_COMM_WORLD,STATUS,ierror)

Cow Reception du buffer flottant double des adresses NODGLOB

            MSGTYP = MSGOFF
            CALL MPI_RECV(BUFSR,SIZ,REAL,IT_SPMD(I),MSGTYP,
     .                  MPI_COMM_WORLD,STATUS,ierror)
      
           NREC = SIZ
           DO K = 1, NREC
             NG = IBUF(K)
             VGATH(NG) = BUFSR(K)
           ENDDO
          ENDDO


       ENDIF

#endif
       RETURN
       END


Chd|====================================================================
Chd|  SPMD_RGATHER9                 source/mpi/interfaces/spmd_outp.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SPMD_RGATHER9(V,LEN,VP0,LENP0,IAD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
#ifdef MPI
#include      "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER LEN,LENP0,IAD
       my_real
     .   V(LEN),VP0(LENP0)

C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,
     .        MSGTYP,I,SIZ,LENP(NSPMD),DISP(NSPMD)

C=======================================================================
        CALL MPI_GATHER(
     S    LEN           ,1     ,MPI_INTEGER,
     R    LENP          ,1     ,MPI_INTEGER,IT_SPMD(1),
     G    MPI_COMM_WORLD,IERROR)
C
        IAD=0
        IF(ISPMD == 0)THEN
          DO I=1,NSPMD
            DISP(I) = IAD
            IAD = IAD+LENP(I)
          END DO
        END IF
C
        CALL MPI_GATHERV(
     S    V             ,LEN   ,REAL,
     R    VP0           ,LENP  ,DISP,REAL  ,IT_SPMD(1),
     G    MPI_COMM_WORLD,IERROR)
c------------
#endif
       RETURN
       END


Chd|====================================================================
Chd|  SPMD_RGATHER9_DP              source/mpi/interfaces/spmd_outp.F
Chd|-- called by -----------
Chd|        DYNAIN_C_STRAG                source/output/dynain/dynain_c_strag.F
Chd|        DYNAIN_C_STRSG                source/output/dynain/dynain_c_strsg.F
Chd|        DYNAIN_SHEL_SPMD              source/output/dynain/dynain_shel_spmd.F
Chd|        STAT_C_AUXF                   source/output/sta/stat_c_auxf.F
Chd|        STAT_C_EPSPF                  source/output/sta/stat_c_epspf.F
Chd|        STAT_C_FAIL                   source/output/sta/stat_c_fail.F
Chd|        STAT_C_OFF                    source/output/sta/stat_c_off.F
Chd|        STAT_C_ORTH_LOC               source/output/sta/stat_c_orth_loc.F
Chd|        STAT_C_STRAF                  source/output/sta/stat_c_straf.F
Chd|        STAT_C_STRAFG                 source/output/sta/stat_c_strafg.F
Chd|        STAT_C_STRSF                  source/output/sta/stat_c_strsf.F
Chd|        STAT_C_STRSFG                 source/output/sta/stat_c_strsfg.F
Chd|        STAT_C_THK                    source/output/sta/stat_c_thk.F
Chd|        STAT_P_AUX                    source/output/sta/stat_p_aux.F
Chd|        STAT_P_FULL                   source/output/sta/stat_p_full.F
Chd|        STAT_R_FULL                   source/output/sta/stat_r_full.F
Chd|        STAT_SHEL_SPMD                source/output/sta/stat_shel_spmd.F
Chd|        STAT_S_AUXF                   source/output/sta/stat_s_auxf.F
Chd|        STAT_S_EREF                   source/output/sta/stat_s_eref.F
Chd|        STAT_S_FAIL                   source/output/sta/stat_s_fail.F
Chd|        STAT_S_ORTHO                  source/output/sta/stat_s_ortho.F
Chd|        STAT_S_STRAF                  source/output/sta/stat_s_straf.F
Chd|        STAT_S_STRSF                  source/output/sta/stat_s_strsf.F
Chd|        STAT_T_FULL                   source/output/sta/stat_t_full.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SPMD_RGATHER9_DP(V,LEN,VP0,LENP0,IAD)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
#ifdef MPI
#include      "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER LEN,LENP0,IAD
       double precision
     .   V(LEN),VP0(LENP0)

C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,
     .        MSGTYP,I,SIZ,LENP(NSPMD),DISP(NSPMD)



         
        CALL MPI_GATHER(
     S    LEN           ,1     ,MPI_INTEGER,
     R    LENP          ,1     ,MPI_INTEGER,IT_SPMD(1),
     G    MPI_COMM_WORLD,IERROR)
C
        IAD=0
        IF(ISPMD == 0)THEN
          DO I=1,NSPMD
            DISP(I) = IAD
            IAD = IAD+LENP(I)
          END DO
        END IF
C
        CALL MPI_GATHERV(
     S    V             ,LEN   ,MPI_DOUBLE_PRECISION,
     R    VP0           ,LENP  ,DISP,MPI_DOUBLE_PRECISION,IT_SPMD(1),
     G    MPI_COMM_WORLD,IERROR)
#endif
       RETURN
       END

Chd|====================================================================
Chd|  SPMD_OUTPITAB                 source/mpi/interfaces/spmd_outp.F
Chd|-- called by -----------
Chd|        GENDYNAIN                     source/output/dynain/gendynain.F
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|        SPMD_VGATH_ERR                source/mpi/anim/spmd_vgath_err.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SPMD_OUTPITAB(V,WEIGHT,NODGLOB,VGLOB)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER
     .   VGLOB(*),V(*)

       INTEGER WEIGHT(*),NODGLOB(*),NUM,SRECBUF
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF
      INTEGER SIZ,MSGTYP,I,K,NG,NREC

      DATA MSGOFF/9005/

       INTEGER BUFSR(2,NUMNODM)
         
       IF (ISPMD.NE.0) THEN

         SIZ = 0
         DO I=1,NUMNOD
           IF (WEIGHT(I) == 1) THEN
             SIZ = SIZ + 1
             BUFSR(1,SIZ) = NODGLOB(I)
             BUFSR(2,SIZ) = V(I) 
           END IF
         END DO


         MSGTYP = MSGOFF
         CALL MPI_SEND(BUFSR,2*SIZ,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .     MPI_COMM_WORLD,ierror)

       ELSE
          DO I=1,NUMNOD
            IF (WEIGHT(I) == 1) THEN
              NG = NODGLOB(I)
              VGLOB(NG) = V(I)
            ENDIF
          ENDDO

          DO I=2,NSPMD

Cow Reception du buffer entier des adresses NODGLOB
            MSGTYP = MSGOFF

            CALL MPI_PROBE(IT_SPMD(I),MSGTYP,
     .                    MPI_COMM_WORLD,STATUS,ierror)
	    CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,SIZ,ierror)


            CALL MPI_RECV(BUFSR,SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                  MPI_COMM_WORLD,STATUS,ierror)
         
            NREC = SIZ/2
 
            DO K = 1, NREC
              NG = BUFSR(1,K)
              VGLOB(NG) = BUFSR(2,K)
            ENDDO
          ENDDO

       ENDIF

#endif
       RETURN
       END
Chd|====================================================================
Chd|  SPMD_RGATHER9_1COMM           source/mpi/interfaces/spmd_outp.F
Chd|-- called by -----------
Chd|        OUTP_C_S                      source/output/sty/outp_c_s.F  
Chd|        OUTP_C_T                      source/output/sty/outp_c_t.F  
Chd|        OUTP_C_TF                     source/output/sty/outp_c_t.F  
Chd|        OUTP_R_S                      source/output/sty/outp_r_s.F  
Chd|        OUTP_R_T                      source/output/sty/outp_r_t.F  
Chd|        OUTP_SP_S                     source/output/sty/outp_sp_s.F 
Chd|        OUTP_SP_T                     source/output/sty/outp_sp_t.F 
Chd|        OUTP_SP_TT                    source/output/sty/outp_sp_t.F 
Chd|        OUTP_S_S                      source/output/sty/outp_s_s.F  
Chd|        OUTP_S_T                      source/output/sty/outp_s_t.F  
Chd|        OUTP_S_TT                     source/output/sty/outp_s_t.F  
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SPMD_RGATHER9_1COMM(V,SIZV,LEN,VP0,SIZV0,ADRESS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
#ifdef MPI
#include      "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER LEN(NSPGROUP),SIZV,SIZV0,ADRESS(NSPGROUP+1,NSPMD)
       my_real
     .   V(SIZV),VP0(*)!SIZV0,NSPMD)

C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,
     .        MSGTYP,I,SIZ,DISP(NSPMD),
     .        LENP(NSPMD*NSPGROUP),LENP_LOC(NSPMD)
      INTEGER IAD,J,LEN_LOC
C=======================================================================
        CALL MPI_GATHER(
     S    LEN           ,NSPGROUP ,MPI_INTEGER,
     R    LENP          ,NSPGROUP ,MPI_INTEGER,IT_SPMD(1),
     G    MPI_COMM_WORLD,IERROR)
C
C
        IF(ISPMD == 0)THEN
        IAD=0
          DO I=1,NSPMD
             LENP_LOC(I) = 0
             DISP(I) = IAD
             DO J=1,NSPGROUP
              IAD = IAD+LENP((I-1)*NSPGROUP+J)
              LENP_LOC(I) = LENP_LOC(I) + LENP((I-1)*NSPGROUP+J)
             ENDDO
          END DO

          DO I=1,NSPMD
           ADRESS(1,I) = DISP(I) + 1
           DO J=2,NSPGROUP+1
            ADRESS(J,I) = LENP((I-1)*NSPGROUP+J-1) + ADRESS(J-1,I)
           ENDDO
          ENDDO
        END IF        ! end if(ispmd = 0)
C
        CALL MPI_GATHERV(
     S    V             ,SIZV   ,REAL,
     R    VP0          ,LENP_LOC ,DISP,REAL  ,IT_SPMD(1),
     G    MPI_COMM_WORLD,IERROR)

c------------
#endif
       RETURN
       END
