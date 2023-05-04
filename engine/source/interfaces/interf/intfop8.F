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
Chd|====================================================================
Chd|  INTFOP8                       source/interfaces/interf/intfop8.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        INTVO8                        source/interfaces/inter3d/intvo8.F
Chd|        SPMD_I8_REDUCE                source/mpi/interfaces/spmd_i8tool.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INT8_MOD                      ../common_source/modules/interfaces/int8_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE INTFOP8(
     1       IPARI ,X      ,A    ,
     2       ICODT ,FSAV    ,WA      ,V      ,MS   ,
     3       DT2T  ,NELTST  ,ITYPTST ,ITAB   ,STIFN,
     4       NPC   ,TF      ,FSKYI   ,ISKY   ,VR   ,
     5       FCONT ,IN      ,BUFSF   ,FNCONT ,NSENSOR,
     6       FTCONT,ICONTACT,RCONTACT,NUM_IMP,NS_IMP,
     7       NE_IMP ,NT_IMP  ,SENSOR_TAB,INTBUF_TAB    ,
     8       H3D_DATA,PSKIDS,TAGNCONT,KLOADPINTER,LOADPINTER,
     9       LOADP_HYD_INTER)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE INTBUFDEF_MOD
      USE INT8_MOD
      USE H3D_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NSENSOR
      INTEGER NELTST,ITYPTST 
      INTEGER NUM_IMP(*),NS_IMP(*),NE_IMP(*),NT_IMP
      INTEGER IPARI(NPARI,*), ICODT(*),ITAB(*), 
     .        NPC(*), ISKY(*), 
     .        ICONTACT(*),TAGNCONT(NLOADP_HYD_INTER,NUMNOD),
     .        KLOADPINTER(NINTER+1),LOADPINTER(NINTER*NLOADP_HYD),
     .        LOADP_HYD_INTER(NLOADP_HYD)

C     REAL
      my_real DT2T, 
     .   X(*), A(*), FSAV(NTHVKI,*) , WA(*), V(*),MS(*),
     .   STIFN(*), TF(*),FSKYI(LSKY,NFSKYI),VR(3,*),FCONT(3,*),IN(*),
     .   BUFSF(*), FNCONT(3,*),FTCONT(3,*),RCONTACT(*),PSKIDS(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, IAD,IDUM, ISENS,I
      my_real TS,STARTT,STOPT
      INTEGER NSLV,SIZ,NBT8
      my_real, DIMENSION(:,:), ALLOCATABLE :: FTSAV_BUF
      INTEGER :: NTY
C-----------------------------------------------
      !CALL STARTIME(96,1)
      NBT8 = 0
    

      IF (IMPL_S==1) IAD = 1
C
      DO 200 N=1,NINTER
       NTY   =IPARI(7,N)
       ! used to identify the communication
       ! Will be replace by MPI_Request with
       ! MPI 3.0 support
c      IF(ISPMD/=0) GOTO 200 !TO REMOVE
       IF(NTY==8) THEN
        NBT8 = NBT8 + 1    
        IF(IPARI(16,N)==NCYCLE)THEN
        ELSE
        IPARI(16,N) = NCYCLE
C 
         ISENS = 0   ! ???

         IF (ISENS > 0) THEN    ! IF INTERFACE IS ACTIVATED BY SENSOR 
            TS = SENSOR_TAB(ISENS)%TSTART
         ELSE
            TS = TT
         ENDIF
C
         IF(N2D==0)THEN
           IF(IMPL_S/=1)THEN
              IF(TT>=TS) THEN
                CALL INTVO8(
     1        IPARI(1,N) ,X,A    ,
     2        ICODT      ,FSAV(1,N)    ,V ,MS       ,
     3        FSKYI      ,ISKY         ,FCONT   ,FNCONT ,FTCONT   ,
     4        ICONTACT   ,RCONTACT,
     5        STIFN      ,ITAB,INTBUF_TAB(N),INTBUF_TAB(N)%T8,H3D_DATA,
     6        N          ,PSKIDS     ,TAGNCONT,KLOADPINTER,LOADPINTER,
     7        LOADP_HYD_INTER)
             ENDIF
           ELSE
             NUM_IMP(N) =0
              IF(TT>=TS) THEN
                CALL INTVO8(
     1          IPARI(1,N) ,X ,A        ,
     2          ICODT      ,FSAV(1,N)    ,V ,MS       ,
     3          FSKYI      ,ISKY         ,FCONT   ,FNCONT ,FTCONT   ,
     4          ICONTACT   ,RCONTACT,
     5          STIFN      ,ITAB,INTBUF_TAB(N),INTBUF_TAB(N)%T8,H3D_DATA,
     6          N          ,PSKIDS     ,TAGNCONT,KLOADPINTER,LOADPINTER,
     7          LOADP_HYD_INTER)
              ENDIF
             IAD=IAD+NUM_IMP(N)
           END IF
         ELSE

         ENDIF !N2D
        ENDIF !IPARI(16)
       ENDIF !NTY==8
  200 CONTINUE
C

      IF(NSPMD > 1) THEN
      NBT8 = 0
      DO N=1,NINTER
         NTY   =IPARI(7,N)
c        IF(ISPMD/=0) GOTO 200 !TO REMOVE
         IF(NTY==8) THEN
           NBT8 = NBT8+1
           STARTT=INTBUF_TAB(N)%VARIABLES(3)
           STOPT=INTBUF_TAB(N)%VARIABLES(11)
           IF(STARTT<=TT .AND. TT<= STOPT) THEN
             IF(IPARI(48,N) == 2) THEN
               NSLV = IPARI(5,N)         
               ALLOCATE(FTSAV_BUF(3,NSLV))
               FTSAV_BUF(1:3,1:NSLV) = 0
               DO I = 1, NSLV
                 FTSAV_BUF(1,I) = INTBUF_TAB(N)%FTSAVX(I) 
                 FTSAV_BUF(2,I) = INTBUF_TAB(N)%FTSAVY(I) 
                 FTSAV_BUF(3,I) = INTBUF_TAB(N)%FTSAVZ(I) 
               ENDDO
               SIZ = 3*NSLV
               CALL SPMD_I8_REDUCE(FTSAV_BUF,SIZ,NBT8)
               DO I = 1, NSLV
                 INTBUF_TAB(N)%FTSAVX(I) = FTSAV_BUF(1,I)
                 INTBUF_TAB(N)%FTSAVY(I) = FTSAV_BUF(2,I)
                 INTBUF_TAB(N)%FTSAVZ(I) = FTSAV_BUF(3,I)
               ENDDO
               DEALLOCATE(FTSAV_BUF)
             ENDIF
           ENDIF
         ENDIF
      ENDDO
      ENDIF

      IF (IMPL_S==1) NT_IMP=IAD-1
      !CALL STOPTIME(96,1)

C      
      RETURN
      END
C
