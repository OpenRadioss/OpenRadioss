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
Chd|  ANISKEWF                      source/output/anim/generate/aniskewf.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        RAD_SPMD_RECV                 source/mpi/generic/rad_spmd_recv.F
Chd|        RAD_SPMD_SEND                 source/mpi/generic/rad_spmd_send.F
Chd|        SPMD_IGATH                    source/mpi/anim/spmd_igath.F  
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE ANISKEWF(GEO,SKEW,IPARG,IXR,DD_IAD,LRBUF)
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
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   GEO(NPROPG,*), SKEW(LSKEW,*)
      INTEGER IXR(NIXR,*),IPARG(NPARG,*),LRBUF,
     .        DD_IAD(NSPMD+1,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,ISK,NB1,NB2,NB3,NB4,NB5,NB6,NB7,NB8,NB9,
     .   NB10,NB11,NB12,NB13,NB14,NEL,LFT,LLT,NG,
     .   ITY,IAD,MLW,NFT,N,II,ISKK,MSGTAG,LEN,IGTYP
      INTEGER SRBUF(LRBUF)
C
C-----------------------------------------------
C      SKEW
C-----------------------------------------------
      ISK=NUMSKW-1
C   spmd
c      IF (IMACH==3) THEN
        II = 0
C   pb sur ISK : incremente par pour ts les elem !
        MSGTAG = 1000
        IF (NSPMD > 1 .AND. ISPMD/=0) THEN

          CALL RAD_SPMD_RECV(ISK,INTSIZE,ISPMD-1,
     .                       IT_SPMD,MSGTAG+ISPMD,INTSIZE)

        ENDIF
c      ENDIF
C-----------------------------------------------
C      SKEW ELEMENT 1D
C-----------------------------------------------
C
      DO NG=1,NGROUP
        MLW   =IPARG(1,NG)
        NEL   =IPARG(2,NG)
        ITY   =IPARG(5,NG)
        NFT   =IPARG(3,NG)
        IAD   =IPARG(4,NG)
        IGTYP   =IPARG(38,NG)
        LFT = 1
        LLT = NEL
C-----------------------------------------------
C      TRUSS
C-----------------------------------------------
        IF(ITY==4)THEN
            DO I=LFT,LLT
              ISK=ISK+1
c              IF (IMACH/=3) THEN
c                CALL WRITE_I_C(ISK,1)
c              ELSE
                II = II + 1
                SRBUF(II) = ISK
c              ENDIF
            ENDDO
C-----------------------------------------------
C       POUTRES
C-----------------------------------------------
        ELSEIF(ITY==5)THEN
            DO I=LFT,LLT
              ISK=ISK+1
c              IF (IMACH/=3) THEN
c                CALL WRITE_I_C(ISK,1)
c              ELSE
                II = II + 1
                SRBUF(II) = ISK
c              ENDIF
            ENDDO
C-----------------------------------------------
C       RESSORTS
C-----------------------------------------------
        ELSEIF(ITY==6)THEN
          IF(MLW==1.OR.MLW==7)THEN
            DO I=LFT,LLT
              ISK=ISK+1
c              IF (IMACH/=3) THEN
c                CALL WRITE_I_C(ISK,1)
c              ELSE
                II = II + 1
                SRBUF(II) = ISK
c              ENDIF
            ENDDO
          ELSEIF(MLW==2)THEN
            DO I=LFT,LLT
              N=I+NFT
              ISKK=NINT(GEO(2,IXR(1,N)))-1
c              IF (IMACH/=3) THEN
c                CALL WRITE_I_C(ISKK,1)
c              ELSE
                II = II + 1
                SRBUF(II) = ISKK
c              ENDIF
            ENDDO
          ELSEIF(MLW==3)THEN
            DO I=LFT,LLT
c              IF (IMACH/=3) THEN
c                ISK=ISK+1
c                CALL WRITE_I_C(ISK,1)
c                ISK=ISK+1
c                CALL WRITE_I_C(ISK,1)
c              ELSE
                ISK=ISK+1
                II = II + 1
                SRBUF(II) = ISK
                ISK=ISK+1
                II = II + 1
                SRBUF(II) = ISK
c              ENDIF
            ENDDO
          ELSEIF((MLW >= 4 .AND. MLW <= 6 ) .OR. IGTYP == 23)THEN
            DO I=LFT,LLT
              ISK=ISK+1
c              IF (IMACH/=3) THEN
c                CALL WRITE_I_C(ISK,1)
c              ELSE
                II = II + 1
                SRBUF(II) = ISK
c              ENDIF
            ENDDO
          ENDIF
C
        ELSE
        ENDIF
      ENDDO
C
c      IF (IMACH==3) THEN
      IF (NSPMD > 1) THEN
        IF (ISPMD/=NSPMD-1) THEN
          CALL RAD_SPMD_SEND(ISK,INTSIZE,ISPMD+1,
     .                       IT_SPMD,MSGTAG+ISPMD+1,INTSIZE)
        ENDIF
C
        CALL SPMD_IGATH(SRBUF,II,LEN)
      ELSE
        LEN = II
      END IF
      IF (ISPMD==0) THEN
        CALL WRITE_I_C(SRBUF,LEN)
      ENDIF
c      ENDIF

C
      RETURN
      END

Chd|====================================================================
Chd|  CNTSKEW                       source/output/anim/generate/aniskewf.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|====================================================================
       SUBROUTINE CNTSKEW(IPARG,CNT,CNTG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER IPARG(NPARG,*),CNT,CNTG

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER NG,MLW,NEL,ITY
       CNT = 0
       DO NG=1,NGROUP
         MLW   =IPARG(1,NG)
         NEL   =IPARG(2,NG)
         ITY   =IPARG(5,NG)

         IF (ITY==4.OR.ITY==5.OR.ITY==6) THEN
           IF (MLW==3) THEN
            CNT=CNT+2*NEL
           ELSE
            CNT = CNT+NEL
           ENDIF
         ENDIF
       ENDDO

       CNTG = CNT
       IF (NSPMD > 1) CALL SPMD_GLOB_ISUM9(CNTG,1)
      END
