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
Chd|  ANISKEWF                      source/output/anim/aniskewf.F 
Chd|-- called by -----------
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE ANISKEWF(GEO,SKEW,IPARG,IXR,LRBUF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   GEO(NPROPG,*), SKEW(LSKEW,*)
      INTEGER IXR(NIXR,*),IPARG(NPARG,*),LRBUF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,ISK,NB1,NB2,NB3,NB4,NB5,NB6,NB7,NB8,NB9,
     .   NB10,NB11,NB12,NB13,NB14,NEL,LFT,LLT,NG,
     .   ITY,IAD,MLW,NFT,N,II,ISKK,MSGTAG,LEN
      INTEGER SRBUF(LRBUF)
C
C-----------------------------------------------
C      SKEW
C-----------------------------------------------
      ISK=NUMSKW-1
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
        LFT = 1
        LLT = NEL
C-----------------------------------------------
C      TRUSS
C-----------------------------------------------
        IF(ITY==4)THEN
            DO I=LFT,LLT
              ISK=ISK+1
              CALL WRITE_I_C(ISK,1)
            ENDDO
C-----------------------------------------------
C       POUTRES
C-----------------------------------------------
        ELSEIF(ITY==5)THEN
            DO I=LFT,LLT
              ISK=ISK+1
              CALL WRITE_I_C(ISK,1)
            ENDDO
C-----------------------------------------------
C       RESSORTS
C-----------------------------------------------
        ELSEIF(ITY==6)THEN
          IF(MLW==1)THEN
            DO I=LFT,LLT
              ISK=ISK+1
              CALL WRITE_I_C(ISK,1)
            ENDDO
          ELSEIF(MLW==2)THEN
            DO I=LFT,LLT
              N=I+NFT
              ISKK=NINT(GEO(2,IXR(1,N)))-1
              CALL WRITE_I_C(ISKK,1)
            ENDDO
          ELSEIF(MLW==3)THEN
            DO I=LFT,LLT
              ISK=ISK+1
              CALL WRITE_I_C(ISK,1)
              ISK=ISK+1
              CALL WRITE_I_C(ISK,1)
            ENDDO
          ELSEIF(MLW==4.OR.MLW==5)THEN
            DO I=LFT,LLT
              ISK=ISK+1
              CALL WRITE_I_C(ISK,1)
            ENDDO
          ENDIF
C
        ELSE
        ENDIF
      ENDDO
C
      RETURN
      END
