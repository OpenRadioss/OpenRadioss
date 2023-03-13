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
Chd|  H3D_ONED_TORSOR               source/output/h3d/h3d_results/h3d_oned_torsor.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        H3D_WRITE_TORSOR              source/output/h3d/h3d_results/h3d_write_torsor.F
Chd|====================================================================
      SUBROUTINE H3D_ONED_TORSOR(
     .                   IPARG   , IFUNC          , IXT     , IXP      , IXR   ,
     .                   TORS    , ONED_TORSOR    , ID_ELEM , ITY_ELEM , INFO1 ,
     .                   INFO2   , IS_WRITTEN_ONED, IPARTT  , IPARTP   , IPARTR,
     .                   H3D_PART, KEYWORD        )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "mvsiz_p.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "nchar_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   EVAR(9,MVSIZ),TORS(15,*),ONED_TORSOR(9,*)
      INTEGER IPARG(NPARG,*),IFUNC,INFO1 ,INFO2 ,
     .   IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),
     .   IS_WRITTEN_ONED(*),ID_ELEM(*),ITY_ELEM(*),
     .        IPARTT(*) ,IPARTP(*),IPARTR(*),H3D_PART(*)
      CHARACTER*ncharline KEYWORD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   OFF, FAC, A1, A2, A3, THK
      REAL R4(18)
      INTEGER I, NG, NEL, NFT, IAD, ITY, LFT, NPT, IPT,M,
     .        IADD, N, J, LLT, MLW, NB1, NB2, NB3, NB4, NB5, 
     .        NB6, NB7, NB8, NB9, NB10, NB11, NB12, ISTRAIN,
     .        IPID, I1, I2, IAD2, NS1, NS2  , IALEL, ISTRE,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,NNI,N0,
     .        IHBE,BUF,IOK_PART(MVSIZ),OFFSET
C-----------------------------------------------
C
       DO NG=1,NGROUP
C       IF(ANIM_K==0.AND.IPARG(8,NG)==1)GOTO 490
        MLW   =IPARG(1,NG)
        NEL   =IPARG(2,NG)
        NFT   =IPARG(3,NG)
        IAD   =IPARG(4,NG)
        ITY   =IPARG(5,NG)
        NB1   =IAD - 1
        LFT=1
        LLT=NEL

        IF (ITY == 4) OFFSET = 0
        IF (ITY == 5) OFFSET = NUMELT
        IF (ITY == 6) OFFSET = NUMELT+NUMELP
c
        DO  I=1,NEL 
          IF (ITY == 4) THEN
            ID_ELEM(OFFSET+NFT+I) = IXT(NIXT,NFT+I)
            ITY_ELEM(OFFSET+NFT+I) = 4
            IF( H3D_PART(IPARTT(NFT+I)) == 1) IOK_PART(I) = 1
          ELSEIF (ITY == 5) THEN 
            ID_ELEM(OFFSET+NFT+I) = IXP(NIXP,NFT+I)
            ITY_ELEM(OFFSET+NFT+I) = 5
            IF( H3D_PART(IPARTP(NFT+I)) == 1) IOK_PART(I) = 1
          ELSEIF (ITY == 6) THEN 
            ID_ELEM(OFFSET+NFT+I) = IXR(NIXR,NFT+I)
            ITY_ELEM(OFFSET+NFT+I) = 6
            IF( H3D_PART(IPARTR(NFT+I)) == 1) IOK_PART(I) = 1
          ENDIF
        ENDDO 

        IF(ITY==4 .OR. ITY==5 .OR. ITY==6)THEN
          DO I=1,NEL
            ONED_TORSOR(1:9,OFFSET+NFT+I) = ZERO   ! Default = zero in all cases !
          ENDDO
        ENDIF
c
        IF(ITY==4)THEN
C-----------------------------------------------
          IF (KEYWORD == 'F1') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(1,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'F2') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(2,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'F3') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(3,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'M11') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(4,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'M21') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(5,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'M31') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(6,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'M12') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(7,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'M22') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(8,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'M32') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(9,M)
            ENDDO
          ENDIF

          CALL H3D_WRITE_TORSOR(IOK_PART,IS_WRITTEN_ONED,ONED_TORSOR,NEL,OFFSET,NFT,
     .          		   EVAR)

        ELSEIF(ITY==5)THEN
C-----------------------------------------------
          IF (KEYWORD == 'F1') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(1,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'F2') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(2,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'F3') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(3,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'M11') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(4,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'M21') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(5,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'M31') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(6,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'M12') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(7,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'M22') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(8,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'M32') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(9,M)
            ENDDO
          ENDIF

          CALL H3D_WRITE_TORSOR(IOK_PART,IS_WRITTEN_ONED,ONED_TORSOR,NEL,OFFSET,NFT,
     .          		   EVAR)
        ELSEIF(ITY==6)THEN
C-----------------------------------------------
          IF (KEYWORD == 'F1') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(1,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'F2') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(2,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'F3') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(3,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'M11') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(4,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'M21') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(5,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'M31') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(6,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'M12') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(7,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'M22') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(8,M)
            ENDDO
C---------------------------------------------
          ELSEIF (KEYWORD == 'M32') THEN
C------------------------------------------------
            DO I=1,NEL
              M = NFT+I+OFFSET
              EVAR(1,I) = TORS(9,M)
            ENDDO
          ENDIF

          CALL H3D_WRITE_TORSOR(IOK_PART,IS_WRITTEN_ONED,ONED_TORSOR,NEL,OFFSET,NFT,
     .          		   EVAR)
        ENDIF
C-----------------------------------------------
      ENDDO
C
C-----------------------------------------------
      RETURN
      END
