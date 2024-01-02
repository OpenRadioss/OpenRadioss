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
Chd|====================================================================
Chd|  LEC_INISTATE_YFILE            source/initial_conditions/inista/lec_inistate_yfile.F
Chd|-- called by -----------
Chd|        LEC_INISTATE                  source/elements/initia/lec_inistate.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LEC_INISTATE_YFILE( 
     1                  NSIGSH    ,NSIGS     ,NSIGSPH   ,NSIGRS    ,NSIGI     ,                 
     2                  SIGSH     ,SIGI      ,SIGSPH    ,SIGRS     ,SIGSP     ,                  
     3                  ISIGSH    ,IUSER     ,
     4                  ID_SIGSH  , ID_SOLID_SIGI, ID_QUAD_SIGI  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "scry_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSIGI, NSIGSH, NSIGS, NSIGSPH, NSIGRS, ISIGSH, IUSER
      INTEGER ID_SIGSH(*), ID_SOLID_SIGI(*), ID_QUAD_SIGI(*)
      my_real
     .   SIGI(NSIGS,*),SIGSH(MAX(1,NSIGSH),*),
     .   SIGSP(NSIGI,*),SIGSPH(NSIGSPH,*),SIGRS(NSIGRS,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, N, I,J, IGTYP,
     .   NUMS,IHBE,NE, ISH3N,IIS,NIP,IPG,NPG,PT,
     .   J1,JJ1,NU,IP,JJ,NUVAR,
     .   NVARSH,NUMR,NUMSOLID,KK,
     .   JJHBE
      INTEGER KN
      INTEGER IS_SOLID,IS_QUAD   
      my_real
     .   THKI, EM, EB, H1, H2, H3, R0 , EIN, SMOOTHL
      my_real
     .   S(6), SIGLOC(6)
C=======================================================================
C
C -- LECTURE OF INITIAL STATE DATA - EXTRACTED FROM INITIA.F
C
C=======================================================================

C------------------
C      FICHIER Y000
C------------------
       IS_SOLID = 0
       IS_QUAD = 0
  110  READ(IIN5,FMT='(A)',END=199,ERR=199)LINE
       IF(LINE(1:7)/='/SOLIDE'.AND.N2D == 0)GOTO 110
       IF(LINE(1:5)/='/QUAD'.AND.N2D/=0)  GOTO 110
       IF(LINE(1:7) == '/SOLIDE')IS_SOLID=1
       IF(LINE(1:5) == '/QUAD')IS_QUAD=1
C
       READ(IIN5,FMT='(A)',END=199,ERR=199)LINE
  115  READ(IIN5,FMT='(A)',END=120,ERR=199)LINE
       IF(LINE(1:1) == '#')GOTO 115
       IF(IOUTP_FMT == 2) THEN
        IF(LINE(1:8) == '        ')GOTO 115
       ELSE
        IF(LINE(1:10) == '        ')GOTO 115
       ENDIF
         IF(LINE(1:1) == '/')GOTO 117
         IF(IOUTP_FMT == 2)THEN
           READ(LINE,'(2I8)')I,N
         ELSE
           READ(LINE,'(2I10)')I,N
         ENDIF
         IF(IS_SOLID == 1) ID_SOLID_SIGI(I) = N
         IF(IS_QUAD == 1) ID_QUAD_SIGI(I) = N
        GOTO 115
 117    IF(LINE(1:5)/='/TETRA'.AND.NUMELS10 == 0) GOTO 120
        READ(IIN5,FMT='(A)',END=199,ERR=199)LINE
 118    READ(IIN5,FMT='(A)',END=120,ERR=199)LINE
         IF(LINE(1:1) == '#')GOTO 118
         IF(IOUTP_FMT == 2) THEN
           IF(LINE(1:8) == '        ')GOTO 118
         ELSE
          IF(LINE(1:10) == '        ')GOTO 118
         ENDIF
         IF(LINE(1:1) == '/')GOTO 120
         IF (IOUTP_FMT == 2) THEN
           READ(LINE,'(2I8)')I,N
         ELSE
           READ(LINE,'(2I10)')I,N
         ENDIF
         ID_SOLID_SIGI(I) = N
       GOTO 118
C------------------
C      FICHIER Ynnn
C      EPS PLASTIC
C------------------
  120  READ(IIN4,FMT='(A)',END=199,ERR=199)LINE
       IF(LINE(1:8) == '/ENDDATA')THEN
         REWIND(IIN4)
         GOTO 130
       ENDIF
  122  IF(LINE(1:27)/='/SOLID     /SCALAR    /EPSP')GOTO 120
       READ(IIN4,FMT='(A)',END=199,ERR=199)LINE
       I = 0
  125  READ(IIN4,FMT='(A)',END=130,ERR=199)LINE
       IF(LINE(1:1) == '#')GOTO 125
       IF(LINE(1:1) == '/')GOTO 132
         IF (IOUTP_FMT == 2) THEN
           READ(LINE,'(6F12.0)')(S(J),J=1,6)
         ELSE
           READ(LINE,'(6F20.0)')(S(J),J=1,6)
         ENDIF
         DO J=1,6
           I=I+1
           IF(I>NUMSOL+NUMQUAD) GOTO 130
           SIGI(10,I) = S(J)
         ENDDO
       GOTO 125
C------------------
C      ENERGY
C------------------
  130  READ(IIN4,FMT='(A)',END=199,ERR=199)LINE
       IF(LINE(1:8) == '/ENDDATA')THEN
         REWIND(IIN4)
         GOTO 140
       ENDIF
  132  IF(LINE(1:27)/='/SOLID     /SCALAR    /ENER')GOTO 130
       READ(IIN4,FMT='(A)',END=199,ERR=199)LINE
C
       I = 0
  135  READ(IIN4,FMT='(A)',END=140,ERR=199)LINE
       IF(LINE(1:1) == '#')GOTO 135
       IF(LINE(1:1) == '/')GOTO 142
         IF (IOUTP_FMT == 2) THEN
           READ(LINE,'(6F12.0)')(S(J),J=1,6)
         ELSE
           READ(LINE,'(6F20.0)')(S(J),J=1,6)
         ENDIF
         DO J=1,6
           I=I+1
           IF(I>NUMSOL+NUMQUAD) GOTO 140
           SIGI(9,I) = S(J)
         ENDDO
       GOTO 135
C------------------
C      RHO
C------------------
  140  READ(IIN4,FMT='(A)',END=199,ERR=199)LINE
       IF(LINE(1:8) == '/ENDDATA')THEN
         REWIND(IIN4)
         GOTO 144
       ENDIF
  142  IF(LINE(1:27)/='/SOLID     /SCALAR    /DENS')GOTO 140
       READ(IIN4,FMT='(A)',END=199,ERR=199)LINE
C
       I = 0
  145  READ(IIN4,FMT='(A)',END=144,ERR=199)LINE
       IF(LINE(1:1) == '#')GOTO 145
       IF(LINE(1:1) == '/')GOTO 146
         IF (IOUTP_FMT == 2) THEN
           READ(LINE,'(6F12.0)')(S(J),J=1,6)
         ELSE
           READ(LINE,'(6F20.0)')(S(J),J=1,6)
         ENDIF
         DO J=1,6
           I=I+1
           IF(I>NUMSOL+NUMQUAD) GOTO 144
           SIGI(8,I) = S(J)
         ENDDO
       GOTO 145
C
  144  READ(IIN4,FMT='(A)',END=199,ERR=199)LINE
       IF(LINE(1:8) == '/ENDDATA')THEN
         REWIND(IIN4)
         GOTO 150
       ENDIF
  146  IF(LINE(1:28)/='/SOLID     /SCALAR    /USERS')GOTO 144
       READ(IIN4,FMT='(A)',END=199,ERR=199)LINE
        IUSER = 1
C
       I = 0
  147  READ(IIN4,FMT='(A)',END=150,ERR=199)LINE
       IF(LINE(1:1) == '#')GOTO 147
       IF(LINE(1:1) == '/')GOTO 152
        IF(IOUTP_FMT == 2)THEN
         READ(LINE,'(4I8)')NUMS,NIP,NUVAR,JJHBE
        ELSE
         READ(LINE,'(4I10)')NUMS,NIP,NUVAR,JJHBE
        ENDIF
C
       I=I+1
       SIGSP(NVSOLID1 + NVSOLID2 +1 , I) = NUMS
       SIGSP(NVSOLID1 + NVSOLID2 +2 , I) = NIP
       SIGSP(NVSOLID1 + NVSOLID2 +3 , I) = NUVAR
       SIGSP(NVSOLID1 + NVSOLID2 +4 , I) = JJHBE
       NU = (NUVAR - MOD(NUVAR,6))/6
       KN = 6
       IF (NUVAR == 0) THEN
         GOTO 147
       ENDIF
       IF(NUVAR < 6 )THEN
         NU = 1
         KN = NUVAR
       ENDIF
       IF(I>NUMSOL+NUMQUAD) GOTO 150
       IF(NUMS == 10.OR.NUMS == 16.OR.NUMS == 20.OR.
     .   (NUMS == 8.AND.JJHBE == 14).OR.(NUMS == 8.AND.JJHBE == 17).OR.
     .   ((NUMS == 6.OR.NUMS == 8).AND.JJHBE == 15).OR.
     .   (NUMS == 8.AND.NIP == 1.AND.
     .   JJHBE/=14.AND.JJHBE/=15).OR.NUMS == 4)THEN
         DO K=1,NIP
          IIS = NVSOLID1 + NVSOLID2 + 4 + NUVAR*(K - 1)
          DO J1 = 1,NU
           JJ1 = (J1 - 1)*6
  149      READ(IIN4,FMT='(A)',END=150,ERR=199)LINE
           IF(LINE(1:1) == '#')GOTO 149
           IF(LINE(1:1) == '/')GOTO 152
           IF(IOUTP_FMT == 2)THEN
            READ(LINE,FMT='(6F12.5)')(SIGSP(IIS + JJ1 + J,I),J=1,KN)
           ELSE
             READ(LINE,FMT='(6F20.0)')(SIGSP(IIS+JJ1 + J,I),J=1,KN)
           ENDIF
          ENDDO
          IF(NUVAR > NU*6)THEN
  151      READ(IIN4,FMT='(A)',END=150,ERR=199)LINE
           IF(LINE(1:1) == '#')GOTO 151
           IF(LINE(1:1) == '/')GOTO 152
           IF(IOUTP_FMT == 2)THEN
            READ(LINE,FMT='(6F12.5)')  (SIGSP(IIS+ NU*6 + J,I),J=1,NUVAR - 6*NU)
           ELSE
             READ(LINE,FMT='(6F20.0)') (SIGSP(IIS +6*NU + J,I),J=1,NUVAR - 6*NU)
           ENDIF
          ENDIF
         ENDDO ! K=1,NIP
       ENDIF
       GO TO 147
C------------------
C      STRESS
C------------------
  150  READ(IIN4,FMT='(A)',END=199,ERR=199)LINE
       IF(LINE(1:8) == '/ENDDATA')THEN
         REWIND(IIN4)
         GOTO 170
       ENDIF
  152  IF(LINE(1:29)/='/SOLID     /TENSOR    /STRESS')GOTO 150
       READ(IIN4,FMT='(A)',END=199,ERR=199)LINE
C
       I = 0
  155  READ(IIN4,FMT='(A)',END=170,ERR=199)LINE
       IF(LINE(1:1) == '#')GOTO 155
       IF(LINE(1:1) == '/')GOTO 172
         IF (IOUTP_FMT == 2) THEN
           READ(LINE,'(6F12.0)')(S(J),J=1,6)
         ELSE
           READ(LINE,'(6F20.0)')(S(J),J=1,6)
         ENDIF
         I=I+1
         IF(I>NUMSOL+NUMQUAD) GOTO 170
         DO J=1,6
           SIGI(J,I) = S(J)
         ENDDO
       GOTO 155
C------------------------------------------------------------------------
C    STRESS FULL + Elastoplastic + Energy + RHO in each point integration
C------------------------------------------------------------------------
  170  READ(IIN4,FMT='(A)',END=199,ERR=199)LINE
       IF(LINE(1:8) == '/ENDDATA')THEN
         REWIND(IIN4)
         GOTO 160
       ENDIF
  172  IF(LINE(1:30)/='/SOLID     /TENSOR    /STR_FUL')GOTO 170
       READ(IIN4,FMT='(A)',END=199,ERR=199)LINE
C
       I = 0
       J = 0
  174  READ(IIN4,FMT='(A)',END=160,ERR=199)LINE
       IF(LINE(1:1) == '#')GOTO 174
       IF(LINE(1:1) == '/')GOTO 160
       J=J+1
         I=I+1
         IF(I>NUMSOL+NUMQUAD) GOTO 170
         IF (IOUTP_FMT == 2) THEN
           READ(LINE,'(3I8)') NIP,NUMS,JJHBE
         ELSE
          READ(LINE,'(3I10)') NIP,NUMS, JJHBE
         ENDIF
         IF((NUMS == 8.OR.NUMS == 4).AND.JJHBE == 0)THEN
  175     READ(IIN4,FMT='(A)',END=160,ERR=199)LINE
          IF(LINE(1:1) == '#')GOTO 175
          IF(LINE(1:1) == '/')GOTO 160
          IF (IOUTP_FMT == 2) THEN
            READ(LINE,FMT='(2F12.0)') EIN,R0
          ELSE
            READ(LINE,FMT='(2F20.0)') EIN,R0
          ENDIF
          SIGSP(2,I) = NIP
          IF(NIP == 8)THEN
            SIGSP(1,I) = 1
            SIGI(8,I) =   R0
            SIGI(9,I) =   EIN
          ELSEIF(NIP == 1) THEN
            SIGI(8,I) =   R0
            SIGI(9,I) =   EIN
          ENDIF
          SIGSP(3,I) = EIN
          SIGSP(4,I) = R0
C
          IF (NIP == 1) THEN
  186      READ(IIN4,FMT='(A)',END=160,ERR=199)LINE
           IF(LINE(1:1) == '#')GOTO 186
           IF(LINE(1:1) == '/')GOTO 160
           IF (IOUTP_FMT == 2) THEN
             READ(LINE,FMT='(6F12.0)') (SIGI(J,I), J=1,6)
           ELSE
             READ(LINE,FMT='(6F20.0)') (SIGI(J,I), J=1,6)
           ENDIF
  187      READ(IIN4,FMT='(A)',END=160,ERR=199)LINE
           IF(LINE(1:1) == '#')GOTO 187
           IF(LINE(1:1) == '/')GOTO 160
           IF (IOUTP_FMT == 2) THEN
             READ(LINE,FMT='(1F12.0)') SIGI(10,I)
           ELSE
             READ(LINE,FMT='(1F20.0)') SIGI(10,I)
           ENDIF
          ELSE
           DO J=1,6
            SIGI(J,I) = ZERO
           ENDDO
           SIGI(10,I) = ZERO
C
           DO K=1,NIP
            IIS= 4 + (K-1)*7
  176       READ(IIN4,FMT='(A)',END=160,ERR=199)LINE
            IF(LINE(1:1) == '#')GOTO 176
            IF(LINE(1:1) == '/')GOTO 160
            IF (IOUTP_FMT == 2) THEN
             READ(LINE,FMT='(6F12.0)')
     .            SIGSP(IIS+1,I),SIGSP(IIS+2,I),SIGSP(IIS+3,I),
     .            SIGSP(IIS+4,I),SIGSP(IIS+5,I),SIGSP(IIS+6,I)
            ELSE
             READ(LINE,FMT='(6F20.0)')
     .            SIGSP(IIS+1,I),SIGSP(IIS+2,I),SIGSP(IIS+3,I),
     .            SIGSP(IIS+4,I),SIGSP(IIS+5,I),SIGSP(IIS+6,I)
            ENDIF
  177       READ(IIN4,FMT='(A)',END=160,ERR=199)LINE
            IF(LINE(1:1) == '#')GOTO 177
            IF(LINE(1:1) == '/')GOTO 160
            IF (IOUTP_FMT == 2) THEN
              READ(LINE,FMT='(1F12.0)') SIGSP(IIS+7,I)
            ELSE
              READ(LINE,FMT='(1F20.0)') SIGSP(IIS+7,I)
            ENDIF
C
            DO J=1,6
              SIGI(J,I) = SIGI(J,I) + 0.125*SIGSP(IIS+J,I)
            ENDDO
            SIGI(10,I)= SIGI(10,I) + 0.125*SIGSP(IIS+7,I)
           ENDDO
          ENDIF
c
         ELSEIF(NUMS == 10.OR.NUMS == 16.OR.NUMS == 20.OR.
     .    (NUMS == 8.AND.JJHBE == 14).OR.(NUMS == 8.AND.JJHBE == 17).OR.
     .    ((NUMS==6.OR.NUMS==8).AND.(JJHBE==15.or.JJHBE==12)))THEN
c
           DO K=1,NIP
             IIS=1 + (K-1)*9
  178        READ(IIN4,FMT='(A)',END=160,ERR=199)LINE
             IF(LINE(1:1) == '#')GOTO 178
             IF(LINE(1:1) == '/')GOTO 160
               READ(LINE,FMT='(6F12.0)')
             IF (IOUTP_FMT == 2) THEN
               READ(LINE,FMT='(6F12.0)')
     .            SIGSP(IIS+1,I),SIGSP(IIS+2,I),SIGSP(IIS+3,I),
     .            SIGSP(IIS+4,I),SIGSP(IIS+5,I),SIGSP(IIS+6,I)
             ELSE
               READ(LINE,FMT='(6F20.0)')
     .            SIGSP(IIS+1,I),SIGSP(IIS+2,I),SIGSP(IIS+3,I),
     .            SIGSP(IIS+4,I),SIGSP(IIS+5,I),SIGSP(IIS+6,I)
             ENDIF

  179        READ(IIN4,FMT='(A)',END=160,ERR=199)LINE
             IF(LINE(1:1) == '#')GOTO 179
             IF(LINE(1:1) == '/')GOTO 160
             IF (IOUTP_FMT == 2) THEN
               READ(LINE,FMT='(3F12.0)') SIGSP(IIS+7,I),SIGSP(IIS+8,I), SIGSP(IIS+9,I)
             ELSE
               READ(LINE,FMT='(3F20.0)') SIGSP(IIS+7,I),SIGSP(IIS+8,I), SIGSP(IIS+9,I)
             ENDIF
           ENDDO
         ENDIF
        GOTO 174
 160    CONTINUE
        REWIND(IIN4)
C--------------------------------------
C STRAIN FULL in each point integration
C--------------------------------------
  180  READ(IIN4,FMT='(A)',END=199,ERR=199)LINE
       IF(LINE(1:8) == '/ENDDATA')THEN
         REWIND(IIN4)
         GOTO 199
       ENDIF
  182  IF(LINE(1:33)/='/SOLID     /TENSOR    /STRAIN_FUL')GOTO 180
       READ(IIN4,FMT='(A)',END=199,ERR=199)LINE
C
       I = 0
       J = 0
  184  READ(IIN4,FMT='(A)',END=199,ERR=199)LINE
       IF(LINE(1:1) == '#')GOTO 184
       IF(LINE(1:1) == '/')GOTO 199
       J=J+1
       IF(I>NUMSOL+NUMQUAD) GOTO 199
       IF (IOUTP_FMT == 2) THEN
          READ(LINE,'(3I8)')NIP,NUMS,NUMSOLID
       ELSE
          READ(LINE,'(3I10)')NIP,NUMS,NUMSOLID
       ENDIF
C
       DO KK = 1,NUMSOLID
        I = I + 1
        DO K=1,NIP
         IIS= NVSOLID1  + (K-1)*6
  185    READ(IIN4,FMT='(A)',END=199,ERR=199)LINE
         IF(LINE(1:1) == '#')GOTO 185
         IF(LINE(1:1) == '/')GOTO 199
         IF (IOUTP_FMT == 2) THEN
             READ(LINE,FMT='(6F12.0)')
     .            SIGSP(IIS+1,I),SIGSP(IIS+2,I),SIGSP(IIS+3,I),
     .            SIGSP(IIS+4,I),SIGSP(IIS+5,I),SIGSP(IIS+6,I)
         ELSE
             READ(LINE,FMT='(6F20.0)')
     .            SIGSP(IIS+1,I),SIGSP(IIS+2,I),SIGSP(IIS+3,I),
     .            SIGSP(IIS+4,I),SIGSP(IIS+5,I),SIGSP(IIS+6,I)
         ENDIF
        ENDDO
       ENDDO
       GOTO 184
 199   CONTINUE
C
C--------------------------------------------------------
C     CONTRAINTES INITIALES + energies densites eps-plast
C     SHELL 4N/3N
C--------------------------------------------------------
C
C      FICHIER Y000
C
       REWIND(IIN4)
       REWIND(IIN5)
C
  310  READ(IIN5,FMT='(A)',END=399,ERR=399)LINE
       IF(LINE(1:6)/='/SHELL')GOTO 310
       READ(IIN5,FMT='(A)',END=399,ERR=399)LINE
C
  312  READ(IIN5,FMT='(A)',END=314,ERR=399)LINE
       IF(LINE(1:1) == '#')GOTO 312
       IF(IOUTP_FMT == 2) THEN
         IF(LINE(1:8) == '        ')GOTO 312
       ELSE
         IF(LINE(1:10) == '        ')GOTO 312
       ENDIF
       IF(LINE(1:1) == '/')GOTO 314
       IF (IOUTP_FMT == 2) THEN
         READ(LINE,'(2I8)')I,N
       ELSE
         READ(LINE,'(2I10)')I,N
       ENDIF
         IF(I>NUMSHEL) GOTO 314
         SIGSH(1,I) = N
         ID_SIGSH(I) = N
       GOTO 312
C
  314  REWIND(IIN5)
  315  READ(IIN5,FMT='(A)',END=399,ERR=399)LINE
       IF(LINE(1:8)/='/SHELL3N')GOTO 315
       READ(IIN5,FMT='(A)',END=399,ERR=399)LINE
C
  316  READ(IIN5,FMT='(A)',END=320,ERR=399)LINE
       IF(LINE(1:1) == '#')GOTO 316
       IF(IOUTP_FMT == 2) THEN
         IF(LINE(1:8) == '        ')GOTO 316
       ELSE
         IF(LINE(1:10) == '        ')GOTO 316
       ENDIF
       IF(LINE(1:1) == '/')GOTO 320
       IF (IOUTP_FMT == 2) THEN
         READ(LINE,'(2I8)')I,N
       ELSE
         READ(LINE,'(2I10)')I,N
       ENDIF
         SIGSH(1,I+NUMSHEL) = N
         ID_SIGSH(I+NUMSHEL) = N
         IF(I>NUMSH3N) GOTO 320
       GOTO 316
C----------------------------------------------
C      FICHIER Ynnn
C      NIP + THK + ENER + STRESS + EPS PLASTIC
C----------------------------------------------
  320  READ(IIN4,FMT='(A)',END=399,ERR=399)LINE
       IF(LINE(1:8) == '/ENDDATA')THEN
         REWIND(IIN4)
         GOTO 350
       ENDIF
  322  IF(LINE(1:33)/='/SHELL     /TENSOR    /STRESS_FUL')GOTO 320
       READ(IIN4,FMT='(A)',END=399,ERR=399)LINE
       ISIGSH =1
C
       I = 0
       J = 0
  325  READ(IIN4,FMT='(A)',END=350,ERR=399)LINE
       IF(LINE(1:1) == '#')GOTO 325
       IF(LINE(1:1) == '/')GOTO 352
         J=J+1
         I=I+1
         IF(J>NUMSHEL+NUMSH3N) GOTO 350
         IF (IOUTP_FMT == 2) THEN
           READ(LINE,'(2I8)')NIP,NPG
           READ(IIN4,FMT='(6F12.0)',END=399,ERR=399)THKI,EM,EB,H1,H2,H3
         ELSE
           READ(LINE,FMT=FMT_2I) NIP,NPG
           READ(IIN4,FMT='(6F20.0)',END=399,ERR=399)THKI,EM,EB,H1,H2,H3
         ENDIF
         SIGSH(2,I) = NIP
         SIGSH(3,I) = THKI
         SIGSH(4,I) = EM
         SIGSH(5,I) = EB 
         SIGSH(NVSHELL - 1,I) = ONE
        IF (NPG == 0.OR.NPG == 1)THEN
         SIGSH(14,I) = H1
         SIGSH(15,I) = H2
         SIGSH(16,I) = H3
         IF(NIP == 0)THEN
           IF (IOUTP_FMT == 2) THEN
             READ(IIN4,FMT='(6F12.0)',END=399,ERR=399)
     .                            SIGSH(22,I),SIGSH(23,I),SIGSH(24,I),
     .                            SIGSH(25,I),SIGSH(26,I),SIGSH(27,I)
             READ(IIN4,FMT='(3F12.0)',END=399,ERR=399)
     .                            SIGSH(28,I),SIGSH(29,I),SIGSH(30,I)
           ELSE
             READ(IIN4,FMT='(6F20.0)',END=399,ERR=399)
     .                            SIGSH(22,I),SIGSH(23,I),SIGSH(24,I),
     .                            SIGSH(25,I),SIGSH(26,I),SIGSH(27,I)
             READ(IIN4,FMT='(3F20.0)',END=399,ERR=399)
     .                            SIGSH(28,I),SIGSH(29,I),SIGSH(30,I)
           ENDIF
         ELSEIF(NIP/=0)THEN
           IF (IOUTP_FMT == 2) THEN
             DO K=1,NIP
               PT = 22 + 6*(K - 1)
               READ(IIN4,FMT='(6F12.0)',END=399,ERR=399)
     .                  SIGSH(PT,I ),SIGSH(PT + 1,I),SIGSH(PT + 2,I),
     .                  SIGSH(PT + 3,I),SIGSH(PT + 4,I),SIGSH(PT + 5,I)
             ENDDO
           ELSE
             DO K=1,NIP
               PT = 22 + 6*(K - 1)
               READ(IIN4,FMT='(6F20.0)',END=399,ERR=399)
     .                  SIGSH(PT,I),SIGSH(PT + 1,I),SIGSH(PT + 2,I),
     .                  SIGSH(PT + 3,I),SIGSH(PT + 4,I),SIGSH(PT + 5,I)
             ENDDO
           ENDIF
         ENDIF
        ELSEIF(NPG>1) THEN
          SIGSH(NVSHELL,I) = NPG
          IF(NIP == 0)THEN
           IF (IOUTP_FMT == 2) THEN
            DO N=1,NPG
             PT= 22 + (N-1)*9
             READ(IIN4,FMT='(6F12.0)',END=399,ERR=399)
     .                        SIGSH(PT,I),SIGSH(PT+1,I),SIGSH(PT+2,I),
     .                      SIGSH(PT+3,I),SIGSH(PT+4,I),SIGSH(PT+5,I)
             READ(IIN4,FMT='(3F12.0)',END=399,ERR=399)
     .                      SIGSH(PT+6,I),SIGSH(PT+7,I),SIGSH(PT+8,I)
            ENDDO
           ELSE
             DO N=1,NPG
             PT= 22 + (N-1)*9
             READ(IIN4,FMT='(6F20.0)',END=399,ERR=399)
     .                        SIGSH(PT,I),SIGSH(PT+1,I),SIGSH(PT+2,I),
     .                      SIGSH(PT+3,I),SIGSH(PT+4,I),SIGSH(PT+5,I)
             READ(IIN4,FMT='(3F20.0)',END=399,ERR=399)
     .                      SIGSH(PT+6,I),SIGSH(PT+7,I),SIGSH(PT+8,I)
            ENDDO
           ENDIF
        ELSE
           PT = 22
           IF (IOUTP_FMT == 2) THEN
             DO K=1,NIP
              DO N=1,NPG
               READ(IIN4,FMT='(6F12.0)',END=399,ERR=399)
     .                        SIGSH(PT,I),SIGSH(PT+1,I),SIGSH(PT+2,I),
     .                      SIGSH(PT+3,I),SIGSH(PT+4,I),SIGSH(PT+5,I)
               PT = PT + 6
              ENDDO
             ENDDO
           ELSE
             DO K=1,NIP
              DO N=1,NPG
               READ(IIN4,FMT='(6F20.0)',END=399,ERR=399)
     .                        SIGSH(PT,I),SIGSH(PT+1,I),SIGSH(PT+2,I),
     .                      SIGSH(PT+3,I),SIGSH(PT+4,I),SIGSH(PT+5,I)
               PT = PT + 6
              ENDDO
             ENDDO
           ENDIF
        ENDIF
       ELSE
c            CALL ANCERR(58,ANINFO_BLIND_2)
       ENDIF
       GOTO 325
C------------------
C------------------
 350   READ(IIN4,FMT='(A)',END=360,ERR=399)LINE
       IF(LINE(1:8) == '/ENDDATA')THEN
         REWIND(IIN4)
         GOTO 360
       ENDIF
  352  IF(LINE(1:33)/='/SHELL     /TENSOR    /STRAIN_FUL')GOTO 350
       READ(IIN4,FMT='(A)',END=399,ERR=399)LINE
       ISIGSH =1
C
       I = 0
       J = 0
  355  READ(IIN4,FMT='(A)',END=360,ERR=399)LINE
       IF(LINE(1:1) == '#')GOTO 355
       IF(LINE(1:1) == '/')GOTO 360
         J=J+1
         I=I+1
         IF(J> NUMSHEL+NUMSH3N) GOTO 360
         SIGSH(NVSHELL - 1,I) = ONE
         IF (IOUTP_FMT == 2) THEN
           READ(LINE,'(6F12.0)')SIGSH(6,I),SIGSH(7,I),SIGSH(8,I),
     .                          SIGSH(9,I),SIGSH(10,I),SIGSH(27,I)
           READ(IIN4,FMT='(A)',END=399,ERR=399)LINE
           READ(LINE,'(3F12.0)')SIGSH(11,I),SIGSH(12,I),SIGSH(13,I)
         ELSE
           READ(LINE,'(6F20.0)')SIGSH(6,I),SIGSH(7,I),SIGSH(8,I),
     .                          SIGSH(9,I),SIGSH(10,I),SIGSH(27,I)
           READ(IIN4,FMT='(A)',END=399,ERR=399)LINE
           READ(LINE,'(3F20.0)')SIGSH(11,I),SIGSH(12,I),SIGSH(13,I)
         ENDIF
       GOTO 355
C
 360   CONTINUE
C--------------------------------
C     /SHELL     /SCALAR    /EPSP
C--------------------------------
        REWIND(IIN4)
  420   READ(IIN4,FMT='(A)',END=399,ERR=399)LINE
        IF(LINE(1:8) == '/ENDDATA')THEN
          REWIND(IIN4)
          GO TO 520
        ENDIF
  422   IF(LINE(1:27)/='/SHELL     /SCALAR    /EPSP')GOTO 420
        READ(IIN4,FMT='(A)',END=399,ERR=399)LINE
        ISIGSH =1
C
        I = 0
        J = 0
  425   READ(IIN4,FMT='(A)',END=520,ERR=399)LINE
        IF(LINE(1:1) == '#')GOTO 425
        IF(LINE(1:1) == '/')GOTO 522
         J=J+1
         I=I+1
         IF(J>NUMSHEL+NUMSH3N) GOTO 520
         SIGSH(NVSHELL - 1,I) = ONE
         IF (IOUTP_FMT == 2) THEN
           READ(LINE,FMT='(6F12.0)')SIGLOC
         ELSE
           READ(LINE,FMT='(6F20.0)')SIGLOC
         ENDIF
         SIGSH(27,I)=SIGLOC(1)
         J=J+1
         I=I+1
         IF(J>NUMSHEL+NUMSH3N) GOTO 520
         SIGSH(27,I)=SIGLOC(2)
         J=J+1
         I=I+1
         IF(J>NUMSHEL+NUMSH3N) GOTO 520
         SIGSH(27,I)=SIGLOC(3)
         J=J+1
         I=I+1
         IF(J>NUMSHEL+NUMSH3N) GOTO 520
         SIGSH(27,I)=SIGLOC(4)
         J=J+1
         I=I+1
         IF(J>NUMSHEL+NUMSH3N) GOTO 520
         SIGSH(27,I)=SIGLOC(5)
         J=J+1
         I=I+1
         IF(J>NUMSHEL+NUMSH3N) GOTO 520
         SIGSH(27,I)=SIGLOC(6)
        GOTO 425
  520   READ(IIN4,FMT='(A)',END=399,ERR=399)LINE
        IF(LINE(1:8) == '/ENDDATA')THEN
          REWIND(IIN4)
          GOTO 450
        ENDIF
  522   IF(LINE(1:28)/='/SHELL     /SCALAR    /USERS')GOTO 520
          READ(IIN4,FMT='(A)',END=399,ERR=399)LINE
C
        IUSER = 1
C
        I = 0
        J = 0
  525   READ(IIN4,FMT='(A)',END=450,ERR=399)LINE
        IF(LINE(1:1) == '#')GOTO 525
        IF(LINE(1:1) == '/')GOTO 450
         J=J+1
         I=I+1
         IF(J>NUMSHEL+NUMSH3N) GOTO 450
         IF(IOUTP_FMT == 2)THEN
           READ(LINE,FMT='(4I8)') IHBE,NIP,NPG,NUVAR
         ELSE
           READ(LINE,FMT='(4I10)')IHBE,NIP,NPG,NUVAR
         ENDIF
          SIGSH(NVSHELL + 1 ,I) = IHBE
          SIGSH(NVSHELL + 2 ,I) = NIP
          SIGSH(NVSHELL + 3 ,I) = NPG
          SIGSH(NVSHELL + 4 ,I) = NUVAR
          NVARSH = NVSHELL + 4
          NU = (NUVAR - MOD(NUVAR,6))/6
          KN = 6
          IF( NUVAR < 6)THEN
            NU = 1
            KN = NUVAR
          ENDIF
C-----------------------
C  COQUE STANDARD
C  IHBE different de 11
C-----------------------
         IF(NPG == 0)THEN
          IF(NIP == 0)THEN
           DO J1 = 1,NU
            JJ1 = (J1 - 1)*6
 526        READ(IIN4,FMT='(A)',END=450,ERR=399)LINE
            IF(LINE(1:1) == '#')GOTO 526
            IF(LINE(1:1) == '/')GOTO 450
            IF(IOUTP_FMT == 2)THEN
             READ(LINE,'(6F12.5)')(SIGSH(NVARSH+K+JJ1,I),K=1,KN)
            ELSE
             READ(LINE,'(6F20.0)')(SIGSH(NVARSH+K+JJ1,I),K=1,KN)
            ENDIF
           ENDDO
           IF(NUVAR > 6*NU)THEN
  536       READ(IIN4,FMT='(A)',END=450,ERR=399)LINE
            IF(LINE(1:1) == '#')GOTO 536
            IF(LINE(1:1) == '/')GOTO 450
            IF(IOUTP_FMT == 2)THEN
            READ(LINE,'(6F12.5)')(SIGSH(NVARSH+K+6*NU,I),K=1,NUVAR-6*NU)
            ELSE
            READ(LINE,'(6F20.0)')(SIGSH(NVARSH+K+6*NU,I),K=1,NUVAR-6*NU)
            ENDIF
           ENDIF
          ELSE
           PT = 0
           DO IP = 1,NIP
            DO J1 = 1, NU
             JJ1 = (J1 - 1)*6
 527         READ(IIN4,FMT='(A)',END=450,ERR=399)LINE
             IF(LINE(1:1) == '#')GOTO 527
             IF(LINE(1:1) == '/')GOTO 450
             IF(IOUTP_FMT == 2)THEN
              READ(LINE,'(6F12.5)')(SIGSH(NVARSH+PT+JJ1+K,I),K=1,KN)
             ELSE
              READ(LINE,'(6F20.0)')(SIGSH(NVARSH+PT+JJ1+K,I),K=1,KN)
             ENDIF
            ENDDO
            IF(NUVAR > NU*6)THEN
  537        READ(IIN4,FMT='(A)',END=450,ERR=399)LINE
             IF(LINE(1:1) == '#')GOTO 537
             IF(LINE(1:1) == '/')GOTO 450
             IF(IOUTP_FMT == 2)THEN
              READ(LINE,'(6F12.5)')
     .            (SIGSH(NVARSH+PT+NU*6+K,I),K=1,NUVAR-6*NU)
             ELSE
              READ(LINE,'(6F20.0)')
     .            (SIGSH(NVARSH+PT+NU*6+K,I),K=1,NUVAR-6*NU)
             ENDIF
            ENDIF
            PT = PT + NUVAR
           ENDDO
          ENDIF
         ELSE
          IF(NIP == 0)THEN
           PT = 0
           DO N=1,NPG
            DO J1 = 1,NU
             JJ1 = (J1 - 1)*6
 528         READ(IIN4,FMT='(A)',END=450,ERR=399)LINE
             IF(LINE(1:1) == '#')GOTO 528
             IF(LINE(1:1) == '/')GOTO 450
             IF(IOUTP_FMT == 2)THEN
              READ(LINE,'(6F12.5)')(SIGSH(NVARSH+PT+JJ1+K,I),K=1,KN)
             ELSE
              READ(LINE,'(6F20.0)')(SIGSH(NVARSH+PT+JJ1+K,I),K=1,KN)
             ENDIF
            ENDDO
            IF(NUVAR > NU*6)THEN
 538         READ(IIN4,FMT='(A)',END=450,ERR=399)LINE
             IF(LINE(1:1) == '#')GOTO 538
             IF(LINE(1:1) == '/')GOTO 450
             IF(IOUTP_FMT == 2)THEN
              READ(LINE,'(6F12.5)')
     .          (SIGSH(NVARSH+PT+NU*6+K,I),K=1,NUVAR-6*NU)
             ELSE
              READ(LINE,'(6F20.0)')
     .          (SIGSH(NVARSH+PT+NU*6+K,I),K=1,NUVAR-6*NU)
             ENDIF
            ENDIF
            PT = PT + NUVAR
           ENDDO
          ELSE
           PT = 0
           DO N = 1,NPG
            DO IP = 1,NIP
             DO J1 = 1, NU
              JJ1 = (J1 -1)*6
 529          READ(IIN4,FMT='(A)',END=450,ERR=399)LINE
              IF(LINE(1:1) == '#')GOTO 529
              IF(LINE(1:1) == '/')GOTO 450
              IF(IOUTP_FMT == 2)THEN
               READ(LINE,'(6F12.5)')(SIGSH(NVARSH+K+PT+JJ1,I),K=1,KN)
              ELSE
               READ(LINE,'(6F20.0)')(SIGSH(NVARSH+K+PT+JJ1,I),K=1,KN)
              ENDIF
             ENDDO
             IF(NUVAR > 6*NU)THEN
 539          READ(IIN4,FMT='(A)',END=450,ERR=399)LINE
              IF(LINE(1:1) == '#')GOTO 539
              IF(LINE(1:1) == '/')GOTO 450
              IF(IOUTP_FMT == 2)THEN
               READ(LINE,'(6F12.5)')
     .          (SIGSH(NVARSH+K+PT+6*NU,I),K=1,NUVAR - 6*NU)
              ELSE
               READ(LINE,'(6F20.0)')
     .          ( SIGSH( NVARSH+K+PT+6*NU,I ),K=1,NUVAR - 6*NU )
              ENDIF
             ENDIF
             PT = PT + NUVAR
            ENDDO
           ENDDO
          ENDIF
         ENDIF
         GO TO 525
 450    CONTINUE
 399    CONTINUE
C
C--------------------------------------------------------
C     CONTRAINTES INITIALES + ENERGIES DENSITES EPS-PLAST
C     SPH particles
C--------------------------------------------------------
C
C      FICHIER Y000
C
       REWIND(IIN4)
       REWIND(IIN5)
  610  READ(IIN5,FMT='(A)',END=699,ERR=699)LINE
       IF(LINE(1:7)/='/SPHCEL')GOTO 610
C
       READ(IIN5,FMT='(A)',END=699,ERR=699)LINE
C
  615  READ(IIN5,FMT='(A)',END=620,ERR=699)LINE
       IF(LINE(1:1) == '#')GOTO 615
       IF(LINE(1:8) == '        ')GOTO 615
         IF(LINE(1:1) == '/')GOTO 620
         IF(IOUTP_FMT == 2)THEN
           READ(LINE,'(2I8)')I,N
         ELSE
           READ(LINE,'(2I10)')I,N
         END IF
         SIGSPH(7,I) = N
         GOTO 615
C
C      FICHIER Ynnn
C      EPS PLASTIC
C
  620  READ(IIN4,FMT='(A)',END=699,ERR=699)LINE
       IF(LINE(1:8) == '/ENDDATA')THEN
         REWIND(IIN4)
         GOTO 630
       ENDIF
  622  IF(LINE(1:27)/='/SPHCEL    /SCALAR    /EPSP')GOTO 620
       READ(IIN4,FMT='(A)',END=699,ERR=699)LINE
C
       I = 0
  625  READ(IIN4,FMT='(A)',END=630,ERR=699)LINE
       IF(LINE(1:1) == '#')GOTO 625
       IF(LINE(1:1) == '/')GOTO 632
         IF(IOUTP_FMT == 2)THEN
           READ(LINE,'(6F12.0)')(S(J),J=1,6)
         ELSE
           READ(LINE,'(6F20.0)')(S(J),J=1,6)
         END IF
         DO J=1,6
           I=I+1
           IF(I>NUMSPHY) GOTO 630
           SIGSPH(10,I) = S(J)
         ENDDO
       GOTO 625
C------------------
C      ENERGY
C------------------
  630  READ(IIN4,FMT='(A)',END=699,ERR=699)LINE
       IF(LINE(1:8) == '/ENDDATA')THEN
         REWIND(IIN4)
         GOTO 640
       ENDIF
  632  IF(LINE(1:27)/='/SPHCEL    /SCALAR    /ENER')GOTO 630
       READ(IIN4,FMT='(A)',END=699,ERR=699)LINE
C
       I = 0
  635  READ(IIN4,FMT='(A)',END=640,ERR=699)LINE
       IF(LINE(1:1) == '#')GOTO 635
       IF(LINE(1:1) == '/')GOTO 642
         IF(IOUTP_FMT == 2)THEN
           READ(LINE,'(6F12.0)')(S(J),J=1,6)
         ELSE
           READ(LINE,'(6F20.0)')(S(J),J=1,6)
         END IF
         DO J=1,6
           I=I+1
           IF(I>NUMSPHY) GOTO 640
           SIGSPH(9,I) = S(J)
         ENDDO
       GOTO 635
C------------------
C      RHO
C------------------
  640  READ(IIN4,FMT='(A)',END=699,ERR=699)LINE
       IF(LINE(1:8) == '/ENDDATA')THEN
         REWIND(IIN4)
         GOTO 650
       ENDIF
  642  IF(LINE(1:27)/='/SPHCEL    /SCALAR    /DENS')GOTO 640
       READ(IIN4,FMT='(A)',END=699,ERR=699)LINE
C
       I = 0
  645  READ(IIN4,FMT='(A)',END=650,ERR=699)LINE
       IF(LINE(1:1) == '#')GOTO 645
       IF(LINE(1:1) == '/')GOTO 652
         IF(IOUTP_FMT == 2)THEN
           READ(LINE,'(6F12.0)')(S(J),J=1,6)
         ELSE
           READ(LINE,'(6F20.0)')(S(J),J=1,6)
         END IF
         DO J=1,6
           I=I+1
           IF(I>NUMSPHY) GOTO 650
           SIGSPH(8,I) = S(J)
         ENDDO
       GOTO 645
C------------------
C      STRESS
C------------------
  650  READ(IIN4,FMT='(A)',END=699,ERR=699)LINE
       IF(LINE(1:8) == '/ENDDATA')THEN
         REWIND(IIN4)
         GOTO 660
       ENDIF
  652  IF(LINE(1:27)/='/SPHCEL  /TENSOR    /STRESS')GOTO 650
       READ(IIN4,FMT='(A)',END=699,ERR=699)LINE
C
       I = 0
  655  READ(IIN4,FMT='(A)',END=660,ERR=699)LINE
       IF(LINE(1:1) == '#')GOTO 655
       IF(LINE(1:1) == '/')GOTO 662
         IF(IOUTP_FMT == 2)THEN
           READ(LINE,'(6F12.0)')(S(J),J=1,6)
         ELSE
           READ(LINE,'(6F20.0)')(S(J),J=1,6)
         END IF
         I=I+1
         IF(I>NUMSPHY) GOTO 660
         DO J=1,6
           SIGSPH(J,I) = S(J)
         ENDDO
       GOTO 655
C-----------------------
C      Smoothing length
C-----------------------
  660  READ(IIN4,FMT='(A)',END=699,ERR=699)LINE
       IF(LINE(1:8) == '/ENDDATA')THEN
         REWIND(IIN4)
         GOTO 670
       ENDIF
  662  IF(LINE(1:27)/='/SPHCEL    /SCALAR    /SLEN')GOTO 660
       READ(IIN4,FMT='(A)',END=699,ERR=699)LINE
C
       I = 0
  665  READ(IIN4,FMT='(A)',END=670,ERR=699)LINE
       IF(LINE(1:1) == '#')GOTO 665
       IF(LINE(1:1) == '/')GOTO 672
         IF(IOUTP_FMT == 2)THEN
           READ(LINE,'(6F12.0)')(S(J),J=1,6)
         ELSE
           READ(LINE,'(6F20.0)')(S(J),J=1,6)
         END IF
         DO J=1,6
           I=I+1
           IF(I>NUMSPHY) GOTO 670
           SIGSPH(11,I) = S(J)
         ENDDO
       GOTO 665
C-------------------------------------------------------------------
C      STRESS FULL + Elastoplastic + Energy + RHO + Smoothing length
C-------------------------------------------------------------------
  670  READ(IIN4,FMT='(A)',END=699,ERR=699)LINE
       IF(LINE(1:8) == '/ENDDATA')THEN
         REWIND(IIN4)
         GOTO 680
       ENDIF
  672  IF(LINE(1:28)/='/SPHCEL  /TENSOR    /STR_FUL')GOTO 670
       READ(IIN4,FMT='(A)',END=699,ERR=699)LINE
C
       I = 0
  675  READ(IIN4,FMT='(A)',END=680,ERR=699)LINE
       IF(LINE(1:1) == '#')GOTO 675
       IF(LINE(1:1) == '/')GOTO 680
        I=I+1
        IF(I>NUMSPHY) GOTO 670
        IF(IOUTP_FMT == 2)THEN
          READ(LINE,FMT='(3F12.0)') EIN,R0,SMOOTHL
        ELSE
          READ(LINE,FMT='(3F20.0)') EIN,R0,SMOOTHL
        END IF
        SIGSPH(8,I)  =   R0
        SIGSPH(9,I)  =   EIN
        SIGSPH(11,I) =   SMOOTHL

  686   READ(IIN4,FMT='(A)',END=680,ERR=699)LINE
           IF(LINE(1:1) == '#')GOTO 686
           IF(LINE(1:1) == '/')GOTO 680
           IF(IOUTP_FMT == 2)THEN
             READ(LINE,FMT='(6F12.0)')(SIGSPH(J,I), J=1,6)
           ELSE
             READ(LINE,FMT='(6F20.0)')(SIGSPH(J,I), J=1,6)
           END IF
  687      READ(IIN4,FMT='(A)',END=680,ERR=699)LINE
           IF(LINE(1:1) == '#')GOTO 687
           IF(LINE(1:1) == '/')GOTO 680
           IF(IOUTP_FMT == 2)THEN
             READ(LINE,FMT='(1F12.0)') SIGSPH(10,I)
           ELSE
             READ(LINE,FMT='(1F20.0)') SIGSPH(10,I)
           END IF
        GOTO 675
 680    CONTINUE
 699    CONTINUE
C
C--------------------------------------------------------------
C     EFFORT et MOMENTS INITIALES + energies densites eps-plast
C     Ressort
C--------------------------------------------------------------
C
C   /INISPRI    -  *STY  file -
C
C
C      FICHIER Y000
C
        REWIND(IIN4)
        REWIND(IIN5)
  710   READ(IIN5,FMT='(A)',END=899,ERR=899)LINE
        IF (LINE(1:7) /= '/SPRING') GOTO 710
C
        READ(IIN5,FMT='(A)',END=899,ERR=899)LINE
C
  715   READ(IIN5,FMT='(A)',END=720,ERR=899)LINE
        IF (LINE(1:1) == '#') GOTO 715
        IF(IOUTP_FMT == 2) THEN
          IF(LINE(1:8) == '        ')GOTO 715
        ELSE
          IF(LINE(1:10) == '        ')GOTO 715
        ENDIF
        IF (LINE(1:1) == '/') GOTO 720
        READ(LINE,FMT_2I) I,N  ! SYSSPRI, USRSPRI
        SIGRS(1,I) = N
        GOTO 715
C
C      FICHIER Ynnn
C      FORCE + MOMENT + EPSP + ENERGY
C
  720   READ(IIN4,FMT='(A)',END=899,ERR=899)LINE
        IF (LINE(1:8) == '/ENDDATA') THEN
          REWIND(IIN4)
          GOTO 740
        ENDIF
  722   IF (LINE(1:16) /= '/SPRING    /FULL') GOTO 720
        READ(IIN4,FMT='(A)',END=740,ERR=899)LINE
C
        I = 0
  725   READ(IIN4,FMT='(A)',END=740,ERR=899)LINE
        IF (LINE(1:1) == '#') GOTO 725
        IF (LINE(1:1) == '/') GOTO 740
        IF (IOUTP_FMT == 2) THEN
          READ(LINE,'(3I8)')IGTYP,NUMR,NUVAR
        ELSE
          READ(LINE,'(3I10)')IGTYP,NUMR,NUVAR
        ENDIF ! IF (IOUTP_FMT == 2)
C------
        IF (IGTYP == 4 .OR. IGTYP == 12) THEN
C------
          DO K=1,NUMR
            I = I + 1
            IF (I > NUMELR) GOTO 899
  726       READ(IIN4,FMT='(A)',END=740,ERR=899)LINE
            IF (LINE(1:1) == '#') GOTO 726
            IF (LINE(1:1) == '/') GOTO 740
            READ(LINE,FMT=FMT_5F) (SIGRS(1+J,I),J=1,5)
            READ(IIN4,FMT='(A)',END=899,ERR=899)LINE
            READ(LINE,FMT=FMT_3F) (SIGRS(6+J,I),J=1,3)
          ENDDO ! DO K=1,NUMR
C------
        ELSEIF (IGTYP == 8  .OR. IGTYP == 13 .OR.
     .          IGTYP == 23 .OR. IGTYP == 25) THEN
C------
          DO K=1,NUMR
            I = I + 1
            IF (I > NUMELR) GOTO 899
  728       READ(IIN4,FMT='(A)',END=740,ERR=899)LINE
            IF (LINE(1:1) == '#') GOTO 728
            IF (LINE(1:1) == '/') GOTO 740
              READ(LINE,FMT=FMT_5F) (SIGRS(1+J,I),J=1,5)
              READ(IIN4,FMT='(A)',END=899,ERR=899)LINE
              READ(LINE,FMT=FMT_5F) (SIGRS(6+J,I),J=1,5)
              READ(IIN4,FMT='(A)',END=899,ERR=899)LINE
              READ(LINE,FMT=FMT_5F) (SIGRS(11+J,I),J=1,5)
              READ(IIN4,FMT='(A)',END=899,ERR=899)LINE
              READ(LINE,FMT=FMT_5F) (SIGRS(16+J,I),J=1,5)
              READ(IIN4,FMT='(A)',END=899,ERR=899)LINE
              READ(LINE,FMT=FMT_5F) (SIGRS(21+J,I),J=1,5)
              READ(IIN4,FMT='(A)',END=899,ERR=899)LINE
              READ(LINE,FMT=FMT_5F) (SIGRS(26+J,I),J=1,5)
              READ(IIN4,FMT='(A)',END=899,ERR=899)LINE
              READ(LINE,FMT=FMT_5F) (SIGRS(31+J,I),J=1,5)
              READ(IIN4,FMT='(A)',END=899,ERR=899)LINE
              READ(LINE,FMT=FMT_5F) (SIGRS(36+J,I),J=1,5)
          ENDDO ! DO K=1,NUMR
C------
        ELSEIF (IGTYP == 26) THEN
C------
          DO K=1,NUMR
            I = I + 1
            IF (I > NUMELR) GOTO 899
  729       READ(IIN4,FMT='(A)',END=740,ERR=899)LINE
            IF (LINE(1:1) == '#') GOTO 729
            IF (LINE(1:1) == '/') GOTO 740
              READ(LINE,FMT=FMT_3F) (SIGRS(1+J,I),J=1,3)
              READ(IIN4,FMT='(A)',END=899,ERR=899)LINE
              READ(LINE,FMT=FMT_3F) (SIGRS(6+J,I),J=1,3)
          ENDDO ! DO K=1,NUMR
C------
        ELSEIF (IGTYP == 29 .OR. IGTYP == 30 .OR. IGTYP == 31 .OR.
     .          IGTYP == 32 .OR. IGTYP == 33 .OR. IGTYP == 35 .OR.
     .          IGTYP == 36 .OR. IGTYP == 44 .OR. IGTYP == 45 .OR.
     .          IGTYP == 46) THEN
C
          DO K= 1,NUMR
            I = I + 1
            IF (I > NUMELR) GOTO 899
  731       READ(IIN4,FMT='(A)',END=740,ERR=899)LINE
            IF (LINE(1:1) == '#') GOTO 731
            IF (LINE(1:1) == '/') GOTO 740
            READ(LINE,FMT=FMT_2F) (SIGRS(1+J,I),J=1,2)
            READ(IIN4,FMT='(A)',END=899,ERR=899)LINE
            READ(LINE,FMT=FMT_2F) (SIGRS(3+J,I),J=1,2)
            READ(IIN4,FMT='(A)',END=899,ERR=899)LINE
            READ(LINE,FMT=FMT_2F) (SIGRS(5+J,I),J=1,2)
            READ(IIN4,FMT='(A)',END=899,ERR=899)LINE
            READ(LINE,FMT=FMT_2F) (SIGRS(7+J,I),J=1,2)
            READ(IIN4,FMT='(A)',END=899,ERR=899)LINE
            READ(LINE,FMT=FMT_2F) (SIGRS(9+J,I),J=1,2)
            READ(IIN4,FMT='(A)',END=899,ERR=899)LINE
            READ(LINE,FMT=FMT_2F) (SIGRS(11+J,I),J=1,2)
            READ(IIN4,FMT='(A)',END=899,ERR=899)LINE
            READ(LINE,FMT=FMT_F) SIGRS(14,I)
C
            PT = 14
C
C  -- UVAR --
C
            NU = (NUVAR - MOD(NUVAR,5))/5
            KN = 5
            IF (NUVAR < 5) THEN
              NU = 1
              KN = NUVAR
            ENDIF
C
            DO J1= 1,NU
              JJ1 = (J1 -1)*5
              READ(IIN4,FMT='(A)',END=899,ERR=899)LINE
              READ(LINE,FMT=FMT_5F) (SIGRS(PT+J+JJ1,I),J=1,KN)
            ENDDO
            IF (NUVAR > 5*NU) THEN
              READ(IIN4,FMT='(A)',END=899,ERR=899)LINE
              READ(LINE,FMT=FMT_5F) (SIGSH(PT+NU*5+J,I),J=1,NUVAR-5*NU)
            ENDIF ! IF (NUVAR > 5*NU)
C
          ENDDO ! DO K= 1,NUMR
        ENDIF ! IF (IGTYP)
        GOTO 725
C
 740    CONTINUE
 899    CONTINUE
C
      RETURN
      END
