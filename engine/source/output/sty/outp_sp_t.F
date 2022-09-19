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
Chd|  OUTP_SP_T                     source/output/sty/outp_sp_t.F 
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SPMD_RGATHER9_1COMM           source/mpi/interfaces/spmd_outp.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE OUTP_SP_T(KEY,TEXT,ELBUF_TAB,IPARG,DD_IAD,SIZLOC,SIZP0,SIZ_WR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "sphcom.inc"
#include      "scr16_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*10 KEY
      CHARACTER*40 TEXT
      INTEGER IPARG(NPARG,*), DD_IAD(NSPMD+1,*),
     .    SIZLOC,SIZP0,SIZ_WR
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NG,NEL,N,II(6),JJ,JJ_OLD, NGF, NGL, NN,
     .   LEN,RESP0,WRTLEN,RES,COMPTEUR,L,K
      INTEGER, DIMENSION(NSPGROUP) :: JJ_LOC
      INTEGER, DIMENSION(NSPGROUP+1,NSPMD) :: ADRESS
      my_real
     .   WA(SIZLOC),WAP0(SIZ_WR),WAP0_LOC(SIZP0)
      my_real
     .   FUNC(6)
      TYPE(G_BUFEL_) ,POINTER :: GBUF
C=======================================================================
      IF (ISPMD == 0) THEN
          WRITE(IUGEO,'(2A)')'/SPHCEL  /TENSOR    /',KEY
          WRITE(IUGEO,'(A)')TEXT
          IF (OUTYY_FMT == 2) THEN
            WRITE(IUGEO,'(2A)') '#FORMAT: (1P6E12.5) ',
     .    '(TX(I),TY(I),TZ(I),TXY(I),TYZ(I),TZX(I),I=1,NUMSPH)'
          ELSE
            WRITE(IUGEO,'(2A)') '#FORMAT: (1P6E20.13) ',
     .    '(TX(I),TY(I),TZ(I),TXY(I),TYZ(I),TZX(I),I=1,NUMSPH)'
          END IF
      END IF
C
      JJ_OLD = 1
      NGF = 1
      NGL = 0
      RESP0=0
      JJ = 0
      COMPTEUR = 0
      DO NN=1,NSPGROUP
         NGL = NGL + DD_IAD(ISPMD+1,NN)
         DO  NG = NGF, NGL
          ITY   =IPARG(5,NG)
          IF(ITY == 51) THEN
          CALL INITBUF (IPARG    ,NG      ,
     2          MTN     ,NEL     ,NFT     ,IAD     ,ITY     ,
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
           GBUF => ELBUF_TAB(NG)%GBUF
           LFT=1
           LLT=NEL
!
           DO I=1,6
             II(I) = NEL*(I-1)
           ENDDO
!
           DO I=LFT,LLT
             WA(JJ+(I-1)*6+1) = GBUF%SIG(II(1)+I)
             WA(JJ+(I-1)*6+2) = GBUF%SIG(II(2)+I)
             WA(JJ+(I-1)*6+3) = GBUF%SIG(II(3)+I)
             WA(JJ+(I-1)*6+4) = GBUF%SIG(II(4)+I)
             WA(JJ+(I-1)*6+5) = GBUF%SIG(II(5)+I)
             WA(JJ+(I-1)*6+6) = GBUF%SIG(II(6)+I)
           ENDDO
           JJ = JJ + 6*LLT
          ENDIF
         ENDDO
         NGF = NGL + 1
        JJ_LOC(NN) = JJ - COMPTEUR            ! size of each group
        COMPTEUR = JJ
      ENDDO
!     ++++++++++
       IF( NSPMD>1 ) THEN
        CALL SPMD_RGATHER9_1COMM(WA,JJ,JJ_LOC,WAP0_LOC,SIZP0,ADRESS)
       ELSE
        WAP0_LOC(1:JJ) = WA(1:JJ)
        ADRESS(1,1) = 1
        DO NN = 2,NSPGROUP+1
         ADRESS(NN,1) = JJ_LOC(NN-1) + ADRESS(NN-1,1)
        ENDDO
       ENDIF
!     ++++++++++
       IF(ISPMD==0) THEN
         RESP0 = 0
         DO NN=1,NSPGROUP
          COMPTEUR = 0
          DO K = 1,NSPMD
           IF((ADRESS(NN+1,K)-1-ADRESS(NN,K))>=0) THEN
            DO L = ADRESS(NN,K),ADRESS(NN+1,K)-1
             COMPTEUR = COMPTEUR + 1
             WAP0(COMPTEUR+RESP0) = WAP0_LOC(L)
            ENDDO  ! l=... , ...
           ENDIF   !if(size_loc>0)
          ENDDO    ! k=1,nspmd

          JJ_OLD = COMPTEUR+RESP0
          IF(JJ_OLD > 0) THEN
           RES=MOD(JJ_OLD,6)
           WRTLEN=JJ_OLD-RES
           IF (WRTLEN > 0) THEN
             IF (OUTYY_FMT == 2) THEN
               WRITE(IUGEO,'(1P6E12.5)')(WAP0(J),J=1,WRTLEN)
             ELSE
               WRITE(IUGEO,'(1P6E20.13)')(WAP0(J),J=1,WRTLEN)
             END IF
           ENDIF
           DO I=1,RES
             WAP0(I)=WAP0(WRTLEN+I)
           ENDDO
           RESP0=RES
          ENDIF      ! jj_old>0
        ENDDO        ! nn=1,nspgroup
        IF (RESP0 > 0) THEN
          IF (OUTYY_FMT == 2) THEN
            WRITE(IUGEO,'(1P6E12.5)')(WAP0(J),J=1,RESP0)
          ELSE
            WRITE(IUGEO,'(1P6E20.13)')(WAP0(J),J=1,RESP0)
          ENDIF
        ENDIF
      END IF         ! ispmd = 0
      RETURN
      END
Chd|====================================================================
Chd|  OUTP_SP_TT                    source/output/sty/outp_sp_t.F 
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SPMD_RGATHER9_1COMM           source/mpi/interfaces/spmd_outp.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE OUTP_SP_TT(KEY,TEXT,ELBUF_TAB,IPARG,DD_IAD,
     2           IPM  ,KXSP ,SPBUF,SIZLOC,SIZP0,SIZ_WR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "scr01_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr16_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER*10 KEY
      CHARACTER*40 TEXT
      INTEGER KXSP(NISP,*),IPARG(NPARG,*), DD_IAD(NSPMD+1,*),
     .        IPM(NPROPMI,*),SIZ_WR
      INTEGER SIZLOC,SIZP0
      my_real
     .   SPBUF(NSPBUF,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,II(6),JJ, JJ_OLD,NPTR, NPTS, NPTT,NPTG,
     .   NG, NEL, IADD, N,NGF, NGL, NN, LEN,MLW,COMPTEUR,L
      INTEGER, DIMENSION(NSPGROUP) :: JJ_LOC
      INTEGER, DIMENSION(NSPGROUP+1,NSPMD) :: ADRESS
      my_real
     .   WA(SIZLOC),WAP0(SIZ_WR),WAP0_LOC(SIZP0)
      my_real
     .   FUNC(6)
      TYPE(G_BUFEL_) ,POINTER :: GBUF
C=======================================================================
        IF (ISPMD == 0) THEN
         WRITE(IUGEO,'(2A)')'/SPHCEL  /TENSOR    /',KEY
         WRITE(IUGEO,'(A)')TEXT
         IF (OUTYY_FMT == 2) THEN
           WRITE(IUGEO,'(A)')
     .      '#FORMAT:(3E12.5),
     .       EINT(I),RHO(I),H(I),I=1,NUMSPH '
           WRITE(IUGEO,'(2A)')'#FORMAT: (1P6E12.5/E12.5) ',
     .      '(TX(I),TY(I),TZ(I),TXY(I),TYZ(I),TZX(I),',
     .      '#EPSP(I),I=1,NUMSPH)'
         ELSE
           WRITE(IUGEO,'(A)')
     .      '#FORMAT:(3E20.13),
     .       EINT(I),RHO(I),H(I),I=1,NUMSPH '
           WRITE(IUGEO,'(2A)')'#FORMAT: (1P6E20.13/E20.13) ',
     .      '(TX(I),TY(I),TZ(I),TXY(I),TYZ(I),TZX(I),',
     .      '#EPSP(I),I=1,NUMSPH)'
         END IF
        END IF
C
       JJ_OLD = 1
       NGF = 1
       NGL = 0
       JJ = 0
       COMPTEUR = 0
       DO NN=1,NSPGROUP
        NGL = NGL + DD_IAD(ISPMD+1,NN)
        DO  NG = NGF, NGL
         ITY   =IPARG(5,NG)
         IF (ITY == 51) THEN
          CALL INITBUF (IPARG    ,NG      ,
     2          MTN     ,NEL     ,NFT     ,IAD     ,ITY     ,
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
          GBUF => ELBUF_TAB(NG)%GBUF
          LFT=1
          LLT=NEL
!
          DO I=1,6
            II(I) = NEL*(I-1)
          ENDDO
!
          DO I=LFT,LLT
             WA(JJ+1) = GBUF%EINT(I)
             WA(JJ+2) = GBUF%RHO(I)
             WA(JJ+3) = SPBUF(1,NFT+I)
             WA(JJ+4) = GBUF%SIG(II(1)+I)
             WA(JJ+5) = GBUF%SIG(II(2)+I)
             WA(JJ+6) = GBUF%SIG(II(3)+I)
             WA(JJ+7) = GBUF%SIG(II(4)+I)
             WA(JJ+8) = GBUF%SIG(II(5)+I)
             WA(JJ+9) = GBUF%SIG(II(6)+I)
             IF (GBUF%G_PLA > 0) THEN
              WA(JJ+10) = GBUF%PLA(I)
              WA(JJ+11) = ONE
             ELSE
              WA(JJ+10) = ZERO
              WA(JJ+11) = - ONE
             ENDIF
             JJ=JJ+11
          ENDDO
         ENDIF
        ENDDO
        NGF = NGL + 1
        JJ_LOC(NN) = JJ - COMPTEUR            ! size of each group
        COMPTEUR = JJ
      ENDDO                                   ! nn=1,nspdgroup

!     ++++++++++
       IF( NSPMD>1 ) THEN
        CALL SPMD_RGATHER9_1COMM(WA,JJ,JJ_LOC,WAP0_LOC,SIZP0,ADRESS)
       ELSE
        WAP0_LOC(1:JJ) = WA(1:JJ)
        ADRESS(1,1) = 1
        DO NN = 2,NSPGROUP+1
         ADRESS(NN,1) = JJ_LOC(NN-1) + ADRESS(NN-1,1)
        ENDDO
       ENDIF
!     ++++++++++
       IF(ISPMD==0) THEN
         DO NN=1,NSPGROUP
          COMPTEUR = 0
          DO K = 1,NSPMD
           IF((ADRESS(NN+1,K)-1-ADRESS(NN,K))>=0) THEN
            DO L = ADRESS(NN,K),ADRESS(NN+1,K)-1
             COMPTEUR = COMPTEUR + 1
             WAP0(COMPTEUR) = WAP0_LOC(L)
            ENDDO  ! l=... , ...
           ENDIF   !if(size_loc>0)
          ENDDO    ! k=1,nspmd

        JJ_OLD = COMPTEUR
        IF(JJ_OLD>0) THEN
         IF (OUTYY_FMT == 2) THEN
           J = 1
           DO WHILE (J<JJ_OLD)
             WRITE(IUGEO,'(1P3E12.5)')(WAP0(J-1+K),K=1,3) ! j-1+k=1,...,3
             J=J+3 ! j = 4
             WRITE(IUGEO,'(1P6E12.5)')(WAP0(J-1+K),K=1,6)! j-1+k = 4,...,9
             IF (WAP0(J+7) > ZERO) WRITE(IUGEO,'(1P1E12.5)') (WAP0(J+6)) ! test on G_PLA (wa(11)) then write wa(10)
             J=J+8 ! j = 12
           ENDDO
         ELSE
           J = 1
           DO WHILE (J<=JJ_OLD)
             WRITE(IUGEO,'(1P3E20.13)')(WAP0(J-1+K),K=1,3) ! j-1+k=1,...,3
             J=J+3 ! j = 4
             WRITE(IUGEO,'(1P6E20.13)')(WAP0(J-1+K),K=1,6) ! j-1+k = 4,...,9
             IF (WAP0(J+7) > ZERO) WRITE(IUGEO,'(1P1E20.13)') (WAP0(J+6)) ! test on G_PLA (wa(11)) then write wa(10)
             J=J+8 ! j = 12
           ENDDO
         END IF
        END IF
       ENDDO   ! nn=1,nspgroup
      END IF   ! ispmd=0
C-----------             
      RETURN
      END
Chd|====================================================================
Chd|  COUNT_ARSZ_SPT                source/output/sty/outp_sp_t.F 
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|        OUTP_ARSZ_SPT                 source/mpi/interfaces/spmd_outp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE COUNT_ARSZ_SPT(IPARG,DD_IAD,WASZ,SIZ_WRITE_LOC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
        INTEGER IPARG(NPARG,*),DD_IAD(NSPMD+1,*),WASZ,SIZ_WRITE_LOC(NSPGROUP+1)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGF,NGL,NN,ITY,NEL,NG,JJ,
     .        P0ARSZ2,WASZ2
C-----------------------------------------------
      WASZ = 0
      IF (OUTP_SPT( 1) == 1 ) THEN

        NGF = 1
        NGL = 0
        DO NN=1,NSPGROUP
         JJ = 0
         NGL = NGL + DD_IAD(ISPMD+1,NN)
         DO NG = NGF, NGL
          ITY   =IPARG(5,NG)
          IF(ITY == 51) THEN
            NEL = IPARG(2,NG)
            JJ = JJ + 6*NEL
          ENDIF
         ENDDO
         NGF = NGL + 1
         WASZ = WASZ + JJ
         SIZ_WRITE_LOC(NN) = JJ
        ENDDO
        SIZ_WRITE_LOC(NSPGROUP+1) = WASZ

      ENDIF

      RETURN
      END
Chd|====================================================================
Chd|  COUNT_ARSZ_SPTT               source/output/sty/outp_sp_t.F 
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|        OUTP_ARSZ_SPTT                source/mpi/interfaces/spmd_outp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE COUNT_ARSZ_SPTT(IPARG,DD_IAD,WASZ,SIZ_WRITE_LOC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
        INTEGER IPARG(NPARG,*),DD_IAD(NSPMD+1,*),WASZ,SIZ_WRITE_LOC(NSPGROUP+1)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGF,NGL,NN,ITY,NEL,NG,JJ,
     .        P0ARSZ2,WASZ2
C-----------------------------------------------
      WASZ = 0
      IF (OUTP_SPT( 1) == 1 ) THEN

        NGF = 1
        NGL = 0
        DO NN=1,NSPGROUP
         JJ = 0
         NGL = NGL + DD_IAD(ISPMD+1,NN)
         DO NG = NGF, NGL
          ITY   =IPARG(5,NG)
          IF(ITY == 51) THEN
            NEL = IPARG(2,NG)
            JJ = JJ + 11*NEL
          ENDIF
         ENDDO
         NGF = NGL + 1
         WASZ = WASZ + JJ
         SIZ_WRITE_LOC(NN) = JJ
        ENDDO
        SIZ_WRITE_LOC(NSPGROUP+1) = WASZ
      ENDIF
      RETURN
      END
