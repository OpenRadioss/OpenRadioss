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
Chd|  LEC_INISTATE_TRI              source/elements/initia/lec_inistate_tri.F
Chd|-- called by -----------
Chd|        LEC_INISTATE                  source/elements/initia/lec_inistate.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        UEL2SYS                       source/initial_conditions/inista/yctrl.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LEC_INISTATE_TRI(
     1                  IXS        ,IXQ          ,IXC          ,IXT          ,IXP       ,
     2                  IXR        ,KXSP         ,IXTG         ,INDEX        ,ITRI      ,
     3                  NSIGSH     ,NSIGS        ,NSIGSPH      ,KSYSUSR      ,KSIGSH3   ,
     4                  NSIGRS     ,NSIGI        ,NSIGBEAM     ,NSIGTRUSS    ,
     5                  PTSHEL     ,PTSH3N       ,PTSOL        ,PTQUAD       ,PTSPH     ,
     6                  PTSPRI     ,PTBEAM       ,PTTRUSS      ,SIGI         ,SIGSH     ,
     7                  SIGSP      ,SIGSPH       ,SIGRS        ,SIGBEAM      ,SIGTRUSS  ,
     8                  ID_SIGSH   ,ID_SOLID_SIGI,ID_QUAD_SIGI ,ID_SIGSPRI ,ID_SIGBEAM  ,
     9                  ID_SIGTRUSS,WORK )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
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
#include      "scry_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*),IXT(NIXT,*),IXP(NIXP,*),IXR(NIXR,*),IXTG(NIXTG,*)
      INTEGER INDEX(*),ITRI(*),KXSP(*),KSYSUSR(*)
      INTEGER PTSHEL(*),PTSH3N(*),PTSOL(*),PTQUAD(*),PTSPH(*),PTSPRI(*),PTBEAM(*),PTTRUSS(*)
      INTEGER NSIGI,NSIGSH,NSIGS, NSIGSPH, NSIGRS, NSIGBEAM, NSIGTRUSS, KSIGSH3
      INTEGER ID_SIGSH(*), ID_SOLID_SIGI(*), ID_QUAD_SIGI(*)
      INTEGER ID_SIGSPRI(*), ID_SIGBEAM(*), ID_SIGTRUSS(*)
      INTEGER WORK(*)
      my_real
     .   SIGI(NSIGS,*),SIGSH(MAX(1,NSIGSH),*),SIGTRUSS(NSIGTRUSS,*),
     .   SIGSP(NSIGI,*),SIGSPH(NSIGSPH,*),SIGRS(NSIGRS,*),SIGBEAM(NSIGBEAM,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K
      INTEGER ISYS,JSYS,II,IE,JE,COMPT,NUMSHEL0
C-----------------------------------------------
      EXTERNAL UEL2SYS
      INTEGER  UEL2SYS
C----------------------------------------------------------------------
      IF (ABS(ISIGI) == 3.OR.ABS(ISIGI) == 4.OR.ABS(ISIGI) == 5) THEN
C
C SHELL 4-NODES
C
        NUMSHEL0=NUMSHEL
        IF(NUMSHEL>0)THEN
C        tri des elts du Y000 par ID croissant
          DO ISYS = 1, NUMSHEL
            ITRI(ISYS) =ID_SIGSH(ISYS)
          END DO
          CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMSHEL,1)
C        checking INISHE : search for multiply define
          ISYS=INDEX(1)
          IE  =ID_SIGSH(ISYS)
          DO J = 2, NUMSHEL
            JSYS=INDEX(J)
            JE  =ID_SIGSH(JSYS)
            IF(JE == IE)THEN
              DO K=2,NSIGSH
                IF(SIGSH(K,JSYS)/=ZERO)THEN
                  IF(SIGSH(K,ISYS)/=ZERO.AND.
     .             SIGSH(K,ISYS)/=SIGSH(K,JSYS))THEN
                    CALL ANCMSG(MSGID=595,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO_BLIND_1,
     .                          I1=IE)
                  ELSE
                    SIGSH(K,ISYS)=SIGSH(K,JSYS)
                  END IF
                END IF
              END DO
              SIGSH(1,JSYS)=ZERO
              ID_SIGSH(JSYS)=0
            ELSE
              IE  =JE
              ISYS=JSYS
            END IF
          END DO
          COMPT=0
          DO J=1,NUMSHEL
            IE=ID_SIGSH(J)
            IF(IE/=0)THEN
              COMPT=COMPT+1
              IF(COMPT<J)THEN
                DO K=1,NSIGSH
                  SIGSH(K,COMPT)=SIGSH(K,J)
                END DO
                ID_SIGSH(COMPT)=ID_SIGSH(J)

                SIGSH(1,J)=ZERO
                ID_SIGSH(J)=0
              END IF
            END IF
          END DO
          NUMSHEL=COMPT
        ENDIF
C
        IF(NUMSHEL>0)THEN
C        tri des elts du Y000 par ID croissant
          DO ISYS = 1, NUMSHEL
            ITRI(ISYS) = ID_SIGSH(ISYS)
          END DO
          CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMSHEL,1)
          DO J = 1, NUMSHEL
            ISYS=INDEX(J)
            KSYSUSR(J)=ID_SIGSH(ISYS)
            KSYSUSR(NUMSHEL+J)=ISYS
          END DO
C
C        Nos systeme dans le Y000, des elts du D00
          DO I=1,NUMELC
            ISYS=UEL2SYS(IXC(NIXC,I),KSYSUSR,NUMSHEL)
            PTSHEL(I) =ISYS
          END DO
        ENDIF
C
C SHELL 3-NODES
C
        IF(NUMSH3N>0)THEN
C        tri des elts du Y000 par ID croissant
          DO ISYS = 1, NUMSH3N
            II=NUMSHEL0+ISYS
            ITRI(ISYS) = ID_SIGSH(II)
          END DO
          CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMSH3N,1)
C
          ISYS=NUMSHEL0+INDEX(1)
          IE  =ID_SIGSH(ISYS)
          DO J = 2, NUMSH3N
            JSYS=NUMSHEL0+INDEX(J)
            JE  =ID_SIGSH(JSYS)
            IF(JE == IE)THEN
              DO K=2,NSIGSH
                IF(SIGSH(K,JSYS)/=ZERO)THEN
                  IF(SIGSH(K,ISYS)/=ZERO.AND.
     .              SIGSH(K,ISYS)/=SIGSH(K,JSYS))THEN
                    CALL ANCMSG(MSGID=596,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO_BLIND_1,
     .                          I1=IE)
                  ELSE
                    SIGSH(K,ISYS)=SIGSH(K,JSYS)
                  END IF
                END IF
              END DO
              SIGSH(1,JSYS)=ZERO
              ID_SIGSH(JSYS)=0
            ELSE
              IE  =JE
              ISYS=JSYS
            END IF
          END DO
          COMPT=0
          DO J=1,NUMSH3N
            IE=ID_SIGSH(NUMSHEL0+J)
            IF(IE/=0)THEN
              COMPT=COMPT+1
              IF(NUMSHEL+COMPT<NUMSHEL0+J)THEN
                DO K=1,NSIGSH
                  SIGSH(K,NUMSHEL+COMPT)=SIGSH(K,NUMSHEL0+J)
                END DO
                ID_SIGSH(NUMSHEL+COMPT)=ID_SIGSH(NUMSHEL0+J)

                SIGSH(1,NUMSHEL0+J)=ZERO
                ID_SIGSH(NUMSHEL0+J)=0
              END IF
            END IF
          END DO
          NUMSH3N=COMPT
        END IF
C
        IF (NUMSH3N > 0) THEN
C        tri des elts du Y000 par ID croissant
          DO ISYS = 1, NUMSH3N
            II=NUMSHEL+ISYS
            ITRI(ISYS) = ID_SIGSH(II)
          END DO
          CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMSH3N,1)
C
          DO J = 1, NUMSH3N
            ISYS=INDEX(J)
            II=NUMSHEL+ISYS
            KSYSUSR(J)=ID_SIGSH(II)
            KSYSUSR(NUMSH3N+J)=ISYS
          END DO
C
C        Nos systeme dans le Y000, des elts du D00
          DO I=1,NUMELTG
            ISYS=UEL2SYS(IXTG(NIXTG,I),KSYSUSR,NUMSH3N)
            PTSH3N(I) =ISYS
          END DO
        ENDIF
C
C BRICK
C
!!       IF(NSLASH(KINIBRI)/=0)THEN
        IF(NUMSOL>0)THEN
C
C        tri des elts de SIGI par ID croissant
          DO ISYS = 1, NUMSOL
            ITRI(ISYS) = ID_SOLID_SIGI(ISYS)
          END DO
          CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMSOL,1)
C
C        checking (search for multiply define) and compacting INISOL
          ISYS=INDEX(1)
          IE  =ID_SOLID_SIGI(ISYS)
          DO J = 2, NUMSOL
            JSYS=INDEX(J)
            JE  =ID_SOLID_SIGI(JSYS)
            IF(JE == IE)THEN
              DO K=1,6
                IF((SIGI(K,JSYS)/=ZERO) .AND.
     .             (SIGI(K,ISYS)/=SIGI(K,JSYS))   )THEN
                  IF(SIGI(K,ISYS)/=ZERO)THEN
                    CALL ANCMSG(MSGID=597,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO_BLIND_1,
     .                          I1=IE)
                  ELSE
                    SIGI(K,ISYS)=SIGI(K,JSYS)
                  END IF
                END IF
              END DO
              DO K=8,10
                IF(SIGI(K,JSYS)/=ZERO .AND.
     .             (SIGI(K,ISYS)/=SIGI(K,JSYS))   )THEN
                  IF(SIGI(K,ISYS)/=ZERO)THEN
                    CALL ANCMSG(MSGID=597,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO_BLIND_1,
     .                          I1=IE)
                  ELSE
                    SIGI(K,ISYS)=SIGI(K,JSYS)
                  END IF
                END IF
              END DO
              DO K=1,NSIGI
                IF(SIGSP(K,JSYS)/=ZERO .AND.
     .             (SIGSP(K,ISYS)/=SIGSP(K,JSYS))   )THEN
                  IF(SIGSP(K,ISYS)/=ZERO)THEN
                    CALL ANCMSG(MSGID=597,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO_BLIND_1,
     .                          I1=IE)
                  ELSE
                    SIGSP(K,ISYS)=SIGSP(K,JSYS)
                  END IF
                END IF
              END DO
              ID_SOLID_SIGI(JSYS)=0
            ELSE
              IE  =JE
              ISYS=JSYS
            END IF
          END DO
          COMPT=0
          DO J=1,NUMSOL
            IE=ID_SOLID_SIGI(J)
            IF(IE/=0)THEN
              COMPT=COMPT+1
              IF(COMPT<J)THEN
                DO K=1,6
                  SIGI(K,COMPT)=SIGI(K,J)
                END DO
                ID_SOLID_SIGI(COMPT)=ID_SOLID_SIGI(J)
                DO K=8,10
                  SIGI(K,COMPT)=SIGI(K,J)
                END DO
                DO K=1,NSIGI
                  SIGSP(K,COMPT)=SIGSP(K,J)
                END DO
                ID_SOLID_SIGI(J)=0
              END IF
            END IF
          END DO
          NUMSOL=COMPT
        END IF
C
        IF(NUMSOL>0)THEN
C
C        tri des elts du Y000 par ID croissant
          DO ISYS = 1, NUMSOL
            ITRI(ISYS) = ID_SOLID_SIGI(ISYS)
          END DO
          CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMSOL,1)
          DO J = 1, NUMSOL
            ISYS=INDEX(J)
            KSYSUSR(J)=ID_SOLID_SIGI(ISYS)
            KSYSUSR(NUMSOL+J)=ISYS
          END DO
C
C        Nos systeme dans le Y000, des elts du D00
          DO I=1,NUMELS
            ISYS=UEL2SYS(IXS(NIXS,I),KSYSUSR,NUMSOL)
            PTSOL(I) =ISYS
          END DO
        END IF
C
C QUAD 2D
C
!!       IF(NSLASH(KINIQUA)/=0)THEN
        IF(NUMQUAD>0)THEN
C
C        tri des elts de SIGI par ID croissant
          DO ISYS = 1, NUMQUAD
            ITRI(ISYS) = ID_QUAD_SIGI(ISYS)
          END DO
          CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMQUAD,1)
C
C        checking (search for multiply define) and compacting INIQUA
          ISYS=INDEX(1)
          IE  =ID_QUAD_SIGI(ISYS)
          DO J = 2, NUMQUAD
            JSYS=INDEX(J)
            JE  =ID_QUAD_SIGI(JSYS)
            IF(JE == IE)THEN
              DO K=1,6
                IF(SIGI(K,JSYS)/=ZERO)THEN
                  IF(SIGI(K,ISYS)/=ZERO)THEN
                    CALL ANCMSG(MSGID=598,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO_BLIND_1,
     .                          I1=IE)
                  ELSE
                    SIGI(K,ISYS)=SIGI(K,JSYS)
                  END IF
                END IF
              END DO
              DO K=8,10
                IF(SIGI(K,JSYS)/=ZERO)THEN
                  IF(SIGI(K,ISYS)/=ZERO)THEN
                    CALL ANCMSG(MSGID=598,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO_BLIND_1,
     .                          I1=IE)
                  ELSE
                    SIGI(K,ISYS)=SIGI(K,JSYS)
                  END IF
                END IF
              END DO
              ID_QUAD_SIGI(JSYS)=ZERO
            ELSE
              IE  =JE
              ISYS=JSYS
            END IF
          END DO
          COMPT=0
          DO J=1,NUMQUAD
            IE=ID_QUAD_SIGI(J)
            IF(IE/=0)THEN
              COMPT=COMPT+1
              IF(COMPT<J)THEN
                DO K=1,6
                  SIGI(K,COMPT)=SIGI(K,J)
                END DO
                ID_QUAD_SIGI(COMPT)= ID_QUAD_SIGI(J)
                DO K=8,10
                  SIGI(K,COMPT)=SIGI(K,J)
                END DO
                ID_QUAD_SIGI(J)=ZERO
              END IF
            END IF
          END DO
          NUMQUAD=COMPT
        END IF
C
        IF(NUMQUAD>0)THEN
C
C        tri des elts du Y000 par ID croissant
          DO ISYS = 1, NUMQUAD
            ITRI(ISYS) = ID_QUAD_SIGI(ISYS)
          END DO
          CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMQUAD,1)
          DO J = 1, NUMQUAD
            ISYS=INDEX(J)
            KSYSUSR(J)=ID_QUAD_SIGI(ISYS)
            KSYSUSR(NUMQUAD+J)=ISYS
          END DO
C
C        Nos systeme dans le Y000, des elts du D00
          DO I=1,NUMELQ
            ISYS=UEL2SYS(IXQ(NIXQ,I),KSYSUSR,NUMQUAD)
            PTQUAD(I) =ISYS
          END DO
        END IF
C
      END IF ! IF (ABS(ISIGI) == 3.OR.ABS(ISIGI) == 4.OR.ABS(ISIGI) == 5)
C
C SPH
C
      IF(NUMSPHY>0)THEN
C
C       tri des elts du Y000 par ID croissant
        DO ISYS = 1, NUMSPHY
          ITRI(ISYS) = NINT(SIGSPH(7,ISYS))
        END DO
        CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMSPHY,1)
        DO J = 1, NUMSPHY
          ISYS=INDEX(J)
          KSYSUSR(J)=NINT(SIGSPH(7,ISYS))
          KSYSUSR(NUMSPHY+J)=ISYS
        END DO
C
C       Nos systeme dans le Y000, des elts du D00
        DO I=1,NUMSPH
          ISYS=UEL2SYS(KXSP(NISP*I),KSYSUSR,NUMSPHY)
          PTSPH(I) =ISYS
        END DO
      END IF
C
      IF (ABS(ISIGI)<3) THEN
        KSIGSH3=1+NUMELC
      ELSE
        KSIGSH3=1+NUMSHEL
      END IF
C
C  SPRING
C
      IF (NUMSPRI > 0) THEN
C
        INISPRI = 1
C        tri des elts du Y000 par ID croissant
        DO ISYS = 1, NUMSPRI
          ITRI(ISYS) =ID_SIGSPRI(ISYS)
        ENDDO
        CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMSPRI,1)
C        checking INISPRI : search for multiply define
        ISYS=INDEX(1)
        IE  =ID_SIGSPRI(ISYS)
        DO J = 2, NUMSPRI
          JSYS=INDEX(J)
          JE  =ID_SIGSPRI(JSYS)
          IF (JE == IE) THEN
            DO K=2,NSIGRS
              IF (SIGRS(K,JSYS) /= ZERO) THEN
                IF (SIGRS(K,ISYS) /= ZERO .AND.
     .              SIGRS(K,ISYS) /= SIGRS(K,JSYS)) THEN
                  CALL ANCMSG(MSGID=1234,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO_BLIND_1,
     .                        I1=IE)
                ELSE
                  SIGRS(K,ISYS)=SIGRS(K,JSYS)
                ENDIF
              ENDIF
            ENDDO
            SIGRS(1,JSYS)=ZERO
            ID_SIGSPRI(JSYS)=0
          ELSE
            IE  =JE
            ISYS=JSYS
          ENDIF
        ENDDO
        COMPT=0
        DO J=1,NUMSPRI
          IE=ID_SIGSPRI(J)
          IF (IE /= 0) THEN
            COMPT=COMPT+1
            IF (COMPT < J) THEN
              DO K=1,NSIGRS
                SIGRS(K,COMPT)=SIGRS(K,J)
              ENDDO
              ID_SIGSPRI(COMPT)=ID_SIGSPRI(J)
              SIGRS(1,J)=ZERO
              ID_SIGSPRI(J)=0
            ENDIF
          ENDIF
        ENDDO
        NUMSPRI=COMPT
      ENDIF ! IF (NUMSPRI > 0)
C
      IF (NUMSPRI > 0) THEN
C       tri des elts du Y000 par ID croissant
        DO ISYS = 1, NUMSPRI
          ITRI(ISYS) = ID_SIGSPRI(ISYS)
        ENDDO
        CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMSPRI,1)
        DO J = 1, NUMSPRI
          ISYS=INDEX(J)
          KSYSUSR(J)=ID_SIGSPRI(ISYS)
          KSYSUSR(NUMSPRI+J)=ISYS
        ENDDO
C
C        Nos systeme dans le Y000, des elts du D00
        DO I=1,NUMELR
          ISYS=UEL2SYS(IXR(NIXR,I),KSYSUSR,NUMSPRI)
          PTSPRI(I) =ISYS
        ENDDO
      ENDIF ! IF (NUMSPRI > 0)
C
C  BEAM
C
      IF (NUMBEAM > 0) THEN
C        tri des elts du Y000 par ID croissant
        DO ISYS = 1, NUMBEAM
          ITRI(ISYS) =ID_SIGBEAM(ISYS)
        ENDDO
        CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMBEAM,1)
C        checking INIBEAM : search for multiply define
        ISYS=INDEX(1)
        IE  =ID_SIGBEAM(ISYS)
        DO J = 2, NUMBEAM
          JSYS=INDEX(J)
          JE  =ID_SIGBEAM(JSYS)
          IF (JE == IE) THEN
            DO K=2,NSIGBEAM
              IF (SIGBEAM(K,JSYS) /= ZERO) THEN
                IF (SIGBEAM(K,ISYS) /= ZERO .AND.
     .              SIGBEAM(K,ISYS) /= SIGBEAM(K,JSYS)) THEN
                  CALL ANCMSG(MSGID=1235,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO_BLIND_1,
     .                        I1=IE)
                ELSE
                  SIGBEAM(K,ISYS)=SIGBEAM(K,JSYS)
                ENDIF
              ENDIF
            ENDDO
            SIGBEAM(1,JSYS)=ZERO
            ID_SIGBEAM(JSYS)=0
          ELSE
            IE  =JE
            ISYS=JSYS
          ENDIF
        ENDDO
        COMPT=0
        DO J=1,NUMBEAM
          IE=ID_SIGBEAM(J)
          IF (IE /= 0) THEN
            COMPT=COMPT+1
            IF (COMPT < J) THEN
              DO K=1,NSIGBEAM
                SIGBEAM(K,COMPT)=SIGBEAM(K,J)
              ENDDO
              ID_SIGBEAM(COMPT)=ID_SIGBEAM(J)
              SIGBEAM(1,J)=ZERO
              ID_SIGBEAM(J)=0
            ENDIF
          ENDIF
        ENDDO
        NUMBEAM=COMPT
      ENDIF ! IF (NUMBEAM > 0)
C
      IF (NUMBEAM > 0) THEN
C        tri des elts du Y000 par ID croissant
        DO ISYS = 1, NUMBEAM
          ITRI(ISYS) = ID_SIGBEAM(ISYS)
        ENDDO
        CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMBEAM,1)
        DO J = 1, NUMBEAM
          ISYS=INDEX(J)
          KSYSUSR(J)=ID_SIGBEAM(ISYS)
          KSYSUSR(NUMBEAM+J)=ISYS
        ENDDO
C
C        Nos systeme dans le Y000, des elts du D00
        DO I=1,NUMELP
          ISYS=UEL2SYS(IXP(NIXP,I),KSYSUSR,NUMBEAM)
          PTBEAM(I) =ISYS
        ENDDO
      ENDIF ! IF (NUMBEAM > 0)
C
C TRUSS
C
      IF (NUMTRUS > 0) THEN
C        tri des elts du Y000 par ID croissant
        DO ISYS = 1, NUMTRUS
          ITRI(ISYS) =ID_SIGTRUSS(ISYS)
        ENDDO
        CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMTRUS,1)
C        checking INITRUSS : search for multiply define
        ISYS=INDEX(1)
        IE  =ID_SIGTRUSS(ISYS)
        DO J = 2, NUMTRUS
          JSYS=INDEX(J)
          JE  =ID_SIGTRUSS(JSYS)
          IF (JE == IE) THEN
            DO K=2,NSIGTRUSS
              IF (SIGTRUSS(K,JSYS) /= ZERO) THEN
                IF (SIGTRUSS(K,ISYS) /= ZERO .AND.
     .              SIGTRUSS(K,ISYS) /= SIGTRUSS(K,JSYS)) THEN
                  CALL ANCMSG(MSGID=1239,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO_BLIND_1,
     .                        I1=IE)
                ELSE
                  SIGTRUSS(K,ISYS)=SIGTRUSS(K,JSYS)
                ENDIF
              ENDIF
            ENDDO
            SIGTRUSS(1,JSYS)=ZERO
            ID_SIGTRUSS(JSYS)=0
          ELSE
            IE  =JE
            ISYS=JSYS
          ENDIF
        ENDDO
        COMPT=0
        DO J=1,NUMTRUS
          IE=ID_SIGTRUSS(J)
          IF (IE /= 0) THEN
            COMPT=COMPT+1
            IF (COMPT < J) THEN
              DO K=1,NSIGTRUSS
                SIGTRUSS(K,COMPT)=SIGTRUSS(K,J)
              ENDDO
              ID_SIGTRUSS(COMPT)=ID_SIGTRUSS(J)
              SIGTRUSS(1,J)=ZERO
              ID_SIGTRUSS(J)=0
            ENDIF
          ENDIF
        ENDDO
        NUMTRUS=COMPT
      ENDIF ! IF (NUMTRUS > 0)
C
      IF (NUMTRUS > 0) THEN
C        tri des elts du Y000 par ID croissant
        DO ISYS = 1, NUMTRUS
          ITRI(ISYS) = ID_SIGTRUSS(ISYS)
        ENDDO
        CALL MY_ORDERS(0,WORK,ITRI,INDEX,NUMTRUS,1)
        DO J = 1, NUMTRUS
          ISYS=INDEX(J)
          KSYSUSR(J)=ID_SIGTRUSS(ISYS)
          KSYSUSR(NUMTRUS+J)=ISYS
        ENDDO
C
C        Nos systeme dans le Y000, des elts du D00
        DO I=1,NUMELT
          ISYS=UEL2SYS(IXT(NIXT,I),KSYSUSR,NUMTRUS)
          PTTRUSS(I) =ISYS
        ENDDO
      ENDIF ! IF (NUMTRUS > 0)
C
      RETURN
      END

