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
Chd|  SPGRTAILS                     source/elements/sph/spgrtails.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        ZEROIN                        source/system/zeroin.F        
Chd|        GET_U_GEO                     source/user_interface/uaccess.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|====================================================================
      SUBROUTINE SPGRTAILS(KXSP   , IPARG , PM  ,IPART ,
     2                     IPARTSP, EADD,   ND  , CEPSP, DD_IAD,
     3                     IDX,   IXSP, IPM  , IGEO,
     4                     SPBUF,SPH2SOL,SOL2SPH,
     5                     IRST   , NOD2SP ,PRINT_FLAG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MOD_SPGRHEAD
      USE MESSAGE_MOD
      USE R2R_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IDX,ND,
     .        KXSP(NISP,*),IPARG(NPARG,*),DD_IAD(NSPMD+1,*),EADD(*),
     .        IPART(LIPART1,*),IPARTSP(*),CEPSP(*),IXSP(KVOISPH,NUMSPH),
     .        IPM(NPROPMI,*), IGEO(NPROPGI,*),
     .        SPH2SOL(*), SOL2SPH(2,*), IRST(3,*), NOD2SP(*)
      INTEGER, INTENT(IN) :: PRINT_FLAG !< flag to print the element group data
      my_real
     .        PM(NPROPM,*), SPBUF(NSPBUF,NUMSPH)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGR1, NG, MT, MLN, I, P, NEL, MODE, NB,
     .        N, IGTYP,JIVF,JHBE,IJK,NE1,
     .        ISSN,IKSNOD,IORDER,IPRT,ISLEEP,IEOS,NEL_PREC,IUN,IG,IFAIL, 
     .        WORK(70000),INUM(13,NUMSPH),INDEX(2*NUMSPH),
     .        NGP(NSPMD+1),K,J,II, MX, NUMBER_FAILURE, IR, IP, STAT,
     .        IPARTR2R, NOD, JALE_FROM_MAT, JALE_FROM_PROP
C     REAL
      my_real
     .        RNUM(NSPBUF,NUMSPH)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      DATA IUN/1/
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      my_real
     .         GET_U_GEO
      EXTERNAL GET_U_GEO
C--------------------------------------------------------------
      ALLOCATE(IXSPS(KVOISPH,NUMSPH),STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                           C1='IXSPS')
C--------------------------------------------------------------
C         BORNAGE DES GROUPES DE MVSIZ
C--------------------------------------------------------------

      NEL = 0
      DO N=1,ND
        NEL = NEL + EADD(N+1)-EADD(N)
      ENDDO
      NGR1 = NGROUP + 1
C
C phase 1 : domain decompostition
C
      IDX=IDX+ND*(NSPMD+1)
      CALL ZEROIN(1,ND*(NSPMD+1),DD_IAD(1,NSPGROUP+1))
C     NSPGROUP = NSPGROUP + ND
      NFT = 0
C initialisation dd_iad
      DO N=1,ND
       DO P=1,NSPMD+1
         DD_IAD(P,NSPGROUP+N) = 0
       END DO
      END DO
C
      DO N=1,ND
        NEL = EADD(N+1)-EADD(N)
        DO I = 1, NEL
          INDEX(I) = I
          INUM(1,I)=IPARTSP(NFT+I)
          INUM(2,I)=KXSP(1,NFT+I)
          INUM(3,I)=KXSP(2,NFT+I)
          INUM(4,I)=KXSP(3,NFT+I)
          INUM(5,I)=KXSP(4,NFT+I)
          INUM(6,I)=KXSP(5,NFT+I)
          INUM(7,I)=KXSP(6,NFT+I)
          INUM(8,I)=KXSP(7,NFT+I)
          INUM(9,I)=KXSP(8,NFT+I)
c retri spbuf
          DO K=1,NSPBUF
           RNUM(K,I)=SPBUF(K,NFT+I)
          END DO
        END DO

        DO I = 1, NEL
          DO J = 1, KVOISPH
            IXSPS(J,I) = IXSP(J,NFT+I)
          END DO
        END DO
        MODE=0
        CALL MY_ORDERS( MODE, WORK, CEPSP(NFT+1), INDEX, NEL , 1)
        DO I = 1, NEL
          IPARTSP(I+NFT)=INUM(1,INDEX(I))
          KXSP(1,I+NFT)=INUM(2,INDEX(I))
          KXSP(2,I+NFT)=INUM(3,INDEX(I))
          KXSP(3,I+NFT)=INUM(4,INDEX(I))
          KXSP(4,I+NFT)=INUM(5,INDEX(I))
          KXSP(5,I+NFT)=INUM(6,INDEX(I))
          KXSP(6,I+NFT)=INUM(7,INDEX(I))
          KXSP(7,I+NFT)=INUM(8,INDEX(I))
          KXSP(8,I+NFT)=INUM(9,INDEX(I))

c tri SPBUF
          DO K=1,NSPBUF
            SPBUF(K,I+NFT)=RNUM(K,INDEX(I))
          END DO
        END DO

        DO I = 1, NEL
          DO J = 1, KVOISPH
            IXSP(J,I+NFT) = IXSPS(J,INDEX(I))
          END DO
        END DO
C
        IF(NSPHSOL/=0)THEN
          DO I=1,NEL
            INUM(10,I)=SPH2SOL(NFT+I)
            IF(NFT+I >= FIRST_SPHSOL .AND. 
     .         NFT+I < FIRST_SPHSOL+NSPHSOL)THEN
              INUM(11,I)=IRST(1,NFT+I-FIRST_SPHSOL+1)
              INUM(12,I)=IRST(2,NFT+I-FIRST_SPHSOL+1)
              INUM(13,I)=IRST(3,NFT+I-FIRST_SPHSOL+1)
            END IF
          END DO
          DO I=1,NEL
            SPH2SOL(NFT+I) = INUM(10,INDEX(I))
C
            IF(NFT+I >= FIRST_SPHSOL .AND. 
     .         NFT+I < FIRST_SPHSOL+NSPHSOL)THEN
C INDEX(I) < FIRST_SPHSOL <=> internal error
              IRST(1,NFT+I-FIRST_SPHSOL+1)=INUM(11,INDEX(I))
              IRST(2,NFT+I-FIRST_SPHSOL+1)=INUM(12,INDEX(I))
              IRST(3,NFT+I-FIRST_SPHSOL+1)=INUM(13,INDEX(I))
            END IF
          END DO
        END IF
C
        P = CEPSP(NFT+INDEX(1))
        NB = 1
        DO I = 2, NEL
          IF (CEPSP(NFT+INDEX(I))/=P) THEN
            DD_IAD(P+1,NSPGROUP+N) = NB
            NB = 1
            P = CEPSP(NFT+INDEX(I))
          ELSE
            NB = NB + 1
          ENDIF
        END DO
        DD_IAD(P+1,NSPGROUP+N) = NB
        DO P = 2, NSPMD
          DD_IAD(P,NSPGROUP+N) = DD_IAD(P,NSPGROUP+N)
     .                         + DD_IAD(P-1,NSPGROUP+N)
        END DO
        DO P = NSPMD+1,2,-1
          DD_IAD(P,NSPGROUP+N) = DD_IAD(P-1,NSPGROUP+N)+1
        END DO
        DD_IAD(1,NSPGROUP+N) = 1
C
C maj CEP
C
        DO I = 1, NEL
          INDEX(I) = CEPSP(NFT+INDEX(I))          
        END DO
        DO I = 1, NEL
          CEPSP(NFT+I) = INDEX(I)          
        END DO
C
        NFT = NFT + NEL
C
      END DO
C
C Rebuild SOL2SPH, SOL2SPH(1,N)+1<=I<=SOLSPH(2,N) <=> N==SPH2SOL(I)
      IF(NSPHSOL/=0)THEN
        DO N=1,NUMELS8
          SOL2SPH(1,N)=0
          SOL2SPH(2,N)=0
        END DO
        N=SPH2SOL(FIRST_SPHSOL)
        SOL2SPH(1,N)=FIRST_SPHSOL-1
        SOL2SPH(2,N)=SOL2SPH(1,N)+1
        DO I=FIRST_SPHSOL+1,FIRST_SPHSOL+NSPHSOL-1
          IF(SPH2SOL(I)==N)THEN
            SOL2SPH(2,N)=SOL2SPH(2,N)+1
          ELSE
            N=SPH2SOL(I)
            SOL2SPH(1,N)=I-1
            SOL2SPH(2,N)=SOL2SPH(1,N)+1
          END IF
        END DO          
      END IF
C ne pas oublier renumeroter selection th et surface si concerne
C
C
C phase 2 : bornage en groupe de mvsiz
C ngroup est global, iparg est global mais organise en fonction de dd
C
      JALE=0
      JEUL=0
      JTUR=0
      JTHE=0
      JIVF=0
      JPOR=0
C
      ISSN   =0
      NPT    =1
      IKSNOD =1
      JHBE   =0
C
      DO N=1,ND
       NFT = 0
       DO P = 1, NSPMD
        NGP(P)=0
        NEL = DD_IAD(P+1,NSPGROUP+N)-DD_IAD(P,NSPGROUP+N)
        IF (NEL>0) THEN
         NEL_PREC = DD_IAD(P,NSPGROUP+N)-DD_IAD(1,NSPGROUP+N)
         NGP(P)=NGROUP
         NG  = (NEL-1)/NVSIZ + 1
         DO I=1,NG
C ngroup global
          ISTRAIN=0
          NGROUP=NGROUP+1
          II = EADD(N)+NFT
          IPRT  =IPARTSP(II)
          IPARTR2R = 0
          IF (NSUBDOM>0) IPARTR2R = TAG_PART(IPRT)          
          MT    =IPART(1,IPRT)
          MLN   =NINT(PM(19,ABS(MT)))
          IG    =IPART(2,IPRT)
          IGTYP = IGEO(11,IG)
          ISORTH= MAX(IGEO(17,IG),MIN(IUN,IGEO(2,IG)))
          ISRAT = IPM(3,MT)
          IEOS  = IPM(4,MT)
          IORDER=INT(GET_U_GEO(5,IG))
          ISLEEP=KXSP(2,II)
          
          JALE_FROM_MAT = NINT(PM(72,MT))
          JALE_FROM_PROP = IGEO(62,IG)
          JALE = MAX(JALE_FROM_MAT, JALE_FROM_PROP) !if inconsistent, error message was displayed in PART reader

          JLAG=0
          IF(JALE==0.AND.MLN/=18)JLAG=1
          JEUL=0
          IF(JALE==2)THEN
            JALE=0
            JEUL=1
          ENDIF
          IF (JALE+JEUL/=0) THEN
C           WRITE(ISTDO,*) ' ** ERROR : BAD ANALYSIS TYPE'
C           WRITE(IOUT,*)  ' ** ERROR : BAD ANALYSIS TYPE'
C           WRITE(IOUT,*)  ' ** ERROR : ALE AND EULERIAN ANALYSIS',
C     .                 ' ARE NOT COMPATIBLE WITH SPH.'
C           IERR=IERR+1
            ID=IGEO(1,IG)
            CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1,IG),LTITR)
            CALL ANCMSG(MSGID=403,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,
     .                  C1=TITR)
          ENDIF
          JTUR=NINT(PM(70,MT))
          JTHE=NINT(PM(71,MT))

          ISPH2SOL=0
          IF(NSPHSOL/=0)ISPH2SOL=SPH2SOL(II)
C     full geometric non linearities.
          NE1 = MIN( NVSIZ, NEL + NEL_PREC - NFT)
          IFAIL = 0
          IF(IPM(111,MT) > 0)THEN
           IFAIL = 1
          ENDIF
C-------------------------------------------------          
          IF((MLN/=14).AND.(MLN/=24).AND.(MLN/=25).AND.
     .       (MLN<28))THEN
            NUMBER_FAILURE = IPM(220, MT)
            DO IJK = 1, NE1
              II = EADD(N)+NFT-1+IJK
              MX = IPART(1,IPARTSP(II))
              DO IR = 1,NUMBER_FAILURE 
                IP=(IR -1)*15
                IF(IPM(111 + IP,MX)==10) THEN
                  ISTRAIN=1
                  GO TO 100
                END IF
              END DO
            END DO
 100        CONTINUE
          ENDIF
C-------------------------------------------------          
          DO IJK = 1, NE1
C
C Attention, penser a sauvegarder KXSP(2) si besoin (backtrack spmd si interface)
C
            KXSP(2,EADD(N)-1+NFT+IJK)=
     .                SIGN(NGROUP,ISLEEP)
          END DO
          IF (MT/=0) THEN
            IPARG(1,NGROUP)=MLN
          ELSE
            IPARG(1,NGROUP)=IGTYP
          END IF
          IPARG(2,NGROUP)=NE1
          IPARG(3,NGROUP)=EADD(N)-1 + NFT
          IPARG(4,NGROUP)=LBUFEL+1
          IPARG(5,NGROUP)=51
          IPARG(6,NGROUP)=NPT
          IPARG(7,NGROUP) =JALE
          IF(ISLEEP==-1.OR.ISPH2SOL/=0)IPARG(8,NGROUP) =1
          IPARG(9,NGROUP) =ISSN
          IF(ISLEEP>0)IPARG(10,NGROUP)=NE1
          IPARG(11,NGROUP)=JEUL
          IPARG(12,NGROUP)=JTUR
          IPARG(13,NGROUP)=-ABS(JTHE)
          IPARG(14,NGROUP)=JLAG
          IPARG(18,NGROUP)=0    ! NMTV(MLN)-11
          IPARG(23,NGROUP)=JHBE
          IPARG(24,NGROUP)=JIVF
          IPARG(27,NGROUP)=JPOR
          IPARG(28,NGROUP)=IKSNOD
          IPARG(32,NGROUP)= P-1
          IPARG(38,NGROUP)=IGTYP
          IPARG(40,NGROUP)=ISRAT
          IPARG(42,NGROUP)=ISORTH
          IPARG(43,NGROUP)=IFAIL
          IPARG(62,NGROUP)=IG
          IPARG(69,NGROUP)=ISPH2SOL
          
C         flag for group of duplicated elements in multidomains
          IF (NSUBDOM>0) IPARG(71,NGROUP)= IPARTR2R
C         thermal expansion
          IF(IPM(218,MT) > 0 .AND. MLN /= 0 .AND. MLN /= 13) IPARG(49,NGROUP)= 1 
C
          IF(MLN/=14.AND.MLN/=24.AND.MLN/=25.AND.MLN<28)THEN
            IPARG(44,NGROUP)= ISTRAIN
          ELSEIF(MLN>=28)THEN
            ISTRAIN=2
            IPARG(44,NGROUP)=ISTRAIN
          ENDIF
C
C         equation of state
          IPARG(55,NGROUP)= IEOS
          NFT = NFT + NE1
         ENDDO
         NGP(P)=NGROUP-NGP(P)
        ENDIF
       ENDDO
C DD_IAD => nb groupes par sous domaine
       NGP(NSPMD+1)=0
       DO P = 1, NSPMD
         NGP(NSPMD+1)=NGP(NSPMD+1)+NGP(P)
         DD_IAD(P,NSPGROUP+N)=NGP(P)
       END DO
       DD_IAD(NSPMD+1,NSPGROUP+N)=NGP(NSPMD+1)
C
      END DO
      NSPGROUP = NSPGROUP + ND
C
      ! rebuild NOD2SP after KXSP renumerotation
      NOD2SP(1:NUMNOD) = 0
      DO I = 1, NUMSPH
        NOD = KXSP(3,I)
        NOD2SP(NOD) = I
      END DO 
C
      IF(PRINT_FLAG>6) THEN
          WRITE(IOUT,1000)
          WRITE(IOUT,1001)(N,IPARG(1,N),IPARG(2,N),IPARG(3,N)+1,
     +                   IPARG(4,N),IPARG(5,N),IPARG(55,N),
     +                 N=NGR1,NGROUP)
      ENDIF
C
 1000 FORMAT(10X,' 3D - SPH CELL GROUPS '/
     +       10X,' -------------------- '/
     +' GROUP   CELL       CELL      FIRST    BUFFER   CELL     IEOS'/
     +'         MATERIAL   NUMBER    CELL     ADDRESS  TYPE     TYPE'/)
 1001 FORMAT(7(1X,I7,1X))
C-----------
      DEALLOCATE(IXSPS)
C-----------
      RETURN
      END
