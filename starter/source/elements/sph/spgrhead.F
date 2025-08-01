Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    spgrhead           ../starter/source/elements/sph/spgrhead.F
!||--- called by ------------------------------------------------------
!||    lectur             ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    get_u_geo          ../starter/source/user_interface/uaccess.F
!||--- uses       -----------------------------------------------------
!||    message_mod        ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE SPGRHEAD(KXSP    ,IXSP    ,IPARG     ,PM  ,IPART,
     2                    IPARTSP ,EADD    ,CEPSP     ,ND  ,IPM  ,
     3                    IGEO    ,SPBUF   ,SPH2SOL   ,
     4                    SOL2SPH ,IRST    ,MAT_PARAM ,IXSPS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
      USE MATPARAM_DEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, DIMENSION(KVOISPH,NUMSPH),INTENT(INOUT) :: IXSPS
      INTEGER KXSP(NISP,*),IPARG(NPARG,*),IXSP(KVOISPH,*),
     .        IPART(LIPART1,*),IPARTSP(*), EADD(*), CEPSP(*),
     .        IPM(NPROPMI,NUMMAT), IGEO(NPROPGI,NUMGEO),
     .        ND, SPH2SOL(*), SOL2SPH(2,*), IRST(3,NSPHSOL)
      my_real PM(NPROPM,NUMMAT), SPBUF(NSPBUF,NUMSPH)
      TYPE(MATPARAM_STRUCT_) ,DIMENSION(NUMMAT), INTENT(IN) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NE, NG, MT, MLN, I, J, MODE, II0, JJ0, 
     .        II, JJ, II1, JJ1, II2, JJ2, II3, JJ3, II4, JJ4,
     .        N, IGTYP,IORDER,IPRT,ISLEEP,IUN,IFAIL,IEOS, IKIND, STAT,
     .        JALE_FROM_MAT, JALE_FROM_PROP
      INTEGER WORK(70000)
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: ITRI
      INTEGER, DIMENSION(:),ALLOCATABLE   :: INDEX
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: INUM
      INTEGER, DIMENSION(:),ALLOCATABLE   :: XEP
      my_real, DIMENSION(:,:),ALLOCATABLE :: RNUM
      DATA IUN/1/
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  MY_SHIFTL,MY_SHIFTR,MY_AND
      EXTERNAL MY_SHIFTL,MY_SHIFTR,MY_AND
      my_real, EXTERNAL :: GET_U_GEO
C-----------------------------------------------

C======================================================================|
C  GENERAL SORTING : ALL CRITERIA, ALL ELEMENTS
C----------------------------------------------------------
      CALL MY_ALLOC(ITRI,7,NUMSPH)
      CALL MY_ALLOC(INDEX,2*NUMSPH)
      CALL MY_ALLOC(INUM,13,NUMSPH)
      CALL MY_ALLOC(XEP,NUMSPH)
      CALL MY_ALLOC(RNUM,NSPBUF,NUMSPH)
      
      DO I=1,NUMSPH
        IF(NSPHSOL==0)THEN
          ITRI(1,I)=0
        ELSE
          ITRI(1,I)=SPH2SOL(I)
        END IF
        ITRI(7,I)=I
        INDEX(I)=I
        INUM(1,I)=IPARTSP(I)
        INUM(2,I)=KXSP(1,I)
        INUM(3,I)=KXSP(2,I)
        INUM(4,I)=KXSP(3,I)
        INUM(5,I)=KXSP(4,I)
        INUM(6,I)=KXSP(5,I)
        INUM(7,I)=KXSP(6,I)
        INUM(8,I)=KXSP(7,I)
        INUM(9,I)=KXSP(8,I)
C tri spbuf     
        DO J=1,NSPBUF
         RNUM(J,I)=SPBUF(J,I)
        END DO
       ENDDO

      DO I=1,NUMSPH
        XEP(I)=CEPSP(I)
      END DO

      DO I = 1, NUMSPH
        DO J = 1, KVOISPH
          IXSPS(J,I) = IXSP(J,I)
        END DO
      END DO
C
      DO I = 1, NUMSPH
        IPRT  =IPARTSP(I)
        MT    =IPART(1,IPRT)
        MLN   =NINT(PM(19,ABS(MT)))
        NG    =IPART(2,IPRT)
        IGTYP = IGEO(11,NG)
        ISORTH= MIN(IUN,IGEO(2,NG))
        ISRAT = IPM(3,MT)
        IEOS  = IPM(4,MT)
C   warning : -1<=IORDER<=1
        IORDER=GET_U_GEO(5,NG)
        ISLEEP=KXSP(2,I)
        IF(NSPHSOL==0)THEN
          ITRI(1,I)=0
        ELSE
          ITRI(1,I)=SPH2SOL(I)
        END IF
C
        JALE_FROM_MAT = NINT(PM(72,MT))
        JALE_FROM_PROP = IGEO(62,NG)
        JALE = MAX(JALE_FROM_MAT, JALE_FROM_PROP) !if inconsistent, error message was displayed in PART reader
        
        JLAG=0
        IF(JALE==0.AND.MLN/=18)JLAG=1
        JEUL=0
        IF(JALE==2)THEN
          JALE=0
          JEUL=1
        END IF
        JTUR=NINT(PM(70,MT))
        JTHE=NINT(PM(71,MT))
        IFAIL = 0
        IF (MAT_PARAM(MT)%NFAIL > 0) IFAIL = 1
C Key 1
        JTHE=MY_SHIFTL(JTHE,1)
        JTUR=MY_SHIFTL(JTUR,2)
        JEUL=MY_SHIFTL(JEUL,3)
        JLAG=MY_SHIFTL(JLAG,4)
        JALE=MY_SHIFTL(JALE,5)
C        ISSN=MY_SHIFTL(ISSN,6)
C        JHBE=MY_SHIFTL(JHBE,9)
C        JPOR=MY_SHIFTL(JPOR,12)
! do not sort in the following cases
        IF(MLN<28.OR.MLN==36.OR.MLN==46.OR.MLN==47)MLN=0
        MLN   = MY_SHIFTL(MLN,21)
        IFAIL = MY_SHIFTL(IFAIL,31)
        ITRI(2,I)=MLN+JALE+JLAG+JEUL+JTUR+JTHE+IFAIL
C
        ITRI(3,I)=NG
C
        ITRI(4,I)=MT
C Key 4
        IORDER= MY_SHIFTL(IORDER,0)
        ISORTH= MY_SHIFTL(ISORTH,2)
        ISRAT = MY_SHIFTL(ISRAT,3)
        IEOS  = MY_SHIFTL(IEOS,5) 
C       next  = MY_SHIFTL(next,9) 
        ITRI(5,I)=IORDER+ISRAT+ISORTH+IEOS
C Key5 5
        ITRI(6,I)=ISLEEP
      END DO
C
      MODE = 0
      CALL MY_ORDERS( MODE, WORK, ITRI, INDEX, NUMSPH , 7)
C
      DO I=1,NUMSPH
        IPARTSP(I)= INUM(1,INDEX(I))
        KXSP(1,I) = INUM(2,INDEX(I))
        KXSP(2,I) = INUM(3,INDEX(I))
        KXSP(3,I) = INUM(4,INDEX(I))
        KXSP(4,I) = INUM(5,INDEX(I))
        KXSP(5,I) = INUM(6,INDEX(I))
        KXSP(6,I) = INUM(7,INDEX(I))
        KXSP(7,I) = INUM(8,INDEX(I))
        KXSP(8,I) = INUM(9,INDEX(I))

c      sorting spbuf
       DO J=1,NSPBUF
         SPBUF(J,I) = RNUM(J,INDEX(I))
       ENDDO 
      END DO
C
      DO I=1,NUMSPH
        CEPSP(I) = XEP(INDEX(I))
      END DO
C
      DO I = 1, NUMSPH
        DO J = 1, KVOISPH
          IXSP(J,I) = IXSPS(J,INDEX(I))
        END DO
      END DO
C
      IF(NSPHSOL/=0)THEN
C
        DO I=1,NUMSPH
          INUM(10,I)=SPH2SOL(I)
          IF(I >= FIRST_SPHSOL .AND. I <  FIRST_SPHSOL+NSPHSOL)THEN
            INUM(11,I)=IRST(1,I-FIRST_SPHSOL+1)
            INUM(12,I)=IRST(2,I-FIRST_SPHSOL+1)
            INUM(13,I)=IRST(3,I-FIRST_SPHSOL+1)
          END IF
        END DO
C
        DO I=1,NUMSPH
          SPH2SOL(I) = INUM(10,INDEX(I))
          IF(I >= FIRST_SPHSOL .AND. I < FIRST_SPHSOL+NSPHSOL)THEN
C INDEX(I) < FIRST_SPHSOL <=> internal error
            IRST(1,I-FIRST_SPHSOL+1)=INUM(11,INDEX(I))
            IRST(2,I-FIRST_SPHSOL+1)=INUM(12,INDEX(I))
            IRST(3,I-FIRST_SPHSOL+1)=INUM(13,INDEX(I))
          END IF
        END DO
C
C Rebuild SOL2SPH, SOL2SPH(1,N)+1<=I<=SOLSPH(2,N) <=> N==SPH2SOL(I)
        DO N=1,NUMELS8
          SOL2SPH(1,N)=0
          SOL2SPH(2,N)=0
        END DO
        N=SPH2SOL(FIRST_SPHSOL)
        SOL2SPH(1,N)=0
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
C
      END IF
C reneumbering th groups and surface buffer
C--------------------------------------------------------------
C         DETERMINATION DES SUPER_GROUPES
C--------------------------------------------------------------
      ND=1
      EADD(1) = 1
      DO I=2,NUMSPH
        II0=ITRI(1,INDEX(I))
        JJ0=ITRI(1,INDEX(I-1))
        II=ITRI(2,INDEX(I))
        JJ=ITRI(2,INDEX(I-1))
        II1=ITRI(3,INDEX(I))
        JJ1=ITRI(3,INDEX(I-1))
        II2=ITRI(4,INDEX(I))
        JJ2=ITRI(4,INDEX(I-1))
        II3=ITRI(5,INDEX(I))
        JJ3=ITRI(5,INDEX(I-1))
        II4=ITRI(6,INDEX(I))
        JJ4=ITRI(6,INDEX(I-1))
        IF((II0==0.AND.II0/=JJ0) .OR. II/=JJ .OR. II1/=JJ1.OR.II2/=JJ2 .OR. II3/=JJ3.OR.II4/=JJ4) THEN
          ND=ND+1
          EADD(ND)=I
        END IF
      END DO
      EADD(ND+1) = NUMSPH+1
      NE = 0
      DO N=1,ND
        NE = NE + EADD(N+1)-EADD(N)
      ENDDO
      DEALLOCATE(ITRI)
      DEALLOCATE(INDEX)
      DEALLOCATE(INUM)
      DEALLOCATE(XEP)
      DEALLOCATE(RNUM)

C-----------
      RETURN
      END
