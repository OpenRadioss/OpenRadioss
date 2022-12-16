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
Chd|  HM_PREREAD_BCSCYC             source/constraints/general/bcs/lecbcscyc.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_PREREAD_BCSCYC(IGRNOD  ,NOM_OPT ,LSUBMODEL,NBCSCYNN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD    
      USE SUBMODEL_MOD        
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "submod_c.inc"
#include      "titr_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NBCSCYNN,NOM_OPT(LNOPT1,*)
C INPUT ARGUMENTS
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IGR1,IGR2,IGRS1,IGRS2,NBCS_CY_N,ID,SUB_INDEX
      CHARACTER KEY*ncharkey,TITR*nchartitle
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NGR2USR
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
C
C======================================================================|
C
      IS_AVAILABLE = .FALSE.
C
      NBCS_CY_N = 0
C--------------------------------------------------
C START BROWSING MODEL /BCS
C--------------------------------------------------
      CALL HM_OPTION_START('/BCS')
C--------------------------------------------------
C BROWSING MODEL PARTS 1->NBCS
C--------------------------------------------------
      DO I=1,NUMBCS
        TITR = ''
C--------------------------------------------------
C EXTRACT DATAS OF /BCS/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       OPTION_TITR = TITR,
     .                       SUBMODEL_INDEX = SUB_INDEX,
     .                       KEYWORD2 = KEY)
        IF (KEY(1:6) /= 'CYCLIC' ) CYCLE
        NOM_OPT(1,I)=ID
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,I),LTITR)
c
        CALL HM_GET_INTV('grnd_ID1',IGR1,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('grnd_ID2',IGR2,IS_AVAILABLE,LSUBMODEL)
        INGR2USR => IGRNOD(1:NGRNOD)%ID
         IGRS1=NGR2USR(IGR1,INGR2USR,NGRNOD)
         IGRS2=NGR2USR(IGR2,INGR2USR,NGRNOD)
         IF (IGRS1==0) THEN
          CALL ANCMSG(MSGID=678,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1=ID,I2=IGR1,C1=TITR)
         END IF
         IF (IGRS2==0) THEN
          CALL ANCMSG(MSGID=678,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1=ID,I2=IGR2,C1=TITR)
         END IF
         IF (IGRNOD(IGRS1)%NENTITY /= IGRNOD(IGRS2)%NENTITY) THEN
          CALL ANCMSG(MSGID=1753,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1=ID,C1=TITR)
         END IF
         NBCS_CY_N = NBCS_CY_N + IGRNOD(IGRS1)%NENTITY
      ENDDO
      NBCSCYNN = 2*NBCS_CY_N
C
      RETURN
      END
Chd|====================================================================
Chd|  INI_BCSCYC                    source/constraints/general/bcs/lecbcscyc.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        INIBCS_CY                     source/constraints/general/bcs/lecbcscyc.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INI_BCSCYC(IBCSCYC,LBCSCYC,SKEW,X,ITAB,ICODE,IBFV,ITAGCYC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBCSCYC(4,*),LBCSCYC(2,*),ITAB(*),ICODE(*),IBFV(NIFV,*),
     .        ITAGCYC(*)
      my_real
     .     X(3,*),SKEW(LSKEW,*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,ISK,IAD,NN,N1,N2,ID,ITAGIMP(NUMNOD),NF1,NF2,ICOOR
C----- ini
      DO I=1,NBCSCYC
       IAD = IBCSCYC(1,I)+1
       ISK = IBCSCYC(2,I)
       NN  = IBCSCYC(3,I)
       ID  = IBCSCYC(4,I)
       CALL INIBCS_CY(NN,LBCSCYC(1,IAD),ISK,SKEW,X  ,ITAB,ID)
      END DO 
C------  ITAGCYC :ID  for incompatibility check   
      ITAGCYC(1:NUMNOD) =0
      DO I=1,NBCSCYC
       IAD = IBCSCYC(1,I)
       ISK = IBCSCYC(2,I)
       NN  = IBCSCYC(3,I)
       DO J = 1,NN
        N1 = LBCSCYC(1,IAD+J)
        N2 = LBCSCYC(2,IAD+J)
        ITAGCYC(N1) =ID
        ITAGCYC(N2) =ID
       END DO 
      END DO 
C----- check
C-------BCS for the moment uncompatible
      DO I=1,NBCSCYC
       IAD = IBCSCYC(1,I)
       ISK = IBCSCYC(2,I)
       NN  = IBCSCYC(3,I)
       ID  = IBCSCYC(4,I)
       DO J = 1,NN
        N1 = LBCSCYC(1,IAD+J)
        N2 = LBCSCYC(2,IAD+J)
        IF (ICODE(N1) >= 512 ) THEN 
          CALL ANCMSG(MSGID=1749,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1=ID,I2=ITAB(N1))
        END IF      
        IF (ICODE(N2) >= 512 ) THEN 
          CALL ANCMSG(MSGID=1750,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1=ID,I2=ITAB(N2))
        END IF      
       END DO 
      END DO 
C-------/IMPDIS,IMPVEL,IMPACC
        ITAGIMP(1:NUMNOD)=0
        DO J=1,NFXVEL
          N1 =IABS(IBFV(1,J))
          ISK = IBFV(2,J)/10
          ICOOR = IBFV(10,J)
          IF (ITAGIMP(N1)==0) THEN
            IF (ICOOR==1) THEN
             ITAGIMP(N1) = ISK
            ELSE
             ITAGIMP(N1) = -ISK
            END IF
          ELSE
            IF (ICOOR==1 .AND. ITAGIMP(N1) == ISK) THEN
            ELSE
             ITAGIMP(N1) = -ISK
            END IF
          END IF
        ENDDO
C        
      DO I=1,NBCSCYC
       IAD = IBCSCYC(1,I)
       ISK = IBCSCYC(2,I)
       NN  = IBCSCYC(3,I)
       ID  = IBCSCYC(4,I)
       DO J = 1,NN
        N1 = LBCSCYC(1,IAD+J)
        N2 = LBCSCYC(2,IAD+J)
        NF1 = ITAGIMP(N1)
        NF2 = ITAGIMP(N2)
C------ok for NF1=0,NF2=0; NF1=NF2=ISK
        IF (NF1==NF2) THEN
         IF (NF1==0.OR.NF1==ISK) THEN
         ELSE
          CALL ANCMSG(MSGID=1751,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1=ID ,I2=ITAB(N1),I3=ITAB(N2))
         END IF
        ELSE
          CALL ANCMSG(MSGID=1752,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1=ID ,I2=ITAB(N1),I3=ITAB(N2))
        END IF        
       END DO 
      END DO 
C
      RETURN
      END SUBROUTINE INI_BCSCYC
Chd|====================================================================
Chd|  INIBCS_CY                     source/constraints/general/bcs/lecbcscyc.F
Chd|-- called by -----------
Chd|        INI_BCSCYC                    source/constraints/general/bcs/lecbcscyc.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        CAR2CYLIN                     source/constraints/general/bcs/lecbcscyc.F
Chd|        MYQSORT                       ../common_source/tools/sort/myqsort.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INIBCS_CY(NBCY_N,IXCYCL,ISK,SKEW,X  ,ITAB,ID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NBCY_N,IXCYCL(2,*),ITAB(*),ISK,ID
      my_real
     .     X(3,*),SKEW(LSKEW,*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,N1(NBCY_N),N2(NBCY_N),INDEX(NBCY_N),NN(NBCY_N),IER1
C
      my_real
     .     CY_X1(3,NBCY_N), CY_X2(3,NBCY_N),DIS1(NBCY_N),DIS2(NBCY_N),LMIN,
     .     CY_TMP(3,NBCY_N),RI,ZI,TOL,ERR_TH,ERMAX
C========================================================================|
C-----for each cut-section nodes, compute cylindrical co-ordinates and dis
      DO I=1,NBCY_N
        N1(I) = IXCYCL(1,I)
        N2(I) = IXCYCL(2,I)
      ENDDO
C-------5% error      
      ERR_TH=ZEP05
      CALL CAR2CYLIN(NBCY_N,N1,X,CY_X1,DIS1,
     .               SKEW(1,ISK),SKEW(10,ISK),ERR_TH,IER1)
c--------check  (r,cos(theta),z), cos(theta) not too diff 
      IF (IER1<0 ) THEN 
         CALL ANCMSG(MSGID=1761,ANMODE=ANINFO,MSGTYPE=MSGERROR,I1=ID)
      END IF      
C------sorting by dis      
      CALL MYQSORT(NBCY_N, DIS1, INDEX, IER1)
      CY_TMP(1:3,1:NBCY_N) = CY_X1(1:3,1:NBCY_N)
      DO I=1,NBCY_N
        J = INDEX(I)
        N1(I) = IXCYCL(1,J)
        CY_X1(1:3,I)=CY_TMP(1:3,J)
      ENDDO
      LMIN = EP20     
      DO I=2,NBCY_N
        RI = ABS(CY_X1(1,I)-CY_X1(1,I-1))
        ZI = ABS(CY_X1(3,I)-CY_X1(3,I-1))
        LMIN =MIN(LMIN,MAX(RI,ZI))
      ENDDO 
      CALL CAR2CYLIN(NBCY_N,N2,X,CY_X2,DIS2,
     .               SKEW(1,ISK),SKEW(10,ISK),ERR_TH,IER1)
c--------check  (r,cos(theta),z), cos(theta) not too diff     
      IF (IER1<0 ) THEN 
         CALL ANCMSG(MSGID=1762,ANMODE=ANINFO,MSGTYPE=MSGERROR,I1=ID)
      END IF      
C------sorting by dis      
      CALL MYQSORT(NBCY_N, DIS2, INDEX, IER1)
      CY_TMP(1:3,1:NBCY_N) = CY_X2(1:3,1:NBCY_N)
      DO I=1,NBCY_N
        J = INDEX(I)
        N2(I) = IXCYCL(2,J)
        CY_X2(1:3,I)=CY_TMP(1:3,J)
      ENDDO 
      DO I=2,NBCY_N
        RI = ABS(CY_X2(1,I)-CY_X2(1,I-1))
        ZI = ABS(CY_X2(3,I)-CY_X2(3,I-1))
        LMIN =MIN(LMIN,MAX(RI,ZI))
      ENDDO
      TOL =  LMIN*ERR_TH    
      ERMAX = ZERO
      J = 1
      DO I=1,NBCY_N
        RI = ABS(CY_X2(1,I)-CY_X1(1,I))
        ZI = ABS(CY_X2(3,I)-CY_X1(3,I))
        LMIN =MAX(RI,ZI)
        IF (LMIN>ERMAX) THEN
         ERMAX=LMIN
         J = I
        END IF
      ENDDO
      IF (ERMAX>TOL ) THEN 
          CALL ANCMSG(MSGID=1763,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1=ID,I2=ITAB(N1(J)),I3=ITAB(N2(J)))
      END IF      
      DO I=1,NBCY_N
        IXCYCL(1,I) = N1(I)
        IXCYCL(2,I) = N2(I) 
      ENDDO      
C
      RETURN
      END SUBROUTINE INIBCS_CY
Chd|====================================================================
Chd|  CAR2CYLIN                     source/constraints/general/bcs/lecbcscyc.F
Chd|-- called by -----------
Chd|        INIBCS_CY                     source/constraints/general/bcs/lecbcscyc.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CAR2CYLIN(NBCY_N,IX,X,CY_X,DIS,SKEW,XYZ0,TOL,IER)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NBCY_N,IX(*),IER
      my_real
     .     X(3,*),SKEW(9),XYZ0(3),CY_X(3,*),DIS(*),TOL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J 
C
      my_real
     .     XX,YY,ZZ,XL,YL,ZL,R2,TH_MEAN,TH_MAX,ZL_MIN
C========================================================================|
C----- compute cylindrical co-ordinates(r,cos(theta),z) and dis=r*r+z*z
      TH_MEAN =ZERO
      ZL_MIN = EP20
      DO I=1,NBCY_N
        XX = X(1,IX(I))-XYZ0(1)
        YY = X(2,IX(I))-XYZ0(2)
        ZZ = X(3,IX(I))-XYZ0(3)
        XL = XX*SKEW(1)+YY*SKEW(2)+ZZ*SKEW(3)
        YL = XX*SKEW(4)+YY*SKEW(5)+ZZ*SKEW(6)
        ZL = XX*SKEW(7)+YY*SKEW(8)+ZZ*SKEW(9)
        R2 = XL*XL+YL*YL
        CY_X(1,I) = SQRT(R2)
        CY_X(2,I) = XL/CY_X(1,I)
        CY_X(3,I) = ZL
        DIS(I) = R2
        TH_MEAN = TH_MEAN + CY_X(2,I)
        ZL_MIN = MIN(ZL_MIN,ZL)
      ENDDO
      DO I=1,NBCY_N
        CY_X(3,I) = CY_X(3,I)-ZL_MIN
        DIS(I) = DIS(I) + CY_X(3,I)*CY_X(3,I)
      ENDDO
      TH_MEAN =TH_MEAN/NBCY_N
      IER = 0
      TH_MAX =ZERO
      DO I=1,NBCY_N
       TH_MAX = MAX(TH_MAX,ABS(CY_X(2,I)-TH_MEAN))
      ENDDO
c      print *,'TH_MAX,TH_MEAN=',TH_MAX,TH_MEAN
      IF (TH_MAX>TOL*ABS(TH_MEAN)) IER = -1
C--- numeric      
      IF (TH_MAX<EM6) IER = 0
C
      RETURN
      END SUBROUTINE CAR2CYLIN
Chd|====================================================================
Chd|  INT2CY_CHK                    source/constraints/general/bcs/lecbcscyc.F
Chd|-- called by -----------
Chd|        ININTR2                       source/interfaces/inter3d1/inintr2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INT2CY_CHK(IPARI,INTBUF_TAB,ITAGCYC,ITAB)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,NINTER),ITAGCYC(*),ITAB(*)
      TYPE(INTBUF_STRUCT_), DIMENSION(NINTER) :: INTBUF_TAB
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB
      EXTERNAL INTAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NTY,NSN,NMN,ISL,NKIN,NOINT,IMODI,II,N1,N2,ND
      INTEGER K,ILEV,NUVAR,IDEL7N,INTTH,IPR,IML,ICOM
      CHARACTER*nchartitle, TITR
      LOGICAL IS1,IS2,ISD
C=======================================================================
       DO N=1,NINTER
        NTY  = IPARI(7,N)
        IF (NTY == 2 ) THEN
         NSN   = IPARI(5,N)
         ILEV = IPARI(20,N)
         NOINT   = IPARI(15,N)
C----------only kinematic ones         
         IF (ILEV >= 25 .AND. ILEV <= 28) CYCLE
         DO I=1,NSN
           ISL = INTBUF_TAB(N)%NSV(I)
           IF (ITAGCYC(ISL)/=0) THEN
              CALL ANCMSG(MSGID=1758,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                    I1=ITAGCYC(ISL),I2=ITAB(ISL),I3=NOINT)
           END IF
         END DO 
        END IF
       END DO 
C       
c-----------
      RETURN
      END SUBROUTINE INT2CY_CHK
C   kinchk 
C   2) partial incompatible : impvel (same between n1,n2 with icoord=1)
C   3) special case         : bcs  now:1) future :2)
C   4) remove ND of NS10E at NS10E side
C   nspmd>1 : n1,n2 in the same domain
