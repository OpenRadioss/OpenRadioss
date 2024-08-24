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
      !||====================================================================
      !||    inint2                 ../starter/source/interfaces/inter2d1/inint2.F
      !||--- called by ------------------------------------------------------
      !||    inintr                 ../starter/source/interfaces/interf1/inintr.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||    i1bcs_check            ../starter/source/interfaces/int01/i1bcs_check.F90
      !||    i1chk2                 ../starter/source/interfaces/inter2d1/i1chk2.F
      !||    i1tid2                 ../starter/source/interfaces/inter2d1/i1tid2.F
      !||    i2main                 ../starter/source/interfaces/interf1/i2master.F
      !||    i3pen2                 ../starter/source/interfaces/inter2d1/i3pen2.F
      !||    i3sti2                 ../starter/source/interfaces/inter2d1/i3sti2.F
      !||    i9bcs_check            ../starter/source/interfaces/int09/i9bcs_check.F90
      !||    i9sti2                 ../starter/source/interfaces/int09/i9sti2.F
      !||    inint0                 ../starter/source/interfaces/interf1/inint0.F
      !||    invoi2                 ../starter/source/interfaces/inter2d1/invoi2.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE ININT2(INTBUF_TAB ,INSCR    ,X         ,IXQ     ,SINSCR   ,
     .                  PM         ,GEO      ,IPARI     ,NINT    ,ITAB     ,
     .                  ITABM1     ,NUMNOD   ,IKINE     ,MWA     ,IPM      ,
     .                  ID         ,TITR     ,KNOD2ELQ  ,NOD2ELQ ,SEGQUADFR,
     .                  NUMMAT     ,NINTER   ,SITAB     ,SITABM1 ,SICODE   ,
     .                  ICODE)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Interfaces initialization for 2D analysis (N2D>0)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: SITAB, SITABM1 !< array sizes
      INTEGER,INTENT(IN) :: SICODE !< array size ICODE
      INTEGER,INTENT(IN) :: ICODE(SICODE) !< boundary condition code for each node
      INTEGER,INTENT(IN) :: NUMMAT,NINTER,SINSCR !< array sizes
      INTEGER NINT, NUMNOD
      INTEGER INSCR(*), IXQ(*), IPARI(NPARI), ITAB(SITAB),
     .        ITABM1(SITABM1), IKINE(*), MWA(*),IPM(NPROPMI,NUMMAT),
     .        KNOD2ELQ(*),NOD2ELQ(*),SEGQUADFR(2,*)
      my_real X(*), PM(NPROPM, NUMMAT), GEO(*)
      INTEGER ID
      CHARACTER(LEN=NCHARTITLE) :: TITR
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NRTS, NRTM, NSN, NMN, NTY, NST, NMT, NOINT, K10, K11, K12,
     .   K13, K14, KFI, J10, J11, J12, JFI, K16, K21, K23, J20, L17,
     .   L20, L22, J21, J22, L16, L21, L23, K15, K17, K18, K19, K20,
     .   K22, J13, J14, J15, J16, J17, J18, J19, IWPENE, K24, K25,K48,
     .   IBIDON,I
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IWPENE = 0
      NRTS  = IPARI(3)
      NRTM  = IPARI(4)
      NSN   = IPARI(5)
      NMN   = IPARI(6)
      NTY   = IPARI(7)
      NST   = IPARI(8)
      NMT   = IPARI(9)
      NOINT = IPARI(15)
C
      NOINT=NINT
      WRITE(IOUT,2100)NOINT,NTY,NRTS,NRTM,NSN,NMN
      K10=1
      K11=K10+4*NRTS
      K12=K11+4*NRTM
      K13=K12+NSN
      K14=K13+NMN
      KFI=K14+NSN
      J10=1
      J11=J10+1
      J12=J11+NPARIR
      JFI=J12+2*NSN
C
      IF(NTY == 1)THEN
        K16=KFI
        K21=K16+NSN
        K23=K21+1+NMN
        J20=JFI
        L17=1
        L20=L17+NMN
        L22=L20+1+NSN
        !must be flushed to 0 (in old code INBUF and BUFIN
        !flushed between 2 domain decomposition (otherwise ININT0 subroutine does not store the expected segments)
         INTBUF_TAB%NRT(1:NMT) = 0
         CALL ININT0(X,INTBUF_TAB%IRECTM,INTBUF_TAB%NSEGM,INTBUF_TAB%NRT,INTBUF_TAB%MSR,
     1           INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,NSN,NMN,NRTM,INTBUF_TAB%S_IRECTM, INTBUF_TAB%S_NRT)
         CALL I1CHK2(X,INTBUF_TAB%IRECTS,IXQ,NRTS, NINT,
     1         NSN,INTBUF_TAB%NSV,NOINT,ID,TITR)
         CALL I1CHK2(X,INTBUF_TAB%IRECTM,IXQ,NRTM,-NINT,
     1         NMN,INTBUF_TAB%MSR,NOINT,ID,TITR)
         CALL INVOI2(X,INTBUF_TAB%IRECTM,INTBUF_TAB%NRT,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     1   INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,INTBUF_TAB%NSEGM,NSN,NRTM)
         WRITE(IOUT,2200)
         CALL I1TID2(X,INTBUF_TAB%IRECTM,INTBUF_TAB%CSTS,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     1       INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,NSN,ITAB ,ID,TITR)
          CALL I1BCS_CHECK(ICODE, SICODE, NSN, INTBUF_TAB%NSV, SITAB, ITAB, NOINT, TITR, NTY)

      ELSEIF(NTY == 2)THEN
        J21=JFI
        J22=J21+3*MAX0(NSN,NMN)
        L16=1
        L17=L16+NSN
        L20=L17+NMN
        L21=L20+1+NSN
        L22=L21+1+NMN
        L23=L22+NST
        K48 = KFI
         CALL ININT0(X,INTBUF_TAB%IRECTM,INSCR(L21),INSCR(L23),INTBUF_TAB%MSR,
     1           INTBUF_TAB%NSV,INSCR(L16),NSN,NMN,NRTM,INTBUF_TAB%S_IRECTM,  SINSCR-L23+1)
         CALL I1CHK2(X,INTBUF_TAB%IRECTS,IXQ,NRTS, NINT,
     1         NSN,INTBUF_TAB%NSV,NOINT,ID,TITR)
         CALL I1CHK2(X,INTBUF_TAB%IRECTM,IXQ,NRTM,-NINT,
     1         NMN,INTBUF_TAB%MSR,NOINT,ID,TITR)

         CALL INVOI2(X,INTBUF_TAB%IRECTM,INSCR(L23),INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     1   INSCR(L16),INTBUF_TAB%IRTLM,INSCR(L21),NSN,NRTM)
         WRITE(IOUT,2200)
         CALL I1TID2(X,INTBUF_TAB%IRECTM,INTBUF_TAB%CSTS,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     1       INSCR(L16),INTBUF_TAB%IRTLM,NSN,ITAB ,ID,TITR)
C Projection on edges is used only for the distribution of masses and inertia to avoid negative masses / inertia on MAIN nodes
         DO I=1,NSN
           INTBUF_TAB%CSTS_BIS(2*(I-1)+1)=MIN(ONE,MAX(-1*ONE,INTBUF_TAB%CSTS(2*(I-1)+1)))
           INTBUF_TAB%CSTS_BIS(2*(I-1)+2)=INTBUF_TAB%CSTS(2*(I-1)+2)
         ENDDO
C selecting relevant main nodes and recompating interface buffer
         CALL I2MAIN(INTBUF_TAB%NSV,INTBUF_TAB%MSR,INTBUF_TAB%IRECTM,IPARI,
     .           MWA,MWA(NUMNOD+1),INTBUF_TAB)

      ELSEIF(NTY == 3)THEN
        K15=KFI
        K16=K15+NMN
        K17=K16+NSN
        K18=K17+NMN
        K19=K18+NSN
        K20=K19+NMN
        K21=K20+1+NSN
        K22=K21+1+NMN
        K23=K22+NST
        J13=JFI
        J14=J13+2*NMN
        J15=J14+NSN
        J16=J15+NMN
        J17=J16+NRTS
        J18=J17+NRTM
        J19=J18+3*NSN

        !must be flushed to 0 (in old code INBUF and BUFIN
        !flushed between 2 domain decomposition
        INTBUF_TAB%LNSV(1:NST) = 0
        INTBUF_TAB%LMSR(1:NMT) = 0
        INTBUF_TAB%STFNS(1:NSN) = 0
        INTBUF_TAB%STFNM(1:NMN) = 0

         CALL ININT0(X,INTBUF_TAB%IRECTS,INTBUF_TAB%NSEGS,INTBUF_TAB%LNSV,INTBUF_TAB%NSV,
     1           INTBUF_TAB%MSR,INTBUF_TAB%ILOCM,NMN,NSN,NRTS,INTBUF_TAB%S_IRECTS,INTBUF_TAB%S_LNSV)
         CALL ININT0(X,INTBUF_TAB%IRECTM,INTBUF_TAB%NSEGM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,
     1           INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,NSN,NMN,NRTM,INTBUF_TAB%S_IRECTM,INTBUF_TAB%S_LMSR)
         CALL I3STI2(
     1   X   ,INTBUF_TAB%IRECTS,INTBUF_TAB%STFS,IXQ    ,PM       ,
     2   NRTS   ,INTBUF_TAB%STFNS,INTBUF_TAB%NSEGS,INTBUF_TAB%LNSV,NINT      ,
     3   NSN   ,INTBUF_TAB%NSV,INTBUF_TAB%STFAC   ,NOINT         ,IPM       ,
     4   ID        ,TITR          ,INTBUF_TAB%AREAS   ,KNOD2ELQ      ,NOD2ELQ   ,
     5   NTY       ,IBIDON        ,IBIDON             ,SEGQUADFR    )
         CALL I3STI2(
     1   X   ,INTBUF_TAB%IRECTM,INTBUF_TAB%STFM,IXQ    ,PM       ,
     2   NRTM   ,INTBUF_TAB%STFNM,INTBUF_TAB%NSEGM,INTBUF_TAB%LMSR,-NINT      ,
     3   NMN   ,INTBUF_TAB%MSR,INTBUF_TAB%STFAC   ,NOINT         ,IPM        ,
     4   ID        ,TITR          ,INTBUF_TAB%AREAM   ,KNOD2ELQ      ,NOD2ELQ    ,
     5   NTY       ,IBIDON        ,IBIDON             ,SEGQUADFR    )

         CALL INVOI2(X,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     1   INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,INTBUF_TAB%NSEGM,NSN,NRTM)
         CALL INVOI2(X,INTBUF_TAB%IRECTS,INTBUF_TAB%LNSV,INTBUF_TAB%NSV,INTBUF_TAB%MSR,
     1   INTBUF_TAB%ILOCM,INTBUF_TAB%IRTLS,INTBUF_TAB%NSEGS,NMN,NRTS)
         WRITE(IOUT,2200)
         CALL I3PEN2
     1     (X      ,INTBUF_TAB%IRECTM  ,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,
     2      INTBUF_TAB%IRTLM,NSN   ,INTBUF_TAB%CSTS,INTBUF_TAB%IRTLOM,INTBUF_TAB%FRICOS,
     3      INTBUF_TAB%VARIABLES(1),INTBUF_TAB%VARIABLES(2),IWPENE,ITAB     ,ID,TITR)
         WRITE(IOUT,2300)
         CALL I3PEN2
     1     (X      ,INTBUF_TAB%IRECTS  ,INTBUF_TAB%NSV,INTBUF_TAB%MSR,INTBUF_TAB%ILOCM,
     2      INTBUF_TAB%IRTLS,NMN   ,INTBUF_TAB%CSTM,INTBUF_TAB%IRTLOS,INTBUF_TAB%FRICOM,
     3      INTBUF_TAB%VARIABLES(1),INTBUF_TAB%VARIABLES(2),IWPENE,ITAB    ,ID,TITR)

      ELSEIF(NTY == 5)THEN
        K15=KFI
        K16=K15+NMN
        K17=K16+NSN
        K18=K17+NMN
        K19=K18+NSN
        K20=K19+NMN
        K21=K20+1+NSN
        K22=K21+1+NMN
        K23=K22+NST
        J13=JFI
        J14=J13+2*NMN
        J15=J14+NSN
        J16=J15+NMN
        J17=J16+NRTS
        J18=J17+NRTM
        J19=J18+3*NSN

        !must be flushed to 0 (in old code INBUF and BUFIN
        !flushed between 2 domain decomposition
        INTBUF_TAB%LNSV(1:NST) = 0
        INTBUF_TAB%LMSR(1:NMT) = 0
        INTBUF_TAB%STFNM(1:NMN) = 0

         CALL ININT0(X,INTBUF_TAB%IRECTM,INTBUF_TAB%NSEGM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,
     1           INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,NSN,NMN,NRTM,INTBUF_TAB%S_IRECTM, INTBUF_TAB%S_LMSR)
         CALL I3STI2(
     1   X   ,INTBUF_TAB%IRECTM,INTBUF_TAB%STFM,IXQ    ,PM       ,
     2   NRTM   ,INTBUF_TAB%STFNM,INTBUF_TAB%NSEGM,INTBUF_TAB%LMSR,-NINT      ,
     3   NMN   ,INTBUF_TAB%MSR,INTBUF_TAB%STFAC  ,NOINT          ,IPM        ,
     4   ID        ,TITR          ,INTBUF_TAB%AREAS  ,KNOD2ELQ       ,NOD2ELQ    ,
     5   NTY       ,NSN           ,INTBUF_TAB%NSV     ,SEGQUADFR     )
         CALL INVOI2(X,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     1   INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,INTBUF_TAB%NSEGM,NSN,NRTM)
         WRITE(IOUT,2200)
         CALL I3PEN2
     1     (X      ,INTBUF_TAB%IRECTM  ,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,
     2      INTBUF_TAB%IRTLM,NSN   ,INTBUF_TAB%CSTS,INTBUF_TAB%IRTLOM,INTBUF_TAB%FRICOS,
     3      INTBUF_TAB%VARIABLES(1),INTBUF_TAB%VARIABLES(2),IWPENE,ITAB     ,ID,TITR)

      ELSEIF(NTY == 9)THEN
        K15=KFI
        K16=K15+NMN
        K17=K16+NSN
        K18=K17+NMN
        K19=K18+NSN
        K20=K19+NMN
        K21=K20+1+NSN
        K22=K21+1+NMN
        K23=K22+NST
        K24=K23+NMT
        K25=K24+NRTS
        J13=JFI
        J14=J13+2*NMN
        J15=J14+NSN
        J16=J15+NMN
        J17=J16+NRTS
        J18=J17+NRTM
        J19=J18+3*NSN

        !must be flushed to 0 (in old code INBUF and BUFIN
        !flushed between 2 domain decomposition
        INTBUF_TAB%LNSV(1:NST) = 0
        INTBUF_TAB%LMSR(1:NMT) = 0
        INTBUF_TAB%STFNS(1:NSN)= 0
        INTBUF_TAB%STFNM(1:NMN)= 0

         CALL ININT0(X,INTBUF_TAB%IRECTS,INTBUF_TAB%NSEGS,INTBUF_TAB%LNSV,INTBUF_TAB%NSV,
     1                 INTBUF_TAB%MSR,INTBUF_TAB%ILOCM,NMN,NSN,NRTS,INTBUF_TAB%S_IRECTS ,INTBUF_TAB%S_LNSV)
         CALL ININT0(X,INTBUF_TAB%IRECTM,INTBUF_TAB%NSEGM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,
     1                 INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,NSN,NMN,NRTM,INTBUF_TAB%S_IRECTM ,INTBUF_TAB%S_LMSR)
         CALL I9STI2(
     1   X         ,INTBUF_TAB%IRECTS, IXQ       ,
     2   NRTS      ,NINT      ,
     3   NSN       ,INTBUF_TAB%NSV, NOINT ,INTBUF_TAB%IELES,
     4   ID        ,TITR)
         CALL I9STI2(
     1   X         ,INTBUF_TAB%IRECTS, IXQ ,
     2   NRTM      ,-NINT      ,
     3   NMN       ,INTBUF_TAB%MSR, NOINT ,INTBUF_TAB%IELEM,
     4   ID        ,TITR)
         CALL INVOI2(X,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     1   INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,INTBUF_TAB%NSEGM,NSN,NRTM)
         CALL INVOI2(X,INTBUF_TAB%IRECTS,INTBUF_TAB%LNSV,INTBUF_TAB%NSV,INTBUF_TAB%MSR,
     1   INTBUF_TAB%ILOCM,INTBUF_TAB%IRTLS,INTBUF_TAB%NSEGS,NMN,NRTS)
         IF(NMN>0)THEN
           WRITE(IOUT,2200)
           CALL I3PEN2
     1     (X         ,INTBUF_TAB%IRECTM  ,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,
     2      INTBUF_TAB%IRTLM,NSN         ,INTBUF_TAB%CSTS,INTBUF_TAB%IRTLOM,INTBUF_TAB%FRICOS,
     3      INTBUF_TAB%VARIABLES(1),INTBUF_TAB%VARIABLES(2),IWPENE,ITAB      ,ID,TITR)
           WRITE(IOUT,2300)
           CALL I3PEN2
     1     (X         ,INTBUF_TAB%IRECTS  ,INTBUF_TAB%NSV,INTBUF_TAB%MSR,INTBUF_TAB%ILOCM,
     2      INTBUF_TAB%IRTLS,NMN         ,INTBUF_TAB%CSTM,INTBUF_TAB%IRTLOS,INTBUF_TAB%FRICOM,
     3      INTBUF_TAB%VARIABLES(1),INTBUF_TAB%VARIABLES(2),IWPENE,ITAB      ,ID,TITR)
         ENDIF
         CALL I9BCS_CHECK(ICODE, SICODE, NSN, INTBUF_TAB%NSV, SITAB, ITAB, NOINT, TITR, NTY ,
     2                    INTBUF_TAB%S_ILOCS, INTBUF_TAB%ILOCS  )

       ENDIF !NTY

      IF(IWPENE/=0) THEN
         CALL ANCMSG(MSGID=342,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_1,I1=ID,C1=TITR,I2=IWPENE)
      ENDIF
C
      RETURN

 2100 FORMAT(//
     .      ,5X,'INTERFACE NUMBER. . . . . . . . . . . . . .',I8/
     .      ,5X,'SLIDE LINE TYPE . . . . . . . . . . . . . .',I5/
     .      ,5X,'NUMBER OF SECONDARY  SEGMENTS . . . . . . .',I5/
     .      ,5X,'NUMBER OF MAIN SEGMENTS . . . . . . . . . .',I5/
     .      ,5X,'NUMBER OF SECONDARY  NODES. . . . . . . . .',I5/
     .      ,5X,'NUMBER OF MAIN NODES. . . . . . . . . . . .',I5/)
 2200 FORMAT(//' SECONDARY   NEAREST   NEAREST      MAIN           S '
     .       / ' NODE        MAIN      SEGMENT      NODES '     )
 2300 FORMAT(//' MAIN        NEAREST   NEAREST      SECONDARY      S '
     .       / ' NODE        SECONDARY SEGMENT      NODES '     )

C-----------------------------------------------
      END
