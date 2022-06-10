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
C=======================================================================
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C
C     INTER2D1.FOR
C
C     V16 DEC88 : ALL ROUTINES MODIFIED , INORI2 ADDED
C=======================================================================
Chd|====================================================================
Chd|  ININT2                        source/interfaces/inter2d1/inint2.F
Chd|-- called by -----------
Chd|        ININTR                        source/interfaces/interf1/inintr.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        I1CHK2                        source/interfaces/inter2d1/i1chk2.F
Chd|        I1TID2                        source/interfaces/inter2d1/i1tid2.F
Chd|        I2MAIN                      source/interfaces/interf1/i2main.F
Chd|        I3PEN2                        source/interfaces/inter2d1/i3pen2.F
Chd|        I3STI2                        source/interfaces/inter2d1/i3sti2.F
Chd|        I9STI2                        source/interfaces/int09/i9sti2.F
Chd|        ININT0                        source/interfaces/interf1/inint0.F
Chd|        INVOI2                        source/interfaces/inter2d1/invoi2.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ININT2(INTBUF_TAB ,INSCR    ,X         ,IXQ    ,
     .                  PM         ,GEO      ,IPARI     ,NINT   ,ITAB     ,
     .                  ITABM1     ,NUMNOD   ,IKINE     ,MWA    ,IPM      ,
     .                  ID         ,TITR     ,KNOD2ELQ ,NOD2ELQ ,SEGQUADFR)
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NINT, NUMNOD
      INTEGER INSCR(*), IXQ(*), IPARI(*), ITAB(*),
     .        ITABM1(*), IKINE(*), MWA(*),IPM(NPROPMI,*),
     .        KNOD2ELQ(*),NOD2ELQ(*),SEGQUADFR(2,*)
C     REAL
      my_real
     .   X(*), PM(*), GEO(*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NRTS, NRTM, NSN, NMN, NTY, NST, NMT, NOINT, K10, K11, K12,
     .   K13, K14, KFI, J10, J11, J12, JFI, K16, K21, K23, J20, L17,
     .   L20, L22, J21, J22, L16, L21, L23, K15, K17, K18, K19, K20,
     .   K22, J13, J14, J15, J16, J17, J18, J19, IWPENE, K24, K25,K48,
     .   IBIDON
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IWPENE = 0
      NRTS  =IPARI(3)
      NRTM  =IPARI(4)
      NSN   =IPARI(5)
      NMN   =IPARI(6)
      NTY   =IPARI(7)
      NST   =IPARI(8)
      NMT   =IPARI(9)
      NOINT =IPARI(15)
C
      IF(INVERS.LT.18) NOINT=NINT
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
      GOTO(10,20,30,40,50,100,100,100,90)NTY
 10   K16=KFI
      K21=K16+NSN
      K23=K21+1+NMN
      J20=JFI
      L17=1
      L20=L17+NMN
      L22=L20+1+NSN
C
       CALL ININT0(X,INTBUF_TAB%IRECTM,INTBUF_TAB%NSEGM,INTBUF_TAB%NRT,INTBUF_TAB%MSR,
     1  	     INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,NSN,NMN,NRTM)
       CALL I1CHK2(X,INTBUF_TAB%IRECTS,IXQ,NRTS, NINT,
     1  	   NSN,INTBUF_TAB%NSV,NOINT,ID,TITR)
       CALL I1CHK2(X,INTBUF_TAB%IRECTM,IXQ,NRTM,-NINT,
     1  	   NMN,INTBUF_TAB%MSR,NOINT,ID,TITR)
       CALL INVOI2(X,INTBUF_TAB%IRECTM,INTBUF_TAB%NRT,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     1 INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,INTBUF_TAB%NSEGM,NSN,NRTM)
       WRITE(IOUT,2200)
       CALL I1TID2(X,INTBUF_TAB%IRECTM,INTBUF_TAB%CSTS,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     1     INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,NSN,ITAB ,ID,TITR)
       GOTO 100
C
 20   J21=JFI
      J22=J21+3*MAX0(NSN,NMN)
      L16=1
      L17=L16+NSN
      L20=L17+NMN
      L21=L20+1+NSN
      L22=L21+1+NMN
      L23=L22+NST
      K48 = KFI
       CALL ININT0(X,INTBUF_TAB%IRECTM,INSCR(L21),INSCR(L23),INTBUF_TAB%MSR,
     1  	     INTBUF_TAB%NSV,INSCR(L16),NSN,NMN,NRTM)
       CALL I1CHK2(X,INTBUF_TAB%IRECTS,IXQ,NRTS, NINT,
     1  	   NSN,INTBUF_TAB%NSV,NOINT,ID,TITR)
       CALL I1CHK2(X,INTBUF_TAB%IRECTM,IXQ,NRTM,-NINT,
     1  	   NMN,INTBUF_TAB%MSR,NOINT,ID,TITR)

       CALL INVOI2(X,INTBUF_TAB%IRECTM,INSCR(L23),INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     1 INSCR(L16),INTBUF_TAB%IRTLM,INSCR(L21),NSN,NRTM)
       WRITE(IOUT,2200)
       CALL I1TID2(X,INTBUF_TAB%IRECTM,INTBUF_TAB%CSTS,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     1     INSCR(L16),INTBUF_TAB%IRTLM,NSN,ITAB ,ID,TITR)
C selection main nodes utiles et recompactage buffer interface
       CALL I2MAIN(INTBUF_TAB%NSV,INTBUF_TAB%MSR,INTBUF_TAB%IRECTM,IPARI,
     .  	     MWA,MWA(NUMNOD+1),INTBUF_TAB)
       GOTO 100
C
 30   K15=KFI
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
     1  	     INTBUF_TAB%MSR,INTBUF_TAB%ILOCM,NMN,NSN,NRTS)
       CALL ININT0(X,INTBUF_TAB%IRECTM,INTBUF_TAB%NSEGM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,
     1  	     INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,NSN,NMN,NRTM)
       CALL I3STI2(
     1 X	 ,INTBUF_TAB%IRECTS,INTBUF_TAB%STFS,IXQ	  ,PM	     ,
     2 NRTS	 ,INTBUF_TAB%STFNS,INTBUF_TAB%NSEGS,INTBUF_TAB%LNSV,NINT      ,
     3 NSN	 ,INTBUF_TAB%NSV,INTBUF_TAB%STFAC   ,NOINT         ,IPM       ,
     4 ID        ,TITR          ,INTBUF_TAB%AREAS   ,KNOD2ELQ      ,NOD2ELQ   ,
     5 NTY       ,IBIDON        ,IBIDON             ,SEGQUADFR    )
       CALL I3STI2(
     1 X	 ,INTBUF_TAB%IRECTM,INTBUF_TAB%STFM,IXQ	  ,PM	     ,
     2 NRTM	 ,INTBUF_TAB%STFNM,INTBUF_TAB%NSEGM,INTBUF_TAB%LMSR,-NINT      ,
     3 NMN	 ,INTBUF_TAB%MSR,INTBUF_TAB%STFAC   ,NOINT         ,IPM        ,
     4 ID        ,TITR          ,INTBUF_TAB%AREAM   ,KNOD2ELQ      ,NOD2ELQ    ,
     5 NTY       ,IBIDON        ,IBIDON             ,SEGQUADFR    )

       CALL INVOI2(X,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     1 INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,INTBUF_TAB%NSEGM,NSN,NRTM)
       CALL INVOI2(X,INTBUF_TAB%IRECTS,INTBUF_TAB%LNSV,INTBUF_TAB%NSV,INTBUF_TAB%MSR,
     1 INTBUF_TAB%ILOCM,INTBUF_TAB%IRTLS,INTBUF_TAB%NSEGS,NMN,NRTS)
       WRITE(IOUT,2200)
       CALL I3PEN2
     1   (X	    ,INTBUF_TAB%IRECTM  ,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,
     2    INTBUF_TAB%IRTLM,NSN	 ,INTBUF_TAB%CSTS,INTBUF_TAB%IRTLOM,INTBUF_TAB%FRICOS,
     3    INTBUF_TAB%VARIABLES(1),INTBUF_TAB%VARIABLES(2),IWPENE,ITAB	   ,NINT,ID,TITR)
       WRITE(IOUT,2300)
       CALL I3PEN2
     1   (X	    ,INTBUF_TAB%IRECTS  ,INTBUF_TAB%NSV,INTBUF_TAB%MSR,INTBUF_TAB%ILOCM,
     2    INTBUF_TAB%IRTLS,NMN	 ,INTBUF_TAB%CSTM,INTBUF_TAB%IRTLOS,INTBUF_TAB%FRICOM,
     3    INTBUF_TAB%VARIABLES(1),INTBUF_TAB%VARIABLES(2),IWPENE,ITAB	  ,NINT,ID,TITR)
       GO TO 100
   40 CONTINUE
       GO TO 100
C
 50   K15=KFI
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
     1  	     INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,NSN,NMN,NRTM)
       CALL I3STI2(
     1 X	 ,INTBUF_TAB%IRECTM,INTBUF_TAB%STFM,IXQ	  ,PM	     ,
     2 NRTM	 ,INTBUF_TAB%STFNM,INTBUF_TAB%NSEGM,INTBUF_TAB%LMSR,-NINT      ,
     3 NMN	 ,INTBUF_TAB%MSR,INTBUF_TAB%STFAC  ,NOINT          ,IPM        ,
     4 ID        ,TITR          ,INTBUF_TAB%AREAS  ,KNOD2ELQ       ,NOD2ELQ    ,
     5 NTY       ,NSN           ,INTBUF_TAB%NSV     ,SEGQUADFR     )
       CALL INVOI2(X,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     1 INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,INTBUF_TAB%NSEGM,NSN,NRTM)
       WRITE(IOUT,2200)
       CALL I3PEN2
     1   (X	    ,INTBUF_TAB%IRECTM  ,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,
     2    INTBUF_TAB%IRTLM,NSN	 ,INTBUF_TAB%CSTS,INTBUF_TAB%IRTLOM,INTBUF_TAB%FRICOS,
     3    INTBUF_TAB%VARIABLES(1),INTBUF_TAB%VARIABLES(2),IWPENE,ITAB	   ,NINT,ID,TITR)
       GO TO 100
 90   K15=KFI
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
     1               INTBUF_TAB%MSR,INTBUF_TAB%ILOCM,NMN,NSN,NRTS)
       CALL ININT0(X,INTBUF_TAB%IRECTM,INTBUF_TAB%NSEGM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,
     1               INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,NSN,NMN,NRTM)
       CALL I9STI2(
     1 X         ,INTBUF_TAB%IRECTS,INTBUF_TAB%STFS,IXQ       ,PM        ,
     2 NRTS      ,INTBUF_TAB%STFNS,INTBUF_TAB%NSEGS,INTBUF_TAB%LNSV,NINT      ,
     3 NSN       ,INTBUF_TAB%NSV,INTBUF_TAB%STFAC,NOINT     ,INTBUF_TAB%IELES,
     4 ID,TITR)
       CALL I9STI2(
     1 X         ,INTBUF_TAB%IRECTM,INTBUF_TAB%STFM,IXQ       ,PM        ,
     2 NRTM      ,INTBUF_TAB%STFNM,INTBUF_TAB%NSEGM,INTBUF_TAB%LMSR,-NINT      ,
     3 NMN       ,INTBUF_TAB%MSR,INTBUF_TAB%STFAC,NOINT     ,INTBUF_TAB%IELEM,
     4 ID,TITR)
       CALL INVOI2(X,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     1 INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,INTBUF_TAB%NSEGM,NSN,NRTM)
       CALL INVOI2(X,INTBUF_TAB%IRECTS,INTBUF_TAB%LNSV,INTBUF_TAB%NSV,INTBUF_TAB%MSR,
     1 INTBUF_TAB%ILOCM,INTBUF_TAB%IRTLS,INTBUF_TAB%NSEGS,NMN,NRTS)
       IF(NMN.GT.0)THEN
         WRITE(IOUT,2200)
         CALL I3PEN2
     1   (X         ,INTBUF_TAB%IRECTM  ,INTBUF_TAB%MSR,INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,
     2    INTBUF_TAB%IRTLM,NSN         ,INTBUF_TAB%CSTS,INTBUF_TAB%IRTLOM,INTBUF_TAB%FRICOS,
     3    INTBUF_TAB%VARIABLES(1),INTBUF_TAB%VARIABLES(2),IWPENE,ITAB      ,NINT,ID,TITR)
         WRITE(IOUT,2300)
         CALL I3PEN2
     1   (X         ,INTBUF_TAB%IRECTS  ,INTBUF_TAB%NSV,INTBUF_TAB%MSR,INTBUF_TAB%ILOCM,
     2    INTBUF_TAB%IRTLS,NMN         ,INTBUF_TAB%CSTM,INTBUF_TAB%IRTLOS,INTBUF_TAB%FRICOM,
     3    INTBUF_TAB%VARIABLES(1),INTBUF_TAB%VARIABLES(2),IWPENE,ITAB      ,NINT,ID,TITR)
       ENDIF
       GO TO 100
  100 CONTINUE
C
      IF(IWPENE.NE.0) THEN
         CALL ANCMSG(MSGID=342,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=ID,
     .               C1=TITR,
     .               I2=IWPENE)
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
 2200 FORMAT(//' SECONDARY   NEAREST  NEAREST  MAIN   S '
     .       / ' NODE    MAIN   SEGMENT  NODES '     )
 2300 FORMAT(//' MAIN  NEAREST  NEAREST  SECONDARY    S '
     .       / ' NODE    SECONDARY    SEGMENT  NODES '     )
      END
