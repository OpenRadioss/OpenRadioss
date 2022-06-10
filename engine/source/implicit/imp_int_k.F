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
Chd|  IMP_INT_K                     source/implicit/imp_int_k.F   
Chd|-- called by -----------
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        I10KE3                        source/interfaces/int10/i10ke3.F
Chd|        I11KE3                        source/interfaces/int11/i11ke3.F
Chd|        I24KE3                        source/interfaces/int24/i24ke3.F
Chd|        I5KE3                         source/interfaces/inter3d/i5ke3.F
Chd|        I7KE3                         source/interfaces/int07/i7ke3.F
Chd|        UPD_INT_K                     source/implicit/upd_glob_k.F  
Chd|        IMP_INTBUF                    share/modules/imp_mod_def.F   
Chd|        IMP_INTTD                     share/modules/imp_mod_def.F   
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IMP_INT_K( A     ,V         ,
     1                      ICODT ,ICODR ,ISKEW  ,IBFV  ,NPC    ,
     2                      TF    ,VEL   ,SENSOR ,LSENSOR,XFRAME ,
     3                      RBY   ,X     ,SKEW   ,LPBY   ,NPBY  ,
     4                      ITAB  ,WEIGHT,MS     ,IN     ,NRBYAC,
     5                      IRBYAC,NSS   ,ISS    ,IPARI  ,INTBUF_TAB,
     6                      NINT2 ,IINT2  ,IAINT2 ,NSS2  ,
     7                      ISS2  ,NDDLI ,NNZI   ,IADI  ,JDII   ,
     8                      DIAG_I ,LT_I  ,IDDLI ,NDDL  ,IADK   ,
     9                      JDIK   ,IKC   ,DIAG_K,LT_K  ,IDDL   ,
     A                      NUM_IMP,NS_IMP,NE_IMP,INDEX2,NDOFI  ,
     B                      ITOK   ,UD    ,LB    ,GAPMIN,DIRUL  ,
     C                      NT_RW  ,NUM_IMP1,IRBE3,LRBE3,FRBE3  ,
     D                      NSS3   ,ISS3   ,IRBE2 ,LRBE2,NSB2   ,
     E                      ISB2   ) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTTD
      USE INTBUFDEF_MOD
      USE IMP_INTBUF
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPC(*),IBFV(NIFV,*),LSENSOR(NSENSI,*),DIRUL(*),
     .        ICODT(*),ICODR(*),ISKEW(*),ITOK(*),NDDL,NT_RW
      INTEGER WEIGHT(*),LPBY(*),NPBY(NNPBY,*),ITAB(*),
     .        IPARI(NPARI,*), NRBYAC,IRBYAC(*),
     .        IDDL(*),IKC(*),NSS(*),ISS(*),NSS2(*),ISS2(*),
     .        IADK(*),JDIK(*),NDDLI,NNZI,IADI(*),JDII(*),
     .        IDDLI(*),NDOFI(*),NINT2 ,IINT2(*),IAINT2(*)
      INTEGER NUM_IMP(*),NS_IMP(*),NE_IMP(*),INDEX2(*),NUM_IMP1(*)
      INTEGER IRBE3(NRBE3L,*),LRBE3(*),NSS3(*),ISS3(*),
     .        IRBE2(*),LRBE2(*),NSB2(*),ISB2(*)
      my_real
     .  A(3,*),V(3,*),RBY(NRBY,*),X(3,*) ,SKEW(*),IN(*),MS(*)
      my_real
     .  TF(*), VEL(LFXVELR,*),SENSOR(NSENSR,*),DIAG_K(*),LT_K(*),
     .  DIAG_I(*),LT_I(*),LB(*),UD(3,*),GAPMIN,XFRAME(NXFRAME,*),
     .  FRBE3(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N, IAD,NTY,I_INT7,NUM_IMP0,
     .        LREM(NINTER), ISENS
      my_real TS
C     REAL
C
      IF (NT_IMP1.GT.0) THEN
       IAD=1
       GAPMIN=EP20
       I_INT7 = IMP_INT7
       IMP_INT7 = 3
C-----------int5 first-------------       
       DO N=1,NINTER
        NTY   =IPARI(7,N)
        IF (NUM_IMP(N)==0) CYCLE
        IF(NTY.EQ.5) THEN
C
          ISENS = IPARI(64,N)
          IF(ISENS.NE.0)  THEN  ! SENSOR
             TS = SENSOR(2,ISENS)
          ELSE
             TS = TT
          ENDIF
C
          IF(TT.GE.TS) THEN  ! If interface is activated
            CALL I5KE3( A,V     ,MS     ,
     1        IPARI(1,N),INTBUF_TAB(N) ,X  ,
     2        NUM_IMP(N),NS_IMP(IAD),NE_IMP(IAD)  ,
     3        IDDLI     ,DIAG_I ,LT_I  , IADI     ,JDII    )
          ENDIF
         IAD=IAD+NUM_IMP(N)
        ENDIF
       END DO
C       
       DO N=1,NINTER
        NTY   =IPARI(7,N)
	    LREM(N) = 0
        IF(NTY.EQ.7) THEN
C
          ISENS = IPARI(64,N)
          IF(ISENS.NE.0)  THEN  ! SENSOR
             TS = SENSOR(2,ISENS)
          ELSE
             TS = TT
          ENDIF
C
          IF(TT.GE.TS) THEN  ! If interface is activated
            CALL I7KE3( A,V     ,MS     ,
     1        IPARI     ,INTBUF_TAB(N) ,X       ,N  ,
     2        NUM_IMP1(N),NS_IMP1(IAD),NE_IMP1(IAD)   ,IND_IMP1(IAD),
     3        IDDLI     ,DIAG_I ,LT_I  , IADI     ,JDII   ,GAPMIN   ,
     4        LREM(N)   )
          ENDIF
          IAD=IAD+NUM_IMP1(N)
        ELSEIF(NTY.EQ.10)THEN
          CALL I10KE3( A,V     ,MS     ,
     1       IPARI(1,N),INTBUF_TAB(N) ,X       ,N     ,
     2       NUM_IMP1(N),NS_IMP1(IAD),NE_IMP1(IAD)   ,IND_IMP1(IAD),
     3       IDDLI     ,DIAG_I ,LT_I  , IADI     ,JDII   ,GAPMIN   ,
     4       LREM(N)   )
          IAD=IAD+NUM_IMP1(N)
        ELSEIF(NTY.EQ.11)THEN
C
          ISENS = IPARI(64,N)
          IF(ISENS.NE.0)  THEN  ! SENSOR
             TS = SENSOR(2,ISENS)
          ELSE
             TS = TT
          ENDIF
C
          IF(TT.GE.TS) THEN  ! If interface is activated
            CALL I11KE3( A,    V     ,MS     ,
     1        IPARI(1,N),INTBUF_TAB(N) ,X       ,N     ,
     2        NUM_IMP1(N),NS_IMP1(IAD),NE_IMP1(IAD)   ,
     3        IDDLI     ,DIAG_I ,LT_I  , IADI     ,JDII   ,GAPMIN   ,
     4        LREM(N)   ,ITAB   )
          ENDIF
          IAD=IAD+NUM_IMP1(N)
        ELSEIF(NTY.EQ.24) THEN
C
          ISENS = IPARI(64,N)
          IF(ISENS.NE.0)  THEN  ! SENSOR
             TS = SENSOR(2,ISENS)
          ELSE
             TS = TT
          ENDIF
C
          IF(TT.GE.TS) THEN  ! If interface is activated
c            CALL I24KE3( A,V     ,MS     ,
c     1        IPARI     ,INTBUF_TAB(N) ,X       ,N  ,
c     2        NUM_IMP1(N),NS_IMP1(IAD),NE_IMP1(IAD)   ,IND_IMP1(IAD),
c     3        IDDLI     ,DIAG_I ,LT_I  , IADI     ,JDII   ,GAPMIN   ,
c     4        LREM(N)   )
            CALL I24KE3( A,V     ,MS     ,
     1        IPARI     ,INTBUF_TAB(N) ,X       ,N  ,
c     2        NUM_IMP1(N),NS_IMP1(IAD),NE_IMP1(IAD)   ,IND_IMP1(IAD),
     3        IDDLI     ,DIAG_I ,LT_I  , IADI     ,JDII   ,GAPMIN   ,
     4        LREM(N)   ,INTBUF_TAB_IMP(N))
          ENDIF
          IAD=IAD+NUM_IMP1(N) 
        ELSE
        ENDIF
       ENDDO
       IMP_INT7 = I_INT7
       DO N=1,NINTER
        NTY   =IPARI(7,N)
        NUM_IMP0 = NUM_IMP(N)-NUM_IMP1(N)
        IF(NTY.EQ.7) THEN
C
          ISENS = IPARI(64,N)
          IF(ISENS.NE.0)  THEN  ! SENSOR
             TS = SENSOR(2,ISENS)
          ELSE
             TS = TT
          ENDIF
C
          IF(TT.GE.TS) THEN  ! If interface is activated
            CALL I7KE3( A,V     ,MS     ,
     1        IPARI     ,INTBUF_TAB(N) ,X       ,N ,
     2        NUM_IMP0  ,NS_IMP1(IAD),NE_IMP1(IAD)   ,IND_IMP1(IAD),
     3        IDDLI     ,DIAG_I ,LT_I  , IADI     ,JDII   ,GAPMIN  ,
     4        LREM(N)   )
          ENDIF
          IAD=IAD+NUM_IMP0
        ELSEIF(NTY.EQ.10)THEN
          CALL I10KE3( A,V     ,MS     ,
     1       IPARI(1,N),INTBUF_TAB(N) ,X       ,N     ,
     2       NUM_IMP0  ,NS_IMP1(IAD),NE_IMP1(IAD)   ,IND_IMP1(IAD),
     3       IDDLI     ,DIAG_I ,LT_I  , IADI     ,JDII   ,GAPMIN  ,
     4       LREM(N)   )
          IAD=IAD+NUM_IMP0
        ELSEIF(NTY.EQ.11)THEN
C
          ISENS = IPARI(64,N)
          IF(ISENS.NE.0)  THEN  ! SENSOR
             TS = SENSOR(2,ISENS)
          ELSE
             TS = TT
          ENDIF
C
          IF(TT.GE.TS) THEN  ! If interface is activated
            CALL I11KE3( A,    V     ,MS     ,
     1        IPARI(1,N),INTBUF_TAB(N) ,X       ,N  ,
     2        NUM_IMP0  ,NS_IMP1(IAD),NE_IMP1(IAD)   ,
     3        IDDLI     ,DIAG_I ,LT_I  , IADI     ,JDII   ,GAPMIN   ,
     4        LREM(N)   ,ITAB   )
          ENDIF
          IAD=IAD+NUM_IMP0
        ELSEIF(NTY.EQ.24) THEN
C
          ISENS = IPARI(64,N)
          IF(ISENS.NE.0)  THEN  ! SENSOR
             TS = SENSOR(2,ISENS)
          ELSE
             TS = TT
          ENDIF
C
          IF(TT.GE.TS) THEN  ! If interface is activated
            CALL I24KE3( A,V     ,MS     ,
     1        IPARI     ,INTBUF_TAB(N) ,X       ,N  ,
c     2        NUM_IMP1(N),NS_IMP1(IAD),NE_IMP1(IAD)   ,IND_IMP1(IAD),
     3        IDDLI     ,DIAG_I ,LT_I  , IADI     ,JDII   ,GAPMIN   ,
     4        LREM(N)   ,INTBUF_TAB_IMP(N))
          ENDIF
          IAD=IAD+NUM_IMP0
        ELSE
        ENDIF
       ENDDO
      ELSE
C----- normal branche------------
      IAD=1
      GAPMIN=EP20
       DO N=1,NINTER
        NTY   =IPARI(7,N)
        IF (NUM_IMP(N)==0) CYCLE
        IF(NTY.EQ.5) THEN
C
          ISENS = IPARI(64,N)
          IF(ISENS.NE.0)  THEN  ! SENSOR
             TS = SENSOR(2,ISENS)
          ELSE
             TS = TT
          ENDIF
C
          IF(TT.GE.TS) THEN  ! If interface is activated
             CALL I5KE3( A,V     ,MS     ,
     1        IPARI(1,N),INTBUF_TAB(N) ,X       ,
     2        NUM_IMP(N),NS_IMP(IAD),NE_IMP(IAD)  ,
     3        IDDLI     ,DIAG_I ,LT_I  , IADI     ,JDII    )
         ENDIF
         IAD=IAD+NUM_IMP(N)
        ENDIF
       END DO
      DO N=1,NINTER
        NTY   =IPARI(7,N)
	LREM(N) = 0
        IF(NTY.EQ.7) THEN
C
          ISENS = IPARI(64,N)
          IF(ISENS.NE.0)  THEN  ! SENSOR
             TS = SENSOR(2,ISENS)
          ELSE
             TS = TT
          ENDIF
C
          IF(TT.GE.TS) THEN  ! If interface is activated
            CALL I7KE3( A,V     ,MS     ,
     1        IPARI     ,INTBUF_TAB(N) ,X       ,N  ,
     2        NUM_IMP(N),NS_IMP(IAD),NE_IMP(IAD)   ,INDEX2(IAD),
     3        IDDLI     ,DIAG_I ,LT_I  , IADI     ,JDII   ,GAPMIN   ,
     4        LREM(N)   )
          ENDIF
          IAD=IAD+NUM_IMP(N)
        ELSEIF(NTY.EQ.10)THEN
          CALL I10KE3( A,V     ,MS     ,
     1       IPARI(1,N),INTBUF_TAB(N) ,X       ,N     ,
     2       NUM_IMP(N),NS_IMP(IAD),NE_IMP(IAD)   ,INDEX2(IAD),
     3       IDDLI     ,DIAG_I ,LT_I  , IADI     ,JDII   ,GAPMIN    ,
     4       LREM(N)   )
          IAD=IAD+NUM_IMP(N)
        ELSEIF(NTY.EQ.11)THEN
C
          ISENS = IPARI(64,N)
          IF(ISENS.NE.0)  THEN  ! SENSOR
             TS = SENSOR(2,ISENS)
          ELSE
             TS = TT
          ENDIF
C
          IF(TT.GE.TS) THEN  ! If interface is activated
            CALL I11KE3( A,    V     ,MS     ,
     1        IPARI(1,N),INTBUF_TAB(N) ,X       ,N   ,
     2        NUM_IMP(N),NS_IMP(IAD),NE_IMP(IAD)   ,
     3        IDDLI     ,DIAG_I ,LT_I  , IADI     ,JDII   ,GAPMIN    ,
     4        LREM(N)   ,ITAB   )
          ENDIF
          IAD=IAD+NUM_IMP(N)
        ELSEIF(NTY.EQ.24) THEN
C
          ISENS = IPARI(64,N)
          IF(ISENS.NE.0)  THEN  ! SENSOR
             TS = SENSOR(2,ISENS)
          ELSE
             TS = TT
          ENDIF
C
          IF(TT.GE.TS) THEN  ! If interface is activated
            CALL I24KE3( A,V     ,MS     ,
     1        IPARI     ,INTBUF_TAB(N) ,X       ,N     ,
     3        IDDLI     ,DIAG_I ,LT_I  , IADI     ,JDII   ,GAPMIN   ,
     4        LREM(N)   ,INTBUF_TAB_IMP(N))
          ENDIF
          IAD=IAD+NUM_IMP(N)
        ENDIF
      ENDDO
      END IF !(NT_IMP1.GT.0) 
C
        IF(GAPMIN.LT.ZERO)RETURN  
            CALL UPD_INT_K(ICODT ,ICODR ,ISKEW ,IBFV    ,NPC    ,
     1                      TF   ,VEL   ,SENSOR ,LSENSOR,XFRAME ,
     2                      RBY   ,X     ,SKEW   ,LPBY   ,NPBY  ,
     3                      ITAB  ,WEIGHT,MS     ,IN     ,NRBYAC,
     4                      IRBYAC,NSS   ,ISS    ,IPARI  ,INTBUF_TAB,
     5                      NINT2 ,IINT2  ,IAINT2 ,NSS2  ,
     5                      ISS2  ,NDDLI ,NNZI   ,IADI  ,JDII   ,
     6                      DIAG_I ,LT_I  ,IDDLI ,NDDL  ,IADK   ,
     7                      JDIK   ,IKC   ,DIAG_K,LT_K  ,IDDL   ,
     8                      NDOFI  ,ITOK  ,UD    ,LB    ,DIRUL  ,
     9                      NT_RW  ,IRBE3 ,LRBE3 ,FRBE3  ,NSS3  ,
     A                      ISS3   ,IRBE2 ,LRBE2 ,NSB2  ,ISB2   ) 
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_INTDT                     source/implicit/imp_int_k.F   
Chd|-- called by -----------
Chd|        IMP_DTKIN                     source/implicit/imp_int_k.F   
Chd|        IMP_INTTD0                    source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|        I11MAIN_CRIT_TRI              source/interfaces/intsort/i11main_crit_tri.F
Chd|        I7MAIN_CRIT_TRI               source/interfaces/intsort/i7main_crit_tri.F
Chd|        IMP_ICOMCRIT                  source/implicit/imp_int_k.F   
Chd|        SPMD_MIN_S                    source/mpi/implicit/imp_spmd.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IMP_INTDT(
     1       IPARI    ,INTBUF_TAB,X        ,
     2       V        ,VR      ,ISENDTO ,IRECVFROM,
     4       NEWFRONT ,ITASK   ,DTK     ,ITAB     ,
     5       INTLIST  ,NBINTC  ,DT_MIN  ,MS       ,
     6       SENSOR   ,MAXDGAP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), ITAB(*),
     .        NEWFRONT(*),NBINTC,INTLIST(*),
     .        ISENDTO(NINTER+1,*),IRECVFROM(NINTER+1,*),
     .        ITASK
      my_real 
     .   X(3,*), V(3,*),VR(3,*),DTK(*),DT_MIN,MS(*),
     .   SENSOR(NSENSR,*),MAXDGAP(NINTER)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, KK,LL, RETRI, NBLIST, IFQ,
     .        INACTI, NSNROLD, IAD17, IGN, IGE, NME, NMES,
     .        NELTST ,ITYPTST ,I,NTY, ISENS, INTERACT
C     REAL
      my_real
     .        XSLV_L(18,NINTER),XMSR_L(12,NINTER),
     .        VSLV_L(6,NINTER),VMSR_L(6,NINTER),
     .        SIZE_T(NINTER),DTI,FAC, TS
C-------------DT kine par interface--------
C Init variable globale interface
C      DO KK=1,NBINTC
C        N = INTLIST(KK)
      DO N=1,NINTER
        XSLV_L(1,N)= -EP30
        XSLV_L(2,N)= -EP30
        XSLV_L(3,N)= -EP30
        XSLV_L(4,N)= EP30
        XSLV_L(5,N)= EP30
        XSLV_L(6,N)= EP30
        XSLV_L( 7,N)= -EP30
        XSLV_L( 8,N)= -EP30
        XSLV_L( 9,N)= -EP30
        XSLV_L(10,N)= EP30
        XSLV_L(11,N)= EP30
        XSLV_L(12,N)= EP30
        XSLV_L(13,N)= -EP30
        XSLV_L(14,N)= -EP30
        XSLV_L(15,N)= -EP30
        XSLV_L(16,N)= EP30
        XSLV_L(17,N)= EP30
        XSLV_L(18,N)= EP30
	
        XMSR_L(1,N)= -EP30
        XMSR_L(2,N)= -EP30
        XMSR_L(3,N)= -EP30
        XMSR_L(4,N)= EP30
        XMSR_L(5,N)= EP30
        XMSR_L(6,N)= EP30
        XMSR_L( 7,N)= -EP30
        XMSR_L( 8,N)= -EP30
        XMSR_L( 9,N)= -EP30
        XMSR_L(10,N)= EP30
        XMSR_L(11,N)= EP30
        XMSR_L(12,N)= EP30

        VSLV_L(1,N)= -EP30
        VSLV_L(2,N)= -EP30
        VSLV_L(3,N)= -EP30
        VSLV_L(4,N)= EP30
        VSLV_L(5,N)= EP30
        VSLV_L(6,N)= EP30
        VMSR_L(1,N)= -EP30
        VMSR_L(2,N)= -EP30
        VMSR_L(3,N)= -EP30
        VMSR_L(4,N)= EP30
        VMSR_L(5,N)= EP30
        VMSR_L(6,N)= EP30
        SIZE_T(N)=ZERO       
      END DO
      DT_MIN = EP30
C      DO KK=1,NBINTC
C
C        N = INTLIST(KK)
      DO N=1,NINTER
        DTK(N) = EP30
C----------ICONT-----	
        IPARI(29,N) = 0
C
        NTY   =IPARI(7,N)
        IF(NTY.EQ.7.OR.NTY.EQ.10.OR.NTY.EQ.18)THEN
          I7KGLO = 1
C         IPARI(4,N) = NRTM ; IPARI(5,N)=NSN
C
          ISENS = 0
          IF(NTY == 7) ISENS = IPARI(64,N)  
          IF(ISENS.NE.0)  THEN
             TS = SENSOR(2,ISENS)
          ELSE
             TS = TT
          ENDIF
C
          IF(TT.GE.TS) THEN
            CALL I7MAIN_CRIT_TRI(
     1        IPARI ,X     ,N     ,
     2        ITASK ,V            ,XSLV_L       ,XMSR_L,VSLV_L,
     3        VMSR_L,INTBUF_TAB(N)              )
          ENDIF
        ELSEIF(NTY.EQ.11)THEN
          I7KGLO = 1
C
          ISENS = IPARI(64,N)  
          IF(ISENS.NE.0)  THEN
             TS = SENSOR(2,ISENS)
          ELSE
             TS = TT
          ENDIF
C
          IF(TT.GE.TS) THEN
            CALL I11MAIN_CRIT_TRI(
     1       IPARI     ,X      ,N      ,
     2       ITASK     ,V            ,XSLV_L       ,XMSR_L , VSLV_L,
     4       VMSR_L    ,INTBUF_TAB(N) )
          ENDIF
        ENDIF
C       
         CALL IMP_ICOMCRIT(
     1    INTBUF_TAB   ,IPARI   ,NEWFRONT ,ISENDTO  ,
     2    IRECVFROM,DTK(N)  ,ITAB    ,XSLV_L   ,XMSR_L   ,
     3    VSLV_L  ,VMSR_L   ,SIZE_T  ,N        ,SENSOR   ,
     4    INTLIST  ,NBINTC  ,MAXDGAP)
C     
          IF (NSPMD.GT.1)CALL SPMD_MIN_S(DTK(N))
         DT_MIN = MIN(DT_MIN,DTK(N))
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_ICOMCRIT                  source/implicit/imp_int_k.F   
Chd|-- called by -----------
Chd|        IMP_INTDT                     source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|        SPMD_GET_STIF                 source/mpi/interfaces/send_cand.F
Chd|        SPMD_GET_STIF11               source/mpi/interfaces/send_cand.F
Chd|        SPMD_SYNC_MMX                 source/mpi/interfaces/spmd_sync_mmx.F
Chd|        INTAB                         source/implicit/ind_glob_k.F  
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IMP_ICOMCRIT(
     1  INTBUF_TAB,IPARI  ,NEWFRONT,ISENDTO,
     2  IRCVFROM,DT2T   ,ITAB   ,XSLV_L  ,XMSR_L ,
     3  VSLV_L ,VMSR_L  ,SIZE_T ,N       ,SENSOR ,
     4  INTLIST ,NBINTC ,MAXDGAP) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
#include      "scr04_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), NEWFRONT(*), ITAB(*),
     .        ISENDTO(NINTER+1,*) ,IRCVFROM(NINTER+1,*),N,
     .        NBINTC,INTLIST(*)
C     REAL
      my_real
     .   DT2T,XSLV_L(6,*), XMSR_L(6,*), VSLV_L(6,*), 
     .VMSR_L(6,*), SIZE_T(*),SENSOR(NSENSR,*),MAXDGAP(NINTER)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IAD,K,IADD, NBNEW, LISTNEW(1),
     .        INTERACT,ISENS
      my_real
     .        XX,XY,XZ,DIST0,VX,VY,VZ,GAPINF,VV,DTI,
     .        MINBOX,
     .        STARTT, STOPT, TZINF(1), TS,PMAX(NINTER)
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB
      EXTERNAL INTAB
C
C
C Precalcul des interfaces utiles
C
      DO J=1,NINTER
       PMAX=ZERO
      END DO
      NBNEW = 0
        I = N
        NTY= IPARI(7,I)
        IF(NTY.NE.17.AND. INTAB(NBINTC,INTLIST,N))THEN
C
          INTERACT = 0
          ISENS = 0
          IF(NTY == 7.OR.NTY == 11.OR.NTY == 24.OR.NTY == 21.OR.
     .       NTY == 5.OR.NTY == 19 ) ISENS = IPARI(64,I)      
          IF(ISENS.NE.0) THEN             
            TS = SENSOR(2,ISENS)
            IF (TT.GE.TS) INTERACT = 1
          ELSE
            STARTT = INTBUF_TAB(I)%VARIABLES(3)
            STOPT  = INTBUF_TAB(I)%VARIABLES(11)
            IF (STARTT.LE.TT.AND.TT.LE.STOPT) INTERACT = 1
          ENDIF
C
          IF(INTERACT.NE.0) THEN
            NBNEW = NBNEW + 1
            LISTNEW(NBNEW) = N
            TZINF(NBNEW) = INTBUF_TAB(I)%VARIABLES(8)
          ENDIF
         END IF
C
      IF(NSPMD.GT.1)THEN
       CALL SPMD_SYNC_MMX(
     1   ISENDTO,IRCVFROM,NEWFRONT,XSLV_L,XMSR_L,
     2   VSLV_L ,VMSR_L  ,LISTNEW ,NBNEW ,TZINF ,
     3   SIZE_T ,IPARI   ,PMAX    ,MAXDGAP)
      END IF
C
       IF (NBNEW.EQ.0) RETURN
C
        NTY  =IPARI(7,I)
        IF(NTY.NE.17)THEN
          INTBUF_TAB(I)%VARIABLES(8)=TZINF(1)
          IF(NSPMD.GT.1) THEN
          IF (NEWFRONT(I).LT.0)THEN
            IF(NTY.EQ.7.OR.NTY.EQ.10) THEN
              CALL SPMD_GET_STIF(
     1          NEWFRONT(I)  ,INTBUF_TAB(I)%I_STOK(1),
     2          INTBUF_TAB(I)%CAND_N,INTBUF_TAB(I)%CAND_E,
     3          IPARI(5,I),I,ISENDTO,IRCVFROM,INTBUF_TAB(I)%NSV,
     4          ITAB)
            ELSEIF(NTY.EQ.11) THEN
              CALL SPMD_GET_STIF11(
     1          NEWFRONT(I)  ,INTBUF_TAB(I)%I_STOK(1) ,
     2           INTBUF_TAB(I)%CAND_N   , INTBUF_TAB(I)%GAP_S,
     3          IPARI(3,I),I,ISENDTO,IRCVFROM, INTBUF_TAB(I)%IRECTS,
     4          ITAB)
            ENDIF
          ENDIF
          END IF !(NSPMD.GT.1) THEN
C maj dist = tzinf - gap ***: seulement pour etre coherent avec smp 
           INTBUF_TAB(I)%VARIABLES(5) = INTBUF_TAB(I)%VARIABLES(8)-
     -                                INTBUF_TAB(I)%VARIABLES(2)
C calcul du critere de tri DIST0
          XX=MAX(XSLV_L(1,I)-XMSR_L(4,I),XMSR_L(1,I)-XSLV_L(4,I),ZERO)
          XY=MAX(XSLV_L(2,I)-XMSR_L(5,I),XMSR_L(2,I)-XSLV_L(5,I),ZERO)
          XZ=MAX(XSLV_L(3,I)-XMSR_L(6,I),XMSR_L(3,I)-XSLV_L(6,I),ZERO)
          DIST0 = INTBUF_TAB(I)%VARIABLES(5) - SQRT(XX**2+XY**2+XZ**2)
C
C Traitement VMAX
C
          VX=MAX(VSLV_L(1,I)-VMSR_L(4,I),VMSR_L(1,I)-VSLV_L(4,I),ZERO)
          VY=MAX(VSLV_L(2,I)-VMSR_L(5,I),VMSR_L(2,I)-VSLV_L(5,I),ZERO)
          VZ=MAX(VSLV_L(3,I)-VMSR_L(6,I),VMSR_L(3,I)-VSLV_L(6,I),ZERO)
          VV=SQRT(VX**2+VY**2+VZ**2)
          IF (VV.NE.ZERO) THEN
            GAPINF = INTBUF_TAB(I)%VARIABLES(6)
            IF (GAPINF.EQ.ZERO) GAPINF = INTBUF_TAB(I)%VARIABLES(2)
C-------supposee s est encore loin de M ------
	    GAPINF =GAPINF+MAX(ZERO,DIST0)
            DTI = ONEP8*GAPINF/VV
Ctmp            DTI = ZEP9*GAPINF/VV
            IF(DTI.LT.DT2T) DT2T    = DTI            
          ENDIF
          IF(DIST0.LE.ZERO) THEN
            INTBUF_TAB(I)%VARIABLES(5) = -ONE
          ENDIF
        ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_INTTD0                    source/implicit/imp_int_k.F   
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        CP_INTTD                      source/implicit/imp_int_k.F   
Chd|        IMP_I11XV                     source/implicit/imp_int_k.F   
Chd|        IMP_I7XV                      source/implicit/imp_int_k.F   
Chd|        IMP_INTDT                     source/implicit/imp_int_k.F   
Chd|        IMP_TRIPI                     source/implicit/imp_int_k.F   
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE IMP_INTTD0(
     1       IPARI ,INTBUF_TAB     ,X       ,D       ,
     2       MS    ,ITAB   ,IN    ,D_IMP   ,DR_IMP  ,
     3       IMSCH ,I2MSCH ,ISIZXV,ILENXV  ,IGRBRIC ,
     4       ISLEN7,IRLEN7 ,ISLEN11,IRLEN11,ISLEN17 ,
     5       IRLEN17,IRLEN7T,ISLEN7T,IAD_ELEM,FR_ELEM ,
     6       NBINTC,INTLIST,ITASK ,KINET   ,NEWFRONT,
     7       NUM_IMP,NS_IMP,NE_IMP,IND_IMP ,ISENDTO ,
     8       IRECVFROM,WEIGHT ,IXS  ,TEMP    ,
     9       DT2PREV,WA,NUM_IMP1,IRLEN20,ISLEN20,
     A       IRLEN20T,ISLEN20T,IRLEN20E,ISLEN20E,
     B       IKINE,DIAG_SMS,COUNT_REMSLV,COUNT_REMSLVE,
     C       SENSOR,XDP,H3D_DATA,MULTI_FVM,FORNEQS,MAXDGAP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
      USE H3D_MOD
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), ITAB(*),
     .        NEWFRONT(*),NBINTC,INTLIST(*),
     .        ISENDTO(NINTER+1,*),IRECVFROM(NINTER+1,*),
     .        ITASK,IMSCH ,I2MSCH ,ISIZXV,ILENXV,IRLEN20,ISLEN20,
     .        IRLEN20T,ISLEN20T,IRLEN20E,ISLEN20E
      INTEGER ISLEN7,IRLEN7 ,ISLEN11,IRLEN11,ISLEN17,
     .       IRLEN17,IRLEN7T,ISLEN7T,IAD_ELEM(*),FR_ELEM(*) ,
     .       WEIGHT(*),IXS(*)  ,NUM_IMP1(*),
     .       NUM_IMP(*),NS_IMP(*),NE_IMP(*),IND_IMP(*),
     .       KINET(*),IKINE(*),COUNT_REMSLV(*),
     .       COUNT_REMSLVE(*)
     
      DOUBLE PRECISION XDP(3,*)
      
      my_real 
     .   X(3,*), D(3,*),MS(*),WA(*),
     .   DT2PREV, TEMP(*),D_IMP(3,*),DR_IMP(3,*),IN(*),DIAG_SMS(*),
     .   SENSOR(NSENSR,*),FORNEQS(3,*),MAXDGAP(NINTER)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
!
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, KK,LL, RETRI, NBLIST, IFQ,
     .        INACTI, NSNROLD, IAD17, IGN, IGE, NME, NMES,
     .        NB ,ILIST(1) ,I,NTY,KD(50), KFI,IAD,NSN,NMN,
     .        JFI,JD(50),IDNS,IDNM,IDSTIF,NTHR_CP,ISENS ,
     .        INTERACT
C     REAL
      my_real
     .        DTK(NINTER),V(3,NUMNOD),VR(3,NUMNOD),
     .        X_TMP(3,NUMNOD),DTI,FAC,FAC1,DT_MIN,DX,DY,DZ,
     .        STARTT,STOPT, TS
C-------------before inttri------
       NT_IMP1 = 0  
       DO N = 1,NINTER
        NUM_IMP1(N)=0
       ENDDO
C----deactive cette fonction.     
       IF (ITTOFF.GT.0) RETURN
       NTHR_CP = NTHREAD
       IF (NTHREAD.GT.1) NTHREAD = 1
       DTI = ONE/DT2
        DO I=1,NUMNOD
          V(1,I)=D_IMP(1,I)*DTI
          V(2,I)=D_IMP(2,I)*DTI
          V(3,I)=D_IMP(3,I)*DTI
        ENDDO
        IF (IRODDL.NE.0) THEN
         DO I=1,NUMNOD
           VR(1,I)=DR_IMP(1,I)*DTI
           VR(2,I)=DR_IMP(2,I)*DTI
           VR(3,I)=DR_IMP(3,I)*DTI
         ENDDO
        ENDIF 
       CALL IMP_INTDT(
     1       IPARI    ,INTBUF_TAB       ,X        ,
     2       V        ,VR      ,ISENDTO ,IRECVFROM,
     4       NEWFRONT ,ITASK   ,DTK     ,ITAB     ,
     5       INTLIST  ,NBINTC  ,DT_MIN  ,MS       ,
     6       SENSOR   ,MAXDGAP)
       IF (DT_MIN .GE. DT2) GOTO 1000
       DO N = 1,NINTER
        NUM_IMP(N) = 0
       END DO
C-------------dans inttri------
c      NI18 = 0
c      LI18 = 1
c      IAD17 = 1
C
      IAD=1
      NB = 1
C
C      DO KK=1,NBINTC
C        N   = INTLIST(KK)
      DO N=1,NINTER
        NTY = IPARI(7,N)
        IF (NTY.NE.7.AND.NTY.NE.10.AND.NTY.NE.11) GOTO 999
        IF (DTK(N).GE.DT2) GOTO 999
        NSN   =IPARI(5,N)
        NMN   =IPARI(6,N)
C
        INTERACT = 0
        ISENS = 0
        IF(NTY == 7.OR.NTY == 11) ISENS = IPARI(64,N)      
        IF(ISENS.NE.0) THEN             !CMAAAAA
           TS = SENSOR(2,ISENS)
           IF (TT.GE.TS) INTERACT = 1
        ELSE
           STARTT = INTBUF_TAB(N)%VARIABLES(3)
           STOPT  = INTBUF_TAB(N)%VARIABLES(11)
           IF (STARTT.LE.TT.AND.TT.LE.STOPT) INTERACT = 1
        ENDIF
        IF(INTERACT.NE.0) GOTO 999
C
        RETRI = 0
        ILIST(1) = N
        FAC = DTK(N)/DT2
        FAC1 = DTI*FAC
        IF(NTY.EQ.11)THEN
         CALL IMP_I11XV(
     1  X            ,INTBUF_TAB(N)%NSV,INTBUF_TAB(N)%MSR,NSN        ,NMN   ,
     2  X_TMP        ,D_IMP        ,DR_IMP       ,V            ,VR    ,
     3  FAC          ,FAC1         )
        ELSE
         CALL IMP_I7XV(
     1  X            ,INTBUF_TAB(N)%NSV ,INTBUF_TAB(N)%MSR,NSN ,NMN   ,
     2  INTBUF_TAB(N)%STFNS,X_TMP       ,D_IMP   ,DR_IMP       ,V     ,
     3  VR           ,FAC          ,FAC1         )
        ENDIF
        CALL IMP_TRIPI(
     1       IPARI ,INTBUF_TAB,X_TMP   ,D       ,
     2       V     ,MS     ,ITAB ,VR      ,IN      ,
     3       IMSCH ,I2MSCH ,ISIZXV,ILENXV ,IGRBRIC ,
     4       ISLEN7,IRLEN7 ,ISLEN11,IRLEN11,ISLEN17,
     5       IRLEN17,IRLEN7T,ISLEN7T,IAD_ELEM,FR_ELEM ,
     6       NB     ,ILIST  ,ITASK ,KINET,NEWFRONT,
     7       NUM_IMP,NS_IMP,NE_IMP,IND_IMP,IAD   ,
     8       ISENDTO ,IRECVFROM ,RETRI,WEIGHT,
     9       IXS    ,TEMP  ,DT2PREV,WA     ,N    ,NTY,
     A       IRLEN20,ISLEN20,IRLEN20T,ISLEN20T,IRLEN20E,
     B       ISLEN20E,IKINE,DIAG_SMS,COUNT_REMSLV,COUNT_REMSLVE,
     C       SENSOR  ,XDP  ,H3D_DATA, MULTI_FVM  ,FORNEQS)
 999   CONTINUE
      ENDDO
C
      NT_IMP1=IAD-1
C---------necessair pour spmd-----
      CALL CP_INTTD(NT_IMP1,NUM_IMP ,NS_IMP,NE_IMP,IND_IMP,NUM_IMP1)
 1000 CONTINUE
      IF (NTHR_CP.GT.1) NTHREAD = NTHR_CP 
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_I11XV                     source/implicit/imp_int_k.F   
Chd|-- called by -----------
Chd|        IMP_INTTD0                    source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE IMP_I11XV(
     1       X       ,NSV   ,MSR   ,NSN   ,NMN     ,
     1       X_TMP   ,D     ,DR    ,V     ,VR      ,
     3       FACD    ,FACV  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,NMN,NSV(*),MSR(*)
      my_real
     .        X(3,*),V(3,*),VR(3,*),D(3,*),DR(3,*),
     .        X_TMP(3,*),FACD,FACV
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C     REAL
      my_real
     .        DX,DY,DZ
C------------------------------------
        DO J=1,NSN
          I=NSV(J)
	 IF (I.GT.0) THEN
          DX = D(1,I)*FACD
          DY = D(2,I)*FACD
          DZ = D(3,I)*FACD
          X_TMP(1,I)=X(1,I) + DX
          X_TMP(2,I)=X(2,I) + DY
          X_TMP(3,I)=X(3,I) + DZ
          V(1,I)=D(1,I)*FACV
          V(2,I)=D(2,I)*FACV
          V(3,I)=D(3,I)*FACV
         END IF
        END DO
        DO J=1,NMN
          I=MSR(J)
	 IF (I.GT.0) THEN
          DX = D(1,I)*FACD
          DY = D(2,I)*FACD
          DZ = D(3,I)*FACD
          X_TMP(1,I)=X(1,I) + DX
          X_TMP(2,I)=X(2,I) + DY
          X_TMP(3,I)=X(3,I) + DZ
          V(1,I)=D(1,I)*FACV
          V(2,I)=D(2,I)*FACV
          V(3,I)=D(3,I)*FACV
         END IF
        END DO
C
        IF (IRODDL.NE.0) THEN
         DO J=1,NSN
          I=NSV(J)
	  IF (I.GT.0) THEN
           VR(1,I)=DR(1,I)*FACV
           VR(2,I)=DR(2,I)*FACV
           VR(3,I)=DR(3,I)*FACV
          END IF
         END DO
         DO J=1,NMN
          I=MSR(J)
	  IF (I.GT.0) THEN
           VR(1,I)=DR(1,I)*FACV
           VR(2,I)=DR(2,I)*FACV
           VR(3,I)=DR(3,I)*FACV
          END IF
         END DO
        END IF 
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_I7XV                      source/implicit/imp_int_k.F   
Chd|-- called by -----------
Chd|        IMP_INTTD0                    source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE IMP_I7XV(
     1       X       ,NSV   ,MSR   ,NSN   ,NMN     ,
     1       STFN    ,X_TMP ,D     ,DR    ,V       ,
     3       VR      ,FACD  ,FACV  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSN,NMN,NSV(*),MSR(*)
      my_real
     .        X(3,*),V(3,*),VR(3,*),D(3,*),DR(3,*),
     .        X_TMP(3,*),FACD,FACV,STFN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C     REAL
      my_real
     .        DX,DY,DZ
C------------------------------------
        DO J=1,NSN
	 IF (STFN(J).NE.ZERO) THEN
          I=NSV(J)
          DX = D(1,I)*FACD
          DY = D(2,I)*FACD
          DZ = D(3,I)*FACD
          X_TMP(1,I)=X(1,I) + DX
          X_TMP(2,I)=X(2,I) + DY
          X_TMP(3,I)=X(3,I) + DZ
          V(1,I)=D(1,I)*FACV
          V(2,I)=D(2,I)*FACV
          V(3,I)=D(3,I)*FACV
         END IF
        END DO
        DO J=1,NMN
          I=MSR(J)
	 IF (I.GT.0) THEN
          DX = D(1,I)*FACD
          DY = D(2,I)*FACD
          DZ = D(3,I)*FACD
          X_TMP(1,I) = X(1,I) + DX
          X_TMP(2,I) = X(2,I) + DY
          X_TMP(3,I) = X(3,I) + DZ
          V(1,I) = D(1,I)*FACV
          V(2,I) = D(2,I)*FACV
          V(3,I) = D(3,I)*FACV
         END IF
        END DO
C
        IF (IRODDL.NE.0) THEN
         DO J=1,NSN
	  IF (STFN(J).NE.ZERO) THEN
           I=NSV(J)
           VR(1,I)=DR(1,I)*FACV
           VR(2,I)=DR(2,I)*FACV
           VR(3,I)=DR(3,I)*FACV
          END IF
         END DO
         DO J=1,NMN
          I=MSR(J)
	  IF (I.GT.0) THEN
           VR(1,I)=DR(1,I)*FACV
           VR(2,I)=DR(2,I)*FACV
           VR(3,I)=DR(3,I)*FACV
          END IF
         END DO
        END IF 
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_TRIPI                     source/implicit/imp_int_k.F   
Chd|-- called by -----------
Chd|        IMP_INTTD0                    source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|        I10MAIN_OPT_TRI               source/interfaces/intsort/i10opt_opt_tri.F
Chd|        I10MAIN_TRI                   source/interfaces/intsort/i10main_tri.F
Chd|        I11MAIN_OPT_TRI               source/interfaces/intsort/i11main_opt_tri.F
Chd|        I11MAIN_TRI                   source/interfaces/intsort/i11main_tri.F
Chd|        I20MAIN_OPT_TRI               source/interfaces/intsort/i20main_opt_tri.F
Chd|        I20MAIN_TRI                   source/interfaces/intsort/i20main_tri.F
Chd|        I7MAIN_OPT_TRI                source/interfaces/intsort/i7main_opt_tri.F
Chd|        I7MAIN_TRI                    source/interfaces/intsort/i7main_tri.F
Chd|        IMP_I10MAINF                  source/interfaces/int10/i10ke3.F
Chd|        IMP_I11MAINF                  source/interfaces/int11/i11ke3.F
Chd|        IMP_I7MAINF                   source/interfaces/int07/i7ke3.F
Chd|        SPMD_I7XVCOM2                 source/mpi/interfaces/spmd_i7xvcom2.F
Chd|        SPMD_IFRONT                   source/mpi/interfaces/spmd_ifront.F
Chd|        SPMD_SD_XV                    source/mpi/nodes/spmd_sd_xv.F 
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|====================================================================
      SUBROUTINE IMP_TRIPI(
     1       IPARI ,INTBUF_TAB   ,X       ,D       ,
     2       V     ,MS     ,ITAB ,VR      ,IN      ,
     3       IMSCH ,I2MSCH ,ISIZXV,ILENXV ,IGRBRIC ,
     4       ISLEN7,IRLEN7 ,ISLEN11,IRLEN11,ISLEN17,
     5       IRLEN17,IRLEN7T,ISLEN7T,IAD_ELEM,FR_ELEM ,
     6       NBINTC,INTLIST,ITASK ,KINET,NEWFRONT,
     7       NUM_IMP,NS_IMP,NE_IMP,IND_IMP,IAD   ,
     8       ISENDTO ,IRECVFROM ,RETRI,WEIGHT,
     9       IXS    ,TEMP  ,DT2PREV,WAG    ,N   ,NTY ,
     A       IRLEN20,ISLEN20,IRLEN20T,ISLEN20T,IRLEN20E,
     B       ISLEN20E,IKINE,DIAG_SMS,COUNT_REMSLV,COUNT_REMSLVE,  
     C       SENSOR  ,XDP,H3D_DATA,MULTI_FVM,FORNEQS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
      USE H3D_MOD
      USE MULTI_FVM_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), ITAB(*),
     .        NEWFRONT(*),NBINTC,INTLIST(*),IKINE(*),
     .        ISENDTO(NINTER+1,*),IRECVFROM(NINTER+1,*),
     .        ITASK,IMSCH ,I2MSCH ,ISIZXV,ILENXV,COUNT_REMSLV(*),
     .        COUNT_REMSLVE(*) 
      INTEGER ISLEN7,IRLEN7 ,ISLEN11,IRLEN11,ISLEN17,
     .        IRLEN20,ISLEN20,IRLEN20T,ISLEN20T,IRLEN20E,ISLEN20E,
     .        IRLEN17,IRLEN7T,ISLEN7T,IAD_ELEM(*),FR_ELEM(*) ,
     .        WEIGHT(*),IAD,N,IXS(*)  ,
     .        NUM_IMP(*),NS_IMP(*),NE_IMP(*),IND_IMP(*),
     .        KINET(*),NTY
     
      DOUBLE PRECISION XDP(3,*)     
      
      my_real 
     .   X(3,*), D(3,*),V(*),MS(*),WAG(*),
     .   VR(3,*),DT2PREV, TEMP(*),IN(*), DIAG_SMS(*), SENSOR(NSENSR,*),
     .   FORNEQS(3,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(MULTI_FVM_STRUCT), INTENT(INOUT) :: MULTI_FVM
!
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER KK,LL, RETRI, NBLIST, IFQ,
     .        INACTI, NSNROLD, IAD17, IGN, IGE, NME, NMES,
     .        I,L1,L2,L3,JTASK,LINDMAX,IBID ,IBIDLEN,NRTM_T,
     .        ESHIFT,RENUM(NUMNOD), NSNFIOLD(NSPMD), ISENS
      INTEGER INT24E2EUSE
C     REAL
      my_real
     .        DTI, TS
      my_real
     .  RDUM(3,1)
C-------------dans inttri------
        INT24E2EUSE=0
        RDUM(1:3,1)=ZERO
        IBID =0
        JTASK = ITASK + 1
        IF (NSPMD.GT.1) THEN
         IF(ISIZXV.GT.0) CALL SPMD_SD_XV(
     1     X     ,D       ,V      ,VR    ,MS   ,
     2     IN    ,IAD_ELEM,FR_ELEM,WEIGHT,IMSCH,
     3     D     ,ISIZXV ,ILENXV  ,XDP)
           L1 = 1+NIXS*NUMELS
           L2 = L1+6*NUMELS10
           L3 = L2+12*NUMELS20
           CALL SPMD_I7XVCOM2(
     1       IPARI   ,X    ,V       ,MS          ,
     2       IMSCH   ,I2MSCH ,DT2PREV ,INTLIST ,NBINTC  ,
     3       ISLEN7  ,IRLEN7 ,ISLEN11 ,IRLEN11 ,ISLEN17 ,
     4       IRLEN17 ,IXS    ,IXS(L3) ,
     5       IGRBRIC ,TEMP   ,1       ,IRLEN7T ,ISLEN7T ,
     6       IRLEN20 ,ISLEN20,IRLEN20T,ISLEN20T,IRLEN20E,
     7       ISLEN20E,IKINE  ,DIAG_SMS,SENSOR  ,INTBUF_TAB,INT24E2EUSE,
     8       FORNEQS ,MULTI_FVM)
           CALL SPMD_I7XVCOM2(
     1       IPARI   ,X       ,V       ,MS      ,
     2       IMSCH   ,I2MSCH ,DT2PREV ,INTLIST ,NBINTC  ,
     3       ISLEN7  ,IRLEN7 ,ISLEN11 ,IRLEN11 ,ISLEN17 ,
     4       IRLEN17 ,IXS    ,IXS(L3) ,
     5       IGRBRIC ,TEMP   ,2       ,IRLEN7T ,ISLEN7T ,
     6       IRLEN20 ,ISLEN20,IRLEN20T,ISLEN20T,IRLEN20E,
     7       ISLEN20E,IKINE  ,DIAG_SMS,SENSOR  ,INTBUF_TAB,INT24E2EUSE,
     8       FORNEQS ,MULTI_FVM)
        ENDIF
          NRTM_T = IPARI(4,N)
          ESHIFT = 0
        IF(NTY.EQ.7.OR.NTY.EQ.18)THEN
C
          ISENS = IPARI(64,N)
          IF(ISENS.NE.0)  THEN  ! SENSOR
             TS = SENSOR(2,ISENS)
          ELSE
             TS = TT
          ENDIF
C
          IF(TT.GE.TS) THEN  ! If interface is activated
            CALL I7MAIN_TRI(
     1 IPARI   ,X       ,V         ,
     2 MS      ,N            ,ITASK        ,WAG     ,WEIGHT    ,
     3 ISENDTO ,IRECVFROM    ,RETRI        ,IAD_ELEM,FR_ELEM   ,
     4 ITAB    ,KINET        ,TEMP         ,NRTM_T  ,RENUM     ,
     5 NSNFIOLD,ESHIFT       ,IBID         ,IBID    ,IBID      ,
     6 INTBUF_TAB,H3D_DATA,IXS,MULTI_FVM)
         ENDIF
        ELSEIF(NTY.EQ.10)THEN
C 
          CALL I10MAIN_TRI(
     1 IPARI(1,N),X       ,V         ,
     2 MS        ,N            ,ITASK        ,WAG     ,WEIGHT    ,
     3 ISENDTO   ,IRECVFROM    ,RETRI        ,IAD_ELEM,FR_ELEM   ,
     4 NRTM_T    ,RENUM        ,NSNFIOLD     ,ESHIFT  ,IBID      ,
     5 IBID      ,IBID         ,ITAB         ,INTBUF_TAB         ,
     6 H3D_DATA  )
        ELSEIF(NTY.EQ.11)THEN
C
         ISENS = IPARI(64,N)
         IF(ISENS.NE.0)  THEN  ! SENSOR
            TS = SENSOR(2,ISENS)
         ELSE
            TS = TT
         ENDIF
C
         IF(TT.GE.TS) THEN  ! If interface is activated	
          CALL I11MAIN_TRI(
     1 IPARI      ,X       ,V        ,
     2 MS         ,N            ,ITASK        ,WEIGHT  ,ISENDTO  ,
     3 IRECVFROM  ,RETRI        ,IAD_ELEM     ,FR_ELEM ,ITAB     ,
     4 NRTM_T     ,ESHIFT       ,IBID         ,RENUM   ,NSNFIOLD ,
     5 INTBUF_TAB ,IBID)
        ENDIF

        ELSEIF(NTY == 20)THEN
C 
          CALL I20MAIN_TRI(
     1 IPARI   ,X       ,V       ,
     2 MS      ,N            ,ITASK        ,WAG     ,WEIGHT  ,
     3 ISENDTO ,IRECVFROM    ,RETRI        ,IAD_ELEM,FR_ELEM ,
     4 ITAB    ,KINET        ,TEMP         ,NRTM_T  ,RENUM   ,
     5 NSNFIOLD,ESHIFT       ,IBID         ,IBID    ,DIAG_SMS,
     5 IBID    ,INTBUF_TAB   ,H3D_DATA     )
        ENDIF
C
        IF (IMACH.EQ.3.AND.NSPMD.GT.1.AND.RETRI.EQ.1) THEN
C--------- a modifier par interface---
         CALL SPMD_IFRONT(
     1    IPARI  ,NEWFRONT,ISENDTO ,IRECVFROM,
     2    NBINTC ,INTLIST ,IBIDLEN ,IBIDLEN  ,
     3    IBIDLEN,IBIDLEN,IBIDLEN ,IBIDLEN ,IBIDLEN  ,
     4    IBIDLEN,IBIDLEN,IBIDLEN ,IBIDLEN ,IBIDLEN  ,
     5    IBIDLEN,IBIDLEN,SENSOR  ,INTBUF_TAB, 1)
         CALL SPMD_IFRONT(
     1    IPARI  ,NEWFRONT,ISENDTO ,IRECVFROM,
     2    NBINTC ,INTLIST ,IBIDLEN ,IBIDLEN  ,
     3    IBIDLEN,IBIDLEN,IBIDLEN ,IBIDLEN ,IBIDLEN  ,
     4    IBIDLEN,IBIDLEN,IBIDLEN ,IBIDLEN ,IBIDLEN  ,
     5    IBIDLEN,IBIDLEN,SENSOR  ,INTBUF_TAB, 2)


        ENDIF
C
        IF(NTY.EQ.7)THEN
C
          ISENS = IPARI(64,N)
          IF(ISENS.NE.0)  THEN  ! SENSOR
             TS = SENSOR(2,ISENS)
          ELSE
             TS = TT
          ENDIF
C
          IF(TT.GE.TS) THEN  ! If interface is activated
            CALL I7MAIN_OPT_TRI(
     1       IPARI     ,INTBUF_TAB(N),X   ,V  ,
     2       N         ,ITASK ,COUNT_REMSLV       )
          ENDIF
        ELSEIF(NTY.EQ.10)THEN
          CALL I10MAIN_OPT_TRI(
     1       IPARI(1,N),INTBUF_TAB(N),X  ,V  ,
     2       N         ,ITASK  ,COUNT_REMSLV      )
        ELSEIF(NTY.EQ.11)THEN
C
          ISENS = IPARI(64,N)
          IF(ISENS.NE.0)  THEN  ! SENSOR
             TS = SENSOR(2,ISENS)
          ELSE
             TS = TT
          ENDIF
C
          IF(TT.GE.TS) THEN  ! If interface is activated
            CALL I11MAIN_OPT_TRI(
     1        IPARI     ,INTBUF_TAB(N),X  ,V  ,
     2        N         ,ITASK  ,COUNT_REMSLV      )
          ENDIF
        ELSEIF(NTY == 20)THEN
          CALL I20MAIN_OPT_TRI(
     1       IPARI     ,INTBUF_TAB(N),X   ,V  ,
     2       N         ,ITASK ,COUNT_REMSLV ,COUNT_REMSLVE  )
        ENDIF
C-------------dans intfop2------
        IF(IMACH.EQ.3) THEN
C LINDMAX = NCONT*MULTIMP
          LINDMAX  = IPARI(18,N)*IPARI(23,N)
        ELSE
C LINDMAX = NSN*MULTIMP
          LINDMAX  = IPARI(5,N)*IPARI(23,N)
        ENDIF
C
        IF(NTY.EQ.7)THEN
C
          ISENS = IPARI(64,N)
          IF(ISENS.NE.0)  THEN  ! SENSOR
             TS = SENSOR(2,ISENS)
          ELSE
             TS = TT
          ENDIF
C
          IF(TT.GE.TS) THEN  ! If interface is activated
           CALL IMP_I7MAINF(
     1  IPARI     ,INTBUF_TAB(N),X          ,V           ,
     2  MS        ,N            ,LINDMAX      ,JTASK      ,
     3  NUM_IMP(N),NS_IMP(IAD)  ,NE_IMP(IAD)  ,IND_IMP(IAD))
           IAD=IAD+NUM_IMP(N)
         ENDIF
        ELSEIF(NTY.EQ.10)THEN
C
           CALL IMP_I10MAINF(
     1  IPARI(1,N),INTBUF_TAB(N),X          ,V           ,
     2  MS        ,N            ,LINDMAX      ,JTASK      ,
     3  NUM_IMP(N),NS_IMP(IAD)  ,NE_IMP(IAD)  ,IND_IMP(IAD))
           IAD=IAD+NUM_IMP(N)
        ELSEIF(NTY.EQ.11)THEN
C
          ISENS = IPARI(64,N)
          IF(ISENS.NE.0)  THEN  ! SENSOR
             TS = SENSOR(2,ISENS)
          ELSE
             TS = TT
          ENDIF
C
          IF(TT.GE.TS) THEN  ! If interface is activated
           CALL IMP_I11MAINF(
     1  IPARI(1,N),INTBUF_TAB(N),X          ,V           ,
     2  MS        ,N            ,LINDMAX      ,JTASK      ,
     3  NUM_IMP(N),NS_IMP(IAD)  ,NE_IMP(IAD)  ,ITAB)
           IAD=IAD+NUM_IMP(N)
          ENDIF
        ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  CP_INTTD                      source/implicit/imp_int_k.F   
Chd|-- called by -----------
Chd|        IMP_INTTD0                    source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|        CP_INT                        source/implicit/produt_v.F    
Chd|        IMP_INTTD                     share/modules/imp_mod_def.F   
Chd|====================================================================
      SUBROUTINE CP_INTTD(NT_IMP1,NUMIMP ,NS_IMP,NE_IMP,IND_IMP,NUMIMP1)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTTD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "scr05_c.inc" 
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NUMIMP(*),NS_IMP(*),NE_IMP(*),IND_IMP(*),
     .         NUMIMP1(*),NT_IMP1
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,N,IAD,IAD1,IADT
      INTEGER IERROR1,IERROR2,IERROR3,IERROR4
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
       IF(NT_IMP1.EQ.0) RETURN
        IF(ALLOCATED(NS_IMP1)) DEALLOCATE(NS_IMP1)
        ALLOCATE(NS_IMP1(NT_IMP1),STAT=IERROR1)
        IF(ALLOCATED(NE_IMP1)) DEALLOCATE(NE_IMP1)
        ALLOCATE(NE_IMP1(NT_IMP1),STAT=IERROR2)
        IF(ALLOCATED(IND_IMP1)) DEALLOCATE(IND_IMP1)
        ALLOCATE(IND_IMP1(NT_IMP1),STAT=IERROR3)
        IF(ALLOCATED(IAD1_NIN)) DEALLOCATE(IAD1_NIN)
        ALLOCATE(IAD1_NIN(NINTER),STAT=IERROR4)
C
        CALL CP_INT(NINTER,NUMIMP,NUMIMP1)
        CALL CP_INT(NT_IMP1,NS_IMP,NS_IMP1)
        CALL CP_INT(NT_IMP1,NE_IMP,NE_IMP1)
        CALL CP_INT(NT_IMP1,IND_IMP,IND_IMP1)
        IAD1 = 0
        DO N = 1,NINTER
         IAD1_NIN(N) =IAD1  
         IAD1 =IAD1 + NUMIMP1(N) 
        END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  SAV_INTTD                     source/implicit/imp_int_k.F   
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_INTTD                     share/modules/imp_mod_def.F   
Chd|====================================================================
      SUBROUTINE SAV_INTTD(NT_IMP,NUMIMP,NS_IMP,NE_IMP,IND_IMP,
     1                     NUMIMP1)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTTD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "impl1_c.inc"
#include      "task_c.inc"
#include      "scr05_c.inc" 
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NT_IMP,NUMIMP(*),NS_IMP(*),NE_IMP(*),IND_IMP(*),
     .         NUMIMP1(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,N,IAD,IAD1,IADT
      INTEGER IERROR1,IERROR2,IERROR3,IERROR4
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
       IF(NT_IMP1.EQ.0) RETURN
C------dans NS_IMP1 input: NT_IMP1 , output:d'abord NT_IMP1, + NT_IMP-NT_IMP1
C------dans NS_IMP input:d'abord NT_IMP1, + NT_IMP-NT_IMP1, output: NT_IMP
       NT_IMP = NT_IMP + NT_IMP1
       IF(NSPMD.GT.1) THEN
        IAD1 = 0
        DO N = 1,NINTER
	 DO I = 1,NUMIMP1(N)
          NS_IMP(IAD1+I)=NS_IMP1(IAD1+I)
         END DO
         IAD1 =IAD1 + NUMIMP1(N) 
        END DO
       ENDIF
C----
        IF(ALLOCATED(NS_IMP1)) DEALLOCATE(NS_IMP1)
        ALLOCATE(NS_IMP1(NT_IMP),STAT=IERROR1)
        IF(ALLOCATED(NE_IMP1)) DEALLOCATE(NE_IMP1)
        ALLOCATE(NE_IMP1(NT_IMP),STAT=IERROR2)
        IF(ALLOCATED(IND_IMP1)) DEALLOCATE(IND_IMP1)
        ALLOCATE(IND_IMP1(NT_IMP),STAT=IERROR3)
C
       IAD1 = 0
       DO N = 1,NINTER
	DO I = 1,NUMIMP1(N)
	 NS_IMP1(IAD1+I) = NS_IMP(IAD1+I)
	 NE_IMP1(IAD1+I) = NE_IMP(IAD1+I)
	 IND_IMP1(IAD1+I) = IND_IMP(IAD1+I)
        END DO
        IAD1 =IAD1 + NUMIMP1(N) 
       END DO
       DO N = 1,NINTER
	DO I = 1,NUMIMP(N)
	 NS_IMP1(IAD1+I) = NS_IMP(IAD1+I)
	 NE_IMP1(IAD1+I) = NE_IMP(IAD1+I)
	 IND_IMP1(IAD1+I) = IND_IMP(IAD1+I)
        END DO
        IAD1 =IAD1 + NUMIMP(N) 
       END DO
C--------change ind for NS_IMP,INE_IMP,IND_IMP---
       IAD = 0
       IAD1 = 0
       DO N = 1,NINTER
	DO I = 1,NUMIMP1(N)
	 NS_IMP(IAD+I) = NS_IMP1(IAD1+I)
	 NE_IMP(IAD+I) = NE_IMP1(IAD1+I)
	 IND_IMP(IAD+I) = IND_IMP1(IAD1+I)
        END DO
        IAD =IAD + NUMIMP1(N) + NUMIMP(N)
        IAD1 = IAD1 + NUMIMP1(N)
       END DO
       IAD = 0
       DO N = 1,NINTER
        IADT =IAD + NUMIMP1(N) 
	DO I = 1,NUMIMP(N)
	 NS_IMP(IADT+I) = NS_IMP1(IAD1+I)
	 NE_IMP(IADT+I) = NE_IMP1(IAD1+I)
	 IND_IMP(IADT+I) = IND_IMP1(IAD1+I)
        END DO
        IAD1 =IAD1 + NUMIMP(N) 
        IAD =IAD + NUMIMP1(N) + NUMIMP(N)
       END DO
C--------change ind for NUM_IMP---
       DO N = 1,NINTER
        NUMIMP(N) = NUMIMP1(N) + NUMIMP(N)
       END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_RNUMCD                    source/implicit/imp_int_k.F   
Chd|-- called by -----------
Chd|        I10MAIN_TRI                   source/interfaces/intsort/i10main_tri.F
Chd|        I20MAIN_TRI                   source/interfaces/intsort/i20main_tri.F
Chd|        I22MAIN_TRI                   source/interfaces/intsort/i22main_tri.F
Chd|        I23MAIN_TRI                   source/interfaces/intsort/i23main_tri.F
Chd|        I7MAIN_TRI                    source/interfaces/intsort/i7main_tri.F
Chd|-- calls ---------------
Chd|        IMP_INTTD                     share/modules/imp_mod_def.F   
Chd|====================================================================
      SUBROUTINE IMP_RNUMCD(CAND_N ,NIN,NSN,NUM_IMP,INDEX )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTTD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER CAND_N(*) ,NIN,NSN,NUM_IMP,INDEX(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NI,IAD
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
       IAD = IAD1_NIN(NIN)
       DO I = 1, NUM_IMP
        NI = NS_IMP1(IAD+I)
        IF(NI.GT.NSN) THEN
          NI = NI - NSN
          NS_IMP1(IAD+I) = CAND_N(INDEX(I))
        END IF        
       END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  IMP_DTKIN                     source/implicit/imp_int_k.F   
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_INTDT                     source/implicit/imp_int_k.F   
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IMP_DTKIN(
     1       IPARI ,INTBUF_TAB      ,X       ,V       ,
     2       VR    ,ITAB   ,D_IMP   ,DR_IMP  ,NBINTC  ,
     3       INTLIST,ITASK ,NEWFRONT,ISENDTO ,IRECVFROM,
     4       IDDL  ,NDOF   ,IKC     ,SCAL    ,MS      ,
     5       SENSOR, MAXDGAP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), ITAB(*),
     .        NEWFRONT(*),NBINTC,INTLIST(*),
     .        ISENDTO(NINTER+1,*),IRECVFROM(NINTER+1,*),
     .        ITASK
      INTEGER IDDL(*)  ,NDOF(*),IKC(*)
      my_real 
     .   X(3,*), V(3,*),VR(3,*),
     .   D_IMP(3,*),DR_IMP(3,*),SCAL,MS(*),SENSOR(NSENSR,*),
     .   MAXDGAP(NINTER)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,I,J,ID,NTHR_CP
C     REAL
      my_real
     .        DTK(NINTER),DTI,DT_MIN
C-------------before inttri------
       SCAL = ONE
       IF (ITTOFF.GT.0.OR.IMCONV.EQ.1.OR.IMCONV.LE.-2) RETURN
        NTHR_CP = NTHREAD
        IF (NTHREAD.GT.1) NTHREAD = 1
        DTI = ONE/DT2
        DO I=1,NUMNOD
          V(1,I)=D_IMP(1,I)*DTI
          V(2,I)=D_IMP(2,I)*DTI
          V(3,I)=D_IMP(3,I)*DTI
        ENDDO
        IF (IRODDL.NE.0) THEN
         DO I=1,NUMNOD
           VR(1,I)=DR_IMP(1,I)*DTI
           VR(2,I)=DR_IMP(2,I)*DTI
           VR(3,I)=DR_IMP(3,I)*DTI
         ENDDO
        ENDIF 
       CALL IMP_INTDT(
     1       IPARI    ,INTBUF_TAB       ,X        ,
     2       V        ,VR      ,ISENDTO ,IRECVFROM,
     4       NEWFRONT ,ITASK   ,DTK     ,ITAB     ,
     5       INTLIST  ,NBINTC  ,DT_MIN  ,MS       ,
     6       SENSOR   ,MAXDGAP)
C  
       SCAL = DT_MIN*DTI
       IF (SCAL.LT.ONE) THEN
        DO I=1,NUMNOD
          D_IMP(1,I)=D_IMP(1,I)*SCAL
          D_IMP(2,I)=D_IMP(2,I)*SCAL
          D_IMP(3,I)=D_IMP(3,I)*SCAL
        ENDDO
        IF (IRODDL.NE.0) THEN
         DO I=1,NUMNOD
           DR_IMP(1,I)=DR_IMP(1,I)*SCAL
           DR_IMP(2,I)=DR_IMP(2,I)*SCAL
           DR_IMP(3,I)=DR_IMP(3,I)*SCAL
         ENDDO
        ENDIF 
       ENDIF
C       
       IF (NTHR_CP.GT.1) NTHREAD = NTHR_CP
C
      RETURN
      END
Chd|====================================================================
Chd|  KIN_KNL                       source/implicit/imp_int_k.F   
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        DIM_KINKN                     source/implicit/imp_int_k.F   
Chd|        IDDL_MINT                     source/implicit/imp_int_k.F   
Chd|        INI_KINKN                     source/implicit/imp_int_k.F   
Chd|        RBE3_MINT                     source/implicit/imp_int_k.F   
Chd|        IMP_ASPC                      share/modules/impbufdef_mod.F 
Chd|        IMP_KNON                      share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE KIN_KNL(         
     1    IPARI     ,INTBUF_TAB ,NUM_IMP   ,NS_IMP    ,NE_IMP    ,
     2    NPBY      ,LPBY      ,ITAB      ,NRBYAC    ,IRBYAC    ,
     3    NINT2     ,IINT2     ,IBFV      ,LJ        ,ISKEW     ,
     4    ICODT     ,NDOFI     ,IDDL      ,IKC       ,NDOF      ,
     5    INLOC     ,IRBE3     ,LRBE3     ,FRBE3     ,X         ,
     6    SKEW      ,IRBE2     ,LRBE2     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_KNON
      USE IMP_ASPC
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*),NUM_IMP(*),NS_IMP(*),
     .        NE_IMP(*),NDOFI(*)
      INTEGER NPBY(NNPBY,*),LPBY(*),ITAB(*),NRBYAC,IRBYAC(*),
     .        NINT2,IINT2(*),IDDL(*),IKC(*),NDOF(*),INLOC(*),
     .        IBFV(*),LJ(*),ISKEW(*),ICODT(*),IRBE3(NRBE3L,*),LRBE3(*),
     .        IRBE2(NRBE2L,*),LRBE2(*)
C     REAL
      my_real
     .        X(*),SKEW(*),FRBE3(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N, IAD,NTY,NDOFII(NUMNOD),NKC,
     .        IERR1,IERR2,IERR3,IERR4,IERR5,IERR6,ND,NMT,IROT,NUMN
C-----------------------------------------------
       DO N=1,NUMNOD
        NDOFII(N) = IABS(NDOFI(N))
       ENDDO 
       NUMN_KN = 0
       DO N=1,NUMNOD
        IF (NDOFII(N).GT.0) NUMN_KN = NUMN_KN + 1
       ENDDO 
C--------allocation------ 
C
       ALLOCATE(IN_KN(NUMN_KN),ID_KN(3,NUMN_KN),STAT=IERR1)
      IF (NUMN_KN .GT. 0) THEN
       ID_KN = -7	  
       N = 0
C------------valeur nega pour les nsl nodes-----      
       DO I=1,NUMNOD
       IF (NDOFII(I).GT.0) THEN
        N = N + 1
        IN_KN(N) = I
       ENDIF
       ENDDO 
      END IF 
      CALL DIM_KINKN(
     1    NPBY      ,LPBY      ,NRBYAC    ,IRBYAC    ,NINT2     ,
     2    IINT2     ,IPARI     ,INTBUF_TAB,NDOFII    ,IBFV      ,
     3    LJ        ,ISKEW     ,ICODT     ,NRB_KN    ,NI2_KN    ,
     4    NBC_KN    ,NFX_KN    ,NRW_KN    ,IRBE3     ,NRBE3_KN  ,
     5    NSPC_KN   ,IRBE2     ,LRBE2     ,NRBE2_KN  )
      IF (NI2_KN.GT.0) THEN
       ALLOCATE(II2_KN(2,NI2_KN),ID_KNM2(6,4,NI2_KN),STAT=IERR2)
      ENDIF 
      IF (NRB_KN.GT.0) THEN
       ALLOCATE(IRB_KN(2,NRB_KN),ID_KNM(6,NRB_KN),STAT=IERR3)
      ENDIF 
      IF (NBC_KN.GT.0) THEN
       IF(ALLOCATED(IBC_KN)) DEALLOCATE(IBC_KN)
       ALLOCATE(IBC_KN(3,NBC_KN),STAT=IERR4)
      ENDIF 
      IF (NSPC_KN.GT.0) THEN
       IF(ALLOCATED(ISPC_KN)) DEALLOCATE(ISPC_KN)
       ALLOCATE(ISPC_KN(NSPC_KN),STAT=IERR4)
      ENDIF 
C--
      IF (NFX_KN.GT.0) THEN
       IF(ALLOCATED(IFX_KN)) DEALLOCATE(IFX_KN)
       ALLOCATE(IFX_KN(2,NFX_KN),STAT=IERR5)
      ENDIF
C 
      IF (NRW_KN.GT.0) THEN
       IF(ALLOCATED(IRW_KN)) DEALLOCATE(IRW_KN)
       ALLOCATE(IRW_KN(NRW_KN),STAT=IERR6)
      ENDIF
C       
      IF (NRBE3_KN.GT.0) THEN
       ALLOCATE(IRBE3_KN(NRBE3_KN),STAT=IERR6)
      ENDIF 
C       
      IF (NRBE2_KN.GT.0) THEN
       ALLOCATE(IRBE2_KN(2,NRBE2_KN),ID_KNM4(6,NRBE2_KN),STAT=IERR3)
      ENDIF 
C       
      CALL INI_KINKN(
     1    NPBY      ,LPBY      ,NRBYAC    ,IRBYAC    ,NINT2     ,
     2    IINT2     ,IPARI     ,INTBUF_TAB,NDOFII    ,IBFV      ,
     3    LJ        ,ISKEW     ,ICODT     ,NRB_KN    ,IRB_KN    ,
     3    NI2_KN    ,II2_KN    ,NBC_KN    ,IBC_KN    ,NFX_KN    ,
     4    IFX_KN    ,NRW_KN    ,IRW_KN    ,IRBE3     ,NRBE3_KN  ,
     5    IRBE3_KN  ,NSPC_KN   ,ISPC_KN   ,IRBE2     ,LRBE2     ,
     6    NRBE2_KN  ,IRBE2_KN  )
C ------ini RBE3---     
      IF (NRBE3_KN.GT.0) THEN
        IAD=0
        NMT = 0
        IROT=0
        DO I=1,NRBE3_KN
         N=IRBE3_KN(I)
         NUMN = IRBE3(5,N)
         IAD=MAX(IAD,NUMN)
         NMT = NMT + NUMN
         IROT=MAX(IROT,IRBE3(6,N))
        ENDDO 
        ALLOCATE(ID_KNM3(6,IAD,NRBE3_KN),STAT=IERR3)
	 ID_KNM3=0
	 RKN_MAX=IAD
        ALLOCATE(FCDI_KN(18*NMT),STAT=IERR5)
	 FCDI_KN=ZERO
        IF (IROT.GT.0) THEN
	 ALLOCATE(MCDI_KN(18*NMT),STAT=IERR5)
	 MCDI_KN=ZERO
        ENDIF 
        CALL RBE3_MINT(IRBE3   ,LRBE3    ,FRBE3   ,X     ,SKEW ,
     .                 NRBE3_KN,IRBE3_KN ,FCDI_KN,MCDI_KN)
      ENDIF 
C--------- use NDOFII(I)--as IDDLM ------------
      IF (NUMN_KN .EQ. 0) RETURN
      NKC=0
      DO N =1,NUMNOD
       I=INLOC(N)
       NDOFII(I)=IDDL(I)-NKC
       DO J=1,NDOF(I)
        ND = IDDL(I)+J
        IF (IKC(ND).NE.0) NKC = NKC + 1
       ENDDO 
      ENDDO
      CALL IDDL_MINT(NUMN_KN,IN_KN   ,IDDL    ,IKC     ,NDOF     ,
     .              NDOFII  ,IPARI   ,INTBUF_TAB,ID_KN   ,NRB_KN   ,
     .              IRB_KN  ,ID_KNM  ,NI2_KN  ,II2_KN  ,ID_KNM2  ,
     .              IRBE3   ,LRBE3   ,NRBE3_KN,IRBE3_KN,ID_KNM3  ,
     .              RKN_MAX ,IRBE2   ,NRBE2_KN,IRBE2_KN,ID_KNM4  )
C------------return origine NDOFI(I)----
      DO I=1,NUMNOD
       IF (NDOFI(I).LT.0) NDOFI(I) = 0
      ENDDO 
      RETURN
      END
Chd|====================================================================
Chd|  DIM_KINKN                     source/implicit/imp_int_k.F   
Chd|-- called by -----------
Chd|        KIN_KNL                       source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|        IMP_ASPC                      share/modules/impbufdef_mod.F 
Chd|        IMP_RWL                       share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DIM_KINKN(
     1    NPBY      ,LPBY      ,NRBYAC    ,IRBYAC    ,NINT2     ,
     2    IINT2     ,IPARI     ,INTBUF_TAB,INLOC     ,IBFV      ,
     3    LJ        ,ISKEW     ,ICODT     ,LNS       ,LNS2      ,
     4    LBCL      ,LFXL      ,LRW       ,IRBE3     ,LNS3      ,
     5    LSPCL     ,IRBE2     ,LRBE2     ,LNS4      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_RWL
      USE IMP_ASPC
      USE INTBUFDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPBY(NNPBY,*),LPBY(*),NRBYAC,IRBYAC(*),
     .        NINT2,IINT2(*),IPARI(NPARI,*)
      INTEGER IBFV(NIFV,*),LJ(*),ISKEW(*) ,ICODT(*),LSPCL
      INTEGER INLOC(*),LNS  ,LNS2,LBCL ,LFXL ,LRW,IRBE3(NRBE3L,*),LNS3,
     .        IRBE2(NRBE2L,*),LRBE2(*),LNS4
C     REAL

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .        I,J,K,N,L,NL,NJ,NI,J1,M,NSN,N1,N2,NK,ID,
     .        JI,K10,K11,K12,K13,K14,KFI,NS
C----------------------------
      LNS2=0
      DO J=1,NINT2
       N=IINT2(J)
       NSN = IPARI(5,N)
       JI=IPARI(1,N)
       K10=JI-1
       K11=K10+4*IPARI(3,N)
C------IRECT(4,NSN)-----
       K12=K11+4*IPARI(4,N)
C------NSV(NSN)--node number---
       K13=K12+NSN
C------MSR(NMN)-----
       K14=K13+IPARI(6,N)
C------IRTL(NSN)--main el number---
       KFI=K14+NSN
       DO I=1,NSN
        NI=INTBUF_TAB(N)%NSV(I)
        IF (INLOC(NI).GT.0) THEN
         LNS2=LNS2+1
        ENDIF
       ENDDO
      ENDDO
C--------RBE3--------------------
      LNS3=0
      DO N=1,NRBE3
       NI = IRBE3(3,N)
       IF (NI.EQ.0) CYCLE
       IF (INLOC(NI).GT.0) THEN
         LNS3=LNS3+1
       ENDIF
      ENDDO
C-----active rigid body main nodes------
      LNS=0
      DO J=1,NRBYAC
       N=IRBYAC(J)
       K=IRBYAC(J+NRBYKIN)
       M  =NPBY(1,N)
       NSN  =NPBY(2,N)
        DO I=1,NSN
         ID = I+K
         NI=LPBY(ID)
         IF (INLOC(NI).GT.0) THEN
          LNS=LNS+1
          IF (INLOC(M).EQ.0) INLOC(M) = 1
         ENDIF
       ENDDO
      ENDDO
C
      LBCL = 0
      DO N=1,NUMNOD
        IF (ISKEW(N).GT.1.AND.ICODT(N).NE.7) THEN
         IF (INLOC(N).GT.0)LBCL = LBCL + 1  
        ENDIF
      ENDDO
      LSPCL = 0
      DO N=1,NSPCL
       IF (INLOC(N).GT.0.AND.IC_SPC(N).LE.3)LSPCL = LSPCL + 1  
      ENDDO
C ---
      LFXL = 0
      DO J=1,NFXVEL
        IF (LJ(J).GT.0.AND.LJ(J).LE.3) THEN
         N=IABS(IBFV(1,J))
         IF (INLOC(N).GT.0)LFXL = LFXL + 1  
        ENDIF
      ENDDO
C
      LRW = 0
      DO J=1,N_RWL
       N=IN_RWL(J)
       IF (INLOC(N).GT.0) LRW = LRW + 1  
      ENDDO
C-----Rbe2------
      LNS4=0
      DO N=1,NRBE2
       K=IRBE2(1,N)
       M  =IRBE2(3,N)
       NSN =IRBE2(5,N)
        DO I=1,NSN
         ID = I+K
         NI=LRBE2(ID)
         IF (INLOC(NI).GT.0) THEN
          LNS4=LNS4+1
          IF (INLOC(M).EQ.0) INLOC(M) = 2
         ENDIF
       ENDDO
      ENDDO
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  INI_KINKN                     source/implicit/imp_int_k.F   
Chd|-- called by -----------
Chd|        KIN_KNL                       source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|        IMP_ASPC                      share/modules/impbufdef_mod.F 
Chd|        IMP_RWL                       share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INI_KINKN(
     1    NPBY      ,LPBY      ,NRBYAC    ,IRBYAC    ,NINT2     ,
     2    IINT2     ,IPARI     ,INTBUF_TAB,INLOC     ,IBFV      ,
     3    LJ        ,ISKEW     ,ICODT     ,NRB_MV    ,IRB_MV    ,
     3    NI2_MV    ,II2_MV    ,NBC_MV    ,IBC_MV    ,NFX_MV    ,
     4    IFX_MV    ,NRW_MV    ,IRW_MV    ,IRBE3     ,NRBE3_MV  ,
     5    IRBE3_MV  ,NSPC_MV   ,ISPC_MV   ,IRBE2     ,LRBE2     ,
     6    NRBE2_MV  ,IRBE2_MV  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_RWL
      USE IMP_ASPC
      USE INTBUFDEF_MOD
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBFV(NIFV,*),LJ(*),ISKEW(*) ,ICODT(*)
      INTEGER NPBY(NNPBY,*),LPBY(*),NRBYAC,IRBYAC(*),
     .        NINT2,IINT2(*),IPARI(NPARI,*),IRBE3(NRBE3L,*),
     .        IRBE2(NRBE2L,*),LRBE2(*)
      INTEGER 
     .   INLOC(*),NRB_MV,NI2_MV,IRB_MV(2,*),II2_MV(2,*),
     .   NBC_MV,IBC_MV(3,*) ,NFX_MV,IFX_MV(2,*),NRW_MV,IRW_MV(*),
     .   NRBE3_MV,IRBE3_MV(*),NSPC_MV,ISPC_MV(*),NRBE2_MV,IRBE2_MV(*)
C     REAL

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .        I,J,K,N,L,NL,NJ,NI,J1,M,NSN,N1,N2,NK,ID,NR3,NR4,
     .        JI,K10,K11,K12,K13,K14,KFI,NI2,NRB,NBC,NFX,NRW,NSPC
c----------------------
      NI2=0
      IF (NI2_MV.GT.0) THEN
       DO J=1,NINT2
        N=IINT2(J)
        NSN = IPARI(5,N)
        JI=IPARI(1,N)
        K10=JI-1
        K11=K10+4*IPARI(3,N)
C------IRECT(4,NSN)-----
        K12=K11+4*IPARI(4,N)
C------NSV(NSN)--node number---
        K13=K12+NSN
C------MSR(NMN)-----
        K14=K13+IPARI(6,N)
C------IRTL(NSN)--main el number---
        KFI=K14+NSN
        DO I=1,NSN
         NI=INTBUF_TAB(N)%NSV(I)
         IF (INLOC(NI).GT.0) THEN
          NI2=NI2+1
          II2_MV(1,NI2)=N
          II2_MV(2,NI2)=I
         ENDIF
        ENDDO
       ENDDO
       IF (NI2.NE.NI2_MV) WRITE(*,*)'pb cal NI2_M'
      ENDIF 
C--------RBE3--------------------
      IF (NRBE3_MV.GT.0) THEN
       NR3=0
       DO N=1,NRBE3
        NI = IRBE3(3,N)
        IF (NI.EQ.0) CYCLE
        IF (INLOC(NI).GT.0) THEN
          NR3=NR3+1
          IRBE3_MV(NR3)=N
        ENDIF
       ENDDO
       IF (NR3.NE.NRBE3_MV) WRITE(*,*)'pb cal NRBE3_M'
      ENDIF 
C-----active rigid body main nodes------
      NRB=0
      IF (NRB_MV.GT.0) THEN
       DO J=1,NRBYAC
        N=IRBYAC(J)
        K=IRBYAC(J+NRBYKIN)
        M  =NPBY(1,N)
        IF (INLOC(M).GT.0) THEN
         NSN  =NPBY(2,N)
         DO I=1,NSN
          ID = I+K
          NI=LPBY(ID)
          IF (INLOC(NI).GT.0) THEN
           NRB=NRB+1
           IRB_MV(1,NRB)=M
           IRB_MV(2,NRB)=NI
          ENDIF
         ENDDO
        ENDIF 
       ENDDO
       IF (NRB.NE.NRB_MV) WRITE(*,*)'pb cal NRB_M'
      ENDIF 
C +++
      IF (NBC_MV.GT.0) THEN
       NBC = 0
       DO N=1,NUMNOD
        IF (ISKEW(N).GT.1.AND.ICODT(N).NE.7) THEN
         IF (INLOC(N).GT.0) THEN
           NBC = NBC + 1  
           IBC_MV(1,NBC) = N 
           IBC_MV(2,NBC) = ISKEW(N)
           IBC_MV(3,NBC) = ICODT(N)
         ENDIF 
        ENDIF
       ENDDO
       IF (NBC.NE.NBC_MV) WRITE(*,*)'pb cal NBC_M'
      ENDIF 
C-
      IF (NSPC_MV.GT.0) THEN
       NSPC = 0
       DO N=1,NSPCL
        IF (INLOC(N).GT.0.AND.IC_SPC(N).LE.3)THEN
	 NSPC = NSPC + 1  
         ISPC_MV(NSPC) = N 
        ENDIF 
       ENDDO
       IF (NSPC.NE.NSPC_MV) WRITE(*,*)'pb cal NSPC_M'
      ENDIF 
C---      
      IF (NFX_MV.GT.0) THEN
       NFX = 0
       DO J=1,NFXVEL
        IF (LJ(J).GT.0.AND.LJ(J).LE.3) THEN
         N=IABS(IBFV(1,J))
         IF (INLOC(N).GT.0) THEN
           NFX = NFX + 1  
           IFX_MV(1,NFX) = J 
           IFX_MV(2,NFX) = LJ(J)
         ENDIF 
        ENDIF
       ENDDO
       IF (NFX.NE.NFX_MV) WRITE(*,*)'pb cal NFX_M'
      ENDIF
C 
      IF (NRW_MV.GT.0) THEN
       NRW = 0
       DO J=1,N_RWL
         N=IN_RWL(J)
         IF (INLOC(N).GT.0) THEN
           NRW = NRW + 1  
           IRW_MV(NRW) = J 
         ENDIF 
       ENDDO
       IF (NRW.NE.NRW_MV) WRITE(*,*)'pb cal NRW_M'
      ENDIF 
C-----RBE2------
      NR4=0
      IF (NRBE2_MV.GT.0) THEN
       DO N=1,NRBE2
        K=IRBE2(1,N)
        M  =IRBE2(3,N)
        IF (INLOC(M).GT.0) THEN
         NSN  =IRBE2(5,N)
         DO I=1,NSN
          ID = I+K
          NI=LRBE2(ID)
          IF (INLOC(NI).GT.0) THEN
           NR4=NR4+1
           IRB_MV(1,NR4)=N
           IRB_MV(2,NR4)=NI
          ENDIF
         ENDDO
        ENDIF 
       ENDDO
       IF (NR4.NE.NRBE2_MV) WRITE(*,*)'pb cal NRBE2'
      ENDIF 
C ---
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  IDDL_MINT                     source/implicit/imp_int_k.F   
Chd|-- called by -----------
Chd|        KIN_KNL                       source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IDDL_MINT(NML    ,IML     ,IDDL    ,IKC     ,NDOF     ,
     .                    IDDLM   ,IPARI   ,INTBUF_TAB,IDDML   ,NRB_FR   ,
     .                    IFRSR   ,IDDMR   ,NI2_FR  ,IFRS2   ,IDDMI2   ,
     .                    IRBE3   ,LRBE3   ,NRBE3_FR,IFRS3   ,IDDMI3   ,
     .                    M_MAX   ,IRBE2   ,NRBE2_FR,IFRS4   ,IDDMI4   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  NML,IML(*),IDDL(*) ,IKC(*) ,NDOF(*) ,IDDLM(*),M_MAX
      INTEGER  IPARI(NPARI,*),IDDML(3,*),NRB_FR , 
     .         IDDMR(6,*) ,NI2_FR  ,IDDMI2(6,4,*) ,IFRSR(2,*),
     .         IFRS2(2,*),IRBE3(NRBE3L,*),LRBE3(*),NRBE3_FR,IFRS3(*),
     .         IDDMI3(6,M_MAX,*),IRBE2(NRBE2L,*),NRBE2_FR,IFRS4(*),
     .         IDDMI4(6,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ID,N,J,NDD,I1
      INTEGER M,NSN,JI,K10,K11,K12,K13,K14,L,NNOD,NJ,NL,NI,IAD
      INTEGER IERROR1,IERROR2,IERROR3,IERROR4,IERROR5,IERROR6
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
       IF (NML.GT.0) THEN
C--------initialization for ndof=0---	
        DO I = 1, NML
         N = IML(I)
         NDD = 0
         DO J = 1 , MIN(3,NDOF(N))
	  ID = IDDL(N) + J
          IF (IKC(ID).LT.1) THEN
           NDD = NDD + 1
           IDDML(J,I) = IDDLM(N) + NDD	  
          ELSE
           IDDML(J,I) = -IKC(ID)	  
          ENDIF
         ENDDO
        ENDDO
       ENDIF 
C
       IF (NRB_FR.GT.0) THEN
        DO I = 1, NRB_FR
         N = IFRSR(1,I)
         NDD = 0
         DO J = 1 , NDOF(N)
	  ID = IDDL(N) + J
          IF (IKC(ID).LT.1) THEN
           NDD = NDD + 1
           IDDMR(J,I) = IDDLM(N) + NDD	  
          ELSE
           IDDMR(J,I) = -IKC(ID)	  
          ENDIF
         ENDDO
        ENDDO
       ENDIF
C
       IF (NI2_FR.GT.0) THEN
        DO I=1,NI2_FR
         N=IFRS2(1,I)
         NI=IFRS2(2,I)
         JI=IPARI(1,N)
         NSN=IPARI(5,N)
         K10=JI-1
         K11=K10+4*IPARI(3,N)
C------IRECT(4,NSN)-----
         K12=K11+4*IPARI(4,N)
C------NSV(NSN)--node number---
         K13=K12+NSN
C------MSR(NMN)-----
         K14=K13+IPARI(6,N)
         L=INTBUF_TAB(N)%IRTLM(NI)
         NL=4*(L-1)
         IF (INTBUF_TAB(N)%IRECTM(NL+3).EQ.INTBUF_TAB(N)%IRECTM(NL+4)) THEN
          NNOD=3
         ELSE
          NNOD=4
         ENDIF
C-------si noeud main est dependant aussi-----
         DO M=1,NNOD
          NJ=INTBUF_TAB(N)%IRECTM(NL+M)
          NDD = 0
          DO J = 1 , NDOF(NJ)
	   ID = IDDL(NJ) + J
           IF (IKC(ID).LT.1) THEN
            NDD = NDD + 1
            IDDMI2(J,M,I) = IDDLM(NJ) + NDD	  
           ELSE
            IDDMI2(J,M,I) = -IKC(ID)	  
           ENDIF
          ENDDO
         ENDDO 
        ENDDO 
       ENDIF 
C-------RBE3-----------
       IF (NRBE3_FR.GT.0) THEN
        DO I=1,NRBE3_FR
         N=IFRS3(I)
         NI=IRBE3(3,N)
         NNOD=IRBE3(5,N)
         IAD=IRBE3(1,N)
C-------
         DO M=1,NNOD
          NJ=LRBE3(IAD+M)
          NDD = 0
          DO J = 1 , NDOF(NJ)
	     ID = IDDL(NJ) + J
           IF (IKC(ID).LT.1) THEN
            NDD = NDD + 1
            IDDMI3(J,M,I) = IDDLM(NJ) + NDD	  
           ELSE
            IDDMI3(J,M,I) = -IKC(ID)	  
           ENDIF
          ENDDO
         ENDDO 
        ENDDO 
       ENDIF 
C------RBE2
       IF (NRBE2_FR.GT.0) THEN
        DO I = 1, NRBE2_FR
         N = IFRSR(1,I)
         M = IRBE2(3,N)
         NDD = 0
         DO J = 1 , NDOF(M)
	  ID = IDDL(M) + J
          IF (IKC(ID).LT.1) THEN
           NDD = NDD + 1
           IDDMI4(J,I) = IDDLM(M) + NDD	  
          ELSE
           IDDMI4(J,I) = -IKC(ID)	  
          ENDIF
         ENDDO
        ENDDO
       ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  RBE3_MINT                     source/implicit/imp_int_k.F   
Chd|-- called by -----------
Chd|        KIN_KNL                       source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|        RBE3CL                        source/constraints/general/rbe3/rbe3f.F
Chd|====================================================================
      SUBROUTINE RBE3_MINT(IRBE3   ,LRBE3    ,FRBE3   ,X     ,SKEW,
     .                     NRBE3_KN,IRBE3_KN ,FRCDI   ,MRCDI )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IRBE3(NRBE3L,*),LRBE3(*) ,NRBE3_KN,IRBE3_KN(*)  
      my_real
     .  FRBE3(*),X(*),SKEW(*),FRCDI(*),MRCDI(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ID,N,J,NDD,I1,IAD,NMT,IROTG,IADS
      INTEGER M,NNOD,NJ,NL,NI
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
       IF (NRBE3_KN.GT.0) THEN
C------- init FRCDI,MRCDI
        NMT = SLRBE3/2
        IADS =1
        DO I=1,NRBE3_KN
         N=IRBE3_KN(I)
         NI=IRBE3(3,N)
         NNOD=IRBE3(5,N)
         IAD=IRBE3(1,N)
         IROTG =IRBE3(6,N)
         CALL RBE3CL(LRBE3(IAD+1),LRBE3(NMT+IAD+1),NI     ,X        ,
     .              FRBE3(IAD+1),SKEW    ,NNOD    ,IROTG  ,FRCDI(IADS),
     .              MRCDI(IADS) ,IRBE3(2,N))
C-------
         IADS = IADS + NNOD
        ENDDO 
       ENDIF 
C
      RETURN
      END
Chd|====================================================================
Chd|  INT_MATV                      source/implicit/imp_int_k.F   
Chd|-- called by -----------
Chd|        MAV_LT2                       source/implicit/produt_v.F    
Chd|-- calls ---------------
Chd|        IMP3_A2B                      source/airbag/monv_imp0.F     
Chd|        IMP3_U2X                      source/airbag/monv_imp0.F     
Chd|        INT_FKU3                      source/implicit/imp_int_k.F   
Chd|        ZEROR                         source/system/zero.F          
Chd|        IMP_KNON                      share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INT_MATV(IPARI  ,INTBUF_TAB     ,NDOF   ,NUM_IMP,
     1                    NS_IMP ,NE_IMP ,INDEX2 ,A      ,AR     ,
     2                    V      ,X      ,MS     ,X_IMP  ,IBFV   ,
     3                    SKEW   ,XFRAME ,U      ,F      ,IUPD   ,
     4                    IRBE3  ,LRBE3  ,IRBE2  ,LRBE2  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_KNON
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), INDEX2(*),NDOF(*)
      INTEGER NUM_IMP(*),NS_IMP(*),NE_IMP(*),IBFV(*),IUPD,
     .        IRBE3(*)  ,LRBE3(*),IRBE2(*)  ,LRBE2(*)
      my_real
     .   X(3,*),A(3,*),AR(3,*), F(*), U(*),
     .   X_IMP(3,*),V(3,*),SKEW(*) ,XFRAME(*),MS(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,N,K,K1,KK1,N1,N2,N3,N4,ID,IAD,IS,NN,NTY
      my_real
     .   D(3,NUMNOD)
C----------------actualise D,X_IMP---------------------
      CALL IMP3_U2X(X     ,IPARI  ,INTBUF_TAB    ,NDOF  ,
     .              U     ,D      ,AR     ,X_IMP ,NUMN_KN,
     .              IN_KN ,ID_KN  ,NRB_KN ,IRB_KN,ID_KNM ,
     .              NI2_KN,II2_KN ,ID_KNM2,NFX_KN,IFX_KN ,
     .              NBC_KN,IBC_KN ,NRW_KN,IRW_KN ,IBFV   ,
     .              SKEW  ,XFRAME ,IRBE3 ,LRBE3  ,NRBE3_KN,
     .              IRBE3_KN,ID_KNM3,RKN_MAX,FCDI_KN,MCDI_KN,
     .              NSPC_KN,ISPC_KN ,IRBE2 ,LRBE2  ,NRBE2_KN,
     .              IRBE2_KN,ID_KNM4)
      CALL ZEROR(A,NUMNOD)
      IF ((NRB_KN+NI2_KN+NRBE3_KN).GT.0) CALL ZEROR(AR,NUMNOD)
C----------------
      IF (IUPD.GT.0) THEN 
       CALL INT_FKU3(A      ,V      ,MS         ,D      ,
     1               IPARI  ,INTBUF_TAB    ,X_IMP,NUM_IMP,
     2               NS_IMP ,NE_IMP ,INDEX2,IUPD )
       CALL IMP3_A2B(IPARI  ,INTBUF_TAB    ,NDOF  ,X_IMP  ,
     1              A      ,AR     ,NUMN_KN,IN_KN,ID_KN  ,
     2              NRB_KN ,IRB_KN ,ID_KNM ,NI2_KN,II2_KN,
     3              ID_KNM2,NFX_KN ,IFX_KN ,NBC_KN,IBC_KN,
     4              NRW_KN ,IRW_KN ,IBFV   ,SKEW  ,XFRAME,
     5              F      ,IRBE3 ,LRBE3  ,NRBE3_KN,IRBE3_KN,
     6              ID_KNM3,RKN_MAX,FCDI_KN,MCDI_KN,NSPC_KN,
     7              ISPC_KN,IRBE2  ,LRBE2  ,NRBE2_KN,IRBE2_KN,
     .              ID_KNM4)
      ELSE 
       CALL INT_FKU3(A      ,V      ,MS         ,D      ,
     1               IPARI  ,INTBUF_TAB   ,X    ,NUM_IMP,
     2               NS_IMP ,NE_IMP ,INDEX2     ,IUPD   )
       CALL IMP3_A2B(IPARI  ,INTBUF_TAB     ,NDOF  ,X     ,
     1              A      ,AR     ,NUMN_KN,IN_KN,ID_KN  ,
     2              NRB_KN ,IRB_KN ,ID_KNM ,NI2_KN,II2_KN,
     3              ID_KNM2,NFX_KN ,IFX_KN ,NBC_KN,IBC_KN,
     4              NRW_KN ,IRW_KN ,IBFV   ,SKEW  ,XFRAME,
     5              F      ,IRBE3 ,LRBE3  ,NRBE3_KN,IRBE3_KN,
     6              ID_KNM3,RKN_MAX,FCDI_KN,MCDI_KN,NSPC_KN,
     7              ISPC_KN,IRBE2  ,LRBE2  ,NRBE2_KN,IRBE2_KN,
     .              ID_KNM4)
      ENDIF     
      RETURN
      END
Chd|====================================================================
Chd|  INT_MATVP                     source/implicit/imp_int_k.F   
Chd|-- called by -----------
Chd|        MAV_LTH                       source/implicit/produt_v.F    
Chd|        MAV_LTH0                      source/implicit/produt_v.F    
Chd|        MAV_LTP                       source/implicit/produt_v.F    
Chd|-- calls ---------------
Chd|        IMP3_A2B                      source/airbag/monv_imp0.F     
Chd|        IMP3_U2X                      source/airbag/monv_imp0.F     
Chd|        INT_FKU3                      source/implicit/imp_int_k.F   
Chd|        SPMD_IFCD                     source/mpi/implicit/imp_spmd.F
Chd|        SPMD_IFCF                     source/mpi/implicit/imp_spmd.F
Chd|        ZEROR                         source/system/zero.F          
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|        IMP_KNON                      share/modules/impbufdef_mod.F 
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INT_MATVP(IPARI  ,INTBUF_TAB  ,NDOF   ,NUM_IMP,
     1                    NS_IMP ,NE_IMP ,INDEX2 ,A      ,AR     ,
     2                    V      ,X      ,MS     ,X_IMP  ,IBFV   ,
     3                    SKEW   ,XFRAME ,U      ,F      ,DR     ,
     4                    NSREM  ,NSL    ,IUPD   ,IRBE3  ,LRBE3  ,
     5                    IRBE2  ,LRBE2  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_KNON
      USE IMP_INTM
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), INDEX2(*),NSREM  ,NSL
      INTEGER NUM_IMP(*),NS_IMP(*),NE_IMP(*),IBFV(*),IUPD,NDOF(*),
     .        IRBE3(*)  ,LRBE3(*),IRBE2(*)  ,LRBE2(*)
      my_real
     .   X(3,*),A(3,*),AR(3,*), F(*), U(*),
     .   X_IMP(3,*),SKEW(*) ,XFRAME(*),DR(3,*),V(3,*),MS(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,N,K,K1,KK1,N1,N2,N3,N4,ID,IAD,IS,NN,NTY
      my_real
     .   D(3,NUMNOD)
C----------------actualise D,X_IMP---------------------       
      CALL IMP3_U2X(X     ,IPARI  ,INTBUF_TAB ,NDOF  ,
     .              U     ,D      ,AR     ,X_IMP ,NUMN_KN,
     .              IN_KN ,ID_KN  ,NRB_KN ,IRB_KN,ID_KNM ,
     .              NI2_KN,II2_KN ,ID_KNM2,NFX_KN,IFX_KN ,
     .              NBC_KN,IBC_KN ,NRW_KN,IRW_KN ,IBFV   ,
     .              SKEW  ,XFRAME ,IRBE3 ,LRBE3  ,NRBE3_KN,
     .              IRBE3_KN,ID_KNM3,RKN_MAX,FCDI_KN,MCDI_KN,
     .              NSPC_KN,ISPC_KN ,IRBE2 ,LRBE2  ,NRBE2_KN,
     .              IRBE2_KN,ID_KNM4)
      CALL ZEROR(A,NUMNOD)
      IF ((NRB_KN+NI2_KN+NRBE3_KN).GT.0) CALL ZEROR(AR,NUMNOD)
C-----renvoie D (NSL) et receive DFI (NSREM)-----
      IF ((NSREM+NSL).GT.0) THEN
       CALL SPMD_IFCD(D  ,NSL, NSREM)
       IF (NSREM.GT.0) CALL ZEROR(FFI,NSREM)
      ENDIF
C----------------
      IF (IUPD.GT.0) THEN      
       CALL INT_FKU3(A      ,V      ,MS         ,D      ,
     1               IPARI  ,INTBUF_TAB,X_IMP,NUM_IMP,
     2               NS_IMP ,NE_IMP ,INDEX2,IUPD )
      IF ((NSREM+NSL).GT.0) CALL SPMD_IFCF(A, NSREM  ,NSL)
       CALL IMP3_A2B(IPARI  ,INTBUF_TAB ,NDOF  ,X_IMP  ,
     1              A      ,AR     ,NUMN_KN,IN_KN,ID_KN  ,
     2              NRB_KN ,IRB_KN ,ID_KNM ,NI2_KN,II2_KN,
     3              ID_KNM2,NFX_KN ,IFX_KN ,NBC_KN,IBC_KN,
     4              NRW_KN ,IRW_KN ,IBFV   ,SKEW  ,XFRAME,
     5              F      ,IRBE3 ,LRBE3  ,NRBE3_KN,IRBE3_KN,
     6              ID_KNM3,RKN_MAX,FCDI_KN,MCDI_KN,NSPC_KN,
     7              ISPC_KN,IRBE2  ,LRBE2  ,NRBE2_KN,IRBE2_KN,
     .              ID_KNM4)
      ELSE      
       CALL INT_FKU3(A      ,V      ,MS         ,D      ,
     1               IPARI  ,INTBUF_TAB   ,X    ,NUM_IMP,
     2               NS_IMP ,NE_IMP ,INDEX2     ,IUPD )
       IF ((NSREM+NSL).GT.0) CALL SPMD_IFCF(A, NSREM  ,NSL)
       CALL IMP3_A2B(IPARI  ,INTBUF_TAB ,NDOF  ,X     ,
     1              A      ,AR     ,NUMN_KN,IN_KN,ID_KN  ,
     2              NRB_KN ,IRB_KN ,ID_KNM ,NI2_KN,II2_KN,
     3              ID_KNM2,NFX_KN ,IFX_KN ,NBC_KN,IBC_KN,
     4              NRW_KN ,IRW_KN ,IBFV   ,SKEW  ,XFRAME,
     5              F      ,IRBE3 ,LRBE3  ,NRBE3_KN,IRBE3_KN,
     6              ID_KNM3,RKN_MAX,FCDI_KN,MCDI_KN,NSPC_KN,
     7              ISPC_KN,IRBE2  ,LRBE2  ,NRBE2_KN,IRBE2_KN,
     .              ID_KNM4)
      ENDIF     
      RETURN
      END
Chd|====================================================================
Chd|  INT_FKU3                      source/implicit/imp_int_k.F   
Chd|-- called by -----------
Chd|        INT_MATV                      source/implicit/imp_int_k.F   
Chd|        INT_MATVP                     source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|        I10FKU3                       source/interfaces/int10/i10ke3.F
Chd|        I11FKU3                       source/interfaces/int11/i11ke3.F
Chd|        I7FKU3                        source/interfaces/int07/i7ke3.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INT_FKU3(A      ,V      ,MS     ,D      ,IPARI  ,
     1                   INTBUF_TAB      ,X      ,NUM_IMP,NS_IMP ,
     1                    NE_IMP ,INDEX2 ,IUPD   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), INDEX2(*)
      INTEGER NUM_IMP(*),NS_IMP(*),NE_IMP(*),IUPD 
      my_real
     .   X(3,*),A(3,*),D(3,*), MS(*),V(3,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,N,K,K1,ID,IAD,IS,NN,NTY,I_INT7
C 
      IAD = 1     
C-----------int5 first-------------       
      DO N=1,NINTER
        NTY   =IPARI(7,N)
        IF(NTY.EQ.7) THEN
          CALL I7FKU3( A      ,V     ,MS     ,D        ,
     1       IPARI     ,INTBUF_TAB(N),X       ,N     ,
     2       NUM_IMP(N),NS_IMP(IAD),NE_IMP(IAD)   ,INDEX2(IAD),IUPD )
          IAD=IAD+NUM_IMP(N)
        ELSEIF(NTY.EQ.10)THEN
          CALL I10FKU3( A      ,V     ,MS     ,D        ,
     1       IPARI(1,N),INTBUF_TAB(N)         ,X       ,N     ,
     2       NUM_IMP(N),NS_IMP(IAD),NE_IMP(IAD)   ,INDEX2(IAD),IUPD )
          IAD=IAD+NUM_IMP(N)
        ELSEIF(NTY.EQ.11)THEN
          CALL I11FKU3( A      ,V     ,MS     ,D        ,
     1       IPARI(1,N),INTBUF_TAB(N) ,X       ,N     ,
     2       NUM_IMP(N),NS_IMP(IAD),NE_IMP(IAD)   ,IUPD )
          IAD=IAD+NUM_IMP(N)
        ENDIF
      ENDDO
c      IMP_INT7 = I_INT7
C      
      RETURN
      END
Chd|====================================================================
Chd|  PR_KINT                       source/implicit/imp_int_k.F   
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PR_KINT(NDDLI  ,IMCONV,
     3                  IADI  ,JDII  ,ITOK   ,DIAG_I ,LT_I   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc" 
#include      "scr05_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      INTEGER 
     .        NDDLI,IADI(*),JDII(*),ITOK(*),IMCONV
      my_real
     .  DIAG_I(*),LT_I(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
Ctmp +3
      INTEGER i,j,N,ID,ND,NKC,IDF,nnod,nk,iad,iad2,id2
      CHARACTER CHIF
      CHARACTER*10 FILNAME
C------
        IDF = ispmd+13
        WRITE(CHIF,'(I1)')ispmd
        FILNAME='KINT'//CHIF//'.TMP'
        OPEN(UNIT=IDF,FILE=FILNAME,STATUS='UNKNOWN',FORM='FORMATTED')
        write(IDF,*)'NDDLI,=', NDDLI
         if (IMCONV.lt.0) return
        write(IDF,*)'[Ki]=',NDDLI
       DO I =1,NDDLI
         write(IDF,*)'DIAG_I,itok=',DIAG_I(I),ITOK(I)
       ENDDO
       DO I =1,NDDLI
         write(IDF,*)'NR,I=',IADI(I+1)-IADI(I),I
        DO J=IADI(I),IADI(I+1)-1
         write(IDF,*)'LT_I,NJ,J=',LT_I(J),ITOK(JDII(J)),J
        ENDDO
       ENDDO
C------------------------------------------
      RETURN
      END
