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
C I17MAIN_LAG split in 3 routines for SPMD reorganization
C nouvelle routine ne contenant plus que l'appel du critere de tri
Chd|====================================================================
Chd|  I17MAIN_CRIT_TRI              source/interfaces/int17/i17main_pena.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        I17CRIT                       source/interfaces/int17/i17crit.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I17MAIN_CRIT_TRI(
     1          IPARI  ,INTBUF_TAB ,X       ,NIN    ,
     2          ITASK  ,IGRBRIC    ,EMINX   ,NME    ,
     3          NMES   ,XSLV  _L   ,XMSR_L  ,SIZE_T ,IXS    ,
     4          IXS16  ,IXS20      )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD    
      USE GROUPDEF_MOD
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C
C-------------------------------------------------------------------------------
C     NOM    DIMENSION               DESCRIPTION                       E/S
C-------------------------------------------------------------------------------
C
C    NIN    1                        NUMERO INTERFACE                   E
C                  
C   IPARI   NPARI,NINTER             PARAMETRES D'INTERFACE             E
C                  
C     X     3,NUMNOD                 COORDONNEES                        E
C                  
C     V     3,NUMNOD                 VITESSES                           E
C                  
C    EMINX  6*(NME+NMES)           MIN MAX DE CHAQUE ELEMENT        TMP_GLOBAL
C                  
C    WAT    4*(NME+100)+4*(NMES+100) TABLEAUX DE TRI                  TMP_TASK
C                  
C-----------------------------------------------
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
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN,ITASK ,NC,N_MUL_MX ,NKMAX , NME, NMES  
      INTEGER IPARI(NPARI,NINTER),
     .        IXS(NIXS,*)  ,IXS16(8,*)  ,IXS20(12,*)
C     REAL ou REAL*8
      my_real 
     .        X(3,*), EMINX(6,*), SIZE_T(*),
     .        XSLV_L(*), XMSR_L(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NAD,EAD,LWAT,
     .        IGN,IGE,MULTIMP,NOINT,I,MX_CAND 
C     REAL ou REAL*8
      my_real
     .        STARTT, STOPT,XX,XY,XZ,TZINF,MINBOX,
     .        AZERO(3,NUMNOD)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO I=1,NUMNOD
        AZERO(1,I) = ZERO
        AZERO(2,I) = ZERO
        AZERO(3,I) = ZERO
      ENDDO
      MULTIMP     = IPARI(23,NIN)
      IGN         = IPARI(36,NIN)
      IGE         = IPARI(34,NIN)
      MX_CAND=MULTIMP*IPARI(18,NIN)
      STARTT = INTBUF_TAB%VARIABLES(3)
      STOPT  = INTBUF_TAB%VARIABLES(11)
      IF(STARTT.GT.TT) RETURN
      IF(TT.GT.STOPT)  RETURN
C -------------------------------------------------------------
C=======================================================================
C      TEST pour savoir s'il faut retrier + initialisation
C=======================================================================
C       SIZE_T(NIN)=ZERO init dans inttri
       CALL I17CRIT(
     1   X     ,AZERO	     ,AZERO	   ,IGRBRIC(IGE)%ENTITY ,EMINX        ,
     2   NME   ,ITASK	     ,INTBUF_TAB%XSAV,IXS 	 ,IXS16        ,
     3   IXS20 ,XMSR_L       ,SIZE_T(NIN))
C -------------------------------------------------------------
C       CALL MY_BARRIER   inutile
C -------------------------------------------------------------
       CALL I17CRIT(
     1   X     ,AZERO        ,AZERO        ,IGRBRIC(IGN)%ENTITY,EMINX(1,NME+1),
     2   NMES  ,ITASK        ,INTBUF_TAB%XSAV,IXS         ,IXS16         ,
     3   IXS20 ,XSLV_L       ,SIZE_T(NIN)  )
C
      RETURN
      END
C
C nouvelle routine ne contenant plus que l'appel du tri
Chd|====================================================================
Chd|  I17MAIN_TRI                   source/interfaces/int17/i17main_pena.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I17BUCE_PENA                  source/interfaces/int17/i17buce.F
Chd|        I17FROT                       source/interfaces/int17/i17main_pena.F
Chd|        I17XSAVE                      source/interfaces/int17/i17xsave.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_TRI17BOX                 source/mpi/interfaces/spmd_tri17box.F
Chd|        SPMD_TRI17GAT                 source/mpi/interfaces/spmd_i7crit.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I17MAIN_TRI(
     1          IPARI  ,INTBUF_TAB ,X       ,NIN     ,
     2          ITASK  ,IGRBRIC    ,NME     ,NMES    ,
     3          EMINX  ,IXS        ,IXS16  ,IXS20   ,WEIGHT  ,
     3          ISENDTO,IRECVFROM  ,RETRI  ,IAD_ELEM,FR_ELEM ,
     4          ITAB   ,V          ,NME_T  ,ESH_T   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD   
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C
C-------------------------------------------------------------------------------
C     NOM    DIMENSION               DESCRIPTION                       E/S
C-------------------------------------------------------------------------------
C
C    NIN    1                        NUMERO INTERFACE                   E
C                  
C   IPARI   NPARI,NINTER             PARAMETRES D'INTERFACE             E
C                  
C     X     3,NUMNOD                 COORDONNEES                        E
C                  
C     V     3,NUMNOD                 VITESSES                           E
C                  
C    EMINX  6*NME<6*NUMELS           MIN MAX DE CHAQUE ELEMENT        TMP_GLOBAL
C                  
C    WAT    4*(NME+100)+4*(NMES+100) TABLEAUX DE TRI                  TMP_TASK
C                  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
#include      "warn_c.inc" 
#include      "scr17_c.inc"
      COMMON /I17MAINC/BMINMA,NMESR
      INTEGER NMESR
      my_real 
     .        BMINMA(6)
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN,ITASK , NME, NMES, RETRI, NME_T, ESH_T
      INTEGER IPARI(NPARI,NINTER),
     .        IXS(NIXS,*)  ,IXS16(8,*)  ,IXS20(12,*),WEIGHT(*),
     .        ISENDTO(*),IRECVFROM(*),IAD_ELEM(*),FR_ELEM(*),
     .        ITAB(*)   
C     REAL ou REAL*8
      my_real 
     .    X(3,*), V(3,*), EMINX(6,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NAD,EAD,LWAT,RESULT,
     .        IGN,IGE,MULTIMP,NOINT,I,MX_CAND, NB_N_B
C     REAL ou REAL*8
      my_real
     .   STARTT, STOPT,XX,XY,XZ,TZINF,MINBOX,DIST,
     .   XMAXL, YMAXL, ZMAXL, XMINL, YMINL, ZMINL,
     .   AZERO(3,NUMNOD)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO I=1,NUMNOD
        AZERO(1,I) = ZERO
        AZERO(2,I) = ZERO
        AZERO(3,I) = ZERO
      ENDDO
      NOINT       = IPARI(15,NIN)
      MULTIMP     = IPARI(23,NIN)
      IGN         = IPARI(36,NIN)
      IGE         = IPARI(34,NIN)
      LWAT = 4*(NME+100)+4*(NMES+100)
C
      MX_CAND=MULTIMP*IPARI(18,NIN)
      STARTT = INTBUF_TAB%VARIABLES(3)
      STOPT  = INTBUF_TAB%VARIABLES(11)
      IF(STARTT.GT.TT) RETURN
      IF(TT.GT.STOPT)  RETURN
C -------------------------------------------------------------
C -------------------------------------------------------------
C-----------------------------------------------
C     frottement (deplace de i7for3 a ici pour maj avant echange SPMD)
C-----------------------------------------------
      CALL I17FROT(
     1  ITASK,INTBUF_TAB%FROTM,INTBUF_TAB%FROTS,NME,NMES)
C=======================================================================
C      TEST pour savoir s'il faut retrier + initialisation
C=======================================================================
      DIST = INTBUF_TAB%VARIABLES(5)
      CALL MY_BARRIER
C -------------------------------------------------------------
      IF(DIST.LT.ZERO) THEN
         RETRI=1
         TZINF  = INTBUF_TAB%VARIABLES(8)
         MINBOX = INTBUF_TAB%VARIABLES(12)
         INTBUF_TAB%VARIABLES(5) = ABS(INTBUF_TAB%VARIABLES(5))
C BMINMA inverse par rapport aux autres interfaces de contact
         BMINMA(1)=EP30
         BMINMA(2)=EP30
         BMINMA(3)=EP30
         BMINMA(4)=-EP30
         BMINMA(5)=-EP30
         BMINMA(6)=-EP30
C=======================================================================
C        TRI
C=======================================================================
         IPARI(35,NIN) = 0
C BARRIER INIT BMINMA
         CALL MY_BARRIER
C -------------------------------------------------------------
C     CALCUL BORNE DOMAINE REMONTE DANS I17XSAVE
C -------------------------------------------------------------
	 CALL I17XSAVE(
     1       X     ,V	  ,AZERO	,IXS	     ,IXS16,
     2       NMES  ,NME_T ,ESH_T	,IGRBRIC(IGE)%ENTITY,IGRBRIC(IGN)%ENTITY,
     3       EMINX ,ITASK ,INTBUF_TAB%XSAV,XMINL       ,YMINL	  ,
     3       ZMINL ,XMAXL ,YMAXL	,ZMAXL  		  )
#include "lockon.inc"
         BMINMA(1) = MIN(BMINMA(1),XMINL)
         BMINMA(2) = MIN(BMINMA(2),YMINL)
         BMINMA(3) = MIN(BMINMA(3),ZMINL)
         BMINMA(4) = MAX(BMINMA(4),XMAXL)
         BMINMA(5) = MAX(BMINMA(5),YMAXL)
         BMINMA(6) = MAX(BMINMA(6),ZMAXL)
#include "lockoff.inc"
C BARRIER BMINMA
         CALL MY_BARRIER
         IF(ITASK.EQ.0)THEN
           IF(ABS(BMINMA(6)-BMINMA(3)).GT.2*EP30.OR.
     +        ABS(BMINMA(5)-BMINMA(2)).GT.2*EP30.OR.
     +        ABS(BMINMA(4)-BMINMA(1)).GT.2*EP30)THEN
             CALL ANCMSG(MSGID=87,ANMODE=ANINFO,
     .            I1=NOINT)
             CALL ARRET(2)
           END IF
           BMINMA(1) = BMINMA(1) - TZINF
           BMINMA(2) = BMINMA(2) - TZINF
           BMINMA(3) = BMINMA(3) - TZINF
           BMINMA(4) = BMINMA(4) + TZINF
           BMINMA(5) = BMINMA(5) + TZINF
           BMINMA(6) = BMINMA(6) + TZINF
           NMESR = 0
           IF(NSPMD.GT.1)THEN
C -------------------------------------------------------------
C recuperation des noeuds remote NMESR stockes dans XREM
C -------------------------------------------------------------
             CALL SPMD_TRI17BOX(
     1         IGRBRIC(IGN)%ENTITY  ,NMES    ,X    ,V    ,INTBUF_TAB%FROTS ,
     2         INTBUF_TAB%KS,BMINMA ,WEIGHT,NIN  ,ISENDTO       ,
     3         IRECVFROM    ,NMESR  ,IXS   ,IXS16,EMINX(1,NME+1))
           END IF
         END IF
         NB_N_B = 1
C -------------------------------------------------------------
         CALL MY_BARRIER
C -------------------------------------------------------------
         CALL I17BUCE_PENA(
     1   IGRBRIC(IGN)%ENTITY,IXS   ,IXS16  ,IXS20        ,IGRBRIC(IGE)%ENTITY,
     2   NME_T      ,LWAT          ,NMES   ,INTBUF_TAB%CAND_E,
     .   INTBUF_TAB%CAND_N,
     3   NOINT      ,IPARI(35,NIN) ,TZINF  ,MINBOX       ,EMINX        ,
     4   INTBUF_TAB%XSAV,ITASK       ,X      ,AZERO        ,AZERO        ,
     5   MX_CAND    ,EMINX(1,NME+1),ESH_T  ,INTBUF_TAB%FROTS,INTBUF_TAB%KS,
     6   NIN        ,NMESR         ,NB_N_B ,BMINMA       )
C -------------------------------------------------------------
         CALL MY_BARRIER
C -------------------------------------------------------------
         IF(NSPMD.GT.1)THEN
!$OMP SINGLE
          IF (IMONM > 0) CALL STARTIME(26,1)
          INTBUF_TAB%VARIABLES(5) = -INTBUF_TAB%VARIABLES(5)
          RESULT = 0
C comm SPMD apres tri (compactage)
          CALL SPMD_TRI17GAT(
     1      RESULT ,NMES ,INTBUF_TAB%CAND_N,IPARI(35,NIN),NIN,
     2      NMESR  )
C sauvegarde des candidats additionnels dans IPARI(24)  pas besoin car pas de inacti
c          IPARI(24,NIN) = NMESR
          IF (IMONM > 0) CALL STOPTIME(26,1)
!$OMP END SINGLE

         END IF
      ENDIF
C
      RETURN
      END
C
C nouvelle routine ne contenant plus que le traitment de force
Chd|====================================================================
Chd|  I17MAIN_PENA                  source/interfaces/int17/i17main_pena.F
Chd|-- called by -----------
Chd|        I7MAINF                       source/interfaces/int07/i7mainf.F
Chd|-- calls ---------------
Chd|        I17FOR3                       source/interfaces/int17/i17for3.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I17MAIN_PENA(
     1          NIN    ,IPARI  ,INTBUF_TAB    ,X      ,V      ,
     2          A      ,ITASK  ,IGRBRIC ,EMINX  ,MS     ,
     3          IXS    ,IXS16  ,IXS20   ,STIFN  ,FSKYI  ,ISKY   ,
     4          FSAV   ,FCONT  ,NISKYFI ,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD   
      USE H3D_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C
C-------------------------------------------------------------------------------
C     NOM    DIMENSION               DESCRIPTION                       E/S
C-------------------------------------------------------------------------------
C
C    NIN    1                        NUMERO INTERFACE                   E
C                  
C   IPARI   NPARI,NINTER             PARAMETRES D'INTERFACE             E
C                  
C     X     3,NUMNOD                 COORDONNEES                        E
C                  
C     V     3,NUMNOD                 VITESSES                           E
C                  
C    EMINX  6*NME<6*NUMELS           MIN MAX DE CHAQUE ELEMENT        TMP_GLOBAL
C                  
C                  
C-----------------------------------------------
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
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN,ITASK,NISKYFI   
      INTEGER IPARI(NPARI,NINTER),
     .        IXS(NIXS,*)  ,IXS16(8,*)  ,IXS20(12,*) ,ISKY(*)
C     REAL ou REAL*8
      my_real 
     .   X(3,*), V(3,*), A(3,*), MS(*),EMINX(6,*),STIFN(*),
     .   FSKYI(*), FSAV(*),FCONT(3,*)   

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NME,NAD,EAD,
     .        IGN,IGE,MULTIMP,NOINT,I,MX_CAND,NMES,LSKYI17
C     REAL ou REAL*8
      my_real
     .        STARTT, STOPT
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      NOINT       = IPARI(15,NIN)
      MULTIMP     = IPARI(23,NIN)
      IGN         = IPARI(36,NIN)
      IGE         = IPARI(34,NIN)
      NMES  =IGRBRIC(IGN)%NENTITY
      NME   =IGRBRIC(IGE)%NENTITY
C
      IF(IMACH.EQ.3)THEN
        MX_CAND=MULTIMP*IPARI(18,NIN)
      ELSE
        MX_CAND=MULTIMP*NMES
      END IF
      IF(IPARIT.NE.0)THEN
C        LSKYI17=MX_CAND
C LSKYI17 fonction de IMAXIMP : nombre d'impacts max defini dans starter/lecint
        LSKYI17=MAX(MX_CAND,MULTIMP*IPARI(5,NIN)*8)
      ELSE
        LSKYI17=0
      END IF
      STARTT = INTBUF_TAB%VARIABLES(3)
      STOPT  = INTBUF_TAB%VARIABLES(11)
      IF(STARTT.GT.TT) RETURN
      IF(TT.GT.STOPT)  RETURN
C -------------------------------------------------------------
C -------------------------------------------------------------
       CALL MY_BARRIER 
C=======================================================================
C      CONTACT
C=======================================================================
       CALL I17FOR3(
     1   X       ,V         ,INTBUF_TAB%CAND_N,INTBUF_TAB%CAND_E,IPARI(35,NIN) ,
     2   IXS     ,IXS16     ,EMINX    ,IGRBRIC(IGN)%ENTITY,IGRBRIC(IGE)%ENTITY,
     3   ITASK   ,A         ,IPARI(30,NIN),NIN          ,EMINX(1,NME+1),
     4   STIFN   ,FSKYI     ,ISKY         ,NME          ,NMES    ,
     5   INTBUF_TAB%FROTM,INTBUF_TAB%FROTS,INTBUF_TAB%KM,INTBUF_TAB%KS,
     5                                                 INTBUF_TAB%VARIABLES(1) ,
     6   FSAV    ,FCONT     ,MS           ,NISKYFI      ,LSKYI17       ,
     7   NOINT   ,H3D_DATA)
C -------------------------------------------------------------
       CALL MY_BARRIER
C -------------------------------------------------------------
      RETURN
      END
C
Chd|====================================================================
Chd|  I17FROT                       source/interfaces/int17/i17main_pena.F
Chd|-- called by -----------
Chd|        I17MAIN_TRI                   source/interfaces/int17/i17main_pena.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I17FROT(ITASK,FROTM,FROTS,NME,NSE)
C-----------------------------------------------
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
#include      "task_c.inc"
#include      "warn_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITASK,NME,NSE
C     REAL ou REAL*8
      my_real
     .        FROTM(7,*),FROTS(7,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NMEF,NMEL,NSEF,NSEL
      my_real
     .   AA
C-----------------------------------------------
      NMEF = 1 + ITASK*NME / NTHREAD
      NMEL = (ITASK+1)*NME / NTHREAD
      NSEF = 1 + ITASK*NSE / NTHREAD
      NSEL = (ITASK+1)*NSE / NTHREAD
C-----------------------------------------------
C     frottement (ancienne contrainte de frottement)
C-----------------------------------------------
      DO I=NMEF,NMEL
        AA = ONE/MAX(EM20,FROTM(4,I))
        FROTM(5,I)=AA*FROTM(1,I)
        FROTM(6,I)=AA*FROTM(2,I)
        FROTM(7,I)=AA*FROTM(3,I)
C
        FROTM(1,I)=ZERO
        FROTM(2,I)=ZERO
        FROTM(3,I)=ZERO
        FROTM(4,I)=ZERO
      ENDDO
C
      DO I=NSEF,NSEL
        AA = ONE/MAX(EM20,FROTS(4,I))
        FROTS(5,I)=AA*FROTS(1,I)
        FROTS(6,I)=AA*FROTS(2,I)
        FROTS(7,I)=AA*FROTS(3,I)
C
        FROTS(1,I)=ZERO
        FROTS(2,I)=ZERO
        FROTS(3,I)=ZERO
        FROTS(4,I)=ZERO
      ENDDO
C
      RETURN
      END
