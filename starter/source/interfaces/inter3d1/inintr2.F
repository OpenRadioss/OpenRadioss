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
Chd|  ININTR2                       source/interfaces/inter3d1/inintr2.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        I20STA                        source/interfaces/inter3d1/inintr2.F
Chd|        I24SURF_PXFEM                 source/interfaces/inter3d1/inintr2.F
Chd|        I2_DTN                        source/interfaces/inter3d1/i2_dtn.F
Chd|        I2_DTN_27                     source/interfaces/inter3d1/i2_dtn_27.F
Chd|        I2_DTN_28                     source/interfaces/inter3d1/i2_dtn_28.F
Chd|        INIEND                        source/interfaces/inter3d1/iniend.F
Chd|        INIEND2D                      source/interfaces/inter3d1/iniend.F
Chd|        INT2CY_CHK                    source/constraints/general/bcs/lecbcscyc.F
Chd|        INT2MODIF_ND                  source/elements/solid/solide10/dim_s10edg.F
Chd|        ITAGSL12                      source/interfaces/inter3d1/inintr2.F
Chd|        ITAGSL2                       source/interfaces/inter3d1/itagsl2.F
Chd|        REMN_I2OP                     source/interfaces/inter3d1/i7remnode.F
Chd|        REMN_I2_EDGOP                 source/interfaces/inter3d1/i7remnode.F
Chd|        REMN_SELF24                   source/interfaces/inter3d1/remn_self24.F
Chd|        RI2_INT24P_INI                source/interfaces/inter3d1/i7remnode.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUFSCRATCH_MOD             source/interfaces/interf1/intbufscratch_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ININTR2(IPARI   ,INSCR   ,X     ,
     .                   IXS     ,IXQ    ,IXC   ,PM      ,GEO   ,
     .                   INTC    ,ITAB   ,MS    ,NPBY    ,LPBY  ,
     .                   MWA     ,IKINE  ,I2NSNT  ,IN    ,
     .                   STIFN   ,STIFINT,NOM_OPT,INOD_PXFEM,MS_PLY,
     .                   INTBUF_TAB,STIFINTR,ITAGND,ICNDS10,MS_B,IN_B,
     .                   NSTRF   ,ITAGCYC,IRBE2  ,IRBE3  ,LRBE3 ,
     .                   KNOD2ELS ,NOD2ELS,IXS10  ,IXS16 ,IXS20,
     .                   S_NOD2ELS)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
      USE INTBUFSCRATCH_MOD
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, DIMENSION(6,NUMELS10)   ,INTENT(IN) :: IXS10
      INTEGER, DIMENSION(8,NUMELS16)   ,INTENT(IN) :: IXS16
      INTEGER, DIMENSION(12,NUMELS20)  ,INTENT(IN) :: IXS20
      INTEGER, DIMENSION(NUMNOD+1)     ,INTENT(IN) :: KNOD2ELS
      INTEGER,                          INTENT(IN) :: S_NOD2ELS
      INTEGER, DIMENSION(S_NOD2ELS)    ,INTENT(IN) :: NOD2ELS
      INTEGER, DIMENSION(NRBE2L,NRBE2), INTENT(IN) :: IRBE2
      INTEGER, DIMENSION(NRBE3L,NRBE3), INTENT(IN) :: IRBE3
      INTEGER, DIMENSION(SLRBE3), INTENT(IN) :: LRBE3
      INTEGER IPARI(NPARI,*), IXS(*), IXQ(*),
     .    IXC(*), INTC(*), ITAB(*), NPBY(*), LPBY(*), MWA(*),
     .    IKINE(*), I2NSNT,INOD_PXFEM(*),ITAGND(*),ICNDS10(3,*),
     .    NSTRF(*),ITAGCYC(*)
C     REAL
      my_real
     .  X(*), PM(*), GEO(*), MS(*), IN(*), STIFN(*),STIFINT(*),
     .  MS_PLY(*),STIFINTR(*),MS_B(*),IN_B(*)
      INTEGER NOM_OPT(LNOPT1,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(SCRATCH_STRUCT_) INSCR(*)

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, JINSCR, NINT, NTY,NRTM,NRTM0,
     .        NRTM_SH,INTPLY,KFI,ILEV,SWITCH,NSN,NREMN(NINTER),NINT2
      INTEGER ID,I,K,J1,J2
      CHARACTER*nchartitle,
     .   TITR
      INTEGER :: FLAG_OUTPUT !< flag for output
      INTEGER :: SKIP_TYPE25_EDGE_2_EDGE !< flag to activate only the computation of interface type 25 with edge to edge
C=======================================================================
C
C     SWITCH OF INTERF2 SPOTFLAG26 TO SPOTFLAG25 IF POSSIBLE 
C
      NINT2 = 0
      DO N=1,NINTER
        NINT=N
        ID=NOM_OPT(1,NINT)
        NTY = IPARI(7,N)
        IF (NTY == 2)  NINT2=NINT2+1
        ILEV  =IPARI(20,N)
        NSN   =IPARI(5,N)
        CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,NINT),LTITR)
        SWITCH = 1
        IF ((NTY==2).AND.(ILEV==26)) THEN
          IF (IRODDL==1) THEN
            DO I=1,NSN
              IF (IN(INTBUF_TAB(N)%NSV(I))>ZERO) SWITCH=0
            ENDDO
          ENDIF
          IF (SWITCH==1) THEN
            IPARI(20,N) = 25
            CALL ANCMSG(MSGID=1177,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR)
          ELSE
            CALL ANCMSG(MSGID=1178,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR)
          ENDIF
        ENDIF
      END DO
C
C
      DO N=1,NINTER
        NTY = IPARI(7,N)
C--     deactivated interface
        IF (NTY==0) CYCLE
C
        JINSCR=IPARI(10,N)
        NINT=N
       
        ID=NOM_OPT(1,NINT)
        CALL FRETITL2(TITR,
     .                NOM_OPT(LNOPT1-LTITR+1,NINT),LTITR)
        IF (N2D == 0) THEN
          CALL INIEND(
     1    INSCR(N)%WA  ,X            ,IXS  ,
     2    IXC          ,PM           ,GEO          ,IPARI(1,N),NINT ,
     3    INTC         ,ITAB         ,MS           ,NPBY      ,LPBY ,
     4    MWA          ,IKINE        ,IN        ,STIFINT,
     5    ID           ,TITR         ,INTBUF_TAB(N),STIFINTR )
        ELSE
          CALL INIEND2D(
     1          IPARI(1,N),NINT, MS, INTBUF_TAB(N)           )
        ENDIF
      ENDDO
C
C TAGAGE DESACTIVATION SECONDARY INTERF 12 AVEC CONDITION CINEMATIQUE
C
      DO N=1,NINTER
       NTY = IPARI(7,N)
       IF (NTY == 12) THEN
         CALL ITAGSL12(IPARI(1,N),ITAB,IKINE,INTBUF_TAB(N))
       ELSEIF(NTY==2)THEN
         I2NSNT = I2NSNT + IPARI(5,N)
       ENDIF
      ENDDO
C
C     SWITCH INTERF 2 AVEC CONDITION CINEMATIQUE => METHODE PENALITE
C
      CALL ITAGSL2(IPARI ,NOM_OPT,ITAB  ,IKINE  ,INTBUF_TAB,
     .                   ITAGND,ICNDS10,NSTRF ,ITAGCYC,IRBE2     ,
     .                   IRBE3 ,LRBE3  )
C
C LIMITATION RIGIDITE ANCRAGE
C
      DO N=1,NINTER
       NTY=IPARI(7,N)
       IF(NTY==20)THEN
         CALL I20STA(IPARI(1,N),STIFN,INTBUF_TAB(N))
       ENDIF
      ENDDO
C Xgement ply xfem
!!      IF(IPLYXFEM > 0) THEN
 
      IF(INTPLYXFEM > 0) THEN         
        DO N=1,NINTER
            NTY=IPARI(7,N)
            INTPLY = IPARI(66,N)
           IF(NTY == 24 .AND. INTPLY > 0 )THEN
             NRTM_SH   = IPARI(42,N)
             NRTM = IPARI(4,N)
             NRTM0   = NRTM - NRTM_SH
             CALL I24SURF_PXFEM(NRTM,INTBUF_TAB(N)%IRECTM,INOD_PXFEM,INTPLY,
     .              INTBUF_TAB(N)%NVOISIN,INTBUF_TAB(N)%MSEGTYP24,MS_PLY ,
     .              INTBUF_TAB(N)%ISEG_PXFEM,INTBUF_TAB(N)%ISEG_PLY)
          ENDIF
        ENDDO      
      ENDIF
C
C     DYNAMIC CONDENSATION FOR TETRA10 - MIDDLE NODES REMOVED FROM INTERFACE
C
      IF (NS10E>0) CALL INT2MODIF_ND(IPARI,INTBUF_TAB,ITAGND,ICNDS10,ITAB)
C     /BCS/CYCLIC      
      IF (NBCSCYC>0) CALL INT2CY_CHK(IPARI,INTBUF_TAB,ITAGCYC,ITAB)
C
C     UPDATE OF NODAL TIME STEP FOR ADDED MASS ESTIMATION
C
      DO N=1,NINTER
        NTY = IPARI(7,N)
        ILEV  =IPARI(20,N)
        NSN   =IPARI(5,N)
        IF (NTY==2) THEN
          IF (ILEV==27) THEN
            CALL I2_DTN_27(X,INTBUF_TAB,IPARI,STIFN,MS_B,IN_B,N,NSN)
          ELSEIF (ILEV==28) THEN
            CALL I2_DTN_28(X,INTBUF_TAB,IPARI,STIFN,MS_B,IN_B,N,NSN)
          ELSEIF ((ILEV==0).OR.(ILEV==1).OR.(ILEV==25)) THEN
            CALL I2_DTN(X,INTBUF_TAB,IPARI,STIFN,MS_B,IN_B,N,NSN,ILEV)
          ENDIF
        ENDIF
      END DO
C
       IF (N2D==0 ) THEN 
         NREMN(1:NINTER) = 0    
         FLAG_OUTPUT = 1   
         SKIP_TYPE25_EDGE_2_EDGE = 0
         IF (NINT2 >0) CALL REMN_I2OP(1,NINTER,IPARI   ,INTBUF_TAB   ,ITAB, NOM_OPT,NREMN,FLAG_OUTPUT,SKIP_TYPE25_EDGE_2_EDGE)
         CALL REMN_SELF24(X   ,IXS   ,IXS10 ,IXS16,IXS20   ,
     .                KNOD2ELS,NOD2ELS,IPARI ,INTBUF_TAB ,
     .                ITAB , NOM_OPT,NREMN, S_NOD2ELS,FLAG_OUTPUT)
         CALL REMN_I2_EDGOP(IPARI   ,INTBUF_TAB   ,ITAB, NREMN)
         CALL RI2_INT24P_INI(IPARI  ,INTBUF_TAB   ,ITAB, NOM_OPT,NREMN )
       END IF !(NINT2 >0) THEN   
C-----------
      RETURN
      END
C
Chd|====================================================================
Chd|  ITAGSL12                      source/interfaces/inter3d1/inintr2.F
Chd|-- called by -----------
Chd|        ININTR2                       source/interfaces/inter3d1/inintr2.F
Chd|-- calls ---------------
Chd|        KINSET                        source/constraints/general/kinset.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ITAGSL12(IPARI,ITAB,IKINE,INTBUF_TAB)
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
#include      "com04_c.inc"
#include      "kincod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER IPARI(*),ITAB(*),IKINE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NRTS,NRTM,NSN,NMN,NST,MST,I,NSV,NOINT,
     .  JWARN,KWARNO,CAS1,CAS2,
     .  IKINE1(3*NUMNOD)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C
C=======================================================================

      NRTS  =IPARI(3)
      NRTM  =IPARI(4)
      NSN   =IPARI(5)
      NMN   =IPARI(6)
      NST   =IPARI(8)
      MST   =IPARI(9)
      NOINT =IPARI(15)
      JWARN=0
      KWARNO=KWARN
C
      DO I=1,3*NUMNOD
        IKINE1(I) = 0
      ENDDO
C
      WRITE(IOUT,2000)NOINT
      DO I=1,NSN
      NSV=INTBUF_TAB%NSV(I)
C Cas1 corps rigide ou interface 2
C Cas3 Condition au limite en translation
      CAS1=ITF(IKINE(NSV))+IRB(IKINE(NSV))
      CAS2=CAS1+
     .  IBC(IKINE(NSV))+IKINE(NSV+NUMNOD) - 10*IKINE(NSV+NUMNOD)/10
c        IF(ITF(IKINE(NSV))/=0 .OR. IRB(IKINE(NSV))/=0 )THEN
c
c si SECONDARY d'une autre interface 2 ou 9 ou 12 ou rby on tag le SECONDARY pour dsactivation
c
        IF(     (IPARI(11)==3 .AND. CAS2/=0)
     .     .OR. (IPARI(11)==2 .AND. CAS1/=0) )THEN
           JWARN=1
           INTBUF_TAB%FCOUNT(I)=-ABS(INTBUF_TAB%FCOUNT(I))
           WRITE(IOUT,1000)ITAB(NSV),NOINT
        ELSE
           CALL KINSET(2,ITAB(NSV),IKINE(NSV),1,0,IKINE1(NSV))
           CALL KINSET(2,ITAB(NSV),IKINE(NSV),2,0,IKINE1(NSV))
           CALL KINSET(2,ITAB(NSV),IKINE(NSV),3,0,IKINE1(NSV))
        ENDIF
      ENDDO
      IF(JWARN>0)THEN
        IF(IPARI(11)==2)WRITE(IOUT,1010)NOINT
        IF(IPARI(11)==3)WRITE(IOUT,1011)NOINT
        IWARN=IWARN+1
      ENDIF
      IF(KWARN-KWARNO ==0) THEN
         WRITE(IOUT,2010)NOINT
      ELSE
         WRITE(IOUT,'(//)')
      ENDIF
C----------- 
      RETURN
C----------------------------------------------------------------------- 
 1000 FORMAT(' SECONDARY NODE ',I10,
     +       ' IS DEACTIVATED FROM FLUID INTERFACE ',I8)
 1010 FORMAT(' *** WARNING SOME NODES'
     +       ' BELONGING TO ANOTHER INTERFACE TYPE 2, 9 or 12,',
     +       ' OR TO A RIGID BODY',
     +       ' WERE DEACTIVATED FROM FLUID INTERFACE ',I8)
 1011 FORMAT(' *** WARNING SOME NODES'
     +       ' BELONGING TO ANOTHER INTERFACE TYPE 2, 9 or 12,',
     +       ' OR TO A RIGID BODY OR WITH A TRANSLATIONAL B.C.',
     +       ' WERE DEACTIVATED FROM FLUID INTERFACE ',I8)
 2000 FORMAT(//,
     +       ' CHECKING KINEMATIC CONDITIONS ON INTERFACE TYPE 12',I8)
 2010 FORMAT(' NO PROBLEM FOUND IN INTERFACE',I10,/)
C----------------------------------------------------------------------- 
      END
Chd|====================================================================
Chd|  I20STA                        source/interfaces/inter3d1/inintr2.F
Chd|-- called by -----------
Chd|        ININTR2                       source/interfaces/inter3d1/inintr2.F
Chd|-- calls ---------------
Chd|        I20STAB                       source/interfaces/inter3d1/inintr2.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I20STA(IPARI,STIFN,INTBUF_TAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(*)
      my_real
     .     STIFN(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NLN
C
      NLN   = IPARI(35)
C
      CALL I20STAB(NLN,INTBUF_TAB%NLG,INTBUF_TAB%STFA,STIFN)

      RETURN
C----------------------------------------------------------------------- 
      END
Chd|====================================================================
Chd|  I20STAB                       source/interfaces/inter3d1/inintr2.F
Chd|-- called by -----------
Chd|        I20STA                        source/interfaces/inter3d1/inintr2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I20STAB(NLN,NLG,STFA,STIFN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NLN,NLG(*)
      my_real
     .     STIFN(NUMNOD),STFA(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C
      DO I=1,NLN
        IF(STIFN(NLG(I)) /= EM20.and.STIFN(NLG(I))<STFA(I))
     .        STFA(I) = STIFN(NLG(I))
      ENDDO
C----------- 
      RETURN
C----------------------------------------------------------------------- 
      END
Chd|====================================================================
Chd|  I24SURF_PXFEM                 source/interfaces/inter3d1/inintr2.F
Chd|-- called by -----------
Chd|        ININTR2                       source/interfaces/inter3d1/inintr2.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I24SURF_PXFEM(NRTM  ,IRECT,INOD_PXFEM,INTPLY,
     .                         NVOISIN, MSEGTYP ,MS_PLY , ISEG_PXFEM,
     .                         ISEG_PLY)
C      
      USE MESSAGE_MOD
C-----------------------------------------------------------------------
C     LECTURE DES SURFACES ET DECOMPTE DES SEGMENTS
C           ENTREE : 
C                    NRT NOMBRE DE RENSEIGNEMENTS A LIRE
C           SORTIE : 
C                    IRECT 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM, INTPLY
      INTEGER IRECT(4,*),  INOD_PXFEM(*),ISEG_PXFEM(*),NVOISIN(8,*),
     .        ISEG_PLY(12,*),MSEGTYP(*)
      my_real
     .        MS_PLY(NPLYXFE,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,K,J1,J2,J3,J4,J5,J6,J7,J8,J9,J10,J11,J12,NN,ILY
      INTEGER IXX(NRTM,12)
      
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
C=======================================================================  
      DO J=1,NRTM  
          ISEG_PXFEM(J) = 0                      
          DO K=1,4  
             IXX(J,K) = IRECT(K,J)  
            IF(INOD_PXFEM(IRECT(K,J)) > 0) THEN
              INTPLY = 1
               ISEG_PXFEM(J) = 1           
            ENDIF 
          ENDDO 
          
          
C voisin 
         IXX(J,5) = IABS(NVOISIN(1,J))
         IXX(J,6) = IABS(NVOISIN(2,J))
         IXX(J,7) = IABS(NVOISIN(3,J))
         IXX(J,8) = IABS(NVOISIN(4,J))
         IXX(J,9) = IABS(NVOISIN(5,J))
         IXX(J,10)= IABS(NVOISIN(6,J))
         IXX(J,11)= IABS(NVOISIN(7,J))
         IXX(J,12)= IABS(NVOISIN(8,J))     
                        
      ENDDO  
C Impacted ply node
        DO I=1,NRTM
          ISEG_PLY(1,I) = 0
          ISEG_PLY(2,I) = 0
          ISEG_PLY(3,I) = 0
          ISEG_PLY(4,I) = 0
                      
          ISEG_PLY(5,I) = 0
          ISEG_PLY(6,I) = 0
          ISEG_PLY(7,I) = 0
          ISEG_PLY(8,I) = 0 
          ISEG_PLY(9,I) = 0
          ISEG_PLY(10,I) = 0
          ISEG_PLY(11,I) = 0
          ISEG_PLY(12,I) = 0
          IF(ISEG_PXFEM(I) == 0) CYCLE
C or by node segment if it needed            
 !!          ISEG(I) = 0

          IF(MSEGTYP(I) == ZERO) CYCLE  
C                          
          IF(MSEGTYP(I) > 0) THEN
           
!!               ISEG(I) = 1
             J1 = IXX(I,1)
             IF(J1 > 0) THEN   
               NN = INOD_PXFEM(J1)                
               IF(NN > 0) THEN         
                    ILY = NPLYMAX                     
                    DO WHILE(MS_PLY(NN,ILY) == ZERO)  
                      ILY = ILY - 1                    
                    ENDDO                             
                    ISEG_PLY(1,I) = ILY                    
               ENDIF 
             ENDIF  
             J1 = IXX(I,2)
             IF(J1 > 0) THEN
               NN = INOD_PXFEM(J1)
               IF(NN > 0) THEN
                  ILY = NPLYMAX
                  DO WHILE(MS_PLY(NN,ILY) == ZERO) 
                     ILY = ILY - 1
                  ENDDO
                  ISEG_PLY(2,I) = ILY
               ENDIF 
             ENDIF  
             J1 = IXX(I,3)
             IF(J1 > 0) THEN
               NN = INOD_PXFEM(J1)
               IF(NN > 0) THEN
                 ILY = NPLYMAX
                 DO WHILE(MS_PLY(NN,ILY) == ZERO) 
                   ILY = ILY - 1
                 ENDDO    
                 ISEG_PLY(3,I) = ILY
               ENDIF 
             ENDIF     
             J1= IXX(I,4)
             IF(J1 > 0) THEN
              NN = INOD_PXFEM(J1)
              IF(NN > 0) THEN
                 ILY = NPLYMAX
                 DO WHILE(MS_PLY(NN,ILY) == ZERO)
                  ILY = ILY - 1
                 ENDDO 
                 ISEG_PLY(4,I) = ILY 
               ENDIF
             ENDIF  
C voisin
             J5 = IXX(I,5)
             IF(J5 > 0) THEN
                 NN = INOD_PXFEM(J5)                
                 IF(NN > 0) THEN         
                      ILY = NPLYMAX                   
                      DO WHILE(MS_PLY(NN,ILY) == ZERO)
                        ILY = ILY - 1                  
                      ENDDO                           
                      ISEG_PLY(5,I) = ILY                  
                 ENDIF 
               ENDIF 
               J6 = IXX(I,6)
               IF(J6 > 0) THEN
                 NN = INOD_PXFEM(J6)                
                 IF(NN > 0) THEN         
                      ILY = NPLYMAX                   
                      DO WHILE(MS_PLY(NN,ILY) == ZERO)
                        ILY = ILY - 1                  
                      ENDDO                           
                      ISEG_PLY(6,I) = ILY                  
                 ENDIF 
               ENDIF 
C               
               J7 = IXX(I,7)
               IF(J7 > 0) THEN
                 NN = INOD_PXFEM(J7)                
                 IF(NN > 0) THEN         
                      ILY = NPLYMAX                   
                      DO WHILE(MS_PLY(NN,ILY) == ZERO)
                        ILY = ILY - 1                  
                      ENDDO                           
                      ISEG_PLY(7,I) = ILY                  
                 ENDIF 
               ENDIF           
               J8 = IXX(I,8)
               IF(J8 > 0) THEN
                 NN = INOD_PXFEM(J8)                
                 IF(NN > 0) THEN         
                      ILY = NPLYMAX                   
                      DO WHILE(MS_PLY(NN,ILY) == ZERO)
                        ILY = ILY - 1                  
                      ENDDO                           
                      ISEG_PLY(8,I) = ILY                  
                 ENDIF 
               ENDIF
                J9 = IXX(I,9)
               IF(J9 > 0) THEN
                 NN = INOD_PXFEM(J9)                
                 IF(NN > 0) THEN         
                      ILY = NPLYMAX                   
                      DO WHILE(MS_PLY(NN,ILY) == ZERO)
                        ILY = ILY - 1                  
                      ENDDO                           
                      ISEG_PLY(9,I) = ILY                  
                 ENDIF 
               ENDIF
                J10 = IXX(I,10)
               IF(J10 > 0) THEN
                 NN = INOD_PXFEM(J10)                
                 IF(NN > 0) THEN         
                      ILY = NPLYMAX                   
                      DO WHILE(MS_PLY(NN,ILY) == ZERO)
                        ILY = ILY - 1                  
                      ENDDO                           
                      ISEG_PLY(10,I) = ILY                  
                 ENDIF 
               ENDIF
                 J11 = IXX(I,11)
               IF(J11 > 0) THEN
                 NN = INOD_PXFEM(J11)                
                 IF(NN > 0) THEN         
                      ILY = NPLYMAX                   
                      DO WHILE(MS_PLY(NN,ILY) == ZERO)
                        ILY = ILY - 1                  
                      ENDDO                           
                      ISEG_PLY(11,I) = ILY                  
                 ENDIF 
               ENDIF   
                J12 = IXX(I,12)
               IF(J12 > 0) THEN
                 NN = INOD_PXFEM(J12)                
                 IF(NN > 0) THEN         
                      ILY = NPLYMAX                   
                      DO WHILE(MS_PLY(NN,ILY) == ZERO)
                        ILY = ILY - 1                  
                      ENDDO                           
                      ISEG_PLY(12,I) = ILY                  
                 ENDIF 
               ENDIF  
           ELSE ! 
                
!!               ISEG(I) = 2
             J1= IXX(I,1)
             IF(J1 > 0) THEN
               NN = INOD_PXFEM(J1)
               IF(NN > 0) THEN
                    ILY = 1
                    DO WHILE(MS_PLY(NN,ILY) == ZERO)
                     ILY = ILY + 1
                    ENDDO
                    ISEG_PLY(1,I) = ILY
               ENDIF
             ENDIF
C  
             J1= IXX(I,2)
             IF(J1 > 0) THEN
               NN = INOD_PXFEM(J1)                
               IF(NN > 0) THEN                    
                  ILY = 1                         
                  DO WHILE(MS_PLY(NN,ILY) == ZERO)
                     ILY = ILY + 1                
                  ENDDO                           
                  ISEG_PLY(2,I) = ILY                 
                ENDIF                             
             ENDIF   
C
             J1= IXX(I,3) 
             IF(J1 > 0) THEN
               NN = INOD_PXFEM(J1)
               IF(NN > 0) THEN
                  ILY = 1
                  DO WHILE(MS_PLY(NN,ILY) == ZERO)
                    ILY = ILY + 1
                  ENDDO
                  ISEG_PLY(3,I) = ILY
               ENDIF 
             ENDIF
             J1= IXX(I,4)
             IF(J1 > 0) THEN
               NN = INOD_PXFEM(J1)
               IF(NN > 0) THEN
                    ILY = 1
                    DO WHILE(MS_PLY(NN,ILY) == ZERO)
                     ILY = ILY + 1
                    ENDDO
                    ISEG_PLY(4,I) = ILY
               ENDIF 
             ENDIF
C
C voisin
               J5 = IXX(I,5)
               IF(J5 > 0) THEN
                 NN = INOD_PXFEM(J5)                
                 IF(NN > 0) THEN         
                    ILY = 1
                    DO WHILE(MS_PLY(NN,ILY) == ZERO)
                     ILY = ILY + 1
                    ENDDO                         
                    ISEG_PLY(5,I) = ILY                  
                 ENDIF 
               ENDIF 
               J6 = IXX(I,6)
               IF(J6 > 0) THEN
                 NN = INOD_PXFEM(J6)                
                 IF(NN > 0) THEN         
                      ILY = 1
                      DO WHILE(MS_PLY(NN,ILY) == ZERO)
                       ILY = ILY + 1                   
                      ENDDO                         
                      ISEG_PLY(6,I) = ILY                  
                 ENDIF 
               ENDIF 
C               
               J7 = IXX(I,7)
               IF(J7 > 0) THEN
                 NN = INOD_PXFEM(J7)                
                 IF(NN > 0) THEN         
                      ILY = 1
                      DO WHILE(MS_PLY(NN,ILY) == ZERO)
                       ILY = ILY + 1                   
                      ENDDO                         
                      ISEG_PLY(7,I) = ILY                  
                 ENDIF 
               ENDIF                
               J8 = IXX(I,8)
               IF(J8 > 0) THEN
                 NN = INOD_PXFEM(J8)                
                 IF(NN > 0) THEN         
                      ILY = 1
                      DO WHILE(MS_PLY(NN,ILY) == ZERO)
                       ILY = ILY + 1                   
                      ENDDO                         
                      ISEG_PLY(8,I) = ILY                  
                 ENDIF 
               ENDIF
                J9 = IXX(I,9)
               IF(J9 > 0) THEN
                 NN = INOD_PXFEM(J9)                
                 IF(NN > 0) THEN         
                      ILY = 1
                      DO WHILE(MS_PLY(NN,ILY) == ZERO)
                       ILY = ILY + 1
                      ENDDO                         
                      ISEG_PLY(9,I) = ILY                  
                 ENDIF 
               ENDIF
                J10 = IXX(I,10)
               IF(J10 > 0) THEN
                 NN = INOD_PXFEM(J10)                
                 IF(NN > 0) THEN         
                      ILY = 1
                      DO WHILE(MS_PLY(NN,ILY) == ZERO)
                       ILY = ILY + 1                   
                      ENDDO                         
                      ISEG_PLY(10,I) = ILY                  
                 ENDIF 
               ENDIF
                 J11 = IXX(I,11)
               IF(J11 > 0) THEN
                 NN = INOD_PXFEM(J11)                
                 IF(NN > 0) THEN         
                      ILY = 1
                      DO WHILE(MS_PLY(NN,ILY) == ZERO)
                       ILY = ILY + 1                   
                      ENDDO                         
                      ISEG_PLY(11,I) = ILY                  
                 ENDIF 
               ENDIF   
                J12 = IXX(I,12)
               IF(J12 > 0) THEN
                 NN = INOD_PXFEM(J12)                
                 IF(NN > 0) THEN         
                      ILY = 1
                      DO WHILE(MS_PLY(NN,ILY) == ZERO)
                       ILY = ILY + 1                   
                      ENDDO                         
                      ISEG_PLY(12,I) = ILY                  
                 ENDIF 
               ENDIF 
C                  
            ENDIF
        ENDDO  
C             
C------------------------------------------------------------
      RETURN
      END
 
 
 
 
