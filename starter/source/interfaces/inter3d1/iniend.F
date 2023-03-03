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
Chd|  INIEND                        source/interfaces/inter3d1/iniend.F
Chd|-- called by -----------
Chd|        ININTR2                       source/interfaces/inter3d1/inintr2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        I6PEN3                        source/interfaces/inter3d1/i6pen3.F
Chd|        I6STI3                        source/interfaces/inter3d1/i6sti3.F
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        ININT0                        source/interfaces/interf1/inint0.F
Chd|        INVOI3                        source/interfaces/inter3d1/invoi3.F
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE INIEND(INSCR,X    ,IXS  ,
     .                  IXC  ,PM   ,GEO  ,IPARI,NOIN ,
     .                  INTC ,ITAB ,MS   ,NPBY ,LPBY ,
     .                  MWA  ,IKINE,IN   ,STIFINT,
     .                  ID   ,TITR ,INTBUF_TAB,STIFINTR  )
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE MESSAGE_MOD
      USE FRONT_MOD
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOIN
      INTEGER INSCR(*), IXS(*), IXC(*), IPARI(*), INTC(*),
     .        ITAB(*), NPBY(*), LPBY(*), MWA(*), IKINE(*)
C     REAL
      my_real
     .   X(*), PM(*), GEO(*), MS(*), IN(*), STIFINT(*),STIFINTR(*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NRTS, NRTM, NSN, NMN, NTY, NST, NMT, IBUC, NOINT,
     .   IWPENE, I, INCREM, P,
     .   ILEV,ICOR,II,JJ,NIR,K,N,L,N1,N2,N3,N4,INACTI,IGSTI
      integer j
C     REAL
      my_real
     .   XMAS1, XMAS2,STFN,STFR
C=======================================================================
      IWPENE = 0
      NRTS  =IPARI(3)
      NRTM  =IPARI(4)
      NSN   =IPARI(5)
      NMN   =IPARI(6)
      NTY   =IPARI(7)
      NST   =IPARI(8)
      NMT   =IPARI(9)
      IBUC  =IPARI(12)
      NOINT =IPARI(15)
      INACTI=IPARI(22)
      ICOR  =IPARI(58)
C
      IF (NSPMD > 1) THEN
C      les interfaces type 7 sont distribuees, les autres centralisees sur P0
       INCREM = 1
C pas de NSN defini en type 16 et interface non spmd compatible
       IF(NTY==16.OR.NTY==17) RETURN
C
       IF(NTY==7.OR.NTY==10.OR.NTY==11.OR.
     .    NTY==20.OR.NTY==21.OR.NTY==22.OR.
     .    NTY==23.OR.NTY==24.OR.NTY==25) INCREM = 100
C
       IF(NTY==8) THEN
         DO P=1,NSPMD
           DO I=1,NSN
         !Comment ne donner les SECONDARYs qu'aux procs
         
             CALL IFRONTPLUS(INTBUF_TAB%NSV(I),P)
           END DO
         ENDDO
C
       ELSEIF(NTY/=2) THEN
	     IF(INCREM==1)THEN
            DO I=1,NSN
	           CALL IFRONTPLUS(INTBUF_TAB%NSV(I),1)
            END DO
            DO I=1,NMN
               CALL IFRONTPLUS(INTBUF_TAB%MSR(I),1)	   
            END DO
	     ENDIF
C  interface ALE -> c. c. traitees par P0
         IF(NTY==1.OR.NTY==9.OR.NTY==12)THEN
           INCREM = 10
           DO I=1,NSN
             !set FLAGKIN to 1 for boundary node with
	            !kinematic constraints (old FRONT TAG=10)	   
             FLAGKIN(INTBUF_TAB%NSV(I)) = 1
           END DO
           DO I=1,NMN
             FLAGKIN(INTBUF_TAB%MSR(I)) = 1
           END DO
         ENDIF
       ELSE
         IF (N2D==0) THEN
           NIR = 4
         ELSE
           NIR = 2
         ENDIF
         DO II=1,NSN
           L = INTBUF_TAB%IRTLM(II)
           N = INTBUF_TAB%NSV(II)
           DO P = 1, NSPMD
             IF (NLOCAL(N,P)==0) THEN
               GO TO 200
             ENDIF
c            pas d optimisation possible
 100         DO JJ=1,NIR
               K = INTBUF_TAB%IRECTM((L-1)*4+JJ)
               CALL IFRONTPLUS(K,P)
             ENDDO
c            optimisation possible
 200         CONTINUE
           ENDDO
         ENDDO
       ENDIF
      ENDIF
C
C
      IF(NTY==6) THEN
C
      !flushed between 2 domain decomposition
      INTBUF_TAB%LNSV(1:NST) = 0
      INTBUF_TAB%LMSR(1:NMT) = 0
      INTBUF_TAB%STFNS(1:NSN) = 0

       WRITE(IOUT,2001)NOINT,NTY
       CALL ININT0(X,INTBUF_TAB%IRECTS,INTBUF_TAB%NSEGS,INTBUF_TAB%LNSV,INTBUF_TAB%NSV,
     1               INTBUF_TAB%MSR,INTBUF_TAB%ILOCM,NMN,NSN,NRTS)
       CALL ININT0(X,INTBUF_TAB%IRECTM,INTBUF_TAB%NSEGM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,
     1               INTBUF_TAB%NSV,INTBUF_TAB%ILOCS,NSN,NMN,NRTM)
       CALL I6STI3(INTBUF_TAB%IRECTS,INTBUF_TAB%STFS,NRTS,INTBUF_TAB%STFNS,NSN,
     1             INTBUF_TAB%NSV,XMAS1,MS,NPBY,LPBY,NOINT,ITAB,ID,TITR)
       CALL I6STI3(INTBUF_TAB%IRECTM,INTBUF_TAB%STFM,NRTM,INTBUF_TAB%STFNM,NMN,
     1             INTBUF_TAB%MSR,XMAS2,MS,NPBY,LPBY,NOINT,ITAB,ID,TITR)
       INTBUF_TAB%VARIABLES(4)= MIN(XMAS1,XMAS2)
       CALL INVOI3(X,INTBUF_TAB%IRECTM,INTBUF_TAB%LMSR,INTBUF_TAB%MSR,INTBUF_TAB%NSV,
     1               INTBUF_TAB%ILOCS,INTBUF_TAB%IRTLM,INTBUF_TAB%NSEGM,NSN,NMN,
     2               ITAB,ID,TITR,NRTM)
       CALL INVOI3(X,INTBUF_TAB%IRECTS,INTBUF_TAB%LNSV,INTBUF_TAB%NSV,INTBUF_TAB%MSR,
     1               INTBUF_TAB%ILOCM,INTBUF_TAB%IRTLS,INTBUF_TAB%NSEGS,NMN,NSN,
     2               ITAB,ID,TITR,NRTS)
       WRITE(IOUT,2002)
       CALL I6PEN3
     1   (X         ,INTBUF_TAB%IRECTM,INTBUF_TAB%MSR,INTBUF_TAB%NSV  ,INTBUF_TAB%ILOCS,
     2    INTBUF_TAB%IRTLM,INTBUF_TAB%CSTS,INTBUF_TAB%IRTLOM,INTBUF_TAB%VARIABLES(2),NSN	 ,
     3    ITAB      ,IWPENE    ,INTBUF_TAB%FCONT,ICOR        ,ID,
     4    INACTI    ,TITR)
       WRITE(IOUT,2003)
       CALL I6PEN3
     1   (X         ,INTBUF_TAB%IRECTS,INTBUF_TAB%NSV,INTBUF_TAB%MSR  ,INTBUF_TAB%ILOCM,
     2    INTBUF_TAB%IRTLS,INTBUF_TAB%CSTM,INTBUF_TAB%IRTLOS,INTBUF_TAB%VARIABLES(2),NMN	 ,
     3    ITAB      ,IWPENE    ,INTBUF_TAB%FCONT,ICOR        ,ID,
     4    INACTI    ,TITR)
C
      ENDIF
C
      IF(IWPENE/=0) THEN
         CALL ANCMSG(MSGID=342,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=ID,
     .               C1=TITR,
     .               I2=IWPENE)
      ENDIF
C
C     Init MMASS int 2
C
      IF (NTY == 2) THEN
        ILEV = IPARI(20)
        DO II = 1, NMN
          I = INTBUF_TAB%MSR(II)
          INTBUF_TAB%NMAS(II)     = MS(I)
          IF (IRODDL == 1) INTBUF_TAB%NMAS(NMN+II) = IN(I)
        ENDDO
        IF (ILEV == 10 .OR. ILEV == 11 .OR. ILEV == 12 .OR.
     .      ILEV == 20 .OR. ILEV == 21 .OR. ILEV == 22) THEN
          DO II = 1, NSN
            I = INTBUF_TAB%NSV(II)
            INTBUF_TAB%SMAS(II)  = MS(I)
            IF (IRODDL == 1) INTBUF_TAB%SINER(II) = IN(I)
          ENDDO
        ELSEIF (ILEV == 25) THEN
          IGSTI = IPARI(58)
          DO II = 1, NSN
            I  = INTBUF_TAB%NSV(II)    ! NSV
            L  = INTBUF_TAB%IRTLM(II)  ! IRTL
C
            INTBUF_TAB%SMAS(II) = MS(I)
            IF (IRODDL == 1) INTBUF_TAB%SINER(II) = IN(I)
            N1 = INTBUF_TAB%IRECTM((L-1)*4+1)
            N2 = INTBUF_TAB%IRECTM((L-1)*4+2)
            N3 = INTBUF_TAB%IRECTM((L-1)*4+3)
            N4 = INTBUF_TAB%IRECTM((L-1)*4+4)
            IF (N3 == N4) THEN
              STFN=THIRD*(STIFINT(N1)+STIFINT(N2)+STIFINT(N3))
            ELSE
              STFN=FOURTH*(STIFINT(N1)+STIFINT(N2)+STIFINT(N3)+STIFINT(N4))
            ENDIF
            SELECT CASE (IGSTI)
            CASE (2)  ! mean stiffness (default)
              STFN = HALF*(STFN+STIFINT(I))
            CASE (3)  ! max stiffness (default)
              STFN = MAX(STFN,STIFINT(I))
            CASE (4)  ! min stiffness (default)
              STFN = MIN(STFN,STIFINT(I))
            CASE (5)  ! min stiffness (default)
              STFN = STFN*STIFINT(I) / (STFN+STIFINT(I))
            CASE DEFAULT  ! MAIN stiffness
              CONTINUE
            END SELECT
            INTBUF_TAB%SPENALTY(II) = STFN*INTBUF_TAB%STFAC(1)
          ENDDO
        ELSEIF (ILEV == 26) THEN
          IGSTI = IPARI(58)
          DO II = 1, NSN
            I  = INTBUF_TAB%NSV(II)    ! NSV
            L  = INTBUF_TAB%IRTLM(II)  ! IRTL
C
            INTBUF_TAB%SMAS(II) = MS(I)
            IF (IRODDL == 1) INTBUF_TAB%SINER(II) = IN(I)
            N1 = INTBUF_TAB%IRECTM((L-1)*4+1)
            N2 = INTBUF_TAB%IRECTM((L-1)*4+2)
            N3 = INTBUF_TAB%IRECTM((L-1)*4+3)
            N4 = INTBUF_TAB%IRECTM((L-1)*4+4)
            IF (N3 == N4) THEN
              STFN=THIRD*(STIFINT(N1)+STIFINT(N2)+STIFINT(N3))
            ELSE
              STFN=FOURTH*(STIFINT(N1)+STIFINT(N2)+STIFINT(N3)+STIFINT(N4))
            ENDIF
            SELECT CASE (IGSTI)
            CASE (2)  ! mean stiffness (default)
              STFN = HALF*(STFN+STIFINT(I))
            CASE (3)  ! max stiffness (default)
              STFN = MAX(STFN,STIFINT(I))
            CASE (4)  ! min stiffness (default)
              STFN = MIN(STFN,STIFINT(I))
            CASE (5)  ! min stiffness (default)
              STFN = STFN*STIFINT(I) / (STFN+STIFINT(I))
            CASE DEFAULT  ! MAIN stiffness
              CONTINUE
            END SELECT
            INTBUF_TAB%SPENALTY(II) = STFN
          ENDDO
        ELSEIF ((ILEV == 27).OR.(ILEV == 28)) THEN
          IGSTI = IPARI(58)
          DO II = 1, NSN
            I  = INTBUF_TAB%NSV(II)    ! NSV
            L  = INTBUF_TAB%IRTLM(II)  ! IRTL
C
            INTBUF_TAB%SMAS(II) = MS(I)
            IF (IRODDL == 1) INTBUF_TAB%SINER(II) = IN(I)
            N1 = INTBUF_TAB%IRECTM((L-1)*4+1)
            N2 = INTBUF_TAB%IRECTM((L-1)*4+2)
            N3 = INTBUF_TAB%IRECTM((L-1)*4+3)
            N4 = INTBUF_TAB%IRECTM((L-1)*4+4)
            IF (N3 == N4) THEN
              STFN=THIRD*(STIFINT(N1)+STIFINT(N2)+STIFINT(N3))
              STFR=THIRD*(STIFINTR(N1)+STIFINTR(N2)+STIFINTR(N3))             
            ELSE
              STFN=FOURTH*(STIFINT(N1)+STIFINT(N2)+STIFINT(N3)+STIFINT(N4))
              STFR=FOURTH*(STIFINTR(N1)+STIFINTR(N2)+STIFINTR(N3)+STIFINTR(N4))
            ENDIF
            SELECT CASE (IGSTI)
            CASE (2)  ! mean stiffness (default)
              STFN = HALF*(STFN+STIFINT(I))
              STFR = HALF*(STFR+STIFINTR(I))
            CASE (3)  ! max stiffness (default)
              STFN = MAX(STFN,STIFINT(I))
              STFR = MAX(STFR,STIFINTR(I))
            CASE (4)  ! min stiffness (default)
              STFN = MIN(STFN,STIFINT(I))
              STFR = MIN(STFR,STIFINTR(I))
            CASE (5)  ! min stiffness (default)
              STFN = STFN*STIFINT(I) / MAX(EM20,(STFN+STIFINT(I)))
              STFR = STFR*STIFINTR(I) / MAX(EM20,(STFR+STIFINTR(I)))
            CASE DEFAULT  ! MAIN stiffness
              CONTINUE
            END SELECT
            INTBUF_TAB%SPENALTY(II) = STFN*INTBUF_TAB%STFAC(1)
            INTBUF_TAB%STFR_PENALTY(II) = STFR*INTBUF_TAB%STFAC(1)
          ENDDO
        ENDIF
      ENDIF
C-----------
      RETURN
C-----------------------------------------------------------------------
 2001 FORMAT(//,1X,'INTERFACE NUMBER. . . . . . . . . . . . . .',I10/
     +      ,1X,'INTERFACE TYPE. . . . . . . . . . . . . . .',I6/)
 2002 FORMAT(//
     +'   SECONDARY NEAREST  NEAREST        MAIN NODES                       SECONDARY '/
     +'   NODE   MAIN  SEGMENT                                      S               T')
 2003 FORMAT(//
     +'  MAIN NEAREST  NEAREST        SECONDARY  NODES                       MAIN'/
     +'   NODE    SECONDARY  SEGMENT                                      S               T')
C-----------------------------------------------------------------------
      END

C nouvelle routine servant uniquement a l initialisation de front pour le spmd
Chd|====================================================================
Chd|  INIEND2D                      source/interfaces/inter3d1/iniend.F
Chd|-- called by -----------
Chd|        ININTR2                       source/interfaces/inter3d1/inintr2.F
Chd|-- calls ---------------
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INIEND2D(IPARI,NOIN,MS,INTBUF_TAB )
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE FRONT_MOD  
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOIN
      INTEGER IPARI(*)
      my_real
     .   MS(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NRTS, NRTM, NSN, NMN, NTY, NOINT,
     .   I, INCREM, P, II,JJ,NIR,K,N,L
C=======================================================================
      NRTS  =IPARI(3)
      NRTM  =IPARI(4)
      NSN   =IPARI(5)
      NMN   =IPARI(6)
      NTY   =IPARI(7)
      NOINT =IPARI(15)
C
      IF(IMACH==3.AND.NSPMD>1) THEN
C      les interfaces type 7 sont distribuees, les autres centralisees sur P0
       INCREM = 1
       IF(NTY==7.OR.NTY==10.OR.NTY==11.OR.NTY==22) INCREM = 100
C
       IF(NTY/=2) THEN
         IF(INCREM==1)THEN
            DO I=1,NSN
               CALL IFRONTPLUS(INTBUF_TAB%NSV(I),1)
            END DO
            DO I=1,NMN
               CALL IFRONTPLUS(INTBUF_TAB%MSR(I),1)
            END DO
         ENDIF
C interface ALE -> c. c. traitees par P0
         IF(NTY==1.OR.NTY==9.OR.NTY==12)THEN
           INCREM = 10
           DO I=1,NSN
              !set FLAGKIN to 1 for boundary node with
	      !kinematic constraints (old FRONT TAG=10)
              FLAGKIN(INTBUF_TAB%NSV(I)) = 1
           END DO
           DO I=1,NMN
             FLAGKIN(INTBUF_TAB%MSR(I)) = 1
           END DO
         ENDIF
       ELSE
         IF (N2D==0) THEN
           NIR = 4
         ELSE
           NIR = 2
         ENDIF
         DO II=1,NSN
           L = INTBUF_TAB%IRTLM(II)
           N = INTBUF_TAB%NSV(II)
           DO P = 1, NSPMD
             IF (NLOCAL(N,P)==0) THEN
               GO TO 200
             ENDIF
C pas d optimisation possible
 100         DO JJ=1,NIR
               K = INTBUF_TAB%IRECTM((L-1)*4+JJ)
	           CALL IFRONTPLUS(K,P)
             ENDDO
C optimisation possible
 200         CONTINUE
           ENDDO
         ENDDO
       ENDIF
      ENDIF
C
C Init MMASS / MINER int 2
C
      IF (NTY == 2) THEN
        DO II = 1, NMN
          I = INTBUF_TAB%MSR(II)
          INTBUF_TAB%NMAS(II)     = MS(I)
        ENDDO
      ENDIF
C
      RETURN
      END
