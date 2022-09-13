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
Chd|  I20SURFI                      source/interfaces/inter3d1/i20surfi.F
Chd|-- called by -----------
Chd|        LECINS                        source/interfaces/interf1/lecins.F
Chd|        LECINT                        source/interfaces/interf1/lecint.F
Chd|-- calls ---------------
Chd|        I20BORD                       source/interfaces/inter3d1/i20surfi.F
Chd|        I20EDGE1                      source/interfaces/inter3d1/i20surfi.F
Chd|        BITSET                        source/interfaces/inter3d1/bitget.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE I20SURFI(IALLO   ,IPARI   ,IGRNOD  ,IGRSURF ,
     2                    IGRSLIN ,IRECT   ,FRIGAP  ,
     3                    NSV     ,MSR     ,IXLINS  ,IXLINM  ,
     4                    NSVE    ,MSRE    ,ITAB    ,ISLINS  ,
     5                    ISLINM  ,NLG     ,X       ,NBINFLG ,
     6                    MBINFLG )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
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
#include      "com01_c.inc"
#include      "com02_c.inc"
#include      "com03_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr12_c.inc"
#include      "scr17_c.inc"
#include      "conv_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IALLO
      INTEGER IPARI(NPARI),
     .        IRECT(4,*), NSV(*),IXLINS(2,*),
     .        IXLINM(2,*),MSR(*),ITAB(*),NSVE(*),MSRE(*),
     .        ISLINS(2,*),ISLINM(2,*),NLG(*),NBINFLG(*),MBINFLG(*)
      my_real
     .   X(3,*),FRIGAP(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE (SURF_)   , DIMENSION(NSLIN)   :: IGRSLIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,LL,NL,ISU1,ISU2,NOD1,NRTM,NRTS,NSN,NMN,
     .        NLINSA,NLINMA,ISYM,IEDGE,NSNE,NMNE,NLN,
     .        NLINS,NLINM,LINE1,LINE2,STAT,IL,IG
      INTEGER TAG(NUMNOD),TAGS(NUMNOD),NEXTK(4),IWORK(70000),
     .        LNTAG(NUMNOD),TAGB(NUMNOD)
      my_real
     .   EDG_COS
      DATA NEXTK/1,1,1,-3/
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER BITSET
      EXTERNAL BITSET
C
      CHARACTER MESS*40
      DATA MESS/'INTERFACE INPUT                         '/

      NSN   = 0
      NMN   = 0
      NRTM  = 0
      NRTS  = 0
      NLINS = 0
      NLINM = 0
      NLINSA= 0
      NLINMA= 0
      NSNE  = 0
      NMNE  = 0
      NOD1  = IPARI(26)
      NLN   = 0
      ISYM  = IPARI(43)
      ISU1  = IPARI(45)
      ISU2  = IPARI(46)
      IEDGE = IPARI(58)
      LINE1 = IPARI(59)
      LINE2 = IPARI(60)
      EDG_COS = FRIGAP(26)
C=======================================================================
c     SURFACES
C=======================================================================
c-----------------------------------------------------------------
c     surface S1
c-----------------------------------------------------------------
      IF(ISU1 /= 0) NRTM = IGRSURF(ISU1)%NSEG
c-----------------------------------------------------------------
c     surface S2
c-----------------------------------------------------------------
      IF(ISU2 /= 0) NRTS = IGRSURF(ISU2)%NSEG
      
      IF(ISYM == 1) NRTM = NRTM + NRTS

c---------------------------------------
c     copie des surfaces (IALLO == 2)
c---------------------------------------
      IF(IALLO == 2)THEN
        IF(ISU1 /= 0)THEN
          L = 0
          DO J=1,IGRSURF(ISU1)%NSEG
            L = L+1
            DO K=1,4
              IRECT(K,L) = IGRSURF(ISU1)%NODES(J,K)
            ENDDO
            MBINFLG(L) = BITSET(MBINFLG(L),0)
          ENDDO
        ENDIF
        IF(ISU2 /= 0 .and. ISYM == 1)THEN
          DO J=1,IGRSURF(ISU2)%NSEG
            L = L+1
            DO K=1,4
              IRECT(K,L) = IGRSURF(ISU2)%NODES(J,K)
            ENDDO
            MBINFLG(L) = BITSET(MBINFLG(L),1)
          ENDDO
        ENDIF
        IF(IPRI>=1) THEN
          WRITE(IOUT,'(/,A,/)')' SEGMENTS USED FOR SURFACE DEFINITION'
          DO I=1,NRTM
            WRITE(IOUT,FMT=FMW_4I)(ITAB(IRECT(K,I)),K=1,4)
          ENDDO      
        ENDIF
      ENDIF
C=======================================================================
c     NOEUDS
C=======================================================================
c-----------------------------------------------------------------
c     tag noeuds surfaces S1 S2
c-----------------------------------------------------------------
      DO I=1,NUMNOD
        TAG(I)=0 ! initialisation 
        TAGS(I)=0 ! initialisation 
        TAGB(I)=0 ! initialisation 
        LNTAG(I)=0 ! initialisation 
      ENDDO
      IF(ISU2 /= 0)THEN
        DO J=1,IGRSURF(ISU2)%NSEG
          DO K=1,4
            TAG(IGRSURF(ISU2)%NODES(J,K)) = 2
            LNTAG(IGRSURF(ISU2)%NODES(J,K)) = 1
          ENDDO
        ENDDO
      ENDIF
      IF(ISU1 /= 0)THEN
        DO J=1,IGRSURF(ISU1)%NSEG
          DO K=1,4
            I=IGRSURF(ISU1)%NODES(J,K)
            IF(TAG(I) == 0)THEN
              TAG(I) = 1
            ELSEIF(TAG(I) == 2)THEN
              TAG(I) = 3
            ENDIF
            LNTAG(I) = 1
          ENDDO
        ENDDO
      ENDIF
c-----------------------------------------------------------------
c     noeuds de la surface S2
c-----------------------------------------------------------------
      IF(ISU2 /= 0)THEN
        DO J=1,IGRSURF(ISU2)%NSEG
          DO K=1,4
            I=IGRSURF(ISU2)%NODES(J,K)
            IF(TAG(I) == 2 .and. ISYM == 1)THEN
              NMN = NMN + 1
              IF(IALLO == 2)MSR(NMN) = I
              TAGB(I) = BITSET(TAGB(I),4)
            ENDIF
            IF(TAG(I) == 2 .or. TAG(I) == 3)THEN
              TAG(I) = - TAG(I)
              TAGS(I) = 1
              NSN = NSN + 1
              IF(IALLO == 2)NSV(NSN) = I
              TAGB(I) = BITSET(TAGB(I),1)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
c-----------------------------------------------------------------
c     noeuds de la surface S1 si ISYM /= 2
c-----------------------------------------------------------------
      IF(ISU1 /= 0)THEN
        DO J=1,IGRSURF(ISU1)%NSEG
          DO K=1,4
            I=IGRSURF(ISU1)%NODES(J,K)
            IF(TAG(I) == 1 .and.
     .         (ISYM == 1 .or. (ISYM == 0 .and. ISU2 == 0))) THEN
              TAGS(I) = 1
              NSN = NSN + 1
              IF(IALLO == 2)NSV(NSN) = I
              TAGB(I) = BITSET(TAGB(I),0)
            ENDIF
            IF(TAG(I) == 1 .or. TAG(I) == -3)THEN
              TAG(I) = - TAG(I)
              NMN = NMN + 1
              IF(IALLO == 2)MSR(NMN) = I
              TAGB(I) = BITSET(TAGB(I),3)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
c-----------------------------------------------------------------
c     noeuds du groupe de noeud NOD1
c-----------------------------------------------------------------
      IF(NOD1 /= 0)THEN
        DO J=1,IGRNOD(NOD1)%NENTITY
          I = IGRNOD(NOD1)%ENTITY(J)
          LNTAG(I) = 1
          IF(TAGS(I) == 0)THEN
            TAGS(I) = 1
            NSN = NSN+1
            IF(IALLO == 2) NSV(NSN) = I
            TAGB(I) = BITSET(TAGB(I),2)
          ENDIF
        ENDDO
      ENDIF

      IF(IALLO == 2 .and. IPRI >= 1) THEN
        WRITE(IOUT,'(/,A,/)')' NODES USED FOR SURFACE DEFINITION'
        WRITE(IOUT,FMT=FMW_10I)(ITAB(NSV(I)),I=1,NSN)
      ENDIF
C=======================================================================
c     EDGES
C=======================================================================
      IF(IEDGE /= 0)THEN
        CALL I20EDGE1(IALLO   ,IGRSURF(ISU1)%NSEG ,IGRSLIN(MAX(1,LINE1))%NSEG ,NLINM   ,NLINMA  ,
     2                IXLINM  ,MSRE               ,NMNE        ,IEDGE  ,
     3                IGRSURF(ISU1)%NODES,IGRSLIN(MAX(1,LINE1))%NODES ,ITAB   ,
     4                ISLINM  ,X       ,EDG_COS   ,LNTAG       ,
     5                TAGB    ,5       ,ISU1      ,LINE1       )
        CALL I20EDGE1(IALLO   ,IGRSURF(ISU2)%NSEG ,IGRSLIN(MAX(1,LINE2))%NSEG ,NLINS   ,NLINSA  ,
     2                IXLINS  ,NSVE               ,NSNE        ,IEDGE  ,
     3                IGRSURF(ISU2)%NODES,IGRSLIN(MAX(1,LINE2))%NODES ,ITAB   ,
     4                ISLINS  ,X       ,EDG_COS   ,LNTAG       ,
     5                TAGB    ,6       ,ISU2      ,LINE2       )
      ENDIF
C=======================================================================
c     BORDS POUR CORRECTION GAP=0 sur bord de coque
C=======================================================================
      IF(IALLO == 2)THEN
        IF(ISU1  /= 0)THEN
          CALL I20BORD(IGRSURF(ISU1)%NSEG ,IGRSURF(ISU1)%NODES, TAGB,ISU1)
        ENDIF
        IF(ISU2  /= 0 .and. ISU2  /= ISU1)THEN
          CALL I20BORD(IGRSURF(ISU2)%NSEG ,IGRSURF(ISU2)%NODES, TAGB,ISU2)
        ENDIF
      ENDIF
c-----------------------------------------------------------------
c     nombre de noeuds dans l'interface(secnd+main+edge)
c-----------------------------------------------------------------
      IF(IALLO == 1)THEN
        DO I=1,NUMNOD
          IF(LNTAG(I)==1)THEN
            NLN=NLN+1
          ENDIF
        ENDDO
      ELSEIF(IALLO == 2)THEN
        NLN   = IPARI(35)
        J=0
        DO I=1,NUMNOD
          IF(LNTAG(I)==1)THEN
            J=J+1
            NLG(J) = I
            NBINFLG(J) = TAGB(I)
          ENDIF
        ENDDO
      ENDIF

      IPARI(3)  = 0
      IPARI(4)  = NRTM
      IPARI(5)  = NSN
      IPARI(6)  = NMN
      IPARI(35) = NLN
      IPARI(51) = NLINS
      IPARI(52) = NLINM
      IPARI(53) = NLINSA
      IPARI(54) = NLINMA
      IPARI(55) = NSNE
      IPARI(56) = NMNE


      RETURN
      END





Chd|====================================================================
Chd|  I20EDGE1                      source/interfaces/inter3d1/i20surfi.F
Chd|-- called by -----------
Chd|        I20SURFI                      source/interfaces/inter3d1/i20surfi.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        BITSET                        source/interfaces/inter3d1/bitget.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I20EDGE1(IALLO     ,NSEG0     ,NLIN0   ,NLIN    ,NACTIF  ,
     2                    IXLINE    ,MSVE      ,NSME    ,IEDGE   ,
     3                    SURF_NODES,SLIN_NODES,ITAB    ,
     4                    ISLINE    ,X         ,EDG_COS ,LNTAG   ,
     5                    TAGB      ,NB        ,ISU     ,LIN     )
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
#include      "com01_c.inc"
#include      "com02_c.inc"
#include      "com03_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr12_c.inc"
#include      "scr17_c.inc"
#include      "conv_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IALLO,NSEG0,NLIN0,NLIN,NACTIF,IEDGE,NSME,NB,ISU,LIN
      INTEGER IXLINE(2,*),ITAB(*),MSVE(*),
     .        LNTAG(*) ,TAGB(*),ISLINE(2,*),SURF_NODES(NSEG0,4),
     .        SLIN_NODES(NLIN0,2)
      my_real
     .   X(3,*),EDG_COS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,NLMAX,STAT,LL,I1,I2,I3,I4,I5,I1M,I2M,NL,IS
      INTEGER NEXTK(4),IWORK(70000),NLL
      my_real
     .   NX,NY,NZ,MX,MY,MZ,AAA,D1X,D1Y,D1Z,D2X,D2Y,D2Z
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: 
     .   LINEIX,LINEIX2,IXWORK
      INTEGER, DIMENSION(:), ALLOCATABLE :: 
     .   INDEX,TAG
      my_real, DIMENSION(:,:), ALLOCATABLE :: 
     .   XLINEIX

      INTEGER BITSET
      EXTERNAL BITSET

      DATA NEXTK/2,3,4,1/
C=======================================================================
      NLMAX = 0
      IF(ISU /= 0) NLMAX = 4*NSEG0

      ALLOCATE (LINEIX(2,NLMAX)    ,STAT=stat)
      ALLOCATE (LINEIX2(2,NLMAX)   ,STAT=stat)
      ALLOCATE (XLINEIX(3,NLMAX)   ,STAT=stat)
      ALLOCATE (INDEX(2*NLMAX)     ,STAT=stat)
      ALLOCATE (TAG(NUMNOD)        ,STAT=stat)
      ALLOCATE (IXWORK(5,NLMAX)    ,STAT=stat)

      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='LINEIX')

c---------------------------------------
c       recherche de toutes les lignes dans la surface
c---------------------------------------
      IF(ISU /= 0)THEN
        IS = 0
        LL = 0
        DO J=1,NSEG0
          IS = IS+1
          I1=SURF_NODES(J,1)
          I2=SURF_NODES(J,2)
          I3=SURF_NODES(J,3)
          I4=SURF_NODES(J,4)
          D1X = X(1,I3) - X(1,I1)
          D1Y = X(2,I3) - X(2,I1)
          D1Z = X(3,I3) - X(3,I1)
          D2X = X(1,I4) - X(1,I2)
          D2Y = X(2,I4) - X(2,I2)
          D2Z = X(3,I4) - X(3,I2)
          NX = D1Y * D2Z - D1Z * D2Y
          NY = D1Z * D2X - D1X * D2Z
          NZ = D1X * D2Y - D1Y * D2X
          AAA = ONE/MAX(SQRT(NX*NX+NY*NY+NZ*NZ),EM20)
          NX = NX * AAA
          NY = NY * AAA
          NZ = NZ * AAA
          DO K=1,4
            I1=SURF_NODES(J,K)
            I2=SURF_NODES(J,NEXTK(K))
            LL = LL+1
            IF(I2 > I1)THEN
              LINEIX(1,LL) = I1
              LINEIX(2,LL) = I2
              LINEIX2(1,LL) = J
              LINEIX2(2,LL) = K
            ELSE
c              LINEIX(1,LL) = I1
c              LINEIX(2,LL) = I2
              LINEIX(1,LL) = I2
              LINEIX(2,LL) = I1
              LINEIX2(1,LL) = J
              LINEIX2(2,LL) = -K
            ENDIF
            XLINEIX(1,LL) = NX
            XLINEIX(2,LL) = NY
            XLINEIX(3,LL) = NZ
          ENDDO
        ENDDO
C
        CALL MY_ORDERS(0,IWORK,LINEIX,INDEX,LL,2)

c---------------------------------------
c       suppression des lignes doubles
c       + calcul des angles(sin) inter-facettes
c---------------------------------------
        I1M = LINEIX(1,INDEX(1))
        I2M = LINEIX(2,INDEX(1))
        NL = 1
        IXWORK(1,NL)=I1M
        IXWORK(2,NL)=I2M
        IXWORK(3,NL)=LINEIX2(1,INDEX(1))
        IXWORK(4,NL)=LINEIX2(2,INDEX(1))
        IXWORK(5,NL)=1
        MX = XLINEIX(1,INDEX(1))
        MY = XLINEIX(2,INDEX(1))
        MZ = XLINEIX(3,INDEX(1))
        DO L=2,LL
          I1 = LINEIX(1,INDEX(L))
          I2 = LINEIX(2,INDEX(L))
          NX = XLINEIX(1,INDEX(L))
          NY = XLINEIX(2,INDEX(L))
          NZ = XLINEIX(3,INDEX(L))
          IF(I2 /= I2M .or. I1 /= I1M)THEN
            NL = NL + 1
            IXWORK(1,NL)=I1
            IXWORK(2,NL)=I2
            IXWORK(3,NL)=LINEIX2(1,INDEX(L))
            IXWORK(4,NL)=LINEIX2(2,INDEX(L))
            IXWORK(5,NL)=1 ! bord
          ELSE
            IXWORK(5,NL)=0 ! interne
            AAA = NX*MX + NY * MY + NZ * MZ
            IF (AAA < EDG_COS) IXWORK(5,NL) = -1 ! arte vive
          ENDIF
          I1M = I1
          I2M = I2
          MX = NX
          MY = NY
          MZ = NZ
        ENDDO

c---------------------------------------
c       suppression des lignes internes (IEDGE == 1)
c---------------------------------------
        LL = NL
        NL = 0
        IF(IEDGE == 1)THEN
c         seuls les bords sont conservs
          DO L=1,LL
            IF(IXWORK(5,L) == 1)THEN
              NL = NL + 1
              I1=IXWORK(1,NL)
              I2=IXWORK(2,NL)
              I3=IXWORK(3,NL)
              I4=IXWORK(4,NL)
              I5=IXWORK(5,NL)
              IXWORK(1,NL)=IXWORK(1,L)
              IXWORK(2,NL)=IXWORK(2,L)
              IXWORK(3,NL)=IXWORK(3,L)
              IXWORK(4,NL)=IXWORK(4,L)
              IXWORK(5,NL)=1 ! bord on
              IXWORK(1,L)=I1  
              IXWORK(2,L)=I2  
              IXWORK(3,L)=I3  
              IXWORK(4,L)=I4  
              IXWORK(5,L)=I5 
            ENDIF
          ENDDO
        ELSEIF(IEDGE == 2)THEN
c         toutes les lignes sont conserves ET actives
          DO L=1,LL
            NL = NL + 1
            IXWORK(5,L)=1 ! all on
          ENDDO
        ELSEIF(IEDGE == 3)THEN
c         les bords sont conservs
c         les artes vives sont conservs (EDG_COS)
          DO L=1,LL
            IF(IABS(IXWORK(5,L)) == 1)THEN
              NL = NL + 1
              I1=IXWORK(1,NL)
              I2=IXWORK(2,NL)
              I3=IXWORK(3,NL)
              I4=IXWORK(4,NL)
              I5=IABS(IXWORK(5,NL))
              IXWORK(1,NL)=IXWORK(1,L)
              IXWORK(2,NL)=IXWORK(2,L)
              IXWORK(3,NL)=IXWORK(3,L)
              IXWORK(4,NL)=IXWORK(4,L)
              IXWORK(5,NL)=1 ! bord on
              IXWORK(1,L)=I1  
              IXWORK(2,L)=I2  
              IXWORK(3,L)=I3  
              IXWORK(4,L)=I4  
              IXWORK(5,L)=I5  
           ENDIF
          ENDDO
        ENDIF
C
      ELSE
C       pas de surfaces
        LL = 0
        NL = 0
      ENDIF
c---------------------------------------
c       nombre de lignes
c---------------------------------------
      NLL    = LL      
      NLIN   = LL      
      NACTIF = NL
      IF(LIN /= 0) THEN
        NLIN =  NLIN + NLIN0
        NACTIF = NACTIF + NLIN0
      ENDIF
c---------------------------------------
c     nombre de noeuds
c---------------------------------------
      NSME = 0
      DO I=1,NUMNOD
        TAG(I) = 0                               
      ENDDO
      DO LL=1,NLL
        TAG(IXWORK(1,LL)) = 1                               
        TAG(IXWORK(2,LL)) = 1                               
      ENDDO
      IF(LIN /= 0)THEN
        DO J=1,NLIN0
          TAG(SLIN_NODES(J,1)) = 1                             
          TAG(SLIN_NODES(J,2)) = 1                             
          LNTAG(SLIN_NODES(J,1)) = 1			      
          LNTAG(SLIN_NODES(J,2)) = 1
        ENDDO
      ENDIF
      DO I=1,NUMNOD
        IF(TAG(I) == 1) THEN
          NSME = NSME + 1 
          TAGB(I) = BITSET(TAGB(I),NB)
        ENDIF                              
      ENDDO
c---------------------------------------
c     copie des lignes (IALLO == 2)
c---------------------------------------
      IF(IALLO == 2)THEN
        L = 0
        IF(LIN /= 0)THEN
          DO J=1,NLIN0
            L = L+1
            IXLINE(1,L) = SLIN_NODES(J,1)   ! noeud 1
            IXLINE(2,L) = SLIN_NODES(J,2)   ! noeud 2
            ISLINE(1,L) = 0              ! surface
            ISLINE(2,L) = 0              ! cot de la surface
          ENDDO
        ENDIF

        DO LL=1,NLL
          IF(IXWORK(5,LL) == 1)THEN
            L = L+1                               
            IXLINE(1,L) = IXWORK(1,LL)    ! noeud 1
            IXLINE(2,L) = IXWORK(2,LL)    ! noeud 2
            ISLINE(1,L) = IXWORK(3,LL)    ! surface
            ISLINE(2,L) = IXWORK(4,LL)    ! cot de la surface
          ENDIF
        ENDDO

c       lignes inactives
        DO LL=1,NLL
          IF(IXWORK(5,LL) /= 1)THEN
            L = L+1                               
            IXLINE(1,L) = IXWORK(1,LL)    ! noeud 1
            IXLINE(2,L) = IXWORK(2,LL)    ! noeud 2
            ISLINE(1,L) = IXWORK(3,LL)    ! surface
            ISLINE(2,L) = IXWORK(4,LL)    ! cot de la surface
          ENDIF
        ENDDO
          
        IF(IPRI >= 1) THEN
          WRITE(IOUT,'(/,A,/)')' ACTIV SEGMENTS USED FOR EDGE'
          K=1
          DO I=1,NACTIF
            WRITE(IOUT,FMT=FMW_4I)(ITAB(IXLINE(K,I)),K=1,2)
          ENDDO      
        ENDIF

c       noeuds
        L = 0
        DO I=1,NUMNOD
          IF(TAG(I) == 1)THEN
            TAG(I) = 0 
            L = L+1                               
            MSVE(L) = I                             
          ENDIF                              
        ENDDO
      ENDIF
C-----------
      DEALLOCATE (INDEX)
      DEALLOCATE (TAG)
      DEALLOCATE (IXWORK)
      DEALLOCATE (LINEIX)
      DEALLOCATE (LINEIX2)
      DEALLOCATE (XLINEIX)
C-----------
      RETURN
      END


Chd|====================================================================
Chd|  I20BORD                       source/interfaces/inter3d1/i20surfi.F
Chd|-- called by -----------
Chd|        I20SURFI                      source/interfaces/inter3d1/i20surfi.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        BITSET                        source/interfaces/inter3d1/bitget.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I20BORD(NSEG  ,SURF_NODES ,TAGB,ISU)
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
#include      "com01_c.inc"
#include      "com02_c.inc"
#include      "com03_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr12_c.inc"
#include      "scr17_c.inc"
#include      "conv_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IALLO,NSEG,SURF_NODES(NSEG,4),ISU
      INTEGER TAGB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,NLMAX,STAT,LL,I1,I2,I3,I4,I5,I1M,I2M,IS,BORD,BOLD
      INTEGER NEXTK(4),IWORK(70000),NL
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: 
     .   LINEIX
      INTEGER, DIMENSION(:), ALLOCATABLE :: 
     .   INDEX

      INTEGER BITSET
      EXTERNAL BITSET

      DATA NEXTK/2,3,4,1/
C=======================================================================
      NLMAX = 0
      IF(ISU /= 0)NLMAX = 4*NSEG

      ALLOCATE (LINEIX(2,NLMAX)    ,STAT=stat)
      ALLOCATE (INDEX(2*NLMAX)     ,STAT=stat)

      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='LINEIX')

c---------------------------------------
c       recherche de toutes les lignes dans la surface
c---------------------------------------
      IF(ISU /= 0)THEN
        IS = 0
        LL = 0
        DO J=1,NSEG
          IS = IS+1
          I1=SURF_NODES(J,1)
          I2=SURF_NODES(J,2)
          I3=SURF_NODES(J,3)
          I4=SURF_NODES(J,4)
          DO K=1,4
            I1=SURF_NODES(J,K)
            I2=SURF_NODES(J,NEXTK(K))
            LL = LL+1
            IF(I2 > I1)THEN
              LINEIX(1,LL) = I1
              LINEIX(2,LL) = I2
            ELSE
c              LINEIX(1,LL) = I1
c              LINEIX(2,LL) = I2
              LINEIX(1,LL) = I2
              LINEIX(2,LL) = I1
            ENDIF
          ENDDO
        ENDDO
C
        CALL MY_ORDERS(0,IWORK,LINEIX,INDEX,LL,2)

c---------------------------------------
c       suppression des lignes doubles
c---------------------------------------
        I1M = LINEIX(1,INDEX(1))
        I2M = LINEIX(2,INDEX(1))
        BORD=1
        BOLD=1
        DO L=2,LL
          I1 = LINEIX(1,INDEX(L))
          I2 = LINEIX(2,INDEX(L))
          IF(I1M == I2M)THEN
c triangle on ne fait rien
            BOLD=1
          ELSEIF(BOLD == 0)THEN
c idem precedent on ne fait rien
            BOLD=1
          ELSEIF(I2 == I2M .and. I1 == I1M)THEN
c idem suivant pas de bord
            BORD=0
            BOLD=0
          ELSE
            BORD=1 ! bord
            BOLD=1
            TAGB(I1M) = BITSET(TAGB(I1M),7)
            TAGB(I2M) = BITSET(TAGB(I2M),7)
          ENDIF
          I1M = I1
          I2M = I2
        ENDDO

        IF(BORD==1)THEN
c         derniere arrete est un bord
          TAGB(I1) = BITSET(TAGB(I1),7)
          TAGB(I2) = BITSET(TAGB(I2),7)
        ENDIF

      ENDIF

      DEALLOCATE (INDEX)
      DEALLOCATE (LINEIX)
C-----------
      RETURN
      END

