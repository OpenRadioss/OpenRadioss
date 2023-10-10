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
Chd|  I24SURFI                      source/interfaces/inter3d1/i24surfi.F
Chd|-- called by -----------
Chd|        LECINS                        source/interfaces/interf1/lecins.F
Chd|        LECINT                        source/interfaces/interf1/lecint.F
Chd|-- calls ---------------
Chd|        I24EDGE1                      source/interfaces/inter3d1/i24surfi.F
Chd|        I24EDGE2                      source/interfaces/inter3d1/i24surfi.F
Chd|        IN24COQ_SOL3                  source/interfaces/inter3d1/i24surfi.F
Chd|        SH2SURF                       source/interfaces/inter3d1/i24surfi.F
Chd|        BITSET                        source/interfaces/inter3d1/bitget.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE I24SURFI(IALLO   ,IPARI   ,IGRNOD  ,IGRSURF ,
     1                    IRECT   ,FRIGAP  ,
     2                    NSV     ,MSR     ,ITAB    ,X       ,
     3                    NBINFLG ,MBINFLG ,MSEGTYP ,ISEADD  ,
     4                    ISEDGE  ,ITAG    ,INTPLY  ,IXC     ,
     5                    IXTG    ,KNOD2ELC,KNOD2ELTG,NOD2ELC,
     6                    NOD2ELTG,KNOD2ELS,NOD2ELS  ,IXS    ,
     7                    IXS10   ,IXS16   ,IXS20    ,IRTSE  ,
     8                    IS2SE   ,IS2PT   ,IS2ID    ,INTNITSCHE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IALLO,INTNITSCHE,NBINFLG(*)
      INTEGER IPARI(NPARI),
     .        IRECT(4,*), NSV(*),MSEGTYP(*),
     .        MSR(*),ITAB(*),MBINFLG(*),
     .        ISEADD(*) ,ISEDGE(*),ITAG(*),INTPLY,
     .        IXC(*),IXTG(*),KNOD2ELC(*),KNOD2ELTG(*),
     .        NOD2ELC(*),NOD2ELTG(*),KNOD2ELS(*),NOD2ELS(*),
     .        IRTSE(5,*) ,IS2SE(*),IS2PT(*)   ,IS2ID(*)
      INTEGER IXS(NIXS,*),IXS10(6,*), IXS16(8,*), IXS20(12,*)
      my_real
     .   X(3,*),FRIGAP(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,LL,NL,ISU1,ISU2,NOD1,NRTM,NRTS,NSN,NMN,
     .        NLINSA,NLINMA,ILEV,IEDGE,NSNE,NMNE,NLN,ISYM,
     .        NLINS,NLINM,LINE1,LINE2,STAT,IADL,IL,IG
      INTEGER TAG(NUMNOD),TAGS(NUMNOD),NEXTK(4),IWORK(70000),
     .        ISHIF,NSU1,NLS1,NLS2,NRTM_SH,ETYP,NRTM_SH1,NRTM0,
     .        IMBIN,IM,L24ADD,ICOQ(4),NRTSE
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
      NOD1  = IPARI(26)
      NLN   = 0
      ILEV  = IPARI(20)
      ISU1  = IPARI(45)
      ISU2  = IPARI(46)
      IEDGE = IPARI(58)
      L24ADD = IPARI(59)
      EDG_COS = FRIGAP(26)
      NSU1 = 0
      NLS1 = 0
      NLS2 = 0
      NSNE  = 0
      NRTSE = 0
      IF(ILEV==2 ) THEN
c      IF(IEDGE /= 0.OR. ILEV==2 ) THEN
       IMBIN=1
      ELSE
       IMBIN=0
      END IF
C=======================================================================
c     SURFACES
C=======================================================================
c-----------------------------------------------------------------
c     surface S1
c-----------------------------------------------------------------
c-----------------------------------------------------------------
c     surface S2
c-----------------------------------------------------------------
      SELECT CASE (ILEV)
C-----attention: ISU2=ISU1 /=0            
          CASE(1)
            NRTM = IGRSURF(ISU1)%NSEG
            IF(INTNITSCHE>0) NRTS = NRTM
          CASE(2)
            NRTM = IGRSURF(ISU1)%NSEG
            NRTS = IGRSURF(ISU2)%NSEG
            NRTM = NRTM + NRTS
            IF(INTNITSCHE>0) NRTS = NRTM
          CASE(3)
            NRTM = IGRSURF(ISU2)%NSEG
      END SELECT
c      ISYM = IPARI(43)
c---------------------------------------
c     copie des surfaces (IALLO == 2)
c---------------------------------------
       IF(IALLO == 2)THEN
        DO I=1,NRTM
         MSEGTYP(I)=0 
        ENDDO
        L = 0
        IF(ISU1 /= 0)THEN
          DO J=1,IGRSURF(ISU1)%NSEG
            L = L+1
            DO K=1,4
              IRECT(K,L) = IGRSURF(ISU1)%NODES(J,K)
            ENDDO
            MSEGTYP(L) = IGRSURF(ISU1)%ELTYP(J)
C------------ call anyway, if coating shell MSEGTYP(L)=MSEGTYP(L)+1             
            CALL IN24COQ_SOL3(IRECT(1,L) ,IXC ,IXTG ,MSEGTYP(L) ,X  ,
     .                   KNOD2ELC ,KNOD2ELTG ,NOD2ELC ,NOD2ELTG,
     .                   KNOD2ELS,NOD2ELS,IXS ,IXS10 ,IXS16 ,IXS20 )
            IF(IMBIN /= 0)MBINFLG(L) = BITSET(MBINFLG(L),0)
          ENDDO
        ENDIF
        NSU1 = L
        IF(ISU2 /= 0 .AND.ILEV /= 1)THEN
          DO J=1,IGRSURF(ISU2)%NSEG
            L = L+1
            DO K=1,4
              IRECT(K,L) = IGRSURF(ISU2)%NODES(J,K)
            ENDDO
            MSEGTYP(L) = IGRSURF(ISU2)%ELTYP(J)
            CALL IN24COQ_SOL3(IRECT(1,L) ,IXC ,IXTG ,MSEGTYP(L) ,X  ,
     .                   KNOD2ELC ,KNOD2ELTG ,NOD2ELC ,NOD2ELTG,
     .                   KNOD2ELS,NOD2ELS,IXS ,IXS10 ,IXS16 ,IXS20 )
            IF(IMBIN /= 0) MBINFLG(L) = BITSET(MBINFLG(L),1)
          ENDDO
        ENDIF
#ifndef HYPERMESH_LIB
        IF(IPRI>=5) THEN
          WRITE(IOUT,'(/,A,/)')' SEGMENTS USED FOR MAIN SURFACE: '
          DO I=1,NRTM
            WRITE(IOUT,FMT=FMW_4I)(ITAB(IRECT(K,I)),K=1,4)
          ENDDO
        ENDIF
#endif HYPERMESH_LIB
      ENDIF
C=======================================================================
c     NOEUDS
C=======================================================================
c-----------------------------------------------------------------
c     tag noeuds surfaces S1 S2; 1,2 on S1,S2, 3 on both (ILEV=1)
c-----------------------------------------------------------------
      DO I=1,NUMNOD
        TAG(I)=0 ! initialisation
        TAGS(I)=0 ! initialisation
      ENDDO
      IF(ISU2 /= 0)THEN
        DO J=1,IGRSURF(ISU2)%NSEG
          DO K=1,4
            TAG(IGRSURF(ISU2)%NODES(J,K)) = 2
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
          ENDDO
        ENDDO
      ENDIF
C for inteply activation needed for Plyxfem + Type24      
      IF(IALLO == 1) THEN
        IF(ISU2 /= 0)THEN
           DO J=1,IGRSURF(ISU2)%NSEG
              DO K=1,4
!!                 IF(ITAG(IBUFSSG(IAD)) > 0) INTPLY = 1
                 I=IGRSURF(ISU2)%NODES(J,K)
                 IF(ITAG(I) > 0) INTPLY = 1
              ENDDO
            ENDDO
         ENDIF
         IF(ISU1 /= 0)THEN
           DO J=1,IGRSURF(ISU1)%NSEG
              DO K=1,4
                I=IGRSURF(ISU1)%NODES(J,K)
                IF(ITAG(I) > 0) INTPLY = 1
              ENDDO
            ENDDO
         ENDIF 
      ENDIF
c-----------------------------------------------------------------
c     noeuds de la surface S2 : build TAGS,set NSV,MSR if IALLO= 2
c-----------------------------------------------------------------
      IF(ISU2 /= 0)THEN
        DO J=1,IGRSURF(ISU2)%NSEG
          DO K=1,4
            I=IGRSURF(ISU2)%NODES(J,K)
            IF(TAG(I) == 2 )THEN
              NMN = NMN + 1
              IF(IALLO == 2)MSR(NMN) = I
c              TAGB(I) = BITSET(TAGB(I),4)
            ENDIF
c     taged nodes on S2 -> negative value
            IF(TAG(I) == 2 .OR. TAG(I) == 3)THEN
              TAG(I) = - TAG(I)
                 IF ( ILEV == 2.AND.TAGS(I) == 0 ) THEN
               TAGS(I) = 1
               NSN = NSN + 1
               IF(IALLO == 2) THEN
                NSV(NSN) = I
                IF(ILEV == 2)NBINFLG(NSN) = BITSET(NBINFLG(NSN),1)
               END IF
                 END IF !( ILEV == 2 ) THEN
            ENDIF
          ENDDO
        ENDDO
      ENDIF
c-----------------------------------------------------------------
c     noeuds de la surface S1: build TAGS,set NSV,MSR if IALLO= 2
c-----------------------------------------------------------------
      IF(ISU1 /= 0)THEN
        DO J=1,IGRSURF(ISU1)%NSEG
          DO K=1,4
            I=IGRSURF(ISU1)%NODES(J,K)
            IF(TAGS(I) == 0 .AND. ILEV /= 3 ) THEN
              TAGS(I) = 1
              NSN = NSN + 1
              IF(IALLO == 2) THEN
               NSV(NSN) = I
               IF(ILEV == 2)NBINFLG(NSN) = BITSET(NBINFLG(NSN),0)
              END IF
            ENDIF
c     taged nodes on S1 -> negative value, ->+3 for nodes on both
            IF(TAG(I) == 1 .or. TAG(I) == -3)THEN
              TAG(I) = - TAG(I)
              NMN = NMN + 1
              IF(IALLO == 2)MSR(NMN) = I
            ENDIF
          ENDDO
        ENDDO
      ENDIF
c-----------------------------------------------------------------
c     noeuds du groupe de noeud NOD1: build TAGS,set NSV if IALLO= 2
c-----------------------------------------------------------------
      IF(NOD1 /= 0)THEN
        DO J=1,IGRNOD(NOD1)%NENTITY
          I = IGRNOD(NOD1)%ENTITY(J)
          IF(TAGS(I) == 0)THEN
            TAGS(I) = 1
            NSN = NSN+1
            IF(IALLO == 2) THEN
             NSV(NSN) = I
             IF(ILEV == 2)NBINFLG(NSN) = BITSET(NBINFLG(NSN),2)
            END IF
          ENDIF
        ENDDO
      ENDIF

#ifndef HYPERMESH_LIB
      IF(IALLO == 2 .and. IPRI >= 5) THEN
        WRITE(IOUT,'(/,A,/)')' NODES USED FOR SECONDARY SIDE'
        WRITE(IOUT,FMT=FMW_10I)(ITAB(NSV(I)),I=1,NSN)
      ENDIF
#endif
C=======================================================================
c     EDGES
C=======================================================================
C-----new subsuf/suf on edge-----
      IF(IEDGE == 4)THEN
        ISHIF=0
         CALL I24EDGE2(IALLO   ,IGRSURF(ISU1)%NSEG,NLN     ,IEDGE-1  ,
     1                 IGRSURF(ISU1)%NODES ,ITAB  ,ISU1    ,
     2                 X       ,EDG_COS ,MBINFLG  ,ISHIF   ,NLS1    ,
     3                 IRECT   ,NRTSE   ,IRTSE    ,NSNE    ,IS2SE   ,
     4                 IS2PT   ,NSN     ,NSV      ,IS2ID)
        ISHIF=NSU1
        IF(ISU2 /= 0 .AND. ILEV /= 1) THEN
         CALL I24EDGE2(IALLO   ,IGRSURF(ISU2)%NSEG,NLN     ,IEDGE-1 ,
     1                 IGRSURF(ISU2)%NODES ,ITAB  ,ISU2    ,
     2                 X       ,EDG_COS ,MBINFLG  ,ISHIF   ,NLS2    ,
     3                 IRECT   ,NRTSE   ,IRTSE    ,NSNE    ,IS2SE   ,
     4                 IS2PT   ,NSN     ,NSV      ,IS2ID)
        END IF
        NSN = NSN + NSNE
      ELSEIF(IEDGE /= 0)THEN
        ISHIF=0
         CALL I24EDGE1(IALLO,IGRSURF(ISU1)%NSEG,NLN     ,IEDGE   ,
     1              IGRSURF(ISU1)%NODES ,ITAB  ,ISU1    ,
     2              X       ,EDG_COS ,MBINFLG  ,ISHIF   ,NLS1 ,
     3              IRECT   ,L24ADD  ,ISEADD   ,ISEDGE  ,NSN  ,
     4              1       ,NSV     )
        ISHIF=NSU1
        IF(ISU2 /= 0 .AND. ILEV /= 1) THEN
         CALL I24EDGE1(IALLO,IGRSURF(ISU2)%NSEG,NLN     ,IEDGE   ,
     1              IGRSURF(ISU2)%NODES ,ITAB  ,ISU2    ,
     2              X       ,EDG_COS ,MBINFLG  ,ISHIF   ,NLS2 ,
     3              IRECT   ,L24ADD  ,ISEADD   ,ISEDGE  ,NSN  ,
     4              2       ,NSV     )
        END IF
      ENDIF
C=======================================================================
C=======================================================================
c-----------------------------------------------------------------
c     nombre de noeuds dans l'interface(SECONDARY+MAIN+edge)
c-----------------------------------------------------------------
      IF(IALLO == 2) THEN 
       NLN   = IPARI(35)
       IPARI(51) = NLS1
       IPARI(52) = NLS2
       IF (IEDGE == 4) IPARI(52) = NRTSE
       IPARI(55) = NSNE
C------initialization of doubler M_seg pour shells  add ISU1>0
       NRTM_SH= IPARI(42)
       NRTM0 = IPARI(4) - NRTM_SH
       CALL SH2SURF(NRTM0,IRECT,IMBIN,MBINFLG,MSEGTYP,IPARI(4))
C-----temporairement set IEDGE=0 for Engine       
       IF (IEDGE == 4) IPARI(58) = 0
C
      ELSE
C----------due the fact that NRTM is modified w/ shell seg
       IF(INTNITSCHE > 0) THEN
          IPARI(3)  = NRTS
       ELSE
          IPARI(3)  = 0
       ENDIF
       IPARI(4)  = NRTM
       IPARI(5)  = NSN
       IPARI(6)  = NMN
       IPARI(35) = NLN
       IPARI(59) = L24ADD
       IF (IEDGE == 4) IPARI(52) = NRTSE
       IPARI(55) = NSNE
C----------doubling shell segments-excepting coating shell-----------
        NRTM_SH=0
        IF(ISU1 /= 0)THEN
          DO J=1,IGRSURF(ISU1)%NSEG
            DO K=1,4
              ICOQ(K) = IGRSURF(ISU1)%NODES(J,K)
            ENDDO
            ETYP = IGRSURF(ISU1)%ELTYP(J)
            CALL IN24COQ_SOL3(ICOQ ,IXC ,IXTG ,ETYP     ,X  ,
     .                   KNOD2ELC ,KNOD2ELTG ,NOD2ELC ,NOD2ELTG,
     .                   KNOD2ELS,NOD2ELS,IXS ,IXS10 ,IXS16 ,IXS20 )
           IF(ETYP ==3 .OR. ETYP ==7 ) NRTM_SH = NRTM_SH + 1
          ENDDO
        ENDIF
        IF(ISU2 /= 0 .AND. ILEV /= 1)THEN
          DO J=1,IGRSURF(ISU2)%NSEG
            DO K=1,4
              ICOQ(K) = IGRSURF(ISU2)%NODES(J,K)
            ENDDO
            ETYP = IGRSURF(ISU2)%ELTYP(J)
            CALL IN24COQ_SOL3(ICOQ ,IXC ,IXTG ,ETYP     ,X  ,
     .                   KNOD2ELC ,KNOD2ELTG ,NOD2ELC ,NOD2ELTG,
     .                   KNOD2ELS,NOD2ELS,IXS ,IXS10 ,IXS16 ,IXS20 )
           IF(ETYP ==3 .OR. ETYP ==7 ) NRTM_SH = NRTM_SH + 1
          ENDDO
        ENDIF
        IPARI(42) = NRTM_SH
      END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  I24EDGE1                      source/interfaces/inter3d1/i24surfi.F
Chd|-- called by -----------
Chd|        I24SURFI                      source/interfaces/inter3d1/i24surfi.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        BITSET                        source/interfaces/inter3d1/bitget.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I24EDGE1(IALLO      ,NSEG    ,NACTIF  ,IEDGE   ,
     1                    SURF_NODES ,ITAB    ,ISU     ,
     2                    X          ,EDG_COS ,MBINFLG ,IADM    ,NLS     ,
     3                    IRECT      ,L24ADD  ,ISEADD  ,ISEDGE  ,NSN     ,
     4                    IFIRST     ,NSV     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
#ifndef HYPERMESH_LIB
      USE MESSAGE_MOD
#endif
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IALLO,NACTIF,IEDGE,IADM,NLS,L24ADD,NSN,IFIRST,ISU
      INTEGER ITAB(*),MBINFLG(*),IRECT(4,*),NSEG,SURF_NODES(NSEG,4),
     .        ISEADD(*) ,ISEDGE(*),NSV(*)
      my_real
     .   X(3,*),EDG_COS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C----- NLS : Num. of element with active edge----
      INTEGER I,J,K,L,NLMAX,STAT,LL,I1,I2,I3,I4,I5,I1M,I2M,NL,IS,
     .        I3M,I4M,I6,I7,IADD,IM,IP,LI
      INTEGER NEXTK(4),KM1(4),KP2(4),IWORK(70000),NLL,NSE
      my_real
     .   NX,NY,NZ,MX,MY,MZ,AAA,D1X,D1Y,D1Z,D2X,D2Y,D2Z,IMJ,IPJ
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: 
     .   LINEIX,LINEIX2,IXWORK
      INTEGER, DIMENSION(:), ALLOCATABLE :: 
     .   INDEX,TAG,ISEADD_L,ISH
      my_real, DIMENSION(:,:), ALLOCATABLE :: 
     .   XLINEIX

      INTEGER BITSET
      EXTERNAL BITSET

      DATA NEXTK/2,3,4,1/
      DATA KM1/4,1,2,3/
      DATA KP2/3,4,1,2/
C=======================================================================
      NLMAX = 0
      NLS = 0
      IF(ISU /= 0)NLMAX = 4*NSEG
c---------------------------------------
c       LINEIX(2,*): LINE; LINEIX2(1,):Id_seg,LINEIX2(2,):Jd_seg(1-4)
c       IXWORK(8,*): reordered lines; (1-2,):LINEIX,(3-4,)or (6-7,) if inverse order of I1,I2
c                    :LINEIX2,(5):I_bord; (8,):flag of inverse I1,I2
c---------------------------------------
      ALLOCATE (LINEIX(2,NLMAX)    ,STAT=stat)
      ALLOCATE (LINEIX2(2,NLMAX)   ,STAT=stat)
      ALLOCATE (XLINEIX(3,NLMAX)   ,STAT=stat)
      ALLOCATE (INDEX(2*NLMAX)     ,STAT=stat)
c      ALLOCATE (TAG(NUMNOD)        ,STAT=stat)
      ALLOCATE (ISEADD_L(NUMNOD)      ,STAT=stat)
      ALLOCATE (ISH(NUMNOD)           ,STAT=stat)
      ALLOCATE (IXWORK(8,NLMAX)    ,STAT=stat)


#ifndef HYPERMESH_LIB 
      IF (STAT /= 0) THEN
          CALL ANCMSG(MSGID=268,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                         C1='LINEIX')
      END IF
#endif

c---------------------------------------
c       recherche de toutes les lignes dans la surface
c---------------------------------------
C initialize IXWORK to zero
      IXWORK(4,1:NLMAX)=0


      IF(ISU /= 0)THEN
        IS = 0
        LL = 0
        DO J=1,NSEG
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
              IXWORK(8,LL) = 0
            ELSE
              LINEIX(1,LL) = I2
              LINEIX(2,LL) = I1
C-----------means I1,I2 has been exchanged              
              IXWORK(8,LL) = 1
            ENDIF
            LINEIX2(1,LL) = J
            LINEIX2(2,LL) = K
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
        LI = INDEX(1)
        I1M = LINEIX(1,LI)
        I2M = LINEIX(2,LI)
        I3M = LINEIX2(1,LI)
        I4M = LINEIX2(2,LI)
        NL = 1
        IXWORK(1,NL)=I1M
        IXWORK(2,NL)=I2M
        IF(IXWORK(8,LI)==0)THEN
          IXWORK(3,NL)=I3M
          IXWORK(4,NL)=I4M
          IXWORK(6,NL)=0
        ELSE
          IXWORK(6,NL)=I3M
          IXWORK(7,NL)=I4M
          IXWORK(3,NL)=0
        ENDIF
C---------first -> border but can be corrected later
        IXWORK(5,NL)=1
        MX = XLINEIX(1,LI)
        MY = XLINEIX(2,LI)
        MZ = XLINEIX(3,LI)
        DO L=2,LL
          LI = INDEX(L)
          I1 = LINEIX(1,LI)
          I2 = LINEIX(2,LI)
          I3 = LINEIX2(1,LI)
          I4 = LINEIX2(2,LI)
          NX = XLINEIX(1,LI)
          NY = XLINEIX(2,LI)
          NZ = XLINEIX(3,LI)
          IF(I2 /= I2M .or. I1 /= I1M)THEN
c store new edge
            NL = NL + 1
            IXWORK(1,NL)=I1
            IXWORK(2,NL)=I2
            IF(IXWORK(8,LI)==0)THEN
              IXWORK(3,NL)=I3
              IXWORK(4,NL)=I4
              IXWORK(6,NL)=0
            ELSE
              IXWORK(6,NL)=I3  
              IXWORK(7,NL)=I4 
              IXWORK(3,NL)=0
            ENDIF
            IXWORK(5,NL)=1 ! bord
          ELSE
C------internal lines are not incremented because they are always double
C--------- and the second one does the correction
            IXWORK(5,NL)=0  ! interne
c           second segment
            IF(IXWORK(8,LI)==0)THEN
              IXWORK(3,NL)=I3
              IXWORK(4,NL)=I4
            ELSE
              IXWORK(6,NL)=I3  
              IXWORK(7,NL)=I4 
            ENDIF
            AAA = NX*MX + NY * MY + NZ * MZ
            IF (AAA < EDG_COS) IXWORK(5,NL) = -1 ! arete vive
          ENDIF
          I1M = I1
          I2M = I2
          MX = NX
          MY = NY
          MZ = NZ
        ENDDO
c---------------------------------------
c       suppression des lignes internes (IEDGE == 1)
c       dimension first
c---------------------------------------
        LL = NL
        NL = 0
        IF(IEDGE == 1)THEN
c         keep only border edges (IXWORK(5,L) == 1)
          DO L=1,LL
            IF(IXWORK(5,L) == 1)THEN
              NL = NL + 1
              I1=IXWORK(1,NL)
              I2=IXWORK(2,NL)
              I3=IXWORK(3,NL)
              I4=IXWORK(4,NL)
              I5=IXWORK(5,NL)
              I6=IXWORK(6,NL)
              I7=IXWORK(7,NL)
              IXWORK(1,NL)=IXWORK(1,L)
              IXWORK(2,NL)=IXWORK(2,L)
              IXWORK(3,NL)=IXWORK(3,L)
              IXWORK(4,NL)=IXWORK(4,L)
              IXWORK(5,NL)=1 ! border
              IXWORK(6,NL)=IXWORK(6,L)
              IXWORK(7,NL)=IXWORK(7,L)
              IXWORK(1,L)=I1  
              IXWORK(2,L)=I2  
              IXWORK(3,L)=I3  
              IXWORK(4,L)=I4  
              IXWORK(5,L)=I5 
              IXWORK(6,L)=I6  
              IXWORK(7,L)=I7 
            ENDIF
          ENDDO
        ELSEIF(IEDGE == 2)THEN
c         toutes les lignes sont conserves ET actives
          DO L=1,LL
            NL = NL + 1
            IF(IXWORK(5,L) == 0)IXWORK(5,L)=-1 ! all on +-1
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
              I6=IXWORK(6,NL)
              I7=IXWORK(7,NL)
              IXWORK(1,NL)=IXWORK(1,L)
              IXWORK(2,NL)=IXWORK(2,L)
              IXWORK(3,NL)=IXWORK(3,L)
              IXWORK(4,NL)=IXWORK(4,L)
C             IXWORK(5,NL)=+-1 ! bord on
              IXWORK(6,NL)=IXWORK(6,L)
              IXWORK(7,NL)=IXWORK(7,L)
              IXWORK(1,L)=I1  
              IXWORK(2,L)=I2  
              IXWORK(3,L)=I3  
              IXWORK(4,L)=I4  
              IXWORK(5,L)=I5  
              IXWORK(6,L)=I6  
              IXWORK(7,L)=I7 
           ENDIF
          ENDDO
        ENDIF
C
      ELSE
C       pas de surfaces
        NL = 0
      ENDIF
c---------------------------------------
c     setup MBINFLG (IALLO == 2)
c       tag segment with active edges
c       (only MAIN segment)
c---------------------------------------
      IF(IALLO == 2 .AND. IFIRST==1)THEN
        DO L=1,LL
            IF(IABS(IXWORK(5,L)) == 1)THEN
                I3 = IXWORK(3,L)
                I6 = IXWORK(6,L)
c             print *,'edge I,J=',itab(IXWORK(1,L)),itab(IXWORK(2,L)),L
             IF(I3/=0)THEN
              I4 = IXWORK(4,L)
              J=I3+IADM
              MBINFLG(J) = BITSET(MBINFLG(J),I4)
              IF(IXWORK(5,L) == 1)MBINFLG(J) = BITSET(MBINFLG(J),6)
              MBINFLG(J) = BITSET(MBINFLG(J),8)
             END IF
             IF(I6/=0)THEN
              I7 = IXWORK(7,L)
              J=I6+IADM
              MBINFLG(J) = BITSET(MBINFLG(J),I7)
              IF(IXWORK(5,L) == 1)MBINFLG(J) = BITSET(MBINFLG(J),6)
c              MBINFLG(J) = BITSET(MBINFLG(J),8)
             END IF
            ENDIF
c only one of both segments connected to an edge is tagged
c            IF(IXWORK(5,L) == -1)THEN
c              I3 = IXWORK(6,L)
c              I4 = IXWORK(7,L)
c              J=I3+IADM
c              MBINFLG(J) = BITSET(MBINFLG(J),I4)
c              IF(IXWORK(5,L) == 1)MBINFLG(J) = BITSET(MBINFLG(J),6)
c              MBINFLG(J) = BITSET(MBINFLG(J),8)
c            ENDIF
        ENDDO
      ENDIF
c---------------------------------------
c       nombre de lignes: may keep only NACTIF
c---------------------------------------
      NACTIF = NACTIF + NL
c---------------------------------------
c     setup MBINFLG (IALLO == 2)
c---------------------------------------
#ifndef HYPERMESH_LIB
      IF(IALLO == 2 .AND. NL >0 .AND. IFIRST==1 )THEN
        IF(IPRI >= 5) THEN
          WRITE(IOUT,'(/,A,/)')' ACTIV SEGMENTS USED FOR EDGE'
          DO I=1,NL
            WRITE(IOUT,FMT=FMW_4I)(ITAB(IXWORK(K,I)),K=1,2)
          ENDDO
        ENDIF
      END IF
#endif
c---------------------------------------
c    edges on SECONDARY segments
c---------------------------------------
c
c    +-------------+-------------+  I=I1:first SECONDARY node on edge IJ
c    |            J|I2           |  J=I2:first SECONDARY node on edge IJ
c    |             |             |  S1=I3: left SECONDARY segment
c    |             |             |  K1=I4: local segment edge K1=[1-4]
c    |      I3     |      I6     |  I5=1 border edge => S2=K2=0
c    |           I4|I7           |  I5=-1 internal edge
c    |             |             |  S2=I6: right SECONDARY segment
c    |             |             |  K2=I7: local segment edge K2=[1-4]
c    |IM          I|I1         IP|  IM : previous SECONDARY node on seg S1
c    +-------------+-------------+  IP : next SECONDARY node on seg S2
c
c---------------------------------------
c       SECONDARY edges array
c---------------------------------------
C------re-look at the case IEDGE=2,3, et structure IXWORK, max_J for each I1_I2=2
      IF(IFIRST==2)THEN
        DO I=1,NUMNOD
            ISEADD_L(I) = 0                             
            ISH(I)=1                          
        ENDDO
        IF(IALLO/=2)THEN
          DO LL=1,NL
c           count number of edges per SECONDARY node
            I1=IXWORK(1,LL)
            I2=IXWORK(2,LL)
            ISEADD_L(I1) = ISEADD_L(I1) + 1                              
            ISEADD_L(I2) = ISEADD_L(I2) + 1                              
          ENDDO
          IADD=1
          DO I=1,NSN
            NSE = ISEADD_L(I)
            IADD = IADD+1+3*NSE                          
          ENDDO
          L24ADD = IADD-1
        ELSE
          DO I=1,NSN
            ISEADD(I) = 0                             
          ENDDO
          DO LL=1,NL
c           count number of edges per SECONDARY node
            I1=IXWORK(1,LL)
            I2=IXWORK(2,LL)
            ISEADD_L(I1) = ISEADD_L(I1) + 1                              
            ISEADD_L(I2) = ISEADD_L(I2) + 1                              
          ENDDO
          IADD=1
          DO I=1,NSN
            NSE = ISEADD_L(NSV(I))
            ISEADD(I) = IADD
            ISEDGE(IADD) = NSE
            IADD = IADD+1+3*NSE
          ENDDO
          DO I=1,NSN
            ISEADD_L(NSV(I)) = ISEADD(I)
          ENDDO
          DO LL=1,NL
c         store SECONDARY node in ISEDGE
            I1=IXWORK(1,LL)
            I2=IXWORK(2,LL)
            I3=IXWORK(3,LL)
            I4=IXWORK(4,LL)
            I5=IXWORK(5,LL)
            I6=IXWORK(6,LL)
            I7=IXWORK(7,LL)
            IADD = ISEADD_L(I1)
            NSE = ISEDGE(IADD)
            ISEDGE(IADD+ISH(I1)) = I2
            IF(I3==0)THEN
             IM=0
             IMJ=0
            ELSE
             K=KM1(I4)
             IM = IRECT(K,I3+IADM)
             K=KP2(I4)
             IMJ = IRECT(K,I3+IADM)
            END IF
            ISEDGE(IADD+NSE+ISH(I1)) = IM
            IF(I6==0)THEN
              IP = 0
              IPJ= 0
            ELSE
              K=KP2(I7)
              IP = IRECT(K,I6+IADM)
              K=KM1(I7)
              IPJ = IRECT(K,I6+IADM)
            ENDIF                          
            ISEDGE(IADD+2*NSE+ISH(I1)) = IP
            ISH(I1)=ISH(I1)+1
                          
            IADD = ISEADD_L(I2)
            NSE  = ISEDGE(IADD)
            ISEDGE(IADD+ISH(I2)) = I1
            ISEDGE(IADD+NSE+ISH(I2)) = IPJ
            ISEDGE(IADD+2*NSE+ISH(I2)) = IMJ
            ISH(I2)=ISH(I2)+1                          
          ENDDO
          
        ENDIF
                
      ENDIF
c---------------------------------------
      DEALLOCATE (INDEX)
c      DEALLOCATE (TAG)
      DEALLOCATE (ISEADD_L)
      DEALLOCATE (ISH)
      DEALLOCATE (IXWORK)
      DEALLOCATE (LINEIX)
      DEALLOCATE (LINEIX2)
      DEALLOCATE (XLINEIX)

C-----------
      RETURN
      END
Chd|====================================================================
Chd|  SH2SURF                       source/interfaces/inter3d1/i24surfi.F
Chd|-- called by -----------
Chd|        I24SURFI                      source/interfaces/inter3d1/i24surfi.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SH2SURF(NRTM0,IRECT,IEDG,MBINFLG,MSEGTYP,NRTM )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM0,IEDG,NRTM
      INTEGER IRECT(4,*),MBINFLG(*),MSEGTYP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NR, IT, J, ETYP(NRTM0),ITYPE,NRTM1
C=======================================================================
C------add asymmetric shell segs at end, change MSEGTYP
       DO I=1,NRTM0
        ETYP(I) = MSEGTYP(I)
       END DO
C       
       NR=NRTM0
       DO I=1,NRTM0
        MSEGTYP(I)=0
        IF(ETYP(I) ==3 .OR. ETYP(I) ==7 ) THEN
         NR =NR +1
         IRECT(1,NR)=IRECT(2,I)
         IRECT(2,NR)=IRECT(1,I)
         IRECT(3,NR)=IRECT(4,I)
         IRECT(4,NR)=IRECT(3,I)
         MSEGTYP(I)=NR
         MSEGTYP(NR)=-I
C------coating shell don't be doubled--         
        ELSEIF(ETYP(I) <0 ) THEN
C------but changing ordering--         
         IT= IRECT(1,I)
         IRECT(1,I)=IRECT(2,I)
         IRECT(2,I)=IT
         IT= IRECT(3,I)
         IRECT(3,I)=IRECT(4,I)
         IRECT(4,I)=IT
C------  coating shell tagged for STIF_ini, reset to zero after 
C--------coating shell >NRTM : avoid conflict       
         MSEGTYP(I)=-ETYP(I)+NRTM
        ELSEIF(ETYP(I) ==4 .OR. ETYP(I) ==8) THEN
         MSEGTYP(I)=ETYP(I)+NRTM
        END IF
       END DO
       NRTM1 = NR
       IF (IEDG> 0) THEN
        NR=NRTM0
        DO I=1,NRTM0
         IF(ETYP(I) ==3 .OR. ETYP(I) ==7 ) THEN
          NR =NR +1
          MBINFLG(NR)=MBINFLG(I)
         END IF
        END DO
       END IF !(IEDGE> 1) THEN
c        print *,'!!!NRTM0,NRTM1=', NRTM0,NRTM1
C------------------------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  IN24COQ_SOL3                  source/interfaces/inter3d1/i24surfi.F
Chd|-- called by -----------
Chd|        I24SURFI                      source/interfaces/inter3d1/i24surfi.F
Chd|        I25SURFI                      source/interfaces/inter3d1/i25surfi.F
Chd|-- calls ---------------
Chd|        SEG_INS                       source/interfaces/inter3d1/i24surfi.F
Chd|====================================================================
      SUBROUTINE IN24COQ_SOL3(IRECT ,IXC ,IXTG ,MSEGTYP   ,X  ,
     .                   KNOD2ELC ,KNOD2ELTG ,NOD2ELC ,NOD2ELTG,
     .                   KNOD2ELS,NOD2ELS,IXS ,IXS10 ,IXS16 ,IXS20 )
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
      INTEGER MSEGTYP, KNOD2ELS(*),NOD2ELS(*)
      INTEGER IRECT(4), IXC(NIXC,*), IXTG(NIXTG,*),
     .        KNOD2ELC(*) ,KNOD2ELTG(*) ,NOD2ELC(*) ,NOD2ELTG(*)
      INTEGER IXS(NIXS,*),IXS10(6,*), IXS16(8,*), IXS20(12,*)
C     REAL
      my_real
     .   X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, J, II, K, IAD ,NEL,NELTG,NS,NELS,NDS(20),NNOD,JJ,INS
C     REAL
C-----------------------------------------------
C-------if coating shell made of two segs, verify the value of MSEGTYP
C--if MSEGTYP=0, seg define -> if shell look at is also solid ; get at least 3n in sloid
C--if MSEGTYP shell -> same than before
C--if MSEGTYP solid -> nothing
      IF (MSEGTYP==3 .OR. MSEGTYP==7) GOTO 300
      IF (MSEGTYP/=0) RETURN
      NEL=0
      NELTG=0
      IF(IRECT(3)==IRECT(4).AND.NUMELTG/=0)THEN
       DO 230 IAD=KNOD2ELTG(IRECT(1))+1,KNOD2ELTG(IRECT(1)+1)
        N = NOD2ELTG(IAD)
        DO 220 J=1,3
          II=IRECT(J)
          DO K=1,3
            IF(IXTG(K+1,N)==II) GOTO 220
          END DO
          GOTO 230
  220   CONTINUE
        NELTG = N
  230  CONTINUE
      ENDIF
C
      IF(NUMELC/=0) THEN
       DO 430 IAD=KNOD2ELC(IRECT(1))+1,KNOD2ELC(IRECT(1)+1)
        N = NOD2ELC(IAD)
        DO 420 J=1,4
          II=IRECT(J)
          DO K=1,4
            IF(IXC(K+1,N)==II) GOTO 420
          END DO
          GOTO 430
  420   CONTINUE
        NEL = N
  430  CONTINUE
      ENDIF
C----
      IF (NEL>0) THEN
       MSEGTYP = 3
      ELSEIF(NELTG>0) THEN
       MSEGTYP = 7
      END IF
      
  300   CONTINUE
C------solid element  
      IF(MSEGTYP==0.OR.NUMELS==0) RETURN
      NELS=0
      NS = IRECT(1)
C
       DO 330 IAD=KNOD2ELS(NS)+1,KNOD2ELS(NS+1)
        N = NOD2ELS(IAD)
        IF(N <= NUMELS8)THEN
          DO 310 JJ=1,4
            II=IRECT(JJ)
            DO K=1,8
              IF(IXS(K+1,N)==II) GOTO 310
            ENDDO
            GOTO 330
  310     CONTINUE
          NDS(1:8)=IXS(2:9,N)
          NNOD = 8
        ELSEIF(N <= NUMELS8+NUMELS10)THEN
          DO 320 JJ=1,4
            II=IRECT(JJ)
            DO K=1,8
              IF(IXS(K+1,N)==II) GOTO 320
            ENDDO
            DO K=1,6
              IF(IXS10(K,N-NUMELS8)==II) GOTO 320
            ENDDO
            GOTO 330
  320     CONTINUE
          NDS(1)=IXS(2,N)
          NDS(2)=IXS(4,N)
          NDS(3)=IXS(7,N)
          NDS(4)=IXS(6,N)
          NDS(5:10)=IXS10(1:6,N-NUMELS8)
          NNOD = 10
        ELSEIF(N <= NUMELS8+NUMELS10+NUMELS20)THEN
          DO 322 JJ=1,4
            II=IRECT(JJ)
            DO K=1,8
              IF(IXS(K+1,N)==II) GOTO 322
            ENDDO
            DO K=1,12
              IF(IXS20(K,N-NUMELS8-NUMELS10)==II) GOTO 322
            ENDDO
            GOTO 330
  322     CONTINUE
          NDS(1:8)=IXS(2:9,N)
          NDS(9:20)=IXS20(1:12,N-NUMELS8-NUMELS10)
          NNOD = 20
        ELSEIF(N <= NUMELS8+NUMELS10+NUMELS20+NUMELS16)THEN
          DO 324 JJ=1,4
            II=IRECT(JJ)
            DO K=1,8
              IF(IXS(K+1,N)==II) GOTO 324
            ENDDO
            DO K=1,8
              IF(IXS16(K,N-NUMELS8-NUMELS10-NUMELS20)==II) GOTO 324
            ENDDO
            GOTO 330
  324     CONTINUE
          NDS(1:8)=IXS(2:9,N)
          NDS(9:16)=IXS16(1:8,N-NUMELS8-NUMELS10-NUMELS20)
          NNOD = 16
        ELSE
          GOTO 330
        END IF
        CALL SEG_INS(IRECT,NDS,NNOD,INS,X )
        IF (INS/=0) NELS = N
        IF (NELS>0) GOTO 500
  330  CONTINUE
  
  500  CONTINUE
      IF (NELS>0 .AND. (MSEGTYP==3 .OR. MSEGTYP==7)) THEN
       MSEGTYP = MSEGTYP + 1
       IF (INS <0) MSEGTYP=-MSEGTYP
      END IF
      
      RETURN
      END
Chd|====================================================================
Chd|  SEG_INS                       source/interfaces/inter3d1/i24surfi.F
Chd|-- called by -----------
Chd|        IN24COQ_SOL3                  source/interfaces/inter3d1/i24surfi.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SEG_INS(IRECT,NDS,NNOD,INS,X)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4),NDS(*),NNOD,INS
      my_real
     .   X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,K,M,LING,NN
      my_real
     .   X1,Y1,Z1,X41,Y41,Z41,X42,Y42,Z42,X43,Y43,Z43,NX,NY,NZ,VOL
C--------------------------------------------------------------------
        INS =0
        NN =0
        DO I=1,3
         II = IRECT(I)
         DO J=1,NNOD
          IF (NDS(J)==II) THEN
           NN = NN +1
           CYCLE
          END IF
         END DO
        END DO !I=1,3
        IF (NN>=3) INS =1
C--------compute the volume of solid center&IRECT(1-3), if V>0 inverse the normal
        IF (INS/=0) THEN
         X1=ZERO
         Y1=ZERO
         Z1=ZERO
         DO J=1,NNOD
          X1=X1+X(1,NDS(J))
          Y1=Y1+X(2,NDS(J))
          Z1=Z1+X(3,NDS(J))
         END DO
          X1=X1/NNOD
          Y1=Y1/NNOD
          Z1=Z1/NNOD
          X41 = X(1,IRECT(3)) - X1
          Y41 = X(2,IRECT(3)) - Y1
          Z41 = X(3,IRECT(3)) - Z1
          X42 = X(1,IRECT(3)) -  X(1,IRECT(1))
          Y42 = X(2,IRECT(3)) -  X(2,IRECT(1))
          Z42 = X(3,IRECT(3)) -  X(3,IRECT(1))
          X43 = X(1,IRECT(3)) -  X(1,IRECT(2))
          Y43 = X(2,IRECT(3)) -  X(2,IRECT(2))
          Z43 = X(3,IRECT(3)) -  X(3,IRECT(2))
C
          NX =  Y43*Z42 - Y42*Z43
          NY =  Z43*X42 - Z42*X43
          NZ =  X43*Y42 - X42*Y43
C
          VOL = X41*NX + Y41*NY + Z41*NZ
          IF (VOL > ZERO) INS= -1
        END IF       
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  I24EDGE2                      source/interfaces/inter3d1/i24surfi.F
Chd|-- called by -----------
Chd|        I24SURFI                      source/interfaces/inter3d1/i24surfi.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I24EDGE2(IALLO      ,NSEG    ,NACTIF  ,IEDGE   ,
     1                    SURF_NODES ,ITAB    ,ISU     ,
     2                    X          ,EDG_COS ,MBINFLG ,IADM    ,NLS     ,
     3                    IRECT      ,NRTSE   ,IRTSE   ,NSNE    ,IS2SE   ,
     4                    IS2PT      ,NSN     ,NSV     ,IS2ID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
#ifndef HYPERMESH_LIB
      USE MESSAGE_MOD
#endif
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IALLO,NACTIF,IEDGE,IADM,NLS,L24ADD,NSN,IFIRST,ISU
      INTEGER ITAB(*),MBINFLG(*),IRECT(4,*),NSEG,SURF_NODES(NSEG,4),
     .        IRTSE(5,*) ,NSV(*),NRTSE,NSNE,IS2SE(2,*),IS2PT(*),IS2ID(*)
      my_real
     .   X(3,*),EDG_COS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C----- NLS : Num. of element with active edge----
      INTEGER I,J,K,L,NLMAX,STAT,LL,I1,I2,I3,I4,I5,I1M,I2M,NL,IS,
     .        I3M,I4M,I6,I7,IADD,IM,IP,LI
      INTEGER NEXTK(4),KM1(4),KP2(4),IWORK(70000),NLL,NSE,NP_EDGE
      my_real
     .   NX,NY,NZ,MX,MY,MZ,AAA,D1X,D1Y,D1Z,D2X,D2Y,D2Z,IMJ,IPJ
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: 
     .   LINEIX,LINEIX2,IXWORK
      INTEGER, DIMENSION(:), ALLOCATABLE :: 
     .   INDEX,TAG,ISEADD_L,ISH
      my_real, DIMENSION(:,:), ALLOCATABLE :: 
     .   XLINEIX

      INTEGER BITSET
      EXTERNAL BITSET

      DATA NEXTK/2,3,4,1/
      DATA KM1/4,1,2,3/
      DATA KP2/3,4,1,2/
C=======================================================================
C--- edges are used only to select SECONDARY segments:IRTSE(5,NRTSE), 
C----IRTSE(5,*) -> id of edge
C=======================================================================
      NLMAX = 0
      NLS = 0
      IF(ISU /= 0)NLMAX = 4*NSEG
c---------------------------------------
c       LINEIX(2,*): LINE; LINEIX2(1,):Id_seg,LINEIX2(2,):Jd_seg(1-4)
c       IXWORK(8,*): reordered lines; (1-2,)<-LINEIX,(3-4,)or (6-7,) if inverse order of I1,I2
c                    <-LINEIX2,(5):I_bord; (8,):flag of inverse I1,I2
c---------------------------------------
      ALLOCATE (LINEIX(2,NLMAX)    ,STAT=stat)
      ALLOCATE (LINEIX2(2,NLMAX)   ,STAT=stat)
      ALLOCATE (XLINEIX(3,NLMAX)   ,STAT=stat)
      ALLOCATE (INDEX(2*NLMAX)     ,STAT=stat)
c      ALLOCATE (TAG(NUMNOD)        ,STAT=stat)
      ALLOCATE (IXWORK(8,NLMAX)    ,STAT=stat)

#ifndef HYPERMESH_LIB
      IF (STAT /= 0) THEN
          CALL ANCMSG(MSGID=268,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                         C1='LINEIX')
      END IF
#endif
c---------------------------------------
c       recherche de toutes les lignes dans la surface
c---------------------------------------
C initialize IXWORK to zero
      IXWORK(4,1:NLMAX)=0


      IF(ISU /= 0)THEN
        IS = 0
        LL = 0
        DO J=1,NSEG
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
            IF (I1==I2) CYCLE
            LL = LL+1
            IF(I2 > I1)THEN
              LINEIX(1,LL) = I1
              LINEIX(2,LL) = I2
              IXWORK(8,LL) = 0
            ELSE
              LINEIX(1,LL) = I2
              LINEIX(2,LL) = I1
C-----------means I1,I2 has been exchanged              
              IXWORK(8,LL) = 1
            ENDIF
            LINEIX2(1,LL) = J
            LINEIX2(2,LL) = K
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
        LI = INDEX(1)
        I1M = LINEIX(1,LI)
        I2M = LINEIX(2,LI)
        I3M = LINEIX2(1,LI)
        I4M = LINEIX2(2,LI)
        NL = 1
        IXWORK(1,NL)=I1M
        IXWORK(2,NL)=I2M
        IF(IXWORK(8,LI)==0)THEN
          IXWORK(3,NL)=I3M
          IXWORK(4,NL)=I4M
          IXWORK(6,NL)=0
        ELSE
          IXWORK(6,NL)=I3M
          IXWORK(7,NL)=I4M
          IXWORK(3,NL)=0
        ENDIF
C---------first -> border but can be corrected later
        IXWORK(5,NL)=1
        MX = XLINEIX(1,LI)
        MY = XLINEIX(2,LI)
        MZ = XLINEIX(3,LI)
        DO L=2,LL
          LI = INDEX(L)
          I1 = LINEIX(1,LI)
          I2 = LINEIX(2,LI)
          I3 = LINEIX2(1,LI)
          I4 = LINEIX2(2,LI)
          NX = XLINEIX(1,LI)
          NY = XLINEIX(2,LI)
          NZ = XLINEIX(3,LI)
          IF(I2 /= I2M .or. I1 /= I1M)THEN
c store new edge
            NL = NL + 1
            IXWORK(1,NL)=I1
            IXWORK(2,NL)=I2
            IF(IXWORK(8,LI)==0)THEN
              IXWORK(3,NL)=I3
              IXWORK(4,NL)=I4
              IXWORK(6,NL)=0
            ELSE
              IXWORK(6,NL)=I3  
              IXWORK(7,NL)=I4 
              IXWORK(3,NL)=0
            ENDIF
            IXWORK(5,NL)=1 ! bord
          ELSE
C------internal lines are not incremented because they are always double
C--------- and the second one does the correction
            IXWORK(5,NL)=0  ! interne
c           second segment
            IF(IXWORK(8,LI)==0)THEN
              IXWORK(3,NL)=I3
              IXWORK(4,NL)=I4
            ELSE
              IXWORK(6,NL)=I3  
              IXWORK(7,NL)=I4 
            ENDIF
            AAA = NX*MX + NY * MY + NZ * MZ
            IF (AAA < EDG_COS) IXWORK(5,NL) = -1 ! arete vive
          ENDIF
          I1M = I1
          I2M = I2
          MX = NX
          MY = NY
          MZ = NZ
        ENDDO
c---------------------------------------
c       suppression des lignes internes (IEDGE == 1)
c       dimension first
c---------------------------------------
        LL = NL
        NL = 0
        IF(IEDGE == 1)THEN
c         keep only border edges (IXWORK(5,L) == 1)
          DO L=1,LL
            IF(IXWORK(5,L) == 1)THEN
              NL = NL + 1
              I1=IXWORK(1,NL)
              I2=IXWORK(2,NL)
              I3=IXWORK(3,NL)
              I4=IXWORK(4,NL)
              I5=IXWORK(5,NL)
              I6=IXWORK(6,NL)
              I7=IXWORK(7,NL)
              IXWORK(1,NL)=IXWORK(1,L)
              IXWORK(2,NL)=IXWORK(2,L)
              IXWORK(3,NL)=IXWORK(3,L)
              IXWORK(4,NL)=IXWORK(4,L)
              IXWORK(5,NL)=1 ! border
              IXWORK(6,NL)=IXWORK(6,L)
              IXWORK(7,NL)=IXWORK(7,L)
              IXWORK(1,L)=I1  
              IXWORK(2,L)=I2  
              IXWORK(3,L)=I3  
              IXWORK(4,L)=I4  
              IXWORK(5,L)=I5 
              IXWORK(6,L)=I6  
              IXWORK(7,L)=I7 
            ENDIF
          ENDDO
        ELSEIF(IEDGE == 2)THEN
c         toutes les lignes sont conserves ET actives
          DO L=1,LL
            NL = NL + 1
            IF(IXWORK(5,L) == 0)IXWORK(5,L)=-1 ! all on +-1
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
              I6=IXWORK(6,NL)
              I7=IXWORK(7,NL)
              IXWORK(1,NL)=IXWORK(1,L)
              IXWORK(2,NL)=IXWORK(2,L)
              IXWORK(3,NL)=IXWORK(3,L)
              IXWORK(4,NL)=IXWORK(4,L)
              IXWORK(5,NL)=IXWORK(5,L)
C             IXWORK(5,NL)=+-1 ! bord on
              IXWORK(6,NL)=IXWORK(6,L)
              IXWORK(7,NL)=IXWORK(7,L)
              IXWORK(1,L)=I1  
              IXWORK(2,L)=I2  
              IXWORK(3,L)=I3  
              IXWORK(4,L)=I4  
              IXWORK(5,L)=I5  
              IXWORK(6,L)=I6  
              IXWORK(7,L)=I7 
           ENDIF
          ENDDO
        ENDIF
C
      ELSE
C       pas de surfaces
        NL = 0
      ENDIF
c---------------------------------------
c     setup MBINFLG (IALLO == 2)
c       tag segment with active edges
c       (only SECONDARY segment)
c---------------------------------------
      NP_EDGE=3
C------count NRTSE; each seg has only one edge :multi-seg <=4 if necessary 
C------(possible to use MBINFLG(*)=IRECTS(5,*) to remove multi-seg, but not easy to read
C-------no double fictive SECONDARY nodes----
C------NSN<-NSN0+NSNE;  3*4*NRTSE for IEDGE = 2  -> more interesting to coding seg/seg
C------
      IF(IALLO == 1 )THEN
        DO L=1,LL
            IF(IABS(IXWORK(5,L)) == 1)THEN
                I3 = IXWORK(3,L)
                I6 = IXWORK(6,L)
c             
             IF(I3/=0)THEN
              NRTSE = NRTSE + 1
              NSNE = NSNE + NP_EDGE
             END IF
             IF(I6/=0)THEN
              NRTSE = NRTSE + 1
              IF(I3==0) THEN
               NSNE = NSNE + NP_EDGE
              ELSE
               NSNE = NSNE + 1
              END IF
             END IF
            ENDIF
        ENDDO
      ELSEIF(IALLO == 2 )THEN
        DO L=1,LL
            IF(IABS(IXWORK(5,L)) == 1)THEN
                I3 = IXWORK(3,L)
                I6 = IXWORK(6,L)
c             print *,'edge I,J=',itab(IXWORK(1,L)),itab(IXWORK(2,L)),L
c             print *,'I3,I6,IADM=',I3,I6,IADM
             IF(I3/=0)THEN
              I4 = IXWORK(4,L)
              J=I3
              NRTSE = NRTSE + 1
              IRTSE(1:4,NRTSE)=SURF_NODES(J,1:4)
              IRTSE(5,NRTSE)=I4
c             print *,'IRTSE(j,NRTSE)=',(itab(IRTSE(k,NRTSE)),k=1,4),
c     +                  IRTSE(5,NRTSE)
              DO I = 1,NP_EDGE
               NSNE = NSNE + 1
               NSV(NSN+NSNE) = NUMNOD+NSNE
               IS2SE(1,NSNE) = NRTSE
               IF (I6/=0.AND.I/=NP_EDGE) THEN
                IS2SE(2,NSNE)=NRTSE+1
               ELSE
                IS2SE(2,NSNE)=0
               END IF
               IS2PT(NSNE) = I
              END DO
c             print *,'NRTSE,NSNE=',NRTSE,NSNE
C -----IF IS2SE(1,NSNE) >0 and IS2SE(2,NSNE) >0, order is inversed on IS2SE(2             
             END IF
             IF(I6/=0)THEN
              I7 = IXWORK(7,L)
              J=I6
              NRTSE = NRTSE + 1
              IRTSE(1:4,NRTSE)=SURF_NODES(J,1:4)
              IRTSE(5,NRTSE)=I7
c             print *,'IRTSE(j,NRTSE)=',(itab(IRTSE(k,NRTSE)),k=1,4),
c     +                  IRTSE(5,NRTSE)
              IF(I3==0) THEN
               DO I = 1,NP_EDGE
                NSNE = NSNE + 1
                NSV(NSN+NSNE) = NUMNOD+NSNE
                IS2SE(2,NSNE) = NRTSE
                IS2SE(1,NSNE) = 0
                IS2PT(NSNE) = I
               END DO
C------------------only  NP_EDGE_th node is added              
              ELSE
                NSNE = NSNE + 1
                NSV(NSN+NSNE) = NUMNOD+NSNE
                IS2SE(1,NSNE) = NRTSE
                IS2SE(2,NSNE) = 0
                IS2PT(NSNE) = NP_EDGE
              END IF
             END IF
            ENDIF
        ENDDO
      ENDIF
C        NSN = NSN + NSNE
c       print *,'IALLO,NSNE,NRTSE,NSN0=',IALLO,NSNE,NRTSE,NSN
c---------------------------------------
c       nombre de lignes: may keep only NACTIF
c---------------------------------------
      NACTIF = NACTIF + NL
c---------------------------------------
c     setup MBINFLG (IALLO == 2)
c---------------------------------------
#ifndef HYPERMESH_LIB
      IF(IALLO == 2 .AND. NL >0 )THEN
        IF(IPRI >= 5) THEN
          WRITE(IOUT,'(/,A,/)')' ACTIV SEGMENTS USED FOR EDGE'
          DO I=1,NL
            WRITE(IOUT,FMT=FMW_4I)(ITAB(IXWORK(K,I)),K=1,2)
          ENDDO
        ENDIF
      END IF
#endif
c---------------------------------------
c    edges on SECONDARY segments 
c---------------------------------------
c
c    +-------------+-------------+  I=I1:first SECONDARY node on edge IJ
c    |            J|I2           |  J=I2:first SECONDARY node on edge IJ
c    |             |             |  S1=I3: left SECONDARY segment
c    |             |             |  K1=I4: local segment edge K1=[1-4]
c    |      I3     |      I6     |  I5=1 border edge => S2=K2=0
c    |           I4|I7           |  I5=-1 internal edge
c    |             |             |  S2=I6: right SECONDARY segment
c    |             |             |  K2=I7: local segment edge K2=[1-4]
c    |IM          I|I1         IP|  IM : previous SECONDARY node on seg S1
c    +-------------+-------------+  IP : next SECONDARY node on seg S2
c
c---------------------------------------
c       SECONDARY edges array
c---------------------------------------
C------Change to simplify SPMD
      IF(IALLO ==2)THEN
       DO I = 1,NSNE
        IF (IS2SE(1,I)==0 .AND.IS2SE(2,I)/=0) THEN
         IS2SE(1,I) = IS2SE(2,I)
         IS2SE(2,I) = 0
        END IF
C IS2ID Give global internal ID for Each Fictive node.
C Useful in SPMD to easily find the SECONDARY
        IS2ID(I)=I
       END DO !I = 1,NSNE
      ENDIF
c---------------------------------------
      DEALLOCATE (INDEX)
c      DEALLOCATE (TAG)
      DEALLOCATE (IXWORK)
      DEALLOCATE (LINEIX)
      DEALLOCATE (LINEIX2)
      DEALLOCATE (XLINEIX)

C-----------
      RETURN
      END
Chd|====================================================================
Chd|  I24XFIC_INI                   source/interfaces/inter3d1/i24surfi.F
Chd|-- called by -----------
Chd|        I24BUC1                       source/interfaces/inter3d1/i24buc1.F
Chd|-- calls ---------------
Chd|        I24FIC_GETN                   source/interfaces/inter3d1/i24surfi.F
Chd|====================================================================
      SUBROUTINE I24XFIC_INI(NRTSE   ,IRTSE   ,NSNE    ,IS2SE   ,IS2PT   ,
     4                       NSN     ,NSV     ,X       ,XFIC    ,NPT     )
C============================================================================
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
      INTEGER IRTSE(5,*) ,NSV(*),NRTSE,NSNE,IS2SE(2,*),IS2PT(*),NSN,NPT
      my_real
     .   X(3,*),XFIC(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
! 4---------------3
! | .           . |
! |   .       .   |
! |     .   .     |
! |       .       |
! |     .   .     |
! |   .       .   |
! | .     o3    . |
! 1---o1------o2--2 NPT=3
C----- NLS : Num. of element with active edge----
      INTEGER I,J,K,NSN0,NS,IP,IK1(4),IK2(4),IE1,IE2,IED,NS1,NS2,IE,NP0
      DATA IK1 /1,2,3,4/
      DATA IK2 /2,3,4,1/
      my_real
     .   X0,Y0,Z0,XE0,YE0,ZE0,S
C=======================================================================
C----IRTSE(5,*) -> id of edge
C=======================================================================
      NSN0 = NSN-NSNE
      DO I=NSN0+1,NSN
       NS=NSV(I)-NUMNOD
       IF (NS<=0) print *,'!!!!error, NSV(I),I=',NSV(I),I
       CALL I24FIC_GETN(NS      ,IRTSE   ,IS2SE   ,IE    ,NS1     ,
     +                  NS2     )
       IP = IS2PT(NS)
       IF (IP==NPT) THEN
C-------seg center-------        
        IF (IRTSE(3,IE)==IRTSE(4,IE)) THEN
         X0=THIRD*(X(1,IRTSE(1,IE))+X(1,IRTSE(2,IE))+X(1,IRTSE(3,IE)))
         Y0=THIRD*(X(2,IRTSE(1,IE))+X(2,IRTSE(2,IE))+X(2,IRTSE(3,IE)))
         Z0=THIRD*(X(3,IRTSE(1,IE))+X(3,IRTSE(2,IE))+X(3,IRTSE(3,IE)))
        ELSE
         X0=FOURTH*(X(1,IRTSE(1,IE))+X(1,IRTSE(2,IE))+X(1,IRTSE(3,IE))+
     +             X(1,IRTSE(4,IE)))
         Y0=FOURTH*(X(2,IRTSE(1,IE))+X(2,IRTSE(2,IE))+X(2,IRTSE(3,IE))+
     +             X(2,IRTSE(4,IE)))
         Z0=FOURTH*(X(3,IRTSE(1,IE))+X(3,IRTSE(2,IE))+X(3,IRTSE(3,IE))+
     +             X(3,IRTSE(4,IE)))
        END IF
C-------edge center-------        
         XE0=HALF*(X(1,NS1)+X(1,NS2))
         YE0=HALF*(X(2,NS1)+X(2,NS2))
         ZE0=HALF*(X(3,NS1)+X(3,NS2))
C         
         XFIC(1,NS) = THIRD*(X0+TWO*XE0)
         XFIC(2,NS) = THIRD*(Y0+TWO*YE0)
         XFIC(3,NS) = THIRD*(Z0+TWO*ZE0)
C-------NPT should be unpair: 3,5,7         
       ELSEIF (IP > 0 ) THEN
C-------edge center-------        
         XE0=HALF*(X(1,NS1)+X(1,NS2))
         YE0=HALF*(X(2,NS1)+X(2,NS2))
         ZE0=HALF*(X(3,NS1)+X(3,NS2))
        NP0 = (NPT-1)/2
        IF (IP > NP0) THEN
C---------right side        
         S = (IP-NP0)*ONE/(NPT-1)
         XFIC(1,NS) = XE0 +S*(X(1,NS2)-XE0)
         XFIC(2,NS) = YE0 +S*(X(2,NS2)-YE0)
         XFIC(3,NS) = ZE0 +S*(X(3,NS2)-ZE0)         
        ELSE
C---------left side        
         S = IP*ONE/(NPT-1)
         XFIC(1,NS) = X(1,NS1) +S*(XE0 -X(1,NS1))
         XFIC(2,NS) = X(2,NS1) +S*(YE0 -X(2,NS1))
         XFIC(3,NS) = X(3,NS1) +S*(ZE0 -X(3,NS1))         
        END IF
       END IF
      END DO
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  I24FICS_INI                   source/interfaces/inter3d1/i24surfi.F
Chd|-- called by -----------
Chd|        I24STSECND                    source/interfaces/inter3d1/i24stslav.F
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I24FICS_INI(IRTSE   ,NSNE    ,IS2SE   ,NSV    ,IS2PT ,
     4                       NSN     ,FIC_S   )
C============================================================================
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
      INTEGER IRTSE(5,*) ,NSV(*),NSNE,IS2SE(2,*),NPT,IS2PT(*),NSN
      my_real
     .   FIC_S(*)
C------FIC_S could be GAP or STIFF-----------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
! 4---------------3
! | .           . |
! |   .       .   |
! |     .   .     |
! |       .       |
! |     .   .     |
! |   .       .   |
! | .     o3    . |
! 1---o1------o2--2 NPT=3
C----- NLS : Num. of element with active edge----
      INTEGER I,J,K,NSN0,NS,NSF,IE1,IE2,IED,NS1,NS2,IE,NP0,N
      INTEGER ITAG(NUMNOD),IK1(4),IK2(4)
      DATA IK1 /1,2,3,4/
      DATA IK2 /2,3,4,1/
      my_real
     .   X0,Y0,Z0,XE0,YE0,ZE0,S
C=======================================================================
C----IRTSE(5,*) -> id of edge
C=======================================================================
      NSN0 = NSN -NSNE
      DO I=1,NSN0
       N= NSV(I)
       ITAG(N) = I
      END DO
      DO I=1,NSNE
       IE1 = IS2SE(1,I)
       IE2 = IS2SE(2,I)
       IF (IE1 > 0) THEN
         IE = IE1
         IED=IRTSE(5,IE)
         NS1= IRTSE(IK1(IED),IE)
         NS2= IRTSE(IK2(IED),IE)
       ELSEIF(IE2 > 0) THEN
         IE = IE2
         IED=IRTSE(5,IE)
         NS1= IRTSE(IK2(IED),IE)
         NS2= IRTSE(IK1(IED),IE)
       ELSE
        print *,'problem EDGE **** I,IE1,IE2=',I,IE1,IE2
       END IF
        S = MAX(FIC_S(ITAG(NS1)),FIC_S(ITAG(NS2)))
        FIC_S(I+NSN0) = S
      END DO ! I=1,NSNE        
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  I24FICI_INI                   source/interfaces/inter3d1/i24surfi.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I24FICI_INI(IRTSE   ,NSNE    ,IS2SE   ,NSV    ,IS2PT ,
     4                       NSN     ,FIC_I   )
C============================================================================
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
      INTEGER IRTSE(5,*),NSV(*),NSNE,IS2SE(2,*),NPT,IS2PT(*),NSN,
     .        FIC_I(*)
C------FIC_S could be GAP or STIFF-----------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
! 4---------------3
! | .           . |
! |   .       .   |
! |     .   .     |
! |       .       |
! |     .   .     |
! |   .       .   |
! | .     o3    . |
! 1---o1------o2--2 NPT=3
C----- NLS : Num. of element with active edge----
      INTEGER I,J,K,NSN0,NS,NSF,IE1,IE2,IED,NS1,NS2,IE,NP0,N,IS
      INTEGER ITAG(NUMNOD),IK1(4),IK2(4)
      DATA IK1 /1,2,3,4/
      DATA IK2 /2,3,4,1/
      my_real
     .   X0,Y0,Z0,XE0,YE0,ZE0
C=======================================================================
C----IRTSE(5,*) -> id of edge
C=======================================================================
      NSN0 = NSN -NSNE
      DO I=1,NSN0
       N= NSV(I)
       ITAG(N) = I
      END DO
      DO I=1,NSNE
       IE1 = IS2SE(1,I)
       IE2 = IS2SE(2,I)
       IF (IE1 > 0) THEN
         IE = IE1
         IED=IRTSE(5,IE)
         NS1= IRTSE(IK1(IED),IE)
         NS2= IRTSE(IK2(IED),IE)
       ELSEIF(IE2 > 0) THEN
         IE = IE2
         IED=IRTSE(5,IE)
         NS1= IRTSE(IK2(IED),IE)
         NS2= IRTSE(IK1(IED),IE)
       ELSE
        print *,'problem EDGE **** I,IE1,IE2=',I,IE1,IE2
       END IF
        IS = MAX(FIC_I(ITAG(NS1)),FIC_I(ITAG(NS2)))
        FIC_I(I+NSN0) = IS
      END DO ! I=1,NSNE        
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  I24ISEGPT_INI                 source/interfaces/inter3d1/i24surfi.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        I24FIC_GETN                   source/interfaces/inter3d1/i24surfi.F
Chd|====================================================================
      SUBROUTINE I24ISEGPT_INI(IRTSE   ,NSNE    ,IS2SE   ,NSV    ,IS2PT ,
     4                         NSN     ,ISEGPT  ,NPT , ISPT2)
C============================================================================
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
      INTEGER IRTSE(5,*),NSV(*),NSNE,IS2SE(2,*),NPT,IS2PT(*),NSN,
     .        ISEGPT(*), ISPT2(*)
C------FIC_S could be GAP or STIFF-----------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
! 4---------------3
! | .           . |
! |   .       .   |
! |     .   .     |
! |       .       |
! |     .   .     |
! |   .       .   |
! | .     o3    . |
! 1---o1------o2--2 NPT=3
C----- NLS : Num. of element with active edge----
      INTEGER I,J,K,NSN0,NS,IP,NS1,NS2,IE,NP0,N,IS1,IS2,IPT
      INTEGER ITAG(NUMNOD)
C=======================================================================
C----IRTSE(5,*) -> id of edge
C=======================================================================
      NSN0 = NSN -NSNE
      DO I=1,NSN0
       N= NSV(I)
       ITAG(N) = I
       ISEGPT(I) = 0
       ISPT2(I) = 0
      END DO
C------fictive nodes first -----      
      DO I=NSN0+1,NSN
       ISPT2(I) = 1
       NS=NSV(I)-NUMNOD
       IP = IS2PT(NS)
       IF (IP == NPT) THEN
C-------internal is negative of id him-self       
        ISEGPT(I) = -I
       ELSEIF (IP == 1.OR.(IP == NPT-1)) THEN
        CALL I24FIC_GETN(NS      ,IRTSE   ,IS2SE   ,IE    ,NS1     ,
     +                  NS2     )
C-------on the edge is positive of id him-self       
C        IPT = I + NPT -IP
        ISEGPT(I) = I
C-------------ISEGPT(IS1,IS2) have not unique seg, takes the first one  
        IF (IP==1) THEN      
         IS1 = ITAG(NS1)
         IF (ISEGPT(IS1) ==0) ISEGPT(IS1)=I
        ELSE
         IS2 = ITAG(NS2)
         IF (ISEGPT(IS2) ==0) ISEGPT(IS2)=I
        END IF
       ELSE
        ISEGPT(I) = I
       END IF
      END DO
C-----------
      RETURN
      END

Chd|====================================================================
Chd|  ISPT2_INI                     source/interfaces/inter3d1/i24surfi.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ISPT2_INI(CAND_N, I_STOK, NSN, IRTLM,
     *                     ISEGPT, ISPT2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
         INTEGER, INTENT(IN)    :: CAND_N(*)
         INTEGER, INTENT(IN)    :: I_STOK
         INTEGER, INTENT(IN)    :: NSN
         INTEGER, INTENT(IN)    :: IRTLM(NSN)
         INTEGER, INTENT(IN)    :: ISEGPT(NSN)
         INTEGER, INTENT(INOUT) :: ISPT2(NSN)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
         INTEGER I,NI,NSI
C-----------------------------------------------
	 ISPT2(1:NSN) = 0
         DO I=1,I_STOK
             NI=CAND_N(I)
             IF(NI <= NSN)THEN
               NSI = ISEGPT(NI)
               IF(NSI > 0)THEN
                  IF(IRTLM(NSI) /=0)THEN
                      ISPT2(NI) = 0
                  ELSE
                      ISPT2(NI) = 1
                  ENDIF
               ELSEIF(NSI<0)THEN
                 ISPT2(NI) = 1
               ENDIF
             ENDIF

         ENDDO  

         
      END

Chd|====================================================================
Chd|  I24FIC_GETN                   source/interfaces/inter3d1/i24surfi.F
Chd|-- called by -----------
Chd|        I24ISEGPT_INI                 source/interfaces/inter3d1/i24surfi.F
Chd|        I24PEN3                       source/interfaces/inter3d1/i24pen3.F
Chd|        I24TRI                        source/interfaces/inter3d1/i24tri.F
Chd|        I24XFIC_INI                   source/interfaces/inter3d1/i24surfi.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I24FIC_GETN(NS      ,IRTSE   ,IS2SE   ,IE    ,NS1     ,
     4                       NS2     )
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRTSE(5,*) ,NS,IS2SE(2,*),NS1,NS2,IE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C----- get edge NS1,NS2 and--SECONDARY seg id :IE-
      INTEGER IK1(4),IK2(4),IE1,IE2,IED
      DATA IK1 /1,2,3,4/
      DATA IK2 /2,3,4,1/
C=======================================================================
C----IRTSE(5,*) -> id of edge
C=======================================================================
       IE1 = IS2SE(1,NS)
       IE2 = IS2SE(2,NS)
       IF (IE1 > 0) THEN
         IE = IE1
         IED=IRTSE(5,IE)
         NS1= IRTSE(IK1(IED),IE)
         NS2= IRTSE(IK2(IED),IE)
       ELSEIF(IE2 > 0) THEN
         IE = IE2
         IED=IRTSE(5,IE)
         NS1= IRTSE(IK2(IED),IE)
         NS2= IRTSE(IK1(IED),IE)
       ELSE
        print *,'problem EDGE IE1,IE2=',IE1,IE2
#ifndef HYPERMESH_LIB
        call arret(2)
#endif
       END IF
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  I24FICV_INI                   source/interfaces/inter3d1/i24surfi.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I24FICV_INI(IRTSE   ,NSNE    ,IS2SE   ,NSV    ,IS2PT ,
     +                       NSN     ,FIC_V   ,NPT   )
C============================================================================
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
      INTEGER IRTSE(5,*) ,NSV(*),NSNE,IS2SE(2,*),NPT,IS2PT(*),NSN
C-------due to the using of PENE_OLD(5,*)-----      
      my_real
     .   FIC_V(5,*)
C------FIC_S could be GAP or STIFF-----------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
! 4---------------3
! | .           . |
! |   .       .   |
! |     .   .     |
! |       .       |
! |     .   .     |
! |   .       .   |
! | .     o3    . |
! 1---o1------o2--2 NPT=3
C----- NLS : Num. of element with active edge----
      INTEGER I,J,K,NSN0,NS,NSF,IE1,IE2,IED,NS1,NS2,IE,NP0,N
      INTEGER ITAG(NUMNOD),IK1(4),IK2(4),IP
      DATA IK1 /1,2,3,4/
      DATA IK2 /2,3,4,1/
      my_real
     .   NX,NY,NZ,DET
C=======================================================================
C----IRTSE(5,*) -> id of edge
C=======================================================================
      NSN0 = NSN -NSNE
      DO I=1,NSN0
       N= NSV(I)
       ITAG(N) = I
      END DO
      DO I=1,NSNE
       IE1 = IS2SE(1,I)
       IE2 = IS2SE(2,I)
       IF (IE1 > 0) THEN
         IE = IE1
         IED=IRTSE(5,IE)
         NS1= IRTSE(IK1(IED),IE)
         NS2= IRTSE(IK2(IED),IE)
       ELSEIF(IE2 > 0) THEN
         IE = IE2
         IED=IRTSE(5,IE)
         NS1= IRTSE(IK2(IED),IE)
         NS2= IRTSE(IK1(IED),IE)
       ELSE
        print *,'problem EDGE **** IE1,IE2=',IE1,IE2
       END IF
        IP = IS2PT(I)
        NX=ZERO
        NY=ZERO
        NZ=ZERO
C------mean value of IE        
        IF (IP==NPT) THEN
C------mean value of NS1,NS2 
         DO J=1,3
          N = ITAG(IRTSE(J,IE))
          NX = NX + FIC_V(1,N)
          NY = NY + FIC_V(2,N)
          NZ = NZ + FIC_V(3,N)
         END DO
         IF (IRTSE(3,IE)/=IRTSE(4,IE)) THEN
          N = ITAG(IRTSE(4,IE))
          NX = NX + FIC_V(1,N)
          NY = NY + FIC_V(2,N)
          NZ = NZ + FIC_V(3,N)
         END IF
        ELSE
          N = ITAG(NS1)
          NX = NX + FIC_V(1,N)
          NY = NY + FIC_V(2,N)
          NZ = NZ + FIC_V(3,N)
          N = ITAG(NS2)
          NX = NX + FIC_V(1,N)
          NY = NY + FIC_V(2,N)
          NZ = NZ + FIC_V(3,N)
        END IF
         DET = ONE/MAX(EM20,SQRT(NX*NX+ NY*NY+ NZ*NZ))
         N = I + NSN0
         FIC_V(1,N) = DET*NX         
         FIC_V(2,N) = DET*NY         
         FIC_V(3,N) = DET*NZ         
      END DO ! I=1,NSNE        
C-----------
      RETURN
      END







