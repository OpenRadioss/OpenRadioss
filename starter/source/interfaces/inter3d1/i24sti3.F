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
Chd|  I24STI3                       source/interfaces/inter3d1/i24sti3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        FRICTION_PARTS_SEARCH         source/interfaces/inter3d1/i7sti3.F
Chd|        I24BORD                       source/interfaces/inter3d1/i24sti3.F
Chd|        I24GAPM                       source/interfaces/inter3d1/i24sti3.F
Chd|        I24NORMNS                     source/interfaces/inter3d1/i24sti3.F
Chd|        INSOL3ET                      source/interfaces/inter3d1/i24sti3.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|====================================================================
      SUBROUTINE I24STI3(
     1 X     ,IRECT ,STF   ,IXS   ,PM    ,
     2 GEO   ,NRT   ,IXC   ,NINT  ,STFAC ,
     3        NTY   ,GAP   ,NOINT ,
     4 STFN  ,NSN   ,MS    ,NSV   ,IXTG  ,
     5 IGAP  ,WA    ,GAP_S ,GAP_M ,GAPMIN,
     6 IXT   ,IXP   ,GAPINF,GAPMAX_S,
     9 INACTI ,KNOD2ELS ,KNOD2ELC ,KNOD2ELTG ,NOD2ELS ,
     A NOD2ELC,NOD2ELTG ,IGRSURF    ,INTTH,
     B IELES  ,IELEC    ,AREAS    ,SH4TREE ,SH3TREE ,
     C IPART  ,IPARTC   ,IPARTTG  ,THK ,THK_PART    ,
     D IXR    ,ITAB    ,BGAPSMX   ,IXS10   ,MSEGTYP ,
     E NRT_SH ,IXS16    ,IXS20    ,GAP_N   ,MVOISN  ,
     F ILEV   ,IGRSURF2   ,GAPMAX_M ,ID,TITR ,IGAP0   ,
     G PEN_OLD,IPARTNS  ,IPARTS   ,IGEO    ,FILLSOL ,
     H PM_STACK, IWORKSH ,INTFRIC ,TAGPRT_FRIC,IPARTFRICS,
     G IPARTFRICM,INTBUF_FRIC_TAB ,INTNITSCHE,NRTS,IRECTS,
     I IELNRTS ,ADRECTS ,FACNRTS  ,NMN       ,MSR ,
     J IPARTT ,IPARTP   ,IPARTR   ,ELEM_LINKED_TO_SEGMENT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD 
      USE INTBUF_FRIC_MOD  
      USE GROUPDEF_MOD
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
#include      "remesh_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRT, NINT, NTY, NOINT,NSN,IGAP,INTFRIC,NMN,
     .        INACTI,NRT_SH ,ILEV ,IGAP0,INTNITSCHE,NRTS,IGEO(NPROPGI,*)
      INTEGER IRECT(4,*), IXS(NIXS,*), IXC(NIXC,*),
     .   NSV(*), IXTG(NIXTG,*), IXT(NIXT,*), IXP(NIXP,*),
     .   KNOD2ELS(*), KNOD2ELC(*), KNOD2ELTG(*), NOD2ELS(*), NOD2ELC(*), 
     .   NOD2ELTG(*), IELES(*), INTTH, IELEC(*),
     .   SH3TREE(KSH3TREE,*), SH4TREE(KSH4TREE,*),IXR(NIXR,*) ,
     .   IPART(LIPART1,*), IPARTC(*), IPARTTG(*),
     .   ITAB(*), IXS10(6,*),MSEGTYP(*), IXS16(8,*), IXS20(12,*),MVOISN(4,*),
     .   IWORKSH(3,*),TAGPRT_FRIC(*),IPARTFRICS(*),IPARTFRICM(*),
     .   IRECTS(4,*),IELNRTS(*),ADRECTS(4,*),FACNRTS(*),MSR(*)
C     REAL
      my_real
     .   STFAC, GAP,GAPMIN,GAPINF, GAPMAX_S,BGAPSMX ,GAPMAX_M
C     REAL
      my_real
     .   X(3,*), STF(*), PM(NPROPM,*), GEO(NPROPG,*), STFN(*),
     .   MS(*),WA(*),GAP_S(*),GAP_M(*),GAP_N(12,*),
     .   AREAS(*),THK(*),THK_PART(*),PEN_OLD(5,NSN), FILLSOL(*),
     .   PM_STACK(20,*)
      INTEGER ID,IPARTNS(*),IPARTS(*)
      INTEGER, DIMENSION(NUMELT), INTENT(IN) :: IPARTT
      INTEGER, DIMENSION(NUMELP), INTENT(IN) :: IPARTP
      INTEGER, DIMENSION(NUMELR), INTENT(IN) :: IPARTR
      INTEGER, DIMENSION(NUMELS), INTENT(INOUT):: ELEM_LINKED_TO_SEGMENT !< working array, dim=numels
      CHARACTER*nchartitle,
     .   TITR
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
      TYPE (SURF_) :: IGRSURF
      TYPE (SURF_) :: IGRSURF2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NDX, I, J, INRT, NELS, MT, JJ, JJJ, NELC,
     .   MG, NUM, NPT, LL, L, NN, NELTG,N1,N2,N3,N4,IE,
     .   IP, NLEV, MYLEV, K, P, R, T,IAD,NRT1,NRT2,NSHIF,
     .   TAGB(NUMNOD),NS,IGTYP,NRTT,ISUBSTACK,IPL,IPFMAX,
     .   IPFLMAX,NM,NEL,FC,PERM,NSHIFF,N,IPG
     
      INTEGER JPERM(4) ,FACES(4,6),TAB1(4),TAB2(4),FACES10(3,16),FACES20(4,24)
C     REAL
      my_real
     .   DXM, GAPMX, GAPMN, AREA, VOL, DX,GAPS1,GAPS2, GAPM, DDX, 
     .   GAPTMP, GAPSCALE,SX1,SY1,SZ1,SX2,SY2,SZ2,SX3,SY3,SZ3,
     .   SLSFAC,XL,GAPS_MN
      INTEGER, DIMENSION(:),ALLOCATABLE ::TAGNOD
      DATA JPERM/2,3,4,1/
      DATA FACES/1,2,3,4,
     .           1,2,6,5,
     .           2,3,7,6,
     .           3,4,8,7,
     .           1,5,8,4,
     .           5,6,7,8/
      DATA FACES10/1,11,14,
     .             3,11,15,
     .             5,14,15,
     .             11,14,15,
     .             1,13,14,
     .             6,13,16,
     .             5,14,16,
     .             13,14,16,
     .             3,11,12,
     .             6,12,13,
     .             1,11,13,
     .             11,12,13,
     .             3,12,15,
     .             6,12,16,
     .             5,15,16,
     .             12,15,16/
C--------------------------------------------------------------
C     CALCUL DES RIGIDITES DES SEGMENTS 
C     V16 : DANS LE CAS OU ONE SEGMENT APPARTIENT A LA FOIS
C           A UNE BRIQUE ET A UNE COQUE ON CHOISIT LA RIGIDITE
C           DE LA COQUE SAUF SI LE MATERIAU COQUE EST NUL.
C---------------------------------------------------------------
C      NRT->NRT0
C---  MVOISN is used temporarily for Pen_ini MVOISN(1,*) -> MTYPE(solid),MVOISN(2,*) -> E_id
C-----MVOISN(3,*) -> part_id, IPARTNS->part_id(SECONDARY)
       SLSFAC = STFAC
      DXM=ZERO
      NDX=0
      NSHIF=0
      GAPMX=EP30
      GAPMN=EP30
      GAPS1=ZERO
      GAPS2=ZERO
      GAPS_MN=EP30
      GAPSCALE = ONE
C-----NRTT:NRTM      
C     NRT_SH nb of shells before symetrization, NRT nb of MAIN segments before symetrization (symetrization in i24surfi)
      NRTT =NRT+NRT_SH
C------------------------------------
C     GAP NOEUDS SECONDS
C------------------------------------
      DO I=1,NUMNOD
        WA(I)=ZERO
      ENDDO
      DO I=1,NUMELC
        MG=IXC(6,I)
        IP = IPARTC(I)
        IGTYP = IGEO(11,MG)
       IF ( THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN
        DX=HALF*THK_PART(IP)
       ELSEIF ( THK(I) /= ZERO .AND. IINTTHICK == 0) THEN
        DX=HALF*THK(I)
       ELSEIF(IGTYP == 17 .OR. IGTYP ==51 .OR. IGTYP ==52) THEN
          DX=HALF*THK(I)         
       ELSE
          DX=HALF*GEO(1,MG)
       ENDIF
        WA(IXC(2,I))=MAX(WA(IXC(2,I)),DX)
        WA(IXC(3,I))=MAX(WA(IXC(3,I)),DX)
        WA(IXC(4,I))=MAX(WA(IXC(4,I)),DX)
        WA(IXC(5,I))=MAX(WA(IXC(5,I)),DX)
      ENDDO
      DO I=1,NUMELTG
        MG=IXTG(5,I)
        IP = IPARTTG(I)
        IGTYP = IGEO(11,MG)
       IF ( THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN
        DX=HALF*THK_PART(IP)
       ELSEIF ( THK(NUMELC+I) /= ZERO .AND. IINTTHICK == 0) THEN
        DX=HALF*THK(NUMELC+I)
       ELSEIF(IGTYP == 17 .OR. IGTYP ==51 .OR. IGTYP ==52) THEN
        DX=HALF*THK(NUMELC+I)
       ELSE
        DX=HALF*GEO(1,MG)
       ENDIF
        WA(IXTG(2,I))=MAX(WA(IXTG(2,I)),DX)
        WA(IXTG(3,I))=MAX(WA(IXTG(3,I)),DX)
        WA(IXTG(4,I))=MAX(WA(IXTG(4,I)),DX)
      ENDDO
C-----for case of coating shell--
      IF (ILEV/=3) THEN
       DO I=1,NUMNOD
        TAGB(I) = 0
       END DO
       DO I=1,NRT
        IF (MSEGTYP(I) /= 0) THEN
         DO J =1,4
          NN= IRECT(J,I)
          TAGB(NN) = 1
         END DO
        END IF
       END DO
       DO I=1,NUMNOD
        IF (TAGB(I)==0) WA(I)=0
       END DO
      END IF
C-------
      DO I=1,NUMELT
        MG=IXT(4,I)
        IP = IPARTT(I)
        IF ( THK_PART(IP) > ZERO ) THEN
          DX=HALF*THK_PART(IP)
        ELSE
          DX=HALF*SQRT(GEO(1,MG))
        END IF
        WA(IXT(2,I))=MAX(WA(IXT(2,I)),DX)
        WA(IXT(3,I))=MAX(WA(IXT(3,I)),DX)
      ENDDO
      DO I=1,NUMELP
        MG=IXP(5,I)
        IP = IPARTP(I)
        IF ( THK_PART(IP) > ZERO ) THEN
          DX=HALF*THK_PART(IP)
        ELSE
          DX=HALF*SQRT(GEO(1,MG))
        END IF
        WA(IXP(2,I))=MAX(WA(IXP(2,I)),DX)
        WA(IXP(3,I))=MAX(WA(IXP(3,I)),DX)
      ENDDO
      DO I=1,NUMELR
        IP = IPARTR(I)
        IF ( THK_PART(IP) > ZERO ) THEN
          MG=IXR(1,I)
          IGTYP = IGEO(11,MG)
          DX=HALF*THK_PART(IP)
          WA(IXR(2,I))=MAX(WA(IXR(2,I)),DX)
          WA(IXR(3,I))=MAX(WA(IXR(3,I)),DX)
          IF (IGTYP==12) WA(IXR(4,I))=MAX(WA(IXR(4,I)),DX)
       END IF
      ENDDO
      DO I=1,NSN
        GAP_S(I)=GAPSCALE * WA(NSV(I))
                GAP_S(I)=MIN(GAP_S(I),GAPMAX_S)
      ENDDO
C---------put SECONDARY node on the free edge to GAP=0
      IF(IGAP0 > 0)THEN
        DO I=1,NUMNOD
         TAGB(I)=0
        ENDDO
C       
        IF(ILEV /= 3 )THEN
          CALL I24BORD(IGRSURF2%NSEG ,IGRSURF2%NODES ,TAGB)
        ENDIF
        IF(ILEV == 2)THEN
          CALL I24BORD(IGRSURF%NSEG ,IGRSURF%NODES ,TAGB)
        ENDIF
       DO I=1,NSN
        NS = NSV(I)
        IF( TAGB(NS) > 0 ) GAP_S(I) = EM20
       ENDDO
      ENDIF
C      
      DO I=1,NSN
        GAPS1=MAX(GAPS1,GAP_S(I))
        GAPS_MN=MIN(GAPS_MN,GAP_S(I))
      ENDDO
C calcul du surface second. ---
      IF(INTTH > 0 ) THEN
        IF(NADMESH==0)THEN
          DO I = 1,NSN    
             AREAS(I) = ZERO
             DO J= KNOD2ELC(NSV(I))+1,KNOD2ELC(NSV(I)+1)
               IE = NOD2ELC(J)
               SX1 = X(1,IXC(4,IE)) - X(1,IXC(2,IE))
               SY1 = X(2,IXC(4,IE)) - X(2,IXC(2,IE))
               SZ1 = X(3,IXC(4,IE)) - X(3,IXC(2,IE))
               SX2 = X(1,IXC(5,IE)) - X(1,IXC(3,IE))
               SY2 = X(2,IXC(5,IE)) - X(2,IXC(3,IE))
               SZ2 = X(3,IXC(5,IE)) - X(3,IXc(3,IE))
               SX3  = SY1*SZ2 - SZ1*SY2
               SY3  = SZ1*SX2 - SX1*SZ2
               SZ3  = SX1*SY2 - SY1*SX2
               AREAS(I) = AREAS(I) 
     .                    + ONE_OVER_8*SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
C overwrite
               IELEC(I) = IXC(1,IE)
             END DO
C
             DO J= KNOD2ELTG(NSV(I))+1,KNOD2ELTG(NSV(I)+1)
               IE = NOD2ELTG(J)
               SX1 = X(1,IXTG(3,IE)) - X(1,IXTG(2,IE))
               SY1 = X(2,IXTG(3,IE)) - X(2,IXTG(2,IE))
               SZ1 = X(3,IXTG(3,IE)) - X(3,IXTG(2,IE))
               SX2 = X(1,IXTG(4,IE)) - X(1,IXTG(2,IE))
               SY2 = X(2,IXTG(4,IE)) - X(2,IXTG(2,IE))
               SZ2 = X(3,IXTG(4,IE)) - X(3,IXTG(2,IE))
               SX3  = SY1*SZ2 - SZ1*SY2
               SY3  = SZ1*SX2 - SX1*SZ2
               SZ3  = SX1*SY2 - SY1*SX2
               AREAS(I) = AREAS(I) 
     .                    + ONE_OVER_6*SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
C overwrite
               IELEC(I) = IXTG(1,IE)
             END DO
          END DO
        ELSE
          DO I = 1,NSN    
             AREAS(I) = ZERO
             DO J= KNOD2ELC(NSV(I))+1,KNOD2ELC(NSV(I)+1)
               IE = NOD2ELC(J)

               IP = IPARTC(IE)
               NLEV =IPART(10,IP)
               MYLEV=SH4TREE(3,IE)
               IF(MYLEV < 0) MYLEV=-(MYLEV+1)

               IF(MYLEV==NLEV)THEN                 
                 SX1 = X(1,IXC(4,IE)) - X(1,IXC(2,IE))
                 SY1 = X(2,IXC(4,IE)) - X(2,IXC(2,IE))
                 SZ1 = X(3,IXC(4,IE)) - X(3,IXC(2,IE))
                 SX2 = X(1,IXC(5,IE)) - X(1,IXC(3,IE))
                 SY2 = X(2,IXC(5,IE)) - X(2,IXC(3,IE))
                 SZ2 = X(3,IXC(5,IE)) - X(3,IXC(3,IE))
                 SX3  = SY1*SZ2 - SZ1*SY2
                 SY3  = SZ1*SX2 - SX1*SZ2
                 SZ3  = SX1*SY2 - SY1*SX2
                 AREAS(I) = AREAS(I) 
     .                      + ONE_OVER_8*SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
C overwrite
                 IELEC(I) = IXC(1,IE)
               END IF

             END DO
C
            DO J= KNOD2ELTG(NSV(I))+1,KNOD2ELTG(NSV(I)+1)
               IE = NOD2ELTG(J)

               IP = IPARTTG(IE)
               NLEV =IPART(10,IP)
               MYLEV=SH3TREE(3,IE)
               IF(MYLEV < 0) MYLEV=-(MYLEV+1)

               IF(MYLEV==NLEV)THEN                 
                 SX1 = X(1,IXTG(3,IE)) - X(1,IXTG(2,IE))
                 SY1 = X(2,IXTG(3,IE)) - X(2,IXTG(2,IE))
                 SZ1 = X(3,IXTG(3,IE)) - X(3,IXTG(2,IE))
                 SX2 = X(1,IXTG(4,IE)) - X(1,IXTG(2,IE))
                 SY2 = X(2,IXTG(4,IE)) - X(2,IXTG(2,IE))
                 SZ2 = X(3,IXTG(4,IE)) - X(3,IXTG(2,IE))
                 SX3  = SY1*SZ2 - SZ1*SY2
                 SY3  = SZ1*SX2 - SX1*SZ2
                 SZ3  = SX1*SY2 - SY1*SX2
                 AREAS(I) = AREAS(I) 
     .                      + ONE_OVER_6*SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
C overwrite
                 IELEC(I) = IXTG(1,IE)
               END IF

             END DO
          END DO
        END IF
      END IF

C -----Friction model  SECONDARY nodes parts------
C-----------if node connects to both shell and solid -> takes shell    

      IF(INTFRIC > 0) THEN

        IF(NUMELS/=0)THEN
          DO I = 1,NSN   
             IPFMAX = 0
             IPFLMAX = 0
             DO J= KNOD2ELS(NSV(I))+1,KNOD2ELS(NSV(I)+1)
               IE = NOD2ELS(J)
               IP = IPARTS(IE)
               IPG = TAGPRT_FRIC(IP)
               IF(IPG > 0.AND.IP>IPFMAX) THEN
                  CALL FRICTION_PARTS_SEARCH (
     .                           IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                           INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL) 
                  IF(IPL /=0) THEN
                     IPFMAX = IP
                     IPFLMAX = IPL
                  ENDIF
               ENDIF
            ENDDO
C
C
            IF(IPFMAX/=0) THEN
              IPARTFRICS(I) = IPFLMAX
            ENDIF

          ENDDO
        ENDIF 

        IF(NUMELC/=0.OR.NUMELTG/=0) THEN
          DO I = 1,NSN  
             IPFMAX = 0
             IPFLMAX = 0
             DO J= KNOD2ELC(NSV(I))+1,KNOD2ELC(NSV(I)+1)
               IE = NOD2ELC(J)
               IP = IPARTC(IE)
               IPG = TAGPRT_FRIC(IP)
               IF(IPG > 0.AND.IP>IPFMAX) THEN
                  CALL FRICTION_PARTS_SEARCH (
     .                           IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                             INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL)  
                  IF(IPL /=0) THEN
                     IPFMAX = IP
                     IPFLMAX = IPL
                  ENDIF
               ENDIF
            ENDDO

C
            DO J= KNOD2ELTG(NSV(I))+1,KNOD2ELTG(NSV(I)+1)
               IE = NOD2ELTG(J)
               IP = IPARTTG(IE)
               IPG = TAGPRT_FRIC(IP)
               IF(IPG > 0.AND.IP>IPFMAX) THEN
                  CALL FRICTION_PARTS_SEARCH (
     .                           IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                           INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL) 

                  IF(IPL /=0) THEN
                     IPFMAX = IP
                     IPFLMAX = IPL
                  ENDIF
               ENDIF
            ENDDO
C
            IF(IPFMAX/=0) THEN
              IPARTFRICS(I) = IPFLMAX
            ENDIF

          ENDDO
        ENDIF  
      ENDIF 

C----------------------------------
C -----NITSCHE method for contact : construction of tabs needed to compute equivalent nodal force------
       IF(INTNITSCHE > 0 ) THEN
C IRECTS tab : case NRTS=NRTM, similar to irect 
C                   but irects  (seg -> SECONDARY or MAIN node) / irect (seg -> local node)

          ALLOCATE(TAGNOD(NUMNOD))
          TAGNOD(1:NUMNOD)=0
          DO NM=1,NMN
            TAGNOD(MSR(NM))=NM
          END DO
         
          DO I=1,NRTS
            DO J=1,4
              NM = TAGNOD(IRECT(J,I))
              IRECTS(J,I) = NM    
            ENDDO
          ENDDO

          DEALLOCATE(TAGNOD)

C IELENRTS tab : Element number for each SECONDARY segment
          IF (ILEV==2) THEN
             NRT1=IGRSURF2%NSEG
             DO I=1,NRT1
                NEL=IGRSURF2%ELEM(I)
                IF(IGRSURF2%ELTYP(I)==1 ) THEN
                   IELNRTS(I) = NEL
                ENDIF
             ENDDO
             NSHIFF = NRT1
             NRT2=IGRSURF%NSEG
             DO I=1,NRT2
                NEL=IGRSURF%ELEM(I)
                IF(IGRSURF%ELTYP(I) == 1 ) THEN
                   IELNRTS(NSHIFF+I) = NEL
                ENDIF
             ENDDO
          ELSE
             DO I=1,NRT
                NEL=IGRSURF%ELEM(I)
                IF(IGRSURF%ELTYP(I) == 1 ) THEN
                   IELNRTS(I) = NEL
                ENDIF
             ENDDO
          ENDIF

C ADRECTS tab : Adress of each SECONDARY node in element connectivity for PARITH/ON computation
          ADRECTS (1:4,1:NRT) = 0
          DO I=1,NRT
            IE = IELNRTS(I)
            N1 = IRECT(1,I)    
            N2 = IRECT(2,I)                           
            N3 = IRECT(3,I)                           
            N4 = IRECT(4,I) 
            

            IF(IE > 0) THEN

             IF (IE <= NUMELS8 ) THEN

                DO K=1,4
                  DO J=1,8
                     IF(ADRECTS(K,I)==0) THEN
                       N=IXS(J+1,IE)
                       IF(N==IRECT(K,I)) THEN
                         ADRECTS(K,I) = J
                       ENDIF
                     ENDIF
                   ENDDO
                ENDDO

               IF(N3==N4) THEN
                 DO K=1,4
                    IF(ADRECTS(K,I) == 5) THEN
                       ADRECTS(K,I) = 6
                    ELSEIF(ADRECTS(K,I) == 6) THEN
                     ADRECTS(K,I) = 5
                    ENDIF
                 ENDDO
               ENDIF         

             ELSEIF(IE <= NUMELS8+NUMELS10 ) THEN
                 DO K=1,3
                    DO J=1,6
                       N=IXS10(J,IE-NUMELS8)
                       IF(N==IRECT(K,I)) THEN
                          ADRECTS(K,I) = 10 +J
                       ENDIF
                     ENDDO
                     DO J=1,8          
                         IF(ADRECTS(K,I)==0) THEN

                          N=IXS(J+1,IE)
                          IF(N==IRECT(K,I)) THEN
                            ADRECTS(K,I) = J
                          ENDIF
                       ENDIF
                     ENDDO

                  ENDDO
             ELSEIF(IE <= NUMELS8+NUMELS10+NUMELS20 ) THEN
                 DO K=1,4
                    DO J=1,12
                       N=IXS20(J,IE-NUMELS8-NUMELS10)
                       IF(N==IRECT(K,I)) THEN
                          ADRECTS(K,I) = 20 +J
                       ENDIF
                     ENDDO
                     DO J=1,8
                       IF(ADRECTS(K,I)==0) THEN
                          N=IXS(J+1,IE)
                          IF(N==IRECT(K,I)) THEN
                            ADRECTS(K,I) = J
                          ENDIF
                       ENDIF
                     ENDDO
                  ENDDO
             ELSEIF(IE <= NUMELS8+NUMELS10+NUMELS20+NUMELS16)THEN
                 DO K=1,4
                    DO J=1,8
                       N=IXS20(J,IE-NUMELS8-NUMELS10-NUMELS20)
                       IF(N==IRECT(K,I)) THEN
                          ADRECTS(K,I) = 40 +J
                       ENDIF
                     ENDDO
                     DO J=1,8
                       IF(ADRECTS(K,I)==0) THEN
                          N=IXS(J+1,IE)
                          IF(N==IRECT(K,I)) THEN
                            ADRECTS(K,I) = J
                          ENDIF
                        ENDIF
                     ENDDO

                  ENDDO
             ENDIF

            ENDIF ! IE >0

          ENDDO ! NRTS

C FACNRTS tab : Facet number in element connectuvty for each SECONDARY segment for PARITH/ON computation
          DO I=1,NRT
            IE = IELNRTS(I)
            N1 = IRECT(1,I)    
            N2 = IRECT(2,I)                           
            N3 = IRECT(3,I)                           
            N4 = IRECT(4,I) 
           
            IF(IE > 0) THEN 

             IF(IE<= NUMELS8 ) THEN
               IF(N3 /= N4) THEN
                  TAB1(1) = N1
                  TAB1(2) = N2
                  TAB1(3) = N3
                  TAB1(4) = N4
                  DO K=1,4
                     DO J=1,4-K
                       IF(TAB1(J+1) < TAB1(J)) THEN
                         PERM = TAB1(J+1)
                         TAB1(J+1) = TAB1(J)
                         TAB1(J) = PERM
                       ENDIF
                     ENDDO
                  ENDDO
        
                  DO FC=1,6
                     TAB2(1) = IXS(FACES(1,FC)+1,IE)
                     TAB2(2) = IXS(FACES(2,FC)+1,IE)
                     TAB2(3) = IXS(FACES(3,FC)+1,IE)
                     TAB2(4) = IXS(FACES(4,FC)+1,IE)
                     DO K=1,4
                        DO J=1,4-K
                         IF(TAB2(J+1) < TAB2(J)) THEN
                           PERM = TAB2(J+1)
                           TAB2(J+1) = TAB2(J)
                           TAB2(J) = PERM
                         ENDIF
                        ENDDO
                     ENDDO
                     IF(TAB1(1)==TAB2(1).AND.TAB1(2)==TAB2(2).AND.TAB1(3)==TAB2(3)) THEN
                        FACNRTS(I) = FC
                        EXIT
                     ENDIF
                  ENDDO
               ELSE
                  TAB1(1) = N1
                  TAB1(2) = N2
                  TAB1(3) = N3

                  DO K=1,3
                     DO J=1,3-K
                       IF(TAB1(J+1) < TAB1(J)) THEN
                         PERM = TAB1(J+1)
                         TAB1(J+1) = TAB1(J)
                         TAB1(J) = PERM
                      ENDIF
                    ENDDO
                 ENDDO
        
                 DO FC=1,6
                    N1 = IXS(FACES(1,FC)+1,IE)
                    N2 = IXS(FACES(2,FC)+1,IE)
                    N3 = IXS(FACES(3,FC)+1,IE)
                    N4 = IXS(FACES(4,FC)+1,IE)
                    TAB2(1) =N1
                    IF(N1/=N2.AND.N2/=N3) THEN
                      TAB2(2) =N2
                      TAB2(3) =N3
                    ELSEIF(N1/=N2) THEN
                      TAB2(2) =N2
                      TAB2(3) =N4
                    ELSEIF(N2/=N3) THEN
                      TAB2(2) =N3
                      TAB2(3) =N4
                    ELSE
                      EXIT
                    ENDIF
                    DO K=1,3
                      DO J=1,3-K
                        IF(TAB2(J+1) < TAB2(J)) THEN
                           PERM = TAB2(J+1)
                           TAB2(J+1) = TAB2(J)
                           TAB2(J) = PERM
                        ENDIF
                      ENDDO
                    ENDDO
                    IF(TAB1(1)==TAB2(1).AND.TAB1(2)==TAB2(2).AND.TAB1(3)==TAB2(3)) THEN
                       FACNRTS(I) = FC
                       EXIT
                    ENDIF
                  ENDDO
               ENDIF
         
            ELSEIF(IE<= NUMELS8+NUMELS10  ) THEN
               TAB1(1) = ADRECTS(1,I)
               TAB1(2) = ADRECTS(2,I)
               TAB1(3) = ADRECTS(3,I)
               DO K=1,3
                  DO J=1,3-K
                    IF(TAB1(J+1) < TAB1(J)) THEN
                       PERM = TAB1(J+1)
                       TAB1(J+1) = TAB1(J)
                       TAB1(J) = PERM
                    ENDIF
                   ENDDO
               ENDDO
               DO FC=1,16
                  IF(TAB1(1)==FACES10(1,FC).AND.TAB1(2)==FACES10(2,FC).AND.TAB1(3)==FACES10(3,FC)) THEN
                     FACNRTS(I) = FC
                     EXIT
                  ENDIF
                ENDDO

            ELSEIF(IE <= NUMELS8+NUMELS10+NUMELS20 ) THEN
!          Not available yet
            ENDIF

           ENDIF ! IE >0

         ENDDO ! NRTS

        ENDIF ! NITSHCHE

C
C------------------------------------
C     GAP STIF FACES MAIN
C------------------------------------
      IF (ILEV==2) THEN
C------------ISU1 first
       NRT1=IGRSURF2%NSEG
       CALL I24GAPM(
     1 X     ,IRECT ,STF   ,IXS   ,PM    ,
     2 GEO   ,NRT1  ,IXC   ,NINT  ,STFAC ,
     3 NTY   ,GAP   ,NOINT ,STFN  ,NSN   ,
     4 MS    ,NSV   ,IXTG  ,IGAP  ,GAP_M ,
     6 IXT   ,IXP   ,SLSFAC,DXM   ,NDX   ,
     9 KNOD2ELS ,KNOD2ELC ,KNOD2ELTG ,NOD2ELS ,
     A NOD2ELC,NOD2ELTG ,IGRSURF2 ,INTTH,
     B IELES  ,IELEC    ,AREAS    ,SH4TREE ,SH3TREE ,
     C IPART  ,IPARTC   ,IPARTTG  ,THK ,THK_PART    ,
     D IXR    ,ITAB    ,BGAPSMX   ,IXS10   ,MSEGTYP ,
     E IXS16  ,IXS20   ,GAP_N     ,GAPS1   ,GAPS2   ,
     F GAPMX  , GAPMN  ,GAPSCALE  ,NSHIF   ,GAPMAX_M,
     G ID     ,TITR    ,IGEO      ,FILLSOL ,NRTT    ,
     H PM_STACK, IWORKSH,INTFRIC ,TAGPRT_FRIC,IPARTFRICS,
     I IPARTFRICM,IPARTS,INTBUF_FRIC_TAB ,ELEM_LINKED_TO_SEGMENT)
       NRT2=IGRSURF%NSEG
       NSHIF = NRT1
       CALL I24GAPM(
     1 X     ,IRECT ,STF   ,IXS   ,PM    ,
     2 GEO   ,NRT2  ,IXC   ,NINT  ,STFAC ,
     3 NTY   ,GAP   ,NOINT ,STFN  ,NSN   ,
     4 MS    ,NSV   ,IXTG  ,IGAP  ,GAP_M ,
     6 IXT   ,IXP   ,
     8 SLSFAC,DXM   ,NDX   ,
     9 KNOD2ELS ,KNOD2ELC ,KNOD2ELTG ,NOD2ELS ,
     A NOD2ELC,NOD2ELTG ,IGRSURF  ,INTTH,
     B IELES  ,IELEC    ,AREAS    ,SH4TREE ,SH3TREE ,
     C IPART  ,IPARTC   ,IPARTTG  ,THK ,THK_PART    ,
     D IXR    ,ITAB    ,BGAPSMX   ,IXS10   ,MSEGTYP ,
     E IXS16  ,IXS20   ,GAP_N     ,GAPS1   ,GAPS2   ,
     F GAPMX  , GAPMN  ,GAPSCALE  ,NSHIF   ,GAPMAX_M,
     G ID     ,TITR    ,IGEO      ,FILLSOL ,NRTT  ,
     H PM_STACK , IWORKSH,INTFRIC,TAGPRT_FRIC,IPARTFRICS,
     I IPARTFRICM,IPARTS,INTBUF_FRIC_TAB ,ELEM_LINKED_TO_SEGMENT)
      ELSE
       CALL I24GAPM(
     1 X     ,IRECT ,STF   ,IXS   ,PM    ,
     2 GEO   ,NRT   ,IXC   ,NINT  ,STFAC ,
     3 NTY   ,GAP   ,NOINT ,STFN  ,NSN   ,
     4 MS    ,NSV   ,IXTG  ,IGAP  ,GAP_M ,
     6 IXT   ,IXP   ,SLSFAC,DXM   ,NDX   ,
     9 KNOD2ELS ,KNOD2ELC ,KNOD2ELTG ,NOD2ELS ,
     A NOD2ELC,NOD2ELTG ,IGRSURF  ,INTTH,
     B IELES  ,IELEC    ,AREAS    ,SH4TREE ,SH3TREE ,
     C IPART  ,IPARTC   ,IPARTTG  ,THK ,THK_PART    ,
     D IXR    ,ITAB    ,BGAPSMX   ,IXS10   ,MSEGTYP ,
     E IXS16  ,IXS20   ,GAP_N     ,GAPS1   ,GAPS2   ,
     F GAPMX  , GAPMN  ,GAPSCALE  ,NSHIF   ,GAPMAX_M,
     G ID     ,TITR    ,IGEO      ,FILLSOL ,NRTT ,
     H PM_STACK , IWORKSH,INTFRIC,TAGPRT_FRIC,IPARTFRICS,
     I IPARTFRICM,IPARTS,INTBUF_FRIC_TAB ,ELEM_LINKED_TO_SEGMENT)
      END IF


C---------------------------
C     GAP 
C---------------------------
      GAPMX=SQRT(GAPMX)
      GAPMX=MIN(GAPMX,GAPMAX_M)
C GAP VARIABLE :
C    - GAPMIN CONTIENT ONE GAP MINIMUM UTILISE SI GAP_S(I)+GAP_M(J) < GAPMIN
C    - GAP CONTIENT LE SUP DE (GAP_S(I)+GAP_M(J),GAPMIN) 
      IF(GAP<=ZERO)THEN
           IF(NDX/=0)THEN
             GAPMIN = GAPMN
             GAPMIN = MIN(HALF*GAPMX,GAPMIN)
           ELSE
C             GAPMIN = EM01 * GAPMX
             GAPMIN = ZERO
           ENDIF
C           WRITE(IOUT,1300)GAPMIN
      ELSE
           GAPMIN = GAP
      ENDIF
C------recalculate GAP_MIN,MAX      
      GAPMX=ZERO
      GAPMN=EP30
      DO I=1,NRT
       GAPMX=MAX(GAPMX,GAP_M(I))
       GAPMN=MIN(GAPMN,GAP_M(I))
      END DO
      IF(IPRI>=1) THEN
      IF(GAP<=ZERO)THEN
        WRITE(IOUT,1400)GAPS_MN,GAPS1
        WRITE(IOUT,1500)GAPMN,GAPMX
      END IF
      END IF!(IPRI>=1) THEN
C SUP DES GAPS VARIABLES
      GAP = GAPS1+GAPS2
C---------------------------------------------
C     MISE A ONE DU MULTIPLICATEUR NODALE DES RIGIDITES 
C---------------------------------------------
      DO 610 L=1,NSN
         STFN(L) = ONE
 610  CONTINUE
C
C Calcul du gap reel a utiliser lors du critere de retri
C
      BGAPSMX = ZERO
      GAPINF=EP30
      DO I = 1, NSN
          GAPINF = MIN(GAPINF,GAP_S(I))
          BGAPSMX = MAX(BGAPSMX,GAP_S(I))
      ENDDO
      DO I = 1, NRT
          GAPINF = MIN(GAPINF,GAP_M(I))
      ENDDO
      GAPINF=MAX(GAPINF,GAPMIN)
C---  MVOISN is used temporarily for Pen_ini MVOISN(1,*) -> MTYPE(solid),MVOISN(2,*) -> E_id
      DO I=1,NRT
        CALL INSOL3ET(X   ,IRECT ,IXS     ,NINT   ,MVOISN(2,I),I ,
     .                AREA ,NOINT ,KNOD2ELS,NOD2ELS,IXS10 ,
     .                IXS16,IXS20 ,MVOISN(1,I))
C-------supposing only small segments (sub-triangles) for 10 nodes tetras --------------
        IF (MVOISN(1,I)==10) THEN
C---Verify this factor 3-------------   
          GAP_N(1,I) = THREE*ONE_OVER_8*GAP_N(1,I)
          STF(I) = SIXTEEN*STF(I)
        ELSEIF (MVOISN(1,I)==16) THEN
          GAP_N(1,I) = GAP_N(1,I)/4
        END IF
      END DO
C-----reset MSEGTYP(I)=0 for coating shell, engine uses MSEGTYP only for symmetry
C------do it at end of init3, used for i24pen3....
c       DO I=1,NRT
c        IF (MSEGTYP(I)==-4.OR.MSEGTYP(I)==-8) MSEGTYP(I) =0 
c       END DO
C------initialize MSEGTYP and asymmetric shell part 
c      IAD=ISURF(3)+1
c      CALL I24NIMTYP(NRT ,IBUFSSG(IAD),MSEGTYP,NRT_SH)
      IF (NRT_SH>0) THEN
       J=NRT
       DO I=1,NRT
        IF (MSEGTYP(I) > 0 .AND.MSEGTYP(I)<=NRTT ) THEN
         J = J + 1
         STF(J) = STF(I)
         GAP_M(J)=GAP_M(I)
         IF(INTTH > 0 ) IELES(J) = IELES(I)
        IF(INTFRIC > 0) IPARTFRICM(J)=IPARTFRICM(I)
        END IF
       END DO
      END IF      
c      print*,'NOINT',NOINT, BGAPSMX    
C---------------------------------------------
C     CALCULATE NODAL NORMAL FOR SECONDARY NODES 
C---------------------------------------------
      IF (INACTI/=0) THEN
       CALL I24NORMNS(
     1 X     ,IRECT ,NRT   ,NSN   ,NSV   ,PEN_OLD, STF)
C------nodal part_id      
        DO I=1,NUMNOD
         TAGB(I)=0
        ENDDO
C-----------if node connects to both shell and solid -> take solid's
       DO I=1,NUMELC
        IP = IPARTC(I)
        DO J=1,4
        TAGB(IXC(1+J,I))=IP
        ENDDO
       ENDDO
       DO I=1,NUMELTG
        IP = IPARTTG(I)
        DO J=1,3
         TAGB(IXTG(1+J,I))=IP
        ENDDO
       ENDDO
C----factulative for Truss and beam
c       DO I=1,NUMELT
c           IP = IPARTT(I)
c        TAGB(IXT(2,I))=IP
c        TAGB(IXT(3,I))=IP
c       ENDDO
c       DO I=1,NUMELP
c           IP = IPARTP(I)
c        TAGB(IXP(2,I))=IP
c        TAGB(IXP(3,I))=IP
c       ENDDO
C-------solid elements
       DO I=1,NRT
        IF (MVOISN(2,I)>0) THEN
         IP = IPARTS(MVOISN(2,I))
         MVOISN(3,I) =IP
         DO J=1,4
          TAGB(IRECT(J,I))=IP
         ENDDO
        END IF
       END DO
       DO I=1,NSN
        NS = NSV(I)
        IPARTNS(I) = TAGB(NS) 
C-------to not have wrong equality IPART_NS=IPART_E  with 0      
        IF (IPARTNS(I)==0) IPARTNS(I) =-1
       ENDDO
C-------shell elements
       J=NRT
       DO I=1,NRT
        IF (MSEGTYP(I) > 0 .AND.MSEGTYP(I)<=NRTT) THEN
         J = J + 1
         IP = TAGB(IRECT(1,I))
         MVOISN(3,I) =IP
         MVOISN(3,J) =IP
        END IF
       END DO
      END IF

      RETURN
 1300 FORMAT(2X,'GAP MIN = ',1PG20.13)
 1400 FORMAT(2X,'MIN,MAX OF SECONDARY GAP: ',2(1PG20.13))
 1500 FORMAT(2X,'MIN,MAX OF MAIN GAP: ',2(1PG20.13)/)
      END
Chd|====================================================================
Chd|  INSOL3ET                      source/interfaces/inter3d1/i24sti3.F
Chd|-- called by -----------
Chd|        I24STI3                       source/interfaces/inter3d1/i24sti3.F
Chd|        I25STI3                       source/interfaces/inter3d1/i25sti3.F
Chd|        REMN_SELF24                   source/interfaces/inter3d1/remn_self24.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INSOL3ET(X   ,IRECT ,IXS     ,NINT   ,NEL,I ,
     .                  AREA ,NOINT ,KNOD2ELS,NOD2ELS,IXS10 ,
     .                  IXS16,IXS20 ,NNOD    )
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
      INTEGER NINT, NEL, I, NOINT,NNOD
      my_real
     .   AREA
      INTEGER IRECT(4,*), IXS(NIXS,*), KNOD2ELS(*), NOD2ELS(*), 
     .        IXS10(6,*), IXS16(8,*), IXS20(12,*)
      my_real
     .   X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IY(4), N, JJ, II, K, NN, KK, IC, IAD,
     .        NUSER, NUSERM
C     REAL
      my_real
     .   N1, N2, N3, DDS
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C
      NEL=0
      IC=0
      NNOD = 0
      IF(NUMELS==0) RETURN
       NUSERM = -1
       DO 230 IAD=KNOD2ELS(IRECT(1,I))+1,KNOD2ELS(IRECT(1,I)+1)
        N = NOD2ELS(IAD)
        IF(N <= NUMELS8)THEN
          DO 210 JJ=1,4
            II=IRECT(JJ,I)
            DO K=1,8
              IF(IXS(K+1,N)==II) GOTO 210
            ENDDO
            GOTO 230
  210     CONTINUE
          IC=IC+1
          NUSER = IXS(11,N)
          IF (NUSER>NUSERM) THEN
            NEL = N
            NUSERM = NUSER
          ENDIF
          NNOD = 8
        ELSEIF(N <= NUMELS8+NUMELS10)THEN
          DO 220 JJ=1,4
            II=IRECT(JJ,I)
            DO K=1,8
              IF(IXS(K+1,N)==II) GOTO 220
            ENDDO
            DO K=1,6
              IF(IXS10(K,N-NUMELS8)==II) GOTO 220
            ENDDO
            GOTO 230
  220     CONTINUE
          IC=IC+1
          NUSER = IXS(11,N)
          IF (NUSER>NUSERM) THEN
            NEL = N
            NUSERM = NUSER
          ENDIF
          NNOD = 10
        ELSEIF(N <= NUMELS8+NUMELS10+NUMELS20)THEN
          DO 222 JJ=1,4
            II=IRECT(JJ,I)
            DO K=1,8
              IF(IXS(K+1,N)==II) GOTO 222
            ENDDO
            DO K=1,12
              IF(IXS20(K,N-NUMELS8-NUMELS10)==II) GOTO 222
            ENDDO
            GOTO 230
  222     CONTINUE
          IC=IC+1
          NUSER = IXS(11,N)
          IF (NUSER>NUSERM) THEN
            NEL = N
            NUSERM = NUSER
          ENDIF
          NNOD = 20
        ELSEIF(N <= NUMELS8+NUMELS10+NUMELS20+NUMELS16)THEN
          DO 224 JJ=1,4
            II=IRECT(JJ,I)
            DO K=1,8
              IF(IXS(K+1,N)==II) GOTO 224
            ENDDO
            DO K=1,8
              IF(IXS16(K,N-NUMELS8-NUMELS10-NUMELS20)==II) GOTO 224
            ENDDO
            GOTO 230
  224     CONTINUE
          IC=IC+1
          NUSER = IXS(11,N)
          IF (NUSER>NUSERM) THEN
            NEL = N
            NUSERM = NUSER
          ENDIF
          NNOD = 16
        ELSE
          GOTO 230
        END IF
  230  CONTINUE
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  I24GAPM                       source/interfaces/inter3d1/i24sti3.F
Chd|-- called by -----------
Chd|        I24STI3                       source/interfaces/inter3d1/i24sti3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRICTION_PARTS_SEARCH         source/interfaces/inter3d1/i7sti3.F
Chd|        I4GMX3                        source/interfaces/inter3d1/i4gmx3.F
Chd|        INCOQ3                        source/interfaces/inter3d1/incoq3.F
Chd|        INELTC                        source/interfaces/inter3d1/inelt.F
Chd|        INELTS_NP                     source/interfaces/inter3d1/i24sti3.F
Chd|        INSOL3D                       source/interfaces/inter3d1/insol3.F
Chd|        VOLINT                        source/interfaces/inter3d1/volint.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I24GAPM(
     1 X     ,IRECT ,STF   ,IXS   ,PM    ,
     2 GEO   ,NRT   ,IXC   ,NINT  ,STFAC ,
     3 NTY   ,GAP   ,NOINT ,STFN  ,NSN   ,
     4 MS    ,NSV   ,IXTG  ,IGAP  ,GAP_M ,
     6 IXT   ,IXP   ,SLSFAC,DXM   ,NDX   ,
     9 KNOD2ELS ,KNOD2ELC ,KNOD2ELTG ,NOD2ELS ,
     A NOD2ELC,NOD2ELTG ,IGRSURF  ,INTTH,
     B IELES  ,IELEC    ,AREAS    ,SH4TREE ,SH3TREE ,
     C IPART  ,IPARTC   ,IPARTTG  ,THK ,THK_PART    ,
     D IXR    ,ITAB    ,BGAPSMX   ,IXS10   ,MSEGTYP ,
     E IXS16  ,IXS20   ,GAP_N     ,GAPS1   ,GAPS2   ,
     F GAPMX  , GAPMN  ,GAPSCALE  ,NSHIFT  ,GAPMAX_M,
     G ID     ,TITR    ,IGEO      ,FILLSOL ,NRTT    ,
     H PM_STACK , IWORKSH ,INTFRIC,TAGPRT_FRIC,IPARTFRICS,
     I IPARTFRICM,IPARTS,INTBUF_FRIC_TAB,ELEM_LINKED_TO_SEGMENT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
      USE INTBUF_FRIC_MOD  
      USE GROUPDEF_MOD
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
#include      "scr17_c.inc"
#include      "scr08_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRT, NINT, NTY, NOINT,NSN,IGAP, NDDIM,NDX,INTFRIC
      INTEGER IRECT(4,*), IXS(NIXS,*), IXC(NIXC,*),
     .   NSV(*), IXTG(NIXTG,*), IXT(NIXT,*), IXP(NIXP,*),
     .   KNOD2ELS(*), KNOD2ELC(*), KNOD2ELTG(*), NOD2ELS(*), NOD2ELC(*), 
     .   NOD2ELTG(*), IELES(*), INTTH, IELEC(*),
     .   SH3TREE(KSH3TREE,*), SH4TREE(KSH4TREE,*),IXR(NIXR,*) ,
     .   IPART(LIPART1,*), IPARTC(*), IPARTTG(*),
     .   ITAB(*), IXS10(6,*),MSEGTYP(*), IXS16(*), IXS20(*),NSHIFT,
     .   IGEO(NPROPGI,*),NRTT,IWORKSH(3,*),TAGPRT_FRIC(*),IPARTFRICS(*),
     .   IPARTFRICM(*),IPARTS(*)
      INTEGER, DIMENSION(NUMELS), INTENT(INOUT):: ELEM_LINKED_TO_SEGMENT !< working array, dim=numels
C     REAL
      my_real
     .   STFAC, GAP,BGAPSMX,GAPS1 ,GAPS2,GAPMX ,GAPMN ,GAPSCALE
C     REAL
      my_real
     .   X(3,*), STF(*), PM(NPROPM,*), GEO(NPROPG,*), STFN(*),
     .   MS(*),GAP_M(*),GAP_N(12,*),
     .   AREAS(*),THK(*),THK_PART(*),SLSFAC,DXM ,GAPMAX_M, FILLSOL(*),
     .   PM_STACK(3,*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*) 
      TYPE (SURF_) :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, INRT, NELS, MT, JJ, JJJ, NELC,
     .   MG, NUM, NPT, LL, L, NN, NELTG,N1,N2,N3,N4,IE,
     .   IP, NLEV, MYLEV, K, P, R, T,IAD,NREV,IGTYP,IPGMAT,IGMAT,
     .   ISUBSTACK,IPL,IPG,NINV
      INTEGER, DIMENSION(:),ALLOCATABLE  :: TAGELEMS,INDEXE
C     REAL
      my_real
     .   AREA, VOL, DX, GAPM, DDX, 
     .   GAPTMP, SX1,SY1,SZ1,SX2,SY2,SZ2,SX3,SY3,SZ3,
     .   XL,STIFC,ST,IELEM(2)
      LOGICAL :: PRINT_ERROR
      INTEGER, DIMENSION(4) :: NODE_ID
C----------------------
      NREV=0
      IPGMAT = 700
      IF(NUMELS > 0) THEN
        CALL MY_ALLOC(TAGELEMS,NUMELS)
        TAGELEMS = 0
        CALL MY_ALLOC(INDEXE,NUMELS)
        INDEXE = 0
      ENDIF
      NINV = 0
      DO I=1+NSHIFT,NRT+NSHIFT
       STF(I)=ZERO
       IF(INTTH > 0 ) IELES(I) = 0
       IF(SLSFAC<ZERO)STF(I)=SLSFAC
       GAPM  =ZERO
       GAP_M(I)=GAPM
       INRT=I-NSHIFT
       CALL I4GMX3(X,IRECT,I,GAPMX)
C-----------------to avoid too much print-out in 0.out file
       CALL INELTS_NP(X        ,IRECT(1,1+NSHIFT),IXS  ,NREV ,NELS         ,
     .                INRT     ,AREA             ,NOINT,0    ,IGRSURF%ELTYP,
     .                IGRSURF%ELEM)
       IF(NELS/=0)THEN
        MT=IXS(1,NELS)
        IF(MT>0)THEN
          DO JJ=1,8
            JJJ=IXS(JJ+1,NELS)
            XC(JJ)=X(1,JJJ)
            YC(JJ)=X(2,JJJ)
            ZC(JJ)=X(3,JJJ)
          END DO
          CALL VOLINT(VOL)
          STF(I)=SLSFAC*FILLSOL(NELS)*AREA*AREA*PM(32,MT)/VOL
        ELSE
          IF(NINT>=0) THEN
             CALL ANCMSG(MSGID=95,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IXS(NIXS,NELS),
     .                   C2='SOLID',
     .                   I3=I)
          ENDIF
          IF(NINT<0) THEN 
             CALL ANCMSG(MSGID=96,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IXS(NIXS,NELS),
     .                   C2='SOLID',
     .                   I3=I)
          ENDIF
        ENDIF
        GAP_N(1,I)=VOL/AREA
C -----Friction model ------
        IF(INTFRIC > 0) THEN
           IP= IPARTS(NELS)
           IPG = TAGPRT_FRIC(IP)
           IF(IPG > 0) THEN
            CALL FRICTION_PARTS_SEARCH (
     .                     IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                     INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL)  
            IPARTFRICM(I) = IPL
           ENDIF
        ENDIF
C------------------------------------
        CYCLE
       ELSE
        CALL INELTC(NELC ,NELTG ,INRT ,IGRSURF%ELTYP, IGRSURF%ELEM)
        IF(NELTG/=0) THEN
          MT=IXTG(1,NELTG)
          MG=IXTG(5,NELTG)
          IGTYP = IGEO(11,MG)
          IGMAT = IGEO(98,MG)
              IP = IPARTTG(NELTG)
              IF ( THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN
               DX=THK_PART(IP)*GAPSCALE
              ELSEIF ( THK(NUMELC+NELTG) /= ZERO .AND. IINTTHICK==0)THEN
               DX=THK(NUMELC+NELTG)*GAPSCALE
              ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP ==52) THEN
                DX=THK(NUMELC+NELTG)*GAPSCALE
              ELSE
                DX=GEO(1,MG)*GAPSCALE
              ENDIF
          GAPM=HALF*DX
          GAPS2=MAX(GAPS2,GAPM)
          GAPMN = MIN(GAPMN,DX)
          DXM=DXM+DX
          NDX=NDX+1
          IF(MT>0)THEN
            IF(IGTYP == 11 .AND. IGMAT > 0) THEN
               IF ( THK(NUMELC+NELTG) /= ZERO .AND. IINTTHICK ==0)THEN
                    STF(I)=SLSFAC*THK(NUMELC+NELTG)*GEO(IPGMAT + 2 ,MG)
               ELSE
                STF(I)=SLSFAC*GEO(1,MG)*GEO(IPGMAT + 2 ,MG)
              ENDIF
            ELSEIF(IGTYP ==52 .OR. 
     .            ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0))THEN
              ISUBSTACK = IWORKSH(3,NUMELC+NELTG)
              ST=PM_STACK(2,ISUBSTACK)
              STF(I)=SLSFAC*THK(NUMELC+NELTG)*ST
            ELSE
                 IF ( THK(NUMELC+NELTG) /= ZERO .AND. IINTTHICK ==0)THEN
                  STF(I)=SLSFAC*THK(NUMELC+NELTG)*PM(20,MT)
                 ELSEIF(IGTYP == 17 .OR. IGTYP == 51) THEN
                  STF(I)=SLSFAC*THK(NUMELC+NELTG)*PM(20,MT)
             ELSE
               STF(I)=SLSFAC*GEO(1,MG)*PM(20,MT)
                 ENDIF
            ENDIF
          ELSE
            IF(NINT>=0) THEN
               CALL ANCMSG(MSGID=95,
     .                     MSGTYPE=MSGWARNING,
     .                     ANMODE=ANINFO_BLIND_2,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=IXTG(NIXTG,NELTG),
     .                     C2='SHELL',
     .                     I3=I)
            END IF
            IF(NINT<0) THEN
               CALL ANCMSG(MSGID=96,
     .                     MSGTYPE=MSGWARNING,
     .                     ANMODE=ANINFO_BLIND_2,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=IXTG(NIXTG,NELTG),
     .                     C2='SHELL',
     .                     I3=I)
            END IF
          END IF
           GAP_M(I)=GAPM
C ----Friction model ------
          IF(INTFRIC > 0) THEN
           IP= IPARTTG(NELTG)
           IPG = TAGPRT_FRIC(IP)
           IF(IPG > 0) THEN
            CALL FRICTION_PARTS_SEARCH (
     .                     IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                     INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL) 
            IPARTFRICM(I) = IPL
           ENDIF
          ENDIF
C-------coating shell  stif=max(sol,shell)         
          IF (MSEGTYP(I)>NRTT) THEN
             PRINT_ERROR = .FALSE.
             CALL INSOL3D(X,IRECT,IXS,NINT,NELS,I   ,
     .            AREA,NOINT,KNOD2ELS ,NOD2ELS ,0,
     .            IXS10,IXS16,IXS20,TAGELEMS,INDEXE,
     .            NINV,IELEM,ELEM_LINKED_TO_SEGMENT,PRINT_ERROR)
             IF(PRINT_ERROR) THEN
                NODE_ID(1:4) = ITAB(IRECT(1:4,I))
          
                CALL ANCMSG(MSGID=3062,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                I2=NODE_ID(1),
     .                I3=NODE_ID(2),
     .                I4=NODE_ID(3),
     .                I5=NODE_ID(4),
     .                C1=TITR ,
     .                PRMOD=MSG_PRINT)
             ENDIF
           IF(NELS/=0) THEN
            MT=IXS(1,NELS)
            IF(MT>0)THEN
               DO JJ=1,8
                JJJ=IXS(JJ+1,NELS)
                XC(JJ)=X(1,JJJ)
                YC(JJ)=X(2,JJJ)
                ZC(JJ)=X(3,JJJ)
               END DO
               CALL VOLINT(VOL)
               STF(I)=MAX(STF(I),SLSFAC*AREA*AREA*PM(32,MT)/VOL)
               GAP_N(1,I)=VOL/AREA
            END IF
C ----Friction model ------
            IF(INTFRIC > 0) THEN
             IP= IPARTS(NELS)
             IPG = TAGPRT_FRIC(IP)
             IF(IPG > 0) THEN
              CALL FRICTION_PARTS_SEARCH (
     .                     IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                     INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL) 
              IPARTFRICM(I) = IPL
             ENDIF
            ENDIF
           END IF!(NELS/=0) THEN
          END IF !(MSEGTYP==8) THEN

          CYCLE
         ELSEIF(NELC/=0) THEN
          MT=IXC(1,NELC)
          MG=IXC(6,NELC)
          IGTYP=IGEO(11,MG)
          IGMAT = IGEO(98,MG)
              IP = IPARTC(NELC)
              IF ( THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN
               DX=THK_PART(IP)*GAPSCALE
              ELSEIF ( THK(NELC) /= ZERO .AND. IINTTHICK == 0) THEN
               DX=THK(NELC)*GAPSCALE
              ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP ==52)THEN
                DX=THK(NELC)*GAPSCALE
          ELSE
            DX=GEO(1,MG)*GAPSCALE
              ENDIF
          GAPM=HALF*DX
          GAPS2=MAX(GAPS2,GAPM)
          GAPMN = MIN(GAPMN,DX)
          DXM=DXM+DX
          NDX=NDX+1
          IF(MT>0)THEN
            IF(IGTYP == 11 .AND. IGMAT > 0) THEN
               IF ( THK(NELC) /= ZERO .AND. IINTTHICK == 0) THEN
                  STF(I)=SLSFAC*THK(NELC)*GEO(IPGMAT + 2 ,MG)
               ELSE
                  STF(I)=SLSFAC*GEO(1,MG)*GEO(IPGMAT + 2 ,MG)
               ENDIF
            ELSEIF(IGTYP ==52 .OR. 
     .            ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0))THEN
                ISUBSTACK = IWORKSH(3,NELC)
                ST=PM_STACK(2,ISUBSTACK)
                STF(I)=SLSFAC*THK(NELC)*ST
            ELSE 
               IF ( THK(NELC) /= ZERO .AND. IINTTHICK == 0) THEN
                  STF(I)=SLSFAC*THK(NELC)*PM(20,MT)
                   ELSEIF(IGTYP == 17 .OR. IGTYP ==51) THEN
                  STF(I)=SLSFAC*THK(NELC)*PM(20,MT)
               ELSE
                  STF(I)=SLSFAC*GEO(1,MG)*PM(20,MT)
                    ENDIF
            ENDIF 
          ELSE
            IF(NINT>=0) THEN
               CALL ANCMSG(MSGID=95,
     .                     MSGTYPE=MSGWARNING,
     .                     ANMODE=ANINFO_BLIND_2,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=IXC(NIXC,NELC),
     .                     C2='SHELL',
     .                     I3=I)
            END IF
            IF(NINT<0) THEN
               CALL ANCMSG(MSGID=96,
     .                     MSGTYPE=MSGWARNING,
     .                     ANMODE=ANINFO_BLIND_2,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=IXC(NIXC,NELC),
     .                     C2='SHELL',
     .                     I3=I)
            END IF
          END IF
          GAP_M(I)=GAPM
C -----Friction model ------
          IF(INTFRIC > 0) THEN
             IP= IPARTC(NELC)
             IPG = TAGPRT_FRIC(IP)
             IF(IPG > 0) THEN
              CALL FRICTION_PARTS_SEARCH (
     .                     IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                     INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL ) 
              IPARTFRICM(I) = IPL
             ENDIF
          ENDIF
C------------------------------------
C-------coating shell  stif=max(sol,shell)         
          IF (MSEGTYP(I)>NRTT) THEN
             PRINT_ERROR = .FALSE.
             CALL INSOL3D(X,IRECT,IXS,NINT,NELS,I   ,
     .            AREA,NOINT,KNOD2ELS ,NOD2ELS ,0,
     .            IXS10,IXS16,IXS20,TAGELEMS,INDEXE ,
     .            NINV,IELEM,ELEM_LINKED_TO_SEGMENT,PRINT_ERROR)
             IF(PRINT_ERROR) THEN
                NODE_ID(1:4) = ITAB(IRECT(1:4,I))
          
                CALL ANCMSG(MSGID=3062,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                I2=NODE_ID(1),
     .                I3=NODE_ID(2),
     .                I4=NODE_ID(3),
     .                I5=NODE_ID(4),
     .                C1=TITR ,
     .                PRMOD=MSG_PRINT)
             ENDIF
           IF(NELS/=0) THEN
            MT=IXS(1,NELS)
            IF(MT>0)THEN
               DO JJ=1,8
                JJJ=IXS(JJ+1,NELS)
                XC(JJ)=X(1,JJJ)
                YC(JJ)=X(2,JJJ)
                ZC(JJ)=X(3,JJJ)
               END DO
               CALL VOLINT(VOL)
               STF(I)=MAX(STF(I),SLSFAC*AREA*AREA*PM(32,MT)/VOL)
               GAP_N(1,I)=VOL/AREA
            END IF
C -----Friction model ------
               IF(INTFRIC > 0) THEN
                IP= IPARTS(NELS)
                IPG = TAGPRT_FRIC(IP)
                IF(IPG > 0) THEN
                  CALL FRICTION_PARTS_SEARCH (
     .                     IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                     INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL ) 
                   IPARTFRICM(I) = IPL
                ENDIF
              ENDIF
C------------------------------------
           END IF!(NELS/=0) THEN
          END IF !(MSEGTYP==8) THEN
          CYCLE
         END IF
       END IF
C----------------------
C     ELEMENTS SOLIDES
C----------------------
       PRINT_ERROR = .FALSE.
       CALL INSOL3D(X,IRECT,IXS,NINT,NELS,I   ,
     .            AREA,NOINT,KNOD2ELS ,NOD2ELS ,0,
     .            IXS10,IXS16,IXS20,TAGELEMS,INDEXE,
     .            NINV ,IELEM,ELEM_LINKED_TO_SEGMENT,PRINT_ERROR)
       IF(PRINT_ERROR) THEN
            NODE_ID(1:4) = ITAB(IRECT(1:4,I))
          
            CALL ANCMSG(MSGID=3062,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                I2=NODE_ID(1),
     .                I3=NODE_ID(2),
     .                I4=NODE_ID(3),
     .                I5=NODE_ID(4),
     .                C1=TITR ,
     .                PRMOD=MSG_PRINT)
       ENDIF
      IF(NELS/=0) THEN
       MT=IXS(1,NELS)
       IF(INTTH > 0 ) IELES(I) = NELS
       IF(MT>0)THEN
        DO JJ=1,8
          JJJ=IXS(JJ+1,NELS)
          XC(JJ)=X(1,JJJ)
          YC(JJ)=X(2,JJJ)
          ZC(JJ)=X(3,JJJ)
        ENDDO
        CALL VOLINT(VOL)
        STF(I)=SLSFAC*FILLSOL(NELS)*AREA*AREA*PM(32,MT)/VOL
       ELSE
          IF(NINT>=0) THEN
             CALL ANCMSG(MSGID=95,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IXS(NIXS,NELS),
     .                   C2='SOLID',
     .                   I3=I)
          ENDIF
          IF(NINT<0) THEN 
             CALL ANCMSG(MSGID=96,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR,
     .                   I2=IXS(NIXS,NELS),
     .                   C2='SOLID',
     .                   I3=I)
          ENDIF
       ENDIF
       GAP_N(1,I)=VOL/AREA
C -----Friction model ------
        IF(INTFRIC > 0) THEN
           IP= IPARTS(NELS)
           IPG = TAGPRT_FRIC(IP)
           IF(IPG > 0) THEN
              CALL FRICTION_PARTS_SEARCH (
     .                IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL) 
              IPARTFRICM(I) = IPL
            ENDIF
         ENDIF
C----------------------------------
C-------add correction for different element
      ENDIF
C---------------------
C     ELEMENTS COQUES
C---------------------
      CALL INCOQ3(IRECT,IXC ,IXTG ,NINT ,NELC     ,
     .            NELTG,I   ,GEO  ,PM   ,KNOD2ELC ,
     .            KNOD2ELTG ,NOD2ELC ,NOD2ELTG,THK,NTY,IGEO,
     .            PM_STACK  , IWORKSH )
       IF(NELTG/=0) THEN
C
        MT=IXTG(1,NELTG)
        MG=IXTG(5,NELTG)
        IGTYP = IGEO(11,MG)
        IP = IPARTTG(NELTG)
        IF ( THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN
         DX=THK_PART(IP)*GAPSCALE
        ELSEIF ( THK(NUMELC+NELTG) /= ZERO .AND. IINTTHICK == 0)THEN
         DX=THK(NUMELC+NELTG)*GAPSCALE
        ELSEIF(IGTYP == 17 .OR. IGTYP ==51 .OR. IGTYP ==52) THEN
         DX=THK(NUMELC+NELTG)*GAPSCALE
        ELSE
         DX=GEO(1,MG)*GAPSCALE
        ENDIF
        GAPM=HALF*DX
        GAPS2=MAX(GAPS2,GAPM)
        GAPMN = MIN(GAPMN,DX)
        DXM=DXM+DX
        NDX=NDX+1
        GAP_M(I)=MAX(GAP_M(I),GAPM)
        IF(MT>0)THEN
         IF(IGTYP ==11 .AND. IGMAT > 0) THEN
              IF ( THK(NUMELC+NELTG) /= ZERO .AND. IINTTHICK == 0) THEN
                  STF(I)=SLSFAC*THK(NUMELC+NELTG)*GEO(IPGMAT + 2 ,MG)
              ELSE
                  STF(I)=SLSFAC*GEO(1,MG)*GEO(IPGMAT + 2 ,MG)
              ENDIF 
         ELSEIF(IGTYP == 52 .OR. 
     .          ((IGTYP == 17 .OR. IGTYP == 51).AND.IGMAT >0)) THEN
           ISUBSTACK = IWORKSH(3,NUMELC+NELTG)
           STF(I)=SLSFAC*THK(NUMELC+NELTG)*PM_STACK( 2 ,ISUBSTACK)   
         ELSE
            IF ( THK(NUMELC+NELTG) /= ZERO .AND. IINTTHICK == 0) THEN
                 STF(I)=MAX(STF(I),SLSFAC*THK(NUMELC+NELTG)*PM(20,MT))
            ELSEIF(IGTYP == 17 .OR. IGTYP ==51) THEN
                STF(I)=MAX(STF(I),SLSFAC*THK(NUMELC+NELTG)*PM(20,MT))
            ELSE
                 STF(I)=MAX(STF(I),SLSFAC*GEO(1,MG)*PM(20,MT))
            ENDIF
         ENDIF   

        ELSE
           IF(NINT>=0) THEN
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXTG(NIXTG,NELTG),
     .                    C2='SHELL',
     .                    I3=I)
           ENDIF
           IF(NINT<0) THEN
              CALL ANCMSG(MSGID=96,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXTG(NIXTG,NELTG),
     .                    C2='SHELL',
     .                    I3=I)
           ENDIF
        ENDIF
C ----- Friction model ------
        IF(INTFRIC > 0) THEN
           IP= IPARTTG(NELTG)
           IPG = TAGPRT_FRIC(IP)
           IF(IPG > 0) THEN
            CALL FRICTION_PARTS_SEARCH (
     .                     IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                     INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL) 
            IPARTFRICM(I) = IPL
           ENDIF
        ENDIF
C------------------------------------
       ELSEIF(NELC/=0) THEN
        MT=IXC(1,NELC)
        MG=IXC(6,NELC)
        IP = IPARTC(NELC)
        IGTYP = IGEO(11,MG)
        IGMAT = IGEO(98,MG)
            IF ( THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN
             DX=THK_PART(IP)*GAPSCALE
            ELSEIF ( THK(NELC) /= ZERO .AND. IINTTHICK == 0) THEN
             DX=THK(NELC)*GAPSCALE
            ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP ==52) THEN
             DX=THK(NELC)*GAPSCALE
            ELSE
              DX=GEO(1,MG)*GAPSCALE
        ENDIF
        GAPM=HALF*DX
        GAPS2=MAX(GAPS2,GAPM)
        GAPMN = MIN(GAPMN,DX)
        DXM=DXM+DX
        NDX=NDX+1
        GAP_M(I)=MAX(GAP_M(I),GAPM)
        IF(MT>0)THEN
         IF(IGTYP == 11 .AND. IGMAT > 0) THEN
              IF ( THK(NELC) /= ZERO .AND. IINTTHICK == 0) THEN
               STF(I)=SLSFAC*THK(NELC)*GEO(IPGMAT + 2 ,MG)
              ELSE
                STF(I)=SLSFAC*GEO(1,MG)*GEO(IPGMAT + 2 ,MG)
              ENDIF
         ELSEIF(IGTYP ==52 .OR.
     .         ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0))THEN
           ISUBSTACK = IWORKSH(3,NELC)
           ST=PM_STACK(2,ISUBSTACK)
           STF(I)=SLSFAC*THK(NELC)*ST
         ELSE 
          IF ( THK(NELC) /= ZERO .AND. IINTTHICK == 0) THEN
             STF(I)=MAX(STF(I),SLSFAC*THK(NELC)*PM(20,MT))
          ELSEIF(IGTYP == 17 .OR. IGTYP == 51 ) THEN
              STF(I)=MAX(STF(I),SLSFAC*THK(NELC)*PM(20,MT))
          ELSE
            STF(I)=MAX(STF(I),SLSFAC*GEO(1,MG)*PM(20,MT))
          ENDIF
         ENDIF   
        ELSE
           IF(NINT>=0) THEN
              CALL ANCMSG(MSGID=95,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXC(NIXC,NELC),
     .                    C2='SHELL',
     .                    I3=I)
           ENDIF
           IF(NINT<0) THEN
              CALL ANCMSG(MSGID=96,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXC(NIXC,NELC),
     .                    C2='SHELL',
     .                    I3=I)
           ENDIF
        ENDIF
C ----- Friction model ------
        IF(INTFRIC > 0) THEN
           IP= IPARTC(NELC)
           IPG = TAGPRT_FRIC(IP)
           IF(IPG > 0) THEN
            CALL FRICTION_PARTS_SEARCH (
     .                     IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                     INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )  
            IPARTFRICM(I) = IPL
           ENDIF
        ENDIF
C------------------------------------
       ENDIF
C
       IF(NELS+NELC+NELTG==0)THEN
        IF (IMACH/=3) THEN
         IF(NINT>0) THEN
            CALL ANCMSG(MSGID=92,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
         ENDIF
         IF(NINT<0) THEN
            CALL ANCMSG(MSGID=93,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
         ENDIF
        ELSE
C      en SPMD il faut un element associe a l'arrete sinon erreur
         IF(NINT>0) THEN
            CALL ANCMSG(MSGID=481,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
         ENDIF
         IF(NINT<0) THEN
            CALL ANCMSG(MSGID=482,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
         ENDIF
        ENDIF
       ENDIF
      END DO
C
      IF(NUMELS > 0) DEALLOCATE(TAGELEMS,INDEXE)
C
      CALL ANCMSG(MSGID=3022,
     .             MSGTYPE=MSGWARNING,
     .             ANMODE=ANINFO_BLIND_1,
     .             I1=ID,
     .             C1=TITR,
     .             PRMOD=MSG_PRINT)
      CALL ANCMSG(MSGID=3024,
     .             MSGTYPE=MSGWARNING,
     .             ANMODE=ANINFO_BLIND_1,
     .             I1=ID,
     .             C1=TITR,
     .             PRMOD=MSG_PRINT)
      IF(NINV > 0 .AND.NINT>0)
     .    CALL ANCMSG(MSGID=3023,
     .             MSGTYPE=MSGWARNING,
     .             ANMODE=ANINFO_BLIND_1,
     .             I1=ID,
     .             C1=TITR,
     .             I2=NINV)
C
      IF(NINV > 0 .AND.NINT< 0)
     .    CALL ANCMSG(MSGID=3025,
     .             MSGTYPE=MSGWARNING,
     .             ANMODE=ANINFO_BLIND_1,
     .             I1=ID,
     .             C1=TITR,
     .             I2=NINV)
C         
C      IF (IPRI>=5.AND.NREV>0) WRITE (IOUT,1400) NREV,NOINT
C----due to Cycle       ------- 
      DO I=1+NSHIFT,NRT+NSHIFT
       GAP_M(I)=MIN(GAP_M(I),GAPMAX_M)
      END DO
C-----------------------------------------------
      RETURN
 1400 FORMAT(I10,' MAIN SEGMENTS',' OF INTERFACE',I10,
     +                      ' ARE REVERSED THE NORMAL DIRECTION')
      END
Chd|====================================================================
Chd|  I24BORD                       source/interfaces/inter3d1/i24sti3.F
Chd|-- called by -----------
Chd|        I24STI3                       source/interfaces/inter3d1/i24sti3.F
Chd|-- calls ---------------
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|====================================================================
      SUBROUTINE I24BORD(NSEG,SURF_NODES ,TAGB)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER TAGB(*),NSEG,SURF_NODES(NSEG,4)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,NLMAX,STAT,LL,I1,I2,I3,I4,I5,I1M,I2M,IS,BORD,BOLD
      INTEGER NEXTK(4),IWORK(70000),NL
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: 
     .   LINEIX
      INTEGER, DIMENSION(:), ALLOCATABLE :: 
     .   INDEX
      DATA NEXTK/2,3,4,1/
C=======================================================================
      NLMAX = 4*NSEG
      ALLOCATE (LINEIX(2,NLMAX)    ,STAT=stat)
      ALLOCATE (INDEX(2*NLMAX)     ,STAT=stat)
c---------------------------------------
c       recherche de toutes les lignes dans la surface
c---------------------------------------
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
            TAGB(I1M) = 1
            TAGB(I2M) = 1
          ENDIF
          I1M = I1
          I2M = I2
        ENDDO

        IF(BORD==1)THEN
c         derniere arrete est un bord
          TAGB(I1) = 1
          TAGB(I2) = 1
        ENDIF


      DEALLOCATE (INDEX)
      DEALLOCATE (LINEIX)
C-----------
      RETURN
      END
Chd|====================================================================
Chd|  I24NORMNS                     source/interfaces/inter3d1/i24sti3.F
Chd|-- called by -----------
Chd|        I24STI3                       source/interfaces/inter3d1/i24sti3.F
Chd|        I25STI3                       source/interfaces/inter3d1/i25sti3.F
Chd|-- calls ---------------
Chd|        NORMV3                        source/interfaces/inter3d1/i24tools.F
Chd|        NORMVEC                       source/interfaces/inter3d1/i24sti3.F
Chd|====================================================================
      SUBROUTINE I24NORMNS(
     1 X     ,IRECT ,NRT   ,NSN   ,NSV   ,PEN_OLD, STF)
C============================================================================
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
      INTEGER NRT,IRECT(4,*),NSN,NSV(*)
      my_real
     .   X(3,*),PEN_OLD(5,NSN)
      my_real  , INTENT(IN) :: STF(NRT)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,NN(4),NJ,NS
C     REAL
      my_real
     .   R(3),S(3),T(3),DET
      my_real, DIMENSION(:,:), ALLOCATABLE :: NORM
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAG  
C=======================================================================
       ALLOCATE(NORM(3,NUMNOD),ITAG(NUMNOD))
       DO I=1,NUMNOD
        NORM(1,I) = ZERO
        NORM(2,I) = ZERO
        NORM(3,I) = ZERO
        ITAG(I) =0
       ENDDO
       DO I=1,NRT
        IF(STF(I) > ZERO) THEN
          DO J=1,4
            NN(J)=IRECT(J,I)
            ITAG(NN(J)) =ITAG(NN(J))+1
          END DO
C------Node 1            
          DO J=1,3
            R(J) = X(J,NN(2))-X(J,NN(1))
            S(J) = X(J,NN(4))-X(J,NN(1))
          END DO
          CALL NORMVEC(R,S,T)
          DO J=1,3
            NORM(J,NN(1)) = NORM(J,NN(1))+T(J)
          END DO
C------Node 2          
          DO J=1,3
            R(J) = X(J,NN(3))-X(J,NN(2))
            S(J) = X(J,NN(1))-X(J,NN(2))
          END DO
          CALL NORMVEC(R,S,T)
          DO J=1,3
           NORM(J,NN(2)) = NORM(J,NN(2))+T(J)
          END DO
C------Node 3,4
          IF (NN(4)/=NN(3)) THEN              
            DO J=1,3
              R(J) = X(J,NN(4))-X(J,NN(3))
              S(J) = X(J,NN(2))-X(J,NN(3))
            END DO
            CALL NORMVEC(R,S,T)
            DO J=1,3
              NORM(J,NN(3)) = NORM(J,NN(3))+T(J)
            END DO
            DO J=1,3
             R(J) = X(J,NN(1))-X(J,NN(4))
             S(J) = X(J,NN(3))-X(J,NN(4))
            END DO
            CALL NORMVEC(R,S,T)
            DO J=1,3
             NORM(J,NN(4)) = NORM(J,NN(4))+T(J)
            END DO
          ELSE ! norm_n3=norm_n2
            DO J=1,3
             NORM(J,NN(3)) = NORM(J,NN(3))+T(J)
            END DO
          END IF
        ENDIF
      ENDDO
C----re-normalizing---       
      DO I=1,NUMNOD
        IF (ITAG(I) >1) THEN
         CALL NORMV3(NORM(1,I),DET)
        END IF
      ENDDO
C       
      DO I=1,NSN
        NS = NSV(I)
        PEN_OLD(1,I) = NORM(1,NS)
        PEN_OLD(2,I) = NORM(2,NS)
        PEN_OLD(3,I) = NORM(3,NS)
      ENDDO
C-----------
       DEALLOCATE(NORM,ITAG)
      RETURN
      END
Chd|====================================================================
Chd|  NORMVEC                       source/interfaces/inter3d1/i24sti3.F
Chd|-- called by -----------
Chd|        I24NORMNS                     source/interfaces/inter3d1/i24sti3.F
Chd|-- calls ---------------
Chd|        NORMV3                        source/interfaces/inter3d1/i24tools.F
Chd|====================================================================
      SUBROUTINE NORMVEC(R,S,T)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   R(3) , S(3) , T(3)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   DET
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C    T  = R x S
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       T(1) = R(2) * S(3) - R(3) * S(2) 
       T(2) = R(3) * S(1) - R(1) * S(3) 
       T(3) = R(1) * S(2) - R(2) * S(1) 
       CALL NORMV3(T,DET)
      RETURN
      END
Chd|====================================================================
Chd|  I24LL_KG                      source/interfaces/inter3d1/i24sti3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        VOLINT                        source/interfaces/inter3d1/volint.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I24LL_KG(
     1 X     ,IRECT ,IXS   ,PM    ,WA    ,
     2 GEO   ,NRT   ,IXC   ,NINT  ,NTY   ,
     3 NOINT ,NSN   ,NSV   ,IXTG  ,IXT   ,
     6 IXP   ,IPART ,IPARTC,IPARTTG,THK  ,
     D THK_PART,IXR ,ITAB  ,IXS10 ,IXS16 ,
     E IXS20 ,NMN   ,MSR   ,LL_S  ,LL_M  ,
     J IPARTT,IPARTP,IPARTR,IGEO  )
C-----------------------------------------------
      USE MESSAGE_MOD
C
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
#include      "scr17_c.inc"
#include      "scr08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN, NTY, NOINT,NSN,NRT,NINT
      INTEGER IRECT(4,*), IXS(NIXS,*), IXC(NIXC,*),MSR(*),
     .   NSV(*), IXTG(NIXTG,*), IXT(NIXT,*), IXP(NIXP,*),
     .   IXR(NIXR,*) ,IPART(LIPART1,*), IPARTC(*), IPARTTG(*),
     .   ITAB(*), IXS10(6,*), IXS16(8,*), IXS20(12,*)
      INTEGER, DIMENSION(NUMELT), INTENT(IN) :: IPARTT
      INTEGER, DIMENSION(NUMELP), INTENT(IN) :: IPARTP
      INTEGER, DIMENSION(NUMELR), INTENT(IN) :: IPARTR
      INTEGER, DIMENSION(NPROPGI,NUMGEO) ,INTENT(IN):: IGEO
C     REAL
      my_real
     .   X(3,*), PM(NPROPM,*), GEO(NPROPG,*), WA(*),
     .   THK(*),THK_PART(*),LL_S(*),LL_M(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N, MT, JJ, JJJ, NELC,
     .   MG, NUM, NPT, LL, L, NN, NELTG,N1,N2,N3,N4,IE,
     .   IP, NLEV, MYLEV, K, P, R, T,IAD,IGTYP
C     REAL
      my_real
     .   AREA, VOL, DX, GAPM, DDX
C----------------------
      DO I=1,NUMNOD
        WA(I)=EP10
      ENDDO
      DO I=1,NSN
        LL_S(I)=EP10
      ENDDO
      DO I=1,NMN
        LL_M(I)=EP10
      ENDDO
C----SHELLS ------------     
      DO I=1,NUMELC
       MG=IXC(6,I)
       IP = IPARTC(I)
       IF ( THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN
        DX=THK_PART(IP)
       ELSEIF ( THK(I) /= ZERO .AND. IINTTHICK == 0) THEN
        DX=THK(I)
       ELSE
        DX=GEO(1,MG)
       ENDIF
        WA(IXC(2,I))=MIN(WA(IXC(2,I)),DX)
        WA(IXC(3,I))=MIN(WA(IXC(3,I)),DX)
        WA(IXC(4,I))=MIN(WA(IXC(4,I)),DX)
        WA(IXC(5,I))=MIN(WA(IXC(5,I)),DX)
      ENDDO
      DO I=1,NUMELTG
        MG=IXTG(5,I)
        IP = IPARTTG(I)
        IF ( THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN
         DX=THK_PART(IP)
        ELSEIF ( THK(NUMELC+I) /= ZERO .AND. IINTTHICK == 0) THEN
         DX=THK(NUMELC+I)
        ELSE
         DX=GEO(1,MG)
        ENDIF
        WA(IXTG(2,I))=MIN(WA(IXTG(2,I)),DX)
        WA(IXTG(3,I))=MIN(WA(IXTG(3,I)),DX)
        WA(IXTG(4,I))=MIN(WA(IXTG(4,I)),DX)
      ENDDO
C----truss------------
      DO I=1,NUMELT
        MG=IXT(4,I)
        IP = IPARTT(I)
        IF ( THK_PART(IP) > ZERO ) THEN
          DX=THK_PART(IP)
        ELSE
          DX=SQRT(GEO(1,MG))
        END IF
        WA(IXT(2,I))=MIN(WA(IXT(2,I)),DX)
        WA(IXT(3,I))=MIN(WA(IXT(3,I)),DX)
      ENDDO
C----beam------------
      DO I=1,NUMELP
        MG=IXP(5,I)
        IP = IPARTP(I)
        IF ( THK_PART(IP) > ZERO ) THEN
          DX=THK_PART(IP)
        ELSE
          DX=SQRT(GEO(1,MG))
        END IF
        WA(IXP(2,I))=MIN(WA(IXP(2,I)),DX)
        WA(IXP(3,I))=MIN(WA(IXP(3,I)),DX)
      ENDDO
      DO I=1,NUMELR
        IP = IPARTR(I)
        IF ( THK_PART(IP) > ZERO ) THEN
          MG=IXR(1,I)
          IGTYP = IGEO(11,MG)
          DX=THK_PART(IP)
          WA(IXR(2,I))=MAX(WA(IXR(2,I)),DX)
          WA(IXR(3,I))=MAX(WA(IXR(3,I)),DX)
          IF (IGTYP==12) WA(IXR(4,I))=MAX(WA(IXR(4,I)),DX)
       END IF
      ENDDO
C----solides------------     
      DO I=1,NUMELS
        MG=IXS(1,I)
        IF(MG>0)THEN
          DO JJ=1,8
            JJJ=IXS(JJ+1,I)
            XC(JJ)=X(1,JJJ)
            YC(JJ)=X(2,JJJ)
            ZC(JJ)=X(3,JJJ)
          END DO
          CALL VOLINT(VOL)
          DX=VOL**THIRD
          DO K=1,8
            WA(IXS(K+1,I))=MIN(WA(IXS(K+1,I)),DX)
          ENDDO
          IF(I <= NUMELS8)THEN
          ELSEIF(I <= NUMELS8+NUMELS10)THEN
           IE = I-NUMELS8
            DO K=1,6
              N= IXS10(K,IE)
              WA(N)=MIN(WA(N),DX)
            ENDDO
          ELSEIF(I <= NUMELS8+NUMELS10+NUMELS20)THEN
           IE = I-NUMELS8-NUMELS10
            DO K=1,12
              N= IXS20(K,IE)
              WA(N)=MIN(WA(N),DX)
            ENDDO
          ELSEIF(I <= NUMELS8+NUMELS10+NUMELS20+NUMELS16)THEN
           IE = I-NUMELS8-NUMELS10-NUMELS20
            DO K=1,8
              N= IXS16(K,IE)
              WA(N)=MIN(WA(N),DX)
            ENDDO
          END IF
        END IF!(MG>0)THEN
      ENDDO
C      
      DO I=1,NSN
        LL_S(I)=MIN(LL_S(I),WA(NSV(I)))
      ENDDO
      DO I=1,NMN
        LL_M(I)=MIN(LL_M(I),WA(MSR(I)))
      ENDDO
C-----------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  INELTS_NP                     source/interfaces/inter3d1/i24sti3.F
Chd|-- called by -----------
Chd|        I24GAPM                       source/interfaces/inter3d1/i24sti3.F
Chd|-- calls ---------------
Chd|        NORMA1                        source/interfaces/inter3d1/norma1.F
Chd|====================================================================
      SUBROUTINE INELTS_NP(X        ,IRECT ,IXS   ,NREV ,NEL       ,
     .                     I        ,AREA  ,NOINT ,IR   ,SURF_ELTYP,
     .                     SURF_ELEM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NREV, NEL, I, NOINT,IR,SURF_ELTYP(*),SURF_ELEM(*)
C     REAL
      my_real
     .   AREA
      INTEGER IRECT(4,*), IXS(NIXS,*)
C     REAL
      my_real
     .   X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IY(4), N, JJ, II, K, NN, KK, IC, IAD,
     .        NUSER, NUSERM
C     REAL
      my_real
     .   N1, N2, N3, DDS
      my_real :: XX1(4), XX2(4),XX3(4),XS1,YS1,ZS1,XC,YC,ZC
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C---Remove print-out in 0.out file (could be too much)
      IC =0
      NEL=0
      IF (SURF_ELTYP(I) /=1) RETURN
C
      NEL=SURF_ELEM(I)
C-----------------------------------------------
C     VERIFICATION DE L'ORIENTATION DES SEGMENTS
C-----------------------------------------------
       XS1=0.
       YS1=0.
       ZS1=0.
       DO 100 JJ=1,4
       NN=IRECT(JJ,I)
       IY(JJ)=NN
       XX1(JJ)=X(1,NN)
       XX2(JJ)=X(2,NN)
       XX3(JJ)=X(3,NN)
       XS1=XS1+.25*X(1,NN)
       YS1=YS1+.25*X(2,NN)
  100  ZS1=ZS1+.25*X(3,NN)
C
       CALL NORMA1(N1,N2,N3,AREA,XX1,XX2,XX3)
       XC=0.
       YC=0.
       ZC=0.
       DO 110 K=1,8
         KK=IXS(K+1,NEL)
         XC=XC+X(1,KK)
         YC=YC+X(2,KK)
         ZC=ZC+X(3,KK)
 110   CONTINUE
       XC=XC*ONE_OVER_8
       YC=YC*ONE_OVER_8
       ZC=ZC*ONE_OVER_8
       IF(IR/=0) RETURN
       IF(IC>=2)RETURN
       DDS=N1*(XC-XS1)+N2*(YC-YS1)+N3*(ZC-ZS1)
       IF(DDS<0) RETURN
       IF(IY(3)==IY(4)) THEN
        IRECT(1,I)=IY(2)
        IRECT(2,I)=IY(1)
       ELSE
        DO 120 KK=1,4
  120   IRECT(KK,I)=IY(4-KK+1)
       ENDIF
       NREV = NREV +1
C          
       RETURN
      END
Chd|====================================================================
Chd|  INSOLBOX                      source/interfaces/inter3d1/i24sti3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE INSOLBOX(X    ,S_TYPE  ,S_EL   ,NOINT ,IXS   ,
     .                   IXS10 ,IXS16   ,IXS20  ,NS    ,GAP   ,
     .                   IPART_E,IPART_NS,IPEN0 ,INS     )
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
      INTEGER S_TYPE  ,S_EL,NS,INS,NOINT,IPART_E,IPART_NS,IPEN0
      INTEGER IXS(NIXS,*), IXS10(6,*), IXS16(8,*), IXS20(12,*)
      my_real
     .   X(3,*),GAP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,N,NC4(4),NC8(8)
C     REAL
      my_real
     .   XI,YI,ZI,XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C
C-----------------------------------------------
      IF (IPEN0==0.AND.IPART_E==IPART_NS) THEN
       INS = 0
       RETURN
      END IF
      INS = 1
      XMIN=EP30
      XMAX=-EP30
      YMIN=EP30
      YMAX=-EP30
      ZMIN=EP30
      ZMAX=-EP30 
      IF (S_TYPE==0.OR.S_EL==0) RETURN
      NC4(1)=IXS(2,S_EL)
      NC4(2)=IXS(4,S_EL)
      NC4(3)=IXS(7,S_EL)
      NC4(4)=IXS(6,S_EL)
      NC8(1:8)=IXS(2:9,S_EL)
       SELECT CASE (S_TYPE)
          CASE(4)
           DO J=1,4
            N= NC4(J)
            IF(N==NS) THEN
             INS = 0
             RETURN
            END IF
            XMIN=MIN(XMIN,X(1,N))
            XMAX=MAX(XMAX,X(1,N))
            YMIN=MIN(YMIN,X(2,N))
            YMAX=MAX(YMAX,X(2,N))
            ZMIN=MIN(ZMIN,X(3,N))
            ZMAX=MAX(ZMAX,X(3,N))
           END DO
          CASE(8)
           DO J=1,8
            N = NC8(J)
            IF(N==NS) THEN
             INS = 0
             RETURN
            END IF
            XMIN=MIN(XMIN,X(1,N))
            XMAX=MAX(XMAX,X(1,N))
            YMIN=MIN(YMIN,X(2,N))
            YMAX=MAX(YMAX,X(2,N))
            ZMIN=MIN(ZMIN,X(3,N))
            ZMAX=MAX(ZMAX,X(3,N))
           END DO
          CASE(10)
           DO J=1,4
            N= NC4(J)
            IF(N==NS) THEN
             INS = 0
             RETURN
            END IF
            XMIN=MIN(XMIN,X(1,N))
            XMAX=MAX(XMAX,X(1,N))
            YMIN=MIN(YMIN,X(2,N))
            YMAX=MAX(YMAX,X(2,N))
            ZMIN=MIN(ZMIN,X(3,N))
            ZMAX=MAX(ZMAX,X(3,N))
           END DO
            DO J=1,6
             N=IXS10(J,S_EL-NUMELS8)
             IF(N==NS) THEN
              INS = 0
              RETURN
             END IF
             XMIN=MIN(XMIN,X(1,N))
             XMAX=MAX(XMAX,X(1,N))
             YMIN=MIN(YMIN,X(2,N))
             YMAX=MAX(YMAX,X(2,N))
             ZMIN=MIN(ZMIN,X(3,N))
             ZMAX=MAX(ZMAX,X(3,N))
            ENDDO
          CASE(16)
           DO J=1,8
            N = NC8(J)
            IF(N==NS) THEN
             INS = 0
             RETURN
            END IF
            XMIN=MIN(XMIN,X(1,N))
            XMAX=MAX(XMAX,X(1,N))
            YMIN=MIN(YMIN,X(2,N))
            YMAX=MAX(YMAX,X(2,N))
            ZMIN=MIN(ZMIN,X(3,N))
            ZMAX=MAX(ZMAX,X(3,N))
           END DO
            DO J=1,8
             N = IXS16(J,S_EL-NUMELS8-NUMELS10-NUMELS20)
             IF(N==NS) THEN
              INS = 0
              RETURN
             END IF
             XMIN=MIN(XMIN,X(1,N))
             XMAX=MAX(XMAX,X(1,N))
             YMIN=MIN(YMIN,X(2,N))
             YMAX=MAX(YMAX,X(2,N))
             ZMIN=MIN(ZMIN,X(3,N))
             ZMAX=MAX(ZMAX,X(3,N))
            ENDDO
          CASE(20)
           DO J=1,8
            N = NC8(J)
            IF(N==NS) THEN
             INS = 0
             RETURN
            END IF
            XMIN=MIN(XMIN,X(1,N))
            XMAX=MAX(XMAX,X(1,N))
            YMIN=MIN(YMIN,X(2,N))
            YMAX=MAX(YMAX,X(2,N))
            ZMIN=MIN(ZMIN,X(3,N))
            ZMAX=MAX(ZMAX,X(3,N))
           END DO
            DO J=1,12
             N =IXS20(J,S_EL-NUMELS8-NUMELS10)
             IF(N==NS) THEN
              INS = 0
              RETURN
             END IF
             XMIN=MIN(XMIN,X(1,N))
             XMAX=MAX(XMAX,X(1,N))
             YMIN=MIN(YMIN,X(2,N))
             YMAX=MAX(YMAX,X(2,N))
             ZMIN=MIN(ZMIN,X(3,N))
             ZMAX=MAX(ZMAX,X(3,N))
            ENDDO
          CASE DEFAULT
           RETURN
        END SELECT
C        
      XI = X(1,NS)
      YI = X(2,NS)
      ZI = X(3,NS)
      IF (IPART_E /= IPART_NS) THEN
       XMIN = XMIN-GAP
       XMAX = XMAX+GAP
       YMIN = YMIN-GAP
       YMAX = YMAX+GAP
       ZMIN = ZMIN-GAP
       ZMAX = ZMAX+GAP
      END IF
        IF(XI < XMIN) THEN
         INS = 0
         RETURN
        END IF
        IF(XI > XMAX)  THEN
         INS = 0
         RETURN
        END IF
        IF(YI < YMIN)  THEN
         INS = 0
         RETURN
        END IF
        IF(YI > YMAX)  THEN
         INS = 0
         RETURN
        END IF
        IF(ZI < ZMIN)  THEN
         INS = 0
         RETURN
        END IF
        IF(ZI > ZMAX)  THEN
         INS = 0
         RETURN
        END IF
C-----------------------------------------------
      RETURN
      END

