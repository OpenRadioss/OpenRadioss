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
C
Chd|====================================================================
Chd|  I7STI3                        source/interfaces/inter3d1/i7sti3.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRICTION_PARTS_SEARCH         source/interfaces/inter3d1/i7sti3.F
Chd|        I4GMX3                        source/interfaces/inter3d1/i4gmx3.F
Chd|        INCOQ3                        source/interfaces/inter3d1/incoq3.F
Chd|        INELTC                        source/interfaces/inter3d1/inelt.F
Chd|        INELTIGEO                     source/interfaces/inter3d1/inelt.F
Chd|        INELTS                        source/interfaces/inter3d1/inelt.F
Chd|        INSOL3D                       source/interfaces/inter3d1/insol3.F
Chd|        VOLINT                        source/interfaces/inter3d1/volint.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I7STI3(
     1              X          ,IRECT   ,STF          ,IXS      ,PM       ,
     2              GEO        ,NRT     ,IXC          ,NINT     ,STFAC    ,
     3              NTY        ,GAP     ,NOINT        ,STFN     ,NSN      ,
     4              MS         ,NSV     ,IXTG         ,IGAP     ,WA       ,
     5              GAP_S      ,GAP_M   ,GAPMIN       ,IXT      ,IXP      ,
     6              GAPINF     ,GAPMAX  ,INACTI       ,KNOD2ELS ,KNOD2ELC ,
     7              KNOD2ELTG  ,NOD2ELS ,NOD2ELC      ,NOD2ELTG ,IGRSURF  ,
     8              INTTH      ,IELES   ,IELEC        ,AREAS    ,SH4TREE  ,
     9              SH3TREE    ,IPART   ,IPARTC       ,IPARTTG  ,THK      ,
     B              THK_PART   ,PERCENT_SIZE,GAP_S_L  ,GAP_M_L  ,NOD2EL1D ,
     C              KNOD2EL1D  ,IXR    ,ITAB          ,BGAPSMX  ,IXS10    ,
     D              IXS16      ,IXS20  ,ID            ,TITR     ,IDDLEVEL ,
     E              DRAD       ,IGEO   ,FILLSOL       ,PM_STACK ,IWORKSH  ,
     F              IT19       ,KXIG3D ,IXIG3D        ,INTFRIC  ,IPARTS   ,
     G              TAGPRT_FRIC,IPARTFRICS,IPARTFRICM ,INTBUF_FRIC_TAB,NRT_IGE,
     I              IREM_GAP   ,GAPM_MX,GAPS_MX       ,GAPM_L_MX,GAPS_L_MX,
     J              IPARTT     ,IPARTP ,IPARTR        )
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
#include      "remesh_c.inc"
#include      "scr05_c.inc"
#include      "scr08_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRT, NINT, NTY, NOINT,NSN,IGAP,
     .        INACTI,INTFRIC, NRT_IGE
      INTEGER IRECT(4,*), IXS(NIXS,*), IXC(NIXC,*),
     .   NSV(*), IXTG(NIXTG,*), IXT(NIXT,*), IXP(NIXP,*),
     .   KNOD2ELS(*), KNOD2ELC(*), KNOD2ELTG(*), NOD2ELS(*), NOD2ELC(*), 
     .   NOD2ELTG(*), IELES(*), INTTH, IELEC(*),
     .   SH3TREE(KSH3TREE,*), SH4TREE(KSH4TREE,*),IXR(NIXR,*) ,
     .   IPART(LIPART1,*),IPARTC(*),IPARTTG(*),NOD2EL1D(*),KNOD2EL1D(*),
     .   ITAB(*), IXS10(6,*), IXS16(*), IXS20(*),IDDLEVEL,IGEO(NPROPGI,*),
     .   IWORKSH(3,*),IT19,KXIG3D(NIXIG3D,*),IXIG3D(*),TAGPRT_FRIC(*),
     .   IPARTFRICS(*),IPARTFRICM(*),IPARTS(*),IREM_GAP
      my_real
     .   STFAC, GAP,GAPMIN,GAPINF, GAPMAX,PERCENT_SIZE, BGAPSMX,
     .   GAPINF_S, GAPINF_M, DRAD, GAPM_MX, GAPS_MX, GAPS_L_MX, GAPM_L_MX
      my_real
     .   X(3,*), STF(*), PM(NPROPM,*), GEO(NPROPG,*), STFN(*),
     .   MS(*),WA(*),GAP_S(*),GAP_M(*),
     .   AREAS(*),THK(*),THK_PART(*),
     .   GAP_S_L(*),GAP_M_L(*), FILLSOL(*),PM_STACK(20,*)
      INTEGER, DIMENSION(NUMELT), INTENT(IN) :: IPARTT
      INTEGER, DIMENSION(NUMELP), INTENT(IN) :: IPARTP
      INTEGER, DIMENSION(NUMELR), INTENT(IN) :: IPARTR
      INTEGER ID
      CHARACTER*nchartitle, TITR
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(*)
      TYPE (SURF_) :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NDX, I, J, INRT, NELS, MT, JJ, JJJ, NELC,
     .   MG, NUM, NPT, LL, L, NN, NELTG,N1,N2,N3,N4,IE,
     .   IP, NLEV, MYLEV, K, P, R, T,IGTYP,IPGMAT,IGMAT,
     .   ISUBSTACK,NELIG3D, COIN_IGE(8), IPID, PX, PY, PZ, IAD ,IPFMAX,IPL,
     .   IPFLMAX,IPG,NINV
      INTEGER JPERM(4)
      LOGICAL TYPE18
      my_real
     .   DXM, GAPMX, GAPMN, AREA, VOL, DX, GAPM, DDX, 
     .   GAPTMP, GAPSCALE,SX1,SY1,SZ1,SX2,SY2,SZ2,SX3,SY3,SZ3,
     .   SLSFAC,XL
      my_real, dimension(:), allocatable :: GAP_S_L_TMP
      INTEGER, DIMENSION(:),ALLOCATABLE  :: TAGELEMS,INDEXE
      DATA JPERM/2,3,4,1/
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C     STIFFNESS SEGMENTS
C        IF ONE SEGMENTS IS BASED ON BOTH SOLID AND SHELL
C        THEN SHELL STIFFNESS IS USED. UNLESS MATERIAL HAS
C        NO STIFFNESS
      ALLOCATE( GAP_S_L_TMP(NUMNOD) )
      TYPE18 = .FALSE.
      IF(INACTI == 7)TYPE18=.TRUE.
      IPGMAT = 700
      IF(NTY == 20)THEN
        SLSFAC = ONE
      ELSE
        SLSFAC = STFAC
      ENDIF
      IF(IGAP == 3)THEN
        DO I=1,NUMNOD
          GAP_S_L_TMP(I)=ZERO
        ENDDO
        DO I=1,NRT+NRT_IGE
          GAP_M_L(I)=EP30
        ENDDO
        DO I=1,NSN
          GAP_S_L(I)=EP30
        ENDDO
      ENDIF
      DXM=0.
      NDX=0
      GAPMX=EP30
      GAPMN=EP30
      GAPS_MX=ZERO
      GAPS_L_MX=ZERO
      GAPM_MX=ZERO
      GAPM_L_MX=ZERO
C
      IF(IGAP == 2)THEN
        IF(IDDLEVEL == 1) IGAP = 1  ! to keep it equal to 2 in case of 2 passes
        GAPSCALE = GAPMIN
        GAPMIN = ZERO
      ELSEIF(IGAP == 3)THEN
        GAPSCALE=GAPMIN
        GAPMIN=ZERO
      ELSE
        GAPSCALE = ONE
      ENDIF
C
      IF(IGAP == 3)THEN
        DO I=1,NRT+NRT_IGE
          XL = EP30
          DO J=1,4
            N1=IRECT(J,I)
            N2=IRECT(JPERM(J),I)
            IF(N1 /= N2 .AND. N1 /= 0)XL=MIN(XL,SQRT((X(1,N1)-X(1,N2))**2+(X(2,N1)-X(2,N2))**2+(X(3,N1)-X(3,N2))**2))
          ENDDO
          DO J=1,4
            IF(GAP_S_L_TMP(IRECT(J,I)) == ZERO) THEN
              GAP_S_L_TMP(IRECT(J,I))= PERCENT_SIZE*XL
            ELSE
              GAP_S_L_TMP(IRECT(J,I))= MIN(GAP_S_L_TMP(IRECT(J,I)),PERCENT_SIZE*XL) 
            ENDIF
          ENDDO

          DO J=1,4
            N1=IRECT(J,I)
            DO K=KNOD2EL1D(N1)+1,KNOD2EL1D(N1+1)
            IF (NOD2EL1D(K) <= NUMELT .AND. NOD2EL1D(K) /= ZERO) THEN
             T=NOD2EL1D(K)
             XL=MIN(XL,SQRT((X(1,IXT(2,T))-X(1,IXT(3,T)))**2 + (X(2,IXT(2,T))-X(2,IXT(3,T)))**2 + (X(3,IXT(2,T))-X(3,IXT(3,T)))**2))
            ELSEIF (NOD2EL1D(K) <= NUMELT+NUMELP .AND. NOD2EL1D(K) /= ZERO) THEN
             P=NOD2EL1D(K) - NUMELT
             XL=MIN(XL,SQRT((X(1,IXP(2,P))-X(1,IXP(3,P)))**2 + (X(2,IXP(2,P))-X(2,IXP(3,P)))**2 + (X(3,IXP(2,P))-X(3,IXP(3,P)))**2))
            ELSEIF (NOD2EL1D(K) <= NUMELT+NUMELP+NUMELR .AND. NOD2EL1D(K) /= ZERO) THEN
             R=NOD2EL1D(K) - NUMELT - NUMELP
             XL=MIN(XL,SQRT((X(1,IXR(2,R))-X(1,IXR(3,R)))**2 + (X(2,IXR(2,R))-X(2,IXR(3,R)))**2 + (X(3,IXR(2,R))-X(3,IXR(3,R)))**2))
            ENDIF
            ENDDO
          ENDDO
          GAP_M_L(I)=PERCENT_SIZE*XL
          GAPM_L_MX = MAX(GAPM_L_MX,GAP_M_L(I))
          DO J=1,4
            GAP_S_L_TMP(IRECT(J,I))=MIN(GAP_S_L_TMP(IRECT(J,I)),PERCENT_SIZE*XL)
          ENDDO
        ENDDO
      ENDIF
C------------------------------------
C     GAP OF SECONDARY NODES
C------------------------------------
      IF(IGAP >= 1)THEN
       DO I=1,NUMNOD
        WA(I)=ZERO
       ENDDO
       DO I=1,NUMELC
        MG=IXC(6,I)
        IGTYP = IGEO(11,MG)
          IP = IPARTC(I)
          IF ( THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN
            DX=HALF*THK_PART(IP)
          ELSEIF ( THK(I)  /= ZERO .AND. IINTTHICK == 0) THEN
            DX=HALF*THK(I)
          ELSEIF(IGTYP == 17 .OR. IGTYP ==51 .OR. IGTYP == 52) THEN
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
        IGTYP = IGEO(11,MG)
          IP = IPARTTG(I)
          IF ( THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN
            DX=HALF*THK_PART(IP)
          ELSEIF ( THK(NUMELC+I) /= ZERO .AND. IINTTHICK == 0) THEN
            DX=HALF*THK(NUMELC+I)
          ELSEIF(IGTYP == 17 .OR. IGTYP ==51 .OR. IGTYP == 52) THEN
            DX=HALF*THK(NUMELC+I)
        ELSE
          DX=HALF*GEO(1,MG)
          ENDIF
        WA(IXTG(2,I))=MAX(WA(IXTG(2,I)),DX)
        WA(IXTG(3,I))=MAX(WA(IXTG(3,I)),DX)
        WA(IXTG(4,I))=MAX(WA(IXTG(4,I)),DX)
       ENDDO
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
         IF(IGAP == 3 .AND. GAP_S_L_TMP(NSV(I)) /= ZERO)GAP_S_L(I)=MIN(GAP_S_L(I),GAP_S_L_TMP(NSV(I)))
         IF(IGAP /= 3) THEN
           GAPS_MX=MAX(GAPS_MX,GAP_S(I))
         ELSE
           GAPS_MX = MAX(GAPS_MX,GAP_S(I))
           GAPS_L_MX = MAX(GAPS_L_MX,GAP_S_L(I))
         END IF
       ENDDO
      ENDIF
C
C SECONDARY SIDE - SURFACE ---
      IF(INTTH > 0 ) THEN
        IF(NADMESH == 0)THEN
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
               SX3 = SY1*SZ2 - SZ1*SY2
               SY3 = SZ1*SX2 - SX1*SZ2
               SZ3 = SX1*SY2 - SY1*SX2
               AREAS(I) = AREAS(I) + ONE_OVER_8*SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
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
               SX3 = SY1*SZ2 - SZ1*SY2
               SY3 = SZ1*SX2 - SX1*SZ2
               SZ3 = SX1*SY2 - SY1*SX2
               AREAS(I) = AREAS(I) + ONE_OVER_6*SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
C overwrite
               IELEC(I) = IXTG(1,IE)
             END DO
          END DO
        ELSE
          DO I = 1,NSN    
             AREAS(I) = ZERO
             DO J=KNOD2ELC(NSV(I))+1,KNOD2ELC(NSV(I)+1)
               IE = NOD2ELC(J)

               IP = IPARTC(IE)
               NLEV =IPART(10,IP)
               MYLEV=SH4TREE(3,IE)
               IF(MYLEV < 0) MYLEV=-(MYLEV+1)

               IF(MYLEV == NLEV)THEN                 
                 SX1 = X(1,IXC(4,IE)) - X(1,IXC(2,IE))
                 SY1 = X(2,IXC(4,IE)) - X(2,IXC(2,IE))
                 SZ1 = X(3,IXC(4,IE)) - X(3,IXC(2,IE))
                 SX2 = X(1,IXC(5,IE)) - X(1,IXC(3,IE))
                 SY2 = X(2,IXC(5,IE)) - X(2,IXC(3,IE))
                 SZ2 = X(3,IXC(5,IE)) - X(3,IXC(3,IE))
                 SX3 = SY1*SZ2 - SZ1*SY2
                 SY3 = SZ1*SX2 - SX1*SZ2
                 SZ3 = SX1*SY2 - SY1*SX2
                 AREAS(I) = AREAS(I) + ONE_OVER_8*SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
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
               IF(MYLEV == NLEV)THEN                 
                 SX1 = X(1,IXTG(3,IE)) - X(1,IXTG(2,IE))
                 SY1 = X(2,IXTG(3,IE)) - X(2,IXTG(2,IE))
                 SZ1 = X(3,IXTG(3,IE)) - X(3,IXTG(2,IE))
                 SX2 = X(1,IXTG(4,IE)) - X(1,IXTG(2,IE))
                 SY2 = X(2,IXTG(4,IE)) - X(2,IXTG(2,IE))
                 SZ2 = X(3,IXTG(4,IE)) - X(3,IXTG(2,IE))
                 SX3 = SY1*SZ2 - SZ1*SY2
                 SY3 = SZ1*SX2 - SX1*SZ2
                 SZ3 = SX1*SY2 - SY1*SX2
                 AREAS(I) = AREAS(I) + ONE_OVER_6*SQRT(SX3*SX3+SY3*SY3+SZ3*SZ3)
C overwrite
                 IELEC(I) = IXTG(1,IE)
               END IF

             END DO
          END DO
        END IF
      END IF
C
C------------------------------------
C     GAP STIFF FACES MAIN
C------------------------------------
      IF(NUMELS > 0) THEN
        CALL MY_ALLOC(TAGELEMS,NUMELS)
        TAGELEMS = 0
        CALL MY_ALLOC(INDEXE,NUMELS)
        INDEXE = 0
      ENDIF
      NINV = 0
      DO I=1,NRT
        STF(I)=ZERO
        IF(INTTH > 0 ) IELES(I) = 0
        IF(SLSFAC < ZERO)STF(I)=SLSFAC
        GAPM=ZERO
        INRT=I
        CALL I4GMX3(X,IRECT,INRT,GAPMX)
C----------------------
        CALL INELTS(X           ,IRECT,IXS  ,NINT,NELS         ,
     .              INRT        ,AREA ,NOINT,0   ,IGRSURF%ELTYP,
     .              IGRSURF%ELEM)
     
        !----------------!
        !  NELS > 0      !
        !----------------!
        IF(NELS /= 0)THEN
          MT=IXS(1,NELS)
          IF(MT > 0)THEN
            DO JJ=1,8
              JJJ=IXS(JJ+1,NELS)
              XC(JJ)=X(1,JJJ)
              YC(JJ)=X(2,JJJ)
              ZC(JJ)=X(3,JJJ)
            END DO
            CALL VOLINT(VOL)
            STF(I)=SLSFAC*FILLSOL(NELS)*AREA*AREA*PM(32,MT)/VOL
          ELSE
            IF(NINT >= 0) THEN
               CALL ANCMSG(MSGID=95,
     .                     MSGTYPE=MSGWARNING,
     .                     ANMODE=ANINFO_BLIND_2,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=IXS(NIXS,NELS),
     .                     C2='SOLID',
     .                     I3=I)
            ENDIF
            IF(NINT < 0) THEN 
               CALL ANCMSG(MSGID=96,
     .                     MSGTYPE=MSGWARNING,
     .                     ANMODE=ANINFO_BLIND_2,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=IXS(NIXS,NELS),
     .                     C2='SOLID',
     .                     I3=I)
            ENDIF
          ENDIF
          IF(IGAP /= 0 .OR. (NTY /=7 .AND. NTY /= 20)) GAP_M(I)=GAPM
C -----Friction model ------
          IF(INTFRIC > 0) THEN
             IP= IPARTS(NELS)
             IPG = TAGPRT_FRIC(IP)
             IF(IPG > 0) THEN
              CALL FRICTION_PARTS_SEARCH (
     .                       IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                       INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )  
              IPARTFRICM(INRT) = IPL
             ENDIF
          ENDIF
C------------------------------------
          CYCLE! next I
        ENDIF ! => (NELS == 0)
        
        
        CALL INELTC(NELC ,NELTG ,INRT ,IGRSURF%ELTYP, IGRSURF%ELEM)
        !----------------!
        !  NELTG > 0     !
        !----------------!
        IF(NELTG /= 0) THEN                                                               
          MT=IXTG(1,NELTG)                                                                
          MG=IXTG(5,NELTG)                                                                
          IGTYP = IGEO(11,MG)                                                             
          IGMAT = IGEO(98,MG)                                                             
          IP = IPARTTG(NELTG)                                                             
          IF (THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN                             
            DX=THK_PART(IP)*GAPSCALE                                                      
          ELSEIF ( THK(NUMELC+NELTG) /= ZERO .AND. IINTTHICK == 0)THEN                    
            DX=THK(NUMELC+NELTG)*GAPSCALE                                                 
          ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN                      
             DX=THK(NUMELC+NELTG)*GAPSCALE                                                
          ELSE                                                                            
            DX=GEO(1,MG)*GAPSCALE                                                         
          ENDIF                                                                           
          GAPM=HALF*DX                                                                    
          GAPM_MX=MAX(GAPM_MX,GAPM)                                                       
          GAPMN=MIN(GAPMN,DX)                                                             
          DXM=DXM+DX                                                                      
          NDX=NDX+1                                                                       
           IF(MT > 0)THEN                                                                 
            IF(IGTYP == 11 .AND. IGMAT > 0) THEN ! igtyp == 11                            
              IF(SLSFAC < ZERO)THEN                                                       
                 STF(I)=-SLSFAC                                                           
              ELSEIF ( THK(NUMELC+NELTG) /= ZERO .AND. IINTTHICK == 0)THEN                
                 STF(I)=SLSFAC*THK(NUMELC+NELTG)*GEO(IPGMAT + 2 ,MG)                      
              ELSE                                                                        
                 STF(I)=SLSFAC*GEO(1,MG)*GEO(IPGMAT + 2 ,MG)                              
              ENDIF                                                                       
           ELSEIF(IGTYP == 52 .OR. ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0))THEN  
              ISUBSTACK = IWORKSH(3,NUMELC + NELTG)                                       
              IF(SLSFAC < ZERO)THEN                                                       
                 STF(I)=-SLSFAC                                                           
              ELSE                                                                        
                STF(I)=SLSFAC*THK(NUMELC+NELTG)*PM_STACK(2 ,ISUBSTACK)                    
              ENDIF                                                                       
           ELSE                                                                           
              IF(SLSFAC < ZERO)THEN                                                       
                STF(I)=-SLSFAC                                                            
              ELSEIF ( THK(NUMELC+NELTG) /= ZERO .AND. IINTTHICK == 0)THEN                
                STF(I)=SLSFAC*THK(NUMELC+NELTG)*PM(20,MT)                                 
              ELSE                                                                        
                STF(I)=SLSFAC*GEO(1,MG)*PM(20,MT)                                         
              ENDIF                                                                       
           ENDIF                                                                          
          ELSE                                                                            
            IF(NINT >= 0) THEN                                                            
               CALL ANCMSG(MSGID=95,                                                      
     .                     MSGTYPE=MSGWARNING,                                            
     .                     ANMODE=ANINFO_BLIND_2,                                         
     .                     I1=ID,                                                         
     .                     C1=TITR,                                                       
     .                     I2=IXTG(NIXTG,NELTG),                                          
     .                     C2='SHELL',                                                    
     .                     I3=I)                                                          
            END IF                                                                        
            IF(NINT < 0) THEN                                                             
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
          IF(IGAP /= 0 .OR. (NTY /= 7 .AND. NTY /= 20)) GAP_M(I)=GAPM                     
C -----Friction model ------                                                              ir
          IF(INTFRIC > 0) THEN                                                            
             IP= IPARTTG(NELTG)                                                           
             IPG = TAGPRT_FRIC(IP)                                                        
             IF(IPG > 0) THEN                                                             
              CALL FRICTION_PARTS_SEARCH (                                                
     .                       IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,                
     .                       INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )                 
              IPARTFRICM(INRT) = IPL                                                      
             ENDIF                                                                        
          ENDIF                                                                           
C----------------------------------                                                     --
          CYCLE!next I                                                                    
        ENDIF
           
        !----------------!
        !  NELC > 0      !
        !----------------!            
        IF(NELC /= 0) THEN                                                                
          MT=IXC(1,NELC)                                                                  
          MG=IXC(6,NELC)                                                                  
          IP = IPARTC(NELC)                                                               
          IGTYP = IGEO(11,MG)                                                             
           IGMAT = IGEO(98,MG)                                                            
          IF (THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN                             
            DX=THK_PART(IP)*GAPSCALE                                                      
          ELSEIF (THK(NELC) /= ZERO .AND. IINTTHICK == 0) THEN                            
            DX=THK(NELC)*GAPSCALE                                                         
          ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN                      
            DX=THK(NELC)*GAPSCALE                                                         
          ELSE                                                                            
            DX=GEO(1,MG)*GAPSCALE                                                         
          ENDIF                                                                           
          GAPM=HALF*DX                                                                    
          GAPM_MX=MAX(GAPM_MX,GAPM)                                                       
          GAPMN = MIN(GAPMN,DX)                                                           
          DXM=DXM+DX                                                                      
          NDX=NDX+1                                                                       
                                                                                          
          IF(MT > 0)THEN                                                                  
           IF(IGTYP == 11 .AND. IGMAT > 0) THEN                                           
            IF(SLSFAC < ZERO)THEN                                                         
             STF(I)=-SLSFAC                                                               
            ELSEIF ( THK(NELC) /= ZERO .AND. IINTTHICK == 0) THEN                         
             STF(I)=SLSFAC*THK(NELC)*GEO(IPGMAT + 2 ,MG)                                  
            ELSE                                                                          
             STF(I)=SLSFAC*GEO(1,MG)*GEO(IPGMAT + 2 ,MG)                                  
            ENDIF                                                                         
           ELSEIF(IGTYP == 52 .OR. ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0))THEN  
            ISUBSTACK = IWORKSH(3,NELC)                                                   
            IF(SLSFAC < ZERO)THEN                                                         
              STF(I)=-SLSFAC                                                              
            ELSE                                                                          
               STF(I)=SLSFAC*THK(NELC)*PM_STACK(2 ,ISUBSTACK )                            
            ENDIF                                                                         
           ELSE                                                                           
            IF(SLSFAC < ZERO)THEN                                                         
             STF(I)=-SLSFAC                                                               
            ELSEIF (THK(NELC) /= ZERO .AND. IINTTHICK == 0) THEN                          
             STF(I)=SLSFAC*THK(NELC)*PM(20,MT)                                            
            ELSE                                                                          
             STF(I)=SLSFAC*GEO(1,MG)*PM(20,MT)                                            
            ENDIF                                                                         
           ENDIF                                                                          
          ELSE                                                                            
            IF(NINT >= 0) THEN                                                            
               CALL ANCMSG(MSGID=95,                                                      
     .                     MSGTYPE=MSGWARNING,                                            
     .                     ANMODE=ANINFO_BLIND_2,                                         
     .                     I1=ID,                                                         
     .                     C1=TITR,                                                       
     .                     I2=IXC(NIXC,NELC),                                             
     .                     C2='SHELL',                                                    
     .                     I3=I)                                                          
            END IF                                                                        
            IF(NINT < 0) THEN                                                             
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
          IF(IGAP /=0 .OR. (NTY /=7 .AND. NTY /= 20)) GAP_M(I)=GAPM                       
C -----Fction model ------                                                              ir
          IF(INTFRIC > 0) THEN                                                            
             IP= IPARTC(NELC)                                                             
             IPG = TAGPRT_FRIC(IP)                                                        
             IF(IPG > 0) THEN                                                             
              CALL FRICTION_PARTS_SEARCH (                                                
     .                       IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,                
     .                       INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )                 
              IPARTFRICM(INRT) = IPL                                                      
             ENDIF                                                                        
          ENDIF                                                                           
C----------------------------------                                                     --
          CYCLE! next I                                                                   
        END IF                                                                            
        
C----------------------
C       SOLID ELEMENTS
C----------------------
        CALL INSOL3D(X     ,IRECT ,IXS      ,NINT    ,NELS,INRT,
     .               AREA  ,NOINT ,KNOD2ELS ,NOD2ELS ,0   ,
     .               IXS10 ,IXS16 ,IXS20    ,TAGELEMS,INDEXE,NINV)
        !----------------!
        !  NELS > 0      !
        !----------------!
        IF(NELS /= 0) THEN
         MT=IXS(1,NELS)
         IF(INTTH > 0 ) IELES(I) = NELS
         IF(MT > 0)THEN
          DO JJ=1,8
            JJJ=IXS(JJ+1,NELS)
            XC(JJ)=X(1,JJJ)
            YC(JJ)=X(2,JJJ)
            ZC(JJ)=X(3,JJJ)
          ENDDO
          CALL VOLINT(VOL)
          STF(I)=SLSFAC*FILLSOL(NELS)*AREA*AREA*PM(32,MT)/VOL
         ELSE
            IF(NINT >= 0) THEN
               CALL ANCMSG(MSGID=95,
     .                     MSGTYPE=MSGWARNING,
     .                     ANMODE=ANINFO_BLIND_2,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=IXS(NIXS,NELS),
     .                     C2='SOLID',
     .                     I3=I)
            ENDIF
            IF(NINT < 0) THEN 
               CALL ANCMSG(MSGID=96,
     .                     MSGTYPE=MSGWARNING,
     .                     ANMODE=ANINFO_BLIND_2,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=IXS(NIXS,NELS),
     .                     C2='SOLID',
     .                     I3=I)
            ENDIF
         ENDIF
C -----Friction model ------
         IF(INTFRIC > 0) THEN                                                                                 
            IP= IPARTS(NELS)                                                                                  
            IPG = TAGPRT_FRIC(IP)                                                                             
            IF(IPG > 0) THEN                                                                                  
             CALL FRICTION_PARTS_SEARCH (                                                                     
     .                      IPG                                   , INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC, 
     .                      INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC, IPL )                                     
             IPARTFRICM(INRT) = IPL                                                                           
            ENDIF                                                                                             
         ENDIF                                                                                                
C----------------------------------
        ENDIF
        
C---------------------
C        SHELL ELEMENTS
C---------------------
        CALL INCOQ3(IRECT     ,IXC      ,IXTG     ,NINT   ,NELC     ,
     .              NELTG     ,INRT     ,GEO      ,PM     ,KNOD2ELC ,
     .              KNOD2ELTG ,NOD2ELC  ,NOD2ELTG ,THK    ,NTY      ,
     .              IGEO      ,PM_STACK ,IWORKSH )
        !----------------!
        !  NELTG > 0     !
        !----------------!
        IF(NELTG /= 0) THEN
          MT=IXTG(1,NELTG)
          MG=IXTG(5,NELTG)
          IGTYP = IGEO(11,MG)
          IGMAT = IGEO(98,MG)
          IP = IPARTTG(NELTG)
          IF ( THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN
            DX=THK_PART(IP)*GAPSCALE
          ELSEIF (THK(NUMELC+NELTG) /= ZERO .AND. IINTTHICK == 0)THEN
            DX=THK(NUMELC+NELTG)*GAPSCALE
          ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
            DX=THK(NUMELC+NELTG)*GAPSCALE
          ELSE
            DX=GEO(1,MG)*GAPSCALE
          ENDIF
          GAPM=HALF*DX
          GAPM_MX=MAX(GAPM_MX,GAPM)
          GAPMN = MIN(GAPMN,DX)
          DXM=DXM+DX
          NDX=NDX+1
          IF(MT > 0)THEN
           IF(IGTYP == 11 .AND. IGMAT > 0) THEN
            IF(SLSFAC < ZERO)THEN
              STF(I)=-SLSFAC
            ELSEIF ( THK(NUMELC+NELTG)  /= ZERO .AND. IINTTHICK == 0) THEN 
              STF(I)=SLSFAC*THK(NUMELC+NELTG)*GEO(IPGMAT + 2 ,MG)
            ELSE
              STF(I)=SLSFAC*GEO(1,MG)*GEO(IPGMAT + 2 ,MG)
            ENDIF
           ELSEIF(IGTYP == 52 .OR. ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0))THEN
             ISUBSTACK = IWORKSH(3,NUMELC+NELTG)
             IF(SLSFAC < ZERO)THEN
              STF(I)=-SLSFAC
            ELSE
              STF(I)=SLSFAC*THK(NUMELC+NELTG)*PM_STACK(2 ,ISUBSTACK)
            ENDIF  
           ELSE
            IF(SLSFAC < ZERO)THEN
              STF(I)=-SLSFAC
            ELSEIF ( THK(NUMELC+NELTG) /= ZERO .AND. IINTTHICK == 0) THEN 
              STF(I)=SLSFAC*THK(NUMELC+NELTG)*PM(20,MT)
            ELSE
              STF(I)=SLSFAC*GEO(1,MG)*PM(20,MT)
            ENDIF
           ENDIF  
          ELSE
             IF(NINT >= 0) THEN
                CALL ANCMSG(MSGID=95,
     .                      MSGTYPE=MSGWARNING,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=IXTG(NIXTG,NELTG),
     .                      C2='SHELL',
     .                      I3=I)
             ENDIF
             IF(NINT < 0) THEN
                CALL ANCMSG(MSGID=96,
     .                      MSGTYPE=MSGWARNING,
     .                      ANMODE=ANINFO_BLIND_2,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=IXTG(NIXTG,NELTG),
     .                      C2='SHELL',
     .                      I3=I)
             ENDIF
          ENDIF
C ----- Friction model ------
          IF(INTFRIC > 0) THEN
             IP= IPARTTG(NELTG)
             IPG = TAGPRT_FRIC(IP)
             IF(IP > 0) THEN
              CALL FRICTION_PARTS_SEARCH (
     .                       IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                       INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )  
              IPARTFRICM(INRT) = IPL
             ENDIF
          ENDIF
C----------------------------------

        !----------------!
        !  NELC > 0      !
        !----------------!
        ELSEIF(NELC /= 0) THEN
          MT=IXC(1,NELC)
          MG=IXC(6,NELC)
          IGTYP = IGEO(11,MG)
          IGMAT = IGEO(98,MG)
          IP = IPARTC(NELC)
          IF ( THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN
            DX=THK_PART(IP)*GAPSCALE
          ELSEIF ( THK(NELC) /= ZERO .AND. IINTTHICK == 0) THEN
            DX=THK(NELC)*GAPSCALE
          ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
            DX=THK(NELC)*GAPSCALE
          ELSE
            DX=GEO(1,MG)*GAPSCALE
          ENDIF
          GAPM=HALF*DX
          GAPM_MX=MAX(GAPM_MX,GAPM)
          GAPMN = MIN(GAPMN,DX)
          DXM=DXM+DX
          NDX=NDX+1
          IF(MT > 0)THEN
            IF(IGTYP == 11 .AND. IGMAT > 0) THEN
             IF(SLSFAC < ZERO)THEN
               STF(I)=-SLSFAC
             ELSEIF (THK(NELC) /= ZERO .AND. IINTTHICK == 0) THEN 
               STF(I)=SLSFAC*THK(NELC)*GEO(IPGMAT + 2 ,MG)
             ELSE
               STF(I)=SLSFAC*GEO(1,MG)*GEO(IPGMAT + 2 ,MG) 
             ENDIF
            ELSEIF(IGTYP == 52 .OR. ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0))THEN
              ISUBSTACK = IWORKSH(3,NELC)
             IF(SLSFAC < ZERO)THEN
               STF(I)=-SLSFAC
             ELSE
               STF(I)=SLSFAC*THK(NELC)*PM_STACK(2 ,ISUBSTACK)
             ENDIF          
            ELSE
             IF(SLSFAC < ZERO)THEN
               STF(I)=-SLSFAC
             ELSEIF ( THK(NELC) /= ZERO .AND. IINTTHICK == 0) THEN 
               STF(I)=SLSFAC*THK(NELC)*PM(20,MT)
             ELSE
               STF(I)=SLSFAC*GEO(1,MG)*PM(20,MT)
             ENDIF
            ENDIF 
          ELSE
            IF(NINT >= 0) THEN                     
               CALL ANCMSG(MSGID=95,               
     .                     MSGTYPE=MSGWARNING,     
     .                     ANMODE=ANINFO_BLIND_2,  
     .                     I1=ID,                  
     .                     C1=TITR,                
     .                     I2=IXC(NIXC,NELC),      
     .                     C2='SHELL',             
     .                     I3=I)                   
            ENDIF                                  
            IF(NINT < 0) THEN                      
               CALL ANCMSG(MSGID=96,               
     .                     MSGTYPE=MSGWARNING,     
     .                     ANMODE=ANINFO_BLIND_2,  
     .                     I1=ID,                  
     .                     C1=TITR,                
     .                     I2=IXC(NIXC,NELC),      
     .                     C2='SHELL',             
     .                     I3=I)                   
            ENDIF                                  
          ENDIF
C -----Friction model ------
          IF(INTFRIC > 0) THEN
            IP= IPARTC(NELC)                                              
            IPG = TAGPRT_FRIC(IP)                                         
            IF(IPG > 0) THEN                                              
              CALL FRICTION_PARTS_SEARCH (                                 
     .                       IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC, 
     .                       INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )  
              IPARTFRICM(INRT) = IPL                                       
            ENDIF                                                         
          ENDIF
C----------------------------------
        ENDIF
        
        IF(IGAP /= 0 .OR. (NTY /= 7 .AND. NTY /= 20)) GAP_M(I)=GAPM
C----------------------------------

        !---------------------------!
        !  NELS+NELC+NELTG = 0      !
        !---------------------------!
        IF(NELS+NELC+NELTG == 0)THEN
         IF (IMACH /= 3) THEN
           IF(NINT > 0) THEN
              CALL ANCMSG(MSGID=92,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=I)
           ENDIF
           IF(NINT < 0) THEN
              CALL ANCMSG(MSGID=93,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=I)
           ENDIF
         ELSE
C        SPMD EXCHANGE : if no element associated to the edge => error
           IF(NINT > 0) THEN
             CALL ANCMSG(MSGID=481,             
     .                   MSGTYPE=MSGERROR,      
     .                   ANMODE=ANINFO_BLIND_2, 
     .                   I1=ID,                 
     .                   C1=TITR,               
     .                   I2=I)                  
           ENDIF
           IF(NINT < 0) THEN
             CALL ANCMSG(MSGID=482,             
     .                   MSGTYPE=MSGERROR,      
     .                   ANMODE=ANINFO_BLIND_2, 
     .                   I1=ID,                 
     .                   C1=TITR,               
     .                   I2=I)                  
           ENDIF
         ENDIF
        ENDIF
C----------------------------------        
      ENDDO!next I=1,NRT
  
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
C------------------------------------
C     GAP STIF FACES MAIN IGE
C------------------------------------
      DO I=NRT+1,NRT+NRT_IGE
        STF(I)=ZERO
        IF(INTTH > 0) IELES(I) = 0
        IF(SLSFAC < ZERO)STF(I)=SLSFAC
        GAPM =ZERO
        INRT=I
        CALL I4GMX3(X,IRECT,INRT,GAPMX)
        !------------------------------------
        !       ISOGEOMETRIC ELEMENTS
        !------------------------------------
        CALL INELTIGEO(X      ,IRECT  ,IXS   ,NINT        ,NELIG3D          ,
     .                 INRT   ,AREA   ,NOINT ,0           ,IGRSURF%ELTYP_IGE,
     .                 IXIG3D ,KXIG3D ,IGEO  ,IGRSURF%ELEM_IGE)
        IF(NELIG3D /= 0)THEN
          MT=KXIG3D(1,NELIG3D)
          IF(MT > 0)THEN
           IPID = KXIG3D(2,NELIG3D)
           PX = IGEO(41,IPID)-1
           PY = IGEO(42,IPID)-1
           PZ = IGEO(43,IPID)-1
           COIN_IGE(1) = (PX+1)*PY+1
           COIN_IGE(2) = (PX+1)*(PY+1)
           COIN_IGE(3) = PX+1
           COIN_IGE(4) = 1
           COIN_IGE(5) = (PX+1)*(PY+1)*PZ+(PX+1)*PY+1
           COIN_IGE(6) = (PX+1)*(PY+1)*(PZ+1)
           COIN_IGE(7) = (PX+1)*(PY+1)*PZ+PX+1
           COIN_IGE(8) = (PX+1)*(PY+1)*PZ+1
           DO JJ=1,8
            XC(JJ)=X(1,IXIG3D(KXIG3D(4,NELIG3D)+COIN_IGE(JJ)-1))
            YC(JJ)=X(2,IXIG3D(KXIG3D(4,NELIG3D)+COIN_IGE(JJ)-1))
            ZC(JJ)=X(3,IXIG3D(KXIG3D(4,NELIG3D)+COIN_IGE(JJ)-1))
           END DO
           CALL VOLINT(VOL)
           STF(I)=SLSFAC*AREA*AREA*PM(32,MT)/VOL
           STF(I)=STF(I)*((PX+1)*(PY+1)+(PY+1)*(PZ+1)+(PZ+1)*(PX+1))/3
          ELSE
           IF(NINT >= 0) THEN
              CALL ANCMSG(MSGID=95,
     .                     MSGTYPE=MSGWARNING,
     .                     ANMODE=ANINFO_BLIND_2,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=KXIG3D(5,NELIG3D),
     .                     C2='ISOGEOMETRIC SOLID',
     .                     I3=I)
           ENDIF
           IF(NINT < 0) THEN 
              CALL ANCMSG(MSGID=96,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=KXIG3D(5,NELIG3D),
     .                    C2='ISOGEOMETRIC SOLID',
     .                    I3=I)
           ENDIF
          ENDIF  
        ELSEIF(NELIG3D == 0)THEN
         IF (IMACH /= 3) THEN
           IF(NINT > 0) THEN
              CALL ANCMSG(MSGID=92,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=I)
           ENDIF
           IF(NINT < 0) THEN
              CALL ANCMSG(MSGID=93,
     .                    MSGTYPE=MSGWARNING,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=I)
           ENDIF
         ELSE
C        SPMD : one element must be associated to the edge
           IF(NINT > 0) THEN
              CALL ANCMSG(MSGID=481,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=I)
           ENDIF
           IF(NINT < 0) THEN
              CALL ANCMSG(MSGID=482,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=I)
           ENDIF
         ENDIF
        ENDIF
      ENDDO!next I
      
C---------------------------
C     GAP 
C---------------------------
        GAPMX=SQRT(GAPMX)
       IF(IGAP == 0)THEN
C CONSTANT GAP
         IF(GAP <= ZERO)THEN
           IF(NDX  /= 0)THEN
             GAP = DXM/NDX
             GAP = MIN(HALF*GAPMX,GAP)
           ELSE
             GAP = EM01 * GAPMX
           ENDIF
           IF (IT19 <= 0 .AND. .NOT.TYPE18) WRITE(IOUT,1300)GAP
         ENDIF
         GAPMIN = GAP
        
         IF (GAPMIN <= 0) THEN
           CALL ANCMSG(MSGID=785,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 I1=ID,
     .                 C1=TITR)
         ENDIF
         IF ((INACTI /= 7).AND.(GAP > 0.5*GAPMX) .AND. (IREM_GAP /= 2)) THEN
          GAPTMP = HALF*GAPMX
          CALL ANCMSG(MSGID=94,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=ID,
     .                C1=TITR,
     .                R1=GAP,
     .                R2=GAPTMP)
       ENDIF
       ELSE
C VARIABLE GAP:
C    - GAPMIN IS ONE MINIMUM GAP USED IF GAP_S(I)+GAP_M(J) < GAPMIN
C    - GAP IS MAX OF (GAP_S(I)+GAP_M(J),GAPMIN) 
         IF(GAP <= ZERO)THEN
           IF(NDX  /= 0)THEN
             GAPMIN = GAPMN
             GAPMIN = MIN(HALF*GAPMX,GAPMIN)
           ELSE
             GAPMIN = EM01 * GAPMX
           ENDIF
           IF (GAPMIN <= 0) THEN
             CALL ANCMSG(MSGID=786,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   I1=ID,
     .                   C1=TITR)
           ENDIF
           IF (IT19 <= 0 .AND. .NOT.TYPE18) WRITE(IOUT,1300)GAPMIN
         ELSE
           GAPMIN = GAP
         ENDIF
C MAX OF VARIABLE GAPS
         IF(IGAP == 3) THEN
           GAP = MAX( MIN(GAPS_MX+GAPM_MX,GAPS_L_MX+GAPM_L_MX) ,GAPMIN)
         ELSE
           GAP = MAX(GAPS_MX+GAPM_MX,GAPMIN)
         ENDIF
         GAP=MIN(GAP,GAPMAX)
         IF ((IGAP /= 3).AND.(IREM_GAP /= 2)) THEN
           IF(INACTI /= 7.AND.GAP > HALF*GAPMX .AND. IDDLEVEL == 1)THEN
            GAPTMP = 0.5*GAPMX
            CALL ANCMSG(MSGID=477,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  R1=GAP)
           ENDIF
         ENDIF
       ENDIF
C
      IF(INTTH /= 0)THEN
        IF(DRAD == ZERO)THEN
C Default value : Drad = max( max of gaps , elem wide )
          DRAD=MAX(GAP,GAPMX)
        ELSEIF(DRAD < GAP)THEN
C Drad is always greater than gap (max of gaps if gap is variable)
          DRAD=GAP
        END IF
        WRITE(IOUT,2001)DRAD

Ce warning for sorting algorithm (performance).
        IF(DRAD > GAPMX)THEN
         CALL ANCMSG(MSGID=918,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=ID,
     .               C1=TITR,
     .               R1=DRAD ,
     .               R2=GAPMX,
     .               I2=ID)
        END IF
      END IF

C -----Friction model  secnd nodes parts------
      IF(INTFRIC > 0) THEN
        IF(NUMELS  /= 0)THEN
          DO I = 1,NSN   
             IPFMAX = 0
             IPFLMAX = 0
             DO J= KNOD2ELS(NSV(I))+1,KNOD2ELS(NSV(I)+1)
               IE = NOD2ELS(J)
               IP = IPARTS(IE)
               IPG = TAGPRT_FRIC(IP)
               IF(IPG > 0 .AND. IP > IPFMAX) THEN
                  CALL FRICTION_PARTS_SEARCH (
     .                           IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                           INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL ) 
                  IF(IPL /= 0) THEN
                     IPFMAX = IP
                     IPFLMAX = IPL
                  ENDIF
               ENDIF
            ENDDO
            IF(IPFMAX /= 0) THEN
              IPARTFRICS(I) = IPFLMAX
            ENDIF

          ENDDO
        ENDIF  

       IF(NUMELC /= 0 .OR. NUMELTG  /= 0) THEN
          DO I = 1,NSN  
             IPFMAX = 0
             IPFLMAX = 0
             DO J= KNOD2ELC(NSV(I))+1,KNOD2ELC(NSV(I)+1)
               IE = NOD2ELC(J)
               IP = IPARTC(IE)
               IPG = TAGPRT_FRIC(IP)
               IF(IPG > 0 .AND. IP > IPFMAX) THEN
                  CALL FRICTION_PARTS_SEARCH (
     .                           IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                             INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )  
                  IF(IPL /= 0) THEN
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
               IF(IPG > 0.AND.IP > IPFMAX) THEN
                  CALL FRICTION_PARTS_SEARCH (
     .                           IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                           INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL ) 

                  IF(IPL /= 0) THEN
                     IPFMAX = IP
                     IPFLMAX = IPL
                  ENDIF
               ENDIF
            ENDDO
            IF(IPFMAX /= 0) THEN
              IPARTFRICS(I) = IPFLMAX
            ENDIF

          ENDDO
        ENDIF  
      ENDIF
C----------------------------------
C
C---------------------------------------------
C     NODAL MULTIPLICATOR OF STIFFNESS : SET TO ONE
C---------------------------------------------
      DO L=1,NSN
         STFN(L) = 1.
      ENDDO
C
C Real gap to use for resorting criterion
C
      BGAPSMX = ZERO
      IF (IGAP == 0) THEN
        GAPINF=GAP
      ELSE
        GAPINF = EP30
        GAPINF_S = EP30
        GAPINF_M = EP30
        DO I = 1, NSN
          GAPINF_S = MIN(GAPINF_S,GAP_S(I))
          BGAPSMX = MAX(BGAPSMX,GAP_S(I))
        ENDDO
        DO I = 1, NRT+NRT_IGE
          GAPINF_M = MIN(GAPINF_M,GAP_M(I))
        ENDDO
        GAPINF=GAPINF_S+GAPINF_M
        GAPINF=MAX(GAPINF,GAPMIN)
      ENDIF 
      DEALLOCATE( GAP_S_L_TMP )
      RETURN
 1300 FORMAT(2X,'GAP MIN = ',1PG20.13)
 2001 FORMAT(2X,'Maximum distance for radiation computation = ',
     .                                                    1PG20.13)
      END

Chd|====================================================================
Chd|  FRICTION_PARTS_SEARCH         source/interfaces/inter3d1/i7sti3.F
Chd|-- called by -----------
Chd|        HM_READ_FRICTION_ORIENTATIONS source/interfaces/friction/reader/hm_read_friction_orientations.F
Chd|        I11STI3                       source/interfaces/inter3d1/i11sti3.F
Chd|        I24GAPM                       source/interfaces/inter3d1/i24sti3.F
Chd|        I24STI3                       source/interfaces/inter3d1/i24sti3.F
Chd|        I25GAPM                       source/interfaces/inter3d1/i25sti3.F
Chd|        I25STI3                       source/interfaces/inter3d1/i25sti3.F
Chd|        I7STI3                        source/interfaces/inter3d1/i7sti3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FRICTION_PARTS_SEARCH(
     .            IP     ,NPARTSFRIC  ,PARTSFRIC  ,IPL )
                          
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IP ,IPL ,NPARTSFRIC
      INTEGER PARTSFRIC(NPARTSFRIC)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IPMID ,IPI ,IPF ,IPLMID
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IPL = 0
      IF (IP == PARTSFRIC(1)) THEN
          IPL = 1
      ELSEIF (IP == PARTSFRIC(NPARTSFRIC)) THEN
          IPL = NPARTSFRIC
      ELSEIF(IP > PARTSFRIC(1).AND.IP < PARTSFRIC(NPARTSFRIC)) THEN
         IPI = 1
         IPF = NPARTSFRIC 
          DO WHILE ((IPF-IPI) >= 1)
             IPLMID = IPI + NINT((IPF-IPI)*HALF)
             IPMID = PARTSFRIC(IPLMID)
             IF(IPMID == IP) THEN
                IPL = IPLMID
                EXIT
             ELSEIF (IP < IPMID) THEN
                IPF = IPLMID
             ELSEIF (IP > IPMID ) THEN
                IPI = IPLMID
             ENDIF  
          ENDDO
      ELSE
        IPL = 0
      ENDIF
C       
      RETURN
      END
