Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    i25sti_edg        ../starter/source/interfaces/inter3d1/i25sti_edg.F
!||--- called by ------------------------------------------------------
!||    inint3            ../starter/source/interfaces/inter3d1/inint3.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE I25STI_EDG(
     1 NEDGE    ,LEDGE     ,STFE       ,STFM      ,IGAP      ,
     2 GAPE     ,GAP_E_L   ,GAP_M      ,GAP_M_L   ,GAP_S_L   ,
     3 BGAPEMX  ,INTFRIC   ,IPARTFRIC_E,IPARTFRICM,IPARTSM   ,
     4 BGAPEMX_L,NSN       ,NSV        ,NEDGE_1D   ,NINTR     ,
     5 NOD2EL1D ,KNOD2EL1D ,IXT        ,IXP       ,IXR       ,
     6 KXX       ,NIXX     ,S_NOD2EL1D ,
     7 IXX      ,IPARTT    ,IPARTP     ,IPARTX    ,IPARTR    ,
     8 GEO      ,SLSFAC    ,GAPSCALE   ,GAPMAX_M  ,PERCENT_SIZE,
     9 INTBUF_FRIC_TAB,ID  ,THK_M      ,THK_M_SCALE,TITR     ,
     A THK_PART ,PM        ,TAGPRT_FRIC,LELX       , X       ,
     B THK      , IXTG     ,IXC        ,IGEO       ,IPARTTG  ,
     C IPARTC   ,IINTTHICK ,KNOD2ELC   ,KNOD2ELTG  ,NOD2ELC  ,
     D NOD2ELTG ,PM_STACK  ,IWORKSH    )
C-----------------------------------------------
      USE INTBUF_FRIC_MOD  
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
      use element_mod , only :nixt,nixp,nixr,nixc,nixtg
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEDGE,IGAP,INTFRIC,
     .   LEDGE(NLEDGE,*),IPARTFRIC_E(*) ,IPARTFRICM(*), IPARTSM(*)
      INTEGER , INTENT(IN) ::  NSN ,NEDGE_1D, NINTR, ID, IINTTHICK,S_NOD2EL1D
      INTEGER , INTENT(IN) ::  IXT(NIXT,NUMELT),IXP(NIXP,NUMELP),
     .   IXR(NIXR,NUMELR) ,KXX(NIXX,NUMELX) ,IXX(NUMELX) ,
     .   NOD2EL1D(S_NOD2EL1D),KNOD2EL1D(NUMNOD+1),
     .   IPARTT(NUMELT) ,IPARTP(NUMELP) ,IPARTX(NUMELX) ,IPARTR(NUMELR),
     .   IXTG(NIXTG,NUMELTG),IXC(NIXC,NUMELC),IPARTTG(NUMELTG),IPARTC(NUMELC),
     .   IGEO(NPROPGI,NUMGEO),KNOD2ELC(NUMNOD+1)   ,KNOD2ELTG(NUMNOD+1)  ,NOD2ELC(4*NUMELC)  ,
     .   NOD2ELTG(4*NUMELTG) ,IWORKSH(3,*)
      INTEGER , INTENT(IN) ::  NSV(NSN),nixx
      INTEGER , INTENT(IN) ::  TAGPRT_FRIC(NPART)
      my_real,INTENT(IN) :: GEO(NPROPG,NUMGEO),THK_PART(NPART),PM(NPROPM,NUMMAT),
     .    LELX(NUMELX), X(3,NUMNOD),THK(NUMELC+NUMELTG),PM_STACK(20,*)
      my_real,INTENT(IN) :: SLSFAC ,GAPSCALE, GAPMAX_M, PERCENT_SIZE
      my_real
     .   STFE(*), GAPE(*), GAP_E_L(*), STFM(*), GAP_M(*), GAP_M_L(*), 
     .   GAP_S_L(*), BGAPEMX,BGAPEMX_L
      TYPE(INTBUF_FRIC_STRUCT_) INTBUF_FRIC_TAB(NINTERFRIC)
      my_real, INTENT(IN) :: THK_M   ,THK_M_SCALE
      CHARACTER(LEN=NCHARTITLE) :: TITR
      my_real,EXTERNAL :: GET_U_GEO
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, A, B, N1, N2, IPRTA, IPRTB, IPRTGA, IPRTGB,
     .        NELP,NELR,NELX,NELT, IRECT(2,NEDGE),MT,MG,IP,IPG,IPL,IGTYP,
     .        OFFT, OFFP,OFFR, NELC,NELTG
      my_real
     .   STFA,STFB,GAPA,GAPB,DX1, DX , XL
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGSLAV
C-----------------------------------------------
      OFFT = NUMELS+NUMELQ+ NUMELC ! offset for truss
      OFFP = NUMELS+NUMELQ+ NUMELC+NUMELT! offset for beam
      OFFR = NUMELS+NUMELQ+ NUMELC+NUMELT+NUMELP ! offset for spring
      DO I=1,NEDGE
         IF(LEDGE(7,I) /= 3) THEN

           STFA=ZERO
           STFB=ZERO

           A=LEDGE(1,I)
           IF(A/=0) STFA=STFM(A)
           B=LEDGE(3,I)
           IF(B/=0) STFB=STFM(B)
           IF(STFA/=ZERO.AND.STFB/=ZERO)THEN
             IF(STFA + STFB < ZERO) THEN
               STFE(I)= ZERO                                 
             ELSE
               STFE(I)=TWO*STFA*STFB/MAX(ZERO,STFA+STFB)
             END IF
           ELSE
             STFE(I)=MAX(STFA,STFB)
           END IF
        ENDIF
      END DO
C
      BGAPEMX=ZERO        
      DO I=1,NEDGE
         IF(LEDGE(7,I) /= 3) THEN

            GAPA=ZERO
            GAPB=ZERO

            A=LEDGE(1,I)
            IF(A/=0) GAPA=GAP_M(A)
            B=LEDGE(3,I)
            IF(B/=0) GAPB=GAP_M(B)
            GAPE(I)=MAX(GAPA,GAPB)

            BGAPEMX   = MAX(BGAPEMX,GAPE(I)) ! BGAPEMX overall the model 
        ENDIF

      END DO

      BGAPEMX_L=ZERO        
      IF(IGAP==3)THEN
        ALLOCATE(TAGSLAV(NUMNOD))
        TAGSLAV (1:NUMNOD)   = 0
        DO I=1,NSN
           TAGSLAV(NSV(I)) = I
        ENDDO
        DO I=1,NEDGE
          IF(LEDGE(7,I) /= 3) THEN
            N1=LEDGE(5,I)
            N2=LEDGE(6,I)                             !      4-node segment
            GAP_E_L(I)=MIN(GAP_S_L(TAGSLAV(N1)),GAP_S_L(TAGSLAV(N2)))   !    O-----------------O
                                                    !   Edge1             Edge2

            BGAPEMX_L   = MAX(BGAPEMX_L,GAP_E_L(I)) ! BGAPEMX_L overall the model 
          ENDIF
        END DO
        DEALLOCATE(TAGSLAV)
      END IF

      IF(INTFRIC > 0) THEN
        DO I=1,NEDGE
          IF(LEDGE(7,I) /= 3) THEN
           IPRTA=0 
           IPRTB=0

           IPRTGA=0
           IPRTGB=0

           A=LEDGE(1,I)
           IF(A/=0) IPRTGA=IPARTSM(A)
           IF(A/=0) IPRTA=IPARTFRICM(A)

           B=LEDGE(3,I)
           IF(B/=0) IPRTGB=IPARTSM(B)
           IF(B/=0) IPRTB=IPARTFRICM(B)

           IF(IPRTA == IPRTB) THEN
              IPARTFRIC_E(I) = IPRTA
           ELSE
              IF(IPRTGA > IPRTGB ) THEN ! bigger part id is taken
                 IPARTFRIC_E(I) = IPRTA
              ELSE
                 IPARTFRIC_E(I) = IPRTB
              ENDIF
           ENDIF
          ENDIF
        END DO
      ENDIF
C
C-----1D edges -----------------
      IF(NEDGE_1D > 0) THEN
        DO I=1,NEDGE

         IF(LEDGE(7,I) == 3) THEN
          IRECT(1,I) = LEDGE(5,I)
          IRECT(2,I) = LEDGE(6,I)

          CALL I11COQ(IRECT   ,IXC      ,IXTG   ,NINTR   ,NELC    ,
     .                NELTG   ,I        ,GEO    ,PM      ,THK     ,
     .                IGEO    ,KNOD2ELC,KNOD2ELTG,NOD2ELC,NOD2ELTG,
     .                PM_STACK,IWORKSH)
          IF(NELTG /= 0) THEN
            MG=IXTG(5,NELTG)  
            IGTYP = IGEO(11,MG)
            IP = IPARTTG(NELTG)
            IF ( IGAP == 5.AND.THK_M /= ZERO) THEN
               DX=THK_M_SCALE*THK_M
            ELSEIF ( THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN
               DX=THK_PART(IP)*GAPSCALE
            ELSEIF ( THK(NUMELC+NELTG) /= ZERO .AND. IINTTHICK == 0)THEN
               DX=THK(NUMELC+NELTG)*GAPSCALE
            ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP ==52) THEN
               DX=THK(NUMELC+NELTG)*GAPSCALE
            ELSE
               DX=GEO(1,MG)*GAPSCALE
            ENDIF
             GAPE(I)=MAX(GAPE(I),HALF*DX)
             BGAPEMX   = MAX(BGAPEMX,GAPE(I)) ! BGAPEMX overall the model 
    
C------------------------------------

          ELSEIF(NELC /= 0) THEN
            MG=IXC(6,NELC)
            IGTYP = IGEO(11,MG)
            IP = IPARTC(NELC)
            IF ( IGAP == 5.AND.THK_M /= ZERO) THEN
              DX=THK_M_SCALE*THK_M
            ELSEIF (THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN
              DX=THK_PART(IP)*GAPSCALE
            ELSEIF ( THK(NELC) /= ZERO .AND. IINTTHICK == 0) THEN
              DX=THK(NELC)*GAPSCALE
            ELSEIF(IGTYP == 17 .OR. IGTYP == 51 .OR. IGTYP ==52) THEN
              DX=THK(NELC)*GAPSCALE
            ELSE
              DX=GEO(1,MG)*GAPSCALE
            ENDIF
            GAPE(I)=MAX(GAPE(I),HALF*DX)
            BGAPEMX   = MAX(BGAPEMX,GAPE(I)) ! BGAPEMX overall the model 
C------------------------------------
          ENDIF


          CALL I11FIL(IRECT,IXT ,IXP,IXR,NINTR,NELT ,
     .                NELP,NELR,NELX,I,NOD2EL1D,
     .                KNOD2EL1D,KXX,IXX)   

          IF(NELT /= 0) THEN
             MT=IXT(1,NELT)
             MG=IXT(4,NELT)
             IP = IPARTT(NELT)
             IF ( IGAP == 5.AND.THK_M /= ZERO) THEN
                DX1=THK_M_SCALE*HALF*THK_M
             ELSEIF (THK_PART(IP) > ZERO ) THEN
                DX1=GAPSCALE*THK_PART(IP)
             ELSE
                DX1=GAPSCALE*SQRT(GEO(1,MG))
             END IF
             DX=SQRT(GEO(1,MG))
             GAPE(I)=MAX(GAPE(I),HALF*DX1)
             BGAPEMX   = MAX(BGAPEMX,GAPE(I)) ! BGAPEMX overall the model 
             IF(MT > 0)THEN
               STFE(I)=SLSFAC*DX*PM(20,MT)
             ELSE
               IF(NINTR >= 0) THEN
                 CALL ANCMSG(MSGID=95,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXT(NIXT,NELT),
     .                    C2='TRUSS',
     .                    I3=I)
               ENDIF
               IF(NINTR < 0) THEN
                 CALL ANCMSG(MSGID=96,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXT(NIXT,NELT),
     .                    C2='TRUSS',
     .                    I3=I)
                ENDIF
             ENDIF
C -----Friction model ------
             IF(INTFRIC > 0) THEN
                IP= IPARTT(NELT)
                IPG = TAGPRT_FRIC(IP)
                IF(IPG > 0) THEN
                   CALL FRICTION_PARTS_SEARCH (
     .                       IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                       INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )
                    IPARTFRIC_E(I) = IPL
                ENDIF
              ENDIF
              LEDGE(1:NLEDGE,I) =  0
              LEDGE(5,I) = IRECT(1,I)  
              LEDGE(6,I) = IRECT(2,I)    
              LEDGE(7,I) = 3 
             LEDGE(15,I) = NELT + OFFT                  
C------------------------------------
          ELSEIF(NELP /= 0) THEN

             MT=IXP(1,NELP)
             MG=IXP(5,NELP)
             IP = IPARTP(NELP)
             IF ( IGAP == 5.AND.THK_M /= ZERO) THEN
               DX=THK_M_SCALE*HALF*THK_M
             ELSEIF (THK_PART(IP) > ZERO ) THEN
                DX1=GAPSCALE*GAPSCALE*THK_PART(IP)
             ELSE
                DX1=GAPSCALE*SQRT(GEO(1,MG))
             END IF
             DX=SQRT(GEO(1,MG))
             GAPE(I)=MAX(GAPE(I),HALF*DX1)
             BGAPEMX   = MAX(BGAPEMX,GAPE(I)) ! BGAPEMX overall the model 
             IF(MT > 0)THEN
                STFE(I)=SLSFAC*DX*PM(20,MT)
             ELSE
                IF(NINTR >= 0) THEN
                  CALL ANCMSG(MSGID=95,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXP(NIXP,NELP),
     .                    C2='BEAM',
     .                    I3=I)
                 ENDIF
                 IF(NINTR<0) THEN
                   CALL ANCMSG(MSGID=96,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXP(NIXP,NELP),
     .                    C2='BEAM',
     .                    I3=I)
                 ENDIF
             ENDIF
C -----Friction model ------
             IF(INTFRIC > 0) THEN
                IP= IPARTP(NELP)
                IPG = TAGPRT_FRIC(IP)
                IF(IPG > 0) THEN
                  CALL FRICTION_PARTS_SEARCH (
     .                       IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                       INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )
                  IPARTFRIC_E(I) = IPL
                ENDIF
             ENDIF
             LEDGE(1:NLEDGE,I) =  0
             LEDGE(5,I) = IRECT(1,I)  
             LEDGE(6,I) = IRECT(2,I)    
             LEDGE(7,I) = 3 
             LEDGE(15,I) = NELP + OFFP
C------------------------------------
          ELSEIF(NELR /= 0) THEN

             MG=IXR(1,NELR)
             MT = IXR(5,NELR)
             IP = IPARTR(NELR)
             IF ( IGAP == 5.AND.THK_M /= ZERO) THEN
               DX1=THK_M_SCALE*HALF*THK_M
             ELSEIF(IP > 0)THEN
                IF (THK_PART(IP) > ZERO ) THEN
                 DX1=GAPSCALE*THK_PART(IP)
                END IF
             ENDIF
             GAPE(I)=MAX(GAPE(I),HALF*DX1)
             BGAPEMX   = MAX(BGAPEMX,GAPE(I)) ! BGAPEMX overall the model 
             IF(MG > 0)THEN
               IGTYP=NINT(GEO(12,MG))
               IF(IGTYP==4.OR.IGTYP==12)THEN
                  STFE(I)=SLSFAC*GEO(2,MG)
               ELSEIF(IGTYP==8.OR.IGTYP==13)THEN
                  STFE(I)=SLSFAC*MAX(GEO(3,MG),GEO(10,MG),GEO(15,MG))
               ELSEIF(IGTYP == 23)THEN
                  STFE(I)=SLSFAC*MAX(PM(191,MT),PM(192,MT),PM(193,MT))
               ELSEIF(IGTYP==25)THEN
                  STFE(I)=SLSFAC*GEO(10,MG)
               ELSEIF(IGTYP>=29)THEN
                  STFE(I)=SLSFAC*GEO(3,MG)
               ELSE
                  WRITE(6,'(A)') 'INTERNAL ERROR 987'
                  CALL MY_EXIT(2)
               ENDIF
             ELSE
               IF(NINTR >= 0) THEN
                  CALL ANCMSG(MSGID=95,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXR(NIXR,NELR),
     .                    C2='SPRING',
     .                    I3=I)
               ENDIF
               IF(NINTR < 0) THEN
                 CALL ANCMSG(MSGID=96,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=IXR(NIXR,NELR),
     .                    C2='SPRING',
     .                    I3=I)
               ENDIF
             ENDIF
C -----Friction model ------
             IF(INTFRIC > 0) THEN
               IP= IPARTR(NELR)
               IPG = TAGPRT_FRIC(IP)
               IF(IPG > 0) THEN
                  CALL FRICTION_PARTS_SEARCH (
     .                       IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                       INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )
                  IPARTFRIC_E(I) = IPL
              ENDIF
             ENDIF
             LEDGE(1:NLEDGE,I) =  0
             LEDGE(5,I) = IRECT(1,I)  
             LEDGE(6,I) = IRECT(2,I)    
             LEDGE(7,I) = 3 
             LEDGE(15,I) = NELR + OFFR
C------------------------------------
          ELSEIF(NELX /= 0) THEN
             MG=KXX(2,NELX)
             IF(MG>0)THEN
               STFE(I)=SLSFAC*GET_U_GEO(4,MG)*(KXX(3,NELX)-1)/LELX(NELX)
             ELSE
               IF(NINTR >= 0) THEN
                  CALL ANCMSG(MSGID=95,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=KXX(NIXX,NELX),
     .                    C2='XELEM',
     .                    I3=I)
               ENDIF
               IF(NINTR < 0) THEN
                 CALL ANCMSG(MSGID=96,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_2,
     .                    I1=ID,
     .                    C1=TITR,
     .                    I2=KXX(NIXX,NELX),
     .                    C2='XELEM',
     .                    I3=I)
               ENDIF
             ENDIF
C -----Friction model ------
             IF(INTFRIC > 0) THEN
               IP= IPARTX(NELX)
               IPG = TAGPRT_FRIC(IP)
               IF(IPG > 0) THEN
                 CALL FRICTION_PARTS_SEARCH (
     .                       IPG,INTBUF_FRIC_TAB(INTFRIC)%S_TABPARTS_FRIC,
     .                       INTBUF_FRIC_TAB(INTFRIC)%TABPARTS_FRIC,IPL )
                 IPARTFRIC_E(I) = IPL
               ENDIF
             ENDIF
C------------------------------------
             LEDGE(1:NLEDGE,I) =  0
             LEDGE(5,I) = IRECT(1,I)  
             LEDGE(6,I) = IRECT(2,I)    
             LEDGE(7,I) = 3 
          ENDIF

          IF(NELT+NELP+NELR+NUMELX==0.AND.LEDGE(1,I)==0)THEN
            IF(NINTR > 0) THEN
              CALL ANCMSG(MSGID=481,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
             ENDIF
             IF(NINTR < 0) THEN
               CALL ANCMSG(MSGID=482,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=I)
          ENDIF
         ENDIF

         IF(IGAP==3)THEN
           N1=LEDGE(5,I)
           N2=LEDGE(6,I)
           IF(N1 /= N2 .AND. N1 /= 0)
     .       XL=SQRT((X(1,N1)-X(1,N2))**2+(X(2,N1)-X(2,N2))**2+
     .                 (X(3,N1)-X(3,N2))**2)

            GAP_E_L(I)=MIN(PERCENT_SIZE*XL,GAPMAX_M)
            BGAPEMX_L   = MAX(BGAPEMX_L,GAP_E_L(I)) ! BGAPEMX_L overall the model   
          ENDIF
         ENDIF
        ENDDO  
      ENDIF 
C-------------------------------      
      RETURN
      END
