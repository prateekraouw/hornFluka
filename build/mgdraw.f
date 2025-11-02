*                                                                      *
*=== mgdraw ===========================================================*
*                                                                      *
      SUBROUTINE MGDRAW ( ICODE, MREG )

      INCLUDE '(DBLPRC)'
      INCLUDE '(DIMPAR)'
      INCLUDE '(IOUNIT)'
*
*----------------------------------------------------------------------*
*                                                                      *
*     Copyright (C) 2003-2019:  CERN & INFN                            *
*     All Rights Reserved.                                             *
*                                                                      *
*----------------------------------------------------------------------*
*
      INCLUDE '(CASLIM)'
      INCLUDE '(COMPUT)'
      INCLUDE '(SOURCM)'
      INCLUDE '(FHEAVY)'
      INCLUDE '(FLKSTK)'
      INCLUDE '(GENSTK)'
      INCLUDE '(MGDDCM)'
      INCLUDE '(PAPROP)'
      INCLUDE '(QUEMGD)'
      INCLUDE '(SUMCOU)'
      INCLUDE '(TRACKR)'
*
      DIMENSION DTQUEN ( MXTRCK, MAXQMG )
*
      CHARACTER*20 FILNAM
      LOGICAL LFCOPE
      LOGICAL LCROSS
      SAVE LFCOPE, LCROSS
      DATA LFCOPE / .FALSE. /
      DATA LCROSS / .FALSE. /
*
*----------------------------------------------------------------------*
*
      IF ( .NOT. LFCOPE ) THEN
         LFCOPE = .TRUE.
         IF ( KOMPUT .EQ. 2 ) THEN
            FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
         ELSE
            FILNAM = CFDRAW
         END IF
         OPEN ( UNIT = IODRAW, FILE = FILNAM, STATUS = 'NEW', FORM =
     &          'UNFORMATTED' )
      END IF
      WRITE (IODRAW) NTRACK, MTRACK, JTRACK, SNGL (ETRACK),
     &               SNGL (WTRACK)
      WRITE (IODRAW) ( SNGL (XTRACK (I)), SNGL (YTRACK (I)),
     &                 SNGL (ZTRACK (I)), I = 0, NTRACK ),
     &               ( SNGL (DTRACK (I)), I = 1, MTRACK ),
     &                 SNGL (CTRACK)
*  +-------------------------------------------------------------------*
*  |  Quenching is activated
      IF ( LQEMGD ) THEN
         IF ( MTRACK .GT. 0 ) THEN
            RULLL  = ZERZER
            CALL QUENMG ( ICODE, MREG, RULLL, DTQUEN )
            WRITE (IODRAW) ( ( SNGL (DTQUEN (I,JBK)), I = 1, MTRACK ),
     &                         JBK = 1, NQEMGD )
         END IF
      END IF
*  |  End of quenching
*  +-------------------------------------------------------------------*
      RETURN
*
*======================================================================*
*                                                                      *
*     Boundary-(X)crossing DRAWing:                                    *
*                                                                      *
*======================================================================*
*                                                                      *
      ENTRY BXDRAW ( ICODE, MREG, NEWREG, XSCO, YSCO, ZSCO )
*
*     Initialize CSV file on first call and write header
      IF (.NOT. LCROSS) THEN
         LCROSS = .TRUE.
         OPEN(UNIT=99, FILE='horn_crossings.csv', STATUS='UNKNOWN')
         WRITE(99,'(A)') 'EvtNo,Type,E,KE,Weight,X,Y,Z,Cx,Cy,Cz,' //
     &        'Age,Px,Py,Pz,MREG,NEWREG'
*        Also open debug file to check all crossings
         OPEN(UNIT=98, FILE='all_crossings_debug.csv', 
     &        STATUS='UNKNOWN')
         WRITE(98,'(A)') 'EvtNo,Type,MREG,NEWREG,X,Y,Z'
      END IF
*
*     Log ALL boundary crossings to debug file
      WRITE(98,'(I11,",",I3,",",I3,",",I3,",",3(1PE15.7,","))')
     &      NCASE, JTRACK, MREG, NEWREG, XSCO, YSCO, ZSCO
*
*     Store particles crossing INTO region 4 (aperturePhys)
*     From any other region
      IF (NEWREG.EQ.4) THEN
*        Store protons(1), mu+(10), mu-(11), pi+(13), pi-(14)
         IF (JTRACK.EQ.1.OR.JTRACK.EQ.10.OR.JTRACK.EQ.11.OR.
     &        JTRACK.EQ.13.OR.JTRACK.EQ.14) THEN
            WRITE(99,102) NCASE,JTRACK,ETRACK,ETRACK-AM(JTRACK),
     &      WTRACK,XSCO,YSCO,ZSCO,CXTRCK,CYTRCK,CZTRCK,ATRACK,
     &      PTRACK*CXTRCK,PTRACK*CYTRCK,PTRACK*CZTRCK,MREG,NEWREG
 102        FORMAT(I11,',',I3,',',1PE15.7,',',1PE15.7,',',1PE15.7,',',
     &             1PE15.7,',',1PE15.7,',',1PE15.7,',',1PE15.7,',',
     &             1PE15.7,',',1PE15.7,',',1PE15.7,',',1PE15.7,',',
     &             1PE15.7,',',1PE15.7,',',I3,',',I3)
         ENDIF
      ENDIF

      RETURN
*
*======================================================================*
*                                                                      *
*     Event End DRAWing:                                               *
*                                                                      *
*======================================================================*
*                                                                      *
      ENTRY EEDRAW ( ICODE )
      RETURN
*
*======================================================================*
*                                                                      *
*     ENergy deposition DRAWing:                                       *
*                                                                      *
*======================================================================*
*                                                                      *
      ENTRY ENDRAW ( ICODE, MREG, RULL, XSCO, YSCO, ZSCO )
      RETURN
*
*======================================================================*
*                                                                      *
*     SOurce particle DRAWing:                                         *
*                                                                      *
*======================================================================*
*
      ENTRY SODRAW
      IF ( .NOT. LFCOPE ) THEN
         LFCOPE = .TRUE.
         IF ( KOMPUT .EQ. 2 ) THEN
            FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
         ELSE
            FILNAM = CFDRAW
         END IF
         OPEN ( UNIT = IODRAW, FILE = FILNAM, STATUS = 'NEW', FORM =
     &          'UNFORMATTED' )
      END IF
      WRITE (IODRAW) -NCASE, NPFLKA, NSTMAX, SNGL (TKESUM),
     &                SNGL (WEIPRI)
*  +-------------------------------------------------------------------*
*  |  (Radioactive) isotope
      IF ( ILOFLK (NPFLKA) .GE. 100000 .AND. LRADDC (NPFLKA) ) THEN
         IARES  = MOD ( ILOFLK (NPFLKA), 100000  )  / 100
         IZRES  = MOD ( ILOFLK (NPFLKA), 10000000 ) / 100000
         IISRES = ILOFLK (NPFLKA) / 10000000
         IONID  = ILOFLK (NPFLKA)
         WRITE (IODRAW) ( IONID,SNGL(-TKEFLK(I)),
     &                    SNGL (WTFLK(I)), SNGL (XFLK (I)),
     &                    SNGL (YFLK (I)), SNGL (ZFLK (I)),
     &                    SNGL (TXFLK(I)), SNGL (TYFLK(I)),
     &                    SNGL (TZFLK(I)), I = 1, NPFLKA )
*  |
*  +-------------------------------------------------------------------*
*  |  Patch for heavy ions
      ELSE IF ( ABS (ILOFLK (NPFLKA)) .GE. 10000 ) THEN
         IONID = ILOFLK (NPFLKA)
         CALL DCDION ( IONID )
         WRITE (IODRAW) ( IONID,SNGL(TKEFLK(I)+AMNHEA(-IONID)),
     &                    SNGL (WTFLK(I)), SNGL (XFLK (I)),
     &                    SNGL (YFLK (I)), SNGL (ZFLK (I)),
     &                    SNGL (TXFLK(I)), SNGL (TYFLK(I)),
     &                    SNGL (TZFLK(I)), I = 1, NPFLKA )
*  |
*  +-------------------------------------------------------------------*
*  |  Patch for heavy ions
      ELSE IF ( ILOFLK (NPFLKA) .LT. -6 ) THEN
         WRITE (IODRAW) ( IONID,SNGL(TKEFLK(I)+AMNHEA(-ILOFLK(NPFLKA))),
     &                    SNGL (WTFLK(I)), SNGL (XFLK (I)),
     &                    SNGL (YFLK (I)), SNGL (ZFLK (I)),
     &                    SNGL (TXFLK(I)), SNGL (TYFLK(I)),
     &                    SNGL (TZFLK(I)), I = 1, NPFLKA )
*  |
*  +-------------------------------------------------------------------*
*  |
      ELSE
         WRITE (IODRAW) ( ILOFLK(I), SNGL (TKEFLK(I)+AM(ILOFLK(I))),
     &                    SNGL (WTFLK(I)), SNGL (XFLK (I)),
     &                    SNGL (YFLK (I)), SNGL (ZFLK (I)),
     &                    SNGL (TXFLK(I)), SNGL (TYFLK(I)),
     &                    SNGL (TZFLK(I)), I = 1, NPFLKA )
      END IF
*  |
*  +-------------------------------------------------------------------*
      RETURN
*
*======================================================================*
*                                                                      *
*     USer dependent DRAWing:                                          *
*                                                                      *
*======================================================================*
*
      ENTRY USDRAW ( ICODE, MREG, XSCO, YSCO, ZSCO )
      IF ( .NOT. LFCOPE ) THEN
         LFCOPE = .TRUE.
         IF ( KOMPUT .EQ. 2 ) THEN
            FILNAM = '/'//CFDRAW(1:8)//' DUMP A'
         ELSE
            FILNAM = CFDRAW
         END IF
         OPEN ( UNIT = IODRAW, FILE = FILNAM, STATUS = 'NEW', FORM =
     &          'UNFORMATTED' )
      END IF
* No output by default:
      RETURN
*=== End of subroutine Mgdraw =========================================*
      END