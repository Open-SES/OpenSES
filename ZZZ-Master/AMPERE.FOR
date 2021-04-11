      SUBROUTINE AMPERE(TE,TEREQ,UTRN,ITYP)

      !DATE: 26 JUNE 2000

      !INPUT TO THIS SUBROUTINE IS TRACTIVE EFFORT AVAILABLE, TRACTIVE
      !EFFORT REQUIRED AND TRAIN SPEED.
      !OUTPUT IS THE AMPERAGE REQUIRED BY THE MOTOR TO PRODUCE THIS
      !TRACTIVE EFFORT AND THE CORRESPONDING LINE CURRENT DRAWN BY EACH
      !POWERED CAR.
      !TRACTIVE EFFORT IS IN LBS AT THE WHEEL RIM AND TRAIN SPEED IS IN
      !FEET PER SECOND.

      INCLUDE  'DSHARE'
      REAL     IBASE, ILMAX
      
      !BRANCH ON MOTOR TYPE
      NUMY1 = NOPTV(ITYP)
      MOTOR_TYPE: SELECT CASE (NUMY1)
      CASE(1)
            !CAM CONTROLLED MOTOR
            UBASE = SP1(ITYP)
            IBASE = COAMP(ITYP,4)
            !NORMALIZE SPEED AND TRACTIVE EFFORT ON BASE CONDITIONS
            TRATIO = TEREQ/TE
            IF(TRATIO .LT. 0.0 ) TRATIO=0.0
            IF(TRATIO .GT. 1.0 ) TRATIO=1.0
            IF(TE .LE. TEREQ) THEN
                  !FULL POWER MODE
                  IF (UTRN .LT. UBASE) THEN
                        !BELOW BASE SPEED
                        IMOTOR = IBASE
                        !COMPUTE LINE CURRENT
                        !CHECK IF TRAIN SPEED IS ABOVE OR BELOW THE TRANSITION SPEED.
                        IF (UTRN .LE. U2M(ITYP)) THEN
                              !THE TRAIN IS OPERATING BELOW TRANSITION SPEED - THEREFORE THE
                              !TRACTION MOTORS ARE IN SERIES.
                              ILINE = IMOTOR
                        ELSE
                              !THE TRAIN IS OPERATING ABOVE TRANSITION SPEED - THEREFORE THE
                              !TRACTION MOTOR CONFIGURATION IS SERIES/PARALLEL
                              ILINE = IMOTOR*2.0
                        END IF
                  ELSE
                        !ABOVE BASE SPEED
                        IMOTOR = COAMP(ITYP,1)+COAMP(ITYP,2)/UTRN
     1                                   +COAMP(ITYP,3)/UTRN**2
                        !CHECK IF AMPERAGE EXCEEDS MAXIMUM VALUE
                        IF (IMOTOR .GT. IBASE) THEN
                              IMOTOR=IBASE
                        END IF
                        IF (UTRN .LE. U2M(ITYP)) THEN
                              !THE TRAIN IS OPERATING BELOW TRANSITION SPEED - THEREFORE THE
                              !TRACTION MOTORS ARE IN SERIES.
                              ILINE = IMOTOR
                        ELSE
                              !THE TRAIN IS OPERATING ABOVE TRANSITION SPEED - THEREFORE THE
                              !TRACTION MOTOR CONFIGURATION IS SERIES/PARALLEL
                              ILINE = IMOTOR*2.0
                        END IF
                  END IF
            ELSE
                  !PARTIAL POWER MODE
                  IF (UTRN .LT. UBASE) THEN
                        !BELOW BASE SPEED
                        IMOTOR = IBASE * SQRT(TRATIO)
                        !COMPUTE LINE CURRENT
                        !CHECK IF TRAIN SPEED IS ABOVE OR BELOW THE TRANSITION SPEED.
                        IF (UTRN .LE. U2M(ITYP)) THEN
                              !THE TRAIN IS OPERATING BELOW TRANSITION SPEED - THEREFORE THE
                              !TRACTION MOTORS ARE IN SERIES.
                              ILINE = IMOTOR
                        ELSE
                              !THE TRAIN IS OPERATING ABOVE TRANSITION SPEED - THEREFORE THE
                              !TRACTION MOTOR CONFIGURATION IS SERIES/PARALLEL
                              ILINE = IMOTOR*2.0
                        END IF
                  ELSE
                        !ABOVE BASE SPEED
                        CURRNT = COAMP(ITYP,1)+COAMP(ITYP,2)/UTRN
     1                                   +COAMP(ITYP,3) /UTRN**2
                        IMOTOR = CURRNT * SQRT(TRATIO)
                        !CHECK IF AMPERAGE EXCEEDS MAXIMUM VALUE
                        IF (IMOTOR .GT. IBASE) THEN
                              IMOTOR=IBASE
                        END IF
                        IF (UTRN .LE. U2M(ITYP)) THEN
                              !THE TRAIN IS OPERATING BELOW TRANSITION SPEED - THEREFORE THE
                              !TRACTION MOTORS ARE IN SERIES.
                              ILINE = IMOTOR
                        ELSE
                              !THE TRAIN IS OPERATING ABOVE TRANSITION SPEED - THEREFORE THE
                              !TRACTION MOTOR CONFIGURATION IS SERIES/PARALLEL
                              ILINE = IMOTOR*2.0
                        END IF
                  END IF
            END IF
      CASE(2)
            !CHOPPER CONTROLLED MOTOR
            UBASE = SP1(ITYP)
            IBASE = COAMP(ITYP,4)
            !MAXIMUM LINE CURRENT
            ILMAX = 2.0 * COAMP(ITYP,4)
            !NORMALIZE SPEED AND TRACTIVE EFFORT ON BASE CONDITIONS
            TRATIO = TEREQ/TE
            IF(TRATIO .LT. 0.0 ) TRATIO=0.0
            IF(TRATIO .GT. 1.0 ) TRATIO=1.0
            IF(TE .LE. TEREQ) THEN
                  !FULL POWER MODE
                  IF (UTRN .LT. UBASE) THEN
                        !BELOW BASE SPEED
                        IMOTOR = IBASE
                        !COMPUTE LINE CURRENT
                        ILINE = COAMPL(ITYP,6)+SLOPEV(ITYP)*UTRN
                  ELSE
                        !ABOVE BASE SPEED
                        IMOTOR = COAMP(ITYP,1)+COAMP(ITYP,2)/UTRN
     1                         +COAMP(ITYP,3)/UTRN**2
                        !CHECK IF AMPERAGE EXCEEDS MAXIMUM VALUE
                        IF (IMOTOR .GT. IBASE) THEN
                              IMOTOR=IBASE
                        END IF
                        !COMPUTE LINE CURRENT
                        ILINE = COAMPL(ITYP,1)+COAMPL(ITYP,2)
     1                                   /UTRN+COAMPL(ITYP,3)/UTRN**2
                        IF (ILINE .GT. ILMAX) THEN
                              ILINE = ILMAX
                        END IF
                  END IF
            ELSE
                  !PARTIAL POWER MODE
                  IF (UTRN .LT. UBASE) THEN
                        !BELOW BASE SPEED
                        IMOTOR = IBASE * SQRT(TRATIO)
                        !COMPUTE LINE CURRENT
                        ILINE = (COAMPL(ITYP,6)+SLOPEV(ITYP)*UTRN)
     1                             *TRATIO
                  ELSE
                        !ABOVE BASE SPEED
                        CURRNT = COAMP(ITYP,1)+COAMP(ITYP,2)/UTRN
     1                                   +COAMP(ITYP,3)/UTRN**2
                        ILINE = COAMPL(ITYP,1)+COAMPL(ITYP,2)/UTRN
     1                                   +COAMPL(ITYP,3)/UTRN**2
                        IMOTOR = CURRNT * SQRT(TRATIO)
                        ILINE  = ILINE  *      TRATIO
                        IF( CURRNT .LT. IMOTOR ) THEN
                              IMOTOR=CURRNT
                        END IF
                        IF (ILINE .GT. ILMAX) THEN
                              ILINE = ILMAX
                        END IF
                  END IF
            END IF
      END SELECT MOTOR_TYPE
      RETURN
      END
      