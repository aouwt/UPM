$NOPREFIX
TYPE UPMType
    Name AS STRING
    Dev AS STRING
    Desc AS STRING
    Date AS STRING
    Ver AS STRING
    VerNo AS FLOAT
    Test AS BYTE
END TYPE

TYPE FileType
    Name AS STRING
    Len AS UNSIGNED LONG
END TYPE

install "github.com/all-other-usernames-were-taken/UPM/blob/main/test.upm"
SUB install (UPMUrl$)
    DIM f AS STRING
    PRINT "Installing package..."

    PRINT "  Downloading file..."
    'LoadFile f, UPMUrl$
    f = DownloadFile(UPMUrl$, 10)
    IF f = "" THEN
        PRINT "Download failed."
        EXIT SUB
    END IF

    'PRINT "  Decompressing...";
    'f = DEFLATE$(f)
    'PRINT "done"

    PRINT "  Reading package info...";
    DIM UPM AS UPMType, File(100) AS FileType
    CALL ParseUPM(f, UPM, File(), ofs%)
    PRINT "done"

    PRINT "  Unpacking files..."
    MKDIR "/tmp/upm"
    DIM n AS UNSIGNED INTEGER
    DIM UPMPos AS UNSIGNED LONG
    UPMPos = ofs%
    DO

        PRINT File(n).Name; "...";
        f% = FREEFILE
        OPEN "/tmp/upm/" + File(n).Name FOR OUTPUT AS #f%
        PRINT #f%, MID$(f, UPMPos, File(n).Len);
        CLOSE #f%
        UPMPos = UPMPos + File(n).Len
        n = n + 1
        PRINT "done"

    LOOP UNTIL File(n).Name = ""
    PRINT "  Files sucessfully unpacked."

    PRINT "  Installing via external installer..."
    SHELL "/tmp/upm/i.sh"
    PRINT "  Installation completed,"

    PRINT "  Cleaning up...";
    'SHELL "rm -rf /tmp/upm"
    'SHELL "/tmp/upm/c.sh"
    PRINT "done"

    PRINT "Installation completed sucessfully!"
END SUB


SUB LoadFile (filedata$, file$)

    f% = FREEFILE
    OPEN file$ FOR BINARY AS #f%
    filedata$ = SPACE$(LOF(f%))
    GET #f%, , filedata$
    CLOSE #f%

END SUB


'Taken from QB64 wiki
'Adapted to use a string instead of a file
FUNCTION DownloadFile$ (url$, timelimit) ' returns -1 if successful, 0 if not
    PRINT "    Parsing URL...";
    url2$ = url$
    x = INSTR(url2$, "/")
    IF x THEN url2$ = LEFT$(url$, x - 1)
    PRINT "done"
    PRINT "    Connecting to " + url2$ + "...";
    client = _OPENCLIENT("TCP/IP:80:" + url2$)
    IF client = 0 THEN
        PRINT "failed"
        PRINT "Failed to connect. Check your internet connection and try again."
        EXIT FUNCTION
    END IF
    PRINT "done"
    PRINT "    Sending HTTP request...";
    e$ = CHR$(13) + CHR$(10) ' end of line characters
    url3$ = RIGHT$(url$, LEN(url$) - x + 1)
    x$ = "GET " + url3$ + " HTTP/1.1" + e$
    x$ = x$ + "Host: " + url2$ + e$ + e$
    PUT #client, , x$
    PRINT "done"
    PRINT "    Recieving file...";
    t! = TIMER ' start time
    DO
        _DELAY 0.05 ' 50ms delay (20 checks per second)
        GET #client, , a2$
        a$ = a$ + a2$
        i = INSTR(a$, "Content-Length:")
        IF i THEN
            i2 = INSTR(i, a$, e$)
            IF i2 THEN
                l = VAL(MID$(a$, i + 15, i2 - i - 14))
                i3 = INSTR(i2, a$, e$ + e$)
                IF i3 THEN
                    i3 = i3 + 4 'move i3 to start of data
                    IF (LEN(a$) - i3 + 1) = l THEN
                        CLOSE client ' CLOSE CLIENT
                        DownloadFile$ = MID$(a$, i3, l)
                        PRINT "done"
                        EXIT FUNCTION
                    END IF ' availabledata = l
                END IF ' i3
            END IF ' i2
        END IF ' i
    LOOP UNTIL TIMER > t! + timelimit ' (in seconds)
    CLOSE client
    PRINT "failed"
END FUNCTION


'taken from QB64-Themes
SUB ParseUPM (UPMData$, UPM AS UPMType, File() AS FileType, UPMEnd AS INTEGER) 'Based on the SCS loader for stupidc
    DO
        GOSUB NewLine
        IF Ln$ = "" THEN EXIT DO
        IF ASC(Ln$) = 46 THEN EXIT DO 'period for end
        IF ASC(Ln$) = 35 THEN _CONTINUE 'Hashtags for comments

        SELECT CASE Ln$
            CASE "desc"
                DO
                    GOSUB NewLine
                    IF Ln$ = "." THEN EXIT DO
                    UPM.Desc = UPM.Desc + Ln$ + CHR$(10)
                LOOP


            CASE "files"
                DIM n AS UNSIGNED LONG
                DO
                    GOSUB NewLine
                    IF Ln$ = "." THEN EXIT DO
                    GOSUB Separate
                    File(n).Name = Val$
                    File(n).Len = VAL(Key$)
                    n = n + 1
                LOOP


            CASE ELSE 'Single line keys
                GOSUB Separate
                SELECT CASE Key$
                    CASE "dev"
                        UPM.Dev = Val$
                    CASE "date"
                        UPM.Date = Val$
                    CASE "ver"
                        UPM.Ver = Val$
                    CASE "verno"
                        UPM.VerNo = VAL(Val$)
                END SELECT
        END SELECT
    LOOP
    UPMEnd = NextLine_Start% + 1
    EXIT SUB


    NewLine:

    NextLine_Start% = NextLine_End% + 1
    NextLine_End% = INSTR(NextLine_Start%, UPMData$, CHR$(10))

    IF NextLine_End% = 0 THEN Ln$ = MID$(UPMData$, NextLine_Start%): RETURN

    Ln$ = LTRIM$(RTRIM$(MID$(UPMData$, NextLine_Start%, NextLine_End% - NextLine_Start%)))
    RETURN


    Separate:
    I% = INSTR(Ln$, " ")
    IF I% = 0 THEN RETURN
    Key$ = LTRIM$(RTRIM$(LEFT$(Ln$, I%)))
    Val$ = LTRIM$(RTRIM$(MID$(Ln$, I% + 1)))
    RETURN
END SUB

