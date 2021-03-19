$NOPREFIX
$CONSOLE:ONLY
_DEST _CONSOLE

TYPE UPMType
    Name AS STRING
    Dev AS STRING
    Desc AS STRING
    Date AS STRING
    Ver AS STRING
    VerNo AS FLOAT
END TYPE

TYPE FileType
    Name AS STRING
    Len AS UNSIGNED LONG
END TYPE
PRINT "############### Universal Package Manager #################"
PRINT "#      v1 -- (c) 2021 all-other-usernames-were-taken      #"
PRINT "#  https://github.com/all-other-usernames-were-taken/upm  #"
PRINT "#"; RandomMsg$(57); "#"
PRINT "###########################################################"
REM install "https://raw.githubusercontent.com/all-other-usernames-were-taken/UPM/main/test.upm" '<- Ah, GH needs HTTPS. We must use curl, wget, or the like, OR use local files for testing.

SELECT CASE COMMAND$(1)
    CASE "install"
        install COMMAND$(2)

    CASE "generate-ui"
        RunGenUI

    CASE "help", "?", "--help", "-h"
        PRINT "USAGE: upm install <package>"
        PRINT "       upm generate-ui"
        'PRINT "       upm generate <options>"
        PRINT "       upm help"
        PRINT ""
    CASE ELSE
        PRINT "Invalid command '" + COMMAND$(1) + "'. Use 'upm help' for valid commands."
END SELECT
SYSTEM


SUB RunGenUI
    DIM UPM AS UPMType
    upmfile$ = "Untitled.upm"
    UPM.Name = "untitled-program"
    UPM.Dev = "NA"
    UPM.Desc = "NA"
    UPM.Date = DATE$
    UPM.Ver = "NA"
    UPM.VerNo = 0

    DO
        ContLoop:
        GOSUB refresh
        INPUT "Press a key: ", k$
        SELECT CASE LCASE$(RIGHT$(k$, 1))
            CASE "n"
                INPUT "Package Name: ", k$
                FOR i% = 1 TO LEN(k$)
                    SELECT CASE ASC(k$, i%)
                        CASE ASC("a") - ASC("z"), ASC("-"), ASC("_"), ASC(".")
                        CASE ELSE
                            PRINT "Error: Package name cannot have control characters!"
                            INPUT "Press any key...", k$
                            GOTO ContLoop
                    END SELECT
                NEXT
                k$ = UPM.Name

                'CASE "d"
            CASE "e"
                INPUT "Developer Name: ", UPM.Dev

            CASE "v"
                INPUT "Version: ", UPM.Ver

            CASE "r"
                INPUT "Version Number: ", UPM.VerNo

            CASE "g"

            CASE "f"
                INPUT "Grab files from: ", k$
                IF NOT DIREXISTS(k$) THEN
                    PRINT "Error: Directory does not exist!"
                    INPUT "Press any key...", k$
                    GOTO ContLoop
                END IF
                upmdir$ = k$

        END SELECT
    LOOP

    refresh:
    CLS
    PRINT "UPM Generator - v1 | "; UPM.Name
    PRINT ""
    PRINT "Grab (f)iles from: "; upmdir$
    PRINT "(N)ame: "; UPM.Name
    PRINT "(D)ate: "; UPM.Date
    PRINT "D(e)veloper: "; UPM.Dev
    PRINT "(V)ersion: "; UPM.Ver
    PRINT "Ve(r)sion Number: "; UPM.VerNo
    PRINT ""
    PRINT "<(G)enerate>"
    RETURN
END SUB

SUB install (UPMUrl$)
    DIM f AS STRING
    PRINT "Installing package..."
    IF DIREXISTS("/tmp/upm") THEN
        PRINT "  Removing /tmp/upm...";
        SHELL "rm -rf /tmp/upm"
        PRINT "done"
        PRINT
    END IF

    MKDIR "/tmp/upm/"

    IF LEFT$(UPMUrl$, 7) = "file://" THEN 'file:// are local files
        PRINT "  Loading file...";
        f = LoadFile(MID$(UPMUrl$, 7))
        PRINT "done"

    ELSEIF LEFT$(UPMUrl$, 7) = "http://" THEN 'http:// we can use built-in downlaoder. QB64 cannot use SSL as far as i have seen, so...
        PRINT "  Downloading file..."
        f = DownloadFile(MID$(UPMUrl$, 7), 10)
        IF f = "" THEN
            PRINT "Download failed."
            EXIT SUB
        END IF

    ELSE 'for https:// we need to use curl/wget.
        PRINT "  Downloading file..."
        PRINT "    curl '" + UPMUrl$ + "' -o /tmp/upm/pkg"
        SHELL "curl '" + UPMUrl$ + "' -o /tmp/upm/pkg"

        IF FILEEXISTS("/tmp/upm/pkg") THEN
            PRINT "    Loading file...";
            f = LoadFile("/tmp/upm/pkg")
            PRINT "done"
            PRINT "    Removing file...";
            SHELL "rm -f /tmp/upm/pkg"
            PRINT "done"
            PRINT
        ELSE
            PRINT "  File could not be downloaded!"
            EXIT SUB
        END IF
    END IF

    'PRINT "  Decompressing...";
    'f = DEFLATE$(f)
    'PRINT "done"
    '?

    PRINT "  Reading package info...";
    DIM UPM AS UPMType, File(100) AS FileType
    CALL ParseUPM(f, UPM, File(), ofs%)
    PRINT "done"
    PRINT

    PRINT "  Unpacking files..."
    DIM n AS UNSIGNED INTEGER
    DIM UPMPos AS UNSIGNED LONG
    UPMPos = ofs%
    n = 0
    DO

        PRINT "    "; File(n).Name; "...";
        f% = FREEFILE
        OPEN "/tmp/upm/" + File(n).Name FOR OUTPUT AS #f%
        PRINT #f%, MID$(f, UPMPos, File(n).Len);
        CLOSE #f%
        UPMPos = UPMPos + File(n).Len
        n = n + 1
        PRINT "done"

    LOOP UNTIL File(n).Name = ""
    PRINT "  Files sucessfully unpacked."
    PRINT


    IF FILEEXISTS("/tmp/upm/i.sh") THEN
        PRINT "  Installing via external installer..."
        SHELL "/tmp/upm/i.sh"
        PRINT "  Installation script done."
        PRINT
    ELSE
        PRINT "  Installation script not found, copying all files to CWD...";
        SHELL "cp -r /tmp/upm/* ."
        PRINT "done"
        PRINT
    END IF

    PRINT "  Cleaning up...";
    SHELL "rm -rf /tmp/upm"
    'SHELL "/tmp/upm/c.sh"
    PRINT "done"
    PRINT

    PRINT "Installation completed sucessfully!"
END SUB


FUNCTION LoadFile$ (file$)

    f% = FREEFILE
    OPEN file$ FOR BINARY AS #f%
    LoadFile$ = SPACE$(LOF(f%))
    GET #f%, , LoadFile$
    CLOSE #f%

END FUNCTION


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
                    IF ASC(Ln$) = 46 THEN EXIT DO
                    UPM.Desc = UPM.Desc + Ln$ + CHR$(10)
                LOOP


            CASE "files"
                DIM n AS UNSIGNED LONG
                DO
                    GOSUB NewLine
                    IF ASC(Ln$) = 46 THEN EXIT DO
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


FUNCTION RandomMsg$ (l%)
    CONST Items = 36
    RANDOMIZE TIMER

    ChooseNew:
    ItemToPrint% = RND * Items
    RESTORE msgs
    FOR i% = 0 TO ItemToPrint%
        READ s$
    NEXT
    IF LEN(s$) > l% GOTO ChooseNew 'Make sure we aren't using a string that's too long

    RandomMsg$ = SPACE$((l% - LEN(s$)) / 2) + s$
    RandomMsg$ = RandomMsg$ + SPACE$(l% - LEN(RandomMsg$))
    msgs:
    DATA "(insert clever joke here)"
    DATA "Totally not a ripoff of dpkg!"
    DATA "rm -rf / --no-preserve-root"
    DATA "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
    DATA "Isn't this the greatest thing ever?": 'no
    DATA ":(){ :|:& };:"
    DATA "yet another package manager nobody needs"
    DATA "Why did the chicken cross the road?"
    DATA "i speel good"
    DATA "[Laughter] - You dumb bitch"
    DATA "computer go brr"
    DATA "  this text is TOTALLY centered!            "
    DATA "HOO HAH TIKI TIKI"
    DATA "reject humanity,return to monke"
    DATA "hmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm"
    DATA "Say hi to Fellippe for me!"
    DATA "10 PRINT 'Hello, World!'  20 GOTO 10"
    DATA "My man Kevin on the ledge and shit"
    DATA "skamtebord"
    DATA "rm -rf /"
    DATA "(Y) S ame"
    DATA "JMP *"
    DATA "ur mom gae"
    DATA "curl > wget. Fight me."
    DATA "hyperfixations go brr"
    DATA "i use arch btw": 'no i dont
    DATA "Dead memes go brr"
    DATA "FFFFFFFFFFFFFFFFUUUUUUUUUUUUUUU..."
    DATA "Free V-Bucks!"
    DATA "Look! It's a thing!"
    DATA "unfinished projects go brr"
    DATA "Hey all, Scott here!"
    DATA "I am out of ideas for what to put here lol."
    DATA "JOKEEFUNNY"
    DATA "I really should be doing homework right now..."
    DATA "penis"
END FUNCTION
