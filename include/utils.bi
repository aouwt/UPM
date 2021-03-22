'Taken from QB64 wiki
'Adapted to use a string instead of a file
FUNCTION DownloadFile$ (url$, timelimit) ' returns -1 if successful, 0 if not
    PRINT "    Parsing URL..."
    url2$ = url$
    x = INSTR(url2$, "/")
    IF x THEN url2$ = LEFT$(url$, x - 1)
    'PRINT "done"
    PRINT "    Connecting to " + url2$ + "..."
    client = _OPENCLIENT("TCP/IP:80:" + url2$)
    IF client = 0 THEN
        'PRINT "failed"
        PRINT "Failed to connect. Check your internet connection and try again."
        EXIT FUNCTION
    END IF
    'PRINT "done"
    PRINT "    Sending HTTP request..."
    e$ = CHR$(13) + CHR$(10) ' end of line characters
    url3$ = RIGHT$(url$, LEN(url$) - x + 1)
    x$ = "GET " + url3$ + " HTTP/1.1" + e$
    x$ = x$ + "Host: " + url2$ + e$ + e$
    PUT #client, , x$
    'PRINT "done"
    PRINT "    Recieving file..."
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
                        'PRINT "done"
                        EXIT FUNCTION
                    END IF ' availabledata = l
                END IF ' i3
            END IF ' i2
        END IF ' i
    LOOP UNTIL TIMER > t! + timelimit ' (in seconds)
    CLOSE client
    'PRINT "failed"
END FUNCTION


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

FUNCTION LoadFile$ (file$)

    f% = FREEFILE
    OPEN file$ FOR BINARY AS #f%
    LoadFile$ = SPACE$(LOF(f%))
    GET #f%, , LoadFile$
    CLOSE #f%

END FUNCTION

FUNCTION Toggle$ (Condition~&&)
    IF Condition~&& THEN Toggle$ = "[X]" ELSE Toggle$ = "[ ]"
END FUNCTION

FUNCTION GetURI$ (URI$)
    DIM f AS STRING
    IF LEFT$(URI$, 1) = "." OR LEFT$(URI$, 1) = "/" THEN '. and / are local files
        PRINT "  Loading file..."
        f = LoadFile(URI$)
        'PRINT "done"

    ELSEIF LEFT$(URI$, 7) = "http://" THEN 'http we can use built-in downlaoder. QB64 cannot use SSL as far as i have seen, so...
        PRINT "  Downloading file..."
        f = DownloadFile(MID$(URI$, 7), 10)
        IF f = "" THEN
            PRINT "Download failed."
            EXIT SUB
        END IF

    ELSE '...for https we need to use curl/wget.
        PRINT "  Downloading file..."
        PRINT "    curl '" + URI$ + "' -o /tmp/upm/pkg"
        SHELL "curl '" + URI$ + "' -o /tmp/upm/pkg"

        IF _FILEEXISTS("/tmp/upm/pkg") THEN
            PRINT "    Loading file..."
            f = LoadFile("/tmp/upm/pkg")
            'PRINT "done"
            PRINT "    Removing file..."
            SHELL "rm -f /tmp/upm/pkg"
            'PRINT "done"
            'PRINT
        ELSE
            PRINT "  File could not be downloaded!"
            EXIT SUB
        END IF
    END IF
    GetURI$ = f
END SUB

