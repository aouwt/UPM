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
    DIM msg(34) AS STRING
    msg(0) = "(insert clever joke here)"
    msg(1) = "Totally not a ripoff of dpkg!"
    msg(2) = "rm -rf / --no-preserve-root"
    msg(3) = "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
    msg(4) = "Isn't this the greatest thing ever?": 'no
    msg(1) = ":(){ :|:& };:"
    msg(5) = "yet another package manager nobody needs"
    msg(6) = "Why did the chicken cross the road?"
    msg(7) = "i speel good"
    msg(8) = "[Laughter] - You dumb bitch"
    msg(9) = "computer go brr"
    msg(10) = "  this text is TOTALLY centered!            "
    msg(11) = "HOO HAH TIKI TIKI"
    msg(12) = "reject humanity,return to monke"
    msg(13) = "hmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm"
    msg(14) = "Say hi to Fellippe for me!"
    msg(15) = "10 PRINT 'Hello, World!'  20 GOTO 10"
    msg(16) = "My man Kevin on the ledge and shit"
    msg(17) = "skamtebord"
    msg(18) = "rm -rf /"
    msg(19) = "(Y) S ame"
    msg(20) = "JMP *"
    msg(21) = "ur mom gae"
    msg(22) = "curl > wget. Fight me."
    msg(23) = "hyperfixations go brr"
    msg(24) = "i use arch btw": 'no i dont
    msg(25) = "Dead memes go brr"
    msg(26) = "FFFFFFFFFFFFFFFFUUUUUUUUUUUUUUU..."
    msg(27) = "Free V-Bucks!"
    msg(28) = "Look! It's a thing!"
    msg(29) = "unfinished projects go brr"
    msg(30) = "Hey all, Scott here!"
    msg(31) = "I am out of ideas for what to put here lol."
    msg(32) = "JOKEEFUNNY"
    msg(33) = "I really should be doing homework right now..."
    msg(34) = "penis"

    RANDOMIZE TIMER
    ChooseNew:
    s$ = msg(RND * UBOUND(msg))
    IF LEN(s$) > l% GOTO ChooseNew 'Make sure we aren't using a string that's too long

    RandomMsg$ = SPACE$((l% - LEN(s$)) / 2) + s$
    RandomMsg$ = RandomMsg$ + SPACE$(l% - LEN(RandomMsg$))
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
            EXIT FUNCTION
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
            EXIT FUNCTION
        END IF
    END IF
    GetURI$ = f
END FUNCTION

