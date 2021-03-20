$NOPREFIX
$RESIZE:SMOOTH
'QB64 1.4 doesn't have VERSION, but 1.5 does, so in 1.4 VERSION = 0
'this catches people using 1.5 and compiles using SCREEN instead of $CONSOLE
$LET TRUE = -1
$LET FALSE = 0
'Uncomment this to force a value
'$LET USECONSOLE = TRUE

$IF USECONSOLE THEN
$ELSE
    $IF VERSION = 1.5 THEN
        $LET USECONSOLE = FALSE
    $ELSE
        $LET USECONSOLE = TRUE
    $END IF
$END IF
$IF USECONSOLE = FALSE THEN
    SCREEN NEWIMAGE(640, 480, 256)
    FONT 8
$ELSE
    $CONSOLE:ONLY
    DEST CONSOLE
$END IF

DIM SHARED NL AS STRING * 1
NL = CHR$(10)
TYPE UPMType
    Name AS STRING
    Dev AS STRING
    Desc AS STRING
    Date AS STRING
    Ver AS STRING
    VerNo AS FLOAT
    Compressed AS BYTE
END TYPE

TYPE FileType
    Name AS STRING
    Len AS UNSIGNED LONG
END TYPE
