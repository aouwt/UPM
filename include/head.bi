$NOPREFIX
$RESIZE:SMOOTH
$LET TRUE = -1
$LET FALSE = 0
CONST True = -1
CONST False = 0


'Choose the OS
$IF WIN THEN
    CONST Win = -1
    CONST Linux = 0
    CONST Mac = 0
    CONST ShExt = ".bat"
$ELSEIF LINUX THEN
    CONST Win = 0
    CONST Linux = -1
    CONST Mac = 0
    CONST ShExt = ".sh"
$ELSEIF MACOS THEN
    'CONST Win = 0
    'CONST Linux = 0
    'CONST Mac = -1
    'CONST ShExt = ".command"
    $error MacOS support is not integrated yet!
    'i dont have a Mac so i cant test it...
$ELSE
    'This literally should _never_ happen (since QB64 has only a predefined set of values for determining the OS) but who knows
    $ERROR Could not determine OS type!
$END IF



'For x64 only packages
'I don't think there's an easy way to detect AMD or ARM archetecture in QB64....
$IF 64BIT THEN
    CONST x64 = -1
    CONST x86 = 0
$ELSEIF 32BIT THEN
    CONST x64 = 0
    CONST x86 = -1
$ELSE
    'This, also, should never happen, but you never know.
    $ERROR Could not determine CPU archetecture!
$END IF



DIM SHARED NL AS STRING * 1
NL = CHR$(10)
TYPE UPMType
    Name AS STRING
    Dev AS STRING
    Desc AS STRING
    ShortDesc AS STRING
    Date AS STRING
    Ver AS STRING
    VerNo AS FLOAT
    Origin AS STRING
    Depends AS STRING
    Compressed AS BYTE
END TYPE

TYPE FileType
    Name AS STRING
    Len AS UNSIGNED LONG
END TYPE

CONST MaxFiles = 1000000
