'$INCLUDE:'./include/head.bi'
PRINT "############### Universal Package Manager #################"
PRINT "#      v1 -- (c) 2021 all-other-usernames-were-taken      #"
PRINT "#  https://github.com/all-other-usernames-were-taken/upm  #"
PRINT "#"; RandomMsg$(57); "#"
PRINT "###########################################################"
InitialCWD$ = CWD$

SELECT CASE COMMAND$(1)
    CASE "install-pkg"
        install COMMAND$(2)

    CASE "install"
        ver$ = "latest"
        repo$ = "https://raw.githubusercontent.com/all-other-usernames-were-taken/UPM/main/pkg/"
        FOR i% = 3 TO COMMANDCOUNT
            SELECT CASE COMMAND$(i%)
                CASE "-V"
                    i% = i% + 1
                    ver$ = COMMAND$(i%)

                CASE "-R"
                    i% = i% + 1
                    repo$ = COMMAND$(i%)
            END SELECT
        NEXT
        install repo$ + COMMAND$(2) + "/" + ver$ + ".upm"

    CASE "gen-ui"
        RunGenUI

    CASE "dump"
        DumpUPM COMMAND$(2)

    CASE "ls"
        PRINT COMMAND$(2)
        DIM UPM AS UPMType, File(MaxFiles) AS FileType
        CALL ParseUPM(GetURI(COMMAND$(2)), UPM, File(), u~&&)
        u~&& = 0
        DO
            PRINT USING "###### &"; File(u~&&).Len, File(u~&&).Name
            u~&& = u~&& + 1
        LOOP UNTIL File(u~&&).Name = ""

    CASE "gen"
        RunGenCMD

    CASE "help", "?", "--help", "-h", ""
        RunHelp
    CASE ELSE
        PRINT "Invalid command '" + COMMAND$(1) + "'. Use 'upm help' for valid commands."
END SELECT
SYSTEM
'$INCLUDE:'./include/help.bi'
'$INCLUDE:'./include/utils.bi'

SUB RunGenUI
    DIM UPM AS UPMType
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
                        CASE ASC("a") TO ASC("z"), ASC("-"), ASC("_"), ASC("."), ASC("0") TO ASC("9")
                        CASE ELSE
                            PRINT "Error: Package name cannot have control characters!"
                            PRINT ASC(k$)
                            INPUT "Press any key...", k$
                            GOTO ContLoop
                    END SELECT
                NEXT
                UPM.Name = k$

                'CASE "d"
            CASE "e"
                INPUT "Developer Name: ", UPM.Dev

            CASE "v"
                INPUT "Version: ", UPM.Ver

            CASE "r"
                INPUT "Version Number: ", UPM.VerNo

            CASE "g"
                GenerateUPM UPM, UPM.Name + ".upm", upmdir$

            CASE "u"
                UPM.Compressed = (UPM.Compressed = 0) 'If equal to 0, set to -1, otherwise set to 0

            CASE "f"
                INPUT "Grab files from: ", k$
                IF NOT DIREXISTS(k$) THEN
                    PRINT "Error: Directory does not exist!"
                    INPUT "Press any key...", k$
                    GOTO ContLoop
                END IF
                upmdir$ = k$
                'hasi.sh` = FILEEXISTS(k$ + "/i.sh") OR FILEEXISTS(k$ + "i.sh")
                'hasi.command` = FILEEXISTS(k$ + "/i.command") OR FILEEXISTS(k$ + "i.command")
                'hasi.bat` = FILEEXISTS(k$ + "/i.bat") OR FILEEXISTS(k$ + "i.bat")
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
    PRINT "(U)se compression: "; UPM.Compressed
    PRINT ""
    'PRINT "Supported OSs: ";
    'IF hasi.sh` THEN PRINT "Linux ";
    'IF hasi.bat` THEN PRINT "Windows ";
    'IF hasi.command` THEN PRINT "MacOS";
    PRINT
    PRINT
    PRINT "(G)enerate!"
    RETURN
END SUB


SUB RunGenCMD
    DIM UPM AS UPMType
    UPM.Name = "untitled-program"
    UPM.Dev = "NA"
    UPM.Desc = "NA"
    UPM.Date = DATE$
    UPM.Ver = "NA"
    UPM.VerNo = 0
END SUB

SUB install (UPMURI$)
    DIM f AS STRING
    PRINT "Installing package..."
    IF DIREXISTS("/tmp/upm") THEN
        PRINT "  Removing /tmp/upm..."
        SHELL "rm -rf /tmp/upm"
        'PRINT "done"
        PRINT
    END IF

    MKDIR "/tmp/upm/"

    f = GetURI(UPMURI$)

    PRINT "  Reading package info..."
    DIM UPM AS UPMType, File(MaxFiles) AS FileType
    CALL ParseUPM(f, UPM, File(), ofs~&&)

    PRINT "  Unpacking files..."
    DIM n AS UNSIGNED LONG
    DIM UPMPos AS UNSIGNED INTEGER64
    UPMPos = ofs~&&
    n = 0
    DO

        PRINT "    "; File(n).Name; "..."
        IF RIGHT$(File(n).Name, 1) = "/" THEN
            SHELL "mkdir /tmp/upm/" + File(n).Name
            n = n + 1
            CONTINUE
        END IF

        f% = FREEFILE
        OPEN "/tmp/upm/" + File(n).Name FOR OUTPUT AS #f%
        IF UPM.Compressed THEN
            PRINT #f%, INFLATE$(MID$(f, UPMPos, File(n).Len));
        ELSE
            PRINT #f%, MID$(f, UPMPos, File(n).Len);
        END IF

        CLOSE #f%
        UPMPos = UPMPos + File(n).Len
        n = n + 1
        'PRINT "done"

    LOOP UNTIL File(n).Name = ""
    PRINT "  Files sucessfully unpacked."
    PRINT


    IF FILEEXISTS("/tmp/upm/i" + ShExt) THEN
        PRINT "  Installing via external installer..."
        IF Win THEN
            SHELL "exec " + CHR$(34) + tmpdir$ + "\i" + ShExt + CHR$(34) 'AUGH why does batch have to be so stupid. Why can't windows just use bash or zsh?
        ELSEIF Linux THEN
            SHELL "/bin/sh '" + tmpdir$ + "/i" + ShExt + "'"
        ELSEIF Mac THEN
            'idk how to access bash in mac
        END IF
        PRINT "  Installation script done."
        PRINT
    ELSE
        PRINT "  Installation script not found, copying all files to CWD..."
        SHELL "cp -r /tmp/upm/* ."
        'PRINT "done"
        'PRINT
    END IF

    PRINT "  Cleaning up..."
    SHELL "rm -rf /tmp/upm"
    'SHELL "/tmp/upm/c.sh"
    'PRINT "done"
    'PRINT

    PRINT "Installation completed sucessfully!"
END SUB

'taken from QB64-Themes
SUB ParseUPM (UPMData$, UPM AS UPMType, File() AS FileType, UPMEnd AS UNSIGNED INTEGER64) 'Based on the SCS loader for stupidc
    DIM NextLine_Start AS UNSIGNED INTEGER64
    DIM NextLine_End AS UNSIGNED INTEGER64
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
                    UPM.Desc = UPM.Desc + Ln$ + NL
                LOOP


            CASE "files"
                DIM n AS UNSIGNED LONG
                DO
                    GOSUB NewLine
                    IF Ln$ = "" THEN n = n + 1: CONTINUE
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
                    CASE "compressed"
                        UPM.Compressed = VAL(Val$)
                    CASE "shortdesc"
                        UPM.ShortDesc = Val$
                END SELECT
        END SELECT
    LOOP
    UPMEnd = NextLine_Start + 1
    EXIT SUB


    NewLine:

    NextLine_Start = NextLine_End + 1
    NextLine_End = INSTR(NextLine_Start, UPMData$, NL)

    IF NextLine_End = 0 THEN Ln$ = MID$(UPMData$, NextLine_Start): RETURN

    Ln$ = LTRIM$(RTRIM$(MID$(UPMData$, NextLine_Start, NextLine_End - NextLine_Start)))
    RETURN


    Separate:
    I% = INSTR(Ln$, " ")
    IF I% = 0 THEN RETURN
    Key$ = LTRIM$(RTRIM$(LEFT$(Ln$, I%)))
    Val$ = LTRIM$(RTRIM$(MID$(Ln$, I% + 1)))
    SELECT CASE UCASE$(Val$)
        CASE "NA": Val$ = ""
        CASE "TRUE": Val$ = "-1"
        CASE "FALSE": Val$ = "0"
    END SELECT
    RETURN
END SUB


SUB GenerateUPM (UPM AS UPMType, Filename$, FileDir$)
    PRINT "Generating UPM file..."
    SHELL "rm -rf /tmp/upm"
    SHELL "mkdir /tmp/upm"

    DIM TmpFile AS INTEGER
    TmpFile = FREEFILE: OPEN "/tmp/upm/tmp" FOR OUTPUT AS #TmpFile

    PRINT "  Generating header..."
    head$ =                                                 _
    "name " + UPM.Name + NL                                 _
    + "dev " + UPM.Dev + NL                                 _
    + "date " + UPM.Date + NL                               _
    + "ver " + UPM.Ver + NL                                 _
    + "verno " + STR$(UPM.VerNo) + NL                       _
    + "compressed " + STR$(UPM.Compressed) + NL             _
    + "desc" + NL + UPM.Desc + NL + "." + NL                _
    + "files"

    'PRINT "done"
    PRINT "  Getting directory listing..."
    CHDIR FileDir$
    SHELL "ls -R -1 -p --color=never -L --quoting-style=literal --group-directories-first > /tmp/upm/dir.txt"
    DIM dir AS INTEGER
    dir = FREEFILE: OPEN "/tmp/upm/dir.txt" FOR INPUT AS #dir
    'PRINT "done"

    PRINT "  Adding files..."
    DO

        LINE INPUT #dir, s$
        s$ = LTRIM$(RTRIM$(s$))
        IF s$ = "" THEN CONTINUE
        IF s$ = ".:" THEN curdir$ = "./": CONTINUE

        SELECT CASE RIGHT$(s$, 1)
            CASE ":"
                curdir$ = LEFT$(s$, LEN(s$) - 1) + "/"
                CONTINUE

            CASE "/"
                head$ = head$ + NL + "/ " + s$
                CONTINUE
        END SELECT

        s$ = curdir$ + s$
        PRINT "    "; s$; "..."

        IF UPM.Compressed THEN
            f$ = DEFLATE$(LoadFile(s$))
        ELSE
            f$ = LoadFile(s$)
        END IF

        head$ = head$ + NL + LTRIM$(STR$(LEN(f$))) + " " + s$

        PRINT #TmpFile, f$;

    LOOP UNTIL EOF(dir)
    PRINT "  Files added sucessfully."

    CLOSE TmpFile
    CLOSE dir
    f$ = ""

    PRINT "  Saving file..."

    DIM outp AS INTEGER
    outp = FREEFILE: OPEN Filename$ FOR OUTPUT AS #outp
    PRINT #outp, head$ + NL + "." + NL + ".";
    PRINT #outp, LoadFile("/tmp/upm/tmp");
    CLOSE outp

    'PRINT "done"

    PRINT "UPM file generated sucessfully! Output is at: "; Filename$
END SUB

SUB DumpUPM (UPMURI$)
    DIM f AS STRING, UPM AS UPMType, File(MaxFiles) AS FileType
    f = GetURI(UPMURI$)
    CALL ParseUPM(f, UPM, File(), e~&&)

    PRINT UPMURI$ + ":"
    PRINT "  Package:     " + UPM.Name
    PRINT "  Developer:   " + UPM.Dev
    PRINT "  Version:     " + UPM.Ver + " (" + LTRIM$(STR$(UPM.VerNo)) + ")"
    PRINT "  Date:        " + UPM.Date
    PRINT "  Origin:      " + UPM.Origin
    PRINT "  Dependencies:" + UPM.Depends
    PRINT "  " + Toggle(UPM.Compressed) + " Compressed files" 'Dunno why _TOGGLE is syntaxed as a statement (like it should be) but does not throw a name in use error...
    PRINT "  " + Toggle((LEN(UPM.Origin) <> 0)) + " From repository"
    PRINT "  Short description:"
    PRINT "    " + UPM.ShortDesc
    PRINT "  Long description:"
    DO
        GOSUB NewLine
        PRINT "    " + Ln$
    LOOP WHILE NextLine_End%
    EXIT SUB

    NewLine:

    NextLine_Start% = NextLine_End% + 1
    NextLine_End% = INSTR(NextLine_Start%, UPM.Desc, NL)

    IF NextLine_End% = 0 THEN Ln$ = MID$(UPM.Desc, NextLine_Start%): RETURN

    Ln$ = LTRIM$(RTRIM$(MID$(UPM.Desc, NextLine_Start%, NextLine_End% - NextLine_Start%)))
    RETURN
END SUB
