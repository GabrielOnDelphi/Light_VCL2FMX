program Light_VCL2FMX_CLI;

{=============================================================================================================
   Gabriel Moraru
   2026.05.14
   Convert VCL to FMX — CLI batch wrapper

   Sibling DPR to Light_VCL2FMX (GUI). Re-uses Parser.TParser; no FMX.Forms / no GUI.

   Usage:
     Light_VCL2FMX_CLI.exe --dict <ini> --in <dfm-or-dir> [--out <dir>] [--recurse]

     --dict   Path to ConversionDict.ini.
     --in     Either a single .dfm file or a directory containing .dfm files.
              If a .pas is given it is normalised to .dfm (same as GUI).
     --out    Optional output directory. If omitted, outputs are written next
              to each input (same behaviour as GUI Save).
     --recurse  When --in is a directory, descend into subdirectories.
                NOTE: With --out set, outputs are flattened into the same
                directory — duplicate filenames from different subdirs will
                overwrite each other. Fine for cases where filenames are unique.

   Exit codes:
     0  All conversions succeeded.
     1  At least one conversion failed (other conversions in the same run
        still ran — failure is reported but does not abort the batch).
     2  Argument error (bad/missing flag, file not found, etc.).

   Output naming matches the GUI: <input>_FMX.fmx + <input>_FMX.pas
   (input filename appended with "_FMX" via LightCore.IO.AppendToFileName).
=============================================================================================================}

{$APPTYPE CONSOLE}

USES
  FastMM4,
  System.SysUtils,
  System.Types,
  System.Classes,
  System.IOUtils,
  Parser            in 'Parser.pas',
  Utils             in 'Utils.pas',
  ParseImage        in 'ParseImage.pas',
  ParseImageList    in 'ParseImageList.pas',
  LightCore,
  LightCore.IO,
  LightCore.TextFile;


VAR
  ArgDict   : string = '';
  ArgIn     : string = '';
  ArgOut    : string = '';
  ArgRecurse: Boolean = FALSE;

  Converted : Integer = 0;
  Skipped   : Integer = 0;
  Failed    : Integer = 0;


procedure PrintUsage;
begin
  Writeln('Light VCL2FMX — CLI batch converter');
  Writeln('');
  Writeln('Usage:');
  Writeln('  Light_VCL2FMX_CLI.exe --dict <ini> --in <dfm-or-dir> [--out <dir>] [--recurse]');
  Writeln('');
  Writeln('  --dict     Path to ConversionDict.ini');
  Writeln('  --in       Single .dfm file or directory containing .dfm files');
  Writeln('  --out      Optional output directory (default: alongside input)');
  Writeln('  --recurse  Descend into subdirectories when --in is a directory');
  Writeln('');
  Writeln('Exit codes: 0=all ok, 1=conversion failure(s), 2=argument error.');
end;


function ParseArgs: Boolean;
VAR
  i: Integer;
  Arg: string;
begin
  Result:= TRUE;
  i:= 1;
  WHILE i <= ParamCount DO
   begin
     Arg:= ParamStr(i);
     { Normalise forward slashes to backslashes — Delphi's ExtractFileName /
       ExtractFilePath only recognise the platform path delimiter (Windows = '\').
       A user pasting a bash/cygwin-style path with '/' otherwise yields
       garbage filenames. }
     if      SameText(Arg, '--dict')    then begin Inc(i); ArgDict := StringReplace(ParamStr(i), '/', '\', [rfReplaceAll]); end
     else if SameText(Arg, '--in')      then begin Inc(i); ArgIn   := StringReplace(ParamStr(i), '/', '\', [rfReplaceAll]); end
     else if SameText(Arg, '--out')     then begin Inc(i); ArgOut  := StringReplace(ParamStr(i), '/', '\', [rfReplaceAll]); end
     else if SameText(Arg, '--recurse') then ArgRecurse:= TRUE
     else if SameText(Arg, '--help')
          OR SameText(Arg, '-h')        then begin PrintUsage; Halt(0); end
     else
       begin
         Writeln('ERROR: unknown argument: ', Arg);
         Result:= FALSE;
         EXIT;
       end;
     Inc(i);
   end;

  if ArgDict = '' then begin Writeln('ERROR: --dict is required.'); Result:= FALSE; end
  else if NOT FileExists(ArgDict) then begin Writeln('ERROR: dictionary not found: ', ArgDict); Result:= FALSE; end;

  if ArgIn = '' then begin Writeln('ERROR: --in is required.'); Result:= FALSE; end
  else if (NOT FileExists(ArgIn)) AND (NOT DirectoryExists(ArgIn))
    then begin Writeln('ERROR: input not found: ', ArgIn); Result:= FALSE; end;

  if (ArgOut <> '') AND (NOT DirectoryExists(ArgOut)) then
    if NOT System.SysUtils.ForceDirectories(ArgOut)
    then begin Writeln('ERROR: cannot create --out directory: ', ArgOut); Result:= FALSE; end;
end;


{ Convert a single DFM file. Mirrors GUI btnProcessClick + btnSaveClick. }
procedure ConvertOne(const InputDfm: string);
VAR
  DfmParser : TParser;
  DfmBody   : TStringList;
  DfmLine   : string;
  LineIndex : Integer;
  P         : Integer;
  InputPas  : string;
  OutputBase: string;     { input filename + '_FMX', minus extension }
  OutputFMX : string;
  OutputPas : string;
begin
  Writeln('Converting: ', InputDfm);

  if NOT FileExists(InputDfm) then
   begin
     Writeln('  SKIPPED — file not found.');
     Inc(Skipped);
     EXIT;
   end;

  if NOT TParser.IsTextDFM(InputDfm) then
   begin
     Writeln('  SKIPPED — binary DFM, not supported.');
     Inc(Skipped);
     EXIT;
   end;

  InputPas:= ChangeFileExt(InputDfm, '.pas');

  { Output filename: <stem>_FMX.fmx / .pas, in ArgOut if set, else next to input. }
  if ArgOut = ''
  then OutputBase:= AppendToFileName(InputPas, '_FMX')
  else OutputBase:= IncludeTrailingPathDelimiter(ArgOut) + AppendToFileName(ExtractFileName(InputPas), '_FMX');
  OutputFMX:= ChangeFileExt(OutputBase, '.fmx');
  OutputPas:= ChangeFileExt(OutputBase, '.pas');

  DfmParser:= NIL;
  DfmBody  := NIL;
  try
    DfmBody:= StringFromFileTSL(InputDfm);
    if DfmBody.Count = 0 then
     begin
       Writeln('  SKIPPED — empty file.');
       Inc(Skipped);
       EXIT;
     end;

    LineIndex:= 0;
    DfmLine:= Trim(DfmBody[LineIndex]);
    P:= PosInsensitive('object', DfmLine);
    if (P <= 0) OR (P >= 10) then
     begin
       Writeln('  SKIPPED — no `object` token on first line.');
       Inc(Skipped);
       EXIT;
     end;

    DfmParser:= TParser.Create(DfmLine, DfmBody, 0, LineIndex);
    DfmParser.LiveBindings;
    DfmParser.LoadConfigFile(ArgDict);

    { WriteFMXToFile internally calls BuildFmxFile, so we don't need to
      build twice. The GUI's separate BuildFmxFile call is purely to populate
      the preview memo. }
    DeleteFile(OutputFMX);
    DeleteFile(OutputPas);
    DfmParser.WriteFMXToFile(OutputFMX);
    DfmParser.WritePasToFile(OutputPas, InputPas);

    Writeln('  -> ', OutputFMX);
    Writeln('  -> ', OutputPas);
    Inc(Converted);
  except
    on E: Exception do
     begin
       Writeln('  FAILED — ', E.ClassName, ': ', E.Message);
       Inc(Failed);
     end;
  end;

  FreeAndNil(DfmParser);
  FreeAndNil(DfmBody);
end;


procedure WalkDirectory(const Dir: string);
VAR
  Files: TStringDynArray;
  SubDirs: TStringDynArray;
  F, Sub, Name: string;
begin
  Files:= TDirectory.GetFiles(Dir, '*.dfm');
  for F in Files do ConvertOne(F);

  if ArgRecurse then
   begin
     SubDirs:= TDirectory.GetDirectories(Dir);
     for Sub in SubDirs do
      begin
        Name:= ExtractFileName(ExcludeTrailingPathDelimiter(Sub));
        { Skip Delphi IDE bookkeeping folders — they hold historical .dfm
          backups (.~1~, .~2~ etc.) that aren't worth re-converting. }
        if (Name = '__history') OR (Name = '__recovery') OR (Name = 'dcu') OR (Name = 'dcu_cli')
        then Continue;
        WalkDirectory(Sub);
      end;
   end;
end;


begin
  ReportMemoryLeaksOnShutdown:= FALSE;

  if NOT ParseArgs then
   begin
     Writeln('');
     PrintUsage;
     Halt(2);
   end;

  try
    if FileExists(ArgIn)
    then ConvertOne(ArgIn)
    else WalkDirectory(ArgIn);
  except
    on E: Exception do
     begin
       Writeln('FATAL: ', E.ClassName, ': ', E.Message);
       Halt(1);
     end;
  end;

  Writeln('');
  Writeln(Format('Done. Converted=%d Skipped=%d Failed=%d', [Converted, Skipped, Failed]));

  if Failed > 0
  then Halt(1)
  else Halt(0);
end.
