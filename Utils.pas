UNIT Utils;

INTERFACE

USES
  System.Classes, System.UITypes,
  System.SyncObjs,
  System.IOUtils,
  System.Win.ComObj,
  System.Win.Registry,
  Winapi.Windows,
  Winapi.ShlObj,
  Winapi.ShellAPI,
  System.SysUtils,
  FMX.Forms,
  FMX.DialogService;

TYPE
  TArrayOfStrings = array of String;
  ArrayOfStrings = TArrayOfStrings;
  TArrayOfUnicodeStrings = array of UnicodeString;
  TArrayOfReal = array of real;
  TArrayofInteger = array of Integer;
  TArrayofLongWord = array of Longword;
  TArrayofObjects = array of TObject;
  TTwoDArrayofInteger = array of TArrayofInteger;
  TTwoDArrayOfString = array of ArrayOfStrings;

  StrCodeInfoRec = record
    CodePage: Word;
    ElementLength: Word;
    RefCount: Integer;
    Length: Integer;
  end;

function  MyMessageDialog(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons; const ADefaultButton: TMsgDlgBtn): Integer;
function  GetArrayFromString(const S: String; SepVal: Char; ARemoveQuote: Boolean = false; ATrim: Boolean = True; ADropNulls: Boolean = false): TArrayOfStrings; overload;
function  FieldSep(var ss: string; SepVal: Char): String; overload;
function  ReadLineFrmStream(AStream: TStream): String;
function  PosNoCase(const ASubstr: String; AFullString: String): Integer; overload;
procedure PopulateStringsFromArray(AStrings: TStrings; AArray: TArrayOfStrings; AObjArray: TArrayofObjects = nil);

//function  ShellExecuteDocument(const Command, Parameters, Directory: String; Visiblity: DWord = SW_RESTORE; Action: String = 'open'): Boolean;
function ExecuteShell(CONST ExeFile: string; Params: string= ''; ShowErrorMsg: Boolean= TRUE; WindowState: Integer= WinApi.Windows.SW_SHOWNORMAL): Boolean;
procedure ExecuteURL(URL: string);

const
  NullStrCodeInfo: StrCodeInfoRec = (CodePage: 0; ElementLength: 0; RefCount: 0; Length: 0);

type
  PStrCodeInfoRec = ^StrCodeInfoRec;

CONST
  ZSISOffset     = 0;
  CRP            = AnsiChar(#141); // (13 + 128);
  LFP            = AnsiChar(#138); // (10 + 128);
//  FirstStrCharNo = 1;

IMPLEMENTATION
USES ccCore;



function MyMessageDialog(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons; const ADefaultButton: TMsgDlgBtn): Integer;
var mr: TModalResult;
begin
  mr := mrNone;

  TDialogService.MessageDialog(
      AMessage,
      ADialogType,
      AButtons,
      ADefaultButton,
      0,
      procedure (const AResult: TModalResult)
      begin
      mr := AResult;
      end);

  while mr = mrNone do // wait for modal result
    Application.ProcessMessages;

  Result := mr;
end;


function GetArrayFromString(const S: String; SepVal: Char; ARemoveQuote: Boolean = false; ATrim: Boolean = True; ADropNulls: Boolean = false): TArrayOfStrings;
var
  i, StartPos: Integer;
  QuoteVal: Char;
  InQuotes: Boolean;
  ThisS, fs: String;
begin
  SetLength(Result, 0);
  if S = '' then
    Exit;

  ThisS := S;
  i := 1;
  StartPos := 1;
  InQuotes := False;
  QuoteVal := #0;

  while i <= Length(ThisS) do
  begin
    if InQuotes then
    begin
      if ThisS[i] = QuoteVal then
      begin
        InQuotes := False;
        QuoteVal := #0;
      end;
    end
    else
    begin
      if ARemoveQuote and CharInSet(ThisS[i], ['''', '"', '[', '{', '(', '<']) then
      begin
        InQuotes := True;
        case ThisS[i] of
          '''', '"': QuoteVal := ThisS[i];
          '[': QuoteVal := ']';
          '{': QuoteVal := '}';
          '(': QuoteVal := ')';
          '<': QuoteVal := '>';
        end;
      end
      else if ThisS[i] = SepVal then
      begin
        fs := Copy(ThisS, StartPos, i - StartPos);
        if ATrim then
          fs := Trim(fs);
        if not (ADropNulls and (fs = '')) then
        begin
          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := fs;
        end;
        StartPos := i + 1;
      end;
    end;
    Inc(i);
  end;

  fs := Copy(ThisS, StartPos, Length(ThisS) - StartPos + 1);
  if ATrim then
    fs := Trim(fs);
  if not (ADropNulls and (fs = '')) then
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := fs;
  end;
end;


function FieldSep(var ss: String; SepVal: Char): String;
var
  StartPos, EndPos: Integer;
begin
  if ss = '' then
    Exit('');

  // Find the start position of the first non-separator character
  StartPos := 1;
  while (StartPos <= Length(ss)) and (ss[StartPos] = SepVal) do
    Inc(StartPos);

  // Find the position of the separator character or the end of the string
  EndPos := StartPos;
  while (EndPos <= Length(ss)) and (ss[EndPos] <> SepVal) do
    Inc(EndPos);

  // Extract the substring
  Result := Copy(ss, StartPos, EndPos - StartPos);

  // Update the input string to exclude the processed part
  if EndPos <= Length(ss) then
    ss := Copy(ss, EndPos + 1, Length(ss) - EndPos)
  else
    ss := '';
end;


 
function ReadLineFrmStream(AStream: TStream): String;
var
  CurPos, EndPos: int64;
  i, EndSZ: Integer;
  Nxt: Char;
begin
  CurPos := AStream.Position;
  EndPos := AStream.Seek(0, soEnd);
  AStream.Seek(CurPos, soBeginning);

  if 256 > EndPos - CurPos then
    EndSZ := Word(EndPos - CurPos)
  else
    EndSZ := 256; // Max Line Size
  SetLength(Result, EndSZ);
  if EndSZ < 1 then
    exit;

  i := 0;
  AStream.Read(Nxt, 1);
  while not CharInSet(Nxt, [CR, LF, CRP, LFP]) and (i < EndSZ) do
  try
    inc(i);
    Result[i] := Nxt;
    AStream.Read(Nxt, 1);
  except
    Nxt := CR;
  end;
  SetLength(Result, i);
  while CharInSet(Nxt, [CR, LF, CRP, LFP]) and (AStream.Position < EndPos) do
    AStream.Read(Nxt, 1);
  CurPos := AStream.Position;
  if CurPos < EndPos then
    AStream.Seek(CurPos - 1, soBeginning);
end;


function PosNoCase(const ASubstr: String; AFullString: String): Integer;
var
  Substr: String;
  S: String;
begin
  if (ASubstr = '') or (AFullString = '') then
  begin
    Result := -1;
    exit;
  end;
  Substr := Lowercase(ASubstr);
  S := Lowercase(AFullString);
  Result := Pos(Substr, S);
end;

procedure PopulateStringsFromArray(AStrings: TStrings; AArray: TArrayOfStrings; AObjArray: TArrayofObjects);
var
  i, ObjMx: Integer;
begin
  if AStrings = nil then
    raise Exception.Create('PopulateStringsFromArray');
  AStrings.Clear;
  if AObjArray = nil then
    ObjMx := -1
  else
    ObjMx := High(AObjArray);
  for i := 0 to High(AArray) do
    if i > ObjMx then
      AStrings.Add(AArray[i])
    else
      AStrings.AddObject(AArray[i], AObjArray[i]);
end;





function ExecuteShell(CONST ExeFile: string; Params: string= ''; ShowErrorMsg: Boolean= TRUE; WindowState: Integer= WinApi.Windows.SW_SHOWNORMAL): Boolean;
VAR
   i: integer;
   WorkingFolder, Msg: string;
begin
 WorkingFolder:= ExtractFilePath(ExeFile);
 i:= ShellExecute(0, 'open', PChar(ExeFile), Pointer(Params), PChar(WorkingFolder), WindowState);   //  See this about using 'Pointer' instead of 'PChar': http://stackoverflow.com/questions/3048188/shellexecute-not-working-from-ide-but-works-otherwise
 Result:= i > 32;
 if NOT Result AND ShowErrorMsg then
  begin
   case i of
      // What are these?
      0  : Msg:= 'The operating system is out of memory or resources.';
      12 : Msg:= 'Application was designed for a different operating system.';
      13 : Msg:= 'Application was designed for MS-DOS 4.0';
      15 : Msg:= 'Attempt to load a real-mode program.';
      16 : Msg:= 'Attempt to load a second instance of an application with non-readonly data segments.';
      19 : Msg:= 'Attempt to load a compressed application file.';
      20 : Msg:= 'Dynamic-link library (DLL) file failure.';

      // Regular WinExec codes
      { 02} SE_ERR_FNF            : Msg:= 'Exe file not found!'+ ExeFile;
      { 03} SE_ERR_PNF            : Msg:= 'Path not found!';
      { 08} SE_ERR_OOM            : Msg:= 'Out of memory!';

      // Error values for ShellExecute beyond the regular WinExec() codes
      { 26} SE_ERR_SHARE          : Msg:= 'A sharing violation occurred!';
      { 27} SE_ERR_ASSOCINCOMPLETE: Msg:= 'The file name association is incomplete or invalid!';
      { 28} SE_ERR_DDETIMEOUT     : Msg:= 'The DDE transaction could not be completed because the request timed out!';
      { 29} SE_ERR_DDEFAIL        : Msg:= 'The DDE transaction failed!';
      { 30} SE_ERR_DDEBUSY        : Msg:= 'The DDE transaction could not be completed because other DDE transactions were being processed!';
      { 31} SE_ERR_NOASSOC        : Msg:= 'There is no application associated with the given file name extension!';

      { 05} SE_ERR_ACCESSDENIED   : Msg:= 'The operating system denied access! Do you have admin rights?';       // https://answers.microsoft.com/en-us/windows/forum/windows_7-windows_programs/getting-error-shellexecuteex-failed-code-5-access/3af7bea3-5733-426c-9e12-6ec68bf7b38b?auth=1
      { 32} SE_ERR_DLLNOTFOUND    : Msg:= 'The specified DLL was not found!'
     else
        Msg:= 'ShellExecute error '+ IntToStr(i);
   end;

   //MesajError(Msg);
  end;
end;


procedure ExecuteURL(URL: string);
begin
  {ToDo 1: encode all other special URL chars}
  URL := StringReplace(URL, '"', '%22', [rfReplaceAll]);
  ExecuteShell(URL, '', TRUE, SW_SHOWNORMAL);
end;



end.
