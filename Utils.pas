UNIT Utils;

INTERFACE

USES
  System.Classes, System.UITypes,







  System.SysUtils,
  FMX.Forms,
  FMX.DialogService;

TYPE
  TArrayOfStrings = array of String;
  ArrayOfStrings  = TArrayOfStrings;
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
  if ATrim then fs := Trim(fs);
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

  if 256 > EndPos - CurPos
  then
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
  assert(AStrings <> nil, 'PopulateStringsFromArray');

  AStrings.Clear;
  if AObjArray = nil
  then ObjMx := -1
  else ObjMx := High(AObjArray);

  for i := 0 to High(AArray) do
    if i > ObjMx
    then AStrings.Add(AArray[i])
    else AStrings.AddObject(AArray[i], AObjArray[i]);
end;


end.
