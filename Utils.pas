﻿UNIT Utils;

{=============================================================================================================
   Gabriel Moraru
   2025.04
   Convert VCL to FMX
--------------------------------------------------------------------------------------------------------------
   This program requires https://github.com/GabrielOnDelphi/Delphi-LightSaber
=============================================================================================================}

{ Fork from FMX2VCL - Eduardo Rodrigues 2019 }

INTERFACE

USES
  System.Classes, System.UITypes, System.SysUtils,
  FMX.Forms, FMX.DialogService;

TYPE
  TArrayOfStrings        = array of String;
  ArrayOfStrings         = TArrayOfStrings;
  TArrayOfUnicodeStrings = array of UnicodeString;
  TArrayOfReal           = array of Real;
  TArrayOfInteger        = array of Integer;
  TArrayOfLongWord       = array of LongWord;
  TArrayOfObjects        = array of TObject;
  TTwoDArrayOfInteger    = array of TArrayOfInteger;
  TTwoDArrayOfString     = array of ArrayOfStrings;

  StrCodeInfoRec = record
    CodePage: Word;
    ElementLength: Word;
    RefCount: Integer;
    Length: Integer;
  end;

  PStrCodeInfoRec = ^StrCodeInfoRec;

CONST
  NullStrCodeInfo: StrCodeInfoRec = (CodePage: 0; ElementLength: 0; RefCount: 0; Length: 0);
  ZSISOffset = 0;
  CRP = AnsiChar(#141); // Carriage return + 128
  LFP = AnsiChar(#138); // Line feed + 128


// Parses a string into an array of strings based on a separator character
function GetArrayFromString(const inputString: String; separator: Char; removeQuotes: Boolean = False; trimValues: Boolean = True; dropEmpty: Boolean = False): TArrayOfStrings;

// Extracts the first field from a string based on a separator and updates the source string
function FieldSep(var sourceString: String; separator: Char): String;

// Populates a TStrings object from an array of strings, optionally with associated objects
procedure PopulateStringsFromArray(targetStrings: TStrings; sourceArray: TArrayOfStrings; objectArray: TArrayOfObjects = nil);



IMPLEMENTATION

USES ccCore;


function GetArrayFromString(const inputString: String; separator: Char; removeQuotes: Boolean = False; trimValues: Boolean = True; dropEmpty: Boolean = False): TArrayOfStrings;
var
  index, startPosition: Integer;
  quoteChar: Char;
  inQuotes: Boolean;
  currentString, fieldValue: String;
begin
  SetLength(Result, 0);
  if inputString = '' then
    Exit;

  currentString := inputString;
  index := 1;
  startPosition := 1;
  inQuotes := False;
  quoteChar := #0;

  while index <= Length(currentString) do
  begin
    if inQuotes then
    begin
      if currentString[index] = quoteChar then
      begin
        inQuotes := False;
        quoteChar := #0;
      end;
    end
    else
    begin
      if removeQuotes and CharInSet(currentString[index], ['''', '"', '[', '{', '(', '<']) then
      begin
        inQuotes := True;
        case currentString[index] of
          '''', '"': quoteChar := currentString[index];
          '[': quoteChar := ']';
          '{': quoteChar := '}';
          '(': quoteChar := ')';
          '<': quoteChar := '>';
        end;
      end
      else if currentString[index] = separator then
      begin
        fieldValue := Copy(currentString, startPosition, index - startPosition);
        if trimValues then
          fieldValue := Trim(fieldValue);
        if not (dropEmpty and (fieldValue = '')) then
        begin
          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := fieldValue;
        end;
        startPosition := index + 1;
      end;
    end;
    Inc(index);
  end;

  // Handle the last field
  fieldValue := Copy(currentString, startPosition, Length(currentString) - startPosition + 1);
  if trimValues then
    fieldValue := Trim(fieldValue);
  if not (dropEmpty and (fieldValue = '')) then
  begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := fieldValue;
  end;
end;


function FieldSep(var sourceString: String; separator: Char): String;
var
  startPosition, endPosition: Integer;
begin
  if sourceString = '' then Exit('');

  // Find the start position of the first non-separator character
  startPosition := 1;
  while (startPosition <= Length(sourceString)) and (sourceString[startPosition] = separator) do
    Inc(startPosition);

  // Find the position of the separator character or the end of the string
  endPosition := startPosition;
  while (endPosition <= Length(sourceString)) and (sourceString[endPosition] <> separator) do
    Inc(endPosition);

  // Extract the substring
  Result := Copy(sourceString, startPosition, endPosition - startPosition);

  // Update the input string to exclude the processed part
  if endPosition <= Length(sourceString)
  then sourceString := Copy(sourceString, endPosition + 1, Length(sourceString) - endPosition)
  else sourceString := '';
end;


// Transfers data from an array of strings to a TStringList
procedure PopulateStringsFromArray(targetStrings: TStrings; sourceArray: TArrayOfStrings; objectArray: TArrayOfObjects = nil);
var
  index, maxObjectIndex: Integer;
begin
  Assert(targetStrings <> nil, 'PopulateStringsFromArray: targetStrings cannot be nil!');
  targetStrings.Clear;

  if objectArray = nil
  then maxObjectIndex := -1
  else maxObjectIndex := High(objectArray);

  for index := 0 to High(sourceArray) do
    if index > maxObjectIndex
    then targetStrings.Add(sourceArray[index])
    else targetStrings.AddObject(sourceArray[index], objectArray[index]);
end;


end.





