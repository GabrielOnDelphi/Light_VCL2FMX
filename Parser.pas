unit Parser;

INTERFACE

USES
  System.Classes,
  System.Types,
  System.SysUtils,
  System.StrUtils,
  Contnrs,
  Winapi.Windows,
  IniFiles,
  FMX.Objects,
  System.Generics.Collections,

  ParseImage,
  ParseImageList,
  ccCore,
  Utils;

TYPE
  TLinkControl = record
    DataSource: String;
    FieldName: String;
    Control: String;
  end;

  TLinkGridColumn = record
    Caption: String;
    FieldName: String;
    Width: String;
  end;

  TLinkGrid = record
    DataSource: String;
    GridControl: String;
    Columns: TArray<TLinkGridColumn>;
  end;

  TParser = class(TObject)
  private
    FLinkControlList: TArray<TLinkControl>;
    FLinkGridList: TArray<TLinkGrid>;
    FDFMClass: String;
    FObjName: String;
    FOwnedObjs: TObjectList<TParser>;
    FOwnedItems: TObjectList<TObject>;
    FDepth: Integer;
    F2DPropertyArray: TTwoDArrayOfString;
    FPropertyArraySz, FPropertyMax: Integer;
    FIniReplaceValues,
    FIniIncludeValues,
    FIniSectionValues,
    FIniAddProperties,
    FUsesTranslation,
    FIniObjectTranslations: TStringList;
    function AddArrayOfItemProperties(APropertyIdx: Integer; APad: String): String;
    function FMXClass: String;
    function FMXProperties(APad: String): String;
    function FMXSubObjects(APad: String): String;
    function GetFMXLiveBindings: String;
    function GetPASLiveBindings: String;
    function IniAddProperties: TStringList;
    function IniIncludeValues: TStringList;
    function IniObjectTranslations: TStringList;
    function IniReplaceValues: TStringList;
    function IniSectionValues: TStringList;
    function OwnedObjs: TObjectList<TParser>;
    function ProcessCodeBody(const ACodeBody: String): String;
    function ProcessUsesString(AOrigUsesArray: TArrayOfStrings): String;
    function PropertyArray(ARow: Integer): TArrayOfStrings;
    function TransformProperty(ACurrentName, ACurrentValue: String; APad: String = ''): String;
    function UsesTranslation: TStringList;
    procedure IniFileLoad(AIni: TIniFile);
    procedure ReadData(Prop: TTwoDArrayOfString; APropertyIdx: Integer; AList: TStringList; var LineIndex: Integer);
    procedure ReadItems(Prop: TTwoDArrayOfString; APropertyIdx: Integer; AList: TStringList; var LineIndex: Integer);
    procedure ReadProperties(AData: String; AList: TStringList; var AIdx: Integer; var LineIndex: Integer);
    procedure ReadText(Prop: TTwoDArrayOfString; APropertyIdx: Integer; AList: TStringList; var LineIndex: Integer);
    procedure UpdateUsesStringList(AUsesList: TStrings);
  public
    constructor Create(ACreateText: String; AList: TStringList; ADepth: Integer; var LineIndex: Integer);
    destructor Destroy; override;
    procedure LoadInFileDefs(AIniFileName: String);
    function  GenPasFile(CONST PascalSourceFileName: String): String;
    function  FMXFile(APad: String = ''): String;
    function  WriteFMXToFile(const AFmxFileName: String): Boolean;
    function  WritePasToFile(const APasOutFileName, APascalSourceFileName: String): Boolean;
    procedure LiveBindings(DfmObject: TObjectList<TParser> = nil);
    class function IsTextDFM(DfmFileName: String): Boolean;
  end;

 { TDfmToFmxListItem = class(TParser)
  private
    FHasMore: Boolean;
    FPropertyIndex: Integer;
    FOwner: TParser;
  public
    constructor Create(AOwner: TParser; APropertyIdx: Integer; AList: TStringList; ADepth: Integer; var LineIndex: Integer);
    property HasMore: Boolean read FHasMore;
  end;   }

IMPLEMENTATION

USES
  ccTextFile, ccStreamBuff, System.IOUtils;

CONST
  ContinueCode: String = '#$Continue$#';



constructor TParser.Create(ACreateText: String; aList: TStringList; ADepth: Integer; var LineIndex: Integer);
var
  i: Integer;
  CurrentLine: String;
  InputArray: TArrayOfStrings;
begin
  inherited Create;
  i := 0;
  FDepth := ADepth;
  FOwnedObjs  := TObjectList<TParser>.Create(True);
  FOwnedItems := TObjectList<TObject>.Create(True);
  if Pos('object', Trim(ACreateText)) = 1 then
  begin
    InputArray := GetArrayFromString(ACreateText, ' ');
    if Length(InputArray) < 3 then
      RAISE Exception.Create('Invalid object declaration: ' + ACreateText);

    FObjName  := InputArray[1];
    FDFMClass := InputArray[2];
    Inc(LineIndex);                // Start reading from the next line
    while (LineIndex < AList.Count) and (Trim(AList[LineIndex]) <> 'end') do
    begin
      CurrentLine := Trim(AList[LineIndex]);
      if Pos('object', CurrentLine) = 1 then
      begin
        // Pass the current LineIndex to the new parser instance and let it manage
        FOwnedObjs.Add(TParser.Create(CurrentLine, AList, FDepth + 1, LineIndex));
        Dec(LineIndex); // Adjust LineIndex after returning from nested parser
      end
      else
      begin
        ReadProperties(CurrentLine, AList, i, LineIndex);
      end;
      Inc(LineIndex);
    end;

    // Ensure to skip the 'end' line if it matches
    if (LineIndex < AList.Count) and (Trim(AList[LineIndex]) = 'end')
    then Inc(LineIndex);
  end
  else
    raise Exception.Create('Bad Start: ' + ACreateText);
  SetLength(F2DPropertyArray, FPropertyMax + 1);
end;



destructor TParser.Destroy;
begin
  FreeAndNil(FOwnedObjs);
  FreeAndNil(FOwnedItems);
  FreeAndNil(FIniReplaceValues);
  FreeAndNil(FIniIncludeValues);
  FreeAndNil(FIniSectionValues);
  FreeAndNil(FUsesTranslation);
  FreeAndNil(FIniAddProperties);
  inherited Destroy;
end;




function TParser.AddArrayOfItemProperties(APropertyIdx: Integer; APad: String): String;
begin
  Result := APad + '  item' + CRLF +
    APad + '  Prop1 = 6' + CRLF +
    APad + '  end>' + CRLF;
  //Tempary patch
end;


class function TParser.IsTextDFM(DfmFileName: String): Boolean;
var
  TestString: String;
begin
  if NOT FileExists(DfmFileName) then EXIT(FALSE);

  TestString:= ccTextFile.StringFromFile(DfmFileName);
  if Length(TestString) <= 20
  then EXIT(FALSE);

  Result:= PosNoCase('object', TestString) > 0;
end;


function TParser.FMXClass: String;
begin
  Result := FDFMClass;
end;


function TParser.FMXFile(APad: String = ''): String;
begin
  Result := APad + 'object ' + FObjName + ': ' + FMXClass + CRLF;
  Result := Result + FMXProperties(APad);
  Result := Result + FMXSubObjects(APad + ' ');
  if APad = EmptyStr
  then Result := Result + GetFMXLiveBindings + CRLF + APad + 'end' + CRLF
  else Result := Result + APad + 'end' + CRLF;
end;


function TParser.FMXProperties(APad: String): String;
var
  i: Integer;
  sProp: String;
begin
  Result := EmptyStr;
  for i := Low(F2DPropertyArray) to High(F2DPropertyArray) do

    if F2DPropertyArray[i, 1] = '<'
    then Result := Result + APad +'  '+ TransformProperty(F2DPropertyArray[i, 0], F2DPropertyArray[i, 1]) + CRLF + AddArrayOfItemProperties(i, APad +'  ') + CRLF
    else
      if F2DPropertyArray[i, 1][1] = '{' then
      begin
        sProp := TransformProperty(F2DPropertyArray[i, 0], F2DPropertyArray[i, 1], APad);
        if not sProp.IsEmpty then
          Result := Result + APad +'  '+ sProp + CRLF;
      end
      else
        if F2DPropertyArray[i, 0] <> EmptyStr then
        begin
          sProp := TransformProperty(F2DPropertyArray[i, 0], F2DPropertyArray[i, 1]);
          if not sProp.IsEmpty then
            Result := Result + APad +'  '+ sProp + CRLF;
        end;


  if IniAddProperties.Count > 0 then
    for i := 0 to Pred(FIniAddProperties.Count) do
      Result := Result + APad +'  '+ StringReplace(FIniAddProperties[i], '=', ' = ', []) + CRLF;
end;


function TParser.FMXSubObjects(APad: String): String;
var
  I: Integer;
begin
  Result := EmptyStr;
  if FOwnedObjs = nil then
    Exit;

  for I := 0 to Pred(FOwnedObjs.Count) do
    if FOwnedObjs[I] is TParser then
      Result := Result + TParser(FOwnedObjs[I]).FMXFile(APad + ' ');
end;


{This function ensures that the Pascal file is correctly modified to include necessary units and live bindings, and it replaces DFM references with FMX references.

    File Reading:
        The function reads the Pascal source file into PreUsesString.
        If the file size is greater than 20 bytes, it reads the entire content into the PreUsesString variable.

    Uses Clause Processing:
        The function locates the uses clause and processes it to insert necessary unit names.
        UsesArray is populated with the units listed in the original uses clause.

    Code Body Processing:
        The function processes the code body to replace DFM references with FMX and add live bindings.
        PostUsesString is updated with the modified code body content.

    Combining Results:
        Finally, the function combines the processed PreUsesString, the new uses clause (UsesString), and the processed code body (PostUsesString).
}
function TParser.GenPasFile(CONST PascalSourceFileName: String): String;
const
  MIN_FILE_SIZE = 20;
var
  PreUsesString, PostUsesString, UsesString: String;
  UsesArray: TArrayOfStrings;
  StartChr, EndChar: PChar;
  FileSize: Integer;
  Idx: Integer;
begin
  if not FileExists(PascalSourceFileName)
  then RAISE Exception.CreateFmt('Pascal source file "%s" does not exist.', [PascalSourceFileName]);

  FileSize := TFile.GetSize(PascalSourceFileName);  //todo: minify

  if FileSize > MIN_FILE_SIZE 
  then
    begin
      PreUsesString:= ccTextFile.StringFromFile(PascalSourceFileName);

      Idx := PosNoCase('uses', PreUsesString);
      if Idx = 0 then
        raise Exception.Create('The "uses" clause was not found in the Pascal source file.');

      StartChr := PChar(PreUsesString) + Idx + 4;
      EndChar := StrPos(StartChr, ';');
      if EndChar = nil then
        raise Exception.Create('The end of the "uses" clause was not found in the Pascal source file.');

      UsesArray      := GetArrayFromString(StringReplace(Copy(PreUsesString, Idx + 5, EndChar - StartChr), CRLF, '', [rfReplaceAll]), ',');
      PostUsesString := Copy(PreUsesString, EndChar - StartChr + Idx + 5, Length(PreUsesString) - (EndChar - StartChr + Idx + 4));
      PostUsesString := ProcessCodeBody(PostUsesString);

      Idx := Pos('TBindSourceDB', PostUsesString);
      if Idx > 0 then
      PostUsesString := Copy(PostUsesString, 1, Idx + 15) + GetPASLiveBindings + Copy(PostUsesString, Idx + 15);

      SetLength(PreUsesString, PosEx('uses', PreUsesString, 1) - 1);
      UsesString := ProcessUsesString(UsesArray);

      Result := PreUsesString + UsesString + PostUsesString;
  end
  else
    Result := '';
end;


function TParser.IniAddProperties: TStringlist;
begin
  if FIniAddProperties = nil
  then FIniAddProperties := TStringlist.Create;
  Result := FIniAddProperties;
end;


procedure TParser.IniFileLoad(AIni: TIniFile);
var
  i: integer;
  NewClassName: String;
begin
  Assert(AIni <> NIL);

  if FDepth < 1 then
   begin
     AIni.ReadSectionValues('ObjectChanges', IniObjectTranslations);
     AIni.ReadSectionValues('TForm', IniSectionValues);
     AIni.ReadSectionValues('TFormReplace', IniReplaceValues);
     AIni.ReadSection      ('TFormInclude', IniIncludeValues);
   end
  else
   begin
     NewClassName := AIni.ReadString('ObjectChanges', FDFMClass, EmptyStr);
     if NewClassName <> EmptyStr
     then FDFMClass := NewClassName;

     AIni.ReadSectionValues(FDFMClass, IniSectionValues);
     AIni.ReadSectionValues(FDFMClass + 'Replace', IniReplaceValues);
     AIni.ReadSection(FDFMClass + 'Include', IniIncludeValues);
     AIni.ReadSectionValues(FDFMClass + 'AddProperty', IniAddProperties);
   end;

  for i := 0 to Pred(OwnedObjs.Count) do
    if OwnedObjs[i] is TParser then
      TParser(OwnedObjs[i]).IniFileLoad(AIni);

  if FOwnedItems <> nil then
    for i := 0 to Pred(fOwnedItems.Count) do
     if fOwnedItems[i] is TParser then
       TParser(fOwnedItems[i]).IniFileLoad(AIni);

  if IniSectionValues.Count < 1 then
  begin
    AIni.WriteString(FDFMClass, 'Empty', 'Add Transformations');
    AIni.WriteString(FDFMClass, 'Top',   'Position.Y');
    AIni.WriteString(FDFMClass, 'Left',  'Position.X');
  end;

  if IniIncludeValues.Count < 1
  then AIni.WriteString(FDFMClass + 'Include', 'FMX.Controls', 'Empty Include');
end;


function TParser.IniIncludeValues: TStringlist;
begin
  if FIniIncludeValues = nil then
    FIniIncludeValues := TStringlist.Create;
  Result := FIniIncludeValues;
end;


function TParser.IniObjectTranslations: TStringList;
begin
  if FIniObjectTranslations = nil then
    FIniObjectTranslations := TStringlist.Create;
  Result := FIniObjectTranslations;
end;


function TParser.IniReplaceValues: TStringlist;
begin
  if FIniReplaceValues = nil then
    FIniReplaceValues := TStringlist.Create;
  Result := FIniReplaceValues;
end;


function TParser.IniSectionValues: TStringlist;
begin
  if FIniSectionValues = nil then
    FIniSectionValues := TStringlist.Create;
  Result := FIniSectionValues;
end;


procedure TParser.LoadInfileDefs(AIniFileName: String);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AIniFileName);
  try
  IniFileLoad(Ini);
  finally
    Ini.Free;
end;
end;

function TParser.OwnedObjs: TObjectList<TParser>;
begin
  if FOwnedObjs = nil
  then FOwnedObjs := TObjectList<TParser>.Create(TRUE);
  Result := FOwnedObjs;
end;


function TParser.ProcessCodeBody(const ACodeBody: String): String;
var
  BdyStr: String;
  Idx: Integer;
  TransArray: TArrayOfStrings;
begin
  BdyStr := StringReplace(ACodeBody, '{$R *.DFM}', '{$R *.FMX}', [rfIgnoreCase]);
  if FIniObjectTranslations <> nil then
  begin
    for Idx := 0 to FIniObjectTranslations.Count - 1 do
    begin
      TransArray := GetArrayFromString(FIniObjectTranslations[Idx], '=');
      if Length(TransArray) > 1 then
        BdyStr := StringReplace(BdyStr, TransArray[0], TransArray[1], [rfReplaceAll, rfIgnoreCase]);
    end;
  end;
  Result := BdyStr;
end;


function TParser.ProcessUsesString(AOrigUsesArray: TArrayOfStrings): String;
var
  i: Integer;
begin
  PopulateStringsFromArray(UsesTranslation, AOrigUsesArray);
  UpdateUsesStringList(UsesTranslation);
  Result := 'uses ';
  for i := 0 to Pred(UsesTranslation.Count) do
    if Trim(FUsesTranslation[i]) <> EmptyStr then
      Result := Result + CRLF +'  '+ FUsesTranslation[i] + ',';
  SetLength(Result, Length(Result) - 1); // Remove the trailing comma
  Result := Result + ';'; // NEW !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end;


function TParser.PropertyArray(ARow: Integer): TArrayOfStrings;
begin
  while ARow >= FPropertyArraySz do
  begin
    Inc(FPropertyArraySz, 5);
    SetLength(F2DPropertyArray, FPropertyArraySz);
  end;
  if ARow > FPropertyMax then
    FPropertyMax := ARow;
  Result := F2DPropertyArray[ARow];
end;


procedure TParser.ReadItems(Prop: TTwoDArrayOfString; APropertyIdx: Integer; AList: TStringList; var LineIndex: Integer);
var
  Data: String;
  saTemp: TArray<String>;
  sTemp: String;
begin
  Inc(LineIndex);
  Data := Trim(AList[LineIndex]);
  while not (Pos('>', Data) > 0) do
    begin
      SetLength(saTemp, Length(saTemp) + 1);
      saTemp[High(saTemp)] := Data;
      Inc(LineIndex);
      Data := Trim(AList[LineIndex]);
    end;
  SetLength(saTemp, Length(saTemp) + 1);
  saTemp[High(saTemp)] := Data;

  for sTemp in saTemp do
    Prop[APropertyIdx, 1] := Prop[APropertyIdx, 1] + #13 + sTemp;
end;


procedure TParser.ReadData(Prop: TTwoDArrayOfString; APropertyIdx: Integer; AList: TStringList; var LineIndex: Integer);
var
  Data: String;
begin
  Inc(LineIndex);
  Data := Trim(AList[LineIndex]);
  while not (Pos('}', Data) > 0) do
    begin
      Prop[APropertyIdx, 1] := Prop[APropertyIdx, 1] + Data;
      Inc(LineIndex);
      Data := Trim(AList[LineIndex]);
    end;
  Prop[APropertyIdx, 1] := Prop[APropertyIdx, 1] + Data;
end;


procedure TParser.ReadText(Prop: TTwoDArrayOfString; APropertyIdx: Integer; AList: TStringList; var LineIndex: Integer);
var
  Data: String;
begin
  Inc(LineIndex);
  Data := Trim(AList[LineIndex]);
  while Pos('+', Data) > 0 do
    begin
      Prop[APropertyIdx, 1] := Prop[APropertyIdx, 1] + Data;
      Inc(LineIndex);
      Data := Trim(AList[LineIndex]);
    end;
  Prop[APropertyIdx, 1] := Prop[APropertyIdx, 1] + Data;
end;


procedure TParser.ReadProperties(AData: String; AList: TStringList; var AIdx: Integer; var LineIndex: Integer);
begin
  PropertyArray(AIdx);
  F2DPropertyArray[AIdx] := GetArrayFromString(AData, '=');
  if High(F2DPropertyArray[AIdx]) < 1 then
    begin
      SetLength(F2DPropertyArray[AIdx], 2);
      F2DPropertyArray[AIdx, 0] := ContinueCode;
      F2DPropertyArray[AIdx, 1] := AData;
    end
  else
  if F2DPropertyArray[AIdx, 1] = '<'
  then ReadItems(F2DPropertyArray, AIdx, AList, LineIndex)
  else
  if F2DPropertyArray[AIdx, 1] = '{'
  then ReadData(F2DPropertyArray, AIdx, AList, LineIndex)
  else
  if F2DPropertyArray[AIdx, 1] = ''
  then ReadText(F2DPropertyArray, AIdx, AList, LineIndex);
  Inc(AIdx);
end;


function TParser.TransformProperty(ACurrentName, ACurrentValue: String; APad: String = ''): String;
var
  s: String;
begin
  if ACurrentName = ContinueCode then
    Result := ACurrentValue
  else
  begin
    s := FIniSectionValues.Values[ACurrentName];
    if s = EmptyStr then
      s := ACurrentName;
    if s = '#Delete#' then
      Result := EmptyStr
    else
    if Pos('#TAlign#', s) > 0 then
    begin
      ACurrentValue := StringReplace(ACurrentValue, 'al', EmptyStr, [rfReplaceAll]);
      Result := ACurrentName + ' = ' + ACurrentValue;
    end
    else
    if Pos('#Class#', s) > 0 then
    begin
      if FDFMClass = 'TImage' then
        Result := StringReplace(s, '#Class#', ProcessImage(ACurrentValue, APad), [])
      else
      if FDFMClass = 'TImageList' then
        Result := StringReplace(s, '#Class#', ProcessImageList(ACurrentValue, APad), [])
    end
    else
      Result := s + ' = ' + ACurrentValue;
  end;
end;


procedure TParser.UpdateUsesStringList(AUsesList: TStrings);
var
  i, Idx: Integer;
begin
  if FIniReplaceValues <> nil then
  begin
    for i := 0 to Pred(AUsesList.Count) do
    begin
      Idx := FIniReplaceValues.IndexOfName(AUsesList[i]);
      if Idx >= 0 then
        AUsesList[i] := FIniReplaceValues.ValueFromIndex[Idx];
    end;
  end;

  for i := Pred(AUsesList.Count) downto 0 do
    if Trim(AUsesList[i]) = EmptyStr then
      AUsesList.Delete(i);

  if FIniIncludeValues <> nil then
  begin
    for i := 0 to Pred(FIniIncludeValues.Count) do
    begin
      Idx := AUsesList.IndexOf(FIniIncludeValues[i]);
      if Idx < 0 then
        AUsesList.Add(FIniIncludeValues[i]);
    end;
  end;

  if FOwnedObjs = nil then
    Exit;

  for i := 0 to Pred(FOwnedObjs.Count) do
    if FOwnedObjs[i] is TParser then
      TParser(FOwnedObjs[i]).UpdateUsesStringList(AUsesList);
end;


function TParser.UsesTranslation: TStringList;
begin
  if FUsesTranslation = nil then
    FUsesTranslation := TStringList.Create;
  Result := FUsesTranslation;
end;


function TParser.WriteFMXToFile(const AFmxFileName: String): Boolean;
var
  OutFile: TFileStream;
  s: String;
begin
  s := FMXFile;
  if s.IsEmpty then
    raise Exception.Create('There is no data for the FMX file!');

  if FileExists(AFmxFileName) then
    RenameFile(AFmxFileName, ChangeFileExt(AFmxFileName, '.fbk'));

  OutFile := TFileStream.Create(AFmxFileName, fmCreate);
  try
    OutFile.Write(s[1], Length(s));
    Result := True;
  finally
    FreeAndNil(OutFile);
  end;
end;


function TParser.WritePasToFile(const APasOutFileName, APascalSourceFileName: String): Boolean;
var
  OutFile: TFileStream;
  s: String;
begin
  if not FileExists(APascalSourceFileName)
  then RAISE Exception.Create('Pascal Source: ' + APascalSourceFileName + ' Does not Exist');

  s := GenPasFile(APascalSourceFileName);
  if s = ''
  then RAISE Exception.Create('No Data for Pas File');
  s := StringReplace(s, ChangeFileExt(ExtractFileName(APascalSourceFileName), EmptyStr), ChangeFileExt(ExtractFileName(APasOutFileName), ''), [rfIgnoreCase]);
  if FileExists(APasOutFileName)
  then RenameFile(APasOutFileName, ChangeFileExt(APasOutFileName, '.bak'));

  OutFile := TFileStream.Create(APasOutFileName, fmCreate);
  try
    OutFile.Write(s[1], Length(s));
    Result := True;
  finally
    FreeAndNil(OutFile);
  end;
end;




procedure TParser.LiveBindings(DfmObject: TObjectList<TParser> = nil);
var
  I, J: Integer;
  sFields: String;
  obj: TParser;
  sItem: String;
  slItem: TStringDynArray;
begin
  // If you haven't reported an object, you get the initial one
  if DfmObject = NIL
  then DfmObject := FOwnedObjs;

  // Passa por todos objetos filhos
  for I := 0 to Pred(DfmObject.Count) do
  begin
    // Se for de conversão
    if DfmObject[I] is TParser then
    begin
      // Obtem o objeto
      obj := TParser(DfmObject[I]);

      // Se for uma grid
      if obj.FDFMClass.Equals('TDBGrid') then
      begin
        // Inicializa
        sFields := EmptyStr;

        // Cria um novo item na lista de grids
        SetLength(FLinkGridList, Succ(Length(FLinkGridList)));

        // Insere o nome da grid
        FLinkGridList[High(FLinkGridList)].GridControl := obj.FObjName;

        // Passa por todas propriedades da grid
        for J := Low(F2DPropertyArray) to High(F2DPropertyArray) do
        begin
          // Obtem os dados do DataSource
          if obj.F2DPropertyArray[J, 0].Equals('DataSource') then
            FLinkGridList[High(FLinkGridList)].DataSource := obj.F2DPropertyArray[J, 1];

          // Se for as colunas
          if obj.F2DPropertyArray[J, 0].Equals('Columns') then
          begin
            // Obtem os dados dos fields
            sFields := obj.F2DPropertyArray[J, 1];

            slItem := System.StrUtils.SplitString(sFields, #13);
            for sItem in slItem do
            begin
              if sItem = 'item' then
                SetLength(FLinkGridList[High(FLinkGridList)].Columns, Succ(Length(FLinkGridList[High(FLinkGridList)].Columns)))
              else if Trim(System.StrUtils.SplitString(sItem, '=')[0]) = 'Title.Caption' then
                FLinkGridList[High(FLinkGridList)].Columns[High(FLinkGridList[High(FLinkGridList)].Columns)].Caption := Trim(System.StrUtils.SplitString(sItem, '=')[1])
              else if Trim(System.StrUtils.SplitString(sItem, '=')[0]) = 'FieldName' then
                FLinkGridList[High(FLinkGridList)].Columns[High(FLinkGridList[High(FLinkGridList)].Columns)].FieldName := Trim(System.StrUtils.SplitString(sItem, '=')[1])
              else if Trim(System.StrUtils.SplitString(sItem, '=')[0]) = 'Width' then
                FLinkGridList[High(FLinkGridList)].Columns[High(FLinkGridList[High(FLinkGridList)].Columns)].Width := Trim(System.StrUtils.SplitString(sItem, '=')[1]);
            end;
          end;

          // Se ja encontrou tudo, sai do loop
          if not FLinkGridList[High(FLinkGridList)].DataSource.IsEmpty and not sFields.IsEmpty then
            Break;
        end;
      end;

      // Se for um dbedit
      if obj.FDFMClass.Equals('TDBEdit') then
      begin
        // Cria um novo item na lista de dbedits
        SetLength(FLinkControlList, Succ(Length(FLinkControlList)));

        // Insere o nome do dbedit
        FLinkControlList[High(FLinkControlList)].Control := obj.FObjName;

        // Passa por todas propriedades do dbedit
        for J := Low(F2DPropertyArray) to High(F2DPropertyArray) do
        begin
          // Obtem os dados do DataSource
          if obj.F2DPropertyArray[J, 0].Equals('DataSource') then
            FLinkControlList[High(FLinkControlList)].DataSource := obj.F2DPropertyArray[J, 1];

          // Obtem os dados do field
          if obj.F2DPropertyArray[J, 0].Equals('DataField') then
            FLinkControlList[High(FLinkControlList)].FieldName := GetArrayFromString(obj.F2DPropertyArray[J, 1], '=', True, True)[0];

          // Se ja encontrou tudo, sai do loop
          if not FLinkControlList[High(FLinkControlList)].DataSource.IsEmpty and not FLinkControlList[High(FLinkControlList)].FieldName.IsEmpty then
            Break;
        end;
      end;

      // Se o componente atual possui componentes nele, faz recursão
      if Assigned(obj.FOwnedObjs) and (obj.FOwnedObjs.Count > 0) then
        LiveBindings(obj.FOwnedObjs);
    end;
  end;
end;


function TParser.GetPASLiveBindings: String;
var
  I: Integer;
begin
  if (Length(FLinkControlList) = 0) and (Length(FLinkGridList) = 0) then
    Exit(EmptyStr);

  // Adiciona BindingsList
  Result := '    BindingsList: TBindingsList; ';

  // Go through the list of controls
  for I := 0 to High(FLinkControlList) do
    Result := Result + CRLF + '    LinkControlToField' + I.ToString + ': TLinkControlToField; ';

  // Passa pela lista de grids
  for I := 0 to High(FLinkGridList) do
    Result := Result + CRLF + '    LinkGridToDataSourceBindSourceDB' + I.ToString + ': TLinkGridToDataSource; ';
end;


function TParser.GetFMXLiveBindings: String;
var
  I, J: Integer;
begin
  if (Length(FLinkControlList) = 0) and (Length(FLinkGridList) = 0)
  then Exit(EmptyStr);

  // Adiciona BindingsList
  Result :=
    '  object BindingsList: TBindingsList ' +
    CRLF + '    Methods = <> ' +
    CRLF + '    OutputConverters = <> ' +
    CRLF + '    Left = 20 ' +
    CRLF + '    Top = 5 ';

  // Passa pela lista de controles
  for I := 0 to High(FLinkControlList) do
  begin
    Result := Result +
      CRLF + '    object LinkControlToField' + I.ToString + ': TLinkControlToField ' +
      CRLF + '      Category = ' + QuotedStr('Quick Bindings') +
      CRLF + '      DataSource = ' + FLinkControlList[I].DataSource +
      CRLF + '      FieldName = ' + QuotedStr(FLinkControlList[I].FieldName) +
      CRLF + '      Control = ' + FLinkControlList[I].Control +
      CRLF + '      Track = False ' +
      CRLF + '    end ';
  end;

  // Passa pela lista de grids
  for I := 0 to High(FLinkGridList) do
  begin
    Result := Result +
      CRLF + '    object LinkGridToDataSourceBindSourceDB' + I.ToString + ': TLinkGridToDataSource ' +
      CRLF + '      Category = ' + QuotedStr('Quick Bindings') +
      CRLF + '      DataSource = ' + FLinkGridList[I].DataSource +
      CRLF + '      GridControl = ' + FLinkGridList[I].GridControl +
      CRLF + '      Columns = < ';

    // Passa pela lista de colunas da grid
    for J := 0 to High(FLinkGridList[I].Columns) do
    begin
      Result := Result +
        CRLF + '        item ' +
        CRLF + '          MemberName = ' + FLinkGridList[I].Columns[J].FieldName;

      // Se tem Caption
      if not FLinkGridList[I].Columns[J].Caption.IsEmpty then
        Result := Result + CRLF + '          Header = ' + FLinkGridList[I].Columns[J].Caption;

      // Se tem Width
      if not FLinkGridList[I].Columns[J].Width.IsEmpty then
        Result := Result + CRLF + '          Width = ' + FLinkGridList[I].Columns[J].Width;

      Result := Result + CRLF + '        end ';
    end;

    Result := Result + CRLF + '        > ' + CRLF + '    end ';
  end;

  Result := Result + CRLF + '  end ';
end;


(*
{ TDfmToFmxListItem }
constructor TDfmToFmxListItem.Create(AOwner: TParser; APropertyIdx: Integer; AList: TStringList; ADepth: Integer; var LineIndex: Integer);
var
  Data: String;
  i, LoopCount: Integer;
begin
  inherited Create('item', AList, ADepth, LineIndex);

  i := 0;
  FOwner := AOwner;
  FDepth := ADepth;
  Data   := EmptyStr;
  FPropertyIndex := APropertyIdx;

  LoopCount := 55;
  while (LoopCount > 0) and (Pos('end', Data) <> 1) do
  begin
    Dec(LoopCount);
    if Pos('object', Data) = 1
    then FOwnedObjs.Add(TParser.Create(Data, AList, FDepth + 1, LineIndex))
    else ReadProperties(Data, AList, i, LineIndex);
    Inc(LineIndex);
    Data := Trim(AList[LineIndex]);
    if Data <> EmptyStr then
      LoopCount := 55;
  end;

  SetLength(F2DPropertyArray, FPropertyMax + 1);
  FHasMore := (Pos('end', Data) = 1) and not (Pos('end>', Data) = 1);
end;
     *)

end.
