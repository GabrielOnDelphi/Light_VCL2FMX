unit Parser;

{=============================================================================================================
   Gabriel Moraru
   2025.04
   Convert VCL to FMX
--------------------------------------------------------------------------------------------------------------
   This program requires https://github.com/GabrielOnDelphi/Delphi-LightSaber
=============================================================================================================}

INTERFACE

USES
  System.Classes,
  System.Types,
  System.SysUtils,
  System.StrUtils,
  System.Generics.Collections,
  Winapi.Windows,
  IniFiles,

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
    OwnedObjs: TObjectList<TParser>;
    FOwnedItems: TObjectList<TObject>;
    FDepth: Integer;
    F2DPropertyArray: TTwoDArrayOfString;
    FPropertyArraySz, FPropertyMax: Integer;
    FIniReplaceValues,
    FIniIncludeValues,
    FIniSectionValues,
    IniAddProperties,
    UsesTranslation,
    FIniObjectTranslations: TStringList;
    function AddArrayOfItemProperties(PropertyIdx: Integer; const Pad: String): String;
    function FMXClass: String;
    function FMXProperties(const Pad: String): String;
    function FMXSubObjects(const Pad: String): String;
    function GetFMXLiveBindings: String;
    function IniIncludeValues: TStringList;
    function IniObjectTranslations: TStringList;
    function IniReplaceValues: TStringList;
    function IniSectionValues: TStringList;

    function ProcessCodeBody(const CodeBody: String): String;
    function ProcessUsesString(const OrigUsesArray: TArrayOfStrings): String;

    function PropertyArray(Row: Integer): TArrayOfStrings;
    function TransformProperty(ACurrentName, ACurrentValue: String; APad: String = ''): String;
    procedure IniFileLoad(Ini: TIniFile);

    procedure ReadData      (Prop: TTwoDArrayOfString; PropertyIdx: Integer; List: TStringList; var LineIndex: Integer);
    procedure ReadItems     (Prop: TTwoDArrayOfString; PropertyIdx: Integer; List: TStringList; var LineIndex: Integer);
    procedure ReadText      (Prop: TTwoDArrayOfString; PropertyIdx: Integer; List: TStringList; var LineIndex: Integer);
    procedure ReadProperties(const sData: String; List: TStringList; var Idx: Integer; var LineIndex: Integer);

    procedure UpdateUsesStringList(UsesList: TStrings);
  protected
    function GetPASLiveBindings: String;    // UNUSED
  public
    constructor Create(const CreateText: String; AList: TStringList; ADepth: Integer; var LineIndex: Integer);
    destructor Destroy; override;
    procedure LoadInFileDefs(const IniFileName: String);
    function  GenPasFile1(const PascalSourceFileName: String): String;
    function  BuildFmxFile(const Pad: String = ''): String;

    procedure WriteFMXToFile(const FmxFileName: String);
    procedure WritePasToFile(const OutputFile, InputFile: String);

    procedure LiveBindings(DfmObject: TObjectList<TParser> = nil);
    class function IsTextDFM(const DfmFileName: String): Boolean;
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
  ccTextFile;

CONST
  ContinueCode: String = '#$Continue$#';



constructor TParser.Create(const CreateText: String; aList: TStringList; ADepth: Integer; var LineIndex: Integer);
var
  i: Integer;
  CurrentLine: String;
  InputArray: TArrayOfStrings;
begin
  inherited Create;

  IniAddProperties := TStringlist.Create;
  UsesTranslation  := TStringList.Create;

  i := 0;
  FDepth := ADepth;
  OwnedObjs  := TObjectList<TParser>.Create(True);
  FOwnedItems := TObjectList<TObject>.Create(True);

  if Pos('object', Trim(CreateText)) = 1
  then
    begin
      InputArray := GetArrayFromString(CreateText, ' ');
      if Length(InputArray) < 3
      then RAISE Exception.Create('Invalid object declaration: ' + CreateText);

      FObjName  := InputArray[1];
      FDFMClass := InputArray[2];
      Inc(LineIndex);                // Start reading from the next line
      while (LineIndex < AList.Count) and (Trim(AList[LineIndex]) <> 'end') do
        begin
          CurrentLine := Trim(AList[LineIndex]);
          if Pos('object', CurrentLine) = 1
          then
            begin
              // Pass the current LineIndex to the new parser instance and let it manage
              OwnedObjs.Add(TParser.Create(CurrentLine, AList, FDepth + 1, LineIndex));
              Dec(LineIndex); // Adjust LineIndex after returning from nested parser
            end
          else
            ReadProperties(CurrentLine, AList, i, LineIndex);
          Inc(LineIndex);
        end;

      // Ensure to skip the 'end' line if it matches
      if (LineIndex < AList.Count) and (Trim(AList[LineIndex]) = 'end')
      then Inc(LineIndex);
    end
  else
    raise Exception.Create('Bad Start: ' + CreateText);

  SetLength(F2DPropertyArray, FPropertyMax + 1);
end;



destructor TParser.Destroy;
begin
  FreeAndNil(OwnedObjs);
  FreeAndNil(FOwnedItems);
  FreeAndNil(FIniReplaceValues);
  FreeAndNil(FIniIncludeValues);
  FreeAndNil(FIniSectionValues);
  FreeAndNil(IniAddProperties);
  FreeAndNil(UsesTranslation);

  inherited Destroy;
end;




function TParser.AddArrayOfItemProperties(PropertyIdx: Integer; const Pad: String): String;
begin
  Result := Pad + '  item' + CRLF +
            Pad + '  Prop1 = 6' + CRLF +
            Pad + '  end>' + CRLF;
  // Temporary patch
end;


class function TParser.IsTextDFM(const DfmFileName: String): Boolean;
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


// Builds the FMX
function TParser.BuildFmxFile(const Pad: String = ''): String;
var
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  try
    Builder.Append(Pad).Append('object ').Append(FObjName).Append(': ').Append(FMXClass).Append(CRLF)
           .Append(FMXProperties(Pad))
           .Append(FMXSubObjects(Pad + ' '));
    if Pad.IsEmpty then
      Builder.Append(GetFMXLiveBindings).Append(CRLF).Append(Pad).Append('end').Append(CRLF)
    else
      Builder.Append(Pad).Append('end').Append(CRLF);
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;


function TParser.FMXProperties(const Pad: String): String;
var
  i: Integer;
  sProp: String;
begin
  Result := EmptyStr;
  for i := Low(F2DPropertyArray) to High(F2DPropertyArray) do

    if F2DPropertyArray[i, 1] = '<'
    then Result := Result + Pad +'  '+ TransformProperty(F2DPropertyArray[i, 0], F2DPropertyArray[i, 1]) + CRLF + AddArrayOfItemProperties(i, Pad +'  ') + CRLF
    else
      if F2DPropertyArray[i, 1][1] = '{' then
      begin
        sProp := TransformProperty(F2DPropertyArray[i, 0], F2DPropertyArray[i, 1], Pad);
        if not sProp.IsEmpty then
          Result := Result + Pad +'  '+ sProp + CRLF;
      end
      else
        if F2DPropertyArray[i, 0] <> EmptyStr then
        begin
          sProp := TransformProperty(F2DPropertyArray[i, 0], F2DPropertyArray[i, 1]);
          if not sProp.IsEmpty then
            Result := Result + Pad +'  '+ sProp + CRLF;
        end;


  if IniAddProperties.Count > 0 then
    for i := 0 to Pred(IniAddProperties.Count) do
      Result := Result + Pad +'  '+ StringReplace(IniAddProperties[i], '=', ' = ', []) + CRLF;
end;


function TParser.FMXSubObjects(const Pad: String): String;
var
  I: Integer;
begin
  Result := '';
  if not Assigned(OwnedObjs) then Exit;

  for I := 0 to OwnedObjs.Count - 1 do
    if OwnedObjs[I] is TParser then
      Result := Result + TParser(OwnedObjs[I]).BuildFmxFile(Pad + ' ');
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
function TParser.GenPasFile1(const PascalSourceFileName: String): String; //mine
var
  Body, PreUsesString, PostUsesString, UsesString, UsesClause: String;
  UsesArray: TArrayOfStrings;
  iPos, StartPos, PosSemicol, IdxInterface: Integer;
begin
  if not FileExists(PascalSourceFileName) then
    raise Exception.CreateFmt('Pascal source file "%s" does not exist.', [PascalSourceFileName]);

  Body := ccTextFile.StringFromFile(PascalSourceFileName);
  if Body.Length <= 20 then Exit('');

  // Find "interface" first
  IdxInterface := PosNoCase('interface', Body);
  if IdxInterface = 0
  then raise Exception.Create('The "interface" keyword was not found in the Pascal source file.');

  // Find "uses" after "interface"
  iPos := PosEx('uses', LowerCase(Body), IdxInterface);
  if iPos = 0
  then raise Exception.Create('The "uses" clause was not found after "interface" in the Pascal source file.');

  StartPos := iPos + 5; // After "uses "
  PosSemicol := PosEx(';', Body, StartPos);
  if PosSemicol = 0
  then raise Exception.Create('The end of the "uses" clause was not found in the Pascal source file.');

  UsesClause     := Copy(Body, StartPos, PosSemicol - StartPos);
  UsesArray      := GetArrayFromString(StringReplace(UsesClause, CRLF, '', [rfReplaceAll]), ',');
  PostUsesString := Copy(Body, PosSemicol + 1, MaxInt); // After ';'
  PostUsesString := ProcessCodeBody(PostUsesString);

  PreUsesString := Copy(Body, 1, iPos - 1); // Everything before "uses"
  UsesString := ProcessUsesString(UsesArray);

  Result := PreUsesString + UsesString + PostUsesString;
end;



procedure TParser.IniFileLoad(Ini: TIniFile);
var
  I: Integer;
  NewClassName: String;
begin
  Assert(Assigned(Ini));

  if FDepth < 1 then
  begin
    Ini.ReadSectionValues('ObjectChanges', IniObjectTranslations);
    Ini.ReadSectionValues('TForm'        , IniSectionValues);
    Ini.ReadSectionValues('TFormReplace' , IniReplaceValues);
    Ini.ReadSection      ('TFormInclude' , IniIncludeValues);
  end
  else
  begin
    NewClassName := Ini.ReadString('ObjectChanges', FDFMClass, '');
    if not NewClassName.IsEmpty then
      FDFMClass := NewClassName;

    Ini.ReadSectionValues(FDFMClass, IniSectionValues);
    Ini.ReadSectionValues(FDFMClass + 'Replace', IniReplaceValues);
    Ini.ReadSection      (FDFMClass + 'Include', IniIncludeValues);
    Ini.ReadSectionValues(FDFMClass + 'AddProperty', IniAddProperties);
  end;

  for I := 0 to OwnedObjs.Count - 1 do
    if OwnedObjs[I] is TParser
    then TParser(OwnedObjs[I]).IniFileLoad(Ini);

  if Assigned(FOwnedItems) then
    for I := 0 to FOwnedItems.Count - 1 do
      if FOwnedItems[I] is TParser
      then TParser(FOwnedItems[I]).IniFileLoad(Ini);

  if IniSectionValues.Count < 1 then
  begin
    Ini.WriteString(FDFMClass, 'Empty', 'Add Transformations');
    Ini.WriteString(FDFMClass, 'Top', 'Position.Y');
    Ini.WriteString(FDFMClass, 'Left', 'Position.X');
  end;

  if IniIncludeValues.Count < 1
  then Ini.WriteString(FDFMClass + 'Include', 'FMX.Controls', 'Empty Include');
end;


function TParser.IniIncludeValues: TStringList;
begin
  if FIniIncludeValues = nil
  then FIniIncludeValues := TStringlist.Create;
  Result := FIniIncludeValues;
end;


function TParser.IniObjectTranslations: TStringList;
begin
  if FIniObjectTranslations = nil
  then FIniObjectTranslations := TStringlist.Create;
  Result := FIniObjectTranslations;
end;


function TParser.IniReplaceValues: TStringList;
begin
  if FIniReplaceValues = nil
  then FIniReplaceValues := TStringlist.Create;
  Result := FIniReplaceValues;
end;


function TParser.IniSectionValues: TStringlist;
begin
  if FIniSectionValues = nil then
    FIniSectionValues := TStringlist.Create;
  Result := FIniSectionValues;
end;


procedure TParser.LoadInFileDefs(const IniFileName: String);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(IniFileName);
  try
    IniFileLoad(Ini);
  finally
    Ini.Free;
  end;
end;


function TParser.ProcessCodeBody(const CodeBody: String): String;
var
  BodyStr: String;
  Idx: Integer;
  TransArray: TArrayOfStrings;
begin
  BodyStr := StringReplace(CodeBody, '{$R *.DFM}', '{$R *.FMX}', [rfIgnoreCase]);
  if Assigned(FIniObjectTranslations) then
    for Idx := 0 to FIniObjectTranslations.Count - 1 do
    begin
      TransArray := GetArrayFromString(FIniObjectTranslations[Idx], '=');
      if Length(TransArray) > 1 then
        BodyStr := StringReplace(BodyStr, TransArray[0], TransArray[1], [rfReplaceAll, rfIgnoreCase]);
    end;
  Result := BodyStr;
end;


function TParser.ProcessUsesString(const OrigUsesArray: TArrayOfStrings): String;
var
  I: Integer;
  Builder: TStringBuilder;
begin
  PopulateStringsFromArray(UsesTranslation, OrigUsesArray);
  UpdateUsesStringList(UsesTranslation);

  Builder := TStringBuilder.Create;
  try
    Builder.Append('uses ');
    for I := 0 to UsesTranslation.Count - 1 do
      if not Trim(UsesTranslation[I]).IsEmpty then
        Builder.Append(CRLF).Append('  ').Append(UsesTranslation[I]).Append(',');
    Result := Builder.ToString.TrimRight([',']) + ';';
  finally
    Builder.Free;
  end;
end;

function TParser.PropertyArray(Row: Integer): TArrayOfStrings;
begin
  while Row >= FPropertyArraySz do
  begin
    Inc(FPropertyArraySz, 5);
    SetLength(F2DPropertyArray, FPropertyArraySz);
  end;
  if Row > FPropertyMax then
    FPropertyMax := Row;
  Result := F2DPropertyArray[Row];
end;


procedure TParser.ReadItems(Prop: TTwoDArrayOfString; PropertyIdx: Integer; List: TStringList; var LineIndex: Integer);
var
  Data: String;
  TempArray: TArray<String>;
begin
  Inc(LineIndex);
  Data := Trim(List[LineIndex]);
  while Pos('>', Data) = 0 do
  begin
    SetLength(TempArray, Length(TempArray) + 1);
    TempArray[High(TempArray)] := Data;
    Inc(LineIndex);
    Data := Trim(List[LineIndex]);
  end;
  SetLength(TempArray, Length(TempArray) + 1);
  TempArray[High(TempArray)] := Data;

  Prop[PropertyIdx, 1] := String.Join(#13, TempArray);
end;


procedure TParser.ReadData(Prop: TTwoDArrayOfString; PropertyIdx: Integer; List: TStringList; var LineIndex: Integer);
var 
  Data: String;
begin
  Inc(LineIndex);
  Data := Trim(List[LineIndex]);
  while Pos('}', Data) = 0 do
  begin
    Prop[PropertyIdx, 1] := Prop[PropertyIdx, 1] + Data;
    Inc(LineIndex);
    Data := Trim(List[LineIndex]);
  end;
  Prop[PropertyIdx, 1] := Prop[PropertyIdx, 1] + Data;
end;


procedure TParser.ReadText(Prop: TTwoDArrayOfString; PropertyIdx: Integer; List: TStringList; var LineIndex: Integer);
var
  Data: String;
begin
  Inc(LineIndex);
  Data := Trim(List[LineIndex]);
  while Pos('+', Data) > 0 do
  begin
    Prop[PropertyIdx, 1] := Prop[PropertyIdx, 1] + Data;
    Inc(LineIndex);
    Data := Trim(List[LineIndex]);
  end;
  Prop[PropertyIdx, 1] := Prop[PropertyIdx, 1] + Data;
end;


procedure TParser.ReadProperties(const sData: String; List: TStringList; var Idx: Integer; var LineIndex: Integer);
begin
  PropertyArray(Idx);
  F2DPropertyArray[Idx] := GetArrayFromString(sData, '=');
  if Length(F2DPropertyArray[Idx]) < 2 then
  begin
    SetLength(F2DPropertyArray[Idx], 2);
    F2DPropertyArray[Idx, 0] := ContinueCode;
    F2DPropertyArray[Idx, 1] := sData;
  end
  else if F2DPropertyArray[Idx, 1] = '<' then
    ReadItems(F2DPropertyArray, Idx, List, LineIndex)
  else if F2DPropertyArray[Idx, 1] = '{' then
    ReadData(F2DPropertyArray, Idx, List, LineIndex)
  else if F2DPropertyArray[Idx, 1].IsEmpty then
    ReadText(F2DPropertyArray, Idx, List, LineIndex);
  Inc(Idx);
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
    if s = EmptyStr
    then s := ACurrentName;
    if s = '#Delete#'
    then Result := EmptyStr
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


procedure TParser.UpdateUsesStringList(UsesList: TStrings);
var
  I, Idx: Integer;
begin
  if Assigned(FIniReplaceValues) then
    for I := 0 to UsesList.Count - 1 do
    begin
      Idx := FIniReplaceValues.IndexOfName(UsesList[I]);
      if Idx >= 0 then
        UsesList[I] := FIniReplaceValues.ValueFromIndex[Idx];
    end;

  for I := UsesList.Count - 1 downto 0 do
    if Trim(UsesList[I]).IsEmpty then
      UsesList.Delete(I);

  if Assigned(FIniIncludeValues) then
    for I := 0 to FIniIncludeValues.Count - 1 do
      if UsesList.IndexOf(FIniIncludeValues[I]) < 0 then
        UsesList.Add(FIniIncludeValues[I]);

  if Assigned(OwnedObjs) then
    for I := 0 to OwnedObjs.Count - 1 do
      if OwnedObjs[I] is TParser then
        TParser(OwnedObjs[I]).UpdateUsesStringList(UsesList);
end;


procedure TParser.WriteFMXToFile(const FmxFileName: String);
var
  FmxBody: String;
begin
  FmxBody := BuildFmxFile;
  if FmxBody.IsEmpty then
    raise Exception.Create('There is no data for the FMX file!');

  if FileExists(FmxFileName) then
    RenameFile(FmxFileName, ChangeFileExt(FmxFileName, '.fbk'));

  StringToFile(FmxFileName, FmxBody);
end;


// Update the PAS file
procedure TParser.WritePasToFile(const OutputFile, InputFile: String);
var
  FileContent: String;
begin
  if not FileExists(InputFile)
  then RAISE Exception.Create('Pascal Source: ' + InputFile + ' Does not Exist');

  FileContent := GenPasFile1(InputFile);
  if FileContent = ''
  then  raise Exception.Create('No data for PAS file');

  // Replace old file name with the new filename
  var s1:= ChangeFileExt(ExtractFileName(InputFile), '');
  var s2:= ChangeFileExt(ExtractFileName(OutputFile), '');

  FileContent := StringReplace(FileContent, s1, s2, [rfIgnoreCase]);
  if FileExists(OutputFile)
  then RenameFile(OutputFile, ChangeFileExt(OutputFile, '.bak'));

  StringToFile(OutputFile, FileContent);
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
  then DfmObject := OwnedObjs;

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
      if Assigned(Obj.OwnedObjs) and (Obj.OwnedObjs.Count > 0) then
        LiveBindings(Obj.OwnedObjs);
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
  Result :='  object BindingsList: TBindingsList ' +
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




(*
function TParser.GenPasFile2(const PascalSourceFileName: String): AnsiString;
var
  PasFile: TFileStream;
  PreUsesString, PostUsesString, UsesString: AnsiString;
  UsesArray: TArrayOfStrings;
  StartChr, EndChar: PAnsiChar;
  Sz: integer;
  Idx: integer;
  s: String;
begin
  Result := '';
  PostUsesString := '';
  UsesString := '';
  if not FileExists(PascalSourceFileName) then
    Exit;

  PasFile := TFileStream.Create(PascalSourceFileName, fmOpenRead);
  try
    Sz := PasFile.Size;
    if Sz > 20 then
    begin
      SetLength(PreUsesString, Sz);
      Idx := PasFile.Read(PreUsesString[1], Sz);
      if Idx <> Sz then
        raise Exception.Create('Error Pas file read');
    end
    else
      PreUsesString := '';
  finally
    FreeAndNil(PasFile);
  end;

  if Sz > 20 then
  begin
    Idx := PosNoCase('uses', String(PreUsesString));
    StartChr := @PreUsesString[Idx + 4];
    s := ';';
    EndChar := StrPos(StartChr, PAnsiChar(s));
    UsesArray := GetArrayFromString(StringReplace(Copy(String(PreUsesString), Idx + 4, EndChar - StartChr), CRLF, '', [rfReplaceAll]), ',');
    PostUsesString := Copy(PreUsesString, EndChar - StartChr + Idx + 4, Sz);
    PostUsesString := AnsiString(ProcessCodeBody(String(PostUsesString)));

    PostUsesString := AnsiString(Copy(String(PostUsesString), 1, Pos('TBindSourceDB', String(PostUsesString)) + 15) +
      GetPASLiveBindings +
      Copy(String(PostUsesString), Pos('TBindSourceDB', String(PostUsesString)) + 15));

    SetLength(PreUsesString, Pred(Idx));
    UsesString := AnsiString(ProcessUsesString(UsesArray));
  end;
  Result := PreUsesString + UsesString + PostUsesString;
end; *)