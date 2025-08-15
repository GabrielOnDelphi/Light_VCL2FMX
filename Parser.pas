unit Parser;

{=============================================================================================================
   Gabriel Moraru
   2025.04
   Convert VCL to FMX
--------------------------------------------------------------------------------------------------------------
   This program requires https://github.com/GabrielOnDelphi/Delphi-LightSaber
=============================================================================================================}

{ Fork from Eduardo Rodrigues 2019 }

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
  LightCore,
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
    DFMClass: String;
    ObjectName: String;
    OwnedObjs: TObjectList<TParser>;
    FOwnedItems: TObjectList<TObject>;
    DepthLevel: Integer;
    Properties: TTwoDArrayOfString;
    FPropertyArraySz,
    MaxPropertyIndex: Integer;
    IniReplaceValues,
    IniIncludeValues,
    IniSectionValues,
    IniAddProperties,
    UsesTranslation,
    IniObjectTranslations: TStringList;
    function AddArrayOfItemProperties(PropertyIdx: Integer; const Pad: String): String;
    function FMXClass: String;
    function FMXProperties(const Indent: String): String;
    function FMXSubObjects(const Indent: String): String;
    function GetFMXLiveBindings: string;

    function ProcessCodeBody(const CodeBody: String): String;
    function ProcessUsesString(const OrigUsesArray: TArrayOfStrings): String;

    function PropertyArray(Row: Integer): TArrayOfStrings;
    function TransformProperty(ACurrentName, ACurrentValue: String; APad: String = ''): String;
    procedure _loadConfigFile(Ini: TIniFile);

    procedure ReadData      (Prop: TTwoDArrayOfString; PropertyIdx: Integer; List: TStringList; var LineIndex: Integer);
    procedure ReadItems     (Prop: TTwoDArrayOfString; PropertyIdx: Integer; List: TStringList; var LineIndex: Integer);
    procedure ReadText      (Prop: TTwoDArrayOfString; PropertyIdx: Integer; List: TStringList; var LineIndex: Integer);
    procedure ReadProperties(const sData: String; List: TStringList; var Idx: Integer; var LineIndex: Integer);

    procedure UpdateUses(UsesList: TStrings);
  protected
    function GetPASLiveBindings: String;    // UNUSED
  public
    constructor Create(const CreateText: String; ContentList: TStringList; Depth: Integer; var LineIndex: Integer);
    destructor Destroy; override;
    procedure LoadConfigFile(const IniFileName: String);
    function  GenPasFile1(const PascalSourceFileName: String): String;
    function  BuildFmxFile(const Indent: String = ''): String;

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
  LightCore.TextFile;

CONST
  ContinueCode: String = '#$Continue$#';



constructor TParser.Create(const CreateText: String; ContentList: TStringList; Depth: Integer; var LineIndex: Integer);
var
  i: Integer;
  CurrentLine: String;
  InputArray: TArrayOfStrings;
begin
  inherited Create;

  IniAddProperties := TStringlist.Create;
  UsesTranslation  := TStringList.Create;
  IniSectionValues := TStringlist.Create;
  IniIncludeValues := TStringlist.Create;
  IniReplaceValues := TStringlist.Create;
  IniObjectTranslations := TStringlist.Create;

  i := 0;
  DepthLevel := Depth;
  OwnedObjs  := TObjectList<TParser>.Create(True);
  FOwnedItems := TObjectList<TObject>.Create(True);

  if Pos('object', Trim(CreateText)) = 1
  then
    begin
      InputArray := GetArrayFromString(CreateText, ' ');
      if Length(InputArray) < 3
      then RAISE Exception.Create('Invalid object declaration: ' + CreateText);

      objectName := StringReplace(inputArray[1], ':', '', [rfReplaceAll]); // Remove any colons to prevent double colon bug
      DFMClass := InputArray[2];
      Inc(LineIndex);                // Start reading from the next line
      while (LineIndex < ContentList.Count) and (Trim(ContentList[LineIndex]) <> 'end') do
        begin
          CurrentLine := Trim(ContentList[LineIndex]);
          if Pos('object', CurrentLine) = 1
          then
            begin
              // Pass the current LineIndex to the new parser instance and let it manage
              OwnedObjs.Add(TParser.Create(CurrentLine, ContentList, DepthLevel + 1, LineIndex));
              Dec(LineIndex); // Adjust LineIndex after returning from nested parser
            end
          else
            ReadProperties(CurrentLine, ContentList, i, LineIndex);
          Inc(LineIndex);
        end;

      // Ensure to skip the 'end' line if it matches
      if (LineIndex < ContentList.Count) and (Trim(ContentList[LineIndex]) = 'end')
      then Inc(LineIndex);
    end
  else
    raise Exception.Create('Invalid object start: ' + createText);

  SetLength(Properties, MaxPropertyIndex + 1);
end;



destructor TParser.Destroy;
begin
  FreeAndNil(OwnedObjs);
  FreeAndNil(FOwnedItems);
  FreeAndNil(IniReplaceValues);
  FreeAndNil(IniIncludeValues);
  FreeAndNil(IniSectionValues);
  FreeAndNil(IniAddProperties);
  FreeAndNil(UsesTranslation);
  FreeAndNil(IniObjectTranslations);

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
begin
  Result:= FileExists(DfmFileName);
  if Result then
    begin
      VAR DfmBody:= LightCore.TextFile.StringFromFile(DfmFileName);
      Result:= PosInsensitive('object', DfmBody) < 20;
    end;
end;


function TParser.FMXClass: String;
begin
  Result := DFMClass;
end;


// Builds the FMX
function TParser.BuildFmxFile(const Indent: String = ''): String;
var
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  try
    Builder.Append(Indent).Append('object ').Append(ObjectName).Append(': ').Append(FMXClass).Append(CRLF)
           .Append(FMXProperties(Indent))
           .Append(FMXSubObjects(Indent + ' '));
    if Indent.IsEmpty
    then
      Builder.Append(GetFMXLiveBindings).Append(CRLF).Append(Indent).Append('end').Append(CRLF)
    else
      Builder.Append(Indent).Append('end').Append(CRLF);

    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;


function TParser.FMXProperties(const Indent: String): String;
var
  i: Integer;
  sProp: String;
begin
  Result := EmptyStr;
  for i := Low(Properties) to High(Properties) do

    if Properties[i, 1] = '<'
    then Result := Result + Indent +'  '+ TransformProperty(Properties[i, 0], Properties[i, 1]) + CRLF + AddArrayOfItemProperties(i, Indent +'  ') + CRLF
    else
      if Properties[i, 1][1] = '{' then
      begin
        sProp := TransformProperty(Properties[i, 0], Properties[i, 1], Indent);
        if not sProp.IsEmpty then
          Result := Result + Indent +'  '+ sProp + CRLF;
      end
      else
        if Properties[i, 0] <> EmptyStr then
        begin
          sProp := TransformProperty(Properties[i, 0], Properties[i, 1]);
          if not sProp.IsEmpty then
            Result := Result + Indent +'  '+ sProp + CRLF;
        end;

  if IniAddProperties.Count > 0 then
    for i := 0 to Pred(IniAddProperties.Count) do
      Result := Result + Indent +'  '+ StringReplace(IniAddProperties[i], '=', ' = ', []) + CRLF;
end;


function TParser.FMXSubObjects(const Indent: String): String;
var
  I: Integer;
begin
  Result := '';
  if not Assigned(OwnedObjs) then Exit;

  for I := 0 to OwnedObjs.Count - 1 do
    if OwnedObjs[I] is TParser then
      Result := Result + TParser(OwnedObjs[I]).BuildFmxFile(Indent + ' ');
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

  Body := LightCore.TextFile.StringFromFile(PascalSourceFileName);
  if Body.Length <= 20 then Exit('');

  // Find "interface" first
  IdxInterface := PosInsensitive('interface', Body);
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



procedure TParser._loadConfigFile(Ini: TIniFile);
var
  I: Integer;
  NewClassName: String;
begin
  Assert(Assigned(Ini));

   if DepthLevel < 1 then
   begin
     Ini.ReadSectionValues('ObjectChanges', IniObjectTranslations);
     Ini.ReadSectionValues('TForm'        , IniSectionValues);
     Ini.ReadSectionValues('TFormReplace' , IniReplaceValues);
     Ini.ReadSection      ('TFormInclude' , IniIncludeValues);
   end
  else
   begin
     NewClassName := Ini.ReadString('ObjectChanges', DFMClass, '');
     if not NewClassName.IsEmpty then
       DFMClass := NewClassName;

     Ini.ReadSectionValues(DFMClass, IniSectionValues);
     Ini.ReadSectionValues(DFMClass + 'Replace', IniReplaceValues);
     Ini.ReadSection      (DFMClass + 'Include', IniIncludeValues);
     Ini.ReadSectionValues(DFMClass + 'AddProperty', IniAddProperties);
   end;

  for I := 0 to OwnedObjs.Count - 1 do
    if OwnedObjs[I] is TParser
    then TParser(OwnedObjs[I])._loadConfigFile(Ini);

  if Assigned(FOwnedItems) then
    for I := 0 to FOwnedItems.Count - 1 do
      if FOwnedItems[I] is TParser
      then TParser(FOwnedItems[I])._loadConfigFile(Ini);

  if IniSectionValues.Count < 1 then
  begin
    Ini.WriteString(DFMClass, 'Empty', 'Add Transformations');
    Ini.WriteString(DFMClass, 'Top', 'Position.Y');
    Ini.WriteString(DFMClass, 'Left', 'Position.X');
  end;

  if IniIncludeValues.Count < 1
  then Ini.WriteString(DFMClass + 'Include', 'FMX.Controls', 'Empty Include');
end;


procedure TParser.LoadConfigFile(const IniFileName: String);
begin
  VAR Ini := TIniFile.Create(IniFileName);
  try
    _loadConfigFile(Ini);
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
  if Assigned(IniObjectTranslations) then
    for Idx := 0 to IniObjectTranslations.Count - 1 do
    begin
      TransArray := GetArrayFromString(IniObjectTranslations[Idx], '=');
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
  UpdateUses(UsesTranslation);

  Builder := TStringBuilder.Create;
  try
    Builder.Append('USES '+ CRLF);
    for I := 0 to UsesTranslation.Count - 1 do
      if not Trim(UsesTranslation[I]).IsEmpty
      then Builder.Append(' ').Append(UsesTranslation[I]).Append(',');
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
    SetLength(Properties, FPropertyArraySz);
  end;
  if Row > MaxPropertyIndex
  then MaxPropertyIndex := Row;

  Result := Properties[Row];
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
  Properties[Idx] := GetArrayFromString(sData, '=');
  if Length(Properties[Idx]) < 2 then
  begin
    SetLength(Properties[Idx], 2);
    Properties[Idx, 0] := ContinueCode;
    Properties[Idx, 1] := sData;
  end
  else if Properties[Idx, 1] = '<' then
    ReadItems(Properties, Idx, List, LineIndex)
  else if Properties[Idx, 1] = '{' then
    ReadData(Properties, Idx, List, LineIndex)
  else if Properties[Idx, 1].IsEmpty then
    ReadText(Properties, Idx, List, LineIndex);
  Inc(Idx);
end;


function TParser.TransformProperty(ACurrentName, ACurrentValue: String; APad: String = ''): String;
var
  s: String;
begin
  if ACurrentName = ContinueCode
  then Result := ACurrentValue
  else
    if ACurrentName = 'AlignWithMargins'
    then
      if ACurrentValue = 'True'
      then Result := '   Margins.Left = 3.0'  + #13#10 +
                     '   Margins.Top = 3.0'   + #13#10 +
                     '   Margins.Right = 3.0' + #13#10 +
                     '   Margins.Bottom = 3.0'
      else Result := ''
    else
      begin
        s := IniSectionValues.Values[ACurrentName];
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
          if DFMClass = 'TImage' then
            Result := StringReplace(s, '#Class#', ProcessImage(ACurrentValue, APad), [])
          else
          if DFMClass = 'TImageList' then
            Result := StringReplace(s, '#Class#', ProcessImageList(ACurrentValue, APad), [])
        end
        else
          Result := s + ' = ' + ACurrentValue;
      end;
end;

(*  del
procedure TParser.UpdateUses(UsesList: TStrings);
var
  I, Idx: Integer;
begin
  // Replace existing units based on IniReplaceValues (if it exists)
  // if Assigned(IniReplaceValues) then
    for I := 0 to UsesList.Count - 1 do
    begin
      Idx := IniReplaceValues.IndexOfName(UsesList[I]);
      if Idx >= 0 then
        UsesList[I] := IniReplaceValues.ValueFromIndex[Idx];
    end;

  // Remove any empty entries
  for I := UsesList.Count - 1 downto 0 do
    if Trim(UsesList[I]).IsEmpty then
      UsesList.Delete(I);

  // Add new units from IniIncludeValues (if it exists)
  if Assigned(IniIncludeValues) then
    for I := 0 to IniIncludeValues.Count - 1 do
      if UsesList.IndexOf(IniIncludeValues[I]) < 0 then
        UsesList.Add(IniIncludeValues[I]);

  // Handle owned objects recursively (if applicable)
  if Assigned(OwnedObjs) then
    for I := 0 to OwnedObjs.Count - 1 do
      if OwnedObjs[I] is TParser then
        TParser(OwnedObjs[I]).UpdateUsesStringList(UsesList);
end;
*)

procedure TParser.UpdateUses(UsesList: TStrings);
var
  i: Integer;
  UnitName: String;
begin
  // Replace existing units based on IniReplaceValues
  for I := UsesList.Count - 1 downto 0 do
    begin
      UnitName := Trim(UsesList[I]);
      VAR Idx := IniReplaceValues.IndexOfName(UnitName);
      if Idx >= 0
      then
          // Replace with the mapped unit, or remove if mapped to empty
          if IniReplaceValues.ValueFromIndex[Idx].IsEmpty
          then UsesList.Delete(I)
          else UsesList[I] := IniReplaceValues.ValueFromIndex[Idx]
      else
        if UnitName.StartsWith('Vcl.', True)
        then UsesList.Delete(I);     // Remove VCL units not explicitly replaced
    end;

  // Remove any remaining empty entries
  for I := UsesList.Count - 1 downto 0 do
    if Trim(UsesList[I]).IsEmpty
    then UsesList.Delete(I);

  // Add new units from IniIncludeValues, avoiding duplicates
  if Assigned(IniIncludeValues) then
    for I := 0 to IniIncludeValues.Count - 1 do
      if NOT IniIncludeValues[I].IsEmpty
        AND (UsesList.IndexOf(IniIncludeValues[I]) < 0)
        AND NOT SameStr(IniIncludeValues[I], 'Empty Include')
      then UsesList.Add(IniIncludeValues[I]);

  // Handle owned objects recursively
  if Assigned(OwnedObjs) then
    for I := 0 to OwnedObjs.Count - 1 do
      if OwnedObjs[I] is TParser
      then TParser(OwnedObjs[I]).UpdateUses(UsesList);
end;


procedure TParser.WriteFMXToFile(const FmxFileName: String);
var
  FmxBody: String;
begin
  FmxBody := BuildFmxFile;
  if FmxBody.IsEmpty then
    raise Exception.Create('No data for FMX file!');

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
      if obj.DFMClass.Equals('TDBGrid') then
      begin
        // Inicializa
        sFields := EmptyStr;

        // Cria um novo item na lista de grids
        SetLength(FLinkGridList, Succ(Length(FLinkGridList)));

        // Insere o nome da grid
        FLinkGridList[High(FLinkGridList)].GridControl := obj.ObjectName;

        // Passa por todas propriedades da grid
        for J := Low(Properties) to High(Properties) do
        begin
          // Obtem os dados do DataSource
          if obj.Properties[J, 0].Equals('DataSource') then
            FLinkGridList[High(FLinkGridList)].DataSource := obj.Properties[J, 1];

          // Se for as colunas
          if obj.Properties[J, 0].Equals('Columns') then
          begin
            // Obtem os dados dos fields
            sFields := obj.Properties[J, 1];

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
      if obj.DFMClass.Equals('TDBEdit') then
      begin
        // Cria um novo item na lista de dbedits
        SetLength(FLinkControlList, Succ(Length(FLinkControlList)));

        // Insere o nome do dbedit
        FLinkControlList[High(FLinkControlList)].Control := obj.ObjectName;

        // Passa por todas propriedades do dbedit
        for J := Low(Properties) to High(Properties) do
        begin
          // Obtem os dados do DataSource
          if obj.Properties[J, 0].Equals('DataSource') then
            FLinkControlList[High(FLinkControlList)].DataSource := obj.Properties[J, 1];

          // Obtem os dados do field
          if obj.Properties[J, 0].Equals('DataField') then
            FLinkControlList[High(FLinkControlList)].FieldName := GetArrayFromString(obj.Properties[J, 1], '=', True, True)[0];

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


end.

