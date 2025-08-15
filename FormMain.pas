UNIT FormMain;

{=============================================================================================================
   Gabriel Moraru
   2025.04
   Convert VCL to FMX
--------------------------------------------------------------------------------------------------------------
   Code branched from edelphi/vcltofmx
   This is the major update! It has:
     True unicode support!
     Fixed bugs
     Better file handling
     Smaller code
     English instead of spanish

   This program requires https://github.com/GabrielOnDelphi/Delphi-LightSaber
=============================================================================================================}

// Accept drag and drop of DFM files into the GUI

INTERFACE

USES
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Win.Registry,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Memo, FMX.StdCtrls,
  FMX.ScrollBox, FMX.Controls.Presentation, FMX.Memo.Types, FMX.DialogService,
  LightFmx.Common.AppData.Form, Parser;

TYPE
  TfrmMain = class(TLightForm)
    btnDictionary: TButton;
    btnOpenFile: TButton;
    btnProcess: TButton;
    btnSave: TButton;
    Layout1: TLayout;
    layTop: TLayout;
    mmoInputDfm: TMemo;
    mmOutput: TMemo;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    Splitter1: TSplitter;
    procedure btnOpenFileClick(Sender: TObject);
    procedure btnProcessClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnDictionaryClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    DfmParser: TParser;
    DictionaryFile: String;

    InputPasFile : String;
    InputDfmFile : String;

    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure UpdateSaveButtonVisibility;
    procedure LoadFile(aFileName: string);
   public
    procedure FormPostInitialize; override;
    procedure FormPreRelease;     override;
  end;

CONST RegKey= 'Light Vcl2Dfm';

VAR frmMain: TfrmMain;

IMPLEMENTATION
{$R *.fmx}

USES
  Utils, FormConfig,
  LightCore.INIFile,
  LightCore.AppData,
  LightCore.TextFile,
  LightCore,
  LightCore.IO,
  LightFmx.Common.DialogsBlocking, LightFmx.Common.AppData;



procedure TfrmMain.FormPostInitialize;
begin
  AutoState:= asFull;  // Must set it before inherited!
  inherited FormPostInitialize;
  LoadSettings;
end;

procedure TfrmMain.FormPreRelease;
begin
  // Free stuff here
  inherited FormPreRelease;
end;


procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
  FreeAndNil(DfmParser);
end;


procedure TfrmMain.LoadFile(aFileName: string);
begin
  if NOT FileExists(aFileName) then EXIT;

  // Automatically "convert" wrong inputed PAS files to DFM.
  if IsThisType(aFileName, 'PAS') then
   begin
     aFileName:= ForceExtension(aFileName, 'DFM');
   end;

  // DFM received?
  if NOT IsThisType(aFileName, 'DFM') then
    begin
      MessageError('Only DFM files accepted. Received:'+ CRLF+ aFileName);
      EXIT;
    end;

  // Is binary DFM?
  if NOT TParser.IsTextDFM(aFileName) then
    begin
      ShowMessage('Binary DFM file not supported!');
      EXIT;
    end;

  InputDfmFile := aFileName;
  InputPasFile := ChangeFileExt(InputDfmFile, '.pas');

  Caption  := InputDfmFile;
  BtnProcess.Enabled:= TRUE;
  mmoInputDfm.Lines.LoadFromFile(InputDfmFile);
  mmOutput.Lines.Clear;

  FreeAndNil(DfmParser);
  UpdateSaveButtonVisibility;
end;


procedure TfrmMain.btnOpenFileClick(Sender: TObject);
begin
  if InputDfmFile <> ''
  then OpenDialog.InitialDir := ExtractFileDir(InputDfmFile);

  if OpenDialog.Execute
  then LoadFile(OpenDialog.FileName);
end;


procedure TfrmMain.btnProcessClick(Sender: TObject);
var
  DfmLine: String;
  DfmBody: TStringList;
  LineIndex: Integer;
begin
  if mmoInputDfm.Text <> '' then
  begin
    FreeAndNil(DfmParser);
    DfmBody := StringFromFileTSL(InputDfmFile);
    try
      if DfmBody.Count > 0 then
      begin
        LineIndex := 0;
        DfmLine := Trim(DfmBody[LineIndex]);

        // Read the first line to check if it contains 'object'
        if PosInsensitive('object', DfmLine) < 10 then
         begin
           DfmParser := TParser.Create(DfmLine, DfmBody, 0, LineIndex);
           DfmParser.LiveBindings;
           DfmParser.LoadConfigFile(DictionaryFile);

           mmOutput.Text := DfmParser.BuildFmxFile; // Show output
                      //BtnProcess.Enabled := False;
           UpdateSaveButtonVisibility;
         end;
      end;
    finally
      FreeAndNil(DfmBody);
    end;
  end;
end;


procedure TfrmMain.btnSaveClick(Sender: TObject);
CONST
  AskToOverwrite: boolean = false;
VAR
  OutputPas: String;      // Filename of the output file
  OutputFMX: String;      // Filename of the output file

  procedure Save;
  begin
    DeleteFile(OutputFMX);
    DeleteFile(OutputPas);
    DfmParser.WriteFMXToFile(OutputFMX);
    DfmParser.WritePasToFile(OutputPas, InputPasFile);
  end;

begin
  if DfmParser <> nil then
    begin
      OutputPas := AppendToFileName(InputPasFile, '_FMX');
      OutputFMX := ChangeFileExt(OutputPas, '.fmx');

      if AskToOverwrite
      AND (FileExists(OutputFMX) or FileExists(OutputPas))
      then
        if MessageYesNo('Replace Existing Files?' + CRLF + OutputFMX + ' - ' + OutputPas)
        then Save
        else
      else Save;
    end;

 UpdateSaveButtonVisibility;
end;


procedure TfrmMain.btnDictionaryClick(Sender: TObject);
begin
  TFrmConfig.Create(Self).ShowModal;
  LoadSettings;
end;


procedure TfrmMain.LoadSettings;  //ToDo: move this to app's INI file!
begin
  VAR Reg := TRegistryIniFile.Create(RegKey);
  try
    DictionaryFile:= Reg.ReadString('Files', 'ConfigFile', '');
    if NOT FileExists(DictionaryFile)
    then DictionaryFile := AppData.ExeFolder+ 'ConversionDict.ini';

    InputDfmFile  := Reg.ReadString('Files', 'InputDfm', '');
    LoadFile(InputDfmFile);
  finally
    FreeAndNil(Reg);
  end;
end;


procedure TfrmMain.SaveSettings;
begin
  VAR Reg := TRegistryIniFile.Create(RegKey);
  try
    Reg.WriteString('Files', 'ConfigFile', DictionaryFile);
    Reg.WriteString('Files', 'InputDFm'  , InputDfmFile);
  finally
    FreeAndNil(Reg);
  end;
end;


// Updates the visibility of the save button based on whether the parser is initialized
procedure TfrmMain.UpdateSaveButtonVisibility;
begin
  btnSave.Visible := DfmParser <> nil;
end;


end.
