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

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Win.Registry,

  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.Memo,
  FMX.StdCtrls,
  FMX.ScrollBox,
  FMX.Controls.Presentation,
  FMX.Memo.Types,
  FMX.DialogService,

  Parser;

type
  TfrmMain = class(TForm)
    mmOutput: TMemo;
    mmoInputDfm: TMemo;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    Layout1: TLayout;
    layTop: TLayout;
    btnConfig: TButton;
    btnOpenFile: TButton;
    btnProcess: TButton;
    btnSave: TButton;
    procedure btnOpenFileClick(Sender: TObject);
    procedure btnProcessClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnConfigClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    DfmParser: TParser;
    ConfigFile: String;

    InputPasFile : String;
    InputDfmFile : String;

    Procedure LoadRegistrySettings;
    Procedure SaveRegistrySettings;
    Procedure UpdateSaveButtonVisibility;
    procedure LoadFile(aFileName: string);
  end;

CONST RegKey= 'Vcl2Dfm';

IMPLEMENTATION
{$R *.fmx}

USES
  Utils,
  ccTextFile,
  ccCore,
  ccIO,
  LightFmx.DialogsDesktop,
  LightFMX.AppData,
  FormConfig;



procedure TfrmMain.FormCreate(Sender: TObject);
begin
  LoadRegistrySettings;
  UpdateSaveButtonVisibility;
end;


procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveRegistrySettings;
  FreeAndNil(DfmParser);
end;





procedure TfrmMain.LoadFile(aFileName: string);
begin
  if FileExists(aFileName) then
    if TParser.IsTextDFM(aFileName)
    then
      begin
        FreeAndNil(DfmParser);
        //BtnProcess.Enabled:= False;

        InputDfmFile := aFileName;
        InputPasFile := ChangeFileExt(InputDfmFile, '.pas');

        Caption  := InputDfmFile;
        mmoInputDfm.Lines.LoadFromFile(InputDfmFile);
        BtnProcess.Enabled:= TRUE;
      end
    else
      ShowMessage('Binary DFM file not supported!');

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
        if Pos('object', DfmLine) = 1 then
         begin
           DfmParser := TParser.Create(DfmLine, DfmBody, 0, LineIndex);
           DfmParser.LiveBindings;
           DfmParser.LoadInfileDefs(ConfigFile);

           // Show output
           mmOutput.Text := DfmParser.BuildFmxFile;
           BtnProcess.Enabled := False;
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
begin
  if DfmParser = nil then
  begin
    UpdateSaveButtonVisibility;
    Exit;
  end;

  OutputPas := AppendToFileName(InputPasFile, '_FMX');
  OutputFMX := ChangeFileExt(OutputPas, '.fmx');

  if AskToOverwrite and (FileExists(OutputFMX) or FileExists(OutputPas)) then
    if MesajYesNo('Replace Existing Files?' + CRLF + OutputFMX + ' - ' + OutputPas) then
      begin
        DeleteFile(OutputFMX);
        DeleteFile(OutputPas);
        DfmParser.WriteFMXToFile(OutputFMX);
        DfmParser.WritePasToFile(OutputPas, InputPasFile);
      end;
end;


// Loads registry settings into form fields
procedure TfrmMain.LoadRegistrySettings;
Var
  RegFile: TRegistryIniFile;
begin
  RegFile := TRegistryIniFile.Create(RegKey);
  try
    ConfigFile    := RegFile.ReadString('Files', 'ConfigFile', '');
    InputDfmFile  := RegFile.ReadString('Files', 'InputDfm', '');

    if InputDfmFile <> ''
    then InputPasFile := ChangeFileExt(InputDfmFile, '.pas');

    if FileExists(InputDfmFile)
    AND TParser.IsTextDFM(InputDfmFile)
    then mmoInputDfm.Lines.LoadFromFile(InputDfmFile);
  finally
    FreeAndNil(RegFile);
  end;

  if NOT FileExists(ConfigFile)
  then ConfigFile := AppData.CurFolder+ 'ConversionDict.ini';
end;


// Opens the configuration form modally and reloads settings
procedure TfrmMain.btnConfigClick(Sender: TObject);
begin
  TFrmConfig.Create(Self).ShowModal;
  LoadRegistrySettings;
end;


// Saves current settings to the registry
procedure TfrmMain.SaveRegistrySettings;
Var
  RegFile: TRegistryIniFile;
begin
  RegFile := TRegistryIniFile.Create(RegKey);
  try
    RegFile.WriteString('Files', 'ConfigFile', ConfigFile);
    RegFile.WriteString('Files', 'InputDFm'  , InputDfmFile);
  finally
    FreeAndNil(RegFile);
  end;
end;


// Updates the visibility of the save button based on whether the parser is initialized
procedure TfrmMain.UpdateSaveButtonVisibility;
begin
  btnSave.Visible := DfmParser <> nil;
end;


end.
