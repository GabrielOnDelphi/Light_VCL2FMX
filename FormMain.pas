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
  //System.Variants,
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
    dlgOpen: TOpenDialog;
    dlgSalvar: TSaveDialog;
    Layout1: TLayout;
    Layout2: TLayout;
    btnConfig: TButton;
    BtnOpenFile: TButton;
    BtnProcess: TButton;
    BtnSaveFMX: TButton;
    procedure BtnOpenFileClick(Sender: TObject);
    procedure BtnProcessClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtnSaveFMXClick(Sender: TObject);
    procedure btnConfigClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    DfmParser: TParser;
    ConfigDict: String;

    InputPAS : String;
    InputDFM : String;

    OutputPas: String;      // Filename of the output file
    OutputFMX: String;      // Filename of the output file

    Procedure RegIniLoad;
    Procedure RegIniSave;
    Procedure ShowSaveButton;
    procedure LoadFile(aFileName: string);
  end;

VAR
  frmMain: TfrmMain;

CONST RegKey= 'Vcl2Dfm';

IMPLEMENTATION
{$R *.fmx}

USES
  Utils,
  ccTextFile,
  ccCore,
  ccIO,

  cbAppDataFmx,
  FormConfig;



procedure TfrmMain.FormCreate(Sender: TObject);
begin
  RegIniLoad;
  ShowSaveButton;
end;


procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  RegIniSave;
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

        InputDFM := aFileName;
        InputPAS := ChangeFileExt(InputDFM, '.pas');

        Caption  := InputDFM;
        mmoInputDfm.Lines.LoadFromFile(InputDFM);
        BtnProcess.Enabled:= TRUE;
      end
    else
      ShowMessage('Binary DFM file not supported!');

  ShowSaveButton;
end;


procedure TfrmMain.BtnOpenFileClick(Sender: TObject);
begin
  if InputDFM <> ''
  then dlgOpen.InitialDir := ExtractFileDir(InputDFM);

  if dlgOpen.Execute
  then LoadFile(dlgOpen.FileName);
end;


procedure TfrmMain.BtnProcessClick(Sender: TObject);
var
  DfmLine: String;
  DfmBody: TStringList;
  LineIndex: Integer;
begin
  if mmoInputDfm.Text <> '' then
  begin
    FreeAndNil(DfmParser);
    DfmBody := StringFromFileTSL(InputDFM);
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
           DfmParser.LoadInfileDefs(ConfigDict);

           // Show output
           mmOutput.Text := DfmParser.BuildFmxFile;
           BtnProcess.Enabled := False;
           ShowSaveButton;
         end;
      end;
    finally
      DfmBody.Free;
    end;
  end;
end;


procedure TfrmMain.BtnSaveFMXClick(Sender: TObject);
const
   AskToOverwrite: boolean= false;
begin
  if DfmParser = nil
  then ShowSaveButton
  else
   begin
     OutputPas:= AppendToFileName(InputPAS, '_FMX');  // del ExtractFilePath(OutputPas) + ChangeFileExt(ExtractFileName(InputDFM), 'FMX.pas');
     {
     if OutputPas <> '' then
     begin
       dlgSalvar.InitialDir := ExtractFileDir(OutputPas);
       dlgSalvar.FileName   := ExtractFileName(ChangeFileExt(OutputPas, '.fmx'));
     end; }

     //OutputPas := ChangeFileExt(dlgSalvar.FileName, '.pas');
     OutputFMX := ChangeFileExt(OutputPas, '.fmx');

     if AskToOverwrite then
       if FileExists(OutputFMX) or FileExists(OutputPas) then
         if myMessageDialog('Replace Existing Files? '+ CRLF+ OutputFMX +' - '+ OutputPas, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbCancel], TMsgDlgBtn.mbOK) = mrOk then
         begin
           DeleteFile(OutputFMX);
           DeleteFile(OutputPas);
         end;

     DfmParser.WriteFMXToFile(OutputFMX);
     DfmParser.WritePasToFile(OutputPas, InputPAS);
   end;
end;







procedure TfrmMain.RegIniLoad;
Var
  RegFile: TRegistryIniFile;
begin
  RegFile := TRegistryIniFile.Create(RegKey);
  try
    ConfigDict:= RegFile.ReadString('Files', 'ConfigFile',   '');
    InputDFM  := RegFile.ReadString('Files', 'InputDfm',  '');
    OutputPas := RegFile.ReadString('Files', 'OutputPas', '');

    if InputDFM <> ''
    then InputPAS := ChangeFileExt(InputDFM, '.pas');

    if OutputPas <> '' then
    begin
      OutputFMX := ChangeFileExt(OutputPas, '.fmx');
      dlgOpen.InitialDir := ExtractFileDir(OutputPas);
    end;

    if FileExists(InputDFM)
    AND TParser.IsTextDFM(InputDFM)
    then mmoInputDfm.Lines.LoadFromFile(InputDFM);
  finally
    FreeAndNil(RegFile);
  end;

  if NOT FileExists(ConfigDict)
  then ConfigDict := AppData.CurFolder+ 'ConversionDict.ini';
end;


procedure TfrmMain.btnConfigClick(Sender: TObject);
begin
  TFrmConfig.Create(Self).ShowModal;
  RegIniLoad;
end;


procedure TfrmMain.RegIniSave;
Var
  RegFile: TRegistryIniFile;
begin
  RegFile := TRegistryIniFile.Create(RegKey);
  try
    RegFile.WriteString('Files', 'ConfigFile', ConfigDict);
    RegFile.WriteString('Files', 'InputDFm'  , InputDFM);
    RegFile.WriteString('Files', 'OutputPas' , OutputPas);
  finally
    FreeAndNil(RegFile);
  end;
end;


procedure TfrmMain.ShowSaveButton;
begin
  BtnSaveFMX.Visible := DfmParser <> nil;
end;


end.
