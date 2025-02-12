UNIT FormMain;

{=============================================================================================================
   Gabriel Moraru
   2025.01
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
  System.Variants,
  System.Win.Registry,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.Memo,
  Parser,
  FMX.StdCtrls,
  FMX.ScrollBox,
  FMX.Controls.Presentation,
  FMX.Objects, FMX.Memo.Types, FMX.Menus;

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
    DFMObj: TParser;
    ConfigDict: String;
    InputPAS: String;
    InputDFM: String;
    OutputPas: String;
    OutputFMX: String;
    function GetRegFile: TRegistryIniFile;
    Procedure RegIniLoad;
    Procedure RegIniSave;
    Procedure UpdateForm;
    procedure LoadFile(aFile: string);
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
  cbAppDataFmx,
  FormConfig,
  FMX.DialogService;



procedure TfrmMain.FormCreate(Sender: TObject);
begin
  RegIniLoad;
  UpdateForm;
end;


procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  RegIniSave;
  FreeAndNil(DFMObj);
end;








procedure TfrmMain.btnConfigClick(Sender: TObject);
begin
  TFrmConfig.Create(Self).ShowModal;
  RegIniLoad;
end;




procedure TfrmMain.LoadFile(aFile: string);
begin
  mmoInputDfm.Lines.LoadFromFile(aFile);
  BtnProcess.Enabled:= TRUE;
  UpdateForm;
end;


procedure TfrmMain.BtnOpenFileClick(Sender: TObject);
begin
  BtnProcess.Enabled:= False;
  FreeAndNil(DFMObj);

  if InputDFM <> ''
  then dlgOpen.InitialDir := ExtractFileDir(InputDFM);
  if dlgOpen.Execute then
  begin
    InputPAS := ChangeFileExt(dlgOpen.FileName, '.pas');
    InputDFM := ChangeFileExt(InputPAS, '.dfm');

    if FileExists(InputDFM) then
      if TParser.IsTextDFM(InputDFM)
      then LoadFile(InputDFM)
      else ShowMessage('Incompatible DFM file:' + InputDFM);
  end;

  UpdateForm;

  if AppData.RunningFirstTime
  then ExecuteURL('https://gabrielmoraru.com/');
end;


procedure TfrmMain.BtnProcessClick(Sender: TObject);
var
  DfmContent: String;
  StringList: TStringList;
  LineIndex: Integer;
begin
  if mmoInputDfm.Text <> '' then
  begin
    FreeAndNil(DFMObj);
    DfmContent := mmoInputDfm.Text;
    StringList := StringFromFileTSL(InputDFM);
    try
      // Read the first line to check if it contains 'object'
      LineIndex := 0;
      if StringList.Count > 0 then
      begin
        DfmContent := Trim(StringList[LineIndex]);
        if Pos('object', DfmContent) = 1 then
          DFMObj := TParser.Create(DfmContent, StringList, 0, LineIndex);
      end;
    finally
      StringList.Free;
    end;
  end;
  if Assigned(DFMObj) then
  begin
    DFMObj.LiveBindings;
    DFMObj.LoadInfileDefs(ConfigDict);
    mmOutput.Text := '';
    mmOutput.Text := DFMObj.FMXFile;
    BtnProcess.Enabled := False;
    UpdateForm;
  end;
end;






procedure TfrmMain.BtnSaveFMXClick(Sender: TObject);
begin
  if DFMObj = nil
  then UpdateForm
  else
   begin
     OutputPas := ExtractFilePath(OutputPas) + ChangeFileExt(ExtractFileName(InputDFM), 'FMX.pas');

     if not OutputPas.IsEmpty then
     begin
       dlgSalvar.InitialDir := ExtractFileDir(OutputPas);
       dlgSalvar.FileName   := ExtractFileName(ChangeFileExt(OutputPas, '.fmx'));
     end;

     if dlgSalvar.Execute then
     begin
       OutputPas := ChangeFileExt(dlgSalvar.FileName, '.pas');
       OutputFMX := ChangeFileExt(OutputPas, '.fmx');

       if FileExists(OutputFMX) or FileExists(OutputPas) then
         if myMessageDialog('Replace Existing Files: '+ OutputFMX +' - '+ OutputPas,
           TMsgDlgType.mtWarning,
           [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbCancel],
           TMsgDlgBtn.mbOK) = mrOk then
         begin
           DeleteFile(OutputFMX);
           DeleteFile(OutputPas);
         end;

       if FileExists(OutputFMX) then
         raise Exception.Create(OutputFMX + 'File already exists! '+ OutputFMX);

       DFMObj.WriteFMXToFile(OutputFMX);

       if FileExists(OutputPas) then
         raise Exception.Create(OutputPas + 'File already exists! '+ OutputPas);

       DFMObj.WritePasToFile(OutputPas, InputPAS);
     end;
   end;
end;






function TfrmMain.GetRegFile: TRegistryIniFile;
begin
  Result:= TRegistryIniFile.Create(RegKey);
end;


procedure TfrmMain.RegIniLoad;
Var
  RegFile: TRegistryIniFile;
begin
  RegFile := GetRegFile;
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


procedure TfrmMain.RegIniSave;
Var
  RegFile: TRegistryIniFile;
begin
  RegFile := GetRegFile;
  try
    RegFile.WriteString('Files', 'ConfigFile', ConfigDict);
    RegFile.WriteString('Files', 'InputDFm'  , InputDFM);
    RegFile.WriteString('Files', 'OutputPas' , OutputPas);
  finally
    FreeAndNil(RegFile);
  end;
end;


procedure TfrmMain.UpdateForm;
begin
  BtnSaveFMX.Visible := DFMObj <> nil;
end;


end.
