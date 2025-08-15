program Light_VCL2FMX;

uses
  FastMM4,
  System.StartUpCopy,
  FMX.Forms,
  LightFmx.Common.AppData,
  Parser in 'Parser.pas',
  FormMain in 'FormMain.pas' {frmMain},
  Utils in 'Utils.pas',
  FormConfig in 'FormConfig.pas' {frmConfig},
  ParseImage in 'ParseImage.pas',
  ParseImageList in 'ParseImageList.pas',
  LightCore.INIFile in '..\LightSaber\LightCore.INIFile.pas',
  LightCore.IO in '..\LightSaber\LightCore.IO.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := FALSE;

  AppData:= TAppData.Create('Light Vcl2Fmx');
  AppData.CreateMainForm(TfrmMain, frmMain, TRUE);       // Main form
  AppData.Run;
end.



