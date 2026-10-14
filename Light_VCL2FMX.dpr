program Light_VCL2FMX;

uses
  FastMM4,
  System.StartUpCopy,
  FMX.Forms,
  LightCore.AppData,
  LightFmx.Common.AppData,
  Parser in 'Parser.pas',
  FormMain in 'FormMain.pas' {frmMain},
  Utils in 'Utils.pas',
  FormConfig in 'FormConfig.pas' {frmConfig},
  ParseImage in 'ParseImage.pas',
  ParseImageList in 'ParseImageList.pas',
  {$IFDEF AUTOPILOT}Autopilot.Bridge.Fmx in 'c:\Projects\Projects AI\Autopilot for Delphi\Source\Bridge\Autopilot.Bridge.Fmx.pas',{$ENDIF}{Optional}
  LightCore.INIFile,
  LightCore.IO;

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := FALSE;

  AppData:= TAppData.Create('Light Vcl2Fmx');
  AppData.CreateMainForm(TfrmMain, frmMain, asFull);    // Main form: save position + GUI controls

  { Optional: Via Autopilot, Claude can run/debug/test this program for you.
    For example, open a .dfm with "Open DFM", press "Process" & check the FMX code in mmOutput.
    Don't have AUTOPILOT? Undefine $AUTOPILOT or get it from www.GabrielMoraru.com }
  {$IFDEF AUTOPILOT}
   Autopilot.Bridge.Fmx.StartBridge;
  {$ENDIF}
  AppData.Run;
end.



