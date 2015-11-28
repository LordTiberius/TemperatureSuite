unit diagram;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, Graphics, BGRABitmapTypes, Math, DateUtils,
  Measurements;

type
  TOnWorkEvent = procedure(Sender: TObject; Progress: Integer) of object;
  { TDiagram }

  TDiagram = class
  protected
    FMeasurements: TMeasurementEngine;

    FBitmap: TBGRABitmap;
    FCanvas: TCanvas;
    FAutoVertScale: Boolean;
    FHeight: Integer;
    FHorizontalSubGrid: Integer;
    FSensors: array of Integer;
    FColors: array of TColor;

    FVertMin, FVertMax, FVertScale:  Double;
    FVertHighlights: array of Integer;
    FAxisHourThreshold: Integer;

    FOnWork: TOnWorkEvent;

    procedure CreateBitmap;
    procedure DrawBackground;
    procedure DrawAxis;
    procedure DrawGrid;

    procedure CalculateVertScale;
    procedure CalculateVertScaleFactor;

    function GetContentHeight: Integer;

    function isHighlight(VertIndex: Integer): Boolean;
  public
    constructor Create;

    procedure AssignMeasurements(Measurements: TMeasurementEngine);
    procedure SetSensor(Number: Integer; Color: TColor);
    procedure ResetSensors;
    procedure SetCanvas(Canvas: TCanvas);
    procedure SetHeight(Height: Integer);
    procedure RegisterHighlight(Highlight: Integer);

    function Width: Integer;

    property AutoVertScale: Boolean read FAutoVertScale write FAutoVertScale;
    property AxisHourThreshold: Integer read FAxisHourThreshold write
      FAxisHourThreshold;
    property HorizontalSubGrid: Integer read FHorizontalSubGrid
      write FHorizontalSubGrid;
    property VerticalMinValue: Double read FVertMin write FVertMin;
    property VerticalMaxValue: Double read FVertMax write FVertMax;

    property OnWork: TOnWorkEvent read FOnWork write FOnWork;

    procedure Draw;
    procedure CanvasResize;
  end;

implementation

{ TDiagram }

procedure TDiagram.CreateBitmap;
var
  Samples, Sensors: Integer;
  Lightened: TBGRAPixel;
begin
  FBitmap.Free;
  FBitmap := TBGRABitmap.Create(FMeasurements.MeasurementCount*2+50, FHeight);

  DrawBackground;
  if FAutoVertScale then
    CalculateVertScale
  else
    CalculateVertScaleFactor;
  DrawAxis;
  DrawGrid;

  for Samples := 0 to FMeasurements.MeasurementCount-2 do
  begin
    if Assigned(FOnWork) then
      FOnWork(Self, Round((Samples/(FMeasurements.MeasurementCount-2))*100));
    for Sensors := 0 to High(FSensors) do
    begin
      Lightened := ColorToBGRA(FColors[Sensors], 50);
      FBitmap.DrawLineAntialias(41+Samples*2, FHeight-40-((
        FMeasurements[Samples].MeasurementValues[FSensors[Sensors]].min-
        FVertMin)*FVertScale), 41+Samples*2, FHeight-40-((
        FMeasurements[Samples].MeasurementValues[FSensors[Sensors]].max-
        FVertMin)*FVertScale), Lightened, 2, True);
      FBitmap.DrawLineAntialias(41+(Samples+1)*2, FHeight-40-((
        FMeasurements[Samples+1].MeasurementValues[FSensors[Sensors]].min-FVertMin)*
        FVertScale), 41+(Samples+1)*2, FHeight-40-((FMeasurements[Samples+1].
        MeasurementValues[FSensors[Sensors]].max-FVertMin)*FVertScale), Lightened, 2,
        True);

      FBitmap.DrawLineAntialias(41+(Samples*2), FHeight-40-((
        FMeasurements[Samples].MeasurementValues[FSensors[Sensors]].mean-
        FVertMin)*FVertScale), 41+((Samples+1)*2), FHeight-40-((
        FMeasurements[Samples+1].MeasurementValues[FSensors[Sensors]].mean-
        FVertMin)*FVertScale), ColorToBGRA(FColors[Sensors]), 2, True);
    end;
  end;
end;

procedure TDiagram.DrawBackground;
begin
  FBitmap.FloodFill(0, 0, ColorToBGRA($00424242), fmSet);
end;

procedure TDiagram.DrawAxis;
var
  Loop, intHours: Integer;
  LastYLabel: Double;
  First: Boolean;
  LastDate: String;
begin
  // Vertical
  FBitmap.DrawLineAntialias(40, FHeight-40, 40, 10, ColorToBGRA(clWhite),
    1, True);
  LastYLabel := 0;

  FBitmap.FontHeight := 10;

  for Loop := Round(FVertMin) to Round(FVertMax) do
  begin
    if Loop > Round(FVertMin) then
    begin
      if LastYLabel-(FHeight-46-(Loop-FVertMin)*FVertScale) > 10 then
      begin
        FBitmap.DrawLineAntialias(40, FHeight-40-(Loop-FVertMin)*FVertScale,
          35, FHeight-40-(Loop-FVertMin)*FVertScale, ColorToBGRA(clWhite), 1, True);
        FBitmap.TextOut(34, FHeight-46-(Loop-FVertMin)*FVertScale,
          IntToStr(Loop), ColorToBGRA(clWhite), taRightJustify);
        LastYLabel := FHeight-46-(Loop-FVertMin)*FVertScale;
      end
      else
        FBitmap.DrawLineAntialias(40, FHeight-40-(Loop-FVertMin)*FVertScale,
          37, FHeight-40-(Loop-FVertMin)*FVertScale, ColorToBGRA(clWhite), 1, True);
    end
    else
    begin
      FBitmap.DrawLineAntialias(40, FHeight-40-(Loop-FVertMin)*FVertScale,
        35, FHeight-40-(Loop-FVertMin)*FVertScale, ColorToBGRA(clWhite), 1, True);
      FBitmap.TextOut(34, FHeight-46-(Loop-FVertMin)*FVertScale,
        IntToStr(Loop), ColorToBGRA(clWhite), taRightJustify);
      LastYLabel := FHeight-46-(Loop-FVertMin)*FVertScale;
    end;
  end;

  // Horizontal

  FBitmap.DrawLineAntialias(40, FHeight-40, 40+FMeasurements.MeasurementCount*2,
    FHeight-40, ColorToBGRA(clWHite), 1, True);

  First := True;
  intHours := 0;
  for Loop := 0 to FMeasurements.MeasurementCount-1 do
  begin
    if MinuteOf(UnixToDateTime(FMeasurements[Loop].Timestamp)) = 0 then
    begin
      if intHours = 0 then
      begin
        FBitmap.TextOut(41+(Loop*2), FHeight-25, FormatDateTime('hh:nn',
          UnixToDateTime(FMeasurements[Loop].Timestamp)), ColorToBGRA(clWhite),
          taCenter);
      end;

      Inc(intHours);

      if intHours = AxisHourThreshold then
        intHours := 0;

      if First then
      begin
        FBitmap.TextOut(41+(Loop*2), FHeight-12, FormatDateTime('DD.MM.YYYY',
          UnixToDateTime(FMeasurements[Loop].Timestamp)), ColorToBGRA(clWhite),
          taCenter);

        LastDate := FormatDateTime('DD.MM.YYYY',
          UnixToDateTime(FMeasurements[Loop].Timestamp));

        First := False;
      end
      else
      begin
        if FormatDateTime('DD.MM.YYYY',
          UnixToDateTime(FMeasurements[Loop].Timestamp)) <> LastDate then
        begin
          FBitmap.TextOut(41+(Loop*2), FHeight-12, FormatDateTime('DD.MM.YYYY',
            UnixToDateTime(FMeasurements[Loop].Timestamp)), ColorToBGRA(clWhite),
            taCenter);

          LastDate := FormatDateTime('DD.MM.YYYY',
          UnixToDateTime(FMeasurements[Loop].Timestamp));
        end;
      end;
    end;
  end;
end;

procedure TDiagram.DrawGrid;
var
  Loop: Integer;
begin
  for Loop := Round(FVertMin) to Round(FVertMax) do
  begin
    if isHighlight(Loop) then
      FBitmap.DrawLineAntialias(40, FHeight-40-(Loop-FVertMin)*FVertScale,
      Width, FHeight-40-(Loop-FVertMin)*FVertScale, ColorToBGRA(clWhite, 127), 3, True)
    else
      FBitmap.DrawLineAntialias(40, FHeight-40-(Loop-FVertMin)*FVertScale,
        Width, FHeight-40-(Loop-FVertMin)*FVertScale, ColorToBGRA(clWhite, 127), 1, True);
  end;

  for Loop := 0 to FMeasurements.MeasurementCount-1 do
  begin
    if MinuteOf(UnixToDateTime(FMeasurements[Loop].Timestamp)) mod
      HorizontalSubGrid <> 0 then
      Continue;

    if MinuteOf(UnixToDateTime(FMeasurements[Loop].Timestamp)) = 0 then
    begin
      FBitmap.DrawLineAntialias(41+(Loop*2), FHeight-40,
        41+(Loop*2), 10, ColorToBGRA(clWhite), 1, True);
    end
    else
    begin
      FBitmap.DrawLineAntialias(41+(Loop*2), FHeight-40,
        41+(Loop*2), 10, ColorToBGRA(clWhite, 127), 1, True);
    end;
  end;
end;

procedure TDiagram.CalculateVertScale;
var
  Samples, Sensors: Integer;
begin
  FVertMin := 10000;
  FVertMax := -10000;

  for Samples := 0 to FMeasurements.MeasurementCount-1 do
  begin
    for Sensors := 0 to High(FSensors) do
    begin
      if FMeasurements.Measurements[Samples].
        MeasurementValues[FSensors[Sensors]].Min < FVertMin then
      begin
        FVertMin := FMeasurements.Measurements[Samples].
          MeasurementValues[FSensors[Sensors]].Min;
      end;

      if FMeasurements.Measurements[Samples].
        MeasurementValues[FSensors[Sensors]].Max > FVertMax then
      begin
        FVertMax := FMeasurements.Measurements[Samples].
          MeasurementValues[FSensors[Sensors]].Max;
      end;
    end;
  end;

  FVertMin := Floor(FVertMin);
  FVertMax := Ceil(FVertMax);

  CalculateVertScaleFactor;
end;

procedure TDiagram.CalculateVertScaleFactor;
begin
  FVertScale := GetContentHeight / (Abs(FVertMax)-Abs(FVertMin));
end;

function TDiagram.GetContentHeight: Integer;
begin
  Result := FHeight - 50;
end;

function TDiagram.isHighlight(VertIndex: Integer): Boolean;
var
  Loop: Integer;
begin
  Result := False;

  for Loop := 0 to High(FVertHighlights) do
    if FVertHighlights[Loop] = VertIndex then
    begin
      Result := True;
      Break;
    end;
end;

constructor TDiagram.Create;
begin
  FBitmap := TBGRABitmap.Create;
  FAutoVertScale := True;
  FHorizontalSubGrid := 10;
end;

procedure TDiagram.AssignMeasurements(Measurements: TMeasurementEngine);
begin
  FMeasurements := Measurements;
end;

procedure TDiagram.SetSensor(Number: Integer; Color: TColor);
begin
  SetLength(FSensors, Length(FSensors)+1);
  SetLength(FColors, Length(FColors)+1);
  FSensors[High(FSensors)] := Number;
  FColors[High(FColors)] := Color;
end;

procedure TDiagram.ResetSensors;
begin
  SetLength(FSensors, 0);
end;


procedure TDiagram.SetCanvas(Canvas: TCanvas);
begin
  FCanvas := Canvas;
end;

procedure TDiagram.SetHeight(Height: Integer);
begin
  FHeight := Height;
end;

procedure TDiagram.RegisterHighlight(Highlight: Integer);
begin
  SetLength(FVertHighlights, Length(FVertHighlights)+1);
  FVertHighlights[High(FVertHighlights)] := Highlight;
end;

function TDiagram.Width: Integer;
begin
  Result := 50 + FMeasurements.MeasurementCount*2;
end;

procedure TDiagram.Draw;
begin
  FBitmap.Draw(FCanvas, 0, 0);
end;

procedure TDiagram.CanvasResize;
begin
  CreateBitmap;
  //Draw;
end;

end.

