unit measurements;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Dialogs;

type
  TOnWorkEvent = procedure(Sender: TObject; Progress: Integer) of object;

  TMeasurementValue = record
    Min, Max, Mean: Double;
    case hasNumberOfMeasurements: Boolean of
      True: (NumberOfMeasurements: Byte;);
      False: ();
  end;

  TMeasurement = record
    Timestamp: Int64;
    MeasurementValues: array of TMeasurementValue;
  end;
  PMeasurement = ^TMeasurement;

  TMeasurementCollection = array of TMeasurement;
  PMeasurementCollection = ^TMeasurementCollection;

  TOutputMode = (omSource, omRecalculated);

  { TMeasurementEngine }

  TMeasurementEngine = class
  private
    function GetMeasurement(Index: Integer): TMeasurement;
    function GetMeasurementCount: Integer;
    procedure SetMeasurement(Index: Integer; AValue: TMeasurement);
  protected
    FSourceMeasurements, FRecalculatedMeasurements: TMeasurementCollection;
    FOutputMode: TOutputMode;
    FRecalculateMinutes: Integer;
    FRecalculateAlign: Boolean;
    FSensorCount: Integer;
    FMaxSecondsCorrected: Integer;
    FOnWork: TOnWorkEvent;
    procedure Clear;
    procedure ClearSourceMeasurements;
    procedure ClearRecalculatedMeasurements;
    function DoCorrectTimestamp(Timestamp: Int64): Int64;
    function GetCollectionPointer: PMeasurementCollection;
    procedure CopySourceWithinBounds(StartTimestamp, EndTimestamp: Int64);
  public
    property Measurements[Index: Integer]: TMeasurement read GetMeasurement
      write SetMeasurement; default;
    property MaxSecondsCorrected: Integer read FMaxSecondsCorrected;
    property MeasurementCount: Integer read GetMeasurementCount;
    property OnWork: TOnWorkEvent read FOnWork write FOnWork;
    property SensorCount: Integer read FSensorCount;


    constructor Create;
    function LoadFromFile(Filename: String; CorrectTimestamps:
      Boolean = True): Boolean;

    procedure Recalculate(Minutes: Integer; Align: Boolean = True;
      StartTimestamp: Int64 = -1; EndTimestamp: Int64 = -1);
    procedure SetSegment(LastHours: Integer);
    procedure SetSegment(StartTimestamp, EndTimestamp: Int64);
    procedure Reset;
  end;

implementation

{ TMeasurementEngine }

function TMeasurementEngine.GetMeasurement(Index: Integer): TMeasurement;
var
  Collection: PMeasurementCollection;
begin
  Collection := GetCollectionPointer;

  if (Index < Low(Collection^))
    or (Index > High(Collection^)) then
  begin
    raise Exception.CreateFmt('Measurement index out of bounds(%d)', [Index]);
  end;

  Result := Collection^[Index];
end;

function TMeasurementEngine.GetMeasurementCount: Integer;
var
  Collection: PMeasurementCollection;
begin
  Collection := GetCollectionPointer;

  Result := Length(Collection^);
end;

procedure TMeasurementEngine.SetMeasurement(Index: Integer; AValue: TMeasurement
  );
var
  Collection: PMeasurementCollection;
begin
  Collection := GetCollectionPointer;

  if (Index < Low(Collection^))
    or (Index > High(Collection^)) then
  begin
    raise Exception.CreateFmt('Measurement index out of bounds(%d)', [Index]);
  end;

  Collection^[Index] := AValue;
end;

procedure TMeasurementEngine.Clear;
begin
  ClearSourceMeasurements;
  ClearRecalculatedMeasurements;
end;

procedure TMeasurementEngine.ClearSourceMeasurements;
var
  MeasurementLoop: Integer;
begin
  for MeasurementLoop := 0 to High(FSourceMeasurements) do
  begin
    SetLength(FSourceMeasurements[MeasurementLoop].MeasurementValues, 0);
  end;
  SetLength(FSourceMeasurements, 0);
  FSensorCount := 0;
end;

procedure TMeasurementEngine.ClearRecalculatedMeasurements;
var
  MeasurementLoop: Integer;
begin
  for MeasurementLoop := 0 to High(FRecalculatedMeasurements) do
  begin
    SetLength(FRecalculatedMeasurements[MeasurementLoop].MeasurementValues, 0);
  end;
  SetLength(FRecalculatedMeasurements, 0);
  FRecalculateMinutes := 0;
end;

function TMeasurementEngine.DoCorrectTimestamp(Timestamp: Int64): Int64;
var
  Seconds: Integer;
begin
  Seconds := SecondOf(UnixToDateTime(Timestamp));
  if Seconds >= 55 then
  begin
    Result := Timestamp + (60 - Seconds);
    if (60 - Seconds) > FMaxSecondsCorrected then
      FMaxSecondsCorrected := 60 - Seconds;
  end;
  if Seconds < 30 then
  begin
    Result := Timestamp - (Seconds);
    if Seconds > FMaxSecondsCorrected then
      FMaxSecondsCorrected := Seconds;
  end;
end;

function TMeasurementEngine.GetCollectionPointer: PMeasurementCollection;
begin
  case FOutputMode of
    omSource: Result := @FSourceMeasurements;
    omRecalculated: Result := @FRecalculatedMeasurements;
  end;
end;

procedure TMeasurementEngine.CopySourceWithinBounds(StartTimestamp,
  EndTimestamp: Int64);
var
  SourceLoop, SourceOffset: Integer;
begin
  ClearRecalculatedMeasurements;
  SourceOffset := 0;
  FOutputMode := omRecalculated;
  while FSourceMeasurements[SourceOffset].Timestamp < StartTimestamp do
    Inc(SourceOffset);
  SetLength(FRecalculatedMeasurements, Length(FSourceMeasurements)-SourceOffset);
  for SourceLoop := 0 to High(FSourceMeasurements)-SourceOffset do
  begin
    if FSourceMeasurements[SourceLoop+SourceOffset].Timestamp > EndTimestamp then
      Break;
    FRecalculatedMeasurements[SourceLoop].Timestamp :=
      FSourceMeasurements[SourceLoop+SourceOffset].Timestamp;
    FRecalculatedMeasurements[SourceLoop].MeasurementValues :=
      FSourceMeasurements[SourceLoop+SourceOffset].MeasurementValues;
  end;
end;

constructor TMeasurementEngine.Create;
begin
  FOutputMode := omSource;
  FSensorCount := 0;
end;

function TMeasurementEngine.LoadFromFile(Filename: String;
  CorrectTimestamps: Boolean): Boolean;
var
  SourceFile, Delimit: TStringList;
  RowLoop, SensorLoop, Offset: Integer;
  FormatSettings: TFormatSettings;
begin
  Result := True;
  Clear;
  SourceFile := TStringList.Create;
  Delimit := TStringList.Create;
  Delimit.Delimiter := ',';
  Delimit.StrictDelimiter := True;
  FormatSettings.DecimalSeparator := '.';
  FMaxSecondsCorrected := 0;
  FSensorCount := 0;

  SourceFile.LoadFromFile(Filename);

  SetLength(FSourceMeasurements, SourceFile.Count);
  for RowLoop := 0 to SourceFile.Count-1 do
  begin
    if Assigned(FOnWork) then
      FOnWork(Self, Round((RowLoop/(SourceFile.Count-1))*100));
    Delimit.DelimitedText := SourceFile.Strings[RowLoop];

    if CorrectTimestamps then
      FSourceMeasurements[RowLoop].Timestamp :=
        DoCorrectTimestamp(StrToInt(Delimit.Strings[0]))
    else
      FSourceMeasurements[RowLoop].TimeStamp := StrToInt(Delimit.Strings[0]);

    Offset := 1;
    while Offset <= Delimit.Count-1 do
    begin
      Inc(FSensorCount);
      SetLength(FSourceMeasurements[RowLoop].MeasurementValues,
       Length(FSourceMeasurements[RowLoop].MeasurementValues)+1);
      SensorLoop := High(FSourceMeasurements[RowLoop].MeasurementValues);

      FSourceMeasurements[RowLoop].MeasurementValues[SensorLoop].Min :=
        StrToFloat(Delimit[Offset], FormatSettings);
      FSourceMeasurements[RowLoop].MeasurementValues[SensorLoop].Max :=
        StrToFloat(Delimit[Offset+1], FormatSettings);
      FSourceMeasurements[RowLoop].MeasurementValues[SensorLoop].Mean :=
        StrToFloat(Delimit[Offset+2], FormatSettings);

      Inc(Offset, 3);

      if Pos('.', Delimit[Offset]) < 1 then
      begin
        FSourceMeasurements[RowLoop].MeasurementValues[SensorLoop].
          hasNumberOfMeasurements := True;
        FSourceMeasurements[RowLoop].MeasurementValues[SensorLoop].
          NumberOfMeasurements := StrToInt(Delimit[Offset]);
        Inc(Offset);
      end;
    end;

    //SetLength(FSourceMeasurements[RowLoop].MeasurementValues,
    //  ((Delimit.Count-1) div 3));
    //FSensorCount := ((Delimit.Count-1) div 3);

    {for SensorLoop := 0 to ((Delimit.Count-1) div 3)-1 do
    begin
      FSourceMeasurements[RowLoop].MeasurementValues[SensorLoop].Min :=
        StrToFloat(Delimit[SensorLoop*3+1], FormatSettings);
      FSourceMeasurements[RowLoop].MeasurementValues[SensorLoop].Max :=
        StrToFloat(Delimit[SensorLoop*3+2], FormatSettings);
      FSourceMeasurements[RowLoop].MeasurementValues[SensorLoop].Mean :=
        StrToFloat(Delimit[SensorLoop*3+3], FormatSettings);
    end;}
  end;

  Delimit.Free;
  SourceFile.Free;
end;

procedure TMeasurementEngine.Recalculate(Minutes: Integer; Align: Boolean;
  StartTimestamp, EndTimestamp: Int64);
var
  SampleLoop, SensorLoop, LoopBegin, CalculateLoop: Integer;
  Min, Max, Mean: Double;
begin
  FOutputMode := omRecalculated;
  FRecalculateMinutes := Minutes;
  FRecalculateAlign := Align;

  ClearRecalculatedMeasurements;

  LoopBegin := 0;
  // If StartTimestamp, find start
  if StartTimestamp <> -1 then
    while FSourceMeasurements[LoopBegin].Timestamp < StartTimestamp do
      Inc(LoopBegin);

  // If Align, find the correct beginning point
  if Align then
    if FSourceMeasurements[LoopBegin].Timestamp mod (Minutes*60) <> 0 then
    begin
      repeat
        Inc(LoopBegin);
      until FSourceMeasurements[LoopBegin].Timestamp mod (Minutes*60) = 0;
    end;

  for SampleLoop := 0 to (Length(FSourceMeasurements)-LoopBegin) div Minutes do
  begin
    if Assigned(FOnWork) then
      FOnWork(Self, Round((SampleLoop/((Length(FSourceMeasurements)-LoopBegin)
        div Minutes))*100));

    if LoopBegin+(SampleLoop+1)*Minutes > High(FSourceMeasurements) then
      Break;

    // if EndTimestamp, check bounds
    if EndTimestamp <> -1 then
      if FSourceMeasurements[LoopBegin+((SampleLoop+1)*Minutes)].Timestamp >
        EndTimestamp then
        Break;

    SetLength(FRecalculatedMeasurements, Length(FRecalculatedMeasurements)+1);

    SetLength(FRecalculatedMeasurements[SampleLoop].MeasurementValues,
      FSensorCount);

    FRecalculatedMeasurements[SampleLoop].Timestamp :=
      FSourceMeasurements[LoopBegin+((SampleLoop+1)*Minutes)].Timestamp;

    for SensorLoop := 0 to FSensorCount-1 do
    begin
      Min := 10000;
      Max := -10000;
      Mean := 0;

      for CalculateLoop := 0 to Minutes-1 do
      begin
        if FSourceMeasurements[LoopBegin+SampleLoop*Minutes+CalculateLoop].
          MeasurementValues[SensorLoop].Min < Min then
          Min := FSourceMeasurements[LoopBegin+SampleLoop*Minutes+CalculateLoop].
            MeasurementValues[SensorLoop].Min;
        if FSourceMeasurements[LoopBegin+SampleLoop*Minutes+CalculateLoop].
          MeasurementValues[SensorLoop].Max > Max then
          Max := FSourceMeasurements[LoopBegin+SampleLoop*Minutes+CalculateLoop].
            MeasurementValues[SensorLoop].Max;
        Mean := Mean + FSourceMeasurements[LoopBegin+SampleLoop*Minutes+CalculateLoop].
          MeasurementValues[SensorLoop].Mean;
      end;

      Mean := Mean / Minutes;

      FRecalculatedMeasurements[SampleLoop].
        MeasurementValues[SensorLoop].Min := Min;
      FRecalculatedMeasurements[SampleLoop].
        MeasurementValues[SensorLoop].Max := Max;
      FRecalculatedMeasurements[SampleLoop].
        MeasurementValues[SensorLoop].Mean := Mean;
    end;
  end;
end;

procedure TMeasurementEngine.SetSegment(LastHours: Integer);
begin
  SetSegment(DateTimeToUnix(TDateTime(now))-LastHours*3600,
    DateTimeToUnix(TDateTime(now)));
end;

procedure TMeasurementEngine.SetSegment(StartTimestamp, EndTimestamp: Int64);
begin
  if FOutputMode = omSource then
    CopySourceWithinBounds(StartTimestamp, EndTimestamp)
  else
    Recalculate(FRecalculateMinutes, FRecalculateAlign, StartTimestamp,
      EndTimestamp);
end;

procedure TMeasurementEngine.Reset;
begin
  FOutputMode := omSource;
  ClearRecalculatedMeasurements;
end;

end.

