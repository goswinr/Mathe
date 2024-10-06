namespace Mathe

open System


[<Obsolete("Not Obsolete, just hidden from editor tooling. Must be public because functions using it are inlined.")>]
module MatheFail =

    let fail11(x:float<_>,func:string) =  raise <| new ArgumentOutOfRangeException $"Mathe.{func}: input must be between -1.00001 and +1.00001 but is %8f{x}"

    let failClamp(maxVal, minVal)      =  raise <| new ArgumentOutOfRangeException $"Mathe.clamp: max value {maxVal} must be bigger than min {minVal}"

    let failInfNaN(func)               =  raise <| new ArgumentException $"Mathe.{func}: given input is NaN or Infinity"
    let failInfNaNv(func,name)         =  raise <| new ArgumentException $"Mathe.{func}: given input '{name}' is NaN"

    let failNaN(func)                  =  raise <| new ArgumentException $"Mathe.{func}: given input is NaN"
    let failNaNv(func,name)            =  raise <| new ArgumentException $"Mathe.{func}: given input '{name}' is NaN"

    let failTiny18(func)               =  raise <| new ArgumentException $"Mathe.{func}: given input is smaller than + or - 1-e18."
    let failTiny18v(func,name)         =  raise <| new ArgumentException $"Mathe.{func}: given input '{name}' is smaller than + or - 1-e18."

    let failBig36(func)                =  raise <| new ArgumentException $"Mathe.{func}: given input is bigger than + or - 1e36."
    let failBig36v(func,name)          =  raise <| new ArgumentException $"Mathe.{func}: given input '{name}' is bigger than + or - 1e36."

    let failNotPos(func, name, v)     =  raise <| new ArgumentException $"Mathe.{func}: given input '{name}' must be positive but is {v}."

#nowarn "44" // obsolete warning
open MatheFail


/// Math Utils.
type Mathe private () =

    static let rand = System.Random()

    /// Test is a floating point number (with Measure)  is NaN (Not a Number) or Infinity.
    static member inline isNanOrInf (f:float<'T>) =
        Double.IsInfinity (float f) || Double.IsNaN (float f)

    /// Test is a floating point number (with Measure)  is NaN (Not a Number)
    static member inline isNan (f:float<'T>) =
        Double.IsNaN (float f)

    /// To make sure a value is within a given range.
    /// minVal -> maxVal -> x -> clamped value.
    static member inline clamp (minVal:'T) (maxVal:'T) (x:'T) :'T =
        if   maxVal < minVal then failClamp(maxVal,minVal)
        elif x     > maxVal  then maxVal
        elif x      < minVal then minVal
        else x

    /// To make sure a value is between 0.0 and 1.0 range.
    static member inline clamp01 (value:'T) :'T =
        // if isNan value then ArgumentException.RaiseBase "Mathe.clamp01: given input is NaN."
        if   value > LanguagePrimitives.GenericOne< ^T>  then LanguagePrimitives.GenericOne< ^T>
        elif value < LanguagePrimitives.GenericZero< ^T> then LanguagePrimitives.GenericZero< ^T>
        else value

    /// Shadows the built in 'acos' (Inverse Cosine) function to include clamping if values are slightly above -1.0 or 1.0.
    /// Tolerance: 0.0001
    /// This is useful on dot products from unit vectors.
    /// Returns angel in Radians.
    static member inline acos (x:float<'T>) : float =  // no measure on returned float !
        if Mathe.isNan(x)    then failNaN  "acos"
        elif x < -1.0001<_>  then fail11(x,"acos")
        elif x >  1.0001<_>  then fail11(x,"acos")
        else x |> float |> Mathe.clamp -1.0 1.0 |> Math.Acos

    /// Shadows the built in 'asin' (Inverse Sine) function to include clamping if values are slightly above -1.0 or 1.0.
    /// Tolerance: 0.00001
    /// Returns angel in Radians.
    static member inline asin (x:float<'T>) : float= // no measure on returned float !
        if Mathe.isNan(x)    then failNaN  "asin"
        elif x < -1.0001<_>  then fail11(x,"asin")
        elif x >  1.0001<_>  then fail11(x,"asin")
        else x |> float |> Mathe.clamp -1.0 1.0 |> Math.Asin

    /// Converts Angels from Degrees to Radians.
    static member inline toRadians (degrees:float<'T>):float = // no measure on returned float !
        0.017453292519943295 * float degrees // 0.017453292519943295 = Math.PI / 180.

    /// Converts Angels from Radians to Degrees.
    static member inline toDegrees (radians:float<'T>) :float = // no measure on returned float !
        57.295779513082323 * float radians // 57.295779513082323 = 180. / Math.PI


    /// Checks if a number is between or on a lower and upper bound value.
    /// x >= minVal && x <= maxVal
    static member inline isInRange (minVal:'T) (maxVal:'T) (x:'T) :bool =
        x >= minVal && x <= maxVal

    /// Checks if a number is bigger or smaller than a lower and upper bound value .
    /// x < minVal || x > maxVal
    static member inline isNotInRange (minVal:'T) (maxVal:'T) (x:'T) :bool =
        x < minVal || x > maxVal

    /// Compares two numbers within a relative tolerance for equality.
    /// The comparing tolerance is calculated as:
    /// let mi = min (abs a) (abs b)
    /// abs(a-b) < relativeTolerance * mi
    static member inline equalsWithRelativeTolerance (relativeTolerance:float) (valueA:float<'T>) (valueB:float<'T>) :bool  =
        //if Mathe.isNanOrInf valueA then raise <| ArgumentException("Mathe.equalsWithRelativeTolerance: given valueA is NaN or Infinity") // don't do this, keep it generic
        //if Mathe.isNanOrInf valueB then raise <| ArgumentException("Mathe.equalsWithRelativeTolerance: given valueB is NaN or Infinity")
        let mi = min (abs valueA) (abs valueB)
        abs(valueA - valueB) < relativeTolerance * mi

    /// Compares two numbers to be within a tolerance for equality.
    /// abs(a-b) < absoluteTolerance
    static member inline equalsWithTolerance (absoluteTolerance:'T) (valueA:'T) (valueB:'T) :bool =
        //if Mathe.isNanOrInf valueA then raise <| ArgumentException("Mathe.equalsWithTolerance: given valueA is NaN or Infinity") // don't do this, keep it generic
        //if Mathe.isNanOrInf valueB then raise <| ArgumentException("Mathe.equalsWithTolerance: given valueB is NaN or Infinity")
        abs(valueA - valueB) < absoluteTolerance


    /// Interpolates between start and end value Generic numbers.
    /// Works on any type that implements  + , - and *
    static member inline interpolate (start:'T)  (ende:'T)  (rel:'T) :'T =
        //if Mathe.isNanOrInf start then raise <| ArgumentException "Mathe.interpolate: given input for 'start' is NaN or Infinity."
        //if Mathe.isNanOrInf ende  then raise <| ArgumentException "Mathe.interpolate: given input for 'ende' is NaN or Infinity."
        //if Mathe.isNanOrInf rel   then raise <| ArgumentException "Mathe.interpolate: given input for 'rel' is NaN or Infinity."
        //start + ( (ende - start) * (float rel) )
        start + ( (ende - start) * rel )


    /// Returns a function to find linear interpolations in one table.
    /// The input table is a sorted (increasing) array of tuples of input and respective output values.
    //static member interpolateTable (table: Collections.Generic.IList< float<'Input> * float<'Output> > ) : float<'Input> -> float<'Output> =
    static member interpolateTable (table: Collections.Generic.IList< float<'Input> * float<'Output> > ) : float<'Input> -> float<'Output> =

        // make sure it is sorted and always increasing
        for tn in Seq.windowed 2 table do
            let  t= tn.[0]
            let  n= tn.[1]
            if fst t >= fst n then
                raise <| ArgumentException $"Mathe.interpolateTable: Table input is not sorted increasing {t} >= {n}:\r\n{table}"

        // return the lookup function
        fun (x:float<'Input>) ->
            if x < fst table.[0] then
                //failwithf "Table query %A is smaller than first element in table %A" x table.[0]
                let stepIn  = fst table.[1] - fst table.[0]
                let stepOut = snd table.[1] - snd table.[0]
                let distIn  = fst table.[0] - x
                if abs(float stepIn) < 1e-16 then
                    raise <| ArgumentException $"Mathe.interpolateTable: Table query {x} is smaller than first element in table {table.[0]} and the first two Input elements are almost the same, so a meaningful prediction is not possible."
                    // snd table.[0]
                else
                    let sc  = distIn / stepIn
                    snd table.[0] - stepOut * sc

            elif x > fst table.[table.Count-1]  then
                //failwithf "Table query %A is bigger than last element in table %A"   x table.[table.Length-1]
                let li = table.Count-1
                let stepIn  = fst table.[li] - fst table.[li-1]
                let stepOut = snd table.[li] - snd table.[li-1]
                let distIn  = x - fst table.[li]
                if abs(float stepIn) < 1e-16 then  // last are duplicates
                    raise <| ArgumentException $"Mathe.interpolateTable: Table query {x} is bigger than last element in table {table.[li]} and the last two Input elements are almost the same, so a meaningful prediction is not possible."
                    //snd table.[li]
                else
                    let sc  = distIn / stepIn
                    snd table.[li] + stepOut * sc
            else
                let hiIdx = table |> Seq.findIndex ( fun v ->  fst v >= x) // finding could be optimized with binary search since its sorted
                if hiIdx = 0 then
                    snd table.[0]
                else
                    let low,lowOut  = table.[hiIdx-1]
                    let hig,higOut  = table.[hiIdx]
                    let delta = hig - low
                    if abs(float delta) < 1e-16 then // tolerance for div by zero (eg. from  duplicate positions in the list)
                        (low+hig) *0.5 |> float |> LanguagePrimitives.FloatWithMeasure<'Output>
                    else
                        let local = x - low
                        let rel = local / delta
                        let deltaOut = higOut - lowOut
                        let res = lowOut + deltaOut * rel
                        res


    /// Returns a function to find linear interpolations from two sorted(increasing) Lists of input and respective output values.
    /// Includes a check that both list have the same length
    static member interpolateLists (input: float<'Input> Collections.Generic.IList) ( output: float<'Output> Collections.Generic.IList ) : (float<'T> ->  float<'U>) =
        if input.Count <> output.Count then
            raise <| ArgumentException $"Mathe.interpolateLists: Tables length don't match:\r\n{input}\r\nand:\r\n{output}"
        Mathe.interpolateTable(Seq.zip input output|> Array.ofSeq)


    /// Multiplicative inverse or reciprocal:
    /// 1/x
    static member inline reciprocal (x:float<'T>) : float< /'T > =
        if Mathe.isNan x then failNaN "reciprocal"
        let a = abs(x)
        if a < 1e-18<_> then failTiny18 "reciprocal"
        if a > 1e36<_>  then failBig36 "reciprocal"
        1.0 / x
        //if x = LanguagePrimitives.GenericZero< ^T> then  raise <| ArgumentException "Mathe.reciprocal: given input is Zero"
        //LanguagePrimitives.GenericOne< ^T> / x

    /// Multiplicative inverse or reciprocal:
    /// 1/x
    static member inline inverse(x:float<'T>) : float< /'T > =
        if Mathe.isNan x then failNaN "inverse"
        let a = abs(x)
        if a < 1e-18<_> then failTiny18 "inverse"
        if a > 1e36<_>  then failBig36 "inverse"
        1.0 / x
        //if x = LanguagePrimitives.GenericZero< ^T> then  raise <| ArgumentException "Mathe.reciprocal: given input is Zero" // generic alternative, not used to have safety checks
        //LanguagePrimitives.GenericOne< ^T> / x

    /// Given the min and max value and a test value, (val-min) / (max-min)
    /// Returns the relative  position of the test value between min (= 0.0) and (max = 1.0),
    /// can also be bigger than 1.0
    static member inline normalize (rangeStart:float<'T>) (rangeStop:float<'T>) (valueAt:float<'T>) :float<'T> =
        if Mathe.isNan rangeStart then failNaNv("normalize", "rangeStart")
        if Mathe.isNan rangeStop  then failNaNv("normalize", "rangeStop")
        if Mathe.isNan valueAt    then failNaNv("normalize", "valueAt")
        let rangeLength = float (rangeStart - rangeStop)
        let a = abs(rangeLength)
        if a < 1e-18 then failTiny18v( "normalize", "rangeStart-rangeStop")
        if a > 1e36  then failBig36v(  "normalize", "rangeStart-rangeStop")
        //if range = LanguagePrimitives.GenericZero< ^T> then raise <| ArgumentException "Mathe.normalize: rangeStart-rangeStop is Zero" // generic alternative, not used to have safety checks
        (rangeStart - valueAt) / rangeLength

    /// Rounds to the next bigger (away from zero) number on logarithmic scale.
    /// Define scale by giving amount of steps(int) to double or half a value.
    /// Fails if input is zero or less than 1e-24.
    static member roundUpToNextLogSteps (stepsToDouble:int) (x:float<'T>) :float<'T> =
        if Mathe.isNan x then failNaN "roundUpToNextLogSteps"
        if stepsToDouble < 0 then failNotPos("roundUpToNextLogSteps", "stepsToDouble", stepsToDouble)
        let a = abs(float x)
        if a < 1e-24 then failTiny18 "roundUpToNextLogSteps"
        let logBase =  2. ** (1./float stepsToDouble)
        if x > 0.0<_> then    logBase ** (Math.Ceiling (Math.Log (    float  x , logBase))) |> LanguagePrimitives.FloatWithMeasure
        else                -(logBase ** (Math.Ceiling (Math.Log (abs(float  x), logBase))))|> LanguagePrimitives.FloatWithMeasure // with negative sign, (log fails on negative numbers)

    /// Rounds to the next smaller (closer to zero) number on logarithmic scale
    /// Define scale by giving amount of steps(int) to double or half a value.
    /// Fails if input is zero or less than 1e-24.
    static member roundDownToNextLogSteps (stepsToDouble:int) (x:float<'T>) :float<'T> =
        if Mathe.isNan x then failNaN "roundDownToNextLogSteps"
        if stepsToDouble < 0 then failNotPos("roundDownToNextLogSteps", "stepsToDouble", stepsToDouble)
        let a = abs(float x)
        if a < 1e-24 then failTiny18 "roundDownToNextLogSteps"
        let logBase =  2. ** (1./float stepsToDouble)
        if x > 0.0<_> then   logBase ** (Math.Floor (Math.Log (    float  x , logBase)))  |> LanguagePrimitives.FloatWithMeasure
        else               -(logBase ** (Math.Floor (Math.Log (abs(float  x), logBase)))) |> LanguagePrimitives.FloatWithMeasure // with negative sign, (log fails on negative numbers)


    /// Numeric Stepping: Converts floats to ints, divided by precision.
    /// Like rounding floats to integers but with another step size than 1.0
    /// = int (v / precision)
    static member inline steppedIntF (precision:float) (value:float) : int =
        if Mathe.isNanOrInf value     then failInfNaNv("steppedIntF", "value")
        if precision < 1e-9           then failNotPos( "steppedIntF", "precision", precision)
        int (value / precision)

    /// Numeric Stepping: Converts floats to ints within defined integer step sizes.
    /// Always rounding down like the int function
    /// = int (value / float precision) * precision
    static member inline steppedInt (precision:int) (value:float) : int =
        if Mathe.isNanOrInf value then failInfNaNv("steppedInt", "value")
        if precision < 1          then failNotPos( "steppedInt", "precision", precision)
        int (value / float precision) * precision

    /// Numeric Stepping:Converts floats to floats within defined float step sizes.
    /// Always rounding down like the int function)
    /// = float (int (value/ precision)) * precision
    static member inline steppedFloat (precision:float) (value:float) : float =
        if Mathe.isNanOrInf value      then failInfNaNv("steppedFloat", "value")
        if precision < 1e-9            then failNotPos( "steppedFloat", "precision", precision)
        float (int (value/ precision)) * precision

    /// Numeric Stepping:Converts floats to floats within defined float step sizes.
    /// Always rounding mid point  like the round function)
    /// =  (round (value/ precision)) * precision
    static member inline steppedFloatMid (precision:float) (value:float) : float =
        if Mathe.isNanOrInf value     then failInfNaNv("steppedFloatMid", "value")
        if precision < 1e-9           then failNotPos( "steppedFloatMid", "precision", precision)
        (round (value/ precision)) * precision

    /// This float range function ensures that the end is always included.
    /// The F# build in range fails for example on [0.0 .. 0.1 .. 0.2 ] , it equals [0.0 .. 0.1 .. 0.3 ]
    /// It increases the stop value by the smallest step possible 15 times, to ensure end value is included in returned seq.
    static member floatRange (start:float<'T>, step:float<'T> , stop:float<'T>) : seq<float<'T>> =
        if Mathe.isNanOrInf start then failInfNaNv("floatRange", "start")
        if Mathe.isNanOrInf stop  then failInfNaNv("floatRange", "stop")
        if Mathe.isNanOrInf step  then failInfNaNv("floatRange", "step")
        let a = abs(float step)
        if a < 1e-36 then raise <| ArgumentOutOfRangeException $"Mathe.floatRange: step-size cannot be zero: start: {start} step: {step} stop: {stop}"
        let range =
            stop - start
            |> float
            |> BitConverter.DoubleToInt64Bits //https://float.exposed/0x3ff0000000000000
            |> (+) 15L
            |> BitConverter.Int64BitsToDouble
            |> LanguagePrimitives.FloatWithMeasure
        let steps =  range / step
        if steps < 1e-36 then raise <| ArgumentOutOfRangeException $"Mathe.floatRange: stop value can never be reached: start: {start} step: {step} stop: {stop}"
        seq {
            let mutable i = 0.0
            while i <= steps do
                yield start + i * step
                i <- i + 1.0
        }


    /// Given mean and standardDeviation returns a random value from this Gaussian distribution.
    /// If mean is 0.0 and the standard deviation is 1.0 then 99% of values are within -2.3 to +2.3 ; 70% within -1 to +1
    static member randomStandardDeviation (mean:float<'T> , standardDeviation:float<'U>) : float<'T> =
        if Mathe.isNanOrInf mean              then failInfNaNv("randomStandardDeviation", "mean")
        if Mathe.isNanOrInf standardDeviation then failInfNaNv("randomStandardDeviation", "standardDeviation")
        let u1 = 1.0 - rand.NextDouble() // so that 0.0  < x <= 1.0 (never 0.0) see https://docs.microsoft.com/en-us/dotnet/api/system.random.nextdouble?view=netframework-4.7.2#remarks
        let u2 = rand.NextDouble()
        let randStdNormal = Math.Sqrt(-2.0 * Math.Log(u1)) * Math.Sin(2.0 * Math.PI * u2) //random normal(0, 1)
        //random normal(mean, stdDev^2)
        mean + (float standardDeviation|> LanguagePrimitives.FloatWithMeasure)* randStdNormal


    /// Just Double.TryParse(x) wrapped in an option
    static member inline tryParseFloat (x:string) : option<float> =
        match Double.TryParse(x) with
        | true, f -> Some f
        | _ ->  None

    /// A float parser for NON English locales.
    /// Replaces ',' with '.' and then runs Double.TryParse(x) wrapped in an option
    static member inline tryParseFloatDe (x:string) : option<float> =
        match Double.TryParse(x.Replace(',','.')) with
        | true, f -> Some f
        | _ ->  None

    /// A very tolerant custom float parser for English locale.
    /// Ignores all non numeric characters ( expect a minus '-' before any digit )
    /// and considers a period '.' as decimal separator.
    /// Does NOT allow for scientific notation !
    static member tryParseFloatTolerant(s:string) : option<float> =
        let sb = Text.StringBuilder(s.Length)
        for i = 0 to s.Length - 1 do
            let c = s.[i]
            if   c >= '0' && c <= '9'       then sb.Append(c)   |> ignore
            elif c = '.'                    then sb.Append(c)   |> ignore
            elif c = '-' && sb.Length = 0   then sb.Append(c)   |> ignore //only add minus before digits if stringBuilder is still empty
        match Double.TryParse(sb.ToString()) with
        | true, f -> Some  f
        | _ ->   None


    /// A very tolerant custom float parser for NON English locales.
    /// Ignores all non numeric characters ( expect a minus '-' before any digit )
    /// and considers a comma ',' as decimal separator.
    /// Does NOT allow for scientific notation !
    static member tryParseFloatDeTolerant(s:string) : option<float> =
        let sb = Text.StringBuilder(s.Length)
        for i = 0 to s.Length - 1 do
            let c = s.[i]
            if   c >= '0' && c <= '9'       then sb.Append(c)   |> ignore
            elif c = ','                    then sb.Append('.') |> ignore
            elif c = '-' && sb.Length = 0   then sb.Append(c)   |> ignore //only add minus before digits if stringBuilder is still empty
        match Double.TryParse(sb.ToString()) with
        | true, f -> Some  f
        | _ ->   None
