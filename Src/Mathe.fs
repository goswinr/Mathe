namespace Mathe

open System


/// Functions to deal with integer ref objects
/// Also works with integers augmented with Units of Measure (UoM)
module IntRef =

    /// Increment a ref cell by one
    /// Shadows built in 'incr' to allow Units of Measure (UoM)
    let inline incr (i:ref<int<'UoM>>) = i.Value <- i.Value + 1<_>

    /// Decrement a ref cell by one
    let inline decr (i:ref<int<'UoM>>) = i.Value <- i.Value - 1<_>

    /// Increment a ref cell by a given int
    let inline incrBy (i:ref<int<'UoM>>) (x:int<'UoM>) = i.Value <- i.Value + x

    /// Decrement a ref cell by a given int
    let inline decrBy (i:ref<int<'UoM>>) (x:int<'UoM>) = i.Value <- i.Value - x

    /// Set ref cell to given int if it is bigger than current ref cell  value
    let inline setMax (i:ref<int<'UoM>>) (x:int<'UoM>) = if x > i.Value then i.Value <- x

    /// Set ref cell to given int if it is smaller than current ref cell  value
    let inline setMin (i:ref<int<'UoM>>)  (x:int<'UoM>) = if x < i.Value then i.Value <- x



/// Functions to deal with float ref objects
/// Also works with floats augmented with Units of Measure (UoM)
module FloatRef =

    /// Increment a ref cell by a given float
    let inline incrBy (f:ref<float<'UoM>>) (x:float<'UoM>) = f.Value <- f.Value + x

    /// Decrement a ref cell by a given float
    let inline decrBy (f:ref<float<'UoM>>) (x:float<'UoM>) = f.Value <- f.Value - x

    /// Set ref cell to given float if it is bigger than current ref cell  value
    let inline setMax (f:ref<float<'UoM>>) (x:float<'UoM>) = if x > f.Value then f.Value <- x

    /// Set ref cell to given float if it is smaller than current ref cell  value
    let inline setMin (f:ref<float<'UoM>>) (x:float<'UoM>) = if x < f.Value then f.Value <- x


/// Provides generic math operators for adding, subtracting, multiplying and dividing
/// numbers that can be converted to a floats.
/// The new operators are: +.  .+   -.  .-   *.  .*   /.  ./
/// There the period is always on the side of the non float value.
/// A Units of Measure on the non-float number gets ignored and lost however.
module FloatMathOperators =
    open Microsoft.FSharp.Core.LanguagePrimitives

    /// Multiplies a float with A-number-that-can-be-converted-to-a-float
    let inline ( *. ) (x:float<'M>) (y) : float<'M> = x * (float y)

    /// Multiplies a-number-that-can-be-converted-to-a-float with a float
    let inline ( .* ) (x) (y :float<'M>) : float<'M> = (float x) * y

    /// Add a float to A-number-that-can-be-converted-to-a-float
    let inline ( +. ) (x:float<'M>) (y) : float<'M> = x + FloatWithMeasure<'M>(float y)

    /// Add A-number-that-can-be-converted-to-a-float to a float
    let inline ( .+ ) (x) (y :float<'M>) : float<'M> = FloatWithMeasure<'M>(float x) + y

    /// Subtract a float from A-number-that-can-be-converted-to-a-float
    let inline ( -. ) (x:float<'M>) (y) : float<'M> = x - FloatWithMeasure<'M>(float y)

    /// Subtract A-number-that-can-be-converted-to-a-float to a float
    let inline ( .- ) (x) (y :float<'M>) : float<'M> = FloatWithMeasure<'M>(float x) - y

    /// Divide a float by A-number-that-can-be-converted-to-a-float
    let inline ( /. ) (x:float<'M>) (y) : float<'M>=  x / (float y)

    /// Divide A-number-that-can-be-converted-to-a-float by a float
    let inline ( ./ ) (x) (y :float) : float = (float x) / y


/// This module is set to auto open when opening Mathe namespace.
/// Static Extension methods on Exceptions to cal Exception.Raise "%A" x with F# printf string formatting
[<AutoOpen>]
[<Obsolete("not obsolete, just hidden from editor tooling, but must be public for inlining")>]
module AutoOpenExtensionsExceptions =

    type ArgumentException with
        /// Raise ArgumentException with F# printf string formatting
        /// this is also the base class of ArgumentOutOfRangeException and ArgumentNullException
        static member RaiseBase msg = Printf.kprintf (fun s -> raise (ArgumentException(s))) msg

    type ArgumentOutOfRangeException with
        /// Raise ArgumentOutOfRangeException with F# printf string formatting
        static member Raise msg = Printf.kprintf (fun s -> raise (ArgumentOutOfRangeException(s))) msg

    type ArgumentNullException with
        /// Raise ArgumentNullException with F# printf string formatting
        static member Raise msg = Printf.kprintf (fun s -> raise (ArgumentNullException(s))) msg

    type IndexOutOfRangeException with
        /// Raise IndexOutOfRangeException with F# printf string formatting
        static member Raise msg = Printf.kprintf (fun s -> raise (IndexOutOfRangeException(s))) msg


/// Math Utils.
type Mathe private () =

    static let rand = System.Random()

    /// Test is a floating point number (with Measure)  is NaN (Not a Number) or Infinity.
    static member inline isNanOrInf (f:float<'T>) = Double.IsInfinity (float f) || Double.IsNaN (float f)

    /// Test is a floating point number (with Measure)  is NaN (Not a Number)
    static member inline isNan (f:float<'T>) = Double.IsNaN (float f)

    /// To make sure a value is within a given range.
    /// minVal -> maxVal -> x -> clamped value.
    static member inline clamp (minVal:'T) (maxVal:'T) (x:'T) :'T =
        if maxVal < minVal then ArgumentOutOfRangeException.Raise "Mathe.clamp: max value %g must be bigger than min %g" maxVal minVal
        if x > maxVal then maxVal elif x < minVal then minVal else x

    /// To make sure a value is between 0.0 and 1.0 range.
    static member inline clamp01 (value:'T) :'T =
        // if isNan value then ArgumentException.RaiseBase "Mathe.clamp01: given input is NaN."
        if   value > LanguagePrimitives.GenericOne< ^T>  then LanguagePrimitives.GenericOne< ^T>
        elif value < LanguagePrimitives.GenericZero< ^T> then LanguagePrimitives.GenericZero< ^T>
        else value

    /// Shadows the built in 'acos' (Inverse Cosine) function to include clamping if values are slightly above -1.0 or 1.0.
    /// Tolerance: 0.00001
    /// This is useful on dot products from unit vectors.
    /// Returns angel in Radians.
    static member inline acos (x:float<'T>) : float =  // no measure on returned float !
        if Mathe.isNanOrInf x then raise <| ArgumentException "Mathe.acos: given input is NaN or Infinity."
        if x < -1.00001<_> then ArgumentOutOfRangeException.Raise "Mathe.acos failed on %f , input must be between -1.00001 and +1.00001" x
        if x >  1.00001<_>  then ArgumentOutOfRangeException.Raise "Mathe.acos failed on %f , input must be between -1.00001 and +1.00001" x
        else x  |> float|> Mathe.clamp -1.0 1.0 |> System.Math.Acos

    /// Shadows the built in 'asin' (Inverse Sine) function to include clamping if values are slightly above -1.0 or 1.0.
    /// Tolerance: 0.00001
    /// Returns angel in Radians.
    static member inline asin (x:float<'T>) : float= // no measure on returned float !
        if Mathe.isNanOrInf x then raise <| ArgumentException "Mathe.asin: given input is NaN or Infinity."
        if x < -1.00001<_> then ArgumentOutOfRangeException.Raise "Mathe.asin failed on %f , input must be between -1.00001 and +1.00001" x
        if x >  1.00001<_> then ArgumentOutOfRangeException.Raise "Mathe.asin failed on %f , input must be between -1.00001 and +1.00001" x
        else x |> float |> Mathe.clamp -1.0 1.0  |> System.Math.Asin

    /// Converts Angels from Degrees to Radians.
    static member inline toRadians (degrees:float<'T>):float =  // no measure on returned float !
        if Mathe.isNanOrInf degrees then raise <| ArgumentException "Mathe.toRadians: given input is NaN or Infinity."
        0.0174532925199433 * float degrees // 0.0174532925199433 = Math.PI / 180.

    /// Converts Angels from Radians to Degrees.
    static member inline toDegrees (radians:float<'T>) :float = // no measure on returned float !
        if Mathe.isNanOrInf radians then raise <| ArgumentException "Mathe.toDegrees: given input is NaN or Infinity."
        57.2957795130823 * float radians // 57.2957795130823 = 180. / Math.PI


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
    static member interpolateTable (table: Collections.Generic.IList< float<'Input> * float<'Output> > ) : float<'Input> -> float<'Output> =

        // make sure it is sorted and always increasing
        for tn in Seq.windowed 2 table do
            let  t= tn.[0]
            let  n= tn.[1]
            if fst t >= fst n then
                ArgumentException.RaiseBase "Mathe.interpolateTable: Table input is not sorted increasing %A >= %A  :\r\n%A" t  n table

        // return the lookup function
        fun (x:float<'Input>) ->
            if x < fst table.[0] then
                //failwithf "Table query %A is smaller than first element in table %A" x table.[0]
                let stepIn  = fst table.[1] - fst table.[0]
                let stepOut = snd table.[1] - snd table.[0]
                let distIn  = fst table.[0] - x
                if abs(float stepIn) < 1e-16 then
                    ArgumentException.RaiseBase "Mathe.interpolateTable: Table query %g is smaller than first element in table %A and the first two Input elements are almost the same, so a meaningful prediction is not possible." x table.[0]
                    snd table.[0]
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
                    ArgumentException.RaiseBase "Mathe.interpolateTable: Table query %g is bigger than last element in table %A and the last two Input elements are almost the same, so a meaningful prediction is not possible."   x table.[li]
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
                        (low+hig) *0.5
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
            ArgumentException.RaiseBase "Mathe.interpolateLists: Tables length don't match:\r\n%A\r\nand:\r\n%A" input output
        Mathe.interpolateTable(Seq.zip input output|> Array.ofSeq)


    /// Multiplicative inverse or reciprocal:
    /// 1/x
    static member inline reciprocal (x:float<'T>) : float< /'T > =
        if Mathe.isNanOrInf x then raise <| ArgumentException "Mathe.reciprocal: given input is NaN or Infinity."  // don't do this, keep it generic
        let a = abs(x) // don't do this, keep it generic?
        if a < 1e-16<_> then raise <| ArgumentException "Mathe.reciprocal: given input is almost Zero, less than + or - 1e-16."
        if a > 1e24<_>  then raise <| ArgumentException "Mathe.reciprocal: given input is extremely large, more than + or - 1e24."  // don't do this, keep it generic
        1.0 / x
        //if x = LanguagePrimitives.GenericZero< ^T> then  raise <| ArgumentException "Mathe.reciprocal: given input is Zero"
        //LanguagePrimitives.GenericOne< ^T> / x

    /// Multiplicative inverse or reciprocal:
    /// 1/x
    static member inline inverse(x:float<'T>) : float< /'T > =
        if Mathe.isNanOrInf x then raise <| ArgumentException "Mathe.inverse: given input is NaN or Infinity."  // don't do this, keep it generic
        let a = abs(x) // don't do this, keep it generic?
        if a < 1e-16<_> then raise <| ArgumentException "Mathe.inverse: given input is almost Zero, less than + or - 1e-16."
        if a > 1e24<_>  then raise <| ArgumentException "Mathe.inverse: given input is extremely large, more than + or - 1e24."  // don't do this, keep it generic
        1.0 / x
        //if x = LanguagePrimitives.GenericZero< ^T> then  raise <| ArgumentException "Mathe.reciprocal: given input is Zero" // generic alternative, not used to have safety checks
        //LanguagePrimitives.GenericOne< ^T> / x

    /// Given the min and max value and a test value,  (val-min) / (max-min)
    /// Returns the relative  position of the test value between min (= 0.0) and (max = 1.0),
    /// can also be bigger than 1.0
    static member inline normalize (rangeStart:float<'T>) (rangeStop:float<'T>) (valueAt:float<'T>) :float<'T> =
        if Mathe.isNanOrInf rangeStart then raise <| ArgumentException "Mathe.normalize: given input for 'rangeStart' is NaN or Infinity."
        if Mathe.isNanOrInf rangeStop  then raise <| ArgumentException "Mathe.normalize: given input for 'rangeStop' is NaN or Infinity."
        if Mathe.isNanOrInf valueAt    then raise <| ArgumentException "Mathe.normalize: given input for 'valueAt' is NaN or Infinity."
        let rangeLength = float (rangeStart - rangeStop)
        let a = abs(rangeLength)
        if a < 1e-16 then raise <| ArgumentException "Mathe.normalize: rangeStart-rangeStop is almost Zero, less than + or - 1e-16."
        if a > 1e24  then raise <| ArgumentException "Mathe.normalize: rangeStart-rangeStop is extremely large, more than + or - 1e24."
        //if range = LanguagePrimitives.GenericZero< ^T> then raise <| ArgumentException "Mathe.normalize: rangeStart-rangeStop is Zero" // generic alternative, not used to have safety checks
        (rangeStart-valueAt)/ rangeLength

    /// Rounds to the next bigger (away from zero) number on logarithmic scale.
    /// Define scale by giving amount of steps(int) to double or half a value.
    /// Fails if input is zero
    static member roundUpToNextLogSteps (stepsToDouble:int) (x:float<'T>) :float<'T> =
        if Mathe.isNanOrInf x then raise <| ArgumentException "Mathe.roundUpToNextLogSteps: given input for 'rangeStart' is NaN or Infinity."
        if stepsToDouble < 0 then ArgumentOutOfRangeException.Raise "Mathe.roundUpToNextLogSteps failed on: int stepsToDouble must be bigger than 0"
        let a = abs(float x)
        if a < 1e-24 then raise <| ArgumentException "Mathe.roundUpToNextLogSteps: input value is almost Zero, less than + or - 1e-24."

        let logBase =  2. ** (1./float stepsToDouble)
        if x > 0.0<_> then    logBase ** (Math.Ceiling (Math.Log (    float  x , logBase))) |> LanguagePrimitives.FloatWithMeasure
        else                -(logBase ** (Math.Ceiling (Math.Log (abs(float  x), logBase))))|> LanguagePrimitives.FloatWithMeasure // with negative sign, (log fails on negative numbers)

    /// Rounds to the next smaller (closer to zero) number on logarithmic scale
    /// Define scale by giving amount of steps(int) to double or half a value.
    /// Fails if input is zero
    static member roundDownToNextLogSteps  (stepsToDouble:int) (x:float<'T>) :float<'T> =
        if Mathe.isNanOrInf x then raise <| ArgumentException "Mathe.roundDownToNextLogSteps: given input for 'rangeStart' is NaN or Infinity."
        if stepsToDouble < 0 then ArgumentOutOfRangeException.Raise "Mathe.roundDownToNextLogSteps failed on: int stepsToDouble must be bigger than 0 but is %d" stepsToDouble
        let a = abs(float x)
        if a < 1e-24 then raise <| ArgumentException "Mathe.roundDownToNextLogSteps: input value is almost Zero, less than + or - 1e-24."

        let logBase =  2. ** (1./float stepsToDouble)
        if x > 0.0<_> then   logBase ** (Math.Floor (Math.Log (    float  x , logBase)))  |> LanguagePrimitives.FloatWithMeasure
        else               -(logBase ** (Math.Floor (Math.Log (abs(float  x), logBase)))) |> LanguagePrimitives.FloatWithMeasure// with negative sign, (log fails on negative numbers)


    /// Numeric Stepping: Converts floats to ints, divided by precision.
    /// Like rounding floats to integers but with another step size than 1.0
    /// = int (v / precision)
    static member inline precisionInt (precision:float) (v:float) : int =
        if Mathe.isNanOrInf v    then raise <| ArgumentException "Mathe.precisionInt: given input for 'v' is NaN or Infinity."
        if Mathe.isNanOrInf precision then raise <| ArgumentException "Mathe.precisionInt: given input for 'precision' is NaN or Infinity."
        if precision < 1e-16    then raise <| ArgumentException "Mathe.precisionInt: precision value is negative or almost Zero, less than +1e-16."
        int (v / precision)

    /// Numeric Stepping:Converts floats to ints within defined integer step sizes.
    /// Always rounding down like the int function
    /// = int (v / float precision) * precision
    static member inline steppedInt (precision:int) (v:float) : int =
        if Mathe.isNanOrInf v    then raise <| ArgumentException "Mathe.steppedInt: given input for 'v' is NaN or Infinity."
        if precision < 1   then raise <| ArgumentException "Mathe.steppedInt: precision value is negative or Zero."
        int (v / float precision) * precision

    /// Numeric Stepping:Converts floats to floats within defined float step sizes.
    /// Always rounding down like the int function)
    /// = float (int (v / precision)) * precision
    static member inline steppedFloat (precision:float) (v:float) : float =
        if Mathe.isNanOrInf v    then raise <| ArgumentException "Mathe.steppedFloat: given input for 'v' is NaN or Infinity."
        if Mathe.isNanOrInf precision then raise <| ArgumentException "Mathe.steppedFloat: given input for 'precision' is NaN or Infinity."
        if precision < 1e-16    then raise <| ArgumentException "Mathe.steppedFloat: precision value is negative or almost Zero, less than +1e-16."
        float (int (v / precision)) * precision

    /// Numeric Stepping:Converts floats to floats within defined float step sizes.
    /// Always rounding mid point  like the round function)
    /// =  (round (v / precision)) * precision
    static member inline steppedFloatMid (precision:float) (v:float) : float =
        if Mathe.isNanOrInf v    then raise <| ArgumentException "Mathe.steppedFloatMid: given input for 'v' is NaN or Infinity."
        if Mathe.isNanOrInf precision then raise <| ArgumentException "Mathe.steppedFloatMid: given input for 'precision' is NaN or Infinity."
        if precision < 1e-16    then raise <| ArgumentException "Mathe.steppedFloatMid: precision value is negative or almost Zero, less than +1e-16."
        (round (v / precision)) * precision

    /// This float range function ensures that the end is always included.
    /// The F# build in range fails for example on [0.0 .. 0.1 .. 0.2 ] , it equals [0.0 .. 0.1 .. 0.3 ]
    /// It increases the stop value by the smallest step possible 15 times, to ensure end value is included in returned seq.
    static member floatRange (start:float<'T>, step:float<'T> , stop:float<'T>) : seq<float<'T>> =
        if Mathe.isNanOrInf start then raise <| ArgumentException "Mathe.floatRange: given input for 'start' is NaN or Infinity."
        if Mathe.isNanOrInf stop then raise <| ArgumentException "Mathe.floatRange: given input for 'stop' is NaN or Infinity."
        if Mathe.isNanOrInf step then raise <| ArgumentException "Mathe.floatRange: given input for 'step' is NaN or Infinity."
        if step = LanguagePrimitives.FloatWithMeasure<'T> 0.0 then  ArgumentOutOfRangeException.Raise "Mathe.floatRange: step-size cannot be zero: start: %f step: %f stop: %f " start step stop
        let range = stop - start
                    |> float
                    |> BitConverter.DoubleToInt64Bits //https://float.exposed/0x3ff0000000000000
                    |> (+) 15L
                    |> BitConverter.Int64BitsToDouble
                    |> LanguagePrimitives.FloatWithMeasure
        let steps = range/step
        if steps < 0.0 then ArgumentOutOfRangeException.Raise "Mathe.floatRange:stop value cannot be reached: start: %f step: %f stop: %f " start step stop
        let rec frange (start, i, steps) =
            seq { if i <= steps then
                    yield start + i*step
                    yield! frange (start, (i + 1.0 ), steps) }
        frange (start, 0.0 , steps)


    /// Given mean and standardDeviation returns a random value from this Gaussian distribution.
    /// If mean is 0 and stDev is 1 then 99% of values are within -2.3 to +2.3 ; 70% within -1 to +1
    static member randomStandardDeviation (mean:float<'T> , standardDeviation:float<'U>) : float<'T> =
        if Mathe.isNanOrInf mean then raise <| ArgumentException "Mathe.randomStandardDeviation: given input for 'mean' is NaN or Infinity."
        if Mathe.isNanOrInf standardDeviation then raise <| ArgumentException "Mathe.randomStandardDeviation: given input for 'standardDeviation' is NaN or Infinity."
        let u1 = 1.0 - rand.NextDouble() // so that 0.0  < x <= 1.0 (never 0.0) see https://docs.microsoft.com/en-us/dotnet/api/system.random.nextdouble?view=netframework-4.7.2#remarks
        let u2 = rand.NextDouble()
        let randStdNormal = Math.Sqrt(-2.0 * Math.Log(u1)) * Math.Sin(2.0 * Math.PI * u2) //random normal(0, 1)
        //random normal(mean, stdDev^2)
        mean + (float standardDeviation|> LanguagePrimitives.FloatWithMeasure)* randStdNormal


    /// Just Double.TryParse(x) wrapped in an option
    static member inline tryParseFloatEn (x:string) : option<float> =
        match Double.TryParse(x) with
        | true, f -> Some f
        | _ ->  None

    /// Replaces ',' with '.' and then runs Double.TryParse(x) wrapped in an option
    static member inline tryParseFloatDe (x:string) : option<float> =
        match Double.TryParse(x.Replace(',','.')) with
        | true, f -> Some f
        | _ ->  None

    /// A very tolerant custom float parser.
    /// Ignores all non numeric characters ( expect a minus '-' before any digit )
    /// and considers '.' as decimal point.
    /// Does NOT allow for scientific notation !
    static member tryParseFloatEnTolerant(s:string) : option<float> =
        let sb = Text.StringBuilder(s.Length)
        for i = 0 to s.Length - 1 do
            let c = s.[i]
            if   c >= '0' && c <= '9'       then sb.Append(c)   |> ignore
            elif c = '.'                    then sb.Append(c)   |> ignore
            elif c = '-' && sb.Length = 0   then sb.Append(c)   |> ignore //only add minus before digits if stringBuilder is still empty
        match Double.TryParse(sb.ToString()) with
        | true, f -> Some  f
        | _ ->   None


    /// A very tolerant custom float parser.
    /// Ignores all non numeric characters ( expect a minus '-' before any digit )
    /// and considers '.' as decimal point.
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
