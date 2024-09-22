namespace Mathe

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

/// Small functions for sorting and finding minimum or maximum values
/// of tuple ,triples and quadruples
module MinMaxSort =

    /// Returns the smallest element.
    /// Elements are compared by applying the predicate function first.
    /// If both are equal then the first element is returned.
    let inline minBy f a b =  if f a > f b then b else a

    /// Returns the biggest element.
    /// Elements are compared by applying the predicate function first.
    /// If both are equal then the first element is returned.
    let inline maxBy f a b =  if f a < f b then b else a

    /// Returns the smallest of three elements.
    let inline min3 (a, b, c) = min a b |> min c

    /// Returns the biggest of three elements.
    let inline max3 (a, b, c) = max a b |> max c

    /// Returns the smallest of four elements.
    let inline min4 (a, b, c, d) = min a b |> min c |> min d

    /// Returns the biggest of four elements.
    let inline max4 (a, b, c, d) = max a b |> max c |> max d

    /// Sort two elements.
    /// If they are equal then the order is kept
    let inline sort2 (a, b)  = if a <= b  then a, b else b, a

    /// Sort two elements.
    /// Elements are compared by applying the predicate function first.
    /// If they are equal after function is applied then the order is kept
    let inline sort2By f (a, b) = if f a <= f b  then a, b else b, a

    /// Sort three elements.
    /// If any are equal then the order is kept
    let inline sort3 (a, b, c) =
        if a <= b then
            if b <= c then      a, b, c
            else // c<b
                if a <= c then  a, c, b
                else            c, a, b
        else // b<a
            if a <= c then      b, a, c
            else //c<a
                if b <= c then  b, c, a
                else            c, b, a

    /// Sort three elements.
    /// Elements are compared by applying the predicate function first.
    /// If any are equal after function is applied then the order is kept
    let inline sort3By f (aa, bb, cc) =
        let a = f aa
        let b = f bb
        let c = f cc
        if a <= b then
            if b <= c then      aa, bb, cc
            else // c<b
                if a <= c then  aa, cc, bb
                else            cc, aa, bb
        else // b<a
            if a <= c then      bb, aa, cc
            else //c<a
                if b <= c then  bb, cc, aa
                else            cc, bb, aa

    /// Gets the positive difference between 2 numbers.
    /// Avoids the integer( or byte) overflow and underflow risk of "abs(a-b)"
    let inline diff a b =
        if   a<b then b-a
        else          a-b


/// Operators for chaining compare operations like: <c> 1 <. x .< 9 </c>
/// or <c> 0 <. x .<. y .< 99 </c>
/// Each chained comparison operator will be combined with the previous result via AND (&&) logic
module CompareOperators =

    /// Point must be at middle of expression: like this: min <=. x .<= max
    let inline (<=.) left middle = (left <= middle, middle)

    /// Point must be at middle of expression: like this: min <. x .< max
    let inline (<.) left middle = (left < middle, middle)

    /// Point must be at middle of expression: like this: min <. x .< max
    let inline (.<) (leftResult, middle) right = leftResult && (middle < right)

    /// Point must be at middle of expression: like this: min <=. x .<= max
    let inline (.<=) (leftResult, middle) right = leftResult && (middle <= right)

    /// For inner expressions: like this: min <. x .<. y .< max
    let inline (.<.) (leftResult, middle) right = leftResult && (middle < right), right

    /// for inner expressions: like this: min <. x .<. y .< max
    let inline (.<=.) (leftResult, middle) right = leftResult && (middle <= right), right


    (*
    this reversed order does not really make sense since the combing logic is always AND (&&)

    let x = 2
    10 >. x .> 20  // to test if x is outside of range would return false, but true could be expected (OR logic)

    /// Point must be at middle of expression: like this: min <. x .< max
    let inline (>.) left middle = (left > middle, middle)

    /// Point must be at middle of expression: like this: min <=. x .<= max
    let inline (>=.) left middle = (left >= middle, middle)

    /// Point must be at middle of expression: like this: min <. x .< max
    let inline (.>) (leftResult, middle) right = leftResult && (middle > right)

    /// Point must be at middle of expression: like this: min <=. x .<= max
    let inline (.>=) (leftResult, middle) right = leftResult && (middle >= right)

    /// for inner expressions: like this: min <. x .<. y .< max
    let inline (.>=.) (leftResult, middle) right = leftResult && (middle >= right), right

    /// For inner expressions: like this: min <. x .<. y .< max
    let inline (.>.) (leftResult, middle) right = leftResult && (middle > right), right
    *)

