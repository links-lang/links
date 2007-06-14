open Defs

let nullsum = 
  begin
    assert (Bounded_nullsum.minBound = N0);
    assert (Bounded_nullsum.maxBound = N3);
  end

let poly0 =
  begin
    assert (Bounded_poly0.minBound = `T0);
    assert (Bounded_poly0.maxBound = `T3);
  end

let tup4 =
  begin
    assert (Bounded_tup4.minBound = (min_int, min_int, false, ()));
    assert (Bounded_tup4.maxBound = (max_int, max_int, true, ()));
  end

let t =
  begin
    assert (Bounded_t.minBound = min_int);
    assert (Bounded_t.maxBound = max_int);
end
