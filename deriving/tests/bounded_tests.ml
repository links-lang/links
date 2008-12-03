open Defs

let nullsum = 
  begin
    assert (Bounded_nullsum.min_bound = N0);
    assert (Bounded_nullsum.max_bound = N3);
  end

let poly0 =
  begin
    assert (Bounded_poly0.min_bound = `T0);
    assert (Bounded_poly0.max_bound = `T3);
  end

let tup4 =
  begin
    assert (Bounded_tup4.min_bound = (min_int, min_int, false, ()));
    assert (Bounded_tup4.max_bound = (max_int, max_int, true, ()));
  end

let t =
  begin
    assert (Bounded_t.min_bound = min_int);
    assert (Bounded_t.max_bound = max_int);
end
