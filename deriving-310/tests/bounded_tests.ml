open Defs

let nullsum = 
  begin
    assert (bounded_nullsum.Bounded.min_bound = N0);
    assert (bounded_nullsum.Bounded.max_bound = N3);
  end

let poly0 =
  begin
    assert (bounded_poly0.Bounded.min_bound = `T0);
    assert (bounded_poly0.Bounded.max_bound = `T3);
  end

let tup4 =
  begin
    assert (bounded_tup4.Bounded.min_bound = (min_int, min_int, false, ()));
    assert (bounded_tup4.Bounded.max_bound = (max_int, max_int, true, ()));
  end

let t =
  begin
    assert (bounded_t.Bounded.min_bound = min_int);
    assert (bounded_t.Bounded.max_bound = max_int);
end
