open Defs

let nullsum = 
  begin
    let module E = Enum_nullsum in

      assert (E.succ N0 = N1);
      assert (E.succ N1 = N2);
      assert (E.succ N2 = N3);
      assert (try ignore (E.succ N3); false
              with Invalid_argument "succ" -> true);

      assert (try ignore (E.pred N0); false
              with Invalid_argument "pred" -> true);
      assert (E.pred N1 = N0);
      assert (E.pred N2 = N1);
      assert (E.pred N3 = N2);

      assert (E.from_enum N0 = 0);
      assert (E.from_enum N1 = 1);
      assert (E.from_enum N2 = 2);
      assert (E.from_enum N3 = 3);

      assert (E.to_enum 0 = N0);
      assert (E.to_enum 1 = N1);
      assert (E.to_enum 2 = N2);
      assert (E.to_enum 3 = N3);
      assert (try ignore (E.to_enum 4); false
              with Invalid_argument "to_enum" -> true);

      assert (E.enum_from N0 = [N0;N1;N2;N3]);
      assert (E.enum_from N1 = [N1;N2;N3]);
      assert (E.enum_from N2 = [N2;N3]);
      assert (E.enum_from N3 = [N3]);

      assert (E.enum_from_then N0 N1 = [N0;N1;N2;N3]);
      assert (E.enum_from_then N0 N2 = [N0;N2]);
      assert (E.enum_from_then N1 N2 = [N1;N2;N3]);
      assert (E.enum_from_then N1 N3 = [N1;N3]);
      assert (try ignore (E.enum_from_then N3 N3); false
              with Invalid_argument _ -> true);
      assert (try ignore (E.enum_from_then N3 N1); false
              with Invalid_argument _ -> true);

      assert (E.enum_from_to N0 N1 = [N0;N1]);
      assert (E.enum_from_to N1 N3 = [N1;N2;N3]);
      assert (E.enum_from_to N1 N1 = [N1]);
      assert (E.enum_from_to N1 N0 = []);

      assert (E.enum_from_then_to N0 N1 N3 = [N0;N1;N2;N3]);
      assert (E.enum_from_then_to N0 N2 N3 = [N0;N2]);
      assert (E.enum_from_then_to N0 N3 N3 = [N0;N3]);
      assert (try ignore (E.enum_from_then_to N0 N0 N0); false
              with Invalid_argument _ -> true);
  end

let poly0 =
  begin
    let module E = Enum_poly0 in

      assert (E.succ `T0 = `T1);
      assert (E.succ `T1 = `T2);
      assert (E.succ `T2 = `T3);
      assert (try ignore (E.succ `T3); false
              with Invalid_argument "succ" -> true);

      assert (try ignore (E.pred `T0); false
              with Invalid_argument "pred" -> true);
      assert (E.pred `T1 = `T0);
      assert (E.pred `T2 = `T1);
      assert (E.pred `T3 = `T2);

  end
    
let t =
  begin
    ListLabels.iter (Enum.Enum_int.enum_from_to (-1000) 1000)
      ~f:(fun i -> 
            assert (Enum_t.succ i = i+1);
            assert (Enum_t.pred i = i-1);
            assert (Enum_t.to_enum i = i);
            assert (Enum_t.from_enum i = i));
  end
