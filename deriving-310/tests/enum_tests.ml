open Defs

let nullsum = 
  begin
    let e = enum_nullsum in

      assert (e.Enum.succ N0 = N1);
      assert (e.Enum.succ N1 = N2);
      assert (e.Enum.succ N2 = N3);
      assert (try ignore (e.Enum.succ N3); false
              with Invalid_argument "succ" -> true);

      assert (try ignore (e.Enum.pred N0); false
              with Invalid_argument "pred" -> true);
      assert (e.Enum.pred N1 = N0);
      assert (e.Enum.pred N2 = N1);
      assert (e.Enum.pred N3 = N2);

      assert (e.Enum.from_enum N0 = 0);
      assert (e.Enum.from_enum N1 = 1);
      assert (e.Enum.from_enum N2 = 2);
      assert (e.Enum.from_enum N3 = 3);

      assert (e.Enum.to_enum 0 = N0);
      assert (e.Enum.to_enum 1 = N1);
      assert (e.Enum.to_enum 2 = N2);
      assert (e.Enum.to_enum 3 = N3);
      assert (try ignore (e.Enum.to_enum 4); false
              with Invalid_argument "to_enum" -> true);

      assert (e.Enum.enum_from N0 = [N0;N1;N2;N3]);
      assert (e.Enum.enum_from N1 = [N1;N2;N3]);
      assert (e.Enum.enum_from N2 = [N2;N3]);
      assert (e.Enum.enum_from N3 = [N3]);

      assert (e.Enum.enum_from_then N0 N1 = [N0;N1;N2;N3]);
      assert (e.Enum.enum_from_then N0 N2 = [N0;N2]);
      assert (e.Enum.enum_from_then N1 N2 = [N1;N2;N3]);
      assert (e.Enum.enum_from_then N1 N3 = [N1;N3]);
      assert (try ignore (e.Enum.enum_from_then N3 N3); false
              with Invalid_argument _ -> true);
      assert (try ignore (e.Enum.enum_from_then N3 N1); false
              with Invalid_argument _ -> true);

      assert (e.Enum.enum_from_to N0 N1 = [N0;N1]);
      assert (e.Enum.enum_from_to N1 N3 = [N1;N2;N3]);
      assert (e.Enum.enum_from_to N1 N1 = [N1]);
      assert (e.Enum.enum_from_to N1 N0 = []);

      assert (e.Enum.enum_from_then_to N0 N1 N3 = [N0;N1;N2;N3]);
      assert (e.Enum.enum_from_then_to N0 N2 N3 = [N0;N2]);
      assert (e.Enum.enum_from_then_to N0 N3 N3 = [N0;N3]);
      assert (try ignore (e.Enum.enum_from_then_to N0 N0 N0); false
              with Invalid_argument _ -> true);
  end

let poly0 =
  begin
    let e = enum_poly0 in

      assert (e.Enum.succ `T0 = `T1);
      assert (e.Enum.succ `T1 = `T2);
      assert (e.Enum.succ `T2 = `T3);
      assert (try ignore (e.Enum.succ `T3); false
              with Invalid_argument "succ" -> true);

      assert (try ignore (e.Enum.pred `T0); false
              with Invalid_argument "pred" -> true);
      assert (e.Enum.pred `T1 = `T0);
      assert (e.Enum.pred `T2 = `T1);
      assert (e.Enum.pred `T3 = `T2);

  end
    
let t =
  begin
    ListLabels.iter (Enum.enum_int.Enum.enum_from_to (-1000) 1000)
      ~f:(fun i -> 
            assert (enum_t.Enum.succ i = i+1);
            assert (enum_t.Enum.pred i = i-1);
            assert (enum_t.Enum.to_enum i = i);
            assert (enum_t.Enum.from_enum i = i));
  end
