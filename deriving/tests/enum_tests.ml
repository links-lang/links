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

      assert (E.fromEnum N0 = 0);
      assert (E.fromEnum N1 = 1);
      assert (E.fromEnum N2 = 2);
      assert (E.fromEnum N3 = 3);

      assert (E.toEnum 0 = N0);
      assert (E.toEnum 1 = N1);
      assert (E.toEnum 2 = N2);
      assert (E.toEnum 3 = N3);
      assert (try ignore (E.toEnum 4); false
              with Invalid_argument "toEnum" -> true);

      assert (E.enumFrom N0 = [N0;N1;N2;N3]);
      assert (E.enumFrom N1 = [N1;N2;N3]);
      assert (E.enumFrom N2 = [N2;N3]);
      assert (E.enumFrom N3 = [N3]);

      assert (E.enumFromThen N0 N1 = [N0;N1;N2;N3]);
      assert (E.enumFromThen N0 N2 = [N0;N2]);
      assert (E.enumFromThen N1 N2 = [N1;N2;N3]);
      assert (E.enumFromThen N1 N3 = [N1;N3]);
      assert (try ignore (E.enumFromThen N3 N3); false
              with Invalid_argument _ -> true);
      assert (try ignore (E.enumFromThen N3 N1); false
              with Invalid_argument _ -> true);

      assert (E.enumFromTo N0 N1 = [N0;N1]);
      assert (E.enumFromTo N1 N3 = [N1;N2;N3]);
      assert (E.enumFromTo N1 N1 = [N1]);
      assert (E.enumFromTo N1 N0 = []);

      assert (E.enumFromThenTo N0 N1 N3 = [N0;N1;N2;N3]);
      assert (E.enumFromThenTo N0 N2 N3 = [N0;N2]);
      assert (E.enumFromThenTo N0 N3 N3 = [N0;N3]);
      assert (try ignore (E.enumFromThenTo N0 N0 N0); false
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
    ListLabels.iter (Enum.Enum_int.enumFromTo (-1000) 1000)
      ~f:(fun i -> 
            assert (Enum_t.succ i = i+1);
            assert (Enum_t.pred i = i-1);
            assert (Enum_t.toEnum i = i);
            assert (Enum_t.fromEnum i = i));
  end
