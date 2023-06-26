open Lens.Utility

module type Printer = sig
  val printf : ('a, Format.formatter, unit) format -> 'a
end

let verbose_printer test_ctx =
  (module struct
    let printf fmt =
      if Options.verbose_opt test_ctx then Format.printf fmt
      else
        let (CamlinternalFormatBasics.Format (fmt, _)) = fmt in
        CamlinternalFormat.make_printf
          (fun _ -> ())
          CamlinternalFormat.End_of_acc fmt
  end : Printer)
