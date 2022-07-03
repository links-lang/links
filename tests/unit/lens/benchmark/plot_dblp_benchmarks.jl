
using Plots
using CSV

pyplot()

# path = "./_build/default/tests/unit/lens/benchmark/"
path = "./"

function plot_bench_1(file)
  try
    csv = CSV.File(path * file * ".csv", normalizenames=true)

    ms=5

      plot(csv.n, csv.cttime, label="naive, total time", framestyle=:box, xaxis="Number of Input Rows", yaxis="Time [ms]", ylim=(-70,2500), legend=:topleft,
          size=(400,250), formatter=:plain, grid=false, marker=:rect, markersize=ms, markerstrokewidth=0, color=:darkred)
      plot!(csv.n, csv.cqtime, label="naive, query time", linestyle=:dash, marker=:rect, markersize=ms, markerstrokewidth=0, color=:red)
    plot!(csv.n, csv.ittime, label="incr, total time", markershape=:utriangle, markersize=ms, markerstrokewidth=0, color=:darkblue)
    plot!(csv.n, csv.iqtime, label="incr. query time", linestyle=:dash, markershape=:utriangle, markersize=ms, markerstrokewidth=0, color=:blue)

    savefig(path * file * ".pdf")
  catch e
      print(string(e))
  end
end

function plot_bench_2(file)
    try
      csv = CSV.File(path * file * ".csv", normalizenames=true)

      ms=5

      plot(csv.n, csv.cttime, label="naive, total time", framestyle=:box, xaxis="Number of Change Set Entries", yaxis="Time [ms]", ylim=(-70,2500), legend=:topleft,
          size=(400,250), formatter=:plain, grid=false, marker=:rect, markersize=ms, markerstrokewidth=0, color=:purple3)
      plot!(csv.n, csv.cqtime, label="naive, query time", linestyle=:dash, marker=:rect, markersize=ms, markerstrokewidth=0, color=:purple1)
      plot!(csv.n, csv.ittime, label="incr, total time", markershape=:utriangle, markersize=ms, markerstrokewidth=0, color=:darkgreen)
      plot!(csv.n, csv.iqtime, label="incr. query time", linestyle=:dash, markershape=:utriangle, markersize=ms, markerstrokewidth=0, color=:green2)

      savefig(path * file * ".pdf")
    catch e
        print(string(e))
    end
end

function plot_bench_3(out, file1, file2)
    try
      csv_loc = CSV.File(path * file1 * ".csv", normalizenames=true)
      csv_rem = CSV.File(path * file2 * ".csv", normalizenames=true)

      ms=5

      plot(csv_loc.n, csv_loc.ttime, label="local, total time", framestyle=:box, xaxis="Number of Input Rows", yaxis="Time [ms]", ylim=(0,40), legend=:topleft,
          size=(400,250), formatter=:plain, grid=false, marker=:rect, markersize=ms, markerstrokewidth=0, color=:darkblue)
      plot!(csv_loc.n, csv_loc.qtime, label="local, query time", linestyle=:dash, marker=:rect, markersize=ms, markerstrokewidth=0, color=:blue)

      plot!(csv_rem.n, csv_rem.ttime, label="remote, total time", markershape=:utriangle, markersize=ms, markerstrokewidth=0, color=:darkgreen)
      plot!(csv_rem.n, csv_rem.qtime, label="remote query time", linestyle=:dash, markershape=:utriangle, markersize=ms, markerstrokewidth=0, color=:green2)

      savefig(path * out * ".pdf")
    catch e
        print(string(e))
    end
end


plot_bench_1("dblp_example_local")
plot_bench_2("dblp_example_remote")
plot_bench_3("dblp_example_incr_both", "dblp_example_incr_local", "dblp_example_incr_remote")

