
using Plots
using CSV

pyplot()

path = "./_build/default/tests/unit/lens/benchmark/"

function plot_bench_1(file)
  try
    csv = CSV.File(path * file * ".csv", normalizenames=true)
    # csv = CSV.File("benchmark_3_dl.csv"; header=["rows", "incremental (query)", "incremental (total)", "naive (query)", "naive (total)"])

    mycols = [:blue, :darkblue, :red, :darkred]

    ms=5

    plot(csv.n, csv.cttime, label="naive, total time", framestyle=:box, xaxis="Number of Input Rows", yaxis="Time [ms]", legend=:topleft,
          size=(400,250), formatter=:plain, grid=false, marker=:rect, markersize=ms, markerstrokewidth=0, color=:darkred)
      plot!(csv.n, csv.cqtime, label="naive, query time", linestyle=:dash, marker=:rect, markersize=ms, markerstrokewidth=0, color=:red)
    plot!(csv.n, csv.ittime, label="incr, total time", markershape=:utriangle, markersize=ms, markerstrokewidth=0, color=:darkblue)
    plot!(csv.n, csv.iqtime, label="incr. query time", linestyle=:dash, markershape=:utriangle, markersize=ms, markerstrokewidth=0, color=:blue)

    savefig(path * file * ".pdf")
  catch e
  end
end

function plot_bench_2(file)
  try
    csv = CSV.File(path * file * ".csv", normalizenames=true)
    # csv = CSV.File("benchmark_3_dl.csv"; header=["rows", "incremental (query)", "incremental (total)", "naive (query)", "naive (total)"])

    mycols = [:blue, :darkblue, :red, :darkred]

    ms=5

    plot(csv.n, csv.cttime, label="naive, total time", framestyle=:box, xaxis="Number of Change Set Entries", yaxis="Time [ms]", legend=:topleft,
         size=(400,250), formatter=:plain, grid=false, marker=:rect, markersize=ms, markerstrokewidth=0, color=:darkred)
    plot!(csv.n, csv.cqtime, label="naive, query time", linestyle=:dash, marker=:rect, markersize=ms, markerstrokewidth=0, color=:red)
    plot!(csv.n, csv.ittime, label="incr, total time", markershape=:utriangle, markersize=ms, markerstrokewidth=0, color=:darkblue)
    plot!(csv.n, csv.iqtime, label="incr. query time", linestyle=:dash, markershape=:utriangle, markersize=ms, markerstrokewidth=0, color=:blue)

    savefig(path * file * ".pdf")
  catch e
  end
end

function plot_bench_3(file)
  try
    csv = CSV.File(path * file * ".csv", normalizenames=true)
    # csv = CSV.File("benchmark_3_dl.csv"; header=["rows", "incremental (query)", "incremental (total)", "naive (query)", "naive (total)"])

    mycols = [:blue, :darkblue, :red, :darkred]

    ms=5

    plot(csv.n, csv.ttime, label="total time", framestyle=:box, xaxis="Number of Change Set Entries", yaxis="Time [ms]", legend=:topleft,
         size=(400,250), formatter=:plain, grid=false, markershape=:utriangle, markersize=ms, markerstrokewidth=0, color=:darkblue)
    plot!(csv.n, csv.qtime, label="query time", linestyle=:dash, markershape=:utriangle, markersize=ms, markerstrokewidth=0, color=:blue)

    savefig(path * file * ".pdf")
  catch e
  end
end


plot_bench_1("drop_benchmark")
plot_bench_1("join_dl_benchmark")
plot_bench_1("join_dr_benchmark")
plot_bench_1("join_db_benchmark")
plot_bench_1("select_benchmark")
plot_bench_2("put_delta_benchmark")
plot_bench_2("select_delta_size_benchmark")
plot_bench_3("get_delta_benchmark")
