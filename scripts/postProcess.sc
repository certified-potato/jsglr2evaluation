import $ivy.`com.lihaoyi::ammonite-ops:2.2.0`, ammonite.ops._

import $file.spoofaxDeps

import $ivy.`org.metaborg:org.spoofax.jsglr2:2.6.0-SNAPSHOT`
import $file.parsers, parsers._

import scala.collection.JavaConverters._

import $file.common, common._, Suite._

println("Processing results...")

mkdir! resultsDir

val languagesWithBatchSources = suite.languages.filter(_.sourcesBatchNonEmpty.nonEmpty)
if (languagesWithBatchSources.nonEmpty) {
    val dir = languagesWithBatchSources(0).measurementsDir

    // Copy header from measurements CSV
    write.over(parseTableMeasurementsPath, "language," + read.lines(dir / "batch" / "parsetable.csv")(0))
    write.over(parsingMeasurementsPath,    "language," + read.lines(dir / "batch" / "parsing.csv")(0))

    // Setup header for benchmarks CSV
    mkdir! batchResultsDir
    
    Seq(
        InternalParse, 
        Internal, 
        External
    ).filter(comparison => suite.implode.fold(true)(_ == comparison.implode)).map { comparison =>
        mkdir! batchResultsDir / comparison.dir
        
        write.over(batchResultsDir / comparison.dir / "time.csv",       "language,variant,score,error,low,high\n")
        write.over(batchResultsDir / comparison.dir / "throughput.csv", "language,variant,score,low,high\n")
        
        suite.languages.foreach { language =>
            val languageResultsDir = batchResultsDir / comparison.dir / language.id

            mkdir! languageResultsDir

            write.over(languageResultsDir / "time.csv",       "language,variant,score,error,low,high\n")
            write.over(languageResultsDir / "throughput.csv", "language,variant,score,low,high\n")
            
            if (suite.individualBatchSources) {
                language.sourcesBatchNonEmpty.foreach { source =>
                    val sourceResultsDir = batchResultsDir / comparison.dir / language.id / source.id

                    mkdir! sourceResultsDir

                    write.over(sourceResultsDir / "time.csv",       "language,variant,score,error,low,high\n")
                    write.over(sourceResultsDir / "throughput.csv", "language,variant,score,low,high\n")
                }
            }
        }
    }
    
    if (suite.languages.exists(exists! _.benchmarksDir / "batch-sampled")) {
        mkdir! batchSampledResultsDir
        write.over(batchSampledResultsDir / "time.csv",       "language,variant,score,error,low,high,size\n")
        write.over(batchSampledResultsDir / "throughput.csv", "language,variant,score,low,high,size\n")
    }
}

// Normalization: chars / ms == 1000 chars / s

suite.languages.foreach { language => 
    println(" " + language.name)

    if (language.sourcesBatchNonEmpty.nonEmpty) {

        // Measurements

        write.append(parseTableMeasurementsPath, "\n" + language.id + "," + read.lines(language.measurementsDir / "batch" / "parsetable.csv")(1))
        write.append(parsingMeasurementsPath, "\n" + language.id + "," + read.lines(language.measurementsDir / "batch" / "parsing.csv")(1))
        write.append(parsingMeasurementsPath, "\n" + language.id + "," + read.lines(language.measurementsDir / "batch" / "parsing.csv")(2))

        // Benchmarks (batch)
        def processBenchmarkCSV(benchmarkCSV: CSV, variant: CSVRow => String, destinationPath: Path, destinationPathNormalized: Path, normalize: BigDecimal => BigDecimal, append: String = "") = {
            benchmarkCSV.rows.foreach { row =>
                val rawScore = row("Score")
                val rawError = row("Score Error (99.9%)")

                val score = BigDecimal(rawScore)
                val error = if (rawError != "NaN") BigDecimal(rawError) else BigDecimal(0)

                write.append(destinationPath, language.id + "," + variant(row) + "," + round(score) + "," + round(error) + "," + round(score - error) + "," + round(score + error) + append + "\n")
                write.append(destinationPathNormalized, language.id + "," + variant(row) + "," + round(normalize(score)) + "," + round(normalize(score + error)) + "," + round(normalize(score - error)) + append + "\n")
            }
        }

        def batchBenchmarks(comparison: Comparison, source: Option[BatchSource]) = {
            val benchmarksSubDir =
                if (comparison.implode)
                    "parse+implode"
                else
                    "parse"

            val (benchmarksDir, resultsDirs) = source match {
                case None => (
                    language.benchmarksDir / "batch" / benchmarksSubDir,
                    Seq(
                        batchResultsDir / comparison.dir,
                        batchResultsDir / comparison.dir / language.id
                    )
                )
                case Some(source) => (
                    language.benchmarksDir / "batch" / benchmarksSubDir / source.id,
                    Seq(
                        batchResultsDir / comparison.dir / language.id / source.id
                    )
                )
            }
                val measurements = language.measurementsBatch(source, "recovery")
                val characters = BigDecimal(measurements("characters"))
                val normalize: BigDecimal => BigDecimal = score => characters / score

                resultsDirs.foreach { resultsDir =>
                    processBenchmarkCSV(CSV.parse(benchmarksDir / "jsglr2.csv"), row => row("Param: variant"), resultsDir / "time.csv", resultsDir / "throughput.csv", normalize)

                    if (suite.variants.contains("jsglr1")) {
                        processBenchmarkCSV(CSV.parse(benchmarksDir / "jsglr1.csv"), _ => "jsglr1", resultsDir / "time.csv", resultsDir / "throughput.csv", normalize)
                    }

                    if (comparison == External) {
                        language.antlrBenchmarks.foreach { antlrBenchmark =>
                            processBenchmarkCSV(CSV.parse(benchmarksDir / s"${antlrBenchmark.id}.csv"), _ => antlrBenchmark.id, resultsDir / "time.csv", resultsDir / "throughput.csv", normalize)
                        }

                        if (language.extension == "java") {
                            processBenchmarkCSV(CSV.parse(benchmarksDir / "tree-sitter.csv"), _ => "tree-sitter", resultsDir / "time.csv", resultsDir / "throughput.csv", normalize)
                        }
                    }
                }
        }

        Seq(
            InternalParse,
            Internal,
            External
        ).filter(comparison => suite.implode.fold(true)(_ == comparison.implode)).map(comparison => {
            batchBenchmarks(comparison, None)

            if (suite.individualBatchSources) {
                language.sourcesBatchNonEmpty.foreach { source =>
                    batchBenchmarks(comparison, Some(source))
                }
            }
        })

        // Benchmarks (batch sampled)

        if (exists! language.benchmarksDir / "batch-sampled") {
            language.sourceFilesBatchSampled.foreach { file =>
                val characters = (read ! file).length
                val normalize: BigDecimal => BigDecimal = score => characters / score

                processBenchmarkCSV(CSV.parse(language.benchmarksDir / "batch-sampled" / s"${file.last.toString}.csv"), row => row("Param: variant"), batchSampledResultsDir / "time.csv", batchSampledResultsDir / "throughput.csv", normalize, "," + characters)
            }
        }
    }
}
