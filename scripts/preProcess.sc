import $ivy.`com.lihaoyi::ammonite-ops:2.2.0`, ammonite.ops._

import $file.common, common._, Suite._
import $file.spoofax, spoofax._
import $file.parsers, parsers._
import org.spoofax.interpreter.terms.IStrategoTerm
import org.spoofax.jsglr2.JSGLR2Variant
import org.spoofax.jsglr2.integration.IntegrationVariant
import org.spoofax.jsglr2.recovery.Reconstruction
import org.spoofax.jsglr2.parser.result.{ParseSuccess => JSGLR2ParseSuccess}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

println("Validate sources...")

object PreProcessing {

    val timeout = 30

    def run = {
        suite.languages.foreach { language =>
            println(" " + language.name)

            val parsers = Parser.variants(language)

            timed("validate " + language.id) {
                (language.sourceFilesBatch() ++ language.sourceFilesIncremental).foreach { file =>
                    val input = read! file
                    val filename = file relativeTo language.sourcesDir

                    val results: Seq[(String, ParseResult)] = parsers.map { parser =>               
                        val result = withTimeout(parser.parse(input), timeout)(ParseFailure(Some("timeout"), Timeout))(e => ParseFailure(Some("failed: " + e.getMessage), Invalid))

                        (parser.id, result)
                    }

                    val failures: Seq[(String, Option[String], ParseFailureReason)] = results.flatMap {
                        case (parser, ParseFailure(error, reason)) => Some((parser, error, reason))
                        case _ => None
                    }

                    val successASTs: Seq[IStrategoTerm] = results.flatMap {
                        case (parser, ParseSuccess(ast)) => ast
                        case _ => None
                    }

                    def consistentASTs(asts: Seq[IStrategoTerm]) = asts.map(_.toString()).distinct.size == 1

                    val verdict =
                        if (failures.nonEmpty) {
                            println("   Invalid: " + filename)
                            failures.foreach { case (parser, error, _) =>
                                println("     " + parser + error.fold("")(" (" + _ + ")"))
                            }

                            val invalid = failures.exists(_._3 == Invalid)
                            val ambiguous = failures.exists(_._3 == Ambiguous)

                            Some(if (invalid) "invalid" else if (ambiguous) "ambiguous" else "timeout")
                        } else if (!consistentASTs(successASTs)) {
                            println("   Inconsistent: " + filename)

                            Some("inconsistent")
                        } else {
                            println("   Valid: " + filename)

                            None
                        }

                    verdict match {
                        case Some(folder) =>
                            val destinationFile = sourcesDir / folder / file.relativeTo(sourcesDir)
                            mkdir! destinationFile / up
                            if (verdict == "ambiguous")
                                // We still want to benchmark ambiguous files, but also want to be able to inspect them
                                cp.over(file, destinationFile)
                            else
                                mv.over(file, destinationFile)
                        case None =>
                    }
                }            
            }
            val sizes =
                language.sources.batch.flatMap { source =>
                    val sourceDir = language.sourcesDir / "batch" / source.id
                    val files = ls! sourceDir |? (_.ext == language.extension)
                    val sizes = files.map(_.size)

                    if (sizes.nonEmpty) {
                        write.over(language.sourcesDir / "batch" / source.id / "sizes.csv", sizes.mkString("\n") + "\n")            
                        %("Rscript", "sourceSizes.R", sourceDir, source.id)(pwd)
                    }

                    sizes
                }
            
            if (sizes.nonEmpty) {
                write.over(language.sourcesDir / "batch" / "sizes.csv", sizes.mkString("\n") + "\n")
                %("Rscript", "sourceSizes.R", language.sourcesDir / "batch", language.name)(pwd)
            }
        }
    }

}

PreProcessing.run

timed("persist dynamic parse tables") {
    persistDynamicParseTables
}
