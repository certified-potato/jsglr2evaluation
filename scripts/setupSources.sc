import $ivy.`com.lihaoyi::ammonite-ops:2.2.0`, ammonite.ops._

import $file.common, common._, Suite._

println("Setting up sources...")

mkdir! sourcesDir

suite.languages.foreach { language =>
    println(" " + language.name)
    
    rm! language.sourcesDir
    mkdir! language.sourcesDir / "repos"
    mkdir! language.sourcesDir / "batch"
    mkdir! language.sourcesDir / "incremental"

    // Inspiration: https://briancoyner.github.io/articles/2013-06-05-git-sparse-checkout/
    def clone(source: RepoSource, languageSourceRepoDir: Path) = {
        %%("git", "init")(languageSourceRepoDir)

        // Config sparse checkout
        %%("git", "config", "core.sparseCheckout", "true")(languageSourceRepoDir)
        write(languageSourceRepoDir / ".git" / "info" / "sparse-checkout", source match {
            // If a list of files is given: only checkout these files
            case IncrementalSource(_, _, _, _, files, _, _) if files.nonEmpty => files.mkString("\n")
            // Else: filter files based on extension
            case _ => "*." + language.extension
        })

        %%("git", "remote", "add", "origin", source.repo)(languageSourceRepoDir)

        source match {
            case BatchRepoSource(_, repo, _, branch) =>
                // Clone without all history
                %%("git", "fetch", "origin", branch, "--depth=1")(languageSourceRepoDir)
            case IncrementalSource(_, repo, _, fetchOptions, _, _, branch) =>
                // Clone with full history, possibly limited by the fetchOptions
                %%("git", "fetch", "origin", branch, fetchOptions)(languageSourceRepoDir)
        }

        %%("git", "checkout", source.branch)(languageSourceRepoDir)
    }

    language.sources.batch.foreach { source =>
        println("  " + source.id)

        val languageSourceDir =
            source match {
                case repoSource: RepoSource =>
                    val languageSourceRepoDir = language.sourcesDir / "repos" / source.id

                    rm! languageSourceRepoDir
                    mkdir! languageSourceRepoDir

                    timed("clone " + source.id)(clone(repoSource, languageSourceRepoDir))

                    languageSourceRepoDir
                case localSource: LocalSource =>
                    getPath(localSource.path)
            }

        timed("collect " + source.id) {
            val files = ls.rec! languageSourceDir |? (_.ext == language.extension)

            mkdir! language.sourcesDir / "batch" / source.id

            val selectedFiles =
                suite.shrinkBatchSources match {
                    case Some(size) if files.size > size => scala.util.Random.shuffle(files).take(size)
                    case _ => files
                }

            // Copy all files to the aggregated directory
            selectedFiles.foreach { file =>
                val pathInSource = file relativeTo languageSourceDir
                val filenameFlat = source.id + "_" + pathInSource.toString.replace("/", "_").replace(".", "_")
                val filename = filenameFlat.dropRight(1 + language.extension.size) + "." + language.extension

                cp.over(file, language.sourcesDir / "batch" / source.id / filename)
            }
        }
    }

    language.sources.incremental.foreach { source =>
        println("  " + source.id)

        val languageSourceRepoDir = language.sourcesDir / "repos" / source.id

        rm! languageSourceRepoDir
        mkdir! languageSourceRepoDir

        timed("clone " + source.id)(clone(source, languageSourceRepoDir))

        timed("preprocess " + source.id) {
            val files = if (source.files.nonEmpty) source.files.map(f => languageSourceRepoDir / RelPath(f))
                        else ls.rec! languageSourceRepoDir |? (_.ext == language.extension)

            val revisions = (
                if (source.files.nonEmpty)
                    %%("git", "log", "--format=%H", "--reverse", "--", files.map(_.toString).toSeq)(languageSourceRepoDir)
                else
                    %%("git", "log", "--format=%H", "--reverse", "--", "*." + language.extension)(languageSourceRepoDir)
            ).out.string.split("\n")

            (if (source.versions >= 0) revisions.takeRight(source.versions) else revisions)
                    .zipWithIndex.foreach { case (revision, i) =>
                %%("git", "checkout", "-f", revision)(languageSourceRepoDir)

                val revisionPath = language.sourcesDir / "incremental" / source.id / i.toString
                mkdir! revisionPath

                // Copy all files to the aggregated directory
                files.filter(_.toIO.exists).foreach { file =>
                    val pathInRepo = file relativeTo languageSourceRepoDir
                    val filename = pathInRepo.toString.replace("/", "_")

                    cp.over(file, revisionPath / filename)
                }
            }
        }
    }

    language.sources.recovery.foreach { source =>
        println("  " + source.id)

        val languageSourceDir = getPath(source.path)

        val files = ls.rec! languageSourceDir |? (_.ext == language.extension)

        mkdir! language.sourcesDir / "recovery" / source.id

        // Copy all files to the aggregated directory
        files.foreach { file =>
            val pathInSource = file relativeTo languageSourceDir
            val filenameFlat = source.id + "_" + pathInSource.toString.replace("/", "_").replace(".", "_")
            val filename = filenameFlat.dropRight(1 + language.extension.size) + "." + language.extension

            cp.over(file, language.sourcesDir / "recovery" / source.id / filename)
        }
    }

    // Repo dirs are now processed, remove
    rm! language.sourcesDir / "repos"
}
