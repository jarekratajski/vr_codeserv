package pl.setblack.vide.walker

import java.nio.file.{FileSystems, Files, Path, Paths}

import com.github.javaparser.JavaParser

import scala.collection.JavaConverters._

class FileScanner {

  def readJavaFile(file: Path, javaRoot: Path, violations: Seq[Violation]): JavaFile = {
    val pack = javaRoot.relativize(file)
    val thisFileViolation = violations.find( v => {
      v.fineName == pack.toString
    })

    val packageName = pack.getParent.toString.replaceAll("/", ".")
    val javaSource = Files.readAllLines(file).asScala.mkString("\n")
    val parsed = JavaParser.parse(javaSource)
    val imports = parsed.getImports.asScala
      .filter(!_.isAsterisk)
      .filter(!_.isStatic)
      .map(node => {
       node.getName.toString
      })

    JavaFile(file.getFileName.toString, packageName, javaSource, imports, javaSource.length,
      thisFileViolation.map(_.cnt).getOrElse(0), thisFileViolation.map(_.highest).getOrElse(0))
  }

  def getAllJavas(dir: Path, javaRoot: Path, violations: Seq[Violation]): Seq[JavaFile] = {
    val elements = Files.list(dir)
    val allJavas = elements.iterator().asScala
      .map(file => {
        if (Files.isDirectory(file)) {
          getAllJavas(file, javaRoot, violations)
        } else {
          if (file.getFileName().toString.endsWith(".java")) {

            Seq(readJavaFile(file, javaRoot, violations))
          } else {
            Seq()
          }
        }
      })
    (Seq[JavaFile]() /: allJavas) (_ ++ _)
  }


  def scanNormalDir(dir: Path): Seq[JavaFile] = {
    val subdirs = Files.list(dir).iterator().asScala.filter(Files.isDirectory(_))
    (Seq[JavaFile]() /: subdirs.map(checkDir(_))) (_ ++ _)
  }

  private def readPMD(pmdFile: Path, javaMainDir: Path):Seq[Violation] = {
    val parsedPMD = scala.xml.XML.loadFile(pmdFile.toFile)
    val files = parsedPMD \\ "pmd" \\ "file"

    files.map( file => {
      val name = (file \\"@name").text

      val javaFilePath = Paths.get(name)
      val subtail = javaMainDir.relativize(javaFilePath)
      println(subtail)

      val violations = file \\ "violation"
      val allCCViolations = violations.map( viol => {
        val rule = (viol \\"@rule").text
        val info = (viol).text


        if (rule == "CyclomaticComplexity") {
          if ( info.contains( "Highest = ")) {
            val place = info.indexOf("Highest =")
            val end = info.substring(place)
            val highestCC = end.substring(10,end.length-3)
            highestCC.toInt
          } else {
            1
          }

        } else {
          0
        }
      })
      Violation(subtail.toString, allCCViolations.size, allCCViolations.max)
    })

  }

  def checkDir(dir: Path): Seq[JavaFile] = {
    if (Files.exists(dir.resolve("pom.xml"))) {
      if (Files.exists(dir.resolve("src/main/java"))) {
        val pmdFile = dir.resolve("target/pmd.xml")
        val javaMain = dir.resolve("src/main/java")
        val violations = if ( Files.exists(pmdFile)) {
          readPMD( pmdFile, javaMain)
        } else {
          Seq()
        }

        getAllJavas(javaMain, javaMain, violations)
      } else {

        scanNormalDir(dir)
      }
    } else {

      scanNormalDir(dir)
    }
  }

  def walk(dir: Path) = {
    if (Files.isDirectory(dir)) {
      checkDir(dir)
    } else {
      Seq()
    }
  }

  private def insertPackageOrClass(javaFile: JavaFile, currentPackage: Seq[String], tail: Seq[String], basePackage:JavaPackage): JavaPackage = {
    if ( tail.isEmpty) {
      basePackage.copy( classes = basePackage.classes :+ javaFile)
    } else {
      insertJavaFile(javaFile, currentPackage :+ tail.head, tail.tail, basePackage)
    }
  }


  private def insertJavaFile(javaFile: JavaFile, currentPackage: Seq[String], tail: Seq[String], basePackage:JavaPackage): JavaPackage = {
    val existing = basePackage.subpackages.find( _.name == currentPackage )//.getOrElse( new Package(currentPackage, Seq(), Seq(), Coords(0,0,0)))
    val newPackage = existing.map( _ => basePackage.copy( subpackages =
      basePackage.subpackages.map( p => if (p.name == currentPackage) {
          insertPackageOrClass(javaFile, currentPackage, tail, p)
      } else {
        p
      })
    )).getOrElse( basePackage.copy(subpackages = basePackage.subpackages :+
      insertPackageOrClass(javaFile, currentPackage, tail,  JavaPackage(currentPackage, Nil, Nil))))
    newPackage
  }

  private def insertJavaFile(javaFile: JavaFile, rootPackage: JavaPackage): JavaPackage = {
    val pack = javaFile.pack.split("\\.").toSeq

    insertJavaFile( javaFile, Seq(pack.head), pack.tail, rootPackage)

  }

  private def extractPackages(javas: Seq[JavaFile]): JavaPackage = {
    var rootPackage = JavaPackage(Nil, Nil, Nil)


    javas.map( javaFile => {
      val pack = javaFile.pack

      rootPackage = insertJavaFile( javaFile, rootPackage)
    })

    rootPackage
  }

}


object FileScanner {

  def run() = {
    //val dir = FileSystems.getDefault.getPath("/home/jarek/dev/external/j2l_tigerbuild/")
    val dir = FileSystems.getDefault.getPath("/home/jarek/dev/external/j2l_tigerbuild/J2L_ServiceImpl")
    val walker = new FileScanner
    val javaFiles = walker.walk(dir)
    //println (javaFiles)
    walker.extractPackages(javaFiles)
  }

}

case class JavaFile(
                     className: String,
                     pack: String,
                     code: String,
                     imports: Seq[String],
                     size : Int,
                     violationCount: Int,
                     violationLevel: Int

                   )

case class JavaPackage(  name : Seq[String], subpackages : List[JavaPackage],  classes : List[JavaFile])


case class Violation(fineName : String, cnt :Int, highest: Int)