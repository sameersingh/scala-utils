package org.sameersingh.utils.cmdopts

object CmdLine {
  def parse(argv:Array[String]) : Map[String,String] = {
    //seperate options from arguments
    val (opts, args) = argv.partition {
      _.startsWith("@")
    }

    //turning options array into map
    val optsMap = Map() ++ opts.map {
      x =>
        val pair = x.split("@{1,2}")(1).split("=")
        if (pair.length == 1) (pair(0), "true")
        else (pair(0), pair(1))
    }
    optsMap
  }
}