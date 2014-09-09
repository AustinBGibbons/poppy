package com.quantifind.poppy.example

import com.quantifind.poppy._
import Vega._

/**
 * User: austin
 * Date: 8/27/14
 */
object Population {

  def main(args: Array[String]) = {

    val dataName = "pop2000"

    println(
      Vega(
        width = 700,
        height = 400,
        padding = Padding(top = 0, left = 0, bottom = 20, right = 0),
        data = Array(Data(
          name = dataName,
          url = "data/population.json" //,
//          transform = Seq(
//
          )
        ),
        scales = Array(
          Scale(
            name = "g",
            domain = Array(0, 1),
            range = Array(340, 10)
          ),
          Scale(
            name = "y",
            scaleType = ScaleType.ordinal,
            range = ScaleRangeLiteral.height,
            reverse = true,
            domain = DataRef(data = dataName, field = "data.age")
          ),
          Scale(
            name = "c",
            scaleType = ScaleType.ordinal,
            domain = Array(1, 2),
            range = Array("#1f77b4", "#e377c2")
          )
        ),
        marks = Array(
          Mark(
            markType = MarkType.text,
            interactive = false,
//            from = DataRef(data = dataName, transform = )
            properties = Properties(
              enter = Property(
                x = ValueRef(`value` = 325),
                y = ValueRef(scale = "y", field = "age", offset = 11),
//                text = ValueRef(field =  = "age"),
//                baseline = ValueRef(`value` = middle)
//                align
                fill = ValueRef(`value` = "#000")
              )
            )
          ),
          Mark(
          // group type and stuff
          )
        )
      )
    )
  }
}
