package com.quantifind.poppy.example

import com.quantifind.poppy._
import Vega._

/**
 * User: austin
 * Date: 8/19/14
 */
object BarChart {

  def main(args: Array[String]) = {

    val dataName = "table"

    println(
      Vega(
        width = 400,
        height = 200,
        padding = Padding(bottom = 30, left = 30, right = 10, top = 10),
        data = Array(
          Data(
            name = dataName,
            values = Some(Array(XYPair(1, 28), XYPair(2, 55), XYPair(3, 43)))
          )
        ),
        scales = Array(
          Scale(
            name = "x",
            scaleType = ScaleType.ordinal,
            range = ScaleRangeLiteral.width,
            domain = DataRef(dataName, "data.x") // can we do better than a string here?
          ),
          Scale(
            name = "y",
            range = ScaleRangeLiteral.height,
//            range = Array(10, 20, 30),
            domain = DataRef(dataName, "data.y"),
            nice = true
          )
        ),
        axes = Array(
          Axis(
            axisType = AxisType.x,
            scale = "x"
          ),
          Axis(
            axisType = AxisType.y,
            scale = "y"
          )
        ),
        marks = Array(
          Mark(
            markType = MarkType.rect,
            from = DataRef(data = dataName),
            properties = Properties(
              enter = Property(
                x = ValueRef(scale = "x", field = "data.x"),
                width = ValueRef(scale = "x", band = true, offset = -1),
                y = ValueRef(scale = "y", field = "data.y"),
                y2 = ValueRef(scale = "y", `value` = 0)
              ),
              update = Property(
                fill = ValueRef(`value` = "steelblue")
              ),
              hover = Property(
                fill = ValueRef(`value` = "red")
              )
            )
          )
        )
      ).toJson
    )
  }

}
