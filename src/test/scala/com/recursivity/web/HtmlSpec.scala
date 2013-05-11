package com.recursivity.web

import org.specs2.mutable._
import com.recursivity.web.Html.View

class HtmlSpec extends Specification{

  "The Html" should{
    "be able to render a template" in{
       Html.renderHtml(View("/child.mustache", Map("name" -> "Wille"))) must_== "Hello Wille"
    }
    "be able to render a template within a parent hierarchically" in{
      Html.renderHtml(View("/child.mustache", Map("name" -> "Wille"), Some((View("/parent.mustache", Map.empty), "child")))) must_== "Parent Hello Wille"
    }
  }

}









