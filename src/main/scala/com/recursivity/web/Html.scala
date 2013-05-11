package com.recursivity.web

import org.fusesource.scalate.{TemplateEngine, DefaultRenderContext}
import org.fusesource.scalate.util.{Resource, FileResourceLoader}
import java.text.{DecimalFormat}
import java.io.{StringWriter,PrintWriter}
import unfiltered.response._


object Html{

  type ViewNameInParentModel = String
  case class View(uri: String, model: Map[String,Any], parent: Option[(View, ViewNameInParentModel)] = None)

  private val engine = new TemplateEngine
  engine.allowCaching = true
  engine.allowReload = false
  engine.resourceLoader = new FileResourceLoader{
    override def resource(uri: String): Option[Resource] = Option(Resource.fromURL(this.getClass.getResource(uri)))
  }

  private def renderTemplate(uri: String, model: Map[String,Any]): String = {
    val writer = new StringWriter()
    val pw = new PrintWriter(writer)
    val context = new DefaultRenderContext(uri, engine, pw)
    val df = new DecimalFormat
    df.setGroupingUsed(false)

    context.numberFormat_=(df)
    context.render(uri,model)
    writer.toString
  }

  def apply(view: View) = Ok ~> HtmlContent ~> ResponseString(renderHtml(view))

  def renderHtml(view: View): String = {
    val currentView = renderTemplate(view.uri, view.model)
    view.parent map{parentView => 
      val (parent, childViewName) = parentView
      renderHtml(parent.copy(model = parent.model ++ Map[String,Any](childViewName -> currentView)))
    } getOrElse currentView
  }

  implicit def uriAndModelToView(uri: String, model: Map[String,Any])(implicit parentView: Option[(View,ViewNameInParentModel)] = None)= View(uri,model,parentView)
  
  implicit def tupleToView(view: (String,Map[String,Any]))(implicit parentView: Option[(View,ViewNameInParentModel)] = None) = View(view._1,view._2,parentView)
}
